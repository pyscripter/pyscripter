{
   LSP client implementation
   (C) PyScripter 2021
}

unit LspClient;

interface

Uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.JSON,
  System.Generics.Collections,
  System.RegularExpressions,
  JclSysUtils,
  JclSynch;

type

  THandleResponse = procedure(Id: Int64; Result, Error: TJsonValue) of object;
  THandleNotify = procedure(const Method: string; Params: TJsonValue) of object;
  THandleErrorOutput = procedure(const ErrorOutput: string) of object;

  // partial list of server capabilities
  // Could be easily extented if needed
  TLspServerCapability =
    (
      // main features
      lspscCompletion,         // Code completion
      lspscHover,              // Mouse hover support
      lspscSignature,          // Function signature
      lspscDefinition,         // Go to definition
      lspscReferences,         // Find references
      lspscHighlight,          // Syntax highlighting
      lspscDocSymbol,          // Document symbols
      lspscRename,             // Rename refactoring
      lspWorkspaceFolders,     // Workspace support
      // secondary capabilities
      lspscIncrementalSync,    // supports incremental sync
      lspscOpenCloseNotify,    // supports DidOpen/DidClose notifications
      lspscSaveNotify,         // supports DidSave notification
      lspscCompletionResolve   // supports completionItem/resolve request
    );

  TLspServerCapabilities = set of TLspServerCapability;

  TLspServerStatus = (lspInactive, lspStarted, lspInitialized);

  TLspClient = class
  private
    FId: Int64;
    FExCmdOptions: TJclExecuteCmdProcessOptions;
    FServerThread: TThread;
    FSendDataThread: TThread;
    FContentHeaderRE: TRegEx;
    FReadLock: TRTLCriticalSection;
    FRequestsLock: TRTLCriticalSection;
    FSyncRequestLock: TRTLCriticalSection;
    FReadBuffer: TBytes;
    FServerCapabilities: TLspServerCapabilities;
    FPendingRequests: TDictionary<Int64, THandleResponse>;
    FSyncHelper: TObject; //TSyncRequestHelper
    FStatus: TLspServerStatus;
    FOnErrorOuput: THandleErrorOutput;
    FOnLspNotification: THandleNotify;
    FOnInitialized: TNotifyEvent;
    FOnShutdown: TNotifyEvent;
    procedure ProcessServerCapabilities(SC: TJsonObject);
    procedure ServerTerminated(Sender: TObject);
    procedure HandleInitialize(id: Int64; Result, Error: TJsonValue);
    procedure HandleSyncRequest(id: Int64; AResult, AError: TJsonValue);
    procedure HandleShutdown(id: Int64; Result, Error: TJsonValue);
    procedure ReceiveData(const Bytes: TBytes; BytesRead: Cardinal);
    procedure ReceiveErrorOutput(const Bytes: TBytes; BytesRead: Cardinal);
  public
    constructor Create(ServerPath: string);
    destructor Destroy; override;
    procedure StartServer;
    procedure SendToServer(const Bytes: TBytes);
    // Asynchronous request - Handler should not Free Result and Error
    // Returns unique Id
    function Request(const Method, Params: string; Handler: THandleResponse): Int64;
    // Synchronous request - Free Result and Error!
    procedure SyncRequest(const Method, Params: string;
      out Result, Error: TJsonValue; Timeout: Cardinal = INFINITE);
    procedure CancelRequest(Id: Int64);
    procedure Notify(const Method, Params: string);
    // LSP protocol
    procedure Initialize(const ClientName, ClientVersion: string;
      ClientCapabilities: TJsonObject;
      InitializationOptions: TJsonObject = nil);
    procedure Shutdown;
    property Status: TLspServerStatus read FStatus;
    property ServerCapabilities: TLspServerCapabilities read fServerCapabilities;
    property OnErrorOutput: THandleErrorOutput read fOnErrorOuput
      write fOnErrorOuput;
    property OnLspNotification: THandleNotify read fOnLspNotification
      write fOnLspNotification;
    property OnInitialized: TNotifyEvent read FOnInitialized write FOnInitialized;
    property OnShutdown: TNotifyEvent read FOnShutdown write FOnShutdown;
  end;

implementation

Uses
  System.Diagnostics,
  System.Variants,
  LspUtils;

const
   LspHeader =
     'Content-Length: %d'#13#10+
     'Content-Type: application/json; charset=utf-8'#13#10#13#10;

   LspRequest =
     '{"jsonrpc": "2.0", "id": %d, "method": "%s", "params": %s}'#13#10#13#10;

   LspNotification =
     '{"jsonrpc": "2.0", "method": "%s", "params": %s}';

type

  TSendDataThread = class(TThread)
  private
    FWriteHandle: THandle;
    FQueue: TThreadedQueue<TBytes>;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetWriteHandle(Handle: THandle);
    procedure SendToServer(Bytes: TBytes);
  end;


  TLspServerThread = class(TThread)
  private
    fProcessHandle: THandle;
    fExCmdOptions: TJclExecuteCmdProcessOptions;
    fAbortEvent: TJclEvent;
    fLspClient: TLspClient;
    procedure BeforeResume(const ProcessInfo: TProcessInformation;
      WriteHandle: THandle);
  protected
    procedure Execute; override;
    procedure TerminatedSet; override;
  public
    constructor Create(LspClient: TLspClient;
      ExCmdOptions: TJclExecuteCmdProcessOptions);
    destructor Destroy; override;
  end;

  TSyncRequestHelper = class
    SyncEvent: TSimpleEvent;
    Result: TJsonValue;
    Error: TJsonValue;
    constructor Create;
    destructor Destroy; override;
  end;

{ TLspClient }

procedure TLspClient.CancelRequest(Id: INt64);
begin
  FRequestsLock.Enter;
  try
    if FPendingRequests.ContainsKey(Id) then
      fPendingRequests.Remove(Id);
  finally
    FRequestsLock.Leave;
  end;
end;

constructor TLspClient.Create(ServerPath: string);
begin
  inherited Create;
  fReadLock.Initialize;
  FRequestsLock.Initialize;
  FSyncRequestLock.Initialize;
  fContentHeaderRE.Create('^(?:[^\r\n]+\r\n)*' +
                    'Content-Length: (?P<length>\d+)\r\n' +
                    '(?:[^\r\n]+\r\n)*\r\n' +
                    '(?={)', //look ahead not part of the capture
                    [roMultiLine, roCompiled]);
  fSyncHelper := TSyncRequestHelper.Create;
  FPendingRequests := TDictionary<Int64, THandleResponse>.Create;
  FExCmdOptions := TJclExecuteCmdProcessOptions.Create(ServerPath);
end;

destructor TLspClient.Destroy;
begin
  FOnLspNotification := nil;
  FOnErrorOuput := nil;
  Shutdown;
  FSyncHelper.Free;
  FExCmdOptions.Free;
  FSyncRequestLock.Free;
  FRequestsLock.Free;
  FReadLock.Free;
  FPendingRequests.Free;
  inherited;
end;

procedure TLspClient.HandleInitialize(id: Int64; Result, Error: TJsonValue);
begin
  if Assigned(Result) then
  begin
    var ServerCapabilities := Result.P['capabilities'] as TJSONObject;
    if not Assigned(ServerCapabilities) then Exit;

    ProcessServerCapabilities(ServerCapabilities);
    FStatus := lspInitialized;
    // Send notifications
    Notify('initialized', '{}');
    if Assigned(FOnInitialized) then
      FOnInitialized(Self);
  end;
end;

procedure TLspClient.HandleShutdown(id: Int64; Result, Error: TJsonValue);
begin
  Notify('exit', 'null');
end;

procedure TLspClient.HandleSyncRequest(id: Int64; AResult, AError: TJsonValue);
begin
  if Assigned(AResult) then
  begin
    AResult.Owned := False;
    TSyncRequestHelper(fSyncHelper).Result := AResult;
  end;
  if Assigned(AError) then
  begin
    AError.Owned := False;
    TSyncRequestHelper(fSyncHelper).Error := AError;
  end;
  TSyncRequestHelper(fSyncHelper).SyncEvent.SetEvent;
end;

procedure TLspClient.ReceiveData(const Bytes: TBytes; BytesRead: Cardinal);
var
  Response,
  Result,
  Error,
  Params: TJsonValue;
  method: string;
  ResponseHandler: THandleResponse;
  Id: Int64;
begin
  fReadLock.Enter;
  try
    FReadBuffer := FReadBuffer + Copy(Bytes, 0, BytesRead);
    while Length(FReadBuffer) > 0 do
    begin
      var Content := TEncoding.Utf8.GetString(fReadBuffer);
      var Match := fContentHeaderRE.Match(Content);
      if Match.Success then
      begin
        var BodyLen := StrToInt(Match.Groups['length'].Value);
        // Match.Length should be equal to the bytes count
        // since the header contains only ascii characters
        if BodyLen + Match.Length <= Length(fReadBuffer) then
        begin
          // We have the complete content
          var json: TBytes := Copy(fReadBuffer, Match.Length, BodyLen);
          // keep the additional bytes
          fReadBuffer := Copy(fReadBuffer, Match.Length + BodyLen);
          try
            Response := TJSONObject.ParseJSONValue(json, 0, True);
          except
            // Should not happen
            // Todo:  handle error
            Assert(False, 'TLspClient.ReceiveData');
            Response := nil;
          end;
          if Assigned(Response) then
          begin
            if Response.TryGetValue('method', Method) then
            begin
              if Assigned(Response.FindValue('id'))  then
              begin
                 // Incoming request
                 // not handled yet
              end
              else
              begin
                // Notification
                if Assigned(fOnLspNotification) and
                  Response.TryGetValue('params', Params)
                then
                  fOnLspNotification(Method, Params);
              end;
            end
            else if Response.TryGetValue<Int64>('id', Id) then
            begin
              //OutputDebugString(PChar(Format('Lsp response %d received', [Id])));
              // id, but not method -> should be a response
              FRequestsLock.Enter;
              try
                if FPendingRequests.TryGetValue(Id, ResponseHandler) then
                  fPendingRequests.Remove(Id)
                else
                  ResponseHandler := nil;
              finally
                FRequestsLock.Leave;
              end;
              if Assigned(ResponseHandler) then
              begin
                Error := nil;
                Result := nil;
                if not Response.TryGetValue('result', Result) then
                  Response.TryGetValue('error', Error);
                ResponseHandler(Id, Result, Error);
              end;
            end;
            Response.Free;
          end;
        end
        else
          break;
      end
      else
        break;
    end;
  finally
    FReadLock.Leave;
  end;
end;

procedure TLspClient.ReceiveErrorOutput(const Bytes: TBytes; BytesRead: Cardinal);
begin
  if Assigned(fOnErrorOuput) then
    fOnErrorOuput(TEncoding.UTF8.GetString(Copy(Bytes, 0, BytesRead)));
end;

procedure TLspClient.Initialize(const ClientName, ClientVersion: string;
  ClientCapabilities: TJsonObject;
  InitializationOptions: TJsonObject);
var
  Params: TJsonObject;
begin
  if FStatus <> lspStarted then Exit;

  Params := LSPInitializeParams(ClientName, ClientVersion,
    ClientCapabilities, InitializationOptions);

  Request('initialize', Params.ToJson, HandleInitialize);
  Params.Free;
end;

procedure TLspClient.Notify(const Method, Params: string);
Var
  Header: string;
  Content: string;
  ContentBytes: TBytes;
  HeaderBytes: TBytes;
begin
  Content := Format(LspNotification, [Method, Params]);
  ContentBytes := TEncoding.UTF8.GetBytes(Content);
  Header := Format(LspHeader, [Length(ContentBytes)]);
  HeaderBytes := TEncoding.UTF8.GetBytes(Header);
  SendToServer(HeaderBytes + ContentBytes);
end;

function TLspClient.Request(const Method, Params: string; Handler: THandleResponse): Int64;
Var
  Id: Int64;
  Header: string;
  Content: string;
  ContentBytes: TBytes;
  HeaderBytes: TBytes;
begin
  Id := AtomicIncrement(fId);
  Result := Id;
  Content := Format(LspRequest, [Id, Method, Params]);
  ContentBytes := TEncoding.UTF8.GetBytes(Content);
  Header := Format(LspHeader, [Length(ContentBytes)]);
  HeaderBytes := TEncoding.UTF8.GetBytes(Header);
  FRequestsLock.Enter;
  try
    fPendingRequests.Add(Id, Handler);
  finally
    FRequestsLock.Leave;
  end;
  SendToServer(HeaderBytes + ContentBytes);
end;

procedure TLspClient.SendToServer(const Bytes: TBytes);
begin
  if Assigned(FSendDataThread) then
    TSendDataThread(FSendDataThread).SendToServer(Bytes);
end;

procedure TLspClient.ServerTerminated(Sender: TObject);
begin
  FStatus := lspInactive;
end;

procedure TLspClient.Shutdown;
begin
  if FStatus <> lspInactive then
  begin
    if Assigned(FOnShutdown) then
      FOnShutdown(Self);

    if FStatus = lspInitialized then
    begin
      Request('shutdown', 'null', HandleShutdown);
      FServerThread.WaitFor;
    end;
    FreeAndNil(FServerThread);
    Assert(FStatus = lspInactive);
  end;
  // TThread destroy calls Terminate and then WaitFor
  if Assigned(FSendDataThread) then
    FreeAndNil(FSendDataThread);
end;

procedure TLspClient.StartServer;
begin
  Shutdown;
  FSendDataThread := TSendDataThread.Create;
  FExCmdOptions.OutputBufferCallback := ReceiveData;
  FExCmdOptions.ErrorBufferCallback := ReceiveErrorOutput;
  FExCmdOptions.BufferSize := 65536;
  FServerThread := TLspServerThread.Create(Self, fExCmdOptions);
  FServerThread.OnTerminate := ServerTerminated;
  FServerThread.Start;
  FStatus := lspStarted;
end;

procedure TLspClient.SyncRequest(const Method, Params: string;
  out Result, Error: TJsonValue; Timeout: Cardinal);
begin
  FSyncRequestLock.Enter;
  try
    Result := nil;
    Error := nil;
    TSyncRequestHelper(fSyncHelper).Result := nil;
    TSyncRequestHelper(fSyncHelper).Error := nil;
    TSyncRequestHelper(fSyncHelper).SyncEvent.ResetEvent;
    Request(Method, Params, HandleSyncRequest);
    TSyncRequestHelper(fSyncHelper).SyncEvent.WaitFor(Timeout);
    Result := TSyncRequestHelper(fSyncHelper).Result;
    Error := TSyncRequestHelper(fSyncHelper).Error;
    TSyncRequestHelper(fSyncHelper).Result := nil;
    TSyncRequestHelper(fSyncHelper).Error := nil;
  finally
    FSyncRequestLock.Leave;
  end;
end;

procedure TLspClient.ProcessServerCapabilities(SC: TJsonObject);
  procedure CheckCapability(Name: string; LspCapability: TLspServerCapability);
  var
    Capability: TJsonValue;
  begin
    if SC.TryGetValue(Name, Capability) and not Capability.Null
      and not (Capability is TJSONFalse)
    then
      fServerCapabilities := fServerCapabilities + [LspCapability];
  end;

begin
  fServerCapabilities := [];

  CheckCapability('completionProvider', lspscCompletion);
  CheckCapability('hoverProvider', lspscHover);
  CheckCapability('signatureHelpProvider', lspscSignature);
  CheckCapability('definitionProvider', lspscDefinition);
  CheckCapability('referencesProvider', lspscReferences);
  CheckCapability('documentHighlightProvider', lspscHighlight);
  CheckCapability('documentSymbolProvider', lspscDocSymbol);
  CheckCapability('documentSymbolProvider', lspscDocSymbol);
  CheckCapability('renameProvider', lspscRename);
  CheckCapability('workspace', lspWorkspaceFolders);
  if lspWorkspaceFolders in fServerCapabilities then
  begin
    var Supported := SC.FindValue('workspace.workspaceFolders.supported');
    if not Assigned(Supported) or not (Supported is TJsonTrue) then
      fServerCapabilities := fServerCapabilities - [lspWorkspaceFolders];
  end;
  var TextDocSync := SC.FindValue('textDocumentSync');
  if Assigned(TextDocSync) then
  begin
    // TextDocSync can be either a number or a JSON object
    if TextDocSync is TJsonNumber then
    begin
      if TextDocSync.GetValue<Integer> = 2 then
        fServerCapabilities := fServerCapabilities + [lspscIncrementalSync];
    end
    else
    begin
      var IncrementalSync := TextDocSync.FindValue('change');
      if Assigned(IncrementalSync) and (IncrementalSync.GetValue<Integer> = 2) then
        fServerCapabilities := fServerCapabilities + [lspscIncrementalSync];
    end;
  end;
  CheckCapability('textDocumentSync.openClose', lspscOpenCloseNotify);
  CheckCapability('textDocumentSync.save', lspscSaveNotify);
  CheckCapability('completionProvider.resolveProvider', lspscCompletionResolve);
end;

{ TLspClientThread }

procedure TLspServerThread.BeforeResume(const ProcessInfo: TProcessInformation;
  WriteHandle: THandle);
begin
  fProcessHandle := ProcessInfo.hProcess;
  TSendDataThread(FLspClient.FSendDataThread).SetWriteHandle(WriteHandle);
  FLspClient.FSendDataThread.Start;
end;

constructor TLspServerThread.Create(LspClient: TLspClient;
  ExCmdOptions: TJclExecuteCmdProcessOptions);
begin
  inherited Create(True);
  FLspClient := LspClient;
  fExCmdOptions := ExCmdOptions;
  fAbortEvent := TJclEvent.Create(nil, True, False, '');
  with fExCmdOptions do
  begin
    AbortEvent := fAbortEvent;
    BeforeResume := Self.BeforeResume;
    AutoConvertOem := False;
    RawOutput := True;
    MergeError := False;
    RawError := True;
    BufferSize := 1024*64;
  end;
end;

destructor TLspServerThread.Destroy;
begin
  inherited;
  fAbortEvent.Free;
  fExCmdOptions.AbortEvent := nil;
  fExCmdOptions.BeforeResume := nil;
end;

procedure TLspServerThread.Execute;
begin
  NameThreadForDebugging('Lsp Server');
  ExecuteCmdProcess(fExCmdOptions);
end;

procedure TLspServerThread.TerminatedSet;
begin
  inherited;
  fAbortEvent.Pulse;
end;

{ TSyncRequestHelper }

constructor TSyncRequestHelper.Create;
begin
  SyncEvent := TSimpleEvent.Create(nil, True, False, '');
end;

destructor TSyncRequestHelper.Destroy;
begin
  SyncEvent.Free;
  inherited;
end;

{ TSendDataThread }

constructor TSendDataThread.Create;
begin
  FQueue := TThreadedQueue<TBytes>.Create(10, 100, 200);
  inherited Create(True);
end;

destructor TSendDataThread.Destroy;
begin
  inherited;
  // FQueue needs to be freed after inherited Destoy
  FQueue.Free;
end;

procedure TSendDataThread.Execute;
var
  Bytes: TBytes;
  Written: DWORD;
begin
  NameThreadForDebugging('LSP Send Data');
  while not Terminated do
    if (FQueue.PopItem(Bytes) = TWaitResult.wrSignaled) and (Length(Bytes) > 0) then
    begin
      WriteFile(FWriteHandle, Bytes[0], Length(Bytes), Written, nil);
      //if Written = 0 then
        //TODO: consider aborting the server
        //RaiseLastOSError;
    end;
end;

procedure TSendDataThread.SendToServer(Bytes: TBytes);
begin
  while not (FQueue.PushItem(Bytes) = TWaitResult.wrSignaled) do
    FQueue.Grow(10);
end;

procedure TSendDataThread.SetWriteHandle(Handle: THandle);
begin
  FWriteHandle := Handle;
end;

end.
