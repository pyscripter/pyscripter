{
   LSP client implementation
   (C) PyScripter 2021
}

unit LspClient;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.JSON,
  System.Generics.Collections,
  System.RegularExpressions,
  PascalProcess;

type

  THandleResponse = procedure(Id: NativeUInt; Result, Error: TJSONValue) of object;
  THandleNotify = procedure(const Method: string; Params: TJSONValue) of object;
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
    FId: NativeUInt;
    FServerPath: string;
    FServerProcess: IPProcess;
    FContentHeaderRE: TRegEx;
    FReadLock: TRTLCriticalSection;
    FRequestsLock: TRTLCriticalSection;
    FSyncRequestLock: TRTLCriticalSection;
    FReadBuffer: TBytes;
    FServerCapabilities: TLspServerCapabilities;
    FPendingRequests: TDictionary<NativeUInt, THandleResponse>;
    FSyncHelper: TObject; //TSyncRequestHelper
    FStatus: TLspServerStatus;
    FOnErrorOuput: THandleErrorOutput;
    FOnLspNotification: THandleNotify;
    FOnInitialized: TNotifyEvent;
    FOnShutdown: TNotifyEvent;
    procedure ProcessServerCapabilities(SC: TJSONObject);
    procedure ServerTerminated(Sender: TObject);
    procedure HandleInitialize(Id: NativeUInt; Result, Error: TJSONValue);
    procedure HandleSyncRequest(Id: NativeUInt; AResult, AError: TJSONValue);
    procedure HandleShutdown(Id: NativeUInt; Result, Error: TJSONValue);
    procedure ReceiveData(Sender: TObject; const Bytes: TBytes; BytesRead:
        Cardinal);
    procedure ReceiveErrorOutput(Sender: TObject; const Bytes: TBytes; BytesRead:
        Cardinal);
  public
    constructor Create(ServerPath: string);
    destructor Destroy; override;
    procedure StartServer;
    procedure SendToServer(const Bytes: TBytes);
    // Asynchronous request - Returns unique Id
    // Handler should Free Result and Error
    function Request(const Method, Params: string; Handler: THandleResponse): NativeUInt;
    // Synchronous request - Free Result and Error!
    procedure SyncRequest(const Method, Params: string;
      out Result, Error: TJSONValue; Timeout: Cardinal = INFINITE);
    procedure CancelRequest(Id: NativeUInt);
    procedure Notify(const Method, Params: string);
    // LSP protocol
    procedure Initialize(const ClientName, ClientVersion: string;
      ClientCapabilities: TJSONObject;
      InitializationOptions: TJSONObject = nil);
    procedure Shutdown;
    property Status: TLspServerStatus read FStatus;
    property ServerCapabilities: TLspServerCapabilities read FServerCapabilities;
    property OnErrorOutput: THandleErrorOutput read FOnErrorOuput
      write FOnErrorOuput;
    property OnLspNotification: THandleNotify read FOnLspNotification
      write FOnLspNotification;
    property OnInitialized: TNotifyEvent read FOnInitialized write FOnInitialized;
    property OnShutdown: TNotifyEvent read FOnShutdown write FOnShutdown;
  end;

implementation

uses
  LspUtils,
  uCommonFunctions;

const
   LspHeader =
     'Content-Length: %d'#13#10+
     'Content-Type: application/json; charset=utf-8'#13#10#13#10;

   LspRequest =
     '{"jsonrpc": "2.0", "id": %d, "method": "%s", "params": %s}'#13#10#13#10;

   LspNotification =
     '{"jsonrpc": "2.0", "method": "%s", "params": %s}';

type
  TSyncRequestHelper = class
    SyncEvent: TSimpleEvent;
    Result: TJSONValue;
    Error: TJSONValue;
    constructor Create;
    destructor Destroy; override;
  end;

{ TLspClient }

procedure TLspClient.CancelRequest(Id: NativeUInt);
begin
  FRequestsLock.Enter;
  try
    if FPendingRequests.ContainsKey(Id) then
      FPendingRequests.Remove(Id);
  finally
    FRequestsLock.Leave;
  end;
end;

constructor TLspClient.Create(ServerPath: string);
begin
  inherited Create;
  FServerPath := ServerPath;
  FReadLock.Initialize;
  FRequestsLock.Initialize;
  FSyncRequestLock.Initialize;
  FContentHeaderRE := CompiledRegEx('^(?:[^\r\n]+\r\n)*' +
    'Content-Length: (?P<length>\d+)\r\n' +
    '(?:[^\r\n]+\r\n)*\r\n' +
    '(?={)', //look ahead not part of the capture
    [roMultiLine]);
  FSyncHelper := TSyncRequestHelper.Create;
  FPendingRequests := TDictionary<NativeUInt, THandleResponse>.Create;
end;

destructor TLspClient.Destroy;
begin
  FOnLspNotification := nil;
  FOnErrorOuput := nil;
  Shutdown;
  FSyncHelper.Free;
  FSyncRequestLock.Free;
  FRequestsLock.Free;
  FReadLock.Free;
  FPendingRequests.Free;
  inherited;
end;

procedure TLspClient.HandleInitialize(Id: NativeUInt; Result, Error: TJSONValue);
begin
  if Assigned(Result) then
  begin
    var ServerCapabilities := Result.FindValue('capabilities');
    if Assigned(ServerCapabilities) then
    begin
      ProcessServerCapabilities(ServerCapabilities as TJSONObject);
      FStatus := lspInitialized;
      // Send notifications
      Notify('initialized', '{}');
      if Assigned(FOnInitialized) then
        FOnInitialized(Self);
    end;
    Result.Free;
  end;
  Error.Free;
end;

procedure TLspClient.HandleShutdown(Id: NativeUInt; Result, Error: TJSONValue);
begin
  Notify('exit', 'null');
  Result.Free;
  Error.Free;
end;

procedure TLspClient.HandleSyncRequest(Id: NativeUInt; AResult, AError: TJSONValue);
begin
  if FSyncRequestLock.TryEnter then
    try
    // This is a timed out sync request.  Ignore it.
       AResult.Free;
       AError.Free;
    finally
       FSyncRequestLock.Leave;
    end
  else
  begin
    TSyncRequestHelper(FSyncHelper).Result := AResult;
    TSyncRequestHelper(FSyncHelper).Error := AError;
    TSyncRequestHelper(FSyncHelper).SyncEvent.SetEvent;
  end;
end;

procedure TLspClient.ReceiveData(Sender: TObject; const Bytes: TBytes;
  BytesRead: Cardinal);
var
  Response,
  Result,
  Error,
  Params: TJSONValue;
  Method: string;
  ResponseHandler: THandleResponse;
  Id: NativeUInt;
begin
  FReadLock.Enter;
  try
    FReadBuffer := FReadBuffer + Copy(Bytes, 0, BytesRead);
    while Length(FReadBuffer) > 0 do
    begin
      var Content := TEncoding.UTF8.GetString(FReadBuffer);
      var Match := FContentHeaderRE.Match(Content);
      if Match.Success then
      begin
        var BodyLen := StrToInt(Match.Groups['length'].Value);
        // Match.Length should be equal to the bytes count
        // since the header contains only ascii characters
        if BodyLen + Match.Length <= Length(FReadBuffer) then
        begin
          // We have the complete content
          var Json: TBytes := Copy(FReadBuffer, Match.Length, BodyLen);
          // keep the additional bytes
          FReadBuffer := Copy(FReadBuffer, Match.Length + BodyLen);
          try
            Response := TJSONObject.ParseJSONValue(Json, 0, True);
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
                if Assigned(FOnLspNotification) and
                  Response.TryGetValue('params', Params)
                then
                begin
                  Params.Owned := False;
                  FreeAndNil(Response);
                  FOnLspNotification(Method, Params);
                end;
              end;
            end
            else if Response.TryGetValue<NativeUInt>('id', Id) then
            begin
              // Id, but not Method -> should be a response
              FRequestsLock.Enter;
              try
                if FPendingRequests.TryGetValue(Id, ResponseHandler) then
                  FPendingRequests.Remove(Id)
                else
                  ResponseHandler := nil;
              finally
                FRequestsLock.Leave;
              end;
              if Assigned(ResponseHandler) then
              begin
                Error := nil;
                Result := Response.FindValue('result');
                if Assigned(Result) then
                  Result.Owned := False
                else
                begin
                  Error := Response.FindValue('error');
                  if Assigned(Error) then
                    Error.Owned := False;
                end;
                FreeAndNil(Response);
                ResponseHandler(Id, Result, Error);
              end;
            end;
            Response.Free;
          end;
        end
        else
          Break;
      end
      else
        Break;
    end;
  finally
    FReadLock.Leave;
  end;
end;

procedure TLspClient.ReceiveErrorOutput(Sender: TObject; const Bytes: TBytes;
  BytesRead: Cardinal);
begin
  if Assigned(FOnErrorOuput) then
    FOnErrorOuput(TEncoding.UTF8.GetString(Copy(Bytes, 0, BytesRead)));
end;

procedure TLspClient.Initialize(const ClientName, ClientVersion: string;
  ClientCapabilities: TJSONObject;
  InitializationOptions: TJSONObject);
var
  Params: TJSONObject;
begin
  if FStatus <> lspStarted then Exit;

  Params := LSPInitializeParams(ClientName, ClientVersion,
    ClientCapabilities, InitializationOptions);

  Request('initialize', Params.ToJSON, HandleInitialize);
  Params.Free;
end;

procedure TLspClient.Notify(const Method, Params: string);
var
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

function TLspClient.Request(const Method, Params: string; Handler: THandleResponse): NativeUInt;
var
  Id: NativeUInt;
  Header: string;
  Content: string;
  ContentBytes: TBytes;
  HeaderBytes: TBytes;
begin
  Id := AtomicIncrement(FId);
  Result := Id;
  Content := Format(LspRequest, [Id, Method, Params]);
  ContentBytes := TEncoding.UTF8.GetBytes(Content);
  Header := Format(LspHeader, [Length(ContentBytes)]);
  HeaderBytes := TEncoding.UTF8.GetBytes(Header);
  FRequestsLock.Enter;
  try
    FPendingRequests.Add(Id, Handler);
  finally
    FRequestsLock.Leave;
  end;
  SendToServer(HeaderBytes + ContentBytes);
end;

procedure TLspClient.SendToServer(const Bytes: TBytes);
begin
  if Assigned(FServerProcess) then
    FServerProcess.WriteProcessInput(Bytes);
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
      FServerProcess.WaitFor;
    end;
    FServerProcess := nil;
    Assert(FStatus = lspInactive, 'Shutdown');
  end;
end;

procedure TLspClient.StartServer;
begin
  Shutdown;
  FServerProcess := TPProcess.Create(FServerPath);
  FServerProcess.BufferSize := $10000;
  FServerProcess.OnRead := ReceiveData;
  FServerProcess.OnErrorRead := ReceiveErrorOutput;
  FServerProcess.OnTerminate := ServerTerminated;
  FServerProcess.Execute;
  FStatus := lspStarted;
end;

procedure TLspClient.SyncRequest(const Method, Params: string;
  out Result, Error: TJSONValue; Timeout: Cardinal);
begin
  FSyncRequestLock.Enter;
  try
    Result := nil;
    Error := nil;
    TSyncRequestHelper(FSyncHelper).Result := nil;
    TSyncRequestHelper(FSyncHelper).Error := nil;
    TSyncRequestHelper(FSyncHelper).SyncEvent.ResetEvent;
    Request(Method, Params, HandleSyncRequest);
    TSyncRequestHelper(FSyncHelper).SyncEvent.WaitFor(Timeout);
    Result := TSyncRequestHelper(FSyncHelper).Result;
    Error := TSyncRequestHelper(FSyncHelper).Error;
    TSyncRequestHelper(FSyncHelper).Result := nil;
    TSyncRequestHelper(FSyncHelper).Error := nil;
  finally
    FSyncRequestLock.Leave;
  end;
end;

procedure TLspClient.ProcessServerCapabilities(SC: TJSONObject);
  procedure CheckCapability(Name: string; LspCapability: TLspServerCapability);
  var
    Capability: TJSONValue;
  begin
    if SC.TryGetValue(Name, Capability) and not Capability.Null
      and not (Capability is TJSONFalse)
    then
      FServerCapabilities := FServerCapabilities + [LspCapability];
  end;

begin
  FServerCapabilities := [];

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
  if lspWorkspaceFolders in FServerCapabilities then
  begin
    var Supported := SC.FindValue('workspace.workspaceFolders.supported');
    if not Assigned(Supported) or not (Supported is TJSONTrue) then
      FServerCapabilities := FServerCapabilities - [lspWorkspaceFolders];
  end;
  var TextDocSync := SC.FindValue('textDocumentSync');
  if Assigned(TextDocSync) then
  begin
    // TextDocSync can be either a number or a JSON object
    if TextDocSync is TJSONNumber then
    begin
      if TextDocSync.GetValue<Integer> = 2 then
        FServerCapabilities := FServerCapabilities + [lspscIncrementalSync];
    end
    else
    begin
      var IncrementalSync := TextDocSync.FindValue('change');
      if Assigned(IncrementalSync) and (IncrementalSync.GetValue<Integer> = 2) then
        FServerCapabilities := FServerCapabilities + [lspscIncrementalSync];
    end;
  end;
  CheckCapability('textDocumentSync.openClose', lspscOpenCloseNotify);
  CheckCapability('textDocumentSync.save', lspscSaveNotify);
  CheckCapability('completionProvider.resolveProvider', lspscCompletionResolve);
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

end.
