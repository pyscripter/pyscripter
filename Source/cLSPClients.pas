{-----------------------------------------------------------------------------
 Unit Name: cLSPClients
 Author:    PyScripter
 Date:      24-Sep-2025
 Purpose:   Implement LSP-client functionality
-----------------------------------------------------------------------------}

unit cLSPClients;

interface

uses
  WinApi.Windows,
  System.SysUtils,
  System.Messaging,
  System.SyncObjs,
  System.JSON,
  System.Generics.Collections,
  XLspTYpes,
  XLspClient,
  SynEdit,
  SynEditTypes;

type

{$REGION 'TPyLspClient'}
  TDocPosition = record
    FileId: string;
    Line: Integer;
    Char: Integer;
  end;

  TPyLspClient = class
  private
    FLspClient: TLSPClient;
    procedure PythonVersionChanged(const Sender: TObject;
      const Msg: System.Messaging.TMessage);
  protected
    FProjectPythonPath: TArray<string>;
    function GetCommandLine: string; virtual; abstract;
    procedure HandleProjectPythonPathChange(const Sender: TObject;
      const Msg: System.Messaging.TMessage);
    procedure OnInitialize(Sender: TObject;
      var value: TLSPInitializeParams); virtual; abstract;
    procedure OnInitialized(Sender: TObject; var value: TLSPInitializeResult);
    procedure OnServerConnected(Sender: TObject);
    procedure OnExit(Sender: TObject; exitCode: Integer;
      const bRestartServer: Boolean);
    procedure CompletionHandler(AJson: TJsonObject);
    procedure SignatureHelpHandler(AJson: TJsonObject);
    procedure OnResponseError(Sender: TObject; const id, errorCode: Integer;
      const errorMsg: string; retriggerRequest: Boolean = False);
  public
    // class vars and methods
    class var LspClients: TObjectList<TPyLspClient>;
    class var MainLspClient: TPyLspClient;
    class var SyncRequestTimeout: Integer;
    class constructor Create;
    class destructor Destroy;

    constructor Create;
    destructor Destroy; override;
    procedure CreateServer; virtual;
    procedure CreateAndRunServer;
    function Ready: Boolean;
    property CommandLine: string read GetCommandLine;
    property LspClient: TLSPClient read FLspClient;

    // lsp Functionality
    /// <summary>Find the definition of the symbol at a given position</summary>
    procedure FindDefinitionByCoordinates(const FileId: string;
      const BC: TBufferCoord; out DefFileName: string; out DefBC: TBufferCoord);
    /// <summary>Find references of the symbol at a given position</summary>
    function FindReferencesByCoordinates(FileId: string;
      const BC: TBufferCoord): TArray<TDocPosition>;
    /// <summary>Requests completion assynchrously</summary>
    function HandleCodeCompletion(const FileId: string;
      const BC: TBufferCoord; out InsertText, DisplayText: string): Boolean;
    /// <summary>Get the documentation of a completion item</summary>
    function ResolveCompletionItem(CCItem: string): string;
    /// <summary>Get the document symbols synchronously</summary>
    function DocumentSymbols(const FileId: string): TLSPDocumentSymbols;
    /// <summary>Get a simple hint synchronously</summary>
    procedure SignatureHelp(const AFileId: string; Editor: TCustomSynEdit);
    /// <summary>Get a code hint for an editor identifier</summary>
    function SimpleHintAtCoordinates(const FileId: string;
      const BC: TBufferCoord): string;
    /// <summary>Like SimpleHintAtCoordinates but includes definition info</summary>
    function CodeHintAtCoordinates(const FileId: string;
      const BC: TBufferCoord; const Ident: string): string;
  end;

{$ENDREGION 'TPyLspClient'}


{$REGION 'TJediClient'}

  TJediClient = class(TPyLspClient)
  protected
    function GetCommandLine: string; override;
    procedure OnInitialize(Sender: TObject;
      var Params: TLSPInitializeParams); override;
  end;

{$ENDREGION 'TJediClient'}

// Notifications
  TLspServerInitializedMessage = class(System.Messaging.TMessage);
  TLspServerShutDownMessage = class(System.Messaging.TMessage);

{$REGION 'Utility functions'}
function LspPosition(const BC: TBufferCoord): TLSPPosition;
function LspRange(const BlockBegin, BlockEnd: TBufferCoord): TLSPRange;
function BufferCoordFromLspPosition(const Pos: TLSPPosition): TBufferCoord;
procedure BufferCoordinatesFromLspRange(const Range: TLSPRange;
  out BB, BE: TBufferCoord);
function FileIdToURI(const FilePath: string): string;
function FileIdFromURI(const URI: string): string;

{$ENDREGION 'Utility functions'}

implementation
uses
  System.Classes,
  System.Character,
  System.IOUtils,
  System.NetEncoding,
  System.RegularExpressions,
  System.Generics.Defaults,
  cPySupportTypes,
  XLSPFunctions,
  JvGnugettext,
  StringResources,
  dmResources,
  dmCommands,
  uEditAppIntfs,
  uCommonFunctions,
  cCodeCompletion,
  cPyScripterSettings,
  cSSHSupport;

{$REGION 'Utility functions'}

{ TDocPosition }

function LspPosition(const BC: TBufferCoord): TLSPPosition;
begin
  Result.line := BC.Line - 1;
  Result.character := BC.Char - 1;
end;

function LspRange(const BlockBegin, BlockEnd: TBufferCoord): TLSPRange;
begin
  Result.start := LspPosition(BlockBegin);
  Result.&end :=  LspPosition(BlockEnd);
end;

function BufferCoordFromLspPosition(const Pos: TLSPPosition): TBufferCoord;
begin
  Result := BufferCoord(Pos.character + 1, Pos.line + 1);
end;

procedure BufferCoordinatesFromLspRange(const Range: TLSPRange;
  out BB, BE: TBufferCoord);
begin
  BB := BufferCoordFromLspPosition(Range.start);
  BE := BufferCoordFromLspPosition(Range.&end);
end;

function FileIdToURI(const FilePath: string): string;
var
  ServerName, FileName: string;
begin
  if FindDelimiter(':\/', FilePath) > 0 then
  begin
    if TSSHFileName.Parse(FilePath, ServerName, FileName) then
    begin
      if not CharInSet(FileName[1], ['\', '/']) then
        FileName := '\' + FileName;
      Result := 'file://SSH/' + ServerName + FileName;
    end
    else
      Result := FilePathToURI(FilePath);
  end
  else
    // Not sure how to handle unsaved files
    // and used the following workaround
    Result := 'file:///c:/untitled/temp/'+ FilePath;
end;

function FileIdFromURI(const URI: string): string;
begin
  if URI.StartsWith('file:///c:/untitled/temp/') then
  begin
    Result := Copy(URI, 26);
  end
  else if URI.StartsWith('file://SSH/') then
  begin
    var FilePath := Copy(URI, 12);
    var Server := StrToken(FilePath, '/');
    FilePath := TNetEncoding.URL.Decode(FilePath, []);
    if FilePath[2] <> DriveDelim then
       FilePath := '/' + FilePath;
    Result := TSSHFileName.Format(Server, FilePath);
  end
  else
    Result := URIToFilePath(URI);
end;

{$ENDREGION 'Utility functions'}


{$REGION 'TPyLspClient'}

function TPyLspClient.CodeHintAtCoordinates(const FileId: string;
  const BC: TBufferCoord; const Ident: string): string;
var
  DefFileName: string;
  DefBC: TBufferCoord;
  ModuleName: string;
  DefinedIn: string;
begin
  if not (Ready and FLspClient.IsRequestSupported(lspHover)) then Exit;

  Result := SimpleHintAtCoordinates(FileId, BC);

  if Result = '' then Exit;

  // In some corner cases the hint may contain html that may cause exceptions
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, '&lt;br&gt;', '<br>', [rfReplaceAll]);
  Result := StringReplace(Result, '<br>---<br>', '<hr>', [rfReplaceAll]);

  FindDefinitionByCoordinates(FileId, BC, DefFileName, DefBC);
  if (DefFileName <> '') then begin
    if FileIsPythonPackage(DefFileName)
    then
      ModuleName := FileNameToModuleName(DefFileName)
    else
      ModuleName := TPath.GetFileName(DefFileName);
    ModuleName := TPath.GetFileNameWithoutExtension(ModuleName);
    DefinedIn := Format(_(SFilePosInfoCodeHint),
      [DefFileName, DefBC.Line, DefBC.Char,
       ModuleName, DefBC.Line]);
    Result := Result + DefinedIn;
  end;
end;

constructor TPyLspClient.Create;
begin
  inherited Create;

  // Notifications
  TMessageManager.DefaultManager.SubscribeToMessage(TPythonVersionChangeMessage,
    PythonVersionChanged);
  TMessageManager.DefaultManager.SubscribeToMessage(TProjectPythonPathChangeMessage,
    HandleProjectPythonPathChange);
end;

class constructor TPyLspClient.Create;
begin
  SyncRequestTimeout := 4000; // ms
  LspClients := TObjectList<TpyLspClient>.Create(True);
  MainLspClient := TJediClient.Create;
  LspClients.Add(MainLspClient);
end;

procedure TPyLspClient.CreateAndRunServer;
begin
  CreateServer;
  FLspClient.RunServer(CommandLine, '');
end;

procedure TPyLspClient.CreateServer;
begin
  // Destroy the client and server if they exist
  if Assigned(FLspClient) then
  begin
    TMessageManager.DefaultManager.SendMessage(Self,
      TLspServerShutDownMessage.Create);
    FreeAndNil(FLspClient);
  end;

  // Create the LSP client and server
  FLspClient := TLSPClient.Create(nil);
  FLspClient.ClientName := 'PyScripter';
  FLspClient.ClientVersion := ApplicationVersion;
  FLspClient.OnInitialize := OnInitialize;
  FLspClient.OnInitialized := OnInitialized;
  FLspClient.OnError := OnResponseError;
  FLspClient.OnServerConnected := OnServerConnected;
end;

destructor TPyLspClient.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TPythonVersionChangeMessage,
    PythonVersionChanged);
  TMessageManager.DefaultManager.Unsubscribe(TProjectPythonPathChangeMessage,
    HandleProjectPythonPathChange);

  TMessageManager.DefaultManager.SendMessage(Self,
    TLspServerShutDownMessage.Create);
  FLspClient.Free;
  inherited;
end;

class destructor TPyLspClient.Destroy;
begin
  FreeAndNil(LspClients);
end;

function TPyLspClient.DocumentSymbols(const FileId: string):
    TLSPDocumentSymbols;
var
  Symbols: TLSPDocumentSymbols;
begin
  if not (Ready and FLspClient.IsRequestSupported(lspDocumentSymbol)) then Exit;

  Symbols := [];

  var Params := TSmartPtr.Make(TLSPDocumentSymbolParams.Create)();
  Params.textDocument.uri := FileIdToURI(FileId);

  FLspClient.SendSyncRequest(lspDocumentSymbol, Params,
    procedure(AJson: TJsonObject)
    begin
      if ResponseError(AJson) then Exit;
      var JsonResult := AJson.Values['result'];
      if JsonResult.Null then Exit;
      var Results :=
        TSmartPtr.Make(JsonDocumentSymbolsResultToObject(JsonResult));
      Symbols := Results.symbols;
    end,
    SyncRequestTimeout);
  Result := Symbols;
end;

procedure TPyLspClient.FindDefinitionByCoordinates(const FileId: string;
  const BC: TBufferCoord; out DefFileName: string; out DefBC: TBufferCoord);
var
  FName: string;
  ResBC: TBufferCoord;
begin
  if not (Ready and FLspClient.IsRequestSupported(lspGotoDefinition)) then Exit;

  ResBC := TBufferCoord.Invalid;
  var Params := TSmartPtr.Make(TLSPDefinitionParams.Create)();
  Params.textDocument.uri := FileIdToURI(FileId);
  Params.position := LspPosition(BC);

  FLspClient.SendSyncRequest(lspGotoDefinition, Params,
    procedure(AJson: TJsonObject)
    var
      Loc: TLSPLocation;
    begin
      if ResponseError(AJson) then Exit;
      var JsonResult := AJson.Values['result'];
      if JsonResult.Null then Exit;
      var Results :=
        TSmartPtr.Make(JsonGotoResultToObject(JsonResult))();
      if Length(Results.locations) > 0 then
        Loc := Results.locations[0]
      else
        Loc := Results.location;

      FName := FileIdFromURI(Loc.uri);
      ResBC := BufferCoordFromLspPosition(Loc.range.start);
    end,
    SyncRequestTimeout);

  DefFileName := FName;
  DefBC := ResBC;
end;

function TPyLspClient.FindReferencesByCoordinates(FileId: string;
  const BC: TBufferCoord): TArray<TDocPosition>;
var
  Locations: TArray<TLSPLocation>;
begin
  if not (Ready and FLspClient.IsRequestSupported(lspReferences)) then Exit;

  var Params := TSmartPtr.Make(TLSPReferencesParams.Create)();
  Params.textDocument.uri := FileIdToURI(FileId);
  Params.position := LspPosition(BC);
  Params.context.includeDeclaration := True;

  Locations := [];
  FLspClient.SendSyncRequest(lspReferences, Params,
    procedure(AJson: TJsonObject)
    begin
      if ResponseError(AJson) then Exit;
      var JsonResult := AJson.Values['result'];
      if JsonResult.Null then Exit;
      var Results :=
        TSmartPtr.Make(JsonFindReferencesResultToObject(JsonResult))();
      Locations := Results.locations;
    end,
    SyncRequestTimeout);

  SetLength(Result, Length(Locations));

  for var I := Low(Locations) to High(Locations) do
  begin
    Result[I].FileId := FileIdFromURI(Locations[I].uri);
    Result[I].Line := Locations[I].range.start.line + 1;
    Result[I].Char := Locations[I].range.start.character + 1;
  end;
end;

function TPyLspClient.HandleCodeCompletion(const FileId: string;
  const BC: TBufferCoord; out InsertText, DisplayText: string): Boolean;
begin
  if not (Ready and FLspClient.IsRequestSupported(lspCompletion)) then
    Exit(False);

  var Params := TSmartPtr.Make(TLSPCompletionParams.Create)();
  Params.textDocument.uri := FileIdToURI(FileId);
  Params.position := LspPosition(BC);
  TIDECompletion.CompletionInfo.Id :=
    FLspClient.SendRequest(lspCompletion, Params,
      procedure(AJson: TJSONObject)
      begin
        CompletionHandler(AJson);
      end);
  Result := True;
end;

procedure TPyLspClient.HandleProjectPythonPathChange(const Sender: TObject;
  const Msg: System.Messaging.TMessage);
begin
  var NewPath := TProjectPythonPathChangeMessage(Msg).Value;

  if Ready and
    FLspClient.IsRequestSupported(lspDidChangeWorkspaceFolders) then
  begin
    var Params := TSmartPtr.Make(TLSPDidChangeWorkspaceFoldersParams.Create)();
    Params.event := Default(TLSPWorkspaceFoldersChangeEvent);
    var Comparer: IComparer<string> := TIStringComparer.Ordinal;

    // Remove old entries
    for var Item in FProjectPythonPath do
      if not TArray.Contains<string>(NewPath, Item, Comparer) then
      begin
        var Folder := Default(TLSPWorkspaceFolder);
        Folder.uri := FilePathToURI(Item);
        Folder.name := TPath.GetFileNameWithoutExtension(Item);
        Params.event.removed := Params.event.removed + [Folder];
      end;
    for var Item in NewPath do
      if not TArray.Contains<string>(FProjectPythonPath, Item, Comparer) then
      begin
        var Folder := Default(TLSPWorkspaceFolder);
        Folder.uri := FilePathToURI(Item);
        Folder.name := TPath.GetFileNameWithoutExtension(Item);
        Params.event.added := Params.event.added + [Folder];
      end;
    if (Length(Params.event.removed) > 0) or (Length(Params.event.added) > 0) then
      FLspClient.SendNotification(lspDidChangeWorkspaceFolders, Params);
  end;
  FProjectPythonPath := NewPath;
end;

procedure TPyLspClient.OnServerConnected(Sender: TObject);
begin
 FLspClient.SendRequest(lspInitialize);
end;

procedure TPyLspClient.CompletionHandler(AJson: TJsonObject);


  function KindToImageIndex(Kind: Integer): Integer;
  begin
    case Kind of
      TLspCompletionItemKind.cConstructor,
      TLspCompletionItemKind.cMethod:     Result := Integer(TCodeImages.Method);
      TLspCompletionItemKind.cFunction:  Result := Integer(TCodeImages.Func);
      TLspCompletionItemKind.cVariable:   Result := Integer(TCodeImages.Variable);
      TLspCompletionItemKind.cClass:     Result := Integer(TCodeImages.Klass);
      TLspCompletionItemKind.cModule:     Result := Integer(TCodeImages.Module);
      TLspCompletionItemKind.cField,
      TLspCompletionItemKind.cProperty:  Result := Integer(TCodeImages.Field);
      TLspCompletionItemKind.cKeyword:    Result := Integer(TCodeImages.Keyword);
    else
      Result := -1;
    end;
  end;

  function ToLabel(Item: TLSPCompletionItem): string;
  begin
    case Item.Kind of
      TLspCompletionItemKind.cConstructor,
      TLspCompletionItemKind.cMethod,
      TLspCompletionItemKind.cFunction:
      Result := Item.&Label + '()'
    else
      Result := Item.&Label;
    end;
  end;

var
  InsertText, DisplayText: string;
begin
  if ResponseError(AJson) then Exit;
  var JsonResult := AJson.Values['result'];
  if JsonResult.Null then Exit;

  var Id := AJson.GetValue<Integer>('id', -1);

  var CompletionList :=
    TSmartPtr.Make(JsonCompletionResultToObject(JsonResult))();

  if Length(CompletionList.items) > 0 then
  begin
    TIDECompletion.CompletionLock.Enter;
    try
      // Check we deal with the most recent completion request
      if Id <> TIDECompletion.CompletionInfo.Id then Exit;

      // process completion items
      InsertText := '';
      DisplayText := '';
      for var Item in CompletionList.items do
      begin
        InsertText := InsertText + ToLabel(Item) + #10;
        var ImageIndex := KindToImageIndex(Item.kind);
        DisplayText := DisplayText + Format('\Image{%d}\hspace{8}%s',
          [ImageIndex, Item.&label]) + #10;
      end;
      TIDECompletion.CompletionInfo.InsertText := InsertText;
      TIDECompletion.CompletionInfo.DisplayText := DisplayText;
    finally
      TIDECompletion.CompletionLock.Leave;
    end;

    TThread.Queue(nil, procedure
    begin
      CommandsDataModule.SynCodeCompletion.ActivateCompletion;
    end)
  end;
end;

procedure TPyLspClient.OnExit(Sender: TObject; exitCode: Integer;
  const bRestartServer: Boolean);
begin
  TMessageManager.DefaultManager.SendMessage(Self,
    TLspServerShutDownMessage.Create);

  if bRestartServer then
  TThread.ForceQueue(nil,
  procedure
  begin
    CreateAndRunServer;
  end);
end;

procedure TPyLspClient.OnInitialized(Sender: TObject;
  var value: TLSPInitializeResult);
begin
  TThread.ForceQueue(nil, procedure
  begin
    TMessageManager.DefaultManager.SendMessage(Self,
      TLspServerInitializedMessage.Create);
  end);
end;


procedure TPyLspClient.SignatureHelpHandler(AJson: TJsonObject);
begin
  var SignatureInfo := TIDECompletion.SignatureHelpInfo;
  SignatureInfo.Lock;
  try
    var Id := AJson.GetValue<Integer>('id', -1);
    // Check we deal with the most recent signature help request
    if Id <> SignatureInfo.RequestId then Exit;

    // Mark the request as handled
    SignatureInfo.Handled := True;

    if ResponseError(AJson) then Exit;
    var JsonResult := AJson.Values['result'];
    if JsonResult.Null then Exit;

    var SigRes := TSmartPtr.Make(JsonSignatureHelpResultToObject(JsonResult))();

    SignatureInfo.Succeeded := Length(SigRes.signatures) > 0;
    if not SignatureInfo.Succeeded then Exit;

    var ActiveSignature := SigRes.activeSignature;
    SignatureInfo.DisplayString := SigRes.signatures[ActiveSignature].&label;
    SignatureInfo.DocString := SigRes.signatures[ActiveSignature].documentationMarkup.value;
    SignatureInfo.ActiveParameter := SigRes.activeParameter;

    if SignatureInfo.DisplayString.StartsWith('class') then
      Delete(SignatureInfo.DisplayString, 1, 5)
    else if SignatureInfo.DisplayString.StartsWith('def') then
      Delete(SignatureInfo.DisplayString, 1, 3);

    var RightPar := SignatureInfo.DisplayString.LastDelimiter(')');
    if RightPar >= 0 then
      Delete(SignatureInfo.DisplayString, RightPar + 1, 1);

    SignatureInfo.StartX := 1;
    var LeftPar := SignatureInfo.DisplayString.IndexOf('(');
    if LeftPar >= 0 then
    begin
      var FunctionName := Copy(SignatureInfo.DisplayString, 1, LeftPar).Trim;
      SignatureInfo.DisplayString := Copy(SignatureInfo.DisplayString, LeftPar + 2);
      var Match := TRegEx.Match(SignatureInfo.CurrentLine, FunctionName + '\s*(\()');
      if Match.Success then
        SignatureInfo.StartX := Match.Groups[1].Index + 1;
    end;

    TThread.Queue(nil, procedure
    begin
      // ActivateCompletion calls SynParamCompletionExecute
      CommandsDataModule.SynParamCompletion.ActivateCompletion;
    end);
  finally
    SignatureInfo.UnLock;
  end;
end;

procedure TPyLspClient.OnResponseError(Sender: TObject; const id,
  errorCode: Integer; const errorMsg: string; retriggerRequest: Boolean);
begin
  if Id = TIDECompletion.CompletionInfo.Id then
    TIDECompletion.CompletionInfo.CleanUp;
end;

procedure TPyLspClient.PythonVersionChanged(const Sender: TObject;
  const Msg: System.Messaging.TMessage);
begin
  CreateAndRunServer;
end;

function TPyLspClient.Ready: Boolean;
begin
  Result := Assigned(FLspClient) and FLspClient.Initialized;
end;

procedure TPyLspClient.SignatureHelp(const AFileId: string;
  Editor: TCustomSynEdit);
begin
  if not (Ready and FLspClient.IsRequestSupported(lspSignatureHelp)) or
    AFileId.IsEmpty
  then
    Exit;

  // Get Completion for the current word
  var Line := Editor.LineText;

  var TmpX := Editor.CaretX;
  if TmpX > Length(Line) then
    TmpX := Length(Line) + 1;
  while (TmpX > 1) and ((Line[TmpX-1] = '_') or Line[TmpX-1].IsLetterOrDigit) do
    Dec(TmpX);

  var Params := TSmartPtr.Make(TLSPTextDocumentPositionParams.Create)();
  Params.textDocument.uri := FileIdToURI(AFileId);
  Params.position := LspPosition(BufferCoord(TmpX, Editor.CaretY));

  TIDECompletion.SignatureHelpInfo.Lock;
  try
    TIDECompletion.SignatureHelpInfo.FileId := AFileId;
    TIDECompletion.SignatureHelpInfo.Caret := Editor.CaretXY;
    TIDECompletion.SignatureHelpInfo.CurrentLine := Line;
    TIDECompletion.SignatureHelpInfo.Handled := False;
    TIDECompletion.SignatureHelpInfo.RequestId :=
      FLspClient.SendRequest(lspSignatureHelp, Params,
      procedure(AJson: TJSONObject)
      begin
        SignatureHelpHandler(AJson);
      end);
  finally
    TIDECompletion.SignatureHelpInfo.UnLock;
  end;
end;

function TPyLspClient.ResolveCompletionItem(CCItem: string): string;
var
  Str: string;
const
  paramTemplate = '{"label": "%s"}';
begin
  if not (Ready and FLspClient.IsRequestSupported(lspCompletionItemResolve)) then Exit('');

  FLspClient.SendSyncRequest(lspCompletionItemResolve, nil,
    procedure(AJson: TJsonObject)
    begin
      if ResponseError(AJson) then Exit;
      var JsonResult := AJson.Values['result'];
      if JsonResult.Null then Exit;
      var Results :=
        TSmartPtr.Make(JsonCompletionItemResolveResultToObject(JsonResult))();
      Str := Results.completionItem.documentationMarkup.value;
    end,
    SyncRequestTimeout div 10,
    Format(paramTemplate, [CCItem]));
  Result := Str;
end;

function TPyLspClient.SimpleHintAtCoordinates(const FileId: string;
  const BC: TBufferCoord): string;
var
  Str: string;
begin
  if not (Ready and FLspClient.IsRequestSupported(lspHover)) then Exit('');

  var Params := TSmartPtr.Make(TLSPHoverParams.Create)();
  Params.textDocument.uri := FileIdToURI(FileId);
  Params.position := LspPosition(BC);

  FLspClient.SendSyncRequest(lspHover, Params,
    procedure(AJson: TJsonObject)
    begin
      if ResponseError(AJson) then Exit;
      var JsonResult := AJson.Values['result'];
      if JsonResult.Null then Exit;
      var Results :=
        TSmartPtr.Make(JsonHoverResultToObject(JsonResult))();
      Str := Results.contents.value;
      if Str <> '' then
        Str := GetLineRange(Str, 1, 20, True);
    end,
    SyncRequestTimeout div 4);
  Result := Str;
end;

{$ENDREGION 'TPyLspClient'}


{$REGION 'TJediClient'}

function TJediClient.GetCommandLine: string;
const
  LspDebugFile = 'JediLspDebug.log';
begin
  if not GI_PyControl.PythonLoaded then Exit;

  var ServerPath :=
    TPath.Combine(TPyScripterSettings.LspServerPath,
    'jls\run-jedi-language-server.py');
  if not FileExists(ServerPath) then Abort;

  Result := Format('"%s" -u "%s"',
    [GI_PyControl.PythonVersion.PythonExecutable, ServerPath]);
  if PyIDEOptions.LspDebug then
  begin
    Result := Format('%s -v --log-file "%s"', [Result,
      TPath.Combine(TPyScripterSettings.UserDataPath, LspDebugFile)]);
  end;
end;

procedure TJediClient.OnInitialize(Sender: TObject;
  var Params: TLSPInitializeParams);
const
   // extrapath

   InitializationOptionsLsp =
    '{'#13#10 +
    '    "diagnostics": {'#13#10 +
    '      "enable": true,'#13#10 +
    '      "didOpen": true,'#13#10 +
    '      "didChange": %s,'#13#10 +
    '      "didSave": true'#13#10 +
    '    },'#13#10 +
    '   "completion": {'#13#10 +
    '       "disableSnippets": true,'#13#10 +
    '       "resolveEagerly": false'#13#10 +
    '   },'#13#10 +
//    '   "markupKindPreferred": "markdown",'#13#10 +
    '   "jediSettings": {'#13#10 +
    '     "autoImportModules": [%s],'#13#10 +
    '     "caseInsensitiveCompletion": %s'#13#10 +
    '   },'#13#10 +
    '   "workspace": {'#13#10 +
    '     "extraPaths": [%s]'#13#10 +
    '   }'#13#10 +
    '}';

  function QuotePackages(Packages: string): string;
  begin
    var Arr := Packages.Split([',']);
    if Length(Arr) = 0 then Exit('');

    for var I := 0 to Length(Arr) - 1 do
      Arr[I] := '"' + Trim(Arr[I]) + '"';

    Result := string.Join(',', Arr);
  end;

  function ExtraPaths: string;
  begin
    var Paths := Copy(FProjectPythonPath);
    for var I := Low(Paths) to High(Paths) do
      Paths[I] := '"' + Paths[I] + '"';
    Result := string.Join(',', Paths);
    Result := Result.Replace('\', '/  ', [rfReplaceAll]);
  end;

begin
  Params.capabilities.AddSynchronizationSupport(True, False, False, False);
  Params.capabilities.AddDocumentSymbolSupport(True, False, False);
  Params.capabilities.AddCompletionSupport(False, False, False, True, False,
    False, False, False, False);
  Params.capabilities.AddDefinitionSupport(False);
  Params.capabilities.AddReferencesSupport(False);
  Params.capabilities.AddHoverSupport(False, True, True);
  Params.capabilities.AddPublishDiagnosticsSupport(False, True, True, False);
  if Length(FProjectPythonPath) > 0 then
  begin
    Params.AddWorkspaceFolders(FProjectPythonPath);
    Params.AddRoot(FProjectPythonPath[0]);
  end;
  Params.initializationOptions :=
    Format(InitializationOptionsLsp,
    [BoolToStr(PyIDEOptions.CheckSyntaxAsYouType, True).ToLower,
     QuotePackages(PyIDEOptions.SpecialPackages),
     BoolToStr(not PyIDEOptions.CodeCompletionCaseSensitive, True).ToLower,
     ExtraPaths]);
end;

{$ENDREGION 'TJediClient'}


end.
