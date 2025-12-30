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
  TB2Item,
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
    class var FProjectPythonPath: TArray<string>;
    class procedure PythonVersionChanged(const Sender: TObject;
      const Msg: System.Messaging.TMessage);
    class procedure ProjectPythonPathChanged(const Sender: TObject;
      const Msg: System.Messaging.TMessage);
  protected
    procedure ClientCapabilities(Capabilities: TLSPClientCapabilities);
    procedure CreateServer; virtual;
    function GetCommandLine: string; virtual; abstract;
    function LogFileName: string; virtual; abstract;
    // Client event handlers
    procedure OnExit(Sender: TObject; exitCode: Integer;
      const bRestartServer: Boolean);
    procedure OnInitialize(Sender: TObject;
      var value: TLSPInitializeParams); virtual; abstract;
    procedure OnInitialized(Sender: TObject; var value: TLSPInitializeResult);
    procedure OnResponseError(Sender: TObject; const id, errorCode: Integer;
      const errorMsg: string; retriggerRequest: Boolean = False);
    procedure OnServerConnected(Sender: TObject);
    procedure OnShowMessage(Sender: TObject; const ntype: TLSPMessageType;
      const msg: string);
    // Completion handlers
    procedure CompletionHandler(AJson: TJsonObject);
    procedure SignatureHelpHandler(AJson: TJsonObject);
  public
    // class vars and methods
    class var LspClients: TObjectList<TPyLspClient>;
    class var MainLspClient: TPyLspClient;
    class var DiagnosticsLspClient: TPyLspClient;
    class var SyncRequestTimeout: Integer;
    class var CodeActions: TLSPCodeActionResult; // for temp storing of CodeActions
    class constructor Create;
    class destructor Destroy;
    class procedure RestartServers;

    destructor Destroy; override;
    procedure CreateAndRunServer;
    function Ready: Boolean;
    property CommandLine: string read GetCommandLine;
    property LspClient: TLSPClient read FLspClient;

    // lsp Functionality
    /// <summary>Get available code actions for a given location</summary>
    procedure GetCodeActions(const FileId: string; BB, BE: TBufferCoord);
    /// <summary>Execute a server command on the given file</summary>
    procedure ExecuteCommand(const Command, FileId: string; Version: Integer);
    /// <summary>Find the definition of the symbol at a given position</summary>
    class procedure FindDefinitionByCoordinates(const FileId: string;
       const BC: TBufferCoord; out DefFileName: string; out DefBC: TBufferCoord);
    /// <summary>Find references of the symbol at a given position</summary>
    class function FindReferencesByCoordinates(FileId: string;
       const BC: TBufferCoord): TArray<TDocPosition>;
    /// <summary>Format selected code or the whole document</summary>
    class procedure FormatCode(FileId: string; SynEdit: TCustomSynEdit);
    /// <summary>Requests completion assynchrously</summary>
    function HandleCodeCompletion(const FileId: string;
       const BC: TBufferCoord; out InsertText, DisplayText: string): Boolean;
    /// <summary>Get the documentation of a completion item</summary>
    function ResolveCompletionItem(CCItem: string): string;
    /// <summary>Get the document symbols synchronously</summary>
    class function DocumentSymbols(const FileId: string): TLSPDocumentSymbols;
    /// <summary>Get a simple hint synchronously</summary>
    procedure SignatureHelp(const AFileId: string; Editor: TCustomSynEdit);
    /// <summary>Get a code hint for an editor identifier</summary>
    class function SimpleHintAtCoordinates(const FileId: string;
       const BC: TBufferCoord): string;
    /// <summary>Like SimpleHintAtCoordinates but includes definition info</summary>
    class function CodeHintAtCoordinates(const FileId: string;
       const BC: TBufferCoord; const Ident: string): string;
    /// <summary>Handles the workspace/applyEdit server request</summary>
    class procedure OnWorkspaceApplyEdit(Sender: TObject;
      const value: TLSPApplyWorkspaceEditParams;
      var errorCode: Integer; var errorMessage: string);
    /// <summary>Send the prepareRename and return the results</summary>
    function PrepareRename(const FileId: string; const BC: TBufferCoord):
        TLSPPrepareRenameResult;
    /// <summary>Send the prepareRename and return the results</summary>
    function Rename(const FileId, NewName: string; const BC: TBufferCoord):
        TLSPWorkspaceEdit;
  end;

{$ENDREGION 'TPyLspClient'}


{$REGION 'TJediClient'}

  TJediClient = class(TPyLspClient)
  protected
    function GetCommandLine: string; override;
    function LogFileName: string; override;
    procedure OnInitialize(Sender: TObject;
      var Params: TLSPInitializeParams); override;
  end;

{$ENDREGION 'TJediClient'}

TRuffLspClient = class(TPyLspClient)
  protected
    function GetCommandLine: string; override;
    function LogFileName: string; override;
    procedure OnInitialize(Sender: TObject;
      var Params: TLSPInitializeParams); override;
end;

// Notifications
  TLspServerInitializedMessage = class(System.Messaging.TMessage);
  TLspServerShutDownMessage = class(System.Messaging.TMessage);

{$REGION 'Utility functions'}
function TextWithLF(SynEdit: TCustomSynEdit): string;
function LspPosition(const BC: TBufferCoord): TLSPPosition;
function LspRange(const BlockBegin, BlockEnd: TBufferCoord): TLSPRange;
function BufferCoordFromLspPosition(const Pos: TLSPPosition): TBufferCoord;
procedure BufferCoordinatesFromLspRange(const Range: TLSPRange;
  out BB, BE: TBufferCoord);
function FileIdToURI(const FilePath: string): string;
function FileIdFromURI(const URI: string): string;
procedure ApplyTextEdit(SynEdit: TCustomSynedit;
  TextEdit: TLSPAnnotatedTextEdit);
procedure ApplyTextEdits(SynEdit: TCustomSynedit; TextEdits:
    TLSPAnnotatedTextEdits);
procedure ApplyWorkspaceEdit(EditParams: TLSPApplyWorkspaceEditParams); overload;
procedure ApplyWorkspaceEdit(Edit: TLSPWorkspaceEdit); overload;
procedure SetupRefactoringMenu(FileId: string; const BB, BE: TBufferCoord;
    MenuItem: TTBCustomItem);
procedure CleanUpRefactoringMenu(MenuItem: TTBCustomItem);


{$ENDREGION 'Utility functions'}

implementation
uses
  System.Classes,
  System.Character,
  System.Variants,
  System.IOUtils,
  System.NetEncoding,
  System.RegularExpressions,
  System.Generics.Defaults,
  Vcl.StdCtrls,
  spTBXItem,
  XLspUtils,
  XLSPFunctions,
  SynEditTextBuffer,
  JvGnugettext,
  StringResources,
  dmResources,
  dmCommands,
  dlgPickList,
  uEditAppIntfs,
  uPythonItfs,
  uCommonFunctions,
  cPySupportTypes,
  cCodeCompletion,
  cPyScripterSettings,
  cSSHSupport;

{$REGION 'Utility functions'}

function TextWithLF(SynEdit: TCustomSynEdit): string;
begin
  Result := TSynEditStringList(SynEdit.Lines).GetSeparatedText(WideLF);
end;

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
    begin
      // change drive to lower case required by Jedi Lsp
      var Path := FilePath;
      if TPath.IsDriveRooted(Path) then
        Path[1] := Path[1].ToLower;
      Result := FilePathToURI(Path);
    end;
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

procedure ApplyTextEdit(SynEdit: TCustomSynedit;
  TextEdit: TLSPAnnotatedTextEdit);
var
  BB, BE: TBufferCoord;
begin
  BB := BufferCoordFromLspPosition(TextEdit.range.start);
  BE := BufferCoordFromLspPosition(TextEdit.range.&end);
  SynEdit.SetSelectionText(TextEdit.newText, BB, BE)
end;

procedure ApplyTextEdits(SynEdit: TCustomSynedit;
  TextEdits: TLSPAnnotatedTextEdits);
begin
  SynEdit.Lines.BeginUpdate;
  SynEdit.BeginUndoBlock;
  try
    // TextEdit positions refer to the original document
    // We need to apply edits in reverse order
    // to avoid having to adjust the ranges
    for var I := High(TextEdits) downto Low(TextEdits) do
    begin
      var TextEdit := TextEdits[I];
      ApplyTextEdit(SynEdit, TextEdit);
    end;

  finally
    SynEdit.EndUndoBlock;
    SynEdit.Lines.EndUpdate;
  end;

end;

procedure ApplyWorkspaceEdit(EditParams: TLSPApplyWorkspaceEditParams);
begin
  ApplyWorkspaceEdit(EditParams.edit);
end;

procedure ApplyWorkspaceEdit(Edit: TLSPWorkspaceEdit); overload;
// Todo: If more than one file is affected, show the affected files
// and let the user decide to which files to apply the edit

  function GetAffectedFiles: TArray<string>;
  begin
    Result := [];
    if Length(Edit.documentChanges) > 0 then
      for var Item in Edit.documentChanges do
      begin
        if not (Item is TLSPTextDocumentEdit) then Continue;
        var DocEdit := TLSPTextDocumentEdit(Item);
        Result := Result + [FileIdFromURI(DocEdit.textDocument.uri)];
      end
    else if Assigned(Edit.changes) and (Edit.changes.Count > 0) then
    begin
      for var Key in Edit.changes.Keys do
        Result := Result + [FileIdFromURI(Key)];
    end;
  end;

  function GetConfirmedFiles(const AffectedFiles: TArray<string>): TArray<string>;
  begin
    var dlg := TPickListDialog.Create(nil);
    dlg.Caption := _('Confirm Rename');
    dlg.lbMessage.Caption := _(SRenameConfirmation);
    dlg.CheckListBox.Items.AddStrings(AffectedFiles);
    dlg.CheckListBox.CheckAll(cbChecked);
    dlg.SetScrollWidth;

    Result := [];
    if dlg.ShowModal = idOK then
      for var I := 0 to dlg.CheckListBox.Items.Count - 1 do
        if dlg.CheckListBox.Checked[I] then
          Result := Result + [dlg.CheckListBox.Items[I]];
    dlg.Free;
  end;

var
  ConfirmedFiles: TArray<string>;
begin
  if not Assigned(Edit) then Exit;

  var AffectedFiles := GetAffectedFiles;
  if Length(AffectedFiles) = 0 then Exit;

  if Length(AffectedFiles) > 1 then
    ConfirmedFiles := GetConfirmedFiles(AffectedFiles)
  else
    ConfirmedFiles := AffectedFiles;

  if Length(ConfirmedFiles) = 0 then Exit;

  if Length(Edit.documentChanges) > 0 then
    for var Item in Edit.documentChanges do
    begin
      // Currently we do not handle workspace commands
      if not (Item is TLSPTextDocumentEdit) then Continue;

      var DocEdit := TLSPTextDocumentEdit(Item);
      var FileId := FileIdFromURI(DocEdit.textDocument.uri);
      if not TArray.Contains<string>(ConfirmedFiles, FileId) then Continue;

      var Editor := GI_EditorFactory.GetEditorByFileId(FileId);
      var Opened := False;
      if not Assigned(Editor) then
        try
          Editor := GI_EditorFactory.OpenFile(FileId);
          Opened := True;
        except
          Continue;
        end;
      if not Assigned(Editor) then Continue;
      Editor.Activate;

      if not Opened and not VarIsNull(DocEdit.textDocument.version) and
        (Editor.Version <> DocEdit.textDocument.version)
      then
        Exit;
      ApplyTextEdits(Editor.SynEdit, DocEdit.edits);
      Editor.PullDiagnostics;
    end
  else if Assigned(Edit.changes) and (Edit.changes.Count > 0) then
  begin
    for var Key in Edit.changes.Keys do
    begin
      var FileId := FileIdFromURI(Key);
      if not TArray.Contains<string>(ConfirmedFiles, FileId) then Continue;
      if not GI_PyIDEServices.ShowFilePosition(FileId) then Continue;
      var Editor := GI_EditorFactory.GetEditorByFileId(FileId);
      if not Assigned(Editor) then
        try
          Editor := GI_EditorFactory.OpenFile(FileId);
        except
          Continue;
        end;
      if not Assigned(Editor) then Continue;
      Editor.Activate;

      if not Assigned(GI_PyIDEServices.ActiveEditor) then Continue;
      ApplyTextEdits(Editor.SynEdit, Edit.changes[Key]);
      Editor.PullDiagnostics;
    end;
  end;
end;

procedure SetupRefactoringMenu(FileId: string; const BB, BE: TBufferCoord;
  MenuItem: TTBCustomItem);
begin
  TPyLspClient.MainLspClient.GetCodeActions(FileId, BB, BE);
  if not Assigned(TPyLspClient.CodeActions) or
    (Length(TPyLspClient.CodeActions.codeActions) = 0)
  then
    Exit;
  for var I := 0 to High(TPyLspClient.CodeActions.codeActions) do
  begin
    var CodeAction := TPyLspClient.CodeActions.codeActions[I];
    if not Assigned(CodeAction.edit) then Continue;
    var Item := TSpTBXItem.Create(nil);
    Item.Action := CommandsDataModule.actCodeAction;
    Item.Hint :=  CodeAction.title;
    if Item.Hint.StartsWith('Inline') then
      Item.Caption := _('Inline')
    else if Item.Hint.StartsWith('Extract expression into function') then
      Item.Caption := _('Extract function')
    else
      Item.Caption := _('Extract variable');
    Item.Tag := I;
    MenuItem.Add(Item);
  end;
end;

procedure CleanUpRefactoringMenu(MenuItem: TTBCustomItem);
begin
  while MenuItem[MenuItem.Count - 1].Action = CommandsDataModule.actCodeAction do
    MenuItem[MenuItem.Count - 1].Free;
end;

{$ENDREGION 'Utility functions'}


{$REGION 'TPyLspClient'}

procedure TPyLspClient.ClientCapabilities(Capabilities: TLSPClientCapabilities);
begin
  Capabilities.AddWorkspaceCapabilities(True, True, False);
  Capabilities.AddWorkspaceEdit(True, True, 'abort', True, True);
  Capabilities.AddWorkspaceExecuteCommand(False);
  Capabilities.AddSynchronizationSupport(True, False, False, False);
  Capabilities.AddDocumentSymbolSupport(True, False, False);
  Capabilities.AddCompletionSupport(False, False, False, True, False,
    False, False, False, False);
  Capabilities.AddDefinitionSupport(False);
  Capabilities.AddReferencesSupport(False);
  Capabilities.AddHoverSupport(False, True, True);
  Capabilities.AddDiagnosticSupport(False, False);
  //Capabilities.AddPublishDiagnosticsSupport(False, True, True, False);
  Capabilities.AddFormattingSupport(False);
  Capabilities.AddRangeFormattingSupport(False);
  Capabilities.AddCodeActionSupport(False, True, False, True, True);
  Capabilities.AddRenameSupport(False, True, True);
end;

procedure TPyLspClient.GetCodeActions(const FileId: string; BB,
  BE: TBufferCoord);
begin
  FreeAndNil(CodeActions);
  if not Ready or not FLspClient.IsRequestSupported(lspCodeAction) then Exit;

  var Params := TSmartPtr.Make(TLSPCodeActionParams.Create)();
  Params.textDocument.uri := FileIdToURI(FileId);
  Params.range := LspRange(BB, BE);
  Params.context.triggerKind := 1;

  FLspClient.SendSyncRequest(lspCodeAction, Params,
    procedure(AJson: TJSONObject)
    begin
      if ResponseError(AJson) then Exit;
      var JsonResult := AJson.Values['result'];
      if JsonResult.Null then Exit;
      CodeActions := JsonCodeActionResultToObject(JsonResult);
    end,
    SyncRequestTimeout);
end;

class function TPyLspClient.CodeHintAtCoordinates(const FileId: string; const
    BC: TBufferCoord; const Ident: string): string;
var
  DefFileName: string;
  DefBC: TBufferCoord;
  ModuleName: string;
  DefinedIn: string;
begin
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

class constructor TPyLspClient.Create;
begin
  SyncRequestTimeout := 4000; // ms
  LspClients := TObjectList<TpyLspClient>.Create(True);
  MainLspClient := TJediClient.Create;
  LspClients.Add(MainLspClient);
  DiagnosticsLspClient := TRuffLspClient.Create;
  LspClients.Add(DiagnosticsLspClient);

  // Notifications
  TMessageManager.DefaultManager.SubscribeToMessage(TPythonVersionChangeMessage,
    PythonVersionChanged);
  TMessageManager.DefaultManager.SubscribeToMessage(TProjectPythonPathChangeMessage,
    ProjectPythonPathChanged);
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
  FLspClient.OnShowMessage := OnShowMessage;
  FLspClient.OnWorkspaceApplyEdit := OnWorkspaceApplyEdit;

  if PyIDEOptions.LspDebug then
  begin
    FLSPClient.LogItems :=
      [liServerRPCMessages, liClientRPCMessages, liServerMessages];
    FLspClient.LogFileName :=
      TPath.Combine(TPyScripterSettings.UserDataPath, 'Lsp', LogFileName);
  end;
end;

destructor TPyLspClient.Destroy;
begin
  TMessageManager.DefaultManager.SendMessage(Self,
    TLspServerShutDownMessage.Create);
  FLspClient.Free;
  inherited;
end;

class destructor TPyLspClient.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TPythonVersionChangeMessage,
    PythonVersionChanged);
  TMessageManager.DefaultManager.Unsubscribe(TProjectPythonPathChangeMessage,
    ProjectPythonPathChanged);

  FreeAndNil(LspClients);
  FreeAndNil(CodeActions);
end;

class function TPyLspClient.DocumentSymbols(const FileId: string):
    TLSPDocumentSymbols;
var
  Symbols: TLSPDocumentSymbols;
begin
  var Client := MainLspClient;
  if not Assigned(Client) or not (Client.Ready and
    Client.LspClient.IsRequestSupported(lspDocumentSymbol))
  then
    Exit;

  Symbols := [];

  var Params := TSmartPtr.Make(TLSPDocumentSymbolParams.Create)();
  Params.textDocument.uri := FileIdToURI(FileId);

  Client.LspClient.SendSyncRequest(lspDocumentSymbol, Params,
    procedure(AJson: TJsonObject)
    begin
      if ResponseError(AJson) then Exit;
      var JsonResult := AJson.Values['result'];
      if JsonResult.Null then Exit;
      var Results :=
        TSmartPtr.Make(JsonDocumentSymbolsResultToObject(JsonResult))();
      Symbols := Results.symbols;
    end,
    SyncRequestTimeout);
  Result := Symbols;
end;

procedure TPyLspClient.ExecuteCommand(const Command, FileId: string; Version:
    Integer);
var
  Doc: TLSPVersionedTextDocumentIdentifier;
begin
  if not (Ready and
    LspClient.IsRequestSupported(lspWorkspaceExecuteCommand) and
    (FileId <> ''))
  then
    Exit;

  Doc.uri := FileIdToURI(FileId);
  Doc.version := Version;
  var JsonDoc := TSerializer.Serialize(Doc);

  LspClient.SendExecuteCommandRequest(Command, Format('[%s]', [JsonDoc]));
end;

class procedure TPyLspClient.FindDefinitionByCoordinates(const FileId: string;
    const BC: TBufferCoord; out DefFileName: string; out DefBC: TBufferCoord);
var
  FName: string;
  ResBC: TBufferCoord;
begin
  var Client := MainLspClient;
  if not Assigned(Client) or not (Client.Ready and
    Client.LspClient.IsRequestSupported(lspGotoDefinition))
  then
    Exit;

  ResBC := TBufferCoord.Invalid;
  var Params := TSmartPtr.Make(TLSPDefinitionParams.Create)();
  Params.textDocument.uri := FileIdToURI(FileId);
  Params.position := LspPosition(BC);

  Client.LspClient.SendSyncRequest(lspGotoDefinition, Params,
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

class function TPyLspClient.FindReferencesByCoordinates(FileId: string; const
    BC: TBufferCoord): TArray<TDocPosition>;
var
  Locations: TArray<TLSPLocation>;
begin
  var Client := MainLspClient;
  if not Assigned(Client) or not (Client.Ready and
    Client.LspClient.IsRequestSupported(lspReferences))
  then
    Exit;

  var Params := TSmartPtr.Make(TLSPReferencesParams.Create)();
  Params.textDocument.uri := FileIdToURI(FileId);
  Params.position := LspPosition(BC);
  Params.context.includeDeclaration := True;

  Locations := [];
  Client.LspClient.SendSyncRequest(lspReferences, Params,
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

class procedure TPyLspClient.FormatCode(FileId: string;
  SynEdit: TCustomSynEdit);
var
  LspKind: TLSPKind;
  Params: TLSPDocumentFormattingParams;
  edits: TArray<TLSPAnnotatedTextEdit>;
begin
  var Client := DiagnosticsLspClient;
  if not (Assigned(Client) and Client.Ready and
    Client.FLspClient.IsRequestSupported(lspDocumentFormatting))
  then
    Exit;

  var BB := SynEdit.BlockBegin;
  var BE := SynEdit.BlockEnd;
  if (BB = BE) then
  begin
    // Format whole document
    if not Client.FLspClient.IsRequestSupported(lspDocumentFormatting) then
      Exit;
    LSPKind := lspDocumentFormatting;
    Params := TSmartPtr.Make(TLSPDocumentFormattingParams.Create)();
  end
  else
  begin
    // Format the document range
    if not Client.FLspClient.IsRequestSupported(lspDocumentRangeFormatting) then
      Exit;
    LSPKind := lspDocumentRangeFormatting;
    Params := TSmartPtr.Make(TLSPDocumentRangeFormattingParams.Create)();
    TLSPDocumentRangeFormattingParams(Params).range := LspRange(BB, BE);
  end;
  Params.textDocument.uri := FileIdToURI(FileId);
  Params.options.tabSize := SynEdit.TabWidth;
  Params.options.insertSpaces := True;
  Params.options.trimTrailingWhitespace := True;
  Params.options.insertFinalNewline := True;
  Params.options.trimFinalNewlines := True;

  Client.LspClient.SendSyncRequest(LspKind, Params,
    procedure(AJson: TJSONObject)
    begin
      if ResponseError(AJson) then Exit;
      var JsonResult := AJson.Values['result'];
      if JsonResult.Null then Exit;
      var Results :=
        TSmartPtr.Make(JsonDocumentFormattingResultToObject(JsonResult))();
      edits := Results.edits;
    end,
    SyncRequestTimeout);
    ApplyTextEdits(SynEdit, edits);
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

procedure TPyLspClient.OnServerConnected(Sender: TObject);
begin
 FLspClient.SendRequest(lspInitialize);
end;

procedure TPyLspClient.OnShowMessage(Sender: TObject;
  const ntype: TLSPMessageType; const msg: string);
begin
  GI_PyIDEServices.WriteStatusMsg(Format('LSP %s: %s', [ntype.ToString, msg]));
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

class procedure TPyLspClient.OnWorkspaceApplyEdit(Sender: TObject;
  const value: TLSPApplyWorkspaceEditParams; var errorCode: Integer;
  var errorMessage: string);
begin
  ApplyWorkspaceEdit(value);
  errorCode := 0;
  errorMessage := '';
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
  TMessageManager.DefaultManager.SendMessage(Self,
    TLspServerInitializedMessage.Create);
end;

procedure TPyLspClient.SignatureHelpHandler(AJson: TJsonObject);
begin
  var SignatureInfo := TIDECompletion.SignatureHelpInfo;
  SignatureInfo.Lock;
  try
    var Id := AJson.GetValue<Integer>('id', 0);
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

  if Ready then
     FLspClient.SaveToLogFile('Response Error: ' + SLineBreak + errorMsg);
end;

function TPyLspClient.PrepareRename(const FileId: string;
  const BC: TBufferCoord): TLSPPrepareRenameResult;
var
  Res: TLSPPrepareRenameResult;
begin
  Res := nil;
  if not (Ready and
    LspClient.IsRequestSupported(lspPrepareRename) and
    (FileId <> ''))
  then
    Exit(nil);

  var Params := TSmartPtr.Make(TLSPPrepareRenameParams.Create)();
  Params.textDocument.uri := FileIdToURI(FileId);
  Params.position := LspPosition(BC);

  FLspClient.SendSyncRequest(lspPrepareRename, Params,
    procedure(AJson: TJSONObject)
    begin
      if ResponseError(AJson) then Exit;
      var JsonResult := AJson.Values['result'];
      Res := JsonPrepareRenameResultToObject(JsonResult);
    end,
    SyncRequestTimeout);

  Result := Res;
end;

function TPyLspClient.Rename(const FileId, NewName: string;
  const BC: TBufferCoord): TLSPWorkspaceEdit;
var
  Res: TLSPWorkspaceEdit;
begin
  Res := nil;
  if not (Ready and
    LspClient.IsRequestSupported(lspRename) and
    (FileId <> ''))
  then
    Exit(nil);

  var Params := TSmartPtr.Make(TLSPRenameParams.Create)();
  Params.textDocument.uri := FileIdToURI(FileId);
  Params.position := LspPosition(BC);
  Params.newName := NewName;

  FLspClient.SendSyncRequest(lspRename, Params,
    procedure(AJson: TJSONObject)
    begin
      if ResponseError(AJson) then Exit;
      var JsonResult := AJson.Values['result'];
      if JsonResult.Null then Exit;
      Res := TSerializer.Deserialize<TLSPWorkspaceEdit>(JsonResult);
    end,
    SyncRequestTimeout);

  Result := Res;
end;

class procedure TPyLspClient.ProjectPythonPathChanged(const Sender:
    TObject; const Msg: System.Messaging.TMessage);
begin
  var NewPath := TProjectPythonPathChangeMessage(Msg).Value;

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

  FProjectPythonPath := NewPath;

  if (Length(Params.event.removed) > 0) or (Length(Params.event.added) > 0) then begin
    for var Client in LspClients do
      if Client.Ready and
        Client.FLspClient.IsRequestSupported(lspDidChangeWorkspaceFolders)
      then
        Client.FLspClient.SendNotification(lspDidChangeWorkspaceFolders, Params);
    RestartServers;
  end;
end;

class procedure TPyLspClient.PythonVersionChanged(const Sender: TObject; const
    Msg: System.Messaging.TMessage);
begin
  RestartServers;
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

class procedure TPyLspClient.RestartServers;
begin
  for var Client in LSPClients do
    try
      Client.CreateAndRunServer;
    except
      // Absorb exceptions
    end;
end;

class function TPyLspClient.SimpleHintAtCoordinates(const FileId: string; const
    BC: TBufferCoord): string;
var
  Str: string;
begin
  var Client := MainLspClient;
  if not Assigned(Client) or not (Client.Ready and
    Client.LspClient.IsRequestSupported(lspHover))
  then
    Exit;

  var Params := TSmartPtr.Make(TLSPHoverParams.Create)();
  Params.textDocument.uri := FileIdToURI(FileId);
  Params.position := LspPosition(BC);

  Client.LspClient.SendSyncRequest(lspHover, Params,
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
begin
  if not GI_PyControl.PythonLoaded then Exit;

  var ServerPath :=
    TPath.Combine(TPyScripterSettings.LspServerPath,
    'jls\run-jedi-language-server.py');
  if not FileExists(ServerPath) then Abort;

  Result := Format('"%s" -u "%s"',
    [GI_PyControl.PythonVersion.PythonExecutable, ServerPath]);
end;

function TJediClient.LogFileName: string;
begin
  Result := 'JediLsp.log';
end;

procedure TJediClient.OnInitialize(Sender: TObject;
  var Params: TLSPInitializeParams);
const
   InitializationOptionsLsp =
    '{'#13#10 +
    '    "diagnostics": {'#13#10 +
    '      "enable": false,'#13#10 +
    '      "didOpen": false,'#13#10 +
    '      "didChange": false,'#13#10 +
    '      "didSave": false'#13#10 +
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
    '   },'#13#10 +
    '   "codeAction": {'#13#10 +
    '      "nameExtractVariable": "extract_var",'#13#10 +
    '      "nameExtractFunction": "extract_def"'#13#10 +
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
    Result := Result.Replace('\', '/', [rfReplaceAll]);
  end;

begin
  ClientCapabilities(Params.capabilities);
  if Length(FProjectPythonPath) > 0 then
  begin
    Params.AddWorkspaceFolders(FProjectPythonPath);
    Params.AddRoot(FProjectPythonPath[0]);
  end;
  if PyIDEOptions.LspDebug then
    Params.trace := 'verbose';
  Params.initializationOptions :=
    Format(InitializationOptionsLsp,
    [QuotePackages(PyIDEOptions.SpecialPackages),
     BoolToStr(not PyIDEOptions.CodeCompletionCaseSensitive, True).ToLower,
     ExtraPaths]);
end;

{$ENDREGION 'TJediClient'}


{$REGION 'TRuffLspClient'}

function TRuffLspClient.GetCommandLine: string;
begin
  var ServerPath :=
    TPath.Combine(TPyScripterSettings.LspServerPath, 'Ruff', 'ruff.exe');
  if not FileExists(ServerPath) then Abort;

  var PyVersion := GI_PyControl.PythonVersion.SysVersion;
  PyVersion := PyVersion.Replace('.', '');

  var CacheDir := TPath.Combine(TPyScripterSettings.UserDataPath, 'Lsp', 'cache');
  CacheDir := CacheDir.Replace('\', '/', [rfReplaceAll]);

  Result := Format(
    '"%s" server' +
    ' --config "target-version=\"py%s\""' +
    ' --config "cache-dir=\"%s\""',
    [ServerPath, PyVersion, CacheDir]);
end;

function TRuffLspClient.LogFileName: string;
begin
  Result := 'RuffLsp.log';
end;

procedure TRuffLspClient.OnInitialize(Sender: TObject;
  var Params: TLSPInitializeParams);
const
  InitOptions =
'{'#13#10 +
'    "settings": {'#13#10 +
'             "configuration": "%s"'#13#10 +
'    }'#13#10 +
'}';
begin
  ClientCapabilities(Params.capabilities);
  if Length(FProjectPythonPath) > 0 then
    Params.AddWorkspaceFolders(FProjectPythonPath);
  if PyIDEOptions.LspDebug then
    Params.trace := 'verbose';

  var ConfigFile := TPath.Combine(TPyScripterSettings.UserDataPath,
                                  'Lsp', 'Ruff', 'ruff.toml');
  ConfigFile := ConfigFile.Replace('\', '/', [rfReplaceAll]);

  Params.initializationOptions := Format(InitOptions, [ConfigFile]);
end;

{$ENDREGION 'TRuffLspClient'}

end.
