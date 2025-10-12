{
   SynEdit plugin that automates the notifications between
   the LSP Client and the Server.
   (C) PyScripter 2020
}

unit SynEditLsp;

interface

uses
  Winapi.Windows,
  System.Classes,
  System.Generics.Collections,
  System.Messaging,
  SynEdit,
  XLSPTypes,
  cLSPClients;

type
  TLangId = (lidNone, lidPython);

  TLspSynEditPlugin = class;

  TDiagnosticUpdateType = (datInvoked, datCleared, datUpdated);
  TDiagnosticUpdateTypes = set of TDiagnosticUpdateType;
  TOnDiagnosticsUpdate = procedure (UpdateTypes: TDiagnosticUpdateTypes) of object;

  TDocSymbols = class
    {Asynchronous symbol support for Code Explorer}
  private
    FRequestId: Integer;
    FPlugIn: TLspSynEditPlugin;
    FOnNotify: TNotifyEvent;
    function GetFileId: string;
  public
    ModuleNode: TObject;  // for storing Code Explorer TModuleNodeCE
    Symbols: TList<TLSPDocumentSymbol>;
    constructor Create(PlugIn: TLspSynEditPlugin);
    destructor Destroy; override;
    procedure Clear;
    procedure Lock;
    procedure Unlock;
    property FileId: string read GetFileId;
    property OnNotify: TNotifyEvent read FOnNotify write FOnNotify;
  end;

  TLSPDiagnostics = TArray<TLSPDiagnostic>;

  TLspSynEditPlugin = class(TSynEditPlugin)
  public
    const DiagnosticsErrorIndicatorId: TGUID = '{48005990-9661-4DA7-A1B1-84A20045F37B}';
    const DiagnosticsWarningIndicatorId: TGUID = '{2D6B2929-BA1A-495A-8942-EF6E2B648857}';
    const DiagnosticsHintIndicatorId: TGUID = '{8BFA8988-6B28-47F9-97AD-8360D0A4CFEE}';
  private
    FFileId: string;
    FLangId: TLangId;
    FIncChanges: TList<TLSPBaseTextDocumentContentChangeEvent>;
    FVersion: NativeUInt;
    FTransmitChanges: Boolean;
    FDiagnostics:  TLSPDiagnostics;
    FNewDiagnostics: TThreadList<TLSPDiagnostic>;
    FNeedToRefreshDiagnostics: Boolean;
    FNeedToRefreshSymbols: Boolean;
    FDocSymbols: TDocSymbols;
    FLastDeletedLiine: string;
    FDiagnosticsRequestId: Integer;
    FOnDiagnosticsUpdate: TOnDiagnosticsUpdate;
    procedure ClearDiagnosticsInternal;
    class var FDocSymbolsLock: TRTLCriticalSection;
    class var Instances: TThreadList<TLspSynEditPlugin>;
  protected
    procedure LinesInserted(FirstLine, Count: Integer); override;
    procedure LinesBeforeDeleted(FirstLine, Count: Integer); override;
    procedure LinesDeleted(FirstLine, Count: Integer); override;
    procedure LinePut(AIndex: Integer; const OldLine: string); override;
    procedure LinesChanged; override;
  public
    constructor Create(AOwner: TCustomSynEdit);
    destructor Destroy; override;
    procedure FileOpened(const FileId: string; LangId: TLangId; AClient:
        TPyLspClient = nil);
    procedure FileClosed;
    procedure FileSaved;
    procedure FileSavedAs(const FileId: string; LangId: TLangId);
    // Document Diagnostics
    procedure ApplyNewDiagnostics(Invoked: Boolean = False);
    procedure ClearDiagnostics;
    procedure PullDiagnostics(Invoked: Boolean = False);
    // Fixes
    procedure PerformQuickFix;
    procedure PerformNoqaEdit;
    // Document Symbols
    procedure RefreshSymbols;
    // properties
    property TransmitChanges: Boolean read FTransmitChanges
      write FTransmitChanges;
    property Diagnostics: TLSPDiagnostics read FDiagnostics; // Sorted by severity
    property DocSymbols: TDocSymbols read FDocSymbols;
    property OnDiagnosticsUpdate: TOnDiagnosticsUpdate
      read FOnDiagnosticsUpdate write FOnDiagnosticsUpdate;
    // class vars and procedures
    class var DiagnosticHintIndex: Integer;
    class var DiagnosticsIndicatorIds: TArray<TGUID>;
    class constructor Create;
    class destructor Destroy;
    class procedure OnLspInitialized(const Sender: TObject; const Msg:
        System.Messaging.TMessage);
  end;

  TLSPDiagnosticHelper = record helper for TLSPDiagnostic
    function SeverityText: string;
    function Hint: string;
    function SimpleMessage: string;
  end;

  TLSPDiagnosticsHelper = record helper for TLSPDiagnostics
    function HasErrors: Boolean;
    function HasSyntaxError: Boolean;
    function HasWarnings: Boolean;
    function ToBCArray(MaxSeverity: Integer = 4): TArray<TBufferCoord>;
    procedure ShowInMessages(const FileId: string);
    function Summary: string;
  end;

implementation

uses
  System.UITypes,
  System.SysUtils,
  System.JSON,
  System.SyncObjs,
  System.Math,
  SynEditTypes,
  System.Generics.Defaults,
  SynEditMiscProcs,
  SynEditMiscClasses,
  SynDWrite,
  XLSPFunctions,
  XLSPUtils,
  JvGnugettext,
  cPyScripterSettings,
  uEditAppIntfs,
  uCommonFunctions;


{ TLspSynEditPlugin }

constructor TLspSynEditPlugin.Create(AOwner: TCustomSynEdit);
begin
  inherited Create(AOwner);
  FHandlers := [phLinesInserted, phLinesBeforeDeleted, phLinesDeleted,
                phLinePut, phLinesChanged];
  FIncChanges := TList<TLSPBaseTextDocumentContentChangeEvent>.Create;
  FNewDiagnostics := TThreadList<TLSPDiagnostic>.Create;
  Instances.Add(Self);
  FDocSymbols := TDocSymbols.Create(Self);
  AOwner.Indicators.RegisterSpec(DiagnosticsErrorIndicatorId,
    TSynIndicatorSpec.New(sisSquiggleMicrosoftWord,
    D2D1ColorF(TColors.Crimson), clNoneF, []));
  AOwner.Indicators.RegisterSpec(DiagnosticsWarningIndicatorId,
    TSynIndicatorSpec.New(sisSquiggleMicrosoftWord,
    D2D1ColorF(TColors.Darkorange), clNoneF, []));
  AOwner.Indicators.RegisterSpec(DiagnosticsHintIndicatorId,
    TSynIndicatorSpec.New(sisSquiggleMicrosoftWord,
    D2D1ColorF(TColors.Goldenrod), clNoneF, []));
end;

destructor TLspSynEditPlugin.Destroy;
begin
  Instances.Remove(Self);
  FNewDiagnostics.Free;
  for var Event in FIncChanges do
    Event.Free;
  FIncChanges.Free;
  FDocSymbols.Free;
  inherited;
end;

class destructor TLspSynEditPlugin.Destroy;
begin
  Instances.Free;
  FDocSymbolsLock.Free;
  TMessageManager.DefaultManager.Unsubscribe(TLspServerInitializedMessage,
    OnLspInitialized);
end;

procedure TLspSynEditPlugin.FileClosed;
begin
  FDocSymbols.Clear;
  var OldLangId := FLangId;
  FLangId := lidNone;
  var OldFileId := FFileId;
  FFileId := '';
  if (OldLangId <> lidPython) or (OldFileId = '') then Exit;

  var Params := TSmartPtr.Make(TLSPDidCloseTextDocumentParams.Create)();
  Params.textDocument.uri := FileIdToURI(OldFileId);
  var Json := Params.AsJson;  //TODO: investigate exception here

  for var Client in TPyLspClient.LspClients do
    if Client.Ready and (OldFileId <> '') and
      Client.LspClient.ServerCapabilities.textDocumentSync.openClose
    then
      Client.LspClient.SendNotification(lspDidCloseTextDocument, '', nil, Json);
end;

procedure TLspSynEditPlugin.FileOpened(const FileId: string; LangId: TLangId;
  AClient: TPyLspClient = nil);
{Support for didOpen is mandatory for the client
This method is called when the editor opens a file with AClient = nil
  meaning that the notification should be sent to all clients.
  It is also called when a server is initialized.  In this case AClient <> nil}
begin
  Assert(FileId <> '', 'TLspSynEditPlugin.FileOpened');
  FFileId := FileId;
  FLangId := LangId;

  FVersion := 0;
  FDocSymbols.Clear;

  if LangId <> lidPython then Exit;

  FTransmitChanges := True;

  var Params := TSmartPtr.Make(TLSPDidOpenTextDocumentParams.Create)();
  Params.textDocument.uri := FileIdToURI(FileId);
  Params.textDocument.languageId := 'python';
  Params.textDocument.text := Editor.Text;

  for var Client in TPyLspClient.LspClients do
    // if AClient <> nil then notify only this client
    if ((AClient = nil) or (AClient = Client)) and Client.Ready and
      Client.LspClient.ServerCapabilities.textDocumentSync.openClose
    then
      Client.LspClient.SendNotification(lspDidOpenTextDocument, Params);

  if (AClient = nil) or (AClient = TPyLspClient.DiagnosticsLspClient) then
  begin
    FNeedToRefreshDiagnostics := True;
    if PyIDEOptions.DiagnosticsOnOpen then
      PullDiagnostics;
  end;
  if (AClient = nil) or (AClient = TPyLspClient.MainLspClient) then
    RefreshSymbols;
end;

procedure TLspSynEditPlugin.FileSaved;
begin
  if (FFileId = '') or (FLangId <> lidPython) then Exit;

  var Params := TSmartPtr.Make(TLSPDidSaveTextDocumentParams.Create)();
  Params.textDocument.uri := FileIdToURI(FFileId);

  for var Client in TPyLspClient.LspClients do
    if Client.Ready and
      Client.LspClient.ServerCapabilities.textDocumentSync.save.value then
    begin
      if Client.LspClient.ServerCapabilities.textDocumentSync.save.includeText then
        Params.text := Editor.Text
      else
        Params.text := '';
      Client.LspClient.SendNotification(lspDidSaveTextDocument, Params);

      if PyIDEOptions.DiagnosticsOnSave then
        PullDiagnostics;
    end;
end;

procedure TLspSynEditPlugin.FileSavedAs(const FileId: string; LangId: TLangId);
begin
  FileClosed;
  FileOpened(FileId, LangId);
end;

procedure TLspSynEditPlugin.PerformNoqaEdit;
begin
  if not InRange(DiagnosticHintIndex, 0, Length(FDiagnostics) - 1) then Exit;

  var Diagnostic := FDiagnostics[DiagnosticHintIndex];
  if Diagnostic.data = '' then Exit;

  var JsonValue := TSmartPtr.Make(TJsonValue.ParseJSONValue(Diagnostic.data))();
  if not (JsonValue is TJSONObject) then Exit;

  var DataObj := TJSONObject(JsonValue);
  var NoqaEdit := DataObj.Values['noqa_edit'];

  if not (NoqaEdit is TJSONObject) then Exit;
  var Edit := TSerializer.Deserialize<TLSPAnnotatedTextEdit>(NoqaEdit);
  ApplyTextEdit(Editor, Edit);
  PullDiagnostics(False);
end;

procedure TLspSynEditPlugin.PerformQuickFix;
begin
  if not InRange(DiagnosticHintIndex, 0, Length(FDiagnostics) - 1) then Exit;

  var Diagnostic := FDiagnostics[DiagnosticHintIndex];
  if Diagnostic.data = '' then Exit;

  var JsonValue := TSmartPtr.Make(TJsonValue.ParseJSONValue(Diagnostic.data))();
  if not (JsonValue is TJSONObject) then Exit;

  var DataObj := TJSONObject(JsonValue);
  var Fix := DataObj.Values['edits'];

  if not (Fix is TJSONArray) or (TJSONArray(Fix).Count = 0) then Exit;
  var Edits := TSerializer.Deserialize<TLSPAnnotatedTextEdits>(Fix);
  ApplyTextEdits(Editor, Edits);
  PullDiagnostics(False);
end;

procedure TLspSynEditPlugin.PullDiagnostics(Invoked: Boolean = False);
begin
  if not FNeedToRefreshDiagnostics then
  begin
    if Invoked then
      if Assigned(FOnDiagnosticsUpdate) then
        FOnDiagnosticsUpdate([datInvoked]);
    Exit;
  end;

  var Client := TPyLspClient.DiagnosticsLspClient;
  if not (Assigned(Client) and Client.Ready and TransmitChanges and
    Client.LspClient.IsRequestSupported(lspDocumentDiagnostic))
  then
    Exit;

  var Params := TSmartPtr.Make(TLSPDocumentDiagnosticParams.Create)();
  Params.textDocument.uri := FileIdToURI(FFileId);
  FDiagnosticsRequestId := Client.LspClient.SendRequest(lspDocumentDiagnostic, Params,
    procedure(AJson: TJSONObject)
    begin
      if ResponseError(AJson) then Exit;
      var Id := AJson.GetValue<Integer>('id', -1);
      if Id <> FDiagnosticsRequestId then Exit;

      var JsonResult := AJson.Values['result'];
      if JsonResult.Null then Exit;
      var Results :=
        TSmartPtr.Make(JsonDocumentDiagnosticReportToObject(JsonResult))();

      var List := FNewDiagnostics.LockList;
      try
        List.Clear;
        List.AddRange(Results.items);
        // Sort the list
        List.Sort(TComparer<TLSPDiagnostic>.Construct(
          function(const Left, Right: TLSPDiagnostic): Integer
          begin
            Result := CompareValue(Left.severity, Right.severity);
            if Result = 0 then
              Result := CompareValue(Left.range.start.line, Right.range.start.line);
            if Result = 0 then
              Result := CompareValue(Left.range.start.character, Right.range.start.character);
          end));
      finally
        FNewDiagnostics.UnlockList;
      end;

      TThread.ForceQueue(TThread.CurrentThread,
        procedure
        begin
          ApplyNewDiagnostics(Invoked);
        end);
    end);

  FNeedToRefreshDiagnostics := False;
end;

procedure TLspSynEditPlugin.LinesBeforeDeleted(FirstLine, Count: Integer);
begin
  if FirstLine + Count = Editor.Lines.Count then
    FLastDeletedLiine := Editor.Lines[FirstLine + Count -1];
end;

procedure TLspSynEditPlugin.LinesChanged;
var
  Change: TLSPBaseTextDocumentContentChangeEvent;
  syncKind: Integer;
begin
  Inc(FVersion);
  FNeedToRefreshSymbols := True;
  // Document changes may invalidate Diagnostics so we clear them
  ClearDiagnostics;

  if (FFileId = '') or (FLangId <> lidPython) or
    not FTransmitChanges or (FIncChanges.Count = 0)
  then
    Exit;

  try
    for var Client in TPyLspClient.LspClients do
    begin
      if not Client.Ready then Continue;

      syncKind := Client.LspClient.GetSyncKind;
      if synckind = TLSPTextDocumentSyncKindRec.None then Continue;

      var Params := TSmartPtr.Make(TLSPDidChangeTextDocumentParams.Create)();
      Params.textDocument.uri := FileIdToURI(FFileId);
      Params.textDocument.version := fVersion;

      if (syncKind = TLSPTextDocumentSyncKindRec.Incremental) and
        InRange(FIncChanges.Count, 1, Editor.Lines.Count div 3)
      then
      begin
        Params.contentChanges := FIncChanges.ToArray;
        Client.LspClient.SendNotification(lspDidChangeTextDocument, Params);
        // This is so that we do not double destroy the content changes
        Params.contentChanges := [];
      end
      else
      begin
        // too many changes - faster to send all text
        // The TLSPBaseTextDocumentContentChangeEvent will be detroyed by
        // the Params (TLSPDidChangeTextDocumentParams) destructor
        Change := TLSPBaseTextDocumentContentChangeEvent.Create;
        Change.text := Editor.Text;
        Params.contentChanges := [Change];
        Client.LspClient.SendNotification(lspDidChangeTextDocument, Params);
      end;
    end;
  finally
    // if the FIncChanges were not used we need to destroy them here
    for var Event in FIncChanges do
      Event.Free;
    FIncChanges.Clear;
  end;
end;

procedure TLspSynEditPlugin.LinesDeleted(FirstLine, Count: Integer);
{
   FirstLine 0-based
   Called after deletion
   Lines.Count may be 0
}
var
  BB, BE: TBufferCoord;
begin
  if (FFileId = '') or (FLangId <> lidPython) or not FTransmitChanges then Exit;

  BB := BufferCoord(1, FirstLine + 1);
  if FirstLine = Editor.Lines.Count then
    // we have deleted the last line
    BE := BufferCoord(FLastDeletedLiine.Length + 1, FirstLine + Count)
  else
    BE := BufferCoord(1, FirstLine + Count + 1);
  var Change := TLSPTextDocumentContentChangeEvent.Create;
  Change.range := LspRange(BB, BE);
  Change.text := '';
  FIncChanges.Add(Change);
end;

procedure TLspSynEditPlugin.LinesInserted(FirstLine, Count: Integer);
{
   FirstLine 0-based
   Called after insertion therefore Lines.Count >= Count and
   0 <= FirstLine <= Lines.Count - Count
}
var
  BB, BE: TBufferCoord;
begin
  if (FFileId = '') or (FLangId <> lidPython) or not FTransmitChanges then Exit;

  if (FirstLine = Editor.Lines.Count - Count) and (FirstLine > 0) then
    // Special case: Added at the end and not starting from the first line
    BB := BufferCoord(Editor.Lines[FirstLine - 1].Length + 1, FirstLine)
  else
    BB := BufferCoord(1, FirstLine + 1);
  BE := BB;

  var TextAdded := '';
  for var I := FirstLine to FirstLine + Count - 1 do
    TextAdded := TextAdded + Editor.Lines[I]  + sLineBreak;
  if (FirstLine = Editor.Lines.Count - Count) and (FirstLine > 0) then
    // Lines added at the end
    TextAdded := sLineBreak + TextAdded;
  if (FirstLine = Editor.Lines.Count - Count) then
    // Lines added at the end
    Delete(TextAdded, TextAdded.Length - Length(sLineBreak) + 1, Length(sLineBreak));

  var Change := TLSPTextDocumentContentChangeEvent.Create;
  Change.range := LspRange(BB, BE);
  Change.text := TextAdded;
  FIncChanges.Add(Change);
end;

class procedure TLspSynEditPlugin.OnLspInitialized(const Sender: TObject;
  const Msg: System.Messaging.TMessage);
begin
  var List := Instances.LockList;
  try
    for var Plugin in List do
      Plugin.FileOpened(Plugin.FFileId, Plugin.FLangId, Sender as TPyLspClient);
  finally
    Instances.UnlockList;
  end;
end;

procedure TLspSynEditPlugin.LinePut(AIndex: Integer; const OldLine: string);
var
  NewLine: string;
  Start, OldLen, NewLen: Integer;
begin
  if (FFileId = '') or (FLangId <> lidPython) or not FTransmitChanges then Exit;

  NewLine := Editor.Lines[AIndex];
  LineDiff(NewLine, OldLine, Start, OldLen, NewLen);

  var Diff := Copy(NewLine, Start, NewLen);
  var BB := BufferCoord(Start, AIndex + 1);
  var BE := BufferCoord(Start + OldLen, AIndex + 1);

  if (Diff = '') and (BB = BE) then Exit;  // no change

  var Change := TLSPTextDocumentContentChangeEvent.Create;
  Change.range := LspRange(BB, BE);
  Change.text := Diff;
  FIncChanges.Add(Change);
end;

procedure TLspSynEditPlugin.RefreshSymbols;
begin
  if not FNeedToRefreshSymbols then Exit;

  var Client := TPyLspClient.MainLspClient;
  if not (Assigned(Client) and Client.Ready and
    Client.LspClient.IsRequestSupported(lspDocumentSymbol))
  then
    Exit;

  var Params := TSmartPtr.Make(TLSPDocumentSymbolParams.Create)();
  Params.textDocument.uri := FileIdToURI(FFileId);

  FDocSymbols.FRequestId := Client.LspClient.SendRequest(lspDocumentSymbol,
    Params,
    procedure(AJson: TJSONObject)
    begin
      FDocSymbolsLock.Enter;
      try
        FreeAndNil(FDocSymbols.Symbols);
        if ResponseError(AJson) then Exit;

        // Check if this is the latest docsymbol request
        var Id := AJson.GetValue<Integer>('id', -1);
        if Id <> FDocSymbols.FRequestId then Exit;

        FDocSymbols.Symbols := TList<TLSPDocumentSymbol>.Create;

        var JsonResult := AJson.Values['result'];
        if not JsonResult.Null then
        begin
          var Results :=
            TSmartPtr.Make(JsonDocumentSymbolsResultToObject(JsonResult))();
          FDocSymbols.Symbols.AddRange(Results.symbols);
        end;

        if Assigned(FDocSymbols.FOnNotify) then
          FDocSymbols.FOnNotify(FDocSymbols);
      finally
        FDocSymbolsLock.Leave;
      end;
    end);

  FNeedToRefreshSymbols := False;
end;

procedure TLspSynEditPlugin.ApplyNewDiagnostics(Invoked: Boolean = False);
var
  Diagnostic: TLSPDiagnostic;
  IndicatorId: TGUID;
begin
  { Should be called from the main thread }
  Assert(GetCurrentThreadId = MainThreadID, 'TLspSynEditPlugin.ApplyNewDiagnostics');

  var List := FNewDiagnostics.LockList;
  try
    ClearDiagnosticsInternal;
    FDiagnostics := List.ToArray;
    for var I := 0 to Length(FDiagnostics) - 1 do
    begin
      Diagnostic := FDiagnostics[I];
      IndicatorId := DiagnosticsHintIndicatorId;
      case Diagnostic.severity of
        0, 1:  IndicatorId := DiagnosticsErrorIndicatorId;
        2:     IndicatorId := DiagnosticsWarningIndicatorId;
      end;
      // TODO Deal with mult-line diagnostics
      Editor.Indicators.Add(Diagnostic.range.start.line + 1,
        TSynIndicator.New(IndicatorId,
        Diagnostic.range.start.character + 1,
        Diagnostic.range.&end.character + 1, I));
    end;
  finally
    FNewDiagnostics.UnlockList;
  end;
  Editor.UpdateScrollBars;

  if Assigned(FOnDiagnosticsUpdate) then
  begin
    if Invoked then
      FOnDiagnosticsUpdate([datInvoked, datUpdated])
    else
      FOnDiagnosticsUpdate([datUpdated]);
  end;
end;

procedure TLspSynEditPlugin.ClearDiagnostics;
begin
  FNeedToRefreshDiagnostics := True;
  if Length(FDiagnostics) > 0 then
  begin
    ClearDiagnosticsInternal;

    if Assigned(FOnDiagnosticsUpdate) then
      FOnDiagnosticsUpdate([datCleared]);
  end;
end;

procedure TLspSynEditPlugin.ClearDiagnosticsInternal;
begin
  DiagnosticHintIndex := -1;
  if Length(FDiagnostics) > 0 then
  begin
    Editor.Indicators.Clear(DiagnosticsErrorIndicatorId);
    Editor.Indicators.Clear(DiagnosticsWarningIndicatorId);
    Editor.Indicators.Clear(DiagnosticsHintIndicatorId);
    FDiagnostics := [];
  end;
end;

class constructor TLspSynEditPlugin.Create;
begin
  DiagnosticsIndicatorIds :=
    [DiagnosticsErrorIndicatorId,
    DiagnosticsWarningIndicatorId,
    DiagnosticsHintIndicatorId];

  Instances := TThreadList<TLspSynEditPlugin>.Create;
  FDocSymbolsLock.Initialize;

  TMessageManager.DefaultManager.SubscribeToMessage(TLspServerInitializedMessage,
    OnLspInitialized);

  DiagnosticHintIndex := -1;
end;

{ TDocSymbols }

procedure TDocSymbols.Clear;
begin
  Lock;
  try
    FreeAndNil(Symbols);
    if Assigned(FOnNotify) then
      FOnNotify(Self);
  finally
    Unlock;
  end;
end;

constructor TDocSymbols.Create(PlugIn: TLspSynEditPlugin);
begin
  inherited Create;
  FPlugIn := PlugIn;
end;

destructor TDocSymbols.Destroy;
begin
  Clear;
  inherited;
end;

function TDocSymbols.GetFileId: string;
begin
  Result := FPlugIn.FFileId;
end;

procedure TDocSymbols.Lock;
begin
  TLspSynEditPlugin.FDocSymbolsLock.Enter;
end;

procedure TDocSymbols.Unlock;
begin
  TLspSynEditPlugin.FDocSymbolsLock.Leave;
end;

{ TLSPDiagnosticHelper }

function TLSPDiagnosticHelper.SimpleMessage: string;
begin
  Result := Format('%s: %s', [SeverityText, &message]);
end;

function TLSPDiagnosticHelper.Hint: string;
begin
  Result := Format('<b>%s</b>: %s', [SeverityText, &message]);
  if (code <> '') and (codeDescription.href <> '') then
    Result := Result + Format(
      ' (<a href="%s"><font color="$FF8844"><u>%s</u></font></a>)',
      [codeDescription.href, code]);
  if Data <> '' then
  begin
    var JsonValue := TSmartPtr.Make(TJsonValue.ParseJSONValue(Data))();
    if not (JsonValue is TJSONObject) then Exit;
    var DataObj := TJSONObject(JsonValue);
    var HasFix := (DataObj.Values['edits'] is TJSONArray) and
      (TJSONArray(DataObj.Values['edits']).Count > 0);
    var HasIgnore := Assigned(DataObj.Values['noqa_edit']);
    var Title := DataObj.GetValue<string>('title', '');

    if HasFix or HasIgnore then
      Result := Result + '<br>';
    if HasFix then
      Result := Result + Format(
        '<a href="QuickFix"><font color="$FF8844"><u>%s: %s</u></font></a> ',
        [_('Quick Fix'), Title]);
    if HasIgnore then
      Result := Result + Format(
        '<a href="Ignore"><font color="$FF8844"><u>%s</u></font></a>',
        [_('Ignore')]);
  end;
end;

function TLSPDiagnosticHelper.SeverityText: string;
begin
  case severity of
    0: Result := _('Syntax Error');
    1: Result := _('Error');
    2: Result := _('Warning');
    3: Result := _('Information');
  else
    Result := _('Hint');
  end;
end;

{ TLSPDiagnosticsHelper }

function TLSPDiagnosticsHelper.HasErrors: Boolean;
begin
  // Diagnostics are sorted according to severity
  Result := (Length(Self) > 0) and (Self[0].severity <= 1);
end;

function TLSPDiagnosticsHelper.HasSyntaxError: Boolean;
begin
  Result := (Length(Self) > 0) and (Self[0].severity = 0);
end;

function TLSPDiagnosticsHelper.HasWarnings: Boolean;
begin
  Result := (Length(Self) > 0) and (Self[0].severity <= 2);
end;

procedure TLSPDiagnosticsHelper.ShowInMessages(const FileId: string);
begin
  var Messages := GI_PyIDEServices.Messages;
  Messages.ClearMessages;
  if Length(Self) > 0 then
  begin
    Messages.AddMessage(Summary);
    for var Diagnostic in Self do
      Messages.AddMessage(
        Diagnostic.SimpleMessage, FileId,
        Diagnostic.range.start.line + 1,
        Diagnostic.range.start.character + 1);
  end
  else
    Messages.AddMessage(_('Code Check') + ': ' + _('No issues found'));
  Messages.ShowWindow;
end;

function TLSPDiagnosticsHelper.Summary: string;
begin
  var Errors := 0;
  var Warnings := 0;
  var Hints := 0;
  for var Diagnostic in Self do
  begin
    case Diagnostic.severity of
      0, 1:Inc(Errors);
      2: Inc(Warnings);
    else
      Inc(Hints);
    end;
  end;
  Result := Format( _('File Check: Errors: %d, Warnings: %d, Hints: %d'),
                   [Errors, Warnings, Hints])
end;

function TLSPDiagnosticsHelper.ToBCArray(
  MaxSeverity: Integer): TArray<TBufferCoord>;
begin
  SetLength(Result, Length(Self));
  var Count := 0;
  for var Diagnostic in Self do
     if Diagnostic.severity <= MaxSeverity then
     begin
       Result[Count] := BufferCoord(Diagnostic.Range.start.character + 1,
                                    Diagnostic.Range.start.line + 1);
       Inc(Count);
     end;
  SetLength(Result, Count);
end;

end.
