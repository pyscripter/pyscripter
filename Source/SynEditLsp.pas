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
  System.JSON,
  System.SyncObjs,
  System.Math,
  System.Generics.Collections,
  System.Messaging,
  SynEditTypes,
  SynEdit,
  XLspTypes,
  cLSPClients;

type
  TLangId = (lidNone, lidPython);

  TDiagnostic = record
    Severity: TLSPDiagnosticSeverity;
    BlockBegin,
    BlockEnd: TBufferCoord;
    Source: string;
    Msg: string;
  end;

  TLspSynEditPlugin = class;

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

  TLspSynEditPlugin = class(TSynEditPlugin)
  public
    const DiagnosticsErrorIndicatorSpec: TGUID = '{48005990-9661-4DA7-A1B1-84A20045F37B}';
  private
    FFileId: string;
    FLangId: TLangId;
    FIncChanges: TList<TLSPBaseTextDocumentContentChangeEvent>;
    FVersion: NativeUInt;
    FTransmitChanges: Boolean;
    FDiagnostics:  TList<TDiagnostic>;
    FNewDiagnostics: TThreadList<TDiagnostic>;
    FNeedToRefreshDiagnostics: Boolean;
    FNeedToRefreshSymbols: Boolean;
    FDocSymbols: TDocSymbols;
    FLastDeletedLiine: string;
    class var FDocSymbolsLock: TRTLCriticalSection;
    class var Instances: TThreadList<TLspSynEditPlugin>;
    class function FindPluginWithFileId(const AFileId: string): TLspSynEditPlugin;
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
    procedure ApplyNewDiagnostics;
    procedure ClearDiagnostics;
    procedure RefreshSymbols;
    property TransmitChanges: Boolean read FTransmitChanges
      write FTransmitChanges;
    property Diagnostics: TList<TDiagnostic> read FDiagnostics;
    property NeedToRefreshSymbols: Boolean read FNeedToRefreshSymbols;
    property DocSymbols: TDocSymbols read FDocSymbols;
    class constructor Create;
    class destructor Destroy;
    class procedure OnLspInitialized(const Sender: TObject; const Msg:
        System.Messaging.TMessage);
    class procedure OnPublishDiagnostics(Sender: TObject; const uri: string;
      const version: Cardinal; const diagnostics: TArray<TLSPDiagnostic>);
  end;

implementation

uses
  System.SysUtils,
  System.UITypes,
  System.Threading,
  SynEditMiscProcs,
  SynEditMiscClasses,
  SynDWrite,
  XLSPFunctions,
  uEditAppIntfs,
  uCommonFunctions;

{ TLspSynEditPlugin }

constructor TLspSynEditPlugin.Create(AOwner: TCustomSynEdit);
begin
  inherited Create(AOwner);
  FHandlers := [phLinesInserted, phLinesBeforeDeleted, phLinesDeleted,
                phLinePut, phLinesChanged];
  FIncChanges := TList<TLSPBaseTextDocumentContentChangeEvent>.Create;
  FDiagnostics := TList<TDiagnostic>.Create;
  FNewDiagnostics := TThreadList<TDiagnostic>.Create;
  Instances.Add(Self);
  FDocSymbols := TDocSymbols.Create(Self);
  AOwner.Indicators.RegisterSpec(DiagnosticsErrorIndicatorSpec,
    TSynIndicatorSpec.New(sisSquiggleMicrosoftWord,
    D2D1ColorF(TColors.Red), clNoneF, []));
end;

destructor TLspSynEditPlugin.Destroy;
begin
  Instances.Remove(Self);
  FNewDiagnostics.Free;
  FDiagnostics.Free;
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

  for var Client in TPyLspClient.LspClients do
    if Client.Ready and (OldFileId <> '') and
      Client.LspClient.ServerCapabilities.textDocumentSync.openClose then
    begin
      var Params := TSmartPtr.Make(TLSPDidCloseTextDocumentParams.Create)();
      Params.textDocument.uri := FileIdToURI(OldFileId);
      Client.LspClient.SendNotification(lspDidCloseTextDocument, Params);
    end;
end;

procedure TLspSynEditPlugin.FileOpened(const FileId: string; LangId: TLangId;
  AClient: TPyLspClient = nil);
{ Support for didOpen is mandatory for the client
  but I assume that is supported by the Server even if it does not say so
  or not (lspscOpenCloseNotify in fLspClient.ServerCapabilities) }
begin
  Assert(FileId <> '', 'TLspSynEditPlugin.FileOpened');
  FFileId := FileId;
  FLangId := LangId;
  FNeedToRefreshSymbols := False;

  FVersion := 0;
  FDocSymbols.Clear;

  if LangId <> lidPython then Exit;

  for var Client in TPyLspClient.LspClients do
    if ((AClient = nil) or (AClient = Client)) and Client.Ready and
      Client.LspClient.ServerCapabilities.textDocumentSync.openClose then
    begin
      var Params := TSmartPtr.Make(TLSPDidOpenTextDocumentParams.Create)();
      Params.textDocument.uri := FileIdToURI(FileId);
      Params.textDocument.languageId := 'python';
      Params.textDocument.text := Editor.Text;
      Client.LspClient.SendNotification(lspDidOpenTextDocument, Params);
    end;

  FTransmitChanges := True;
  RefreshSymbols;
end;

procedure TLspSynEditPlugin.FileSaved;
begin
  if (FFileId = '') or (FLangId <> lidPython) then Exit;

  for var Client in TPyLspClient.LspClients do
    if Client.Ready and
      Client.LspClient.ServerCapabilities.textDocumentSync.save.value then
    begin
      var Params := TSmartPtr.Make(TLSPDidSaveTextDocumentParams.Create)();
      Params.textDocument.uri := FileIdToURI(FFileId);
      if Client.LspClient.ServerCapabilities.textDocumentSync.save.includeText then
        Params.text := Editor.Text;
      Client.LspClient.SendNotification(lspDidSaveTextDocument, Params);
    end;
end;

procedure TLspSynEditPlugin.FileSavedAs(const FileId: string; LangId: TLangId);
begin
  FileClosed;
  FileOpened(FileId, LangId);
end;

class procedure TLspSynEditPlugin.OnPublishDiagnostics(Sender: TObject;
  const uri: string; const version: Cardinal;
  const diagnostics: TArray<TLSPDiagnostic>);
var
  Diagnostic: TDiagnostic;
begin
  if GI_PyIDEServices.IsClosing then Exit;

  var LFileId := FileIdFromURI(Uri);
  if LFileId = '' then Exit;

  // Exit if we cannot find the Plugin
  var Plugin := FindPluginWithFileId(LFileId);
  if not Assigned(Plugin) then Exit;

  // Exit if the diagnostics are not for the current version
  if (version <> 0) and (version <> Plugin.FVersion) then Exit;

  var List := Plugin.FNewDiagnostics.LockList;
  try
    List.Clear;
    for var LspDiagnostic in diagnostics do
    begin
      Diagnostic.Severity := LspDiagnostic.severity;
      Diagnostic.Msg := LspDiagnostic.message;
      Diagnostic.Source := LspDiagnostic.source;
      BufferCoordinatesFromLspRange(LspDiagnostic.range,
        Diagnostic.BlockBegin, Diagnostic.BlockEnd);
      List.Add(Diagnostic);
    end;
    Plugin.FNeedToRefreshDiagnostics := True;
  finally
    Plugin.FNewDiagnostics.UnlockList;
  end;
end;

class function TLspSynEditPlugin.FindPluginWithFileId(
  const AFileId: string): TLspSynEditPlugin;
begin
  Result := nil;
  var List := Instances.LockList;
  try
     for var Plugin in List do
       if SameFileName(AFileId, Plugin.FFileId) then
          Exit(Plugin);
  finally
    Instances.UnlockList;
  end;
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
        // too many changes - faster to send all text
        InRange(FIncChanges.Count, 1, Editor.Lines.Count div 3)
      then
      begin
        Params.contentChanges := FIncChanges.ToArray;
        // the TLSPBaseTextDocumentContentChangeEvents will be detroyed by
        // the Params (TLSPDidChangeTextDocumentParams) destructor
        FIncChanges.Clear;
      end
      else
      begin
        Change := TLSPBaseTextDocumentContentChangeEvent.Create;
        Change.text := Editor.Text;
        Params.contentChanges := [Change];
      end;
      Client.LspClient.SendNotification(lspDidChangeTextDocument, Params);
    end;
  finally
    // if the FIncChanges were not used we need to destroy them here
    for var Event in FIncChanges do
      Event.Free;
    FIncChanges.Clear;
  end;

  FNeedToRefreshSymbols := True;
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
  if Sender = TPyLspClient.MainLspClient then
    TPyLspClient.MainLspClient.LspClient.OnPublishDiagnostics :=
      OnPublishDiagnostics;

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
  if not (TPyLspClient.MainLspClient.Ready and
    TPyLspClient.MainLspClient.LspClient.IsRequestSupported(lspDocumentSymbol))
  then
    Exit;

  var Params := TSmartPtr.Make(TLSPDocumentSymbolParams.Create)();
  Params.textDocument.uri := FileIdToURI(FFileId);

  FDocSymbols.FRequestId := TPyLspClient.MainLspClient.LspClient.SendRequest(
    lspDocumentSymbol, Params,
    procedure(AJson: TJSONObject)
    begin
      FDocSymbolsLock.Enter;
      try
        FreeAndNil(FDocSymbols.Symbols);
        if ResponseError(AJson) then Exit;
        var JsonResult := AJson.Values['result'];

        // Check if this is the latest docsymbol request
        var Id := AJson.GetValue<Integer>('id', -1);
        if Id <> FDocSymbols.FRequestId then Exit;

        FDocSymbols.Symbols := TList<TLSPDocumentSymbol>.Create;

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

procedure TLspSynEditPlugin.ApplyNewDiagnostics;
begin
  { Should be called from the main thread }
  Assert(GetCurrentThreadId = MainThreadID, 'TLspSynEditPlugin.ApplyNewDiagnostics');
  if not FNeedToRefreshDiagnostics then Exit;

  ClearDiagnostics;
  var List := FNewDiagnostics.LockList;
  try
    FDiagnostics.AddRange(List);
    FNeedToRefreshDiagnostics := False;
    for var I := 0 to FDiagnostics.Count - 1 do
      Editor.Indicators.Add(FDiagnostics[I].BlockBegin.Line,
        TSynIndicator.New(DiagnosticsErrorIndicatorSpec,
        FDiagnostics[I].BlockBegin.Char, FDiagnostics[I].BlockEnd.Char, I));
  finally
    FNewDiagnostics.UnlockList;
  end;
end;

procedure TLspSynEditPlugin.ClearDiagnostics;
begin
  Editor.Indicators.Clear(DiagnosticsErrorIndicatorSpec);
  FDiagnostics.Clear;
end;

class constructor TLspSynEditPlugin.Create;
begin
  Instances := TThreadList<TLspSynEditPlugin>.Create;
  FDocSymbolsLock.Initialize;

  TMessageManager.DefaultManager.SubscribeToMessage(TLspServerInitializedMessage,
    OnLspInitialized);
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

end.
