{
   SynEdit plugin that automates the notifications between
   the LSP Client and the Server.
   (C) PyScripter 2020
}

unit SynEditLsp;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.SyncObjs,
  System.Generics.Collections,
  SynEditTypes,
  SynEdit,
  LspUtils;

type
  TLangId = (lidNone, lidPython);

  TDiagnostic = record
    Severity: TDiagnositicSeverity;
    BlockBegin,
    BlockEnd: TBufferCoord;
    Source: string;
    Msg: string;
  end;

  TLspSynEditPlugin = class;

  TDocSymbols = class
    {Asynchronous symbol support for Code Explorer}
  private
    FPlugIn: TLspSynEditPlugin;
    FRefreshing: Boolean;
    FOnNotify: TNotifyEvent;
    function GetFileId: string;
  public
    ModuleNode: TObject;  // for storing Code Explorer TModuleNodeCE
    Symbols: TJsonArray;
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
    FIncChanges: TJSONArray;
    FVersion: NativeUInt;
    FTransmitChanges: Boolean;
    FDiagnostics:  TList<TDiagnostic>;
    FNewDiagnostics: TThreadList<TDiagnostic>;
    FNeedToRefreshDiagnostics: Boolean;
    FNeedToRefreshSymbols: Boolean;
    FDocSymbols: TDocSymbols;
    procedure FOnLspInitialized(Sender: TObject);
    class var FDocSymbolsLock: TRTLCriticalSection;
    class var FSymbolsRequests: TStringList;
    class var Instances: TThreadList<TLspSynEditPlugin>;
    class function FindPluginWithFileId(const AFileId: string): TLspSynEditPlugin;
    class procedure HandleSymbolsResponse(Id: NativeUInt; Result, Error: TJsonValue);
  protected
    procedure LinesInserted(FirstLine, Count: Integer); override;
    procedure LinesDeleted(FirstLine, Count: Integer); override;
    procedure LinePut(aIndex: Integer; const OldLine: string); override;
    procedure LinesChanged; override;
  public
    constructor Create(AOwner: TCustomSynEdit);
    destructor Destroy; override;
    procedure FileOpened(const FileId: string; LangId: TLangId);
    procedure FileClosed;
    procedure FileSaved;
    procedure FileSavedAs(const FileId: string; LangId: TLangId);
    procedure ApplyNewDiagnostics;
    procedure ClearDiagnostics;
    property TransmitChanges: boolean read fTransmitChanges
      write fTransmitChanges;
    property Diagnostics: TList<TDiagnostic> read FDiagnostics;
    property NeedToRefreshSymbols: Boolean read FNeedToRefreshSymbols;
    property DocSymbols: TDocSymbols read FDocSymbols;
    class constructor Create;
    class destructor Destroy;
    class procedure RefreshSymbols(const AFileId: string);
    class procedure OnLspShutDown(Sender: TObject);
    class procedure HandleLspNotify(const Method: string; Params: TJsonValue);
  end;

function LspPosition(const BC: TBufferCoord): TJsonObject; overload;
function LspDocPosition(const FileName: string; const BC: TBufferCoord): TJsonObject; overload;
function LspRange(const BlockBegin, BlockEnd: TBufferCoord): TJsonObject;
procedure RangeFromJson(Json: TJsonObject; out BlockBegin, BlockEnd: TBufferCoord);

implementation

uses
  System.UITypes,
  System.Threading,
  System.Math,
  SynEditMiscProcs,
  SynEditMiscClasses,
  SynDWrite,
  LspClient,
  JediLspClient,
  uEditAppIntfs,
  uCommonFunctions;

{ TLspSynEditPlugin }

constructor TLspSynEditPlugin.Create(AOwner: TCustomSynEdit);
begin
  inherited Create(AOwner);
  FHandlers := [phLinesInserted, phLinesDeleted, phLinePut, phLinesChanged];
  FIncChanges := TJSONArray.Create;
  FIncChanges.Owned := False;
  FDiagnostics := TList<TDiagnostic>.Create;
  FNewDiagnostics := TThreadList<TDiagnostic>.Create;
  TJedi.OnInitialized.AddHandler(FOnLspInitialized);
  Instances.Add(Self);
  FDocSymbols := TDocSymbols.Create(Self);
  AOwner.Indicators.RegisterSpec(DiagnosticsErrorIndicatorSpec,
    TSynIndicatorSpec.New(sisSquiggleMicrosoftWord,
    D2D1ColorF(TColors.Red), clNoneF, []));
end;

destructor TLspSynEditPlugin.Destroy;
begin
  Instances.Remove(Self);
  TJedi.OnInitialized.RemoveHandler(FOnLspInitialized);
  FNewDiagnostics.Free;
  FDiagnostics.Free;
  FIncChanges.Free;
  FDocSymbols.Free;
  inherited;
end;

class destructor TLspSynEditPlugin.Destroy;
begin
  Instances.Free;
  FDocSymbolsLock.Free;
  FSymbolsRequests.Free;
end;

procedure TLspSynEditPlugin.FileClosed;
begin
  FDocSymbols.Clear;
  var OldLangId := FLangId;
  FLangId := lidNone;
  var OldFileId := FFileId;
  FFileId := '';
  if not TJedi.Ready or (OldLangId <> lidPython) or (OldFileId = '') or
     not (lspscOpenCloseNotify in TJedi.LspClient.ServerCapabilities)
  then
    Exit;

  var Params := TSmartPtr.Make(TJsonObject.Create)();
  Params.AddPair('textDocument', LspTextDocumentIdentifier(OldFileId));
  TJedi.LspClient.Notify('textDocument/didClose', Params.ToJson);
end;

procedure TLspSynEditPlugin.FileOpened(const FileId: string; LangId: TLangId);
{ Support for didOpen is mandatory for the client
  but I assume that is supported by the Server even if it does not say so
  or not (lspscOpenCloseNotify in fLspClient.ServerCapabilities) }
begin
  Assert(FileId <> '');
  FFileId := FileId;
  FLangId := LangId;
  FNeedToRefreshSymbols := False;

  FVersion := 0;
  if not TJedi.Ready or (LangId <> lidPython) then
  begin
    FDocSymbols.Clear;
    Exit;
  end;

  var Params := TSmartPtr.Make(TJsonObject.Create)();
  Params.AddPair('textDocument', LspTextDocumentItem(FileId, 'python',
    Editor.Text, 0));
  TJedi.LspClient.Notify('textDocument/didOpen', Params.ToJson);

  FTransmitChanges := True;
  RefreshSymbols(FileId);
end;

procedure TLspSynEditPlugin.FileSaved;
begin
  if not TJedi.Ready or (FFileId = '') or (FLangId <> lidPython) or
    not (lspscSaveNotify in TJedi.LspClient.ServerCapabilities)
  then
    Exit;

  var Params := TSmartPtr.Make(TJsonObject.Create)();
  Params.AddPair('textDocument', LspTextDocumentIdentifier(FFileId));
  TJedi.LspClient.Notify('textDocument/didSave', Params.ToJson);
end;

procedure TLspSynEditPlugin.FileSavedAs(const FileId: string; LangId: TLangId);
begin
  FileClosed;
  FileOpened(FileId, LangId);
end;

procedure TLspSynEditPlugin.FOnLspInitialized(Sender: TObject);
begin
  if (FFileId <> '') and (FLangId = lidPython) then
    FileOpened(FFileId, lidPython);
end;

class procedure TLspSynEditPlugin.OnLspShutDown(Sender: TObject);
begin
  FDocSymbolsLock.Enter;
  try
    for var Index := 0 to FSymbolsRequests.Count - 1 do
      TJedi.LspClient.CancelRequest(NativeUInt(FSymbolsRequests.Objects[Index]));

    FSymbolsRequests.Clear;
  finally
    FDocSymbolsLock.Leave;
  end;
end;

class procedure TLspSynEditPlugin.HandleLspNotify(const Method: string;
  Params: TJsonValue);
begin
  if GI_PyIDEServices.IsClosing or (Method <> 'textDocument/publishDiagnostics') then
  begin
    Params.Free;
    Exit;
  end;

  var Task := TTask.Create(procedure
  var
    DiagArray: TArray<TDiagnostic>;
    LFileId: string;
    DocVersion: NativeUInt;
  begin
    try
      var Uri: string;
      if not Params.TryGetValue('uri', Uri) then Exit;
      LFileId := FileIdFromUri(Uri);
      if LFileId = '' then Exit;

      // compare versions
      if Params.TryGetValue('version', DocVersion) then
      begin
        var Plugin := FindPluginWithFileId(LFileId);
        if not Assigned(Plugin) then Exit;
        if DocVersion <> Plugin.FVersion then
          Exit;
      end;

      var Diagnostics := Params.FindValue('diagnostics');
      if not (Diagnostics is TJSONArray) then Exit;

      var Diag: TJsonValue;
      SetLength(DiagArray, TJsonArray(Diagnostics).Count);

      for var I := 0  to TJsonArray(Diagnostics).Count - 1 do
      begin
        Diag := TJsonArray(Diagnostics).Items[I];
        DiagArray[I].Severity := TDiagnositicSeverity(Diag.GetValue<integer>('severity'));
        DiagArray[I].Msg := Diag.GetValue<string>('message');
        DiagArray[I].Source := Diag.GetValue<string>('source');
        RangeFromJson(Diag as TJsonObject, DiagArray[I].BlockBegin, DiagArray[I].BlockEnd);
      end;
    finally
      Params.Free;
    end;

    // Find plugin again just in case it has been destroyed
    var Plugin := FindPluginWithFileId(LFileId);
    if not Assigned(Plugin) then Exit;

    var List := PlugIn.FNewDiagnostics.LockList;
    try
      List.Clear;
      List.AddRange(DiagArray);
      Plugin.FNeedToRefreshDiagnostics := True;
    finally
      PlugIn.FNewDiagnostics.UnLockList;
    end;
  end);
  Task.Start;
end;

class procedure TLspSynEditPlugin.HandleSymbolsResponse(Id: NativeUInt; Result,
  Error: TJsonValue);
begin
  FDocSymbolsLock.Enter;
  try
    var Index := FSymbolsRequests.IndexOfObject(TObject(Id));
    if Index < 0  then Exit;

    var PlugIn := FindPluginWithFileId(FSymbolsRequests[Index]);
    if not Assigned(PlugIn) then Exit;

    FSymbolsRequests.Delete(Index);

    var DocSymbols := Plugin.DocSymbols;
    FreeAndNil(DocSymbols.Symbols);
    if (Result <> nil) and (Result is TJSONArray) then
    begin
      DocSymbols.Symbols := TJsonArray(Result);
      Result := nil;
    end
    else
      DocSymbols.Symbols := TJSONArray.Create;
    if Assigned(DocSymbols.FOnNotify) then
      DocSymbols.FOnNotify(DocSymbols);
  finally
    FDocSymbolsLock.Leave;
    Result.Free;
    Error.Free;
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

procedure TLspSynEditPlugin.LinesChanged;
begin
  Inc(FVersion);
  if not TJedi.Ready or (FFileId = '') or (FLangId <> lidPython) or
    not FTransmitChanges or (FIncChanges.Count = 0)
  then
    Exit;

  var Params := TSmartPtr.Make(TJsonObject.Create)();
  try
    Params.AddPair('textDocument', LspVersionedTextDocumentIdentifier(FFileId,
      FVersion));
    if not(lspscIncrementalSync in TJedi.LspClient.ServerCapabilities) or
      // too many changes - faster to send all text
      (FIncChanges.Count > Editor.Lines.Count div 3)
    then
    begin
      FIncChanges.Clear;
      var Text := Editor.Text;

      var Change := TJsonObject.Create;
      Change.AddPair('text', TJsonString.Create(Text));

      FIncChanges.Add(Change);
    end;
    Params.AddPair('contentChanges', FIncChanges as TJsonValue);
    TJedi.LspClient.Notify('textDocument/didChange', Params.ToJson);
  finally
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
  Change: TJsonObject;
  BB, BE: TBufferCoord;
begin
  if not TJedi.Ready or (FFileId = '') or (FLangId <> lidPython) or
    not FTransmitChanges or
    not (lspscIncrementalSync in TJedi.LspClient.ServerCapabilities)
  then
    Exit;

  BB := BufferCoord(1, FirstLine + 1);
  if FirstLine = Editor.Lines.Count then
    // we have deleted the last line
    BE := BufferCoord(Editor.Lines[FirstLine + Count - 1].Length + 1, FirstLine + Count)
  else
    BE := BufferCoord(1, FirstLine + Count + 1);
  Change := TJsonObject.Create;
  Change.AddPair('range', LspRange(BB, BE));
  Change.AddPair('text', TJsonString.Create(''));
  fIncChanges.Add(Change);
end;

procedure TLspSynEditPlugin.LinesInserted(FirstLine, Count: Integer);
{
   FirstLine 0-based
   Called after insertion therefore Lines.Count >= Count and
   0 <= FirstLine <= Lines.Count - Count
}
var
  Change: TJsonObject;
  BB, BE: TBufferCoord;
begin
  if not TJedi.Ready or (FFileId = '') or (FlangId <> lidPython) or
    not FTransmitChanges or (Count <= 0) or
    not (lspscIncrementalSync in TJedi.LspClient.ServerCapabilities)
  then
    Exit;

  if (FirstLine = Editor.Lines.Count - Count) and (FirstLine > 0) then
    // Special case: Added at the end and not starting from the first line
    BB := BufferCoord(Editor.Lines[FirstLine - 1].Length + 1, FirstLine)
  else
    BB := BufferCoord(1, FirstLine + 1);
  BE := BB;

  var TextAdded := '';
  for var I := FirstLine to FirstLine + Count - 1 do
    TextAdded := TextAdded + Editor.Lines[I]  + SLineBreak;
  if (FirstLine = Editor.Lines.Count - Count) and (FirstLine > 0) then
    // Lines added at the end
    TextAdded := SLineBreak + TextAdded;
  if (FirstLine = Editor.Lines.Count - Count) then
    // Lines added at the end
    Delete(TextAdded, TextAdded.Length - Length(SLineBreak) + 1, Length(SLineBreak));

  Change := TJsonObject.Create;
  Change.AddPair('range', LspRange(BB, BE));
  Change.AddPair('text', TJsonString.Create(TextAdded));
  fIncChanges.Add(Change);
end;

procedure TLspSynEditPlugin.LinePut(aIndex: Integer; const OldLine: string);
var
  NewLine: string;
  Start, OldLen, NewLen: integer;
begin
  if not TJedi.Ready or (FFileId = '') or (FLangId <> lidPython) or
    not FTransmitChanges or
    not (lspscIncrementalSync in TJedi.LspClient.ServerCapabilities)
  then
    Exit;

  NewLine := Editor.Lines[aIndex];
  LineDiff(NewLine, OldLine, Start, OldLen, NewLen);

  var Diff := Copy(NewLine, Start, NewLen);
  var BB := BufferCoord(Start, aIndex + 1);
  var BE := BufferCoord(Start + OldLen, aIndex + 1);

  if (Diff = '') and (BB = BE) then Exit;  // no change

  var Change := TJsonObject.Create;
  Change.AddPair('range', LspRange(BB, BE));
  Change.AddPair('text', TJsonString.Create(Diff));
  fIncChanges.Add(Change);
end;

class procedure TLspSynEditPlugin.RefreshSymbols(const AFileId: string);
begin
  if not TJedi.Ready then Exit;

  var Task := TTask.Create(procedure
  begin
    var PlugIn := FindPluginWithFileId(AFileId);
    if not Assigned(PlugIn) or Plugin.DocSymbols.FRefreshing then Exit;
    Plugin.DocSymbols.FRefreshing := True;

    FDocSymbolsLock.Enter;
    try
      var Index := FSymbolsRequests.IndexOf(AFileId);
      if Index >= 0 then
      begin
        TJedi.LspClient.CancelRequest(NativeUInt(FSymbolsRequests.Objects[Index]));
        FSymbolsRequests.Delete(Index);
      end;
      var Param := TSmartPtr.Make(TJsonObject.Create)();
      Param.AddPair('textDocument', LspTextDocumentIdentifier(AFileId));

      var Id := TJedi.LspClient.Request('textDocument/documentSymbol',
        Param.ToJson, HandleSymbolsResponse);
      FSymbolsRequests.AddObject(AFileId, TObject(Id));
    finally
      Plugin.DocSymbols.FRefreshing := False;
      Plugin.FNeedToRefreshSymbols := False;
      FDocSymbolsLock.Leave;
    end;
  end);
  Task.Start;
end;

function LspPosition(const BC: TBufferCoord): TJsonObject; overload;
begin
  Result := TJsonObject.Create;
  Result.AddPair('line', TJSONNumber.Create(BC.Line - 1));
  Result.AddPair('character', TJSONNumber.Create(BC.Char - 1));
end;

function LspDocPosition(const FileName: string; const BC: TBufferCoord): TJsonObject; overload;
begin
  Result := LspDocPosition(FileName, BC.Line - 1, BC.Char - 1);
end;

function LspRange(const BlockBegin, BlockEnd: TBufferCoord): TJsonObject;
begin
  Result := TJsonObject.Create;
  Result.AddPair('start', LspPosition(BlockBegin));
  Result.AddPair('end', LspPosition(BlockEnd));
end;

procedure RangeFromJson(Json: TJsonObject; out BlockBegin, BlockEnd: TBufferCoord);
begin
  BlockBegin := Default(TBufferCoord);
  BlockEnd := BlockBegin;

  if not Assigned(Json) then Exit;

  var Range := Json.FindValue('range');
  if not (Range is TJSONObject) then Exit;

  if Range.TryGetValue('start.line', BlockBegin.Line) then Inc(BlockBegin.Line);
  if Range.TryGetValue('start.character', BlockBegin.Char) then Inc(BlockBegin.Char);
  if Range.TryGetValue('end.line', BlockEnd.Line) then Inc(BlockEnd.Line);
  if Range.TryGetValue('end.character', BlockEnd.Char) then Inc(BlockEnd.Char);
end;

procedure TLspSynEditPlugin.ApplyNewDiagnostics;
begin
  { Should be called from the main thread }
  Assert(GetCurrentThreadId = MainThreadId);
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
  FSymbolsRequests := TStringList.Create;
  FSymbolsRequests.CaseSensitive := False;
  FSymbolsRequests.UseLocale := True;
  FSymbolsRequests.Duplicates := dupError;
  FSymbolsRequests.Sorted := True;
end;

{ TDocSymbols }

procedure TDocSymbols.Clear;
begin
  Lock;
  try
    var Index := FPlugIn.FSymbolsRequests.IndexOf(FileId);
    if Index >= 0 then
    begin
      TJedi.LspClient.CancelRequest(NativeUInt(FPlugIn.FSymbolsRequests.Objects[Index]));
      FPlugIn.FSymbolsRequests.Delete(Index);
    end;

    FreeAndNil(Symbols);
    if Assigned(FOnNotify) then
      FOnNotify(Self);
  finally
    UnLock;
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
  Result := FPlugin.FFileId;
end;

procedure TDocSymbols.Lock;
begin
  FPlugIn.FDocSymbolsLock.Enter;
end;

procedure TDocSymbols.Unlock;
begin
  FPlugIn.FDocSymbolsLock.Leave;
end;

end.
