{
   SynEdit plugin that automates the notifications between
   the LSP Client and the Server.
   (C) PyScripter 2020
}


unit SynEditLsp;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections,
  LspUtils,
  SynEditTypes,
  SynEdit;

type
  TDiagnostic = record
    Severity: TDiagnositicSeverity;
    BlockBegin,
    BlockEnd: TBufferCoord;
    Source: string;
    Msg: string;
  end;

  TDiagnostics = TThreadList<TDiagnostic>;

  TLspSynEditPlugin = class(TSynEditPlugin)
  private
    FFileId: string;
    FIncChanges: TJSONArray;
    FVersion: Int64;
    FTransmitChanges: Boolean;
    FDiagnostics: TDiagnostics;
    procedure FOnLspInitialized(Sender: TObject);
    class var Instances: TThreadList<TLspSynEditPlugin>;
    class function FindPluginWithFileId(const AFileId: string): TLspSynEditPlugin;
  protected
    procedure LinesInserted(FirstLine, Count: Integer); override;
    procedure LinesDeleted(FirstLine, Count: Integer); override;
    procedure LinesPutted(aIndex: Integer; aCount: Integer); override;
    procedure LinesChanged; override;
  public
    constructor Create(AOwner: TCustomSynEdit);
    destructor Destroy; override;
    procedure FileOpened(const FileId, LangId: string);
    procedure FileClosed;
    procedure FileSaved;
    procedure FileSavedAs(const FileId, LangId: string);
    procedure InvalidateErrorLines;
    property TransmitChanges: boolean read fTransmitChanges
      write fTransmitChanges;
    property Diagnostics: TDiagnostics read FDiagnostics;
    class constructor Create;
    class destructor Destroy;
    class procedure HandleLspNotify(const Method: string; Params: TJsonValue);
  end;

function LspPosition(const BC: TBufferCoord): TJsonObject; overload;
function LspDocPosition(const FileName: string; const BC: TBufferCoord): TJsonObject; overload;
function LspRange(const BlockBegin, BlockEnd: TBufferCoord): TJsonObject;
procedure RangeFromJson(Json: TJsonObject; out BlockBegin, BlockEnd: TBufferCoord);

implementation

uses
  Winapi.Windows,
  System.StrUtils,
  System.Threading,
  LspClient,
  JediLspClient,
  uEditAppIntfs,
  uCommonFunctions;

{ TLspSynEditPlugin }

constructor TLspSynEditPlugin.Create(AOwner: TCustomSynEdit);
begin
  inherited Create(AOwner);
  FIncChanges := TJSONArray.Create;
  FIncChanges.Owned := False;
  FDiagnostics := TDiagnostics.Create;
  TJedi.OnInitialized.AddHandler(FOnLspInitialized);
  Instances.Add(Self);
end;

destructor TLspSynEditPlugin.Destroy;
begin
  Instances.Remove(Self);
  TJedi.OnInitialized.RemoveHandler(FOnLspInitialized);
  FDiagnostics.Free;
  FIncChanges.Free;
  inherited;
end;

class destructor TLspSynEditPlugin.Destroy;
begin
  Instances.Free;
end;

procedure TLspSynEditPlugin.FileClosed;
begin
  var TempFileName := FFileId;
  FFileId := '';
  if not TJedi.Ready or
    not (lspscOpenCloseNotify in TJedi.LspClient.ServerCapabilities) or
    (TempFileName = '')
  then
    Exit;

  var Params := TSmartPtr.Make(TJsonObject.Create)();
  Params.AddPair('textDocument', LspTextDocumentIdentifier(TempFileName));
  TJedi.LspClient.Notify('textDocument/didClose', Params.ToJson);
end;

procedure TLspSynEditPlugin.FileOpened(const FileId, LangId: string);
begin
  FFileId := FileId;

  fVersion := 0;
  if not TJedi.Ready or (FileId = '') or (LangId <> 'python') then
  // support for didOpen is mandatory for the client
  // but I assume that is supported by the Server even if it does not say so
  // or not (lspscOpenCloseNotify in fLspClient.ServerCapabilities)
    Exit;

  var Params := TSmartPtr.Make(TJsonObject.Create)();
//  var OldTrailingLineBreak := Editor.Lines.TrailingLineBreak;
//  Editor.Lines.TrailingLineBreak := True;
//  try
    Params.AddPair('textDocument', LspTextDocumentItem(FileId, LangId,
      Editor.Text, 0));
//  finally
//    Editor.Lines.TrailingLineBreak := OldTrailingLineBreak;
//  end;
  TJedi.LspClient.Notify('textDocument/didOpen', Params.ToJson);
end;

procedure TLspSynEditPlugin.FileSaved;
begin
  if not TJedi.Ready or (FFileId = '') or
    not (lspscSaveNotify in TJedi.LspClient.ServerCapabilities)
  then
    Exit;

  var Params := TSmartPtr.Make(TJsonObject.Create)();
  Params.AddPair('textDocument', LspTextDocumentIdentifier(FFileId));
  TJedi.LspClient.Notify('textDocument/didSave', Params.ToJson);
end;

procedure TLspSynEditPlugin.FileSavedAs(const FileId, LangId: string);
begin
  if (FFileId <> '') then
    FileClosed;
  if (FileId <> '') and (LangId = 'python') then
    FileOpened(FileId, LangId);
end;

procedure TLspSynEditPlugin.FOnLspInitialized(Sender: TObject);
begin
  if FFileId <> '' then
    FileOpened(FFileId, 'python');
end;

class procedure TLspSynEditPlugin.HandleLspNotify(const Method: string;
  Params: TJsonValue);
var
  DiagArray: TArray<TDiagnostic>;
  LFileId : string;
begin
  if GI_PyIDEServices.IsClosing then Exit;
  if Method <> 'textDocument/publishDiagnostics' then Exit;

  Params.Owned := False;
  var Task := TTask.Create(procedure
  begin
    try
      var Uri: string;
      if not Params.TryGetValue('uri', Uri) then Exit;
      LFileId := FileIdFromUri(Uri);
      if LFileId = '' then Exit;
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

    if GI_PyIDEServices.IsClosing then Exit;

    TThread.ForceQueue(nil, procedure
    begin
      var Plugin := FindPluginWithFileId(LFileId);
      if not Assigned(Plugin) then Exit;

      var List := PlugIn.FDiagnostics.LockList;
      try
        PlugIn.InvalidateErrorLines; // old errors
        List.Clear;
        List.AddRange(DiagArray);
        PlugIn.InvalidateErrorLines;  // new errors
      finally
        PlugIn.FDiagnostics.UnLockList;
      end;
    end);
  end);
  Task.Start;
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

procedure TLspSynEditPlugin.InvalidateErrorLines;
begin
  { Should be called from the main thread }
  Assert(GetCurrentThreadId = MainThreadId);
  var List := FDiagnostics.LockList;
  try
     for var Diag in List do
       Editor.InvalidateLine(Diag.BlockBegin.Line);
  finally
    FDiagnostics.UnlockList;
  end;
end;

procedure TLspSynEditPlugin.LinesChanged;
begin
  Inc(fVersion);
  if not TJedi.Ready or (FFileId = '') or not fTransmitChanges then Exit;

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
      var Change := TJsonObject.Create;
//      var OldTrailingLineBreak := Editor.Lines.TrailingLineBreak;
//      Editor.Lines.TrailingLineBreak := True;
//      try
        Change.AddPair('text', TJsonString.Create(Editor.Text));
//      finally
//        Editor.Lines.TrailingLineBreak := OldTrailingLineBreak;
//      end;
      FIncChanges.Add(Change);
    end;
    Params.AddPair('contentChanges', FIncChanges as TJsonValue);
    TJedi.LspClient.Notify('textDocument/didChange', Params.ToJson);
  finally
    FIncChanges.Clear;
  end;
end;

procedure TLspSynEditPlugin.LinesDeleted(FirstLine, Count: Integer);
var
  Change: TJsonObject;
  BB, BE: TBufferCoord;
begin
  if not TJedi.Ready or (FFileId = '') or not fTransmitChanges or
    not (lspscIncrementalSync in TJedi.LspClient.ServerCapabilities)
  then
    Exit;

  BB := BufferCoord(1, FirstLine + 1);
  BE := BufferCoord(1, FirstLine + Count + 1);
  Change := TJsonObject.Create;
  Change.AddPair('range', LspRange(BB, BE));
  Change.AddPair('text', TJsonString.Create(''));
  fIncChanges.Add(Change);
end;

procedure TLspSynEditPlugin.LinesInserted(FirstLine, Count: Integer);
var
  Change: TJsonObject;
  BB, BE: TBufferCoord;
begin
  if not TJedi.Ready or (FFileId = '') or not fTransmitChanges or
    not (lspscIncrementalSync in TJedi.LspClient.ServerCapabilities)
  then
    Exit;

  BB := BufferCoord(1, FirstLine + 1);
  BE := BB;
  Change := TJsonObject.Create;
  Change.AddPair('range', LspRange(BB, BE));
  Change.AddPair('text',
    TJsonString.Create(DupeString(Editor.Lines.LineBreak, Count)));
  fIncChanges.Add(Change);
end;

procedure TLspSynEditPlugin.LinesPutted(aIndex, aCount: Integer);
var
  Change: TJsonObject;
  BB, BE: TBufferCoord;
begin
  if not TJedi.Ready or (FFileId = '') or not fTransmitChanges or
    not (lspscIncrementalSync in TJedi.LspClient.ServerCapabilities)
  then
    Exit;

  for var I := 0 to aCount - 1 do
  begin
    BB := BufferCoord(1, aIndex + I + 1);
    BE := BufferCoord(1, aIndex + I + 2);
    Change := TJsonObject.Create;
    Change.AddPair('range', LspRange(BB, BE));
    Change.AddPair('text', TJsonString.Create(Editor.Lines[aIndex + I] +
      Editor.Lines.LineBreak));
    fIncChanges.Add(Change);
  end;
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

class constructor TLspSynEditPlugin.Create;
begin
  Instances := TThreadList<TLspSynEditPlugin>.Create;
end;

end.
