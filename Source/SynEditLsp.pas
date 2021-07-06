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
  SynEditTypes,
  SynEdit;

type
  TLspSynEditPlugin = class(TSynEditPlugin)
  private
    FFileName: string;
    FIncChanges: TJSONArray;
    FVersion: Int64;
    FTransmitChanges: Boolean;
    procedure FOnLspInitialized(Sender: TObject);
  protected
    procedure LinesInserted(FirstLine, Count: Integer); override;
    procedure LinesDeleted(FirstLine, Count: Integer); override;
    procedure LinesPutted(aIndex: Integer; aCount: Integer); override;
    procedure LinesChanged; override;
  public
    constructor Create(AOwner: TCustomSynEdit);
    destructor Destroy; override;
    procedure FileOpened(const FileName, LanguageID: string);
    procedure FileClosed;
    procedure FileSaved;
    procedure FileSavedAs(const FileName, LanguageID: string);
    property TransmitChanges: boolean read fTransmitChanges
      write fTransmitChanges;
  end;

function LspPosition(const BC: TBufferCoord): TJsonObject; overload;
function LspDocPosition(const FileName: string; const BC: TBufferCoord): TJsonObject; overload;
function LspRange(const BlockBegin, BlockEnd: TBufferCoord): TJsonObject;

implementation

uses
  System.StrUtils,
  LspUtils,
  LspClient,
  JediLspClient,
  uCommonFunctions;

{ TLspSynEditPlugin }

constructor TLspSynEditPlugin.Create(AOwner: TCustomSynEdit);
begin
  inherited Create(AOwner);
  FIncChanges := TJSONArray.Create;
  FIncChanges.Owned := False;
  TJedi.OnInitialized.AddHandler(FOnLspInitialized);
end;

destructor TLspSynEditPlugin.Destroy;
begin
  TJedi.OnInitialized.RemoveHandler(FOnLspInitialized);
  FIncChanges.Free;
  inherited;
end;

procedure TLspSynEditPlugin.FileClosed;
begin
  var TempFileName := fFileName;
  fFileName := '';
  if not TJedi.Ready or
    not (lspscOpenCloseNotify in TJedi.LspClient.ServerCapabilities) or
    (TempFileName = '')
  then
    Exit;

  var Params := TSmartPtr.Make(TJsonObject.Create)();
  Params.AddPair('textDocument', LspTextDocumentIdentifier(TempFileName));
  TJedi.LspClient.Notify('textDocument/didClose', Params.ToJson);
end;

procedure TLspSynEditPlugin.FileOpened(const FileName, LanguageID: string);
begin
  fFileName := FileName;
  fVersion := 0;
  if not TJedi.Ready or (FileName = '') or (LanguageId <> 'python') then
  // support for didOpen is mandatory for the client
  // but I assume that is supported by the Server even if it does not say so
  // or not (lspscOpenCloseNotify in fLspClient.ServerCapabilities)
    Exit;

  var Params := TSmartPtr.Make(TJsonObject.Create)();
  var OldTrailingLineBreak := Editor.Lines.TrailingLineBreak;
  Editor.Lines.TrailingLineBreak := True;
  try
    Params.AddPair('textDocument', LspTextDocumentItem(FileName, LanguageId,
      Editor.Text, 0));
  finally
    Editor.Lines.TrailingLineBreak := OldTrailingLineBreak;
  end;
  TJedi.LspClient.Notify('textDocument/didOpen', Params.ToJson);
end;

procedure TLspSynEditPlugin.FileSaved;
begin
  if not TJedi.Ready or (FFileName = '') or
    not (lspscSaveNotify in TJedi.LspClient.ServerCapabilities)
  then
    Exit;

  var Params := TSmartPtr.Make(TJsonObject.Create)();
  Params.AddPair('textDocument', LspTextDocumentIdentifier(fFileName));
  TJedi.LspClient.Notify('textDocument/didClose', Params.ToJson);
end;

procedure TLspSynEditPlugin.FileSavedAs(const FileName, LanguageID: string);
begin
  if (fFileName <> '') then
    FileClosed;
  if LanguageID = 'python' then
    FileOpened(FileName, LanguageID);
end;

procedure TLspSynEditPlugin.FOnLspInitialized(Sender: TObject);
begin
  FileOpened(FFileName, 'python');
end;

procedure TLspSynEditPlugin.LinesChanged;
begin
  Inc(fVersion);
  if not TJedi.Ready or (FFileName = '') or not fTransmitChanges then Exit;

  var Params := TSmartPtr.Make(TJsonObject.Create)();
  try
    Params.AddPair('textDocument', LspVersionedTextDocumentIdentifier(FFileName,
      FVersion));
    if not(lspscIncrementalSync in TJedi.LspClient.ServerCapabilities) or
      // too many changes - faster to send all text
      (FIncChanges.Count > Editor.Lines.Count div 3)
    then
    begin
      FIncChanges.Clear;
      var Change := TJsonObject.Create;
      var OldTrailingLineBreak := Editor.Lines.TrailingLineBreak;
      Editor.Lines.TrailingLineBreak := True;
      try
        Change.AddPair('text', TJsonString.Create(Editor.Text));
      finally
        Editor.Lines.TrailingLineBreak := OldTrailingLineBreak;
      end;
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
  if not TJedi.Ready or (FFileName = '') or not fTransmitChanges or
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
  if not TJedi.Ready or (FFileName = '') or not fTransmitChanges or
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
  if not TJedi.Ready or (FFileName = '') or not fTransmitChanges or
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

end.
