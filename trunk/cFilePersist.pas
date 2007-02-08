unit cFilePersist;
{-----------------------------------------------------------------------------
 Unit Name: cFilePersist
 Author:    Kiriakos Vlahos
 Date:      09-Mar-2006
 Purpose:   Support class for editor file persistence
 History:
-----------------------------------------------------------------------------}

interface
Uses
  Classes, SysUtils, Contnrs, JvAppStorage, uEditAppIntfs, dlgSynEditOptions;

Type
  TBookMarkInfo = class(TPersistent)
  private
    fLine, fChar, fBookmarkNumber : integer;
  published
    property Line : integer read fLine write fLine;
    property Char : integer read fChar write fChar;
    property BookmarkNumber : integer read fBookmarkNumber write fBookmarkNumber;
  end;

  TFilePersistInfo = class (TInterfacedPersistent, IJvAppStorageHandler)
  //  For storage/loading of a file's persistent info
  private
    Line, Char, TopLine : integer;
    BreakPoints : TObjectList;
    BookMarks : TObjectList;
    FileName : string;
    Highlighter : string;
    EditorOptions : TSynEditorOptionsContainer;
  protected
    // IJvAppStorageHandler implementation
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
    function CreateListItem(Sender: TJvCustomAppStorage; const Path: string;
      Index: Integer): TPersistent;
  public
    constructor Create;
    constructor CreateFromEditor(Editor : IEditor);
    destructor Destroy; override;
  end;

  TPersistFileInfo = class
  // Stores/loads open editor file information through the class methods
  // WriteToAppStorage and ReadFromAppStorage
  private
    fFileInfoList : TObjectList;
    function CreateListItem(Sender: TJvCustomAppStorage; const Path: string;
      Index: Integer): TPersistent;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GetFileInfo;
    class procedure WriteToAppStorage(AppStorage : TJvCustomAppStorage; Path : String);
    class procedure ReadFromAppStorage(AppStorage : TJvCustomAppStorage; Path : String);
  end;

  implementation

uses
  cPyBaseDebugger, frmPyIDEMain, SynEditTypes, dmCommands, uHighlighterProcs;

{ TFilePersistInfo }

constructor TFilePersistInfo.Create;
begin
  BreakPoints := TObjectList.Create(True);
  BookMarks := TObjectList.Create(True);
  EditorOptions := TSynEditorOptionsContainer.Create(nil);
end;

procedure TFilePersistInfo.WriteToAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
var
  IgnoreProperties : TStringList;
begin
   AppStorage.WriteString(BasePath+'\FileName', FileName);
   AppStorage.WriteInteger(BasePath+'\Line', Line);
   AppStorage.WriteInteger(BasePath+'\Char', Char);
   AppStorage.WriteInteger(BasePath+'\TopLine', TopLine);
   AppStorage.WriteString(BasePath+'\Highlighter', Highlighter);
   AppStorage.WriteObjectList(BasePath+'\BreakPoints', BreakPoints, 'BreakPoint');
   AppStorage.WriteObjectList(BasePath+'\BookMarks', BookMarks, 'BookMarks');
   IgnoreProperties := TStringList.Create;
   try
     IgnoreProperties.Add('Keystrokes');
     AppStorage.WritePersistent(BasePath+'\Editor Options', EditorOptions,
       True, IgnoreProperties);
   finally
     IgnoreProperties.Free;
   end;
end;

procedure TFilePersistInfo.ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
begin
   FileName := AppStorage.ReadString(BasePath+'\FileName');
   Line := AppStorage.ReadInteger(BasePath+'\Line');
   Char := AppStorage.ReadInteger(BasePath+'\Char');
   TopLine := AppStorage.ReadInteger(BasePath+'\TopLine');
   Highlighter := AppStorage.ReadString(BasePath+'\Highlighter', Highlighter);
   AppStorage.ReadObjectList(BasePath+'\BreakPoints', BreakPoints, CreateListItem, True, 'BreakPoint');
   AppStorage.ReadObjectList(BasePath+'\BookMarks', BookMarks, CreateListItem, True, 'BookMarks');
   EditorOptions.Assign(CommandsDataModule.EditorOptions);
   AppStorage.ReadPersistent(BasePath+'\Editor Options', EditorOptions, True, True);
end;

destructor TFilePersistInfo.Destroy;
begin
  BreakPoints.Free;
  BookMarks.Free;
  EditorOptions.Free;
  inherited;
end;

function TFilePersistInfo.CreateListItem(Sender: TJvCustomAppStorage;
  const Path: string; Index: Integer): TPersistent;
begin
  if Pos('BreakPoint', Path) > 0 then
    Result := TBreakPoint.Create
  else if Pos('BookMark', Path) > 0 then
    Result := TBookMarkInfo.Create
  else
    Result := nil;
end;

constructor TFilePersistInfo.CreateFromEditor(Editor: IEditor);
Var
  i : integer;
  BookMark : TBookMarkInfo;
  BreakPoint : TBreakPoint;
begin
  Create;
  FileName := Editor.FileName;
  Char := Editor.SynEdit.CaretX;
  Line := Editor.SynEdit.CaretY;
  TopLine := Editor.Synedit.TopLine;
  if Assigned(Editor.SynEdit.Highlighter) then
    Highlighter := Editor.SynEdit.Highlighter.GetLanguageName;
  if Assigned(Editor.SynEdit.Marks) then
    for i := 0 to Editor.SynEdit.Marks.Count - 1 do
      if Editor.SynEdit.Marks[i].IsBookmark then with Editor.SynEdit.Marks[i] do
      begin
        BookMark := TBookMarkInfo.Create;
        BookMark.fChar := Char;
        BookMark.fLine := Line;
        BookMark.fBookmarkNumber := BookmarkNumber;
        BookMarks.Add(BookMark);
      end;
  for i := 0 to Editor.BreakPoints.Count - 1 do begin
    BreakPoint := TBreakPoint.Create;
    with TBreakPoint(Editor.BreakPoints[i]) do begin
      BreakPoint.LineNo := LineNo;
      BreakPoint.Disabled := Disabled;
      BreakPoint.Condition := Condition;
      BreakPoints.Add(BreakPoint);
    end;
  end;
  EditorOptions.Assign(Editor.SynEdit);
end;

{ TPersistFileInfo }

constructor TPersistFileInfo.Create;
begin
  fFileInfoList := TObjectList.Create(True);
end;

class procedure TPersistFileInfo.ReadFromAppStorage(
  AppStorage: TJvCustomAppStorage; Path : String);
Var
  PersistFileInfo : TPersistFileInfo;
  FilePersistInfo : TFilePersistInfo;
  Editor : IEditor;
  i, j : integer;
  FName : string;
begin
  PersistFileInfo := TPersistFileInfo.Create;
  try
    AppStorage.ReadObjectList(Path, PersistFileInfo.fFileInfoList,
      PersistFileInfo.CreateListItem,  True, 'File');
    for i := 0 to PersistFileInfo.fFileInfoList.Count - 1 do begin
      FilePersistInfo := TFilePersistInfo(PersistFileInfo.fFileInfoList[i]);
      if FileExists(FilePersistInfo.FileName) then
        Editor := PyIDEMainForm.DoOpenFile(FilePersistInfo.FileName);
      if Assigned(Editor) then begin
        Editor.SynEdit.TopLine := FilePersistInfo.TopLine;
        Editor.SynEdit.CaretXY := BufferCoord(FilePersistInfo.Char, FilePersistInfo.Line);
        for j := 0 to FilePersistInfo.BreakPoints.Count - 1 do
          with TBreakPoint(FilePersistInfo.BreakPoints[j]) do
            PyControl.SetBreakPoint(FilePersistInfo.FileName,
              LineNo, Disabled, Condition);
        for j := 0 to FilePersistInfo.BookMarks.Count - 1 do
          with TBookMarkInfo(FilePersistInfo.BookMarks[j]) do
            Editor.SynEdit.SetBookMark(BookMarkNumber, Char, Line);
        if FilePersistInfo.Highlighter <> '' then
          Editor.SynEdit.Highlighter := GetHighlighterFromLanguageName(
            FilePersistInfo.Highlighter, CommandsDataModule.Highlighters);
        Editor.SynEdit.Assign(FilePersistInfo.EditorOptions);
      end;
    end;
  finally
    PersistFileInfo.Free;
  end;
  FName := AppStorage.ReadString(Path+'\ActiveEditor', FName);
  if FName <> '' then begin
    Editor := GI_EditorFactory.GetEditorByName(FName);
    if Assigned(Editor) then
      Editor.Activate;
  end;
end;

function TPersistFileInfo.CreateListItem(Sender: TJvCustomAppStorage;
  const Path: string; Index: Integer): TPersistent;
begin
  Result := TFilePersistInfo.Create;
end;

destructor TPersistFileInfo.Destroy;
begin
  fFileInfoList.Free;
  inherited;
end;

class procedure TPersistFileInfo.WriteToAppStorage(
  AppStorage: TJvCustomAppStorage; Path : String);
Var
  PersistFileInfo : TPersistFileInfo;
  ActiveEditor : IEditor;
  FName : string;
begin
  PersistFileInfo := TPersistFileInfo.Create;
  try
    PersistFileInfo.GetFileInfo;
    AppStorage.WriteObjectList(Path, PersistFileInfo.fFileInfoList, 'File');
  finally
    PersistFileInfo.Free;
  end;
  ActiveEditor := PyIDEMainForm.GetActiveEditor;
  if Assigned(ActiveEditor) then
    FName := ActiveEditor.FileName
  else
    FName := '';
  AppStorage.WriteString(Path+'\ActiveEditor', FName);
end;

procedure TPersistFileInfo.GetFileInfo;
var
  i : integer;
  Editor : IEditor;
  FilePersistInfo : TFilePersistInfo;
begin
  for i := 0 to GI_EditorFactory.Count - 1 do begin
    Editor := GI_EditorFactory.Editor[i];
    if Editor.FileName <> '' then begin
      FilePersistInfo := TFilePersistInfo.CreateFromEditor(Editor);
      fFileInfoList.Add(FilePersistInfo)
    end;
  end;
end;

end.

