{-----------------------------------------------------------------------------
 Unit Name: cFilePersist
 Author:    Kiriakos Vlahos
 Date:      09-Mar-2006
 Purpose:   Support class for editor file persistence
 History:
-----------------------------------------------------------------------------}

unit cFilePersist;

interface
Uses
  System.Classes,
  System.SysUtils,
  System.Contnrs,
  Vcl.Controls,
  JvAppStorage,
  uEditAppIntfs,
  dlgSynEditOptions;

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
    TabControlIndex : integer;
    Line, Char, TopLine : integer;
    BreakPoints : TObjectList;
    BookMarks : TObjectList;
    FileName : string;
    Highlighter : string;
    UseCodeFolding: Boolean;
    EditorOptions : TSynEditorOptionsContainer;
    EditorOptions2 : TSynEditorOptionsContainer;
    SecondEditorVisible : Boolean;
    SecondEditorAlign : TAlign;
    SecondEditorSize : integer;
    SecondEditorUseCodeFolding: Boolean;
    ReadOnly : Boolean;
    FoldState : string;
    FoldState2 : string;
  protected
    // IJvAppStorageHandler implementation
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string); virtual;
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string); virtual;
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
    class procedure WriteToAppStorage(AppStorage : TJvCustomAppStorage; Path : string);
    class procedure ReadFromAppStorage(AppStorage : TJvCustomAppStorage; Path : string);
  end;

  TTabsPersistInfo = class (TInterfacedPersistent, IJvAppStorageHandler)
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string); virtual;
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string); virtual;
  end;

Var
  TabsPersistsInfo : TTabsPersistInfo;   // Singleton

implementation

uses
  System.Math,
  SynEditTypes,
  SynEdit,
  SpTBXTabs,
  TB2Item,
  JvJCLUtils,
  dmResources,
  frmPyIDEMain,
  uHighlighterProcs,
  cPyBaseDebugger,
  cPyControl,
  cPyScripterSettings;

{ TFilePersistInfo }

constructor TFilePersistInfo.Create;
begin
  BreakPoints := TObjectList.Create(True);
  BookMarks := TObjectList.Create(True);
  EditorOptions := TSynEditorOptionsContainer.Create(nil);
  EditorOptions2 := TSynEditorOptionsContainer.Create(nil);
  SecondEditorAlign := alRight;
end;

procedure TFilePersistInfo.WriteToAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
var
  IgnoreProperties : TStringList;
begin
   AppStorage.WriteString(BasePath+'\FileName', FileName);
   AppStorage.WriteInteger(BasePath+'\TabControlIndex', TabControlIndex);
   AppStorage.WriteInteger(BasePath+'\Line', Line);
   AppStorage.WriteInteger(BasePath+'\Char', Char);
   AppStorage.WriteInteger(BasePath+'\TopLine', TopLine);
   AppStorage.WriteString(BasePath+'\Highlighter', Highlighter);
   AppStorage.WriteObjectList(BasePath+'\BreakPoints', BreakPoints, 'BreakPoint');
   AppStorage.WriteObjectList(BasePath+'\BookMarks', BookMarks, 'BookMarks');
   AppStorage.WriteBoolean(BasePath+'\UseCodeFolding', UseCodeFolding);
   if UseCodeFolding then AppStorage.WriteString(BasePath+'\FoldState', FoldState);
   AppStorage.WriteBoolean(BasePath+'\ReadOnly', ReadOnly);

   AppStorage.WriteBoolean(BasePath+'\SecondEditorVisible', SecondEditorVisible);
   IgnoreProperties := TStringList.Create;
   try
     IgnoreProperties.AddStrings(['Keystrokes', 'TrackChanges', 'SelectedColor', 'IndentGuides']);
     AppStorage.WritePersistent(BasePath+'\Editor Options', EditorOptions,
       True, IgnoreProperties);
     if SecondEditorVisible then begin
       AppStorage.WriteEnumeration(BasePath+'\Second Editor Align', TypeInfo(TAlign), SecondEditorAlign);
       AppStorage.WriteInteger(BasePath+'Second Editor Size', SecondEditorSize);
       AppStorage.WriteBoolean(BasePath+'\Second Editor UseCodeFolding', SecondEditorUseCodeFolding);
       if SecondEditorUseCodeFolding then
         AppStorage.WriteString(BasePath+'\Second Editor FoldState', FoldState2);
       AppStorage.WritePersistent(BasePath+'\Second Editor Options', EditorOptions2,
         True, IgnoreProperties);
     end;
   finally
     IgnoreProperties.Free;
   end;
end;

procedure TFilePersistInfo.ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
begin
   FileName := AppStorage.ReadString(BasePath+'\FileName');
   TabControlIndex := AppStorage.ReadInteger(BasePath+'\TabControlIndex', 1);
   Line := AppStorage.ReadInteger(BasePath+'\Line');
   Char := AppStorage.ReadInteger(BasePath+'\Char');
   TopLine := AppStorage.ReadInteger(BasePath+'\TopLine');
   Highlighter := AppStorage.ReadString(BasePath+'\Highlighter', Highlighter);
   AppStorage.ReadObjectList(BasePath+'\BreakPoints', BreakPoints, CreateListItem, True, 'BreakPoint');
   AppStorage.ReadObjectList(BasePath+'\BookMarks', BookMarks, CreateListItem, True, 'BookMarks');
   UseCodeFolding := AppStorage.ReadBoolean(BasePath+'\UseCodeFolding', False);
   if UseCodeFolding then
     FoldState := AppStorage.ReadString(BasePath+'\FoldState', '');
   ReadOnly := AppStorage.ReadBoolean(BasePath+'\ReadOnly', False);
   EditorOptions.Assign(cPyScripterSettings.EditorOptions);
   AppStorage.ReadPersistent(BasePath+'\Editor Options', EditorOptions, True, True);
   EditorOptions.Options := EditorOptions.Options + [eoBracketsHighlight, eoAccessibility];

   SecondEditorVisible := AppStorage.ReadBoolean(BasePath+'\SecondEditorVisible', False);
   if SecondEditorVisible then begin
     AppStorage.ReadEnumeration(BasePath+'\Second Editor Align', TypeInfo(TAlign),
       SecondEditorAlign, SecondEditorAlign);
     SecondEditorSize := AppStorage.ReadInteger(BasePath+'Second Editor Size');
     SecondEditorUseCodeFolding := AppStorage.ReadBoolean(BasePath+'\Second Editor UseCodeFolding', False);
     if SecondEditorUseCodeFolding then
       FoldState2 := AppStorage.ReadString(BasePath+'\Second Editor FoldState', '');
     EditorOptions2.Assign(cPyScripterSettings.EditorOptions);
     AppStorage.ReadPersistent(BasePath+'\Second Editor Options', EditorOptions2, True, True);
   end;
end;

destructor TFilePersistInfo.Destroy;
begin
  BreakPoints.Free;
  BookMarks.Free;
  EditorOptions.Free;
  EditorOptions2.Free;
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

  procedure GetFoldInfo(SynEdit: TSynEdit; var UseCodeFolding: Boolean; var FoldState: string);
  Var
    Stream : TMemoryStream;
  begin
    UseCodeFolding := SynEdit.UseCodeFolding;
    if UseCodeFolding then begin
      Stream := TMemoryStream.Create;
      try
        SynEdit.AllFoldRanges.StoreCollapsedState(Stream);
        FoldState := BufToBinStr(Stream.Memory, Stream.Size);
      finally
        Stream.Free;
      end;
    end;
  end;

Var
  i : integer;
  BookMark : TBookMarkInfo;
  BreakPoint : TBreakPoint;
begin
  Create;
  FileName := Editor.FileId;
  TabControlIndex := Editor.TabControlIndex;
  Char := Editor.SynEdit.CaretX;
  Line := Editor.SynEdit.CaretY;
  TopLine := Editor.Synedit.TopLine;
  if Assigned(Editor.SynEdit.Highlighter) then
    Highlighter := Editor.SynEdit.Highlighter.FriendlyLanguageName;
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
  GetFoldInfo(Editor.SynEdit, UseCodeFolding, FoldState);
  ReadOnly := Editor.ReadOnly;

  SecondEditorVisible := Editor.SynEdit2.Visible;
  if SecondEditorVisible then begin
    SecondEditorAlign := Editor.SynEdit2.Align;
    SecondEditorSize := IfThen(SecondEditorAlign = alRight,
      Editor.SynEdit2.Width, Editor.SynEdit2.Height);
    EditorOptions2.Assign(Editor.SynEdit2);
    GetFoldInfo(Editor.SynEdit2, SecondEditorUseCodeFolding, FoldState2);
  end;
end;

{ TPersistFileInfo }

constructor TPersistFileInfo.Create;
begin
  fFileInfoList := TObjectList.Create(True);
end;

class procedure TPersistFileInfo.ReadFromAppStorage(
  AppStorage: TJvCustomAppStorage; Path : string);

  procedure RestoreFoldInfo(SynEdit: TSynEdit; UseCodeFolding: Boolean; FoldState: string);
  Var
    Stream: TMemoryStream;
  begin
    SynEdit.UseCodeFolding := UseCodeFolding;
    if UseCodeFolding and (FoldState <> '') then begin
      Stream := TMemoryStream.Create;
      try
        Stream.Size := FoldState.Length div 2;
        BinStrToBuf(FoldState, Stream.Memory, Stream.Size);
        Stream.Position := 0;
        SynEdit.AllFoldRanges.RestoreCollapsedState(Stream);
        SynEdit.Invalidate;
        SynEdit.InvalidateGutter;
      finally
        Stream.Free;
      end;
    end;
  end;

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
      try
        Editor := PyIDEMainForm.DoOpenFile(FilePersistInfo.FileName, '',
          FilePersistInfo.TabControlIndex);
      except
        Continue; // to the next file
      end;
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
        if FilePersistInfo.Highlighter <> '' then begin
          Editor.SynEdit.Highlighter := ResourcesDataModule.Highlighters.
           HighlighterFromFriendlyName(FilePersistInfo.Highlighter);
          Editor.SynEdit2.Highlighter := Editor.SynEdit.Highlighter;
        end;
        RestoreFoldInfo(Editor.SynEdit, FilePersistInfo.UseCodeFolding, FilePersistInfo.FoldState);
        Editor.SynEdit.Assign(FilePersistInfo.EditorOptions);
        Editor.ReadOnly := FilePersistInfo.ReadOnly;

        if FilePersistInfo.SecondEditorVisible then begin
          RestoreFoldInfo(Editor.SynEdit2, FilePersistInfo.SecondEditorUseCodeFolding,
            FilePersistInfo.FoldState2);
          Editor.SynEdit2.Assign(FilePersistInfo.EditorOptions2);

          if FilePersistInfo.SecondEditorAlign = alRight then begin
            Editor.SplitEditorVertrically;
            Editor.SynEdit2.Width := FilePersistInfo.SecondEditorSize;
          end else begin
            Editor.SplitEditorHorizontally;
            Editor.SynEdit2.Height := FilePersistInfo.SecondEditorSize;
          end;
        end;
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
  AppStorage: TJvCustomAppStorage; Path : string);
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
  ActiveEditor := GI_PyIDEServices.ActiveEditor;
  if Assigned(ActiveEditor) then
    FName := ActiveEditor.FileName
  else
    FName := '';
  AppStorage.WriteString(Path+'\ActiveEditor', FName);
end;

procedure TPersistFileInfo.GetFileInfo;
begin
  GI_EditorFactory.ApplyToEditors(procedure(Editor: IEditor)
  begin
    if Assigned(Editor) and ((Editor.FileName <> '') or (Editor.RemoteFileName <> '')) then begin
      var FilePersistInfo := TFilePersistInfo.CreateFromEditor(Editor);
      fFileInfoList.Add(FilePersistInfo);
      // We need to do it here before we call StoreApplicationData
      GI_PyIDEServices.MRUAddEditor(Editor);
    end;
  end);
end;

{ TTabsPersistInfo }

procedure TTabsPersistInfo.ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
Var
  IsVisible : Boolean;
  Size : integer;
  Alignment : TAlign;
begin
  IsVisible := AppStorage.ReadBoolean(BasePath+'\Visible', False);
  if IsVisible then begin
    Alignment := alRight;
    AppStorage.ReadEnumeration(BasePath+'\Align', TypeInfo(TAlign),
      Alignment, Alignment);
    Size := AppStorage.ReadInteger(BasePath+'\Size', -1);
    PyIDEMainForm.SplitWorkspace(True, Alignment, Size);
  end else
    PyIDEMainForm.SplitWorkspace(False);
end;

procedure TTabsPersistInfo.WriteToAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
begin
  AppStorage.WriteBoolean(BasePath+'\Visible', PyIDEMainForm.TabControl2.Visible);
  with PyIDEMainForm do if TabControl2.Visible then begin
    AppStorage.WriteEnumeration(BasePath+'\Align', TypeInfo(TAlign), TabControl2.Align);
    AppStorage.WriteInteger(BasePath+'\Size',
     IfThen(TabControl2.Align = alRight, TabControl2.Width, TabControl2.Height));
  end;
end;

initialization
  TabsPersistsInfo := TTabsPersistInfo.Create;
finalization
  FreeAndNil(TabsPersistsInfo);
end.
