{-----------------------------------------------------------------------------
 Unit Name: cProjectClasses
 Author:    Kiriakos Vlahos
 Date:      30-Nov-2007
 Purpose:   Data structures for PyScripter projects
 History:

 16-Jun-2008 Roman Krivoruchko
    Project property ExtraPythonPath added with persistence
-----------------------------------------------------------------------------}
unit cProjectClasses;

interface

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Contnrs,
  JvAppStorage,
  cPySupportTypes;

type
  TAbstractProjectNode = class;
  TAbstractProjectNodeClass = class of TAbstractProjectNode;

  TProjectNodeAction = function (Node: TAbstractProjectNode; Data : Pointer):boolean;

  TAbstractProjectNode = class(TInterfacedPersistent, IJvAppStorageHandler, IJvAppStoragePublishedProps)
  private
    fChildren : TObjectList;
    fParent : TAbstractProjectNode;
    fModified : Boolean;
    function GetRootNode: TAbstractProjectNode;
    function GetModified: Boolean;
    procedure SetModified(const Value: Boolean);
    procedure SetParent(const Value: TAbstractProjectNode);
  protected
    function GetCaption: string; virtual; abstract;
    // IJvAppStorageHandler implementation
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
      const BasePath: string); virtual;
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage;
      const BasePath: string); virtual;
    function CreateListItem(Sender: TJvCustomAppStorage; const Path: string;
      Index: Integer): TPersistent;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ClearChildren;
    procedure AddChild(Child:TAbstractProjectNode);
    procedure RemoveChild(Child:TAbstractProjectNode);
    procedure ForEach(Proc: TProjectNodeAction; Data:Pointer = nil);
    function FirstThat(Proc: TProjectNodeAction; Data:Pointer = nil): TAbstractProjectNode;
    procedure SortChildren; virtual;

    property Parent : TAbstractProjectNode read fParent write SetParent;
    property Children : TObjectList read fChildren;
    property RootNode : TAbstractProjectNode read GetRootNode;
    property Modified : Boolean read GetModified write SetModified;
    property Caption : string read GetCaption;
  end;

  TProjectRootNode = class(TAbstractProjectNode)
  private
    fFileName: string;
    fStoreRelativePaths : Boolean;
    fShowFileExtensions : Boolean;
    fExtraPythonPath: TStrings;
    function GetName: string;
  protected
    function GetCaption: string; override;
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
      const BasePath: string); override;
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage;
      const BasePath: string); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function HasFile(const FileName : string) : Boolean;
    procedure AppendExtraPaths;
    procedure RemoveExtraPaths;
    property Name : string read GetName;
    property FileName : string read fFileName write fFileName;
  published
    property StoreRelativePaths : Boolean read fStoreRelativePaths write fStoreRelativePaths;
    property ShowFileExtensions : Boolean read fShowFileExtensions write fShowFileExtensions;
    property ExtraPythonPath: TStrings read fExtraPythonPath;
  end;

  TProjectFileNode = class;
  TProjectFolderNode = class;

  TProjectFilesNode = class(TAbstractProjectNode)
  private
    function GetFileChild(FileName: string): TProjectFileNode;
    function GetFolderChild(FolderName: string): TProjectFolderNode;
  {There should be only one FilesNode per project under the Project Root}
  protected
    function GetCaption: string; override;
  public
    procedure SortChildren; override;
    procedure ImportDirectory(const Directory, Masks : string; Recursive : Boolean);
    property  FileChild[FileName : string] : TProjectFileNode read GetFileChild;
    property  FolderChild[FolderName : string] : TProjectFolderNode read GetFolderChild;
  end;

  TProjectFolderNode = class(TProjectFilesNode)
  {Like ProjectFilesNode but with a name that can be changed}
  private
    fName: string;
  protected
    function GetCaption: string; override;
  published
    property Name : string read fName write fName;
  end;

  TProjectFileNode = class(TAbstractProjectNode)
  private
    fFileName: string;
    function GetName: string;
  protected
    function GetCaption: string; override;
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
      const BasePath: string); override;
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage;
      const BasePath: string); override;
  public
    property Name : string read GetName;
    property FileName : string read fFileName write fFileName;
  end;

  TProjectRunConfiguationsNode = class(TAbstractProjectNode)
  protected
    function GetCaption: string; override;
  end;

  TProjectRunConfiguationNode = class(TAbstractProjectNode)
  private
    fName: string;
    fRunConfig : TRunConfiguration;
    procedure SetRunConfig(const Value: TRunConfiguration);
  protected
    function GetCaption: string; override;
  published
    constructor Create; override;
    destructor Destroy; override;
    property Name : string read fName write fName;
    property RunConfig : TRunConfiguration read FRunConfig write SetRunConfig;
  end;

Var
  ActiveProject : TProjectRootNode;

implementation

uses
  System.IOUtils,
  JvGnuGetText,
  uCommonFunctions,
  cPyControl,
  uEditAppIntfs;

{ TAbstractProjectNode }

procedure TAbstractProjectNode.AddChild(Child: TAbstractProjectNode);
begin
  fChildren.Add(Child);
  Child.fParent := Self;
  if fChildren.Count > 1 then SortChildren;
  fModified := True;
end;

procedure TAbstractProjectNode.ClearChildren;
begin
  while fChildren.Count > 0 do
    fChildren.Last.Free;
end;

constructor TAbstractProjectNode.Create;
begin
  inherited;
  fChildren := TObjectList.Create(False);
end;

function TAbstractProjectNode.CreateListItem(Sender: TJvCustomAppStorage;
  const Path: string; Index: Integer): TPersistent;

  procedure RaiseError;
  begin
    raise Exception.Create('Error in reading project data');
  end;

Var
  ClassName : string;
  NodeClass : TAbstractProjectNodeClass;
begin
  Result := nil;
  ClassName := Sender.ReadString(Path + '\ClassName');
  if ClassName = '' then
    RaiseError;
  try
    NodeClass := TAbstractProjectNodeClass(GetClass(ClassName));
    if not Assigned(NodeClass) then
      RaiseError;
    Result := NodeClass.Create;
  except
      RaiseError;
  end;
end;

destructor TAbstractProjectNode.Destroy;
begin
  SetParent(nil);
  ClearChildren;
  fChildren.Free;
  inherited;
end;

function TAbstractProjectNode.FirstThat(Proc: TProjectNodeAction;
  Data: Pointer): TAbstractProjectNode;
var
  i : integer;
begin
  Result := nil;
  if Proc(Self, Data) then
    Result := Self
  else
    for i := 0 to fChildren.Count-1 do begin
      Result := TAbstractProjectNode(fChildren[i]).FirstThat(Proc, Data);
      if Result <> nil then exit;
    end;
end;

procedure TAbstractProjectNode.ForEach(Proc: TProjectNodeAction; Data: Pointer);
var
  i : integer;
begin
  Proc(Self, Data);
  for i := 0 to fChildren.Count-1 do
    TAbstractProjectNode(Children[i]).ForEach(Proc, Data);
end;

function CheckModified(Node: TAbstractProjectNode; Data : Pointer):boolean;
begin
  Result := Node.fModified;
end;

function TAbstractProjectNode.GetModified: Boolean;
var
  Node : TAbstractProjectNode;
begin
  Result := fModified;
  if not Result then begin
    Node := FirstThat(CheckModified);
    Result := Assigned(Node);
  end;
end;

function TAbstractProjectNode.GetRootNode: TAbstractProjectNode;
begin
  if fParent = nil then
    Result := Self
  else
    Result := fParent.RootNode;
end;

procedure TAbstractProjectNode.ReadFromAppStorage(
  AppStorage: TJvCustomAppStorage; const BasePath: string);
var
  i : integer;
begin
  if AppStorage.PathExists(BasePath+'\ChildNodes') then begin
    ClearChildren;
    AppStorage.ReadObjectList(BasePath+'\ChildNodes', fChildren, CreateListItem, True, 'Node');
    for i := 0 to fChildren.Count - 1 do
      TAbstractProjectNode(fChildren[i]).fParent := Self;
    if fChildren.Count > 1 then SortChildren;
  end;
end;

procedure TAbstractProjectNode.RemoveChild(Child: TAbstractProjectNode);
begin
  if Child = fChildren.Last then
    fChildren.Delete(fChildren.Count - 1)  //speed up when destroying
  else
    fChildren.Remove(Child);
  Child.fParent := nil;
  fModified := True;
end;

function ReSetModified(Node: TAbstractProjectNode; Data : Pointer):boolean;
begin
  Node.fModified := False;
  Result := True;
end;

procedure TAbstractProjectNode.SetModified(const Value: Boolean);
begin
  fModified := Value;
  // Only apply to children if Modified is False
  if not Value then
    ForEach(ReSetModified);
end;

procedure TAbstractProjectNode.SetParent(const Value: TAbstractProjectNode);
begin
  if fParent <> Value then
  begin
    if  fParent <> nil then fParent.RemoveChild(Self);
    if Value <> nil then Value.AddChild(Self);
  end;
end;

procedure TAbstractProjectNode.SortChildren;
begin
  // Do nothing by default
end;

procedure TAbstractProjectNode.WriteToAppStorage(
  AppStorage: TJvCustomAppStorage; const BasePath: string);
begin
  AppStorage.WriteString(BasePath+'\ClassName', Self.ClassName);
  if fChildren.Count > 0 then
    AppStorage.WriteObjectList(BasePath+'\ChildNodes', fChildren, 'Node');
end;

{ TProjectRootNode }

procedure TProjectRootNode.AppendExtraPaths;
var
  i: Integer;
begin
  if not (Assigned(PyControl) and Assigned(PyControl.ActiveInterpreter)) then Exit;

  for i := 0 to fExtraPythonPath.Count-1 do begin
    PyControl.InternalInterpreter.SysPathAdd(fExtraPythonPath[i]);
    if PyControl.ActiveInterpreter <> PyControl.InternalInterpreter then
      PyControl.ActiveInterpreter.SysPathAdd(fExtraPythonPath[i]);
  end;
end;

constructor TProjectRootNode.Create;
begin
  inherited;
  AddChild(TProjectFilesNode.Create);
  AddChild(TProjectRunConfiguationsNode.Create);
  Modified := False;
  fExtraPythonPath := TStringList.Create;
  fStoreRelativePaths := True;
end;

destructor TProjectRootNode.Destroy;
begin
  RemoveExtraPaths;
  FreeAndNil(fExtraPythonPath);
  inherited;
end;

function TProjectRootNode.GetCaption: string;
begin
  Result := Name;
end;

function TProjectRootNode.GetName: string;
begin
  if fFileName <> '' then
    Result := ChangeFileExt(TPath.GetFileName(fFileName), '')
  else
    Result := _('Untitled');
end;

function NodeHasFile(Node: TAbstractProjectNode; Data : Pointer):boolean;
begin
  Result := False;
  if (Node is TProjectFileNode) and
     AnsiSameText(PWideChar(Data), TProjectFileNode(Node).FileName)
  then
    Result := True;
end;

function TProjectRootNode.HasFile(const FileName: string): Boolean;
begin
  Result := FirstThat(NodeHasFile, PWideChar(FileName)) <> nil;
end;

procedure TProjectRootNode.ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
begin
  inherited;
  RemoveExtraPaths;
  fExtraPythonPath.Clear;
  AppStorage.ReadStringList(BasePath+'\ExtraPythonPath', fExtraPythonPath);
  AppendExtraPaths;
end;

procedure TProjectRootNode.RemoveExtraPaths;
var
  i: Integer;
begin
  if not (Assigned(PyControl) and Assigned(PyControl.ActiveInterpreter)) then Exit;

  for i := 0 to fExtraPythonPath.Count-1 do begin
    PyControl.InternalInterpreter.SysPathRemove(fExtraPythonPath[i]);
    if PyControl.ActiveInterpreter <> PyControl.InternalInterpreter then
      PyControl.ActiveInterpreter.SysPathRemove(fExtraPythonPath[i]);
  end;
end;

procedure TProjectRootNode.WriteToAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
begin
  inherited;
  AppStorage.WriteStringList(BasePath+'\ExtraPythonPath', fExtraPythonPath);
end;

{ TProjectFolderNode }

function TProjectFolderNode.GetCaption: string;
begin
  Result := fName;
end;

{ TProjectFilesNode }

function TProjectFilesNode.GetCaption: string;
begin
  Result := _('Files');
end;

function CompareFolderChildren(P1, P2: Pointer): integer;
Var
  Node1, Node2 : TAbstractProjectNode;
begin
  Result := 0;  // to keep compiler happy...
  Node1 := TAbstractProjectNode(P1);
  Node2 := TAbstractProjectNode(P2);
  if (Node1 is TProjectFolderNode) and (Node2 is TProjectFileNode) then
    Result := -1
  else if (Node1 is TProjectFileNode) and (Node2 is TProjectFolderNode) then
    Result := 1
  else if (Node1 is TProjectFolderNode) and (Node2 is TProjectFolderNode) then
    Result := AnsiCompareText(TProjectFolderNode(Node1).Name, TProjectFolderNode(Node2).Name)
  else if (Node1 is TProjectFileNode) and (Node2 is TProjectFileNode) then
    Result := AnsiCompareText(TProjectFileNode(Node1).Name, TProjectFileNode(Node2).Name)
  else
    Assert(False, 'Unexpected Child types in TProjectFilesNode');
end;

function TProjectFilesNode.GetFileChild(FileName: string): TProjectFileNode;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to fChildren.Count - 1 do begin
    if (fChildren[i] is TProjectFileNode) and
       (AnsiCompareText(TProjectFileNode(fChildren[i]).fFileName, FileName) = 0) then
     begin
       Result := TProjectFileNode(fChildren[i]);
       break;
     end;
  end;
end;

function TProjectFilesNode.GetFolderChild(FolderName: string): TProjectFolderNode;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to fChildren.Count - 1 do begin
    if (fChildren[i] is TProjectFolderNode) and
       (AnsiCompareText(TProjectFolderNode(fChildren[i]).fName, FolderName) = 0) then
     begin
       Result := TProjectFolderNode(fChildren[i]);
       break;
     end;
  end;
end;

procedure TProjectFilesNode.ImportDirectory(const Directory, Masks: string;
  Recursive: Boolean);
Var
  FileList: TStringList;
  i : integer;
  FileNode : TProjectFileNode;
  FolderNode : TProjectFolderNode;
  FolderName : string;
  FileName : string;
begin
  FolderName := TPath.GetFileName(Directory);
  if (FolderName = '.') or (FolderName = '..') then
    Exit;

  FolderNode := FolderChild[FolderName];
  if not Assigned(FolderNode) then begin
    FolderNode := TProjectFolderNode.Create;
    FolderNode.fName := FolderName;
    AddChild(FolderNode);
  end;

  FileList := TStringList.Create;
  try
    GetFilesInPaths(Directory, Masks, FileList, False);
    for i := 0 to FileList.Count - 1 do begin
      FileName := FileList[i];
      if not Assigned(FileChild[FileName]) then begin
        FileNode := TProjectFileNode.Create;
        FileNode.fFileName := FileName;
        FolderNode.AddChild(FileNode);
      end;
    end;

    if Recursive then begin
      FileList.Clear;
      GetDirectoriesInPaths(Directory, '*.*', FileList, False);
      for i := 0 to FileList.Count - 1 do begin
        FolderName := FileList[i];
        if (FolderName = '.') or (FolderName = '..') then
          continue;
        FolderNode.ImportDirectory(FolderName, Masks, Recursive);
      end;
    end;

  finally
    FileList.Free;
  end;

  if FolderNode.Children.Count = 0 then
    FolderNode.Free;  //Delete empty nodes
end;

procedure TProjectFilesNode.SortChildren;
begin
  fChildren.Sort(CompareFolderChildren);
end;

{ TProjectRunConfiguationsNode }

function TProjectRunConfiguationsNode.GetCaption: string;
begin
  Result := _('Run Configurations');
end;

{ TProjectFileNode }

function TProjectFileNode.GetCaption: string;
begin
  Result := Name;
end;

function TProjectFileNode.GetName: string;
begin
  if fFileName <> '' then  begin
    Result := TPath.GetFileName(GI_PyIDEServices.ReplaceParams(fFileName));
    if not ActiveProject.ShowFileExtensions then
      Result := ChangeFileExt(Result, '');
  end else
    Result := _('Untitled');
end;

procedure TProjectFileNode.ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
begin
  inherited;
  FileName := AppStorage.ReadString(BasePath + '\FileName');
end;

procedure TProjectFileNode.WriteToAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
Var
  BaseDir : string;
begin
  inherited;

  BaseDir := '';
  if ActiveProject.StoreRelativePaths then begin
    BaseDir := ExtractFilePath(ActiveProject.fFileName);

    if BaseDir <> '' then begin
      if Pos(BaseDir, fFileName) = 1 then begin
        Delete(fFileName, 1, Length(BaseDir));
        fFileName := '$[Project-Path]'+fFileName;
      end;
    end;
  end else
    fFileName := GI_PyIDEServices.ReplaceParams(fFileName);

  AppStorage.WriteString(BasePath + '\FileName', fFileName);
end;

{ TProjectRunConfiguationNode }

constructor TProjectRunConfiguationNode.Create;
begin
  inherited;
  fRunConfig := TRunConfiguration.Create;
end;

destructor TProjectRunConfiguationNode.Destroy;
begin
  fRunConfig.Free;
  inherited;
end;

function TProjectRunConfiguationNode.GetCaption: string;
begin
  Result := Name;
end;

procedure TProjectRunConfiguationNode.SetRunConfig(const Value: TRunConfiguration);
begin
  FRunConfig.Assign(Value);
end;

initialization
  RegisterClasses([TProjectRootNode, TProjectFolderNode, TProjectFilesNode,
                   TProjectFileNode, TProjectRunConfiguationsNode, TProjectRunConfiguationNode]);
  ActiveProject := TProjectRootNode.Create;
finalization
  FreeAndNil(ActiveProject);
end.
