{-----------------------------------------------------------------------------
 Unit Name: cProjectClasses
 Author:    Kiriakos Vlahos
 Date:      30-Nov-2007
 Purpose:   Data structures for PyScripter projects
 History:
-----------------------------------------------------------------------------}
unit cProjectClasses;

interface

uses
  SysUtils, Classes, Contnrs, JvAppStorage, cPyBaseDebugger;

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
    function GetCaption: WideString; virtual; abstract;
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
    property Caption : WideString read GetCaption;
  end;

  TProjectRootNode = class(TAbstractProjectNode)
  private
    fFileName: string;
    fStoreRelativePaths : Boolean;
    function GetName: WideString;
  protected
    function GetCaption: WideString; override;
  public
    constructor Create; override;
    function HasFile(FileName : WideString) : Boolean;
    property Name : WideString read GetName;
    property FileName : string read fFileName write fFileName;
  published
    property StoreRelativePaths : Boolean read fStoreRelativePaths write fStoreRelativePaths;
  end;

  TProjectFileNode = class;
  TProjectFolderNode = class;

  TProjectFilesNode = class(TAbstractProjectNode)
  private
    function GetFileChild(FileName: WideString): TProjectFileNode;
    function GetFolderChild(FolderName: WideString): TProjectFolderNode;
  {There should be only one FilesNode per project under the Project Root}
  protected
    function GetCaption: WideString; override;
  public
    procedure SortChildren; override;
    procedure ImportDirectory(Directory, Mask : string; Recursive : Boolean);
    property  FileChild[FileName : WideString] : TProjectFileNode read GetFileChild;
    property  FolderChild[FolderName : WideString] : TProjectFolderNode read GetFolderChild;
  end;

  TProjectFolderNode = class(TProjectFilesNode)
  {Like ProjectFilesNode but with a name that can be changed}
  private
    fName: WideString;
  protected
    function GetCaption: WideString; override;
  published
    property Name : WideString read fName write fName;
  end;

  TProjectFileNode = class(TAbstractProjectNode)
  private
    fFileName: WideString;
    function GetName: WideString;
  protected
    function GetCaption: WideString; override;
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
      const BasePath: string); override;
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage;
      const BasePath: string); override;
  public
    property Name : WideString read GetName;
    property FileName : WideString read fFileName write fFileName;
  end;

  TProjectRunConfiguationsNode = class(TAbstractProjectNode)
  protected
    function GetCaption: WideString; override;
  end;

  TProjectRunConfiguationNode = class(TAbstractProjectNode)
  private
    fName: WideString;
    fRunConfig : TRunConfiguration;
    procedure SetRunConfig(const Value: TRunConfiguration);
  protected
    function GetCaption: WideString; override;
  published
    constructor Create; override;
    destructor Destroy; override;
    property Name : WideString read fName write fName;
    property RunConfig : TRunConfiguration read FRunConfig write SetRunConfig;
  end;

Var
  ActiveProject : TProjectRootNode;

implementation

uses
  JclFileUtils, cParameters;

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

constructor TProjectRootNode.Create;
begin
  inherited;
  AddChild(TProjectFilesNode.Create);
  AddChild(TProjectRunConfiguationsNode.Create);
  Modified := False;
end;

function TProjectRootNode.GetCaption: WideString;
begin
  Result := Name;
end;

function TProjectRootNode.GetName: WideString;
begin
  if fFileName <> '' then
    Result := PathRemoveExtension(ExtractFileName(fFileName))
  else
    Result := 'Untitled';
end;

function NodeHasFile(Node: TAbstractProjectNode; Data : Pointer):boolean;
begin
  Result := False;
  if (Node is TProjectFileNode) and
     (WideCompareText(PWideChar(Data), TProjectFileNode(Node).FileName) = 0)
  then
    Result := True;
end;

function TProjectRootNode.HasFile(FileName: WideString): Boolean;
begin
  Result := FirstThat(NodeHasFile, PWideChar(FileName)) <> nil;
end;

{ TProjectFolderNode }

function TProjectFolderNode.GetCaption: WideString;
begin
  Result := fName;
end;

{ TProjectFilesNode }

function TProjectFilesNode.GetCaption: WideString;
begin
  Result := 'Files';
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
    Result := WideCompareText(TProjectFolderNode(Node1).Name, TProjectFolderNode(Node2).Name)
  else if (Node1 is TProjectFileNode) and (Node2 is TProjectFileNode) then
    Result := WideCompareText(TProjectFileNode(Node1).Name, TProjectFileNode(Node2).Name)
  else
    Assert(False, 'Unexpected Child types in TProjectFilesNode');
end;

function TProjectFilesNode.GetFileChild(FileName: WideString): TProjectFileNode;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to fChildren.Count - 1 do begin
    if (fChildren[i] is TProjectFileNode) and
       (WideCompareText(TProjectFileNode(fChildren[i]).fFileName, FileName) = 0) then
     begin
       Result := TProjectFileNode(fChildren[i]);
       break;
     end;
  end;
end;

function TProjectFilesNode.GetFolderChild(FolderName: WideString): TProjectFolderNode;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to fChildren.Count - 1 do begin
    if (fChildren[i] is TProjectFolderNode) and
       (WideCompareText(TProjectFolderNode(fChildren[i]).fName, FolderName) = 0) then
     begin
       Result := TProjectFolderNode(fChildren[i]);
       break;
     end;
  end;
end;

procedure TProjectFilesNode.ImportDirectory(Directory, Mask: string;
  Recursive: Boolean);
Var
  FileList: TStringList;
  i : integer;
  FileNode : TProjectFileNode;
  FolderNode : TProjectFolderNode;
  FolderName : WideString;
begin
  FileList := TStringList.Create;
  try
    AdvBuildFileList(PathAddSeparator(Directory)+Mask, faReadOnly or faArchive,
      FileList, amSuperSetOf, [flFullNames]);
    for i := 0 to FileList.Count - 1 do begin
      if not Assigned(FileChild[FileList[i]]) then begin
        FileNode := TProjectFileNode.Create;
        FileNode.fFileName := FileList[i];
        AddChild(FileNode);
      end;
    end;

    if Recursive then begin
      FileList.Clear;
      AdvBuildFileList(PathAddSeparator(Directory)+'*.*', faDirectory,
        FileList, amSubSetOf, [flFullNames]);
      for i := 0 to FileList.Count - 1 do begin
        FolderName := ExtractFileName(FileList[i]);
        if (FolderName = '.') or (FolderName = '..') then
          continue;
        FolderNode := FolderChild[FolderName];
        if not Assigned(FolderNode) then begin
          FolderNode := TProjectFolderNode.Create;
          FolderNode.fName := FolderName;
          AddChild(FolderNode);
        end;
        FolderNode.ImportDirectory(FileList[i], Mask, Recursive);
        if FolderNode.Children.Count = 0 then
          FolderNode.Free;  //Delete empty nodes
      end;
    end;
  finally
    FileList.Free;
  end;
end;

procedure TProjectFilesNode.SortChildren;
begin
  fChildren.Sort(CompareFolderChildren);
end;

{ TProjectRunConfiguationsNode }

function TProjectRunConfiguationsNode.GetCaption: WideString;
begin
  Result := 'Run Configurations';
end;

{ TProjectFileNode }

function TProjectFileNode.GetCaption: WideString;
begin
  Result := Name;
end;

function TProjectFileNode.GetName: WideString;
begin
  if fFileName <> '' then
    Result := PathRemoveExtension(ExtractFileName(Parameters.ReplaceInText(fFileName)))
  else
    Result := 'Untitled';
end;

procedure TProjectFileNode.ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
begin
  inherited;
  FileName := AppStorage.ReadWideString(BasePath + '\FileName');
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
    fFileName := Parameters.ReplaceInText(fFileName);

  AppStorage.WriteWideString(BasePath + '\FileName', fFileName);
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

function TProjectRunConfiguationNode.GetCaption: WideString;
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
