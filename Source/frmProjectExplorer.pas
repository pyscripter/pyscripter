{-----------------------------------------------------------------------------
 Unit Name: frmProjectExplorer
 Author:    Kiriakos Vlahos
 Date:      02-Dec-2007
 Purpose:   PyScripter project explorer
 History:

 16-Jun-2008 Roman Krivoruchko
 - Project 'Extra Python path' editing and appliyng upon project opening
-----------------------------------------------------------------------------}
unit frmProjectExplorer;

interface

uses
  WinApi.Windows,
  WinApi.Messages,
  WinApi.ActiveX,
  System.UITypes,
  System.SysUtils,
  System.Contnrs,
  System.Variants,
  System.Classes,
  System.Actions,
  System.ImageList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  Vcl.BaseImageCollection,
  SVGIconImageCollection,
  TB2Item,
  SpTBXItem,
  ActnList,
  TB2Dock,
  TB2Toolbar,
  SpTBXSkins,
  SpTBXControls,
  JvComponentBase,
  JvDockControlForm,
  MPCommonObjects,
  VirtualTrees.Types,
  VirtualTrees.BaseAncestorVCL,
  VirtualTrees.AncestorVCL,
  VirtualTrees.BaseTree,
  VirtualTrees,
  frmIDEDockWin,
  cProjectClasses;

type
  TProjectExplorerWindow = class(TIDEDockWindow)
    Panel1: TPanel;
    ExplorerTree: TVirtualStringTree;
    ProjectMainPopUpMenu: TSpTBXPopupMenu;
    ProjectFolderPopupMenu: TSpTBXPopupMenu;
    mnAddFiles: TSpTBXItem;
    SpTBXItem6: TSpTBXItem;
    mnRename: TSpTBXItem;
    mnRemove: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    ProjectFilePopupMenu: TSpTBXPopupMenu;
    mnFileRemove: TSpTBXItem;
    SpTBXSeparatorItem3: TSpTBXSeparatorItem;
    mnFileEdit: TSpTBXItem;
    mnAddActiveFile: TSpTBXItem;
    mnImportDir: TSpTBXItem;
    ProjectRunSettingsPopupMenu: TSpTBXPopupMenu;
    ProjectRunConfigPopupMenu: TSpTBXPopupMenu;
    mnAddRunConfig: TSpTBXItem;
    mnRemoveRunConfig: TSpTBXItem;
    SpTBXSeparatorItem4: TSpTBXSeparatorItem;
    mnRenameRunConfig: TSpTBXItem;
    SpTBXSeparatorItem5: TSpTBXSeparatorItem;
    mnEditRunConfig: TSpTBXItem;
    mnFileProperties: TSpTBXItem;
    SpTBXSeparatorItem6: TSpTBXSeparatorItem;
    SpTBXSeparatorItem7: TSpTBXSeparatorItem;
    mnExternalRun: TSpTBXItem;
    SpTBXDock1: TSpTBXDock;
    SpTBXToolbar1: TSpTBXToolbar;
    tbiProjectNew: TSpTBXItem;
    tbiProjectOpen: TSpTBXItem;
    tbiProjectSave: TSpTBXItem;
    SpTBXSeparatorItem8: TSpTBXSeparatorItem;
    tbiRunLast: TSpTBXItem;
    tbiDebugLast: TSpTBXItem;
    tbiRunLastExternal: TSpTBXItem;
    mnRun: TSpTBXItem;
    mnDebug: TSpTBXItem;
    SpTBXSeparatorItem9: TSpTBXSeparatorItem;
    mnStoreRelativePaths: TSpTBXItem;
    SpTBXSeparatorItem10: TSpTBXSeparatorItem;
    mnCollapseAll: TSpTBXItem;
    mnExpandAll: TSpTBXItem;
    SpTBXSeparatorItem11: TSpTBXSeparatorItem;
    tbiCollapseAll: TSpTBXItem;
    tbiExpandAll: TSpTBXItem;
    mnShowFileExt: TSpTBXItem;
    ImmutableProjectActionList: TActionList;
    ProjectActionList: TActionList;
    actProjectDebug: TAction;
    actProjectExternalRun: TAction;
    actProjectRun: TAction;
    actProjectFileProperties: TAction;
    actProjectEditRunConfig: TAction;
    actProjectAddRunConfig: TAction;
    actProjectImportDirectory: TAction;
    actProjectAddActiveFile: TAction;
    actProjectFileEdit: TAction;
    actProjectRename: TAction;
    actProjectRemove: TAction;
    actProjectAddFolder: TAction;
    actProjectAddFiles: TAction;
    actProjectCollapseAll: TAction;
    actProjectExpandAll: TAction;
    actProjectShowFileExtensions: TAction;
    actProjectRelativePaths: TAction;
    actProjectSaveAs: TAction;
    actProjectSave: TAction;
    actProjectOpen: TAction;
    actProjectNew: TAction;
    actProjectExtraPythonPath: TAction;
    SpTBXSeparatorItem12: TSpTBXSeparatorItem;
    mnProjectNew: TSpTBXItem;
    mnProjectOpen: TSpTBXItem;
    mnProjectSave: TSpTBXItem;
    mnProjectSaveAs: TSpTBXItem;
    mnExtraPythonPath: TSpTBXItem;
    actProjectAddRemoteFile: TAction;
    SpTBXItem1: TSpTBXItem;
    vilProjects: TVirtualImageList;
    vilImages: TVirtualImageList;
    icProjects: TSVGIconImageCollection;
    procedure FormCreate(Sender: TObject);
    procedure ExplorerTreeInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure ExplorerTreeInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ExplorerTreeContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure ExplorerTreeEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure actProjectAddFolderExecute(Sender: TObject);
    procedure ExplorerTreeNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: string);
    procedure actProjectAddFilesExecute(Sender: TObject);
    procedure actProjectRenameExecute(Sender: TObject);
    procedure actProjectSaveExecute(Sender: TObject);
    procedure actProjectSaveAsExecute(Sender: TObject);
    procedure actProjectNewExecute(Sender: TObject);
    procedure actProjectOpenExecute(Sender: TObject);
    procedure actProjectRemoveExecute(Sender: TObject);
    procedure actProjectFileEditExecute(Sender: TObject);
    procedure ExplorerTreeKeyPress(Sender: TObject; var Key: Char);
    procedure actProjectAddActiveFileExecute(Sender: TObject);
    procedure actProjectImportDirectoryExecute(Sender: TObject);
    procedure actProjectAddRunConfigExecute(Sender: TObject);
    procedure actProjectFilePropertiesExecute(Sender: TObject);
    procedure ExplorerTreeGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: string);
    procedure actProjectEditRunConfigExecute(Sender: TObject);
    procedure actProjectExternalRunExecute(Sender: TObject);
    procedure actProjectRunExecute(Sender: TObject);
    procedure actProjectDebugExecute(Sender: TObject);
    procedure actProjectRelativePathsExecute(Sender: TObject);
    procedure ProjectMainPopUpMenuPopup(Sender: TObject);
    procedure ExplorerTreeDragAllowed(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure ExplorerTreeDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
      var Effect: Integer; var Accept: Boolean);
    procedure ExplorerTreeDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure actProjectExpandAllExecute(Sender: TObject);
    procedure actProjectCollapseAllExecute(Sender: TObject);
    procedure actProjectShowFileExtensionsExecute(Sender: TObject);
    procedure actProjectExtraPythonPathExecute(Sender: TObject);
    procedure ExplorerTreeIncrementalSearch(Sender: TBaseVirtualTree;
      Node: PVirtualNode; const SearchText: string; var Result: Integer);
    procedure ExplorerTreeGetCellText(Sender: TCustomVirtualStringTree;
      var E: TVSTGetCellTextEventArgs);
    procedure actProjectAddRemoteFileExecute(Sender: TObject);
    procedure ExplorerTreeGetImageIndexEx(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex;
      var ImageList: TCustomImageList);
    procedure ExplorerTreeNodeDblClick(Sender: TBaseVirtualTree; const HitInfo:
        THitInfo);
  private
    procedure ProjectFileNodeEdit(Node: PVirtualNode);
    procedure UpdatePopupActions(Node : PVirtualNode);
  protected
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  public
    { Public declarations }
    FileImageList: TStringList;
    procedure DoOpenProjectFile(FileName : string);
    function DoSave: boolean;
    function DoSaveFile: boolean;
    function DoSaveAs: boolean;
    function CanClose: boolean;
  end;

resourcestring
  ProjectFilter = 'PyScripter project files (*.%s)|*.%0:s';

const
  ProjectDefaultExtension = 'psproj';
var
  ProjectExplorerWindow: TProjectExplorerWindow;

implementation

uses
  System.IOUtils,
  Vcl.Themes,
  MPDataObject,
  JclShell,
  JclFileUtils,
  JvAppIniStorage,
  JvAppStorage,
  JvJVCLUtils,
  JvGnugettext,
  StringResources,
  dmResources,
  frmPyIDEMain,
  uCommonFunctions,
  dlgImportDirectory,
  dlgRunConfiguration,
  dlgDirectoryList,
  uEditAppIntfs,
  uHighlighterProcs,
  cPyBaseDebugger,
  cPyScripterSettings,
  cPyControl,
  cSSHSupport,
  dlgRemoteFile;

{$R *.dfm}

Type
  PNodeDataRec = ^TNodeDataRec;
  TNodeDataRec = record
    ProjectNode : TAbstractProjectNode;
  end;


procedure TProjectExplorerWindow.actProjectAddRemoteFileExecute(Sender: TObject);
Var
  Data : PNodeDataRec;
  Node: PVirtualNode;
  ProjectNode : TProjectFileNode;
  FileName, FName, Server : string;
begin
  Node := ExplorerTree.GetFirstSelected;
  if not Assigned(Node) then Exit;

  if not ExecuteRemoteFileDialog(FileName, Server, rfdAdd) then Exit;
  FName := TSSHFileName.Format(Server, FileName);

  if not ActiveProject.HasFile(FName) then
  begin
    Data := ExplorerTree.GetNodeData(Node);
    if Data.ProjectNode is TProjectFilesNode then
    begin
      ProjectNode := TProjectFileNode.Create;
      ProjectNode.FileName := FName;
      Data.ProjectNode.AddChild(ProjectNode);
    end;
    ExplorerTree.ReinitNode(Node, True);
  end;
end;

procedure TProjectExplorerWindow.actProjectAddActiveFileExecute(
  Sender: TObject);
Var
  Editor : IEditor;
  Data : PNodeDataRec;
  Node: PVirtualNode;
  ProjectNode : TProjectFileNode;
  FName : string;
begin
  Node := ExplorerTree.GetFirstSelected;
  Editor := GI_PyIDEServices.ActiveEditor;
  if not Assigned(Node) or not Assigned(Editor) then Exit;

  if Editor.FileName <> '' then
    FName := Editor.FileName
  else if Editor.RemoteFileName <> '' then
    FName := TSSHFileName.Format(Editor.SSHServer, Editor.RemoteFileName)
  else
    Exit;

  if not ActiveProject.HasFile(FName) then
  begin
    Data := ExplorerTree.GetNodeData(Node);
    if Data.ProjectNode is TProjectFilesNode then
    begin
      ProjectNode := TProjectFileNode.Create;
      ProjectNode.FileName := FName;
      Data.ProjectNode.AddChild(ProjectNode);
    end;
    ExplorerTree.ReinitNode(Node, True);
  end;
end;

procedure TProjectExplorerWindow.actProjectAddFilesExecute(Sender: TObject);
Var
  i : integer;
  Editor : IEditor;
  Data : PNodeDataRec;
  Node: PVirtualNode;
  ProjectNode : TProjectFileNode;
begin
  Node := ExplorerTree.GetFirstSelected;
  if Assigned(Node) then begin
    Data := ExplorerTree.GetNodeData(Node);
    if Data.ProjectNode is TProjectFilesNode then
    begin
      Application.ProcessMessages;  // to update the display until the dialog appears
      with ResourcesDataModule.dlgFileOpen do begin
        Title := _(SAddFilesToProject);
        FileName := '';
        Filter := ResourcesDataModule.Highlighters.FileFilters + _(SFilterAllFiles);
        Editor := GI_PyIDEServices.ActiveEditor;
        if Assigned(Editor) and (Editor.FileName <> '') and
          (TPath.GetDirectoryName(Editor.FileName) <> '')
        then
          InitialDir := TPath.GetDirectoryName(Editor.FileName);

        Options := Options + [ofAllowMultiSelect];
        if Execute then begin
          for i := 0 to Files.Count - 1 do begin
            if not ActiveProject.HasFile(Files[i]) then begin
              ProjectNode := TProjectFileNode.Create;
              ProjectNode.FileName := Files[i];
              Data.ProjectNode.AddChild(ProjectNode);
            end;
          end;
          ExplorerTree.ReinitNode(Node, True);
        end;
        Options := Options - [ofAllowMultiSelect];
      end;
    end;
  end;
end;

procedure TProjectExplorerWindow.actProjectAddFolderExecute(Sender: TObject);
var
  Data : PNodeDataRec;
  Node: PVirtualNode;
  ProjectNode : TProjectFolderNode;
begin
  Node := ExplorerTree.GetFirstSelected;
  if Assigned(Node) then begin
    Data := ExplorerTree.GetNodeData(Node);
    if Data.ProjectNode is TProjectFilesNode then
    begin
      ProjectNode := TProjectFolderNode.Create;
      ProjectNode.Name := 'New Folder';
      Data.ProjectNode.AddChild(ProjectNode);
      ExplorerTree.ReinitNode(Node, True);
      Node := ExplorerTree.GetFirstChild(Node);
      while Assigned(Node) do begin
        Data := ExplorerTree.GetNodeData(Node);
        if Data.ProjectNode = ProjectNode then begin
          ExplorerTree.EditNode(Node, -1);
          ExplorerTree.Selected[Node] := True;
          break;
        end;
        Node := Node.NextSibling;
      end;
    end;
  end;
end;

procedure TProjectExplorerWindow.actProjectAddRunConfigExecute(Sender: TObject);
var
  Data : PNodeDataRec;
  Node: PVirtualNode;
  ProjectNode : TProjectRunConfiguationNode;
begin
  Node := ExplorerTree.GetFirstSelected;
  if Assigned(Node) then begin
    Data := ExplorerTree.GetNodeData(Node);
    if Data.ProjectNode is TProjectRunConfiguationsNode then
    begin
      ProjectNode := TProjectRunConfiguationNode.Create;
      ProjectNode.Name := _('Untitled');
      Data.ProjectNode.AddChild(ProjectNode);
      ExplorerTree.ReinitNode(Node, True);
      Node := ExplorerTree.GetFirstChild(Node);
      while Assigned(Node) do begin
        Data := ExplorerTree.GetNodeData(Node);
        if Data.ProjectNode = ProjectNode then begin
          ExplorerTree.EditNode(Node, -1);
          ExplorerTree.Selected[Node] := True;
          break;
        end;
        Node := Node.NextSibling;
      end;
    end;
  end;
end;

procedure TProjectExplorerWindow.actProjectCollapseAllExecute(Sender: TObject);
Var
  Node : PVirtualNode;
begin
  Node := ExplorerTree.RootNode.FirstChild;
  if Assigned(Node) then
    Node := Node.FirstChild;  // Files Node
  if Assigned(Node) then
    ExplorerTree.FullCollapse(Node);
end;

procedure TProjectExplorerWindow.actProjectDebugExecute(Sender: TObject);
var
  Data : PNodeDataRec;
  Node: PVirtualNode;
begin
  Node := ExplorerTree.GetFirstSelected;
  if Assigned(Node) then begin
    Data := ExplorerTree.GetNodeData(Node);
    if Data.ProjectNode is TProjectRunConfiguationNode then
      PyControl.Debug(TProjectRunConfiguationNode(Data.ProjectNode).RunConfig);
  end;
end;

procedure TProjectExplorerWindow.actProjectEditRunConfigExecute(Sender: TObject);
var
  Data : PNodeDataRec;
  Node: PVirtualNode;
begin
  Node := ExplorerTree.GetFirstSelected;
  if Assigned(Node) then begin
    Data := ExplorerTree.GetNodeData(Node);
    if Data.ProjectNode is TProjectRunConfiguationNode then
      if EditRunConfiguration(TProjectRunConfiguationNode(Data.ProjectNode).RunConfig) then
        Data.ProjectNode.Modified:= True;
  end;
end;

procedure TProjectExplorerWindow.actProjectExpandAllExecute(Sender: TObject);
begin
  ExplorerTree.FullExpand();
end;

procedure TProjectExplorerWindow.actProjectExternalRunExecute(Sender: TObject);
var
  Data : PNodeDataRec;
  Node: PVirtualNode;
begin
  Node := ExplorerTree.GetFirstSelected;
  if Assigned(Node) then begin
    Data := ExplorerTree.GetNodeData(Node);
    if Data.ProjectNode is TProjectRunConfiguationNode then
      PyControl.ExternalRun(TProjectRunConfiguationNode(Data.ProjectNode).RunConfig);
  end;
end;

procedure TProjectExplorerWindow.actProjectExtraPythonPathExecute(
  Sender: TObject);
var
  Paths: TStrings;
begin
  Paths := TStringList.Create;
  Paths.Assign(ActiveProject.ExtraPythonPath);
  try
    if EditFolderList(Paths, _(SProjectPythonPath), 0) then begin
      ActiveProject.RemoveExtraPaths;
      ActiveProject.ExtraPythonPath.Assign(Paths);
      ActiveProject.AppendExtraPaths;
      ActiveProject.Modified := True;
    end;
  finally
    Paths.Free;
  end;
end;

procedure TProjectExplorerWindow.actProjectFileEditExecute(Sender: TObject);
var
  Node: PVirtualNode;
  i : integer;
  SelectedNodes : TNodeArray;
begin
  SelectedNodes := ExplorerTree.GetSortedSelection(False);
  for i := Low(SelectedNodes) to High(SelectedNodes) do begin
    Node := SelectedNodes[i];
    ProjectFileNodeEdit(Node);
  end;
end;

procedure TProjectExplorerWindow.actProjectFilePropertiesExecute(
  Sender: TObject);
var
  Data : PNodeDataRec;
  Node: PVirtualNode;
  Server, FName: string;
begin
  Node := ExplorerTree.GetFirstSelected;
  if Assigned(Node) then begin
    Data := ExplorerTree.GetNodeData(Node);
    if Data.ProjectNode is TProjectFileNode then begin
      if (TProjectFilenode(Data.ProjectNode).FileName <> '') and not
        TSSHFileName.Parse(TProjectFilenode(Data.ProjectNode).FileName, Server, FName)
      then
        DisplayPropDialog(Handle,
          GI_PyIDEServices.ReplaceParams(TProjectFilenode(Data.ProjectNode).FileName));
    end;
  end;
end;

procedure TProjectExplorerWindow.actProjectImportDirectoryExecute(
  Sender: TObject);
var
  Data : PNodeDataRec;
  Node: PVirtualNode;
  TempCursor : IInterface;
begin
  Node := ExplorerTree.GetFirstSelected;
  if Assigned(Node) then begin
    Data := ExplorerTree.GetNodeData(Node);
    if Data.ProjectNode is TProjectFilesNode then begin
      with TImportDirectoryForm do
        if Execute and (Directory<>'') then begin
          TempCursor := WaitCursor;
          TProjectFilesNode(Data.ProjectNode).ImportDirectory(Directory, FileMasks, Recursive);
          ExplorerTree.ReinitNode(Node, True);
        end;
    end;
  end;
end;

procedure TProjectExplorerWindow.actProjectNewExecute(Sender: TObject);
begin
  if CanClose then begin
    if ActiveProject.FileName <> '' then
      PyIDEMainForm.tbiRecentProjects.MRUAdd(ActiveProject.FileName);

    ExplorerTree.Clear;
    FreeAndNil(ActiveProject);
    ActiveProject := TProjectRootNode.Create;
    ExplorerTree.RootNodeCount := 1;
  end;
end;

procedure TProjectExplorerWindow.actProjectOpenExecute(Sender: TObject);
var
  Editor : IEditor;
begin
  if CanClose then begin
    with ResourcesDataModule.dlgFileOpen do begin
      Title := _(SOpenProject);
      FileName := '';
      Filter := Format(ProjectFilter, [ProjectDefaultExtension]);
      Editor := GI_PyIDEServices.ActiveEditor;
      if Assigned(Editor) and (Editor.FileName <> '') and
        (TPath.GetDirectoryName(Editor.FileName) <> '')
      then
        InitialDir := TPath.GetDirectoryName(Editor.FileName);

      if Execute then
        DoOpenProjectFile(FileName);
    end;
  end;
end;

procedure TProjectExplorerWindow.actProjectRelativePathsExecute(
  Sender: TObject);
begin
  ActiveProject.StoreRelativePaths := actProjectRelativePaths.Checked;
  ActiveProject.Modified := True;
end;

procedure TProjectExplorerWindow.actProjectRemoveExecute(Sender: TObject);
var
  Node, ParentNode: PVirtualNode;
  Data : PNodeDataRec;
  SelectedNodes : TNodeArray;
  i : integer;
  CanDelete : Boolean;
begin
  SelectedNodes := ExplorerTree.GetSortedSelection(False);
  ExplorerTree.BeginUpdate;
  try
    // Check the all have the same parent (they should given the selection constraint)
    ParentNode := nil;
    CanDelete := True;
    for i := Low(SelectedNodes) to High(SelectedNodes) do begin
      Node := SelectedNodes[i];
      if not Assigned(ParentNode) then
        ParentNode := Node.Parent
      else if ParentNode <> Node.Parent then begin
        CanDelete := False;
        Break;
      end;
    end;
    CanDelete := CanDelete and Assigned(ParentNode);

    if CanDelete then begin
      for i := Low(SelectedNodes) to High(SelectedNodes) do begin
        Node := SelectedNodes[i];
        Data := ExplorerTree.GetNodeData(Node);
        if (Data.ProjectNode is TProjectFolderNode) or
           (Data.ProjectNode is TProjectFileNode) or
           (Data.ProjectNode is TProjectRunConfiguationNode)
        then begin
          Data.ProjectNode.Free;
        end;
      end;
      ExplorerTree.ReinitNode(ParentNode,True);
      ExplorerTree.ClearSelection;
      ExplorerTree.Selected[ParentNode] := True;
    end;
  finally
    ExplorerTree.EndUpdate;
  end;
end;

procedure TProjectExplorerWindow.actProjectRenameExecute(Sender: TObject);
var
  Data : PNodeDataRec;
  Node: PVirtualNode;
begin
  Node := ExplorerTree.GetFirstSelected;
  if Assigned(Node) then begin
    Data := ExplorerTree.GetNodeData(Node);
    if (Data.ProjectNode is TProjectFolderNode) or
       (Data.ProjectNode is TProjectRunConfiguationNode) then
      ExplorerTree.EditNode(Node, -1)
  end;
end;

procedure TProjectExplorerWindow.actProjectRunExecute(Sender: TObject);
var
  Data : PNodeDataRec;
  Node: PVirtualNode;
begin
  Node := ExplorerTree.GetFirstSelected;
  if Assigned(Node) then begin
    Data := ExplorerTree.GetNodeData(Node);
    if Data.ProjectNode is TProjectRunConfiguationNode then
      PyControl.Run(TProjectRunConfiguationNode(Data.ProjectNode).RunConfig);
  end;
end;

procedure TProjectExplorerWindow.actProjectSaveAsExecute(Sender: TObject);
begin
  DoSaveAs;
end;

procedure TProjectExplorerWindow.actProjectSaveExecute(Sender: TObject);
begin
  DoSave;
end;

procedure TProjectExplorerWindow.actProjectShowFileExtensionsExecute(
  Sender: TObject);
begin
  ActiveProject.ShowFileExtensions := actProjectShowFileExtensions.Checked;
  ActiveProject.Modified := True;
  ExplorerTree.Invalidate;
end;

function TProjectExplorerWindow.CanClose: boolean;
begin
  Result := not ActiveProject.Modified;
  if not Result then begin
    case StyledMessageDlg(_(SAskSaveProject), mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes : Result := DoSave;
      mrNo  : Result := True;
      mrCancel :  Result := False;
    end;
  end;
end;

procedure TProjectExplorerWindow.ProjectFileNodeEdit(Node: PVirtualNode);
var
  Data: PNodeDataRec;
begin
  if Assigned(Node) then
  begin
    Data := ExplorerTree.GetNodeData(Node);
    if Data.ProjectNode is TProjectFileNode and (TProjectFileNode(Data.ProjectNode).FileName <> '') then
    with PyIDEMainForm do begin
      DoOpenFile(GI_PyIDEServices.ReplaceParams(TProjectFileNode(Data.ProjectNode).FileName),
        '', TabControlIndex(ActiveTabControl));
    end;
  end;
end;

procedure TProjectExplorerWindow.ProjectMainPopUpMenuPopup(Sender: TObject);
begin
  actProjectRelativePaths.Checked := ActiveProject.StoreRelativePaths;
  actProjectShowFileExtensions.Checked := ActiveProject.ShowFileExtensions;
end;

procedure TProjectExplorerWindow.DoOpenProjectFile(FileName : string);
var
  AppStorage: TJvAppIniFileStorage;
begin
  if ActiveProject.FileName <> '' then
    PyIDEMainForm.tbiRecentProjects.MRUAdd(ActiveProject.FileName);

  ExplorerTree.Clear;
  ActiveProject.Free;
  ActiveProject := TProjectRootNode.Create;
  ActiveProject.FileName := FileName;
  AppStorage := TJvAppIniFileStorage.Create(nil);
  try
    try
      AppStorage.Encoding := TEncoding.UTF8;
      AppStorage.FlushOnDestroy := False;
      AppStorage.Location := flCustom;
      AppStorage.FileName := ActiveProject.FileName;
      AppStorage.ReadPersistent('Project', ActiveProject);
      ActiveProject.Modified := False;
      ExplorerTree.RootNodeCount := 1;
      PyIDEMainForm.tbiRecentProjects.MRURemove(FileName);
    finally
      AppStorage.Free;
    end;
  except
    on E: Exception do begin
      StyledMessageDlg(Format(_(SErrorInOpeningProject) + sLineBreak +
          'Error: %s', [ActiveProject.FileName, E.Message]), mtError, [mbOK], 0);
    end;
  end;
end;

function TProjectExplorerWindow.DoSave: boolean;
begin
  if ActiveProject.FileName <> '' then
    Result := DoSaveFile
  else
    Result := DoSaveAs;
end;

function TProjectExplorerWindow.DoSaveAs: boolean;
var
  NewName: string;
begin
  NewName := ActiveProject.FileName;
  if NewName = '' then
    NewName := ActiveProject.Name;

  if  ExtractFileExt(NewName) = '' then
    NewName := NewName + '.' + ProjectDefaultExtension;

  with ResourcesDataModule.dlgFileSave do begin
    if NewName <> '' then begin
      InitialDir := TPath.GetDirectoryName(NewName);
      FileName := TPath.GetFileName(NewName);
      Title := Format(_(SSaveProjectFileAs), [FileName]);
    end else begin
      InitialDir := '';
      FileName := '';
      Title := _(SSaveProjectAs);
    end;
    Filter := Format(ProjectFilter, [ProjectDefaultExtension]);
    DefaultExt := ProjectDefaultExtension;

    if Execute then begin
      NewName := FileName;
      Result := True;
    end else
      Result := False;
  end;

  if Result then  begin
    ActiveProject.FileName := NewName;
    Result := DoSaveFile;
  end;
end;

function TProjectExplorerWindow.DoSaveFile: boolean;
Var
  AppStorage : TJvAppIniFileStorage;
begin
  // Create Backup
  if PyIDEOptions.CreateBackupFiles and
    FileExists(ActiveProject.FileName) then
  begin
    try
      FileBackup(ActiveProject.FileName);
    except
      StyledMessageDlg(Format(_(SFailedToBackupProject), [ActiveProject.FileName]),
        mtWarning, [mbOK], 0);
    end;
  end;

  AppStorage := TJvAppIniFileStorage.Create(nil);
  try
    try
      AppStorage.Encoding := TEncoding.UTF8;
      AppStorage.FlushOnDestroy := True;
      AppStorage.Location := flCustom;
      AppStorage.FileName := ActiveProject.FileName;
      AppStorage.DeleteSubTree('Project');
      AppStorage.StorageOptions.StoreDefaultValues := False;
      AppStorage.WriteString('PyScripter\Version', ApplicationVersion);
      AppStorage.WritePersistent('Project', ActiveProject);
    finally
      AppStorage.Free;
    end;
    ActiveProject.Modified := False;
    Result := True;
    ExplorerTree.ReinitNode(ExplorerTree.RootNode, False);
  except
    on E: Exception do begin
      StyledMessageDlg(Format(_(SErrorInSavingProject) + sLineBreak +
          'Error: %s', [ActiveProject.FileName, E.Message]), mtError, [mbOK], 0);
      Result := False;
    end;
  end;
end;

procedure TProjectExplorerWindow.UpdatePopupActions(Node : PVirtualNode);
Var
  Data : PNodeDataRec;
  SingleNodeSelected : Boolean;
begin
   actProjectExtraPythonPath.Enabled := GI_PyControl.PythonLoaded and not GI_PyControl.Running;
   // We update project actions here based on selection
   SingleNodeSelected := Assigned(Node) and
     (Length(ExplorerTree.GetSortedSelection(False)) = 1);
   if Assigned(Node) then begin
     Data := ExplorerTree.GetNodeData(Node);
     Assert(Assigned(Data.ProjectNode));
     actProjectAddFiles.Enabled := (Data.ProjectNode is TProjectFilesNode) and SingleNodeSelected;
     actProjectAddActiveFile.Enabled := actProjectAddFiles.Enabled;
     actProjectAddRemoteFile.Enabled := actProjectAddFiles.Enabled;
     actProjectAddFolder.Enabled := actProjectAddFiles.Enabled;
     actProjectRemove.Enabled := (Data.ProjectNode is TProjectFolderNode) or
       (Data.ProjectNode is TProjectFileNode) or
       (Data.ProjectNode is TProjectRunConfiguationNode);
     actProjectRename.Enabled := ((Data.ProjectNode is TProjectFolderNode) or
       (Data.ProjectNode is TProjectRunConfiguationNode)) and SingleNodeSelected;
     actProjectImportDirectory.Enabled := (Data.ProjectNode is TProjectFilesNode) and SingleNodeSelected;
     actProjectFileEdit.Enabled := Data.ProjectNode is TProjectFileNode;
     actProjectFileProperties.Enabled := (Data.ProjectNode is TProjectFileNode) and SingleNodeSelected;
     actProjectAddRunConfig.Enabled := (Data.ProjectNode is TProjectRunConfiguationsNode) and SingleNodeSelected;
     actProjectEditRunConfig.Enabled := (Data.ProjectNode is TProjectRunConfiguationNode) and SingleNodeSelected;
     actProjectRun.Enabled := GI_PyControl.Inactive and
       (Data.ProjectNode is TProjectRunConfiguationNode) and SingleNodeSelected;
     actProjectExternalRun.Enabled := actProjectRun.Enabled;
     actProjectDebug.Enabled := actProjectRun.Enabled;
   end else begin
     actProjectAddFiles.Enabled := False;
     actProjectAddActiveFile.Enabled := False;
     actProjectAddRemoteFile.Enabled := False;
     actProjectRemove.Enabled := False;
     actProjectRename.Enabled := False;
     actProjectAddFolder.Enabled := False;
     actProjectImportDirectory.Enabled := False;
     actProjectFileEdit.Enabled := False;
     actProjectFileProperties.Enabled := False;
     actProjectAddRunConfig.Enabled := False;
     actProjectEditRunConfig.Enabled := False;
     actProjectRun.Enabled := False;
     actProjectExternalRun.Enabled := False;
     actProjectDebug.Enabled := False;
   end;
end;

procedure TProjectExplorerWindow.WMSpSkinChange(var Message: TMessage);
begin
  inherited;
  icProjects.SVGIconItems.BeginUpdate;
  try
    icProjects.FixedColor := SvgFixedColor(clWindowText);
    icProjects.AntiAliasColor := StyleServices.GetSystemColor(clWindow);
  finally
    icProjects.SVGIconItems.EndUpdate;
  end;
end;

procedure TProjectExplorerWindow.ExplorerTreeContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
  Data : PNodeDataRec;
  Node : PVirtualNode;
  PopUpMenu : TPopupMenu;
  Pos : TPoint;
  HitInfo : THitInfo;
begin
  PopUpMenu := nil;
  if (MousePos.X = -1) and (MousePos.Y = -1) then
    // Keyboard invocation
    Node := ExplorerTree.GetFirstSelected
  else begin
    ExplorerTree.GetHitTestInfoAt(MousePos.X, MousePos.Y, True, HitInfo);
    Node := HitInfo.HitNode;
    if not ([hiOnItemLabel, hiOnNormalIcon] * HitInfo.HitPositions <> []) then
      Node := nil;
    if Assigned(Node) and not (vsSelected in Node.States) then
      Node := nil;
  end;
  if Assigned(Node) then begin
    Data := ExplorerTree.GetNodeData(Node);
    if (Data.ProjectNode is TProjectRootNode) then
      PopUpMenu := ProjectMainPopupMenu
    else if (Data.ProjectNode is TProjectFileNode) then
      PopUpMenu := ProjectFilePopupMenu
    else if Data.ProjectNode is TProjectRunConfiguationsNode then
      PopUpMenu := ProjectRunSettingsPopupMenu
    else if Data.ProjectNode is TProjectRunConfiguationNode then
      PopUpMenu := ProjectRunConfigPopupMenu
    else if (Data.ProjectNode is TProjectFolderNode) or
       (Data.ProjectNode is TProjectFilesNode)
    then
      PopUpMenu := ProjectFolderPopupMenu;

  end else begin
    PopUpMenu := ProjectMainPopUpMenu;
  end;
  if Assigned(PopUpMenu) then begin
    UpdatePopupActions(Node);
    Pos := ExplorerTree.ClientToScreen(MousePos);
    PopUpMenu.Popup(Pos.X, Pos.Y);
  end;
  Handled := True;
end;

procedure TProjectExplorerWindow.ExplorerTreeDragAllowed(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  var Allowed: Boolean);
var
  Data: PNodeDataRec;
begin
  Allowed := False;
  if Assigned(Node) then
  begin
    Data := ExplorerTree.GetNodeData(Node);
    if (Data.ProjectNode is TProjectFileNode) or (Data.ProjectNode is TProjectFolderNode) then
      Allowed := True;
  end;
end;

procedure TProjectExplorerWindow.ExplorerTreeDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
Var
  HitInfo : THitInfo;
  Node, ParentNode: PVirtualNode;
  Data, SelectedData : PNodeDataRec;
  FileNode : TProjectFileNode;
  i: Integer;
  CommonHDrop : TCommonHDrop;
  FileName : string;
  SelectedNodes : TNodeArray;
  SelectedNode : TAbstractProjectNode;
  CanMove : Boolean;
  TempCursor : IInterface;
begin
  ExplorerTree.GetHitTestInfoAt(Pt.X, Pt.Y, True, HitInfo);
  Node := HitInfo.HitNode;
  if not ([hiOnItemLabel, hiOnNormalIcon] * HitInfo.HitPositions <> []) then
    Node := nil;
  if Assigned(Node) then begin
    Data := ExplorerTree.GetNodeData(Node);
    if Data.ProjectNode is TProjectFilesNode then begin
      TempCursor := WaitCursor;
      if Sender = Source then begin
        // Internal Drop
        SelectedNodes := ExplorerTree.GetSortedSelection(False);
        if Length(SelectedNodes) > 0 then begin
          ParentNode := nil;
          // Check the all have the same parent (they should given the selection constraint)
          CanMove := True;
          for i := Low(SelectedNodes) to High(SelectedNodes) do begin
            SelectedData := ExplorerTree.GetNodeData(SelectedNodes[i]);
            SelectedNode := SelectedData.ProjectNode;
            if not Assigned(ParentNode) then
              ParentNode := SelectedNodes[i].Parent
            else if ParentNode <> SelectedNodes[i].Parent then begin
              CanMove := False;
              Break;
            end;
            if not ((SelectedNode is TProjectFolderNode) or (SelectedNode is TProjectFileNode)) then begin
              CanMove := False;
              Break;
            end;
          end;
          CanMove := CanMove and Assigned(ParentNode) and (ParentNode <> Node);
          if CanMove then begin
            ExplorerTree.BeginUpdate;
            try
              for i := Low(SelectedNodes) to High(SelectedNodes) do begin
                SelectedData := ExplorerTree.GetNodeData(SelectedNodes[i]);
                SelectedNode := SelectedData.ProjectNode;
                SelectedNode.Parent := Data.ProjectNode;
              end;
              ExplorerTree.ReinitNode(Node, True);
              ExplorerTree.ReinitNode(ParentNode, True);
              ExplorerTree.ClearSelection;
              ExplorerTree.Selected[Node]:= True;
            finally
              ExplorerTree.EndUpdate;
            end;
          end;
        end;
      end else if Assigned(DataObject) then begin
        // OLE drag drop
        CommonHDrop := TCommonHDrop.Create;
        ExplorerTree.BeginUpdate;
        try
          if CommonHDrop.LoadFromDataObject(DataObject) then begin
            for i := 0 to CommonHDrop.FileCount - 1 do begin
              FileName := CommonHDrop.FileName(i);
              with TProjectFilesNode(Data.ProjectNode) do
                if DirectoryExists(FileName) then
                  ImportDirectory(FileName,
                    PyIDEOptions.PythonFileExtensions,
                    True)
                else if FileExists(FileName) then begin
                  if not Assigned(FileChild[FileName]) then begin
                    FileNode := TProjectFileNode.Create;
                    FileNode.FileName := FileName;
                    AddChild(FileNode);
                  end;
                end;
            end;
            ExplorerTree.ReinitNode(Node, True);
            ExplorerTree.ClearSelection;
            ExplorerTree.Selected[Node]:= True;
          end;
        finally
          CommonHDrop.Free;
          ExplorerTree.EndUpdate;
        end;
      end;
    end;
  end;
end;

procedure TProjectExplorerWindow.ExplorerTreeDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
  Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
Var
  HitInfo: THitInfo;
  Node: PVirtualNode;
  Data: PNodeDataRec;
begin
  Accept := False;
  Sender.GetHitTestInfoAt(Pt.X, Pt.Y, True, HitInfo);
  Node := HitInfo.HitNode;
  if not ([hiOnItemLabel, hiOnNormalIcon] * HitInfo.HitPositions <> []) then
    Node := nil;
  if Assigned(Node) then begin
    Data := ExplorerTree.GetNodeData(Node);
    if Data.ProjectNode is TProjectFilesNode then begin
        Accept := True;
      if Assigned(Source) and (Sender = Source) then
        Effect := DROPEFFECT_MOVE
      else
        Effect := DROPEFFECT_COPY;
    end;
  end;
end;

procedure TProjectExplorerWindow.ExplorerTreeEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  Data : PNodeDataRec;
begin
  Data := ExplorerTree.GetNodeData(Node);
  Allowed := (Data.ProjectNode is TProjectFolderNode) or
             (Data.ProjectNode is TProjectRunConfiguationNode);
end;

procedure TProjectExplorerWindow.ExplorerTreeGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
var
  Data: PNodeDataRec;
begin
  if Assigned(Node) then
  begin
    Data := ExplorerTree.GetNodeData(Node);
    if Data.ProjectNode is TProjectFileNode and (TProjectFileNode(Data.ProjectNode).FileName <> '') then
    begin
      HintText := GI_PyIDEServices.ReplaceParams(TProjectFileNode(Data.ProjectNode).FileName);
    end else if Data.ProjectNode is TProjectRootNode and (TProjectRootNode(Data.ProjectNode).FileName <> '') then
    begin
      HintText := GI_PyIDEServices.ReplaceParams(TProjectRootNode(Data.ProjectNode).FileName);
    end else if Data.ProjectNode is TProjectRunConfiguationNode and
      (TProjectRunConfiguationNode(Data.ProjectNode).RunConfig.Description <> '') then
    begin
      HintText := TProjectRunConfiguationNode(Data.ProjectNode).RunConfig.Description;
    end;
  end;
end;

procedure TProjectExplorerWindow.ExplorerTreeGetImageIndexEx(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex;
  var ImageList: TCustomImageList);
var
  Data : PNodeDataRec;
  Extension : string;
  Index : Integer;
  FileName : string;
begin
  if not (Kind in [ikNormal, ikSelected]) then Exit;
  ImageIndex := -1;
  Data := ExplorerTree.GetNodeData(Node);
  if Data.ProjectNode is TProjectRootNode then
    ImageIndex := 0
  else if (Data.ProjectNode is TProjectFilesNode) or
          (Data.ProjectNode is TProjectFolderNode)
  then
    ImageIndex := 1
  else if Data.ProjectNode is TProjectRunConfiguationNode then
    ImageIndex := 3
  else if Data.ProjectNode is TProjectFileNode then begin
    FileName := GI_PyIDEServices.ReplaceParams(TProjectFileNode(Data.ProjectNode).FileName);
    Extension := ExtractFileExt(FileName);
    if Extension <> '' then begin
      Index := FileImageList.IndexOf(Extension);
      if Index < 0 then begin
        if FileExists(FileName) then begin
          ImageIndex := GetIconIndexFromFile(FileName, True);
          FileImageList.AddObject(Extension, TObject(ImageIndex));
        end;
      end else
        ImageIndex := Integer(FileImageList.Objects[Index]);
      if ImageIndex >= 0 then
        ImageList := SmallSysImages;
    end;
  end else if Data.ProjectNode is TProjectRunConfiguationsNode then
    ImageIndex := 2;
end;

procedure TProjectExplorerWindow.ExplorerTreeGetCellText(
  Sender: TCustomVirtualStringTree; var E: TVSTGetCellTextEventArgs);
var
  Data : PNodeDataRec;
begin
  Data := ExplorerTree.GetNodeData(E.Node);
  if Assigned(Data) then
    E.CellText := Data.ProjectNode.Caption;
end;

procedure TProjectExplorerWindow.ExplorerTreeIncrementalSearch(
  Sender: TBaseVirtualTree; Node: PVirtualNode; const SearchText: string;
  var Result: Integer);
var
  Data : PNodeDataRec;
begin
  Result := -1;
  Data := ExplorerTree.GetNodeData(Node);
  if Assigned(Data)
    and (Pos(SearchText.ToUpper, Data.ProjectNode.Caption.ToUpper) > 0)
  then
    Result := 0;
end;

procedure TProjectExplorerWindow.ExplorerTreeInitChildren(
  Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
var
  Data : PNodeDataRec;
begin
  Data := ExplorerTree.GetNodeData(Node);
  ChildCount := Data.ProjectNode.Children.Count;
end;

procedure TProjectExplorerWindow.ExplorerTreeInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
Var
  Data, ParentData: PNodeDataRec;
begin
  Data := ExplorerTree.GetNodeData(Node);
  if ParentNode = nil then
    Data.ProjectNode := ActiveProject
  else begin
    ParentData := ExplorerTree.GetNodeData(ParentNode);
    Data.ProjectNode :=
      ParentData.ProjectNode.Children[Node.Index] as TAbstractProjectNode;
  end;
  if Data.ProjectNode.Children.Count > 0 then begin
    InitialStates := [ivsHasChildren];
    if (not (ivsReInit in InitialStates) and PyIDEOptions.ProjectExplorerInitiallyExpanded)
      or (Node.Parent = ExplorerTree.RootNode)
    then
      Include(InitialStates, ivsExpanded);
  end;
end;

procedure TProjectExplorerWindow.ExplorerTreeKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = Char(VK_Return) then
    ProjectFileNodeEdit(ExplorerTree.GetFirstSelected);
end;

procedure TProjectExplorerWindow.ExplorerTreeNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: string);
var
  Data, ParentData : PNodeDataRec;
  ModifiedText : Boolean;
begin
  ModifiedText := False;
  Data := ExplorerTree.GetNodeData(Node);
  if Data.ProjectNode is TProjectFolderNode then begin
    TProjectFolderNode(Data.ProjectNode).Name := NewText;
    Data.ProjectNode.Modified := True;
    ModifiedText := True;
  end else if Data.ProjectNode is TProjectRunConfiguationNode then begin
    TProjectRunConfiguationNode(Data.ProjectNode).Name := NewText;
    Data.ProjectNode.Modified := True;
    ModifiedText := True;
  end;

  if ModifiedText then begin
    Assert(Assigned(Node.Parent));
    ParentData := ExplorerTree.GetNodeData(Node.Parent);
    Assert(Assigned(ParentData));
    ExplorerTree.BeginUpdate;
    try
      ParentData.ProjectNode.SortChildren;
      ExplorerTree.ReInitNode(Node.Parent,True);
    finally
      ExplorerTree.EndUpdate;
    end;
  end;
end;

procedure TProjectExplorerWindow.ExplorerTreeNodeDblClick(Sender:
    TBaseVirtualTree; const HitInfo: THitInfo);
var
  Data : PNodeDataRec;
  Node: PVirtualNode;
begin
  Node := HitInfo.HitNode;
  if Assigned(Node) and (hiOnItem in HitInfo.HitPositions) then begin
    Data := ExplorerTree.GetNodeData(Node);
    if Data.ProjectNode is TProjectFileNode then
      ProjectFileNodeEdit(Node)
    else if Data.ProjectNode is TProjectRunConfiguationNode then
      if EditRunConfiguration(TProjectRunConfiguationNode(Data.ProjectNode).RunConfig) then
        Data.ProjectNode.Modified:= True;
  end;
end;

procedure TProjectExplorerWindow.FormActivate(Sender: TObject);
begin
  inherited;
  if ExplorerTree.CanFocus then
    ExplorerTree.SetFocus;
end;

procedure TProjectExplorerWindow.FormCreate(Sender: TObject);
begin
  ImageName := 'ProjectExplorer';
  inherited;

  // Let the tree know how much data space we need.
  ExplorerTree.NodeDataSize := SizeOf(TNodeDataRec);
  FileImageList := TStringList.Create;
  FileImageList.Sorted := True;
  FileImageList.Duplicates := dupError;

  // Wierd translation bug
  TP_Ignore(self, 'mnProjectNew');
  TP_Ignore(self, 'mnProjectOpen');
  TP_Ignore(self, 'mnProjectSave');
  TP_Ignore(self, 'mnProjectSaveAs');
end;

procedure TProjectExplorerWindow.FormDestroy(Sender: TObject);
begin
  ExplorerTree.Clear;
  FileImageList.Free;
  inherited;
end;

procedure TProjectExplorerWindow.FormShow(Sender: TObject);
begin
  inherited;
  ExplorerTree.RootNodeCount := 1;
end;

end.
