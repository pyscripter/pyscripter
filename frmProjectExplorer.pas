{-----------------------------------------------------------------------------
 Unit Name: frmProjectExplorer
 Author:    Kiriakos Vlahos
 Date:      02-Dec-2007
 Purpose:   PyScripter project explorer
 History:
-----------------------------------------------------------------------------}

unit frmProjectExplorer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmIDEDockWin, JvComponentBase, JvDockControlForm, ExtCtrls,
  cProjectClasses, VirtualTrees, ImgList, Menus, TB2Item, TBX, SpTBXItem,
  ActnList, TB2Dock, TB2Toolbar, JvDragDrop;

type
  TProjectExplorerWindow = class(TIDEDockWindow)
    Panel1: TPanel;
    ExplorerTree: TVirtualStringTree;
    ImmutableProjectActionList: TActionList;
    ProjectMainPopUpMenu: TSpTBXPopupMenu;
    ProjectImageList: TImageList;
    ProjectFolderPopupMenu: TSpTBXPopupMenu;
    actProjectNew: TAction;
    actProjectSave: TAction;
    actProjectOpen: TAction;
    actProjectSaveAs: TAction;
    actProjectAddFiles: TAction;
    actProjectRemove: TAction;
    actProjectRename: TAction;
    actProjectAddFolder: TAction;
    SpTBXItem1: TSpTBXItem;
    SpTBXItem2: TSpTBXItem;
    SpTBXItem3: TSpTBXItem;
    SpTBXItem4: TSpTBXItem;
    SpTBXItem5: TSpTBXItem;
    SpTBXItem6: TSpTBXItem;
    SpTBXItem7: TSpTBXItem;
    SpTBXItem8: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    ProjectFilePopupMenu: TSpTBXPopupMenu;
    SpTBXItem9: TSpTBXItem;
    actProjectFileEdit: TAction;
    SpTBXSeparatorItem3: TSpTBXSeparatorItem;
    SpTBXItem10: TSpTBXItem;
    actProjectAddActiveFile: TAction;
    SpTBXItem11: TSpTBXItem;
    actProjectImportDirectory: TAction;
    SpTBXItem12: TSpTBXItem;
    ProjectRunSettingsPopupMenu: TSpTBXPopupMenu;
    ProjectRunConfigPopupMenu: TSpTBXPopupMenu;
    actProjectAddRunConfig: TAction;
    SpTBXItem13: TSpTBXItem;
    SpTBXItem14: TSpTBXItem;
    SpTBXSeparatorItem4: TSpTBXSeparatorItem;
    SpTBXItem15: TSpTBXItem;
    actProjectEditRunConfig: TAction;
    SpTBXSeparatorItem5: TSpTBXSeparatorItem;
    SpTBXItem16: TSpTBXItem;
    actProjectFileProperties: TAction;
    SpTBXItem17: TSpTBXItem;
    SpTBXSeparatorItem6: TSpTBXSeparatorItem;
    actProjectExternalRun: TAction;
    SpTBXSeparatorItem7: TSpTBXSeparatorItem;
    SpTBXItem18: TSpTBXItem;
    SpTBXDock1: TSpTBXDock;
    SpTBXToolbar1: TSpTBXToolbar;
    SpTBXItem19: TSpTBXItem;
    SpTBXItem20: TSpTBXItem;
    SpTBXItem21: TSpTBXItem;
    SpTBXSeparatorItem8: TSpTBXSeparatorItem;
    SpTBXItem24: TSpTBXItem;
    SpTBXItem23: TSpTBXItem;
    SpTBXItem22: TSpTBXItem;
    actProjectRun: TAction;
    SpTBXItem25: TSpTBXItem;
    actProjectDebug: TAction;
    SpTBXItem26: TSpTBXItem;
    actProjectRelativePaths: TAction;
    SpTBXSeparatorItem9: TSpTBXSeparatorItem;
    SpTBXItem27: TSpTBXItem;
    ProjectActionList: TActionList;
    JvDropTarget: TJvDropTarget;
    procedure FormCreate(Sender: TObject);
    procedure ExplorerTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
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
      Column: TColumnIndex; NewText: WideString);
    procedure actProjectAddFilesExecute(Sender: TObject);
    procedure actProjectRenameExecute(Sender: TObject);
    procedure ExplorerTreeGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure actProjectSaveExecute(Sender: TObject);
    procedure actProjectSaveAsExecute(Sender: TObject);
    procedure actProjectNewExecute(Sender: TObject);
    procedure actProjectOpenExecute(Sender: TObject);
    procedure actProjectRemoveExecute(Sender: TObject);
    procedure ExplorerTreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure actProjectFileEditExecute(Sender: TObject);
    procedure ExplorerTreeDblClick(Sender: TObject);
    procedure ExplorerTreeKeyPress(Sender: TObject; var Key: Char);
    procedure actProjectAddActiveFileExecute(Sender: TObject);
    procedure actProjectImportDirectoryExecute(Sender: TObject);
    procedure actProjectAddRunConfigExecute(Sender: TObject);
    procedure actProjectFilePropertiesExecute(Sender: TObject);
    procedure ExplorerTreeGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: WideString);
    procedure actProjectEditRunConfigExecute(Sender: TObject);
    procedure actProjectExternalRunExecute(Sender: TObject);
    procedure actProjectRunExecute(Sender: TObject);
    procedure ProjectRunConfigPopupMenuPopup(Sender: TObject);
    procedure actProjectDebugExecute(Sender: TObject);
    procedure actProjectRelativePathsExecute(Sender: TObject);
    procedure ProjectMainPopUpMenuPopup(Sender: TObject);
    procedure JvDropTargetDragOver(Sender: TJvDropTarget;
      var Effect: TJvDropEffect);
    procedure JvDropTargetDragDrop(Sender: TJvDropTarget;
      var Effect: TJvDropEffect; Shift: TShiftState; X, Y: Integer);
  private
    procedure ProjectFileNodeEdit(Node: PVirtualNode);
  public
    { Public declarations }
    FileImageList: TStringList;
    procedure DoOpenProjectFile(FileName : WideString);
    function DoSave: boolean;
    function DoSaveFile: boolean;
    function DoSaveAs: boolean;
    function CanClose: boolean;
  end;

Const
  ProjectDefaultExtension = 'psproj';
  ProjectFilter = 'PyScripter project files (*.%s)|*.%0:s';
var
  ProjectExplorerWindow: TProjectExplorerWindow;

implementation

uses dmCommands, uHighlighterProcs, StringResources, uEditAppIntfs,
  frmPyIDEMain, uCommonFunctions, JvAppIniStorage, JvAppStorage, JclFileUtils,
  dlgImportDirectory, JclShell, dlgRunConfiguration, cPyBaseDebugger,
  cParameters;

{$R *.dfm}

Type
  PNodeDataRec = ^TNodeDataRec;
  TNodeDataRec = record
    ProjectNode : TAbstractProjectNode;
  end;


procedure TProjectExplorerWindow.actProjectAddActiveFileExecute(
  Sender: TObject);
Var
  Editor : IEditor;
  Data : PNodeDataRec;
  Node: PVirtualNode;
  ProjectNode : TProjectFileNode;
begin
  Node := ExplorerTree.GetFirstSelected;
  Editor := PyIDEMainForm.GetActiveEditor;
  if Assigned(Node) and Assigned(Editor) and (Editor.FileName <> '') and
    not ActiveProject.HasFile(Editor.FileName) then
  begin
    Data := ExplorerTree.GetNodeData(Node);
    if Data.ProjectNode is TProjectFilesNode then
    begin
      ProjectNode := TProjectFileNode.Create;
      ProjectNode.FileName := Editor.FileName;
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
      with CommandsDataModule.dlgFileOpen do begin
        Title := 'Add File(s) to Project';
        FileName := '';
        Filter := GetHighlightersFilter(CommandsDataModule.Highlighters) + SFilterAllFiles;
        Editor := PyIDEMainForm.GetActiveEditor;
        if Assigned(Editor) and (Editor.FileName <> '') and
          (ExtractFileDir(Editor.FileName) <> '')
        then
          InitialDir := ExtractFileDir(Editor.FileName);

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
      ProjectNode.Name := 'Untitled';
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

procedure TProjectExplorerWindow.actProjectFileEditExecute(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := ExplorerTree.GetFirstSelected;
  ProjectFileNodeEdit(Node);
end;

procedure TProjectExplorerWindow.actProjectFilePropertiesExecute(
  Sender: TObject);
var
  Data : PNodeDataRec;
  Node: PVirtualNode;
begin
  Node := ExplorerTree.GetFirstSelected;
  if Assigned(Node) then begin
    Data := ExplorerTree.GetNodeData(Node);
    if Data.ProjectNode is TProjectFileNode then begin
      if TProjectFilenode(Data.ProjectNode).FileName <> '' then
        DisplayPropDialog(Handle,
          Parameters.ReplaceInText(TProjectFilenode(Data.ProjectNode).FileName));
    end;
  end;
end;

procedure TProjectExplorerWindow.actProjectImportDirectoryExecute(
  Sender: TObject);
var
  Data : PNodeDataRec;
  Node: PVirtualNode;
begin
  Node := ExplorerTree.GetFirstSelected;
  if Assigned(Node) then begin
    Data := ExplorerTree.GetNodeData(Node);
    if Data.ProjectNode is TProjectFilesNode then begin
      with TImportDirectoryForm do
        if Execute and (Directory<>'') then begin
          TProjectFilesNode(Data.ProjectNode).ImportDirectory(Directory, FileMask, Recursive);
          ExplorerTree.ReinitNode(Node, True);
        end;
    end;
  end;
end;

procedure TProjectExplorerWindow.actProjectNewExecute(Sender: TObject);
begin
  if CanClose then begin
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
    with CommandsDataModule.dlgFileOpen do begin
      Title := 'Open Project';
      FileName := '';
      Filter := Format(ProjectFilter, [ProjectDefaultExtension]);
      Editor := PyIDEMainForm.GetActiveEditor;
      if Assigned(Editor) and (Editor.FileName <> '') and
        (ExtractFileDir(Editor.FileName) <> '')
      then
        InitialDir := ExtractFileDir(Editor.FileName);

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
  Data : PNodeDataRec;
  Node: PVirtualNode;
begin
  Node := ExplorerTree.GetFirstSelected;
  if Assigned(Node) then begin
    Data := ExplorerTree.GetNodeData(Node);
    if (Data.ProjectNode is TProjectFolderNode) or
       (Data.ProjectNode is TProjectFileNode) or
       (Data.ProjectNode is TProjectRunConfiguationNode)
    then begin
      ExplorerTree.BeginUpdate;
      try
        Data.ProjectNode.Free;
        ExplorerTree.ReinitNode(Node.Parent,True);
      finally
        ExplorerTree.EndUpdate;
      end;
    end;
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

function TProjectExplorerWindow.CanClose: boolean;
begin
  Result := not ActiveProject.Modified;
  if not Result then begin
    case MessageDlg('''The active project has not been saved.  Do you want to save the changes?''',
      mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      idYes : Result := DoSave;
      idNo  : Result := True;
      idCancel :  Result := False;
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
    begin
      PyIDEMainForm.DoOpenFile(Parameters.ReplaceInText(TProjectFileNode(Data.ProjectNode).FileName));
    end;
  end;
end;

procedure TProjectExplorerWindow.ProjectMainPopUpMenuPopup(Sender: TObject);
begin
  actProjectRelativePaths.Checked := ActiveProject.StoreRelativePaths;
end;

procedure TProjectExplorerWindow.ProjectRunConfigPopupMenuPopup(
  Sender: TObject);
begin
   actProjectRun.Enabled := PyControl.DebuggerState = dsInactive;
   actProjectDebug.Enabled := actProjectRun.Enabled;
   actProjectExternalRun.Enabled := actProjectRun.Enabled;
end;

procedure TProjectExplorerWindow.DoOpenProjectFile(FileName : WideString);
var
  AppStorage: TJvAppIniFileStorage;
begin
  ExplorerTree.Clear;
  ActiveProject.Free;
  ActiveProject := TProjectRootNode.Create;
  ActiveProject.FileName := FileName;
  AppStorage := TJvAppIniFileStorage.Create(nil);
  try
    AppStorage.FlushOnDestroy := False;
    AppStorage.Location := flCustom;
    AppStorage.FileName := ActiveProject.FileName;
    AppStorage.ReadPersistent('Project', ActiveProject);
    ActiveProject.Modified := False;
    ExplorerTree.RootNodeCount := 1;
  finally
    AppStorage.Free;
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

  with CommandsDataModule.dlgFileSave do begin
    if NewName <> '' then begin
      InitialDir := ExtractFileDir(NewName);
      FileName := ExtractFileName(NewName);
      Title := Format('Save Project "%s" As', [FileName]);
    end else begin
      InitialDir := '';
      FileName := '';
      Title := 'Save Project As';
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
  if CommandsDataModule.PyIDEOptions.CreateBackupFiles and
    FileExists(ActiveProject.FileName) then
  begin
    try
      FileBackup(ActiveProject.FileName);
    except
      MessageDlg(Format('Failed to backup project "%s"', [ActiveProject.FileName]),
        mtWarning, [mbOK], 0);
    end;
  end;

  AppStorage := TJvAppIniFileStorage.Create(nil);
  try
    try
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
      MessageBox(0, PChar(E.Message), PChar(Format('Error in saving project: "%s"', [ActiveProject.FileName])),
        MB_ICONERROR or MB_OK);
      Result := False;
    end;
  end;
end;

procedure TProjectExplorerWindow.ExplorerTreeChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
Var
  TreeNode: PVirtualNode;
  Data : PNodeDataRec;
begin
   // We update project actions here based on selection
   TreeNode := ExplorerTree.GetFirstSelected;
   if Assigned(TreeNode) then begin
     Data := ExplorerTree.GetNodeData(Node);
     Assert(Assigned(Data.ProjectNode));
     actProjectAddFiles.Enabled := Data.ProjectNode is TProjectFilesNode;
     actProjectAddActiveFile.Enabled := Data.ProjectNode is TProjectFilesNode;
     actProjectRemove.Enabled := (Data.ProjectNode is TProjectFolderNode) or
       (Data.ProjectNode is TProjectFileNode) or
       (Data.ProjectNode is TProjectRunConfiguationNode);
     actProjectRename.Enabled := (Data.ProjectNode is TProjectFolderNode) or
       (Data.ProjectNode is TProjectRunConfiguationNode);
     actProjectAddFolder.Enabled := Data.ProjectNode is TProjectFilesNode;
     actProjectImportDirectory.Enabled := Data.ProjectNode is TProjectFilesNode;
     actProjectFileEdit.Enabled := Data.ProjectNode is TProjectFileNode;
     actProjectFileProperties.Enabled := Data.ProjectNode is TProjectFileNode;
     actProjectAddRunConfig.Enabled := Data.ProjectNode is TProjectRunConfiguationsNode;
     actProjectEditRunConfig.Enabled := Data.ProjectNode is TProjectRunConfiguationNode;
   end else begin
     actProjectAddFiles.Enabled := False;
     actProjectAddActiveFile.Enabled := False;
     actProjectRemove.Enabled := False;
     actProjectRename.Enabled := False;
     actProjectAddFolder.Enabled := False;
     actProjectImportDirectory.Enabled := False;
     actProjectFileEdit.Enabled := False;
     actProjectFileProperties.Enabled := False;
     actProjectAddRunConfig.Enabled := False;
     actProjectEditRunConfig.Enabled := False;
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
    Pos := ExplorerTree.ClientToScreen(MousePos);
    PopUpMenu.Popup(Pos.X, Pos.Y);
  end;
  Handled := True;
end;

procedure TProjectExplorerWindow.ExplorerTreeDblClick(Sender: TObject);
var
  Data : PNodeDataRec;
  Node: PVirtualNode;
begin
  Node := ExplorerTree.GetFirstSelected;
  if Assigned(Node) then begin
    Data := ExplorerTree.GetNodeData(Node);
    if Data.ProjectNode is TProjectFileNode then
      ProjectFileNodeEdit(ExplorerTree.HotNode)
    else if Data.ProjectNode is TProjectRunConfiguationNode then
      actProjectEditRunConfigExecute(Sender);
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
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: WideString);
var
  Data: PNodeDataRec;
begin
  if Assigned(Node) then
  begin
    Data := ExplorerTree.GetNodeData(Node);
    if Data.ProjectNode is TProjectFileNode and (TProjectFileNode(Data.ProjectNode).FileName <> '') then
    begin
      HintText := Parameters.ReplaceInText(TProjectFileNode(Data.ProjectNode).FileName);
    end else if Data.ProjectNode is TProjectRootNode and (TProjectRootNode(Data.ProjectNode).FileName <> '') then
    begin
      HintText := Parameters.ReplaceInText(TProjectRootNode(Data.ProjectNode).FileName);
    end else if Data.ProjectNode is TProjectRunConfiguationNode and
      (TProjectRunConfiguationNode(Data.ProjectNode).RunConfig.Description <> '') then
    begin
      HintText := TProjectRunConfiguationNode(Data.ProjectNode).RunConfig.Description;
    end;
  end;
end;

procedure TProjectExplorerWindow.ExplorerTreeGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data : PNodeDataRec;
  Extension : string;
  Index, ImgIndex : Integer;
  FileName : WideString;
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
    FileName := TProjectFileNode(Data.ProjectNode).FileName;
    Extension := ExtractFileExt(FileName);
    Index := FileImageList.IndexOf(Extension);
    if Index < 0 then begin
      if (Extension <> '') and FileExists(FileName) then begin
        ImgIndex := GetIconIndexFromFile(FileName, True);
        if ImgIndex >= 0 then begin
          ImageIndex :=
            ProjectImageList.AddImage(CommandsDataModule.imlShellIcon, ImgIndex);
          FileImageList.AddObject(Extension, TObject(ImageIndex));
        end;
      end;
    end else
      ImageIndex := Integer(FileImageList.Objects[Index]);
  end else if Data.ProjectNode is TProjectRunConfiguationsNode then
      ImageIndex := 2;
end;

procedure TProjectExplorerWindow.ExplorerTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  Data : PNodeDataRec;
begin
  Data := ExplorerTree.GetNodeData(Node);
  CellText := Data.ProjectNode.Caption;
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
  if ExplorerTree.GetNodeLevel(Node) = 0 then
    Data.ProjectNode := ActiveProject
  else begin
    ParentData := ExplorerTree.GetNodeData(ParentNode);
    Data.ProjectNode :=
      ParentData.ProjectNode.Children[Node.Index] as TAbstractProjectNode;
  end;
  if Data.ProjectNode.Children.Count > 0 then
      InitialStates := [ivsHasChildren, ivsExpanded];
end;

procedure TProjectExplorerWindow.ExplorerTreeKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = Char(VK_Return) then
    ProjectFileNodeEdit(ExplorerTree.GetFirstSelected);
end;

procedure TProjectExplorerWindow.ExplorerTreeNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
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

procedure TProjectExplorerWindow.FormActivate(Sender: TObject);
begin
  inherited;
  if ExplorerTree.CanFocus then
    ExplorerTree.SetFocus;
end;

procedure TProjectExplorerWindow.FormCreate(Sender: TObject);
begin
  inherited;
  // Let the tree know how much data space we need.
  ExplorerTree.NodeDataSize := SizeOf(TNodeDataRec);
  FileImageList := TStringList.Create;
  FileImageList.Sorted := True;
  FileImageList.Duplicates := dupError;
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
  JvDropTarget.Control := ExplorerTree;
  ExplorerTree.RootNodeCount := 1;
end;

procedure TProjectExplorerWindow.JvDropTargetDragDrop(Sender: TJvDropTarget;
  var Effect: TJvDropEffect; Shift: TShiftState; X, Y: Integer);
Var
  HitInfo : THitInfo;
  Node: PVirtualNode;
  Data : PNodeDataRec;
  List: TStringList;
  FileNode : TProjectFileNode;
  Pt : TPoint;
  i: Integer;
begin
  Pt := ExplorerTree.ScreenToClient(Point(X, Y));
  ExplorerTree.GetHitTestInfoAt(Pt.X, Pt.Y, True, HitInfo);
  Node := HitInfo.HitNode;
  if not ([hiOnItemLabel, hiOnNormalIcon] * HitInfo.HitPositions <> []) then
    Node := nil;
  if Assigned(Node) then begin
    Data := ExplorerTree.GetNodeData(Node);
    if Data.ProjectNode is TProjectFilesNode then begin
      List := TStringList.Create;
      try
        Sender.GetFilenames(List);
        for i := 0 to List.Count - 1 do
          with TProjectFilesNode(Data.ProjectNode) do begin
            if DirectoryExists(List[i]) then
              ImportDirectory(List[i], '*.py', True)
            else if FileExists(List[i]) then begin
              if not Assigned(FileChild[List[i]]) then begin
                FileNode := TProjectFileNode.Create;
                FileNode.FileName := List[i];
                AddChild(FileNode);
              end;
            end;
          end;
      finally
        List.Free;
      end;
      ExplorerTree.ReinitNode(Node, True);
    end;
  end;
end;

procedure TProjectExplorerWindow.JvDropTargetDragOver(Sender: TJvDropTarget;
  var Effect: TJvDropEffect);
Var
  HitInfo : THitInfo;
  Node: PVirtualNode;
  Data : PNodeDataRec;
begin
  with ExplorerTree.ScreenToClient(Mouse.CursorPos) do
    ExplorerTree.GetHitTestInfoAt(X, Y, True, HitInfo);
  Node := HitInfo.HitNode;
  if not ([hiOnItemLabel, hiOnNormalIcon] * HitInfo.HitPositions <> []) then
    Node := nil;
  if Assigned(Node) then begin
    Data := ExplorerTree.GetNodeData(Node);
    if Data.ProjectNode is TProjectFilesNode then
      Effect := deCopy
    else
      Effect := deNone;
  end else
    Effect := deNone;
end;

end.
