{-----------------------------------------------------------------------------
 Unit Name: frmFileExplorer
 Author:    Kiriakos Vlahos
 Purpose:   File Explorer Window
 History:
-----------------------------------------------------------------------------}


unit frmFileExplorer;

interface

uses
  WinApi.Windows,
  WinApi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Actions,
  System.ImageList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ActnList,
  Vcl.Menus,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  TB2Item,
  TB2Dock,
  TB2Toolbar,
  SpTBXItem,
  SpTBXControls,
  JvComponentBase,
  JvDockControlForm,
  JvAppStorage,
  VirtualTrees.BaseAncestorVCL,
  VirtualTrees.AncestorVCL,
  VirtualTrees.BaseTree,
  VirtualTrees,
  VirtualExplorerTree,
  VirtualShellHistory,
  MPShellUtilities,
  cPyScripterSettings,
  frmIDEDockWin;

type
  TFileExplorerWindow = class(TIDEDockWindow)
    FileExplorerTree: TVirtualExplorerTree;
    VirtualShellHistory: TVirtualShellHistory;
    ExplorerDock: TSpTBXDock;
    ExplorerToolbar: TSpTBXToolbar;
    tbiGoUp: TSpTBXItem;
    tbiEnableFilter: TSpTBXItem;
    TBXSubmenuItem1: TSpTBXSubmenuItem;
    tbiPythonPath: TSpTBXItem;
    tbiItemBack: TSpTBXSubmenuItem;
    tbiItemForward: TSpTBXSubmenuItem;
    TBXSeparatorItem1: TSpTBXSeparatorItem;
    ExplorerPopUp: TSpTBXPopupMenu;
    mnBack: TSpTBXItem;
    mnForward: TSpTBXItem;
    mnGoUp: TSpTBXItem;
    N1: TSpTBXSeparatorItem;
    mnBrowsePath: TSpTBXSubmenuItem;
    Desktop: TSpTBXItem;
    MyComputer: TSpTBXItem;
    MyDocuments: TSpTBXItem;
    CurrentDirectory: TSpTBXItem;
    mnManagePythonPath: TSpTBXItem;
    N2: TSpTBXSeparatorItem;
    mnEnableFilter: TSpTBXItem;
    mnChangeFilter: TSpTBXItem;
    N3: TSpTBXSeparatorItem;
    mnRefresh: TSpTBXItem;
    mnPythonPath: TSpTBXSubmenuItem;
    ShellContextPopUp: TPopupMenu;
    tbiBrowsePath: TSpTBXSubmenuItem;
    ExploreHere: TMenuItem;
    SearchPath1: TMenuItem;
    TBXSeparatorItem2: TSpTBXSeparatorItem;
    tbiSearchPath: TSpTBXItem;
    ActiveScript: TSpTBXItem;
    TBXSeparatorItem3: TSpTBXSeparatorItem;
    mnFavorites: TSpTBXSubmenuItem;
    TBXItem2: TSpTBXItem;
    TBXItem7: TSpTBXItem;
    TBXSeparatorItem5: TSpTBXSeparatorItem;
    N4: TMenuItem;
    AddToFavorites1: TMenuItem;
    TBXSeparatorItem6: TSpTBXSeparatorItem;
    TBXSubmenuItem2: TSpTBXSubmenuItem;
    TBXSubmenuItem3: TSpTBXSubmenuItem;
    TBXSeparatorItem4: TSpTBXSeparatorItem;
    mnNewFolder: TSpTBXItem;
    N5: TMenuItem;
    CreateNewFolder1: TMenuItem;
    tbiNewFolder: TSpTBXItem;
    FileExplorerActions: TActionList;
    actNewFolder: TAction;
    actAddToFavorites: TAction;
    actManageFavorites: TAction;
    actExploreHere: TAction;
    actSearchPath: TAction;
    actEnableFilter: TAction;
    actRefresh: TAction;
    actGoUp: TAction;
    actGoForward: TAction;
    actGoBack: TAction;
    vilImages: TVirtualImageList;
    actGoToCustomPath: TAction;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    SpTBXItem1: TSpTBXItem;
    procedure VirtualShellHistoryChange(Sender: TBaseVirtualShellPersistent;
      ItemIndex: Integer; ChangeType: TVSHChangeType);
    procedure FileExplorerTreeKeyPress(Sender: TObject; var Key: Char);
    procedure ActiveScriptClick(Sender: TObject);
    procedure FileExplorerTreeEnumFolder(
      Sender: TCustomVirtualExplorerTree; Namespace: TNamespace;
      var AllowAsChild: Boolean);
    procedure DesktopClick(Sender: TObject);
    procedure MyComputerClick(Sender: TObject);
    procedure MyDocumentsClick(Sender: TObject);
    procedure CurrentDirectoryClick(Sender: TObject);
    procedure mnChangeFilterClick(Sender: TObject);
    procedure FileExplorerTreeDblClick(Sender: TObject);
    procedure ExploreHereClick(Sender: TObject);
    procedure actGoBackExecute(Sender: TObject);
    procedure FileExplorerActionsUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure actGoForwardExecute(Sender: TObject);
    procedure actGoUpExecute(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actEnableFilterExecute(Sender: TObject);
    procedure tbiItemBackPopup(Sender: TTBCustomItem;
      FromLink: Boolean);
    procedure tbiItemForwardPopup(Sender: TTBCustomItem;
      FromLink: Boolean);
    procedure BrowsePathPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure actSearchPathExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actManageFavoritesExecute(Sender: TObject);
    procedure actAddToFavoritesExecute(Sender: TObject);
    procedure actGoToCustomPathExecute(Sender: TObject);
    procedure mnFavoritesPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure actNewFolderExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    fFavorites: TStringList;
    { Private declarations }
    procedure PathItemClick(Sender: TObject);
    function GetExplorerPath: string;
    procedure ApplyPyIDEOptions;
    procedure SetExplorerPath(const Value: string);
  public
    { Public declarations }
    procedure UpdateWindow;
    procedure RestoreSettings(AppStorage: TJvCustomAppStorage); override;
    procedure StoreSettings(AppStorage: TJvCustomAppStorage); override;
    procedure ConfigureThreads(FCN : TFileChangeNotificationType;
      BackgroundProcessing : Boolean);
    property Favorites : TStringList read fFavorites;
    property ExplorerPath : string read GetExplorerPath write SetExplorerPath;
  end;

var
  FileExplorerWindow: TFileExplorerWindow;

implementation

uses
  WinApi.SHlObj,
  System.IOUtils,
  Vcl.FileCtrl,
  MPCommonObjects,
  JvGnugettext,
  StringResources,
  dmResources,
  dmCommands,
  frmFindResults,
  dlgDirectoryList,
  uEditAppIntfs,
  cPyBaseDebugger,
  cFindInFiles,
  cPyControl;

{$R *.dfm}

procedure TFileExplorerWindow.UpdateWindow;
begin
end;

procedure TFileExplorerWindow.FileExplorerTreeEnumFolder(
  Sender: TCustomVirtualExplorerTree; Namespace: TNamespace;
  var AllowAsChild: Boolean);
Var
  FileExt: string;
begin
  AllowAsChild := True;
  if actEnableFilter.Checked and not Namespace.Folder  { Don't filter folders } then begin
    FileExt := UpperCase(ExtractFileExt(Namespace.NameParseAddress));
    if Pos(FileExt, UpperCase(PyIDEOptions.FileExplorerFilter)) = 0 then
      AllowAsChild := False;
  end;
end;

procedure TFileExplorerWindow.DesktopClick(Sender: TObject);
begin
  FileExplorerTree.RootFolder := rfDeskTop;
end;

procedure TFileExplorerWindow.MyComputerClick(Sender: TObject);
begin
  FileExplorerTree.RootFolder := rfDrives;
end;

procedure TFileExplorerWindow.MyDocumentsClick(Sender: TObject);
begin
  FileExplorerTree.RootFolder := rfPersonal;
end;

procedure TFileExplorerWindow.ConfigureThreads(FCN: TFileChangeNotificationType;
  BackgroundProcessing: Boolean);
begin
  with FileExplorerTree.TreeOptions do
  begin
    case FCN of
      fcnFull:
        VETMiscOptions := VETMiscOptions +
          [toChangeNotifierThread, toTrackChangesInMappedDrives];
      fcnNoMappedDrives:
        VETMiscOptions := VETMiscOptions +
          [toChangeNotifierThread] - [toTrackChangesInMappedDrives];
      fcnDisabled:
        VETMiscOptions := VETMiscOptions -
          [toChangeNotifierThread, toTrackChangesInMappedDrives];
     end;

    if BackgroundProcessing then begin
      VETImageOptions := VETImageOptions + [toThreadedImages];
      VETFolderOptions := VETFolderOptions + [toThreadedExpandMark];
    end else begin
      VETImageOptions := VETImageOptions - [toThreadedImages];
      VETFolderOptions := VETFolderOptions - [toThreadedExpandMark];
    end;
  end;

  // Connect ChangeNotify
  if FCN = fcnDisabled then
    FileExplorerTree.OnAfterShellNotify := nil
  else
    FileExplorerTree.OnAfterShellNotify := CommandsDataModule.ProcessShellNotify;

  if FileExplorerTree.Active then
    FileExplorerTree.RefreshTree;
end;

procedure TFileExplorerWindow.CurrentDirectoryClick(Sender: TObject);
begin
  FileExplorerTree.RootFolderCustomPath := GetCurrentDir;
end;

procedure TFileExplorerWindow.ActiveScriptClick(Sender: TObject);
var
  Editor : IEditor;
  FileName : string;
begin
  Editor := GI_PyIDEServices.ActiveEditor;
  if Assigned(Editor) then begin
    FileName := Editor.FileName;
    if FileName <> '' then
      FileExplorerTree.RootFolderCustomPath := TPath.GetDirectoryName(FileName);
  end;
end;

procedure TFileExplorerWindow.mnChangeFilterClick(Sender: TObject);
begin
  PyIDEOptions.FileExplorerFilter :=
    InputBox(_('File Explorer Filter'), _('Enter Filter:'), PyIDEOptions.FileExplorerFilter);
  FileExplorerTree.RefreshTree;
end;

procedure TFileExplorerWindow.PathItemClick(Sender: TObject);
begin
  if Sender is TTBCustomItem then
    FileExplorerTree.RootFolderCustomPath := TTBCustomItem(Sender).Caption;
end;

procedure TFileExplorerWindow.FileExplorerTreeDblClick(Sender: TObject);
Var
  NameSpace : TNameSpace;
begin
  if FileExplorerTree.ValidateNamespace(FileExplorerTree.GetFirstSelected, NameSpace) and
      not NameSpace.Folder and NameSpace.FileSystem
  then
    GI_PyIDEServices.ShowFilePosition(NameSpace.NameForParsing);
end;

procedure TFileExplorerWindow.ExploreHereClick(Sender: TObject);
Var
  NameSpace : TNameSpace;
begin
  if FileExplorerTree.ValidateNamespace(FileExplorerTree.GetFirstSelected, NameSpace) and
    NameSpace.Folder and Assigned(NameSpace.AbsolutePIDL)
  then
    TThread.ForceQueue(nil, procedure
      begin
        FileExplorerTree.RootFolderCustomPIDL := NameSpace.AbsolutePIDL;
      end);
end;

procedure TFileExplorerWindow.actSearchPathExecute(Sender: TObject);
Var
  NameSpace : TNameSpace;
begin
  if not Assigned(FindResultsWindow) then Exit;
  if FileExplorerTree.ValidateNamespace(FileExplorerTree.GetFirstSelected, NameSpace) and
    NameSpace.Folder
  then begin
    AddMRUString(NameSpace.NameForParsing,
       FindResultsWindow.FindInFilesExpert.DirList, True);
    FindResultsWindow.FindInFilesExpert.GrepSearch := 3;  //Directory
    FindResultsWindow.Execute(False)
  end;
end;

procedure TFileExplorerWindow.ApplyPyIDEOptions;
begin
  TThread.ForceQueue(nil, procedure
  begin
    ConfigureThreads(PyIDEOptions.FileChangeNotification,
      PyIDEOptions.FileExplorerBackgroundProcessing);
  end);

end;

function TFileExplorerWindow.GetExplorerPath: string;
begin
  Result := '';
  if Assigned (FileExplorerTree.RootFolderNamespace) then
    with FileExplorerTree.RootFolderNamespace do
      if not (IsDesktop or IsMyComputer or not FileSystem) then
        Result := NameForParsing;
end;

procedure TFileExplorerWindow.mnFavoritesPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
var
  i : integer;
  Item : TSpTBXItem;
begin
  while mnFavorites.Count > 3 do
    mnFavorites.Items[0].Free;
  if fFavorites.Count = 0 then begin
    Item := TSpTBXItem.Create(mnFavorites);
    Item.Caption := _(SEmptyList);
    mnFavorites.Insert(0, Item);
  end else
    for i := fFavorites.Count - 1 downto 0 do begin
      Item := TSpTBXItem.Create(mnFavorites);
      Item.Caption := fFavorites[i];
      Item.OnClick := PathItemClick;
      mnFavorites.Insert(0, Item);
    end;
end;

procedure TFileExplorerWindow.SetExplorerPath(const Value: string);
begin
  try
    FileExplorerTree.RootFolderCustomPath := Value;
  except
    FileExplorerTree.RootFolder := rfDrives;
  end;
  //VirtualShellHistory.Clear;
end;

procedure TFileExplorerWindow.actGoBackExecute(Sender: TObject);
begin
  VirtualShellHistory.Back;
end;

procedure TFileExplorerWindow.actGoForwardExecute(Sender: TObject);
begin
  VirtualShellHistory.Next;
end;

procedure TFileExplorerWindow.actGoUpExecute(Sender: TObject);
Var
  PIDL: PItemIDList;
begin
  if not Assigned(FileExplorerTree.RootFolderNamespace) or
    FileExplorerTree.RootFolderNamespace.IsDesktop then Exit;  // No parent!

  PIDL := PIDLMgr.CopyPIDL(FileExplorerTree.RootFolderNamespace.AbsolutePIDL);
  try
    PIDLMgr.StripLastID(PIDL);
    FileExplorerTree.RootFolderCustomPIDL := PIDL;
  finally
    PIDLMgr.FreePIDL(PIDL);
  end;
end;

procedure TFileExplorerWindow.actAddToFavoritesExecute(Sender: TObject);
Var
  NameSpace : TNameSpace;
  Path : string;
begin
  if FileExplorerTree.ValidateNamespace(FileExplorerTree.GetFirstSelected, NameSpace) and
    NameSpace.Folder
  then begin
    Path := NameSpace.NameForParsing;
    if fFavorites.IndexOf(Path) < 0 then
      fFavorites.Add(Path);
  end;
end;

procedure TFileExplorerWindow.actEnableFilterExecute(Sender: TObject);
begin
  FileExplorerTree.RefreshTree;
end;

procedure TFileExplorerWindow.actGoToCustomPathExecute(Sender: TObject);
var
   Directories: TArray<string>;
begin
  if SelectDirectory('', Directories, [], _('Select directory')) then
    FileExplorerTree.RootFolderCustomPath := Directories[0];
end;

procedure TFileExplorerWindow.actManageFavoritesExecute(Sender: TObject);
begin
  EditFolderList(fFavorites, _('File Explorer favorites'));
end;

procedure TFileExplorerWindow.actNewFolderExecute(Sender: TObject);
Var
  SelectedNode : PVirtualNode;
begin
  SelectedNode := FileExplorerTree.GetFirstSelected;
  if Assigned(SelectedNode) then
    FileExplorerTree.CreateNewFolderByNode(SelectedNode);
end;

procedure TFileExplorerWindow.actRefreshExecute(Sender: TObject);
begin
  FileExplorerTree.RefreshTree(True);
end;

procedure TFileExplorerWindow.FileExplorerActionsUpdate(
  Action: TBasicAction; var Handled: Boolean);
Var
  NameSpace : TNameSpace;
begin
  actSearchPath.Enabled :=
    FileExplorerTree.ValidateNamespace(FileExplorerTree.GetFirstSelected, NameSpace) and
    NameSpace.Folder;
  actAddToFavorites.Enabled := actSearchPath.Enabled;
  actNewFolder.Enabled := actSearchPath.Enabled;
  actExploreHere.Enabled := actSearchPath.Enabled;

  actGoBack.Enabled := VirtualShellHistory.ItemIndex > 0;
  actGoForward.Enabled := VirtualShellHistory.ItemIndex < VirtualShellHistory.Count-1;
end;

procedure TFileExplorerWindow.FormActivate(Sender: TObject);
begin
  inherited;
  if FileExplorerTree.CanFocus then
    FileExplorerTree.SetFocus;
end;

procedure TFileExplorerWindow.FormCreate(Sender: TObject);
begin
  ImageName := 'FileExplorer';
  inherited;
  fFavorites := TStringList.Create;
  fFavorites.Duplicates := dupIgnore;
  fFavorites.Sorted := True;
  PyIDEOptions.OnChange.AddHandler(ApplyPyIDEOptions);
end;

procedure TFileExplorerWindow.FormDestroy(Sender: TObject);
begin
  PyIDEOptions.OnChange.RemoveHandler(ApplyPyIDEOptions);
  fFavorites.Free;
  FileExplorerWindow := nil;
  inherited;
end;

procedure TFileExplorerWindow.tbiItemBackPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
begin
  VirtualShellHistory.FillPopupMenu_TB2000(tbiItemBack, TSpTBXItem, fpdNewestToOldest);
end;

procedure TFileExplorerWindow.tbiItemForwardPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
begin
  VirtualShellHistory.FillPopupMenu_TB2000(tbiItemForward, TSpTBXItem, fpdOldestToNewest);
end;

procedure TFileExplorerWindow.BrowsePathPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
var
  i : integer;
  Item : TSpTBXItem;
  Paths : TStringList;
begin
  mnPythonPath.Clear;
  if not (Assigned(PyControl) and Assigned(PyControl.ActiveInterpreter)) then Exit;

  Paths := TStringList.Create;
  try
    PyControl.ActiveInterpreter.SysPathToStrings(Paths);
    for i := 0 to Paths.Count - 1  do begin
      if Paths[i] <> '' then begin
        Item := TSpTBXItem.Create(mnPythonPath);
        Item.Caption := Paths[i];
        Item.OnClick := PathItemClick;
        mnPythonPath.Add(Item);
      end;
    end;
  finally
    Paths.Free;
  end;
end;

procedure TFileExplorerWindow.FileExplorerTreeKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = Char(VK_Return) then
    FileExplorerTreeDblClick(Sender);
end;

procedure TFileExplorerWindow.VirtualShellHistoryChange(
  Sender: TBaseVirtualShellPersistent; ItemIndex: Integer;
  ChangeType: TVSHChangeType);
begin
  if not Assigned(VirtualShellHistory.VirtualExplorerTree) then exit;
  if ChangeType = hctSelected then begin
      if not ILIsParent(FileExplorerTree.RootFolderNamespace.AbsolutePIDL,
        VirtualShellHistory.Items[ItemIndex].AbsolutePIDL, False) then begin
         VirtualShellHistory.VirtualExplorerTree := nil;
         FileExplorerTree.RootFolderCustomPIDL :=
           VirtualShellHistory.Items[ItemIndex].AbsolutePIDL;
         VirtualShellHistory.VirtualExplorerTree := FileExplorerTree;
      end;
  end;
end;

procedure TFileExplorerWindow.RestoreSettings(AppStorage: TJvCustomAppStorage);
begin
  inherited;
  actEnableFilter.Checked := AppStorage.ReadBoolean('File Explorer Filter', True);
  ExplorerPath := AppStorage.ReadString('File Explorer Path');
  AppStorage.ReadStringList('File Explorer Favorites', Favorites);
end;

procedure TFileExplorerWindow.StoreSettings(AppStorage: TJvCustomAppStorage);
begin
  inherited;
  AppStorage.WriteBoolean('File Explorer Filter', actEnableFilter.Checked);
  AppStorage.WriteString('File Explorer Path', ExplorerPath);
  AppStorage.WriteStringList('File Explorer Favorites', Favorites);
end;

end.
