{-----------------------------------------------------------------------------
 Unit Name: frmFileExplorer
 Author:    Kiriakos Vlahos
 Purpose:   File Explorer Window
 History:
-----------------------------------------------------------------------------}


unit frmFileExplorer;

interface

uses
  System.Classes,
  System.Actions,
  System.ImageList,
  System.Messaging,
  Vcl.Controls,
  Vcl.ActnList,
  Vcl.Menus,
  Vcl.ExtCtrls,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  TB2Item,
  TB2Dock,
  TB2Toolbar,
  SpTBXItem,
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
  uEditAppIntfs,
  cPyScripterSettings,
  frmIDEDockWin;

type
  TFileExplorerWindow = class(TIDEDockWindow, IFileExplorer)
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
    mnSeparator1: TSpTBXSeparatorItem;
    mnBrowsePath: TSpTBXSubmenuItem;
    Desktop: TSpTBXItem;
    MyComputer: TSpTBXItem;
    MyDocuments: TSpTBXItem;
    CurrentDirectory: TSpTBXItem;
    mnManagePythonPath: TSpTBXItem;
    mnSeparator2: TSpTBXSeparatorItem;
    mnEnableFilter: TSpTBXItem;
    mnChangeFilter: TSpTBXItem;
    mnSeparator3: TSpTBXSeparatorItem;
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
    mnSeparator4: TMenuItem;
    AddToFavorites1: TMenuItem;
    TBXSeparatorItem6: TSpTBXSeparatorItem;
    TBXSubmenuItem2: TSpTBXSubmenuItem;
    TBXSubmenuItem3: TSpTBXSubmenuItem;
    TBXSeparatorItem4: TSpTBXSeparatorItem;
    mnNewFolder: TSpTBXItem;
    mnSeparator5: TMenuItem;
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
    FFavorites: TStringList;
    procedure PathItemClick(Sender: TObject);
    function GetExplorerPath: string;
    procedure ApplyPyIDEOptions(const Sender: TObject; const Msg:
        System.Messaging.TMessage);
    procedure SetExplorerPath(const Value: string);
    // IFileExplorer implementation
    procedure SetActive(Value: Boolean);
  public
    procedure RestoreSettings(AppStorage: TJvCustomAppStorage); override;
    procedure StoreSettings(AppStorage: TJvCustomAppStorage); override;
    procedure ConfigureThreads(FCN: TFileChangeNotificationType;
      BackgroundProcessing: Boolean);
    property Favorites: TStringList read FFavorites;
    property ExplorerPath: string read GetExplorerPath write SetExplorerPath;
    class function CreateInstance: TIDEDockWindow; override;
  end;

var
  FileExplorerWindow: TFileExplorerWindow;

implementation

uses
  Winapi.Windows,
  Winapi.ShlObj,
  System.SysUtils,
  System.IOUtils,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.FileCtrl,
  MPCommonObjects,
  JvGnugettext,
  StringResources,
  uPythonItfs,
  dmResources,
  dmCommands,
  frmFindResults,
  dlgDirectoryList,
  cFindInFiles;

{$R *.dfm}

procedure TFileExplorerWindow.FileExplorerTreeEnumFolder(
  Sender: TCustomVirtualExplorerTree; Namespace: TNamespace;
  var AllowAsChild: Boolean);
var
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
  FileExplorerTree.RootFolder := rfDesktop;
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

  if FileExplorerTree.Active then
    FileExplorerTree.RefreshTree;
end;

class function TFileExplorerWindow.CreateInstance: TIDEDockWindow;
begin
  FileExplorerWindow := TFileExplorerWindow.Create(Application);
  Result := FileExplorerWindow;
end;

procedure TFileExplorerWindow.CurrentDirectoryClick(Sender: TObject);
begin
  FileExplorerTree.RootFolderCustomPath := GetCurrentDir;
end;

procedure TFileExplorerWindow.ActiveScriptClick(Sender: TObject);
var
  Editor: IEditor;
  FileName: string;
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
var
  NameSpace: TNamespace;
begin
  if FileExplorerTree.ValidateNamespace(FileExplorerTree.GetFirstSelected, NameSpace) and
      not NameSpace.Folder and NameSpace.FileSystem
  then
    GI_PyIDEServices.ShowFilePosition(NameSpace.NameForParsing);
end;

procedure TFileExplorerWindow.ExploreHereClick(Sender: TObject);
var
  NameSpace: TNamespace;
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
var
  NameSpace: TNamespace;
begin
  if not Assigned(FindResultsWindow) then Exit;
  if FileExplorerTree.ValidateNamespace(FileExplorerTree.GetFirstSelected, NameSpace) and
    NameSpace.Folder
  then begin
    AddMRUString(NameSpace.NameForParsing,
       FindResultsWindow.FindInFilesExpert.DirList, True);
    FindResultsWindow.FindInFilesExpert.GrepSearch := gaDirGrep;  //Directory
    FindResultsWindow.Execute(False);
  end;
end;

procedure TFileExplorerWindow.ApplyPyIDEOptions(const Sender: TObject;
  const Msg: System.Messaging.TMessage);
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
  Item: TSpTBXItem;
begin
  while mnFavorites.Count > 3 do
    mnFavorites[0].Free;
  if FFavorites.Count = 0 then begin
    Item := TSpTBXItem.Create(mnFavorites);
    Item.Caption := _(SEmptyList);
    mnFavorites.Insert(0, Item);
  end else
    for var I := FFavorites.Count - 1 downto 0 do begin
      Item := TSpTBXItem.Create(mnFavorites);
      Item.Caption := FFavorites[I];
      Item.OnClick := PathItemClick;
      mnFavorites.Insert(0, Item);
    end;
end;

procedure TFileExplorerWindow.SetActive(Value: Boolean);
begin
  FileExplorerTree.Active := Value;
  if not Value then
    ConfigureThreads(fcnDisabled, False);
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
var
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
var
  NameSpace: TNamespace;
  Path: string;
begin
  if FileExplorerTree.ValidateNamespace(FileExplorerTree.GetFirstSelected, NameSpace) and
    NameSpace.Folder
  then begin
    Path := NameSpace.NameForParsing;
    if FFavorites.IndexOf(Path) < 0 then
      FFavorites.Add(Path);
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
  EditFolderList(FFavorites, _('File Explorer favorites'));
end;

procedure TFileExplorerWindow.actNewFolderExecute(Sender: TObject);
var
  SelectedNode: PVirtualNode;
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
var
  NameSpace: TNamespace;
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
  FFavorites := TStringList.Create;
  FFavorites.Duplicates := dupIgnore;
  FFavorites.Sorted := True;

  GI_FileExplorer := Self;

  TMessageManager.DefaultManager.SubscribeToMessage(TIDEOptionsChangedMessage,
    ApplyPyIDEOptions);
end;

procedure TFileExplorerWindow.FormDestroy(Sender: TObject);
begin
  GI_FileExplorer := nil;
  TMessageManager.DefaultManager.Unsubscribe(TIDEOptionsChangedMessage,
    ApplyPyIDEOptions);
  FFavorites.Free;
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
  Item: TSpTBXItem;
  Paths: TStringList;
begin
  mnPythonPath.Clear;
  if not Assigned(GI_PyControl.ActiveInterpreter) then Exit;

  Paths := TStringList.Create;
  try
    GI_PyControl.ActiveInterpreter.SysPathToStrings(Paths);
    for var Path in Paths do
      if Path <> '' then
      begin
        Item := TSpTBXItem.Create(mnPythonPath);
        Item.Caption := Path;
        Item.OnClick := PathItemClick;
        mnPythonPath.Add(Item);
      end;
  finally
    Paths.Free;
  end;
end;

procedure TFileExplorerWindow.FileExplorerTreeKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = Char(VK_RETURN) then
    FileExplorerTreeDblClick(Sender);
end;

procedure TFileExplorerWindow.VirtualShellHistoryChange(
  Sender: TBaseVirtualShellPersistent; ItemIndex: Integer;
  ChangeType: TVSHChangeType);
begin
  if not Assigned(VirtualShellHistory.VirtualExplorerTree) then Exit;
  if ChangeType = hctSelected then
  begin
      if not ILIsParent(FileExplorerTree.RootFolderNamespace.AbsolutePIDL,
        VirtualShellHistory[ItemIndex].AbsolutePIDL, False) then
      begin
         VirtualShellHistory.VirtualExplorerTree := nil;
         FileExplorerTree.RootFolderCustomPIDL :=
           VirtualShellHistory[ItemIndex].AbsolutePIDL;
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

initialization
  TIDEDockWindow.RegisterDockWinClass(ideFileExplorer, TFileExplorerWindow);
end.
