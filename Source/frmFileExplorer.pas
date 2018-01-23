{-----------------------------------------------------------------------------
 Unit Name: frmFileExplorer
 Author:    Kiriakos Vlahos
 Purpose:   File Explorer Window
 History:
-----------------------------------------------------------------------------}


unit frmFileExplorer; 

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Vcl.ExtCtrls, JvComponentBase, JvDockControlForm, VirtualTrees,
  MPShellUtilities, VirtualExplorerTree, Menus, frmIDEDockWin,
  ActnList, VirtualShellHistory,  TB2Item, TB2Dock,
  TB2Toolbar, SpTBXItem, System.Actions, SpTBXControls;

const
  WM_EXPLOREHERE = WM_USER + 1000;

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
    procedure mnFavoritesPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure actNewFolderExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    fFavorites: TStringList;
    { Private declarations }
    procedure PathItemClick(Sender: TObject);
    procedure WMExploreHere(var Message: TMessage); message WM_EXPLOREHERE;
    function GetExplorerPath: string;
    procedure SetExplorerPath(const Value: string);
  public
    { Public declarations }
    procedure UpdateWindow;
    property Favorites : TStringList read fFavorites;
    property ExplorerPath : string read GetExplorerPath write SetExplorerPath;
  end;

var
  FileExplorerWindow: TFileExplorerWindow;

implementation

uses
  frmPyIDEMain, uEditAppIntfs, dmCommands, SHlObj, frmFindResults,
  MPCommonObjects, dlgDirectoryList, StringResources, cPyBaseDebugger,
  cPyScripterSettings, cFindInFiles, JvGnugettext;

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

procedure TFileExplorerWindow.CurrentDirectoryClick(Sender: TObject);
begin
  FileExplorerTree.RootFolderCustomPath := GetCurrentDir;
end;

procedure TFileExplorerWindow.ActiveScriptClick(Sender: TObject);
var
  Editor : IEditor;
  FileName : string;
begin
  Editor := PyIDEMainForm.GetActiveEditor;
  if Assigned(Editor) then begin
    FileName := Editor.FileName;
    if FileName <> '' then
      FileExplorerTree.RootFolderCustomPath := ExtractFileDir(FileName);
  end;
end;

procedure TFileExplorerWindow.mnChangeFilterClick(Sender: TObject);
begin
  PyIDEOptions.FileExplorerFilter :=
    InputBox('File Explorer Filter', 'Enter Filter:', PyIDEOptions.FileExplorerFilter);
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
  then with PyIDEMainForm do
      DoOpenFile(NameSpace.NameForParsing, '', TabControlIndex(ActiveTabControl));
end;

procedure TFileExplorerWindow.ExploreHereClick(Sender: TObject);
Var
  NameSpace : TNameSpace;
begin
  if FileExplorerTree.ValidateNamespace(FileExplorerTree.GetFirstSelected, NameSpace) and
    NameSpace.Folder
  then begin
    PostMessage(Handle, WM_EXPLOREHERE, 0, LPARAM(NameSpace.AbsolutePIDL));
  end;
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

procedure TFileExplorerWindow.WMExploreHere(var Message: TMessage);
Var
  PIDL: PItemIDList;
begin
  PIDL := PItemIDList(Message.LParam);
  if Assigned(PIDL) then begin
    FileExplorerTree.RootFolderCustomPIDL := PIDL;
    //VirtualShellHistory.Clear;
  end;
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

procedure TFileExplorerWindow.actManageFavoritesExecute(Sender: TObject);
begin
  EditFolderList(fFavorites, 'File Explorer Favorites');
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
  inherited;
  fFavorites := TStringList.Create;
  fFavorites.Duplicates := dupIgnore;
  fFavorites.Sorted := True;
end;

procedure TFileExplorerWindow.FormDestroy(Sender: TObject);
begin
  inherited;
  fFavorites.Free;
  FileExplorerWindow := nil;
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
  Paths := TStringList.Create;
  try
    PyControl.ActiveInterpreter.SysPathToStrings(Paths);
    mnPythonPath.Clear;
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

end.
