{-----------------------------------------------------------------------------
 Unit Name: frmWatches
 Author:    Kiriakos Vlahos
 Date:      09-Mar-2005
 Purpose:   Watches Window
 History:
-----------------------------------------------------------------------------}

unit frmWatches;

interface
       
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, frmIDEDockWin, JvDockControlForm, JvComponent,
  Contnrs, cPyBaseDebugger, ExtCtrls, TB2Item, TBX, TBXThemes, VirtualTrees,
  JvComponentBase, JvAppStorage, SpTBXItem;

type
  TWatchesWindow = class(TIDEDockWindow, IJvAppStorageHandler)
    TBXPopupMenu: TSpTBXPopupMenu;
    mnAddWatch: TSpTBXItem;
    mnRemoveWatch: TSpTBXItem;
    mnEditWatch: TSpTBXItem;
    mnClearall: TSpTBXItem;
    WatchesView: TVirtualStringTree;
    TBXSeparatorItem1: TSpTBXSeparatorItem;
    TBXSeparatorItem2: TSpTBXSeparatorItem;
    mnCopyToClipboard: TSpTBXItem;
    TBXItem1: TSpTBXItem;
    procedure mnCopyToClipboardClick(Sender: TObject);
    procedure mnAddWatchClick(Sender: TObject);
    procedure mnEditWatchClick(Sender: TObject);
    procedure mnRemoveWatchClick(Sender: TObject);
    procedure mnClearAllClick(Sender: TObject);
    procedure WatchesViewDblClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure WatchesViewInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure WatchesViewGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure TBXPopupMenuPopup(Sender: TObject);
    procedure WatchesViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    fWatchesList : TObjectList;
  protected
    procedure TBMThemeChange(var Message: TMessage); message TBM_THEMECHANGE;
    // IJvAppStorageHandler implementation
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
    function CreateWatch(Sender: TJvCustomAppStorage; const Path: string;
      Index: Integer): TPersistent;
  public
    { Public declarations }
    procedure UpdateWindow(DebuggerState : TDebuggerState);
    procedure AddWatch(S: string);
  end;

var
  WatchesWindow: TWatchesWindow;

implementation

uses frmPyIDEMain, PythonEngine, frmPythonII, dmCommands, uCommonFunctions,
  Clipbrd, JvDockGlobals, StringResources, gnugettext;


{$R *.dfm}

Type
  TWatchInfo = class(TPersistent)
  private
    fWatch : WideString;
  public
    Value : WideString;
  published
    property Watch : WideString read fWatch write fWatch;
  end;

  PWatchRec = ^TWatchRec;
  TWatchRec = record
    WatchInfo : TWatchInfo;
  end;

{ TWatchesWindow }

procedure TWatchesWindow.UpdateWindow(DebuggerState : TDebuggerState);
Var
  S : WideString;
  i : integer;
begin
  // Exit if there are no wathces
  if fWatchesList.Count <= 0 then Exit;
  // Clear values
  for i := 0 to fWatchesList.Count - 1 do
    TWatchInfo(fWatchesList[i]).Value := _(SNotAvailable);
  // Exit if Debugger is not in Stopped state
  if DebuggerState in [dsPaused, dsPostMortem] then
    for i := 0 to fWatchesList.Count - 1 do begin
      PyControl.ActiveDebugger.Evaluate(TWatchInfo(fWatchesList[i]).Watch,
        S, TWatchInfo(fWatchesList[i]).Value);
    end;
  WatchesView.Clear;
  WatchesView.RootNodeCount := fWatchesList.Count;
end;

procedure TWatchesWindow.AddWatch(S: string);
var
  WatchInfo: TWatchInfo;
begin
  WatchInfo := TWatchInfo.Create;
  WatchInfo.Watch := S;
  fWatchesList.Add(WatchInfo);
  UpdateWindow(PyControl.DebuggerState);
end;

procedure TWatchesWindow.mnAddWatchClick(Sender: TObject);
Var
  S : string;
begin
  S := InputBox('Add Watch', 'Enter watch expression:', '');
  if S <> '' then begin
    AddWatch(S);
  end;
end;

procedure TWatchesWindow.mnEditWatchClick(Sender: TObject);
Var
  Node : PVirtualNode;
  WatchInfo : TWatchInfo;
begin
  Node := WatchesView.GetFirstSelected();
  if Assigned(Node) then begin
    WatchInfo := PWatchRec(WatchesView.GetNodeData(Node))^.WatchInfo;
    WatchInfo.Watch := InputBox('Edit Watch', 'Enter new expression:', WatchInfo.Watch);
    if WatchInfo.Watch = '' then fWatchesList.Remove(WatchInfo);
    UpdateWindow(PyControl.DebuggerState);
  end;
end;

procedure TWatchesWindow.mnRemoveWatchClick(Sender: TObject);
Var
  Node : PVirtualNode;
  WatchInfo : TWatchInfo;
begin
  Node := WatchesView.GetFirstSelected();
  if Assigned(Node) then begin
    WatchInfo := PWatchRec(WatchesView.GetNodeData(Node))^.WatchInfo;
    WatchesView.Clear;
    fWatchesList.Remove(WatchInfo);
    UpdateWindow(PyControl.DebuggerState);
  end;
end;

procedure TWatchesWindow.mnClearAllClick(Sender: TObject);
begin
  fWatchesList.Clear;
  WatchesView.Clear;
end;

procedure TWatchesWindow.WatchesViewDblClick(Sender: TObject);
Var
  Pt : TPoint;
  HitInfo: THitInfo;
begin
  try
    Pt := Mouse.CursorPos;
    Pt := WatchesView.ScreenToClient(Pt);
    WatchesView.GetHitTestInfoAt(Pt.X, Pt.Y, True, HitInfo);
  except
    HitInfo.HitNode := nil;
  end;
  if Assigned(HitInfo.HitNode) then
    mnEditWatchClick(Sender)
  else
    mnAddWatchClick(Sender);
end;

procedure TWatchesWindow.mnCopyToClipboardClick(Sender: TObject);
begin
  Clipboard.AsText := WatchesView.ContentToText(tstAll, #9);
end;

procedure TWatchesWindow.FormActivate(Sender: TObject);
begin
  inherited;
  if WatchesView.CanFocus then
    WatchesView.SetFocus;
  //PostMessage(WatchesView.Handle, WM_SETFOCUS, 0, 0);
end;

procedure TWatchesWindow.TBMThemeChange(var Message: TMessage);
begin
  inherited;
  if Message.WParam = TSC_VIEWCHANGE then begin
    WatchesView.Header.Invalidate(nil, True);
    WatchesView.Colors.HeaderHotColor :=
      CurrentTheme.GetItemTextColor(GetItemInfo('active'));
  end;
end;

procedure TWatchesWindow.FormCreate(Sender: TObject);
begin
  inherited;
  fWatchesList := TObjectList.Create(True);  // Onwns objects
  // Let the tree know how much data space we need.
  WatchesView.NodeDataSize := SizeOf(TWatchRec);
  WatchesView.OnAdvancedHeaderDraw :=
    CommandsDataModule.VirtualStringTreeAdvancedHeaderDraw;
  WatchesView.OnHeaderDrawQueryElements :=
    CommandsDataModule.VirtualStringTreeDrawQueryElements;
end;

procedure TWatchesWindow.FormDestroy(Sender: TObject);
begin
  fWatchesList.Free;
  inherited;
end;

procedure TWatchesWindow.WatchesViewInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
begin
  Assert(WatchesView.GetNodeLevel(Node) = 0);
  Assert(Integer(Node.Index) < fWatchesList.Count);
  PWatchRec(WatchesView.GetNodeData(Node))^.WatchInfo :=
    fWatchesList[Node.Index] as TWatchInfo;
end;

procedure TWatchesWindow.WatchesViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if Key = VK_Delete then begin
    mnRemoveWatchClick(Sender);
    Key := 0;
  end;
end;

procedure TWatchesWindow.WatchesViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
begin
  Assert(WatchesView.GetNodeLevel(Node) = 0);
  Assert(Integer(Node.Index) < fWatchesList.Count);
  with PWatchRec(WatchesView.GetNodeData(Node))^.WatchInfo do
    case Column of
      0:  CellText := Watch;
      1:  CellText := Value;
    end;
end;

procedure TWatchesWindow.TBXPopupMenuPopup(Sender: TObject);
begin
  mnRemoveWatch.Enabled := Assigned(WatchesView.GetFirstSelected());
  mnEditWatch.Enabled := mnRemoveWatch.Enabled;
  mnClearAll.Enabled := fWatchesList.Count > 0;
  mnCopyToClipboard.Enabled := fWatchesList.Count > 0;
end;

procedure TWatchesWindow.WriteToAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
begin
  AppStorage.WriteObjectList(BasePath, fWatchesList, 'Watch');
  AppStorage.WriteInteger(BasePath+'\WatchesWidth', WatchesView.Header.Columns[0].Width);
end;

procedure TWatchesWindow.ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
begin
  mnClearAllClick(Self);
  AppStorage.ReadObjectList(BasePath, fWatchesList, CreateWatch, True, 'Watch');
  WatchesView.Header.Columns[0].Width := AppStorage.ReadInteger(BasePath+'\WatchesWidth', 200);
  UpdateWindow(PyControl.DebuggerState);
end;

function TWatchesWindow.CreateWatch(Sender: TJvCustomAppStorage;
  const Path: string; Index: Integer): TPersistent;
begin
  Result := TWatchInfo.Create;
end;

end.


