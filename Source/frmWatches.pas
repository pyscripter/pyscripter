{ -----------------------------------------------------------------------------
  Unit Name: frmWatches
  Author:    Kiriakos Vlahos
  Date:      09-Mar-2005
  Purpose:   Watches Window
  History:
  ----------------------------------------------------------------------------- }

unit frmWatches;

interface

uses
  WinApi.Windows,
  WinApi.Messages,
  WinApi.ActiveX,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Contnrs,
  System.ImageList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Menus,
  Vcl.ExtCtrls,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  frmIDEDockWin,
  JvComponentBase,
  JvAppStorage,
  JvDockControlForm,
  TB2Item,
  SpTBXSkins,
  SpTBXControls,
  SpTBXItem,
  VirtualTrees.Types,
  VirtualTrees.BaseAncestorVCL,
  VirtualTrees.AncestorVCL,
  VirtualTrees.BaseTree,
  VirtualTrees,
  cPyControl;

type
  TWatchesWindow = class(TIDEDockWindow)
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
    Panel1: TPanel;
    vilCodeImages: TVirtualImageList;
    vilImages: TVirtualImageList;
    procedure mnCopyToClipboardClick(Sender: TObject);
    procedure mnAddWatchClick(Sender: TObject);
    procedure mnEditWatchClick(Sender: TObject);
    procedure mnRemoveWatchClick(Sender: TObject);
    procedure mnClearAllClick(Sender: TObject);
    procedure WatchesViewDblClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure WatchesViewInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure WatchesViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TBXPopupMenuPopup(Sender: TObject);
    procedure WatchesViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure WatchesViewDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
      var Effect: Integer; var Accept: Boolean);
    procedure WatchesViewDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure WatchesViewGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure WatchesViewInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure WatchesViewFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    const FBasePath = 'Watches'; // Used for storing settings
    var fWatchesList: TObjectList;
  protected
    function CreateWatch(Sender: TJvCustomAppStorage; const Path: string;
      Index: Integer): TPersistent;
  public
    // AppStorage
    procedure StoreSettings(AppStorage: TJvCustomAppStorage); override;
    procedure RestoreSettings(AppStorage: TJvCustomAppStorage); override;

    procedure UpdateWindow(DebuggerState: TDebuggerState);
    procedure AddWatch(S: string);
  end;

var
  WatchesWindow: TWatchesWindow;

implementation

uses
  Vcl.Clipbrd,
  SynEdit,
  JvGnugettext,
  StringResources,
  uEditAppIntfs,
  uCommonFunctions,
  dmResources,
  frmPyIDEMain,
  frmCallStack,
  cPySupportTypes,
  cPyBaseDebugger;

{$R *.dfm}

Type
  TWatchInfo = class(TPersistent)
  private
    fWatch: string;
    fNS: TBaseNameSpaceItem;
  public
    destructor Destroy; override;
  published
    property Watch: string read fWatch write fWatch;
  end;

  PWatchRec = ^TWatchRec;
  TWatchRec = record
    Name : string;
    ObjectType : string;
    Value : string;
    ImageIndex : integer;
    NS: TBaseNameSpaceItem;
  end;

destructor TWatchInfo.Destroy;
begin
  if Assigned(fNS) then
  begin
    var Py := GI_PyControl.SafePyEngine;
    FreeAndNil(fNS);
  end;
  inherited;
end;

{ TWatchesWindow }

procedure TWatchesWindow.FormCreate(Sender: TObject);
begin
  ImageName := 'WatchesWin';
  inherited;
//  AllocConsole;
  fWatchesList := TObjectList.Create(True); // Onwns objects
  // Let the tree know how much data space we need.
  WatchesView.NodeDataSize := SizeOf(TWatchRec);
end;

procedure TWatchesWindow.WatchesViewInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
var
  Data, ParentData: PWatchRec;
begin
  Data := Node.GetData;

  if WatchesView.GetNodeLevel(Node) = 0 then
  begin
    Assert(Integer(Node.Index) < fWatchesList.Count);
    if Assigned(Data.NS) then
    begin
      var Py := GI_PyControl.SafePyEngine;
      ChildCount := Data.NS.ChildCount
    end
    else
      ChildCount := 0;
  end
  else
  begin
    var Py := GI_PyControl.SafePyEngine;
    ParentData := Node.Parent.GetData;
    Assert(Assigned(ParentData.NS));
    Data.NS := ParentData.NS.ChildNode[Node.Index];
    ChildCount := Data.NS.ChildCount;
  end;
end;

procedure TWatchesWindow.WatchesViewInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Data, ParentData: PWatchRec;
  ChildCount : integer;
begin
  Data := Node.GetData;
  if not WatchesView.Enabled then
  begin
    Data.NS := nil;
    ChildCount := 0;
  end
  else
  if WatchesView.GetNodeLevel(Node) = 0 then
  begin
    Assert(Integer(Node.Index) < fWatchesList.Count);
    Data.NS := TWatchInfo(fWatchesList[Node.Index]).fNS;
    if Assigned(Data.NS) then
    begin
      var Py := GI_PyControl.SafePyEngine;
      ChildCount := Data.NS.ChildCount
    end else
      ChildCount := 0;
  end
  else
  begin
    var Py := GI_PyControl.SafePyEngine;
    ParentData := ParentNode.GetData;
    Assert(Assigned(ParentData.NS));
    Data.NS := ParentData.NS.ChildNode[Node.Index];
    ChildCount := Data.NS.ChildCount;
  end;
  if ChildCount > 0 then
    InitialStates := [ivsHasChildren]
  else
    InitialStates := [];

  // Node Text
  if Assigned(Data.NS) then begin
     var Py := GI_PyControl.SafePyEngine;
     Data.Name := Data.NS.Name;
     Data.ObjectType := Data.NS.ObjectType;
     Data.Value := Data.NS.Value;
  end else begin
    if WatchesView.GetNodeLevel(Node) = 0 then
      Data.Name  := TWatchInfo(fWatchesList[Node.Index]).Watch
    else
      Data.Name := _(SNotAvailable);
    Data.ObjectType := _(SNotAvailable);
    Data.Value := _(SNotAvailable);
  end;

  // ImageIndex
  if Assigned(Data.NS) then begin
    if Data.NS.IsDict then
      Data.ImageIndex := Ord(TCodeImages.Namespace)
    else if Data.NS.IsModule then
      Data.ImageIndex := Ord(TCodeImages.Module)
    else if Data.NS.IsMethod then
      Data.ImageIndex := Ord(TCodeImages.Method)
    else if Data.NS.IsFunction then
      Data.ImageIndex := Ord(TCodeImages.Func)
    else if Data.NS.IsClass or Data.NS.Has__dict__ then
      Data.ImageIndex := Ord(TCodeImages.Klass)
    else if (Data.ObjectType = 'list') or (Data.ObjectType = 'tuple') then
      Data.ImageIndex := Ord(TCodeImages.List)
    else begin
      if Assigned(ParentNode) and
        (PWatchRec(ParentNode.GetData).NS.IsDict
          or PWatchRec(ParentNode.GetData).NS.IsModule)
      then
        Data.ImageIndex := Ord(TCodeImages.Variable)
      else
        Data.ImageIndex := Ord(TCodeImages.Field);
    end;
  end else
    Data.ImageIndex := -1;
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
var
  S: string;
begin
  S := InputBox('Add Watch', 'Enter watch expression:', '');
  if S <> '' then
  begin
    AddWatch(S);
  end;
end;

procedure TWatchesWindow.mnEditWatchClick(Sender: TObject);
var
  Node: PVirtualNode;
  WatchInfo: TWatchInfo;
begin
  Node := WatchesView.GetFirstSelected();

  if Assigned(Node) then
  begin
    while Assigned(Node.Parent) and (Node.Parent <> WatchesView.RootNode) do
      Node := Node.Parent;
    WatchInfo := fWatchesList[Node.Index] as TWatchInfo;
    WatchInfo.Watch := InputBox(_('Edit Watch'), _('Enter new expression:'), WatchInfo.Watch);
    if WatchInfo.Watch = '' then
      fWatchesList.Remove(WatchInfo);
    UpdateWindow(PyControl.DebuggerState);
  end;
end;

procedure TWatchesWindow.mnRemoveWatchClick(Sender: TObject);
var
  Node: PVirtualNode;
  WatchInfo: TWatchInfo;
begin
  Node := WatchesView.GetFirstSelected();

  if Assigned(Node) then
  begin
    while Assigned(Node.Parent) and (Node.Parent <> WatchesView.RootNode) do
      Node := Node.Parent;
    WatchInfo := fWatchesList[Node.Index] as TWatchInfo;
    fWatchesList.Remove(WatchInfo);
    WatchesView.Clear;
    UpdateWindow(PyControl.DebuggerState);
  end;
end;

procedure TWatchesWindow.mnClearAllClick(Sender: TObject);
begin
  fWatchesList.Clear;
  WatchesView.Clear;
end;

procedure TWatchesWindow.WatchesViewDblClick(Sender: TObject);
var
  Pt: TPoint;
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

procedure TWatchesWindow.WatchesViewDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
begin
  if (Source is TCustomSynEdit) and TCustomSynEdit(Source).SelAvail then
  begin
    AddWatch(TCustomSynEdit(Source).SelText);
  end;
end;

procedure TWatchesWindow.WatchesViewDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
  Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
begin
  Accept := Source is TSynEdit;
end;

procedure TWatchesWindow.WatchesViewFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data : PWatchRec;
begin
  Data := Node.GetData;
  Finalize(Data^);
end;

procedure TWatchesWindow.mnCopyToClipboardClick(Sender: TObject);
begin
  Clipboard.AsText := string(WatchesView.ContentToText(tstAll, #9));
end;

procedure TWatchesWindow.FormActivate(Sender: TObject);
begin
  inherited;
  if CanActuallyFocus(WatchesView) then
    WatchesView.SetFocus;
  //PostMessage(WatchesView.Handle, WM_SETFOCUS, 0, 0);
end;

procedure TWatchesWindow.FormDestroy(Sender: TObject);
begin
  fWatchesList.Free;
  inherited;
end;

procedure TWatchesWindow.WatchesViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if Key = VK_Delete then
  begin
    mnRemoveWatchClick(Sender);
    Key := 0;
  end;
end;

procedure TWatchesWindow.WatchesViewGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  Data: PWatchRec;
begin
  if (Column = 0) and (Kind in [ikNormal, ikSelected]) then begin
    Data := Node.GetData;
    ImageIndex := Data.ImageIndex;
  end;
end;

procedure TWatchesWindow.WatchesViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data: PWatchRec;
begin
  if TextType <> ttNormal then Exit;
  Data := Node.GetData;
  if not Assigned(Data) then Exit;

  case Column of
    0: CellText := Data.Name;
    1: CellText := Data.ObjectType;
    2: CellText := Data.Value;
  end
end;

procedure TWatchesWindow.UpdateWindow(DebuggerState: TDebuggerState);
Var
  i: Integer;
begin
  if not GI_PyControl.PythonLoaded or GI_PyControl.Running then begin
    WatchesView.Enabled := False;
    Exit;
  end else
    WatchesView.Enabled := True;

  var Py := GI_PyControl.SafePyEngine;
  // Clear NameSpace Items
  for i := 0 to fWatchesList.Count - 1 do
    with TWatchInfo(fWatchesList[i]) do
      begin
        FreeAndNil(fNS);
        if DebuggerState in [dsPaused, dsPostMortem] then
        fNS := PyControl.ActiveDebugger.Evaluate(Watch);
      end;

  // Turn off Animation to speed things up
  WatchesView.TreeOptions.AnimationOptions :=
    WatchesView.TreeOptions.AnimationOptions - [toAnimatedToggle];

  WatchesView.BeginUpdate;
  try
    WatchesView.RootNodeCount := fWatchesList.Count;
    // The following will Reinitialize only initialized nodes
    // No need to initialize other nodes they will be initialized as needed
    WatchesView.ReinitChildren(nil, True);
    WatchesView.Invalidate;
 finally
    WatchesView.EndUpdate;
  end;
  WatchesView.TreeOptions.AnimationOptions :=
    WatchesView.TreeOptions.AnimationOptions + [toAnimatedToggle];
end;

procedure TWatchesWindow.TBXPopupMenuPopup(Sender: TObject);
begin
  mnRemoveWatch.Enabled := Assigned(WatchesView.GetFirstSelected());
  mnEditWatch.Enabled := mnRemoveWatch.Enabled;
  mnClearall.Enabled := fWatchesList.Count > 0;
  mnCopyToClipboard.Enabled := fWatchesList.Count > 0;
end;

procedure TWatchesWindow.StoreSettings(AppStorage: TJvCustomAppStorage);
begin
  inherited;
  AppStorage.WriteObjectList(FBasePath, fWatchesList, 'Watch');
  AppStorage.WriteInteger(FBasePath + '\WatchesWidth',
    PPIUnScale(WatchesView.Header.Columns[0].Width));
  AppStorage.WriteInteger(FBasePath+'\Types Width',
    PPIUnScale(WatchesView.Header.Columns[1].Width));
end;

procedure TWatchesWindow.RestoreSettings(AppStorage: TJvCustomAppStorage);
begin
  inherited;
  mnClearAllClick(Self);
  AppStorage.ReadObjectList(FBasePath, fWatchesList, CreateWatch, True,
    'Watch');
  WatchesView.Header.Columns[0].Width :=
    PPIScale(AppStorage.ReadInteger(FBasePath + '\WatchesWidth', 200));
  WatchesView.Header.Columns[1].Width :=
    PPIScale(AppStorage.ReadInteger(FBasePath+'\Types Width', 100));
  UpdateWindow(PyControl.DebuggerState);
end;

function TWatchesWindow.CreateWatch(Sender: TJvCustomAppStorage;
  const Path: string; Index: Integer): TPersistent;
begin
  Result := TWatchInfo.Create;
end;

{ TWatchInfo }

end.
