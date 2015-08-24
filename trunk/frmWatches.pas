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
  Windows, Messages, ActiveX, SysUtils, Variants, Classes, Graphics, Controls,
  Forms,
  Dialogs, Menus, frmIDEDockWin, JvDockControlForm,
  Contnrs, cPyBaseDebugger, ExtCtrls, TB2Item, VirtualTrees,
  JvComponentBase, JvAppStorage, SpTBXSkins, SpTBXItem, PythonEngine,
  cThemedVirtualStringTree;

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
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure WatchesViewInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
  private
    { Private declarations }
    fWatchesList: TObjectList;
  protected
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
    // IJvAppStorageHandler implementation
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
      const BasePath: string);
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage;
      const BasePath: string);
    function CreateWatch(Sender: TJvCustomAppStorage; const Path: string;
      Index: Integer): TPersistent;
  public
    { Public declarations }
    procedure UpdateWindow(DebuggerState: TDebuggerState);
    procedure AddWatch(S: string);
  end;

var
  WatchesWindow: TWatchesWindow;

implementation

uses frmPyIDEMain, dmCommands,
  Clipbrd, StringResources, gnugettext, uCommonFunctions, SynEdit, frmCallStack;
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
    NS: TBaseNameSpaceItem;
  end;

destructor TWatchInfo.Destroy;
begin
  FreeAndNil(fNS);
  inherited;
end;

{ TWatchesWindow }

procedure TWatchesWindow.FormCreate(Sender: TObject);
begin
  inherited;
//  AllocConsole;
  fWatchesList := TObjectList.Create(True); // Onwns objects
  // Let the tree know how much data space we need.
  WatchesView.NodeDataSize := SizeOf(TWatchRec);
  WatchesView.Header.Height := MulDiv(WatchesView.Header.Height,
    Screen.PixelsPerInch, 96);
end;

procedure TWatchesWindow.WatchesViewInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
var
  Data, ParentData: PWatchRec;
begin
  Data := WatchesView.GetNodeData(Node);
  if WatchesView.GetNodeLevel(Node) = 0 then
  begin
    Assert(Integer(Node.Index) < fWatchesList.Count);
    Data.NS := TWatchInfo(fWatchesList[Node.Index]).fNS;
    if Assigned(Data.NS) then
      ChildCount := Data.NS.ChildCount
    else
      ChildCount := 0;
  end
  else
  begin
    ParentData := WatchesView.GetNodeData(Node.Parent);
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
  Data := WatchesView.GetNodeData(Node);
  if WatchesView.GetNodeLevel(Node) = 0 then
  begin
    Assert(Integer(Node.Index) < fWatchesList.Count);
    Data.NS := TWatchInfo(fWatchesList[Node.Index]).fNS;
    if Assigned(Data.NS) then
      ChildCount := Data.NS.ChildCount
    else
      ChildCount := 0;
  end
  else
  begin
    ParentData := WatchesView.GetNodeData(ParentNode);
    Assert(Assigned(ParentData.NS));
    Data.NS := ParentData.NS.ChildNode[Node.Index];
    ChildCount := Data.NS.ChildCount;
  end;
  if ChildCount > 0 then
    InitialStates := [ivsHasChildren]
  else
    InitialStates := [];
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
  S: string;
begin
  S := InputBox('Add Watch', 'Enter watch expression:', '');
  if S <> '' then
  begin
    AddWatch(S);
  end;
end;

procedure TWatchesWindow.mnEditWatchClick(Sender: TObject);

Var
  Node: PVirtualNode;
  WatchInfo: TWatchInfo;
begin
  Node := WatchesView.GetFirstSelected();

  if Assigned(Node) then
  begin
    while Assigned(Node.Parent) and (Node.Parent <> WatchesView.RootNode) do
      Node := Node.Parent;
    WatchInfo := fWatchesList[Node.Index] as TWatchInfo;
    WatchInfo.Watch := InputBox('Edit Watch', 'Enter new expression:', WatchInfo.Watch);
    if WatchInfo.Watch = '' then
      fWatchesList.Remove(WatchInfo);
    UpdateWindow(PyControl.DebuggerState);
  end;
end;

procedure TWatchesWindow.mnRemoveWatchClick(Sender: TObject);

Var
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

Var
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

procedure TWatchesWindow.WMSpSkinChange(var Message: TMessage);
begin
  inherited;
  WatchesView.SkinTree;
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
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PWatchRec;
begin
  if (Column = 0) and (Kind in [ikNormal, ikSelected]) then begin
    Data := WatchesView.GetNodeData(Node);

    if not Assigned(Data) or not Assigned(Data.NS) then Exit;

    with GetPythonEngine do begin
      if Data.NS.IsDict then
        ImageIndex := Integer(TCodeImages.Namespace)
      else if Data.NS.IsModule then
        ImageIndex := Integer(TCodeImages.Module)
      else if Data.NS.IsMethod then
        ImageIndex := Integer(TCodeImages.Method)
      else if Data.NS.IsFunction then
        ImageIndex := Integer(TCodeImages.Func)
      else if Data.NS.IsClass or Data.NS.Has__dict__ then
          ImageIndex := Integer(TCodeImages.Klass)
      else if (Data.NS.ObjectType = 'list') or (Data.NS.ObjectType = 'tuple') then
        ImageIndex := Integer(TCodeImages.List)
      else begin
        if Assigned(Node.Parent) and (Node.Parent <> WatchesView.RootNode) and
          (PWatchRec(WatchesView.GetNodeData(Node.Parent)).NS.IsDict
            or PWatchRec(WatchesView.GetNodeData(Node.Parent)).NS.IsModule)
        then
          ImageIndex := Integer(TCodeImages.Variable)
        else
          ImageIndex := Integer(TCodeImages.Field);
      end;
    end
  end else
    ImageIndex := -1;
end;

procedure TWatchesWindow.WatchesViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data: PWatchRec;
begin
  Data := WatchesView.GetNodeData(Node);
  if not Assigned(Data) then Exit;

  if Assigned(Data.NS) then
    case Column of
      0: CellText := Data.NS.Name;
      1: CellText := Data.NS.ObjectType;
      2: CellText := Data.NS.Value;
    end
  else begin
    Assert(WatchesView.GetNodeLevel(Node) = 0);
    case Column of
      0: CellText := TWatchInfo(fWatchesList[Node.Index]).Watch;
      1: CellText := _(SNotAvailable);
      2: CellText := _(SNotAvailable);
    end;
  end;
end;

procedure TWatchesWindow.UpdateWindow(DebuggerState: TDebuggerState);
Var
  i: Integer;
begin
  if PyControl.IsRunning then begin
    WatchesView.Enabled := False;
    Exit;
  end else
    WatchesView.Enabled := True;

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
    WatchesView.ReinitInitializedChildren(nil, True);
    WatchesView.InvalidateToBottom(WatchesView.GetFirstVisible);
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

procedure TWatchesWindow.WriteToAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
begin
  AppStorage.WriteObjectList(BasePath, fWatchesList, 'Watch');
  AppStorage.WriteInteger(BasePath + '\WatchesWidth',
    WatchesView.Header.Columns[0].Width);
  AppStorage.WriteInteger(BasePath+'\Types Width', WatchesView.Header.Columns[1].Width);
  WatchesView.Header.Columns[1].Width := AppStorage.ReadInteger(BasePath+'\Types Width', 100);
end;

procedure TWatchesWindow.ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
begin
  mnClearAllClick(Self);
  AppStorage.ReadObjectList(BasePath, fWatchesList, CreateWatch, True,
    'Watch');
  WatchesView.Header.Columns[0].Width := AppStorage.ReadInteger
    (BasePath + '\WatchesWidth', 200);
  UpdateWindow(PyControl.DebuggerState);
end;

function TWatchesWindow.CreateWatch(Sender: TJvCustomAppStorage;
  const Path: string; Index: Integer): TPersistent;
begin
  Result := TWatchInfo.Create;
end;

{ TWatchInfo }

end.
