{-----------------------------------------------------------------------------
 Unit Name: frmBreakPoints
 Author:    Kiriakos Vlahos
 Purpose:   Breakpoints Window
 History:
-----------------------------------------------------------------------------}

unit frmBreakPoints;

interface

uses
  System.SysUtils,
  System.Classes,
  System.ImageList,
  System.Generics.Collections,
  Vcl.Controls,
  Vcl.Menus,
  Vcl.ExtCtrls,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  JvAppStorage,
  JvComponentBase,
  JvDockControlForm,
  VirtualTrees.Types,
  VirtualTrees.BaseTree,
  VirtualTrees.BaseAncestorVCL,
  VirtualTrees.AncestorVCL,
  VirtualTrees,
  TB2Item,
  SpTBXItem,
  cPySupportTypes,
  frmIDEDockWin;

type

  TBreakPointsWindow = class(TIDEDockWindow)
    TBXPopupMenu: TSpTBXPopupMenu;
    mnClear: TSpTBXItem;
    Breakpoints1: TSpTBXItem;
    BreakPointsView: TVirtualStringTree;
    mnSetCondition: TSpTBXItem;
    TBXSeparatorItem1: TSpTBXSeparatorItem;
    TBXSeparatorItem2: TSpTBXSeparatorItem;
    mnCopyToClipboard: TSpTBXItem;
    Panel1: TPanel;
    vilImages: TVirtualImageList;
    procedure TBXPopupMenuPopup(Sender: TObject);
    procedure mnCopyToClipboardClick(Sender: TObject);
    procedure mnEditClick(Sender: TObject);
    procedure BreakPointsViewChecked(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure BreakPointLVDblClick(Sender: TObject);
    procedure mnClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BreakPointsViewInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure BreakPointsViewGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure BreakPointsViewKeyDown(Sender: TObject; var Key: Word; Shift:
        TShiftState);
    procedure BreakPointsViewNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; NewText: string);
    procedure FormActivate(Sender: TObject);
  private
    const FBasePath = 'Breakpoints Window Options'; // Used for storing settings
    var FBreakPoints: TArray<TBreakpointInfo>;
  public
    procedure UpdateWindow;
    // AppStorage
    procedure StoreSettings(AppStorage: TJvCustomAppStorage); override;
    procedure RestoreSettings(AppStorage: TJvCustomAppStorage); override;
  end;

var
  BreakPointsWindow: TBreakPointsWindow;

implementation

uses
  Winapi.Windows,
  System.Math,
  System.Contnrs,
  Vcl.Dialogs,
  Vcl.Clipbrd,
  uEditAppIntfs,
  uCommonFunctions,
  cPyControl,
  JvGnugettext,
  StringResources,
  dmResources;

{$R *.dfm}

procedure TBreakPointsWindow.UpdateWindow;
begin
  BreakPointsView.Clear;
  FBreakPoints := GI_BreakpointManager.AllBreakPoints;

  BreakPointsView.RootNodeCount := Length(FBreakPoints);
end;

procedure TBreakPointsWindow.RestoreSettings(AppStorage: TJvCustomAppStorage);
begin
  inherited;
  BreakPointsView.Header.Columns[0].Width :=
    PPIScale(AppStorage.ReadInteger(FBasePath+'\FileName Width', 250));
  BreakPointsView.Header.Columns[1].Width :=
    PPIScale(AppStorage.ReadInteger(FBasePath+'\Line Width', 90));
  BreakPointsView.Header.Columns[2].Width :=
    PPIScale(AppStorage.ReadInteger(FBasePath+'\Ignore Width', 90));
end;

procedure TBreakPointsWindow.StoreSettings(AppStorage: TJvCustomAppStorage);
begin
  inherited;
  AppStorage.WriteInteger(FBasePath+'\FileName Width',
    PPIUnScale(BreakPointsView.Header.Columns[0].Width));
  AppStorage.WriteInteger(FBasePath+'\Line Width',
    PPIUnScale(BreakPointsView.Header.Columns[1].Width));
  AppStorage.WriteInteger(FBasePath+'\Ignore Width',
    PPIUnScale(BreakPointsView.Header.Columns[2].Width));
end;

procedure TBreakPointsWindow.BreakPointLVDblClick(Sender: TObject);
begin
  var Node := BreakPointsView.GetFirstSelected;
  if Assigned(Node) then
    with FBreakPoints[Node.Index] do
    begin
      if FileName ='' then Exit; // No FileName or LineNumber
      GI_PyIDEServices.ShowFilePosition(FileName, LineNo);
    end;
end;

procedure TBreakPointsWindow.mnClearClick(Sender: TObject);
begin
  var Node := BreakPointsView.GetFirstSelected;
  if Assigned(Node) then
    with FBreakPoints[Node.Index] do
    begin
      if FileName <> '' then
      begin
        GI_BreakpointManager.ToggleBreakpoint(FileName, LineNo, False);
        Delete(FBreakPoints, Node.Index, 1);
        BreakPointsView.DeleteNode(Node);
        Assert(BreakPointsView.RootNodeCount = Length(FBreakPoints));
      end;
    end;
end;

procedure TBreakPointsWindow.mnEditClick(Sender: TObject);
var
  Value: Integer;
begin
  var Node := BreakPointsView.GetFirstSelected;
  if Assigned(Node) then
    with FBreakPoints[Node.Index] do
    begin
      if FileName = '' then Exit; // No FileName or LineNumber
      var Values := [IntToStr(IgnoreCount), Condition];
      var Prompts := [_('Ignore Count') + ':', _('Condition') + ':'];

      if InputQuery(_(SEditBreakpointCond), Prompts, Values,
        function (const Values: array of string): Boolean
        begin
          if not TryStrToInt(Values[0], Value) then
          begin
            StyledMessageDlg(_('Ignore Count must be a positive integer'),
              mtError, [TMsgDlgBtn.mbOK], 0);
            Exit(False);
          end;
          Result := True;
        end)
      then
      begin
        Condition := Values[1];
        IgnoreCount := Max(StrToInt(Values[0]), 0);
        GI_BreakpointManager.SetBreakpoint(FileName, LineNo,
          Disabled, Condition, IgnoreCount, False);
        BreakPointsView.InvalidateNode(Node);
      end;
    end;
end;

procedure TBreakPointsWindow.mnCopyToClipboardClick(Sender: TObject);
begin
  Clipboard.AsText := BreakPointsView.ContentToText(tstAll, #9);
end;

procedure TBreakPointsWindow.FormActivate(Sender: TObject);
begin
  inherited;
  if CanActuallyFocus(BreakPointsView) then
    BreakPointsView.SetFocus;
end;

procedure TBreakPointsWindow.FormCreate(Sender: TObject);
begin
  ImageName := 'BreakpointsWin';
  inherited;
  // Let the tree know how much data space we need.
  BreakPointsView.NodeDataSize := 0;
end;

procedure TBreakPointsWindow.BreakPointsViewInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
begin
  Assert(ParentNode = nil, 'BreakPointsViewInitNode');
  Assert(Integer(Node.Index) < Length(FBreakPoints), 'BreakPointsViewInitNode');

  Node.CheckType := ctCheckBox;
  if FBreakPoints[Node.Index].Disabled then
    Node.CheckState := csUncheckedNormal
  else
    Node.CheckState := csCheckedNormal;
end;

procedure TBreakPointsWindow.BreakPointsViewGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  Assert(Integer(Node.Index) < Length(FBreakPoints), 'BreakPointsViewGetText');
  with FBreakPoints[Node.Index] do
    case Column of
      0:  CellText := FileName;
      1:  if LineNo > 0
            then CellText := LineNo.ToString
          else
            CellText := '';
      2:  CellText := IgnoreCount.ToString;
      3:  CellText := Condition;
    end;
end;

procedure TBreakPointsWindow.BreakPointsViewChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  with FBreakPoints[Node.Index] do
  begin
    if Node.CheckState = csCheckedNormal then
      Disabled := False
    else
      Disabled := True;
    GI_BreakpointManager.SetBreakpoint(FileName, LineNo, Disabled, Condition,
      IgnoreCount, False);
  end;
  BreakPointsView.InvalidateNode(Node);
end;

procedure TBreakPointsWindow.BreakPointsViewKeyDown(Sender: TObject; var Key:
    Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
  begin
    mnClearClick(Sender);
    Key := 0;
  end;
end;

procedure TBreakPointsWindow.BreakPointsViewNewText(Sender: TBaseVirtualTree;
    Node: PVirtualNode; Column: TColumnIndex; NewText: string);
begin
  if Assigned(Node) then
  begin
    with FBreakPoints[Node.Index] do
    begin
      case Column of
        2: IgnoreCount := StrToIntDef(NewText, IgnoreCount);
        3: Condition := NewText;
      end;
      GI_BreakpointManager.SetBreakpoint(FileName, LineNo,
        Disabled, Condition, IgnoreCount, False);
    end;
  end;
end;

procedure TBreakPointsWindow.TBXPopupMenuPopup(Sender: TObject);
begin
  mnClear.Enabled := Assigned(BreakPointsView.GetFirstSelected());
  mnSetCondition.Enabled := Assigned(BreakPointsView.GetFirstSelected());
  mnCopyToClipboard.Enabled := Length(FBreakPoints) > 0;
end;


{$REGION 'TBreakpointManagement'}
type

  TBreakpointManager = class(TInterfacedObject, IBreakpointManager)
  private
    FBreakpointsChanged: Boolean;
    FIsUpdating: Boolean;
    procedure DoBreakpointsChanged(const FileName: string; Line: Integer;
    UpdateUI: Boolean = True);
    procedure DoUpdateUI;
    {IBreakpointManager implementation}
    function GetBreakpointsChanged: Boolean;
    procedure SetBreakpointsChanged(Value: Boolean);
    procedure ToggleBreakpoint(const FileName: string; ALine: Integer;
      CtrlPressed: Boolean = False; UpdateUI: Boolean = True);
    procedure SetBreakpoint(const FileName: string; ALine: Integer;
      Disabled: Boolean; Condition: string = ''; IgnoreCount: Integer = 0;
      UpdateUI: Boolean = True);
    function AllBreakPoints: TArray<TBreakpointInfo>;
    procedure ClearAllBreakpoints;
  end;


{ TBreakpointManager }

function TBreakpointManager.AllBreakPoints: TArray<TBreakpointInfo>;
var
  BPInfo: TBreakpointInfo;
  Res: TArray<TBreakpointInfo>;
begin
  Res := [];

  GI_EditorFactory.ApplyToEditors(procedure(Editor: IEditor)
  begin
    for var BP in Editor.BreakPoints do
    begin
      FillChar(BPInfo, SizeOf(TBreakpointInfo), 0);
      BPInfo.FileName := Editor.FileId;
      BPInfo.LineNo := TBreakpoint(BP).LineNo;
      BPInfo.Disabled := TBreakpoint(BP).Disabled;
      BPInfo.Condition := TBreakpoint(BP).Condition;
      BPInfo.IgnoreCount := TBreakpoint(BP).IgnoreCount;
      Res := Res + [BPInfo];
    end;
  end);
  Result := Res;
end;

procedure TBreakpointManager.ClearAllBreakpoints;
begin
  GI_EditorFactory.ApplyToEditors(procedure(Editor: IEditor)
  begin
    if Editor.BreakPoints.Count > 0 then begin
      Editor.BreakPoints.Clear;
      DoBreakpointsChanged(Editor.FileId, -1);
      DoUpdateUI;
    end;
  end);
end;

procedure TBreakpointManager.DoBreakpointsChanged(const FileName: string;
  Line: Integer; UpdateUI: Boolean = True);
begin
  GI_EditorFactory.InvalidatePos(FileName, Line, itGutter);
  FBreakpointsChanged := True;
end;

procedure TBreakpointManager.DoUpdateUI;
begin
  if not FIsUpdating then
  begin
    FIsUpdating := True;
    TThread.ForceQueue(nil, procedure
      begin
        BreakPointsWindow.UpdateWindow;
        FIsUpdating := False;
      end);
  end;
end;

function TBreakpointManager.GetBreakPointsChanged: Boolean;
begin
  Result := FBreakpointsChanged;
end;

procedure TBreakpointManager.SetBreakpoint(const FileName: string; ALine: Integer;
  Disabled: Boolean; Condition: string = ''; IgnoreCount: Integer = 0;
  UpdateUI: Boolean = True);
var
  Editor: IEditor;
begin
  Editor := GI_EditorFactory.GetEditorByFileId(FileName);
  if Assigned(Editor) and (ALine > 0) then
  begin
    (Editor.BreakPoints as TBreakpointList).SetBreakPoint(
      ALine, Disabled, Condition, IgnoreCount);
    DoBreakpointsChanged(FileName, ALine);
    if UpdateUI then DoUpdateUI;
  end;
end;

procedure TBreakpointManager.SetBreakpointsChanged(Value: Boolean);
begin
  FBreakpointsChanged := Value;
  if Value then DoUpdateUI;
end;

procedure TBreakpointManager.ToggleBreakpoint(const FileName: string;
  ALine: Integer; CtrlPressed: Boolean; UpdateUI: Boolean);
var
  Index: NativeInt;
  Breakpoint: TBreakpoint;
begin
  if ALine <= 0 then Exit;

  var Editor := GI_EditorFactory.GetEditorByFileId(FileName);
  if not Assigned(Editor) then Exit;

  var BPList := Editor.BreakPoints as TBreakpointList;
  if BPList.FindLine(ALine, Index) then
  begin
    Breakpoint := TBreakpoint(BPList[Index]);
    if not CtrlPressed then
      BPList.Delete(Index);
  end
  else
  begin
    Breakpoint := TBreakpoint.Create(ALine);
    BPList.Insert(Index, Breakpoint);
  end;
  if CtrlPressed then
    // Toggle disabled
    Breakpoint.Disabled := not Breakpoint.Disabled;

  DoBreakpointsChanged(FileName, ALine);
  if UpdateUI then DoUpdateUI;
end;

{$ENDREGION 'TBreakpointManagement'}

initialization
  GI_BreakpointManager := TBreakpointManager.Create;
end.


