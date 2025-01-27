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
  System.Contnrs,
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
    procedure mnSetConditionClick(Sender: TObject);
    procedure BreakPointsViewChecked(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure BreakPointLVDblClick(Sender: TObject);
    procedure mnClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BreakPointsViewInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure BreakPointsViewGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure BreakPointsViewKeyDown(Sender: TObject; var Key: Word; Shift:
        TShiftState);
    procedure FormActivate(Sender: TObject);
  private
    const FBasePath = 'Breakpoints Window Options'; // Used for storing settings
    var FBreakPointsList: TObjectList;
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
  Vcl.Dialogs,
  Vcl.Clipbrd,
  uEditAppIntfs,
  uCommonFunctions,
  cPyControl,
  JvGnugettext,
  StringResources,
  dmResources,
  cPySupportTypes;

{$R *.dfm}

type
  TBreakPointInfo = class
    FileName: string;
    Line: Integer;
    Disabled: Boolean;
    Condition: string;
  end;

  PBreakPointRec = ^TBreakPointRec;
  TBreakPointRec = record
    BreakPoint: TBreakPointInfo;
  end;

procedure TBreakPointsWindow.UpdateWindow;
begin
  BreakPointsView.Clear;
  FBreakPointsList.Clear;

  GI_EditorFactory.ApplyToEditors(procedure(Editor: IEditor)
  begin
    for var BP in Editor.BreakPoints do
    begin
      var BPInfo := TBreakPointInfo.Create;
      BPInfo.FileName := Editor.FileId;
      BPInfo.Line := TBreakPoint(BP).LineNo;
      BPInfo.Disabled := TBreakPoint(BP).Disabled;
      BPInfo.Condition := TBreakPoint(BP).Condition;
      FBreakPointsList.Add(BPInfo);
    end;
  end);

  BreakPointsView.RootNodeCount := FBreakPointsList.Count;
end;

procedure TBreakPointsWindow.RestoreSettings(AppStorage: TJvCustomAppStorage);
begin
  inherited;
  BreakPointsView.Header.Columns[0].Width :=
    PPIScale(AppStorage.ReadInteger(FBasePath+'\FileName Width', 200));
  BreakPointsView.Header.Columns[1].Width :=
    PPIScale(AppStorage.ReadInteger(FBasePath+'\Line Width', 50));
end;

procedure TBreakPointsWindow.StoreSettings(AppStorage: TJvCustomAppStorage);
begin
  inherited;
  AppStorage.WriteInteger(FBasePath+'\FileName Width',
    PPIUnScale(BreakPointsView.Header.Columns[0].Width));
  AppStorage.WriteInteger(FBasePath+'\Line Width',
    PPIUnScale(BreakPointsView.Header.Columns[1].Width));
end;

procedure TBreakPointsWindow.BreakPointLVDblClick(Sender: TObject);
var
  Node: PVirtualNode;
  BreakPoint: TBreakPointInfo;
begin
  Node := BreakPointsView.GetFirstSelected;
  if Assigned(Node) then begin
    BreakPoint := PBreakPointRec(BreakPointsView.GetNodeData(Node))^.BreakPoint;

    if (BreakPoint.FileName ='') then Exit; // No FileName or LineNumber
    GI_PyIDEServices.ShowFilePosition(BreakPoint.FileName, BreakPoint.Line, 1);
  end;
end;

procedure TBreakPointsWindow.mnClearClick(Sender: TObject);
var
  Editor: IEditor;
  Node: PVirtualNode;
begin
  Node := BreakPointsView.GetFirstSelected;
  if Assigned(Node) then
    with PBreakPointRec(BreakPointsView.GetNodeData(Node))^.BreakPoint do begin
     if FileName = '' then Exit; // No FileName or LineNumber
     Editor := GI_EditorFactory.GetEditorByFileId(FileName);
     if Assigned(Editor) then
       PyControl.ToggleBreakpoint(Editor, Line);
    end;
end;

procedure TBreakPointsWindow.mnSetConditionClick(Sender: TObject);
var
  Editor: IEditor;
  Node: PVirtualNode;
begin
  Node := BreakPointsView.GetFirstSelected;
  if Assigned(Node) then
    with PBreakPointRec(BreakPointsView.GetNodeData(Node))^.BreakPoint do begin
      if FileName = '' then Exit; // No FileName or LineNumber
      Editor := GI_EditorFactory.GetEditorByFileId(FileName);
      if Assigned(Editor) then begin
        if InputQuery(_(SEditBreakpointCond), _(SEnterPythonExpression), Condition)
        then
          PyControl.SetBreakPoint(FileName, Line, Disabled, Condition);
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
  FBreakPointsList := TObjectList.Create(True);  // Onwns objects
  // Let the tree know how much data space we need.
  BreakPointsView.NodeDataSize := SizeOf(TBreakPointRec);
end;

procedure TBreakPointsWindow.FormDestroy(Sender: TObject);
begin
  FBreakPointsList.Free;
  inherited;
end;

procedure TBreakPointsWindow.BreakPointsViewInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
begin
  Assert(ParentNode = nil, 'BreakPointsViewInitNode');
  Assert(Integer(Node.Index) < FBreakPointsList.Count, 'BreakPointsViewInitNode');
  PBreakPointRec(BreakPointsView.GetNodeData(Node))^.BreakPoint :=
    FBreakPointsList[Node.Index] as TBreakPointInfo;
  Node.CheckType := ctCheckBox;
  if TBreakPointInfo(FBreakPointsList[Node.Index]).Disabled then
    Node.CheckState := csUncheckedNormal
  else
    Node.CheckState := csCheckedNormal;
end;

procedure TBreakPointsWindow.BreakPointsViewGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  Assert(Integer(Node.Index) < FBreakPointsList.Count, 'BreakPointsViewGetText');
  with PBreakPointRec(BreakPointsView.GetNodeData(Node))^.BreakPoint do
    case Column of
      0:  CellText := FileName;
      1:  if Line > 0
            then CellText := IntToStr(Line)
          else
            CellText := '';
      2:  CellText := Condition;
    end;
end;

procedure TBreakPointsWindow.BreakPointsViewChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  with PBreakPointRec(BreakPointsView.GetNodeData(Node))^.BreakPoint do begin
    if Node.CheckState = csCheckedNormal then
      Disabled := False
    else
      Disabled := True;
    PyControl.SetBreakPoint(FileName, Line, Disabled, Condition);
  end;

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

procedure TBreakPointsWindow.TBXPopupMenuPopup(Sender: TObject);
begin
  mnClear.Enabled := Assigned(BreakPointsView.GetFirstSelected());
  mnSetCondition.Enabled := Assigned(BreakPointsView.GetFirstSelected());
  mnCopyToClipboard.Enabled := FBreakPointsList.Count > 0;
end;

end.


