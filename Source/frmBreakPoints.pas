{-----------------------------------------------------------------------------
 Unit Name: frmBreakPoints
 Author:    Kiriakos Vlahos
 Purpose:   Breakpoints Window
 History:
-----------------------------------------------------------------------------}

unit frmBreakPoints;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvDockControlForm, frmIDEDockWin, ExtCtrls,
  Contnrs, TB2Item, Menus, VirtualTrees, JvComponentBase,
  SpTBXSkins, SpTBXItem, JvAppStorage, SpTBXControls, System.ImageList,
  Vcl.ImgList, Vcl.VirtualImageList;

type
  TBreakPointsWindow = class(TIDEDockWindow, IJvAppStorageHandler)
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
    { Private declarations }
    fBreakPointsList : TObjectList;
  protected
    // IJvAppStorageHandler implementation
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
  public
    { Public declarations }
    procedure UpdateWindow;
  end;

var
  BreakPointsWindow: TBreakPointsWindow;

implementation

uses
  Vcl.Clipbrd,
  dmCommands,
  uEditAppIntfs,
  uCommonFunctions,
  cPyControl,
  JvGnugettext,
  StringResources;

{$R *.dfm}

Type
  TBreakPointInfo = class
    FileName : string;
    Line : integer;
    Disabled : Boolean;
    Condition : string;
  end;

  PBreakPointRec = ^TBreakPointRec;
  TBreakPointRec = record
    BreakPoint : TBreakPointInfo;
  end;

procedure TBreakPointsWindow.UpdateWindow;
begin
  BreakPointsView.Clear;
  fBreakPointsList.Clear;

  GI_EditorFactory.ApplyToEditors(procedure(Editor: IEditor)
  begin
    for var BP in Editor.BreakPoints do begin
      var BPInfo := TBreakPointInfo.Create;
      BPInfo.FileName := Editor.FileId;
      BPInfo.Line := TBreakPoint(BP).LineNo;
      BPInfo.Disabled := TBreakPoint(BP).Disabled;
      BPInfo.Condition := TBreakPoint(BP).Condition;
      fBreakPointsList.Add(BPInfo);
    end;
  end);

  BreakPointsView.RootNodeCount := fBreakPointsList.Count;
end;

procedure TBreakPointsWindow.ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
begin
  BreakPointsView.Header.Columns[0].Width :=
    PPIScale(AppStorage.ReadInteger(BasePath+'\FileName Width', 200));
  BreakPointsView.Header.Columns[1].Width :=
    PPIScale(AppStorage.ReadInteger(BasePath+'\Line Width', 50));
end;

procedure TBreakPointsWindow.WriteToAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
begin
  AppStorage.WriteInteger(BasePath+'\FileName Width',
    PPIUnScale(BreakPointsView.Header.Columns[0].Width));
  AppStorage.WriteInteger(BasePath+'\Line Width',
    PPIUnScale(BreakPointsView.Header.Columns[1].Width));
end;

procedure TBreakPointsWindow.BreakPointLVDblClick(Sender: TObject);
var
  Node : PVirtualNode;
  BreakPoint : TBreakPointInfo;
begin
  Node := BreakPointsView.GetFirstSelected();
  if Assigned(Node) then begin
    BreakPoint := PBreakPointRec(BreakPointsView.GetNodeData(Node))^.BreakPoint;

    if (BreakPoint.FileName ='') then Exit; // No FileName or LineNumber
    GI_PyIDEServices.ShowFilePosition(BreakPoint.FileName, BreakPoint.Line, 1);
  end;
end;

procedure TBreakPointsWindow.mnClearClick(Sender: TObject);
var
  Editor : IEditor;
  Node : PVirtualNode;
begin
  Node := BreakPointsView.GetFirstSelected();
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
  Editor : IEditor;
  Node : PVirtualNode;
begin
  Node := BreakPointsView.GetFirstSelected();
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
  Clipboard.AsText := string(BreakPointsView.ContentToText(tstAll, #9));
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
  fBreakPointsList := TObjectList.Create(True);  // Onwns objects
  // Let the tree know how much data space we need.
  BreakPointsView.NodeDataSize := SizeOf(TBreakPointRec);
end;

procedure TBreakPointsWindow.FormDestroy(Sender: TObject);
begin
  fBreakPointsList.Free;
  inherited;
end;

procedure TBreakPointsWindow.BreakPointsViewInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
begin
  Assert(BreakPointsView.GetNodeLevel(Node) = 0);
  Assert(Integer(Node.Index) < fBreakPointsList.Count);
  PBreakPointRec(BreakPointsView.GetNodeData(Node))^.BreakPoint :=
    fBreakPointsList[Node.Index] as TBreakPointInfo;
  Node.CheckType := ctCheckBox;
  if TBreakPointInfo(fBreakPointsList[Node.Index]).Disabled then
    Node.CheckState := csUnCheckedNormal
  else
    Node.CheckState := csCheckedNormal;
end;

procedure TBreakPointsWindow.BreakPointsViewGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  Assert(BreakPointsView.GetNodeLevel(Node) = 0);
  Assert(Integer(Node.Index) < fBreakPointsList.Count);
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
  inherited;
  if Key = VK_Delete then
  begin
    mnClearClick(Sender);
    Key := 0;
  end;
end;

procedure TBreakPointsWindow.TBXPopupMenuPopup(Sender: TObject);
begin
  mnClear.Enabled := Assigned(BreakPointsView.GetFirstSelected());
  mnSetCondition.Enabled := Assigned(BreakPointsView.GetFirstSelected());
  mnCopyToClipboard.Enabled := fBreakPointsList.Count > 0;
end;

end.


