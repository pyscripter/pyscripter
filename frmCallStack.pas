{-----------------------------------------------------------------------------
 Unit Name: frmCallStack
 Author:    Kiriakos Vlahos
 Purpose:   Call Stack Window
 History:
-----------------------------------------------------------------------------}

unit frmCallStack;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, JvDockControlForm, JvComponent, cPyBaseDebugger, frmIDEDockWin,
  Contnrs, ExtCtrls, TBX, TBXThemes, VirtualTrees, JvComponentBase;

type
  TCallStackWindow = class(TIDEDockWindow)
    CallStackView: TVirtualStringTree;
    procedure CallStackViewDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CallStackViewGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure CallStackViewInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure CallStackViewChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
    SelectedNode: PVirtualNode;
    fCallStackList : TObjectList;
  protected
    procedure TBMThemeChange(var Message: TMessage); message TBM_THEMECHANGE;
  public
    { Public declarations }
    procedure ClearAll;
    function GetSelectedStackFrame : TBaseFrameInfo;
    procedure UpdateWindow(DebuggerState : TDebuggerState);
  end;

var
  CallStackWindow: TCallStackWindow = nil;

implementation

uses frmPyIDEMain, VarPyth, frmVariables, PythonEngine, uCommonFunctions,
  dmCommands, JvDockGlobals, frmWatches;

{$R *.dfm}

Type
  PCallStackRec = ^TCallStackRec;
  TCallStackRec = record
    FrameInfo : TBaseFrameInfo;
  end;

{ TCallStackWindow }

procedure TCallStackWindow.UpdateWindow(DebuggerState : TDebuggerState);
begin
  //  Do not update anything if the debugger just entered running state
  //  The callstack and the variables window will be updated when the
  //  Debugger becomes Paused or Inactive
  case DebuggerState of
    dsPaused, dsPostMortem:
      begin
        CallStackView.Enabled := True;
        ClearAll;
        PyControl.ActiveDebugger.GetCallStack(fCallStackList);

        CallStackView.RootNodeCount := fCallStackList.Count;  // Fills the View
        CallStackView.ReinitNode(CallStackView.RootNode, True);

        //  The following statement updates the Variables and Watches Windows as well
        if Assigned(CallStackView.RootNode.FirstChild) then
          CallStackView.Selected[CallStackView.RootNode.FirstChild] := True;
      end;
    dsRunning:
      begin
        CallStackView.Enabled := False;
        if Assigned(VariablesWindow) then VariablesWindow.UpdateWindow;
        if Assigned(WatchesWindow) then WatchesWindow.UpdateWindow(DebuggerState);
      end;
    dsRunningNoDebug,
    dsInactive:
      begin
        ClearAll;
        CallStackView.Enabled := False;
        if Assigned(VariablesWindow) then VariablesWindow.UpdateWindow;
        if Assigned(WatchesWindow) then WatchesWindow.UpdateWindow(DebuggerState);
      end;
  end;
end;

procedure TCallStackWindow.ClearAll;
begin
  CallStackView.Clear;
  fCallStackList.Clear;
  SelectedNode := nil;
end;

procedure TCallStackWindow.CallStackViewChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  if Assigned(Node) and CallStackView.Selected[Node] and
     (Node <> SelectedNode) then
  begin
    // Update the Variables Window
    SelectedNode := Node;
    PyControl.ActiveDebugger.MakeFrameActive(
      PCallStackRec(CallStackView.GetNodeData(Node))^.FrameInfo);
    if Assigned(VariablesWindow) then
      VariablesWindow.UpdateWindow;
    if Assigned(WatchesWindow) then
      WatchesWindow.UpdateWindow(PyControl.DebuggerState);
  end;
end;

procedure TCallStackWindow.CallStackViewDblClick(Sender: TObject);
begin
  if Assigned(SelectedNode) then
    with PCallStackRec(CallStackView.GetNodeData(SelectedNode))^.FrameInfo do
      if FileName <> '' then
        PyIDEMainForm.ShowFilePosition(FileName, Line, 1);
end;

procedure TCallStackWindow.TBMThemeChange(var Message: TMessage);
begin
  inherited;
  if Message.WParam = TSC_VIEWCHANGE then begin
    CallStackView.Header.Invalidate(nil, True);
    CallStackView.Colors.HeaderHotColor :=
      CurrentTheme.GetItemTextColor(GetItemInfo('active'));
  end;
end;

procedure TCallStackWindow.FormActivate(Sender: TObject);
begin
  inherited;
  if CallStackView.CanFocus then
    CallStackView.SetFocus;
end;

procedure TCallStackWindow.FormCreate(Sender: TObject);
begin
  inherited;
  fCallStackList := TObjectList.Create(True);  // Onwns objects
  // Let the tree know how much data space we need.
  CallStackView.NodeDataSize := SizeOf(TCallStackRec);
  CallStackView.OnAdvancedHeaderDraw :=
    CommandsDataModule.VirtualStringTreeAdvancedHeaderDraw;
  CallStackView.OnHeaderDrawQueryElements :=
    CommandsDataModule.VirtualStringTreeDrawQueryElements;
end;

procedure TCallStackWindow.FormDestroy(Sender: TObject);
begin
  fCallStackList.Free;
  CallStackWindow := nil;
  inherited;
end;

procedure TCallStackWindow.CallStackViewInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
begin
  Assert(CallStackView.GetNodeLevel(Node) = 0);
  Assert(Integer(Node.Index) < fCallStackList.Count);
  PCallStackRec(CallStackView.GetNodeData(Node))^.FrameInfo :=
    fCallStackList[Node.Index] as TBaseFrameInfo;
end;

procedure TCallStackWindow.CallStackViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
begin
  Assert(CallStackView.GetNodeLevel(Node) = 0);
  Assert(Integer(Node.Index) < fCallStackList.Count);
  with PCallStackRec(CallStackView.GetNodeData(Node))^.FrameInfo do
    case Column of
      0:  CellText := FunctionName;
      1:  CellText := FileName;
      2:  if Line > 0
            then CellText := IntToStr(Line)
          else
            CellText := '';
    end;
end;

function TCallStackWindow.GetSelectedStackFrame: TBaseFrameInfo;
begin
  Result := nil;
  if Assigned(SelectedNode) then
    Result :=
      PCallStackRec(CallStackView.GetNodeData(SelectedNode))^.FrameInfo;
end;

end.


