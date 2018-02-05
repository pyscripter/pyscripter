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
  Dialogs, JvDockControlForm, cPyBaseDebugger, frmIDEDockWin,
  Contnrs, ExtCtrls, VirtualTrees, JvComponentBase,  SpTBXSkins,
  JvAppStorage, ActnList, System.Actions, SpTBXItem,
  SpTBXControls;

type
  TCallStackWindow = class(TIDEDockWindow, IJvAppStorageHandler)
    CallStackView: TVirtualStringTree;
    actlCallStack: TActionList;
    actPreviousFrame: TAction;
    actNextFrame: TAction;
    procedure CallStackViewDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CallStackViewGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure CallStackViewInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure CallStackViewChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure FormActivate(Sender: TObject);
    procedure actPreviousFrameExecute(Sender: TObject);
    procedure actNextFrameExecute(Sender: TObject);
  private
    { Private declarations }
    SelectedNode: PVirtualNode;
    fCallStackList : TObjectList;
  protected
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
  public
    { Public declarations }
    procedure ClearAll;
    function GetSelectedStackFrame : TBaseFrameInfo;
    procedure UpdateWindow(DebuggerState, OldState : TDebuggerState);
  end;

var
  CallStackWindow: TCallStackWindow = nil;

implementation

uses frmPyIDEMain, frmVariables,
  dmCommands, frmWatches, uCommonFunctions;

{$R *.dfm}

Type
  PCallStackRec = ^TCallStackRec;
  TCallStackRec = record
    FrameInfo : TBaseFrameInfo;
  end;

{ TCallStackWindow }

procedure TCallStackWindow.UpdateWindow(DebuggerState, OldState : TDebuggerState);
begin
  //  Do not update anything if the debugger just entered running state
  //  The callstack and the variables window will be updated when the
  //  Debugger becomes Paused or Inactive
  case DebuggerState of
    dsPaused, dsPostMortem:
       if OldState = dsRunning then begin
          // This sequence of states happens with RunSource.  No need to update the CallStack - Issue 461
          CallStackView.Enabled := True;
          if Assigned(VariablesWindow) then VariablesWindow.UpdateWindow;
          if Assigned(WatchesWindow) then WatchesWindow.UpdateWindow(DebuggerState);
        end else
        begin
          CallStackView.BeginUpdate;
          try
            ClearAll;
            PyControl.ActiveDebugger.GetCallStack(fCallStackList);
            CallStackView.RootNodeCount := fCallStackList.Count;  // Fills the View
            CallStackView.ValidateNode(nil, True);
            //CallStackView.ReinitNode(CallStackView.RootNode, True);
          finally
            CallStackView.EndUpdate;
          end;
          CallStackView.Enabled := True;

        //  The following statement updates the Variables and Watches Windows as well
          if Assigned(CallStackView.RootNode.FirstChild) then
            CallStackView.Selected[CallStackView.RootNode.FirstChild] := True;
        end;
    dsRunning,
    dsDebugging:
      begin
        CallStackView.Enabled := False;
        if Assigned(VariablesWindow) then VariablesWindow.UpdateWindow;
        if Assigned(WatchesWindow) then WatchesWindow.UpdateWindow(DebuggerState);
      end;
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

procedure TCallStackWindow.actNextFrameExecute(Sender: TObject);
begin
  if CallStackView.Enabled and Assigned(SelectedNode) and
    Assigned(SelectedNode.PrevSibling)
  then
    CallStackView.Selected[SelectedNode.PrevSibling] := True;
end;

procedure TCallStackWindow.actPreviousFrameExecute(Sender: TObject);
begin
  if CallStackView.Enabled and Assigned(SelectedNode) and
    Assigned(SelectedNode.NextSibling)
  then
    CallStackView.Selected[SelectedNode.NextSibling] := True;
end;

procedure TCallStackWindow.CallStackViewChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  if Assigned(Node) and not (tsUpdating in CallStackView.TreeStates) and
    CallStackView.Selected[Node] and (Node <> SelectedNode) then
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

procedure TCallStackWindow.FormActivate(Sender: TObject);
begin
  inherited;
  if CanActuallyFocus(CallStackView) then
    CallStackView.SetFocus;
end;

procedure TCallStackWindow.FormCreate(Sender: TObject);
begin
  inherited;
  fCallStackList := TObjectList.Create(True);  // Onwns objects
  // Let the tree know how much data space we need.
  CallStackView.NodeDataSize := SizeOf(TCallStackRec);
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
  var CellText: string);
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

procedure TCallStackWindow.ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
begin
  CallStackView.Header.Columns[0].Width :=
    PPIScaled(AppStorage.ReadInteger(BasePath+'\Function Width', 100));
  CallStackView.Header.Columns[2].Width :=
    PPIScaled(AppStorage.ReadInteger(BasePath+'\Line Width', 50));
end;

procedure TCallStackWindow.WriteToAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
begin
  AppStorage.WriteInteger(BasePath+'\Function Width',
   PPIUnScaled(CallStackView.Header.Columns[0].Width));
  AppStorage.WriteInteger(BasePath+'\Line Width',
    PPIUnScaled(CallStackView.Header.Columns[2].Width));
end;

end.


