{-----------------------------------------------------------------------------
 Unit Name: frmCallStack
 Author:    Kiriakos Vlahos
 Purpose:   Call Stack Window
 History:
-----------------------------------------------------------------------------}

unit frmCallStack;

interface

uses
  WinApi.Windows,
  WinApi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  System.Actions,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.ActnList,
  JvComponentBase,
  JvDockControlForm,
  JvAppStorage,
  SpTBXSkins,
  SpTBXItem,
  SpTBXControls,
  VirtualTrees,
  frmIDEDockWin,
  dmCommands,
  cPyControl,
  cPyBaseDebugger;

type
  TCallStackWindow = class(TIDEDockWindow, IJvAppStorageHandler)
    CallStackView: TVirtualStringTree;
    actlCallStack: TActionList;
    actPreviousFrame: TAction;
    actNextFrame: TAction;
    ThreadView: TVirtualStringTree;
    Splitter1: TSplitter;
    procedure CallStackViewDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CallStackViewGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure CallStackViewInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure FormActivate(Sender: TObject);
    procedure actPreviousFrameExecute(Sender: TObject);
    procedure actNextFrameExecute(Sender: TObject);
    procedure ThreadViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure ThreadViewGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure CallStackViewGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure ThreadViewAddToSelection(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure CallStackViewAddToSelection(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
  private
    { Private declarations }
    fCallStackList : TObjectList<TBaseFrameInfo>;
    fThreads : TArray<TThreadInfo>;
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

uses
  frmPyIDEMain,
  frmVariables,
  frmWatches,
  uCommonFunctions;

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
          ThreadView.Enabled := True;
          CallStackView.Enabled := True;
        end else
        begin
          ClearAll;
          ThreadView.BeginUpdate;
          try
            PyControl.ActiveDebugger.GetThreadInfos(fThreads);
            ThreadView.RootNodeCount := Length(fThreads);  // Fills the View
            ThreadView.ValidateNode(nil, True);
          finally
            ThreadView.EndUpdate;
          end;
          ThreadView.Enabled := True;
          //  The following statement updates the CallStackView
          //  and the Variables and Watches Windows as well
          if Assigned(ThreadView.RootNode.FirstChild) then begin
            ThreadView.Selected[ThreadView.RootNode.FirstChild] := True;
            Exit;  // No need to update Variables and Watches Window twice
          end;
        end;
    dsRunning:
        begin
          if OldState in [dsPaused, dsPostMortem] then begin
          // This sequence of states happens with RunSource.  No need to update the CallStack
           ThreadView.Enabled := False;
           CallStackView.Enabled := False;
          end else
            ClearAll;
        end;
    dsDebugging, dsInactive: ClearAll;
  end;
  // Now update dependent windows
  if Assigned(VariablesWindow) then VariablesWindow.UpdateWindow;
  if Assigned(WatchesWindow) then WatchesWindow.UpdateWindow(DebuggerState);
end;

procedure TCallStackWindow.ClearAll;
begin
  ThreadView.Clear;
  SetLength(fThreads, 0);
  CallStackView.Clear;
  fCallStackList := nil;
  ThreadView.Enabled := False;
  CallStackView.Enabled := False;
end;

procedure TCallStackWindow.actNextFrameExecute(Sender: TObject);
Var
  SelectedNode : PVirtualNode;
begin
  SelectedNode := CallStackView.GetFirstSelected;
  if CallStackView.Enabled and Assigned(SelectedNode) and
    Assigned(SelectedNode.PrevSibling)
  then
    CallStackView.Selected[SelectedNode.PrevSibling] := True;
end;

procedure TCallStackWindow.actPreviousFrameExecute(Sender: TObject);
Var
  SelectedNode : PVirtualNode;
begin
  SelectedNode := CallStackView.GetFirstSelected;
  if CallStackView.Enabled and Assigned(SelectedNode) and
    Assigned(SelectedNode.NextSibling)
  then
    CallStackView.Selected[SelectedNode.NextSibling] := True;
end;

procedure TCallStackWindow.CallStackViewAddToSelection(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  if Assigned(Node) and not (tsUpdating in CallStackView.TreeStates) then
  begin
    // Update the Variables Window
    PyControl.ActiveDebugger.MakeFrameActive(
      PCallStackRec(CallStackView.GetNodeData(Node))^.FrameInfo);
    if Assigned(VariablesWindow) then VariablesWindow.UpdateWindow;
    if Assigned(WatchesWindow) then WatchesWindow.UpdateWindow(PyControl.DebuggerState);
  end;
end;

procedure TCallStackWindow.CallStackViewDblClick(Sender: TObject);
Var
  SelectedNode : PVirtualNode;
begin
  SelectedNode := CallStackView.GetFirstSelected;
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
  // Let the tree know how much data space we need.
  CallStackView.NodeDataSize := SizeOf(TCallStackRec);
end;

procedure TCallStackWindow.FormDestroy(Sender: TObject);
begin
  CallStackWindow := nil;
  inherited;
end;

procedure TCallStackWindow.CallStackViewInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
begin
  Assert(CallStackView.GetNodeLevel(Node) = 0);
  Assert(Assigned(fCallStackList));
  Assert(Integer(Node.Index) < fCallStackList.Count);
  PCallStackRec(CallStackView.GetNodeData(Node))^.FrameInfo :=
    fCallStackList[Node.Index];
end;

procedure TCallStackWindow.CallStackViewGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  if (Kind = ikState) and (vsSelected in Node.States) and (Column = 0) then
    ImageIndex := 41;
end;

procedure TCallStackWindow.CallStackViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  if TextType <> ttNormal then Exit;
  Assert(CallStackView.GetNodeLevel(Node) = 0);
  Assert(Assigned(fCallStackList));
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
Var
  SelectedNode : PVirtualNode;
begin
  Result := nil;
  SelectedNode := CallStackView.GetFirstSelected;
  if Assigned(SelectedNode) then
    Result :=
      PCallStackRec(CallStackView.GetNodeData(SelectedNode))^.FrameInfo;
end;

procedure TCallStackWindow.ThreadViewAddToSelection(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
Var
  Thread : TThreadInfo;
begin
  Thread := fThreads[Node.Index];
  CallStackView.BeginUpdate;
  try
    CallStackView.Clear;
    fCallStackList := Thread.CallStack;
    CallStackView.RootNodeCount := fCallStackList.Count;  // Fills the View
    CallStackView.ValidateNode(nil, True);
  finally
    CallStackView.EndUpdate;
  end;
  CallStackView.Enabled := True;

//  The following statement updates the Variables and Watches Windows as well
  if Assigned(CallStackView.RootNode.FirstChild) then
    CallStackView.Selected[CallStackView.RootNode.FirstChild] := True;
end;

procedure TCallStackWindow.ThreadViewGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  Assert(ThreadView.GetNodeLevel(Node) = 0);
  Assert(Integer(Node.Index) < Length(fThreads));
  if Kind in [ikNormal, ikSelected] then begin
    if fThreads[Node.Index].Status = thrdRunning then
      ImageIndex := 152
    else
      ImageIndex := 153
  end else if (Kind = ikState) and (vsSelected in Node.States) then
    ImageIndex := 41;
end;

procedure TCallStackWindow.ThreadViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  if TextType <> ttNormal then Exit;
  Assert(ThreadView.GetNodeLevel(Node) = 0);
  Assert(Integer(Node.Index) < Length(fThreads));
  CellText := fThreads[Node.Index].Name;
end;

procedure TCallStackWindow.WriteToAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
begin
  AppStorage.WriteInteger(BasePath+'\Threads Width',
   PPIUnScaled(ThreadView.Width));
  AppStorage.WriteInteger(BasePath+'\Function Width',
   PPIUnScaled(CallStackView.Header.Columns[0].Width));
  AppStorage.WriteInteger(BasePath+'\Line Width',
    PPIUnScaled(CallStackView.Header.Columns[2].Width));
end;

procedure TCallStackWindow.ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
begin
  ThreadView.Width :=
    PPIScaled(AppStorage.ReadInteger(BasePath+'\Threads Width', 140));
  CallStackView.Header.Columns[0].Width :=
    PPIScaled(AppStorage.ReadInteger(BasePath+'\Function Width', 100));
  CallStackView.Header.Columns[2].Width :=
    PPIScaled(AppStorage.ReadInteger(BasePath+'\Line Width', 50));
end;

end.


