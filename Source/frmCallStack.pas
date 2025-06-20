{-----------------------------------------------------------------------------
 Unit Name: frmCallStack
 Author:    Kiriakos Vlahos
 Purpose:   Call Stack Window
 History:
-----------------------------------------------------------------------------}

unit frmCallStack;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Actions,
  System.ImageList,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.ActnList,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  JvComponentBase,
  JvDockControlForm,
  JvAppStorage,
  SpTBXDkPanels,
  VirtualTrees.Types,
  VirtualTrees.BaseAncestorVCL,
  VirtualTrees.AncestorVCL,
  VirtualTrees.BaseTree,
  VirtualTrees,
  frmIDEDockWin,
  cPySupportTypes,
  cPyBaseDebugger;

type
  TCallStackWindow = class(TIDEDockWindow, ICallStackWindow)
    CallStackView: TVirtualStringTree;
    actlCallStack: TActionList;
    actPreviousFrame: TAction;
    actNextFrame: TAction;
    ThreadView: TVirtualStringTree;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSpTBXSplitter;
    vilImages: TVirtualImageList;
    procedure CallStackViewDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure actPreviousFrameExecute(Sender: TObject);
    procedure actNextFrameExecute(Sender: TObject);
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
    procedure CallStackViewInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure CallStackViewFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure CallStackViewGetCellText(Sender: TCustomVirtualStringTree;
      var E: TVSTGetCellTextEventArgs);
    procedure ThreadViewGetCellText(Sender: TCustomVirtualStringTree;
      var E: TVSTGetCellTextEventArgs);
  private
    const FBasePath = 'Call Stack Window Options'; // Used for storing settings
    var FActiveThread: TThreadInfo;
    FThreads: TList<TThreadInfo>;
    procedure ThreadChangeNotify(Thread: TThreadInfo; ChangeType: TThreadChangeType);
    procedure UpdateCallStack;
    procedure SetActiveThread(const Value: TThreadInfo);
    // ICallStackWindow Implementation
    function GetSelectedStackFrame: TBaseFrameInfo;
    procedure ClearAll(IncludeThreads: Boolean = True);
    procedure UpdateWindow(NewState, OldState: TDebuggerState);
  public
    // AppStorage
    procedure StoreSettings(AppStorage: TJvCustomAppStorage); override;
    procedure RestoreSettings(AppStorage: TJvCustomAppStorage); override;

    property ActiveThread: TThreadInfo read FActiveThread write SetActiveThread;
  end;

var
  CallStackWindow: TCallStackWindow = nil;

implementation

uses
  System.Generics.Defaults,
  System.Math,
  PythonEngine,
  uCommonFunctions,
  uEditAppIntfs,
  dmResources,
  cPyControl;

{$R *.dfm}

{ CallStackView storage }
type
  PFrameData = ^TFrameData;
  TFrameData = record
    Func: string;
    FileName: string;
    Line: Integer;
  end;

{ TCallStackWindow }

procedure TCallStackWindow.UpdateCallStack;
var
  Py: IPyEngineAndGIL;
  FirstNode: PVirtualNode;
  FrameData: PFrameData;
  Editor: IEditor;
begin
  if Assigned(FActiveThread) and (FActiveThread.CallStack.Count > 0) then begin
    CallStackView.BeginUpdate;
    try
      Py := SafePyEngine;
      CallStackView.RootNodeCount := FActiveThread.CallStack.Count;  // Fills the View
      CallStackView.ReinitNode(nil, True, True);
    finally
      CallStackView.EndUpdate;
    end;
    CallStackView.Enabled := True;

    FirstNode := CallStackView.RootNode.FirstChild;
    if Assigned(FirstNode) then begin
      //  The following statement updates the Variables and Watches Windows as well
      CallStackView.Selected[FirstNode] := True;

      // Now Show the Current debugger position
      FrameData := FirstNode.GetData;
      Editor := GI_EditorFactory.GetEditorByFileId(FrameData.FileName);
      if Assigned(Editor) then
        GI_PyControl.CurrentPos := TEditorPos.New(FrameData.FileName, FrameData.Line);
    end;
  end else
    ClearAll(False);
end;

procedure TCallStackWindow.UpdateWindow(NewState, OldState: TDebuggerState);
// ThreadView is always showing while debugging
// The Call Stack is visible only when paused or in Post Mortem
// The visibility of the Call stack is controlled by ThreadChangeNotify
begin
  if GI_PyControl.PythonLoaded then
    case NewState of
      dsPaused, dsPostMortem:
         begin
           ThreadView.Enabled := True;
           if OldState = dsRunning then begin
              // This sequence of states happens with RunSource.  No need to update the CallStack - Issue 461
              CallStackView.Enabled := True;
           end else
             // The Variables and Watches window will be updated when the
             // status of the running thread will be changed to broken
             Exit;
         end;
      dsRunning:
          begin
            if OldState in [dsPaused, dsPostMortem] then begin
            // This sequence of states happens with RunSource.  No need to update the CallStack
             ThreadView.Enabled := False;
             CallStackView.Enabled := False;
            end;
          end;
      dsDebugging:
          begin
            ThreadView.Enabled := True;
            ClearAll(False);
          end;
      dsInactive: ClearAll;
    end
  else
     ClearAll;
  // Now update dependent windows
  if Assigned(GI_VariablesWindow) then GI_VariablesWindow.UpdateWindow;
  if Assigned(GI_WatchManager) then GI_WatchManager.UpdateWindow;
end;

procedure TCallStackWindow.ClearAll(IncludeThreads: Boolean = True);
begin
  CallStackView.Clear;
  CallStackView.Enabled := False;
  if IncludeThreads then begin
    ThreadView.Clear;
    FThreads.Clear;
    ThreadView.Enabled := False;
    FActiveThread := nil;
  end;
end;

procedure TCallStackWindow.actNextFrameExecute(Sender: TObject);
begin
  var SelectedNode := CallStackView.GetFirstSelected;
  if CallStackView.Enabled and Assigned(SelectedNode) and
    Assigned(SelectedNode.PrevSibling)
  then
    CallStackView.Selected[SelectedNode.PrevSibling] := True;
end;

procedure TCallStackWindow.actPreviousFrameExecute(Sender: TObject);
begin
  var SelectedNode := CallStackView.GetFirstSelected;
  if CallStackView.Enabled and Assigned(SelectedNode) and
    Assigned(SelectedNode.NextSibling)
  then
    CallStackView.Selected[SelectedNode.NextSibling] := True;
end;

procedure TCallStackWindow.CallStackViewAddToSelection(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  if Assigned(Node) and not CallStackView.IsUpdating then
  begin
    Assert(Assigned(FActiveThread), 'CallStackViewAddToSelection');
    Assert(Integer(Node.Index) < FActiveThread.CallStack.Count, 'CallStackViewAddToSelection');
    PyControl.ActiveDebugger.MakeFrameActive(FActiveThread.CallStack[Node.Index]);

    // Update the Variables Window
    if Assigned(GI_VariablesWindow) then GI_VariablesWindow.UpdateWindow;
    if Assigned(GI_WatchManager) then GI_WatchManager.UpdateWindow;
  end;
end;

procedure TCallStackWindow.CallStackViewDblClick(Sender: TObject);
var
  SelectedNode: PVirtualNode;
  FrameData: PFrameData;
begin
  SelectedNode := CallStackView.GetFirstSelected;
  if Assigned(SelectedNode) then
  begin
    Assert(Assigned(FActiveThread), 'CallStackViewDblClick');
    Assert(Integer(SelectedNode.Index) < FActiveThread.CallStack.Count, 'CallStackViewDblClick');
    FrameData := SelectedNode.GetData;
    if FrameData.FileName <> '' then
      GI_PyIDEServices.ShowFilePosition(FrameData.FileName, FrameData.Line, 1);
  end;
end;

procedure TCallStackWindow.CallStackViewFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  FrameData: PFrameData;
begin
  FrameData := Node.GetData;
  Finalize(FrameData^);
end;

procedure TCallStackWindow.FormActivate(Sender: TObject);
begin
  inherited;
  if CanActuallyFocus(CallStackView) then
    CallStackView.SetFocus;
end;

procedure TCallStackWindow.FormCreate(Sender: TObject);
begin
  ImageName := 'CallStack';
  inherited;
  CallStackView.NodeDataSize := SizeOf(TFrameData);
  // Let the tree know how much data space we need.
  FThreads := TList<TThreadInfo>.Create(TComparer<TThreadInfo>.Construct(
    function(const L, R: TThreadInfo): Integer
    begin
      if L = R then
        Result := 0
      else if L.Name = 'MainThead' then
        Result := -1
      else if R.Name = 'MainThread' then
        Result := 1
      else begin
        Result := L.Name.CompareTo(R.Name);
        if Result = 0 then
          // in case two threads have the same name
          Result := CompareValue(L.Thread_ID, R.Thread_ID);
      end;
    end));

  TPyBaseDebugger.ThreadChangeNotify := ThreadChangeNotify;

  GI_CallStackWindow := Self;
end;

procedure TCallStackWindow.FormDestroy(Sender: TObject);
begin
  CallStackWindow := nil;
  FThreads.Free;
  inherited;
end;

procedure TCallStackWindow.CallStackViewGetCellText(
  Sender: TCustomVirtualStringTree; var E: TVSTGetCellTextEventArgs);
var
  FrameData: PFrameData;
begin
  Assert(CallStackView.GetNodeLevel(E.Node) = 0, 'CallStackViewGetCellText');
  Assert(Assigned(FActiveThread), 'CallStackViewGetCellText');
  FrameData := E.Node.GetData;
  case E.Column of
    0:  E.CellText := FrameData.Func;
    1:  E.CellText := FrameData.FileName;
    2:  if FrameData.Line > 0
          then E.CellText := FrameData.Line.ToString
        else
          E.CellText := '';
  end;
end;

procedure TCallStackWindow.CallStackViewGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  if (Kind = ikState) and (vsSelected in Node.States) and (Column = 0) then
    ImageIndex := 0;
end;

procedure TCallStackWindow.CallStackViewInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  FrameData: PFrameData;
begin
  Assert(ParentNode = nil, 'CallStackViewInitNode');
  Assert(Assigned(FActiveThread), 'CallStackViewInitNode');
  Assert(Integer(Node.Index) < FActiveThread.CallStack.Count, 'CallStackViewInitNode');
  FrameData := Node.GetData;
  with FActiveThread.CallStack[Node.Index] do
  begin
    FrameData.Func := FunctionName;
    FrameData.FileName := PyControl.ActiveInterpreter.FromPythonFileName(FileName);
    FrameData.Line := Line;
  end;
end;

function TCallStackWindow.GetSelectedStackFrame: TBaseFrameInfo;
var
  SelectedNode: PVirtualNode;
begin
  Result := nil;
  SelectedNode := CallStackView.GetFirstSelected;
  if Assigned(SelectedNode) then
  begin
    Assert(Assigned(FActiveThread), 'GetSelectedStackFrame');
    Assert(Integer(SelectedNode.Index) < FActiveThread.CallStack.Count, 'GetSelectedStackFrame');
    Result := FActiveThread.CallStack[SelectedNode.Index];
  end;
end;

procedure TCallStackWindow.ThreadChangeNotify(Thread: TThreadInfo;
  ChangeType: TThreadChangeType);
{
  Updates the ThreadView while debugging
  When a Thread is selected the CallStackView is filled, the Top stack gets
  selected and the Variables and Watches Windows are updated as well
}
  function NodeFromThread(ThreadInfo: TThreadInfo): PVirtualNode;
  var
    Idx: Integer;
    Node: PVirtualNode;
  begin
    Result := nil;
    if FThreads.BinarySearch(ThreadInfo, Idx) then
      for Node in ThreadView.Nodes do
        if Integer(Node.Index) = Idx then
          Exit(Node);
  end;

var
  Py: IPyEngineAndGIL;
  Index: Integer;
  Node,
  Node1: PVirtualNode;
  ThreadInfo: TThreadInfo;
begin
  Py := SafePyEngine;
  case ChangeType of
    tctAdded:
      begin
        // Keep sorted
        if not FThreads.BinarySearch(Thread, Index) then begin
          FThreads.Insert(Index, Thread);
          ThreadView.RootNodeCount := FThreads.Count;
        end;
        if (FActiveThread = nil) and (Thread.Status = thrdBroken) then begin
          ActiveThread := Thread;
        end;
        ThreadView.Invalidate;
      end;
    tctRemoved:
      begin
        if FActiveThread = Thread then begin
          // Should not happen since a broken thread is first
          // changed to running before finishing
          // Select another broken thread
          ActiveThread := nil;
        end;
        FThreads.Remove(Thread);
        ThreadView.RootNodeCount := FThreads.Count;
        if not Assigned(FActiveThread) then begin
          for ThreadInfo in FThreads do
            if ThreadInfo.Status = thrdBroken then begin
              ActiveThread := ThreadInfo;
              Break;
            end;
        end;
        ThreadView.Invalidate;
      end;
    tctStatusChange:
      begin
        Node := NodeFromThread(Thread);
        if Assigned(Node) then begin
          if (Thread.Status = thrdRunning) and (FActiveThread = Thread) then begin
            ActiveThread := nil;
            for ThreadInfo in FThreads do
              if ThreadInfo.Status = thrdBroken then begin
                ActiveThread := ThreadInfo;
                Node1 := NodeFromThread(FActiveThread);
                ThreadView.InvalidateNode(Node1);
                Break;
              end;
          end
          else if Thread.Status = thrdBroken then
          begin
            if Assigned(FActiveThread) then
            begin
              Node1 := NodeFromThread(FActiveThread);
              ThreadView.InvalidateNode(Node1);
            end;
            ActiveThread := Thread;
            ThreadView.ScrollIntoView(Node, False);
          end;
          ThreadView.InvalidateNode(Node);
        end;
      end;
  end;
end;

procedure TCallStackWindow.ThreadViewAddToSelection(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  Assert(Integer(Node.Index) < FThreads.Count, 'ThreadViewAddToSelection');
  if (FThreads[Node.Index].Status = thrdBroken) and
    (FActiveThread <> FThreads[Node.Index]) then
  begin
    ActiveThread := FThreads[Node.Index];
    ThreadView.Invalidate;
  end;
end;

procedure TCallStackWindow.ThreadViewGetCellText(
  Sender: TCustomVirtualStringTree; var E: TVSTGetCellTextEventArgs);
begin
  Assert(Integer(E.Node.Index) < FThreads.Count, 'ThreadViewGetCellText');
  E.CellText := FThreads[E.Node.Index].Name;
end;

procedure TCallStackWindow.ThreadViewGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  Assert(Integer(Node.Index) < FThreads.Count, 'ThreadViewGetImageIndex');
  if Kind in [ikNormal, ikSelected] then begin
    if FThreads[Node.Index].Status = thrdRunning then
      ImageIndex := 1
    else
      ImageIndex := 2;
  end else if (Kind = ikState) and (FThreads[Node.Index] = FActiveThread) then
    ImageIndex := 0;
end;

procedure TCallStackWindow.StoreSettings(AppStorage: TJvCustomAppStorage);
begin
  inherited;
  AppStorage.WriteInteger(FBasePath+'\Threads Width',
   PPIUnScale(ThreadView.Width));
  AppStorage.WriteInteger(FBasePath+'\Function Width',
   PPIUnScale(CallStackView.Header.Columns[0].Width));
  AppStorage.WriteInteger(FBasePath+'\Line Width',
    PPIUnScale(CallStackView.Header.Columns[2].Width));
end;

procedure TCallStackWindow.RestoreSettings(AppStorage: TJvCustomAppStorage);
begin
  inherited;
  ThreadView.Width :=
    PPIScale(AppStorage.ReadInteger(FBasePath+'\Threads Width', 140));
  CallStackView.Header.Columns[0].Width :=
    PPIScale(AppStorage.ReadInteger(FBasePath+'\Function Width', 100));
  CallStackView.Header.Columns[2].Width :=
    PPIScale(AppStorage.ReadInteger(FBasePath+'\Line Width', 50));
end;

procedure TCallStackWindow.SetActiveThread(const Value: TThreadInfo);
begin
  if FActiveThread <> Value then begin
    FActiveThread := Value;
    PyControl.ActiveDebugger.MakeThreadActive(FActiveThread);
    UpdateCallStack;
  end;
end;

end.


