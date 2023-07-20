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
  System.ImageList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.ActnList,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  JvComponentBase,
  JvDockControlForm,
  JvAppStorage,
  SpTBXSkins,
  SpTBXItem,
  SpTBXControls,
  SpTBXDkPanels,
  VirtualTrees.Types,
  VirtualTrees.BaseAncestorVCL,
  VirtualTrees.AncestorVCL,
  VirtualTrees.BaseTree,
  VirtualTrees,
  frmIDEDockWin,
  cPyControl,
  cPyBaseDebugger;

type
  TCallStackWindow = class(TIDEDockWindow)
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
    var fActiveThread : TThreadInfo;
    fThreads : TList<TThreadInfo>;
    procedure ThreadChangeNotify(Thread : TThreadInfo; ChangeType : TThreadChangeType);
    procedure UpdateCallStack;
    procedure SetActiveThread(const Value: TThreadInfo);
  public
    // AppStorage
    procedure StoreSettings(AppStorage: TJvCustomAppStorage); override;
    procedure RestoreSettings(AppStorage: TJvCustomAppStorage); override;

    procedure ClearAll(IncludeThreads : Boolean = True);
    function GetSelectedStackFrame : TBaseFrameInfo;
    procedure UpdateWindow(DebuggerState, OldState : TDebuggerState);
    property ActiveThread: TThreadInfo read fActiveThread write SetActiveThread;
  end;

var
  CallStackWindow: TCallStackWindow = nil;

implementation

uses
  System.Generics.Defaults,
  System.Math,
  frmVariables,
  frmWatches,
  uCommonFunctions,
  uEditAppIntfs,
  cPySupportTypes,
  dmResources;

{$R *.dfm}

{ CallStackView storage }
type
  PFrameData = ^TFrameData;
  TFrameData = record
    Func : string;
    FileName : string;
    Line : integer;
  end;

{ TCallStackWindow }

procedure TCallStackWindow.UpdateCallStack;
var
  FirstNode : PVirtualNode;
  FrameData : PFrameData;
  Editor : IEditor;
begin
  if Assigned(fActiveThread) and (fActiveThread.CallStack.Count > 0) then begin
    CallStackView.BeginUpdate;
    try
      // OutputDebugString('Call Stack filled');
      var Py := GI_PyControl.SafePyEngine;
      CallStackView.RootNodeCount := fActiveThread.CallStack.Count;  // Fills the View
      CallStackView.ReInitNode(nil, True, True);
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
        PyControl.CurrentPos := TEditorPos.NPos(Editor, FrameData.Line);
    end;
  end else
    ClearAll(False);
end;

procedure TCallStackWindow.UpdateWindow(DebuggerState, OldState : TDebuggerState);
// ThreadView is always showing while debugging
// The Call Stack is visible only when paused or in Post Mortem
// The visibility of the Call stack is controlled by ThreadChangeNotify
begin
  if GI_PyControl.PythonLoaded then
    case DebuggerState of
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
  if Assigned(VariablesWindow) then VariablesWindow.UpdateWindow;
  if Assigned(WatchesWindow) then WatchesWindow.UpdateWindow(DebuggerState);
end;

procedure TCallStackWindow.ClearAll(IncludeThreads : Boolean = True);
begin
  CallStackView.Clear;
  // OutputDebugString(PChar(Format('Call Stack cleared - RootNodeCount: %d', [CallStackView.RootNodeCount])));
  CallStackView.Enabled := False;
  if IncludeThreads then begin
    ThreadView.Clear;
    fThreads.Clear;
    ThreadView.Enabled := False;
    fActiveThread := nil;
  end;
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
  if Assigned(Node) and not CallStackView.IsUpdating then
  begin
    Assert(Assigned(fActiveThread));
    Assert(Integer(Node.Index) < fActiveThread.CallStack.Count);
    PyControl.ActiveDebugger.MakeFrameActive(fActiveThread.CallStack[Node.Index]);

    // Update the Variables Window
    if Assigned(VariablesWindow) then VariablesWindow.UpdateWindow;
    if Assigned(WatchesWindow) then WatchesWindow.UpdateWindow(PyControl.DebuggerState);
  end;
end;

procedure TCallStackWindow.CallStackViewDblClick(Sender: TObject);
var
  SelectedNode : PVirtualNode;
  FrameData : PFrameData;
begin
  SelectedNode := CallStackView.GetFirstSelected;
  if Assigned(SelectedNode) then begin
    Assert(Assigned(fActiveThread));
    Assert(Integer(SelectedNode.Index) < fActiveThread.CallStack.Count);
    FrameData := SelectedNode.GetData;
    if FrameData.FileName <> '' then
      GI_PyIDEServices.ShowFilePosition(FrameData.FileName, FrameData.Line, 1);
  end;
end;

procedure TCallStackWindow.CallStackViewFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  FrameData : PFrameData;
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
  fThreads := TList<TThreadInfo>.Create(TComparer<TThreadInfo>.Construct(
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
end;

procedure TCallStackWindow.FormDestroy(Sender: TObject);
begin
  CallStackWindow := nil;
  fThreads.Free;
  inherited;
end;

procedure TCallStackWindow.CallStackViewGetCellText(
  Sender: TCustomVirtualStringTree; var E: TVSTGetCellTextEventArgs);
var
  FrameData : PFrameData;
begin
  Assert(CallStackView.GetNodeLevel(E.Node) = 0);
  Assert(Assigned(fActiveThread));
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
  FrameData : PFrameData;
begin
  Assert(ParentNode = nil);
  Assert(Assigned(fActiveThread));
  Assert(Integer(Node.Index) < fActiveThread.CallStack.Count);
  FrameData := Node.GetData;
  with fActiveThread.CallStack[Node.Index] do
  begin
    FrameData.Func := FunctionName;
    FrameData.FileName := PyControl.ActiveInterpreter.FromPythonFileName(FileName);
    FrameData.Line := Line;
  end;
end;

function TCallStackWindow.GetSelectedStackFrame: TBaseFrameInfo;
var
  SelectedNode : PVirtualNode;
begin
  Result := nil;
  SelectedNode := CallStackView.GetFirstSelected;
  if Assigned(SelectedNode) then begin
    Assert(Assigned(fActiveThread));
    Assert(Integer(SelectedNode.Index) < fActiveThread.CallStack.Count);
    Result := fActiveThread.CallStack[SelectedNode.Index];
  end;
end;

procedure TCallStackWindow.ThreadChangeNotify(Thread: TThreadInfo;
  ChangeType : TThreadChangeType);
{
  Updates the ThreadView while debugging
  When a Thread is selected the CallStackView is filled, the Top stack gets
  selected and the Variables and Watches Windows are updated as well
}
  function NodeFromThread(T : TThreadInfo) : PVirtualNode;
  var
    I : Integer;
    N : PVirtualNode;
  begin
    Result := nil;
    if fThreads.BinarySearch(T, I) then
      for N in ThreadView.Nodes do
        if Integer(N.Index) = I then begin
          Result := N;
          break;
        end;
  end;

var
  Index : integer;
  Node,
  Node1 : PVirtualNode;
  T : TThreadInfo;
begin
  // OutputDebugString(PChar(Format('status: %d change: %d', [Ord(Thread.Status), Ord(ChangeType)])));
  var Py := GI_PyControl.SafePyEngine;
  case ChangeType of
    tctAdded:
      begin
        // Keep sorted
        if not fThreads.BinarySearch(Thread, Index) then begin
          fThreads.Insert(Index, Thread);
          ThreadView.RootNodeCount := fThreads.Count;
        end;
        if (fActiveThread = nil) and (Thread.Status = thrdBroken) then begin
          ActiveThread := Thread;
        end;
        ThreadView.Invalidate;
      end;
    tctRemoved:
      begin
        if fActiveThread = Thread then begin
          // Should not happen since a broken thread is first
          // changed to running before finishing
          // Select another broken thread
          ActiveThread := nil;
        end;
        fThreads.Remove(Thread);
        ThreadView.RootNodeCount := fThreads.Count;
        if not Assigned(fActiveThread) then begin
          for T in fThreads do
            if T.Status = thrdBroken then begin
              ActiveThread := T;
              break;
            end;
        end;
        ThreadView.Invalidate;
      end;
    tctStatusChange:
      begin
        Node := NodeFromThread(Thread);
        if Assigned(Node) then begin
          if (Thread.Status = thrdRunning) and (fActiveThread = Thread) then begin
            ActiveThread := nil;
            for T in fThreads do
              if T.Status = thrdBroken then begin
                ActiveThread := T;
                Node1 := NodeFromThread(fActiveThread);
                ThreadView.InvalidateNode(Node1);
                break;
              end;
          end
          else if Thread.Status = thrdBroken then
          begin
            if Assigned(fActiveThread) then
            begin
              Node1 := NodeFromThread(fActiveThread);
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
  Assert(Integer(Node.Index) < fThreads.Count);
  if (fThreads[Node.Index].Status = thrdBroken) and
    (fActiveThread <> fThreads[Node.Index]) then
  begin
    ActiveThread := fThreads[Node.Index];
    ThreadView.Invalidate;
  end;
end;

procedure TCallStackWindow.ThreadViewGetCellText(
  Sender: TCustomVirtualStringTree; var E: TVSTGetCellTextEventArgs);
begin
  Assert(Integer(E.Node.Index) < fThreads.Count);
  E.CellText := fThreads[E.Node.Index].Name;
end;

procedure TCallStackWindow.ThreadViewGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  Assert(Integer(Node.Index) < fThreads.Count);
  if Kind in [ikNormal, ikSelected] then begin
    if fThreads[Node.Index].Status = thrdRunning then
      ImageIndex := 1
    else
      ImageIndex := 2
  end else if (Kind = ikState) and (fThreads[Node.Index] = fActiveThread) then
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
  if fActiveThread <> Value then begin
    fActiveThread := Value;
    PyControl.ActiveDebugger.MakeThreadActive(fActiveThread);
    UpdateCallStack;
  end;
end;

end.


