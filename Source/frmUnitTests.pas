unit frmUnitTests;

interface

uses
  System.UITypes, System.ImageList, Windows, Messages, SysUtils, Variants,
  System.Actions, Classes, Graphics, Controls, Forms, Dialogs, frmIDEDockWin,
  JvComponentBase, JvDockControlForm, ExtCtrls, ImgList, SpTBXDkPanels, VirtualTrees,
  TB2Item, TB2Dock, TB2Toolbar, StdCtrls,   ActnList, SpTBXControls, SpTBXItem,
  ComCtrls, SpTBXSkins;

type
  TUnitTestWindowStatus = (utwEmpty, utwLoaded, utwRunning, utwRun);

  TUnitTestWindow = class(TIDEDockWindow)
    ExplorerDock: TSpTBXDock;
    ExplorerToolbar: TSpTBXToolbar;
    RunImages: TImageList;
    tbiRefresh: TSpTBXItem;
    TBXSeparatorItem1: TSpTBXSeparatorItem;
    tbiSelectFailed: TSpTBXItem;
    tbiDeselectAll: TSpTBXItem;
    tbiSelectAll: TSpTBXItem;
    TBXSeparatorItem2: TSpTBXSeparatorItem;
    tbiRun: TSpTBXItem;
    tbiStop: TSpTBXItem;
    tbiCollapseAll: TSpTBXItem;
    tbiExpandAll: TSpTBXItem;
    TBXSeparatorItem7: TSpTBXSeparatorItem;
    tbiClearAll: TSpTBXItem;
    SpTBXSplitter1: TSpTBXSplitter;
    DialogActions: TActionList;
    actClearAll: TAction;
    actCollapseAll: TAction;
    actExpandAll: TAction;
    actSelectFailed: TAction;
    actDeselectAll: TAction;
    actSelectAll: TAction;
    actStop: TAction;
    actRun: TAction;
    actRefresh: TAction;
    Panel1: TPanel;
    UnitTests: TVirtualStringTree;
    Panel2: TPanel;
    Bevel1: TBevel;
    Label2: TLabel;
    ModuleName: TLabel;
    lbFoundTests: TLabel;
    lblRunTests: TLabel;
    lblFailures: TLabel;
    SpTBXPanel1: TPanel;
    ErrorText: TRichEdit;
    procedure UnitTestsDblClick(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actClearAllExecute(Sender: TObject);
    procedure actSelectFailedExecute(Sender: TObject);
    procedure UnitTestsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure actRunExecute(Sender: TObject);
    procedure UnitTestsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure actDeselectAllExecute(Sender: TObject);
    procedure actCollapseAllExecute(Sender: TObject);
    procedure actExpandAllExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure UnitTestsGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: string);
    procedure UnitTestsGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure UnitTestsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure UnitTestsInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure UnitTestsInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
    TestClasses : TStringList;
    TestSuite, TestResult : Variant;
  protected
    procedure UpdateActions; override;
  public
    { Public declarations }
    Status : TUnitTestWindowStatus;
    TestsRun, TestsFailed, TestErrors : integer;
    ElapsedTime : double;
    procedure ClearAll;
    procedure StartTest(Test: Variant);
    procedure StopTest(Test: Variant);
    procedure AddError(Test, Err: Variant);
    procedure AddFailure(Test, Err: Variant);
    procedure AddSuccess(Test: Variant);
    function SelectedTestCount : integer;
    function FindTestNode(Test : Variant) : PVirtualNode;
  end;

var
  UnitTestWindow: TUnitTestWindow;

implementation

uses uCommonFunctions, frmPyIDEMain, VarPyth, JvJVCLUtils,
  uEditAppIntfs, PythonEngine, dmCommands, cPyBaseDebugger, JclSysUtils,
  cPyDebugger, StringResources, JvGnugettext;

{$R *.dfm}

{ Indexes of the color images used in the test tree and failure list }

Type
  TTestStatus = (tsNotRun, tsRunning, tsRun, tsFailed, tsError);

  PNodeDataRec = ^TNodeDataRec;
  TNodeDataRec = record
  end;

Const
  FoundTestsLabel = 'Found %d test%s';
  RunTestsLabel = 'Ran %d test%s%s';
  ElapsedTimeFormat = ' in %.3fs';
  FailuresLabel = 'Failures/Errors : %d/%d';
{ TUnitTestWindow }

procedure TUnitTestWindow.actRefreshExecute(Sender: TObject);
Var
  i, j, Index : integer;
  Editor : IEditor;
  UnitTest, Module, InnerTestSuite,
  TestCase : Variant;
  ClassName, TestName : string;
  SL : TStringList;
  Cursor : IInterface;
  PyTestCase : PPyObject;
  TestCount : integer;
begin
  ClearAll;
  Editor := PyIDEMainForm.GetActiveEditor;
  if Assigned(Editor) then begin
    Cursor := WaitCursor;

    ModuleName.Caption := 'Module: ' + Editor.FileTitle;
    ModuleName.Hint := Editor.FileName;

    Module := PyControl.ActiveInterpreter.ImportModule(Editor);
    UnitTest := PyControl.ActiveInterpreter.EvalCode('__import__("unittest")');
    TestSuite := UnitTest.findTestCases(Module);
    //  This TestSuite contains a list of TestSuites
    //  each of which contains TestCases corresponding to
    //  a TestCase class in the module!!!
    TestCount := 0;
    for i := 0 to Len(TestSuite._tests) - 1 do begin
      InnerTestSuite := TestSuite._tests.__getitem__(i);
      for j := 0 to Len(InnerTestSuite._tests) - 1 do begin
        TestCase := InnerTestSuite._tests.__getitem__(j);
        //  set the TestStatus
        TestCase.testStatus := Ord(tsNotRun);
        TestCase.errMsg := string('');
        TestCase.enabled := True;
        ClassName := PyControl.ActiveInterpreter.GetObjectType(TestCase);
        Index := TestClasses.IndexOf(ClassName);
        if Index < 0 then begin
          SL := TStringList.Create;
          Index := TestClasses.AddObject(ClassName, SL);
        end;
        PyTestCase := ExtractPythonObjectFrom(TestCase); // Store the TestCase PPyObject
        GetPythonEngine.Py_XINCREF(PyTestCase);

        // Python 2.4
        if GetPythonEngine.PyObject_HasAttrString(PyTestCase, '_testMethodName') = 1 then
          //Python 2.5
          TestName := TestCase._testMethodName
        else
          // earlier versions - hack to access a private variable!
          TestName := TestCase._TestCase__testMethodName;

        TStringList(TestClasses.Objects[Index]).AddObject(TestName, TObject(PyTestCase));
        Inc(TestCount);
      end;
    end;

    if TestCount = 0 then begin
      Dialogs.MessageDlg(_(SNoTestsFound), mtWarning, [mbOK], 0);
      ClearAll;
    end else begin
      UnitTests.RootNodeCount := TestClasses.Count;
      UnitTests.ReinitNode(UnitTests.RootNode, True);
      lbFoundTests.Caption := Format(FoundTestsLabel, [TestCount, Iff(TestCount=1, '', 's')]);
      Status := utwLoaded;
      actSelectAllExecute(Self);
    end;
  end;
end;

procedure TUnitTestWindow.FormActivate(Sender: TObject);
begin
  inherited;
  if UnitTests.CanFocus then
    UnitTests.SetFocus;
  //PostMessage(UnitTests.Handle, WM_SETFOCUS, 0, 0);
end;

procedure TUnitTestWindow.FormCreate(Sender: TObject);
begin
  inherited;
  ScaleImageList(RunImages, Screen.PixelsPerInch, 96);

  UnitTests.NodeDataSize := SizeOf(TNodeDataRec);

  TestClasses := TStringList.Create;
  TestClasses.Sorted := True;
  TestClasses.Duplicates := dupError;

  Status := utwEmpty;
end;

procedure TUnitTestWindow.FormDestroy(Sender: TObject);
begin
  ClearAll;
  TestClasses.Free;
  inherited;
end;

procedure TUnitTestWindow.ClearAll;
Var
  i, j : integer;
  SL : TStringList;
  PyTestCase : PPyObject;
begin
  UnitTests.Clear;
  for i := 0 to TestClasses.Count - 1 do begin
    SL := TStringList(TestClasses.Objects[i]);
    for j := 0 to SL.Count - 1 do with GetPythonEngine do begin
      PyTestCase := PPyObject(SL.Objects[j]);
      Py_XDECREF(PyTestCase);
    end;
    SL.Free;
  end;
  TestClasses.Clear;
  VarClear(TestSuite);

  TestsRun := 0;
  TestsFailed := 0;
  TestErrors := 0;
  ElapsedTime := 0;
  lblRunTests.Caption := Format(RunTestsLabel, [TestsRun, Iff(TestsRun=1, '', 's'), '']);
  lblFailures.Caption := Format(FailuresLabel, [TestsFailed, TestErrors]);

  ModuleName.Caption := 'No Module Loaded';
  ModuleName.Hint := '';

  lbFoundTests.Caption := Format(FoundTestsLabel, [0, 's']);

  Status := utwEmpty;
end;

procedure TUnitTestWindow.UnitTestsInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  if UnitTests.GetNodeLevel(Node) = 0 then begin
    Node.CheckType := ctTriStateCheckBox;
    if TStringList(TestClasses.Objects[Node.Index]).Count > 0 then
      InitialStates := [ivsHasChildren, ivsExpanded]
    else
      InitialStates := []
  end else begin
    Node.CheckType := ctCheckBox;
    InitialStates := [];
  end;
end;

procedure TUnitTestWindow.UnitTestsInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
begin
  if UnitTests.GetNodeLevel(Node) = 0 then
    ChildCount := TStringList(TestClasses.Objects[Node.Index]).Count
  else
    ChildCount := 0;
end;

procedure TUnitTestWindow.UnitTestsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  if UnitTests.GetNodeLevel(Node) = 0 then
    CellText := TestClasses[Node.Index]
  else
    CellText := TStringList(TestClasses.Objects[Node.Parent.Index])[Node.Index]
end;

procedure TUnitTestWindow.UnitTestsGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  PyTestCase, PytestStatus : PPyObject;
begin
  if not (Kind in [ikNormal, ikSelected]) then exit;
  if UnitTests.GetNodeLevel(Node) = 0 then begin
    if vsExpanded in Node.States then
      ImageIndex := 5
    else
      ImageIndex := 6;
  end else with GetPythonEngine do begin
    PyTestCase := PPyObject(TStringList(TestClasses.Objects[Node.Parent.Index]).Objects[Node.Index]);
    PytestStatus := PyObject_GetAttrString(PyTestCase, 'testStatus');
    CheckError;
    ImageIndex := PyLong_AsLong(PytestStatus);
    Py_XDECREF(PytestStatus);
  end;
end;

procedure TUnitTestWindow.UnitTestsGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
var
  PyTestCase : PPyObject;
  TestCase : Variant;
begin
  HintText := '';
  if UnitTests.GetNodeLevel(Node) = 0 then begin
    if Assigned(Node.FirstChild) then begin
      PyTestCase := PPyObject(TStringList(TestClasses.Objects[Node.Index]).Objects[0]);
      TestCase := VarPythonCreate(PyTestCase);
      if not VarIsNone(TestCase.__doc__) then
        HintText := FormatDocString(TestCase.__doc__);
    end;
  end else with GetPythonEngine do begin
    PyTestCase := PPyObject(TStringList(TestClasses.Objects[Node.Parent.Index]).Objects[Node.Index]);
    TestCase := VarPythonCreate(PyTestCase);
    if not VarIsNone(TestCase.shortDescription()) then
      HintText := TestCase.shortDescription();
  end;
end;

procedure TUnitTestWindow.UnitTestsChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  PyTestCase : PPyObject;
  TestCase : Variant;
begin
  if (UnitTests.GetNodeLevel(Node) = 1) and (vsInitialized in Node.States) then begin
    PyTestCase := PPyObject(TStringList(TestClasses.Objects[Node.Parent.Index]).Objects[Node.Index]);
    TestCase := VarPythonCreate(PyTestCase);
    TestCase.enabled := Node.CheckState in [csCheckedNormal, csCheckedPressed];
  end;
end;

procedure TUnitTestWindow.actSelectAllExecute(Sender: TObject);
Var
  Node : PVirtualNode;
begin
   Node := UnitTests.RootNode^.FirstChild;
   while Assigned(Node) do begin
     UnitTests.CheckState[Node] := csCheckedNormal;
     Node := Node.NextSibling;
   end;
end;

procedure TUnitTestWindow.actDeselectAllExecute(Sender: TObject);
Var
  Node : PVirtualNode;
begin
   Node := UnitTests.RootNode^.FirstChild;
   while Assigned(Node) do begin
     UnitTests.CheckState[Node] := csUncheckedNormal;
     Node := Node.NextSibling;
   end;
end;

procedure TUnitTestWindow.actSelectFailedExecute(Sender: TObject);
Var
  PyTestCase : PPyObject;
  TestCase : Variant;
  ClassNode, TestCaseNode : PVirtualNode;
begin
  actDeselectAllExecute(Sender);

  ClassNode := UnitTests.RootNode^.FirstChild;
  while Assigned(ClassNode) do begin
    TestCaseNode := ClassNode.FirstChild;
    while Assigned(TestCaseNode) do begin
      PyTestCase :=
        PPyObject(TStringList(TestClasses.Objects[TestCaseNode.Parent.Index]).Objects[TestCaseNode.Index]);
      TestCase := VarPythonCreate(PyTestCase);
      if TTestStatus(TestCase.testStatus) in [tsFailed, tsError] then
         UnitTests.CheckState[TestCaseNode] := csCheckedNormal;
      TestCaseNode := TestCaseNode.NextSibling;
    end;
    ClassNode := ClassNode.NextSibling;
  end;
end;

procedure TUnitTestWindow.actExpandAllExecute(Sender: TObject);
begin
  UnitTests.FullExpand;
end;

procedure TUnitTestWindow.actCollapseAllExecute(Sender: TObject);
begin
  UnitTests.FullCollapse;
end;

procedure TUnitTestWindow.actRunExecute(Sender: TObject);
Var
  UnitTestModule, TempTestSuite : Variant;
  PyTestCase : PPyObject;
  TestCase : Variant;
  ClassNode, TestCaseNode : PVirtualNode;
  StartTime, StopTime, Freq : Int64;
begin
  // Only allow when PyControl.ActiveDebugger is inactive
  if PyControl.DebuggerState <> dsInactive then Exit;

  // bugfix for Python 2.6 or higher
  //if (PyControl.PythonVersionIndex >= 10) and (PyControl.ActiveInterpreter is TPyRemoteInterpreter) then
  //  BuiltinModule.issubclass := Import('Rpyc').issubclass;

  UnitTestModule := PyControl.ActiveInterpreter.EvalCode('__import__("unittest")');

  //  Create a TempTestSuite that contains only the checked tests
  TempTestSuite := UnitTestModule.TestSuite();

  ClassNode := UnitTests.RootNode^.FirstChild;
  while Assigned(ClassNode) do begin
    TestCaseNode := ClassNode.FirstChild;
    while Assigned(TestCaseNode) do begin
      PyTestCase :=
        PPyObject(TStringList(TestClasses.Objects[TestCaseNode.Parent.Index]).Objects[TestCaseNode.Index]);
      TestCase := VarPythonCreate(PyTestCase);
      TestCase.testStatus := Ord(tsNotRun);
      TestCase.errMsg := string('');
      if TestCase.enabled then
        TempTestSuite._tests.append(TestCase);
      TestCaseNode := TestCaseNode.NextSibling;
    end;
    ClassNode := ClassNode.NextSibling;
  end;

  TestsRun := 0;
  TestsFailed := 0;
  TestErrors := 0;
  ElapsedTime := 0;
  lblRunTests.Caption := Format(RunTestsLabel, [TestsRun, Iff(TestsRun=1, '', 's'), '']);
  lblFailures.Caption := Format(FailuresLabel, [TestsFailed, TestErrors]);

  Status := utwRunning;
  UpdateActions;
  PyIDEMainForm.DebuggerStateChange(Self, dsInactive, dsRunning);
  Application.ProcessMessages;

  TestResult := PyControl.ActiveInterpreter.UnitTestResult();
  try
    QueryPerformanceCounter(StartTime);
    TempTestSuite.run(TestResult);
    QueryPerformanceCounter(StopTime);
  finally
    if QueryPerformanceFrequency(Freq) then
      ElapsedTime := (StopTime-StartTime) / Freq
    else
      ElapsedTime := 0;
    VarClear(TestResult);
    VarClear(TempTestSuite);
    Status := utwRun;
    PyIDEMainForm.DebuggerStateChange(Self, dsRunning, dsInactive);
    lblRunTests.Caption := Format(RunTestsLabel,
      [TestsRun, Iff(TestsRun=1, '', 's'), Format(ElapsedTimeFormat, [ElapsedTime])]);
  end;
  // bugfix for Python 2.6 or higher - Restore original
  //if (PyControl.PythonVersionIndex >= 10) and (PyControl.ActiveInterpreter is TPyRemoteInterpreter) then
  //  BuiltinModule.issubclass := Import('Rpyc.Lib').orig_issubclass;
end;

procedure TUnitTestWindow.AddFailure(Test, Err: Variant);
// Called from IDETestResult
Var
  TestCaseNode : PVirtualNode;
begin
  Test.testStatus := Ord(tsFailed);
  Test.errMsg := Err;
  TestCaseNode := FindTestNode(Test);
  if Assigned(TestCaseNode) then begin
    UnitTests.ScrollIntoView(TestCaseNode, True);
    UnitTests.Refresh;
  end;
  Inc(TestsFailed);
  lblFailures.Caption := Format(FailuresLabel, [TestsFailed, TestErrors]);
  Application.ProcessMessages;
end;

procedure TUnitTestWindow.AddSuccess(Test: Variant);
// Called from IDETestResult
Var
  TestCaseNode : PVirtualNode;
begin
  Test.testStatus := Ord(tsRun);
  TestCaseNode := FindTestNode(Test);
  if Assigned(TestCaseNode) then begin
    UnitTests.ScrollIntoView(TestCaseNode, True);
    UnitTests.Refresh;
  end;
  Application.ProcessMessages;
end;

procedure TUnitTestWindow.StopTest(Test: Variant);
// Called from IDETestResult
begin
  Inc(TestsRun);
  lblRunTests.Caption := Format(RunTestsLabel, [TestsRun, Iff(TestsRun=1, '', 's'), '']);
  Application.ProcessMessages;
end;

procedure TUnitTestWindow.StartTest(Test: Variant);
// Called from IDETestResult
Var
  TestCaseNode : PVirtualNode;
begin
  Test.testStatus := Ord(tsRunning);
  TestCaseNode := FindTestNode(Test);
  if Assigned(TestCaseNode) then begin
    UnitTests.ScrollIntoView(TestCaseNode, True);
    UnitTests.Refresh;
    Application.ProcessMessages;
  end;
end;

procedure TUnitTestWindow.AddError(Test, Err: Variant);
// Called from IDETestResult
Var
  TestCaseNode : PVirtualNode;
begin
  Test.testStatus := Ord(tsError);
  Test.errMsg := Err;
  TestCaseNode := FindTestNode(Test);
  if Assigned(TestCaseNode) then begin
    UnitTests.ScrollIntoView(TestCaseNode, True);
    UnitTests.Refresh;
  end;
  Inc(TestErrors);
  lblFailures.Caption := Format(FailuresLabel, [TestsFailed, TestErrors]);
  Application.ProcessMessages;
end;

function TUnitTestWindow.FindTestNode(Test: Variant): PVirtualNode;
Var
  PyTestCase : PPyObject;
  TestCase : Variant;
  ClassNode, TestCaseNode : PVirtualNode;
begin
  Result := nil;
  ClassNode := UnitTests.RootNode^.FirstChild;
  while Assigned(ClassNode) do begin
    TestCaseNode := ClassNode.FirstChild;
    while Assigned(TestCaseNode) do begin
      PyTestCase :=
        PPyObject(TStringList(TestClasses.Objects[TestCaseNode.Parent.Index]).Objects[TestCaseNode.Index]);
      TestCase := VarPythonCreate(PyTestCase);
      if VarIsSame(Test, TestCase) then begin
        Result := TestCaseNode;
        Break;
      end;
      TestCaseNode := TestCaseNode.NextSibling;
    end;
    ClassNode := ClassNode.NextSibling;
  end;
end;

procedure TUnitTestWindow.UnitTestsChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
Var
  PyTestCase : PPyObject;
  TestCase : Variant;
begin
  if Assigned(Node) and (vsSelected in Node.States) and
    (UnitTests.GetNodeLevel(Node) = 1) then
  begin
    PyTestCase := PPyObject(TStringList(TestClasses.Objects[Node.Parent.Index]).Objects[Node.Index]);
    TestCase := VarPythonCreate(PyTestCase);
    ErrorText.Text := TestCase.errMsg;
  end else
    ErrorText.Text := '';
end;

procedure TUnitTestWindow.actClearAllExecute(Sender: TObject);
begin
  ClearAll;
end;

function TUnitTestWindow.SelectedTestCount: integer;
Var
  ClassNode, TestCaseNode : PVirtualNode;
begin
  Result := 0;
  ClassNode := UnitTests.RootNode^.FirstChild;
  while Assigned(ClassNode) do begin
    TestCaseNode := ClassNode.FirstChild;
    while Assigned(TestCaseNode) do begin
      if TestCaseNode.CheckState in [csCheckedNormal, csCheckedPressed] then
        Inc(Result);
      TestCaseNode := TestCaseNode.NextSibling;
    end;
    ClassNode := ClassNode.NextSibling;
  end;
end;

procedure TUnitTestWindow.UpdateActions;
Var
  Count : integer;
begin
  Count := SelectedTestCount;

  actRun.Enabled := (Status in [utwLoaded, utwRun]) and (Count > 0) and
    (PyControl.DebuggerState = dsInactive);

  actSelectAll.Enabled := Status in [utwLoaded, utwRun];
  actDeselectAll.Enabled := Status in [utwLoaded, utwRun];
  actSelectFailed.Enabled := Status = utwRun;

  actRefresh.Enabled := Status <> utwRunning;

  actExpandAll.Enabled := Status <> utwEmpty;
  actCollapseAll.Enabled := Status <> utwEmpty;

  actClearAll.Enabled := not (Status in [utwEmpty, utwRunning]);

  actStop.Enabled := Status = utwRunning;
  inherited;
end;

procedure TUnitTestWindow.actStopExecute(Sender: TObject);
begin
  if VarIsPython(TestResult) then
    TestResult.stop();
end;

procedure TUnitTestWindow.UnitTestsDblClick(Sender: TObject);
var
  InspectModule : Variant;
  Node : PVirtualNode;
  PyTestCase : PPyObject;
  TestCase, PythonObject : Variant;
  FileName, TestName : string;
  NodeLevel, LineNo : integer;
begin
  Node := UnitTests.HotNode;
  if Assigned(Node) then begin
    NodeLevel := UnitTests.GetNodeLevel(Node);
    if NodeLevel = 0 then
      Node := Node.FirstChild;
    if (NodeLevel > 1)  or not Assigned(Node) then Exit;
    PyTestCase := PPyObject(TStringList(TestClasses.Objects[Node.Parent.Index]).Objects[Node.Index]);
    TestCase := VarPythonCreate(PyTestCase);
    if NodeLevel = 0 then
      PythonObject := TestCase.__class__
    else begin
      if GetPythonEngine.PyObject_HasAttrString(PyTestCase, '_testMethodName') = 1 then
        //Python 2.5
        TestName := TestCase._testMethodName
      else
        // earlier versions - hack to access a private variable!
        TestName := TestCase._TestCase__testMethodName;
      PythonObject := BuiltinModule.getattr(TestCase, TestName);
    end;
    if VarIsPython(PythonObject) then begin
      InspectModule := PyControl.ActiveInterpreter.EvalCode('__import__("inspect")');
      if InspectModule.ismethod(PythonObject) then begin
        FileName := InspectModule.getsourcefile(PythonObject);
        if FileName = 'None' then begin
          if GetPythonEngine.IsPython3000 then begin
            FileName := PythonObject.__func__.__code__.co_filename;
            if ExtractFileExt(FileName) <> '' then
              Exit;
            // otherwise it should be an unsaved file
            LineNo := PythonObject.__func__.__code__.co_firstlineno-1;
          end else begin
            FileName := PythonObject.im_func.func_code.co_filename;
            if ExtractFileExt(FileName) <> '' then
              Exit;
            // otherwise it should be an unsaved file
            LineNo := PythonObject.im_func.func_code.co_firstlineno-1;
          end;
        end else
          LineNo := InspectModule.findsource(PythonObject).__getitem__(1);
        PyIDEMainForm.ShowFilePosition(FileName, Succ(LineNo), 1);
      end;
    end;
  end;
end;

end.
