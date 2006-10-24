unit frmUnitTests;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmIDEDockWin, JvComponentBase, JvDockControlForm, ExtCtrls, ImgList,
  JvExControls, JvComponent, JvLinkLabel, TBXDkPanels, VirtualTrees,
  JvExExtCtrls, JvNetscapeSplitter, TBXStatusBars, TB2Item, TBXExtItems,
  TB2Dock, TB2Toolbar, TBX, StdCtrls, JvExStdCtrls, JvRichEdit, mbTBXJvRichEdit,
  ActnList, TBXThemes;

type
  TUnitTestWindowStatus = (utwEmpty, utwLoaded, utwRunning, utwRun);

  TUnitTestWindow = class(TIDEDockWindow)
    ExplorerDock: TTBXDock;
    ExplorerToolbar: TTBXToolbar;
    Panel1: TPanel;
    RunImages: TImageList;
    UnitTests: TVirtualStringTree;
    DialogActions: TActionList;
    actRun: TAction;
    actStop: TAction;
    actSelectAll: TAction;
    actDeselectAll: TAction;
    actSelectFailed: TAction;
    actRefresh: TAction;
    TBXItem1: TTBXItem;
    TBXSeparatorItem1: TTBXSeparatorItem;
    TBXItem2: TTBXItem;
    TBXItem3: TTBXItem;
    TBXItem4: TTBXItem;
    TBXSeparatorItem2: TTBXSeparatorItem;
    TBXItem6: TTBXItem;
    TBXItem7: TTBXItem;
    Panel2: TPanel;
    ErrorText: TmbTBXJvRichEdit;
    Label2: TLabel;
    ModuleName: TLabel;
    Splitter: TJvNetscapeSplitter;
    actExpandAll: TAction;
    actCollapseAll: TAction;
    TBXItem10: TTBXItem;
    TBXItem8: TTBXItem;
    TBXSeparatorItem7: TTBXSeparatorItem;
    actClearAll: TAction;
    TBXItem5: TTBXItem;
    lbFoundTests: TLabel;
    lblRunTests: TLabel;
    lblFailures: TLabel;
    Bevel1: TBevel;
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
      var HintText: WideString);
    procedure UnitTestsGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure UnitTestsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure UnitTestsInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure UnitTestsInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
  private
    { Private declarations }
    TestClasses : TStringList;
    TestSuite, TestResult : Variant;
  protected
    procedure TBMThemeChange(var Message: TMessage); message TBM_THEMECHANGE;
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

uses uCommonFunctions, uHighlighterProcs, frmPyIDEMain, VarPyth, JvJVCLUtils,
  uEditAppIntfs, PythonEngine, frmPythonII, dmCommands, cPyBaseDebugger, JclSysUtils;

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

procedure TUnitTestWindow.TBMThemeChange(var Message: TMessage);
begin
  inherited;
  if Message.WParam = TSC_VIEWCHANGE then begin
    FGPanel.Color := CurrentTheme.GetItemColor(GetItemInfo('inactive'));
    Splitter.ButtonColor :=
      CurrentTheme.GetItemColor(GetItemInfo('inactive'));
    Splitter.ButtonHighlightColor :=
      CurrentTheme.GetItemColor(GetItemInfo('active'));
  end;
end;

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

    Module := PyIDEMainForm.PyDebugger.ImportModule(Editor);
    UnitTest := Import('unittest');
    TestSuite := UnitTest.findTestCases(Module);
    //  This TestSuite contains a list of TestSuites
    //  each of which contains TestCases corresponding to
    //  a TestCase class in the module!!!
    TestCount := 0;
    for i := 0 to Len(TestSuite._tests) - 1 do begin
      InnerTestSuite := TestSuite._tests[i];
      for j := 0 to Len(InnerTestSuite._tests) - 1 do begin
        TestCase := InnerTestSuite._tests[j];
        //  set the TestStatus
        TestCase.testStatus := Ord(tsNotRun);
        TestCase.errMsg := '';
        TestCase.enabled := True;
        ClassName := TestCase.__class__.__name__;
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
      MessageDlg('No tests found!', mtWarning, [mbOK], 0);
      ClearAll;
    end else begin
      UnitTests.RootNodeCount := TestClasses.Count;
      UnitTests.ReinitNode(UnitTests.RootNode, True);
      lbFoundTests.Caption := Format(FoundTestsLabel, [TestCount, Iff(TestCount=1, '', 's')]);
      Status := utwLoaded;
    end;
  end;
end;

procedure TUnitTestWindow.FormCreate(Sender: TObject);
begin
  inherited;
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
  Node.CheckState := csCheckedNormal;
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
  var CellText: WideString);
begin
  if UnitTests.GetNodeLevel(Node) = 0 then
    CellText := TestClasses[Node.Index]
  else
    CellText := TStringList(TestClasses.Objects[Node.Parent.Index])[Node.Index]
end;

procedure TUnitTestWindow.UnitTestsGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
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
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: WideString);
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
  if UnitTests.GetNodeLevel(Node) = 1 then begin
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
  // Only allow when PyDebugger is inactive
  if PyIDEMainForm.PyDebugger.DebuggerState <> dsInactive then Exit;
  UnitTestModule := Import('unittest');

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
      TestCase.errMsg := '';
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
  PyIDEMainForm.DebuggerStateChange(Self, dsInactive, dsRunningNoDebug);
  Application.ProcessMessages;
  TestResult := PythonIIForm.II.IDETestResult();
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
    Status := utwRun;
    PyIDEMainForm.DebuggerStateChange(Self, dsRunningNoDebug, dsInactive);
    lblRunTests.Caption := Format(RunTestsLabel,
      [TestsRun, Iff(TestsRun=1, '', 's'), Format(ElapsedTimeFormat, [ElapsedTime])]);
  end;
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
    (PyIDEMainForm.PyDebugger.DebuggerState = dsInactive);

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
      InspectModule := Import('inspect');
      if InspectModule.ismethod(PythonObject) then begin
        FileName := InspectModule.getsourcefile(PythonObject);
        if FileName = 'None' then begin
          FileName := PythonObject.im_func.func_code.co_filename;
          if ExtractFileExt(FileName) <> '' then
            Exit;
          // otherwise it should be an unsaved file
          LineNo := PythonObject.im_func.func_code.co_firstlineno-1;
        end else
          LineNo := InspectModule.findsource(PythonObject).GetItem(1);
        PyIDEMainForm.ShowFilePosition(FileName, Succ(LineNo), 1);
      end;
    end;
  end;
end;

end.

