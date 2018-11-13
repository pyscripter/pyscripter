{-----------------------------------------------------------------------------
 Unit Name: cPyDebugger
 Author:    Kiriakos Vlahos
 Date:      23-Feb-2005
 Purpose:   Interpreter and Debugger based on P4D PythonEngine
 History:   Origianlly Based on SynEdit IDE Demo debugger
-----------------------------------------------------------------------------}

unit cPyDebugger;

interface

uses
  WinApi.Windows,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Contnrs,
  System.Generics.Collections,
  Vcl.Forms,
  PythonEngine,
  uEditAppIntfs,
  cPySupportTypes,
  cPyBaseDebugger;

type
  TFrameInfo = class(TBaseFrameInfo)
  private
    fPyFrame : Variant;
  protected
  // Implementation of the Base class for the internal debugger
    function GetFunctionName : string; override;
    function GetFileName : string; override;
    function GetLine : integer; override;
  public
    constructor Create(Frame : Variant);
    property PyFrame : Variant read fPyFrame;
  end;

Const
  oi_Initialized = 1;
  oi_Has__dict__ = 2;
  oi_IsModule    = 4;
  oi_IsMethod    = 8;
  oi_IsFunction  = 16;
  oi_IsClass     = 32;
  oi_IsDict      = 64;

type
  TNameSpaceItem = class(TBaseNameSpaceItem)
  // Implementation of the Base class for the internal debugger
  protected
    fChildCount : integer;
    fChildNodes : TStringList;
    fName : string;
    fObjectType : string; // for caching ObjectType
    fObjectInfo : Integer;
  protected
    function GetName : string; override;
    function GetObjectType : string; override;
    function GetValue : string; override;
    function GetDocString : string; override;
    function GetChildCount : integer; override;
    function GetChildNode(Index: integer): TBaseNameSpaceItem; override;
    function GetObjectInfo : integer;
    procedure FillObjectInfo; virtual;
  public
    constructor Create(aName : string; aPyObject : Variant);
    destructor Destroy; override;
    function IsClass : Boolean; override;
    function IsDict : Boolean; override;
    function IsModule : Boolean; override;
    function IsFunction : Boolean; override;
    function IsMethod : Boolean; override;
    function Has__dict__ : Boolean; override;
    function IndexOfChild(AName : string): integer; override;
    procedure GetChildNodes; override;
    property ObjectInfo : integer read GetObjectInfo;
  end;

  TPyInternalInterpreter = class(TPyBaseInterpreter)
  private
    fII : Variant;  // Python VarPyth wrapper to the interactive interpreter
    fDebugger : Variant;
    fOldargv : Variant;
  protected
    procedure CreateMainModule; override;
  public
    constructor Create(II : Variant);

    procedure Initialize; override;
    function SysPathAdd(const Path : string) : boolean; override;
    function SysPathRemove(const Path : string) : boolean; override;
    procedure SysPathToStrings(Strings : TStrings); override;
    procedure StringsToSysPath(Strings : TStrings); override;
    function Compile(ARunConfig : TRunConfiguration) : Variant;
    function GetGlobals : TBaseNameSpaceItem; override;
    procedure GetModulesOnPath(Path : Variant; SL : TStrings); override;
    function NameSpaceFromExpression(const Expr : string) : TBaseNameSpaceItem; override;
    function CallTipFromExpression(const Expr : string;
      var DisplayString, DocString : string) : Boolean; override;
    procedure SetCommandLine(ARunConfig : TRunConfiguration); override;
    procedure RestoreCommandLine; override;
    function ImportModule(Editor : IEditor; AddToNameSpace : Boolean = False) : Variant; override;
    procedure Run(ARunConfig : TRunConfiguration); override;
    function SyntaxCheck(Editor : IEditor; Quiet : Boolean = False) : Boolean;
    function RunSource(Const Source, FileName : Variant; symbol : string = 'single') : boolean; override;
    function EvalCode(const Expr : string) : Variant; override;
    function GetObjectType(Ob : Variant) : string; override;
    function UnitTestResult : Variant; override;
    function NameSpaceItemFromPyObject(aName : string; aPyObject : Variant): TBaseNameSpaceItem; override;
    property Debugger : Variant read fDebugger;
    property PyInteractiveInterpreter : Variant read fII;
  end;

  TPyInternalDebugger = class(TPyBaseDebugger)
  // pdb based internal debugger
  private
    fDebuggerCommand : TDebuggerCommand;
    fLineCache : Variant;
    fMainThread : TThreadInfo;
    fOldPS1, fOldPS2 : string;
    InternalInterpreter : TPyInternalInterpreter;
  protected
    procedure SetCommandLine(ARunConfig : TRunConfiguration); override;
    procedure RestoreCommandLine; override;
    procedure SetDebuggerBreakpoints; override;
    procedure LoadLineCache;
    procedure UserCall(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
    procedure UserLine(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
    procedure UserThread(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
    procedure UserException(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
    procedure UserYield(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
    // Fills in CallStackList with TBaseFrameInfo objects
    procedure GetCallStack(CallStackList : TObjectList<TBaseFrameInfo>;
      Frame, Botframe : Variant);
  public
    constructor Create;
    destructor Destroy; override;

    // Debugging
    procedure Debug(ARunConfig : TRunConfiguration; InitStepIn : Boolean = False;
          RunToCursorLine : integer = -1); override;
    procedure RunToCursor(Editor : IEditor; ALine: integer); override;
    procedure StepInto; override;
    procedure StepOver; override;
    procedure StepOut; override;
    procedure Resume; override;
    procedure Pause; override;
    procedure Abort; override;
    // Evaluate expression in the current frame
    procedure Evaluate(const Expr : string; out ObjType, Value : string); overload; override;
    function Evaluate(const Expr : string) : TBaseNamespaceItem; overload; override;
    // Like the InteractiveInterpreter runsource but for the debugger frame
    function RunSource(Const Source, FileName : Variant; symbol : string = 'single') : boolean; override;
    // functions to get TBaseNamespaceItems corresponding to a frame's gloabals and locals
    function GetFrameGlobals(Frame : TBaseFrameInfo) : TBaseNameSpaceItem; override;
    function GetFrameLocals(Frame : TBaseFrameInfo) : TBaseNameSpaceItem; override;
    function NameSpaceFromExpression(const Expr : string) : TBaseNameSpaceItem; override;
    procedure MakeThreadActive(Thread : TThreadInfo); override;
    procedure MakeFrameActive(Frame : TBaseFrameInfo); override;
    // post mortem stuff
    function HaveTraceback : boolean; override;
    procedure EnterPostMortem; override;
    procedure ExitPostMortem; override;
  end;

implementation

uses
  WinApi.MMSystem,
  System.Math,
  System.Variants,
  Vcl.Dialogs,
  JvDSADialogs,
  JvGnugettext,
  JclSysInfo,
  VarPyth,
  StringResources,
  dmCommands,
  frmPythonII,
  frmMessages,
  frmPyIDEMain,
  uCommonFunctions,
  cRefactoring,
  cPyScripterSettings,
  cParameters,
  cPyControl,
  cInternalPython;

{ TFrameInfo }

constructor TFrameInfo.Create(Frame: Variant);
begin
  fPyFrame := Frame;
  inherited Create;
end;

function TFrameInfo.GetFileName: string;
begin
  Result := fPyFrame.f_code.co_filename;
end;

function TFrameInfo.GetFunctionName: string;
begin
  Result := fPyFrame.f_code.co_name;
end;

function TFrameInfo.GetLine: integer;
begin
  Result := fPyFrame.f_lineno;
end;

{ TNameSpaceItem }

constructor TNameSpaceItem.Create(aName : string; aPyObject: Variant);
begin
  Assert(VarIsPython(aPyObject));
  fName := aName;
  fPyObject := aPyObject;
  fChildCount := -1;  // unknown
  fObjectInfo := 0;
  fExpandSequences := True;
end;

destructor TNameSpaceItem.Destroy;
Var
  i : integer;
begin
  if Assigned(fChildNodes) then begin;
    for i := 0 to fChildNodes.Count - 1 do
      fChildNodes.Objects[i].Free;
    fChildNodes.Free;
  end;
  inherited;
end;

function TNameSpaceItem.GetChildCount: integer;
var
  SuppressOutput : IInterface;
begin
  if Assigned(fChildNodes) then
    Result := fChildNodes.Count
  else if fChildCount >= 0 then
    Result := fChildCount
  else begin
    SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
    try
      Result := TPyInternalInterpreter(PyControl.InternalInterpreter).PyInteractiveInterpreter.membercount(fPyObject, ExpandSequences,
        ExpandCommonTypes, ExpandSequences);
    except
      Result := 0;
    end;
    fChildCount := Result;
  end;
end;

function TNameSpaceItem.GetChildNode(Index: integer): TBaseNameSpaceItem;
begin
  Assert(Index >= 0, 'TNameSpaceItem.GetChildNode');
  if not Assigned(fChildNodes) then
    GetChildNodes;
  Assert(Index < fChildNodes.Count, 'TNameSpaceItem.GetChildNode');
  Result := fChildNodes.Objects[Index] as TBaseNameSpaceItem;
end;

procedure TNameSpaceItem.GetChildNodes;
Var
  FullInfoTuple, APyObject: Variant;
  PyFullInfoTuple, PyMemberInfo, PyFullInfo: PPyObject;
  i : integer;
  ObjName : string;
  NameSpaceItem : TNameSpaceItem;
  SuppressOutput : IInterface;
begin
  if not (Assigned(fChildNodes) or GotChildNodes) then begin
    GotChildNodes := True;
    SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
    try
      FullInfoTuple := TPyInternalInterpreter(PyControl.InternalInterpreter).PyInteractiveInterpreter.safegetmembersfullinfo(fPyObject, ExpandSequences,
        ExpandCommonTypes, ExpandSequences);
      fChildCount := len(FullInfoTuple);

      if fChildCount > 0 then begin
        fChildNodes := TStringList.Create;
        fChildNodes.CaseSensitive := True;

        PyFullInfoTuple := ExtractPythonObjectFrom(FullInfoTuple);
        for i := 0 to fChildCount - 1 do with GetPythonEngine do begin
          PyMemberInfo := PyTuple_GetItem(PyFullInfoTuple, i);
          ObjName :=  PyString_AsWideString(PyTuple_GetItem(PyMemberInfo, 0));
          PyFullInfo := PyTuple_GetItem(PyMemberInfo, 1);
          APyObject := VarPythonCreate(PyTuple_GetItem(PyFullInfo, 0));

          NameSpaceItem := TNameSpaceItem.Create(ObjName, APyObject);
          NameSpaceItem.ExpandCommonTypes := ExpandCommonTypes;
          NameSpaceItem.ExpandSequences := ExpandSequences;

          //NameSpaceItem.BufferedValue := PyString_AsWideString(PyTuple_GetItem(PyFullInfo, 1));
          //NameSpaceItem.GotBufferedValue := True;
          NameSpaceItem.fObjectType := PyString_AsWideString(PyTuple_GetItem(PyFullInfo, 1));
          NameSpaceItem.fObjectInfo := PyInt_AsLong(PyTuple_GetItem(PyFullInfo, 2));
          //NameSpaceItem.fChildCount := PyInt_AsLong(PyTuple_GetItem(PyFullInfo, 3));

          fChildNodes.AddObject(ObjName, NameSpaceItem);
        end;
        GetPythonEngine.CheckError;
        if (ObjectType <> 'list') and (ObjectType <> 'tuple') then
          fChildNodes.CustomSort(ComparePythonIdents);
      end;
    except
      fChildCount := 0;
      Vcl.Dialogs.MessageDlg(Format(_(SErrorGettingNamespace), [fName]), mtError, [mbAbort], 0);
      System.SysUtils.Abort;
    end;
  end;
end;

function TNameSpaceItem.GetDocString: string;
Var
  SuppressOutput : IInterface;
begin
  SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
  try
    Result := Import('inspect').getdoc(fPyObject);
  except
    Result := '';
  end;
end;

function TNameSpaceItem.GetName: string;
begin
  Result := fName;
end;

procedure TNameSpaceItem.FillObjectInfo;
var
  SuppressOutput : IInterface;
begin
  SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
  try
    fObjectInfo := TPyInternalInterpreter(PyControl.InternalInterpreter).PyInteractiveInterpreter.objectinfo(fPyObject);
  except
    fObjectInfo := 0;
  end;
end;

function TNameSpaceItem.GetObjectType: string;
begin
  if fObjectType <> '' then
    Result := fObjectType
  else begin
    Result := PyControl.InternalInterpreter.GetObjectType(fPyObject);
    fObjectType := Result;
  end;
end;

function TNameSpaceItem.GetValue: string;
Var
  SuppressOutput : IInterface;
begin
  SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
  try
    Result :=  TPyInternalInterpreter(PyControl.InternalInterpreter).PyInteractiveInterpreter.saferepr(fPyObject);
  except
    Result := '';
  end;
end;

function TNameSpaceItem.Has__dict__: Boolean;
begin
  Result := (ObjectInfo and oi_Has__dict__) = oi_Has__dict__;
end;

function TNameSpaceItem.IndexOfChild(AName: string): integer;
var
  L, H, I, C: Integer;
  Found : Boolean;
begin
  if not Assigned(fChildNodes) then
    GetChildNodes;
  if Assigned(fChildNodes) then begin
    // Child nodes are sorted with ComparePythonIdents
    // Do a quick search
    Result := -1;
    Found := False;
    L := 0;
    H := fChildNodes.Count - 1;
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := ComparePythonIdents(fChildNodes[I], AName);
      if C < 0 then L := I + 1 else
      begin
        H := I - 1;
        if C = 0 then
        begin
          Found := True;
          L := I;
        end;
      end;
    end;
    if Found then
      Result := L;
    //Result := fChildNodes.IndexOf(AName)
  end else
    Result := -1;
end;

function TNameSpaceItem.IsClass: Boolean;
begin
  Result := (ObjectInfo and oi_IsClass) = oi_IsClass;
end;

function TNameSpaceItem.IsDict: Boolean;
begin
  Result := (ObjectInfo and oi_IsDict) = oi_IsDict;
end;

function TNameSpaceItem.IsFunction: Boolean;
begin
  Result := (ObjectInfo and oi_IsFunction) = oi_IsFunction;
end;

function TNameSpaceItem.IsMethod: Boolean;
begin
  Result := (ObjectInfo and oi_IsMethod) = oi_IsMethod;
end;

function TNameSpaceItem.IsModule: Boolean;
begin
  Result := (ObjectInfo and oi_IsModule) = oi_IsModule;
end;

function TNameSpaceItem.GetObjectInfo: integer;
begin
  if (fObjectInfo and oi_Initialized) = 0 then
    FillObjectInfo;
  Result := fObjectInfo;
end;

{ TPyDebugger }

constructor TPyInternalDebugger.Create;
begin
  inherited Create;
  InternalInterpreter := PyControl.InternalInterpreter as TPyInternalInterpreter;

  fLineCache := Import('linecache');

  fMainThread := TThreadInfo.Create;
  fMainThread.Name := 'MainThread';
  fMainThread.Status := thrdRunning;

  fDebuggerCommand := dcNone;
  PyControl.BreakPointsChanged := True;
end;

destructor TPyInternalDebugger.Destroy;
begin
  with PyControl.InternalPython.DebugIDE.Events do begin
    Items[dbie_user_call].OnExecute := nil;
    Items[dbie_user_line].OnExecute := nil;
    Items[dbie_user_thread].OnExecute := nil;
    Items[dbie_user_exception].OnExecute := nil;
    Items[dbie_user_yield].OnExecute := nil;
  end;

  FreeAndNil(fMainThread);
  inherited;
end;

procedure TPyInternalDebugger.EnterPostMortem;
Var
  Frame, BotFrame, TraceBack : Variant;
begin
  if not (HaveTraceback and (PyControl.DebuggerState = dsInactive)) then
    Exit;
  with PythonIIForm do begin
    fOldPS1 := PS1;
    PS1 := PMPrefix + PS1;
    fOldPS2 := PS2;
    PS2 := PMPrefix + PS2;
    AppendPrompt;
  end;

  TraceBack := SysModule.last_traceback;
  Botframe := TraceBack.tb_frame;
  while not VarIsNone(TraceBack.tb_next) do
    TraceBack := TraceBack.tb_next;
  Frame := TraceBack.tb_frame;

  PyControl.DoStateChange(dsPostMortem);
  fMainThread.Status := thrdBroken;
  GetCallStack(fMainThread.CallStack, Frame, Botframe);
  TPyBaseDebugger.ThreadChangeNotify(fMainThread, tctAdded );

  DSAMessageDlg(dsaPostMortemInfo, 'PyScripter', _(SPostMortemInfo),
     mtInformation, [mbOK], 0, dckActiveForm, 0, mbOK);
end;

function TPyInternalDebugger.Evaluate(const Expr: string): TBaseNamespaceItem;
Var
  SuppressOutput : IInterface;
  V : Variant;
begin
  Result := nil;
  if PyControl.DebuggerState in [dsPaused, dsPostMortem] then begin
    SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
    try
      // evalcode knows we are in the debugger and uses current frame locals/globals
      V := InternalInterpreter.PyInteractiveInterpreter.evalcode(Expr);
      Result := TNameSpaceItem.Create(Expr, V);
    except
      // fail quitely
    end;
  end;
end;

procedure TPyInternalDebugger.Evaluate(const Expr : string; out ObjType, Value : string);
Var
  SuppressOutput : IInterface;
  V : Variant;
begin
  ObjType := _(SNotAvailable);
  Value := _(SNotAvailable);
  if PyControl.DebuggerState in [dsPaused, dsPostMortem] then begin
    SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
    try
      // evalcode knows we are in the debugger and uses current frame locals/globals
      V := InternalInterpreter.PyInteractiveInterpreter.evalcode(Expr);
      ObjType := InternalInterpreter.PyInteractiveInterpreter.objecttype(V);
      Value := InternalInterpreter.PyInteractiveInterpreter.saferepr(V);
    except
      // fail quitely
    end;
  end;
end;

procedure TPyInternalDebugger.ExitPostMortem;
begin
  with PythonIIForm do begin
    PS1 := fOldPS1;
    PS2 := fOldPS2;
    AppendText(sLineBreak+PS1);
  end;
  fMainThread.Status := thrdRunning;
  fMainThread.CallStack.Clear;
  TPyBaseDebugger.ThreadChangeNotify(fMainThread, tctStatusChange);
  MakeFrameActive(nil);
  PyControl.DoStateChange(dsInactive);
end;

procedure TPyInternalDebugger.GetCallStack(CallStackList: TObjectList<TBaseFrameInfo>;
  Frame, Botframe : Variant);
begin
  CallStackList.Clear;
  if VarIsPython(Frame) then begin
    while not VarIsNone(Frame.f_back) and not VarIsNone(Frame.f_back.f_back) do begin
      CallStackList.Add(TFrameInfo.Create(Frame));
      Frame := Frame.f_back;
      if VarIsSame(Frame, Botframe) then break;
    end;
    if CallStackList.Count = 0  then
      CallStackList.Add(TFrameInfo.Create(Frame));
  end;
end;

function TPyInternalDebugger.GetFrameGlobals(Frame: TBaseFrameInfo): TBaseNameSpaceItem;
begin
  Result := nil;
  if not (PyControl.DebuggerState in [dsPaused, dsPostMortem]) then
    Exit;
  Result := TNameSpaceItem.Create('globals', (Frame as TFrameInfo).fPyFrame.f_globals);
end;

function TPyInternalDebugger.GetFrameLocals(Frame: TBaseFrameInfo): TBaseNameSpaceItem;
begin
  Result := nil;
  if not (PyControl.DebuggerState in [dsPaused, dsPostMortem]) then
    Exit;
  Result := TNameSpaceItem.Create('locals', (Frame as TFrameInfo).fPyFrame.f_locals);
end;

function TPyInternalDebugger.HaveTraceback: boolean;
begin
  try
    Result := VarModuleHasObject(SysModule, 'last_traceback');
  except
    Result := False;
  end;
end;

procedure TPyInternalDebugger.Pause;
begin
  fDebuggerCommand := dcPause;
end;

// Timer callback function
procedure TPyInternalDebugger.RestoreCommandLine;
begin
  InternalInterpreter.RestoreCommandLine;
end;

procedure TPyInternalDebugger.Resume;
begin
  fDebuggerCommand := dcRun;
end;

procedure TPyInternalDebugger.Debug(ARunConfig: TRunConfiguration;
  InitStepIn: Boolean = False; RunToCursorLine : integer = -1);
var
  Code : Variant;
  Path, OldPath : string;
  PythonPathAdder : IInterface;
  ReturnFocusToEditor: Boolean;
  CanDoPostMortem : Boolean;
  Editor : IEditor;
begin
  CanDoPostMortem := False;

  // Repeat here to make sure it is set right
  MaskFPUExceptions(PyIDEOptions.MaskFPUExceptions);

  fDebuggerCommand := dcRun;
  Assert(PyControl.DebuggerState = dsInactive);

  //Compile
  Code := InternalInterpreter.Compile(ARunConfig);

  if VarIsPython(Code) then begin

    // Add the path of the script to the Python Path - Will be automatically removed
    Path := InternalInterpreter.ToPythonFileName(ARunConfig.ScriptName);
    if (Path.Length > 0) and (Path[1] <> '<') then
      Path := ExtractFileDir(Path)
    else
      Path := '';
    InternalInterpreter.SysPathRemove('');
    if Path.Length > 1 then
      PythonPathAdder := InternalInterpreter.AddPathToPythonPath(Path);

    // Set the Working directory
    if ARunConfig.WorkingDir <> '' then
      Path := Parameters.ReplaceInText(ARunConfig.WorkingDir);
    if Path.Length <= 1 then
      Path := InternalInterpreter.SystemTempFolder;
    OldPath := GetCurrentDir;

    // Change the current path
    try
      SetCurrentDir(Path)
    except
      Vcl.Dialogs.MessageDlg(_(SCouldNotSetDirectory), mtWarning, [mbOK], 0);
    end;

    PyControl.DoStateChange(dsDebugging);
    TPyBaseDebugger.ThreadChangeNotify(fMainThread, tctAdded );

    MessagesWindow.ClearMessages;
    Editor := GI_ActiveEditor;
    ReturnFocusToEditor := Assigned(Editor);
    // Set the layout to the Debug layout is it exists
    if PyIDEMainForm.Layouts.IndexOf('Debug') >= 0 then begin
      PyIDEMainForm.SaveLayout('Current');
      PyIDEMainForm.LoadLayout('Debug');
      Application.ProcessMessages;
    end else
      PyIDEMainForm.actNavInterpreterExecute(nil);

    try
      with PythonIIForm do begin
        fOldPS1 := PS1;
        PS1 := DebugPrefix + PS1;
        fOldPS2 := PS2;
        PS2 := DebugPrefix + PS2;
        AppendPrompt;
      end;
      //attach debugger callback routines
      with PyControl.InternalPython.DebugIDE.Events do begin
        Items[dbie_user_call].OnExecute := UserCall;
        Items[dbie_user_line].OnExecute := UserLine;
        Items[dbie_user_thread].OnExecute := UserThread;
        Items[dbie_user_exception].OnExecute := UserException;
        Items[dbie_user_yield].OnExecute := UserYield;
      end;

      //set breakpoints
      SetDebuggerBreakPoints;
      if RunToCursorLine >= 0 then  // add temp breakpoint
        InternalInterpreter.Debugger.set_break(Code.co_filename, RunToCursorLine, 1);

      // New Line for output
      PythonIIForm.AppendText(sLineBreak);

      // Set the command line parameters
      SetCommandLine(ARunConfig);

      InternalInterpreter.Debugger.InitStepIn := InitStepIn;
      try
        InternalInterpreter.Debugger.run(Code);
      except
        // CheckError already called by VarPyth
        on E: EPythonError do begin
          InternalInterpreter.HandlePyException(E, 2);
          ReturnFocusToEditor := False;
          CanDoPostMortem := True;
          System.SysUtils.Abort;
        end;
      end;

    finally
      MakeFrameActive(nil);
      with PythonIIForm do begin
        PS1 := fOldPS1;
        PS2 := fOldPS2;
        AppendPrompt;
      end;

      // Restore the command line parameters
      RestoreCommandLine;

      //  Add again the empty path
      InternalInterpreter.SysPathAdd('');

      // Change the back current path
      SetCurrentDir(OldPath);

      if PyIDEMainForm.Layouts.IndexOf('Debug') >= 0 then
        PyIDEMainForm.LoadLayout('Current');

      PyControl.DoStateChange(dsInactive);
      if ReturnFocusToEditor then
        Editor.Activate;
      if CanDoPostMortem and PyIDEOptions.PostMortemOnException then
        EnterPostMortem;
    end;
  end;
end;

function TPyInternalDebugger.RunSource(Const Source, FileName : Variant; symbol : string = 'single') : boolean;
// The internal interpreter RunSource calls II.runsource which differs
// according to whether we debugging or not
Var
  OldCurrentPos : TEditorPos;
begin
  OldCurrentPos := PyControl.CurrentPos;
  try
    Result := InternalInterpreter.RunSource(Source, FileName, symbol);
  finally
    PyControl.CurrentPos := OldCurrentPos;
  end;
end;

procedure TPyInternalDebugger.RunToCursor(Editor : IEditor; ALine: integer);
Var
  FName : string;
begin
  Assert(PyControl.DebuggerState = dsPaused);
  // Set Temporary breakpoint
  SetDebuggerBreakPoints;  // So that this one is not cleared
  FName := Editor.FileName;
  if FName = '' then
    FName := '<'+Editor.FileTitle+'>';
  InternalInterpreter.Debugger.set_break(VarPythonCreate(FName), ALine, 1);

  fDebuggerCommand := dcRunToCursor;
end;

procedure TPyInternalDebugger.StepInto;
begin
  fDebuggerCommand := dcStepInto;
end;

procedure TPyInternalDebugger.StepOver;
begin
  fDebuggerCommand := dcStepOver;
end;

procedure TPyInternalDebugger.StepOut;
begin
  fDebuggerCommand := dcStepOut;
end;

procedure TPyInternalDebugger.Abort;
begin
  if PyControl.DebuggerState = dsPostMortem then
    ExitPostMortem
  else
    fDebuggerCommand := dcAbort;
end;

procedure TPyInternalDebugger.UserCall(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
begin
   // PythonIIForm.AppendText('UserCall'+sLineBreak);
   Result := GetPythonEngine.ReturnNone;
end;

procedure TPyInternalDebugger.UserException(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
   // PythonIIForm.AppendText('UserException'+sLineBreak);
   Result := GetPythonEngine.ReturnNone;
end;

procedure TPyInternalDebugger.UserLine(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
Var
  Frame : Variant;
  FName : string;
begin
   Result := GetPythonEngine.ReturnNone;

   Frame := VarPythonCreate(Args).__getitem__(0);
   FName := Frame.f_code.co_filename;
   if (FName[1] ='<') and (FName[Length(FName)] = '>') then
     FName :=  Copy(FName, 2, Length(FName)-2);

   if PyIDEMainForm.ShowFilePosition(FName, Frame.f_lineno, 1, 0, True, False) and
     (Frame.f_lineno > 0) then
   begin
     if PyControl.DebuggerState = dsDebugging then
       PyControl.DoStateChange(dsPaused);
     fMainThread.Status := thrdBroken;
     GetCallStack(fMainThread.CallStack, Frame, InternalInterpreter.Debugger.botframe);
     TPyBaseDebugger.ThreadChangeNotify(fMainThread, tctStatusChange);

     FDebuggerCommand := dcNone;
     While  FDebuggerCommand = dcNone do
       PyControl.DoYield(True);

     if PyControl.BreakPointsChanged then SetDebuggerBreakpoints;

     fMainThread.Status := thrdRunning;
     TPyBaseDebugger.ThreadChangeNotify(fMainThread, tctStatusChange);
     fMainThread.CallStack.Clear;
     PyControl.DoStateChange(dsDebugging);
   end;

   case fDebuggerCommand of
     dcRun         : InternalInterpreter.Debugger.set_continue();
     dcStepInto    : InternalInterpreter.Debugger.set_step();
     dcStepOver    : InternalInterpreter.Debugger.set_next(Frame);
     dcStepOut     : InternalInterpreter.Debugger.set_return(Frame);
     dcRunToCursor : InternalInterpreter.Debugger.set_continue;
     dcPause       : InternalInterpreter.Debugger.set_step();
     dcAbort       : begin
                       InternalInterpreter.Debugger.set_quit();
                       MessagesWindow.AddMessage(_(SDebuggingAborted));
                       MessagesWindow.ShowWindow;
                     end;
   end;
end;

procedure TPyInternalDebugger.UserThread(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
begin
   Result := GetPythonEngine.ReturnNone;
end;

procedure TPyInternalDebugger.UserYield(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
begin
  PyControl.DoYield(False);
  if fDebuggerCommand = dcAbort then begin
    InternalInterpreter.Debugger.set_quit();
    MessagesWindow.AddMessage(_(SDebuggingAborted));
    MessagesWindow.ShowWindow;
  end else if fDebuggerCommand = dcPause then
    InternalInterpreter.Debugger.set_step();
  Result := GetPythonEngine.ReturnNone;
end;

procedure TPyInternalDebugger.SetCommandLine(ARunConfig : TRunConfiguration);
begin
  InternalInterpreter.SetCommandLine(ARunConfig);
end;

procedure TPyInternalDebugger.SetDebuggerBreakpoints;
var
  i, j : integer;
  FName : string;
begin
  if not PyControl.BreakPointsChanged then Exit;
  LoadLineCache;
  InternalInterpreter.Debugger.clear_all_breaks();
  for i := 0 to GI_EditorFactory.Count - 1 do
    with GI_EditorFactory.Editor[i] do begin
      if FileName <> '' then
        FName := FileName
      else
        FName := '<'+FileTitle+'>';
      for j := 0 to BreakPoints.Count - 1 do begin
        if not TBreakPoint(BreakPoints[j]).Disabled then begin
          if TBreakPoint(BreakPoints[j]).Condition <> '' then begin
            InternalInterpreter.Debugger.set_break(VarPythonCreate(FName),
              TBreakPoint(BreakPoints[j]).LineNo,
              0, VarPythonCreate(TBreakPoint(BreakPoints[j]).Condition));
          end else begin
            InternalInterpreter.Debugger.set_break(VarPythonCreate(FName),
              TBreakPoint(BreakPoints[j]).LineNo);
          end;
        end;
      end;
    end;
  PyControl.BreakPointsChanged := False;
end;

procedure TPyInternalDebugger.LoadLineCache;
Var
  i : integer;
  FName, Source, LineList : Variant;
begin
  // inject unsaved code into LineCache
  fLineCache.cache.clear();
  for i := 0 to GI_EditorFactory.Count - 1 do
    with GI_EditorFactory.Editor[i] do
      if HasPythonfile and (FileName = '') and (RemoteFileName = '') then
      begin
        if InternalInterpreter.IsPython3000 then
          Source := CleanEOLs(SynEdit.Text)+WideLF
        else
          Source := CleanEOLs(EncodedText)+#10;
        LineList := VarPythonCreate(Source);
        LineList := LineList.splitlines(True);
        FName := '<'+FileTitle+'>';
        fLineCache.cache.SetItem(VarPythonCreate(FName),
          VarPythonCreate([Length(Source), None, LineList, FName], stTuple));
      end;
end;

procedure TPyInternalDebugger.MakeFrameActive(Frame: TBaseFrameInfo);
begin
  if Assigned(Frame) then
    InternalInterpreter.Debugger.currentframe := (Frame as TFrameInfo).fPyFrame
  else
    InternalInterpreter.Debugger.currentframe := None;
end;

procedure TPyInternalDebugger.MakeThreadActive(Thread: TThreadInfo);
begin
  // Do nothing - Thread debugging is not supported
end;

function TPyInternalDebugger.NameSpaceFromExpression(
  const Expr: string): TBaseNameSpaceItem;
//  The internal interpreter knows whether we are debugging and
//  adjusts accordingly (in II.debugger.evalcode)
begin
  Result := InternalInterpreter.NameSpaceFromExpression(Expr);
end;

{ TPyInternalInterpreter }

constructor TPyInternalInterpreter.Create(II: Variant);
begin
  fEngineType := peInternal;
  fII := II;
  fDebugger := II.debugger;

  // sys.displayhook
  fII.setupdisplayhook()
end;

procedure TPyInternalInterpreter.CreateMainModule;
begin
  fMainModule := TModuleProxy.CreateFromModule(VarPyth.MainModule, self);
end;

function TPyInternalInterpreter.EvalCode(const Expr: string): Variant;
begin
  // may raise exceptions
  Result := fII.evalcode(Expr);
end;

function TPyInternalInterpreter.CallTipFromExpression(const Expr: string;
  var DisplayString, DocString: string) : Boolean;
var
  LookupObj, ArgText, PyDocString: Variant;
  SuppressOutput : IInterface;
begin
  Result := False;
  DisplayString := '';
  DocString := '';
  if Expr = '' then Exit;

  SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
  try
    //Evaluate the lookup expression and get the hint text
    LookupObj := fII.evalcode(Expr);
    ArgText := fII.get_arg_text(LookupObj);
    DisplayString := ArgText.__getitem__(0);
    PyDocString := ArgText.__getitem__(1);
    Result := True;

    if not VarIsNone(PyDocString) then begin
      //DocString := GetNthLine(PyDocString, 1)
      DocString := PyDocString;
      DocString := GetLineRange(DocString, 1, 20);  // first 20 lines
    end else
      DocString := '';
  except
  end;
end;

function TPyInternalInterpreter.Compile(ARunConfig: TRunConfiguration): Variant;
Var
  co : PPyObject;
  FName, Source : AnsiString;
  Editor : IEditor;
begin
  if PyControl.DebuggerState <> dsInactive then begin
    Vcl.Dialogs.MessageDlg(_(SCannotCompileWhileRunning),
      mtError, [mbAbort], 0);
    System.SysUtils.Abort;
  end;

  VarClear(Result);
  PyControl.ErrorPos.Clear;
  PyControl.DoErrorPosChanged;

  MessagesWindow.ClearMessages;

  Editor := GI_EditorFactory.GetEditorByNameOrTitle(ARunConfig.ScriptName);
  if Assigned(Editor) then begin
    FName :=  GetPythonEngine.EncodeWindowsFilePath(Editor.FileName);
    if FName = '' then FName := GetPythonEngine.EncodeWindowsFilePath('<'+Editor.FileTitle+'>');
    Source := CleanEOLs(Editor.EncodedText) + AnsiString(#10);
  end else begin
    try
      FName := GetPythonEngine.EncodeWindowsFilePath(ToPythonFileName(ARunConfig.ScriptName));
      Source := CleanEOLs(FileToEncodedStr(ARunConfig.ScriptName)) + AnsiString(#10);
    except
      on E: Exception do begin
        Vcl.Dialogs.MessageDlg(Format(_(SFileOpenError), [ARunConfig.ScriptName, E.Message]), mtError, [mbOK], 0);
        System.SysUtils.Abort;
      end;
    end;
  end;

  with GetPythonEngine do begin
    co := Py_CompileString(PAnsiChar(Source), PAnsiChar(FName), file_input );
    if not Assigned( co ) then begin
      // New Line for output
      PythonIIForm.AppendText(sLineBreak);
      try
        // Print and throw exception for the error
        CheckError;
      except
        on E: EPySyntaxError do begin
          if PyIDEMainForm.ShowFilePosition(E.EFileName, E.ELineNumber, E.EOffset) and
            Assigned(GI_ActiveEditor)
          then begin
            PyControl.ErrorPos.NewPos(GI_ActiveEditor, E.ELineNumber, E.EOffset, True);
            PyControl.DoErrorPosChanged;
          end;

          PythonIIForm.AppendPrompt;
          Vcl.Dialogs.MessageDlg(E.Message, mtError, [mbOK], 0);
          System.SysUtils.Abort;
        end;
        on E: EPythonError do begin  //may raise OverflowError or ValueError
          HandlePyException(E);
          System.SysUtils.Abort;
        end;
      end;
    end else begin
      Result := VarPythonCreate(co);
      // Cleanup ByteCode
      GetPythonEngine.Py_XDECREF(co);
    end;
  end;
end;

function TPyInternalInterpreter.GetGlobals: TBaseNameSpaceItem;
begin
  Result := TNameSpaceItem.Create('globals', VarPythonEval('globals()'));
end;

procedure TPyInternalInterpreter.GetModulesOnPath(Path: Variant; SL: TStrings);
Var
  i : integer;
  ModuleList : Variant;
begin
  ModuleList := fII.getmodules(Path);
  for I := 0 to Len(ModuleList) - 1 do
    SL.Add(ModuleList.__getitem__(i));
end;

function TPyInternalInterpreter.GetObjectType(Ob: Variant): string;
Var
  SuppressOutput : IInterface;
begin
  SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
  try
    Result := fII.objecttype(Ob);
  except
    Result := 'Unknown type';
  end;
end;

function TPyInternalInterpreter.ImportModule(Editor: IEditor;
  AddToNameSpace : Boolean = False) : Variant;
{
  Imports Editor text without saving the file.
  Does not add the module name to the locals()
  of the interpreter.
}
Var
  Code : Variant;
  Path, NameOfModule : string;
  //PyObject, Module : PPyObject;
  PythonPathAdder : IInterface;
  RunConfiguration : TRunConfiguration;
begin
  Assert(Assigned(Editor));
  VarClear(Result);
  //Compile
  RunConfiguration := TRunConfiguration.Create;
  try
    RunConfiguration.ScriptName := Editor.GetFileNameOrTitle;
    Code := Compile(RunConfiguration);
  finally
    RunConfiguration.Free;
  end;

  // Add the path of the imported script to the Python Path
  Path := ExtractFileDir(Editor.FileName);
  if Path.Length > 1 then begin
    PythonPathAdder := AddPathToPythonPath(Path, False);
    SysPathRemove('');
  end;

  if Editor.FileName <> '' then
    NameOfModule := FileNameToModuleName(Editor.FileName)
  else
    NameOfModule := ChangeFileExt(Editor.FileTitle, '');

  PyControl.DoStateChange(dsRunning);
  try
    try
      Result := fII._import(NameOfModule, Code);
      GetPythonEngine.CheckError;
    except
      on E: EPythonError do begin
        HandlePyException(E);
        Result := None;
      end;
    end;
    if VarIsNone(Result) then begin
      Vcl.Dialogs.MessageDlg(_(SErrorInImportingModule), mtError, [mbOK], 0);
      System.SysUtils.Abort;
    end else if AddToNameSpace then
      // add Module name to the locals() of the interpreter
      GetPythonEngine.ExecString(AnsiString('import ' + NameOfModule));
  finally
    //  Add again the empty path
    SysPathAdd('');

    PyControl.DoStateChange(dsInactive);
  end;
end;

procedure TPyInternalInterpreter.Initialize;
begin
  // do not run Python_Init if this is not our main engine
  if PyIDEOptions.PythonEngineType <> peInternal then Exit;
  inherited;
end;

function TPyInternalInterpreter.NameSpaceFromExpression(
  const Expr: string): TBaseNameSpaceItem;
var
  InspectModule, LookupObj, ItemsDict: Variant;
  SuppressOutput : IInterface;
begin
  SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
  InspectModule := Import('inspect');
  try
    if Expr <> '' then begin
      //Evaluate the lookup expression and get the hint text
      LookupObj := fII.evalcode(Expr);
      Result := TNameSpaceItem.Create(Expr, LookupObj);
//      ItemsDict := fII.safegetmembers(LookupObj);
//      Result := TNameSpaceItem.Create(Expr, ItemsDict);
    end else begin
      ItemsDict := NewPythonDict;
      ItemsDict.update(BuiltinModule.__dict__);
      ItemsDict.update(fII.evalcode('vars()'));
      Result := TNameSpaceItem.Create(Expr, ItemsDict);
    end;
  except
    Result := nil;
    //Result := TNameSpaceItem.Create(Expr, None);
  end;
end;

function TPyInternalInterpreter.NameSpaceItemFromPyObject(aName: string;
  aPyObject: Variant): TBaseNameSpaceItem;
begin
  Result := TNameSpaceItem.Create(aName, aPyObject);
end;

procedure TPyInternalInterpreter.RestoreCommandLine;
begin
  SysModule.argv := fOldargv;
end;

procedure TPyInternalInterpreter.SetCommandLine(ARunConfig : TRunConfiguration);
var
  SysMod : Variant;
  S, Param : string;
  P : PChar;
begin
  SysMod := SysModule;
  fOldargv := SysMod.argv;
  SysMod.argv := NewPythonList;
  // Workaround due to PREFER_UNICODE flag to make sure
  // no conversion to Unicode and back will take place
  S := ToPythonFileName(ARunConfig.ScriptName);
  if IsPython3000 then        // Issue 425
    SysMod.argv.append(VarPythonCreate(S))
  else
    SysMod.argv.append(VarPythonCreate(AnsiString(S)));

  S := Trim(ARunConfig.Parameters);
  if S <> '' then begin
    S := Parameters.ReplaceInText(S);
    P := PChar(S);
    while P[0] <> #0 do begin
      P := GetParamStr(P, Param);
      if IsPython3000 then        // Issue 425
       SysMod.argv.append(VarPythonCreate(Param))
      else
       SysMod.argv.append(VarPythonCreate(AnsiString(Param)));
    end;
    PythonIIForm.AppendText(Format(_(SCommandLineMsg), [S]));
  end;
end;

procedure TPyInternalInterpreter.StringsToSysPath(Strings: TStrings);
var
  i: Integer;
  PythonPath: Variant;
begin
  PythonPath := NewPythonList;
  for i := 0 to Strings.Count - 1 do
    PythonPath.append(Strings[i]);
  SysModule.path := PythonPath;
end;

procedure TimeCallBack(TimerID, Msg: Uint; dwUser, dw1, dw2: DWORD); stdcall;
begin
  if PLongWord(dwUser)^ = 0 then Exit;
  if (MessageBoxW(0, PWideChar(_(SInterruptRunningScript)),
  'ScriptTimeOut', MB_ICONWARNING or MB_YESNO) = idYes) then begin
    GetPythonEngine.PyErr_SetInterrupt;  //Generate Keyboard interrupt signal
    TimeKillEvent(PLongWord(dwUser)^);
    PLongWord(dwUser)^ := 0;
  end;
end;

procedure TPyInternalInterpreter.Run(ARunConfig: TRunConfiguration);
Var
  Code : Variant;
  mmResult,Resolution : LongWord;
  tc : TTimeCaps;
  Path, OldPath : string;
  PythonPathAdder : IInterface;
  ReturnFocusToEditor : Boolean;
  CanDoPostMortem : Boolean;
  Editor : IEditor;
begin
  CanDoPostMortem := False;

  // Repeat here to make sure it is set right
  MaskFPUExceptions(PyIDEOptions.MaskFPUExceptions);

  //Compile
  Code := Compile(ARunConfig);

  if VarIsPython(Code) then begin
    PyControl.DoStateChange(dsRunning);

    // New Line for output
    PythonIIForm.AppendText(sLineBreak);

    mmResult := 0;
    Resolution := 100;

    // Add the path of the executed file to the Python path - Will be automatically removed
    Path := ToPythonFileName(ARunConfig.ScriptName);
    if (Path.Length > 0) and (Path[1] <> '<') then
      Path := ExtractFileDir(Path)
    else
      Path := '';
    SysPathRemove('');
    if Length(Path) > 1 then
      PythonPathAdder := AddPathToPythonPath(Path);

  // Set the Working directory
    if ARunConfig.WorkingDir <> '' then
      Path := Parameters.ReplaceInText(ARunConfig.WorkingDir);
    if Path.Length <= 1 then
      Path := SystemTempFolder;
    OldPath := GetCurrentDir;

    try
      SetCurrentDir(Path)
    except
      Vcl.Dialogs.MessageDlg(_(SCouldNotSetDirectory), mtWarning, [mbOK], 0);
    end;


    // Set the command line parameters
    SetCommandLine(ARunConfig);

    Editor := GI_ActiveEditor;
    ReturnFocusToEditor := Assigned(Editor);
    PyIDEMainForm.actNavInterpreterExecute(nil);

    try
      // Set Multimedia Timer
      if (PyIDEOptions.TimeOut > 0) and
        (timeGetDevCaps(@tc, SizeOf(tc))=TIMERR_NOERROR) then
      begin
        Resolution := Min(Resolution,tc.wPeriodMax);
        TimeBeginPeriod(Resolution);
        mmResult := TimeSetEvent(PyIDEOptions.TimeOut, resolution,
          @TimeCallBack, DWORD(@mmResult), TIME_PERIODIC or 256);
      end;

      try
        fII.run_nodebug(Code);
      except
        // CheckError already called by VarPyth
        on E: EPythonError do begin
          HandlePyException(E);
          ReturnFocusToEditor := False;
          CanDoPostMortem := True;
          System.SysUtils.Abort;
        end;
      end;
    finally
      if PyIDEOptions.TimeOut > 0 then begin
        if (mmResult <> 0) then TimeKillEvent(mmResult);
        TimeEndPeriod(Resolution);
      end;
      PythonIIForm.AppendPrompt;

      // Restore the command line parameters
      RestoreCommandLine;

      //  Add again the empty path
      SysPathAdd('');

      // Change the back current path
      SetCurrentDir(OldPath);

      PyControl.DoStateChange(dsInactive);
      if ReturnFocusToEditor then
        Editor.Activate;
      if CanDoPostMortem and PyIDEOptions.PostMortemOnException then
        PyControl.ActiveDebugger.EnterPostMortem;
    end;
  end;
end;

function TPyInternalInterpreter.RunSource(Const Source, FileName : Variant; symbol : string = 'single') : boolean;
Var
  OldDebuggerState : TDebuggerState;
  PySource : Variant;
begin
  Assert(not PyControl.Running, 'RunSource called while the Python engine is active');
  OldDebuggerState := PyControl.DebuggerState;
  PyControl.DoStateChange(dsRunning);
  try
    // Workaround due to PREFER_UNICODE flag to make sure
    // no conversion to Unicode and back will take place
    PySource := VarPythonCreate(Source);
    Result := fII.runsource(PySource, FileName, symbol);
  finally
    PyControl.DoStateChange(OldDebuggerState);
  end;
end;

function TPyInternalInterpreter.SyntaxCheck(Editor: IEditor; Quiet : Boolean = False): Boolean;
Var
  FName : string;
  Source : AnsiString;
  tmp: PPyObject;
  PyErrType, PyErrValue, PyErrTraceback, PyErrValueTuple : PPyObject;
  SuppressOutput : IInterface;
begin
  PyControl.ErrorPos.Clear;
  PyControl.DoErrorPosChanged;

  FName := Editor.FileName;
  if FName = '' then FName := '<'+Editor.FileTitle+'>';
  Source := CleanEOLs(Editor.EncodedText)+AnsiString(#10);

  with GetPythonEngine do begin
    if Quiet then
      SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
    Result := CheckExecSyntax(Source);
    if not Result then begin
      if Quiet then begin
        if Assigned(PyErr_Occurred()) then begin
          if (PyErr_ExceptionMatches(PyExc_SyntaxError^) = 1) then begin
            PyErr_Fetch(@PyErrType, @PyErrValue, @PyErrTraceback);  // Clears the Error
            if Assigned(PyErrValue) then begin
            // Sometimes there's a tuple instead of instance...
              if PyTuple_Check( PyErrValue )  and (PyTuple_Size( PyErrValue) >= 2) then
              begin
                PyControl.ErrorPos.ErrorMsg := PyString_AsDelphiString(PyTuple_GetItem( PyErrValue, 0));
                PyErrValueTuple := PyTuple_GetItem( PyErrValue, 1);
                if PyTuple_Check( PyErrValueTuple )  and (PyTuple_Size( PyErrValueTuple) >= 4) then
                begin
                  PyControl.ErrorPos.Line := PyInt_AsLong(PyTuple_GetItem( PyErrValueTuple, 1));
                  PyControl.ErrorPos.Char := PyInt_AsLong(PyTuple_GetItem( PyErrValueTuple, 2));
                end;
              end else
                // Is it an instance of the SyntaxError class ?
              if PyInstance_Check( PyErrValue ) or (PyType_IsSubtype(PyErrValue^.ob_type,
                PPyTypeObject(GetPythonEngine.PyExc_SyntaxError^)) = 1) then
              begin
                // Get the text containing the error, cut of carriage return
                tmp := PyObject_GetAttrString(PyErrValue, 'text');
                if Assigned(tmp) and PyString_Check(tmp) then
                  PyControl.ErrorPos.ErrorMsg := Trim(PyString_AsDelphiString(tmp));
                Py_XDECREF(tmp);
                // Get the offset where the error should appear
                tmp := PyObject_GetAttrString(PyErrValue, 'offset' );
                if Assigned(tmp) and PyInt_Check(tmp) then
                  PyControl.ErrorPos.Char := PyInt_AsLong(tmp);
                Py_XDECREF(tmp);
                // Get the line number of the error
                tmp := PyObject_GetAttrString(PyErrValue, 'lineno' );
                if Assigned(tmp) and PyInt_Check(tmp) then
                  PyControl.ErrorPos.Line := PyInt_AsLong(tmp);
                Py_XDECREF(tmp);
                PyErr_Clear;
              end;
              PyControl.ErrorPos.Editor := Editor;
              PyControl.ErrorPos.IsSyntax := True;
              PyControl.DoErrorPosChanged;
            end;
            Py_XDECREF(PyErrType);
            Py_XDECREF(PyErrValue);
            Py_XDECREF(PyErrTraceback);
          end else
            PyErr_Clear;
        end;
      end else begin
        // Display error
        // New Line for output
        MessagesWindow.ClearMessages;

        PythonIIForm.AppendText(sLineBreak);
        try
          // Print and throw exception for the error
          CheckError;
        except
          on E: EPySyntaxError do begin
            E.EFileName := FName;  // add the filename
            if PyIDEMainForm.ShowFilePosition(E.EFileName, E.ELineNumber, E.EOffset) and
              Assigned(GI_ActiveEditor)
            then begin
              PyControl.ErrorPos.NewPos(GI_ActiveEditor, E.ELineNumber, E.EOffset, True);
              PyControl.DoErrorPosChanged;
            end;
            if Not Quiet then Vcl.Dialogs.MessageDlg(E.Message, mtError, [mbOK], 0);
          end;
        end;
        PythonIIForm.AppendPrompt;
      end;
    end;
  end;
end;

function TPyInternalInterpreter.SysPathAdd(const Path: string): boolean;
begin
  if SysModule.path.__contains__(Path) then
    Result := false
  else begin
    SysModule.path.insert(0, Path);
    Result := true;
  end;
end;

function TPyInternalInterpreter.SysPathRemove(const Path: string): boolean;
begin
  if SysModule.path.__contains__(Path) then begin
    Result := True;
    SysModule.path.remove(Path);
  end else
    Result := False;
end;

procedure TPyInternalInterpreter.SysPathToStrings(Strings: TStrings);
var
  i: Integer;
begin
  for i := 0 to Len(SysModule.path) - 1  do
    Strings.Add(SysModule.path.__getitem__(i));
end;

function TPyInternalInterpreter.UnitTestResult: Variant;
begin
  Result := PyInteractiveInterpreter.IDETestResult();
end;

end.






