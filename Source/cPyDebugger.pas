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
  System.SyncObjs,
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
    constructor Create(aName : string; const aPyObject : Variant);
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
    function GetInterpreter: Variant; override;
  public
    constructor Create(II : Variant);
    function CreateDebugger: TPyBaseDebugger; override;

    procedure Initialize; override;
    function SysPathAdd(const Path : string) : boolean; override;
    function SysPathRemove(const Path : string) : boolean; override;
    procedure SysPathToStrings(Strings : TStrings); override;
    procedure StringsToSysPath(Strings : TStrings); override;
    function Compile(ARunConfig : TRunConfiguration) : Variant;
    function GetGlobals : TBaseNameSpaceItem; override;
    procedure GetModulesOnPath(const Path : Variant; SL : TStrings); override;
    function NameSpaceFromExpression(const Expr : string) : TBaseNameSpaceItem; override;
    function CallTipFromExpression(const Expr : string;
      var DisplayString, DocString : string) : Boolean; override;
    procedure SetCommandLine(ARunConfig : TRunConfiguration); override;
    procedure RestoreCommandLine; override;
    function ImportModule(Editor : IEditor; AddToNameSpace : Boolean = False) : Variant; override;
    procedure Run(ARunConfig : TRunConfiguration); override;
    function SyntaxCheck(Editor : IEditor; out ErrorPos: TEditorPos; Quiet : Boolean = False) : Boolean;
    function RunSource(Const Source, FileName : Variant; symbol : string = 'single') : boolean; override;
    function EvalCode(const Expr : string) : Variant; override;
    procedure SystemCommand(const Cmd : string); override;
    function GetObjectType(Ob : Variant) : string; override;
    function UnitTestResult : Variant; override;
    function NameSpaceItemFromPyObject(aName : string; aPyObject : Variant): TBaseNameSpaceItem; override;
    property Debugger : Variant read fDebugger;
  end;

  TPyInternalDebugger = class(TPyBaseDebugger)
  // pdb based internal debugger
  private
    fDebuggerCommand: TDebuggerCommand;
    fLineCache: Variant;
    fMainThread: TThreadInfo;
    fDebugEvent: TSimpleEvent;
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
    function GetPostMortemEnabled: boolean; override;
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
  System.StrUtils,
  System.IOUtils,
  Vcl.Dialogs,
  JvDSADialogs,
  JvGnugettext,
  JclSysInfo,
  VarPyth,
  StringResources,
  uCommonFunctions,
  cPyScripterSettings,
  cPyControl;

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

constructor TNameSpaceItem.Create(aName : string; const aPyObject: Variant);
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
    SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
    try
      Result := PyControl.InternalInterpreter.PyInteractiveInterpreter.membercount(fPyObject, ExpandSequences,
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
    SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
    try
      FullInfoTuple := PyControl.InternalInterpreter.PyInteractiveInterpreter.safegetmembersfullinfo(fPyObject, ExpandSequences,
        ExpandCommonTypes, ExpandSequences);
      fChildCount := len(FullInfoTuple);

      if fChildCount > 0 then begin
        fChildNodes := TStringList.Create;
        fChildNodes.CaseSensitive := True;

        PyFullInfoTuple := ExtractPythonObjectFrom(FullInfoTuple);
        for i := 0 to fChildCount - 1 do with GetPythonEngine do begin
          PyMemberInfo := PyTuple_GetItem(PyFullInfoTuple, i);
          ObjName :=  PyUnicodeAsString(PyTuple_GetItem(PyMemberInfo, 0));
          PyFullInfo := PyTuple_GetItem(PyMemberInfo, 1);
          APyObject := VarPythonCreate(PyTuple_GetItem(PyFullInfo, 0));

          NameSpaceItem := TNameSpaceItem.Create(ObjName, APyObject);
          NameSpaceItem.ExpandCommonTypes := ExpandCommonTypes;
          NameSpaceItem.ExpandSequences := ExpandSequences;

          //NameSpaceItem.BufferedValue := PyString_AsWideString(PyTuple_GetItem(PyFullInfo, 1));
          //NameSpaceItem.GotBufferedValue := True;
          NameSpaceItem.fObjectType := PyUnicodeAsString(PyTuple_GetItem(PyFullInfo, 1));
          NameSpaceItem.fObjectInfo := PyLong_AsLong(PyTuple_GetItem(PyFullInfo, 2));
          //NameSpaceItem.fChildCount := PyInt_AsLong(PyTuple_GetItem(PyFullInfo, 3));

          fChildNodes.AddObject(ObjName, NameSpaceItem);
        end;
        GetPythonEngine.CheckError;
        if (ObjectType <> 'list') and (ObjectType <> 'tuple') then
          fChildNodes.CustomSort(ComparePythonIdents);
      end;
      // VarClear(APyObject);
      // VarClear(FullInfoTuple);
    except
      fChildCount := 0;
      StyledMessageDlg(Format(_(SErrorGettingNamespace), [fName]), mtError, [mbAbort], 0);
      System.SysUtils.Abort;
    end;
  end;
end;

function TNameSpaceItem.GetDocString: string;
Var
  SuppressOutput : IInterface;
begin
  SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
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
  SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
  try
    fObjectInfo := PyControl.InternalInterpreter.PyInteractiveInterpreter.objectinfo(fPyObject);
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
  SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
  try
    Result :=  PyControl.InternalInterpreter.PyInteractiveInterpreter.saferepr(fPyObject);
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
  fDebugEvent := TSimpleEvent.Create(nil, False, False, '');

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
  FreeAndNil(fDebugEvent);
  inherited;
end;

procedure TPyInternalDebugger.EnterPostMortem;
Var
  Py: IPyEngineAndGIL;
  Frame, BotFrame, TraceBack : Variant;
begin
  Py:= GI_PyControl.SafePyEngine;
  if not (HaveTraceback and (PyControl.DebuggerState = dsInactive)) then
    Exit;

  GI_PyInterpreter.SetPyInterpreterPrompt(pipPostMortem);
  GI_PyInterpreter.AppendPrompt;

  TraceBack := SysModule.last_traceback;
  Botframe := TraceBack.tb_frame;
  while not VarIsNone(TraceBack.tb_next) do
    TraceBack := TraceBack.tb_next;
  Frame := TraceBack.tb_frame;

  PyControl.DebuggerState := dsPostMortem;
  fMainThread.Status := thrdBroken;
  GetCallStack(fMainThread.CallStack, Frame, Botframe);
  TPyBaseDebugger.ThreadChangeNotify(fMainThread, tctAdded );

  DSAMessageDlg(dsaPostMortemInfo, 'PyScripter', _(SPostMortemInfo),
     mtInformation, [mbOK], 0, dckActiveForm, 0, mbOK);
end;

function TPyInternalDebugger.Evaluate(const Expr: string): TBaseNamespaceItem;
Var
  Py: IPyEngineAndGIL;
  SuppressOutput : IInterface;
  V : Variant;
begin
  Result := nil;
  if PyControl.DebuggerState in [dsPaused, dsPostMortem] then begin
    SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
    try
      Py := GI_PyControl.SafePyEngine;
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
  Py: IPyEngineAndGIL;
  SuppressOutput : IInterface;
  V : Variant;
begin
  ObjType := _(SNotAvailable);
  Value := _(SNotAvailable);
  if PyControl.DebuggerState in [dsPaused, dsPostMortem] then begin
    SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
    try
      Py := GI_PyControl.SafePyEngine;
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
  GI_PyInterpreter.SetPyInterpreterPrompt(pipNormal);
  GI_PyInterpreter.AppendPrompt;

  fMainThread.Status := thrdRunning;
  fMainThread.CallStack.Clear;
  TPyBaseDebugger.ThreadChangeNotify(fMainThread, tctStatusChange);
  MakeFrameActive(nil);
  PyControl.DebuggerState := dsInactive;
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

function TPyInternalDebugger.GetPostMortemEnabled: boolean;
begin
  Result := InternalInterpreter.CanDoPostMortem;
end;

function TPyInternalDebugger.HaveTraceback: boolean;
begin
  try
    var Py := GI_PyControl.SafePyEngine;
    Result := VarModuleHasObject(SysModule, 'last_traceback');
  except
    Result := False;
  end;
end;

procedure TPyInternalDebugger.Pause;
begin
  fDebuggerCommand := dcPause;
  if PyControl.DebuggerState = dsPaused then fDebugEvent.SetEvent;
end;

// Timer callback function
procedure TPyInternalDebugger.RestoreCommandLine;
begin
  InternalInterpreter.RestoreCommandLine;
end;

procedure TPyInternalDebugger.Resume;
begin
  fDebuggerCommand := dcRun;
  if PyControl.DebuggerState = dsPaused then fDebugEvent.SetEvent;
end;

procedure TPyInternalDebugger.Debug(ARunConfig: TRunConfiguration;
  InitStepIn: Boolean = False; RunToCursorLine : integer = -1);
var
  Code : Variant;
  Path, OldPath : string;
  PythonPathAdder : IInterface;
  ReturnFocusToEditor: Boolean;
  [weak] Editor : IEditor;
begin
  InternalInterpreter.CanDoPostMortem := False;

  // Repeat here to make sure it is set right
  MaskFPUExceptions(PyIDEOptions.MaskFPUExceptions);

  fDebuggerCommand := dcRun;
  Assert(PyControl.DebuggerState = dsInactive);

  //Compile
  Code := InternalInterpreter.Compile(ARunConfig);

  if VarIsPython(Code) then begin

    // Add the path of the script to the Python Path - Will be automatically removed
    Path := InternalInterpreter.ToPythonFileName(ARunConfig.ScriptName);
    if Path.StartsWith('<') then
      Path := ''
    else
      Path := TPath.GetDirectoryName(Path);
    InternalInterpreter.SysPathRemove('');
    if Length(Path) > 1 then
      PythonPathAdder := InternalInterpreter.AddPathToPythonPath(Path);

    // Set the Working directory
    if ARunConfig.WorkingDir <> '' then
      Path := GI_PyIDEServices.ReplaceParams(ARunConfig.WorkingDir);
    if Path.Length <= 1 then
      Path := InternalInterpreter.SystemTempFolder;
    OldPath := GetCurrentDir;

    // Change the current path
    try
      SetCurrentDir(Path)
    except
      StyledMessageDlg(_(SCouldNotSetDirectory), mtWarning, [mbOK], 0);
    end;

    PyControl.DebuggerState := dsDebugging;
    TPyBaseDebugger.ThreadChangeNotify(fMainThread, tctAdded );

    GI_PyIDEServices.Messages.ClearMessages;
    Editor := GI_ActiveEditor;
    ReturnFocusToEditor := Assigned(Editor);
    // Set the layout to the Debug layout is it exists
    if GI_PyIDEServices.Layouts.LayoutExists('Debug') then begin
      GI_PyIDEServices.Layouts.SaveLayout('Current');
      GI_PyIDEServices.Layouts.LoadLayout('Debug');
      Application.ProcessMessages;
    end else
      GI_PyInterpreter.ShowWindow;

    //try
    GI_PyInterpreter.SetPyInterpreterPrompt(pipDebug);
    GI_PyInterpreter.AppendPrompt;
    //attach debugger callback routines
    with PyControl.InternalPython.DebugIDE.Events do begin
      Items[dbie_user_call].OnExecute := UserCall;
      Items[dbie_user_line].OnExecute := UserLine;
      Items[dbie_user_thread].OnExecute := UserThread;
      Items[dbie_user_exception].OnExecute := UserException;
      Items[dbie_user_yield].OnExecute := UserYield;
    end;

    // New Line for output
    GI_PyInterpreter.AppendText(sLineBreak);

    // Set the command line parameters
    SetCommandLine(ARunConfig);

    //set breakpoints
    SetDebuggerBreakPoints;

    GI_PyControl.ThreadPythonExec(procedure
    begin
      if RunToCursorLine >= 0 then  // add temp breakpoint
        InternalInterpreter.Debugger.set_break(Code.co_filename, RunToCursorLine, 1);

      InternalInterpreter.Debugger.InitStepIn := InitStepIn;
      try
        InternalInterpreter.Debugger.run(Code);
      except
        // CheckError already called by VarPyth
        on E: EPythonError do begin
          TThread.Synchronize(nil, procedure
          begin
            InternalInterpreter.HandlePyException(GetPythonEngine.Traceback, E.Message, 2);
            ReturnFocusToEditor := False;
            InternalInterpreter.CanDoPostMortem := True;
            //System.SysUtils.Abort;
          end);
        end;
      end;
      VarClear(Code);
    end,
    //finally
    procedure
    begin
      MakeFrameActive(nil);
      GI_PyInterpreter.WritePendingMessages;
      GI_PyInterpreter.SetPyInterpreterPrompt(pipNormal);
      GI_PyInterpreter.AppendPrompt;

      // Restore the command line parameters
      RestoreCommandLine;

      //  Add again the empty path
      InternalInterpreter.SysPathAdd('');

      // Change the back current path
      SetCurrentDir(OldPath);

      if GI_PyIDEServices.Layouts.LayoutExists('Debug') then
        GI_PyIDEServices.Layouts.LoadLayout('Current');

      PyControl.DebuggerState := dsInactive;
      if ReturnFocusToEditor and Assigned(Editor) then
        Editor.Activate;
      if InternalInterpreter.CanDoPostMortem and PyIDEOptions.PostMortemOnException then
        EnterPostMortem;
    end);
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
  FName := InternalInterpreter.ToPythonFileName(Editor.FileId);
  var Py := GI_PyControl.SafePyEngine;
  InternalInterpreter.Debugger.set_break(VarPythonCreate(FName), ALine, 1);

  fDebuggerCommand := dcRunToCursor;
  if PyControl.DebuggerState = dsPaused then fDebugEvent.SetEvent;
end;

procedure TPyInternalDebugger.StepInto;
begin
  fDebuggerCommand := dcStepInto;
  if PyControl.DebuggerState = dsPaused then fDebugEvent.SetEvent;
end;

procedure TPyInternalDebugger.StepOver;
begin
  fDebuggerCommand := dcStepOver;
  if PyControl.DebuggerState = dsPaused then fDebugEvent.SetEvent;
end;

procedure TPyInternalDebugger.StepOut;
begin
  fDebuggerCommand := dcStepOut;
  if PyControl.DebuggerState = dsPaused then fDebugEvent.SetEvent;
end;

procedure TPyInternalDebugger.Abort;
begin
  case PyControl.DebuggerState of
    dsPostMortem: ExitPostMortem;
    dsDebugging: fDebuggerCommand := dcAbort;
    //dsRunning: GetPythonEngine.PyErr_SetInterrupt;  //Generate Keyboard interrupt signal
    dsPaused:
      begin
        fDebuggerCommand := dcAbort;
        fDebugEvent.SetEvent;;
      end;
  end;
end;

procedure TPyInternalDebugger.UserCall(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
begin
   // GI_PyInterpreter.AppendText('UserCall'+sLineBreak);
   Result := GetPythonEngine.ReturnNone;
end;

procedure TPyInternalDebugger.UserException(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
   // GI_PyInterpreter.AppendText('UserException'+sLineBreak);
   Result := GetPythonEngine.ReturnNone;
end;

procedure TPyInternalDebugger.UserLine(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
Var
  Frame : Variant;
  FName : string;
  LineNo: integer;
  CanBreak: Boolean;
begin
  Result := GetPythonEngine.ReturnNone;

  Frame := VarPythonCreate(Args).__getitem__(0);
  FName := InternalInterpreter.FromPythonFileName(Frame.f_code.co_filename);
  LineNo := Frame.f_lineno;

  CanBreak := (LineNo > 0);

  TThread.Synchronize(nil, procedure
  begin
    CanBreak :=  (LineNo > 0) and
      (not PyIDEOptions.TraceOnlyIntoOpenFiles or
       Assigned(GI_EditorFactory.GetEditorByFileId(FName))) and
       GI_PyIDEServices.ShowFilePosition(FName, LineNo, 1, 0, True, False);
  end);

  if CanBreak then
  begin
    if PyControl.DebuggerState = dsDebugging then
      PyControl.DebuggerState := dsPaused;
    fMainThread.Status := thrdBroken;
    GetCallStack(fMainThread.CallStack, Frame, InternalInterpreter.Debugger.botframe);
    TPythonThread.Py_Begin_Allow_Threads;
    TThread.Synchronize(nil, procedure
    begin
      TPyBaseDebugger.ThreadChangeNotify(fMainThread, tctStatusChange);
    end);
    TPythonThread.Py_End_Allow_Threads;

    FDebuggerCommand := dcNone;

    TPythonThread.Py_Begin_Allow_Threads;
    FDebugEvent.WaitFor(INFINITE);
    TPythonThread.Py_End_Allow_Threads;

    PyControl.DebuggerState := dsDebugging;

    if PyControl.BreakPointsChanged then SetDebuggerBreakpoints;

    fMainThread.Status := thrdRunning;

    TPythonThread.Py_Begin_Allow_Threads;
    TThread.Synchronize(nil, procedure
    begin
      TPyBaseDebugger.ThreadChangeNotify(fMainThread, tctStatusChange);
      fMainThread.CallStack.Clear;
    end);
    TPythonThread.Py_End_Allow_Threads;
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
                      TThread.Synchronize(nil, procedure
                      begin
                        GI_PyIDEServices.Messages.AddMessage(_(SDebuggingAborted));
                        GI_PyIDEServices.Messages.ShowWindow;
                      end);
                    end;
   end;
   VarClear(Frame);
end;

procedure TPyInternalDebugger.UserThread(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
begin
   Result := GetPythonEngine.ReturnNone;
end;

procedure TPyInternalDebugger.UserYield(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
begin
  if fDebuggerCommand = dcAbort then begin
    InternalInterpreter.Debugger.set_quit();
    TThread.Synchronize(nil, procedure
    begin
      GI_PyIDEServices.Messages.AddMessage(_(SDebuggingAborted));
      GI_PyIDEServices.Messages.ShowWindow;
    end);
  end else if fDebuggerCommand = dcPause then
    InternalInterpreter.Debugger.set_step();
  Result := GetPythonEngine.ReturnNone;
end;

procedure TPyInternalDebugger.SetCommandLine(ARunConfig : TRunConfiguration);
begin
  InternalInterpreter.SetCommandLine(ARunConfig);
end;

procedure TPyInternalDebugger.SetDebuggerBreakpoints;
begin
  if not PyControl.BreakPointsChanged then Exit;

  var Py := GI_PyControl.SafePyEngine;
  LoadLineCache;
  InternalInterpreter.Debugger.clear_all_breaks();

  GI_EditorFactory.ApplyToEditors(procedure(Editor: IEditor)
  var
    FName : string;
  begin
    FName := InternalInterpreter.ToPythonFileName(Editor.FileId);
    for var I := 0 to Editor.BreakPoints.Count - 1 do begin
      var BreakPoint := TBreakPoint(Editor.BreakPoints[I]);
      if not BreakPoint.Disabled then begin
        if BreakPoint.Condition <> '' then begin
          InternalInterpreter.Debugger.set_break(VarPythonCreate(FName),
            BreakPoint.LineNo, 0, VarPythonCreate(BreakPoint.Condition));
        end else begin
          InternalInterpreter.Debugger.set_break(VarPythonCreate(FName),
            BreakPoint.LineNo);
        end;
      end;
    end;
  end);

  PyControl.BreakPointsChanged := False;
end;

procedure TPyInternalDebugger.LoadLineCache;
begin
  // inject unsaved code into LineCache
  fLineCache.cache.clear();
  GI_EditorFactory.ApplyToEditors(procedure(Editor: IEditor)
  var
    FName, Source, LineList: Variant;
    SFName: string;
  begin
    with Editor do begin
      if not HasPythonFile then Exit;
      SFName := InternalInterpreter.ToPythonFileName(FileId);
      if SFName.StartsWith('<') then
      begin
        FName := SFName;
        Source := CleanEOLs(SynEdit.Text)+WideLF;
        LineList := VarPythonCreate(Source);
        LineList := LineList.splitlines(True);
        fLineCache.cache.SetItem(VarPythonCreate(FName),
          VarPythonCreate([Length(Source), None, LineList, FName], stTuple));
      end;
    end;
  end);
end;

procedure TPyInternalDebugger.MakeFrameActive(Frame: TBaseFrameInfo);
begin
  var Py := GI_PyControl.SafePyEngine;
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
  if PyIDEOptions.PrettyPrintOutput then
    fII.setupdisplayhook()
end;

function TPyInternalInterpreter.CreateDebugger: TPyBaseDebugger;
begin
  Result := TPyInternalDebugger.Create;
end;

function TPyInternalInterpreter.EvalCode(const Expr: string): Variant;
begin
  // may raise exceptions
  Result := fII.evalcode(Expr);
end;

function TPyInternalInterpreter.CallTipFromExpression(const Expr: string;
  var DisplayString, DocString: string) : Boolean;
var
  Py: IPyEngineAndGIL;
  LookupObj, ArgText, PyDocString: Variant;
  SuppressOutput : IInterface;
begin
  Result := False;
  DisplayString := '';
  DocString := '';
  if Expr = '' then Exit;

  Py := GI_PyControl.SafePyEngine;
  SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
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
    StyledMessageDlg(_(SCannotCompileWhileRunning),
      mtError, [mbAbort], 0);
    System.SysUtils.Abort;
  end;

  var Py := GI_PyControl.SafePyEngine;

  VarClear(Result);
  PyControl.ErrorPos := TEditorPos.EmptyPos;

  GI_PyIDEServices.Messages.ClearMessages;

  Editor := GI_EditorFactory.GetEditorByFileId(ARunConfig.ScriptName);
  if Assigned(Editor) then begin
    Source := CleanEOLs(Editor.EncodedText) + AnsiString(#10);
  end else begin
    try
      Source := CleanEOLs(FileToEncodedStr(ARunConfig.ScriptName)) + AnsiString(#10);
    except
      on E: Exception do begin
        StyledMessageDlg(Format(_(SFileOpenError), [ARunConfig.ScriptName, E.Message]), mtError, [mbOK], 0);
        System.SysUtils.Abort;
      end;
    end;
  end;
  FName := Py.PythonEngine.EncodeWindowsFilePath(ToPythonFileName(ARunConfig.ScriptName));

  co := Py.PythonEngine.Py_CompileString(PAnsiChar(Source), PAnsiChar(FName), file_input );
  if not Assigned( co ) then begin
    // New Line for output
    GI_PyInterpreter.AppendText(sLineBreak);
    try
      // Print and throw exception for the error
      Py.PythonEngine.CheckError;
    except
      on E: EPySyntaxError do begin
        if GI_PyIDEServices.ShowFilePosition(E.EFileName, E.ELineNumber, E.EOffset) and
          Assigned(GI_ActiveEditor)
        then
          PyControl.ErrorPos :=
            TEditorPos.NPos(GI_ActiveEditor, E.ELineNumber, E.EOffset, True);

        GI_PyInterpreter.AppendPrompt;
        System.SysUtils.Abort;
      end;
      on E: EPythonError do begin  //may raise OverflowError or ValueError
        HandlePyException(Py.PythonEngine.Traceback, E.Message);
        System.SysUtils.Abort;
      end;
    end;
  end else begin
    Result := VarPythonCreate(co);
    // Cleanup ByteCode
    Py.PythonEngine.Py_XDECREF(co);
  end;
end;

function TPyInternalInterpreter.GetGlobals: TBaseNameSpaceItem;
begin
  Result := TNameSpaceItem.Create('globals', VarPythonEval('globals()'));
end;

procedure TPyInternalInterpreter.GetModulesOnPath(const Path: Variant; SL: TStrings);
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
  SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
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
  var Py := GI_PyControl.SafePyEngine;
  Assert(Assigned(Editor));
  VarClear(Result);
  //Compile
  RunConfiguration := TRunConfiguration.Create;
  try
    RunConfiguration.ScriptName := Editor.FileId;
    Code := Compile(RunConfiguration);
  finally
    RunConfiguration.Free;
  end;

  // Add the path of the imported script to the Python Path
  Path := ToPythonFileName(Editor.FileId);
  if Path.StartsWith('<') then
    Path := ''
  else
    Path := TPath.GetDirectoryName(Path);
  if Path.Length > 1 then begin
    PythonPathAdder := AddPathToPythonPath(Path, False);
    SysPathRemove('');
  end;

  if Editor.FileName <> '' then
    NameOfModule := FileNameToModuleName(Editor.FileName)
  else
    NameOfModule := ChangeFileExt(Editor.FileTitle, '');

  PyControl.DebuggerState := dsRunning;
  try
    try
      Result := fII._import(NameOfModule, Code);
      Py.PythonEngine.CheckError;
    except
      on E: EPythonError do begin
        HandlePyException(Py.PythonEngine.Traceback, E.Message);
        Result := None;
      end;
    end;
    VarClear(Code);
    if VarIsNone(Result) then begin
      StyledMessageDlg(_(SErrorInImportingModule), mtError, [mbOK], 0);
      System.SysUtils.Abort;
    end else if AddToNameSpace then
      // add Module name to the locals() of the interpreter
      Py.PythonEngine.ExecString(AnsiString('import ' + NameOfModule));
  finally
    //  Add again the empty path
    SysPathAdd('');

    PyControl.DebuggerState := dsInactive;
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
  SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
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
var
  Py: IPyEngineAndGIL;
begin
  Py := GI_PyControl.SafePyEngine;
  SysModule.argv := fOldargv;
end;

procedure TPyInternalInterpreter.SetCommandLine(ARunConfig : TRunConfiguration);
var
  Py: IPyEngineAndGIL;
  SysMod : Variant;
  S, Param : string;
  P : PChar;
begin
  Py := GI_PyControl.SafePyEngine;
  SysMod := SysModule;
  fOldargv := SysMod.argv;
  SysMod.argv := NewPythonList;
  // Workaround due to PREFER_UNICODE flag to make sure
  // no conversion to Unicode and back will take place
  S := ToPythonFileName(ARunConfig.ScriptName);
  SysMod.argv.append(VarPythonCreate(S));

  S := Trim(ARunConfig.Parameters);
  if S <> '' then begin
    S := GI_PyIDEServices.ReplaceParams(S);
    P := PChar(S);
    while P[0] <> #0 do begin
      P := GetParamStr(P, Param);
      SysMod.argv.append(VarPythonCreate(Param))
    end;
    GI_PyInterpreter.AppendText(Format(_(SCommandLineMsg), [S]));
  end;
end;

procedure TPyInternalInterpreter.StringsToSysPath(Strings: TStrings);
var
  Py: IPyEngineAndGIL;
  i: Integer;
  PythonPath: Variant;
begin
  Py := GI_PyControl.SafePyEngine;
  PythonPath := NewPythonList;
  for i := 0 to Strings.Count - 1 do
    PythonPath.append(Strings[i]);
  SysModule.path := PythonPath;
end;

procedure TimeCallBack(TimerID, Msg: Uint; dwUser, dw1, dw2: DWORD); stdcall;
begin
  if PLongWord(dwUser)^ = 0 then Exit;
  if (MessageBoxW(0, PWideChar(_(SInterruptRunningScript)),
    'ScriptTimeOut', MB_ICONWARNING or MB_YESNO) = idYes) then
  begin
    GetPythonEngine.PyErr_SetInterrupt;  //Generate Keyboard interrupt signal
    TimeKillEvent(PLongWord(dwUser)^);
    PLongWord(dwUser)^ := 0;
  end;
end;

function TPyInternalInterpreter.GetInterpreter: Variant;
begin
  Result := fII;
end;

procedure TPyInternalInterpreter.Run(ARunConfig: TRunConfiguration);
Var
  Code : Variant;
  mmResult,Resolution : LongWord;
  tc : TTimeCaps;
  Path, OldPath : string;
  PythonPathAdder : IInterface;
  ReturnFocusToEditor : Boolean;
  [weak] Editor : IEditor;
begin
  CanDoPostMortem := False;

  // Repeat here to make sure it is set right
  MaskFPUExceptions(PyIDEOptions.MaskFPUExceptions);

  //Compile
  Code := Compile(ARunConfig);

  if VarIsPython(Code) then begin
    PyControl.DebuggerState := dsRunning;

    // New Line for output
    GI_PyInterpreter.AppendText(sLineBreak);

    mmResult := 0;
    Resolution := 100;

    // Add the path of the executed file to the Python path - Will be automatically removed
    Path := ToPythonFileName(ARunConfig.ScriptName);
    if Path.StartsWith('<') then
      Path := ''
    else
      Path := TPath.GetDirectoryName(Path);
    SysPathRemove('');
    if Length(Path) > 1 then
      PythonPathAdder := AddPathToPythonPath(Path);

  // Set the Working directory
    if ARunConfig.WorkingDir <> '' then
      Path := GI_PyIDEServices.ReplaceParams(ARunConfig.WorkingDir);
    if Path.Length <= 1 then
      Path := SystemTempFolder;
    OldPath := GetCurrentDir;

    try
      SetCurrentDir(Path)
    except
      StyledMessageDlg(_(SCouldNotSetDirectory), mtWarning, [mbOK], 0);
    end;

    // Set the command line parameters
    SetCommandLine(ARunConfig);

    Editor := GI_ActiveEditor;
    ReturnFocusToEditor := Assigned(Editor);
    GI_PyInterpreter.ShowWindow;

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
      var Py := GI_PyControl.SafePyEngine;
      try
        try
          fII.run_nodebug(Code);
        finally
          VarClear(Code);
        end;
      except
        // CheckError already called by VarPyth
        on E: EPythonError do
        begin
          HandlePyException(Py.PythonEngine.Traceback, E.Message);
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
      GI_PyInterpreter.AppendPrompt;

      // Restore the command line parameters
      RestoreCommandLine;

      //  Add again the empty path
      SysPathAdd('');

      // Change the back current path
      SetCurrentDir(OldPath);

      PyControl.DebuggerState := dsInactive;
      if ReturnFocusToEditor and Assigned(Editor) then
        Editor.Activate;
      if CanDoPostMortem and PyIDEOptions.PostMortemOnException then
        PyControl.ActiveDebugger.EnterPostMortem;
    end;
  end;
end;

function TPyInternalInterpreter.RunSource(Const Source, FileName : Variant; symbol : string = 'single') : boolean;
Var
  OldDebuggerState : TDebuggerState;
  OldPos : TEditorPos;
begin
  Assert(not GI_PyControl.Running, 'RunSource called while the Python engine is active');

  OldDebuggerState := PyControl.DebuggerState;
  OldPos := PyControl.CurrentPos;
  PyControl.DebuggerState := dsRunning;
  try
    var Py := GI_PyControl.SafePyEngine;
    // Workaround due to PREFER_UNICODE flag to make sure
    // no conversion to Unicode and back will take place
    var PySource := VarPythonCreate(Source);
    Result := fII.runsource(PySource, FileName, symbol);
  finally
    PyControl.DebuggerState := OldDebuggerState;
    if OldDebuggerState = dsPaused then
      PyControl.CurrentPos := OldPos;
  end;
end;

function TPyInternalInterpreter.SyntaxCheck(Editor: IEditor; out ErrorPos: TEditorPos; Quiet : Boolean = False): Boolean;
Var
  FName: string;
  Source: AnsiString;
  SuppressOutput: IInterface;
  Flags: PyCompilerFlags;
begin
  ErrorPos := TEditorPos.EmptyPos;

  TThread.Synchronize(nil, procedure
  begin
    FName := ToPythonFileName(Editor.FileId);
    Source := CleanEOLs(Editor.EncodedText)+AnsiString(#10);
  end);

  with GI_PyControl.SafePyEngine.PythonEngine do begin
    if Quiet then
      SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors

    Result := True;
    Flags.flags := PyCF_ONLY_AST;
    Flags.cf_feature_version := MinorVersion;
    try
      // Print and throw exception for the error
      Py_CompileStringExFlags(PAnsiChar(Source),
        PAnsiChar(EncodeWindowsFilePath(FName)), file_input, @Flags, -1);
      if Assigned(PyErr_Occurred()) and not Quiet then
        GI_PyInterpreter.AppendText(sLineBreak);
      CheckError;
    except
      on E: EPySyntaxError do begin
        Result := False;
        ErrorPos := TEditorPos.NPos(Editor, E.ELineNumber, E.EOffset, True);

        if not Quiet then
        begin
          GI_PyIDEServices.Messages.ClearMessages;
          // New Line for output
          GI_PyInterpreter.AppendPrompt;
          if GI_PyIDEServices.ShowFilePosition(E.EFileName, E.ELineNumber, E.EOffset) then
            PyControl.ErrorPos := ErrorPos;
        end;
      end;
    end;
  end;
end;

function TPyInternalInterpreter.SysPathAdd(const Path: string): boolean;
Var
  Py: IPyEngineAndGIL;
begin
  Py := GI_PyControl.SafePyEngine;
  if SysModule.path.__contains__(Path) then
    Result := false
  else begin
    SysModule.path.insert(0, Path);
    Result := true;
  end;
end;

function TPyInternalInterpreter.SysPathRemove(const Path: string): boolean;
Var
  Py: IPyEngineAndGIL;
begin
  Py := GI_PyControl.SafePyEngine;
  if SysModule.path.__contains__(Path) then begin
    Result := True;
    SysModule.path.remove(Path);
  end else
    Result := False;
end;

procedure TPyInternalInterpreter.SysPathToStrings(Strings: TStrings);
var
  Py: IPyEngineAndGIL;
  i: Integer;
begin
  Py := GI_PyControl.SafePyEngine;
  for i := 0 to Len(SysModule.path) - 1  do
    Strings.Add(SysModule.path.__getitem__(i));
end;

procedure TPyInternalInterpreter.SystemCommand(const Cmd: string);
begin
  var Py := GI_PyControl.SafePyEngine;
  fII.system_command(Cmd);
end;

function TPyInternalInterpreter.UnitTestResult: Variant;
begin
  Result := PyInteractiveInterpreter.IDETestResult();
end;

end.






