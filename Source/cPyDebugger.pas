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
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.SyncObjs,
  PythonEngine,
  uEditAppIntfs,
  cPySupportTypes,
  cPyBaseDebugger;

type
  TFrameInfo = class(TBaseFrameInfo)
  private
    FPyFrame: Variant;
  protected
  // Implementation of the Base class for the internal debugger
    function GetFunctionName: string; override;
    function GetFileName: string; override;
    function GetLine: Integer; override;
  public
    constructor Create(Frame: Variant);
    property PyFrame: Variant read FPyFrame;
  end;

const
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
    FChildCount: Integer;
    FChildNodes: TStringList;
    FName: string;
    FObjectInfo: Integer;
    function GetName: string; override;
    function GetObjectType: string; override;
    function GetValue: string; override;
    function GetDocString: string; override;
    function GetChildCount: Integer; override;
    function GetChildNode(Index: Integer): TBaseNameSpaceItem; override;
    function GetObjectInfo: Integer;
    procedure FillObjectInfo; virtual;
  public
    constructor Create(AName: string; const APyObject: Variant);
    destructor Destroy; override;
    function IsClass: Boolean; override;
    function IsDict: Boolean; override;
    function IsModule: Boolean; override;
    function IsFunction: Boolean; override;
    function IsMethod: Boolean; override;
    function Has__dict__: Boolean; override;
    function IndexOfChild(AName: string): Integer; override;
    procedure GetChildNodes; override;
    property ObjectInfo: Integer read GetObjectInfo;
  end;

  TPyInternalInterpreter = class(TPyBaseInterpreter)
  private
    FII: Variant;  // Python VarPyth wrapper to the interactive interpreter
    FDebugger: Variant;
    FOldargv: Variant;
  protected
    function GetInterpreter: Variant; override;
  public
    constructor Create(II: Variant);
    function CreateDebugger: TPyBaseDebugger; override;

    function SysPathAdd(const Path: string): Boolean; override;
    function SysPathRemove(const Path: string): Boolean; override;
    procedure SysPathToStrings(Strings: TStrings); override;
    procedure StringsToSysPath(Strings: TStrings); override;
    function Compile(ARunConfig: TRunConfiguration): Variant;
    function GetGlobals: TBaseNameSpaceItem; override;
    function NameSpaceFromExpression(const Expr: string): TBaseNameSpaceItem; override;
    function CallTipFromExpression(const Expr: string;
      var DisplayString, DocString: string): Boolean; override;
    procedure SetCommandLine(ARunConfig: TRunConfiguration); override;
    procedure RestoreCommandLine; override;
    function ImportModule(Editor: IEditor; AddToNameSpace: Boolean = False): Variant; override;
    procedure Run(ARunConfig: TRunConfiguration); override;
    function SyntaxCheck(Editor: IEditor; out ErrorPos: TEditorPos; Quiet: Boolean = False): Boolean;
    function RunSource(const Source, FileName: string; const Symbol: string = 'single'): Boolean; override;
    function EvalCode(const Expr: string): Variant; override;
    procedure SystemCommand(const Cmd: string); override;
    function GetObjectType(Obj: Variant): string; override;
    function UnitTestResult: Variant; override;
    function NameSpaceItemFromPyObject(AName: string; APyObject: Variant): TBaseNameSpaceItem; override;
    procedure Pickle(AValue: Variant; const FileName: string); override;
    property Debugger: Variant read FDebugger;
  end;

  TPyInternalDebugger = class(TPyBaseDebugger)
  // pdb based internal debugger
  private
    FDebuggerCommand: TDebuggerCommand;
    FLineCache: Variant;
    FMainThread: TThreadInfo;
    FDebugEvent: TSimpleEvent;
    InternalInterpreter: TPyInternalInterpreter;
  protected
    procedure SetCommandLine(ARunConfig: TRunConfiguration); override;
    procedure RestoreCommandLine; override;
    procedure SetDebuggerBreakpoints; override;
    procedure LoadLineCache;
    procedure UserCall(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
    procedure UserLine(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
    procedure UserThread(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
    procedure UserException(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
    procedure UserYield(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
    // Fills in CallStackList with TBaseFrameInfo objects
    procedure GetCallStack(CallStackList: TObjectList<TBaseFrameInfo>;
      Frame, Botframe: Variant);
    function GetPostMortemEnabled: Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;

    // Debugging
    procedure Debug(ARunConfig: TRunConfiguration; InitStepIn: Boolean = False;
          RunToCursorLine: Integer = -1); override;
    procedure RunToCursor(Editor: IEditor; ALine: Integer); override;
    procedure StepInto; override;
    procedure StepOver; override;
    procedure StepOut; override;
    procedure Resume; override;
    procedure Pause; override;
    procedure Abort; override;
    // Evaluate expression in the current frame
    procedure Evaluate(const Expr: string; out ObjType, Value: string); overload; override;
    function Evaluate(const Expr: string): TBaseNameSpaceItem; overload; override;
    // Like the InteractiveInterpreter runsource but for the debugger frame
    function RunSource(const Source, FileName: string; const Symbol: string = 'single'): Boolean; override;
    // functions to get TBaseNamespaceItems corresponding to a frame's gloabals and locals
    function GetFrameGlobals(Frame: TBaseFrameInfo): TBaseNameSpaceItem; override;
    function GetFrameLocals(Frame: TBaseFrameInfo): TBaseNameSpaceItem; override;
    function NameSpaceFromExpression(const Expr: string): TBaseNameSpaceItem; override;
    procedure MakeThreadActive(Thread: TThreadInfo); override;
    procedure MakeFrameActive(Frame: TBaseFrameInfo); override;
    // post mortem stuff
    function HaveTraceback: Boolean; override;
    procedure EnterPostMortem; override;
    procedure ExitPostMortem; override;
  end;

implementation

uses
  Winapi.Windows,
  Winapi.MMSystem,
  System.UITypes,
  System.Math,
  System.IOUtils,
  System.Contnrs,
  Vcl.Forms,
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
  FPyFrame := Frame;
  inherited Create;
end;

function TFrameInfo.GetFileName: string;
begin
  Result := FPyFrame.f_code.co_filename;
end;

function TFrameInfo.GetFunctionName: string;
begin
  Result := FPyFrame.f_code.co_name;
end;

function TFrameInfo.GetLine: Integer;
begin
  Result := FPyFrame.f_lineno;
end;

{ TNameSpaceItem }

constructor TNameSpaceItem.Create(AName: string; const APyObject: Variant);
begin
  Assert(VarIsPython(APyObject), 'TNameSpaceItem.Create');
  FName := AName;
  FPyObject := APyObject;
  FChildCount := -1;  // unknown
  FObjectInfo := 0;
  FExpandSequences := True;
end;

destructor TNameSpaceItem.Destroy;
begin
  if Assigned(FChildNodes) then begin
    for var I := 0 to FChildNodes.Count - 1 do
      FChildNodes.Objects[I].Free;
    FChildNodes.Free;
  end;
  inherited;
end;

function TNameSpaceItem.GetChildCount: Integer;
begin
  if Assigned(FChildNodes) then
    Result := FChildNodes.Count
  else if FChildCount >= 0 then
    Result := FChildCount
  else begin
    var SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
    try
      Result := PyControl.InternalInterpreter.PyInteractiveInterpreter.membercount(FPyObject, ExpandSequences,
        ExpandCommonTypes, ExpandSequences);
    except
      Result := 0;
    end;
    FChildCount := Result;
  end;
end;

function TNameSpaceItem.GetChildNode(Index: Integer): TBaseNameSpaceItem;
begin
  Assert(Index >= 0, 'TNameSpaceItem.GetChildNode');
  if not Assigned(FChildNodes) then
    GetChildNodes;
  Assert(Index < FChildNodes.Count, 'TNameSpaceItem.GetChildNode');
  Result := FChildNodes.Objects[Index] as TBaseNameSpaceItem;
end;

procedure TNameSpaceItem.GetChildNodes;
var
  FullInfoTuple, APyObject: Variant;
  PyFullInfoTuple, PyMemberInfo, PyFullInfo: PPyObject;
  ObjName: string;
  NameSpaceItem: TNameSpaceItem;
begin
  if not (Assigned(FChildNodes) or FGotChildNodes) then begin
    FGotChildNodes := True;
    var SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
    try
      FullInfoTuple := PyControl.InternalInterpreter.PyInteractiveInterpreter.safegetmembersfullinfo(FPyObject, ExpandSequences,
        ExpandCommonTypes, ExpandSequences);
      FChildCount := len(FullInfoTuple);

      if FChildCount > 0 then begin
        FChildNodes := TStringList.Create;
        FChildNodes.CaseSensitive := True;

        PyFullInfoTuple := ExtractPythonObjectFrom(FullInfoTuple);
        for var I := 0 to FChildCount - 1 do with GetPythonEngine do begin
          PyMemberInfo := PyTuple_GetItem(PyFullInfoTuple, I);
          ObjName :=  PyUnicodeAsString(PyTuple_GetItem(PyMemberInfo, 0));
          PyFullInfo := PyTuple_GetItem(PyMemberInfo, 1);
          APyObject := VarPythonCreate(PyTuple_GetItem(PyFullInfo, 0));

          NameSpaceItem := TNameSpaceItem.Create(ObjName, APyObject);
          NameSpaceItem.ExpandCommonTypes := ExpandCommonTypes;
          NameSpaceItem.ExpandSequences := ExpandSequences;

          //NameSpaceItem.FBufferedValue := PyString_AsWideString(PyTuple_GetItem(PyFullInfo, 1));
          //NameSpaceItem.FGotBufferedValue := True;
          NameSpaceItem.FQualifiedObjectType := PyUnicodeAsString(PyTuple_GetItem(PyFullInfo, 1));
          NameSpaceItem.FObjectInfo := PyLong_AsLong(PyTuple_GetItem(PyFullInfo, 2));
          //NameSpaceItem.FChildCount := PyInt_AsLong(PyTuple_GetItem(PyFullInfo, 3));

          FChildNodes.AddObject(ObjName, NameSpaceItem);
        end;
        GetPythonEngine.CheckError;
        if (ObjectType <> 'list') and (ObjectType <> 'tuple') then
          FChildNodes.CustomSort(ComparePythonIdents);
      end;
    except
      FChildCount := 0;
      StyledMessageDlg(Format(_(SErrorGettingNamespace), [FName]), mtError, [mbAbort], 0);
      System.SysUtils.Abort;
    end;
  end;
end;

function TNameSpaceItem.GetDocString: string;
begin
  var SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
  try
    Result := Import('inspect').getdoc(FPyObject);
  except
    Result := '';
  end;
end;

function TNameSpaceItem.GetName: string;
begin
  Result := FName;
end;

procedure TNameSpaceItem.FillObjectInfo;
begin
  var SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
  try
    FObjectInfo := PyControl.InternalInterpreter.PyInteractiveInterpreter.objectinfo(FPyObject);
  except
    FObjectInfo := 0;
  end;
end;

function TNameSpaceItem.GetObjectType: string;
begin
  if FQualifiedObjectType <> '' then
    Result := FQualifiedObjectType.Substring(fQualifiedObjectType.LastIndexOf('.') + 1)
  else
    Result := PyControl.InternalInterpreter.GetObjectType(FPyObject);
end;

function TNameSpaceItem.GetValue: string;
begin
  var SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
  try
    Result :=  PyControl.InternalInterpreter.PyInteractiveInterpreter.saferepr(FPyObject);
  except
    Result := '';
  end;
end;

function TNameSpaceItem.Has__dict__: Boolean;
begin
  Result := (ObjectInfo and oi_Has__dict__) = oi_Has__dict__;
end;

function TNameSpaceItem.IndexOfChild(AName: string): Integer;
var
  L, H, I, C: Integer;
  Found: Boolean;
begin
  if not Assigned(FChildNodes) then
    GetChildNodes;
  if Assigned(FChildNodes) then begin
    // Child nodes are sorted with ComparePythonIdents
    // Do a quick search
    Result := -1;
    Found := False;
    L := 0;
    H := FChildNodes.Count - 1;
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := ComparePythonIdents(FChildNodes[I], AName);
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
    //Result := FChildNodes.IndexOf(AName)
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

function TNameSpaceItem.GetObjectInfo: Integer;
begin
  if (FObjectInfo and oi_Initialized) = 0 then
    FillObjectInfo;
  Result := FObjectInfo;
end;

{ TPyDebugger }

constructor TPyInternalDebugger.Create;
begin
  inherited Create;
  FDebugEvent := TSimpleEvent.Create(nil, False, False, '');

  InternalInterpreter := PyControl.InternalInterpreter as TPyInternalInterpreter;

  FLineCache := Import('linecache');

  FMainThread := TThreadInfo.Create;
  FMainThread.Name := 'MainThread';
  FMainThread.Status := thrdRunning;

  FDebuggerCommand := dcNone;
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

  FreeAndNil(FMainThread);
  FreeAndNil(FDebugEvent);
  inherited;
end;

procedure TPyInternalDebugger.EnterPostMortem;
var
  Py: IPyEngineAndGIL;
  Frame, BotFrame, TraceBack: Variant;
begin
  Py := SafePyEngine;
  if not (HaveTraceback and (GI_PyControl.DebuggerState = dsInactive)) then
    Exit;

  GI_PyInterpreter.SetPyInterpreterPrompt(pipPostMortem);
  GI_PyInterpreter.AppendPrompt;

  TraceBack := SysModule.last_traceback;
  BotFrame := TraceBack.tb_frame;
  while not VarIsNone(TraceBack.tb_next) do
    TraceBack := TraceBack.tb_next;
  Frame := TraceBack.tb_frame;

  GI_PyControl.DebuggerState := dsPostMortem;
  FMainThread.Status := thrdBroken;
  GetCallStack(FMainThread.CallStack, Frame, BotFrame);
  TPyBaseDebugger.ThreadChangeNotify(FMainThread, tctAdded );

  DSAMessageDlg(dsaPostMortemInfo, 'PyScripter', _(SPostMortemInfo),
     mtInformation, [mbOK], 0, dckActiveForm, 0, mbOK);
end;

function TPyInternalDebugger.Evaluate(const Expr: string): TBaseNameSpaceItem;
var
  Py: IPyEngineAndGIL;
  Value: Variant;
begin
  Result := nil;
  if GI_PyControl.DebuggerState in [dsPaused, dsPostMortem] then begin
    var SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
    try
      Py := SafePyEngine;
      // evalcode knows we are in the debugger and uses current frame locals/globals
      Value := InternalInterpreter.PyInteractiveInterpreter.evalcode(Expr);
      Result := TNameSpaceItem.Create(Expr, Value);
    except
      // fail quitely
    end;
  end;
end;

procedure TPyInternalDebugger.Evaluate(const Expr: string; out ObjType, Value: string);
var
  Py: IPyEngineAndGIL;
  ExprValue: Variant;
begin
  ObjType := _(SNotAvailable);
  Value := _(SNotAvailable);
  if GI_PyControl.DebuggerState in [dsPaused, dsPostMortem] then begin
    var SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
    try
      Py := SafePyEngine;
      // evalcode knows we are in the debugger and uses current frame locals/globals
      ExprValue := InternalInterpreter.PyInteractiveInterpreter.evalcode(Expr);
      ObjType := InternalInterpreter.PyInteractiveInterpreter.objecttype(ExprValue);
      Value := InternalInterpreter.PyInteractiveInterpreter.saferepr(ExprValue);
    except
      // fail quitely
    end;
  end;
end;

procedure TPyInternalDebugger.ExitPostMortem;
begin
  GI_PyInterpreter.SetPyInterpreterPrompt(pipNormal);
  GI_PyInterpreter.AppendPrompt;

  FMainThread.Status := thrdRunning;
  FMainThread.CallStack.Clear;
  TPyBaseDebugger.ThreadChangeNotify(FMainThread, tctStatusChange);
  MakeFrameActive(nil);
  GI_PyControl.DebuggerState := dsInactive;
end;

procedure TPyInternalDebugger.GetCallStack(CallStackList: TObjectList<TBaseFrameInfo>;
  Frame, Botframe: Variant);
begin
  CallStackList.Clear;
  if VarIsPython(Frame) then begin
    while not VarIsNone(Frame.f_back) and not VarIsNone(Frame.f_back.f_back) do begin
      CallStackList.Add(TFrameInfo.Create(Frame));
      Frame := Frame.f_back;
      if VarIsSame(Frame, Botframe) then Break;
    end;
    if CallStackList.Count = 0  then
      CallStackList.Add(TFrameInfo.Create(Frame));
  end;
end;

function TPyInternalDebugger.GetFrameGlobals(Frame: TBaseFrameInfo): TBaseNameSpaceItem;
begin
  Result := nil;
  if not (GI_PyControl.DebuggerState in [dsPaused, dsPostMortem]) then
    Exit;
  Result := TNameSpaceItem.Create('globals', (Frame as TFrameInfo).FPyFrame.f_globals);
end;

function TPyInternalDebugger.GetFrameLocals(Frame: TBaseFrameInfo): TBaseNameSpaceItem;
begin
  Result := nil;
  if not (GI_PyControl.DebuggerState in [dsPaused, dsPostMortem]) then
    Exit;
  Result := TNameSpaceItem.Create('locals', (Frame as TFrameInfo).FPyFrame.f_locals);
end;

function TPyInternalDebugger.GetPostMortemEnabled: Boolean;
begin
  Result := InternalInterpreter.CanDoPostMortem;
end;

function TPyInternalDebugger.HaveTraceback: Boolean;
var
  Py: IPyEngineAndGIL;
begin
  try
    Py := SafePyEngine;
    Result := VarModuleHasObject(SysModule, 'last_traceback');
  except
    Result := False;
  end;
end;

procedure TPyInternalDebugger.Pause;
begin
  FDebuggerCommand := dcPause;
  if GI_PyControl.DebuggerState = dsPaused then FDebugEvent.SetEvent;
end;

// Timer callback function
procedure TPyInternalDebugger.RestoreCommandLine;
begin
  InternalInterpreter.RestoreCommandLine;
end;

procedure TPyInternalDebugger.Resume;
begin
  FDebuggerCommand := dcRun;
  if GI_PyControl.DebuggerState = dsPaused then FDebugEvent.SetEvent;
end;

procedure TPyInternalDebugger.Debug(ARunConfig: TRunConfiguration;
  InitStepIn: Boolean = False; RunToCursorLine: Integer = -1);
var
  Code: Variant;
  Path, OldPath: string;
begin
  InternalInterpreter.CanDoPostMortem := False;

  // Repeat here to make sure it is set right
  MaskFPUExceptions(PyIDEOptions.MaskFPUExceptions);

  FDebuggerCommand := dcRun;
  Assert(GI_PyControl.DebuggerState = dsInactive, 'TPyInternalDebugger.Debug');

  //Compile
  Code := InternalInterpreter.Compile(ARunConfig);

  if VarIsPython(Code) then
  begin
    InternalInterpreter.SysPathRemove('');

    // Set the Working directory
    if ARunConfig.WorkingDir <> '' then
      Path := GI_PyIDEServices.ReplaceParams(ARunConfig.WorkingDir);
    if Path.Length <= 1 then
      Path := InternalInterpreter.SystemTempFolder;
    OldPath := GetCurrentDir;

    // Change the current path
    try
      SetCurrentDir(Path);
    except
      StyledMessageDlg(_(SCouldNotSetDirectory), mtWarning, [mbOK], 0);
    end;

    GI_PyControl.DebuggerState := dsDebugging;
    TPyBaseDebugger.ThreadChangeNotify(FMainThread, tctAdded );

    GI_MessagesService.ClearMessages;
    // Set the layout to the Debug layout is it exists
    if GI_PyIDEServices.Layouts.LayoutExists('Debug') then begin
      GI_PyIDEServices.Layouts.SaveLayout('Current');
      GI_PyIDEServices.Layouts.LoadLayout('Debug');
      Application.ProcessMessages;
    end else
      GI_PyInterpreter.ShowWindow(not Assigned(GI_ActiveEditor));

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
    SetDebuggerBreakpoints;

    ThreadPythonExec(procedure
    var
      PythonPathAdder: IInterface;
    begin
      // Add the path of the script to the Python Path - Will be automatically removed
      var Path := InternalInterpreter.ToPythonFileName(ARunConfig.ScriptName);
      if Path.StartsWith('<') then
        Path := ''
      else
        Path := TPath.GetDirectoryName(Path);
      if Length(Path) > 1 then
        PythonPathAdder := InternalInterpreter.AddPathToPythonPath(Path);

      if RunToCursorLine >= 0 then  // add temp breakpoint
        InternalInterpreter.Debugger.set_break(Code.co_filename, RunToCursorLine, True);

      InternalInterpreter.Debugger.InitStepIn := InitStepIn;
      try
        InternalInterpreter.Debugger.run(Code);
      except
        // CheckError already called by VarPyth
        on E: EPythonError do begin
          TThread.Synchronize(nil, procedure
          begin
            InternalInterpreter.HandlePyException(GetPythonEngine.Traceback, E.Message, 2);
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

      GI_PyControl.DebuggerState := dsInactive;
      if InternalInterpreter.CanDoPostMortem and PyIDEOptions.PostMortemOnException then
        EnterPostMortem;
    end);
  end;
end;

function TPyInternalDebugger.RunSource(const Source, FileName: string; const Symbol: string = 'single'): Boolean;
// The internal interpreter RunSource calls II.runsource which differs
// according to whether we debugging or not
begin
  Result := InternalInterpreter.RunSource(Source, FileName, Symbol);
end;

procedure TPyInternalDebugger.RunToCursor(Editor: IEditor; ALine: Integer);
var
  Py: IPyEngineAndGIL;
  FName: string;
begin
  Assert(GI_PyControl.DebuggerState = dsPaused, 'TPyInternalDebugger.RunToCursor');
  // Set Temporary breakpoint
  if GI_BreakpointManager.BreakPointsChanged then
    SetDebuggerBreakpoints;  // So that this one is not cleared
  FName := InternalInterpreter.ToPythonFileName(Editor.FileId);
  Py := SafePyEngine;
  InternalInterpreter.Debugger.set_break(FName, ALine, True);

  FDebuggerCommand := dcRunToCursor;
  if GI_PyControl.DebuggerState = dsPaused then FDebugEvent.SetEvent;
end;

procedure TPyInternalDebugger.StepInto;
begin
  FDebuggerCommand := dcStepInto;
  if GI_PyControl.DebuggerState = dsPaused then FDebugEvent.SetEvent;
end;

procedure TPyInternalDebugger.StepOver;
begin
  FDebuggerCommand := dcStepOver;
  if GI_PyControl.DebuggerState = dsPaused then FDebugEvent.SetEvent;
end;

procedure TPyInternalDebugger.StepOut;
begin
  FDebuggerCommand := dcStepOut;
  if GI_PyControl.DebuggerState = dsPaused then FDebugEvent.SetEvent;
end;

procedure TPyInternalDebugger.Abort;
begin
  case GI_PyControl.DebuggerState of
    dsPostMortem: ExitPostMortem;
    dsDebugging: FDebuggerCommand := dcAbort;
    //dsRunning: GetPythonEngine.PyErr_SetInterrupt;  //Generate Keyboard interrupt signal
    dsPaused:
      begin
        FDebuggerCommand := dcAbort;
        FDebugEvent.SetEvent;
      end;
  end;
end;

procedure TPyInternalDebugger.UserCall(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
begin
   Result := GetPythonEngine.ReturnNone;
end;

procedure TPyInternalDebugger.UserException(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
   Result := GetPythonEngine.ReturnNone;
end;

procedure TPyInternalDebugger.UserLine(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
var
  Frame: Variant;
  FName: string;
  LineNo: Integer;
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
    if GI_PyControl.DebuggerState = dsDebugging then
      GI_PyControl.DebuggerState := dsPaused;
    FMainThread.Status := thrdBroken;
    GetCallStack(FMainThread.CallStack, Frame, InternalInterpreter.Debugger.botframe);
    TPythonThread.Py_Begin_Allow_Threads;
    TThread.Synchronize(nil, procedure
    begin
      TPyBaseDebugger.ThreadChangeNotify(FMainThread, tctStatusChange);
    end);
    TPythonThread.Py_End_Allow_Threads;

    FDebuggerCommand := dcNone;

    TPythonThread.Py_Begin_Allow_Threads;
    // Wait for a debugger command
    FDebugEvent.WaitFor(INFINITE);
    TPythonThread.Py_End_Allow_Threads;

    GI_PyControl.DebuggerState := dsDebugging;

    if GI_BreakpointManager.BreakPointsChanged then
      SetDebuggerBreakpoints;

    FMainThread.Status := thrdRunning;

    TPythonThread.Py_Begin_Allow_Threads;
    TThread.Synchronize(nil, procedure
    begin
      TPyBaseDebugger.ThreadChangeNotify(FMainThread, tctStatusChange);
      FMainThread.CallStack.Clear;
    end);
    TPythonThread.Py_End_Allow_Threads;
  end;

  case FDebuggerCommand of
    dcRun        : InternalInterpreter.Debugger.set_continue();
    dcStepInto   : InternalInterpreter.Debugger.set_step();
    dcStepOver   : InternalInterpreter.Debugger.set_next(Frame);
    dcStepOut    : InternalInterpreter.Debugger.set_return(Frame);
    dcRunToCursor: InternalInterpreter.Debugger.set_continue;
    dcPause      : InternalInterpreter.Debugger.set_step();
    dcAbort      : InternalInterpreter.Debugger.set_quit();
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
  if FDebuggerCommand = dcAbort then begin
    InternalInterpreter.Debugger.set_quit();
    TThread.Synchronize(nil, procedure
    begin
      GI_MessagesService.AddMessage(_(SDebuggingAborted));
      GI_MessagesService.ShowWindow;
    end);
  end else if FDebuggerCommand = dcPause then
    InternalInterpreter.Debugger.set_step();
  Result := GetPythonEngine.ReturnNone;
end;

procedure TPyInternalDebugger.SetCommandLine(ARunConfig: TRunConfiguration);
begin
  InternalInterpreter.SetCommandLine(ARunConfig);
end;

procedure TPyInternalDebugger.SetDebuggerBreakpoints;
var
  Py: IPyEngineAndGIL;
begin
  Py := SafePyEngine;
  LoadLineCache;
  InternalInterpreter.Debugger.clear_all_breaks();

  for var BPInfo in GI_BreakpointManager.AllBreakPoints do
    if not BPInfo.Disabled then
      InternalInterpreter.Debugger.set_break(
        InternalInterpreter.ToPythonFileName(BPInfo.FileName),
        BPInfo.LineNo, False, BPInfo.Condition, BPInfo.IgnoreCount);

  GI_BreakpointManager.BreakPointsChanged := False;
end;

procedure TPyInternalDebugger.LoadLineCache;
begin
  // Inject unsaved code into LineCache
  FLineCache.cache.clear();
  GI_EditorFactory.ApplyToEditors(procedure(Editor: IEditor)
  var
    LineList: Variant;
    Source, SFName: string;
  begin
    with Editor do begin
      if not HasPythonFile then Exit;
      SFName := InternalInterpreter.ToPythonFileName(FileId);
      if SFName.StartsWith('<') then
      begin
        Source := CleanEOLs(SynEdit.Text)+WideLF;
        LineList := VarPythonCreate(Source);
        LineList := LineList.splitlines(True);
        FLineCache.cache.SetItem(SFName,
          VarPythonCreate([Length(Source), None, LineList, SFName], stTuple));
      end;
    end;
  end);
end;

procedure TPyInternalDebugger.MakeFrameActive(Frame: TBaseFrameInfo);
var
  Py: IPyEngineAndGIL;
begin
  Py := SafePyEngine;
  if Assigned(Frame) then
    InternalInterpreter.Debugger.currentframe := (Frame as TFrameInfo).FPyFrame
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
  FEngineType := peInternal;
  FII := II;
  FDebugger := II.debugger;

  FPythonVersion := SysModule.version;
  FPythonPlatform := SysModule.platform;

  // sys.displayhook
  if PyIDEOptions.PrettyPrintOutput then
    FII.setupdisplayhook()
end;

function TPyInternalInterpreter.CreateDebugger: TPyBaseDebugger;
begin
  Result := TPyInternalDebugger.Create;
end;

function TPyInternalInterpreter.EvalCode(const Expr: string): Variant;
begin
  // may raise exceptions
  Result := FII.evalcode(Expr);
end;

function TPyInternalInterpreter.CallTipFromExpression(const Expr: string;
  var DisplayString, DocString: string): Boolean;
var
  Py: IPyEngineAndGIL;
  LookupObj, ArgText, PyDocString: Variant;
begin
  Result := False;
  DisplayString := '';
  DocString := '';
  if Expr = '' then Exit;

  Py := SafePyEngine;
  var SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
  try
    //Evaluate the lookup expression and get the hint text
    LookupObj := FII.evalcode(Expr);
    ArgText := FII.get_arg_text(LookupObj);
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
var
  Py: IPyEngineAndGIL;
  co: PPyObject;
  FName, Source: AnsiString;
  Editor: IEditor;
begin
  if GI_PyControl.DebuggerState <> dsInactive then begin
    StyledMessageDlg(_(SCannotCompileWhileRunning),
      mtError, [mbAbort], 0);
    System.SysUtils.Abort;
  end;

  Py := SafePyEngine;

  VarClear(Result);
  GI_PyControl.ErrorPos := TEditorPos.EmptyPos;

  GI_MessagesService.ClearMessages;

  Editor := GI_EditorFactory.GetEditorByFileId(ARunConfig.ScriptName);
  if Assigned(Editor) then begin
    Source := CleanEOLs(Editor.EncodedText) + #10;
  end else
    try
      Source := CleanEOLs(FileToEncodedStr(ARunConfig.ScriptName)) + #10;
    except
      on E: Exception do begin
        StyledMessageDlg(Format(_(SFileOpenError), [ARunConfig.ScriptName, E.Message]), mtError, [mbOK], 0);
        System.SysUtils.Abort;
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
      on E: EPySyntaxError do
        begin
          var FileName := FromPythonFileName(E.EFileName);
          GI_PyControl.ErrorPos := TEditorPos.New(FileName, E.ELineNumber, E.EOffset, True);

          GI_PyInterpreter.AppendPrompt;
          System.SysUtils.Abort;
        end;
      on E: EPythonError do
        begin  //may raise OverflowError or ValueError
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

function TPyInternalInterpreter.GetObjectType(Obj: Variant): string;
begin
  var SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
  try
    Result := FII.objecttype(Obj);
  except
    Result := 'Unknown type';
  end;
end;

function TPyInternalInterpreter.ImportModule(Editor: IEditor;
  AddToNameSpace: Boolean = False): Variant;
{
  Imports Editor text without saving the file.
  Does not add the module name to the locals()
  of the interpreter.
}
var
  Py: IPyEngineAndGIL;
  Code: Variant;
  Path, NameOfModule: string;
  PythonPathAdder: IInterface;
  RunConfiguration: TRunConfiguration;
begin
  Py := SafePyEngine;
  Assert(Assigned(Editor), 'TPyInternalInterpreter.ImportModule');
  VarClear(Result);
  //Compile                           ,
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

  GI_PyControl.DebuggerState := dsRunning;
  try
    try
      Result := FII._import(NameOfModule, Code);
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

    GI_PyControl.DebuggerState := dsInactive;
  end;
end;

function TPyInternalInterpreter.NameSpaceFromExpression(
  const Expr: string): TBaseNameSpaceItem;
var
  InspectModule, LookupObj, ItemsDict: Variant;
begin
  var SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
  InspectModule := Import('inspect');
  try
    if Expr <> '' then begin
      //Evaluate the lookup expression and get the hint text
      LookupObj := FII.evalcode(Expr);
      Result := TNameSpaceItem.Create(Expr, LookupObj);
    end else begin
      ItemsDict := NewPythonDict;
      ItemsDict.update(BuiltinModule.__dict__);
      ItemsDict.update(FII.evalcode('vars()'));
      Result := TNameSpaceItem.Create(Expr, ItemsDict);
    end;
  except
    Result := nil;
  end;
end;

function TPyInternalInterpreter.NameSpaceItemFromPyObject(AName: string;
  APyObject: Variant): TBaseNameSpaceItem;
begin
  Result := TNameSpaceItem.Create(AName, APyObject);
end;

procedure TPyInternalInterpreter.RestoreCommandLine;
var
  Py: IPyEngineAndGIL;
begin
  Py := SafePyEngine;
  SysModule.argv := FOldargv;
end;

procedure TPyInternalInterpreter.SetCommandLine(ARunConfig: TRunConfiguration);
var
  Py: IPyEngineAndGIL;
  SysMod: Variant;
  Params, Param: string;
  P: PChar;
begin
  Py := SafePyEngine;
  SysMod := SysModule;
  FOldargv := SysMod.argv;
  SysMod.argv := NewPythonList;
  var ScriptName := ToPythonFileName(ARunConfig.ScriptName);
  SysMod.argv.append(ScriptName);

  Params := Trim(GI_PyIDEServices.ReplaceParams(ARunConfig.Parameters));
  if Params <> '' then begin
    Params := GI_PyIDEServices.ReplaceParams(Params);
    P := PChar(Params);
    while P[0] <> #0 do begin
      P := GetParamStr(P, Param);
      SysMod.argv.append(Param);
    end;
    GI_PyInterpreter.AppendText(Format(_(SCommandLineMsg), [Params]) + SLineBreak);
  end;
end;

procedure TPyInternalInterpreter.StringsToSysPath(Strings: TStrings);
var
  Py: IPyEngineAndGIL;
  PythonPath: Variant;
begin
  Py := SafePyEngine;
  PythonPath := NewPythonList;
  for var I := 0 to Strings.Count - 1 do
    PythonPath.append(Strings[I]);
  SysModule.path := PythonPath;
end;

procedure TimeCallBack(TimerID, Msg: UINT; dwUser, dw1, dw2: DWORD_PTR); stdcall;
begin
  if PUINT(dwUser)^ = 0 then Exit;
  if (MessageBoxW(0, PWideChar(_(SInterruptRunningScript)),
    'ScriptTimeOut', MB_ICONWARNING or MB_YESNO) = idYes) then
  begin
    GetPythonEngine.PyErr_SetInterrupt;  //Generate Keyboard interrupt signal
    timeKillEvent(PUINT(dwUser)^);
    PUINT(dwUser)^ := 0;
  end;
end;

function TPyInternalInterpreter.GetInterpreter: Variant;
begin
  Result := FII;
end;

procedure TPyInternalInterpreter.Pickle(AValue: Variant; const FileName: string);
var
  f: Variant;
  Py: IPyEngineAndGIL;
begin
  Py := SafePyEngine;
  var SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
  try
    f := BuiltinModule.open(Filename, 'wb');
  except
    raise Exception.CreateFmt(SCouldNotOpenOutputFile, [FileName]);
  end;

  try
    Import('pickle').dump(AValue, f);
  except
    f.close();
    raise Exception.Create(SPickleFailed);
  end;
  f.close();
end;

procedure TPyInternalInterpreter.Run(ARunConfig: TRunConfiguration);
var
  Py: IPyEngineAndGIL;
  Code: Variant;
  mmResult, Resolution: UINT;
  TimeCaps: TTimeCaps;
  Path, OldPath: string;
  PythonPathAdder: IInterface;
begin
  CanDoPostMortem := False;

  // Repeat here to make sure it is set right
  MaskFPUExceptions(PyIDEOptions.MaskFPUExceptions);

  //Compile
  Code := Compile(ARunConfig);

  if VarIsPython(Code) then begin
    GI_PyControl.DebuggerState := dsRunning;

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
      SetCurrentDir(Path);
    except
      StyledMessageDlg(_(SCouldNotSetDirectory), mtWarning, [mbOK], 0);
    end;

    // Set the command line parameters
    SetCommandLine(ARunConfig);

    GI_PyInterpreter.ShowWindow(not Assigned(GI_ActiveEditor));

    try
      // Set Multimedia Timer
      if (PyIDEOptions.TimeOut > 0) and
        (timeGetDevCaps(@TimeCaps, SizeOf(TimeCaps)) = TIMERR_NOERROR) then
      begin
        Resolution := Min(Resolution, TimeCaps.wPeriodMax);
        timeBeginPeriod(Resolution);
        mmResult := timeSetEvent(PyIDEOptions.TimeOut, Resolution,
          @TimeCallBack, DWORD_PTR(@mmResult), TIME_PERIODIC or 256);
      end;
      Py := SafePyEngine;
      try
        try
          FII.run_nodebug(Code);
        finally
          VarClear(Code);
        end;
      except
        // CheckError already called by VarPyth
        on E: EPythonError do
        begin
          HandlePyException(Py.PythonEngine.Traceback, E.Message);
          CanDoPostMortem := True;
          System.SysUtils.Abort;
        end;
      end;
    finally
      if PyIDEOptions.TimeOut > 0 then begin
        if (mmResult <> 0) then timeKillEvent(mmResult);
        timeEndPeriod(Resolution);
      end;
      GI_PyInterpreter.AppendPrompt;

      // Restore the command line parameters
      RestoreCommandLine;

      //  Add again the empty path
      SysPathAdd('');

      // Change the back current path
      SetCurrentDir(OldPath);

      GI_PyControl.DebuggerState := dsInactive;
      if CanDoPostMortem and PyIDEOptions.PostMortemOnException then
        PyControl.ActiveDebugger.EnterPostMortem;
    end;
  end;
end;

function TPyInternalInterpreter.RunSource(const Source, FileName: string; const Symbol: string = 'single'): Boolean;
var
  Py: IPyEngineAndGIL;
  OldDebuggerState: TDebuggerState;
begin
  Assert(not GI_PyControl.Running, 'RunSource called while the Python engine is active');

  OldDebuggerState := GI_PyControl.DebuggerState;
  GI_PyControl.DebuggerState := dsRunning;
  try
    Py := SafePyEngine;
    Result := FII.runsource(Source, FileName, Symbol);
  finally
    GI_PyControl.DebuggerState := OldDebuggerState;
  end;
end;

function TPyInternalInterpreter.SyntaxCheck(Editor: IEditor; out ErrorPos: TEditorPos; Quiet: Boolean = False): Boolean;
var
  Py: IPyEngineAndGIL;
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

  Py :=  SafePyEngine;
  with Py.PythonEngine do begin
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
        ErrorPos := TEditorPos.New(Editor.FileId, E.ELineNumber, E.EOffset, True);

        if not Quiet then
        begin
          GI_MessagesService.ClearMessages;
          // New Line for output
          GI_PyInterpreter.AppendPrompt;
          GI_PyControl.ErrorPos := ErrorPos;
        end;
      end;
    end;
  end;
end;

function TPyInternalInterpreter.SysPathAdd(const Path: string): Boolean;
var
  Py: IPyEngineAndGIL;
  SysPath: Variant;
begin
  Py := SafePyEngine;

  SysPath := SysModule.path;
  if SysPath.__contains__(Path) then
    Result := False
  else
  begin
    if (SysPath.__len__() > 0) and (SysPath.__getitem__(0) = '') then
      SysPath.insert(1, Path)
    else
      SysPath.insert(0, Path);
    Result := True;
  end;
end;

function TPyInternalInterpreter.SysPathRemove(const Path: string): Boolean;
var
  Py: IPyEngineAndGIL;
begin
  Py := SafePyEngine;
  if SysModule.path.__contains__(Path) then begin
    Result := True;
    SysModule.path.remove(Path);
  end else
    Result := False;
end;

procedure TPyInternalInterpreter.SysPathToStrings(Strings: TStrings);
var
  Py: IPyEngineAndGIL;
begin
  Py := SafePyEngine;
  for var I := 0 to len(SysModule.path) - 1  do
    Strings.Add(SysModule.path.__getitem__(I));
end;

procedure TPyInternalInterpreter.SystemCommand(const Cmd: string);
var
  Py: IPyEngineAndGIL;
begin
  Py := SafePyEngine;
  FII.system_command(Cmd);
end;

function TPyInternalInterpreter.UnitTestResult: Variant;
begin
  Result := PyInteractiveInterpreter.IDETestResult();
end;

end.






