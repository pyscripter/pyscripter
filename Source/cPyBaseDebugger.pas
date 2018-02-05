{-----------------------------------------------------------------------------
 Unit Name: cPyBaseDebugger
 Author:    Kiriakos Vlahos
 Date:      23-Apr-2006
 Purpose:
 History:   Base debugger classes
-----------------------------------------------------------------------------}
unit cPyBaseDebugger;

interface
uses
  Windows, SysUtils, Classes, uEditAppIntfs, Forms,
  Contnrs, cTools, cPythonSourceScanner, PythonEngine;

type
  TPythonEngineType = (peInternal, peRemote, peRemoteTk, peRemoteWx);
  TDebuggerState = (dsInactive, dsDebugging, dsPaused, dsRunning, dsPostMortem);
  TDebuggerCommand = (dcNone, dcRun, dcStepInto, dcStepOver, dcStepOut,
                      dcRunToCursor, dcPause, dcAbort);

  TDebuggerLineInfo = (dlCurrentLine,
                       dlBreakpointLine,
                       dlDisabledBreakpointLine,
                       dlExecutableLine,
                       dlErrorLine);
  TDebuggerLineInfos = set of TDebuggerLineInfo;

  TInterpreterCapability = (icReInitialize);
  TInterpreterCapabilities = set of TInterpreterCapability;

  TNamespaceItemAttribute = (nsaNew, nsaChanged);
  TNamespaceItemAttributes = set of TNamespaceItemAttribute;

  TBreakpointChangeEvent = procedure(Sender: TObject; Editor : IEditor; ALine: integer) of object;
  TDebuggerStateChangeEvent = procedure(Sender: TObject;
    OldState, NewState: TDebuggerState) of object;
  TDebuggerYieldEvent = procedure(Sender: TObject; DoIdle : Boolean) of object;

  TRunConfiguration = class(TPersistent)
  private
    fScriptName: string;
    fEngineType: TPythonEngineType;
    fWorkingDir: string;
    fParameters: string;
    fReinitializeBeforeRun: Boolean;
    fOutputFileName: string;
    fWriteOutputToFile: Boolean;
    fAppendToFile: Boolean;
    fExternalRun: TExternalRun;
    fDescription: string;
    procedure SetExternalRun(const Value: TExternalRun);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ScriptName : string read fScriptName write fScriptName;
    property Description : string read fDescription write fDescription;
    property EngineType : TPythonEngineType read fEngineType write fEngineType;
    property ReinitializeBeforeRun : Boolean read fReinitializeBeforeRun
      write fReinitializeBeforeRun;
    property Parameters : string read fParameters write fParameters;
    property WorkingDir : string read fWorkingDir write fWorkingDir;
    property WriteOutputToFile : Boolean read fWriteOutputToFile
      write fWriteOutputToFile;
    property OutputFileName : string read fOutputFileName write fOutputFileName;
    property AppendToFile : Boolean read fAppendToFile write fAppendToFile;
    property ExternalRun : TExternalRun read fExternalRun write SetExternalRun;
  end;

  TEditorPos = class(TPersistent)
  public
    Editor : IEditor;
    Line : integer;
    Char : integer;
    IsSyntax : Boolean;
    ErrorMsg : string;
    procedure Clear;
    procedure Assign(Source: TPersistent); override;
  end;

  TBaseFrameInfo = class(TObject)
  // Base (abstract) class for Call Stack frame information
  protected
    function GetFunctionName : string; virtual; abstract;
    function GetFileName : string; virtual; abstract;
    function GetLine : integer; virtual; abstract;
  public
    property FunctionName : string read GetFunctionName;
    property FileName : string read GetFileName;
    property Line : integer read GetLine;
  end;

  TBaseNameSpaceItem = class(TObject)
  // Base (abstract) class for Namespace item information
  protected
    fPyObject : Variant;
    fExpandCommonTypes : Boolean;
    fExpandSequences : Boolean;
    GotChildNodes : Boolean;
    GotBufferedValue : Boolean;
    BufferedValue : string;
    function GetOrCalculateValue : string;
    function GetName : string; virtual; abstract;
    function GetObjectType : string; virtual; abstract;
    function GetValue : string; virtual; abstract;
    function GetDocString : string; virtual; abstract;
    function GetChildCount : integer; virtual; abstract;
    function GetChildNode(Index: integer): TBaseNameSpaceItem; virtual; abstract;
  public
    Attributes : TNamespaceItemAttributes;
    function IsClass : Boolean; virtual; abstract;
    function IsDict : Boolean; virtual; abstract;
    function IsModule : Boolean; virtual; abstract;
    function IsFunction : Boolean; virtual; abstract;
    function IsMethod : Boolean; virtual; abstract;
    function Has__dict__ : Boolean; virtual; abstract;
    function IndexOfChild(AName : string): integer; virtual; abstract;
    procedure GetChildNodes; virtual; abstract;
    procedure CompareToOldItem(OldItem : TBaseNameSpaceItem); virtual;
    property Name : string read GetName;
    property ObjectType : string read GetObjectType;
    property Value : string read GetOrCalculateValue;
    property DocString : string read GetDocString;
    property ChildCount : integer read GetChildCount;
    property ChildNode[Index : integer] : TBaseNameSpaceItem
      read GetChildNode;
    property PyObject : Variant read fPyObject;
    property ExpandCommonTypes : Boolean read fExpandCommonTypes write fExpandCommonTypes;
    property ExpandSequences : Boolean read fExpandSequences write fExpandSequences;
  end;

  TModuleProxy = class;

  TPyBaseInterpreter = class(TObject)
  //  Base (abstract) class for implementing Python Interpreters
  private
    function GetMainModule: TModuleProxy;
  protected
    fInterpreterCapabilities : TInterpreterCapabilities;
    fEngineType : TPythonEngineType;
    fMainModule : TModuleProxy;
    procedure CreateMainModule; virtual; abstract;
  public
    destructor Destroy; override;
    procedure Initialize; virtual;
    // Python Path
    function SysPathAdd(const Path : string) : boolean; virtual; abstract;
    function SysPathRemove(const Path : string) : boolean; virtual; abstract;
    function AddPathToPythonPath(const Path : string; AutoRemove : Boolean = True) : IInterface;
    procedure SysPathToStrings(Strings : TStrings); virtual; abstract;
    procedure StringsToSysPath(Strings : TStrings); virtual; abstract;
    // NameSpace
    function GetGlobals : TBaseNameSpaceItem; virtual; abstract;
    procedure GetModulesOnPath(Path : Variant; SL : TStrings); virtual; abstract;
    function NameSpaceFromExpression(const Expr : string) : TBaseNameSpaceItem; virtual; abstract;
    function CallTipFromExpression(const Expr : string;
      var DisplayString, DocString : string) : Boolean; virtual; abstract;
    // Service routines
    procedure HandlePyException(E : EPythonError; SkipFrames : integer = 1); virtual;
    procedure SetCommandLine(ARunConfig : TRunConfiguration); virtual; abstract;
    procedure RestoreCommandLine; virtual; abstract;
    procedure ReInitialize; virtual;
    // Main interface
    function ImportModule(Editor : IEditor; AddToNameSpace : Boolean = False) : Variant; virtual; abstract;
    procedure Run(ARunConfig : TRunConfiguration); virtual; abstract;
    function RunSource(Const Source, FileName : Variant; symbol : string = 'single') : boolean; virtual; abstract;
    procedure RunScript(FileName : string); virtual;
    function EvalCode(const Expr : string) : Variant; virtual; abstract;
    function GetObjectType(Ob : Variant) : string; virtual; abstract;
    function UnitTestResult : Variant; virtual; abstract;
    function NameSpaceItemFromPyObject(aName : string; aPyObject : Variant): TBaseNameSpaceItem; virtual; abstract;
    property EngineType : TPythonEngineType read fEngineType;
    property InterpreterCapabilities : TInterpreterCapabilities read fInterpreterCapabilities;
    property MainModule : TModuleProxy read GetMainModule;
  end;

  TPyBaseDebugger = class(TObject)
  {  Base (abstract) class for implementing Python Debuggers }
  protected
    procedure SetCommandLine(ARunConfig : TRunConfiguration); virtual; abstract;
    procedure RestoreCommandLine; virtual; abstract;
    procedure SetDebuggerBreakpoints; virtual; abstract;
  public
    // Python Path
    function SysPathAdd(const Path : string) : boolean; virtual; abstract;
    function SysPathRemove(const Path : string) : boolean; virtual; abstract;
    function AddPathToPythonPath(const Path : string; AutoRemove : Boolean = True) : IInterface;
    // Debugging
    procedure Debug(ARunConfig : TRunConfiguration; InitStepIn : Boolean = False;
            RunToCursorLine : integer = -1); virtual; abstract;
    procedure RunToCursor(Editor : IEditor; ALine: integer); virtual; abstract;
    procedure StepInto; virtual; abstract;
    procedure StepOver; virtual; abstract;
    procedure StepOut; virtual; abstract;
    procedure Resume; virtual; abstract;
    procedure Pause; virtual; abstract;
    procedure Abort; virtual; abstract;
    // Evaluate expression in the current frame
    procedure Evaluate(const Expr : string; out ObjType, Value : string); overload; virtual; abstract;
    function Evaluate(const Expr : string) : TBaseNamespaceItem; overload; virtual; abstract;
    // Like the InteractiveInterpreter runsource but for the debugger frame
    function RunSource(Const Source, FileName : Variant; symbol : string = 'single') : boolean; virtual; abstract;
    // Fills in CallStackList with TBaseFrameInfo objects
    procedure GetCallStack(CallStackList : TObjectList); virtual; abstract;
    // functions to get TBaseNamespaceItems corresponding to a frame's gloabals and locals
    function GetFrameGlobals(Frame : TBaseFrameInfo) : TBaseNameSpaceItem; virtual; abstract;
    function GetFrameLocals(Frame : TBaseFrameInfo) : TBaseNameSpaceItem; virtual; abstract;
    function NameSpaceFromExpression(const Expr : string) : TBaseNameSpaceItem; virtual; abstract;
    procedure MakeFrameActive(Frame : TBaseFrameInfo); virtual; abstract;
    // post mortem stuff
    function HaveTraceback : boolean; virtual; abstract;
    procedure EnterPostMortem; virtual; abstract;
    procedure ExitPostMortem; virtual; abstract;
  end;

  TPythonControl = class(Tobject)
  {
    Interface between PyScripter and the Interpreter/Debugger.
    Holds information Breakpoints, ErrorPos, CurrentPos
  }
  private
    fBreakPointsChanged : Boolean;
    fDebuggerState: TDebuggerState;
    fErrorPos : TEditorPos;
    fCurrentPos : TEditorPos;
    fOnBreakpointChange: TBreakpointChangeEvent;
    fOnCurrentPosChange: TNotifyEvent;
    fOnErrorPosChange: TNotifyEvent;
    fOnStateChange: TDebuggerStateChangeEvent;
    fOnYield: TDebuggerYieldEvent;
    fActiveInterpreter : TPyBaseInterpreter;
    fActiveDebugger : TPyBaseDebugger ;
    fRunConfig : TRunConfiguration;
    FPythonVersionIndex: integer;
    procedure DoOnBreakpointChanged(Editor : IEditor; ALine: integer);
    procedure SetActiveDebugger(const Value: TPyBaseDebugger);
    procedure SetActiveInterpreter(const Value: TPyBaseInterpreter);
    function GetPythonEngineType: TPythonEngineType;
    procedure SetPythonEngineType(const Value: TPythonEngineType);
    procedure SetRunConfig(ARunConfig: TRunConfiguration);
    procedure PrepareRun;
  public
    // ActiveInterpreter and ActiveDebugger are created
    // and destroyed in frmPythonII

    constructor Create;
    destructor Destroy; override;
    // Breakpoint related
    procedure ToggleBreakpoint(Editor : IEditor; ALine: integer;
      CtrlPressed : Boolean = False);
    procedure SetBreakPoint(FileName : string; ALine : integer;
      Disabled : Boolean; Condition : string);
    procedure ClearAllBreakpoints;
    // Editor related
    function GetLineInfos(Editor : IEditor; ALine: integer): TDebuggerLineInfos;
    function IsBreakpointLine(Editor: IEditor; ALine: integer;
      var Disabled : boolean): boolean;
    function IsExecutableLine(Editor: IEditor; ALine: integer): boolean;
    // Event processing
    procedure DoCurrentPosChanged;
    procedure DoErrorPosChanged;
    procedure DoStateChange(NewState : TDebuggerState);
    procedure DoYield(DoIdle : Boolean);
    // Other
    function IsRunning: boolean;
    // Running Python Scripts
    procedure Run(ARunConfig : TRunConfiguration);
    procedure Debug(ARunConfig : TRunConfiguration;  InitStepIn : Boolean = False;
      RunToCursorLine : integer = -1);
    procedure ExternalRun(ARunConfig : TRunConfiguration);
    // properties and events
    // PythonVersionIndex is the Index of Python version in the PYTHON_KNOWN_VERSIONS array
    property PythonVersionIndex : integer read FPythonVersionIndex write FPythonVersionIndex;
    property PythonEngineType : TPythonEngineType read GetPythonEngineType
      write SetPythonEngineType;
    property ActiveInterpreter : TPyBaseInterpreter read fActiveInterpreter
      write SetActiveInterpreter;
    property ActiveDebugger : TPyBaseDebugger read fActiveDebugger
      write SetActiveDebugger;
    property BreakPointsChanged : Boolean read fBreakPointsChanged
      write fBreakPointsChanged;
    property DebuggerState : TDebuggerState read fDebuggerState;
    property ErrorPos: TEditorPos read fErrorPos;
    property CurrentPos: TEditorPos read fCurrentPos;
    property RunConfig : TRunConfiguration read fRunConfig;
    property OnBreakpointChange: TBreakpointChangeEvent read fOnBreakpointChange
      write fOnBreakpointChange;
    property OnCurrentPosChange: TNotifyEvent read fOnCurrentPosChange
      write fOnCurrentPosChange;
    property OnErrorPosChange: TNotifyEvent read fOnErrorPosChange
      write fOnErrorPosChange;
    property OnStateChange: TDebuggerStateChangeEvent read fOnStateChange
      write fOnStateChange;
    property OnYield: TDebuggerYieldEvent read fOnYield write fOnYield;
  end;

 TModuleProxy = class(TParsedModule)
  private
    fPyModule : Variant;
    fIsExpanded : boolean;
    fPyInterpreter: TPyBaseInterpreter;
  protected
    function GetAllExportsVar: string; override;
    function GetDocString: string; override;
    function GetCodeHint : string; override;
  public
    constructor CreateFromModule(AModule : Variant; aPyInterpreter : TPyBaseInterpreter);
    procedure Expand;
    procedure GetNameSpace(SList : TStringList); override;
    property PyModule : Variant read fPyModule;
    property IsExpanded : boolean read fIsExpanded;
    property Interpreter: TPyBaseInterpreter read fPyInterpreter;
  end;

  TClassProxy = class(TParsedClass)
  private
    fPyClass : Variant;
    fIsExpanded : boolean;
  protected
    function GetDocString: string; override;
  public
    constructor CreateFromClass(AName : string; AClass : Variant);
    function GetConstructor : TParsedFunction; override;
    procedure Expand;
    procedure GetNameSpace(SList : TStringList); override;
    property PyClass : Variant read fPyClass;
    property IsExpanded : boolean read fIsExpanded;
  end;

  TFunctionProxy = class(TParsedFunction)
  private
    fPyFunction : Variant;
    fIsExpanded : boolean;
  protected
    function GetDocString: string; override;
  public
    constructor CreateFromFunction(AName : string; AFunction : Variant);
    procedure Expand;
    function ArgumentsString : string; override;
    procedure GetNameSpace(SList : TStringList); override;
    property PyFunction : Variant read fPyFunction;
    property IsExpanded : boolean read fIsExpanded;
  end;

  TVariableProxy = class(TCodeElement)
  private
    fPyObject : Variant;
    fIsExpanded : boolean;
  protected
    function GetDocString: string; override;
    function GetCodeHint : string; override;
  public
    constructor CreateFromPyObject(const AName : string; AnObject : Variant);
    procedure Expand;
    procedure GetNameSpace(SList : TStringList); override;
    property PyObject : Variant read fPyObject;
    property IsExpanded : boolean read fIsExpanded;
  end;

Const
 CommonTypes: array[1..29] of TIdentMapEntry = (
      (Value: 0; Name: 'NoneType'),
      (Value: 1; Name: 'NotImplementedType'),
      (Value: 2; Name: 'bool'),
      (Value: 3; Name: 'buffer'),
      (Value: 4; Name: 'builtin_function_or_method'),
      (Value: 5; Name: 'code' ),
      (Value: 6; Name: 'complex'),
      (Value: 7; Name: 'dict'),
      (Value: 8; Name: 'dictproxy'),
      (Value: 9; Name: 'ellipsis'),
      (Value: 10; Name: 'file'),
      (Value: 11; Name: 'float'),
      (Value: 12; Name: 'frame'),
      (Value: 13; Name: 'function'),
      (Value: 14; Name: 'generator'),
      (Value: 15; Name: 'getset_descriptor'),
      (Value: 16; Name: 'instancemethod'),
      (Value: 17; Name: 'int'),
      (Value: 18; Name: 'list'),
      (Value: 19; Name: 'long'),
      (Value: 20; Name: 'member_descriptor'),
      (Value: 21; Name: 'method-wrapper'),
      (Value: 22; Name: 'object'),
      (Value: 23; Name: 'slice'),
      (Value: 24; Name: 'str'),
      (Value: 25; Name: 'traceback'),
      (Value: 26; Name: 'tuple'),
      (Value: 27; Name: 'unicode'),
      (Value: 28; Name: 'xrange')
      );

var
  PyControl : TPythonControl = nil;

const
   EngineInitFile = 'python_init.py';
   PyScripterInitFile = 'pyscripter_init.py';

implementation

uses
  System.UITypes, Dialogs,
  dmCommands, frmPythonII, frmMessages, frmPyIDEMain,
  uCommonFunctions, VarPyth,
  cParameters, StringResources, cPyDebugger,
  frmCommandOutput, JvGnuGettext,
  cPyScripterSettings;

{ TEditorPos }

procedure TEditorPos.Assign(Source: TPersistent);
begin
  if Source is TEditorPos then begin
    Self.Editor := TEditorPos(Source).Editor;
    Self.Line := TEditorPos(Source).Line;
    Self.Char := TEditorPos(Source).Char;
    Self.IsSyntax := TEditorPos(Source).IsSyntax;
    Self.ErrorMsg := TEditorPos(Source).ErrorMsg;
  end else
    inherited;
end;

procedure TEditorPos.Clear;
begin
  Editor := nil;
  Line := -1;
  Char := -1;
  IsSyntax := False;
  ErrorMsg := '';
end;

{ TPythonPathAdder }
type

  TSysPathFunction = function(const Path : string) : boolean of object;

  TPythonPathAdder = class(TInterfacedObject, IInterface)
  private
    fPath : string;
    fPathAdded : boolean;
    PackageRootAdder : IInterface;
    fAutoRemove : Boolean;
    fSysPathRemove : TSysPathFunction;
  public
    constructor Create(SysPathAdd, SysPathRemove : TSysPathFunction;
      const Path : string; AutoRemove : Boolean = True);
    destructor Destroy; override;
  end;

constructor TPythonPathAdder.Create(SysPathAdd, SysPathRemove : TSysPathFunction;
  const Path: string; AutoRemove : Boolean = True);
var
  S : string;
begin
  inherited Create;
  fPath := ExcludeTrailingPathDelimiter(Path);
  fAutoRemove := AutoRemove;
  fSysPathRemove := SysPathRemove;
  if (fPath <> '') and DirectoryExists(fPath) then begin
    // Add parent directory of the root of the package first
    if DirIsPythonPackage(fPath) then begin
      S := ExtractFileDir(GetPackageRootDir(fPath));
      if S <> fPath then
        PackageRootAdder :=
          TPythonPathAdder.Create(SysPathAdd, SysPathRemove, S, AutoRemove);
    end;
    fPathAdded := SysPathAdd(fPath);
  end;
end;

destructor TPythonPathAdder.Destroy;
begin
  PackageRootAdder := nil;  // will remove package root
  if fPathAdded and FAutoRemove then
    fSysPathRemove(fPath);
  inherited;
end;

{ TPyBaseInterpreter }

function TPyBaseInterpreter.AddPathToPythonPath(const Path: string;
  AutoRemove: Boolean): IInterface;
begin
  Result := TPythonPathAdder.Create(SysPathAdd, SysPathRemove, Path, AutoRemove);
end;

destructor TPyBaseInterpreter.Destroy;
begin
  FreeAndNil(fMainModule);
  inherited;
end;

function TPyBaseInterpreter.GetMainModule: TModuleProxy;
begin
  if not Assigned(fMainModule) then
    CreateMainModule;
  Result := fMainModule;
end;

procedure TPyBaseInterpreter.HandlePyException(E: EPythonError; SkipFrames : integer = 1);
Var
  TI : TTracebackItem;
  FileName : string;
  Editor : IEditor;
begin
  MessagesWindow.ShowPythonTraceback(SkipFrames);
  MessagesWindow.AddMessage(E.Message);
  with GetPythonEngine.Traceback do begin
    if ItemCount > 0 then begin
      TI := Items[ItemCount -1];
      FileName := TI.FileName;
      if (FileName[1] ='<') and (FileName[Length(FileName)] = '>') then
        FileName :=  Copy(FileName, 2, Length(FileName)-2);
      Editor := GI_EditorFactory.GetEditorByNameOrTitle(FileName);
      // Check whether the error occurred in the active editor
      if (Assigned(Editor) and (Editor = PyIDEMainForm.GetActiveEditor)) or
        PyIDEOptions.JumpToErrorOnException then
      begin
        if PyIDEMainForm.ShowFilePosition(TI.FileName, TI.LineNo, 1) and
          Assigned(GI_ActiveEditor)
        then begin
          PyControl.ErrorPos.Editor := GI_ActiveEditor;
          PyControl.ErrorPos.Line := TI.LineNo;
          PyControl.DoErrorPosChanged;
        end;
      end;
    end;
  end;
end;

procedure TPyBaseInterpreter.Initialize;
// Execute python_init.py
Var
  FileName : String;
begin
  FileName := CommandsDataModule.UserDataPath + EngineInitFile;

  try
    RunScript(FileName);
  except
    on E: Exception do
      Dialogs.MessageDlg(Format(_(SErrorInitScript),
        [EngineInitFile, E.Message]), mtError, [mbOK], 0);
  end;
end;

procedure TPyBaseInterpreter.ReInitialize;
begin
  raise Exception.Create(_(SNotImplented));
end;

procedure TPyBaseInterpreter.RunScript(FileName: string);
Var
  Source : string;
  AnsiSource : AnsiString;
begin
  // Execute pyscripterEngineSetup.py
  if FileExists(FileName) then begin
    if GetPythonEngine.IsPython3000 then begin
      Source := CleanEOLs(FileToStr(FileName))+#10;
      RunSource(Source, FileName, 'exec');
    end else begin
      AnsiSource := CleanEOLs(FileToEncodedStr(FileName))+#10;
      RunSource(AnsiSource, FileName, 'exec');
    end;
  end;
end;

{ TBaseNameSpaceItem }

procedure TBaseNameSpaceItem.CompareToOldItem(OldItem: TBaseNameSpaceItem);
var
  i, Index : integer;
  Child : TBaseNameSpaceItem;
begin
  if OldItem.GotBufferedValue then begin
    if OldItem.BufferedValue <> Value then
      Attributes := [nsaChanged];
  end;
  if OldItem.GotChildNodes then begin
    GetChildNodes;
    for i := 0 to ChildCount - 1 do begin
      Child := ChildNode[i];
      Index := OldItem.IndexOfChild(Child.Name);
      if Index >= 0 then
        Child.CompareToOldItem(OldItem.ChildNode[Index])
      else
        Child.Attributes := [nsaNew];
    end;
  end;
end;

function TBaseNameSpaceItem.GetOrCalculateValue: string;
begin
  if GotBufferedValue then
    Result := BufferedValue
  else begin
    BufferedValue := GetValue;
    GotBufferedValue := True;
    Result := BufferedValue;
  end;
end;

{ TPythonControl }

constructor TPythonControl.Create;
begin
  fDebuggerState := dsInactive;
  fCurrentPos := TEditorPos.Create;
  fCurrentPos.Clear;
  fErrorPos := TEditorPos.Create;
  fErrorPos.Clear;
  fRunConfig := TRunConfiguration.Create;
end;

procedure TPythonControl.Debug(ARunConfig: TRunConfiguration; InitStepIn : Boolean = False;
      RunToCursorLine : integer = -1);
begin
  SetRunConfig(ARunConfig);

  if not Assigned(ActiveDebugger) then Exit;

  PrepareRun;

  if fRunConfig.WriteOutputToFile then
    PythonIIForm.StartOutputMirror(Parameters.ReplaceInText(fRunConfig.OutputFileName),
      fRunConfig.AppendToFile);
  try
    ActiveDebugger.Debug(fRunConfig, InitStepIn, RunToCursorLine);
  finally
    if fRunConfig.WriteOutputToFile then
      PythonIIForm.StopFileMirror;
  end;
end;

destructor TPythonControl.Destroy;
begin
  fCurrentPos.Free;
  fErrorPos.Free;
  fRunConfig.Free;
  inherited;
end;

function TPythonControl.GetLineInfos(Editor : IEditor; ALine: integer): TDebuggerLineInfos;
Var
  Disabled : boolean;
begin
  Result := [];
  if ALine > 0 then begin
    if (Editor = PyControl.CurrentPos.Editor) and (ALine = PyControl.CurrentPos.Line) then
      Include(Result, dlCurrentLine);
    if (Editor = PyControl.ErrorPos.Editor) and (ALine = PyControl.ErrorPos.Line) then
      Include(Result, dlErrorLine);
    if IsExecutableLine(Editor, ALine) then
      Include(Result, dlExecutableLine);
    Disabled := False;
    if IsBreakpointLine(Editor, ALine, Disabled) then
      if Disabled then
        Include(Result, dlDisabledBreakpointLine)
      else
        Include(Result, dlBreakpointLine);
  end;
end;

function TPythonControl.GetPythonEngineType: TPythonEngineType;
begin
  if Assigned(ActiveInterpreter) then
    Result := ActiveInterpreter.EngineType
  else
    Result := peInternal;
end;

function TPythonControl.IsBreakpointLine(Editor: IEditor; ALine: integer;
  var Disabled : boolean): boolean;
Var
  i: integer;
begin
  Result := FALSE;
  if ALine > 0 then begin
    i := Editor.Breakpoints.Count - 1;
    while i >= 0 do begin
      if TBreakPoint(Editor.Breakpoints[i]).LineNo = ALine then begin
        Disabled := TBreakPoint(Editor.Breakpoints[i]).Disabled;
        Result := TRUE;
        break;
      end;
      Dec(i);
    end;
  end;
end;

function TPythonControl.IsExecutableLine(Editor: IEditor; ALine: integer): boolean;
begin
  Assert(Assigned(Editor));
  with Editor.SynEdit do begin
    Result := CommandsDataModule.IsExecutableLine(Lines[ALine-1]);
  end;
end;

procedure TPythonControl.ToggleBreakpoint(Editor : IEditor; ALine: integer;
  CtrlPressed : Boolean = False);
var
  Index : integer;
  i: integer;
  BreakPoint : TBreakPoint;
begin
  if ALine > 0 then begin
    Index := Editor.Breakpoints.Count;  // append at the end
    for i := 0 to Editor.Breakpoints.Count - 1 do begin
      if TBreakPoint(Editor.Breakpoints[i]).LineNo = ALine then begin
        if CtrlPressed then
          // Toggle disabled
          TBreakPoint(Editor.Breakpoints[i]).Disabled :=
            not TBreakPoint(Editor.Breakpoints[i]).Disabled
        else
          Editor.Breakpoints.Delete(i);
        Index := -1;
        break;
      end else if TBreakPoint(Editor.Breakpoints[i]).LineNo > ALine then begin
        Index := i;
        break;
      end;
    end;
    if Index >= 0 then begin
      BreakPoint := TBreakPoint.Create;
      BreakPoint.LineNo := ALine;
      if CtrlPressed then
        BreakPoint.Disabled := True;
      Editor.Breakpoints.Insert(Index, BreakPoint);
    end;
    DoOnBreakpointChanged(Editor, ALine);
  end;
end;

procedure TPythonControl.SetActiveDebugger(const Value: TPyBaseDebugger);
begin
  if fActiveDebugger <> Value then begin
    if Assigned(fActiveDebugger) then
      FreeAndNil(fActiveDebugger);
    fActiveDebugger := Value;
  end;
end;

procedure TPythonControl.SetActiveInterpreter(const Value: TPyBaseInterpreter);
begin
  if fActiveInterpreter <> Value then begin
    if Assigned(fActiveInterpreter) and
      (fActiveInterpreter <> InternalInterpreter)
    then
      FreeAndNil(fActiveInterpreter);
    fActiveInterpreter := Value;
  end;
end;

procedure TPythonControl.SetBreakPoint(FileName: string; ALine: integer;
  Disabled : Boolean; Condition: string);
var
  Editor : IEditor;
  i: integer;
  BreakPoint : TBreakPoint;
begin
  Editor := GI_EditorFactory.GetEditorByNameOrTitle(FileName);

  BreakPoint := nil;
  if Assigned(Editor) and (ALine > 0) then begin
    for i := 0 to Editor.Breakpoints.Count - 1 do begin
      if TBreakPoint(Editor.Breakpoints[i]).LineNo = ALine then begin
        BreakPoint := TBreakPoint(Editor.Breakpoints[i]);
        break;
      end else if TBreakPoint(Editor.Breakpoints[i]).LineNo > ALine then begin
        BreakPoint := TBreakPoint.Create;
        Editor.Breakpoints.Insert(i, BreakPoint);
        break;
      end;
    end;
    if not Assigned(BreakPoint) then begin
      BreakPoint := TBreakPoint.Create;
      Editor.Breakpoints.Add(BreakPoint);
    end;
    BreakPoint.LineNo := ALine;
    BreakPoint.Disabled := Disabled;
    BreakPoint.Condition := Condition;

    DoOnBreakpointChanged(Editor, ALine);
  end;
end;

procedure TPythonControl.SetPythonEngineType(const Value: TPythonEngineType);
begin
  if Value <> PythonEngineType then
    PythonIIForm.SetPythonEngineType(Value);
end;

procedure TPythonControl.ClearAllBreakpoints;
Var
  i : integer;
begin
  for i := 0 to GI_EditorFactory.Count -1 do
    if GI_EditorFactory.Editor[i].Breakpoints.Count > 0 then begin
      GI_EditorFactory.Editor[i].Breakpoints.Clear;
      DoOnBreakpointChanged(GI_EditorFactory.Editor[i], -1);
    end;
end;

procedure TPythonControl.DoCurrentPosChanged;
begin
  if Assigned(fOnCurrentPosChange) then
    fOnCurrentPosChange(Self);
end;

procedure TPythonControl.DoErrorPosChanged;
begin
  if Assigned(fOnErrorPosChange) then
    fOnErrorPosChange(Self);
end;

procedure TPythonControl.DoOnBreakpointChanged(Editor : IEditor; ALine: integer);
begin
  fBreakPointsChanged := True;
  if Assigned(fOnBreakpointChange) then
    fOnBreakpointChange(Self, Editor, ALine);
end;

procedure TPythonControl.DoStateChange(NewState : TDebuggerState);
Var
  OldDebuggerState: TDebuggerState;
begin
  OldDebuggerState := fDebuggerState;
  if NewState in [dsInactive, dsDebugging, dsRunning] then
    CurrentPos.Clear
  else begin
    ErrorPos.Clear;
    DoErrorPosChanged;
  end;
  fDebuggerState := NewState;
  if Assigned(fOnStateChange) then
    fOnStateChange(Self, OldDebuggerState, NewState);
  PyControl.DoCurrentPosChanged;
end;

procedure TPythonControl.DoYield(DoIdle : Boolean);
begin
  if Assigned(fOnYield) then
    fOnYield(Self, DoIdle);
end;

procedure TPythonControl.ExternalRun(ARunConfig: TRunConfiguration);
begin
  SetRunConfig(ARunConfig);
  OutputWindow.ExecuteTool(fRunConfig.ExternalRun);
end;

procedure TPythonControl.PrepareRun;
begin
  if PyIDEOptions.SaveFilesBeforeRun then begin
    PyIDEMainForm.SaveFileModules;
//    Application.ProcessMessages;
//    Application.DoApplicationIdle;
//    Application.ProcessMessages;
    PyIDEMainForm.Refresh;        // To update save flags
  end;
  if PyIDEOptions.SaveEnvironmentBeforeRun then
    PyIDEMainForm.SaveEnvironment;
  if PyIDEOptions.ClearOutputBeforeRun then
    PythonIIForm.actClearContentsExecute(nil);

  if fRunConfig.EngineType <> PythonEngineType then
    PythonEngineType := fRunConfig.EngineType
  else if (icReInitialize in ActiveInterpreter.InterpreterCapabilities) and
    fRunConfig.ReinitializeBeforeRun
  then
    ActiveInterpreter.ReInitialize;
end;

procedure TPythonControl.SetRunConfig(ARunConfig: TRunConfiguration);
begin
  if ARunConfig <> fRunConfig then
  begin
    fRunConfig.Assign(ARunConfig);
    // Expand Parameters in filename
    fRunConfig.fScriptName := '';  // to avoid circular substitution
    fRunConfig.fScriptName := Parameters.ReplaceInText(ARunConfig.fScriptName);
    PyIDEMainForm.SetRunLastScriptHints(fRunConfig.fScriptName);
  end;
end;

function TPythonControl.IsRunning: boolean;
begin
  Result := fDebuggerState in [dsDebugging, dsRunning];
end;

procedure TPythonControl.Run(ARunConfig: TRunConfiguration);
begin
  SetRunConfig(ARunConfig);

  if not Assigned(ActiveInterpreter) then Exit;

  PrepareRun;

  if fRunConfig.WriteOutputToFile then
    PythonIIForm.StartOutputMirror(Parameters.ReplaceInText(fRunConfig.OutputFileName),
      fRunConfig.AppendToFile);
  try
    ActiveInterpreter.Run(fRunConfig);
  finally
    if fRunConfig.WriteOutputToFile then
      PythonIIForm.StopFileMirror;
  end;
end;

{ TPyBaseDebugger }

function TPyBaseDebugger.AddPathToPythonPath(const Path: string;
  AutoRemove: Boolean): IInterface;
begin
  Result := TPythonPathAdder.Create(SysPathAdd, SysPathRemove, Path, AutoRemove);
end;

{ TRunConfiguration }

procedure TRunConfiguration.Assign(Source: TPersistent);
begin
  if Source is TRunConfiguration then with TRunConfiguration(Source) do begin
    Self.fScriptName := ScriptName;
    Self.fDescription := Description;
    Self.fEngineType := EngineType;
    Self.fWorkingDir := WorkingDir;
    Self.fParameters := fParameters;
    Self.fReinitializeBeforeRun := ReinitializeBeforeRun;
    Self.fWriteOutputToFile := WriteOutputToFile;
    Self.fOutputFileName := OutputFileName;
    Self.fAppendToFile := AppendToFile;
    Self.fExternalRun.Assign(fExternalRun);
  end else
    inherited;
end;

constructor TRunConfiguration.Create;
begin
  inherited;
  fEngineType := peRemote;
  fReinitializeBeforeRun := True;
  fOutputFileName := '$[ActiveScript-NoExt].log';
  fWorkingDir := '$[ActiveScript-Dir]';
  fExternalRun := TExternalRun.Create;
  fExternalRun.Assign(ExternalPython);
  fExternalRun.Caption := 'External Run';
  fExternalRun.Description := 'Run script using an external Python Interpreter';
  fExternalRun.Parameters := '$[ActiveScript-Short]';
  fExternalRun.WorkingDirectory := '$[ActiveScript-Dir]';
end;

destructor TRunConfiguration.Destroy;
begin
  fExternalRun.Free;
  inherited;
end;

procedure TRunConfiguration.SetExternalRun(const Value: TExternalRun);
begin
  fExternalRun.Assign(Value);
end;

{ TModuleProxy }

procedure TModuleProxy.Expand;
Var
  i : integer;
  VariableProxy : TVariableProxy;
  NS, ChildNS : TBaseNameSpaceItem;
begin
  if Name = '__main__' then begin
    if Assigned(fChildren) then fChildren.Clear;
    fGlobals.Clear;
  end else if fIsExpanded then
    Exit;

  NS := Interpreter.NameSpaceItemFromPyObject(Name, fPyModule);
  try
    for I := 0 to NS.ChildCount - 1 do begin
      ChildNS := NS.ChildNode[i];
      if ChildNS.IsFunction or ChildNS.IsMethod then
       AddChild(TFunctionProxy.CreateFromFunction(ChildNS.Name, ChildNS.PyObject))
      else if ChildNS.IsClass then
        AddChild(TClassProxy.CreateFromClass(ChildNS.Name, ChildNS.PyObject))
      else begin
        VariableProxy := TVariableProxy.CreateFromPyObject(ChildNS.Name, ChildNS.PyObject);
        VariableProxy.Parent := self;
        Globals.Add(VariableProxy);
      end;
    end;
  finally
    NS.Free;
  end;
  fIsExpanded := True;
end;

constructor TModuleProxy.CreateFromModule(AModule: Variant; aPyInterpreter : TPyBaseInterpreter);
begin
  inherited Create;
  if not VarIsPython(AModule) or (AModule.__class__.__name__ <> 'module') then
    Raise Exception.Create('TModuleProxy creation error');
  Name := AModule.__name__;
  fPyModule := AModule;
  fIsExpanded := false;
  fIsProxy := True;
  if BuiltInModule.hasattr(fPyModule, '__file__') then
    FileName := fPyModule.__file__;
  fPyInterpreter := aPyInterpreter;
end;

procedure TModuleProxy.GetNameSpace(SList: TStringList);
begin
  Expand;
  inherited;
end;

function TModuleProxy.GetAllExportsVar: string;
begin
   Result := '';
//   No need since we are exporting what is needed
//   if BuiltInModule.hasattr(fPyModule, '__all__') then begin
//     try
//       PythonIIForm.ShowOutput := False;
//       Result := BuiltInModule.str(fPyModule.__all__);
//       Result := Copy(Result, 2, Length(Result) - 2);
//     except
//       Result := '';
//     end;
//     PythonIIForm.ShowOutput := True;
//   end;
end;

function TModuleProxy.GetDocString: string;
Var
  PyDocString : Variant;
begin
  PyDocString := Import('inspect').getdoc(fPyModule);
  if not VarIsNone(PyDocString) then
    Result := PyDocString
  else
    Result := '';
end;

function TModuleProxy.GetCodeHint: string;
begin
  if IsPackage then
    Result := Format(_(SPackageProxyCodeHint), [Name])
  else
    Result := Format(_(SModuleProxyCodeHint), [Name]);
end;

{ TClassProxy }

procedure TClassProxy.Expand;
Var
  i : integer;
  VariableProxy : TVariableProxy;
  NS, ChildNS : TBaseNameSpaceItem;
begin
  if fIsExpanded then Exit;

  NS := (GetModule as TModuleProxy).Interpreter.NameSpaceItemFromPyObject(Name, fPyClass);
  NS.ExpandCommonTypes := True;
  NS.ExpandSequences := False;
  try
    for I := 0 to NS.ChildCount - 1 do begin
      ChildNS := NS.ChildNode[i];
      if ChildNS.IsFunction or ChildNS.IsMethod then
       AddChild(TFunctionProxy.CreateFromFunction(ChildNS.Name, ChildNS.PyObject))
      else if ChildNS.IsClass then
        AddChild(TClassProxy.CreateFromClass(ChildNS.Name, ChildNS.PyObject))
      else begin
        VariableProxy := TVariableProxy.CreateFromPyObject(ChildNS.Name, ChildNS.PyObject);
        VariableProxy.Parent := self;
        Attributes.Add(VariableProxy);
      end;
    end;
  finally
    NS.Free;
  end;

  // setup base classes
  try
    for i := 0 to len(fPyClass.__bases__) - 1 do
      SuperClasses.Add(fPyClass.__bases__[i].__name__);
  except
    // absorb this exception - nothing we can do
  end;

  fIsExpanded := True;
end;

constructor TClassProxy.CreateFromClass(AName : string; AClass: Variant);
begin
  inherited Create;
  if not VarIsPythonClass(AClass) then
    Raise Exception.Create('TClassProxy creation error');
  Name := AName;
  fPyClass := AClass;
  fIsExpanded := false;
  fIsProxy := True;
end;

procedure TClassProxy.GetNameSpace(SList: TStringList);
Var
  i : integer;
begin
  Expand;
  //  There is no need to examine base classes so we do not call inherited
  //  Add from Children
  for i := 0 to ChildCount - 1 do
    SList.AddObject(TCodeElement(Children[i]).Name, Children[i]);
  for i := 0 to Attributes.Count - 1 do
    SList.AddObject(TVariable(Attributes[i]).Name, Attributes[i])
end;

function TClassProxy.GetDocString: string;
Var
  PyDocString : Variant;
begin
  PyDocString := Import('inspect').getdoc(fPyClass);
  if not VarIsNone(PyDocString) then
    Result := PyDocString
  else
    Result := '';
end;

function TClassProxy.GetConstructor: TParsedFunction;
begin
  Expand;
  Result := inherited GetConstructor;
end;

{ TFunctionProxy }

function TFunctionProxy.ArgumentsString: string;
begin
  Result := InternalInterpreter.PyInteractiveInterpreter.get_arg_text(fPyFunction).__getitem__(0);
end;

constructor TFunctionProxy.CreateFromFunction(AName : string; AFunction: Variant);
var
  InspectModule : Variant;
begin
  inherited Create;
  InspectModule := Import('inspect');
  if InspectModule.isroutine(AFunction) then begin
//    Name := AFunction.__name__;
    Name := AName;
    fPyFunction := AFunction;
    fIsExpanded := false;
    fIsProxy := True;
  end else
    Raise Exception.Create('TFunctionProxy creation error');
end;

procedure TFunctionProxy.Expand;
Var
  i : integer;
  NoOfArgs : integer;
  Variable : TVariable;
  NS, ChildNS : TBaseNameSpaceItem;
begin
  if fIsExpanded then Exit;

  NS := (GetModule as TModuleProxy).Interpreter.NameSpaceItemFromPyObject(Name, fPyFunction);
  NS.ExpandCommonTypes := True;
  NS.ExpandSequences := False;
  try
    for I := 0 to NS.ChildCount - 1 do begin
      ChildNS := NS.ChildNode[i];
      if ChildNS.IsFunction or ChildNS.IsMethod then
       AddChild(TFunctionProxy.CreateFromFunction(ChildNS.Name, ChildNS.PyObject))
      else if ChildNS.IsClass then
        AddChild(TClassProxy.CreateFromClass(ChildNS.Name, ChildNS.PyObject))
      else begin
        AddChild(TVariableProxy.CreateFromPyObject(ChildNS.Name, ChildNS.PyObject));
      end;
    end;
  finally
    NS.Free;
  end;

  fIsExpanded := True;

  // Arguments and Locals
  if BuiltinModule.hasattr(fPyFunction, 'func_code') then begin
    NoOfArgs := fPyFunction.func_code.co_argcount;
    for i := 0 to len(fPyFunction.func_code.co_varnames) - 1 do begin
      Variable := TVariable.Create;
      Variable.Name := fPyFunction.func_code.co_varnames[i];
      Variable.Parent := Self;
      if i < NoOfArgs then begin
        Variable.Attributes := [vaArgument];
        Arguments.Add(Variable);
      end else
        Locals.Add(Variable);
    end;
  end else if BuiltinModule.hasattr(fPyFunction, '__code__') then begin  //Python 3000
    NoOfArgs := fPyFunction.__code__.co_argcount;
    for i := 0 to len(fPyFunction.__code__.co_varnames) - 1 do begin
      Variable := TVariable.Create;
      Variable.Name := fPyFunction.__code__.co_varnames[i];
      Variable.Parent := Self;
      if i < NoOfArgs then begin
        Variable.Attributes := [vaArgument];
        Arguments.Add(Variable);
      end else
        Locals.Add(Variable);
    end;
  end;
end;

function TFunctionProxy.GetDocString: string;
Var
  PyDocString : Variant;
begin
  PyDocString := Import('inspect').getdoc(fPyFunction);
  if not VarIsNone(PyDocString) then
    Result := PyDocString
  else
    Result := '';
end;

procedure TFunctionProxy.GetNameSpace(SList: TStringList);
begin
  Expand;
  inherited;
end;

{ TVariableProxy }

constructor TVariableProxy.CreateFromPyObject(const AName: string; AnObject: Variant);
begin
  inherited Create;
  Name := AName;
  fPyObject := AnObject;
  fIsExpanded := false;
  fIsProxy := True;
end;

procedure TVariableProxy.GetNameSpace(SList: TStringList);
begin
  Expand;
  inherited;
end;

procedure TVariableProxy.Expand;
Var
  i : integer;
  NS, ChildNS : TBaseNameSpaceItem;
begin
  if fIsExpanded then Exit;

  NS := (GetModule as TModuleProxy).Interpreter.NameSpaceItemFromPyObject(Name, fPyObject);
  NS.ExpandCommonTypes := True;
  NS.ExpandSequences := False;
  try
    for I := 0 to NS.ChildCount - 1 do begin
      ChildNS := NS.ChildNode[i];
      if ChildNS.IsFunction or ChildNS.IsMethod then
       AddChild(TFunctionProxy.CreateFromFunction(ChildNS.Name, ChildNS.PyObject))
      else if ChildNS.IsClass then
        AddChild(TClassProxy.CreateFromClass(ChildNS.Name, ChildNS.PyObject))
      else if ChildNS.IsModule then
        AddChild(TModuleProxy.CreateFromModule(ChildNS.PyObject,
          (GetModule as TModuleProxy).Interpreter))
      else begin
        AddChild(TVariableProxy.CreateFromPyObject(ChildNS.Name, ChildNS.PyObject));
      end;
    end;
  finally
    NS.Free;
  end;
  fIsExpanded := True;
end;

function TVariableProxy.GetDocString: string;
Var
  PyDocString : Variant;
begin
  PyDocString := Import('inspect').getdoc(fPyObject);
  if not VarIsNone(PyDocString) then
    Result := PyDocString
  else
    Result := '';
end;

function TVariableProxy.GetCodeHint: string;
Var
  Fmt, ObjType : string;
begin
  if Parent is TParsedFunction then
    Fmt := _(SLocalVariableCodeHint)
  else if Parent is TParsedClass then
    Fmt := _(SInstanceVariableCodeHint)
  else if Parent is TParsedModule then
    Fmt := _(SGlobalVariableCodeHint)
  else
    Fmt := '';
  if Fmt <> '' then begin
    Result := Format(Fmt,
      [Name, Parent.Name, '']);

    ObjType := BuiltInModule.type(PyObject).__name__;
    Result := Result + Format(_(SVariableTypeCodeHint), [ObjType]);
  end else
    Result := '';
end;


initialization
  PyControl := TPythonControl.Create;
finalization
  FreeAndNil(PyControl);
end.
