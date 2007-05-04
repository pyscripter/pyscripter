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
  Windows, SysUtils, Classes, uEditAppIntfs, PythonEngine, Forms, Contnrs;

type
  TPythonEngineType = (peInternal, peRemote, peRemoteTk, peRemoteWx);
  TDebuggerState = (dsInactive, dsRunning, dsPaused, dsRunningNoDebug);
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
    GotChildNodes : Boolean;
    GotBufferedValue : Boolean;
    BufferedValue : string;
    function GetOrCalculateValue : String;
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
  end;

  TPyBaseInterpreter = class(TObject)
  //  Base (abstract) class for implementing Python Interpreters
  protected
    fInterpreterCapabilities : TInterpreterCapabilities;
  public
    // Python Path
    function SysPathAdd(const Path : WideString) : boolean; virtual; abstract;
    function SysPathRemove(const Path : WideString) : boolean; virtual; abstract;
    function AddPathToPythonPath(const Path : string; AutoRemove : Boolean = True) : IInterface;
    procedure SysPathToStrings(Strings : TStrings); virtual; abstract;
    procedure StringsToSysPath(Strings : TStrings); virtual; abstract;
    // NameSpace
    function GetGlobals : TBaseNameSpaceItem; virtual; abstract;
    function NameSpaceFromExpression(const Expr : string) : TBaseNameSpaceItem; virtual; abstract;
    function CallTipFromExpression(const Expr : string;
      var DisplayString, DocString : string) : Boolean; virtual; abstract;
    // Service routines
    procedure HandlePyException(E : EPythonError; SkipFrames : integer = 1); virtual;
    procedure SetCommandLine(const ScriptName : string); virtual; abstract;
    procedure RestoreCommandLine; virtual; abstract;
    procedure ReInitialize; virtual;
    // Main interface
    function ImportModule(Editor : IEditor; AddToNameSpace : Boolean = False) : Variant; virtual; abstract;
    procedure RunNoDebug(Editor : IEditor); virtual; abstract;
    function RunSource(Const Source, FileName : string; symbol : string = 'single') : boolean; virtual; abstract;
    function EvalCode(const Expr : string) : Variant; virtual; abstract;
    property InterpreterCapabilities : TInterpreterCapabilities read fInterpreterCapabilities;
  end;

  TPyBaseDebugger = class(TObject)
  {  Base (abstract) class for implementing Python Debuggers }
  protected
    procedure SetCommandLine(const ScriptName : string); virtual; abstract;
    procedure RestoreCommandLine; virtual; abstract;
    procedure SetDebuggerBreakpoints; virtual; abstract;
  public
    // Python Path
    function SysPathAdd(const Path : WideString) : boolean; virtual; abstract;
    function SysPathRemove(const Path : WideString) : boolean; virtual; abstract;
    function AddPathToPythonPath(const Path : string; AutoRemove : Boolean = True) : IInterface;
    // Debugging
    procedure Run(Editor : IEditor; InitStepIn : Boolean = False); virtual; abstract;
    procedure RunToCursor(Editor : IEditor; ALine: integer); virtual; abstract;
    procedure StepInto(Editor : IEditor); virtual; abstract;
    procedure StepOver; virtual; abstract;
    procedure StepOut; virtual; abstract;
    procedure Pause; virtual; abstract;
    procedure Abort; virtual; abstract;
    // Evaluate expression in the current frame
    procedure Evaluate(const Expr : string; out ObjType, Value : string); virtual; abstract;
    // Like the InteractiveInterpreter runsource but for the debugger frame
    function RunSource(Const Source, FileName : string; symbol : string = 'single') : boolean; virtual; abstract;
    // Fills in CallStackList with TBaseFrameInfo objects
    procedure GetCallStack(CallStackList : TObjectList); virtual; abstract;
    // functions to get TBaseNamespaceItems corresponding to a frame's gloabals and locals
    function GetFrameGlobals(Frame : TBaseFrameInfo) : TBaseNameSpaceItem; virtual; abstract;
    function GetFrameLocals(Frame : TBaseFrameInfo) : TBaseNameSpaceItem; virtual; abstract;
    function NameSpaceFromExpression(const Expr : string) : TBaseNameSpaceItem; virtual; abstract;
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
    procedure DoOnBreakpointChanged(Editor : IEditor; ALine: integer);
    procedure SetActiveDebugger(const Value: TPyBaseDebugger);
    procedure SetActiveInterpreter(const Value: TPyBaseInterpreter);
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
    // properties and events
    property ActiveInterpreter : TPyBaseInterpreter read fActiveInterpreter
      write SetActiveInterpreter;
    property ActiveDebugger : TPyBaseDebugger read fActiveDebugger
      write SetActiveDebugger;
    property BreakPointsChanged : Boolean read fBreakPointsChanged
      write fBreakPointsChanged;
    property DebuggerState : TDebuggerState read fDebuggerState;
    property ErrorPos: TEditorPos read fErrorPos;
    property CurrentPos: TEditorPos read fCurrentPos;
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

implementation

uses dmCommands, frmPythonII, VarPyth, frmMessages, frmPyIDEMain,
  MMSystem, Math, JvDockControlForm, JclFileUtils, Dialogs, uCommonFunctions,
  cParameters, JclSysUtils, StringResources, SynUnicode, cPyDebugger;

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

  TSysPathFunction = function(const Path : WideString) : boolean of object;

  TPythonPathAdder = class(TInterfacedObject, IInterface)
  private
    fPath : WideString;
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
  fPath := PathRemoveSeparator(Path);
  fAutoRemove := AutoRemove;
  fSysPathRemove := SysPathRemove;
  if (fPath <> '') and DirectoryExists(fPath) then begin
    // Add parent directory of the root of the package first
    if IsDirPythonPackage(fPath) then begin
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

procedure TPyBaseInterpreter.HandlePyException(E: EPythonError; SkipFrames : integer = 1);
Var
  TI : TTracebackItem;
begin
  MessagesWindow.ShowPythonTraceback(SkipFrames);
  MessagesWindow.AddMessage(E.Message);
  with GetPythonEngine.Traceback do begin
    if ItemCount > 0 then begin
      TI := Items[ItemCount -1];
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

procedure TPyBaseInterpreter.ReInitialize;
begin
  raise Exception.CreateRes(@SNotImplented);
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

function TBaseNameSpaceItem.GetOrCalculateValue: String;
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
end;

destructor TPythonControl.Destroy;
begin
  fCurrentPos.Free;
  fErrorPos.Free;
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
  if NewState in [dsInactive, dsRunning, dsRunningNoDebug] then
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

function TPythonControl.IsRunning: boolean;
begin
  Result := fDebuggerState in [dsRunning, dsRunningNoDebug];
end;

{ TPyBaseDebugger }

function TPyBaseDebugger.AddPathToPythonPath(const Path: string;
  AutoRemove: Boolean): IInterface;
begin
  Result := TPythonPathAdder.Create(SysPathAdd, SysPathRemove, Path, AutoRemove);
end;

initialization
  PyControl := TPythonControl.Create;
finalization
  FreeAndNil(PyControl);
end.


