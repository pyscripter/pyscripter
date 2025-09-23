{-----------------------------------------------------------------------------
 Unit Name: cPySupportTypes
 Author:    PyScripter
 Date:      09-Feb-2018
 Purpose:   Defines basic types used in interfacing with Python
 History:
-----------------------------------------------------------------------------}

unit cPySupportTypes;

interface

uses
  System.Classes,
  System.RegularExpressions,
  System.Contnrs,
  System.Messaging,
  SynEdit,
  PythonEngine,
  PythonVersions,
  cTools;

type
  { The available types of PythonEngines }
  TPythonEngineType =
    (peInternal,   // always available - used to communicate with external ones
     peRemote,     // rpyc based external Python - default
     peRemoteTk,   // specialized engines used to run GUI scripts
     peRemoteWx,
     peSSH);       // SSH Python engine

  TDebuggerState = (dsInactive, dsDebugging, dsPaused, dsRunning, dsPostMortem);

  // Notification messages
  TPythonVersionChangeMessage = class(System.Messaging.TMessage);
  TProjectPythonPathChangeMessage = class(System.Messaging.TMessage<TArray<string>>);

const
  // Defined DebugIDE events
  dbie_user_call            = 0;
  dbie_user_line            = 1;
  dbie_user_thread          = 2;
  dbie_user_exception       = 3;
  dbie_user_yield           = 4;

  EngineTypeName: array [TPythonEngineType] of string =
    ('Internal', 'Remote', 'Remote TK', 'Remote Wx', 'SSH');
  FilePosInfoFormat: string = '%s (%d:%d)';
  FilePosInfoRegExpr: string = '(.+) \((\d+):(\d+)\)$';

type
  TEditorPos = record
  public
    FileName: string;
    Line: Integer;
    Char: Integer;
    IsSyntax: Boolean;
    ErrorMsg: string;
    function PointsTo(AFileName: string; ALine: Integer): Boolean;
    function IsValid: Boolean;
    procedure Clear;
    class function EmptyPos: TEditorPos; static;
    class function New(FName: string; ALine: Integer; AChar: Integer = -1;
      IsSyntaxError: Boolean = False; AErrorMsg: string = ''): TEditorPos; static;
  end;

  TBreakpoint = class(TPersistent)
  private
    FLineNo: Integer;
    FDisabled: Boolean;
    FCondition: string;
    FIgnoreCount: Integer;
  public
    procedure Assign(Source: TPersistent); override;
  published
    constructor Create(ALineNo: Integer = 0);
    property LineNo: Integer read FLineNo write FLineNo;
    property Disabled: Boolean read FDisabled write FDisabled default False;
    property Condition: string read FCondition write FCondition;
    property IgnoreCount: Integer read FIgnoreCount write FIgnoreCount default 0;
  end;

  // list with TBreakpoints - is kept sorted
  TBreakpointList = class(TObjectList)
  public
    function FindBreakpoint(ALine: Integer; out Breakpoint: TBreakpoint): Boolean;
    procedure SetBreakpoint(ALine: Integer; ADisabled: Boolean;
      ACondition: string = ''; AIgnoreCount: Integer = 0);
    function HasBreakPoint(ALine: Integer): Boolean;
  end;

  {Container of all info needed to run a given file
   Projects can contain multiple run configurations}
  TRunConfiguration = class(TPersistent)
  private
    FScriptName: string;
    FEngineType: TPythonEngineType;
    FWorkingDir: string;
    FParameters: string;
    FReinitializeBeforeRun: Boolean;
    FOutputFileName: string;
    FWriteOutputToFile: Boolean;
    FAppendToFile: Boolean;
    FExternalRun: TExternalRun;
    FDescription: string;
    procedure SetExternalRun(const Value: TExternalRun);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ScriptName: string read FScriptName write FScriptName;
    property Description: string read FDescription write FDescription;
    property EngineType: TPythonEngineType read FEngineType write FEngineType;
    property ReinitializeBeforeRun: Boolean read FReinitializeBeforeRun
      write FReinitializeBeforeRun;
    property Parameters: string read FParameters write FParameters;
    property WorkingDir: string read FWorkingDir write FWorkingDir;
    property WriteOutputToFile: Boolean read FWriteOutputToFile
      write FWriteOutputToFile;
    property OutputFileName: string read FOutputFileName write FOutputFileName;
    property AppendToFile: Boolean read FAppendToFile write FAppendToFile;
    property ExternalRun: TExternalRun read FExternalRun write SetExternalRun;
  end;

  // Base (abstract) class for Call Stack frame information
  TBaseFrameInfo = class(TObject)
  protected
    function GetFunctionName: string; virtual; abstract;
    function GetFileName: string; virtual; abstract;
    function GetLine: Integer; virtual; abstract;
  public
    property FunctionName: string read GetFunctionName;
    property FileName: string read GetFileName;
    property Line: Integer read GetLine;
  end;

  { Python related regular expressions }
  TPyRegExpr = class
    class var BlockOpenerRE: TRegEx;
    class var BlockCloserRE: TRegEx;
    class var CodeCommentLineRE: TRegEx;
    class var NonExecutableLineRE: TRegEx;
    class var FunctionCallRE: TRegEx;
    class constructor Create;
    class function IsBlockOpener(Line: string): Boolean;
    class function IsBlockCloser(Line: string): Boolean;
    class function IsExecutableLine(Line: string): Boolean;
  end;

{$REGION 'Python IDE Interfaces'}

  IPyControl = interface
  ['{DE1C1145-DC0F-4829-B36B-74EC818E168E}']
    function PythonLoaded: Boolean;
    function Running: Boolean;
    function Inactive: Boolean;
    function GetCurrentPos: TEditorPos;
    function GetDebuggerState: TDebuggerState;
    function GetErrorPos: TEditorPos;
    function GetPythonVersion: TPythonVersion;
    function GetActiveSSHServerName: string;
    procedure AppendProjectPaths;
    procedure SetCurrentPos(const NewPos: TEditorPos);
    procedure SetDebuggerState(const NewState: TDebuggerState);
    procedure SetErrorPos(const NewPos: TEditorPos);
    procedure Pickle(AValue: Variant; FileName: string);
    property CurrentPos: TEditorPos read GetCurrentPos write SetCurrentPos;
    property DebuggerState: TDebuggerState read GetDebuggerState
      write SetDebuggerState;
    property ErrorPos: TEditorPos read GetErrorPos write SetErrorPos;
    property PythonVersion: TPythonVersion read GetPythonVersion;
    property ActiveSSHServerName: string read GetActiveSSHServerName;
  end;

  TPyInterpreterPropmpt = (pipNormal, pipDebug, pipPostMortem);
  IPyInterpreter = interface
  ['{6BAAD187-B00E-4E2A-B01D-C47EED922E59}']
    procedure ShowWindow;
    procedure AppendPrompt;
    procedure RemovePrompt;
    procedure AppendText(const Str: string);
    procedure PrintEngineType;
    procedure PrintInterpreterBanner(AVersion: string = ''; APlatform: string = '');
    procedure WritePendingMessages;
    procedure ClearPendingMessages;
    procedure ClearDisplay;
    procedure ClearLastPrompt;
    function OutputSuppressor: IInterface;
    procedure StartOutputMirror(const AFileName: string; Append: Boolean);
    procedure StopFileMirror;
    procedure UpdatePythonKeywords;
    procedure SetPyInterpreterPrompt(Pip: TPyInterpreterPropmpt);
    procedure ReinitInterpreter;
    function GetPythonIO: TPythonInputOutput;
    function GetEditor: TCustomSynEdit;
    function GetShowOutput: Boolean;
    procedure SetShowOutput(const Value: Boolean);
    property Editor: TCustomSynEdit read GetEditor;
    property PythonIO: TPythonInputOutput read GetPythonIO;
    property ShowOutput: Boolean read GetShowOutput write SetShowOutput;
  end;

  TBreakpointInfo = record
    FileName: string;
    LineNo: Integer;
    Disabled: Boolean;
    Condition: string;
    IgnoreCount: Integer;
  end;

  IBreakpointManager = interface
  ['{2A3F48C2-06E0-455D-B2D1-73BEAD0CF8F7}']
    function GetBreakpointsChanged: Boolean;
    procedure SetBreakpointsChanged(Value: Boolean);
    procedure ToggleBreakpoint(const FileName: string; ALine: Integer;
      CtrlPressed: Boolean = False; UpdateUI: Boolean = True);
    procedure SetBreakpoint(const FileName: string; ALine: Integer;
      Disabled: Boolean; Condition: string = ''; IgnoreCount: Integer = 0;
      UpdateUI: Boolean = True);
    function AllBreakPoints: TArray<TBreakpointInfo>;
    function EditProperties(var Condition: string; var IgnoreCount: Integer): Boolean;
    procedure ClearAllBreakpoints;
    property BreakpointsChanged: Boolean read GetBreakpointsChanged
      write SetBreakpointsChanged;
  end;

  IWatchManager = interface
  ['{98C8EE88-8C29-436F-9CDC-730E7C7F0CA8}']
    procedure AddWatch(Str: string);
    procedure UpdateWindow;
  end;

  IVariablesWindow = interface
  ['{9BD1D8C0-A0A2-4A56-B30F-615DFC41846B}']
    procedure ClearAll;
    procedure UpdateWindow;
  end;

  ICallStackWindow = interface
  ['{CE08088E-6AE5-4F14-9114-92AB67E241E7}']
    function GetSelectedStackFrame: TBaseFrameInfo;
    procedure ClearAll(IncludeThreads: Boolean = True);
    procedure UpdateWindow(NewState, OldState: TDebuggerState);
  end;

  // Global Interfaces
var
  GI_PyControl: IPyControl;
  GI_PyInterpreter: IPyInterpreter;
  GI_BreakpointManager: IBreakpointManager;
  GI_WatchManager: IWatchManager;
  GI_VariablesWindow: IVariablesWindow;
  GI_CallStackWindow: ICallStackWindow;

{$ENDREGION 'Python IDE Interfaces'}

const
  IdentRE = '[_\p{L}]\w*';
  DottedIdentRE = '[_\p{L}][\w\.]*';

implementation

uses
  System.SysUtils,
  uCommonFunctions;

{ TRunConfiguration }

procedure TRunConfiguration.Assign(Source: TPersistent);
begin
  if Source is TRunConfiguration then
  begin
    var SourceRC := TRunConfiguration(Source);
    FScriptName := SourceRC.ScriptName;
    FDescription := SourceRC.Description;
    FEngineType := SourceRC.EngineType;
    FWorkingDir := SourceRC.WorkingDir;
    FParameters := SourceRC.Parameters;
    FReinitializeBeforeRun := SourceRC.ReinitializeBeforeRun;
    FWriteOutputToFile := SourceRC.WriteOutputToFile;
    FOutputFileName := SourceRC.OutputFileName;
    FAppendToFile := SourceRC.AppendToFile;
    FExternalRun.Assign(SourceRC.ExternalRun);
  end else
    inherited;
end;

constructor TRunConfiguration.Create;
begin
  inherited;
  FEngineType := peRemote;
  FReinitializeBeforeRun := True;
  FOutputFileName := '$[ActiveScript-NoExt].log';
  FWorkingDir := '$[ActiveScript-Dir]';
  FExternalRun := TExternalRun.Create;
  with FExternalRun do begin
    Caption := 'External Run';
    Description := 'Run script using an external Python Interpreter';
    ApplicationName := '$[PythonExe-Short]';
    Parameters := '$[ActiveScript-Short]';
    WorkingDirectory := '$[ActiveScript-Dir]';
    SaveFiles := sfAll;
    Context := tcActiveFile;
    ParseTraceback := True;
    CaptureOutput := True;
    ConsoleHidden := True;
  end;
end;

destructor TRunConfiguration.Destroy;
begin
  FExternalRun.Free;
  inherited;
end;

procedure TRunConfiguration.SetExternalRun(const Value: TExternalRun);
begin
  FExternalRun.Assign(Value);
end;

{ TPyRegExpr }

class constructor TPyRegExpr.Create;
begin
  BlockOpenerRE := CompiledRegEx(':\s*(#.*)?$');
  BlockCloserRE := CompiledRegEx('\s*(return|break|continue|raise|pass)\b');
  CodeCommentLineRE := CompiledRegEx('^([ \t]*)##', [roNotEmpty, roMultiLine]);
  NonExecutableLineRE := CompiledRegEx('(^\s*(class|def)\b)|(^\s*#)|(^\s*$)');
  FunctionCallRE := CompiledRegEx(Format('^[ \t]*(%s)(\(?)', [DottedIdentRE]));
end;

class function TPyRegExpr.IsBlockCloser(Line: string): Boolean;
begin
  Result := TPyRegExpr.BlockCloserRE.IsMatch(Line);
end;

class function TPyRegExpr.IsBlockOpener(Line: string): Boolean;
begin
  Result := TPyRegExpr.BlockOpenerRE.IsMatch(Line);
end;

class function TPyRegExpr.IsExecutableLine(Line: string): Boolean;
begin
  Result := not ((Line = '') or TPyRegExpr.NonExecutableLineRE.IsMatch(Line));
end;

{ TEditorPos }

class function TEditorPos.EmptyPos: TEditorPos;
begin
  with Result do begin
    FileName := '';
    Line := -1;
    Char := -1;
    IsSyntax := False;
    ErrorMsg := '';
  end;
end;

function TEditorPos.IsValid: Boolean;
begin
  Result := FileName <> '';
end;

class function TEditorPos.New(FName: string; ALine: Integer; AChar: Integer =
    -1; IsSyntaxError: Boolean = False; AErrorMsg: string = ''): TEditorPos;
begin
  with Result do begin
    FileName := FName;
    Line := ALine;
    Char := AChar;
    IsSyntax := IsSyntaxError;
    ErrorMsg := AErrorMsg;
  end;
end;

function TEditorPos.PointsTo(AFileName: string; ALine: Integer): Boolean;
begin
  Result := (ALine = Line) and (AnsiCompareText(AFileName, FileName) = 0);
end;

procedure TEditorPos.Clear;
begin
  FileName := '';
  Line := -1;
  Char := -1;
  IsSyntax := False;
  ErrorMsg := '';
end;


{ TBreakpoint }

procedure TBreakpoint.Assign(Source: TPersistent);
var
  Src: TBreakpoint;
begin
  if (Source <> nil) and (Source is TBreakpoint) then
  begin
    Src := TBreakpoint(Source);
    FLineNo := Src.LineNo;
    FDisabled := Src.Disabled;
    FCondition := Src.Condition;
    FIgnoreCount := Src.IgnoreCount;
  end
  else
    inherited Assign(Source);
end;

constructor TBreakpoint.Create(ALineNo: Integer);
begin
  inherited Create;
  LineNo := ALineNo;
end;

{ TBreakpointList }

function TBreakpointList.FindBreakpoint(ALine: Integer;
  out Breakpoint: TBreakpoint): Boolean;
var
  Index: NativeInt;
begin
  Result := False;
  Breakpoint := nil;
  Index := 0;
  while Index < Count do
  begin
    if TBreakpoint(Items[Index]).LineNo = ALine then
    begin
      Result := True;
      Breakpoint := TBreakpoint(Items[Index]);
      Break;
    end
    else if TBreakpoint(Items[Index]).LineNo > ALine  then
      Break;
    Inc(Index);
  end;
end;

function TBreakpointList.HasBreakPoint(ALine: Integer): Boolean;
var
  Breakpoint: TBreakpoint;
begin
  Result := FindBreakpoint(ALine, Breakpoint);
end;

procedure TBreakpointList.SetBreakpoint(ALine: Integer; ADisabled: Boolean;
  ACondition: string; AIgnoreCount: Integer);
var
  Index: Integer;
  BreakPoint: TBreakpoint;
begin
  Breakpoint := nil;
  Index := 0;
  while Index < Count do
  begin
    if TBreakpoint(Items[Index]).LineNo = ALine then
    begin
      Breakpoint := TBreakpoint(Items[Index]);
      Break;
    end
    else if TBreakpoint(Items[Index]).LineNo > ALine  then
      Break;
    Inc(Index);
  end;
  if Breakpoint = nil then
  begin
    BreakPoint := TBreakpoint.Create(ALine);
    Insert(Index, Breakpoint);
  end;
  BreakPoint.Disabled := ADisabled;
  BreakPoint.Condition := ACondition;
  BreakPoint.IgnoreCount := AIgnoreCount;
end;

end.
