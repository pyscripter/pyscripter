{-----------------------------------------------------------------------------
 Unit Name: cPySupportTypes
 Author:    PyScripter
 Date:      09-Feb-2018
 Purpose:   Defines basic types used in interfacing with Python
 History:
-----------------------------------------------------------------------------}

unit cPySupportTypes;

interface

Uses
  System.SysUtils,
  System.Classes,
  SynRegExpr,
  cTools;

type
   { The available types of PythonEngines }
   TPythonEngineType =
     (peInternal,   // always available - used to communicate with external ones
      peRemote,     // rpyc based external python - default
      peRemoteTk,   // specialized engines used to run GUI scripts
      peRemoteWx);

  {
     Container of all info needed to run a given file
     Projects can contain multiple run configurations
  }
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

  { Python related regular expressions }
  TPyRegExpr = class
    class var BlockOpenerRE : TRegExpr;
    class var BlockCloserRE : TRegExpr;
    class var CommentLineRE : TRegExpr;
    class var NonExecutableLineRE : TRegExpr;
    class constructor Create;
    class destructor Destroy;
    class function IsBlockOpener(S : string) : Boolean;
    class function IsBlockCloser(S : string) : Boolean;
    class function IsExecutableLine(Line : string) : Boolean;
  end;

{ Executes Python code in a Delphi thread }
procedure ThreadPythonExec(ExecuteProc : TProc; TerminateProc : TProc = nil);

Const
  IdentRE = '[A-Za-z_][A-Za-z0-9_]*';
  DottedIdentRE = '[A-Za-z_][A-Za-z0-9_.]*';

implementation

Uses
  Winapi.Windows,
  PythonEngine;

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
  with fExternalRun do begin
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
    WaitForTerminate := True;
  end;
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


{ TPyRegExpr }

class constructor TPyRegExpr.Create;
begin
  TPyRegExpr.BlockOpenerRE := TRegExpr.Create;
  TPyRegExpr.BlockOpenerRE.Expression := ':\s*(#.*)?$';
  TPyRegExpr.BlockCloserRE := TRegExpr.Create;
  TPyRegExpr.BlockCloserRE.Expression := '\s*(return|break|continue|raise|pass)\b';
  TPyRegExpr.NonExecutableLineRE := TRegExpr.Create;
  TPyRegExpr.NonExecutableLineRE.Expression := '(^\s*(class|def)\b)|(^\s*#)|(^\s*$)';
  TPyRegExpr.CommentLineRE := TRegExpr.Create;
  TPyRegExpr.CommentLineRE.Expression := '^([ \t]*)##';
  TPyRegExpr.CommentLineRE.ModifierM := True;
end;

class destructor TPyRegExpr.Destroy;
begin
  TPyRegExpr.BlockOpenerRE.Free;
  TPyRegExpr.BlockCloserRE.Free;
  TPyRegExpr.NonExecutableLineRE.Free;
  TPyRegExpr.CommentLineRE.Free;
end;

class function TPyRegExpr.IsBlockCloser(S: string): Boolean;
begin
  Result := TPyRegExpr.BlockCloserRE.Exec(S);
end;

class function TPyRegExpr.IsBlockOpener(S: string): Boolean;
begin
  Result := TPyRegExpr.BlockOpenerRE.Exec(S);
end;

class function TPyRegExpr.IsExecutableLine(Line: string): Boolean;
begin
  Result := not ((Line = '') or TPyRegExpr.NonExecutableLineRE.Exec(Line));
end;

{ TAnonymousPythonThread }
{ Exprerimental - not used }
type
TAnonymousPythonThread = class(TPythonThread)
private
  fTerminateProc : TProc;
  fExecuteProc : TProc;
  fMainThreadState : PPyThreadState;
  gilstate : PyGILState_STATE;
  procedure Terminate(Sender: TObject);
public
  procedure ExecuteWithPython; override;
  constructor Create(ExecuteProc : TProc; TerminateProc : TProc = nil);
end;


constructor TAnonymousPythonThread.Create(ExecuteProc, TerminateProc: TProc);
begin
  fExecuteProc := ExecuteProc;
  fTerminateProc := TerminateProc;
  OnTerminate := Terminate;
  FreeOnTerminate := True;
  with GetPythonEngine do
  begin
    gilstate := PyGILState_Ensure();
    Self.InterpreterState := InterpreterState;
    fMainThreadState := PyEval_SaveThread;
  end;
  inherited Create;
end;

procedure TAnonymousPythonThread.ExecuteWithPython;
begin
  if Assigned(fExecuteProc) then
    try
        fExecuteProc();
    except
    end;
end;

procedure TAnonymousPythonThread.Terminate(Sender: TObject);
begin
  with GetPythonEngine do begin
    PyEval_RestoreThread(fMainThreadState);
    PyGILState_Release(gilstate);
  end;
  if Assigned(fTerminateProc) then
    fTerminateProc();
end;

{ ThreadPythonExec }

procedure ThreadPythonExec(ExecuteProc : TProc; TerminateProc : TProc = nil);
begin
  if GetCurrentThreadId <> MainThreadID then
    raise Exception.Create('ThreadPythonExec should only be called from the main thread');
  TAnonymousPythonThread.Create(ExecuteProc, TerminateProc);
end;


end.
