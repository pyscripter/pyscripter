{-----------------------------------------------------------------------------
 Unit Name: cPyControl
 Author:    PyScripter
 Date:      09-Feb-2018
 Purpose:   PyControl is the main interface between Python classes
            and PyScripter GUI
 History:
-----------------------------------------------------------------------------}

unit cPyControl;

interface

Uses
  System.Classes,
  uEditAppIntfs,
  cPySupportTypes,
  cPyBaseDebugger;

type
  TDebuggerState = (dsInactive, dsDebugging, dsPaused, dsRunning, dsPostMortem);

  TDebuggerLineInfo = (dlCurrentLine,
                       dlBreakpointLine,
                       dlDisabledBreakpointLine,
                       dlExecutableLine,
                       dlErrorLine);
  TDebuggerLineInfos = set of TDebuggerLineInfo;

  TBreakpointChangeEvent = procedure(Sender: TObject; Editor : IEditor; ALine: integer) of object;
  TDebuggerStateChangeEvent = procedure(Sender: TObject;
    OldState, NewState: TDebuggerState) of object;
  TDebuggerYieldEvent = procedure(Sender: TObject; DoIdle : Boolean) of object;

  TEditorPos = record
  public
    Editor : IEditor;
    Line : integer;
    Char : integer;
    IsSyntax : Boolean;
    ErrorMsg : string;
    procedure Clear;
    procedure NewPos(AEditor : IEditor; ALine : integer; AChar : integer = -1;
                     IsSyntaxError : Boolean = False; AErrorMsg : string = '');
  end;

  TPythonControl = class(Tobject)
  {
    Interface between PyScripter and the Interpreter/Debugger.
    Holds information Breakpoints, ErrorPos, CurrentPos
  }
  private
    fBreakPointsChanged : Boolean;
    fDebuggerState: TDebuggerState;
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
    ErrorPos: TEditorPos;
    CurrentPos: TEditorPos;

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

var
  PyControl : TPythonControl = nil;

implementation

uses
  System.SysUtils,
  System.Contnrs,
  System.UITypes,
  Vcl.Forms,
  Vcl.Dialogs,
  JvGnugettext,
  JvJVCLUtils,
  StringResources,
  frmPythonII,
  frmPyIDEMain,
  frmCommandOutput,
  frmVariables,
  frmUnitTests,
  cPyScripterSettings,
  cParameters,
  cPyDebugger,
  cPyRemoteDebugger,
  cProjectClasses;

{ TEditorPos }

procedure TEditorPos.NewPos(AEditor : IEditor; ALine : integer; AChar : integer = -1;
                 IsSyntaxError : Boolean = False; AErrorMsg : string = '');
begin
  Editor := AEditor;
  Line := ALine;
  Char := AChar;
  IsSyntax := IsSyntaxError;
  ErrorMsg := AErrorMsg;
end;

procedure TEditorPos.Clear;
begin
  Editor := nil;
  Line := -1;
  Char := -1;
  IsSyntax := False;
  ErrorMsg := '';
end;


{ TPythonControl }

constructor TPythonControl.Create;
begin
  fDebuggerState := dsInactive;
  CurrentPos.Clear;
  ErrorPos.Clear;
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
    Result := TPyRegExpr.IsExecutableLine(Lines[ALine-1]);
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
Var
  Cursor : IInterface;
  RemoteInterpreter : TPyRemoteInterpreter;
  Connected : Boolean;
  Msg : string;
begin
  if Value = PythonEngineType then Exit;

  if DebuggerState <> dsInactive then begin
    Vcl.Dialogs.MessageDlg(_(SCannotChangeEngine), mtError, [mbAbort], 0);
    Exit;
  end;

  VariablesWindow.ClearAll;
  UnitTestWindow.ClearAll;

  case Value of
    peInternal:
      begin
        ActiveInterpreter := InternalInterpreter;
        ActiveDebugger := TPyInternalDebugger.Create;
        PyIDEOptions.PythonEngineType := peInternal;
      end;
    peRemote, peRemoteTk, peRemoteWx:
      begin
        Application.ProcessMessages;
        Cursor := WaitCursor;
        // Destroy any active remote interpeter
        ActiveDebugger := nil;
        ActiveInterpreter := nil;
        try
          RemoteInterpreter := TPyRemoteInterpreter.Create(Value);
          Connected := RemoteInterpreter.Connected;
        except
          Connected := False;
        end;
        if Connected then begin
          ActiveInterpreter := RemoteInterpreter;
          ActiveDebugger := TPyRemDebugger.Create(RemoteInterpreter);
          PyIDEOptions.PythonEngineType := Value;

          // Add extra project paths
          if Assigned(ActiveProject) then
            ActiveProject.AppendExtraPaths;
        end else begin
          // failed to connect
          FreeAndNil(RemoteInterpreter);
          ActiveInterpreter := InternalInterpreter;
          ActiveDebugger := TPyInternalDebugger.Create;
          PyIDEOptions.PythonEngineType := peInternal;
        end;
      end;
  end;

  case PyIDEOptions.PythonEngineType of
    peInternal :  Msg := Format(_(SEngineActive), ['Internal','']);
    peRemote : Msg := Format(_(SEngineActive), ['Remote','']);
    peRemoteTk : Msg := Format(_(SEngineActive), ['Remote','(Tkinter) ']);
    peRemoteWx : Msg := Format(_(SEngineActive), ['Remote','(wxPython) ']);
  end;
  with PythonIIForm do begin
    if SynEdit.Lines[SynEdit.Lines.Count-1] = PS1 then
      SynEdit.Lines.Delete(SynEdit.Lines.Count -1);
    AppendText(sLineBreak + Msg);
    AppendPrompt;
  end;

  DoStateChange(dsInactive);
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
  if NewState in [dsInactive, dsDebugging, dsRunning] then begin
    CurrentPos.Clear;
    PyControl.DoCurrentPosChanged;
  end else begin
    ErrorPos.Clear;
    DoErrorPosChanged;
  end;
  fDebuggerState := NewState;
  if Assigned(fOnStateChange) then
    fOnStateChange(Self, OldDebuggerState, NewState);
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
    fRunConfig.ScriptName := '';  // to avoid circular substitution
    fRunConfig.ScriptName := Parameters.ReplaceInText(ARunConfig.ScriptName);
    PyIDEMainForm.SetRunLastScriptHints(fRunConfig.ScriptName);
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


initialization
  PyControl := TPythonControl.Create;
finalization
  FreeAndNil(PyControl);
end.
