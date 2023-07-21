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
  System.SysUtils,
  System.Classes,
  JclNotify,
  JvAppStorage,
  PythonVersions,
  PythonEngine,
  uEditAppIntfs,
  cPySupportTypes,
  cPyBaseDebugger,
  cInternalPython;

type
  TDebuggerState = (dsInactive, dsDebugging, dsPaused, dsRunning, dsPostMortem);

  TDebuggerLineInfo = (dlCurrentLine,
                       dlBreakpointLine,
                       dlDisabledBreakpointLine,
                       dlExecutableLine,
                       dlErrorLine);
  TDebuggerLineInfos = set of TDebuggerLineInfo;

  TBreakpointChangeEvent = procedure(Sender: TObject; Editor : IEditor; ALine: integer) of object;
  TDebuggerStateChangeEvent = procedure(Sender: TObject; OldState, NewState: TDebuggerState) of object;
  TDebuggerPosChangeEvent = procedure(Sender: TObject; const OldPos, NewPos: TEditorPos) of object;

  TPythonControl = class(TComponent, IPyControl)
  {
    Interface between PyScripter and the Interpreter/Debugger.
    Holds information Breakpoints, ErrorPos, CurrentPos
  }
  private
    fCurrentPos: TEditorPos;
    fErrorPos: TEditorPos;
    fFinalizing: Boolean;
    fBreakPointsChanged: Boolean;
    fDebuggerState: TDebuggerState;
    fOnBreakpointChange: TBreakpointChangeEvent;
    fOnCurrentPosChange: TDebuggerPosChangeEvent;
    fOnErrorPosChange: TDebuggerPosChangeEvent;
    fOnStateChange: TDebuggerStateChangeEvent;
    fOnPythonVersionChange: TJclNotifyEventBroadcast;
    fActiveInterpreter: TPyBaseInterpreter;
    fActiveDebugger: TPyBaseDebugger ;
    fRunConfig: TRunConfiguration;
    fPythonVersionIndex: integer;
    fRegPythonVersions: TPythonVersions;
    fInternalPython: TInternalPython;
    fInternalInterpreter: TPyBaseInterpreter;
    fActiveSSHServerName: string;
    fPythonHelpFile: string;
    function InitPythonVersions : Boolean;
    procedure DoOnBreakpointChanged(Editor : IEditor; ALine: integer);
    procedure SetActiveDebugger(const Value: TPyBaseDebugger);
    procedure SetActiveInterpreter(const Value: TPyBaseInterpreter);
    procedure SetDebuggerState(NewState : TDebuggerState);
    procedure SetCurrentPos(const NewPos : TEditorPos);
    procedure SetErrorPos(const NewPos : TEditorPos);
    function GetPythonEngineType: TPythonEngineType;
    procedure SetPythonEngineType(const Value: TPythonEngineType);
    procedure SetRunConfig(ARunConfig: TRunConfiguration);
    procedure PrepareRun;
    function GetInternalInterpreter: TPyBaseInterpreter;
    procedure SetPythonVersionIndex(const Value: integer);
    // IPyControl implementation
    function PythonLoaded: Boolean;
    function Running: boolean;
    function Inactive: boolean;
    function GetPythonVersion: TPythonVersion;
    function GetOnPythonVersionChange: TJclNotifyEventBroadcast;
    function AddPathToInternalPythonPath(const Path: string): IInterface;
    function SafePyEngine: IPyEngineAndGIL;
    procedure ThreadPythonExec(ExecuteProc : TProc; TerminateProc : TProc = nil;
      WaitToFinish: Boolean = False; ThreadExecMode : TThreadExecMode = emNewState);
  public
    const MinPyVersion = '3.7';
    const MaxPyVersion = '3.12'; //PYTHON312
  public
    // ActiveInterpreter and ActiveDebugger are created
    // and destroyed in frmPythonII
    CustomPythonVersions: TPythonVersions;

    constructor Create(AOwner: TComponent); override;
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
    // Running Python Scripts
    procedure Run(ARunConfig : TRunConfiguration);
    procedure Debug(ARunConfig : TRunConfiguration;  InitStepIn : Boolean = False;
      RunToCursorLine : integer = -1);
    procedure ExternalRun(ARunConfig : TRunConfiguration);
    // InternalPython
    procedure LoadPythonEngine; overload;
    procedure LoadPythonEngine(const APythonVersion : TPythonVersion); overload;
    // (Re)storing PythonVersions
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
      out SysVersion, InstallPath: string);
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage);
    // Custom versions
    function RemoveCustomVersion(AIndex: Integer): Boolean;
    // properties and events
    // PythonVersionIndex is the Index of Python version in the PYTHON_KNOWN_VERSIONS array
    property PythonVersion: TPythonVersion read GetPythonVersion;
    property PythonVersionIndex: integer read fPythonVersionIndex write SetPythonVersionIndex;
    property RegPythonVersions: TPythonVersions read fRegPythonVersions;
    property PythonEngineType: TPythonEngineType read GetPythonEngineType
      write SetPythonEngineType;
    property ActiveSSHServerName: string read fActiveSSHServerName write fActiveSSHServerName;
    property InternalPython: TInternalPython read fInternalPython;
    property InternalInterpreter: TPyBaseInterpreter read GetInternalInterpreter;
    property ActiveInterpreter: TPyBaseInterpreter read fActiveInterpreter
      write SetActiveInterpreter;
    property PythonHelpFile: string read fPythonHelpFile;
    property ActiveDebugger: TPyBaseDebugger read fActiveDebugger
      write SetActiveDebugger;
    property CurrentPos: TEditorPos read fCurrentPos write SetCurrentPos;
    property ErrorPos: TEditorPos read fErrorPos write SetErrorPos;
    property BreakPointsChanged : Boolean read fBreakPointsChanged
      write fBreakPointsChanged;
    property DebuggerState: TDebuggerState read fDebuggerState write SetDebuggerState;
    property RunConfig: TRunConfiguration read fRunConfig;
    property Finalizing: Boolean read fFinalizing;
    property OnBreakpointChange: TBreakpointChangeEvent read fOnBreakpointChange
      write fOnBreakpointChange;
    property OnCurrentPosChange: TDebuggerPosChangeEvent read fOnCurrentPosChange
      write fOnCurrentPosChange;
    property OnErrorPosChange: TDebuggerPosChangeEvent read fOnErrorPosChange
      write fOnErrorPosChange;
    property OnStateChange: TDebuggerStateChangeEvent read fOnStateChange
      write fOnStateChange;
    property OnPythonVersionChange: TJclNotifyEventBroadcast read GetOnPythonVersionChange;
  end;

var
  PyControl : TPythonControl = nil;

implementation

uses
  WinApi.Windows,
  System.Contnrs,
  System.UITypes,
  System.Math,
  Vcl.Forms,
  Vcl.Dialogs,
  JvGnugettext,
  JvJVCLUtils,
  VarPyth,
  StringResources,
  uCmdLine,
  uCommonFunctions,
  cPyScripterSettings,
  cPyDebugger,
  cPyRemoteDebugger,
  cProjectClasses,
  cSSHSupport,
  cPySSHDebugger;

{ TPythonControl }

constructor TPythonControl.Create(AOwner: TComponent);
begin
  inherited;
  GI_PyControl := Self;
  fDebuggerState := dsInactive;
  fCurrentPos.Clear;
  fErrorPos.Clear;
  fRunConfig := TRunConfiguration.Create;
  fInternalPython := TInternalPython.Create;
  fRegPythonVersions := GetRegisteredPythonVersions(MinPyVersion, MaxPyVersion);
  fOnPythonVersionChange := TJclNotifyEventBroadcast.Create;
end;

procedure TPythonControl.Debug(ARunConfig: TRunConfiguration; InitStepIn : Boolean = False;
      RunToCursorLine : integer = -1);
begin
  SetRunConfig(ARunConfig);

  if not Assigned(ActiveDebugger) then Exit;

  PrepareRun;

  if fRunConfig.WriteOutputToFile then
    GI_PyInterpreter.StartOutputMirror(GI_PyIDEServices.ReplaceParams(fRunConfig.OutputFileName),
      fRunConfig.AppendToFile);
  try
    ActiveDebugger.Debug(fRunConfig, InitStepIn, RunToCursorLine);
  finally
    if fRunConfig.WriteOutputToFile then
      GI_PyInterpreter.StopFileMirror;
  end;
end;

destructor TPythonControl.Destroy;
begin
  GI_PyControl := nil;
  FreeAndNil(fInternalInterpreter);
  FreeAndNil(fInternalPython);
  FreeAndNil(fOnPythonVersionChange);
  fRunConfig.Free;
  inherited;
end;

function TPythonControl.GetInternalInterpreter: TPyBaseInterpreter;
begin
  Result := fInternalInterpreter;
  if not (InternalPython.Loaded and Assigned(fInternalInterpreter)) then
  begin
    StyledMessageDlg(_(SInterpreterNA), mtError, [mbAbort], 0);
    Abort;
  end;
end;

function TPythonControl.GetLineInfos(Editor : IEditor; ALine: integer): TDebuggerLineInfos;
Var
  Disabled : boolean;
begin
  Result := [];
  if ALine > 0 then begin
    if (Editor = CurrentPos.Editor) and (ALine = CurrentPos.Line) then
      Include(Result, dlCurrentLine);
    if (Editor = ErrorPos.Editor) and (ALine = ErrorPos.Line) then
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

function TPythonControl.GetOnPythonVersionChange: TJclNotifyEventBroadcast;
begin
  Result := fOnPythonVersionChange;
end;

function TPythonControl.GetPythonEngineType: TPythonEngineType;
begin
  if Assigned(ActiveInterpreter) then
    Result := ActiveInterpreter.EngineType
  else
    Result := peInternal;
end;

function TPythonControl.GetPythonVersion: TPythonVersion;
begin
  if (fPythonVersionIndex >= 0) and (fPythonVersionIndex < Length(fRegPythonVersions)) then
    Result := fRegPythonVersions[fPythonVersionIndex]
  else if (fPythonVersionIndex < 0) and (-fPythonVersionIndex <= Length(CustomPythonVersions)) then
    Result := CustomPythonVersions[-fPythonVersionIndex -1]
  else
    Assert(False, 'Invalid PythonVersionIndex');
end;

function TPythonControl.Inactive: boolean;
begin
  Result := InternalPython.Loaded and (fDebuggerState = dsInactive);
end;

function TPythonControl.InitPythonVersions: Boolean;
Var
  expectedVersion,
  LastVersion,
  LastInstallPath,
  DLLPath : string;
  Version : TPythonVersion;
  I : integer;
begin
  // first find an optional parameter specifying the expected Python version in the form of -PYTHONXY
  expectedVersion := '';

  if CmdLineReader.readFlag('PYTHON37') then
    expectedVersion := '3.7'
  else if CmdLineReader.readFlag('PYTHON38') then
    expectedVersion := '3.8'
  else if CmdLineReader.readFlag('PYTHON39') then
    expectedVersion := '3.9'
  else if CmdLineReader.readFlag('PYTHON310') then
    expectedVersion := '3.10'
  else if CmdLineReader.readFlag('PYTHON311') then
    expectedVersion := '3.11'
  else if CmdLineReader.readFlag('PYTHON312') then
    expectedVersion := '3.12';
  DllPath := CmdLineReader.readString('PYTHONDLLPATH');

  ReadFromAppStorage(GI_PyIDEServices.LocalAppStorage, LastVersion, LastInstallPath);
  if (DllPath = '') and (expectedVersion = '') then
  begin
    expectedVersion := LastVersion;
    DLLPath := LastInstallPath;
  end;

  Result := False;
  if (DllPath = '') and (expectedVersion = '') then
  begin
    if Length(fRegPythonVersions) > 0 then begin
      fPythonVersionIndex := 0;
      Result := True;
    end;
  end
  else if DllPath = '' then
  begin
    for I := 0 to Length(fRegPythonVersions) - 1 do
      if fRegPythonVersions[I].SysVersion = expectedVersion then
      begin
        fPythonVersionIndex := I;
        Result := True;
        break;
      end;
    // if the expectedVersion is not available load the latest registred version
    if not Result and (Length(fRegPythonVersions) > 0) then
    begin
      fPythonVersionIndex := 0;
      Result := True;
    end;
  end
  else
  begin
    for I := 0 to Length(CustomPythonVersions) -1 do
      if (CustomPythonVersions[I].DLLPath = DLLPath) or
         (CustomPythonVersions[I].InstallPath = DLLPath) then
      begin
        Result := True;
        fPythonVersionIndex := -(I + 1);
        break;
      end;
    if not Result then begin
      Result := PythonVersionFromPath(DLLPath, Version, True,
        MinPyVersion, MaxPyVersion);
      if Result then begin
        SetLength(CustomPythonVersions, Length(CustomPythonVersions) + 1);
        CustomPythonVersions[Length(CustomPythonVersions)-1] := Version;
        fPythonVersionIndex := - Length(CustomPythonVersions);
      end;
    end;
    // if the loading from path fails load the latest registered version
    if not Result and (Length(fRegPythonVersions) > 0) then
    begin
      fPythonVersionIndex := 0;
      Result := True;
    end;
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
    Result := TPyRegExpr.IsExecutableLine(Lines[ALine-1]);
  end;
end;

procedure TPythonControl.ThreadPythonExec(ExecuteProc, TerminateProc: TProc;
  WaitToFinish: Boolean; ThreadExecMode: TThreadExecMode);
begin
  InternalThreadPythonExec(ExecuteProc, TerminateProc, WaitToFinish, ThreadExecMode);
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

function TPythonControl.SafePyEngine: IPyEngineAndGIL;
begin
  Result := InternalSafePyEngine;
end;

procedure TPythonControl.SetActiveDebugger(const Value: TPyBaseDebugger);
begin
  if fActiveDebugger <> Value then begin
    if Assigned(fActiveDebugger) then
    begin
      var Py := SafePyEngine;
      FreeAndNil(fActiveDebugger);
    end;
    fActiveDebugger := Value;
  end;
end;

procedure TPythonControl.SetActiveInterpreter(const Value: TPyBaseInterpreter);
begin
  if fActiveInterpreter <> Value then begin
    if Assigned(fActiveInterpreter) and
      (fActiveInterpreter <> fInternalInterpreter)
    then
    begin
      var Py := SafePyEngine;
      FreeAndNil(fActiveInterpreter);
    end;
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
  Editor := GI_EditorFactory.GetEditorByFileId(FileName);

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
  SSHServer: TSSHServer;
begin
  if not InternalPython.Loaded or ((Value = PythonEngineType) and not
   ((Value = peSSH) and (ActiveInterpreter is TPySSHInterpreter) and
    (ActiveSSHServerName <> TPySSHInterpreter(ActiveInterpreter).SSHServerName)))
  then
    Exit;

  var Py := SafePyEngine;

  if DebuggerState <> dsInactive then begin
    StyledMessageDlg(_(SCannotChangeEngine), mtError, [mbAbort], 0);
    Exit;
  end;

  GI_PyIDEServices.ClearPythonWindows;

  case Value of
    peInternal:
      begin
        ActiveInterpreter := fInternalInterpreter;
        ActiveDebugger := ActiveInterpreter.CreateDebugger;
        PyIDEOptions.PythonEngineType := peInternal;
      end;
    peRemote, peRemoteTk, peRemoteWx, peSSH:
      begin
        SSHServer := nil;
        if Value = peSSH then begin
          if ActiveSSHServerName = '' then begin
            ActiveSSHServerName := SelectSSHServer;
            if ActiveSSHServerName = '' then Exit;
          end;
          SSHServer := ServerFromName(ActiveSSHServerName);
          if not Assigned(SSHServer) then
          begin
            StyledMessageDlg(Format(_(SSHUnknownServer), [ActiveSSHServerName]),
              mtError, [mbAbort],0);
            Exit;
          end;
        end;
        //Application.ProcessMessages;
        Cursor := WaitCursor;
        // Destroy any active remote interpeter
        ActiveDebugger := nil;
        ActiveInterpreter := nil;
        try
          if Value = peSSH then
            RemoteInterpreter := TPySSHInterpreter.Create(SSHServer)
          else
            RemoteInterpreter := TPyRemoteInterpreter.Create(Value);
          Connected := RemoteInterpreter.Connected;
        except
          Connected := False;
        end;
        if Connected then begin
          ActiveInterpreter := RemoteInterpreter;
          ActiveDebugger := ActiveInterpreter.CreateDebugger;
          PyIDEOptions.PythonEngineType := Value;

          // Add extra project paths
          if Assigned(ActiveProject) then
            ActiveProject.AppendExtraPaths;
        end else begin
          // failed to connect
          FreeAndNil(RemoteInterpreter);
          ActiveInterpreter := fInternalInterpreter;
          ActiveDebugger := TPyInternalDebugger.Create;
          PyIDEOptions.PythonEngineType := peInternal;
        end;
      end;
  end;

  case PyIDEOptions.PythonEngineType of
    peInternal :  Msg := Format(_(SEngineActive), [_('Internal')]);
    peRemote : Msg := Format(_(SEngineActive), [_('Remote')]);
    peRemoteTk : Msg := Format(_(SEngineActive), [_('Remote (Tk)')]);
    peRemoteWx : Msg := Format(_(SEngineActive), ['Remote (Wx)']);
    peSSH : Msg := Format(_(SEngineActive), [Format('"%s" SSH', [ActiveSSHServerName])]);
  end;
  GI_PyInterpreter.ClearLastPrompt;
  GI_PyInterpreter.AppendText(sLineBreak + Msg);
  if PyIDEOptions.PythonEngineType = peSSH then with ActiveInterpreter as TPySSHInterpreter do
    GI_PyInterpreter.PrintInterpreterBanner(PythonVersion, RemotePlatform);
  GI_PyInterpreter.AppendPrompt;

  DebuggerState := dsInactive;
end;

procedure TPythonControl.SetPythonVersionIndex(const Value: integer);
begin
  if (Value <> fPythonVersionIndex) and (DebuggerState = dsInactive) then begin
    fPythonVersionIndex := Value;
    LoadPythonEngine(PythonVersion);
  end;
end;

function TPythonControl.AddPathToInternalPythonPath(const Path: string): IInterface;
begin
  Result := InternalInterpreter.AddPathToPythonPath(Path);
end;

procedure TPythonControl.ClearAllBreakpoints;
begin
  GI_EditorFactory.ApplyToEditors(procedure(Editor: IEditor)
  begin
    if Editor.Breakpoints.Count > 0 then begin
      Editor.Breakpoints.Clear;
      DoOnBreakpointChanged(Editor, -1);
    end;
  end);
end;

procedure TPythonControl.SetCurrentPos(const NewPos : TEditorPos);
begin
  if Assigned(fOnCurrentPosChange) then
  begin
    if (GetCurrentThreadId = MainThreadId) then
      fOnCurrentPosChange(Self, fCurrentPos, NewPos)
    else
      TThread.Synchronize(nil, procedure
      begin
        fOnCurrentPosChange(Self, fCurrentPos, NewPos);
      end);
  end;
  fCurrentPos := NewPos;
end;

procedure TPythonControl.SetErrorPos(const NewPos : TEditorPos);
begin
  if Assigned(fOnErrorPosChange) then
  begin
    if (GetCurrentThreadId = MainThreadId) then
      fOnErrorPosChange(Self, fErrorPos, NewPos)
    else
      TThread.Synchronize(nil, procedure
      begin
        fOnErrorPosChange(Self, fErrorPos, NewPos);
      end);
  end;
  fErrorPos := NewPos;
end;

procedure TPythonControl.DoOnBreakpointChanged(Editor : IEditor; ALine: integer);
begin
  fBreakPointsChanged := True;
  if Assigned(fOnBreakpointChange) then
  begin
    if (GetCurrentThreadId = MainThreadId) then
      fOnBreakpointChange(Self, Editor, ALine)
    else
      TThread.Synchronize(nil, procedure
      begin
        fOnBreakpointChange(Self, Editor, ALine);
      end);
  end;
end;

procedure TPythonControl.SetDebuggerState(NewState : TDebuggerState);
Var
  OldDebuggerState: TDebuggerState;
begin
  OldDebuggerState := fDebuggerState;
  if NewState in [dsInactive, dsDebugging, dsRunning] then
    CurrentPos := TEditorPos.EmptyPos
  else
    ErrorPos := TEditorPos.EmptyPos;
  fDebuggerState := NewState;
  if Assigned(fOnStateChange) then
  begin
    if (GetCurrentThreadId = MainThreadId) then
      fOnStateChange(Self, OldDebuggerState, NewState)
    else
    begin
      TPythonThread.Py_Begin_Allow_Threads;
      try
        TThread.Synchronize(nil, procedure
        begin
          fOnStateChange(Self, OldDebuggerState, NewState);
        end);
      finally
        TPythonThread.Py_End_Allow_Threads;
      end;
    end;
  end;
end;

procedure TPythonControl.ExternalRun(ARunConfig: TRunConfiguration);
begin
  SetRunConfig(ARunConfig);
  fRunConfig.ExternalRun.Execute;
end;

procedure TPythonControl.PrepareRun;
Var
  Server, FName: string;
begin
  if PyIDEOptions.SaveFilesBeforeRun then begin
    GI_PyIDEServices.SaveFileModules;
    Application.MainForm.Refresh;        // To update save flags
  end;
  if PyIDEOptions.SaveEnvironmentBeforeRun then
    GI_PyIDEServices.SaveEnvironment;
  if PyIDEOptions.ClearOutputBeforeRun then
    GI_PyInterpreter.ClearDisplay;

  if (fRunConfig.EngineType <> PythonEngineType) or ((PythonEngineType = peSSH) and
    TSSHFileName.Parse(fRunConfig.ScriptName, Server, FName) and (Server <> ActiveSSHServerName))
  then begin
    if Server <> '' then
      ActiveSSHServerName := Server;
    PythonEngineType := fRunConfig.EngineType
  end else if (icReInitialize in ActiveInterpreter.InterpreterCapabilities) and
    fRunConfig.ReinitializeBeforeRun
  then begin
    ActiveInterpreter.ReInitialize;
    GI_PyInterpreter.ClearLastPrompt;
  end;
end;

function TPythonControl.PythonLoaded: Boolean;
begin
  Result := InternalPython.Loaded;
end;

procedure TPythonControl.SetRunConfig(ARunConfig: TRunConfiguration);
begin
  if ARunConfig <> fRunConfig then
  begin
    fRunConfig.Assign(ARunConfig);
    // Expand Parameters in filename
    fRunConfig.ScriptName := '';  // to avoid circular substitution
    fRunConfig.ScriptName := GI_PyIDEServices.ReplaceParams(ARunConfig.ScriptName);
    GI_PyIDEServices.SetRunLastScriptHints(fRunConfig.ScriptName);
  end;
end;

function TPythonControl.Running: boolean;
begin
  Result := fDebuggerState in [dsDebugging, dsRunning];
end;

procedure TPythonControl.LoadPythonEngine;
begin
  if InitPythonVersions then
    LoadPythonEngine(PythonVersion)
  else
    StyledMessageDlg(Format(_(SPythonLoadError), [MinPyVersion]), mtError, [mbOK], 0);
end;

procedure TPythonControl.LoadPythonEngine(const APythonVersion : TPythonVersion);
Var
  II : Variant;   // wrapping sys and code modules
begin
  if InternalPython.Loaded then
    GI_PyIDEServices.ClearPythonWindows;

  // Destroy Active debugger and interpreter
  PyControl.ActiveDebugger := nil;
  PyControl.ActiveInterpreter := nil;
  FreeAndNil(fInternalInterpreter);

  if InternalPython.LoadPython(APythonVersion) then
  begin
    fPythonHelpFile := APythonVersion.HelpFile;
    GI_PyInterpreter.PrintInterpreterBanner;

    // Create internal Interpreter and Debugger
    II := VarPythonEval('_II');
    InternalPython.PythonEngine.ExecString('del _II');

    fInternalInterpreter := TPyInternalInterpreter.Create(II);
    fActiveInterpreter := fInternalInterpreter;
    fActiveDebugger := TPyInternalDebugger.Create;

    // Allow threads
    TPythonThread.Py_Begin_Allow_Threads;

    // Execute python_init.py
    fInternalInterpreter.Initialize;

    // Execute pyscripter_init.py
    try
      fInternalInterpreter.RunScript(TPyScripterSettings.PyScripterInitFile);
    except
      on E: Exception do
        StyledMessageDlg(Format(_(SErrorInitScript),
          [TPyScripterSettings.PyScripterInitFile, E.Message]), mtError, [mbOK], 0);
    end;

    // Notify Python Version Change
    fOnPythonVersionChange.Notify(Self);

    //  Set the current PythonEngine
    PyControl.PythonEngineType := PyIDEOptions.PythonEngineType;

  end else
    StyledMessageDlg(Format(_(SPythonLoadError), [MinPyVersion]), mtError, [mbOK], 0);
end;

procedure TPythonControl.Run(ARunConfig: TRunConfiguration);
begin
  SetRunConfig(ARunConfig);

  if not Assigned(ActiveInterpreter) then Exit;

  PrepareRun;

  if fRunConfig.WriteOutputToFile then
    GI_PyInterpreter.StartOutputMirror(GI_PyIDEServices.ReplaceParams(fRunConfig.OutputFileName),
      fRunConfig.AppendToFile);
  try
    ActiveInterpreter.Run(fRunConfig);
  finally
    if fRunConfig.WriteOutputToFile then
      GI_PyInterpreter.StopFileMirror;
  end;
end;

function PythonVersionsKey: string;
begin
  {$IFDEF CPUX64}
     Result := 'PythonVersions-x64';
  {$ELSE}
     Result := 'PythonVersions-x86';
  {$ENDIF}
end;

procedure TPythonControl.ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
  out SysVersion, InstallPath: string);
Var
  Index : integer;
  CustomVersions : TStringList;
  Version : TPythonVersion;
  Path : string;
  Name : string;
  Count : integer;
begin
  if not AppStorage.PathExists(PythonVersionsKey+'\Custom Versions') then Exit;
  CustomVersions := TStringList.Create;
  try
    AppStorage.ReadStringList(PythonVersionsKey+'\Custom Versions', CustomVersions, True, 'Path');
    Count := 0;
    SetLength(CustomPythonVersions, CustomVersions.Count);
    for Index := 0 to  CustomVersions.Count -1  do
    begin
      Name := CustomVersions.Names[Index];
      if Name = '' then
        Path := CustomVersions[Index]
      else
         Path := CustomVersions.ValueFromIndex[Index];
      if PythonVersionFromPath(Path, Version, True,
        MinPyVersion, MaxPyVersion)
      then
      begin
        CustomPythonVersions[Count] := Version;
        if Name <> '' then
          CustomPythonVersions[Count].DisplayName := Name;
        Inc(Count);
      end;
    end;
    SetLength(CustomPythonVersions, Count);
  finally
    CustomVersions.Free;
  end;
  SysVersion := AppStorage.ReadString(PythonVersionsKey+'\SysVerion');
  InstallPath := AppStorage.ReadString(PythonVersionsKey+'\InstallPath');

  ActiveSSHServerName  := AppStorage.ReadString('SSHServer');
end;

function TPythonControl.RemoveCustomVersion(AIndex: Integer): Boolean;
begin
   Result := InRange(AIndex, 0, Length(CustomPythonVersions) - 1) and
     (FPythonVersionIndex <> -(AIndex + 1));  // Cannot delete active custom version
   if Result then
   begin
     Delete(CustomPythonVersions, AIndex, 1);
     if -FPythonVersionIndex > AIndex then
       Inc(FPythonVersionIndex);
   end;
end;

procedure TPythonControl.WriteToAppStorage(AppStorage: TJvCustomAppStorage);
Var
  CustomVersions : TStringList;
  Version : TPythonVersion;
begin
  AppStorage.DeleteSubTree(PythonVersionsKey);
  CustomVersions := TStringList.Create;
  try
    for Version in CustomPythonVersions do
      CustomVersions.Add(Version.DisplayName + CustomVersions.NameValueSeparator +  Version.InstallPath);
    AppStorage.WriteStringList(PythonVersionsKey+'\Custom Versions', CustomVersions, 'Path');
  finally
    CustomVersions.Free;
  end;

  if InternalPython.Loaded then begin
    if fPythonVersionIndex >= 0 then
      AppStorage.WriteString(PythonVersionsKey+'\SysVerion', PythonVersion.SysVersion)
    else
      AppStorage.WriteString(PythonVersionsKey+'\InstallPath', PythonVersion.InstallPath);
  end;

  AppStorage.WriteString('SSHServer', ActiveSSHServerName);
end;

initialization
  PyControl := TPythonControl.Create(nil);
finalization
  PyControl.fFinalizing := True;
  // Destroy Active debugger outside PyControl.Destory
  PyControl.ActiveDebugger := nil;
  PyControl.ActiveInterpreter := nil;
  FreeAndNil(PyControl);
end.
