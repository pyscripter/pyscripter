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
  JclNotify,
  JvAppStorage,
  PythonVersions,
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

  TPythonControl = class(TObject)
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
    fOnPythonVersionChange: TJclNotifyEventBroadcast;
    fActiveInterpreter : TPyBaseInterpreter;
    fActiveDebugger : TPyBaseDebugger ;
    fRunConfig : TRunConfiguration;
    fPythonVersionIndex : integer;
    fRegPythonVersions : TPythonVersions;
    fInternalPython : TInternalPython;
    fInternalInterpreter : TPyBaseInterpreter;
    fActiveSSHServerName : string;
    function InitPythonVersions : Boolean;
    procedure DoOnBreakpointChanged(Editor : IEditor; ALine: integer);
    procedure SetActiveDebugger(const Value: TPyBaseDebugger);
    procedure SetActiveInterpreter(const Value: TPyBaseInterpreter);
    function GetPythonEngineType: TPythonEngineType;
    procedure SetPythonEngineType(const Value: TPythonEngineType);
    procedure SetRunConfig(ARunConfig: TRunConfiguration);
    procedure PrepareRun;
    function GetInternalInterpreter: TPyBaseInterpreter;
    function GetPythonVersion: TPythonVersion;
    procedure SetPythonVersionIndex(const Value: integer);
  public
    // ActiveInterpreter and ActiveDebugger are created
    // and destroyed in frmPythonII
    ErrorPos: TEditorPos;
    CurrentPos: TEditorPos;
    CustomPythonVersions: TPythonVersions;

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
    function Running: boolean;
    function Inactive: boolean;
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

    // properties and events
    // PythonVersionIndex is the Index of Python version in the PYTHON_KNOWN_VERSIONS array
    property PythonVersion : TPythonVersion read GetPythonVersion;
    property PythonVersionIndex : integer read fPythonVersionIndex write SetPythonVersionIndex;
    property RegPythonVersions : TPythonVersions read fRegPythonVersions;
    property PythonEngineType : TPythonEngineType read GetPythonEngineType
      write SetPythonEngineType;
    property ActiveSSHServerName : string read fActiveSSHServerName write fActiveSSHServerName;
    property InternalPython : TInternalPython read fInternalPython;
    property InternalInterpreter : TPyBaseInterpreter read GetInternalInterpreter;
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
    property OnPythonVersionChange: TJclNotifyEventBroadcast read fOnPythonVersionChange;
  end;

var
  PyControl : TPythonControl = nil;

implementation

uses
  WinApi.Windows,
  System.SysUtils,
  System.Contnrs,
  System.UITypes,
  Vcl.Forms,
  Vcl.Dialogs,
  JvGnugettext,
  JvJVCLUtils,
  VarPyth,
  StringResources,
  dmCommands,
  frmPythonII,
  frmPyIDEMain,
  frmCommandOutput,
  frmVariables,
  frmUnitTests,
  uCmdLine,
  cPyScripterSettings,
  cParameters,
  cPyDebugger,
  cPyRemoteDebugger,
  cProjectClasses,
  cRefactoring,
  cSSHSupport,
  cPySSHDebugger;

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
  fInternalPython := TInternalPython.Create;
  fRegPythonVersions := GetRegisteredPythonVersions;
  fOnPythonVersionChange := TJclNotifyEventBroadcast.Create;
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
    Vcl.Dialogs.MessageDlg(_(SInterpreterNA), mtError, [mbAbort], 0);
    Abort;
  end;
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

function TPythonControl.GetPythonVersion: TPythonVersion;
begin
  if (fPythonVersionIndex >= 0) and (fPythonVersionIndex < Length(fRegPythonVersions)) then
    Result := fRegPythonVersions[fPythonVersionIndex]
  else if (fPythonVersionIndex < 0) and (-fPythonVersionIndex <= Length(CustomPythonVersions)) then
    Result := CustomPythonVersions[-fPythonVersionIndex -1]
  else
    Assert(False, 'Invalide PythonVersionIndex');
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

  if CmdLineReader.readFlag('PYTHON26') then
    expectedVersion := '2.6'
  else if CmdLineReader.readFlag('PYTHON27') then
    expectedVersion := '2.7'
  else if CmdLineReader.readFlag('PYTHON30') then
    expectedVersion := '3.0'
  else if CmdLineReader.readFlag('PYTHON31') then
    expectedVersion := '3.1'
  else if CmdLineReader.readFlag('PYTHON32') then
    expectedVersion := '3.2'
  else if CmdLineReader.readFlag('PYTHON33') then
    expectedVersion := '3.3'
  else if CmdLineReader.readFlag('PYTHON34') then
    expectedVersion := '3.4'
  else if CmdLineReader.readFlag('PYTHON35') then
    expectedVersion := '3.5'
  else if CmdLineReader.readFlag('PYTHON36') then
    expectedVersion := '3.6'
  else if CmdLineReader.readFlag('PYTHON37') then
    expectedVersion := '3.7';
  DllPath := CmdLineReader.readString('PYTHONDLLPATH');

  ReadFromAppStorage(PyIDEMainForm.LocalAppStorage, LastVersion, LastInstallPath);
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
      Result := PythonVersionFromPath(DLLPath, Version);
      if Result then begin
        SetLength(CustomPythonVersions, Length(CustomPythonVersions) + 1);
        CustomPythonVersions[Length(CustomPythonVersions)-1] := Version;
        fPythonVersionIndex := - Length(CustomPythonVersions);
      end;
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
      (fActiveInterpreter <> fInternalInterpreter)
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
  SSHServer: TSSHServer;
begin
  if not InternalPython.Loaded or ((Value = PythonEngineType) and not
   ((Value = peSSH) and (ActiveInterpreter is TPySSHInterpreter) and
    (ActiveSSHServerName <> TPySSHInterpreter(ActiveInterpreter).SSHServerName)))
  then
    Exit;

  if DebuggerState <> dsInactive then begin
    Vcl.Dialogs.MessageDlg(_(SCannotChangeEngine), mtError, [mbAbort], 0);
    Exit;
  end;

  VariablesWindow.ClearAll;
  UnitTestWindow.ClearAll;

  case Value of
    peInternal:
      begin
        ActiveInterpreter := fInternalInterpreter;
        ActiveDebugger := TPyInternalDebugger.Create;
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
            Vcl.Dialogs.MessageDlg(Format(_(SSHUnknownServer), [ActiveSSHServerName]),
              mtError, [mbAbort],0);
            Exit;
          end;
        end;
        Application.ProcessMessages;
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
          if Value = peSSH then
            ActiveDebugger := TPySSHDebugger.Create(RemoteInterpreter)
          else
            ActiveDebugger := TPyRemDebugger.Create(RemoteInterpreter);
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
    peInternal :  Msg := Format(_(SEngineActive), ['Internal','']);
    peRemote : Msg := Format(_(SEngineActive), ['Remote','']);
    peRemoteTk : Msg := Format(_(SEngineActive), ['Remote','(Tkinter) ']);
    peRemoteWx : Msg := Format(_(SEngineActive), ['Remote','(wxPython) ']);
    peSSH : Msg := Format(_(SEngineActive), ['SSH', Format('"%s" ', [ActiveSSHServerName])]);
  end;
  with PythonIIForm do begin
    if SynEdit.Lines[SynEdit.Lines.Count-1] = PS1 then
      SynEdit.Lines.Delete(SynEdit.Lines.Count -1);
    AppendText(sLineBreak + Msg);
    if PyIDEOptions.PythonEngineType = peSSH then with ActiveInterpreter as TPySSHInterpreter do
      PrintInterpreterBanner(PythonVersion, RemotePlatform);
    AppendPrompt;
  end;

  DoStateChange(dsInactive);
end;

procedure TPythonControl.SetPythonVersionIndex(const Value: integer);
begin
  if (Value <> fPythonVersionIndex) and (DebuggerState = dsInactive) then begin
    fPythonVersionIndex := Value;
    LoadPythonEngine(PythonVersion);
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

function TPythonControl.Running: boolean;
begin
  Result := fDebuggerState in [dsDebugging, dsRunning];
end;

procedure TPythonControl.LoadPythonEngine;
begin
  if InitPythonVersions then
    LoadPythonEngine(PythonVersion)
  else
    Vcl.Dialogs.MessageDlg(_(SPythonLoadError), mtError, [mbOK], 0);
end;

procedure TPythonControl.LoadPythonEngine(const APythonVersion : TPythonVersion);
Var
  II : Variant;   // wrapping sys and code modules
  FileName : String;
begin
  if InternalPython.Loaded then
  begin
    VariablesWindow.ClearAll;
    PyScripterRefactor.ClearProxyModules;
  end;

  // Destroy Active debugger and interpreter
  PyControl.ActiveDebugger := nil;
  PyControl.ActiveInterpreter := nil;
  FreeAndNil(fInternalInterpreter);

  if InternalPython.LoadPython(APythonVersion) then
  begin
    PythonIIForm.PythonHelpFile := APythonVersion.HelpFile;
    PythonIIForm.PrintInterpreterBanner;

    // Create internal Interpreter and Debugger
    II := VarPythonEval('_II');
    InternalPython.PythonEngine.ExecString('del _II');

    fInternalInterpreter := TPyInternalInterpreter.Create(II);
    fActiveInterpreter := fInternalInterpreter;
    fActiveDebugger := TPyInternalDebugger.Create;
    fInternalInterpreter.Initialize;

    // Execute pyscripter_init.py
    FileName := CommandsDataModule.UserDataPath + PyScripterInitFile;
    try
     fInternalInterpreter.RunScript(FileName);
    except
      on E: Exception do
        Vcl.Dialogs.MessageDlg(Format(_(SErrorInitScript),
          [PyScripterInitFile, E.Message]), mtError, [mbOK], 0);
    end;

    // Notify Python Version Change
    fOnPythonVersionChange.Notify(Self);

    //  Set the current PythonEngine
    PyControl.PythonEngineType := PyIDEOptions.PythonEngineType;
  end else
    Vcl.Dialogs.MessageDlg(_(SPythonLoadError), mtError, [mbOK], 0);
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

// IJvAppStorageHandler implementation
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
      if PythonVersionFromPath(Path, Version) then
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
  PyControl := TPythonControl.Create;
finalization
  // Destroy Active debugger outside PyControl.Destory
  PyControl.ActiveDebugger := nil;
  PyControl.ActiveInterpreter := nil;
  FreeAndNil(PyControl);
end.
