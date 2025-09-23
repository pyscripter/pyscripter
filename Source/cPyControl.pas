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

uses
  System.Classes,
  System.Messaging,
  JvAppStorage,
  PythonVersions,
  uEditAppIntfs,
  cPySupportTypes,
  cPyBaseDebugger,
  cInternalPython;

type
  TPythonControl = class(TComponent, IPyControl)
  {
    Interface between PyScripter and the Interpreter/Debugger.
    Holds information Breakpoints, ErrorPos, CurrentPos
  }
  private
    FErrorPos: TEditorPos;
    FCurrentPos: TEditorPos;
    FFinalizing: Boolean;
    FDebuggerState: TDebuggerState;
    FOnStateChange: TNotifyEvent;
    FActiveInterpreter: TPyBaseInterpreter;
    FActiveDebugger: TPyBaseDebugger ;
    FRunConfig: TRunConfiguration;
    FPythonVersionIndex: Integer;
    FRegPythonVersions: TPythonVersions;
    FInternalPython: TInternalPython;
    FInternalInterpreter: TPyBaseInterpreter;
    FActiveSSHServerName: string;
    FPythonHelpFile: string;
    FProjectPythonPath: TArray<string>;
    function InitPythonVersions: Boolean;
    procedure SetActiveDebugger(const Value: TPyBaseDebugger);
    procedure SetActiveInterpreter(const Value: TPyBaseInterpreter);
    function GetPythonEngineType: TPythonEngineType;
    procedure SetPythonEngineType(const Value: TPythonEngineType);
    procedure SetRunConfig(ARunConfig: TRunConfiguration);
    procedure PrepareRun;
    function GetInternalInterpreter: TPyBaseInterpreter;
    procedure SetPythonVersionIndex(const Value: Integer);
    // IPyControl implementation
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

    procedure HandleProjectPythonPathChange(const Sender: TObject;
      const Msg: System.Messaging.TMessage);
  public
    const MinPyVersion = '3.8';
    const MaxPyVersion = '3.14';
  public
    // ActiveInterpreter and ActiveDebugger are created
    // and destroyed in frmPythonII
    CustomPythonVersions: TPythonVersions;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Running Python Scripts
    procedure Run(ARunConfig: TRunConfiguration);
    procedure Debug(ARunConfig: TRunConfiguration; InitStepIn: Boolean = False;
      RunToCursorLine: Integer = -1);
    procedure ExternalRun(ARunConfig: TRunConfiguration);
    // InternalPython
    procedure LoadPythonEngine; overload;
    procedure LoadPythonEngine(const APythonVersion: TPythonVersion); overload;
    // (Re)storing PythonVersions
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
      out SysVersion, InstallPath: string);
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage);
    // Custom versions
    function RemoveCustomVersion(AIndex: Integer): Boolean;
    // properties and events
    // PythonVersionIndex is the Index of Python version in the PYTHON_KNOWN_VERSIONS array
    property PythonVersion: TPythonVersion read GetPythonVersion;
    property PythonVersionIndex: Integer read FPythonVersionIndex write SetPythonVersionIndex;
    property RegPythonVersions: TPythonVersions read FRegPythonVersions;
    property PythonEngineType: TPythonEngineType read GetPythonEngineType
      write SetPythonEngineType;
    property ActiveSSHServerName: string read GetActiveSSHServerName write FActiveSSHServerName;
    property InternalPython: TInternalPython read FInternalPython;
    property InternalInterpreter: TPyBaseInterpreter read GetInternalInterpreter;
    property ActiveInterpreter: TPyBaseInterpreter read FActiveInterpreter
      write SetActiveInterpreter;
    property PythonHelpFile: string read FPythonHelpFile;
    property ActiveDebugger: TPyBaseDebugger read FActiveDebugger
      write SetActiveDebugger;
    property CurrentPos: TEditorPos read FCurrentPos write SetCurrentPos;
    property ErrorPos: TEditorPos read FErrorPos write SetErrorPos;
    property DebuggerState: TDebuggerState read FDebuggerState write SetDebuggerState;
    property RunConfig: TRunConfiguration read FRunConfig;
    property Finalizing: Boolean read FFinalizing;
    property OnStateChange: TNotifyEvent read FOnStateChange
      write FOnStateChange;
  end;

var
  PyControl: TPythonControl = nil;

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  System.Contnrs,
  System.UITypes,
  System.Math,
  System.Generics.Collections,
  System.Generics.Defaults,
  Vcl.Forms,
  Vcl.Dialogs,
  JvGnugettext,
  JvJVCLUtils,
  PythonEngine,
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

procedure TPythonControl.AppendProjectPaths;
begin
  if ActiveInterpreter = nil then Exit;

  for var Item in FProjectPythonPath do
    ActiveInterpreter.SysPathAdd(Item);
end;

constructor TPythonControl.Create(AOwner: TComponent);
begin
  inherited;
  GI_PyControl := Self;
  FDebuggerState := dsInactive;
  FCurrentPos.Clear;
  FErrorPos.Clear;
  FRunConfig := TRunConfiguration.Create;
  FInternalPython := TInternalPython.Create;
  FRegPythonVersions := GetRegisteredPythonVersions(MinPyVersion, MaxPyVersion);
  TMessageManager.DefaultManager.SubscribeToMessage(TProjectPythonPathChangeMessage,
    HandleProjectPythonPathChange);
end;

procedure TPythonControl.Debug(ARunConfig: TRunConfiguration;
  InitStepIn: Boolean = False; RunToCursorLine: Integer = -1);
begin
  SetRunConfig(ARunConfig);

  if not Assigned(ActiveDebugger) then Exit;

  PrepareRun;

  if FRunConfig.WriteOutputToFile then
    GI_PyInterpreter.StartOutputMirror(GI_PyIDEServices.ReplaceParams(FRunConfig.OutputFileName),
      FRunConfig.AppendToFile);
  try
    ActiveDebugger.Debug(FRunConfig, InitStepIn, RunToCursorLine);
  finally
    if FRunConfig.WriteOutputToFile then
      GI_PyInterpreter.StopFileMirror;
  end;
end;

destructor TPythonControl.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TProjectPythonPathChangeMessage,
    HandleProjectPythonPathChange);
  GI_PyControl := nil;
  FreeAndNil(FInternalInterpreter);
  FreeAndNil(FInternalPython);
  FRunConfig.Free;
  inherited;
end;

function TPythonControl.GetActiveSSHServerName: string;
begin
  Result := FActiveSSHServerName;
end;

function TPythonControl.GetCurrentPos: TEditorPos;
begin
  Result := FCurrentPos;
end;

function TPythonControl.GetDebuggerState: TDebuggerState;
begin
  Result := FDebuggerState;
end;

function TPythonControl.GetErrorPos: TEditorPos;
begin
  Result := FErrorPos;
end;

function TPythonControl.GetInternalInterpreter: TPyBaseInterpreter;
begin
  Result := FInternalInterpreter;
  if not (InternalPython.Loaded and Assigned(FInternalInterpreter)) then
  begin
    StyledMessageDlg(_(SInterpreterNA), mtError, [mbAbort], 0);
    Abort;
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
  if (FPythonVersionIndex >= 0) and (FPythonVersionIndex < Length(FRegPythonVersions)) then
    Result := FRegPythonVersions[FPythonVersionIndex]
  else if (FPythonVersionIndex < 0) and (-FPythonVersionIndex <= Length(CustomPythonVersions)) then
    Result := CustomPythonVersions[-FPythonVersionIndex -1]
  else
    Assert(False, 'Invalid PythonVersionIndex');
end;

procedure TPythonControl.HandleProjectPythonPathChange(const Sender: TObject;
  const Msg: System.Messaging.TMessage);
begin
  if ActiveInterpreter = nil then Exit;

  var NewPath := TProjectPythonPathChangeMessage(Msg).Value;
  var Comparer: IComparer<string> := TIStringComparer.Ordinal;

  // First remove old entries
  for var Item in FProjectPythonPath do
    if not TArray.Contains<string>(NewPath, Item, Comparer) then
      ActiveInterpreter.SysPathRemove(Item);
  FProjectPythonPath := NewPath;
  // Add the new entries
  AppendProjectPaths;
end;

function TPythonControl.Inactive: Boolean;
begin
  Result := InternalPython.Loaded and (FDebuggerState = dsInactive);
end;

function TPythonControl.InitPythonVersions: Boolean;
var
  ExpectedVersion,
  LastVersion,
  LastInstallPath,
  DLLPath: string;
  Version: TPythonVersion;
begin
  // first find an optional parameter specifying the expected Python version in the form of -PYTHONXY
  ExpectedVersion := '';

  if CmdLineReader.readFlag('PYTHON38') then
    ExpectedVersion := '3.8'
  else if CmdLineReader.readFlag('PYTHON39') then
    ExpectedVersion := '3.9'
  else if CmdLineReader.readFlag('PYTHON310') then
    ExpectedVersion := '3.10'
  else if CmdLineReader.readFlag('PYTHON311') then
    ExpectedVersion := '3.11'
  else if CmdLineReader.readFlag('PYTHON312') then
    ExpectedVersion := '3.12'
  else if CmdLineReader.readFlag('PYTHON313') then
    ExpectedVersion := '3.13'
  else if CmdLineReader.readFlag('PYTHON314') then
    ExpectedVersion := '3.14';
  DLLPath := CmdLineReader.readString('PYTHONDLLPATH');

  ReadFromAppStorage(GI_PyIDEServices.LocalAppStorage, LastVersion, LastInstallPath);
  if (DLLPath = '') and (ExpectedVersion = '') then
  begin
    ExpectedVersion := LastVersion;
    DLLPath := LastInstallPath;
  end;

  Result := False;
  if (DLLPath = '') and (ExpectedVersion = '') then
  begin
    if Length(FRegPythonVersions) > 0 then begin
      FPythonVersionIndex := 0;
      Result := True;
    end;
  end
  else if DLLPath = '' then
  begin
    for var I := 0 to Length(FRegPythonVersions) - 1 do
      if FRegPythonVersions[I].SysVersion = ExpectedVersion then
      begin
        FPythonVersionIndex := I;
        Result := True;
        Break;
      end;
    // if the ExpectedVersion is not available load the latest registred version
    if not Result and (Length(FRegPythonVersions) > 0) then
    begin
      FPythonVersionIndex := 0;
      Result := True;
    end;
  end
  else
  begin
    for var I := 0 to Length(CustomPythonVersions) -1 do
      if (CustomPythonVersions[I].DLLPath = DLLPath) or
         (CustomPythonVersions[I].InstallPath = DLLPath) then
      begin
        Result := True;
        FPythonVersionIndex := -(I + 1);
        Break;
      end;
    if not Result then begin
      Result := PythonVersionFromPath(DLLPath, Version, True,
        MinPyVersion, MaxPyVersion);
      if Result then begin
        SetLength(CustomPythonVersions, Length(CustomPythonVersions) + 1);
        CustomPythonVersions[Length(CustomPythonVersions)-1] := Version;
        FPythonVersionIndex := - Length(CustomPythonVersions);
      end;
    end;
    // if the loading from path fails load the latest registered version
    if not Result and (Length(FRegPythonVersions) > 0) then
    begin
      FPythonVersionIndex := 0;
      Result := True;
    end;
  end;
end;

procedure TPythonControl.SetActiveDebugger(const Value: TPyBaseDebugger);
begin
  if FActiveDebugger <> Value then begin
    if Assigned(FActiveDebugger) then
    begin
      var Py := SafePyEngine;
      FreeAndNil(FActiveDebugger);
    end;
    FActiveDebugger := Value;
  end;
end;

procedure TPythonControl.SetActiveInterpreter(const Value: TPyBaseInterpreter);
begin
  if FActiveInterpreter <> Value then begin
    if Assigned(FActiveInterpreter) and
      (FActiveInterpreter <> FInternalInterpreter)
    then
    begin
      var Py := SafePyEngine;
      FreeAndNil(FActiveInterpreter);
    end;
    FActiveInterpreter := Value;
  end;
end;

procedure TPythonControl.SetPythonEngineType(const Value: TPythonEngineType);
var
  Cursor: IInterface;
  RemoteInterpreter: TPyRemoteInterpreter;
  Connected: Boolean;
  SSHServer: TSSHServer;
begin
  if not InternalPython.Loaded or ((Value = PythonEngineType) and not
   ((Value = peSSH) and (ActiveInterpreter is TPySSHInterpreter) and
    (ActiveSSHServerName <> TPySSHInterpreter(ActiveInterpreter).SSHServerName)))
  then
    Exit;

  if DebuggerState <> dsInactive then
  begin
    StyledMessageDlg(_(SCannotChangeEngine), mtError, [mbAbort], 0);
    Exit;
  end;

  var Py := SafePyEngine;

  GI_PyIDEServices.ClearPythonWindows;

  case Value of
    peInternal:
      begin
        ActiveInterpreter := FInternalInterpreter;
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
        if Connected then
        begin
          ActiveInterpreter := RemoteInterpreter;
          ActiveDebugger := ActiveInterpreter.CreateDebugger;
          PyIDEOptions.PythonEngineType := Value;
        end
        else
        begin
          // failed to connect - fall back to the internal engine
          FreeAndNil(RemoteInterpreter);
          ActiveInterpreter := FInternalInterpreter;
          ActiveDebugger := ActiveInterpreter.CreateDebugger;
          PyIDEOptions.PythonEngineType := peInternal;
        end;
      end;
  end;

  AppendProjectPaths;
  ActiveInterpreter.Initialize;
end;

procedure TPythonControl.SetPythonVersionIndex(const Value: Integer);
begin
  if (Value <> FPythonVersionIndex) and (DebuggerState = dsInactive) then begin
    FPythonVersionIndex := Value;
    LoadPythonEngine(PythonVersion);
  end;
end;

procedure CurrentPosChanged(OldPos, NewPos: TEditorPos);
begin
  if GI_PyIDEServices.IsClosing  then Exit;

  GI_EditorFactory.InvalidatePos(OldPos.FileName, OldPos.Line, itBoth);
  if NewPos.IsValid and
    GI_PyIDEServices.ShowFilePosition(NewPos.FileName, NewPos.Line, 1, 0, True, False)
  then
    GI_EditorFactory.InvalidatePos(NewPos.FileName, NewPos.Line, itBoth);
end;

procedure TPythonControl.SetCurrentPos(const NewPos: TEditorPos);
begin
  if NewPos.PointsTo(FCurrentPos.FileName, FCurrentPos.Line) then Exit;

  var OldPos := FCurrentPos;
  FCurrentPos := NewPos;

  if (GetCurrentThreadId = MainThreadID) then
    CurrentPosChanged(OldPos, NewPos)
  else
    TThread.Synchronize(nil, procedure
    begin
      CurrentPosChanged(OldPos, NewPos);
    end);
  FCurrentPos := NewPos;
end;

procedure ErrorPosChanged(OldPos, NewPos: TEditorPos);
begin
  if GI_PyIDEServices.IsClosing  then Exit;

  GI_EditorFactory.InvalidatePos(OldPos.FileName, OldPos.Line, itLine);
  if NewPos.IsValid and
    GI_PyIDEServices.ShowFilePosition(NewPos.FileName, NewPos.Line)
  then
    GI_EditorFactory.InvalidatePos(NewPos.FileName, NewPos.Line, itLine);
end;

procedure TPythonControl.SetErrorPos(const NewPos: TEditorPos);
begin
  var OldPos := FErrorPos;
  FErrorPos := NewPos;

  if (GetCurrentThreadId = MainThreadID) then
    ErrorPosChanged(OldPos, NewPos)
  else
    TThread.Synchronize(nil, procedure
    begin
      ErrorPosChanged(OldPos, NewPos);
    end);
  FErrorPos := NewPos;
end;

procedure TPythonControl.SetDebuggerState(const NewState: TDebuggerState);
var
  OldDebuggerState: TDebuggerState;
begin
  OldDebuggerState := FDebuggerState;
  if NewState in [dsInactive, dsDebugging] then
    CurrentPos := TEditorPos.EmptyPos
  else
    ErrorPos := TEditorPos.EmptyPos;

  FDebuggerState := NewState;

  if Assigned(FOnStateChange) then
    TThread.Queue(nil, procedure
    begin
      FOnStateChange(Self)
    end);

  if Assigned(ActiveInterpreter) and ActiveInterpreter.Initialized then
  begin
    if (GetCurrentThreadId = MainThreadID) then
      GI_CallStackWindow.UpdateWindow(NewState, OldDebuggerState)  // also updates Variables and Watches
    else
    begin
      TPythonThread.Py_Begin_Allow_Threads;
      try
        TThread.Synchronize(nil, procedure
        begin
          GI_CallStackWindow.UpdateWindow(NewState, OldDebuggerState);  // also updates Variables and Watches
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
  FRunConfig.ExternalRun.Execute;
end;

procedure TPythonControl.Pickle(AValue: Variant; FileName: string);
begin
  ActiveInterpreter.Pickle(AValue, FileName);
end;

procedure TPythonControl.PrepareRun;
var
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

  if (FRunConfig.EngineType <> PythonEngineType) or ((PythonEngineType = peSSH) and
    TSSHFileName.Parse(FRunConfig.ScriptName, Server, FName) and (Server <> ActiveSSHServerName))
  then begin
    if Server <> '' then
      ActiveSSHServerName := Server;
    PythonEngineType := FRunConfig.EngineType;
  end else if (icReInitialize in ActiveInterpreter.InterpreterCapabilities) and
    FRunConfig.ReinitializeBeforeRun
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
  if ARunConfig <> FRunConfig then
  begin
    FRunConfig.Assign(ARunConfig);
    // Expand Parameters in filename
    FRunConfig.ScriptName := '';  // to avoid circular substitution
    FRunConfig.ScriptName := GI_PyIDEServices.ReplaceParams(ARunConfig.ScriptName);
    GI_PyIDEServices.SetRunLastScriptHints(FRunConfig.ScriptName);
  end;
end;

function TPythonControl.Running: Boolean;
begin
  Result := FDebuggerState in [dsDebugging, dsRunning];
end;

procedure TPythonControl.LoadPythonEngine;
begin
  if InitPythonVersions then
    LoadPythonEngine(PythonVersion)
  else
    StyledMessageDlg(Format(_(SPythonLoadError), [MinPyVersion]), mtError, [mbOK], 0);
end;

procedure TPythonControl.LoadPythonEngine(const APythonVersion: TPythonVersion);
begin
  if InternalPython.Loaded then
    GI_PyIDEServices.ClearPythonWindows;

  // Destroy Active debugger and interpreter
  PyControl.ActiveDebugger := nil;
  PyControl.ActiveInterpreter := nil;
  FreeAndNil(FInternalInterpreter);

  if InternalPython.LoadPython(APythonVersion) then
  begin
    FPythonHelpFile := APythonVersion.HelpFile;

    var II := VarPythonEval('_II'); // wrapping sys and code modules
    // Create internal Interpreter and Debugger
    InternalPython.PythonEngine.ExecString('del _II');

    FInternalInterpreter := TPyInternalInterpreter.Create(II);
    FActiveInterpreter := FInternalInterpreter;
    FActiveDebugger := TPyInternalDebugger.Create;

    // Allow threads
    TPythonThread.Py_Begin_Allow_Threads;

    // Execute pyscripter_init.py
    try
      FInternalInterpreter.RunScript(TPyScripterSettings.PyScripterInitFile);
    except
      on E: Exception do
        StyledMessageDlg(Format(_(SErrorInitScript),
          [TPyScripterSettings.PyScripterInitFile, E.Message]), mtError, [mbOK], 0);
    end;

    // Notify Python Version Change
    TMessageManager.DefaultManager.SendMessage(Self, TPythonVersionChangeMessage.Create);

    //  Set the current PythonEngine
    if PyIDEOptions.PythonEngineType = peInternal then
      FInternalInterpreter.Initialize
    else
      PyControl.PythonEngineType := PyIDEOptions.PythonEngineType;

    GI_PyInterpreter.PrintInterpreterBanner;
    GI_PyInterpreter.PrintEngineType;
  end
  else
    StyledMessageDlg(Format(_(SPythonLoadError), [MinPyVersion]), mtError, [mbOK], 0);
end;

procedure TPythonControl.Run(ARunConfig: TRunConfiguration);
begin
  SetRunConfig(ARunConfig);

  if not Assigned(ActiveInterpreter) then Exit;

  PrepareRun;

  if FRunConfig.WriteOutputToFile then
    GI_PyInterpreter.StartOutputMirror(GI_PyIDEServices.ReplaceParams(FRunConfig.OutputFileName),
      FRunConfig.AppendToFile);
  try
    ActiveInterpreter.Run(FRunConfig);
  finally
    if FRunConfig.WriteOutputToFile then
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
var
  Index: Integer;
  CustomVersions: TStringList;
  Version: TPythonVersion;
  Path: string;
  Name: string;
  Count: Integer;
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
var
  CustomVersions: TStringList;
  Version: TPythonVersion;
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
    if FPythonVersionIndex >= 0 then
      AppStorage.WriteString(PythonVersionsKey+'\SysVerion', PythonVersion.SysVersion)
    else
      AppStorage.WriteString(PythonVersionsKey+'\InstallPath', PythonVersion.InstallPath);
  end;

  AppStorage.WriteString('SSHServer', ActiveSSHServerName);
end;

initialization
  PyControl := TPythonControl.Create(nil);
finalization
  PyControl.FFinalizing := True;
  // Destroy Active debugger outside PyControl.Destory
  PyControl.ActiveDebugger := nil;
  PyControl.ActiveInterpreter := nil;
  FreeAndNil(PyControl);
end.
