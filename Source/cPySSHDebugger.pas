{-----------------------------------------------------------------------------
 Unit Name: cPySSHDebugger
 Author:    Kiriakos Vlahos
 Date:      5-September-2018
 Purpose:   Debugger run in a remote using an Rpyc based server
            connected via an SSH tunnel
 History:
 -----------------------------------------------------------------------------}

unit cPySSHDebugger;

interface

uses
  WinApi.Windows,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Threading,
  JclSysUtils,
  PythonEngine,
  cPyBaseDebugger,
  cPyDebugger,
  cPyRemoteDebugger,
  cSSHSupport;

type

  TPySSHInterpreter = class(TPyRemoteInterpreter)
  { Rpyc based remote Python Interpreter running on an SSH server }
  private
    fIs3K : Boolean;
    fRemServerFile : string;
    fRemRpycFile : string;
    TunnelProcessOptions : TJclExecuteCmdProcessOptions;
    TunnelProcessInfo: TProcessInformation;
    TunnelTask : ITask;
    fShuttingDown : Boolean;
    function ProcessPlatformInfo(Info: string; out Is3k: Boolean;
      out Sep: Char; out TempDir: string): boolean;
    procedure StoreTunnelProcessInfo(const ProcessInfo: TProcessInformation);
  protected
    function SystemTempFolder: string; override;
    procedure CreateAndRunServerProcess; override;
    procedure ShutDownServer;  override;
  public
    SSHServerName : string;
    HostName : string;
    UserName : string;
    PythonCommand : string;
    PathSeparator : Char;
    TempDir : string;
    PythonVersion : string;
    RemotePlatform : string;
    function IsPython3000 : Boolean; override;
    function ToPythonFileName(const FileName: string): string; override;
    function FromPythonFileName(const FileName: string): string; override;

    constructor Create(SSHServer : TSSHServer);
    destructor Destroy; override;
  end;

  TPySSHDebugger = class(TPyRemDebugger)
  { Rpyc based remote Python Debugger running on an SSH server }
  public
    procedure Abort; override;
  end;

implementation

Uses
  Vcl.Dialogs,
  Vcl.Forms,
  StringResources,
  cPySupportTypes,
  cPyScripterSettings,
  System.StrUtils,
  uCommonFunctions,
  cPyControl;

{ TPySSHInterpreter }

constructor TPySSHInterpreter.Create(SSHServer : TSSHServer);
{
   1. ssh kiria@192.168.1.5 pythoncommand -c '"import sys,os,tempfile;print(sys.version[0]);print(os.sep);print(tempfile.gettempdir())"'
   2. scp server script, rpyc.zip
   3. Start python server process
   4. start port tunneling process ssh username@hostname -L 127.0.0.1:port:127.0.0.1:port -N
   5. connect to server
}
Var
  CommandOutput : string;
  Task : ITask;
begin
  fServerIsAvailable := False;
  fConnected := False;
  SSHServerName := SSHServer.Name;
  UserName := SSHServer.UserName;
  HostName := SSHServer.HostName;
  PythonCommand := SSHServer.PythonCommand;
  //  Test SSH connection and get information about the server
  Task := TTask.Create(procedure
  begin
    fServerIsAvailable := Execute(Format('ssh %s %s@%s %s -c ' +
    '''"import sys,os,tempfile;print(sys.version[0]);print(os.sep);print(tempfile.gettempdir())"''',
    [SshCommandOptions, UserName, HostName, PythonCommand]), CommandOutput) = 0
  end);
  Task.Start;

  if Task.Wait(3000) and fServerIsAvailable then
    fServerIsAvailable := ProcessPlatformInfo(CommandOutput, fIs3K, PathSeparator, TempDir);

  if not fServerIsAvailable then begin
    Vcl.Dialogs.MessageDlg(Format(SSSHPythonError, [PythonCommand]), mtError, [mbAbort], 0);
    Exit;
  end;

  // Check for version mismatch
  if IsPython3000 <> GetPythonEngine.IsPython3000 then
  begin
    Vcl.Dialogs.MessageDlg(Format(SSHVersionMismatch,
      [IfThen(GetPythonEngine.IsPython3000, '3.x', '2,x'), IfThen(IsPython3000, '3.x', '2,x')]),
      mtError, [mbAbort], 0);
    Exit;
  end;

  TunnelProcessOptions := TJclExecuteCmdProcessOptions.Create('');
  TunnelProcessOptions.BeforeResume := StoreTunnelProcessInfo;
  TunnelProcessOptions.MergeError := True;
  TunnelProcessOptions.CreateProcessFlags := 
    TunnelProcessOptions.CreateProcessFlags and CREATE_NO_WINDOW;

  inherited Create(peSSH);
  DebuggerClass := TPySSHDebugger;
  if fConnected then begin
    PythonVersion := Conn.modules.sys.version;
    RemotePlatform := Conn.modules.sys.platform;
  end;
end;

procedure TPySSHInterpreter.CreateAndRunServerProcess;
Var
  ErrorMsg : string;
begin
  fShuttingDown := False;
  fServerIsAvailable := False;
  // Upload server and rpyc files
  fRemServerFile := TempDir + PathSeparator + RemoteServerBaseName;
  if not ScpUpload(SSHServerName, fServerFile, fRemServerFile, ErrorMsg) then
    fRemServerFile := ''
  else begin
    fRemRpycFile :=  TempDir + PathSeparator + RpycZipModule;
    if not ScpUpload(SSHServerName, fRpycPath, fRemRpycFile, ErrorMsg) then
      fRemRpycFile := '';
  end;

  if (fRemServerFile = '') or (fRemRpycFile = '') then
  begin
    Vcl.Dialogs.MessageDlg(ErrorMsg, mtError, [mbAbort], 0);
    Exit;
  end;

  // Find a new port
  Randomize;
  fSocketPort := 18000 + Random(1000);

  // Create and Run Server process
  ServerProcessOptions.CommandLine := Format('ssh -n %s %s@%s %s ',
    [SshCommandOptions, UserName, HostName, PythonCommand]) +
    Format('"%s" %d "%s"', [fRemServerFile, fSocketPort, fRemRpycFile]);
  ServerTask := TTask.Create(procedure
    begin
      ExecuteCmdProcess(ServerProcessOptions);
    end).Start;
  Sleep(100);
  fServerIsAvailable := ServerTask.Status = TTaskStatus.Running;

  Application.ProcessMessages;
  if not fServerIsAvailable then Exit;

  TunnelProcessOptions.CommandLine := Format('ssh -n %s %s@%s -L 127.0.0.1:%d:127.0.0.1:%3:d -N',
    [SshCommandOptions, UserName, HostName, fSocketPort]);
  TunnelTask := TTask.Create(procedure
    begin
      ExecuteCmdProcess(TunnelProcessOptions);
    end).Start;
  Sleep(100);
  fServerIsAvailable := TunnelTask.Status = TTaskStatus.Running;
end;

destructor TPySSHInterpreter.Destroy;
begin
  inherited;
  FreeAndNil(TunnelProcessOptions);
end;

function TPySSHInterpreter.IsPython3000: Boolean;
begin
  Result := fIs3K;
end;

function TPySSHInterpreter.ProcessPlatformInfo(Info: string; out Is3k: Boolean;
  out Sep: Char; out TempDir: string): boolean;
Var
  SL : TStrings;
begin
  Result := True;
  SL := TStringList.Create;
  try
    SL.Text := Info;
    if SL.Count < 3 then Exit(False);

    // Python version
    if SL[0] = '2' then Is3k := False
    else if SL[0] = '3' then Is3k := True
    else Exit(False);
    // Path Separator
    if (SL[1] = '\') or (SL[1] = '/') then
      Sep := SL[1][1]
    else
      Exit(False);
    // TempDir
    TempDir := SL[2];
    if TempDir = '' then Exit(False);
    if TempDir[Length(TempDir)] = Sep then
      Delete(TempDir, TempDir.Length, 1);
  finally
    SL.Free;
  end;

end;

procedure TPySSHInterpreter.ShutDownServer;
Var
  CommandOutput : string;
begin
  fShuttingDown := True;
  inherited;
  // Delete temp files
  if (fRemServerFile <> '') and (Execute(Format('ssh %s %s@%s rm %s',
      [SshCommandOptions, UserName, HostName, fRemServerFile]), CommandOutput) = 0) 
  then
    fRemServerFile := '';
  if (fRemRpycFile <> '')  and (Execute(Format('ssh %s %s@%s rm %s',
      [SshCommandOptions, UserName, HostName, fRemRpycFile]), CommandOutput) = 0)
  then
    fRemRpycFile := '';
  // shut down tunnel
  if Assigned(TunnelTask) and (TunnelTask.Status = TTaskStatus.Running) then begin
    RaiseKeyboardInterrupt(TunnelProcessInfo.dwProcessId);
    Sleep(100);
    if TunnelTask.Status = TTaskStatus.Running then
      TerminateProcessTree(TunnelProcessInfo.dwProcessId);
  end;
  TunnelTask := nil;
end;

procedure TPySSHInterpreter.StoreTunnelProcessInfo(
  const ProcessInfo: TProcessInformation);
begin
  TunnelProcessInfo := ProcessInfo;
end;

function TPySSHInterpreter.SystemTempFolder: string;
begin
  Result := TempDir;
end;

function TPySSHInterpreter.ToPythonFileName(const FileName: string): string;
Var
  Server, FName : string;
begin
  if TUnc.Parse(FileName, Server, FName) and (Server = SSHServerName) then
    Result := FName
  else
    Result := '<' + FileName + '>';
end;

function TPySSHInterpreter.FromPythonFileName(const FileName: string): string;
begin
  if FileName = '' then
    Result := ''
  else if FileName[1] ='<' then
    Result := FileName
  else
    Result := TUnc.Format(SSHServerName, FileName);
end;

{ TPySSHDebugger }

procedure TPySSHDebugger.Abort;
begin
  case PyControl.DebuggerState of
    dsPostMortem: ExitPostMortem;
    dsDebugging,
    dsRunning: 
      begin
        if not TPySSHInterpreter(fRemotePython).fShuttingDown then
          TThread.ForceQueue(nil, procedure
          begin
            TPySSHInterpreter(fRemotePython).Reinitialize;
          end);
      end;
    dsPaused:
      begin
        fDebuggerCommand := dcAbort;
        DoDebuggerCommand;
      end;
  end;
end;

end.
