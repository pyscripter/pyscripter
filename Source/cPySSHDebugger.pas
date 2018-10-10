{-----------------------------------------------------------------------------
 Unit Name: cPySSHDebugger
 Author:    Kiriakos Vlahos
 Date:      5-September-2018
 Purpose:   Debugger run in a remote using an Rpyc based server
            connected via an SSH tunnel
 History:
 -----------------------------------------------------------------------------}
//  ssh username@hostname -L 127.0.0.1:port:127.0.0.1:port -N
//  ssh kiria@192.168.1.5 /c/python/python36/python -c '"import tempfile;print(tempfile.gettempdir())"'


unit cPySSHDebugger;

interface

uses
  WinApi.Windows,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  JvCreateProcess,
  PythonEngine,
  uEditAppIntfs,
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
    function ProcessPlatformInfo(Info: string; out Is3k: Boolean;
      out Sep: Char; out TempDir: string): boolean;
  protected
    procedure CreateAndRunServerProcess; override;
    procedure ShutDownServer;  override;
  public
    SSHServerName : string;
    HostName : string;
    UserName : string;
    PythonCommand : string;
    PathSeparator : Char;
    TempDir : string;

    function IsPython3000 : Boolean; override;

    constructor Create(SSHServer : TSSHServer);
    destructor Destroy; override;
  end;

  TPySSHDebugger = class(TPyRemDebugger)
  { Remote debugger using Rpyc for communication with an SSH server }
  end;


implementation

Uses
  System.Threading,
  Vcl.Dialogs,
  JclSysUtils,
  StringResources,
  cPySupportTypes,
  cPyScripterSettings,
  System.StrUtils;

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
    fServerIsAvailable := ProcessPlatformInfo(CommandOutput, fIs3K, PathSeparator, TempDir)
  else
  begin
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


  inherited Create(peSSH);
end;

procedure TPySSHInterpreter.CreateAndRunServerProcess;
Var
  ErrorMsg : string;
begin
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

  // Create and Run Server process
  ServerProcess := TJvCreateProcess.Create(nil);
  with ServerProcess do
  begin
    Name := 'PyScripterServerProcess';
    CommandLine := Format('ssh %s %s@%s %s ',
                    [SshCommandOptions, UserName, HostName, PythonCommand]) +
                   Format('"%s" %d "%s"', [fRemServerFile, fSocketPort, fRemRpycFile]) ;
    ConsoleOptions := [coOwnerData, coRedirect];
    CreationFlags := CreationFlags + [cfCreateNoWindow];
    StartupInfo.ForceOffFeedback := True;
    OnRawRead := ProcessServerOuput;
    OnErrorRawRead := ProcessServerOuput;

    // Repeat here to make sure it is set right
    MaskFPUExceptions(PyIDEOptions.MaskFPUExceptions);

    // Execute Process
    ServerProcess.Run;

    Sleep(100);  // give it some time

    fServerIsAvailable := State = psWaiting;
  end;
  if not fServerIsAvailable then FreeAndNil(ServerProcess);

  // TODO: Create and run Tunnel Process

end;

destructor TPySSHInterpreter.Destroy;
begin
  inherited;
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
var
  OldExceptHook : Variant;
begin
  inherited;
  if fRemServerFile <> '' then
    Execute(Format('ssh %s %s@%s %s rm %s',
      [SshCommandOptions, UserName, HostName, PythonCommand, fRemServerFile]), CommandOutput);
  if fRemRpycFile <> '' then
    Execute(Format('ssh %s %s@%s %s rm %s',
      [SshCommandOptions, UserName, HostName, PythonCommand, fRemServerFile]), CommandOutput);
end;

end.
