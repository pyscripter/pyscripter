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
  Winapi.Windows,
  System.Threading,
  PascalProcess,
  cPyRemoteDebugger,
  cSSHSupport;

type

  TPySSHInterpreter = class(TPyRemoteInterpreter)
  { Rpyc based remote Python Interpreter running on an SSH server }
  private
    FIs3K: Boolean;
    FRemServerFile: string;
    FRemRpycFile: string;
    FTunnelProcess: IPProcess;
    FShuttingDown: Boolean;
    FSSHCommand: string;
    FSSHOptions: string;
    FPathSeparator: Char;
    FTempDir: string;
    function ProcessPlatformInfo(Info: string; out Is3k: Boolean;
      out Sep: Char; out TempDir: string): Boolean;
  protected
    function SystemTempFolder: string; override;
    procedure CreateAndRunServerProcess; override;
    procedure ShutDownServer;  override;
  public
    SSHServerName: string;
    SSHDestination: string;
    PythonCommand: string;
    function ToPythonFileName(const FileName: string): string; override;
    function FromPythonFileName(const FileName: string): string; override;

    constructor Create(SSHServer: TSSHServer);
  end;

implementation

uses
  System.UITypes,
  System.SysUtils,
  System.Classes,
  Vcl.Dialogs,
  JvGnugettext,
  StringResources,
  cPySupportTypes,
  uEditAppIntfs,
  uCommonFunctions,
  MPCommonUtilities;

{ TPySSHInterpreter }

constructor TPySSHInterpreter.Create(SSHServer: TSSHServer);
{
   1. ssh user@host pythoncommand
     'import sys,os,tempfile;print(sys.version[0]);print(os.sep);print(tempfile.gettempdir())'
   2. Upload with scp server script and rpyc.zip
   3. Start Python server process
   4. Start port tunneling process ssh user@host -L 127.0.0.1:port:127.0.0.1:port -N
   5. Connect to server
}
var
  PyProcess: IPProcess;

  procedure CreatePythonTask;
  {$IFDEF CPUX86}
  var
    IsWow64: LongBool;
  {$ENDIF CPUX86}
  begin
    PyProcess := TPProcess.Create(Format('"%s" %s %s %s -c ' +
          '"''import sys,os,tempfile;print(sys.version[0]);print(os.sep);print(tempfile.gettempdir())''"',
          [FSSHCommand, FSSHOptions, SSHDestination, PythonCommand]));
    {$IFDEF CPUX86}
    IsWow64 := IsWow64Process(GetCurrentProcess, IsWow64) and IsWow64;
    if IsWow64 then Wow64EnableWow64FsRedirection_MP(False);
    try
    {$ENDIF CPUX86}
    PyProcess.Execute;
    while PyProcess.State = TPPState.Created do
      TThread.Yield;
    {$IFDEF CPUX86}
    finally
      if IsWow64 then Wow64EnableWow64FsRedirection_MP(True);
    end;
    {$ENDIF CPUX86}
  end;

begin
  FSSHCommand := SSHServer.SSHCommand;
  FSSHOptions := SSHServer.SSHOptionsPW;

  FServerIsAvailable := False;
  FConnected := False;
  SSHServerName := SSHServer.Name;
  SSHDestination := SSHServer.Destination;
  PythonCommand := SSHServer.PythonCommand;

  CreatePythonTask;
  if PyProcess.WaitFor(SSHTimeout) then
  begin
    if SSHServer.IsClientPutty and (Length(PyProcess.ErrorOutput) > 0) and
      SSHServer.ExtractHostKey(TEncoding.UTF8.GetString(PyProcess.ErrorOutput)) then
    begin
      // Unknown hostkey failure
      if StyledMessageDlg(Format(_(SSHUnknownServerQuery), [SSHServer.HostKey]),
        mtConfirmation, [mbYes, mbCancel], 0) = mrYes then
      begin
        FSSHOptions := SSHServer.SSHOptionsPW;
        CreatePythonTask;
        if not PyProcess.WaitFor(SSHTimeout) then begin
          // Timeout
          StyledMessageDlg(Format(_(SSHPythonTimeout), [PythonCommand]), mtError, [mbAbort], 0);
          Exit;
        end;
      end
      else
      begin
        SSHServer.HostKey := '';
        Exit;
      end;
    end;
    FServerIsAvailable := (PyProcess.ExitCode = 0) and
       ProcessPlatformInfo(
       TEncoding.UTF8.GetString(PyProcess.Output),
       FIs3K, FPathSeparator, FTempDir);
  end
  else
  begin
    // Timeout
    StyledMessageDlg(Format(_(SSHPythonTimeout), [PythonCommand]), mtError, [mbAbort], 0);
    Exit;
  end;

  if not FServerIsAvailable then
  begin
    SSHServer.ExtractHostKey(TEncoding.UTF8.GetString(PyProcess.ErrorOutput));
    StyledMessageDlg(Format(_(SSHPythonError),
      [PythonCommand, PyProcess.ExitCode,
      TEncoding.UTF8.GetString(PyProcess.Output),
      TEncoding.UTF8.GetString(PyProcess.ErrorOutput)]), mtError, [mbAbort], 0);
    Exit;
  end;

  // Check for version mismatch
  if not FIs3K then
  begin
    StyledMessageDlg(Format(_(SSHVersionMismatch), ['3.x', '2,x']),
      mtError, [mbAbort], 0);
    Exit;
  end;

  inherited Create(peSSH);
  DebuggerClass := TPyRemDebugger;
end;

procedure TPySSHInterpreter.CreateAndRunServerProcess;
var
  ErrorMsg: string;
  {$IFDEF CPUX86}
    IsWow64: LongBool;
  {$ENDIF CPUX86}
begin
  FShuttingDown := False;
  FServerIsAvailable := False;
  // Upload server and rpyc files
  FRemServerFile := FTempDir + FPathSeparator + RemoteServerBaseName;
  if not GI_SSHServices.ScpUpload(SSHServerName, FServerFile, FRemServerFile, ErrorMsg) then
    FRemServerFile := ''
  else begin
    FRemRpycFile :=  FTempDir + FPathSeparator + RpycZipModule;
    if not GI_SSHServices.ScpUpload(SSHServerName, FRpycPath, FRemRpycFile, ErrorMsg) then
      FRemRpycFile := '';
  end;

  if (FRemServerFile = '') or (FRemRpycFile = '') then
  begin
    StyledMessageDlg(ErrorMsg, mtError, [mbAbort], 0);
    Exit;
  end;

  // Find a new port
  Randomize;
  FSocketPort := 18000 + Random(1000);

  // Create and Run Server process
  ServerProcess := TPProcess.Create(Format('"%s" %s %s %s -u -X utf8 ',
    [FSSHCommand, FSSHOptions, SSHDestination, PythonCommand]) +
    Format('"%s" %d "%s"', [FRemServerFile, FSocketPort, FRemRpycFile]));
  ServerProcess.MergeError := True;
  ServerProcess.OnRead := ProcessServerOutput;

  {$IFDEF CPUX86}
  IsWow64 := IsWow64Process(GetCurrentProcess, IsWow64) and IsWow64;
  if IsWow64 then Wow64EnableWow64FsRedirection_MP(False);
  try
  {$ENDIF CPUX86}
  ServerProcess.Execute;
  while ServerProcess.State = TPPState.Created do
    TThread.Yield;
  {$IFDEF CPUX86}
  finally
    if IsWow64 then Wow64EnableWow64FsRedirection_MP(True);
  end;
  {$ENDIF CPUX86}
  FServerIsAvailable := ServerProcess.State = TPPState.Running;

  FTunnelProcess := TPProcess.Create(
    Format('"%s" %s %s -L 127.0.0.1:%d:127.0.0.1:%3:d -N',
    [FSSHCommand, FSSHOptions, SSHDestination, FSocketPort]));
  {$IFDEF CPUX86}
  IsWow64 := IsWow64Process(GetCurrentProcess, IsWow64) and IsWow64;
  if IsWow64 then Wow64EnableWow64FsRedirection_MP(False);
  try
  {$ENDIF CPUX86}
    FTunnelProcess.Execute;
    while FTunnelProcess.State = TPPState.Created do
      TThread.Yield;
  {$IFDEF CPUX86}
  finally
    if IsWow64 then Wow64EnableWow64FsRedirection_MP(True);
  end;
  {$ENDIF CPUX86}
  FServerIsAvailable := FServerIsAvailable and
    (FTunnelProcess.State = TPPState.Running);
end;

function TPySSHInterpreter.ProcessPlatformInfo(Info: string; out Is3k: Boolean;
  out Sep: Char; out TempDir: string): Boolean;
begin
  Result := True;
  var SL := TStringList.Create;
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
var
  CommandOutput: TBytes;
  ErrorOutput: TBytes;
  {$IFDEF CPUX86}
  IsWow64: LongBool;
  {$ENDIF CPUX86}
begin
  FShuttingDown := True;
  inherited;
  // Delete temp files
  {$IFDEF CPUX86}
  IsWow64 := IsWow64Process(GetCurrentProcess, IsWow64) and IsWow64;
  if IsWow64 then Wow64EnableWow64FsRedirection_MP(False);
  try
  {$ENDIF CPUX86}
  if (FRemServerFile <> '') and (TPProcess.Execute(Format('"%s" %s %s rm ''%s''',
      [FSSHCommand, FSSHOptions, SSHDestination, FRemServerFile]), '',
      CommandOutput, ErrorOutput) = 0)
  then
    FRemServerFile := '';
  if (FRemRpycFile <> '')  and (TPProcess.Execute(Format('"%s" %s %s rm ''%s''',
      [FSSHCommand, FSSHOptions, SSHDestination, FRemRpycFile]), '',
      CommandOutput, ErrorOutput) = 0)
  then
    FRemRpycFile := '';
  {$IFDEF CPUX86}
  finally
    if IsWow64 then Wow64EnableWow64FsRedirection_MP(True);
  end;
  {$ENDIF CPUX86}
  // shut down tunnel
  if Assigned(FTunnelProcess) then
    FTunnelProcess := nil;
end;

function TPySSHInterpreter.SystemTempFolder: string;
begin
  Result := FTempDir;
end;

function TPySSHInterpreter.ToPythonFileName(const FileName: string): string;
var
  Server, FName: string;
begin
  if TSSHFileName.Parse(FileName, Server, FName) and (Server = SSHServerName) then
    Result := FName
  else
    Result := '<' + FileName + '>';
end;

function TPySSHInterpreter.FromPythonFileName(const FileName: string): string;
begin
  if FileName = '' then
    Result := ''
  else if (FileName[1] = '<') and (FileName[Length(FileName)] = '>') then
     Result :=  Copy(FileName, 2, Length(FileName)-2)
  else
    Result := TSSHFileName.Format(SSHServerName, FileName);
end;

end.
