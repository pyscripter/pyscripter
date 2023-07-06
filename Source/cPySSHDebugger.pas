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
  uSysUtils,
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
    fSSHCommand : string;
    fSSHOptions : string;
    function ProcessPlatformInfo(Info: string; out Is3k: Boolean;
      out Sep: Char; out TempDir: string): boolean;
    procedure StoreTunnelProcessInfo(const ProcessInfo: TProcessInformation; InWritePipe: PHandle);
  protected
    function SystemTempFolder: string; override;
    procedure CreateAndRunServerProcess; override;
    procedure ShutDownServer;  override;
  public
    SSHServerName : string;
    SSHDestination : string;
    PythonCommand : string;
    PathSeparator : Char;
    TempDir : string;
    PythonVersion : string;
    RemotePlatform : string;
    function ToPythonFileName(const FileName: string): string; override;
    function FromPythonFileName(const FileName: string): string; override;

    constructor Create(SSHServer : TSSHServer);
    destructor Destroy; override;
  end;


implementation

Uses
  Vcl.Dialogs,
  Vcl.Forms,
  JvGNUGetText,
  StringResources,
  cPySupportTypes,
  cPyScripterSettings,
  System.StrUtils,
  uEditAppIntfs,
  uCommonFunctions,
  cPyControl,
  MPCommonUtilities;

{ TPySSHInterpreter }

constructor TPySSHInterpreter.Create(SSHServer : TSSHServer);
{
   1. ssh user@host pythoncommand 'import sys,os,tempfile;print(sys.version[0]);print(os.sep);print(tempfile.gettempdir())'
   2. Upload with scp server script and rpyc.zip
   3. Start Python server process
   4. Start port tunneling process ssh user@host -L 127.0.0.1:port:127.0.0.1:port -N
   5. Connect to server
}
Var
  CommandOutput, ErrorOutput : string;
  Task : ITask;
  ReturnCode:Integer;

  procedure CreatePythonTask;
  begin
    //  Test SSH connection and get information about the server
    Task := TTask.Create(procedure
    {$IFDEF CPUX86}
    Var
      IsWow64: LongBool;
    {$ENDIF CPUX86}
    begin
    {$IFDEF CPUX86}
    IsWow64 := IsWow64Process(GetCurrentProcess, IsWow64) and IsWow64;
    if IsWow64 then Wow64EnableWow64FsRedirection_MP(False);
    try
    {$ENDIF CPUX86}
      ReturnCode := ExecuteCmd(Format('"%s" %s %s %s -c ' +
        '"''import sys,os,tempfile;print(sys.version[0]);print(os.sep);print(tempfile.gettempdir())''"',
        [fSSHCommand, fSSHOptions, SSHDestination, PythonCommand]),
        CommandOutput, ErrorOutput);
      fServerIsAvailable :=  ReturnCode = 0;
    {$IFDEF CPUX86}
    finally
      if IsWow64 then Wow64EnableWow64FsRedirection_MP(True);
    end;
    {$ENDIF CPUX86}
    end).Start;
  end;

begin
  fSSHCommand := SSHServer.SSHCommand;
  fSSHOptions := SSHServer.SSHOptionsPW;

  fServerIsAvailable := False;
  fConnected := False;
  SSHServerName := SSHServer.Name;
  SSHDestination := SSHServer.Destination;
  PythonCommand := SSHServer.PythonCommand;

  CreatePythonTask;
  if Task.Wait(SSHTimeout) then begin
    if SSHServer.IsClientPutty and (ErrorOutput <> '') and
      SSHServer.ExtractHostKey(ErrorOutput) then
    begin
      // Unknown hostkey failure
      if StyledMessageDlg(Format(_(SSHUnknownServerQuery), [SSHServer.HostKey]),
        mtConfirmation, [mbYes, mbCancel], 0) = mrYes then
      begin
        fSSHOptions := SSHServer.SSHOptionsPW;
        CreatePythonTask;
        if not Task.Wait(SSHTimeout) then begin
          // Timeout
          StyledMessageDlg(Format(_(SSHPythonTimeout), [PythonCommand]), mtError, [mbAbort], 0);
          Exit;
        end;
      end else begin
        SSHServer.HostKey := '';
        Exit;
      end;
    end;
    if fServerIsAvailable then
      fServerIsAvailable := ProcessPlatformInfo(CommandOutput, fIs3K, PathSeparator, TempDir);
  end else begin
    // Timeout
    StyledMessageDlg(Format(_(SSHPythonTimeout), [PythonCommand]), mtError, [mbAbort], 0);
    Exit;
  end;

  if not fServerIsAvailable then begin
    SSHServer.ExtractHostKey(ErrorOutput);
    StyledMessageDlg(Format(_(SSHPythonError),
      [PythonCommand, ReturnCode, CommandOutput, ErrorOutput]), mtError, [mbAbort], 0);
    Exit;
  end;

  // Check for version mismatch
  if not fIs3K then
  begin
    StyledMessageDlg(Format(_(SSHVersionMismatch), ['3.x', '2,x']),
      mtError, [mbAbort], 0);
    Exit;
  end;

  TunnelProcessOptions := TJclExecuteCmdProcessOptions.Create('');
  TunnelProcessOptions.BeforeResume := StoreTunnelProcessInfo;
  TunnelProcessOptions.MergeError := False;
  TunnelProcessOptions.RawOutput := True;
  TunnelProcessOptions.RawError := True;
  TunnelProcessOptions.AutoConvertOEM := False;
  TunnelProcessOptions.CreateProcessFlags :=
    TunnelProcessOptions.CreateProcessFlags or
     CREATE_UNICODE_ENVIRONMENT or CREATE_NO_WINDOW or CREATE_NEW_CONSOLE;

  inherited Create(peSSH);
  DebuggerClass := TPyRemDebugger;
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
  if not GI_SSHServices.ScpUpload(SSHServerName, fServerFile, fRemServerFile, ErrorMsg) then
    fRemServerFile := ''
  else begin
    fRemRpycFile :=  TempDir + PathSeparator + RpycZipModule;
    if not GI_SSHServices.ScpUpload(SSHServerName, fRpycPath, fRemRpycFile, ErrorMsg) then
      fRemRpycFile := '';
  end;

  if (fRemServerFile = '') or (fRemRpycFile = '') then
  begin
    StyledMessageDlg(ErrorMsg, mtError, [mbAbort], 0);
    Exit;
  end;

  // Find a new port
  Randomize;
  fSocketPort := 18000 + Random(1000);

  // Create and Run Server process
  ServerProcessOptions.CommandLine := Format('"%s" %s %s %s -u -X utf8 ',
    [fSSHCommand, fSSHOptions, SSHDestination, PythonCommand]) +
    Format('"%s" %d "%s"', [fRemServerFile, fSocketPort, fRemRpycFile]);
  ServerTask := TTask.Create(procedure
    {$IFDEF CPUX86}
    Var
      IsWow64: LongBool;
   {$ENDIF CPUX86}
    begin
    {$IFDEF CPUX86}
    IsWow64 := IsWow64Process(GetCurrentProcess, IsWow64) and IsWow64;
    if IsWow64 then Wow64EnableWow64FsRedirection_MP(False);
    try
    {$ENDIF CPUX86}
      ExecuteCmdProcess(ServerProcessOptions);
    {$IFDEF CPUX86}
    finally
      if IsWow64 then Wow64EnableWow64FsRedirection_MP(True);
    end;
  {$ENDIF CPUX86}
    end).Start;
  Sleep(100);
  fServerIsAvailable := ServerTask.Status = TTaskStatus.Running;

  TunnelProcessOptions.CommandLine := Format('"%s" %s %s -L 127.0.0.1:%d:127.0.0.1:%3:d -N',
    [fSSHCommand, fSSHOptions, SSHDestination, fSocketPort]);
  TunnelTask := TTask.Create(procedure
    {$IFDEF CPUX86}
    Var
      IsWow64: LongBool;
   {$ENDIF CPUX86}
    begin
    {$IFDEF CPUX86}
    IsWow64 := IsWow64Process(GetCurrentProcess, IsWow64) and IsWow64;
    if IsWow64 then Wow64EnableWow64FsRedirection_MP(False);
    try
    {$ENDIF CPUX86}
      ExecuteCmdProcess(TunnelProcessOptions);
    {$IFDEF CPUX86}
    finally
      if IsWow64 then Wow64EnableWow64FsRedirection_MP(True);
    end;
  {$ENDIF CPUX86}
    end).Start;
  Sleep(100);
  fServerIsAvailable := TunnelTask.Status = TTaskStatus.Running;
end;

destructor TPySSHInterpreter.Destroy;
begin
  inherited;
  FreeAndNil(TunnelProcessOptions);
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
  {$IFDEF CPUX86}
  IsWow64: LongBool;
  {$ENDIF CPUX86}
begin
  fShuttingDown := True;
  inherited;
  // Delete temp files
  {$IFDEF CPUX86}
  IsWow64 := IsWow64Process(GetCurrentProcess, IsWow64) and IsWow64;
  if IsWow64 then Wow64EnableWow64FsRedirection_MP(False);
  try
  {$ENDIF CPUX86}
  if (fRemServerFile <> '') and (ExecuteCmd(Format('"%s" %s %s rm ''%s''',
      [fSSHCommand, fSSHOptions, SSHDestination, fRemServerFile]), CommandOutput) = 0)
  then
    fRemServerFile := '';
  if (fRemRpycFile <> '')  and (ExecuteCmd(Format('"%s" %s %s rm ''%s''',
      [fSSHCommand, fSSHOptions, SSHDestination, fRemRpycFile]), CommandOutput) = 0)
  then
    fRemRpycFile := '';
  {$IFDEF CPUX86}
  finally
    if IsWow64 then Wow64EnableWow64FsRedirection_MP(True);
  end;
  {$ENDIF CPUX86}
  // shut down tunnel
  if Assigned(TunnelTask) and (TunnelTask.Status = TTaskStatus.Running) then
    TerminateProcessTree(TunnelProcessInfo.dwProcessId);
  TunnelTask.Wait;
  TunnelTask := nil;
end;

procedure TPySSHInterpreter.StoreTunnelProcessInfo(const ProcessInfo:
    TProcessInformation; InWritePipe: PHandle);
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
  if TSSHFileName.Parse(FileName, Server, FName) and (Server = SSHServerName) then
    Result := FName
  else
    Result := '<' + FileName + '>';
end;

function TPySSHInterpreter.FromPythonFileName(const FileName: string): string;
begin
  if FileName = '' then
    Result := ''
  else if (FileName[1] ='<') and (FileName[Length(FileName)] = '>') then
     Result :=  Copy(FileName, 2, Length(FileName)-2)
  else
    Result := TSSHFileName.Format(SSHServerName, FileName);
end;


end.
