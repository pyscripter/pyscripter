{-----------------------------------------------------------------------------
 Unit Name: cSSHSupport
 Author:    Kiriakos Vlahos
 Date:      20-Sep-2018
 Purpose:   Support classes for editing and running remote files
 History:
-----------------------------------------------------------------------------}

unit cSSHSupport;

interface

Uses
  System.SysUtils,
  System.Classes,
  System.RegularExpressions,
  cPyScripterSettings,
  uEditAppIntfs;

type

  TSSHServer = class(TBaseOptions)
  private
    fName : string;
    fHostName : string;
    fUserName : string;
    fPythonCommand : string;
    fSSHCommand : string;
    fSSHOptions : string;
    fScpCommand : string;
    fScpOptions : string;
    fPassword : string;
    fPasswordNeeded : boolean;
    procedure GetPassword;
  public
    HostKey : string;
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
    function Destination: string;
    function IsClientPutty: boolean;
    function SSHOptionsPW: string;
    function ScpOptionsPW: string;
    function ExtractHostKey(ErrorOutput: string): Boolean;
  published
    property Name : string read fName write fName;
    property HostName : string read fHostName write fHostName;
    property UserName : string read fUserName write fUserName;
    property PythonCommand : string read fPythonCommand write fPythonCommand;
    property SSHCommand : string read fSSHCommand write fSSHCommand;
    property SSHOptions : string read fSSHOptions write fSSHOptions;
    property ScpCommand : string read fScpCommand write fScpCommand;
    property ScpOptions : string read fScpOptions write fScpOptions;
    property PasswordNeeded : Boolean read fPasswordNeeded write fPasswordNeeded;
  end;

  TSSHServerItem = class(TCollectionItem)
  private
    fSSHServer : TSSHServer;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property SSHServer : TSSHServer read fSSHServer write fSSHServer;
  end;

  TSSHFileName = class
    class var UncRE: TRegEx;
    class constructor Create;
    class function Format(Server, FileName : string): string;
    class function Parse(Const Unc : string; out Server, FileName : string): boolean;
  end;

  TSSHServices = class(TInterfacedObject, ISSHServices)
    function FormatFileName(Server, FileName : string): string;
    function ParseFileName(Const Unc : string; out Server, FileName : string): boolean;
    // SCP
    function Scp(const ScpCommand, FromFile, ToFile: string; out ErrorMsg: string;
       ScpOptions : string = ''): Boolean;
    function ScpUpload(const ServerName, LocalFile, RemoteFile: string; out ErrorMsg: string): boolean;
    function ScpDownload(const ServerName, RemoteFile, LocalFile: string; out ErrorMsg: string): boolean;
  end;

  function ServerFromName(ServerName: string): TSSHServer;
  function EditSSHServers : boolean;
  function SelectSSHServer : string;
  function EditSSHConfiguration(Item : TCollectionItem) : boolean;
  procedure FillSSHConfigNames(Strings: TStrings);

var
  SSHTimeout : integer = 10000; // 10 seconds
  ScpTimeout : integer = 30000; // 30 seconds
  SSHServers : TCollection;

implementation

uses
  WinApi.Windows,
  System.Threading,
  System.StrUtils,
  Vcl.Forms,
  Vcl.Dialogs,
  uSysUtils,
  JvGnugettext,
  MPCommonUtilities,
  dlgCollectionEditor,
  dlgOptionsEditor,
  StringResources,
  uCommonFunctions;

{ TSSHConfig }

procedure TSSHServer.Assign(Source: TPersistent);
begin
  fPassword := '';
  HostKey := '';
  if Source is TSSHServer then with TSSHServer(Source) do begin
    Self.fName := Name;
    Self.fHostName := HostName;
    Self.UserName := UserName;
    Self.fPythonCommand := PythonCommand;
    Self.fSSHCommand := SSHCommand;
    Self.fSSHOptions := SSHOptions;
    Self.fScpCommand := ScpCommand;
    Self.fScpOptions := ScpOptions;
    Self.fPasswordNeeded := PasswordNeeded;
  end else
    inherited;
end;

constructor TSSHServer.Create;
begin
  inherited;
  PythonCommand := 'python3';
  SSHCommand := PyIDEOptions.SSHCommand;
  SSHOptions := PyIDEOptions.SSHOptions;
  ScpCommand := PyIDEOptions.ScpCommand;
  ScpOptions := PyIDEOptions.ScpOptions;
end;

function TSSHServer.Destination: string;
begin
  Result := HostName;
  if UserName <> '' then
    Result := UserName + '@' + HostName;
end;

function TSSHServer.ExtractHostKey(ErrorOutput: string): Boolean;
Var
  Match : TMatch;
begin
  Match := TRegEx.Match(ErrorOutput, '[\w\d][\w\d](:[\w\d][\w\d])+');
  Result := Match.Success;
  if Result then
    HostKey := Match.Value;
end;

procedure TSSHServer.GetPassword;
begin
  if fPasswordNeeded and (fPassword = '') then
    fPassWord := InputBox(Format(_('Enter SSH Password for %s'), [Destination]),
      #31+_('Password:'), '');
end;

function TSSHServer.IsClientPutty: boolean;
begin
  Result := fScpCommand.ToLower.Contains('pscp')
end;

function TSSHServer.ScpOptionsPW: string;
begin
  Result := fScpOptions;
  if not IsClientPutty then Exit;

  GetPassword;
  if fPassword <> '' then
    Result := Format('-pw %s %s', [fPassword, Result]);
  if HostKey <> '' then
    Result := Format('-hostkey %s %s', [HostKey, Result]);
end;

function TSSHServer.SSHOptionsPW: string;
begin
  Result := fSSHOptions;
  if not IsClientPutty then Exit;

  GetPassword;
  if fPassword <> '' then
    Result := Format('-pw %s %s', [fPassword, Result]);
  if HostKey <> '' then
    Result := Format('-hostkey %s %s', [HostKey, Result]);
end;

procedure TSSHServerItem.Assign(Source: TPersistent);
begin
  if Source is TSSHServerItem then with TSSHServerItem(Source) do
    Self.fSSHServer.Assign(TSSHServerItem(Source).SSHServer)
  else
    inherited;
end;

constructor TSSHServerItem.Create(Collection: TCollection);
begin
  inherited;
  fSSHServer := TSSHServer.Create;
end;

destructor TSSHServerItem.Destroy;
begin
  fSSHServer.Free;
  inherited;
end;

function TSSHServerItem.GetDisplayName: string;
begin
  if SSHServer.Name <> '' then
    Result := SSHServer.Name
  else
    Result := SSHServer.Destination;
end;

function EditSSHServers : boolean;
begin
  Result := EditCollection(SSHServers,
    TSSHServerItem, _('SSH Servers'), EditSSHConfiguration, 580);
end;

function SelectSSHServer : string;
Var
  Index : integer;
begin
  Result := '';
  if SelectFromCollection(SSHServers,
    TSSHServerItem, _('Select SSH Server'), EditSSHConfiguration, 580, Index)
  then
    Result := TSSHServerItem(SSHServers.Items[Index]).SSHServer.Name;
end;

function EditSSHConfiguration(Item : TCollectionItem) : boolean;
Var
  Categories : array of TOptionCategory;
begin
  SetLength(Categories, 1);
  with Categories[0] do begin
    DisplayName :='SSH';
    SetLength(Options, 9);
    Options[0].PropertyName := 'Name';
    Options[0].DisplayName := _('SSH Server name');
    Options[1].PropertyName := 'HostName';
    Options[1].DisplayName := _('Host name');
    Options[2].PropertyName := 'UserName';
    Options[2].DisplayName := _('User name');
    Options[3].PropertyName := 'PythonCommand';
    Options[3].DisplayName := _('Command to execute Python (no spaces)');
    Options[4].PropertyName := 'SSHCommand';
    Options[4].DisplayName := _('Path to SSH Command');
    Options[5].PropertyName := 'SSHOptions';
    Options[5].DisplayName := _('SSH options');
    Options[6].PropertyName := 'ScpCommand';
    Options[6].DisplayName := _('Path to SCP Command');
    Options[7].PropertyName := 'ScpOptions';
    Options[7].DisplayName := _('SCP options');
    Options[8].PropertyName := 'PasswordNeeded';
    Options[8].DisplayName := _('Password Needed (PyTTY only)');
  end;

  Result := InspectOptions((Item as TSSHServerItem).fSSHServer,
     Categories, _('Edit SSH Server'), 580, False);
end;


procedure FillSSHConfigNames(Strings: TStrings);
Var
  Item : TCollectionItem;
begin
   Strings.Clear;
   for Item in SSHServers do
     Strings.Add(TSSHServerItem(Item).DisplayName);
end;

function ServerFromName(ServerName: string): TSSHServer;
Var
  Item : TCollectionItem;
begin
  Result := nil;
  for Item in SSHServers do
    if TSSHServerItem(Item).SSHServer.Name = ServerName then
      Result := TSSHServerItem(Item).SSHServer;
end;

{ Unc }

class constructor TSSHFileName.Create;
begin
  UNCRE := CompiledRegEx('^ssh://([^/]+)/(.+)');
end;

class function TSSHFileName.Format(Server, FileName: string): string;
begin
  Result := System.SysUtils.Format('ssh://%s/%s', [Server, FileName]);
end;

class function TSSHFileName.Parse(const Unc: string; out Server,
  FileName: string): boolean;
begin
  Server := '';
  FileName := '';
  with UncRE.Match(Unc) do
  begin
    if Success then begin
      Server := GroupValue(1);
      FileName := GroupValue(2);
    end;
    Exit(Success)
  end;
end;

{ TSSHServices }

function TSSHServices.FormatFileName(Server, FileName: string): string;
begin
  Result := TSSHFileName.Format(Server, FileName);
end;

function TSSHServices.ParseFileName(const Unc: string; out Server,
  FileName: string): boolean;
begin
  Result := TSSHFileName.Parse(Unc, Server, FileName);
end;

function TSSHServices.Scp(const ScpCommand, FromFile, ToFile: string;
  out ErrorMsg: string; ScpOptions: string): Boolean;
Var
  Task : ITask;
  Command, Output, Error: string;
  ExitCode : integer;
begin
  Command := Format('"%s" %s %s %s', [ScpCommand, ScpOptions, FromFile, ToFile]);

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
    ExitCode := ExecuteCmd(Command, Output, Error);
  {$IFDEF CPUX86}
  finally
    if IsWow64 then Wow64EnableWow64FsRedirection_MP(True);
  end;
  {$ENDIF CPUX86}
  end);
  Task.Start;
  if not Task.Wait(ScpTimeout) then
  begin
    ErrorMsg := SScpErrorTimeout;
    Exit(False);
  end;

  Result :=  ExitCode = 0;

  case ExitCode of
    0: ErrorMsg :=  '';
    4: ErrorMsg := _(SScpError4);
    5: ErrorMsg := _(SScpError5);
    else
      ErrorMsg := Format(_(SScpErrorOther), [Output, Error]);
  end;
end;

function TSSHServices.ScpDownload(const ServerName, RemoteFile,
  LocalFile: string; out ErrorMsg: string): boolean;
Var
  SSHServer : TSSHServer;
  SFormat : string;
begin
  SSHServer := ServerFromName(ServerName);
  if not Assigned(SSHServer) then begin
    ErrorMsg := Format(_(SSHUnknownServer), [ServerName]);
    Exit(False);
  end;

  SFormat := IfThen(SSHServer.IsClientPutty, '%s:"%s"', '"%s:''%s''"');
  Result := scp(SSHServer.ScpCommand,
    Format(SFormat, [SSHServer.Destination, RemoteFile]),
    Format('"%s"', [LocalFile]), ErrorMsg, SSHServer.ScpOptionsPW);
end;

function TSSHServices.ScpUpload(const ServerName, LocalFile, RemoteFile: string;
  out ErrorMsg: string): boolean;
Var
  SSHServer : TSSHServer;
  SFormat : string;
begin
  SSHServer := ServerFromName(ServerName);
  if not Assigned(SSHServer) then begin
    ErrorMsg := Format(_(SSHUnknownServer), [ServerName]);
    Exit(False);
  end;

  SFormat := IfThen(SSHServer.IsClientPutty, '%s:"%s"', '"%s:''%s''"');
  Result := scp(SSHServer.ScpCommand, Format('"%s"', [LocalFile]),
    Format(SFormat, [SSHServer.Destination, RemoteFile]),
    ErrorMsg, SSHServer.ScpOptionsPW);
end;

initialization
  SSHServers := TCollection.Create(TSSHServerItem);
  GI_SSHServices := TSSHServices.Create;
finalization
  SSHServers.Free;
end.
