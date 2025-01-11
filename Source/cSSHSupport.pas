{-----------------------------------------------------------------------------
 Unit Name: cSSHSupport
 Author:    Kiriakos Vlahos
 Date:      20-Sep-2018
 Purpose:   Support classes for editing and running remote files
 History:
-----------------------------------------------------------------------------}

unit cSSHSupport;

interface

uses
  System.Classes,
  System.RegularExpressions,
  cPyScripterSettings,
  uEditAppIntfs;

type

  TSSHServer = class(TBaseOptions)
  private
    FName: string;
    FHostName: string;
    FUserName: string;
    FPythonCommand: string;
    FSSHCommand: string;
    FSSHOptions: string;
    FScpCommand: string;
    FScpOptions: string;
    FPassword: string;
    FPasswordNeeded: Boolean;
    procedure GetPassword;
  public
    HostKey: string;
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
    function Destination: string;
    function IsClientPutty: Boolean;
    function SSHOptionsPW: string;
    function ScpOptionsPW: string;
    function ExtractHostKey(ErrorOutput: string): Boolean;
  published
    property Name: string read FName write FName;
    property HostName: string read FHostName write FHostName;
    property UserName: string read FUserName write FUserName;
    property PythonCommand: string read FPythonCommand write FPythonCommand;
    property SSHCommand: string read FSSHCommand write FSSHCommand;
    property SSHOptions: string read FSSHOptions write FSSHOptions;
    property ScpCommand: string read FScpCommand write FScpCommand;
    property ScpOptions: string read FScpOptions write FScpOptions;
    property PasswordNeeded: Boolean read FPasswordNeeded write FPasswordNeeded;
  end;

  TSSHServerItem = class(TCollectionItem)
  private
    FSSHServer: TSSHServer;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property SSHServer: TSSHServer read FSSHServer write FSSHServer;
  end;

  TSSHFileName = class
    class var UncRE: TRegEx;
    class constructor Create;
    class function Format(Server, FileName: string): string;
    class function Parse(const Unc: string; out Server, FileName: string): Boolean;
  end;

  TSSHServices = class(TInterfacedObject, ISSHServices)
    function FormatFileName(Server, FileName: string): string;
    function ParseFileName(const Unc: string; out Server, FileName: string): Boolean;
    // SCP
    function Scp(const ScpCommand, FromFile, ToFile: string; out ErrorMsg: string;
       ScpOptions: string = ''): Boolean;
    function ScpUpload(const ServerName, LocalFile, RemoteFile: string; out ErrorMsg: string): Boolean;
    function ScpDownload(const ServerName, RemoteFile, LocalFile: string; out ErrorMsg: string): Boolean;
  end;

function ServerFromName(ServerName: string): TSSHServer;
function EditSSHServers: Boolean;
function SelectSSHServer: string;
function EditSSHConfiguration(Item: TCollectionItem): Boolean;
procedure FillSSHConfigNames(Strings: TStrings);

var
  SSHTimeout: Integer = 10000; // 10 seconds
  ScpTimeout: Integer = 30000; // 30 seconds
  SSHServers: TCollection;

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  System.Threading,
  System.StrUtils,
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
  FPassword := '';
  HostKey := '';
  if Source is TSSHServer then with TSSHServer(Source) do begin
    Self.FName := Name;
    Self.FHostName := HostName;
    Self.UserName := UserName;
    Self.FPythonCommand := PythonCommand;
    Self.FSSHCommand := SSHCommand;
    Self.FSSHOptions := SSHOptions;
    Self.FScpCommand := ScpCommand;
    Self.FScpOptions := ScpOptions;
    Self.FPasswordNeeded := PasswordNeeded;
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
var
  Match: TMatch;
begin
  Match := TRegEx.Match(ErrorOutput, '[\w\d][\w\d](:[\w\d][\w\d])+');
  Result := Match.Success;
  if Result then
    HostKey := Match.Value;
end;

procedure TSSHServer.GetPassword;
begin
  if FPasswordNeeded and (FPassword = '') then
    FPassword := InputBox(Format(_('Enter SSH Password for %s'), [Destination]),
      #31+_('Password:'), '');
end;

function TSSHServer.IsClientPutty: Boolean;
begin
  Result := FScpCommand.ToLower.Contains('pscp')
end;

function TSSHServer.ScpOptionsPW: string;
begin
  Result := FScpOptions;
  if not IsClientPutty then Exit;

  GetPassword;
  if FPassword <> '' then
    Result := Format('-pw %s %s', [FPassword, Result]);
  if HostKey <> '' then
    Result := Format('-hostkey %s %s', [HostKey, Result]);
end;

function TSSHServer.SSHOptionsPW: string;
begin
  Result := FSSHOptions;
  if not IsClientPutty then Exit;

  GetPassword;
  if FPassword <> '' then
    Result := Format('-pw %s %s', [FPassword, Result]);
  if HostKey <> '' then
    Result := Format('-hostkey %s %s', [HostKey, Result]);
end;

procedure TSSHServerItem.Assign(Source: TPersistent);
begin
  if Source is TSSHServerItem then with TSSHServerItem(Source) do
    Self.FSSHServer.Assign(TSSHServerItem(Source).SSHServer)
  else
    inherited;
end;

constructor TSSHServerItem.Create(Collection: TCollection);
begin
  inherited;
  FSSHServer := TSSHServer.Create;
end;

destructor TSSHServerItem.Destroy;
begin
  FSSHServer.Free;
  inherited;
end;

function TSSHServerItem.GetDisplayName: string;
begin
  if SSHServer.Name <> '' then
    Result := SSHServer.Name
  else
    Result := SSHServer.Destination;
end;

function EditSSHServers: Boolean;
begin
  Result := EditCollection(SSHServers,
    TSSHServerItem, _('SSH Servers'), EditSSHConfiguration, 580);
end;

function SelectSSHServer: string;
var
  Index: Integer;
begin
  Result := '';
  if SelectFromCollection(SSHServers,
    TSSHServerItem, _('Select SSH Server'), EditSSHConfiguration, 580, Index)
  then
    Result := TSSHServerItem(SSHServers.Items[Index]).SSHServer.Name;
end;

function EditSSHConfiguration(Item: TCollectionItem): Boolean;
var
  Categories: array of TOptionCategory;
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

  Result := InspectOptions((Item as TSSHServerItem).FSSHServer,
     Categories, _('Edit SSH Server'), [], nil, 580, False);
end;


procedure FillSSHConfigNames(Strings: TStrings);
var
  Item: TCollectionItem;
begin
   Strings.Clear;
   for Item in SSHServers do
     Strings.Add(TSSHServerItem(Item).DisplayName);
end;

function ServerFromName(ServerName: string): TSSHServer;
var
  Item: TCollectionItem;
begin
  Result := nil;
  for Item in SSHServers do
    if TSSHServerItem(Item).SSHServer.Name = ServerName then
      Result := TSSHServerItem(Item).SSHServer;
end;

{ Unc }

class constructor TSSHFileName.Create;
begin
  UncRE := CompiledRegEx('^ssh://([^/]+)/(.+)');
end;

class function TSSHFileName.Format(Server, FileName: string): string;
begin
  Result := System.SysUtils.Format('ssh://%s/%s', [Server, FileName]);
end;

class function TSSHFileName.Parse(const Unc: string; out Server,
  FileName: string): Boolean;
begin
  Server := '';
  FileName := '';
  with UncRE.Match(Unc) do
  begin
    if Success then begin
      Server := GroupValue(1);
      FileName := GroupValue(2);
    end;
    Exit(Success);
  end;
end;

{ TSSHServices }

function TSSHServices.FormatFileName(Server, FileName: string): string;
begin
  Result := TSSHFileName.Format(Server, FileName);
end;

function TSSHServices.ParseFileName(const Unc: string; out Server,
  FileName: string): Boolean;
begin
  Result := TSSHFileName.Parse(Unc, Server, FileName);
end;

function TSSHServices.Scp(const ScpCommand, FromFile, ToFile: string;
  out ErrorMsg: string; ScpOptions: string): Boolean;
var
  Task: ITask;
  Command, Output, Error: string;
  ExitCode: Integer;
begin
  Command := Format('"%s" %s %s %s', [ScpCommand, ScpOptions, FromFile, ToFile]);

  Task := TTask.Create(procedure
  {$IFDEF CPUX86}
  var
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
  LocalFile: string; out ErrorMsg: string): Boolean;
var
  SSHServer: TSSHServer;
  SFormat: string;
begin
  SSHServer := ServerFromName(ServerName);
  if not Assigned(SSHServer) then begin
    ErrorMsg := Format(_(SSHUnknownServer), [ServerName]);
    Exit(False);
  end;

  SFormat := IfThen(SSHServer.IsClientPutty, '%s:"%s"', '"%s:''%s''"');
  Result := Scp(SSHServer.ScpCommand,
    Format(SFormat, [SSHServer.Destination, RemoteFile]),
    Format('"%s"', [LocalFile]), ErrorMsg, SSHServer.ScpOptionsPW);
end;

function TSSHServices.ScpUpload(const ServerName, LocalFile, RemoteFile: string;
  out ErrorMsg: string): Boolean;
var
  SSHServer: TSSHServer;
  SFormat: string;
begin
  SSHServer := ServerFromName(ServerName);
  if not Assigned(SSHServer) then begin
    ErrorMsg := Format(_(SSHUnknownServer), [ServerName]);
    Exit(False);
  end;

  SFormat := IfThen(SSHServer.IsClientPutty, '%s:"%s"', '"%s:''%s''"');
  Result := Scp(SSHServer.ScpCommand, Format('"%s"', [LocalFile]),
    Format(SFormat, [SSHServer.Destination, RemoteFile]),
    ErrorMsg, SSHServer.ScpOptionsPW);
end;

initialization
  SSHServers := TCollection.Create(TSSHServerItem);
  GI_SSHServices := TSSHServices.Create;
finalization
  SSHServers.Free;
end.
