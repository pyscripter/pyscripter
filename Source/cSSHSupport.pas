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
  cPyScripterSettings,
  SynRegExpr;

type

  TSSHServer = class(TBaseOptions)
  private
    fName : string;
    fHostName : string;
    fUserName : string;
    fUsePython2 : boolean;
    fPythonCommand : string;
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
    function DefaultName : string;
  published
    property Name : string read fName write fName;
    property HostName : string read fHostName write fHostName;
    property UserName : string read fUserName write fUserName;
    property UsePython2 : boolean read fUsePython2 write fUsePython2;
    property PythonCommand : string read fPythonCommand write fPythonCommand;
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

  TUnc = class
    class var UncRE: TRegExpr;
    class constructor Create;
    class destructor Destroy;
    class function Format(Server, FileName : string): string;
    class function Parse(Const Unc : string; out Server, FileName : string): boolean;
  end;

  function ServerFromName(ServerName: string): TSSHServer;
  function EditSSHConfigurations : boolean;
  function EditSSHConfiguration(Item : TCollectionItem) : boolean;
  procedure FillSSHConfigNames(Strings: TStrings);

  // SCP
  function Scp(const FromFile, ToFile: string; out ErrorMsg: string): boolean;
  function ScpUpload(const ServerName, LocalFile, RemoteFile: string; out ErrorMsg: string): boolean;
  function ScpDownload(const ServerName, RemoteFile, LocalFile: string; out ErrorMsg: string): boolean;

Var
  SSHServers : TCollection;

implementation

uses
  Vcl.Forms,
  JvGnugettext,
  dlgCollectionEditor,
  dlgOptionsEditor,
  cTools,
  frmCommandOutput,
  StringResources;

{ TSSHConfig }

procedure TSSHServer.Assign(Source: TPersistent);
begin
  if Source is TSSHServer then with TSSHServer(Source) do begin
    Self.fName := Name;
    Self.fHostName := HostName;
    Self.UserName := UserName;
    Self.fUsePython2 := UsePython2;
    Self.fPythonCommand := PythonCommand;
  end else
    inherited;
end;

constructor TSSHServer.Create;
begin
  inherited;
  PythonCommand := 'python';
end;

function TSSHServer.DefaultName: string;
begin
  Result := HostName;
  if UserName <> '' then
    Result := UserName + '@' + HostName;
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
    Result := SSHServer.DefaultName;
end;

function EditSSHConfigurations : boolean;
begin
  Result := EditCollection(SSHServers,
    TSSHServerItem, _('SSH Servers'), EditSSHConfiguration, 580);
end;

function EditSSHConfiguration(Item : TCollectionItem) : boolean;
Var
  Categories : array of TOptionCategory;
begin
  SetLength(Categories, 1);
  with Categories[0] do begin
    DisplayName :='SSH';
    SetLength(Options, 5);
    Options[0].PropertyName := 'Name';
    Options[0].DisplayName := _('SSH Server name');
    Options[1].PropertyName := 'HostName';
    Options[1].DisplayName := _('Host name');
    Options[2].PropertyName := 'UserName';
    Options[2].DisplayName := _('User name');
    Options[3].PropertyName := 'UsePython2';
    Options[3].DisplayName := _('Use python 2.x');
    Options[4].PropertyName := 'PythonCommand';
    Options[4].DisplayName := _('Command to execute python');
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

function Scp(const FromFile, ToFile: string; out ErrorMsg: string): Boolean;
Var
  ExternalTool : TExternalTool;
begin
  ExternalTool := TExternalTool.Create;
  try
    ExternalTool.ApplicationName := 'scp';
    ExternalTool.Parameters :=
      Format('-o PasswordAuthentication=no -o StrictHostKeyChecking=no %s %s',
      [FromFile, ToFile]);
    ExternalTool.TimeOut := 2000;
    ExternalTool.CaptureOutput := False;

    OutputWindow.ExecuteTool(ExternalTool);

    while OutputWindow.IsRunning do
      Application.ProcessMessages;

    Result :=  OutputWindow.LastExitCode = 0;

    case OutputWindow.LastExitCode of
      0: ErrorMsg :=  '';
      4: ErrorMsg := SScpError4;
      5: ErrorMsg := SScpError5;
      else
        ErrorMsg := SScpOtherError;
    end;

  finally
    ExternalTool.Free;
  end;
end;

function ScpUpload(const ServerName, LocalFile, RemoteFile: string; out ErrorMsg: string): boolean;
Var
  SSHServer : TSSHServer;
begin
  SSHServer := ServerFromName(ServerName);
  if not Assigned(SSHServer) then begin
    ErrorMsg := Format(SSSHUnknownServer, [ServerName]);
    Exit(False);
  end;

  Result := scp(LocalFile, Format('%s:%s', [SSHServer.DefaultName, RemoteFile]), ErrorMsg);
end;

function ScpDownload(const ServerName, RemoteFile, LocalFile: string; out ErrorMsg: string): boolean;
Var
  SSHServer : TSSHServer;
begin
  SSHServer := ServerFromName(ServerName);
  if not Assigned(SSHServer) then begin
    ErrorMsg := Format(SSSHUnknownServer, [ServerName]);
    Exit(False);
  end;

  Result := scp(Format('%s:%s', [SSHServer.DefaultName, RemoteFile]), LocalFile, ErrorMsg);
end;

{ Unc }

class constructor TUnc.Create;
begin
  UNCRE := TRegExpr.Create;
  UncRE.Expression := '^\\\\([^\\]+)\\(.+)';
  UncRe.Compile;
end;

class destructor TUnc.Destroy;
begin
  UncRe.Free;
end;

class function TUnc.Format(Server, FileName: string): string;
begin
  Result := System.SysUtils.Format('\\%s\%s', [Server, FileName]);
end;

class function TUnc.Parse(const Unc: string; out Server,
  FileName: string): boolean;
begin
  Result := UncRE.Exec(Unc);
  if Result then begin
    Server := UncRE.Match[1];
    FileName := UncRE.Match[2];
  end;
end;

initialization
  SSHServers := TCollection.Create(TSSHServerItem);
finalization
  SSHServers.Free;
end.
