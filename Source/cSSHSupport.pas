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
  System.Classes;

type

  TSSHConfig = class(TCollectionItem)
  private
    fName : string;
    fHostName : string;
    fUserName : string;
    fNeedsPassword : boolean;
    fUsePython3K : boolean;
    fPythonCommand : string;
  protected
    function GetDisplayName: string; override;
  public
    Password : string;
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  public
  published
    property Name : string read fName write fName;
    property HostName : string read fHostName write fHostName;
    property UserName : string read fUserName write fUserName;
    property NeedsPassword : boolean read fNeedsPassword write fNeedsPassword;
    property UsePython3K : boolean read fUsePython3K write fUsePython3K;
    property PythonCommand : string read fPythonCommand write fPythonCommand;
  end;

Var
  TSSHConfigurations : TCollection;


implementation

{ TSSHConfig }

procedure TSSHConfig.Assign(Source: TPersistent);
begin
  if Source is TSSHConfig then with TSSHConfig(Source) do begin
    Self.fName := Name;
    Self.fHostName := HostName;
    Self.UserName := UserName;
    Self.fNeedsPassword := NeedsPassword;
    Self.Password := Password;
    Self.fUsePython3K := UsePython3K;
    Self.fPythonCommand := PythonCommand;
  end else
    inherited;
end;

constructor TSSHConfig.Create(Collection: TCollection);
begin
  inherited;
  PythonCommand := 'python';
end;

function TSSHConfig.GetDisplayName: string;
begin
  if Name <> '' then
    Result := Name
  else begin
    Result := HostName;
    if UserName <> '' then
      Result := UserName + '@' + HostName;
  end;
end;

initialization
  TSSHConfigurations := TCollection.Create(TSSHConfig);
finalization
  TSSHConfigurations.Free;
end.
