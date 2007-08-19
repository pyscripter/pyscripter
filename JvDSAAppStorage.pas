unit JvDSAAppStorage;

interface
uses
  JvDSADialogs, JvAppStorage;

type
  TJvDSAAppStorage = class(TDSAStorage)
  private
    FAppStorage : TJvCustomAppStorage;
    FPath : string;
  protected
    function GetCheckMarkTextSuffix: string; override;
    procedure SetCheckMarkTextSuffix(const Value: string); override;
  public
    constructor Create(AAppStorage : TJvCustomAppStorage; const APath : string);
    function ReadBool(const DSAInfo: TDSARegItem; const Key: string): Boolean; override;
    function ReadBoolDef(const DSAInfo: TDSARegItem; const Key: string; const Default: Boolean): Boolean; override;
    function ReadFloat(const DSAInfo: TDSARegItem; const Key: string): Extended; override;
    function ReadFloatDef(const DSAInfo: TDSARegItem; const Key: string; const Default: Extended): Extended; override;
    function ReadInt64(const DSAInfo: TDSARegItem; const Key: string): Int64; override;
    function ReadInt64Def(const DSAInfo: TDSARegItem; const Key: string; const Default: Int64): Int64; override;
    function ReadInteger(const DSAInfo: TDSARegItem; const Key: string): Integer; override;
    function ReadIntegerDef(const DSAInfo: TDSARegItem; const Key: string; const Default: Integer): Integer; override;
    function ReadString(const DSAInfo: TDSARegItem; const Key: string): string; override;
    function ReadStringDef(const DSAInfo: TDSARegItem; const Key: string; const Default: string): string; override;
    procedure WriteBool(const DSAInfo: TDSARegItem; const Key: string; const Value: Boolean); override;
    procedure WriteFloat(const DSAInfo: TDSARegItem; const Key: string; const Value: Extended); override;
    procedure WriteInt64(const DSAInfo: TDSARegItem; const Key: string; const Value: Int64); override;
    procedure WriteInteger(const DSAInfo: TDSARegItem; const Key: string; const Value: Integer); override;
    procedure WriteString(const DSAInfo: TDSARegItem; const Key: string; const Value: string); override;
    property AppStorage: TJvCustomAppStorage read FAppStorage write FAppStorage;
    property Path: string read FPath write FPath;
  end;


implementation
Uses
  SysUtils;

const
  sPathStr = '%s\%s\%s';

constructor TJvDSAAppStorage.Create(AAppStorage : TJvCustomAppStorage; const APath : string);
begin
  inherited Create;
  FAppStorage := AAppStorage;
  FPath := APath;
end;

function TJvDSAAppStorage.GetCheckMarkTextSuffix: string;
begin
  Result := '';
end;

procedure TJvDSAAppStorage.SetCheckMarkTextSuffix(const Value: string);
begin
end;

function TJvDSAAppStorage.ReadBool(const DSAInfo: TDSARegItem; const Key: string): Boolean;
begin
  Result := FAppStorage.ReadBoolean(Format(sPathStr, [FPath, DSAInfo.Name, Key]));
end;

function TJvDSAAppStorage.ReadBoolDef(const DSAInfo: TDSARegItem; const Key: string;
  const Default: Boolean): Boolean;
begin
  Result := FAppStorage.ReadBoolean(Format(sPathStr, [FPath, DSAInfo.Name, Key]), Default);
end;

function TJvDSAAppStorage.ReadFloat(const DSAInfo: TDSARegItem; const Key: string): Extended;
begin
  Result := FAppStorage.ReadFloat(Format(sPathStr, [FPath, DSAInfo.Name, Key]));
end;

function TJvDSAAppStorage.ReadFloatDef(const DSAInfo: TDSARegItem; const Key: string;
  const Default: Extended): Extended;
begin
  Result := FAppStorage.ReadFloat(Format(sPathStr, [FPath, DSAInfo.Name, Key]), Default);
end;

function TJvDSAAppStorage.ReadInt64(const DSAInfo: TDSARegItem; const Key: string): Int64;
begin
  Result := FAppStorage.ReadInteger(Format(sPathStr, [FPath, DSAInfo.Name, Key]));
end;

function TJvDSAAppStorage.ReadInt64Def(const DSAInfo: TDSARegItem; const Key: string; const Default: Int64): Int64;
begin
  Result := FAppStorage.ReadInteger(Format(sPathStr, [FPath, DSAInfo.Name, Key]), Default);
end;

function TJvDSAAppStorage.ReadInteger(const DSAInfo: TDSARegItem; const Key: string): Integer;
begin
  Result := FAppStorage.ReadInteger(Format(sPathStr, [FPath, DSAInfo.Name, Key]));
end;

function TJvDSAAppStorage.ReadIntegerDef(const DSAInfo: TDSARegItem; const Key: string;
  const Default: Integer): Integer;
begin
  Result := FAppStorage.ReadInteger(Format(sPathStr, [FPath, DSAInfo.Name, Key]), Default);
end;

function TJvDSAAppStorage.ReadString(const DSAInfo: TDSARegItem; const Key: string): string;
begin
  Result := FAppStorage.ReadString(Format(sPathStr, [FPath, DSAInfo.Name, Key]));
end;

function TJvDSAAppStorage.ReadStringDef(const DSAInfo: TDSARegItem; const Key: string;
  const Default: string): string;
begin
  Result := FAppStorage.ReadString(Format(sPathStr, [FPath, DSAInfo.Name, Key]), Default);
end;

procedure TJvDSAAppStorage.WriteBool(const DSAInfo: TDSARegItem; const Key: string;
  const Value: Boolean);
begin
  FAppStorage.WriteBoolean(Format(sPathStr, [FPath, DSAInfo.Name, Key]), Value);
end;

procedure TJvDSAAppStorage.WriteFloat(const DSAInfo: TDSARegItem; const Key: string;
  const Value: Extended);
begin
  FAppStorage.WriteFloat(Format(sPathStr, [FPath, DSAInfo.Name, Key]), Value);
end;

procedure TJvDSAAppStorage.WriteInt64(const DSAInfo: TDSARegItem; const Key: string;
  const Value: Int64);
begin
  FAppStorage.WriteInteger(Format(sPathStr, [FPath, DSAInfo.Name, Key]), Value);
end;

procedure TJvDSAAppStorage.WriteInteger(const DSAInfo: TDSARegItem; const Key: string;
  const Value: Integer);
begin
  FAppStorage.WriteInteger(Format(sPathStr, [FPath, DSAInfo.Name, Key]), Value);
end;

procedure TJvDSAAppStorage.WriteString(const DSAInfo: TDSARegItem; const Key: string;
  const Value: string);
begin
  FAppStorage.WriteString(Format(sPathStr, [FPath, DSAInfo.Name, Key]), Value);
end;


end.
