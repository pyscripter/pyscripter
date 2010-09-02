{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAppIniStorage.pas, released on --.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Marcel
Bestebroer
All Rights Reserved.

Contributor(s):
  Jens Fudickar
  Olivier Sannier

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvAppIniStorage;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, SysUtils, Classes, IniFiles,
  JclBase,
  JvAppStorage, JvPropertyStore, JvTypes;

type
  TJvAppIniStorageOptions = class(TJvAppStorageOptions)
  private
    FReplaceCRLF: Boolean;
    FPreserveLeadingTrailingBlanks: Boolean;
  protected
    procedure SetReplaceCRLF(Value: Boolean); virtual;
    procedure SetPreserveLeadingTrailingBlanks(Value: Boolean); virtual;
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ReplaceCRLF: Boolean read FReplaceCRLF write SetReplaceCRLF default False;
    property PreserveLeadingTrailingBlanks: Boolean read FPreserveLeadingTrailingBlanks
      write SetPreserveLeadingTrailingBlanks default False;
    property FloatAsString default False;
  end;

  // Storage to INI file, all in memory. This is the base class
  // for INI type storage, descendents will actually implement
  // the writing to a file or anything else
  TJvCustomAppIniStorage = class(TJvCustomAppMemoryFileStorage)
  private
    FIniFile: TMemIniFile;
    FDefaultSection: string;
    function CalcDefaultSection(Section: string): string;
    function GetStorageOptions: TJvAppIniStorageOptions;
    procedure SetStorageOptions(Value: TJvAppIniStorageOptions);
    {$IFDEF UNICODE}
    function GetEncoding: TEncoding;
    procedure SetEncoding(const Value: TEncoding);
    {$ENDIF UNICODE}
  protected
    class function GetStorageOptionsClass: TJvAppStorageOptionsClass; override;

    // Replaces all CRLF through "\n"
    function ReplaceCRLFToSlashN(const Value: string): string;
    // Replaces all "\n" through CRLF
    function ReplaceSlashNToCRLF(const Value: string): string;
    // Adds " at the beginning and the end
    function SaveLeadingTrailingBlanks(const Value: string): string;
    // Removes " at the beginning and the end
    function RestoreLeadingTrailingBlanks(const Value: string): string;

    function GetAsString: string; override;
    procedure SetAsString(const Value: string); override;
    function DefaultExtension: string; override;

    procedure EnumFolders(const Path: string; const Strings: TStrings;
      const ReportListAsValue: Boolean = True); override;
    procedure EnumValues(const Path: string; const Strings: TStrings;
      const ReportListAsValue: Boolean = True); override;
    function PathExistsInt(const Path: string): Boolean; override;
    function ValueExists(const Section, Key: string): Boolean;
    function IsFolderInt(const Path: string; ListIsValue: Boolean = True): Boolean; override;
    function ReadValue(const Section, Key: string): string; virtual;
    procedure WriteValue(const Section, Key, Value: string); virtual;
    procedure RemoveValue(const Section, Key: string); virtual;
    procedure DeleteSubTreeInt(const Path: string); override;
    procedure SplitKeyPath(const Path: string; out Key, ValueName: string); override;
    function ValueStoredInt(const Path: string): Boolean; override;
    procedure DeleteValueInt(const Path: string); override;
    function DoReadInteger(const Path: string; Default: Integer): Integer; override;
    procedure DoWriteInteger(const Path: string; Value: Integer); override;
    function DoReadFloat(const Path: string; Default: Extended): Extended; override;
    procedure DoWriteFloat(const Path: string; Value: Extended); override;
    function DoReadString(const Path: string; const Default: string): string; override;
    procedure DoWriteString(const Path: string; const Value: string); override;
    function DoReadBinary(const Path: string; Buf: TJvBytes; BufSize: Integer): Integer; override;
    procedure DoWriteBinary(const Path: string; const Buf: TJvBytes; BufSize: Integer); override;
    property DefaultSection: string read FDefaultSection write FDefaultSection;
    property IniFile: TMemIniFile read FIniFile;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF UNICODE}
    property Encoding: TEncoding read GetEncoding write SetEncoding;
    {$ENDIF UNICODE}
  published
    property StorageOptions: TJvAppIniStorageOptions read GetStorageOptions write SetStorageOptions;
  end;

  // This class handles the flushing into a disk file
  // and publishes a few properties for them to be
  // used by the user in the IDE
  TJvAppIniFileStorage = class(TJvCustomAppIniStorage)
  private
    procedure FlushInternal;
    procedure ReloadInternal;
  public
    procedure Flush; override;
    procedure Reload; override;
    property AsString;
    property IniFile;
  published
    property AutoFlush;
    property AutoReload;
    property FileName;
    property FlushOnDestroy;
    property Location;
    property DefaultSection;
    property SubStorages;
    property OnGetFileName;
    //1 Synchronize the Flush and Reload procedure
    /// Defines if the execution of flush and reload for the current
    /// File should be synchronized via a global mutex
    property SynchronizeFlushReload;
  end;

procedure StorePropertyStoreToIniFile(APropertyStore: TJvCustomPropertyStore;
    const AFileName: string; const AAppStoragePath: string = ''; const
    ADefaultSection: string = ''; AStorageOptions: TJvCustomAppStorageOptions =
    nil);
procedure LoadPropertyStoreFromIniFile(APropertyStore: TJvCustomPropertyStore;
    const AFileName: string; const AAppStoragePath: string = ''; const
    ADefaultSection: string = ''; AStorageOptions: TJvCustomAppStorageOptions =
    nil);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  JvJCLUtils, // BinStrToBuf & BufToBinStr
  JvConsts, JvResources,
  JclStrings; // JvConsts or PathDelim under D5 and BCB5

const
  cNullDigit = '0';
  cCount = 'Count';
  cSectionHeaderStart = '[';
  cSectionHeaderEnd = ']';
  cKeyValueSeparator = '=';

//=== { TJvAppIniStorageOptions } ============================================

constructor TJvAppIniStorageOptions.Create;
begin
  inherited Create;
  FReplaceCRLF := False;
  FPreserveLeadingTrailingBlanks := False;
  FloatAsString := False;
end;

procedure TJvAppIniStorageOptions.Assign(Source: TPersistent);
begin
  if (Source = Self) then
    Exit;
  if Source is TJvAppIniStorageOptions then
  begin
    ReplaceCRLF := TJvAppIniStorageOptions(Source).ReplaceCRLF;
    PreserveLeadingTrailingBlanks := TJvAppIniStorageOptions(Source).PreserveLeadingTrailingBlanks;
  end;
  inherited Assign(Source);
end;

procedure TJvAppIniStorageOptions.SetReplaceCRLF(Value: Boolean);
begin
  FReplaceCRLF := Value;
end;

procedure TJvAppIniStorageOptions.SetPreserveLeadingTrailingBlanks(Value: Boolean);
begin
  FPreserveLeadingTrailingBlanks := Value;
end;

{ Optimalization of TCustomIniFile.ValueExists is only done for Delphi 7; Probably
  works the same for other versions, but I can't check that.
  Note that this is a dirty hack, a better way would be to rewrite TMemIniFile;
  especially expose FSections, but other optimizations can be done also.
  For example TCustomIniFile.SectionExists}
{$IFDEF DELPHI7}
type
  TJvMemIniFile = class(TMemIniFile)
  public
    function DoesValueExists(const Section, Ident: string): Boolean;
  end;

  TMemIniFileAccessPrivate = class(TCustomIniFile)
  public
    FSections: TStringList;
  end;

function TJvMemIniFile.DoesValueExists(const Section, Ident: string): Boolean;
var
  I: Integer;
  Strings: TStrings;
begin
  I := TMemIniFileAccessPrivate(Self).FSections.IndexOf(Section);
  if I >= 0 then
  begin
    Strings := TStrings(TMemIniFileAccessPrivate(Self).FSections.Objects[I]);
    I := Strings.IndexOfName(Ident);
    Result := I >= 0;
  end else
    Result := False;
end;
{$ENDIF DELPHI7}

//=== { TJvCustomAppIniStorage } =============================================

constructor TJvCustomAppIniStorage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF DELPHI7}
  FIniFile := TJvMemIniFile.Create(Name);
  {$ELSE}
  FIniFile := TMemIniFile.Create(Name);
  {$ENDIF DELPHI7}
end;

destructor TJvCustomAppIniStorage.Destroy;
begin
  inherited Destroy;
  // Has to be done AFTER inherited, see comment in
  // TJvCustomAppMemoryFileStorage
  FIniFile.Free;
end;

// Replaces all CRLF through "\n"
// (rom) probably better use JclStrings.StrEscapedToString and StrStringToEscaped
function TJvCustomAppIniStorage.ReplaceCRLFToSlashN(const Value: string): string;
begin
  if (Pos(#13, Value) > 0) or (Pos(#10, Value) > 0) then
  begin
    Result := StringReplace(Value, '\', '\\', [rfReplaceAll]);
    Result := StringReplace(Result , #13#10, '\n', [rfReplaceAll]);
    Result := StringReplace(Result , #10, '\n', [rfReplaceAll]);
    Result := StringReplace(Result , #13, '\n', [rfReplaceAll]);
  end
  else
    Result := Value;
end;

// Replaces all "\n" through CRLF
function TJvCustomAppIniStorage.ReplaceSlashNToCRLF(const Value: string): string;
var
  P: Integer;
  C1, C2: Char;

  function GetNext: Boolean;
  begin
    Result := Length(Value) >= P;
    if Result then
    begin
      C1 := Value[P];
      C2 := Value[P + 1];
    end;
    Inc(P);
  end;

begin
  P := 1;
  C1 := #0;
  C2 := #0;
  while GetNext do
  begin
    if (C1 = '\') and (C2 = '\') then
    begin
      Result := Result + C1;
      Inc(P);
    end
    else
    if (C1 = '\') and (C2 = 'n') then
    begin
      Result := Result + #13#10;
      Inc(P);
    end
    else
      Result := Result + C1;
  end;
end;

// Adds " at the beginning and the end
function TJvCustomAppIniStorage.SaveLeadingTrailingBlanks(const Value: string): string;
var
  C1, C2: Char;
begin
  if Value = '' then
    Result := ''
  else
  begin
    C1 := Value[1];
    C2 := Value[Length(Value)];
    if (C1 = ' ') or (C2 = ' ') or
      ((C1 = '"') and (C2 = '"')) then
      Result := '"' + Value + '"'
    else
      Result := Value;
  end;
end;

// Removes " at the beginning and the end
function TJvCustomAppIniStorage.RestoreLeadingTrailingBlanks(const Value: string): string;
begin
  if (Length(Value)>=2) and (Value[1] = '"') and (Value[Length(Value)] = '"') then
    Result := Copy(Value, 2, Length(Value) - 2)
  else
    Result := Value;
end;

procedure TJvCustomAppIniStorage.SplitKeyPath(const Path: string; out Key, ValueName: string);
begin
  inherited SplitKeyPath(Path, Key, ValueName);
  if Key = '' then
    Key := DefaultSection;
end;

function TJvCustomAppIniStorage.ValueStoredInt(const Path: string): Boolean;
var
  Section: string;
  Key: string;
begin
  SplitKeyPath(Path, Section, Key);
  Result := ValueExists(Section, Key);
end;

procedure TJvCustomAppIniStorage.DeleteValueInt(const Path: string);
var
  Section: string;
  Key: string;
begin
  SplitKeyPath(Path, Section, Key);
  RemoveValue(Section, Key);
end;

function TJvCustomAppIniStorage.DoReadInteger(const Path: string; Default: Integer): Integer;
var
  Section: string;
  Key: string;
  Value: string;
begin
  SplitKeyPath(Path, Section, Key);
  if ValueExists(Section, Key) then
  begin
    Value := ReadValue(Section, Key);
    if Value = '' then
      Value := cNullDigit;
    Result := StrToInt(Value);
  end
  else
    Result := Default;
end;

procedure TJvCustomAppIniStorage.DoWriteInteger(const Path: string; Value: Integer);
var
  Section: string;
  Key: string;
begin
  SplitKeyPath(Path, Section, Key);
  WriteValue(Section, Key, IntToStr(Value));
end;

function TJvCustomAppIniStorage.DoReadFloat(const Path: string; Default: Extended): Extended;
var
  Section: string;
  Key: string;
  Value: string;
begin
  SplitKeyPath(Path, Section, Key);
  if ValueExists(Section, Key) then
  begin
    Value := ReadValue(Section, Key);
    if BinStrToBuf(Value, @Result, SizeOf(Result)) <> SizeOf(Result) then
      Result := Default;
  end
  else
    Result := Default;
end;

procedure TJvCustomAppIniStorage.DoWriteFloat(const Path: string; Value: Extended);
var
  Section: string;
  Key: string;
begin
  SplitKeyPath(Path, Section, Key);
  WriteValue(Section, Key, BufToBinStr(@Value, SizeOf(Value)));
end;

function TJvCustomAppIniStorage.DoReadString(const Path: string; const Default: string): string;
var
  Section: string;
  Key: string;
begin
  SplitKeyPath(Path, Section, Key);
  if ValueExists(Section, Key) then
    Result := ReadValue(Section, Key)
  else
    Result := Default;
end;

procedure TJvCustomAppIniStorage.DoWriteString(const Path: string; const Value: string);
var
  Section: string;
  Key: string;
begin
  SplitKeyPath(Path, Section, Key);
  WriteValue(Section, Key, Value);
end;

function TJvCustomAppIniStorage.DoReadBinary(const Path: string; Buf: TJvBytes; BufSize: Integer): Integer;
var
  Section: string;
  Key: string;
  Value: string;
begin
  SplitKeyPath(Path, Section, Key);
  if ValueExists(Section, Key) then
  begin
    Value := ReadValue(Section, Key);
    Result := BinStrToBuf(Value, Buf, BufSize);
  end
  else
    Result := 0;
end;

procedure TJvCustomAppIniStorage.DoWriteBinary(const Path: string; const Buf: TJvBytes; BufSize: Integer);
var
  Section: string;
  Key: string;
begin
  SplitKeyPath(Path, Section, Key);
  WriteValue(Section, Key, BufToBinStr(Buf, BufSize));
end;

procedure TJvCustomAppIniStorage.EnumFolders(const Path: string; const Strings: TStrings;
  const ReportListAsValue: Boolean);
var
  RefPath: string;
  I: Integer;
  TempStrings : tStrings;
  s : String;
  p : Integer;
  lr : Integer;
begin
  TempStrings := TStringlist.Create;
  Strings.BeginUpdate;
  try
    RefPath := GetAbsPath(Path);
    ReloadIfNeeded;
    IniFile.ReadSections(TempStrings);
    lr := Length(RefPath);
    for i := 0 to TempStrings.Count - 1 do
    begin
      s := TempStrings[i];
      if (RefPath <> '') and (Copy(s, 1, lr + 1) <> RefPath + PathDelim) then
        Continue;
      if ReportListAsValue and ValueExists(s, cCount) then
        Continue;
      if RefPath <> '' then
        s := Copy(s, 2 + lr, Length(s) - lr);
      p := Pos(PathDelim, s);
      if p > 0 then
        s := Copy(s, 1, p-1);
      if (RefPath = '') and (s = DefaultSection) then
        Continue;
      if Strings.IndexOf(s) < 0 then
        Strings.Add(s);
    end;
  finally
    Strings.EndUpdate;
    TempStrings.Free;
  end;
end;

procedure TJvCustomAppIniStorage.EnumValues(const Path: string; const Strings: TStrings;
  const ReportListAsValue: Boolean);
var
  PathIsList: Boolean;
  RefPath: string;
  I: Integer;
begin
  Strings.BeginUpdate;
  try
    PathIsList := ReportListAsValue and ListStored(Path);
    RefPath := GetAbsPath(Path);
    if RefPath = '' then
      RefPath := DefaultSection;
    ReloadIfNeeded;
    IniFile.ReadSectionValues(RefPath, Strings);
    for I := Strings.Count - 1 downto 0 do
    begin
      Strings[I] := Copy(Strings[I], 1, Pos(cKeyValueSeparator, Strings[I]) - 1);
      if PathIsList and (AnsiSameText(cCount, Strings[I]) or NameIsListItem(Strings[I])) then
        Strings.Delete(I);
    end;
    if PathIsList then
      Strings.Add('');
  finally
    Strings.EndUpdate;
  end;
end;


function TJvCustomAppIniStorage.CalcDefaultSection(Section: string): string;
begin
  if (Section = '') or (Section[1] = '.') then
    Result := DefaultSection + Section
  else
    Result := Section;
  if (Result = '') or (Result[1] = '.') then
    raise EJVCLAppStorageError.CreateRes(@RsEReadValueFailed);
end;

function TJvCustomAppIniStorage.GetStorageOptions: TJvAppIniStorageOptions;
begin
  Result := TJvAppIniStorageOptions(inherited StorageOptions);
end;

procedure TJvCustomAppIniStorage.SetStorageOptions(Value: TJvAppIniStorageOptions);
begin
  (Inherited StorageOptions).Assign(Value);
end;

function TJvCustomAppIniStorage.ValueExists(const Section, Key: string): Boolean;
begin
  if IniFile <> nil then
  begin
    ReloadIfNeeded;
    {$IFDEF DELPHI7}
    Result := TJvMemIniFile(IniFile).DoesValueExists(CalcDefaultSection(Section), Key);
    {$ELSE}
    Result := IniFile.ValueExists(CalcDefaultSection(Section), Key);
    {$ENDIF DELPHI7}
  end
  else
    Result := False;
end;

function TJvCustomAppIniStorage.ReadValue(const Section, Key: string): string;
begin
  if IniFile <> nil then
  begin
    ReloadIfNeeded;
    if TJvAppIniStorageOptions(StorageOptions).ReplaceCRLF then
      Result := ReplaceSlashNToCRLF(IniFile.ReadString(CalcDefaultSection(Section), Key, ''))
    else
      Result := IniFile.ReadString(CalcDefaultSection(Section), Key, '');
    if TJvAppIniStorageOptions(StorageOptions).PreserveLeadingTrailingBlanks then
      Result := RestoreLeadingTrailingBlanks(Result);
  end
  else
    Result := '';
end;

procedure TJvCustomAppIniStorage.WriteValue(const Section, Key, Value: string);
begin
  if IniFile <> nil then
  begin
    ReloadIfNeeded;
    if TJvAppIniStorageOptions(StorageOptions).PreserveLeadingTrailingBlanks then
      if TJvAppIniStorageOptions(StorageOptions).ReplaceCRLF then
        IniFile.WriteString(CalcDefaultSection(Section), Key,
          SaveLeadingTrailingBlanks(ReplaceCRLFToSlashN(Value)))
      else
        IniFile.WriteString(CalcDefaultSection(Section), Key,
          SaveLeadingTrailingBlanks(Value))
    else
      if TJvAppIniStorageOptions(StorageOptions).ReplaceCRLF then
        IniFile.WriteString(CalcDefaultSection(Section), Key, ReplaceCRLFToSlashN(Value))
      else
        IniFile.WriteString(CalcDefaultSection(Section), Key, Value);
    FlushIfNeeded;
  end;
end;

procedure TJvCustomAppIniStorage.DeleteSubTreeInt(const Path: string);
var
  TopSection: string;
  Sections: TStringList;
  I: Integer;
begin
  if IniFile <> nil then
  begin
    TopSection := GetAbsPath(Path);
    Sections := TStringList.Create;
    try
      if AutoReload and not IsUpdating then
        Reload;
      IniFile.ReadSections(Sections);
      if TopSection = '' then
        for I := 0 to Sections.Count - 1 do
          IniFile.EraseSection(Sections[I])
      else
        for I := 0 to Sections.Count - 1 do
          if Pos(TopSection, Sections[I]) = 1 then
            IniFile.EraseSection(Sections[I]);
      FlushIfNeeded;
    finally
      Sections.Free;
    end;
  end;
end;

procedure TJvCustomAppIniStorage.RemoveValue(const Section, Key: string);
var
  LSection: string;
begin
  if IniFile <> nil then
  begin
    ReloadIfNeeded;
    LSection := CalcDefaultSection(Section);
    if IniFile.ValueExists(LSection, Key) then
    begin
      IniFile.DeleteKey(LSection, Key);
      FlushIfNeeded;
    end
    else
    if IniFile.SectionExists(LSection + PathDelim + Key) then
    begin
      IniFile.EraseSection(LSection + PathDelim + Key);
      FlushIfNeeded;
    end;
  end;
end;

function TJvCustomAppIniStorage.PathExistsInt(const Path: string): Boolean;
begin
  ReloadIfNeeded;
  Result := IniFile.SectionExists(StrEnsureNoPrefix(PathDelim, Path));
end;

function TJvCustomAppIniStorage.IsFolderInt(const Path: string; ListIsValue: Boolean): Boolean;
var
  RefPath: string;
  ValueNames: TStringList;
  Sections : TStringList;
  I: Integer;
begin
  RefPath := GetAbsPath(Path);
  if RefPath = '' then
    RefPath := DefaultSection;
  ReloadIfNeeded;
  if IniFile.SectionExists(RefPath) then
    if ListIsValue and IniFile.ValueExists(RefPath, cCount) then
    begin
      ValueNames := TStringList.Create;
      try
        EnumValues(Path, ValueNames, True);
        I := ValueNames.Count - 1;
        Result := i > 0;
        while Result and (I >= 0) do
        begin
          Result := not AnsiSameText(ValueNames[I], cCount) and not NameIsListItem(ValueNames[I]);
          Dec(I);
        end;
      finally
        ValueNames.Free;
      end;
    end
    else
      Result := True
  else
  begin
    Sections := tStringList.Create;
    try
      IniFile.ReadSections(Sections);
      for i := 0 to Sections.Count - 1 do
        if Pos(RefPath+PathDelim, Sections[i]) = 1  then
        begin
          Result := True;
          Exit;
        end;
      Result := False;
    finally
      Sections.Free;
    end;
  end;
end;

class function TJvCustomAppIniStorage.GetStorageOptionsClass: TJvAppStorageOptionsClass;
begin
  Result := TJvAppIniStorageOptions;
end;

function TJvCustomAppIniStorage.GetAsString: string;
var
  TmpList: TStringList;
begin
  TmpList := TStringList.Create;
  try
    IniFile.GetStrings(TmpList);
    Result := TmpList.Text;
  finally
    TmpList.Free;
  end;
end;

{$IFDEF UNICODE}
function TJvCustomAppIniStorage.GetEncoding: TEncoding;
begin
  Result := FIniFile.Encoding;
end;

procedure TJvCustomAppIniStorage.SetEncoding(const Value: TEncoding);
begin
  FIniFile.Encoding := Value;
end;
{$ENDIF UNICODE}

procedure TJvCustomAppIniStorage.SetAsString(const Value: string);
var
  TmpList: TStringList;
begin
  TmpList := TStringList.Create;
  try
    TmpList.Text := Value;
    IniFile.SetStrings(TmpList);
  finally
    TmpList.Free;
  end;
end;

function TJvCustomAppIniStorage.DefaultExtension: string;
begin
  Result := 'ini';
end;

//=== { TJvAppIniFileStorage } ===============================================

procedure TJvAppIniFileStorage.Flush;
var
  Path: string;
begin
  if (FullFileName <> '') and not ReadOnly and not (csDesigning in ComponentState) then
  begin
    try
      Path := ExtractFilePath(IniFile.FileName);
      if Path <> '' then
        ForceDirectories(Path);
      if SynchronizeFlushReload then
        Synchronize(FlushInternal, FullFileName)
      else
        FlushInternal;
    except
      on E: Exception do
        DoError(E.Message);
    end;
  end;
end;

procedure TJvAppIniFileStorage.FlushInternal;
begin
  IniFile.Rename(FullFileName, False);
  IniFile.UpdateFile;
end;

procedure TJvAppIniFileStorage.Reload;
begin
  if not IsUpdating and not (csDesigning in ComponentState) then
  begin
    inherited Reload;
    if FileExists(FullFileName) then
      if SynchronizeFlushReload then
        Synchronize(ReloadInternal, FullFileName)
      else
        ReloadInternal
    else  // file may have disappeared. If so, clear the file
      IniFile.Clear;
  end;
end;

procedure TJvAppIniFileStorage.ReloadInternal;
begin
  IniFile.Rename(FullFileName, True);
end;

//=== { Common procedures } ==================================================

procedure StorePropertyStoreToIniFile(APropertyStore: TJvCustomPropertyStore;
    const AFileName: string; const AAppStoragePath: string = ''; const
    ADefaultSection: string = ''; AStorageOptions: TJvCustomAppStorageOptions =
    nil);
var
  AppStorage: TJvAppIniFileStorage;
  SaveAppStorage: TJvCustomAppStorage;
  SaveAppStoragePath: string;
begin
  if not Assigned(APropertyStore) then
    Exit;
  AppStorage := TJvAppIniFileStorage.Create(nil);
  try
    if Assigned(AStorageOptions) then
      AppStorage.StorageOptions.Assign(AStorageOptions);
    AppStorage.Location := flCustom;
    AppStorage.FileName := AFileName;
    AppStorage.DefaultSection := ADefaultSection;
    SaveAppStorage := APropertyStore.AppStorage;
    SaveAppStoragePath := APropertyStore.AppStoragePath;
    try
      APropertyStore.AppStoragePath := AAppStoragePath;
      APropertyStore.AppStorage := AppStorage;
      APropertyStore.StoreProperties;
    finally
      APropertyStore.AppStoragePath := SaveAppStoragePath;
      APropertyStore.AppStorage := SaveAppStorage;
    end;
  finally
    AppStorage.Free;
  end;
end;

procedure LoadPropertyStoreFromIniFile(APropertyStore: TJvCustomPropertyStore;
    const AFileName: string; const AAppStoragePath: string = ''; const
    ADefaultSection: string = ''; AStorageOptions: TJvCustomAppStorageOptions =
    nil);
var
  AppStorage: TJvAppIniFileStorage;
  SaveAppStorage: TJvCustomAppStorage;
  SaveAppStoragePath: string;
begin
  if not Assigned(APropertyStore) then
    Exit;
  AppStorage := TJvAppIniFileStorage.Create(nil);
  try
    if Assigned(AStorageOptions) then
      AppStorage.StorageOptions.Assign(AStorageOptions);
    AppStorage.Location := flCustom;
    AppStorage.FileName := AFileName;
    AppStorage.DefaultSection := ADefaultSection;
    SaveAppStorage := APropertyStore.AppStorage;
    SaveAppStoragePath := APropertyStore.AppStoragePath;
    try
      APropertyStore.AppStoragePath := AAppStoragePath;
      APropertyStore.AppStorage := AppStorage;
      APropertyStore.LoadProperties;
    finally
      APropertyStore.AppStoragePath := SaveAppStoragePath;
      APropertyStore.AppStorage := SaveAppStorage;
    end;
  finally
    AppStorage.Free;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
