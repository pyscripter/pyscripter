{Copyright (C) 2006  Benito van der Zander

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
}
unit uCmdLine;

interface
{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

uses
  Sysutils; //for exceptions

type
  TStringArray = array of string;
  TLongintArray = array of Longint;
  TFloatArray = array of Extended;
  TBooleanArray = array of Boolean;
  TCommandLineReaderShowError = procedure(errorDescription: string);
  ECommandLineParseException = class(Exception);
  TKindOfProperty = (kpStr, kpFile, kpInt, kpFloat, kpFlag);
  TProperty = record
    name, desc, strvalue: string;
    found: Boolean;
    case kind: TKindOfProperty of
      kpStr, kpFile: ();
      kpInt: (intvalue: Longint);
      kpFloat: (floatvalue: Extended);
      kpFlag: (flagvalue, flagdefault: Boolean;
        abbreviation: Char)
  end;
  PProperty = ^TProperty;

  { TCommandLineReader }

  TCommandLineReader = class
  protected
    Parsed: Boolean;
    propertyArray: array of TProperty;
    nameless: TStringArray;
    function FindProperty(Name: string): PProperty;
    function DeclareProperty(Name, Description, Default: string; kind:
      TKindOfProperty): PProperty;
  public
    OnShowError: TCommandLineReaderShowError;
    AutomaticalShowError: Boolean;
    AllowDOSStyle: Boolean;

    constructor Create;

    function AvaibleOptions: string;

    procedure Parse(); overload;
    procedure Parse(const s: string); overload;

    //DeclareFlag allows the use of flags
    //Example:
    //   declareFlag('flag','f',true);
    //  Following command-line options are always possible
    //    --enable-flag      =>     flag:=true
    //    --disable-flag     =>     flag:=false
    //    --flag             =>     flag:=not default
    //    -xfy               =>     flag:=not default
    procedure DeclareFlag(const Name, Description: string; FlagNameAbbreviation:
      Char; Default: Boolean = False); overload;
    procedure DeclareFlag(const Name, Description: string; Default: Boolean =
      False); overload;

    //declareFile allows the use of a file name
    //Example:
    //   declareFile('file');
    //  Following command-line options are  possible
    //    --file C:\test                  =>     file:=C:\test
    //    --file 'C:\test'                =>     file:=C:\test
    //    --file "C:\test"                =>     file:=C:\test
    //    --file='C:\test'                =>     file:=C:\test
    //    --file="C:\test"                =>     file:=C:\test
    //    --file C:\Eigene Dateien\a.bmp  =>     file:=C:\Eigene
    //                                           or file:=C:\Eigene Dateien\a.bmp,
    //                                             if C:\Eigene does not exist
    procedure DeclareFile(const Name, Description: string; Default: string =
      '');

    //DeclareXXXX allows the use of string, int, float, ...
    //Example:
    //   declareString('property');
    //  Following command-line options are  possible
    //    --file 123                  =>     file:=123
    //    --file '123'                =>     file:=123
    //    --file "123"                =>     file:=123
    //    --file='123'                =>     file:=123
    //    --file="123"                =>     file:=123

    procedure DeclareString(const Name, Description: string; Value: string =
      '');
    procedure DeclareInt(const Name, Description: string; Value: Longint = 0);
    procedure DeclareFloat(const Name, Description: string; Value: Extended =
      0);

    function ReadString(const Name: string): string; overload;
    function ReadInt(const Name: string): Longint; overload;
    function ReadFloat(const Name: string): Extended; overload;
    function ReadFlag(const Name: string): Boolean; overload;

    function ExistsProperty(const Name: string): Boolean;

    function ReadNamelessFiles(): TStringArray;
    function ReadNamelessString(): TStringArray;
    function ReadNamelessInt(): TLongintArray;
    function ReadNamelessFloat(): TFloatArray;
    function ReadNamelessFlag(): TBooleanArray;
  end;

var
  CmdLineReader: TCommandLineReader;

implementation
uses
  Windows, JvGnugettext, StringResources;

const
  Null = Char(#0);
  Tab = Char(#9);
  Space = Char(#32);
  Quote = Char(#34);

constructor TCommandLineReader.Create;
begin
  Parsed := false;
{$IFDEF Win32}
  AllowDOSStyle := true;
{$ELSE}
  AllowDOSStyle := false;
{$ENDIF}
  OnShowError := nil;
  {searchNameLessFile:=false;
  searchNameLessInt:=false;
  searchNameLessFloat:=false;
  searchNameLessFlag:=false;}
  AutomaticalShowError := not IsLibrary;
end;

function TCommandLineReader.AvaibleOptions: string;
begin
  Result := '';
  for var I := 0 to high(propertyArray) do
  begin
    Result := Result + '--' + propertyArray[I].name;
    case propertyArray[I].kind of
      kpFlag: if propertyArray[I].abbreviation <> #0 then
          Result := Result + ' or -' + propertyArray[I].abbreviation;
    else
      Result := Result + '=';
    end;
    Result := Result + Tab + Tab + propertyArray[I].desc + #13#10;
  end;
end;

procedure TCommandLineReader.Parse();
var
  Params: string;
  P: Integer;
begin
  Params := GetCommandLineW;
  if Params = '' then
    Exit;
  if Params[1] = Quote then
  begin
    Params[1] := 'X';
    delete(Params, 1, Pos(Quote, Params));
  end
  else
  begin
    P := Pos(Space, Params);
    if P > 0 then
      delete(Params, 1, P)
    else
      // if there is no quote and no space then it must be just the executable name
      Params := '';
  end;
  Parse(Params);
end;

procedure TCommandLineReader.Parse(const s: string);
var
  cmd: PChar;

  procedure raiseError;
  var
    ErrorMessage: string;
  begin
    if Assigned(OnShowError) or AutomaticalShowError then
    begin
      ErrorMessage := _(SParseError) + copy(string(cmd), 1, 100) + #13#10;
      if length(propertyArray) = 0 then
        ErrorMessage := _(SParseErrorOptions)
      else
        ErrorMessage := _(SParseInvalidOptions) + AvaibleOptions;
    end;

    if Assigned(OnShowError) then
      OnShowError(ErrorMessage);
    if AutomaticalShowError then
      if system.IsConsole then
        writeln(ErrorMessage)
      else
        MessageBox(0, PChar(ErrorMessage),
          PChar(string(_(SCommandLineOptions))), MB_ICONWARNING or MB_OK);

    raise ECommandLineParseException.create('Error before ' + string(cmd));
  end;

var
  currentProperty: Longint;
  valueStart: PChar;
  valueLength: Longint;
  flagValue: Boolean;
  stringStart: Char;
  I: Integer;
begin
  cmd := PChar(s);
  currentProperty := -1;
  SetLength(nameless, 0);
  for i := 0 to high(propertyArray) do
    propertyArray[i].found := false;
  while cmd^ <> Null do
  begin
    if (cmd^ = '-') or (AllowDOSStyle and (cmd^ = '/')) then
    begin
      //Start of property name
      if (cmd^ = '/') or ((cmd + 1)^ = '-') then
      begin //long property
        if cmd^ <> '/' then
          Inc(cmd);
        Inc(cmd);
        valueStart := cmd;
        while not CharInSet(cmd^, [Space, Null, Char('=')]) do
          Inc(cmd);
        currentProperty := -1;
        if (StrLIComp(valueStart, 'enable-', 7) = 0) or
          (StrLIComp(valueStart, 'disable-', 8) = 0) then
        begin
          flagValue := valueStart^ = 'e';
          if flagValue then
            Inc(valueStart, 7)
          else
            Inc(valueStart, 8);
          {while not (cmd^ in ['-',#0,'=']) do
            inc(cmd);}
          valueLength := Longint(cmd - valueStart);
          for i := 0 to high(propertyArray) do
            if (length(propertyArray[i].name) = valueLength) and
              (StrLIComp(valueStart, @propertyArray[i].name[1], valueLength) = 0)
                and
              (propertyArray[i].kind = kpFlag) then
            begin
              propertyArray[i].flagvalue := flagValue;
              propertyArray[i].found := true;
              Break;
            end;
        end
        else
        begin
          valueLength := Longint(cmd - valueStart);
          for i := 0 to high(propertyArray) do
            if (length(propertyArray[i].name) = valueLength) and
              (StrLIComp(valueStart, @propertyArray[i].name[1], valueLength) = 0)
                then
            begin
              if propertyArray[i].kind = kpFlag then
                propertyArray[i].flagvalue := not propertyArray[i].flagdefault;
              currentProperty := i;
              propertyArray[i].found := true;
              Break;
            end;
          if currentProperty = -1 then
            raiseError;
          if propertyArray[currentProperty].kind = kpFlag then
            currentProperty := -1;
        end;
      end
      else if CharInSet((cmd + 1)^, [Space, Null]) then
        raiseError //unknown format
      else
      begin //flag abbreviation string
        Inc(cmd);
        while not CharInSet(cmd^, [Space, Null]) do
        begin
          for i := 0 to high(propertyArray) do
            if (propertyArray[i].kind = kpFlag) and
              (propertyArray[i].abbreviation = cmd^) then
            begin
              propertyArray[i].flagvalue := not propertyArray[i].flagdefault;
              propertyArray[i].found := true;
            end;
          Inc(cmd);
        end;
      end;
    end
    else if cmd^ <> ' ' then
    begin
      //Start of property value
      if CharInSet(cmd^, [Quote, Char('''')]) then
      begin
        stringStart := cmd^;
        Inc(cmd);
        valueStart := cmd;
        while not CharInSet(cmd^, [stringStart, Null]) do
          Inc(cmd);
        valueLength := Longint(cmd - valueStart);
        if currentProperty <> -1 then
        begin
          setlength(propertyArray[currentProperty].strvalue, valueLength);
          move(valueStart^, propertyArray[currentProperty].strvalue[1],
            valueLength * SizeOf(Char));
          case propertyArray[currentProperty].kind of
            kpInt: propertyArray[currentProperty].intvalue :=
              StrToInt(propertyArray[currentProperty].strvalue);
            kpFloat: propertyArray[currentProperty].floatvalue :=
              StrToFloat(propertyArray[currentProperty].strvalue);
          end;
        end
        else
        begin
          SetLength(nameless, length(nameless) + 1);
          setlength(nameless[high(nameless)], valueLength);
          move(valueStart^, nameless[high(nameless)][1], valueLength *
            SizeOf(Char));
        end;
      end
      else
      begin
        valueStart := cmd;
        while not CharInSet(cmd^, [Space, Null]) do
          Inc(cmd);
        valueLength := Longint(cmd - valueStart);
        if currentProperty = -1 then
        begin
          SetLength(nameless, length(nameless) + 1);
          setlength(nameless[high(nameless)], valueLength);
          move(valueStart^, nameless[high(nameless)][1], valueLength *
            SizeOf(Char));
        end
        else
        begin
          setlength(propertyArray[currentProperty].strvalue, valueLength);
          move(valueStart^, propertyArray[currentProperty].strvalue[1],
            valueLength * SizeOf(Char));
          try
            case propertyArray[currentProperty].kind of
              kpInt: propertyArray[currentProperty].intvalue :=
                StrToInt(propertyArray[currentProperty].strvalue);
              kpFloat: propertyArray[currentProperty].floatvalue :=
                StrToFloat(propertyArray[currentProperty].strvalue);
              kpFile: with propertyArray[currentProperty] do
                begin
                  while not FileExists(strvalue) do
                  begin
                    while cmd^ = ' ' do
                      Inc(cmd);
                    while not CharInSet(cmd^, [Space, Null]) do
                      Inc(cmd);
                    valueLength := Longint(cmd - valueStart);
                    setlength(strvalue, valueLength);
                    move(valueStart^, strvalue[1], valueLength * SizeOf(Char));
                    if cmd^ = Null then
                      Exit;
                  end;
                end;
            end;
          except
            raiseError();
          end;
        end;
      end;
      currentProperty := -1;
    end;
    if cmd^ <> Null then
      Inc(cmd)
    else
      Break;

  end;
  Parsed := true;
end;

function TCommandLineReader.FindProperty(Name: string): PProperty;
begin
  Name := LowerCase(Name);
  for var I := 0 to high(propertyArray) do
    if propertyArray[I].Name = Name then
    begin
      Result := @propertyArray[I];
      Exit;
    end;
  raise ECommandLineParseException.Create('Property not found: ' + Name);
end;

function TCommandLineReader.DeclareProperty(Name, Description, Default: string;
  kind: TKindOfProperty): PProperty;
begin
  SetLength(propertyArray, length(propertyArray) + 1);
  Result := @propertyArray[high(propertyArray)];
  Result^.Name := LowerCase(Name);
  Result^.desc := Description;
  Result^.strvalue := Default;
  Result^.kind := kind;
end;

procedure TCommandLineReader.DeclareFlag(const Name, Description: string;
  FlagNameAbbreviation: Char; Default: Boolean = False);
begin
  with declareProperty(Name, Description, '', kpFlag)^ do
  begin
    flagvalue := Default;
    flagdefault := Default;
    abbreviation := FlagNameAbbreviation;
  end;
end;

procedure TCommandLineReader.DeclareFlag(const Name, Description: string;
  Default: Boolean = false);
begin
  declareFlag(Name, Description, Null, Default);
end;

procedure TCommandLineReader.DeclareFile(const Name, Description: string;
  Default: string = '');
begin
  DeclareProperty(Name, Description, '', kpFile);
end;

procedure TCommandLineReader.DeclareString(const Name, Description: string;
  Value: string = '');
begin
  DeclareProperty(Name, Description, Value, kpStr);
end;

procedure TCommandLineReader.DeclareInt(const Name, Description: string; Value:
  Longint = 0);
begin
  DeclareProperty(Name, Description, IntToStr(Value), kpInt)^.intvalue := Value;
end;

procedure TCommandLineReader.DeclareFloat(const Name, Description: string;
  Value: Extended = 0);
begin
  DeclareProperty(Name, Description, FloatToStr(Value), kpFloat)^.floatvalue :=
    Value;
end;

function TCommandLineReader.ReadString(const Name: string): string;
begin
  if not Parsed then
    Parse;
  result := findProperty(Name)^.strvalue;
end;

function TCommandLineReader.ReadInt(const Name: string): Longint;
var
  Prop: PProperty;
begin
  if not Parsed then
    Parse;
  Prop := findProperty(name);
  if Prop^.kind <> kpInt then
    raise ECommandLineParseException.create('No integer property: ' + name);
  Result := Prop^.intvalue;
end;

function TCommandLineReader.ReadFloat(const Name: string): Extended;
var
  Prop: PProperty;
begin
  if not Parsed then
    Parse;
  Prop := findProperty(Name);
  if Prop^.kind <> kpFloat then
    raise ECommandLineParseException.create('No extended property: ' + Name);
  Result := Prop^.Floatvalue;
end;

function TCommandLineReader.ReadFlag(const Name: string): Boolean;
var
  Prop: PProperty;
begin
  if not Parsed then
    Parse;
  Prop := findProperty(Name);
  if Prop^.kind <> kpFlag then
    raise ECommandLineParseException.create('No flag property: ' + Name);
  Result := Prop^.flagvalue;
end;

function TCommandLineReader.ExistsProperty(const Name: string): Boolean;
begin
  if not Parsed then
    Parse;
  Result := findProperty(Name)^.found;
end;

function TCommandLineReader.ReadNamelessFiles(): TStringArray;
begin
  Result := nameless;
end;

function TCommandLineReader.ReadNamelessString(): TStringArray;
begin
  Result := nameless;
end;

function TCommandLineReader.ReadNamelessInt(): TLongintArray;
begin
  SetLength(Result, Length(nameless));
  var P := 0;
  for var I := 0 to high(nameless) do
    try
      Result[P] := StrToInt(nameless[I]);
      Inc(P);
    except
    end;
  SetLength(Result, P);
end;

function TCommandLineReader.ReadNamelessFloat(): TFloatArray;
begin
  SetLength(Result, Length(nameless));
  var P := 0;
  for var I := 0 to High(nameless) do
    try
      Result[P] := StrToFloat(nameless[I]);
      Inc(P);
    except
    end;
  SetLength(Result, P);
end;

function TCommandLineReader.ReadNamelessFlag(): TBooleanArray;
begin
  SetLength(Result, Length(nameless));
  var P := 0;
  for var I := 0 to high(nameless) do
  begin
    if LowerCase(nameless[I]) = 'true' then
      Result[P] := True
    else if LowerCase(nameless[I]) = 'false' then
      Result[P] := False
    else
      Dec(P);
    Inc(P);
  end;
  SetLength(Result, P);
end;

const
  sSyntax = 'PyScripter command line syntax:';
  sOptions = 'Command line options:';
  sPyScripterCommandLine = 'pyscripter [options] [filename1, [filename2, ...]]';

initialization
  CmdLineReader := TCommandLineReader.Create;
  CmdLineReader.AllowDOSStyle := true;
  CmdLineReader.AutomaticalShowError := True;
  CmdLineReader.declareFlag('HELP', 'Show PyScripter command line options',
    Char('H'), False);
  CmdLineReader.declareFlag('NEWINSTANCE', 'Start a new instance of PyScripter',
    Char('N'), False);
  CmdLineReader.declareFlag('PYTHON38', 'Use Python version 3.8', False);
  CmdLineReader.declareFlag('PYTHON39', 'Use Python version 3.9', False);
  CmdLineReader.declareFlag('PYTHON310', 'Use Python version 3.10', False);
  CmdLineReader.declareFlag('PYTHON311', 'Use Python version 3.11', False);
  CmdLineReader.declareFlag('PYTHON312', 'Use Python version 3.12', False);
  CmdLineReader.declareFlag('PYTHON313', 'Use Python version 3.13', False);
  CmdLineReader.declareFlag('PYTHON314', 'Use Python version 3.14', False);
  CmdLineReader.declareString('PROJECT', 'Specify a project file to open');
  CmdLineReader.declareString('PYTHONDLLPATH', 'Use a specific Pythonxx.dll');

  try
    CmdLineReader.parse;
    if CmdLineReader.readFlag('HELP') then
    begin
      if system.IsConsole then
      begin
        writeln(sSyntax);
        writeln(sPyScripterCommandLine);
        writeln(sOptions);
        writeln(CmdLineReader.AvaibleOptions);
      end
      else
        MessageBox(0,
          PChar(sPyScripterCommandLine + sLineBreak + sOptions + sLineBreak +
            CmdLineReader.AvaibleOptions),
          sSyntax, MB_ICONINFORMATION or MB_OK);
      Halt(0);
    end;
  except
    on E: ECommandLineParseException do
      Halt(1);
  end;

finalization
  CmdLineReader.free;
end.

