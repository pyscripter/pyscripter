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
  {$mode objfpc}{$H+}
{$ENDIF}

uses sysutils; //for exceptions
type
  TStringArray=array of WideString;
  TLongintArray=array of longint;
  TFloatArray=array of extended;
  TBooleanArray=array of boolean;
  TCommandLineReaderShowError = procedure (errorDescription: WideString);
  ECommandLineParseException = class(Exception);
  TKindOfProperty=(kpStr,kpFile,kpInt,kpFloat,kpFlag);
  TProperty=record
   name,desc,strvalue:WideString;
   found: boolean;
   case kind: TKindOfProperty of
     kpStr,kpFile: ();
     kpInt: (intvalue: longint);
     kpFloat: (floatvalue: extended);
     kpFlag: (flagvalue,flagdefault: boolean;
              abbreviation: WideChar)
  end;
  PProperty=^TProperty;

  { TCommandLineReader }

  TCommandLineReader=class
  protected
    parsed{,searchNameLessFile,searchNameLessInt,searchNameLessFloat,searchNameLessFlag}: boolean;
    propertyArray: array of TProperty;
    nameless: TStringArray;
    function findProperty(name:WideString):PProperty;
    function declareProperty(name,description,default:WideString;kind: TKindOfProperty):PProperty;
  public
    onShowError: TCommandLineReaderShowError;
    automaticalShowError: boolean;
    allowDOSStyle: boolean;

    constructor create;
    destructor Destroy; override;

    function avaibleOptions:WideString;

    procedure parse();overload;
    procedure parse(const s:WideString);overload;

    //DeclareFlag allows the use of flags
    //Example:
    //   declareFlag('flag','f',true);
    //  Following command-line options are always possible
    //    --enable-flag      =>     flag:=true
    //    --disable-flag     =>     flag:=false
    //    --flag             =>     flag:=not default
    //    -xfy               =>     flag:=not default
    procedure declareFlag(const name,description:WideString;flagNameAbbreviation:WideChar;default:boolean=false);overload;
    procedure declareFlag(const name,description:WideString;default:boolean=false);overload;


    //DeclareFlag allows the use of a file name
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
    procedure declareFile(const name,description:WideString;default:WideString='');overload;

    //DeclareXXXX allows the use of WideString, int, float, ...
    //Example:
    //   declareFlag('property');
    //  Following command-line options are  possible
    //    --file 123                  =>     file:=123
    //    --file '123'                =>     file:=123
    //    --file "123"                =>     file:=123
    //    --file='123'                =>     file:=123
    //    --file="123"                =>     file:=123

    procedure declareString(const name,description:WideString;value: WideString='');overload;
    procedure declareInt(const name,description:WideString;value: longint=0);overload;
    procedure declareFloat(const name,description:WideString;value: extended=0);overload;

    function readString(const name:WideString):WideString; overload;
    function readInt(const name:WideString):longint;overload;
    function readFloat(const name:WideString):extended; overload;
    function readFlag(const name:WideString):boolean;overload;

    function existsProperty(const name:WideString):boolean;

    function readNamelessFiles():TStringArray;
    function readNamelessString():TStringArray;
    function readNamelessInt():TLongintArray;
    function readNamelessFloat():TFloatArray;
    function readNamelessFlag():TBooleanArray;
  end;

var
  CmdLineReader: TCommandLineReader;

implementation
uses
  Windows, gnugettext, StringResources, uCommonFunctions,
  TntSysUtils, TntWideStrUtils, WideStrUtils //for messages
  ;

const
  WideNull = WideChar(#0);
  WideTab = WideChar(#9);
  WideSpace = WideChar(#32);
  WideQuote = WideChar(#34);

constructor TCommandLineReader.create;
begin
  parsed:=false;
  {$IFDEF Win32}
    allowDOSStyle := true;
  {$ELSE}
    allowDOSStyle := false;
  {$ENDIF}
  onShowError := nil;
  {searchNameLessFile:=false;
  searchNameLessInt:=false;
  searchNameLessFloat:=false;
  searchNameLessFlag:=false;}
  automaticalShowError := not IsLibrary;
end;

destructor TCommandLineReader.destroy;
begin
  inherited;
end;

function TCommandLineReader.avaibleOptions: WideString;
var i:integer;
begin
  result := '';
  for i := 0 to high(propertyArray) do begin
    result := result + '--' + propertyArray[i].name;
    case propertyArray[i].kind of
      kpFlag: if propertyArray[i].abbreviation <> #0 then
                result := result + ' or -' + propertyArray[i].abbreviation;
      else result := result + '=';
    end;
    result := result + WideTab + WideTab + propertyArray[i].desc + #13#10;
  end;
end;

procedure TCommandLineReader.parse();
var params: WideString;
begin
  params := GetCommandLineW;
  if params = '' then exit;
  if params[1] = WideQuote then begin
    params[1] := 'X';
    delete(params, 1 , WideCharPos(params, WideQuote));
  end else
    delete(params, 1, WideCharPos(params, WideSpace));
  parse(params);
end;

procedure TCommandLineReader.parse(const s:WideString);
var cmd: PWideChar;

  procedure raiseError;
  var errorMessage: WideString;
  begin
    if assigned(onShowError) or automaticalShowError then begin
      errorMessage := _(SParseError) + copy(WideString(cmd), 1, 100) + #13#10;
      if length(propertyArray) = 0 then
        errorMessage := _(SParseErrorOptions)
      else
        errorMessage := _(SParseInvalidOptions) + avaibleOptions;
    end;

    if assigned(onShowError) then
      onShowError(errorMessage);
    if automaticalShowError then
      if system.IsConsole then
         writeln(errorMessage)
       else
         MessageBoxW(0, PWideChar(errorMessage), PWideChar(_(SCommandLineOptions)), MB_ICONWARNING or MB_OK);

    raise ECommandLineParseException.create('Error before '+WideString(cmd));
  end;

var currentProperty:longint;
    valueStart: PWideChar;
    valueLength: longint;
    flagValue: boolean;
    stringStart: WideChar;
    i:integer;
begin
  cmd := PWideChar(s);
  currentProperty := -1;
  SetLength(nameless,0);
  for i := 0 to high(propertyArray) do
    propertyArray[i].found := false;
  while cmd^ <> WideNull do begin
    if (cmd^ = '-') or (allowDOSStyle and (cmd^ = '/')) then begin
      //Start of property name
      if (cmd^ = '/') or ((cmd + 1)^ = '-') then begin //long property
        if cmd^ <> '/' then inc(cmd);
        inc(cmd);
        valueStart := cmd;
        while not (cmd^ in [WideSpace, WideNull, WideChar('=')]) do
          inc(cmd);
        currentProperty := -1;
        if (WStrLIComp(valueStart, 'enable-', 7) = 0) or
           (WStrLIComp(valueStart, 'disable-', 8) = 0) then
        begin
          flagValue := valueStart^ = 'e';
          if flagValue then inc(valueStart,7)
          else inc(valueStart,8);
          {while not (cmd^ in ['-',#0,'=']) do
            inc(cmd);}
          valueLength := longint(cmd-valueStart);
          for i := 0 to high(propertyArray) do
            if (length(propertyArray[i].name) = valueLength) and
               (WStrLIComp(valueStart,@propertyArray[i].name[1], valueLength) = 0) and
               (propertyArray[i].kind = kpFlag) then
            begin
              propertyArray[i].flagvalue := flagValue;
              propertyArray[i].found := true;
              break;
            end;
        end else begin
          valueLength := longint(cmd-valueStart);
          for i := 0 to high(propertyArray) do
            if (length(propertyArray[i].name)=valueLength) and
               (WStrLIComp(valueStart, @propertyArray[i].name[1], valueLength)=0) then begin
              if propertyArray[i].kind = kpFlag then
                propertyArray[i].flagvalue := not propertyArray[i].flagdefault;
              currentProperty := i;
              propertyArray[i].found := true;
              break;
            end;
          if currentProperty=-1 then raiseError;
          if propertyArray[currentProperty].kind = kpFlag then
            currentProperty := -1;
        end;
      end else if (cmd+1)^ in [WideSpace, WideNull] then raiseError //unknown format
      else begin //flag abbreviation WideString
        inc(cmd);
        while not (cmd^ in [WideSpace, WideNull]) do begin
          for i := 0 to high(propertyArray) do
            if (propertyArray[i].kind = kpFlag) and (propertyArray[i].abbreviation = cmd^) then begin
              propertyArray[i].flagvalue := not propertyArray[i].flagdefault;
              propertyArray[i].found := true;
            end;
          inc(cmd);
        end;
      end;
    end else if cmd^ <> ' ' then begin
      //Start of property value
      if cmd^ in [WideQuote, WideChar('''')] then begin
        stringStart := cmd^;
        inc(cmd);
        valueStart := cmd;
        while not inOpArray(cmd^, [stringStart, WideNull]) do
          inc(cmd);
        valueLength := longint(cmd-valueStart);
        if currentProperty<>-1 then begin
          setlength(propertyArray[currentProperty].strvalue, valueLength);
          move(valueStart^,propertyArray[currentProperty].strvalue[1], valueLength*SizeOf(WideChar));
          case propertyArray[currentProperty].kind of
            kpInt: propertyArray[currentProperty].intvalue := StrToInt(propertyArray[currentProperty].strvalue);
            kpFloat:  propertyArray[currentProperty].floatvalue := StrToFloat(propertyArray[currentProperty].strvalue);
          end;
        end else begin
          SetLength(nameless, length(nameless)+1);
          setlength(nameless[high(nameless)], valueLength);
          move(valueStart^,nameless[high(nameless)][1], valueLength*SizeOf(WideChar));
        end;
      end else begin
        valueStart := cmd;
        while not (cmd^ in [WideSpace, WideNull]) do inc(cmd);
        valueLength := longint(cmd - valueStart);
        if currentProperty=-1 then begin
          SetLength(nameless,length(nameless) + 1);
          setlength(nameless[high(nameless)], valueLength);
          move(valueStart^,nameless[high(nameless)][1], valueLength*SizeOf(WideChar));
        end else begin
          setlength(propertyArray[currentProperty].strvalue, valueLength);
          move(valueStart^,propertyArray[currentProperty].strvalue[1], valueLength*SizeOf(WideChar));
          try
            case propertyArray[currentProperty].kind of
              kpInt: propertyArray[currentProperty].intvalue := StrToInt(propertyArray[currentProperty].strvalue);
              kpFloat:  propertyArray[currentProperty].floatvalue := StrToFloat(propertyArray[currentProperty].strvalue);
              kpFile: with propertyArray[currentProperty] do begin
                while not WideFileExists(strvalue) do begin
                  while cmd^ = ' ' do inc(cmd);
                  while not (cmd^ in [WideSpace, WideNull]) do inc(cmd);
                  valueLength := longint(cmd-valueStart);
                  setlength(strvalue, valueLength);
                  move(valueStart^,strvalue[1], valueLength*SizeOf(WideChar));
                  if cmd^ = WideNull then exit;
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
    if cmd^ <> WideNull then inc(cmd)
    else break;

  end;
  parsed := true;
end;

function TCommandLineReader.findProperty(name:WideString):PProperty;
var i:integer;
begin
  name := WideLowerCase(name);
  for i := 0 to high(propertyArray) do
    if propertyArray[i].name = name then begin
      result := @propertyArray[i];
      exit;
    end;
  raise ECommandLineParseException.Create('Property not found: '+name);
end;

function TCommandLineReader.declareProperty(name,description,default:WideString;kind: TKindOfProperty):PProperty;
begin
  SetLength(propertyArray,length(propertyArray)+1);
  result := @propertyArray[high(propertyArray)];
  result^.name := WideLowerCase(name);
  result^.desc := description;
  result^.strvalue := default;
  result^.kind := kind;
end;
procedure TCommandLineReader.declareFlag(const name,description:WideString;flagNameAbbreviation:WideChar;default:boolean=false);
begin
  with declareProperty(name,description,'',kpFlag)^ do begin
    flagvalue := default;
    flagdefault := default;
    abbreviation := flagNameAbbreviation;
  end;
end;
procedure TCommandLineReader.declareFlag(const name,description:WideString;default:boolean=false);
begin
  declareFlag(name, description, WideNull, default);
end;

procedure TCommandLineReader.declareFile(const name,description:WideString; default:WideString='');
begin
  declareProperty(name, description, '', kpFile);
end;

procedure TCommandLineReader.declareString(const name, description:WideString; value: WideString='');
begin
  declareProperty(name,description,value,kpStr);
end;
procedure TCommandLineReader.declareInt(const name, description:WideString; value: longint=0);
begin
  declareProperty(name,description,IntToStr(value),kpInt)^.intvalue := value;
end;
procedure TCommandLineReader.declareFloat(const name, description:WideString; value: extended=0);
begin
  declareProperty(name,description,FloatToStr(value),kpFloat)^.floatvalue := value;
end;

function TCommandLineReader.readString(const name:WideString):WideString;
begin
  if not parsed then parse;
  result := findProperty(name)^.strvalue;
end;

function TCommandLineReader.readInt(const name:WideString):longint;
var prop: PProperty;
begin
  if not parsed then parse;
  prop := findProperty(name);
  if prop^.kind<>kpInt then raise ECommandLineParseException.create('No integer property: '+name);
  result := prop^.intvalue;
end;

function TCommandLineReader.readFloat(const name:WideString):extended;
var prop: PProperty;
begin
  if not parsed then parse;
  prop := findProperty(name);
  if prop^.kind<>kpFloat then raise ECommandLineParseException.create('No extended property: '+name);
  result := prop^.Floatvalue;
end;

function TCommandLineReader.readFlag(const name:WideString):boolean;
var prop: PProperty;
begin
  if not parsed then parse;
  prop := findProperty(name);
  if prop^.kind <> kpFlag then
    raise ECommandLineParseException.create('No flag property: '+name);
  result := prop^.flagvalue;
end;

function TCommandLineReader.existsProperty(const name:WideString):boolean;
begin
  if not parsed then parse;
  result := findProperty(name)^.found;
end;

function TCommandLineReader.readNamelessFiles():TStringArray;
begin
  Result := nameless;
end;

function TCommandLineReader.readNamelessString():TStringArray;
begin
  result := nameless;
end;

function TCommandLineReader.readNamelessInt():TLongintArray;
var i,p:integer;
begin
  SetLength(result,length(nameless));
  p := 0;
  for i := 0 to high(nameless) do
    try
      result[p] := StrToInt(nameless[i]);
      inc(p);
    except
    end;
  SetLength(result,p);
end;

function TCommandLineReader.readNamelessFloat():TFloatArray;
var i, p:integer;
begin
  SetLength(result,length(nameless));
  p := 0;
  for i := 0 to high(nameless) do
    try
      result[p] := StrToFloat(nameless[i]);
      inc(p);
    except
    end;
  SetLength(result,p);
end;

function TCommandLineReader.readNamelessFlag():TBooleanArray;
var i, p:integer;
begin
  SetLength(result,length(nameless));
  p := 0;
  for i := 0 to high(nameless) do begin
    if WideLowerCase(nameless[i]) = 'true' then Result[p] := true
    else if WideLowerCase(nameless[i]) = 'false' then Result[p] := false
    else dec(p);
    inc(p);
  end;
  SetLength(result,p);
end;

Const
  sSyntax = 'PyScripter command line syntax:';
  sOptions = 'Command line options:';
  sPyScripterCommandLine = 'pyscripter [options] [filename1, [filename2, ...]]';

initialization
  CmdLineReader  :=  TCommandLineReader.Create;
  CmdLineReader.allowDOSStyle := true;
  CmdLineReader.automaticalShowError  :=  True;
  CmdLineReader.declareFlag('HELP','Show PyScripter command line options', WideChar('H'),False);
  CmdLineReader.declareFlag('NEWINSTANCE','Start a new instance of PyScripter', WideChar('N'),False);
  CmdLineReader.declareFlag('DPIAWARE','Make PyScripter DPI aware in VISTA', WideChar('D'),False);
  CmdLineReader.declareFlag('DEBUG','Use debug version of Python', WideChar('B'),False);
  CmdLineReader.declareFlag('PYTHON23','Use Python version 2.3',False);
  CmdLineReader.declareFlag('PYTHON24','Use Python version 2.4',False);
  CmdLineReader.declareFlag('PYTHON25','Use Python version 2.5',False);
  CmdLineReader.declareFlag('PYTHON26','Use Python version 2.6',False);
  CmdLineReader.declareFlag('PYTHON27','Use Python version 2.7',False);
  CmdLineReader.declareFlag('PYTHON30','Use Python version 3.0',False);
  CmdLineReader.declareFlag('PYTHON31','Use Python version 3.1',False);
  CmdLineReader.declareFlag('PYTHON32','Use Python version 3.2',False);
  CmdLineReader.declareFile('PROJECT','Specify a project file to open');
  CmdLineReader.declareFile('PYTHONDLLPATH','Use a specific Pythonxx.dll');

  try
      CmdLineReader.parse;
      if CmdLineReader.readFlag('HELP') then begin
        if system.IsConsole then begin
          writeln(sSyntax);
          writeln(sPyScripterCommandLine);
          writeln(sOptions);
          writeln(CmdLineReader.avaibleOptions);
         end else
          MessageBoxW(0,
            PWideChar(sPyScripterCommandLine + sLineBreak + sOptions + sLineBreak + CmdLineReader.avaibleOptions),
            sSyntax, MB_ICONINFORMATION or MB_OK);
        Halt(0);
      end;
  except
    on E: ECommandLineParseException do Halt(1);
  end;

finalization
  CmdLineReader.free;
end.

