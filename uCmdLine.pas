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
  TStringArray=array of string;
  TLongintArray=array of longint;
  TFloatArray=array of extended;
  TBooleanArray=array of boolean;
  TCommandLineReaderLanguage=(clrlEnglish,clrlGerman);
  TCommandLineReaderShowError=procedure (errorDescription: string);
  ECommandLineParseException=class(Exception);
  TKindOfProperty=(kpStr,kpFile,kpInt,kpFloat,kpFlag);
  TProperty=record
   name,desc,strvalue:string;
   found: boolean;
   case kind: TKindOfProperty of
     kpStr,kpFile: ();
     kpInt: (intvalue: longint);
     kpFloat: (floatvalue: extended);
     kpFlag: (flagvalue,flagdefault: boolean;
              abbreviation: char)
  end;
  PProperty=^TProperty;

  { TCommandLineReader }

  TCommandLineReader=class
  protected
    parsed{,searchNameLessFile,searchNameLessInt,searchNameLessFloat,searchNameLessFlag}: boolean;
    propertyArray: array of TProperty;
    nameless: TStringArray;
    function findProperty(name:string):PProperty;
    function declareProperty(name,description,default:string;kind: TKindOfProperty):PProperty;
  public
    language:TCommandLineReaderLanguage; //not implemented yet
    onShowError: TCommandLineReaderShowError;
    automaticalShowError: boolean;
    allowDOSStyle: boolean;

    constructor create;
    destructor Destroy; override;

    function avaibleOptions:string;

    procedure parse();overload;
    procedure parse(const s:string);overload;

    //DeclareFlag allows the use of flags
    //Example:
    //   declareFlag('flag','f',true);
    //  Following command-line options are always possible
    //    --enable-flag      =>     flag:=true
    //    --disable-flag     =>     flag:=false
    //    --flag             =>     flag:=not default
    //    -xfy               =>     flag:=not default
    procedure declareFlag(const name,description:string;flagNameAbbreviation:char;default:boolean=false);overload;
    procedure declareFlag(const name,description:string;default:boolean=false);overload;


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
    procedure declareFile(const name,description:string;default:string='');overload;

    //DeclareXXXX allows the use of string, int, float, ...
    //Example:
    //   declareFlag('property');
    //  Following command-line options are  possible
    //    --file 123                  =>     file:=123
    //    --file '123'                =>     file:=123
    //    --file "123"                =>     file:=123
    //    --file='123'                =>     file:=123
    //    --file="123"                =>     file:=123

    procedure declareString(const name,description:string;value: string='');overload;
    procedure declareInt(const name,description:string;value: longint=0);overload;
    procedure declareFloat(const name,description:string;value: extended=0);overload;

    function readString(const name:string):string; overload;
    function readInt(const name:string):longint;overload;
    function readFloat(const name:string):extended; overload;
    function readFlag(const name:string):boolean;overload;

    function existsProperty(const name:string):boolean;

    function readNamelessFiles():TStringArray;
    function readNamelessString():TStringArray;
    function readNamelessInt():TLongintArray;
    function readNamelessFloat():TFloatArray;
    function readNamelessFlag():TBooleanArray;
  end;

var
  CmdLineReader: TCommandLineReader;

implementation
uses Windows //for messages
     ;

constructor TCommandLineReader.create;
begin
  parsed:=false;
  {$IFDEF Win32}
    allowDOSStyle:=true;
  {$ELSE}
    allowDOSStyle:=false;
  {$ENDIF}
  language:=clrlEnglish;
  onShowError:=nil;
  {searchNameLessFile:=false;
  searchNameLessInt:=false;
  searchNameLessFloat:=false;
  searchNameLessFlag:=false;}
  automaticalShowError:=not IsLibrary;
end;
destructor TCommandLineReader.destroy;
begin
  inherited;
end;

function TCommandLineReader.avaibleOptions: string;
var i:integer;
begin
  result:='';
  for i:=0 to high(propertyArray) do begin
    result:=result+'--'+propertyArray[i].name;
    case propertyArray[i].kind of
      kpFlag: if propertyArray[i].abbreviation<>#0 then
                result:=result+' or -'+propertyArray[i].abbreviation;
      else result:=result+'=';
    end;
    result:=result+#9+propertyArray[i].desc+#13#10;
  end;
end;

procedure TCommandLineReader.parse();
var params: string;
begin
  params:=string(cmdline);//string(getcommandline);
  if params='' then exit;
  if params[1]='"' then begin
    params[1]:='X';
    delete(params,1,pos('"',params));
  end else delete(params,1,pos(' ',params));                            
  parse(params);
end;
procedure TCommandLineReader.parse(const s:string);
var cmd: pchar;

  procedure raiseError;
  var errorMessage: string;
  begin
    if assigned(onShowError) or automaticalShowError then begin
      errorMessage:='Parse error at this position: '+copy(string(cmd),1,100)+#13#10;
      if length(propertyArray)=0 then
        errorMessage:='you are not allowed to use command line options starting with -'
       else
        errorMessage:='The following command line options are valid: '#13#10+avaibleOptions;
    end;

    if assigned(onShowError) then
      onShowError(errorMessage);
    if automaticalShowError then
      if system.IsConsole then
        writeln(errorMessage)
       else
        MessageBox(0, PChar(errorMessage), 'PyScripter Command Line Options', MB_ICONWARNING or MB_OK);

    raise ECommandLineParseException.create('Error before '+string(cmd));
  end;


var currentProperty:longint;
    valueStart: pchar;
    valueLength: longint;
    flagValue: boolean;
    stringStart: char;
    i:integer;
begin
  cmd:=pchar(s);
  currentProperty:=-1;
  SetLength(nameless,0);
  for i:=0 to high(propertyArray) do
    propertyArray[i].found:=false;
  while cmd^<>#0 do begin
    if (cmd^='-') or (allowDOSStyle and (cmd^='/')) then begin
      //Start of property name
      if (cmd^='/') or ((cmd+1)^='-') then begin //long property
        if cmd^<>'/' then inc(cmd);
        inc(cmd);
        valueStart:=cmd;
        while not (cmd^  in [' ',#0,'=']) do
          inc(cmd);
        currentProperty:=-1;
        if (StrLIComp(valueStart,'enable-',7) = 0)or
           (StrLIComp(valueStart,'disable-',8) = 0)  then begin
          flagValue:=valueStart^='e';
          if flagValue then inc(valueStart,7)
          else inc(valueStart,8);
          {while not (cmd^ in ['-',#0,'=']) do
            inc(cmd);}
          valueLength:=longint(cmd-valueStart);
          for i:=0 to high(propertyArray) do
            if (length(propertyArray[i].name)=valueLength) and
               (StrLIComp(valueStart,@propertyArray[i].name[1],valueLength)=0) and
               (propertyArray[i].kind=kpFlag) then begin
              propertyArray[i].flagvalue:=flagValue;
              propertyArray[i].found:=true;
              break;
            end;
        end else begin
          valueLength:=longint(cmd-valueStart);
          for i:=0 to high(propertyArray) do
            if (length(propertyArray[i].name)=valueLength) and
               (StrLIComp(valueStart,@propertyArray[i].name[1],valueLength)=0) then begin
              if propertyArray[i].kind=kpFlag then
                propertyArray[i].flagvalue:=not propertyArray[i].flagdefault;
              currentProperty:=i;
              propertyArray[i].found:=true;
              break;
            end;
          if currentProperty=-1 then raiseError;
          if propertyArray[currentProperty].kind=kpFlag then
            currentProperty:=-1;
        end;
      end else if (cmd+1)^ in [' ',#0] then raiseError //unknown format
      else begin //flag abbreviation string
        inc(cmd);
        while not (cmd^ in [' ',#0]) do begin
          for i:=0 to high(propertyArray) do
            if (propertyArray[i].kind=kpFlag) and (propertyArray[i].abbreviation=cmd^) then begin
              propertyArray[i].flagvalue:=not propertyArray[i].flagdefault;
              propertyArray[i].found:=true;
            end;
          inc(cmd);
        end;
      end;
    end else if cmd^ <> ' ' then begin
      //Start of property value
      if cmd^ in ['"',''''] then begin
        stringStart:=cmd^;
        inc(cmd);
        valueStart:=cmd;
        while not (cmd^ in [stringStart,#0]) do
          inc(cmd);
        valueLength:=longint(cmd-valueStart);
        if currentProperty<>-1 then begin
          setlength(propertyArray[currentProperty].strvalue,valueLength);
          move(valueStart^,propertyArray[currentProperty].strvalue[1],valueLength);
          case propertyArray[currentProperty].kind of
            kpInt: propertyArray[currentProperty].intvalue:=StrToInt(propertyArray[currentProperty].strvalue);
            kpFloat:  propertyArray[currentProperty].floatvalue:=StrToFloat(propertyArray[currentProperty].strvalue);
          end;
        end else begin
          SetLength(nameless,length(nameless)+1);
          setlength(nameless[high(nameless)],valueLength);
          move(valueStart^,nameless[high(nameless)][1],valueLength);
        end;
      end else begin
        valueStart:=cmd;
        while not (cmd^ in [' ',#0]) do inc(cmd);
        valueLength:=longint(cmd-valueStart);
        if currentProperty=-1 then begin
          SetLength(nameless,length(nameless)+1);
          setlength(nameless[high(nameless)],valueLength);
          move(valueStart^,nameless[high(nameless)][1],valueLength);
        end else begin
          setlength(propertyArray[currentProperty].strvalue,valueLength);
          move(valueStart^,propertyArray[currentProperty].strvalue[1],valueLength);
          try
            case propertyArray[currentProperty].kind of
              kpInt: propertyArray[currentProperty].intvalue:=StrToInt(propertyArray[currentProperty].strvalue);
              kpFloat:  propertyArray[currentProperty].floatvalue:=StrToFloat(propertyArray[currentProperty].strvalue);
              kpFile: with propertyArray[currentProperty] do begin
                while not FileExists(strvalue) do begin
                  while cmd^ = ' ' do inc(cmd);
                  while not (cmd^ in [' ',#0]) do inc(cmd);
                  valueLength:=longint(cmd-valueStart);
                  setlength(strvalue,valueLength);
                  move(valueStart^,strvalue[1],valueLength);
                  if cmd^ = #0 then exit;
                end;
              end;
            end;
          except
            raiseError();
          end;
        end;
      end;
      currentProperty:=-1;
    end;
    if cmd^<>#0 then inc(cmd)
    else break;

  end;
  parsed:=true;
end;

function TCommandLineReader.findProperty(name:string):PProperty;
var i:integer;
begin
  name:=lowercase(name);
  for i:=0 to high(propertyArray) do
    if propertyArray[i].name=name then begin
      result:=@propertyArray[i];
      exit;
    end;
  raise ECommandLineParseException.Create('Property not found: '+name);
end;

function TCommandLineReader.declareProperty(name,description,default:string;kind: TKindOfProperty):PProperty;
begin
  SetLength(propertyArray,length(propertyArray)+1);
  result:=@propertyArray[high(propertyArray)];
  result^.name:=lowercase(name);
  result^.desc:=description;
  result^.strvalue:=default;
  result^.kind:=kind;
end;
procedure TCommandLineReader.declareFlag(const name,description:string;flagNameAbbreviation:char;default:boolean=false);
begin
  with declareProperty(name,description,'',kpFlag)^ do begin
    flagvalue:=default;
    flagdefault:=default;
    abbreviation:=flagNameAbbreviation;
  end;
end;
procedure TCommandLineReader.declareFlag(const name,description:string;default:boolean=false);
begin
  declareFlag(name,description,#0,default);
end;

procedure TCommandLineReader.declareFile(const name,description:string;default:string='');
begin
  declareProperty(name,description,'',kpFile);
end;

procedure TCommandLineReader.declareString(const name,description:string;value: string='');
begin
  declareProperty(name,description,value,kpStr);
end;
procedure TCommandLineReader.declareInt(const name,description:string;value: longint=0);
begin
  declareProperty(name,description,IntToStr(value),kpInt)^.intvalue:=value;
end;
procedure TCommandLineReader.declareFloat(const name,description:string;value: extended=0);
begin
  declareProperty(name,description,FloatToStr(value),kpFloat)^.floatvalue:=value;
end;

function TCommandLineReader.readString(const name:string):string;
begin
  if not parsed then parse;
  result:=findProperty(name)^.strvalue;
end;
function TCommandLineReader.readInt(const name:string):longint;
var prop: PProperty;
begin
  if not parsed then parse;
  prop:=findProperty(name);
  if prop^.kind<>kpInt then raise ECommandLineParseException.create('No integer property: '+name);
  result:=prop^.intvalue;
end;
function TCommandLineReader.readFloat(const name:string):extended;
var prop: PProperty;
begin
  if not parsed then parse;
  prop:=findProperty(name);
  if prop^.kind<>kpFloat then raise ECommandLineParseException.create('No extended property: '+name);
  result:=prop^.Floatvalue;
end;
function TCommandLineReader.readFlag(const name:string):boolean;
var prop: PProperty;
begin
  if not parsed then parse;
  prop:=findProperty(name);
  if prop^.kind<>kpFlag then raise ECommandLineParseException.create('No flag property: '+name);
  result:=prop^.flagvalue;
end;

function TCommandLineReader.existsProperty(const name:string):boolean;
begin
  if not parsed then parse;
  result:=findProperty(name)^.found;
end;

function TCommandLineReader.readNamelessFiles():TStringArray;
begin
  Result:=nameless;
end;
function TCommandLineReader.readNamelessString():TStringArray;
begin
  result:=nameless;
end;
function TCommandLineReader.readNamelessInt():TLongintArray;
var i,p:integer;
begin
  SetLength(result,length(nameless));
  p:=0;
  for i:=0 to high(nameless) do
    try
      result[p]:=StrToInt(nameless[i]);
      inc(p);
    except
    end;
  SetLength(result,p);
end;
function TCommandLineReader.readNamelessFloat():TFloatArray;
var i,p:integer;
begin
  SetLength(result,length(nameless));
  p:=0;
  for i:=0 to high(nameless) do
    try
      result[p]:=StrToFloat(nameless[i]);
      inc(p);
    except
    end;
  SetLength(result,p);
end;
function TCommandLineReader.readNamelessFlag():TBooleanArray;
var i,p:integer;
begin
  SetLength(result,length(nameless));
  p:=0;
  for i:=0 to high(nameless) do begin
    if lowercase(nameless[i])='true' then Result[p]:=true
    else if lowercase(nameless[i])='false' then Result[p]:=false
    else dec(p);
    inc(p);
  end;
  SetLength(result,p);
end;

initialization
  CmdLineReader := TCommandLineReader.Create;
  CmdLineReader.allowDOSStyle:=true;
  CmdLineReader.automaticalShowError := True;
  CmdLineReader.declareFlag('NEWINSTANCE','Start a new instance of PyScripter', 'N',False);
  CmdLineReader.declareFlag('DPIAWARE','Make PyScripter DPI aware in VISTA', 'D',False);
  CmdLineReader.declareFlag('DEBUG','Use debug version of Python', 'B',False);
  CmdLineReader.declareFlag('PYTHON23','Use Python version 2.3',False);
  CmdLineReader.declareFlag('PYTHON23','Use Python version 2.3',False);
  CmdLineReader.declareFlag('PYTHON24','Use Python version 2.4',False);
  CmdLineReader.declareFlag('PYTHON25','Use Python version 2.5',False);
  CmdLineReader.declareFile('PYTHONDLLPATH','Use a specific Pythonxx.dll');

  try
      CmdLineReader.parse;
  except
    on E: ECommandLineParseException do Halt(1);
  end;

finalization
  CmdLineReader.free;
end.

