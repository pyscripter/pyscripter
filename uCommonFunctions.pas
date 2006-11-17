{-----------------------------------------------------------------------------
 Unit Name: uCommonFunctions
 Author:    Kiriakos Vlahos
 Date:      23-Jun-2005
 Purpose:   Functions common to many units in PyScripter
 History:
-----------------------------------------------------------------------------}

unit uCommonFunctions;

interface
Uses
  Windows, Classes, SysUtils, Graphics, TBX, TBXThemes;

const
  UTF8BOMString : string = Char($EF) + Char($BB) + Char($BF);
  IdentChars: TSysCharSet = ['_', '0'..'9', 'A'..'Z', 'a'..'z'];
  WideLineBreak : WideString = WideString(sLineBreak);
  SFileExpr = '(([a-zA-Z]:)?[^\*\?="<>|:,;\+\^]+)'; // fwd slash (/) is allowed
  STracebackFilePosExpr =  '"' + SFileExpr + '", line (\d+)(, in ([\?\w]+))?';


type
  (* function type for translation of strings to other language *)
  TTranslateProc = function (const AText: string): string;

(* returns the System ImageList index of the icon of a given file *)
function GetIconIndexFromFile(const AFileName: string;
  const ASmall: boolean): integer;

(* returns long file name even for nonexisting files *)
function GetLongFileName(const APath: string): string;

(* from cStrings *)

var
  (* function for translation of strings to other language *)
  Translate: TTranslateProc;

(* checks if AText starts with ALeft *)
function WideStrIsLeft(AText, ALeft: PWideChar): Boolean;

(* checks if AText starts with ALeft *)
function StrIsLeft(AText, ALeft: PChar): Boolean;

(* checks if AText ends with ARight *)
function StrIsRight(AText, ARight: PChar): Boolean;

(* returns next token - based on Classes.ExtractStrings *)
function StrGetToken(var Content: PChar;
                     Separators, WhiteSpace, QuoteChars: TSysCharSet): string;

(* removes quotes to AText, if needed *)
function StrUnQuote(const AText: string): string;

(* allows reading of locked files *)
function FileToStr(const FileName: String): String;

(* Get the current TBX theme border color for a given state *)
function GetBorderColor(const State: string): TColor;

(* Get the current TBX theme item color for a given state *)
function GetItemInfo(const State: string) : TTBXItemInfo;

(* Lighten a given Color by a certain percentage *)
function LightenColor(Color:TColor; Percentage:integer):TColor;

(* Darken a given Color by a certain percentage *)
function DarkenColor(Color:TColor; Percentage:integer):TColor;

(* Return either clSkyBlue or clHighlight depending on current settings *)
function SelectionBackgroundColor():TColor;

{* Get Exe File Version string *}
function ApplicationVersion : string;

{* Compares two Version strings and returns -1, 0, 1 depending on result *}
function  CompareVersion(const A, B : String) : Integer;

{* Checks whether we are connected to the Internet *}
function ConnectedToInternet : boolean;

{* Extracts the nth line from a string *}
function GetNthLine(const S : string; LineNo : integer) : string;

{* Extracts a range of lines from a string *}
function GetLineRange(const S : string; StartLine, EndLine : integer) : string;

{* Extracts a word from a string *}
function GetWordAtPos(const LineText : String; Start : Integer; WordChars : TSysCharSet;
  ScanBackwards : boolean = True; ScanForward : boolean = True;
  HandleBrackets : Boolean = False) : string;

{* Mask FPU Excptions - Useful for importing SciPy and other Python libs *}
procedure MaskFPUExceptions(ExceptionsMasked : boolean);

{* Adds Path to Python path and automatically deletes it when the
   returned interface is destroyed *}
function AddPathToPythonPath(const Path : string; AutoRemove : Boolean = True) : IInterface;

{* Format a doc string by removing left space and blank lines at start and bottom *}
function FormatDocString(const DocString : string) : string;

{* Calculate the indentation level of a line *}
function CalcIndent(S : string; TabWidth : integer = 4): integer;

{* check if a directory is a Python Package *}
function IsDirPythonPackage(Dir : string): boolean;

{* Get Python Package Root directory *}
function GetPackageRootDir(Dir : string): string;

{* Python FileName to possibly dotted ModuleName accounting for packages *}
function FileNameToModuleName(const FileName : string): string;

{* Convert <  > to &lt; &gt; *}
function HTMLSafe(const S : string): string;

{* Parses command line parameters *}
// From Delphi's system.pas unit! Need to rewrite
function GetParamStr(P: PChar; var Param: string): PChar;

{* ReadLn that works with Sreams *}
// Adapted from Indy
function ReadLnFromStream(Stream : TStream; AMaxLineLength: Integer = -1;
  AExceptionIfEOF: Boolean = FALSE): String;

{* Parse a line for a Python encoding spec *}
function ParsePySourceEncoding(Textline : string): string;

implementation
Uses
  Controls, Forms, ShellApi, JclFileUtils, Math, VarPyth, JclStrings, JclBase,
  SynRegExpr;

function GetIconIndexFromFile(const AFileName: string;
  const ASmall: boolean): integer;
const
  small: array[Boolean] of Integer = (SHGFI_LARGEICON, SHGFI_SMALLICON);
var
  SHFileInfo: TSHFileInfo;
begin
  SHGetFileInfo(PChar(AFileName), 0, SHFileInfo, SizeOf(SHFileInfo),
    SHGFI_SYSICONINDEX or small[ASmall]);
  Result := SHFileInfo.iIcon;
end;

function GetLongFileName(const APath: string): string;
(* returns long file name even for nonexisting files *)
begin
  if APath = '' then Result:= ''
  else begin
    Result:= PathGetLongName(APath);
    // if different - function is working
    if (Result = '') or
       ((Result = APath) and
         not (FileExists(PathRemoveSeparator(APath)) or
              DirectoryExists(PathRemoveSeparator(APath)))) then
    begin
      Result:= ExtractFilePath(APath);
      // we are up to top level
      if (Result = '') or (Result[Length(Result)] = ':') then
        Result:= APath
      else Result:= Concat(GetLongFileName(PathRemoveSeparator(Result)),
                           PathDelim, ExtractFileName(APath));
    end;
  end;
end;

(* from cStrings *)

function NoTranslate(const AText: string): string;
(* default is to return text as is *)
begin
  Result:= AText;
end;

function StrIsLeft(AText, ALeft: PChar): Boolean;
(* checks if AText starts with ALeft *)
begin
  while (ALeft^ <> #0) and (AText^ <> #0) and (ALeft^ = AText^) do begin
    Inc(ALeft);
    Inc(AText);
  end;
  Result := ALeft^ = #0;
end;

function WideStrIsLeft(AText, ALeft: PWideChar): Boolean;
(* checks if AText starts with ALeft *)
begin
  while (ALeft^ <> #0) and (AText^ <> #0) and (ALeft^ = AText^) do begin
    Inc(ALeft);
    Inc(AText);
  end;
  Result := ALeft^ = #0;
end;

function StrIsRight(AText, ARight: PChar): Boolean;
(* checks if AText ends with ARight *)
var
  LenDiff: Integer;
begin
  Result:= ARight = nil;
  LenDiff := StrLen(AText) - StrLen(ARight);
  if not Result and (LenDiff >= 0) then begin
    Inc(AText, LenDiff);
    Result := StrIsLeft(AText, ARight);
  end;
end;

function StrGetToken(var Content: PChar;
                     Separators, WhiteSpace, QuoteChars: TSysCharSet): string;
(* returns next token - based on Classes.ExtractStrings *)
var
  Head, Tail: PChar;
  InQuote: Boolean;
  QuoteChar: Char;
begin
  Result:= '';
  if (Content = nil) or (Content^=#0) then Exit;
  Tail := Content;
  InQuote := False;
  QuoteChar := #0;
  while Tail^ in WhiteSpace do Inc(Tail);
  Head := Tail;
  while True do begin
    while (InQuote and not (Tail^ in QuoteChars + [#0])) or
      not (Tail^ in Separators + WhiteSpace + QuoteChars + [#0]) do Inc(Tail);
    if Tail^ in QuoteChars then begin
      if (QuoteChar <> #0) and (QuoteChar = Tail^) then
        QuoteChar := #0
      else QuoteChar := Tail^;
      InQuote := QuoteChar <> #0;
      Inc(Tail);
    end else Break;
  end;
  if (Head <> Tail) and (Head^ <> #0) then begin
    SetString(Result, Head, Tail - Head);
    Content:= Tail;
  end;
end;

function StrUnQuote(const AText: string): string;
(* removes quotes to AText, if needed *)
var
  PText: PChar;
begin
  if PChar(AText)^ in ['"', ''''] then begin
    PText:= PChar(AText);
    Result:= AnsiExtractQuotedStr(PText, PText^);
  end
  else Result:= AText;
end;

function FileToStr(const FileName: String): String;
(* allows reading of locked files *)
var
  fs: TFileStream;
  len: Integer;
begin
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    len := fs.Size;
    SetLength(Result, len);
    if len > 0 then
      fs.ReadBuffer(Result[1], len);
  finally
    fs.Free;
  end;
end;

(* from cStrings end *)

function GetBorderColor(const state: string): TColor;
var
 Bmp: TBitmap;
 i: TTBXItemInfo;
begin
 Bmp:= TBitmap.Create;
 try
  Bmp.PixelFormat := pf32Bit;
  Bmp.Width := 19;
  Bmp.Height := 19;

  i := GetItemInfo(state);
  CurrentTheme.PaintBackgnd(BMP.Canvas, BMP.Canvas.ClipRect, BMP.Canvas.ClipRect, BMP.Canvas.ClipRect, CurrentTheme.GetViewColor(TVT_NORMALTOOLBAR), false, TVT_NORMALTOOLBAR);
  CurrentTheme.PaintButton(BMP.Canvas, BMP.Canvas.ClipRect, i);

  Result := Bmp.Canvas.Pixels[10, 0];
 finally
  Bmp.Free;
 end;
end;

function GetItemInfo(const State: string) : TTBXItemInfo;
begin
  FillChar(Result, SizeOf(TTBXItemInfo), 0);
  Result.ViewType := TVT_NORMALTOOLBAR;
  Result.ItemOptions := IO_TOOLBARSTYLE or IO_APPACTIVE;
  Result.IsVertical := False;
  Result.Enabled := true;

  if State = 'inactive' then
  begin
   Result.Pushed := False;
   Result.Selected := False;
   Result.HoverKind := hkNone;
  end else if State = 'active' then begin
    Result.Pushed := False;
    Result.Selected := True;
    Result.HoverKind := hkMouseHover;
  end else if State = 'hot' then begin
    Result.Pushed := True;
    Result.Selected := True;
    Result.HoverKind := hkMouseHover;
  end;
end;

function LightenColor(Color:TColor; Percentage:integer):TColor;
var
   wRGB, wR, wG, wB : longint;
begin
   wRGB := ColorToRGB(Color);
   wR := Min(round(GetRValue(wRGB) * (1+(percentage / 100))), 255);
   wG := Min(round(GetGValue(wRGB) * (1+(percentage / 100))), 255);
   wB := Min(round(GetBValue(wRGB) * (1+(percentage / 100))), 255);
   result := RGB(wR, wG, wB);
end;

function DarkenColor(Color:TColor; Percentage:integer):TColor;
var
   wRGB, wR, wG, wB : longint;
begin
   wRGB := ColorToRGB(Color);
   wR := round(GetRValue(wRGB) / (1+(percentage / 100)));
   wG := round(GetGValue(wRGB) / (1+(percentage / 100)));
   wB := round(GetBValue(wRGB) / (1+(percentage / 100)));
   result := RGB(wR, wG, wB);
end;

(* Return either clSkyBlue or clHighlight depending on current settings *)
function SelectionBackgroundColor():TColor;
begin
  if (ColorToRGB(clWindowText) = clBlack) and (ColorToRGB(clWindow) = clWhite) and
    (ColorToRGB(clHighlightText) = clWhite)
  then
    Result := clSkyBlue
  else
    Result := clHighlight;  // Just play it safe safe
end;

function ApplicationVersion : string;
var
  ExeFile : string;
begin
  ExeFile := Application.ExeName;
  if VersionResourceAvailable(ExeFile) then begin
    with TJclFileVersionInfo.Create(ExeFile) do begin
      Result := BinFileVersion;
      Free;
    end;
  end else
    Result := '1.0.0';
end;

function  CompareVersion(const A, B : String) : Integer; var
  i : Integer;
  _delta : Integer;
  _version1 : TStringList;
  _version2 : TStringList;
  _version : TStringList;
begin
  Result := 0;
  _version1 := TStringList.Create;
  try
    _version1.Delimiter := '.';
    _version1.DelimitedText := A;
    _version2 := TStringList.Create;
    try
      _version2.Delimiter := '.';
      _version2.DelimitedText := B;
      for i := 0 to Min(_version1.Count, _version2.Count)-1 do
      begin
        try
          _delta := StrToInt(_version1[i]) - StrToInt(_version2[i]);
        except
          _delta := CompareText(_version1[i], _version2[i]);
        end;
        if _delta <> 0 then
        begin
          if _delta > 0 then
            Result := 1
          else
            Result := -1;
          Break;
        end;
      end;
      // if we have an equality but the 2 versions don't have the same number of parts
      // then check the remaining parts of the stronger version, and if it contains
      // something different from 0, it will win.
      if Result = 0 then
        if _version1.Count <> _version2.Count then
        begin
          if _version1.Count > _version2.Count then
            _version := _version1
          else
            _version := _version2;
          for i := Min(_version1.Count, _version2.Count) to _version.Count-1 do
          begin
            if StrToIntDef(_version[i], -1) <> 0 then
            begin
              if _version1.Count > _version2.Count then
                Result := 1
              else
                Result := -1;
              Break;
            end;
          end;
        end;
    finally
      _version2.Free;
    end;
  finally
    _version1.Free;
  end;
end;

function ConnectedToInternet : boolean;
{
Call SHELL32.DLL for Win < Win98
otherwise call URL.dll
}
{button code:}
const
  WininetDLL = 'wininet.dll';
  URLDLL = 'url.dll';
  INTERNET_CONNECTION_MODEM = 1;
  INTERNET_CONNECTION_LAN = 2;
  INTERNET_CONNECTION_PROXY = 4;
  INTERNET_CONNECTION_MODEM_BUSY = 8;
var
  hURLDLL: THandle;
  hWininetDLL: THandle;
  dwReserved: DWORD;
  dwConnectionTypes: DWORD;
  fn_InternetGetConnectedState: function(lpdwFlags: LPDWORD; dwReserved: DWORD): BOOL; stdcall;
  InetIsOffline : function(dwFlags: DWORD): BOOL; stdcall;
begin
  Result := False;
  hURLDLL := LoadLibrary(URLDLL);
  if hURLDLL > 0 then
  begin
    @InetIsOffline := GetProcAddress(hURLDLL,'InetIsOffline');
    if Assigned(InetIsOffline) then begin
      if InetIsOffLine(0) then
        Result := False
      else
        Result := True;
    end;
    FreeLibrary(hURLDLL);
  end;

  // Double checking
  if Result then begin
    hWininetDLL := LoadLibrary(WininetDLL);
    if hWininetDLL > 0 then
    begin
      @fn_InternetGetConnectedState := GetProcAddress(hWininetDLL,'InternetGetConnectedState');
      if Assigned(fn_InternetGetConnectedState) then
      begin
        dwReserved := 0;
        dwConnectionTypes := INTERNET_CONNECTION_MODEM or INTERNET_CONNECTION_LAN
          or INTERNET_CONNECTION_PROXY or INTERNET_CONNECTION_MODEM_BUSY;
        Result := fn_InternetGetConnectedState(@dwConnectionTypes, dwReserved);
      end;
      FreeLibrary(hWininetDLL);
    end;
  end;
end;

function GetNthLine(const S : string; LineNo : integer) : string;
var
  SL : TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := S;
    if LineNo <= SL.Count then
      Result := SL[LineNo-1]
    else
      Result := '';
  finally
    SL.Free;
  end;
end;

function GetLineRange(const S : string; StartLine, EndLine : integer) : string;
var
  SL : TStringList;
  i, LastLine : integer;
begin
  Result := '';
  SL := TStringList.Create;
  try
    SL.Text := S;
    LastLine := Min(EndLine-1, SL.Count -1);
    for i := Max(0, StartLine-1) to LastLine do
      if i = LastLine then
        Result := Result + SL[i]
      else
        Result := Result + SL[i] + sLineBreak;
  finally
    SL.Free;
  end;
end;

function GetWordAtPos(const LineText : String; Start : Integer; WordChars : TSysCharSet;
  ScanBackwards : boolean = True; ScanForward : boolean = True;
  HandleBrackets : Boolean = False) : string;
Var
  i, j : integer;
  L, WordStart, WordEnd, ParenCounter, NewStart : integer;
  Bracket, MatchingBracket : Char;
Const
  AllBrackets: array[0..3] of char = ('(', ')', '[', ']');
  CloseBrackets = [')', ']'];
  OpenBrackets = ['(', '['];
begin
  L := Length(LineText);
  WordStart := Start;
  WordEnd := Start;
  if (Start <= 0) or (Start > L) or not (LineText[Start] in WordChars) then
    Result := ''
  else begin
    if ScanBackwards then begin
      i := Start;
      while (i > 1) and (LineText[i-1] in WordChars) do
        Dec(i);
      WordStart := i;
    end;
    if ScanForward then begin
      i := Start;
      while (i < L) and (LineText[i+1] in WordChars) do
        Inc(i);
      WordEnd := i;
    end;
    Result := Copy(LineText, WordStart, WordEnd - WordStart + 1);
  end;

  if HandleBrackets and ScanBackwards then begin
    if (Result = '') then
      NewStart := Start
    else
      NewStart := WordStart - 1;

    if (NewStart > 0) and (LineText[NewStart] in  CloseBrackets) then begin
      //We found a close, go till it's opening paren

      Bracket := LineText[NewStart];
      MatchingBracket := '(';  // Just to avoid warning
      for j := Low(AllBrackets) to High(AllBrackets) do
        if Bracket = AllBrackets[j] then begin
          MatchingBracket := AllBrackets[j xor 1]; // 0 -> 1, 1 -> 0, ...
          break;
        end;

      ParenCounter := 1;
      i := NewStart - 1;
      while (i > 0) and (ParenCounter > 0) do
      begin
        if Linetext[i] = Bracket then inc(ParenCounter)
        else if Linetext[i] = MatchingBracket then dec(ParenCounter);
        Dec(i);
      end;
      WordStart := i+1;
      Result := Copy(LineText, WordStart, NewStart - WordStart + 1) + Result;
      if WordStart > 1 then
        // Recursive call
        Result := GetWordAtPos(LineText, WordStart - 1, WordChars,
          ScanBackWards, False, True) + Result;
    end;
  end;
end;

procedure MaskFPUExceptions(ExceptionsMasked : boolean);
begin
  if ExceptionsMasked then
    Set8087CW($1332 or $3F)
  else
    Set8087CW($1332);
end;

type
  TPythonPathAdder = class(TInterfacedObject, IInterface)
  private
    fPath : string;
    fPathAdded : boolean;
    PackageRootAdder : IInterface;
    fAutoRemove : Boolean;
  public
    constructor Create(const Path : string; AutoRemove : Boolean = True);
    destructor Destroy; override;
  end;

{ TPythonPathAdder }

constructor TPythonPathAdder.Create(const Path: string; AutoRemove : Boolean = True);
var
  S : string;
begin
  inherited Create;
  fPath := PathRemoveSeparator(Path);
  fAutoRemove := AutoRemove;
  if (fPath <> '') and DirectoryExists(fPath) then begin
    // Add parent directory of the root of the package first
    if IsDirPythonPackage(fPath) then begin
      S := ExtractFileDir(GetPackageRootDir(fPath));
      if S <> fPath then
        PackageRootAdder := AddPathToPythonPath(S, AutoRemove);
    end;
    if SysModule.path.contains(Path) then
      fPathAdded := false
    else begin
      SysModule.path.insert(0, fPath);
      fPathAdded := true;
    end;
  end;
end;

destructor TPythonPathAdder.Destroy;
begin
  PackageRootAdder := nil;  // will remove package root
  if fPathAdded and FAutoRemove then
    SysModule.path.remove(fPath);
  inherited;
end;

function AddPathToPythonPath(const Path : string; AutoRemove : Boolean = True) : IInterface;
begin
  Result := TPythonPathAdder.Create(Path, AutoRemove);
end;

function FormatDocString(const DocString : string) : string;
var
  SL : TStringList;
  i, Margin : integer;
begin
  Result := DocString;
  if Result = '' then Exit;

  // Expand Tabs
  Result := StringReplace(Result, #9, '    ', [rfReplaceAll]);

  //Find minimum indentation of any non-blank lines after first line.
  Margin := MaxInt;
  SL := TStringList.Create;
  try
    SL.Text := Result;
    // Trim First Line
    if SL.Count > 0 then
      SL[0] := Trim(SL[0]);

    //  Remove left margin and clear empty lines
    for i := 1 to SL.Count - 1 do
      Margin := Min(Margin, CalcIndent(SL[i]));
    for i := 1 to SL.Count - 1 do begin
      if Margin < MaxInt then
        SL[i] := Copy(SL[i], Margin+1, Length(SL[i]) - Margin);
      if Trim(SL[i]) = '' then
        SL[i] := '';
    end;
    Result := SL.Text;
    // Remove any trailing or leading blank lines.
    Result := StrTrimCharsRight(Result, [#10, #13]);
    Result := StrTrimCharsLeft(Result, [#10, #13]);
  finally
    SL.Free;
  end;
end;

function CalcIndent(S : string; TabWidth : integer = 4): integer;
Var
  i : integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
    if S[i] = #9 then
      Inc(Result, TabWidth)
    else if S[i] = ' ' then
      Inc(Result)
    else
      break;
end;

function IsDirPythonPackage(Dir : string): boolean;
begin
  Result := DirectoryExists(Dir) and
    FileExists(PathAddSeparator(Dir) + '__init__.py');
end;

function GetPackageRootDir(Dir : string): string;
Var
  S : string;
begin
  if not IsDirPythonPackage(Dir) then
    raise Exception.CreateFmt('"%s" is not a Python package', [Dir]);
  S := Dir;
  Repeat
    Result := S;
    S := ExtractFileDir(S);
  Until (Result = S) or (not IsDirPythonPackage(S));
end;

function FileNameToModuleName(const FileName : string): string;
Var
  Path, Dir : string;
begin
  Result := PathRemoveExtension(ExtractFileName(FileName));
  Path := ExtractFileDir(FileName);
  Dir := ExtractFileName(Path);

  if Path <> '' then begin
    while IsDirPythonPackage(Path) and (Dir <> '') do begin
      Result := Dir + '.' + Result;
      Path := ExtractFileDir(Path);
      Dir := ExtractFileName(Path);
    end;
    if StrIsRight(PChar(Result), '.__init__') then
      Delete(Result, Length(Result) - 8, 9);
  end;
end;

function HTMLSafe(const S : string): string;
begin
  Result := StringReplace(S, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, #13#10, '<br>', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '<br>', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '<br>', [rfReplaceAll]);
end;

function GetParamStr(P: PChar; var Param: string): PChar;
// From Delphi's system.pas unit!
var
  i, Len: Integer;
  Start, S, Q: PChar;
begin
  while True do
  begin
    while (P[0] <> #0) and (P[0] <= ' ') do
      P := CharNext(P);
    if (P[0] = '"') and (P[1] = '"') then Inc(P, 2) else Break;
  end;
  Len := 0;
  Start := P;
  while P[0] > ' ' do
  begin
    if P[0] = '"' then
    begin
      P := CharNext(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        Q := CharNext(P);
        Inc(Len, Q - P);
        P := Q;
      end;
      if P[0] <> #0 then
        P := CharNext(P);
    end
    else
    begin
      Q := CharNext(P);
      Inc(Len, Q - P);
      P := Q;
    end;
  end;

  SetLength(Param, Len);

  P := Start;
  S := Pointer(Param);
  i := 0;
  while P[0] > ' ' do
  begin
    if P[0] = '"' then
    begin
      P := CharNext(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        Q := CharNext(P);
        while P < Q do
        begin
          S[i] := P^;
          Inc(P);
          Inc(i);
        end;
      end;
      if P[0] <> #0 then P := CharNext(P);
    end
    else
    begin
      Q := CharNext(P);
      while P < Q do
      begin
        S[i] := P^;
        Inc(P);
        Inc(i);
      end;
    end;
  end;

  Result := P;
end;

function ReadLnFromStream(Stream : TStream; AMaxLineLength: Integer = -1;
  AExceptionIfEOF: Boolean = FALSE): String;

  function FindEOL(ABuf: PChar; var VLineBufSize: Integer; var VCrEncountered: Boolean): Integer;
  var
    i: Integer;
  begin
    Result := VLineBufSize; //EOL not found => use all
    i := 0; //[0..ALineBufSize-1]
    while i < VLineBufSize do begin
      case ABuf[i] of
        AnsiLineFeed:
          begin
            Result := i; {string size}
            VCrEncountered := TRUE;
            VLineBufSize := i+1;
            BREAK;
          end;//LF
        AnsiCarriageReturn:
          begin
            Result := i; {string size}
            VCrEncountered := TRUE;
            inc(i); //crLF?
            if (i < VLineBufSize) and (ABuf[i] = AnsiLineFeed) then begin
              VLineBufSize := i+1;
            end
            else begin
              VLineBufSize := i;
            end;
            BREAK;
          end;//CR
      end;//case
      Inc(i);
    end;//while
  End;//FindEOL

const
  LBUFMAXSIZE = 2048;
var
  LBufSize, LStringLen, LResultLen: Integer;
  LBuf: packed array [0..LBUFMAXSIZE] of Char;
  LStrmPos, LStrmSize: Integer; //LBytesToRead = stream size - Position
  LCrEncountered: Boolean;
begin
  if AMaxLineLength < 0 then begin
    AMaxLineLength := MaxInt;
  end;//if
  LCrEncountered := FALSE;
  Result := '';
  { we store the stream size for the whole routine to prevent
  so do not incur a performance penalty with TStream.Size.  It has
  to use something such as Seek each time the size is obtained}
  {LStrmPos := SrcStream.Position; LStrmSize:= SrcStream.Size; 4 seek vs 3 seek}
  LStrmPos := Stream.Seek(0, soFromCurrent); //Position
  LStrmSize:= Stream.Seek(0, soFromEnd); //Size
  Stream.Seek(LStrmPos, soFromBeginning); //return position

  if (LStrmSize - LStrmPos) > 0 then begin

    while (LStrmPos < LStrmSize) and NOT LCrEncountered do begin
      LBufSize := Min(LStrmSize - LStrmPos, LBUFMAXSIZE);
      Stream.ReadBuffer(LBuf, LBufSize);
      LStringLen := FindEOL(LBuf,LBufSize,LCrEncountered);
      Inc(LStrmPos,LBufSize);

      LResultLen := Length(Result);
      if (LResultLen + LStringLen) > AMaxLineLength then begin
        LStringLen := AMaxLineLength - LResultLen;
        LCrEncountered := TRUE;
        Dec(LStrmPos,LBufSize);
        Inc(LStrmPos,LStringLen);
      end;//if
      SetLength(Result, LResultLen + LStringLen);
      Move(LBuf[0], PChar(Result)[LResultLen], LStringLen);
    end;//while
    Stream.Position := LStrmPos;
  end
  else begin
    if AExceptionIfEOF then begin
      raise Exception.Create(Format('End of stream at %d',[LStrmPos])); //LOCALIZE
    end;
  end;//if NOT EOF
End;//ReadLn


function ParsePySourceEncoding(Textline : string): string;
var
  RegExpr : TRegExpr;
begin
  Result := '';
  RegExpr := TRegExpr.Create;
  try
    RegExpr.Expression := 'coding[:=]\s*([-\w.]+)';
    if RegExpr.Exec(TextLine) then
      Result := RegExpr.Match[1];
  finally
    RegExpr.Free;
  end;
end;

initialization
  (* default is to return text without translation *)
  Translate:= NoTranslate;
end.



