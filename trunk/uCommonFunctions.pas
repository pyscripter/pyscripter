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
  Windows, Classes, SysUtils, Graphics, SynEditTypes,
  WideStrings, SynUnicode, uEditAppIntfs, VirtualFileSearch, SpTBXMDIMRU,
  SpTBXSkins, Controls;

const
  UTF8BOMString : string = Char($EF) + Char($BB) + Char($BF);
  IdentChars: TSysCharSet = ['_', '0'..'9', 'A'..'Z', 'a'..'z'];
  WideLineBreak : WideString = WideString(sLineBreak);
  SFileExpr = '(([a-zA-Z]:)?[^\*\?="<>|:,;\+\^]+)'; // fwd slash (/) is allowed
  STracebackFilePosExpr =  '"\<?' + SFileExpr + '\>?", line (\d+)(, in ([\<\>\?\w]+))?';
  SWarningFilePosExpr = SFileExpr + ':(\d+):';
  WideLF = WideChar(#10);
  WideNull = WideChar(#0);
  AnsiLineFeed       = Char(#10);
  AnsiCarriageReturn = Char(#13);
  AnsiCrLf           = string(#13#10);

(* returns the System ImageList index of the icon of a given file *)
function GetIconIndexFromFile(const AFileName: WideString;
  const ASmall: boolean): integer;

(* returns long file name even for nonexisting files *)
function GetLongFileName(const APath: string): string; overload;
function GetLongFileName(const APath: WideString): WideString; overload;

(* from cStrings *)
(* checks if AText starts with ALeft *)
function WideStrIsLeft(AText, ALeft: PWideChar): Boolean;

(* checks if AText starts with ALeft *)
function StrIsLeft(AText, ALeft: PChar): Boolean;

(* checks if AText ends with ARight *)
function StrIsRight(AText, ARight: PChar): Boolean;

(* checks if AText ends with ARight *)
function WideStrIsRight(AText, ARight: PWideChar): Boolean;

(* returns next token - based on Classes.ExtractStrings *)
function StrGetToken(var Content: PWideChar;
                     Separators, WhiteSpace, QuoteChars: TSysCharSet): WideString;

(* removes quotes to AText, if needed *)
function StrUnQuote(const AText: WideString): WideString;

(* Lighten a given Color by a certain percentage *)
function LightenColor(Color:TColor; Percentage:integer):TColor;

(* Darken a given Color by a certain percentage *)
function DarkenColor(Color:TColor; Percentage:integer):TColor;

(* Return either clSkyBlue or clHighlight depending on current settings *)
function SelectionBackgroundColor():TColor;

(* Get Exe File Version string *)
function ApplicationVersion : string;

(* Compares two Version strings and returns -1, 0, 1 depending on result *)
function  CompareVersion(const A, B : String) : Integer;

(* Checks whether we are connected to the Internet *)
function ConnectedToInternet : boolean;

(* Extracts the nth line from a string *)
function GetNthLine(const S : WideString; LineNo : integer) : WideString;

(* Extracts a range of lines from a string *)
function GetLineRange(const S : string; StartLine, EndLine : integer) : string;

(* Extracts a word from a string *)
function GetWordAtPos(const LineText : WideString; Start : Integer; WordChars : TSysCharSet;
  ScanBackwards : boolean = True; ScanForward : boolean = True;
  HandleBrackets : Boolean = False) : WideString;

(* Mask FPU Excptions - Useful for importing SciPy and other Python libs *)
procedure MaskFPUExceptions(ExceptionsMasked : boolean);

(* Format a doc string by removing left space and blank lines at start and bottom *)
function FormatDocString(const DocString : WideString) : WideString;

(* Calculate the indentation level of a line *)
function CalcIndent(S : WideString; TabWidth : integer = 4): integer;

(* check if a directory is a Python Package *)
function IsDirPythonPackage(Dir : WideString): boolean;

(* Get Python Package Root directory *)
function GetPackageRootDir(Dir : WideString): WideString;

(* Python FileName to possibly dotted ModuleName accounting for packages *)
function FileNameToModuleName(const FileName : WideString): WideString;

(* Convert <  > to &lt; &gt; *)
function HTMLSafe(const S : string): string; overload;
function HTMLSafe(const S : WideString): WideString; overload;

(* Parses command line parameters *)
// From Delphi's system.pas unit! Need to rewrite
function GetParamStr(P: PChar; var Param: string): PChar;

(* ReadLn that works with Sreams *)
// Adapted from Indy
function ReadLnFromStream(Stream : TStream; AMaxLineLength: Integer = -1;
  AExceptionIfEOF: Boolean = FALSE): String;

(* Parse a line for a Python encoding spec *)
function ParsePySourceEncoding(Textline : WideString): string;

(* Delphi's InputQuery supporting Wide strings - based on TnT library *)
//function WideInputQuery(const ACaption, APrompt: WideString; var Value: WideString): Boolean;

(* Version of WideInputQuery that can be called from threads and executes in the main thread *)
function SyncWideInputQuery(const ACaption, APrompt: WideString; var Value: WideString): Boolean;

(* Covert all line breaks to #10 *)
function CleanEOLs(S: string): string; overload;
function CleanEOLs(S: WideString): WideString; overload;

(* Similar to Delphi's IdentToInt but operating on sorted IdentMapEntries *)
function SortedIdentToInt(const Ident: string; var Int: Longint;
                          const SortedMap: array of TIdentMapEntry;
                          CaseSensitive : Boolean = False): Boolean;

(* Used for sorting Python Identifiers *)
function ComparePythonIdents(const S1, S2 : WideString): Integer; overload;
function ComparePythonIdents(List: TWideStringList; Index1, Index2: Integer): Integer; overload;

(* Used to get Vista fonts *)
procedure SetDefaultFonts(const AFont: TFont);
procedure SetDesktopIconFonts(const AFont: TFont);
procedure SetVistaContentFonts(const AFont: TFont);

(* Get the text between two Synedit Block coordinates *)
function GetBlockText(Strings : TUnicodeStrings; BlockBegin, BlockEnd : TBufferCoord) : WideString;

(* Extract Error information from a VarPyth variant containing the Python error *)
procedure ExtractPyErrorInfo(E: Variant; var FileName: WideString; var LineNo: Integer; var Offset: Integer);

(* Get Encoded Ansi string from WideStrings ttaking into account Python file encodings *)
function WideStringsToEncodedText(const AFileName: WideString;
  Lines : TUnicodeStrings; Encoding : TFileSaveFormat; var EncodedText: string;
  InformationLossWarning: Boolean = False) : Boolean;

(* Load file into WideStrings taking into account Python file encodings *)
function LoadFileIntoWideStrings(const AFileName: WideString;
  Lines : TUnicodeStrings; var Encoding : TFileSaveFormat): boolean;

(* Save WideStrings to file taking into account Python file encodings *)
function SaveWideStringsToFile(const AFileName: WideString;
  Lines : TUnicodeStrings; Encoding : TFileSaveFormat;
  DoBackup : Boolean = True) : boolean;

(* Read File contents. Allows reading of locked files *)
function FileToStr(const FileName: String): String;

(* Read File contents into encoded string. Takes into account Python encodings *)
function FileToEncodedStr(const AFileName : WideString) : string;

(* Read File contents into Widestring. Takes into account Python encodings *)
function FileToWideStr(const AFileName : WideString) : WideString;

(*
  Builds a list of files in FileList matching certain criteria.
  Uses VirtualFileSearch that is found in the Mustangpeak VirtualFileExplorer package.
  It is Unicode based.
*)
procedure BuildFileList(const Path, Masks: WideString;
  FileList: TWideStrings; Recursive: Boolean; SearchAttribs,
  SearchExcludeAttribs: TVirtualSearchAttribs);

(* Get the raw FileTime of a File's last write operation *)
function FileTimeLastWriteRaw(AFileName: WideString; var Time : TFileTime) : Boolean;

(* Find the position of a WideChar in a WideString *)
function WideCharPos(const S: WideString; const C: WideChar; const Index: Integer = 1): Integer;

(* Check whether is S is likely to be a number *)
function WideStrConsistsofNumberChars(const S: WideString): Boolean;

(* Trim certain chars from left of string *)
function WideStrTrimCharsLeft(const S: WideString; const Chars: TSysCharSet): WideString;

(* Trim certain chars from right of string *)
function WideStrTrimCharsRight(const S: Widestring; const Chars: TSysCharSet): WideString;

(* Extracts a token and returns the remainder of a string *)
function WideStrToken(var S: WideString; Separator: WideChar): WideString;

(* Gets the Clipboard contents as Unicode string *)
function GetClipboardWideText : WideString;

(* Sets the Clipboard contents as Unicode string *)
procedure SetClipboardWideText(AText : WideString);

(* Check whether the Clipboard can provide Unicode string *)
function ClipboardProvidesWideText : Boolean;

(* Unicode vesion of PosEx *)
function WidePosEx(const SubStr, S: WideString; Offset: Integer = 1): Integer;

(* Unicode vesion of CharLastPos (in JclStrings) *)
function WideCharLastPos(const S: WideString; const C: WideChar; const Index: Integer = 1): Integer;

(* Unicode vesion of StrReplaceChars (in JclStrings) *)
function WideStrReplaceChars(const S: WideString; const Chars: TSysCharSet; Replace: WideChar): WideString;

(* Unicode vesion of StrRemoveChars (in JclStrings) *)
function WideStrRemoveChars(const S: WideString; const Chars: TSysCharSet): WideString;

(*  Returns the index of the last occurence of Substr in Str.
   If IgnoreCase=True, the search is not case sensitive, assuming that Substr is
   in lower case *)
function WideLastPos(const Substr, Str: WideString; IgnoreCase: Boolean = False): Integer;

(* Extract MRU items to Wide Strings *)
procedure MRUToWideStrings(MRU : TSpTBXMRUListItem; SL : TWideStrings);

(* Load MRU items From WideStrings *)
procedure WideStringsToMRU(MRU : TSpTBXMRUListItem; SL : TWideStrings);

(* Get Hot Color from SpTBX Skin option entry *)
function GetHotColor(OptionEntry : TSpTBXSkinOptionEntry) : TColor;

(* Trim avoiding widestring copying in not needed *)
procedure TrimIfNeeded(var S : WideString);

(* Improved CanFocus *)
function CanActuallyFocus(WinControl: TWinControl): Boolean;


Const
  ZeroFileTime : TFileTime = (dwLowDateTime : 0; dwHighDateTime : 0);

implementation
Uses
  Forms, JclFileUtils, Math, VarPyth,
  JclBase, SynRegExpr, TntDialogs, TntClasses,
  TntWindows, StrUtils, WideStrUtils, PythonEngine, dmCommands, Dialogs,
  StringResources, TntSysUtils, frmPythonII, gnugettext, MPCommonUtilities,
  MPCommonObjects, MPShellUtilities;

function GetIconIndexFromFile(const AFileName: WideString;
  const ASmall: boolean): integer;
Var
  NameSpace : TNameSpace;
  IconSize : TIconSize;
begin
  Result:= -1;
  // swallow any exceptions (bug report by Colin Williams)
  try
    if WideFileExists(AFileName) then begin
      if ASmall then
        IconSize := icSmall
      else
        IconSize := icLarge;
      NameSpace := TNameSpace.CreateFromFileName(AFileName);
      try
        Result := NameSpace.GetIconIndex(False, IconSize);
      finally
        NameSpace.Free;
      end;
    end;
  except
  end;
end;

// Not defined in Windows.pas
function GetLongPathNameW(lpszLongPath: PWideChar; lpszShortPath: PWideChar; cchBuffer: DWORD): DWORD; stdcall; external 'Kernel32.dll';

function ShortPathToLongPath(const Path: WideString): WideString;

// Converts the given path to its long form (if that exists). If Path is not an existing path or the long form
// does not exist for any reason the Path itself is returned.

var
  Buffer: array[0..MAX_PATH] of WideChar;
  Count: DWORD;

begin
  Count := GetLongPathNameW(PWideChar(Path), Buffer, SizeOf(Buffer));
  SetString(Result, Buffer, Count);
end;


function GetLongFileName(const APath: WideString): WideString;
(* returns long file name even for nonexisting files *)
begin
  if APath = '' then Result:= ''
  else begin
    Result:= ShortPathToLongPath(APath);
    // if different - function is working
    if (Result = '') or
       ((Result = APath) and
         not (WideFileExists(WideExcludeTrailingPathDelimiter(APath)) or
              WideDirectoryExists(WideExcludeTrailingPathDelimiter(APath)))) then
    begin
      Result:= WideExtractFilePath(APath);
      // we are up to top level
      if (Result = '') or (Result[Length(Result)] = ':') then
        Result:= APath
      else Result:= GetLongFileName(WideExcludeTrailingPathDelimiter(Result)) +
                           PathDelim + WideExtractFileName(APath);
    end;
  end;
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
         not (FileExists(ExcludeTrailingPathDelimiter(APath)) or
              DirectoryExists(ExcludeTrailingPathDelimiter(APath)))) then
    begin
      Result:= ExtractFilePath(APath);
      // we are up to top level
      if (Result = '') or (Result[Length(Result)] = ':') then
        Result:= APath
      else Result:= Concat(GetLongFileName(ExcludeTrailingPathDelimiter(Result)),
                           PathDelim, ExtractFileName(APath));
    end;
  end;
end;

(* from cStrings *)

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

function WideStrIsRight(AText, ARight: PWideChar): Boolean;
(* checks if AText ends with ARight *)
var
  LenDiff: Integer;
begin
  Result:= ARight = nil;
  LenDiff := WStrLen(AText) - WStrLen(ARight);
  if not Result and (LenDiff >= 0) then begin
    Inc(AText, LenDiff);
    Result := WideStrIsLeft(AText, ARight);
  end;
end;

function StrGetToken(var Content: PWideChar;
                     Separators, WhiteSpace, QuoteChars: TSysCharSet): WideString;
(* returns next token - based on Classes.ExtractStrings *)
var
  Head, Tail: PWideChar;
  InQuote: Boolean;
  QuoteChar: WideChar;
begin
  Result:= '';
  if (Content = nil) or (Content^=#0) then Exit;
  Tail := Content;
  InQuote := False;
  QuoteChar := #0;
  while inOpSet(Tail^, WhiteSpace) do Inc(Tail);
  Head := Tail;
  while True do begin
    while (InQuote and not InOpSet(Tail^, QuoteChars + [#0])) or
      not InOpSet(Tail^, Separators + WhiteSpace + QuoteChars + [#0]) do Inc(Tail);
    if InOpSet(Tail^, QuoteChars) then begin
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

function StrUnQuote(const AText: WideString): WideString;
(* removes quotes to AText, if needed *)
var
  PText: PWideChar;
begin
  if PWideChar(AText)^ in [WideChar('"'), WideChar('''')] then begin
    PText:= PWideChar(AText);
    Result:= WideExtractQuotedStr(PText, PText^);
  end
  else Result:= AText;
end;

(* from cStrings end *)

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
  hURLDLL := SafeLoadLibrary(URLDLL);
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
    hWininetDLL := SafeLoadLibrary(WininetDLL);
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

function GetNthLine(const S : WideString; LineNo : integer) : WideString;
var
  SL : TWideStringList;
begin
  SL := TWideStringList.Create;
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

function GetWordAtPos(const LineText : WideString; Start : Integer; WordChars : TSysCharSet;
  ScanBackwards : boolean = True; ScanForward : boolean = True;
  HandleBrackets : Boolean = False) : Widestring;
Var
  i, j : integer;
  L, WordStart, WordEnd, ParenCounter, NewStart : integer;
  Bracket, MatchingBracket : WideChar;
Const
  AllBrackets: array[0..3] of WideChar = ('(', ')', '[', ']');
  CloseBrackets = [')', ']'];
  OpenBrackets = ['(', '['];
begin
  L := Length(LineText);
  WordStart := Start;
  WordEnd := Start;
  if (Start <= 0) or (Start > L) or not InOpSet(LineText[Start], WordChars) then
    Result := ''
  else begin
    if ScanBackwards then begin
      i := Start;
      while (i > 1) and InOpSet(LineText[i-1], WordChars) do
        Dec(i);
      WordStart := i;
    end;
    if ScanForward then begin
      i := Start;
      while (i < L) and InOpSet(LineText[i+1], WordChars) do
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

    if (NewStart > 0) and InOpSet(LineText[NewStart], CloseBrackets) then begin
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

function FormatDocString(const DocString : WideString) : WideString;
var
  SL : TWideStringList;
  i, Margin : integer;
begin
  Result := DocString;
  if Result = '' then Exit;

  // Expand Tabs
  Result := StringReplace(Result, #9, '    ', [rfReplaceAll]);

  //Find minimum indentation of any non-blank lines after first line.
  Margin := MaxInt;
  SL := TWideStringList.Create;
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
    Result := WideStrTrimCharsRight(Result, [#10, #13]);
    Result := WideStrTrimCharsLeft(Result, [#10, #13]);
  finally
    SL.Free;
  end;
end;

function CalcIndent(S : WideString; TabWidth : integer = 4): integer;
Var
  i : integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
    if S[i] = WideChar(#9) then
      Inc(Result, TabWidth)
    else if S[i] = ' ' then
      Inc(Result)
    else
      break;
end;

function IsDirPythonPackage(Dir : WideString): boolean;
begin
  Result := WideDirectoryExists(Dir) and
    WideFileExists(WideIncludeTrailingPathDelimiter(Dir) + '__init__.py');
end;

function GetPackageRootDir(Dir : WideString): WideString;
Var
  S : WideString;
begin
  if not IsDirPythonPackage(Dir) then
    raise Exception.CreateFmt('"%s" is not a Python package', [Dir]);
  S := Dir;
  Repeat
    Result := S;
    S := WideExtractFileDir(S);
  Until (Result = S) or (not IsDirPythonPackage(S));
end;

function FileNameToModuleName(const FileName : WideString): WideString;
Var
  Path, Dir : WideString;
begin
  Result := WideStripExt(WideExtractFileName(FileName));
  Path := WideExtractFileDir(FileName);
  Dir := WideExtractFileName(Path);

  if Path <> '' then begin
    while IsDirPythonPackage(Path) and (Dir <> '') do begin
      Result := Dir + '.' + Result;
      Path := WideExtractFileDir(Path);
      Dir := WideExtractFileName(Path);
    end;
    if WideStrIsRight(PWideChar(Result), '.__init__') then
      Delete(Result, Length(Result) - 8, 9);
  end;
end;

function HTMLSafe(const S : string): string; overload;
begin
  Result := StringReplace(S, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, #13#10, '<br>', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '<br>', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '<br>', [rfReplaceAll]);
end;

function HTMLSafe(const S : WideString): WideString; overload;
begin
  Result := WideStringReplace(S, '<', '&lt;', [rfReplaceAll]);
  Result := WideStringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := WideStringReplace(Result, #13#10, '<br>', [rfReplaceAll]);
  Result := WideStringReplace(Result, #13, '<br>', [rfReplaceAll]);
  Result := WideStringReplace(Result, #10, '<br>', [rfReplaceAll]);
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


function ParsePySourceEncoding(Textline : WideString): string;
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

function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of WideChar;
  tm: TTextMetric;
begin
  for I := 0 to 25 do Buffer[I] := WideChar(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := WideChar(I + Ord('a'));
  GetTextMetrics(Canvas.Handle, tm);
  GetTextExtentPointW(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := (Result.X div 26 + 1) div 2;
  Result.Y := tm.tmHeight;
end;

//function WideInputQuery(const ACaption, APrompt: WideString; var Value: WideString): Boolean;
//var
//  Form: TForm;
//  Prompt: TLabel;
//  Edit: TWideEdit;
//  DialogUnits: TPoint;
//  ButtonTop, ButtonWidth, ButtonHeight: Integer;
//begin
//  Result := False;
//  Form := TForm.Create(Application);
//  with Form do begin
//    try
//      Canvas.Font := Font;
//      DialogUnits := GetAveCharSize(Canvas);
//      BorderStyle := bsDialog;
//      Caption := ACaption;
//      ClientWidth := MulDiv(180, DialogUnits.X, 4);
//      Position := poScreenCenter;
//      Prompt := TLabel.Create(Form);
//      with Prompt do
//      begin
//        Parent := Form;
//        Caption := APrompt;
//        Left := MulDiv(8, DialogUnits.X, 4);
//        Top := MulDiv(8, DialogUnits.Y, 8);
//        Constraints.MaxWidth := MulDiv(164, DialogUnits.X, 4);
//        WordWrap := True;
//      end;
//      Edit := TWideEdit.Create(Form);
//      with Edit do
//      begin
//        Parent := Form;
//        Left := Prompt.Left;
//        Top := Prompt.Top + Prompt.Height + 5;
//        Width := MulDiv(164, DialogUnits.X, 4);
//        MaxLength := 255;
//        Text := Value;
//        SelectAll;
//      end;
//      ButtonTop := Edit.Top + Edit.Height + 15;
//      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
//      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
//      with TButton.Create(Form) do
//      begin
//        Parent := Form;
//        Caption := SMsgDlgOK;
//        ModalResult := mrOk;
//        Default := True;
//        SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth,
//          ButtonHeight);
//      end;
//      with TButton.Create(Form) do
//      begin
//        Parent := Form;
//        Caption := SMsgDlgCancel;
//        ModalResult := mrCancel;
//        Cancel := True;
//        SetBounds(MulDiv(92, DialogUnits.X, 4), Edit.Top + Edit.Height + 15, ButtonWidth,
//          ButtonHeight);
//        Form.ClientHeight := Top + Height + 13;
//      end;
//      if ShowModal = mrOk then
//      begin
//        Value := Edit.Text;
//        Result := True;
//      end;
//    finally
//      Form.Free;
//    end;
//  end;
//end;

type
  TSyncInputQuery = class
    public
    Caption, Prompt, Value : WideString;
    Res : Boolean;
    constructor Create(ACaption, APrompt, AValue : WideString);
    procedure InputQuery;
  end;

{ TSyncInputQuery }

constructor TSyncInputQuery.Create(ACaption, APrompt, AValue: WideString);
begin
  Caption := ACaption;
  Prompt := APrompt;
  Value := AValue;
end;

procedure TSyncInputQuery.InputQuery;
begin
  Res := WideInputQuery(Caption, Prompt, Value);
end;

function SyncWideInputQuery(const ACaption, APrompt: WideString; var Value: WideString): Boolean;
var
  SyncInputQuery : TSyncInputQuery;
begin
  if GetCurrentThreadId = MainThreadId then
    Result := WideInputQuery(ACaption, APrompt, Value)
  else begin
    SyncInputQuery := TSyncInputQuery.Create(ACaption, APrompt, Value);
    try
      TThread.Synchronize(nil, SyncInputQuery.InputQuery);
      Result := SyncInputQuery.Res;
      Value := SyncInputQuery.Value;
    finally
      SyncInputQuery.Free;
    end;
  end;
end;

function CleanEOLs(S: string): string;
begin
  Result := AdjustLineBreaks(S, System.tlbsLF)
end;

function CleanEOLs(S: WideString): WideString;
begin
  Result := WideAdjustLineBreaks(S, System.tlbsLF)
end;

function SortedIdentToInt(const Ident: string; var Int: Longint;
                          const SortedMap: array of TIdentMapEntry;
                          CaseSensitive : Boolean = False): Boolean;
var
  m, n, k, I: Integer;
begin
  m := Low(SortedMap); n := High(SortedMap);
  while m<=n do
    begin
      k := m+(n-m) div 2;
      if CaseSensitive then
        I := CompareStr(Ident, SortedMap[k].Name)
      else
        I := CompareText(Ident, SortedMap[k].Name);
      if I = 0 then begin
          Result := true;
          Int := SortedMap[k].Value;
          exit;
      end else if I > 0 then
         m := k+1
      else
        n := k-1;
    end;
  Result := false
end;

function ComparePythonIdents(const S1, S2 : WideString): Integer; overload;
begin
  if (S1[1] = WideChar('_')) and (S2[1] = WideChar('_')) then
    Result := WideCompareStr(S1, S2)
  else if S1[1] = '_' then
    Result := 1
  else if S2[1] = '_' then
    Result := -1
  else
    Result := WideCompareStr(S1, S2)
end;

function ComparePythonIdents(List: TWideStringList; Index1, Index2: Integer): Integer; overload;
Var
  S1, S2 : WideString;
begin
  S1 := List[Index1];
  S2 := List[Index2];
  Result := ComparePythonIdents(S1, S2);
end;

procedure SetDefaultFonts(const AFont: TFont);
begin
  AFont.Handle := GetStockObject(DEFAULT_GUI_FONT);
end;

procedure SetDesktopIconFonts(const AFont: TFont);
var
  LogFont: TLogFont;
begin
  if SystemParametersInfo(SPI_GETICONTITLELOGFONT, SizeOf(LogFont),
    @LogFont, 0) then
    AFont.Handle := CreateFontIndirect(LogFont)
  else
    SetDefaultFonts(AFont);
end;

procedure SetVistaContentFonts(const AFont: TFont);      
Const
  VistaContentFont = 'Calibri';
begin
  if Win32PlatformIsVista
    and not SameText(AFont.Name, VistaContentFont)
    and (Screen.Fonts.IndexOf(VistaContentFont) >= 0) then
  begin
    AFont.Size := AFont.Size + 1;
    AFont.Name := VistaContentFont;
  end;
end;

function GetBlockText(Strings : TUnicodeStrings; BlockBegin, BlockEnd : TBufferCoord) : WideString;
Var
  Line :  integer;
begin
  // preconditions start
  Assert(BlockBegin.Line <= Strings.Count);
  Assert(BlockEnd.Line <= Strings.Count);
  Assert(BlockBegin.Line <= BlockEnd.Line);
  if BlockBegin.Line <= 0 then Exit;
  if BlockEnd.Line <= 0 then Exit;
  // preconditions end

  // work backwards
  Line := BlockEnd.Line;
  Result := StrUtils.LeftStr(Strings[Line-1], BlockEnd.Char - 1);
  While (Line > BlockBegin.Line) and (Line > 1) do begin
    Dec(Line);
    Result := Strings[Line-1] + WideCRLF + Result;
  end;
  if Line = BlockBegin.Line then
    Delete(Result, 1, BlockBegin.Char -1);
end;

procedure ExtractPyErrorInfo(E: Variant; var FileName: WideString; var LineNo: Integer; var Offset: Integer);
begin
  try
    FileName := E.filename;
  except
    FileName := '';
  end;
  try
    LineNo := E.lineno;
  except
    LineNo := 0;
  end;
  try
    Offset := E.offset;
  except
    Offset := 0;
  end;
end;

function WideStringsToEncodedText(const AFileName: WideString;
  Lines : TUnicodeStrings; Encoding : TFileSaveFormat; var EncodedText: string;
  InformationLossWarning: Boolean = False) : Boolean;
// AFileName is passed just for the warning
var
  PyEncoding : string;
  UniPy, EncodedString : PPyObject;
  wStr, LineBreak : WideString;
  SuppressOutput : IInterface;
begin
  Result := True;

  case Lines.FileFormat of
    sffDos:
      LineBreak := WideCRLF;
    sffUnix:
      LineBreak := WideLF;
    sffMac:
      LineBreak := WideCR;
    sffUnicode:
      if Encoding = sf_Ansi then
        // Ansi-file cannot contain Unicode LINE SEPARATOR,
        // so default to platform-specific Ansi-compatible LineBreak
        LineBreak := SynUnicode.SLineBreak
      else
        LineBreak := WideLineSeparator;
  end;

  wStr := Lines.GetSeparatedText(LineBreak);

  case Encoding of
    sf_Ansi :
      if CommandsDataModule.FileIsPythonSource(AFileName) then begin
        PyEncoding := '';
        if Lines.Count > 0 then
          PyEncoding := ParsePySourceEncoding(Lines[0]);
        if (PyEncoding = '') and (Lines.Count > 1) then
          PyEncoding := ParsePySourceEncoding(Lines[1]);

        with GetPythonEngine do begin
          if PyEncoding = '' then
            PyEncoding := SysModule.getdefaultencoding();
          SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
          UniPy := nil;
          EncodedString := nil;
          try
            try
              UniPy := PyUnicode_FromWideChar(PWideChar(wStr), Length(wStr));
              CheckError;
              if InformationLossWarning then begin
                try
                  EncodedString := PyUnicode_AsEncodedString(UniPy, PChar(PyEncoding), 'strict');
                  CheckError;
                  EncodedText := PyString_AsDelphiString(EncodedString);
                  CheckError;
                except
                  on UnicodeEncodeError do begin
                    Result :=
                      WideMessageDlg(WideFormat(_(SFileEncodingWarning),
                        [AFileName, PyEncoding]), mtWarning, [mbYes, mbCancel], 0)= mrYes;
                    if Result then begin
                      EncodedString := PyUnicode_AsEncodedString(UniPy, PChar(PyEncoding), 'replace');
                      CheckError;
                      EncodedText := PyString_AsDelphiString(EncodedString);
                      CheckError;
                    end;
                  end;
                end;
              end else begin
                  EncodedString := PyUnicode_AsEncodedString(UniPy, PChar(PyEncoding), 'replace');
                  CheckError;
                  EncodedText := PyString_AsDelphiString(EncodedString);
                  CheckError;
              end;
            finally
              Py_XDECREF(UniPy);
              Py_XDECREF(EncodedString);
            end;
          except
            PyErr_Clear;
            EncodedText := wStr;
            if InformationLossWarning then
              Result :=
                WideMessageDlg(WideFormat(_(SFileEncodingWarning),
                  [AFileName, PyEncoding]), mtWarning, [mbYes, mbCancel], 0)= mrYes ;
          end;
        end;
      end else begin
        EncodedText := wStr;
        if InformationLossWarning and not IsAnsiOnly(wStr) then begin
          Result :=
            WideMessageDlg(WideFormat(_(SFileEncodingWarning),
            [AFileName, 'ANSI']), mtWarning, [mbYes, mbCancel], 0)= mrYes ;
        end;
      end;
    sf_UTF8 : EncodedText := UTF8BOMString + UTF8Encode(wStr);
    sf_UTF8_NoBOM : EncodedText := UTF8Encode(wStr);
    sf_UTF16LE, sf_UTF16BE : EncodedText := wStr;
  end;
end;

function LoadFileIntoWideStrings(const AFileName: WideString;
  Lines : TUnicodeStrings; var Encoding : TFileSaveFormat): boolean;
Var
  FileStream : TWideFileStream;
  FileText, S, PyEncoding : string;
  Len : integer;
  IsPythonFile : boolean;
  FileEncoding : TSynEncoding;
  HasBOM : Boolean;
  PyWstr : PPyObject;
begin
  Result := True;
  if (AFileName <> '') and WideFileExists(AFileName) then begin
    IsPythonFile :=  CommandsDataModule.FileIsPythonSource(AFileName);
    try
      FileStream := TWideFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
      try
        // Read the file into FileText
        Len := FileStream.Size;
        SetLength(FileText, Len);
        FileStream.ReadBuffer(FileText[1], Len);
        FileStream.Seek(0, soFromBeginning);

        // This routine detects UTF8 text even if there is no BOM
        FileEncoding := GetEncoding(FileStream, HasBOM);
        case FileEncoding of
          seAnsi : Encoding := sf_Ansi;
          seUTF8 :
            begin
              if not HasBOM then begin
                if IsPythonFile then
                  // Ignore detected UTF8 if it is Python and does not have BOM
                  // File will still be read as UTF8 if it has an encoding comment
                  Encoding := sf_Ansi
                else begin
                  if CommandsDataModule.PyIDEOptions.DetectUTF8Encoding then
                    Encoding := sf_UTF8_NoBOM
                  else
                    Encoding := sf_Ansi;
                end;
              end else
                Encoding := sf_UTF8;
            end;
          seUTF16LE : Encoding := sf_UTF16LE;
          seUTF16BE : Encoding := sf_UTF16BE;
        else
          Raise Exception.Create(WideFormat(_(SInternalError), ['LoadFileIntoWideStrings']));
        end;

        case Encoding of
          sf_Ansi :
            // if it is a Pytyhon file detect an encoding spec
            if IsPythonFile then begin
              PyEncoding := '';
              S := ReadLnFromStream(FileStream);
              PyEncoding := ParsePySourceEncoding(S);
              if PyEncoding = '' then begin
                S := ReadLnFromStream(FileStream);
                PyEncoding := ParsePySourceEncoding(S);
              end;
              FileStream.Seek(0, soFromBeginning);
              if PyEncoding <> '' then begin
                if PyEncoding = 'utf-8' then
                  Encoding := sf_UTF8_NoBOM;
                PyWstr := nil;
                try
                  with GetPythonEngine do begin
                    try
                        PyWstr := GetPythonEngine.PyUnicode_Decode(PChar(FileText), Length(FileText),
                          PChar(PyEncoding), 'replace');
                        CheckError;
                        Lines.Text := PyUnicode_AsWideString(PyWstr);
                    finally
                      Py_XDECREF(PyWstr);
                    end;
                  end;
                except
                  WideMessageDlg(WideFormat(_(SDecodingError),
                     [AFileName, PyEncoding]), mtWarning, [mbOK], 0);
                  Lines.Text := FileText;
                end;
              end else
                Lines.LoadFromStream(FileStream);
            end else
              Lines.LoadFromStream(FileStream);
          sf_UTF8, sf_UTF8_NoBOM :
            LoadFromStream(Lines, FileStream, seUTF8, HasBOM);
          sf_UTF16LE:
            LoadFromStream(Lines, FileStream, seUTF16LE, HasBOM);
          sf_UTF16BE:
            LoadFromStream(Lines, FileStream, seUTF16BE, HasBOM);
        end;
      finally
        FileStream.Free;
      end;
    except
      on E: Exception do begin
        WideMessageDlg(WideFormat(_(SFileOpenError), [AFileName, E.Message]), mtError, [mbOK], 0);
        Result := False;
      end;
    end;
  end else
    Result := False;
end;

(* Save WideStrings to file taking into account Python file encodings *)
function SaveWideStringsToFile(const AFileName: WideString;
  Lines : TUnicodeStrings; Encoding : TFileSaveFormat;
  DoBackup : Boolean = True) : boolean;
Var
  FileStream : TWideFileStream;
  S : string;
begin
  try
    // Create Backup
    if DoBackup and
      WideFileExists(AFileName) then
    begin
      try
        FileBackup(AFileName);
      except
        WideMessageDlg(WideFormat(_(SFailedToBackupFile), [AFileName]),
          mtWarning, [mbOK], 0);
      end;
    end;

    Result := True;

    if Encoding = sf_Ansi then
      Result := WideStringsToEncodedText(AFileName, Lines, Encoding, S, True);

    if Result then begin
      FileStream := TWideFileStream.Create(AFileName, fmCreate);
      try
        case Encoding of
          sf_Ansi : FileStream.WriteBuffer(S[1], Length(S));
          sf_UTF8 : SaveToStream(Lines, FileStream, seUTF8, True);
          sf_UTF8_NoBOM : SaveToStream(Lines, FileStream, seUTF8, False);
          sf_UTF16LE: SaveToStream(Lines, FileStream, seUTF16LE, True);
          sf_UTF16BE: SaveToStream(Lines, FileStream, seUTF16BE, True);
        end;
      finally
        FileStream.Free;
      end;

    end;
  except
    on E: Exception do begin
      WideMessageDlg(WideFormat(_(SFileSaveError), [AFileName, E.Message]), mtError, [mbOK], 0);
      Result := False;
    end;
  end;
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

function FileToEncodedStr(const AFileName : WideString) : string;
Var
  SL : TUnicodeStrings;
  Encoding: TFileSaveFormat;
begin
  SL := TUnicodeStringList.Create;
  try
    LoadFileIntoWideStrings(AFileName, SL, Encoding);
    WideStringsToEncodedText(AFileName, SL, Encoding, Result, False);
  finally
    SL.Free;
  end;
end;


function FileToWideStr(const AFileName : WideString) : WideString;
Var
  SL : TUnicodeStrings;
  Encoding: TFileSaveFormat;
begin
  SL := TUnicodeStringList.Create;
  try
    LoadFileIntoWideStrings(AFileName, SL, Encoding);
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function FileTimeLastWriteRaw(AFileName: WideString; var Time : TFileTime) : Boolean;
Var
  FindData: TWIN32FindDataW;
  Handle: THandle;
begin
  Result := False;
  Time := ZeroFileTime;
  Handle := Tnt_FindFirstFileW(PWideChar(AFileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      Result := True;
      Time := FindData.ftLastWriteTime;
    end;
  end;
end;


// Support class for BuildFileList
type
TBuildFileList = class
  private
    fFileList : TWideStrings;
    procedure VirtualFileSearchEnd(Sender: TObject;  Results: TCommonPIDLList);
    procedure BuildFileList(const Path, Masks: WideString;
      FileList: TWideStrings; Recursive: Boolean; SearchAttribs,
      SearchExcludeAttribs: TVirtualSearchAttribs);
end;

{ TBuildFileList }

procedure TBuildFileList.BuildFileList(const Path, Masks: WideString;
  FileList: TWideStrings; Recursive: Boolean; SearchAttribs,
  SearchExcludeAttribs: TVirtualSearchAttribs);
var
  FileSearch : TVirtualFileSearch;
begin
  fFileList := FileList;
  FileSearch := TVirtualFileSearch.Create(nil);
  try
    FileSearch.SearchAttribs := SearchAttribs;
    FileSearch.SearchExcludeAttribs := SearchExcludeAttribs;

    FileSearch.SearchPaths.StrictDelimiter := True;
    FileSearch.SearchPaths.Delimiter := ';';
    FileSearch.SearchPaths.DelimitedText := Path;

    FileSearch.SearchCriteriaFilename.StrictDelimiter := True;
    FileSearch.SearchCriteriaFilename.Delimiter := ';';
    FileSearch.SearchCriteriaFilename.DelimitedText := Masks;

    FileSearch.SubFolders := Recursive;
    FileSearch.UpdateRate := 100;
    FileSearch.OnSearchEnd := VirtualFileSearchEnd;
    FileSearch.RunAndWait;
  finally
    FileSearch.Free;
  end;
end;

procedure TBuildFileList.VirtualFileSearchEnd(Sender: TObject;
  Results: TCommonPIDLList);
Var
  i : integer;
begin
  for i := 0 to Results.Count - 1 do
    fFileList.Add(PIDLToPath(Results[i]));
  Results.Clear;
end;

procedure BuildFileList(const Path, Masks: WideString;
  FileList: TWideStrings; Recursive: Boolean; SearchAttribs,
  SearchExcludeAttribs: TVirtualSearchAttribs);
Var
  BFL : TBuildFileList;
begin
  BFL := TBuildFileList.Create;
  try
    BFL.BuildFileList(Path, Masks, FileList, Recursive, SearchAttribs,
      SearchExcludeAttribs);
  finally
    BFL.Free;
  end;
end;

function WideCharPos(const S: WideString; const C: WideChar; const Index: Integer): Integer;
begin
  if (Index > 0) and (Index <= Length(S)) then
  begin
    for Result := Index to Length(S) do
      if S[Result] = C then
        Exit;
  end;
  Result := 0;
end;

function WideCharIsNumberChar(const C: WideChar): Boolean;
begin
  Result := IsWideCharDigit(C) or InOpSet(C, ['-', '+', DecimalSeparator]);
end;

function WideStrConsistsofNumberChars(const S: WideString): Boolean;
var
  I: Integer;
begin
  Result := S <> '';
  for I := 1 to Length(S) do
  begin
    if not WideCharIsNumberChar(S[I]) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function WideStrTrimCharsLeft(const S: WideString; const Chars: TSysCharSet): WideString;
var
  I, L: Integer;
begin
  I := 1;
  L := Length(S);
  while (I <= L) and InOpSet(S[I], Chars) do Inc(I);
  Result := Copy(S, I, L - I + 1);
end;

function WideStrTrimCharsRight(const S: Widestring; const Chars: TSysCharSet): WideString;
var
  I: Integer;
begin
  I := Length(S);
  while (I >= 1) and InOpSet(S[I], Chars) do Dec(I);
  Result := Copy(S, 1, I);
end;

function WideStrToken(var S: WideString; Separator: WideChar): WideString;
var
  I: Integer;
begin
  I := WideCharPos(S, Separator);
  if I <> 0 then
  begin
    Result := Copy(S, 1, I - 1);
    Delete(S, 1, I);
  end
  else
  begin
    Result := S;
    S := '';
  end;
end;

function GetClipboardWideText : WideString;
begin
  Result := GetClipboardText;
end;

procedure SetClipboardWideText(AText : WideString);
begin
  SetClipboardText(AText);
end;

function ClipboardProvidesWideText : Boolean;
begin
  Result := ClipboardProvidesText;
end;

function WidePosEx(const SubStr, S: WideString; Offset: Integer = 1): Integer;
var
  I,X: Integer;
  Len, LenSubStr: Integer;
begin
  if Offset = 1 then
    Result := Pos(SubStr, S)
  else
  begin
    if Offset < 0 then
    begin
      Result := 0;
      exit;
    end;
    I := Offset;
    LenSubStr := Length(SubStr);
    Len := Length(S) - LenSubStr + 1;
    while I <= Len do
    begin
      if S[I] = SubStr[1] then
      begin
        X := 1;
        while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then
        begin
          Result := I;
          exit;
        end;
      end;
      Inc(I);
    end;
    Result := 0;
  end;
end;

function WideCharLastPos(const S: Widestring; const C: WideChar; const Index: Integer): Integer;
begin
  if (Index > 0) and (Index <= Length(S)) then
  begin
    for Result := Length(S) downto Index do
      if S[Result] = C then
        Exit;
  end;
  Result := 0;
end;

function WideStrReplaceChars(const S: WideString; const Chars: TSysCharSet; Replace: WideChar): WideString;
var
  I: Integer;
begin
  Result := S;
  for I := 1 to Length(S) do
    if InOpSet(Result[I], Chars) then
      Result[I] := Replace;
end;

function WideStrRemoveChars(const S: WideString; const Chars: TSysCharSet): WideString;
var
  Source, Dest: PWideChar;
  Len, Index: Integer;
begin
  Len := Length(S);
  SetLength(Result, Len);
  Source := PWideChar(S);
  Dest := PWideChar(Result);
  for Index := 0 to Len-1 do
  begin
    if not InOpSet(Source^, Chars) then
    begin
      Dest^ := Source^;
      Inc(Dest);
    end;
    Inc(Source);
  end;
  SetLength(Result, Dest - PWideChar(Result));
end;

function WideLastPos(const Substr, Str: WideString; IgnoreCase: Boolean = False): Integer;
var PSubstr, PStr, PStart: PWideChar;
    Str2: WideString;
begin
  Result := 0;
  if IgnoreCase then begin
    Str2 := WideLowerCase(Str);
    PStr := PWideChar(Str2);
    end
  else
    PStr := PWideChar(Str);
  PStart := PStr;
  PSubStr := PWideChar(Substr);
  repeat
    PStr := WStrPos(PStr, PSubstr);
    if PStr=nil then
      exit;
    Result := PStr-PStart+1;
    inc(PStr);
  until PStr^ = WideChar(#0);
end;

procedure MRUToWideStrings(MRU : TSpTBXMRUListItem; SL : TWideStrings);
var
  I: Integer;
begin
  SL.Clear;
  for I := 0 to MRU.Count - 1 do
    if MRU.Items[I] is TSpTBXMRUItem then
      SL.Add(TSpTBXMRUItem(MRU.Items[I]).MRUString);
end;

procedure WideStringsToMRU(MRU : TSpTBXMRUListItem; SL : TWideStrings);
var
  I: Integer;
begin
  MRU.Clear;
  for I := 0 to SL.Count - 1 do
      MRU.MRUAdd(SL[I]);
end;

function GetHotColor(OptionEntry : TSpTBXSkinOptionEntry) : TColor;
begin
  with OptionEntry do
    case SkinType of
      0 : Result := Color1;
      1,2 : Result := Color2;
    else
      Result := Color4;
    end;
end;

procedure TrimIfNeeded(var S : WideString);
var
  I, L, Len: Integer;
begin
  Len := Length(S);
  L := Len;
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then
    S := ''
  else
  begin
    while S[L] <= ' ' do Dec(L);
    if (I > 1) or (L < Len) then
     S := Copy(S, I, L - I + 1);
  end;
end;

function CanActuallyFocus(WinControl: TWinControl): Boolean;
var
  Form: TCustomForm;
begin
  Result := False;
  if Assigned(WinControl) and not WinControl.Focused then begin
    Form := GetParentForm(WinControl);
    if Assigned(Form) and Form.Enabled and Form.Visible then
      Result := WinControl.CanFocus;
  end;
end;

end.

