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
  Windows, Classes, System.SysUtils, Graphics, SynEditTypes, SynUnicode,
  uEditAppIntfs,  SpTBXSkins, Controls, SynEdit, SynRegExpr, Vcl.ComCtrls;

const
  UTF8BOMString : RawByteString = AnsiChar($EF) + AnsiChar($BB) + AnsiChar($BF);
  IdentChars: TSysCharSet = ['_', '0'..'9', 'A'..'Z', 'a'..'z'];
  SFileExpr = '(([a-zA-Z]:)?[^\*\?="<>|:,;\+\^]+)'; // fwd slash (/) is allowed
  STracebackFilePosExpr =  '"\<?' + SFileExpr + '\>?", line (\d+)(, in ([\<\>\?\w]+))?';
  SWarningFilePosExpr = SFileExpr + ':(\d+):';
  WideLF = WideChar(#10);
  WideNull = WideChar(#0);
  AnsiLineFeed       = AnsiChar(#10);
  AnsiCarriageReturn = AnsiChar(#13);
  AnsiCrLf           = AnsiString(#13#10);
  WordBreakString = ',.;:"´`°^!?&$@§%#~[](){}<>-=+*/\| ';

(* returns the System ImageList index of the icon of a given file *)
function GetIconIndexFromFile(const AFileName: string;
  const ASmall: boolean): integer;

(* returns long file name even for nonexisting files *)
function GetLongFileName(const APath: string): string;

(* from cStrings *)
(* checks if AText starts with ALeft *)
function StrIsLeft(AText, ALeft: PWideChar): Boolean;

(* checks if AText ends with ARight *)
function StrIsRight(AText, ARight: PChar): Boolean;

(* returns next token - based on Classes.ExtractStrings *)
function StrGetToken(var Content: PChar;
                     Separators, WhiteSpace, QuoteChars: TSysCharSet): string;

(* removes quotes to AText, if needed *)
function StrUnQuote(const AText: string): string;

(* Lighten a given Color by a certain percentage *)
function LightenColor(Color:TColor; Percentage:integer):TColor;

(* Darken a given Color by a certain percentage *)
function DarkenColor(Color:TColor; Percentage:integer):TColor;

(* Get Exe File Version string *)
function ApplicationVersion : string;

(* Compares two Version strings and returns -1, 0, 1 depending on result *)
function  CompareVersion(const A, B : String) : Integer;

(* Checks whether we are connected to the Internet *)
function ConnectedToInternet : boolean;

(* Extracts the nth line from a string *)
function GetNthLine(const S : string; LineNo : integer) : string;

(* Extracts a range of lines from a string *)
function GetLineRange(const S : string; StartLine, EndLine : integer) : string;

(* Extracts a word from a string *)
function GetWordAtPos(const LineText : string; Start : Integer; WordChars : TSysCharSet;
  ScanBackwards : boolean = True; ScanForward : boolean = True;
  HandleBrackets : Boolean = False) : string;

(* Format a doc string by removing left space and blank lines at start and bottom *)
function FormatDocString(const DocString : string) : string;

(* Calculate the indentation level of a line *)
function CalcIndent(S : string; TabWidth : integer = 4): integer;

(* check if a directory is a Python Package *)
function DirIsPythonPackage(Dir : string): boolean;

(* check if a directory is a Python Package *)
function FileIsPythonPackage(FileName : string): boolean;

(* Get Python Package Root directory *)
function GetPackageRootDir(Dir : string): string;

(* Python FileName to possibly dotted ModuleName accounting for packages *)
function FileNameToModuleName(const FileName : string): string;

(* Convert <  > to &lt; &gt; *)
function HTMLSafe(const S : string): string;

(* Parses command line parameters *)
// From Delphi's system.pas unit! Need to rewrite
function GetParamStr(P: PChar; var Param: string): PChar;

(* ReadLn that works with Sreams *)
// Adapted from Indy
function ReadLnFromStream(Stream : TStream; AMaxLineLength: Integer = -1;
  AExceptionIfEOF: Boolean = FALSE): AnsiString;

(* Parse a line for a Python encoding spec *)
function ParsePySourceEncoding(Textline : string): string;

(* Version of InputQuery that can be called from threads and executes in the main thread *)
function SyncWideInputQuery(const ACaption, APrompt: string; var Value: string): Boolean;

(* Covert all line breaks to #10 *)
function CleanEOLs(S: AnsiString): AnsiString; overload;
function CleanEOLs(S: string): string; overload;

(* Similar to Delphi's IdentToInt but operating on sorted IdentMapEntries *)
function SortedIdentToInt(const Ident: string; var Int: Longint;
                          const SortedMap: array of TIdentMapEntry;
                          CaseSensitive : Boolean = False): Boolean;

(* Used for sorting Python Identifiers *)
function ComparePythonIdents(const S1, S2 : string): Integer; overload;
function ComparePythonIdents(List: TStringList; Index1, Index2: Integer): Integer; overload;

(* Used to get Vista fonts *)
procedure SetDefaultFonts(const AFont: TFont);
procedure SetDesktopIconFonts(const AFont: TFont);
procedure SetVistaContentFonts(const AFont: TFont);

(* Visual Studio replacement for SynEdits NextWord *)
function VSNextWordPos(SynEdit: TCustomSynEdit; const XY: TBufferCoord): TBufferCoord;

(* Visual Studio replacement for SynEdits PrevWord *)
function VSPrevWordPos(SynEdit: TCustomSynEdit; const XY: TBufferCoord): TBufferCoord;

(* Get the text between two Synedit Block coordinates *)
function GetBlockText(Strings : TStrings; BlockBegin, BlockEnd : TBufferCoord) : string;

(* Extract Error information from a VarPyth variant containing the Python error *)
procedure ExtractPyErrorInfo(E: Variant; var FileName: string; var LineNo: Integer; var Offset: Integer);

(* Get Encoded Ansi string from WideStrings ttaking into account Python file encodings *)
function WideStringsToEncodedText(const AFileName: string;
  Lines : TStrings; var EncodedText: AnsiString;
  InformationLossWarning: Boolean = False;
  IsPython: Boolean = False) : Boolean;

(* Load file into WideStrings taking into account Python file encodings *)
function LoadFileIntoWideStrings(const AFileName: string;
  Lines : TStrings; var Encoding : TFileSaveFormat): boolean;

(* Save WideStrings to file taking into account Python file encodings *)
function SaveWideStringsToFile(const AFileName: string;
  Lines : TStrings; Encoding : TFileSaveFormat;
  DoBackup : Boolean = True) : boolean;

(* Read File contents. Allows reading of locked files *)
function FileToAnsiStr(const FileName: String): AnsiString;

(* Read File contents into encoded string. Takes into account Python encodings *)
function FileToEncodedStr(const AFileName : string) : AnsiString;

(* Read File contents into Widestring. Takes into account Python encodings *)
function FileToStr(const AFileName : string) : string;

type
  TDirectoryWalkProc = reference to function (const Path: string;
      const FileInfo: TSearchRec): Boolean;

(*
   Directory traversal function.  Paths and Masks are semi-colon delimited lists.
*)
procedure WalkThroughDirectories(const Paths, Masks: string;
  const PreCallback: TDirectoryWalkProc;
  const Recursive: Boolean);

(*
   Find files and place them in FileList. Paths and Masks are semi-colon delimited lists.
*)
procedure GetFilesInPaths(Paths, Masks : string; FileList: TStrings; Recursive : Boolean = True);

(*
   Find directories and place them in DirList. Paths and Masks are semi-colon delimited lists.
*)
procedure GetDirectoriesInPaths(Paths, Masks : string; DirList: TStrings; Recursive : Boolean = True);

(* Check whether is S is likely to be a number *)
//function WideStrConsistsofNumberChars(const S: WideString): Boolean;

(* Trim certain chars from left of string *)
function StrTrimCharsLeft(const S: string; const Chars: TSysCharSet): string;

(* Trim certain chars from right of string *)
function StrTrimCharsRight(const S: string; const Chars: TSysCharSet): string;

(* Extracts a token and returns the remainder of a string *)
function StrToken(var S: String; Separator: Char): string;

(* Gets the Clipboard contents as Unicode string *)
function GetClipboardWideText : string;

(* Sets the Clipboard contents as Unicode string *)
procedure SetClipboardWideText(AText : string);

(* Check whether the Clipboard can provide Unicode string *)
function ClipboardProvidesWideText : Boolean;

(* Get Hot Color from SpTBX Skin option entry *)
function GetHotColor(OptionEntry : TSpTBXSkinOptionEntry) : TColor;

(* Improved CanFocus *)
function CanActuallyFocus(WinControl: TWinControl): Boolean;

(* Create a Regular Expression and compile it *)
function CompiledRegExpr(Expr : string): TRegExpr;

(* Checks whether S contains digits only *)
function IsDigits(S : string): Boolean;

(* Remove the white space in front of the first line from all lines *)
function Dedent (const S : string) : string;

(* Returns true for dark colors *)
function IsColorDark(AColor : TColor) : boolean;

(* Returns true if the styled clWindows system oolor is dark *)
function IsStyledWindowsColorDark : boolean;

(* Adds formated text to a Richedit control *)
procedure AddFormatText(RE : TRichEdit; const S: string;  FontStyle: TFontStyles = [];
 const FontColor: TColor = clDefault; FontSize: Integer = 0);

(* Scales the images of an ImageList *)
procedure ScaleImageList(const ImgList: TImageList; M, D: Integer);

(* Resize Bitmap *)
procedure ResizeBitmap(Bitmap: TBitmap; const NewWidth, NewHeight: integer);

(* Scale a value according to the Screen.PixelperInch *)
function PPIScaled(I : Integer): Integer;

(* Reverse PPI Scaling  *)
function PPIUnScaled(I : Integer): Integer;

(* Returns string with Desktop size *)
function DesktopSizeString: string;

(* Downlads a file from the Interent *)
function DownloadUrlToFile(const URL, Filename: string): Boolean;

Const
  IdentRE = '[A-Za-z_][A-Za-z0-9_]*';
  DottedIdentRE = '[A-Za-z_][A-Za-z0-9_.]*';


Type
  {  TStringlist that preserves the LineBreak and BOM of a read File }
  TLineBreakStringList = class(TStringList)
  protected
    procedure SetTextStr(const Value: string); override;
  public
    procedure LoadFromStream(Stream: TStream); override;
  end;

implementation
Uses
  Types, Forms, JclFileUtils, Math, VarPyth, JclBase,
  StrUtils, PythonEngine, dmCommands, Dialogs,
  StringResources, frmPythonII, JvGnugettext, MPCommonUtilities,
  MPCommonObjects, MPShellUtilities, IOUtils, Vcl.Themes, System.AnsiStrings,
  System.UITypes, Winapi.CommCtrl, JclStrings, SynEditMiscClasses,
  cPyScripterSettings,   Winapi.UrlMon,
  SynEditTextBuffer;

function GetIconIndexFromFile(const AFileName: string;
  const ASmall: boolean): integer;
Var
  NameSpace : TNameSpace;
  IconSize : TIconSize;
begin
  Result:= -1;
  // swallow any exceptions (bug report by Colin Williams)
  try
    if FileExists(AFileName) then begin
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
  while CharInSet(Tail^, WhiteSpace) do Inc(Tail);
  Head := Tail;
  while True do begin
    while (InQuote and not CharInSet(Tail^, QuoteChars + [#0])) or
      not CharInSet(Tail^, Separators + WhiteSpace + QuoteChars + [#0]) do Inc(Tail);
    if CharInSet(Tail^, QuoteChars) then begin
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
  if CharInSet(PChar(AText)^, ['"', '''']) then begin
    PText:= PChar(AText);
    Result:= AnsiExtractQuotedStr(PText, PText^);
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

function  CompareVersion(const A, B : String) : Integer;
var
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

function GetWordAtPos(const LineText : string; Start : Integer; WordChars : TSysCharSet;
  ScanBackwards : boolean = True; ScanForward : boolean = True;
  HandleBrackets : Boolean = False) : string;
Var
  i, j : integer;
  L, WordStart, WordEnd, ParenCounter, NewStart : integer;
  Bracket, MatchingBracket : WideChar;
Const
  AllBrackets: array[0..5] of WideChar = ('(', ')', '[', ']','{', '}');
  CloseBrackets = [')', ']', '}'];
  OpenBrackets = ['(', '[', '{'];
begin
  L := Length(LineText);
  WordStart := Start;
  WordEnd := Start;
  if (Start <= 0) or (Start > L) or not CharInSet(LineText[Start], WordChars) then
    Result := ''
  else begin
    if ScanBackwards then begin
      i := Start;
      while (i > 1) and CharInSet(LineText[i-1], WordChars) do
        Dec(i);
      WordStart := i;
    end;
    if ScanForward then begin
      i := Start;
      while (i < L) and CharInSet(LineText[i+1], WordChars) do
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

    if (NewStart > 0) and CharInSet(LineText[NewStart], CloseBrackets) then begin
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
    if S[i] = WideChar(#9) then
      Inc(Result, TabWidth)
    else if S[i] = ' ' then
      Inc(Result)
    else
      break;
end;

function DirIsPythonPackage(Dir : string): boolean;
begin
  Result := DirectoryExists(Dir) and
    FileExists(IncludeTrailingPathDelimiter(Dir) + '__init__.py');
end;

function FileIsPythonPackage(FileName : string): boolean;
begin
  Result := (ExtractFileExt(FileName) = '.py') and
    (ChangeFileExt(ExtractFileName(FileName), '') = '__init__');
end;

function GetPackageRootDir(Dir : string): string;
Var
  S : string;
begin
  if not DirIsPythonPackage(Dir) then
    raise Exception.CreateFmt('"%s" is not a Python package', [Dir]);
  S := Dir;
  Repeat
    Result := S;
    S := ExtractFileDir(S);
  Until (Result = S) or (not DirIsPythonPackage(S));
end;

function FileNameToModuleName(const FileName : string): string;
Var
  Path, Dir : string;
begin
  Result := ChangeFileExt(ExtractFileName(FileName), '');
  Path := ExtractFileDir(FileName);
  Dir := ExtractFileName(Path);

  if Path <> '' then begin
    while DirIsPythonPackage(Path) and (Dir <> '') do begin
      Result := Dir + '.' + Result;
      Path := ExtractFileDir(Path);
      Dir := ExtractFileName(Path);
    end;
    if StrIsRight(PChar(Result), '.__init__') then
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
     {if (P[0] = '"') and (P[1] = '"') then Inc(P, 2) else} Break;    // Issue 371
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
  AExceptionIfEOF: Boolean = FALSE): AnsiString;

  function FindEOL(ABuf: PAnsiChar; var VLineBufSize: Integer; var VCrEncountered: Boolean): Integer;
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
  LBuf: packed array [0..LBUFMAXSIZE] of AnsiChar;
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
      Move(LBuf[0], PAnsiChar(Result)[LResultLen], LStringLen);
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

type
  TSyncInputQuery = class
    public
    Caption, Prompt, Value : string;
    Res : Boolean;
    constructor Create(ACaption, APrompt, AValue : string);
    procedure InputQuery;
  end;

{ TSyncInputQuery }

constructor TSyncInputQuery.Create(ACaption, APrompt, AValue: string);
begin
  Caption := ACaption;
  Prompt := APrompt;
  Value := AValue;
end;

procedure TSyncInputQuery.InputQuery;
begin
  Res := Dialogs.InputQuery(Caption, Prompt, Value);
end;

function SyncWideInputQuery(const ACaption, APrompt: string; var Value: string): Boolean;
var
  SyncInputQuery : TSyncInputQuery;
begin
  if GetCurrentThreadId = MainThreadId then begin
    Result := InputQuery(ACaption, APrompt, Value);
  end else begin
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

function CleanEOLs(S: AnsiString): AnsiString;

  function AnsiAdjustLineBreaks(const S: AnsiString; Style: TTextLineBreakStyle): AnsiString;
  {From AnsiStrings units which forgot to export it}
  var
    Source, SourceEnd, Dest: PAnsiChar;
    DestLen: Integer;
    L: Integer;
  begin
    Source := Pointer(S);
    SourceEnd := Source + Length(S);
    DestLen := Length(S);
    while Source < SourceEnd do
    begin
      case Source^ of
        #10:
          if Style = tlbsCRLF then
            Inc(DestLen);
        #13:
          if Style = tlbsCRLF then
            if Source[1] = #10 then
              Inc(Source)
            else
              Inc(DestLen)
          else
            if Source[1] = #10 then
              Dec(DestLen);
      else
        if Source^ in LeadBytes then
        begin
          Source := System.AnsiStrings.StrNextChar(Source);
          continue;
        end;
      end;
      Inc(Source);
    end;
    if DestLen = Length(Source) then
      Result := S
    else
    begin
      Source := Pointer(S);
      SetString(Result, nil, DestLen);
      Dest := Pointer(Result);
      while Source < SourceEnd do
        case Source^ of
          #10:
            begin
              if Style = tlbsCRLF then
              begin
                Dest^ := #13;
                Inc(Dest);
              end;
              Dest^ := #10;
              Inc(Dest);
              Inc(Source);
            end;
          #13:
            begin
              if Style = tlbsCRLF then
              begin
                Dest^ := #13;
                Inc(Dest);
              end;
              Dest^ := #10;
              Inc(Dest);
              Inc(Source);
              if Source^ = #10 then Inc(Source);
            end;
        else
          if Source^ in LeadBytes then
          begin
            L := System.AnsiStrings.StrCharLength(Source);
            Move(Source^, Dest^, L);
            Inc(Dest, L);
            Inc(Source, L);
            continue;
          end;
          Dest^ := Source^;
          Inc(Dest);
          Inc(Source);
        end;
    end;
  end;

begin
  Result := AnsiAdjustLineBreaks(S, System.tlbsLF)
end;

function CleanEOLs(S: string): string;
begin
  Result := AdjustLineBreaks(S, System.tlbsLF)
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

function ComparePythonIdents(const S1, S2 : string): Integer; overload;
Var
  L1, L2 : integer;
begin
  L1 := Length(S1);
  L2 := Length(S2);
  if (L1 > 0) and (S1[1] = Char('_')) and (L2>0) and (S2[1] = Char('_')) then
    Result := CompareText(S1, S2)
  else if (L1 > 0) and (S1[1] = Char('_')) then
    Result := 1
  else if (L2>0) and (S2[1] = Char('_')) then
    Result := -1
  else
    Result := CompareText(S1, S2)
end;

function ComparePythonIdents(List: TStringList; Index1, Index2: Integer): Integer; overload;
Var
  S1, S2 : string;
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
  if CheckWin32Version(6)
    and not SameText(AFont.Name, VistaContentFont)
    and (Screen.Fonts.IndexOf(VistaContentFont) >= 0) then
  begin
    AFont.Size := AFont.Size + 1;
    AFont.Name := VistaContentFont;
  end;
end;

function GetBlockText(Strings : TStrings; BlockBegin, BlockEnd : TBufferCoord) : string;
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

function VSNextWordPos(SynEdit: TCustomSynEdit; const XY: TBufferCoord): TBufferCoord;
var
  CX, CY, LineLen: Integer;
  Line: UnicodeString;
begin
  CX := XY.Char;
  CY := XY.Line;

  // valid line?
  if (CY >= 1) and (CY <= SynEdit.Lines.Count) then
  with SynEdit do begin
    Line := Lines[CY - 1];

    LineLen := Length(Line);
    if CX > LineLen then
    begin
      // invalid char
      // find first char in the next line
      if CY < Lines.Count then
      begin
        Line := Lines[CY];
        LineLen := Length(Line);
        Inc(CY);
        CX := 1;
        while (CX <= LineLen) and IsWhiteChar(Line[CX]) do
          Inc(CX);
      end;
    end
    else
    begin
      if CX = 0 then
        CX := 1;
      // valid char
      if IsIdentChar(Line[CX]) then begin
        while (CX <= LineLen) and IsIdentChar(Line[CX]) do
          Inc(CX);
        while (CX <= LineLen) and IsWhiteChar(Line[CX]) do
          Inc(CX);
      end else if IsWhiteChar(Line[CX]) then begin
        while (CX <= LineLen) and IsWhiteChar(Line[CX]) do
          Inc(CX);
      end else begin
        // breakchar and not whitechar
        while (CX <= LineLen) and (IsWordBreakChar(Line[CX]) and not IsWhiteChar(Line[CX])) do
          Inc(CX);
        while (CX <= LineLen) and IsWhiteChar(Line[CX]) do
          Inc(CX);
      end;
    end;
  end;
  Result.Char := CX;
  Result.Line := CY;
end;

function VSPrevWordPos(SynEdit: TCustomSynEdit; const XY: TBufferCoord): TBufferCoord;
var
  CX, CY: Integer;
  Line: UnicodeString;
begin
  CX := XY.Char;
  CY := XY.Line;

  // valid line?
  if (CY >= 1) and (CY <= SynEdit.Lines.Count) then
  with SynEdit do begin
    Line := Lines[CY - 1];
    CX := Min(CX, Length(Line) + 1);

    if CX <= 1 then
    begin
      // find last IdentChar in the previous line
      if CY > 1 then
      begin
        Dec(CY);
        Line := Lines[CY - 1];
        CX := Length(Line) + 1;
        while (CX > 1) and IsWhiteChar(Line[CX-1]) do
          Dec(CX);
      end;
    end
    else
    begin
      // CX > 1 and <= LineLenght + 1
      if IsIdentChar(Line[CX-1]) then begin
        while (CX > 1) and IsIdentChar(Line[CX-1]) do
          Dec(CX);
      end else if IsWhiteChar(Line[CX-1]) then begin
        while (CX > 1) and IsWhiteChar(Line[CX-1]) do
          Dec(CX);
        if CX <= 1 then begin
          // find last IdentChar in the previous line
          if CY > 1 then
          begin
            Dec(CY);
            Line := Lines[CY - 1];
            CX := Length(Line) + 1;
            while (CX > 1) and IsWhiteChar(Line[CX-1]) do
              Dec(CX);
          end;
        end else if IsIdentChar(Line[CX-1]) then begin
          while (CX > 1) and IsIdentChar(Line[CX-1]) do
            Dec(CX);
        end else begin
          // breakchar and not whitechar
          while (CX > 1) and (IsWordBreakChar(Line[CX-1]) and not IsWhiteChar(Line[CX-1])) do
            Dec(CX);
        end;
      end else begin
        // breakchar and not whitechar
        while (CX > 1) and (IsWordBreakChar(Line[CX-1]) and not IsWhiteChar(Line[CX-1])) do
          Dec(CX);
      end;
    end;
  end;
  Result.Char := CX;
  Result.Line := CY;
end;

procedure ExtractPyErrorInfo(E: Variant; var FileName: string; var LineNo: Integer; var Offset: Integer);
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

function WideStringsToEncodedText(const AFileName: string;
  Lines : TStrings; var EncodedText: AnsiString;
  InformationLossWarning: Boolean = False;
  IsPython: Boolean = False) : Boolean;
// AFileName is passed just for the warning
var
  PyEncoding : string;
  UniPy, EncodedString : PPyObject;
  wStr: string;
  SuppressOutput : IInterface;
  OldLineBreak : string;
begin
  Result := True;

  //  Encoded strings cannot contain WideLineSeparator
  OldLineBreak := Lines.LineBreak;
  if Lines.LineBreak = WideLineSeparator then
    Lines.LineBreak := sLineBreak;

  wStr := Lines.Text;
  Lines.LineBreak := OldLineBreak;

  if IsPython or CommandsDataModule.FileIsPythonSource(AFileName) then begin
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
              EncodedString := PyUnicode_AsEncodedString(UniPy, PAnsiChar(AnsiString(PyEncoding)), 'strict');
              CheckError;
              EncodedText := PyString_AsAnsiString(EncodedString);
              CheckError;
            except
              on UnicodeEncodeError do begin
                Result :=
                  Dialogs.MessageDlg(Format(_(SFileEncodingWarning),
                    [AFileName, PyEncoding]), mtWarning, [mbYes, mbCancel], 0)= mrYes;
                if Result then begin
                  EncodedString := PyUnicode_AsEncodedString(UniPy,
                    PAnsiChar(AnsiString(PyEncoding)), PAnsiChar(AnsiString('replace')));
                  CheckError;
                  EncodedText := PyString_AsAnsiString(EncodedString);
                  CheckError;
                end;
              end;
            end;
          end else begin
            EncodedString := PyUnicode_AsEncodedString(UniPy, PAnsiChar(AnsiString(PyEncoding)), 'replace');
            CheckError;
            EncodedText := PyString_AsAnsiString(EncodedString);
            CheckError;
          end;
        finally
          Py_XDECREF(UniPy);
          Py_XDECREF(EncodedString);
        end;
      except
        PyErr_Clear;
        EncodedText := AnsiString(wStr);
        if InformationLossWarning then
          Result :=
            Dialogs.MessageDlg(Format(_(SFileEncodingWarning),
              [AFileName, PyEncoding]), mtWarning, [mbYes, mbCancel], 0)= mrYes ;
      end;
    end;
  end else begin
    EncodedText := AnsiString(wStr);
    if InformationLossWarning and not IsAnsiOnly(wStr) then begin
      Result :=
        Dialogs.MessageDlg(Format(_(SFileEncodingWarning),
        [AFileName, 'ANSI']), mtWarning, [mbYes, mbCancel], 0)= mrYes ;
    end;
  end;
end;

function LoadFileIntoWideStrings(const AFileName: string;
  Lines : TStrings; var Encoding : TFileSaveFormat): boolean;
Var
  FileStream : TFileStream;
  FileText, S, PyEncoding : AnsiString;
  Len : integer;
  IsPythonFile : boolean;
  FileEncoding : TSynEncoding;
  HasBOM : Boolean;
  PyWstr : PPyObject;
begin
  Result := True;
  if (AFileName <> '') and FileExists(AFileName) then begin
    IsPythonFile :=  CommandsDataModule.FileIsPythonSource(AFileName);
    try
      FileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
      try
        // Read the file into FileText
        Len := FileStream.Size;
        SetLength(FileText, Len);
        FileStream.ReadBuffer(FileText[1], Len);
        FileStream.Seek(0, soFromBeginning);

        // This routine detects UTF8 text even if there is no BOM
        FileEncoding := GetEncoding(FileStream, HasBOM);
        case FileEncoding of
          seAnsi :
            // if it is a Pytyhon file detect an encoding spec
            if IsPythonFile then
            begin
              PyEncoding := '';
              S := ReadLnFromStream(FileStream);
              PyEncoding := AnsiString(ParsePySourceEncoding(string(S)));
              if PyEncoding = '' then begin
                S := ReadLnFromStream(FileStream);
                PyEncoding := AnsiString(ParsePySourceEncoding(string(S)));
              end;
              FileStream.Seek(0, soFromBeginning);
              if PyEncoding <> '' then
              begin
                if (LowerCase(PyEncoding) = 'utf-8') or (LowerCase(PyEncoding) = 'utf8')
                then
                  Encoding := sf_UTF8_NoBOM
                else
                  Encoding := sf_Ansi;
              end
              else
              begin
                if GetPythonEngine.IsPython3000 then
                  Encoding := sf_UTF8_NoBOM
                else
                  Encoding := sf_Ansi;
              end;
            end else
              Encoding := sf_Ansi;

          seUTF8 :
            begin
              if not HasBOM then begin
//                PEP8 states that in Python 3 the default encoding is UTF8 without BOM
//                if IsPythonFile then
//                  // Ignore detected UTF8 if it is Python and does not have BOM
//                  // File will still be read as UTF8 if it has an encoding comment
//                  Encoding := sf_Ansi
//                else begin
                  if PyIDEOptions.DetectUTF8Encoding then
                    Encoding := sf_UTF8_NoBOM
                  else
                    Encoding := sf_Ansi;
//                end;
              end else
                Encoding := sf_UTF8;
            end;
          seUTF16LE : Encoding := sf_UTF16LE;
          seUTF16BE : Encoding := sf_UTF16BE;
        else
          Raise Exception.Create(Format(_(SInternalError), ['LoadFileIntoWideStrings']));
        end;

        case Encoding of
          sf_Ansi :
            // if it is a Pytyhon file detect an encoding spec
            if IsPythonFile and (PyEncoding <> '') then
            begin
              PyWstr := nil;
              try
                with GetPythonEngine do begin
                  try
                      PyWstr := GetPythonEngine.PyUnicode_Decode(PAnsiChar(FileText),
                        Length(FileText),
                        PAnsiChar(PyEncoding), 'replace');
                      CheckError;
                      Lines.Text := PyUnicode_AsWideString(PyWstr);
                  finally
                    Py_XDECREF(PyWstr);
                  end;
                end;
              except
                Dialogs.MessageDlg(Format(_(SDecodingError),
                   [AFileName, PyEncoding]), mtWarning, [mbOK], 0);
                Lines.Text := string(FileText);
              end;
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
        Dialogs.MessageDlg(Format(_(SFileOpenError), [AFileName, E.Message]), mtError, [mbOK], 0);
        Result := False;
      end;
    end;
  end else
    Result := False;
end;

type
  TSynEditStringListAccess = class(TSynEditStringList);

(* Save WideStrings to file taking into account Python file encodings *)
function SaveWideStringsToFile(const AFileName: string;
  Lines : TStrings; Encoding : TFileSaveFormat;
  DoBackup : Boolean = True) : boolean;
Var
  FileStream : TFileStream;
  S : AnsiString;
  SaveFStreaming: Boolean;
begin
  try
    // Create Backup
    if DoBackup and
      FileExists(AFileName) then
    begin
      try
        FileBackup(AFileName);
      except
        Dialogs.MessageDlg(Format(_(SFailedToBackupFile), [AFileName]),
          mtWarning, [mbOK], 0);
      end;
    end;

    Result := True;

    if Encoding = sf_Ansi then begin
      if Lines is TSynEditStringList then
      begin
        SaveFStreaming := TSynEditStringListAccess(Lines).FStreaming;
        TSynEditStringListAccess(Lines).FStreaming := True;
      end;

      Result := WideStringsToEncodedText(AFileName, Lines, S, True);

      if Lines is TSynEditStringList then
        TSynEditStringListAccess(Lines).FStreaming := SaveFStreaming;
    end;

    if Result then begin
      FileStream := TFileStream.Create(AFileName, fmCreate);
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
      Dialogs.MessageDlg(Format(_(SFileSaveError), [AFileName, E.Message]), mtError, [mbOK], 0);
      Result := False;
    end;
  end;
end;

function FileToAnsiStr(const FileName: String): AnsiString;
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

function FileToEncodedStr(const AFileName : string) : AnsiString;
Var
  SL : TStrings;
  Encoding: TFileSaveFormat;
begin
  SL := TStringList.Create;
  try
    LoadFileIntoWideStrings(AFileName, SL, Encoding);
    WideStringsToEncodedText(AFileName, SL, Result, False);
  finally
    SL.Free;
  end;
end;


function FileToStr(const AFileName : string) : string;
Var
  SL : TStrings;
  Encoding : TFileSaveFormat;
begin
  SL := TStringList.Create;
  try
    LoadFileIntoWideStrings(AFileName, SL, Encoding);
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

(*
   Single directory traversal function used in WalkThroughDirectories
*)
procedure WalkThroughDirectory(const Path: string; Masks : TStringDynArray;
  const PreCallback: TDirectoryWalkProc;
  const Recursive: Boolean);
var
  SearchRec: TSearchRec;
  Match: Boolean;
  Stop: Boolean;
  PathWithSep, Mask : string;
begin
  PathWithSep := IncludeTrailingPathDelimiter(Path);
  if FindFirst(PathWithSep + '*', faAnyFile, SearchRec) = 0 then
  try
    Stop := False;

    repeat
      Match := Length(Masks) = 0;
      for Mask in Masks do begin
        Match := TPath.MatchesPattern(SearchRec.Name, Mask, False);
        if Match then Break;
      end;

      // call the preorder callback method
      if Match and Assigned(PreCallback) then
        Stop := not PreCallback(PathWithSep, SearchRec);

      if not Stop then
      begin
        // go recursive in subdirectories
        if Recursive and (SearchRec.Attr and System.SysUtils.faDirectory <> 0) and
           (SearchRec.Name <> '.') and
           (SearchRec.Name <> '..') then
          WalkThroughDirectory(PathWithSep + SearchRec.Name,
            Masks, PreCallback, Recursive);

      end;
    until Stop or (FindNext(SearchRec) <> 0);
  finally
    FindClose(SearchRec);
  end;
end;

procedure WalkThroughDirectories(const Paths, Masks: string;
  const PreCallback: TDirectoryWalkProc;
  const Recursive: Boolean);
Var
  Path : string;
  PathList, MaskList : TStringDynArray;
begin
  PathList := SplitString(Paths, ';');
  MaskList := SplitString(Masks, ';');
  for Path in PathList do
    WalkThroughDirectory(Path, MaskList, PreCallback, Recursive);
end;

procedure GetFilesInPaths(Paths, Masks : string; FileList: TStrings; Recursive : Boolean = True);
Var
  PreCallback: TDirectoryWalkProc;
begin
  PreCallback :=
    function (const Path: string; const FileInfo: TSearchRec): Boolean
    begin
      Result := True;

      if FileInfo.Attr and System.SysUtils.faDirectory = 0 then
        FileList.Add(Path+FileInfo.Name);
    end;

  WalkThroughDirectories(Paths, Masks, PreCallback, Recursive);
end;

procedure GetDirectoriesInPaths(Paths, Masks : string; DirList: TStrings; Recursive : Boolean = True);
Var
  PreCallback: TDirectoryWalkProc;
begin
  PreCallback :=
    function (const Path: string; const FileInfo: TSearchRec): Boolean
    begin
      Result := True;

      if (FileInfo.Attr and System.SysUtils.faDirectory <> 0) and
         (FileInfo.Name <> '.') and (FileInfo.Name <> '..') then
        DirList.Add(Path+FileInfo.Name);
    end;

  WalkThroughDirectories(Paths, Masks, PreCallback, Recursive);
end;

function StrTrimCharsLeft(const S: string; const Chars: TSysCharSet): string;
var
  I, L: Integer;
begin
  I := 1;
  L := Length(S);
  while (I <= L) and CharInSet(S[I], Chars) do Inc(I);
  Result := Copy(S, I, L - I + 1);
end;

function StrTrimCharsRight(const S: string; const Chars: TSysCharSet): string;
var
  I: Integer;
begin
  I := Length(S);
  while (I >= 1) and CharInSet(S[I], Chars) do Dec(I);
  Result := Copy(S, 1, I);
end;

function StrToken(var S: string; Separator: WideChar): string;
var
  I: Integer;
begin
  I := CharPos(S, Separator);
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

function GetClipboardWideText : string;
begin
  Result := GetClipboardText;
end;

procedure SetClipboardWideText(AText : string);
begin
  SetClipboardText(AText);
end;

function ClipboardProvidesWideText : Boolean;
begin
  Result := ClipboardProvidesText;
end;

//function WideStrReplaceChars(const S: WideString; const Chars: TSysCharSet; Replace: WideChar): WideString;
//var
//  I: Integer;
//begin
//  Result := S;
//  for I := 1 to Length(S) do
//    if CharInSet(Result[I], Chars) then
//      Result[I] := Replace;
//end;

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

function IsDigits(S : string): Boolean;
Var
  i : integer;
begin
  Result := True;
  for I := 1 to Length(S) do
    if not CharInSet(S[I], ['0'..'9']) then begin
      Result := False;
      break;
    end;
end;

function Dedent (const S : string) : string;
Var
  LeadWhiteSpace: string;
  RegExpr: TRegExpr;
begin
  RegExpr := TRegExpr.Create;
  try
    RegExpr.ModifierM := False;
    RegExpr.Expression := '^\s*';
    if RegExpr.Exec(S) then
    begin
      LeadWhiteSpace := RegExpr.Match[0];
      RegExpr.ModifierM := True;
      RegExpr.Expression := '^' + LeadWhiteSpace;
      Result := RegExpr.Replace(S, '');
    end else
      Result := S;
  finally
    RegExpr.Free;
  end;
end;

function CompiledRegExpr(Expr : string): TRegExpr;
begin
  Result := TRegExpr.Create;
  Result.Expression := Expr;
  Result.Compile;
end;

function IsColorDark(AColor : TColor) : boolean;
var
  ACol: Longint;
begin
  ACol := ColorToRGB(AColor) and $00FFFFFF;
  Result := ((2.99 * GetRValue(ACol) + 5.87 * GetGValue(ACol) +
                 1.14 * GetBValue(ACol)) < $400);
end;

function IsStyledWindowsColorDark : boolean;
begin
  Result := IsColorDark(StyleServices.GetSystemColor(clWindow));
end;

{ TLineBreakeStingList }

procedure TLineBreakStringList.LoadFromStream(Stream: TStream);
Var
  HasBOM : Boolean;
begin
  if IsUTF8(Stream, HasBOM) then begin
    WriteBOM := HasBOM;
    LoadFromStream(Stream, Encoding.UTF8);
  end
  else
    inherited;
end;

procedure TLineBreakStringList.SetTextStr(const Value: string);
var
  S: string;
  Size: Integer;
  P, Start, Pmax: PWideChar;
  fCR, fLF, fWideLineSeparater: Boolean;
begin
  fWideLineSeparater := False;
  fCR := False;
  fLF := False;
  BeginUpdate;
  try
    Clear;
    P := Pointer(Value);
    if P <> nil then
    begin
      Size := Length(Value);
      Pmax := @Value[Size];
      while (P <= Pmax) do
      begin
        Start := P;
        while (P^ <> WideCR) and (P^ <> WideLF) and (P^ <> WideLineSeparator) and (P <= Pmax) do
        begin
          Inc(P);
        end;
        if P<>Start then
        begin
          SetString(S, Start, P - Start);
          InsertItem(Count, S, nil);
        end else InsertItem(Count, '', nil);
        if P^ = WideLineSeparator then
        begin
          fWideLineSeparater := True;
          Inc(P);
        end;
        if P^ = WideCR then
        begin
          fCR := True;
          Inc(P);
        end;
        if P^ = WideLF then
        begin
          fLF := True;
          Inc(P);
        end;
      end;
      // keep the old format of the file
      if (CharInSet(Value[Size], [#10, #13]) or (Value[Size] = WideLineSeparator))
      then
        InsertItem(Count, '', nil);
    end;
  finally
    EndUpdate;
  end;
  if fWideLineSeparater then
    LineBreak := WideLineSeparator
  else if fCR and not fLF then
    LineBreak := WideCR
  else if fLF and not fCR then
    LineBreak := WideLF
  else
    LineBreak := sLineBreak;
end;

procedure AddFormatText(RE : TRichEdit; const S: string;  FontStyle: TFontStyles = [];
 const FontColor: TColor = clDefault; FontSize: Integer = 0);
begin
  with RE do
  begin
    //move caret to end
    SelStart := GetTextLen;
    SelAttributes.Style := FontStyle;
    if FontSize <> 0 then
      SelAttributes.Size := FontSize
    else
      SelAttributes.Size := DefAttributes.Size;
    if FontColor <> clDefault then
      SelAttributes.Color := FontColor
    else
      SelAttributes.Color := DefAttributes.Color;

    SelText := S;
  end;
end;

procedure ScaleImageList(const ImgList: TImageList; M, D: Integer);
var
  {ScaleFactor, }ii : integer;
  mb, ib, sib, smb : TBitmap;
  TmpImgList : TImageList;
begin
  if M = D then Exit;

//  ScaleFactor := M div D;
  //clear images
  TmpImgList := TImageList.Create(nil);
  try
    TmpImgList.Assign(ImgList);

    //set size to match DPI size (like 250% of 16px = 40px)
    ImgList.Clear;
    ImgList.SetSize(MulDiv(ImgList.Width, M, D), MulDiv(ImgList.Height, M, D));

    //add images back to original ImageList stretched (if DPI scaling > 150%) or centered (if DPI scaling <= 150%)
    for ii := 0 to -1 + TmpImgList.Count do
    begin
      ib := TBitmap.Create;
      mb := TBitmap.Create;
      try
        ib.Width := TmpImgList.Width;
        ib.Height := TmpImgList.Height;
        ib.Canvas.FillRect(ib.Canvas.ClipRect);

        mb.Width := TmpImgList.Width;
        mb.Height := TmpImgList.Height;
        mb.Canvas.FillRect(mb.Canvas.ClipRect);

        ImageList_DrawEx(TmpImgList.Handle, ii, ib.Canvas.Handle, 0, 0, ib.Width, ib.Height, CLR_NONE, CLR_NONE, ILD_NORMAL);
        ImageList_DrawEx(TmpImgList.Handle, ii, mb.Canvas.Handle, 0, 0, mb.Width, mb.Height, CLR_NONE, CLR_NONE, ILD_MASK);

        sib := TBitmap.Create; //stretched (or centered) image
        smb := TBitmap.Create; //stretched (or centered) mask
        try
          sib.Width := ImgList.Width;
          sib.Height := ImgList.Height;
          sib.Canvas.FillRect(sib.Canvas.ClipRect);
          smb.Width := ImgList.Width;
          smb.Height := ImgList.Height;
          smb.Canvas.FillRect(smb.Canvas.ClipRect);

          if M * 100 / D >= 150 then //stretch if >= 150%
          begin
            sib.Canvas.StretchDraw(Rect(0, 0, sib.Width, sib.Width), ib);
            smb.Canvas.StretchDraw(Rect(0, 0, smb.Width, smb.Width), mb);
          end
          else //center if < 150%
          begin
            sib.Canvas.Draw((sib.Width - ib.Width) DIV 2, (sib.Height - ib.Height) DIV 2, ib);
            smb.Canvas.Draw((smb.Width - mb.Width) DIV 2, (smb.Height - mb.Height) DIV 2, mb);
          end;
          ImgList.Add(sib, smb);
        finally
          sib.Free;
          smb.Free;
        end;
    finally
        ib.Free;
        mb.Free;
      end;
    end;
  finally
    TmpImgList.Free;
  end;
end;

procedure ResizeBitmap(Bitmap: TBitmap; const NewWidth, NewHeight: integer);
var
  buffer: TBitmap;
begin
  buffer := TBitmap.Create;
  try
    buffer.SetSize(NewWidth, NewHeight);
    buffer.Canvas.StretchDraw(Rect(0, 0, NewWidth, NewHeight), Bitmap);
    Bitmap.SetSize(NewWidth, NewHeight);
    Bitmap.Canvas.Draw(0, 0, buffer);
  finally
    buffer.Free;
  end;
end;

function PPIScaled(I : Integer): Integer;
begin
  Result := MulDiv(I, Screen.PixelsPerInch, 96);
end;

function PPIUnScaled(I : Integer): Integer;
begin
  Result := MulDiv(I, 96, Screen.PixelsPerInch);
end;

function DesktopSizeString: string;
begin
  Result := Format('(%dx%d)', [Screen.DesktopWidth, Screen.DesktopHeight]);
end;


function DownloadUrlToFile(const URL, Filename: string): Boolean;
begin
  Result := Succeeded(URLDownloadToFile(nil, PWideChar(URL), PWideChar(Filename), 0, nil));
end;

end.
