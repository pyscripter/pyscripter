{-----------------------------------------------------------------------------
 Unit Name: uCommonFunctions
 Author:    Kiriakos Vlahos
 Date:      23-Jun-2005
 Purpose:   Functions common to many units in PyScripter
 History:
-----------------------------------------------------------------------------}

unit uCommonFunctions;

interface
uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.Diagnostics,
  System.RegularExpressions,
  Vcl.Controls,
  Vcl.ComCtrls,
  Vcl.Graphics,
  Vcl.Dialogs,
  SynEditTypes,
  SynEdit;

const
  SFileExpr = '((?:[a-zA-Z]:)?[^:*?"<>|]+)'; // fwd slash (/) is allowed
  STracebackFilePosExpr =  '"\<?' + SFileExpr + '\>?", line (\d+)(, in ([\<\>\?\w]+))?';
  SWarningFilePosExpr = '^<?' +SFileExpr + '>?:(\d+):';
  WideLF = #$000A;
  WideNull = #$0000;
  WordBreakString = ',.;:"´`°^!?&$@§%#~[](){}<>-=+*/\| ';

(* returns the System ImageList index of the icon of a given file *)
function GetIconIndexFromFile(const AFileName: string;
  const ASmall: Boolean): Integer;

(* returns long file name even for nonexisting files *)
function GetLongFileName(const APath: string): string;

(* from cStrings *)
(* checks if AText starts with ALeft *)
function StrIsLeft(AText, ALeft: PWideChar): Boolean;

(* returns next token - based on Classes.ExtractStrings *)
function StrGetToken(var Str: PChar; Separators, WhiteSpace,
  QuoteChars: TSysCharSet): string;

(* removes quotes to AText, if needed *)
function StrUnQuote(const AText: string): string;

(* Lighten a given Color by a certain percentage *)
function LightenColor(Color: TColor; Percentage: Integer): TColor;

(* Darken a given Color by a certain percentage *)
function DarkenColor(Color: TColor; Percentage: Integer):TColor;

(* Get Exe File Version string *)
function ApplicationVersion: string;

(* Checks whether we are connected to the Internet *)
function ConnectedToInternet: Boolean;

(* Extracts the nth line from a string *)
function GetNthLine(const Str: string; LineNo: Integer): string;

(* Extracts a range of lines from a string *)
function GetLineRange(const Str: string; StartLine, EndLine: Integer;
  HtmlBreaks: Boolean = False): string;

(* Extracts a word from a string *)
function GetWordAtPos(const LineText: string; Start: Integer;
  AllowDot: Boolean; ScanBackwards: Boolean = True; ScanForward: Boolean = True;
  HandleBrackets: Boolean = False): string;

(* Format a doc string by removing left space and blank lines at start and bottom *)
function FormatDocString(const DocString: string): string;

(* check if a directory is a Python Package *)
function DirIsPythonPackage(Dir: string): Boolean;

(* check if a directory is a Python Package *)
function FileIsPythonPackage(FileName: string): Boolean;

(* Get Python Package Root directory *)
function GetPackageRootDir(const Dir: string): string;

(* Python FileName to possibly dotted ModuleName accounting for packages *)
function FileNameToModuleName(const FileName: string): string;

(* Convert <  > to &lt; &gt; *)
function HTMLSafe(const Str: string): string;

(* Parses command line parameters *)
// From Delphi's system.pas unit! Need to rewrite
function GetParamStr(P: PChar; var Param: string): PChar;

(* Parse a line for a Python encoding spec *)
function ParsePySourceEncoding(TextLine: string): string;

(* Version of InputQuery that can be called from threads and executes in the main thread *)
function SyncWideInputQuery(const ACaption, APrompt: string; var Value: string): Boolean;

(* Covert all line breaks to #10 *)
function CleanEOLs(Str: AnsiString): AnsiString; overload;
function CleanEOLs(Str: string): string; overload;

(* Similar to Delphi's IdentToInt but operating on sorted IdentMapEntries *)
function SortedIdentToInt(const Ident: string; var Int: LongInt;
                          const SortedMap: array of TIdentMapEntry;
                          CaseSensitive: Boolean = False): Boolean;

(* Used for sorting Python Identifiers *)
function ComparePythonIdents(const Ident1, Ident2: string): Integer; overload;
function ComparePythonIdents(List: TStringList; Index1, Index2: Integer): Integer; overload;

(* Used to get Vista and code fonts *)
function DefaultCodeFontName: string;
procedure SetDefaultUIFont(const AFont: TFont);
procedure SetContentFont(const AFont: TFont);

(* Get the text between two Synedit Block coordinates *)
function GetBlockText(Strings: TStrings; BlockBegin, BlockEnd: TBufferCoord): string;

(* Get Encoded Ansi string from WideStrings ttaking into account Python file encodings *)
function WideStringsToEncodedText(const AFileName: string;
  Lines: TStrings; var EncodedText: AnsiString;
  InformationLossWarning: Boolean = False;
  IsPython: Boolean = False): Boolean;

(* Load file into WideStrings taking into account Python file encodings *)
function LoadFileIntoWideStrings(const AFileName: string;
  Lines: TStrings): Boolean;

(* Save WideStrings to file taking into account Python file encodings *)
function SaveWideStringsToFile(const AFileName: string;
  Lines: TStrings;  DoBackup: Boolean = True): Boolean;

(* Copy (download) remote file to a local temp file. Return the name of the temp file *)
function CopyRemoteFileToTemp(const AFileName, Server: string): string;

(* Read File contents into encoded string. Takes into account Python encodings *)
function FileToEncodedStr(const AFileName: string): AnsiString;

(* Read File contents into Widestring. Takes into account Python encodings *)
function FileToStr(const AFileName: string): string;

(* Get Nth Line of file *)
function GetNthSourceLine(const AFileName: string; LineNo: Integer): string;


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
procedure GetFilesInPaths(Paths, Masks: string; FileList: TStrings; Recursive: Boolean = True);

(*
   Find directories and place them in DirList. Paths and Masks are semi-colon delimited lists.
*)
procedure GetDirectoriesInPaths(Paths, Masks: string; DirList: TStrings; Recursive: Boolean = True);

(* Extracts a token and returns the remainder of a string *)
function StrToken(var Str: string; Separator: Char): string;

(* Strips a given character from a string *)
function StrStripChar(const Str: string; const AChar: Char): string;

(* Improved CanFocus *)
function CanActuallyFocus(WinControl: TWinControl): Boolean;

(* Create a PCRE Regular Expression and compile it *)
function CompiledRegEx(Expr: string; Options: TRegExOptions = [roNotEmpty];
  UCP: Boolean = True): TRegEx;

(* Checks whether S contains digits only *)
function IsDigits(Str: string): Boolean;

(* Remove the white space in front of the first line from all lines *)
function Dedent (const Str: string): string;

(* Returns true for dark colors *)
function IsColorDark(AColor: TColor): Boolean;

(* Returns true if the styled clWindows system oolor is dark *)
function IsStyledWindowsColorDark: Boolean;

(* Calculates styled border colors *)
procedure StyledBorderColors(out BorderNormal, BorderHighlight: TColor);

(* Adds formated text to a Richedit control *)
procedure AddFormatText(RE: TRichEdit; const Str: string;  FontStyle: TFontStyles = [];
 const FontColor: TColor = clDefault; FontSize: Integer = 0);

(* Returns string with Desktop size *)
function MonitorProfile: string;

(* Downlads a file from the Interent *)
function DownloadUrlToFile(const URL, Filename: string): Boolean;

(* Simple routine to hook/detour a function *)
procedure RedirectFunction(OrgProc, NewProc: Pointer);

{ Styled MessageDlg (do not use TaskDialog) }
function StyledMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: LongInt): Integer; overload;
function StyledMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: LongInt; DefaultButton: TMsgDlgBtn): Integer; overload;

{Style adjusted svg FixedColor}
function SvgFixedColor(Color: TColor): TColor;

{Frees Encoding if it is not standard}
procedure FreeAndNilEncoding(var Encoding: TEncoding);

{Create URI from FileName}
function FilePathToURI(FilePath: string): string;

{Extract FileName from URI}
function URIToFilePath(URI: string): string;

{RegisterApplicationRestart}
function RegisterApplicationRestart(Flags: DWORD = 0): Boolean;

{UnregisterApplicationRestart}
procedure UnregisterApplicationRestart;

{Checks whether a file downloaded from the Internet is blocked}
function IsFileBlocked(const FileName: string): Boolean;

{Unblock a file downloaded from the Internet}
procedure UnblockFile(const FileName: string);

(* Replaces <>"& with HTML entities *)
function HTMLEncode(const Str: string): string;

type
  TMatchHelper = record helper for TMatch
  public
    function GroupIndex(Index: Integer): Integer;
    function GroupLength(Index: Integer): Integer;
    function GroupValue(Index: Integer): string;
  end;

  (*  Helper method for forms *)
  TControlHelper = class helper for TControl
  public
    (* Scale a value according to the FCurrentPPI *)
    function PPIScale(ASize: Integer): Integer;
    (* Reverse PPI Scaling  *)
    function PPIUnScale(ASize: Integer): Integer;
  end;

  (*
     TSynStringList is a general purpose TStringList descendent that adds
     the following features:
     - LoadFromFile followed by SaveToFile results in an identical file
     - Detects the LineBreak in the read stream and uses it in SaveToStream
     - UseBOM is set when reading a stream depending on whether BOM exists
     - When reading a file without a BOM it tries to detect whether the e
       encoding is UTF8
     - Event handler for dealing with information loss in Unicode to ANSI
       conversion
  *)
  TXStringList = class(TStringList)
  private
    FUTF8CheckLen: Integer;
    FOnInfoLoss: TSynInfoLossEvent;
    FDetectUTF8: Boolean;
  public
    constructor Create; overload;
    procedure SetTextAndFileFormat(const Value: string);
    procedure LoadFromStream(Stream: TStream; Encoding: TEncoding); override;
    procedure SaveToStream(Stream: TStream; Encoding: TEncoding); override;
  published
    property UTF8CheckLen: Integer read FUTF8CheckLen write FUTF8CheckLen default -1;
    property DetectUTF8: Boolean read FDetectUTF8 write FDetectUTF8 default True;
    property OnInfoLoss: TSynInfoLossEvent read FOnInfoLoss write FOnInfoLoss;
  end;

  (*
    Interfaced based Timer that can be used with anonymous methods
     Developed by: Nuno Picado (https://github.com/nunopicado/Reusable-Objects)
  *)
  ITimer = interface(IInvokable)
  ['{1C06BCF6-1C6D-473E-993F-2B231B17D4F5}']
    function Start(const Action: TProc): ITimer;
    function Stop: ITimer;
    function Restart: ITimer;
  end;

function NewTimer(Interval: Cardinal): ITimer;

type
(*
  Minimalist SmartPointer implementation based on a blog post by Barry Kelly:
  http://blog.barrkel.com/2008/11/reference-counted-pointers-revisited.html,
  https://stackoverflow.com/questions/30153682/why-does-this-optimization-of-a-smartpointer-not-work
*)
  TSmartPtr = record
  private type
    TObjectHandle<T: class> = class(TInterfacedObject, TFunc<T>)
    private
      FValue: T;
    protected
      function Invoke:  T;
    public
      constructor Create(AValue:  T);
      destructor Destroy;  override;
    end;
  public
    class function Make<T: class>(AValue: T): TFunc<T>; static;
  end;


(* Font that persists Size instead of Height to be used for DPI awareness *)
  TStoredFont = class(Vcl.Graphics.TFont)
  published
    property Height stored False;
    property Size stored True;
    property Style default [];
  end;

var
  StopWatch: TStopwatch;

implementation

uses
  Winapi.UrlMon,
  Winapi.WinInet,
  Winapi.ShLwApi,
  System.Types,
  System.AnsiStrings,
  System.UITypes,
  System.IOUtils,
  System.Character,
  System.Math,
  System.Win.ComObj,
  System.NetEncoding,
  System.RegularExpressionsAPI,
  System.RegularExpressionsCore,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.Themes,
  JclFileUtils,
  JclPeImage,
  JvGnugettext,
  MPShellUtilities,
  SynUnicode,
  SynEditMiscProcs,
  SynEditTextBuffer,
  VarPyth,
  PythonEngine,
  StringResources,
  uEditAppIntfs,
  cPySupportTypes;

function GetIconIndexFromFile(const AFileName: string;
  const ASmall: Boolean): Integer;
var
  NameSpace: TNamespace;
  IconSize: TIconSize;
begin
  Result:= -1;
  // swallow any exceptions (bug report by Colin Williams)
  try
    if FileExists(AFileName) then begin
      if ASmall then
        IconSize := icSmall
      else
        IconSize := icLarge;
      NameSpace := TNamespace.CreateFromFileName(AFileName);
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
           System.SysUtils.DirectoryExists(ExcludeTrailingPathDelimiter(APath)))) then
    begin
      Result:= ExtractFilePath(APath);
      // we are up to top level
      if (Result = '') or (Result[Length(Result)] = ':') then
        Result:= APath
      else Result:= Concat(GetLongFileName(ExcludeTrailingPathDelimiter(Result)),
                           PathDelim,TPath.GetFileName(APath));
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

function StrGetToken(var Str: PChar;  Separators, WhiteSpace,
  QuoteChars: TSysCharSet): string;
(* returns next token - based on Classes.ExtractStrings *)
var
  Head, Tail: PChar;
  InQuote: Boolean;
  QuoteChar: Char;
begin
  Result:= '';
  if (Str = nil) or (Str^=#0) then Exit;
  Tail := Str;
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
    Str:= Tail;
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

function LightenColor(Color: TColor; Percentage: Integer): TColor;
begin
   Result := Winapi.ShLwApi.ColorAdjustLuma(ColorToRGB(Color),
     Percentage * 10, True);
end;

function DarkenColor(Color: TColor; Percentage: Integer): TColor;
begin
   Result := Winapi.ShLwApi.ColorAdjustLuma(ColorToRGB(Color),
      -Percentage * 10, True);
end;

function ApplicationVersion: string;
var
  ExeFile: string;
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

function ConnectedToInternet: Boolean;
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
  InetIsOffline: function(dwFlags: DWORD): BOOL; stdcall;
begin
  Result := False;
  hURLDLL := SafeLoadLibrary(URLDLL);
  if hURLDLL > 0 then
  begin
    @InetIsOffline := GetProcAddress(hURLDLL,'InetIsOffline');
    if Assigned(InetIsOffline) then begin
      if InetIsOffline(0) then
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

function GetNthLine(const Str: string; LineNo: Integer): string;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := Str;
    if LineNo <= SL.Count then
      Result := SL[LineNo-1]
    else
      Result := '';
  finally
    SL.Free;
  end;
end;

function GetLineRange(const Str: string; StartLine, EndLine: Integer;
   HtmlBreaks: Boolean): string;
var
  SL: TStringList;
  LastLine: Integer;
  LB: string;
begin
  Result := '';
  if HtmlBreaks then
    LB := '<br>'
  else
    LB:= sLineBreak;
  SL := TStringList.Create;
  try
    SL.Text := Str;
    LastLine := Min(EndLine-1, SL.Count -1);
    for var I := Max(0, StartLine-1) to LastLine do
      if I = LastLine then
        Result := Result + SL[I]
      else
        Result := Result + SL[I] + LB;
  finally
    SL.Free;
  end;
end;

function GetWordAtPos(const LineText: string; Start: Integer;
  AllowDot: Boolean; ScanBackwards: Boolean = True; ScanForward: Boolean = True;
  HandleBrackets: Boolean = False): string;

  function IsIdentChar(Chr: Char): Boolean;
  begin
    Result := Chr.IsLetterOrDigit or (Chr = '_') or (AllowDot and (Chr = '.'));
  end;

var
  I: Integer;
  Len, WordStart, WordEnd, ParenCounter, NewStart: Integer;
  Bracket, MatchingBracket: WideChar;
const
  AllBrackets = '()[]{}';
  CloseBrackets = [')', ']', '}'];
  OpenBrackets = ['(', '[', '{'];
begin
  Len := Length(LineText);
  WordStart := Start;
  WordEnd := Start;
  if (Start <= 0) or (Start > Len) then
    Exit('')
  else if not IsIdentChar(LineText[Start]) then
    Result := ''
  else begin
    if ScanBackwards then begin
      I := Start;
      while (I > 1) and IsIdentChar(LineText[I-1])
      do
        Dec(I);
      WordStart := I;
    end;
    if ScanForward then begin
      I := Start;
      while (I < Len) and IsIdentChar(LineText[I+1]) do
        Inc(I);
      WordEnd := I;
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
      MatchingBracket := AllBrackets[AllBrackets.IndexOf(Bracket)]; // IndexOf is zero based!

      ParenCounter := 1;
      I := NewStart - 1;
      while (I > 0) and (ParenCounter > 0) do
      begin
        if LineText[I] = Bracket then Inc(ParenCounter)
        else if LineText[I] = MatchingBracket then Dec(ParenCounter);
        Dec(I);
      end;
      WordStart := I+1;
      Result := Copy(LineText, WordStart, NewStart - WordStart + 1) + Result;
      if WordStart > 1 then
        // Recursive call
        Result := GetWordAtPos(LineText, WordStart - 1, AllowDot,
          ScanBackwards, False, True) + Result;
    end;
  end;
end;

function FormatDocString(const DocString: string): string;
var
  SL: TStringList;
  I, Margin: Integer;
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
    for I := 1 to SL.Count - 1 do
      if Trim(SL[I]) = '' then
        SL[I] := ''
      else
        Margin := Min(Margin, LeftSpaces(SL[I], True, 4));
    if (Margin > 0) and (Margin < MaxInt) then
      for I := 1 to SL.Count - 1 do
        if SL[I] <> '' then
          SL[I] := Copy(SL[I], Margin + 1);
    Result := SL.Text;
    // Remove any trailing or leading blank lines.
    Result.Trim([#10, #13]);
  finally
    SL.Free;
  end;
end;

function DirIsPythonPackage(Dir: string): Boolean;
begin
  Result :=  System.SysUtils.DirectoryExists(Dir) and
    FileExists(IncludeTrailingPathDelimiter(Dir) + '__init__.py');
end;

function FileIsPythonPackage(FileName: string): Boolean;
begin
  Result := TPath.GetFileName(FileName) = '__init__.py';
end;

function GetPackageRootDir(const Dir: string): string;
begin
  if not DirIsPythonPackage(Dir) then
    raise Exception.CreateFmt('"%s" is not a Python package', [Dir]);

  var CurrDir := Dir;
  repeat
    Result := CurrDir;
    CurrDir := TPath.GetDirectoryName(CurrDir);
  until (Result = CurrDir) or (not DirIsPythonPackage(CurrDir));
end;

function FileNameToModuleName(const FileName: string): string;
var
  Path, Dir, Server: string;
begin
  Result := TPath.GetFileNameWithoutExtension(FileName);
  if GI_SSHServices.ParseFileName(FileName, Server, Path) then Exit;

  Path := TPath.GetDirectoryName(FileName);
  Dir := TPath.GetFileName(Path);

  if Path <> '' then begin
    while DirIsPythonPackage(Path) and (Dir <> '') do begin
      Result := Dir + '.' + Result;
      Path := TPath.GetDirectoryName(Path);
      Dir := TPath.GetFileName(Path);
    end;
    if Result.EndsWith('.__init__') then
      Delete(Result, Length(Result) - 8, 9);
  end;
end;

function HTMLSafe(const Str: string): string;
begin
  Result := StringReplace(Str, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, #13#10, '<br>', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '<br>', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '<br>', [rfReplaceAll]);
end;

function GetParamStr(P: PChar; var Param: string): PChar;
// From Delphi's system.pas unit!
var
  I, Len: Integer;
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
  I := 0;
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
          S[I] := P^;
          Inc(P);
          Inc(I);
        end;
      end;
      if P[0] <> #0 then P := CharNext(P);
    end
    else
    begin
      Q := CharNext(P);
      while P < Q do
      begin
        S[I] := P^;
        Inc(P);
        Inc(I);
      end;
    end;
  end;

  Result := P;
end;

function ParsePySourceEncoding(TextLine: string): string;
begin
  Result := '';
  with TRegEx.Match(TextLine, 'coding[:=]\s*([-\w.]+)') do
    if Success then
      Exit(Groups[1].Value);
end;

function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  Buffer: array[0..51] of WideChar;
  TM: TTextMetric;
begin
  for var I := 0 to 25 do Buffer[I] := WideChar(I + Ord('A'));
  for var I := 0 to 25 do Buffer[I + 26] := WideChar(I + Ord('a'));
  GetTextMetrics(Canvas.Handle, TM);
  GetTextExtentPointW(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := (Result.X div 26 + 1) div 2;
  Result.Y := TM.tmHeight;
end;

type
  TSyncInputQuery = class
  private
    FCaption, FPrompt, FValue: string;
    FRes: Boolean;
  public
    constructor Create(ACaption, APrompt, AValue: string);
    procedure InputQuery;
  end;

{ TSyncInputQuery }

constructor TSyncInputQuery.Create(ACaption, APrompt, AValue: string);
begin
  FCaption := ACaption;
  FPrompt := APrompt;
  FValue := AValue;
end;

procedure TSyncInputQuery.InputQuery;
begin
  FRes := Vcl.Dialogs.InputQuery(FCaption, FPrompt, FValue);
end;

function SyncWideInputQuery(const ACaption, APrompt: string; var Value: string): Boolean;
var
  SyncInputQuery: TSyncInputQuery;
  SaveThreadState: PPyThreadState;
begin
  if GetCurrentThreadId = MainThreadID then
    Result := InputQuery(ACaption, APrompt, Value)
  else begin
    SyncInputQuery := TSyncInputQuery.Create(ACaption, APrompt, Value);
    try
      with GetPythonEngine do begin
        SaveThreadState := PyEval_SaveThread();
        try
          Sleep(100);  // So that PythonIOSendData delay expires #1296
          TThread.Synchronize(nil, SyncInputQuery.InputQuery);
        finally
          PyEval_RestoreThread(SaveThreadState);
        end;
      end;
      Result := SyncInputQuery.FRes;
      Value := SyncInputQuery.FValue;
    finally
      SyncInputQuery.Free;
    end;
  end;
end;

function CleanEOLs(Str: AnsiString): AnsiString;
begin
  Result := System.AnsiStrings.AdjustLineBreaks(Str, System.tlbsLF);
end;

function CleanEOLs(Str: string): string;
begin
  Result := System.SysUtils.AdjustLineBreaks(Str, System.tlbsLF);
end;

function SortedIdentToInt(const Ident: string; var Int: LongInt;
                          const SortedMap: array of TIdentMapEntry;
                          CaseSensitive: Boolean = False): Boolean;
var
  M, N, K, I: Integer;
begin
  M := Low(SortedMap);
  N := High(SortedMap);
  while M<=N do
    begin
      K := M+(N-M) div 2;
      if CaseSensitive then
        I := CompareStr(Ident, SortedMap[K].Name)
      else
        I := CompareText(Ident, SortedMap[K].Name);
      if I = 0 then begin
          Result := True;
          Int := SortedMap[K].Value;
          Exit;
      end else if I > 0 then
         M := K+1
      else
        N := K-1;
    end;
  Result := False;
end;

function ComparePythonIdents(const Ident1, Ident2: string): Integer; overload;
var
  Len1, Len2: Integer;
begin
  Len1 := Length(Ident1);
  Len2 := Length(Ident2);
  if (Len1 > 0) and (Ident1[1] = '_') and (Len2>0) and (Ident2[1] = '_') then
    Result := CompareText(Ident1, Ident2)
  else if (Len1 > 0) and (Ident1[1] = '_') then
    Result := 1
  else if (Len2>0) and (Ident2[1] = '_') then
    Result := -1
  else
    Result := CompareText(Ident1, Ident2);
end;

function ComparePythonIdents(List: TStringList; Index1, Index2: Integer): Integer; overload;
var
  Str1, Str2: string;
begin
  Str1 := List[Index1];
  Str2 := List[Index2];
  Result := ComparePythonIdents(Str1, Str2);
end;

function DefaultCodeFontName: string;
begin
    if CheckWin32Version(6) then
    begin
      if Screen.Fonts.IndexOf('Cascadia Code') >= 0 then
        Result := 'Cascadia Code'
      else
        Result := 'Consolas';
    end
    else
      Result := 'Courier New';
end;

procedure SetDefaultUIFont(const AFont: TFont);
const
  UIFont = 'Segoe UI';
begin
  if CheckWin32Version(6) and (Screen.Fonts.IndexOf(UIFont) >= 0) then
  begin
    AFont.Size := 9;
    AFont.Name := UIFont;
  end;
end;

procedure SetContentFont(const AFont: TFont);
const
  VistaContentFont = 'Calibri';
begin
  if CheckWin32Version(6)
    and not SameText(AFont.Name, VistaContentFont)
    and (Screen.Fonts.IndexOf(VistaContentFont) >= 0) then
  begin
    AFont.Size := 9;
    AFont.Name := VistaContentFont;
  end;
end;

function GetBlockText(Strings: TStrings; BlockBegin, BlockEnd: TBufferCoord): string;
var
  Line: Integer;
begin
  // preconditions start
  Assert(BlockBegin.Line <= Strings.Count, 'GetBlockText');
  Assert(BlockEnd.Line <= Strings.Count, 'GetBlockText');
  Assert(BlockBegin.Line <= BlockEnd.Line, 'GetBlockText');
  if BlockBegin.Line <= 0 then Exit;
  if BlockEnd.Line <= 0 then Exit;
  // preconditions end

  // work backwards
  Line := BlockEnd.Line;
  Result := Copy(Strings[Line-1], 1, BlockEnd.Char - 1);
  while (Line > BlockBegin.Line) and (Line > 1) do begin
    Dec(Line);
    Result := Strings[Line-1] + WideCRLF + Result;
  end;
  if Line = BlockBegin.Line then
    Delete(Result, 1, BlockBegin.Char -1);
end;

function WideStringsToEncodedText(const AFileName: string;
  Lines: TStrings; var EncodedText: AnsiString;
  InformationLossWarning: Boolean = False;
  IsPython: Boolean = False): Boolean;
// AFileName is passed just for the warning
var
  PyEncoding: AnsiString;
  UniPy, EncodedString: PPyObject;
  WStr: string;
  SuppressOutput: IInterface;
  OldLineBreak: string;
begin
  Result := True;

  //  Encoded strings cannot contain WideLineSeparator
  OldLineBreak := Lines.LineBreak;
  if Lines.LineBreak = WideLineSeparator then
    Lines.LineBreak := sLineBreak;

  WStr := Lines.Text;
  Lines.LineBreak := OldLineBreak;

  if GI_PyControl.PythonLoaded and
    (IsPython or GI_PyIDEServices.FileIsPythonSource(AFileName)) then
  begin
    PyEncoding := '';
    if Lines.Count > 0 then
      PyEncoding := UTF8Encode(ParsePySourceEncoding(Lines[0]));
    if (PyEncoding = '') and (Lines.Count > 1) then
      PyEncoding := UTF8Encode(ParsePySourceEncoding(Lines[1]));

    with SafePyEngine.PythonEngine do
    begin
      if PyEncoding = '' then
        PyEncoding := UTF8Encode(SysModule.getdefaultencoding());
      SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
      UniPy := nil;
      EncodedString := nil;
      try
        try
          UniPy := PyUnicode_FromWideChar(PWideChar(WStr), Length(WStr));
          CheckError;
          if InformationLossWarning then begin
            try
              EncodedString := PyUnicode_AsEncodedString(UniPy, PAnsiChar(PyEncoding), 'strict');
              CheckError;
              EncodedText := PyBytesAsAnsiString(EncodedString);
              CheckError;
            except
              on UnicodeEncodeError do
              begin
                Result :=
                  StyledMessageDlg(Format(_(SFileEncodingWarning),
                    [AFileName, PyEncoding]), mtWarning, [mbYes, mbCancel], 0)= mrYes;
                if Result then
                begin
                  EncodedString := PyUnicode_AsEncodedString(UniPy,
                    PAnsiChar(PyEncoding), 'replace');
                  CheckError;
                  EncodedText := PyBytesAsAnsiString(EncodedString);
                  CheckError;
                end;
              end;
            end;
          end
          else
          begin
            EncodedString := PyUnicode_AsEncodedString(UniPy, PAnsiChar(PyEncoding), 'replace');
            CheckError;
            EncodedText := PyBytesAsAnsiString(EncodedString);
            CheckError;
          end;
        finally
          Py_XDECREF(UniPy);
          Py_XDECREF(EncodedString);
        end;
      except
        PyErr_Clear;
        EncodedText := AnsiString(WStr);
        if InformationLossWarning then
          Result :=
            StyledMessageDlg(Format(_(SFileEncodingWarning),
              [AFileName, PyEncoding]), mtWarning, [mbYes, mbCancel], 0)= mrYes ;
      end;
    end;
  end
  else
  begin
    EncodedText := AnsiString(WStr);
    if InformationLossWarning and not IsAnsiOnly(WStr) then begin
      Result :=
        StyledMessageDlg(Format(_(SFileEncodingWarning),
        [AFileName, 'ANSI']), mtWarning, [mbYes, mbNo], 0) = mrYes ;
    end;
  end;
end;

function LoadFileIntoWideStrings(const AFileName: string; Lines: TStrings): Boolean;

  procedure LoadFromString(Str: string);
  begin
    if Lines is TSynEditStringList then begin
      TSynEditStringList(Lines).SetTextAndFileFormat(Str);
      TSynEditStringList(Lines).SetEncoding(TEncoding.ANSI);
    end else if Lines is TXStringList then begin
      TXStringList(Lines).SetTextAndFileFormat(Str);
      TXStringList(Lines).SetEncoding(TEncoding.ANSI);
    end else
      Lines.Text := Str;
  end;

var
  Reader: TStreamReader;
  FileStream: TFileStream;
  FileText, PyEncoding: AnsiString;
  Len: Integer;
  PyWstr: PPyObject;
begin
  Result := True;
  if (AFileName = '') or not FileExists(AFileName) then Exit(False);

  try
    if not GI_PyIDEServices.FileIsPythonSource(AFileName) then
    begin
      Lines.LoadFromFile(AFileName);
      Exit(True);
    end;

    // Special processing for python files
    // Try to use an encoding comment if present
    Reader := TStreamReader.Create(AFileName, TEncoding.Default, True, 128);
    try
      // Reads the first Line - Also sets the CurrentEncoding
      var Line := Reader.ReadLine;
      if Reader.CurrentEncoding <> TEncoding.ANSI then
      begin
          // BOM found
        Lines.LoadFromFile(AFileName);
        Exit(True);
      end;

      // Detect an encoding spec
      PyEncoding := UTF8Encode(ParsePySourceEncoding(Line));
      if PyEncoding = '' then
      begin
        Line := Reader.ReadLine;
        PyEncoding := UTF8Encode(ParsePySourceEncoding(Line));
      end;
    finally
      Reader.Free;
    end;

    // if there is no encoding line or Python is not loaded use default encoding UTF8
    if not GI_PyControl.PythonLoaded or (PyEncoding = '') or
      (LowerCase(PyEncoding) = 'utf-8') or (LowerCase(PyEncoding) = 'utf8')
    then
    begin
      Lines.LoadFromFile(AFileName, TEncoding.UTF8);
      Exit(True);
    end;

    // we have an encoding we can use with python
    FileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    // Read the file into FileText
    try
      Len := FileStream.Size;
      SetLength(FileText, Len);
      FileStream.ReadBuffer(FileText[1], Len);
    finally
      FileStream.Free;
    end;

    PyWstr := nil;
    try
      var Py := SafePyEngine;
      with Py.PythonEngine do begin
        try
            PyWstr := PyUnicode_Decode(PAnsiChar(FileText),
              Length(FileText),
              PAnsiChar(PyEncoding), 'replace');
            CheckError;
            LoadFromString(PyUnicodeAsString(PyWstr));
        finally
          Py_XDECREF(PyWstr);
        end;
      end;
    except
      StyledMessageDlg(Format(_(SDecodingError),
         [AFileName, PyEncoding]), mtWarning, [mbOK], 0);
      LoadFromString(string(FileText));
    end;

  except
    on E: Exception do begin
      StyledMessageDlg(Format(_(SFileOpenError), [AFileName, E.Message]), mtError, [mbOK], 0);
      Result := False;
    end;
  end;
end;

type
  TSynEditStringListAccess = class(TSynEditStringList);

(* Save WideStrings to file taking into account Python file encodings *)
function SaveWideStringsToFile(const AFileName: string;
  Lines: TStrings;  DoBackup: Boolean = True): Boolean;
var
  FileStream: TFileStream;
  EncodedText: AnsiString;
begin
  try
    // Create Backup
    if DoBackup and
      FileExists(AFileName) then
    begin
      try
        FileBackup(AFileName);
      except
        StyledMessageDlg(Format(_(SFailedToBackupFile), [AFileName]),
          mtWarning, [mbOK], 0);
      end;
    end;

    if not GI_PyIDEServices.FileIsPythonSource(AFileName) then
    begin
      if (Lines.Encoding <> TEncoding.ANSI) or IsAnsiOnly(Lines.Text) then
      begin
        Lines.SaveToFile(AFileName);
        Exit(True);
      end
      // Warn about information loss
      else if StyledMessageDlg(Format(_(SFileEncodingWarning),
        [AFileName, 'ANSI']), mtWarning, [mbYes, mbNo], 0) = mrYes then
      begin
        Lines.SaveToFile(AFileName);
        Exit(True);
      end;
      Exit(False);
    end
    else if (Lines.Encoding <> TEncoding.ANSI) then
    begin
      Lines.SaveToFile(AFileName);
      Exit(True);
    end;

    // For Ansi encoded Python files we deal with coding comments
    Result := WideStringsToEncodedText(AFileName, Lines, EncodedText, True);

    if Result then begin
      FileStream := TFileStream.Create(AFileName, fmCreate);
      try
        FileStream.WriteBuffer(EncodedText[1], Length(EncodedText));
      finally
        FileStream.Free;
      end;
    end;
  except
    on E: Exception do begin
      StyledMessageDlg(Format(_(SFileSaveError), [AFileName, E.Message]), mtError, [mbOK], 0);
      Result := False;
    end;
  end;
end;

function CopyRemoteFileToTemp(const AFileName, Server: string): string;
// Aborts on failure
var
  ErrorMsg: string;
begin
  var AppName := TPath.GetFileNameWithoutExtension(Application.ExeName);
  Result := ChangeFileExt(FileGetTempName(AppName), ExtractFileExt(AFileName));

  if not GI_SSHServices.ScpDownload(Server, AFileName, Result, ErrorMsg) then
  begin
    StyledMessageDlg(Format(_(SFileOpenError), [AFileName, ErrorMsg]), mtError, [mbOK], 0);
    Abort;
  end;
end;


function FileToEncodedStr(const AFileName: string): AnsiString;
var
  SL: TStrings;
  Server, FName, TempFileName: string;
begin
  if GI_SSHServices.ParseFileName(AFileName, Server, FName) then
    TempFileName := CopyRemoteFileToTemp(FName, Server)
  else
    TempFileName := AFileName;

  SL := TStringList.Create;
  try
    if not LoadFileIntoWideStrings(TempFileName, SL) then Abort;
    WideStringsToEncodedText(AFileName, SL, Result, False);
  finally
    SL.Free;
  end;
  if Server <> '' then DeleteFile(TempFileName);
end;


function FileToStr(const AFileName: string): string;
var
  SL: TStrings;
  Server, FName, TempFileName: string;
begin
  if GI_SSHServices.ParseFileName(AFileName, Server, FName) then
    TempFileName := CopyRemoteFileToTemp(FName, Server)
  else
    TempFileName := AFileName;

  SL := TStringList.Create;
  try
    if not LoadFileIntoWideStrings(TempFileName, SL) then Abort;
    Result := SL.Text;
  finally
    SL.Free;
  end;
  if Server <> '' then DeleteFile(TempFileName);
end;


function GetNthSourceLine(const AFileName: string; LineNo: Integer): string;
var
  TempS: string;
begin
  Result := '';
  var Editor := GI_EditorFactory.GetEditorByFileId(AFileName);
  if Assigned(Editor) then begin
    if Editor.SynEdit.Lines.Count >= LineNo then
      Exit(Editor.SynEdit.Lines[LineNo - 1])
    else
      Exit;
  end
  else if FileExists(AFileName) then
  try
    var SR := TSmartPtr.Make(TStreamReader.Create(AFileName))();
    for var I := 1 to LineNo do
      if SR.EndOfStream then
        Exit
      else
        TempS := SR.ReadLine;
    Result := TempS;
  except
  end;
end;

(*
   Single directory traversal function used in WalkThroughDirectories
*)
procedure WalkThroughDirectory(const Path: string; Masks: TStringDynArray;
  const PreCallback: TDirectoryWalkProc;
  const Recursive: Boolean);
var
  SearchRec: TSearchRec;
  Match: Boolean;
  Stop: Boolean;
  PathWithSep, Mask: string;
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
           (SearchRec.Attr and System.SysUtils.faHidden = 0) and
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
var
  Path, PathName: string;
  PathList, MaskList: TArray<string>;
begin
  PathList := Paths.Split([';']);
  MaskList := Masks.Split([';']);
  for Path in PathList do
  begin
    PathName := GI_PyIDEServices.ReplaceParams(Path);
    if  System.SysUtils.DirectoryExists(PathName) then
      WalkThroughDirectory(PathName, MaskList, PreCallback, Recursive);
  end;
end;

procedure GetFilesInPaths(Paths, Masks: string; FileList: TStrings; Recursive: Boolean = True);
var
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

procedure GetDirectoriesInPaths(Paths, Masks: string; DirList: TStrings; Recursive: Boolean = True);
var
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

function StrToken(var Str: string; Separator: Char): string;
var
  I: Integer;
begin
  I := Str.IndexOf(Separator);
  if I >= 0 then
  begin
    Result := Str.Substring(0, I);
    Delete(Str, 1, I + 1);
  end
  else
  begin
    Result := Str;
    Str := '';
  end;
end;

function StrStripChar(const Str: string; const AChar: Char): string;
var
  Source, Dest: PChar;
  Len, Index: Integer;
begin
  Len := Length(Str);
  SetLength(Result, Len);
  UniqueString(Result);
  Source := PChar(Str);
  Dest := PChar(Result);
  for Index := 0 to Len - 1 do
  begin
    if Source^ <> AChar then
    begin
      Dest^ := Source^;
      Inc(Dest);
    end;
    Inc(Source);
  end;
  SetLength(Result, Dest - PChar(Result));
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

function IsDigits(Str: string): Boolean;
begin
  Result := True;
  for var I := 1 to Str.Length do
    if not CharInSet(Str[I], ['0'..'9']) then
      Exit(False);
end;

function Dedent(const Str: string): string;
begin
  with TRegEx.Match(Str, '^\s+') do
    if Success then
      // to avoid repeated replacements of initial space
      Exit(TRegEx.Replace(TRegEx.Replace(Str,
         '^'+Groups[0].Value , '!', [roNotEmpty, roMultiLine]),
         '^!' , '', [roNotEmpty, roMultiLine]))
    else
      Exit(Str);
end;

function CompiledRegEx(Expr: string; Options: TRegExOptions = [roNotEmpty];
  UCP: Boolean = True): TRegEx;
begin
  try
    Result := TRegEx.Create(Expr, Options);
    if UCP then
      Result.AddRawOptions(PCRE_UCP);
    Result.Study([preJIT]);
  except
    on E: ERegularExpressionError do
      begin
        StyledMessageDlg(Format(_(SInvalidRegularExpression), [E.Message]),
          mtError, [mbOK], 0);
        Abort;
      end;
    else
      raise;
  end;
end;

function IsColorDark(AColor: TColor): Boolean;
var
  ACol: LongInt;
begin
  ACol := ColorToRGB(AColor) and $00FFFFFF;
  Result := ((2.99 * GetRValue(ACol) + 5.87 * GetGValue(ACol) +
                 1.14 * GetBValue(ACol)) < $400);
end;

function IsStyledWindowsColorDark: Boolean;
begin
  Result := IsColorDark(StyleServices.GetSystemColor(clWindow));
end;

procedure StyledBorderColors(out BorderNormal, BorderHighlight: TColor);
begin
  if IsStyledWindowsColorDark then begin
    BorderHighlight := StyleServices.GetSystemColor(clBtnHighlight);
    BorderNormal := StyleServices.GetSystemColor(clBtnFace);
  end else begin
    BorderHighlight := StyleServices.GetSystemColor(clBtnShadow);
    BorderNormal := StyleServices.GetSystemColor(clBtnFace);
  end;
end;

{ TXStringList }

constructor TXStringList.Create;
begin
  inherited Create;
  FUTF8CheckLen := -1;
  Options := Options - [soWriteBOM, soTrailingLineBreak];
  FDetectUTF8 := True;
end;

procedure TXStringList.LoadFromStream(Stream: TStream; Encoding: TEncoding);
var
  Size: Integer;
  Buffer: TBytes;
begin
  BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    SetLength(Buffer, Size);
    Stream.Read(Buffer, 0, Size);
    Size := TEncoding.GetBufferEncoding(Buffer, Encoding, DefaultEncoding);
    WriteBOM := Size > 0; // Keep WriteBom in case the stream is saved
    // If the encoding is ANSI and DetectUtf8 is True try to Detect UTF8
    if (Encoding = TEncoding.ANSI) and DetectUTF8 and IsUTF8(Buffer, Size) then
      Encoding := TEncoding.UTF8;
    SetEncoding(Encoding); // Keep Encoding in case the stream is saved
    SetTextAndFileFormat(Encoding.GetString(Buffer, Size, Length(Buffer) - Size));
  finally
    EndUpdate;
  end;
end;

procedure TXStringList.SaveToStream(Stream: TStream; Encoding: TEncoding);
var
  Cancel: Boolean;
  Content: string;
  Buffer, Preamble: TBytes;
begin
  if Encoding = nil then
    Encoding := DefaultEncoding;

  Content := GetTextStr;
  Cancel := False;
  if (Encoding = TEncoding.ANSI) and Assigned(FOnInfoLoss) and not IsAnsiOnly(Content) then
  begin
    FOnInfoLoss(Encoding, Cancel);
    if Cancel then
      Exit;
    if Encoding <> TEncoding.ANSI then
      SetEncoding(Encoding);
  end;

  Buffer := Encoding.GetBytes(Content);
  if WriteBOM then
  begin
    Preamble := Encoding.GetPreamble;
    if Length(Preamble) > 0 then
      Stream.WriteBuffer(Preamble, Length(Preamble));
  end;
  Stream.WriteBuffer(Buffer, Length(Buffer));
end;

procedure TXStringList.SetTextAndFileFormat(const Value: string);
var
  LineText: string;
  Size: Integer;
  P, Start, Pmax: PWideChar;
  HasCR, HasLF, UnicodeSeparator: Boolean;
begin
  UnicodeSeparator := False;
  HasCR := False;
  HasLF := False;
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
        while not (Ord(P^) in [0, $A, $D]) and (P^ <> WideLineSeparator) do
        begin
          Inc(P);
        end;
        if P<>Start then
        begin
          SetString(LineText, Start, P - Start);
          Insert(Count, LineText);
        end else Insert(Count, '');
        if P^ = WideLineSeparator then
        begin
          UnicodeSeparator := True;
          Inc(P);
        end;
        if P^ = WideCR then
        begin
          HasCR := True;
          Inc(P);
        end;
        if P^ = WideLF then
        begin
          HasLF := True;
          Inc(P);
        end;
      end;
      // keep the old format of the file
      if not TrailingLineBreak and
        (CharInSet(Value[Size], [#10, #13]) or (Value[Size] = WideLineSeparator))
      then
        Insert(Count, '');
    end;
  finally
    EndUpdate;
  end;
  if UnicodeSeparator then
    LineBreak := WideLineSeparator
  else if HasCR and not HasLF then
    LineBreak := WideCR
  else if HasLF and not HasCR then
    LineBreak := WideLF
  else
    LineBreak := WideCRLF;
end;

procedure AddFormatText(RE: TRichEdit; const Str: string;  FontStyle: TFontStyles = [];
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

    SelText := Str;
  end;
end;

function MonitorProfile: string;

  function DesktopSizeString: string;
  begin
    Result := Format('(%dx%d)', [Screen.DesktopWidth, Screen.DesktopHeight]);
  end;
const
  StrMask = '%d:%dDPI(%s,%d,%d,%d,%d)';
var
  MonitorNumber: Integer;
  Monitor: TMonitor;
begin
  Result := DesktopSizeString;
  for MonitorNumber := 0 to Screen.MonitorCount - 1 do
    begin
        Monitor := Screen.Monitors[MonitorNumber];
        Result := Result + Format(StrMask, [
        Monitor.MonitorNum,
        Monitor.PixelsPerInch,
        BoolToStr(Monitor.Primary, True),
        Monitor.Left,
        Monitor.Top,
        Monitor.Width,
        Monitor.Height
      ]);
    end;
end;

function DownloadUrlToFile(const URL, Filename: string): Boolean;
begin
  Result := Succeeded(URLDownloadToFile(nil, PWideChar(URL), PWideChar(Filename), 0, nil));
end;

{ ITimer implementation }
type
  TTimer = class(TInterfacedObject, ITimer)
  private var
    FTimer: Vcl.ExtCtrls.TTimer;
    FAction: TProc;
    procedure RunAction(Sender: TObject);
  public
    constructor Create(const Interval: Cardinal);
    destructor Destroy; override;
    class function New(const Interval: Cardinal): ITimer;
    function Start(const Action: TProc): ITimer;
    function Stop: ITimer;
    function Restart: ITimer;
  end;

{ TTimer }

constructor TTimer.Create(const Interval: Cardinal);
begin
  FTimer          := Vcl.ExtCtrls.TTimer.Create(nil);
  FTimer.Enabled  := False;
  FTimer.Interval := Interval;
  FTimer.OnTimer  := RunAction;
end;

destructor TTimer.Destroy;
begin
  FTimer.Free;
  inherited;
end;

class function TTimer.New(const Interval: Cardinal): ITimer;
begin
  Result := Create(Interval);
end;

procedure TTimer.RunAction(Sender: TObject);
begin
  FAction;
end;

function TTimer.Start(const Action: TProc): ITimer;
begin
  Result         := Self;
  FAction        := Action;
  FTimer.Enabled := True;
end;

function TTimer.Stop: ITimer;
begin
  Result         := Self;
  FTimer.Enabled := False;
end;

function TTimer.Restart: ITimer;
begin
  Result         := Self;
  FTimer.Enabled := True;
end;

function NewTimer(Interval: Cardinal): ITimer;
begin
  Result := TTimer.Create(Interval);
end;

function StyledMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: LongInt): Integer;
begin
  UseLatestCommonDialogs := False;
  Result := Vcl.Dialogs.MessageDlg(Msg, DlgType, Buttons, HelpCtx);
  UseLatestCommonDialogs := True;
end;

function StyledMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: LongInt; DefaultButton: TMsgDlgBtn): Integer;
begin
  UseLatestCommonDialogs := False;
  Result := Vcl.Dialogs.MessageDlg(Msg, DlgType, Buttons, HelpCtx, DefaultButton);
  UseLatestCommonDialogs := True;
end;

function SvgFixedColor(Color: TColor): TColor;
begin
  Result := StyleServices.GetSystemColor(Color);
  if Result = clBlack then
    Result := $191919
  else if Result = clWhite then
    Result := $E6E6E6;
end;

procedure FreeAndNilEncoding(var Encoding: TEncoding);
begin
  if Assigned(Encoding) then
  begin
    if not TEncoding.IsStandardEncoding(Encoding) then
      Encoding.Free;
    Encoding := nil;
  end;
end;

function FilePathToURI(FilePath: string): string;
const
  // The use of this flag in UrlCreateFromPath is undocumented.
  // It is used in URLEscape.  Its use fixes #1346.
  URL_ESCAPE_AS_UTF8: DWORD = $00040000;
begin
  var BufferLen: DWORD := INTERNET_MAX_URL_LENGTH;
  SetLength(Result, BufferLen);
  OleCheck(UrlCreateFromPath(PChar(FilePath), PChar(Result), @BufferLen, URL_ESCAPE_AS_UTF8));
  SetLength(Result, BufferLen);
end;

function URIToFilePath(URI: string): string;
begin
  var DecodedURI := TURLEncoding.URL.Decode(URI);
  var BufferLen: DWORD := MAX_PATH;
  SetLength(Result, BufferLen);
  OleCheck(PathCreateFromUrl(PChar(DecodedURI), PChar(Result), @BufferLen, 0));
  SetLength(Result, BufferLen);
end;

function RegisterApplicationRestart(Flags: DWORD = 0): Boolean;
type
  TPKernelRegisterApplicationRestart = function(lpCmdLine: PWideChar; dwFlags: DWORD): HRESULT; stdcall;
var
  RegisterApplicationRestart: TPKernelRegisterApplicationRestart;
begin
  GI_PyIDEServices.Logger.Write('RegisterApplicationRestart');
  Result := False;
  @RegisterApplicationRestart := GetProcAddress(GetModuleHandle('kernel32.dll'), 'RegisterApplicationRestart');
  if @RegisterApplicationRestart <> nil then
    Result := RegisterApplicationRestart('', 0) = S_OK;
end;

procedure UnregisterApplicationRestart;
type
  TPKernelUnregisterApplicationRestart = function: HRESULT; stdcall;
var
  UnregisterApplicationRestart: TPKernelUnregisterApplicationRestart;
begin
  @UnregisterApplicationRestart := GetProcAddress(GetModuleHandle('kernel32.dll'), 'UnregisterApplicationRestart');
  if @UnregisterApplicationRestart <> nil then
    UnregisterApplicationRestart();
end;

function IsFileBlocked(const FileName: string): Boolean;
const
  STREAM_NAME: PChar = ':Zone.Identifier';
var
  FileHandle: THandle;
begin
  FileHandle := CreateFile(PChar(FileName + STREAM_NAME), GENERIC_READ,
    FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  Result := FileHandle <> INVALID_HANDLE_VALUE;
  if Result then
    CloseHandle(FileHandle);
end;

procedure UnblockFile(const FileName: string);
const
  STREAM_NAME: PChar = ':Zone.Identifier';
var
  FileHandle: THandle;
begin
  FileHandle := CreateFile(PChar(FileName + STREAM_NAME), GENERIC_WRITE, 0, nil,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if FileHandle <> INVALID_HANDLE_VALUE then
  begin
    CloseHandle(FileHandle);
    DeleteFile(PChar(FileName + STREAM_NAME));
  end;
end;

function HTMLEncode(const Str: string): string;
var
  Chr: Char;
  SB: TStringBuilder;
begin
  if Str = '' then Exit('');

  SB := TStringBuilder.Create(Length(Str) * 2); // Initial capacity estimate
  try
    for Chr in Str do
    begin
      case Chr of
        '&': SB.Append('&amp;');
        '"': SB.Append('&quot;');
        '<': SB.Append('&lt;');
        '>': SB.Append('&gt;');
        else SB.Append(Chr);
      end;
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

{ TMatchHelper }

function TMatchHelper.GroupIndex(Index: Integer): Integer;
begin
  if Index < Groups.Count then
    Result := Groups[Index].Index
  else
    Result := -1;
end;

function TMatchHelper.GroupLength(Index: Integer): Integer;
begin
  if Index < Groups.Count then
    Result := Groups[Index].Length
  else
    Result := 0;
end;

function TMatchHelper.GroupValue(Index: Integer): string;
begin
  if Index < Groups.Count then
    Result := Groups[Index].Value
  else
    Result := '';
end;

procedure RedirectFunction(OrgProc, NewProc: Pointer);
{
  From spring4d
  See https://devblogs.microsoft.com/oldnewthing/20181206-00/?p=100415
  in relation to the use of WriteProcessMemory
}
type
  TJmpBuffer = packed record
    Jmp: Byte;
    Offset: Integer;
  end;
var
  BytesWritten: NativeUInt;
  JmpBuffer: TJmpBuffer;
begin
  JmpBuffer.Jmp := $E9;
  JmpBuffer.Offset := PByte(NewProc) - (PByte(OrgProc) + 5);
  if not WriteProcessMemory(GetCurrentProcess, OrgProc,
    @JmpBuffer, SizeOf(JmpBuffer), BytesWritten)
  then
    RaiseLastOSError;
  FlushInstructionCache(GetCurrentProcess, OrgProc, SizeOf(JmpBuffer));
end;

function PatchedGetCursorPos(var lpPoint: TPoint): BOOL; stdcall;
(* The GetCursorPos API in user32 fails if it is passed a memory address >2GB
   which breaks LARGEADDRESSAWARE apps.  We counter this by calling GetCursorInfo
   instead which does not suffer from the same problem.

   In addition we have had problems with GetCursorPos failing when called
   immediately after a password protected screensaver or a locked workstation
   re-authenticates. This problem initially appeared with XP SP1 and is brought
   about because TMouse.GetCursorPos checks the return value of the GetCursorPos
   API call and raises an OS exception if the API has failed.

   https://stackoverflow.com/questions/20142166/explain-errors-from-getkeystate-getcursorpos
*)
var
  CursorInfo: TCursorInfo;
begin
  CursorInfo.cbSize := SizeOf(CursorInfo);
  Result := GetCursorInfo(CursorInfo);
  if Result then begin
    lpPoint := CursorInfo.ptScreenPos;
  end else begin
    lpPoint := Point(0, 0);
  end;
end;

var
  OldGetCursorPos: function(var lpPoint: TPoint): BOOL; stdcall = nil;

{ TFormHelper }

function TControlHelper.PPIScale(ASize: Integer): Integer;
begin
   Result := MulDiv(ASize, FCurrentPPI, 96);
end;

function TControlHelper.PPIUnScale(ASize: Integer): Integer;
begin
   Result := MulDiv(ASize, 96, FCurrentPPI);
end;

{ TSmartPointer }

constructor TSmartPtr.TObjectHandle<T>.Create(AValue:  T);
begin
  FValue  :=  AValue;
end;

destructor TSmartPtr.TObjectHandle<T>.Destroy;
begin
  FValue.Free;
  inherited;
end;

function TSmartPtr.TObjectHandle<T>.Invoke: T;
begin
  Result  :=  FValue;
end;

class function TSmartPtr.Make<T>(AValue: T): TFunc<T>;
begin
  Result := TObjectHandle<T>.Create(AValue);
end;

initialization
  StopWatch := TStopwatch.StartNew;

  @OldGetCursorPos := GetProcAddress(GetModuleHandle(user32), 'GetCursorPos');
  with TJclPeMapImgHooks do
    ReplaceImport(SystemBase, user32, @OldGetCursorPos, @PatchedGetCursorPos);

finalization
 with TJclPeMapImgHooks do
   ReplaceImport(SystemBase, user32, @PatchedGetCursorPos, @OldGetCursorPos);
end.
