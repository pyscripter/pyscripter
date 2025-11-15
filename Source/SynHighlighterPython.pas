{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterPython.pas, released 2000-06-23.
The Original Code is based on the odPySyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Olivier Deckmyn.
Portions created by M.Utku Karatas and Dennis Chuah.
Unicode translation by Maλl Hφrz.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.
-------------------------------------------------------------------------------}
{
@abstract(A Python language highlighter for SynEdit)
@author(Olivier Deckmyn, converted to SynEdit by David Muir <dhmn@dmsoftware.co.uk>)
@created(unknown, converted to SynEdit on 2000-06-23)
@lastmod(2003-02-13)
The SynHighlighterPython implements a highlighter for Python for the SynEdit projects.
}
unit SynHighlighterPython;

interface

uses
  System.Classes,
  System.RegularExpressions,
  SynEditHighlighter,
  SynEditCodeFolding,
  StringResources,
  SynEditTypes;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkString, tkSymbol, tkNonKeyword, tkCodeComment, tkTrippleQuotedString,
    tkTrippleQuotedString2, tkFunctionName, tkClassName, tkSystemDefined, tkHex,
    tkOct, tkFloat, tkUnknown,
    // used in the interpreter
    tkBanner, tkOutput, tkTraceback,
    tkPrompt, tkSystemCmd);

  TRangeState = (rsUnknown, rsMultilineString, rsMultilineString2,
                 rsMultilineString3, //this is to indicate if a string is made multiline by backslash char at line end
                 rsTraceback  // used in the interpreter
                );

  TPyFoldType = (pftCodeBlock, pftMultiLineStringFoldType, pftClassDefType, pftFunctionDefType);

  TSynPythonSyn = class(TSynCustomCodeFoldingHighlighter)
  private
    FStringStarter: WideChar;  // used only for rsMultilineString3 stuff
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FKeywords: TStringList;
    FLastIdentifier: UnicodeString;
    FStringAttri: TSynHighlighterAttributes;
    FDocStringAttri: TSynHighlighterAttributes;
    FMultiLineStringAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FHexAttri: TSynHighlighterAttributes;
    FOctalAttri: TSynHighlighterAttributes;
    FFloatAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNonKeyAttri: TSynHighlighterAttributes;
    FSystemAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FCommentAttri: TSynHighlighterAttributes;
    FCodeCommentAttri: TSynHighlighterAttributes;
    FFunctionNameAttri: TSynHighlighterAttributes;
    FClassNameAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FErrorAttri: TSynHighlighterAttributes;
    FMatchingBraceAttri: TSynHighlighterAttributes;
    FUnbalancedBraceAttri: TSynHighlighterAttributes;
    FTempSpaceAttri: TSynHighlighterAttributes;
    FBlockOpenerRE: TRegEx;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure SymbolProc;
    procedure CRProc;
    procedure CommentProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SpaceProc;
    procedure PreStringProc;
    procedure StringProc;
    procedure String2Proc;
    procedure StringEndProc(EndChar: WideChar);
    procedure UnknownProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
    procedure GetKeywordIdentifiers(KeywordList: TStrings); virtual;
    procedure DispatchProc; virtual;
    property TokenID: TtkTokenKind read FTokenID;
  public
    class function GetCapabilities: TSynHighlighterCapabilities; override;
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    function GetKeyWords(TokenKind: Integer): UnicodeString; override;
    procedure InitFoldRanges(FoldRanges: TSynFoldRanges); override;
    procedure ScanForFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings; FromLine: Integer; ToLine: Integer); override;
    procedure AdjustFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings); override;
    function FlowControlAtLine(Lines: TStrings; Line: Integer): TSynFlowControl; override;
    property Keywords: TStringList read FKeywords;
  published
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property CodeCommentAttri: TSynHighlighterAttributes read FCodeCommentAttri
      write FCodeCommentAttri;
    property FunctionNameAttri: TSynHighlighterAttributes read FFunctionNameAttri
      write FFunctionNameAttri;
    property ClassNameAttri: TSynHighlighterAttributes read FClassNameAttri
      write FClassNameAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property NonKeyAttri: TSynHighlighterAttributes read FNonKeyAttri
      write FNonKeyAttri;
    property SystemAttri: TSynHighlighterAttributes read FSystemAttri
      write FSystemAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property HexAttri: TSynHighlighterAttributes read FHexAttri
      write FHexAttri;
    property OctalAttri: TSynHighlighterAttributes read FOctalAttri
      write FOctalAttri;
    property FloatAttri: TSynHighlighterAttributes read FFloatAttri
      write FFloatAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri
      write FStringAttri;
    property DocStringAttri: TSynHighlighterAttributes read FDocStringAttri
      write FDocStringAttri;
    property MultiLineStringAttri: TSynHighlighterAttributes read FMultiLineStringAttri
      write FMultiLineStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
    property ErrorAttri: TSynHighlighterAttributes read FErrorAttri
      write FErrorAttri;
    property MatchingBraceAttri: TSynHighlighterAttributes read FMatchingBraceAttri
      write FMatchingBraceAttri;
    property UnbalancedBraceAttri: TSynHighlighterAttributes read FUnbalancedBraceAttri
      write FUnbalancedBraceAttri;
  end;

  TSynPythonInterpreterSyn = class(TSynPythonSyn)
  private
    FPS1: string;
    FPS2: string;
    FDbg: string;
    FPM: string;
    FBannerAttri: TSynHighlighterAttributes;
    FOutputAttri: TSynHighlighterAttributes;
    FTracebackAttri: TSynHighlighterAttributes;
    FPromptAttri: TSynHighlighterAttributes;
    FSystemCmdAttri: TSynHighlighterAttributes;
    FTracebackStartRE: TRegEx;
    FTracebackEndRE: TRegEx;
    FSystemCmdRE: TRegEx;
    procedure BannerProc;
    procedure OutputProc;
    procedure TracebackProc;
    procedure PromptProc(Len: Integer);
    procedure SystemCmdProc;
  protected
    procedure DispatchProc; override;
    function GetSampleSource: UnicodeString; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
  published
    property BannerAttri: TSynHighlighterAttributes read FBannerAttri
      write FBannerAttri;
    property OutputAttri: TSynHighlighterAttributes read FOutputAttri
      write FOutputAttri;
    property TracebackAttri: TSynHighlighterAttributes read FTracebackAttri
      write FTracebackAttri;
    property PromptAttri: TSynHighlighterAttributes read FPromptAttri
      write FPromptAttri;
    property SystemCmdAttri: TSynHighlighterAttributes read FSystemCmdAttri
      write FSystemCmdAttri;
    property PS1: string read FPS1 write FPS1;
    property PS2: string read FPS2 write FPS2;
    property Dbg: string read FDbg write FDbg;
    property PM: string read FPM write FPM;
  end;

  TSynCythonSyn = class(TSynPythonSyn)
  protected
    procedure GetKeywordIdentifiers(KeywordList: TStrings); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddCythonKeywords(KeywordList: TStrings);
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  end;

const
  SYNS_CommentedCode = 'Commented Code';
  SYNS_FunctionName = 'Function Name';
  SYNS_ClassName = 'Class Name';
  SYNS_MatchingBrace = 'Matching Brace';
  SYNS_UnbalancedBrace = 'Unbalanced Brace';
  SYNS_MultiLineString = 'Multi-Line String';
  SYNS_FilterCython =  sCythonFileFilter;

resourcestring
  SYNS_FriendlyCommentedCode = 'Commented Code';
  SYNS_FriendlyFunctionName = 'Function Name';
  SYNS_FriendlyClassName = 'Class Name';
  SYNS_FriendlyMatchingBrace = 'Matching Brace';
  SYNS_FriendlyUnbalancedBrace = 'Unbalanced Brace';
  SYNS_FriendlyMultiLineString = 'Multi-Line String';

implementation

uses
  System.SysUtils,
  System.Character,
  Vcl.Graphics,
  SynEditStrConst,
  SynEditMiscProcs;

function TSynPythonSyn.GetKeyWords(TokenKind: Integer): UnicodeString;
begin
  Result := Keywords.CommaText;
end;

procedure TSynPythonSyn.GetKeywordIdentifiers(KeywordList: TStrings);
const
  // No need to localise keywords!

  // List of keywords
  KEYWORDCOUNT = 31;
  KEYWORDS: array [1..KEYWORDCOUNT] of UnicodeString =
    (
    'and',
    'as',
    'assert',
    'break',
    'class',
    'continue',
    'def',
    'del',
    'elif',
    'else',
    'except',
    'exec',
    'finally',
    'for',
    'from',
    'global',
    'if',
    'import',
    'in',
    'is',
    'lambda',
    'not',
    'or',
    'pass',
    'print',
    'raise',
    'return',
    'try',
    'with',
    'while',
    'yield'
    );

  // List of non-keyword identifiers
  NONKEYWORDCOUNT = 137;
  NONKEYWORDS: array [1..NONKEYWORDCOUNT] of UnicodeString =
    (
    'ArithmeticError',
    'AssertionError',
    'AttributeError',
    'BaseException',
    'DeprecationWarning',
    'EOFError',
    'Ellipsis',
    'EnvironmentError',
    'Exception',
    'False',
    'FloatingPointError',
    'FutureWarning',
    'GeneratorExit',
    'IOError',
    'ImportError',
    'ImportWarning',
    'IndentationError',
    'IndexError',
    'KeyError',
    'KeyboardInterrupt',
    'LookupError',
    'MemoryError',
    'NameError',
    'None',
    'NotImplemented',
    'NotImplementedError',
    'OSError',
    'OverflowError',
    'PendingDeprecationWarning',
    'ReferenceError',
    'RuntimeError',
    'RuntimeWarning',
    'StandardError',
    'StopIteration',
    'SyntaxError',
    'SyntaxWarning',
    'SystemError',
    'SystemExit',
    'TabError',
    'True',
    'TypeError',
    'UnboundLocalError',
    'UnicodeDecodeError',
    'UnicodeEncodeError',
    'UnicodeError',
    'UnicodeTranslateError',
    'UnicodeWarning',
    'UserWarning',
    'ValueError',
    'Warning',
    'WindowsError',
    'ZeroDivisionError',
    '_',
    '__debug__',
    '__doc__',
    '__future__',
    '__import__',
    '__name__',
    'abs',
    'all',
    'any',
    'apply',
    'basestring',
    'bool',
    'buffer',
    'callable',
    'chr',
    'classmethod',
    'cmp',
    'coerce',
    'compile',
    'complex',
    'copyright',
    'credits',
    'delattr',
    'dict',
    'dir',
    'divmod',
    'enumerate',
    'eval',
    'execfile',
    'exit',
    'file',
    'filter',
    'float',
    'frozenset',
    'getattr',
    'globals',
    'hasattr',
    'hash',
    'help',
    'hex',
    'id',
    'input',
    'int',
    'intern',
    'isinstance',
    'issubclass',
    'iter',
    'len',
    'license',
    'list',
    'locals',
    'long',
    'map',
    'max',
    'min',
    'object',
    'oct',
    'open',
    'ord',
    'pow',
    'property',
    'quit',
    'range',
    'raw_input',
    'reduce',
    'reload',
    'repr',
    'reversed',
    'round',
    'self',
    'set',
    'setattr',
    'slice',
    'sorted',
    'staticmethod',
    'str',
    'sum',
    'super',
    'tuple',
    'type',
    'unichr',
    'unicode',
    'vars',
    'xrange',
    'zip'
    );
var
  Keyword: string;
begin
  for Keyword in KEYWORDS do
    KeywordList.AddObject(Keyword, Pointer(Ord(tkKey)));
  for Keyword in NONKEYWORDS do
    KeywordList.AddObject(Keyword, Pointer(Ord(tkNonKeyword)));
end;

function TSynPythonSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Index: Integer;
  Temp: PWideChar;
  Key: UnicodeString;
begin
  // Extract the identifier out - it is assumed to terminate in a
  //   non-alphanumeric character
  fToIdent := MayBe;
  Temp := MayBe;
  while IsIdentChar(Temp^) do
    Inc(Temp);
  fStringLen := Temp - fToIdent;

  SetString(Key, fToIdent, fStringLen);

  // Check to see if it is a keyword
  if ((Run <= 0) or (fLine[Run-1]<> '.')) and (FLastIdentifier <> 'class') and
    (FLastIdentifier <> 'def') and FKeywords.Find(Key, Index)
  then
    Result := TtkTokenKind(FKeywords.Objects[Index])

  // Check if it is a class name
  else if FLastIdentifier = 'class' then
    Result := tkClassName

  // Check if it is a function name
  else if FLastIdentifier = 'def' then
    Result := tkFunctionName

  // Check if it is a system identifier (__*__)
  else if (fStringLen >= 5) and
     (MayBe[0] = '_') and (MayBe[1] = '_') and (MayBe[2] <> '_') and
     (MayBe[fStringLen - 1] = '_') and (MayBe[fStringLen - 2] = '_') and
     (MayBe[fStringLen - 3] <> '_') then
    Result := tkSystemDefined

  // Else, hey, it is an ordinary run-of-the-mill identifier!
  else
    Result := tkIdentifier;

  //FLastIdentifier := Key;
  SetString(FLastIdentifier, fToIdent, fStringLen);
end;

constructor TSynPythonSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := True;

  FKeywords := TStringList.Create;
  GetKeywordIdentifiers(FKeywords);
  FKeywords.CaseSensitive := True;
  FKeywords.Duplicates := dupIgnore;
  FKeywords.Sorted := True;

  FBlockOpenerRE := CompiledRegEx(
     '^(def|class|while|for|if|else|elif|try|except|finally|with|match|case'+
     '|(async[ \t]+def)|(async[ \t]+with)|(async[ \t]+for))\b');

  FRange := rsUnknown;
  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Foreground := clGray;
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);
  FCodeCommentAttri := TSynHighlighterAttributes.Create(SYNS_CommentedCode, SYNS_FriendlyCommentedCode);
  FCodeCommentAttri.Foreground := clSilver;
  FCodeCommentAttri.Style := [fsItalic];
  AddAttribute(FCodeCommentAttri);
  FFunctionNameAttri := TSynHighlighterAttributes.Create(SYNS_FunctionName, SYNS_FriendlyFunctionName);
  FFunctionNameAttri.Foreground := clTeal;
  FFunctionNameAttri.Style := [fsBold];
  AddAttribute(FFunctionNameAttri);
  FClassNameAttri := TSynHighlighterAttributes.Create(SYNS_ClassName, SYNS_FriendlyClassName);
  FClassNameAttri.Foreground := clNavy;
  FClassNameAttri.Style := [fsBold];
  AddAttribute(FClassNameAttri);
  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  FIdentifierAttri.Foreground := clBlack;
  AddAttribute(FIdentifierAttri);
  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Foreground := clNavy;
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);
  FNonKeyAttri := TSynHighlighterAttributes.Create (SYNS_AttrNonReservedKeyword, SYNS_FriendlyAttrNonReservedKeyword);
  FNonKeyAttri.Foreground := clNavy;
  AddAttribute (FNonKeyAttri);
  FSystemAttri := TSynHighlighterAttributes.Create (SYNS_AttrSystem, SYNS_FriendlyAttrSystem);
  FSystemAttri.Style := [fsBold];
  AddAttribute (FSystemAttri);
  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := clTeal;
  AddAttribute(FNumberAttri);
  FHexAttri := TSynHighlighterAttributes.Create(SYNS_AttrHexadecimal, SYNS_FriendlyAttrHexadecimal);
  FHexAttri.Foreground := clTeal;
  AddAttribute(FHexAttri);
  FOctalAttri := TSynHighlighterAttributes.Create(SYNS_AttrOctal, SYNS_FriendlyAttrOctal);
  FOctalAttri.Foreground := clTeal;
  AddAttribute(FOctalAttri);
  FFloatAttri := TSynHighlighterAttributes.Create(SYNS_AttrFloat, SYNS_FriendlyAttrFloat);
  FFloatAttri.Foreground := clTeal;
  AddAttribute(FFloatAttri);
  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  FSpaceAttri.Background := clWhite;
  FSpaceAttri.Foreground := clSilver;
  AddAttribute(FSpaceAttri);
  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Foreground := clOlive;
  AddAttribute(FStringAttri);
  FDocStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrDocumentation, SYNS_FriendlyAttrDocumentation);
  FDocStringAttri.Foreground := $FF00CC;
  AddAttribute(FDocStringAttri);
  FMultiLineStringAttri := TSynHighlighterAttributes.Create(SYNS_MultiLineString, SYNS_FriendlyMultiLineString);
  FMultiLineStringAttri.Foreground := clOlive;
  AddAttribute(FMultiLineStringAttri);
  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  FSymbolAttri.Foreground := clMaroon;
  AddAttribute(FSymbolAttri);
  FErrorAttri := TSynHighlighterAttributes.Create(SYNS_AttrSyntaxError, SYNS_FriendlyAttrSyntaxError);
  FErrorAttri.Foreground := clRed;
  AddAttribute(FErrorAttri);
  FMatchingBraceAttri := TSynHighlighterAttributes.Create(SYNS_MatchingBrace, SYNS_FriendlyMatchingBrace);
  FMatchingBraceAttri.Foreground := clBlue;
  FMatchingBraceAttri.Style := [fsBold];
  AddAttribute(FMatchingBraceAttri);
  FUnbalancedBraceAttri := TSynHighlighterAttributes.Create(SYNS_UnbalancedBrace, SYNS_FriendlyUnbalancedBrace);
  FUnbalancedBraceAttri.Background := clRed;
  FUnbalancedBraceAttri.Foreground := clYellow;
  AddAttribute(FUnbalancedBraceAttri);

  SetAttributesOnChange(DefHighlightChange);
  fDefaultFilter := SYNS_FilterPython;
  // for coloring doc comment background
  FTempSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
end; { Create }

destructor TSynPythonSyn.Destroy;
begin
  FKeywords.Free;
  FTempSpaceAttri.Free;
  inherited;
end;

procedure TSynPythonSyn.SymbolProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynPythonSyn.CRProc;
begin
  FTokenID := tkSpace;
  case fLine[Run + 1] of
    #10: Inc(Run, 2);
  else
    Inc(Run);
  end;
end;

procedure TSynPythonSyn.AdjustFoldRanges(FoldRanges: TSynFoldRanges;
  LinesToScan: TStrings);
var
  I: Integer;
begin
  inherited;
  for I := 0 to FoldRanges.Count - 1 do
    with FoldRanges.Ranges.List[I] do
      if FoldType <> Integer(pftCodeBlock) then
        Indent := 0;
end;

procedure TSynPythonSyn.CommentProc;
begin
  Inc(Run);
  if fLine[Run] = '#' then
    FTokenID := tkCodeComment
  else
    FTokenID := tkComment;
  while not IsLineEnd(Run) do
    Inc(Run);
end;

procedure TSynPythonSyn.GreaterProc;
begin
  case fLine[Run + 1] of
    '=': begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
  else begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPythonSyn.IdentProc;
begin
  FTokenID := IdentKind((fLine + Run));
  Inc(Run, fStringLen);
end;

procedure TSynPythonSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynPythonSyn.LowerProc;
begin
  case fLine[Run + 1] of
    '=': begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
    '>': begin
        Inc(Run, 2);
        FTokenID := tkSymbol;
      end;
  else
    begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPythonSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynPythonSyn.NumberProc;
type
  TNumberState =
    (
    nsStart,
    nsDotFound,
    nsHex,
    nsOct,
    nsBinary,
    nsExpFound
    );

var
  CurrChar: WideChar;
  State: TNumberState;

  function CheckSpecialCases: Boolean;
  begin
    case CurrChar of
      // Look for dot (.)
      '.': begin
        // .45
        if CharInSet(fLine[Run], ['0'..'9']) then
        begin
          Inc(Run);
          FTokenID := tkFloat;
          State := nsDotFound;

        // Non-number dot
        end else begin
          // Ellipsis
          if (fLine[Run] = '.') and (fLine[Run+1] = '.') then
            Inc (Run, 2);
          FTokenID := tkSymbol;
          Result := False;
          Exit;
        end; // if
      end; // DOT

      // Look for zero (0)
      '0': begin
        CurrChar := fLine[Run];
        // 0x123ABC
        if CharInSet(CurrChar, ['x', 'X']) then begin
          Inc (Run);
          FTokenID := tkHex;
          State := nsHex;
        // 0o123
        end else if CharInSet(CurrChar, ['o', 'O']) then begin
          Inc (Run);
          FTokenID := tkOct;
          State := nsOct;
        // 0b1010
        end else if CharInSet(CurrChar, ['b', 'B']) then begin
          Inc (Run);
          FTokenID := tkOct; //paint same as octal
          State := nsBinary;
        // 0.45
        end else if CurrChar = '.' then begin
          Inc (Run);
          State := nsDotFound;
          FTokenID := tkFloat;
        end; // if
      end; // ZERO
    end; // case

    Result := True;
  end; // CheckSpecialCases

  function HandleBadNumber: Boolean;
  begin
    Result := False;
    FTokenID := tkUnknown;
    // Ignore all tokens till end of "number"
    while IsIdentChar(fLine[Run]) or (fLine[Run] = '.') do
      Inc (Run);
  end; // HandleBadNumber

  function HandleExponent: Boolean;
  begin
    State := nsExpFound;
    FTokenID := tkFloat;
    // Skip e[+/-]
    if CharInSet(fLine[Run+1], ['+', '-']) then
      Inc (Run);
    // Invalid token: 1.0e
    if not CharInSet(fLine[Run+1], ['0'..'9']) then begin
      Inc (Run);
      Result := HandleBadNumber;
      Exit;
    end; // if

    Result := True;
  end; // HandleExponent

  function HandleDot: Boolean;
  begin
    // Check for ellipsis
    Result := (fLine[Run+1] <> '.') or (fLine[Run+2] <> '.');
    if Result then begin
      State := nsDotFound;
      FTokenID := tkFloat;
    end; // if
  end; // HandleDot

  function CheckStart: Boolean;
  begin
    // Allow underscores inside the number
    if CurrChar = '_' then begin
      if CharInSet(fLine[Run + 1], ['0'..'9']) then
        Result := True
      else
        Result := HandleBadNumber;
    // 1234
    end else if CharInSet(CurrChar, ['0'..'9']) then begin
      Result := True;
    //123e4
    end else if CharInSet(CurrChar, ['e', 'E']) then begin
      Result := HandleExponent;
    // 123.45j
    end else if CharInSet(CurrChar, ['j', 'J']) then begin
      Inc (Run);
      FTokenID := tkFloat;
      Result := False;
    // 123.45
    end else if CurrChar = '.' then begin
      Result := HandleDot;
    // Error!
    end else if IsIdentChar(CurrChar) then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckStart

  function CheckDotFound: Boolean;
  begin
    // Allow underscores inside the number
    if CurrChar = '_' then begin
      if CharInSet(fLine[Run - 1], ['0'..'9']) and
        CharInSet(fLine[Run + 1], ['0'..'9'])
      then
        Result := True
      else
        Result := HandleBadNumber;
    // 1.0e4
    end else if CharInSet(CurrChar, ['e', 'E']) then begin
      Result := HandleExponent;
    // 123.45
    end else if CharInSet(CurrChar, ['0'..'9']) then begin
      Result := True;
    // 123.45j
    end else if CharInSet(CurrChar, ['j', 'J']) then begin
      Inc (Run);
      Result := False;
    // 123.45.45: Error!
    end else if CurrChar = '.' then begin
      Result := False;
      if HandleDot then
        HandleBadNumber;
    // Error!
    end else if IsIdentChar(CurrChar) then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckDotFound

  function CheckSpecialInt(ValidChars: TSysCharSet): Boolean;
  begin
    // Allow underscores inside the number
    if CurrChar = '_' then begin
      if CharInSet(fLine[Run - 1], ValidChars) and
        CharInSet(fLine[Run + 1], ValidChars)
      then
        Result := True
      else
        Result := HandleBadNumber;
    end else if CharInSet(CurrChar, ValidChars) then
    begin
      Result := True;
    end else if CharInSet(CurrChar, ['l', 'L']) then begin
      Inc (Run);
      Result := False;
    end else if CurrChar = '.' then begin
      Result := False;
      if HandleDot then
        HandleBadNumber;
    end else if IsIdentChar(CurrChar) then begin
      Result := HandleBadNumber;
    end else begin
      Result := False;
    end; // if
  end; // CheckHex

  function CheckExpFound: Boolean;
  begin
    // Allow underscores inside the number
    if CurrChar = '_' then begin
      if CharInSet(fLine[Run - 1], ['0'..'9']) and
        CharInSet(fLine[Run + 1], ['0'..'9'])
      then
        Result := True
      else
        Result := HandleBadNumber;
    // 1e+123
    end else if CharInSet(CurrChar, ['0'..'9']) then begin
      Result := True;
    // 1e+123j
    end else if CharInSet(CurrChar, ['j', 'J']) then begin
      Inc (Run);
      Result := False;
    // 1e4.5: Error!
    end else if CurrChar = '.' then begin
      Result := False;
      if HandleDot then
        HandleBadNumber;
    // Error!
    end else if IsIdentChar(CurrChar) then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckExpFound

begin
  State := nsStart;
  FTokenID := tkNumber;

  CurrChar := fLine[Run];
  Inc (Run);

  // Special cases
  if not CheckSpecialCases then
    Exit;

  // Use a state machine to parse numbers
  while True do begin
    CurrChar := fLine[Run];

    case State of
      nsStart:
        if not CheckStart then Exit;
      nsDotFound:
        if not CheckDotFound then Exit;
      nsHex:
        if not CheckSpecialInt(['a'..'f', 'A'..'F', '0'..'9']) then Exit;
      nsOct:
        if not CheckSpecialInt(['0'..'7']) then Exit;
      nsBinary:
        if not CheckSpecialInt(['0'..'1']) then Exit;
      nsExpFound:
        if not CheckExpFound then Exit;
    end; // case

    Inc (Run);
  end; // while
end;

procedure TSynPythonSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (fLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynPythonSyn.String2Proc;
var
  BackslashCount: Integer;
begin
  FTokenID := tkString;
  if (fLine[Run + 1] = '"') and (fLine[Run + 2] = '"') then
  begin
    FTokenID := tkTrippleQuotedString2;
    Inc(Run, 3);

    FRange := rsMultilineString2;
    while fLine[Run] <> #0 do
    begin
      case fLine[Run] of

        '\':begin
               { If we're looking at a backslash, and the following character is an
               end quote, and it's preceeded by an odd number of backslashes, then
               it shouldn't mark the end of the string.  If it's preceeded by an
               even number, then it should. !!!THIS RULE DOESNT APPLY IN RAW STRINGS}
               if fLine[Run + 1] = '"' then
                 begin
                   BackslashCount := 1;

                   while ((Run > BackslashCount) and (fLine[Run - BackslashCount] = '\')) do
                     BackslashCount := BackslashCount + 1;

                   if (BackslashCount mod 2 = 1) then Inc(Run);
               end;
               Inc(Run);
            end;// '\':

        '"':
          if (fLine[Run + 1] = '"') and (fLine[Run + 2] = '"') then begin
            FRange := rsUnknown;
            Inc(Run, 3);
            Exit;
          end else
            Inc(Run);
        #10: Exit;
        #13: Exit;
        else
          Inc(Run);
      end;
    end;
  end
  else //if short string
  repeat
    case fLine[Run] of
      #0, #10, #13:
        begin
          if fLine[Run-1] = '\' then
          begin
            FStringStarter := '"';
            FRange := rsMultilineString3;
          end;
          Break;
        end;
      {The same backslash stuff above...}
      '\':begin
             if fLine[Run + 1] = '"' then
               begin
                 BackslashCount := 1;

                 while ((Run > BackslashCount) and (fLine[Run - BackslashCount] = '\')) do
                   BackslashCount := BackslashCount + 1;

                 if (BackslashCount mod 2 = 1) then Inc(Run);
             end;
             Inc(Run);
          end;// '\':

      else Inc(Run);
    end; //case
  until (fLine[Run] = '"');
  if fLine[Run] <> #0 then Inc(Run);
end;

procedure TSynPythonSyn.PreStringProc;
// Handle string prefixes
// Valid prefixes: u, b, r, f, br, rb, rf, fr possibly capitalized
var
  PrefixLen: Integer;
begin
  PrefixLen := 0;
  case fLine[Run] of
    'u', 'U':
        if CharInSet(fLine[Run + 1], ['"', '''']) then
          PrefixLen := 1;
    'b', 'B':
      case fLine[Run + 1] of
        '"', '''': PrefixLen := 1;
        'r', 'R':
            if CharInSet(fLine[Run + 2], ['"', '''']) then
              PrefixLen := 2;
      end;
    'r', 'R':
      case fLine[Run + 1] of
        '"', '''': PrefixLen := 1;
        'b', 'B', 'f', 'F':
            if CharInSet(fLine[Run + 2], ['"', '''']) then
              PrefixLen := 2;
      end;
    'f', 'F':
      case fLine[Run + 1] of
        '"', '''': PrefixLen := 1;
        'r', 'R':
            if CharInSet(fLine[Run + 2], ['"', '''']) then
              PrefixLen := 2;
      end;
  end;

  if PrefixLen > 0 then
  begin
    Inc(Run, PrefixLen);
    if fLine[Run] = '"' then
      String2Proc
    else
      StringProc;
  end
  else
    IdentProc;
end;

procedure TSynPythonSyn.StringProc;
var
  BackslashCount: Integer;
begin
  FTokenID := tkString;
  if (fLine[Run + 1] = #39) and (fLine[Run + 2] = #39) then begin
    FTokenID := tkTrippleQuotedString;
    Inc(Run, 3);

    FRange := rsMultilineString;
    while fLine[Run] <> #0 do begin
      case fLine[Run] of

        '\': begin
             { If we're looking at a backslash, and the following character is an
             end quote, and it's preceeded by an odd number of backslashes, then
             it shouldn't mark the end of the string.  If it's preceeded by an
             even number, then it should. !!!THIS RULE DOESNT APPLY IN RAW STRINGS}
              if fLine[Run + 1] = #39 then
                begin
                  BackslashCount := 1;

                  while ((Run > BackslashCount) and (fLine[Run - BackslashCount] = '\')) do
                    BackslashCount := BackslashCount + 1;

                  if (BackslashCount mod 2 = 1) then Inc(Run);
              end;
              Inc(Run);
            end;// '\':

        #39:
          if (fLine[Run + 1] = #39) and (fLine[Run + 2] = #39) then begin
            FRange := rsUnknown;
            Inc(Run, 3);
            Exit;
          end else
            Inc(Run);
        #10: Exit;
        #13: Exit;
        else
          Inc(Run);
      end;
    end;
  end
      else //if short string
  repeat
    case fLine[Run] of
      #0, #10, #13: begin
        if fLine[Run-1] = '\' then begin
          FStringStarter := #39;
          FRange := rsMultilineString3;
        end;
        Break;
        end;

      {The same backslash stuff above...}
      '\':begin
             if fLine[Run + 1] = #39 then
               begin
                 BackslashCount := 1;

                 while ((Run > BackslashCount) and (fLine[Run - BackslashCount] = '\')) do
                   BackslashCount := BackslashCount + 1;

                 if (BackslashCount mod 2 = 1) then Inc(Run);
             end;
             Inc(Run);
          end;// '\':

      else Inc(Run);
    end; //case
  until (fLine[Run] = #39);
  if fLine[Run] <> #0 then Inc(Run);
end;

procedure TSynPythonSyn.StringEndProc(EndChar: WideChar);
var
  BackslashCount: Integer;
begin
  if FRange = rsMultilineString3 then
    FTokenID := tkString
  else if EndChar = '"' then
    FTokenID := tkTrippleQuotedString2
  else
    FTokenID := tkTrippleQuotedString;

  case fLine[Run] of
    #0:
      begin
        NullProc;
        Exit;
      end;
    #10:
      begin
        LFProc;
        Exit;
    end;
    #13:
      begin
        CRProc;
        Exit;
      end;
  end;

  if FRange = rsMultilineString3 then begin
    repeat
      if fLine[Run]=FStringStarter then begin
        Inc(Run);
        FRange:=rsUnknown;
        Exit;
      end else if fLine[Run]='\' then   {The same backslash stuff above...}
      begin
         if fLine[Run + 1] = FStringStarter then
           begin
             BackslashCount := 1;

             while ((Run >= BackslashCount) and (fLine[Run - BackslashCount] = '\')) do
               BackslashCount := BackslashCount + 1;

             if (BackslashCount mod 2 = 1) then Inc(Run);
         end;
       end;// if fLine[Run]...

      Inc(Run);
    until IsLineEnd(Run);
    if fLine[Run-1]<>'\' then begin
      FRange := rsUnknown;
      Exit;
    end;
  end else
  repeat
    if (fLine[Run]=EndChar) and (fLine[Run+1]=EndChar) and (fLine[Run+2]=EndChar) then begin
      Inc(Run,3);
      FRange := rsUnknown;
      Exit;
    end;
    if fLine[Run] = '\' then begin
      if fLine[Run + 1] = EndChar then begin
          BackslashCount := 1;

          while ((Run >= BackslashCount) and (fLine[Run - BackslashCount] = '\')) do
            BackslashCount := BackslashCount + 1;

          if (BackslashCount mod 2 = 1) then Inc(Run);
      end;
    end;
    Inc(Run);
  until IsLineEnd(Run);
end;

procedure TSynPythonSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynPythonSyn.DispatchProc;
begin
  case FRange of
    rsMultilineString:
      StringEndProc(#39);
    rsMultilineString2:
      StringEndProc('"');
    rsMultilineString3:
      StringEndProc(FStringStarter);
    else
      case fLine[Run] of
        '&', '}', '{', ':', ',', ']', '[', '*', '`',
        '^', ')', '(', ';', '/', '=', '-', '+', '!', '\',
        '%', '|', '~', '@' :
          SymbolProc;
        #13: CRProc;
        '#': CommentProc;
        '>': GreaterProc;
        'A', 'C'..'E', 'G'..'Q', 'S', 'T', 'V'..'Z', 'a', 'c'..'e', 'g'..'q', 's', 't', 'v'..'z', '_': IdentProc;
        #10: LFProc;
        '<': LowerProc;
        #0: NullProc;
        '.', '0'..'9': NumberProc;
        #1..#9, #11, #12, #14..#32: SpaceProc;
        'r', 'R', 'u', 'U', 'b', 'B', 'f', 'F': PreStringProc;
        '''': StringProc;
        '"': String2Proc;
        else if IsIdentChar(fLine[Run]) then IdentProc
        else UnknownProc;
      end;
  end;
end;

function TSynPythonSyn.FlowControlAtLine(Lines: TStrings;
  Line: Integer): TSynFlowControl;
var
  SLine: string;
  Index: Integer;
begin
  Result := fcNone;

  SLine := Lines[Line - 1];

  Index :=  SLine.IndexOf('continue');
  if Index >= 0 then
    Result := fcContinue
  else
  begin
    Index :=  SLine.IndexOf('break');
    if Index >= 0 then
      Result := fcBreak
    else
    begin
      Index :=  SLine.IndexOf('return');
      if Index >= 0 then
        Result := fcExit
      else
      begin
        Index :=  SLine.IndexOf('yield');
        if Index >= 0 then
          Result := fcExit;
      end;
    end;
  end;

  // Index is 0-based
  if (Index >= 0) and
    not (GetHighlighterAttriAtRowCol(Lines, Line - 1, Index + 1) = KeyAttri)
  then
    Result := fcNone;
end;

procedure TSynPythonSyn.Next;
begin
  fTokenPos := Run;
  DispatchProc;
  inherited;
end;

class function TSynPythonSyn.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcStructureHighlight];
end;

function TSynPythonSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_WHITESPACE:
      begin
        // To allow background coloring of doc strings
        FTempSpaceAttri.Assign(FSpaceAttri);
        if (FRange = rsMultilineString) and (FMultiLineStringAttri.Background <> clNone) then
          FTempSpaceAttri.Background := FMultiLineStringAttri.Background
        else if (FRange = rsMultilineString2) and (FDocStringAttri.Background <> clNone)  then
          FTempSpaceAttri.Background := FDocStringAttri.Background;
        Result := FTempSpaceAttri;
      end;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
    SYN_ATTR_STRING:
      begin
        if (FRange = rsMultilineString) then
          Result := FMultiLineStringAttri
        else if (FRange = rsMultilineString2) then
          Result := FDocStringAttri
        else
          Result := FStringAttri;
      end;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynPythonSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynPythonSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynPythonSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynPythonSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkCodeComment: Result := FCodeCommentAttri;
    tkFunctionName: Result := FFunctionNameAttri;
    tkClassName: Result := FClassNameAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNonKeyword: Result := FNonKeyAttri;
    tkSystemDefined: Result := FSystemAttri;
    tkNumber: Result := FNumberAttri;
    tkHex: Result := FHexAttri;
    tkOct: Result := FOctalAttri;
    tkFloat: Result := FFloatAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkTrippleQuotedString: Result := FMultiLineStringAttri;
    tkTrippleQuotedString2: Result := FDocStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FErrorAttri;
  else
    Result := nil;
  end;
end;

function TSynPythonSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

procedure TSynPythonSyn.InitFoldRanges(FoldRanges: TSynFoldRanges);
begin
  inherited;
  FoldRanges.CodeFoldingMode := cfmIndentation;
end;

procedure TSynPythonSyn.ScanForFoldRanges(FoldRanges: TSynFoldRanges;
  LinesToScan: TStrings; FromLine, ToLine: Integer);
var
  CurLine: string;
  LeftTrimmedLine: string;
  Line: Integer;
  Indent: Integer;
  TabW: Integer;
  FoldType: Integer;

  function IsMultiLineString(Line: Integer; Range: TRangeState; Fold: Boolean): Boolean;
  begin
    Result := True;
    if TRangeState(GetLineRange(LinesToScan, Line)) = Range then
    begin
      if (TRangeState(GetLineRange(LinesToScan, Line - 1)) <> Range) and Fold then
        FoldRanges.StartFoldRange(Line + 1, Integer(pftMultiLineStringFoldType))
      else
        FoldRanges.NoFoldInfo(Line + 1);
    end
    else if (TRangeState(GetLineRange(LinesToScan, Line - 1)) = Range) and Fold then
    begin
      FoldRanges.StopFoldRange(Line + 1, Integer(pftMultiLineStringFoldType));
    end else
      Result := False;
  end;

  function FoldRegion(Line: Integer): Boolean;
  begin
    Result := False;
    if UpperCase(Copy(LeftTrimmedLine, 1, 7)) = '#REGION' then
    begin
      FoldRanges.StartFoldRange(Line + 1, FoldRegionType);
      Result := True;
    end
    else if UpperCase(Copy(LeftTrimmedLine, 1, 10)) = '#ENDREGION' then
    begin
      FoldRanges.StopFoldRange(Line + 1, FoldRegionType);
      Result := True;
    end;
  end;

begin
  //  Deal with multiline strings
  for Line := FromLine to ToLine do begin
    if IsMultiLineString(Line, rsMultilineString, True) or
       IsMultiLineString(Line, rsMultilineString2, True) or
       IsMultiLineString(Line, rsMultilineString3, False)
    then
      Continue;

    // Find Fold regions
    CurLine := LinesToScan[Line];
    LeftTrimmedLine := TrimLeft(CurLine);

    // Skip empty lines
    if LeftTrimmedLine = '' then begin
      FoldRanges.NoFoldInfo(Line + 1);
      Continue;
    end;

    // Skip code comments
    if Copy(CurLine, 1, 2) = '##' then begin
      FoldRanges.NoFoldInfo(Line + 1);
      Continue;
    end;

    // Find Fold regions
    if FoldRegion(Line) then
      Continue;

    TabW := TabWidth(LinesToScan);
    Indent := LeftSpaces(CurLine, True, TabW);

    // find fold openers
    with FBlockOpenerRE.Match(LeftTrimmedLine) do
      if Success then
      begin
        if Groups[1].Value = 'class' then
          FoldType := Integer(pftClassDefType)
        else if Pos('def', Groups[1].Value) >= 1 then
          FoldType := Integer(pftFunctionDefType)
        else
          FoldType := Integer(pftCodeBlock);

        FoldRanges.StartFoldRange(Line + 1, FoldType, Indent);
        Continue;
      end;

    FoldRanges.StopFoldRange(Line + 1, 1, Indent);
  end;
end;

procedure TSynPythonSyn.ResetRange;
begin
  FRange := rsUnknown;
  FLastIdentifier := '';
end;

procedure TSynPythonSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynPythonSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterPython;
end;

function TSynPythonSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  Result := AChar.IsLetterOrDigit or (AChar = '_');
end;

class function TSynPythonSyn.GetLanguageName: string;
begin
  Result := SYNS_LangPython;
end;

function TSynPythonSyn.GetSampleSource: UnicodeString;
begin
  Result :=
    '#!/usr/local/bin/python'#13#10 +
    '__version__ = "$Revision: 01 $"'#13#10 +
    'import string, sys'#13#10 +
    'class Conversions:'#13#10 +
    '"""A collection of conversion functions"""'#13#10 +
    '    if len(sys.argv) < 1:'#13#10 +
    '        sys.exit(2)'#13#10 +
    '        assert 0x00 == 00    #well, it better'#13#10+
    ''#13#10+
    'if __name__ == ''__main__'''#13#10+
    '    main()';
end;

class function TSynPythonSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangPython;
end;

{ TSynPythonInterpreterSyn }

resourcestring
  SYNS_AttrBanner = 'Banner';
  SYNS_FriendlyAttrBanner = 'Banner';
  SYNS_AttrOutput = 'Output';
  SYNS_FriendlyAttrOutput = 'Output';
  SYNS_AttrTraceback = 'Traceback';
  SYNS_FriendlyAttrTraceback = 'Traceback';
  SYNS_AttrPrompt = 'Prompt';
  SYNS_FriendlyAttrPrompt = 'Prompt';
  SYNS_AttrSystemCmd = 'System Command';
  SYNS_FriendlyAttrSystemCmd = 'System Command';
  SYNS_LangCython = 'Cython';
  SYNS_FriendlyLangCython = 'Cython';

const
  // Do not localise
  SYNS_LangPythonInterpreter = 'Python Interpreter';
  SYNS_FriendlyLangPythonInterpreter = 'Python Interpreter';

procedure TSynPythonInterpreterSyn.BannerProc;
begin
  Inc(Run);
  FTokenID := tkBanner;
  while not IsLineEnd(Run) do
    Inc(Run);
end;

constructor TSynPythonInterpreterSyn.Create(AOwner: TComponent);
begin
  inherited;
  FPS1 := '>>>';
  FPS2 := '...';
  FDbg := '[Dbg]';
  FPM := '[PM]';
  FBannerAttri := TSynHighlighterAttributes.Create(SYNS_AttrBanner, SYNS_FriendlyAttrBanner);
  FBannerAttri.Foreground := clBlue;
  AddAttribute(FBannerAttri);
  FOutputAttri := TSynHighlighterAttributes.Create(SYNS_AttrOutput, SYNS_FriendlyAttrOutput);
  FOutputAttri.Foreground := clTeal;
  AddAttribute(FOutputAttri);
  FTracebackAttri := TSynHighlighterAttributes.Create(SYNS_AttrTraceback, SYNS_FriendlyAttrTraceback);
  FTracebackAttri.Foreground := clRed;
  AddAttribute(FTracebackAttri);
  FPromptAttri := TSynHighlighterAttributes.Create(SYNS_AttrPrompt, SYNS_FriendlyAttrPrompt);
  FPromptAttri.Foreground := clGreen;
  AddAttribute(FPromptAttri);
  FSystemCmdAttri := TSynHighlighterAttributes.Create(SYNS_AttrSystemCmd, SYNS_FriendlyAttrSystemCmd);
  FSystemCmdAttri.Foreground := clFuchsia;
  AddAttribute(FSystemCmdAttri);
  FTracebackStartRE := CompiledRegEx('^Traceback \(|File ".*line');
  FTracebackEndRE := CompiledRegEx('^\w*(Error|Exception|Warning|KeyboardInterrupt):');
  FSystemCmdRE := CompiledRegEx(Format('^(%s)?%s\s*!', [FDbg, FPS1]));

  SetAttributesOnChange(DefHighlightChange);
end;

procedure TSynPythonInterpreterSyn.DispatchProc;
var
  Line, Prompt: string;
begin
  Line := fLineStr;
  Prompt := '';
  if Line.StartsWith(FPS1) then
    Prompt := FPS1
  else if Line.StartsWith(FPS2) then
    Prompt := FPS2
  else if Line.StartsWith(FDbg + FPS1) then
    Prompt := FDbg + FPS1
  else if Line.StartsWith(FDbg + FPS2) then
    Prompt := FDbg + FPS2
  else if Line.StartsWith(FPM + FPS1) then
    Prompt := FPM + FPS1
  else if Line.StartsWith(FPM + FPS2) then
    Prompt := FPM + FPS2;

  if (Prompt <> '') then begin
    if FRange = rsTraceback then
      FRange := rsUnknown;
    if Run < Length(Prompt) then
      PromptProc(Length(Prompt))
    else if FSystemCmdRE.IsMatch(Line) then
       SystemCmdProc
    else
      inherited; //Normal Python syntax
  end else if Line.StartsWith('***') then
    BannerProc
  else if FRange = rsTraceback then begin
    TracebackProc;
    if FTracebackEndRE.IsMatch(fLineStr) then
      FRange := rsUnknown;
  end else if (fLineLen < 100) and FTracebackStartRE.IsMatch(fLineStr) then begin
    FRange := rsTraceback;
    TracebackProc;
  end else
    OutputProc;
end;

class function TSynPythonInterpreterSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangPythonInterpreter;
end;

class function TSynPythonInterpreterSyn.GetLanguageName: string;
begin
  Result := SYNS_LangPythonInterpreter;
end;

function TSynPythonInterpreterSyn.GetSampleSource: UnicodeString;
begin
  Result :=
    '*** Python 3.6.4 (v3.6.4:d48eceb, Dec 19 2017, 06:54:40) [MSC v.1900 64 bit (AMD64)] on win32. ***'#13#10 +
    '>>> def Greet(person):'#13#10 +
    '...     return "Hi "+ person'#13#10 +
    '>>> Greet("John")'#13#10 +
    'Hi John'#13#10 +
    '>>> errorvar = 1/0'#13#10 +
    'Traceback (most recent call last):'#13#10 +
    '  File "<interactive input>", line 1, in <module>'#13#10 +
    'ZeroDivisionError: integer division or modulo by zero'#13#10 +
    '>>> class Conversions:'#13#10 +
    '... """A collection of conversion functions"""'#13#10 +
    '...    if len(sys.argv) < 1:'#13#10 +
    '...        sys.exit(2)'#13#10 +
    '...        assert 0x00 == 00    #well, it better';
end;

function TSynPythonInterpreterSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkBanner: Result := FBannerAttri;
    tkOutput: Result := FOutputAttri;
    tkTraceback: Result := FTracebackAttri;
    tkPrompt: Result := FPromptAttri;
    tkSystemCmd: Result := FSystemCmdAttri;
  else
    Result := inherited GetTokenAttribute;
  end;
end;

procedure TSynPythonInterpreterSyn.PromptProc(Len: Integer);
begin
  Inc(Run);
  FTokenID := tkPrompt;
  while Run < Len do
    Inc(Run);
end;

procedure TSynPythonInterpreterSyn.SystemCmdProc;
begin
  Inc(Run);
  FTokenID := tkSystemCmd;
  while not IsLineEnd(Run) do
    Inc(Run);
end;

procedure TSynPythonInterpreterSyn.OutputProc;
begin
  Inc(Run);
  FTokenID := tkOutput;
  while not IsLineEnd(Run) do
    Inc(Run);
end;

procedure TSynPythonInterpreterSyn.TracebackProc;
begin
  Inc(Run);
  FTokenID := tkTraceback;
  while not IsLineEnd(Run) do
    Inc(Run);
end;

{ TSynCythonSyn }

constructor TSynCythonSyn.Create(AOwner: TComponent);
begin
  inherited;
  fDefaultFilter := SYNS_FilterCython;
end;

class function TSynCythonSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangCython;
end;

procedure TSynCythonSyn.AddCythonKeywords(KeywordList: TStrings);
const
  // No need to localise keywords!

  // List of keywords
  KEYWORDCOUNT = 7;
  KEYWORDS: array [1..KEYWORDCOUNT] of UnicodeString =
    (
      'cdef',
      'ctypedef',
      'cpdef',
      'inline',
      'cimport',
      'include',
      'DEF'
    );

  // List of non-keyword identifiers
  NONKEYWORDCOUNT = 13;
  NONKEYWORDS: array [1..NONKEYWORDCOUNT] of UnicodeString =
    (
      'bool',
      'char',
      'double',
      'enum',
      'float',
      'int',
      'long',
      'mutable',
      'short',
      'signed',
      'struct',
      'unsigned',
      'void'
    );
var
  Keyword: string;
begin
  for Keyword in KEYWORDS do
    KeywordList.AddObject(Keyword, Pointer(Ord(tkKey)));
  for Keyword in NONKEYWORDS do
    KeywordList.AddObject(Keyword, Pointer(Ord(tkNonKeyword)));
end;

procedure TSynCythonSyn.GetKeywordIdentifiers(KeywordList: TStrings);
begin
    inherited;
    AddCythonKeywords(KeywordList);
end;

class function TSynCythonSyn.GetLanguageName: string;
begin
  Result := SYNS_LangCython;
end;

initialization
  RegisterPlaceableHighlighter(TSynPythonSyn);
  RegisterPlaceableHighlighter(TSynCythonSyn);
end.

