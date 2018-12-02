{-----------------------------------------------------------------------------
 Unit Name: cPythonSourceScanner
 Author:    Kiriakos Vlahos
 Date:      14-Jun-2005
 Purpose:   Class for Scanning and analysing Python code
            Does not check correctness
            Code draws from Bicycle Repair Man and Boa Constructor
 History:
-----------------------------------------------------------------------------}
unit cPythonSourceScanner;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Contnrs,
  System.RegularExpressions,
  System.Threading;

Type
  TParsedModule = class;

  TCodePos = record
    LineNo : integer;
    CharOffset : integer;
  end;

  TBaseCodeElement = class
    // abstract base class
  private
    fParent : TBaseCodeElement;
  protected
    fIsProxy : boolean;
    fCodePos : TCodePos;
    function GetCodeHint : string; virtual; abstract;
  public
    Name : string;
    function GetRoot : TBaseCodeElement;
    function GetModule : TParsedModule;
    function GetDottedName : string;
    function GetModuleSource : string;
    property CodePos : TCodePos read fCodePos;
    property Parent : TBaseCodeElement read fParent write fParent;
    property IsProxy : boolean read fIsProxy;  // true if derived from live Python object
    property CodeHint : string read GetCodeHint;
  end;

  TCodeBlock = record
    StartLine : integer;
    EndLine : integer;
  end;

  TModuleImport = class(TBaseCodeElement)
  private
    fRealName : string; // used if name is an alias
    fPrefixDotCount : integer; // for relative package imports
    fCodeBlock : TCodeBlock;
    function GetRealName: string;
  protected
    function GetCodeHint : string; override;
  public
    ImportAll : Boolean;
    ImportedNames : TObjectList;
    property RealName : string read GetRealName;
    property PrefixDotCount : integer read fPrefixDotCount;
    property CodeBlock : TCodeBlock read fCodeBlock;
    constructor Create(AName : string; CB : TCodeBlock);
    destructor Destroy; override;
  end;

  TVariableAttribute = (vaBuiltIn, vaClassAttribute, vaCall, vaArgument,
                        vaStarArgument, vaStarStarArgument, vaArgumentWithDefault,
                        vaImported);
  TVariableAttributes = set of TVariableAttribute;

  TVariable = class(TBaseCodeElement)
    // The parent can be TParsedModule, TParsedClass, TParsedFunction or TModuleImport
  private
    // only used if Parent is TModuleImport and Name is an alias
    fRealName : string;
    function GetRealName: string;
  protected
    function GetCodeHint : string; override;
  public
    ObjType : string;
    DefaultValue : string;
    Attributes : TVariableAttributes;
    property RealName : string read GetRealName;
  end;

  TCodeElement = class(TBaseCodeElement)
  private
    fCodeBlock : TCodeBlock;
    fDocString : string;
    fIndent : integer;
    fDocStringExtracted : boolean;
    function GetChildCount: integer;
    function GetChildren(i : integer): TCodeElement;
    procedure ExtractDocString;
  protected
    fChildren : TObjectList;
    function GetDocString: string; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddChild(CE : TCodeElement);
    procedure GetSortedClasses(SortedClasses : TObjectList);
    procedure GetSortedFunctions(SortedFunctions : TObjectList);
    procedure GetNameSpace(SList : TStringList); virtual;
    function GetScopeForLine(LineNo : integer) : TCodeElement;
    function GetChildByName(ChildName : string): TCodeElement;
    property CodeBlock : TCodeBlock read fCodeBlock;
    property Indent : integer read fIndent;
    property ChildCount : integer read GetChildCount;
    property Children[i : integer] : TCodeElement read GetChildren;
    property DocString : string read GetDocString;
  end;

  TParsedModule = class(TCodeElement)
  private
    fImportedModules : TObjectList;
    fSource : string;
    fFileName : string;
    fMaskedSource : string;
    fAllExportsVar : string;
    fFileAge : TDateTime;
    function GetIsPackage: boolean;
    procedure SetFileName(const Value: string);
  protected
    fGlobals : TObjectList;
    function GetAllExportsVar: string; virtual;
    function GetCodeHint : string; override;
    procedure GetNameSpaceInternal(SList, ImportedModuleCache : TStringList);
public
    constructor Create; overload;
    constructor Create(const Source : string); overload;
    constructor Create(const FName : string; const Source : string); overload;
    destructor Destroy; override;
    procedure Clear;
    procedure GetNameSpace(SList : TStringList); override;
    procedure GetSortedImports(ImportsList : TObjectList);
    procedure GetUniqueSortedGlobals(GlobalsList : TObjectList);
    property ImportedModules : TObjectList read fImportedModules;
    property Globals : TObjectList read fGlobals;
    property Source : string read fSource write fSource;
    property FileName : string read fFileName write SetFileName;
    property MaskedSource : string read fMaskedSource;
    property IsPackage : boolean read GetIsPackage;
    property AllExportsVar : string read GetAllExportsVar;
    property FileAge : TDateTime read fFileAge write fFileAge;
  end;

  TParsedFunction = class(TCodeElement)
  private
    fArguments : TObjectList;
    fLocals : TObjectList;
  protected
    function GetCodeHint : string; override;
  public
    ReturnType : string;
    ReturnAttributes : TVariableAttributes;
    constructor Create;
    destructor Destroy; override;
    function ArgumentsString : string; virtual;
    function HasArgument(Name : string): Boolean;
    procedure GetNameSpace(SList : TStringList); override;
    property Arguments : TObjectList read fArguments;
    property Locals : TObjectList read fLocals;
  end;

  TParsedClass = class(TCodeElement)
  private
    fSuperClasses : TStringList;
    fAttributes : TObjectList;
    procedure GetNameSpaceImpl(SList: TStringList; BaseClassResolver : TStringList);
    function GetConstructorImpl(BaseClassResolver : TStringList) : TParsedFunction;
  protected
    function GetCodeHint : string; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GetNameSpace(SList : TStringList); override;
    procedure GetUniqueSortedAttibutes(AttributesList: TObjectList);
    function GetConstructor : TParsedFunction; virtual;
    property SuperClasses : TStringList read fSuperClasses;
    property Attributes : TObjectList read fAttributes;
  end;

  TScannerProgressEvent = procedure(CharNo, NoOfChars : integer; var Stop : Boolean) of object;

  TPythonScanner = class
  private
    fOnScannerProgress : TScannerProgressEvent;
    fCodeRE : TRegEx;
    fBlankLineRE : TRegEx;
    //fEscapedQuotesRE : TRegEx;
    //fStringsAndCommentsRE : TRegEx;
    fLineContinueRE : TRegEx;
    fImportRE : TRegEx;
    fFromImportRE : TRegEx;
    fAssignmentRE : TRegEx;
    fFunctionCallRE : TRegEx;
    fForRE : TRegEx;
    fReturnRE : TRegEx;
    fWithRE : TRegEx;
    fGlobalRE : TRegEx;
    fAliasRE : TRegEx;
    fListRE : TRegEx;
    fCommentLineRE : TRegEx;
  protected
    procedure DoScannerProgress(CharNo, NoOfChars : integer; var Stop : Boolean);
  public
    property OnScannerProgress : TScannerProgressEvent
      read fOnScannerProgress write fOnScannerProgress;

    constructor Create;
    function ScanModule(Module : TParsedModule) : boolean;
    function GetExpressionType(Expr : string; Var VarAtts : TVariableAttributes) : string;
  end;

  IAsyncSourceScanner = interface
    function GetParsedModule : TParsedModule;
    function Finished : Boolean;
    procedure StopScanning;
    property ParsedModule : TParsedModule read GetParsedModule;
  end;

  TAsynchSourceScanner = class(TInterfacedObject, IAsyncSourceScanner)
  private
    fStopped : Boolean;
    fParsedModule : TParsedModule;
    fPythonScanner : TPythonScanner;
    fFuture : IFuture<TParsedModule>;
    function FutureTask(Sender : TObject) : TParsedModule;
    procedure ScanProgress(CharNo, NoOfChars : integer; var Stop : Boolean);
    // IAsyncSourceScanner implementation
    function Finished : Boolean;
    function GetParsedModule : TParsedModule;
    procedure StopScanning;
  public
    constructor Create(const FileName : string; const Source : String);
    destructor Destroy; override;
  end;

  TAsynchSourceScannerFactory = class
  private
    fIList : TInterfaceList;
    procedure ClearFinished;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReleaseScanner(Scanner : IAsyncSourceScanner);
    function CreateAsynchSourceScanner(const FileName : string; const Source : String): IAsyncSourceScanner;
  end;

  Var
    AsynchSourceScannerFactory : TAsynchSourceScannerFactory;

  function CodeBlock(StartLine, EndLine : integer) : TCodeBlock;
  function GetExpressionBuiltInType(Expr : string; Var IsBuiltIn : boolean) : string;

implementation

uses
  WinApi.Windows,
  System.Math,
  VarPyth,
  JclStrings,
  JclSysUtils,
  JvGnugettext,
  SynCompletionProposal,
  StringResources,
  uCommonFunctions,
  cRefactoring,
  cPySupportTypes,
  cPyBaseDebugger,
  cPyDebugger,
  cPyControl,
  cSSHSupport;

Const
  MaskChar = WideChar(#96);

Var
  DocStringRE : TRegEx;

function HaveImplicitContinuation(S : string; var BracesCount : integer) : Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    if (Ord(S[i]) < 128) then begin
      if AnsiChar(S[i]) in ['(', '[', '{'] then
        Inc(BracesCount)
      else if AnsiChar(S[i]) in [')', ']', '}'] then
         Dec(BracesCount);
    end;
  Result := BracesCount > 0;
end;


{ Code Ellement }

constructor TCodeElement.Create;
begin
  inherited;
  fParent := nil;
  fChildren := nil;
end;

destructor TCodeElement.Destroy;
begin
  FreeAndNil(fChildren);
  inherited;
end;

procedure TCodeElement.AddChild(CE : TCodeElement);
begin
  if fChildren = nil then
    fChildren := TObjectList.Create(True);

  CE.fParent := Self;
  fChildren.Add(CE);
end;

function TCodeElement.GetChildCount: integer;
begin
  if Assigned(fChildren) then
    Result := fChildren.Count
  else
    Result := 0;
end;

function TCodeElement.GetChildren(i : integer): TCodeElement;
begin
  if Assigned(fChildren) then begin
    Result := TCodeElement(fChildren[i]);
    Assert(Result is TCodeElement);
    Assert(Assigned(Result));
  end else
    Result := nil;
end;

function TCodeElement.GetChildByName(ChildName: string): TCodeElement;
var
  i : integer;
  CE : TCodeElement;
begin
  Result := nil;
  if not Assigned(fChildren) then Exit;
  for i := 0 to fChildren.Count - 1 do begin
    CE := GetChildren(i);
    if CE.Name = ChildName then begin
      Result := CE;
      Exit;
    end;
  end;
end;

function CompareCodeElements(Item1, Item2: Pointer): Integer;
begin
  Result := CompareStr(TCodeElement(Item1).Name, TCodeElement(Item2).Name);
end;

procedure TCodeElement.GetSortedClasses(SortedClasses: TObjectList);
Var
  i : integer;
begin
  if not Assigned(fChildren) then Exit;
  for i := 0 to Self.fChildren.Count - 1 do
    if fChildren[i] is TParsedClass then
      SortedClasses.Add(fChildren[i]);
  SortedClasses.Sort(CompareCodeElements);
end;

procedure TCodeElement.GetSortedFunctions(SortedFunctions: TObjectList);
Var
  i : integer;
begin
  if not Assigned(fChildren) then Exit;
  for i := 0 to Self.fChildren.Count - 1 do
    if fChildren[i] is TParsedFunction then
      SortedFunctions.Add(fChildren[i]);
  SortedFunctions.Sort(CompareCodeElements);
end;

procedure TCodeElement.GetNameSpace(SList: TStringList);
Var
  i : integer;
begin
  //  Add from Children
  if Assigned(fChildren) then
    for i := 0 to fChildren.Count - 1 do
      SList.AddObject(TCodeElement(fChildren[i]).Name, fChildren[i]);
end;

function TCodeElement.GetScopeForLine(LineNo: integer): TCodeElement;
Var
  i : integer;
  CE : TCodeElement;
begin
  if (LineNo >= fCodeBlock.StartLine) and (LineNo <= fCodeBlock.EndLine) then begin
    Result := Self;
    //  try to see whether the line belongs to a child
    if not Assigned(fChildren) then Exit;
    for i := 0 to fChildren.Count - 1 do begin
      CE := Children[i];
      if LineNo < CE.CodeBlock.StartLine then
        break
      else if LineNo > CE.CodeBlock.EndLine then
        continue
      else begin
        // recursive call
        Result := CE.GetScopeForLine(LineNo);
        break;
      end;
    end;
  end else
    Result := nil;
end;

procedure TCodeElement.ExtractDocString;
var
  ModuleSource, DocStringSource : string;
  CB : TCodeBlock;
begin
  if fDocStringExtracted then Exit;

  fDocStringExtracted := True;
  fDocString := '';

  CB := fCodeBlock;
  if Assigned(fChildren) and (fChildren.Count > 0) then
    CB.EndLine := Pred(Children[0].CodeBlock.StartLine);
  if CB.StartLine > CB.EndLine then Exit;

  ModuleSource := GetModuleSource;
  if ModuleSource = '' then Exit;

  DocStringSource := GetLineRange(ModuleSource, CB.StartLine, CB.EndLine);
  if DocStringSource = '' then Exit;

  if DocStringRE.IsMatch(DocStringSource) then begin
    fDocString := DocStringRE.PerlRegEx.Groups[2];
    if fDocString = '' then
      fDocString := DocStringRE.PerlRegEx.Groups[3];

    fDocString := FormatDocString(fDocString);
  end;
end;

function TCodeElement.GetDocString: string;
begin
  if not fDocStringExtracted then
    ExtractDocString;
  Result := fDocString;
end;

{ TPythonScanner }

constructor TPythonScanner.Create;
begin
  inherited;
  fCodeRE := CompiledRegEx(Format('^([ \t]*)(class|def)[ \t]+(%s)[ \t]*(\(([^>]*)\))?[ \t]*(->[ \t]*([^ \t:][^:]*))?:',
    [IdentRE]));
  fBlankLineRE := CompiledRegEx('^[ \t]*($|\$|\#|\"\"\"|''''''|' + MaskChar +')');
  //fEscapedQuotesRE := CompiledRegEx('(\\\\|\\\"|\\\'')');
  //fStringsAndCommentsRE :=
  //  CompiledRegEx('(?sm)(\"\"\".*?\"\"\"|''''''.*?''''''|\"[^\"]*\"|\''[^\'']*\''|#.*?\n)');
  fLineContinueRE := CompiledRegEx('\\[ \t]*(#.*)?$');
  fImportRE := CompiledRegEx('^[ \t]*import[ \t]+([^#;]+)');
  fFromImportRE :=
    CompiledRegEx(Format('^[ \t]*from[ \t]+(\.*)(%s)[ \t]+import[ \t]+([^#;]+)', [DottedIdentRE]));
    { TODO : Deal with form . import module as module  syntax }
    //CompiledRegEx(Format('^[ \t]*from[ \t]+(\.*)(%s)?[ \t]+import[ \t]+([^#;]+)', [DottedIdentRE]));
  fAssignmentRE :=
    CompiledRegEx(Format('^([ \t]*(self.)?%s[ \t]*(,[ \t]*(self.)?%s[ \t]*)*(=))+(.*)',
      [IdentRE, IdentRE]));
  fFunctionCallRE := CompiledRegEx(Format('^[ \t]*(%s)(\(?)', [DottedIdentRE]));
  fForRE := CompiledRegEx(Format('^\s*for +(%s)( *, *%s)* *(in)', [IdentRe, IdentRe]));
  fReturnRE := CompiledRegEx('^[ \t]*return[ \t]+(.*)');
  fWithRE :=
    CompiledRegEx(Format('^[ \t]*with +(%s) *(\(?).*as +(%s)',
      [DottedIdentRE, IdentRE]));
  fGlobalRE :=
    CompiledRegEx(Format('^[ \t]*global +((%s)( *, *%s)*)',
      [IdentRE, IdentRE]));
  fAliasRE :=
    CompiledRegEx(Format('^[ \t]*(%s)([ \t]+as[ \t]+(%s))?',
      [DottedIdentRE, IdentRE]));
  fListRE :=
    CompiledRegEx('\[(.*)\]');
  fCommentLineRE :=
    CompiledRegEx('^([ \t]*)#');
end;

procedure TPythonScanner.DoScannerProgress(CharNo, NoOfChars : integer;
  var Stop: Boolean);
begin
  if Assigned(fOnScannerProgress) then
    fOnScannerProgress(CharNo, NoOfChars, Stop);
end;

function TPythonScanner.GetExpressionType(Expr: string;
  var VarAtts: TVariableAttributes): string;
Var
  IsBuiltInType : Boolean;
begin
  if Expr.Length = 0 then Exit('');

  if fFunctionCallRE.IsMatch(Expr) then begin
    Result := fFunctionCallRE.PerlRegEx.Groups[1];
    if fFunctionCallRE.PerlRegEx.Groups[2] <> '' then  //= '('
    Include(VarAtts, vaCall);
  end else begin
    Result := GetExpressionBuiltInType(Expr, IsBuiltInType);
    if IsBuiltInType then
      Include(VarAtts, vaBuiltIn);
  end;
end;

function TPythonScanner.ScanModule(Module : TParsedModule): boolean;
// Expectes Module Source code in Module.Source
// Parses the Python Source code and adds code elements as children of Module
{ TODO 2 : Optimize out calls to Trim }
Var
  UseModifiedSource : boolean;
  SourceLines : TStringList;
  SourceLinesSafeGuard: ISafeGuard;

  function GetNthSourceLine(LineNo : integer) : string;
  begin
    if not Assigned(SourceLines) then begin
      SourceLines := TStringList(Guard(TStringList.Create, SourceLinesSafeGuard));
      SourceLines.Text := Module.Source;
    end;

    if LineNo <= SourceLines.Count then
      Result := SourceLines[LineNo-1]
    else
      Result := '';
  end;

  procedure GetLine(var P : PWideChar; var Line : string; var LineNo : integer);
  Var
    Start : PWideChar;
  begin
    Inc(LineNo);
    Start := P;
    while not CharInSet(P^, [#0, #10, #13]) do Inc(P);
    if UseModifiedSource then
      SetString(Line, Start, P - Start)
    else
      Line := GetNthSourceLine(LineNo);
    if P^ = WideChar(#13) then Inc(P);
    if P^ = WideChar(#10) then Inc(P);
  end;

  procedure CharOffsetToCodePos(CharOffset, FirstLine : integer; LineStarts : TList;
    var CodePos: TCodePos);
  var
    i : integer;
  begin
    CodePos.LineNo := FirstLine;
    CodePos.CharOffset := CharOffset;
    for i := LineStarts.Count - 1 downto 0 do begin
      if Integer(LineStarts[i]) <= CharOffset then begin
        CodePos.CharOffset := CharOffset - Integer(LineStarts[i]) + 1;
        CodePos.LineNo := FirstLine + i + 1;
        break;
      end;
    end;
  end;

  procedure RemoveComment(var S : string);
  var
    Index : Integer;
  begin
    // Remove comment
    Index := CharPos(S, WideChar('#'));
    if Index > 0 then
      S := Copy(S, 1, Index -1);
  end;

  function ProcessLineContinuation(var P : PWideChar; var Line : string;
    var LineNo: integer; LineStarts : TList): boolean;
  // Process continuation lines
  var
    ExplicitContinuation, ImplicitContinuation : boolean;
    NewLine : string;
    TrimmedLine : string;
    BracesCount : integer;
  begin
    BracesCount := 0;
    LineStarts.Clear;
    RemoveComment(Line);
    ExplicitContinuation := fLineContinueRE.IsMatch(Line);
    ImplicitContinuation := not ExplicitContinuation  and
      HaveImplicitContinuation(Line, BracesCount);
    Result := ExplicitContinuation or ImplicitContinuation;

    while (ExplicitContinuation or ImplicitContinuation) and (P^ <> WideChar(#0)) do begin
      if ExplicitContinuation then
        // Drop the continuation char
        Line := Copy(Line, 1, fLineContinueRE.PerlRegEx.GroupOffsets[0] - 1);
      LineStarts.Add(Pointer(Length(Line)+2));
      GetLine(P, NewLine, LineNo);
      RemoveComment(NewLine);
      TrimmedLine := Trim(NewLine);
      if ExplicitContinuation and (TrimmedLine='') then break;
      // issue 212
      if StrIsLeft(PWideChar(TrimmedLine), 'class ') or StrIsLeft(PWideChar(TrimmedLine), 'def ') then break;

      Line := Line + WideChar(' ') + NewLine;
      ExplicitContinuation := fLineContinueRE.IsMatch(Line);
      ImplicitContinuation := not ExplicitContinuation  and
        HaveImplicitContinuation(NewLine, BracesCount);
    end;
  end;

  function GetActiveClass(CodeElement : TBaseCodeElement) : TParsedClass;
  begin
    while Assigned(CodeElement) and (CodeElement.ClassType <> TParsedClass) do
      CodeElement := CodeElement.Parent;
    Result := TParsedClass(CodeElement);
  end;

  procedure ReplaceQuotedChars(var Source : string);
  //  replace quoted \ ' " with **
  Var
    pRes, pSource : PWideChar;
  begin
    if Length(Source) = 0 then Exit;
    pRes := PWideChar(Source);
    pSource := PWideChar(Source);
    while pSource^ <> WideChar(#0) do begin
      if (pSource^ = WideChar('\')) then begin
        Inc(pSource);
        if CharInSet(pSource^, ['\', '''', '"']) then begin
          pRes^ := WideChar('*');
          Inc(pRes);
          pRes^ := WideChar('*');
        end else
          Inc(pRes);
      end;
      inc(pSource);
      inc(pRes);
    end;
  end;

  procedure MaskStringsAndComments(var Source : string);
  // Replace all chars in strings and comments with *
  Type
    TParseState = (psNormal, psInTripleSingleQuote, psInTripleDoubleQuote,
    psInSingleString, psInDoubleString, psInComment);
  Var
    pRes, pSource : PWideChar;
    ParseState : TParseState;
  begin
    SourceLines := nil;
    if Length(Source) = 0 then Exit;
    pRes := PWideChar(Source);
    pSource := PWideChar(Source);
    ParseState := psNormal;
    while pSource^ <> #0 do begin
      case pSource^ of
        WideChar('"') :
           case ParseState of
             psNormal :
               if StrIsLeft(psource + 1, '""') then begin
                 ParseState := psInTripleDoubleQuote;
                 Inc(pRes,2);
                 Inc(pSource, 2);
               end else
                 ParseState := psInDoubleString;
             psInTripleSingleQuote,
             psInSingleString,
             psInComment :
               pRes^ := MaskChar;
             psInTripleDoubleQuote :
               if StrIsLeft(psource + 1, '""') then begin
                 ParseState := psNormal;
                 Inc(pRes,2);
                 Inc(pSource, 2);
               end else
                 pRes^ := MaskChar;
             psInDoubleString :
               ParseState := psNormal;
           end;
        WideChar(''''):
           case ParseState of
             psNormal :
               if StrIsLeft(psource + 1, '''''') then begin
                 ParseState := psInTripleSingleQuote;
                 Inc(pRes, 2);
                 Inc(pSource, 2);
               end else
                 ParseState := psInSingleString;
             psInTripleDoubleQuote,
             psInDoubleString,
             psInComment :
               pRes^ := MaskChar;
             psInTripleSingleQuote :
               if StrIsLeft(psource + 1, '''''') then begin
                 ParseState := psNormal;
                 Inc(pRes, 2);
                 Inc(pSource, 2);
               end else
                 pRes^ := MaskChar;
             psInSingleString :
               ParseState := psNormal;
           end;
        WideChar('#') :
          if ParseState = psNormal then
            ParseState := psInComment
          else
            pRes^ := MaskChar;
        WideChar(#10), WideChar(#13):
          begin
            if ParseState in [psInSingleString, psInDoubleString, psInComment] then
              ParseState := psNormal;
          end;
        WideChar(' '),
        WideChar(#9) : {do nothing};
      else
        if ParseState <> psNormal then
          pRes^ := MaskChar;
      end;
      inc(pSource);
      inc(pRes);
    end;
  end;

var
  P, CodeStartP : PWideChar;
  LineNo, Indent, Index, CharOffset, CharOffset2, LastLength : integer;
  CodeStart : integer;
  Line, Param, AsgnTargetList, S, SourceLine, LeftS, RightS : string;
  Stop : Boolean;
  CodeElement, LastCodeElement, Parent : TCodeElement;
  ModuleImport : TModuleImport;
  Variable : TVariable;
  Klass : TParsedClass;
  LineStarts: TList;
  LineStartsGuard: ISafeGuard;
  GlobalList : TStringList;
  GlobalListGuard : ISafeGuard;
  AsgnTargetCount : integer;
begin
  LineStarts := TList(Guard(TList.Create, LineStartsGuard));
  GlobalList := TStringList(Guard(TStringList.Create, GlobalListGuard));
  GlobalList.CaseSensitive := True;
  UseModifiedSource := True;

  Module.Clear;
  Module.fCodeBlock.StartLine := 1;
  Module.fIndent := -1;  // so that everything is a child of the module

  // Change \" \' and \\ into ** so that text searches
  // for " and ' won't hit escaped ones
  //Module.fMaskedSource := fEscapedQuotesRE.Replace(Source, '**', False);
  Module.fMaskedSource := Copy(Module.fSource, 1, MaxInt);
  ReplaceQuotedChars(Module.fMaskedSource);

  // Replace all chars in strings and comments with *
  // This ensures that text searches don't mistake comments for keywords, and that all
  // matches are in the same line/comment as the original
  MaskStringsAndComments(Module.fMaskedSource);

  P := PWideChar(Module.fMaskedSource);
  LineNo := 0;
  Stop := False;

  LastCodeElement := Module;
  while not Stop and (P^ <> #0) do begin
    GetLine(P, Line, LineNo);


    // skip blank lines and comment lines
    if (Length(Line) = 0) or fBlankLineRE.IsMatch(Line) then continue;
    // skip comments
    if fCommentLineRE.IsMatch(Line) then continue;

    CodeStartP := P;
    CodeStart := LineNo;
    // Process continuation lines
    ProcessLineContinuation(P, Line, LineNo, LineStarts);

    if fCodeRE.IsMatch(Line) then begin
      // found class or function definition
      GlobalList.Clear;

      S := fCodeRE.PerlRegEx.Groups[5];
      if fCodeRE.PerlRegEx.Groups[2] = 'class' then begin
        // class definition
        CodeElement := TParsedClass.Create;
        TParsedClass(CodeElement).fSuperClasses.CommaText := S;
      end else begin
        // function or method definition
        CodeElement := TParsedFunction.Create;
        TParsedFunction(CodeElement).ReturnType := fCodeRE.PerlRegEx.Groups[7];

        if S <> '' then begin
          CharOffset := fCodeRE.PerlRegEx.GroupOffsets[5];
          LastLength := Length(S);
          Param := GetParameter(S);
          CharOffset2 := CalcIndent(Param);
          Param := Trim(Param);
          Index := 0;
          While Param <> '' do begin
            Variable := TVariable.Create;
            Variable.Parent := CodeElement;
            CharOffsetToCodePos(CharOffset + CharOffset2, CodeStart, LineStarts, Variable.fCodePos);
            if StrIsLeft(PWideChar(Param), '**') then begin
              Variable.Name := Copy(Param, 3, Length(Param) -2);
              Include(Variable.Attributes, vaStarStarArgument);
            end else if Param[1] = '*' then begin
              Variable.Name := Copy(Param, 2, Length(Param) - 1);
              Include(Variable.Attributes, vaStarArgument);
            end else begin
              Index := CharPos(Param, WideChar('='));
              if Index > 0 then begin
                Variable.Name := Trim(Copy(Param, 1, Index - 1));
                Variable.DefaultValue := Copy(Param, Index + 1, Length(Param) - Index);
                if Variable.DefaultValue.Length > 0 then begin
                  // Deal with string arguments (Issue 32)
                  if CharPos(Variable.DefaultValue, MaskChar) > 0 then begin
                    SourceLine := GetNthSourceLine(Variable.fCodePos.LineNo);
                    Variable.DefaultValue :=
                      Copy(SourceLine, Variable.CodePos.CharOffset + Index, Length(Variable.DefaultValue));
                  end;
                  Variable.DefaultValue := Trim(Variable.DefaultValue);

                  Include(Variable.Attributes, vaArgumentWithDefault);
                  Variable.ObjType := GetExpressionType(Variable.DefaultValue, Variable.Attributes);
                end;
              end else begin
                Variable.Name := Param;
                Include(Variable.Attributes, vaArgument);
              end;
            end;
            // Deal with string annotations (Issue 511)
            if CharPos(Variable.Name, MaskChar) > 0 then begin
              SourceLine := GetNthSourceLine(Variable.fCodePos.LineNo);
              Variable.Name :=
                Copy(SourceLine, Variable.CodePos.CharOffset, Length(Variable.Name));
            end;
            if StrSplit(':', Variable.Name, LeftS, RightS) then begin
              Variable.Name := TrimRight(LeftS);
              Variable.ObjType := Trim(RightS);
            end;
            TParsedFunction(CodeElement).fArguments.Add(Variable);

            Inc(CharOffset,  LastLength - Length(S));
            LastLength := Length(S);
            Param := GetParameter(S);
            CharOffset2 := CalcIndent(Param);
            Param := Trim(Param);
          end;
        end;
      end;
      CodeElement.Name := fCodeRE.PerlRegEx.Groups[3];
      CodeElement.fCodePos.LineNo := CodeStart;
      CodeElement.fCodePos.CharOffset := fCodeRe.PerlRegEx.GroupOffsets[3];

      CodeElement.fIndent := CalcIndent(fCodeRE.PerlRegEx.Groups[1]);
      CodeElement.fCodeBlock.StartLine := CodeStart;

      // Decide where to insert CodeElement
      if CodeElement.Indent > LastCodeElement.Indent then
        LastCodeElement.AddChild(CodeElement)
      else begin
        LastCodeElement.fCodeBlock.EndLine := Pred(CodeStart);
        Parent := LastCodeElement.Parent as TCodeElement;
        while Assigned(Parent) do begin
          // Note that Module.Indent = -1
          if Parent.Indent < CodeElement.Indent then begin
            Parent.AddChild(CodeElement);
            break;
          end else
            Parent.fCodeBlock.EndLine := Pred(CodeStart);
          Parent := Parent.Parent as TCodeElement;
        end;
      end;
      LastCodeElement := CodeElement;
    end else begin
      // Close Functions and Classes based on indentation
      Indent := CalcIndent(Line);
      while Assigned(LastCodeElement) and (LastCodeElement.Indent >= Indent) do begin
        // Note that Module.Indent = -1
        LastCodeElement.fCodeBlock.EndLine := Pred(LineNo);
        LastCodeElement := LastCodeElement.Parent as TCodeElement;
      end;
      // search for imports
      if fImportRE.IsMatch(Line) then begin
        // Import statement
        S := fImportRE.PerlRegEx.Groups[1];
        CharOffset := fImportRE.PerlRegEx.GroupOffsets[1];
        LastLength := Length(S);
        Param := StrToken(S, ',');
        While Param <> '' do begin
          if fAliasRE.IsMatch(Param) then begin
            if fAliasRE.PerlRegEx.Groups[3] <> '' then begin
              Param := fAliasRE.PerlRegEx.Groups[3];
              CharOffset2 := fAliasRE.PerlRegEx.GroupOffsets[3] - 1;
            end else begin
              Param := fAliasRE.PerlRegEx.Groups[1];
              CharOffset2 := fAliasRE.PerlRegEx.GroupOffsets[1] - 1;
            end;
            ModuleImport := TModuleImport.Create(Param, CodeBlock(CodeStart, LineNo));
            CharOffsetToCodePos(CharOffset + CharOffset2, CodeStart, LineStarts, ModuleImport.fCodePos);
            ModuleImport.Parent := Module;
            if fAliasRE.PerlRegEx.Groups[3] <> '' then
              ModuleImport.fRealName := fAliasRE.PerlRegEx.Groups[1];
            Module.fImportedModules.Add(ModuleImport);
          end;
          Inc(CharOffset,  LastLength - Length(S));
          LastLength := Length(S);
          Param := StrToken(S, ',');
        end;
      end else if fFromImportRE.IsMatch(Line) then begin
        // From Import statement
        ModuleImport := TModuleImport.Create(fFromImportRE.PerlRegEx.Groups[2],
          CodeBlock(CodeStart, LineNo));
        ModuleImport.fPrefixDotCount := fFromImportRE.PerlRegEx.Groups[1].Length;
        ModuleImport.fCodePos.LineNo := CodeStart;
        ModuleImport.fCodePos.CharOffset := fFromImportRE.PerlRegEx.GroupOffsets[2];
        S := fFromImportRE.PerlRegEx.Groups[3];
        if Trim(S) = '*' then
          ModuleImport.ImportAll := True
        else begin
          ModuleImport.ImportedNames := TObjectList.Create(True);
          CharOffset := fFromImportRE.PerlRegEx.GroupOffsets[3];
          if Pos('(', S) > 0 then begin
            Inc(CharOffset);
            S := StrRemoveChars(S, ['(',')']); //from module import (a,b,c) form
          end;
          LastLength := Length(S);
          Param := StrToken(S, ',');
          While Param <> '' do begin
            if fAliasRE.IsMatch(Param) then begin
              if fAliasRE.PerlRegEx.Groups[3] <> '' then begin
                Param := fAliasRE.PerlRegEx.Groups[3];
                CharOffset2 := fAliasRE.PerlRegEx.GroupOffsets[3] - 1;
              end else begin
                Param := fAliasRE.PerlRegEx.Groups[1];
                CharOffset2 := fAliasRE.PerlRegEx.GroupOffsets[1] - 1;
              end;
              Variable := TVariable.Create;
              Variable.Name := Param;
              CharOffsetToCodePos(CharOffset + CharOffset2, CodeStart, LineStarts, Variable.fCodePos);
              Variable.Parent := ModuleImport;
              Include(Variable.Attributes, vaImported);
              if fAliasRE.PerlRegEx.Groups[3] <> '' then
                Variable.fRealName := fAliasRE.PerlRegEx.Groups[1];
              ModuleImport.ImportedNames.Add(Variable);
            end;
            Inc(CharOffset,  LastLength - Length(S));
            LastLength := Length(S);
            Param := StrToken(S, ',');
          end;
        end;
        ModuleImport.Parent := Module;
        Module.fImportedModules.Add(ModuleImport);
      end else if fAssignmentRE.IsMatch(Line) then begin
        S := Copy(Line, 1, fAssignmentRE.PerlRegEx.GroupOffsets[5]-1);
        AsgnTargetList := StrToken(S, '=');
        CharOffset2 := 1; // Keeps track of the end of the identifier
        while AsgnTargetList <> '' do begin
          AsgnTargetCount := 0;
          Variable := nil;
          while AsgnTargetList <> '' do begin
            Param := StrToken(AsgnTargetList, ',');
            CharOffset := CharOffset2;  // Keeps track of the start of the identifier
            Inc(CharOffset, CalcIndent(Param, 1)); // do not expand tabs
            Inc(CharOffset2, Succ(Length(Param))); // account for ,
            Param := Trim(Param);
            if StrIsLeft(PWideChar(Param), 'self.') then begin
              // class variable
              Param := Copy(Param, 6, Length(Param) - 5);
              Inc(CharOffset, 5);  // Length of "self."
              // search for class attributes
              Klass := GetActiveClass(LastCodeElement);
              if Assigned(Klass) then begin
                Variable := TVariable.Create;
                Variable.Name := Param;
                Variable.Parent := Klass;
                CharOffsetToCodePos(CharOffset, CodeStart, LineStarts, Variable.fCodePos);
                Klass.fAttributes.Add(Variable);
                Inc(AsgnTargetCount);
              end;
            end else if (GlobalList.IndexOf(Param) < 0) then begin
              // search for local/global variables
              Variable := TVariable.Create;
              Variable.Name := Param;
              Variable.Parent := LastCodeElement;
              CharOffsetToCodePos(CharOffset, CodeStart, LineStarts, Variable.fCodePos);
              if LastCodeElement.ClassType = TParsedFunction then begin
                if TParsedFunction(LastCodeElement).HasArgument(Variable.Name) then
                  FreeAndNil(Variable)
                else
                  TParsedFunction(LastCodeElement).Locals.Add(Variable);
              end else if LastCodeElement.ClassType = TParsedClass then begin
                Include(Variable.Attributes, vaClassAttribute);
                TParsedClass(LastCodeElement).Attributes.Add(Variable)
              end else begin
                Module.Globals.Add(Variable);
                if Variable.Name = '__all__' then begin
                  Line := GetNthSourceLine(CodeStart);
                  UseModifiedSource := False;
                  ProcessLineContinuation(CodeStartP, Line, CodeStart, LineStarts);
                  if fListRE.IsMatch(Line) then
                    Module.fAllExportsVar := fListRE.PerlRegEx.Groups[1];
                  UseModifiedSource := True;
                end;
              end;
              Inc(AsgnTargetCount);
            end;
          end;
          // Variable Type if the assignment has a single target
          if Assigned(Variable) and (AsgnTargetCount = 1) then
            Variable.ObjType := GetExpressionType(fAssignmentRE.PerlRegEx.Groups[6],
              Variable.Attributes);

          AsgnTargetList := StrToken(S, '=');
        end;
      end else if fForRE.IsMatch(Line) then begin
        AsgnTargetList := Copy(Line, fForRE.PerlRegEx.GroupOffsets[1],
          fForRE.PerlRegEx.GroupOffsets[3]-fForRE.PerlRegEx.GroupOffsets[1]);
        CharOffset2 := fForRE.PerlRegEx.GroupOffsets[1]; // Keeps track of the end of the identifier
        while AsgnTargetList <> '' do begin
          Param := StrToken(AsgnTargetList, ',');
          CharOffset := CharOffset2;  // Keeps track of the start of the identifier
          Inc(CharOffset, CalcIndent(Param, 1)); // do not expand tabs
          Inc(CharOffset2, Succ(Length(Param))); // account for ,
          Param := Trim(Param);
          if (GlobalList.IndexOf(Param) < 0) then begin
            // search for local/global variables
            Variable := TVariable.Create;
            Variable.Name := Param;
            Variable.Parent := LastCodeElement;
            CharOffsetToCodePos(CharOffset, CodeStart, LineStarts, Variable.fCodePos);
            if LastCodeElement.ClassType = TParsedFunction then
              TParsedFunction(LastCodeElement).Locals.Add(Variable)
            else if LastCodeElement.ClassType = TParsedClass then begin
              //  Do not add for variables to class
              Variable.Free;
            end else begin
              Module.Globals.Add(Variable);
            end;
          end;
        end;
      end else if fReturnRE.IsMatch(Line) then begin
        // only process first return statement
        if (LastCodeElement is TParsedFunction) and
          (TParsedFunction(LastCodeElement).ReturnType = '')
        then
          TParsedFunction(LastCodeElement).ReturnType :=
            GetExpressionType(fReturnRE.PerlRegEx.Groups[1],
            TParsedFunction(LastCodeElement).ReturnAttributes);
      end else if fWithRE.IsMatch(Line) then begin
        Variable := TVariable.Create;
        Variable.Name := fWithRE.PerlRegEx.Groups[3];
        Variable.Parent := LastCodeElement;
        Variable.fCodePos.LineNo := LineNo;
        Variable.fCodePos.CharOffset := fWithRE.PerlRegEx.GroupOffsets[3];
        Variable.ObjType := fWithRE.PerlRegEx.Groups[1];
        if fWithRE.PerlRegEx.Groups[2] <> '' then
          Include(Variable.Attributes, vaCall);
        if LastCodeElement.ClassType = TParsedFunction then
          TParsedFunction(LastCodeElement).Locals.Add(Variable)
        else if LastCodeElement.ClassType = TParsedClass then begin
          Include(Variable.Attributes, vaClassAttribute);
          TParsedClass(LastCodeElement).Attributes.Add(Variable)
        end else
          Module.Globals.Add(Variable);
      end else if fGlobalRE.IsMatch(Line) then begin
        S := fGlobalRE.PerlRegEx.Groups[1];
        while S <> '' do
          GlobalList.Add(Trim(StrToken(S, ',')));
      end;
    end;
    DoScannerProgress(P - PWideChar(Module.fMaskedSource), Length(Module.fMaskedSource), Stop);
  end;
  // Account for blank line in the end;
  if Length(Module.fMaskedSource) > 0 then begin
    Dec(P);
    if CharInSet(P^, [#10, #13]) then
      Inc(LineNo);
  end;
  while Assigned(LastCodeElement) do begin
    LastCodeElement.fCodeBlock.EndLine := Max(LineNo, LastCodeElement.fCodeBlock.StartLine);
    LastCodeElement := LastCodeElement.Parent as TCodeElement;
  end;

  Result := not Stop;
end;

{ TParsedModule }

constructor TParsedModule.Create;
begin
   inherited;
   fImportedModules := TObjectList.Create(True);
   fGlobals := TObjectList.Create(True);
   fCodePos.LineNo := 1;
   fCodePos.CharOffset := 1;
end;

procedure TParsedModule.Clear;
begin
  //fSource := '';
  fMaskedSource := '';
  if Assigned(fChildren) then
    fChildren.Clear;
  fImportedModules.Clear;
  fGlobals.Clear;
  inherited;
end;

constructor TParsedModule.Create(const Source: string);
begin
  Create;
  fSource := Source;
end;

constructor TParsedModule.Create(const FName, Source: string);
begin
  Create(Source);
  FileName := FName;
end;

destructor TParsedModule.Destroy;
begin
  fImportedModules.Free;
  fGlobals.Free;
  inherited;
end;

function CompareVariables(Item1, Item2: Pointer): Integer;
begin
  Result := CompareStr(TVariable(Item1).Name, TVariable(Item2).Name);
end;

procedure TParsedModule.GetUniqueSortedGlobals(GlobalsList: TObjectList);
Var
  i, j : integer;
  HasName : boolean;
begin
  for i := 0 to fGlobals.Count - 1 do begin
    HasName := False;
    for j := 0 to GlobalsList.Count - 1 do
      if (TVariable(fGlobals[i]).Name = TVariable(GlobalsList[j]).Name) then begin
        HasName := True;
        break;
      end;
    if not HasName then
      GlobalsList.Add(fGlobals[i]);
  end;
  GlobalsList.Sort(CompareVariables);
end;

procedure TParsedModule.SetFileName(const Value: string);
begin
  fFileName := Value;
  Name := FileNameToModuleName(Value);
end;

function CompareImports(Item1, Item2: Pointer): Integer;
begin
  Result := CompareStr(TModuleImport(Item1).Name, TModuleImport(Item2).Name);
end;

procedure TParsedModule.GetSortedImports(ImportsList: TObjectList);
Var
  i : integer;
begin
  for i := 0 to ImportedModules.Count - 1 do
    ImportsList.Add(ImportedModules[i]);
  ImportsList.Sort(CompareImports);
end;

procedure TParsedModule.GetNameSpace(SList: TStringList);
{
   GetNameSpaceInternal takes care of cyclic imports
}
var
  ImportedModuleCache : TStringList;
begin
  ImportedModuleCache := TStringList.Create;
  try
    GetNameSpaceInternal(SList, ImportedModuleCache);
  finally
    ImportedModuleCache.Free;
  end;
end;

procedure TParsedModule.GetNameSpaceInternal(SList, ImportedModuleCache : TStringList);
var
  CurrentCount: Integer;
  j: Integer;
  Index: Integer;
  Server, Path: string;
  PackageRootName: string;
  i: Integer;
  PythonPathAdder: IInterface;
  ModuleImport: TModuleImport;
  ParsedModule: TParsedModule;
begin
  if ImportedModuleCache.IndexOf(FileName) >= 0 then
    Exit;  //  Called from a circular input

  ImportedModuleCache.Add(FileName);

  inherited GetNameSpace(SList);

  //  Add from Globals
  for i := 0 to fGlobals.Count - 1 do
    SList.AddObject(TVariable(fGlobals[i]).Name, fGlobals[i]);
  // Add the path of the executed file to the Python path
  if not TSSHFileName.Parse(Self.FileName, Server, Path) then
    Path := ExtractFileDir(Self.fFileName)
  else
    Path := '';
  if Length(Path) > 1 then
  begin
    PythonPathAdder :=  PyControl.InternalInterpreter.AddPathToPythonPath(Path);
  end;
  //  Add from imported modules
  for i := 0 to fImportedModules.Count - 1 do
  begin
    ModuleImport := TModuleImport(fImportedModules[i]);
    // imported names
    if ModuleImport.ImportAll then
    begin
      // from "module import *" imports
      ParsedModule := PyScripterRefactor.GetParsedModule(ModuleImport.Name, None);
      //  Deal with modules imported themselves (yes it can happen!)
      if not Assigned(ParsedModule) or (ParsedModule = Self) then
        break;
      CurrentCount := SList.Count;
      if ParsedModule is TModuleProxy then
        TModuleProxy(ParsedModule).Expand;
      ParsedModule.GetNameSpaceInternal(SList, ImportedModuleCache);
      // Now filter out added names for private and accounting for __all__
      if not (ParsedModule is TModuleProxy) then
        for j := Slist.Count - 1 downto CurrentCount do
        begin
          if (StrIsLeft(PWideChar(SList[j]), '__') and not StrIsRight(PWidechar(SList[j]), '__')) or ((ParsedModule.AllExportsVar <> '') and (Pos(SList[j], ParsedModule.AllExportsVar) = 0)) then
            SList.Delete(j);
        end;
    end else if Assigned(ModuleImport.ImportedNames) then begin
      for j := 0 to ModuleImport.ImportedNames.Count - 1 do
        SList.AddObject(TVariable(ModuleImport.ImportedNames[j]).Name, ModuleImport.ImportedNames[j]);
    end else begin
      // imported modules
      Index := CharPos(ModuleImport.Name, '.');
      if Index = 0 then
        SList.AddObject(ModuleImport.Name, ModuleImport)
      else if Index > 0 then
      begin
        // we have a package import add implicit import name
        PackageRootName := Copy(ModuleImport.Name, 1, Index - 1);
        if SList.IndexOf(PackageRootName) < 0 then
        begin
          ParsedModule := PyScripterRefactor.GetParsedModule(PackageRootName, None);
          if Assigned(ParsedModule) then
            SList.AddObject(PackageRootName, ParsedModule);
        end;
      end;
    end;
  end;
end;

function TParsedModule.GetIsPackage: boolean;
begin
  Result := FileIsPythonPackage(fFileName);
end;

function TParsedModule.GetAllExportsVar: string;
begin
  Result := fAllExportsVar;
end;

function TParsedModule.GetCodeHint: string;
begin
  if IsPackage then
    Result := Format(_(SParsedPackageCodeHint), [FileName, Name])
  else
    Result := Format(_(SParsedModuleCodeHint), [FileName, Name]);
end;

{ TModuleImport }

constructor TModuleImport.Create(AName : string; CB : TCodeBlock);
begin
  inherited Create;
  Name := AName;
  fCodeBlock := CB;
  ImportAll := False;
  ImportedNames := nil;
end;

destructor TModuleImport.Destroy;
begin
  FreeAndNil(ImportedNames);
  inherited;
end;

function TModuleImport.GetCodeHint: string;
begin
  Result := Format(_(SModuleImportCodeHint), [RealName]);
end;

function TModuleImport.GetRealName: string;
begin
  if fRealName <> '' then
    Result := fRealName
  else
    Result := Name;
end;

{ TParsedFunction }

function TParsedFunction.ArgumentsString: string;
  function FormatArgument(Variable : TVariable) : string;
  begin
    if vaStarArgument in Variable.Attributes then
      Result := '*' + Variable.Name
    else if vaStarStarArgument in Variable.Attributes then
      Result := '**' + Variable.Name
    else
      Result := Variable.Name;
    if Variable.ObjType <> '' then
       Result := Result + ': ' +Variable.ObjType;
    if vaArgumentWithDefault in Variable.Attributes then
      Result := Result + '=' + Variable.DefaultValue;
  end;

Var
  i : integer;
begin
  Result:= '';
  if fArguments.Count > 0 then begin
    Result := FormatArgument(TVariable(fArguments[0]));
    for i := 1 to fArguments.Count - 1 do
      Result := Result + ', ' + FormatArgument(TVariable(Arguments[i]));
  end;
end;

constructor TParsedFunction.Create;
begin
  inherited;
  fLocals := TObjectList.Create(True);
  fArguments := TObjectList.Create(True);
end;

destructor TParsedFunction.Destroy;
begin
  FreeAndNil(fLocals);
  FreeAndNil(fArguments);
  inherited;
end;

function TParsedFunction.GetCodeHint: string;
Var
  Module : TParsedModule;
  DefinedIn : string;
begin
  Module := GetModule;
  if Module is TModuleProxy then
    DefinedIn := Format(_(SDefinedInModuleCodeHint), [Module.Name])
  else
    DefinedIn := Format(_(SFilePosInfoCodeHint),
      [Module.FileName, fCodePos.LineNo, fCodePos.CharOffset,
       Module.Name, fCodePos.LineNo]);

  if Parent is TParsedClass then
    Result := Format(_(SParsedMethodCodeHint),
      [Parent.Name, Name, ArgumentsString, DefinedIn])
  else
    Result := Format(_(SParsedFunctionCodeHint),
      [Name, ArgumentsString, DefinedIn])
end;

procedure TParsedFunction.GetNameSpace(SList: TStringList);
Var
  i : integer;
begin
  inherited;
  // Add Locals
  for i := 0 to fLocals.Count - 1 do
    SList.AddObject(TVariable(fLocals[i]).Name, fLocals[i]);
  // Add arguments
  for i := 0 to fArguments.Count - 1 do
    SList.AddObject(TVariable(fArguments[i]).Name, fArguments[i]);
end;

function TParsedFunction.HasArgument(Name: string): Boolean;
var
  Variable : TObject;
begin
  Result := False;
  for Variable in fArguments do
    if (Variable as TVariable).Name = Name then begin
      Result := True;
      Exit;
    end;
end;

{ TParsedClass }

constructor TParsedClass.Create;
begin
  inherited;
  fSuperClasses := TStringList.Create;
  fSuperClasses.CaseSensitive := True;
  fSuperClasses.StrictDelimiter := True;
  fAttributes := TObjectList.Create(True);
end;

destructor TParsedClass.Destroy;
begin
  FreeAndNil(fSuperClasses);
  FreeAndNil(fAttributes);
  inherited;
end;

function TParsedClass.GetCodeHint: string;
Var
  Module : TParsedModule;
  DefinedIn : string;
begin
  Module := GetModule;
  if Module is TModuleProxy then
    DefinedIn := Format(_(SDefinedInModuleCodeHint), [Module.Name])
  else
    DefinedIn := Format(_(SFilePosInfoCodeHint),
      [Module.FileName, fCodePos.LineNo, fCodePos.CharOffset,
       Module.Name, fCodePos.LineNo]);

  Result := Format(_(SParsedClassCodeHint),
    [Name, DefinedIn]);
  if fSuperClasses.Count > 0 then
    Result := Result + Format(_(SInheritsFromCodeHint), [fSuperClasses.CommaText]);
end;

function TParsedClass.GetConstructor: TParsedFunction;
var
  BaseClassResolver : TStringList;
begin
  BaseClassResolver := TStringList.Create;
  BaseClassResolver.CaseSensitive := True;
  try
    Result := GetConstructorImpl(BaseClassResolver);
  finally
    BaseClassResolver.Free;
  end;
end;

function TParsedClass.GetConstructorImpl(
  BaseClassResolver: TStringList): TParsedFunction;
var
  Module : TParsedModule;
  S: string;
  ErrMsg : string;
  CE : TCodeElement;
  i : integer;
  BaseClass : TBaseCodeElement;
begin
  Result := nil;
  Module := GetModule;
  S  := Module.Name + '.' + Parent.Name + '.' + Name;
  if BaseClassResolver.IndexOf(S) < 0 then begin
    BaseClassResolver.Add(S);
    try
      for i := 0 to ChildCount - 1 do begin
        CE := Children[i];
        if (CE.Name = '__init__') and (CE is TParsedFunction) then begin
          Result := TParsedFunction(CE);
          break;
        end;
      end;

      if not Assigned(Result) then begin
        // search superclasses
        for i := 0 to fSuperClasses.Count - 1 do begin
          BaseClass := PyScripterRefactor.FindDottedDefinition(fSuperClasses[i],
            Module, self.Parent as TCodeElement, ErrMsg);
          if not (Assigned(BaseClass) and (BaseClass is TParsedClass)) then continue;
          // we have found BaseClass
          Result := TParsedClass(BaseClass).GetConstructorImpl(BaseClassResolver);
          if Assigned(Result) then break;
        end;
      end;
    finally
      BaseClassResolver.Delete(BaseClassResolver.IndexOf(S));
    end;
  end;
end;

procedure TParsedClass.GetNameSpace(SList: TStringList);
var
  BaseClassResolver : TStringList;
begin
  BaseClassResolver := TStringList.Create;
  BaseClassResolver.CaseSensitive := True;
  try
    GetNameSpaceImpl(SList, BaseClassResolver);
  finally
    BaseClassResolver.Free;
  end;
end;

procedure TParsedClass.GetNameSpaceImpl(SList: TStringList;
      BaseClassResolver : TStringList);
Var
  i : integer;
  Module : TParsedModule;
  ErrMsg: string;
  BaseClass : TBaseCodeElement;
  S : string;
begin
  Module := GetModule;
  S  := Module.Name + '.' + Parent.Name + '.' + Name;
  if BaseClassResolver.IndexOf(S) < 0 then begin
    BaseClassResolver.Add(S);
    try
      inherited GetNameSpace(SList);
      // Add attributes
      for i := 0 to fAttributes.Count - 1 do
        SList.AddObject(TVariable(fAttributes[i]).Name, fAttributes[i]);
      if fSuperClasses.Count > 0 then begin
        for i := 0 to fSuperClasses.Count - 1 do begin
          BaseClass := PyScripterRefactor.FindDottedDefinition(fSuperClasses[i],
            Module, self.Parent as TCodeElement, ErrMsg);
          if not (Assigned(BaseClass) and (BaseClass is TParsedClass)) then continue;
          // we have found BaseClass
          TParsedClass(BaseClass).GetNameSpaceImpl(SList, BaseClassResolver);
        end;
      end;
    finally
      BaseClassResolver.Delete(BaseClassResolver.IndexOf(S));
    end;
  end;
end;

procedure TParsedClass.GetUniqueSortedAttibutes(AttributesList: TObjectList);
Var
  i, j : integer;
  HasName : boolean;
begin
  for i := 0 to fAttributes.Count - 1 do begin
    HasName := False;
    for j := 0 to AttributesList.Count - 1 do
      if TVariable(fAttributes[i]).Name = TVariable(AttributesList[j]).Name then begin
        HasName := True;
        break;
      end;
    if not HasName then
      AttributesList.Add(fAttributes[i]);
  end;
  AttributesList.Sort(CompareVariables);
end;

function CodeBlock(StartLine, EndLine : integer) : TCodeBlock;
begin
  Result.StartLine := StartLine;
  Result.EndLine := EndLine;
end;

function GetExpressionBuiltInType(Expr : string; Var IsBuiltIn : boolean) : string;
Var
  i :  integer;
begin
  Result := '';
  IsBuiltIn := False;

  Expr := Trim(Expr);
  if (Expr = '') or (Expr[1] > #$FF) then Exit;

  IsBuiltIn := True;
  case Expr[1] of
    '"','''' : Result := 'str';
    '0'..'9', '+', '-' :
      begin
        Result := 'int';
        for i := 2 to Length(Expr) - 1 do begin
          if Expr[i] = '.' then begin
            Result := 'float';
            break;
          end else if not CharInSet(Expr[i], ['0'..'9', '+', '-']) then
            break;
        end;
      end;
    '{' :
      if (CharPos(Expr, ',') = 0) or (CharPos(Expr, ':') <> 0) then
        Result := 'dict'
      else
        Result := 'set';
    '[': Result := 'list';
  else
    if (Expr[1] = '(') and (CharPos(Expr, ',') <> 0) then
      Result := 'tuple'  // speculative
    else
      IsBuiltIn := False;
  end;
end;

{ TBaseCodeElement }

function TBaseCodeElement.GetDottedName: string;
// Unique name in dotted notation;
begin
  if Assigned(Parent) then
    Result := Parent.GetDottedName + '.' + Name
  else
    Result := Name;
end;

function TBaseCodeElement.GetModule: TParsedModule;
begin
  Result := GetRoot as TParsedModule;
end;

function TBaseCodeElement.GetModuleSource: string;
var
  ParsedModule : TParsedModule;
begin
  ParsedModule := GetModule;
  if Assigned(ParsedModule) then
    Result := ParsedModule.Source
  else
    Result := '';
end;

function TBaseCodeElement.GetRoot: TBaseCodeElement;
begin
  Result := self;
  while Assigned(Result.fParent) do
    Result := Result.fParent;
end;

{ TVariable }

function TVariable.GetCodeHint: string;
Var
  Module : TParsedModule;
  Fmt, DefinedIn : string;
  ErrMsg : string;
  CE : TCodeElement;
begin
  Module := GetModule;
  if Module is TModuleProxy then
    DefinedIn := Format(_(SDefinedInModuleCodeHint), [Module.Name])
  else
    DefinedIn := Format(_(SFilePosInfoCodeHint),
      [Module.FileName, fCodePos.LineNo, fCodePos.CharOffset,
       Module.Name, fCodePos.LineNo]);

  if Parent is TParsedFunction then begin
    if [vaArgument, vaStarArgument, vaStarStarArgument, vaArgumentWithDefault] *
      Attributes <> []
    then
      Fmt := _(SFunctionParameterCodeHint)
    else
      Fmt := _(SLocalVariableCodeHint);
  end else if Parent is TParsedClass then begin
    if vaClassAttribute in Attributes then
      Fmt := _(SClassVariableCodeHint)
    else
      Fmt := _(SInstanceVariableCodeHint);
  end else if Parent is TParsedModule then begin
    Fmt := _(SGlobalVariableCodeHint);
  end else if Parent is TModuleImport then begin
    Fmt := _(SImportedVariableCodeHint);
  end else
    Fmt := '';
  Result := Format(Fmt,
    [Name, Parent.Name, DefinedIn]);

  CE := PyScripterRefactor.GetVarType(Self, ErrMsg);
  if Assigned(CE) then
    Result := Result + Format(_(SVariableTypeCodeHint), [CE.Name]);
end;

function TVariable.GetRealName: string;
begin
  if fRealName <> '' then
    Result := fRealName
  else
    Result := Name;
end;

{ TAsynchSourceScanner }

constructor TAsynchSourceScanner.Create(const FileName, Source: String);
begin
  inherited Create;
  fParsedModule := TParsedModule.Create(FileName, Source);
  fPythonScanner := TPythonScanner.Create;
  fPythonScanner.OnScannerProgress := ScanProgress;
  fFuture := TTask.Future<TParsedModule>(Self, FutureTask);
end;

destructor TAsynchSourceScanner.Destroy;
begin
  inherited;
  fFuture.Wait;
  FreeAndNil(fParsedModule);
  FreeAndNil(fPythonScanner);
  inherited;
end;

function TAsynchSourceScanner.Finished: Boolean;
begin
  Result := fFuture.Status in [TTaskStatus.Completed, TTaskStatus.Exception];
end;

function TAsynchSourceScanner.GetParsedModule: TParsedModule;
begin
  Result := fFuture.Value;
end;

procedure TAsynchSourceScanner.ScanProgress(CharNo, NoOfChars: integer;
  var Stop: Boolean);
begin
  if fStopped then Stop := True;
end;

procedure TAsynchSourceScanner.StopScanning;
begin
  fStopped := True;
end;

function TAsynchSourceScanner.FutureTask(Sender : TObject) : TParsedModule;
begin
  //StopWatch.Reset;
  //StopWatch.Start;
  if not fPythonScanner.ScanModule(fParsedModule) then
    FreeAndNil(fParsedModule);
  Result := fParsedModule;
  //StopWatch.Stop;
  //OutputDebugString(PWideChar(Format('ScanModule time in ms: %d', [StopWatch.ElapsedMilliseconds])));
end;

{ TAsynchSourceScannerFactory }

procedure TAsynchSourceScannerFactory.ClearFinished;
var
  i : integer;
  Scanner :  IAsyncSourceScanner;
begin
  fIList.Lock;
  try
    for i := fIList.Count - 1 downto 0 do begin
      Scanner := IAsyncSourceScanner(fIList[i]);
      if Scanner.Finished then
        fIList.Delete(i);
    end;
  finally
    fIList.UnLock;
  end;
end;

constructor TAsynchSourceScannerFactory.Create;
begin
  inherited;
  fIList := TInterfaceList.Create;
end;

function TAsynchSourceScannerFactory.CreateAsynchSourceScanner(const FileName,
  Source: String): IAsyncSourceScanner;
begin
  ClearFinished;
  Result := TAsynchSourceScanner.Create(FileName, Source);
  fIList.Add(Result);
end;

destructor TAsynchSourceScannerFactory.Destroy;
begin
  while fIList.Count > 0 do begin
    ClearFinished;
    Sleep(10);  // wait for threads to finish
  end;
  fIList.Free;
  inherited;
end;

procedure TAsynchSourceScannerFactory.ReleaseScanner(
  Scanner: IAsyncSourceScanner);
begin
  fIList.Remove(Scanner);
end;

initialization
  DocStringRE.Create('(?sm)^[ \t]*[ur]?(\"\"\"(.*?)\"\"\"|''''''(.*?)'''''')');
  DocStringRE.Study;

  AsynchSourceScannerFactory := TAsynchSourceScannerFactory.Create;
finalization
  FreeAndNil(AsynchSourceScannerFactory);
end.
