{-----------------------------------------------------------------------------
 Unit Name: cCodeCompletion
 Author:    Kiriakos Vlahos
 Purpose:   Code completion support classes
 History:
-----------------------------------------------------------------------------}

unit cCodeCompletion;

interface
Uses
  Winapi.Windows,
  System.Types,
  System.Classes,
  System.Contnrs,
  System.SyncObjs,
  SynEdit,
  SynEditTypes,
  SynEditHighlighter;

type
  TBaseCodeCompletionSkipHandler = class
    function SkipCodeCompletion(const Line, FileName : string; Caret : TBufferCoord;
      Highlighter : TSynCustomHighlighter; HighlighterAttr : TSynHighlighterAttributes) : Boolean; virtual; abstract;
  end;

  TBaseCodeCompletionHandler = class
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    function HandleCodeCompletion(const Line, FileName : string; Caret : TBufferCoord;
      Highlighter : TSynCustomHighlighter; HighlighterAttr : TSynHighlighterAttributes;
      out InsertText, DisplayText : string) : Boolean; virtual; abstract;
    function GetInfo(CCItem: string) : string; virtual; abstract;
  end;

  TCompletionInfo = record
    Editor: TSynEdit;
    CaretXY: TBufferCoord;
    InsertText,
    DisplayText : string;
    CompletionHandler: TBaseCodeCompletionHandler;
  end;

  TCodeCompletion = class
  public
    SkipHandlers : TObjectList;
    CompletionHandlers : TObjectList;
    CompletionInfo: TCompletionInfo;
    Lock: TRTLCriticalSection;
    constructor Create;
    destructor Destroy; override;
    procedure CleanUp;
    procedure RegisterSkipHandler(Handler : TBaseCodeCompletionSkipHandler);
    procedure RegisterCompletionHandler(Handler : TBaseCodeCompletionHandler);
  end;

  TBaseParamCompletion = class
  private type
    TCachedResults = record
      Editor: TSynEdit;
      StartX: Integer;
      Lookup, DisplayString, DocString: string;
    end;
  private
    FCachedResults: TCachedResults;
  protected
    AllowFunctionCalls: Boolean;
   function CallTipFromExpression(const Expr, FileName : string;
      const Line: integer; var DisplayString, DocString : string): Boolean; virtual; abstract;
  public
    function HandleParamCompletion(const FileName : string;
      Editor: TSynEdit; out DisplayString, DocString : string;
      out StartX: Integer): Boolean;
  end;

  TInterpreterParamCompletion = class(TBaseParamCompletion)
  protected
    function CallTipFromExpression(const Expr, FileName : string;
        const Line: integer; var DisplayString, DocString : string): Boolean; override;
  public
    constructor Create;
  end;

  TEditorParamCompletion = class(TBaseParamCompletion)
  protected
   function CallTipFromExpression(const Expr, FileName : string;
      const Line: integer; var DisplayString, DocString : string): Boolean; override;
  end;

  TIDECompletion = class
    class var EditorCodeCompletion: TCodeCompletion;
    class var InterpreterCodeCompletion: TCodeCompletion;
    class var InterpreterParamCompletion: TBaseParamCompletion;
    class var EditorParamCompletion: TBaseParamCompletion;
    class constructor Create;
    class destructor Destroy;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  System.Threading,
  System.RegularExpressions,
  VarPyth,
  JclStrings,
  JvGnugettext,
  dmCommands,
  SynHighlighterPython,
  StringResources,
  uEditAppIntfs,
  uCommonFunctions,
  cPyBaseDebugger,
  cPyDebugger,
  cPythonSourceScanner,
  cRefactoring,
  PythonEngine,
  cPyScripterSettings,
  cPySupportTypes,
  cInternalPython,
  cPyControl,
  cSSHSupport;

procedure GetModuleList(Path: Variant; out InsertText, DisplayText : string);
Var
  i: Integer;
  S: string;
  SortedNameSpace: TStringList;
begin
  SortedNameSpace := TStringList.Create;
  SortedNameSpace.CaseSensitive := True;
  SortedNameSpace.Duplicates := dupIgnore; // Remove duplicates
  SortedNameSpace.Sorted := True;
  try
    PyControl.InternalInterpreter.GetModulesOnPath(Path, SortedNameSpace);
    InsertText := SortedNameSpace.Text;
    for i := 0 to SortedNameSpace.Count - 1 do
    begin
      S := SortedNameSpace[i];
      DisplayText := DisplayText + Format('\Image{%d}\hspace{2}%s', [16, S]);
      if i < SortedNameSpace.Count - 1 then
        DisplayText := DisplayText + #10;
    end;
  finally
    SortedNameSpace.Free;
  end;
end;

procedure ProcessNamespace(SortedNameSpace, NameSpace : TStringList; out InsertText, DisplayText : string);
Var
  i: Integer;
  ImageIndex : integer;
  S: string;
  CE: TBaseCodeElement;
begin
  //fSortedNameSpace is freed in Close event
  SortedNameSpace.Sorted := True;
  SortedNameSpace.AddStrings(NameSpace);
  SortedNameSpace.Sorted := False;
  SortedNameSpace.CustomSort(ComparePythonIdents);
  InsertText := SortedNameSpace.Text;
  for i := 0 to SortedNameSpace.Count - 1 do
  begin
    S := SortedNameSpace[i];
    CE := SortedNameSpace.Objects[i] as TBaseCodeElement;
    if not Assigned(CE) then
    begin
      // Keyword
      ImageIndex := Integer(TCodeImages.Keyword);
      DisplayText := DisplayText + Format('\Image{%d}\hspace{8}\color{$FF8844}%s', [ImageIndex, S]);
    end
    else
    begin
      if (CE is TParsedModule) or (CE is TModuleImport) then
        ImageIndex := Integer(TCodeImages.Module)
      else if CE is TParsedFunction then
      begin
        if CE.Parent is TParsedClass then
          ImageIndex := Integer(TCodeImages.Method)
        else
          ImageIndex := Integer(TCodeImages.Func)
      end
      else if CE is TParsedClass then
        ImageIndex := Integer(TCodeImages.Klass)
      else
      begin // TVariable or TParsedVariable
        if Assigned(CE) and (CE.Parent is TParsedClass) then
          ImageIndex := Integer(TCodeImages.Field)
        else
          ImageIndex := Integer(TCodeImages.Variable);
      end;
      DisplayText := DisplayText + Format('\Image{%d}\hspace{8}%s', [ImageIndex, S]);
    end;
    if i < SortedNameSpace.Count - 1 then
      DisplayText := DisplayText + #10;
  end;
end;

{ TRegExpressions }
type
TRegExpressions = class
  class var RE_Import : TRegEx;
  class var RE_From : TRegEx;
  class var RE_FromImport : TRegEx;
  class var RE_For : TRegEx;
  class var RE_ImportAs : TRegEx;
  class var RE_FromImportAs : TRegEx;
  class var RE_String : TRegEx;
  class constructor Create;
end;

class constructor TRegExpressions.Create;
begin
  RE_Import := CompiledRegEx(
    Format('^\s*import +(%s( +as +%s)? *, *)*(%s)?$',
    [DottedIdentRE, IdentRE, DottedIdentRE]));
  RE_From := CompiledRegEx(
    Format('^\s*from +(\.*)(%s)?$', [DottedIdentRE]));
  RE_FromImport := CompiledRegEx(
    Format('^\s*from +(\.*)(%s)? +import +\(? *(%s( +as +%s)? *, *)*(%s)?$',
      [DottedIdentRE, IdentRE, IdentRE, IdentRE]));
  RE_For := CompiledRegEx(
    Format('^\s*for +(%s *, *)*(%s)?$', [IdentRE, IdentRE]));
  RE_ImportAs := CompiledRegEx(
    Format('^\s*import +(%s( +as +%s)? *, *)*%s +as +(%s)?$',
    [DottedIdentRE, IdentRE, DottedIdentRE, IdentRE]));
  RE_FromImportAs := CompiledRegEx(
    Format('^\s*from +(\.*)(%s)? +import +\(? *(%s( +as +%s)? *, *)*%s +as +(%s)?$',
      [DottedIdentRE, IdentRE, IdentRE, IdentRE, IdentRE]));
  RE_String := CompiledRegEx(
    Format('[''"]\.(%s)?$', [IdentRE, IdentRE]));
end;

{ TStringAndCommentSkipHandler }
type
  TStringAndCommentSkipHandler = class(TBaseCodeCompletionSkipHandler)
    function SkipCodeCompletion(const Line, FileName : string; Caret : TBufferCoord;
      Highlighter : TSynCustomHighlighter; HighlighterAttr : TSynHighlighterAttributes) : Boolean; override;
  end;

function TStringAndCommentSkipHandler.SkipCodeCompletion(const Line,
  FileName: string; Caret : TBufferCoord;
  Highlighter : TSynCustomHighlighter; HighlighterAttr: TSynHighlighterAttributes): Boolean;
begin
  Result :=  Assigned(HighlighterAttr) and
        ((HighlighterAttr = Highlighter.StringAttribute) or
        (HighlighterAttr = Highlighter.CommentAttribute) or
        ((Highlighter is TSynPythonSyn) and
        (HighlighterAttr = TSynPythonSyn(Highlighter).CodeCommentAttri) or
        (HighlighterAttr = TSynPythonSyn(Highlighter).MultiLineStringAttri) or
        (HighlighterAttr = TSynPythonSyn(Highlighter).DocStringAttri)));
end;


{ TForStatementSkipHandler }
type
  TForStatementSkipHandler = class(TBaseCodeCompletionSkipHandler)
    function SkipCodeCompletion(const Line, FileName : string; Caret : TBufferCoord;
      Highlighter : TSynCustomHighlighter; HighlighterAttr : TSynHighlighterAttributes) : Boolean; override;
  end;

function TForStatementSkipHandler.SkipCodeCompletion(const Line,
  FileName: string; Caret: TBufferCoord; Highlighter: TSynCustomHighlighter;
  HighlighterAttr: TSynHighlighterAttributes): Boolean;
begin
  Result := TRegExpressions.RE_For.IsMatch(Copy(Line, 1, Caret.Char - 1));
end;


{ TImportAsStatementSkipHandler }
type
  TImportAsStatementSkipHandler = class(TBaseCodeCompletionSkipHandler)
    function SkipCodeCompletion(const Line, FileName : string; Caret : TBufferCoord;
      Highlighter : TSynCustomHighlighter; HighlighterAttr : TSynHighlighterAttributes) : Boolean; override;
  end;

function TImportAsStatementSkipHandler.SkipCodeCompletion(const Line,
  FileName: string; Caret: TBufferCoord; Highlighter: TSynCustomHighlighter;
  HighlighterAttr: TSynHighlighterAttributes): Boolean;
begin
  Result := TRegExpressions.RE_ImportAs.IsMatch(Copy(Line, 1, Caret.Char - 1)) or
    TRegExpressions.RE_FromImportAs.IsMatch(Copy(Line, 1, Caret.Char - 1));
end;

{ TImportStatementHandler }
type
  TImportStatementHandler = class(TBaseCodeCompletionHandler)
  protected
    PythonPathAdder: IInterface;
    fFileName : string;
    fModulePrefix : string;
    fPathDepth : integer;
  public
    procedure Finalize; override;
    function HandleCodeCompletion(const Line, FileName : string; Caret : TBufferCoord;
      Highlighter : TSynCustomHighlighter; HighlighterAttr : TSynHighlighterAttributes;
      out InsertText, DisplayText : string) : Boolean; override;
    function GetInfo(CCItem: string) : string; override;
  end;

procedure TImportStatementHandler.Finalize;
begin
  inherited;
  PythonPathAdder := nil;
  fFileName := '';
  fModulePrefix := '';
  fPathDepth := 0;
end;

function TImportStatementHandler.GetInfo(CCItem: string): string;
Var
  Module : string;
  ParsedModule: TParsedModule;
begin
  Module := CCItem;
  if fModulePrefix <> '' then
    Module := fModulePrefix + '.' + Module;

  if PyScripterRefactor.InitializeQuery then
  try
    var Py := SafePyEngine;
    ParsedModule :=
      PyScripterRefactor.ResolveModuleImport(Module, fFileName, fPathDepth);
    if Assigned(ParsedModule) then
      Result := GetLineRange(ParsedModule.DocString, 1, 20);
  finally
    PyScripterRefactor.FinalizeQuery;
  end;
end;

function TImportStatementHandler.HandleCodeCompletion(const Line,
  FileName: string; Caret : TBufferCoord; Highlighter: TSynCustomHighlighter;
  HighlighterAttr: TSynHighlighterAttributes; out InsertText,
  DisplayText: string): Boolean;
Var
  Py: IPyEngineAndGIL;
  FNameVar : Variant;
  Dir, DottedModName, Server : string;
  Match : TMatch;
begin
  Match := TRegExpressions.RE_Import.Match(Copy(Line, 1, Caret.Char - 1));
  Result := Match.Success;
  if Result then
  begin
    Py := SafePyEngine;
    // autocomplete import statement
    fFileName := FileName;
    // Add the file path to the Python path - Will be automatically removed
    if not TSSHFileName.Parse(fFileName, Server, Dir) then
      Dir := ExtractFileDir(fFileName)
    else
      Dir := '';
    if Length(Dir) > 1 then
    begin
      PythonPathAdder :=  GI_PyControl.AddPathToInternalPythonPath(Dir);
    end;

    DottedModName := Match.GroupValue(3);
    if CharPos(DottedModName, '.') > 0 then begin
      fModulePrefix := DottedModName;
      Delete(fModulePrefix, LastDelimiter('.', fModulePrefix), MaxInt);
      FNameVar := TPyInternalInterpreter(PyControl.InternalInterpreter).PyInteractiveInterpreter.findModuleOrPackage(fModulePrefix, None);
      if not VarIsNone(FNameVar) then begin
        Dir := FNameVar;
        Dir := ExtractFileDir(Dir);
        GetModuleList(VarPythonCreate([Dir]), InsertText, DisplayText);
      end;
    end
    else
    begin
      fModulePrefix := '';
      GetModuleList(None, InsertText, DisplayText);
    end;
  end
end;

{ TFromStatementHandler }
type
  TFromStatementHandler = class(TImportStatementHandler)
  public
    function HandleCodeCompletion(const Line, FileName : string; Caret : TBufferCoord;
      Highlighter : TSynCustomHighlighter; HighlighterAttr : TSynHighlighterAttributes;
      out InsertText, DisplayText : string) : Boolean; override;
  end;

function TFromStatementHandler.HandleCodeCompletion(const Line,
  FileName: string; Caret : TBufferCoord; Highlighter: TSynCustomHighlighter;
  HighlighterAttr: TSynHighlighterAttributes; out InsertText,
  DisplayText: string): Boolean;
Var
  Py: IPyEngineAndGIL;
  i : integer;
  FNameVar : Variant;
  Dir, DottedModName, Server : string;
  SArray : TStringDynArray;
  Match : TMatch;
begin
  Match := TRegExpressions.RE_From.Match(Copy(Line, 1, Caret.Char - 1));
  Result := Match.Success;

  if Result then
  begin
    Py := SafePyEngine;
    // autocomplete from statement
    fModulePrefix := '';
    fFileName := FileName;
    fPathDepth := Match.GroupLength(1);
    DottedModName := Match.GroupValue(2);
    if fPathDepth > 0 then
    begin
      if (FileName = '') or TSSHFileName.Parse(FileName, Server, Dir) then begin
        Result := True;  // No completion
        Exit;
      end;

      // relative import
      Dir := fFileName;
      for i := 1 to fPathDepth do
        Dir := ExtractFileDir(Dir);
      PythonPathAdder := GI_PyControl.AddPathToInternalPythonPath(Dir);
      if (DottedModName <> '') and (CharPos(DottedModName, '.') > 0) then
      begin
        fModulePrefix := DottedModName;
        Delete(fModulePrefix, LastDelimiter('.', fModulePrefix), MaxInt);
        SArray := SplitString(DottedModName, '.');
        for i := Low(SArray) to High(SArray) - 1 do
          Dir := Dir + PathDelim + SArray[i];
      end;
      GetModuleList(VarPythonCreate([Dir]), InsertText, DisplayText);
    end
    else
    begin
      // Add the file path to the Python path - Will be automatically removed
      if (fFileName <> '') and not TSSHFileName.Parse(fFileName, Server, Dir) then
        PythonPathAdder := GI_PyControl.AddPathToInternalPythonPath
          (ExtractFileDir(fFileName));
      if CharPos(DottedModName, '.') > 0 then begin
        fModulePrefix := DottedModName;
        Delete(fModulePrefix, LastDelimiter('.', fModulePrefix), MaxInt);
        FNameVar := TPyInternalInterpreter(PyControl.InternalInterpreter).PyInteractiveInterpreter.findModuleOrPackage(fModulePrefix, None);
        if not VarIsNone(FNameVar) then begin
          Dir := FNameVar;
          Dir := ExtractFileDir(Dir);
          GetModuleList(VarPythonCreate([Dir]), InsertText, DisplayText);
        end;
      end
      else
      begin
        GetModuleList(None, InsertText, DisplayText);
      end;
    end;
  end;
end;

{ TFromImportModuleHandler }
type
  TFromImportModuleHandler = class(TImportStatementHandler)
  public
    function HandleCodeCompletion(const Line, FileName : string; Caret : TBufferCoord;
      Highlighter : TSynCustomHighlighter; HighlighterAttr : TSynHighlighterAttributes;
      out InsertText, DisplayText : string) : Boolean; override;
  end;

function TFromImportModuleHandler.HandleCodeCompletion(const Line,
  FileName: string; Caret: TBufferCoord; Highlighter: TSynCustomHighlighter;
  HighlighterAttr: TSynHighlighterAttributes; out InsertText,
  DisplayText: string): Boolean;
Var
  Py: IPyEngineAndGIL;
  Dir : string;
  i : integer;
  ParsedModule : TParsedModule;
  Match : TMatch;
begin
  Match :=  TRegExpressions.RE_FromImport.Match(Copy(Line, 1, Caret.Char - 1));
  Result := Match.Success;

  if Result then
  begin
    Py := SafePyEngine;
    // autocomplete from statement
    fPathDepth := Match.GroupLength(1);
    if Match.GroupLength(2) > 0 then
    begin
      // from ...module import identifiers
      fFileName := FileName;
      if PyScripterRefactor.InitializeQuery then
      try
        ParsedModule := PyScripterRefactor.ResolveModuleImport
          (Match.GroupValue(2), fFileName, fPathDepth);
        if Assigned(ParsedModule) then
        begin
          if (fPathDepth > 0) and ParsedModule.IsPackage then
          begin
            Dir := ExtractFileDir(ParsedModule.FileName);
            if Dir <> '' then
              GetModuleList(VarPythonCreate([Dir]), InsertText, DisplayText);
          end
          else
            // will be handled by FromImportNamespace
            Result := False
        end;
      finally
        PyScripterRefactor.FinalizeQuery;
      end;
    end
    else
    begin
      // from ... import modules
      fModulePrefix := '';
      fFileName := FileName;
      if fPathDepth > 0 then
      begin
        if FileName = '' then begin
          Result := True;  // No completion
          Exit;
        end;
        Dir := fFileName;
        for i := 1 to fPathDepth do
          Dir := ExtractFileDir(Dir);
        GetModuleList(VarPythonCreate([Dir]), InsertText, DisplayText);
      end;
    end;
  end
end;

{ TNamespaceCompletionHandler }
type
  TNamespaceCompletionHandler = class(TBaseCodeCompletionHandler)
  protected
    FFileName : string;
    FSortedNameSpace : TStringList;
    FRefactorInitialized: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Finalize; override;
    function HandleCodeCompletion(const Line, FileName : string; Caret : TBufferCoord;
      Highlighter : TSynCustomHighlighter; HighlighterAttr : TSynHighlighterAttributes;
      out InsertText, DisplayText : string) : Boolean; override;
    function GetInfo(CCItem: string) : string; override;
  end;

constructor TNamespaceCompletionHandler.Create;
begin
  FSortedNameSpace := TStringList.Create;
  FSortedNameSpace.CaseSensitive := True;
  FSortedNameSpace.Duplicates := dupIgnore; // Remove duplicates
end;

destructor TNamespaceCompletionHandler.Destroy;
begin
  FSortedNameSpace.Free;
  inherited;
end;

procedure TNamespaceCompletionHandler.Finalize;
begin
  inherited;
  FFileName := '';
  FSortedNameSpace.Clear;
  if FRefactorInitialized then
    PyScripterRefactor.FinalizeQuery;
  FRefactorInitialized := False;
end;

function TNamespaceCompletionHandler.GetInfo(CCItem: string): string;
Var
  Index : integer;
  CE : TBaseCodeElement;
begin
  Index := FSortedNameSpace.IndexOf(CCItem);
  if Index >=0  then
  begin
    CE := FSortedNameSpace.Objects[Index] as TBaseCodeElement;
    if not Assigned(CE) then
      Result := _(SPythonKeyword)
    else if Assigned(CE) and (CE is TCodeElement) then
      Result := GetLineRange(TCodeElement(CE).DocString, 1, 20);
  end;
end;

function TNamespaceCompletionHandler.HandleCodeCompletion(const Line,
  FileName: string; Caret : TBufferCoord; Highlighter: TSynCustomHighlighter;
  HighlighterAttr: TSynHighlighterAttributes; out InsertText,
  DisplayText: string): Boolean;
Var
  TmpX, Index : Integer;
  lookup, ErrMsg, Server, Dir: string;
  ParsedModule, ParsedBuiltInModule: TParsedModule;
  Scope: TCodeElement;
  Def: TBaseCodeElement;
  NameSpace: TStringList;
  PythonPathAdder : IInterface;
begin
  FFileName := FileName;
  InsertText := '';
  DisplayText := '';

  TmpX := Caret.Char;
  if TmpX > Length(Line) then
    TmpX := Length(Line)
  else
    Dec(TmpX);

  lookup := GetWordAtPos(Line, TmpX, IdentChars + ['.'], True, False, True);
  Index := CharLastPos(lookup, WideChar('.'));

  // Add the file path to the Python path - Will be automatically removed
  if not TSSHFileName.Parse(FFileName, Server, Dir) then
    Dir := ExtractFileDir(FFileName)
  else
    Dir := '';
  if Length(Dir) > 1 then
  begin
    PythonPathAdder :=  GI_PyControl.AddPathToInternalPythonPath(Dir);
  end;

  if PyScripterRefactor.InitializeQuery then
  begin
    FRefactorInitialized := True;
    // GetParsedModule
    ParsedModule := PyScripterRefactor.GetParsedModule(FFileName, []);
    Scope := nil;
    if Assigned(ParsedModule) then
      Scope := ParsedModule.GetScopeForLine(Caret.Line);
    if Assigned(ParsedModule) and Assigned(Scope) then
    begin
      NameSpace := TStringList.Create;
      try
        if Index > 0 then
        begin
          lookup := Copy(lookup, 1, Index - 1);
          Def := PyScripterRefactor.FindDottedDefinition(lookup,
            ParsedModule, Scope, ErrMsg);
          if Assigned(Def) and (Def.ClassType = TVariable) then
            Def := PyScripterRefactor.GetVarType(TVariable(Def), ErrMsg);
          if Assigned(Def) and (Def.ClassType = TParsedFunction) then
            Def := PyScripterRefactor.GetFuncReturnType(TParsedFunction(Def), ErrMsg);
          if Assigned(Def) then (Def as TCodeElement).GetNamespace(NameSpace);
        end else begin
          // extract namespace from current scope and its parents
          while Assigned(Scope) do
          begin
            Scope.GetNamespace(NameSpace);
            Scope := Scope.Parent as TCodeElement;
            while Assigned(Scope) and (Scope is TParsedClass) do
              // Class namespace not visible to class functions and nested classes  #672
              Scope := Scope.Parent as TCodeElement;
          end;
          // builtins
          ParsedBuiltInModule := PyScripterRefactor.GetParsedModule(GetPythonEngine.BuiltInModuleName, []);
          ParsedBuiltInModule.GetNamespace(NameSpace);
          if PyIDEOptions.CompleteKeywords then
          begin
            for var I := 0 to (Highlighter as TSynPythonSyn).Keywords.Count - 1 do
            begin
              if TtkTokenKind(TSynPythonSyn(Highlighter).Keywords.Objects[I]) = tkKey then
                NameSpace.Add(TSynPythonSyn(Highlighter).Keywords[I]);
            end;
          end
        end;
        ProcessNamespace(FSortedNameSpace, NameSpace, InsertText, DisplayText);
      finally
        NameSpace.Free;
      end;
    end;
  end;
  Result := InsertText <> '';
end;


{ TStringCompletionHandler }
type
  TStringCompletionHandler = class(TNamespaceCompletionHandler)
    function HandleCodeCompletion(const Line, FileName : string; Caret : TBufferCoord;
      Highlighter : TSynCustomHighlighter; HighlighterAttr : TSynHighlighterAttributes;
      out InsertText, DisplayText : string) : Boolean; override;
  end;

function TStringCompletionHandler.HandleCodeCompletion(const Line,
  FileName: string; Caret: TBufferCoord; Highlighter: TSynCustomHighlighter;
  HighlighterAttr: TSynHighlighterAttributes; out InsertText,
  DisplayText: string): Boolean;
Var
  ParsedBuiltInModule: TParsedModule;
  NameSpace: TStringList;
  Index : integer;
  CE : TCodeElement;
begin
  Result :=  TRegExpressions.RE_String.IsMatch(Copy(Line, 1, Caret.Char - 1));
  if Result then begin
    if PyScripterRefactor.InitializeQuery then
    begin
      FRefactorInitialized := True;
      // GetParsedModule
      ParsedBuiltInModule := PyScripterRefactor.GetParsedModule(GetPythonEngine.BuiltInModuleName, []);
      if Assigned(ParsedBuiltInModule) then begin
        NameSpace := TStringList.Create;
        try
          ParsedBuiltInModule.GetNameSpace(NameSpace);
          Index := NameSpace.IndexOf('str');
          if Index > 0 then
          begin
            CE := NameSpace.Objects[Index] as TCodeElement;
            NameSpace.Clear;
            CE.GetNameSpace(NameSpace);
            ProcessNamespace(FSortedNameSpace, NameSpace, InsertText, DisplayText);
          end;
        finally
          NameSpace.Free;
        end;
      end;
    end;
  end;
end;

{ TFromImportNamespaceHandler }
type
  TFromImportNamespaceHandler = class(TNamespaceCompletionHandler)
    function HandleCodeCompletion(const Line, FileName : string; Caret : TBufferCoord;
      Highlighter : TSynCustomHighlighter; HighlighterAttr : TSynHighlighterAttributes;
      out InsertText, DisplayText : string) : Boolean; override;
  end;

function TFromImportNamespaceHandler.HandleCodeCompletion(const Line,
  FileName: string; Caret: TBufferCoord; Highlighter: TSynCustomHighlighter;
  HighlighterAttr: TSynHighlighterAttributes; out InsertText,
  DisplayText: string): Boolean;
Var
  ParsedModule : TParsedModule;
  NameSpace : TStringList;
  fPathDepth : integer;
  Match : TMatch;
begin
  Match :=  TRegExpressions.RE_FromImport.Match(Copy(Line, 1, Caret.Char - 1));
  Result := Match.Success;

  if Result then
  begin
    // autocomplete from statement
    fPathDepth := Match.GroupLength(1);
    if (fPathDepth > 0) and (FileName = '') then begin
      Result := True; // No Completion
      Exit;
    end;
    if Match.GroupLength(2) > 0 then
    begin
      // from ...module import identifiers
      FFileName := FileName;
      if PyScripterRefactor.InitializeQuery then
      begin
        FRefactorInitialized := True;
        ParsedModule := PyScripterRefactor.ResolveModuleImport
          (Match.GroupValue(2), FFileName, fPathDepth);
        if Assigned(ParsedModule) then
        begin
          if (fPathDepth > 0) and ParsedModule.IsPackage then
            // will be handled by FromImportModules
            Result := False
          else
          begin
            NameSpace := TStringList.Create;
            try
              ParsedModule.GetNamespace(NameSpace);
              ProcessNamespace(FSortedNameSpace, NameSpace, InsertText, DisplayText);
            finally
              NameSpace.Free;
            end;
          end;
        end;
      end;
    end
    else
    begin
      Result := False;
    end;
  end
end;

{ TLiveNamespaceCompletionHandler }
type
  TLiveNamespaceCompletionHandler = class(TBaseCodeCompletionHandler)
  private
    fPyNameSpace : TBaseNameSpaceItem;
    fNameSpace : TStringList;
  public
    procedure Finalize; override;
    function HandleCodeCompletion(const Line, FileName : string; Caret : TBufferCoord;
      Highlighter : TSynCustomHighlighter; HighlighterAttr : TSynHighlighterAttributes;
      out InsertText, DisplayText : string) : Boolean; override;
    function GetInfo(CCItem: string) : string; override;
  end;

procedure TLiveNamespaceCompletionHandler.Finalize;
begin
  FreeAndNil(fNameSpace);
  if Assigned(fPyNameSpace) then
  begin
    var Py := SafePyEngine;
    FreeAndNil(fPyNameSpace);
  end;
end;

function TLiveNamespaceCompletionHandler.GetInfo(CCItem: string): string;
Var
  Index: integer;
  NameSpaceItem: TBaseNameSpaceItem;
begin
  Index := fNameSpace.IndexOf(CCItem);
  if Index >=0  then
  begin
    NameSpaceItem := fNameSpace.Objects[Index] as TBaseNameSpaceItem;
    if not Assigned(NameSpaceItem) then
      Result := _(SPythonKeyword)
    else
    begin
      var Py := SafePyEngine;
      Result := GetLineRange(NameSpaceItem.DocString, 1, 20);
    end;
  end;
end;

function TLiveNamespaceCompletionHandler.HandleCodeCompletion(const Line,
  FileName: string; Caret: TBufferCoord; Highlighter: TSynCustomHighlighter;
  HighlighterAttr: TSynHighlighterAttributes; out InsertText,
  DisplayText: string): Boolean;
Var
  I, TmpX, Index, ImageIndex : integer;
  lookup : string;
  NameSpaceItem : TBaseNameSpaceItem;
begin
  // Clean-up of FNameSpace and FNameSpaceDict takes place in the Close event
  TmpX := Caret.Char;
  if TmpX > length(Line) then
    TmpX := length(Line)
  else dec(TmpX);

  lookup := GetWordAtPos(Line, TmpX, IdentChars+['.'], True, False, True);
  Index := CharLastPos(lookup, '.');
  fPyNameSpace := nil;
  if Index > 0 then begin
    lookup := Copy(lookup, 1, Index-1);
    if CharPos(lookup, ')') > 0 then
      lookup := ''  // Issue 422  Do not evaluate functions
    else if IsDigits(lookup) then
      lookup := ''  // Issue 478  User is typing a number
  end else
    lookup := '';  // Completion from global namespace

  var Py := SafePyEngine;
  if (Index <= 0) or (lookup <> '') then begin
    if GI_PyControl.Inactive then
      fPyNameSpace := PyControl.ActiveInterpreter.NameSpaceFromExpression(lookup)
    else if GI_PyControl.PythonLoaded and not GI_PyControl.Running then
      fPyNameSpace := PyControl.ActiveDebugger.NameSpaceFromExpression(lookup);
  end;

  DisplayText := '';
  InsertText := '';

  fNameSpace := TStringList.Create;
  if Assigned(fPyNameSpace) then begin
    if lookup <> '' then begin
      // fNameSpace corresponds to a Python object not a dict
      fPyNameSpace.ExpandCommonTypes := True;
      fPyNameSpace.ExpandSequences := False;
    end;
    for I := 0 to fPyNameSpace.ChildCount - 1 do begin
      NameSpaceItem := fPyNameSpace.ChildNode[I];
      fNameSpace.AddObject(NameSpaceItem.Name, NameSpaceItem);
    end;
  end;
  if (lookup = '') and PyIDEOptions.CompleteKeywords then begin
    // only add keywords to the completion of the global namespace
    for I := 0 to (Highlighter as TSynPythonSyn).Keywords.Count - 1 do
    begin
      if TtkTokenKind(TSynPythonSyn(Highlighter).Keywords.Objects[I]) = tkKey then
        fNameSpace.Add(TSynPythonSyn(Highlighter).Keywords[I]);
    end;
  end;

  fNameSpace.CustomSort(ComparePythonIdents);

  for I := 0 to fNameSpace.Count - 1 do begin
    InsertText := InsertText + fNameSpace[I];

    NameSpaceItem := fNameSpace.Objects[I] as TBaseNameSpaceItem;
    if not Assigned(NameSpaceItem) then
       DisplayText := DisplayText + Format('\Image{%d}\hspace{8}\color{$FF8844}%s',
         [Integer(TCodeImages.Keyword), fNameSpace[I]])
    else
    begin
      if NameSpaceItem.IsModule then
        ImageIndex := Integer(TCodeImages.Module)
      else if NameSpaceItem.IsMethod
           {or NameSpaceItem.IsMethodDescriptor} then
        ImageIndex := Integer(TCodeImages.Method)
      else if NameSpaceItem.IsFunction
           {or NameSpaceItem.IsBuiltin} then
        ImageIndex := Integer(TCodeImages.Func)
      else if NameSpaceItem.IsClass then
        ImageIndex := Integer(TCodeImages.Klass)
      else begin
        if Index > 0 then
          ImageIndex := Integer(TCodeImages.Field)
        else
          ImageIndex := Integer(TCodeImages.Variable);
      end;
      DisplayText := DisplayText + Format('\Image{%d}\hspace{8}%s', [ImageIndex, NameSpaceItem.Name]);
    end;
    if I < fNameSpace.Count - 1 then begin
      DisplayText := DisplayText + #10;
      InsertText := InsertText + #10;
    end;
  end;
  Result := InsertText <> '';
end;

{ TCodeCompletion }

procedure TCodeCompletion.CleanUp;
begin
  Lock.Enter;
  try
    CompletionInfo.InsertText := '';
    CompletionInfo.DisplayText := '';
    CompletionInfo.Editor := nil;
    CompletionInfo.CaretXY := BufferCoord(0,9);
    if Assigned(CompletionInfo.CompletionHandler) then
    begin
      CompletionInfo.CompletionHandler.Finalize;
      CompletionInfo.CompletionHandler := nil;
    end;
  finally
    Lock.Leave;
  end;
end;

constructor TCodeCompletion.Create;
begin
  SkipHandlers := TObjectList.Create(True);
  CompletionHandlers := TObjectList.Create(True);
  Lock.Initialize;
end;

destructor TCodeCompletion.Destroy;
begin
  Lock.Enter;
  Lock.Leave;
  Lock.Free;
  SkipHandlers.Free;
  CompletionHandlers.Free;
end;

procedure TCodeCompletion.RegisterCompletionHandler(Handler : TBaseCodeCompletionHandler);
begin
  CompletionHandlers.Add(Handler);
end;

procedure TCodeCompletion.RegisterSkipHandler(Handler : TBaseCodeCompletionSkipHandler);
begin
  SkipHandlers.Add(Handler);
end;

{ TBaseCodeCompletionHandler }

procedure TBaseCodeCompletionHandler.Finalize;
begin
end;

procedure TBaseCodeCompletionHandler.Initialize;
begin
end;

{ TBaseParamCompletion }

function TBaseParamCompletion.HandleParamCompletion(const FileName: string;
  Editor: TSynEdit; out DisplayString, DocString: string;
  out StartX: Integer): Boolean;
const
  ParamCompletionWaitTime = 2000;  //ms
var
  LocLine, Lookup: string;
  TmpX, ParenCounter: Integer;
  FoundMatch : Boolean;
  DummyToken: string;
  Attr: TSynHighlighterAttributes;
  Caret: TBufferCoord;
  Highlighter: TSynPythonSyn;
begin
  DisplayString := '';
  DocString := '';
  Caret := Editor.CaretXY;
  Result := False;

  Highlighter := Editor.Highlighter as TSynPythonSyn;
  if Editor.GetHighlighterAttriAtRowCol(Caret, DummyToken, Attr) and
   ({(attr = Highlighter.StringAttribute) or} (attr = Highlighter.CommentAttribute) or
    (Attr = Highlighter.CodeCommentAttri) or (Attr = Highlighter.MultiLineStringAttri) or
    (Attr = Highlighter.DocStringAttri))
   then
    // Do not code complete inside doc strings, code or multiline comments
    Exit;

  LocLine := Editor.LineText;

  //go back from the cursor and find the first open paren
  StartX := Caret.Char;
  TmpX := Caret.Char;
  if TmpX > length(LocLine)
  then
    TmpX := length(LocLine)
  else
    dec(TmpX);

  FoundMatch := False;
  while (TmpX > 0) and not(FoundMatch) do
  begin
    Editor.GetHighlighterAttriAtRowCol(BufferCoord(TmpX, Caret.Line), DummyToken, Attr);
    if (Attr = Highlighter.StringAttri) or (Attr = Highlighter.SpaceAttri)
    then
      Dec(TmpX)
    else if LocLine[TmpX] = ')' then
    begin
      //We found a close, go till it's opening paren
      ParenCounter := 1;
      dec(TmpX);
      while (TmpX > 0) and (ParenCounter > 0) do
      begin
        if LocLine[TmpX] = ')' then inc(ParenCounter)
        else if LocLine[TmpX] = '(' then dec(ParenCounter);
        dec(TmpX);
      end;
    end else if LocLine[TmpX] = '(' then
    begin
      //we have a valid open paren, lets see what the word before it is
      StartX := TmpX;
      Dec(TmpX);
      while (TmpX > 0) and (LocLine[TmpX] = ' ') do
        Dec(TmpX);
      if TmpX > 0 then
      begin
        Lookup := GetWordAtPos(LocLine, TmpX, IdentChars+['.'], True, False, True);

        if (Lookup <> '') and (Lookup[1] = '.') and
           (TmpX > Length(Lookup)) and
           CharInSet(LocLine[TmpX - Length(Lookup)], ['''', '"'])
        then
          Lookup := 'str' + Lookup;

        FoundMatch :=
          (AllowFunctionCalls or (CharPos(Lookup, ')') <= 0)) // Issue 422  Do not evaluate functions
          and (Lookup <> '');

        if FoundMatch then
        begin
          // Use CachedResults
          if (Editor = FCachedResults.Editor) and (Lookup = FCachedResults.Lookup)
            and (StartX = FCachedResults.StartX)  then
          begin
            DisplayString := FCachedResults.DisplayString;
            DocString := FCachedResults.DocString;
          end
          else
          begin
            var DispS, DocS: string;
            var Task := TTask.Create(procedure
            begin
              FoundMatch := CallTipFromExpression(Lookup, FileName, Caret.Line, DispS, DocS);
            end).Start;
            if Task.Wait(ParamCompletionWaitTime) then
            begin
              if FoundMatch then
              begin
                DisplayString := DispS;
                DocString := DocS;
                // cache the results
                FCachedResults.Editor := Editor;
                FCachedResults.StartX := StartX;
                FCachedResults.Lookup := Lookup;
                FCachedResults.DisplayString := DisplayString;
                FCachedResults.DocString := DocString;
              end;
            end
            else
            begin
              if Self is TEditorParamCompletion then
                PyScripterRefactor.Cancel;
              Exit; // We got stuck!
            end;
          end;
        end;

        if not(FoundMatch) then
        begin
          TmpX := StartX;
          dec(TmpX);
        end;
      end;
    end
    else
      dec(TmpX)
  end;
  Result := FoundMatch;
end;


{ TInterpreterParamCompletion }

function TInterpreterParamCompletion.CallTipFromExpression(const Expr, FileName : string;
  const Line: integer; var DisplayString, DocString : string): Boolean;
begin
  Result := PyControl.ActiveInterpreter.CallTipFromExpression(
    Expr, DisplayString, DocString);
end;

constructor TInterpreterParamCompletion.Create;
begin
  inherited;
  AllowFunctionCalls := True;
end;

{ TEditorParamCompletion }

function TEditorParamCompletion.CallTipFromExpression(const Expr,
  FileName: string; const Line: integer; var DisplayString,
  DocString: string): Boolean;
var
  Scope: TCodeElement;
  Def: TBaseCodeElement;
  ParsedModule: TParsedModule;
  PythonPathAdder: IInterface;
  ErrMsg: string;
begin
  DisplayString := '';
  DocString := '';
  Result := False;
  // Add the file path to the Python path - Will be automatically removed
  if FileExists(FileName) then
    PythonPathAdder := GI_PyControl.AddPathToInternalPythonPath
      (ExtractFileDir(FileName));

  if PyScripterRefactor.InitializeQuery then
  try
    // GetParsedModule
    ParsedModule := PyScripterRefactor.GetParsedModule(FileName, []);
    Scope := nil;
    if Assigned(ParsedModule) then
      Scope := ParsedModule.GetScopeForLine(Line);
    if Assigned(ParsedModule) and Assigned(Scope) then
    begin
      Def := PyScripterRefactor.FindDottedDefinition(Expr,
        ParsedModule, Scope, ErrMsg);

      if Assigned(Def) and (Def is TParsedClass) and
        not (Def.GetModule.Name = GetPythonEngine.BuiltInModuleName)
      then
        Def := TParsedClass(Def).GetConstructor;

      if Assigned(Def) and ((Def is TParsedFunction) or (Def is TParsedClass)) then
      begin
        Result := True;
        if Def is TParsedFunction then begin
          DisplayString := TParsedFunction(Def).ArgumentsString;
          // Remove self arguments from methods
          if StrIsLeft(PWideChar(DisplayString), 'self') then
            Delete(DisplayString, 1, 4);
          if StrIsLeft(PWideChar(DisplayString), ', ') then
            Delete(DisplayString, 1, 2);
          DocString := TParsedFunction(Def).DocString;
        end else if Def is TParsedClass then begin
           DisplayString := '';
           DocString := TParsedClass(Def).DocString;
        end;
      end;
    end;
  finally
    PyScripterRefactor.FinalizeQuery;
  end;
end;

{ TIDECompletion }

class constructor TIDECompletion.Create;
begin
  InterpreterParamCompletion := TInterpreterParamCompletion.Create;
  EditorParamCompletion := TEditorParamCompletion.Create;

  EditorCodeCompletion := TCodeCompletion.Create;
  InterpreterCodeCompletion := TCodeCompletion.Create;

  //  Register handlers - Order may be important
  EditorCodeCompletion.RegisterSkipHandler(TStringAndCommentSkipHandler.Create);
  EditorCodeCompletion.RegisterSkipHandler(TForStatementSkipHandler.Create);
  EditorCodeCompletion.RegisterSkipHandler(TImportAsStatementSkipHandler.Create);

  EditorCodeCompletion.RegisterCompletionHandler(TImportStatementHandler.Create);
  EditorCodeCompletion.RegisterCompletionHandler(TFromStatementHandler.Create);
  EditorCodeCompletion.RegisterCompletionHandler(TFromImportModuleHandler.Create);
  EditorCodeCompletion.RegisterCompletionHandler(TFromImportNamespaceHandler.Create);
  EditorCodeCompletion.RegisterCompletionHandler(TStringCompletionHandler.Create);
  EditorCodeCompletion.RegisterCompletionHandler(TNamespaceCompletionHandler.Create);

  InterpreterCodeCompletion.RegisterSkipHandler(TStringAndCommentSkipHandler.Create);
  InterpreterCodeCompletion.RegisterSkipHandler(TForStatementSkipHandler.Create);
  InterpreterCodeCompletion.RegisterSkipHandler(TImportAsStatementSkipHandler.Create);

  InterpreterCodeCompletion.RegisterCompletionHandler(TImportStatementHandler.Create);
  InterpreterCodeCompletion.RegisterCompletionHandler(TFromStatementHandler.Create);
  InterpreterCodeCompletion.RegisterCompletionHandler(TFromImportModuleHandler.Create);
  InterpreterCodeCompletion.RegisterCompletionHandler(TFromImportNamespaceHandler.Create);
  InterpreterCodeCompletion.RegisterCompletionHandler(TStringCompletionHandler.Create);
  InterpreterCodeCompletion.RegisterCompletionHandler(TLiveNamespaceCompletionHandler.Create);
end;

class destructor TIDECompletion.Destroy;
begin
  EditorCodeCompletion.Free;
  InterpreterCodeCompletion.Free;
  InterpreterParamCompletion.Free;
  EditorParamCompletion.Free;
end;


end.
