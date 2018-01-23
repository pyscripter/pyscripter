{-----------------------------------------------------------------------------
 Unit Name: cCodeCompletion
 Author:    Kiriakos Vlahos
 Purpose:   Code completion support classes
 History:
-----------------------------------------------------------------------------}

unit cCodeCompletion;

interface
Uses
  Types,
  Classes,
  Contnrs,
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

  TCodeCompletion = class
    SkipHandlers : TObjectList;
    CompletionHandlers : TObjectList;
    constructor Create;
    destructor Destroy; override;
    procedure RegisterSkipHandler(Handler : TBaseCodeCompletionSkipHandler);
    procedure RegisterCompletionHandler(Handler : TBaseCodeCompletionHandler);
  end;

  TIDECompletion = class
    class var EditorCodeCompletion : TCodeCompletion;
    class var InterpreterCodeCompletion : TCodeCompletion;
    class constructor Create;
    class destructor Destroy;
  end;

implementation

uses
  SysUtils,
  StrUtils,
  SynHighlighterPython,
  SynRegExpr,
  uCommonFunctions,
  cPyBaseDebugger,
  cPyDebugger,
  cPythonSourceScanner,
  cRefactoring,
  dmCommands,
  VarPyth,
  JclStrings, PythonEngine, StringResources, JvGnugettext,
  cPyScripterSettings;

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
    InternalInterpreter.GetModulesOnPath(Path, SortedNameSpace);
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
      DisplayText := DisplayText + Format('\Image{%d}\hspace{2}\color{clHotlight}%s', [ImageIndex, S]);
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
      DisplayText := DisplayText + Format('\Image{%d}\hspace{2}%s',
        [ImageIndex, S]);
    end;
    if i < SortedNameSpace.Count - 1 then
      DisplayText := DisplayText + #10;
  end;
end;

{ TRegExpressions }
type
TRegExpressions = class
  class var RE_Import : TRegExpr;
  class var RE_From : TRegExpr;
  class var RE_FromImport : TRegExpr;
  class var RE_For : TRegExpr;
  class var RE_ImportAs : TRegExpr;
  class var RE_FromImportAs : TRegExpr;
  class var RE_String : TRegExpr;
  class constructor Create;
  class destructor Destroy;
end;

class constructor TRegExpressions.Create;
begin
  RE_Import := CompiledRegExpr(
    Format('^\s*import +(%s( +as +%s)? *, *)*(%s)?$',
    [DottedIdentRe, IdentRE, DottedIdentRE]));
  RE_From := CompiledRegExpr(
    Format('^\s*from +(\.*)(%s)?$', [DottedIdentRE]));
  RE_FromImport := CompiledRegExpr(
    Format('^\s*from +(\.*)(%s)? +import +\(? *(%s( +as +%s)? *, *)*(%s)?$',
      [DottedIdentRe, IdentRE, IdentRe, IdentRe]));
  RE_For := CompiledRegExpr(
    Format('^\s*for +(%s *, *)*(%s)?$', [IdentRe, IdentRe]));
  RE_ImportAs := CompiledRegExpr(
    Format('^\s*import +(%s( +as +%s)? *, *)*%s +as +(%s)?$',
    [DottedIdentRe, IdentRE, DottedIdentRE, IdentRE]));
  RE_FromImportAs := CompiledRegExpr(
    Format('^\s*from +(\.*)(%s)? +import +\(? *(%s( +as +%s)? *, *)*%s +as +(%s)?$',
      [DottedIdentRe, IdentRE, IdentRe, IdentRe, IdentRE]));
  RE_String := CompiledRegExpr(
    Format('[''"]\.(%s)?$', [IdentRe, IdentRe]));
end;

class destructor TRegExpressions.Destroy;
begin
  RE_Import.Free;
  RE_From.Free;
  RE_FromImport.Free;
  RE_For.Free;
  RE_ImportAs.Free;
  RE_FromImportAs.Free;
  RE_String.Free;
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
  Result := TRegExpressions.RE_For.Exec(Copy(Line, 1, Caret.Char - 1));
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
  Result := TRegExpressions.RE_ImportAs.Exec(Copy(Line, 1, Caret.Char - 1)) or
    TRegExpressions.RE_FromImportAs.Exec(Copy(Line, 1, Caret.Char - 1));
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
  begin
    ParsedModule :=
      PyScripterRefactor.ResolveModuleImport(Module, fFileName, fPathDepth);
    if Assigned(ParsedModule) then
      Result := GetLineRange(ParsedModule.DocString, 1, 20);
    PyScripterRefactor.FinalizeQuery;
  end;
end;

function TImportStatementHandler.HandleCodeCompletion(const Line,
  FileName: string; Caret : TBufferCoord; Highlighter: TSynCustomHighlighter;
  HighlighterAttr: TSynHighlighterAttributes; out InsertText,
  DisplayText: string): Boolean;
Var
  FNameVar : Variant;
  Dir, DottedModName : string;
begin
  Result := TRegExpressions.RE_Import.Exec(Copy(Line, 1, Caret.Char - 1));
  if Result then
  begin
    // autocomplete import statement
    fFileName := FileName;
    // Add the file path to the Python path - Will be automatically removed
    if fFileName <> '' then
      PythonPathAdder := InternalInterpreter.AddPathToPythonPath
        (ExtractFileDir(fFileName));

    DottedModName := TRegExpressions.RE_Import.Match[3];
    if CharPos(DottedModName, '.') > 0 then begin
      fModulePrefix := DottedModName;
      Delete(fModulePrefix, LastDelimiter('.', fModulePrefix), MaxInt);
      FNameVar := InternalInterpreter.PyInteractiveInterpreter.findModuleOrPackage(fModulePrefix, None);
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
  i : integer;
  FNameVar : Variant;
  Dir, DottedModName : string;
  SArray : TStringDynArray;
begin
  Result := TRegExpressions.RE_From.Exec(Copy(Line, 1, Caret.Char - 1));

  if Result then
  begin
    // autocomplete from statement
    fModulePrefix := '';
    fFileName := FileName;
    fPathDepth := TRegExpressions.RE_From.MatchLen[1];
    DottedModName := TRegExpressions.RE_From.Match[2];
    if fPathDepth > 0 then
    begin
      if FileName = '' then begin
        Result := True;  // No completion
        Exit;
      end;

      // relative import
      Dir := fFileName;
      for i := 1 to fPathDepth do
        Dir := ExtractFileDir(Dir);
      PythonPathAdder := InternalInterpreter.AddPathToPythonPath(Dir);
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
      if fFileName <> '' then
        PythonPathAdder := InternalInterpreter.AddPathToPythonPath
          (ExtractFileDir(fFileName));
      if CharPos(DottedModName, '.') > 0 then begin
        fModulePrefix := DottedModName;
        Delete(fModulePrefix, LastDelimiter('.', fModulePrefix), MaxInt);
        FNameVar := InternalInterpreter.PyInteractiveInterpreter.findModuleOrPackage(fModulePrefix, None);
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
  Dir : string;
  i : integer;
  ParsedModule : TParsedModule;
begin
  Result :=  TRegExpressions.RE_FromImport.Exec(Copy(Line, 1, Caret.Char - 1));
  if Result then
  begin
    // autocomplete from statement
    fPathDepth := TRegExpressions.RE_FromImport.MatchLen[1];
    if TRegExpressions.RE_FromImport.MatchLen[2] > 0 then
    begin
      // from ...module import identifiers
      fFileName := FileName;
      if PyScripterRefactor.InitializeQuery then
      begin
        ParsedModule := PyScripterRefactor.ResolveModuleImport
          (TRegExpressions.RE_FromImport.Match[2], fFileName, fPathDepth);
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
    fFileName : string;
    fSortedNameSpace : TStringList;
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
  fSortedNameSpace := TStringList.Create;
  fSortedNameSpace.CaseSensitive := True;
  fSortedNameSpace.Duplicates := dupIgnore; // Remove duplicates
end;

destructor TNamespaceCompletionHandler.Destroy;
begin
  fSortedNameSpace.Free;
  inherited;
end;

procedure TNamespaceCompletionHandler.Finalize;
begin
  inherited;
  fFileName := '';
  fSortedNameSpace.Clear;
  PyScripterRefactor.FinalizeQuery;
end;

function TNamespaceCompletionHandler.GetInfo(CCItem: string): string;
Var
  Index : integer;
  CE : TBaseCodeElement;
begin
  Index := fSortedNameSpace.IndexOf(CCItem);
  if Index >=0  then
  begin
    CE := fSortedNameSpace.Objects[Index] as TBaseCodeElement;
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
  lookup, ErrMsg: string;
  ParsedModule, ParsedBuiltInModule: TParsedModule;
  Scope: TCodeElement;
  Def: TBaseCodeElement;
  NameSpace, KeywordList: TStringList;
  Keywords: Variant;
  PythonPathAdder : IInterface;
begin
  fFileName := FileName;
  InsertText := '';
  DisplayText := '';

  TmpX := Caret.Char;
  if TmpX > Length(Line) then
    TmpX := Length(Line)
  else
    Dec(TmpX);

  lookup := GetWordAtPos(Line, TmpX, IdentChars + ['.'], True, False);
  Index := CharLastPos(lookup, WideChar('.'));

  // Add the file path to the Python path - Will be automatically removed
  PythonPathAdder := InternalInterpreter.AddPathToPythonPath
    (ExtractFileDir(fFileName));

  if PyScripterRefactor.InitializeQuery then
  begin
    // GetParsedModule
    ParsedModule := PyScripterRefactor.GetParsedModule(fFileName, None);
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
          if Assigned(Def) then (Def as TCodeElement).GetNamespace(NameSpace);
        end else begin
          // extract namespace from current scope and its parents
          while Assigned(Scope) do
          begin
            Scope.GetNamespace(NameSpace);
            Scope := Scope.Parent as TCodeElement;
          end;
          // builtins (could add keywords as well)
          ParsedBuiltInModule := PyScripterRefactor.GetParsedModule(GetPythonEngine.BuiltInModuleName, None);
          ParsedBuiltInModule.GetNamespace(NameSpace);
          if PyIDEOptions.CompleteKeywords then
          begin
            Keywords := Import('keyword').kwlist;
            Keywords := BuiltinModule.tuple(Keywords);
            KeywordList := TStringList.Create;
            try
              GetPythonEngine.PyTupleToStrings(ExtractPythonObjectFrom(Keywords), KeywordList);
              NameSpace.AddStrings(KeywordList);
            finally
              KeywordList.Free;
            end;
          end
        end;
        ProcessNamespace(fSortedNameSpace, NameSpace, InsertText, DisplayText);
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
  Result :=  TRegExpressions.RE_String.Exec(Copy(Line, 1, Caret.Char - 1));
  if Result then begin
    if PyScripterRefactor.InitializeQuery then
    begin
      // GetParsedModule
      ParsedBuiltInModule := PyScripterRefactor.GetParsedModule(GetPythonEngine.BuiltInModuleName, None);
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
            ProcessNamespace(fSortedNameSpace, NameSpace, InsertText, DisplayText);
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
begin
  Result :=  TRegExpressions.RE_FromImport.Exec(Copy(Line, 1, Caret.Char - 1));
  if Result then
  begin
    // autocomplete from statement
    fPathDepth := TRegExpressions.RE_FromImport.MatchLen[1];
    if (fPathDepth > 0) and (FileName = '') then begin
      Result := True; // No Completion
      Exit;
    end;
    if TRegExpressions.RE_FromImport.MatchLen[2] > 0 then
    begin
      // from ...module import identifiers
      fFileName := FileName;
      if PyScripterRefactor.InitializeQuery then
      begin
        ParsedModule := PyScripterRefactor.ResolveModuleImport
          (TRegExpressions.RE_FromImport.Match[2], fFileName, fPathDepth);
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
              ProcessNamespace(fSortedNameSpace, NameSpace, InsertText, DisplayText);
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
    fNameSpaceDict : TBaseNameSpaceItem;
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
  FreeAndNil(fNameSpaceDict);
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
      Result := GetLineRange(NameSpaceItem.DocString, 1, 20);
  end;
end;

function TLiveNamespaceCompletionHandler.HandleCodeCompletion(const Line,
  FileName: string; Caret: TBufferCoord; Highlighter: TSynCustomHighlighter;
  HighlighterAttr: TSynHighlighterAttributes; out InsertText,
  DisplayText: string): Boolean;
Var
  i, TmpX, Index, ImageIndex : integer;
  lookup : string;
  NameSpaceItem : TBaseNameSpaceItem;
  KeywordList : TStringList;
  Keywords : Variant;
begin
  // Clean-up of FNameSpace and FNameSpaceDict takes place in the Close event
  TmpX := Caret.Char;
  if TmpX > length(Line) then
    TmpX := length(Line)
  else dec(TmpX);

  lookup := GetWordAtPos(Line, TmpX, IdentChars+['.'], True, False, True);
  Index := CharLastPos(lookup, '.');
  fNameSpaceDict := nil;
  if Index > 0 then begin
    lookup := Copy(lookup, 1, Index-1);
    if CharPos(lookup, ')') > 0 then
      lookup := ''  // Issue 422  Do not evaluate functions
    else if IsDigits(lookup) then
      lookup := ''  // Issue 478  User is typing a number
  end else
    lookup := '';  // Completion from global namespace
  if (Index <= 0) or (lookup <> '') then begin
    if PyControl.DebuggerState = dsInactive then
      fNameSpaceDict := PyControl.ActiveInterpreter.NameSpaceFromExpression(lookup)
    else
      fNameSpaceDict := PyControl.ActiveDebugger.NameSpaceFromExpression(lookup);
  end;

  DisplayText := '';
  InsertText := '';

  fNameSpace := TStringList.Create;
  if Assigned(fNameSpaceDict) then begin
    for i := 0 to fNameSpaceDict.ChildCount - 1 do begin
      NameSpaceItem := fNameSpaceDict.ChildNode[i];
      fNameSpace.AddObject(NameSpaceItem.Name, NameSpaceItem);
    end;
  end;
  if (Index <= 0) and PyIDEOptions.CompleteKeywords then begin
    Keywords := Import('keyword').kwlist;
    Keywords := BuiltinModule.tuple(Keywords);
    KeywordList := TStringList.Create;
    try
      GetPythonEngine.PyTupleToStrings(ExtractPythonObjectFrom(Keywords), KeywordList);
      fNameSpace.AddStrings(KeywordList);
    finally
      KeywordList.Free;
    end;
  end;

  fNameSpace.CustomSort(ComparePythonIdents);

  for i := 0 to fNameSpace.Count - 1 do begin
    InsertText := InsertText + fNameSpace[i];

    NameSpaceItem := fNameSpace.Objects[i] as TBaseNameSpaceItem;
    if not Assigned(NameSpaceItem) then
       DisplayText := DisplayText + Format('\Image{%d}\hspace{2}\color{clHotlight}%s',
         [Integer(TCodeImages.Keyword), fNameSpace[i]])
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
      DisplayText := DisplayText + Format('\Image{%d}\hspace{2}%s', [ImageIndex, NameSpaceItem.Name]);
    end;
    if i < fNameSpace.Count - 1 then begin
      DisplayText := DisplayText + #10;
      InsertText := InsertText + #10;
    end;
  end;
  Result := InsertText <> '';
end;

{ TCodeCompletion }

constructor TCodeCompletion.Create;
begin
  SkipHandlers := TObjectList.Create(True);
  CompletionHandlers := TObjectList.Create(True);
end;

destructor TCodeCompletion.Destroy;
begin
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

{ TIDECompletion }

class constructor TIDECompletion.Create;
begin
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
end;


{ TBaseCodeCompletionHandler }

procedure TBaseCodeCompletionHandler.Finalize;
begin
end;

procedure TBaseCodeCompletionHandler.Initialize;
begin
end;

end.
