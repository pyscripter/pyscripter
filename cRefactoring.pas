{-----------------------------------------------------------------------------
 Unit Name: cRefactoring
 Author:    Kiriakos Vlahos
 Date:      03-Jul-2005
 Purpose:   Refactoring support
 History:
-----------------------------------------------------------------------------}

unit cRefactoring;

interface

uses
  SysUtils, Classes, Windows, Variants, cPythonSourceScanner, Contnrs;

type
  {
     Wrapper class for Bicycle RepainMan
  }
  TBRMRefactor = class
  private
    fBRMContext : Variant;
    fBRMCache : Variant;
    fInitialized : boolean;
    fRefactoringIsAvailable : boolean;
    procedure LoadOpenFilesToBRMCache;
    procedure SetupBRM;
    function GeTBRMRefactoringIsAvailable: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    { an ide friendly method which renames a class/fn/method
      pointed to by the coords and filename }
    function RenameByCoordinates(Filename : string; Line, Col: integer;
       NewName : string) : boolean;

    { extracts the region into the named method/function based on context }
    function Extract(Filename : string; BeginLine, BeginCol, EndLine,
      EndCol : integer; Name : string) : boolean;

    { Inlines the variable pointed to by line:col.
      (N.B. line:col can also point to a reference to the
        variable as well as the definition) }
    function InlineLocalVariable(Filename : string; Line, Col: integer) : boolean;

    { Extracts the region into a variable }
    function ExtractLocalVariable(Filename : string; BeginLine, BeginCol, EndLine,
      EndCol : integer; VvariableName: string) : boolean;

   { undoes the last refactoring. WARNING: this is dangerous if
     the user has modified files since the last refactoring.
     Raises UndoStackEmptyException }
    function Undo : boolean;

    { given the coords of a function, class, method or variable
        returns a generator which finds references to it. }
    function FindReferencesByCoordinates(Filename : string; Line, Col: integer;
      ShowMessages : boolean = True; Silent : boolean = False) : Variant;

    { given the coordates to a reference, tries to find the
        definition of that reference - Returns an iterator over mathces}
    function FindDefinitionByCoordinates(Filename : string; Line, Col: integer;
      ShowMessages : boolean = True; Silent : boolean = False) : Variant;

    { moves the class pointed to by (filename_path, line) to a new module }
    function MoveClassToNewModule(Filename : string; Line, Col: integer;
      NewFilename : string) : boolean;

    procedure ProcessBRMMatches(Matches: Variant; ShowMessages,
      Silent: Boolean; var FirstMatch : Variant);
    property Initialized : boolean read fInitialized;
    property RefactoringIsAvailable : boolean read GeTBRMRefactoringIsAvailable;
  end;

  {
     Our own limited refactoring implementation
  }

  ERefactoringException = class(Exception);

  TModuleProxy = class(TParsedModule)
  private
    fPyModule : Variant;
    fIsExpanded : boolean;
  protected
    function GetAllExportsVar: string; override;
    function GetDocString: string; override;
    function GetCodeHint : string; override;
  public
    constructor CreateFromModule(AModule : Variant);
    procedure Expand;
    procedure GetNameSpace(SList : TStringList); override;
    property PyModule : Variant read fPyModule;
    property IsExpanded : boolean read fIsExpanded;
  end;

  TClassProxy = class(TParsedClass)
  private
    fPyClass : Variant;
    fIsExpanded : boolean;
  protected
    function GetDocString: string; override;
  public
    constructor CreateFromClass(AName : string; AClass : Variant);
    function GetConstructor : TParsedFunction; override;
    procedure Expand;
    procedure GetNameSpace(SList : TStringList); override;
    property PyClass : Variant read fPyClass;
    property IsExpanded : boolean read fIsExpanded;
  end;

  TFunctionProxy = class(TParsedFunction)
  private
    fPyFunction : Variant;
    fIsExpanded : boolean;
  protected
    function GetDocString: string; override;
  public
    constructor CreateFromFunction(AName : string; AFunction : Variant);
    procedure Expand;
    function ArgumentsString : string; override;
    procedure GetNameSpace(SList : TStringList); override;
    property PyFunction : Variant read fPyFunction;
    property IsExpanded : boolean read fIsExpanded;
  end;

  TVariableProxy = class(TCodeElement)
  private
    fPyObject : Variant;
    fIsExpanded : boolean;
  protected
    function GetDocString: string; override;
    function GetCodeHint : string; override;
  public
    constructor CreateFromPyObject(const AName : string; AnObject : Variant);
    procedure Expand;
    procedure GetNameSpace(SList : TStringList); override;
    property PyObject : Variant read fPyObject;
    property IsExpanded : boolean read fIsExpanded;
  end;

  TPyScripterRefactor = class
  private
    fPythonScanner : TPythonScanner;
    fProxyModules : TStringList;
    fParsedModules : TStringList;
    fImportResolverCache : TStringList;
    fGetTypeCache : TStringList;
    fSpecialPackages : TStringList;
    //  Begin For find references
    fFindRefCE : TBaseCodeElement;
    fFindRefFileList : TStringList;
    fFindRefDirectoryList : TStringList;
    fFindRefResults : TStringList;
    procedure FindRefFileHandler(const FileName: string);
    procedure FindRefFileHandlerEx(const Directory: string; const FileInfo: TSearchRec);
    //  End For find references
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearParsedModules;
    procedure ClearProxyModules;
    procedure InitializeQuery;
    function GetSource(const FName : string; var Source : string): Boolean;
    function GetParsedModule(const ModuleName : string; PythonPath : Variant) : TParsedModule;
    { given the coordates to a reference, tries to find the
        definition of that reference - Returns TCodeElement, TVariable or nil}
    function FindDefinitionByCoordinates(const Filename : string; Line, Col: integer;
      var ErrMsg : string; Initialize : Boolean = True) : TBaseCodeElement;
    { given the coords of a function, class, method or variable
        returns a list of references to it. }
    procedure FindReferencesByCoordinates(Filename : string; Line, Col: integer;
      var ErrMsg : string; List : TStringList);
    function FindUnDottedDefinition(const Ident : string; ParsedModule : TParsedModule;
      Scope : TCodeElement; var ErrMsg : string) : TBaseCodeElement;
    function FindDottedIdentInScope(const DottedIdent : string; Scope : TCodeElement;
      var ErrMsg: string) : TBaseCodeElement;
    function FindDottedDefinition(const DottedIdent : string; ParsedModule : TParsedModule;
      Scope : TCodeElement; var ErrMsg : string) : TBaseCodeElement;
    function ResolveModuleImport(ModuleImport : TModuleImport): TParsedModule;
    function ResolveImportedName(const Ident: string; ModuleImport: TModuleImport;
      var ErrMsg: string): TBaseCodeElement;
    function GetType(Variable : TVariable; var ErrMsg : string) : TCodeElement;
    procedure FindReferences(CE : TBaseCodeElement; var ErrMsg : string;
      List : TStringList);
    procedure FindReferencesInModule(CE : TBaseCodeElement; Module : TParsedModule;
      CodeBlock: TCodeBlock; var ErrMsg : string; List : TStringList);
    procedure FindReferencesGlobally(CE : TBaseCodeElement; var ErrMsg : string;
      List : TStringList);
  end;

var
  PyScripterRefactor : TPyScripterRefactor;

Const
  FilePosInfoFormat = '%s (%d:%d)';
  FilePosInfoRegExpr = '(.+) \((\d+):(\d+)\)$';

implementation

uses
  frmPyIDEMain, frmPythonII, PythonEngine, VarPyth, dmCommands,
  uEditAppIntfs, frmMessages, JvDockControlForm, Dialogs, JclStrings,
  uCommonFunctions, SynEditTypes, JclFileUtils, Math, StringResources,
  cPyDebugger;

{ Refactor }

constructor TBRMRefactor.Create;
begin
  inherited;
  fInitialized := False;
  fRefactoringIsAvailable := False
end;

destructor TBRMRefactor.Destroy;
begin
  VarClear(fBRMContext);
  VarClear(fBRMCache);
  inherited;
end;

function TBRMRefactor.Extract(Filename: string; BeginLine, BeginCol, EndLine,
  EndCol: integer; Name: string): boolean;
begin
  Result := False;
end;

function TBRMRefactor.ExtractLocalVariable(Filename: string; BeginLine,
  BeginCol, EndLine, EndCol: integer; VvariableName: string): boolean;
begin
  Result := False;
end;

function TBRMRefactor.FindDefinitionByCoordinates(Filename: string; Line,
  Col: integer; ShowMessages : boolean = True; Silent : boolean = False): Variant;
Var
  SupressOutput : IInterface;
begin
  VarClear(Result);
  if not fRefactoringIsAvailable then Exit;
  LoadOpenFilesToBRMCache;
  try
    if Silent then
      SupressOutput := PythonIIForm.OutputSuppressor;
    // we use the brmctx to avoid resetting the file Cache
    Result := fBRMContext.brmctx.findDefinitionByCoordinates(Filename, Line, Col);
  except
    // CheckError already called by VarPyth
    on E: Exception do begin
      if ShowMessages then begin
        MessagesWindow.ShowPythonTraceback;
        MessagesWindow.AddMessage(E.Message);
        ShowDockForm(MessagesWindow);
      end;
      if not Silent then
        raise;
    end;
  end;
end;

function TBRMRefactor.FindReferencesByCoordinates(Filename: string; Line,
  Col: integer; ShowMessages : boolean = True; Silent : boolean = False): Variant;
Var
  SupressOutput : IInterface;
begin
  VarClear(Result);
  if not fRefactoringIsAvailable then Exit;
  LoadOpenFilesToBRMCache;
  try
    if Silent then
      SupressOutput := PythonIIForm.OutputSuppressor;
    // wih use th brmctx to avoid resetting the file Cache
    Result := fBRMContext.brmctx.findReferencesByCoordinates(Filename, Line, Col);
  except
    // CheckError already called by VarPyth
    on E: Exception do begin
      MessageBeep(MB_ICONASTERISK);
      if ShowMessages then begin
        MessagesWindow.ShowPythonTraceback;
        MessagesWindow.AddMessage(E.Message);
        ShowDockForm(MessagesWindow);
      end;
      if not Silent then
        raise;
    end;
  end;
end;

function TBRMRefactor.GeTBRMRefactoringIsAvailable: boolean;
begin
  if not fInitialized then
    SetupBRM;
  Result := fRefactoringIsAvailable;
end;

function TBRMRefactor.InlineLocalVariable(Filename: string; Line,
  Col: integer): boolean;
begin
  Result := False;
end;

procedure TBRMRefactor.LoadOpenFilesToBRMCache;
Var
  i : integer;
  FName,
  Source : string;
begin
  // inject unsaved code into BRMCache
  fBRMCache.instance.reset();
  for i := 0 to GI_EditorFactory.Count - 1 do
    with GI_EditorFactory.Editor[i] do
      if HasPythonFile then begin
        FName := GetFileNameOrTitle;
        Source := CleanEOLs(SynEdit.Text)+#10;
        InternalInterpreter.PyInteractiveInterpreter.loadFileToBRMCache(FName, Source);
      end;
end;

function TBRMRefactor.MoveClassToNewModule(Filename: string; Line,
  Col: integer; NewFilename: string): boolean;
begin
  Result := False;
end;

procedure TBRMRefactor.ProcessBRMMatches(Matches: Variant; ShowMessages,
  Silent: Boolean; var FirstMatch : Variant);
var
  Match : Variant;
  SupressOutput : IInterface;
begin
  if Silent then
    SupressOutput := PythonIIForm.OutputSuppressor;
  VarClear(FirstMatch);
  if not VarIsPython(Matches) or VarIsNone(Matches) then Exit;

  while True do
    try
      Match := Matches.next();
      if ShowMessages then
        MessagesWindow.AddMessage(Format('  Certainty %s%%', [match.confidence]),
          Match.filename, Match.lineno, Match.colno + 1); //ColNo zero based!!
      if VarIsEmpty(FirstMatch) then
        FirstMatch := Match;
    except
      on EPyStopIteration do break;
      on E: Exception do begin
        MessageBeep(MB_ICONASTERISK);
        if ShowMessages then begin
          MessagesWindow.ShowPythonTraceback;
          MessagesWindow.AddMessage(E.Message);
          ShowDockForm(MessagesWindow);
        end;
        if Silent then
          Exit
        else
          raise;
      end;
    end;
end;

function TBRMRefactor.RenameByCoordinates(Filename: string; Line, Col: integer;
  NewName: string): boolean;
begin
  Result := False;
end;

procedure TBRMRefactor.SetupBRM;
begin
  InternalInterpreter.PyInteractiveInterpreter.setupRefactoring();
  fBRMContext := InternalInterpreter.PyInteractiveInterpreter.BRMContext;
  if VarIsNone(fBRMContext) then begin
    fRefactoringIsAvailable := False;
     MessageDlg('Refactoring services are not available.  Please install Bicycle Repair Man from http://sourceforge.net/projects/bicyclerepair/ .',
       mtError, [mbOK], 0);
  end else begin
    fRefactoringIsAvailable := True;
    fBRMCache := InternalInterpreter.PyInteractiveInterpreter.BRMCache;
  end;
  fInitialized := True;
end;

function TBRMRefactor.Undo: boolean;
begin
  Result := False;
end;

{ TPyScripterRefactor }

constructor TPyScripterRefactor.Create;
begin
  inherited;
  fPythonScanner := TPythonScanner.Create;

  fParsedModules := TStringList.Create;
  fParsedModules.CaseSensitive := True;
  fParsedModules.Sorted := True;
  fParsedModules.Duplicates := dupError;

  fProxyModules := TStringList.Create;
  fProxyModules.CaseSensitive := True;
  fProxyModules.Sorted := True;
  fProxyModules.Duplicates := dupError;

  fImportResolverCache := TStringList.Create;
  fImportResolverCache.CaseSensitive := True;
  fGetTypeCache := TStringList.Create;
  fGetTypeCache.CaseSensitive := True;

  fSpecialPackages := TStringList.Create;
  fSpecialPackages.CaseSensitive := true;
end;

function TPyScripterRefactor.FindDefinitionByCoordinates(const Filename: string; Line,
  Col: integer; var ErrMsg : string; Initialize : Boolean = True): TBaseCodeElement;
var
  DottedIdent, LineS : string;
  ParsedModule : TParsedModule;
  Scope : TCodeElement;
  PythonPathAdder : IInterface;
begin
  Result := nil;
  if Initialize then begin
    InitializeQuery;

    // Add the file path to the Python path - Will be automatically removed
    PythonPathAdder := InternalInterpreter.AddPathToPythonPath(ExtractFilePath(FileName));
  end;

  // GetParsedModule
  ParsedModule := GetParsedModule(FileNameToModuleName(FileName), None);
  if not Assigned(ParsedModule) then begin
    ErrMsg := Format('Could not load and parse module: "%s"', [FileName]);
    Exit;
  end;

  // Extract the identifier
  LineS := GetNthLine(ParsedModule.Source, Line);
  DottedIdent := GetWordAtPos(LineS, Col, IdentChars+['.'], True, False);
  DottedIdent := DottedIdent + GetWordAtPos(LineS, Col + 1, IdentChars, False, True);

  if DottedIdent = '' then begin
    ErrMsg := 'No Identifier at the given line and column';
    Exit;
 end;

  // Find scope for line
  Scope := ParsedModule.GetScopeForLine(Line);
  if not assigned(Scope) then
    ErrMsg := 'Could not find scope for the given line'
  else
    // Find identifier in the module and scope
    Result := FindDottedDefinition(DottedIdent, ParsedModule, Scope, ErrMsg);
end;

destructor TPyScripterRefactor.Destroy;
begin
  fPythonScanner.Free;

  ClearParsedModules;
  fParsedModules.Free;

  ClearProxyModules;
  fProxyModules.Free;

  fImportResolverCache.Free;
  fGetTypeCache.Free;

  fSpecialPackages.Free;
  inherited;
end;

function TPyScripterRefactor.GetParsedModule(const ModuleName: string;
  PythonPath : Variant): TParsedModule;
var
  Index, SpecialPackagesIndex : integer;
  FName : Variant;
  ModuleSource, DottedModuleName : string;
  ParsedModule : TParsedModule;
  Editor : IEditor;
  FoundSource : boolean;
begin
  DottedModuleName := ModuleName;
  fSpecialPackages.CommaText := CommandsDataModule.PyIDEOptions.SpecialPackages;
  SpecialPackagesIndex := fSpecialPackages.IndexOf(ModuleName);
  if SpecialPackagesIndex >= 0 then
    try
      Import(ModuleName);
    except
      SpecialPackagesIndex := -1;
    end;

  FoundSource := False;

  if SpecialPackagesIndex < 0 then begin
    // Check whether it is an unsaved file
    Editor := GI_EditorFactory.GetEditorByNameOrTitle(ModuleName);
    if Assigned(Editor) and (Editor.FileName = '') and Editor.HasPythonFile then
    begin
      ModuleSource := Editor.SynEdit.Text;
      FName := ModuleName;
      FoundSource := True;
    end else begin
      // Find the source file
      FName := InternalInterpreter.PyInteractiveInterpreter.findModuleOrPackage(ModuleName, PythonPath);
      if not VarIsNone(FName) and (ExtractFileExt(FName) = '.py') and
        GetSource(FName, ModuleSource)
      then begin
        FoundSource := True;
      end;
    end;
  end;

  if FoundSource then begin
    DottedModuleName := FileNameToModuleName(FName);
    Index := fParsedModules.IndexOf(DottedModuleName);
    if Index < 0 then begin
      ParsedModule := TParsedModule.Create;
      ParsedModule.Name := DottedModuleName;
      ParsedModule.FileName := FName;
      fPythonScanner.ScanModule(ModuleSource, ParsedModule);
      fParsedModules.AddObject(DottedModuleName, ParsedModule);
      Result := ParsedModule;
    end else
      Result := fParsedModules.Objects[Index] as TParsedModule;
  end else if SysModule.modules.has_key(DottedModuleName) then begin
    // If the source file does not exist look at sys.modules to see whether it
    // is available in the interpreter.  If yes then create a proxy module
    Index := fProxyModules.IndexOf(DottedModuleName);
    if Index < 0 then begin
      Index := fProxyModules.AddObject(DottedModuleName,
        TModuleProxy.CreateFromModule(SysModule.modules.GetItem(DottedModuleName)));
      Result := fProxyModules.Objects[Index] as TParsedModule;
    end else
      Result := fProxyModules.Objects[Index] as TParsedModule;
  end else
    Result := nil;  // no source and not in sys.modules
end;

function TPyScripterRefactor.GetSource(const FName: string;
  var Source: string): Boolean;
var
  Editor : IEditor;
begin
  Result := False;
  Editor := GI_EditorFactory.GetEditorByNameOrTitle(FName);
  if Assigned(Editor) then begin
    Source := Editor.SynEdit.Text;
    Result := True;
  end;
  if not Result then begin
    if not FileExists(FName) then
      Exit;
    try
      Source := FileToString(FName);
      Result := True;
    except
      // We cannot open the file for some reason
      Result := False;
    end;
  end;
end;

procedure TPyScripterRefactor.ClearParsedModules;
var
  i : integer;
begin
  for i := 0 to fParsedModules.Count - 1 do
    fParsedModules.Objects[i].Free;
  fParsedModules.Clear;
end;

procedure TPyScripterRefactor.ClearProxyModules;
var
  i : integer;
begin
  for i := 0 to fProxyModules.Count - 1 do
    fProxyModules.Objects[i].Free;
  fProxyModules.Clear;
end;

function TPyScripterRefactor.FindDottedDefinition(const DottedIdent: string;
  ParsedModule : TParsedModule; Scope: TCodeElement; var ErrMsg: string): TBaseCodeElement;
{
  Look for a dotted identifier in a given CodeElement (scope) of a ParsedModule
  The function first finds the first part of the dotted definition and then
  calls the recursive function FindDottedIdentInScope
}
Var
  Prefix, Suffix : string;
  Def : TBaseCodeElement;
begin
  Result := nil;
  Suffix := DottedIdent;
  Prefix := StrToken(Suffix, '.');
  Def := FindUnDottedDefinition(Prefix, ParsedModule, Scope, ErrMsg);

  if Assigned(Def) then begin
    if Suffix <> '' then begin
      if Def.ClassType = TVariable then
        Def := GetType(TVariable(Def), ErrMsg);
      if Assigned(Def) then
        Result := FindDottedIdentInScope(Suffix, Def as TCodeElement, ErrMsg);
    end else
      Result := Def;
  end else
    ErrMsg := Format('Could not find identifier "%s" in scope "%s"',
          [DottedIdent, Scope.Name]);
end;

function TPyScripterRefactor.FindUnDottedDefinition(const Ident: string;
  ParsedModule : TParsedModule; Scope: TCodeElement; var ErrMsg: string): TBaseCodeElement;
{
  Look for an undotted (root) identifier in a given CodeElement (scope)
  of a ParsedModule
  First it checks the Scope and Parent scopes
  Then it checks the builtin module
  Finally it looks for implicitely imported modules and from * imports
}
Var
  NameSpace : TStringList;
  ParsedBuiltinModule : TParsedModule;
  Index: integer;
  CodeElement : TCodeElement;
begin
  Result := nil;
  if Ident = 'self' then begin
    Result := Scope;
    while not (Result is TParsedClass) and Assigned(TCodeElement(Result).Parent) do
      Result := TCodeElement(Result).Parent;
    if not (Result is TParsedClass) then begin
      Result := nil;
      ErrMsg := '"self" used outside a class scope';
    end;
    Exit;
  end;

  NameSpace := TStringList.Create;
  NameSpace.CaseSensitive := True;
  try
    // First check the Scope and Parent scopes
    CodeElement := Scope;
    while Assigned(CodeElement) do begin
      NameSpace.Clear;
      CodeElement.GetNameSpace(NameSpace);
      Index := NameSpace.IndexOf(Ident);
      if Index >= 0 then begin
        Result := NameSpace.Objects[Index] as TBaseCodeElement;
        break;
      end;
      CodeElement := CodeElement.Parent as TCodeElement;
    end;

    // then check the builtin module
    NameSpace.Clear;
    if not Assigned(Result) then begin
      ParsedBuiltInModule := GetParsedModule('__builtin__', None);
      if not Assigned(ParsedBuiltInModule) then
        raise ERefactoringException.Create(
          'Internal Error in FindUnDottedDefinition: Could not get the Builtin module');
      ParsedBuiltInModule.GetNameSpace(NameSpace);
      Index := NameSpace.IndexOf(Ident);
      if Index >= 0 then
        Result := NameSpace.Objects[Index] as TBaseCodeelement;
      NameSpace.Clear;
    end;
  finally
    NameSpace.Free;
  end;

  if Assigned(Result) and (Result is TVariable)
    and (TVariable(Result).Parent is TModuleImport)
  then
    // Resolve further
    Result := ResolveImportedName(TVariable(Result).RealName,
      TModuleImport(TVariable(Result).Parent), ErrMsg);

  if Assigned(Result) and (Result is TModuleImport) then
    Result := ResolveModuleImport(TModuleImport(Result));


  if not Assigned(Result) then
    ErrMsg := Format('Could not find identifier "%s" in module "%s"',
      [Ident, ParsedModule.Name]);
end;

function TPyScripterRefactor.ResolveModuleImport(ModuleImport: TModuleImport) : TParsedModule;
var
  ParentModule : TParsedModule;
  RealName : string;
begin
  RealName := ModuleImport.RealName;
  Result := GetParsedModule(RealName, None);
  if not Assigned(Result) then begin
    // try a relative import
    ParentModule := ModuleImport.GetModule;
    if ParentModule.IsPackage then
      Result := GetParsedModule(ParentModule.Name + '.' + RealName, None);
    { Should we check whether ParentModule belongs to a package?}
  end;
end;

function TPyScripterRefactor.ResolveImportedName(const Ident: string;
  ModuleImport: TModuleImport; var ErrMsg: string): TBaseCodeElement;
// May be called recursively
// fImportResolverCache is used to prevent infinite recursion
Var
  S : string;
  ImportedModule : TParsedModule;
  NameSpace : TStringList;
  Index : integer;
begin
  Result := nil;
  S := ModuleImport.Name + '.' + Ident;
  if fImportResolverCache.IndexOf(S) >= 0 then
    ErrMsg := 'Cyclic imports encountered!'
  else begin
    ImportedModule := ResolveModuleImport(ModuleImport);
    if not Assigned(ImportedModule) then
      ErrMsg := Format('Could not analyse module: "%s"', [ModuleImport.Name])
    else begin
      fImportResolverCache.Add(S);
      NameSpace := TStringList.Create;
      NameSpace.CaseSensitive := True;
      try
        ImportedModule.GetNameSpace(NameSpace);
        Index := NameSpace.IndexOf(Ident);
        if Index >= 0 then begin
          Result := NameSpace.Objects[Index] as TBaseCodeElement;
          if Assigned(Result) and (Result is TVariable)
            and (TVariable(Result).Parent is TModuleImport)
          then
            // Resolve further
            Result := ResolveImportedName(TVariable(Result).RealName,
              TModuleImport(TVariable(Result).Parent), ErrMsg);

          if Assigned(Result) and (Result is TModuleImport) then
            Result := ResolveModuleImport(TModuleImport(Result));
        end;
        { Check whether Ident is a sub-packages }
        if not Assigned(Result) and ImportedModule.IsPackage then
          Result := GetParsedModule(ImportedModule.Name + '.' + Ident, None);

        if not Assigned(Result) then
          ErrMsg := Format('Could not find identifier "%s" in module "%s"',
          [Ident, ModuleImport.Name]);
      finally
        NameSpace.Free;
        fImportResolverCache.Delete(fImportResolverCache.IndexOf(S));
      end;
    end;
  end;
end;

function TPyScripterRefactor.GetType(Variable: TVariable;
  var ErrMsg: string): TCodeElement;
// Returns the type of a TVariable as a TCodeElement
// One limitation is that it does not differentiate between Classes and their
// instances, which should be OK for our needs (FindDefinition and CodeCompletion)
Var
  BaseCE, TypeCE : TBaseCodeElement;
  AVar : TVariable;
  Module : TParsedModule;
  ParsedBuiltInModule : TParsedModule;
  S : string;
begin
  Result := nil;
  // Resolve imported variables
  if Variable.Parent is TModuleImport then begin
    // ResolveImportedName returns either a CodeElement or Variable whose
    // parent is not a ModuleImport
    BaseCE := ResolveImportedName(Variable.RealName,
      TModuleImport(Variable.Parent), ErrMsg);
    if not Assigned(BaseCE) then
      Exit
    else if BaseCE is TCodeElement then begin
      Result := TCodeElement(BaseCE);
      Exit;
    end else begin
      // cannot be anything but TVariable !
      Assert(BaseCE is TVariable, 'Internal Error in GetType');
      AVar := TVariable(BaseCE);
    end;
  end else
    AVar := Variable;

  Module := AVar.GetModule;
  S := Module.Name + '.' + AVar.Parent.Name + '.' + AVar.Name;
  if fGetTypeCache.IndexOf(S) >= 0 then
    ErrMsg := 'Cyclic imports encountered!'
  else begin
    fGetTypeCache.Add(S);
    try
      // check standard types
      if vaBuiltIn in AVar.Attributes then begin
        ParsedBuiltInModule := GetParsedModule('__builtin__', None);
        (ParsedBuiltInModule as TModuleProxy).Expand;
        Result := ParsedBuiltInModule.GetChildByName(AVar.ObjType)
      end else if (AVar.ObjType <> '') and Assigned(AVar.Parent) and
        (AVar.Parent is TCodeElement) then
      begin
        TypeCE := FindDottedDefinition(AVar.ObjType, Module,
          TCodeElement(AVar.Parent), ErrMsg);
        // Note: currently we are not able to detect the return type of functions
        if (TypeCE is TParsedClass) or (TypeCE is TParsedModule) or
           ((TypeCE is TVariableProxy) and not (vaCall in AVar.Attributes)) or
           ((TypeCE is TParsedFunction) and not (vaCall in AVar.Attributes))
        then
          Result := TCodeElement(TypeCE)
        else if TypeCE is TVariable then
          Result := GetType(TVariable(TypeCE), ErrMsg);
      end;
      if not Assigned(Result) then
        ErrMsg := Format('Type of "%s" is unknown', [AVar.Name]);
    finally
      fGetTypeCache.Delete(fGetTypeCache.IndexOf(S));
    end;
  end;
end;

function TPyScripterRefactor.FindDottedIdentInScope(const DottedIdent: string;
  Scope: TCodeElement; var ErrMsg: string): TBaseCodeElement;
// Recursive routine
// Do not call directly - It is called from FindDottedDefinition
// and assumes that the first part (Suffix) of the DottedIdent is not
// a root code element
Var
  Prefix, Suffix : string;
  NameSpace : TStringList;
  Def : TBaseCodeElement;
  Index : integer;
begin
  Result := nil;
  Suffix := DottedIdent;
  Prefix := StrToken(Suffix, '.');
  Def := nil;
  NameSpace := TStringList.Create;
  NameSpace.CaseSensitive := True;
  try
    Scope.GetNameSpace(NameSpace);
    Index := NameSpace.IndexOf(Prefix);
    if Index >= 0 then begin
      Def := NameSpace.Objects[Index] as TBaseCodeElement;

      if Assigned(Def) and (Def is TVariable) and (Def.Parent is TModuleImport) then
         Def := ResolveImportedName(TVariable(Def).RealName, TModuleImport(Def.Parent), ErrMsg);

      if Assigned(Def) and (Def is TModuleImport) then
        Def := ResolveModuleImport(TModuleImport(Def));
    end else if (Scope is TParsedModule) and TParsedModule(Scope).IsPackage then
      // check for submodules of packages
      Def := GetParsedModule(TParsedModule(Scope).Name + '.' + Prefix, None);
  finally
    NameSpace.Free;
  end;

  if Assigned(Def) then begin
    if Suffix <> '' then begin
      if Def.ClassType = TVariable then
        Def := GetType(TVariable(Def), ErrMsg);
      if Assigned(Def) then
        Result := FindDottedIdentInScope(Suffix, Def as TCodeElement, ErrMsg);
    end else
      Result := Def;
  end else
    ErrMsg := Format('Could not find identifier "%s" in scope "%s"',
          [DottedIdent, Scope.Name]);
end;

procedure TPyScripterRefactor.InitializeQuery;
begin
  ClearParsedModules;  // in case source has changed
  fImportResolverCache.Clear;  // fresh start
  fGetTypeCache.Clear;  // fresh start
end;

procedure TPyScripterRefactor.FindReferencesByCoordinates(Filename: string;
  Line, Col: integer; var ErrMsg: string; List: TStringList);
Var
  DottedIdent, LineS : string;
  ParsedModule : TParsedModule;
  Scope : TCodeElement;
  PythonPathAdder : IInterface;
  Def : TBaseCodeElement;
begin
  InitializeQuery;

  // Add the file path to the Python path - Will be automatically removed
  PythonPathAdder := InternalInterpreter.AddPathToPythonPath(ExtractFilePath(FileName));

  // GetParsedModule
  ParsedModule := GetParsedModule(FileNameToModuleName(FileName), None);
  if not Assigned(ParsedModule) then begin
    ErrMsg := Format('Could not load and parse module: "%s"', [FileName]);
    Exit;
  end;

  // Extract the identifier
  LineS := GetNthLine(ParsedModule.Source, Line);
  DottedIdent := GetWordAtPos(LineS, Col, IdentChars+['.'], True, False);
  DottedIdent := DottedIdent + GetWordAtPos(LineS, Col + 1, IdentChars, False, True);

  if DottedIdent = '' then begin
    ErrMsg := 'No Identifier at the given line and column';
    Exit;
 end;

  // Find scope for line
  Scope := ParsedModule.GetScopeForLine(Line);
  Def := nil;
  if not assigned(Scope) then
    ErrMsg := 'Could not find scope for the given line'
  else
    // Find identifier in the module and scope
    Def := FindDottedDefinition(DottedIdent, ParsedModule, Scope, ErrMsg);

  if Assigned(Def) and (Def is TVariable)
    and (TVariable(Def).Parent is TModuleImport)
  then
    // Resolve further
    Def := ResolveImportedName(TVariable(Def).RealName,
      TModuleImport(TVariable(Def).Parent), ErrMsg);

  if Assigned(Def) and (Def is TModuleImport) then
    Def := ResolveModuleImport(TModuleImport(Def));

  if Assigned(Def) then
    FindReferences(Def, ErrMsg, List);
end;

procedure TPyScripterRefactor.FindReferences(CE: TBaseCodeElement;
  var ErrMsg: string; List: TStringList);
Var
  Module : TParsedModule;
  SearchScope : TCodeElement;
begin
  Assert(Assigned(CE));
  Assert(Assigned(List));
  Module := CE.GetModule;
  Assert(Assigned(Module));

  if (CE is TParsedModule) or (CE.Parent is TParsedModule) then
    SearchScope := nil  // global scope
  else if CE.Parent is TParsedFunction then
    //  Local variable or argument or classes/ functions nested in functions
    SearchScope := TParsedFunction(CE.Parent)
  else if (CE.Parent is TParsedClass) and (CE.Parent.Parent is TParsedModule) then
    // methods and properties
    SearchScope := nil  // global scope
  else if CE is TParsedClass then
    // Nested functions and classes
    SearchScope := CE.Parent as TCodeElement
  else
    // e.g. methods of nested classes
    SearchScope := CE.Parent.Parent as TCodeElement;

  if Assigned(SearchScope) then
     FindReferencesInModule(CE, Module, SearchScope.CodeBlock, ErrMsg, List)
  else
    FindReferencesGlobally(CE, ErrMsg, List);
end;

procedure TPyScripterRefactor.FindRefFileHandlerEx(const Directory: string;
  const FileInfo: TSearchRec);
Var
  ParsedModule : TParsedModule;
  FileName, ErrMsg, ModuleSource, CEName : string;
begin
  FileName := Directory + FileInfo.Name;
  // the following if for seaching for sub-modules and sub-packages
  CEName := Copy(fFindRefCE.Name, CharLastPos(fFindRefCE.Name, '.') + 1, MaxInt);
  { TODO 2 : Currently we are reading the source code twice for modules that get searched.
    Once to scan them and then when they get parsed. This can be optimised out. }
  if GetSource(FileName, ModuleSource) and
    (Pos(CEName, ModuleSource) > 0) then
  begin
    ParsedModule := GetParsedModule(FileNameToModuleName(FileName), None);
    if Assigned(ParsedModule) then
      FindReferencesInModule(fFindRefCE, ParsedModule, ParsedModule.CodeBlock,
        ErrMsg, fFindRefResults);
  end;
end;

procedure TPyScripterRefactor.FindRefFileHandler(const FileName: string);
begin
  fFindRefDirectoryList.Add(FileName);
end;

procedure TPyScripterRefactor.FindReferencesGlobally(CE: TBaseCodeElement;
  var ErrMsg: string; List: TStringList);
Var
  Module : TParsedModule;
  FileName, Dir, PackageRootDir : string;
  i : integer;
begin
  Module := CE.GetModule;
  Assert(Assigned(Module));
  fFindRefFileList := TStringList.Create;
  fFindRefDirectoryList := TStringList.Create;
  fFindRefResults := List;
  fFindRefCE := CE;
  try
    FileName := Module.FileName;
    Dir := ExtractFileDir(FileName);
    if IsDirPythonPackage(Dir) then begin
      PackageRootDir := GetPackageRootDir(Dir);
      EnumDirectories(PathAddSeparator(PackageRootDir), FindRefFileHandler);
    end else
      fFindRefDirectoryList.Add(PathAddSeparator(Dir));
    for i := 0 to fFindRefDirectoryList.Count - 1 do
      EnumFiles(fFindRefDirectoryList[i]+ '*.py', FindRefFileHandlerEx);
  finally
    FreeAndNil(fFindRefFileList);
    FreeAndNil(fFindRefDirectoryList);
  end;
end;

procedure TPyScripterRefactor.FindReferencesInModule(CE: TBaseCodeElement;
  Module: TParsedModule; CodeBlock: TCodeBlock; var ErrMsg: string;
  List: TStringList);
Var
  SL : TStringList;
  Line, CEName, CEModuleName : string;
  i, j : integer;
  LinePos: Integer;
  Found : Boolean;
  EndPos : integer;
  Def : TBaseCodeElement;
  Start: Integer;
  TestChar: Char;
  ModuleIsImported : boolean;
begin
  // the following if for seaching for sub-modules and sub-packages
  CEName := Copy(CE.Name, CharLastPos(CE.Name, '.') + 1, MaxInt);
  ModuleIsImported := False;
  if not SameFileName(CE.GetModule.FileName, Module.FileName) then begin
    // Check (approximately!) whether CE.GetModule gets imported in Module
    CEModuleName := CE.GetModule.Name;
    if CharPos(CE.GetModule.Name, '.') > 0 then
      CEModuleName := Copy(CEModuleName, CharLastPos(CEModuleName, '.') + 1, MaxInt);
    for i := 0 to Module.ImportedModules.Count - 1 do begin
      if Pos(CEModuleName, TModuleImport(Module.ImportedModules[i]).Name) >0 then begin
        ModuleIsImported := True;
        break;
      end;

      // the following is for dealing with the syntax
      //      from package import submodule
      // if CE.Module is a submodule and subpackage
      if CharPos(CE.GetModule.Name, '.') > 0 then begin
        if Assigned(TModuleImport(Module.ImportedModules[i]).ImportedNames) then
          for j := 0 to TModuleImport(Module.ImportedModules[i]).ImportedNames.Count - 1 do
            if Pos(CEModuleName, TVariable(
              TModuleImport(Module.ImportedModules[i]).ImportedNames[j]).Name) >0 then
            begin
              ModuleIsImported := True;
              break;
            end;
      end;
    end;
  end else
    ModuleIsImported := True;

  if not ModuleIsImported then Exit; // no need to process further

  SL := TStringList.Create;
  SL.Text := Module.MaskedSource;
  try
    for i := CodeBlock.StartLine-1 to Min(SL.Count, CodeBlock.EndLine) - 1 do begin
      Line := SL[i];
      EndPos := 0;
      Repeat
        LinePos := StrSearch(CEName, Line, EndPos + 1);
        Found := LinePos > 0;
        if Found then begin
          // check if it is a whole word
          EndPos := LinePos + Length(CEName) - 1;

          Start := LinePos - 1; // Point to previous character 
          if (Start > 0) then
          begin
            TestChar := Line[Start];
            if IsCharAlphaNumeric(TestChar) or (TestChar = '_') then
              Continue;
          end;
          if EndPos < Length(Line) then begin
            TestChar := Line[EndPos+1];  // Next Character
            if IsCharAlphaNumeric(TestChar) or (TestChar = '_') then
              Continue;
          end;
          // Got Match - now process it  -------------------
          Def := FindDefinitionByCoordinates(Module.FileName, i+1, LinePos, ErrMsg, False);
          if Assigned(Def) and (Def is TVariable)
            and (TVariable(Def).Parent is TModuleImport)
          then
            // Resolve further
            Def := ResolveImportedName(TVariable(Def).RealName,
              TModuleImport(TVariable(Def).Parent), ErrMsg);

          if Assigned(Def) and (Def is TModuleImport) then
            Def := ResolveModuleImport(TModuleImport(Def));

          if Def = CE then
            List.Add(Format(FilePosInfoFormat, [Module.FileName, i+1, LinePos]));
          // End of processing  -------------------
        end;
      Until not Found;
    end;
  finally
    SL.Free;
  end;
end;

{ TModuleProxy }

procedure TModuleProxy.Expand;
Var
  InspectModule, ItemsDict, ItemKeys, ItemValue : Variant;
  i : integer;
  S : string;
  VariableProxy : TVariableProxy;
begin
  InspectModule := Import('inspect');
  ItemsDict := fPyModule.__dict__;
  ItemKeys := ItemsDict.keys();
  ItemKeys.sort();
  for i := 0 to len(ItemKeys) - 1 do begin
    S := ItemKeys.GetItem(i);
    ItemValue := ItemsDict.GetItem(S);
    if InspectModule.isroutine(ItemValue) then
      AddChild(TFunctionProxy.CreateFromFunction(S, ItemValue))
    else if InspectModule.isclass(ItemValue) then
      AddChild(TClassProxy.CreateFromClass(S, ItemValue))
//   the following would risk infinite recursion and fails in e.g. os.path
//   path is a variable pointing to the module ntpath
//    else if InspectModule.ismodule(ItemValue) then
//      AddChild(TModuleProxy.CreateFromModule(ItemValue))
    else begin
      VariableProxy := TVariableProxy.CreateFromPyObject(S, ItemValue);
      VariableProxy.Parent := self;
      Globals.Add(VariableProxy);
    end;
  end;
  fIsExpanded := True;
end;

constructor TModuleProxy.CreateFromModule(AModule: Variant);
begin
  inherited Create;
  if not VarIsPythonModule(AModule) then
    Raise Exception.Create('TModuleProxy creation error');
  Name := AModule.__name__;
  fPyModule := AModule;
  fIsExpanded := false;
  fIsProxy := True;
  if BuiltInModule.hasattr(fPyModule, '__file__') then
    FileName := fPyModule.__file__;
end;

procedure TModuleProxy.GetNameSpace(SList: TStringList);
begin
  if not fIsExpanded then Expand;
  inherited;
end;

function TModuleProxy.GetAllExportsVar: string;
begin
   Result := '';
//   No need since we are exporting what is needed
//   if BuiltInModule.hasattr(fPyModule, '__all__') then begin
//     try
//       PythonIIForm.ShowOutput := False;
//       Result := BuiltInModule.str(fPyModule.__all__);
//       Result := Copy(Result, 2, Length(Result) - 2);
//     except
//       Result := '';
//     end;
//     PythonIIForm.ShowOutput := True;
//   end;
end;

function TModuleProxy.GetDocString: string;
Var
  PyDocString : Variant;
begin
  PyDocString := Import('inspect').getdoc(fPyModule);
  if not VarIsNone(PyDocString) then
    Result := PyDocString
  else
    Result := '';
end;

function TModuleProxy.GetCodeHint: string;
begin
  if IsPackage then
    Result := Format(SPackageProxyCodeHint, [Name])
  else
    Result := Format(SModuleProxyCodeHint, [Name]);
end;

{ TClassProxy }

procedure TClassProxy.Expand;
Var
  InspectModule, ItemsDict, ItemKeys, ItemValue : Variant;
  i : integer;
  S : string;
  VariableProxy : TVariableProxy;
begin
  InspectModule := Import('inspect');
  ItemsDict := BuiltInModule.dict(InspectModule.getmembers(fPyClass));
  ItemKeys := ItemsDict.keys();
  ItemKeys.sort();
  for i := 0 to len(ItemKeys) - 1 do begin
    S := ItemKeys.GetItem(i);
    ItemValue := ItemsDict.GetItem(S);
    if InspectModule.isroutine(ItemValue) then
      AddChild(TFunctionProxy.CreateFromFunction(S, ItemValue))
    else if InspectModule.isclass(ItemValue) then
      AddChild(TClassProxy.CreateFromClass(S, ItemValue))
    else begin
      VariableProxy := TVariableProxy.CreateFromPyObject(S, ItemValue);
      VariableProxy.Parent := self;
      Attributes.Add(VariableProxy);
    end;
  end;
  // setup base classes
  for i := 0 to len(fPyClass.__bases__) - 1 do
    SuperClasses.Add(fPyClass.__bases__[i].__name__);

  fIsExpanded := True;
end;

constructor TClassProxy.CreateFromClass(AName : string; AClass: Variant);
begin
  inherited Create;
  if not VarIsPythonClass(AClass) then
    Raise Exception.Create('TClassProxy creation error');
  Name := AName;
  fPyClass := AClass;
  fIsExpanded := false;
  fIsProxy := True;
end;

procedure TClassProxy.GetNameSpace(SList: TStringList);
Var
  i : integer;
begin
  if not fIsExpanded then Expand;
  //  There is no need to examine base classes so we do not call inherited
  //  Add from Children
  for i := 0 to ChildCount - 1 do
    SList.AddObject(TCodeElement(Children[i]).Name, Children[i]);
  for i := 0 to Attributes.Count - 1 do
    SList.AddObject(TVariable(Attributes[i]).Name, Attributes[i])
end;

function TClassProxy.GetDocString: string;
Var
  PyDocString : Variant;
begin
  PyDocString := Import('inspect').getdoc(fPyClass);
  if not VarIsNone(PyDocString) then
    Result := PyDocString
  else
    Result := '';
end;

function TClassProxy.GetConstructor: TParsedFunction;
begin
  if not fIsExpanded then Expand;
  Result := inherited GetConstructor;
end;

{ TFunctionProxy }

function TFunctionProxy.ArgumentsString: string;
begin
  Result := InternalInterpreter.PyInteractiveInterpreter.get_arg_text(fPyFunction);
end;

constructor TFunctionProxy.CreateFromFunction(AName : string; AFunction: Variant);
var
  InspectModule : Variant;
begin
  inherited Create;
  InspectModule := Import('inspect');
  if InspectModule.isroutine(AFunction) then begin
//    Name := AFunction.__name__;
    Name := AName;
    fPyFunction := AFunction;
    fIsExpanded := false;
    fIsProxy := True;
  end else
    Raise Exception.Create('TFunctionProxy creation error');
end;

procedure TFunctionProxy.Expand;
Var
  InspectModule, ItemsDict, ItemKeys, ItemValue : Variant;
  i : integer;
  S : string;
  NoOfArgs : integer;
  Variable : TVariable;
  VariableProxy : TVariableProxy;
begin
  //  insert members of Function type
  InspectModule := Import('inspect');
  ItemsDict := BuiltInModule.dict(InspectModule.getmembers(fPyFunction));
  ItemKeys := ItemsDict.keys();
  ItemKeys.sort();
  for i := 0 to len(ItemKeys) - 1 do begin
    S := ItemKeys.GetItem(i);
    ItemValue := ItemsDict.GetItem(S);
    if InspectModule.isroutine(ItemValue) then
      AddChild(TFunctionProxy.CreateFromFunction(S, ItemValue))
    else if InspectModule.isclass(ItemValue) then
      AddChild(TClassProxy.CreateFromClass(S, ItemValue))
    else begin
      VariableProxy := TVariableProxy.CreateFromPyObject(S, ItemValue);
      VariableProxy.Parent := self;
      Locals.Add(VariableProxy);
    end;
  end;
  fIsExpanded := True;

  // Arguments and Locals
  if BuiltinModule.hasattr(fPyFunction, 'func_code') then begin
    NoOfArgs := fPyFunction.func_code.co_argcount;
    for i := 0 to len(fPyFunction.func_code.co_varnames) - 1 do begin
      Variable := TVariable.Create;
      Variable.Name := fPyFunction.func_code.co_varnames[i];
      Variable.Parent := Self;
      if i < NoOfArgs then begin
        Variable.Attributes := [vaArgument];
        Arguments.Add(Variable);
      end else
        Locals.Add(Variable);
    end;
  end;
end;

function TFunctionProxy.GetDocString: string;
Var
  PyDocString : Variant;
begin
  PyDocString := Import('inspect').getdoc(fPyFunction);
  if not VarIsNone(PyDocString) then
    Result := PyDocString
  else
    Result := '';
end;

procedure TFunctionProxy.GetNameSpace(SList: TStringList);
begin
  if not fIsExpanded then Expand;
  inherited;
end;

{ TVariableProxy }

constructor TVariableProxy.CreateFromPyObject(const AName: string; AnObject: Variant);
begin
  inherited Create;
  Name := AName;
  fPyObject := AnObject;
  fIsExpanded := false;
  fIsProxy := True;
end;

procedure TVariableProxy.GetNameSpace(SList: TStringList);
begin
  if not fIsExpanded then Expand;
  inherited;
end;

procedure TVariableProxy.Expand;
Var
  InspectModule, ItemsDict, ItemKeys, ItemValue : Variant;
  i : integer;
  S : string;
begin
  InspectModule := Import('inspect');
  ItemsDict := BuiltinModule.dict(InspectModule.getmembers(fPyObject));
  ItemKeys := ItemsDict.keys();
  ItemKeys.sort();
  for i := 0 to len(ItemKeys) - 1 do begin
    S := ItemKeys.GetItem(i);
    ItemValue := ItemsDict.GetItem(S);
    if InspectModule.isroutine(ItemValue) then
      AddChild(TFunctionProxy.CreateFromFunction(S, ItemValue))
    else if InspectModule.isclass(ItemValue) then
      AddChild(TClassProxy.CreateFromClass(S, ItemValue))
    else if InspectModule.ismodule(ItemValue) then
      AddChild(TModuleProxy.CreateFromModule(ItemValue))
    else begin
      AddChild(TVariableProxy.CreateFromPyObject(S, ItemValue))
    end;
  end;
  fIsExpanded := True;
end;

function TVariableProxy.GetDocString: string;
Var
  PyDocString : Variant;
begin
  PyDocString := Import('inspect').getdoc(fPyObject);
  if not VarIsNone(PyDocString) then
    Result := PyDocString
  else
    Result := '';
end;

function TVariableProxy.GetCodeHint: string;
Var
  Fmt, ObjType : string;
begin
  if Parent is TParsedFunction then
    Fmt := SLocalVariableCodeHint
  else if Parent is TParsedClass then
    Fmt := SInstanceVariableCodeHint
  else if Parent is TParsedModule then
    Fmt := SGlobalVariableCodeHint
  else
    Fmt := '';
  if Fmt <> '' then begin
    Result := Format(Fmt,
      [Name, Parent.Name, '']);

    ObjType := BuiltInModule.type(PyObject).__name__;
    Result := Result + Format(SVariableTypeCodeHint, [ObjType]);
  end else
    Result := '';
end;

initialization
  PyScripterRefactor := TPyScripterRefactor.Create;
finalization
  PyScripterRefactor.Free;
end.

