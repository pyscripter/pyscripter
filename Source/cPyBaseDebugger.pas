{-----------------------------------------------------------------------------
 Unit Name: cPyBaseDebugger
 Author:    Kiriakos Vlahos
 Date:      23-Apr-2006
 Purpose:   Base debugger classes
 History:
-----------------------------------------------------------------------------}
unit cPyBaseDebugger;

interface
uses
  WinApi.Windows,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Vcl.Forms,
  uEditAppIntfs,
  cPySupportTypes,
  cPythonSourceScanner,
  PythonEngine;

type
  TDebuggerCommand = (dcNone, dcRun, dcStepInto, dcStepOver, dcStepOut,
                      dcRunToCursor, dcPause, dcAbort);

  TInterpreterCapability = (icReInitialize);
  TInterpreterCapabilities = set of TInterpreterCapability;

  TNamespaceItemAttribute = (nsaNew, nsaChanged);
  TNamespaceItemAttributes = set of TNamespaceItemAttribute;

  TThreadStatus = (thrdRunning, thrdBroken, thrdFinished);

  // Base (abstract) class for Call Stack frame information
  TBaseFrameInfo = class(TObject)
  protected
    function GetFunctionName : string; virtual; abstract;
    function GetFileName : string; virtual; abstract;
    function GetLine : integer; virtual; abstract;
  public
    property FunctionName : string read GetFunctionName;
    property FileName : string read GetFileName;
    property Line : integer read GetLine;
  end;

  // Base (abstract) class for thread information
  TThreadInfo = class(TObject)
    Thread_ID : Int64;
    Name : string;
    Status : TThreadStatus;
    CallStack : TObjectList<TBaseFrameInfo>;
    constructor Create;
    destructor Destroy; override;
  end;

  // Base (abstract) class for Namespace item information
  TBaseNameSpaceItem = class(TObject)
  protected
    fPyObject : Variant;
    fExpandCommonTypes : Boolean;
    fExpandSequences : Boolean;
    GotChildNodes : Boolean;
    GotBufferedValue : Boolean;
    BufferedValue : string;
    function GetOrCalculateValue : string;
    function GetName : string; virtual; abstract;
    function GetObjectType : string; virtual; abstract;
    function GetValue : string; virtual; abstract;
    function GetDocString : string; virtual; abstract;
    function GetChildCount : integer; virtual; abstract;
    function GetChildNode(Index: integer): TBaseNameSpaceItem; virtual; abstract;
  public
    Attributes : TNamespaceItemAttributes;
    function IsClass : Boolean; virtual; abstract;
    function IsDict : Boolean; virtual; abstract;
    function IsModule : Boolean; virtual; abstract;
    function IsFunction : Boolean; virtual; abstract;
    function IsMethod : Boolean; virtual; abstract;
    function Has__dict__ : Boolean; virtual; abstract;
    function IndexOfChild(AName : string): integer; virtual; abstract;
    procedure GetChildNodes; virtual; abstract;
    procedure CompareToOldItem(OldItem : TBaseNameSpaceItem); virtual;
    property Name : string read GetName;
    property ObjectType : string read GetObjectType;
    property Value : string read GetOrCalculateValue;
    property DocString : string read GetDocString;
    property ChildCount : integer read GetChildCount;
    property ChildNode[Index : integer] : TBaseNameSpaceItem
      read GetChildNode;
    property PyObject : Variant read fPyObject;
    property ExpandCommonTypes : Boolean read fExpandCommonTypes write fExpandCommonTypes;
    property ExpandSequences : Boolean read fExpandSequences write fExpandSequences;
  end;

  TModuleProxy = class;

  TPyBaseInterpreter = class(TObject)
  //  Base (abstract) class for implementing Python Interpreters
  private
    function GetMainModule: TModuleProxy;
  protected
    fInterpreterCapabilities : TInterpreterCapabilities;
    fEngineType : TPythonEngineType;
    fMainModule : TModuleProxy;
    procedure CreateMainModule; virtual; abstract;
    function SystemTempFolder: string; virtual;
  public
    destructor Destroy; override;
    procedure Initialize; virtual;
    function IsPython3000 : Boolean; virtual;
    // Python Path
    function SysPathAdd(const Path : string) : boolean; virtual; abstract;
    function SysPathRemove(const Path : string) : boolean; virtual; abstract;
    function AddPathToPythonPath(const Path : string; AutoRemove : Boolean = True) : IInterface;
    procedure SysPathToStrings(Strings : TStrings); virtual; abstract;
    procedure StringsToSysPath(Strings : TStrings); virtual; abstract;
    // NameSpace
    function GetGlobals : TBaseNameSpaceItem; virtual; abstract;
    procedure GetModulesOnPath(Path : Variant; SL : TStrings); virtual; abstract;
    function NameSpaceFromExpression(const Expr : string) : TBaseNameSpaceItem; virtual; abstract;
    function CallTipFromExpression(const Expr : string;
      var DisplayString, DocString : string) : Boolean; virtual; abstract;
    // Service routines
    procedure HandlePyException(E : EPythonError; SkipFrames : integer = 1); virtual;
    procedure SetCommandLine(ARunConfig : TRunConfiguration); virtual; abstract;
    procedure RestoreCommandLine; virtual; abstract;
    procedure ReInitialize; virtual;
    // FileName conversion
    function ToPythonFileName(const FileName: string): string; virtual;
    function FromPythonFileName(const FileName: string): string; virtual;
    // Main interface
    function ImportModule(Editor : IEditor; AddToNameSpace : Boolean = False) : Variant; virtual; abstract;
    procedure Run(ARunConfig : TRunConfiguration); virtual; abstract;
    function RunSource(Const Source, FileName : Variant; symbol : string = 'single') : boolean; virtual; abstract;
    procedure RunScript(FileName : string); virtual;
    function EvalCode(const Expr : string) : Variant; virtual; abstract;
    procedure SystemCommand(const Cmd : string); virtual; abstract;
    function GetObjectType(Ob : Variant) : string; virtual; abstract;
    function UnitTestResult : Variant; virtual; abstract;
    function NameSpaceItemFromPyObject(aName : string; aPyObject : Variant): TBaseNameSpaceItem; virtual; abstract;
    property EngineType : TPythonEngineType read fEngineType;
    property InterpreterCapabilities : TInterpreterCapabilities read fInterpreterCapabilities;
    property MainModule : TModuleProxy read GetMainModule;
  end;

  TThreadChangeType = (tctAdded, tctRemoved, tctStatusChange);
  TThreadChangeNotifyEvent = procedure(Thread : TThreadInfo;
    ChangeType : TThreadChangeType) of object;

  TPyBaseDebugger = class(TObject)
  {  Base (abstract) class for implementing Python Debuggers }
  protected
    procedure SetCommandLine(ARunConfig : TRunConfiguration); virtual; abstract;
    procedure RestoreCommandLine; virtual; abstract;
    procedure SetDebuggerBreakpoints; virtual; abstract;
    function GetPostMortemEnabled: boolean; virtual;
  public
    // Debugging
    procedure Debug(ARunConfig : TRunConfiguration; InitStepIn : Boolean = False;
            RunToCursorLine : integer = -1); virtual; abstract;
    procedure RunToCursor(Editor : IEditor; ALine: integer); virtual; abstract;
    procedure StepInto; virtual; abstract;
    procedure StepOver; virtual; abstract;
    procedure StepOut; virtual; abstract;
    procedure Resume; virtual; abstract;
    procedure Pause; virtual; abstract;
    procedure Abort; virtual; abstract;
    // Evaluate expression in the current frame
    procedure Evaluate(const Expr : string; out ObjType, Value : string); overload; virtual; abstract;
    function Evaluate(const Expr : string) : TBaseNamespaceItem; overload; virtual; abstract;
    // Like the InteractiveInterpreter runsource but for the debugger frame
    function RunSource(Const Source, FileName : Variant; symbol : string = 'single') : boolean; virtual; abstract;
    // functions to get TBaseNamespaceItems corresponding to a frame's gloabals and locals
    function GetFrameGlobals(Frame : TBaseFrameInfo) : TBaseNameSpaceItem; virtual; abstract;
    function GetFrameLocals(Frame : TBaseFrameInfo) : TBaseNameSpaceItem; virtual; abstract;
    function NameSpaceFromExpression(const Expr : string) : TBaseNameSpaceItem; virtual; abstract;
    procedure MakeThreadActive(Thread : TThreadInfo); virtual; abstract;
    procedure MakeFrameActive(Frame : TBaseFrameInfo); virtual; abstract;
    // post mortem stuff
    property PostMortemEnabled: boolean read GetPostMortemEnabled;
    function HaveTraceback : boolean; virtual; abstract;
    procedure EnterPostMortem; virtual; abstract;
    procedure ExitPostMortem; virtual; abstract;

    class var ThreadChangeNotify : TThreadChangeNotifyEvent;
  end;

  TModuleProxy = class(TParsedModule)
  private
    fPyModule : Variant;
    fIsExpanded : boolean;
    fPyInterpreter: TPyBaseInterpreter;
  protected
    function GetAllExportsVar: string; override;
    function GetDocString: string; override;
    function GetCodeHint : string; override;
  public
    constructor CreateFromModule(AModule : Variant; aPyInterpreter : TPyBaseInterpreter);
    procedure Expand;
    procedure GetNameSpace(SList : TStringList); override;
    property PyModule : Variant read fPyModule;
    property IsExpanded : boolean read fIsExpanded;
    property Interpreter: TPyBaseInterpreter read fPyInterpreter;
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

Const
 CommonTypes: array[1..29] of TIdentMapEntry = (
      (Value: 0; Name: 'NoneType'),
      (Value: 1; Name: 'NotImplementedType'),
      (Value: 2; Name: 'bool'),
      (Value: 3; Name: 'buffer'),
      (Value: 4; Name: 'builtin_function_or_method'),
      (Value: 5; Name: 'code' ),
      (Value: 6; Name: 'complex'),
      (Value: 7; Name: 'dict'),
      (Value: 8; Name: 'dictproxy'),
      (Value: 9; Name: 'ellipsis'),
      (Value: 10; Name: 'file'),
      (Value: 11; Name: 'float'),
      (Value: 12; Name: 'frame'),
      (Value: 13; Name: 'function'),
      (Value: 14; Name: 'generator'),
      (Value: 15; Name: 'getset_descriptor'),
      (Value: 16; Name: 'instancemethod'),
      (Value: 17; Name: 'int'),
      (Value: 18; Name: 'list'),
      (Value: 19; Name: 'long'),
      (Value: 20; Name: 'member_descriptor'),
      (Value: 21; Name: 'method-wrapper'),
      (Value: 22; Name: 'object'),
      (Value: 23; Name: 'slice'),
      (Value: 24; Name: 'str'),
      (Value: 25; Name: 'traceback'),
      (Value: 26; Name: 'tuple'),
      (Value: 27; Name: 'unicode'),
      (Value: 28; Name: 'xrange')
      );


const
   EngineInitFile = 'python_init.py';
   PyScripterInitFile = 'pyscripter_init.py';

implementation

uses
  System.UITypes,
  System.Contnrs,
  Vcl.Dialogs,
  VarPyth,
  JvGnuGettext,
  JclSysInfo,
  StringResources,
  uCommonFunctions,
  cPyControl,
  cPyDebugger,
  cPyScripterSettings,
  cSSHSupport;

{ TPythonPathAdder }
type

  TSysPathFunction = function(const Path : string) : boolean of object;

  TPythonPathAdder = class(TInterfacedObject, IInterface)
  private
    fPath : string;
    fPathAdded : boolean;
    PackageRootAdder : IInterface;
    fAutoRemove : Boolean;
    fSysPathRemove : TSysPathFunction;
  public
    constructor Create(SysPathAdd, SysPathRemove : TSysPathFunction;
      const Path : string; AutoRemove : Boolean = True);
    destructor Destroy; override;
  end;

constructor TPythonPathAdder.Create(SysPathAdd, SysPathRemove : TSysPathFunction;
  const Path: string; AutoRemove : Boolean = True);
var
  S : string;
begin
  inherited Create;
  fPath := ExcludeTrailingPathDelimiter(Path);
  fAutoRemove := AutoRemove;
  fSysPathRemove := SysPathRemove;
  if (fPath <> '') then begin
    // Add parent directory of the root of the package first
    if DirIsPythonPackage(fPath) then begin
      S := ExtractFileDir(GetPackageRootDir(fPath));
      if S <> fPath then
        PackageRootAdder :=
          TPythonPathAdder.Create(SysPathAdd, SysPathRemove, S, AutoRemove);
    end;
    fPathAdded := SysPathAdd(fPath);
  end;
end;

destructor TPythonPathAdder.Destroy;
begin
  PackageRootAdder := nil;  // will remove package root
  if fPathAdded and FAutoRemove then
    fSysPathRemove(fPath);
  inherited;
end;

{ TPyBaseInterpreter }

function TPyBaseInterpreter.AddPathToPythonPath(const Path: string;
  AutoRemove: Boolean): IInterface;
begin
  Result := nil;
  // DirectoryExists would fail when TPythonPathAdder is used with the SSH engine.
  if (fEngineType = peSSH) or DirectoryExists(Path)  then
    Result := TPythonPathAdder.Create(SysPathAdd, SysPathRemove, Path, AutoRemove)
end;

destructor TPyBaseInterpreter.Destroy;
begin
  FreeAndNil(fMainModule);
  inherited;
end;

function TPyBaseInterpreter.GetMainModule: TModuleProxy;
begin
  if not Assigned(fMainModule) then
    CreateMainModule;
  Result := fMainModule;
end;

procedure TPyBaseInterpreter.HandlePyException(E: EPythonError; SkipFrames : integer = 1);
Var
  TI : TTracebackItem;
  FileName : string;
  Editor : IEditor;
begin
  GI_PyIDEServices.Messages.ShowPythonTraceback(SkipFrames);
  GI_PyIDEServices.Messages.AddMessage(E.Message);
  with GetPythonEngine.Traceback do begin
    if ItemCount > 0 then begin
      TI := Items[ItemCount -1];
      FileName := FromPythonFileName(TI.FileName);
      Editor := GI_EditorFactory.GetEditorByNameOrTitle(FileName);
      // Check whether the error occurred in the active editor
      if (Assigned(Editor) and (Editor = GI_PyIDEServices.GetActiveEditor)) or
        PyIDEOptions.JumpToErrorOnException then
      begin
        if GI_PyIDEServices.ShowFilePosition(TI.FileName, TI.LineNo, 1) and
          Assigned(GI_ActiveEditor)
        then
          PyControl.DoErrorPosChanged(TEditorPos.NPos(GI_ActiveEditor, TI.LineNo));
      end;
    end;
  end;
end;

procedure TPyBaseInterpreter.Initialize;
// Execute python_init.py
Var
  FileName : String;
begin
  FileName := TPyScripterSettings.UserDataPath + EngineInitFile;

  try
    RunScript(FileName);
  except
    on E: Exception do
      Vcl.Dialogs.MessageDlg(Format(_(SErrorInitScript),
        [EngineInitFile, E.Message]), mtError, [mbOK], 0);
  end;
end;

function TPyBaseInterpreter.IsPython3000: Boolean;
begin
  Result := GetPythonEngine.IsPython3000;
end;

procedure TPyBaseInterpreter.ReInitialize;
begin
  raise Exception.Create(_(SNotImplented));
end;

procedure TPyBaseInterpreter.RunScript(FileName: string);
Var
  Source : string;
  AnsiSource : AnsiString;
begin
  if FileExists(FileName) then begin
    if IsPython3000 then begin
      Source := CleanEOLs(FileToStr(FileName))+#10;
      RunSource(Source, FileName, 'exec');
    end else begin
      AnsiSource := CleanEOLs(FileToEncodedStr(FileName))+#10;
      RunSource(AnsiSource, FileName, 'exec');
    end;
  end;
end;

function TPyBaseInterpreter.SystemTempFolder: string;
begin
  Result := GetWindowsTempFolder;
end;

function TPyBaseInterpreter.FromPythonFileName(const FileName: string): string;
begin
  if FileName = '' then
    Result := ''
 else if (FileName[1] ='<') and (FileName[Length(FileName)] = '>') then
   Result :=  Copy(FileName, 2, Length(FileName)-2)
 else
  Result := FileName;
end;

function TPyBaseInterpreter.ToPythonFileName(const FileName: string): string;
Var
  Server, FName : string;
begin
  // check for untitled or remote files
  if (FileName.IndexOfAny(['\', '/', '.']) < 0) or TSSHFileName.Parse(FileName, Server, FName) then
    Result := '<' + FileName + '>'
  else
    Result := FileName;
end;

{ TBaseNameSpaceItem }

procedure TBaseNameSpaceItem.CompareToOldItem(OldItem: TBaseNameSpaceItem);
var
  i, Index : integer;
  Child, OldChild : TBaseNameSpaceItem;
begin
  if OldItem.GotBufferedValue then begin
    if OldItem.BufferedValue <> Value then
      Attributes := [nsaChanged];
  end;
  if OldItem.GotChildNodes then begin
    GetChildNodes;
    for i := 0 to ChildCount - 1 do begin
      Child := ChildNode[i];
      Index := OldItem.IndexOfChild(Child.Name);
      if Index >= 0 then begin
        OldChild := OldItem.ChildNode[Index];
        if OldChild.GotBufferedValue then
          Child.CompareToOldItem(OldChild);
      end else
        Child.Attributes := [nsaNew];
    end;
  end;
end;

function TBaseNameSpaceItem.GetOrCalculateValue: string;
begin
  if GotBufferedValue then
    Result := BufferedValue
  else begin
    BufferedValue := GetValue;
    GotBufferedValue := True;
    Result := BufferedValue;
  end;
end;

{ TPyBaseDebugger }

function TPyBaseDebugger.GetPostMortemEnabled: boolean;
begin
  Result := HaveTraceback;
end;

{ TModuleProxy }

procedure TModuleProxy.Expand;
Var
  i : integer;
  VariableProxy : TVariableProxy;
  NS, ChildNS : TBaseNameSpaceItem;
begin
  if Name = '__main__' then begin
    if Assigned(fChildren) then fChildren.Clear;
    fGlobals.Clear;
  end else if fIsExpanded then
    Exit;

  NS := Interpreter.NameSpaceItemFromPyObject(Name, fPyModule);
  try
    for I := 0 to NS.ChildCount - 1 do begin
      ChildNS := NS.ChildNode[i];
      if ChildNS.IsFunction or ChildNS.IsMethod then
       AddChild(TFunctionProxy.CreateFromFunction(ChildNS.Name, ChildNS.PyObject))
      else if ChildNS.IsClass then
        AddChild(TClassProxy.CreateFromClass(ChildNS.Name, ChildNS.PyObject))
      else begin
        VariableProxy := TVariableProxy.CreateFromPyObject(ChildNS.Name, ChildNS.PyObject);
        VariableProxy.Parent := self;
        Globals.Add(VariableProxy);
      end;
    end;
  finally
    NS.Free;
  end;
  fIsExpanded := True;
end;

constructor TModuleProxy.CreateFromModule(AModule: Variant; aPyInterpreter : TPyBaseInterpreter);
begin
  inherited Create;
  if not VarIsPython(AModule) or (AModule.__class__.__name__ <> 'module') then
    Raise Exception.Create('TModuleProxy creation error');
  Name := AModule.__name__;
  fPyModule := AModule;
  fIsExpanded := false;
  fIsProxy := True;
  if BuiltInModule.hasattr(fPyModule, '__file__') then
    FileName := fPyModule.__file__;
  fPyInterpreter := aPyInterpreter;
end;

procedure TModuleProxy.GetNameSpace(SList: TStringList);
begin
  Expand;
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
    Result := Format(_(SPackageProxyCodeHint), [Name])
  else
    Result := Format(_(SModuleProxyCodeHint), [Name]);
end;

{ TClassProxy }

procedure TClassProxy.Expand;
Var
  i : integer;
  VariableProxy : TVariableProxy;
  NS, ChildNS : TBaseNameSpaceItem;
begin
  if fIsExpanded then Exit;

  NS := (GetModule as TModuleProxy).Interpreter.NameSpaceItemFromPyObject(Name, fPyClass);
  NS.ExpandCommonTypes := True;
  NS.ExpandSequences := False;
  try
    for I := 0 to NS.ChildCount - 1 do begin
      ChildNS := NS.ChildNode[i];
      if ChildNS.IsFunction or ChildNS.IsMethod then
       AddChild(TFunctionProxy.CreateFromFunction(ChildNS.Name, ChildNS.PyObject))
      else if ChildNS.IsClass then
        AddChild(TClassProxy.CreateFromClass(ChildNS.Name, ChildNS.PyObject))
      else begin
        VariableProxy := TVariableProxy.CreateFromPyObject(ChildNS.Name, ChildNS.PyObject);
        VariableProxy.Parent := self;
        Attributes.Add(VariableProxy);
      end;
    end;
  finally
    NS.Free;
  end;

  // setup base classes
  try
    for i := 0 to len(fPyClass.__bases__) - 1 do
      SuperClasses.Add(fPyClass.__bases__[i].__name__);
  except
    // absorb this exception - nothing we can do
  end;

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
  Expand;
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
  Expand;
  Result := inherited GetConstructor;
end;

{ TFunctionProxy }

function TFunctionProxy.ArgumentsString: string;
begin
  Result := TPyInternalInterpreter(PyControl.InternalInterpreter).
    PyInteractiveInterpreter.get_arg_text(fPyFunction).__getitem__(0);
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
  i : integer;
  NoOfArgs : integer;
  Variable : TVariable;
  NS, ChildNS : TBaseNameSpaceItem;
begin
  if fIsExpanded then Exit;

  NS := (GetModule as TModuleProxy).Interpreter.NameSpaceItemFromPyObject(Name, fPyFunction);
  NS.ExpandCommonTypes := True;
  NS.ExpandSequences := False;
  try
    for I := 0 to NS.ChildCount - 1 do begin
      ChildNS := NS.ChildNode[i];
      if ChildNS.IsFunction or ChildNS.IsMethod then
       AddChild(TFunctionProxy.CreateFromFunction(ChildNS.Name, ChildNS.PyObject))
      else if ChildNS.IsClass then
        AddChild(TClassProxy.CreateFromClass(ChildNS.Name, ChildNS.PyObject))
      else begin
        AddChild(TVariableProxy.CreateFromPyObject(ChildNS.Name, ChildNS.PyObject));
      end;
    end;
  finally
    NS.Free;
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
  end else if BuiltinModule.hasattr(fPyFunction, '__code__') then begin  //Python 3000
    NoOfArgs := fPyFunction.__code__.co_argcount;
    for i := 0 to len(fPyFunction.__code__.co_varnames) - 1 do begin
      Variable := TVariable.Create;
      Variable.Name := fPyFunction.__code__.co_varnames[i];
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
  Expand;
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
  Expand;
  inherited;
end;

procedure TVariableProxy.Expand;
Var
  i : integer;
  NS, ChildNS : TBaseNameSpaceItem;
begin
  if fIsExpanded then Exit;

  NS := (GetModule as TModuleProxy).Interpreter.NameSpaceItemFromPyObject(Name, fPyObject);
  NS.ExpandCommonTypes := True;
  NS.ExpandSequences := False;
  try
    for I := 0 to NS.ChildCount - 1 do begin
      ChildNS := NS.ChildNode[i];
      if ChildNS.IsFunction or ChildNS.IsMethod then
       AddChild(TFunctionProxy.CreateFromFunction(ChildNS.Name, ChildNS.PyObject))
      else if ChildNS.IsClass then
        AddChild(TClassProxy.CreateFromClass(ChildNS.Name, ChildNS.PyObject))
      else if ChildNS.IsModule then
        AddChild(TModuleProxy.CreateFromModule(ChildNS.PyObject,
          (GetModule as TModuleProxy).Interpreter))
      else begin
        AddChild(TVariableProxy.CreateFromPyObject(ChildNS.Name, ChildNS.PyObject));
      end;
    end;
  finally
    NS.Free;
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
    Fmt := _(SLocalVariableCodeHint)
  else if Parent is TParsedClass then
    Fmt := _(SInstanceVariableCodeHint)
  else if Parent is TParsedModule then
    Fmt := _(SGlobalVariableCodeHint)
  else
    Fmt := '';
  if Fmt <> '' then begin
    Result := Format(Fmt,
      [Name, Parent.Name, '']);

    ObjType := BuiltInModule.type(PyObject).__name__;
    Result := Result + Format(_(SVariableTypeCodeHint), [ObjType]);
  end else
    Result := '';
end;


{ TThreadInfo }

constructor TThreadInfo.Create;
begin
  CallStack := TObjectList<TBaseFrameInfo>.Create(True);
end;

destructor TThreadInfo.Destroy;
begin
  FreeAndNil(CallStack);
  inherited;
end;

end.
