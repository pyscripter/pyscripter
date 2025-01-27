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
  System.Classes,
  System.Generics.Collections,
  uEditAppIntfs,
  cPySupportTypes,
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
    function GetFunctionName: string; virtual; abstract;
    function GetFileName: string; virtual; abstract;
    function GetLine: Integer; virtual; abstract;
  public
    property FunctionName: string read GetFunctionName;
    property FileName: string read GetFileName;
    property Line: Integer read GetLine;
  end;

  // Base (abstract) class for thread information
  TThreadInfo = class(TObject)
    Thread_ID: Int64;
    Name: string;
    Status: TThreadStatus;
    CallStack: TObjectList<TBaseFrameInfo>;
    constructor Create;
    destructor Destroy; override;
  end;

  // Base (abstract) class for Namespace item information
  TBaseNameSpaceItem = class(TObject)
  protected
    FPyObject: Variant;
    FExpandCommonTypes: Boolean;
    FExpandSequences: Boolean;
    FGotChildNodes: Boolean;
    FGotBufferedValue: Boolean;
    FBufferedValue: string;
    FQualifiedObjectType: string;
    function GetOrCalculateValue: string;
    function GetName: string; virtual; abstract;
    function GetObjectType: string; virtual; abstract;
    function GetValue: string; virtual; abstract;
    function GetDocString: string; virtual; abstract;
    function GetChildCount: Integer; virtual; abstract;
    function GetChildNode(Index: Integer): TBaseNameSpaceItem; virtual; abstract;
  public
    Attributes: TNamespaceItemAttributes;
    function IsClass: Boolean; virtual; abstract;
    function IsDict: Boolean; virtual; abstract;
    function IsModule: Boolean; virtual; abstract;
    function IsFunction: Boolean; virtual; abstract;
    function IsMethod: Boolean; virtual; abstract;
    function Has__dict__: Boolean; virtual; abstract;
    function IndexOfChild(AName: string): Integer; virtual; abstract;
    procedure GetChildNodes; virtual; abstract;
    procedure CompareToOldItem(OldItem: TBaseNameSpaceItem); virtual;
    property Name: string read GetName;
    property ObjectType: string read GetObjectType;
    property Value: string read GetOrCalculateValue;
    property DocString: string read GetDocString;
    property ChildCount: Integer read GetChildCount;
    property ChildNode[Index: Integer]: TBaseNameSpaceItem
      read GetChildNode;
    property PyObject: Variant read FPyObject;
    property QualifiedObjectType: string read FQualifiedObjectType;
    property ExpandCommonTypes: Boolean read FExpandCommonTypes write FExpandCommonTypes;
    property ExpandSequences: Boolean read FExpandSequences write FExpandSequences;
  end;

  TPyBaseDebugger = class;

  TPyBaseInterpreter = class(TObject)
  //  Base (abstract) class for implementing Python Interpreters
  protected
    FInterpreterCapabilities: TInterpreterCapabilities;
    FEngineType: TPythonEngineType;
    FCanDoPostMortem: Boolean;
    FPythonVersion: string;
    FPythonPlatform: string;
    function SystemTempFolder: string; virtual;
    function GetInterpreter: Variant; virtual; abstract;
  public
    procedure Initialize; virtual;
    // Create matching debugger
    function CreateDebugger: TPyBaseDebugger; virtual; abstract;
    // Python Path
    function SysPathAdd(const Path: string): Boolean; virtual; abstract;
    function SysPathRemove(const Path: string): Boolean; virtual; abstract;
    function AddPathToPythonPath(const Path: string; AutoRemove: Boolean = True): IInterface;
    procedure SysPathToStrings(Strings: TStrings); virtual; abstract;
    procedure StringsToSysPath(Strings: TStrings); virtual; abstract;
    // NameSpace
    function GetGlobals: TBaseNameSpaceItem; virtual; abstract;
    function NameSpaceFromExpression(const Expr: string): TBaseNameSpaceItem; virtual; abstract;
    function CallTipFromExpression(const Expr: string;
      var DisplayString, DocString: string): Boolean; virtual; abstract;
    // Service routines
    procedure HandlePyException(Traceback: TPythonTraceback; ErrorMsg: string;
      SkipFrames: Integer = 1); virtual;
    procedure SetCommandLine(ARunConfig: TRunConfiguration); virtual; abstract;
    procedure RestoreCommandLine; virtual; abstract;
    procedure ReInitialize; virtual;
    // FileName conversion
    function ToPythonFileName(const FileName: string): string; virtual;
    function FromPythonFileName(const FileName: string): string; virtual;
    // Main interface
    function ImportModule(Editor: IEditor; AddToNameSpace: Boolean = False): Variant; virtual; abstract;
    procedure Run(ARunConfig: TRunConfiguration); virtual; abstract;
    function RunSource(const Source, FileName: string; const Symbol: string = 'single'): Boolean; virtual; abstract;
    procedure RunScript(FileName: string); virtual;
    function EvalCode(const Expr: string): Variant; virtual; abstract;
    procedure SystemCommand(const Cmd: string); virtual; abstract;
    function GetObjectType(Obj: Variant): string; virtual; abstract;
    function UnitTestResult: Variant; virtual; abstract;
    function NameSpaceItemFromPyObject(AName: string; APyObject: Variant): TBaseNameSpaceItem; virtual; abstract;
    procedure Pickle(AValue: Variant; const FileName: string); virtual; abstract;
    property PythonVersion: string read FPythonVersion;
    property PythonPlatform: string read FPythonPlatform;
    property EngineType: TPythonEngineType read FEngineType;
    property InterpreterCapabilities: TInterpreterCapabilities read FInterpreterCapabilities;
    property CanDoPostMortem: Boolean read FCanDoPostMortem write FCanDoPostMortem;
    property PyInteractiveInterpreter: Variant read GetInterpreter;
  end;

  TThreadChangeType = (tctAdded, tctRemoved, tctStatusChange);
  TThreadChangeNotifyEvent = procedure(Thread: TThreadInfo;
    ChangeType: TThreadChangeType) of object;

  TPyBaseDebugger = class(TObject)
  {  Base (abstract) class for implementing Python Debuggers }
  protected
    procedure SetCommandLine(ARunConfig: TRunConfiguration); virtual; abstract;
    procedure RestoreCommandLine; virtual; abstract;
    procedure SetDebuggerBreakpoints; virtual; abstract;
    function GetPostMortemEnabled: Boolean; virtual;
  public
    // Debugging
    procedure Debug(ARunConfig: TRunConfiguration; InitStepIn: Boolean = False;
            RunToCursorLine: Integer = -1); virtual; abstract;
    procedure RunToCursor(Editor: IEditor; ALine: Integer); virtual; abstract;
    procedure StepInto; virtual; abstract;
    procedure StepOver; virtual; abstract;
    procedure StepOut; virtual; abstract;
    procedure Resume; virtual; abstract;
    procedure Pause; virtual; abstract;
    procedure Abort; virtual; abstract;
    // Evaluate expression in the current frame
    procedure Evaluate(const Expr: string; out ObjType, Value: string); overload; virtual; abstract;
    function Evaluate(const Expr: string): TBaseNameSpaceItem; overload; virtual; abstract;
    // Like the InteractiveInterpreter runsource but for the debugger frame
    function RunSource(const Source, FileName: string; const Symbol: string = 'single'): Boolean; virtual; abstract;
    // functions to get TBaseNamespaceItems corresponding to a frame's gloabals and locals
    function GetFrameGlobals(Frame: TBaseFrameInfo): TBaseNameSpaceItem; virtual; abstract;
    function GetFrameLocals(Frame: TBaseFrameInfo): TBaseNameSpaceItem; virtual; abstract;
    function NameSpaceFromExpression(const Expr: string): TBaseNameSpaceItem; virtual; abstract;
    procedure MakeThreadActive(Thread: TThreadInfo); virtual; abstract;
    procedure MakeFrameActive(Frame: TBaseFrameInfo); virtual; abstract;
    // post mortem stuff
    function HaveTraceback: Boolean; virtual; abstract;
    procedure EnterPostMortem; virtual; abstract;
    procedure ExitPostMortem; virtual; abstract;
    property PostMortemEnabled: Boolean read GetPostMortemEnabled;
  public
    class var ThreadChangeNotify: TThreadChangeNotifyEvent;
  end;

const
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

implementation

uses
  System.SysUtils,
  System.IOUtils,
  Vcl.Dialogs,
  JvGnugettext,
  JclSysInfo,
  StringResources,
  uCommonFunctions,
  cPyControl,
  cPyScripterSettings,
  cSSHSupport;

{ TPythonPathAdder }
type

  TSysPathFunction = function(const Path: string): Boolean of object;

  TPythonPathAdder = class(TInterfacedObject, IInterface)
  private
    FPath: string;
    FPathAdded: Boolean;
    FPackageRootAdder: IInterface;
    FAutoRemove: Boolean;
    FSysPathRemove: TSysPathFunction;
  public
    constructor Create(SysPathAdd, SysPathRemove: TSysPathFunction;
      const Path: string; AutoRemove: Boolean = True);
    destructor Destroy; override;
  end;

constructor TPythonPathAdder.Create(SysPathAdd, SysPathRemove: TSysPathFunction;
  const Path: string; AutoRemove: Boolean = True);
begin
  inherited Create;
  FPath := ExcludeTrailingPathDelimiter(Path);
  FAutoRemove := AutoRemove;
  FSysPathRemove := SysPathRemove;
  if (FPath <> '') then begin
    // Add parent directory of the root of the package first
    if DirIsPythonPackage(FPath) then begin
      var ParentDir := TPath.GetDirectoryName(GetPackageRootDir(FPath));
      if ParentDir <> FPath then
        FPackageRootAdder := TPythonPathAdder.Create(SysPathAdd, SysPathRemove,
          ParentDir, AutoRemove);
    end;
    FPathAdded := SysPathAdd(FPath);
  end;
end;

destructor TPythonPathAdder.Destroy;
begin
  FPackageRootAdder := nil;  // will remove package root
  if FPathAdded and FAutoRemove then
    FSysPathRemove(FPath);
  inherited;
end;

{ TPyBaseInterpreter }

function TPyBaseInterpreter.AddPathToPythonPath(const Path: string;
  AutoRemove: Boolean): IInterface;
begin
  Result := nil;
  // DirectoryExists would fail when TPythonPathAdder is used with the SSH engine.
  if (FEngineType = peSSH) or DirectoryExists(Path)  then
    Result := TPythonPathAdder.Create(SysPathAdd, SysPathRemove, Path, AutoRemove);
end;

procedure TPyBaseInterpreter.HandlePyException(Traceback: TPythonTraceback;
  ErrorMsg: string; SkipFrames: Integer = 1);
var
  TBItem: TTracebackItem;
  FileName: string;
  Editor: IEditor;
begin
  GI_PyIDEServices.Messages.ShowPythonTraceback(Traceback, SkipFrames);
  GI_PyIDEServices.Messages.AddMessage(ErrorMsg);
  with Traceback do begin
    if ItemCount > 0 then begin
      TBItem := Items[ItemCount -1];
      FileName := FromPythonFileName(TBItem.FileName);
      Editor := GI_EditorFactory.GetEditorByFileId(FileName);
      // Check whether the error occurred in the active editor
      if (Assigned(Editor) and (Editor = GI_PyIDEServices.ActiveEditor)) or
        PyIDEOptions.JumpToErrorOnException then
      begin
        if GI_PyIDEServices.ShowFilePosition(FileName, TBItem.LineNo, 1)
        then
          GI_PyControl.ErrorPos := TEditorPos.New(FileName, TBItem.LineNo);
      end;
    end;
  end;
end;

procedure TPyBaseInterpreter.Initialize;
// Execute python_init.py
begin
  try
    RunScript(TPyScripterSettings.EngineInitFile);
  except
    on E: Exception do
      StyledMessageDlg(Format(_(SErrorInitScript),
        [TPyScripterSettings.EngineInitFile, E.Message]), mtError, [mbOK], 0);
  end;
end;

procedure TPyBaseInterpreter.ReInitialize;
begin
  raise Exception.Create(_(SNotImplented));
end;

procedure TPyBaseInterpreter.RunScript(FileName: string);
var
  Source: string;
begin
  if FileExists(FileName) then begin
    Source := CleanEOLs(FileToStr(FileName))+#10;
    RunSource(Source, FileName, 'exec');
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
var
  Server, FName: string;
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
  Index: Integer;
  Child, OldChild: TBaseNameSpaceItem;
begin
  if OldItem.FGotBufferedValue then begin
    if OldItem.FBufferedValue <> Value then
      Attributes := [nsaChanged];
  end;
  if OldItem.FGotChildNodes then begin
    GetChildNodes;
    for var I := 0 to ChildCount - 1 do begin
      Child := ChildNode[I];
      if (ObjectType <> 'list') and (ObjectType <> 'tuple') then
        Index := OldItem.IndexOfChild(Child.Name)
      else if I < OldItem.ChildCount then
        Index := I
      else
        Index := -1;
      if Index >= 0 then begin
        OldChild := OldItem.ChildNode[Index];
        if OldChild.FGotBufferedValue then
          Child.CompareToOldItem(OldChild);
      end else
        Child.Attributes := [nsaNew];
    end;
  end;
end;

function TBaseNameSpaceItem.GetOrCalculateValue: string;
begin
  if FGotBufferedValue then
    Result := FBufferedValue
  else begin
    FBufferedValue := GetValue;
    FGotBufferedValue := True;
    Result := FBufferedValue;
  end;
end;

{ TPyBaseDebugger }

function TPyBaseDebugger.GetPostMortemEnabled: Boolean;
begin
  Result := HaveTraceback;
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
