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

  TPyBaseDebugger = class;

  TPyBaseInterpreter = class(TObject)
  //  Base (abstract) class for implementing Python Interpreters
  protected
    fInterpreterCapabilities : TInterpreterCapabilities;
    fEngineType : TPythonEngineType;
    fCanDoPostMortem : Boolean;
    function SystemTempFolder: string; virtual;
    function GetInterpreter: Variant; virtual; abstract;
  public
    procedure Initialize; virtual;
    // Create matching debugger
    function CreateDebugger: TPyBaseDebugger; virtual; abstract;
    // Python Path
    function SysPathAdd(const Path : string) : boolean; virtual; abstract;
    function SysPathRemove(const Path : string) : boolean; virtual; abstract;
    function AddPathToPythonPath(const Path : string; AutoRemove : Boolean = True) : IInterface;
    procedure SysPathToStrings(Strings : TStrings); virtual; abstract;
    procedure StringsToSysPath(Strings : TStrings); virtual; abstract;
    // NameSpace
    function GetGlobals : TBaseNameSpaceItem; virtual; abstract;
    procedure GetModulesOnPath(const Path : Variant; SL : TStrings); virtual; abstract;
    function NameSpaceFromExpression(const Expr : string) : TBaseNameSpaceItem; virtual; abstract;
    function CallTipFromExpression(const Expr : string;
      var DisplayString, DocString : string) : Boolean; virtual; abstract;
    // Service routines
    procedure HandlePyException(Traceback: TPythonTraceback; ErrorMsg : string; SkipFrames : integer = 1); virtual;
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
    property CanDoPostMortem: Boolean read fCanDoPostMortem write fCanDoPostMortem;
    property PyInteractiveInterpreter : Variant read GetInterpreter;
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

implementation

uses
  System.UITypes,
  System.Contnrs,
  System.IOUtils,
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
      S := TPath.GetDirectoryName(GetPackageRootDir(fPath));
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

procedure TPyBaseInterpreter.HandlePyException(Traceback: TPythonTraceback; ErrorMsg : string; SkipFrames : integer = 1);
Var
  TI : TTracebackItem;
  FileName : string;
  Editor : IEditor;
begin
  GI_PyIDEServices.Messages.ShowPythonTraceback(Traceback, SkipFrames);
  GI_PyIDEServices.Messages.AddMessage(ErrorMsg);
  with Traceback do begin
    if ItemCount > 0 then begin
      TI := Items[ItemCount -1];
      FileName := FromPythonFileName(TI.FileName);
      Editor := GI_EditorFactory.GetEditorByFileId(FileName);
      // Check whether the error occurred in the active editor
      if (Assigned(Editor) and (Editor = GI_PyIDEServices.ActiveEditor)) or
        PyIDEOptions.JumpToErrorOnException then
      begin
        if GI_PyIDEServices.ShowFilePosition(TI.FileName, TI.LineNo, 1) and
          Assigned(GI_ActiveEditor)
        then
          PyControl.ErrorPos := TEditorPos.NPos(GI_ActiveEditor, TI.LineNo);
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
Var
  Source : string;
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
  I, Index : integer;
  Child, OldChild : TBaseNameSpaceItem;
begin
  if OldItem.GotBufferedValue then begin
    if OldItem.BufferedValue <> Value then
      Attributes := [nsaChanged];
  end;
  if OldItem.GotChildNodes then begin
    GetChildNodes;
    for I := 0 to ChildCount - 1 do begin
      Child := ChildNode[I];
      if (ObjectType <> 'list') and (ObjectType <> 'tuple') then
        Index := OldItem.IndexOfChild(Child.Name)
      else if I < OldItem.ChildCount then
        Index := I
      else
        Index := -1;
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
