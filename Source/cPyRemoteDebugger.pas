{-----------------------------------------------------------------------------
 Unit Name: cPyRemoteDebugger
 Author:    Kiriakos Vlahos
 Date:      23-April-2006
 Purpose:   Remote debugger using an Rpyc based server
 History:
 -----------------------------------------------------------------------------}
unit cPyRemoteDebugger;

interface

uses
  WinApi.Windows,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Contnrs,
  System.Generics.Collections,
  System.Variants,
  System.SyncObjs,
  System.Threading,
  Vcl.Forms,
  uSysUtils,
  PythonEngine,
  uEditAppIntfs,
  cTools,
  cPySupportTypes,
  cPyBaseDebugger,
  cPyDebugger;


type
  TRemoteDebuggerClass = class of TPyRemDebugger;

  { Rpyc based remote Python Interpreter  }
  TPyRemoteInterpreter = class(TPyBaseInterpreter)
  private
    fUseNamedPipes : boolean;
    fNamedPipeStream : Variant;
    procedure CreateAndConnectToServer;
    procedure StoreServerProcessInfo(const ProcessInfo: TProcessInformation; InWritePipe: PHandle);
  protected
    const RemoteServerBaseName = 'remserver.py';
    const RpycZipModule = 'rpyc.zip';
  protected
    Rpyc : Variant;
    Conn : Variant;
    RPI : Variant; // RemotePythonInterpreter
    OutputRedirector : Variant;
    fServerIsAvailable : Boolean;
    fOldargv : Variant;
    fOldsysmodules : Variant;
    fConnected : Boolean;
    fSocketPort: integer;
    fServerFile: string;
    fRpycPath : string;
    ServerProcessOptions : TJclExecuteCmdProcessOptions;
    ServerProcessInfo: TProcessInformation;
    ServerTask : ITask;
    DebuggerClass : TRemoteDebuggerClass;
    procedure CreateAndRunServerProcess; virtual;
    procedure ConnectToServer;
    procedure ShutDownServer;  virtual;
    procedure ProcessServerOutput(const Bytes: TBytes; BytesRead: Cardinal);
    function GetInterpreter: Variant; override;
  public
    constructor Create(AEngineType : TPythonEngineType = peRemote);
    destructor Destroy; override;
    function CreateDebugger: TPyBaseDebugger; override;
    function Compile(ARunConfig : TRunConfiguration) : Variant;
    procedure HandleRemoteException(const ExcInfo : Variant; SkipFrames : integer = 1);
    procedure ReInitialize; override;
    procedure CheckConnected(Quiet : Boolean = False; Abort : Boolean = True);
    procedure ServeConnection(MaxCount : integer = 0);
    // Python Path
    function SysPathAdd(const Path : string) : boolean; override;
    function SysPathRemove(const Path : string) : boolean; override;
    procedure SysPathToStrings(Strings : TStrings); override;
    procedure StringsToSysPath(Strings : TStrings); override;
    // NameSpace
    function GetGlobals : TBaseNameSpaceItem; override;
    procedure GetModulesOnPath(const Path : Variant; SL : TStrings); override;
    function NameSpaceFromExpression(const Expr : string) : TBaseNameSpaceItem; override;
    function CallTipFromExpression(const Expr : string;
      var DisplayString, DocString : string) : Boolean; override;
    // Service routines
    procedure SetCommandLine(ARunConfig : TRunConfiguration); override;
    procedure RestoreCommandLine; override;
    // Main interface
    function ImportModule(Editor : IEditor; AddToNameSpace : Boolean = False) : Variant; override;
    procedure Run(ARunConfig : TRunConfiguration); override;
    function RunSource(Const Source, FileName : Variant; symbol : string = 'single') : boolean; override;
    function EvalCode(const Expr : string) : Variant; override;
    procedure SystemCommand(const Cmd : string); override;
    function GetObjectType(Ob : Variant) : string; override;
    function UnitTestResult : Variant; override;
    function NameSpaceItemFromPyObject(aName : string; aPyObject : Variant): TBaseNameSpaceItem; override;

    property IsAvailable : Boolean read fServerIsAvailable;
    property Connected : Boolean read fConnected;
  end;


  TRemNameSpaceItem = class(TNameSpaceItem)
  { Implementation of the Base class for the remote debugger }
  private
    fRemotePython : TPyRemoteInterpreter;
    fIsProxy : Boolean;
  protected
    procedure FillObjectInfo; override;
    function GetObjectType : string; override;
    function GetValue : string; override;
    function GetDocString : string; override;
    function GetChildCount : integer; override;
  public
    constructor Create(aName : string; const aPyObject : Variant;
      RemotePython : TPyRemoteInterpreter);
    procedure GetChildNodes; override;
    property IsProxy : Boolean read fIsProxy;
  end;

  TPyRemDebugger = class(TPyBaseDebugger)
  { Remote debugger using Rpyc for communication with the server }
  private
    fExecPaused: Boolean;
    fDebugManager : Variant;
    fMainDebugger : Variant;
    fLineCache : Variant;
    fMainThread : TThreadInfo;
    fThreads : TObjectDictionary<Int64, TThreadInfo>;
    procedure SyncThreadInfo(Thread_ID: Int64; ThreadName: string; ThreadStatus : integer);
  protected
    fDebuggerCommand : TDebuggerCommand;
    fRemotePython : TPyRemoteInterpreter;
    procedure SetCommandLine(ARunConfig : TRunConfiguration); override;
    procedure RestoreCommandLine; override;
    procedure SetDebuggerBreakpoints; override;
    procedure LoadLineCache;
    procedure DoDebuggerCommand;
    procedure UserCall(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
    procedure UserLine(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
    procedure UserThread(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
    procedure UserException(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
    procedure UserYield(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
    // Fills in CallStackList with TBaseFrameInfo objects
    procedure GetCallStack(CallStackList : TObjectList<TBaseFrameInfo>;
      Frame, Botframe : Variant);
    function GetPostMortemEnabled: boolean; override;
  public
    constructor Create(RemotePython : TPyRemoteInterpreter);
    destructor Destroy; override;

    // Debugging
    procedure Debug(ARunConfig : TRunConfiguration; InitStepIn : Boolean = False;
      RunToCursorLine : integer = -1); override;
    procedure RunToCursor(Editor : IEditor; ALine: integer); override;
    procedure StepInto; override;
    procedure StepOver; override;
    procedure StepOut; override;
    procedure Resume; override;
    procedure Pause; override;
    procedure Abort; override;
    // Evaluate expression in the current frame
    procedure Evaluate(const Expr : string; out ObjType, Value : string); overload; override;
    function Evaluate(const Expr : string) : TBaseNamespaceItem; overload; override;
    // Like the InteractiveInterpreter runsource but for the debugger frame
    function RunSource(Const Source, FileName : Variant; symbol : string = 'single') : boolean; override;
    // functions to get TBaseNamespaceItems corresponding to a frame's gloabals and locals
    function GetFrameGlobals(Frame : TBaseFrameInfo) : TBaseNameSpaceItem; override;
    function GetFrameLocals(Frame : TBaseFrameInfo) : TBaseNameSpaceItem; override;
    function NameSpaceFromExpression(const Expr : string) : TBaseNameSpaceItem; override;
    procedure MakeThreadActive(Thread : TThreadInfo); override;
    procedure MakeFrameActive(Frame : TBaseFrameInfo); override;
    // post mortem stuff
    function HaveTraceback : boolean; override;
    procedure EnterPostMortem; override;
    procedure ExitPostMortem; override;
  end;

implementation

uses
  System.Generics.Defaults,
  System.StrUtils,
  System.IOUtils,
  Vcl.Dialogs,
  VarPyth,
  JclSysInfo,
  JvJCLUtils,
  JvDSADialogs,
  JvGnugettext,
  StringResources,
  cProjectClasses,
  cPyScripterSettings,
  cPyControl,
  uCommonFunctions,
  cSSHSupport;

{ TRemNameSpaceItem }
constructor TRemNameSpaceItem.Create(aName : string; const aPyObject : Variant;
  RemotePython : TPyRemoteInterpreter);
begin
  inherited Create(aName, aPyObject);
  fRemotePython := RemotePython;
  fIsProxy := VarIsInstanceOf(fPyObject,  fRemotePython.Rpyc.BaseNetref);
end;

function TRemNameSpaceItem.GetChildCount: integer;
var
  SuppressOutput : IInterface;
begin
  if Assigned(fChildNodes) then
    Result := fChildNodes.Count
  else if fChildCount >= 0 then
    Result := fChildCount
  else begin
    if not IsProxy then
      Result := inherited GetChildCount
    else begin
      fRemotePython.CheckConnected;
      SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
      try
        Result := fRemotePython.RPI.membercount(fPyObject, ExpandSequences, ExpandCommonTypes, ExpandSequences);
      except
        Result := 0;
      end;
    end;
    fChildCount := Result;
  end;
end;

procedure TRemNameSpaceItem.GetChildNodes;
Var
  FullInfoTuple, APyObject: Variant;
  PyFullInfoTuple, PyMemberInfo, PyFullInfo: PPyObject;
  i : integer;
  ObjName : string;
  NameSpaceItem : TRemNameSpaceItem;
  SuppressOutput : IInterface;
begin
  if not (Assigned(fChildNodes) or GotChildNodes) then begin
    fRemotePython.CheckConnected;
    GotChildNodes := True;  //Do not try again
    SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
    try
      if IsProxy then
        FullInfoTuple := fRemotePython.RPI.safegetmembersfullinfo(fPyObject, ExpandSequences,
          ExpandCommonTypes, ExpandSequences)
      else
        FullInfoTuple := PyControl.InternalInterpreter.PyInteractiveInterpreter.safegetmembersfullinfo(fPyObject,
          ExpandSequences, ExpandCommonTypes, ExpandSequences);
      fChildCount := len(FullInfoTuple);

      if fChildCount > 0 then begin
        fChildNodes := TStringList.Create;
        fChildNodes.CaseSensitive := True;

        PyFullInfoTuple := ExtractPythonObjectFrom(FullInfoTuple);
        for i := 0 to fChildCount - 1 do with GetPythonEngine do begin
          PyMemberInfo := PyTuple_GetItem(PyFullInfoTuple, i);
          ObjName :=  PyUnicodeAsString(PyTuple_GetItem(PyMemberInfo, 0));
          PyFullInfo := PyTuple_GetItem(PyMemberInfo, 1);
          APyObject := VarPythonCreate(PyTuple_GetItem(PyFullInfo, 0));

          NameSpaceItem := TRemNameSpaceItem.Create(ObjName, APyObject, fRemotePython);
          NameSpaceItem.ExpandCommonTypes := ExpandCommonTypes;
          NameSpaceItem.ExpandSequences := ExpandSequences;

//          NameSpaceItem.BufferedValue := PyString_AsWideString(PyTuple_GetItem(PyFullInfo, 1));
//          NameSpaceItem.GotBufferedValue := True;
          NameSpaceItem.fObjectType := PyUnicodeAsString(PyTuple_GetItem(PyFullInfo, 1));
          NameSpaceItem.fObjectInfo := PyLong_AsLong(PyTuple_GetItem(PyFullInfo, 2));
//          if NameSpaceItem.IsProxy then
//            NameSpaceItem.fChildCount := PyInt_AsLong(PyTuple_GetItem(PyFullInfo, 3))
//          else
//            NameSpaceItem.fChildCount := NameSpaceItem.ChildCount;

          fChildNodes.AddObject(ObjName, NameSpaceItem);
        end;
        GetPythonEngine.CheckError;
        if (ObjectType <> 'list') and (ObjectType <> 'tuple') then
          fChildNodes.CustomSort(ComparePythonIdents);
      end;
    except
      fChildCount := 0;

      //StyledMessageDlg(Format(_(SErrorGettingNamespace), [fName]), mtError, [mbAbort], 0);
      System.SysUtils.Abort;
    end;
  end;
end;

function TRemNameSpaceItem.GetDocString: string;
Var
  SuppressOutput : IInterface;
begin
  fRemotePython.CheckConnected;
  SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
  if not IsProxy then
    Result := inherited GetDocString
  else
    try
      Result := fRemotePython.RPI.inspect.getdoc(fPyObject);
    except
      Result := '';
    end;
end;

procedure TRemNameSpaceItem.FillObjectInfo;
var
  SuppressOutput : IInterface;
begin
  if not IsProxy then
    inherited
  else begin
    fRemotePython.CheckConnected;
    SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
    try
      fObjectInfo := fRemotePython.RPI.objectinfo(fPyObject);
    except
      fObjectInfo := 0;
    end;
  end;
end;

function TRemNameSpaceItem.GetObjectType: string;
begin
  if fObjectType <> '' then
    Result := fObjectType
  else begin
    Result := fRemotePython.GetObjectType(fPyObject);
    fObjectType := Result;
  end;
end;

function TRemNameSpaceItem.GetValue: string;
Var
  SuppressOutput : IInterface;
begin
  fRemotePython.CheckConnected;
  SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
  if not IsProxy then
    Result := inherited GetValue
  else
    try
      Result :=  fRemotePython.RPI.saferepr(fPyObject);
    except
      Result := '';
    end;
end;

{ TPyRemoteInterpreter }

function TPyRemoteInterpreter.CallTipFromExpression(const Expr: string;
  var DisplayString, DocString: string): Boolean;
var
  Py: IPyEngineAndGIL;
  LookupObj, ArgText, PyDocString: Variant;
  SuppressOutput : IInterface;
begin
  CheckConnected;
  Result := False;
  DisplayString := '';
  DocString := '';
  if Expr = '' then Exit;

  Py := GI_PyControl.SafePyEngine;
  SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
  try
    //Evaluate the lookup expression and get the hint text
    LookupObj := RPI.evalcode(Expr);
    ArgText := RPI.get_arg_text(LookupObj);
    DisplayString := ArgText.__getitem__(0);
    PyDocString := ArgText.__getitem__(1);
    Result := True;

    if not VarIsNone(PyDocString) then begin
      //DocString := GetNthLine(PyDocString, 1)
      DocString := PyDocString;
      DocString := GetLineRange(DocString, 1, 20);  // first 20 lines
    end else
      DocString := '';
  except
  end;
end;

procedure TPyRemoteInterpreter.CheckConnected(Quiet : Boolean = False; Abort : Boolean = True);
begin
  if not (Assigned(ServerTask) and fServerIsAvailable and fConnected and
    (ServerTask.Status = TTaskStatus.Running)) then
  begin
    fConnected := False;
    if not Quiet then
      StyledMessageDlg(_(SRemoteServerNotConnected),
        mtError, [mbAbort], 0);
    //VariablesWindow.VariablesTree.Enabled := False;
    if Abort then
      System.SysUtils.Abort;
  end;
end;

function TPyRemoteInterpreter.Compile(ARunConfig: TRunConfiguration): Variant;
Var
  Py: IPyEngineAndGIL;
  FName : string;
  Source : Variant;
  ExcInfo, Error : Variant;
  FileName : string;
  LineNo, Offset : integer;
  Editor : IEditor;
begin
  CheckConnected;
  if PyControl.DebuggerState <> dsInactive then begin
    StyledMessageDlg(_(SCannotCompileWhileRunning),
      mtError, [mbAbort], 0);
    System.SysUtils.Abort;
  end;

  Py := GI_PyControl.SafePyEngine;

  VarClear(Result);
  PyControl.ErrorPos := TEditorPos.EmptyPos;

  GI_PyIDEServices.Messages.ClearMessages;

  Editor := GI_EditorFactory.GetEditorByFileId(ARunConfig.ScriptName);
  if Assigned(Editor) then
    Source := CleanEOLs(Editor.SynEdit.Text) + WideLF
  else
    try
      Source := CleanEOLs(FileToStr(ARunConfig.ScriptName)) + WideLF
    except
      on E: Exception do begin
        StyledMessageDlg(Format(_(SFileOpenError), [ARunConfig.ScriptName, E.Message]), mtError, [mbOK], 0);
        System.SysUtils.Abort;
      end;
    end;
  FName := ToPythonFileName(ARunConfig.ScriptName);

  try
    // Source is a UnicodeString
    Result := RPI.rem_compile(VarPythonCreate(Source), VarPythonCreate(FName));

    Py.PythonEngine.CheckError;
    ExcInfo := RPI.exc_info;
    if not VarIsNone(RPI.exc_info) then begin
      VarClear(Result);
      Error := ExcInfo.__getitem__(1);
      if ExcInfo.__getitem__(3) then begin
        // SyntaxError
        ExtractPyErrorInfo(Error, FileName, LineNo, Offset);
        FileName := FromPythonFileName(FileName);
        if GI_PyIDEServices.ShowFilePosition(FileName, LineNo, Offset) and
          Assigned(GI_ActiveEditor)
        then
          PyControl.ErrorPos :=
            TEditorPos.NPos(GI_ActiveEditor, LineNo, Offset, True);
      end else
        HandleRemoteException(ExcInfo);

      GI_PyInterpreter.AppendPrompt;
      System.SysUtils.Abort;
    end;
  except
    on E: EPythonError do begin  //may raise OverflowError or ValueError
      // New Line for output
      GI_PyInterpreter.AppendText(sLineBreak);
      VarClear(Result);

      HandlePyException(Py.PythonEngine.Traceback, E.Message);
      System.SysUtils.Abort;
    end;
    else begin
      VarClear(Result);
      raise;
    end;
  end;
end;

constructor TPyRemoteInterpreter.Create(AEngineType : TPythonEngineType = peRemote);
  function IsPyWinAvailable: Boolean;
  var
    _module : PPyObject;
  begin
    with GetPythonEngine do begin
      _module := PyImport_ImportModule('pywintypes');
      try
        if Assigned(_module) then
          Result := True
        else begin
          PyErr_Clear;
          Result := False;
        end;
      finally
        Py_XDecRef(_module);
      end;
    end;
  end;

Var
  SuppressOutput : IInterface;
  ServerSource: TStrings;
  ServerName: string;
begin
  fInterpreterCapabilities := [icReInitialize];
  fEngineType := AEngineType;
  DebuggerClass := TPyRemDebugger;

  Randomize;
  fSocketPort := 18000 + Random(1000);

  if (fEngineType = peRemote) and not PyIDEOptions.AlwaysUseSockets and IsPyWinAvailable then
  begin
    fUseNamedPipes := True;
    Inc(fSocketPort, 1000);
  end else
    fUseNamedPipes := False;

  // Import Rpyc
  SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors

  fOldSysModules := SysModule.modules.copy();
  try
    fRpycPath := Format('%sLib\%s', [ExtractFilePath(Application.ExeName), RpycZipModule]);
    PyControl.InternalInterpreter.SysPathAdd(fRpycPath);
    Rpyc := Import('rpyc');
    fServerIsAvailable := True;
  except
    StyledMessageDlg(_(SRpycNotAvailable), mtError, [mbAbort], 0);
    fServerIsAvailable := False;
  end;

  if fServerIsAvailable then begin
    fServerFile := TPyScripterSettings.UserDataPath + RemoteServerBaseName;
    case fEngineType of
      peRemote, peSSH: ServerName := 'SimpleServer';
      peRemoteTk: ServerName := 'TkServer';
      peRemoteWx: ServerName := 'WxServer';
    else
      raise Exception.Create('Invalid Engine type in TPyRemoteInterpreter constructor');
    end;
    ServerSource := GI_PyIDEServices.GetStoredScript(ServerName);
    if not SaveWideStringsToFile(fServerFile, ServerSource, False) then
    begin
      StyledMessageDlg(Format(_(SCouldNotWriteServerFile), [fServerFile]), mtError, [mbAbort], 0);
      fServerIsAvailable := False;
    end;
  end;

  ServerProcessOptions := TJclExecuteCmdProcessOptions.Create('');
  ServerProcessOptions.OutputBufferCallback := ProcessServerOutput;
  ServerProcessOptions.ErrorBufferCallback := ProcessServerOutput;
  ServerProcessOptions.BeforeResume := StoreServerProcessInfo;
  ServerProcessOptions.CreateProcessFlags :=
    ServerProcessOptions.CreateProcessFlags or CREATE_NO_WINDOW or CREATE_UNICODE_ENVIRONMENT;
  ServerProcessOptions.StartupVisibility := svNotSet;
  ServerProcessOptions.MergeError := False;
  ServerProcessOptions.RawOutput := True;
  ServerProcessOptions.RawError := True;

  if fServerIsAvailable then CreateAndConnectToServer;
end;

destructor TPyRemoteInterpreter.Destroy;
Var
  ModulesDict : Variant;
begin
  ShutDownServer;
  VarClear(Rpyc);
  if VarIsPython(fOldSysModules) and not VarIsNone(fOldSysModules) then begin
    ModulesDict := SysModule.modules;
    ModulesDict.clear();
    ModulesDict.update(fOldSysModules);
  end;
  VarClear(fOldSysModules);
  ServerProcessOptions.Free;
  PyControl.InternalInterpreter.SysPathRemove(fRpycPath);
  inherited;
end;

function TPyRemoteInterpreter.CreateDebugger: TPyBaseDebugger;
begin
  Result := DebuggerClass.Create(Self);
end;

function TPyRemoteInterpreter.EvalCode(const Expr: string): Variant;
begin
  // may raise exceptions
  if not fConnected then
    Result := None
  else
    Result := RPI.evalcode(Expr);
end;

function TPyRemoteInterpreter.GetGlobals: TBaseNameSpaceItem;
begin
  CheckConnected(True);
  Result := TRemNameSpaceItem.Create('globals',
    Evalcode('globals()'), Self);
end;

procedure TPyRemoteInterpreter.GetModulesOnPath(const Path: Variant; SL: TStrings);
Var
  i : integer;
  ModuleList : Variant;
begin
  ModuleList := RPI.getmodules(Path);
  for I := 0 to Len(ModuleList) - 1 do
    SL.Add(ModuleList.__getitem__(i));
end;

function TPyRemoteInterpreter.GetObjectType(Ob: Variant): string;
begin
  Result := PyControl.InternalInterpreter.GetObjectType(ob);
end;

procedure TPyRemoteInterpreter.HandleRemoteException(const ExcInfo: Variant; SkipFrames : integer = 1);

  procedure ExtractTracebackFronExcInfo(TraceBack: TPythonTraceback);
  var
    PyTraceback, CurrentTraceback, CodeObject : Variant;
    Context, FileName: string;
    LineNo: Integer;
  begin
    PyTraceback := ExcInfo.__getitem__(2);
    if not VarIsPython(PyTraceback) or VarIsNone(PyTraceback) then Exit;

    CurrentTraceback := PyTraceback;
    for var i  := 1 to SkipFrames do begin
      CurrentTraceback := CurrentTraceback.tb_next;
      if VarIsNone(CurrentTraceback) then break;
    end;
    while not VarIsNone(CurrentTraceback) do begin
      try
        LineNo := CurrentTraceback.tb_lineno;
      except
        LineNo := 0;
      end;
      CodeObject := CurrentTraceback.tb_frame.f_code;
      Context := CodeObject.co_name;
      FileName := CodeObject.co_filename;
      Traceback.AddItem(Context, Filename, LineNo);
      CurrentTraceback := CurrentTraceback.tb_next;
    end;
  end;

var
  Traceback: TPythonTraceback;
  ErrMsg: string;
begin
  CheckConnected;
  if not VarIsPython(ExcInfo) or VarIsNone(ExcInfo) then Exit;

  ErrMsg := Format('%s: %s',
            [VarPythonAsString(ExcInfo.__getitem__(0)),
             VarPythonAsString(RPI.safestr(ExcInfo.__getitem__(1)))]);
  Traceback := TPythonTraceback.Create;
  try
    ExtractTracebackFronExcInfo(Traceback);
    TThread.Synchronize(nil, procedure
    begin
      HandlePyException(Traceback, ErrMsg, 0);
    end)
  finally
    Traceback.Free;
  end;
end;

function TPyRemoteInterpreter.ImportModule(Editor: IEditor;
  AddToNameSpace : Boolean = False): Variant;
{
  Imports Editor text without saving the file.
  Does not add the module name to the locals()
  of the interpreter.
}
Var
  Code : Variant;
  Path, NameOfModule : string;
  PythonPathAdder : IInterface;
  RunConfiguration : TRunConfiguration;
begin
  var Py := GI_PyControl.SafePyEngine;
  Assert(Assigned(Editor));
  CheckConnected;
  VarClear(Result);
  //Compile
  RunConfiguration := TRunConfiguration.Create;
  try
    RunConfiguration.ScriptName := Editor.FileId;
    Code := Compile(RunConfiguration);
  finally
    RunConfiguration.Free;
  end;

  Assert(VarIsPython(Code));  // an exception should have been raised if failed

  // Add the path of the imported script to the Python path
  Path := ToPythonFileName(Editor.FileId);
  if Path.StartsWith('<') then
    Path := ''
  else
    Path := TPath.GetDirectoryName(Path);
  if Length(Path) > 1 then begin
    PythonPathAdder := AddPathToPythonPath(Path, False);
    SysPathRemove('');
  end;

  if Editor.FileName <> '' then
    NameOfModule := FileNameToModuleName(Editor.FileName)
  else
    NameOfModule := ChangeFileExt(Editor.FileTitle, '');

  PyControl.DebuggerState := dsRunning;
  try
    try
      Result := RPI.rem_import(NameOfModule, Code);
      Py.PythonEngine.CheckError;
      var ExcInfo: Variant := RPI.exc_info;
      if not VarIsNone(ExcInfo) then begin
        Result := None;
        HandleRemoteException(ExcInfo);
        System.SysUtils.Abort;
      end;
    except
      on E: EPythonError do begin
        HandlePyException(Py.PythonEngine.Traceback, E.Message);
        Result := None;
      end;
    end;
    VarClear(Code);
    if VarIsNone(Result) then begin
      StyledMessageDlg(_(SErrorInImportingModule), mtError, [mbOK], 0);
      System.SysUtils.Abort;
    end else if AddToNameSpace then
      RPI.locals.__setitem__(VarPythonCreate(NameOfModule), Result);
  finally
    //  Add again the empty path
    SysPathAdd('');

    PyControl.DebuggerState := dsInactive;
  end;
end;

function TPyRemoteInterpreter.NameSpaceFromExpression(const Expr: string): TBaseNameSpaceItem;
var
  LookupObj, ItemsDict: Variant;
  SuppressOutput : IInterface;
begin
  CheckConnected;
  SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
  try
    if Expr <> '' then begin
      //Evaluate the lookup expression
      LookupObj := RPI.evalcode(Expr);
      Result := TRemNameSpaceItem.Create(Expr, LookupObj, Self);
//      ItemsDict := RPI.safegetmembers(LookUpObj);
//      Result := TRemNameSpaceItem.Create(Expr, ItemsDict, Self);
    end else begin
//      BuiltinDict := RPI.ImmDict(Conn.modules.__builtin__.__dict__);
//      BuiltinDict.update(RPI.ImmDict(RPI.evalcode('vars()')));
      ItemsDict := RPI.evalcode('dict()');
//      ItemsDict.update(Conn.modules.__builtin__.__dict__);
      ItemsDict.update(Conn.modules.__getitem__(GetPythonEngine.BuiltInModuleName).__dict__);
      ItemsDict.update(RPI.evalcode('vars()'));
      Result := TRemNameSpaceItem.Create(Expr, ItemsDict, Self);
    end;
  except
    Result := nil;
    //Result := TRemNameSpaceItem.Create(Expr, None, Self);
  end;
end;

function TPyRemoteInterpreter.NameSpaceItemFromPyObject(aName: string;
  aPyObject: Variant): TBaseNameSpaceItem;
begin
  Result := TRemNameSpaceItem.Create(aName, aPyObject, Self);
end;

procedure TPyRemoteInterpreter.CreateAndConnectToServer;
begin
  try
    CreateAndRunServerProcess;  // Sets fServerIsAvailable to true
    if FServerIsAvailable then ConnectToServer;  // Sets fConnected to true
  except
    on E: Exception do
    begin
      fConnected := False;
      StyledMessageDlg(_(SErrorCreatingRemoteEngine) + E.Message, mtError, [mbAbort], 0);
    end;
  end;
  if not (fServerIsAvailable and fConnected) then
  begin
    StyledMessageDlg(_(SCouldNotConnectRemoteEngine), mtError, [mbAbort], 0);
    ShutDownServer;
  end;
end;

procedure TPyRemoteInterpreter.ProcessServerOutput(const Bytes: TBytes; BytesRead: Cardinal);
var
   S: string;
begin
   S := TEncoding.UTF8.GetString(Bytes, 0, BytesRead);
   GI_PyInterpreter.PythonIO.OnSendUniData(Self, S);
end;

procedure TPyRemoteInterpreter.CreateAndRunServerProcess;
begin
  fServerIsAvailable := False;

  // Repeat here to make sure it is set right
  MaskFPUExceptions(PyIDEOptions.MaskFPUExceptions);

  ServerProcessOptions.CommandLine := AddQuotesUnless(PrepareCommandLine('$[PythonExe]')) +
        Format(' -u -X utf8 "%s" %d "%s"', [fServerFile, fSocketPort, fRpycPath]);
  ServerTask := TTask.Create(procedure
    begin
      ExecuteCmdProcess(ServerProcessOptions);
    end).Start;
  if fUseNamedPipes then
    Sleep(500)
  else
    Sleep(100);
  fServerIsAvailable := ServerTask.Status = TTaskStatus.Running;
end;

procedure TPyRemoteInterpreter.ReInitialize;
begin
  var Py := GI_PyControl.SafePyEngine;
  case PyControl.DebuggerState of
    dsDebugging, dsRunning:
      begin
        GI_PyInterpreter.ShowOutput := False;
        ShutDownServer;  // sets fIsConnected to False
        Py.PythonEngine.PyErr_Clear;
        //  Running/Debugging will detect that fIsConnected is false and
        //  finish run/debug orderly and post a WM_REINITINTERPRETER message
        //  So we should not reinitialize here.
      end;
    dsInactive:
      begin
        GI_PyInterpreter.ShowOutput := True;
        ShutDownServer;
        CreateAndConnectToServer;
        if fServerIsAvailable and fConnected then begin
          GI_PyInterpreter.AppendText(sLineBreak + _(SRemoteInterpreterInit));
          GI_PyInterpreter.AppendPrompt;
          // Recreate the Active debugger
          PyControl.ActiveDebugger := DebuggerClass.Create(Self);

          // Add extra project paths
          if Assigned(ActiveProject) then
            ActiveProject.AppendExtraPaths;

          PyControl.DebuggerState := dsInactive;
        end;
      end;
    else
      // Should not happen.  Reinitialise is not enabled for other states
      Assert(False, Format(SInternalError, ['ShutdownServer']));
  end;
end;

function TPyRemoteInterpreter.GetInterpreter: Variant;
begin
  Result := RPI;
end;

procedure TPyRemoteInterpreter.RestoreCommandLine;
begin
  CheckConnected;
  var Py := GI_PyControl.SafePyEngine;
  Conn.modules.sys.argv := fOldargv;
end;

procedure TPyRemoteInterpreter.Run(ARunConfig: TRunConfiguration);
Var
  Py: IPyEngineAndGIL;
  Code: Variant;
  AsyncRun : Variant;
  AsyncResult : Variant;
  AsyncReady : boolean;
  Path, OldPath: string;
  PythonPathAdder: IInterface;
  ReturnFocusToEditor: Boolean;
  Timer : ITimer;
  [weak] Editor: IEditor;
begin
  CheckConnected;
  CanDoPostMortem := False;

  Py := GI_PyControl.SafePyEngine;

  //Compile
  Code := Compile(ARunConfig);
  if not VarIsPython(Code) then Exit;

  // Add the path of the script to the Python Path - Will be automatically removed
  Path := ToPythonFileName(ARunConfig.ScriptName);
  if Path.StartsWith('<') then
    Path := ''
  else
    Path := TPath.GetDirectoryName(Path);
  SysPathRemove('');
  if Path.Length > 1 then
    PythonPathAdder := AddPathToPythonPath(Path);

  // Set the Working directory
  if ARunConfig.WorkingDir <> '' then
    Path := GI_PyIDEServices.ReplaceParams(ARunConfig.WorkingDir);
  if Path.Length <= 1 then
    Path := SystemTempFolder;
  OldPath := RPI.rem_getcwdu();

  // Change the current path
  try
    RPI.rem_chdir(Path)
  except
    StyledMessageDlg(_(SCouldNotSetCurrentDir), mtWarning, [mbOK], 0);
  end;

  PyControl.DebuggerState := dsRunning;

  // New Line for output
  GI_PyInterpreter.AppendText(sLineBreak);

  // Set the command line parameters
  SetCommandLine(ARunConfig);

  Editor := GI_ActiveEditor;
  ReturnFocusToEditor := Assigned(Editor);
  GI_PyInterpreter.ShowWindow;

  AsyncRun := Rpyc.async_(RPI.run_nodebug);
  AsyncResult := AsyncRun.__call__(Code);
  GetPythonEngine.CheckError;
  Timer := NewTimer(50);
  Timer.Start(procedure
  begin
    // Do not reenter
    Timer.Stop;

    var Py := GI_PyControl.SafePyEngine;
    try
      if Connected then ServeConnection(500);
    except
      fConnected := False;
      GI_PyInterpreter.ClearPendingMessages;
    end;
    if Connected then
    try
      AsyncReady := AsyncResult._is_ready;
    except
      fConnected := False;
    end;
    if not Connected or AsyncReady then
    try
      try
        if Connected then begin
          var ExcInfo := RPI.exc_info;
          if not VarIsNone(ExcInfo) then begin
            HandleRemoteException(ExcInfo);
            ReturnFocusToEditor := False;
            CanDoPostMortem := True;
          end;
        end;
      except
        // CheckError already called by VarPyth
        on E: EPythonError do begin
          // should not happen
          CheckConnected(True, False);
          if Connected then HandlePyException(GetPythonEngine.Traceback,E.Message);
        end;
      end;
    finally
      VarClear(AsyncResult);
      GI_PyInterpreter.WritePendingMessages;
      GI_PyInterpreter.AppendPrompt;
      CheckConnected(True, False);

      if Connected then begin
        // Restore the command line parameters
        RestoreCommandLine;

        //  Add again the empty path
        SysPathAdd('');

        // Change the back current path
        RPI.rem_chdir(OldPath);
      end;
      PyControl.DebuggerState := dsInactive;
      if ReturnFocusToEditor and Assigned(Editor) then
        Editor.Activate;
      if not Connected then begin
        PythonPathAdder := nil;
        GI_PyInterpreter.ClearPendingMessages;
        GI_PyInterpreter.ReinitInterpreter;
      end else if CanDoPostMortem and PyIDEOptions.PostMortemOnException then
        PyControl.ActiveDebugger.EnterPostMortem;
      Timer := nil;
    end
    else
      Timer.Restart;
  end);
end;

function TPyRemoteInterpreter.RunSource(Const Source, FileName : Variant; symbol : string = 'single') : boolean;
var
  OldDebuggerState : TDebuggerState;
  OldPos : TEditorPos;
begin
  CheckConnected;
  Assert(not GI_PyControl.Running, 'RunSource called while the Python engine is active');
  OldDebuggerState := PyControl.DebuggerState;
  OldPos := PyControl.CurrentPos;

  PyControl.DebuggerState := dsRunning;
  var Py := GI_PyControl.SafePyEngine;
  try
    try
      Result := RPI.runsource(VarPythonCreate(Source), VarPythonCreate(FileName), VarPythonCreate(symbol));
    except
      Result := False;
    end;

    CheckConnected(True, False);
    if not Connected then
    begin
      Result := False;
      GI_PyInterpreter.ClearPendingMessages;
      GI_PyInterpreter.ReinitInterpreter;
    end;
  finally
    PyControl.DebuggerState := OldDebuggerState;
    if OldDebuggerState = dsPaused then
      PyControl.CurrentPos := OldPos;
  end;
end;

procedure TPyRemoteInterpreter.ConnectToServer;
var
  SuppressOutput : IInterface;
  I: Integer;
  Source : string;
  InitScriptName : string;
  PySource : Variant;
begin
  fConnected := False;

  SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors

  if fUseNamedPipes then
    try
      fNamedPipeStream := Rpyc.core.stream.NamedPipeStream.create_client(fSocketPort.ToString);
      Assert(VarIsPython(fNamedPipeStream));
      fUseNamedPipes := True;
    except
      with GetPythonEngine do if PyErr_Occurred <> nil then PyErr_Clear;
      fUseNamedPipes := False;
    end;

  // Try to connect a few times
  for i := 1 to 5 do begin
    try
      if fUseNamedPipes then
        Conn := Rpyc.classic.connect_stream(fNamedPipeStream)
      else
        Conn := Rpyc.classic.connect('localhost', fSocketPort);
      fConnected := True;
      break;
    except
      // wait and try again
      with GetPythonEngine do if PyErr_Occurred <> nil then PyErr_Clear;
      Sleep(100);
    end;
  end;

  if fConnected then begin
    Conn._config.__setitem__('sync_request_timeout', None);
//    // Redirect Output
//    OutputRedirector := Rpyc.classic.redirected_stdio(Conn);
//    OutputRedirector.__enter__();
/////////////////////////////////////////////////////
//    Conn.modules('sys').modules.__setitem__('__oldmain__', Conn.modules('sys').modules.__getitem__('__main__'));
//    Conn.modules('sys').modules.__setitem__('__main__', Conn.modules('imp').new_module('__main__'));
//    Conn.remote_conn._local_namespace := Conn.modules('sys').modules.__getitem__('__main__').__dict__;
//    Conn.namespace := Rpyc.Lib.AttrFrontend(Conn.remote_conn._local_namespace);
/////////////////////////////////////////////////////
//    Conn.namespace.__name__ := '__main__';
    // Create the remote interpreter
    InitScriptName := 'Rpyc_Init';

    Source := CleanEOLs(GI_PyIDEServices.GetStoredScript(InitScriptName).Text)+#10;
    PySource := VarPythonCreate(Source);
    Conn.execute(PySource);
    RPI := Conn.namespace.__getitem__('_RPI');
    //  pass a reference to the P4D module DebugIDE
    Conn.namespace.__delitem__('_RPI');

    // make sys.stdout etc asynchronous when writing
//    RPI.asyncIO();
//    // Install Rpyc excepthook gets automatically installed
//    Rpyc.install_rpyc_excepthook;

//    PyScripterMod := Import('pyscripter');
//    Conn.namespace.__setitem__('pyscripter', PyScripterMod);

    // sys.displayhook
    if PyIDEOptions.PrettyPrintOutput then
      RPI.setupdisplayhook();
    GetPythonEngine.CheckError;

    Initialize;
  end;
end;

procedure TPyRemoteInterpreter.ServeConnection(MaxCount : integer);
Var
  Count : integer;
begin
  CheckConnected(False, False);
  if not Connected then Exit;

  Count := 0;
  While Conn.poll() do begin
    Inc(Count);
    if (MaxCount > 0) and (Count >= MaxCount) then
      break;
  end;
end;

procedure TPyRemoteInterpreter.SetCommandLine(ARunConfig : TRunConfiguration);
var
  ArgV : Variant;
  S, Param : string;
  P : PChar;
begin
  CheckConnected;
  var Py := GI_PyControl.SafePyEngine;
  fOldargv := Conn.modules.sys.argv;
  Conn.execute('__import__("sys").argv = []');
  Argv := Conn.modules.sys.argv;
  // Workaround due to PREFER_UNICODE flag to make sure
  // no conversion to Unicode and back will take place
  S :=  ToPythonFileName(ARunConfig.ScriptName);
  Argv.append(VarPythonCreate(S));

  S := Trim(ARunConfig.Parameters);
  if S <> '' then begin
    S := GI_PyIDEServices.ReplaceParams(S);
    P := PChar(S);
    while P[0] <> #0 do begin
      P := GetParamStr(P, Param);
      Argv.append(VarPythonCreate(Param));
    end;
    GI_PyInterpreter.AppendText(Format(_(SCommandLineMsg), [S]));
  end;
end;

procedure TPyRemoteInterpreter.ShutDownServer;
var
  OldExceptHook : Variant;
begin
  if fServerIsAvailable and Assigned(ServerTask) and (ServerTask.Status = TTaskStatus.Running) then begin
    if Assigned(PyControl.ActiveDebugger) then
       PyControl.ActiveDebugger.Abort;

    if fConnected then begin
      try
        Conn.close();  // closes and shuts down SimpleServer
      except
      end;
    end;
    fConnected := False;
    try
      VarClear(fNamedPipeStream);
      VarClear(OutputRedirector);
    except
    end;
    try
      if not PyControl.Finalizing then
        GI_PyIDEServices.ClearPythonWindows;

      // Do not destroy Remote Debugger
      // PyControl.ActiveDebugger := nil;

      VarClear(fOldArgv);
      VarClear(RPI);
      VarClear(Conn);

      // Restore excepthook
      OldExceptHook := Varpyth.SysModule.__excepthook__;
      SysModule.excepthook := OldExceptHook;
    except
     // swallow exceptions
    end;
    Sleep(100);
    if ServerTask.Status = TTaskStatus.Running then
      TerminateProcessTree(ServerProcessInfo.dwProcessId);
  end;

  ServerTask := nil;
  fServerIsAvailable := False;
  fConnected := False;
end;

procedure TPyRemoteInterpreter.StoreServerProcessInfo(const ProcessInfo:
    TProcessInformation; InWritePipe: PHandle);
begin
  ServerProcessInfo := ProcessInfo;
end;

procedure TPyRemoteInterpreter.StringsToSysPath(Strings: TStrings);
var
  Py: IPyEngineAndGIL;
  i: Integer;
  PythonPath: Variant;
begin
  CheckConnected;
  Py := GI_PyControl.SafePyEngine;
  // could have used Conn.deliver
  Conn.execute('__import__("sys").path = []');
  PythonPath := Conn.modules.sys.path;
  for i := 0 to Strings.Count - 1 do
    PythonPath.append(Strings[i]);
end;

function TPyRemoteInterpreter.SysPathAdd(const Path: string): boolean;
var
  Py: IPyEngineAndGIL;
begin
  Result := False;
  CheckConnected(True, False);
  if not Connected then Exit;

  Py := GI_PyControl.SafePyEngine;
  if not Conn.modules.sys.path.__contains__(Path) then begin
    Conn.modules.sys.path.insert(0, Path);
    Result := true;
  end;
end;

function TPyRemoteInterpreter.SysPathRemove(const Path: string): boolean;
var
  Py: IPyEngineAndGIL;
begin
  Result := False;
  CheckConnected(True, False);
  if not Connected then Exit;

  Py := GI_PyControl.SafePyEngine;
  if Conn.modules.sys.path.__contains__(Path) then begin
    Result := True;
    Conn.modules.sys.path.remove(Path);
  end;
end;

procedure TPyRemoteInterpreter.SysPathToStrings(Strings: TStrings);
var
  Py: IPyEngineAndGIL;
  i: Integer;
  RemPath : Variant;
begin
  CheckConnected;

  Py := GI_PyControl.SafePyEngine;
  RemPath := Rpyc.classic.obtain(Conn.modules.sys.path);
  for i := 0 to Len(RemPath) - 1  do
    Strings.Add(RemPath.__getitem__(i));
end;

procedure TPyRemoteInterpreter.SystemCommand(const Cmd: string);
begin
  var Py := GI_PyControl.SafePyEngine;
  RPI.system_command(Cmd);
end;

function TPyRemoteInterpreter.UnitTestResult: Variant;
begin
  Result := RPI.IDETestResult();
end;

{ TPyRemDebugger }

procedure TPyRemDebugger.Abort;
begin
  case PyControl.DebuggerState of
    dsPostMortem: ExitPostMortem;
    dsDebugging,
    dsRunning: RaiseKeyboardInterrupt(fRemotePython.ServerProcessInfo.dwProcessId);
    dsPaused:
      begin
         GI_PyInterpreter.RemovePrompt;
        fDebuggerCommand := dcAbort;
        DoDebuggerCommand;
      end;
  end;
end;

constructor TPyRemDebugger.Create(RemotePython: TPyRemoteInterpreter);
begin
  inherited Create;
  fRemotePython := RemotePython;
  fDebugManager := fRemotePython.RPI.DebugManager;
  fDebugManager.debugIDE :=
    PyControl.InternalInterpreter.PyInteractiveInterpreter.debugIDE;

  fMainDebugger := fDebugManager.main_debugger;

  fMainThread := TThreadInfo.Create;
  fMainThread.Name := 'MainThread';
  fMainThread.Status := thrdRunning;
  fMainThread.Thread_ID := fDebugManager.main_thread_id;

  fThreads := TObjectDictionary<Int64, TThreadInfo>.Create([doOwnsValues]);
  fThreads.Add(fMainThread.Thread_ID, fMainThread);

  fLineCache := fRemotePython.Conn.modules.linecache;
  fDebuggerCommand := dcNone;
  PyControl.BreakPointsChanged := True;
end;

destructor TPyRemDebugger.Destroy;
begin
  with PyControl.InternalPython.DebugIDE.Events do begin
    Items[dbie_user_call].OnExecute := nil;
    Items[dbie_user_line].OnExecute := nil;
    Items[dbie_user_thread].OnExecute := nil;
    Items[dbie_user_exception].OnExecute := nil;
    Items[dbie_user_yield].OnExecute := nil;
  end;
  FreeAndNil(fThreads);
  inherited;
end;

procedure TPyRemDebugger.EnterPostMortem;
Var
  Py: IPyEngineAndGIL;
  Frame, BotFrame, TraceBack : Variant;
begin
  Py := GI_PyControl.SafePyEngine;
  if not (HaveTraceback and (PyControl.DebuggerState = dsInactive)) then
    Exit;

  GI_PyInterpreter.SetPyInterpreterPrompt(pipPostMortem);
  GI_PyInterpreter.AppendPrompt;

  TraceBack := fRemotePython.Conn.modules.sys.last_traceback;
  Botframe := TraceBack.tb_frame;
  while not VarIsNone(TraceBack.tb_next) do
    TraceBack := TraceBack.tb_next;
  Frame := TraceBack.tb_frame;

  PyControl.DebuggerState := dsPostMortem;
  fMainThread.Status := thrdBroken;
  GetCallStack(fMainThread.CallStack, Frame, Botframe);
  TPyBaseDebugger.ThreadChangeNotify(fMainThread, tctAdded );

  DSAMessageDlg(dsaPostMortemInfo, 'PyScripter', _(SPostMortemInfo),
     mtInformation, [mbOK], 0, dckActiveForm, 0, mbOK);
end;

function TPyRemDebugger.Evaluate(const Expr: string): TBaseNamespaceItem;
Var
  Py: IPyEngineAndGIL;
  SuppressOutput : IInterface;
  V : Variant;
begin
  Result := nil;
  if PyControl.DebuggerState in [dsPaused, dsPostMortem] then begin
    SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
    try
      Py := GI_PyControl.SafePyEngine;
      // evalcode knows we are in the debugger and uses current frame locals/globals
      V := fRemotePython.RPI.evalcode(Expr);
      Result := TNameSpaceItem.Create(Expr, V);
    except
      // fail quitely
    end;
  end;
end;

procedure TPyRemDebugger.Evaluate(const Expr: string; out ObjType,
  Value: string);
Var
  Py: IPyEngineAndGIL;
  SuppressOutput : IInterface;
  V : Variant;
begin
  ObjType := _(SNotAvailable);
  Value := _(SNotAvailable);
  if PyControl.DebuggerState in [dsPaused, dsPostMortem] then begin
    SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
    try
      Py := GI_PyControl.SafePyEngine;
      // evalcode knows we are in the debugger and uses current frame locals/globals
      V := fRemotePython.RPI.evalcode(Expr);
      ObjType := fRemotePython.RPI.objecttype(V);
      Value := fRemotePython.RPI.saferepr(V);
    except
      // fail quietly
    end;
  end;
end;

procedure TPyRemDebugger.ExitPostMortem;
begin
  GI_PyInterpreter.SetPyInterpreterPrompt(pipNormal);
  GI_PyInterpreter.AppendPrompt;

  var Py := GI_PyControl.SafePyEngine;
  fMainThread.Status := thrdRunning;
  fMainThread.CallStack.Clear;
  TPyBaseDebugger.ThreadChangeNotify(fMainThread, tctStatusChange);
  MakeFrameActive(nil);
  PyControl.DebuggerState := dsInactive;
end;

procedure TPyRemDebugger.GetCallStack(CallStackList : TObjectList<TBaseFrameInfo>;
  Frame, Botframe : Variant);
begin
  CallStackList.Clear;
  if VarIsPython(Frame) then begin
    while not VarIsNone(Frame.f_back) and not VarIsNone(Frame.f_back.f_back) do begin
      CallStackList.Add(TFrameInfo.Create(Frame));
      Frame := Frame.f_back;
      if VarIsSame(Frame, Botframe) then break;
    end;
    // at least one frame
    if CallStackList.Count = 0  then
      CallStackList.Add(TFrameInfo.Create(Frame));
  end;
end;

function TPyRemDebugger.GetFrameGlobals(Frame: TBaseFrameInfo): TBaseNameSpaceItem;
begin
  Result := nil;
  if not (PyControl.DebuggerState in [dsPaused, dsPostMortem]) then
    Exit;
  Result := TRemNameSpaceItem.Create('globals',
    (Frame as TFrameInfo).PyFrame.f_globals, fRemotePython);
end;

function TPyRemDebugger.GetFrameLocals(Frame: TBaseFrameInfo): TBaseNameSpaceItem;
begin
  Result := nil;
  if not (PyControl.DebuggerState in [dsPaused, dsPostMortem]) then
    Exit;
  Result := TRemNameSpaceItem.Create('locals',
    (Frame as TFrameInfo).PyFrame.f_locals, fRemotePython);
end;

function TPyRemDebugger.GetPostMortemEnabled: boolean;
begin
  Result := fRemotePython.CanDoPostMortem;
end;

procedure TPyRemDebugger.DoDebuggerCommand;
begin
   fRemotePython.CheckConnected;
   if (PyControl.DebuggerState <> dsPaused) or not fRemotePython.Connected then
     Exit;

   if PyControl.BreakPointsChanged then SetDebuggerBreakpoints;

//   case fDebuggerCommand of
//     dcRun         : fRemotePython.Debugger.set_continue();
//     dcStepInto    : fRemotePython.Debugger.set_step();
//     dcStepOver    : fRemotePython.Debugger.set_next(fCurrentFrame);
//     dcStepOut     : fRemotePython.Debugger.set_return(fCurrentFrame);
//     dcRunToCursor : fRemotePython.Debugger.set_continue;
//     dcPause       : fRemotePython.Debugger.set_step();
//     dcAbort       : fRemotePython.Debugger.set_quit();
//   end;
   var Py := GI_PyControl.SafePyEngine;
   fDebugManager.debug_command := Ord(fDebuggerCommand);
   fDebuggerCommand := dcNone;
end;

function TPyRemDebugger.HaveTraceback: boolean;
begin
  Result := False;
  with fRemotePython do
    if not (IsAvailable and Connected) then
      Exit;
  try
    var Py := GI_PyControl.SafePyEngine;
    Result := fRemotePython.Conn.eval('hasattr(__import__("sys"), "last_traceback")');
  except
  end;
end;

procedure TPyRemDebugger.LoadLineCache;
begin
  // inject unsaved code into LineCache
  fLineCache.cache.clear();
  GI_EditorFactory.ApplyToEditors(procedure(Editor: IEditor)
  var
    FName, Source, LineList: Variant;
    SFName: string;
  begin
    with Editor do begin
      if not HasPythonFile then Exit;
      SFName := fRemotePython.ToPythonFileName(FileId);
      if SFName.StartsWith('<') and
        // so that we do not push to ssh engine linecache all local open files
        (PyControl.RunConfig.ScriptName = FileId)
      then
      begin
        FName := SFName;
        Source := CleanEOLs(SynEdit.Text)+WideLF;
        LineList := VarPythonCreate(Source);
        LineList := fRemotePython.Rpyc.classic.deliver(fRemotePython.Conn, LineList.splitlines(True));
        fLineCache.cache.__setitem__(VarPythonCreate(FName),
          VarPythonCreate([0, None, LineList, FName], stTuple));
      end;
    end;
  end);
end;

procedure TPyRemDebugger.MakeFrameActive(Frame: TBaseFrameInfo);
begin
  var Py := GI_PyControl.SafePyEngine;
  if Assigned(Frame) then
    fDebugManager.active_frame := (Frame as TFrameInfo).PyFrame
  else
    fDebugManager.active_frame := None;
end;

procedure TPyRemDebugger.MakeThreadActive(Thread: TThreadInfo);
begin
  var Py := GI_PyControl.SafePyEngine;
  if Assigned(Thread) then
    fDebugManager.active_thread := Thread.Thread_ID
  else
    fDebugManager.active_thread := None;
end;

function TPyRemDebugger.NameSpaceFromExpression(const Expr: string): TBaseNameSpaceItem;
//  The internal interpreter knows whether we are debugging and
//  adjusts accordingly (in II.evalcode)
begin
  Result := fRemotePython.NameSpaceFromExpression(Expr);
end;

procedure TPyRemDebugger.Pause;
begin
  fDebuggerCommand := dcPause;
  DoDebuggerCommand;
end;

procedure TPyRemDebugger.RestoreCommandLine;
begin
  fRemotePython.RestoreCommandLine;
end;

procedure TPyRemDebugger.Resume;
begin
  GI_PyInterpreter.RemovePrompt;
  fDebuggerCommand := dcRun;
  DoDebuggerCommand;
end;

procedure TPyRemDebugger.Debug(ARunConfig: TRunConfiguration;
  InitStepIn: Boolean = False; RunToCursorLine : integer = -1);
var
  Py: IPyEngineAndGIL;
  Code : Variant;
  AsyncRun : Variant;
  AsyncResult : Variant;
  AsyncReady : boolean;
  Path, OldPath : string;
  PythonPathAdder : IInterface;
  ReturnFocusToEditor: Boolean;
  Timer : ITimer;
  [weak] Editor : IEditor;
begin
  fRemotePython.CheckConnected;
  fRemotePython.CanDoPostMortem := False;
  fDebuggerCommand := dcRun;
  Assert(PyControl.DebuggerState = dsInactive);

  Py := GI_PyControl.SafePyEngine;
  //Compile
  Code := fRemotePython.Compile(ARunConfig);
  if not VarIsPython(Code) then Exit;

  // Add the path of the script to the Python Path - Will be automatically removed
  Path := fRemotePython.ToPythonFileName(ARunConfig.ScriptName);
  if Path.StartsWith('<') then
    Path := ''
  else
    Path := TPath.GetDirectoryName(Path);
  fRemotePython.SysPathRemove('');
  if Path.Length > 1 then
    PythonPathAdder := fRemotePython.AddPathToPythonPath(Path);

  // Set the Working directory
  if ARunConfig.WorkingDir <> '' then
    Path := GI_PyIDEServices.ReplaceParams(ARunConfig.WorkingDir);
  if Path.Length <= 1 then
    Path := fRemotePython.SystemTempFolder;
  OldPath := fRemotePython.RPI.rem_getcwdu();

  // Change the current path
  try
    fRemotePython.RPI.rem_chdir(Path)
  except
    StyledMessageDlg(_(SCouldNotSetCurrentDir), mtWarning, [mbOK], 0);
  end;

  PyControl.DebuggerState := dsDebugging;
  TPyBaseDebugger.ThreadChangeNotify(fMainThread, tctAdded );


  GI_PyIDEServices.Messages.ClearMessages;
  Editor := GI_ActiveEditor;
  ReturnFocusToEditor := Assigned(Editor);
  // Set the layout to the Debug layout is it exists
  if GI_PyIDEServices.Layouts.LayoutExists('Debug') then begin
    GI_PyIDEServices.Layouts.SaveLayout('Current');
    GI_PyIDEServices.Layouts.LoadLayout('Debug');
    Application.ProcessMessages;
  end else
    GI_PyInterpreter.ShowWindow;

  GI_PyInterpreter.SetPyInterpreterPrompt(pipDebug);
  // Better without.  A prompt will be added when the debugger breaks
  //GI_PyInterpreter.AppendPrompt;
  //attach debugger callback routines
  with PyControl.InternalPython.DebugIDE.Events do begin
    Items[dbie_user_call].OnExecute := UserCall;
    Items[dbie_user_line].OnExecute := UserLine;
    Items[dbie_user_thread].OnExecute := UserThread;
    Items[dbie_user_exception].OnExecute := UserException;
    Items[dbie_user_yield].OnExecute := UserYield;
  end;

  //set breakpoints
  SetDebuggerBreakPoints;
  if RunToCursorLine >= 0 then  // add temp breakpoint
    fMainDebugger.set_break(Code.co_filename, RunToCursorLine, 1);

  // New Line for output
  GI_PyInterpreter.AppendText(sLineBreak);

  // Set the command line parameters
  SetCommandLine(ARunConfig);

  fMainDebugger.InitStepIn := InitStepIn;

  AsyncRun := fRemotePython.Rpyc.async_(fMainDebugger.run);
  AsyncResult := AsyncRun.__call__(Code);
  GetPythonEngine.CheckError;

  Timer := NewTimer(50);
  Timer.Start(procedure
  begin
    // Do not reenter
    Timer.Stop;

    // Check whether we are executing statements while broken
    if fExecPaused then
    begin
      // Wait for the execution to finish
      Timer.Restart;
      Exit;
    end;

    var Py := GI_PyControl.SafePyEngine;
    try
      if fRemotePython.Connected then fRemotePython.ServeConnection(500);
    except
      fRemotePython.fConnected := False;
      GI_PyInterpreter.ClearPendingMessages;
    end;
    if fRemotePython.Connected then
    try
      AsyncReady := AsyncResult._is_ready;
    except
      fRemotePython.fConnected := False;
    end;
    if not fRemotePython.Connected or AsyncReady then
    try
      try
        if fRemotePython.Connected then begin
          var ExcInfo: Variant := fMainDebugger.exc_info;
          if not VarIsNone(ExcInfo) then begin
            fRemotePython.HandleRemoteException(ExcInfo, 2);
            ReturnFocusToEditor := False;
            fRemotePython.CanDoPostMortem := True;
          end;
        end;
      except
        // CheckError already called by VarPyth
        on E: EPythonError do begin
          // should not happen
          fRemotePython.CheckConnected(True, False);
          if fRemotePython.Connected then
              fRemotePython.HandlePyException(GetPythonEngine.Traceback, E.Message);
        end;
      end;
    finally
      VarClear(AsyncResult);
      GI_PyInterpreter.WritePendingMessages;
      GI_PyInterpreter.SetPyInterpreterPrompt(pipNormal);
      GI_PyInterpreter.AppendPrompt;

      fRemotePython.CheckConnected(True, False);
      if fRemotePython.Connected then begin
        MakeFrameActive(nil);

        // Restore the command line parameters
        RestoreCommandLine;

        //  Add again the empty path
        fRemotePython.SysPathAdd('');

        // Change the back current path
        fRemotePython.RPI.rem_chdir(OldPath);
      end;

      if GI_PyIDEServices.Layouts.LayoutExists('Debug') then
        GI_PyIDEServices.Layouts.LoadLayout('Current');

      PyControl.DebuggerState := dsInactive;
      if ReturnFocusToEditor and Assigned(Editor) then
        Editor.Activate;
      if not fRemotePython.Connected then begin
        PythonPathAdder := nil;
        GI_PyInterpreter.ClearPendingMessages;
        //fRemotePython.ReInitialize;
        // Reinitialize destroys the debugger which executes this method!
        // So handle with a PostMessage
        GI_PyInterpreter.ReinitInterpreter;
      end else if fRemotePython.CanDoPostMortem and PyIDEOptions.PostMortemOnException then
        PyControl.ActiveDebugger.EnterPostMortem;
      Timer := nil;
    end
    else
      Timer.Restart;
  end);
end;

function TPyRemDebugger.RunSource(Const Source, FileName : Variant; symbol : string = 'single') : boolean;
// The interpreter RunSource calls II.runsource which differs
// according to whether we are debugging or not
Var
  OldCurrentPos : TEditorPos;
begin
  if not (PyControl.DebuggerState in [dsPaused, dsPostMortem]) then
    Exit(False);

  OldCurrentPos := PyControl.CurrentPos;
  fExecPaused := True;
  try
    Result := fRemotePython.RunSource(Source, FileName, symbol);
  finally
    fExecPaused := False;
    PyControl.CurrentPos := OldCurrentPos;
  end;
end;

procedure TPyRemDebugger.RunToCursor(Editor: IEditor; ALine: integer);
Var
  FName : string;
begin
  Assert(PyControl.DebuggerState = dsPaused);
  GI_PyInterpreter.RemovePrompt;
  // Set Temporary breakpoint
  SetDebuggerBreakPoints;  // So that this one is not cleared
  FName := fRemotePython.ToPythonFileName(Editor.FileId);
  var Py := GI_PyControl.SafePyEngine;
  fMainDebugger.set_break(VarPythonCreate(FName), ALine, 1);

  fDebuggerCommand := dcRunToCursor;
  DoDebuggerCommand;
end;

procedure TPyRemDebugger.SetCommandLine(ARunConfig : TRunConfiguration);
begin
  fRemotePython.SetCommandLine(ARunConfig);
end;

procedure TPyRemDebugger.SetDebuggerBreakpoints;
begin
  if not PyControl.BreakPointsChanged then Exit;

  var Py := GI_PyControl.SafePyEngine;
  LoadLineCache;
  fMainDebugger.clear_all_breaks();

  GI_EditorFactory.ApplyToEditors(procedure(Editor: IEditor)
  var
    FName : string;
  begin
    FName := fRemotePython.ToPythonFileName(Editor.FileId);
    for var I := 0 to Editor.BreakPoints.Count - 1 do begin
      var BreakPoint := TBreakPoint(Editor.BreakPoints[I]);
      if not BreakPoint.Disabled then begin
        if BreakPoint.Condition <> '' then begin
          fMainDebugger.set_break(VarPythonCreate(FName), BreakPoint.LineNo,
            0, VarPythonCreate(BreakPoint.Condition));
        end else
          fMainDebugger.set_break(VarPythonCreate(FName),
            BreakPoint.LineNo);
      end;
    end;
  end);

  PyControl.BreakPointsChanged := False;
end;

procedure TPyRemDebugger.StepInto;
begin
  GI_PyInterpreter.RemovePrompt;
  fDebuggerCommand := dcStepInto;
  DoDebuggerCommand;
end;

procedure TPyRemDebugger.StepOut;
begin
  GI_PyInterpreter.RemovePrompt;
  fDebuggerCommand := dcStepOut;
  DoDebuggerCommand;
end;

procedure TPyRemDebugger.StepOver;
begin
  GI_PyInterpreter.RemovePrompt;
  fDebuggerCommand := dcStepOver;
  DoDebuggerCommand;
end;

procedure TPyRemDebugger.UserCall(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
// Not used
begin
   var Py := GI_PyControl.SafePyEngine;
   Result := Py.PythonEngine.ReturnNone;
end;

procedure TPyRemDebugger.UserException(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
// Not used
begin
   var Py := GI_PyControl.SafePyEngine;
   Result := Py.PythonEngine.ReturnNone;
end;

procedure TPyRemDebugger.UserLine(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
Var
  Py: IPyEngineAndGIL;
  FName : string;
  Frame : Variant;
  LineNumber : integer;
  Thread_ID : Int64;
  ThreadInfo : TThreadInfo;
  Arguments : Variant;
  BotFrame : Variant;
begin
  Py := GI_PyControl.SafePyEngine;

  fRemotePython.CheckConnected;

  Arguments := VarPythonCreate(Args);
  Thread_ID := Arguments.__getitem__(0);
  // Do not stop while executing code in paused state
  if fExecPaused or not fThreads.TryGetValue(Thread_ID, ThreadInfo) then
  begin
    Result := Py.PythonEngine.ReturnFalse;
    Exit;
  end;

  Frame := Arguments.__getitem__(1);
  BotFrame := Arguments.__getitem__(2);
  Assert(VarIsPython(Frame) and not VarisNone(Frame));
  FName := fRemotePython.FromPythonFileName(Frame.f_code.co_filename);
  LineNumber := Frame.f_lineno;

  if (LineNumber > 0) and
     (not PyIDEOptions.TraceOnlyIntoOpenFiles or
        Assigned(GI_EditorFactory.GetEditorByFileId(FName))) and
     GI_PyIDEServices.ShowFilePosition(FName, LineNumber, 1, 0, True, False)
  then
  begin
    if PyControl.DebuggerState = dsDebugging then
    begin
      PyControl.DebuggerState := dsPaused;
      GI_PyInterpreter.AppendPrompt;
    end;

    ThreadInfo.Status := thrdBroken;
    GetCallStack(ThreadInfo.CallStack, Frame, BotFrame);
    TPyBaseDebugger.ThreadChangeNotify(ThreadInfo, tctStatusChange);

    FDebuggerCommand := dcNone;

    // Return true if broken
    Result := Py.PythonEngine.ReturnTrue;
  end
  else
    Result := Py.PythonEngine.ReturnFalse;
end;

procedure TPyRemDebugger.SyncThreadInfo(Thread_ID: Int64; ThreadName: string; ThreadStatus : integer);

  function HaveBrokenThread: Boolean;
  Var
    T : TThreadInfo;
  begin
    Result := False;
    for T in fThreads.Values do
      if T.Status = thrdBroken then
      begin
        Result := True;
        break;
      end;
  end;

var
  ThreadInfo : TThreadInfo;
  OldStatus : TThreadStatus;
begin
  if TThreadStatus(ThreadStatus) = thrdFinished then begin
    if fThreads.ContainsKey(Thread_ID) then begin
      TPyBaseDebugger.ThreadChangeNotify(fThreads[Thread_ID], tctRemoved);
      fThreads.Remove(Thread_ID);
    end;
  end else begin
    if fThreads.TryGetValue(Thread_ID, ThreadInfo) then begin
      OldStatus := ThreadInfo.Status;
      ThreadInfo.Status := TThreadStatus(ThreadStatus);
      if OldStatus <> ThreadInfo.Status then
      begin
        TPyBaseDebugger.ThreadChangeNotify(fThreads[Thread_ID], tctStatusChange);
        if OldStatus = thrdBroken then
        begin
          ThreadInfo.CallStack.Clear;
          if not HaveBrokenThread then
            PyControl.DebuggerState := dsDebugging;
        end;
      end;
    end else begin
      ThreadInfo := TThreadInfo.Create;
      ThreadInfo.Thread_ID := Thread_ID;
      ThreadInfo.Name := ThreadName;
      ThreadInfo.Status :=  TThreadStatus(ThreadStatus);
      fThreads.Add(Thread_ID, ThreadInfo);
      TPyBaseDebugger.ThreadChangeNotify(fThreads[Thread_ID], tctAdded);
    end;
  end;
end;

procedure TPyRemDebugger.UserThread(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
var
  Py: IPyEngineAndGIL;
  Thread_ID: Int64;
  ThreadName: string;
  ThreadStatus : integer;
begin
  Py := GI_PyControl.SafePyEngine;
  Result := Py.PythonEngine.ReturnNone;

  Thread_ID := Py.PythonEngine.GetSequenceItem(Args, 0);
  ThreadName := Py.PythonEngine.GetSequenceItem(Args, 1);
  ThreadStatus := Py.PythonEngine.GetSequenceItem(Args, 2);

  // Make sure SyncThreadInfo runs in the main thread when fExecPaused if False
  TThread.Queue(nil, procedure
  begin
    while fExecPaused do
      // can't think of a better way to wait
      Application.ProcessMessages;
    SyncThreadInfo(Thread_ID, ThreadName, ThreadStatus);
  end);
end;

procedure TPyRemDebugger.UserYield(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
begin
  var Py := GI_PyControl.SafePyEngine;
  Result := GetPythonEngine.PyLong_FromLong(Ord(fDebuggerCommand));
end;

end.
