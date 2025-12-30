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
  Winapi.Windows,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Threading,
  PascalProcess,
  PythonEngine,
  cPySupportTypes,
  cPyBaseDebugger,
  cPyDebugger;

type
  TRemoteDebuggerClass = class of TPyRemDebugger;

  { Rpyc-based remote Python Interpreter  }
  TPyRemoteInterpreter = class(TPyBaseInterpreter)
  private
    FUseNamedPipes: Boolean;
    FNamedPipeStream: Variant;
    procedure CreateAndConnectToServer;
  protected
    const RemoteServerBaseName = 'remserver.py';
    const RpycZipModule = 'rpyc.zip';
  protected
    Rpyc: Variant;
    Conn: Variant;
    RPI: Variant; // RemotePythonInterpreter
    FServerIsAvailable: Boolean;
    FOldargv: Variant;
    FOldsysmodules: Variant;
    FConnected: Boolean;
    FSocketPort: Integer;
    FServerFile: string;
    FRpycPath: string;
    ServerProcess: IPProcess;
    DebuggerClass: TRemoteDebuggerClass;
    FStoredServerOutput: TBytes;
    procedure CreateAndRunServerProcess; virtual;
    procedure ConnectToServer;
    procedure ShutDownServer;  virtual;
    procedure ProcessServerOutput(Sender: TObject; const Bytes: TBytes; BytesRead:
        Cardinal);
    function GetInterpreter: Variant; override;
  public
    constructor Create(AEngineType: TPythonEngineType = peRemote);
    destructor Destroy; override;
    function CreateDebugger: TPyBaseDebugger; override;
    function Compile(ARunConfig: TRunConfiguration): Variant;
    procedure HandleRemoteException(const ExcInfo: Variant; SkipFrames: Integer = 0);
    procedure ReInitialize; override;
    procedure CheckConnected(Quiet: Boolean = False; Abort: Boolean = True);
    procedure ServeConnection(MaxCount: Integer = 0);
    // Python Path
    function SysPathAdd(const Path: string): Boolean; override;
    function SysPathRemove(const Path: string): Boolean; override;
    procedure SysPathToStrings(Strings: TStrings); override;
    procedure StringsToSysPath(Strings: TStrings); override;
    // NameSpace
    function GetGlobals: TBaseNameSpaceItem; override;
    function NameSpaceFromExpression(const Expr: string): TBaseNameSpaceItem; override;
    function CallTipFromExpression(const Expr: string;
      var DisplayString, DocString: string): Boolean; override;
    // Service routines
    procedure SetCommandLine(ARunConfig: TRunConfiguration); override;
    procedure RestoreCommandLine; override;
    // Main interface
    function ImportModule(const FileId: string; AddToNameSpace: Boolean = False): Variant; override;
    procedure Run(ARunConfig: TRunConfiguration); override;
    function RunSource(const Source, FileName: string; const Symbol: string = 'single'): Boolean; override;
    function EvalCode(const Expr: string): Variant; override;
    procedure SystemCommand(const Cmd: string); override;
    function GetObjectType(Obj: Variant): string; override;
    function UnitTestResult: Variant; override;
    function NameSpaceItemFromPyObject(AName: string; APyObject: Variant): TBaseNameSpaceItem; override;
    procedure Pickle(AValue: Variant; const FileName: string); override;
    class procedure ExtractPyErrorInfo(E: Variant; var FileName: string;
      var LineNo: Integer; var Offset: Integer); static;

    property IsAvailable: Boolean read FServerIsAvailable;
    property Connected: Boolean read FConnected;
  end;


  TRemNameSpaceItem = class(TNameSpaceItem)
  { Implementation of the Base class for the remote debugger }
  private
    FRemotePython: TPyRemoteInterpreter;
    FIsProxy: Boolean;
  protected
    procedure FillObjectInfo; override;
    function GetObjectType: string; override;
    function GetValue: string; override;
    function GetDocString: string; override;
    function GetChildCount: Integer; override;
  public
    constructor Create(AName: string; const APyObject: Variant;
      RemotePython: TPyRemoteInterpreter);
    procedure GetChildNodes; override;
    property IsProxy: Boolean read FIsProxy;
  end;

  TPyRemDebugger = class(TPyBaseDebugger)
  { Remote debugger using Rpyc for communication with the server }
  private
    FExecPaused: Boolean;
    FDebugManager: Variant;
    FMainDebugger: Variant;
    FLineCache: Variant;
    FMainThread: TThreadInfo;
    FThreads: TObjectDictionary<Int64, TThreadInfo>;
    procedure SyncThreadInfo(Thread_ID: Int64; ThreadName: string; ThreadStatus: Integer);
  protected
    FDebuggerCommand: TDebuggerCommand;
    FRemotePython: TPyRemoteInterpreter;
    procedure SetCommandLine(ARunConfig: TRunConfiguration); override;
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
    procedure GetCallStack(CallStackList: TObjectList<TBaseFrameInfo>;
      Frame, Botframe: Variant);
    function GetPostMortemEnabled: Boolean; override;
  public
    constructor Create(RemotePython: TPyRemoteInterpreter);
    destructor Destroy; override;

    // Debugging
    procedure Debug(ARunConfig: TRunConfiguration; InitStepIn: Boolean = False;
      RunToCursorLine: Integer = -1); override;
    procedure RunToCursor(const FileId: string; ALine: Integer); override;
    procedure StepInto; override;
    procedure StepOver; override;
    procedure StepOut; override;
    procedure Resume; override;
    procedure Pause; override;
    procedure Abort; override;
    // Evaluate expression in the current frame
    procedure Evaluate(const Expr: string; out ObjType, Value: string); overload; override;
    function Evaluate(const Expr: string): TBaseNameSpaceItem; overload; override;
    // Like the InteractiveInterpreter runsource but for the debugger frame
    function RunSource(const Source, FileName: string; const Symbol: string = 'single'): Boolean; override;
    // functions to get TBaseNamespaceItems corresponding to a frame's gloabals and locals
    function GetFrameGlobals(Frame: TBaseFrameInfo): TBaseNameSpaceItem; override;
    function GetFrameLocals(Frame: TBaseFrameInfo): TBaseNameSpaceItem; override;
    function NameSpaceFromExpression(const Expr: string): TBaseNameSpaceItem; override;
    procedure MakeThreadActive(Thread: TThreadInfo); override;
    procedure MakeFrameActive(Frame: TBaseFrameInfo); override;
    // post mortem stuff
    function HaveTraceback: Boolean; override;
    procedure EnterPostMortem; override;
    procedure ExitPostMortem; override;
  end;

implementation

uses
  System.Contnrs,
  System.IOUtils,
  Vcl.Forms,
  Vcl.Dialogs,
  VarPyth,
  JclSysInfo,
  JvDSADialogs,
  JvGnugettext,
  StringResources,
  uEditAppIntfs,
  uPythonItfs,
  cPyScripterSettings,
  cPyControl,
  cTools,
  uCommonFunctions,
  cAppPaths;

{ TRemNameSpaceItem }
constructor TRemNameSpaceItem.Create(AName: string; const APyObject: Variant;
  RemotePython: TPyRemoteInterpreter);
begin
  inherited Create(AName, APyObject);
  FRemotePython := RemotePython;
  FIsProxy := VarIsInstanceOf(FPyObject,  FRemotePython.Rpyc.BaseNetref);
end;

function TRemNameSpaceItem.GetChildCount: Integer;
begin
  if Assigned(FChildNodes) then
    Result := FChildNodes.Count
  else if FChildCount >= 0 then
    Result := FChildCount
  else begin
    if not IsProxy then
      Result := inherited GetChildCount
    else begin
      FRemotePython.CheckConnected;
      var SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
      try
        Result := FRemotePython.RPI.membercount(fPyObject, ExpandSequences, ExpandCommonTypes, ExpandSequences);
      except
        Result := 0;
      end;
    end;
    FChildCount := Result;
  end;
end;

procedure TRemNameSpaceItem.GetChildNodes;
var
  FullInfoTuple, APyObject: Variant;
  PyFullInfoTuple, PyMemberInfo, PyFullInfo: PPyObject;
  ObjName: string;
  NameSpaceItem: TRemNameSpaceItem;
begin
  if not (Assigned(FChildNodes) or FGotChildNodes) then begin
    FRemotePython.CheckConnected;
    FGotChildNodes := True;  //Do not try again
    var SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
    try
      if IsProxy then
        FullInfoTuple := FRemotePython.RPI.safegetmembersfullinfo(fPyObject, ExpandSequences,
          ExpandCommonTypes, ExpandSequences)
      else
        FullInfoTuple := GI_PyControl.InternalInterpreter.PyInteractiveInterpreter.safegetmembersfullinfo(fPyObject,
          ExpandSequences, ExpandCommonTypes, ExpandSequences);
      FChildCount := len(FullInfoTuple);

      if FChildCount > 0 then begin
        FChildNodes := TStringList.Create;
        FChildNodes.CaseSensitive := True;

        PyFullInfoTuple := ExtractPythonObjectFrom(FullInfoTuple);
        for var I := 0 to FChildCount - 1 do with GetPythonEngine do begin
          PyMemberInfo := PyTuple_GetItem(PyFullInfoTuple, I);
          ObjName :=  PyUnicodeAsString(PyTuple_GetItem(PyMemberInfo, 0));
          PyFullInfo := PyTuple_GetItem(PyMemberInfo, 1);
          APyObject := VarPythonCreate(PyTuple_GetItem(PyFullInfo, 0));

          NameSpaceItem := TRemNameSpaceItem.Create(ObjName, APyObject, FRemotePython);
          NameSpaceItem.ExpandCommonTypes := ExpandCommonTypes;
          NameSpaceItem.ExpandSequences := ExpandSequences;

//          NameSpaceItem.FBufferedValue := PyString_AsWideString(PyTuple_GetItem(PyFullInfo, 1));
//          NameSpaceItem.FGotBufferedValue := True;
          NameSpaceItem.fQualifiedObjectType := PyUnicodeAsString(PyTuple_GetItem(PyFullInfo, 1));
          NameSpaceItem.FObjectInfo := PyLong_AsLong(PyTuple_GetItem(PyFullInfo, 2));
//          if NameSpaceItem.IsProxy then
//            NameSpaceItem.FChildCount := PyInt_AsLong(PyTuple_GetItem(PyFullInfo, 3))
//          else
//            NameSpaceItem.FChildCount := NameSpaceItem.ChildCount;

          FChildNodes.AddObject(ObjName, NameSpaceItem);
        end;
        GetPythonEngine.CheckError;
        if (ObjectType <> 'list') and (ObjectType <> 'tuple') then
          FChildNodes.CustomSort(ComparePythonIdents);
      end;
    except
      FChildCount := 0;

      //StyledMessageDlg(Format(_(SErrorGettingNamespace), [FName]), mtError, [mbAbort], 0);
      System.SysUtils.Abort;
    end;
  end;
end;

function TRemNameSpaceItem.GetDocString: string;
begin
  FRemotePython.CheckConnected;
  var SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
  if not IsProxy then
    Result := inherited GetDocString
  else
    try
      Result := FRemotePython.RPI.inspect.getdoc(fPyObject);
    except
      Result := '';
    end;
end;

procedure TRemNameSpaceItem.FillObjectInfo;
begin
  if not IsProxy then
    inherited
  else begin
    FRemotePython.CheckConnected;
    var SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
    try
      FObjectInfo := FRemotePython.RPI.objectinfo(fPyObject);
    except
      FObjectInfo := 0;
    end;
  end;
end;

function TRemNameSpaceItem.GetObjectType: string;
begin
  if FQualifiedObjectType <> '' then
    Result := FQualifiedObjectType.Substring(FQualifiedObjectType.LastIndexOf('.') + 1)
  else
    Result := FRemotePython.GetObjectType(FPyObject);
end;

function TRemNameSpaceItem.GetValue: string;
begin
  FRemotePython.CheckConnected;
  var SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
  if not IsProxy then
    Result := inherited GetValue
  else
    try
      Result :=  FRemotePython.RPI.saferepr(fPyObject);
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
begin
  CheckConnected;
  Result := False;
  DisplayString := '';
  DocString := '';
  if Expr = '' then Exit;

  Py := SafePyEngine;
  var SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
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

procedure TPyRemoteInterpreter.CheckConnected(Quiet: Boolean = False; Abort: Boolean = True);
begin
  if not (Assigned(ServerProcess) and FServerIsAvailable and FConnected and
    (ServerProcess.State = TPPState.Running)) then
  begin
    FConnected := False;
    if not Quiet then
      StyledMessageDlg(_(SRemoteServerNotConnected),
        mtError, [mbAbort], 0);
    if Abort then
      System.SysUtils.Abort;
  end;
end;

function TPyRemoteInterpreter.Compile(ARunConfig: TRunConfiguration): Variant;
var
  Py: IPyEngineAndGIL;
  FName: string;
  Source: string;
  FileName: string;
  LineNo, Offset: Integer;
  Editor: IEditor;
begin
  CheckConnected;
  if GI_PyControl.DebuggerState <> dsInactive then begin
    StyledMessageDlg(_(SCannotCompileWhileRunning),
      mtError, [mbAbort], 0);
    System.SysUtils.Abort;
  end;

  Py := SafePyEngine;

  VarClear(Result);
  GI_PyControl.ErrorPos := TEditorPos.EmptyPos;

  GI_MessagesService.ClearMessages;

  Editor := GI_EditorFactory.GetEditorByFileId(ARunConfig.ScriptName);
  if Assigned(Editor) then
    Source := CleanEOLs(Editor.SynEdit.Text) + WideLF
  else
    try
      Source := CleanEOLs(FileToStr(ARunConfig.ScriptName)) + WideLF;
    except
      on E: Exception do
        begin
          StyledMessageDlg(Format(_(SFileOpenError),
            [ARunConfig.ScriptName, E.Message]), mtError, [mbOK], 0);
          System.SysUtils.Abort;
        end;
    end;
  FName := ToPythonFileName(ARunConfig.ScriptName);

  try
    Result := RPI.rem_compile(Source, FName);

    Py.PythonEngine.CheckError;
    var ExcInfo := RPI.traceback_exception;
    if not VarIsNone(ExcInfo) then
    begin
      VarClear(Result);
      if ExcInfo.exc_type_str = 'SyntaxError' then
      begin
        ExtractPyErrorInfo(ExcInfo, FileName, LineNo, Offset);
        FileName := FromPythonFileName(FileName);
        GI_PyControl.ErrorPos := TEditorPos.New(FileName, LineNo, Offset, True);
      end
      else
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
    else
    begin
      VarClear(Result);
      raise;
    end;
  end;
end;

constructor TPyRemoteInterpreter.Create(AEngineType: TPythonEngineType = peRemote);
  function IsPyWinAvailable: Boolean;
  var
    Module: PPyObject;
  begin
    with GetPythonEngine do begin
      Module := PyImport_ImportModule('pywintypes');
      try
        if Assigned(Module) then
          Result := True
        else begin
          PyErr_Clear;
          Result := False;
        end;
      finally
        Py_XDECREF(Module);
      end;
    end;
  end;

var
  ServerSource: TStrings;
  ServerName: string;
begin
  FInterpreterCapabilities := [icReInitialize];
  FEngineType := AEngineType;
  DebuggerClass := TPyRemDebugger;

  Randomize;
  FSocketPort := 18000 + Random(1000);

  if (FEngineType = peRemote) and not PyIDEOptions.AlwaysUseSockets and IsPyWinAvailable then
  begin
    FUseNamedPipes := True;
    Inc(FSocketPort, 1000);
  end else
    FUseNamedPipes := False;

  // Import Rpyc
  var SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors

  FOldsysmodules := SysModule.modules.copy();
  try
    FRpycPath := Format('%sLib\%s', [GetAppRootPath, RpycZipModule]);
    GI_PyControl.InternalInterpreter.SysPathAdd(FRpycPath);
    Rpyc := Import('rpyc');
    FServerIsAvailable := True;
  except
    StyledMessageDlg(_(SRpycNotAvailable), mtError, [mbAbort], 0);
    FServerIsAvailable := False;
  end;

  if FServerIsAvailable then begin
    FServerFile :=
      TPath.Combine(TPyScripterSettings.UserDataPath, RemoteServerBaseName);
    case FEngineType of
      peRemote, peSSH: ServerName := 'SimpleServer';
      peRemoteTk: ServerName := 'TkServer';
      peRemoteWx: ServerName := 'WxServer';
    else
      raise Exception.Create('Invalid Engine type in TPyRemoteInterpreter constructor');
    end;
    ServerSource := GI_PyIDEServices.GetStoredScript(ServerName);
    if not SaveWideStringsToFile(FServerFile, ServerSource, False) then
    begin
      StyledMessageDlg(Format(_(SCouldNotWriteServerFile), [FServerFile]), mtError, [mbAbort], 0);
      FServerIsAvailable := False;
    end;
  end;

  if FServerIsAvailable then CreateAndConnectToServer;
end;

destructor TPyRemoteInterpreter.Destroy;
var
  ModulesDict: Variant;
begin
  ShutDownServer;
  VarClear(Rpyc);
  if VarIsPython(FOldsysmodules) and not VarIsNone(FOldsysmodules) then begin
    ModulesDict := SysModule.modules;
    ModulesDict.clear();
    ModulesDict.update(FOldSysModules);
  end;
  VarClear(FOldSysModules);
  GI_PyControl.InternalInterpreter.SysPathRemove(FRpycPath);
  inherited;
end;

function TPyRemoteInterpreter.CreateDebugger: TPyBaseDebugger;
begin
  Result := DebuggerClass.Create(Self);
end;

function TPyRemoteInterpreter.EvalCode(const Expr: string): Variant;
begin
  // may raise exceptions
  if not FConnected then
    Result := None
  else
    Result := RPI.evalcode(Expr);
end;

function TPyRemoteInterpreter.GetGlobals: TBaseNameSpaceItem;
begin
  CheckConnected(True);
  Result := TRemNameSpaceItem.Create('globals',
    EvalCode('globals()'), Self);
end;

function TPyRemoteInterpreter.GetObjectType(Obj: Variant): string;
begin
  Result := GI_PyControl.InternalInterpreter.GetObjectType(Obj);
end;

procedure TPyRemoteInterpreter.HandleRemoteException(const ExcInfo: Variant; SkipFrames: Integer);
// ExcInfo is a traceback.TracebackException instance

  procedure ExtractTracebackFromExcInfo(TraceBack: TPythonTraceback);
  var
    PyFrame: Variant;
    Context, FileName: string;
    LineNo: Integer;
  begin
    if not VarIsPython(ExcInfo) or VarIsNone(ExcInfo) then Exit;

    for var I  := SkipFrames to len(ExcInfo.stack) - 1 do
    begin
      PyFrame := ExcInfo.stack[I];
      if VarIsNone(PyFrame) then Break;
      try
        LineNo := PyFrame.lineno;
      except
        LineNo := 0;
      end;
      Context := PyFrame.name;
      FileName := PyFrame.filename;
      TraceBack.AddItem(Context, FileName, LineNo);
    end;
  end;

var
  Traceback: TPythonTraceback;
  ErrMsg: string;
begin
  CheckConnected;
  if not VarIsPython(ExcInfo) or VarIsNone(ExcInfo) then Exit;

  ErrMsg := Format('%s: %s',
            [VarPythonAsString(ExcInfo.exc_type_str),
             VarPythonAsString(RPI.safestr(ExcInfo))]);
  Traceback := TPythonTraceback.Create;
  try
    ExtractTracebackFromExcInfo(Traceback);
    TThread.Synchronize(nil, procedure
    begin
      HandlePyException(Traceback, ErrMsg, 0);
    end);
  finally
    Traceback.Free;
  end;
end;

function TPyRemoteInterpreter.ImportModule(const FileId: string;
    AddToNameSpace: Boolean = False): Variant;
{
  Imports Editor text without saving the file.
  Does not add the module name to the locals()
  of the interpreter.
}
var
  Py: IPyEngineAndGIL;
  Code: Variant;
  Path, NameOfModule: string;
  PythonPathAdder: IInterface;
  RunConfiguration: TRunConfiguration;
begin
  Py := SafePyEngine;
  CheckConnected;
  VarClear(Result);
  //Compile
  RunConfiguration := TRunConfiguration.CreateFromFileId(FileId);
  try
    Code := Compile(RunConfiguration);
  finally
    RunConfiguration.Free;
  end;

  // an exception should have been raised if failed
  Assert(VarIsPython(Code), 'TPyRemoteInterpreter.ImportModule');

  // Add the path of the imported script to the Python path
  Path := ToPythonFileName(FileId);
  if Path.StartsWith('<') then
    Path := ''
  else
    Path := TPath.GetDirectoryName(Path);
  if Length(Path) > 1 then begin
    PythonPathAdder := AddPathToPythonPath(Path, False);
    SysPathRemove('');
  end;

  NameOfModule := FileNameToModuleName(FileId);

  GI_PyControl.DebuggerState := dsRunning;
  try
    try
      Result := RPI.rem_import(NameOfModule, Code);
      Py.PythonEngine.CheckError;
      var ExcInfo: Variant := RPI.traceback_exception;
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
      Conn.execute('import ' + NameOfModule);
      RPI.locals.__setitem__(NameOfModule, Result);
  finally
    //  Add again the empty path
    SysPathAdd('');

    GI_PyControl.DebuggerState := dsInactive;
  end;
end;

function TPyRemoteInterpreter.NameSpaceFromExpression(const Expr: string): TBaseNameSpaceItem;
var
  LookupObj, ItemsDict: Variant;
begin
  CheckConnected;
  var SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
  try
    if Expr <> '' then begin
      //Evaluate the lookup expression
      LookupObj := RPI.evalcode(Expr);
      Result := TRemNameSpaceItem.Create(Expr, LookupObj, Self);
    end else begin
      ItemsDict := RPI.evalcode('dict()');
      ItemsDict.update(Conn.modules.__getitem__(GetPythonEngine.BuiltInModuleName).__dict__);
      ItemsDict.update(RPI.evalcode('vars()'));
      Result := TRemNameSpaceItem.Create(Expr, ItemsDict, Self);
    end;
  except
    Result := nil;
  end;
end;

function TPyRemoteInterpreter.NameSpaceItemFromPyObject(AName: string;
  APyObject: Variant): TBaseNameSpaceItem;
begin
  Result := TRemNameSpaceItem.Create(AName, APyObject, Self);
end;

procedure TPyRemoteInterpreter.CreateAndConnectToServer;
begin
  try
    CreateAndRunServerProcess;  // Sets FServerIsAvailable to true
    if FServerIsAvailable then ConnectToServer;  // Sets FConnected to true
  except
    on E: Exception do
    begin
      FConnected := False;
      StyledMessageDlg(_(SErrorCreatingRemoteEngine) + E.Message, mtError, [mbAbort], 0);
    end;
  end;
  if not (FServerIsAvailable and FConnected) then
  begin
    StyledMessageDlg(_(SCouldNotConnectRemoteEngine), mtError, [mbAbort], 0);
    ShutDownServer;
  end;
end;

procedure TPyRemoteInterpreter.ProcessServerOutput(Sender: TObject;
  const Bytes: TBytes; BytesRead: Cardinal);
begin
   // deal with Bytes containing incomplete UTF8 character
   FStoredServerOutput := FStoredServerOutput + Copy(Bytes, 0, BytesRead);
   try
     var Output := TEncoding.UTF8.GetString(FStoredServerOutput);
     GI_PyInterpreter.PythonIO.OnSendUniData(Self, Output);
     FStoredServerOutput := [];
   except
   end;
end;

procedure TPyRemoteInterpreter.CreateAndRunServerProcess;
var
  ExeName: string;
begin
  FServerIsAvailable := False;

  // Repeat here to make sure it is set right
  MaskFPUExceptions(PyIDEOptions.MaskFPUExceptions);

  if PyIDEOptions.PreferFreeThreaded and
    (GI_PyControl.PythonVersion.PythonFreeThreadedExecutable <> '')
  then
    ExeName := GI_PyControl.PythonVersion.PythonFreeThreadedExecutable
  else
    ExeName := GI_PyControl.PythonVersion.PythonExecutable;

  ServerProcess := TPProcess.Create(AddQuotesUnless(ExeName) +
    Format(' -u -X utf8 "%s" %d "%s"', [FServerFile, FSocketPort, FRpycPath]));
  ServerProcess.MergeError := True;
  ServerProcess.ShowWindow := swNotSet;
  ServerProcess.CreationFlag := cfNoWindow;
  ServerProcess.OnRead := ProcessServerOutput;
  ServerProcess.Execute;
  while ServerProcess.State = TPPState.Created do
    TThread.Yield;
  FServerIsAvailable := ServerProcess.State = TPPState.Running;
end;

procedure TPyRemoteInterpreter.ReInitialize;
var
  Py: IPyEngineAndGIL;
begin
  FStoredServerOutput := [];
  Py := SafePyEngine;
  case GI_PyControl.DebuggerState of
    dsDebugging, dsRunning:
      begin
        GI_PyInterpreter.ShowOutput := False;
        ShutDownServer;  // sets fIsConnected to False
        Py.PythonEngine.PyErr_Clear;
        //  Running/Debugging will detect that fIsConnected is false,
        //  finish run/debug orderly and Reinitialize the server
        //  So we should not reinitialize here.
      end;
    dsInactive:
      begin
        GI_PyInterpreter.ShowOutput := True;
        ShutDownServer;
        CreateAndConnectToServer;
        if FServerIsAvailable and FConnected then begin
          GI_PyInterpreter.AppendText(sLineBreak + _(SRemoteInterpreterInit));
          GI_PyInterpreter.AppendPrompt;
          // Recreate the Active debugger
          PyControl.ActiveDebugger := DebuggerClass.Create(Self);

          // Add extra project paths
          GI_PyControl.AppendProjectPaths;

          Initialize;
        end;
      end;
    else
      // Should not happen.  Reinitialise is not enabled for other states
      Assert(False, Format(SInternalError, ['ReInitialize']));
  end;
end;

function TPyRemoteInterpreter.GetInterpreter: Variant;
begin
  Result := RPI;
end;

procedure TPyRemoteInterpreter.RestoreCommandLine;
var
  Py: IPyEngineAndGIL;
begin
  CheckConnected;
  Py := SafePyEngine;
  Conn.modules.sys.argv := FOldargv;
end;

procedure TPyRemoteInterpreter.Run(ARunConfig: TRunConfiguration);
var
  Py: IPyEngineAndGIL;
  Code: Variant;
  Path, OldPath: string;
  PythonPathAdder: IInterface;
begin
  CheckConnected;
  CanDoPostMortem := False;

  Py := SafePyEngine;

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
  if Path = '' then
    Path := SystemTempFolder;
  OldPath := RPI.rem_getcwdu();

  // Change the current path
  try
    RPI.rem_chdir(Path);
  except
    StyledMessageDlg(_(SCouldNotSetCurrentDir), mtWarning, [mbOK], 0);
  end;

  GI_PyControl.DebuggerState := dsRunning;

  // New Line for output
  GI_PyInterpreter.AppendText(sLineBreak);

  // Set the command line parameters
  SetCommandLine(ARunConfig);

  GI_PyInterpreter.ShowWindow(not Assigned(GI_ActiveEditor));

  ThreadPythonExec(
    procedure
    begin
      TThread.NameThreadForDebugging('Remote Debugger');
      var Py := SafePyEngine;
      var Py_RPI := ExtractPythonObjectFrom(RPI);
      var Py_Code := ExtractPythonObjectFrom(Code);
      Py.PythonEngine.Py_XINCREF(Py_Code);
      VarClear(Code);
      // 'N' means that we are passing ownership of Py_Code
      Py.PythonEngine.PyObject_CallMethod(Py_RPI, 'run_nodebug', 'N', Py_Code);
      if Py.PythonEngine.PyErr_Occurred <> nil then
      begin
        // This means that the python server crashed or got disconnected
        FConnected := False;
        Py.PythonEngine.PyErr_Clear; // Ignore errors
      end;
    end,

    procedure
    var
      Py: IPyEngineAndGIL;
    begin
      Py := SafePyEngine;
      try
        try
          if Connected then begin
            var ExcInfo := RPI.traceback_exception;
            if not VarIsNone(ExcInfo) then begin
              HandleRemoteException(ExcInfo);
              CanDoPostMortem := True;
            end;
          end;
        except
          // CheckError already called by VarPyth
          on E: EPyEOFError do FConnected := False;
          on E: EPythonError do
            begin
              // should not happen
              CheckConnected(True, False);
              if Connected then HandlePyException(GetPythonEngine.Traceback,E.Message);
            end;
        end;
      finally
        GI_PyInterpreter.WritePendingMessages;
        GI_PyInterpreter.AppendPrompt;
        CheckConnected(True, False);

        if Connected then begin
          // Restore the command line parameters
          RestoreCommandLine;

          // Add again the empty path
          SysPathAdd('');

          // Change the back current path
          RPI.rem_chdir(OldPath);
        end;
        GI_PyControl.DebuggerState := dsInactive;
        PythonPathAdder := nil;
        if not Connected then begin
          GI_PyInterpreter.ClearPendingMessages;
          // Reinitialize destroys the interpreter which executes this method!
          // So handle with a delay
          TThread.ForceQueue(nil, procedure
          begin
            GI_PyControl.ActiveInterpreter.ReInitialize;
          end, 500);
        end else if CanDoPostMortem and PyIDEOptions.PostMortemOnException then
          GI_PyControl.ActiveDebugger.EnterPostMortem;
      end;
    end);
end;

function TPyRemoteInterpreter.RunSource(const Source, FileName: string; const Symbol: string = 'single'): Boolean;
var
  Py: IPyEngineAndGIL;
  OldDebuggerState: TDebuggerState;
//  OldPos: TEditorPos;
begin
  CheckConnected;
  Assert(not GI_PyControl.Running, 'RunSource called while the Python engine is active');
  OldDebuggerState := GI_PyControl.DebuggerState;

  GI_PyControl.DebuggerState := dsRunning;
  Py := SafePyEngine;
  try
    try
      Result := RPI.runsource(Source, FileName, Symbol);
    except
      Result := False;
    end;

    CheckConnected(True, False);
    if not Connected then
    begin
      Result := False;
      GI_PyInterpreter.ClearPendingMessages;
      TThread.ForceQueue(nil, procedure
      begin
        GI_PyControl.ActiveInterpreter.ReInitialize;
      end, 500);
    end;
  finally
    GI_PyControl.DebuggerState := OldDebuggerState;
  end;
end;

procedure TPyRemoteInterpreter.ConnectToServer;
var
  Source: string;
  InitScriptName: string;
begin
  FConnected := False;

  var SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors

  if FUseNamedPipes then
    try
      Sleep(500);  // Give the server some time to import pywin32 and create the pipe
      FNamedPipeStream := Rpyc.core.stream.NamedPipeStream.create_client(FSocketPort.ToString);
      Assert(VarIsPython(FNamedPipeStream), 'TPyRemoteInterpreter.ConnectToServer');
      FUseNamedPipes := True;
    except
      with GetPythonEngine do
        if PyErr_Occurred <> nil then
          PyErr_Clear;
      FUseNamedPipes := False;
    end;

  // Try to connect a few times
  for var I := 1 to 5 do
    try
      if FUseNamedPipes then
        Conn := Rpyc.classic.connect_stream(FNamedPipeStream)
      else
        Conn := Rpyc.classic.connect('localhost', FSocketPort);
      FConnected := True;
      Break;
    except
      on E: Exception do
      begin
        // wait and try again
        with GetPythonEngine do
          if PyErr_Occurred <> nil then
            PyErr_Clear;
        TThread.Yield;
        Sleep(300);
      end;
    end;

  if FConnected then begin
    Conn._config.__setitem__('sync_request_timeout', None);

    // Create the remote interpreter
    InitScriptName := 'Rpyc_Init';

    Source := CleanEOLs(GI_PyIDEServices.GetStoredScript(InitScriptName).Text) + #10;
    Conn.execute(Source);
    RPI := Conn.namespace.__getitem__('_RPI');
    //  pass a reference to the P4D module DebugIDE
    Conn.namespace.__delitem__('_RPI');

    // sys.displayhook
    if PyIDEOptions.PrettyPrintOutput then
      RPI.setupdisplayhook();
    GetPythonEngine.CheckError;

    FPythonVersion := Conn.modules.sys.version;
    FPythonPlatform := Conn.modules.sys.platform;
  end;
end;

procedure TPyRemoteInterpreter.Pickle(AValue: Variant; const FileName: string);
var
  FileObject: Variant;
  Py: IPyEngineAndGIL;
begin
  Py := SafePyEngine;
  var SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
  try
    FileObject := Conn.modules.builtins.open(FileName, 'wb');
  except
    raise Exception.CreateFmt(SCouldNotOpenOutputFile, [FileName]);
  end;

  try
    Conn.modules.pickle.dump(AValue, FileObject);
  except
    FileObject.close();
    raise Exception.Create(SPickleFailed);
  end;
  FileObject.close();
end;

procedure TPyRemoteInterpreter.ServeConnection(MaxCount: Integer);
var
  Count: Integer;
begin
  CheckConnected(True, False);
  if not Connected then Exit;

  Count := 0;
  while Conn.poll() do begin
    Inc(Count);
    if (MaxCount > 0) and (Count >= MaxCount) then
      Break;
  end;
end;

procedure TPyRemoteInterpreter.SetCommandLine(ARunConfig: TRunConfiguration);
var
  Py: IPyEngineAndGIL;
  ArgV: Variant;
  Params, Param: string;
begin
  CheckConnected;
  Py := SafePyEngine;
  FOldargv := Conn.modules.sys.argv;
  Conn.execute('__import__("sys").argv = []');
  ArgV := Conn.modules.sys.argv;
  var ScriptName :=  ToPythonFileName(ARunConfig.ScriptName);
  ArgV.append(ScriptName);

  Params := Trim(GI_PyIDEServices.ReplaceParams(ARunConfig.Parameters));
  if Params <> '' then begin
    Params := GI_PyIDEServices.ReplaceParams(Params);
    var P := PChar(Params);
    while P^ <> #0 do begin
      P := GetParamStr(P, Param);
      ArgV.append(Param);
    end;
    GI_PyInterpreter.AppendText(Format(_(SCommandLineMsg), [Params]) + SLineBreak);
  end;
end;

procedure TPyRemoteInterpreter.ShutDownServer;
var
  OldExceptHook: Variant;
begin
  if FServerIsAvailable and Assigned(ServerProcess) and (ServerProcess.State = TPPState.Running) then
  begin
    if Assigned(GI_PyControl.ActiveDebugger) then
       GI_PyControl.ActiveDebugger.Abort;

    if FConnected then begin
      try
        Conn.close();  // closes and shuts down SimpleServer
      except
      end;
    end;
    FConnected := False;
    try
      VarClear(FNamedPipeStream);
    except
    end;

    try
      if not PyControl.Finalizing then
        GI_PyIDEServices.ClearPythonWindows;

      VarClear(FOldArgv);
      VarClear(RPI);
      VarClear(Conn);

      // Restore excepthook
      OldExceptHook := VarPyth.SysModule.__excepthook__;
      SysModule.excepthook := OldExceptHook;
    except
     // swallow exceptions
    end;
    Sleep(100);
    if ServerProcess.State = TPPState.Running then
      ServerProcess.Terminate;
  end;

  ServerProcess := nil;
  FServerIsAvailable := False;
  FConnected := False;
  FInitialized := False;
end;

procedure TPyRemoteInterpreter.StringsToSysPath(Strings: TStrings);
var
  Py: IPyEngineAndGIL;
  PythonPath: Variant;
begin
  CheckConnected;
  Py := SafePyEngine;
  // could have used Conn.deliver
  Conn.execute('__import__("sys").path = []');
  PythonPath := Conn.modules.sys.path;
  for var I := 0 to Strings.Count - 1 do
    PythonPath.append(Strings[I]);
end;

function TPyRemoteInterpreter.SysPathAdd(const Path: string): Boolean;
var
  Py: IPyEngineAndGIL;
  SysPath: Variant;
begin
  Result := False;
  CheckConnected(True, False);
  if not Connected then Exit;

  Py := SafePyEngine;
  SysPath := Conn.modules.sys.path;
  if not SysPath.__contains__(Path) then
  begin
    if (SysPath.__len__() > 0) and (SysPath.__getitem__(0) = '') then
      SysPath.insert(1, Path)
    else
      SysPath.insert(0, Path);
    Result := True;
  end;
end;

function TPyRemoteInterpreter.SysPathRemove(const Path: string): Boolean;
var
  Py: IPyEngineAndGIL;
  SysPath: Variant;
begin
  Result := False;
  CheckConnected(True, False);
  if not Connected then Exit;

  Py := SafePyEngine;
  SysPath := Conn.modules.sys.path;
  if SysPath.__contains__(Path) then
  begin
    Result := True;
    SysPath.remove(Path);
  end;
end;

procedure TPyRemoteInterpreter.SysPathToStrings(Strings: TStrings);
var
  Py: IPyEngineAndGIL;
  RemPath: Variant;
begin
  CheckConnected(True, False);
  if not Connected then Exit;

  Py := SafePyEngine;
  RemPath := Rpyc.classic.obtain(Conn.modules.sys.path);
  for var I := 0 to len(RemPath) - 1  do
    Strings.Add(RemPath.__getitem__(I));
end;

procedure TPyRemoteInterpreter.SystemCommand(const Cmd: string);
var
  Py: IPyEngineAndGIL;
begin
  Py := SafePyEngine;
  RPI.system_command(Cmd);
end;

function TPyRemoteInterpreter.UnitTestResult: Variant;
begin
  Result := RPI.IDETestResult();
end;

(* Extract Error information from a VarPyth variant containing a Python error *)
class procedure TPyRemoteInterpreter.ExtractPyErrorInfo(E: Variant;
  var FileName: string; var LineNo: Integer; var Offset: Integer);
begin
  try
    FileName := E.filename;
  except
    FileName := '';
  end;
  try
    LineNo := E.lineno;
  except
    LineNo := 0;
  end;
  try
    Offset := E.offset;
  except
    Offset := 0;
  end;
end;

{ TPyRemDebugger }

procedure TPyRemDebugger.Abort;
begin
  case GI_PyControl.DebuggerState of
    dsPostMortem: ExitPostMortem;
    dsDebugging,
    dsRunning: FRemotePython.ServerProcess.RaiseKeyboardInterrupt;
    dsPaused:
      begin
        GI_PyInterpreter.RemovePrompt;
        FDebuggerCommand := dcAbort;
        DoDebuggerCommand;
      end;
  end;
end;

constructor TPyRemDebugger.Create(RemotePython: TPyRemoteInterpreter);
begin
  inherited Create;
  FRemotePython := RemotePython;
  FDebugManager := FRemotePython.RPI.DebugManager;
  FDebugManager.debugIDE :=
    GI_PyControl.InternalInterpreter.PyInteractiveInterpreter.debugIDE;

  FMainDebugger := FDebugManager.main_debugger;

  FMainThread := TThreadInfo.Create;
  FMainThread.Name := 'MainThread';
  FMainThread.Status := thrdRunning;
  FMainThread.Thread_ID := FDebugManager.main_thread_id;

  FThreads := TObjectDictionary<Int64, TThreadInfo>.Create([doOwnsValues]);
  FThreads.Add(FMainThread.Thread_ID, FMainThread);

  FLineCache := FRemotePython.Conn.modules.linecache;
  FDebuggerCommand := dcNone;
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
  FreeAndNil(FThreads);
  inherited;
end;

procedure TPyRemDebugger.EnterPostMortem;
var
  Py: IPyEngineAndGIL;
  Frame, BotFrame, TraceBack: Variant;
begin
  Py := SafePyEngine;
  if not (HaveTraceback and (GI_PyControl.DebuggerState = dsInactive)) then
    Exit;

  GI_PyInterpreter.SetPyInterpreterPrompt(pipPostMortem);
  GI_PyInterpreter.AppendPrompt;

  TraceBack := FRemotePython.Conn.modules.sys.last_traceback;
  BotFrame := TraceBack.tb_frame.f_back; // so that TraceBack.tb_frame is included
  while not VarIsNone(TraceBack.tb_next) do
    TraceBack := TraceBack.tb_next;
  Frame := TraceBack.tb_frame;

  GI_PyControl.DebuggerState := dsPostMortem;
  FMainThread.Status := thrdBroken;
  GetCallStack(FMainThread.CallStack, Frame, BotFrame);
  TPyBaseDebugger.ThreadChangeNotify(FMainThread, tctAdded );

  DSAMessageDlg(dsaPostMortemInfo, 'PyScripter', _(SPostMortemInfo),
     mtInformation, [mbOK], 0, dckActiveForm, 0, mbOK);
end;

function TPyRemDebugger.Evaluate(const Expr: string): TBaseNameSpaceItem;
var
  Py: IPyEngineAndGIL;
  ExprValue: Variant;
begin
  Result := nil;
  if GI_PyControl.DebuggerState in [dsPaused, dsPostMortem] then begin
    var SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
    FExecPaused := True;
    try
      try
        Py := SafePyEngine;
        // evalcode knows we are in the debugger and uses current frame locals/globals
        ExprValue := FRemotePython.RPI.evalcode(Expr);
        Result := TNameSpaceItem.Create(Expr, ExprValue);
      except
        // fail quietly
      end;
    finally
      FExecPaused := False;
    end;
  end;
end;

procedure TPyRemDebugger.Evaluate(const Expr: string; out ObjType,
  Value: string);
var
  Py: IPyEngineAndGIL;
  ExprValue: Variant;
begin
  ObjType := _(SNotAvailable);
  Value := _(SNotAvailable);
  if GI_PyControl.DebuggerState in [dsPaused, dsPostMortem] then begin
    var SuppressOutput := GI_PyInterpreter.OutputSuppressor; // Do not show errors
    FExecPaused := True;
    try
      try
        Py := SafePyEngine;
        // evalcode knows we are in the debugger and uses current frame locals/globals
        ExprValue := FRemotePython.RPI.evalcode(Expr);
        ObjType := FRemotePython.RPI.objecttype(ExprValue);
        Value := FRemotePython.RPI.saferepr(ExprValue);
      except
        // fail quietly
      end;
    finally
      FExecPaused := False;
    end;
  end;
end;

procedure TPyRemDebugger.ExitPostMortem;
var
  Py: IPyEngineAndGIL;
begin
  GI_PyInterpreter.SetPyInterpreterPrompt(pipNormal);
  GI_PyInterpreter.AppendPrompt;

  Py := SafePyEngine;
  FMainThread.Status := thrdRunning;
  FMainThread.CallStack.Clear;
  TPyBaseDebugger.ThreadChangeNotify(FMainThread, tctStatusChange);
  MakeFrameActive(nil);
  GI_PyControl.DebuggerState := dsInactive;
end;

procedure TPyRemDebugger.GetCallStack(CallStackList: TObjectList<TBaseFrameInfo>;
  Frame, Botframe: Variant);
begin
  CallStackList.Clear;
  if VarIsPython(Frame) then begin
    while not VarIsNone(Frame.f_back) and not VarIsNone(Frame.f_back.f_back) do
    begin
      CallStackList.Add(TFrameInfo.Create(Frame));
      Frame := Frame.f_back;
      if VarIsSame(Frame, Botframe) then Break;
    end;
    // at least one frame
    if CallStackList.Count = 0  then
      CallStackList.Add(TFrameInfo.Create(Frame));
  end;
end;

function TPyRemDebugger.GetFrameGlobals(Frame: TBaseFrameInfo): TBaseNameSpaceItem;
begin
  Result := nil;
  if not (GI_PyControl.DebuggerState in [dsPaused, dsPostMortem]) then
    Exit;
  Result := TRemNameSpaceItem.Create('globals',
    (Frame as TFrameInfo).PyFrame.f_globals, FRemotePython);
end;

function TPyRemDebugger.GetFrameLocals(Frame: TBaseFrameInfo): TBaseNameSpaceItem;
begin
  Result := nil;
  if not (GI_PyControl.DebuggerState in [dsPaused, dsPostMortem]) then
    Exit;
  Result := TRemNameSpaceItem.Create('locals',
    (Frame as TFrameInfo).PyFrame.f_locals, FRemotePython);
end;

function TPyRemDebugger.GetPostMortemEnabled: Boolean;
begin
  Result := FRemotePython.CanDoPostMortem;
end;

procedure TPyRemDebugger.DoDebuggerCommand;
var
  Py: IPyEngineAndGIL;
begin
   FRemotePython.CheckConnected;

   if (GI_PyControl.DebuggerState <> dsPaused) or not FRemotePython.Connected then
     Exit;

   if GI_BreakpointManager.BreakpointsChanged then SetDebuggerBreakpoints;

//   case FDebuggerCommand of
//     dcRun         : FRemotePython.Debugger.set_continue();
//     dcStepInto    : FRemotePython.Debugger.set_step();
//     dcStepOver    : FRemotePython.Debugger.set_next(fCurrentFrame);
//     dcStepOut     : FRemotePython.Debugger.set_return(fCurrentFrame);
//     dcRunToCursor : FRemotePython.Debugger.set_continue;
//     dcPause       : FRemotePython.Debugger.set_step();
//     dcAbort       : FRemotePython.Debugger.set_quit();
//   end;
   Py := SafePyEngine;
   FDebugManager.debug_command := Ord(FDebuggerCommand);
   FDebuggerCommand := dcNone;
end;

function TPyRemDebugger.HaveTraceback: Boolean;
var
  Py: IPyEngineAndGIL;
begin
  Result := False;
  with FRemotePython do
    if not (IsAvailable and Connected) then
      Exit;
  try
    Py := SafePyEngine;
    Result := FRemotePython.Conn.eval('hasattr(__import__("sys"), "last_traceback")');
  except
  end;
end;

procedure TPyRemDebugger.LoadLineCache;
begin
  // inject unsaved code into LineCache
  FLineCache.cache.clear();
  GI_EditorFactory.ApplyToEditors(procedure(Editor: IEditor)
  var
    LineList: Variant;
    SFName, Source: string;
  begin
    with Editor do begin
      if not HasPythonFile then Exit;
      SFName := FRemotePython.ToPythonFileName(FileId);
      if SFName.StartsWith('<') and
        // so that we do not push to ssh engine linecache all local open files
        (PyControl.RunConfig.ScriptName = FileId) then
      begin
        Source := CleanEOLs(SynEdit.Text)+WideLF;
        LineList := VarPythonCreate(Source);
        LineList := FRemotePython.Rpyc.classic.deliver(FRemotePython.Conn, LineList.splitlines(True));
        FLineCache.cache.__setitem__(SFName,
          VarPythonCreate([0, None, LineList, SFName], stTuple));
      end;
    end;
  end);
end;

procedure TPyRemDebugger.MakeFrameActive(Frame: TBaseFrameInfo);
var
  Py: IPyEngineAndGIL;
begin
  Py := SafePyEngine;
  if Assigned(Frame) then
    FDebugManager.active_frame := (Frame as TFrameInfo).PyFrame
  else
    FDebugManager.active_frame := None;
end;

procedure TPyRemDebugger.MakeThreadActive(Thread: TThreadInfo);
var
  Py: IPyEngineAndGIL;
begin
  Py := SafePyEngine;
  if Assigned(Thread) then
    FDebugManager.active_thread := Thread.Thread_ID
  else
    FDebugManager.active_thread := None;
end;

function TPyRemDebugger.NameSpaceFromExpression(const Expr: string): TBaseNameSpaceItem;
//  The internal interpreter knows whether we are debugging and
//  adjusts accordingly (in II.evalcode)
begin
  Result := FRemotePython.NameSpaceFromExpression(Expr);
end;

procedure TPyRemDebugger.Pause;
begin
  FDebuggerCommand := dcPause;
  DoDebuggerCommand;
end;

procedure TPyRemDebugger.RestoreCommandLine;
begin
  FRemotePython.RestoreCommandLine;
end;

procedure TPyRemDebugger.Resume;
begin
  GI_PyInterpreter.RemovePrompt;
  FDebuggerCommand := dcRun;
  DoDebuggerCommand;
end;

procedure TPyRemDebugger.Debug(ARunConfig: TRunConfiguration;
  InitStepIn: Boolean = False; RunToCursorLine: Integer = -1);
var
  Py: IPyEngineAndGIL;
  Code: Variant;
  AsyncRun: Variant;
  AsyncResult: Variant;
  Path, OldPath: string;
  PythonPathAdder: IInterface;
  TerminateProc: TThreadProcedure;
begin
  FRemotePython.CheckConnected;
  FRemotePython.CanDoPostMortem := False;
  FDebuggerCommand := dcRun;
  Assert(GI_PyControl.DebuggerState = dsInactive, 'TPyRemDebugger.Debug');

  Py := SafePyEngine;
  //Compile
  Code := FRemotePython.Compile(ARunConfig);
  if not VarIsPython(Code) then Exit;

  // Add the path of the script to the Python Path - Will be automatically removed
  Path := FRemotePython.ToPythonFileName(ARunConfig.ScriptName);
  if Path.StartsWith('<') then
    Path := ''
  else
    Path := TPath.GetDirectoryName(Path);
  FRemotePython.SysPathRemove('');
  if Path.Length > 1 then
    PythonPathAdder := FRemotePython.AddPathToPythonPath(Path);

  // Set the Working directory
  if ARunConfig.WorkingDir <> '' then
    Path := GI_PyIDEServices.ReplaceParams(ARunConfig.WorkingDir);
  if Path.Length <= 1 then
    Path := FRemotePython.SystemTempFolder;
  OldPath := FRemotePython.RPI.rem_getcwdu();

  // Change the current path
  try
    FRemotePython.RPI.rem_chdir(Path);
  except
    StyledMessageDlg(_(SCouldNotSetCurrentDir), mtWarning, [mbOK], 0);
  end;

  GI_PyControl.DebuggerState := dsDebugging;
  TPyBaseDebugger.ThreadChangeNotify(FMainThread, tctAdded );

  GI_MessagesService.ClearMessages;
  // Set the layout to the Debug layout is it exists
  if GI_PyIDEServices.Layouts.LayoutExists('Debug') then begin
    GI_PyIDEServices.Layouts.SaveLayout('Current');
    GI_PyIDEServices.Layouts.LoadLayout('Debug');
    Application.ProcessMessages;
  end else
    GI_PyInterpreter.ShowWindow(not Assigned(GI_ActiveEditor));

  GI_PyInterpreter.SetPyInterpreterPrompt(pipDebug);
  // A prompt will be added when the debugger breaks

  //attach debugger callback routines
  with PyControl.InternalPython.DebugIDE.Events do begin
    Items[dbie_user_call].OnExecute := UserCall;
    Items[dbie_user_line].OnExecute := UserLine;
    Items[dbie_user_thread].OnExecute := UserThread;
    Items[dbie_user_exception].OnExecute := UserException;
    Items[dbie_user_yield].OnExecute := UserYield;
  end;

  //set breakpoints
  SetDebuggerBreakpoints;
  if RunToCursorLine >= 0 then  // add temp breakpoint
    FMainDebugger.set_break(Code.co_filename, RunToCursorLine, True);

  // New Line for output
  GI_PyInterpreter.AppendText(sLineBreak);

  // Set the command line parameters
  SetCommandLine(ARunConfig);

  FMainDebugger.InitStepIn := InitStepIn;

  AsyncRun := FRemotePython.Rpyc.async_(FMainDebugger.run);
  AsyncResult := AsyncRun.__call__(Code);
  GetPythonEngine.CheckError;

  TerminateProc := procedure
  var
    Py: IPyEngineAndGIL;
  begin
    Py := SafePyEngine;
    try
      try
        if FRemotePython.Connected then
        begin
          var ExcInfo: Variant := FRemotePython.RPI.traceback_exception;
          if not VarIsNone(ExcInfo) then begin
            FRemotePython.HandleRemoteException(ExcInfo);
            FRemotePython.CanDoPostMortem := True;
          end;
        end;
      except
        // CheckError already called by VarPyth
        on E: EPyEOFError do FRemotePython.FConnected := False;
        on E: EPythonError do
          begin
            // should not happen
            FRemotePython.CheckConnected(True, False);
            if FRemotePython.Connected then
              FRemotePython.HandlePyException(GetPythonEngine.Traceback, E.Message);
          end;
      end;
    finally
      VarClear(AsyncResult);
      GI_PyInterpreter.WritePendingMessages;
      GI_PyInterpreter.SetPyInterpreterPrompt(pipNormal);
      GI_PyInterpreter.AppendPrompt;

      FRemotePython.CheckConnected(True, False);
      if FRemotePython.Connected then begin
        MakeFrameActive(nil);

        // Restore the command line parameters
        RestoreCommandLine;

        // Add again the empty path
        FRemotePython.SysPathAdd('');

        // Change the back current path
        FRemotePython.RPI.rem_chdir(OldPath);
      end;

      if GI_PyIDEServices.Layouts.LayoutExists('Debug') then
        GI_PyIDEServices.Layouts.LoadLayout('Current');

      GI_PyControl.DebuggerState := dsInactive;
      PythonPathAdder := nil;
      if not FRemotePython.Connected then begin
        GI_PyInterpreter.ClearPendingMessages;
        // Reinitialize destroys the debugger which executes this method!
        // So handle with a delay
        TThread.ForceQueue(nil, procedure
        begin
          GI_PyControl.ActiveInterpreter.ReInitialize;
        end, 500);
      end else if FRemotePython.CanDoPostMortem and PyIDEOptions.PostMortemOnException then
        GI_PyControl.ActiveDebugger.EnterPostMortem;
    end;
  end;

  TThread.CreateAnonymousThread(
    procedure
    var
      Py: IPyEngineAndGIL;
      AsyncReady: Boolean;
    begin
      TThread.NameThreadForDebugging('Remote Debugger');
      AsyncReady := False;

      repeat
        Sleep(50);
        if FExecPaused then Continue;

        Py := SafePyEngine;  // Aquire the GIL
        try
          // Serve the connection
          if FRemotePython.Connected then FRemotePython.ServeConnection(500);
        except
          FRemotePython.FConnected := False;
          GI_PyInterpreter.ClearPendingMessages;
        end;
        if FRemotePython.Connected then
        try
          // Check whether we are done debugging
          AsyncReady := AsyncResult._is_ready;
        except
          FRemotePython.FConnected := False;
        end;
        Py := nil; // Release the GIL
      until not FRemotePython.Connected or AsyncReady;

      if FRemotePython.FUseNamedPipes then
        // #1375 GetOverlappedResult fails on thread exit!
        TThread.Synchronize(nil, TerminateProc)
      else
        TThread.Queue(nil, TerminateProc);

      // See https://en.delphipraxis.net/topic/10361-memory-leak-with-anonymous-methods/
      TerminateProc := nil;
    end
  ).Start;
end;

function TPyRemDebugger.RunSource(const Source, FileName: string; const Symbol: string = 'single'): Boolean;
// The interpreter RunSource calls II.runsource which differs
// according to whether we are debugging or not
begin
  if not (GI_PyControl.DebuggerState in [dsPaused, dsPostMortem]) then
    Exit(False);

  FExecPaused := True;
  try
    Result := FRemotePython.RunSource(Source, FileName, Symbol);
  finally
    FExecPaused := False;
  end;
end;

procedure TPyRemDebugger.RunToCursor(const FileId: string; ALine: Integer);
var
  Py: IPyEngineAndGIL;
  FName: string;
begin
  Assert(GI_PyControl.DebuggerState = dsPaused, 'TPyRemDebugger.RunToCursor');
  GI_PyInterpreter.RemovePrompt;
  // Set Temporary breakpoint
  if GI_BreakpointManager.BreakpointsChanged then
    SetDebuggerBreakpoints;  // So that the temp one is not cleared
  FName := FRemotePython.ToPythonFileName(FileId);
  Py := SafePyEngine;
  FMainDebugger.set_break(FName, ALine, True);

  FDebuggerCommand := dcRunToCursor;
  DoDebuggerCommand;
end;

procedure TPyRemDebugger.SetCommandLine(ARunConfig: TRunConfiguration);
begin
  FRemotePython.SetCommandLine(ARunConfig);
end;

procedure TPyRemDebugger.SetDebuggerBreakpoints;
var
  Py: IPyEngineAndGIL;
begin
  Py := SafePyEngine;
  LoadLineCache;
  FMainDebugger.clear_all_breaks();

  for var BPInfo in GI_BreakpointManager.AllBreakPoints do
    if not BPInfo.Disabled then
      FMainDebugger.set_break(FRemotePython.ToPythonFileName(BPInfo.FileId),
       BPInfo.LineNo, False, BPInfo.Condition, BPInfo.IgnoreCount);

  GI_BreakpointManager.BreakpointsChanged := False;
end;

procedure TPyRemDebugger.StepInto;
begin
  GI_PyInterpreter.RemovePrompt;
  FDebuggerCommand := dcStepInto;
  DoDebuggerCommand;
end;

procedure TPyRemDebugger.StepOut;
begin
  GI_PyInterpreter.RemovePrompt;
  FDebuggerCommand := dcStepOut;
  DoDebuggerCommand;
end;

procedure TPyRemDebugger.StepOver;
begin
  GI_PyInterpreter.RemovePrompt;
  FDebuggerCommand := dcStepOver;
  DoDebuggerCommand;
end;

procedure TPyRemDebugger.UserCall(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
// Not used
begin
   var Py := SafePyEngine;
   Result := Py.PythonEngine.ReturnNone;
end;

procedure TPyRemDebugger.UserException(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
// Not used
begin
   var Py := SafePyEngine;
   Result := Py.PythonEngine.ReturnNone;
end;

procedure TPyRemDebugger.UserLine(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
var
  Py: IPyEngineAndGIL;
  FName: string;
  Frame: Variant;
  LineN: Integer;
  Thread_ID: Int64;
  ThreadInfo: TThreadInfo;
  Arguments: Variant;
  BotFrame: Variant;
  CanBreak: Boolean;
begin
  Py := SafePyEngine;

  FRemotePython.CheckConnected;

  Arguments := VarPythonCreate(Args);
  Thread_ID := Arguments.__getitem__(0);
  // Do not stop while executing code in paused state
  if FExecPaused or not FThreads.TryGetValue(Thread_ID, ThreadInfo) then
  begin
    Result := Py.PythonEngine.ReturnFalse;
    Exit;
  end;

  Frame := Arguments.__getitem__(1);
  BotFrame := Arguments.__getitem__(2);
  Assert(VarIsPython(Frame) and not VarIsNone(Frame));
  FName := FRemotePython.FromPythonFileName(Frame.f_code.co_filename);
  LineN := Frame.f_lineno;

  TThread.Synchronize(nil, procedure
  begin
    CanBreak :=  (LineN > 0) and
      (not PyIDEOptions.TraceOnlyIntoOpenFiles or
       Assigned(GI_EditorFactory.GetEditorByFileId(FName))) and
       GI_PyIDEServices.ShowFilePosition(FName, LineN, 1, 0, True, False);
  end);

  if CanBreak then
  begin
    ThreadInfo.Status := thrdBroken;
    GetCallStack(ThreadInfo.CallStack, Frame, BotFrame);

    if GI_PyControl.DebuggerState = dsDebugging then
    begin
      GI_PyControl.DebuggerState := dsPaused;
      TThread.Queue(nil, procedure
      begin
        GI_PyInterpreter.AppendPrompt;
      end);
    end;

    // Make sure ThreadChangeNotify runs in the main thread when FExecPaused is False
    while FExecPaused do
    begin
      FRemotePython.CheckConnected(True, True);
      Sleep(20);
    end;
    TPythonThread.Py_Begin_Allow_Threads;
    try
      TThread.Synchronize(nil, procedure
      begin
        TPyBaseDebugger.ThreadChangeNotify(ThreadInfo, tctStatusChange);
      end);
    finally
      TPythonThread.Py_End_Allow_Threads;
    end;

    FDebuggerCommand := dcNone;

    // Return true if broken
    Result := Py.PythonEngine.ReturnTrue;
  end
  else
    Result := Py.PythonEngine.ReturnFalse;
end;

procedure TPyRemDebugger.SyncThreadInfo(Thread_ID: Int64; ThreadName: string; ThreadStatus: Integer);

  function HaveBrokenThread: Boolean;
  begin
    Result := False;
    for var ThreadInfo in FThreads.Values do
      if ThreadInfo.Status = thrdBroken then
      begin
        Result := True;
        Break;
      end;
  end;

var
  ThreadInfo: TThreadInfo;
  OldStatus: TThreadStatus;
begin
  if TThreadStatus(ThreadStatus) = thrdFinished then
  begin
    if FThreads.ContainsKey(Thread_ID) then
    begin
      TPyBaseDebugger.ThreadChangeNotify(FThreads[Thread_ID], tctRemoved);
      FThreads.Remove(Thread_ID);
    end;
  end
  else
  begin
    if FThreads.TryGetValue(Thread_ID, ThreadInfo) then begin
      OldStatus := ThreadInfo.Status;
      ThreadInfo.Status := TThreadStatus(ThreadStatus);
      if OldStatus <> ThreadInfo.Status then
      begin
        TPyBaseDebugger.ThreadChangeNotify(FThreads[Thread_ID], tctStatusChange);
        if OldStatus = thrdBroken then
        begin
          ThreadInfo.CallStack.Clear;
          if not HaveBrokenThread then
            GI_PyControl.DebuggerState := dsDebugging;
        end;
      end;
    end
    else
    begin
      ThreadInfo := TThreadInfo.Create;
      ThreadInfo.Thread_ID := Thread_ID;
      ThreadInfo.Name := ThreadName;
      ThreadInfo.Status :=  TThreadStatus(ThreadStatus);
      FThreads.Add(Thread_ID, ThreadInfo);
      TPyBaseDebugger.ThreadChangeNotify(FThreads[Thread_ID], tctAdded);
    end;
  end;
end;

procedure TPyRemDebugger.UserThread(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
var
  Py: IPyEngineAndGIL;
  Thread_ID: Int64;
  ThreadName: string;
  ThreadStatus: Integer;
begin
  Py := SafePyEngine;
  Result := Py.PythonEngine.ReturnNone;

  Thread_ID := Py.PythonEngine.GetSequenceItem(Args, 0);
  ThreadName := Py.PythonEngine.GetSequenceItem(Args, 1);
  ThreadStatus := Py.PythonEngine.GetSequenceItem(Args, 2);

  // Make sure SyncThreadInfo runs in the main thread when FExecPaused is False
  while FExecPaused do
  begin
    FRemotePython.CheckConnected(True, True);
    Sleep(20);
  end;
  TPythonThread.Py_Begin_Allow_Threads;
  try
    TThread.Synchronize(nil, procedure
    var
      Py: IPyEngineAndGIL;
    begin
      Py := SafePyEngine;
      SyncThreadInfo(Thread_ID, ThreadName, ThreadStatus);
    end);
  finally
    TPythonThread.Py_End_Allow_Threads;
  end;
end;

procedure TPyRemDebugger.UserYield(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
begin
  var Py := SafePyEngine;
  Result := GetPythonEngine.PyLong_FromLong(Ord(FDebuggerCommand));
end;

end.
