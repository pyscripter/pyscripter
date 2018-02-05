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
  System.UITypes, Windows, SysUtils, Classes, uEditAppIntfs, PythonEngine,
  Forms, Contnrs, JvCreateProcess, cTools, cPyBaseDebugger, cPyDebugger,
  Variants, SyncObjs;

type

  TPyRemoteInterpreter = class(TPyBaseInterpreter)
  {
    Rpyc based remote Python Interpreter
  }
  private
    Rpyc : Variant;
    Conn : Variant;
    RPI : Variant; // RemotePythonInterpreter
    OutputRedirector : Variant;
    ServerProcess : TJvCreateProcess;
    RemoteServer : TExternalTool;
    fIsAvailable : Boolean;
    fIsConnected : Boolean;
    fOldargv : Variant;
    fOldsysmodules : Variant;
    fThreadExecInterrupted: Boolean;
    fDebugger : Variant;
    fSocketPort: integer;
    fRpycPath : string;
  protected
    procedure CreateMainModule; override;
  public
    constructor Create(AEngineType : TPythonEngineType = peRemote);
    destructor Destroy; override;
    function CreateAndConnectToServer : Boolean;
    procedure ShutDownServer;
    function Compile(ARunConfig : TRunConfiguration) : Variant;
    function ExecuteInThread(Callable, Arguments: Variant) : Variant;
    procedure HandleRemoteException(ExcInfo : Variant; SkipFrames : integer = 1);
    procedure ReInitialize; override;
    procedure CheckConnected(Quiet : Boolean = False; Abort : Boolean = True);
    procedure ServeConnection;
    // Python Path
    function SysPathAdd(const Path : string) : boolean; override;
    function SysPathRemove(const Path : string) : boolean; override;
    procedure SysPathToStrings(Strings : TStrings); override;
    procedure StringsToSysPath(Strings : TStrings); override;
    // NameSpace
    function GetGlobals : TBaseNameSpaceItem; override;
    procedure GetModulesOnPath(Path : Variant; SL : TStrings); override;
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
    function GetObjectType(Ob : Variant) : string; override;
    function UnitTestResult : Variant; override;
    function NameSpaceItemFromPyObject(aName : string; aPyObject : Variant): TBaseNameSpaceItem; override;

    property IsAvailable : Boolean read fIsAvailable;
    property IsConnected : Boolean read fIsConnected;
    property Debugger : Variant read fDebugger;
  end;


  TRemNameSpaceItem = class(TNameSpaceItem)
  {
    Implementation of the Base class for the remote debugger
  }
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
    constructor Create(aName : string; aPyObject : Variant;
      RemotePython : TPyRemoteInterpreter);
    function IsDict : Boolean; override;
    procedure GetChildNodes; override;
    property IsProxy : Boolean read fIsProxy;
  end;

  TPyRemDebugger = class(TPyBaseDebugger)
  {
    Remote debugger using Rpyc for communication with the server
  }
  private
    fRemotePython : TPyRemoteInterpreter;
    fDebuggerCommand : TDebuggerCommand;
    fCurrentFrame : Variant;
    fLineCache : Variant;
    fOldPS1, fOldPS2 : string;
    procedure HandleUserLine;
  protected
    procedure SetCommandLine(ARunConfig : TRunConfiguration); override;
    procedure RestoreCommandLine; override;
    procedure SetDebuggerBreakpoints; override;
    procedure LoadLineCache;
    procedure UserCall(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
    procedure UserLine(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
    procedure UserReturn(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
    procedure UserException(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
    procedure UserYield(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
    property CurrentFrame : Variant read fCurrentFrame;
  public
    constructor Create(RemotePython : TPyRemoteInterpreter);
    destructor Destroy; override;

    // Python Path
    function SysPathAdd(const Path : string) : boolean; override;
    function SysPathRemove(const Path : string) : boolean; override;
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
    // Fills in CallStackList with TBaseFrameInfo objects
    procedure GetCallStack(CallStackList : TObjectList); override;
    // functions to get TBaseNamespaceItems corresponding to a frame's gloabals and locals
    function GetFrameGlobals(Frame : TBaseFrameInfo) : TBaseNameSpaceItem; override;
    function GetFrameLocals(Frame : TBaseFrameInfo) : TBaseNameSpaceItem; override;
    function NameSpaceFromExpression(const Expr : string) : TBaseNameSpaceItem; override;
    procedure MakeFrameActive(Frame : TBaseFrameInfo); override;
    // post mortem stuff
    function HaveTraceback : boolean; override;
    procedure EnterPostMortem; override;
    procedure ExitPostMortem; override;
  end;

implementation

uses
  VarPyth, StringResources, frmPythonII, Dialogs, dmCommands,
  cParameters, uCommonFunctions, frmMessages, frmPyIDEMain,
  frmVariables, frmCallStack, frmUnitTests, JvDSADialogs,
  JvGnugettext, JclStrings, JclSysUtils, cProjectClasses, JvJCLUtils,
  cRefactoring,
  cPyScripterSettings;

{ TRemNameSpaceItem }
constructor TRemNameSpaceItem.Create(aName : string; aPyObject : Variant;
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
      SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
      try
        Result := fRemotePython.RPI.membercount(fPyObject, True, ExpandCommonTypes, ExpandSequences);
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
    SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
    try
      if IsProxy then
        FullInfoTuple := fRemotePython.RPI.safegetmembersfullinfo(fPyObject, True,
          ExpandCommonTypes, ExpandSequences)
      else
        FullInfoTuple := InternalInterpreter.PyInteractiveInterpreter.safegetmembersfullinfo(fPyObject, True,
          ExpandCommonTypes, ExpandSequences);
      fChildCount := len(FullInfoTuple);

      if fChildCount > 0 then begin
        fChildNodes := TStringList.Create;
        fChildNodes.CaseSensitive := True;

        PyFullInfoTuple := ExtractPythonObjectFrom(FullInfoTuple);
        for i := 0 to fChildCount - 1 do with GetPythonEngine do begin
          PyMemberInfo := PyTuple_GetItem(PyFullInfoTuple, i);
          ObjName :=  PyString_AsWideString(PyTuple_GetItem(PyMemberInfo, 0));
          PyFullInfo := PyTuple_GetItem(PyMemberInfo, 1);
          APyObject := VarPythonCreate(PyTuple_GetItem(PyFullInfo, 0));

          NameSpaceItem := TRemNameSpaceItem.Create(ObjName, APyObject, fRemotePython);
          NameSpaceItem.ExpandCommonTypes := ExpandCommonTypes;
          NameSpaceItem.ExpandSequences := ExpandSequences;

          NameSpaceItem.BufferedValue := PyString_AsWideString(PyTuple_GetItem(PyFullInfo, 1));
          NameSpaceItem.GotBufferedValue := True;
          NameSpaceItem.fObjectType := PyString_AsWideString(PyTuple_GetItem(PyFullInfo, 2));
          NameSpaceItem.ExtractObjectInfo(PyTuple_GetItem(PyFullInfo, 3));
          if NameSpaceItem.IsProxy then
            NameSpaceItem.fChildCount := PyInt_AsLong(PyTuple_GetItem(PyFullInfo, 4))
          else
            NameSpaceItem.fChildCount := NameSpaceItem.ChildCount;

          fChildNodes.AddObject(ObjName, NameSpaceItem);
        end;
        GetPythonEngine.CheckError;
        if (ObjectType <> 'list') and (ObjectType <> 'tuple') then
          fChildNodes.CustomSort(ComparePythonIdents);
      end;
    except
      fChildCount := 0;
      Dialogs.MessageDlg(Format(_(SErrorGettingNamespace), [fName]), mtError, [mbAbort], 0);
      SysUtils.Abort;
    end;
  end;
end;

function TRemNameSpaceItem.GetDocString: string;
Var
  SuppressOutput : IInterface;
begin
  fRemotePython.CheckConnected;
  SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
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
  PyObjectInfo : PPyObject;
begin
  if not IsProxy then
    inherited
  else begin
    fRemotePython.CheckConnected;
    SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
    try
      PyObjectInfo := ExtractPythonObjectFrom(fRemotePython.RPI.objectinfo(fPyObject));
    except
      PyObjectInfo := nil;
    end;
    ExtractObjectInfo(PyObjectInfo);
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
  SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
  if not IsProxy then
    Result := inherited GetValue
  else
    try
      Result :=  fRemotePython.RPI.saferepr(fPyObject);
    except
      Result := '';
    end;
end;

function TRemNameSpaceItem.IsDict: Boolean;
begin
  if not IsProxy then
    Result := VarIsPythonDict(fPyObject)
  else
    Result := ObjectInfo.IsDict;
end;


{ TPyRemoteInterpreter }

function TPyRemoteInterpreter.CallTipFromExpression(const Expr: string;
  var DisplayString, DocString: string): Boolean;
var
  LookupObj, ArgText, PyDocString: Variant;
  SuppressOutput : IInterface;
begin
  CheckConnected;
  Result := False;
  DisplayString := '';
  DocString := '';
  if Expr = '' then Exit;

  SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
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
  if not (fIsAvailable and fIsConnected and (ServerProcess.State = psWaiting)) then begin
    fIsConnected := False;
    if not Quiet then
      Dialogs.MessageDlg(_(SRemoteServerNotConnected),
        mtError, [mbAbort], 0);
    VariablesWindow.VariablesTree.Enabled := False;
    fThreadExecInterrupted := True;
    if Abort then
      SysUtils.Abort;
  end;
end;

function TPyRemoteInterpreter.Compile(ARunConfig: TRunConfiguration): Variant;
Var
  FName : string;
  Source : Variant;
  ExcInfo, Error : Variant;
  FileName : string;
  LineNo, Offset : integer;
  Editor : IEditor;
begin
  CheckConnected;
  if PyControl.DebuggerState <> dsInactive then begin
    Dialogs.MessageDlg(_(SCannotCompileWhileRunning),
      mtError, [mbAbort], 0);
    SysUtils.Abort;
  end;

  VarClear(Result);
  PyControl.ErrorPos.Clear;
  PyControl.DoErrorPosChanged;

  MessagesWindow.ClearMessages;

  Editor := GI_EditorFactory.GetEditorByNameOrTitle(ARunConfig.ScriptName);

  if Assigned(Editor) then begin
    FName := Editor.FileName;
    if FName = '' then FName := '<'+Editor.FileTitle+'>';
    if GetPythonEngine.IsPython3000 then
      Source := CleanEOLs(Editor.SynEdit.Text) + WideLF
    else
      Source := CleanEOLs(Editor.EncodedText)+#10;
  end else begin
    try
      FName := ARunConfig.ScriptName;
      if GetPythonEngine.IsPython3000 then
        Source := CleanEOLs(FileToStr(FName)) + WideLF
      else
        Source := CleanEOLs(FileToEncodedStr(FName)) + AnsiChar(#10);
    except
      on E: Exception do begin
        Dialogs.MessageDlg(Format(_(SFileOpenError), [FName, E.Message]), mtError, [mbOK], 0);
        SysUtils.Abort;
      end;
    end;
  end;

  try
    // if IsPython3000 then Source is a WideStrings else it is an encoded string
    Result := RPI.rem_compile(VarPythonCreate(Source), VarPythonCreate(FName));

    GetPythonEngine.CheckError;
    ExcInfo := RPI.exc_info;
    if not VarIsNone(RPI.exc_info) then begin
      VarClear(Result);
      Error := ExcInfo.__getitem__(1);
      if ExcInfo.__getitem__(3) then begin
        // SyntaxError
        MessagesWindow.ShowPythonSyntaxError(ExcInfo.__getitem__(0), Error);

        ExtractPyErrorInfo(Error, FileName, LineNo, Offset);
        if PyIDEMainForm.ShowFilePosition(FileName, LineNo, Offset) and
          Assigned(GI_ActiveEditor)
        then begin
          PyControl.ErrorPos.Editor := GI_ActiveEditor;
          PyControl.ErrorPos.Line := LineNo;
          PyControl.ErrorPos.Char := Offset;
          PyControl.ErrorPos.IsSyntax := True;
          PyControl.DoErrorPosChanged;
        end;
      end else
        HandleRemoteException(ExcInfo);
      Dialogs.MessageDlg(Format('%s: %s',
        [VarPythonAsString(ExcInfo.__getitem__(0)), VarPythonAsString(RPI.safestr(Error))]),
        mtError, [mbOK], 0);
      SysUtils.Abort;
    end;
  except
    on E: EPythonError do begin  //may raise OverflowError or ValueError
      // New Line for output
      PythonIIForm.AppendText(sLineBreak);
      VarClear(Result);

      HandlePyException(E);
      Dialogs.MessageDlg(E.Message, mtError, [mbOK], 0);
      SysUtils.Abort;
    end;
    else begin
      VarClear(Result);
      raise;
    end;
  end;
end;

constructor TPyRemoteInterpreter.Create(AEngineType : TPythonEngineType = peRemote);
Var
  SuppressOutput : IInterface;
  ServerFile: string;
  ServerSource: string;
  ServerName: string;
begin
  fInterpreterCapabilities := [icReInitialize];
  fEngineType := AEngineType;

  Randomize;
  fSocketPort := 18000 + Random(1000);  //18888

  ServerProcess := TJvCreateProcess.Create(nil);
  with ServerProcess do
  begin
    Name := 'PyScripterServerProcess';
    ConsoleOptions := [coRedirect];
    CreationFlags := CreationFlags +[cfCreateNoWindow];
    StartupInfo.ForceOnFeedback := False;
    StartupInfo.ForceOffFeedback := True;
    StartupInfo.DefaultWindowState := True;
    StartupInfo.ShowWindow := swNormal;
  end;

  // Import Rpyc
  SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors

  fOldSysModules := SysModule.modules.copy();
  try
    fRpycPath := Format('%sLib\rpyc.zip', [ExtractFilePath(Application.ExeName)]);
    InternalInterpreter.SysPathAdd(fRpycPath);
    Rpyc := Import('rpyc');
    fIsAvailable := True;
  except
    Dialogs.MessageDlg(_(SRpycNotAvailable), mtError, [mbAbort], 0);
    fIsAvailable := False;
  end;

  if fIsAvailable then begin
    ServerFile := CommandsDataModule.UserDataPath + 'remserver.py';
    case fEngineType of
      peRemote: ServerName := 'SimpleServer';
      peRemoteTk: ServerName := 'TkServer';
      peRemoteWx: ServerName := 'WxServer';
    else
      raise Exception.Create('Invalid Engine type in TPyRemoteInterpreter constructor');
    end;
    ServerSource := CommandsDataModule.JvMultiStringHolder.StringsByName[ServerName].Text;
    //  Python 3000 fix
    if (FEngineType = peRemoteTk) and GetPythonEngine.IsPython3000 then
      ServerSource :=	 StringReplace(ServerSource, 'Tkinter', 'tkinter', [rfReplaceAll]);
    try
      StringToFile(ServerFile, AnsiString(ServerSource));
    except
      Dialogs.MessageDlg(Format(_(SCouldNotWriteServerFile), [ServerFile]), mtError, [mbAbort], 0);
      fIsAvailable := False;
    end;
  end;

  if fIsAvailable then begin
    RemoteServer := TExternalTool.Create;
    RemoteServer.CaptureOutput := False;
    RemoteServer.ApplicationName := '$[PythonDir]\python.exe';
    RemoteServer.Parameters := Format('"%s" %d "%s"', [ServerFile, fSocketPort, fRpycPath]);
    try
      fIsAvailable := fIsAvailable and CreateAndConnectToServer;
    except
      on E: Exception do begin
        fIsAvailable := False;
        Dialogs.MessageDlg(_(SErrorCreatingRemoteEngine) + E.Message, mtError, [mbAbort], 0);
      end;
    end;
    if not (fIsAvailable and fIsConnected) then
      Dialogs.MessageDlg(_(SCouldNotConnectRemoteEngine), mtError, [mbAbort], 0)
  end;
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
  FreeAndNil(ServerProcess);
  FreeAndNil(RemoteServer);

  InternalInterpreter.SysPathRemove(fRpycPath);
  inherited;
end;

function TPyRemoteInterpreter.EvalCode(const Expr: string): Variant;
begin
  // may raise exceptions
  if not fIsConnected or fThreadExecInterrupted then
    Result := None
  else
    Result := RPI.evalcode(Expr);
end;

function TPyRemoteInterpreter.ExecuteInThread(Callable, Arguments : Variant): Variant;
// Coded in Python.dll calls to avoid VarPyth error printing
Var
  PyExecInThread : PPyObject;
  PyResult : PPyObject;
  PyArgTuple : PPyObject;
begin
  CheckConnected;
  // Starts the processing
  fThreadExecInterrupted := False;
  InternalInterpreter.PyInteractiveInterpreter.thread_id := 0;
  PyExecInThread := ExtractPythonObjectFrom(
    InternalInterpreter.PyInteractiveInterpreter.execInThread);

  PyArgTuple := ExtractPythonObjectFrom(VarPythonCreate([Callable, Arguments], stTuple));

  PyResult := nil;
  with GetPythonEngine do begin
    Py_XINCREF(PyExecInThread);
    Py_XINCREF(PyArgTuple);
    try
      PyResult :=
        PyObject_CallObject(PyExecInThread, PyArgTuple);

      if Assigned(PyResult) then
        Result := VarPythonCreate(PyResult)
      else
        Result := None;

    finally
      Py_XDECREF(PyExecInThread);
      Py_XDECREF(PyArgTuple);
      Py_XDECREF(PyResult);
    end;
  end;

  if fThreadExecInterrupted then begin
    GetPythonEngine.PyErr_Clear;
    PythonIIForm.ClearPendingMessages;
  end else begin
    PythonIIForm.WritePendingMessages;
    GetPythonEngine.CheckError;
  end;
  InternalInterpreter.PyInteractiveInterpreter.thread_id := 0;
end;

function TPyRemoteInterpreter.GetGlobals: TBaseNameSpaceItem;
begin
  CheckConnected(True);
  Result := TRemNameSpaceItem.Create('globals',
    Evalcode('globals()'), Self);
end;

procedure TPyRemoteInterpreter.GetModulesOnPath(Path: Variant; SL: TStrings);
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
  Result := InternalInterpreter.GetObjectType(ob);
end;

procedure TPyRemoteInterpreter.HandleRemoteException(ExcInfo: Variant; SkipFrames : integer = 1);
Var
  LineNo : integer;
  FileName : string;
  Editor : IEditor;
  Traceback : Variant;
begin
  CheckConnected;
  if not VarIsPython(ExcInfo) or VarIsNone(ExcInfo) then Exit;

  Traceback := ExcInfo.__getitem__(2);
  MessagesWindow.ShowTraceback(Traceback, SkipFrames);
  MessagesWindow.AddMessage(Format('%s: %s',
            [VarPythonAsString(ExcInfo.__getitem__(0)),
             VarPythonAsString(RPI.safestr(ExcInfo.__getitem__(1)))]));

  if VarIsPython(Traceback) and not VarIsNone(Traceback) then begin
    while not VarIsNone(Traceback.tb_next) do
      Traceback := Traceback.tb_next;
    FileName := Traceback.tb_frame.f_code.co_filename;
    try
      LineNo := Traceback.tb_lineno;
    except
      LineNo := 0;
    end;

    if (FileName[1] ='<') and (FileName[Length(FileName)] = '>') then
      FileName :=  Copy(FileName, 2, Length(FileName)-2);
    Editor := GI_EditorFactory.GetEditorByNameOrTitle(FileName);
    // Check whether the error occurred in the active editor
    if (Assigned(Editor) and (Editor = PyIDEMainForm.GetActiveEditor)) or
      PyIDEOptions.JumpToErrorOnException then
    begin
      if PyIDEMainForm.ShowFilePosition(FileName, LineNo, 1) and
        Assigned(GI_ActiveEditor)
      then begin
        PyControl.ErrorPos.Editor := GI_ActiveEditor;
        PyControl.ErrorPos.Line := LineNo;
        PyControl.DoErrorPosChanged;
      end;
    end;
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
  ExcInfo : Variant;
  RunConfiguration : TRunConfiguration;
begin
  Assert(Assigned(Editor));
  CheckConnected;
  VarClear(Result);
  //Compile
  RunConfiguration := TRunConfiguration.Create;
  try
    RunConfiguration.ScriptName := Editor.GetFileNameOrTitle;
    Code := Compile(RunConfiguration);
  finally
    RunConfiguration.Free;
  end;

  Assert(VarIsPython(Code));  // an exception should have been raised if failed

  Path := ExtractFileDir(Editor.FileName);
  if Length(Path) > 1 then begin
    // Add the path of the executed file to the Python path
    PythonPathAdder := AddPathToPythonPath(Path, False);
    SysPathRemove('');
  end;

  if Editor.FileName <> '' then
    NameOfModule := FileNameToModuleName(Editor.FileName)
  else
    NameOfModule := ChangeFileExt(Editor.FileTitle, '');

  PyControl.DoStateChange(dsRunning);
  try
    try
      Result := RPI.rem_import(NameOfModule, Code);
      GetPythonEngine.CheckError;
      ExcInfo := RPI.exc_info;
      if not VarIsNone(RPI.exc_info) then begin
        Result := None;
        HandleRemoteException(ExcInfo);
        Dialogs.MessageDlg(Format('%s: %s',
          [VarPythonAsString(ExcInfo.__getitem__(0)), VarPythonAsString(RPI.safestr(ExcInfo.__getitem__(1)))]),
          mtError, [mbOK], 0);
        SysUtils.Abort;
      end;
    except
      on E: EPythonError do begin
        HandlePyException(E);
        Result := None;
      end;
    end;
    if VarIsNone(Result) then begin
      Dialogs.MessageDlg(_(SErrorInImportingModule), mtError, [mbOK], 0);
      SysUtils.Abort;
    end else if AddToNameSpace then
      RPI.locals.__setitem__(VarPythonCreate(NameOfModule), Result);
  finally
    //  Add again the empty path
    SysPathAdd('');

    PyControl.DoStateChange(dsInactive);
  end;
end;

function TPyRemoteInterpreter.NameSpaceFromExpression(const Expr: string): TBaseNameSpaceItem;
var
  LookupObj, ItemsDict: Variant;
  SuppressOutput : IInterface;
begin

  CheckConnected;
  SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
  try
    if Expr <> '' then begin
      //Evaluate the lookup expression
      LookupObj := RPI.evalcode(Expr);
      //Result := TRemNameSpaceItem.Create(Expr, LookupObj, Self);
      ItemsDict := RPI.safegetmembers(LookUpObj);
      Result := TRemNameSpaceItem.Create(Expr, ItemsDict, Self);
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
    ItemsDict := NewPythonDict;
    Result := TRemNameSpaceItem.Create(Expr, ItemsDict, Self);
  end;
end;

function TPyRemoteInterpreter.NameSpaceItemFromPyObject(aName: string;
  aPyObject: Variant): TBaseNameSpaceItem;
begin
  Result := TRemNameSpaceItem.Create(aName, aPyObject, Self);
end;

procedure TPyRemoteInterpreter.ReInitialize;
Var
  SuppressOutput : IInterface;
begin
  case PyControl.DebuggerState of
    dsDebugging, dsRunning:
      begin
        SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
        try
          // raise an asynchronous exception in the running thread
          if InternalInterpreter.PyInteractiveInterpreter.thread_id <> 0 then
            with GetPythonEngine do begin
              if PyThreadState_SetAsyncExc(InternalInterpreter.PyInteractiveInterpreter.thread_id,
                PyExc_KeyboardInterrupt^) > 1
//                PyExc_SystemExit^) > 1
              then  // call again with exception set to nil
                PyThreadState_SetAsyncExc(InternalInterpreter.PyInteractiveInterpreter.thread_id,
                  nil);
            end;
//          // This will cause the running thread to exit
//          CallStackWindow.ClearAll;
//          VariablesWindow.ClearAll;
//          fIsConnected := False;
//          if VarIsPython(OutputRedirector) then
//            OutputRedirector._restored := True;
//          VarClear(OutputRedirector);
//          Conn.close();
          ShutDownServer;
          if ServerProcess.State <> psReady then
            Dialogs.MessageDlg(_(SCouldNotShutDownRemoteEngine), mtError, [mbAbort], 0)
          else
            fThreadExecInterrupted := True;
        except
          // swalow exceptions
        end;
        GetPythonEngine.PyErr_Clear;
      end;
    dsInactive:
      begin
        ShutDownServer;
        if ServerProcess.State = psReady then begin
          fIsAvailable := fIsAvailable and CreateAndConnectToServer;
          if not (fIsAvailable and fIsConnected) then
            Dialogs.MessageDlg(_(SCouldNotConnectRemoteEngine), mtError, [mbAbort], 0)
          else begin
            PythonIIForm.AppendText(sLineBreak + _(SRemoteInterpreterInit));
            PythonIIForm.AppendPrompt;
            // Recreate the Active debugger
            PyControl.ActiveDebugger := TPyRemDebugger.Create(Self);

            // Add extra project paths
            if Assigned(ActiveProject) then
              ActiveProject.AppendExtraPaths;

            PyControl.DoStateChange(dsInactive);
          end;
        end else
          Dialogs.MessageDlg(_(SCouldNotShutDownRemoteEngine), mtError, [mbAbort], 0);
      end;
    else
      // Should not happen.  Reinitialise is not enabled for other states
      Exit;
  end;
end;

procedure TPyRemoteInterpreter.RestoreCommandLine;
begin
  CheckConnected;
  Conn.modules.sys.argv := fOldargv;
end;

procedure TPyRemoteInterpreter.Run(ARunConfig: TRunConfiguration);
Var
  Code : Variant;
//  AsyncRunNoDebug : Variant;
//  PyAsyncResult : PPyObject;
//  PyArgTuple : PPyObject;
//  AsyncResult : Variant;
  Path, OldPath : string;
  PythonPathAdder : IInterface;
  ExcInfo : Variant;
  ReturnFocusToEditor: Boolean;
  CanDoPostMortem : Boolean;
  Editor : IEditor;
begin
  CheckConnected;
  CanDoPostMortem := False;
  //Compile
  Code := Compile(ARunConfig);

  if VarIsPython(Code) then begin
    PyControl.DoStateChange(dsRunning);

    // New Line for output
    PythonIIForm.AppendText(sLineBreak);

    Path := ExtractFileDir(ARunConfig.ScriptName);
    SysPathRemove('');
    if Length(Path) > 1 then
      // Add the path of the executed file to the Python path - Will be automatically removed
      PythonPathAdder := AddPathToPythonPath(Path);
    if ARunConfig.WorkingDir <> '' then
      Path := Parameters.ReplaceInText(ARunConfig.WorkingDir);
    OldPath := RPI.rem_getcwdu();

    // Change the current path
    try
      RPI.rem_chdir(Path)
    except
      Dialogs.MessageDlg(_(SCouldNotSetCurrentDir), mtWarning, [mbOK], 0);
    end;

    // Set the command line parameters
    SetCommandLine(ARunConfig);

    Editor := GI_ActiveEditor;
    ReturnFocusToEditor := Assigned(Editor);
    PyIDEMainForm.actNavInterpreterExecute(nil);

    try
      try
        ExecuteInThread(RPI.run_nodebug, VarPythonCreate([Code], stTuple));
        GetPythonEngine.CheckError;
        if not fThreadExecInterrupted then begin
          ExcInfo := RPI.exc_info;
          if not VarIsNone(ExcInfo) then begin
            HandleRemoteException(ExcInfo);
            ReturnFocusToEditor := False;
            Dialogs.MessageDlg(Format('%s: %s',
              [VarPythonAsString(ExcInfo.__getitem__(0)),
               VarPythonAsString(RPI.safestr(ExcInfo.__getitem__(1)))]),
              mtError, [mbOK], 0);
            CanDoPostMortem := True;
            SysUtils.Abort;
          end;
        end;
      except
        on E: EPythonError do begin
          // should not happen
          if not fThreadExecInterrupted then begin
            HandlePyException(E);
            Dialogs.MessageDlg(E.Message, mtError, [mbOK], 0);
          end;
          SysUtils.Abort;
        end;
      end;
    finally
      PythonIIForm.AppendPrompt;
      CheckConnected(True, False);
      if not fThreadExecInterrupted then begin
        // happpens when the remote server was shutdown

        // Restore the command line parameters
        RestoreCommandLine;

        //  Add again the empty path
        SysPathAdd('');

        // Change the back current path
        RPI.rem_chdir(OldPath);
      end;
      PyControl.DoStateChange(dsInactive);
      if ReturnFocusToEditor then
        Editor.Activate;
      if fThreadExecInterrupted then begin
        PythonPathAdder := nil;
        PythonIIForm.ClearPendingMessages;
        ReInitialize;
      end else if CanDoPostMortem and PyIDEOptions.PostMortemOnException then
        PyControl.ActiveDebugger.EnterPostMortem;
    end;
  end;
end;

function TPyRemoteInterpreter.RunSource(Const Source, FileName : Variant; symbol : string = 'single') : boolean;
//  works asynchronously
Var
  OldDebuggerState : TDebuggerState;
begin
  CheckConnected;
  Assert(not PyControl.IsRunning, 'RunSource called while the Python engine is active');
  OldDebuggerState := PyControl.DebuggerState;
  PyControl.DoStateChange(dsRunning);
  try
    Result := ExecuteInThread(RPI.runsource, VarPythonCreate([Source, FileName, symbol], stTuple));
  finally
    PyControl.DoStateChange(OldDebuggerState);
  end;
end;

function TPyRemoteInterpreter.CreateAndConnectToServer : Boolean;
var
  Arguments: string;
  AppName: string;
  SuppressOutput : IInterface;
  I: Integer;
  Source : string;
  InitScriptName : string;
  PyScripterMod, PySource, debugIDE : Variant;
begin
  Result := False;

  Assert(ServerProcess.State = psReady,
    'Trying to create servrer when the server process is busy');

  AppName := AddQuotesUnless(PrepareCommandLine(RemoteServer.ApplicationName));
  Arguments := PrepareCommandLine(RemoteServer.Parameters);
  ServerProcess.WaitForTerminate := RemoteServer.WaitForTerminate;

  // Repeat here to make sure it is set right
  MaskFPUExceptions(PyIDEOptions.MaskFPUExceptions);

  // Check whether a process is still running
  Assert(ServerProcess.State = psReady, Format(_(SInternalError), ['StartServer']));
  with ServerProcess do
  begin
    // According to the Help file it is more robust to add the appname to the command line
    // ApplicationName := AppName;
    CommandLine := Trim(AppName + ' ' + Arguments);
    if RemoteServer.UseCustomEnvironment then
      Environment.Assign(RemoteServer.Environment);
    // Execute Process
    Run;
  end;
  Sleep(100);  // give it sometime

  // Now try to connect to it
  SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
  if ServerProcess.State = psReady then
    Result := False // S
  else
    for i := 1 to 10 do begin
      try
        Conn := Rpyc.classic.connect('localhost', fSocketPort);
        Result := True;
        fIsConnected := True;
        fThreadExecInterrupted := False;
        break;
      except
        // wait and try again
        Sleep(100);
      end;
    end;

  if Result then begin
    // Redirect Output
    OutputRedirector := Rpyc.classic.redirected_stdio(Conn);
/////////////////////////////////////////////////////
//    Conn.modules('sys').modules.__setitem__('__oldmain__', Conn.modules('sys').modules.__getitem__('__main__'));
//    Conn.modules('sys').modules.__setitem__('__main__', Conn.modules('imp').new_module('__main__'));
//    Conn.remote_conn._local_namespace := Conn.modules('sys').modules.__getitem__('__main__').__dict__;
//    Conn.namespace := Rpyc.Lib.AttrFrontend(Conn.remote_conn._local_namespace);
/////////////////////////////////////////////////////
//    Conn.namespace.__name__ := '__main__';
    // Create the remote interpreter
    with GetPythonEngine do begin
      if IsPython3000 then
        InitScriptName := 'Rpyc_Init3000'
      else
        InitScriptName := 'Rpyc_Init';
    end;

    Source := CleanEOLs(
      CommandsDataModule.JvMultiStringHolder.StringsByName[InitScriptName].Text)+#10;
    PySource := VarPythonCreate(Source);
    Conn.execute(PySource);
    RPI := Conn.namespace.__getitem__('_RPI');
    //  pass a reference to the P4D module DebugIDE
    Conn.namespace.__delitem__('_RPI');
//    Conn.execute('del _RPI.locals["_RPI"]');
//    Rpyc.getattr(RPI, '__class__').debugIDE :=
//      InternalInterpreter.PyInteractiveInterpreter.debugIDE;
    debugIDE := InternalInterpreter.PyInteractiveInterpreter.debugIDE;
    RPI.__class__.debugIDE := debugIDE;

    // make sys.stdout etc asynchronous when writing
    RPI.asyncIO();
    // debugger
    fDebugger := RPI.debugger;
    fDebugger.__class__.debugIDE := debugIDE;
//    // Install Rpyc excepthook gets automatically installed
//    Rpyc.install_rpyc_excepthook;
    // Execute PyscripterSetup.py here
    PyScripterMod := Import('pyscripter');
    Conn.namespace.__setitem__('pyscripter', PyScripterMod);

    // sys.displayhook
    RPI.setupdisplayhook();
    GetPythonEngine.CheckError;

    Initialize;
  end;
end;

procedure TPyRemoteInterpreter.CreateMainModule;
begin
  fMainModule := TModuleProxy.CreateFromModule(Conn.modules.__main__, self);
end;

procedure TPyRemoteInterpreter.ServeConnection;
begin
  CheckConnected;
  While Conn.poll() do begin
  end;
end;

procedure TPyRemoteInterpreter.SetCommandLine(ARunConfig : TRunConfiguration);
var
  ArgV : Variant;
  S, Param : string;
  P : PChar;
begin
  CheckConnected;
  fOldargv := Conn.modules.sys.argv;
  Conn.execute('__import__("sys").argv = []');
  Argv := Conn.modules.sys.argv;
  // Workaround due to PREFER_UNICODE flag to make sure
  // no conversion to Unicode and back will take place
  if GetPythonEngine.IsPython3000 then        // Issue 425
    Argv.append(VarPythonCreate(ARunConfig.ScriptName))
  else
    Argv.append(VarPythonCreate(AnsiString(ARunConfig.ScriptName)));

  S := ARunConfig.Parameters;
  if Trim(S) <> '' then begin
    S := Parameters.ReplaceInText(S);
    P := PChar(S);
    while P[0] <> #0 do begin
      P := GetParamStr(P, Param);
      if GetPythonEngine.IsPython3000 then
        Argv.append(VarPythonCreate(Param))
      else
        Argv.append(VarPythonCreate(AnsiString(Param)))
    end;
    PythonIIForm.AppendText(Format(_(SCommandLineMsg), [S]));
  end;
end;

procedure TPyRemoteInterpreter.ShutDownServer;
var
  i : integer;
  OldState : TDebuggerState;
  OldExceptHook : Variant;
begin
//  if PyControl.DebuggerState <> dsInactive then begin
//    if Dialogs.MessageDlg('The Python interpreter is busy.  Are you sure you want to terminate it?',
//      mtWarning, [mbYes, mbNo], 0) = idNo then Exit;
//  end;

  if not (csDestroying in PyIDEMainForm.ComponentState) then begin
    VariablesWindow.ClearAll;
    CallStackWindow.ClearAll;
    UnitTestWindow.ClearAll;
  end;
  // Do not destroy Remote Debugger
  // PyControl.ActiveDebugger := nil;

  FreeAndNil(fMainModule);
  VarClear(fOldArgv);
  VarClear(fDebugger);
  VarClear(RPI);
  if VarIsPython(OutputRedirector) then
    OutputRedirector._restored:= True;
  VarClear(OutputRedirector);
  if ServerProcess.State <> psReady then begin
    if fIsConnected then
      try
        Conn.close();
      except
      end;

    OldState := PyControl.DebuggerState;
    PyControl.DoStateChange(dsDebugging);  // So that we do not reenter run-debug commands
    try
      fThreadExecInterrupted := True;
      ServerProcess.TerminateTree;
      for i := 0 to 100 do
        if ServerProcess.State <> psReady then begin
          // Wait for the threads to terminate
          Application.ProcessMessages;
          CheckSynchronize;
          Sleep(20);
        end else
          break;
      ServerProcess.ConsoleOutput.Clear;
    finally
      PyControl.DoStateChange(OldState);
    end;
  end;
  VarClear(Conn);
  // Restore excepthook
  OldExceptHook := Varpyth.SysModule.__excepthook__;
  SysModule.excepthook := OldExceptHook;
  fIsConnected := False;
end;

procedure TPyRemoteInterpreter.StringsToSysPath(Strings: TStrings);
var
  i: Integer;
  PythonPath: Variant;
begin
  CheckConnected;
  // could have used Conn.deliver
  Conn.execute('__import__("sys").path = []');
  PythonPath := Conn.modules.sys.path;
  for i := 0 to Strings.Count - 1 do
    PythonPath.append(Strings[i]);
end;

function TPyRemoteInterpreter.SysPathAdd(const Path: string): boolean;
begin
  CheckConnected;
  if Conn.modules.sys.path.__contains__(Path) then
    Result := false
  else begin
    Conn.modules.sys.path.insert(0, Path);
    Result := true;
  end;
end;

function TPyRemoteInterpreter.SysPathRemove(const Path: string): boolean;
begin
  if fThreadExecInterrupted then begin
    Result := False;
    Exit;
  end;
  CheckConnected;
  if Conn.modules.sys.path.__contains__(Path) then begin
    Result := True;
    Conn.modules.sys.path.remove(Path);
  end else
    Result := False;
end;

procedure TPyRemoteInterpreter.SysPathToStrings(Strings: TStrings);
var
  i: Integer;
  RemPath : Variant;
begin
  CheckConnected;
  RemPath := Rpyc.classic.obtain(Conn.modules.sys.path);
  for i := 0 to Len(RemPath) - 1  do
    Strings.Add(RemPath.__getitem__(i));
end;


function TPyRemoteInterpreter.UnitTestResult: Variant;
begin
  Result := RPI.IDETestResult();
  Result.debugIDE := RPI.debugIDE;
end;

{ TPyRemDebugger }
function CtrlHandler( fdwCtrlType : dword): LongBool; stdcall;
begin
  Result := True;
end;


procedure TPyRemDebugger.Abort;
Var
  AttachConsole: Function (dwProcessId: longword): LongBool; stdCall;
begin
  case PyControl.DebuggerState of
    dsPostMortem: ExitPostMortem;
    dsDebugging,
    dsRunning:
      begin
        AttachConsole := GetProcAddress (GetModuleHandle ('kernel32.dll'), 'AttachConsole');
        if Assigned(AttachConsole) then
        try
          OSCheck(AttachConsole(fRemotePython.ServerProcess.ProcessInfo.dwProcessId));
          OSCheck(SetConsoleCtrlHandler(@CtrlHandler, True));
          try
            OSCheck(GenerateConsoleCtrlEvent(CTRL_C_EVENT, 0));
            Sleep(100);
          finally
            OSCheck(SetConsoleCtrlHandler(@CtrlHandler, False));
            OSCheck(FreeConsole);
          end;
        except
        end;
      end;
    dsPaused: fDebuggerCommand := dcAbort;
  end;
end;

constructor TPyRemDebugger.Create(RemotePython: TPyRemoteInterpreter);
begin
  inherited Create;
  fRemotePython := RemotePython;
  fLineCache := fRemotePython.Conn.modules.linecache;
  fDebuggerCommand := dcNone;
  PyControl.BreakPointsChanged := True;
end;

destructor TPyRemDebugger.Destroy;
begin
  with PythonIIForm.DebugIDE.Events do begin
    Items[0].OnExecute := nil;
    Items[1].OnExecute := nil;
    Items[2].OnExecute := nil;
    Items[3].OnExecute := nil;
    Items[4].OnExecute := nil;
  end;

  inherited;
end;

procedure TPyRemDebugger.EnterPostMortem;
Var
  TraceBack : Variant;
begin
  if not (HaveTraceback and (PyControl.DebuggerState = dsInactive)) then
    Exit;
  with PythonIIForm do begin
    fOldPS1 := PS1;
    PS1 := PMPrefix + PS1;
    fOldPS2 := PS2;
    PS2 := PMPrefix + PS2;
    AppendPrompt;
  end;

  TraceBack := fRemotePython.Conn.modules.sys.last_traceback;
  fRemotePython.Debugger.botframe := TraceBack.tb_frame;
  while not VarIsNone(TraceBack.tb_next) do
    TraceBack := TraceBack.tb_next;
  fCurrentFrame := TraceBack.tb_frame;

  PyControl.DoStateChange(dsPostMortem);
  DSAMessageDlg(dsaPostMortemInfo, 'PyScripter', _(SPostMortemInfo),
     mtInformation, [mbOK], 0, dckActiveForm, 0, mbOK);
end;

function TPyRemDebugger.Evaluate(const Expr: string): TBaseNamespaceItem;
Var
  SuppressOutput : IInterface;
  V : Variant;
begin
  Result := nil;
  if PyControl.DebuggerState in [dsPaused, dsPostMortem] then begin
    SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
    try
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
  SuppressOutput : IInterface;
  V : Variant;
begin
  ObjType := _(SNotAvailable);
  Value := _(SNotAvailable);
  if PyControl.DebuggerState in [dsPaused, dsPostMortem] then begin
    SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
    try
      // evalcode knows we are in the debugger and uses current frame locals/globals
      V := fRemotePython.RPI.evalcode(Expr);
      ObjType := fRemotePython.RPI.objecttype(V);
      Value := fRemotePython.RPI.saferepr(V);
    except
      // fail quitely
    end;
  end;
end;

procedure TPyRemDebugger.ExitPostMortem;
begin
  with PythonIIForm do begin
    PS1 := fOldPS1;
    PS2 := fOldPS2;
    AppendText(sLineBreak+PS1);
  end;
  VarClear(fCurrentFrame);
  MakeFrameActive(nil);
  PyControl.DoStateChange(dsInactive);
end;

procedure TPyRemDebugger.GetCallStack(CallStackList: TObjectList);
Var
  Frame : Variant;
begin
  CallStackList.Clear;
  if not (PyControl.DebuggerState in [dsPaused, dsPostMortem]) then
    Exit;

  Frame := CurrentFrame;
  if VarIsPython(Frame) then
    while not VarIsNone(Frame.f_back) and not VarIsNone(Frame.f_back.f_back) do begin
      CallStackList.Add(TFrameInfo.Create(Frame));
      Frame := Frame.f_back;
      if VarIsSame(Frame, fRemotePython.Debugger.botframe) then
        break;
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

procedure TPyRemDebugger.HandleUserLine;
Var
  FName : string;
begin
   fRemotePython.CheckConnected;
   if fRemotePython.fThreadExecInterrupted then
     Exit;

   Assert(VarIsPython(fCurrentFrame) and not VarisNone(fCurrentFrame));
   FName := fCurrentFrame.f_code.co_filename;
   if (FName[1] ='<') and (FName[Length(FName)] = '>') then
     FName :=  Copy(FName, 2, Length(FName)-2);
   // PythonIIForm.AppendText('UserLine '+ FName + ' ' + IntToStr(Frame.f_lineno) +sLineBreak);

   if PyIDEMainForm.ShowFilePosition(FName, fCurrentFrame.f_lineno, 1, 0, True, False) and
     (fCurrentFrame.f_lineno > 0) then
   begin
     PyControl.CurrentPos.Editor := GI_EditorFactory.GetEditorByNameOrTitle(FName);
     PyControl.CurrentPos.Line := fCurrentFrame.f_lineno;

      if PyControl.DebuggerState = dsDebugging then
        PyControl.DoStateChange(dsPaused);
     FDebuggerCommand := dcNone;
     While  (FDebuggerCommand = dcNone) and not fRemotePython.fThreadExecInterrupted do
       PyControl.DoYield(True);

     if fRemotePython.fThreadExecInterrupted then
       Exit;

     if PyControl.BreakPointsChanged then SetDebuggerBreakpoints;

     PyControl.DoStateChange(dsDebugging);
   end;

   case fDebuggerCommand of
     dcRun         : fRemotePython.Debugger.set_continue();
     dcStepInto    : fRemotePython.Debugger.set_step();
     dcStepOver    : fRemotePython.Debugger.set_next(fCurrentFrame);
     dcStepOut     : fRemotePython.Debugger.set_return(fCurrentFrame);
     dcRunToCursor : fRemotePython.Debugger.set_continue;
     dcPause       : fRemotePython.Debugger.set_step();
     dcAbort       : begin
                       fRemotePython.Debugger.set_quit();
                       MessagesWindow.AddMessage(_(SDebuggingAborted));
                       MessagesWindow.ShowWindow;
                     end;
   end;
   VarClear(fCurrentFrame);
end;

function TPyRemDebugger.HaveTraceback: boolean;
begin
  Result := False;
  with fRemotePython do
    if not (IsAvailable and IsConnected and (ServerProcess.State = psWaiting)) then
      Exit;
  try
    Result := fRemotePython.Conn.eval('hasattr(__import__("sys"), "last_traceback")');
  except
  end;
end;

procedure TPyRemDebugger.LoadLineCache;
Var
  i : integer;
  FName, Source, LineList : Variant;
begin
  // inject unsaved code into LineCache
  fLineCache.cache.clear();
  for i := 0 to GI_EditorFactory.Count - 1 do
    with GI_EditorFactory.Editor[i] do
      if HasPythonfile and (FileName = '') then
      begin
        if GetPythonEngine.IsPython3000 then
          Source := CleanEOLs(SynEdit.Text)+WideLF
        else
          Source := CleanEOLs(EncodedText)+#10;
        LineList := VarPythonCreate(Source);
        LineList := fRemotePython.Rpyc.classic.deliver(fRemotePython.Conn, LineList.splitlines(True));
        FName := '<'+FileTitle+'>';
        fLineCache.cache.__setitem__(VarPythonCreate(FName),
          VarPythonCreate([0, None, LineList, FName], stTuple));
      end;
end;

procedure TPyRemDebugger.MakeFrameActive(Frame: TBaseFrameInfo);
begin
  if Assigned(Frame) then
    fRemotePython.Debugger.currentframe := (Frame as TFrameInfo).PyFrame
  else
    fRemotePython.Debugger.currentframe := None;
end;

function TPyRemDebugger.NameSpaceFromExpression(const Expr: string): TBaseNameSpaceItem;
//  The internal interpreter knows whether we are debugging and
//  adjusts accordingly (in II.debugger.evalcode)
begin
  Result := fRemotePython.NameSpaceFromExpression(Expr);
end;

procedure TPyRemDebugger.Pause;
begin
  fDebuggerCommand := dcPause;
end;

procedure TPyRemDebugger.RestoreCommandLine;
begin
  fRemotePython.RestoreCommandLine;
end;

procedure TPyRemDebugger.Resume;
begin
  fDebuggerCommand := dcRun;
end;

procedure TPyRemDebugger.Debug(ARunConfig: TRunConfiguration;
  InitStepIn: Boolean = False; RunToCursorLine : integer = -1);
var
  Code : Variant;
//  AsyncRun, AsyncResult : Variant;
  Path, OldPath : string;
  PythonPathAdder : IInterface;
  ExcInfo : Variant;
  ReturnFocusToEditor: Boolean;
  CanDoPostMortem : Boolean;
  Editor : IEditor;
begin
  fRemotePython.CheckConnected;
  CanDoPostMortem := False;
  fDebuggerCommand := dcRun;
  Assert(PyControl.DebuggerState = dsInactive);

  //Compile
  Code := fRemotePython.Compile(ARunConfig);

  if VarIsPython(Code) then begin
    Path := ExtractFileDir(ARunConfig.ScriptName);
    SysPathRemove('');
    if Length(Path) > 1 then
      // Add the path of the executed file to the Python path - Will be automatically removed
      PythonPathAdder := AddPathToPythonPath(Path);
    if ARunConfig.WorkingDir <> '' then
      Path := Parameters.ReplaceInText(ARunConfig.WorkingDir);
    OldPath := fRemotePython.RPI.rem_getcwdu();

    // Change the current path
    try
      fRemotePython.RPI.rem_chdir(Path)
    except
      Dialogs.MessageDlg(_(SCouldNotSetCurrentDir), mtWarning, [mbOK], 0);
    end;

    PyControl.DoStateChange(dsDebugging);

    MessagesWindow.ClearMessages;
    Editor := GI_ActiveEditor;
    ReturnFocusToEditor := Assigned(Editor);
    // Set the layout to the Debug layout is it exists
    if PyIDEMainForm.Layouts.IndexOf('Debug') >= 0 then begin
      PyIDEMainForm.SaveLayout('Current');
      PyIDEMainForm.LoadLayout('Debug');
      Application.ProcessMessages;
    end else
      PyIDEMainForm.actNavInterpreterExecute(nil);

    try
      with PythonIIForm do begin
        fOldPS1 := PS1;
        PS1 := DebugPrefix + PS1;
        fOldPS2 := PS2;
        PS2 := DebugPrefix + PS2;
        AppendPrompt;
      end;
      //attach debugger callback routines
      with PythonIIForm.DebugIDE.Events do begin
        Items[0].OnExecute := UserCall;
        Items[1].OnExecute := UserLine;
        Items[2].OnExecute := UserReturn;
        Items[3].OnExecute := UserException;
        Items[4].OnExecute := UserYield;
      end;

      //set breakpoints
      SetDebuggerBreakPoints;
      if RunToCursorLine >= 0 then  // add temp breakpoint
        fRemotePython.Debugger.set_break(Code.co_filename, RunToCursorLine, 1);

      // New Line for output
      PythonIIForm.AppendText(sLineBreak);

      // Set the command line parameters
      SetCommandLine(ARunConfig);

      fRemotePython.Debugger.InitStepIn := InitStepIn;
      try
        fRemotePython.ExecuteInThread(fRemotePython.Debugger.run, VarPythonCreate([Code], stTuple));
//        AsyncRun := fRemotePython.Rpyc.async(fRemotePython.Debugger.run);
//        AsyncResult := AsyncRun.__call__(Code);
//        try
//          While not (fRemotePython.fThreadExecInterrupted) do
//            if  AsyncResult._is_ready then
//              break
//            else begin
//              fRemotePython.Conn.poll(0);
//              PyControl.DoYield(False);
//            end;
//        except
//        end;

        GetPythonEngine.CheckError;
        if not fRemotePython.fThreadExecInterrupted then begin
          ExcInfo := fRemotePython.Debugger.exc_info;
          if not VarIsNone(ExcInfo) then begin
            fRemotePython.HandleRemoteException(ExcInfo, 2);
            ReturnFocusToEditor := False;
            Dialogs.MessageDlg(Format('%s: %s',
              [VarPythonAsString(ExcInfo.__getitem__(0)),
               VarPythonAsString(fRemotePython.RPI.safestr(ExcInfo.__getitem__(1)))]),
               mtError, [mbOK], 0);
            CanDoPostMortem := True;
            SysUtils.Abort;
          end;
        end;
      except
        // CheckError already called by VarPyth
        on E: EPythonError do begin
          // should not happen
          if not fRemotePython.fThreadExecInterrupted then begin
            fRemotePython.HandlePyException(E);
            Dialogs.MessageDlg(E.Message, mtError, [mbOK], 0);
          end;
          SysUtils.Abort;
        end;
      end;

    finally
      VarClear(fCurrentFrame);
      with PythonIIForm do begin
        PS1 := fOldPS1;
        PS2 := fOldPS2;
        AppendText(sLineBreak+PS1);
      end;

      fRemotePython.CheckConnected(True, False);
      if not fRemotePython.fThreadExecInterrupted then begin
        // happpens when the remote server was shutdown
        MakeFrameActive(nil);

        // Restore the command line parameters
        RestoreCommandLine;

        //  Add again the empty path
        SysPathAdd('');

        // Change the back current path
        fRemotePython.RPI.rem_chdir(OldPath);
      end;

      if PyIDEMainForm.Layouts.IndexOf('Debug') >= 0 then
        PyIDEMainForm.LoadLayout('Current');

      PyControl.DoStateChange(dsInactive);
      if ReturnFocusToEditor then
        Editor.Activate;
      if fRemotePython.fThreadExecInterrupted then begin
        PythonPathAdder := nil;
        PythonIIForm.ClearPendingMessages;
        //fRemotePython.ReInitialize;
        // Reinitialize destroys the debugger which executes this method!
        // So handle with a PostMessage
        PostMessage(PythonIIForm.Handle, WM_REINITINTERPRETER, 0, 0);
      end else if CanDoPostMortem and PyIDEOptions.PostMortemOnException then
        PyControl.ActiveDebugger.EnterPostMortem;
    end;
  end;
end;

function TPyRemDebugger.RunSource(Const Source, FileName : Variant; symbol : string = 'single') : boolean;
// The internal interpreter RunSource calls II.runsource which differs
// according to whether we are debugging or not
Var
  OldCurrentPos : TEditorPos;
begin
  OldCurrentPos := TEditorPos.Create;
  OldCurrentPos.Assign(PyControl.CurrentPos);
  try
    Result := fRemotePython.RunSource(Source, FileName, symbol);
    PyControl.CurrentPos.Assign(OldCurrentPos);
    PyControl.DoCurrentPosChanged;
  finally
    OldCurrentPos.Free;
  end;
end;

procedure TPyRemDebugger.RunToCursor(Editor: IEditor; ALine: integer);
Var
  FName : string;
begin
  Assert(PyControl.DebuggerState = dsPaused);
  // Set Temporary breakpoint
  SetDebuggerBreakPoints;  // So that this one is not cleared
  FName := Editor.FileName;
  if FName = '' then
    FName := '<'+Editor.FileTitle+'>';
  fRemotePython.Debugger.set_break(VarPythonCreate(FName), ALine, 1);

  fDebuggerCommand := dcRunToCursor;
end;

procedure TPyRemDebugger.SetCommandLine(ARunConfig : TRunConfiguration);
begin
  fRemotePython.SetCommandLine(ARunConfig);
end;

procedure TPyRemDebugger.SetDebuggerBreakpoints;
var
  i, j : integer;
  FName : string;
begin
  if not PyControl.BreakPointsChanged then Exit;
  LoadLineCache;
  fRemotePython.Debugger.clear_all_breaks();
  for i := 0 to GI_EditorFactory.Count - 1 do
    with GI_EditorFactory.Editor[i] do begin
      FName := FileName;
      if FName = '' then
        FName := '<'+FileTitle+'>';
      for j := 0 to BreakPoints.Count - 1 do begin
        if not TBreakPoint(BreakPoints[j]).Disabled then begin
          if TBreakPoint(BreakPoints[j]).Condition <> '' then begin
            fRemotePython.Debugger.set_break(VarPythonCreate(FName),
              TBreakPoint(BreakPoints[j]).LineNo,
              0, VarPythonCreate(TBreakPoint(BreakPoints[j]).Condition));
          end else
            fRemotePython.Debugger.set_break(VarPythonCreate(FName),
              TBreakPoint(BreakPoints[j]).LineNo);
        end;
      end;
    end;
  PyControl.BreakPointsChanged := False;
end;

procedure TPyRemDebugger.StepInto;
begin
  fDebuggerCommand := dcStepInto;
end;

procedure TPyRemDebugger.StepOut;
begin
  fDebuggerCommand := dcStepOut;
end;

procedure TPyRemDebugger.StepOver;
begin
  fDebuggerCommand := dcStepOver;
end;

function TPyRemDebugger.SysPathAdd(const Path: string): boolean;
begin
  Result := fRemotePython.SysPathAdd(Path);
end;

function TPyRemDebugger.SysPathRemove(const Path: string): boolean;
begin
  Result := fRemotePython.SysPathRemove(Path);
end;

procedure TPyRemDebugger.UserCall(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
// Not used
begin
   Result := GetPythonEngine.ReturnNone;
end;

procedure TPyRemDebugger.UserException(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
// Not used
begin
   Result := GetPythonEngine.ReturnNone;
end;

procedure TPyRemDebugger.UserLine(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
Var
  SaveThreadState: PPyThreadState;
  NotInMainThread : Boolean;
begin
  fCurrentFrame := VarPythonCreate(Args).__getitem__(0);
  NotInMainThread := GetCurrentThreadId <> MainThreadID;
  with GetPythonEngine do begin
    Result := ReturnNone;
    if NotInMainThread then begin
      SaveThreadState := PyEval_SaveThread();
      try
        TThread.Synchronize(nil, HandleUserLine);
      finally
        PyEval_RestoreThread(SaveThreadState);
      end;
    end else
      HandleUserLine;
  end;
end;

procedure TPyRemDebugger.UserReturn(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
// Not used
begin
   Result := GetPythonEngine.ReturnNone;
end;

procedure TPyRemDebugger.UserYield(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
begin
  if fDebuggerCommand = dcAbort then begin
    fRemotePython.Debugger.set_quit();
  end else if fDebuggerCommand = dcPause then
    fRemotePython.Debugger.set_step();
  Result := GetPythonEngine.ReturnNone;
end;

end.
