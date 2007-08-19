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
  Windows, SysUtils, Classes, uEditAppIntfs, PythonEngine, Forms, Contnrs,
  JvCreateProcess, cTools, cPyBaseDebugger, cPyDebugger, Variants;

type

  TServerType = (stStandard, stTkinter, stWxPython);

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
    fServerType : TServerType;
    fSocketPort: integer;
  public
    constructor Create(AServerType : TServerType = stStandard);
    destructor Destroy; override;
    function CreateAndConnectToServer : Boolean;
    procedure ShutDownServer;
    function Compile(Editor : IEditor) : Variant;
    function ExecuteInThread(Callable, Arguments: Variant) : Variant;
    procedure HandleRemoteException(ExcInfo : Variant; SkipFrames : integer = 1);
    procedure ReInitialize; override;
    procedure CheckConnected;
    procedure ServeConnection;
    // Python Path
    function SysPathAdd(const Path : WideString) : boolean; override;
    function SysPathRemove(const Path : WideString) : boolean; override;
    procedure SysPathToStrings(Strings : TStrings); override;
    procedure StringsToSysPath(Strings : TStrings); override;
    // NameSpace
    function GetGlobals : TBaseNameSpaceItem; override;
    function NameSpaceFromExpression(const Expr : string) : TBaseNameSpaceItem; override;
    function CallTipFromExpression(const Expr : string;
      var DisplayString, DocString : string) : Boolean; override;
    // Service routines
    procedure SetCommandLine(const ScriptName : string); override;
    procedure RestoreCommandLine; override;
    // Main interface
    function ImportModule(Editor : IEditor; AddToNameSpace : Boolean = False) : Variant; override;
    procedure RunNoDebug(Editor : IEditor); override;
    function RunSource(Const Source, FileName : string; symbol : string = 'single') : boolean; override;
    function EvalCode(const Expr : string) : Variant; override;

    property IsAvailable : Boolean read fIsAvailable;
    property IsConnected : Boolean read fIsConnected;
    property Debugger : Variant read fDebugger;
    property ServerType : TServerType read fServerType;
  end;


  TRemNameSpaceItem = class(TNameSpaceItem)
  {
    Implementation of the Base class for the remote debugger
  }
  private
    fRemotePython : TPyRemoteInterpreter;
    fIsProxy : Boolean;
    fObjectType : string; // for caching ObjectType
    fObjectInfo : Variant;
    function ObjectInfoAtIndex(Index: Integer) : Boolean;
  protected
    procedure GetObjectInfo;
    function GetObjectType : string; override;
    function GetValue : string; override;
    function GetDocString : string; override;
    function GetChildCount : integer; override;
  public
    constructor Create(aName : string; aPyObject : Variant;
      RemotePython : TPyRemoteInterpreter);
    function IsClass : Boolean; override;
    function IsDict : Boolean; override;
    function IsModule : Boolean; override;
    function IsFunction : Boolean; override;
    function IsMethod : Boolean; override;
    function Has__dict__ : Boolean; override;
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
    procedure SetCommandLine(const ScriptName : string); override;
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
    function SysPathAdd(const Path : WideString) : boolean; override;
    function SysPathRemove(const Path : WideString) : boolean; override;
    // Debugging
    procedure Run(Editor : IEditor; InitStepIn : Boolean = False); override;
    procedure RunToCursor(Editor : IEditor; ALine: integer); override;
    procedure StepInto(Editor : IEditor); override;
    procedure StepOver; override;
    procedure StepOut; override;
    procedure Pause; override;
    procedure Abort; override;
    // Evaluate expression in the current frame
    procedure Evaluate(const Expr : string; out ObjType, Value : string); override;
    // Like the InteractiveInterpreter runsource but for the debugger frame
    function RunSource(Const Source, FileName : string; symbol : string = 'single') : boolean; override;
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
  VarPyth, StringResources, frmPythonII, Dialogs, JclSysUtils, dmCommands,
  cParameters, uCommonFunctions, frmMessages, frmPyIDEMain, JclFileUtils,
  JvDockControlForm, frmVariables, frmCallStack, frmUnitTests, JclStrings, JvDSADialogs;

{ TRemNameSpaceItem }
constructor TRemNameSpaceItem.Create(aName : string; aPyObject : Variant;
  RemotePython : TPyRemoteInterpreter);
begin
  inherited Create(aName, aPyObject);
  fRemotePython := RemotePython;
  fIsProxy := fRemotePython.Rpyc.isproxy(fPyObject);
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
        Result := fRemotePython.RPI.membercount(fPyObject);
      except
        Result := 0;
      end;
    end;
    fChildCount := Result;
  end;
end;

procedure TRemNameSpaceItem.GetChildNodes;
Var
  Dict, DictKeys, APyObject, PyFullInfo : Variant;
  i : integer;
  Name : string;
  NameSpaceItem : TRemNameSpaceItem;
  SuppressOutput : IInterface;
  FullInfo : Boolean;
begin
  if not (Assigned(fChildNodes) or GotChildNodes) then begin
    fRemotePython.CheckConnected;
    GotChildNodes := True;  //Do not try again
    SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
    FullInfo := False;
    try
      if IsDict then begin
        if IsProxy then begin
          Dict := fRemotePython.RPI.rem_getitemsfullinfo(fPyObject);
          FullInfo := True;
        end else
          Dict := fPyObject;
        fChildCount := len(Dict);
      end else if SortedIdentToInt(ObjectType, i, CommonTypes, True) then
        fChildCount := 0  // do not expand common types
//      else if Has__dict__ then begin
//        Dict := fPyObject.__dict__;
//        fChildCount := len(Dict);
      else begin
        if IsProxy then begin
          Dict := fRemotePython.RPI.safegetmembersfullinfo(fPyObject);
          FullInfo := True;
        end else begin
          Dict := InternalInterpreter.PyInteractiveInterpreter.safegetmembers(fPyObject)
        end;
        fChildCount := len(Dict);
      end;

      if fChildCount > 0 then begin
        fChildNodes := TStringList.Create;
        fChildNodes.CaseSensitive := True;
        DictKeys := Dict.keys();
        for i := 0 to fChildCount - 1 do begin
          Name :=  DictKeys.GetItem(i);
          try
            APyObject := Dict.GetItem(DictKeys.GetItem(i));
            if FullInfo then begin
               PyFullInfo := APyObject;
               APyObject := PyFullInfo.GetItem(0);
            end;
          except
            APyObject := VarPythonCreate('Error in getting object');
            VarClear(PyFullInfo);
          end;
          NameSpaceItem := TRemNameSpaceItem.Create(Name,
            APyObject, fRemotePython);
          if FullInfo and VarIsPython(PyFullInfo) then begin
             NameSpaceItem.BufferedValue := PyFullInfo.GetItem(1);
             NameSpaceItem.GotBufferedValue := True;
             NameSpaceItem.fObjectType := PyFullInfo.GetItem(2);
             NameSpaceItem.fObjectInfo := PyFullInfo.GetItem(3);
             NameSpaceItem.fChildCount := PyFullInfo.GetItem(4);
          end;
          fChildNodes.AddObject(Name, NameSpaceItem);
        end;
        fChildNodes.CustomSort(ComparePythonIdents);
      end;
    except
      fChildCount := 0;
      Dialogs.MessageDlg(Format('Error in getting the namespace of %s', [fName]), mtError, [mbAbort], 0);
      SysUtils.Abort;
    end;
  end;
end;

function TRemNameSpaceItem.ObjectInfoAtIndex(Index: Integer) : Boolean;
begin
  if VarIsEmpty(fObjectInfo) then
    GetObjectInfo;
  if VarIsNull(fObjectInfo) then
    Result := False
  else
    Result := fObjectInfo.GetItem(Index);
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

procedure TRemNameSpaceItem.GetObjectInfo;
var
  SuppressOutput : IInterface;
begin
  fRemotePython.CheckConnected;
  Assert(fIsProxy);
  SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
  try
    fObjectInfo := fRemotePython.RPI.rem_objectinfo(fPyObject);
  except
    fObjectInfo := VarNull;
  end;
end;

function TRemNameSpaceItem.GetObjectType: string;
Var
  SuppressOutput : IInterface;
begin
  if fObjectType <> '' then begin
    Result := fObjectType;
    Exit
  end;
  fRemotePython.CheckConnected;
  SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
  if not IsProxy then
    Result := inherited GetObjectType
  else
    try
      Result := fRemotePython.RPI.objecttype(fPyObject);
    except
      Result := 'Unknown type';
    end;
  fObjectType := Result;
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

function TRemNameSpaceItem.Has__dict__: Boolean;
begin
  if not IsProxy and VarIsEmpty(fObjectInfo) then
    Result := inherited Has__dict__
  else
    Result := ObjectInfoAtIndex(0);
end;

function TRemNameSpaceItem.IsClass: Boolean;
begin
  if not IsProxy and VarIsEmpty(fObjectInfo) then
    Result := inherited IsClass
  else
    Result := ObjectInfoAtIndex(4);
end;

function TRemNameSpaceItem.IsDict: Boolean;
begin
  if not IsProxy then
    Result := inherited IsDict
  else
    Result := ObjectType = 'dict';
end;

function TRemNameSpaceItem.IsFunction: Boolean;
begin
  if not IsProxy and VarIsEmpty(fObjectInfo) then
    Result := inherited IsFunction
  else
    Result := ObjectInfoAtIndex(3);
end;

function TRemNameSpaceItem.IsMethod: Boolean;
begin
  if not IsProxy and VarIsEmpty(fObjectInfo) then
    Result := inherited IsMethod
  else
    Result := ObjectInfoAtIndex(2);
end;

function TRemNameSpaceItem.IsModule: Boolean;
begin
  if not IsProxy and VarIsEmpty(fObjectInfo) then
    Result := inherited IsModule
  else
    Result := ObjectInfoAtIndex(1);
end;


{ TPyRemoteInterpreter }

function TPyRemoteInterpreter.CallTipFromExpression(const Expr: string;
  var DisplayString, DocString: string): Boolean;
var
  LookupObj, PyDocString: Variant;
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
    DisplayString := RPI.get_arg_text(LookupObj);
    Result := True;

    PyDocString := RPI.inspect.getdoc(LookupObj);
    if not VarIsNone(PyDocString) then
      //DocString := GetNthLine(PyDocString, 1)
      DocString := PyDocString
    else
      DocString := '';
  except
  end;
end;

procedure TPyRemoteInterpreter.CheckConnected;
begin
  if not (fIsAvailable and fIsConnected and (ServerProcess.State = psWaiting)) then begin
    Dialogs.MessageDlg('Remote Server is not connected.  Please reinitialize or disconnect the remote  interpreter.',
      mtError, [mbAbort], 0);
    fIsConnected := False;
    VariablesWindow.VariablesTree.Enabled := False;
    fThreadExecInterrupted := True;
    SysUtils.Abort;
  end;
end;

function TPyRemoteInterpreter.Compile(Editor: IEditor): Variant;
Var
  FName, Source : string;
  ExcInfo, Error : Variant;
  FileName : string;
  LineNo, Offset : integer;
begin
  CheckConnected;
  if PyControl.DebuggerState <> dsInactive then begin
    Dialogs.MessageDlg('You cannot compile, import or run modules while debugging or running programs',
      mtError, [mbAbort], 0);
    SysUtils.Abort;
  end;

  VarClear(Result);
  PyControl.ErrorPos.Clear;
  PyControl.DoErrorPosChanged;

  MessagesWindow.ClearMessages;

  FName := Editor.FileName;
  if FName = '' then FName := '<'+Editor.FileTitle+'>';
  Source := CleanEOLs(Editor.EncodedText)+#10;

  try
    Result := RPI.rem_compile(VarPythonCreate(Source), VarPythonCreate(FName));

    GetPythonEngine.CheckError;
    ExcInfo := RPI.exc_info;
    if not VarIsNone(RPI.exc_info) then begin
      VarClear(Result);
      Error := ExcInfo.getitem(1);
      if ExcInfo.getitem(3) then begin
        // SyntaxError
        MessagesWindow.ShowPythonSyntaxError(ExcInfo.getitem(0), Error);

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
      Dialogs.MessageDlg(Format('%s: %s',[ExcInfo.getitem(0), RPI._some_str(Error)]),
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

constructor TPyRemoteInterpreter.Create(AServerType : TServerType = stStandard);
Var
  SuppressOutput : IInterface;
  ServerFile: string;
  ServerSource: string;
  ServerName: string;
begin
  fInterpreterCapabilities := [icReInitialize];
  fServerType := AServerType;
  fSocketPort := 18888;

  ServerProcess := TJvCreateProcess.Create(nil);
  with ServerProcess do
  begin
    Name := 'JvCreateProcess';
    ConsoleOptions := [coRedirect];
    StartupInfo.ForceOnFeedback := False;
    StartupInfo.ForceOffFeedback := True;
  end;

  // Import Rpyc
  SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors

  fOldSysModules := SysModule.modules.copy();
  try
    Rpyc := Import('Rpyc');
    fIsAvailable := True;

    // Check Rpyc version
    if VarModuleHasObject(Rpyc, '__version__') and
        ((Rpyc.__version__[0] <> 2) or (Rpyc.__version__[1] <> 60)) then
    begin
      Dialogs.MessageDlg('Wrong Rpyc version.  To use the remote Python Engine '+
        'download version 2.6 Rpyc from http://Rpyc.sf.net and install it.',
        mtError, [mbAbort], 0);
      fIsAvailable := False;
    end;
  except
    Dialogs.MessageDlg('The Rpyc module is not available.  To use the remote Python Engine '+
    'download Rpyc from http://Rpyc.sf.net and install it.', mtError, [mbAbort], 0);
    fIsAvailable := False;
  end;

  if fIsAvailable then begin
    ServerFile := CommandsDataModule.UserDataDir + 'remserver.py';
    case fServerType of
      stStandard: ServerName := 'SimpleServer';
      stTkinter: ServerName := 'TkServer';
      stWxPython: ServerName := 'WxServer';
    end;
    ServerSource := CommandsDataModule.JvMultiStringHolder.StringsByName[ServerName].Text;
    try
      StringToFile(ServerFile, ServerSource);
    except
      Dialogs.MessageDlg('Could not write file "' + ServerFile +
        '" and cannot use a remote Python engine', mtError, [mbAbort], 0);
      fIsAvailable := False;
    end;
  end;

  if fIsAvailable then begin
    RemoteServer := TExternalTool.Create;
    RemoteServer.CaptureOutput := False;
    RemoteServer.ApplicationName := '$[PythonDir]\pythonw.exe';
    RemoteServer.Parameters := Format('"%s" %d', [ServerFile, fSocketPort]);
//       '$[PythonExe-Short-Path]Lib\site-packages\Rpyc\Servers\threaded_server.py';
//       '"$[PythonExe-Path]Lib\site-packages\Rpyc\Servers\simple_server_wx.py"';
    fIsAvailable := fIsAvailable and CreateAndConnectToServer;
    if not (fIsAvailable and fIsConnected) then
      Dialogs.MessageDlg('Could not connect to the remote Python engine server. '+
        'The remote interpreter and debugger is not available.',
        mtError, [mbAbort], 0)
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
  ServerProcess.Free;
  RemoteServer.Free;

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
end;

function TPyRemoteInterpreter.GetGlobals: TBaseNameSpaceItem;
begin
  CheckConnected;
  Result := TRemNameSpaceItem.Create('globals',
    Evalcode('globals()'), Self);
end;

procedure TPyRemoteInterpreter.HandleRemoteException(ExcInfo: Variant; SkipFrames : integer = 1);
Var
  LineNo : integer;
  FileName : string;
  Traceback : Variant;
begin
  CheckConnected;
  if not VarIsPython(ExcInfo) or VarIsNone(ExcInfo) then Exit;

  Traceback := ExcInfo.getitem(2);
  MessagesWindow.ShowTraceback(Traceback, SkipFrames);
  MessagesWindow.AddMessage(Format('%s: %s',[ExcInfo.getitem(0),
    RPI._some_str(ExcInfo.getitem(1))]));

  if VarIsPython(Traceback) and not VarIsNone(Traceback) then begin
    while not VarIsNone(Traceback.tb_next) do
      Traceback := Traceback.tb_next;
    FileName := Traceback.tb_frame.f_code.co_filename;
    try
      LineNo := Traceback.tb_lineno;
    except
      LineNo := 0;
    end;
    if PyIDEMainForm.ShowFilePosition(FileName, LineNo, 1) and
      Assigned(GI_ActiveEditor)
    then begin
      PyControl.ErrorPos.Editor := GI_ActiveEditor;
      PyControl.ErrorPos.Line := LineNo;
      PyControl.DoErrorPosChanged;
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
begin
  CheckConnected;
  VarClear(Result);
  //Compile
  Code := Compile(Editor);

  Assert(VarIsPython(Code));  // an exception should have been raised if failed

  Path := ExtractFileDir(Editor.FileName);
  if Length(Path) > 1 then begin
    // Add the path of the executed file to the Python path
    PythonPathAdder := AddPathToPythonPath(Path, False);
  end;

  if Editor.FileName <> '' then
    NameOfModule := FileNameToModuleName(Editor.FileName)
  else
    NameOfModule := PathRemoveExtension(Editor.FileTitle);

  PyControl.DoStateChange(dsRunningNoDebug);
  try
    try
      Result := RPI.rem_import(NameOfModule, Code);
      GetPythonEngine.CheckError;
      ExcInfo := RPI.exc_info;
      if not VarIsNone(RPI.exc_info) then begin
        Result := None;
        HandleRemoteException(ExcInfo);
        Dialogs.MessageDlg(Format('%s: %s',[ExcInfo.getitem(0), RPI._some_str(ExcInfo.getitem(1))]),
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
      Dialogs.MessageDlg('Error in importing module', mtError, [mbOK], 0);
      SysUtils.Abort;
    end else if AddToNameSpace then
      RPI.locals.__setitem__(VarPythonCreate(NameOfModule), Result);
  finally
    PyControl.DoStateChange(dsInactive);
  end;
end;

function TPyRemoteInterpreter.NameSpaceFromExpression(const Expr: string): TBaseNameSpaceItem;
var
  LookupObj, ItemsDict, BuiltinDict: Variant;
  SuppressOutput : IInterface;
begin
  CheckConnected;
  SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
  try
    if Expr <> '' then begin
      //Evaluate the lookup expression
      LookupObj := RPI.evalcode(Expr);
      ItemsDict := RPI.safegetmembers(LookUpObj);
      Result := TRemNameSpaceItem.Create(Expr, ItemsDict, Self);
    end else begin
//      BuiltinDict := RPI.ImmDict(Conn.modules.__builtin__.__dict__);
//      BuiltinDict.update(RPI.ImmDict(RPI.evalcode('vars()')));
      BuiltinDict := Conn.modules.__builtin__.__dict__;
      BuiltinDict.update(RPI.evalcode('vars()'));
      Result := TRemNameSpaceItem.Create(Expr, BuiltinDict, Self);
    end;
  except
    ItemsDict := NewPythonDict;
    Result := TRemNameSpaceItem.Create(Expr, ItemsDict, Self);
  end;
end;

procedure TPyRemoteInterpreter.ReInitialize;
Var
  SuppressOutput : IInterface;
begin
  case PyControl.DebuggerState of
    dsRunning, dsRunningNoDebug:
      begin
        fThreadExecInterrupted := True;
        SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
        try
          // This will cause the running thread to exit
          Conn.close();
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
            Dialogs.MessageDlg('Could not connect to the remote Python engine server. '+
              'The remote interpreter and debugger is not available.',
              mtError, [mbAbort], 0)
          else begin
            PythonIIForm.AppendText(WideLineBreak + '*** Remote Interpreter Reinitialized  ***');
            PythonIIForm.AppendPrompt;
            // Recreate the Active debugger
            PyControl.ActiveDebugger := TPyRemDebugger.Create(Self);
            PyControl.DoStateChange(dsInactive);
          end;
        end else
          Dialogs.MessageDlg('Could not shut down the remote Python engine server. '+
            'Please shut down and restart Pyscripter.',
            mtError, [mbAbort], 0);
      end;
    {else
      Exit; }
  end;
end;

procedure TPyRemoteInterpreter.RestoreCommandLine;
begin
  CheckConnected;
  Conn.modules.sys.argv := fOldargv;
end;

procedure TPyRemoteInterpreter.RunNoDebug(Editor: IEditor);
Var
  Code : Variant;
//  AsyncRunNoDebug : Variant;
//  PyAsyncResult : PPyObject;
//  PyArgTuple : PPyObject;
//  AsyncResult : Variant;
  Path, OldPath : WideString;
  PythonPathAdder : IInterface;
  ExcInfo : Variant;
  ReturnFocusToEditor: Boolean;
  CanDoPostMortem : Boolean;
begin
  CheckConnected;
  CanDoPostMortem := False;
  //Compile
  Code := Compile(Editor);

  PyControl.DoStateChange(dsRunningNoDebug);

  // New Line for output
  PythonIIForm.AppendText(sLineBreak);

  Path := ExtractFileDir(Editor.FileName);
  OldPath := RPI.rem_getcwdu();
  if Length(Path) > 1 then begin
    // Add the path of the executed file to the Python path - Will be automatically removed
    PythonPathAdder := AddPathToPythonPath(Path);

    // Change the current path
    try
      RPI.rem_chdir(Path)
    except
      Dialogs.MessageDlg('Could not set the current directory to the script path', mtWarning, [mbOK], 0);
    end;
  end;

  // Set the command line parameters
  SetCommandLine(Editor.GetFileNameOrTitle);

  ReturnFocusToEditor := Editor = GI_ActiveEditor;
  PyIDEMainForm.actNavInterpreterExecute(nil);
  if CommandsDataModule.PyIDEOptions.ClearOutputBeforeRun then
    PythonIIForm.actClearContentsExecute(nil);

  try
    try
      ExecuteInThread(RPI.run_nodebug, VarPythonCreate([Code], stTuple));
      GetPythonEngine.CheckError;
      if not fThreadExecInterrupted then begin
        ExcInfo := RPI.exc_info;
        if not VarIsNone(ExcInfo) then begin
          HandleRemoteException(ExcInfo);
          ReturnFocusToEditor := False;
          Dialogs.MessageDlg(Format('%s: %s',[ExcInfo.getitem(0), RPI._some_str(ExcInfo.getitem(1))]),
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
    if not fThreadExecInterrupted then begin
      // happpens when the remote server was shutdown

      // Restore the command line parameters
      RestoreCommandLine;

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
    end else if CanDoPostMortem and CommandsDataModule.PyIDEOptions.PostMortemOnException then
      PyControl.ActiveDebugger.EnterPostMortem;
  end;
end;

function TPyRemoteInterpreter.RunSource(const Source,
  FileName: string; symbol : string = 'single'): boolean;
//  works asynchronously
Var
  OldDebuggerState : TDebuggerState;
begin
  CheckConnected;
  Assert(not PyControl.IsRunning, 'RunSource called while the Python engine is active');
  OldDebuggerState := PyControl.DebuggerState;
  PyControl.DoStateChange(dsRunningNoDebug);
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
begin
  Result := False;

  Assert(ServerProcess.State = psReady,
    'Trying to create servrer when the server process is busy');

  AppName := AddQuotesUnless(PrepareCommandLine(RemoteServer.ApplicationName));
  Arguments := PrepareCommandLine(RemoteServer.Parameters);
  ServerProcess.WaitForTerminate := RemoteServer.WaitForTerminate;
  // Check whether a process is still running
  Assert(ServerProcess.State = psReady, Format(SInternalError, ['StartServer']));
  with ServerProcess do
  begin
    // According to the Help file it is more robust to add the appname to the command line
    // ApplicationName := AppName;
    CommandLine := Trim(AppName + ' ' + Arguments);
    StartupInfo.DefaultWindowState := True;
    StartupInfo.ShowWindow := swNormal;
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
        Conn := Rpyc.SocketConnection('localhost', fSocketPort);
        Result := True;
        fIsConnected := True;
        break;
      except
        // wait and try again
        Sleep(100);
      end;
    end;

  if Result then begin
    // Redirect Output
    OutputRedirector := Rpyc.RedirectedStd(Conn);
    OutputRedirector.redirect();
    Conn.namespace.__name__ := '__main__';
    // Create the remote interpreter
    Source := CleanEOLs(
      CommandsDataModule.JvMultiStringHolder.StringsByName['Rpyc_Init'].Text)+#10;
    Conn.execute(VarPythonCreate(Source));
    RPI := Conn.namespace._RPI;
    //  pass a reference to the P4D module DebugIDE
    Conn.execute('del _RPI.locals["_RPI"]');
    Rpyc.getattr(RPI, '__class__').debugIDE :=
      InternalInterpreter.PyInteractiveInterpreter.debugIDE;
    // input and raw_input
    Conn.modules.__builtin__.raw_input := InternalInterpreter.PyInteractiveInterpreter.Win32RawInput;

    // make sys.stdout etc asynchronous when writing
    RPI.asyncIO();
    // debugger
    fDebugger := RPI.debugger;
    Rpyc.getattr(fDebugger, '__class__').debugIDE :=
      InternalInterpreter.PyInteractiveInterpreter.debugIDE;
    // Install Rpyc excepthook
    SysModule.excepthook := Rpyc.rpyc_excepthook;
    // Execute PyscripterSetup.py here
    Conn.namespace.pyscripter := Import('pyscripter');

    // sys.displayhook
    RPI.setupdisplayhook();
    GetPythonEngine.CheckError;
  end;
end;

procedure TPyRemoteInterpreter.ServeConnection;
begin
  CheckConnected;
  While Conn.poll() do begin
  end;
end;

procedure TPyRemoteInterpreter.SetCommandLine(const ScriptName: string);
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
  Argv.append(VarPythonCreate(ScriptName));
  S := iff(CommandsDataModule.PyIDEOptions.UseCommandLine,
     CommandsDataModule.PyIDEOptions.CommandLine, '');
  if S <> '' then begin
    S := Parameters.ReplaceInText(S);
    P := GetParamStr(PChar(S), Param);
    while Param <> '' do begin
      Argv.append(VarPythonCreate(Param));
      P := GetParamStr(P, Param);
    end;
    PythonIIForm.AppendText(Format(SCommandLineMsg, [S]));
  end;
end;

procedure TPyRemoteInterpreter.ShutDownServer;
var
  i : integer;
begin
  if PyControl.DebuggerState <> dsInactive then begin
    if Dialogs.MessageDlg('The Python interpreter is busy.  Are you sure you want to terminate it?',
      mtWarning, [mbYes, mbNo], 0) = idNo then Exit;
  end;

  if not (csDestroying in PyIDEMainForm.ComponentState) then begin
    VariablesWindow.ClearAll;
    CallStackWindow.ClearAll;
    UnitTestWindow.ClearAll;
  end;
  // Destroy Remote Debugger
  PyControl.ActiveDebugger := nil;

  VarClear(fOldArgv);
  VarClear(fDebugger);
  VarClear(RPI);
  if VarIsPython(OutputRedirector) then
    OutputRedirector.redirected:= False;
  VarClear(OutputRedirector);
  if ServerProcess.State <> psReady then begin
    if fIsConnected then
      try
        Conn.close();
      except
      end;
    fThreadExecInterrupted := True;
    ServerProcess.Terminate;
    for i := 0 to 20 do
      if ServerProcess.State <> psReady then begin
        // Wait for the threads to terminate
        Application.ProcessMessages;
        CheckSynchronize;
        Sleep(20);
      end else
        break;
    Assert(ServerProcess.State = psReady);
  end;
  VarClear(Conn);
  // Restore excepthook
  SysModule.excepthook := SysModule.__excepthook__;
  fIsConnected := False;
end;

procedure TPyRemoteInterpreter.StringsToSysPath(Strings: TStrings);
var
  i: Integer;
  PythonPath: Variant;
begin
  CheckConnected;
  // could have used Conn.deliver but this was not in version 2.55
  // and would break Python 2.3 compatibility
  Conn.execute('__import__("sys").path = []');
  PythonPath := Conn.modules.sys.path;
  for i := 0 to Strings.Count - 1 do
    PythonPath.append(Strings[i]);
end;

function TPyRemoteInterpreter.SysPathAdd(const Path: WideString): boolean;
begin
  CheckConnected;
  if Conn.modules.sys.path.__contains__(Path) then
    Result := false
  else begin
    Conn.modules.sys.path.insert(0, Path);
    Result := true;
  end;
end;

function TPyRemoteInterpreter.SysPathRemove(const Path: WideString): boolean;
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
  RemPath := Rpyc.obtain(Conn.modules.sys.path);
  for i := 0 to Len(RemPath) - 1  do
    Strings.Add(RemPath.GetItem(i));
end;


{ TPyRemDebugger }

procedure TPyRemDebugger.Abort;
begin
  if PyControl.DebuggerState = dsPostMortem then
    ExitPostMortem
  else
    fDebuggerCommand := dcAbort;
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
    PS1 := '[PM]' + PS1;
    fOldPS2 := PS2;
    PS2 := '[PM]' + PS2;
    AppendPrompt;
  end;

  TraceBack := fRemotePython.Conn.modules.sys.last_traceback;
  fRemotePython.Debugger.botframe := TraceBack.tb_frame;
  while not VarIsNone(TraceBack.tb_next) do
    TraceBack := TraceBack.tb_next;
  fCurrentFrame := TraceBack.tb_frame;

  PyControl.DoStateChange(dsPostMortem);
  DSAMessageDlg(dsaPostMortemInfo, 'PyScripter', SPostMortemInfo,
     mtInformation, [mbOK], 0, dckActiveForm, 0, mbOK);
end;

procedure TPyRemDebugger.Evaluate(const Expr: string; out ObjType,
  Value: string);
Var
  SuppressOutput : IInterface;
  V : Variant;
begin
  ObjType := SNotAvailable;
  Value := SNotAvailable;
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
     
   Assert(VarIsPython(fCurrentFrame));
   FName := fCurrentFrame.f_code.co_filename;
   if (FName[1] ='<') and (FName[Length(FName)] = '>') then
     FName :=  Copy(FName, 2, Length(FName)-2);
   // PythonIIForm.AppendText('UserLine '+ FName + ' ' + IntToStr(Frame.f_lineno) +sLineBreak);

   if PyIDEMainForm.ShowFilePosition(FName, fCurrentFrame.f_lineno, 1) and
     (fCurrentFrame.f_lineno > 0) then
   begin
     PyControl.CurrentPos.Editor := GI_EditorFactory.GetEditorByNameOrTitle(FName);
     PyControl.CurrentPos.Line := fCurrentFrame.f_lineno;

      if PyControl.DebuggerState = dsRunning then
        PyControl.DoStateChange(dsPaused);
     FDebuggerCommand := dcNone;
     While  (FDebuggerCommand = dcNone) and not fRemotePython.fThreadExecInterrupted do
       PyControl.DoYield(True);

     if fRemotePython.fThreadExecInterrupted then
       Exit; 

     if PyControl.BreakPointsChanged then SetDebuggerBreakpoints;

     PyControl.DoStateChange(dsRunning);
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
                       MessagesWindow.AddMessage('Debugging Aborted');
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
    Result := fRemotePython.Conn.execute('hasattr(__import__("sys"), "last_traceback")', 'eval');
  except
  end;
end;

procedure TPyRemDebugger.LoadLineCache;
Var
  i : integer;
  FName, Source : string;
begin
  // inject unsaved code into LineCache
  fLineCache.cache.clear();
  for i := 0 to GI_EditorFactory.Count - 1 do
    with GI_EditorFactory.Editor[i] do
      if HasPythonfile and (FileName = '') then
      begin
        Source := CleanEOLs(EncodedText)+#10;
        FName := '<'+FileTitle+'>';
        fLineCache.cache.__setitem__(VarPythonCreate(FName),
          VarPythonCreate([0, None, Source, FName], stTuple));
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

procedure TPyRemDebugger.Run(Editor: IEditor; InitStepIn: Boolean);
var
  Code : Variant;
  Path, OldPath : string;
  PythonPathAdder : IInterface;
  ExcInfo : Variant;
  ReturnFocusToEditor: Boolean;
  CanDoPostMortem : Boolean;
begin
  fRemotePython.CheckConnected;
  CanDoPostMortem := False;
  fDebuggerCommand := dcRun;
  if (PyControl.DebuggerState <> dsInactive) then exit; // pass control to user_line

  //Compile
  Code := fRemotePython.Compile(Editor);

  if VarIsPython(Code) then begin
    Path := ExtractFilePath(Editor.FileName);
    OldPath := fRemotePython.RPI.rem_getcwdu();
    if Length(Path) > 1 then begin
      Path := PathRemoveSeparator(Path);
      // Add the path of the executed file to the Python path - Will be automatically removed
      PythonPathAdder := AddPathToPythonPath(Path);

      // Change the current path
      try
        fRemotePython.RPI.rem_chdir(Path)
      except
        Dialogs.MessageDlg('Could not set the current directory to the script path', mtWarning, [mbOK], 0);
      end;
    end;

    PyControl.DoStateChange(dsRunning);

    MessagesWindow.ClearMessages;
    ReturnFocusToEditor := Editor = GI_ActiveEditor;
    // Set the layout to the Debug layout is it exists
    if PyIDEMainForm.Layouts.IndexOf('Debug') >= 0 then begin
      PyIDEMainForm.SaveLayout('Current');
      PyIDEMainForm.LoadLayout('Debug');
      Application.ProcessMessages;
    end else
      PyIDEMainForm.actNavInterpreterExecute(nil);
    if CommandsDataModule.PyIDEOptions.ClearOutputBeforeRun then
      PythonIIForm.actClearContentsExecute(nil);

    try
      with PythonIIForm do begin
        fOldPS1 := PS1;
        PS1 := '[Dbg]' + PS1;
        fOldPS2 := PS2;
        PS2 := '[Dbg]' + PS2;
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

      // New Line for output
      PythonIIForm.AppendText(sLineBreak);

      // Set the command line parameters
      SetCommandLine(Editor.GetFileNameOrTitle);

      fRemotePython.Debugger.InitStepIn := InitStepIn;
      try
        fRemotePython.ExecuteInThread(fRemotePython.Debugger.run, VarPythonCreate([Code], stTuple));
        GetPythonEngine.CheckError;
        if not fRemotePython.fThreadExecInterrupted then begin
          ExcInfo := fRemotePython.Debugger.exc_info;
          if not VarIsNone(ExcInfo) then begin
            fRemotePython.HandleRemoteException(ExcInfo, 2);
            ReturnFocusToEditor := False;
            Dialogs.MessageDlg(Format('%s: %s',[ExcInfo.getitem(0),
              fRemotePython.RPI._some_str(ExcInfo.getitem(1))]), mtError, [mbOK], 0);
            CanDoPostMortem := True;
            SysUtils.Abort;
          end;
        end;
        if fRemotePython.Debugger.interrupted then begin
          MessagesWindow.AddMessage('Remote Debugging Aborted');
          MessagesWindow.ShowWindow;
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
      MakeFrameActive(nil);
      with PythonIIForm do begin
        PS1 := fOldPS1;
        PS2 := fOldPS2;
        AppendText(sLineBreak+PS1);
      end;

      if not fRemotePython.fThreadExecInterrupted then begin
        // happpens when the remote server was shutdown

        // Restore the command line parameters
        RestoreCommandLine;

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
        fRemotePython.ReInitialize;
      end else if CanDoPostMortem and CommandsDataModule.PyIDEOptions.PostMortemOnException then
        PyControl.ActiveDebugger.EnterPostMortem;
    end;
  end;
end;

function TPyRemDebugger.RunSource(const Source, FileName: string; symbol : string = 'single'): boolean;
// The internal interpreter RunSource calls II.runsource which differs
// according to whether we debugging or not
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
  // Set Temporary breakpoint
  SetDebuggerBreakPoints;  // So that this one is not cleared
  FName := Editor.FileName;
  if FName = '' then
    FName := '<'+Editor.FileTitle+'>';
  fRemotePython.Debugger.set_break(VarPythonCreate(FName), ALine, 1);

  if PyControl.DebuggerState = dsInactive then
    Run(Editor)
  else
    fDebuggerCommand := dcRunToCursor;
end;

procedure TPyRemDebugger.SetCommandLine(const ScriptName: string);
begin
  fRemotePython.SetCommandLine(ScriptName);
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
              0, TBreakPoint(BreakPoints[j]).Condition);
          end else
            fRemotePython.Debugger.set_break(VarPythonCreate(FName),
              TBreakPoint(BreakPoints[j]).LineNo);
        end;
      end;
    end;
  PyControl.BreakPointsChanged := False;
end;

procedure TPyRemDebugger.StepInto(Editor: IEditor);
begin
  if PyControl.DebuggerState = dsInactive then
    Run(Editor, True)
  else
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

function TPyRemDebugger.SysPathAdd(const Path: WideString): boolean;
begin
  Result := fRemotePython.SysPathAdd(Path);
end;

function TPyRemDebugger.SysPathRemove(const Path: WideString): boolean;
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
  fCurrentFrame := VarPythonCreate(Args).GetItem(0);
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

