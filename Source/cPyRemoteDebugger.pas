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
  Vcl.Forms,
  JvCreateProcess,
  PythonEngine,
  uEditAppIntfs,
  cTools,
  cPySupportTypes,
  cPyBaseDebugger,
  cPyDebugger;

type

  {
    Rpyc based remote Python Interpreter
  }
  TPyRemoteInterpreter = class(TPyBaseInterpreter)
  private
    Rpyc : Variant;
    Conn : Variant;
    RPI : Variant; // RemotePythonInterpreter
    OutputRedirector : Variant;
    ServerProcess : TJvCreateProcess;
    RemoteServer : TExternalTool;
    fIsAvailable : Boolean;
    fConnected : Boolean;
    fOldargv : Variant;
    fOldsysmodules : Variant;
    fSocketPort: integer;
    fRpycPath : string;
    procedure CreateServerProcess;
  protected
    procedure CreateMainModule; override;
  public
    constructor Create(AEngineType : TPythonEngineType = peRemote);
    destructor Destroy; override;
    function CreateAndConnectToServer : Boolean;
    procedure ShutDownServer;
    function Compile(ARunConfig : TRunConfiguration) : Variant;
    procedure HandleRemoteException(ExcInfo : Variant; SkipFrames : integer = 1);
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
    property Connected : Boolean read fConnected;
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
    procedure GetChildNodes; override;
    property IsProxy : Boolean read fIsProxy;
  end;

  TPyRemDebugger = class(TPyBaseDebugger)
  {
    Remote debugger using Rpyc for communication with the server
  }
  private
    fRemotePython : TPyRemoteInterpreter;
    fDebugManager : Variant;
    fMainDebugger : Variant;
    fDebuggerCommand : TDebuggerCommand;
    fLineCache : Variant;
    fMainThread : TThreadInfo;
    fThreads : TObjectDictionary<integer, TThreadInfo>;
    fOldPS1, fOldPS2 : string;
  protected
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
  Vcl.Dialogs,
  VarPyth,
  JclStrings,
  JclSysUtils,
  JvJCLUtils,
  JvDSADialogs,
  JvGnugettext,
  StringResources,
  dmCommands,
  frmPythonII,
  frmMessages,
  frmPyIDEMain,
  frmVariables,
  frmCallStack,
  frmUnitTests,
  cProjectClasses,
  cParameters,
  cRefactoring,
  cPyScripterSettings,
  cPyControl,
  uCommonFunctions,
  cInternalPython;

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
        FullInfoTuple := TPyInternalInterpreter(PyControl.InternalInterpreter).PyInteractiveInterpreter.safegetmembersfullinfo(fPyObject, True,
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
          NameSpaceItem.fObjectInfo := PyInt_AsLong(PyTuple_GetItem(PyFullInfo, 3));
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
      Vcl.Dialogs.MessageDlg(Format(_(SErrorGettingNamespace), [fName]), mtError, [mbAbort], 0);
      System.SysUtils.Abort;
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
begin
  if not IsProxy then
    inherited
  else begin
    fRemotePython.CheckConnected;
    SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
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
  if not (fIsAvailable and fConnected and (ServerProcess.State = psWaiting)) then begin
    fConnected := False;
    if not Quiet then
      Vcl.Dialogs.MessageDlg(_(SRemoteServerNotConnected),
        mtError, [mbAbort], 0);
    VariablesWindow.VariablesTree.Enabled := False;
    if Abort then
      System.SysUtils.Abort;
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
    Vcl.Dialogs.MessageDlg(_(SCannotCompileWhileRunning),
      mtError, [mbAbort], 0);
    System.SysUtils.Abort;
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
        Vcl.Dialogs.MessageDlg(Format(_(SFileOpenError), [FName, E.Message]), mtError, [mbOK], 0);
        System.SysUtils.Abort;
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
        ExtractPyErrorInfo(Error, FileName, LineNo, Offset);
        if PyIDEMainForm.ShowFilePosition(FileName, LineNo, Offset) and
          Assigned(GI_ActiveEditor)
        then begin
          PyControl.ErrorPos.NewPos(GI_ActiveEditor, LineNo, Offset, True);
          PyControl.DoErrorPosChanged;
        end;
      end else
        HandleRemoteException(ExcInfo);

      PythonIIForm.AppendPrompt;
      Vcl.Dialogs.MessageDlg(Format('%s: %s',
        [VarPythonAsString(ExcInfo.__getitem__(0)), VarPythonAsString(RPI.safestr(Error))]),
        mtError, [mbOK], 0);
      System.SysUtils.Abort;
    end;
  except
    on E: EPythonError do begin  //may raise OverflowError or ValueError
      // New Line for output
      PythonIIForm.AppendText(sLineBreak);
      VarClear(Result);

      HandlePyException(E);
      Vcl.Dialogs.MessageDlg(E.Message, mtError, [mbOK], 0);
      System.SysUtils.Abort;
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
  fSocketPort := 18000 + Random(1000);
  CreateServerProcess;

  // Import Rpyc
  SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors

  fOldSysModules := SysModule.modules.copy();
  try
    fRpycPath := Format('%sLib\rpyc.zip', [ExtractFilePath(Application.ExeName)]);
    PyControl.InternalInterpreter.SysPathAdd(fRpycPath);
    Rpyc := Import('rpyc');
    fIsAvailable := True;
  except
    Vcl.Dialogs.MessageDlg(_(SRpycNotAvailable), mtError, [mbAbort], 0);
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
      Vcl.Dialogs.MessageDlg(Format(_(SCouldNotWriteServerFile), [ServerFile]), mtError, [mbAbort], 0);
      fIsAvailable := False;
    end;
  end;

  if fIsAvailable then begin
    RemoteServer := TExternalTool.Create;
    RemoteServer.CaptureOutput := False;
    RemoteServer.ApplicationName := '$[PythonDir]python.exe';
    RemoteServer.Parameters := Format('"%s" %d "%s"', [ServerFile, fSocketPort, fRpycPath]);
    try
      fIsAvailable := fIsAvailable and CreateAndConnectToServer;
    except
      on E: Exception do begin
        fIsAvailable := False;
        Vcl.Dialogs.MessageDlg(_(SErrorCreatingRemoteEngine) + E.Message, mtError, [mbAbort], 0);
      end;
    end;
    if not (fIsAvailable and fConnected) then
      Vcl.Dialogs.MessageDlg(_(SCouldNotConnectRemoteEngine), mtError, [mbAbort], 0)
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

  PyControl.InternalInterpreter.SysPathRemove(fRpycPath);
  inherited;
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
  Result := PyControl.InternalInterpreter.GetObjectType(ob);
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
        PyControl.ErrorPos.NewPos(GI_ActiveEditor, LineNo);
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
        Vcl.Dialogs.MessageDlg(Format('%s: %s',
          [VarPythonAsString(ExcInfo.__getitem__(0)), VarPythonAsString(RPI.safestr(ExcInfo.__getitem__(1)))]),
          mtError, [mbOK], 0);
        System.SysUtils.Abort;
      end;
    except
      on E: EPythonError do begin
        HandlePyException(E);
        Result := None;
      end;
    end;
    if VarIsNone(Result) then begin
      Vcl.Dialogs.MessageDlg(_(SErrorInImportingModule), mtError, [mbOK], 0);
      System.SysUtils.Abort;
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

procedure TPyRemoteInterpreter.CreateServerProcess;
begin
  ServerProcess := TJvCreateProcess.Create(nil);
  with ServerProcess do
  begin
    Name := 'PyScripterServerProcess';
    ConsoleOptions := [coRedirect];
    CreationFlags := CreationFlags + [cfCreateNoWindow];
    StartupInfo.ForceOnFeedback := False;
    StartupInfo.ForceOffFeedback := True;
    StartupInfo.DefaultWindowState := True;
    StartupInfo.ShowWindow := swNormal;
  end;
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
          ShutDownServer;  // sets fIsConnected to False
          if ServerProcess.State <> psReady then
            Vcl.Dialogs.MessageDlg(_(SCouldNotShutDownRemoteEngine), mtError, [mbAbort], 0);
        except
          // swalow exceptions
        end;
        GetPythonEngine.PyErr_Clear;
        //  Running/Debugging will detect that fIsConnected is false and
        //  finish run/debug orderly and post a WM_REINITINTERPRETER message
        //  So we should not reinitialize here.
      end;
    dsInactive:
      begin
        ShutDownServer;
        if ServerProcess.State = psReady then begin
          fIsAvailable := fIsAvailable and CreateAndConnectToServer;
          if not (fIsAvailable and fConnected) then
            Vcl.Dialogs.MessageDlg(_(SCouldNotConnectRemoteEngine), mtError, [mbAbort], 0)
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
          Vcl.Dialogs.MessageDlg(_(SCouldNotShutDownRemoteEngine), mtError, [mbAbort], 0);
      end;
//    else
//      // Should not happen.  Reinitialise is not enabled for other states
//      Exit;
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
  AsyncRun : Variant;
  AsyncResult : Variant;
  AsyncReady : boolean;
  Path, OldPath : string;
  PythonPathAdder : IInterface;
  ExcInfo : Variant;
  ReturnFocusToEditor: Boolean;
  CanDoPostMortem : Boolean;
  Editor : IEditor;
  Timer : ITimer;
begin
  CheckConnected;
  CanDoPostMortem := False;
  //Compile
  Code := Compile(ARunConfig);
  if not VarIsPython(Code) then Exit;

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
    Vcl.Dialogs.MessageDlg(_(SCouldNotSetCurrentDir), mtWarning, [mbOK], 0);
  end;

  PyControl.DoStateChange(dsRunning);

  // New Line for output
  PythonIIForm.AppendText(sLineBreak);

  // Set the command line parameters
  SetCommandLine(ARunConfig);

  Editor := GI_ActiveEditor;
  ReturnFocusToEditor := Assigned(Editor);
  PyIDEMainForm.actNavInterpreterExecute(nil);

  AsyncRun := Rpyc.asynch(RPI.run_nodebug);
  AsyncResult := AsyncRun.__call__(Code);
  GetPythonEngine.CheckError;
  Timer := NewTimer(50);
  Timer.Start(procedure
  begin
    // Do not reenter
    Timer.Stop;

    try
      ServeConnection(500);
    except
      fConnected := False;
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
          ExcInfo := RPI.exc_info;
          if not VarIsNone(ExcInfo) then begin
            HandleRemoteException(ExcInfo);
            ReturnFocusToEditor := False;
            Vcl.Dialogs.MessageDlg(Format('%s: %s',
              [VarPythonAsString(ExcInfo.__getitem__(0)),
               VarPythonAsString(RPI.safestr(ExcInfo.__getitem__(1)))]),
              mtError, [mbOK], 0);
            CanDoPostMortem := True;
          end;
        end;
      except
        on E: EPythonError do begin
          // should not happen
          CheckConnected(True, False);
          if Connected then begin
            HandlePyException(E);
            Vcl.Dialogs.MessageDlg(E.Message, mtError, [mbOK], 0);
          end;
        end;
      end;
    finally
      PythonIIForm.AppendPrompt;
      CheckConnected(True, False);
      if Connected then begin
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
      if not Connected then begin
        PythonPathAdder := nil;
        PythonIIForm.ClearPendingMessages;
        PostMessage(PythonIIForm.Handle, WM_REINITINTERPRETER, 0, 0);
      end else if CanDoPostMortem and PyIDEOptions.PostMortemOnException then
        PyControl.ActiveDebugger.EnterPostMortem;
      Timer := nil;
    end
    else
      Timer.Restart;
  end);
end;

function TPyRemoteInterpreter.RunSource(Const Source, FileName : Variant; symbol : string = 'single') : boolean;
//  works asynchronously
Var
  AsyncRun : Variant;
  AsyncResult : Variant;
  AsyncReady : boolean;
  OldDebuggerState : TDebuggerState;
begin
  CheckConnected;
  Assert(not PyControl.Running, 'RunSource called while the Python engine is active');
  OldDebuggerState := PyControl.DebuggerState;
  PyControl.DoStateChange(dsRunning);

  AsyncRun := Rpyc.asynch(RPI.runsource);
  AsyncResult := AsyncRun.__call__(VarPythonCreate(Source), VarPythonCreate(FileName), VarPythonCreate(symbol));
  GetPythonEngine.CheckError;
  AsyncReady := False;
  try
    while Connected and not AsyncReady do
    begin
      PyControl.DoYield(False);
      try
        ServeConnection(500);
      except
        fConnected := False;
      end;
      if Connected then
      try
        AsyncReady := AsyncResult._is_ready;
      except
        fConnected := False;
      end;
    end;

    if Connected then
    begin
      try
        if AsyncResult.error then
          Result := False
        else
          Result := AsyncResult.value;
      except
        Result := False;
      end;
    end else
    begin
      Result := False;
      PythonIIForm.ClearPendingMessages;
      PostMessage(PythonIIForm.Handle, WM_REINITINTERPRETER, 0, 0);
    end;
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
  PyScripterMod, PySource : Variant;
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
        fConnected := True;
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

    // make sys.stdout etc asynchronous when writing
    RPI.asyncIO();
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
  OldExceptHook : Variant;
begin
  if ServerProcess.State <> psReady then begin

    if not (csDestroying in PyIDEMainForm.ComponentState) then begin
      VariablesWindow.ClearAll;
      UnitTestWindow.ClearAll;
      CallStackWindow.ClearAll;
    end;
    // Do not destroy Remote Debugger
    // PyControl.ActiveDebugger := nil;

    if VarIsPython(OutputRedirector) then
      OutputRedirector._restored:= True;
    VarClear(OutputRedirector);
    FreeAndNil(fMainModule);
    VarClear(fOldArgv);
    VarClear(RPI);
    if fConnected then
      try
        fConnected := False;
        Conn.close();
      except
      end;
    VarClear(Conn);

    // Restore excepthook
    OldExceptHook := Varpyth.SysModule.__excepthook__;
    SysModule.excepthook := OldExceptHook;

    ServerProcess.TerminateTree;
    Sleep(500);  // to free the handles
    ServerProcess.Free;
    CreateServerProcess;
  end;
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
  Result := False;
  CheckConnected(True, False);
  if not Connected then Exit;

  if not Conn.modules.sys.path.__contains__(Path) then begin
    Conn.modules.sys.path.insert(0, Path);
    Result := true;
  end;
end;

function TPyRemoteInterpreter.SysPathRemove(const Path: string): boolean;
begin
  Result := False;
  CheckConnected(True, False);
  if not Connected then Exit;

  if Conn.modules.sys.path.__contains__(Path) then begin
    Result := True;
    Conn.modules.sys.path.remove(Path);
  end;
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
    dsPaused:
      begin
        fDebuggerCommand := dcAbort;
        DoDebuggerCommand;
      end;
  end;
end;

constructor TPyRemDebugger.Create(RemotePython: TPyRemoteInterpreter);
begin
  inherited Create;
  fRemotePython := RemotePython;
  fDebugManager := fRemotePython.RPI.__class__.DebugManager;
  fDebugManager.debugIDE :=
    TPyInternalInterpreter(PyControl.InternalInterpreter).PyInteractiveInterpreter.debugIDE;

  fMainDebugger := fDebugManager.main_debugger;

  fMainThread := TThreadInfo.Create;
  fMainThread.Name := 'MainThread';
  fMainThread.Status := thrdRunning;
  fMainThread.Thread_ID := fDebugManager.main_thread_id;

  fThreads := TObjectDictionary<integer, TThreadInfo>.Create([doOwnsValues]);
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
  Frame, BotFrame, TraceBack : Variant;
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
  Botframe := TraceBack.tb_frame;
  while not VarIsNone(TraceBack.tb_next) do
    TraceBack := TraceBack.tb_next;
  Frame := TraceBack.tb_frame;

  PyControl.DoStateChange(dsPostMortem);
  fMainThread.Status := thrdBroken;
  GetCallStack(fMainThread.CallStack, Frame, Botframe);
  TPyBaseDebugger.ThreadChangeNotify(fMainThread, tctAdded );

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
  fMainThread.Status := thrdRunning;
  fMainThread.CallStack.Clear;
  TPyBaseDebugger.ThreadChangeNotify(fMainThread, tctStatusChange);
  MakeFrameActive(nil);
  PyControl.DoStateChange(dsInactive);
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
   fDebugManager.debug_command := Ord(fDebuggerCommand);
   fDebuggerCommand := dcNone;
end;

function TPyRemDebugger.HaveTraceback: boolean;
begin
  Result := False;
  with fRemotePython do
    if not (IsAvailable and Connected and (ServerProcess.State = psWaiting)) then
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
    fDebugManager.active_frame := (Frame as TFrameInfo).PyFrame
  else
    fDebugManager.active_frame := None;
end;

procedure TPyRemDebugger.MakeThreadActive(Thread: TThreadInfo);
begin
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
  fDebuggerCommand := dcRun;
  DoDebuggerCommand;
end;

procedure TPyRemDebugger.Debug(ARunConfig: TRunConfiguration;
  InitStepIn: Boolean = False; RunToCursorLine : integer = -1);
var
  Code : Variant;
  AsyncRun : Variant;
  AsyncResult : Variant;
  AsyncReady : boolean;
  Path, OldPath : string;
  PythonPathAdder : IInterface;
  ExcInfo : Variant;
  ReturnFocusToEditor: Boolean;
  CanDoPostMortem : Boolean;
  Editor : IEditor;
  Timer : ITimer;
begin
  fRemotePython.CheckConnected;
  CanDoPostMortem := False;
  fDebuggerCommand := dcRun;
  Assert(PyControl.DebuggerState = dsInactive);

  //Compile
  Code := fRemotePython.Compile(ARunConfig);
  if not VarIsPython(Code) then Exit;

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
    Vcl.Dialogs.MessageDlg(_(SCouldNotSetCurrentDir), mtWarning, [mbOK], 0);
  end;

  PyControl.DoStateChange(dsDebugging);
  TPyBaseDebugger.ThreadChangeNotify(fMainThread, tctAdded );


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

  with PythonIIForm do begin
    fOldPS1 := PS1;
    PS1 := DebugPrefix + PS1;
    fOldPS2 := PS2;
    PS2 := DebugPrefix + PS2;
    AppendPrompt;
  end;
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
  PythonIIForm.AppendText(sLineBreak);

  // Set the command line parameters
  SetCommandLine(ARunConfig);

  fMainDebugger.InitStepIn := InitStepIn;

  AsyncRun := fRemotePython.Rpyc.asynch(fMainDebugger.run);
  AsyncResult := AsyncRun.__call__(Code);
  GetPythonEngine.CheckError;
  Timer := NewTimer(50);
  Timer.Start(procedure
  begin
    // Do not reenter
    Timer.Stop;

    try
      fRemotePython.ServeConnection(500);
    except
      fRemotePython.fConnected := False;
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
          ExcInfo := fMainDebugger.exc_info;
          if not VarIsNone(ExcInfo) then begin
            fRemotePython.HandleRemoteException(ExcInfo, 2);
            ReturnFocusToEditor := False;
            Vcl.Dialogs.MessageDlg(Format('%s: %s',
              [VarPythonAsString(ExcInfo.__getitem__(0)),
               VarPythonAsString(fRemotePython.RPI.safestr(ExcInfo.__getitem__(1)))]),
               mtError, [mbOK], 0);
            CanDoPostMortem := True;
          end;
        end;
      except
        // CheckError already called by VarPyth
        on E: EPythonError do begin
          // should not happen
          fRemotePython.CheckConnected(True, False);
          if fRemotePython.Connected then begin
            fRemotePython.HandlePyException(E);
            Vcl.Dialogs.MessageDlg(E.Message, mtError, [mbOK], 0);
          end;
        end;
      end;
    finally
      with PythonIIForm do begin
        PS1 := fOldPS1;
        PS2 := fOldPS2;
        AppendPrompt;
      end;

      fRemotePython.CheckConnected(True, False);
      if fRemotePython.Connected then begin
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
      if not fRemotePython.Connected then begin
        PythonPathAdder := nil;
        PythonIIForm.ClearPendingMessages;
        //fRemotePython.ReInitialize;
        // Reinitialize destroys the debugger which executes this method!
        // So handle with a PostMessage
        PostMessage(PythonIIForm.Handle, WM_REINITINTERPRETER, 0, 0);
      end else if CanDoPostMortem and PyIDEOptions.PostMortemOnException then
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
  OldCurrentPos := PyControl.CurrentPos;
  try
    Result := fRemotePython.RunSource(Source, FileName, symbol);
  finally
    PyControl.CurrentPos := OldCurrentPos;
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
  fMainDebugger.set_break(VarPythonCreate(FName), ALine, 1);

  fDebuggerCommand := dcRunToCursor;
  DoDebuggerCommand;
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
  fMainDebugger.clear_all_breaks();
  for i := 0 to GI_EditorFactory.Count - 1 do
    with GI_EditorFactory.Editor[i] do begin
      FName := FileName;
      if FName = '' then
        FName := '<'+FileTitle+'>';
      for j := 0 to BreakPoints.Count - 1 do begin
        if not TBreakPoint(BreakPoints[j]).Disabled then begin
          if TBreakPoint(BreakPoints[j]).Condition <> '' then begin
            fMainDebugger.set_break(VarPythonCreate(FName),
              TBreakPoint(BreakPoints[j]).LineNo,
              0, VarPythonCreate(TBreakPoint(BreakPoints[j]).Condition));
          end else
            fMainDebugger.set_break(VarPythonCreate(FName),
              TBreakPoint(BreakPoints[j]).LineNo);
        end;
      end;
    end;
  PyControl.BreakPointsChanged := False;
end;

procedure TPyRemDebugger.StepInto;
begin
  fDebuggerCommand := dcStepInto;
  DoDebuggerCommand;
end;

procedure TPyRemDebugger.StepOut;
begin
  fDebuggerCommand := dcStepOut;
  DoDebuggerCommand;
end;

procedure TPyRemDebugger.StepOver;
begin
  fDebuggerCommand := dcStepOver;
  DoDebuggerCommand;
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
  FName : string;
  Frame : Variant;
  LineNumber : integer;
  Thread_ID : integer;
  ThreadInfo : TThreadInfo;
  Arguments : Variant;
  BotFrame : Variant;
begin
  Result := GetPythonEngine.ReturnNone;

  fRemotePython.CheckConnected;

  Arguments := VarPythonCreate(Args);
  Thread_ID := Arguments.__getitem__(0);
  if not fThreads.TryGetValue(Thread_ID, ThreadInfo) then
    Exit;  // Should not happen

  Frame := Arguments.__getitem__(1);
  BotFrame := Arguments.__getitem__(2);
  Assert(VarIsPython(Frame) and not VarisNone(Frame));
  FName := Frame.f_code.co_filename;
  LineNumber := Frame.f_lineno;
  if (FName[1] ='<') and (FName[Length(FName)] = '>') then
    FName :=  Copy(FName, 2, Length(FName)-2);

  if PyIDEMainForm.ShowFilePosition(FName, LineNumber, 1, 0, True, False) and
    (LineNumber > 0) then
  begin
    if PyControl.DebuggerState = dsDebugging then
      PyControl.DoStateChange(dsPaused);

    ThreadInfo.Status := thrdBroken;
    GetCallStack(ThreadInfo.CallStack, Frame, BotFrame);
    TPyBaseDebugger.ThreadChangeNotify(ThreadInfo, tctStatusChange);

    FDebuggerCommand := dcNone;

    // Return true if broken
    with GetPythonEngine do begin
      Result := PPyObject(Py_True);
      Py_INCREF(Result);
    end;
  end;
end;

procedure TPyRemDebugger.UserThread(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);

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

Var
  Thread_ID : integer;
  ThreadName : string;
  ThreadStatus : integer;
  Arguments: Variant;
  ThreadInfo : TThreadInfo;
  OldStatus : TThreadStatus;
begin
  Result := GetPythonEngine.ReturnNone;

  Arguments := VarPythonCreate(Args);
  Thread_ID := Arguments.__getitem__(0);
  ThreadName := Arguments.__getitem__(1);
  ThreadStatus := Arguments.__getitem__(2);

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
            PyControl.DoStateChange(dsDebugging);
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

procedure TPyRemDebugger.UserYield(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
begin
  Result := GetPythonEngine.PyInt_FromLong(Ord(fDebuggerCommand));
end;

end.
