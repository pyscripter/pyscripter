{-----------------------------------------------------------------------------
 Unit Name: cInternalPython
 Author:    Kiriakos Vlahos
 Date:      23-Apr-20018
 Purpose:   Encapsulate the creation of the internal Python engine.
            Support multiple Python versions and virtual environments.
 History:
-----------------------------------------------------------------------------}

unit cInternalPython;

interface

Uses
  System.SysUtils,
  System.Classes,
  PythonEngine,
  PythonVersions,
  WrapDelphi,
  uEditAppIntfs;

type

  TInternalPython = class
  private
    fPythonEngine: TPythonEngine;
    fDebugIDE: TPythonModule;
    PyDelphiWrapper: TPyDelphiWrapper;
    PyscripterModule: TPythonModule;
    fOldPath: string;
    procedure CreateDebugIDE;
    procedure CreatePyScripterModule;
    procedure CreatePythonEngine;
    procedure CreatePythonComponents;
    procedure DestroyPythonComponents;
    procedure PythonEngineAfterInit(Sender: TObject);
    procedure InputBoxExecute(Sender: TObject; PSelf, Args: PPyObject;
      var Result: PPyObject);
    procedure StatusWriteExecute(Sender: TObject; PSelf,
      Args: PPyObject; var Result: PPyObject);
    procedure MessageWriteExecute(Sender: TObject; PSelf,
      Args: PPyObject; var Result: PPyObject);
    procedure testResultAddError(Sender: TObject; PSelf, Args: PPyObject;
      var Result: PPyObject);
    procedure testResultAddFailure(Sender: TObject; PSelf, Args: PPyObject;
      var Result: PPyObject);
    procedure testResultAddSuccess(Sender: TObject; PSelf, Args: PPyObject;
      var Result: PPyObject);
    procedure testResultStopTestExecute(Sender: TObject; PSelf, Args: PPyObject;
      var Result: PPyObject);
    procedure testResultStartTestExecute(Sender: TObject; PSelf,
      Args: PPyObject; var Result: PPyObject);
    procedure MaskFPUExceptionsExecute(Sender: TObject; PSelf, Args: PPyObject;
      var Result: PPyObject);
    procedure UnMaskFPUExceptionsExecute(Sender: TObject; PSelf,
      Args: PPyObject; var Result: PPyObject);
    procedure Get8087CWExecute(Sender: TObject; PSelf, Args: PPyObject;
      var Result: PPyObject);
    procedure Initialize;
    function GetLoaded: Boolean;
  public
    destructor Destroy; override;
    function LoadPython(const Version: TPythonVersion): Boolean;
    property Loaded : Boolean read GetLoaded;
    property DebugIDE : TPythonModule read fDebugIDE;
    property PythonEngine : TPythonEngine read fPythonEngine;
  end;

{ Access the PythonEngine with thread safety}
function InternalSafePyEngine: IPyEngineAndGIL;

{ Executes Python code in a Delphi thread }
procedure InternalThreadPythonExec(ExecuteProc : TProc; TerminateProc : TProc = nil;
  WaitToFinish: Boolean = False; ThreadExecMode : TThreadExecMode = emNewState);

implementation

uses
  WinApi.Windows,
  System.UITypes,
  System.StrUtils,
  Vcl.Dialogs,
  VarPyth,
  SynHighlighterPython,
  cPyScripterSettings,
  uCommonFunctions;

{ TInternalPython }

procedure TInternalPython.CreateDebugIDE;
begin
  fDebugIDE := TPythonModule.Create(nil);

  fDebugIDE.Name := 'DebugIDE';
  fDebugIDE.Engine := PythonEngine;
  with fDebugIDE.Events.Add do Name := 'user_call';
  with fDebugIDE.Events.Add do Name := 'user_line';
  with fDebugIDE.Events.Add do Name := 'user_thread';
  with fDebugIDE.Events.Add do Name := 'user_exception';
  with fDebugIDE.Events.Add do Name := 'user_yield';
  with fDebugIDE.Events.Add do begin
    Name := 'InputBox';
    OnExecute := InputBoxExecute;
  end;
  with fDebugIDE.Events.Add do begin
    Name := 'statusWrite';
    OnExecute := StatusWriteExecute;
  end;
  with fDebugIDE.Events.Add do begin
    Name := 'messageWrite';
    OnExecute := MessageWriteExecute;
  end;
  with fDebugIDE.Events.Add do begin
    Name := 'get8087CW';
    OnExecute := Get8087CWExecute;
  end;
  with fDebugIDE.Events.Add do begin
    Name := 'maskFPUexceptions';
    OnExecute := MaskFPUExceptionsExecute;
  end;
  with fDebugIDE.Events.Add do begin
    Name := 'unmaskFPUexceptions';
    OnExecute := UnMaskFPUExceptionsExecute;
  end;
  with fDebugIDE.Events.Add do begin
    Name := 'testResultStartTest';
    OnExecute := testResultStartTestExecute;
  end;
  with fDebugIDE.Events.Add do begin
    Name := 'testResultStopTest';
    OnExecute := testResultStopTestExecute;
  end;
  with fDebugIDE.Events.Add do begin
    Name := 'testResultAddSuccess';
    OnExecute := testResultAddSuccess;
  end;
  with fDebugIDE.Events.Add do begin
    Name := 'testResultAddFailure';
    OnExecute := testResultAddFailure;
  end;
  with fDebugIDE.Events.Add do begin
    Name := 'testResultAddError';
    OnExecute := testResultAddError;
  end;
  fDebugIDE.ModuleName := 'DebugIDE';
end;

procedure TInternalPython.CreatePyScripterModule;
begin
  PyscripterModule := TPythonModule.Create(nil);

  PyscripterModule.Name := 'PyscripterModule';
  PyscripterModule.Engine := PythonEngine;
  PyscripterModule.ModuleName := 'pyscripter';

  PyDelphiWrapper := TPyDelphiWrapper.Create(nil);

  PyDelphiWrapper.Name := 'PyDelphiWrapper';
  PyDelphiWrapper.Engine := PythonEngine;
  PyDelphiWrapper.Module := PyscripterModule;
end;

procedure TInternalPython.CreatePythonComponents;
begin
  DestroyPythonComponents;
  CreatePythonEngine;
  CreateDebugIDE;
  CreatePyScripterModule;
end;

procedure TInternalPython.CreatePythonEngine;
begin
  fPythonEngine := TPythonEngine.Create(nil);

  fPythonEngine.Name := 'PythonEngine';
  fPythonEngine.AutoLoad := False;
  fPythonEngine.DllName := 'python39.dll';
  fPythonEngine.APIVersion := 1013;
  fPythonEngine.RegVersion := '3.9';
  fPythonEngine.FatalAbort := False;
  fPythonEngine.FatalMsgDlg := False;
  fPythonEngine.UseLastKnownVersion := False;
  fPythonEngine.AutoFinalize := False;
  fPythonEngine.InitThreads := True;
  fPythonEngine.IO := GI_PyInterpreter.PythonIO;
  fPythonEngine.PyFlags := [pfInteractive];
  fPythonEngine.OnAfterInit := PythonEngineAfterInit;
end;

destructor TInternalPython.Destroy;
begin
  DestroyPythonComponents;
  inherited;
end;

procedure TInternalPython.DestroyPythonComponents;

  procedure UnloadPythonDLL(DLL: PWideChar);
  Var
    Module : HMODULE;
  begin
    MODULE := GetModuleHandle(DLL);
    if MODULE >= 32 then
      FreeLibrary(Module);
  end;

Var
  WasLoaded: Boolean;
  RegVersion : string;
begin
  WasLoaded := Loaded;
  if WasLoaded then begin
    TPythonThread.Py_End_Allow_Threads;
    PyscripterModule.DeleteVar('IDEOptions');
    RegVersion := fPythonEngine.RegVersion;
    Delete(RegVersion, 2, 1);
  end;
  FreeAndNil(fPythonEngine);  // Unloads Python Dll
  FreeAndNil(fDebugIDE);
  FreeAndNil(PyDelphiWrapper);
  FreeAndNil(PyscripterModule);

  if WasLoaded then
  begin
    UnloadPythonDLL('select.pyd');
    UnloadPythonDLL('_socket.pyd');
    UnloadPythonDLL('python3.dll');
    UnloadPythonDLL('_ssl.pyd');
    UnloadPythonDLL('win32file.pyd');
    UnloadPythonDLL('win32pipe.pyd');
    UnloadPythonDLL('win32event.pyd');
    UnloadPythonDLL('_win32sysloader.pyd');
    UnloadPythonDLL('unicodedata.pyd');
    UnloadPythonDLL('_ctypes.pyd');
    UnloadPythonDLL('_hashlib.pyd');
    UnloadPythonDLL('_asyncio.pyd');
    UnloadPythonDLL('_overlapped.pyd');
    UnloadPythonDLL('_bz2.pyd');
    UnloadPythonDLL('_lzma.pyd');
    UnloadPythonDLL('_queue.pyd');
    UnloadPythonDLL(PChar('pywintypes' + RegVersion + '.dll'));
  end;
end;

procedure TInternalPython.Get8087CWExecute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  Result := PythonEngine.PyLong_FromUnsignedLong(Get8087CW);
end;

function TInternalPython.GetLoaded: Boolean;
begin
  Result := Assigned(PythonEngine) and PythonEngine.IsHandleValid;
end;

procedure TInternalPython.Initialize;
Var
  P : PPyObject;
begin
  // Wrap IDE Options
  p := PyDelphiWrapper.Wrap(PyIDEOptions);
  PyscripterModule.SetVar('IDEOptions', p);
  PythonEngine.Py_XDECREF(p);
end;

procedure TInternalPython.InputBoxExecute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
Var
  PCaption, PPrompt, PDefault : PWideChar;
  WideS : string;
  Res : Boolean;
begin
  with PythonEngine do
    if PyArg_ParseTuple( args, 'uuu:InputBox', @PCaption, @PPrompt, @PDefault) <> 0 then begin
      WideS := PDefault;

      Res := SyncWideInputQuery(PCaption, PPrompt, WideS);
      if Res then
        Result := PyUnicode_FromWideChar(PWideChar(WideS), Length(WideS))
      else
        Result := ReturnNone;
    end else
      Result := nil;
end;

function TInternalPython.LoadPython(const Version: TPythonVersion): Boolean;
Var
  Path, NewPath : string;
begin
  DestroyPythonComponents;

  CreatePythonComponents;
  try
    Version.AssignTo(PythonEngine);
    // set environment variables
    if fOldPath <> '' then begin
      SetEnvironmentVariable('PATH', PWideChar(fOldPath));
      fOldPath := '';
    end;

    SetEnvironmentVariable('PYTHONHOME', nil);  // delete it
    if Version.Is_conda then begin
      fOldPath := System.SysUtils.GetEnvironmentVariable('PATH');
      if not ContainsText(Path, Version.InstallPath) then begin
        NewPath := Format('%s;%0:s\Library\bin;', [Version.InstallPath]) + fOldPath;
        SetEnvironmentVariable('PATH', PWideChar(NewPath));
      end;
    end;

    // Repeat here to make sure it is set right
    MaskFPUExceptions(PyIDEOptions.MaskFPUExceptions);

    PythonEngine.LoadDll;
    Result := PythonEngine.IsHandleValid;
    if Result then begin
      Initialize;
    end else
      DestroyPythonComponents;
  except
    on E: Exception do begin
      DestroyPythonComponents;
      StyledMessageDlg(E.Message, mtError, [mbOK], 0);
      Result := False;
    end;
  end;
end;

procedure TInternalPython.MaskFPUExceptionsExecute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  MaskFPUExceptions(True);
  PyIDEOptions.MaskFPUExceptions := True;
  Result := PythonEngine.ReturnNone;
end;

procedure TInternalPython.MessageWriteExecute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
Var
  Msg, FName : PAnsiChar;
  LineNo, Offset : integer;
  S : string;
begin
  FName := nil;
  LineNo := 0;
  Offset := 0;
  with PythonEngine do
    if PyArg_ParseTuple( args, 's|sii:messageWrite', @Msg, @FName, @LineNo, @Offset) <> 0 then begin
      if Assigned(FName) then
        S := string(FName)
      else
        S := '';
      GI_PyIDEServices.Messages.AddMessage(string(Msg), S, LineNo, Offset);
      Result := ReturnNone;
    end else
      Result := nil;
end;

procedure TInternalPython.PythonEngineAfterInit(Sender: TObject);
begin
  // Execute initialization script
  PythonEngine.ExecStrings(GI_PyIDEServices.GetStoredScript('InitScript'));

  // Update Highlighter keywords
  GI_PyInterpreter.UpdatePythonKeywords;
end;

procedure TInternalPython.StatusWriteExecute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
Var
  Msg : PAnsiChar;
begin
  with PythonEngine do
    if PyArg_ParseTuple( args, 's:statusWrite', @Msg) <> 0 then begin
      GI_PyIDEServices.WriteStatusMsg(string(Msg));
      Result := ReturnNone;
    end else
      Result := nil;
end;

procedure TInternalPython.testResultAddError(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  GI_PyIDEServices.UnitTests.AddError(VarPythonCreate(Args).__getitem__(0),
    VarPythonCreate(Args).__getitem__(1));
  Result := PythonEngine.ReturnNone;
end;

procedure TInternalPython.testResultAddFailure(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  GI_PyIDEServices.UnitTests.AddFailure(VarPythonCreate(Args).__getitem__(0),
    VarPythonCreate(Args).__getitem__(1));
  Result := PythonEngine.ReturnNone;
end;

procedure TInternalPython.testResultAddSuccess(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  GI_PyIDEServices.UnitTests.AddSuccess(VarPythonCreate(Args).__getitem__(0));
  Result := PythonEngine.ReturnNone;
end;

procedure TInternalPython.testResultStartTestExecute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  GI_PyIDEServices.UnitTests.StartTest(VarPythonCreate(Args).__getitem__(0));
  Result := PythonEngine.ReturnNone;
end;

procedure TInternalPython.testResultStopTestExecute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  GI_PyIDEServices.UnitTests.StopTest(VarPythonCreate(Args).__getitem__(0));
  Result := PythonEngine.ReturnNone;
end;

procedure TInternalPython.UnMaskFPUExceptionsExecute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  MaskFPUExceptions(False);
  PyIDEOptions.MaskFPUExceptions := False;
  Result := PythonEngine.ReturnNone;
end;

type
  TPyEngineAndGIL = class(TInterfacedObject, IPyEngineAndGIL)
  fPythonEngine: TPythonEngine;
  fThreadState: PPyThreadState;
  fGILState: PyGILstate_STATE;
  private
    function GetPyEngine: TPythonEngine;
    function GetThreadState: PPyThreadState;
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TPyEngineAndGIL }

constructor TPyEngineAndGIL.Create;
begin
  inherited Create;
  fPythonEngine := GetPythonEngine;
  fGILState := fPythonEngine.PyGILState_Ensure;
  fThreadState := fPythonEngine.PyThreadState_Get;
end;

destructor TPyEngineAndGIL.Destroy;
begin
  fPythonEngine.PyGILState_Release(fGILState);
  inherited;
end;

function TPyEngineAndGIL.GetPyEngine: TPythonEngine;
begin
  Result := fPythonEngine;
end;

function TPyEngineAndGIL.GetThreadState: PPyThreadState;
begin
  Result := fThreadState;
end;

{ Access the PythonEngine with thread safety}
function InternalSafePyEngine: IPyEngineAndGIL;
begin
  Result := TPyEngineAndGIL.Create
end;

{ TAnonymousPythonThread }
type
TAnonymousPythonThread = class(TPythonThread)
private
  fTerminateProc : TProc;
  fExecuteProc : TProc;
  procedure DoTerminate; override;
public
  procedure ExecuteWithPython; override;
  constructor Create(ExecuteProc : TProc; TerminateProc : TProc = nil;
    Suspended: Boolean = False; AThreadExecMode : TThreadExecMode = emNewState);
end;

constructor TAnonymousPythonThread.Create(ExecuteProc : TProc; TerminateProc : TProc;
    Suspended: Boolean; AThreadExecMode : TThreadExecMode);
begin
  inherited Create(Suspended);
  fExecuteProc := ExecuteProc;
  fTerminateProc := TerminateProc;
  FreeOnTerminate := True;
  ThreadExecMode := AThreadExecMode;
end;

procedure TAnonymousPythonThread.ExecuteWithPython;
begin
  if Assigned(fExecuteProc) then
    try
        fExecuteProc();
    except
    end;
end;

procedure TAnonymousPythonThread.DoTerminate;
// Use Thread.Queue to run the TerminateProc in the main thread
// Could use Synchronize instead, but such calls better be avoided
var
  TerminateProc: TProc;
begin
  TerminateProc := fTerminateProc;  // to keep fTerminateProc alive at destruction
  if Assigned(TerminateProc) then
    TThread.Queue(nil, procedure
    begin
        TerminateProc();
    end);
end;


{ InternalThreadPythonExec }

procedure InternalThreadPythonExec(ExecuteProc : TProc; TerminateProc : TProc;
  WaitToFinish: Boolean; ThreadExecMode : TThreadExecMode);
var
  Thread: TAnonymousPythonThread;
begin
  if GetCurrentThreadId <> MainThreadID then
    raise Exception.Create('ThreadPythonExec should only be called from the main thread');
  Thread := TAnonymousPythonThread.Create(ExecuteProc, TerminateProc, WaitToFinish, ThreadExecMode);
  if WaitToFinish then
  begin
    Thread.FreeOnTerminate := False;
    Thread.Start;
    Thread.WaitFor; // Note that it calls CheckSyncrhonize
    Thread.Free;
  end;
end;

end.
