{-----------------------------------------------------------------------------
 Unit Name: cInternalPython
 Author:    Kiriakos Vlahos
 Date:      23-Apr-20018
 Purpose:   Encapsulate the creation of the internal Python engine.
            Support multiple python versions and virtual environments.
 History:
-----------------------------------------------------------------------------}

unit cInternalPython;

interface

Uses
  System.SysUtils,
  System.Classes,
  PythonEngine,
  PythonVersions,
  WrapDelphi;


Const
  // Defined DebugIDE events
  dbie_user_call            = 0;
  dbie_user_line            = 1;
  dbie_user_thread          = 2;
  dbie_user_exception       = 3;
  dbie_user_yield           = 4;

type

  TInternalPython = class
  private
    fPythonEngine : TPythonEngine;
    fDebugIDE: TPythonModule;
    PyDelphiWrapper: TPyDelphiWrapper;
    PyscripterModule: TPythonModule;
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

implementation

uses
  WinApi.Windows,
  VarPyth,
  dmCommands,
  frmPyIDEMain,
  frmPythonII,
  frmMessages,
  frmUnitTests,
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
  fPythonEngine.DllName := 'python27.dll';
  fPythonEngine.APIVersion := 1012;
  fPythonEngine.RegVersion := '2.7';
  fPythonEngine.FatalAbort := False;
  fPythonEngine.FatalMsgDlg := False;
  fPythonEngine.UseLastKnownVersion := False;
  fPythonEngine.AutoFinalize := False;
  fPythonEngine.InitThreads := True;
  fPythonEngine.IO := PythonIIForm.PythonIO;
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
begin
  WasLoaded := Loaded;
  if WasLoaded then PyscripterModule.DeleteVar('IDEOptions');
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
    UnloadPythonDLL('unicodedata.pyd');
    UnloadPythonDLL('_ctypes.pyd');
    UnloadPythonDLL('_hashlib.pyd');
    //UnloadPythonDLL('pywintypes36.dll');
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
begin
  DestroyPythonComponents;
  // set environment variables
  SetEnvironmentVariable('PYTHONHOME', '');
  if not (Version.IsRegistered or Version.Is_venv) then
    SetEnvironmentVariable('PYTHONHOME', PWideChar(Version.InstallPath));

  CreatePythonComponents;
  Version.AssignTo(PythonEngine);
  PythonEngine.LoadDll;
  Result := PythonEngine.IsHandleValid;
  if Result then begin
    Initialize;
  end else
    DestroyPythonComponents;
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
      MessagesWindow.AddMessage(string(Msg), S, LineNo, Offset);
      Result := ReturnNone;
    end else
      Result := nil;
end;

procedure TInternalPython.PythonEngineAfterInit(Sender: TObject);
Var
  Keywords, Builtins, BuiltInMod : Variant;
  i : integer;
begin
  // Execute initialization script
  with PythonEngine do begin
    if IsPython3000 then
      ExecStrings(CommandsDataModule.JvMultiStringHolder.StringsByName['InitScript3000'])
    else
      ExecStrings(CommandsDataModule.JvMultiStringHolder.StringsByName['InitScript'])
  end;

  // Setup Highlighter keywords
  with CommandsDataModule do begin
    SynPythonSyn.Keywords.Clear;
    SynPythonSyn.Keywords.Sorted := False;
    Keywords := Import('keyword').kwlist;
    for i := 0 to Len(Keywords) - 1 do
      SynPythonSyn.Keywords.AddObject(Keywords.__getitem__(i), Pointer(Ord(tkKey)));
    BuiltInMod := VarPyth.BuiltinModule;
    Builtins := BuiltinMod.dir(BuiltinMod);
    for i := 0 to Len(Builtins) - 1 do
      SynPythonSyn.Keywords.AddObject(Builtins.__getitem__(i), Pointer(Ord(tkNonKeyword)));
    // add pseudo keyword self
    SynPythonSyn.Keywords.AddObject('self', Pointer(Ord(tkNonKeyword)));
    SynPythonSyn.Keywords.Sorted := True;

    with SynCythonSyn do begin
      Keywords.Clear;
      Keywords.Sorted := False;
      Keywords.AddStrings(SynPythonSyn.Keywords);
      AddCythonKeywords(SynCythonSyn.Keywords);
      Keywords.Sorted := True;
    end;

    with (PythonIIForm.SynEdit.Highlighter as TSynPythonInterpreterSyn) do begin
      Keywords.Clear;
      Keywords.Sorted := False;
      Keywords.AddStrings(SynPythonSyn.Keywords);
      Keywords.Sorted := True;
    end;
  end;
end;

procedure TInternalPython.StatusWriteExecute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
Var
  Msg : PAnsiChar;
begin
  with PythonEngine do
    if PyArg_ParseTuple( args, 's:statusWrite', @Msg) <> 0 then begin
      PyIDEMainForm.WriteStatusMsg(string(Msg));
      Result := ReturnNone;
    end else
      Result := nil;
end;

procedure TInternalPython.testResultAddError(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  UnitTestWindow.AddError(VarPythonCreate(Args).__getitem__(0),
    VarPythonCreate(Args).__getitem__(1));
  Result := PythonEngine.ReturnNone;
end;

procedure TInternalPython.testResultAddFailure(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  UnitTestWindow.AddFailure(VarPythonCreate(Args).__getitem__(0),
    VarPythonCreate(Args).__getitem__(1));
  Result := PythonEngine.ReturnNone;
end;

procedure TInternalPython.testResultAddSuccess(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  UnitTestWindow.AddSuccess(VarPythonCreate(Args).__getitem__(0));
  Result := PythonEngine.ReturnNone;
end;

procedure TInternalPython.testResultStartTestExecute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  UnitTestWindow.StartTest(VarPythonCreate(Args).__getitem__(0));
  Result := PythonEngine.ReturnNone;
end;

procedure TInternalPython.testResultStopTestExecute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  UnitTestWindow.StopTest(VarPythonCreate(Args).__getitem__(0));
  Result := PythonEngine.ReturnNone;
end;

procedure TInternalPython.UnMaskFPUExceptionsExecute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  MaskFPUExceptions(False);
  PyIDEOptions.MaskFPUExceptions := False;
  Result := PythonEngine.ReturnNone;
end;

end.
