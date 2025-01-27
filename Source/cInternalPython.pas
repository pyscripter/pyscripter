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

uses
  PythonEngine,
  PythonVersions,
  WrapDelphi;

type

  TInternalPython = class
  private
    FPythonEngine: TPythonEngine;
    FDebugIDE: TPythonModule;
    FPyDelphiWrapper: TPyDelphiWrapper;
    FPyscripterModule: TPythonModule;
    FOldPath: string;
    procedure CreateDebugIDE;
    procedure CreateFPyscripterModule;
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
    property Loaded: Boolean read GetLoaded;
    property DebugIDE: TPythonModule read FDebugIDE;
    property PythonEngine: TPythonEngine read FPythonEngine;
  end;

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.StrUtils,
  Vcl.Dialogs,
  VarPyth,
  uEditAppIntfs,
  cPyScripterSettings,
  cPySupportTypes,
  uCommonFunctions;

{ TInternalPython }

procedure TInternalPython.CreateDebugIDE;
begin
  FDebugIDE := TPythonModule.Create(nil);

  FDebugIDE.Name := 'DebugIDE';
  FDebugIDE.Engine := PythonEngine;
  with FDebugIDE.Events.Add do Name := 'user_call';
  with FDebugIDE.Events.Add do Name := 'user_line';
  with FDebugIDE.Events.Add do Name := 'user_thread';
  with FDebugIDE.Events.Add do Name := 'user_exception';
  with FDebugIDE.Events.Add do Name := 'user_yield';
  with FDebugIDE.Events.Add do begin
    Name := 'InputBox';
    OnExecute := InputBoxExecute;
  end;
  with FDebugIDE.Events.Add do begin
    Name := 'statusWrite';
    OnExecute := StatusWriteExecute;
  end;
  with FDebugIDE.Events.Add do begin
    Name := 'messageWrite';
    OnExecute := MessageWriteExecute;
  end;
  with FDebugIDE.Events.Add do begin
    Name := 'get8087CW';
    OnExecute := Get8087CWExecute;
  end;
  with FDebugIDE.Events.Add do begin
    Name := 'maskFPUexceptions';
    OnExecute := MaskFPUExceptionsExecute;
  end;
  with FDebugIDE.Events.Add do begin
    Name := 'unmaskFPUexceptions';
    OnExecute := UnMaskFPUExceptionsExecute;
  end;
  with FDebugIDE.Events.Add do begin
    Name := 'testResultStartTest';
    OnExecute := testResultStartTestExecute;
  end;
  with FDebugIDE.Events.Add do begin
    Name := 'testResultStopTest';
    OnExecute := testResultStopTestExecute;
  end;
  with FDebugIDE.Events.Add do begin
    Name := 'testResultAddSuccess';
    OnExecute := testResultAddSuccess;
  end;
  with FDebugIDE.Events.Add do begin
    Name := 'testResultAddFailure';
    OnExecute := testResultAddFailure;
  end;
  with FDebugIDE.Events.Add do begin
    Name := 'testResultAddError';
    OnExecute := testResultAddError;
  end;
  FDebugIDE.ModuleName := 'DebugIDE';
end;

procedure TInternalPython.CreateFPyscripterModule;
begin
  FPyscripterModule := TPythonModule.Create(nil);

  FPyscripterModule.Name := 'FPyscripterModule';
  FPyscripterModule.Engine := PythonEngine;
  FPyscripterModule.ModuleName := 'pyscripter';

  FPyDelphiWrapper := TPyDelphiWrapper.Create(nil);

  FPyDelphiWrapper.Name := 'FPyDelphiWrapper';
  FPyDelphiWrapper.Engine := PythonEngine;
  FPyDelphiWrapper.Module := FPyscripterModule;
end;

procedure TInternalPython.CreatePythonComponents;
begin
  DestroyPythonComponents;
  CreatePythonEngine;
  CreateDebugIDE;
  CreateFPyscripterModule;
end;

procedure TInternalPython.CreatePythonEngine;
begin
  FPythonEngine := TPythonEngine.Create(nil);

  FPythonEngine.Name := 'PythonEngine';
  FPythonEngine.AutoLoad := False;
  FPythonEngine.DllName := 'python39.dll';
  FPythonEngine.APIVersion := 1013;
  FPythonEngine.RegVersion := '3.9';
  FPythonEngine.FatalAbort := False;
  FPythonEngine.FatalMsgDlg := False;
  FPythonEngine.UseLastKnownVersion := False;
  FPythonEngine.AutoFinalize := False;
  FPythonEngine.IO := GI_PyInterpreter.PythonIO;
  FPythonEngine.PyFlags := [pfInteractive];
  FPythonEngine.OnAfterInit := PythonEngineAfterInit;
end;

destructor TInternalPython.Destroy;
begin
  DestroyPythonComponents;
  inherited;
end;

procedure TInternalPython.DestroyPythonComponents;

  procedure UnloadPythonDLL(DLL: PWideChar);
  var
    Module: HMODULE;
  begin
    Module := GetModuleHandle(DLL);
    if Module >= 32 then
      FreeLibrary(Module);
  end;

var
  WasLoaded: Boolean;
  RegVersion: string;
begin
  WasLoaded := Loaded;
  if WasLoaded then begin
    TPythonThread.Py_End_Allow_Threads;
    FPyscripterModule.DeleteVar('IDEOptions');
    RegVersion := FPythonEngine.RegVersion;
    Delete(RegVersion, 2, 1);
  end;
  FreeAndNil(FPythonEngine);  // Unloads Python Dll
  FreeAndNil(FDebugIDE);
  FreeAndNil(FPyDelphiWrapper);
  FreeAndNil(FPyscripterModule);

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
  Result := Assigned(PythonEngine) and PythonEngine.Initialized;
end;

procedure TInternalPython.Initialize;
begin
  // Wrap IDE Options
  var PyOptions := FPyDelphiWrapper.Wrap(PyIDEOptions);
  FPyscripterModule.SetVar('IDEOptions', PyOptions);
  PythonEngine.Py_XDECREF(PyOptions);
end;

procedure TInternalPython.InputBoxExecute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
var
  PCaption, PPrompt, PDefault: PAnsiChar;
  WideS: string;
  Res: Boolean;
begin
  with PythonEngine do
    if PyArg_ParseTuple(Args, 'sss:InputBox', @PCaption, @PPrompt, @PDefault) <> 0 then begin
      WideS := UTF8ToUnicodeString(PDefault);

      Res := SyncWideInputQuery(UTF8ToUnicodeString(PCaption), UTF8ToUnicodeString(PPrompt), WideS);
      if Res then
        Result := PyUnicode_FromWideChar(PWideChar(WideS), Length(WideS))
      else
        Result := ReturnNone;
    end else
      Result := nil;
end;

function TInternalPython.LoadPython(const Version: TPythonVersion): Boolean;
var
  Path, NewPath: string;
begin
  DestroyPythonComponents;

  CreatePythonComponents;
  try
    Version.AssignTo(PythonEngine);
    // set environment variables
    if FOldPath <> '' then begin
      SetEnvironmentVariable('PATH', PWideChar(FOldPath));
      FOldPath := '';
    end;

    SetEnvironmentVariable('PYTHONHOME', nil);  // delete it
    if Version.Is_conda then begin
      FOldPath := System.SysUtils.GetEnvironmentVariable('PATH');
      if not ContainsText(Path, Version.InstallPath) then begin
        NewPath := Format('%s;%0:s\Library\bin;', [Version.InstallPath]) + FOldPath;
        SetEnvironmentVariable('PATH', PWideChar(NewPath));
      end;
    end;

    // Repeat here to make sure it is set right
    MaskFPUExceptions(PyIDEOptions.MaskFPUExceptions);

    PythonEngine.LoadDll;
    Result := PythonEngine.IsHandleValid;
    if Result then
      Initialize
    else
      DestroyPythonComponents;
  except
    on E: Exception do begin
      StyledMessageDlg(E.Message, mtError, [mbOK], 0);
      DestroyPythonComponents;
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
var
  Msg, FName: PAnsiChar;
  LineNo, Offset: Integer;
begin
  FName := nil;
  LineNo := 0;
  Offset := 0;
  with PythonEngine do
    if PyArg_ParseTuple(Args, 's|sii:messageWrite', @Msg, @FName, @LineNo, @Offset) <> 0 then begin
      GI_PyIDEServices.Messages.AddMessage(UTF8ToUnicodeString(Msg),
        UTF8ToUnicodeString(FName), LineNo, Offset);
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
var
  Msg: PAnsiChar;
begin
  with PythonEngine do
    if PyArg_ParseTuple(Args, 's:statusWrite', @Msg) <> 0 then begin
      GI_PyIDEServices.WriteStatusMsg(UTF8ToUnicodeString(Msg));
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

end.
