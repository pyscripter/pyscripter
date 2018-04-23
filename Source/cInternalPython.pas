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
  WrapDelphi;


Const
  // Defined DebugIDE events
  dbie_user_call            = 0; 
  dbie_user_line            = 1; 
  dbie_user_thread          = 2; 
  dbie_user_exception       = 3; 
  dbie_user_yield           = 4; 
  dbie_InputBox             = 5; 
  dbie_statusWrite          = 6; 
  dbie_messageWrite         = 7; 
  dbie_get8087CW            = 8; 
  dbie_maskFPUexceptions    = 9; 
  dbie_unmaskFPUexceptions  = 10; 
  dbie_testResultStartTest  = 11; 
  dbie_testResultStopTest   = 12; 
  dbie_testResultAddSuccess = 13; 
  dbie_testResultAddFailure = 14; 
  dbie_testResultAddError   = 15; 
  dbie_awakeGUI             = 16; 
 
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
  procedure PythonEngineAfterInit(Sender: TObject);
public
  property DebugIDE : TPythonModule read fDebugIDE;
  property PythonEngine : TPythonEngine read fPythonEngine;
end;

implementation

uses
  cPyScripterSettings, frmPythonII,
  dmCommands,
  VarPyth,
  SynHighlighterPython;

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
    //OnExecute := InputBoxExecute;
  end;
  with fDebugIDE.Events.Add do begin
    Name := 'statusWrite';
    //OnExecute := StatusWriteExecute;
  end;
  with fDebugIDE.Events.Add do begin
    Name := 'messageWrite';
    //OnExecute := MessageWriteExecute;
  end;
  with fDebugIDE.Events.Add do begin
    Name := 'get8087CW';
    //OnExecute := Get8087CWExecute;
  end;
  with fDebugIDE.Events.Add do begin
    Name := 'maskFPUexceptions';
    //OnExecute := MaskFPUExceptionsExecute;
  end;
  with fDebugIDE.Events.Add do begin
    Name := 'unmaskFPUexceptions';
    //OnExecute := UnMaskFPUExceptionsExecute;
  end;
  with fDebugIDE.Events.Add do begin
    Name := 'testResultStartTest';
    //OnExecute := testResultStartTestExecute;
  end;
  with fDebugIDE.Events.Add do begin
    Name := 'testResultStopTest';
    //OnExecute := testResultStopTestExecute;
  end;
  with fDebugIDE.Events.Add do begin
    Name := 'testResultAddSuccess';
    //OnExecute := testResultAddSuccess;
  end;
  with fDebugIDE.Events.Add do begin
    Name := 'testResultAddFailure';
    //OnExecute := testResultAddFailure;
  end;
  with fDebugIDE.Events.Add do begin
    Name := 'testResultAddError';
    //OnExecute := testResultAddError;
  end;
  with fDebugIDE.Events.Add do begin
    Name := 'awakeGUI';
    //OnExecute := awakeGUIExecute;
  end;
  fDebugIDE.ModuleName := 'DebugIDE';
end;

procedure TInternalPython.CreatePyScripterModule;
Var
  P : PPyObject;
begin
  PyscripterModule := TPythonModule.Create(nil);

  PyscripterModule.Name := 'PyscripterModule';
  PyscripterModule.Engine := PythonEngine;
  PyscripterModule.ModuleName := 'pyscripter';

  PyDelphiWrapper := TPyDelphiWrapper.Create(nil);

  PyDelphiWrapper.Name := 'PyDelphiWrapper';
  PyDelphiWrapper.Engine := PythonEngine;
  PyDelphiWrapper.Module := PyscripterModule;

  // Wrap IDE Options
  p := PyDelphiWrapper.Wrap(PyIDEOptions);
  PyscripterModule.SetVar('IDEOptions', p);
  PythonEngine.Py_XDECREF(p);
end;

procedure TInternalPython.CreatePythonEngine;
begin
  fPythonEngine := TPythonEngine.Create(nil);

  fPythonEngine.Name := 'PythonEngine';
  fPythonEngine.AutoLoad := False;
  fPythonEngine.DllName := 'python25.dll';
  fPythonEngine.APIVersion := 1012;
  fPythonEngine.RegVersion := '2.5';
  fPythonEngine.FatalAbort := False;
  fPythonEngine.FatalMsgDlg := False;
  fPythonEngine.UseLastKnownVersion := False;
  fPythonEngine.AutoFinalize := False;
  fPythonEngine.InitThreads := True;
  fPythonEngine.IO := PythonIIForm.PythonIO;
  fPythonEngine.PyFlags := [pfInteractive];
  fPythonEngine.OnAfterInit := PythonEngineAfterInit;
end;

procedure TInternalPython.PythonEngineAfterInit(Sender: TObject);
Var
  Keywords, Builtins, BuiltInMod : Variant;
  i : integer;
begin
  // Execute initialization script
  with GetPythonEngine do begin
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

end.
