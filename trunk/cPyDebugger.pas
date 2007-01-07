{-----------------------------------------------------------------------------
 Unit Name: cPyDebugger
 Author:    Kiriakos Vlahos
 Date:      23-Feb-2005
 Purpose:   Interpreter and Debugger based on P4D PythonEngine
 History:   Origianlly Based on SynEdit IDE Demo debugger
-----------------------------------------------------------------------------}

unit cPyDebugger;

interface

uses
  Windows, SysUtils, Classes, uEditAppIntfs, PythonEngine, Forms, Contnrs,
  cPyBaseDebugger;

type
  TFrameInfo = class(TBaseFrameInfo)
  private
    fPyFrame : Variant;
  protected
  // Implementation of the Base class for the internal debugger
    function GetFunctionName : string; override;
    function GetFileName : string; override;
    function GetLine : integer; override;
  public
    constructor Create(Frame : Variant);
    property PyFrame : Variant read fPyFrame;
  end;

  TNameSpaceItem = class(TBaseNameSpaceItem)
  // Implementation of the Base class for the internal debugger
  private
    fPyObject : Variant;
    fChildNodes : TStringList;
    fName : string;
  protected
    function GetName : string; override;
    function GetObjectType : string; override;
    function GetValue : string; override;
    function GetDocString : string; override;
    function GetChildCount : integer; override;
    function GetChildNode(Index: integer): TBaseNameSpaceItem; override;
  public
    constructor Create(aName : string; aPyObject : Variant);
    destructor Destroy; override;
    function IsClass : Boolean; override;
    function IsDict : Boolean; override;
    function IsModule : Boolean; override;
    function IsFunction : Boolean; override;
    function IsMethod : Boolean; override;
    function Has__dict__ : Boolean; override;
    function IndexOfChild(AName : string): integer; override;
    procedure GetChildNodes; override;
  end;

  TPyInternalInterpreter = class(TPyBaseInterpreter)
  private
    fII : Variant;  // Python VarPyth wrapper to the interactive interpreter
    fDebugger : Variant;
    fOldargv : Variant;
  public
    constructor Create(II : Variant);
    function SysPathAdd(const Path : WideString) : boolean; override;
    function SysPathRemove(const Path : WideString) : boolean; override;
    procedure SysPathToStrings(Strings : TStrings); override;
    procedure StringsToSysPath(Strings : TStrings); override;
    function Compile(Editor : IEditor) : Variant;
    function GetGlobals : TBaseNameSpaceItem; override;
    function NameSpaceFromExpression(const Expr : string) : TBaseNameSpaceItem; override;
    function CallTipFromExpression(const Expr : string;
      var DisplayString, DocString : string) : Boolean; override;
    procedure SetCommandLine(const ScriptName : string); override;
    procedure RestoreCommandLine; override;
    function ImportModule(Editor : IEditor) : Variant; override;
    procedure RunNoDebug(Editor : IEditor); override;
    function SyntaxCheck(Editor : IEditor; Quiet : Boolean = False) : Boolean;
    function RunSource(const Source, FileName : string) : boolean; override;
    property Debugger : Variant read fDebugger;
    property PyInteractiveInterpreter : Variant read fII;
  end;

  TPyInternalDebugger = class(TPyBaseDebugger)
  // pdb based internal debugger
  private
    fDebuggerCommand : TDebuggerCommand;
    fCurrentFrame : Variant;
    fLineCache : Variant;
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
    constructor Create;

    function SysPathAdd(const Path : WideString) : boolean; override;
    function SysPathRemove(const Path : WideString) : boolean; override;
    procedure Run(Editor : IEditor; InitStepIn : Boolean = False); override;
    procedure RunToCursor(Editor : IEditor; ALine: integer); override;
    procedure StepInto(Editor : IEditor); override;
    procedure StepOver; override;
    procedure StepOut; override;
    procedure Pause; override;
    procedure Abort; override;
    procedure Evaluate(const Expr : string; out ObjType, Value : string); override;
    function RunSource(Const Source, FileName : string) : boolean; override;
    procedure GetCallStack(CallStackList : TObjectList); override;
    function GetFrameGlobals(Frame : TBaseFrameInfo) : TBaseNameSpaceItem; override;
    function GetFrameLocals(Frame : TBaseFrameInfo) : TBaseNameSpaceItem; override;
    function NameSpaceFromExpression(const Expr : string) : TBaseNameSpaceItem; override;
  end;

var
  InternalInterpreter : TPyInternalInterpreter = nil;

implementation

uses dmCommands, frmPythonII, VarPyth, frmMessages, frmPyIDEMain,
  MMSystem, Math, JvDockControlForm, JclFileUtils, Dialogs, uCommonFunctions,
  cParameters, JclSysUtils, StringResources;

{ TFrameInfo }

constructor TFrameInfo.Create(Frame: Variant);
begin
  fPyFrame := Frame;
  inherited Create;
end;

function TFrameInfo.GetFileName: string;
begin
  Result := fPyFrame.f_code.co_filename;
end;

function TFrameInfo.GetFunctionName: string;
begin
  Result := fPyFrame.f_code.co_name;
end;

function TFrameInfo.GetLine: integer;
begin
  Result := fPyFrame.f_lineno;
end;

{ TNameSpaceItem }

constructor TNameSpaceItem.Create(aName : string; aPyObject: Variant);
begin
  fName := aName;
  fPyObject := aPyObject;
end;

destructor TNameSpaceItem.Destroy;
Var
  i : integer;
begin
  if Assigned(fChildNodes) then begin;
    for i := 0 to fChildNodes.Count - 1 do
      fChildNodes.Objects[i].Free;
    fChildNodes.Free;
  end;
  inherited;
end;

function TNameSpaceItem.GetChildCount: integer;
var
  i : integer;
  SupressOutput : IInterface;
begin
  if Assigned(fChildNodes) then
    Result := fChildNodes.Count
  else begin
    SupressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
    try
      if VarIsPythonDict(fPyObject) then
        Result := len(fPyObject)
      else if SortedIdentToInt(_type(fPyObject).__name__, i, CommonTypes, True) then
        Result := 0  // do not expand common types
      else if Has__dict__ then
        Result := len(fPyObject.__dict__)
      else begin
        Result := len(Import('inspect').getmembers(fPyObject));
      end;
    except
      Result := 0;
    end;
  end;
end;

function TNameSpaceItem.GetChildNode(Index: integer): TBaseNameSpaceItem;
begin
  Assert(Index >= 0, 'TNameSpaceItem.GetChildNode');
  if not Assigned(fChildNodes) then
    GetChildNodes;
  Assert(Index < fChildNodes.Count, 'TNameSpaceItem.GetChildNode');
  Result := fChildNodes.Objects[Index] as TBaseNameSpaceItem;
end;

procedure TNameSpaceItem.GetChildNodes;
Var
  Dict, DictKeys : Variant;
  Count, i : integer;
  Name : string;
  NameSpaceItem : TNameSpaceItem;
  SupressOutput : IInterface;
begin
  if not Assigned(fChildNodes) then begin
    SupressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
    try
      if VarIsPythonDict(fPyObject) then begin
        Dict := fPyObject;
        Count := len(fPyObject);
      end else if SortedIdentToInt(_type(fPyObject).__name__, i, CommonTypes, True) then
        Count := 0  // do not expand common types
      else begin
        Dict := BuiltinModule.dict(Import('inspect').getmembers(fPyObject));
        Count := len(Dict);
      end;

      if Count > 0 then begin
        fChildNodes := TStringList.Create;
        fChildNodes.CaseSensitive := True;
        fChildNodes.Duplicates := dupAccept;
        DictKeys := Dict.keys();
        for i := 0 to Count - 1 do begin
          Name :=  DictKeys.GetItem(i);
          NameSpaceItem := TNameSpaceItem.Create(Name,
            Dict.GetItem(DictKeys.GetItem(i)));
          fChildNodes.AddObject(Name, NameSpaceItem);
        end;
        fChildNodes.CustomSort(ComparePythonIdents);
        GotChildNodes := True;
      end;
    except
      // fail quietly
    end;
  end;
end;

function TNameSpaceItem.GetDocString: string;
Var
  SupressOutput : IInterface;
begin
  SupressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
  try
    Result := Import('inspect').getdoc(fPyObject);
  except
    Result := '';
  end;
end;

function TNameSpaceItem.GetName: string;
begin
  Result := fName;
end;

function TNameSpaceItem.GetObjectType: string;
Var
  SupressOutput : IInterface;
begin
  SupressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
  try
    Result := _type(fPyObject).__name__;
  except
    Result := '';
  end;
end;

function TNameSpaceItem.GetValue: string;
Var
  SupressOutput : IInterface;
begin
  SupressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
  try
    Result :=  InternalInterpreter.PyInteractiveInterpreter.saferepr(fPyObject);
  except
    Result := '';
  end;
end;

function TNameSpaceItem.Has__dict__: Boolean;
begin
  Result := (GetPythonEngine.PyObject_HasAttrString(
        ExtractPythonObjectFrom(fPyObject), '__dict__') = 1) and
        VarIsPythonDict(fPyObject.__dict__) // it can be None with extensions!;
end;

function TNameSpaceItem.IndexOfChild(AName: string): integer;
begin
  if not Assigned(fChildNodes) then
    GetChildNodes;
  if Assigned(fChildNodes) then
    Result := fChildNodes.IndexOf(AName)
  else
    Result := -1;
end;

function TNameSpaceItem.IsClass: Boolean;
begin
  Result := VarIsPythonClass(fPyObject);
end;

function TNameSpaceItem.IsDict: Boolean;
begin
  Result := VarIsPythonDict(fPyObject);
end;

function TNameSpaceItem.IsFunction: Boolean;
begin
  Result := VarIsPythonFunction(fPyObject);
end;

function TNameSpaceItem.IsMethod: Boolean;
begin
  Result := VarIsPythonMethod(fPyObject);
end;

function TNameSpaceItem.IsModule: Boolean;
begin
  Result := VarIsPythonModule(fPyObject);
end;

{ TPyDebugger }

constructor TPyInternalDebugger.Create;
begin
  inherited Create;
  fLineCache := Import('linecache');
  fDebuggerCommand := dcNone;
end;

procedure TPyInternalDebugger.Evaluate(const Expr : string; out ObjType, Value : string);
Var
  SupressOutput : IInterface;
  V : Variant;
begin
  ObjType := SNotAvailable;
  Value := SNotAvailable;
  if PyControl.DebuggerState = dsPaused then begin
    SupressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
    try
      V := BuiltInModule.eval(Expr,
             CurrentFrame.f_globals, CurrentFrame.f_locals);
      ObjType := BuiltInModule.type(V).__name__;
      Value := InternalInterpreter.PyInteractiveInterpreter.saferepr(V);
    except
      // fail quitely
    end;
  end;
end;

procedure TPyInternalDebugger.GetCallStack(CallStackList: TObjectList);
Var
  Frame : Variant;
begin
  CallStackList.Clear;
  if PyControl.DebuggerState <> dsPaused then
    Exit;
  Frame := CurrentFrame;
  if VarIsPython(Frame) then
    // We do not show the last two frames
    while not VarIsNone(Frame.f_back) and not VarIsNone(Frame.f_back.f_back) do begin
      CallStackList.Add(TFrameInfo.Create(Frame));
      Frame := Frame.f_back;
    end;
end;

function TPyInternalDebugger.GetFrameGlobals(Frame: TBaseFrameInfo): TBaseNameSpaceItem;
begin
  Result := nil;
  if PyControl.DebuggerState <> dsPaused then
    Exit;
  Result := TNameSpaceItem.Create('globals', (Frame as TFrameInfo).fPyFrame.f_globals);
end;

function TPyInternalDebugger.GetFrameLocals(Frame: TBaseFrameInfo): TBaseNameSpaceItem;
begin
  Result := nil;
  if PyControl.DebuggerState <> dsPaused then
    Exit;
  Result := TNameSpaceItem.Create('locals', (Frame as TFrameInfo).fPyFrame.f_locals);
end;

procedure TPyInternalDebugger.Pause;
// Not pausible to implement with the debugger running in the main thread
begin
  if PyControl.DebuggerState = dsRunning then
    PyControl.DoStateChange(dsPaused);
end;

// Timer callback function
procedure TPyInternalDebugger.RestoreCommandLine;
begin
  InternalInterpreter.RestoreCommandLine;
end;

procedure TPyInternalDebugger.Run(Editor : IEditor; InitStepIn : Boolean = False);
var
  OldPS1, OldPS2 : string;
  Code : Variant;
  Path, OldPath : string;
  PythonPathAdder : IInterface;
begin
  // Repeat here to make sure it is set right
  MaskFPUExceptions(CommandsDataModule.PyIDEOptions.MaskFPUExceptions);

  fDebuggerCommand := dcRun;
  if (PyControl.DebuggerState <> dsInactive) then exit; // pass control to user_line

  //Compile
  Code := InternalInterpreter.Compile(Editor);

  if VarIsPython(Code) then with PythonIIForm do begin
    Path := ExtractFilePath(Editor.FileName);
    OldPath := GetCurrentDir;
    if Length(Path) > 1 then begin
      Path := PathRemoveSeparator(Path);
      // Add the path of the executed file to the Python path - Will be automatically removed
      PythonPathAdder := AddPathToPythonPath(Path);

      // Change the current path
      try
        ChDir(Path)
      except
        MessageDlg('Could not set the current directory to the script path', mtWarning, [mbOK], 0);
      end;
    end;

    PyControl.DoStateChange(dsRunning);

    // Set the layout to the Debug layout is it exists
    MessagesWindow.ClearMessages;
    if PyIDEMainForm.Layouts.IndexOf('Debug') >= 0 then begin
      PyIDEMainForm.SaveLayout('Current');
      PyIDEMainForm.LoadLayout('Debug');
      Application.ProcessMessages;
    end else
      ShowDockForm(PythonIIForm);

    try
      OldPS1 := PS1;
      PS1 := '[Dbg]' + PS1;
      OldPS2 := PS2;
      PS2 := '[Dbg]' + PS2;
      AppendText(sLineBreak+PS1);
      //attach debugger callback routines
      DebugIDE.Events.Items[0].OnExecute := UserCall;
      DebugIDE.Events.Items[1].OnExecute := UserLine;
      DebugIDE.Events.Items[2].OnExecute := UserReturn;
      DebugIDE.Events.Items[3].OnExecute := UserException;
      DebugIDE.Events.Items[4].OnExecute := UserYield;
      //set breakpoints
      SetDebuggerBreakPoints;

      // New Line for output
      PythonIIForm.AppendText(sLineBreak);

      // Set the command line parameters
      SetCommandLine(Editor.GetFileNameOrTitle);

      InternalInterpreter.Debugger.InitStepIn := InitStepIn;
      try
        InternalInterpreter.Debugger.run(Code);
      except
        // CheckError already called by VarPyth
        on E: EPyException do begin
          InternalInterpreter.HandlePyException(E);
          MessageDlg(E.Message, mtError, [mbOK], 0);
          SysUtils.Abort;
        end;
      end;

    finally
      PS1 := OldPS1;
      PS2 := OldPS2;
      AppendText(sLineBreak+PS1);

      // Restore the command line parameters
      RestoreCommandLine;

      // Change the back current path
      ChDir(OldPath);

      if PyIDEMainForm.Layouts.IndexOf('Debug') >= 0 then
        PyIDEMainForm.LoadLayout('Current');

      PyControl.DoStateChange(dsInactive);
    end;
  end;
end;

function TPyInternalDebugger.RunSource(const Source, FileName: string): boolean;
// The internal interpreter RunSource calls II.runsource which differs
// according to whether we debugging or not
begin
  Result := InternalInterpreter.RunSource(Source, FileName);
end;

procedure TPyInternalDebugger.RunToCursor(Editor : IEditor; ALine: integer);
Var
  FName : string;
begin
  // Set Temporary breakpoint
  SetDebuggerBreakPoints;  // So that this one is not cleared
  FName := Editor.FileName;
  if FName = '' then
    FName := '<'+Editor.FileTitle+'>';
  InternalInterpreter.Debugger.set_break(FName, ALine, 1);

  if PyControl.DebuggerState = dsInactive then
    Run(Editor)
  else
    fDebuggerCommand := dcRunToCursor;
end;

procedure TPyInternalDebugger.StepInto(Editor : IEditor);
begin
  if PyControl.DebuggerState = dsInactive then
    Run(Editor, True)
  else
    fDebuggerCommand := dcStepInto;
end;

procedure TPyInternalDebugger.StepOver;
begin
  fDebuggerCommand := dcStepOver;
end;

function TPyInternalDebugger.SysPathAdd(const Path: WideString): boolean;
begin
  Result := InternalInterpreter.SysPathAdd(Path);
end;

function TPyInternalDebugger.SysPathRemove(const Path: WideString): boolean;
begin
  Result := InternalInterpreter.SysPathRemove(Path);
end;

procedure TPyInternalDebugger.StepOut;
begin
  fDebuggerCommand := dcStepOut;
end;

procedure TPyInternalDebugger.Abort;
begin
  fDebuggerCommand := dcAbort;
end;

procedure TPyInternalDebugger.UserCall(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
begin
   // PythonIIForm.AppendText('UserCall'+sLineBreak);
   Result := GetPythonEngine.ReturnNone;
end;

procedure TPyInternalDebugger.UserException(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
   // PythonIIForm.AppendText('UserException'+sLineBreak);
   Result := GetPythonEngine.ReturnNone;
end;

procedure TPyInternalDebugger.UserLine(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
Var
  Frame : Variant;
  FName : string;
begin
   Result := GetPythonEngine.ReturnNone;

   Frame := VarPythonCreate(Args).GetItem(0);
   FName := Frame.f_code.co_filename;
   if (FName[1] ='<') and (FName[Length(FName)] = '>') then
     FName :=  Copy(FName, 2, Length(FName)-2);
   // PythonIIForm.AppendText('UserLine '+ FName + ' ' + IntToStr(Frame.f_lineno) +sLineBreak);

   if PyIDEMainForm.ShowFilePosition(FName, Frame.f_lineno, 1) and
     (Frame.f_lineno > 0) then
   begin
     PyControl.CurrentPos.Editor := GI_EditorFactory.GetEditorByNameOrTitle(FName);
     PyControl.CurrentPos.Line := Frame.f_lineno;
     FCurrentFrame := Frame;

     Pause;
     FDebuggerCommand := dcNone;
     While  FDebuggerCommand = dcNone do
       PyControl.DoYield(True);

     VarClear(fCurrentFrame);
     if PyControl.BreakPointsChanged then SetDebuggerBreakpoints;

     PyControl.DoStateChange(dsRunning);
   end;

   case fDebuggerCommand of
     dcRun         : InternalInterpreter.Debugger.set_continue();
     dcStepInto    : InternalInterpreter.Debugger.set_step();
     dcStepOver    : InternalInterpreter.Debugger.set_next(Frame);
     dcStepOut     : InternalInterpreter.Debugger.set_return(Frame);
     dcRunToCursor : InternalInterpreter.Debugger.set_continue;
     dcAbort       : begin
                       InternalInterpreter.Debugger.set_quit();
                       MessagesWindow.AddMessage('Debugging Aborted');
                       MessagesWindow.ShowWindow;
                     end;
   end;

end;

procedure TPyInternalDebugger.UserReturn(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
begin
   // PythonIIForm.AppendText('UserReturn'+sLineBreak);
   Result := GetPythonEngine.ReturnNone;
end;

procedure TPyInternalDebugger.UserYield(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
begin
  PyControl.DoYield(False);
  if fDebuggerCommand = dcAbort then begin
    InternalInterpreter.Debugger.set_quit();
    MessagesWindow.AddMessage('Debugging Aborted');
    MessagesWindow.ShowWindow;
  end;
  Result := GetPythonEngine.ReturnNone;
end;

procedure TPyInternalDebugger.SetCommandLine(const ScriptName: string);
begin
  InternalInterpreter.SetCommandLine(ScriptName);
end;

procedure TPyInternalDebugger.SetDebuggerBreakpoints;
var
  i, j : integer;
  FName : string;
  V : Variant;
begin
  if not PyControl.BreakPointsChanged then Exit;
  LoadLineCache;
  InternalInterpreter.Debugger.clear_all_breaks();
  for i := 0 to GI_EditorFactory.Count - 1 do
    with GI_EditorFactory.Editor[i] do begin
      FName := FileName;
      if FName = '' then
        FName := '<'+FileTitle+'>';
      for j := 0 to BreakPoints.Count - 1 do begin
        V := InternalInterpreter.Debugger.set_break;
        if TBreakPoint(BreakPoints[j]).Condition <> '' then begin
          GetPythonEngine.EvalFunction(ExtractPythonObjectFrom(V),
            [FName, TBreakPoint(BreakPoints[j]).LineNo,
            0, TBreakPoint(BreakPoints[j]).Condition]);
           //  To avoid the implicit conversion of the filename to unicode
           //  which causes problems due to the strange behaviour of os.path.normcase
          //InternalInterpreter.Debugger.set_break(FName, TBreakPoint(BreakPoints[j]).LineNo,
          //  0, TBreakPoint(BreakPoints[j]).Condition);
        end else
          GetPythonEngine.EvalFunction(ExtractPythonObjectFrom(V),
            [FName, TBreakPoint(BreakPoints[j]).LineNo]);
          //InternalInterpreter.Debugger.set_break(FName, TBreakPoint(BreakPoints[j]).LineNo);
      end;
    end;
  PyControl.BreakPointsChanged := False;
end;

procedure TPyInternalDebugger.LoadLineCache;
Var
  i : integer;
  Source : string;
begin
  // inject unsaved code into LineCache
  fLineCache.cache.clear();
  for i := 0 to GI_EditorFactory.Count - 1 do
    with GI_EditorFactory.Editor[i] do
      if HasPythonfile and (FileName = '') then
      begin
        Source := CleanEOLs(EncodedText)+#10;
        fLineCache.cache.SetItem('<'+FileTitle+'>',
          VarPythonCreate([0,0,Source, '<'+FileTitle+'>'], stTuple));
      end;
end;

function TPyInternalDebugger.NameSpaceFromExpression(
  const Expr: string): TBaseNameSpaceItem;
//  The internal interpreter knows whether we are debugging and
//  adjusts accordingly (in II.debugger.evalcode)
begin
  Result := InternalInterpreter.NameSpaceFromExpression(Expr);
end;

{ TPyInternalInterpreter }

constructor TPyInternalInterpreter.Create(II: Variant);
begin
  fII := II;
  fDebugger := II.debugger;
end;

function TPyInternalInterpreter.CallTipFromExpression(const Expr: string;
  var DisplayString, DocString: string) : Boolean;
var
  LookupObj, PyDocString: Variant;
  SupressOutput : IInterface;
begin
  Result := False;
  DisplayString := '';
  DocString := '';
  if Expr = '' then Exit;

  SupressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
  try
    //Evaluate the lookup expression and get the hint text
    LookupObj := fII.evalcode(Expr);
    DisplayString := fII.get_arg_text(LookupObj);
    Result := True;

    PyDocString := Import('inspect').getdoc(LookupObj);
    if not VarIsNone(PyDocString) then
      DocString := GetNthLine(PyDocString, 1)
    else
      DocString := '';
  except
  end;
end;

function TPyInternalInterpreter.Compile(Editor: IEditor): Variant;
Var
  co : PPyObject;
  FName, Source : string;
begin
  VarClear(Result);
  PyControl.ErrorPos.Clear;
  PyControl.DoErrorPosChanged;

  MessagesWindow.ClearMessages;

  FName := Editor.FileName;
  if FName = '' then FName := '<'+Editor.FileTitle+'>';
  Source := CleanEOLs(Editor.EncodedText)+#10;

  with GetPythonEngine do begin
    co := Py_CompileString(PChar(Source), PChar(FName), file_input );
    if not Assigned( co ) then begin
      // New Line for output
      PythonIIForm.AppendText(sLineBreak);
      try
      // Print and throw exception for the error
       CheckError;
      except
        on E: EPySyntaxError do begin
          MessagesWindow.ShowPythonSyntaxError(E);
          if PyIDEMainForm.ShowFilePosition(E.EFileName, E.ELineNumber, E.EOffset) and
            Assigned(GI_ActiveEditor)
          then begin
            PyControl.ErrorPos.Editor := GI_ActiveEditor;
            PyControl.ErrorPos.Line := E.ELineNumber;
            PyControl.ErrorPos.Char := E.EOffset;
            PyControl.ErrorPos.IsSyntax := True;
            PyControl.DoErrorPosChanged;
          end;
          MessageDlg(E.Message, mtError, [mbOK], 0);
          SysUtils.Abort;
        end;
        on E: EPyException do begin  //may raise OverflowError or ValueError
          HandlePyException(E);
          MessageDlg(E.Message, mtError, [mbOK], 0);
          SysUtils.Abort;
        end;
      end;
    end else begin
      Result := VarPythonCreate(co);
      // Cleanup ByteCode
      GetPythonEngine.Py_XDECREF(co);
    end;
  end;
end;

function TPyInternalInterpreter.GetGlobals: TBaseNameSpaceItem;
begin
  Result := TNameSpaceItem.Create('globals', VarPythonEval('globals()'));
end;

function TPyInternalInterpreter.ImportModule(Editor: IEditor) : Variant;
{
  Imports Editor text without saving the file.
  Does not add the module name to the locals()
  of the interpreter.
}
Var
  Code : Variant;
  Path, NameOfModule : string;
  PyObject, Module : PPyObject;
  PythonPathAdder : IInterface;
begin
  VarClear(Result);
  //Compile
  Code := Compile(Editor);

  Path := ExtractFileDir(Editor.FileName);
  if Length(Path) > 1 then begin
    // Add the path of the executed file to the Python path
    PythonPathAdder := AddPathToPythonPath(Path, False);
  end;

  if Editor.FileName <> '' then
    NameOfModule := FileNameToModuleName(Editor.FileName)
  else
    NameOfModule := PathRemoveExtension(Editor.FileTitle);
  PyObject := ExtractPythonObjectFrom(Code);

  PyControl.DoStateChange(dsRunningNoDebug);

  try
    with GetPythonEngine do begin
      Py_XINCREF(PyObject);
      try
        try
          Module := PyImport_ExecCodeModule(NameOfModule, PyObject);
          Result := VarPythonCreate(Module);
          Py_XDECREF(Module);
        finally
          Py_XDECREF(PyObject);
        end;
        CheckError;
      except
        on E: EPyException do begin
          HandlePyException(E);
          Result := None;
        end;
      end;
    end;
    if VarIsNone(Result) then begin
      MessageDlg('Error in importing module', mtError, [mbOK], 0);
      SysUtils.Abort;
    end;
  finally
    PyControl.DoStateChange(dsInactive);
  end;
end;

function TPyInternalInterpreter.NameSpaceFromExpression(
  const Expr: string): TBaseNameSpaceItem;
var
  InspectModule, LookupObj, ItemsDict: Variant;
  SupressOutput : IInterface;
begin
  SupressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
  InspectModule := Import('inspect');
  ItemsDict := NewPythonDict;
  try
    if Expr <> '' then begin
        //Evaluate the lookup expression and get the hint text
        LookupObj := fII.evalcode(Expr);
        ItemsDict := BuiltinModule.dict(InspectModule.getmembers(LookupObj));
    end else begin
      ItemsDict.update(fII.evalcode('vars()'));
      ItemsDict.update(BuiltinModule.__dict__);
    end;
  except
    // Ignore exceptions
  end;
  Result := TNameSpaceItem.Create(Expr, ItemsDict);
end;

procedure TPyInternalInterpreter.RestoreCommandLine;
begin
  SysModule.argv := fOldargv;
end;

procedure TPyInternalInterpreter.SetCommandLine(const ScriptName : string);
var
  SysMod : Variant;
  S, Param : string;
  P : PChar;
//  List : TStringList;
//  I: integer;
begin
  SysMod := SysModule;
  fOldargv := SysMod.argv;
  SysMod.argv := NewPythonList;
  // Workaround due to PREFER_UNICODE flag to make sure
  // no conversion to Unicode and back will take place
  SysMod.argv.append(VarPythonCreate(ScriptName));
  S := iff(CommandsDataModule.PyIDEOptions.UseCommandLine,
     CommandsDataModule.PyIDEOptions.CommandLine, '');
  if S <> '' then begin
    S := Parameters.ReplaceInText(S);
    P := GetParamStr(PChar(S), Param);
    while Param <> '' do begin
      SysMod.argv.append(VarPythonCreate(Param));
      P := GetParamStr(P, Param);
    end;
    PythonIIForm.AppendText(Format(SCommandLineMsg, [S]));
  end;
//  List := TStringList.Create;
//  try
//    ExtractStrings([' '], [' '], PChar(S), List);
//    for I := 0 to List.Count - 1 do
//      SysMod.argv.append(AnsiDequotedStr(List[i], '"'));
//  finally
//    List.Free;
//  end;
end;

procedure TPyInternalInterpreter.StringsToSysPath(Strings: TStrings);
var
  i: Integer;
  PythonPath: Variant;
begin
  PythonPath := NewPythonList;
  for i := 0 to Strings.Count - 1 do
    PythonPath.append(Strings[i]);
  SysModule.path := PythonPath;
end;

procedure TimeCallBack(TimerID, Msg: Uint; dwUser, dw1, dw2: DWORD); stdcall;
begin
  if PLongWord(dwUser)^ = 0 then Exit;
  if (MessageBox(0, 'The Python Script has timed out.  Do you want to interrupt it?',
  'ScriptTimeOut', MB_ICONWARNING or MB_YESNO) = idYes) then begin
    GetPythonEngine.PyErr_SetInterrupt;  //Generate Keyboard interrupt signal
    TimeKillEvent(PLongWord(dwUser)^);
    PLongWord(dwUser)^ := 0;
  end;
end;

procedure TPyInternalInterpreter.RunNoDebug(Editor: IEditor);
Var
  Code : Variant;
  mmResult,Resolution : LongWord;
  tc : TTimeCaps;
  Path, OldPath : string;
  PythonPathAdder : IInterface;
begin
  // Repeat here to make sure it is set right
  MaskFPUExceptions(CommandsDataModule.PyIDEOptions.MaskFPUExceptions);

  //Compile
  Code := Compile(Editor);

  PyControl.DoStateChange(dsRunningNoDebug);

  // New Line for output
  PythonIIForm.AppendText(sLineBreak);

  mmResult := 0;
  Resolution := 100;

  Path := ExtractFileDir(Editor.FileName);
  OldPath := GetCurrentDir;
  if Length(Path) > 1 then begin
    // Add the path of the executed file to the Python path - Will be automatically removed
    PythonPathAdder := AddPathToPythonPath(Path);

    // Change the current path
    try
      ChDir(Path)
    except
      MessageDlg('Could not set the current directory to the script path', mtWarning, [mbOK], 0);
    end;
  end;

  // Set the command line parameters
  SetCommandLine(Editor.GetFileNameOrTitle);

  ShowDockForm(PythonIIForm);

  try
    // Set Multimedia Timer
    if (CommandsDataModule.PyIDEOptions.TimeOut > 0) and
      (timeGetDevCaps(@tc, SizeOf(tc))=TIMERR_NOERROR) then
    begin
      Resolution := Min(Resolution,tc.wPeriodMax);
      TimeBeginPeriod(Resolution);
      mmResult := TimeSetEvent(CommandsDataModule.PyIDEOptions.TimeOut, resolution,
        @TimeCallBack, DWORD(@mmResult), TIME_PERIODIC or 256);
    end;
    try
      Debugger.run_nodebug(Code);
    except
      // CheckError already called by VarPyth
      on E: EPyException do begin
        HandlePyException(E);
        MessageDlg(E.Message, mtError, [mbOK], 0);
        SysUtils.Abort;
      end;
    end;
  finally
    if CommandsDataModule.PyIDEOptions.TimeOut > 0 then begin
      if (mmResult <> 0) then TimeKillEvent(mmResult);
      TimeEndPeriod(Resolution);
    end;
    PythonIIForm.AppendText(sLineBreak+PythonIIForm.PS1);

    // Restore the command line parameters
    RestoreCommandLine;

    // Change the back current path
    ChDir(OldPath);

    PyControl.DoStateChange(dsInactive);
  end;
end;

function TPyInternalInterpreter.RunSource(const Source, FileName : string): boolean;
begin
  // Workaround due to PREFER_UNICODE flag to make sure
  // no conversion to Unicode and back will take place
  Result := fII.runsource(VarPythonCreate(Source), FileName);
end;

function TPyInternalInterpreter.SyntaxCheck(Editor: IEditor; Quiet : Boolean = False): Boolean;
Var
  FName, Source : string;
  tmp: PPyObject;
  PyErrType, PyErrValue, PyErrTraceback, PyErrValueTuple : PPyObject;
  SupressOutput : IInterface;
begin
  PyControl.ErrorPos.Clear;
  PyControl.DoErrorPosChanged;

  MessagesWindow.ClearMessages;

  FName := Editor.FileName;
  if FName = '' then FName := '<'+Editor.FileTitle+'>';
  Source := CleanEOLs(Editor.EncodedText)+#10;

  with GetPythonEngine do begin
    if Quiet then
      SupressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
    Result := CheckExecSyntax(Source);
    if not Result then begin
      if Quiet then begin
        if Assigned(PyErr_Occurred()) then begin
          if (PyErr_ExceptionMatches(PyExc_SyntaxError^) = 1) then begin
            PyErr_Fetch(@PyErrType, @PyErrValue, @PyErrTraceback);  // Clears the Error
            if Assigned(PyErrValue) then begin
            // Sometimes there's a tuple instead of instance...
              if PyTuple_Check( PyErrValue )  and (PyTuple_Size( PyErrValue) >= 2) then
              begin
                PyControl.ErrorPos.ErrorMsg := PyString_AsString(PyTuple_GetItem( PyErrValue, 0));
                PyErrValueTuple := PyTuple_GetItem( PyErrValue, 1);
                if PyTuple_Check( PyErrValueTuple )  and (PyTuple_Size( PyErrValueTuple) >= 4) then
                begin
                  PyControl.ErrorPos.Line := PyInt_AsLong(PyTuple_GetItem( PyErrValueTuple, 1));
                  PyControl.ErrorPos.Char := PyInt_AsLong(PyTuple_GetItem( PyErrValueTuple, 2));
                end;
              end else
                // Is it an instance of the SyntaxError class ?
              if PyInstance_Check( PyErrValue ) or (PyType_IsSubtype(PyErrValue^.ob_type,
                PPyTypeObject(GetPythonEngine.PyExc_SyntaxError^)) = 1) then
              begin
                // Get the text containing the error, cut of carriage return
                tmp := PyObject_GetAttrString(PyErrValue, 'text');
                if Assigned(tmp) and PyString_Check(tmp) then
                  PyControl.ErrorPos.ErrorMsg := Trim(PyString_AsString(tmp));
                Py_XDECREF(tmp);
                // Get the offset where the error should appear
                tmp := PyObject_GetAttrString(PyErrValue, 'offset' );
                if Assigned(tmp) and PyInt_Check(tmp) then
                  PyControl.ErrorPos.Char := PyInt_AsLong(tmp);
                Py_XDECREF(tmp);
                // Get the line number of the error
                tmp := PyObject_GetAttrString(PyErrValue, 'lineno' );
                if Assigned(tmp) and PyInt_Check(tmp) then
                  PyControl.ErrorPos.Line := PyInt_AsLong(tmp);
                Py_XDECREF(tmp);
                PyErr_Clear;
              end;
              PyControl.ErrorPos.Editor := Editor;
              PyControl.ErrorPos.IsSyntax := True;
              PyControl.DoErrorPosChanged;
            end;
            Py_XDECREF(PyErrType);
            Py_XDECREF(PyErrValue);
            Py_XDECREF(PyErrTraceback);
          end else
            PyErr_Clear;
        end;
      end else begin
        // Display error
        // New Line for output
        PythonIIForm.AppendText(sLineBreak);
        try
          // Print and throw exception for the error
          CheckError;
        except
          on E: EPySyntaxError do begin
            E.EFileName := FName;  // add the filename
            MessagesWindow.ShowPythonSyntaxError(E);
            if PyIDEMainForm.ShowFilePosition(E.EFileName, E.ELineNumber, E.EOffset) and
              Assigned(GI_ActiveEditor)
            then begin
              PyControl.ErrorPos.Editor := GI_ActiveEditor;
            end;
            PyControl.ErrorPos.Line := E.ELineNumber;
            PyControl.ErrorPos.Char := E.EOffset;
            PyControl.ErrorPos.IsSyntax := True;
            PyControl.DoErrorPosChanged;
            if Not Quiet then MessageDlg(E.Message, mtError, [mbOK], 0);
          end;
        end;
      end;
    end;
  end;
end;

function TPyInternalInterpreter.SysPathAdd(const Path: WideString): boolean;
begin
  if SysModule.path.contains(Path) then
    Result := false
  else begin
    SysModule.path.insert(0, Path);
    Result := true;
  end;
end;

function TPyInternalInterpreter.SysPathRemove(const Path: WideString): boolean;
begin
  if SysModule.path.contains(Path) then begin
    Result := True;
    SysModule.path.remove(Path);
  end else
    Result := False;
end;

procedure TPyInternalInterpreter.SysPathToStrings(Strings: TStrings);
var
  i: Integer;
begin
  for i := 0 to Len(SysModule.path) - 1  do
    Strings.Add(SysModule.path.GetItem(i));
end;

end.








