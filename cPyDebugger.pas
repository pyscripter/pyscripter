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
  protected
    fPyObject : Variant;
    fChildCount : integer;
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
    function ImportModule(Editor : IEditor; AddToNameSpace : Boolean = False) : Variant; override;
    procedure RunNoDebug(Editor : IEditor); override;
    function SyntaxCheck(Editor : IEditor; Quiet : Boolean = False) : Boolean;
    function RunSource(const Source, FileName : string; symbol : string = 'single') : boolean; override;
    function EvalCode(const Expr : string) : Variant; override;
    property Debugger : Variant read fDebugger;
    property PyInteractiveInterpreter : Variant read fII;
  end;

  TPyInternalDebugger = class(TPyBaseDebugger)
  // pdb based internal debugger
  private
    fDebuggerCommand : TDebuggerCommand;
    fCurrentFrame : Variant;
    fLineCache : Variant;
    fOldPS1, fOldPS2 : string;
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

var
  InternalInterpreter : TPyInternalInterpreter = nil;

implementation

uses dmCommands, frmPythonII, VarPyth, frmMessages, frmPyIDEMain,
  MMSystem, Math, JvDockControlForm, JclFileUtils, uCommonFunctions,
  cParameters, JclSysUtils, StringResources, Dialogs, JvDSADialogs;

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
  fChildCount := -1;  // unknown
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
  SuppressOutput : IInterface;
begin
  if Assigned(fChildNodes) then
    Result := fChildNodes.Count
  else if fChildCount >= 0 then
    Result := fChildCount
  else begin
    SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
    try
      if IsDict then
        Result := len(fPyObject)
      else if SortedIdentToInt(ObjectType, i, CommonTypes, True) then
        Result := 0  // do not expand common types
//      else if Has__dict__ then
//        Result := len(fPyObject.__dict__)
      else begin
        Result := len(InternalInterpreter.PyInteractiveInterpreter.safegetmembers(fPyObject));
      end;
    except
      Result := 0;
    end;
    fChildCount := Result;
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
  Dict, DictKeys, APyObject : Variant;
  i : integer;
  Name : string;
  NameSpaceItem : TNameSpaceItem;
  SuppressOutput : IInterface;
begin
  if not (Assigned(fChildNodes) or GotChildNodes) then begin
    GotChildNodes := True;
    SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
    try
      if IsDict then begin
        Dict := fPyObject;
        fChildCount := len(Dict);
      end else if SortedIdentToInt(ObjectType, i, CommonTypes, True) then
        fChildCount := 0  // do not expand common types
      else begin
        Dict := InternalInterpreter.PyInteractiveInterpreter.safegetmembers(fPyObject);
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
          except
            APyObject := VarPythonCreate('Error in getting object');
          end;
          NameSpaceItem := TNameSpaceItem.Create(Name, APyObject);
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

function TNameSpaceItem.GetDocString: string;
Var
  SuppressOutput : IInterface;
begin
  SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
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
  SuppressOutput : IInterface;
begin
  SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
  try
    if GetPythonEngine.PyObject_HasAttrString(
      ExtractPythonObjectFrom(fPyObject), '__class__') <> 0
    then
      Result := fPyObject.__class__.__name__
    else
      Result := _type(fPyObject).__name__;
  except
    Result := '';
  end;
end;

function TNameSpaceItem.GetValue: string;
Var
  SuppressOutput : IInterface;
begin
  SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
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
var
  L, H, I, C: Integer;
  Found : Boolean;
begin
  if not Assigned(fChildNodes) then
    GetChildNodes;
  if Assigned(fChildNodes) then begin
    // Child nodes are sorted with ComparePythonIdents
    // Do a quick search
    Result := -1;
    Found := False;
    L := 0;
    H := fChildNodes.Count - 1;
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := ComparePythonIdents(fChildNodes[I], AName);
      if C < 0 then L := I + 1 else
      begin
        H := I - 1;
        if C = 0 then
        begin
          Found := True;
          L := I;
        end;
      end;
    end;
    if Found then
      Result := L;
    //Result := fChildNodes.IndexOf(AName)
  end else
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
  PyControl.BreakPointsChanged := True;
end;

destructor TPyInternalDebugger.Destroy;
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

procedure TPyInternalDebugger.EnterPostMortem;
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

  TraceBack := SysModule.last_traceback;
  InternalInterpreter.Debugger.botframe := TraceBack.tb_frame;
  while not VarIsNone(TraceBack.tb_next) do
    TraceBack := TraceBack.tb_next;
  fCurrentFrame := TraceBack.tb_frame;

  PyControl.DoStateChange(dsPostMortem);
  DSAMessageDlg(dsaPostMortemInfo, 'PyScripter', SPostMortemInfo,
     mtInformation, [mbOK], 0, dckActiveForm, 0, mbOK);
end;

procedure TPyInternalDebugger.Evaluate(const Expr : string; out ObjType, Value : string);
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
      V := InternalInterpreter.PyInteractiveInterpreter.evalcode(Expr);
      ObjType := InternalInterpreter.PyInteractiveInterpreter.objecttype(V);
      Value := InternalInterpreter.PyInteractiveInterpreter.saferepr(V);
    except
      // fail quitely
    end;
  end;
end;

procedure TPyInternalDebugger.ExitPostMortem;
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

procedure TPyInternalDebugger.GetCallStack(CallStackList: TObjectList);
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
      if VarIsSame(Frame, InternalInterpreter.Debugger.botframe) then
        break;
    end;
end;

function TPyInternalDebugger.GetFrameGlobals(Frame: TBaseFrameInfo): TBaseNameSpaceItem;
begin
  Result := nil;
  if not (PyControl.DebuggerState in [dsPaused, dsPostMortem]) then
    Exit;
  Result := TNameSpaceItem.Create('globals', (Frame as TFrameInfo).fPyFrame.f_globals);
end;

function TPyInternalDebugger.GetFrameLocals(Frame: TBaseFrameInfo): TBaseNameSpaceItem;
begin
  Result := nil;
  if not (PyControl.DebuggerState in [dsPaused, dsPostMortem]) then
    Exit;
  Result := TNameSpaceItem.Create('locals', (Frame as TFrameInfo).fPyFrame.f_locals);
end;

function TPyInternalDebugger.HaveTraceback: boolean;
begin
  try
    Result := VarModuleHasObject(SysModule, 'last_traceback');
  except
    Result := False;
  end;
end;

procedure TPyInternalDebugger.Pause;
begin
  fDebuggerCommand := dcPause;
end;

// Timer callback function
procedure TPyInternalDebugger.RestoreCommandLine;
begin
  InternalInterpreter.RestoreCommandLine;
end;

procedure TPyInternalDebugger.Run(Editor : IEditor; InitStepIn : Boolean = False);
var
  Code : Variant;
  Path, OldPath : string;
  PythonPathAdder : IInterface;
  ReturnFocusToEditor: Boolean;
  CanDoPostMortem : Boolean;
begin
  CanDoPostMortem := False;

  // Repeat here to make sure it is set right
  MaskFPUExceptions(CommandsDataModule.PyIDEOptions.MaskFPUExceptions);

  fDebuggerCommand := dcRun;
  if (PyControl.DebuggerState <> dsInactive) then exit; // pass control to user_line

  //Compile
  Code := InternalInterpreter.Compile(Editor);

  if VarIsPython(Code) then begin
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

      InternalInterpreter.Debugger.InitStepIn := InitStepIn;
      try
        InternalInterpreter.Debugger.run(Code);
      except
        // CheckError already called by VarPyth
        on E: EPythonError do begin
          InternalInterpreter.HandlePyException(E, 2);
          ReturnFocusToEditor := False;
          Dialogs.MessageDlg(E.Message, mtError, [mbOK], 0);
          CanDoPostMortem := True;
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

      // Restore the command line parameters
      RestoreCommandLine;

      // Change the back current path
      ChDir(OldPath);

      if PyIDEMainForm.Layouts.IndexOf('Debug') >= 0 then
        PyIDEMainForm.LoadLayout('Current');

      PyControl.DoStateChange(dsInactive);
      if ReturnFocusToEditor then
        Editor.Activate;
      if CanDoPostMortem and CommandsDataModule.PyIDEOptions.PostMortemOnException then
        EnterPostMortem;
    end;
  end;
end;

function TPyInternalDebugger.RunSource(const Source, FileName: string; symbol : string = 'single'): boolean;
// The internal interpreter RunSource calls II.runsource which differs
// according to whether we debugging or not
Var
  OldCurrentPos : TEditorPos;
begin
  OldCurrentPos := TEditorPos.Create;
  OldCurrentPos.Assign(PyControl.CurrentPos);
  try
    Result := InternalInterpreter.RunSource(Source, FileName, symbol);
    PyControl.CurrentPos.Assign(OldCurrentPos);
    PyControl.DoCurrentPosChanged;
  finally
    OldCurrentPos.Free;
  end;
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
  InternalInterpreter.Debugger.set_break(VarPythonCreate(FName), ALine, 1);

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
  if PyControl.DebuggerState = dsPostMortem then
    ExitPostMortem
  else
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

      if PyControl.DebuggerState = dsRunning then
        PyControl.DoStateChange(dsPaused);
     FDebuggerCommand := dcNone;
     While  FDebuggerCommand = dcNone do
       PyControl.DoYield(True);

     if PyControl.BreakPointsChanged then SetDebuggerBreakpoints;

     PyControl.DoStateChange(dsRunning);
   end;

   case fDebuggerCommand of
     dcRun         : InternalInterpreter.Debugger.set_continue();
     dcStepInto    : InternalInterpreter.Debugger.set_step();
     dcStepOver    : InternalInterpreter.Debugger.set_next(Frame);
     dcStepOut     : InternalInterpreter.Debugger.set_return(Frame);
     dcRunToCursor : InternalInterpreter.Debugger.set_continue;
     dcPause       : InternalInterpreter.Debugger.set_step();
     dcAbort       : begin
                       InternalInterpreter.Debugger.set_quit();
                       MessagesWindow.AddMessage('Debugging Aborted');
                       MessagesWindow.ShowWindow;
                     end;
   end;
   VarClear(fCurrentFrame);
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
  end else if fDebuggerCommand = dcPause then
    InternalInterpreter.Debugger.set_step();
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
        if not TBreakPoint(BreakPoints[j]).Disabled then begin
          if TBreakPoint(BreakPoints[j]).Condition <> '' then begin
            InternalInterpreter.Debugger.set_break(VarPythonCreate(FName),
              TBreakPoint(BreakPoints[j]).LineNo,
              0, TBreakPoint(BreakPoints[j]).Condition);
          end else
            InternalInterpreter.Debugger.set_break(VarPythonCreate(FName),
              TBreakPoint(BreakPoints[j]).LineNo);
        end;
      end;
    end;
  PyControl.BreakPointsChanged := False;
end;

procedure TPyInternalDebugger.LoadLineCache;
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
        fLineCache.cache.SetItem(VarPythonCreate(FName),
          VarPythonCreate([0, None, Source, FName], stTuple));
      end;
end;

procedure TPyInternalDebugger.MakeFrameActive(Frame: TBaseFrameInfo);
begin
  if Assigned(Frame) then
    InternalInterpreter.Debugger.currentframe := (Frame as TFrameInfo).fPyFrame
  else
    InternalInterpreter.Debugger.currentframe := None;
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

  // Execute PyscripterSetup.py here

  // sys.displayhook
  fII.setupdisplayhook()
end;

function TPyInternalInterpreter.EvalCode(const Expr: string): Variant;
begin
  // may raise exceptions
  Result := fII.evalcode(Expr);
end;

function TPyInternalInterpreter.CallTipFromExpression(const Expr: string;
  var DisplayString, DocString: string) : Boolean;
var
  LookupObj, PyDocString: Variant;
  SuppressOutput : IInterface;
begin
  Result := False;
  DisplayString := '';
  DocString := '';
  if Expr = '' then Exit;

  SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
  try
    //Evaluate the lookup expression and get the hint text
    LookupObj := fII.evalcode(Expr);
    DisplayString := fII.get_arg_text(LookupObj);
    Result := True;

    PyDocString := Import('inspect').getdoc(LookupObj);
    if not VarIsNone(PyDocString) then
      //DocString := GetNthLine(PyDocString, 1)
      DocString := PyDocString
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
          Dialogs.MessageDlg(E.Message, mtError, [mbOK], 0);
          SysUtils.Abort;
        end;
        on E: EPythonError do begin  //may raise OverflowError or ValueError
          HandlePyException(E);
          Dialogs.MessageDlg(E.Message, mtError, [mbOK], 0);
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

function TPyInternalInterpreter.ImportModule(Editor: IEditor;
  AddToNameSpace : Boolean = False) : Variant;
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
        on E: EPythonError do begin
          HandlePyException(E);
          Result := None;
        end;
      end;
    end;
    if VarIsNone(Result) then begin
      Dialogs.MessageDlg('Error in importing module', mtError, [mbOK], 0);
      SysUtils.Abort;
    end else if AddToNameSpace then
      // add Module name to the locals() of the interpreter
      GetPythonEngine.ExecString('import ' + NameOfModule);
  finally
    PyControl.DoStateChange(dsInactive);
  end;
end;

function TPyInternalInterpreter.NameSpaceFromExpression(
  const Expr: string): TBaseNameSpaceItem;
var
  InspectModule, LookupObj, ItemsDict: Variant;
  SuppressOutput : IInterface;
begin
  SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
  InspectModule := Import('inspect');
  try
    if Expr <> '' then begin
      //Evaluate the lookup expression and get the hint text
      LookupObj := fII.evalcode(Expr);
      ItemsDict := fII.safegetmembers(LookupObj);
      Result := TNameSpaceItem.Create(Expr, ItemsDict);
    end else begin
      ItemsDict := NewPythonDict;
      ItemsDict.update(BuiltinModule.__dict__);
      ItemsDict.update(fII.evalcode('vars()'));
      Result := TNameSpaceItem.Create(Expr, ItemsDict);
    end;
  except
    ItemsDict := NewPythonDict;
    Result := TNameSpaceItem.Create(Expr, ItemsDict);
  end;
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
  ReturnFocusToEditor : Boolean;
  CanDoPostMortem : Boolean;
begin
  CanDoPostMortem := False;

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
      fII.run_nodebug(Code);
    except
      // CheckError already called by VarPyth
      on E: EPythonError do begin
        HandlePyException(E);
        ReturnFocusToEditor := False;
        Dialogs.MessageDlg(E.Message, mtError, [mbOK], 0);
        CanDoPostMortem := True;
        SysUtils.Abort;
      end;
    end;
  finally
    if CommandsDataModule.PyIDEOptions.TimeOut > 0 then begin
      if (mmResult <> 0) then TimeKillEvent(mmResult);
      TimeEndPeriod(Resolution);
    end;
    PythonIIForm.AppendPrompt;

    // Restore the command line parameters
    RestoreCommandLine;

    // Change the back current path
    ChDir(OldPath);

    PyControl.DoStateChange(dsInactive);
    if ReturnFocusToEditor then
      Editor.Activate;
    if CanDoPostMortem and CommandsDataModule.PyIDEOptions.PostMortemOnException then
      PyControl.ActiveDebugger.EnterPostMortem;
  end;
end;

function TPyInternalInterpreter.RunSource(const Source, FileName : string; symbol : string = 'single'): boolean;
Var
  OldDebuggerState : TDebuggerState;
begin
  Assert(not PyControl.IsRunning, 'RunSource called while the Python engine is active');
  OldDebuggerState := PyControl.DebuggerState;
  PyControl.DoStateChange(dsRunningNoDebug);
  try
    // Workaround due to PREFER_UNICODE flag to make sure
    // no conversion to Unicode and back will take place
    Result := fII.runsource(VarPythonCreate(Source), FileName, symbol);
  finally
    PyControl.DoStateChange(OldDebuggerState);
  end;
end;

function TPyInternalInterpreter.SyntaxCheck(Editor: IEditor; Quiet : Boolean = False): Boolean;
Var
  FName, Source : string;
  tmp: PPyObject;
  PyErrType, PyErrValue, PyErrTraceback, PyErrValueTuple : PPyObject;
  SuppressOutput : IInterface;
begin
  PyControl.ErrorPos.Clear;
  PyControl.DoErrorPosChanged;

  MessagesWindow.ClearMessages;

  FName := Editor.FileName;
  if FName = '' then FName := '<'+Editor.FileTitle+'>';
  Source := CleanEOLs(Editor.EncodedText)+#10;

  with GetPythonEngine do begin
    if Quiet then
      SuppressOutput := PythonIIForm.OutputSuppressor; // Do not show errors
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
            if Not Quiet then Dialogs.MessageDlg(E.Message, mtError, [mbOK], 0);
          end;
        end;
      end;
    end;
  end;
end;

function TPyInternalInterpreter.SysPathAdd(const Path: WideString): boolean;
begin
  if SysModule.path.__contains__(Path) then
    Result := false
  else begin
    SysModule.path.insert(0, Path);
    Result := true;
  end;
end;

function TPyInternalInterpreter.SysPathRemove(const Path: WideString): boolean;
begin
  if SysModule.path.__contains__(Path) then begin
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









