{-----------------------------------------------------------------------------
 Unit Name: cPyBaseDebugger
 Author:    Kiriakos Vlahos
 Date:      23-Apr-2006
 Purpose:
 History:   Base debugger classes
-----------------------------------------------------------------------------}
unit cPyBaseDebugger;

interface
uses
  Windows, SysUtils, Classes, uEditAppIntfs, PythonEngine, Forms, Contnrs;

type
  TDebuggerState = (dsInactive, dsRunning, dsPaused, dsRunningNoDebug);
  TDebuggerCommand = (dcNone, dcRun, dcStepInto, dcStepOver, dcStepOut, dcRunToCursor, dcAbort);

  TDebuggerLineInfo = (dlCurrentLine,
                       dlBreakpointLine,
                       dlDisabledBreakpointLine,
                       dlExecutableLine,
                       dlErrorLine);
  TDebuggerLineInfos = set of TDebuggerLineInfo;

  TNamespaceItemAttribute = (nsaNew, nsaChanged);
  TNamespaceItemAttributes = set of TNamespaceItemAttribute;

  TBreakpointChangeEvent = procedure(Sender: TObject; Editor : IEditor; ALine: integer) of object;
  TDebuggerStateChangeEvent = procedure(Sender: TObject;
    OldState, NewState: TDebuggerState) of object;
  TDebuggerYieldEvent = procedure(Sender: TObject; DoIdle : Boolean) of object;

  TEditorPos = class(TPersistent)
  public
    Editor : IEditor;
    Line : integer;
    Char : integer;
    IsSyntax : Boolean;
    ErrorMsg : string;
    procedure Clear;
    procedure Assign(Source: TPersistent); override;
  end;

  TBaseFrameInfo = class(TObject)
  // Base (abstract) class for Call Stack frame information
  protected
    function GetFunctionName : string; virtual; abstract;
    function GetFileName : string; virtual; abstract;
    function GetLine : integer; virtual; abstract;
  public
    property FunctionName : string read GetFunctionName;
    property FileName : string read GetFileName;
    property Line : integer read GetLine;
  end;

 TBaseNameSpaceItem = class(TObject)
  // Base (abstract) class for Namespace item information
  protected
    GotChildNodes : Boolean;
    GotBufferedValue : Boolean;
    BufferedValue : string;
    function GetOrCalculateValue : String;
    function GetName : string; virtual; abstract;
    function GetObjectType : string; virtual; abstract;
    function GetValue : string; virtual; abstract;
    function GetDocString : string; virtual; abstract;
    function GetChildCount : integer; virtual; abstract;
    function GetChildNode(Index: integer): TBaseNameSpaceItem; virtual; abstract;
  public
    Attributes : TNamespaceItemAttributes;
    function IsDict : Boolean; virtual; abstract;
    function IsModule : Boolean; virtual; abstract;
    function IsFunction : Boolean; virtual; abstract;
    function IsMethod : Boolean; virtual; abstract;
    function Has__dict__ : Boolean; virtual; abstract;
    function IndexOfChild(AName : string): integer; virtual; abstract;
    procedure GetChildNodes; virtual; abstract;
    procedure CompareToOldItem(OldItem : TBaseNameSpaceItem);
    property Name : string read GetName;
    property ObjectType : string read GetObjectType;
    property Value : string read GetOrCalculateValue;
    property DocString : string read GetDocString;
    property ChildCount : integer read GetChildCount;
    property ChildNode[Index : integer] : TBaseNameSpaceItem
      read GetChildNode;
  end;

  TPyBaseDebugger = class(TObject)
  //  Base class for implementing Python Debuggers
  private
    fOnBreakpointChange: TBreakpointChangeEvent;
    fOnCurrentPosChange: TNotifyEvent;
    fOnErrorPosChange: TNotifyEvent;
    fOnStateChange: TDebuggerStateChangeEvent;
    fOnYield: TDebuggerYieldEvent;
  protected
    fBreakPointsChanged : Boolean;
    fCurrentPos : TEditorPos;
    fErrorPos : TEditorPos;
    fDebuggerState: TDebuggerState;
    fWantedState: TDebuggerState;
    fOldargv : Variant;
    procedure DoOnBreakpointChanged(Editor : IEditor; ALine: integer);
    procedure DoCurrentPosChanged;
    procedure DoErrorPosChanged;
    procedure DoStateChange;
    procedure DoYield(DoIdle : Boolean);
    procedure SetCommandLine(const ScriptName : string); virtual;
    procedure RestoreCommandLine;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ToggleBreakpoint(Editor : IEditor; ALine: integer);
    procedure SetBreakPoint(FileName : string; ALine : integer;
      Disabled : Boolean; Condition : string);
    procedure ClearAllBreakpoints;

    function GetLineInfos(Editor : IEditor; ALine: integer): TDebuggerLineInfos;
    function IsBreakpointLine(Editor: IEditor; ALine: integer;
      var Disabled : boolean): boolean;
    function IsExecutableLine(Editor: IEditor; ALine: integer): boolean;

    function SyntaxCheck(Editor : IEditor; Quiet : Boolean = False) : Boolean;
    function IsRunning: boolean;
    function Compile(Editor : IEditor) : Variant;
    function ImportModule(Editor : IEditor) : Variant;
    procedure RunNoDebug(Editor : IEditor);

    procedure Run(Editor : IEditor; InitStepIn : Boolean = False); virtual; abstract;
    procedure RunToCursor(Editor : IEditor; ALine: integer); virtual; abstract;
    procedure StepInto(Editor : IEditor); virtual; abstract;
    procedure StepOver; virtual; abstract;
    procedure StepOut; virtual; abstract;
    procedure Pause; virtual; abstract;
    procedure Abort; virtual; abstract;
    // Evaluate expression in the current frame
    procedure Evaluate(const Expr : string; out ObjType, Value : string); virtual; abstract;
    // Fills in CallStackList with TBaseFrameInfo objects
    procedure GetCallStack(CallStackList : TObjectList); virtual; abstract;
    // functions to get TBaseNamespaceItems corresponding to a frame's gloabals and locals
    function GetFrameGlobals(Frame : TBaseFrameInfo) : TBaseNameSpaceItem; virtual; abstract;
    function GetFrameLocals(Frame : TBaseFrameInfo) : TBaseNameSpaceItem; virtual; abstract;

    property CurrentPos: TEditorPos read fCurrentPos;
    property ErrorPos: TEditorPos read fErrorPos;
    property DebuggerState : TDebuggerState read fDebuggerState;
    property BreakPointsChanged : Boolean read fBreakPointsChanged
      write fBreakPointsChanged;
    property OnBreakpointChange: TBreakpointChangeEvent read fOnBreakpointChange
      write fOnBreakpointChange;
    property OnCurrentPosChange: TNotifyEvent read fOnCurrentPosChange
      write fOnCurrentPosChange;
    property OnErrorPosChange: TNotifyEvent read fOnErrorPosChange
      write fOnErrorPosChange;
    property OnStateChange: TDebuggerStateChangeEvent read fOnStateChange
      write fOnStateChange;
    property OnYield: TDebuggerYieldEvent read fOnYield write fOnYield;
  end;

implementation

uses dmCommands, frmPythonII, VarPyth, frmMessages, frmPyIDEMain,
  MMSystem, Math, JvDockControlForm, JclFileUtils, Dialogs, uCommonFunctions,
  cParameters, JclSysUtils, StringResources, SynUnicode;

{ TEditorPos }

procedure TEditorPos.Assign(Source: TPersistent);
begin
  if Source is TEditorPos then begin
    Self.Editor := TEditorPos(Source).Editor;
    Self.Line := TEditorPos(Source).Line;
    Self.Char := TEditorPos(Source).Char;
    Self.IsSyntax := TEditorPos(Source).IsSyntax;
    Self.ErrorMsg := TEditorPos(Source).ErrorMsg;
  end else
    inherited;
end;

procedure TEditorPos.Clear;
begin
  Editor := nil;
  Line := -1;
  Char := -1;
  IsSyntax := False;
  ErrorMsg := '';
end;

{ TPyBaseDebugger }

procedure TPyBaseDebugger.DoOnBreakpointChanged(Editor : IEditor; ALine: integer);
begin
  fBreakPointsChanged := True;
  if Assigned(fOnBreakpointChange) then
    fOnBreakpointChange(Self, Editor, ALine);
end;

procedure TPyBaseDebugger.DoCurrentPosChanged;
begin
  if Assigned(fOnCurrentPosChange) then
    fOnCurrentPosChange(Self);
end;

procedure TPyBaseDebugger.DoErrorPosChanged;
begin
  if Assigned(fOnErrorPosChange) then
    fOnErrorPosChange(Self);
end;

procedure TPyBaseDebugger.DoYield(DoIdle : Boolean);
begin
  if Assigned(fOnYield) then
    fOnYield(Self, DoIdle);
end;

constructor TPyBaseDebugger.Create;
begin
  inherited Create;
  fDebuggerState := dsInactive;
  fBreakPointsChanged := False;
  fCurrentPos := TEditorPos.Create;
  fCurrentPos.Clear;
  fErrorPos := TEditorPos.Create;
  fErrorPos.Clear;
end;

procedure TPyBaseDebugger.DoStateChange;
Var
  OldDebuggerState: TDebuggerState;
begin
  if fDebuggerState <> fWantedState then begin
    OldDebuggerState := fDebuggerState;
    if fWantedState in [dsInactive, dsRunning, dsRunningNoDebug] then
      fCurrentPos.Clear
    else begin
      fErrorPos.Clear;
      DoErrorPosChanged;
    end;
    fDebuggerState := fWantedState;
    if Assigned(fOnStateChange) then
      fOnStateChange(Self, OldDebuggerState, fWantedState);
    DoCurrentPosChanged;
  end;
end;

destructor TPyBaseDebugger.Destroy;
begin
  fCurrentPos.Free;
  fErrorPos.Free;
  inherited;
end;

procedure TPyBaseDebugger.ToggleBreakpoint(Editor : IEditor; ALine: integer);
var
  SetBP: boolean;
  i: integer;
  BreakPoint : TBreakPoint;
begin
  if ALine > 0 then begin
    SetBP := TRUE;
    for i := 0 to Editor.Breakpoints.Count - 1 do begin
      if TBreakPoint(Editor.Breakpoints[i]).LineNo = ALine then begin
        Editor.Breakpoints.Delete(i);
        SetBP := FALSE;
        break;
      end else if TBreakPoint(Editor.Breakpoints[i]).LineNo > ALine then begin
        BreakPoint := TBreakPoint.Create;
        BreakPoint.LineNo := ALine;
        Editor.Breakpoints.Insert(i, BreakPoint);
        SetBP := FALSE;
        break;
      end;
    end;
    if SetBP then begin
      BreakPoint := TBreakPoint.Create;
      BreakPoint.LineNo := ALine;
      Editor.Breakpoints.Add(BreakPoint);
    end;
    DoOnBreakpointChanged(Editor, ALine);
  end;
end;

procedure TPyBaseDebugger.SetBreakPoint(FileName: string; ALine: integer;
  Disabled : Boolean; Condition: string);
var
  Editor : IEditor;
  i: integer;
  BreakPoint : TBreakPoint;
begin
  Editor := GI_EditorFactory.GetEditorByNameOrTitle(FileName);

  BreakPoint := nil;
  if Assigned(Editor) and (ALine > 0) then begin
    for i := 0 to Editor.Breakpoints.Count - 1 do begin
      if TBreakPoint(Editor.Breakpoints[i]).LineNo = ALine then begin
        BreakPoint := TBreakPoint(Editor.Breakpoints[i]);
        break;
      end else if TBreakPoint(Editor.Breakpoints[i]).LineNo > ALine then begin
        BreakPoint := TBreakPoint.Create;
        Editor.Breakpoints.Insert(i, BreakPoint);
        break;
      end;
    end;
    if not Assigned(BreakPoint) then begin
      BreakPoint := TBreakPoint.Create;
      Editor.Breakpoints.Add(BreakPoint);
    end;
    BreakPoint.LineNo := ALine;
    BreakPoint.Disabled := Disabled;
    BreakPoint.Condition := Condition;

    DoOnBreakpointChanged(Editor, ALine);
  end;
end;

procedure TPyBaseDebugger.ClearAllBreakpoints;
Var
  i : integer;
begin
  for i := 0 to GI_EditorFactory.Count -1 do
    if GI_EditorFactory.Editor[i].Breakpoints.Count > 0 then begin
      GI_EditorFactory.Editor[i].Breakpoints.Clear;
      DoOnBreakpointChanged(GI_EditorFactory.Editor[i], -1);
    end;
end;

function TPyBaseDebugger.GetLineInfos(Editor : IEditor; ALine: integer): TDebuggerLineInfos;
Var
  Disabled : boolean;
begin
  Result := [];
  if ALine > 0 then begin
    if (Editor = fCurrentPos.Editor) and (ALine = fCurrentPos.Line) then
      Include(Result, dlCurrentLine);
    if (Editor = fErrorPos.Editor) and (ALine = fErrorPos.Line) then
      Include(Result, dlErrorLine);
    if IsExecutableLine(Editor, ALine) then
      Include(Result, dlExecutableLine);
    Disabled := False;
    if IsBreakpointLine(Editor, ALine, Disabled) then
      if Disabled then
        Include(Result, dlDisabledBreakpointLine)
      else
        Include(Result, dlBreakpointLine);
  end;
end;

function TPyBaseDebugger.IsBreakpointLine(Editor: IEditor; ALine: integer;
  var Disabled : boolean): boolean;
Var
  i: integer;
begin
  Result := FALSE;
  if ALine > 0 then begin
    i := Editor.Breakpoints.Count - 1;
    while i >= 0 do begin
      if TBreakPoint(Editor.Breakpoints[i]).LineNo = ALine then begin
        Disabled := TBreakPoint(Editor.Breakpoints[i]).Disabled;
        Result := TRUE;
        break;
      end;
      Dec(i);
    end;
  end;
end;

function TPyBaseDebugger.IsExecutableLine(Editor: IEditor; ALine: integer): boolean;
begin
  Assert(Assigned(Editor));
  with Editor.SynEdit do begin
    Result := CommandsDataModule.IsExecutableLine(Lines[ALine-1]);
  end;
end;

function TPyBaseDebugger.SyntaxCheck(Editor: IEditor; Quiet : Boolean = False): Boolean;
Var
  FName, Source : string;
  tmp: PPyObject;
  PyErrType, PyErrValue, PyErrTraceback, PyErrValueTuple : PPyObject;
  SupressOutput : IInterface;
begin
  ErrorPos.Clear;
  DoErrorPosChanged;

  MessagesWindow.ClearMessages;

  FName := Editor.FileName;
  if FName = '' then FName := '<'+Editor.FileTitle+'>';
  Source := CommandsDataModule.CleanEOLs(Editor.EncodedText)+#10;

  with GetPythonEngine do begin
    if Quiet then
      SupressOutput := PythonIIForm.OutputSupressor; // Do not show errors
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
                fErrorPos.ErrorMsg := PyString_AsString(PyTuple_GetItem( PyErrValue, 0));
                PyErrValueTuple := PyTuple_GetItem( PyErrValue, 1);
                if PyTuple_Check( PyErrValueTuple )  and (PyTuple_Size( PyErrValueTuple) >= 4) then
                begin
                  fErrorPos.Line := PyInt_AsLong(PyTuple_GetItem( PyErrValueTuple, 1));
                  fErrorPos.Char := PyInt_AsLong(PyTuple_GetItem( PyErrValueTuple, 2));
                end;
              end else
                // Is it an instance of the SyntaxError class ?
              if PyInstance_Check( PyErrValue ) or (PyType_IsSubtype(PyErrValue^.ob_type,
                PPyTypeObject(GetPythonEngine.PyExc_SyntaxError^)) = 1) then
              begin
                // Get the text containing the error, cut of carriage return
                tmp := PyObject_GetAttrString(PyErrValue, 'text');
                if Assigned(tmp) and PyString_Check(tmp) then
                  fErrorPos.ErrorMsg := Trim(PyString_AsString(tmp));
                Py_XDECREF(tmp);
                // Get the offset where the error should appear
                tmp := PyObject_GetAttrString(PyErrValue, 'offset' );
                if Assigned(tmp) and PyInt_Check(tmp) then
                  fErrorPos.Char := PyInt_AsLong(tmp);
                Py_XDECREF(tmp);
                // Get the line number of the error
                tmp := PyObject_GetAttrString(PyErrValue, 'lineno' );
                if Assigned(tmp) and PyInt_Check(tmp) then
                  fErrorPos.Line := PyInt_AsLong(tmp);
                Py_XDECREF(tmp);
              end;
              FErrorPos.Editor := Editor;
              fErrorPos.IsSyntax := True;
              DoErrorPosChanged;
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
        PyErr_Print;
        // Throw exception for the error
        try
          RaiseError;
        except
          on E: EPySyntaxError do begin
            E.EFileName := FName;  // add the filename
            MessagesWindow.ShowPythonSyntaxError(E);
            if PyIDEMainForm.ShowFilePosition(E.EFileName, E.ELineNumber, E.EOffset) and
              Assigned(GI_ActiveEditor)
            then begin
              FErrorPos.Editor := GI_ActiveEditor;
            end;
            fErrorPos.Line := E.ELineNumber;
            fErrorPos.Char := E.EOffset;
            fErrorPos.IsSyntax := True;
            DoErrorPosChanged;
            if Not Quiet then MessageDlg(E.Message, mtError, [mbOK], 0);
          end;
        end;
      end;
    end;
  end;
end;

function TPyBaseDebugger.IsRunning: boolean;
begin
  Result := fDebuggerState in [dsRunning, dsRunningNoDebug];
end;

function TPyBaseDebugger.Compile(Editor: IEditor): Variant;
Var
  co : PPyObject;
  FName, Source : string;
begin
  VarClear(Result);
  ErrorPos.Clear;
  DoErrorPosChanged;

  MessagesWindow.ClearMessages;

  FName := Editor.FileName;
  if FName = '' then FName := '<'+Editor.FileTitle+'>';
  Source := CommandsDataModule.CleanEOLs(Editor.EncodedText)+#10;

  with GetPythonEngine do begin
    co := Py_CompileString(PChar(Source), PChar(FName), file_input );
    if not Assigned( co ) then begin
      // New Line for output
      PythonIIForm.AppendText(sLineBreak);
      // Display error
      PyErr_Print;
      // Throw exception for the error
      try
        RaiseError;
      except
        on E: EPySyntaxError do begin
          MessagesWindow.ShowPythonSyntaxError(E);
          if PyIDEMainForm.ShowFilePosition(E.EFileName, E.ELineNumber, E.EOffset) and
            Assigned(GI_ActiveEditor)
          then begin
            FErrorPos.Editor := GI_ActiveEditor;
            fErrorPos.Line := E.ELineNumber;
            fErrorPos.Char := E.EOffset;
            fErrorPos.IsSyntax := True;
            DoErrorPosChanged;
          end;
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

function TPyBaseDebugger.ImportModule(Editor: IEditor) : Variant;
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
  TI : TTracebackItem;
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

  fWantedState := dsRunningNoDebug;
  DoStateChange;

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
        on E: Exception do begin
          MessagesWindow.ShowPythonTraceback;
          MessagesWindow.AddMessage(E.Message);
          with GetPythonEngine.Traceback do begin
            if ItemCount > 0 then begin
              TI := Items[ItemCount -1];
              if PyIDEMainForm.ShowFilePosition(TI.FileName, TI.LineNo, 1) and
                Assigned(GI_ActiveEditor)
              then begin
                FErrorPos.Editor := GI_ActiveEditor;
                fErrorPos.Line := TI.LineNo;
                DoErrorPosChanged;
              end;
            end;
          end;
          Result := None;
        end;
      end;
    end;
    if VarIsNone(Result) then begin
      MessageDlg('Error in importing module', mtError, [mbOK], 0);
      SysUtils.Abort;
    end;
  finally
    fWantedState := dsInactive;
    DoStateChange;
  end;
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

procedure TPyBaseDebugger.RunNoDebug(Editor: IEditor);
Var
  Code : Variant;
  TI : TTracebackItem;
  mmResult,Resolution : LongWord;
  tc : TTimeCaps;
  Path, OldPath : string;
  PythonPathAdder : IInterface;
begin
  // Repeat here to make sure it is set right
  MaskFPUExceptions(CommandsDataModule.PyIDEOptions.MaskFPUExceptions);

  //Compile
  Code := Compile(Editor);

  fWantedState := dsRunningNoDebug;
  DoStateChange;

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
      PythonIIForm.Debugger.run_nodebug(Code);
    except
      // CheckError already called by VarPyth
      on E: Exception do begin
        MessagesWindow.ShowPythonTraceback;
        MessagesWindow.AddMessage(E.Message);
        with GetPythonEngine.Traceback do begin
          if ItemCount > 0 then begin
            TI := Items[ItemCount -1];
            if PyIDEMainForm.ShowFilePosition(TI.FileName, TI.LineNo, 1) and
              Assigned(GI_ActiveEditor)
            then begin
              FErrorPos.Editor := GI_ActiveEditor;
              fErrorPos.Line := TI.LineNo;
              DoErrorPosChanged;
            end;
          end;
        end;
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

    fWantedState := dsInactive;
    DoStateChange;
  end;
end;

procedure TPyBaseDebugger.RestoreCommandLine;
begin
  SysModule.argv := fOldargv;
end;

procedure TPyBaseDebugger.SetCommandLine(const ScriptName : string);
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

{ TBaseNameSpaceItem }

procedure TBaseNameSpaceItem.CompareToOldItem(OldItem: TBaseNameSpaceItem);
var
  i, Index : integer;
  Child : TBaseNameSpaceItem;
begin
  if OldItem.GotBufferedValue then begin
    if OldItem.BufferedValue <> Value then
      Attributes := [nsaChanged];
  end;
  if OldItem.GotChildNodes then begin
    GetChildNodes;
    for i := 0 to ChildCount - 1 do begin
      Child := ChildNode[i];
      Index := OldItem.IndexOfChild(Child.Name);
      if Index >= 0 then
        Child.CompareToOldItem(OldItem.ChildNode[Index])
      else
        Child.Attributes := [nsaNew];
    end;
  end;
end;

function TBaseNameSpaceItem.GetOrCalculateValue: String;
begin
  if GotBufferedValue then
    Result := BufferedValue
  else begin
    BufferedValue := GetValue;
    GotBufferedValue := True;
    Result := BufferedValue;
  end;
end;

end.
