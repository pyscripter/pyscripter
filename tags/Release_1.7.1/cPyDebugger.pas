{-----------------------------------------------------------------------------
 Unit Name: cPyDebugger
 Author:    Kiriakos Vlahos
 Date:      23-Feb-2005
 Purpose:
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
    function IsDict : Boolean; override;
    function IsModule : Boolean; override;
    function IsFunction : Boolean; override;
    function IsMethod : Boolean; override;
    function Has__dict__ : Boolean; override;
    function IndexOfChild(AName : string): integer; override;
    procedure GetChildNodes; override;
  end;

  TPyDebugger = class(TPyBaseDebugger)
  // pdb based internal debugger
  private
    fDebuggerCommand : TDebuggerCommand;
    fCurrentFrame : Variant;
  protected
    procedure SetDebuggerBreakpoints;
    procedure LoadLineCache;
    procedure UserCall(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
    procedure UserLine(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
    procedure UserReturn(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
    procedure UserException(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
    procedure UserYield(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
    property CurrentFrame : Variant read fCurrentFrame;
  public
    constructor Create;

    procedure Run(Editor : IEditor; InitStepIn : Boolean = False); override;
    procedure RunToCursor(Editor : IEditor; ALine: integer); override;
    procedure StepInto(Editor : IEditor); override;
    procedure StepOver; override;
    procedure StepOut; override;
    procedure Pause; override;
    procedure Abort; override;
    procedure Evaluate(const Expr : string; out ObjType, Value : string); override;
    procedure GetCallStack(CallStackList : TObjectList); override;
    function GetFrameGlobals(Frame : TBaseFrameInfo) : TBaseNameSpaceItem; override;
    function GetFrameLocals(Frame : TBaseFrameInfo) : TBaseNameSpaceItem; override;
  end;

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
begin
  if Assigned(fChildNodes) then
    fChildNodes.Free;
  inherited;
end;

function TNameSpaceItem.GetChildCount: integer;
begin
  if Assigned(fChildNodes) then
    Result := fChildNodes.Count
  else begin
    try
      if VarIsPythonDict(fPyObject) then
        Result := len(fPyObject)
      else if Has__dict__ then
        Result := len(fPyObject.__dict__)
      else
        Result := 0;
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
    SupressOutput := PythonIIForm.OutputSupressor; // Do not show errors
    try
      if VarIsPythonDict(fPyObject) then begin
        Dict := fPyObject;
        Count := len(fPyObject);
      end else if Has__dict__ then begin
        Dict := fPyObject.__dict__;
        Count := len(Dict);
      end else
        Count := 0;

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
        fChildNodes.Sorted := True;
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
  SupressOutput := PythonIIForm.OutputSupressor; // Do not show errors
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
  SupressOutput := PythonIIForm.OutputSupressor; // Do not show errors
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
  SupressOutput := PythonIIForm.OutputSupressor; // Do not show errors
  try
    Result := PythonIIForm.II.saferepr(fPyObject);
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

constructor TPyDebugger.Create;
begin
  inherited Create;
  fDebuggerCommand := dcNone;
end;

procedure TPyDebugger.Evaluate(const Expr : string; out ObjType, Value : string);
Var
  SupressOutput : IInterface;
  V : Variant;
begin
  ObjType := SNotAvailable;
  Value := SNotAvailable;
  if fDebuggerState = dsPaused then begin
    SupressOutput := PythonIIForm.OutputSupressor; // Do not show errors
    try
      V := PythonIIForm.BuiltIns.eval(Expr,
             CurrentFrame.f_globals, CurrentFrame.f_locals);
      ObjType := BuiltInModule.type(V).__name__; 
      Value := PythonIIForm.II.saferepr(V);
    except
      // fail quitely
    end;
  end;
end;

procedure TPyDebugger.GetCallStack(CallStackList: TObjectList);
Var
  Frame : Variant;
begin
  CallStackList.Clear;
  if fDebuggerState <> dsPaused then
    Exit;
  Frame := CurrentFrame;
  if VarIsPython(Frame) then
    // We do not show the last two frames
    while not VarIsNone(Frame.f_back) and not VarIsNone(Frame.f_back.f_back) do begin
      CallStackList.Add(TFrameInfo.Create(Frame));
      Frame := Frame.f_back;
    end;
end;

function TPyDebugger.GetFrameGlobals(Frame: TBaseFrameInfo): TBaseNameSpaceItem;
begin
  Result := nil;
  if fDebuggerState <> dsPaused then
    Exit;
  Result := TNameSpaceItem.Create('globals', (Frame as TFrameInfo).fPyFrame.f_globals);
end;

function TPyDebugger.GetFrameLocals(Frame: TBaseFrameInfo): TBaseNameSpaceItem;
begin
  Result := nil;
  if fDebuggerState <> dsPaused then
    Exit;
  Result := TNameSpaceItem.Create('locals', (Frame as TFrameInfo).fPyFrame.f_locals);
end;

procedure TPyDebugger.Pause;
// Not pausible to implement with the debugger running in the main thread
begin
  if fDebuggerState = dsRunning then
    fWantedState := dsPaused;
  DoStateChange;
end;

// Timer callback function
procedure TPyDebugger.Run(Editor : IEditor; InitStepIn : Boolean = False);
var
  OldPS1, OldPS2 : string;
  Code : Variant;
  TI : TTracebackItem;
  Path, OldPath : string;
  PythonPathAdder : IInterface;
begin
  // Repeat here to make sure it is set right
  MaskFPUExceptions(CommandsDataModule.PyIDEOptions.MaskFPUExceptions);

  fDebuggerCommand := dcRun;
  if (fDebuggerState <> dsInactive) then exit; // pass control to user_line

  //Compile
  Code := Compile(Editor);

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

    fWantedState := dsRunning;
    DoStateChange;

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

      Debugger.InitStepIn := InitStepIn;
      try
        Debugger.run(Code);
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
      PS1 := OldPS1;
      PS2 := OldPS2;
      AppendText(sLineBreak+PS1);

      // Restore the command line parameters
      RestoreCommandLine;

      // Change the back current path
      ChDir(OldPath);

      if PyIDEMainForm.Layouts.IndexOf('Debug') >= 0 then
        PyIDEMainForm.LoadLayout('Current');

      fWantedState := dsInactive;
      DoStateChange;
    end;
  end;
end;

procedure TPyDebugger.RunToCursor(Editor : IEditor; ALine: integer);
Var
  FName : string;
begin
  // Set Temporary breakpoint
  SetDebuggerBreakPoints;  // So that this one is not cleared
  FName := Editor.FileName;
  if FName = '' then
    FName := '<'+Editor.FileTitle+'>';
  PythonIIForm.Debugger.set_break(FName, ALine, 1);

  if fDebuggerState = dsInactive then
    Run(Editor)
  else
    fDebuggerCommand := dcRunToCursor;
end;

procedure TPyDebugger.StepInto(Editor : IEditor);
begin
  if fDebuggerState = dsInactive then
    Run(Editor, True)
  else
    fDebuggerCommand := dcStepInto;
end;

procedure TPyDebugger.StepOver;
begin
  fDebuggerCommand := dcStepOver;
end;

procedure TPyDebugger.StepOut;
begin
  fDebuggerCommand := dcStepOut;
end;

procedure TPyDebugger.Abort;
begin
  fDebuggerCommand := dcAbort;
end;

procedure TPyDebugger.UserCall(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
begin
   // PythonIIForm.AppendText('UserCall'+sLineBreak);
   Result := GetPythonEngine.ReturnNone;
end;

procedure TPyDebugger.UserException(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
   // PythonIIForm.AppendText('UserException'+sLineBreak);
   Result := GetPythonEngine.ReturnNone;
end;

procedure TPyDebugger.UserLine(Sender: TObject; PSelf, Args: PPyObject;
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
     FCurrentPos.Editor := GI_EditorFactory.GetEditorByNameOrTitle(FName);
     FCurrentPos.Line := Frame.f_lineno;
     FCurrentFrame := Frame;

     Pause;
     FDebuggerCommand := dcNone;
     While  FDebuggerCommand = dcNone do
       DoYield(True);

     VarClear(fCurrentFrame);
     if fBreakPointsChanged then SetDebuggerBreakpoints;

     fWantedState := dsRunning;
     DoStateChange;
   end;

   case fDebuggerCommand of
     dcRun         : PythonIIForm.Debugger.set_continue();
     dcStepInto    : PythonIIForm.Debugger.set_step();
     dcStepOver    : PythonIIForm.Debugger.set_next(Frame);
     dcStepOut     : PythonIIForm.Debugger.set_return(Frame);
     dcRunToCursor : PythonIIForm.Debugger.set_continue;
     dcAbort       : begin
                       PythonIIForm.Debugger.set_quit();
                       MessagesWindow.AddMessage('Debugging Aborted');
                       MessagesWindow.ShowWindow;
                     end;
   end;

end;

procedure TPyDebugger.UserReturn(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
begin
   // PythonIIForm.AppendText('UserReturn'+sLineBreak);
   Result := GetPythonEngine.ReturnNone;
end;

procedure TPyDebugger.UserYield(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
begin
  DoYield(False);
  if fDebuggerCommand = dcAbort then begin
    PythonIIForm.Debugger.set_quit();
    MessagesWindow.AddMessage('Debugging Aborted');
    MessagesWindow.ShowWindow;
  end;
  Result := GetPythonEngine.ReturnNone;
end;

procedure TPyDebugger.SetDebuggerBreakpoints;
var
  i, j : integer;
  FName : string;
  V : Variant;
begin
  if not fBreakPointsChanged then Exit;
  LoadLineCache;
  PythonIIForm.Debugger.clear_all_breaks();
  for i := 0 to GI_EditorFactory.Count - 1 do
    with GI_EditorFactory.Editor[i] do begin
      FName := FileName;
      if FName = '' then
        FName := '<'+FileTitle+'>';
      for j := 0 to BreakPoints.Count - 1 do begin
        V := PythonIIForm.Debugger.set_break;
        if TBreakPoint(BreakPoints[j]).Condition <> '' then begin
          GetPythonEngine.EvalFunction(ExtractPythonObjectFrom(V),
            [FName, TBreakPoint(BreakPoints[j]).LineNo,
            0, TBreakPoint(BreakPoints[j]).Condition]);
           //  To avoid the implicit conversion of the filename to unicode
           //  which causes problems due to the strange behaviour of os.path.normcase
          //PythonIIForm.Debugger.set_break(FName, TBreakPoint(BreakPoints[j]).LineNo,
          //  0, TBreakPoint(BreakPoints[j]).Condition);
        end else
          GetPythonEngine.EvalFunction(ExtractPythonObjectFrom(V),
            [FName, TBreakPoint(BreakPoints[j]).LineNo]);
          //PythonIIForm.Debugger.set_break(FName, TBreakPoint(BreakPoints[j]).LineNo);
      end;
    end;
  fBreakPointsChanged := False;
end;

procedure TPyDebugger.LoadLineCache;
Var
  i : integer;
  Source : string;
begin
  // inject unsaved code into LineCache
  PythonIIForm.LineCache.cache.clear();
  for i := 0 to GI_EditorFactory.Count - 1 do
    with GI_EditorFactory.Editor[i] do
      if HasPythonfile and (FileName = '') then
      begin
        Source := CommandsDataModule.CleanEOLs(EncodedText+#10);
        PythonIIForm.LineCache.cache.SetItem('<'+FileTitle+'>',
          VarPythonCreate([0,0,Source, '<'+FileTitle+'>'], stTuple));
      end;
end;

end.





