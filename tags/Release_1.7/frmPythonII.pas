{-----------------------------------------------------------------------------
 Unit Name: frmPythonII
 Author:    Kiriakos Vlahos
 Date:      20-Jan-2005
 Purpose:   Python Interactive Interperter using Python for Delphi and Synedit
 Features:  Syntax Highlighting
            Brace Highlighting
            Command History
                    - Alt-UP : previous command
                    - Alt-Down : next command
                    - Esc : clear command
            Code Completion
            Call Tips

 History:
-----------------------------------------------------------------------------}
unit frmPythonII;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs , Menus, PythonEngine, SyncObjs, SynHighlighterPython,
  SynEditHighlighter, SynEdit,
  SynEditKeyCmds, SynCompletionProposal, JvComponent, JvDockControlForm,
  frmIDEDockWin, ExtCtrls, TBX, TBXThemes, PythonGUIInputOutput, JvComponentBase,
  SynUnicode, TB2Item, ActnList;

const
  WM_APPENDTEXT = WM_USER + 1020;

type
  TPythonIIForm = class(TIDEDockWindow)
    SynEdit: TSynEdit;
    PythonEngine: TPythonEngine;
    PythonIO: TPythonInputOutput;
    SynCodeCompletion: TSynCompletionProposal;
    DebugIDE: TPythonModule;
    SynParamCompletion: TSynCompletionProposal;
    InterpreterPopUp: TTBXPopupMenu;
    InterpreterActionList: TActionList;
    actCleanUpNameSpace: TAction;
    actCleanUpSysModules: TAction;
    TBXItem1: TTBXItem;
    TBXItem2: TTBXItem;
    TBXSeparatorItem1: TTBXSeparatorItem;
    TBXItem3: TTBXItem;
    actCopyHistory: TAction;
    TBXSeparatorItem2: TTBXSeparatorItem;
    TBXItem4: TTBXItem;
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
    procedure SynEditReplaceText(Sender: TObject; const ASearch,
      AReplace: WideString; Line, Column: Integer; var Action: TSynReplaceAction);
    procedure SynEditPaintTransient(Sender: TObject; Canvas: TCanvas;
      TransientType: TTransientType);
    procedure FormCreate(Sender: TObject);
    procedure SynEditProcessCommand(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: WideChar; Data: Pointer);
    procedure SynEditProcessUserCommand(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: WideChar; Data: Pointer);
    procedure SynCodeCompletionExecute(Kind: SynCompletionType;
      Sender: TObject; var CurrentInput: WideString; var x, y: Integer;
      var CanExecute: Boolean);
    function FormHelp(Command: Word; Data: Integer;
      var CallHelp: Boolean): Boolean;
    procedure InputBoxExecute(Sender: TObject; PSelf,
      Args: PPyObject; var Result: PPyObject);
    procedure FormActivate(Sender: TObject);
    procedure StatusWriteExecute(Sender: TObject; PSelf,
      Args: PPyObject; var Result: PPyObject);
    procedure MessageWriteExecute(Sender: TObject; PSelf,
      Args: PPyObject; var Result: PPyObject);
    procedure FormDestroy(Sender: TObject);
    procedure SynParamCompletionExecute(Kind: SynCompletionType;
      Sender: TObject; var CurrentInput: WideString; var x, y: Integer;
      var CanExecute: Boolean);
    procedure SynEditCommandProcessed(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: WideChar; Data: Pointer);
    procedure actCleanUpNameSpaceExecute(Sender: TObject);
    procedure InterpreterPopUpPopup(Sender: TObject);
    procedure actCleanUpSysModulesExecute(Sender: TObject);
    procedure actCopyHistoryExecute(Sender: TObject);
    procedure SynEditDblClick(Sender: TObject);
  private
    { Private declarations }
    fCommandHistory : TWideStringList;
    fCommandHistorySize : integer;
    fCommandHistoryPointer : integer;
    fShowOutput : Boolean;
    FCriticalSection : TCriticalSection;
    fOutputStream : TMemoryStream;
    fCloseBracketChar: WideChar;
    procedure GetBlockBoundary(LineN: integer; var StartLineN,
              EndLineN: integer; var IsCode: Boolean);
    function GetPromptPrefix(line: string): string;
    procedure SetCommandHistorySize(const Value: integer);
  protected
    procedure PythonIOSendData(Sender: TObject; const Data: WideString);
    procedure PythonIOReceiveData(Sender: TObject; var Data: WideString);
    procedure TBMThemeChange(var Message: TMessage); message TBM_THEMECHANGE;
    procedure WMAPPENDTEXT(var Message: TMessage); message WM_APPENDTEXT;
  public
    { Public declarations }
    II : Variant;   // wrapping sys and code modules
    Debugger, LineCache, BuiltIns : Variant;
    PS1, PS2 : WideString;
    PythonHelpFile : string;
    function OutputSupressor : IInterface;
    procedure AppendText(S: WideString);
    procedure AppendToPrompt(Buffer : array of WideString);
    function IsEmpty : Boolean;
    function CanFind: boolean;
    function CanFindNext: boolean;
    function CanReplace: boolean;
    procedure ExecFind;
    procedure ExecFindNext;
    procedure ExecFindPrev;
    procedure ExecReplace;
    procedure RegisterHistoryCommands;
    property ShowOutput : boolean read fShowOutput write fShowOutput;
    property CommandHistorySize : integer read fCommandHistorySize write SetCommandHistorySize;
  end;

var
  PythonIIForm: TPythonIIForm;

implementation

Uses
  SynEditTypes, Math, frmPyIDEMain, dmCommands, VarPyth, Registry,
  frmMessages, uCommonFunctions, JclStrings, frmVariables, StringResources,
  dlgConfirmReplace, frmUnitTests, JvDockGlobals, SynRegExpr;

{$R *.dfm}

{ Class TSuppressOuptput modelled after JVCL.WaitCursor}
type
TSuppressOutput = class(TInterfacedObject, IInterface)
private
  fPythonIIForm : TPythonIIForm;
  OldShowOutput : Boolean;
public
  constructor Create(PythonIIForm : TPythonIIForm);
  destructor Destroy; override;
end;

constructor TSuppressOutput.Create(PythonIIForm : TPythonIIForm);
begin
  inherited Create;
  fPythonIIForm := PythonIIForm;
  if Assigned(fPythonIIForm) then begin
    OldShowOutput := PythonIIForm.ShowOutput;
    PythonIIForm.ShowOutput := False;
  end;
end;

destructor TSuppressOutput.Destroy;
begin
  if Assigned(fPythonIIForm) then
    fPythonIIForm.ShowOutput := OldShowOutput;
  inherited Destroy;
end;

{ PythonIIForm }

function TPythonIIForm.OutputSupressor: IInterface;
begin
  Result := TSuppressOutput.Create(Self);
end;

procedure TPythonIIForm.SynEditPaintTransient(Sender: TObject; Canvas: TCanvas;
  TransientType: TTransientType);
begin
  if (not Assigned(SynEdit.Highlighter)) then
    Exit;
  CommandsDataModule.PaintMatchingBrackets(Canvas, SynEdit, TransientType);
end;

procedure TPythonIIForm.PythonIOReceiveData(Sender: TObject;
  var Data: WideString);
Var
  S : string;
begin
  S := Data;
  if not InputQuery('PyScripter - Input requested', 'Input:', S) then
    with GetPythonEngine do
      PyErr_SetString(PyExc_KeyboardInterrupt^, 'Operation cancelled')
  else
    Data := S + #10;
end;

procedure TPythonIIForm.PythonIOSendData(Sender: TObject; const Data: WideString);
Var
  WS : WideString;
begin
  if fShowOutput then begin
    fCriticalSection.Acquire;
    try
      fOutputStream.Write(Data[1], Length (Data) * 2);
      //fOutputStream.Write(WideLineBreak[1], Length (WideLineBreak) * 2);  RawOutput
      if GetCurrentThreadId = MainThreadId then begin
        SetLength(WS, fOutputStream.Size div 2);
        fOutputStream.Position := 0;
        fOutputStream.Read(WS[1], Length(WS)*2);
        AppendText(WS);
        fOutputStream.Size := 0;
      end else begin
        PostMessage(Handle, WM_APPENDTEXT, 0, 0);
      end;
    finally
      fCriticalSection.Release;
    end;
  end;
end;

procedure TPythonIIForm.actCleanUpNameSpaceExecute(Sender: TObject);
begin
  CommandsDataModule.PyIDEOptions.CleanupMainDict := (Sender as TAction).Checked;
end;

procedure TPythonIIForm.actCleanUpSysModulesExecute(Sender: TObject);
begin
  CommandsDataModule.PyIDEOptions.CleanupSysModules := (Sender as TAction).Checked;
end;

procedure TPythonIIForm.actCopyHistoryExecute(Sender: TObject);
begin
  SetClipboardText(fCommandHistory.Text);
end;

procedure TPythonIIForm.AppendText(S: WideString);
begin
  SynEdit.ExecuteCommand(ecEditorBottom, ' ', nil);
  SynEdit.SelText := S;
  SynEdit.ExecuteCommand(ecEditorBottom, ' ', nil);
  SynEdit.EnsureCursorPosVisible;
end;

procedure TPythonIIForm.AppendToPrompt(Buffer: array of WideString);
Var
  LineCount, i : integer;
  Line : WideString;
begin
  LineCount := SynEdit.Lines.Count;
  Line := SynEdit.Lines[LineCount-1];
  SynEdit.BeginUpdate;
  try
    if Line <> PS1 then begin
      if Line <> '' then AppendText(WideLineBreak);
      AppendText(PS1);
    end;
    for i := Low(Buffer) to High(Buffer) - 1  do
        AppendText(Buffer[i] + WideLineBreak + PS2);
    if Length(Buffer) > 0 then AppendText(Buffer[High(Buffer)]);
  finally
    SynEdit.EndUpdate;   
  end;
end;

procedure TPythonIIForm.FormCreate(Sender: TObject);

  function IsPythonVersionParam(const AParam : String; out AVersion : String) : Boolean;
  begin
    Result := (Length(AParam) = 9) and
              SameText(Copy(AParam, 1, 7), '-PYTHON') and
              (AParam[8] in ['0'..'9']) and
              (AParam[9] in ['0'..'9']);
    if Result then
      AVersion := AParam[8] + '.' + AParam[9];
  end;

  function IndexOfKnownVersion(const AVersion : String) : Integer;
  var
    i : Integer;
  begin
    Result := -1;
    for i := High(PYTHON_KNOWN_VERSIONS) downto Low(PYTHON_KNOWN_VERSIONS) do
      if PYTHON_KNOWN_VERSIONS[i].RegVersion = AVersion then
      begin
        Result := i;
        Break;
      end;
  end;

Var
  S : string;
  Registry : TRegistry;
  i : integer;
  idx : Integer;
  versionIdx : Integer;
  expectedVersion : string;
  RegKey : string;
  expectedVersionIdx : Integer;
begin
  inherited;
  SynEdit.ControlStyle := SynEdit.ControlStyle + [csOpaque];

  SynEdit.Highlighter := TSynPythonInterpreterSyn.Create(Self);
  SynEdit.Highlighter.Assign(CommandsDataModule.SynPythonSyn);

  SynEdit.Assign(CommandsDataModule.InterpreterEditorOptions);
  RegisterHistoryCommands;

  // IO
  PythonIO.OnSendUniData := PythonIOSendData;
  PythonIO.OnReceiveUniData := PythonIOReceiveData;
  PythonIO.UnicodeIO := True;
  PythonIO.RawOutput := True;

  // Load Python DLL

  // first find an optional parameter specifying the expected Python version in the form of -PYTHONXY
  expectedVersion := '';
  expectedVersionIdx := -1;
  for i := 1 to ParamCount do begin
    if IsPythonVersionParam(ParamStr(i), expectedVersion) then
    begin
      idx := IndexOfKnownVersion(expectedVersion);
      if idx >= COMPILED_FOR_PYTHON_VERSION_INDEX then
        expectedVersionIdx := idx;
      if expectedVersionIdx = -1 then
        if idx = -1 then
          MessageDlg(Format('PyScripter can''t use command line parameter %s because it doesn''t know this version of Python.',
            [ParamStr(i)]), mtWarning, [mbOK], 0)
        else
          MessageDlg(Format('PyScripter can''t use command line parameter %s because it was compiled for Python %s or later.',
            [ParamStr(i), PYTHON_KNOWN_VERSIONS[COMPILED_FOR_PYTHON_VERSION_INDEX].RegVersion]), mtWarning, [mbOK], 0);
      Break;
    end;
  end;
  // disable feature that will try to use the last version of Python because we provide our
  // own behaviour. Note that this feature would not load the latest version if the python dll
  // matching the compiled version of P4D was found.
  PythonEngine.UseLastKnownVersion := False;
  if expectedVersionIdx > -1 then
  begin
    // if we found a parameter requiring a specific version of Python,
    // then we must immediatly fail if P4D did not find the expected dll.
    versionIdx := expectedVersionIdx;
    PythonEngine.FatalMsgDlg := True;
    PythonEngine.FatalAbort  := True;
  end
  else
  begin
    // otherwise, let's start searching a valid python dll from the latest known version
    versionIdx := High(PYTHON_KNOWN_VERSIONS);
    PythonEngine.FatalMsgDlg := False;
    PythonEngine.FatalAbort  := False;
  end;
  // try to find an acceptable version of Python, starting from either the specified version,
  // or the latest know version, but stop when we reach the version targeted on compilation.
  for i := versionIdx downto COMPILED_FOR_PYTHON_VERSION_INDEX do begin
    PythonEngine.DllName    := PYTHON_KNOWN_VERSIONS[i].DllName;
    PythonEngine.APIVersion := PYTHON_KNOWN_VERSIONS[i].APIVersion;
    PythonEngine.RegVersion := PYTHON_KNOWN_VERSIONS[i].RegVersion;
    if i = COMPILED_FOR_PYTHON_VERSION_INDEX then begin // last chance, so raise an error if it goes wrong
      PythonEngine.FatalMsgDlg := True;
      PythonEngine.FatalAbort  := True;
    end;
    PythonEngine.LoadDll;
    if PythonEngine.IsHandleValid then // we found a valid version
      Break;
  end;

  fShowOutput := True;
  // For handling output from Python threads
  FCriticalSection := TCriticalSection.Create;
  fOutputStream := TMemoryStream.Create;

  //  For recalling old commands in Interactive Window;
  fCommandHistory := TWideStringList.Create();
  fCommandHistorySize := 20;
  fCommandHistoryPointer := 0;

  // Get Python vars and print banner
  S := Format('*** Python %s on %s. ***'+sLineBreak,
            [SysModule.version, SysModule.platform]);
  AppendText(S);

  PS1 := SysModule.ps1;
  PS2 := SysModule.ps2;

  II := VarPythonEval('_II');
  PythonEngine.ExecString('del _II');
  Debugger := II.debugger;
  LineCache := Import('linecache');
  BuiltIns := VarPythonEval('__builtins__');

  AppendText(PS1);

  // Python Help File
  Registry := TRegistry.Create(KEY_READ);
  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    // False because we do not want to create it if it doesn't exist
    RegKey := '\SOFTWARE\Python\PythonCore\'+SysModule.winver+
      '\Help\Main Python Documentation';
    if Registry.OpenKey(RegKey, False) then
      PythonHelpFile := Registry.ReadString('')
    else begin
      // try Current User
      Registry.RootKey := HKEY_CURRENT_USER;
      if Registry.OpenKey(RegKey, False) then
        PythonHelpFile := Registry.ReadString('')
    end;
  finally
    Registry.Free;
  end;
end;

procedure TPythonIIForm.FormDestroy(Sender: TObject);
begin
  fCommandHistory.Free;
  FCriticalSection.Free;
  fOutputStream.Free;
  inherited;
end;

procedure TPythonIIForm.GetBlockBoundary(LineN: integer; var StartLineN,
  EndLineN: integer; var IsCode: Boolean);
{-----------------------------------------------------------------------------
	  GetBlockBoundary takes a line number, and will return the
	  start and and line numbers of the block, and a flag indicating if the
	  block is a Python code block.
	  If the line specified has a Python prompt, then the lines are parsed
    and forwards, and the IsCode is true.
	  If the line does not start with a prompt, the block is searched forward
	  and backward until a prompt _is_ found, and all lines in between without
	  prompts are returned, and the IsCode is false.
-----------------------------------------------------------------------------}
Var
  Line, Prefix : string;
  MaxLineNo : integer;
begin
  Line := SynEdit.Lines[LineN];
  MaxLineNo := SynEdit.Lines.Count - 1;
  Prefix := GetPromptPrefix(line);
  if Prefix = '' then begin
    IsCode := False;
    StartLineN := LineN;
    while StartLineN > 0 do begin
      if GetPromptPrefix(SynEdit.Lines[StartLineN-1]) <> '' then break;
      Dec(StartLineN);
    end;
    EndLineN := LineN;
    while EndLineN < MaxLineNo do begin
      if GetPromptPrefix(SynEdit.Lines[EndLineN+1]) <> '' then break;
      Inc(EndLineN);
    end;
  end else begin
    IsCode := True;
    StartLineN := LineN;
    while (StartLineN > 0) and (Prefix <> PS1) do begin
      Prefix := GetPromptPrefix(SynEdit.Lines[StartLineN-1]);
      if Prefix = '' then break;
      Dec(StartLineN);
    end;
    EndLineN := LineN;
    while EndLineN < MaxLineNo do begin
      Prefix := GetPromptPrefix(SynEdit.Lines[EndLineN+1]);
      if (Prefix = PS1) or (Prefix = '') then break;
      Inc(EndLineN);
    end;
  end;
end;

function TPythonIIForm.GetPromptPrefix(line: string): string;
begin
  if Copy(line, 1, Length(PS1)) = PS1 then
    Result := PS1
  else if Copy(line, 1, Length(PS2)) = PS2 then
    Result := PS2
  else
    Result := '';
end;

procedure TPythonIIForm.SynEditProcessCommand(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: WideChar; Data: Pointer);
Var
  LineN, StartLineN, EndLineN, i, Len, Position : integer;
  NeedIndent : boolean;
  IsCode : Boolean;
  Line, CurLine, Source, Indent : WideString;
  EncodedSource : string;
  Buffer : array of WideString;
  P : PPyObject;
  V : Variant;
begin
  case Command of
    ecLineBreak :
      begin
        Command := ecNone;  // do not processed it further
        if SynParamCompletion.Form.Visible then
          SynParamCompletion.CancelCompletion;

        LineN := SynEdit.CaretY - 1;  // Caret is 1 based
        GetBlockBoundary(LineN, StartLineN, EndLineN, IsCode);
        // If we are in a code-block, but it isnt at the end of the buffer
        // then copy it to the end ready for editing and subsequent execution
        if not IsCode then begin
           SetLength(Buffer, 0);
           AppendToPrompt(Buffer);
        end else begin
          SetLength(Buffer, EndLineN-StartLineN + 1);
          Source := '';
          for i := StartLineN to EndLineN do begin
            Line := SynEdit.Lines[i];
            Len := Length(GetPromptPrefix(Line));
            Buffer[i-StartLineN] := Copy(Line, Len+1, MaxInt);
            Source := Source + Buffer[i-StartLineN] + WideLF;
          end;
          Delete(Source, Length(Source), 1);
          // If we are in a code-block, but it isnt at the end of the buffer
          // then copy it to the end ready for editing and subsequent execution
          if EndLineN <> SynEdit.Lines.Count - 1 then
            AppendToPrompt(Buffer)
          else if Trim(Source) = '' then begin
            AppendText(WideLineBreak);
            AppendText(PS1);
          end else begin
            SynEdit.ExecuteCommand(ecEditorBottom, ' ', nil);
            AppendText(WideLineBreak);

            //remove trailing tabs
            for i := Length(Source) downto 1 do
              if Source[i] = #9 then Delete(Source, i, 1)
            else
              break;

            if CommandsDataModule.PyIDEOptions.UTF8inInterpreter then
              EncodedSource := UTF8BOMString + Utf8Encode(Source)
            else
              EncodedSource := Source;

            // Workaround due to PREFER_UNICODE flag to make sure
            // no conversion to Unicode and back will take place
            with GetPythonEngine do begin
              P := PyString_FromString(PChar(EncodedSource));
              V := VarPythonCreate(P);
              Py_XDECREF(P);
            end;
            if II.runsource(V, '<interactive input>') then
              NeedIndent := True
            else begin
              // The source code has been executed
              // If the last line isnt empty, append a newline
              SetLength(Buffer, 0);
              AppendToPrompt(Buffer);
              NeedIndent := False;

              //  Add the command executed to History
              FCommandHistory.Add(Source);
              SetCommandHistorySize(fCommandHistorySize);
              fCommandHistoryPointer := fCommandHistory.Count;

              VariablesWindow.UpdateWindow;
            end;
            if NeedIndent then begin
              // Now attempt to correct indentation
              CurLine := Copy(SynEdit.Lines[lineN], Length(PS2)+1, MaxInt); //!!
              Position := 1;
              Indent := '';
              while (Length(CurLine)>=Position) and
                   (CurLine[Position] in [WideChar(#09), WideChar(#32)]) do begin
                Indent := Indent + CurLine[Position];
                Inc(Position);
              end;

              if CommandsDataModule.IsBlockOpener(CurLine) then
                Indent := Indent + #9
              else if CommandsDataModule.IsBlockCloser(CurLine) then
                Delete(Indent, Length(Indent), 1);
              // use ReplaceSel to ensure it goes at the cursor rather than end of buffer.
              SynEdit.SelText := PS2 + Indent;
            end;
          end;
        end;
        SynEdit.EnsureCursorPosVisible;
      end;
    ecDeleteLastChar :
      begin
        Line := SynEdit.Lines[SynEdit.CaretY - 1];
        if ((Pos(PS1, Line) = 1) and (SynEdit.CaretX <= Length(PS1)+1)) or
           ((Pos(PS2, Line) = 1) and (SynEdit.CaretX <= Length(PS2)+1)) then
          Command := ecNone;  // do not processed it further
      end;
    ecLineStart :
      begin
        Line := SynEdit.Lines[SynEdit.CaretY - 1];
        if Pos(PS1, Line) = 1 then begin
          Command := ecNone;  // do not processed it further
          SynEdit.CaretX := Length(PS1) + 1;
        end else if Pos(PS2, Line) = 1 then begin
          Command := ecNone;  // do not processed it further
          SynEdit.CaretX := Length(PS2) + 1;
        end;
      end;
    ecChar, ecDeleteChar, ecDeleteWord, ecDeleteLastWord, ecCut, ecPaste:
      begin
        Line := SynEdit.Lines[SynEdit.CaretY - 1];
        if ((Pos(PS1, Line) = 1) and (SynEdit.CaretX <= Length(PS1))) or
             ((Pos(PS2, Line) = 1) and (SynEdit.CaretX <= Length(PS2)))
        then
            Command := ecNone;  // do not processed it further
      end;
  end;
end;

procedure TPythonIIForm.SynEditCommandProcessed(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: WideChar; Data: Pointer);
const
  OpenBrackets : WideString = '([{';
  CloseBrackets : WideString = ')]}';
Var
  OpenBracketPos : integer;
  Line: WideString;
begin
  if (Command = ecChar) and CommandsDataModule.PyIDEOptions.AutoCompleteBrackets then
  with SynEdit do begin
    if aChar = fCloseBracketChar then begin
      Line := LineText;
      if InsertMode and (CaretX <= Length(Line)) and (Line[CaretX] = fCloseBracketChar) then
        ExecuteCommand(ecDeleteChar, WideChar(#0), nil);
      fCloseBracketChar := #0;
    end else begin
      fCloseBracketChar := #0;
      OpenBracketPos := Pos(aChar, OpenBrackets);
      if (OpenBracketPos > 0) and
          (CaretX > Length(LineText)) then
      begin
        SelText := CloseBrackets[OpenBracketPos];
        CaretX := CaretX - 1;
        fCloseBracketChar := CloseBrackets[OpenBracketPos];
      end;
    end;
  end;
end;

procedure TPythonIIForm.SynEditDblClick(Sender: TObject);
var
   RegExpr : TRegExpr;
   ErrLineNo : integer;
   FileName : string;
begin
  RegExpr := TRegExpr.Create;
  try
    RegExpr.Expression := STracebackFilePosExpr;
    if RegExpr.Exec(Synedit.LineText) then begin
      ErrLineNo := StrToIntDef(RegExpr.Match[3], 0);
      FileName := GetLongFileName(ExpandFileName(RegExpr.Match[1]));
      PyIDEMainForm.ShowFilePosition(FileName, ErrLineNo, 1);
    end;
  finally
    RegExpr.Free;
  end;

end;

procedure TPythonIIForm.SynEditProcessUserCommand(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: WideChar; Data: Pointer);
Var
  LineN, StartLineN, EndLineN, i: integer;
  IsCode: Boolean;
  Source : WideString;
  Buffer : array of WideString;
  SL : TWideStringList;
begin
  if Command = ecCodeCompletion then begin
    if SynCodeCompletion.Form.Visible then
      SynCodeCompletion.CancelCompletion;
    //SynCodeCompletion.DefaultType := ctCode;
    SynCodeCompletion.ActivateCompletion;
    Command := ecNone;
  end else if Command = ecParamCompletion then begin
    if SynParamCompletion.Form.Visible then
      SynParamCompletion.CancelCompletion;
    //SynCodeCompletion.DefaultType := ctParams;
    SynParamCompletion.ActivateCompletion;
    Command := ecNone;
  end;

  //  The following does not work. compiler bug???
  // if Command in [ecRecallCommandPrev, ecRecallCommandNext, ecRecallCommandEsc] then begin
  if (Command = ecRecallCommandPrev) or (Command = ecRecallCommandNext) or
     (Command = ecRecallCommandEsc) then
  begin
    SynCodeCompletion.CancelCompletion;
    SynParamCompletion.CancelCompletion;
    LineN := SynEdit.CaretY -1;
    GetBlockBoundary(LineN, StartLineN, EndLineN, IsCode);
    // if we have code at the end remove code block
    if IsCode and (EndLineN = SynEdit.Lines.Count - 1) then begin
      SynEdit.BeginUpdate;
      try
        for i := EndLineN downto StartLineN do
          SynEdit.Lines.Delete(i);
      finally
        SynEdit.EndUpdate;
      end;
    end;
    //Append new prompt if needed
    SetLength(Buffer, 0);
    AppendToPrompt(Buffer);
    SynEdit.ExecuteCommand(ecEditorBottom, ' ', nil);
    SynEdit.EnsureCursorPosVisible;
  end else
    Exit;

  // We get here if Command is one or our defined commands

  Source := '';
  if fCommandHistory.Count = 0 then Exit;

  if Command = ecRecallCommandPrev then
    fCommandHistoryPointer := Max (fCommandHistoryPointer - 1, -1)
  else if Command = ecRecallCommandNext then
    fCommandHistoryPointer := Min (fCommandHistoryPointer + 1, fCommandHistory.Count)
  else if Command = ecRecallCommandEsc then
    fCommandHistoryPointer := fCommandHistory.Count;


  if (fCommandHistoryPointer >= 0) and
     (fCommandHistoryPointer < fCommandHistory.Count) then
    Source := fCommandHistory[fCommandHistoryPointer];

  if Source <> '' then begin
    SL := TWideStringList.Create;
    try
      SL.Text := Source;
      SetLength(Buffer, SL.Count);
      for i := 0 to SL.Count - 1 do
        Buffer[i] := SL[i];
      AppendToPrompt(Buffer);
      SynEdit.ExecuteCommand(ecEditorBottom, ' ', nil);
      SynEdit.EnsureCursorPosVisible;
    finally
      SL.Free;
    end;
  end;
  Command := ecNone;  // do not processed it further
end;


procedure TPythonIIForm.SetCommandHistorySize(const Value: integer);
Var
  i : integer;
begin
  fCommandHistorySize := Value;
  if FCommandHistory.Count > Value then begin
    for i := 1 to FCommandHistory.Count - Value do
      FCommandHistory.Delete(0);
  end;
end;

procedure TPythonIIForm.SynCodeCompletionExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: WideString; var x, y: Integer;
  var CanExecute: Boolean);
{-----------------------------------------------------------------------------
  Based on code from Syendit Demo
-----------------------------------------------------------------------------}
var locline, lookup: String;
    TmpX, Index, ImageIndex, i,
    TmpLocation    : Integer;
    FoundMatch     : Boolean;
    DisplayText, InsertText, S : string;
    InspectModule, ItemsDict, ItemKeys,
    ItemValue, StringModule, LookupObj : Variant;
begin
  with TSynCompletionProposal(Sender).Editor do
  begin
    locLine := LineText;

    //go back from the cursor and find the first open paren
    TmpX := CaretX;
    if TmpX > length(locLine) then
      TmpX := length(locLine)
    else dec(TmpX);
    TmpLocation := 0;

    lookup := GetWordAtPos(LocLine, TmpX, IdentChars+['.'], True, False, True);
    Index := CharLastPos(lookup, '.');
    InspectModule := Import('inspect');
    StringModule := Import('string');
    if Index > 0 then begin
      lookup := Copy(lookup, 1, Index-1);
      DisplayText := '';
      try
        //Evaluate the lookup expression and get the hint text
        fShowOutput := False;  // Do not show Traceback for errors
        LookupObj := II.evalcode(lookup);
        ItemsDict := BuiltinModule.dict(InspectModule.getmembers(LookupObj));
      except
        ItemsDict := BuiltinModule.dict();
      end;
      fShowOutput := True;
    end else begin
      //  globals and builtins (could add keywords as well)
      ItemsDict := BuiltInModule.dict();
      ItemsDict.update(VarPythonEval('vars()'));
      ItemsDict.update(BuiltinModule.__dict__);
    end;
    ItemKeys := ItemsDict.keys();
    ItemKeys.sort();
    DisplayText := '';
    for i := 0 to len(ItemKeys) - 1 do begin
      S := ItemKeys.GetItem(i);
      ItemValue := ItemsDict.GetItem(S);
      if InspectModule.ismodule(ItemValue) then
        ImageIndex := 16
      else if InspectModule.isfunction(ItemValue)
           or InspectModule.isbuiltin(ItemValue) then
        ImageIndex := 17
      else if InspectModule.ismethod(ItemValue)
           or InspectModule.ismethoddescriptor(ItemValue) then
        ImageIndex := 14
      else if InspectModule.isclass(ItemValue) then
        ImageIndex := 13
      else begin
        if Index > 0 then
          ImageIndex := 1
        else
          ImageIndex := 0;
      end;
      DisplayText := DisplayText + Format('\Image{%d}\hspace{2}%s', [ImageIndex, S]);
      if i < len(ItemKeys) - 1 then
        DisplayText := DisplayText + #10;
    end;
    InsertText := StringModule.join(ItemKeys, ''#10);
    FoundMatch := DisplayText <> '';
  end;

  CanExecute := FoundMatch;

  if CanExecute then begin
    TSynCompletionProposal(Sender).Form.CurrentIndex := TmpLocation;
    TSynCompletionProposal(Sender).ItemList.Text := DisplayText;
    TSynCompletionProposal(Sender).InsertList.Text := InsertText;
  end else begin
    TSynCompletionProposal(Sender).ItemList.Clear;
    TSynCompletionProposal(Sender).InsertList.Clear;
  end;
end;

procedure TPythonIIForm.SynParamCompletionExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: WideString; var x, y: Integer;
  var CanExecute: Boolean);
var locline, lookup: String;
    TmpX, StartX,
    ParenCounter,
    TmpLocation : Integer;
    FoundMatch : Boolean;
    DisplayText, DocString : string;
    p : TPoint;
    LookUpObject, PyDocString : Variant;
begin
  with TSynCompletionProposal(Sender).Editor do
  begin
    locLine := LineText;

    //go back from the cursor and find the first open paren
    TmpX := CaretX;
    StartX := CaretX;
    if TmpX > length(locLine) then
      TmpX := length(locLine)
    else dec(TmpX);
    FoundMatch := False;
    TmpLocation := 0;

    while (TmpX > 0) and not(FoundMatch) do
    begin
      if LocLine[TmpX] = ',' then
      begin
        inc(TmpLocation);
        dec(TmpX);
      end else if LocLine[TmpX] = ')' then
      begin
        //We found a close, go till it's opening paren
        ParenCounter := 1;
        dec(TmpX);
        while (TmpX > 0) and (ParenCounter > 0) do
        begin
          if LocLine[TmpX] = ')' then inc(ParenCounter)
          else if LocLine[TmpX] = '(' then dec(ParenCounter);
          dec(TmpX);
        end;
        if TmpX > 0 then dec(TmpX);  //eat the open paren
      end else if locLine[TmpX] = '(' then
      begin
        //we have a valid open paren, lets see what the word before it is
        StartX := TmpX;
        while (TmpX > 0) and not(locLine[TmpX] in IdentChars+['.']) do  // added [.]
          Dec(TmpX);
        if TmpX > 0 then
        begin
          lookup := GetWordAtPos(LocLine, TmpX, IdentChars+['.'], True, False, True);

          try
            //Evaluate the lookup expression and get the hint text
            fShowOutput := False;  // Do not show Traceback for errors
            LookUpObject := II.evalcode(lookup);
            DisplayText := II.get_arg_text(LookUpObject);
            FoundMatch := True;
          except
            DisplayText := '';
            FoundMatch := False;
          end;
          fShowOutput := True;

          if not(FoundMatch) then
          begin
            TmpX := StartX;
            dec(TmpX);
          end;
        end;
      end else dec(TmpX)
    end;
  end;

  if FoundMatch then begin
    PyDocString := Import('inspect').getdoc(LookUpObject);
    if not VarIsNone(PyDocString) then
      DocString := GetNthLine(PyDocString, 1)
    else
      DocString := '';
//    CanExecute := (DisplayText <> '') or (DocString <> '');
    CanExecute := True;
  end else
    CanExecute := False;

  if CanExecute then begin
    with TSynCompletionProposal(Sender) do begin
      if DisplayText = '' then begin
        FormatParams := False;
        DisplayText :=  '\style{~B}' + SNoParameters + '\style{~B}';
      end else begin
        FormatParams := True;
      end;
      if (DocString <> '') then
        DisplayText := DisplayText + sLineBreak;

      Form.CurrentIndex := TmpLocation;
      ItemList.Text := DisplayText + DocString;
    end;

    //  position the hint window at and just below the opening bracket
    p := SynEdit.ClientToScreen(SynEdit.RowColumnToPixels(
      SynEdit.BufferToDisplayPos(BufferCoord(Succ(StartX), SynEdit.CaretY))));
    Inc(p.y, SynEdit.LineHeight);
    x := p.X;
    y := p.Y;
  end else begin
    TSynCompletionProposal(Sender).ItemList.Clear;
    TSynCompletionProposal(Sender).InsertList.Clear;
  end;
end;

function TPythonIIForm.FormHelp(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
Var
  KeyWord : string;
begin
  Keyword := SynEdit.WordAtCursor;
  if not PyIDEMainForm.PythonKeywordHelpRequested and
    not PyIDEMainForm.MenuHelpRequested and (Keyword <> '') then
  begin
    CallHelp := not CommandsDataModule.ShowPythonKeywordHelp(KeyWord);
    Result := True;
  end else begin
    CallHelp := True;
    Result := False;
  end;
end;

procedure TPythonIIForm.InputBoxExecute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
// InputBox function
Var
  PCaption, PPrompt, PDefault : PChar;
  S : string;
begin
  with GetPythonEngine do
    if PyArg_ParseTuple( args, 'sss:InputBox', [@PCaption, @PPrompt, @PDefault] ) <> 0 then begin
      S := PDefault;
      if InputQuery(PCaption, PPrompt, S) then
        Result := PyString_FromString(PChar(S))
      else
        Result := ReturnNone;
    end else
      Result := nil;
end;

procedure TPythonIIForm.StatusWriteExecute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
// statusWrite
Var
  Msg : PChar;
begin
  with GetPythonEngine do
    if PyArg_ParseTuple( args, 's:statusWrite', [@Msg] ) <> 0 then begin
      PyIDEMainForm.WriteStatusMsg(Msg);
      Application.ProcessMessages;
      Result := ReturnNone;
    end else
      Result := nil;
end;

procedure TPythonIIForm.MessageWriteExecute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
// messageWrite
Var
  Msg, FName : PChar;
  LineNo, Offset : integer;
  S : string;
begin
  FName := nil;
  LineNo := 0;
  Offset := 0;
  with GetPythonEngine do
    if PyArg_ParseTuple( args, 's|sii:messageWrite', [@Msg, @FName, @LineNo, @Offset] ) <> 0 then begin
      if Assigned(FName) then
        S := FName
      else
        S := '';
      MessagesWindow.AddMessage(Msg, S, LineNo, Offset);
      Application.ProcessMessages;
      Result := ReturnNone;
    end else
      Result := nil;
end;

procedure TPythonIIForm.Get8087CWExecute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  Result := GetPythonEngine.PyLong_FromUnsignedLong(Get8087CW);
end;

procedure TPythonIIForm.UnMaskFPUExceptionsExecute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  MaskFPUExceptions(False);
  CommandsDataModule.PyIDEOptions.MaskFPUExceptions := False;
  Result := GetPythonEngine.ReturnNone;
end;

procedure TPythonIIForm.MaskFPUExceptionsExecute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  MaskFPUExceptions(True);
  CommandsDataModule.PyIDEOptions.MaskFPUExceptions := True;
  Result := GetPythonEngine.ReturnNone;
end;

procedure TPythonIIForm.testResultStartTestExecute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  UnitTestWindow.StartTest(VarPythonCreate(Args).GetItem(0));
  Result := GetPythonEngine.ReturnNone;
end;

procedure TPythonIIForm.testResultStopTestExecute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  UnitTestWindow.StopTest(VarPythonCreate(Args).GetItem(0));
  Result := GetPythonEngine.ReturnNone;
end;

procedure TPythonIIForm.testResultAddSuccess(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  UnitTestWindow.AddSuccess(VarPythonCreate(Args).GetItem(0));
  Result := GetPythonEngine.ReturnNone;
end;

procedure TPythonIIForm.testResultAddFailure(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  UnitTestWindow.AddFailure(VarPythonCreate(Args).GetItem(0),
    VarPythonCreate(Args).GetItem(1));
  Result := GetPythonEngine.ReturnNone;
end;

procedure TPythonIIForm.testResultAddError(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  UnitTestWindow.AddError(VarPythonCreate(Args).GetItem(0),
    VarPythonCreate(Args).GetItem(1));
  Result := GetPythonEngine.ReturnNone;
end;

procedure TPythonIIForm.FormActivate(Sender: TObject);
begin
  inherited;
  if not HasFocus then begin
    FGPanelEnter(Self);
    PostMessage(SynEdit.Handle, WM_SETFOCUS, 0, 0);
  end;
end;

procedure TPythonIIForm.TBMThemeChange(var Message: TMessage);
begin
  inherited;
  if Message.WParam = TSC_VIEWCHANGE then begin
    // Update the gutter of the PythonII editor
    PyIDEMainForm.ThemeEditorGutter(SynEdit.Gutter);
    SynEdit.InvalidateGutter;
  end;
end;

procedure TPythonIIForm.InterpreterPopUpPopup(Sender: TObject);
begin
  actCleanUpNameSpace.Checked := CommandsDataModule.PyIDEOptions.CleanupMainDict;
  actCleanUpSysModules.Checked := CommandsDataModule.PyIDEOptions.CleanupSysModules;
end;

procedure TPythonIIForm.WMAPPENDTEXT(var Message: TMessage);
Var
  WS : WideString;
begin
  fCriticalSection.Acquire;
  try
    if fOutputStream.Size > 0 then begin
      SetLength(WS, fOutputStream.Size div 2);
      fOutputStream.Position := 0;
      fOutputStream.Read(WS[1], Length(WS)*2);
      AppendText(WS);
      fOutputStream.Size := 0;
    end;
  finally
    fCriticalSection.Release;
  end;
end;

function TPythonIIForm.IsEmpty : Boolean;
begin
  Result := (SynEdit.Lines.Count  = 0) or
    ((SynEdit.Lines.Count  = 1) and (SynEdit.Lines[0] = ''));
end;

function TPythonIIForm.CanFind: boolean;
begin
  Result := not IsEmpty;
end;

function TPythonIIForm.CanFindNext: boolean;
begin
  Result := not IsEmpty and
    (CommandsDataModule.EditorSearchOptions.SearchText <> '');
end;

function TPythonIIForm.CanReplace: boolean;
begin
  Result := not IsEmpty;
end;

procedure TPythonIIForm.ExecFind;
begin
  CommandsDataModule.ShowSearchReplaceDialog(SynEdit, FALSE);
end;

procedure TPythonIIForm.ExecFindNext;
begin
  CommandsDataModule.DoSearchReplaceText(SynEdit, FALSE, FALSE);
end;

procedure TPythonIIForm.ExecFindPrev;
begin
  CommandsDataModule.DoSearchReplaceText(SynEdit, FALSE, TRUE);
end;

procedure TPythonIIForm.ExecReplace;
begin
  CommandsDataModule.ShowSearchReplaceDialog(SynEdit, TRUE);
end;

procedure TPythonIIForm.RegisterHistoryCommands;
begin
  // Register the Recall History Command
  with SynEdit.Keystrokes.Add do
  begin
    ShortCut := Menus.ShortCut(VK_UP, [ssAlt]);
    Command := ecRecallCommandPrev;
  end;
  with SynEdit.Keystrokes.Add do
  begin
    ShortCut := Menus.ShortCut(VK_DOWN, [ssAlt]);
    Command := ecRecallCommandNext;
  end;
  with SynEdit.Keystrokes.Add do
  begin
    ShortCut := Menus.ShortCut(VK_ESCAPE, []);
    Command := ecRecallCommandEsc;
  end;
end;


procedure TPythonIIForm.SynEditReplaceText(Sender: TObject; const ASearch,
  AReplace: WideString; Line, Column: Integer; var Action: TSynReplaceAction);
var
  APos: TPoint;
  EditRect: TRect;
begin
  if ASearch = AReplace then
    Action := raSkip
  else begin
    APos := SynEdit.ClientToScreen(
      SynEdit.RowColumnToPixels(
      SynEdit.BufferToDisplayPos(
      BufferCoord(Column, Line) ) ) );
    EditRect := ClientRect;
    EditRect.TopLeft := ClientToScreen(EditRect.TopLeft);
    EditRect.BottomRight := ClientToScreen(EditRect.BottomRight);

    if ConfirmReplaceDialog = nil then
      ConfirmReplaceDialog := TConfirmReplaceDialog.Create(Application);
    ConfirmReplaceDialog.PrepareShow(EditRect, APos.X, APos.Y,
      APos.Y + SynEdit.LineHeight, ASearch);
    case ConfirmReplaceDialog.ShowModal of
      mrYes: Action := raReplace;
      mrYesToAll: Action := raReplaceAll;
      mrNo: Action := raSkip;
      else Action := raCancel;
    end;
  end;
end;

end.






