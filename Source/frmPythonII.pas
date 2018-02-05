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
  Types, System.UITypes, System.Contnrs, Windows, Messages, SysUtils,
  Variants, Classes, Graphics, Controls, Forms,
  Dialogs , Menus, PythonEngine, SyncObjs, SynHighlighterPython,
  SynEditHighlighter, SynEdit,
  SynEditKeyCmds, SynCompletionProposal, JvDockControlForm,
  frmIDEDockWin, ExtCtrls, JvComponentBase,
  TB2Item, ActnList, cPyBaseDebugger, WrapDelphi,
  SpTBXItem, SpTBXSkins, uEditAppIntfs, cCodeCompletion, System.Actions,
  SpTBXControls;

const
  WM_APPENDTEXT = WM_USER + 1020;
  WM_REINITINTERPRETER = WM_USER + 1030;
  WM_PARAMCOMPLETION = WM_USER +1040;

type

  TCodeCompletionType = (cctNone, cctModule, cctFrom, cctNameSpace);

  TPythonIIForm = class(TIDEDockWindow, ISearchCommands)
    SynEdit: TSynEdit;
    PythonEngine: TPythonEngine;
    PythonIO: TPythonInputOutput;
    SynCodeCompletion: TSynCompletionProposal;
    DebugIDE: TPythonModule;
    SynParamCompletion: TSynCompletionProposal;
    InterpreterPopUp: TSpTBXPopupMenu;
    TBXSeparatorItem1: TSpTBXSeparatorItem;
    mnInterpreterEditorOptions: TSpTBXItem;
    TBXSeparatorItem2: TSpTBXSeparatorItem;
    mnCopyHistory: TSpTBXItem;
    mnClearAll: TSpTBXItem;
    TBXPythonEngines: TSpTBXSubmenuItem;
    TBXSeparatorItem3: TSpTBXSeparatorItem;
    PyDelphiWrapper: TPyDelphiWrapper;
    PyscripterModule: TPythonModule;
    mnEditPaste: TSpTBXItem;
    mnEditCopy: TSpTBXItem;
    mnEditCut: TSpTBXItem;
    mnCopyNoPrompts: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    mnPasteWithPrompts: TSpTBXItem;
    InterpreterActionList: TActionList;
    actPasteAndExecute: TAction;
    actCopyWithoutPrompts: TAction;
    actClearContents: TAction;
    actCopyHistory: TAction;
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
    procedure SynEditPaintTransient(Sender: TObject; Canvas: TCanvas;
      TransientType: TTransientType);
    procedure FormCreate(Sender: TObject);
    procedure SynEditProcessCommand(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: WideChar; Data: Pointer);
    procedure SynEditProcessUserCommand(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: WideChar; Data: Pointer);
    procedure SynCodeCompletionExecute(Kind: SynCompletionType;
      Sender: TObject; var CurrentInput: string; var x, y: Integer;
      var CanExecute: Boolean);
    function FormHelp(Command: Word; Data: NativeInt;
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
      Sender: TObject; var CurrentInput: string; var x, y: Integer;
      var CanExecute: Boolean);
    procedure SynEditCommandProcessed(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: WideChar; Data: Pointer);
    procedure actCopyHistoryExecute(Sender: TObject);
    procedure SynEditDblClick(Sender: TObject);
    procedure awakeGUIExecute(Sender: TObject; PSelf, Args: PPyObject;
      var Result: PPyObject);
    procedure actClearContentsExecute(Sender: TObject);
    procedure SynEditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SynCodeCompletionClose(Sender: TObject);
    procedure PythonEngineAfterInit(Sender: TObject);
    procedure actCopyWithoutPromptsExecute(Sender: TObject);
    procedure actPasteAndExecuteExecute(Sender: TObject);
    procedure SynEditEnter(Sender: TObject);
    procedure SynEditExit(Sender: TObject);
    procedure SynEditMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure SynEditMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure SynCodeCompletionAfterCodeCompletion(Sender: TObject;
      const Value: string; Shift: TShiftState; Index: Integer; EndToken: Char);
  private
    { Private declarations }
    fCommandHistory : TStringList;
    fCommandHistorySize : integer;
    fCommandHistoryPointer : integer;
    fCommandHistoryPrefix : string;
    fShowOutput : Boolean;
    FCriticalSection : TCriticalSection;
    fOutputStream : TMemoryStream;
    fCloseBracketChar: WideChar;
    fOutputMirror : TFileStream;
    fCompletionHandler : TBaseCodeCompletionHandler;
    procedure CleanupCodeCompletion;
    procedure GetBlockBoundary(LineN: integer; var StartLineN,
              EndLineN: integer; var IsCode: Boolean);
    function GetPromptPrefix(line: string): string;
    procedure SetCommandHistorySize(const Value: integer);
    procedure GetBlockCode(var Source: string;
      var Buffer: array of string; EndLineN: Integer; StartLineN: Integer);
    function LoadPythonEngine : integer;
    procedure PrintInterpreterBanner;
    // ISearchCommands implementation
    function CanFind: boolean;
    function CanFindNext: boolean;
    function ISearchCommands.CanFindPrev = CanFindNext;
    function CanReplace: boolean;
    function GetSearchTarget : TSynEdit;
    procedure ExecFind;
    procedure ExecFindNext;
    procedure ExecFindPrev;
    procedure ExecReplace;
    procedure FindPythonHelpFile;
    procedure SynCodeCompletionCodeItemInfo(Sender: TObject;
      AIndex: Integer; var Info : string);
  protected
    procedure PythonIOSendData(Sender: TObject; const Data: string);
    procedure PythonIOReceiveData(Sender: TObject; var Data: string);
    procedure EditorMouseWheel(theDirection: Integer; Shift: TShiftState );
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
    procedure WMAPPENDTEXT(var Message: TMessage); message WM_APPENDTEXT;
    procedure WMREINITINTERPRETER(var Message: TMessage); message WM_REINITINTERPRETER;
    procedure WMPARAMCOMPLETION(var Message: TMessage); message WM_PARAMCOMPLETION;
  public
    { Public declarations }
    PS1, PS2, DebugPrefix, PMPrefix : string;
    PythonVersionIsRegistered : Boolean;
    AllUserInstall : Boolean;
    PythonHelpFile : string;
    function OutputSuppressor : IInterface;
    procedure WritePendingMessages;
    procedure ClearPendingMessages;
    procedure AppendText(S: string);
    procedure AppendToPrompt(const Buffer : array of string);
    procedure AppendPrompt;
    function IsEmpty : Boolean;
    procedure RegisterHistoryCommands;
    procedure SetPythonEngineType(PythonEngineType : TPythonEngineType);
    procedure StartOutputMirror(AFileName : string; Append : Boolean);
    procedure StopFileMirror;
    procedure UpdateInterpreterActions;
    procedure DoOnIdle;
    property ShowOutput : boolean read fShowOutput write fShowOutput;
    property CommandHistory : TStringList read fCommandHistory;
    property CommandHistoryPointer : integer read fCommandHistoryPointer write fCommandHistoryPointer;
    property CommandHistorySize : integer read fCommandHistorySize write SetCommandHistorySize;
  end;

var
  PythonIIForm: TPythonIIForm;

implementation

Uses
  SynEditTypes, Math, frmPyIDEMain, dmCommands, VarPyth, Registry,
  frmMessages, uCommonFunctions, frmVariables, StringResources,
  frmUnitTests, SynRegExpr,  JvJVCLUtils, uCmdLine,
  cPyDebugger, cPyRemoteDebugger, JclStrings, JvGnugettext,
  cProjectClasses, cPyScripterSettings;

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

function TPythonIIForm.OutputSuppressor: IInterface;
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

procedure TPythonIIForm.PythonEngineAfterInit(Sender: TObject);
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

    with (SynEdit.Highlighter as TSynPythonInterpreterSyn) do begin
      Keywords.Clear;
      Keywords.Sorted := False;
      Keywords.AddStrings(SynPythonSyn.Keywords);
      Keywords.Sorted := True;
    end;
  end;
end;

procedure TPythonIIForm.PythonIOReceiveData(Sender: TObject;
  var Data: string);
Var
  SaveThreadState: PPyThreadState;
  Res : Boolean;
begin
  with GetPythonEngine do begin
    SaveThreadState := PyEval_SaveThread();
    try
      Res := SyncWideInputQuery('PyScripter - Input requested', 'Input:', Data);
    finally
      PyEval_RestoreThread(SaveThreadState);
    end;
  end;
  if not Res then
    with GetPythonEngine do
      PyErr_SetString(PyExc_KeyboardInterrupt^, 'Operation cancelled')
  else
    Data := Data + #10;
end;

procedure TPythonIIForm.PythonIOSendData(Sender: TObject; const Data: string);
Var
  S : AnsiString;
begin
  if fShowOutput then begin
    fCriticalSection.Acquire;
    try
      fOutputStream.Write(Data[1], Length (Data) * 2);
      //fOutputStream.Write(sLineBreak[1], Length (sLineBreak) * 2);  RawOutput
      if Assigned(fOutputMirror) then begin
        S := Utf8Encode(Data);
        fOutputMirror.Write(S[1], Length(S));
      end;

      if GetCurrentThreadId = MainThreadId then
        WritePendingMessages
      else
        PostMessage(Handle, WM_APPENDTEXT, 0, 0);
    finally
      fCriticalSection.Release;
    end;
  end;
end;

procedure TPythonIIForm.actClearContentsExecute(Sender: TObject);
begin
  Synedit.ClearAll;
  PrintInterpreterBanner;
end;

procedure TPythonIIForm.actCopyHistoryExecute(Sender: TObject);
begin
  SetClipboardWideText(fCommandHistory.Text);
end;

procedure TPythonIIForm.actCopyWithoutPromptsExecute(Sender: TObject);
Var
  SelText : string;
  RegExpr : TRegExpr;
begin
  SelText := SynEdit.SelText;
  if SelText = '' then Exit;

  RegExpr := TRegExpr.Create;
  try
    RegExpr.ModifierM := True;
    RegExpr.Expression := '^((\[(Dbg|PM)\])?(>>>\ |\.\.\.\ ))';
    SelText := RegExpr.Replace(SelText, '')
  finally
    RegExpr.Free;
  end;

  SetClipboardWideText(SelText);
end;

procedure TPythonIIForm.actPasteAndExecuteExecute(Sender: TObject);

  procedure ExecuteBuffer(Buffer : TStringDynArray; NLines : integer);
  begin
    if NLines > 1 then begin
      SetLength(Buffer, NLines + 1);
      Buffer[NLines] := '';
    end
    else
      SetLength(Buffer, NLines);
    AppendToPrompt(Buffer);
    SynEdit.CommandProcessor(ecLineBreak, ' ', nil);
  end;

Var
  Buffer : TStringDynArray;
  Text : string;
  SL : TStringList;
  i, Line: Integer;
begin
  Text := GetClipboardWideText;
  if Text = '' then Exit;

  // Untabify
  Text :=  StringReplace(Text, #9,
     StringOfChar(' ', SynEdit.TabWidth), [rfReplaceAll]);
  // Dedent
  Text := Dedent(Text);
  Line := 0;

  SL := TStringList.Create;
  try
    SL.Text := Text;

    for i := 0 to SL.Count - 1 do begin
      if SL[i] = '' then continue;

      if CalcIndent(SL[i]) = 0 then
      begin
        if Length(Buffer) > 0 then
          // Allready have a full statement we need to execute
          ExecuteBuffer(Buffer, Line);
        Line := 0;
        SetLength(Buffer, SL.Count);
      end;
      Buffer[Line] := SL[i];
      Inc(Line);
    end;
  finally
    SL.Free;
  end;
  if Line > 0 then
    ExecuteBuffer(Buffer, Line);
end;

procedure TPythonIIForm.SetPythonEngineType(PythonEngineType: TPythonEngineType);
Var
  Cursor : IInterface;
  RemoteInterpreter : TPyRemoteInterpreter;
  Connected : Boolean;
  Msg : string;
begin
//  if PythonEngineType = CommandsDataModule.PyIDEOptions.PythonEngineType then
//    Exit;

  if PyControl.DebuggerState <> dsInactive then begin
    Dialogs.MessageDlg(_(SCannotChangeEngine), mtError, [mbAbort], 0);
    Exit;
  end;

  VariablesWindow.ClearAll;
  UnitTestWindow.ClearAll;

  case PythonEngineType of
    peInternal:
      begin
        PyControl.ActiveInterpreter := InternalInterpreter;
        PyControl.ActiveDebugger := TPyInternalDebugger.Create;
        PyIDEOptions.PythonEngineType := peInternal;
      end;
    peRemote, peRemoteTk, peRemoteWx:
      begin
        Application.ProcessMessages;
        Cursor := WaitCursor;
        // Destroy any active remote interpeter
        PyControl.ActiveDebugger := nil;
        PyControl.ActiveInterpreter := nil;
        try
          RemoteInterpreter := TPyRemoteInterpreter.Create(PythonEngineType);
          Connected := RemoteInterpreter.IsConnected;
        except
          Connected := False;
        end;
        if Connected then begin
          PyControl.ActiveInterpreter := RemoteInterpreter;
          PyControl.ActiveDebugger := TPyRemDebugger.Create(RemoteInterpreter);
          PyIDEOptions.PythonEngineType := PythonEngineType;

          // Add extra project paths
          if Assigned(ActiveProject) then
            ActiveProject.AppendExtraPaths;
        end else begin
          // failed to connect
          FreeAndNil(RemoteInterpreter);
          PyControl.ActiveInterpreter := InternalInterpreter;
          PyControl.ActiveDebugger := TPyInternalDebugger.Create;
          PyIDEOptions.PythonEngineType := peInternal;
        end;
      end;
  end;
  case PyIDEOptions.PythonEngineType of
    peInternal :  Msg := Format(_(SEngineActive), ['Internal','']);
    peRemote : Msg := Format(_(SEngineActive), ['Remote','']);
    peRemoteTk : Msg := Format(_(SEngineActive), ['Remote','(Tkinter) ']);
    peRemoteWx : Msg := Format(_(SEngineActive), ['Remote','(wxPython) ']);
  end;
  if SynEdit.Lines[SynEdit.Lines.Count-1] = PS1 then
    SynEdit.Lines.Delete(SynEdit.Lines.Count -1);
  AppendText(sLineBreak + Msg);
  AppendPrompt;

  PyControl.DoStateChange(dsInactive);
end;

procedure TPythonIIForm.PrintInterpreterBanner;
var
  SVersion, SPlatform, S: string;
begin
  SVersion := SysModule.version;
  SPlatform := SysModule.platform;
  S := Format('*** Python %s on %s. ***' + sLineBreak, [SVersion, SPlatform]);
  AppendText(S);
  AppendText(PS1);
end;

procedure TPythonIIForm.AppendPrompt;
var
  Buffer: array of string;
begin
  SetLength(Buffer, 0);
  AppendToPrompt(Buffer);
end;

procedure TPythonIIForm.ClearPendingMessages;
begin
  fCriticalSection.Acquire;
  try
    fOutputStream.Clear;
  finally
    fCriticalSection.Release;
  end;
end;

procedure TPythonIIForm.DoOnIdle;
begin
  if not Application.Active then
  begin
    if SynCodeCompletion.Form.Visible then
      SynCodeCompletion.CancelCompletion;
    if SynParamCompletion.Form.Visible then
      SynParamCompletion.CancelCompletion;
  end;
end;

procedure TPythonIIForm.WritePendingMessages;
var
  WS: string;
begin
  Assert(GetCurrentThreadId = MainThreadId);
  fCriticalSection.Acquire;
  try
    if fOutputStream.Size > 0 then begin
      SetLength(WS, fOutputStream.Size div 2);
      fOutputStream.Position := 0;
      fOutputStream.Read(WS[1], Length(WS) * 2);
      AppendText(WS);
      fOutputStream.Size := 0;
    end;
  finally
    fCriticalSection.Release;
  end;
end;

procedure TPythonIIForm.AppendText(S: string);
begin
  SynEdit.BeginUpdate;
  try
    SynEdit.ExecuteCommand(ecEditorBottom, ' ', nil);
    SynEdit.SelText := S;
    SynEdit.ExecuteCommand(ecEditorBottom, ' ', nil);
    SynEdit.EnsureCursorPosVisible;
  finally
    SynEdit.EndUpdate;
  end;
end;

procedure TPythonIIForm.AppendToPrompt(const Buffer: array of string);
Var
  LineCount, i : integer;
  Line : string;
begin
  LineCount := SynEdit.Lines.Count;
  Line := SynEdit.Lines[LineCount-1];
  SynEdit.BeginUpdate;
  try
    if Line <> PS1 then begin
      if Line <> '' then AppendText(sLineBreak);
      AppendText(PS1);
    end;
    for i := Low(Buffer) to High(Buffer) - 1  do
        AppendText(Buffer[i] + sLineBreak + PS2);
    if Length(Buffer) > 0 then AppendText(Buffer[High(Buffer)]);
  finally
    SynEdit.EndUpdate;
  end;
end;

procedure TPythonIIForm.FormCreate(Sender: TObject);
Var
  II : Variant;   // wrapping sys and code modules
  P : PPyObject;
begin
  inherited;
//  SynEdit.ControlStyle := SynEdit.ControlStyle + [csOpaque];

  SynCodeCompletion.EndOfTokenChr := WordBreakString;
  SynParamCompletion.EndOfTokenChr := WordBreakString;
  // Scale
  SynCodeCompletion.ChangeScale(Screen.PixelsPerInch, 96);
  SynParamCompletion.ChangeScale(Screen.PixelsPerInch, 96);

  SynEdit.OnReplaceText := CommandsDataModule.SynEditReplaceText;
  SynEdit.Highlighter := TSynPythonInterpreterSyn.Create(Self);
  SynEdit.Highlighter.Assign(CommandsDataModule.SynPythonSyn);

  SynEdit.Assign(InterpreterEditorOptions);
  RegisterHistoryCommands;

  // IO
  PythonIO.OnSendUniData := PythonIOSendData;
  PythonIO.OnReceiveUniData := PythonIOReceiveData;
  PythonIO.UnicodeIO := True;
  PythonIO.RawOutput := True;

  PyControl.PythonVersionIndex := LoadPythonEngine;

  fShowOutput := True;
  // For handling output from Python threads
  FCriticalSection := TCriticalSection.Create;
  fOutputStream := TMemoryStream.Create;

  //  For recalling old commands in Interactive Window;
  fCommandHistory := TStringList.Create();
  fCommandHistorySize := 50;
  fCommandHistoryPointer := 0;

  PS1 := SysModule.ps1;
  PS2 := SysModule.ps2;
  DebugPrefix := '[Dbg]';
  PMPrefix := '[PM]';

  PrintInterpreterBanner;

  FindPythonHelpFile;

  // Create internal Interpreter and Debugger
  II := VarPythonEval('_II');
  PythonEngine.ExecString('del _II');

  // Wrap IDE Options
  p := PyDelphiWrapper.Wrap(PyIDEOptions);
  PyscripterModule.SetVar('IDEOptions', p);
  PythonEngine.Py_XDECREF(p);

  InternalInterpreter := TPyInternalInterpreter.Create(II);
  PyControl.ActiveInterpreter := InternalInterpreter;
  PyControl.ActiveDebugger := TPyInternalDebugger.Create;
  InternalInterpreter.Initialize;

  SynCodeCompletion.OnCodeItemInfo := SynCodeCompletionCodeItemInfo;
end;

procedure TPythonIIForm.FormDestroy(Sender: TObject);
begin
  PyscripterModule.DeleteVar('IDEOptions');
  PyControl.ActiveDebugger := nil;  // Frees it
  PyControl.ActiveInterpreter := nil;  // Frees it

  FreeAndNil(InternalInterpreter);
  FreeAndNil(fCommandHistory);
  FreeAndNil(FCriticalSection);
  FreeAndNil(fOutputStream);
  FreeAndNil(fOutputMirror);
  inherited;
end;

procedure TPythonIIForm.GetBlockBoundary(LineN: integer; var StartLineN,
  EndLineN: integer; var IsCode: Boolean);
{-----------------------------------------------------------------------------
	  GetBlockBoundary takes a line number, and will return the
	  start and end line numbers of the block, and a flag indicating if the
	  block is a Python code block.
	  If the line specified has a Python prompt, then the lines are parsed
    backwards and forwards, and the IsCode is true.
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

function TPythonIIForm.GetSearchTarget: TSynEdit;
begin
  Result := SynEdit;
end;

procedure TPythonIIForm.SynEditProcessCommand(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: WideChar; Data: Pointer);
Var
  LineN, StartLineN, EndLineN, i, Position, Index : integer;
  NeedIndent : boolean;
  IsCode : Boolean;
  Line, CurLine, Source, Indent : string;
  EncodedSource : AnsiString;
  Buffer : array of string;
  NewCommand : TSynEditorCommand;
  WChar : WideChar;
  BC : TBufferCoord;
begin
  if (Command <> ecLostFocus) and (Command <> ecGotFocus) then
    EditorSearchOptions.InitSearch;
  case Command of
    ecLineBreak :
      begin
        Command := ecNone;  // do not processed it further

        fCommandHistoryPrefix := '';

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
          GetBlockCode(Source, Buffer, EndLineN, StartLineN);
          // If we are in a code-block, but it isnt at the end of the buffer
          // then copy it to the end ready for editing and subsequent execution
          if EndLineN <> SynEdit.Lines.Count - 1 then
            AppendToPrompt(Buffer)
          else if Trim(Source) = '' then begin
            AppendText(sLineBreak);
            AppendText(PS1);
          end else begin
            SynEdit.ExecuteCommand(ecEditorBottom, ' ', nil);
            AppendText(sLineBreak);

            //remove trailing whitespace
            for i := Length(Source) downto 1 do
              if (Source[i] = #9) or (Source[i] = #32) then Delete(Source, i, 1)
            else
              break;

            if PyIDEOptions.UTF8inInterpreter then
              EncodedSource := UTF8BOMString + Utf8Encode(Source)
            else
              EncodedSource := AnsiString(Source);

            // RunSource
            NeedIndent := False;  // True denotes an incomplete statement
            case PyControl.DebuggerState of
              dsInactive :
                if GetPythonEngine.IsPython3000 then
                  NeedIndent :=
                    PyControl.ActiveInterpreter.RunSource(Source, '<interactive input>')
                else
                  NeedIndent :=
                    PyControl.ActiveInterpreter.RunSource(EncodedSource, '<interactive input>');
              dsPaused, dsPostMortem :
                if GetPythonEngine.IsPython3000 then
                  NeedIndent :=
                    PyControl.ActiveDebugger.RunSource(Source, '<interactive input>')
                else
                  NeedIndent :=
                    PyControl.ActiveDebugger.RunSource(EncodedSource, '<interactive input>');
              else //dsRunning, dsRunningNoDebug
                // it is dangerous to execute code while running scripts
                // so just beep and do nothing
                MessageBeep(MB_ICONERROR);
            end;

            if not NeedIndent then begin
              // The source code has been executed
              WritePendingMessages;
              // If the last line isnt empty, append a newline
              SetLength(Buffer, 0);
              AppendToPrompt(Buffer);

              //  Add the command executed to History
              Index := fCommandHistory.IndexOf(Source);
              if Index >= 0  then
                fCommandHistory.Delete(Index);
              FCommandHistory.Add(Source);
              SetCommandHistorySize(fCommandHistorySize);
              fCommandHistoryPointer := fCommandHistory.Count;
              SynEdit.Refresh;
            end else begin
              // Now attempt to correct indentation
              CurLine := Copy(SynEdit.Lines[lineN], Length(PS2)+1, MaxInt); //!!
              Position := 1;
              Indent := '';
              while (Length(CurLine)>=Position) and
                   CharInSet(CurLine[Position], [#09, #32]) do begin
                Indent := Indent + CurLine[Position];
                Inc(Position);
              end;

              if CommandsDataModule.IsBlockOpener(CurLine) then begin
                if eoTabsToSpaces in SynEdit.Options then
                  Indent := Indent + StringOfChar(' ', SynEdit.TabWidth)
                else
                  Indent := indent + #9;
              end else if CommandsDataModule.IsBlockCloser(CurLine) then begin
                if (eoTabsToSpaces in SynEdit.Options) and (Length(Indent) > 0) and
                  (Indent[Length(Indent)] <> #9)
                then
                  Delete(Indent, Length(Indent) - SynEdit.TabWidth + 1, SynEdit.TabWidth)
                else
                  Delete(Indent, Length(Indent), 1);
              end;
              // use ReplaceSel to ensure it goes at the cursor rather than end of buffer.
              SynEdit.SelText := PS2 + Indent;
            end;
          end;
        end;
        SynEdit.EnsureCursorPosVisible;
      end;
    ecDeleteLastChar, ecDeleteLastWord :
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
    ecChar, ecDeleteChar, ecDeleteWord, ecCut, ecPaste:
      begin
        Line := SynEdit.Lines[SynEdit.CaretY - 1];
        if ((Pos(PS1, Line) = 1) and (SynEdit.CaretX <= Length(PS1))) or
             ((Pos(PS2, Line) = 1) and (SynEdit.CaretX <= Length(PS2)))
        then
          Command := ecNone;  // do not processed it further
      end;
    ecUp, ecDown :
      begin
        LineN := SynEdit.CaretY - 1;  // Caret is 1 based
        GetBlockBoundary(LineN, StartLineN, EndLineN, IsCode);
        if IsCode and (EndLineN = SynEdit.Lines.Count - 1) and
          (SynEdit.CaretX = Length(SynEdit.Lines[SynEdit.Lines.Count - 1])+1) then
        begin
          if Command = ecUp then
            NewCommand := ecRecallCommandPrev
          else
            NewCommand := ecRecallCommandNext;
          WChar := WideNull;
          SynEditProcessUserCommand(Self, NewCommand, WChar, nil);
          Command := ecNone;  // do not processed it further
        end;
      end;
    ecLeft :  // Implement Visual Studio like behaviour when selection is available
      if SynEdit.SelAvail then with SynEdit do begin
        CaretXY := BlockBegin;
        Command := ecNone;  // do not processed it further
      end;
    ecRight :  // Implement Visual Studio like behaviour when selection is available
      if SynEdit.SelAvail then with SynEdit do begin
        CaretXY := BlockEnd;
        Command := ecNone;  // do not processed it further
      end;
    ecWordRight, ecSelWordRight:  // Implement Visual Studio like behaviour
      begin
        BC := VSNextWordPos(SynEdit, SynEdit.CaretXY);
        if Command = ecWordRight then
          SynEdit.CaretXY := BC
        else begin
          if (SynEdit.BlockEnd.Line = SynEdit.CaretXY.Line) and
             (SynEdit.BlockEnd.Char = SynEdit.CaretXY.Char)
          then
            SynEdit.SetCaretAndSelection(BC, SynEdit.BlockBegin, BC)
          else
            SynEdit.SetCaretAndSelection(BC, SynEdit.BlockEnd, BC);
        end;
        Command := ecNone;  // do not processed it further
      end;
    ecWordLeft, ecSelWordLeft:  // Implement Visual Studio like behaviour
      begin
        BC := VSPrevWordPos(SynEdit, SynEdit.CaretXY);
        if Command = ecWordLeft then
          SynEdit.CaretXY := BC
        else begin
          if (SynEdit.BlockEnd.Line = SynEdit.CaretXY.Line) and
             (SynEdit.BlockEnd.Char = SynEdit.CaretXY.Char)
          then
            SynEdit.SetCaretAndSelection(BC, SynEdit.BlockBegin, BC)
          else
            SynEdit.SetCaretAndSelection(BC, SynEdit.BlockEnd, BC);
        end;
        Command := ecNone;  // do not processed it further
      end;
    ecMatchBracket :
      begin
        BC := GetMatchingBracket(SynEdit);
        if BC.Char > 0 then
          SynEdit.CaretXY := BC;
        Command := ecNone;  // do not processed it further
      end;
    ecLostFocus:
      if not (SynCodeCompletion.Form.Visible or SynEdit.Focused) then
        SynParamCompletion.CancelCompletion;
  end;
end;

procedure TPythonIIForm.SynEditCommandProcessed(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: WideChar; Data: Pointer);
const
  OpenBrackets : string = '([{"''';
  CloseBrackets : string = ')]}"''';
Var
  OpenBracketPos : integer;
  Line: string;
  Len, Position : Integer;
  CharRight: WideChar;
  CharLeft: WideChar;
  Attr: TSynHighlighterAttributes;
  DummyToken : string;
  BC : TBufferCoord;
begin
  if (Command = ecChar) and PyIDEOptions.AutoCompleteBrackets then
  with SynEdit do begin
    Line := LineText;
    Len := Length(LineText);

    if aChar = fCloseBracketChar then begin
      if InsertMode and (CaretX <= Len) and (Line[CaretX] = fCloseBracketChar) then
        ExecuteCommand(ecDeleteChar, WideChar(#0), nil);
      fCloseBracketChar := #0;
    end else if CharInSet(aChar, [')', ']', '}']) then begin
      fCloseBracketChar := #0;
      Position := CaretX;
      if Position <= Len then
        CharRight := Line[Position]
      else
        CharRight := WideNull;
      if (AChar = CharRight) and (GetMatchingBracket.Line <= 0) then
        ExecuteCommand(ecDeleteChar, #0, nil);
    end else begin
      fCloseBracketChar := #0;
      OpenBracketPos := Pos(aChar, OpenBrackets);

      BC := CaretXY;
      Dec(BC.Char, 2);
      if (BC.Char >= 1) and GetHighlighterAttriAtRowCol(BC, DummyToken, Attr) and
        ((attr = Highlighter.StringAttribute) or (attr = Highlighter.CommentAttribute)) then
          OpenBracketPos := 0;  // Do not auto complete brakets inside strings or comments

      if (OpenBracketPos > 0) then begin
        CharRight := WideNull;
        Position := CaretX;
        while (Position <= Len) and Highlighter.IsWhiteChar(LineText[Position]) do
          Inc(Position);
        if Position <= Len then
          CharRight := Line[Position];

        CharLeft := WideNull;
        Position := CaretX-2;
        while (Position >= 1) and Highlighter.IsWhiteChar(LineText[Position]) do
          Dec(Position);
        if Position >= 1 then
          CharLeft := Line[Position];

        if CharInSet(CharRight, [WideNull, ')', ']', '}', ',']) and
          not (CharInSet(aChar, ['"', '''']) and
           (Highlighter.IsIdentChar(CharLeft) or (CharLeft= aChar))) then
        begin
          SelText := CloseBrackets[OpenBracketPos];
          CaretX := CaretX - 1;
          fCloseBracketChar := CloseBrackets[OpenBracketPos];
        end;
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
      FileName := RegExpr.Match[1];
      //FileName := GetLongFileName(ExpandFileName(RegExpr.Match[1]));
      PyIDEMainForm.ShowFilePosition(FileName, ErrLineNo, 1);
    end else begin
      RegExpr.Expression := SWarningFilePosExpr;
      if RegExpr.Exec(Synedit.LineText) then begin
        ErrLineNo := StrToIntDef(RegExpr.Match[3], 0);
        FileName := RegExpr.Match[1];
        PyIDEMainForm.ShowFilePosition(FileName, ErrLineNo, 1);
      end;
    end;
  finally
    RegExpr.Free;
  end;

end;

procedure TPythonIIForm.SynEditEnter(Sender: TObject);
begin
  inherited;
  EditorSearchOptions.InterpreterIsSearchTarget := True;
  GI_SearchCmds := Self;
end;

procedure TPythonIIForm.SynEditExit(Sender: TObject);
begin
  inherited;
  GI_SearchCmds := nil;
end;

procedure TPythonIIForm.SynEditMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  EditorSearchOptions.InitSearch;
  if SynParamCompletion.Form.Visible then
    SynParamCompletion.CancelCompletion;
end;

procedure TPythonIIForm.SynEditMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  EditorMouseWheel( +1, Shift );
  Handled := True;
end;

procedure TPythonIIForm.SynEditMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  EditorMouseWheel( -1, Shift );
  Handled := True;
end;

procedure TPythonIIForm.SynEditProcessUserCommand(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: WideChar; Data: Pointer);
Var
  LineN, StartLineN, EndLineN, i: integer;
  IsCode: Boolean;
  Source, BlockSource : string;
  Buffer : array of string;
  P1, P2 : PWideChar;
  BC : TBufferCoord;
begin
  case Command of
    ecCodeCompletion :
      begin
        if SynCodeCompletion.Form.Visible then
          SynCodeCompletion.CancelCompletion;
        //SynCodeCompletion.DefaultType := ctCode;
        SynCodeCompletion.ActivateCompletion;
      end;
    ecParamCompletion:
      begin
        if SynParamCompletion.Form.Visible then
          SynParamCompletion.CancelCompletion;
        //SynCodeCompletion.DefaultType := ctParams;
        SynParamCompletion.ActivateCompletion;
      end;
    ecSelMatchBracket :
      begin
        BC := GetMatchingBracket(SynEdit);
        if BC.Char > 0 then
          SynEdit.SetCaretAndSelection(BC, SynEdit.CaretXY, BC);
      end;
    ecRecallCommandPrev,
    ecRecallCommandNext,
    ecRecallCommandEsc :
      begin
        if (Command = ecRecallCommandEsc) and SynParamCompletion.Form.Visible then
          SynParamCompletion.CancelCompletion
        else
        begin
          SynParamCompletion.CancelCompletion;
          LineN := SynEdit.CaretY -1;
          GetBlockBoundary(LineN, StartLineN, EndLineN, IsCode);
          SetLength(Buffer, EndLineN-StartLineN + 1);
          GetBlockCode(BlockSource, Buffer, EndLineN, StartLineN);
          // Prefix
          if fCommandHistoryPrefix <> '' then begin
            if not (IsCode and (EndLineN = SynEdit.Lines.Count - 1) and
                    (SynEdit.CaretX = Length(SynEdit.Lines[SynEdit.Lines.Count - 1])+1) and
                    InRange(fCommandHistoryPointer, 0, fCommandHistory.Count-1) and
                    (BlockSource =  fCommandHistory[fCommandHistoryPointer])) then
              fCommandHistoryPrefix := ''
          end else begin
            if IsCode and (EndLineN = SynEdit.Lines.Count - 1) and
                    (SynEdit.CaretX = Length(SynEdit.Lines[SynEdit.Lines.Count - 1])+1) and
                    not (InRange(fCommandHistoryPointer, 0, fCommandHistory.Count-1) and
                    (BlockSource =  fCommandHistory[fCommandHistoryPointer]))
            then
              fCommandHistoryPrefix := BlockSource;
          end;

          Source := '';
          if Command = ecRecallCommandEsc then begin
           fCommandHistoryPointer := fCommandHistory.Count;
           fCommandHistoryPrefix := '';
          end else
            Repeat
              if Command = ecRecallCommandPrev then
                Dec(fCommandHistoryPointer)
              else if Command = ecRecallCommandNext then
                Inc(fCommandHistoryPointer);
              fCommandHistoryPointer := EnsureRange(fCommandHistoryPointer, -1, fCommandHistory.Count);
            Until not InRange(fCommandHistoryPointer, 0, fCommandHistory.Count-1) or
              (fCommandHistoryPrefix = '') or
               StrIsLeft(PWideChar(fCommandHistory[fCommandHistoryPointer]), PWideChar(fCommandHistoryPrefix));

          if InRange(fCommandHistoryPointer, 0, fCommandHistory.Count-1) then
            Source := fCommandHistory[fCommandHistoryPointer]
          else begin
            if Command <> ecRecallCommandEsc then
              Beep();
            Source := fCommandHistoryPrefix;
            fCommandHistoryPrefix := '';
          end;

          SynEdit.BeginUpdate;
          try
            if IsCode and (EndLineN = SynEdit.Lines.Count - 1) then begin
              // already at the bottom and inside the prompt
             if (BlockSource <> Source) then begin
                for i := EndLineN downto StartLineN do
                  SynEdit.Lines.Delete(i);
                //Append new prompt if needed
                SetLength(Buffer, 0);
                AppendToPrompt(Buffer);
             end;  // else do nothing
            end else begin
              SetLength(Buffer, 0);
              AppendToPrompt(Buffer);
            end;

            if (Source <> '') and
              ((BlockSource <> Source) or (EndLineN < SynEdit.Lines.Count - 1)) then
            begin
              i := 0;
              P1 := PWideChar(Source);
              while P1 <> nil do begin
                P1 := StrScan(P1, WideLF);
                if Assigned(P1) then Inc(P1);
                Inc(i);
              end;
              SetLength(Buffer, i);

              i := 0;
              P1 := PWideChar(Source);
              while P1 <> nil do begin
                P2 := StrScan(P1, WideLF);
                if P2 = nil then
                  Buffer[i] := Copy(Source, P1 - PWideChar(Source) + 1,
                    Length(Source) - (P1 - PWideChar(Source)))
                else begin
                  Buffer[i] := Copy(Source, P1 - PWideChar(Source) + 1, P2 - P1);
                  Inc(P2);
                end;
                P1 := P2;
                Inc(i);
              end;
              AppendToPrompt(Buffer);
            end;
            SynEdit.ExecuteCommand(ecEditorBottom, ' ', nil);
            SynEdit.EnsureCursorPosVisible;
          finally
            SynEdit.EndUpdate;
          end;
        end;
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

procedure TPythonIIForm.SynCodeCompletionAfterCodeCompletion(Sender: TObject;
  const Value: string; Shift: TShiftState; Index: Integer; EndToken: Char);
begin
  if EndToken = '(' then
    PostMessage(Handle, WM_PARAMCOMPLETION, 0, 0);
end;

procedure TPythonIIForm.CleanupCodeCompletion;
begin
  if Assigned(fCompletionHandler) then
    fCompletionHandler.Finalize;
  fCompletionHandler := nil;
end;

procedure TPythonIIForm.SynCodeCompletionClose(Sender: TObject);
begin
  PyIDEOptions.CodeCompletionListSize :=
    SynCodeCompletion.NbLinesInWindow;
  //  Clean-up
  CleanupCodeCompletion;
end;

procedure TPythonIIForm.SynCodeCompletionExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: string; var x, y: Integer;
  var CanExecute: Boolean);
{-----------------------------------------------------------------------------
  Based on code from Syendit Demo
-----------------------------------------------------------------------------}
Var
  i : integer;
  Skipped, Handled : Boolean;
  locline: string;
  DisplayText, InsertText: string;
  FileName : string;
  Attr: TSynHighlighterAttributes;
  Prompt, DummyToken: string;
  BC: TBufferCoord;
  SkipHandler : TBaseCodeCompletionSkipHandler;
begin
  if PyControl.IsRunning or not PyIDEOptions.InterpreterCodeCompletion
  then
    // No code completion while Python is running
    Exit;

  with TSynCompletionProposal(Sender).Editor do
  begin
    locLine := StrPadRight(LineText, CaretX - 1, ' '); // to deal with trim trailing spaces
    Prompt := GetPromptPrefix(locLine);
    if Prompt <> '' then
      locLine := Copy(locLine, Length(Prompt) + 1, MaxInt)
    else
      Exit;  // This is not a code line

    BC := CaretXY;
    Dec(BC.Char);
    GetHighlighterAttriAtRowCol(BC, DummyToken, Attr);

    BC := CaretXY;
    BC.Char := BC.Char - Length(Prompt);
    FileName := '';

    Skipped := False;
    for I := 0 to TIDECompletion.InterpreterCodeCompletion.SkipHandlers.Count -1 do
    begin
      SkipHandler := TIDECompletion.InterpreterCodeCompletion.SkipHandlers[i] as
        TBaseCodeCompletionSkipHandler;
      Skipped := SkipHandler.SkipCodeCompletion(locline, FileName, BC, Highlighter, Attr);
      if Skipped then Break;
    end;

    Handled := False;
    if not Skipped then
    begin
      for I := 0 to TIDECompletion.InterpreterCodeCompletion.CompletionHandlers.Count -1 do
      begin
        fCompletionHandler := TIDECompletion.InterpreterCodeCompletion.CompletionHandlers[i] as
          TBaseCodeCompletionHandler;
        Handled := fCompletionHandler.HandleCodeCompletion(locline, FileName,
          BC, Highlighter, Attr, InsertText, DisplayText);
        if Handled then Break;
      end;
    end;

    CanExecute := not Skipped and Handled and (InsertText <> '');
  end;

  with TSynCompletionProposal(Sender) do
    if CanExecute then begin
      Font := PyIDEOptions.AutoCompletionFont;
      ItemList.Text := DisplayText;
      InsertList.Text := InsertText;
      NbLinesInWindow :=
        PyIDEOptions.CodeCompletionListSize;

      // Auto-complete with one entry without showing the form
      if PyIDEOptions.CompleteWithOneEntry then begin
        CurrentString := CurrentInput;
        if Form.AssignedList.Count = 1 then begin
          CanExecute := False;
          OnValidate(Form, [], #0);
          CleanupCodeCompletion;
        end;
      end;
    end else begin
      ItemList.Clear;
      InsertList.Clear;
      CleanupCodeCompletion;
    end;
end;

procedure TPythonIIForm.SynCodeCompletionCodeItemInfo(Sender: TObject;
  AIndex: Integer; var Info: string);
begin
  if Assigned(fCompletionHandler) then
    Info := fCompletionHandler.GetInfo((Sender as TSynCompletionProposal).InsertList[AIndex]);
end;

procedure TPythonIIForm.SynParamCompletionExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: string; var x, y: Integer;
  var CanExecute: Boolean);
var locline, lookup: string;
    TmpX, StartX,
    ParenCounter,
    TmpLocation : Integer;
    FoundMatch : Boolean;
    DisplayText, DocString : string;
    p : TPoint;
    Attr: TSynHighlighterAttributes;
    DummyToken : string;
    BC : TBufferCoord;
begin
  if PyControl.IsRunning or not PyIDEOptions.InterpreterCodeCompletion
  then
    Exit;
  with TSynCompletionProposal(Sender).Editor do
  begin
    BC := CaretXY;
    Dec(BC.Char);
    if GetHighlighterAttriAtRowCol(BC, DummyToken, Attr) and
     ({(attr = Highlighter.StringAttribute) or} (attr = Highlighter.CommentAttribute) or
      (attr = TSynPythonInterpreterSyn(Highlighter).CodeCommentAttri) or
      (attr = TSynPythonInterpreterSyn(Highlighter).MultiLineStringAttri) or
      (attr = TSynPythonInterpreterSyn(Highlighter).DocStringAttri)) then
    begin
      // Do not code complete inside strings or comments
      CanExecute := False;
      Exit;
    end;

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
      end else if locLine[TmpX] = '(' then
      begin
        //we have a valid open paren, lets see what the word before it is
        StartX := TmpX;
        Dec(TmpX);
        while (TmpX > 0) and (locLine[TmpX] = ' ') do
          Dec(TmpX);
        if TmpX > 0 then
        begin
          lookup := GetWordAtPos(LocLine, TmpX, IdentChars+['.'], True, False, True);

          if (lookup <> '') and (lookup[1] = '.') and
             (TmpX > Length(lookup)) and
             CharInSet(locline[TmpX - Length(lookup)], ['''', '"'])
          then
            lookup := 'str' + lookup;

          FoundMatch := (CharPos(lookup, ')') <= 0) // Issue 422  Do not evaluate functions
            and (lookup <> '')
            and PyControl.ActiveInterpreter.CallTipFromExpression(
            lookup, DisplayText, DocString);

          if not(FoundMatch) then
          begin
            TmpX := StartX;
            dec(TmpX);
          end;
        end;
      end else dec(TmpX)
    end;
  end;

  CanExecute := FoundMatch;

  if CanExecute then begin
    with TSynCompletionProposal(Sender) do begin
      Font := PyIDEOptions.AutoCompletionFont;
      FormatParams := not (DisplayText = '');
      if not FormatParams then
        DisplayText :=  '\style{~B}' + _(SNoParameters) + '\style{~B}';

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

function TPythonIIForm.FormHelp(Command: Word; Data: NativeInt;
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
  PCaption, PPrompt, PDefault : PWideChar;
  WideS : string;
  SaveThreadState: PPyThreadState;
  Res : Boolean;
begin
  with GetPythonEngine do
    if PyArg_ParseTuple( args, 'uuu:InputBox', @PCaption, @PPrompt, @PDefault) <> 0 then begin
      WideS := PDefault;

      with GetPythonEngine do begin
        SaveThreadState := PyEval_SaveThread();
        try
          Res := SyncWideInputQuery(PCaption, PPrompt, WideS);
        finally
          PyEval_RestoreThread(SaveThreadState);
        end;
      end;

      if Res then
        Result := PyUnicode_FromWideChar(PWideChar(WideS), Length(WideS))
      else
        Result := ReturnNone;
    end else
      Result := nil;
end;

procedure TPythonIIForm.StartOutputMirror(AFileName: string;
  Append: Boolean);
Var
  Mode : integer;
begin
  fCriticalSection.Acquire;
  try
    FreeAndNil(fOutputMirror);
    try
      if Append and FileExists(AFileName) then
        Mode := fmOpenReadWrite
      else
        Mode := fmCreate;
      fOutputMirror := TFileStream.Create(AFileName, Mode or fmShareDenyWrite);

      if Append and (fOutputMirror.Size > 0) then
        fOutputMirror.Seek(0, soFromEnd)
      else
        fOutputMirror.Write(UTF8BOMString[1], Length(UTF8BomString));  // save in utf8 encoding
    except
      Dialogs.MessageDlg(Format(_(SCouldNotOpenOutputFile), [AFileName]), mtWarning, [mbOK], 0);
    end;
  finally
    fCriticalSection.Release;
  end;
end;

procedure TPythonIIForm.StatusWriteExecute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
// statusWrite
Var
  Msg : PAnsiChar;
begin
  with GetPythonEngine do
    if PyArg_ParseTuple( args, 's:statusWrite', @Msg) <> 0 then begin
      PyIDEMainForm.WriteStatusMsg(string(Msg));
      Application.ProcessMessages;
      Result := ReturnNone;
    end else
      Result := nil;
end;

procedure TPythonIIForm.StopFileMirror;
begin
  fCriticalSection.Acquire;
  try
    FreeAndNil(fOutputMirror);
  finally
    fCriticalSection.Release;
  end;
end;

procedure TPythonIIForm.MessageWriteExecute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
// messageWrite
Var
  Msg, FName : PAnsiChar;
  LineNo, Offset : integer;
  S : string;
begin
  FName := nil;
  LineNo := 0;
  Offset := 0;
  with GetPythonEngine do
    if PyArg_ParseTuple( args, 's|sii:messageWrite', @Msg, @FName, @LineNo, @Offset) <> 0 then begin
      if Assigned(FName) then
        S := string(FName)
      else
        S := '';
      MessagesWindow.AddMessage(string(Msg), S, LineNo, Offset);
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

Var
  AwakeCount : integer;  //static variable
procedure TPythonIIForm.awakeGUIExecute(Sender: TObject; PSelf, Args: PPyObject;
  var Result: PPyObject);
var
  Done: Boolean;
begin
  //  This routine is called 100 times a second
  //  Do the Idle handling only twice per second to avoid slow down.
  PyControl.DoYield(False);
  if AwakeCount > 50 then begin
    AwakeCount := 0;
    PyIDEMainForm.ApplicationOnIdle(Application, Done);
  end else
    Inc(AwakeCount);
  //PostMessage(Application.Handle, WM_NULL, 0, 0);
  Result := GetPythonEngine.ReturnNone;
end;

procedure TPythonIIForm.UnMaskFPUExceptionsExecute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  MaskFPUExceptions(False);
  PyIDEOptions.MaskFPUExceptions := False;
  Result := GetPythonEngine.ReturnNone;
end;

procedure TPythonIIForm.UpdateInterpreterActions;
begin
  actCopyWithoutPrompts.Enabled := SynEdit.SelAvail;
  actPasteAndExecute.Enabled := ClipboardProvidesWideText;
end;

procedure TPythonIIForm.FindPythonHelpFile;
var
  Res: Integer;
  Registry: TRegistry;
  SR: TSearchRec;
  RegKey: string;
  PythonHelpFilePath : string;
begin
  if PythonVersionIsRegistered then
  begin
    Registry := TRegistry.Create(KEY_READ and not KEY_NOTIFY);
    try
      Registry.RootKey := HKEY_LOCAL_MACHINE;
      // False because we do not want to create it if it doesn't exist
      RegKey := '\SOFTWARE\Python\PythonCore\' + SysModule.winver + '\Help\Main Python Documentation';
      if Registry.OpenKey(RegKey, False) then
        PythonHelpFile := Registry.ReadString('')
      else
      begin
        // try Current User
        Registry.RootKey := HKEY_CURRENT_USER;
        if Registry.OpenKey(RegKey, False) then
          PythonHelpFile := Registry.ReadString('');
      end;
    finally
      Registry.Free;
    end;
  end;

  // for unregistered Python
  if PythonHelpFile = '' then
  begin
    PythonHelpFilePath := SysModule.prefix + '\Doc\*.chm';
    Res := FindFirst(PythonHelpFilePath, faAnyFile, SR);
    if Res = 0 then
      PythonHelpFile := SysModule.prefix + '\Doc\' + SR.Name;
    FindClose(SR);
  end;
end;

procedure TPythonIIForm.MaskFPUExceptionsExecute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  MaskFPUExceptions(True);
  PyIDEOptions.MaskFPUExceptions := True;
  Result := GetPythonEngine.ReturnNone;
end;

procedure TPythonIIForm.testResultStartTestExecute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  UnitTestWindow.StartTest(VarPythonCreate(Args).__getitem__(0));
  Result := GetPythonEngine.ReturnNone;
end;

procedure TPythonIIForm.testResultStopTestExecute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  UnitTestWindow.StopTest(VarPythonCreate(Args).__getitem__(0));
  Result := GetPythonEngine.ReturnNone;
end;

procedure TPythonIIForm.testResultAddSuccess(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  UnitTestWindow.AddSuccess(VarPythonCreate(Args).__getitem__(0));
  Result := GetPythonEngine.ReturnNone;
end;

procedure TPythonIIForm.testResultAddFailure(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  UnitTestWindow.AddFailure(VarPythonCreate(Args).__getitem__(0),
    VarPythonCreate(Args).__getitem__(1));
  Result := GetPythonEngine.ReturnNone;
end;

procedure TPythonIIForm.testResultAddError(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  UnitTestWindow.AddError(VarPythonCreate(Args).__getitem__(0),
    VarPythonCreate(Args).__getitem__(1));
  Result := GetPythonEngine.ReturnNone;
end;

procedure TPythonIIForm.FormActivate(Sender: TObject);
begin
  inherited;
  if CanActuallyFocus(Synedit) then
    SynEdit.SetFocus;
end;

procedure TPythonIIForm.WMSpSkinChange(var Message: TMessage);
begin
  inherited;
  // Update the gutter of the PythonII editor
  PyIDEMainForm.ThemeEditorGutter(SynEdit.Gutter);
  SynEdit.InvalidateGutter;
end;

procedure TPythonIIForm.WMAPPENDTEXT(var Message: TMessage);
Var
  Msg : TMsg;
begin
  // Remove other similar messages
  while PeekMessage(Msg, 0, WM_APPENDTEXT, WM_APPENDTEXT, PM_REMOVE) do
    ; // do nothing
  WritePendingMessages;
end;

procedure TPythonIIForm.WMPARAMCOMPLETION(var Message: TMessage);
begin
  SynParamCompletion.ActivateCompletion;
end;

procedure TPythonIIForm.WMREINITINTERPRETER(var Message: TMessage);
begin
  if Assigned(PyControl.ActiveInterpreter) then
    PyControl.ActiveInterpreter.ReInitialize;
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
    (EditorSearchOptions.SearchText <> '');
end;

function TPythonIIForm.CanReplace: boolean;
begin
  Result := not IsEmpty;
end;

procedure TPythonIIForm.EditorMouseWheel(theDirection: Integer;
  Shift: TShiftState);

  function OwnScroll(Shift: TShiftState; LinesInWindow: Integer): Integer;
  begin
    if (ssShift in Shift) or (Mouse.WheelScrollLines = -1)
    then
      Result := LinesInWindow shr Ord(eoHalfPageScroll in SynEdit.Options)
    else
      Result := Mouse.WheelScrollLines;
  end;
//
begin
{*
  Manage Zoom in and out, Page up and down, Line scroll - with the Mouse Wheel
*}
  if ssCtrl in Shift then
  begin
    if not ( (theDirection < 1) and (SynEdit.Font.Size <= 2) ) then begin
      SynEdit.Font.Size        := SynEdit.Font.Size        + theDirection;
      SynEdit.Gutter.Font.Size := Max(SynEdit.Font.Size -2, 1);
    end;
  end
  else
  begin
    SynEdit.TopLine := SynEdit.TopLine +
     (theDirection * OwnScroll( Shift, SynEdit.LinesInWindow ) );
  end;
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

function TPythonIIForm.LoadPythonEngine : integer;

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

var
  i: Integer;
  idx : Integer;
  versionIdx : Integer;
  expectedVersion : string;
  expectedVersionIdx : Integer;
  UseDebugVersion : Boolean;
  InstallPath : string;
begin
  Result := 0;
  // first find an optional parameter specifying the expected Python version in the form of -PYTHONXY
  expectedVersion := '';
  expectedVersionIdx := -1;

  if CmdLineReader.readFlag('PYTHON24') then
    expectedVersion := '2.4'
  else if CmdLineReader.readFlag('PYTHON25') then
    expectedVersion := '2.5'
  else if CmdLineReader.readFlag('PYTHON26') then
    expectedVersion := '2.6'
  else if CmdLineReader.readFlag('PYTHON27') then
    expectedVersion := '2.7'
  else if CmdLineReader.readFlag('PYTHON30') then
    expectedVersion := '3.0'
  else if CmdLineReader.readFlag('PYTHON31') then
    expectedVersion := '3.1'
  else if CmdLineReader.readFlag('PYTHON32') then
    expectedVersion := '3.2'
  else if CmdLineReader.readFlag('PYTHON33') then
    expectedVersion := '3.3'
  else if CmdLineReader.readFlag('PYTHON34') then
    expectedVersion := '3.4'
  else if CmdLineReader.readFlag('PYTHON35') then
    expectedVersion := '3.5'
  else if CmdLineReader.readFlag('PYTHON36') then
    expectedVersion := '3.6'
  else if CmdLineReader.readFlag('PYTHON37') then
    expectedVersion := '3.7';
  PythonEngine.DllPath := CmdLineReader.readString('PYTHONDLLPATH');
  UseDebugVersion := CmdLineReader.readFlag('DEBUG');

  if expectedVersion <> '' then begin
    idx := IndexOfKnownVersion(expectedVersion);
    if idx >= COMPILED_FOR_PYTHON_VERSION_INDEX then
      expectedVersionIdx := idx;
    if expectedVersionIdx = -1 then
      if idx = -1 then
        Dialogs.MessageDlg(Format(_(SUnknownPythonVersion),
          [StringReplace(expectedVersion, '.', '', [])]), mtWarning, [mbOK], 0)
      else
        Dialogs.MessageDlg(Format(_(SUnsupportedPythonVersion),
          [StringReplace(expectedVersion, '.', '', []),
           PYTHON_KNOWN_VERSIONS[COMPILED_FOR_PYTHON_VERSION_INDEX].RegVersion]),
           mtWarning, [mbOK], 0);
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
    PythonEngine.FatalAbort := True;
  end
  else
  begin
    // otherwise, let's start searching a valid python dll from the latest known version
    versionIdx := High(PYTHON_KNOWN_VERSIONS);
    PythonEngine.FatalMsgDlg := False;
    PythonEngine.FatalAbort := False;
  end;
  // try to find an acceptable version of Python, starting from either the specified version,
  // or the latest know version, but stop when we reach the version targeted on compilation.
  for i := versionIdx downto COMPILED_FOR_PYTHON_VERSION_INDEX do
  begin
    PythonEngine.DllName := PYTHON_KNOWN_VERSIONS[i].DllName;
    If UseDebugVersion then
      PythonEngine.DllName := ChangeFileExt(PythonEngine.DllName, '') + '_d.dll';
    PythonEngine.APIVersion := PYTHON_KNOWN_VERSIONS[i].APIVersion;
    PythonEngine.RegVersion := PYTHON_KNOWN_VERSIONS[i].RegVersion;

    if i = COMPILED_FOR_PYTHON_VERSION_INDEX then
    begin
      // last chance, so raise an error if it goes wrong
      PythonEngine.FatalMsgDlg := True;
      PythonEngine.FatalAbort := True;
    end;

    PythonVersionIsRegistered :=
      IsPythonVersionRegistered(PythonEngine.RegVersion, InstallPath, AllUserInstall);

    if PythonVersionIsRegistered  or (PythonEngine.DllPath <> '') then begin
      try
        PythonEngine.LoadDll;
        Result := i;
      except on E: EPyImportError do
        Dialogs.MessageDlg(_(SPythonInitError), mtError, [mbOK], 0);
      end;
      if PythonEngine.IsHandleValid then
        // we found a valid version
        Break;
    end;
  end;

  if not PythonEngine.IsHandleValid then begin
    Dialogs.MessageDlg(_(SPythonLoadError), mtError, [mbOK], 0);
    ExitProcess(1);
  end;
end;

procedure TPythonIIForm.GetBlockCode(var Source: string;
  var Buffer: array of string; EndLineN: Integer; StartLineN: Integer);
var
  Len: Integer;
  Line: string;
  i: Integer;
begin
  Assert(Length(Buffer) = EndLineN-StartLineN + 1);

  Source := '';
  for i := StartLineN to EndLineN do
  begin
    Line := SynEdit.Lines[i];
    Len := Length(GetPromptPrefix(Line));
    Buffer[i - StartLineN] := Copy(Line, Len + 1, MaxInt);
    Source := Source + Buffer[i - StartLineN] + WideLF;
  end;
  Delete(Source, Length(Source), 1);
end;


end.
