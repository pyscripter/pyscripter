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
  WinApi.Windows,
  WinApi.Messages,
  System.Types,
  System.UITypes,
  System.Contnrs,
  System.SysUtils,
  System.Classes,
  System.Variants,
  System.Actions,
  System.SyncObjs,
  System.ImageList,
  Vcl.Clipbrd,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Dialogs ,
  Vcl.Menus,
  Vcl.ActnList,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  JvComponentBase,
  JvDockControlForm,
  JvAppStorage,
  SynHighlighterPython,
  SynEditHighlighter,
  dlgSynEditOptions,
  TB2Item,
  SpTBXItem,
  SpTBXSkins,
  SpTBXControls,
  SynEdit,
  SynEditTypes,
  SynCompletionProposal,
  PythonEngine,
  WrapDelphi,
  frmIDEDockWin,
  uEditAppIntfs,
  cPySupportTypes,
  cCodeCompletion;

const
  WM_REINITINTERPRETER = WM_USER + 1030;

type
  TPythonIIForm = class(TIDEDockWindow, ISearchCommands, IPyInterpreter)
    SynEdit: TSynEdit;
    PythonIO: TPythonInputOutput;
    InterpreterPopUp: TSpTBXPopupMenu;
    TBXSeparatorItem1: TSpTBXSeparatorItem;
    mnInterpreterEditorOptions: TSpTBXItem;
    TBXSeparatorItem2: TSpTBXSeparatorItem;
    mnCopyHistory: TSpTBXItem;
    mnClearAll: TSpTBXItem;
    TBXPythonEngines: TSpTBXSubmenuItem;
    TBXSeparatorItem3: TSpTBXSeparatorItem;
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
    mnPythonVersions: TSpTBXSubmenuItem;
    vilCodeImages: TVirtualImageList;
    vilImages: TVirtualImageList;
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
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SynParamCompletionExecute(Kind: SynCompletionType;
      Sender: TObject; var CurrentInput: string; var x, y: Integer;
      var CanExecute: Boolean);
    procedure SynEditCommandProcessed(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: WideChar; Data: Pointer);
    procedure actCopyHistoryExecute(Sender: TObject);
    procedure SynEditDblClick(Sender: TObject);
    procedure actClearContentsExecute(Sender: TObject);
    procedure SynEditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SynCodeCompletionClose(Sender: TObject);
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
    FCriticalSection : TRTLCriticalSection;
    fOutputStream : TMemoryStream;
    fCloseBracketChar: WideChar;
    fOutputMirror : TFileStream;
    procedure GetBlockBoundary(LineN: integer; var StartLineN,
              EndLineN: integer; var IsCode: Boolean);
    function GetPromptPrefix(line: string): string;
    procedure SetCommandHistorySize(const Value: integer);
    procedure GetBlockCode(var Source: string;
      var Buffer: array of string; EndLineN: Integer; StartLineN: Integer);
    procedure DoCodeCompletion(Editor: TSynEdit; Caret: TBufferCoord);
    procedure ApplyPyIDEOptions;
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
    procedure SynCodeCompletionCodeItemInfo(Sender: TObject;
      AIndex: Integer; var Info : string);
    // Implementation of IPyInterpreter
    procedure ShowWindow;
    procedure AppendPrompt;
    procedure RemovePrompt;
    procedure AppendText(const S: string);
    procedure PrintInterpreterBanner(AVersion: string = ''; APlatform: string = '');
    procedure WritePendingMessages;
    procedure ClearPendingMessages;
    procedure ClearDisplay;
    procedure ClearLastPrompt;
    function OutputSuppressor : IInterface;
    procedure StartOutputMirror(const AFileName : string; Append : Boolean);
    procedure StopFileMirror;
    procedure UpdatePythonKeywords;
    procedure SetPyInterpreterPrompt(Pip: TPyInterpreterPropmpt);
    procedure ReinitInterpreter;
    function GetEditor: TCustomSynEdit;
    function GetPythonIO: TPythonInputOutput;
    function GetShowOutput: boolean;
    procedure SetShowOutput(const Value: boolean);
  protected
    procedure PythonIOReceiveData(Sender: TObject; var Data: string);
    procedure EditorMouseWheel(theDirection: Integer; Shift: TShiftState );
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
    procedure WMREINITINTERPRETER(var Message: TMessage); message WM_REINITINTERPRETER;
  public
    { Public declarations }
    PS1, PS2 : string;
    // AppStorage
    procedure StoreSettings(AppStorage: TJvCustomAppStorage); override;
    procedure RestoreSettings(AppStorage: TJvCustomAppStorage); override;

    procedure PythonIOSendData(Sender: TObject; const Data: string);
    procedure AppendToPrompt(const Buffer : array of string);
    function IsEmpty : Boolean;
    procedure UpdateInterpreterActions;
    procedure RegisterHistoryCommands;
    procedure ValidateEditorOptions(SynEditOptions: TSynEditorOptionsContainer);
    procedure ApplyEditorOptions;
    procedure ExecuteStatement(const SourceCode: string; WaitToFinish: Boolean = False);
    property ShowOutput : boolean read GetShowOutput write SetShowOutput;
    property CommandHistory : TStringList read fCommandHistory;
    property CommandHistoryPointer : integer read fCommandHistoryPointer write fCommandHistoryPointer;
    property CommandHistorySize : integer read fCommandHistorySize write SetCommandHistorySize;
  end;

var
  PythonIIForm: TPythonIIForm;

implementation

Uses
  System.Math,
  System.Win.Registry,
  System.RegularExpressions,
  System.Threading,
  VarPyth,
  JclStrings,
  JvJVCLUtils,
  JvAppIniStorage,
  JvGnugettext,
  SynEditKeyCmds,
  SynEditMiscProcs,
  StringResources,
  frmPyIDEMain,
  dmResources,
  dmCommands,
  frmUnitTests,
  uCommonFunctions,
  uCmdLine,
  cPyDebugger,
  cPyScripterSettings,
  cPyControl;

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

procedure TPythonIIForm.PythonIOReceiveData(Sender: TObject;
  var Data: string);
Var
  Res : Boolean;
begin
  with GetPythonEngine do begin
    Res := SyncWideInputQuery('PyScripter - Input requested', 'Input:', Data);
    if not Res then
      PyErr_SetString(PyExc_KeyboardInterrupt^, 'Operation cancelled')
    else
      Data := Data + #10;
  end;
end;

procedure TPythonIIForm.PythonIOSendData(Sender: TObject; const Data: string);
Var
  S : AnsiString;
  IsPending: Boolean;
begin
  if Data.Length = 0 then Exit;

  if fShowOutput then begin
    fCriticalSection.Enter;
    try
      IsPending := fOutputStream.Size > 0;

      fOutputStream.Write(Data[1], Length (Data) * 2);
      if Assigned(fOutputMirror) then begin
        S := Utf8Encode(Data);
        fOutputMirror.Write(S[1], Length(S));
      end;

      // If IsPending then a previous request to WritePendingMessages will be
      // executed. There is no need to send a new one.
      // The delay (100) is so that if more output comes soon enough,
      // it will be processed by the same request.
      if not IsPending then
        TThread.ForceQueue(nil, procedure
        begin
          WritePendingMessages;
        end, 100);
//      Queue a call to WritePendingMessages now if buffer becomes big enough
//      else if fOutputStream.Size > 1000 then
//        TThread.ForceQueue(nil, procedure
//        begin
//          WritePendingMessages;
//        end);

    finally
      fCriticalSection.Leave;
    end;
  end;
end;

procedure TPythonIIForm.actClearContentsExecute(Sender: TObject);
begin
  ClearDisplay;
end;

procedure TPythonIIForm.actCopyHistoryExecute(Sender: TObject);
begin
  Clipboard.AsText :=  fCommandHistory.Text;
end;

procedure TPythonIIForm.actCopyWithoutPromptsExecute(Sender: TObject);
Var
  SelText : string;
begin
  SelText := SynEdit.SelText;
  if SelText = '' then Exit;

  SelText := TRegEx.Replace(SelText,
     '^((\[(Dbg|PM)\])?(>>>\ |\.\.\.\ ))', '', [roNotEmpty, roMultiLine]);

  Clipboard.AsText := SelText;
end;

procedure TPythonIIForm.actPasteAndExecuteExecute(Sender: TObject);
Var
  Buffer: TArray<string>;
  Buffers: TArray<TArray<string>>;
  Text : string;
begin
  Text := Clipboard.AsText;
  if Text = '' then Exit;

  // Untabify
  Text :=  StringReplace(Text, #9,
     StringOfChar(' ', SynEdit.TabWidth), [rfReplaceAll]);
  // Dedent
  Text := Dedent(Text);

  var SL := TSmartPtr.Make(TStringList.Create)();
    SL.Text := Text;

  for var Line in SL do begin
    if Line = '' then Continue;

    if LeftSpaces(Line, False) = 0 then
    begin
      if Length(Buffer) > 0 then
        // Allready have a full statement we need to execute
        Buffers := Buffers + [Copy(Buffer)];
      Buffer := [];
    end;
    Buffer := Buffer + [Line];
  end;
  if Length(Buffer) > 0 then
    Buffers := Buffers + [Copy(Buffer)];

  TThread.ForceQueue(nil, procedure
    procedure ExecuteBuffer(Buffer : TArray<string>; WaitToFinish: Boolean);
    begin
      if Length(Buffer) > 1 then
        Buffer := Buffer + [''];
      AppendToPrompt(Buffer);
      var Code := String.Join(#10, Buffer);
      ExecuteStatement(Code, WaitToFinish);
    end;

  begin
    for var I := Low(Buffers) to High(Buffers) do
      // don't wait for the last statement
      ExecuteBuffer(Buffers[I], I < High(Buffers));
  end);
end;

procedure TPythonIIForm.PrintInterpreterBanner(AVersion: string = ''; APlatform: string = '');
var
  S: string;
begin
  var Py := GI_PyControl.SafePyEngine;
  if AVersion = '' then AVersion := SysModule.version;
  if APlatform = '' then APlatform := SysModule.platform;
  AVersion := AVersion.Replace(Char($A), ' ');
  S := Format('*** Python %s on %s. ***' + sLineBreak, [AVersion, APlatform]);
  if SynEdit.Lines.Count > 0 then AppendText(sLineBreak);
  AppendText(S);
  AppendText(PS1);
end;

procedure TPythonIIForm.AppendPrompt;
var
  Buffer: array of string;
begin
  WritePendingMessages;
  SetLength(Buffer, 0);
  AppendToPrompt(Buffer);
end;

procedure TPythonIIForm.ClearPendingMessages;
begin
  fCriticalSection.Enter;
  try
    fOutputStream.Clear;
  finally
    fCriticalSection.Leave;
  end;
end;

procedure TPythonIIForm.ClearDisplay;
begin
  Synedit.ClearAll;
  PrintInterpreterBanner;
end;

procedure TPythonIIForm.ClearLastPrompt;
begin
  if SynEdit.Lines[SynEdit.Lines.Count-1] = PS1 then
    SynEdit.Lines.Delete(SynEdit.Lines.Count -1);
end;

procedure TPythonIIForm.WritePendingMessages;
var
  WS: string;
begin
  Assert(GetCurrentThreadId = MainThreadId);
  fCriticalSection.Enter;
  try
    if fOutputStream.Size > 0 then begin
      SetLength(WS, fOutputStream.Size div 2);
      fOutputStream.Position := 0;
      fOutputStream.Read(WS[1], Length(WS) * 2);
      AppendText(WS);
      fOutputStream.Size := 0;
    end;
  finally
    fCriticalSection.Leave;
  end;
end;

procedure TPythonIIForm.AppendText(const S: string);
begin
  SynEdit.BeginUpdate;
  try
    SynEdit.ExecuteCommand(ecEditorBottom, ' ', nil);
    if Pos(#0, S) > 0 then
      SynEdit.SelText := StrStripChar(S, #0)
    else
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

procedure TPythonIIForm.ValidateEditorOptions(
  SynEditOptions: TSynEditorOptionsContainer);
begin
  with SynEditOptions do begin
    Options := Options - [eoTrimTrailingSpaces, eoScrollPastEol, eoShowLigatures];
    Gutter.Visible := False;
    IndentGuides.Visible := False;
    RightEdge := 0;
  end;
end;

procedure TPythonIIForm.ApplyEditorOptions;
begin
  var SynEditOptions := TSmartPtr.Make(TSynEditorOptionsContainer.Create(nil))();

  var OldWordWrap := SynEdit.WordWrap;
  SynEditOptions.Assign(EditorOptions);
  ValidateEditorOptions(SynEditOptions);
  SynEdit.Assign(SynEditOptions);
  SynEdit.WordWrap := OldWordWrap;
  RegisterHistoryCommands;

  SynEdit.Highlighter.Assign(ResourcesDataModule.SynPythonSyn);
end;

procedure TPythonIIForm.ApplyPyIDEOptions;
begin
  SynEdit.SelectedColor.Assign(PyIDEOptions.SelectionColor);

  // Command History Size
  CommandHistorySize := PyIDEOptions.InterpreterHistorySize;
end;

procedure TPythonIIForm.FormCreate(Sender: TObject);
begin
  ImageName := 'Python';
  inherited;
  SynEdit.OnReplaceText := CommandsDataModule.SynEditReplaceText;
  SynEdit.Highlighter := TSynPythonInterpreterSyn.Create(Self);
  SynEdit.Highlighter.Assign(ResourcesDataModule.SynPythonSyn);

  ApplyEditorOptions;

  // IO
  PythonIO.OnSendUniData := PythonIOSendData;
  PythonIO.OnReceiveUniData := PythonIOReceiveData;
  PythonIO.UnicodeIO := True;
  PythonIO.RawOutput := True;

  fShowOutput := True;
  // For handling output from Python threads
  FCriticalSection.Initialize;
  fOutputStream := TMemoryStream.Create;

  //  For recalling old commands in Interactive Window;
  fCommandHistory := TStringList.Create();
  fCommandHistorySize := 50;
  fCommandHistoryPointer := 0;

  SetPyInterpreterPrompt(pipNormal);

  // PyIDEOptions change notification
  PyIDEOptions.OnChange.AddHandler(ApplyPyIDEOptions);

  GI_PyInterpreter := Self;
end;

procedure TPythonIIForm.FormDestroy(Sender: TObject);
begin
  // PyIDEOptions change notification
  PyIDEOptions.OnChange.RemoveHandler(ApplyPyIDEOptions);

  GI_PyInterpreter := nil;
  FreeAndNil(fCommandHistory);
  FCriticalSection.Destroy;
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

function TPythonIIForm.GetPythonIO: TPythonInputOutput;
begin
  Result := PythonIO;
end;

function TPythonIIForm.GetSearchTarget: TSynEdit;
begin
  Result := SynEdit;
end;

function TPythonIIForm.GetShowOutput: boolean;
begin
  Result := fShowOutput;
end;

procedure TPythonIIForm.ExecuteStatement(const SourceCode: string; WaitToFinish: Boolean = False);
{ Execute a possibly incomplete python statement }
var
  NeedIndent : boolean;
  LineN: Integer;
begin
  NeedIndent := False;  // True denotes an incomplete statement
  LineN := SynEdit.CaretY - 1;  // Caret is 1 based
  SynEdit.ExecuteCommand(ecEditorBottom, ' ', nil);
  AppendText(sLineBreak);

  // Call RunSource
  GI_PyControl.ThreadPythonExec(
    procedure
    begin
      if GI_PyControl.PythonLoaded and not GI_PyControl.Running then
        case PyControl.DebuggerState of
          dsInactive :
            NeedIndent :=
              PyControl.ActiveInterpreter.RunSource(SourceCode, '<interactive input>');
          dsPaused, dsPostMortem :
            NeedIndent :=
              PyControl.ActiveDebugger.RunSource(SourceCode, '<interactive input>');
        end;
    end,
    procedure
    var
      Buffer : array of string;
      Index, Position: integer;
      CurLine, Indent: string;
    begin
      if not NeedIndent then begin
        // The source code has been executed
        WritePendingMessages;
        // If the last line isnt empty, append a newline
        SetLength(Buffer, 0);
        AppendToPrompt(Buffer);

        //  Add the command executed to History
        Index := fCommandHistory.IndexOf(SourceCode);
        if Index >= 0  then
          fCommandHistory.Delete(Index);
        FCommandHistory.Add(SourceCode);
        SetCommandHistorySize(fCommandHistorySize);
        fCommandHistoryPointer := fCommandHistory.Count;
        SynEdit.Refresh;
      end else begin
        // Now attempt to correct indentation
        CurLine := Copy(SynEdit.Lines[lineN], Length(PS2)+1); //!!
        Position := 1;
        Indent := '';
        while (Length(CurLine)>=Position) and
             CharInSet(CurLine[Position], [#09, #32]) do begin
          Indent := Indent + CurLine[Position];
          Inc(Position);
        end;

        if TPyRegExpr.IsBlockOpener(CurLine) then begin
          if eoTabsToSpaces in SynEdit.Options then
            Indent := Indent + StringOfChar(' ', SynEdit.TabWidth)
          else
            Indent := indent + #9;
        end else if TPyRegExpr.IsBlockCloser(CurLine) then begin
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
      SynEdit.EnsureCursorPosVisible;
    end, WaitToFinish);
end;

procedure TPythonIIForm.SynEditProcessCommand(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: WideChar; Data: Pointer);
Var
  LineN, StartLineN, EndLineN: integer;
  IsCode : Boolean;
  Line, Source : string;
  Buffer : array of string;
  NewCommand : TSynEditorCommand;
  WChar : WideChar;
  Match : TMatch;
begin
  if (Command <> ecLostFocus) and (Command <> ecGotFocus) then
    EditorSearchOptions.InitSearch;
  case Command of
    ecLineBreak :
      begin
        Command := ecNone;  // do not processed it further

        fCommandHistoryPrefix := '';

        CommandsDataModule.SynParamCompletion.CancelCompletion;
        CommandsDataModule.SynCodeCompletion.CancelCompletion;

        LineN := SynEdit.CaretY - 1;  // Caret is 1 based
        GetBlockBoundary(LineN, StartLineN, EndLineN, IsCode);
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
            //remove trailing whitespace
            Source := Source.TrimRight([#9, #32]);

            if not GI_PyControl.PythonLoaded or GI_PyControl.Running then
              // it is dangerous to execute code while running scripts
              // so just beep and do nothing
              MessageBeep(MB_ICONERROR)
            else begin
              Match := TRegEx.Match(Source, '^\s*!(.+)');
              if Match.Success and (EndLineN = StartLineN) then
                // System Command
              begin
                SynEdit.ExecuteCommand(ecEditorBottom, ' ', nil);
                AppendText(sLineBreak);
                GI_PyControl.ThreadPythonExec(
                  procedure
                  begin
                    PyControl.ActiveInterpreter.SystemCommand(GI_PyIDEServices.ReplaceParams(Match.Groups[1].Value));
                  end)
              end
              else
                ExecuteStatement(Source);
            end;
          end;
          SynEdit.EnsureCursorPosVisible;
        end;
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
    ecLostFocus:
      if not (CommandsDataModule.SynCodeCompletion.Form.Visible or SynEdit.Focused) then
        CommandsDataModule.SynParamCompletion.CancelCompletion;
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
  Caret, BC : TBufferCoord;
begin
  // Should AutoCompletion be trigerred?
  if (Command = ecChar) and  PyIDEOptions.InterpreterCodeCompletion then
  begin
    if (TIDECompletion.InterpreterCodeCompletion.CompletionInfo.Editor = nil)
      and (Pos(AChar, CommandsDataModule.SynCodeCompletion.TriggerChars) > 0)
    then
    begin
      Caret := SynEdit.CaretXY;
      TThread.ForceQueue(nil, procedure
        begin
          DoCodeCompletion(SynEdit, Caret);
        end, IfThen(AChar = '.', 200,
        CommandsDataModule.SynCodeCompletion.TimerInterval));
    end;
  end;

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
   RegEx : TRegEx;
   Match : TMatch;
   ErrLineNo : integer;
   FileName : string;
begin
  RegEx := CompiledRegEx(STracebackFilePosExpr);
  Match := RegEx.Match(Synedit.LineText);
  if Match.Success then begin
    ErrLineNo := StrToIntDef(Match.GroupValue(3), 0);
    FileName := Match.GroupValue(1);
    //FileName := GetLongFileName(ExpandFileName(RegExpr.Match[1]));
    if Assigned(PyControl.ActiveInterpreter) then
      FileName := PyControl.ActiveInterpreter.FromPythonFileName(FileName);
    GI_PyIDEServices.ShowFilePosition(FileName, ErrLineNo, 1);
  end else begin
    RegEx := CompiledRegEx(SWarningFilePosExpr);
    Match := RegEx.Match(Synedit.LineText);
    if Match.Success then begin
      ErrLineNo := StrToIntDef(Match.GroupValue(3), 0);
      FileName := Match.GroupValue(1);
      GI_PyIDEServices.ShowFilePosition(FileName, ErrLineNo, 1);
    end;
  end;
end;

procedure TPythonIIForm.SynEditEnter(Sender: TObject);
begin
  inherited;
  EditorSearchOptions.InterpreterIsSearchTarget := True;
  GI_SearchCmds := Self;
  // SynCodeCompletion
  CommandsDataModule.SynCodeCompletion.Editor := SynEdit;
  CommandsDataModule.SynCodeCompletion.OnExecute := SynCodeCompletionExecute;
  CommandsDataModule.SynCodeCompletion.OnAfterCodeCompletion := SynCodeCompletionAfterCodeCompletion;
  CommandsDataModule.SynCodeCompletion.OnClose := SynCodeCompletionClose;
  CommandsDataModule.SynCodeCompletion.OnCodeItemInfo := SynCodeCompletionCodeItemInfo;
  CommandsDataModule.SynCodeCompletion.Images := vilCodeImages;
  // SynParamCompletion
  CommandsDataModule.SynParamCompletion.Editor := SynEdit;
  CommandsDataModule.SynParamCompletion.OnExecute := SynParamCompletionExecute;
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
  if CommandsDataModule.SynParamCompletion.Form.Visible then
    CommandsDataModule.SynParamCompletion.CancelCompletion;
end;

procedure TPythonIIForm.SynEditMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  EditorMouseWheel(+1, Shift );
  Handled := True;
end;

procedure TPythonIIForm.SynEditMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  EditorMouseWheel(-1, Shift );
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
begin
  case Command of
    ecCodeCompletion :
      begin
        if CommandsDataModule.SynCodeCompletion.Form.Visible then
          CommandsDataModule.SynCodeCompletion.CancelCompletion;
        DoCodeCompletion(SynEdit, SynEdit.CaretXY);
        //SynCodeCompletion.ActivateCompletion;
      end;
    ecParamCompletion:
      begin
        if CommandsDataModule.SynParamCompletion.Form.Visible then
          CommandsDataModule.SynParamCompletion.CancelCompletion;
        CommandsDataModule.SynParamCompletion.ActivateCompletion;
      end;
    ecRecallCommandPrev,
    ecRecallCommandNext,
    ecRecallCommandEsc :
      begin
        if (Command = ecRecallCommandEsc) and CommandsDataModule.SynParamCompletion.Form.Visible then
          CommandsDataModule.SynParamCompletion.CancelCompletion
        else
        begin
          CommandsDataModule.SynParamCompletion.CancelCompletion;
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
              fCommandHistory[fCommandHistoryPointer].StartsWith(fCommandHistoryPrefix);

          if InRange(fCommandHistoryPointer, 0, fCommandHistory.Count-1) then
            Source := fCommandHistory[fCommandHistoryPointer]
          else begin
            if Command <> ecRecallCommandEsc then
              Beep();
            Source := fCommandHistoryPrefix;
            fCommandHistoryPrefix := '';
          end;

          SynEdit.BeginUpdate;
          SynEdit.LockDrawing;
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
            SynEdit.UnLockDrawing;
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

procedure TPythonIIForm.SetPyInterpreterPrompt(Pip: TPyInterpreterPropmpt);
const
  NormalPS1 = '>>> ';
  NormalPS2 = '... ';
  DebugPrefix = '[Dbg]';
  PMPrefix = '[PM]';
begin
  case Pip of
    pipNormal:
      begin
        PS1 := NormalPS1;
        PS2 := NormalPS2;
      end;
    pipDebug:
      begin
        PS1 := DebugPrefix + NormalPS1;
        PS2 := DebugPrefix + NormalPS2;
      end;
    pipPostMortem:
      begin
        PS1 := PMPrefix + NormalPS1;
        PS2 := PMPrefix + NormalPS2;
      end;
  end;
end;

procedure TPythonIIForm.SetShowOutput(const Value: boolean);
begin
  fShowOutput := Value;
end;

procedure TPythonIIForm.ShowWindow;
begin
  PyIDEMainForm.ShowIDEDockForm(Self);
end;

procedure TPythonIIForm.SynCodeCompletionAfterCodeCompletion(Sender: TObject;
  const Value: string; Shift: TShiftState; Index: Integer; EndToken: Char);
begin
  if EndToken = '(' then
    TThread.ForceQueue(nil, procedure
    begin
      CommandsDataModule.SynParamCompletion.ActivateCompletion;
    end);
end;

procedure TPythonIIForm.SynCodeCompletionClose(Sender: TObject);
begin
  PyIDEOptions.CodeCompletionListSize :=
    CommandsDataModule.SynCodeCompletion.NbLinesInWindow;
  //  Clean-up
  TIDECompletion.InterpreterCodeCompletion.CleanUp;
end;

procedure TPythonIIForm.DoCodeCompletion(Editor: TSynEdit; Caret: TBufferCoord);
var
  locline: string;
  Attr: TSynHighlighterAttributes;
  Highlighter: TSynCustomHighlighter;
  Prompt, DummyToken: string;
begin
  // No code completion while Python is running
  if not (GI_PyControl.PythonLoaded and not GI_PyControl.Running and
    PyIDEOptions.InterpreterCodeCompletion)
  then
    Exit;

  //Exit if cursor has moved
  if Caret <> Editor.CaretXY then Exit;

  Highlighter := SynEdit.Highlighter;
  Dec(Caret.Char);
  SynEdit.GetHighlighterAttriAtRowCol(Caret, DummyToken, Attr);
  locLine := SynEdit.LineText.PadRight(Caret.Char); // to deal with trim trailing spaces
  Inc(Caret.Char);

  Prompt := GetPromptPrefix(locLine);
  if Prompt <> '' then begin
    // Replace prompt with spaces
    for var I := 1 to Length(Prompt) do
      locline[I] := ' ';
    // Exit if it is a system command
    if TRegEx.IsMatch(locLine, '^\s*!') then
      Exit;
  end else
    Exit;  // This is not a code line

  var CC := TIDECompletion.InterpreterCodeCompletion;
  if not CC.Lock.TryEnter then Exit;
  try
    // Exit if busy
    if CC.CompletionInfo.Editor <> nil then Exit;
    CC.CleanUp;
    CC.CompletionInfo.Editor := SynEdit;
    CC.CompletionInfo.CaretXY := Caret;
  finally
    CC.Lock.Leave;
  end;

  TTask.Create(procedure
  var
    DisplayText, InsertText: string;
  begin
    var CC := TIDECompletion.InterpreterCodeCompletion;
    if not CC.Lock.TryEnter then Exit;
    try
      var Skipped := False;
      for var I := 0 to CC.SkipHandlers.Count -1 do
      begin
        var SkipHandler := CC.SkipHandlers[I] as TBaseCodeCompletionSkipHandler;
        Skipped := SkipHandler.SkipCodeCompletion(locline, '', Caret, Highlighter, Attr);
        if Skipped then Break;
      end;

      var Handled := False;
      if not Skipped then
      begin
        for var I := 0 to CC.CompletionHandlers.Count -1 do
        begin
          var CompletionHandler := CC.CompletionHandlers[I] as TBaseCodeCompletionHandler;
          CompletionHandler.Initialize;
          try
            Handled := CompletionHandler.HandleCodeCompletion(locline, '',
              Caret, Highlighter, Attr, InsertText, DisplayText);
          except
          end;
          if Handled then begin
            //CompletionHandler will be finalized in the Cleanup call
            CC.CompletionInfo.CompletionHandler := CompletionHandler;
            CC.CompletionInfo.InsertText := InsertText;
            CC.CompletionInfo.DisplayText := DisplayText;
            Break;
          end
          else
            CompletionHandler.Finalize;
        end;
      end;

      if not Skipped and Handled and (InsertText <> '') then
        TThread.Queue(nil, procedure
        begin
          CommandsDataModule.SynCodeCompletion.ActivateCompletion;
        end)
      else
        CC.CleanUp;
    finally
      CC.Lock.Leave;
    end;
  end).Start;
end;

procedure TPythonIIForm.SynCodeCompletionExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: string; var x, y: Integer;
  var CanExecute: Boolean);
begin
  var CC := TIDECompletion.InterpreterCodeCompletion;
  var CP := TSynCompletionProposal(Sender);

  CanExecute := False;
  if CC.Lock.TryEnter then
  try
    CanExecute := Application.Active and
      (GetParentForm(SynEdit).ActiveControl = SynEdit) and
      (CC.CompletionInfo.CaretXY = SynEdit.CaretXY);

    if CanExecute then
    begin
      CP.Font := PyIDEOptions.AutoCompletionFont;
      CP.ItemList.Text := CC.CompletionInfo.DisplayText;
      CP.InsertList.Text := CC.CompletionInfo.InsertText;
      CP.NbLinesInWindow := PyIDEOptions.CodeCompletionListSize;
      CP.CurrentString := CurrentInput;

      if CP.Form.AssignedList.Count = 0 then
      begin
        CanExecute := False;
        CC.CleanUp;
      end
      else
      if PyIDEOptions.CompleteWithOneEntry and (CP.Form.AssignedList.Count = 1) then
      begin
        // Auto-complete with one entry without showing the form
        CanExecute := False;
        CP.OnValidate(CP.Form, [], #0);
        CC.CleanUp;
      end;
    end else begin
      CP.ItemList.Clear;
      CP.InsertList.Clear;
      CC.CleanUp;
    end;
  finally
    CC.Lock.Leave;
  end;
end;

procedure TPythonIIForm.SynCodeCompletionCodeItemInfo(Sender: TObject;
  AIndex: Integer; var Info: string);
begin
  var CC := TIDECompletion.InterpreterCodeCompletion;
  if not CC.Lock.TryEnter then Exit;
  try
    if Assigned(CC.CompletionInfo.CompletionHandler) then
      Info := CC.CompletionInfo.CompletionHandler.GetInfo(
        (Sender as TSynCompletionProposal).InsertList[AIndex]);
  finally
    CC.Lock.Leave;
  end;
end;

procedure TPythonIIForm.SynParamCompletionExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: string; var x, y: Integer;
  var CanExecute: Boolean);
var
  StartX,
  ArgIndex : Integer;
  DisplayString, DocString : string;
  p : TPoint;
  ParamString : string;
begin
  CanExecute := False;
  if not GI_PyControl.PythonLoaded or GI_PyControl.Running or not PyIDEOptions.InterpreterCodeCompletion
  then
    Exit;

  CanExecute := TIDECompletion.InterpreterParamCompletion.HandleParamCompletion('',
    SynEdit, DisplayString, DocString, StartX) and Application.Active and
    (GetParentForm(SynEdit).ActiveControl = SynEdit);

  var CP := Sender as TSynCompletionProposal;
  if CanExecute then begin
    CP.FormatParams := not (DisplayString = '');
    if not CP.FormatParams then
      DisplayString :=  '\style{~B}' + _(SNoParameters) + '\style{~B}';

    if (DocString <> '') then
    begin
      DisplayString := DisplayString + sLineBreak;
      DocString := GetLineRange(DocString, 1, 20) // 20 lines max
    end;

    // Determine active argument
    ParamString := Copy(SynEdit.LineText, Succ(StartX),
      TSynCompletionProposal(Sender).Editor.CaretX - Succ(StartX));
    ParamString := ParamString + ' ';  // To deal with for instance '1,'
    ArgIndex := 0;
    GetParameter(ParamString);
    While ParamString <> '' do begin
      Inc(ArgIndex);
      GetParameter(ParamString);
    end;

    CP.Form.CurrentIndex := ArgIndex;
    CP.ItemList.Text := DisplayString + DocString;

    //  position the hint window at and just below the opening bracket
    p := SynEdit.ClientToScreen(SynEdit.RowColumnToPixels(
      SynEdit.BufferToDisplayPos(BufferCoord(Succ(StartX), SynEdit.CaretY))));
    Inc(p.y, SynEdit.LineHeight);
    x := p.X;
    y := p.Y;
  end else begin
    CP.ItemList.Clear;
    CP.InsertList.Clear;
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

procedure TPythonIIForm.StartOutputMirror(const AFileName: string;
  Append: Boolean);
Var
  Mode : integer;
begin
  fCriticalSection.Enter;
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
      begin
        var BOM := TEncoding.UTF8.GetPreamble;
        fOutputMirror.Write(BOM, Length(BOM));  // save in utf8 encoding
      end;
    except
      StyledMessageDlg(Format(_(SCouldNotOpenOutputFile), [AFileName]), mtWarning, [mbOK], 0);
    end;
  finally
    fCriticalSection.Leave;
  end;
end;

procedure TPythonIIForm.StopFileMirror;
begin
  fCriticalSection.Enter;
  try
    FreeAndNil(fOutputMirror);
  finally
    fCriticalSection.Leave;
  end;
end;

procedure TPythonIIForm.UpdateInterpreterActions;
begin
  actCopyWithoutPrompts.Enabled := SynEdit.SelAvail;
  actPasteAndExecute.Enabled := Clipboard.HasFormat(CF_UNICODETEXT);
end;

procedure TPythonIIForm.UpdatePythonKeywords;
Var
  Keywords, Builtins, BuiltInMod : Variant;
  i : integer;
begin
  with ResourcesDataModule do begin
    SynPythonSyn.Keywords.Clear;
    SynPythonSyn.Keywords.Sorted := False;
    Keywords := Import('keyword').kwlist;
    for i := 0 to Len(Keywords) - 1 do
      SynPythonSyn.Keywords.AddObject(Keywords.__getitem__(i), Pointer(Ord(tkKey)));
    // Avoid adding duplicates (None, True, False)
    SynPythonSyn.Keywords.Sorted := True;
    BuiltInMod := VarPyth.BuiltinModule;
    Builtins := BuiltinMod.dir(BuiltinMod);
    for i := 0 to Len(Builtins) - 1 do
      SynPythonSyn.Keywords.AddObject(Builtins.__getitem__(i), Pointer(Ord(tkNonKeyword)));
    // add pseudo keyword self
    SynPythonSyn.Keywords.AddObject('self', Pointer(Ord(tkNonKeyword)));

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
    if not ( (theDirection > 1) and (SynEdit.Font.Size <= 2) ) then begin
      SynEdit.Font.Size := SynEdit.Font.Size  - theDirection;
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

procedure TPythonIIForm.ReinitInterpreter;
begin
  if Assigned(PyControl.ActiveInterpreter) then
    TThread.ForceQueue(nil,
    procedure
    begin
      PyControl.ActiveInterpreter.ReInitialize;
    end, 500);
end;

procedure TPythonIIForm.RemovePrompt;
begin
  var LastLine := SynEdit.Lines.Count;
  if (LastLine > 0) and (SynEdit.Lines[LastLine-1] = PS1) then
  begin
    SynEdit.BeginUpdate;
    try
      SynEdit.BlockBegin := BufferCoord(1, LastLine);
      SynEdit.BlockEnd := BufferCoord(Length(PS1) + 1, LastLine);
      SynEdit.SelText := ''
    finally
      SynEdit.EndUpdate;
    end;
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

function TPythonIIForm.GetEditor: TCustomSynEdit;
begin
  Result := SynEdit;
end;

procedure TPythonIIForm.RegisterHistoryCommands;
// Register the Recall History Command

  procedure AddEditorCommand(Cmd: TSynEditorCommand; SC: TShortcut);
  begin
    // Remove if it exists
    var Index :=  SynEdit.Keystrokes.FindShortcut(SC);
    if Index >= 0 then
      SynEdit.Keystrokes.Delete(Index);
    // Addit
    with SynEdit.Keystrokes.Add do
    begin
      ShortCut := SC;
      Command := Cmd;
    end;
  end;

begin
  AddEditorCommand(ecRecallCommandPrev, Vcl.Menus.ShortCut(VK_UP, [ssAlt]));
  AddEditorCommand(ecRecallCommandNext, Vcl.Menus.ShortCut(VK_DOWN, [ssAlt]));
  AddEditorCommand(ecRecallCommandEsc, Vcl.Menus.ShortCut(VK_ESCAPE, []));
end;

procedure TPythonIIForm.StoreSettings(AppStorage: TJvCustomAppStorage);
begin
  var TempStringList := TSmartPtr.Make(TStringList.Create)();

  // Save Options
  var SynEditOptions := TSmartPtr.Make(TSynEditorOptionsContainer.Create(nil))();
  SynEditOptions.Assign(SynEdit);

  AppStorage.DeleteSubTree('Interpreter Editor Options');
  TempStringList.AddStrings(['TrackChanges', 'SelectedColor', 'IndentGuides', 'KeyStrokes']);
  AppStorage.WritePersistent('Interpreter Editor Options', SynEditOptions, True, TempStringList);

  //Save Highlighter
  AppStorage.WritePersistent('Highlighters\Intepreter', SynEdit.Highlighter);

  // Save Interpreter History
  TempStringList.Clear;
  for var I := 0 to CommandHistory.Count - 1 do
    TempStringList.Add(StrStringToEscaped(CommandHistory[i]));
  var AppIniStorageOptions := TJvAppIniStorageOptions(AppStorage.StorageOptions);
  var OldPreserveLeadingTrailingBlanks := AppIniStorageOptions.PreserveLeadingTrailingBlanks;
  AppIniStorageOptions.PreserveLeadingTrailingBlanks := True;
  AppStorage.WriteStringList('Command History', TempStringList);
  AppIniStorageOptions.PreserveLeadingTrailingBlanks := OldPreserveLeadingTrailingBlanks;
end;

procedure TPythonIIForm.RestoreSettings(AppStorage: TJvCustomAppStorage);
begin
  var TempStringList := TSmartPtr.Make(TStringList.Create)();

  // Restore Options
  if AppStorage.PathExists('Interpreter Editor Options') then begin
    var SynEditOptions := TSmartPtr.Make(TSynEditorOptionsContainer.Create(nil))();

    SynEditOptions.Assign(SynEdit);

    TempStringList.AddStrings(['TrackChanges', 'SelectedColor', 'IndentGuides', 'KeyStrokes']);
    AppStorage.ReadPersistent('Interpreter Editor Options', SynEditOptions, True, True, TempStringList);

    ValidateEditorOptions(SynEditOptions);
    SynEdit.Assign(SynEditOptions);
  end;

  // Restore Highlighter
  if AppStorage.PathExists('Highlighters\Intepreter') then
  begin
    SynEdit.Highlighter.BeginUpdate;
    try
      AppStorage.ReadPersistent('Highlighters\Intepreter', SynEdit.Highlighter);
    finally
      SynEdit.Highlighter.EndUpdate;
    end;
  end;

  // Restore Interpreter History
  TempStringList.Clear;
  var AppIniStorageOptions := TJvAppIniStorageOptions(AppStorage.StorageOptions);
  var OldPreserveLeadingTrailingBlanks := AppIniStorageOptions.PreserveLeadingTrailingBlanks;
  AppIniStorageOptions.PreserveLeadingTrailingBlanks := True;
  AppStorage.ReadStringList('Command History', TempStringList);
  AppIniStorageOptions.PreserveLeadingTrailingBlanks := OldPreserveLeadingTrailingBlanks;

  CommandHistory.Clear;
  for var I := 0 to TempStringList.Count - 1 do
    CommandHistory.Add(StrEscapedToString(TempStringList[i]));
  CommandHistoryPointer := TempStringList.Count;  // one after the last one
end;

end.
