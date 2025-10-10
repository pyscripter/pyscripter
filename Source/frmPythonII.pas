{-----------------------------------------------------------------------------
 Unit Name: frmPythonII
 Author:    Kiriakos Vlahos
 Date:      20-Jan-2005
 Purpose:   Python Interactive Interperter using Python for Delphi and Synedit
 Features:  Syntax Highlighting
            Brace Highlighting
            Command History
                    - Alt-UP: previous command
                    - Alt-Down: next command
                    - Esc: clear command
            Code Completion
            Call Tips

 History:
-----------------------------------------------------------------------------}
unit frmPythonII;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.UITypes,
  System.Classes,
  System.Actions,
  System.ImageList,
  System.Messaging,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Menus,
  Vcl.ActnList,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  JvComponentBase,
  JvDockControlForm,
  JvAppStorage,
  dlgSynEditOptions,
  TB2Item,
  SpTBXItem,
  SpTBXSkins,
  SynEdit,
  SynEditTypes,
  SynCompletionProposal,
  PythonEngine,
  frmIDEDockWin,
  uEditAppIntfs,
  cPySupportTypes;

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
      Sender: TObject; var CurrentInput: string; var X, Y: Integer;
      var CanExecute: Boolean);
    function FormHelp(Command: Word; Data: THelpEventData;
      var CallHelp: Boolean): Boolean;
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SynParamCompletionExecute(Kind: SynCompletionType;
      Sender: TObject; var CurrentInput: string; var X, Y: Integer;
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
    procedure SynCodeCompletionAfterCodeCompletion(Sender: TObject;
      const Value: string; Shift: TShiftState; Index: Integer; EndToken: Char);
  private
    { Private declarations }
    FCommandHistory: TStringList;
    FCommandHistorySize: Integer;
    FCommandHistoryPointer: Integer;
    FCommandHistoryPrefix: string;
    FShowOutput: Boolean;
    FCriticalSection: TRTLCriticalSection;
    FOutputStream: TMemoryStream;
    FOutputMirror: TFileStream;
    procedure GetBlockBoundary(LineN: Integer; var StartLineN,
              EndLineN: Integer; var IsCode: Boolean);
    function GetPromptPrefix(Line: string): string;
    procedure SetCommandHistorySize(const Value: Integer);
    procedure GetBlockCode(var Source: string;
      var Buffer: array of string; EndLineN: Integer; StartLineN: Integer);
    procedure DoCodeCompletion(Editor: TSynEdit; Caret: TBufferCoord);
    procedure ApplyPyIDEOptions(const Sender: TObject; const Msg:
        System.Messaging.TMessage);
    // ISearchCommands implementation
    function CanFind: Boolean;
    function CanFindNext: Boolean;
    function ISearchCommands.CanFindPrev = CanFindNext;
    function CanReplace: Boolean;
    function GetSearchTarget: TSynEdit;
    procedure ExecFind;
    procedure ExecFindNext;
    procedure ExecFindPrev;
    procedure ExecReplace;
    procedure SynCodeCompletionCodeItemInfo(Sender: TObject;
      AIndex: Integer; var Info: string);
    // Implementation of IPyInterpreter
    procedure ShowWindow(Activate: Boolean = True);
    procedure AppendPrompt;
    procedure RemovePrompt;
    procedure AppendText(const Str: string);
    procedure PrintEngineType;
    procedure PrintInterpreterBanner(AVersion: string = ''; APlatform: string = '');
    procedure WritePendingMessages;
    procedure ClearPendingMessages;
    procedure ClearDisplay;
    procedure ClearLastPrompt;
    function OutputSuppressor: IInterface;
    procedure StartOutputMirror(const AFileName: string; Append: Boolean);
    procedure StopFileMirror;
    procedure UpdatePythonKeywords;
    procedure SetPyInterpreterPrompt(Pip: TPyInterpreterPropmpt);
    procedure ReinitInterpreter;
    function GetEditor: TCustomSynEdit;
    function GetPythonIO: TPythonInputOutput;
    function GetShowOutput: Boolean;
    procedure SetShowOutput(const Value: Boolean);
  protected
    procedure PythonIOReceiveData(Sender: TObject; var Data: string);
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
    procedure WMREINITINTERPRETER(var Message: TMessage); message WM_REINITINTERPRETER;
  public
    { Public declarations }
    PS1, PS2: string;
    // AppStorage
    procedure StoreSettings(AppStorage: TJvCustomAppStorage); override;
    procedure RestoreSettings(AppStorage: TJvCustomAppStorage); override;

    procedure PythonIOSendData(Sender: TObject; const Data: string);
    procedure AppendToPrompt(const Buffer: array of string);
    function IsEmpty: Boolean;
    procedure UpdateInterpreterActions;
    procedure RegisterHistoryCommands;
    procedure ValidateEditorOptions(SynEditOptions: TSynEditorOptionsContainer);
    procedure ApplyEditorOptions;
    procedure ExecuteStatement(const SourceCode: string; WaitToFinish: Boolean = False);
    property ShowOutput: Boolean read GetShowOutput write SetShowOutput;
    property CommandHistory: TStringList read FCommandHistory;
    property CommandHistoryPointer: Integer read FCommandHistoryPointer write FCommandHistoryPointer;
    property CommandHistorySize: Integer read FCommandHistorySize write SetCommandHistorySize;
  end;

var
  PythonIIForm: TPythonIIForm;

implementation

uses
  System.Types,
  System.SysUtils,
  System.Contnrs,
  System.Math,
  System.RegularExpressions,
  System.Threading,
  System.SyncObjs,
  Vcl.Dialogs ,
  Vcl.Clipbrd,
  VarPyth,
  JclStrings,
  JvAppIniStorage,
  JvGnugettext,
  SynHighlighterPython,
  SynEditHighlighter,
  SynEditKeyCmds,
  SynEditMiscProcs,
  SynUnicode,
  StringResources,
  frmPyIDEMain,
  dmResources,
  dmCommands,
  uCommonFunctions,
  cPyScripterSettings,
  cPyControl,
  cCodeCompletion;

{$R *.dfm}

{ Class TSuppressOuptput modelled after JVCL.WaitCursor}
type
  TSuppressOutput = class(TInterfacedObject, IInterface)
  private
    FPythonIIForm: TPythonIIForm;
    FOldShowOutput: Boolean;
  public
    constructor Create(PythonIIForm: TPythonIIForm);
    destructor Destroy; override;
  end;

constructor TSuppressOutput.Create(PythonIIForm: TPythonIIForm);
begin
  inherited Create;
  FPythonIIForm := PythonIIForm;
  if Assigned(FPythonIIForm) then begin
    FOldShowOutput := PythonIIForm.ShowOutput;
    PythonIIForm.ShowOutput := False;
  end;
end;

destructor TSuppressOutput.Destroy;
begin
  if Assigned(FPythonIIForm) then
    FPythonIIForm.ShowOutput := FOldShowOutput;
  inherited Destroy;
end;

{ PythonIIForm }

function TPythonIIForm.OutputSuppressor: IInterface;
begin
  Result := TSuppressOutput.Create(Self);
end;

procedure TPythonIIForm.PythonIOReceiveData(Sender: TObject;
  var Data: string);
var
  Res: Boolean;
begin
  with GetPythonEngine do begin
    Res := SyncWideInputQuery(_('PyScripter - Input requested'), _('Input:'), Data);
    if not Res then
      PyErr_SetString(PyExc_KeyboardInterrupt^, PAnsiChar(EncodeString(_('Operation cancelled'))))
    else
      Data := Data + #10;
  end;
end;

procedure TPythonIIForm.PythonIOSendData(Sender: TObject; const Data: string);
begin
  if Data.Length = 0 then Exit;

  if FShowOutput then begin
    FCriticalSection.Enter;
    try
      var IsPending := FOutputStream.Size > 0;

      FOutputStream.Write(Data[1], Length (Data) * 2);
      if Assigned(FOutputMirror) then begin
        var MirrorData := UTF8Encode(Data);
        FOutputMirror.Write(MirrorData[1], Length(MirrorData));
      end;

      // If IsPending then a previous request to WritePendingMessages will be
      // executed. There is no need to send a new one.
      // The delay (100) is so that if more output comes soon enough,
      // it will be processed by the same request.
      if not IsPending then
        TTask.Create(procedure
        begin
          Sleep(100);
          TThread.ForceQueue(nil, procedure
          begin
            WritePendingMessages;
          end);
          WakeMainThread(nil);
        end).Start;
    finally
      FCriticalSection.Leave;
    end;
  end;
end;

procedure TPythonIIForm.actClearContentsExecute(Sender: TObject);
begin
  ClearDisplay;
end;

procedure TPythonIIForm.actCopyHistoryExecute(Sender: TObject);
begin
  Clipboard.AsText :=  FCommandHistory.Text;
end;

procedure TPythonIIForm.actCopyWithoutPromptsExecute(Sender: TObject);
begin
  var SelText := SynEdit.SelText;
  if SelText = '' then Exit;

  SelText := TRegEx.Replace(SelText,
     '^((\[(Dbg|PM)\])?(>>>\ |\.\.\.\ ))', '', [roNotEmpty, roMultiLine]);

  Clipboard.AsText := SelText;
end;

procedure TPythonIIForm.actPasteAndExecuteExecute(Sender: TObject);
var
  Buffer: TArray<string>;
  Buffers: TArray<TArray<string>>;
  Text: string;
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
    procedure ExecuteBuffer(Buffer: TArray<string>; WaitToFinish: Boolean);
    begin
      if Length(Buffer) > 1 then
        Buffer := Buffer + [''];
      AppendToPrompt(Buffer);
      var Code := string.Join(#10, Buffer);
      ExecuteStatement(Code, WaitToFinish);
    end;

  begin
    for var I := Low(Buffers) to High(Buffers) do
      // don't wait for the last statement
      ExecuteBuffer(Buffers[I], I < High(Buffers));
  end);
end;

procedure TPythonIIForm.PrintEngineType;
var
  Msg: string;
begin
  case PyIDEOptions.PythonEngineType of
    peInternal:  Msg := Format(_(SEngineActive), [_('Internal')]);
    peRemote: Msg := Format(_(SEngineActive), [_('Remote')]);
    peRemoteTk: Msg := Format(_(SEngineActive), [_('Remote (Tk)')]);
    peRemoteWx: Msg := Format(_(SEngineActive), ['Remote (Wx)']);
    peSSH: Msg := Format(_(SEngineActive),
      [Format('"%s" SSH', [GI_PyControl.ActiveSSHServerName])]);
  end;

  GI_PyInterpreter.AppendText(sLineBreak + Msg);
  GI_PyInterpreter.AppendPrompt;
end;

procedure TPythonIIForm.PrintInterpreterBanner(AVersion: string = ''; APlatform: string = '');
begin
  if AVersion = '' then AVersion := PyControl.ActiveInterpreter.PythonVersion;
  if APlatform = '' then APlatform := PyControl.ActiveInterpreter.PythonPlatform;
  AVersion := AVersion.Replace(Char($A), ' ');
  if SynEdit.Lines.Count > 0 then AppendText(sLineBreak);
  var Str := Format('*** Python %s on %s. ***', [AVersion, APlatform]);
  AppendText(Str);
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
  FCriticalSection.Enter;
  try
    FOutputStream.Clear;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TPythonIIForm.ClearDisplay;
begin
  SynEdit.ClearAll;
  PrintInterpreterBanner;
  AppendPrompt;
end;

procedure TPythonIIForm.ClearLastPrompt;
begin
  if SynEdit.Lines[SynEdit.Lines.Count-1] = PS1 then
    SynEdit.Lines.Delete(SynEdit.Lines.Count -1);
end;

procedure TPythonIIForm.WritePendingMessages;
var
  WS: string;

  procedure ProcessOutput;
  // Handle single CR used by python modules such as tdqm
  var
    P, PStart: PChar;
    Line: string;
    Overwrite: Integer;

    procedure AddLine(AddLB: Boolean);
    var
      Str: string;
    begin
      if P < PStart then Exit;
      if SynEdit.Lines.Count = 0 then
        SynEdit.Lines.Add('');

      SetString(Str, PStart, P - PStart);
      Line := SynEdit.Lines[SynEdit.Lines.Count - 1];
      if Overwrite = 0 then
        Line := Line + Str
      else
      begin
        Delete(Line, Overwrite, Str.Length);
        Insert(Str, Line, Overwrite);
        Inc(Overwrite, Str.Length);
      end;
      SynEdit.Lines[SynEdit.Lines.Count - 1] := Line;

      if AddLB then
        SynEdit.Lines.Add('');

      PStart := P + 1;
    end;

  begin
    Overwrite := 0;
    SynEdit.BeginUpdate;
    try
      if SynEdit.Lines.Count = 0 then
        SynEdit.Lines.Add('');

      PStart := PChar(WS);
      P := PStart;

      while True do
      begin
        case P^ of
          WideNull:
            begin
              AddLine(False);
              Break;
            end;
          WideCR:
            begin
              if (P + 1)^ = WideLF then
              begin
                AddLine(True);
                Overwrite := 0;
                Inc(PStart);
                Inc(P);
              end
              else
              begin
                AddLine(False);
                Overwrite := 1;
              end;
            end;
          WideLF:
          begin
            AddLine(True);
            Overwrite := 0;
          end;
        end;
        Inc(P);
      end;
      SynEdit.ExecuteCommand(ecEditorBottom, ' ', nil);
    finally
      SynEdit.EndUpdate;
    end;
  end;

begin
  Assert(GetCurrentThreadId = MainThreadID, 'WritePendingMessages');
  FCriticalSection.Enter;
  try
    if FOutputStream.Size > 0 then begin
      SetLength(WS, FOutputStream.Size div 2);
      FOutputStream.Position := 0;
      FOutputStream.Read(WS[1], Length(WS) * 2);
      FOutputStream.Size := 0;
      ProcessOutput;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TPythonIIForm.AppendText(const Str: string);
begin
  SynEdit.BeginUpdate;
  try
    SynEdit.ExecuteCommand(ecEditorBottom, ' ', nil);
    if Pos(#0, Str) > 0 then
      SynEdit.SelText := StrStripChar(Str, #0)
    else
      SynEdit.SelText := Str;
    SynEdit.ExecuteCommand(ecEditorBottom, ' ', nil);
    SynEdit.EnsureCursorPosVisible;
  finally
    SynEdit.EndUpdate;
  end;
end;

procedure TPythonIIForm.AppendToPrompt(const Buffer: array of string);
var
  LineCount: Integer;
  Line: string;
begin
  LineCount := SynEdit.Lines.Count;
  Line := SynEdit.Lines[LineCount-1];
  SynEdit.BeginUpdate;
  try
    if Line <> PS1 then begin
      if Line <> '' then AppendText(sLineBreak);
      AppendText(PS1);
    end;
    for var I := Low(Buffer) to High(Buffer) - 1  do
        AppendText(Buffer[I] + sLineBreak + PS2);
    if Length(Buffer) > 0 then AppendText(Buffer[High(Buffer)]);
  finally
    SynEdit.EndUpdate;
  end;
end;

procedure TPythonIIForm.ValidateEditorOptions(
  SynEditOptions: TSynEditorOptionsContainer);
begin
  with SynEditOptions do begin
    Options := Options - [eoTrimTrailingSpaces, eoShowLigatures];
    ScrollOptions := ScrollOptions - [eoScrollPastEol];
    Gutter.Visible := False;
    IndentGuides.Visible := False;
    RightEdge := 0;
    if PyIDEOptions.AutoCompleteBrackets then
      Options := Options + [eoCompleteBrackets, eoCompleteQuotes]
    else
      Options := Options - [eoCompleteBrackets, eoCompleteQuotes];
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

  SynEdit.Highlighter.Assign(ResourcesDataModule.SynPythonSyn);
  RegisterHistoryCommands;
end;

procedure TPythonIIForm.ApplyPyIDEOptions(const Sender: TObject;
  const Msg: System.Messaging.TMessage);
begin
  SynEdit.SelectedColor.Assign(PyIDEOptions.SelectionColor);
  if PyIDEOptions.AutoCompleteBrackets then
    SynEdit.Options := SynEdit.Options + [eoCompleteBrackets, eoCompleteQuotes]
  else
    SynEdit.Options := SynEdit.Options - [eoCompleteBrackets, eoCompleteQuotes];
  if PyIDEOptions.AccessibilitySupport then
    SynEdit.Options := SynEdit.Options + [eoAccessibility]
  else
    SynEdit.Options := SynEdit.Options - [eoAccessibility];

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
  SynEdit.ScrollbarAnnotations.Clear;

  ApplyEditorOptions;

  // IO
  PythonIO.OnSendUniData := PythonIOSendData;
  PythonIO.OnReceiveUniData := PythonIOReceiveData;
  PythonIO.UnicodeIO := True;
  PythonIO.RawOutput := True;

  FShowOutput := True;
  // For handling output from Python threads
  FCriticalSection.Initialize;
  FOutputStream := TMemoryStream.Create;

  //  For recalling old commands in Interactive Window;
  FCommandHistory := TStringList.Create;
  FCommandHistorySize := 50;
  FCommandHistoryPointer := 0;

  SetPyInterpreterPrompt(pipNormal);

  // PyIDEOptions change notification
  TMessageManager.DefaultManager.SubscribeToMessage(TIDEOptionsChangedMessage,
    ApplyPyIDEOptions);

  GI_PyInterpreter := Self;
end;

procedure TPythonIIForm.FormDestroy(Sender: TObject);
begin
  // PyIDEOptions change notification
  TMessageManager.DefaultManager.Unsubscribe(TIDEOptionsChangedMessage,
    ApplyPyIDEOptions);

  GI_PyInterpreter := nil;
  FreeAndNil(FCommandHistory);
  FCriticalSection.Destroy;
  FreeAndNil(FOutputStream);
  FreeAndNil(FOutputMirror);
  inherited;
end;

procedure TPythonIIForm.GetBlockBoundary(LineN: Integer; var StartLineN,
  EndLineN: Integer; var IsCode: Boolean);
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
var
  Line, Prefix: string;
  MaxLineNo: Integer;
begin
  Line := SynEdit.Lines[LineN];
  MaxLineNo := SynEdit.Lines.Count - 1;
  Prefix := GetPromptPrefix(Line);
  if Prefix = '' then begin
    IsCode := False;
    StartLineN := LineN;
    while StartLineN > 0 do begin
      if GetPromptPrefix(SynEdit.Lines[StartLineN-1]) <> '' then Break;
      Dec(StartLineN);
    end;
    EndLineN := LineN;
    while EndLineN < MaxLineNo do begin
      if GetPromptPrefix(SynEdit.Lines[EndLineN+1]) <> '' then Break;
      Inc(EndLineN);
    end;
  end else begin
    IsCode := True;
    StartLineN := LineN;
    while (StartLineN > 0) and (Prefix <> PS1) do begin
      Prefix := GetPromptPrefix(SynEdit.Lines[StartLineN-1]);
      if Prefix = '' then Break;
      Dec(StartLineN);
    end;
    EndLineN := LineN;
    while EndLineN < MaxLineNo do begin
      Prefix := GetPromptPrefix(SynEdit.Lines[EndLineN+1]);
      if (Prefix = PS1) or (Prefix = '') then Break;
      Inc(EndLineN);
    end;
  end;
end;

function TPythonIIForm.GetPromptPrefix(Line: string): string;
begin
  if Copy(Line, 1, Length(PS1)) = PS1 then
    Result := PS1
  else if Copy(Line, 1, Length(PS2)) = PS2 then
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

function TPythonIIForm.GetShowOutput: Boolean;
begin
  Result := FShowOutput;
end;

procedure TPythonIIForm.ExecuteStatement(const SourceCode: string; WaitToFinish: Boolean = False);
{ Execute a possibly incomplete python statement }
var
  NeedIndent: Boolean;
  LineN: Integer;
begin
  NeedIndent := False;  // True denotes an incomplete statement
  LineN := SynEdit.CaretY - 1;  // Caret is 1 based
  SynEdit.ExecuteCommand(ecEditorBottom, ' ', nil);
  AppendText(sLineBreak);

  // Call RunSource
  ThreadPythonExec(
    procedure
    begin
      if GI_PyControl.PythonLoaded and not GI_PyControl.Running then
        case GI_PyControl.DebuggerState of
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
      Buffer: array of string;
      Index, Position: Integer;
      CurLine, Indent: string;
    begin
      if not NeedIndent then begin
        // The source code has been executed
        WritePendingMessages;
        // If the last line isnt empty, append a newline
        SetLength(Buffer, 0);
        AppendToPrompt(Buffer);

        //  Add the command executed to History
        Index := FCommandHistory.IndexOf(SourceCode);
        if Index >= 0  then
          FCommandHistory.Delete(Index);
        FCommandHistory.Add(SourceCode);
        SetCommandHistorySize(FCommandHistorySize);
        FCommandHistoryPointer := FCommandHistory.Count;
        SynEdit.Refresh;
      end else begin
        // Now attempt to correct indentation
        CurLine := Copy(SynEdit.Lines[LineN], Length(PS2)+1); //!!
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
            Indent := Indent + #9;
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
var
  LineN, StartLineN, EndLineN: Integer;
  IsCode: Boolean;
  Line, Source: string;
  Buffer: array of string;
  NewCommand: TSynEditorCommand;
  WChar: WideChar;
  Match: TMatch;
begin
  if (Command <> ecLostFocus) and (Command <> ecGotFocus) then
    EditorSearchOptions.InitSearch;

  if SynEdit.Selections.Count > 1 then
    SynEdit.Selections.Clear;

  case Command of
    ecLineBreak :
      begin
        Command := ecNone;  // do not processed it further

        SynEdit.Selections.Clear;

        FCommandHistoryPrefix := '';

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
                ThreadPythonExec(
                  procedure
                  begin
                    PyControl.ActiveInterpreter.SystemCommand(GI_PyIDEServices.ReplaceParams(Match.Groups[1].Value));
                  end);
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
        SynEdit.Selections.Clear;
        Line := SynEdit.Lines[SynEdit.CaretY - 1];
        if ((Pos(PS1, Line) = 1) and (SynEdit.CaretX <= Length(PS1)+1)) or
           ((Pos(PS2, Line) = 1) and (SynEdit.CaretX <= Length(PS2)+1)) then
          Command := ecNone;  // do not processed it further
      end;
    ecLineStart, ecSelLineStart:
      begin
        SynEdit.Selections.Clear;
        var DC := SynEdit.DisplayXY;
        Line := SynEdit.Rows[DC.Row];
        if Pos(PS1, Line) = 1 then
          DC.Column := Length(PS1) + 1
        else if Pos(PS2, Line) = 1 then
          DC.Column := Length(PS2) + 1
        else
          Exit;
        SynEdit.MoveDisplayPosAndSelection(DC, Command = ecSelLineStart);
        Command := ecNone;  // do not processed it further
      end;
    ecChar, ecDeleteChar, ecDeleteWord, ecCut, ecPaste:
      begin
        SynEdit.Selections.Clear;
        Line := SynEdit.Lines[SynEdit.CaretY - 1];
        if ((Pos(PS1, Line) = 1) and (SynEdit.CaretX <= Length(PS1))) or
             ((Pos(PS2, Line) = 1) and (SynEdit.CaretX <= Length(PS2)))
        then
          Command := ecNone;  // do not processed it further
      end;
    ecUp, ecDown :
      begin
        SynEdit.Selections.Clear;
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
    ecLostFocus:
      if not (CommandsDataModule.SynCodeCompletion.Form.Visible or SynEdit.Focused) then
        CommandsDataModule.SynParamCompletion.CancelCompletion;
  end;
end;

procedure TPythonIIForm.SynEditCommandProcessed(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: WideChar; Data: Pointer);
var
  Caret: TBufferCoord;
begin
  // Should AutoCompletion be trigerred?
  if (Command = ecChar) and  PyIDEOptions.InterpreterCodeCompletion and
    (SynEdit.Selections.Count = 1)
  then
  begin
    if (TIDECompletion.CompletionInfo.Editor = nil)
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
end;

procedure TPythonIIForm.SynEditDblClick(Sender: TObject);
var
   RegExTraceback, RegExWarning : TRegEx;
   Match : TMatch;
   ErrLineNo, LineNo : integer;
   FileName : string;
   Token: string;
   Attr: TSynHighlighterAttributes;
begin
  RegExWarning := CompiledRegEx(SWarningFilePosExpr);
  Match := RegExWarning.Match(SynEdit.LineText);
  // Also try the previous line
  if not Match.Success and (SynEdit.CaretY > 1) then
    Match := RegExWarning.Match(SynEdit.Lines[SynEdit.CaretY - 2]);

  SynEdit.GetHighlighterAttriAtRowCol(BufferCoord(1, SynEdit.CaretY), Token, Attr);

  if not Match.Success and
    (Attr = TSynPythonInterpreterSyn(SynEdit.Highlighter).TracebackAttri)
  then
  begin
    RegExTraceback := CompiledRegEx(STracebackFilePosExpr);
    LineNo:= Synedit.CaretY;
    if Synedit.LineText.StartsWith('Traceback') then
      Inc(LineNo);

    while Attr = TSynPythonInterpreterSyn(SynEdit.Highlighter).TracebackAttri do
    begin
      Match := RegExTraceback.Match(Synedit.Lines[LineNo - 1]);
      if Match.Success then
        Break
      else
      begin
        Dec(LineNo);
        if LineNo < 1 then  // just in case
          Break;

        SynEdit.GetHighlighterAttriAtRowCol(BufferCoord(1, LineNo), Token, Attr);
      end;
    end;
  end;

  if Match.Success then
  begin
    SynEdit.CaretXY := SynEdit.CaretXY; // remove selection
    ErrLineNo := StrToIntDef(Match.GroupValue(2), 0);
    FileName := Match.GroupValue(1);
    if Assigned(PyControl.ActiveInterpreter) then
      FileName := PyControl.ActiveInterpreter.FromPythonFileName(FileName);
    GI_PyIDEServices.ShowFilePosition(FileName, ErrLineNo, 1);
  end;
end;

procedure TPythonIIForm.SynEditEnter(Sender: TObject);
begin
  EditorSearchOptions.InitSearch;
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
  GI_SearchCmds := nil;
end;

procedure TPythonIIForm.SynEditMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  EditorSearchOptions.InitSearch;
  if CommandsDataModule.SynParamCompletion.Form.Visible then
    CommandsDataModule.SynParamCompletion.CancelCompletion;
end;

procedure TPythonIIForm.SynEditProcessUserCommand(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: WideChar; Data: Pointer);
var
  LineN, StartLineN, EndLineN, I: Integer;
  IsCode: Boolean;
  Source, BlockSource: string;
  Buffer: array of string;
  P1, P2: PWideChar;
begin
  if SynEdit.Selections.Count > 1 then
    SynEdit.Selections.Clear;

  case Command of
    ecCodeCompletion :
      begin
        if CommandsDataModule.SynCodeCompletion.Form.Visible then
          CommandsDataModule.SynCodeCompletion.CancelCompletion;
        DoCodeCompletion(SynEdit, SynEdit.CaretXY);
      end;
    ecParamCompletion:
      begin
        if CommandsDataModule.SynParamCompletion.Form.Visible then
          CommandsDataModule.SynParamCompletion.CancelCompletion;
        CommandsDataModule.SynParamCompletion.ActivateCompletion;
      end;
    ecRecallCommandPrev,
    ecRecallCommandNext,
    ecRecallCommandEsc:
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
          if FCommandHistoryPrefix <> '' then
          begin
            if not (IsCode and (EndLineN = SynEdit.Lines.Count - 1) and
                    (SynEdit.CaretX = Length(SynEdit.Lines[SynEdit.Lines.Count - 1])+1) and
                    InRange(FCommandHistoryPointer, 0, FCommandHistory.Count-1) and
                    (BlockSource =  FCommandHistory[FCommandHistoryPointer])) then
              FCommandHistoryPrefix := '';
          end else
          begin
            if IsCode and (EndLineN = SynEdit.Lines.Count - 1) and
                    (SynEdit.CaretX = Length(SynEdit.Lines[SynEdit.Lines.Count - 1])+1) and
                    not (InRange(FCommandHistoryPointer, 0, FCommandHistory.Count-1) and
                    (BlockSource =  FCommandHistory[FCommandHistoryPointer]))
            then
              FCommandHistoryPrefix := BlockSource;
          end;

          Source := '';
          if Command = ecRecallCommandEsc then begin
           FCommandHistoryPointer := FCommandHistory.Count;
           FCommandHistoryPrefix := '';
          end else
            repeat
              if Command = ecRecallCommandPrev then
                Dec(FCommandHistoryPointer)
              else if Command = ecRecallCommandNext then
                Inc(FCommandHistoryPointer);
              FCommandHistoryPointer := EnsureRange(FCommandHistoryPointer, -1, FCommandHistory.Count);
            until not InRange(FCommandHistoryPointer, 0, FCommandHistory.Count-1) or
              (FCommandHistoryPrefix = '') or
              FCommandHistory[FCommandHistoryPointer].StartsWith(FCommandHistoryPrefix);

          if InRange(FCommandHistoryPointer, 0, FCommandHistory.Count-1) then
            Source := FCommandHistory[FCommandHistoryPointer]
          else
          begin
            if Command <> ecRecallCommandEsc then
              Beep;
            Source := FCommandHistoryPrefix;
            FCommandHistoryPrefix := '';
          end;

          SynEdit.BeginUpdate;
          SynEdit.LockDrawing;
          try
            if IsCode and (EndLineN = SynEdit.Lines.Count - 1) then begin
              // already at the bottom and inside the prompt
             if (BlockSource <> Source) then
             begin
                for I := EndLineN downto StartLineN do
                  SynEdit.Lines.Delete(I);
                //Append new prompt if needed
                SetLength(Buffer, 0);
                AppendToPrompt(Buffer);
             end;  // else do nothing
            end
            else
            begin
              SetLength(Buffer, 0);
              AppendToPrompt(Buffer);
            end;

            if (Source <> '') and
              ((BlockSource <> Source) or (EndLineN < SynEdit.Lines.Count - 1)) then
            begin
              I := 0;
              P1 := PWideChar(Source);
              while P1 <> nil do
              begin
                P1 := StrScan(P1, WideLF);
                if Assigned(P1) then Inc(P1);
                Inc(I);
              end;
              SetLength(Buffer, I);

              I := 0;
              P1 := PWideChar(Source);
              while P1 <> nil do
              begin
                P2 := StrScan(P1, WideLF);
                if P2 = nil then
                  Buffer[I] := Copy(Source, P1 - PWideChar(Source) + 1,
                    Length(Source) - (P1 - PWideChar(Source)))
                else
                begin
                  Buffer[I] := Copy(Source, P1 - PWideChar(Source) + 1, P2 - P1);
                  Inc(P2);
                end;
                P1 := P2;
                Inc(I);
              end;
              AppendToPrompt(Buffer);
            end;
            SynEdit.ExecuteCommand(ecEditorBottom, ' ', nil);
            SynEdit.EnsureCursorPosVisible;
          finally
            SynEdit.UnlockDrawing;
            SynEdit.EndUpdate;
          end;
        end;

        if Command = ecRecallCommandEsc then
          SynEdit.ExecuteCommand(ecCancelSelections, ' ', nil);
      end;
  end;
  Command := ecNone;  // do not processed it further
end;


procedure TPythonIIForm.SetCommandHistorySize(const Value: Integer);
begin
  FCommandHistorySize := Value;
  if FCommandHistory.Count > Value then begin
    for var I := 1 to FCommandHistory.Count - Value do
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

procedure TPythonIIForm.SetShowOutput(const Value: Boolean);
begin
  FShowOutput := Value;
end;

procedure TPythonIIForm.ShowWindow(Activate: Boolean = True);
begin
  GI_PyIDEServices.ShowIDEDockForm(Self, Activate);
end;

procedure TPythonIIForm.SynCodeCompletionAfterCodeCompletion(Sender: TObject;
  const Value: string; Shift: TShiftState; Index: Integer; EndToken: Char);
begin
  if Value.EndsWith('()') then
  begin
    // if the next char is an opening bracket remove the added brackets
    if (EndToken = '(') or ((SynEdit.CaretX <= SynEdit.LineText.Length) and
      IsOpeningBracket(SynEdit.LineText[SynEdit.CaretX], SynEdit.Brackets)) then
    begin
      SynEdit.BeginUpdate;
      try
        SynEdit.ExecuteCommand(ecDeleteLastChar, #0, nil);
        SynEdit.ExecuteCommand(ecDeleteLastChar, #0, nil);
      finally
        SynEdit.EndUpdate;
      end;
    end
    else
    begin
      SynEdit.CaretX:= SynEdit.CaretX - 1;
      EndToken:= '(';
    end;
  end;
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
  TIDECompletion.CompletionInfo.CleanUp;
end;

procedure TPythonIIForm.DoCodeCompletion(Editor: TSynEdit; Caret: TBufferCoord);
var
  LocLine: string;
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
  LocLine := SynEdit.LineText.PadRight(Caret.Char); // to deal with trim trailing spaces
  Inc(Caret.Char);

  Prompt := GetPromptPrefix(LocLine);
  if Prompt <> '' then begin
    // Replace prompt with spaces
    for var I := 1 to Length(Prompt) do
      LocLine[I] := ' ';
    // Exit if it is a system command
    if TRegEx.IsMatch(LocLine, '^\s*!') then
      Exit;
  end else
    Exit;  // This is not a code line

  if not TIDECompletion.CompletionLock.TryEnter then Exit;
  try
    // Exit if busy
    if TIDECompletion.CompletionInfo.Editor <> nil then Exit;
    TIDECompletion.CompletionInfo.CleanUp;
    TIDECompletion.CompletionInfo.Editor := SynEdit;
    TIDECompletion.CompletionInfo.CaretXY := Caret;
  finally
    TIDECompletion.CompletionLock.Leave;
  end;

  TTask.Create(procedure
  var
    DisplayText, InsertText: string;
  begin
    var CC := TIDECompletion.InterpreterCodeCompletion;
    if not TIDECompletion.CompletionLock.TryEnter then Exit;
    try
      var Skipped := False;
      for var I := 0 to CC.SkipHandlers.Count -1 do
      begin
        var SkipHandler := CC.SkipHandlers[I] as TBaseCodeCompletionSkipHandler;
        Skipped := SkipHandler.SkipCodeCompletion(LocLine, '', Caret, Highlighter, Attr);
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
            Handled := CompletionHandler.HandleCodeCompletion(LocLine, '',
              Caret, Highlighter, Attr, InsertText, DisplayText);
          except
          end;
          if Handled then begin
            //CompletionHandler will be finalized in the Cleanup call
            TIDECompletion.CompletionInfo.CompletionHandler := CompletionHandler;
            TIDECompletion.CompletionInfo.InsertText := InsertText;
            TIDECompletion.CompletionInfo.DisplayText := DisplayText;
            Break;
          end
          else
            CompletionHandler.Finalize;
        end;
      end;

      if not Skipped and Handled then
      begin
        // TBaseLspCompletionHandler descendents with activate completion
        // from the completion handler
        if not (TIDECompletion.CompletionInfo.CompletionHandler is
          TBaseLspCompletionHandler) and (InsertText <> '')
        then
          TThread.Queue(nil, procedure
          begin
            CommandsDataModule.SynCodeCompletion.ActivateCompletion;
          end)
      end
      else
        TIDECompletion.CompletionInfo.CleanUp;
    finally
      TIDECompletion.CompletionLock.Leave;
    end;
  end).Start;
end;

procedure TPythonIIForm.SynCodeCompletionExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: string; var X, Y: Integer;
  var CanExecute: Boolean);
begin
  var CP := TSynCompletionProposal(Sender);

  CanExecute := False;
  if TIDECompletion.CompletionLock.TryEnter then
  try
    CanExecute := Application.Active and
      (GetParentForm(SynEdit).ActiveControl = SynEdit) and
      (TIDECompletion.CompletionInfo.CaretXY = SynEdit.CaretXY);

    if CanExecute then
    begin
      CP.Font := PyIDEOptions.AutoCompletionFont;
      CP.ItemList.Text := TIDECompletion.CompletionInfo.DisplayText;
      CP.InsertList.Text := TIDECompletion.CompletionInfo.InsertText;
      CP.NbLinesInWindow := PyIDEOptions.CodeCompletionListSize;
      CP.CurrentString := CurrentInput;

      if CP.Form.AssignedList.Count = 0 then
      begin
        CanExecute := False;
        TIDECompletion.CompletionInfo.CleanUp;
      end
      else
      if PyIDEOptions.CompleteWithOneEntry and (CP.Form.AssignedList.Count = 1) then
      begin
        // Auto-complete with one entry without showing the form
        CanExecute := False;
        CP.OnValidate(CP.Form, [], #0);
        TIDECompletion.CompletionInfo.CleanUp;
      end;
    end else
    begin
      CP.ItemList.Clear;
      CP.InsertList.Clear;
      TIDECompletion.CompletionInfo.CleanUp;
    end;
  finally
    TIDECompletion.CompletionLock.Leave;
  end;
end;

procedure TPythonIIForm.SynCodeCompletionCodeItemInfo(Sender: TObject;
  AIndex: Integer; var Info: string);
begin
  if not TIDECompletion.CompletionLock.TryEnter then Exit;
  try
    if Assigned(TIDECompletion.CompletionInfo.CompletionHandler) then
      Info := TIDECompletion.CompletionInfo.CompletionHandler.GetInfo(
        (Sender as TSynCompletionProposal).InsertList[AIndex]);
  finally
    TIDECompletion.CompletionLock.Leave;
  end;
end;

procedure TPythonIIForm.SynParamCompletionExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: string; var X, Y: Integer;
  var CanExecute: Boolean);
var
  StartX,
  ArgIndex: Integer;
  DisplayString, DocString: string;
  Pt: TPoint;
  ParamString: string;
begin
  CanExecute := False;
  if not GI_PyControl.PythonLoaded or GI_PyControl.Running or not PyIDEOptions.InterpreterCodeCompletion
  then
    Exit;

  CanExecute := TIDECompletion.InterpreterSignatureHelp.HandleParamCompletion('',
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
      DocString := GetLineRange(DocString, 1, 20); // 20 lines max
    end;

    // Determine active argument
    ParamString := Copy(SynEdit.LineText, Succ(StartX),
      TSynCompletionProposal(Sender).Editor.CaretX - Succ(StartX));
    ParamString := ParamString + ' ';  // To deal with for instance '1,'
    ArgIndex := 0;
    GetParameter(ParamString);
    while ParamString <> '' do
    begin
      Inc(ArgIndex);
      GetParameter(ParamString);
    end;

    CP.Form.CurrentIndex := ArgIndex;
    CP.ItemList.Text := DisplayString + DocString;

    //  position the hint window at and just below the opening bracket
    Pt := SynEdit.ClientToScreen(SynEdit.RowColumnToPixels(
      SynEdit.BufferToDisplayPos(BufferCoord(Succ(StartX), SynEdit.CaretY))));
    Inc(Pt.Y, SynEdit.LineHeight);
    X := Pt.X;
    Y := Pt.Y;
  end else begin
    CP.ItemList.Clear;
    CP.InsertList.Clear;
  end;
end;

function TPythonIIForm.FormHelp(Command: Word; Data: THelpEventData;
  var CallHelp: Boolean): Boolean;
var
  Keyword: string;
begin
  Keyword := SynEdit.WordAtCursor;
  if not PyIDEMainForm.PythonKeywordHelpRequested and
    not PyIDEMainForm.MenuHelpRequested and (Keyword <> '') then
  begin
    CallHelp := not CommandsDataModule.ShowPythonKeywordHelp(Keyword);
    Result := True;
  end else begin
    CallHelp := True;
    Result := False;
  end;
end;

procedure TPythonIIForm.StartOutputMirror(const AFileName: string;
  Append: Boolean);
var
  Mode: Integer;
begin
  FCriticalSection.Enter;
  try
    FreeAndNil(FOutputMirror);
    try
      if Append and FileExists(AFileName) then
        Mode := fmOpenReadWrite
      else
        Mode := fmCreate;
      FOutputMirror := TFileStream.Create(AFileName, Mode or fmShareDenyWrite);

      if Append and (FOutputMirror.Size > 0) then
        FOutputMirror.Seek(0, soFromEnd)
      else
      begin
        var BOM := TEncoding.UTF8.GetPreamble;
        FOutputMirror.Write(BOM, Length(BOM));  // save in utf8 encoding
      end;
    except
      StyledMessageDlg(Format(_(SCouldNotOpenOutputFile), [AFileName]), mtWarning, [mbOK], 0);
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TPythonIIForm.StopFileMirror;
begin
  FCriticalSection.Enter;
  try
    FreeAndNil(FOutputMirror);
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TPythonIIForm.UpdateInterpreterActions;
begin
  actCopyWithoutPrompts.Enabled := SynEdit.SelAvail;
  actPasteAndExecute.Enabled := Clipboard.HasFormat(CF_UNICODETEXT);
end;

procedure TPythonIIForm.UpdatePythonKeywords;
var
  Keywords, Builtins, BuiltInMod: Variant;
begin
  with ResourcesDataModule do begin
    SynPythonSyn.Keywords.Clear;
    SynPythonSyn.Keywords.Sorted := False;
    Keywords := Import('keyword').kwlist;
    for var I := 0 to len(Keywords) - 1 do
      SynPythonSyn.Keywords.AddObject(Keywords.__getitem__(I), Pointer(Ord(tkKey)));
    // Avoid adding duplicates (None, True, False)
    SynPythonSyn.Keywords.Sorted := True;
    BuiltInMod := VarPyth.BuiltinModule;
    Builtins := BuiltInMod.dir(BuiltinMod);
    for var I := 0 to len(Builtins) - 1 do
      SynPythonSyn.Keywords.AddObject(Builtins.__getitem__(I), Pointer(Ord(tkNonKeyword)));
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
  if CanActuallyFocus(SynEdit) then
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

function TPythonIIForm.IsEmpty: Boolean;
begin
  Result := (SynEdit.Lines.Count  = 0) or
    ((SynEdit.Lines.Count  = 1) and (SynEdit.Lines[0] = ''));
end;

function TPythonIIForm.CanFind: Boolean;
begin
  Result := not IsEmpty;
end;

function TPythonIIForm.CanFindNext: Boolean;
begin
  Result := not IsEmpty and
    (EditorSearchOptions.SearchText <> '');
end;

function TPythonIIForm.CanReplace: Boolean;
begin
  Result := not IsEmpty;
end;

procedure TPythonIIForm.ExecFind;
begin
  CommandsDataModule.ShowSearchReplaceDialog(SynEdit, False);
end;

procedure TPythonIIForm.ExecFindNext;
begin
  CommandsDataModule.DoSearchReplaceText(SynEdit, False, False);
end;

procedure TPythonIIForm.ExecFindPrev;
begin
  CommandsDataModule.DoSearchReplaceText(SynEdit, False, True);
end;

procedure TPythonIIForm.ExecReplace;
begin
  CommandsDataModule.ShowSearchReplaceDialog(SynEdit, True);
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
      SynEdit.SelText := '';
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
begin
  Assert(Length(Buffer) = EndLineN-StartLineN + 1, 'TPythonIIForm.GetBlockCode');

  Source := '';
  for var I := StartLineN to EndLineN do
  begin
    Line := SynEdit.Lines[I];
    Len := Length(GetPromptPrefix(Line));
    Buffer[I - StartLineN] := Copy(Line, Len + 1, MaxInt);
    Source := Source + Buffer[I - StartLineN] + WideLF;
  end;
  Delete(Source, Length(Source), 1);
end;

function TPythonIIForm.GetEditor: TCustomSynEdit;
begin
  Result := SynEdit;
end;

procedure TPythonIIForm.RegisterHistoryCommands;
// Register the Recall History Command

  procedure AddEditorCommand(Cmd: TSynEditorCommand; SC: TShortCut);
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
    TempStringList.Add(StrStringToEscaped(CommandHistory[I]));
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
    CommandHistory.Add(StrEscapedToString(TempStringList[I]));
  CommandHistoryPointer := TempStringList.Count;  // one after the last one
end;

end.
