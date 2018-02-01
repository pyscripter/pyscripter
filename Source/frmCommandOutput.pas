{-----------------------------------------------------------------------------
 Unit Name: frmCommandOutput
 Author:    Kiriakos Vlahos
 Date:      24-Apr-2008
 Purpose:
 History:
-----------------------------------------------------------------------------}

unit frmCommandOutput;

interface

uses
  System.UITypes, Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmIDEDockWin, JvDockControlForm, ExtCtrls,
  StdCtrls, JvCreateProcess, Menus, ActnList, uEditAppIntfs, cTools,
  SynEditTypes, SynRegExpr, TB2Item, JvComponentBase, SpTBXItem, System.Actions,
  SpTBXControls;

type
  TOutputWindow = class(TIDEDockWindow)
    lsbConsole: TListBox;
    TimeoutTimer: TTimer;
    OutputPopup: TSpTBXPopupMenu;
    RunningProcess: TSpTBXSubmenuItem;
    mnClose: TSpTBXItem;
    mnQuit: TSpTBXItem;
    mnTerminate: TSpTBXItem;
    N3: TSpTBXSeparatorItem;
    mnStopWaiting: TSpTBXItem;
    N1: TSpTBXSeparatorItem;
    mnCopy: TSpTBXItem;
    mnClear: TSpTBXItem;
    N2: TSpTBXSeparatorItem;
    mnFont: TSpTBXItem;
    OutputActions: TActionList;
    actToolStopWaiting: TAction;
    actToolQuit: TAction;
    actToolClose: TAction;
    actToolTerminate: TAction;
    actClearOutput: TAction;
    actOutputFont: TAction;
    actCopy: TAction;
    procedure actOutputFontExecute(Sender: TObject);
    procedure actClearOutputExecute(Sender: TObject);
    procedure JvCreateProcessRead(Sender: TObject; const S: String;
      const StartsOnNewLine: Boolean);
    procedure JvCreateProcessTerminate(Sender: TObject;
      ExitCode: Cardinal);
    procedure actCopyExecute(Sender: TObject);
    procedure actToolTerminateExecute(Sender: TObject);
    procedure actToolCloseExecute(Sender: TObject);
    procedure actToolQuitExecute(Sender: TObject);
    procedure actToolStopWaitingExecute(Sender: TObject);
    procedure OutputActionsUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimeoutTimerTimer(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
    fTool : TExternalTool;
    fEditor : IEditor;
    fBlockBegin : TBufferCoord;
    fBlockEnd : TBufferCoord;
    fRegExpr : TRegExpr;
    fItemMaxWidth : integer;  // Calculating max width to show hor scrollbar
  public
    { Public declarations }
    JvCreateProcess: TJvCreateProcess;
    procedure AddNewLine(const S: string);
    procedure ChangeLastLine(const S: string);
    procedure ClearScreen;
    procedure FontOrColorUpdated;
    procedure ExecuteTool(Tool : TExternalTool);
  end;

var
  OutputWindow: TOutputWindow;

implementation

uses
  Winapi.ShellAPI,
  dmCommands, Clipbrd,
  SynEdit, frmPyIDEMain, frmMessages, JclStrings,
  uCommonFunctions, Math, StringResources, JvGnugettext;

{$R *.dfm}

procedure TOutputWindow.AddNewLine(const S: string);
begin
  with lsbConsole do
  begin
    //  Add the string and calculate the Max Length
    Items.Add(S);
    Canvas.Font := Font;
    fItemMaxWidth := Max(lsbConsole.Canvas.TextWidth(S), fItemMaxWidth);
    ScrollWidth := fItemMaxWidth + 5;
    ItemIndex := Count - 1;
  end;
end;

procedure TOutputWindow.ChangeLastLine(const S: string);
begin
  with lsbConsole do
  begin
    if Count > 0 then begin
      Items[Count - 1] := S;
      Canvas.Font := Font;
      fItemMaxWidth := Max(Canvas.TextWidth(S), fItemMaxWidth);
      ScrollWidth := fItemMaxWidth + 5;
      ItemIndex := Count - 1;
    end else
      AddNewLine(S);
  end;
end;

procedure TOutputWindow.ClearScreen;
begin
  lsbConsole.Clear;
  lsbConsole.ScrollWidth := 0;
  fItemMaxWidth := 0;
end;

procedure TOutputWindow.actOutputFontExecute(Sender: TObject);
begin
  with TFontDialog.Create(Application) do
  try
    Font := lsbConsole.Font;
    if Execute then
    begin
      lsbConsole.Font := Font;
      FontOrColorUpdated;
    end;
  finally
    Free;
  end;
end;

procedure TOutputWindow.actClearOutputExecute(Sender: TObject);
begin
  ClearScreen;
end;

procedure TOutputWindow.FontOrColorUpdated;
begin
  { Force refresh of the window, maybe can be done more elegant? }
  lsbConsole.Perform(CM_RECREATEWND, 0, 0);
end;

procedure TOutputWindow.JvCreateProcessRead(Sender: TObject;
  const S: String; const StartsOnNewLine: Boolean);
begin
  // $0C is the Form Feed char.
  if S = #$C then
    ClearScreen
  else
  if StartsOnNewLine then
    AddNewLine(S)
  else
    ChangeLastLine(S);
end;

procedure TOutputWindow.JvCreateProcessTerminate(Sender: TObject;
  ExitCode: Cardinal);
{
   Called when a proces created with WaitForTermination set to true is finished
   Can parse Traceback info and Messages (ParseTraceback and ParseMessages Tool options
}
Var
 FilePos, LinePos, ColPos: Integer;

  function IsEditorValid(Editor : IEditor) : boolean;
  Var
    i : integer;
  begin
    Result := False;
    for i := 0 to GI_EditorFactory.Count - 1 do
      if GI_EditorFactory.Editor[i] = Editor then begin
        Result := True;
        Exit;
      end;
  end;

  function ReplacePos(var S : string; const FromText, ToText: string): integer;
  begin
    Result:= Pos(UpperCase(FromText), UpperCase(S));
    if Result > 0 then
      S := Copy(S, 1, Result-1) + ToText +
           Copy(S, Result + Length(FromText), MaxInt);
  end;

  function RegExMatch(APos: integer): string;
  // find pos of Match
  Var
    MatchPos : integer;
  begin
    if APos = 0 then
      Result:= ''
    else begin
      MatchPos := 1 +                                        // whole text
           (Ord((FilePos > 0) and (APos > FilePos)) shl 1) + // file name
           Ord((LinePos > 0) and (APos > LinePos)) +         // line number
           Ord((ColPos > 0) and (APos > ColPos));            // col number
      Result:= fRegExpr.Match[MatchPos];
    end;
  end;

 Var
  LineNo, ErrLineNo, ColNo : integer;
  ErrorMsg, RE, FileName, OutStr, OldCurrentDir : string;
begin
  TimeoutTimer.Enabled := False;
  Assert(Assigned(fTool));

  ErrorMsg := Format(_(sProcessTerminated), [StrRemoveChars(fTool.Caption, ['&']), ExitCode]);
  AddNewLine(ErrorMsg);
  PyIDEMainForm.WriteStatusMsg(ErrorMsg);

  if fTool.CaptureOutput then
    ShowDockForm(Self);
  //Standard Output
  if coRedirect in JvCreateProcess.ConsoleOptions then begin
    case fTool.ProcessOutput of
      poWordAtCursor,
      poCurrentLine,
      poSelection :
        if IsEditorValid(fEditor) then with fEditor do begin
          Activate;
          OutStr := JvCreateProcess.ConsoleOutput.Text;
          if JvCreateProcess.ConsoleOutput.Count > 0 then
            Delete(OutStr, Length(OutStr) - Length(sLineBreak) + 1, Length(sLineBreak));
          SynEdit.BlockBegin := fBlockBegin;
          SynEdit.BlockEnd := fBlockEnd;
          SynEdit.SelText := OutStr;
        end;
      poActiveFile :
        if IsEditorValid(fEditor) then with fEditor do begin
          Activate;
          SynEdit.SelectAll;
          SynEdit.SelText := JvCreateProcess.ConsoleOutput.Text;
        end;
      poNewFile :
        begin
          PyIDEMainForm.DoOpenFile(''); // NewFile
          if Assigned(GI_ActiveEditor) then
            GI_ActiveEditor.SynEdit.SelText := JvCreateProcess.ConsoleOutput.Text;
        end;
    end;

    if fTool.ParseTraceback then with JvCreateProcess.ConsoleOutput do begin
      MessagesWindow.ClearMessages;
      //  Parse TraceBack and Syntax Errors from Python output
      fRegExpr.Expression := STracebackFilePosExpr;
      LineNo := 0;
      while LineNo < Count do begin
        if StrIsLeft(PChar(Strings[LineNo]), 'Traceback') then begin
          // Traceback found
          MessagesWindow.AddMessage('Traceback');
          Inc(LineNo);
          while (LineNo < Count) and (Strings[LineNo][1] = ' ') do begin
            if fRegExpr.Exec(Strings[LineNo]) then begin
              ErrLineNo := StrToIntDef(fRegExpr.Match[3], 0);
              // add traceback info (function name, filename, linenumber)
              MessagesWindow.AddMessage('    ' + fRegExpr.Match[5],
                GetLongFileName(ExpandFileName(fRegExpr.Match[1])), ErrLineNo);
            end;
            Inc(LineNo);
          end;
          // Add the actual Error line
          if LineNo < Count then
            MessagesWindow.AddMessage(Strings[LineNo]);
          ShowDockForm(MessagesWindow);
          MessageBeep(MB_ICONEXCLAMATION);
          break;  // finished processing traceback
        end else if StrIsLeft(PChar(Strings[LineNo]), 'SyntaxError:')
          and (LineNo > 2) then
        begin
          // Syntax error found
          ErrorMsg := '    ' + Copy(Strings[LineNo], 14, MaxInt);
          Dec(LineNo);
          ColNo := Pos('^', Strings[LineNo]) - 4; // line indented by 4 spaces
          Dec(LineNo, 2);
          if fRegExpr.Exec(Strings[LineNo]) then begin
            ErrLineNo := StrToIntDef(fRegExpr.Match[3], 0);
            // add Syntax error info (error message, filename, linenumber)
            MessagesWindow.AddMessage(_(SSyntaxError));
            MessagesWindow.AddMessage(ErrorMsg + fRegExpr.Match[5],
              GetLongFileName(ExpandFileName(fRegExpr.Match[1])), ErrLineNo, ColNo);
          end;
          ShowDockForm(MessagesWindow);
          MessageBeep(MB_ICONEXCLAMATION);
          break;  // finished processing Syntax Error
        end;
        Inc(LineNo);
      end;
    end;

    // ParseMessages
    if fTool.ParseMessages then with JvCreateProcess.ConsoleOutput do begin
      MessagesWindow.ClearMessages;
      //  Parse TraceBack and Syntax Errors from Python output

      RE := fTool.MessagesFormat;
      // build actual regular expression
      FilePos:= ReplacePos(RE, GrepFileNameParam, SFileExpr);
      LinePos:= ReplacePos(RE, GrepLineNumberParam, '(\d+)');
      ColPos:=  ReplacePos(RE, GrepColumnNumberParam, '(\d+)');
      try
        fRegExpr.Expression := RE+'(.*)';
        fRegExpr.Compile;
      except
        on E: ERegExpr do begin
          Dialogs.MessageDlg(Format(_(SInvalidRegularExpression), [E.Message]), mtError, [mbOK], 0);
          Exit;
        end;
      end;

      if JvCreateProcess.CurrentDirectory <> '' then begin
         OldCurrentDir := GetCurrentDir;
         SetCurrentDir(JvCreateProcess.CurrentDirectory)
      end;

      try
        LineNo := 0;
        while LineNo < Count do begin
          if fRegExpr.Exec(Strings[LineNo]) then begin
            FileName := RegExMatch(FilePos);
            StringReplace(FileName, '/', '\', [rfReplaceAll]); // fix for filenames with '/'
            FileName := GetLongFileName(ExpandFileName(FileName)); // always full filename
            ErrLineNo := StrToIntDef(RegExMatch(LinePos), -1);
            ColNo :=  StrToIntDef(RegExMatch(ColPos), -1);
           // add Message info (message, filename, linenumber)
            MessagesWindow.AddMessage(fRegExpr.Match[fRegExpr.SubExprMatchCount], FileName, ErrLineNo, ColNo);
          end;
          Inc(LineNo);
        end;
        ShowDockForm(MessagesWindow);
      finally
        if JvCreateProcess.CurrentDirectory <> '' then
         SetCurrentDir(OldCurrentDir);
      end;
    end;
  end;
end;

procedure TOutputWindow.ExecuteTool(Tool : TExternalTool);
Var
  AppName, Arguments, WorkDir, S : string;
  SL : TStringList;
  i : integer;

const
  SwCmds: array[Boolean] of Integer = (SW_SHOWNORMAL, SW_HIDE);

begin
  // Check whether a process is still running
  if jvCreateProcess.State <> psReady then begin
    Dialogs.MessageDlg(_(SProcessRunning), mtError, [mbOK], 0);
    Exit;
  end;

  fTool.Assign(Tool);
  AppName := AddQuotesUnless(PrepareCommandLine(Tool.ApplicationName));
  Arguments := PrepareCommandLine(Tool.Parameters);
  WorkDir := PrepareCommandLine(Tool.WorkingDirectory);

  if (Workdir <> '') and not DirectoryExists(WorkDir) then begin
    Dialogs.MessageDlg(Format(_(SDirNotFound), [WorkDir]), mtError, [mbOK], 0);
    Exit;
  end;

  // In all these cases we need to wait for termination
  JvCreateProcess.WaitForTerminate := Tool.CaptureOutput or Tool.WaitForTerminate or
    (Tool.ProcessInput <> piNone) or (Tool.ProcessOutput <> poNone) or
    Tool.ParseMessages or Tool.ParseTraceback;

  // Check / do Save all files.
  case Tool.SaveFiles of
    sfActive :  if Assigned(GI_ActiveEditor) then GI_FileCmds.ExecSave;
    sfAll    :  CommandsDataModule.actFileSaveAllExecute(nil);
  end;

  // Clear old output
  ClearScreen;
  JvCreateProcess.ConsoleOutput.Clear;
  Application.ProcessMessages;

  // Print Command line info
  AddNewLine(Format(_(SPrintCommandLine), [AppName + ' ' + Arguments]));
  AddNewLine(_(SPrintWorkingDir) + WorkDir);
  AddNewLine(Format(_(SPrintTimeOut), [Tool.TimeOut]));
  AddNewLine('');


  if not (JvCreateProcess.WaitForTerminate or Tool.UseCustomEnvironment) then
  // simple case has the advantage of "executing" files
  begin
    ShellExecute(0, 'open', PChar(AppName), PChar(Arguments),
      PChar(WorkDir), SwCmds[Tool.ConsoleHidden]);
    Exit;
  end;


  with JvCreateProcess do begin
    // According to the Help file it is more robust to add the appname to the command line
    // ApplicationName := AppName;
    CommandLine := Trim(AppName + ' ' + Arguments);
    CurrentDirectory := WorkDir;

    if Tool.CaptureOutput or (Tool.ProcessOutput <> poNone) or
      Tool.ParseMessages or Tool.ParseTraceback or
      (Assigned(GI_ActiveEditor) and (Tool.ProcessInput <> piNone))
    then begin
      ConsoleOptions := ConsoleOptions + [coRedirect];

      if Tool.CaptureOutput then
        OnRead := Self.JvCreateProcessRead
      else
        OnRead := nil;

      if (Tool.ParseMessages) or (Tool.ParseTraceback) or
        (Tool.ProcessOutput <> poNone)
      then
        //Keeps console output in JvCreateProcess.ConsoleOutput
        ConsoleOptions := ConsoleOptions - [coOwnerData]
      else
        ConsoleOptions := ConsoleOptions + [coOwnerData];

    end else begin
      OnRead := nil;
      ConsoleOptions := ConsoleOptions - [coRedirect];
    end;

    if Tool.ConsoleHidden then begin
      StartupInfo.DefaultWindowState := False;
      StartupInfo.ShowWindow := swHide;
    end else begin
      StartupInfo.DefaultWindowState := True;
      StartupInfo.ShowWindow := swNormal;
    end;

    // Prepare for ProcessOutput
    if Tool.ProcessOutput <> poNone then begin
      fEditor := GI_ActiveEditor;
      case fTool.ProcessOutput of
        poWordAtCursor :
          if Assigned(fEditor) and (fEditor.SynEdit.WordAtCursor <> '') then
            with fEditor.SynEdit do begin
              fBlockBegin := WordStart;
              fBlockEnd := WordEnd;
            end
          else
            fTool.ProcessOutput := poNone;
        poCurrentLine :
          if Assigned(fEditor) then
            with fEditor.SynEdit do begin
              fBlockBegin := BufferCoord(1, CaretXY.Line);
              fBlockEnd := BufferCoord(Length(LineText)+1, CaretXY.Line);
            end
          else
            fTool.ProcessOutput := poNone;
        poSelection :
          if Assigned(fEditor) then
            with fEditor.SynEdit do begin
              fBlockBegin := BlockBegin;
              fBlockEnd := BlockEnd;
            end
          else
            fTool.ProcessOutput := poNone;
        poActiveFile :
          if not Assigned(fEditor) then
            fTool.ProcessOutput := poNone;
      end;
    end;

    if Tool.UseCustomEnvironment then
      JvCreateProcess.Environment.Assign(Tool.Environment);

    // Execute Process
    Run;

    // Provide standard input
    if Assigned(GI_ActiveEditor) then begin
      case Tool.ProcessInput of
        piWordAtCursor : JvCreateProcess.Write(AnsiString(GI_ActiveEditor.SynEdit.WordAtCursor));
        piCurrentLine : JvCreateProcess.WriteLn(AnsiString(GI_ActiveEditor.SynEdit.LineText));
        piSelection :
          begin
            S := GI_ActiveEditor.SynEdit.SelText;
            if Length(S) < CCPS_BufferSize then begin
              JvCreateProcess.Write(AnsiString(S));
            end else begin
              SL := TStringList.Create;
              try
                SL.Text := S;
                for i := 0 to SL.Count - 1 do begin
                  JvCreateProcess.WriteLn(AnsiString(SL[i]));
                  Sleep(1);  // give some time to process the the input
                end;
              finally
                SL.Free;
              end;
            end;
          end;
        piActiveFile :
          begin
            SL := TStringList.Create;
            try
              SL.Text := GI_ActiveEditor.SynEdit.Text;
              for i := 0 to SL.Count - 1 do begin
                JvCreateProcess.WriteLn(AnsiString(SL[i]));
                Sleep(1);  // give some time to process the the input
              end;
            finally
              SL.Free;
            end;
          end;
      end;
      if Tool.ProcessInput <> piNone then
        // The following fails in Python 3.x
        // JvCreateProcess.Write(#26); // write EOF character
        JvCreateProcess.CloseWrite;
    end;

    if Tool.WaitForTerminate and (Tool.TimeOut > 0) then begin
      TimeoutTimer.Interval := Tool.Timeout;
      TimeoutTimer.Enabled := True;
    end;

    if Tool.CaptureOutput then begin
      ShowDockForm(Self);
      Application.ProcessMessages;
    end;
  end;
end;

procedure TOutputWindow.actCopyExecute(Sender: TObject);
begin
  Clipboard.AsText := lsbConsole.Items.Text;
end;

procedure TOutputWindow.actToolTerminateExecute(Sender: TObject);
var
  i: Integer;
begin
  if (JvCreateProcess.State <> psReady) then begin
    JvCreateProcess.Terminate;
    TimeoutTimer.Enabled := False;
    for i := 0 to 10 do
      if JvCreateProcess.State <> psReady then begin
        // Wait for the threads to terminate
        Application.ProcessMessages;
        CheckSynchronize;
        Sleep(10);
      end;
  end;
end;

procedure TOutputWindow.actToolCloseExecute(Sender: TObject);
begin
  if (JvCreateProcess.State <> psReady) then
    JvCreateProcess.CloseApplication(False);
end;

procedure TOutputWindow.actToolQuitExecute(Sender: TObject);
begin
  if (JvCreateProcess.State <> psReady) then
    JvCreateProcess.CloseApplication(True);
end;

procedure TOutputWindow.actToolStopWaitingExecute(Sender: TObject);
begin
  if (JvCreateProcess.State <> psReady) then
    JvCreateProcess.StopWaiting;
end;

procedure TOutputWindow.OutputActionsUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  actToolQuit.Enabled := JvCreateProcess.State <> psReady;
  actToolClose.Enabled := JvCreateProcess.State <> psReady;
  actToolTerminate.Enabled := JvCreateProcess.State <> psReady;
  actToolStopWaiting.Enabled := (JvCreateProcess.State <> psReady) and
    not (coRedirect in JvCreateProcess.ConsoleOptions);
end;

procedure TOutputWindow.FormActivate(Sender: TObject);
begin
  inherited;
  if lsbConsole.CanFocus then
    lsbConsole.SetFocus;
end;

procedure TOutputWindow.FormCreate(Sender: TObject);
begin
  inherited;
  fTool := TExternalTool.Create;
  fRegExpr := TRegExpr.Create;

  JvCreateProcess := TJvCreateProcess.Create(Self);
  with JvCreateProcess do
  begin
    Name := 'ExternalToolProcess';
    ConsoleOptions := [];
    CreationFlags := [cfUnicode];
    OnTerminate := JvCreateProcessTerminate;
    OnRead := JvCreateProcessRead;
  end;
end;

procedure TOutputWindow.FormDestroy(Sender: TObject);
begin
  fTool.Free;
  fRegExpr.Free;
  inherited;
end;

procedure TOutputWindow.TimeoutTimerTimer(Sender: TObject);
begin
  if (JvCreateProcess.State <> psReady) and Assigned(fTool) then begin
    TimeoutTimer.Enabled := False;
    if Dialogs.MessageDlg(Format(_(SExternalToolStillRunning),
      [fTool.Caption]), mtConfirmation, [mbYes, mbNo], 0) = mrYes
    then
      actToolTerminateExecute(Sender)
    else begin
      if (JvCreateProcess.State <> psReady) then
        TimeoutTimer.Enabled := True;  // start afresh
    end;
  end else
    TimeoutTimer.Enabled := False;
end;

end.
