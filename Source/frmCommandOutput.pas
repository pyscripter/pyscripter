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
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Actions,
  System.ImageList,
  System.RegularExpressions,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Menus,
  Vcl.ActnList,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  TB2Item,
  SpTBXItem,
  JclSynch,
  JvComponentBase,
  JvDockControlForm,
  JvAppStorage,
  frmIDEDockWin,
  uSysUtils,
  cTools;

type
{$SCOPEDENUMS ON}
  TOutputType = (Normal, Error);
{$SCOPEDENUMS OFF}

  TOutputWindow = class(TIDEDockWindow)
    lsbConsole: TListBox;
    OutputPopup: TSpTBXPopupMenu;
    RunningProcess: TSpTBXSubmenuItem;
    mnTerminate: TSpTBXItem;
    siSeparator1: TSpTBXSeparatorItem;
    mnCopy: TSpTBXItem;
    mnClear: TSpTBXItem;
    siSeparator2: TSpTBXSeparatorItem;
    mnFont: TSpTBXItem;
    OutputActions: TActionList;
    actToolTerminate: TAction;
    actClearOutput: TAction;
    actOutputFont: TAction;
    actCopy: TAction;
    vilImages: TVirtualImageList;
    procedure actOutputFontExecute(Sender: TObject);
    procedure actClearOutputExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actToolTerminateExecute(Sender: TObject);
    procedure OutputActionsUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    const FBasePath = 'Output Window Options'; // Used for storing settings
    var FTool: TExternalTool;
    FCmdOptions: TJclExecuteCmdProcessOptions;
    FAbortEvent: TJclEvent;
    FIsRunning: Boolean;
    FRegEx: TRegEx;
    FItemMaxWidth: Integer;  // Calculating max width to show hor scrollbar
    {Process Output stuff}
    FOutputLock: TRTLCriticalSection;
    FInputEncoding: TEncoding;
    FOutputEncoding: TEncoding;
    FOutputReader: array [TOutputType] of TStreamReader;
    FLastStreamPos: array [TOutputType] of Integer;
    FNewLine: array [TOutputType] of Boolean;
    FInputString: string;
    FActiveEditorId: string;
    FRunningTool: string;
    procedure InitializeOutput;
    procedure FinalilzeOuput;
    procedure ProcessOutput(OutType: TOutputType; const Bytes: TBytes; BytesRead: Cardinal);
    procedure ProcessStdOut(const Bytes: TBytes; BytesRead: Cardinal);
    procedure ProcessStdErr(const Bytes: TBytes; BytesRead: Cardinal);
    procedure BeforeResume(const ProcessInfo: TProcessInformation; WriteHandle:
        PHandle);
    procedure ProcessTerminate;
    procedure WriteOutput(OutputType: TOutputType);
  public
    procedure AddNewLine(const Str: string; OutputType: TOutputType = TOutputType.Normal);
    procedure AppendToLastLine(const Str: string; OutputType: TOutputType);
    procedure ClearScreen;
    procedure FontOrColorUpdated;
    procedure ExecuteTool(Tool: TExternalTool);
    procedure StoreSettings(Storage: TJvCustomAppStorage); override;
    procedure RestoreSettings(Storage: TJvCustomAppStorage); override;
    property IsRunning: Boolean read FIsRunning;
    property RunningTool: string read FRunningTool;
  end;

var
  OutputWindow: TOutputWindow;

implementation

uses
  Winapi.ShellAPI,
  System.Math,
  System.SyncObjs,
  System.Threading,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Clipbrd,
  JvGnugettext,
  SynEditTypes,
  SynEdit,
  StringResources,
  uEditAppIntfs,
  dmResources,
  dmCommands,
  uCommonFunctions;

{$R *.dfm}

procedure TOutputWindow.AddNewLine(const Str: string; OutputType: TOutputType);
begin
  with lsbConsole do
  begin
    //  Add the string and calculate the Max Length
    Items.AddObject(Str, TObject(OutputType));
    Canvas.Font := Font;
    FItemMaxWidth := Max(lsbConsole.Canvas.TextWidth(Str), FItemMaxWidth);
    ScrollWidth := FItemMaxWidth + 5;
    ItemIndex := Count - 1;
  end;
end;

procedure TOutputWindow.BeforeResume(const ProcessInfo: TProcessInformation;
  WriteHandle: PHandle);
{ Encode and write std. input }
begin
  if WriteHandle^ = 0 then Exit;
  if FInputString <> '' then
  begin
    var Task := TTask.Create(procedure
    begin
      var Bytes := FInputEncoding.GetBytes(FInputString);
      var Written: DWORD;
      // WriteFile may block for large input
      WriteFile(WriteHandle^, Bytes[0], Length(Bytes), Written, nil);
      // Signal the end of input
      CloseHandle(WriteHandle^);
      WriteHandle^ := 0;
    end);
    Task.Start;
  end
  else
  begin
    // Signal the end of input
    CloseHandle(WriteHandle^);
    WriteHandle^ := 0;
  end;
end;

procedure TOutputWindow.AppendToLastLine(const Str: string; OutputType: TOutputType);
begin
  with lsbConsole do
  begin
    if (Count > 0) and (Items.Objects[Count - 1] = TObject(OutputType)) then begin
      Items[Count - 1] := Items[Count - 1] + Str;
      Canvas.Font := Font;
      FItemMaxWidth := Max(Canvas.TextWidth(Str), FItemMaxWidth);
      ScrollWidth := FItemMaxWidth + 5;
      ItemIndex := Count - 1;
    end else
      AddNewLine(Str, OutputType);
  end;
end;

procedure TOutputWindow.ClearScreen;
begin
  lsbConsole.Clear;
  lsbConsole.ScrollWidth := 0;
  FItemMaxWidth := 0;
end;

procedure TOutputWindow.actOutputFontExecute(Sender: TObject);
begin
  lsbConsole.Font.PixelsPerInch := FCurrentPPI;
  with TFontDialog.Create(Application) do
  try
    Font := lsbConsole.Font;
    if Execute then
    begin
      lsbConsole.Font.Assign(Font);
      FontOrColorUpdated;
    end;
  finally
    Free;
  end;
  {$IF CompilerVersion < 36}
  lsbConsole.Font.PixelsPerInch := Screen.PixelsPerInch;
  {$ENDIF}
end;

procedure TOutputWindow.actClearOutputExecute(Sender: TObject);
begin
  ClearScreen;
end;

procedure TOutputWindow.FinalilzeOuput;
begin
  FOutputLock.Enter;
  try
    for var OutType in [TOutputType.Normal, TOutputType.Error] do
      FreeAndNil(FOutputReader[OutType]);
  finally
    FOutputLock.Leave;
  end;
  FreeAndNilEncoding(FInputEncoding);
  FreeAndNilEncoding(FOutputEncoding);
end;

procedure TOutputWindow.FontOrColorUpdated;
begin
  { Force refresh of the window, maybe can be done more elegant? }
  lsbConsole.Perform(CM_RECREATEWND, 0, 0);
end;

procedure TOutputWindow.ProcessTerminate;
{
   Called when a proces created with WaitForTermination set to true is finished
   Can parse Traceback info and Messages (ParseTraceback and ParseMessages Tool options
}
var
 FilePos, LinePos, ColPos: Integer;

  function ReplacePos(var Str: string; const FromText, ToText: string): Integer;
  begin
    Result:= Pos(UpperCase(FromText), UpperCase(Str));
    if Result > 0 then
      Str := Copy(Str, 1, Result-1) + ToText +
           Copy(Str, Result + Length(FromText), MaxInt);
  end;

  function MatchIndex(APos: Integer): Integer;
  // find pos of Match
  begin
    if APos = 0 then
      Result:= 0
    else begin
      Result := 1 +                                    // whole text
           Ord((FilePos > 0) and (APos > FilePos)) +   // file name
           Ord((LinePos > 0) and (APos > LinePos)) +   // line number
           Ord((ColPos > 0) and (APos > ColPos));       // col number
    end;
  end;

 var
  LineNo, ErrLineNo, ColNo, Indx: Integer;
  ErrorMsg, RE, FileName, OutStr, OldCurrentDir: string;
  ActiveEditor: IEditor;
begin
  if GI_PyIDEServices.IsClosing then Exit;

  Assert(Assigned(FTool), 'TOutputWindow.ProcessTerminate');

  if FTool.CaptureOutput then
  begin
    // Finish off writing
    WriteOutput(TOutputType.Normal);
    WriteOutput(TOutputType.Error);
  end;

  if FCmdOptions.ExitCode = 0 then
    ErrorMsg := Format(_(SProcessSuccessful), [FTool.Caption.Replace('&', '')])
  else
    ErrorMsg := Format(_(SProcessTerminated), [FTool.Caption.Replace('&', ''), FCmdOptions.ExitCode]);
  AddNewLine('');
  AddNewLine(ErrorMsg);
  GI_PyIDEServices.WriteStatusMsg(ErrorMsg);

  if FTool.CaptureOutput then
    ShowDockForm(Self);

  //Standard Output
  if (FTool.ProcessOutput <> TProcessStdOutputOption.poNone) or FTool.ParseMessages then
  begin
    FOutputReader[TOutputType.Normal].Rewind;
    OutStr := FOutputReader[TOutputType.Normal].ReadToEnd;
  end;

  if OutStr <> '' then
  begin
    ActiveEditor := GI_PyIDEServices.ActiveEditor;
    if Assigned(ActiveEditor) and (ActiveEditor.FileId <> FActiveEditorId) then
      ActiveEditor := nil;

    case FTool.ProcessOutput of
      poWordAtCursor:
        if Assigned(ActiveEditor) then with ActiveEditor.ActiveSynEdit do
        begin
          SetCaretAndSelection(WordEnd, WordStart, WordEnd);
          SelText := OutStr;
        end;
      poCurrentLine:
        if Assigned(ActiveEditor) then with ActiveEditor.ActiveSynEdit do
        begin
          var BlockEnd := BufferCoord(Length(LineText)+1, CaretXY.Line);
          SetCaretAndSelection(BlockEnd, BufferCoord(1, CaretXY.Line), BlockEnd);
          SelText := OutStr;
        end;
      TProcessStdOutputOption.poSelection:
        if Assigned(ActiveEditor) then with ActiveEditor.ActiveSynEdit do begin
          SelText := OutStr;
        end;
      poActiveFile :
        if Assigned(ActiveEditor) then with ActiveEditor do begin
          SynEdit.SelectAll;
          SynEdit.SelText := OutStr;
        end;
      poNewFile:
        begin
          GI_EditorFactory.OpenFile(''); // NewFile
          if Assigned(GI_ActiveEditor) then
            GI_ActiveEditor.SynEdit.SelText := OutStr;
        end;
    end;

    // ParseMessages
    if FTool.ParseMessages then
    begin
      GI_PyIDEServices.Messages.ClearMessages;
      //  Parse TraceBack and Syntax Errors from Python output
      var Strings := OutStr.Split([#10,#13], TStringSplitOptions.ExcludeEmpty);

      RE := FTool.MessagesFormat;
      // build actual regular expression
      FilePos:= ReplacePos(RE, GrepFileNameParam, SFileExpr);
      LinePos:= ReplacePos(RE, GrepLineNumberParam, '(\d+)');
      ColPos:=  ReplacePos(RE, GrepColumnNumberParam, '(\d+)');

      FRegEx := CompiledRegEx(RE+'(.*)');

      if FCmdOptions.CurrentDir <> '' then begin
         OldCurrentDir := GetCurrentDir;
         SetCurrentDir(FCmdOptions.CurrentDir);
      end;

      try
        LineNo := 0;
        while LineNo < Length(Strings) do begin
          with FRegEx.Match(Strings[LineNo]) do
            if Success then begin
              Indx := MatchIndex(FilePos);
              if Indx > 0 then begin
                FileName := Groups[Indx].Value;
                StringReplace(FileName, '/', '\', [rfReplaceAll]); // fix for filenames with '/'
                FileName := GetLongFileName(ExpandFileName(FileName)); // always full filename
              end;
              Indx := MatchIndex(LinePos);
              if Indx > 0 then
                ErrLineNo := StrToIntDef(Groups[Indx].Value, -1)
              else
                ErrLineNo := -1;
              Indx := MatchIndex(ColPos);
              if Indx > 0 then
                ColNo := StrToIntDef(Groups[Indx].Value, -1)
              else
                ColNo := -1;
             // add Message info (message, filename, linenumber)
              GI_PyIDEServices.Messages.AddMessage(Groups[Groups.Count-1].Value, FileName, ErrLineNo, ColNo);
            end;
          Inc(LineNo);
        end;
        GI_PyIDEServices.Messages.ShowWindow;
      finally
        if FCmdOptions.CurrentDir <> '' then
         SetCurrentDir(OldCurrentDir);
      end;
    end;
  end;

  if FTool.ParseTraceback then
  begin
    //Standard Error
    FOutputReader[TOutputType.Error].Rewind;
    OutStr := FOutputReader[TOutputType.Error].ReadToEnd;
    if OutStr <> '' then
    begin
      GI_PyIDEServices.Messages.ClearMessages;
      var Strings := OutStr.Split([#10,#13], TStringSplitOptions.ExcludeEmpty);
      //  Parse TraceBack and Syntax Errors from Python output
      FRegEx := CompiledRegEx(STracebackFilePosExpr);
      LineNo := 0;
      while LineNo < Length(Strings) do begin
        if Strings[LineNo].StartsWith('Traceback') then begin
          // Traceback found
          GI_PyIDEServices.Messages.AddMessage('Traceback');
          Inc(LineNo);
          while (LineNo < Length(Strings)) and (Strings[LineNo][1] = ' ') do begin
            with FRegEx.Match(Strings[LineNo]) do
              if Success then begin
                ErrLineNo := StrToIntDef(GroupValue(2), 0);
                // add traceback info (function name, filename, linenumber)
                GI_PyIDEServices.Messages.AddMessage('    ' + GroupValue(4),
                  GetLongFileName(ExpandFileName(GroupValue(1))), ErrLineNo);
              end;
            Inc(LineNo);
          end;
          // Add the actual Error line
          if LineNo < Length(Strings) then
            GI_PyIDEServices.Messages.AddMessage(Strings[LineNo]);
          GI_PyIDEServices.Messages.ShowWindow;
          Break;  // finished processing traceback
        end else if Strings[LineNo].StartsWith('SyntaxError:')
          and (LineNo > 2) then
        begin
          // Syntax error found
          ErrorMsg := '    ' + Copy(Strings[LineNo], 14, MaxInt);
          Dec(LineNo);
          ColNo := Pos('^', Strings[LineNo]) - 4; // line indented by 4 spaces
          Dec(LineNo, 2);
          with FRegEx.Match(Strings[LineNo]) do
          if Success then begin
            ErrLineNo := StrToIntDef(GroupValue(3), 0);
            // add Syntax error info (error message, filename, linenumber)
            GI_PyIDEServices.Messages.AddMessage(_(SSyntaxError));
            GI_PyIDEServices.Messages.AddMessage(ErrorMsg + GroupValue(5),
              GetLongFileName(ExpandFileName(GroupValue(1))), ErrLineNo, ColNo);
          end;
          GI_PyIDEServices.Messages.ShowWindow;
          MessageBeep(MB_ICONEXCLAMATION);
          Break;  // finished processing Syntax Error
        end;
        Inc(LineNo);
      end;
    end;
  end;
end;

procedure TOutputWindow.RestoreSettings(Storage: TJvCustomAppStorage);
begin
  inherited;
  {$IF CompilerVersion >= 36}
  lsbConsole.Font.IsDPIRelated := True;
  lsbConsole.Font.PixelsPerInch := FCurrentPPI;
  {$ENDIF}
  Storage.ReadPersistent(FBasePath, lsbConsole.Font);
  {$IF CompilerVersion < 36}
  lsbConsole.Font.Height := MulDiv(lsbConsole.Font.Height, FCurrentPPI,
    Screen.PixelsPerInch);
  {$ENDIF}
  FontOrColorUpdated;
end;

procedure TOutputWindow.StoreSettings(Storage: TJvCustomAppStorage);
begin
  inherited;
  var StoredFont := TSmartPtr.Make(TStoredFont.Create)();
  StoredFont.Assign(lsbConsole.Font);
  StoredFont.PixelsPerInch := FCurrentPPI;
  StoredFont.Height := lsbConsole.Font.Height;
  Storage.DeleteSubTree(FBasePath);
  Storage.WritePersistent(FBasePath, StoredFont);
end;

procedure TOutputWindow.ExecuteTool(Tool: TExternalTool);
var
  AppName, Arguments, WorkDir: string;
  WaitForTerminate: Boolean;

const
  SwCmds: array[Boolean] of Integer = (SW_SHOWNORMAL, SW_HIDE);

begin
  // Check whether a process is still running
  if IsRunning then begin
    StyledMessageDlg(_(SProcessRunning), mtError, [mbOK], 0);
    Exit;
  end;

  FTool.Assign(Tool);
  AppName := AddQuotesUnless(PrepareCommandLine(Tool.ApplicationName));
  Arguments := PrepareCommandLine(Tool.Parameters);
  WorkDir := PrepareCommandLine(Tool.WorkingDirectory);

  if (WorkDir <> '') and not DirectoryExists(WorkDir) then begin
    StyledMessageDlg(Format(_(SDirNotFound), [WorkDir]), mtError, [mbOK], 0);
    Exit;
  end;

  // In all these cases we need to wait for termination
  WaitForTerminate := Tool.CaptureOutput or
    (Tool.ProcessInput <> piNone) or (Tool.ProcessOutput <> TProcessStdOutputOption.poNone) or
    Tool.ParseMessages or Tool.ParseTraceback or Tool.UseCustomEnvironment;

  // Check / do Save all files.
  case Tool.SaveFiles of
    sfActive:  if Assigned(GI_ActiveEditor) then GI_FileCmds.ExecSave;
    sfAll:     CommandsDataModule.actFileSaveAllExecute(nil);
  end;

  // Clear old output
  ClearScreen;
  Application.ProcessMessages;

  // Print Command line info
  AddNewLine(Format(_(SCommandLineMsg), [AppName + ' ' + Arguments]));
  AddNewLine(_(SPrintWorkingDir) + WorkDir);
  AddNewLine('');


  if not WaitForTerminate then
  // simple case has the advantage of "executing" files
  begin
    ShellExecute(0, 'open', PChar(AppName), PChar(Arguments),
      PChar(WorkDir), SwCmds[Tool.ConsoleHidden]);
    Exit;
  end;

  FCmdOptions.CommandLine := Trim(AppName + ' ' + Arguments);
  FCmdOptions.CurrentDir := WorkDir;
  FCmdOptions.MergeError := False;
  FCmdOptions.AbortEvent := FAbortEvent;
  FCmdOptions.OutputBufferCallback := ProcessStdOut;
  FCmdOptions.ErrorBufferCallback := ProcessStdErr;
  FCmdOptions.BeforeResume := BeforeResume;
  if Tool.ConsoleHidden then
    FCmdOptions.StartupVisibility := svHide
  else
    FCmdOptions.StartupVisibility := svNotSet; // svShow
  FCmdOptions.Environment.Clear;
  if Tool.UseCustomEnvironment then
    FCmdOptions.Environment.Assign(Tool.Environment);

  // standard input
  FInputString := '';
  FActiveEditorId := '';
  if Assigned(GI_ActiveEditor) then begin
    FActiveEditorId := GI_ActiveEditor.FileId;
    case Tool.ProcessInput of
      piWordAtCursor: FInputString := GI_ActiveEditor.SynEdit.WordAtCursor;
      piCurrentLine:  FInputString := GI_ActiveEditor.SynEdit.LineText;
      piSelection:    FInputString := GI_ActiveEditor.SynEdit.SelText;
      piActiveFile:   FInputString := GI_ActiveEditor.SynEdit.Text;
    end;
  end;

  InitializeOutput;
  // Execute Process
  FRunningTool := Tool.Caption;
  var Task := TTask.Create(procedure
    begin
       FIsRunning := True;
       try
         ExecuteCmdProcess(FCmdOptions);
       finally
         FIsRunning := False;
       end;

       FRunningTool := '';
       if not GI_PyIDEServices.IsClosing then
         TThread.ForceQueue(nil, procedure
           begin
             ProcessTerminate;
           end);
    end);
  Task.Start;

  if Tool.CaptureOutput then begin
    ShowDockForm(Self);
    Application.ProcessMessages;
  end;
end;

procedure TOutputWindow.actCopyExecute(Sender: TObject);
begin
  Clipboard.AsText := lsbConsole.Items.Text;
end;

procedure TOutputWindow.actToolTerminateExecute(Sender: TObject);
begin
  if IsRunning then
    FAbortEvent.Pulse;
end;

procedure TOutputWindow.OutputActionsUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  actToolTerminate.Enabled := IsRunning;
end;

procedure TOutputWindow.ProcessOutput(OutType: TOutputType; const Bytes:
    TBytes; BytesRead: Cardinal);
var
  OutputPending: Boolean;
  OutputReader: TStreamReader;
begin
  if BytesRead <= 0 then Exit;

  FOutputLock.Enter;
  try
    OutputReader := FOutputReader[OutType];
    Assert(Assigned(OutputReader), 'TOutputWindow.ProcessOutput');
    OutputPending :=
      not (OutputReader.BaseStream.Position = OutputReader.BaseStream.Size);
    OutputReader.BaseStream.Write(Bytes, BytesRead);
  finally
    FOutputLock.Leave;
  end;

  if FTool.CaptureOutput and not OutputPending then
    TThread.ForceQueue(nil, procedure
    begin
      WriteOutput(OutType);
    end, 100);
end;

procedure TOutputWindow.ProcessStdErr(const Bytes: TBytes; BytesRead: Cardinal);
begin
  ProcessOutput(TOutputType.Error, Bytes, BytesRead);
end;

procedure TOutputWindow.ProcessStdOut(const Bytes: TBytes; BytesRead: Cardinal);
begin
  ProcessOutput(TOutputType.Normal, Bytes, BytesRead);
end;

procedure TOutputWindow.FormActivate(Sender: TObject);
begin
  inherited;
  if lsbConsole.CanFocus then
    lsbConsole.SetFocus;
end;

procedure TOutputWindow.FormCreate(Sender: TObject);
begin
  ImageName := 'CmdOuputWin';
  inherited;
  FTool := TExternalTool.Create;
  FCmdOptions := TJclExecuteCmdProcessOptions.Create('');
  FAbortEvent := TJclEvent.Create(nil, True, False, '');

  TExternalTool.ExternalToolExecute := ExecuteTool;
  FOutputLock.Initialize;

  lsbConsole.Font.Name := DefaultCodeFontName;
end;

procedure TOutputWindow.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTool);
  FreeAndNil(FAbortEvent);
  FreeAndNil(FCmdOptions);
  FinalilzeOuput;
  FOutputLock.Free;
  inherited;
end;

procedure TOutputWindow.InitializeOutput;
begin
  FinalilzeOuput;

  if FTool.Utf8IO then
  begin
    FInputEncoding := TEncoding.UTF8;
    FOutputEncoding := TEncoding.UTF8;
  end
  else
  begin
    FInputEncoding := TEncoding.GetEncoding(GetConsoleCP);
    FOutputEncoding := TEncoding.GetEncoding(GetConsoleOutputCP);
  end;

  for var OutType in [TOutputType.Normal, TOutputType.Error] do
  begin
    FreeAndNil(FOutputReader[OutType]);
    FOutputReader[OutType] := TStreamReader.Create(TMemoryStream.Create, FOutputEncoding);
    FOutputReader[OutType].OwnStream;
    FLastStreamPos[OutType] := 0;
    FNewLine[OutType] := True;
  end;
end;

procedure TOutputWindow.WriteOutput(OutputType: TOutputType);
var
  Line: string;
  OutputReader: TStreamReader;
begin
  FOutputLock.Enter;
  try
    OutputReader := FOutputReader[OutputType];
    if not Assigned(OutputReader) or (OutputReader.BaseStream.Size = 0) then
      Exit;

    OutputReader.DiscardBufferedData;
    OutputReader.BaseStream.Position := FLastStreamPos[OutputType];
    while not OutputReader.EndOfStream do
    begin
      Line := OutputReader.ReadLine;
      // $0C is the Form Feed char.
      if Line = #$C then
        ClearScreen
      else if FNewLine[OutputType] then
        AddNewLine(Line, OutputType)
      else
        AppendToLastLine(Line, OutputType);
      FNewLine[OutputType] := True;
    end;
    FLastStreamPos[OutputType] := OutputReader.BaseStream.Position;
    Assert(OutputReader.BaseStream.Position = OutputReader.BaseStream.Size,
      'TOutputWindow.WriteOutput');

    OutputReader.BaseStream.Seek(-1, soEnd);
    //  If the last char was not a CR or LF then set FNewLine to False
    var LastChar: AnsiChar;
    FNewLine[OutputType] :=
      not ((OutputReader.BaseStream.ReadData<AnsiChar>(LastChar) = 1) and
      not (LastChar in [#10, #13]));
  finally
    FOutputLock.Leave;
  end;
end;

end.