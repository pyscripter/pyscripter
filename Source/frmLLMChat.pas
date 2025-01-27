unit frmLLMChat;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  System.ImageList,
  System.Actions,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Menus,
  Vcl.ExtCtrls,
  Vcl.Buttons,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  Vcl.WinXPanels,
  Vcl.WinXCtrls,
  Vcl.ActnList,
  Vcl.AppEvnts,
  SynEdit,
  SynEditHighlighter,
  SynHighlighterMulti,
  JvComponentBase,
  JvDockControlForm,
  SpTBXItem,
  SpTBXDkPanels,
  TB2Dock,
  TB2Toolbar,
  TB2Item,
  SpTBXEditors,
  SpTBXSkins,
  frmIDEDockWin,
  uLLMSupport;

type
  // Interposer class to prevent the auto-scrolling when clicking
  TScrollBox = class(Vcl.Forms.TScrollBox)
  protected
    procedure AutoScrollInView(AControl: TControl); override;
  end;

  TLLMChatForm = class(TIDEDockWindow)
    pnlQuestion: TPanel;
    vilImages: TVirtualImageList;
    ScrollBox: TScrollBox;
    QAStackPanel: TStackPanel;
    aiBusy: TActivityIndicator;
    ChatActionList: TActionList;
    actChatSave: TAction;
    sbAsk: TSpeedButton;
    AppEvents: TApplicationEvents;
    SpTBXDock: TSpTBXDock;
    SpTBXToolbar: TSpTBXToolbar;
    spiSave: TSpTBXItem;
    spiSettings: TSpTBXSubmenuItem;
    spiApiKey: TSpTBXEditItem;
    SpTBXRightAlignSpacerItem: TSpTBXRightAlignSpacerItem;
    spiEndpoint: TSpTBXEditItem;
    spiModel: TSpTBXEditItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    spiTimeout: TSpTBXEditItem;
    spiMaxTokens: TSpTBXEditItem;
    spiSystemPrompt: TSpTBXEditItem;
    actChatRemove: TAction;
    actChatNew: TAction;
    actChatPrevious: TAction;
    actChatNext: TAction;
    spiNextTopic: TSpTBXItem;
    spiPreviousTopic: TSpTBXItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    spiNewTopic: TSpTBXItem;
    spiRemoveTopic: TSpTBXItem;
    actCopyText: TAction;
    actAskQuestion: TAction;
    SynMultiSyn: TSynMultiSyn;
    synQuestion: TSynEdit;
    Splitter: TSpTBXSplitter;
    pmAsk: TSpTBXPopupMenu;
    mnCopy: TSpTBXItem;
    SpTBXSeparatorItem3: TSpTBXSeparatorItem;
    mnSpelling: TSpTBXSubmenuItem;
    mnPaste: TSpTBXItem;
    pmTextMenu: TSpTBXPopupMenu;
    mnCopyText: TSpTBXItem;
    actTopicTitle: TAction;
    SpTBXSeparatorItem4: TSpTBXSeparatorItem;
    spiTitle: TSpTBXItem;
    actCancelRequest: TAction;
    spiCancel: TTBItem;
    actCopyCode: TAction;
    SpTBXItem1: TSpTBXItem;
    actCopyToNewEditor: TAction;
    SpTBXSeparatorItem5: TSpTBXSeparatorItem;
    SpTBXItem2: TSpTBXItem;
    SpTBXSeparatorItem6: TSpTBXSeparatorItem;
    spiOpenai: TSpTBXItem;
    spiOllama: TSpTBXItem;
    spiGemini: TSpTBXItem;
    SpTBXSeparatorItem7: TSpTBXSeparatorItem;
    procedure actChatSaveExecute(Sender: TObject);
    procedure AppEventsMessage(var Msg: TMsg; var Handled: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure synQuestionKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AcceptSettings(Sender: TObject; var NewText: string; var
        Accept: Boolean);
    procedure actAskQuestionExecute(Sender: TObject);
    procedure actCancelRequestExecute(Sender: TObject);
    procedure actChatNewExecute(Sender: TObject);
    procedure actChatNextExecute(Sender: TObject);
    procedure actChatPreviousExecute(Sender: TObject);
    procedure actChatRemoveExecute(Sender: TObject);
    procedure actCopyCodeExecute(Sender: TObject);
    procedure actCopyTextExecute(Sender: TObject);
    procedure actCopyToNewEditorExecute(Sender: TObject);
    procedure actTopicTitleExecute(Sender: TObject);
    procedure ChatActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure mnProviderClick(Sender: TObject);
    procedure pmTextMenuPopup(Sender: TObject);
    procedure spiSettingsInitPopup(Sender: TObject; PopupView: TTBView);
    procedure synQuestionEnter(Sender: TObject);
  private
    function GetCodeBlock(Editor: TSynEdit): string;
    procedure DisplayTopicTitle(Title: string);
    procedure PanelQAResize(Sender: TObject);
    procedure DisplayQA(const QA, ImgName: string);
    procedure ClearConversation;
    procedure DisplayActiveChatTopic;
    procedure PythonHighlighterChange(Sender: TObject);
    procedure SetQuestionTextHint;
    procedure OnLLMResponse(Sender: TObject; const Prompt, Answer: string);
    procedure OnLLMError(Sender: TObject; const Error: string);
  protected
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  public
    LLMChat: TLLMChat;
  end;

var
  LLMChatForm: TLLMChatForm;

implementation

{$R *.dfm}

uses
  System.UITypes,
  System.SysUtils,
  System.Math,
  System.IOUtils,
  Vcl.Graphics,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Themes,
  Vcl.Clipbrd,
  SpTBXControls,
  SVGIconImage,
  JvGnugettext,
  dmCommands,
  dmResources,
  SynMarkdownViewer,
  uEditAppIntfs,
  uCommonFunctions,
  cParameters,
  cPyScripterSettings;

resourcestring
  SQuestionHintValid = 'Ask me anything';
  SQuestionHintInvalid = 'Chat setup incomplete';

procedure TLLMChatForm.actChatSaveExecute(Sender: TObject);
begin
  var FileName := TPath.Combine(TPyScripterSettings.UserDataPath,
    'Chat history.json');
  LLMChat.SaveChat(FileName);
end;

procedure TLLMChatForm.FormDestroy(Sender: TObject);
begin
  ResourcesDataModule.SynPythonSyn.UnhookAttrChangeEvent(PythonHighlighterChange);

  var FileName := TPath.Combine(TPyScripterSettings.UserDataPath,
    'Chat Settings.json');
  LLMChat.SaveSettings(FileName);
  LLMChat.Free;
end;

procedure TLLMChatForm.PanelQAResize(Sender: TObject);
begin
  var PnlAnswer := Sender as TSpTBXPanel;
  var SynAnswer := PnlAnswer.Controls[1] as TSynEdit;
  var NewHeight := Max(MulDiv(30, CurrentPPI, 96),
    SynAnswer.DisplayRowCount * SynAnswer.LineHeight);
  if NewHeight <> PnlAnswer.Height then
    PnlAnswer.Height := NewHeight;
end;

procedure TLLMChatForm.PythonHighlighterChange(Sender: TObject);
begin
  SynMultiSyn.Schemes[0].MarkerAttri.Foreground :=
    ResourcesDataModule.SynPythonSyn.WhitespaceAttribute.Foreground;
  SynMultiSyn.Schemes[1].MarkerAttri.Foreground :=
    ResourcesDataModule.SynPythonSyn.WhitespaceAttribute.Foreground;
end;

procedure TLLMChatForm.AppEventsMessage(var Msg: TMsg; var Handled: Boolean);
begin
  if Msg.message = WM_MOUSEWHEEL then
  begin
    var Window := WindowFromPoint(Msg.pt);
    var WinControl := FindControl(Window);
    if (WinControl is TSynEdit) and string(WinControl.Name).StartsWith('synQA') then
    begin
      SendMessage(WinControl.Parent.Handle, WM_MOUSEWHEEL, Msg.wParam, Msg.lParam);
      Handled := True;
    end;
  end;
end;

procedure TLLMChatForm.ClearConversation;
begin
  while QAStackPanel.ControlCount > 0  do
    QAStackPanel.Controls[QAStackPanel.ControlCount - 1].Free;
end;

procedure TLLMChatForm.DisplayQA(const QA, ImgName: string);
begin
  QAStackPanel.Color := StyleServices.GetSystemColor(clBtnFace);
  var PanelQA := TSpTBXPanel.Create(Self);
  with PanelQA do begin
    Name := 'PanelQA' + QAStackPanel.ControlCount.ToString;
    Color := clNone;
    Anchors := [akLeft,akTop,akRight];
    Width := 570;
    Height := 50;
    Anchors := [akLeft, akTop, akRight];
    Borders := False;
    AlignWithMargins := True;
  end;
  var SvgImage := TSVGIconImage.Create(Self);
  with SvgImage do begin
    Left := 0;
    Top := 0;
    Width := 24;
    Height := 24;
    AutoSize := False;
    ImageList := vilImages;
    ImageName := ImgName;
    Anchors := [akLeft, akTop];
    FixedColor := StyleServices.GetSystemColor(clWindowText);
    ApplyFixedColorToRootOnly := True;
    Parent := PanelQA;
  end;
  var SynQA := TSynMarkdownViewer.Create(Self);
  with SynQA do begin
    Name := 'synQA' + QAStackPanel.ControlCount.ToString;
    Font.Color := StyleServices.GetSystemColor(clWindowText);
    Color := StyleServices.GetSystemColor(clWindow);
    BorderStyle := bsNone;
    Anchors := [akLeft, akRight, akTop, akBottom];
    Options := Options + [eoRightMouseMovesCursor];
    Highlighter := SynMultiSyn;
    Top := 0;
    Left := 30;
    Width := PanelQA.Width - 30;
    Height := PanelQA.Height;
    Font.Name := 'Consolas';
    Font.Size := 10;
    Gutter.Visible := False;
    PopupMenu := pmTextMenu;
    ScrollBars := ssNone;
    WantTabs := True;
    WantReturns := True;
    HideSelection := True;
    Parent := PanelQA;
  end;
  PanelQA.ScaleForPPI(CurrentPPI);
  PanelQA.Parent := QAStackPanel;
  if ImgName = 'Assistant' then
    SynQA.Markdown := QA.Trim
  else
    SynQA.Text := QA.Trim;
  PanelQA.OnResize :=  PanelQAResize;
  // Resize twice! - The first time the Scrollbox scrollbar may be shown
  PanelQAResize(PanelQA);
  PanelQAResize(PanelQA);
  ScrollBox.VertScrollBar.Position := ScrollBox.VertScrollBar.Range - 1;
end;

procedure TLLMChatForm.DisplayTopicTitle(Title: string);
begin
  if Title = '' then
    Caption := _('Chat')
  else
    Caption := _('Chat') + ' - ' + Title;
end;

procedure TLLMChatForm.DisplayActiveChatTopic;
begin
  ClearConversation;
  DisplayTopicTitle(LLMChat.ActiveTopic.Title);

  QAStackPanel.LockDrawing;
  try
    for var QAItem in LLMChat.ActiveTopic.QAItems do
    begin
      DisplayQA(QAItem.Prompt, 'UserQuestion');
      DisplayQA(QAItem.Answer, 'Assistant');
    end;
  finally
    QAStackPanel.UnlockDrawing;
  end;

  if synQuestion.HandleAllocated then
    synQuestion.SetFocus;
end;

procedure TLLMChatForm.FormCreate(Sender: TObject);
begin
  ImageName := 'Chat\Chat';
  inherited;

  {$IF CompilerVersion >= 36}
  ScrollBox.UseWheelForScrolling := True;
  {$ENDIF}
  synQuestion.Font.Color := StyleServices.GetSystemColor(clWindowText);
  synQuestion.Color := StyleServices.GetSystemColor(clWindow);

  SynMultiSyn.Schemes[0].Highlighter := ResourcesDataModule.SynPythonSyn;
  SynMultiSyn.Schemes[0].MarkerAttri.Foreground :=
    ResourcesDataModule.SynPythonSyn.IdentifierAttri.Foreground;
  SynMultiSyn.Schemes[1].Highlighter := ResourcesDataModule.SynPythonSyn;
  SynMultiSyn.Schemes[1].MarkerAttri.Foreground :=
    ResourcesDataModule.SynPythonSyn.IdentifierAttri.Foreground;
  ResourcesDataModule.SynPythonSyn.HookAttrChangeEvent(PythonHighlighterChange);
  LLMChat := TLLMChat.Create;
  LLMChat.OnLLMError := OnLLMError;
  LLMChat.OnLLMResponse := OnLLMResponse;


  // Restore settings and history
  var FileName := TPath.Combine(TPyScripterSettings.UserDataPath,
    'Chat history.json');
  try
    LLMChat.LoadChat(FileName);
  except
    StyledMessageDlg(_('Could read the Chat history'), TMsgDlgType.mtError,
      [TMsgDlgBtn.mbOK], 0);
    DeleteFile(FileName);
  end;

  FileName := TPath.Combine(TPyScripterSettings.UserDataPath,
    'Chat Settings.json');
  try
    LLMChat.LoadSettrings(FileName);
  except
    StyledMessageDlg(_('Could not read the Chat settings'), TMsgDlgType.mtError,
      [TMsgDlgBtn.mbOK], 0);
    DeleteFile(FileName);
  end;

  SetQuestionTextHint;
end;

procedure TLLMChatForm.FormShow(Sender: TObject);
begin
  TThread.ForceQueue(nil, procedure
  begin
    DisplayActiveChatTopic;
  end);
end;

function TLLMChatForm.GetCodeBlock(Editor: TSynEdit): string;
var
  Token: string;
  Attri: TSynHighlighterAttributes;
begin
  Result := '';
  var BC := Editor.CaretXY;

  Editor.GetHighlighterAttriAtRowCol(BC, Token, Attri);
  if SynMultiSyn.CurrScheme < 0 then // not inside python code
    Exit;

  var StartLine := BC.Line;
  var EndLine := BC.Line;

  while (StartLine > 1) and  not Editor.Lines[StartLine -1].StartsWith('```') do
    Dec(StartLine);
  while (EndLine < Editor.Lines.Count) and not Editor.Lines[EndLine -1].StartsWith('```') do
    Inc(EndLine);

  Result := '';
  for var Line := StartLine + 1 to EndLine - 1 do
  begin
    Result := Result + Editor.Lines[Line - 1];
    if Line < EndLine - 1 then
      Result := Result + sLineBreak;
  end;
end;

procedure TLLMChatForm.OnLLMError(Sender: TObject; const Error: string);
begin
  StyledMessageDlg(Error, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
end;

procedure TLLMChatForm.OnLLMResponse(Sender: TObject; const Prompt,
  Answer: string);
begin
  DisplayQA(Prompt, 'UserQuestion');
  DisplayQA(Answer, 'Assistant');
  synQuestion.Clear;
end;

procedure TLLMChatForm.synQuestionKeyDown(Sender: TObject; var Key: Word; Shift:
    TShiftState);
begin
  if (Shift * [ssShift, ssCtrl] <> []) and  (Key = vkReturn) then
  begin
    actAskQuestion.Execute;
    Key := 0;
  end;
end;

procedure TLLMChatForm.AcceptSettings(Sender: TObject; var NewText:
    string; var Accept: Boolean);
begin
  Accept := False;
  try
    var Settings := LLMChat.Settings;
    if Sender = spiEndpoint then
      Settings.EndPoint := NewText
    else if Sender = spiModel then
      Settings.Model := NewText
    else if Sender = spiApiKey then
      Settings.ApiKey := NewText
    else if Sender = spiTimeout then
      Settings.TimeOut := NewText.ToInteger * 1000
    else if Sender = spiMaxTokens then
      Settings.MaxTokens := NewText.ToInteger
    else if Sender = spiSystemPrompt then
      Settings.SystemPrompt := NewText;

    case LLMChat.Providers.Provider of
      llmProviderOpenAI: LLMChat.Providers.OpenAI := Settings;
      llmProviderGemini: LLMChat.Providers.Gemini := Settings;
      llmProviderOllama: LLMChat.Providers.Ollama := Settings;
    end;

    Accept := True;
  except
    on E: Exception do
      StyledMessageDlg(E.Message, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  end;
  if Accept then
    SetQuestionTextHint;
end;

procedure TLLMChatForm.actAskQuestionExecute(Sender: TObject);
begin
  if synQuestion.Text = '' then
    Exit;
  LLMChat.Ask(Parameters.ReplaceInText(synQuestion.Text));
end;

procedure TLLMChatForm.actCancelRequestExecute(Sender: TObject);
begin
  LLMChat.CancelRequest;
end;

procedure TLLMChatForm.actChatNewExecute(Sender: TObject);
begin
  LLMChat.NewTopic;
  DisplayActiveChatTopic;
end;

procedure TLLMChatForm.actChatNextExecute(Sender: TObject);
begin
  LLMChat.NextTopic;
  DisplayActiveChatTopic;
end;

procedure TLLMChatForm.actChatPreviousExecute(Sender: TObject);
begin
  LLMChat.PreviousTopic;
  DisplayActiveChatTopic;
end;

procedure TLLMChatForm.actChatRemoveExecute(Sender: TObject);
begin
  LLMChat.RemoveTopic;
  DisplayActiveChatTopic;
end;

procedure TLLMChatForm.actCopyCodeExecute(Sender: TObject);
begin
  if not (pmTextMenu.PopupComponent is TSynEdit) then
    Exit;

  var Editor := TSynEdit(pmTextMenu.PopupComponent);
  var Code := GetCodeBlock(Editor);
  if Code <> '' then
    Clipboard.AsText := Code;
end;

procedure TLLMChatForm.actCopyTextExecute(Sender: TObject);
begin
  if pmTextMenu.PopupComponent is TSynEdit then with TSynEdit(pmTextMenu.PopupComponent) do
  begin
    if SelAvail then
      Clipboard.AsText := SelText
    else
      Clipboard.AsText := Text;
  end
  else if pmTextMenu.PopupComponent is TSpTBXLabel then
    Clipboard.AsText := TSpTBXLabel(pmTextMenu.PopupComponent).Caption;
end;

procedure TLLMChatForm.actCopyToNewEditorExecute(Sender: TObject);
begin
  if not (pmTextMenu.PopupComponent is TSynEdit) then
    Exit;

  var Editor := TSynEdit(pmTextMenu.PopupComponent);
  var Code := GetCodeBlock(Editor);

  if Code = '' then
    Exit;

  var NewEditor := GI_EditorFactory.NewEditor;
  NewEditor.OpenLocalFile('', 'Python');
  NewEditor.SynEdit.Text := Code;
  NewEditor.Activate;
end;

procedure TLLMChatForm.actTopicTitleExecute(Sender: TObject);
var
  Title: string;
begin
  Title := LLMChat.ChatTopics[LLMChat.ActiveTopicIndex].Title;
  if InputQuery(_('Topic Title'), _('Enter title:'), Title) then
  LLMChat.ChatTopics[LLMChat.ActiveTopicIndex].Title := Title;
  DisplayTopicTitle(Title);
end;

procedure TLLMChatForm.ChatActionListUpdate(Action: TBasicAction; var Handled:
    Boolean);
begin
  Handled := True;
  actChatNew.Enabled := ScrollBox.ControlCount > 0;
  actChatNext.Enabled := LLMChat.ActiveTopicIndex < High(LLMChat.ChatTopics);
  actChatPrevious.Enabled := LLMChat.ActiveTopicIndex > 0;
  actAskQuestion.Enabled := LLMChat.ValidateSettings = svValid;

  var IsBusy := LLMChat.IsBusy;
  if aiBusy.Animate <> IsBusy then
    aiBusy.Animate := IsBusy;
  actCancelRequest.Visible := IsBusy;
  actCancelRequest.Enabled := IsBusy;
end;

procedure TLLMChatForm.mnProviderClick(Sender: TObject);
begin
  if Sender = spiOpenai then
    LLMChat.Providers.Provider := llmProviderOpenAI
  else if Sender = spiOllama then
    LLMChat.Providers.Provider := llmProviderOllama
  else if Sender = spiGemini then
    LLMChat.Providers.Provider := llmProviderGemini;

  spiSettingsInitPopup(Sender, nil);
  SetQuestionTextHint;
end;

procedure TLLMChatForm.pmTextMenuPopup(Sender: TObject);
var
  Token: string;
  Attri: TSynHighlighterAttributes;
begin
  actCopyCode.Enabled := False;
  actCopyToNewEditor.Enabled := False;
  if not (pmTextMenu.PopupComponent is TSynEdit) then
    Exit;

  var Editor := TSynEdit(pmTextMenu.PopupComponent);
  var BC := Editor.CaretXY;

  Editor.GetHighlighterAttriAtRowCol(BC, Token, Attri);
  actCopyCode.Enabled := (SynMultiSyn.CurrScheme >= 0) and
    not Editor.Lines[BC.Line - 1].StartsWith('```');
  actCopyToNewEditor.Enabled := actCopyCode.Enabled;
end;

procedure TLLMChatForm.SetQuestionTextHint;
begin
  var Validation := LLMChat.ValidateSettings;

  if Validation = svValid then
    synQuestion.TextHint := SQuestionHintValid
  else
    synQuestion.TextHint := SQuestionHintInvalid + ': ' + LLMChat.ValidationErrMsg(Validation);
end;

procedure TLLMChatForm.spiSettingsInitPopup(Sender: TObject; PopupView:
    TTBView);
begin
  case LLMChat.Providers.Provider of
    llmProviderOpenAI: spiOpenai.Checked := True;
    llmProviderGemini: spiGemini.Checked := True;
    llmProviderOllama: spiOllama.Checked := True;
  end;

  var Settings := LLMChat.Settings;
  spiEndpoint.Text := Settings.EndPoint;
  spiModel.Text := Settings.Model;
  spiApiKey.Text := Settings.ApiKey;
  spiTimeout.Text := (Settings.TimeOut div 1000).ToString;
  spiMaxTokens.Text := Settings.MaxTokens.ToString;
  spiSystemPrompt.Text := Settings.SystemPrompt;
end;

procedure TLLMChatForm.synQuestionEnter(Sender: TObject);
begin
  // Spell Checking
  CommandsDataModule.SynSpellCheck.Editor := synQuestion;
end;

procedure TLLMChatForm.WMSpSkinChange(var Message: TMessage);
begin
  inherited;
  synQuestion.Font.Color := StyleServices.GetSystemColor(clWindowText);
  synQuestion.Color := StyleServices.GetSystemColor(clWindow);
  {$IF CompilerVersion >= 36}
  aiBusy.IndicatorColor := aicCustom;
  aiBusy.IndicatorCustomColor := StyleServices.GetSystemColor(clWindowText);
  {$ENDIF}
  DisplayActiveChatTopic;
end;

{ TScrollBox }

// To avoid the jumping of the cursor
procedure TScrollBox.AutoScrollInView(AControl: TControl);
begin
end;

end.
