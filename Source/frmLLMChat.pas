unit frmLLMChat;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.WebView2,
  Winapi.ActiveX,
  System.Classes,
  System.ImageList,
  System.Actions,
  System.RegularExpressions,
  Vcl.Graphics,
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
  Vcl.Edge,
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
  MarkdownProcessor,
  uLLMSupport;

type
  TLLMChatForm = class(TIDEDockWindow)
    pnlQuestion: TPanel;
    vilImages: TVirtualImageList;
    aiBusy: TActivityIndicator;
    ChatActionList: TActionList;
    actChatSave: TAction;
    sbAsk: TSpeedButton;
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
    actAskQuestion: TAction;
    SynMultiSyn: TSynMultiSyn;
    synQuestion: TSynEdit;
    Splitter: TSpTBXSplitter;
    pmAsk: TSpTBXPopupMenu;
    mnCopy: TSpTBXItem;
    SpTBXSeparatorItem3: TSpTBXSeparatorItem;
    mnSpelling: TSpTBXSubmenuItem;
    mnPaste: TSpTBXItem;
    actTopicTitle: TAction;
    SpTBXSeparatorItem4: TSpTBXSeparatorItem;
    spiTitle: TSpTBXItem;
    actCancelRequest: TAction;
    spiCancel: TTBItem;
    SpTBXSeparatorItem6: TSpTBXSeparatorItem;
    spiOpenai: TSpTBXItem;
    spiOllama: TSpTBXItem;
    spiGemini: TSpTBXItem;
    SpTBXSeparatorItem7: TSpTBXSeparatorItem;
    spiTemperature: TSpTBXEditItem;
    spiDeepSeek: TSpTBXItem;
    EdgeBrowser: TEdgeBrowser;
    actPrint: TAction;
    spiPrint: TTBItem;
    SpTBXSeparatorItem5: TSpTBXSeparatorItem;
    pnlBrowserCover: TPanel;
    spiGrok: TSpTBXItem;
    procedure actChatSaveExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure synQuestionKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AcceptSettings(Sender: TObject; var NewText: string; var
        Accept: Boolean);
    procedure actAskQuestionExecute(Sender: TObject);
    procedure actCancelRequestExecute(Sender: TObject);
    procedure actChatNewExecute(Sender: TObject);
    procedure actChatNextExecute(Sender: TObject);
    procedure actChatPreviousExecute(Sender: TObject);
    procedure actChatRemoveExecute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
    procedure actTopicTitleExecute(Sender: TObject);
    procedure ChatActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure EdgeBrowserCreateWebViewCompleted(Sender: TCustomEdgeBrowser;
        AResult: HRESULT);
    procedure EdgeBrowserNavigationCompleted(Sender: TCustomEdgeBrowser; IsSuccess:
        Boolean; WebErrorStatus: COREWEBVIEW2_WEB_ERROR_STATUS);
    procedure EdgeBrowserWebMessageReceived(Sender: TCustomEdgeBrowser; Args:
        TWebMessageReceivedEventArgs);
    procedure FormShow(Sender: TObject);
    procedure HighlightCheckedImg(Sender: TObject; ACanvas: TCanvas; State:
        TSpTBXSkinStatesType; const PaintStage: TSpTBXPaintStage; var AImageList:
        TCustomImageList; var AImageIndex: Integer; var ARect: TRect; var
        PaintDefault: Boolean);
    procedure mnProviderClick(Sender: TObject);
    procedure spiSettingsInitPopup(Sender: TObject; PopupView: TTBView);
    procedure synQuestionEnter(Sender: TObject);
  private
    FDefaultLang: string;
    FBlockCount: Integer;
    FCodeBlocksRE: TRegEx;
    FBrowserReady: Boolean;
    FMarkdownProcessor: TMarkdownProcessor;
    procedure CopyToNewEditor(const Code: string);
    procedure ClearConversation;
    procedure DisplayActiveChatTopic;
    procedure DisplayQA(const Prompt, Answer, Reason: string);
    procedure DisplayTopicTitle(Title: string);
    procedure LoadBoilerplate;
    function MarkdownToHTML(const MD: string): string;
    function NavigateToString(Html: string): Boolean;
    procedure PythonHighlighterChange(Sender: TObject);
    procedure SetBrowserColorScheme;
    procedure SetQuestionTextHint;
    procedure StyleForm;
    procedure StyleWebPage;
    procedure OnLLMResponse(Sender: TObject; const Prompt, Answer, Reason: string);
    procedure OnLLMError(Sender: TObject; const Error: string);
  protected
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
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
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Themes,
  Vcl.Clipbrd,
  SpTBXControls,
  MarkdownUtils,
  SynEditMiscProcs,
  SynEditKeyCmds,
  JvDockGlobals,
  JvGnugettext,
  StringResources,
  dmCommands,
  dmResources,
  uEditAppIntfs,
  uCommonFunctions,
  cParameters,
  cPyScripterSettings;

resourcestring
  SQuestionHintValid = 'Ask me anything';
  SQuestionHintInvalid = 'Chat setup incomplete';


{$REGION 'HTML templates'}

const
  Boilerplate = '''
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>LLM Chat</title>
%s
</head>
<body>
    <!-- svgs -->
%s
    <!-- Title and other Content -->
%s
    <!-- Container where Q&A will be added -->
    <div id="qa-list">
    </div>
    <!-- Scripts -->
%s
</body>
</html>
''';

  MainStyleSheetTemplate = '''
    <style>
        ::-webkit-scrollbar {
            width: 6px; /* Initial width of the scrollbar */
            height: 6px; /* Initial height of the scrollbar */
        }
        ::-webkit-scrollbar-track {
            background: %s; /* Background of the scrollbar track */
        }
        ::-webkit-scrollbar-thumb {
            background: %s; /* Color of the scrollbar thumb */
            border-radius: 4px; /* Rounded corners for the scrollbar thumb */
        }
        ::-webkit-scrollbar-thumb:hover {
            background: %s; /* Color of the scrollbar thumb on hover */
        }
        ::-webkit-scrollbar-corner {
            background: %0:s; /* The corner between scrollbars */
        }
        @media print {
            @page {
                @bottom-center {
                    content: "Page " counter(page);
                    }
            }
        }

        body {
            background-color: %0:s;
            color: %3:s;
            font-family: Aptos, Calibri, Arial, sans-serif;
            line-height: 1.4;
            margin: 10px;
        }
        a {
            color: %4:s;
            text-decoration: none;
        }
        a:hover {
            text-decoration: underline;
        }
        blockquote {
          color: gray;
          border-left: 3px solid #ccc;
          padding-left: 10px;
          margin: 10px 0;
          max-height: 400px;
          overflow-y: auto;
        }
    </style>

''';
var
  MainStyleSheet: string;

const
  QAStyleSheet = '''
    <style>
        .qa-container {
            display: flex;
            align-items: flex-start;
            margin-bottom: 5px; /* Space after each question */
            overflow: hidden
        }
        .icon {
            margin-right: 10px;
            width: 24px;
            height: 24px;
            flex-shrink: 0;
            display: flex;
            align-items: center;
        }
        .question {
            font-weight: bold;
            overflow: hidden
        }
        .answer {
            /* margin-bottom: 15px;  Space after each answer */
            overflow: hidden
       }
    </style>

''';

const
  CodeStyleSheetTemplate = '''
    <style>
        .code-box {
            border: 1px solid #ccc;
            border-radius: 5px;
            margin: 10px 0;
            max-width: 1000px;
            overflow: hidden; /* Ensures the box doesn't overflow */
        }
        .code-header {
            background: %s;
            padding: 5px;
            border-bottom: 1px solid #ccc;
            font-weight: bold;
            color: %s;
        }
        .code-container {
            padding: 5px;
            max-height: 500px; /* Limits the height of the code box */
            overflow-y: auto; /* Adds vertical scrolling */
            font-size: 14px;
            /* the Prism style background. Need to change if you change the style*/
            background: %s;
        }
        .code-header-button {
            float: right;
            background: transparent;
            color: inherit;
            border: none;
            border-radius: 3px;
            cursor: pointer;
            font-size: 12px;
            padding: 0px;
            height: 24px;
            margin-right: 1em;
        }
        .code-header-button:hover {
            background-color: #6F6F6F6F;
        }

        /* prism style begin */
        code[class*=language-],
        pre[class*=language-] {
            color: #ccc;
            background: 0 0;
            font-family: Consolas, 'Courier New', monospace;
            font-size: 1em;
            text-align: left;
            white-space: pre;
            word-spacing: normal;
            word-break: normal;
            word-wrap: normal;
            line-height: 1.5;
            -moz-tab-size: 4;
            -o-tab-size: 4;
            tab-size: 4;
            -webkit-hyphens: none;
            -moz-hyphens: none;
            -ms-hyphens: none;
            hyphens: none
        }

        pre[class*=language-] {
            padding: 1em;
            margin: .5em 0;
            overflow: auto
        }

        :not(pre)>code[class*=language-],
        pre[class*=language-] {
            background: #2d2d2d
        }

        :not(pre)>code[class*=language-] {
            padding: .1em;
            border-radius: .3em;
            white-space: normal
        }

        .token.block-comment,
        .token.cdata,
        .token.comment,
        .token.doctype,
        .token.prolog {
            color: %s
        }

       .token.operator,
       .token.punctuation {
            color: %s
        }

        .token.attr-name{
            color: %s
        }

        .token.deleted,
        .token.namespace,
        .token.tag {
            color: %s
        }

        .token.function,
        .token.function-name {
            color: %s
        }

        .token.boolean,
        .token.number {
            color: %s
        }

        .token.class-name,
        .token.constant,
        .token.property,
        .token.symbol {
            color: %s
        }

        .token.atrule,
        .token.builtin,
        .token.important,
        .token.keyword,
        .token.selector {
            color: %s
        }

        .token.attr-value,
        .token.char,
        .token.regex,
        .token.string,
        .token.variable {
            color: %s
        }

        .triple-quoted-string {
            color: %s
        }

        .token.entity,
        .token.url {
            color: %s
        }

        .token.bold,
        .token.important {
            font-weight: 700
        }

        .token.italic {
            font-style: italic
        }

        .token.entity {
            cursor: help
        }

        .token.inserted {
            color: green
        }
        /* prism style end*/


        /* ** modify prism style ** */

        /* inline code */
        :not(pre)>code {
            background-color: #444; /* Darker background for code */
            border-radius: 4px;
            padding: 2px 4px;
            font-family: monospace;
            color: #ffcc00; /* Light color for code text */
        }
        code[class*=language-] {
            color: %s;
        }
        pre[class*=language-] {
            background: %2:s;
            line-height: 1.2;
            padding: 1em;
            margin: 0 0;
            overflow: visible
        }

    </style>

''';
var
  CodeStyleSheet: string;

const
  CodeBlock = '''
      <div class="code-box">
          <div class="code-header">
              %s
              <button class="code-header-button" title="%s" onclick="copyCode('code%s')">
                  <svg width="24" height="24">
                      <use href="#copy-icon">
                  </svg>
              </button>
              <button class="code-header-button" title="%s" onclick="codeToEditor('code%2:s')">
                  <svg width="24" height="24">
                      <use href="#new-editor-icon">
                  </svg>
              </button>
          </div>
          <div class="code-container" id="code%2:s">
              <pre><code class="language-%4:s">%5:s</code></pre>
          </div>
      </div>

  ''';

  SvgIcons = '''
    <!-- Define the SVG icon once -->
    <svg style="display: none;">
        <symbol id="question-icon" viewBox="0 0 24 24" fill="none" stroke=currentColor>
            <path opacity="0.8" d="M12 11C14.2091 11 16 9.20914 16 7C16 4.79086 14.2091 3 12 3C9.79086 3 8 4.79086 8 7C8 9.20914 9.79086 11 12 11Z" fill="cornflowerblue" />
            <path d="M20.9531 13V12.995M19 7.4C19.2608 6.58858 20.0366 6 20.9531 6C22.0836 6 23 6.89543 23 8C23 9.60675 21.2825 8.81678 21 10.5M8 15H16C18.2091 15 20 16.7909 20 19V21H4V19C4 16.7909 5.79086 15 8 15ZM16 7C16 9.20914 14.2091 11 12 11C9.79086 11 8 9.20914 8 7C8 4.79086 9.79086 3 12 3C14.2091 3 16 4.79086 16 7Z" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
        </symbol>

        <symbol id="assistant-icon" viewBox="0 -960 960 960" fill=currentColor>
            <circle r="70" cx="360" cy="-640" fill="#E24444" />
            <circle r="70" cx="600" cy="-640" fill="#E24444" />
            <path d="M160-120v-200q0-33 23.5-56.5T240-400h480q33 0 56.5 23.5T800-320v200H160Zm200-320q-83 0-141.5-58.5T160-640q0-83 58.5-141.5T360-840h240q83 0 141.5 58.5T800-640q0 83-58.5 141.5T600-440H360ZM240-200h480v-120H240v120Zm120-320h240q50 0 85-35t35-85q0-50-35-85t-85-35H360q-50 0-85 35t-35 85q0 50 35 85t85 35Z" />
        </symbol>

        <symbol id="copy-icon" viewBox="0 0 24 24" fill=currentColor>
        <path d="M19,21H8V7H19M19,5H8A2,2 0 0,0 6,7V21A2,2 0 0,0 8,23H19A2,2 0 0,0 21,21V7A2,2 0 0,0 19,5M16,1H4A2,2 0 0,0 2,3V17H4V3H16V1Z" />
        </symbol>

        <symbol id="new-editor-icon" viewBox="0 0 24 24" fill=currentColor>
          <path d="M14,3V5H17.59L7.76,14.83L9.17,16.24L19,6.41V10H21V3M19,19H5V5H12V3H5C3.89,3 3,3.9 3,5V19A2,2 0 0,0 5,21H19A2,2 0 0,0 21,19V12H19V19Z" />
        </symbol>
    </svg>
''';

  JSScripts = '''
    <script src="https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/prism.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/prism/1.24.1/plugins/autoloader/prism-autoloader.min.js"></script>
    <script>
        // Function to add a Q&A to the page
        function addQA(question, answer) {
            var qaList = document.getElementById('qa-list');

            // Create the question container
            const questionContainer = document.createElement('div');
            questionContainer.classList.add('qa-container');

            // Create the question icon container
            const questionIconDiv = document.createElement('div'); // More descriptive name
            questionIconDiv.classList.add('icon');
            const questionIconSvg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
            questionIconSvg.setAttribute('width', '24');
            questionIconSvg.setAttribute('height', '24');
            const questionUseElement = document.createElementNS('http://www.w3.org/2000/svg', 'use');
            questionUseElement.setAttribute('href', '#question-icon');
            questionIconSvg.appendChild(questionUseElement);
            questionIconDiv.appendChild(questionIconSvg);

            const questionDiv = document.createElement('div');
            questionDiv.classList.add('question');
            questionDiv.innerHTML = question;

            questionContainer.appendChild(questionIconDiv);
            questionContainer.appendChild(questionDiv);


            // Create the answer container
            const answerContainer = document.createElement('div');
            answerContainer.classList.add('qa-container');

            // Create the answer icon container (Corrected variable name!)
            const answerIconDiv = document.createElement('div');
            answerIconDiv.classList.add('icon');
            const answerIconSvg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
            answerIconSvg.setAttribute('width', '24');
            answerIconSvg.setAttribute('height', '24');
            const answerUseElement = document.createElementNS('http://www.w3.org/2000/svg', 'use');
            answerUseElement.setAttribute('href', '#assistant-icon');
            answerIconSvg.appendChild(answerUseElement);
            answerIconDiv.appendChild(answerIconSvg);

            // Create the answer div
            const answerDiv = document.createElement('div');
            answerDiv.classList.add('answer');
            answerDiv.innerHTML = answer;

            answerContainer.appendChild(answerIconDiv);
            answerContainer.appendChild(answerDiv);

            qaList.appendChild(questionContainer);
            qaList.appendChild(answerContainer);
            qaList.appendChild(document.createElement('hr'));
        }

        function copyCode(id) {
            // Get the code content
            const code = document.getElementById(id).innerText;
             window.chrome.webview.postMessage(code);
        }

        function codeToEditor(id) {
            // Get the code content
            const code = document.getElementById(id).innerText;
             window.chrome.webview.postMessage(String.fromCharCode(31) + code);
        }

        function clearQA() {
            var qaList = document.getElementById('qa-list');
            qaList.replaceChildren();
        }
    </script>
''';

{$ENDREGION 'HTML templates'}

{$REGION 'Utility functions'}

function RemoveCommonIndentation(const Text: string): string;
var
  Trimmed: string;
  MinIndent: Integer;
begin
  // Split the input text into lines
  var Lines := Text.Split([#13#10, #10]);
  if Length(Lines) = 0 then
    Exit(Text.TrimLeft);

  // Find the minimum indentation (number of leading spaces or tabs)
  MinIndent := MaxInt;
  for var Line in Lines do
  begin
    Trimmed := Line.TrimLeft;
    if (Trimmed <> '') and ((Line.Length - Trimmed.Length) < MinIndent) then
      MinIndent := (Line.Length - Trimmed.Length);
  end;

  if MinIndent = 0 then Exit(Text);

  // Remove the common indentation from each line
  for var I := Low(Lines) to High(Lines) do
  begin
    Lines[I] := Copy(Lines[I], MinIndent + 1);
  end;

  // Combine the lines back into a single string
  Result := string.Join(#13#10, Lines);
end;

{$ENDREGION 'Utility functions'}

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
  FMarkdownProcessor.Free;
end;

procedure TLLMChatForm.PythonHighlighterChange(Sender: TObject);
begin
  SynMultiSyn.Schemes[0].MarkerAttri.Foreground :=
    ResourcesDataModule.SynPythonSyn.WhitespaceAttribute.Foreground;
  SynMultiSyn.Schemes[1].MarkerAttri.Foreground :=
    ResourcesDataModule.SynPythonSyn.WhitespaceAttribute.Foreground;

  if FBrowserReady then
  begin
    StyleWebPage;
    LoadBoilerplate;
  end;
end;

procedure TLLMChatForm.ClearConversation;
begin
  FBlockCount := 0;
  EdgeBrowser.ExecuteScript('clearQA()')
end;

procedure TLLMChatForm.CopyToNewEditor(const Code: string);
begin
  var NewEditor := GI_EditorFactory.NewEditor;
  NewEditor.OpenLocalFile('', 'Python');
  NewEditor.SynEdit.Text := Code;
  NewEditor.Activate;
end;

procedure TLLMChatForm.DisplayQA(const Prompt, Answer, Reason: string);
const
  QAScriptCode = '''
  var question = `%s`;
  var answer = `%s`;
  addQA(question, answer);
  Prism.highlightAll();
  window.scroll(0,100000);
  ''';
  ReasonTemplate = '''
  <details>
  <summary><b>Reasoning</b></summary>
  <blockquote>%s</blockquote>
  </details>
  <p>
  ''';
begin
  if not FBrowserReady then Exit;

  var PromptHtml := MarkdownToHTML(Prompt);
  var AnswerHtml := MarkdownToHTML(Answer);

  if Reason <> '' then
  begin
    var ReasonHtml := MarkdownToHTML(Reason).Trim;
    ReasonHtml := Format(ReasonTemplate, [ReasonHtml]);
    AnswerHtml := ReasonHtml + AnswerHtml;
  end;
  EdgeBrowser.ExecuteScript(Format(QAScriptCode, [PromptHtml, AnswerHtml]));
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

  for var QAItem in LLMChat.ActiveTopic.QAItems do
    DisplayQA(QAItem.Prompt, QAItem.Answer, QAItem.Reason);

  if SynQuestion.HandleAllocated and HasFocus then
    synQuestion.SetFocus;
end;

procedure TLLMChatForm.FormCreate(Sender: TObject);
const
  CodeRegEx = '```(\w+)?\s*\n([\s\S]*?)\n?```';
begin
  ImageName := 'Chat\Chat';
  inherited;

  FDefaultLang := 'python';
  FCodeBlocksRE := CompiledRegEx(CodeRegEx);
  FMarkdownProcessor := TMarkdownProcessor.CreateDialect(mdCommonMark);

  EdgeBrowser.UserDataFolder := TPath.Combine(TPyScripterSettings.UserDataPath,
    'WebView2');

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
    StyledMessageDlg(_('Could not read the Chat history'), TMsgDlgType.mtError,
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

  StyleForm;
end;

procedure TLLMChatForm.OnLLMError(Sender: TObject; const Error: string);
begin
  StyledMessageDlg(Error, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
end;

procedure TLLMChatForm.OnLLMResponse(Sender: TObject; const Prompt,
  Answer, Reason: string);
begin
  DisplayQA(Prompt, Answer, Reason);
  synQuestion.Clear;
end;

procedure TLLMChatForm.synQuestionKeyDown(Sender: TObject; var Key: Word; Shift:
    TShiftState);
begin
  if Key = vkReturn then
  begin
    if Shift * [ssShift, ssCtrl] <> [] then
      synQuestion.ExecuteCommand(ecLineBreak, ' ', nil)
    else
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
    else if Sender = spiTemperature then
      Settings.Temperature := NewText.ToSingle
    else if Sender = spiMaxTokens then
      Settings.MaxTokens := NewText.ToInteger
    else if Sender = spiSystemPrompt then
      Settings.SystemPrompt := NewText;

    case LLMChat.Providers.Provider of
      llmProviderOpenAI: LLMChat.Providers.OpenAI := Settings;
      llmProviderDeepSeek: LLMChat.Providers.DeepSeek := Settings;
      llmProviderGrok: LLMChat.Providers.Grok := Settings;
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

procedure TLLMChatForm.actPrintExecute(Sender: TObject);
begin
  EdgeBrowser.ShowPrintUI(TEdgeBrowser.TPrintUIDialogKind.Browser);
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
  actChatNew.Enabled := FBrowserReady and (Length(LLMChat.ActiveTopic.QAItems) > 0);
  actChatNext.Enabled := FBrowserReady and (LLMChat.ActiveTopicIndex < High(LLMChat.ChatTopics));
  actChatPrevious.Enabled := FBrowserReady and (LLMChat.ActiveTopicIndex > 0);
  actAskQuestion.Enabled := FBrowserReady and (LLMChat.ValidateSettings = svValid);

  var IsBusy := LLMChat.IsBusy;
  if aiBusy.Animate <> IsBusy then
    aiBusy.Animate := IsBusy;
  actCancelRequest.Visible := IsBusy;
  actCancelRequest.Enabled := IsBusy;
end;

procedure TLLMChatForm.EdgeBrowserCreateWebViewCompleted(Sender:
    TCustomEdgeBrowser; AResult: HRESULT);
// Also called when the Browser is recreated (style change)
begin
  if AResult <> S_OK then
    StyledMessageDlg(_(SWebView2Error), mtError, [mbOK], 0)
  else
  begin
    FBrowserReady := True;
    StyleWebPage;
    SetBrowserColorScheme;
    LoadBoilerplate;
  end;
end;

procedure TLLMChatForm.EdgeBrowserNavigationCompleted(Sender:
    TCustomEdgeBrowser; IsSuccess: Boolean; WebErrorStatus:
    COREWEBVIEW2_WEB_ERROR_STATUS);
begin
  //  Called after LoadBoireplate loads the basic Web page
  if not IsSuccess then
  begin
    StyledMessageDlg(_(SWebNavigationError), mtError, [mbOK], 0);
    Exit;
  end;
  FBrowserReady := True;
  DisplayActiveChatTopic;
  pnlBrowserCover.Visible := False;
end;

procedure TLLMChatForm.EdgeBrowserWebMessageReceived(Sender:
    TCustomEdgeBrowser; Args: TWebMessageReceivedEventArgs);
var
  ArgsString: PWideChar;
begin
  Args.ArgsInterface.TryGetWebMessageAsString(ArgsString);
  if ArgsString^ = #31 then
  begin
    Inc(ArgsString);
    CopyToNewEditor(ArgsString);
  end
  else
    Clipboard.AsText := ArgsString;
end;

procedure TLLMChatForm.FormShow(Sender: TObject);
begin
  if EdgeBrowser.DefaultInterface = nil then
    TThread.ForceQueue(nil, procedure
    begin
      EdgeBrowser.CreateWebView;
    end);
end;

procedure TLLMChatForm.HighlightCheckedImg(Sender: TObject; ACanvas: TCanvas; State:
    TSpTBXSkinStatesType; const PaintStage: TSpTBXPaintStage; var AImageList:
    TCustomImageList; var AImageIndex: Integer; var ARect: TRect; var
    PaintDefault: Boolean);
begin
  if (PaintStage = pstPrePaint) and (Sender as TSpTBXItem).Checked then
  begin
    ACanvas.Brush.Color := StyleServices.GetSystemColor(clHighlight);
    ACanvas.FillRect(ARect);
  end;
  PaintDefault := True;
end;

procedure TLLMChatForm.mnProviderClick(Sender: TObject);
begin
  if Sender = spiOpenai then
    LLMChat.Providers.Provider := llmProviderOpenAI
  else if Sender = spiDeepSeek then
    LLMChat.Providers.Provider := llmProviderDeepSeek
  else if Sender = spiGrok then
    LLMChat.Providers.Provider := llmProviderGrok
  else if Sender = spiOllama then
    LLMChat.Providers.Provider := llmProviderOllama
  else if Sender = spiGemini then
    LLMChat.Providers.Provider := llmProviderGemini;

  spiSettingsInitPopup(Sender, nil);
  SetQuestionTextHint;
end;

procedure TLLMChatForm.LoadBoilerplate;
// Loads the basic web pages
begin
  NavigateToString(Format(Boilerplate,
    [MainStyleSheet + CodeStyleSheet + QAStyleSheet,
     SvgIcons, '', JSScripts]));
end;

function TLLMChatForm.NavigateToString(Html: string): Boolean;
begin
  if not FBrowserReady then Exit(False);

  FBrowserReady := False;
  EdgeBrowser.NavigateToString(Html);
  Result := True;
end;

function TLLMChatForm.MarkdownToHTML(const MD: string): string;
begin
  Result := '';
  var Matches := FCodeBlocksRE.Matches(MD);
  if Matches.Count > 0 then
  begin
    var CodeEnd := 1;
    for var Match in Matches do
    begin
      var TextBefore := Copy(MD, CodeEnd, Match.Index - CodeEnd);
      if TextBefore <> '' then
        Result := Result + FMarkdownProcessor.process(TextBefore);
      Inc(FBlockCount);
      var Lang := Match.Groups[1].Value;
      var Code := RemoveCommonIndentation(Match.Groups[2].Value);
      Code := HTMLEncode(Code);
      if Lang = 'delphi' then
        Lang := 'pascal';
      var LangId := Lang;
      if Lang = '' then
      begin
        Lang := '&nbsp';
        LangId := FDefaultLang;
      end;
      Result := Result + Format(CodeBlock, [
        Lang,
        SEdCmdCopy,
        FBlockCount.ToString,
        _('Copy code to new editor'),
        LangId,
        Code]);
      CodeEnd := Match.Index + Match.Length;
    end;
    var TextAfter := Copy(MD, CodeEnd);
    if TextAfter <> '' then
      Result := Result + FMarkdownProcessor.process(TextAfter);
  end
  else
    Result := FMarkdownProcessor.process(MD);

  if Result.StartsWith('<p>') then
    Delete(Result, 1, 3);
  // Escape for JavaScript template strings (within backticks)
  Result := Result.Replace('\', '\\');
  Result := Result.Replace('$', '\$');
  Result := Result.Replace('`', '\`');
end;

procedure TLLMChatForm.SetBrowserColorScheme;
var
  Profile: ICoreWebView2Profile;
  Scheme: COREWEBVIEW2_PREFERRED_COLOR_SCHEME;
begin
  if IsStyledWindowsColorDark then
    Scheme := COREWEBVIEW2_PREFERRED_COLOR_SCHEME_DARK
  else
    Scheme := COREWEBVIEW2_PREFERRED_COLOR_SCHEME_LIGHT;
  (EdgeBrowser.DefaultInterface as ICoreWebView2_13).Get_Profile(Profile);
  Profile.Set_PreferredColorScheme(Scheme);
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
    llmProviderDeepSeek: spiDeepSeek.Checked := True;
    llmProviderGrok: spiGrok.Checked := True;
    llmProviderOpenAI: spiOpenai.Checked := True;
    llmProviderGemini: spiGemini.Checked := True;
    llmProviderOllama: spiOllama.Checked := True;
  end;

  var Settings := LLMChat.Settings;
  spiEndpoint.Text := Settings.EndPoint;
  spiModel.Text := Settings.Model;
  spiApiKey.Text := Settings.ApiKey;
  spiTimeout.Text := (Settings.TimeOut div 1000).ToString;
  spiTemperature.Text := Format('%4.2f', [Settings.Temperature]);
  spiMaxTokens.Text := Settings.MaxTokens.ToString;
  spiSystemPrompt.Text := Settings.SystemPrompt;
end;

procedure TLLMChatForm.StyleForm;
begin
  synQuestion.Font.Color := StyleServices.GetSystemColor(clWindowText);
  synQuestion.Color := StyleServices.GetSystemColor(clWindow);
  {$IF CompilerVersion >= 36}
  aiBusy.IndicatorColor := aicCustom;
  aiBusy.IndicatorCustomColor := StyleServices.GetSystemColor(clWindowText);
  {$ENDIF};
end;

procedure TLLMChatForm.StyleWebPage;
var
  LinkColor: TColor;
  TextColor: TColor;
  CodeHeaderBkg, CodeHeaderFg: string;
  ThumbColor, ThumbHoverColor: string;
begin
  if IsStyledWindowsColorDark then
  begin
    LinkColor :=  TColors.LightBlue;
    CodeHeaderBkg := '#2d2d2d';
    CodeHeaderFg := '#f4f4f4';
    ThumbColor := '#666';
    ThumbHoverColor := '#888';
  end
  else
  begin
    LinkColor := clBlue;
    CodeHeaderBkg := '#f4f4f4';
    CodeHeaderFg := '#333';
    ThumbColor := '#ccc';
    ThumbHoverColor := '#999';
  end;

  // Style the main sheet
  MainStyleSheet := Format(MainStyleSheetTemplate, [
    ColorToHtml(StyleServices.GetSystemColor(clWindow)),
    ThumbColor,
    ThumbHoverColor,
    ColorToHtml(StyleServices.GetSystemColor(clWindowText)),
    ColorToHtml(LinkColor)]);


  // style the display of code to make it compatible with the PyScripter Editor
  with ResourcesDataModule do
  begin
    if IsColorDark(SynPythonSyn.WhitespaceAttribute.Background) then
      TextColor := DarkenColor(clWhite, 10)
    else
      TextColor := LightenColor(clBlack, 10);

    CodeStyleSheet := Format(CodeStyleSheetTemplate,[
        CodeHeaderBkg,
        CodeHeaderFg,
        ColorToHtml(SynPythonSyn.WhitespaceAttribute.Background),
        ColorToHtml(SynPythonSyn.CommentAttri.Foreground),
        ColorToHtml(SynPythonSyn.SymbolAttri.Foreground),
        ColorToHtml(SynWebEngine.MLTagKeyAttri.Foreground),
        ColorToHtml(SynWebEngine.MLTagNameAttri.Foreground),
        ColorToHtml(SynPythonSyn.FunctionNameAttri.Foreground),
        ColorToHtml(SynPythonSyn.NumberAttri.Foreground),
        ColorToHtml(SynPythonSyn.ClassNameAttri.Foreground),
        ColorToHtml(SynPythonSyn.KeyAttri.Foreground),
        ColorToHtml(SynPythonSyn.StringAttri.Foreground),
        ColorToHtml(SynPythonSyn.MultiLineStringAttri.Foreground),
        ColorToHtml(SynWebEngine.MLEscapeAttri.Foreground),
        ColorToHtml(TextColor)
      ]);
  end;
end;

procedure TLLMChatForm.synQuestionEnter(Sender: TObject);
begin
  // Spell Checking
  CommandsDataModule.SynSpellCheck.Editor := synQuestion;
end;

procedure TLLMChatForm.WMDestroy(var Message: TWMDestroy);
begin
  // So that it is not automatically recreated
  EdgeBrowser.CloseWebView;
  FBrowserReady := False;
  pnlBrowserCover.Visible := True;
  inherited;
end;

procedure TLLMChatForm.WMSpSkinChange(var Message: TMessage);
begin
  inherited;
  StyleForm;
end;

end.
