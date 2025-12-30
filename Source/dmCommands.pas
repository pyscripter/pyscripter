{-----------------------------------------------------------------------------
 Unit Name: dmCommands
 Author:    Kiriakos Vlahos
 Date:      09-Mar-2005
 Purpose:   Data Module of PyScripter
 History:
-----------------------------------------------------------------------------}

unit dmCommands;

interface

uses
  System.Types,
  System.Classes,
  System.Actions,
  System.Messaging,
  Vcl.Graphics,
  Vcl.ActnList,
  Vcl.StdActns,
  Vcl.Menus,
  Vcl.ExtActns,
  Vcl.ImgList,
  SynEdit,
  SynEditPrint,
  SynEditRegexSearch,
  SynEditHighlighter,
  SynCompletionProposal,
  SynEditMiscClasses,
  SynEditSearch,
  SynHighlighterGeneral,
  JvComponentBase,
  JvProgramVersionCheck,
  JvPropertyStore,
  TB2Item,
  SpTBXSkins,
  SpTBXItem,
  SpTBXEditors,
  VirtualExplorerTree,
  VirtualShellNotifier,
  uEditAppIntfs,
  SynSpellCheck;

type
  TSynGeneralSyn = class(SynHighlighterGeneral.TSynGeneralSyn)
  public
    class function GetFriendlyLanguageName: string; override;
  end;

  TCommandsDataModule = class(TDataModule)
    SynEditPrint: TSynEditPrint;
    SynEditSearch: TSynEditSearch;
    SynEditRegexSearch: TSynEditRegexSearch;
    ProgramVersionCheck: TJvProgramVersionCheck;
    ProgramVersionHTTPLocation: TJvProgramVersionHTTPLocation;
    actlMain: TActionList;
    actPythonManuals: THelpContents;
    actHelpContents: THelpContents;
    actEditSelectAll: TEditSelectAll;
    actEditUndo: TEditUndo;
    actEditDelete: TEditDelete;
    actEditPaste: TEditPaste;
    actEditCopy: TEditCopy;
    actEditCut: TEditCut;
    actFileCloseAllOther: TAction;
    actSearchGoToDebugLine: TAction;
    actEditWordWrap: TAction;
    actSearchHighlight: TAction;
    actSearchReplaceNow: TAction;
    actExportHighlighters: TAction;
    actImportHighlighters: TAction;
    actExportShortCuts: TAction;
    actImportShortcuts: TAction;
    actFileReload: TAction;
    actEditUTF16BE: TAction;
    actEditUTF16LE: TAction;
    actEditUTF8NoBOM: TAction;
    actEditUTF8: TAction;
    actFileTemplates: TAction;
    actEditToggleComment: TAction;
    actInterpreterEditorOptions: TAction;
    actUnitTestWizard: TAction;
    actCheckForUpdates: TAction;
    actHelpEditorShortcuts: TAction;
    actEditAnsi: TAction;
    actEditLBMac: TAction;
    actEditLBUnix: TAction;
    actEditLBDos: TAction;
    actFindNextReference: TAction;
    actFindPreviousReference: TAction;
    actEditShowSpecialChars: TAction;
    actEditLineNumbers: TAction;
    actFindFunction: TAction;
    actHelpExternalTools: TAction;
    actConfigureTools: TAction;
    actCodeTemplates: TAction;
    actIDEShortcuts: TAction;
    actCustomizeParameters: TAction;
    actInsertTemplate: TAction;
    actHelpParameters: TAction;
    actReplaceParameters: TAction;
    actModifierCompletion: TAction;
    actParameterCompletion: TAction;
    actFindInFiles: TAction;
    actSearchGoToSyntaxError: TAction;
    actSearchGoToLine: TAction;
    actAbout: TAction;
    actPythonPath: TAction;
    actEditUntabify: TAction;
    actEditTabify: TAction;
    actSearchMatchingBrace: TAction;
    actEditUncomment: TAction;
    actEditCommentOut: TAction;
    actEditDedent: TAction;
    actEditIndent: TAction;
    actIDEOptions: TAction;
    actEditorOptions: TAction;
    actPageSetup: TAction;
    actPrintPreview: TAction;
    actPrinterSetup: TAction;
    actFilePrint: TAction;
    actFileSaveAll: TAction;
    actSearchReplace: TAction;
    actSearchFindPrev: TAction;
    actSearchFindNext: TAction;
    actSearchFind: TAction;
    actFileClose: TAction;
    actFileSaveAs: TAction;
    actFileSave: TAction;
    actEditCopyFileName: TAction;
    actToolsEditStartupScripts: TAction;
    actFoldVisible: TAction;
    actFoldAll: TAction;
    actUnfoldAll: TAction;
    actFoldNearest: TAction;
    actUnfoldNearest: TAction;
    actFoldRegions: TAction;
    actUnfoldRegions: TAction;
    actFoldLevel1: TAction;
    actUnfoldLevel1: TAction;
    actFoldLevel2: TAction;
    actUnfoldLevel2: TAction;
    actFoldLevel3: TAction;
    actUnfoldLevel3: TAction;
    actFoldClasses: TAction;
    actUnfoldClasses: TAction;
    actFoldFunctions: TAction;
    actUnfoldFunctions: TAction;
    actFileCloseAllToTheRight: TAction;
    actEditReadOnly: TAction;
    actFileSaveToRemote: TAction;
    SynWebCompletion: TSynCompletionProposal;
    SynParamCompletion: TSynCompletionProposal;
    SynCodeCompletion: TSynCompletionProposal;
    actToolsRestartLS: TAction;
    SynSpellCheck: TSynSpellCheck;
    actSynSpellCheckFile: TSynSpellCheckFile;
    actSynSpellCheckLine: TSynSpellCheckLine;
    actSynSpellCheckSelection: TSynSpellCheckSelection;
    actSynSpellCheckWord: TSynSpellCheckWord;
    actSynSpellClearErrors: TSynSpellClearErrors;
    actSynSpellCheckAsYouType: TSynSpellCheckAsYouType;
    actSynSpellErrorAdd: TSynSpellErrorAdd;
    actSynSpellErrorIgnoreOnce: TSynSpellErrorIgnoreOnce;
    actSynSpellErrorIgnore: TSynSpellErrorIgnore;
    actSynSpellErrorDelete: TSynSpellErrorDelete;
    pmSpelling: TSpTBXPopupMenu;
    mnSpelling: TSpTBXSubmenuItem;
    mnSpellCheckTopSeparator: TSpTBXSeparatorItem;
    mnSpellCheckAdd: TSpTBXItem;
    mnSpellCheckDelete: TSpTBXItem;
    mnSpellCheckIgnore: TSpTBXItem;
    mnSpellCheckIgnoreOnce: TSpTBXItem;
    mnSpellCheckSecondSeparator: TSpTBXSeparatorItem;
    SpTBXItem20: TSpTBXItem;
    SpTBXItem21: TSpTBXItem;
    SpTBXItem22: TSpTBXItem;
    SpTBXItem23: TSpTBXItem;
    SpTBXSeparatorItem24: TSpTBXSeparatorItem;
    SpTBXItem24: TSpTBXItem;
    SpTBXSeparatorItem25: TSpTBXSeparatorItem;
    SpTBXItem25: TSpTBXItem;
    pmAssistant: TSpTBXPopupMenu;
    spiAssistant: TSpTBXSubmenuItem;
    spiSettings: TSpTBXSubmenuItem;
    spiGemini: TSpTBXItem;
    spiOllama: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    spiEndpoint: TSpTBXEditItem;
    spiModel: TSpTBXEditItem;
    spiApiKey: TSpTBXEditItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    spiTimeout: TSpTBXEditItem;
    spiMaxTokens: TSpTBXEditItem;
    spiSystemPrompt: TSpTBXEditItem;
    actAssistantSuggest: TAction;
    SpTBXSeparatorItem3: TSpTBXSeparatorItem;
    spiSuggest: TSpTBXItem;
    actAssistantCancel: TAction;
    spiAssistantCancel: TSpTBXItem;
    SpTBXSeparatorItem4: TSpTBXSeparatorItem;
    actAssistantOptimize: TAction;
    spiOptimize: TSpTBXItem;
    actAssistantFixBugs: TAction;
    spiFixBugs: TSpTBXItem;
    actAssistantExplain: TAction;
    spiAssistantExplain: TSpTBXItem;
    spiOpenAI: TSpTBXItem;
    actHelpWebBlog: TBrowseURL;
    actHelpWebGroupSupport: TBrowseURL;
    actHelpWebProjectHome: TBrowseURL;
    actDonate: TBrowseURL;
    spiTemperature: TSpTBXEditItem;
    spiDeepSeek: TSpTBXItem;
    spiGrok: TSpTBXItem;
    actEditRedo: TSynEditRedo;
    actFormatCode: TAction;
    actCodeCheck: TAction;
    actClearIssues: TAction;
    actNextIssue: TAction;
    actPreviousIssue: TAction;
    actFixAll: TAction;
    actOrganizeImports: TAction;
    actRefactorRename: TAction;
    actCodeAction: TAction;
    actShowRefactorMenu: TAction;
    actFileExit: TAction;
    actProjectNew: TAction;
    actProjectOpen: TAction;
    actProjectSave: TAction;
    actProjectSaveAs: TAction;
    actNavFindResults: TAction;
    actNavWatches: TAction;
    actNavBreakpoints: TAction;
    actNavInterpreter: TAction;
    actNavVariables: TAction;
    actNavCallStack: TAction;
    actNavMessages: TAction;
    actNavFileExplorer: TAction;
    actNavCodeExplorer: TAction;
    actNavTodo: TAction;
    actNavUnitTests: TAction;
    actNavOutput: TAction;
    actNavEditor: TAction;
    actNavProjectExplorer: TAction;
    actNavRegExp: TAction;
    actNavChat: TAction;
    actCommandLine: TAction;
    actToggleBreakPoint: TAction;
    actClearAllBreakpoints: TAction;
    actImportModule: TAction;
    actPythonInternal: TAction;
    actPythonRemote: TAction;
    actPythonRemoteTk: TAction;
    actPythonRemoteWx: TAction;
    actPythonSSH: TAction;
    actPythonReinitialize: TAction;
    actPythonFreeThreaded: TAction;
    actAddWatchAtCursor: TAction;
    actPostMortem: TAction;
    actRun: TAction;
    actDebug: TAction;
    actExternalRun: TAction;
    actRunToCursor: TAction;
    actStepInto: TAction;
    actStepOver: TAction;
    actStepOut: TAction;
    actDebugPause: TAction;
    actDebugAbort: TAction;
    actExternalRunConfigure: TAction;
    actRunLastScript: TAction;
    actDebugLastScript: TAction;
    actExternalRunLastScript: TAction;
    actExecSelection: TAction;
    actPythonSetup: TAction;
    function ProgramVersionHTTPLocationLoadFileFromRemote(
      AProgramVersionLocation: TJvProgramVersionHTTPLocation; const ARemotePath,
      ARemoteFileName, ALocalPath, ALocalFileName: string): string;
    procedure actCheckForUpdatesExecute(Sender: TObject);
    procedure actUnitTestWizardExecute(Sender: TObject);
    procedure actIDEShortcutsExecute(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure actFilePrintExecute(Sender: TObject);
    procedure actFileCloseExecute(Sender: TObject);
    procedure actSearchFindExecute(Sender: TObject);
    procedure actSearchFindNextExecute(Sender: TObject);
    procedure actSearchFindPrevExecute(Sender: TObject);
    procedure actSearchReplaceExecute(Sender: TObject);
    procedure actFileSaveAllExecute(Sender: TObject);
    procedure actPrinterSetupExecute(Sender: TObject);
    procedure actPageSetupExecute(Sender: TObject);
    procedure actPrintPreviewExecute(Sender: TObject);
    procedure actEditorOptionsExecute(Sender: TObject);
    procedure actEditIndentExecute(Sender: TObject);
    procedure actEditDedentExecute(Sender: TObject);
    procedure actEditCommentOutExecute(Sender: TObject);
    procedure actEditUncommentExecute(Sender: TObject);
    procedure actSearchMatchingBraceExecute(Sender: TObject);
    procedure actEditTabifyExecute(Sender: TObject);
    procedure actEditUntabifyExecute(Sender: TObject);
    procedure actIDEOptionsExecute(Sender: TObject);
    procedure actPythonPathExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actAddWatchAtCursorExecute(Sender: TObject);
    procedure actAssistantExplainExecute(Sender: TObject);
    procedure actAssistantCancelExecute(Sender: TObject);
    procedure actAssistantFixBugsExecute(Sender: TObject);
    procedure actAssistantOptimizeExecute(Sender: TObject);
    procedure actAssistantSuggestExecute(Sender: TObject);
    procedure actClearAllBreakpointsExecute(Sender: TObject);
    procedure UpdateAssistantActions(Sender: TObject);
    procedure actClearIssuesExecute(Sender: TObject);
    procedure actCodeActionExecute(Sender: TObject);
    procedure actCodeCheckExecute(Sender: TObject);
    procedure actPythonManualsExecute(Sender: TObject);
    procedure actSearchGoToLineExecute(Sender: TObject);
    procedure actFindInFilesExecute(Sender: TObject);
    procedure actHelpContentsExecute(Sender: TObject);
    procedure ParameterCompletionCodeCompletion(Sender: TObject;
      var Value: string; Shift: TShiftState; Index: Integer;
      EndToken: WideChar);
    procedure actParameterCompletionExecute(Sender: TObject);
    procedure actModifierCompletionExecute(Sender: TObject);
    procedure actReplaceParametersExecute(Sender: TObject);
    procedure actHelpParametersExecute(Sender: TObject);
    procedure actInsertTemplateExecute(Sender: TObject);
    procedure actCustomizeParametersExecute(Sender: TObject);
    procedure actCodeTemplatesExecute(Sender: TObject);
    procedure actCommandLineExecute(Sender: TObject);
    procedure actConfigureToolsExecute(Sender: TObject);
    procedure actDebugAbortExecute(Sender: TObject);
    procedure actDebugExecute(Sender: TObject);
    procedure actDebugPauseExecute(Sender: TObject);
    procedure actHelpExternalToolsExecute(Sender: TObject);
    procedure actFindFunctionExecute(Sender: TObject);
    procedure actEditLineNumbersExecute(Sender: TObject);
    procedure actEditShowSpecialCharsExecute(Sender: TObject);
    procedure actFindNextReferenceExecute(Sender: TObject);
    procedure actEditLBExecute(Sender: TObject);
    procedure actHelpEditorShortcutsExecute(Sender: TObject);
    procedure actInterpreterEditorOptionsExecute(Sender: TObject);
    procedure actEditToggleCommentExecute(Sender: TObject);
    procedure actFileTemplatesExecute(Sender: TObject);
    procedure actEditFileEncodingExecute(Sender: TObject);
    procedure actFileReloadExecute(Sender: TObject);
    procedure actExportShortCutsExecute(Sender: TObject);
    procedure actImportShortcutsExecute(Sender: TObject);
    procedure actExportHighlightersExecute(Sender: TObject);
    procedure actImportHighlightersExecute(Sender: TObject);
    procedure actSearchReplaceNowExecute(Sender: TObject);
    procedure actSearchGoToSyntaxErrorExecute(Sender: TObject);
    procedure actSearchHighlightExecute(Sender: TObject);
    procedure actEditWordWrapExecute(Sender: TObject);
    procedure actSearchGoToDebugLineExecute(Sender: TObject);
    procedure actFileCloseWorkspaceTabsExecute(Sender: TObject);
    procedure actEditCopyFileNameExecute(Sender: TObject);
    procedure actToolsEditStartupScriptsExecute(Sender: TObject);
    procedure actFoldVisibleExecute(Sender: TObject);
    procedure actFoldAllExecute(Sender: TObject);
    procedure actUnfoldAllExecute(Sender: TObject);
    procedure actFoldNearestExecute(Sender: TObject);
    procedure actUnfoldNearestExecute(Sender: TObject);
    procedure actUnfoldRegionsExecute(Sender: TObject);
    procedure actFoldRegionsExecute(Sender: TObject);
    procedure actUnfoldLevel1Execute(Sender: TObject);
    procedure actFoldLevel1Execute(Sender: TObject);
    procedure actFoldLevel2Execute(Sender: TObject);
    procedure actUnfoldLevel2Execute(Sender: TObject);
    procedure actFoldLevel3Execute(Sender: TObject);
    procedure actUnfoldLevel3Execute(Sender: TObject);
    procedure actFoldClassesExecute(Sender: TObject);
    procedure actUnfoldClassesExecute(Sender: TObject);
    procedure actFoldFunctionsExecute(Sender: TObject);
    procedure actUnfoldFunctionsExecute(Sender: TObject);
    procedure actEditReadOnlyExecute(Sender: TObject);
    procedure actExternalRunConfigureExecute(Sender: TObject);
    procedure actExternalRunExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actFileSaveToRemoteExecute(Sender: TObject);
    procedure actFixAllExecute(Sender: TObject);
    procedure actFormatCodeExecute(Sender: TObject);
    procedure actImportModuleExecute(Sender: TObject);
    procedure actNavEditorExecute(Sender: TObject);
    procedure actNavigateToDockWindow(Sender: TObject);
    procedure actNextIssueExecute(Sender: TObject);
    procedure actOrganizeImportsExecute(Sender: TObject);
    procedure actPostMortemExecute(Sender: TObject);
    procedure actPreviousIssueExecute(Sender: TObject);
    procedure actProjectNewExecute(Sender: TObject);
    procedure actProjectOpenExecute(Sender: TObject);
    procedure actProjectSaveAsExecute(Sender: TObject);
    procedure actProjectSaveExecute(Sender: TObject);
    procedure actPythonEngineExecute(Sender: TObject);
    procedure actPythonFreeThreadedExecute(Sender: TObject);
    procedure actPythonReinitializeExecute(Sender: TObject);
    procedure actRefactorRenameExecute(Sender: TObject);
    procedure actDebugLastScriptExecute(Sender: TObject);
    procedure actExecSelectionExecute(Sender: TObject);
    procedure actRunExecute(Sender: TObject);
    procedure actRunLastScriptExecute(Sender: TObject);
    procedure actExternalRunLastScriptExecute(Sender: TObject);
    procedure actPythonSetupExecute(Sender: TObject);
    procedure actRunToCursorExecute(Sender: TObject);
    procedure actShowRefactorMenuExecute(Sender: TObject);
    procedure actStepIntoExecute(Sender: TObject);
    procedure actStepOutExecute(Sender: TObject);
    procedure actStepOverExecute(Sender: TObject);
    procedure actToggleBreakPointExecute(Sender: TObject);
    procedure actToolsRestartLSExecute(Sender: TObject);
    procedure UpdateActionAlwaysEnabled(Sender: TObject);
    procedure HighlightCheckedImg(Sender: TObject; ACanvas: TCanvas; State:
        TSpTBXSkinStatesType; const PaintStage: TSpTBXPaintStage; var AImageList:
        TCustomImageList; var AImageIndex: Integer; var ARect: TRect; var
        PaintDefault: Boolean);
    procedure UpdateLineBreakActions(Sender: TObject);
    procedure mnProviderClick(Sender: TObject);
    procedure mnSpellingPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure UpdateEditActions(Sender: TObject);
    procedure UpdateCodeFoldingActions(Sender: TObject);
    procedure spiAcceptSettings(Sender: TObject; var NewText: string; var
        Accept: Boolean);
    procedure spiSettingsInitPopup(Sender: TObject; PopupView: TTBView);
    procedure SynSpellCheckChange(Sender: TObject);
    procedure UpdateBreakpointActions(Sender: TObject);
    procedure UpdateEncodingActions(Sender: TObject);
    procedure UpdateFileActions(Sender: TObject);
    procedure UpdateIssuesActions(Sender: TObject);
    procedure UpdateParameterActions(Sender: TObject);
    procedure UpdatePythonEngineActions(Sender: TObject);
    procedure UpdateRefactorActions(Sender: TObject);
    procedure UpdateRunActions(Sender: TObject);
    procedure UpdateSearchActions(Sender: TObject);
    procedure UpdateSourceCodeActions(Sender: TObject);
    procedure UpdateToolsActions(Sender: TObject);
  private
    FConfirmReplaceDialogRect: TRect;
    procedure PyIDEOptionsChanged(const Sender: TObject; const Msg:
        System.Messaging.TMessage);
    procedure DebugActiveScript(ActiveEditor: IEditor;
      InitStepIn: Boolean = False; RunToCursorLine: Integer = -1);
  protected
    procedure Loaded; override;
  public
    procedure AutoCheckForUpdates;
    procedure SynEditOptionsDialogGetHighlighterCount(Sender: TObject;
                var Count: Integer);
    procedure SynEditOptionsDialogGetHighlighter(Sender: TObject;
                Index: Integer; var SynHighlighter: TSynCustomHighlighter);
    procedure SynEditOptionsDialogSetHighlighter(Sender: TObject;
                Index: Integer; SynHighlighter: TSynCustomHighlighter);
    procedure SynInterpreterOptionsDialogGetHighlighterCount(Sender: TObject;
                var Count: Integer);
    procedure SynInterpreterOptionsDialogGetHighlighter(Sender: TObject;
                Index: Integer; var SynHighlighter: TSynCustomHighlighter);
    procedure SynInterpreterOptionsDialogSetHighlighter(Sender: TObject;
                Index: Integer; SynHighlighter: TSynCustomHighlighter);
    function ShowPythonKeywordHelp(KeyWord: string): Boolean;
    procedure GetEditorUserCommand(AUserCommand: Integer; var ADescription: string);
    procedure GetEditorAllUserCommands(ACommands: TStrings);
    function DoSearchReplaceText(SynEdit: TSynEdit;
      AReplace, ABackwards: Boolean ; IsIncremental: Boolean = False): Integer;
    procedure ShowSearchReplaceDialog(SynEdit: TSynEdit; AReplace: Boolean);
    procedure SynEditReplaceText(Sender: TObject; const ASearch,
      AReplace: string; Line, Column: Integer; var Action: TSynReplaceAction);
    procedure IncrementalSearch;
    procedure ApplyEditorOptions;
    function FindSearchTarget: ISearchCommands;
    procedure HighlightWordInActiveEditor(SearchWord: string);
    class procedure RegisterActionList(ActionList: TActionList);
  end;

var
  CommandsDataModule: TCommandsDataModule = nil;

implementation

{$R *.DFM}

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ShlObj,
  Winapi.ShellAPI,
  Winapi.ActiveX,
  System.UITypes,
  System.SysUtils,
  System.Win.Registry,
  System.StrUtils,
  System.DateUtils,
  System.IOUtils,
  System.IniFiles,
  System.Math,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Clipbrd,
  Vcl.BaseImageCollection,
  Vcl.Themes,
  MPShellUtilities,
  MPCommonUtilities,
  SpTBXMDIMRU,
  SpTBXTabs,
  PythonEngine,
  JclDebug,
  JclShell,
  JvStringHolder,
  JvAppIniStorage,
  JvAppStorage,
  JvDSADialogs,
  JvDynControlEngineVCL,
  JvGnugettext,
  SynEditTypes,
  SynHighlighterPython,
  SynEditTextBuffer,
  SynEditKeyCmds,
  SynEditMiscProcs,
  SVGIconImage,
  XLSPTypes,
  dmResources,
  StringResources,
  uPythonItfs,
  dlgCodeTemplates,
  dlgCollectionEditor,
  dlgDirectoryList,
  dlgAboutPyScripter,
  dlgCommandLine,
  dlgConfirmReplace,
  dlgCustomParams,
  dlgCustomShortcuts,
  dlgFileTemplates,
  dlgPickList,
  dlgPythonVersions,
  dlgSynPageSetup,
  dlgToolProperties,
  dlgUnitTestWizard,
  frmPythonII,
  frmPyIDEMain,
  frmEditor,
  frmIDEDockWin,
  frmFindResults,
  frmFunctionList,
  cLspClients,
  uParams,
  uCommonFunctions,
  uSearchHighlighter,
  uLLMSupport,
  cTools,
  cPySupportTypes,
  cPyScripterSettings,
  cParameters,
  cSSHSupport,
  dlgSynEditOptions,
  dlgOptionsEditor;

{ TCommandsDataModule }

procedure TCommandsDataModule.DataModuleCreate(Sender: TObject);
begin
  // Set the language before all calls to TranslateComponent
  // This avoids retranslating if the local lanuage <> "en"
  // and the stored lanuage option is "en".
  UseLanguage('en');
  TranslateComponent(Self);
  RegisterActionList(actlMain);

  TMessageManager.DefaultManager.SubscribeToMessage(TIDEOptionsChangedMessage,
    PyIDEOptionsChanged);
end;

procedure TCommandsDataModule.DataModuleDestroy(Sender: TObject);
begin
  CommandsDataModule := nil;
  TMessageManager.DefaultManager.Unsubscribe(TIDEOptionsChangedMessage,
    PyIDEOptionsChanged);
end;

procedure TCommandsDataModule.DebugActiveScript(ActiveEditor: IEditor;
  InitStepIn: Boolean; RunToCursorLine: Integer);
begin
  Assert(GI_PyControl.Inactive, 'DebugActiveScript');
  var RunConfig :=
    TSmartPtr.Make(TRunConfiguration.CreateFromFileId(ActiveEditor.FileId))();
  GI_PyControl.Debug(RunConfig, InitStepIn, RunToCursorLine);
end;

{$REGION 'Highlighter methods'}

procedure TCommandsDataModule.SynEditOptionsDialogGetHighlighterCount(Sender: TObject;
  var Count: Integer);
begin
   Count := ResourcesDataModule.Highlighters.Count + 1; // The last one is the Python Interpreter
end;

procedure TCommandsDataModule.SynEditOptionsDialogGetHighlighter(Sender: TObject;
  Index: Integer; var SynHighlighter: TSynCustomHighlighter);
begin
   if (Index >= 0) and (Index < ResourcesDataModule.Highlighters.Count) then
      SynHighlighter := ResourcesDataModule.Highlighters[Index]
   else if Index = ResourcesDataModule.Highlighters.Count then
     SynHighlighter := GI_PyInterpreter.Editor.Highlighter
   else
     SynHighlighter := nil;
end;

procedure TCommandsDataModule.SynEditOptionsDialogSetHighlighter(Sender: TObject;
  Index: Integer; SynHighlighter: TSynCustomHighlighter);
begin
   if (Index >= 0) and (Index < ResourcesDataModule.Highlighters.Count) then
     ResourcesDataModule.Highlighters[Index].Assign(SynHighlighter)
   else if Index = ResourcesDataModule.Highlighters.Count then
     GI_PyInterpreter.Editor.Highlighter.Assign(SynHighlighter);
end;

procedure TCommandsDataModule.SynInterpreterOptionsDialogGetHighlighter(
  Sender: TObject; Index: Integer; var SynHighlighter: TSynCustomHighlighter);
begin
  if Index = 0 then
    SynHighlighter := GI_PyInterpreter.Editor.Highlighter
  else
    SynHighlighter := nil;
end;

procedure TCommandsDataModule.SynInterpreterOptionsDialogGetHighlighterCount(
  Sender: TObject; var Count: Integer);
begin
  Count := 1;
end;

procedure TCommandsDataModule.SynInterpreterOptionsDialogSetHighlighter(
  Sender: TObject; Index: Integer; SynHighlighter: TSynCustomHighlighter);
begin
  if Index = 0 then
    GI_PyInterpreter.Editor.Highlighter.Assign(SynHighlighter);
end;

procedure TCommandsDataModule.HighlightWordInActiveEditor(SearchWord: string);
var
  OldWholeWords: Boolean;
  OldSearchText: string;
begin
  EditorSearchOptions.InitSearch;
  OldWholeWords := EditorSearchOptions.SearchWholeWords;
  OldSearchText := EditorSearchOptions.SearchText;

  EditorSearchOptions.SearchWholeWords := True;
  EditorSearchOptions.SearchText := SearchWord;
  actSearchHighlight.Checked := True;
  actSearchHighlightExecute(Self);

  EditorSearchOptions.SearchWholeWords := OldWholeWords;
  EditorSearchOptions.SearchText := OldSearchText;
end;

{$ENDREGION 'Highlighter methods'}


procedure TCommandsDataModule.Loaded;
begin
  inherited;
  // SynEditPrint
  with SynEditPrint do begin
    Font.Name := DefaultCodeFontName;
    Font.Size := 10;
    with Header do begin
      Add('$TITLE$', nil, taCenter, 2);
      DefaultFont.Size := 10;
    end;
    with Footer do begin
      Add('$PAGENUM$/$PAGECOUNT$', nil, taCenter, 1);
      DefaultFont.Size := 10;
    end;
  end;

  //Program Version Check
  ProgramVersionCheck.ThreadDialog.DialogOptions.ShowDialog := False;
  {$IFDEF CPUX64}
  ProgramVersionHTTPLocation.VersionInfoFileName := 'PyScripterVersionInfo-x64.ini';
  {$ELSE}
  ProgramVersionHTTPLocation.VersionInfoFileName := 'PyScripterVersionInfo.ini';
  {$ENDIF}
end;

procedure TCommandsDataModule.ApplyEditorOptions;
// Assign Editor Options to all open editors
begin
  GI_EditorFactory.ApplyToEditors(procedure(Editor: IEditor)
  begin
    Editor.ApplyEditorOptions(EditorOptions);
  end);
end;

procedure TCommandsDataModule.AutoCheckForUpdates;
begin
  if PyIDEOptions.AutoCheckForUpdates then
  begin
    var DateLastCheckedForUpdates := GI_PyIDEServices.AppStorage.ReadDateTime(
      'Date checked for updates', MinDateTime);
    if (DaysBetween(Now, DateLastCheckedForUpdates) >=
      PyIDEOptions.DaysBetweenChecks) and ConnectedToInternet
    then
      actCheckForUpdatesExecute(nil);  // nil so that we get no confirmation
  end;
end;


{$REGION 'Action Execute Handlers'}

procedure TCommandsDataModule.actFileSaveExecute(Sender: TObject);
begin
  if GI_FileCmds <> nil then
    GI_FileCmds.ExecSave;
end;

procedure TCommandsDataModule.actFileSaveToRemoteExecute(Sender: TObject);
begin
  if GI_FileCmds <> nil then
    GI_FileCmds.ExecSaveAsRemote;
end;

procedure TCommandsDataModule.actFileSaveAsExecute(Sender: TObject);
begin
  if GI_FileCmds <> nil then
    GI_FileCmds.ExecSaveAs;
end;

procedure TCommandsDataModule.actFileSaveAllExecute(Sender: TObject);
begin
  GI_EditorFactory.ApplyToEditors(procedure(Editor: IEditor)
  begin
    var FileCommands := Editor as IFileCommands;
    if Assigned(FileCommands) and FileCommands.CanSave then
      FileCommands.ExecSave;
  end);
end;

procedure TCommandsDataModule.actFilePrintExecute(Sender: TObject);
begin
  if GI_FileCmds <> nil then
    GI_FileCmds.ExecPrint;
end;

procedure TCommandsDataModule.actFileReloadExecute(Sender: TObject);
begin
  if GI_FileCmds <> nil then
    GI_FileCmds.ExecReload;
end;

procedure TCommandsDataModule.actPrintPreviewExecute(Sender: TObject);
begin
  if GI_FileCmds <> nil then
    GI_FileCmds.ExecPrintPreview;
end;

procedure TCommandsDataModule.actPrinterSetupExecute(Sender: TObject);
begin
  with TPrinterSetupDialog.Create(Self) do
  begin
    Execute;
    Free;
  end;
end;

procedure TCommandsDataModule.actPageSetupExecute(Sender: TObject);
begin
  with TPageSetupDlg.Create(Self) do begin
    SetValues(SynEditPrint);
    if ShowModal = mrOk then
      GetValues(SynEditPrint);
    Release;
  end;
end;

procedure TCommandsDataModule.actFileCloseWorkspaceTabsExecute(Sender: TObject);

  procedure WorkspaceCloseOtherTabs(Editor: IEditor; Backwards: Boolean);
  var
    NextTab: TSpTBXTabItem;
    NextEditor: IEditor;
  begin
    repeat
      NextTab := TEditorForm(Editor.Form).ParentTabItem.GetNextTab(not Backwards, sivtNormal);
      if Assigned(NextTab) then
      begin
        NextEditor := PyIDEMainForm.EditorFromTab(NextTab);
        if Assigned(NextEditor) then
          if NextEditor.AskSaveChanges then
            NextEditor.Close
          else
            Break;
      end;
    until not Assigned(NextTab);
  end;

begin
  var Editor := GI_PyIDEServices.ActiveEditor;
  if not Assigned(Editor) then Exit;

  WorkspaceCloseOtherTabs(Editor, False);
  if (Sender as TBasicAction).Tag > 0 then
    WorkspaceCloseOtherTabs(Editor, True);

  Editor.Activate;
end;

procedure TCommandsDataModule.actFileCloseExecute(Sender: TObject);
begin
  var Editor := GI_PyIDEServices.ActiveEditor;
  if Assigned(Editor) then
    (Editor as IFileCommands).ExecClose;
end;

procedure TCommandsDataModule.actEditReadOnlyExecute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    GI_ActiveEditor.ReadOnly := not GI_ActiveEditor.ReadOnly;
end;

procedure TCommandsDataModule.actSearchFindExecute(Sender: TObject);
begin
  var SearchCommands := FindSearchTarget;
  if SearchCommands <> nil then
    SearchCommands.ExecFind;
end;

procedure TCommandsDataModule.actSearchFindNextExecute(Sender: TObject);
begin
  var SearchCommands := FindSearchTarget;
  if SearchCommands <> nil then
    SearchCommands.ExecFindNext;
end;

procedure TCommandsDataModule.actSearchFindPrevExecute(Sender: TObject);
begin
  var SearchCommands := FindSearchTarget;
  if SearchCommands <> nil then
    SearchCommands.ExecFindPrev;
end;

procedure TCommandsDataModule.actSearchReplaceExecute(Sender: TObject);
begin
  var SearchCommands := FindSearchTarget;
  if SearchCommands <> nil then
    SearchCommands.ExecReplace;
end;

procedure TCommandsDataModule.IncrementalSearch;
begin
  var SearchCmds := FindSearchTarget;
  if Assigned(SearchCmds) then with SearchCmds do begin
    SearchTarget.SetCaretAndSelection(SearchTarget.BlockBegin, SearchTarget.BlockBegin, SearchTarget.BlockBegin);
    EditorSearchOptions.InitSearch;
    DoSearchReplaceText(SearchTarget, False, False, True);
  end;
end;

procedure TCommandsDataModule.actSearchReplaceNowExecute(Sender: TObject);
begin
  var SearchCmds := FindSearchTarget;
  if Assigned(SearchCmds) then with SearchCmds do begin
    EditorSearchOptions.InitSearch;
    DoSearchReplaceText(SearchTarget, True, False);
  end;
end;

procedure TCommandsDataModule.actToolsEditStartupScriptsExecute(Sender: TObject);
begin
  GI_EditorFactory.OpenFile(TPyScripterSettings.PyScripterInitFile);
  GI_EditorFactory.OpenFile(TPyScripterSettings.EngineInitFile);
end;

procedure TCommandsDataModule.actSearchGoToDebugLineExecute(Sender: TObject);
begin
  with GI_PyControl.CurrentPos do
    if (Line >= 1) and GI_PyControl.PythonLoaded and not GI_PyControl.Running then
      GI_PyIDEServices.ShowFilePosition(FileId , Line, 1, 0, True, True);
end;

procedure TCommandsDataModule.actSearchGoToLineExecute(Sender: TObject);
var
  Line: string;
  LineNo: Integer;
begin
  if Assigned(GI_ActiveEditor) then
    if InputQuery(_(SGoToLineNumber), _(SEnterLineNumber), Line) then begin
      try
        LineNo := StrToInt(Line);
        GI_ActiveEditor.ActiveSynEdit.CaretXY := BufferCoord(1, LineNo);
      except
        on E: EConvertError do begin
          StyledMessageDlg(Format(_(SNotAValidNumber), [Line]), mtError,
            [mbAbort], 0);
        end;
      end;
    end;
end;

procedure TCommandsDataModule.actSearchGoToSyntaxErrorExecute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    GI_ActiveEditor.GoToSyntaxError;
end;

procedure TCommandsDataModule.actSearchHighlightExecute(Sender: TObject);
var
  SearchEngine: TSynEditSearchCustom;
  SearchOptions: TSynSearchOptions;
  Editor: IEditor;
begin
  Editor := GI_PyIDEServices.ActiveEditor;
  if Assigned(Editor) then begin
    if actSearchHighlight.Checked and (EditorSearchOptions.SearchText <> '') then
    begin
      SearchOptions := [];
      case EditorSearchOptions.SearchCaseSensitiveType of
        scsAuto:
          if LowerCase(EditorSearchOptions.SearchText) <> EditorSearchOptions.SearchText then
            Include(SearchOptions, ssoMatchCase);
        scsCaseSensitive: Include(SearchOptions, ssoMatchCase);
      end;
      if EditorSearchOptions.SearchWholeWords then
        Include(SearchOptions, ssoWholeWord);

      if EditorSearchOptions.UseRegExp then
        SearchEngine := SynEditRegexSearch
      else
        SearchEngine := SynEditSearch;
      try
        HighligthtSearchTerm(EditorSearchOptions.SearchText, Editor, SearchEngine,
          SearchOptions);
      except
        on E: ESynRegEx do begin
          MessageBeep(MB_ICONERROR);
          GI_PyIDEServices.WriteStatusMsg(Format(_(SInvalidRegularExpression), [E.Message]));
          Exit;
        end;
      end;
    end else if not actSearchHighlight.Checked then
      ClearAllHighlightedTerms;
  end;
end;

procedure TCommandsDataModule.actFindInFilesExecute(Sender: TObject);
begin
  if Assigned(FindResultsWindow) then
    FindResultsWindow.Execute(False);
end;

procedure TCommandsDataModule.actUnfoldAllExecute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    GI_ActiveEditor.ActiveSynEdit.UncollapseAll;
end;

procedure TCommandsDataModule.actUnfoldClassesExecute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    GI_ActiveEditor.ActiveSynEdit.UnCollapseFoldType(3);
end;

procedure TCommandsDataModule.actUnfoldFunctionsExecute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    GI_ActiveEditor.ActiveSynEdit.UnCollapseFoldType(4);
end;

procedure TCommandsDataModule.actUnfoldLevel1Execute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    GI_ActiveEditor.ActiveSynEdit.ExecuteCommand(ecUnfoldLevel1, ' ', nil);
end;

procedure TCommandsDataModule.actUnfoldLevel2Execute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    GI_ActiveEditor.ActiveSynEdit.ExecuteCommand(ecUnfoldLevel2, ' ', nil);
end;

procedure TCommandsDataModule.actUnfoldLevel3Execute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    GI_ActiveEditor.ActiveSynEdit.ExecuteCommand(ecUnfoldLevel3, ' ', nil);
end;

procedure TCommandsDataModule.actUnfoldNearestExecute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    GI_ActiveEditor.ActiveSynEdit.ExecuteCommand(ecUnfoldNearest, ' ', nil);
end;

procedure TCommandsDataModule.actUnfoldRegionsExecute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    GI_ActiveEditor.ActiveSynEdit.ExecuteCommand(ecUnfoldRegions, ' ', nil);
end;

procedure TCommandsDataModule.actUnitTestWizardExecute(Sender: TObject);
var
  Tests: string;
  Editor: IEditor;
begin
  if Assigned(GI_ActiveEditor) and GI_ActiveEditor.HasPythonFile then begin
    Tests := TUnitTestWizard.GenerateTests(GI_ActiveEditor.FileId);
    if Tests <> '' then begin
      Editor := GI_EditorFactory.OpenFile('', 'Python');
      if Assigned(Editor) then
        Editor.SynEdit.SelText := Tests;
    end;
  end;
end;

procedure TCommandsDataModule.actEditorOptionsExecute(Sender: TObject);
var
  TempEditorOptions: TSynEditorOptionsContainer;
  TempStream: TMemoryStream;
  TempIniFile: TMemIniFile;
begin
  TempEditorOptions := TSynEditorOptionsContainer.Create(Self);
  TempEditorOptions.Assign(EditorOptions);  // Initialize with defaults
  // SynWeb attribute changes are taking immediate effect in Web Engine
  // So Cancel does not work.  As a workaround we save the syntax attributes
  // and restore them if the dialog is cancelled.
  TempStream := TMemoryStream.Create;
  TempIniFile := TMemIniFile.Create(TempStream);
  ResourcesDataModule.SynWebHtmlSyn.SaveToIniFile(TempIniFile);
  try
    with TSynEditOptionsDialog.Create(Self) do
    begin
      Form.cbApplyToAll.Checked := True;
      if Assigned(GI_ActiveEditor) then
      begin
        TempEditorOptions.Assign(GI_ActiveEditor.ActiveSynEdit);
        Form.cbApplyToAll.Enabled := True;
      end else
      begin
        TempEditorOptions.Assign(EditorOptions);
        Form.cbApplyToAll.Enabled := False;
      end;
      OnGetHighlighterCount := SynEditOptionsDialogGetHighlighterCount;
      OnGetHighlighter := SynEditOptionsDialogGetHighlighter;
      OnSetHighlighter := SynEditOptionsDialogSetHighlighter;
      VisiblePages := [soDisplay, soOptions, soKeystrokes, soColor];
      TSynEditOptionsDialog.HighlighterFileDir := TPyScripterSettings.ColorThemesFilesDir;
      Form.ColorTheme := PyIDEMainForm.AppStorage.ReadString('ColorTheme');
      GetUserCommand := GetEditorUserCommand;
      GetAllUserCommands := GetEditorAllUserCommands;
      UseExtendedStrings := True;
      if Execute(TempEditorOptions) then
      begin
        UpdateHighlighters;
        PyIDEMainForm.AppStorage.WriteString('ColorTheme', Form.ColorTheme);
        if Form.cbApplyToAll.Checked then
        begin
          EditorOptions.Assign(TempEditorOptions);
          ApplyEditorOptions;
          (GI_PyInterpreter as TPythonIIForm).ApplyEditorOptions(EditorOptions);
          PyIDEMainForm.StoreApplicationData;
        end else if Assigned(GI_ActiveEditor) then
          GI_ActiveEditor.ActiveSynEdit.Assign(TempEditorOptions);
      end else
        //  If canceled restore original settings
        ResourcesDataModule.SynWebHtmlSyn.LoadFromIniFile(TempIniFile);
      Free;
    end;
  finally
    TempEditorOptions.Free;
    TempIniFile.Free;
    TempStream.Free;
  end;
end;

procedure TCommandsDataModule.actInterpreterEditorOptionsExecute(
  Sender: TObject);
begin
  var TempEditorOptions := TSmartPtr.Make(TSynEditorOptionsContainer.Create(Self))();

  with TSynEditOptionsDialog.Create(Self) do begin
    TempEditorOptions.Assign(GI_PyInterpreter.Editor);
    Form.cbApplyToAll.Checked := False;
    Form.cbApplyToAll.Enabled := False;
    Form.Caption := 'Interpreter Editor Options';
    OnGetHighlighterCount := SynInterpreterOptionsDialogGetHighlighterCount;
    OnGetHighlighter := SynInterpreterOptionsDialogGetHighlighter;
    OnSetHighlighter := SynInterpreterOptionsDialogSetHighlighter;
    VisiblePages := [soDisplay, soOptions, soColor];
    TSynEditOptionsDialog.HighlighterFileDir := TPyScripterSettings.ColorThemesFilesDir;
    if Execute(TempEditorOptions) then begin
      UpdateHighlighters;
      (GI_PyInterpreter as TPythonIIForm).ApplyEditorOptions(TempEditorOptions);
    end;
    Free;
  end;
end;

procedure TCommandsDataModule.actEditIndentExecute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    GI_ActiveEditor.ActiveSynEdit.CommandProcessor(ecBlockIndent, ' ', nil);
end;

procedure TCommandsDataModule.actEditDedentExecute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    GI_ActiveEditor.ActiveSynEdit.CommandProcessor(ecBlockUnindent, ' ', nil);
end;

procedure TCommandsDataModule.actEditToggleCommentExecute(Sender: TObject);
var
  EndLine: Integer;
  BlockIsCommented: Boolean;
begin
  if Assigned(GI_ActiveEditor) then with GI_ActiveEditor.ActiveSynEdit do begin
    if (BlockBegin.Line <> BlockEnd.Line) and (BlockEnd.Char = 1) then
      EndLine := BlockEnd.Line - 1
    else
      EndLine := BlockEnd.Line;

    BlockIsCommented := True;
    for var I := BlockBegin.Line to EndLine do
      if Copy(Trim(Lines[I-1]), 1, 2) <> '##' then begin
        BlockIsCommented := False;
        Break;
      end;

    if BlockIsCommented then
      actEditUncommentExecute(Sender)
    else
      actEditCommentOutExecute(Sender);
  end;
end;

procedure TCommandsDataModule.actEditFileEncodingExecute(Sender: TObject);
begin
  if (Sender is TAction) and Assigned(GI_ActiveEditor) then begin
    GI_ActiveEditor.FileEncoding := TFileSaveFormat(TAction(Sender).Tag);
    GI_ActiveEditor.SynEdit.Modified := True;
  end;
end;

procedure TCommandsDataModule.actEditCommentOutExecute(Sender: TObject);
var
  Str: string;
  Offset: Integer;
  OldBlockBegin, OldBlockEnd: TBufferCoord;
begin
  if Assigned(GI_ActiveEditor) then with GI_ActiveEditor.ActiveSynEdit do
  begin
    OldBlockBegin := BlockBegin;
    OldBlockEnd := BlockEnd;
    if SelAvail then begin // has selection
      OldBlockBegin := BufferCoord(1, OldBlockBegin.Line);
      BlockBegin := OldBlockBegin;
      BlockEnd := OldBlockEnd;
      BeginUpdate;
      Str:='##' + SelText;
      Offset:=0;
      if Str[Length(Str)] = #10 then begin // if the selection ends with a newline, eliminate it
        if Str[Length(Str) - 1] = #13 then // do we ignore 1 or 2 chars?
          Offset:=2
        else
          Offset:=1;
        Str:=Copy(Str, 1, Length(Str)-Offset);
      end;
      Str := StringReplace(Str, #10, #10'##', [rfReplaceAll]);
      if Offset=1 then
        Str := Str + #10
      else if Offset=2 then
        Str:=Str + sLineBreak;
      SelText := Str;
      EndUpdate;
      BlockBegin := OldBlockBegin;
      if Offset = 0 then
        Inc(OldBlockEnd.Char, 2);
      BlockEnd := BufferCoord(OldBlockEnd.Char, OldBlockEnd.Line);
    end
    else
    begin // no selection; easy stuff ;)
      // Do with selection to be able to undo
      CaretXY := BufferCoord(1, CaretY);
      SelText := '##';
      CaretXY := BufferCoord(OldBlockEnd.Char + 2, OldBlockEnd.Line);
    end;
    UpdateCarets;
  end;
end;

procedure TCommandsDataModule.actEditUncommentExecute(Sender: TObject);
var
  OldBlockBegin, OldBlockEnd: TBufferCoord;
begin
  if Assigned(GI_ActiveEditor) then with GI_ActiveEditor.ActiveSynEdit do begin
    OldBlockBegin := BlockBegin;
    OldBlockEnd := BlockEnd;
    if SelAvail then
    begin
      OldBlockBegin := BufferCoord(1, OldBlockBegin.Line);
      BlockBegin := OldBlockBegin;
      BlockEnd := OldBlockEnd;
      SelText := TPyRegExpr.CodeCommentLineRE.Replace(SelText, '$1');
      BlockBegin := OldBlockBegin;
      BlockEnd := BufferCoord(OldBlockEnd.Char - 2, OldBlockEnd.Line);
    end else begin
      BlockBegin := BufferCoord(1, CaretY);
      BlockEnd := BufferCoord(Length(LineText)+1, CaretY);
      SelText := TPyRegExpr.CodeCommentLineRE.Replace(SelText, '$1');
      CaretXY := BufferCoord(OldBlockEnd.Char - 2, OldBlockEnd.Line);
    end;
    UpdateCarets;
  end;
end;

procedure TCommandsDataModule.actEditTabifyExecute(Sender: TObject);
var
  BB, BE: TBufferCoord;
begin
  if Assigned(GI_ActiveEditor) then with GI_ActiveEditor.ActiveSynEdit do
  begin
    if SelAvail then
    begin
      BB := BlockBegin;
      BE := BlockEnd;
    end
    else
    begin
      BB := BufferCoord(1,1);
      BE := BufferCoord(Lines[Lines.Count -1].Length + 1, Lines.Count);
    end;
    var S := TabifyString(SelectionText(BB, BE));
    SetSelectionText(S, BB, BE);
  end;
end;

procedure TCommandsDataModule.actEditUntabifyExecute(Sender: TObject);
var
  BB, BE: TBufferCoord;
begin
  if Assigned(GI_ActiveEditor) then with GI_ActiveEditor.ActiveSynEdit do
  begin
    if SelAvail then
    begin
      BB := BlockBegin;
      BE := BlockEnd;
    end
    else
    begin
      BB := BufferCoord(1,1);
      BE := BufferCoord(Lines[Lines.Count -1].Length + 1, Lines.Count);
    end;
    var S := SelectionText(BB, BE);
    var SLines := StringToLines(S);
    for var I := 0 to Length(SLines) - 1 do
       SLines[I] := ExpandTabs(SLines[I], TabWidth);
    S := ''.Join(SLineBreak, SLines);
    SetSelectionText(S, BB, BE);
  end;
end;

procedure TCommandsDataModule.actExportHighlightersExecute(Sender: TObject);
begin
  ResourcesDataModule.ExportHighlighters;
end;

procedure TCommandsDataModule.actExportShortCutsExecute(Sender: TObject);
var
  AppStorage: TJvAppIniFileStorage;
  ActionProxyCollection: TActionProxyCollection;
begin
  with ResourcesDataModule.dlgFileSave do begin
    Title := _(SExportShortcuts);
    Filter := ResourcesDataModule.SynIniSyn.DefaultFilter;
    DefaultExt := 'ini';
    if Execute then begin
      AppStorage := TJvAppIniFileStorage.Create(nil);
      try
        AppStorage.FlushOnDestroy := True;
        AppStorage.Location := flCustom;
        AppStorage.FileName := FileName;
        AppStorage.StorageOptions.StoreDefaultValues := False;
        AppStorage.WriteString('PyScripter\Version', ApplicationVersion);
        AppStorage.DeleteSubTree('IDE Shortcuts');
        ActionProxyCollection := TActionProxyCollection.Create(apcctAll);
        try
          AppStorage.WriteCollection('IDE Shortcuts', ActionProxyCollection, 'Action');
        finally
          ActionProxyCollection.Free;
        end;
        AppStorage.DeleteSubTree('Editor Shortcuts');
        AppStorage.WriteCollection('Editor Shortcuts', EditorOptions.Keystrokes);
      finally
        AppStorage.Free;
      end;
    end;
  end;
end;

procedure TCommandsDataModule.actImportHighlightersExecute(Sender: TObject);
begin
  ResourcesDataModule.ImportHighlighters;
end;

procedure TCommandsDataModule.actImportShortcutsExecute(Sender: TObject);
var
  AppStorage: TJvAppIniFileStorage;
  ActionProxyCollection: TActionProxyCollection;
  TempKeyStrokes: TSynEditKeyStrokes;
begin
  with ResourcesDataModule.dlgFileOpen do begin
    Title := _(SImportShortcuts);
    Filter := ResourcesDataModule.SynIniSyn.DefaultFilter;
    FileName := '';
    if Execute then begin
      AppStorage := TJvAppIniFileStorage.Create(nil);
      try
        AppStorage.Location := flCustom;
        AppStorage.FileName := FileName;
        AppStorage.ReadOnly := True;

        if AppStorage.PathExists('IDE Shortcuts') then
        begin
          ActionProxyCollection := TActionProxyCollection.Create(apcctEmpty);
          try
            AppStorage.ReadCollection('IDE Shortcuts', ActionProxyCollection, True, 'Action');
            ActionProxyCollection.ApplyShortCuts;
          finally
            ActionProxyCollection.Free;
          end;
        end;
        if AppStorage.PathExists('Editor Shortcuts') then
        begin
          TempKeyStrokes := TSmartPtr.Make(TSynEditKeyStrokes.Create(nil))();
          try
            AppStorage.ReadCollection('Editor Shortcuts', TempKeyStrokes, True);
            EditorOptions.Keystrokes.Assign(TempKeyStrokes);
          except
           on E: ESynKeyError do
             StyledMessageDlg(E.Message, mtError, [TMsgDlgBtn.mbOK], 0);
          end;
          GI_EditorFactory.ApplyToEditors(procedure(Editor: IEditor)
          begin
            Editor.SynEdit.Keystrokes.Assign(EditorOptions.Keystrokes);
          end);

          (GI_PyInterpreter as TPythonIIForm).ApplyEditorOptions(EditorOptions, True);
        end;
      finally
        AppStorage.Free;
      end;
    end;
  end;
end;

procedure TCommandsDataModule.actEditLBExecute(Sender: TObject);
begin
  if (Sender is TAction) and Assigned(GI_ActiveEditor) then begin
    (GI_ActiveEditor.SynEdit.Lines as TSynEditStringList).FileFormat :=
      TSynEditFileFormat(TAction(Sender).Tag);
    GI_ActiveEditor.SynEdit.Modified := True;
  end;
end;

procedure TCommandsDataModule.actSearchMatchingBraceExecute(
  Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    GI_ActiveEditor.ActiveSynEdit.CommandProcessor(ecMatchBracket, #0, nil);
end;

procedure TCommandsDataModule.actIDEOptionsExecute(Sender: TObject);
var
  Categories: array of TOptionCategory;
  Reg: TRegistry;
  IsRegistered: Boolean;
  Key: string;
  IgnoredProperties: TArray<string>;
begin
  SetLength(Categories, 12);
  with Categories[0] do begin
    DisplayName := _('IDE');
    SetLength(Options, 15);
    Options[0].PropertyName := 'AutoCheckForUpdates';
    Options[0].DisplayName := _('Check for updates automatically');
    Options[1].PropertyName := 'DaysBetweenChecks';
    Options[1].DisplayName := _('Days between update checks');
    Options[2].PropertyName := 'EditorsTabPosition';
    Options[2].DisplayName := _('Editor tab position');
    Options[3].PropertyName := 'SmartNextPrevPage';
    Options[3].DisplayName := _('Smart Next Previous Page');
    Options[4].PropertyName := 'ShowTabCloseButton';
    Options[4].DisplayName := _('Show tab close button');
    Options[5].PropertyName := 'DockAnimationInterval';
    Options[5].DisplayName := _('Dock animation interval (ms)');
    Options[6].PropertyName := 'DockAnimationMoveWidth';
    Options[6].DisplayName := _('Dock animation move width (pixels)');
    Options[7].PropertyName := 'FileTemplateForNewScripts';
    Options[7].DisplayName := _('File template for new Python scripts');
    Options[8].PropertyName := 'NoOfRecentFiles';
    Options[8].DisplayName := _('Number of recent files');
    Options[9].PropertyName := 'StyleMainWindowBorder';
    Options[9].DisplayName := _('Style Main Window Border');
    Options[10].PropertyName := 'RestoreOpenFiles';
    Options[10].DisplayName := _('Restore open files');
    Options[11].PropertyName := 'RestoreOpenProject';
    Options[11].DisplayName := _('Restore open project');
    Options[12].PropertyName := 'AutoRestart';
    Options[12].DisplayName := _('Automatic restart');
    Options[13].PropertyName := 'LoggingEnabled';
    Options[13].DisplayName := _('Logging enabled');
    Options[14].PropertyName := 'UIContentFontSize';
    Options[14].DisplayName := _('User Interface content font size in points');
  end;
  with Categories[1] do begin
    DisplayName := _('Python Interpreter');
    SetLength(Options, 14);
    Options[0].PropertyName := 'SaveFilesBeforeRun';
    Options[0].DisplayName := _('Save files before run');
    Options[1].PropertyName := 'SaveEnvironmentBeforeRun';
    Options[1].DisplayName := _('Save environment before run');
    Options[2].PropertyName := 'TimeOut';
    Options[2].DisplayName := _('Timeout for running scripts in ms');
    Options[3].PropertyName := 'PrettyPrintOutput';
    Options[3].DisplayName := _('Pretty print output');
    Options[4].PropertyName := 'ClearOutputBeforeRun';
    Options[4].DisplayName := _('Clear output before run');
    Options[5].PropertyName := 'PostMortemOnException';
    Options[5].DisplayName := _('Post-mortem on exception');
    Options[6].PropertyName := 'InterpreterHistorySize';
    Options[6].DisplayName := _('Interpreter history size');
    Options[7].PropertyName := 'SaveInterpreterHistory';
    Options[7].DisplayName := _('Save interpreter history');
    Options[8].PropertyName := 'ReinitializeBeforeRun';
    Options[8].DisplayName := _('Reinitialize before run');
    Options[9].PropertyName := 'JumpToErrorOnException';
    Options[9].DisplayName := _('Jump to error on exception');
    Options[10].PropertyName := 'InternalInterpreterHidden';
    Options[10].DisplayName := _('Internal Interpreter hidden');
    Options[11].PropertyName := 'AlwaysUseSockets';
    Options[11].DisplayName := _('Always use sockets');
    Options[12].PropertyName := 'TraceOnlyIntoOpenFiles';
    Options[12].DisplayName := _('Step into open files only');
    Options[13].PropertyName := 'MaskFPUExceptions';
    Options[13].DisplayName := _('Mask FPU Exceptions');
  end;
  with Categories[2] do begin
    DisplayName := _('Code Explorer');
    SetLength(Options, 1);
    Options[0].PropertyName := 'ExplorerInitiallyExpanded';
    Options[0].DisplayName := _('Initially expanded');
  end;
  with Categories[3] do begin
    DisplayName := _('Project Explorer');
    SetLength(Options, 1);
    Options[0].PropertyName := 'ProjectExplorerInitiallyExpanded';
    Options[0].DisplayName := _('Initially expanded');
  end;
  with Categories[4] do begin
    DisplayName := _('File Filters');
    SetLength(Options, 12);
    Options[0].PropertyName := 'PythonFileFilter';
    Options[0].DisplayName := Format(_(SOpenDialogFilter), ['Python']);
    Options[1].PropertyName := 'HTMLFileFilter';
    Options[1].DisplayName := Format(_(SOpenDialogFilter), ['HTML']);
    Options[2].PropertyName := 'XMLFileFilter';
    Options[2].DisplayName := Format(_(SOpenDialogFilter), ['XML']);
    Options[3].PropertyName := 'CSSFileFilter';
    Options[3].DisplayName := Format(_(SOpenDialogFilter), ['CSS']);
    Options[4].PropertyName := 'CPPFileFilter';
    Options[4].DisplayName := Format(_(SOpenDialogFilter), ['CPP']);
    Options[5].PropertyName := 'YAMLFileFilter';
    Options[5].DisplayName := Format(_(SOpenDialogFilter), ['YAML']);
    Options[6].PropertyName := 'JSFileFilter';
    Options[6].DisplayName := Format(_(SOpenDialogFilter), ['JavaScript']);
    Options[7].PropertyName := 'PHPFileFilter';
    Options[7].DisplayName := Format(_(SOpenDialogFilter), ['PHP']);
    Options[8].PropertyName := 'FileExplorerFilter';
    Options[8].DisplayName := _('File explorer filter');
    Options[9].PropertyName := 'CythonFileFilter';
    Options[9].DisplayName := Format(_(SOpenDialogFilter), ['Cython']);
    Options[10].PropertyName := 'JSONFileFilter';
    Options[10].DisplayName := Format(_(SOpenDialogFilter), ['JSON']);
    Options[11].PropertyName := 'GeneralFileFilter';
    Options[11].DisplayName := Format(_(SOpenDialogFilter), [_('text file')]);
  end;
  with Categories[5] do begin
    DisplayName := _('Editor');
    SetLength(Options, 25);
    Options[0].PropertyName := 'SearchTextAtCaret';
    Options[0].DisplayName := _('Search text at caret');
    Options[1].PropertyName := 'CreateBackupFiles';
    Options[1].DisplayName := _('Create backup files');
    Options[2].PropertyName := 'UndoAfterSave';
    Options[2].DisplayName := _('Undo after save');
    Options[3].PropertyName := 'ShowCodeHints';
    Options[3].DisplayName := _('Show code hints');
    Options[4].PropertyName := 'ShowDebuggerHints';
    Options[4].DisplayName := _('Show debugger hints');
    Options[5].PropertyName := 'AutoCompleteBrackets';
    Options[5].DisplayName := _('Auto-complete brackets');
    Options[6].PropertyName := 'MarkExecutableLines';
    Options[6].DisplayName := _('Show executable line marks');
    Options[7].PropertyName := 'NewFileLineBreaks';
    Options[7].DisplayName := _('Default line break format for new files');
    Options[8].PropertyName := 'NewFileEncoding';
    Options[8].DisplayName := _('Default file encoding for new files');
    Options[9].PropertyName := 'DetectUTF8Encoding';
    Options[9].DisplayName := _('Detect UTF-8 encoding when opening files');
    Options[10].PropertyName := 'AutoReloadChangedFiles';
    Options[10].DisplayName := _('Auto-reload changed files');
    Options[11].PropertyName := 'AutoHideFindToolbar';
    Options[11].DisplayName := _('Auto-hide find toolbar');
    Options[12].PropertyName := 'HighlightSelectedWord';
    Options[12].DisplayName := _('Highlight selected word');
    Options[13].PropertyName := 'HighlightSelectedWordColor';
    Options[13].DisplayName := _('Highlight color of selected word');
    Options[14].PropertyName := 'DisplayPackageNames';
    Options[14].DisplayName := _('Display package names in editor tabs');
    Options[15].PropertyName := 'CodeFoldingEnabled';
    Options[15].DisplayName := _('Code folding enabled by default');
    Options[16].PropertyName := 'CodeFolding';
    Options[16].DisplayName := _('Code folding options');
    Options[17].PropertyName := 'CompactLineNumbers';
    Options[17].DisplayName := _('Compact Line Numbers');
    Options[18].PropertyName := 'TrimTrailingSpacesOnSave';
    Options[18].DisplayName := _('Trim trailing spaces when files are saved');
    Options[19].PropertyName := 'TrackChanges';
    Options[19].DisplayName := _('Track changes bar');
    Options[20].PropertyName := 'SelectionColor';
    Options[20].DisplayName := _('Selection color');
    Options[21].PropertyName := 'IndentGuides';
    Options[21].DisplayName := _('Indentation Guides');
    Options[22].PropertyName := 'ScrollbarAnnotation';
    Options[22].DisplayName := _('Scrollbar annotation');
    Options[23].PropertyName := 'DisplayFlowControl';
    Options[23].DisplayName := _('Display flow control symbols');
    Options[24].PropertyName := 'AccessibilitySupport';
    Options[24].DisplayName := _('Accessibility support');
  end;
  with Categories[6] do begin
    DisplayName := _('Code Completion');
    SetLength(Options, 9);
    Options[0].PropertyName := 'CodeCompletionListSize';
    Options[0].DisplayName := _('Code completion list size');
    Options[1].PropertyName := 'EditorCodeCompletion';
    Options[1].DisplayName := _('Editor code completion');
    Options[2].PropertyName := 'InterpreterCodeCompletion';
    Options[2].DisplayName := _('Interpreter code completion');
    Options[3].PropertyName := 'CodeCompletionCaseSensitive';
    Options[3].DisplayName := _('Case sensitive');
    Options[4].PropertyName := 'CompleteKeywords';
    Options[4].DisplayName := _('Complete Python keywords');
    Options[5].PropertyName := 'CompleteAsYouType';
    Options[5].DisplayName := _('Complete as you type');
    Options[6].PropertyName := 'AutoCompletionFont';
    Options[6].DisplayName := _('Auto completion font');
    Options[7].PropertyName := 'CompleteWithWordBreakChars';
    Options[7].DisplayName := _('Complete with word-break characters');
    Options[8].PropertyName := 'CompleteWithOneEntry';
    Options[8].DisplayName := _('Auto-complete with one entry');
  end;
  with Categories[7] do begin
    DisplayName := _('Shell Integration');
    SetLength(Options, 1);
    Options[0].PropertyName := 'FileExplorerContextMenu';
    Options[0].DisplayName := _('File Explorer context menu');
  end;
  with Categories[8] do begin
    DisplayName := _('File Explorer');
    SetLength(Options, 2);
    Options[0].PropertyName := 'FileExplorerBackgroundProcessing';
    Options[0].DisplayName := _('File Explorer background processing');
    Options[1].PropertyName := 'FileChangeNotification';
    Options[1].DisplayName := _('File Change Notification');
  end;
  with Categories[9] do begin
    DisplayName := 'SSH';
    SetLength(Options, 5);
    Options[0].PropertyName := 'SSHCommand';
    Options[0].DisplayName := _('SSH command');
    Options[1].PropertyName := 'SSHOptions';
    Options[1].DisplayName := _('SSH options');
    Options[2].PropertyName := 'ScpCommand';
    Options[2].DisplayName := _('SCP command');
    Options[3].PropertyName := 'ScpOptions';
    Options[3].DisplayName := _('SCP options');
    Options[4].PropertyName := 'SSHDisableVariablesWin';
    Options[4].DisplayName := _('Disable Variables Window with SSH');
  end;
  with Categories[10] do begin
    DisplayName := _('Language Server');
    SetLength(Options, 5);
    Options[0].PropertyName := 'LspDebug';
    Options[0].DisplayName := _('Language server debug output');
    Options[1].PropertyName := 'CheckSyntaxAsYouType';
    Options[1].DisplayName := _('Check syntax as you type');
    Options[2].PropertyName := 'SpecialPackages';
    Options[2].DisplayName := _('Special packages');
    Options[3].PropertyName := 'DiagnosticsOnOpen';
    Options[3].DisplayName := _('Code check on file open');
    Options[4].PropertyName := 'DiagnosticsOnSave';
    Options[4].DisplayName := _('Code check on file save');
  end;
  with Categories[11] do begin
    DisplayName := _('Spell Checking');
    SetLength(Options, 3);
    Options[0].PropertyName := 'DictLanguage';
    Options[0].DisplayName := _('Dictionary language code');
    Options[1].PropertyName := 'SpellCheckAsYouType';
    Options[1].DisplayName := _('Spell check as you type');
    Options[2].PropertyName := 'SpellCheckedTokens';
    Options[2].DisplayName := _('Spell checked syntax tokens');
  end;

  // Shell Integration
  IsRegistered := False;
  Reg := TRegistry.Create(KEY_READ and not KEY_NOTIFY);
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Key := 'Python.File\shell\Edit with PyScripter';
    IsRegistered := Reg.KeyExists(Key);
  except
  end;
  FreeAndNil(Reg);

  PyIDEOptions.FileExplorerContextMenu := IsRegistered;

  PyIDEOptions.SearchTextAtCaret := EditorSearchOptions.SearchTextAtCaret;

  IgnoredProperties := ['StructureColors', 'UseStructureColors'];

  var AdditionalFriendlyNames := TSmartPtr.Make(TStringList.Create)();
  AdditionalFriendlyNames.AddPair('StructureHighlight', _('Highlight structure'));
  AdditionalFriendlyNames.AddPair('ShowHintMark', _('Show hint mark'));
  AdditionalFriendlyNames.AddPair('GutterShapeSize', _('Gutter shape size'));
  AdditionalFriendlyNames.AddPair('ShowCollapsedLine', _('Show collapsed line'));
  AdditionalFriendlyNames.AddPair('CollapsedLineColor', _('Collapsed line color'));
  AdditionalFriendlyNames.AddPair('FolderBarLinesColor', _('Folder bar lines color'));
  AdditionalFriendlyNames.AddPair('FillWholeLines', _('Fill whole lines'));
  AdditionalFriendlyNames.AddPair('ModifiedColor', _('Modified color'));
  AdditionalFriendlyNames.AddPair('OriginalColor', _('Original color'));
  AdditionalFriendlyNames.AddPair('SavedColor', _('Saved color'));
  AdditionalFriendlyNames.AddPair('SavedModifiedColor', _('Saved and modified color'));

  if InspectOptions(PyIDEOptions, Categories, _(SIDEOptions), IgnoredProperties,
    AdditionalFriendlyNames, 610) then begin
    PyIDEOptions.Changed;
    PyIDEMainForm.StoreApplicationData;
    if PyIDEOptions.FileExplorerContextMenu <> IsRegistered then begin
      Reg := TRegistry.Create;
      try
        Reg.RootKey := HKEY_CLASSES_ROOT;
        if IsRegistered then
          Reg.DeleteKey(Key)
        else begin
          Reg.OpenKey(Key, True);
          Reg.CloseKey;
          Reg.OpenKey(Key + '\command', True);
          Reg.WriteString('', '"'+ Application.ExeName + ' "%1"');
          Reg.CloseKey;
        end;

        SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
      except
        StyledMessageDlg(_(SRegistryAccessDenied), mtError, [mbOK], 0);
      end;
      FreeAndNil(Reg);
    end;
  end;
end;

procedure TCommandsDataModule.actIDEShortcutsExecute(Sender: TObject);
begin
  with TfrmCustomKeyboard.Create(Application.MainForm) do begin
    if Execute then
      PyIDEMainForm.StoreApplicationData;
    Release;
  end;
end;

procedure TCommandsDataModule.actPythonPathExecute(Sender: TObject);
begin
  if not GI_PyControl.PythonLoaded then Exit;

  var Paths := TStringList.Create;
  var PathsCurrent := TStringList.Create;
  try
    GI_PyControl.ActiveInterpreter.SysPathToStrings(Paths);
    PathsCurrent.Assign(Paths);
    if EditFolderList(Paths, _('Python Path'), 870) then
      if not PathsCurrent.Equals(Paths) then begin
        GI_PyControl.ActiveInterpreter.StringsToSysPath(Paths);
        TPyLspClient.RestartServers;
      end;
  finally
    Paths.Free;
  end;
end;

procedure TCommandsDataModule.actAboutExecute(Sender: TObject);
begin
  with TAboutBox.Create(Self) do begin
    ShowModal;
    Release;
  end;
end;

procedure TCommandsDataModule.actAddWatchAtCursorExecute(Sender: TObject);
begin
  var Editor := GI_PyIDEServices.ActiveEditor;
  if Assigned(Editor) then
    TEditorForm(Editor.Form).AddWatchAtCursor;
end;

procedure TCommandsDataModule.actAssistantExplainExecute(Sender: TObject);
begin
   LLMAssistant.Explain;
end;

procedure TCommandsDataModule.actAssistantCancelExecute(Sender: TObject);
begin
  if LLMAssistant.IsBusy then
    LLMAssistant.CancelRequest;
end;

procedure TCommandsDataModule.actAssistantFixBugsExecute(Sender: TObject);
begin
  LLMAssistant.FixBugs;
end;

procedure TCommandsDataModule.actAssistantOptimizeExecute(Sender: TObject);
begin
  LLMAssistant.Optimize;
end;

procedure TCommandsDataModule.actAssistantSuggestExecute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
  begin
    SynCodeCompletion.CancelCompletion;
    SynParamCompletion.CancelCompletion;
    LLMAssistant.Suggest;
  end;
end;

procedure TCommandsDataModule.actPythonManualsExecute(Sender: TObject);
begin
  var PythonHelpFile := GI_PyControl.PythonHelpFile;
  if PythonHelpFile = '' then Exit;

  if ExtractFileExt(PythonHelpFile) = '.chm' then begin
    var OldHelpFile := Application.HelpFile;
    Application.HelpFile := PythonHelpFile;
    try
      Application.HelpCommand(HELP_CONTENTS, 0);
    finally
      Application.HelpFile := OldHelpFile;
    end;
  end else if ExtractFileExt(PythonHelpFile) = '.html' then  // python 11
    ShellExecute(0, 'open', PChar(FilePathToURI(PythonHelpFile)), '', '', SW_SHOWNORMAL);
end;

function TCommandsDataModule.ShowPythonKeywordHelp(KeyWord: string): Boolean;
begin
  Result := False;
  var PythonHelpFile := GI_PyControl.PythonHelpFile;
  if PythonHelpFile = '' then Exit;

  if ExtractFileExt(PythonHelpFile) = '.chm' then begin
    var OldHelpFile := Application.HelpFile;
    Application.HelpFile := PythonHelpFile;
    try
      Result := Application.HelpKeyword(KeyWord);
    finally
      Application.HelpFile := OldHelpFile;
    end;
  end
  else if ExtractFileExt(PythonHelpFile) = '.html' then  // python 11
  begin
    var Executable := ShellFindExecutable(PythonHelpFile, '');
    if Executable <> '' then
    begin
      var Args := Format('?q=%s&check_keywords=yes', [KeyWord]);
      var URI := FilePathToURI(ExtractFilePath(PythonHelpFile) + 'search.html')  + Args;
      Result := ShellExecute(0, 'open', PChar(Executable), PChar(URI), '', SW_SHOWNORMAL) > 32;
    end;
  end;
end;

procedure TCommandsDataModule.actHelpContentsExecute(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTENTS, 0);
end;

procedure TCommandsDataModule.ParameterCompletionCodeCompletion(
  Sender: TObject; var Value: string; Shift: TShiftState; Index: Integer;
  EndToken: WideChar);
begin
  if ssCtrl in Shift then
    Value := Parameters.Values[Value]
  else
    Value := Parameters.MakeParameter(Value);
end;

procedure TCommandsDataModule.actParameterCompletionExecute(
  Sender: TObject);
begin
  if Screen.ActiveControl is TCustomSynEdit then
  begin
    ResourcesDataModule.ParameterCompletion.Editor := TCustomSynEdit(Screen.ActiveControl);
    ResourcesDataModule.ParameterCompletion.ActivateCompletion;
  end;
end;

procedure TCommandsDataModule.actModifierCompletionExecute(
  Sender: TObject);
begin
  if Screen.ActiveControl is TCustomSynEdit then
  begin
    ResourcesDataModule.ModifierCompletion.Editor := TCustomSynEdit(Screen.ActiveControl);
    ResourcesDataModule.ModifierCompletion.ActivateCompletion;
  end;
end;

procedure TCommandsDataModule.actReplaceParametersExecute(Sender: TObject);
begin
  if Screen.ActiveControl is TCustomSynEdit then
    with TCustomSynEdit(Screen.ActiveControl) do begin
      var OldCaret := CaretXY;
      if SelAvail then
        SelText := Parameters.ReplaceInText(SelText)
      else try
        BeginUpdate;
        for var Line:= 0 to Lines.Count - 1 do begin
          var SLine := Lines[Line];
          var MaskPos := AnsiPos(Parameters.StartMask, SLine);
          if MaskPos > 0 then begin
            BeginUndoBlock;
            try
              BlockBegin := BufferCoord(MaskPos, Line + 1);
              BlockEnd := BufferCoord(Length(SLine) + 1, Line + 1);
              SelText := Parameters.ReplaceInText(Copy(SLine, MaskPos, MaxInt));
            finally
              EndUndoBlock;
            end;
          end;
        end;
      finally
        EndUpdate;
      end;
      CaretXY := OldCaret;
    end;
end;

procedure TCommandsDataModule.actInsertTemplateExecute(Sender: TObject);
var
  SynEdit: TSynEdit;
begin
  if (Screen.ActiveControl is TSynEdit) and Assigned(GI_ActiveEditor) then begin
    SynEdit := TSynEdit(Screen.ActiveControl);
    with ResourcesDataModule.CodeTemplatesCompletion do
      Execute(GetPreviousToken(SynEdit), SynEdit);
  end;
end;

procedure TCommandsDataModule.actHelpParametersExecute(Sender: TObject);
begin
  Application.HelpJump('parameters');
end;

procedure TCommandsDataModule.actHelpExternalToolsExecute(Sender: TObject);
begin
  Application.HelpJump('externaltools');
end;

procedure TCommandsDataModule.actHelpEditorShortcutsExecute(
  Sender: TObject);
begin
  Application.HelpJump('editorshortcuts');
end;

procedure TCommandsDataModule.actCustomizeParametersExecute(
  Sender: TObject);
begin
  with TCustomizeParams.Create(Self) do begin
    SetItems(CustomParams);
    if ShowModal = mrOk then begin
      GetItems(CustomParams);
      RegisterCustomParams;
    end;
    Free;
  end;
end;

procedure TCommandsDataModule.actCodeTemplatesExecute(Sender: TObject);
begin
  with ResourcesDataModule.CodeTemplatesCompletion, TCodeTemplates.Create(Self) do
  begin
    CodeTemplateText := AutoCompleteList.Text;
    if ShowModal = mrOk then
      AutoCompleteList.Text := CodeTemplateText;
    Free;
  end;
end;

procedure TCommandsDataModule.actFileTemplatesExecute(Sender: TObject);
begin
  with TFileTemplatesDialog.Create(Self) do begin
    SetItems;
    if ShowModal = mrOk then
      GetItems;
    Free;
  end;
end;

procedure TCommandsDataModule.actConfigureToolsExecute(Sender: TObject);
begin
  if EditCollection(ToolsCollection, TToolItem, _('Configure Tools'), EditToolItem, 710) then
  begin
    PyIDEMainForm.SetupToolsMenu;
    PyIDEMainForm.SetupCustomizer;
  end;
end;

procedure TCommandsDataModule.actEditCopyFileNameExecute(Sender: TObject);
begin
  var Editor := GI_PyIDEServices.ActiveEditor;
  if Assigned(Editor) then
    Clipboard.AsText := Editor.FileId;
end;

procedure TCommandsDataModule.actFindFunctionExecute(Sender: TObject);
begin
  JumpToFunction;
end;

procedure TCommandsDataModule.actEditLineNumbersExecute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    GI_ActiveEditor.ActiveSynEdit.Gutter.ShowLineNumbers := not
      GI_ActiveEditor.ActiveSynEdit.Gutter.ShowLineNumbers;
end;

procedure TCommandsDataModule.actEditWordWrapExecute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    GI_ActiveEditor.ActiveSynEdit.WordWrap := not GI_ActiveEditor.ActiveSynEdit.WordWrap
  else if GI_PyInterpreter.Editor.Focused then
    GI_PyInterpreter.Editor.WordWrap := not GI_PyInterpreter.Editor.WordWrap;
end;

procedure TCommandsDataModule.actEditShowSpecialCharsExecute(
  Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
  begin
    var VisibleSpecialChars := GI_ActiveEditor.ActiveSynEdit.VisibleSpecialChars;
    if VisibleSpecialChars = [] then
      GI_ActiveEditor.ActiveSynEdit.VisibleSpecialChars := [scWhitespace, scControlChars, scEOL]
    else
      GI_ActiveEditor.ActiveSynEdit.VisibleSpecialChars := [];
  end
  else if GI_PyInterpreter.Editor.Focused then
  begin
    var VisibleSpecialChars := GI_PyInterpreter.Editor.Options;
    if VisibleSpecialChars = [] then
      GI_PyInterpreter.Editor.VisibleSpecialChars := [scWhitespace, scControlChars, scEOL]
    else
      GI_PyInterpreter.Editor.VisibleSpecialChars := [];
  end;
end;

procedure TCommandsDataModule.actFindNextReferenceExecute(
  Sender: TObject);
var
  SearchOptions: TSynSearchOptions;
  SearchText: string;
  OldCaret: TBufferCoord;
begin
  if Assigned(GI_ActiveEditor) then
    with GI_ActiveEditor.ActiveSynEdit do
    begin
      SearchText :=  WordAtCursor;
      if SearchText <> '' then
      begin
        OldCaret := CaretXY;

        SearchEngine := SynEditSearch;
        SearchOptions := [];
        if (Sender as TComponent).Tag = 1 then
          Include(SearchOptions, ssoBackwards)
        else
          CaretX := CaretX + 1;  //  So that we find the next identifier
        Include(SearchOptions, ssoMatchCase);
        Include(SearchOptions, ssoWholeWord);
        GI_PyIDEServices.WriteStatusMsg('');
        if SearchReplace(SearchText, '', SearchOptions) = 0 then
        begin
          CaretXY := OldCaret;
          MessageBeep(MB_ICONASTERISK);
          GI_PyIDEServices.WriteStatusMsg(Format(_(SNotFound), [SearchText]));
        end
        else
        begin
          CaretXY := BlockBegin;
        end;
      end;
    end;
end;

procedure TCommandsDataModule.actFoldAllExecute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    GI_ActiveEditor.ActiveSynEdit.CollapseAll;
end;

procedure TCommandsDataModule.actFoldClassesExecute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    GI_ActiveEditor.ActiveSynEdit.CollapseFoldType(Integer(pftClassDefType));
end;

procedure TCommandsDataModule.actFoldFunctionsExecute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    GI_ActiveEditor.ActiveSynEdit.CollapseFoldType(Integer(pftFunctionDefType));
end;

procedure TCommandsDataModule.actFoldLevel1Execute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    GI_ActiveEditor.ActiveSynEdit.ExecuteCommand(ecFoldLevel1, ' ', nil);
end;

procedure TCommandsDataModule.actFoldLevel2Execute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    GI_ActiveEditor.ActiveSynEdit.ExecuteCommand(ecFoldLevel2, ' ', nil);
end;

procedure TCommandsDataModule.actFoldLevel3Execute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    GI_ActiveEditor.ActiveSynEdit.ExecuteCommand(ecFoldLevel3, ' ', nil);
end;

procedure TCommandsDataModule.actFoldNearestExecute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    GI_ActiveEditor.ActiveSynEdit.ExecuteCommand(ecFoldNearest, ' ', nil);
end;

procedure TCommandsDataModule.actFoldRegionsExecute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    GI_ActiveEditor.ActiveSynEdit.ExecuteCommand(ecFoldRegions, ' ', nil);
end;

procedure TCommandsDataModule.actFoldVisibleExecute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    GI_ActiveEditor.ActiveSynEdit.UseCodeFolding :=
      not GI_ActiveEditor.ActiveSynEdit.UseCodeFolding;
end;

procedure TCommandsDataModule.actCheckForUpdatesExecute(Sender: TObject);
var
  PlatformSuffix: string;
begin
  {$IFDEF WIN64}
  PlatformSuffix := '-x64';
  {$ELSE}
  PlatformSuffix := '';
  {$ENDIF}
  ProgramVersionHTTPLocation.VersionInfoFileName := 'PyScripterVersionInfo' +
    PlatformSuffix + '.ini';
  try
    ProgramVersionCheck.LocalDirectory :=
      TPath.Combine(TPyScripterSettings.UserDataPath, 'Updates');
    try
      FormatSettings.DateSeparator := '/';
      FormatSettings.ShortDateFormat := 'dd/MM/yyyy';
      ProgramVersionCheck.Execute;
    finally
      GetFormatSettings;
    end;
  except
    if Assigned(Sender) then
      raise
    else
      Exit;
  end;

  if Assigned(Sender) and not ProgramVersionCheck.IsRemoteProgramVersionNewer then
    if ProgramVersionCheck.DownloadError <> '' then
      StyledMessageDlg(_(SErrorWhileDownload) +
        ProgramVersionCheck.DownloadError, mtError, [mbOK], 0)
    else
      StyledMessageDlg(_(SCurrentVersionUptodate), mtInformation, [mbOK], 0);
  GI_PyIDEServices.AppStorage.WriteDateTime('Date checked for updates', Now);
end;

procedure TCommandsDataModule.actClearAllBreakpointsExecute(Sender: TObject);
begin
  GI_BreakpointManager.ClearAllBreakpoints;
end;

procedure TCommandsDataModule.actClearIssuesExecute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    (GI_ActiveEditor as TEditor).ClearDiagnostics;
end;

procedure TCommandsDataModule.actCodeActionExecute(Sender: TObject);
begin
  Tag := (Sender as TSpTBXItem).Tag;
  if Assigned(TPyLspClient.CodeActions) and
    InRange(Tag, 0, High(TPyLspClient.CodeActions.codeActions))
  then
    ApplyWorkspaceEdit(TPyLspClient.CodeActions.codeActions[Tag].edit);
end;

procedure TCommandsDataModule.actCodeCheckExecute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    GI_ActiveEditor.PullDiagnostics;
end;

procedure TCommandsDataModule.actCommandLineExecute(Sender: TObject);
begin
  TCommandLineDlg.Execute;
end;

procedure TCommandsDataModule.actDebugAbortExecute(Sender: TObject);
begin
  GI_PyControl.ActiveDebugger.Abort;
end;

procedure TCommandsDataModule.actDebugExecute(Sender: TObject);
begin
  Assert(GI_PyControl.PythonLoaded and not GI_PyControl.Running, 'actDebugExecute');
  var ActiveEditor := GI_PyIDEServices.ActiveEditor;
  if Assigned(ActiveEditor) then
  begin
    if GI_PyControl.Inactive then
      DebugActiveScript(ActiveEditor)
    else if GI_PyControl.DebuggerState = dsPaused then
      GI_PyControl.ActiveDebugger.Resume;
  end;
end;

procedure TCommandsDataModule.actDebugLastScriptExecute(Sender: TObject);
begin
  if GI_PyControl.Inactive then
    GI_PyControl.Debug;
end;

procedure TCommandsDataModule.actDebugPauseExecute(Sender: TObject);
begin
  GI_PyControl.ActiveDebugger.Pause;
end;

procedure TCommandsDataModule.actExecSelectionExecute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) and GI_ActiveEditor.HasPythonFile then
    GI_ActiveEditor.ExecuteSelection;
end;

procedure TCommandsDataModule.actExternalRunConfigureExecute(Sender: TObject);
begin
  EditTool(ExternalPython, True);
end;

procedure TCommandsDataModule.actExternalRunExecute(Sender: TObject);
begin
  var ActiveEditor := GI_PyIDEServices.ActiveEditor;
  if not Assigned(ActiveEditor) or ActiveEditor.FileName.IsEmpty then Exit;

  var RunConfig :=
    TSmartPtr.Make(TRunConfiguration.CreateFromFileId(ActiveEditor.FileId))();
  GI_PyControl.ExternalRun(RunConfig);
end;

procedure TCommandsDataModule.actExternalRunLastScriptExecute(Sender: TObject);
begin
  GI_PyControl.ExternalRun;
end;

procedure TCommandsDataModule.actFileExitExecute(Sender: TObject);
begin
  Application.MainForm.Close;
end;

procedure TCommandsDataModule.actFixAllExecute(Sender: TObject);
begin
  var Editor := GI_ActiveEditor;
  if Assigned(Editor) and Editor.HasPythonFile then
    TPyLspClient.DiagnosticsLspClient.ExecuteCommand('ruff.applyAutofix',
      Editor.FileId, Editor.Version);
end;

procedure TCommandsDataModule.actFormatCodeExecute(Sender: TObject);
begin
  var Editor := GI_ActiveEditor;
  if Assigned(Editor) and Editor.HasPythonFile then
    TPyLspClient.FormatCode(Editor.FileId, Editor.ActiveSynEdit);
end;

procedure TCommandsDataModule.actImportModuleExecute(Sender: TObject);
var
  Py: IPyEngineAndGIL;
begin
  var ActiveEditor := GI_PyIDEServices.ActiveEditor;
  if not Assigned(ActiveEditor) then Exit;

  Py := SafePyEngine;
  var PyModule :=
    GI_PyControl.ActiveInterpreter.ImportModule(ActiveEditor.FileId, True);
  VarClear(PyModule);

  GI_MessagesService.AddMessage(Format(_(SModuleImportedOK), [ActiveEditor.FileId]));
  GI_PyIDEServices.ShowIDEDockForm(IDEDockForm(ideMessages), False);
end;

procedure TCommandsDataModule.actNavEditorExecute(Sender: TObject);
begin
  var Editor := GI_PyIDEServices.GetActiveEditor;
  if Assigned(Editor) then
    Editor.Activate;
end;

procedure TCommandsDataModule.actNavigateToDockWindow(Sender: TObject);
begin
  var DockForm := IDEDockForm(TIDEDockWindowKind((Sender as TAction).Tag));
  if Assigned(DockForm) then
    GI_PyIDEServices.ShowIDEDockForm(DockForm);
end;

procedure TCommandsDataModule.actNextIssueExecute(Sender: TObject);
begin
  if  Assigned(GI_ActiveEditor) then
    (GI_ActiveEditor as TEditor).NextDiagnostic;
end;

procedure TCommandsDataModule.actOrganizeImportsExecute(Sender: TObject);
begin
  var Editor := GI_ActiveEditor;
  if Assigned(Editor) and Editor.HasPythonFile then
    TPyLspClient.DiagnosticsLspClient.ExecuteCommand('ruff.applyOrganizeImports',
      Editor.FileId, Editor.Version);
end;

procedure TCommandsDataModule.actPostMortemExecute(Sender: TObject);
begin
  GI_PyControl.ActiveDebugger.EnterPostMortem;
end;

procedure TCommandsDataModule.actPreviousIssueExecute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    (GI_ActiveEditor as TEditor).PreviousDiagnostic;
end;

procedure TCommandsDataModule.actProjectNewExecute(Sender: TObject);
begin
  GI_ProjectService.NewProject;
end;

procedure TCommandsDataModule.actProjectOpenExecute(Sender: TObject);
begin
  GI_ProjectService.OpenProject;
end;

procedure TCommandsDataModule.actProjectSaveAsExecute(Sender: TObject);
begin
  GI_ProjectService.SaveProjectAs;
end;

procedure TCommandsDataModule.actProjectSaveExecute(Sender: TObject);
begin
  GI_ProjectService.SaveProject;
end;

procedure TCommandsDataModule.actPythonEngineExecute(Sender: TObject);
begin
  var EngineType := TPythonEngineType((Sender as TAction).Tag);
  if EngineType = peSSH then begin
    var SSHServer := SelectSSHServer;
    if SSHServer <> '' then
      GI_PyControl.ActiveSSHServerName := SSHServer
    else
      Exit;
  end;
  GI_PyControl.PythonEngineType := EngineType;
  GI_PyInterpreter.PrintEngineType;
end;

procedure TCommandsDataModule.actPythonFreeThreadedExecute(Sender: TObject);
begin
  PyIDEOptions.PreferFreeThreaded := actPythonFreeThreaded.Checked;
  actPythonReinitializeExecute(Sender);
end;

procedure TCommandsDataModule.actPythonReinitializeExecute(Sender: TObject);
begin
  if not GI_PyControl.Inactive then begin
    if StyledMessageDlg(_(STerminateInterpreter),
      mtWarning, [mbYes, mbNo], 0) = idNo then Exit;
  end;
  GI_PyControl.ActiveInterpreter.ReInitialize;
end;

procedure TCommandsDataModule.actPythonSetupExecute(Sender: TObject);
begin
  with TPythonVersionsDialog.Create(Self) do
  begin
    ShowModal;
    Release;
  end;
  PyIDEMainForm.SetupPythonVersionsMenu;
end;

procedure TCommandsDataModule.actRefactorRenameExecute(Sender: TObject);
var
  BB, BE: TBufferCoord;
begin
  var Editor := GI_ActiveEditor;
  if not Assigned(Editor) or not Editor.HasPythonFile then Exit;
  var SynEdit := Editor.ActiveSynEdit;

  var Client := TPyLspClient.MainLspClient;
  if Client.LspClient.IsRequestSupported(lspPrepareRename) then
  begin
    var PrepRenameRes := Client.PrepareRename(Editor.FileId, SynEdit.CaretXY);
    if not Assigned(PrepRenameRes) then
    begin
      GI_PyIDEServices.WriteStatusMsg(_('Cannot perform rename'));
      Exit;
    end;
    BufferCoordinatesFromLspRange(PrepRenameRes.range, BB, BE);
    if BB <> BE then
      SynEdit.SetCaretAndSelection(BE, BB, BE)
    else
      SynEdit.ExecuteCommand(ecSelWord);
  end
  else
    SynEdit.ExecuteCommand(ecSelWord);

  var NewName := SynEdit.WordAtCursor;
  if not InputQuery(_('Rename'), _('New name')+':', NewName) then
    Exit;

  var Edit := Client.Rename(Editor.FileId, NewName, SynEdit.CaretXY);
  if not Assigned(Edit) then
    GI_PyIDEServices.WriteStatusMsg(_('Cannot perform rename'))
  else
    try
      ApplyWorkspaceEdit(Edit);
    finally
      Edit.Free;
    end;
end;

procedure TCommandsDataModule.actRunExecute(Sender: TObject);
begin
  Application.ProcessMessages;
  var ActiveEditor := GI_PyIDEServices.ActiveEditor;
  if not Assigned(ActiveEditor) then Exit;

  var RunConfig :=
    TSmartPtr.Make(TRunConfiguration.CreateFromFileId(ActiveEditor.FileId))();
  GI_PyControl.Run(RunConfig);
end;

procedure TCommandsDataModule.actRunLastScriptExecute(Sender: TObject);
begin
  if GI_PyControl.Inactive then
    GI_PyControl.Run;
end;

procedure TCommandsDataModule.actRunToCursorExecute(Sender: TObject);
begin
  Application.ProcessMessages;
  var ActiveEditor := GI_PyIDEServices.ActiveEditor;
  if Assigned(ActiveEditor) then
  begin
    if GI_PyControl.Inactive then
      DebugActiveScript(ActiveEditor, False, ActiveEditor.SynEdit.CaretY)
    else if GI_PyControl.DebuggerState = dsPaused then
      GI_PyControl.ActiveDebugger.RunToCursor(ActiveEditor.FileId,
        ActiveEditor.SynEdit.CaretY);
  end;
end;

procedure TCommandsDataModule.actShowRefactorMenuExecute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) and GI_ActiveEditor.HasPythonFile then
    GI_ActiveEditor.ShowRefactoringMenu;
end;

procedure TCommandsDataModule.actStepIntoExecute(Sender: TObject);
begin
  Assert(GI_PyControl.PythonLoaded and not GI_PyControl.Running, 'actStepIntoExecute');
  var ActiveEditor := GI_PyIDEServices.ActiveEditor;
  if Assigned(ActiveEditor) then
  begin
    if GI_PyControl.Inactive then
      DebugActiveScript(ActiveEditor, True)
    else if GI_PyControl.DebuggerState = dsPaused then
      GI_PyControl.ActiveDebugger.StepInto;
  end;
end;

procedure TCommandsDataModule.actStepOutExecute(Sender: TObject);
begin
  GI_PyControl.ActiveDebugger.StepOut;
end;

procedure TCommandsDataModule.actStepOverExecute(Sender: TObject);
begin
  GI_PyControl.ActiveDebugger.StepOver;
end;

procedure TCommandsDataModule.actToggleBreakPointExecute(Sender: TObject);
begin
  var ActiveEditor := GI_PyIDEServices.ActiveEditor;
  if Assigned(ActiveEditor) and ActiveEditor.HasPythonFile then
    GI_BreakpointManager.ToggleBreakpoint(ActiveEditor.FileId,
      ActiveEditor.SynEdit.CaretY);
end;

procedure TCommandsDataModule.actToolsRestartLSExecute(Sender: TObject);
begin
  TPyLspClient.RestartServers;
end;

{$ENDREGION 'Action Execute Handlers'}

{$REGION 'Action Upadate Handlers'}

procedure TCommandsDataModule.UpdateActionAlwaysEnabled(Sender: TObject);
begin
  TAction(Sender).Enabled := True;
end;

procedure TCommandsDataModule.UpdateAssistantActions(Sender: TObject);
begin
  var SelAvail := Assigned(GI_ActiveEditor) and GI_ActiveEditor.ActiveSynEdit.SelAvail;
  var HasPython := Assigned(GI_ActiveEditor) and GI_ActiveEditor.HasPythonFile;
  if Sender = actAssistantCancel then
    actAssistantCancel.Enabled := LLMAssistant.IsBusy
  else
  begin
    TAction(Sender).Enabled := HasPython and not LLMAssistant.IsBusy;
    if Sender = actAssistantSuggest then
      TAction(Sender).Enabled := TAction(Sender).Enabled and not SelAvail
    else
      TAction(Sender).Enabled := TAction(Sender).Enabled and SelAvail
  end;
end;

procedure TCommandsDataModule.UpdateBreakpointActions(Sender: TObject);
begin
  var Editor := GI_PyIDEServices.ActiveEditor;
  TAction(Sender).Enabled := Assigned(Editor) and Editor.HasPythonFile;
end;

procedure TCommandsDataModule.UpdateCodeFoldingActions(Sender: TObject);
begin
  var HasFold := Assigned(GI_ActiveEditor) and
    GI_ActiveEditor.ActiveSynEdit.UseCodeFolding;
  if Sender = actFoldVisible then
  begin
    actFoldVisible.Enabled := Assigned(GI_ActiveEditor);
    actFoldVisible.Checked := HasFold;
  end
  else
    TAction(Sender).Enabled := HasFold;
end;

procedure TCommandsDataModule.UpdateEditActions(Sender: TObject);
begin
  if Sender = actEditLineNumbers then
  begin
    actEditLineNumbers.Enabled := Assigned(GI_ActiveEditor);
    actEditLineNumbers.Checked := Assigned(GI_ActiveEditor) and
      GI_ActiveEditor.ActiveSynEdit.Gutter.ShowLineNumbers;
  end
  else if Sender = actEditReadOnly then
    actEditReadOnly.Checked := Assigned(GI_ActiveEditor) and
      GI_ActiveEditor.SynEdit.ReadOnly
  else if Sender = actEditWordWrap then
  begin
    actEditWordWrap.Enabled := Assigned(GI_ActiveEditor) and
      not GI_ActiveEditor.ActiveSynEdit.UseCodeFolding or
      GI_PyInterpreter.Editor.Focused;
    actEditWordWrap.Checked := Assigned(GI_ActiveEditor) and
      GI_ActiveEditor.ActiveSynEdit.WordWrap or
      GI_PyInterpreter.Editor.Focused and GI_PyInterpreter.Editor.WordWrap;
  end
  else if Sender = actEditShowSpecialChars then
  begin
    actEditShowSpecialChars.Enabled := Assigned(GI_ActiveEditor) or
      GI_PyInterpreter.Editor.Focused;
    actEditShowSpecialChars.Checked := Assigned(GI_ActiveEditor) and
      not (GI_ActiveEditor.ActiveSynEdit.VisibleSpecialChars = []);
  end
  else if Sender = actInsertTemplate then
    actInsertTemplate.Enabled := Assigned(GI_ActiveEditor);
end;

procedure TCommandsDataModule.UpdateEncodingActions(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(GI_ActiveEditor);
  if TAction(Sender).Enabled then
  begin
    var Encoding := GI_ActiveEditor.FileEncoding;
    if Sender = actEditAnsi then
      actEditAnsi.Checked := Encoding = sf_Ansi
    else if Sender = actEditUTF8 then
      actEditUTF8.Checked := Encoding = sf_UTF8
    else if Sender = actEditUTF8NoBOM then
      actEditUTF8NoBOM.Checked := Encoding = sf_UTF8_NoBOM
    else if Sender = actEditUTF16LE then
      actEditUTF16LE.Checked := Encoding = sf_UTF16LE
    else if Sender = actEditUTF16BE then
      actEditUTF16BE.Checked := Encoding = sf_UTF16BE;
  end
  else
    TAction(Sender).Checked := False;
end;

procedure TCommandsDataModule.UpdateFileActions(Sender: TObject);
begin
  var HaveFile := Assigned(GI_FileCmds);
  if Sender = actFileClose then
    actFileClose.Enabled := HaveFile and GI_FileCmds.CanClose
  else if Sender = actFileSave then
    actFileSave.Enabled := HaveFile and GI_FileCmds.CanSave
  else if Sender = actFileSaveAs then
    actFileSaveAs.Enabled := HaveFile
  else if Sender = actFileSaveToRemote then
    actFileSaveToRemote.Enabled := HaveFile
  else if Sender = actFilePrint then
    actFilePrint.Enabled := HaveFile
  else if Sender = actPrintPreview then
    actPrintPreview.Enabled := HaveFile
  else if Sender = actFileReload then
    actFileReload.Enabled := HaveFile and GI_FileCmds.CanReload
  else if Sender = actFileSaveAll then
    // Lesson to remember do not change the Enabled state of an Action from false to true
    // and back within an Update or OnIdle handler. The result is 100% CPU utilisation.
    actFileSaveAll.Enabled := Assigned(GI_EditorFactory.FirstEditorCond(
      function(Ed: IEditor): Boolean
      begin
        Result := (Ed as IFileCommands).CanSave;
      end));
end;

procedure TCommandsDataModule.UpdateLineBreakActions(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(GI_ActiveEditor);
  if TAction(Sender).Enabled then
  begin
    var Fmt := (GI_ActiveEditor.SynEdit.Lines as TSynEditStringList).FileFormat;
    if Sender = actEditLBDos then
      actEditLBDos.Checked := Fmt = sffDos
    else if Sender = actEditLBUnix then
      actEditLBUnix.Checked := Fmt = sffUnix
    else if Sender = actEditLBMac then
      actEditLBMac.Checked := Fmt = sffMac;
  end
  else
    TAction(Sender).Checked := False;
end;

procedure TCommandsDataModule.UpdateParameterActions(Sender: TObject);
begin
  TAction(Sender).Enabled := Screen.ActiveControl is TCustomSynEdit;;
end;

procedure TCommandsDataModule.UpdateRefactorActions(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(GI_ActiveEditor)
    and GI_ActiveEditor.HasPythonFile and not GI_ActiveEditor.SynEdit.ReadOnly
    and Assigned(TPyLspClient.MainLspClient) and
    TPyLspClient.MainLspClient.Ready;
end;

procedure TCommandsDataModule.UpdateRunActions(Sender: TObject);
begin
  var Editor := GI_PyIDEServices.ActiveEditor;
  var PyFileActive := Assigned(Editor) and Editor.HasPythonFile;
  var DbgState := GI_PyControl.DebuggerState;

  if Sender = actCommandLine then
    actCommandLine.Checked := CommandLineParams <> ''
  else if Sender = actImportModule then
    actImportModule.Enabled := PyFileActive and GI_PyControl.Inactive
  else if Sender = actPythonReinitialize then
    actPythonReinitialize.Enabled := Assigned(GI_PyControl.ActiveInterpreter) and
      (icReInitialize in GI_PyControl.ActiveInterpreter.InterpreterCapabilities) and
      not (DbgState in [dsPaused, dsPostMortem])
  else if Sender = actPythonFreeThreaded then
  begin
    actPythonFreeThreaded.Enabled :=
      GI_PyControl.PythonLoaded and
      (PyIDEOptions.PythonEngineType = peRemote) and
      (GI_PyControl.PythonVersion.PythonFreeThreadedExecutable <> '');
    actPythonFreeThreaded.Checked :=
      PyIDEOptions.PreferFreeThreaded;
  end
  else if Sender = actAddWatchAtCursor then
    actAddWatchAtCursor.Enabled := PyFileActive
  else if Sender = actPostMortem then
    actPostMortem.Enabled := GI_PyControl.Inactive and
      Assigned(GI_PyControl.ActiveDebugger) and
      GI_PyControl.ActiveDebugger.PostMortemEnabled
  else if Sender = actRun then
    actRun.Enabled := PyFileActive and GI_PyControl.Inactive
  else if Sender = actDebug then
  begin
    actDebug.Enabled := PyFileActive and
      (GI_PyControl.Inactive or (DbgState = dsPaused));
    if DbgState = dsPaused then
    begin
      actDebug.Caption := _(SResumeCaption);
      actDebug.Hint := _(SResumeHint);
    end else
    begin
      actDebug.Caption := _('Debug');
      actDebug.Hint := _(SDebugHint);
    end;
  end
  else if Sender = actExternalRun then
    actExternalRun.Enabled := PyFileActive and not Editor.FileName.IsEmpty
  else if Sender = actRunToCursor then
    actRunToCursor.Enabled := PyFileActive and
      (GI_PyControl.Inactive or (DbgState = dsPaused)) and
      TPyRegExpr.IsExecutableLine(Editor.SynEdit.LineText)
  else if Sender = actStepInto then
    actStepInto.Enabled := PyFileActive and
      (GI_PyControl.Inactive or (DbgState = dsPaused))
  else if (Sender = actStepOver) or (Sender = actStepOut) then
    TAction(Sender).Enabled := DbgState = dsPaused
  else if Sender = actDebugAbort then
    actDebugAbort.Enabled :=
      DbgState in [dsPaused, dsDebugging, dsRunning, dsPostMortem]
  else if Sender = actDebugPause then
    actDebugPause.Enabled := DbgState = dsDebugging
  else if (Sender = actRunLastScript) or (Sender = actDebugLastScript) or
    (Sender = actExternalRunLastScript) then
  begin
    TAction(Sender).Enabled := GI_PyControl.Inactive and
      (GI_PyControl.LastRunFileId <> '');
     var FName := TPath.GetFileName(GI_PyControl.LastRunFileId);
     if FName <> '' then
       FName := Format(' - %s', [FName]);
     if Sender = actRunLastScript then
       actRunLastScript.Hint := _(SHintRun) + FName
     else if Sender = actDebugLastScript then
       actDebugLastScript.Hint := _(SHintDebug) + FName
     else
       actExternalRunLastScript.Hint := _(SHintExternalRun) + FName;
  end
  else if Sender = actExecSelection then
    actExecSelection.Enabled := GI_PyControl.PythonLoaded and
      not GI_PyControl.Running and PyFileActive;
end;

procedure TCommandsDataModule.UpdatePythonEngineActions(Sender: TObject);
begin
  case PyIDEOptions.PythonEngineType of
    peInternal:  actPythonInternal.Checked := True;
    peRemote: actPythonRemote.Checked := True;
    peRemoteTk: actPythonRemoteTk.Checked := True;
    peRemoteWx: actPythonRemoteWx.Checked := True;
    peSSH: actPythonSSH.Checked := True;
  end;
end;

procedure TCommandsDataModule.UpdateSearchActions(Sender: TObject);
begin
  if Sender = actSearchMatchingBrace then
    actSearchMatchingBrace.Enabled := Assigned(GI_ActiveEditor)
  else if Sender = actSearchMatchingBrace then
    actSearchGoToLine.Enabled := Assigned(actSearchGoToLine)
  else if Sender = actSearchGoToSyntaxError then
    actSearchGoToSyntaxError.Enabled := Assigned(GI_ActiveEditor) and
      GI_ActiveEditor.HasSyntaxError
  else if Sender = actSearchHighlight then
  begin
    var Editor := GI_PyIDEServices.ActiveEditor;
    actSearchHighlight.Enabled := actSearchHighlight.Checked or
      Assigned(Editor) and (EditorSearchOptions.SearchText <> '');
    actSearchHighlight.Checked := Assigned(Editor) and Editor.HasSearchHighlight;
  end
  else if Sender = actSearchGoToDebugLine then
    actSearchGoToDebugLine.Enabled := (GI_PyControl.CurrentPos.Line >= 1) and
     GI_PyControl.PythonLoaded and not GI_PyControl.Running
  else if Sender = actFindInFiles then
    actFindInFiles.Enabled := not FindResultsWindow.IsBusy
  else if Sender = actFindFunction then
    actFindFunction.Enabled := Assigned(GI_ActiveEditor) and
      GI_ActiveEditor.HasPythonFile
  else if (Sender = actFindNextReference) or (Sender = actFindPreviousReference) then
    actFindNextReference.Enabled := Assigned(GI_ActiveEditor)
  else
  begin
    var SearchCommands := FindSearchTarget;
    if Sender = actSearchFind then
      actSearchFind.Enabled := (SearchCommands <> nil) and
        SearchCommands.CanFind
    else if (Sender = actSearchFindNext) or (Sender = actSearchFindPrev) then
      TAction(Sender).Enabled := (SearchCommands <> nil) and
        SearchCommands.CanFindNext
    else if Sender = actSearchReplace then
      actSearchReplace.Enabled := (SearchCommands <> nil) and
        SearchCommands.CanReplace
    else if Sender = actSearchReplaceNow then
      actSearchReplaceNow.Enabled := (SearchCommands <> nil) and
        SearchCommands.CanFindNext and SearchCommands.CanReplace;
  end;
end;

procedure TCommandsDataModule.UpdateIssuesActions(Sender: TObject);
begin
  if Sender = actFixAll then
    actFixAll.Enabled := Assigned(GI_ActiveEditor) and
      GI_ActiveEditor.HasFixableIssues
  else if (Sender = actClearIssues) or (Sender = actNextIssue) or
    (Sender = actPreviousIssue)
  then
  TAction(Sender).Enabled := Assigned(GI_ActiveEditor) and
    GI_ActiveEditor.HasIssues
  else if Sender = actCodeCheck then
    actCodeCheck.Enabled := Assigned(GI_ActiveEditor) and
      GI_ActiveEditor.HasPythonFile and
      Assigned(TPyLspClient.DiagnosticsLspClient) and
      TPyLspClient.DiagnosticsLspClient.Ready;
end;

procedure TCommandsDataModule.UpdateSourceCodeActions(Sender: TObject);
begin
  var NotReadOnly := Assigned(GI_ActiveEditor) and not GI_ActiveEditor.SynEdit.ReadOnly;
  var SelAvail := Assigned(GI_ActiveEditor) and GI_ActiveEditor.ActiveSynEdit.SelAvail;
  if (Sender = actEditIndent)  or (Sender = actEditDedent)then
    TAction(Sender).Enabled := SelAvail and NotReadOnly
  else if (Sender = actEditTabify)  or (Sender = actEditUntabify) then
    TAction(Sender).Enabled := NotReadOnly
  else if (Sender = actEditCommentOut)  or (Sender = actEditUncomment) or
    (Sender = actEditToggleComment)
  then
    TAction(Sender).Enabled := NotReadOnly
  else if Sender = actFormatCode then
    actFormatCode.Enabled := NotReadOnly and GI_ActiveEditor.HasPythonFile and
      Assigned(TPyLspClient.DiagnosticsLspClient) and
      TPyLspClient.DiagnosticsLspClient.Ready;
end;

procedure TCommandsDataModule.UpdateToolsActions(Sender: TObject);
begin
  if Sender = actPythonPath then
    actPythonPath.Enabled := GI_PyControl.PythonLoaded
  else if Sender = actUnitTestWizard then
    actUnitTestWizard.Enabled := Assigned(GI_ActiveEditor) and
      GI_ActiveEditor.HasPythonFile;
end;

{$ENDREGION 'Action Upadate Handlers'}


procedure TCommandsDataModule.GetEditorUserCommand(AUserCommand: Integer;
  var ADescription: string);
begin
  if AUserCommand = ecCodeCompletion then
    ADescription := _(SEdCmdCodeCompletion)
  else if AUserCommand = ecParamCompletion then
    ADescription := _(SEdCmdParameterCompletion);
end;

procedure TCommandsDataModule.GetEditorAllUserCommands(ACommands: TStrings);
begin
  ACommands.AddObject(_(SEdCmdCodeCompletion), TObject(ecCodeCompletion));
  ACommands.AddObject(_(SEdCmdParameterCompletion), TObject(ecParamCompletion));
end;

function TCommandsDataModule.DoSearchReplaceText(SynEdit: TSynEdit;
      AReplace, ABackwards: Boolean ; IsIncremental: Boolean = False): Integer;

  function EndReached(ABackwards, SelectionOnly: Boolean): string;
  begin
    if ABackwards then
      Result := Format(_(SReachedTheStart),
              [IfThen(SelectionOnly, _(SOfTheSelection), _(SOfTheDocument))])
    else
      Result := Format(_(SReachedTheEnd),
              [IfThen(SelectionOnly, _(SOfTheSelection), _(SOfTheDocument))]);
  end;

var
  Options: TSynSearchOptions;
  IsNewSearch: Boolean;
  MsgText: string;
  BB, BE: TBufferCoord;
  dlgID: Integer;
  OldNoReplaceCount: Integer;
begin
  Result := 0;
  OldNoReplaceCount := 0;

  if EditorSearchOptions.SearchText = '' then Exit; //Nothing to Search
  if PyIDEOptions.AutoHideFindToolbar and not IsIncremental then begin
    PyIDEMainForm.FindToolbar.Visible := False;
  end;
  if EditorSearchOptions.UseRegExp then
    SynEdit.SearchEngine := SynEditRegexSearch
  else
    SynEdit.SearchEngine := SynEditSearch;

  IsNewSearch := (not EditorSearchOptions.InitCaretXY.IsValid) or
                 (EditorSearchOptions.BackwardSearch <> ABackwards);
  if IsNewSearch then
    EditorSearchOptions.NewSearch(SynEdit, ABackwards);

  if AReplace then begin
    Options := [ssoPrompt, ssoReplace, ssoReplaceAll];
    EditorSearchOptions.NoReplaceCount := 0;
  end else
    Options := [];
  if ABackwards then
    Include(Options, ssoBackwards);
  case EditorSearchOptions.SearchCaseSensitiveType of
    scsAuto:
      if LowerCase(EditorSearchOptions.SearchText) <> EditorSearchOptions.SearchText then
        Include(Options, ssoMatchCase);
    scsCaseSensitive: Include(Options, ssoMatchCase);
  end;
  if EditorSearchOptions.SearchWholeWords then
    Include(Options, ssoWholeWord);

  with EditorSearchOptions do
    if EditorSearchOptions.TempSelectionOnly then
    begin
      Options := Options + [ssoSelectedOnly] - [ssoEntireScope];
      BE := TBufferCoord.Invalid;
      if IsNewSearch then
        BB := TBufferCoord.Invalid
      else
      begin
        BB := SynEdit.CaretXY;
        SynEdit.Selections.Restore(SelStorage);
      end;
    end
    else if WrappedSearch then
    begin
      if ABackwards then begin
        BB := InitCaretXY;
        if CanWrapSearch then
          BE := BufferCoord(Length(SynEdit.Lines[SynEdit.Lines.Count - 1]) + 1,
            SynEdit.Lines.Count)
        else
          BE := SynEdit.CaretXY;
      end else begin
        BE := InitCaretXY;
        if CanWrapSearch then
          BB := BufferCoord(1,1)
        else
          BB := SynEdit.CaretXY;
      end;
      CanWrapSearch := False;  //Do not wrap again!
    end
    else
    begin
      BB := BufferCoord(1,1);
      BE := BufferCoord(Length(SynEdit.Lines[SynEdit.Lines.Count - 1]) + 1,
        SynEdit.Lines.Count);
      if TempSearchFromCaret then
      begin
        if ABackwards then
          BE := SynEdit.CaretXY
        else
          BB := SynEdit.CaretXY;
      end;
    end;

  GI_PyIDEServices.WriteStatusMsg('');

  if EditorSearchOptions.TempSelectionOnly and SynEdit.Selections.IsEmpty then
    Result := 0
  else
    try
        Result := SynEdit.SearchReplace(EditorSearchOptions.SearchText,
         EditorSearchOptions.ReplaceText, Options, BB, BE);
    except
      on E: ESynRegEx do begin
        Result := 0;
        MessageBeep(MB_ICONERROR);
        GI_PyIDEServices.WriteStatusMsg(Format(_(SInvalidRegularExpression), [E.Message]));
        Exit;
      end;
    end;

  with EditorSearchOptions do
  begin
    // Further searches will be from caret. Only applies if not SelectionOnly
    TempSearchFromCaret := True;

    if (Result = 0) or (ssoReplace in Options) then
    // We have reached the end of the search range
    begin
      MessageBeep(MB_ICONASTERISK);
      if TempSelectionOnly and not AReplace then
        // All done - Restore the original selection
        SynEdit.Selections.Restore(SelStorage);

      if WrappedSearch then begin
        MsgText := _(SStartReached);
        GI_PyIDEServices.WriteStatusMsg(MsgText);
        DSAMessageDlg(dsaSearchStartReached, 'PyScripter', MsgText,
           mtInformation, [mbOK], 0, dckActiveForm, 0, mbOK);
        InitSearch;
      end
      else
      begin
        MsgText := EndReached(ABackwards, TempSelectionOnly);
        if Result = 0 then
          GI_PyIDEServices.WriteStatusMsg(Format(_(SNotFound), [SearchText]))
        else
          GI_PyIDEServices.WriteStatusMsg(MsgText);
        if CanWrapSearch and (LastReplaceAction <> raCancel) then
        begin
          dlgID := IfThen(ssoReplace in Options, dsaReplaceFromStart, dsaSearchFromStart);
          MsgText :=  Format(MsgText + sLineBreak + _(SContinueSearch),
            [IfThen(ssoReplace in Options, _(STheSearchAndReplace), _(STheSearch)),
             IfThen(ABackwards, _(SFromTheEnd), _(SFromTheStart))]);

          if  IsIncremental or (DSAMessageDlg(dlgID, 'PyScripter', MsgText,
             mtConfirmation, [mbYes, mbNo], 0, dckActiveForm, 0, mbYes, mbNo) = mrYes) then
          begin
            WrappedSearch := True;
            OldNoReplaceCount := NoReplaceCount;
            Result := Result + DoSearchReplaceText(SynEdit,  AReplace, ABackwards, IsIncremental);
            WrappedSearch := False;
          end;
        end
        else
        begin
          MsgText := _(SStartReached);
          if (Result = 0) and not (ssoReplace in Options) then
            GI_PyIDEServices.WriteStatusMsg(Format(_(SNotFound), [SearchText]))
          else
            GI_PyIDEServices.WriteStatusMsg(MsgText);
          InitSearch;
        end;
      end;
    end;

    if (ssoReplace in Options) and (Result > 0) and not WrappedSearch then
    begin
      MsgText := Format(_(SItemsReplaced),
        [Result + NoReplaceCount + OldNoReplaceCount, Result]);
      GI_PyIDEServices.WriteStatusMsg(MsgText);
      DSAMessageDlg(dsaReplaceNumber, 'PyScripter', MsgText,
         mtInformation, [mbOK], 0, dckActiveForm, 0, mbOK);
    end;
  end;

  ConfirmReplaceDialog.Free;
end;

function TCommandsDataModule.FindSearchTarget: ISearchCommands;
begin
  Result := GI_SearchCmds;
  if not Assigned(GI_SearchCmds) then
  begin
    if EditorSearchOptions.InterpreterIsSearchTarget and CanActuallyFocus(GI_PyInterpreter.Editor) then
      Result := GI_PyInterpreter as ISearchCommands
    else
    begin
      var Editor := GI_PyIDEServices.ActiveEditor;
      if Assigned(Editor) then
        Result := Editor as ISearchCommands;
    end;
  end;
end;

procedure TCommandsDataModule.HighlightCheckedImg(Sender: TObject; ACanvas:
    TCanvas; State: TSpTBXSkinStatesType; const PaintStage: TSpTBXPaintStage;
    var AImageList: TCustomImageList; var AImageIndex: Integer; var ARect:
    TRect; var PaintDefault: Boolean);
begin
  if (PaintStage = pstPrePaint) and (Sender as TSpTBXItem).Checked then
  begin
    ACanvas.Brush.Color := StyleServices.GetSystemColor(clHighlight);
    ACanvas.FillRect(ARect);
  end;
  PaintDefault := True;
end;

procedure TCommandsDataModule.mnProviderClick(Sender: TObject);
begin
  if Sender = spiOpenAI then
    LLMAssistant.Providers.Provider := llmProviderOpenAI
  else if Sender = spiDeepSeek then
    LLMAssistant.Providers.Provider := llmProviderDeepSeek
  else if Sender = spiGrok then
    LLMAssistant.Providers.Provider := llmProviderGrok
  else if Sender = spiGemini then
    LLMAssistant.Providers.Provider := llmProviderGemini
  else if Sender = spiOllama then
    LLMAssistant.Providers.Provider := llmProviderOllama;

  spiSettingsInitPopup(Sender, nil);
end;

procedure TCommandsDataModule.mnSpellingPopup(Sender: TTBCustomItem; FromLink:
    Boolean);
var
  Error: ISpellingError;
  CorrectiveAction: CORRECTIVE_ACTION;
  Replacement: PChar;
  MenuItem: TTBCustomItem;
  Action: TSynSpellErrorReplace;
  Suggestions: IEnumString;
  Suggestion: PWideChar;
  Fetched: LongInt;
  Indicator: TSynIndicator;
  AWord: string;
  HaveError: Boolean;
  Editor: TCustomSynEdit;
begin
  Editor := SynSpellCheck.Editor;
  if not Assigned(Editor) then
    Exit;

  // Remove replacement menu items and actions;
  repeat
    MenuItem := mnSpelling[0];
    if MenuItem.Action is TSynSpellErrorReplace then
    begin
      mnSpelling.Remove(MenuItem);
      MenuItem.Action.Free;
      MenuItem.Free;
    end
    else
      Break;
  until False;

  if not Assigned(SynSpellCheck.SpellChecker()) then
  begin
    mnSpelling.Visible := False;
    Exit;
  end;

  if Editor.Indicators.IndicatorAtPos(Editor.CaretXY,
   TSynSpellCheck.SpellErrorIndicatorId, Indicator)
  then
     AWord := Copy(Editor.Lines[Editor.CaretY - 1], Indicator.CharStart,
       Indicator.CharEnd - Indicator.CharStart)
  else
    AWord := '';

  //SynSpellCheck.Editor := Editor;
  Error := SynSpellCheck.ErrorAtPos(Editor.CaretXY);
  HaveError := Assigned(Error) and (AWord <> '');

  mnSpellCheckTopSeparator.Visible := HaveError;
  mnSpellCheckSecondSeparator.Visible := HaveError;
  mnSpellCheckAdd.Visible := HaveError;
  mnSpellCheckIgnore.Visible := HaveError;
  mnSpellCheckIgnoreOnce.Visible := HaveError;
  mnSpellCheckDelete.Visible := HaveError;


  if HaveError then
  begin
    Error.Get_CorrectiveAction(CorrectiveAction);
    case CorrectiveAction of
      CORRECTIVE_ACTION_GET_SUGGESTIONS:
        begin
          CheckOSError(SynSpellCheck.SpellChecker.Suggest(
            PChar(AWord), Suggestions));
          while Suggestions.Next(1, Suggestion, @Fetched) = S_OK do
          begin
            Action := TSynSpellErrorReplace.Create(Self);
            Action.Caption := Suggestion;
            MenuItem := TSpTBXItem.Create(Self);
            MenuItem.Action := Action;
            mnSpelling.Insert(mnSpelling.IndexOf(mnSpellCheckTopSeparator), MenuItem);
            CoTaskMemFree(Suggestion);
          end;
        end;
      CORRECTIVE_ACTION_REPLACE:
        begin
          Error.Get_Replacement(Replacement);
          Action := TSynSpellErrorReplace.Create(Self);
          Action.Caption := Replacement;
          MenuItem := TSpTBXItem.Create(Self);
          MenuItem.Action := Action;
          mnSpelling.Insert(0, MenuItem);
        end;
    end;
  end;
end;

procedure TCommandsDataModule.ShowSearchReplaceDialog(SynEdit: TSynEdit; AReplace: Boolean);
var
  Str: string;
begin
  EditorSearchOptions.InitSearch;
  with PyIDEMainForm do begin
    tbiReplaceSeparator.Visible := AReplace;
    tbiReplaceLabel.Visible := AReplace;
    tbiReplaceText.Visible := AReplace;
    tbiReplaceExecute.Visible := AReplace;
    if AReplace then begin
      tbiReplaceText.Text := EditorSearchOptions.ReplaceText;
      tbiReplaceText.Items.CommaText := EditorSearchOptions.ReplaceTextHistory;
      PyIDEMainForm.tbiReplaceTextChange(Self);
    end;
    FindToolbar.Visible := True;
    FindToolbar.View.CancelMode;
    // start with last search text
    tbiSearchText.Text := EditorSearchOptions.SearchText;
    if EditorSearchOptions.SearchTextAtCaret then
    begin
      // if something is selected search for that text
      if SynEdit.SelAvail and (SynEdit.BlockBegin.Line = SynEdit.BlockEnd.Line)
      then
        tbiSearchText.Text := SynEdit.SelText
      else begin
        Str := SynEdit.WordAtCursor;
        if Str <> '' then
          tbiSearchText.Text := Str;
      end;
    end;
    tbiSearchText.Items.CommaText := EditorSearchOptions.SearchTextHistory;
    PyIDEMainForm.tbiSearchTextChange(Self);

    if CanActuallyFocus(tbiSearchText) then
       tbiSearchText.SetFocus;
  end;
end;

procedure TCommandsDataModule.SynEditReplaceText(Sender: TObject; const ASearch,
  AReplace: string; Line, Column: Integer; var Action: TSynReplaceAction);
var
  APos: TPoint;
  EditRect: TRect;
  SynEdit: TSynEdit;
begin
  SynEdit := Sender as TSynEdit;
  if ASearch = AReplace then
    Action := raSkip
  else begin
    APos := SynEdit.ClientToScreen(
      SynEdit.RowColumnToPixels(
      SynEdit.BufferToDisplayPos(
      BufferCoord(Column, Line))));
    EditRect := SynEdit.ClientToScreen(SynEdit.ClientRect);

    if ConfirmReplaceDialog = nil then begin
      ConfirmReplaceDialog := TConfirmReplaceDialog.Create(Application);
      FConfirmReplaceDialogRect := ConfirmReplaceDialog.BoundsRect;
    end;
    if EqualRect(FConfirmReplaceDialogRect, ConfirmReplaceDialog.BoundsRect) then begin
      ConfirmReplaceDialog.PrepareShow(EditRect, APos.X, APos.Y,
        APos.Y + SynEdit.LineHeight, ASearch);
      FConfirmReplaceDialogRect := ConfirmReplaceDialog.BoundsRect;
    end else
      FConfirmReplaceDialogRect := Rect(0, 0, 0, 0);

    case ConfirmReplaceDialog.ShowModal of
      mrYes:      Action := raReplace;
      mrYesToAll: Action := raReplaceAll;
      mrNo:       Action := raSkip;
      else        Action := raCancel;
    end;
  end;
  EditorSearchOptions.LastReplaceAction := Action;
  if Action in [raSkip, raCancel] then
    Inc(EditorSearchOptions.NoReplaceCount);
end;

function TCommandsDataModule.ProgramVersionHTTPLocationLoadFileFromRemote(
  AProgramVersionLocation: TJvProgramVersionHTTPLocation; const ARemotePath,
  ARemoteFileName, ALocalPath, ALocalFileName: string): string;
var
  LocalFileName, URL: string;
begin
  Result := '';
  if (DirectoryExists(ALocalPath) or (ALocalPath = '')) then
    if ALocalFileName = '' then
      LocalFileName := TPath.Combine(ALocalPath, ARemoteFileName)
    else
      LocalFileName := TPath.Combine(ALocalPath, ALocalFileName)
  else
    Exit;

  if Copy(ARemotePath, Length(ARemotePath), 1) <> '/' then
    URL := ARemotePath + '/' + ARemoteFileName
  else
    URL := ARemotePath + ARemoteFileName;

  GI_PyIDEServices.SetActivityIndicator(True, _('Downloading...'));
  try
    if DownloadUrlToFile(URL, LocalFileName) and FileExists(LocalFileName) then
      Result := LocalFileName
    else
    begin
      if FileExists(LocalFileName) then
        DeleteFile(LocalFileName);
      ProgramVersionHTTPLocation.DownloadError := _('File download failed');
    end;
  finally
    GI_PyIDEServices.SetActivityIndicator(False);
  end;
end;

procedure TCommandsDataModule.PyIDEOptionsChanged(const Sender: TObject;
  const Msg: System.Messaging.TMessage);
begin
  // Syntax Code Completion
  SynCodeCompletion.Font.Assign(PyIDEOptions.AutoCompletionFont);
  with SynCodeCompletion do begin
    if PyIDEOptions.CodeCompletionCaseSensitive then
      Options := Options + [scoCaseSensitive]
    else
      Options := Options - [scoCaseSensitive];
    if PyIDEOptions.CompleteWithWordBreakChars then
      Options := Options + [scoEndCharCompletion]
    else
      Options := Options - [scoEndCharCompletion];

    TriggerChars := '.';
    TimerInterval := 200;
    if PyIDEOptions.CompleteAsYouType then begin
      for var I := ord('a') to ord('z') do TriggerChars := TriggerChars + Chr(I);
      for var I := ord('A') to ord('Z') do TriggerChars := TriggerChars + Chr(I);
      if PyIDEOptions.CompleteWithWordBreakChars or PyIDEOptions.CompleteWithOneEntry then
        TimerInterval := 500;
    end;
  end;

  // Syntax Parameter Completion
  SynParamCompletion.Font.Assign(PyIDEOptions.AutoCompletionFont);

  // Set Python engine
  actPythonInternal.Visible := not PyIDEOptions.InternalInterpreterHidden;
  if not actPythonInternal.Visible and
    (PyIDEOptions.PythonEngineType = peInternal)
  then
    PyIDEOptions.PythonEngineType := peRemote;

  // SpellCheck
  SynSpellCheck.BeginUpdate;
  try
    SynSpellCheck.CheckAsYouType := PyIDEOptions.SpellCheckAsYouType;
    SynSpellCheck.AttributesChecked.CommaText := PyIDEOptions.SpellCheckedTokens;
    SynSpellCheck.LanguageCode := PyIDEOptions.DictLanguage;
  finally
    SynSpellCheck.EndUpdate;
  end;

  TThread.ForceQueue(nil, procedure
  begin
    if SynSpellCheck.SpellChecker = nil then
      DSAMessageDlg(dsaDictonaryNA, 'PyScripter',
      Format(_(SDictionaryNA),
      [CommandsDataModule.SynSpellCheck.LanguageCode]),
      mtInformation, [mbOK], 0, dckActiveForm, 0, mbOK);
  end);
end;

class procedure TCommandsDataModule.RegisterActionList(ActionList: TActionList);
begin
  TActionProxyCollection.ActionLists := TActionProxyCollection.ActionLists +
    [ActionList];
end;

procedure TCommandsDataModule.spiAcceptSettings(Sender: TObject; var
    NewText: string; var Accept: Boolean);
begin
  Accept := False;
  try
    var Settings := LLMAssistant.Settings;
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

    case LLMAssistant.Providers.Provider of
      llmProviderOpenAI: LLMAssistant.Providers.OpenAI := Settings;
      llmProviderDeepSeek: LLMAssistant.Providers.DeepSeek := Settings;
      llmProviderGrok: LLMAssistant.Providers.Grok := Settings;
      llmProviderGemini: LLMAssistant.Providers.Gemini := Settings;
      llmProviderOllama: LLMAssistant.Providers.Ollama := Settings;
    end;

    Accept := True;
  except
    on E: Exception do
      StyledMessageDlg(E.Message, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  end;
end;

procedure TCommandsDataModule.spiSettingsInitPopup(Sender: TObject; PopupView:
    TTBView);
begin
  case LLMAssistant.Providers.Provider of
    llmProviderOpenAI: spiOpenAI.Checked := True;
    llmProviderDeepSeek: spiDeepSeek.Checked := True;
    llmProviderGrok: spiGrok.Checked := True;
    llmProviderGemini: spiGemini.Checked := True;
    llmProviderOllama: spiOllama.Checked := True;
  end;

  var Settings := LLMAssistant.Settings;
  spiEndpoint.Text := Settings.EndPoint;
  spiModel.Text := Settings.Model;
  spiApiKey.Text := Settings.ApiKey;
  spiTimeout.Text := (Settings.TimeOut div 1000).ToString;
  spiTemperature.Text := Format('%4.2f', [Settings.Temperature]);
  spiMaxTokens.Text := Settings.MaxTokens.ToString;
  spiSystemPrompt.Text := Settings.SystemPrompt;
end;

procedure TCommandsDataModule.SynSpellCheckChange(Sender: TObject);
begin
  PyIDEOptions.SpellCheckAsYouType := SynSpellCheck.CheckAsYouType;
end;

{ TSynGeneralSyn }

class function TSynGeneralSyn.GetFriendlyLanguageName: string;
begin
  Result := 'General';  // Do not localize
end;

initialization
  // gettext stuff
  // Classes that should not be translated
  TP_GlobalIgnoreClass(TCustomImageCollection);
  TP_GlobalIgnoreClass(TSVGIconImage);
  TP_GlobalIgnoreClass(TJvMultiStringHolder);
  TP_GlobalIgnoreClass(TSynEdit);
  TP_GlobalIgnoreClass(TSynCompletionProposal);
  TP_GlobalIgnoreClass(TSynAutoComplete);
  TP_GlobalIgnoreClass(TSpTBXMRUListItem);
  TP_GlobalIgnoreClass(TSynCustomHighlighter);
  TP_GlobalIgnoreClass(TCommonDialog);
  TP_GlobalIgnoreClass(TJvProgramVersionCheck);
  TP_GlobalIgnoreClass(TJvProgramVersionHTTPLocation);
  TP_GlobalIgnoreClass(TJvCustomAppStorage);
  TP_GlobalIgnoreClass(TPythonModule);
  // VCL stuff
  TP_GlobalIgnoreClass(TFont);
  TP_GlobalIgnoreClassProperty(TCustomAction,'Category');
  TP_GlobalIgnoreClassProperty(TCustomAction,'HelpKeyword');
  TP_GlobalIgnoreClassProperty(TObject,'ImageName');
  TP_GlobalIgnoreClassProperty(TControl,'HelpKeyword');
  TP_GlobalIgnoreClassProperty(TControl,'StyleName');
  TP_GlobalIgnoreClassProperty(TBrowseURL,'URL');

  //JCL Debug
  AddIgnoredException(EClipboardException);
end.





