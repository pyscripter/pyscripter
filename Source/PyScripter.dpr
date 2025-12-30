  {-----------------------------------------------------------------------------
 Program:   PyScripter
 Author:    Kiriakos Vlahos
 Date:      19-Oct-2005
 Purpose:   Python IDE written with Python for Delphi
 History:
-----------------------------------------------------------------------------}
// JCL_DEBUG_EXPERT_GENERATEJDBG ON
// JCL_DEBUG_EXPERT_INSERTJDBG ON
// JCL_DEBUG_EXPERT_DELETEMAPFILE ON
program PyScripter;
{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}

uses
  WinApi.Windows,
  System.SysUtils,
  Vcl.HTMLHelpViewer,
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  VirtualTrees.Accessibility,
  uCmdLine in 'uCmdLine.pas',
  dlgExceptionMail in 'dlgExceptionMail.pas' {ExceptionDialogMail},
  frmPyIDEMain in 'frmPyIDEMain.pas' {PyIDEMainForm},
  uEditAppIntfs in 'uEditAppIntfs.pas',
  frmEditor in 'frmEditor.pas' {EditorForm},
  dmResources in 'dmResources.pas' {ResourcesDataModule: TDataModule},
  dmCommands in 'dmCommands.pas' {CommandsDataModule: TDataModule},
  uHighlighterProcs in 'uHighlighterProcs.pas',
  dlgConfirmReplace in 'dlgConfirmReplace.pas' {ConfirmReplaceDialog},
  frmPythonII in 'frmPythonII.pas' {PythonIIForm},
  frmMessages in 'frmMessages.pas' {MessagesWindow},
  cPyDebugger in 'cPyDebugger.pas',
  dlgSynPageSetup in 'dlgSynPageSetup.pas' {PageSetupDlg},
  dlgSynPrintPreview in 'dlgSynPrintPreview.pas' {PrintPreviewDlg},
  frmCallStack in 'frmCallStack.pas' {CallStackWindow},
  frmBreakPoints in 'frmBreakPoints.pas' {BreakPointsWindow},
  frmWatches in 'frmWatches.pas' {WatchesWindow},
  frmVariables in 'frmVariables.pas' {VariablesWindow},
  frmCodeExplorer in 'frmCodeExplorer.pas' {CodeExplorerWindow},
  dlgOptionsEditor in 'dlgOptionsEditor.pas' {OptionsInspector},
  dlgDirectoryList in 'dlgDirectoryList.pas' {DirectoryListDialog},
  frmFileExplorer in 'frmFileExplorer.pas' {FileExplorerWindow},
  frmIDEDockWin in 'frmIDEDockWin.pas' {IDEDockWindow},
  frmDisassemlyView in 'frmDisassemlyView.pas' {DisForm},
  frmDocView in 'frmDocView.pas' {DocForm},
  frmWebPreview in 'frmWebPreview.pas' {WebPreviewForm},
  SynHighlighterPython in 'SynHighlighterPython.pas',
  frmToDo in 'frmToDo.pas' {ToDoWindow},
  dlgToDoOptions in 'dlgToDoOptions.pas' {ToDoOptionsDlg},
  cFileSearch in 'cFileSearch.pas',
  cFindInFiles in 'cFindInFiles.pas',
  dlgFindInFiles in 'dlgFindInFiles.pas' {FindInFilesDialog},
  frmFindResults in 'frmFindResults.pas' {FindResultsWindow},
  dlgFindResultsOptions in 'dlgFindResultsOptions.pas' {FindResultsOptionsDialog},
  dlgReplaceInFiles in 'dlgReplaceInFiles.pas' {ReplaceInFilesDialog},
  cParameters in 'cParameters.pas',
  uParams in 'uParams.pas',
  dlgCustomParams in 'dlgCustomParams.pas' {CustomizeParams},
  dlgAskParam in 'dlgAskParam.pas' {AskParamForm},
  dlgFileTemplates in 'dlgFileTemplates.pas' {FileTemplatesDialog},
  cTools in 'cTools.pas',
  dlgCollectionEditor in 'dlgCollectionEditor.pas' {CollectionEditor},
  dlgToolProperties in 'dlgToolProperties.pas' {ToolProperties},
  frmCommandOutput in 'frmCommandOutput.pas' {OutputWindow},
  frmFunctionList in 'frmFunctionList.pas' {FunctionListWindow},
  uCommonFunctions in 'uCommonFunctions.pas',
  StringResources in 'StringResources.pas',
  SynCompletionProposal in 'SynCompletionProposal.pas',
  frmRegExpTester in 'frmRegExpTester.pas' {RegExpTesterWindow},
  cCodeHint in 'cCodeHint.pas',
  dlgCommandLine in 'dlgCommandLine.pas' {CommandLineDlg},
  dlgCustomShortcuts in 'dlgCustomShortcuts.pas' {frmCustomKeyboard},
  dlgUnitTestWizard in 'dlgUnitTestWizard.pas' {UnitTestWizard},
  frmUnitTests in 'frmUnitTests.pas' {UnitTestWindow},
  cFilePersist in 'cFilePersist.pas',
  dlgPickList in 'dlgPickList.pas' {PickListDialog},
  dlgAboutPyScripter in 'dlgAboutPyScripter.pas' {AboutBox},
  cPyBaseDebugger in 'cPyBaseDebugger.pas',
  cPyRemoteDebugger in 'cPyRemoteDebugger.pas',
  cFileTemplates in 'cFileTemplates.pas',
  dlgCodeTemplates in 'dlgCodeTemplates.pas' {CodeTemplates},
  dlgNewFile in 'dlgNewFile.pas' {NewFileDialog},
  JvDockVSNetStyle in 'JvDockVSNetStyle.pas',
  JvDockControlForm in 'JvDockControlForm.pas',
  uSearchHighlighter in 'uSearchHighlighter.pas',
  frmModSpTBXCustomize in 'frmModSpTBXCustomize.pas',
  cProjectClasses in 'cProjectClasses.pas',
  frmProjectExplorer in 'frmProjectExplorer.pas' {ProjectExplorerWindow},
  dlgImportDirectory in 'dlgImportDirectory.pas' {ImportDirectoryForm},
  dlgRunConfiguration in 'dlgRunConfiguration.pas' {RunConfigurationForm},
  dlgPyIDEBase in 'dlgPyIDEBase.pas' {PyIDEDlgBase},
  JvDockInfo in 'JvDockInfo.pas',
  dlgSynEditOptions in 'dlgSynEditOptions.pas' {EditorOptionsDialog: TForm},
  JvDockSupportControl in 'JvDockSupportControl.pas',
  JvDockVIDStyle in 'JvDockVIDStyle.pas',
  cCodeCompletion in 'cCodeCompletion.pas',
  dlgStyleSelector in 'dlgStyleSelector.pas' {StyleSelectorForm},
  VCL.Styles.PyScripter in 'VCL.Styles.PyScripter.pas' {/  Vcl.Styles.Utils.Forms;},
  cAppPaths in 'cAppPaths.pas',
  cPyScripterSettings in 'cPyScripterSettings.pas',
  cPySupportTypes in 'cPySupportTypes.pas',
  cPyControl in 'cPyControl.pas',
  JvGnugettext in 'JvGnugettext.pas',
  cInternalPython in 'cInternalPython.pas',
  dlgPythonVersions in 'dlgPythonVersions.pas' {PythonVersionsDialog},
  cSSHSupport in 'cSSHSupport.pas',
  dlgRemoteFile in 'dlgRemoteFile.pas' {RemoteFileDialog},
  cPySSHDebugger in 'cPySSHDebugger.pas',
  RtlVclFixes in 'RtlVclFixes.pas',
  JvDockAdvTree in 'JvDockAdvTree.pas',
  JvDockSupportProc in 'JvDockSupportProc.pas',
  JvDockTree in 'JvDockTree.pas',
  JvDockVSNetStyleSpTBX in 'JvDockVSNetStyleSpTBX.pas',
  SynEditLsp in 'SynEditLsp.pas',
  uLLMSupport in 'uLLMSupport.pas',
  frmLLMChat in 'frmLLMChat.pas' {LLMChatForm},
  frmSuggest in 'frmSuggest.pas' {SuggestWindow},
  cLSPClients in 'cLSPClients.pas',
  uPythonItfs in 'uPythonItfs.pas';

{$R *.RES}

{$SetPEFlags IMAGE_FILE_RELOCS_STRIPPED
  or IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP
  or IMAGE_FILE_NET_RUN_FROM_SWAP}

begin
  ReportMemoryLeaksOnShutdown :=  DebugHook <> 0;

  TStyleManager.SystemHooks := TStyleManager.SystemHooks - [shMenus, shDialogs];

  Application.Initialize;
  SetDefaultUIFont(Application.DefaultFont);

  Application.MainFormOnTaskbar := True;

  if TStyleManager.TrySetStyle('Windows11 MineShaft') then
    TStyleSelectorForm.CurrentSkinName := 'Windows11 MineShaft';

  Application.Title := 'PyScripter';
  Application.CreateForm(TResourcesDataModule, ResourcesDataModule);
  Application.CreateForm(TCommandsDataModule, CommandsDataModule);
  Application.CreateForm(TPyIDEMainForm, PyIDEMainForm);
  Application.Run;

 end.


