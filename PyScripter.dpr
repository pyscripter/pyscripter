{-----------------------------------------------------------------------------
 Program:   PyScrtiper
 Author:    Kiriakos Vlahos
 Date:      19-Oct-2005
 Purpose:   Python IDE written with Python for Delphi
 History:
-----------------------------------------------------------------------------}


program PyScripter;
uses
  Windows,
  Forms,
  frmPyIDEMain in 'frmPyIDEMain.pas' {PyIDEMainForm},
  uEditAppIntfs in 'uEditAppIntfs.pas',
  frmEditor in 'frmEditor.pas' {EditorForm},
  dmCommands in 'dmCommands.pas' {CommandsDataModule: TDataModule},
  uHighlighterProcs in 'uHighlighterProcs.pas',
  dlgSearchText in 'dlgSearchText.pas' {TextSearchDialog},
  dlgReplaceText in 'dlgReplaceText.pas' {TextReplaceDialog},
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
  dlgDirectoryList in 'dlgDirectoryList.pas' {JvDirectoryListDialog},
  frmFileExplorer in 'frmFileExplorer.pas' {FileExplorerWindow},
  frmIDEDockWin in 'frmIDEDockWin.pas' {IDEDockWindow},
  frmDocView in 'frmDocView.pas' {DocForm},
  frmDisassemlyView in 'frmDisassemlyView.pas' {DisForm},
  dlgSynEditOptions in 'dlgSynEditOptions.pas' {fmEditorOptionsDialog},
  SynHighlighterPython in 'SynHighlighterPython.pas',
  frmToDo in 'frmToDo.pas' {ToDoWindow},
  dlgToDoOptions in 'dlgToDoOptions.pas' {fmToDoOptions},
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
  dlgConfigureTools in 'dlgConfigureTools.pas' {ConfigureTools},
  dlgToolProperties in 'dlgToolProperties.pas' {ToolProperties},
  frmCommandOutput in 'frmCommandOutput.pas' {OutputWindow},
  frmFunctionList in 'frmFunctionList.pas' {FunctionListWindow},
  cPythonSourceScanner in 'cPythonSourceScanner.pas',
  uCommonFunctions in 'uCommonFunctions.pas',
  StringResources in 'StringResources.pas',
  cRefactoring in 'cRefactoring.pas',
  StoHtmlHelp in 'StoHtmlHelp.pas',
  SynCompletionProposal in 'SynCompletionProposal.pas',
  frmRegExpTester in 'frmRegExpTester.pas' {RegExpTesterWindow},
  cCodeHint in 'cCodeHint.pas',
  dlgExceptionMail in 'dlgExceptionMail.pas' {ExceptionDialogMail},
  dlgCommandLine in 'dlgCommandLine.pas' {CommandLineDlg},
  dlgCustomShortcuts in 'dlgCustomShortcuts.pas' {frmCustomKeyboard},
  dlgUnitTestWizard in 'dlgUnitTestWizard.pas' {UnitTestWizard},
  frmUnitTests in 'frmUnitTests.pas' {UnitTestWindow},
  cFilePersist in 'cFilePersist.pas',
  dlgPickList in 'dlgPickList.pas' {PickListDialog},
  dlgAboutPyScripter in 'dlgAboutPyScripter.pas' {AboutBox},
  JvProgramVersionCheck in 'JvProgramVersionCheck.pas',
  JvChangeNotify in 'JvChangeNotify.pas',
  JvAppStorage in 'JvAppStorage.pas',
  cPyBaseDebugger in 'cPyBaseDebugger.pas',
  cPyRemoteDebugger in 'cPyRemoteDebugger.pas',
  JvCreateProcess in 'JvCreateProcess.pas',
  JvDockVSNetStyle in 'JvDockVSNetStyle.pas',
  JvDockControlForm in 'JvDockControlForm.pas',
  cFileTemplates in 'cFileTemplates.pas',
  dlgCodeTemplates in 'dlgCodeTemplates.pas' {CodeTemplates},
  JvAppIniStorage in 'JvAppIniStorage.pas',
  dlgNewFile in 'dlgNewFile.pas' {NewFileDialog},
  JvDockInfo in 'JvDockInfo.pas',
  JvTabBar in 'JvTabBar.pas';

{$R *.RES}

{$SetPEFlags IMAGE_FILE_RELOCS_STRIPPED}

begin
  Application.Initialize;
  Application.Title := 'PyScripter';
  Application.CreateForm(TCommandsDataModule, CommandsDataModule);
  Application.CreateForm(TPyIDEMainForm, PyIDEMainForm);
  Application.Run;
end.


