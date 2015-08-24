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
{$IF System.CompilerVersion >= 21}
  {$WEAKLINKRTTI ON}
  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF}

uses
  Windows,
  Forms,
  Dialogs,
  SysUtils,
  HTMLHelpViewer,
  VTAccessibility,
  uCmdLine in 'uCmdLine.pas',
  uDpiAware in 'uDpiAware.pas',
  dlgExceptionMail in 'dlgExceptionMail.pas' {ExceptionDialogMail},
  frmPyIDEMain in 'frmPyIDEMain.pas' {PyIDEMainForm},
  uEditAppIntfs in 'uEditAppIntfs.pas',
  frmEditor in 'frmEditor.pas' {EditorForm},
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
  JvProgramVersionCheck in 'JvProgramVersionCheck.pas',
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
  SynHighlighterWeb in 'SynHighlighterWeb.pas',
  SynEditTextBuffer in 'SynEditTextBuffer.pas',
  SynUnicode in 'SynUnicode.pas',
  gnugettext in 'gnugettext.pas',
  dlgPyIDEBase in 'dlgPyIDEBase.pas' {PyIDEDlgBase},
  JvDockInfo in 'JvDockInfo.pas',
  SynHighlighterYAML in 'SynHighlighterYAML.pas',
  VirtualTrees in 'VirtualTrees.pas',
  dlgSynEditOptions in 'dlgSynEditOptions.pas' {fmEditorOptionsDialog: TForm},
  JvDockSupportControl in 'JvDockSupportControl.pas',
  AsyncCalls in 'AsyncCalls.pas',
  SpTBXItem in 'SpTBXItem.pas',
  TB2Item in 'TB2Item.pas',
  JvDockVIDStyle in 'JvDockVIDStyle.pas',
  JvCreateProcess in 'JvCreateProcess.pas',
  cCodeCompletion in 'cCodeCompletion.pas',
  cThemedVirtualStringTree in 'cThemedVirtualStringTree.pas',
  SpTBXPageScroller in 'SpTBXPageScroller.pas',
  SpTBXSkins in 'SpTBXSkins.pas',
  SynEdit,
  JvInspector,
  Vcl.StdCtrls,
  Vcl.Themes,
  Vcl.Styles,
//  Vcl.Styles.UxTheme,
//  Vcl.Styles.Utils.StdCtrls,
  Vcl.Styles.Utils.ComCtrls,
//  Vcl.Styles.Utils.ScreenTips,
//  Vcl.Styles.Utils.SysControls,
//  Vcl.Styles.Utils.SysStyleHook,
//  Vcl.Styles.Utils.Forms,
  Vcl.Styles.Utils.SystemMenu;

{$R *.RES}
{$R WebCopyAvi.RES}
{$R XP_UAC.RES}


{$SetPEFlags IMAGE_FILE_RELOCS_STRIPPED}

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;

  UseLatestCommonDialogs := False;
  TStyleManager.Engine.RegisterStyleHook(TCustomSynEdit, TMemoStyleHook);
  TStyleManager.Engine.RegisterStyleHook(TJvInspector, TMemoStyleHook);

  Application.Initialize;

  if CheckWin32Version(6) then // at least Vista
    begin
      Application.DefaultFont.Name := 'Segoe UI';
      Application.DefaultFont.Size := 9;
    end;
  Application.MainFormOnTaskbar := True;

  TStyleManager.TrySetStyle('Jet');
  Application.Title := 'PyScripter';
  Application.CreateForm(TCommandsDataModule, CommandsDataModule);
  Application.CreateForm(TPyIDEMainForm, PyIDEMainForm);
  TVclStylesSystemMenu.Create(PyIDEMainForm);
  Application.Run;

  TStyleManager.Engine.UnRegisterStyleHook(TCustomSynEdit, TMemoStyleHook);
  TStyleManager.Engine.UnRegisterStyleHook(TJvInspector, TMemoStyleHook);

  end.


