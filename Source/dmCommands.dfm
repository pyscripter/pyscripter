object CommandsDataModule: TCommandsDataModule
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 280
  Width = 509
  object SynEditPrint: TSynEditPrint
    Copies = 1
    Header.DefaultFont.Charset = DEFAULT_CHARSET
    Header.DefaultFont.Color = clBlack
    Header.DefaultFont.Height = -13
    Header.DefaultFont.Name = 'Arial'
    Header.DefaultFont.Style = []
    Footer.DefaultFont.Charset = DEFAULT_CHARSET
    Footer.DefaultFont.Color = clBlack
    Footer.DefaultFont.Height = -13
    Footer.DefaultFont.Name = 'Arial'
    Footer.DefaultFont.Style = []
    Margins.Left = 25.000000000000000000
    Margins.Right = 15.000000000000000000
    Margins.Top = 25.000000000000000000
    Margins.Bottom = 25.000000000000000000
    Margins.Header = 18.000000000000000000
    Margins.Footer = 18.000000000000000000
    Margins.LeftHFTextIndent = 2.000000000000000000
    Margins.RightHFTextIndent = 2.000000000000000000
    Margins.HFInternalMargin = 0.500000000000000000
    Margins.MirrorMargins = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TabWidth = 8
    Color = clWhite
    Left = 244
    Top = 68
  end
  object SynEditSearch: TSynEditSearch
    Left = 147
    Top = 67
  end
  object SynEditRegexSearch: TSynEditRegexSearch
    Left = 41
    Top = 68
  end
  object ProgramVersionCheck: TJvProgramVersionCheck
    AllowedReleaseType = prtAlpha
    AppStoragePath = 'Check for Updates'
    CheckFrequency = 0
    LocalDirectory = 'Updates'
    LocalVersionInfoFileName = 'versioninfo.ini'
    LocationHTTP = ProgramVersionHTTPLocation
    LocationType = pvltHTTP
    UserOptions = [uoCheckFrequency, uoLocalDirectory, uoAllowedReleaseType, uoLocationHTTP]
    VersionHistoryFileOptions.INIOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    VersionHistoryFileOptions.INIOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    VersionHistoryFileOptions.INIOptions.SetAsString = True
    VersionHistoryFileOptions.INIOptions.FloatAsString = True
    VersionHistoryFileOptions.INIOptions.DefaultIfReadConvertError = True
    VersionHistoryFileOptions.XMLOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    VersionHistoryFileOptions.XMLOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    VersionHistoryFileOptions.XMLOptions.SetAsString = True
    VersionHistoryFileOptions.XMLOptions.FloatAsString = True
    VersionHistoryFileOptions.XMLOptions.DefaultIfReadConvertError = True
    VersionHistoryFileOptions.XMLOptions.UseOldItemNameFormat = False
    VersionHistoryFileOptions.XMLOptions.WhiteSpaceReplacement = '_'
    VersionHistoryFileOptions.XMLOptions.InvalidCharReplacement = '_'
    Left = 51
    Top = 7
  end
  object ProgramVersionHTTPLocation: TJvProgramVersionHTTPLocation
    OnLoadFileFromRemote = ProgramVersionHTTPLocationLoadFileFromRemote
    VersionInfoLocationPathList.Strings = (
      'https://raw.githubusercontent.com/pyscripter/pyscripter/master')
    VersionInfoFileName = 'PyScripterVersionInfo.ini'
    Left = 206
    Top = 8
  end
  object actlMain: TActionList
    Images = PyIDEMainForm.vilImages
    Left = 13
    Top = 165
    object actFileSave: TAction
      Category = 'File'
      Caption = '&Save'
      Enabled = False
      HelpContext = 310
      Hint = 'Save|Save active file'
      ImageIndex = 2
      ImageName = 'Save'
      ShortCut = 16467
      OnExecute = actFileSaveExecute
    end
    object actFileSaveAs: TAction
      Category = 'File'
      Caption = 'Sav&e As...'
      Enabled = False
      HelpContext = 310
      Hint = 'Save As|Save active file under different name'
      OnExecute = actFileSaveAsExecute
    end
    object actFileClose: TAction
      Category = 'File'
      Caption = '&Close'
      Enabled = False
      HelpContext = 310
      Hint = 'Close|Close active file'
      ImageIndex = 92
      ImageName = 'TabCLose'
      ShortCut = 16499
      OnExecute = actFileCloseExecute
    end
    object actEditCut: TEditCut
      Category = 'Edit'
      Caption = 'Cu&t'
      Enabled = False
      HelpContext = 320
      HelpType = htContext
      Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
      ImageIndex = 9
      ImageName = 'Cut'
      ShortCut = 16472
    end
    object actEditCopy: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      Enabled = False
      HelpContext = 320
      HelpType = htContext
      Hint = 'Copy|Copies the selection and puts it on the Clipboard'
      ImageIndex = 10
      ImageName = 'Copy'
      ShortCut = 16451
    end
    object actEditPaste: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      HelpContext = 320
      HelpType = htContext
      Hint = 'Paste|Inserts Clipboard contents'
      ImageIndex = 11
      ImageName = 'Paste'
      ShortCut = 16470
    end
    object actEditDelete: TEditDelete
      Category = 'Edit'
      Caption = 'De&lete'
      Enabled = False
      HelpContext = 320
      HelpType = htContext
      Hint = 'Delete|Delete selection'
      ImageIndex = 12
      ImageName = 'Delete'
    end
    object actEditUndo: TEditUndo
      Category = 'Edit'
      Caption = '&Undo'
      Enabled = False
      HelpContext = 320
      HelpType = htContext
      Hint = 'Undo|Reverts the last action'
      ImageIndex = 7
      ImageName = 'Undo'
      ShortCut = 16474
    end
    object actEditRedo: TAction
      Category = 'Edit'
      Caption = '&Redo'
      Enabled = False
      HelpContext = 320
      Hint = 'Redo| Redo last action'
      ImageIndex = 8
      ImageName = 'Redo'
      ShortCut = 24666
      OnExecute = actEditRedoExecute
    end
    object actEditSelectAll: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select &All'
      HelpContext = 320
      HelpType = htContext
      Hint = 'Select All|Selects the entire document'
      ShortCut = 16449
    end
    object actEditReadOnly: TAction
      Category = 'Edit'
      Caption = 'Read Only'
      HelpContext = 320
      Hint = 'Enable/disable editing'
      OnExecute = actEditReadOnlyExecute
    end
    object actSearchFind: TAction
      Category = 'Search'
      Caption = '&Find...'
      Enabled = False
      HelpContext = 330
      Hint = 'Search|Search for a string'
      ImageIndex = 13
      ImageName = 'Search'
      ShortCut = 16454
      OnExecute = actSearchFindExecute
    end
    object actSearchFindNext: TAction
      Category = 'Search'
      Caption = 'Find &Next'
      Enabled = False
      HelpContext = 330
      Hint = 'Find next|Find next match'
      ImageIndex = 14
      ImageName = 'FindNext'
      ShortCut = 114
      OnExecute = actSearchFindNextExecute
    end
    object actSearchFindPrev: TAction
      Category = 'Search'
      Caption = 'Find &Previous'
      Enabled = False
      HelpContext = 330
      Hint = 'Find previous|Find Previous match'
      ImageIndex = 76
      ImageName = 'FindPrevious'
      ShortCut = 8306
      OnExecute = actSearchFindPrevExecute
    end
    object actSearchReplace: TAction
      Category = 'Search'
      Caption = '&Replace...'
      Enabled = False
      HelpContext = 330
      Hint = 'Replace|Search & Replace'
      ImageIndex = 15
      ImageName = 'Replace'
      ShortCut = 16456
      OnExecute = actSearchReplaceExecute
    end
    object actFileSaveAll: TAction
      Category = 'File'
      Caption = 'Save &All'
      HelpContext = 310
      Hint = 'Save all|Save project and all open files'
      ImageIndex = 3
      ImageName = 'SaveAll'
      OnExecute = actFileSaveAllExecute
    end
    object actFilePrint: TAction
      Category = 'File'
      Caption = '&Print...'
      Enabled = False
      HelpContext = 310
      Hint = 'Print|Print active file'
      ImageIndex = 6
      ImageName = 'Print'
      ShortCut = 16464
      OnExecute = actFilePrintExecute
    end
    object actPrinterSetup: TAction
      Category = 'File'
      Caption = 'Printer Set&up...'
      HelpContext = 310
      Hint = 'Printer setup'
      ImageIndex = 4
      ImageName = 'PrintSetup'
      OnExecute = actPrinterSetupExecute
    end
    object actPrintPreview: TAction
      Category = 'File'
      Caption = 'Print Pre&view'
      HelpContext = 310
      Hint = 'Print preview'
      ImageIndex = 5
      ImageName = 'PrintPreview'
      OnExecute = actPrintPreviewExecute
    end
    object actPageSetup: TAction
      Category = 'File'
      Caption = 'Pa&ge Setup...'
      HelpContext = 310
      Hint = 'Page setup'
      ImageIndex = 52
      ImageName = 'PageSetup'
      OnExecute = actPageSetupExecute
    end
    object actEditorOptions: TAction
      Category = 'Options'
      Caption = '&Editor Options...'
      HelpContext = 620
      Hint = 'Set Editor Options'
      ImageIndex = 18
      ImageName = 'EditOptions'
      OnExecute = actEditorOptionsExecute
    end
    object actIDEOptions: TAction
      Category = 'Options'
      Caption = '&IDE Options...'
      HelpContext = 610
      Hint = 'Set IDE Options'
      ImageIndex = 19
      ImageName = 'AppSettings'
      OnExecute = actIDEOptionsExecute
    end
    object actEditIndent: TAction
      Category = 'Source Code'
      Caption = '&Indent Block'
      HelpContext = 320
      Hint = 'Indent block|Indent selected block of code'
      ImageIndex = 45
      ImageName = 'Indent'
      ShortCut = 24649
      OnExecute = actEditIndentExecute
    end
    object actEditDedent: TAction
      Category = 'Source Code'
      Caption = '&Unindent Block'
      HelpContext = 320
      Hint = 'Unindent|Unindent selected block of code'
      ImageIndex = 46
      ImageName = 'Dedent'
      ShortCut = 24661
      OnExecute = actEditDedentExecute
    end
    object actEditCommentOut: TAction
      Category = 'Source Code'
      Caption = '&Comment out'
      HelpContext = 320
      Hint = 'Comment out| Comment out block of code'
      ImageIndex = 47
      ImageName = 'CodeComment'
      ShortCut = 49342
      OnExecute = actEditCommentOutExecute
    end
    object actEditUncomment: TAction
      Category = 'Source Code'
      Caption = '&Uncomment'
      HelpContext = 320
      Hint = 'Uncomment| Uncomment block of code'
      ImageIndex = 48
      ImageName = 'UnCodeComment'
      ShortCut = 49340
      OnExecute = actEditUncommentExecute
    end
    object actSearchMatchingBrace: TAction
      Category = 'Search'
      Caption = '&Matching Brace'
      HelpContext = 330
      Hint = 'Find Matching Brace'
      ShortCut = 16605
      OnExecute = actSearchMatchingBraceExecute
    end
    object actEditTabify: TAction
      Category = 'Source Code'
      Caption = '&Tabify'
      HelpContext = 320
      Hint = 'Tabify|Convert spaces to tabs'
      ShortCut = 32990
      OnExecute = actEditTabifyExecute
    end
    object actEditUntabify: TAction
      Category = 'Source Code'
      Caption = 'U&ntabify'
      HelpContext = 320
      Hint = 'Untabify|Convert tabs to spaces'
      ShortCut = 41182
      OnExecute = actEditUntabifyExecute
    end
    object actPythonPath: TAction
      Category = 'Tools'
      Caption = 'Python &Path...'
      HelpContext = 870
      Hint = 'Python Path|View or edit the Python path'
      ImageIndex = 20
      ImageName = 'Folders'
      OnExecute = actPythonPathExecute
    end
    object actHelpContents: THelpContents
      Category = 'Help'
      Caption = '&Contents'
      Enabled = False
      HelpContext = 370
      HelpType = htContext
      Hint = 'Help Contents'
      ImageIndex = 27
      ImageName = 'Help'
      OnExecute = actHelpContentsExecute
    end
    object actPythonManuals: THelpContents
      Category = 'Help'
      Caption = '&Python Manuals'
      HelpContext = 370
      HelpType = htContext
      Hint = 'Show Python Manuals'
      ImageIndex = 51
      ImageName = 'PyDoc'
      OnExecute = actPythonManualsExecute
    end
    object actAbout: TAction
      Category = 'Help'
      Caption = 'About...'
      HelpContext = 370
      Hint = 'About|Info about the application'
      ImageIndex = 25
      ImageName = 'Info'
      OnExecute = actAboutExecute
    end
    object actSearchGoToLine: TAction
      Category = 'Search'
      Caption = 'Go To &Line...'
      HelpContext = 330
      Hint = 'Go to line number'
      ImageIndex = 26
      ImageName = 'GoToLine'
      ShortCut = 32839
      OnExecute = actSearchGoToLineExecute
    end
    object actSearchGoToSyntaxError: TAction
      Category = 'Search'
      Caption = 'Go To Syntax &Error'
      HelpContext = 330
      Hint = 'Jump to the position of the first syntax error'
      ImageIndex = 78
      ImageName = 'GoToError'
      ShortCut = 24645
      OnExecute = actSearchGoToSyntaxErrorExecute
    end
    object actFindInFiles: TAction
      Category = 'Search'
      Caption = 'Find &in Files...'
      HelpContext = 330
      Hint = 'Search in Files|Search for a string in Files'
      ImageIndex = 59
      ImageName = 'SearchFolder'
      ShortCut = 24646
      OnExecute = actFindInFilesExecute
    end
    object actParameterCompletion: TAction
      Category = 'Parameters'
      Caption = 'Insert &parameter'
      HelpContext = 720
      Hint = 'Insert parameter to the edited file'
      ShortCut = 24656
      OnExecute = actParameterCompletionExecute
    end
    object actModifierCompletion: TAction
      Category = 'Parameters'
      Caption = 'Insert &modifier'
      HelpContext = 720
      Hint = 'Insert parameter to the edited file'
      ShortCut = 24653
      OnExecute = actModifierCompletionExecute
    end
    object actReplaceParameters: TAction
      Category = 'Parameters'
      Caption = '&Replace parameters'
      HelpContext = 720
      Hint = 'Replace parameters with their values'
      ShortCut = 24658
      OnExecute = actReplaceParametersExecute
    end
    object actHelpParameters: TAction
      Category = 'Help'
      Caption = '&Parameters'
      HelpContext = 370
      Hint = 'Help on custom parameters'
      OnExecute = actHelpParametersExecute
    end
    object actInsertTemplate: TAction
      Category = 'Edit'
      Caption = 'Insert &Template'
      HelpContext = 320
      Hint = 'Insert a Code Template'
      ShortCut = 16458
      OnExecute = actInsertTemplateExecute
    end
    object actCustomizeParameters: TAction
      Category = 'Parameters'
      Caption = 'Custom &Parameters...'
      HelpContext = 720
      Hint = 'Add/Remove custom parameters'
      OnExecute = actCustomizeParametersExecute
    end
    object actIDEShortcuts: TAction
      Category = 'Options'
      Caption = 'IDE &Shortcuts...'
      HelpContext = 615
      Hint = 'Customize IDE shortcuts'
      ImageIndex = 67
      ImageName = 'Keyboard'
      OnExecute = actIDEShortcutsExecute
    end
    object actCodeTemplates: TAction
      Category = 'Options'
      Caption = '&Code Templates...'
      HelpContext = 540
      Hint = 'Add/Remove code templates'
      OnExecute = actCodeTemplatesExecute
    end
    object actConfigureTools: TAction
      Category = 'Tools'
      Caption = 'Configure &Tools...'
      HelpContext = 710
      Hint = 'Configure Tools|Add/remove/edit command-line tools'
      ImageIndex = 56
      ImageName = 'ToolsSetup'
      OnExecute = actConfigureToolsExecute
    end
    object actHelpExternalTools: TAction
      Category = 'Help'
      Caption = 'External &Tools'
      HelpContext = 370
      Hint = 'Help on External Tools'
      OnExecute = actHelpExternalToolsExecute
    end
    object actFindFunction: TAction
      Category = 'Search'
      Caption = 'Find F&unction...'
      HelpContext = 330
      Hint = 'Find Function|Find function from function list'
      ImageIndex = 21
      ImageName = 'Function'
      ShortCut = 16455
      OnExecute = actFindFunctionExecute
    end
    object actEditLineNumbers: TAction
      Category = 'Edit'
      Caption = 'Line &Numbers'
      HelpContext = 320
      Hint = 'Show/Hide line numbers'
      ImageIndex = 31
      ImageName = 'LineNumbers'
      OnExecute = actEditLineNumbersExecute
    end
    object actEditShowSpecialChars: TAction
      Category = 'Edit'
      Caption = 'Special &Characters'
      HelpContext = 320
      Hint = 'Show/Hide special characters'
      ImageIndex = 63
      ImageName = 'SpecialChars'
      OnExecute = actEditShowSpecialCharsExecute
    end
    object actFindPreviousReference: TAction
      Tag = 1
      Category = 'Search'
      Caption = 'Find Previous Reference'
      HelpContext = 330
      Hint = 'Find previous identifier reference'
      ShortCut = 49190
      OnExecute = actFindNextReferenceExecute
    end
    object actFindNextReference: TAction
      Category = 'Search'
      Caption = 'Find Next Reference'
      HelpContext = 330
      Hint = 'Find next identifier reference'
      ShortCut = 49192
      OnExecute = actFindNextReferenceExecute
    end
    object actEditLBDos: TAction
      Category = 'Edit'
      Caption = '&DOS/Windows'
      Checked = True
      HelpContext = 320
      Hint = 'DOS/Windows|Convert to DOS line break'
      OnExecute = actEditLBExecute
    end
    object actEditLBUnix: TAction
      Tag = 1
      Category = 'Edit'
      Caption = '&UNIX'
      HelpContext = 320
      Hint = 'UNIX|Convert to UNIX line break'
      OnExecute = actEditLBExecute
    end
    object actEditLBMac: TAction
      Tag = 2
      Category = 'Edit'
      Caption = '&Mac'
      HelpContext = 320
      Hint = 'Mac|Convert to Mac line break'
      OnExecute = actEditLBExecute
    end
    object actEditAnsi: TAction
      Category = 'Edit'
      Caption = 'ANSI'
      Checked = True
      HelpContext = 320
      Hint = 'Use ANSI encoding'
      OnExecute = actEditFileEncodingExecute
    end
    object actHelpEditorShortcuts: TAction
      Category = 'Help'
      Caption = 'Editor &Shortcuts'
      HelpContext = 370
      Hint = 'Help on editor shortcuts'
      OnExecute = actHelpEditorShortcutsExecute
    end
    object actCheckForUpdates: TAction
      Category = 'Tools'
      Caption = 'Check For &Updates'
      HelpContext = 350
      Hint = 'Check whether a newer version of PyScripter is available'
      OnExecute = actCheckForUpdatesExecute
    end
    object actUnitTestWizard: TAction
      Category = 'Tools'
      Caption = 'Unit Test &Wizard...'
      HelpContext = 930
      Hint = 'Unit test wizard|Create unit test for active module'
      ImageIndex = 68
      ImageName = 'UnitTestWin'
      OnExecute = actUnitTestWizardExecute
    end
    object actInterpreterEditorOptions: TAction
      Category = 'Options'
      Caption = '&Interpreter Editor Options...'
      HelpContext = 620
      Hint = 'Set Interpreter Editor Options'
      ImageIndex = 18
      ImageName = 'EditOptions'
      OnExecute = actInterpreterEditorOptionsExecute
    end
    object actEditToggleComment: TAction
      Category = 'Source Code'
      Caption = 'Toggle &Comment'
      HelpContext = 320
      Hint = 'Toggle Comment| Comment/Uncomment block of code'
      ImageIndex = 47
      ImageName = 'CodeComment'
      ShortCut = 16606
      OnExecute = actEditToggleCommentExecute
    end
    object actFileTemplates: TAction
      Category = 'Options'
      Caption = '&File Templates...'
      HelpContext = 640
      Hint = 'Add/Remove file templates'
      OnExecute = actFileTemplatesExecute
    end
    object actEditUTF8: TAction
      Tag = 1
      Category = 'Edit'
      Caption = 'UTF-8 with BOM'
      HelpContext = 320
      Hint = 'Use UTF-8 encoding when saving the file'
      OnExecute = actEditFileEncodingExecute
    end
    object actEditUTF8NoBOM: TAction
      Tag = 2
      Category = 'Edit'
      Caption = 'UTF-8'
      HelpContext = 320
      Hint = 'Use UTF-8 encoding without BOM'
      OnExecute = actEditFileEncodingExecute
    end
    object actEditUTF16LE: TAction
      Tag = 3
      Category = 'Edit'
      Caption = 'UTF-16LE'
      HelpContext = 320
      Hint = 'Use UTF-16LE encoding'
      OnExecute = actEditFileEncodingExecute
    end
    object actEditUTF16BE: TAction
      Tag = 4
      Category = 'Edit'
      Caption = 'UTF-16BE'
      HelpContext = 320
      Hint = 'Use UTF-16BE encoding'
      OnExecute = actEditFileEncodingExecute
    end
    object actFileReload: TAction
      Category = 'File'
      Caption = 'Re&load'
      Enabled = False
      HelpContext = 310
      Hint = 'Reload|Reload active file'
      ImageIndex = 29
      ImageName = 'Refresh'
      OnExecute = actFileReloadExecute
    end
    object actImportShortcuts: TAction
      Category = 'Import/Export'
      Caption = 'Import Shortcuts'
      Hint = 'Import Shortcuts'
      OnExecute = actImportShortcutsExecute
    end
    object actExportShortCuts: TAction
      Category = 'Import/Export'
      Caption = 'Export Shortcuts'
      Hint = 'Export Shortcuts'
      OnExecute = actExportShortCutsExecute
    end
    object actImportHighlighters: TAction
      Category = 'Import/Export'
      Caption = 'Import Highlighters'
      Hint = 'Import Syntax Highlighters'
      OnExecute = actImportHighlightersExecute
    end
    object actExportHighlighters: TAction
      Category = 'Import/Export'
      Caption = 'Export Highlighters'
      Hint = 'Export Syntax Highlighters'
      OnExecute = actExportHighlightersExecute
    end
    object actSearchReplaceNow: TAction
      Category = 'Search'
      Caption = 'Replace'
      Enabled = False
      HelpContext = 330
      Hint = 'Replace with existing settings'
      ImageIndex = 15
      ImageName = 'Replace'
      OnExecute = actSearchReplaceNowExecute
    end
    object actSearchHighlight: TAction
      Category = 'Search'
      AutoCheck = True
      Caption = '&Highlight Search Text'
      HelpContext = 330
      Hint = 'Highlight the search text in the current editor'
      ImageIndex = 77
      ImageName = 'Highlight'
      ShortCut = 24648
      OnExecute = actSearchHighlightExecute
    end
    object actEditWordWrap: TAction
      Category = 'Edit'
      Caption = 'Word &Wrap'
      HelpContext = 320
      Hint = 'Turn word wrap on/off (incompatible with code folding)'
      ImageIndex = 79
      ImageName = 'WordWrap'
      OnExecute = actEditWordWrapExecute
    end
    object actSearchGoToDebugLine: TAction
      Category = 'Search'
      Caption = 'Go To &Debugger Position'
      HelpContext = 330
      Hint = 'Go to the current position of the debugger'
      OnExecute = actSearchGoToDebugLineExecute
    end
    object actHelpWebProjectHome: TAction
      Category = 'Help'
      Caption = '&Project Home'
      HelpContext = 370
      Hint = 'Go to the project home page'
      ImageIndex = 90
      ImageName = 'Link'
      OnExecute = actHelpWebProjectHomeExecute
    end
    object actHelpWebGroupSupport: TAction
      Category = 'Help'
      Caption = '&Group Support'
      HelpContext = 370
      Hint = 'Go to the PyScripter Internet group'
      ImageIndex = 90
      ImageName = 'Link'
      OnExecute = actHelpWebGroupSupportExecute
    end
    object actFileCloseAllOther: TAction
      Tag = 1
      Category = 'File'
      Caption = 'Close All &Other'
      HelpContext = 310
      Hint = 'Close all files except the active one'
      OnExecute = actFileCloseWorkspaceTabsExecute
    end
    object actFileCloseAllToTheRight: TAction
      Category = 'File'
      Caption = 'Close All to the &Right'
      HelpContext = 310
      Hint = 'Close all files to the right'
      OnExecute = actFileCloseWorkspaceTabsExecute
    end
    object actEditCopyFileName: TAction
      Category = 'Edit'
      Caption = 'Copy File Name'
      HelpContext = 320
      HelpType = htContext
      Hint = 'Copy file name of active file to clipboard'
      ImageIndex = 10
      ImageName = 'Copy'
      OnExecute = actEditCopyFileNameExecute
    end
    object actToolsEditStartupScripts: TAction
      Category = 'Tools'
      Caption = 'Edit &Startup Scripts'
      HelpContext = 350
      HelpType = htContext
      Hint = 'Edit PyScripter initialization files'
      OnExecute = actToolsEditStartupScriptsExecute
    end
    object actHelpWebBlog: TAction
      Category = 'Help'
      Caption = '&Blog'
      HelpContext = 370
      Hint = 'Go to the PyScripter Blog'
      ImageIndex = 90
      ImageName = 'Link'
      OnExecute = actHelpWebBlogExecute
    end
    object actFoldVisible: TAction
      Category = 'Code Folding'
      Caption = 'Code Folding'
      Hint = 'Show/Hide Code Folding'
      OnExecute = actFoldVisibleExecute
    end
    object actFoldAll: TAction
      Category = 'Code Folding'
      Caption = 'All'
      Hint = 'Fold all'
      ImageIndex = 24
      ImageName = 'Collapse'
      OnExecute = actFoldAllExecute
    end
    object actUnfoldAll: TAction
      Category = 'Code Folding'
      Caption = 'All'
      Hint = 'Unfold all'
      ImageIndex = 23
      ImageName = 'Expand'
      OnExecute = actUnfoldAllExecute
    end
    object actFoldNearest: TAction
      Category = 'Code Folding'
      Caption = 'Nearest'
      Hint = 'Collapse nearest fold'
      OnExecute = actFoldNearestExecute
    end
    object actUnfoldNearest: TAction
      Category = 'Code Folding'
      Caption = 'Nearest'
      Hint = 'Expand nearest fold'
      OnExecute = actUnfoldNearestExecute
    end
    object actFoldRegions: TAction
      Category = 'Code Folding'
      Caption = 'Regions'
      Hint = 'Fold Ranges'
      OnExecute = actFoldRegionsExecute
    end
    object actUnfoldRegions: TAction
      Category = 'Code Folding'
      Caption = 'Regions'
      Hint = 'Unfold ranges'
      OnExecute = actUnfoldRegionsExecute
    end
    object actFoldLevel1: TAction
      Category = 'Code Folding'
      Caption = 'Level 1'
      Hint = 'Fold level 1'
      OnExecute = actFoldLevel1Execute
    end
    object actUnfoldLevel1: TAction
      Category = 'Code Folding'
      Caption = 'Level 1'
      Hint = 'Unfold level 1'
      OnExecute = actUnfoldLevel1Execute
    end
    object actFoldLevel2: TAction
      Category = 'Code Folding'
      Caption = 'Level 2'
      Hint = 'Fold level 2'
      OnExecute = actFoldLevel2Execute
    end
    object actUnfoldLevel2: TAction
      Category = 'Code Folding'
      Caption = 'Level 2'
      Hint = 'Unfold level 2'
      OnExecute = actUnfoldLevel2Execute
    end
    object actFoldLevel3: TAction
      Category = 'Code Folding'
      Caption = 'Level 3'
      Hint = 'Fold level 3'
      OnExecute = actFoldLevel3Execute
    end
    object actUnfoldLevel3: TAction
      Category = 'Code Folding'
      Caption = 'Level 3'
      Hint = 'Unfold level 3'
      OnExecute = actUnfoldLevel3Execute
    end
    object actFoldClasses: TAction
      Category = 'Code Folding'
      Caption = 'Classes'
      Hint = 'Fold classes'
      OnExecute = actFoldClassesExecute
    end
    object actUnfoldClasses: TAction
      Category = 'Code Folding'
      Caption = 'Classes'
      Hint = 'Unfold classes'
      OnExecute = actUnfoldClassesExecute
    end
    object actFoldFunctions: TAction
      Category = 'Code Folding'
      Caption = 'Functions'
      Hint = 'Fold functions'
      OnExecute = actFoldFunctionsExecute
    end
    object actUnfoldFunctions: TAction
      Category = 'Code Folding'
      Caption = 'Functions'
      Hint = 'Unfold functions'
      OnExecute = actUnfoldFunctionsExecute
    end
    object actFileSaveToRemote: TAction
      Category = 'File'
      Caption = 'Save to Remote File'
      Hint = 'Save to remote file with SSH'
      ImageIndex = 96
      ImageName = 'Upload'
      OnExecute = actFileSaveToRemoteExecute
    end
    object actDonate: TAction
      Category = 'Help'
      Caption = '&Donate'
      HelpContext = 370
      Hint = 'Donate to the PyScripter project'
      ImageIndex = 90
      ImageName = 'Link'
      OnExecute = actDonateExecute
    end
    object actToolsRestartLS: TAction
      Category = 'Tools'
      Caption = 'Restart &Language Server'
      HelpContext = 350
      HelpType = htContext
      Hint = 'Restart the Language Server'
      OnExecute = actToolsRestartLSExecute
    end
    object actSynSpellCheckFile: TSynSpellCheckFile
      Category = 'Spell Checking'
      Caption = 'Check File'
    end
    object actSynSpellCheckLine: TSynSpellCheckLine
      Category = 'Spell Checking'
      Caption = 'Check Line'
    end
    object actSynSpellCheckSelection: TSynSpellCheckSelection
      Category = 'Spell Checking'
      Caption = 'Check Selection'
    end
    object actSynSpellCheckWord: TSynSpellCheckWord
      Category = 'Spell Checking'
      Caption = 'Check Word'
    end
    object actSynSpellClearErrors: TSynSpellClearErrors
      Category = 'Spell Checking'
      Caption = 'Clear Errors'
    end
    object actSynSpellCheckAsYouType: TSynSpellCheckAsYouType
      Category = 'Spell Checking'
      Caption = 'Check As You Type'
    end
    object actSynSpellErrorAdd: TSynSpellErrorAdd
      Category = 'Spell Checking'
      Caption = 'Add'
    end
    object actSynSpellErrorIgnoreOnce: TSynSpellErrorIgnoreOnce
      Category = 'Spell Checking'
      Caption = 'Ignore Once'
    end
    object actSynSpellErrorIgnore: TSynSpellErrorIgnore
      Category = 'Spell Checking'
      Caption = 'Ignore'
    end
    object actSynSpellErrorDelete: TSynSpellErrorDelete
      Category = 'Spell Checking'
      Caption = 'Delete'
    end
  end
  object SynWebCompletion: TSynCompletionProposal
    Options = [scoLimitToMatchedText, scoUseInsertList, scoUsePrettyText, scoUseBuiltInTimer, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter]
    Width = 340
    EndOfTokenChr = ';>()[] .'
    TriggerChars = '<'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    GripperFont.Charset = DEFAULT_CHARSET
    GripperFont.Color = clBtnText
    GripperFont.Height = -36
    GripperFont.Name = 'Segoe UI'
    GripperFont.Style = []
    Columns = <
      item
        ColumnWidth = 100
      end>
    Resizeable = True
    ShortCut = 0
    Left = 380
    Top = 104
  end
  object SynParamCompletion: TSynCompletionProposal
    DefaultType = ctParams
    Options = [scoCaseSensitive, scoLimitToMatchedText, scoUseInsertList, scoUsePrettyText, scoUseBuiltInTimer, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter]
    EndOfTokenChr = '()[]. ='
    TriggerChars = '('
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    GripperFont.Charset = DEFAULT_CHARSET
    GripperFont.Color = clBtnText
    GripperFont.Height = -36
    GripperFont.Name = 'Segoe UI'
    GripperFont.Style = []
    Columns = <>
    ShortCut = 0
    TimerInterval = 300
    Left = 380
    Top = 63
  end
  object SynCodeCompletion: TSynCompletionProposal
    Options = [scoCaseSensitive, scoLimitToMatchedText, scoUseInsertList, scoUsePrettyText, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter]
    Width = 200
    EndOfTokenChr = '()[]{}. =:'
    TriggerChars = '.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    GripperFont.Charset = DEFAULT_CHARSET
    GripperFont.Color = clBtnText
    GripperFont.Height = -36
    GripperFont.Name = 'Segoe UI'
    GripperFont.Style = []
    Columns = <>
    Resizeable = True
    ShortCut = 0
    TimerInterval = 300
    Left = 380
    Top = 17
  end
  object SynSpellCheck: TSynSpellCheck
    AttributesChecked.Strings = (
      'Comment'
      'Text'
      'String'
      'Documentation')
    CheckAsYouType = False
    OnChange = SynSpellCheckChange
    Left = 384
    Top = 168
  end
end
