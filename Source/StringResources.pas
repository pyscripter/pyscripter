unit StringResources;

interface
{gnugettext: scan-all}

Const
  // File Filters
  sPythonFileFilter = 'Python Files (*.py;*.pyw;*.pyi)|*.py;*.pyw;*.pyi';
  sCythonFileFilter = 'Cython Files (*.pyx*.pxd;*.pxi)|*.pyx;*.pxd;*.pxi';
  sHTMLFileFilter = 'HTML Documents (*.htm;*.html)|*.htm;*.html';
  sXMLFileFilter = 'XML Files (*.xml;*.xsd;*.xsl;*.xslt;*.dtd)|*.xml;*.xsd;*.xsl;*.xslt;*.dtd';
  sCSSFileFilter = 'Cascading Stylesheets (*.css)|*.css';
  sCPPFileFilter = 'C/C++ Files (*.c;*.cpp;*.cc;*.h;*.hpp;*.hh;*.cxx;*.hxx;*.cu)|*.c;*.cpp;*.cc;*.h;*.hpp;*.hh;*.cxx;*.hxx;*.cu';
  sYAMLFileFilter = 'YAML files (*.yaml)|*.yaml';
  sJSFileFilter = 'Javascript Files (*.js)|*.js';
  sPHPFileFilter = 'PHP Files (*.php;*.php3;*.phtml;*.inc)|*.php;*.php3;*.phtml;*.inc';
  sJSONFileFilter = 'JSON Files (*.json;*.ipynb)|*.json;*.ipynb';
  sGeneralFileFilter = 'Text Files(*.txt,*.*)|*.txt;*.*';

  // Syntax element friendly names
  SyntaxElementFriendlyName_Anchor               =  'Anchor';
  SyntaxElementFriendlyName_Assembler            =  'Assembler';
  SyntaxElementFriendlyName_Attribute            =  'Attribute';
  SyntaxElementFriendlyName_Brackets             =  'Brackets';
  SyntaxElementFriendlyName_Character            =  'Character';
  SyntaxElementFriendlyName_Directive            =  'Directive';
  SyntaxElementFriendlyName_Documentation        =  'Documentation';
  SyntaxElementFriendlyName_DocumentDelimiter    =  'Document Delimiter';
  SyntaxElementFriendlyName_Float                =  'Float';
  SyntaxElementFriendlyName_Hexadecimal          =  'Hexadecimal';
  SyntaxElementFriendlyName_Identifier           =  'Identifier';
  SyntaxElementFriendlyName_IllegalChar          =  'Illegal Char';
  SyntaxElementFriendlyName_Key                  =  'Key';
  SyntaxElementFriendlyName_NonReservedKeyword   =  'Non-reserved Keyword';
  SyntaxElementFriendlyName_Number               =  'Number';
  SyntaxElementFriendlyName_NumericValue         =  'Numeric Value';
  SyntaxElementFriendlyName_Octal                =  'Octal';
  SyntaxElementFriendlyName_Preprocessor         =  'Preprocessor';
  SyntaxElementFriendlyName_ReservedWord         =  'Reserved Word';
  SyntaxElementFriendlyName_Section              =  'Section';
  SyntaxElementFriendlyName_Space                =  'Space';
  SyntaxElementFriendlyName_String               =  'String';
  SyntaxElementFriendlyName_Symbol               =  'Symbol';
  SyntaxElementFriendlyName_SyntaxError          =  'Syntax Error';
  SyntaxElementFriendlyName_System               =  'System Functions and Variables';
  SyntaxElementFriendlyName_Tag                  =  'Tag';
  SyntaxElementFriendlyName_Text                 =  'Text';
  SyntaxElementFriendlyName_TextValue            =  'Text Value';
  SyntaxElementFriendlyName_Value                =  'Value';

  // Editor scroll hints
  sEditorScrollInfo         =  'Top Line: %d';
  sPrintPreviewScrollInfo   =  'Page: %d';

  // Editor commands
  //Needs to be manually updated as new editor commands are added
  SEdCmdAutoCompletion = 'Auto Completion';
  SEdCmdBlockIndent = 'Block Indent';
  SEdCmdBlockUnindent = 'Block Unindent';
  SEdCmdClearAll = 'Clear All';
  SEdCmdColumnSelect = 'Column Select';
  SEdCmdCommentBlock = 'Comment Block';
  SEdCmdContextHelp = 'Context Help';
  SEdCmdCopy = 'Copy';
  SEdCmdCopyLineDown = 'Copy Line Down';
  SEdCmdCopyLineUp = 'Copy Line Up';
  SEdCmdCut = 'Cut';
  SEdCmdDeleteBOL = 'Delete BOL';
  SEdCmdDeleteChar = 'Delete Char';
  SEdCmdDeleteEOL = 'Delete EOL';
  SEdCmdDeleteLastChar = 'Delete Last Char';
  SEdCmdDeleteLastWord = 'Delete Last Word';
  SEdCmdDeleteLine = 'Delete Line';
  SEdCmdDeleteWord = 'Delete Word';
  SEdCmdDown = 'Down';
  SEdCmdEditorBottom = 'Editor Bottom';
  SEdCmdEditorTop = 'Editor Top';
  SEdCmdFoldAll = 'Fold All';
  SEdCmdFoldLevel1 = 'Fold Level 1';
  SEdCmdFoldLevel2 = 'Fold Level 2';
  SEdCmdFoldLevel3 = 'Fold Level 3';
  SEdCmdFoldNearest = 'Fold Nearest';
  SEdCmdFoldRanges = 'Fold Ranges';
  SEdCmdGotoBookmark0 = 'Goto Bookmark 0';
  SEdCmdGotoBookmark1 = 'Goto Bookmark 1';
  SEdCmdGotoBookmark2 = 'Goto Bookmark 2';
  SEdCmdGotoBookmark3 = 'Goto Bookmark 3';
  SEdCmdGotoBookmark4 = 'Goto Bookmark 4';
  SEdCmdGotoBookmark5 = 'Goto Bookmark 5';
  SEdCmdGotoBookmark6 = 'Goto Bookmark 6';
  SEdCmdGotoBookmark7 = 'Goto Bookmark 7';
  SEdCmdGotoBookmark8 = 'Goto Bookmark 8';
  SEdCmdGotoBookmark9 = 'Goto Bookmark 9';
  SEdCmdInsertLine = 'Insert Line';
  SEdCmdInsertMode = 'Insert Mode';
  SEdCmdLeft = 'Left';
  SEdCmdLineBreak = 'Line Break';
  SEdCmdLineEnd = 'Line End';
  SEdCmdLineSelect = 'Line Select';
  SEdCmdLineStart = 'Line Start';
  SEdCmdLowerCase = 'Lower Case';
  SEdCmdMatchBracket = 'Match Bracket';
  SEdCmdMoveLineDown = 'Move Line Down';
  SEdCmdMoveLineUp = 'Move Line Up';
  SEdCmdNormalSelect = 'Normal Select';
  SEdCmdOverwriteMode = 'Overwrite Mode';
  SEdCmdPageBottom = 'Page Bottom';
  SEdCmdPageDown = 'Page Down';
  SEdCmdPageLeft = 'Page Left';
  SEdCmdPageRight = 'Page Right';
  SEdCmdPageTop = 'Page Top';
  SEdCmdPageUp = 'Page Up';
  SEdCmdPaste = 'Paste';
  SEdCmdRedo = 'Redo';
  SEdCmdRight = 'Right';
  SEdCmdScrollDown = 'Scroll Down';
  SEdCmdScrollLeft = 'Scroll Left';
  SEdCmdScrollRight = 'Scroll Right';
  SEdCmdScrollUp = 'Scroll Up';
  SEdCmdSelectAll = 'Select All';
  SEdCmdSelectDown = 'Select Down';
  SEdCmdSelectEditorBottom = 'Select Editor Bottom';
  SEdCmdSelectEditorTop = 'Select Editor Top';
  SEdCmdSelectLeft = 'Select Left';
  SEdCmdSelectLineEnd = 'Select Line End';
  SEdCmdSelectLineStart = 'Select Line Start';
  SEdCmdSelectPageBottom = 'Select Page Bottom';
  SEdCmdSelectPageDown = 'Select Page Down';
  SEdCmdSelectPageLeft = 'Select Page Left';
  SEdCmdSelectPageRight = 'Select Page Right';
  SEdCmdSelectPageTop = 'Select Page Top';
  SEdCmdSelectPageUp = 'Select Page Up';
  SEdCmdSelectRight = 'Select Right';
  SEdCmdSelectUp = 'Select Up';
  SEdCmdSelectWord = 'Select Word';
  SEdCmdSelectWordLeft = 'Select Word Left';
  SEdCmdSelectWordRight = 'Select Word Right';
  SEdCmdSetBookmark0 = 'Set Bookmark 0';
  SEdCmdSetBookmark1 = 'Set Bookmark 1';
  SEdCmdSetBookmark2 = 'Set Bookmark 2';
  SEdCmdSetBookmark3 = 'Set Bookmark 3';
  SEdCmdSetBookmark4 = 'Set Bookmark 4';
  SEdCmdSetBookmark5 = 'Set Bookmark 5';
  SEdCmdSetBookmark6 = 'Set Bookmark 6';
  SEdCmdSetBookmark7 = 'Set Bookmark 7';
  SEdCmdSetBookmark8 = 'Set Bookmark 8';
  SEdCmdSetBookmark9 = 'Set Bookmark 9';
  SEdCmdShiftTab = 'Shift Tab';
  SEdCmdTab = 'Tab';
  SEdCmdTitleCase = 'Title Case';
  SEdCmdToggleCase = 'Toggle Case';
  SEdCmdToggleInsertMode = 'Toggle Mode';
  SEdCmdUndo = 'Undo';
  SEdCmdUnfoldAll = 'Unfold All';
  SEdCmdUnfoldLevel1 = 'Unfold Level 1';
  SEdCmdUnfoldLevel2 = 'Unfold Level 2';
  SEdCmdUnfoldLevel3 = 'Unfold Level 3';
  SEdCmdUnfoldNearest = 'Unfold Nearest';
  SEdCmdUnfoldRanges = 'Unfold Ranges';
  SEdCmdUp = 'Up';
  SEdCmdUpperCase = 'Upper Case';
  SEdCmdWordLeft = 'Word Left';
  SEdCmdWordRight = 'Word Right';
  // User editor commands
  SEdCmdCodeCompletion = 'Code Completion';
  SEdCmdParameterCompletion = 'Parameter Completion';
  SEdCmdSelectToBracket = 'Select Match Bracket';

  // Delphi dialogs (from Vcl.Consts)
  srNone = '(None)';
  SDlgWarning = 'Warning';
  SDlgError = 'Error';
  SDlgInformation = 'Information';
  SDlgConfirm = 'Confirm';
  SDlgYes = '&Yes';
  SDlgNo = '&No';
  SDlgOK = 'OK';
  SDlgCancel = 'Cancel';
  SDlgHelp = '&Help';
  SDlgHelpHelp = 'Help';
  SDlgAbort = '&Abort';
  SDlgRetry = '&Retry';
  SDlgIgnore = '&Ignore';
  SDlgAll = '&All';
  SDlgNoToAll = 'N&o to All';
  SDlgYesToAll = 'Yes to &All';
  SDlgClose = '&Close';

  STrueValue = 'True';
  SFalseValue = 'False';

  SDSActkShowText = 'Do not show this dialog again';
  SDSActkRememberText = 'Remember answer and do not show again';

  SInternalError = 'Internal Error in %s';
  SNotFound = '"%s" not found';
  SItemsReplaced = '%d items found and %d items replaced';
  SNotAvailable = 'n/a';
  SNotImplented = 'Not implemented';
  SFailedToBackupFile = 'Failed to backup file "%s"';
  SErrorLoadLayout = 'Failed to load layout from "%s". Please close and restart PyScripter';
  SFilterAllFiles = 'All files|*.*|';
  SNoParameters = '** No/Unknown parameters **';
  SInvalidRegularExpression = 'Invalid Regular Expression: %s';
  SEmptyList = '(Empty List)';
  SCommandLineMsg  = 'Command Line : %s' + sLineBreak;
  SEngineActive = '*** %s Python engine is active ***';
  SInterpreterNA = 'The internal Python interpreter is not available';
  SPythonLoadError = 'PyScripter could not load a Python engine.' + SLineBreak +
  '**Before** using PyScripter, you must ensure that a version of Python ' +
  'greater or equal to %s is installed on your machine.' + SLineBreak +
  'If you do not have one installed, you can download one from http://www.python.org/.'
  + SLineBreak +  SLineBreak +
  'The 64-bit version of PyScripter (x64) works only on 64-bit Windows **and** with 64-bit versions of Python.'
   + SLineBreak +
  'The 32-bit version of PyScripter works on both 32-bit and 64-bit Windows with 32-bit versions of Python.';
  SPythonFindError =  'PyScripter could not find a usable Python installation at the specified path.' + SLineBreak +
  'Note that the 64-bit version of PyScripter (x64) works only on 64-bit Windows **and**' + SLineBreak +
  'with 64-bit versions of Python. The 32-bit version of PyScripter works on both' + SLineBreak +
  '32-bit and 64-bit Windows with 32-bit versions of Python.';
  SPostMortemInfo = 'You are now in post-mortem analysis mode.' + SLineBreak +
                    'You can examine the Call Stack, ' +
                    'Variables and Watches windows, evaluate expressions etc.' + SLineBreak +
                    'To exit the post-mortem analysis select the ''Abort Debugging'' command.';
  SFileEncodingWarning = 'Encoding file "%s" using "%s" encoding will ' +
                        'result in information loss.' + SLineBreak + 'Do you want to proceed?';
  SDecodingError = 'Error in decoding file "%s" from "%s" encoding';
  SFileOpenError = 'Error in opening file: "%s".' + sLineBreak + 'Error: %s';
  SFileSaveError = 'Error in saving file: "%s".' + sLineBreak +'Error: %s';
  SErrorGettingNamespace = 'Error in getting the namespace of %s';
  SCouldNotSetDirectory = 'Could not set the current directory to the script path';
  SCannotCompileWhileRunning = 'You cannot compile, import or run modules while debugging or running programs';
  SErrorInImportingModule = 'Error in importing module';
  SCouldNotSetCurrentDir = 'Could not set the current directory to the script path';
  SRemoteServerNotConnected = 'Remote Server is not connected.' + SLineBreak +
                              'Please reinitialize or disconnect the remote interpreter.';
  SRpycNotAvailable = 'The ''rpyc'' package is not available.' + SLineBreak +
                      'Please restart PyScripter and try again.' + SLineBreak +
                      'If the error persists reinstall PyScripter.';
  SCouldNotWriteServerFile = 'Could not write file "%s" and cannot use a remote Python engine';
  SErrorCreatingRemoteEngine = 'Error in creating the remote interpreter: ';
  SCouldNotConnectRemoteEngine = 'Could not connect to the remote Python engine server. '+
        'The remote interpreter and debugger is not available.';
  SRemoteInterpreterInit = '*** Remote Interpreter Reinitialized ***';
  SSameName = 'Another item has the same name';
  SCodeTemplateModified = 'The template has been modified.' + SLineBreak +
                          'Do you want to update it with the new definition?';
  SInvalidNumber = 'Invalid number!';
  SEmptyTokenTextError = 'You cannot insert a token that only consists of white space.';
  SAccessAppDataDir = 'Pyscripter needs access to the User Application Data directory: "%s". '+
                      'Certain features such as remote debugging will not function properly otherwise.';
  SNotAValidNumber = '"%s" is not a valid integer';
  SGoToLineNumber = 'Go to line number';
  SEnterLineNumber = 'Enter line number:';
  SFileRenamedOrDeleted = '%s has been renamed or deleted.';
  SRegistryAccessDenied = 'Registry access denied. Could not change the file association.';
  SErrorWhileDownload = 'Error while downloading: ';
  SCurrentVersionUptodate = 'Current version is up-to-date!';
  SKillExternalTool = 'An External Tool is still running.' + SLineBreak +
                      'Do you want to terminate it and exit?';
  STerminateInterpreter = 'The Python interpreter is busy.' + SLineBreak +
                          'Are you sure you want to terminate it?';
  SFindDefinitionWarning = 'This is the definition of "%s"';
  SPlaceCursorOnName = 'Please place the cursor on a function/class name or identifier';
  SCannotChangeEngine = 'Cannot change the Python engine while it is active.';
  SCouldNotOpenOutputFile = 'Could not open/create output file %s';
  SUnknownPythonVersion = 'PyScripter can''t use command line parameter PYTHON%s because it doesn''t know this version of Python.';
  SUnsupportedPythonVersion = 'PyScripter can''t use command line parameter PYTHON%s because it was compiled for Python %s or later.';
  SAbortDebugging = 'A debugging session is in progress.' + SLineBreak +
                    'Do you want to abort the session and Exit?';
  SInterruptRunningScript = 'The Python Script has timed out.' + SLineBreak +
                            'Do you want to interrupt it?';
  SNoTestsFound = 'No tests found!';
  SDuplicateKey = 'Duplicate Key';
  SChangedFilesReloaded = 'Changed files have been reloaded';
  SFileChangeNotification = 'File Change Notification';
  SFileReloadWarning = 'The following files have been changed on disk.'+
      ' Select the files that you wish to reload and press the OK button.'+
      ' Please note that you will lose all changes to these files.';
  StrScriptRunOK = 'Script run OK';
  SDebuggingAborted = 'Debugging Aborted';
  SSyntaxError = 'Syntax Error';
  StrCertainty = ' Certainty %s%%';
  SReferencesNotFound = ' References not found';
  SReferencesOf = 'References of "';
  SDefinitionsOf = 'Definition(s) of "';
  SDefinitionFound = ' Definition found';
  SDefinitionNotFound = ' Definition not found';
  SSyntaxIsOK = 'Syntax of %s is OK!';
  SModuleImportedOK = 'Module %s was imported successfully!';
  SResumeCaption = 'Resume';
  SResumeHint = 'Resume|Resume the running script';
  SDebugHint = 'Debug|Debug active script';
  SHintRun = 'Run last script';
  SHintDebug = 'Debug last script';
  SHintExternalRun = 'Run last script externally';
  SModified = 'Modified';
  SOpenFile = 'Open File';
  SSelectPythonScript = 'Select Python script to run';
  SSelectOutputFile = 'Select output file';
  SSelectApplication = 'Select application or file to execute';
  SAddFilesToProject = 'Add File(s) to Project';
  SOpenProject = 'Open Project';
  SApplyLayout = 'Apply %s layout';
  SUseSyntax = 'Use %s syntax';
  SSwitchtoVersion = 'Switch to %s';
  SThemeHint = 'Set theme to "%s".';
  SSaveCurrentLayout = 'Save current Layout';
  SLayoutName = 'Layout Name:';
  SEditBreakpointCond = 'Edit Breakpoint Condition';
  SEnterPythonExpression = 'Enter Python expression:';
  SDeleteLayouts = 'Delete Layouts';
  SSelectLayouts = 'Please select the layouts you want to delete and press the OK button:';
  SSaveModifiedFiles = 'Save modified files';
  SSelectModifiedFiles = 'The following files have been modified.' + SLineBreak +
    'Press ''OK'' to save selected files and exit.'+ SLineBreak +
    'Press ''Cancel'' to return to PyScripter without saving files.';
  SImportHighlighters = 'Import Highlighters';
  SImportShortcuts = 'Import Shortcuts';
  SSaveAs = 'Save "%s" As';
  SSaveFileAs = 'Save File As';
  SExportHighlighters = 'Export Highlighters';
  SExportShortcuts = 'Export Shortcuts';
  SIDEOptions = 'IDE Options';
  SDisassembly = 'Dis&assembly';
  SDisassemblyHint = 'Disassembly|Disassembly View';
  SDisassemblyTab = 'Disassembly';
  SDocumentationHint = 'Documentation|Generate HTML documentation';
  SDocumentation = '&Documentation';
  SDocTab = 'Documentation';
  SWebPreviewHint = 'Web Preview|Preview HTML in browser';
  SWebPreview = '&Web Preview';
  SWebPreviewTab = 'Browser';
  SSourceTabCaption = 'Source';
  SErrorInitScript = 'Error in running initialization script %s: "%s"';
  SOpenDialogFilter = 'Open dialog %s filter';
  SOnlyJupyterFiles  = 'Web preview is only available for Jupyter JSON files';
  SNoJupyter = 'The ''jupyter'' package is not available.' + SLineBreak + 'Please install ''jupyter'' first.';
  SWebView2Error = 'The WebView2 control creation failed.' + SLineBreak +  'Please install the WebView2 runtime';
  SExternalProcessRunning = 'An external process is still running.' + SLineBreak +
                            'Please terminate it first from the ''Output'' window.';
  SDictionaryNA = 'The dictionary for language code "%s" is not available.' + sLineBreak +
                  'You can get spell checking dictionaries through the Windows language settings.' + sLineBreak +
                  'You can change the active language through the IDE options dialog.';

  //  Project Manager
  SAskSaveProject = 'The active project has not been saved.' + SLineBreak +
                    'Do you want to save the changes?';
  SFailedToBackupProject = 'Failed to backup project "%s"';
  SErrorInOpeningProject = 'Error in opening project: "%s".';
  SErrorInSavingProject = 'Error in saving project: "%s".';
  SSaveProjectAs = 'Save Project As';
  SSaveProjectFileAs = 'Save Project "%s" As';
  SProjectPythonPath = 'Project Python Path';

  // Search and Replace
  SStartReached = 'The starting point of the search was reached';
  SContinueSearch = 'Do you want to continue %s %s?';
  STheSearchAndReplace = 'the search and replace';
  STheSearch = 'the search';
  SFromTheEnd = 'from the end';
  SFromTheStart = 'from the start';
  SReachedTheStart = 'You reached the start %s';
  SReachedTheEnd = 'You reached the end %s';
  SOfTheSelection = 'of the selection';
  SOfTheDocument = 'of the document';
  SSearchDirectoryDoesNotExist = 'The search directory %s does not exist';

  // Find in Files Results
  SGrepReplaceStats = 'Replaced %d occurrence(s) in %.2f seconds';
  SAllMatchedFiles = 'All matched files';
  SOnLine = 'On line: ';
  SItemMatch = '%5d matches';
  SGrepActive = 'A Grep search is currently active; either abort it or wait until it is finished.';
  SGrepSearchStats = 'Searched %d files in %.2f seconds';
  SMatches = '%d matches';
  SMatchContextNotAvail = 'Unable to load match context lines';
  SProcessing = 'Processing %s';
  SFileChangedAbort = '%s' + sLineBreak + 'has changed since it was searched. Replacement aborted.'
    + sLineBreak + 'Expected: %s' + sLineBreak + 'Found: %s';
  SFileSkipped   = 'The following file will be skipped: ';
  SCouldNotBackup= 'Could not backup file "%s" and will skip it';

  // Command Line Options
  SParseError = 'Parse error at this position: ';
  SParseErrorOptions = 'you are not allowed to use command line options starting with -';
  SParseInvalidOptions = 'The following command line options are valid: '#13#10;
  SCommandLineOptions = 'PyScripter Command Line Options';

  // Command Window
  SProcessTerminated         = 'Process "%s" terminated, ExitCode: %.8x';
  SDirNotFound               = 'Directory "%s" does not exists';
  SProcessRunning            = 'One process is still running, stop it first.';
  SPrintCommandLine          = 'Command line: %s';
  SPrintWorkingDir           = 'Working directory: ';
  SExternalToolStillRunning  = 'The External Tool "%s" is still running. Do you want to terminate it?';

  // Editor
  SInsert = 'Insert';
  SOverwrite = 'Overwrite';
  SReadOnly = 'Read Only';
  SNonameFileTitle = 'Untitled';
  SNonamePythonFileTitle = 'module';
  SAskSaveChanges = 'The text in the "%s" file has changed.'#13#10#13#10 + 'Do you want to save the modifications?';
  SFileReloadingWarning = 'Reloading the file will result in the loss of all changes.' + SLineBreak +
                          'Do you want to proceed?';
  SFileAlreadyOpen = 'Another editor with the same file is open.' + SLineBreak +
                     'You can not have two editors with the same file.';

  // Parameters
  SEnterParameterCaption = 'Enter parameter value';
  SEnterParameterText = 'Parameter value';
  SParamCircularReference = 'Parameter "%s" is referenced circularly';
  SParameterNotFound = 'Parameter with name "%s" is not found';
  SModifierNotFound = 'Modifier with name "%s" is not found';
  SObjectNotFound = 'Object with name "%s" is not found';
  SPropertyNotFound = 'Object "%s" does not have registered property with name "%s"';
  SInvalidObjectProperty  = 'Object "%s" does not have property with name "%s"';
  SInvalidParameterFormat = '"%s" is not valid parameter format';
  SInvalidConditionFormat = 'Invalid condition format';
  SDuplicateModifier = 'Duplicate Modifier "%s"';

  // Tools
  SPackageName = 'Package Name';

  // Refactoring
  SCouldNotLoadModule = 'Could not load and parse module: "%s"';
  SNoIdentifier = 'No Identifier at the given line and column';
  SCouldNotFindScope = 'Could not find scope for the given line';
  STypeOfSIsUnknown = 'Type of "%s" is unknown';
  SCyclicImports = 'Cyclic imports encountered!';
  SCyclicTypeDependency = 'Cyclic type dependency encountered!';
  SCouldNotFindIdent = 'Could not find identifier "%s" in module "%s"';
  SCouldNotAnalyseModule = 'Could not analyse module: "%s"';
  SCouldNotFindModule = 'Could not find module: "%s"';
  SSelfOutsideClassScope = '"self" or "cls" used outside a class scope';
  SCouldNotFindIdentInScope = 'Could not find identifier "%s" in scope "%s"';
  SCouldNotInferType = 'Could not infer type from expresson "%s"';
  SCouldInferFunctionReturnType = 'Could not infer return type of function "%s"';
  SRefactoryEngineBusy = 'Refactoring engine is busy';
  SPythonKeyword = 'Python keyword';

  // File Templates
  SFileTemplateCategoryInternet = 'Internet';
  SFileTemplateCategoryOther = 'Other';
  SPythonTemplateName = 'Python Script';
  SCythonTemplateName = 'Cython Script';
  SCSSFileTemplateName = 'Cascading Style Sheet';
  SHTMLFileTemplateName = 'HTML Document';
  STextFileTemplateName = 'Text File';
  SXMLTemplateName = 'XML Document';
  SJSONTemplateName = 'JSON File';
  SYAMLTemplateName = 'YAML File';
  SJSTemplateName = 'JavaScript Code';
  SPHPTemplateName = 'PHP Code';
  SJupyterTemplateName = 'Jupyter Notebook';

  // Python Versions
  SRegisteredVersions = 'Registered Versions';
  SUnRegisteredVersions = 'Unregistered Versions';

  //  SSH File and Remote engine
  SRemoteFileOpen = 'Open Remote File';
  SRemoteFIleSave = 'Save to Remote File';
  SRemoteFileAdd = 'Add Remote File';
  SRemoteFileSelect = 'Select Remote File';
  SErrorEmptyPath = 'File Path cannot be empty';
  SErrorEmptySSH =  'The SSH server name cannot be empty';
  SScpError4 = 'SCP Error: Connecting to host failed';
  SScpError5 = 'SCP Error: Connection lost for some reason';
  SScpErrorTimeout = 'SCP Error: Timeout';
  SScpErrorOther = 'SCP Error:'+#13#10+'Output: %s+'+#13#10+'Error Output: %s+';
  SSHUnknownServer = 'Unknown SSH server "%s"';
  SSHUnknownServerQuery = 'Unknown SSH server with fingerprint "%s". Do you want to proceed?';
  SSHPythonTimeout = 'Execution of Python ("%s") on the SSH server timed out';
  SSHPythonError = 'Failed to execute Python ("%s") on the SSH server:'+#13#10+
     'ReturnCode: %d'+#13#10+
     'Output: %s'+#13#10+
     'ErrorOutput: %s';
  SSHVersionMismatch = 'Local Python version is %s and remote Python version is %s. '+
    'To use this SSH server please switch to Python version %1:s.';

  // Missing action categories
  SActionCategoryFile = 'File';
  SActionCategoryView = 'View';
  SActionCategoryRun = 'Run';
  SActionCategoryProject = 'Project';
  SActionCategoryDebugWindows = 'Debug Windows';
  SActionCategoryIDENavigation = 'IDE Navigation';
  SActionCategoryRefactoring = 'Refactoring';
  SActionCategoryInterpreter = 'Interpreter';


  // do not localize further

{gnugettext: reset }
  SDebuggerHintFormat  = '<b>Name: </b> <font color="$FF8844">%s</font><br>'+
                         '<b>Type: </b> <font color="$FF8844">%s</font><br>'+
                         '<b>Value:</b><br>%s<br>';
  SNamespaceFormat = 'Frame(Function: "%s" of module: "%s")';
  SFilePosInfoCodeHint = '<br>Defined in module <a href="%s (%d:%d)"><font color="$FF8844"><u>%s (%d)</u></font></a>';
  SDefinedInModuleCodeHint = '<br>Defined in module <font color="$FF8844">%s</font>';
  SParsedClassCodeHint =  '<b>class <font color="$FF8844">%s</font></b>%s';
  SInheritsFromCodeHint = '<br>Inherits from: <font color="$FF8844">%s</font>';
  SParsedFunctionCodeHint = '<b>function <font color="$FF8844">%s</font>(%s)</b>%s';
  SParsedMethodCodeHint =  '<b>Method <font color="$FF8844">%s.%s(%s)</font></b>%s';
  SFunctionParameterCodeHint = '<b>Function Parameter <font color="$FF8844">%s</font>'+
                          '</b> of function <font color="$FF8844">%s</font>%s';
  SVariableCodeHint = '<b>Variable <font color="$FF8844">%s</font>%s%s';
  SLocalVariableCodeHint = '<b>Local variable <font color="$FF8844">%s</font>'+
                          '</b> of function <font color="$FF8844">%s</font>%s';
  SGlobalVariableCodeHint = '<b>Global variable <font color="$FF8844">%s</font>'+
                          '</b> of module <font color="$FF8844">%s</font>%s';
  SClassVariableCodeHint = '<b>Class variable <font color="$FF8844">%s</font>' +
                           '</b> of class <font color="$FF8844">%s</font>%s';
  SInstanceVariableCodeHint = '<b>Instance variable <font color="$FF8844">%s</font>' +
                           '</b> of class <font color="$FF8844">%s</font>%s';
  SImportedVariableCodeHint = '<b>Imported variable <b><font color="$FF8844">%s</font>' +
                           '</b> from module <font color="$FF8844">%s</font>%s';
  SVariableTypeCodeHint = '<br><b>Type:</b> <font color="$FF8844">%s</font>';
  SParsedModuleCodeHint = '<b>Module <a href="%s (1:1)"><font color="$FF8844"><u>%s</u></font></a></b>';
  SParsedPackageCodeHint = '<b>Package <a href="%s (1:1)"><font color="$FF8844"><u>%s</u></font></a></b>';
  SModuleProxyCodeHint = '<b>Module <font color="$FF8844">%s</font></b>';
  SPackageProxyCodeHint = '<b>Package <font color="$FF8844">%s</font></b>';
  SModuleImportCodeHint = '<b>Imported module <font color="$FF8844">%s</font></b>';

  SPythonFileTemplate =
    '#-------------------------------------------------------------------------------' + sLineBreak +
    '# Name:        $[ActiveDoc-Name]' + sLineBreak +
    '# Purpose:     ' + sLineBreak +
    '#' + sLineBreak +
    '# Author:      $[UserName]' + sLineBreak +
    '#' + sLineBreak +
    '# Created:     $[DateTime-''DD/MM/YYYY''-DateFormat]' + sLineBreak +
    '# Copyright:   (c) $[UserName] $[DateTime-''YYYY''-DateFormat]' + sLineBreak +
    '# Licence:     <your licence>' + sLineBreak +
    '#-------------------------------------------------------------------------------' + sLineBreak +
    '' + sLineBreak +
    'def main():' + sLineBreak +
    '    pass' + sLineBreak +
    '' + sLineBreak +
    'if __name__ == ''__main__'':' + sLineBreak +
    '    main()';
  SXMLFileTemplate =
    '<?xml version="1.0" encoding="UTF-8"?>' + sLineBreak;
  SJSONFileTemplate ='{'+ SLineBreak + SLineBreak + '}';
  SYAMLFileTemplate = '---'+ SLineBreak + SLineBreak + '...';
  SCSSFileTemplate = // do not localize
    'BODY {' + sLineBreak +
    '' + sLineBreak +
    '}';
  SHTMLFileTemplate = // do not localize
    '<!-- Created: $[DateTime-''DD/MM/YYYY''-DateFormat] by $[UserName] -->' + sLineBreak +
    '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional/EN">' + sLineBreak +
    '<html>' + sLineBreak +
    '  <head>' + sLineBreak +
    '    <title>Untitled</title>' + sLineBreak +
    '    <meta http-equiv="content-type" content="text/html; charset=ISO-8859-1">' + sLineBreak +
    '    <meta name="generator" content="PyScripter">' + sLineBreak +
    '  </head>' + sLineBreak +
    '  <body>' + sLineBreak +
    '' + sLineBreak +
    '  </body>' + sLineBreak +
    '</html>';

  SJupyterFileTemplate = // do not localize
    '{' + SLineBreak +
    ' "cells": [' + SLineBreak +
    '  {' + SLineBreak +
    '   "cell_type": "code",' + SLineBreak +
    '   "execution_count": null,' + SLineBreak +
    '   "metadata": {},' + SLineBreak +
    '   "outputs": [],' + SLineBreak +
    '   "source": []' + SLineBreak +
    '  }' + SLineBreak +
    ' ],' + SLineBreak +
    ' "metadata": {' + SLineBreak +
    '  "language_info": {' + SLineBreak +
    '   "codemirror_mode": {' + SLineBreak +
    '    "name": "ipython"' + SLineBreak +
    '   },' + SLineBreak +
    '   "file_extension": ".py",' + SLineBreak +
    '   "mimetype": "text/x-python",' + SLineBreak +
    '   "name": "python",' + SLineBreak +
    '   "nbconvert_exporter": "python"' + SLineBreak +
    '  }' + SLineBreak +
    ' },' + SLineBreak +
    ' "nbformat": 4,' + SLineBreak +
    ' "nbformat_minor": 2' + SLineBreak+
    '}';

implementation

//uses JvGnugettext, SysUtils;
//initialization
//  // GNU Initialization is put here to make sure that
//  // all localized stings get translated
//  // Setup Languages
//  UseLanguage('');
//  UseLanguage(LowerCase(Copy(GetCurrentLanguage,1,2)));  // do not use the country part
end.








