unit StringResources;

interface
{gnugettext: scan-all}
Const
  SInternalError = 'Internal Error in %s';
  SNotFound = '"%s" not found';
  SItemsReplaced = '%d items found and %d" items replaced';
  SNotAvailable = 'n/a';
  SNotImplented = 'Not implemented';
  SFailedToBackupFile = 'Failed to backup file "%s"';
  SFilterAllFiles = 'All files|*.*|';
  SNoParameters = '** No/Unknown parameters **';
  SInvalidRegularExpression = 'Invalid Regular Expression: %s';
  SEmptyList = '(Empty List)';
  SCommandLineMsg  = 'Command Line : %s' + sLineBreak;
  SEngineActive = '*** %s Python engine %s is active ***';
  SPythonInitError =
  'PyScripter cannot find the standard Python library modules.' + SLineBreak +
  'This sounds like a Python installation error.  Please check the Windows Registry key ' + SLineBreak +
  '    HKEY_LOCAL_MACHINE\SOFTWARE\Python\PythonCore\x.y  or ' + SLineBreak +
  '    HKEY_CURRENT_USER\SOFTWARE\Python\PythonCore\x.y' + SLineBreak +
  'for a single user installation of Python.  ("x.y" stands for the version of Python).' + SLineBreak +
  'In a proper installation the key should exist and the "InstallPath" and "PythonPath" variables defined.' + SLineBreak +
  'Also check the PYTHONHOME and PYTHONPATH environment variables to see whether they conflict with the Python version used.' ;
  SPythonLoadError =
  'PyScripter could not load a Python engine' + SLineBreak +
  '**Before** using PyScripter, you must ensure that a version of Python' + SLineBreak +
  'greater or equal to 2.5 is installed on your machine. If you do not have one' + SLineBreak +
  'installed, you can download one from http://www.python.org/.' + SLineBreak +  SLineBreak +
  'The 64-bit version of PyScripter (x64) works only on 64-bit Windows **and**' + SLineBreak +
  'with 64-bit versions of Python.  The 32-bit version of PyScripter works on both' + SLineBreak +
  '32-bit and 64-bit Windows with 32-bit versions of Python';
  SCouldNotShutDownRemoteEngine =
  'PyScripter is waiting for the remote engine to shut-down for a long time without success.' + SLineBreak +
  'This might mean that either the system is very busy or more likely something is preventing' + SLineBreak +
  'the remote engine from shutting down, such as waiting in a blocked state.' + SLineBreak +
  'You can use the task manager to kill the engine (appears as a process "python.exe")' + SLineBreak +
  'and then reinitialize the remote engine.  Alternatively, you can close down and restart PyScripter';
  SPostMortemInfo = 'You are now in post-mortem analysis mode.  You can examine the Call Stack, ' +
                    'Variables and Watches windows, evaluate expressions etc.' + SLineBreak +
                    'To exit the post-mortem analysis select the Abort Debugging command.';
  SFileEncodingWarning = 'Encoding file "%s" using "%s" encoding will ' +
                        'result in information loss.  Do you want to proceed?';
  SDecodingError = 'Error in decoding file "%s" from "%s" encoding';
  SFileOpenError = 'Error in opening file: "%s".' + sLineBreak + 'Error: %s';
  SFileSaveError = 'Error in saving file: "%s".' + sLineBreak +'Error: %s';
  SErrorGettingNamespace = 'Error in getting the namespace of %s';
  SCouldNotSetDirectory = 'Could not set the current directory to the script path';
  SCannotCompileWhileRunning = 'You cannot compile, import or run modules while debugging or running programs';
  SErrorInImportingModule = 'Error in importing module';
  SCouldNotSetCurrentDir = 'Could not set the current directory to the script path';
  SRemoteServerNotConnected = 'Remote Server is not connected.  Please reinitialize or disconnect the remote  interpreter.';
  SRpycNotAvailable = 'The Rpyc module is not available.  To use the remote Python Engine '+
    'download Rpyc from http://Rpyc.sf.net and install it.';
  SWrongRpycVersion = 'Wrong Rpyc version.  To use the remote Python Engine '+
        'download version 2.6 Rpyc from http://Rpyc.sf.net and install it.';
  SCouldNotWriteServerFile = 'Could not write file "%s" and cannot use a remote Python engine';
  SErrorCreatingRemoteEngine = 'Error in creating the remote interpreter: ';
  SCouldNotConnectRemoteEngine = 'Could not connect to the remote Python engine server. '+
        'The remote interpreter and debugger is not available.';
  SRemoteInterpreterInit = '*** Remote Interpreter Reinitialized  ***';
  SSameName = 'Another item has the same name';
  SCodeTemplateModified = 'The template has been modified.  Do you want to update it with the new definition?';
  SInvalidNumber = 'Invalid number!';
  SEmptyTokenTextError = 'You cannot insert a token that only consists of white space.';
  SAccessAppDataDir = 'Pyscripter needs access to the User Application Data directory: "%s". '+
                      'Certain features such as remote debugging  will not function properly otherwise.';
  SNotAValidNumber = '"%s" is not a valid integer';
  SGoToLineNumber = 'Go to line number';
  SEnterLineNumber = 'Enter line number:';
  SFileRenamedOrDeleted = '%s has been renamed or deleted.';
  SRegistryAccessDenied = 'Registry access denied. Could not change the file association.';
  SErrorWhileDownload = 'Error while downloading: ';
  SCurrentVersionUptodate = 'Current version is up-to-date!';
  SKillExternalTool = 'An External Tool is still running.  Do you want to terminate it and exit?';
  STerminateInterpreter = 'The Python interpreter is busy.  Are you sure you want to terminate it?';
  SFindDefinitionWarning = 'This is the definition of "%s"';
  SPlaceCursorOnName = 'Please place the cursor on a function/class name or identifier';
  SCannotChangeEngine = 'Cannot change the Python engine while it is active.';
  SCouldNotOpenOutputFile = 'Could not open/create output file %s';
  SUnknownPythonVersion = 'PyScripter can''t use command line parameter PYTHON%s because it doesn''t know this version of Python.';
  SUnsupportedPythonVersion = 'PyScripter can''t use command line parameter PYTHON%s because it was compiled for Python %s or later.';
  SAbortDebugging = 'A debugging session is in progress.  Do you want to abort the session and Exit?';
  SInterruptRunningScript = 'The Python Script has timed out.  Do you want to interrupt it?';
  SNoTestsFound = 'No tests found!';
  SDuplicateKey = 'Duplicate Key';
  SChangedFilesReloaded = 'Changed files have been reloaded';
  SFileChangeNotification = 'File Change Notification';
  SFileReloadWarning = 'The following files have been changed on disk.'+
      ' Select the files that you wish to reload and press the OK button. '+
      ' Please note that you will lose all changes to these files.';
  StrScriptRunOK = 'Script run OK';
  SDebuggingAborted = 'Debugging Aborted';
  SSyntaxError = 'Syntax Error';
  StrCertainty = '  Certainty %s%%';
  SReferencesNotFound = '  References not found';
  SReferencesOf = 'References of "';
  SDefinitionsOf = 'Definition(s) of "';
  SDefinitionFound = '  Definition found';
  SDefinitionNotFound = '  Definition not found';
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
  SThemeHint = 'Set theme to "%s".';
  SSaveCurrentLayout = 'Save current Layout';
  SLayoutName = 'Layout Name:';
  SEditBreakpointCond = 'Edit Breakpoint Condition';
  SEnterPythonExpression = 'Enter Python expression:';
  SDeleteLayouts = 'Delete Layouts';
  SSelectLayouts = 'Please select the layouts you want to delete and press the OK button:';
  SSaveModifiedFiles = 'Save modified files';
  SSelectModifiedFiles = 'The following files have been modified.'+
    ' Please select the files that you wish to save and press the OK button. '+
    ' Press Cancel to go back to PyScripter';
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
  SDocTab = 'Doc';
  SWebPreviewHint = 'Web Preview|Preview HTML in browser';
  SWebPreview = '&Web Preview';
  SWebPreviewTab = 'Browser';
  SSourceTabCaption = 'Source';
  SErrorInitScript = 'Error in running initialization script %s: "%s';

  //  Project Manager
  SAskSaveProject = 'The active project has not been saved.  Do you want to save the changes?';
  SFailedToBackupProject = 'Failed to backup project "%s"';
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
  SFileChangedAbort = '%s' + sLineBreak + 'has changed since it was searched.  Replacement aborted.'
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
  SPrintTimeOut              = 'Timeout: %d ms';
  SExternalToolStillRunning  = 'The External Tool "%s" is still running  Do you want to terminate it?';

  // Editor
  SInsert = 'Insert';
  SOverwrite = 'Overwrite';
  SReadOnly = 'Read Only';
  SNonameFileTitle = 'Untitled';
  SNonamePythonFileTitle = 'module';
  SAskSaveChanges = 'The text in the "%s" file has changed.'#13#10#13#10 + 'Do you want to save the modifications?';
  SFileReloadingWarning = 'Reloading the file will result in the loss of all changes.  Do you want to proceed?';
  SFileAlreadyOpen = 'Another editor with the same file is open.  You can not have two editors with the same file.';

  // Parameters
  SEnterParameterCaption = 'Parameter replacement';
  SEnterParameterText = 'Enter parameter value';
  SParamCircularReference = 'Parameter "%s" is referenced circularly';
  SParameterNotFound = 'Parameter with name "%s" is not found';
  SModifierNotFound = 'Modifier with name "%s" is not found';
  SObjectNotFound = 'Object with name "%s" is not found';
  SPropertyNotFound = 'Object "%s" does not have registered property with name "%s"';
  SInvalidObjectProperty  = 'Object "%s" does not have property with name "%s"';
  SInvalidParameterFormat = '"%s" is not valid parameter format';
  SInvalidConditionFormat = 'Invalid condition format';
  SDuplicateModifier = 'Duplicate Modifier "%s"';

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
  SSelfOutsideClassScope = '"self" used outside a class scope';
  SCouldNotFindIdentInScope = 'Could not find identifier "%s" in scope "%s"';
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
  SJSTemplateName = 'JavaScript Code';
  SPHPTemplateName = 'PHP Code';

  // do not localize further

{gnugettext: reset }
  SDebuggerHintFormat  = '<b>Name: </b> <font color="clHotlight">%s</font><br>'+
                         '<b>Type: </b> <font color="clHotlight">%s</font><br>'+
                         '<b>Value:</b><br>%s<br>';
  SNamespaceFormat = 'Frame(Function: "%s" of module: "%s" at line %d)';
  SFilePosInfoCodeHint = '<br>Defined in module <a href="%s (%d:%d)"><u>%s (%d)</u></a>';
  SDefinedInModuleCodeHint = '<br>Defined in module <font color="clHotlight">%s</font>';
  SParsedClassCodeHint =  '<b>class <font color="clHotlight">%s</font></b>%s';
  SInheritsFromCodeHint = '<br>Inherits from: <font color="clHotlight">%s</font>';
  SParsedFunctionCodeHint = '<b>function <font color="clHotlight">%s</font>(%s)</b>%s';
  SParsedMethodCodeHint =  '<b>Method <font color="clHotlight">%s.%s(%s)</font></b>%s';
  SFunctionParameterCodeHint = '<b>Function Parameter <font color="clHotlight">%s</font>'+
                          '</b> of function <font color="clHotlight">%s</font>%s';
  SLocalVariableCodeHint = '<b>Local variable <font color="clHotlight">%s</font>'+
                          '</b> of function <font color="clHotlight">%s</font>%s';
  SGlobalVariableCodeHint = '<b>Global variable <font color="clHotlight">%s</font>'+
                          '</b> of module <font color="clHotlight">%s</font>%s';
  SClassVariableCodeHint = '<b>Class variable <font color="clHotlight">%s</font>' +
                           '</b> of class <font color="clHotlight">%s</font>%s';
  SInstanceVariableCodeHint = '<b>Instance variable <font color="clHotlight">%s</font>' +
                           '</b> of class <font color="clHotlight">%s</font>%s';
  SImportedVariableCodeHint = '<b>Imported variable <b><font color="clHotlight">%s</font>' +
                           '</b> from module <font color="clHotlight">%s</font>%s';
  SVariableTypeCodeHint = '<br><b>Type:</b> <font color="clHotlight">%s</font>';
  SParsedModuleCodeHint = '<b>Module <a href="%s (1:1)"><u>%s</u></a></b>';
  SParsedPackageCodeHint = '<b>Package <a href="%s (1:1)"><u>%s</u></a></b>';
  SModuleProxyCodeHint = '<b>Module <font color="clHotlight">%s</font></b>';
  SPackageProxyCodeHint = '<b>Package <font color="clHotlight">%s</font></b>';
  SModuleImportCodeHint = '<b>Imported module <font color="clHotlight">%s</font></b>';

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


implementation

uses JvGnugettext, SysUtils;
initialization
  // GNU Initialization is put here to make sure that
  // all localized stings get translated
  // Setup Languages
  UseLanguage('');
  UseLanguage(LowerCase(Copy(GetCurrentLanguage,1,2)));  // do not use the country part
end.








