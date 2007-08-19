unit StringResources;

interface

resourcestring
  SInternalError = 'Internal Error in %s';
  SNotFound = '"%s" not found';
  SItemsReplaced = '"%d" items replaced';
  SNotAvailable = 'n/a';
  SNotImplented = 'Not implemented';
  SFilterAllFiles = 'All files|*.*|';
  SVariablesDocNotSelected = '<b>Namespace: </b> <color=clBlue>%s</color>';
  SVariablesDocSelected = '<b>Namespace: </b> <color=clBlue>%s</color><br>'+
                          '<b>Name: </b> <color=clBlue>%s</color><br>'+
                          '<b>Type: </b> %s<br><b>Value:</b><br>%s<br>'+
                          '<b>Docstring:</b><br>%s';
  SDebuggerHintFormat  = '<b>Name: </b> <font color="clBlue">%s</font><br>'+
                         '<b>Type: </b> <font color="clBlue">%s</font><br>'+
                         '<b>Value:</b><br>%s<br>';
  SNoParameters = '** No/Unknown parameters **';
  SNamespaceFormat = 'Frame(Function: "%s" of module: "%s" at line %d)';
  SFilePosInfoCodeHint = '<br>Defined in module <a href="%s (%d:%d)"><u>%s (%d)</u></a>';
  SDefinedInModuleCodeHint = '<br>Defined in module <font color="clBlue">%s</font>';
  SParsedClassCodeHint =  '<b>class <font color="clBlue">%s</font></b>%s';
  SInheritsFromCodeHint = '<br>Inherits from: <font color="clBlue">%s</font>';
  SParsedFunctionCodeHint = '<b>function <font color="clBlue">%s</font>(%s)</b>%s';
  SParsedMethodCodeHint =  '<b>Method <font color="clBlue">%s.%s(%s)</font></b>%s';
  SFunctionParameterCodeHint = '<b>Function Parameter <font color="clBlue">%s</font>'+
                          '</b> of function <font color="clBlue">%s</font>%s';
  SLocalVariableCodeHint = '<b>Local variable <font color="clBlue">%s</font>'+
                          '</b> of function <font color="clBlue">%s</font>%s';
  SGlobalVariableCodeHint = '<b>Global variable <font color="clBlue">%s</font>'+
                          '</b> of module <font color="clBlue">%s</font>%s';
  SClassVariableCodeHint = '<b>Class variable <font color="clBlue">%s</font>' +
                           '</b> of class <font color="clBlue">%s</font>%s';
  SInstanceVariableCodeHint = '<b>Instance variable <font color="clBlue">%s</font>' +
                           '</b> of class <font color="clBlue">%s</font>%s';
  SImportedVariableCodeHint = '<b>Imported variable <b><font color="clBlue">%s</font>' +
                           '</b> from module <font color="clBlue">%s</font>%s';
  SVariableTypeCodeHint = '<br><b>Type:</b> <font color="clBlue">%s</font>';
  SParsedModuleCodeHint = '<b>Module <a href="%s (1:1)"><u>%s</u></a></b>';
  SParsedPackageCodeHint = '<b>Package <a href="%s (1:1)"><u>%s</u></a></b>';
  SModuleProxyCodeHint = '<b>Module <font color="clBlue">%s</font></b>';
  SPackageProxyCodeHint = '<b>Package <font color="clBlue">%s</font></b>';
  SModuleImportCodeHint = '<b>Imported module <font color="clBlue">%s</font></b>';
  SRegError = 'Regular Expression Error:';
  SEmptyList = '(Empty List)';
  SNewFolder = 'New Folder';
  SCommandLineMsg  = 'Command Line : %s' + sLineBreak;
  SEngineActive = '*** %s Python engine%s is active ***';
  SPythonInitError =
  'PyScripter cannot find the standard Python library modules.' + SLineBreak +
  'This sounds like a Python installation error.  Please check the Windows Registry key ' + SLineBreak +
  '    HKEY_LOCAL_MACHINE\SOFTWARE\Python\PythonCore\2.x  or ' + SLineBreak +
  '    HKEY_CURRENT_USER\SOFTWARE\Python\PythonCore\2.x' + SLineBreak +
  'for a single user installation of Python.  ("x" stands for the minor version of Python).' + SLineBreak +
  'In a proper installation the key should exist and the "InstallPath" and "PythonPath" variables defined.' + SLineBreak +
  'Also check the PYTHONHOME and PYTHONPATH environment variables to see whether they conflict with the Python version used.' ;
  SPostMortemInfo = 'You are now in post-mortem analysis mode.  You can examine the Call Stack, ' +
                    'Variables and Watches windows, evaluate expressions etc.' + SLineBreak +
                    'To exit the post-mortem analysis select the Abort Debugging command.';
  SFileChangeNotificationProblem = 'File change notification is now disabled, ' +
                    'due to a problem in the monitoring process.';

implementation
end.
