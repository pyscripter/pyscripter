unit StringResources;

interface

resourcestring
  SInternalError = 'Internal Error in %s';
  SNotFound = '"%s" not found';
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

implementation
end.
