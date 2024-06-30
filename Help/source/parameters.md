:::{index} Parameters
:::

# Parameters

Custom Parameters are implemented using ideas and code from the [Syn Editor](http:\\syn.sf.net) 
project. It is a very powerful feature allowing the developement of custom command line 
tools and facilitating autocompletion.

:::{contents}
:local:
:::

### Parameter syntax

Parameter is any identifier, that is enclosed by the parameter delimiters ("$["  and "]") e.g.
```$[ProgramFiles]```.


### Predefined parameters

Predefined system parameters (variables, that can be replaced in the commandline, scripts, 
templates and in inserted text) include:

| Parameter | Value |
| --- | --- |
| **Python Paths** | |
| PythonDir |  Installation directory of active Python version |
| Python38Dir | Installation directory of Python version 3.8 |
| Python39Dir | Installation directory of Python version 3.9 |
| Python310Dir | Installation directory of Python version 3.10 |
| Python311Dir | Installation directory of Python version 3.11 |
| Python312Dir | Installation directory of Python version 3.12 |
| Python313Dir | Installation directory of Python version 3.13 |
| PythonExe | Executable of active Python version |
| Python38Exe | Executable of Python version 3.8 |
| Python39Exe | Executable of Python version 3.9 |
| Python310Exe | Executable of Python version 3.10 |
| Python311Exe | Executable of Python version 3.11 |
| Python312Exe | Executable of Python version 3.12 |
| Python313Exe | Executable of Python version 3.13 |
| PythonVersion |  Version of active Python |
| **System Folders** | |
| ProgramFiles | Program Files folder |
| CommonFiles | Common Files folder |
| Windows | Windows folder |
| WindowsSystem | Windows System folder |
| WindowsTemp | Windows Temp folder |
| MyDocuments | My Documents folder |
| Desktop | Desktop folder |
| **System Information** | |
| CurrentDir | Current Directory |
| DateTime | Current date and time in the default short date-time format |
| UserName | User name of currently logged-in Windows user |
| Paste | Contents of the clipboard |
| CmdLineArgs | Command line arguments |
| **PyScripter specific parameters** | |
| ActiveDoc | File name of the Active Document |
| ActiveScript | File name of the Script you are about to run/debug |
| Project | File name of the Active Project |
| OpenFiles | File names of all open files, separated with space |
| ModFiles | File names of modified files, separated with space |
| Exe | PyScripter executable file name |
| **Other useful parameters** | |
| SelectFile | Opens a FileOpen dialog for file selection |
| SelectedFile | The last selected file |
| SelectDir | Opens "Browse for folder" dialog for folder selection |
| SelectedDir | The last selected folder name |


:::{index} Parameters; Modifiers
:::
### Parameter modifiers

You can modify/transform the value of a parameter by using the so-called "parameter modifiers".
Modifiers are small words, that are placed after a parameter, separated by '-'.


### Defined paramerer modifiers

| Modifier | Effect on the parameter value |
| --- | --- |
| CurLine | Returns the current line of the given file, if is open in editor |
| CurLineNumber | Returns the current line number of the given file, if is open in editor |
| CurWord | Returns the current word of the given file, if is open in editor |
| Date | Returns the date portion from a date-time parameter |
| DateAccess | Returns the file last access date |
| DateCreate | Returns the file creation date |
| DateFormat | Rormats date with given format (the prior modifier must contain the desired format)| DateWrite | Returns the file last write date |
| Dir | Extracts file directory from filename (without '\' at the end) |
| Drive | Extracts only the file drive |
| EdText | Returns the file text (but actual text from editor, if the file is modified) |
| Env | Returns the value of an environment variable (the parameter must contain its name) |
| Ext |  Extracts file extension |
| ExtOnly  | Extracts file extension without the starting dot |
| FileDate | Returns the file date |
| Full | Expands a relative file path to an absolute one |
| Long | Returns a long file name |
| LowerCase | Converts the parameter value to lowercase |
| Name | Extracts only the file name |
| NoExt | Removes the file extension from a file name |
| NoSep | Removes '\' from the end, if there is one |
| Param | Returns the n-th command line parameter e.g. ```$['1'-Param]``` |
| Path | Extracts file path from filename (with '\' at the end) |
| Quote | Adds quotes to the parameter value |
| Reg | Returns the registry key value for a given registry key name |
| SelText | Returns the selected text of the given file, if is open in editor |
| Sep | Adds '\' at the end, if there is none |
| Short | Returns a short file name (no spaces, old DOS format)|
| Text | Returns the file contents |
| Time | Returns the time portion from a date-time parameter |
| Type | Returns the type of a file, as shown in the explorer |
| UnQuote | Removes the quotes from the parameter value |
| UpperCase | Converts the parameter value to uppercase |


### Extended parameter syntax

Parameter syntax is extended so you can type

```$[Parameter=DefaultValue]```

which means, that if this Parameter is not found, Default Value will be used.

  
```$[Parameter?Question]``` will open an Input Box and ask with 'Question' for a parameter value.

 ```$[Parameter=DefaultValue?Question]```

will open an Input Box and ask with 'Question' for a value of Parameter, but if there is 
no value, will offer DefaultValue as default.

When the Parameter contains the equal sign, then its value is defined for the current 
session, e.g. 
 
```$[proc=Test]```

will define the Parameter "proc" with the value  "Test". Or 
 
```$[proc=?Enter proc]```

will prompt for a value and define the Parameter "proc" with the provided value. To remove a 
parameter use the equal sign without any value or question, e.g. 
 
```$[proc=]``` 

will remove the parameter proc if it exists.

```$['Some value']```

is returned as it is (only parameters in Some Value are replaced with their values). This 
is useful, if you want to pass specific value to modifier, for example:
 
```$['31.01.2002'-'YYYY/MM/DD'-DateFormat]```
 
will return 2002.01.31

The parameter value can be a conditional parameter. The format is:

```$[(ParameterCondition)TrueValue:FalseValue]```

where TrueValue and FalseValue are any valid parameter values.
The symbol ':' is not required - if  it is missing, its assumed that this
means empty FalseValue. The ParameterCondition  can contain one or two paarameter values 
and one of operations "=", "<>", "<", ">", "<=", ">=", "IS NULL" or "IS NOT NULL" or 
text to be asked in dialog (in single quotes) and "?" (question mark symbol) 
after it. In this case the  value will depend from the user input.

Examples:

```
Condition1=$[($[Project] IS NULL)'There is no project open':'Project file is $[Project]']
Condition2=$[('Answer Yes or No'?)'Your answer was Yes':'Your answer was No']
```


### Editor Shortcuts

You can use  parameters in the IDE editor, the [external tool](externaltools) 
configuration dialog and the [Code Template](codetemplates) 
definition. To facilitate the entry of parameters and modifiers PyScripter 
provides parameter and modifier completion (selection from a pop-up list) using 
the following shortcuts.

Shft+Ctrl+P: provides Parameter completion\
Shft+Ctrl+M: provides Modifier completion\
Shft+Ctrl+R: replaces all parameters with their values


### Examples

To see parameters and modifiers in action, copy the following into an editor 
and select Edit, Parameters, Replace parameters from the "Edit menu" or 
press Shft+Ctrl+R.


**System parameters:**

```
CurrentDir=$[CurrentDir]
ProgramFiles=$[ProgramFiles]
CommonFiles=$[CommonFiles]
Windows=$[Windows]
WindowsSystem=$[WindowsSystem]
WindowsTemp=$[WindowsTemp]
MyDocuments=$[MyDocuments]
Desktop=$[Desktop]
Exe=$[Exe]
ActiveDoc=$[ActiveDoc]
ActiveScript=$[ActiveScript]
Project=$[Project]
ModFiles=$[ModFiles]
DateTime=$[DateTime]
UserName=$[UserName]
SelectFile=$[SelectFile]
SelectedFile=$[SelectedFile]
SelectDir=$[SelectDir]
SelectedDir=$[SelectedDir]
Paste=$[Paste]
OpenFiles=$[OpenFiles]
```

**Parameter Modifiers:**

```
ActiveDoc-CurLine=$[ActiveDoc-CurLine]
ActiveDoc-CurWord=$[ActiveDoc-CurWord]
ActiveDoc-SelText=$[ActiveDoc-SelText]
'PATH'-Env=$['PATH'-Env]
PYTHON24DIR=$['HKLM\SOFTWARE\Python\PythonCore\2.4\InstallPath\'-Reg]
'0'-Param=$['0'-Param]
Exe-DateAccess=$[Exe-DateAccess]
Exe-DateCreate=$[Exe-DateCreate]
Exe-DateWrite=$[Exe-DateWrite]
Exe-FileDate=$[Exe-FileDate]
Exe-FileDate-'DD/MM/YYYY HH:NN:SS'-DateFormat=$[Exe-FileDate-'DD/MM/YYYY HH:NN:SS'-DateFormat]
Exe-FileDate-Date=$[Exe-FileDate-Date]
Exe-FileDate-Time=$[Exe-FileDate-Time]
Exe-Dir=$[Exe-Dir]
Exe-Dir-Sep=$[Exe-Dir-Sep]
Exe-Path=$[Exe-Path]
Exe-Path-NoSep=$[Exe-Path-NoSep]
Exe-Drive=$[Exe-Drive]
Exe-Ext=$[Exe-Ext]
Exe-Full=$[Exe-Full]
Exe-Long=$[Exe-Long]
Exe-LowerCase=$[Exe-LowerCase]
Exe-Name=$[Exe-Name]
Exe-NoExt=$[Exe-NoExt]
Exe-Quote=$[Exe-Quote]
Exe-Short=$[Exe-Short]
Exe-Type=$[Exe-Type]
Exe-UpperCase=$[Exe-UpperCase]
Day=$[DateTime-'DD'-DateFormat]
Month=$[DateTime-'MM'-DateFormat]
Year=$[DateTime-'YYYY'-DateFormat]
Hour=$[DateTime-'HH'-DateFormat]
Minutes=$[DateTime-'NN'-DateFormat]
Seconds=$[DateTime-'SS'-DateFormat]
```

:::{index} Parameters; Custom parameters
:::

### Custom parameters

You can define your own custom parameters by selecting the "Custom Parameters..." 
menu command under Tools|Options. The Custom Parameters dialog is displayed 
below:

![graphic](images/parameters1.JPG){align=center width="26.31em" height="23.44em"}

Custom parameter values may contain other parameters and modifiers. They are calculated when 
the value is required, not when the parameter is loaded from file, so 
they always points to the actual other parameter value. For example
 
```QuotedSelText=$[ActiveDoc-SelText-Quote]```

will return the quoted selected text of the active document.

