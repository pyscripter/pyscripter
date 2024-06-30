:::{index} External Tools
:::

# External Tools

PyScripter offers the ability to define External Tools that can be run independently or 
interact with the IDE editor. There is great flexibility in specifying 
such tools allowing you to integrate your favorite Python utilities or command-line 
programs tightly with PyScripter.

You can define or modify external tools through the "Tools|Configure Tools.." menu 
option. The definition of the tools is persisted to the PyScritper.ini file.

![graphic](images/externaltools1.JPG){align=center width="17.25em" height="18.69em"}


In the above  dialog box you can modify existing external tools or create new ones using the 
the External Tool properties dialog shown below:  
  
![graphic](images/externaltools2.JPG){align=center width="28.125em" height="35.5em"}


A large number of options are available in the External Tool properties dialog which are explained 
below:

**Tool Configuration  Options**

| Option | Meaning |
| --- | --- |
| Caption | The caption of the Menu Item corresponding to this tool |
| Description | The hint of the Menu Item corresponding to this tool |
| Application | The full name of file to execute |
| Parameters | Command-line parameters |
| Working Directory | The working directory |
| Shortcut | Menu shortcut for the external tool |
| Context | Specifies when can the tool be executed      Always   Enabled,     Active   File,     Active   Python File    Selection   Available |
| Save Files | Save files option. Possible values    None,      Active   File,     All   Files |
| Standard Input | Feed standard input to the running process. Possible values:      None:   No standard input    Word   at cursor    Current   line    Selection    Active   file |
| Standard output | Send standard output to    None:   Do nothing    Replace   word at cursor    Replace   current line    Replace   selection    Replace   active file     Place   in new file |
| Parse Messages | Parse File/Line/LinePos info from output and put it in the Messages   Window. Useful for integrating command-line tools. |
| Message Format | Regular expression for parsing messages. You should use the   predefined grep expression macros \$[FileName], \$[LineNumber], \$[ColumnNumber] for specifying the grep expression. |
| Parse Traceback | Parse TraceBack and Syntax Errors from Python output and put it   in the Messages Window. |
| Capture Output | Capture command line output and place it in the Output Window |
| Hide Console | Hide Console or External Tool window |
| UTF8 IO | Use the UTF8 encoding for encoding standard input and decoding   standard output. If false the default console encodings   are used. |
| Environment | The Environment tab in tool properties allows you to run the external   tool with customized environment variables. |

Custom [parameters](parameters) (Shift+Ctrl+P) and modifiers (Shft+Ctrl+M) are available when specifying the 
Application, the Parameters and the Working directory.

A few external tools that demonstrate the possibilities opened by this feature of PyScripter 
 are offered by default:

- Python Interpreter (Runs a separate python shell)
- PythonWin help (Shows the PythonWin help file)
- Command Prompt (starts a console)
- Format Selection (formats the selected code using [black](https://github.com/psf/black))
- Sort  Selection (a python one-liner that demonstrates the use of Standard Input and Standard Output options)
- Profiler  (profiles the active python script using the standard Python profile module)
- PyLint - python source code checking tool ([http://www.logilab.org/projects/pylint](http://www.logilab.org/projects/pylint))
- Advanced Search and Replace (using re.sub)

You can delete or modify these tools as well as create new ones.
