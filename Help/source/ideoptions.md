:::{index} Customization; IDE Options
:::

# IDE Options

This dialog provides a number of options that affect the operation of
PyScripter. These options are explained below.
   
![graphic](images/ideoptions1.JPG){align=center width="27.38em" height="23.94em"}

**Code Completion**

*Auto-Completion font*\
Allows you to customize the size and type of font used in auto-completion.

*Case Sensitive*\
This option determines whether the filtering of the code completion list
when you type characters is case sensitive (default True). 

*Code completion list size*\
The size of the code completion window in number or lines.

*Complete as you type*\
Code completion is invoked automatically as you type.

*Complete Python keywords*\
Python keywords appear in the completion list when appropriate.

*Complete with word-break characters*\
When the completion list is displayed completion with the currently
selected entry occurs when word-break characters are typed (e.g.
space, brackets etc.). This is in addition to completing
with the Tab and Enter keys.

*Auto-complete with one entry*\
If true, when the completion list contains one entry complete
automatically without showing the list.

*Editor code completion*\
Enable/Disable code completion in the editor. 

*Interpreter code completion*\
Enable/Disable code completion in the interpreter. 


**Code Explorer**

*Initially expanded*\
If checked the Code Explorer stats with its nodes initially expanded.


**Editor Options**

*Auto-complete brackets*\
If checked, when you edit Python scripts and you type an open bracket
("(", "[", "{") the corresponding closing bracket is entered
automatically. When editing HTML and XML files the opening
bracket is "<". It also auto-completes strings.

*Auto-reload changed files*\
If checked, files changed on disk will be reloaded without prompting if
the files have not been changed inside PyScirpter. A message is
shown in the status bar and a beep sound can be heard when this happens.

*Create backup files*\
If checked PyScripter will create backup files before overwriting
existing files.

*Default line breaks for new files*\
Controls the line break format for new files. Options: sffDos, sffUnix,
sffMac, sffUnicode. The last option although available is not
currently supported.

*Display packages names*\
Display package names instead of file names for package files (\_\_init\_\_.py) in
editor tabs.

*Default file encoding for new files*\
Controls the encoding for new files. Options: sf\_Ansi, sf\_UTF8,
sf\_UTF8\_NoBOM. See the topic on [Encoded Source Files](encodedsourcefiles) 
for further information.

*Detect UTF-8 encoding when opening files*\
This option controls whether PyScripter attempts to detect utf-8 encoding
when opening files without the BOM mark. This detection is done
by analyzing the first 4000 characters of the file and is
imperfect. It only applies to non-Python files since utf-8
encoded Python files are required to have either the BOM mark or an
encoding comment.
    
*Highlight selected word*\
If checked, when you select a word by double-clicking or by issuing the
editor command "Select Word", all occurrences of the selected word in
the editor are highlighted.

*Indentation guides*\
Indentation guides help you visualize the indentation level of different blocks of
code. These options determine, whether they are visible and their
style and color.

*Search text at caret*\
If checked, when the search function is invoked the search expression is
set to the word containing the editor caret.

*Selection color*\
[Options](https://pyscripter.blogspot.com/2022/10/new-feature-preview-selection-color.html)
controlling how selected text is painted. You can change the
foreground and background color. Alpha is a number between 0 and 1
that specifies the alpha-blending parameter. 1 corresponds to no alpha
blending. If "FillWholeLines" is not checked only the selected text is
highlighted. Otherwise the fully sellected lines are highlighted from
the left to the right margin.

*Show code hints*\
If unchecked code hints are not shown.

*Show debugger hints*\
If unchecked debugger hints are not shown.

*Show executable line marks*\
If checked the editor will show in the gutter special marks for executable lines.

*Track changes bar*\
The related options control the visibility and appearance (width and colors) of the 
 [track changes bar](https://pyscripter.blogspot.com/2022/10/new-feature-track-changes-bar.html ).

*Trim trailing spaces when saving files*\
If checked the editor will trim spaces at the end of the lines when
saving files. This works independently of the corresponding
editor option which trims trailing spaces while editing.
    

*Undo after save*\
If checked, you can undo editing actions beyond the point at which you
saved the file. Otherwise undo can only take you back to point at
which you last saved the file.


**File Explorer**

*File Explorer background processing*\
If checked (default) the File Explorer processes folder expand makrs and
file impages in threads. Uncheck if you encounter errors related to
File Explorer.

*File Change Notification*\
Controls whether the user is notified about changes in the edited files
(possible values: fcnFull, fcnNoMappedDrives, fcnDisabled).
Default: fcnNoMappedDrives.


**File Filters**

*File Explorer Filter*\
The file extensions separated by semi-colon that the File Explorer
recognizes as Python files. Used when the filter function is applied,
e.g. \*.py;\*.pyw

*Open Dialog Python Filter*\
The Python file filter for the File Open dialog. Multiple filters
should be separated by vertical bars ("|"), e.g. Python Files
(\*.py;\*.pyw)|\*.py;\*.pyw 

*Open Dialog CSS Filter*\
The CSS file filter for the File Open dialog. 

*Open Dialog CPP Filter*\
The C/C++ file filter for the File Open dialog. 

*Open Dialog HTML Filter*\
The HTML file filter for the File Open dialog. 

*Open Dialog JavaScript Filter*\
The JavaScript file filter for the File Open dialog. 

*Open Dialog PHP Filter*\
The PHP file filter for the File Open dialog. 

*Open Dialog XML Filter*\
The XML file filter for the File Open dialog. 

*Open Dialog YAML Filter*\
The YAML file filter for the File Open dialog. 


**Language Server**

*Check syntax as you type*\
If checked, when you edit Python files Pyscripter continuously check the
active file for syntax errors, which are shown in a similar way in
which word processors show spelling errors. If you place the
cursor on an error indicator you will see a hint explaining the
problem. If this option is not checked, the file is checked only when
you open or save it. 

*Language server debug output*\
If set, the language server produces debug output in the file
 %APPDATA%\PyScripter\LSPDebug.log for diagnostic purposes.

*Special packages*\
The code and parameter completion should be one of the best you can find
in any Python IDE. However,if you find that code and parameter
completion is not very accurate for certain modules and packages such
as wxPython and scipy you can achieve near perfect completion if you
add these packages to this option (comma separated list). Special
packages are imported on demand by the language server, instead of
scanning their source code.


**IDE**

*Automatic restart*\
If checked, PyScripter will restart automatically after system restart or
crash.

*Check for updates automatically*\
If checked, you will be notified when new version of the PyScripter
become available for download.

*Days between update checks*\
You can define how often you want to check for PyScripter updates.

*Dock animation interval*\
This and the following option, control the animation when you show a hidden
docked form. The Dock animation interval controls the interval in
milliseconds, between successive redraws of the shown window. It
needs to be greater than or equal to 1.

*Dock animation move width*\
The Dock animation move width controls the width in pixels, by which the
animated form is moved. To disable the animation set this
parameter to a large value, e.g. 1000.

*Editor tab position*\
Controls whether the editor tabs should appear at the top or at the bottom
(possible values: toBottom, toTop).

*File template for new Python scripts*\
The "New Python Module" command checks the value of this option and, if it
is not empty and is the name of an existing   [File Template](filetemplates), 
it uses the template for the new module. By default points to the provided
File Template for Python scripts.

*Logging Enabled*\
Enable logging of internal messages for diagnostic purposes.

*Restore open files*\
If checked, PyScripter will restore the files which were open when the
last session ended, when it starts up.

*Restore open* project\
If checked, PyScripter will restore the project which was open when the
last session ended, when it starts up.

*Show tab close button*\
Shows/hides the close buttons on the editor tabs.

*Smart Next/Previous Page*\
(Shift+)Ctrl+Tab can be used to move to the (previous) next page. If this option
is checked these keys move through the pages according to the Z-order
of the pages (as the Alt+Tab does in Windows). Otherwise they
move according to the visual order of the pages.

*Style Main Window Border*\
If true the main application window border is styled to match the
selected style. If false it has the standard windows border.

*User Interface content font size in points*\
This option controls the font size of the content of all IDE Windows (e.g.
File Explorer, Code Explorer, etc.). The default font size is 9 pt.  


**Project Explorer**

*Initially expanded*\
If checked the Project Explorer stats with its nodes initially expanded.


**Python Interpreter**

*Always use sockets*\
Pyscripter can use sockets or Windows named pipes for communicating with
the server. Named pipes can only be used if [pywin32](https://github.com/mhammond/pywin32)
is installed in the active python version. The main advantage of
named pipes is that it avoids firewall issues related to socket
connections. If this option is checked sockets are used in
preference to named pipes. (default True)

*Clear output before run*\
If checked the interpreter output is cleared before running scripts. (default False)

*Interpreter history size*\
Specifies the size of the interpreter command line history. (default 50)

*Jump to error on Exception*\
If set, when an exception occurs the file in which the exception occurred
opens and the error position is displayed. (default true)

*Mask FPU exceptions*\
Floating point operations that result in special numbers such as Nan, + or -
Infinity etc, normally raise exceptions. However some packages
such as Scipy would raise such exceptions when they get imported into
Python and they wouldn't be usable. Keep this option checked if
you want to use such packages with PyScripter.

*Post mortem on exception*\
If this option is checked when an unhandled exception occurs while
running or debugging a script, PyScripter enters the post-mortem
analysis mode.

*Pretty print output*\
If checked the standard python module pprint is used to format
interpreter output.

*Python Engine Type*\
Controls which engine type is used. Possible values (peInternal,
peRemote) See ([Remote Python Engines](pythonengines) for
details).

*Reinitialize before run*\
If set and a remote Python engine is used, it is re-initialized before
running or debugging a script. This is necessary when using GUI
applications (Tkinter, wxPython, etc.). It is set by default.

*Save environment before run*\
It saves environment options including open files. layout etc. before
running scripts so that you can recover if the IDE crashes.

*Save files before run*\
If checked all open files are saved before running scripts.

*Save interpreter history*\
If checked the interpreter history is saved at program exit and restored
when PyScripter is started again.

*Step into open files only*\
If checked the debugger will not step into files that are not currently
open. (defaults False).

*Timeout for running scripts*\
Time in ms for running scripts timeout. If different than zero, when
the elapsed time since the start of running a Python script exceeds
the timeout value, you are given the opportunity to interrupt the
script. Due to Python limitations this does not work
when the script loops indefinitely inside a function.

**Shell Integration**
    
*File Explorer Context Menu*\
If checked, it adds a menu item "Edit with Pyscripter" to the Windows
File Explorer context menu for Python files. If unchecked this
option removes such context menu if it is present.


**Spell Checking**

*Dictionary language code*\
The [BCP47](https://www.techonthenet.com/js/language_tags.php)
language tag of the language used by the speller. It is initialized
using the Windows user locale.

*Spell check as you type*\
If checked, the text you type is immediately checked.

*Spell checked syntax tokens*\
This is a comma-separated list of the code syntax elements that are spell
checked. By defalut, comments and all string types are syntax checked.


**SSH**

*Disable Variables Window with SSH*\
The updating of the [Variables window](variableswindow)
requires data transers between PyScripter and remote Python
Engines. With slow SSH connections this could slow down
PyScipter. If checked, the Variables Window is desabled while
using an SSH engine.

*Scp command*\
This option allows you to specify a path to an scp command. 
This could be useful if you have multiple
versions of scp installed and want to specify which one to use. The
defalut value 'scp' assumes scp can be found on the system path. You
can overwrite this value in the SSH server configuration.

*Scp options*\
This option allows you to specify options to used with the scp command. 
You can overwrite this value in the SSH server configuration.

*SSH command*\
This option allows you to specify a path to an ssh command. 
This could be useful if you have multiple
versions of ssh installed and want to specify which one to use. The
defalut value 'ssh' assumes ssh can be found on the system path. You
can overwrite this value in the SSH server configuration.

*SSH options*\
This option allows you to specify options to used with the ssh command. 
You can overwrite this value in the SSH server configuration.
