:::{index} Main Menu; Run
:::

# The Run Menu

![graphic](images/runmenu1.png){width="15.938em" height="33.38em"}

***Commands/Actions:***

*Syntax Check*\
Checks the syntax of the active Python script.

*Import Module*\
Imports the active Python script into the [Python Interactive Interpreter](interpreter).

*Run*\
Runs the active Python script without debugging using the embedded Python interpreter.

*Command Line Parameters...*\
Provide [command line parameters](commandline) for a script running in the embedded Python interpreter
or being debugged.

*External Run*\
Runs the active Python script in an external Python interpreter. Result will be displayed 
in the "Output" tab.

*Configure External Run...*\
Opens up an "External Run Properties" window using which we can configure the External Run command. 
Options are provided for selecting the Python interpreter, specifying command line parameters, 
capturing the standard output and reporting Traceback information. The options are the same as the 
options that we provide for the specification of "Python Interpreter" in the 
[External Tools](externaltools).

*Debug*\
Runs the active Python script with debugging using the embedded Python interpreter.

*Run To Cursor*\
Inserts a temporary breakpoint at the cursor position. Runs the active Python script 
with debugging until cursor position using the embedded Python interpreter.

*Step into*\
Starts or resumes debugging by stepping into the next line of code.

*Step over*\
Resumes debugging by stepping over the next line of code.

*Step out*\
Resumes debugging by stepping out the current execution frame (subroutine).

*Pause*\
Stops the running program at the first available opportunity. Please note that pausing 
and aborting is only possibly if there are breakpoints in the running script.

*Abort Debugging*\
Aborts the debugging process.

*Post mortem*\
Enter post mortem analysis mode after an unhandled exception has occurred. In this 
mode, you can use the "Call Stack", "Variables" and "Watches" windows as well as evaluate 
expressions in the interpreter, to examine the causes of the exception. 
To exit this mode, use the "Abort Debugging" command.

*Toggle Breakpoint*\
Toggles the breakpoint at the cursor position.

*Clear all Breakpoints*\
Clear all breakpoints in all of the open files.

*Add Watch at Cursor*\
Add the expression at the current editor position as a watch expression.


### Python Versions submenu

![graphic](images/runmenu2.png){width="12.5em"  height="7.8em"}

From this submenu you can switch python versions by selecting from the shown list. 
It is also available from the toolbar of the main application window with the Python logo.

*Setup Python Versions...*\
Manage the python versions that PyScripter knows about. See the 
[Python versions](pythonversions) topic for details.


### Python Engine submenu

![graphic](images/runmenu3.png){width="16.81em"  height="11.06em"}

From this submenu you can select the active Python engine. See the 
[Python Engines](pythonengines) topic for details.

*Free-Threaded*\
Click to switch between the standard and the [free-threaded](https://docs.python.org/3/howto/free-threading-python.html) versions of Python. It is checked if the free-threaded python is used. It is disabled if the free-threaded Python is not available.

*Reinitialize Python engine*\
This option is only available with the remote Python engines. It restarts the active 
remote engine and it works even when a script is running.
