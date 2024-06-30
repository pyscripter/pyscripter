:::{index} Project Explorer; Run configurations
:::


# Run Configurations

Using the [Project  Explorer](projectexplorer), you can create multiple 
Run Configurations which offer some more advanced options 
than those available when you run/debug the active script. You can create 
run configurations by selecting "Add Run Configuration" from the context 
menu of the "Run Configurations" node of the Project Explorer. 
The following dialog box is then displayed.

![graphic](images/runconfigurations1.JPG){align=center width="25.10em" height="29.15em"}

Here follows an explanation of the main fields:

*Description (optional):*\
A short description of the purpose of this run configuration. It is currently only used as 
a hint, when hover the mouse on top of the run configuration in the project 
explorer.

*File Name*\
The name of the script you would like to run. You can select a local or remote 
 file using the buttons next to the edit box.

*Parameters*\
Command line parameters that are placed in the argv list of the Python sys module before 
running or debugging scripts. Note that the script name is automatically 
inserted as the first argument and should not be specified here.

*Working Directory*\
If specified the current directory will be changed to this one before running the script 
 and restored back to the original one at the end of the run.

*Python Engine*\
The engine with which you would like to run or debug the script.

*Reinitialze Before Run*\
If checked, the Python engine will be reinitialized before running the script. This 
is necessary with some GUI scripts. This option is not available with 
the internal Python engine.

*Save Output*\
If checked the output of the script will be saved to the specified file.

*Output File Name*\
The path of the output file.

*Append to File*\
If checked output will be appended to that of earlier runs. Otherwise the most recent 
output will overwrite earlier output.

*Set External Run properties*\
Press this  button to specify the options for running the script with an external Python 
interpreter. The various settings are the same as in the [External Tools](externaltools) 
configuration. By default output is captured and shown at the [Output window](outputwindow).

In entering  the File Name, Parameters, Working Directory and Output file name you can use
[parameters and modifiers](parameters).

**Executing Run Configurations**

After defining a Run Configuration you can execute it in three different ways, by selecting 
the appropriate command from its context menu in the Project Explorer:
-  Run
- Debug
- External Run

After the  first execution you can use the commands
- Run last script (Shift+Ctrl+F9)
- Debug last script (Shift+F9)
- Run  last script externally(Shift+Alt+F9)
to run/debug  the last run configuration again.

These commands are available from the toolbar of the Project Explorer.
