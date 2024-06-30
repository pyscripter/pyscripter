:::{index} Debugging
:::
:::{index} Running Scripts
:::

# Running Scripts

There are  many ways of running Python scripts from PyScripter:

1. **Debug using the integrated Python debugger**

    Set any breakpoints you need and then from the [Run menu](runmenu) 
    select the Debug command. All the debugging facilities (step-into, step-out 
    etc.) are available in this case. When the execution stops at a breakpoint 
    or while stepping through the code you can use the 
    [Call Stack  window](callstackwindow), the [Variables window](variableswindow) 
    and the [Watches window](watcheswindow) to better understand the behavior of your 
    code. All output is redirected to the [Interpreter  window](interpreter). 
    You can also use the Interpreter Window while debugging for running Python code 
    in the context of the Call Stack frame at which the execution stopped for example 
    if you want to change the value of a variable.

    You can also  start debugging by using the "Step-Into" and the "Run to Cursor" 
    commands of of the Run menu. In that case executions stops at the first 
    executable statement or the current line of the active module.

 2. **Run without debugging**

    Select the Run command from the [Run menu](runmenu).   All output is again redirected 
    to the [Interpreter Window](interpreter).

3. **Run or debug using one of the internal Python engine (deprecated)**
    The internal python engine is deprecated and hidden by default.  To
    use it you first need to enable it using the [IDE option](ideoptions)
    Python Interpreter, "Internal interpreter hidden".

    After enabling the Internal python engine, you can select it from the Python Engine 
    submenu of the [Run menu](runmenu).  Then run or debug as when using the remote Python engine. 
    
    See the [Python Engine](pythonengines) topic for details and limitations.

 4. **Run externally from PyScripter**

    Select the External Run command from the [Run menu](runmenu). 
    Extensive customization (choice of interpreter, command-line, environment variables 
    etc.) is available through the Configure External Run command. The various 
    settings are the same as in the [External  Tools](externaltools) 
    configuration. By default output is captured and shown at the 
    [Output window](outputwindow).

For the first three options you can set command line parameter using the 
[Command  Line](commandline) command. 

In addition, using the [Project Explorer](projectexplorer), you can create 
multiple [Run Configurations](runconfigurations) with some more advanced options.
