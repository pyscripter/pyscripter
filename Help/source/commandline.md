:::{index} Command-Line Argumemts;
:::


# Command-Line Arguments

You can specify command line arguments for scripts you run or debug, via 
the *Command Line Parameters...* command of the [Run menu](runmenu).  This command invokes 
the following dialog in which you specify the command line 
parameters as well as enable or disable their use.

![graphic](images/commandline1.JPG){align=center width="29em" height="11.44em"}

The small button with the down arrow next to the edit field provides access to the most 
recently used command line arguments.

When command line parameters are enabled, the provided arguments are placed in the argv list 
of the Python sys module before running or debugging scripts. Note that the script name is 
automatically inserted as the first argument and should not be specified here. 
In entering the command line you can use [parameters and modifiers](parameters).


Note that:
- Shft+Ctrl+P provides Parameter completion
- Shft+Ctrl+M provides Modifier completion 
