:::{index} Debugging; Post Mortem Analysis
:::

# Post Mortem Analysis

If your program raises an exception and stops, you can use the Post-Mortem command from the 
[Run Menu](runmenu) to analyse the reason of failure. In this mode 
you can use the debugger windows (Call Stack, Variables, Watches) to examine the state of 
execution when the exception occurred and you can also issue interpreter commands in the 
context of the selected frame of the Traceback. 

To exit the Post-Mortem analysis you can use the "Abort Debugging" command.  
The Post-Mort command is available only when the last script that you ran or debugged exited 
with an exception and until the next time you run or debug a script.  

You may also set the [IDE option](ideoptions) "Post mortem on exception" to automatically 
enter the Post-Mortem when a script exits with an exception.
