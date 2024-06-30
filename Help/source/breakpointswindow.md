:::{index} Debug Windows; Breakpoints
:::


# The BreakPoints Window

This window shows the breakpoints in all open Python scripts and modules. Double-clicking 
on a specific breakpoint takes you to the given code position.

![graphic](images/breakpointswindow1.JPG){width="64em" height="8.5em"}

You can enable/disable a breakpoint by checking/unchecking the check-box at that start of the corresponding 
row. You can also apply a condition by specifying a Python expression using the context menu.

*The Context Menu*

![graphic](images/breakpointswindow2.JPG){width="9.81em" height="4.56em"}

**Commands:**

*Set Condition*\
Specify a python expression to serve as a condition for the breakpoint. The execution 
will stop at this breakpoint only if the evaluated expression returns True.

*Clear*\
Clear the currently selected breakpoint.

*Clear All Breakpoints*\
Clear all breakpoints.
