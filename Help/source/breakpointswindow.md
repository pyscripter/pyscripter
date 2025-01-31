:::{index} Debug Windows; Breakpoints
:::


# The BreakPoints Window

This window shows the breakpoints in all open Python scripts and modules.

![graphic](images/breakpointswindow1.png){width="45.875em" height="12.56em"}

#### Breakpoint Properties

- *Ignore count*\
If you set the the Ignore Count to a positive integer value, then the breakpoint will be ignored a number of times equal to that value.

- *Condition*\
A python expression to serve as a condition for the breakpoint. The execution 
will stop at this breakpoint only if the evaluated expression evaluates to True.

See this[ blog post](https://pyscripter.blogspot.com/2025/01/breakpoint-conditions-and-ignore-counts.html) for further details.

#### The Context Menu

![graphic](images/breakpointscontextmenu.png){width="12.69em" height="7.94em"}

**Commands:**

*Breakpoint Properties...*\
Select to edit the breakpoint properties.

*Clear*\
Clear the currently selected breakpoint.

*Clear All Breakpoints*\
Clear all breakpoints.

*Copy to Clipboard*\
Copy the breakpoint information to the Clipboard.

#### The Breakpoints Properties Dialog Box 

![graphic](images/breakpointproperties.png){width="25.56em" height="9.94em"}

Use this dialog box to specify values for the breakpoint Ignore Count and Condition.

#### Breakpoint Tips

- Double-clicking on a specific breakpoint takes you to the given code position.
- You can enable/disable a breakpoint by checking/unchecking the check-box at that start of the corresponding row. 
- You also can quickly disable/enable a breakpoint by Ctrl + clicking on it in the editor gutter.
- In the editor, you can right-click on a breakpoint to get a breakpoint context menu similar to the one above.
- You can modify a breakpoint's  properties in one of the following ways:\
  1. In the Breakpoints Window select the property you want to change and press F2.  This will allow you to edit the property directly.  Press Enter when you are done.
  2. In the Breakpoints Window right-click on the breakpoint and from the context menu (see above) select "Breakpoint Properties...".  A dialog box like the following pops up and you can edit the values.
  3. In the editor, when you click on a breakpoint you get a context menu from which you can select "Breakpoint Properties...". 
