:::{index} Main Menu; View
:::

# The View Menu

![graphic](images/viewmenu.png){width="10.06em"  height="19em"}

***Commands/Actions:***

*Status  Bar*\
Shows/hides the Status bar.

*Styles...*\
Allows you to change the visual appearance (style) of the application.

### The Navigate submenu

![graphic](images/viewmenu1.png){width="9.88em"  height="5.438em"}

Use the navigation commands to show and activate the editor and the various 
IDE and Debug Windows.

***Commands/Actions:***

*Editor*\
Move the input focus to the active editor.


##### The IDE Windows submenu

![graphic](images/viewmenu5.png){width="18.56em"  height="18.25em"}
  
*Interactive Interpreter*\
Activates the [Interactive Interpreter](interpreter) window.

*Code Explorer*\
Activates the [Code Explorer](codeexplorer) window.

*File Explorer*\
Activates the [File Explorer](fileexplorer)  window.
  
*Project Explorer*\
Activates the [Project Explorer](projectexplorer) window.

*Chat*\
Activates the [Chat Window](chatwindow) window.

*Output Window*\
Activates the [Output](outputwindow) window.
  
*Unit Tests*\
Activates the [Unit Tests](unittestwindow) window.
  
*To-Do List*\
Activates the [To-Do List](todolistwindow) window.

*Regular Expression Tester*\
Activates the [Regular Expression Tester](regularexpressiontesting) window.

*Find-in-Files Results*\
Activates the [Find-in-Files Results](findinfileswindow) window.


##### The Debug Windows submenu
  
![graphic](images/viewmenu6.png){width="13.25em"  height="8.125em"}

*Breakpoints*\
Activates the [Breakpoints](breakpointswindow) window.

*Call Stack*\
Activates the [Call Stack](callstackwindow) window.
  
*Messages*\
Activates the [Messages](messageswindow) window.

*Variables*\
Activates the [Variables](variableswindow) window.

*Watches*\
Activates the [Watches](watcheswindow) window.


### The Editor submenu

![graphic](images/viewmenu7.png){width="16.4375em"  height="11em"}

*Next/Previous Editor*\
Shows the next/previous editor.

*Zoom In*\
Increase the editor font size by 1.
  
*Zoom Out*\
Decrease the editor font size by 1.

*Reset Zoom*\
Reset the editor font size to its default value.


#### The Syntax submenu

The Syntax submenu allows you to select the syntax highlighting scheme for the active editor.

![graphic](images/viewmenu8.png){width="7.625em"  height="21.75em"}


### The Toolbars submenu
  
![graphic](images/viewmenu4.png){width="12.38em"  height="12em"}
  
This submenu allow you to show/hide the different PyScripter toolbars. The Customize 
command displays the [Toolbar  customization](toolbarcustomization) dialog box.
 

### The Languages submenu

Use the Languages submenu to change the language of the User Interface of PyScripter. See the
[Localization](localization) topic for information about creating new translations.

![graphic](images/viewmenu9.png){width="11.87em"  height="26em"}
  

### The Layouts submenu

The entries above the menu separator correspond to specific layouts that have been saved 
and you can restore them.

![graphic](images/viewmenu10.png){width="14.69em"  height="12.62em"}

*Save Layout...*\
Saves the current layout under a name the user provides

*Delete Layouts...*\
The user is prompted for the list of layouts to delete.

*Set Debug layout*\
Saves the current layout under the name 'Debug'. If a layout named 'Debug' is available 
when you start debugging, then this layout is loaded. When debugging terminates 
the layout active before starting debugging is restored.

*Maximize editor*\

Maximize the editor window by auto-hiding all other IDE windows.

*Restore editor*\
Restore the maximized editor window to each state before maximizing.


### Editor Views

PyScripter provides different views of Python modules that can be seen alongside the source 
code. Currently two such views are provided. The Documentation view 
and the Disassembly view. When views other than the source code are available 
you can switch between the source code and these views using the tabs at the 
top of the editing area. You can close additional views by right clicking on 
their tab and selecting "Close". When the only view is the source 
code the tabs at the top of the editing area are hidden.

![graphic](images/toolsmenu2.png){width="9.94em"  height="5.38em"}
  

***Commands/Actions:***

*Documentation*\
Shows the [Documentation view](documentationview).

*Disassembly*\
Shows the [Disassembly view](disassemblyview).

*Web Preview*\
Shows a Web preview of html files in a built-in browser.  It also works with python Jupyter notebooks  allowing the creation/editing/running of code cells.


### The Split Editor submenu

![graphic](images/viewmenu2.png){width="12.69em"  height="5.5em"}
  
*Split Editor Vertically*\
Creates two editor views arranged side by sided. This allows the editing of two different
sections of the **same file**.
  
*Split Editor Horizontally*\
Similar to the previous option, but the editor views are arranged one above the other.
  
*Hide Second Editor*\
Reverses the impact of the previous two commands hiding the second editor.

### The Split Workspace submenu

The following commands allow [side-by-side file editing](splitworkspace).
  
![graphic](images/viewmenu3.png){width="14.5em"  height="5.5em"}

*Split Workspace Vertically*\
View secondary workspace vertically aligned to the primary one.

*Split Workspace Horizontally*\
View secondary Workspace horizontally aligned to the primary one.

*Hide Secondary Tabs*\
Hide the secondary workspace and move all contained tabs to the primary one.