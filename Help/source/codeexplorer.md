:::{index} IDE Windows; Code Explorer
:::

# The Code Explorer Window

It shows a structured (tree) view of the source code with functions classes and their methods. 
It can help you navigate through the code. Double-clicking on a any function or class name 
moves the editor caret to the section of the code where the respective function or class are 
defined.  

![graphic](images/codeexplorer1.JPG){align=center width="15.81em" height="32.50em"}

**The context menus**

*a) Window Background*  
  
![graphic](images/codeexplorer2.JPG){width="8.94em" height="7.5em"}
  
***Commands:***  

*Expand All*\
Expand all nodes of the tree.

*Collapse All*\
Collapse all nodes of the tree.

*Alpha Sort\
*If checked, tree nodes are sorted alphabetically, otherwise the node order follows the position 
of the identifiers in the code.  

*Follow Editor*\
If checked, as you move the cursor in the editor the class, method or function that contains 
the cursor gets selected in the Code Explorer.  

*Show Selection*\
If checked, when you select a node by mouse or keyboard, the position of the identifier in the 
code is shown without moving the focus to the editor.  
  
*b) Node context menu*  

![graphic](images/codeexplorer3.JPG){width="9.13em" height="4.75em"}  

***Commands:***  

*Find Definition*\
Moves the editor caret to the section of the code where the respective function or class are 
defined. Focus is shifted to the editor. It does the same as double-clicking on the identifier.  

*Find References*\
Invokes the [Find References](findreferences) command for the selected identifier.  

*Highlight*\
If checked the occurrences of the selected identifier in the editor are highlighted.

