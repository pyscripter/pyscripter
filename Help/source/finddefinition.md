:::{index} Find Definition
:::

# Find Definition

This utility function allows you to find and jump to the definition of the identifier under 
the cursor, which may be in a different file. This feature can be invoked in two ways:

* From the [Search  menu](searchmenu)
  In this case  PyScripter provides detailed feedback on the found matches in the 
  [Messages Window](messageswindow) (see below) and it reports any problems that may have 
  occurred. If a definition is found the editor jumps to the that definition, which may be 
  in a different file. In some cases the exact definition cannot be found with certainty 
  and this is why the Messages Window also reports the degree of certainty 
  for each candidate definition.

![graphic](images/finddefinition1.JPG){align=center width="44.81em" height="9.38em"}

2. By clicking on an identifier while pressing the Ctrl key.
   When you press the Ctrl key while the mouse hovers on a Python identifier, the identifier 
   appears as a hyperlink. You can invoke the find definition function by clicking 
   on that identifier. If the definition is found the cursor jumps to that 
   definition, otherwise and unlike the case described above, no feedback is provided 
   except for a beep sound.

In both case  the original cursor position and the found definition is added to the Find 
Definition browsing history. You can move backwards and forward within the the browsing 
history by clicking on the Browse Back and Browse Forward buttons in the editor 
toolbar ( the first two buttons in the toolbar shown below). These two 
buttons also provide a drop-down list from which you can select to jump to a 
specific found definition.

![graphic](images/finddefinition2.JPG){align=center width="14.75em" height="1.63em"}
