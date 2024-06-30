:::{index} Main Menu; Search
:::
# The Search Menu

![graphic](images/searchmenu1.png){width="17.79em"  height="26.43em"}
  

***Commands/Actions:***

*Find...*\
Display and activate the "Find" toolbar.

*Find Next*\
Search forward for the next match of a previously searched text pattern.
  
*Find previous*\
Search backwards for the next match of a previously searched text pattern.
  

*Replace...*\
Display the "Find" Toolbar along with the replace text field.

*Highlight Search Text*\
Highlights all occurrences of the search text.

*Find in Files...*\
It is an advanced search. This action will display the "[Find in Files](findinfiles)" dialog box to 
search a text pattern in the given set of selected files respectively.

*Go To Line...*\
Displays an input box for entry of a line number and then repositions the cursor to that line number.

*Go To Syntax Error*\
Jump to the first syntax error (if any) in the active script.

*Go To Debugger Position*\
Jump to the current execution line of the debugger if the debugger is active.

*Find Function...*\
Displays the [Find Function](findfunction) dialog box.

*Find Next Reference*\
Moves the cursor to the next occurance of the identifier containing the caret/cursor.

*Find Previous Reference*\
Moves the cursor to the previous occurance of the identifier containing the caret/cursor.

*Matching Brace*\
If the cursor is at a brace ('(', ')', '[', ]', '{' , '}') moves the cursor to the matching brace.

*Find Definition*\
[Finds the definition](finddefinition) of the identifier containing the caret/cursor.

*Find References*\
[Finds references](findreferences) of the identifier containing the caret/cursor.


### Find Toolbar

PyScripter provides a Firefox like Find Toolbar for search or replace functionality:
  
![graphic](images/searchmenu2.png){width="35.56em"  height="1.69em"}

The last settings button in above Toolbar allows you to select different search options from the 
below menu:

![graphic](images/searchmenu3.png){width="10.88em"  height="12.50em"}
  

Here is a brief explanation of the options:

*Search From Caret*\
If this option is checked, the search begins from the cursor position. Otherwise searches from the 
top of the file.

*Auto Case Sensitive*\
This option activates the case sensitive search if the search text contains upper case characters.

*Case Sensitive*\
Specifies whether the search is case sensitive.

*Whole Words Only*\
If you check this option, the search is restricted to whole words only.  

*Search in Selection*\
If this options is checked, the search is restricted to the current selection, otherwise the 
whole file is searched.

*Regular Expressions*\
If this option is checked, the search text is interpreted as a regular expression. 
The replacement text can contain sub-expressions (e.g. $1).

*Incremental Search*\
If checked, Find Next is executed every time you modify the search text.

  

  



