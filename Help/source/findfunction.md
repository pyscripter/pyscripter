:::{index} Find Function
:::

# Find Function

The Find function command pops up a dialog with a list of Python functions and methods 
defined in the current module and allows you to quickly jump to a given function. The function 
list window is pictured below:  

![graphic](images/findfunction1.JPG){align=center width="28.06em" height="20em"}

To search for a function, start typing in the Search edit box. As you type, the characters will 
appear at the top next to the word Search. To jump to a selected function, double click or 
highlight it and press enter.

Two search modes are provided. In the first search mode (match only from the start), 
searches are conducted only on the beginning of the function name (after an optional class 
reference). In the second search mode (match anywhere), the search string can match at 
any point in the function name. For example, if you had two functions, MyClass.assign and 
MyClass.assignWidget, searching for "assign" would return both methods in either mode, 
whereas searching for widget would return neither when searching from the start, while 
MyClass.assignWidget would appear if searching for a match at any point. 

Using the Objects combobox on the right, you can filter the function list to display all 
functions, only those without an associated class, or only those associated with a specific 
class. The Copy button will copy all function details to the clipboard.  

*Credits:*\
This utility is based on code from the GExperts project 
([www.gexperts.org](http:\\www.gexperts.org)).
