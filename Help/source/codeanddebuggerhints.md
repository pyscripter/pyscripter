
# Code and Debugger Hints

The Pyscripter editor supports code and debugger hints. These hints are displayed when you 
let the mouse hover on a Python code identifier.   

:::{index} Code Hints
:::
## Code Hints   

These hints display information about the definition of the identifier under the mouse i.e. 
module name and line number in which the identifier is defined. Additional information is 
displayed according to the type of the identifier, e.g. parameters of functions, superclasses of 
classes or the type of variables. When the source code of the module in which the identifier 
was defined is available, the hint presents a hyperlink which, if clicked, takes you to the 
definition. This works in a similar way to the [Find Definition](finddefinition) 
feature and and the found definition is added to the Find Definition browsing history.   

:::{index} Debugger Hints
:::
## Debugger hints

While debugging, code hints are not available, but instead debugger hints are displayed. 
These hints show the value and type of the identifier under the cursor. Debugger hints work 
also with expressions e.g. sys.path[1]. For debugger expression hints place the cursor on a 
closing parenthesis ')' or square bracket ']' 

*Note:*\
Code and/or debugger hints can be disabled from the [IDE options](ideoptions) dialog box.


