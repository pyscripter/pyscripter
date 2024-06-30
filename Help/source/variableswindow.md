:::{index} Debug Windows; Variables
:::

# The Variables Window

During debugging and while the interpreter is stopped at a breakpoint , the
Variables window displays the local and global variables for the
selected stack frame in the [Call Stack](callstackwindow) window
which is usually the top frame. The left pane shows a
hierarchical view with the value of each variable. Any Python
object with a dictionary interface (classes, objects, dictionaries
etc.) can be expanded so that key-value pairs are inspected.
Variables that have been changed or are new while stepping through
code are color coded. Changed variables are displayed with red
color and new variables with blue color. The left hand pane of
the Variables window displays the type, value and documentation of the
selected variable.
  
When the debugger is not active the Variables window displays the global
variables of the interpreter.
  
You cannot change the values of variables in this window. In
fact you cannot change local function variables while debugging in
Python (the locals dictionary is read-only). Global variables
can be changed though in the [Interactive Interpreter](interpreter) window.

![graphic](images/variableswindow.png){width="50.45em" height="10.1em"}

### Variable Inspectors

Variable inspectors, also known as debug visualizers, allow you to visually 
inspect variables of certain data types. In the last column of the Variables 
window a magnifying glass is shown for variables that can be inspected. 
Clicking this magnifying glass, opens a window that provides a visual view of 
that variable, in this case a pandas dataframe. See this 
[blog post](https://pyscripter.blogspot.com/2023/10/feature-preview-debug-inspectors.html) 
for further details and information about how to create your own debug inspectors.
