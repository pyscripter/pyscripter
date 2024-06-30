:::{index} IDE Windows; Unit tests
:::
:::{index} Unit testing
:::

# The Unit Tests Window

This window provides an advanced GUI for running tests based on unittest, the standard Python 
module.

![graphic](images/unittestswindow1.JPG){align=center width="14.69em" height="31.63em"}

***Toolbar Commands:***  

*Refresh*\
Loads unit tests from the currently active module. Note that this involves importing the module 
into the integrated interpreter. After loading a module you can then select (check) the tests you 
want to run at the provided tree view. Double clicking on a test or a test class name takes you to 
the source code where the test method or class are defined.

*Clear*\
Clears all the tests and related information from this window.  

*Run*\
Run the selected tests. After running the tests their status is indicated by the colour next to the 
test in the tree view. Green indicates success, Purple indicates assertion failure and Red 
indicates a Python exception (i.e. any other error). The pane below the tree view shows the 
overall statistics and you can view information about the errors that occurred by clicking on the 
tests that failed. 

*Stop tests*\
If clicked while running the tests the testing process stops.

*Select all*\
Selects all available tests.*Deselect all*Deselects all available tests.

*Select failed tests*\
After running a set of tests this command selects the tests that were not successful.

*Expand all*\
Expands all tree nodes.

*Collapse all*\
Collapses all tree nodes.
  
*Note:*

To use this GUI to run tests from multiple Python files (for example tests1.py, tests2.py and 
tests3.py) create a new script with the following content:  

```python
from tests1 import *
from tests2 import *
from tests3 import *  
```

Then use the Unit Tests GUI with that file. (Press the Refresh button while this file is active).  



