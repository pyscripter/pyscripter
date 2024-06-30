:::{index} Unit test wizard
:::
:::{index} Unit testing
:::


# Unit Testing

PyScripter provides important support for unit testing. The support comes in two levels:

**a) Automatic generation of test scripts**  

Use the Unit Test Wizard command from the Tools menu to generate the basic structure of a test 
script. This command invokes the following dialog box:  
![graphic](images/unittesting1.JPG){align=center width="25.38em" height="27.25em"}
  
The source code of the active module is scanned and its functions and methods are displayed. 
You can select (check) the functions and methods for which you want to generate tests and then 
press OK. A test script based on the unittest standard Python module is automatically generated 
for you. You can then write the code for each test that is generated.  
  
**b) GUI for unit testing**  

From the [View Menu](viewmenu) select Unit Tests to show the 
[Unit Tests window](unittestwindow). This window provides an advanced user interface for
running unit tests based on Python's unittest standard module.

*Note:*

To use this GUI to run tests from multiple Python files (for example tests1.py, tests2.py and 
tests3.py) create a new script with the following content:  

```python
from tests1 import *
from tests2 import *
from tests3 import *  
```

Then use the Unit Tests GUI with that file. (Press the Refresh button while this file is active).  

