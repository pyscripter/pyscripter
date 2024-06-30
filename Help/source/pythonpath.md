:::{index} Python Path
:::

# Python Path Configuration

This feature  allows the configuration of the Python Path of the embedded Python interpreter. 
It is accessible from the [Tools menu](toolsmenu) or the context menu of the [File Explorer](fileexplorer).  It pops-up the following dialog:

![graphic](images/pythonpath1.JPG){align=center width="26.88em" height="13em"}


Pressing the Add.. button shows a directory selection dialog from which can select a directory 
to add to the path. The Modify button replaces the selected directory with one chosen using the 
directory selection dialog. 


*Tip:*\
You can rearrange the order of the directories by dragging and dropping.


*Note that the changes to the Python path apply only to the current session. To permanently 
change the Python path you can either modify the PYTHONPATH environment variable 
or modify the HKEY_LOCAL_MACHINE\SOFTWARE\Python\PythonCore PythonPath registry 
setting. Alternatively, you can use the PyScripter [startup script](startup-python-scripts)
python_init.py to modify the path.*
