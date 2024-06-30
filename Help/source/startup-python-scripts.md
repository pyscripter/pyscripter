:::{index} Customization; Startup Python Scripts
:::

# Startup Python Scripts

PyScripter users can create two startup python files that can be run at PyScripter startup.  

1. pyscripter_init.py

   This script is run once in the space of the internal Python engine at 
   start-up. It can be used to set various PyScripter options and to customize the PyScripter 
   user environment.  
2. python\_init.py

   This file is executed when a Python engine, internal or remote, is initialized and every 
   time the engine is reinitialized. It can be used to customize the Python engine, for 
   example by always importing certain units.  
  
PyScripter searches for startup python files first at the PyScripter.exe directory, and if it is 
not found there at %APPDATA%\Pyscripter, where %APPDATA% is the environment variable. The 
PyScripter installation program places these files without any content at 
%APPDATA%\Pyscripter, if the files are not there already.

You can edit the startup files by using the [Tools Menu](toolsmenu) command "Edit Startup Files".
