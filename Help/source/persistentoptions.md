:::{index} Customization; Persistance
:::

# Persistent Options

All user options including [IDE options](ideoptions), IDE shortcuts, 
[editor  options](editoroptions), [code templates](codetemplates) 
and [custom parameters](parameters)  are saved in a file called "PyScripter.ini". 
PyScripter searches  for this file in the following locations:

1. Exe directory

2. ```%APPDATA%\Pyscripter\```\
   where ```%APPDATA%``` is the folder pointed by the environment variable "APPDATA"

So the default location is the user's Application Data directory . If however the 
"PyScripter.ini" file exists in the same directory as the PyScripter executable 
then it is used in preference to the one in the user directory. This allows 
for portable and "registry-free" "PyScripter installations in USB storage for example.  

Application form sizes and positions (referred to as layouts) are saved in a 
separate settings file called "PyScripter.local.ini", which is located in the same folder as
"PyScripter.ini". 
