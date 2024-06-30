:::{index} PyScripter; Command-line Options
:::

# Command-line Options

When invoked without any arguments PyScripter will load the latest version of Python 
and restore the files which were open when the last editing session ended. 
If no files were open an empty Python module is created. This behavior 
can be changed with command-line arguments:

*PyScripter [--pythonversion] filename1 filename2 ...*
  
where pythonversion can be PYTHON38, PYTHON39 etc.

If pythonversion is provided on the command-line PyScripter tries to use that version 
if it is available.

If one or more filenames are provided on the command-line they are opened when PyScripter 
starts.

To open a file at a specific line and column use "filename (lll:ccc)", i.e. 
the filename should be followed by a space and enclosed in parentheses the line 
and column numbers separated by colon. The whole expression should be enclosed 
in double quotes. The expression in double quotes should match the regular 
expression ```(.+) \((\d+):(\d+)\)$```

**Other command line flags:**

--PROJECT filename

Open a specific  PyScripter project file 

*--PYTHONDLLPATH*

In order to  allow PyScripter to work with unregistered version of Python such as 
[Portable  Python](http://www.portablepython.com/), 
another command line argument is provided PYTHONDLLPATH. When such an argument 
is provided the registry search is bypassed and the Python DLL found in that 
path is used instead. 

e.g. 
 PyScripter --PYTHON38 --PYTHONPATHDLL "E:\PortablePython" 


*--NEWINSTANCE* or -N

If set a new instance of Pyscripter is started. This to prevent the default behavior 
 which is to activate an existing instance of PyScripter is one is running.


**Further information on --PYTHONDLLPATH**

- PyScripter without any command line flags looks at the registry to find the latest version 
  of Python.

- When PyScripter is used with a --PYTHONxx flag then it does the above but searching only 
  for the specific version. 
  
- The Registry lookup does not take place when PyScripter is used with the --PYTHONDLLPATH. 
  Instead PyScripter tries to load the Python dll from the specified path.

- The --PYTHONDLLPATH flag should be used with the --PYTHONxx flag. 

- The %PYTHONHOME% environment variable is not used by PyScripter directly but by
  Python to find the installed libraries. See the Python documentation for its use.
 



