:::{index} Encoded Source Files
:::

# Encoded Source Files

The editor internally uses Unicode strings. When saved, Python files can be encoded in either utf-8 or Ansi encoding.  **The default encoding for source code files is UTF8** in Python 3. However, PyScripter fully supports [PEP 263](http://www.python.org/dev/peps/pep-0263/). 

**UTF-8 encoded source files**  

You can select this encoding from the File Formats submenu of the [Edit menu](editmenu). 
From that menu you can also select whether UTF-8 encoded source files include the BOM UTF-8 signature, which is detected by the Python interpreter and other editors. This signature is also detected by PyScripter when a file is loaded.  Although it is not necessary you may include an encoding comment such as  

```*# -\*- coding: utf-8 -\*-*```

as the first or second line of the python script. The advantage of using UTF-8 encoded files is 
that they can run without modification in other computers with different default encoding.

**ANSI encoded files**

To define a specific source code encoding other than UTF-8, select ANSI from the File Formats submenu of the [Edit menu](editmenu) and place a PEP 263 magic comment into the source file either as first or second line, e.g.:   

```python
#!/usr/bin/python 
# -\*- coding: <encoding name> -\*-
```

More precisely, the first or second line must match the regular expression ```coding[:=]\s\*([-\w.]+)```. The first group of this expression is then interpreted as encoding name. If the 
encoding is unknown to Python, an error is raised during compilation. There must not be any 
Python statement on the line that contains the encoding declaration.  PyScripter detects such comments when it loads Python Source files and decodes them to Unicode using the appropriate 
encoding. **If such a comment is not present then the default encoding UTF-8 is assumed.**

**IDE encoding options for new files**  

Pyscripter provides two [IDE options](ideoptions) controlling the encoding of new files:  
- *Default line breaks for new files* 
- *Default encoding for new files*  

**IDE option for detecting UTF-8 encoding when opening files**  

Another IDE option (*Detect UTF-8 when opening files*) controls whether PyScripter attempts 
to detect utf-8 encoding when opening files without the BOM mark. It only applies to non-Python 
files since Python files without a PEP263 encoding comment are assumed to be utf-8 encoded.


