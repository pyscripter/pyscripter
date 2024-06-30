
# Code Completion and Call Tips

Code completion and call tips are available both in the editor windows and in the interactive 
Python interpreter window.  Code completition and 

:::{index} Editor; Code Completion
:::
## Code Completion

When you type a qualified identifier (containing ".", e.g. sys.modules) as soon as you press 
the "." and after a short delay a list with all available members pops up from which you can 
select using the mouse or filter by typing the first few letters. The current selection in this 
list is copied to the interactive interpreter window as soon as:
- you press ENTER 
- you press TAB
- you press ".", "(", ")", "[", "]" or space

You can hide the code completion list by pressing the ESC key.  
You may also activate code completion at any point either before you start writing an identifier 
name or after, as well as before typing the '.' or after, by pressing the **keyboard shortcut 
Ctrl+SPACE**. At all times you will get a filtered list of the names which are within the scope 
of the position at which you are within a module.  

Support is also provided for the completion of the import statement. e.g. (^ stands for pressing 
Ctrl+Space)
- import ^
- import in^
- import inspect as isp, cty^
- from ^
- from inspect import g^
- from inspect import a as b, g^
- from ..modname import a as b, g^
- from .. import modname as m, another^  

## Code Completion Options

You can customize how code completion works by setting a number of different options in the
[IDE options dialog](ideoptions).  The following options are available:

*Auto-Completion font*\
Allows you to customize the size and type of font used in auto-completion.

*Case Sensitive*\
This option determines whether the filtering of the code completion list
when you type characters is case sensitive (default True). 

*Code completion list size*\
The size of the code completion window in number or lines.

*Complete as you type*\
Code completion is invoked automatically as you type.

*Complete Python keywords*\
Python keywords appear in the completion list when appropriate.

*Complete with word-break characters*\
When the completion list is displayed completion with the currently
selected entry occurs when word-break characters are typed (e.g.
space, brackets etc.). This is in addition to completing
with the Tab and Enter keys.

*Auto-complete with one entry*\
If true, when the completion list contains one entry complete
automatically without showing the list.

*Editor code completion*\
Enable/Disable code completion in the editor. 

*Interpreter code completion*\
Enable/Disable code completion in the interpreter. 

For further explations see this [blog post](https://pyscripter.blogspot.com/2011/08/code-completion-improvements.html).


:::{index} Editor; Call tips
:::
## Call tips

When you open a left bracket "(" after typing a function name or a class name, and after a 
short delay, PyScripter pops up a call tip (hint window) with information about the expected 
parameters of the function you are entering as well as the doc string of the function if it is 
available. This call tip window stays on until you complete entering the function parameters 
and type the right bracket. You can hide a call tip by clicking on it).  

You may also activate call tips at any point after you started writing the parameters of a 
function, by pressing the **keyboard shortcut Shift+Ctrl+Space**.   

*Note: The code and parameter completion should be one of the best you can find in any 
Python IDE. However,if you find that code and parameter completion is not very accurate for 
certain modules and packages such as wxPython and scipy you can achieve near perfect 
completion if you add these packages to the "Special Packages" [*IDE option*](ideoptions)
(comma separated list). By default it is set to "os, wx, scipy". Special packages are 
imported on demand to the interpreter instead of scanning their source code.*


