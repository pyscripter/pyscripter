:::{index} IDE Windows; Interpreter
:::

# The Python Interactive Interpreter

PyScripter provides an integrated interactive Python interpreter featuring command history, 
code completion and call tips. This window also serves as the standard output of scripts 
running within the IDE. During debugging and when the execution has stopped at a 
breakpoint, the prompt changes to "[Dbg]>>> ".  

![graphic](images/pythoninteractiveinterpreter1.JPG){align=center width="38.50em" height="8.88em"}
  
**Context Menu**  

![graphic](images/pythoninteractiveinterpreter2.PNG){width="15.56em" height="13.75em"}
  
*Copy (No Prompts)*\
Copies the selected text to the clipboard without the interpreter prompts.

*Paste & Execute*\
Pastes text from the clipboard adding each contained statement to the prompt and interpreter 
history and executing it.

*Copy History*\
Copies the entered command history to the clipboard.  

*Clear All*\
Clear all interpreter output.  

*Interpreter Editor Options...*\
Shows the Editor Options Dialog (see the [Editor Options](editoroptions) topic for details) for the Interpreter 
Window.

The Python engine submenu is the same to the one available in the [Run menu](runmenu).  

**Command History**  
- Alt-Up : previous command  
- Alt-Down : next command  
- Esc : clear command  

If you scroll up and click on a previously issued command, possibly modified, then this 
command is copied to the current prompt ready to be reissued. Copy and paste operations 
work as in the text editor, but pieces of code need to be entered line-by-line.  


*Command Filtering:*\
If you type some characters in the Python prompt and then invoke the 
history commands the history is filtered and only entries matching what you typed are shown.  

*Up/Down Keys:*\
Up/Down keys can be used for the history previous/next commands, when the cursor is at the 
last line of the interpreter and this line contains the Python prompt. In that case thought the 
Up/Down keys are unavailable for scrolling, so you have to use the mouse to move to say the 
previous line, beyond which the Up/Down keys work as normal.  

**Code Completion and Call Tips** 

Code completion and call tips are available when you type code 
in the interactive Python interpreter window. Click [here](codecompletion) for details. 

**Traceback Information**

Traceback information is displayed in red. By double clicking on a line with traceback call 
stack information the corresponding file position is displayed in the editor if available.  

**System Commands**

You can issue system commands from the Interpreter window by prefixing the command with an
exclamation mark.  For example "!dir" shows you the contents of the current directory.
