:::{index} Main Menu; Edit
:::

# The Edit Menu

![graphic](images/editmenu1.png){width="13.06em"  height="24.94em"}

***Commands/Actions:***

*Undo*\
Undo the last change in the Editor.

*Redo*\
Reverse the action of the last Undo command.

*Cut*\
Cut the selected text. If no text is selected cut the current line to the clipboard.

*Copy*\
Copy the selected text to the Clipboard. If no text is selected, this action will copy the 
current line to the clipboard.

*Paste*\
Paste the selected text from the Clipboard.

*Delete*\
Delete the selected text.

*Select All*\
Select all the text in the active editor.

*Read Only*\
Enable or disable editing in the active editor. Files opened by PyScripter during the debugging process 
are read only by default to prevent accidental changes.

*Insert Template*\
Insert a [code template](codetemplates) in the active editor.


### Source Code submenu

![graphic](images/editmenu2.png){width="15.24em"  height="10.69em"}

***Commands/Actions:***

*Indent Block*\
Indent the selected block of code.

*Dedent Block*\
Dedent the selected block of code.

*Toggle Comment*\
Comment out the selected block of code by inserting "##" at the beginning of each 
line. Uncomment by deleting "##" at the beginning of each line of the selected block of code.

*Tabify*\
Replace spaces with tabs in the selected block of code.

*Untabify*\
Replace tabs with spaces in the selected block of code.

*Execute Selection*\
Execute the current editor selection in the interpreter (multi-line selection). If 
only part of a line is selected, the selection is evaluated and the result is 
printed in the interpreter window. Finally, interpreter evaluates the word at the 
cursor if there is no selection.


### Spelling submenu

![graphic](images/editmenu5.png){width="10.688em"  height="11.188em"}

***Commands/Actions:***

*Check Word*\
Spell check the word under the cursor.

*Check Line*\
Spell check the active line.

*Check Selection*\
Spell check the current selection.

*Check File*\
Spell check the active file.

*Clear Errors*\
Clear all spell check error indicators in the current file.

*Check As You Type*\
If checked, the text you type is immediately checked.  
  
See this [blog post](https://pyscripter.blogspot.com/2022/10/feature-preview-spell-checking.html) 
for more explanations.


### Parameters submenu

![graphic](images/editmenu3.png){width="16.94em"  height="5.5em"}

***Commands/Actions:***

*Insert parameter*\
Insert a [parameter](parameters) at the current editor position.

*Insert modifier*\
Insert a [parameter modifier](parameters) at the current editor position.

*Replace parameters*\
Replace parameters [parameter](parameters) with their values in the 
active editor.

### File Format submenu

![graphic](images/editmenu4.png){width="10.01em"  height="13.44em"}

***Commands/Actions:***

*Ansi*\
If checked, the active file will be saved in Ansi encoding. Python files may provide an encoding comment 
(see [Python Source File Encodings](encodedsourcefiles)).

*UTF-8*\
If checked,  the active file will be saved in the [UTF-8 encoding](encodedsourcefiles) 
including the Byte Order Mark (BOM) mark.

*UTF-8 (No BOM)*\
If checked, the active file will be saved in the [UTF-8 encoding](encodedsourcefiles) 
without the BOM mark.

*UTF-16LE*\
If checked, the active file will be saved in the UTF-16 LE (little-endian) format.

*UTF-16BE*\
If checked, the active file will be saved in the UTF-16 BE (big-endian) format.

*DOS/Windows*\
If checked,  the active file will be saved using DOS/Windows line breaks (CRLF).

*UNIX*\
If checked, the active file will be saved using Unix line breaks (LF).

*Mac*\
If checked, the active file will be saved using Mac line breaks (CR).

