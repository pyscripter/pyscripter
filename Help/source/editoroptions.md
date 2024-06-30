:::{index} Customization; Editor Options
:::

# Editor Options

A number of editor options are user-configurable. To customize the
editor options select "Editor Options..." from the   [Tools](toolsmenu)|Options
menu which displays the Editor Options dialog. This dialog contains
four tabs and the options in each tab are explained below. You
can apply the changes to either the Active editor if any or else to
all editors, by checking the relevant checkbox at the bottom of the
dialog. Changes applying to all editor windows affect also the
key bindings and the syntax highlighter colors interactive
interpreter, but not the other options. Use the context menu of
the interactive interpreter to changes the other editor options of the
interpreter window.
    
:::{contents}
:local:
:::
  
## Display tab

![graphic](images/editoroptions1.JPG){align=center width="37.94em" height="28.56em"}

*Gutter (The Margin at the left hand side from the Editor)*
  
  - *Visible*\
    If not checked the right margin with the line numbers will not be visible.
  - *Autosize*\
    Autosize the width to the line numbers.
  - *Gutter Gradient*\
     If checked a gutter gradient is painted according to the selected theme.
  - *Gutter color*\
    The colour used when the gutter gradient is disabled.
  - *Show line numbers*\
    Self explanatory.
  - *Show leading zeros*\
    If checked it shows leading zeros in line numbers.
  - *Zero start*\
     The first line number will be zero.
  - *Gutter Font*:\
    Changes the Gutter font
  
*Right Edge (a gray line showing the right margin)*:

  - *Edge column*\
    Enter the count of characters where the right edge should appear. Use
    zero to hide this line.
  - *Edge color*\
    Changes color of the right edge.
  
*Active Line Color*\
 Select the color for highlighting the active editor line or None if
 you do not want to highlight it.
  
*Bookmarks*:

  - *Bookmark keys*\
    Enable the bookmark shortcuts (see the [Keystrokes tab](#keystrokes-tab)).
  - *Bookmarks visible*\
     Show bookmarks in the editor gutter.


*Line Spacing/Tab spacing*:

  - *Extra lines*\
    Extra spacing between the lines.
  - *Tab width*\
    The width of tabs in characters.

*Editor Font*\
 Changes the editor font.
 

## Options tab
  
Various other editor options. 

![graphic](images/editoroptions2.JPG){align=center width="42em" height="25em"}
  
*ALT sets column mode*\
If activated then the Alt key sets the editor in column selection
mode. i.e. you'll be able to select columns from a text. 

*Auto indent*\
Enables automatic indentation whereby a new line keeps the indent of
the previous line.

*Disable scroll arrows:\
Disables the scroll bar arrow buttons when you can't scroll in that
direction any more.

*Drag and Drop editing*\
If activated you will be able to drag text and drop it to another position. 

*Enhanced Home key*\
If activated and you use the Home key one time, the caret is placed at
the first occurrence of a non white-space, the second time it's placed
in column 1. 

*Enhanced End key*\
End key behaves in a way analogous to the Enhanced Home.

*Group Undo*\
Concequtive editing actions are undone/redone together

*Half page scroll*\
When scrolling with PageDown and PageUp keys, scrol of a half screen
instead of a full. 

*Hide scrollbars as necessary*\
If enabled, the scrollbars will only show when necessary. If you
have selected "Scroll Past end of line", then it the horizontal bar
will always be there. 

*Insert/overwrite caret*\
Select the caret to be displayed in insert and overwrite editor modes.

*Maintain caret column*\
When moving through lines w/o the option "Scroll past end of line",
keeps the column position of the cursor. 

*Right mouse moves cursor*\
When clicking with the right mouse for a popup menu, move the cursor
to that location. 

*Scroll by one less*\
Scrolls page by one line less than the page length.

*Scroll hint follows mouse*\
The scroll hint follows the mouse when scrolling vertically. 

*Scroll past end of file*\
Cursor can go after the theoretical end of file. 

*Scroll past end of line*\
You'll be able to place the caret beyond the text (width). 

*Show scroll hint*\
Shows a hint window with the numbers of the displayed lines when you
scroll the editor.

*Show font ligatures*\
Show [font ligatures](editorfonts.md#font-ligatures) when using a font that 
supports them (e.g. Fira Code) the editor. 

*Show special chars*\
Show special chars such as TABS and new lines. 

*Smart tab delete*\
Similar to Smart tabs, but applies to character deletion with the
BackSpace key. 

*Smart tabs*\
When tabbing, the cursor will go to the next non-white space character
of the previous line. 

*Tab Indent*\
When some text is selected the Tab key indents and the Shift-Tab key
unindents the selection.

*Tabs to Spaces*\
Replaces tabs with spaces while you are typing. 

*Trim trailing spaces*\
Removes all spaces at the end of the lines. 

*Word wrap*\
Enable/disable word wrap. Please note that word wrap is not compatible
with code folding. To enable word wrap you have to first disable code
folding.


## Keystrokes tab

This tab allows you to customize the editor [shortcuts](editorshortcuts). 
    


Note that the second keystroke is for defining a sequence of keystrokes as a 
shortcut (e.g. Ctrl+K, I)

![graphic](images/editoroptions3.JPG){align=center width="37.37em" height="23.31em"}


## Syntax Colors tab

The options in this tab allow you to customize the syntax
highlighting for Python and other file types.  

![graphic](images/editoroptions4.JPG){align=center width="37.625em" height="23.125em"}
    

##Color theme tab

In this tab you can preview and apply available syntax highlighting
themes. These themes are located in the
%PROGRAMDATA%\PyScripter\Highlighters directory.  

![graphic](images/SelectTheme.JPG){align=center width="39.53em" height="35.625em"}
