:::{index} Programmer Utilities; Find in Files
:::
:::{index} Find in Files
:::


# Find in Files

PyScripter incorporates a powerful search facility that enables you to quickly locate text 
strings in files. Using "Find in Files", you can search the current file, all open files or 
all files in a directory (optionally including sub-directories). 

To begin a search, select "Find in Files" from the PyScripter menu. A dialog 
will appear like the one below into which you can enter your search criteria. 
Note that the word under the cursor or the selected text when calling up this dialog, 
will be used as the default search string.

![graphic](images/findinfiles1.JPG){align=center width="22.94em" height="19.63em"}

The various options in the search dialog are as follows:

**Text to  Find:** The text or regular expression to search for.

**Options:**

| Option | Meaning |
| --- | --- |
| Case sensitive: | Search is case sensitive (a and A are treated as different characters).|
| Whole word: | Return  matches that are whole words (delimited by whitespace or punctuation such as "().,<>-{}!@#$"). Note that 0-9 and \_ are treated as  part of a word. |
| Regular expression: | The text to find is a regular expression. |
| Ignore Comments: | Ignore  matches in comments for Python files
 

**Where** (search scope):

| Option | Meaning |
| --- | --- |
| Current file only: | Only the file that is currently in focus for editing |
| Open files: | All files that are currently open in the editor |
| Project files: |All project files |
| Search in directories:| All files specified by the Directory Search options (see below)|

**Directory  Search:**

Note: This portion of the dialog is only enabled if "Search in directories" is 
selected. [Custom paremeters](parameters) are supported in the directories field.

| Option | Meaning |
| --- | --- |
| Directories: |A semicolon separated list of directories to search |
| "..." Button: |Allows browsing for a search directory
| File masks: | Limits the search to a semicolon separated list of file extensions|
| Include subdirectories: | Enables recursive searching of the search directories


Once you have  entered the search criteria, click the OK button or press enter to initiate 
the search. As the search progresses, results will be shown in the 
[Find in Files Results](findinfileswindow) window. From the results window, 
you can also perform multi-file search/replace on the matches.

*Credits: This utility is based on code from the GExperts project* ([www.gexperts.org](http:\\www.gexperts.org)).
