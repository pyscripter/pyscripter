:::{index} IDE Windows; Project Explorer
:::

# The Project Explorer Window

PyScripter projects serve two purposes:

1. To create and maintain collections of files with which you tend to work together, 
 structured hierarchically in folders and sub-folders. These folders, do 
 not necessarily correspond to file system folders but instead they may based 
 on some other logical categorization of files, for example based on their type 
 (e.g Html, ini and script files grouped under different folders) . This similar 
 to what other IDEs call Workspaces.

2. To  create and maintain an associated set of Run configurations.

Other PyScripter Tools such as the [Find in  Files](findinfiles) and the 
[Todo list](todolistwindow) tools are designed can take advantage of projects.

PyScripter  projects are saved as "ini" files have the default extension "psproj". 
At any point in time one such project is active and if that project is not saved 
it has the name "Untitiled". The Project Explorer IDE Window 
helps you explore and manage PyScripter projects.

![graphic](images/theprojectexplorer1.png){align=center width="18.4375em" height="33.75em"}


The Root project  node always has exactly two child nodes:

1. Files
2. Run Configurations

Under the "Files" node you can add files or folders which can contain further files and folders. 
Under the "Run Configurations" node you can add multiple [Run Configurations](runconfigurations), 
which can be used for running and debugging Python scripts.

*Drag & Drop support*

You can drag  and drop files/folders from the built-in [File Explorer](fileexplorer) 
or the Windows Explorer onto folder nodes to import these files and folders. 
You can also restructure the project by using drag and drop to move project 
nodes.

*Opening files for editing*

You can double  click on files to open them in the editor. Alternatively you can select 
multiple files and select the "Edit" command from the context menu 
(see below).

## Folder types

You can add files and folders under the Files node, using its context menu (see below).  There are two types of folders you can add:

- **Virtual folders**\
These folders allow you to organize your project files in a logical manner.  For example you can 
have a folder for source files, another one for documentation files and another one for 
configuration files.  These folders are just collections of files and do not necessarily correspond 
to physical folders on disk.  They may contain subfolders.  You can create virtual folders using 
the Add commands of the folder context menu.

- **File-system folders**\
These folders mirror the directory structure on disk.  Their content reflects the content of
the physical folders and they are **auto-updating** when the contents of the folders on disk
change, e.g. when files are added, deleted or renamed.  They are created using the Import 
Directory folder context menu.  They may also contain subfolders.

The Project Explorer uses different icons to distinguish between these two types of folder nodes.
For example in the picture above, "Doc" is a virtual folder and "rpyc" a file-system one.


## The Import directory dialog box

![graphic](images/import_directory.png){align=center width="31.3125em" height="15.375em"}

This dialog box is accessible from the folder context menu.  It allows you to import a 
directory into the project.  In the file masks field, you can provide one or more masks
separated by semi-colon that determines which file types will be imported into the project.  If 
"Recursive" is checked subfolders will be imported too.  If "Auto-update" is checked then a 
file-system folder will be created that will automatically refresh, whenever a change in the 
specified directory takes place.  Otherwise, a virtual folder will be created.   Finally, you 
have the option to add the specified directory to the to the project's 
"***Extra Python Path"***, so that the python interpreter can find and import modules in this directory.

## Context sensitive menus

There are different context menus available for each type of node:

###  Window background and Root Project node context menu

![graphic](images/theprojectexplorer2.png){width="11.31em" height="16.5625em"}

***Commands:***

*New Project*\
Clear the active project and start a new one.

*Open Project*\
Open a saved project and replace the active one.

*Save Project*\
Save the active  project.

*Save Project As...*\
Save the active project under a different name.

*Expand All*\
Expand all project nodes

*Collapse All*\
Collapse all  project nodes.

*Show File Extensions*\
If this option is set, file extensions are shown, otherwise hidden.

*Store Relative Paths*\
If this option is set, project files that are in the same directory as the project file or 
a subfolder of the directory are saved as file paths relative to the Project 
file path. 

*Extra Python Path*\
This option allows a project specific customization of the Python Path. Extra directories 
specified are added to the Python path at the time the project is loaded and 
every time an engine is reinitialized.

### Folder context menu

![graphic](images/theprojectexplorer3.png){width="10em" height="12.375em"}

***Commands:***

*Add File(s)...*\
Add on or more files to the selected folder using and Open File dialog.

*Add Active File*\
Add the active editor file to the the selected folder.

*Add Remote File*\
Add a remote file to the the selected folder using the 
[remote file dialog](remotefiles.md#opening-remote-files).

*Add Subfolder*\
Add a subfolder to the selected folder.

*Import Directory...*\
This command recursively imports a folder with its files and subfolders to the project replicating 
 the directory structure on the disk. 

*Rename*\
Rename the selected folder.

*Remove*\
Remove the selected folder and its child nodes.

**Note**:  The first five commands are not available on file-system folders.

### File context menu

![graphic](images/theprojectexplorer4.png){width="7.5em" height="5.9375em"}

***Commands:***

*Edit*\
Open the selected file in the editor. The same can be done by double-clicking the file name.

*Remove*\
Remove the selected file from the project.

*Properties*\
Show the standard Windows File Properties dialog for the selected file.

**Note**:  Remove is disabled in file-system folders.

### Run Configurations node context menu

![graphic](images/theprojectexplorer5.png){width="12.125em" height="1.5em"}

***Commands:***

*Add Run Configuration*\
Create and add to the project a new Run Configuration.

### Run Configuration context menu

![graphic](images/theprojectexplorer6.png){width="12.0625em" height="11.6875em"}

***Commands:***

*Run*\
Run the selected Run Configuration.

*Debug*\
Debug the selected Run Configuration.

*External Run*\
Run the selected Run Configuration using an external Python interpreter.

*Edit Run Configuration*\

Edit the selected Run Configuration.

*Rename*\
Rename the selected Run Configuration.

*Remove*\
Remove the selected Run Configuration.


**The Project Explorer Toolbar**

![graphic](images/theprojectexplorer7.png){width="14.9375em" height="1.6875em"}

***Commands:***

*New Project*\
Clear the active project and start a new one.

*Open Project*\
Open a saved project and replace the active one.

*Save Project*\
Save the active project.

*Run Last Configuration*\
Run the Configuration that was run last.

*Debug Last Configuration*\
Debug the Configuration that was run last.

*External Run Last Configuration*\
Run the Configuration that was run last using an external Python interpreter.
