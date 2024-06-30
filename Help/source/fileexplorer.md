:::{index} IDE Windows; File Explorer
:::

# The File Explorer Window

This is a  powerful file explorer similar to that supplied by the Windows operating system. 
You can navigate through the local file system and open Python scripts by clicking 
on them. You can filter the displayed files and navigate directly to commonly 
used directories and directories on the Python path.

If you have installed [Tortoise Git](http://www.tortoisegit.org/) 
or [Tortoise SVN](http://tortoisesvn.tigris.org/) 
you have access to version control functionality directly from PyScripter.

You can add the folders you commonly use to the **Favourites** list and you can easily 
set the root directory of the File Explorer to one of these folders.

![graphic](images/fileexplorer1.JPG){align=center width="12.50em" height="32.31em"}


**The Toolbar**

**Commands:**

*Browse Back/Forward*\
Navigated through the history of browsed directories. Note that the browsing history 
 is cleared when you change the root directory of the File Explorer.

*Go Up*\
Change the root of the File Explorer to the parent of the currently selected directory.

*Browse Path*\
Change the root of the File Explorer to commonly used directories, directories in the Favourites 
list and directories on the Python path,

*Filter files*\
If selected the File Explorer hides all files except Python scripts and modules. By 
 default these are the files with ".py" extension. You can modify 
this default filter thought the [IDE options](ideoptions) 
customization. 

*Create New Folder*\
Create a new sub-folder of the currently selected folder.


**The Context Menu (on files and directories)**

![graphic](images/fileexplorer2.JPG){width="9.69em" height="25.44em"}

This is the standard explorer menu with the addition of the last item "File Explorer". 
This submenu contains the following options:

- "*Explore here*", which allows to change the root directory of the File Explorer 
 to the selected folder.
- "*Add to Favourites",* which adds the currently selected folder to the *Favourites* list 
 - *"Create  New Folder"* which *c*reates a new sub-folder of the currently 
 selected folder
 - "*Search Path*" which invokes the [Find-in-Files](findinfiles) tool on the selected folder.

![graphic](images/fileexplorer3.JPG){width="8.38em" height="5.75em"}

**The Context menu (on empty space)**

![graphic](images/fileexplorer4.JPG){width="10.69em" height="16.31em"}

This context menu offers options similar to those found in the toolbar plus the following 
commands:

*Manage Python Path...*\
Shows a dialog box from which you can modify (add and remove folders) from the python path 
(sys.path).

*Change Filter...*\
Allows you  to change the filter which applies to the files shown. Use a semicolon 
separated list, i.e. "*.py;*.pyw".

*Refresh*\
Refresh the contents of the File Explorer. Normally not needed since it updates automatically.

**The Browse path submenu**

![graphic](images/fileexplorer5.JPG){width="8.94em" height="7.13em"}

By selecting menu options from this submenu, you can set the root directory of the File Explorer 
to the corresponding file directory. "Active Script" sets the 
root directory to the directory of the script currently edited.


**The Favourites submenu**

![graphic](images/fileexplorer6.JPG){width="12.81em" height="4.56em"}

Shows the list of favourites from which you can select the one you want to set as the 
root folder of the File Explorer. 

It also provides the following commands:

*Add to Favourites*\
Adds the currently selected folder to the *Favourites* list.

*Manage Favourites...*\
Shows a dialog box (see below) from which can manage (add and remove folders) the Favourites 
list.


*The Python Path submenu*

This submenu shows you the directories in the Python path of the embedded Python interpreter. 
Selecting a directory changes the the root directory of the File Explorer to 
that directory

![graphic](images/fileexplorer7.JPG){width="23.38em" height="16.75em"}


**The Manage Favourites dialog box:**

This dialog box allows you to add or remove folders from the Favourites list.

![graphic](images/fileexplorer8.JPG){width="26.88em" height="14.25em"}

