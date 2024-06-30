:::{index} IDE Windows; Find in Files
:::
:::{index} Find in Files
:::


# The Find-in-Files Window


The Find-in-Files  Results window is where the results of a 
[Find-in-Files  Search](findinfiles) are shown. It also provides an interface for 
multi-file search and replace  on matches. The window uses a folding display of matches 
to allow you  to easily locate a particular match. This window supports IDE docking. 
An example of this window appears below.

![graphic](images/findinfileswindow1.JPG){align=center width="30.69em" height="22.81em"}


*The Context Menu*

![graphic](images/findinfileswindow2.JPG){align=center width="15.94em" height="17.69em"}


The results window displays all files which contained one or more matches for the search 
term. Under each file, a list of matches for that particular file can 
be shown. To expand or contract a file's matches, click on the filename, 
press enter, or use the '+' and '-' keys. When a specific match line is 
selected, the window can show a number of lines of match context using the Show 
Match Context menu item. Note that the match context might not be accurate if 
you have edited the searched files since the search.

The number  to the left of each match is the line number where the match was found. 
The results list highlights the matching characters in each entry to indicate 
where the match occurred.

To jump to a match in the IDE editor, double click the desired line, press enter, or use 
the Goto toolbar button. To start a new search, click the Search button 
and the Find-in-Files Search dialog will appear. As the search progresses, the 
new search button will be disabled and the abort button will be enabled to cancel 
the current search. Once a search is completed, the results window displays 
on the status bar the number of files searched, the search time, and the total 
number of matches.

You can expand all items in the list by clicking the expand button in the toolbar. 
Similarly, clicking the contract button in the tool bar will contract all result items. 
The entire match list can be coped to the clipboard or saved to a file using 
the items on the File menu.


**Search and Replace on Matches**

You can do a search and replace operation on all of the matches in the list or only the 
selected file/match. When you choose one of those options, the dialog 
below appears prompting for the string to use in place of the matched text. 
If you were using a Regular Expression search you can use sub-expressions 
in the replace expression using the $x syntax, where x is the sub-expression 
number.

![graphic](images/findinfileswindow3.JPG){align=center width="23.38em" height="11.88em"}


**Find-in-Files Results options**

Using the Options dialog below, define how selected matches are shown in the editor, whether 
to expand all matches by default, the number of context lines to show, and the 
list and context fonts/colors. 

![graphic](images/findinfileswindow4.JPG){align=center width="17.06em" height="19.44em"}

*Credits: This utility is based on code from the GExperts project* ([www.gexperts.org](http:\\www.gexperts.org )).
