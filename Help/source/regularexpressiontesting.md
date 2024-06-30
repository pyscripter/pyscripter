:::{index} Regular Expressions
:::

# Regular Expression Testing

PyScripter provides for integrated regular expression testing. From the [View menu](viewmenu) select IDE 
Windows, Regular Expression Tester to show the following window.  
  
![graphic](images/regularexpressiontesting1.JPG){align=center width="17.94em" height="32em"}

In this window you can type a regular expression and the search text and then press the Execute 
button to see the matched text and the value of each group of the regular expression. If you use 
the "findall" search type (see options below) then you can examine all matches found by using 
the spin edit control in the matches section.  

**Buttons on the Toolbar**  

*Clear*\
Clears all the information entered.  

*Options*\
Allows to specify the various options of the re Python module such as IGNORECASE, VERBOSE 
etc. as well as whether you want to use the search, match or findall function of the regular 
expression objects. For more information look at the Python help file in the "re (standard 
module)" page. 

The "Auto Execute" option determines whether the regular expression is executed 
every time the regular expression or the search text is changed.   

![graphic](images/regularexpressiontesting2.JPG){align=center width="8.31em" height="14.63em"}

*Execute*\
Executes the search using the options specified and shows the results. There is no need to 
press if the "Auto Execute" option explained above is checked.

*Help on re*\
Shows information about the re Python module and the syntax of regular expressions from the 
Python help file.  
  
*Note:*\
For more fully-featured regular expression testing you can use other more specialized programs 
such as [Kodos](http://kodos.sourceforge.net/) or [Kiki](http://project5.freezope.org/kiki/). 
You can easily integrate such programs with PyScripter as* [External Tools](externaltools).


