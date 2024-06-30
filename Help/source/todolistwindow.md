:::{index} IDE Windows; To-do List
:::

# The To-do List Window

The To Do List helps you organize a list of items in your source that need special attention. 
You can click a column header to sort by any column in the list.  

![graphic](images/todolistwindow1.JPG){align=center width="30.38em" height="15.31em"}

To add new to do items, type comments in your code such as:\

```python
#ToDo1 Rewrite this code to work under Linux\
#ToDo2 Add support for Oracle here later
```

To jump to a To-Do item in the IDE editor, double click the desired line, press enter, or use 
the Goto toolbar button. Using the buttons in the toolbar or the similar options in the context 
menu, you can refresh or print the To-Do list. Clicking in header columns sorts the items 
according to that column. Clicking the same column again changes the sort order.

**To-Do options**  

The to do keywords (such as ToDo1, ToDo2, etc.) can each have an assigned priority of 
High, Medium, or Low. You can also add new keywords with associated priorities using the 
To-Do configuration dialog shown below. This utility can scan open files, all project files, or 
complete directories for to do items. It may be helpful to create an 
IDE [Code Template](codetemplates) to quickly create new to do items while you are coding.   
  
![graphic](images/todolistwindow2.JPG){align=center width="32.38em" height="18.50em"}
  
*Credits: This utility is based on code from the GExperts project* ([www.gexperts.org](http:\\www.gexperts.org)).

