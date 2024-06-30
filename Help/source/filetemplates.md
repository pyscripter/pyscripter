:::{index} File Templates
:::

# File Templates

:::{index} New File Dialog
:::
  
## The New File Dialog

This dialog allows to create a new file based one of the many pre-defined file templates. You 
can access the dialog (shown below) from the [File Menu](filemenu) selecting 
New, File...  

![graphic](images/filetemplates1.JPG){align=center width="29.50em" height="20.56em"}

In this dialog select a category in the tree view on the left side and a specific template on 
the right side. Then press the "Create" button to create a new file based on the selected 
template. The "Manage File Templates..." button allows you to customize the available templates (see below).  
  
:::{index} Customization; File Templates
:::

## File Template Customization

You can customize file templates through the File Templates dialog shown below. This dialog is 
accessible either via the New File dialog shown above or through the Options submenu of the [Tools Menu](toolsmenu).  
  
![graphic](images/filetemplates2.JPG){align=center width="33.38em" height="30.19em"}

With this dialog you can modify add new templates or modify/delete existing templates. Each 
template has the following properties: 

*Name*\
The name of the template that appears in the New File dialog.

*Category*\
The category under which this template will be listed.Default Extension:The default extension 
that will be added to the filename when a file based on this template is saved.

*Highlighter*\
The syntax highlighter that will be used for files based on this template.

*Template*\
The actual text that will be inserted into new files created from this 
template. The template text can contain [custom parameters](parameters) 
which are expanded upon the activation of the template. If the character "|" is 
present in the template, after the insertion of the template text, the cursor 
is placed at the position of that character and the character is deleted.   
