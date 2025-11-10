:::{index} Refactoring
:::
:::{index} Refactoring; Rename
:::
:::{index} Refactoring; Organize Imports
:::

# Refactoring

PyScripter supports a number of refactorings, by exploiting the relevant Language Server
functionality.

## The Refactoring Menu

You can invoke the refactoring menu while working in the editor using:
- the main menu [Edit, Source Code](editmenu.md#source-code-submenu) 
- the Source Code submenu of the editor context menu
- the refactor menu shortcut (Shift+F2)

This menu contain only those refactorings that apply to the current editor position.
The image below shows the full list of available refactorings.

![graphic](images/menu_refactor.png){align=center width="10.5625em" height="8.9375em"}

## Organize Imports

Sorts imports alphabetically and separates into sections and by type.  It also splits
import statements with multiple modules to separate ones.

## Rename

Renames the identifier under the cursor (or selected).  You will be prompted to provide
a new name.  If the current file belongs to a project that specifies 
[Extra Python Paths](projectexplorer.md#the-import-directory-dialog-box), the renaming is
project-wide.

## Extract Variable

Moves an expression to a new statement.  You select the expression you want to extract
and invoke the command.  By default the new variable will be called `extract_var`, but
you can rename it to your preferred name using the rename refactoring.

## Extract Function

Moves an expression to a new statement.  You select the expression you want to extract
and invoke the command.  By default the new function will be called `extract_def`, but
you can rename it to your preferred name using the rename refactoring.

## Inline

This is basically the opposite of extracting a variable.
The variable name under the cursor is replaced with the underlying expression.

---

You can view a [video-tutorial](https://pyscripter.blogspot.com/2025/10/feature-preview-refactoring.html)
that introduces PyScripter various refactorings with examples.