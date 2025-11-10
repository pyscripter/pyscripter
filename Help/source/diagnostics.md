:::{index} Diagnostics
:::
:::{index} Diagnostics; Check Code
:::
:::{index} Diagnostics; Quick Fix
:::

# Code Diagnostics

PyScripter includes [ruff](https://docs.astral.sh/ruff/) an extremely fast linter and language server for the Python language.  Ruff provides code diagnostics for your Python
code as well as code formatting.

Code checking happens when you open or save a python file and 
[optionally](ideoptions.md#language-server) as you type. You can also inititate a code 
check at any time by issuing the [Check Code](editmenu.md#source-code-submenu) command.

The flagged issues are highlighted in the editor by a squiggly underline with a color that
depends on the issue sererity. Syntax and other errors are shown in red, warnings in orange
and information messages and hints in yellow.

## The Code Check menu

In addition to the automatic code checks (see above), you can instigate a code check at any time
by using the Code Check command from the [Source Code submenu](editmenu.md#source-code-submenu).
This submenu, is available from both the Edit main menu and the context menu of the editor.

![graphic](images/diagnostic_check_menu.png){align=center width="14.375em" height="9.56em"}

***Commands/Actions:***

*Fix all*\
Performs a quick fix for all issues that can be fixed.

*Check Code*\
Performs a code check to discover errors and issues with the code.

*Clear Issues*\
Clear all issues, and remove diagnostic marks and hints.

*Next/Previous Issue*\
Move the editor position to the next/previous issue.


When you invoke a code check via the menu, the Messages Window is shown
with the list of found issues sorted by severity.  Automatic checks do not 
display issues in the Messages Window.

![graphic](images/diagnostic_messages.png){align=center width="44.625em" height="10.1875em"}

## Diagnostic hints

![graphic](images/diagnostic_hint.png){align=center width="31.94em" height="4.625em"}

When you hover the mouse over the squiggly line of a diagnostic issue, a hint is displayed
with the diagnostic message and the error code like in the image above.  
Clicking on the error code, displays a web page in you browser with information about this type 
of issue.  

Many issues can be fixed quickly.  If the issue can be fixed, two clickable commands will be 
displayed in the hint, below the message.

- **Quick Fix**
- **Ignore**

Quick fix fixes the issue and Ignore adds a comment that instructs the linter to ignore this 
issue such as the following:

```python
import logging  # noqa: F401
```

## Quick Fix Marks

After a code check, issues that can fixed are marked with a red light bulb icon in the editor gutter as in the picture below.  Clicking on the icon displays the following context menu.

![graphic](images/diagnostic_mark.png){align=center width="12.94em" height="6.06em"}

You can choose to either fix the issue or ignore it.

## Tab Error Indicator and Hint

If a code check finds syntax or other errors the editor tab flags that with a "bug" icon.  Also
the hint on the editor tab shows a summary of the file check findings as in the image below. 

![graphic](images/diagnostic_editor_tab.png){align=center width="17.8125em" height="4.625em"}

## Customization

Ruff can perform **over 800 checks**, but only a fraction of those are enabled by default.
You can customize the checks that ruff performs as well as other 
[ruff settings](https://docs.astral.sh/ruff/settings/) in different ways:

- *Global settings* \
  A file named ruff.toml can be found in the PyScripter user directory 
  (%APPDATA%\Pyscripter\Lsp, where %APPDATA% is the folder pointed by the environment 
  variable “APPDATA” or the "Lsp" subdirecotry of the root folder of PyScripter for 
  registry-free installations).  This file contains the default ruff options. You can modify and 
  customize this file. The changes in these settings will apply globally.
- *Project specific settings* \
  Global settings can be overwritten by project specific settings. Please consult the 
  ruff [config file discovery](https://docs.astral.sh/ruff/configuration/#config-file-discovery) information for details.

---
You can view a [video-tutorial](https://pyscripter.blogspot.com/2025/10/feature-preview-code-checks-and-quick.html)
that introduces the above functionality of PyScripter.