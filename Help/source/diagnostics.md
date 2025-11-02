:::{index} Diagnostics; Check Code
:::
:::{index} Diagnostics; Quick Fix
:::

# Code Diagnostics

PyScripter includes [ruff](https://docs.astral.sh/ruff/) an extremely fast linter and language server for the Python language.  Ruff provides  code diagnostics for your Python
code and also provides code formatting.

Code checking happens when you open or save a python file and 
[optionally](ideoptions.md#language-server) as you type. You can also inititate a code 
check at any time by issuing the [Check Code](editmenu.md#source-code-submenu) command.

The flagged issues are highlighted in the editor by a squiggly underline with a color that
depends on the issue sererity. Syntax and other errors are shown in red, warnings in orange
and information messages and hints in yellow.

## Diagnostic hints
TODO: image

## Quick Fix
TODO: image

## The code check menu

TODO: image

When you invoke a code check via the menu, the Messages Window is shown
with the list of found issues sorted by severity.

TODO: image

## Customization

Ruff can perform **over 800 checks**, but only a fraction of those are enabled by default.
You can customize the checks that ruff performs as well as other 
[ruff settings](https://docs.astral.sh/ruff/settings/) in different ways:

- *Global settings* \
  A file named ruff.toml can be found in the PyScripter user directory 
  (%APPDATA%\Pyscripter\Lsp, where %APPDATA% is the folder pointed by the environment 
  variable “APPDATA” or the root folder of PyScripter for registry-free installations).  
  This file contains the default ruff options. You can modify and customize this file.
  The changes in these settings will apply globally.
- *Project specific settings* \
  Global settings can be overwritten by project specific settings. Please consult the 
  ruff [config file discovery](https://docs.astral.sh/ruff/configuration/#config-file-discovery) information for details.

You can view a [video-tutorial](https://pyscripter.blogspot.com/2025/10/feature-preview-code-checks-and-quick.html)
that introduces the above functionality of PyScripter.