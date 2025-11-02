:::{index} Language Server, LSP, Jedi
:::

# Language Server Protocol (LSP)

Modern programming editors and Integrated Development Environments (IDEs), help programmers 
to write better code efficiently, by providing features such as:

- [Code completion](codecompletion)
- [Call tips](codecompletion.md#call-tips) (Signature help)
- [Code explorer](codeexplorer) (hierarchical view of module symbols)
- [Code hints](codeanddebuggerhints)
- [Find definition](finddefinition)
- [Find references](findreferences)
- [Refactoring](refactoring)
- … and more

These features are collectively referred to as “Code IntelliSense”, a term coined by 
Microsoft. To provide such features editors and IDEs need to parse and analyze source 
code, while users are typing code. A recent trend has been to move the Code IntelliSense 
processing out of the editors and IDEs by using external Language Servers.  Microsoft has 
standardized the communication between IDEs and Language Servers by promoting the adoption 
of the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) (LSP).  
The LSP is now used by Microsoft’s Visual Studio and Visual Studio Code as well as by many 
commercial and free IDEs.  There are numerous Language Server implementations for many programming languages, including Python.

PyScripter implements the Language Server Protocol and uses the python 
[Jedi Language Server](https://github.com/pappasam/jedi-language-server), which is
based on the [Jedi library](https://github.com/davidhalter/jedi), 
that is used by many other IDEs. This language server is bundled with the PyScripter
distribution.  

PyScripter also includes [ruff](https://docs.astral.sh/ruff/), an extremely fast python 
linter, to provide code diagnostics and quick fixes.
 
:::{toctree}
finddefinition
findreferences
refactoring
diagnostics
:::
