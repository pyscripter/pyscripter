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
 
 The use of the LSP brings a number of advantages:
- Improved stability since the source code parsing and analysis is carried 
  out in external processes.
- Better quality of Code IntelliSense, since the Jedi Language Server is actively 
  developed and supports the latest python features.
- The default Language Server (Jedi) can be easily swapped for alternative python 
  Language Servers.
- New features such as code diagnostics and refactoring can be easily added, since 
  they are supported by the LSP.
- Development resources are freed and can be diverted to other improvements of PyScripter.
