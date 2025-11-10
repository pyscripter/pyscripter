:::{index} PyScripter; History
:::

# History

### Version 5.3 (November 10, 2025)

* *New features:*
	+ [Code diagnostics](diagnostics) using Ruff a very fast linter and language server
	+ File Check, with issues shown in the editor
	+ Fixable issues flagged in the gutter
	+ Quick Fix or Ignore fixable found issues
	+ Fix all command
	+ [Refactoring](refactoring) support: Organize Imports, rename, extract variable, extract function, inline
	+ Built-in code formatting (Edit, Source Code, Format)
    + Added [auto-refreshing project folders](projectexplorer.md#folder-types) that mirror physical folders (*#521*)
	+ Scrollbar annotation for highlighted search term

* *Issues addressed:*
	+  *#1407*, *#1409*, *#1411*, *#1418*, *#1419*, *#1436*, *#1438*


### Version 5.2.3 (May 10, 2025)

* *New features:*
	+ Turkish translation added

* *Issues addressed:*
	+  *#1382*, *#1392*, *#1397*, *#1401*


### Version 5.2.2 (April 14, 2025)

* *New features:*
	+ Support for Grok LLM
    + New editor commands Next/Previous change (Shift+Ctrl+Num+/-)
    + IDE option to enable/disable editor accessibility support

* *Issues addressed:*
	+  *#1367*, *#1369*, *#1372*, *#1373*, *#1374*, *#1375*


### Version 5.2.1 (Feburay 10, 2025)

* *New features:*
	+ [LLM Support improvements](https://pyscripter.blogspot.com/2025/02/deepseek-and-other-improvements-to-llm.html)
		+ Added support for DeepSeek LLM
		+ Added support for DeepSeek and OpenAI "o" LLM reasoning models
		+ Improved display of LLM output
		+ Syntax Highlighting for 300 languages
		+ Exposed the temperature LLM parameter
		+ Printing of LLM output
	+ Debugging
		+ Much [faster debugging](https://pyscripter.blogspot.com/2025/01/teaser-super-fast-debugging-is-coming.html) for python >= 3.13	
		+ Debugging improvements for python >= 3.10
		+ Added support for [breakpoint ignore counts](https://pyscripter.blogspot.com/2025/01/breakpoint-conditions-and-ignore-counts.html)
	+ Surround editor selection with brackets and quotes
    + Added two new styles: Windows11 MineShaft (new default) and Windows 11 Impressive Light

* *Issues addressed:*
	+  *#1347*


### Version 5.1.3 (January 7, 2025)

* *New features:*
	+ [Multi-caret and multi-selection editing](https://pyscripter.blogspot.com/2024/10/teaser-multi-caret-editing-is-coming-to.html) (*#483*)
	+ [Annotated scrollbars](https://pyscripter.blogspot.com/2024/10/teaser-annotated-scrollbars-are-coming.html)
	+ [Colored code structure highlight](https://pyscripter.blogspot.com/2024/10/teaser-highlight-program-structure-is.html)
	+ [Display of program flow control symbols](https://pyscripter.blogspot.com/2024/10/teaser-display-of-program-flow-control.html)
	+ Support for TOML files added
	+ Support for Python 3.14
	+ Support for [free-threaded python](https://pyscripter.blogspot.com/2024/12/free-threaded-python-support-is-coming.html)
	+ New external tool "[Create venv](https://docs.python.org/3/library/venv.html)"
    + Support tqdm and similar modules in the interactive interpreter (*#812*)
    + The Chat Window renders Markdown
	+ The Assistant can use a greater variety of Ollama models
    + Layouts now include the secondary workspace status (*#494*)

* *Issues addressed:*
	+ *#1307*, *#1321*, *#1329*, *#1336*, *#1341*, *#1346*



### Version 5.0.1 (September 10, 2024)

* *New features:*
	+ Support for Google's Gemini LLM
* *Issues addressed:*
	+ *#1227*, *#1271*, *#1319*



### Version 5.0.0 (July 1, 2024)

* *New features:*
	+ [Integrated LLM support](llmintegration)
	+ OpenAI and local LLM models using Ollama supported.
	+ New IDE Window Chat for interacting with Large Language Models
	+ Code Assistant functionality (Suggest, Find bugs, Optimize, Comment)
	+ Python 3.13 support added.  Dropped support for python 3.7
	+ Updated and improved documentation (Help file)
* *Issues addressed:*
	+ *#1164*, *#1228*, *#1275*, *#1278*, *#1296*, *#1297*, *#1300*, *#1306*



### Version 4.3.3 (October 31, 2023)

* *New features:*
	+ Python 3.12 support added
	+ Debug Inspectors (*#1219*)
	+ Improved multi-monitor support (per monitor DPI awareness)
	+ Customizable user interface content font size (*#1209*)
	+ Screen reader support in the editor
* *Issues addressed:*
	+ *#1172*, *#1195*, *#1197*, *#1198*, *#1199*,
	 *#1200*, *#1208*, *#1210*, *#1212*, *#1214*,
	 *#1221*, *#1227*, *#1268*, *#1269*



#### Version 4.2.2 (December 10, 2022)

* *New features:*
	+ Internet Explorer replaced with the Edge browser
	+ Added Format Selection external tool using the "black" module
	+ New IDE option 'Automatic Restart'(*#1188*)
	+ Recovery of unsaved files on system shutdown or application
	 crash
	+ New IDE command "Zoom Reset" Atl+Num 0 (*#650*)
	+ Two new styles added: Windows 11 Polar Dark and Windows 11
	 Polar Light
* *Issues addressed:*
	+ *#1152*, *#1155*, *#1177*, *#1181*, *#1182*,
	 *#1183*, *#1185*, *#1186*, *#1187*, *#1189*



#### Version 4.2.0 (November 5, 2022)

* *New features:*
	+ Python 3.11 support added - Support for python 3.6 removed
	+ Spell checking of comments and strings *#84*
	+ Track changes bar as in Visual Studio
	+ Editor Unicode handling improvements (emojis, bi-directional
	 text, etc.)
	+ Editor selection options (alpha blending, fill whole lines)
	+ Complete Portuguese (Brazil) translation added
* *Issues addressed:*
	+ *#1140*, *#1146*, *#1149*, *#1151*, *#1163*,
	 *#1165*


#### Version 4.1.0 (October 15, 2021)

* *New features:*
	+ Implementation of the Language Server Protocol
	+ Python language support provided by the Jedi language server
	+ Two new styles added Windows11\_Light and Windows11\_Dark
	+ Copy and paste code as html to Powerpoint and other
	 applications
	+ Removed support for python 3.3-3.5
	+ Read only indicator on tabs
	+ Added traditional Chinese translation
* *Issues addressed:*
	+ *#939*, *#951*, *#1116*, *#1118*, *#1119*,
	 *#1122*, *#1123*, *#1125, #1129, #1133, #1136*

  

#### Version 4.0.0 (May 5, 2021)

* *New features:*
	+ Major redesign of the User Interface - Material icons and new
	 logo
	+ Re-architecture the interaction with python, code-completion
	 etc. It should result in a more responsive user experience
	 without delays and freezes.
	+ Added support for Python 3.10 and removed support for Python
	 2.7, 3.2
	+ Installer and executable are now code-signed
	+ Persian translation added
	+ New IDE option "Restore open project"
	+ New File Explorer command "Select Directory..." (#1034)
* *Issues addressed:*
	+ *#824*, *#990*, *#1031*, *#1035*, *#1038*
	*#1039*, *#1040*, *#1105*, *#1109*, *#1111*


  
#### Version 3.6.4 (October 6, 2020)

* *New features:*
	+ Added support for Python 3.9 (and removed support for Python
	 2.6)
	+ Added support for virtualenv v20+. Dropped support for earlier
	 versions.
	+ Added support for font ligatures
* *Issues addressed:*
	+ *#998*, *#1001*, *#1003*, *#1008*, *#1009*



#### Version 3.6.3 (January 25, 2020)

* *New features:*
	+ The status panel with text position info can now be clicked to
	 show the "Go to line" dialog
* *Issues addressed:*
	+ *#983*, *#985*
  

#### Version 3.6.2 (November 29, 2019)

* *New features:*
	+ Improved compatibility with venv virtual environments
	+ Restore code folding state when you start PyScripter (#973)
	+ Syntax for adding and removing parameters (#971).
	 $[proc=?Question] adds parameter proc and $[proc=] removes it
	+ Improved DPI scaling
	+ Highlighters and styles are now installed under ProgramData to
	 better support multiple users on the same computer
	+ Two new styles added (Calypso and Stellar)
* *Issues addressed:*
	+ *#948*, *#962*, *#966*, *#967*, *#968*,
	 *#972*


  
#### Version 3.6.1 (July 20, 2019)

* *New features:*
	+ Python 3.8 support. Dropped support for Python 3.0 and 3.1.
	+ Compatibility with conda distributions
	+ JSON and YAML file templates added
	+ Three new styles added (Windows10BlackPearl,
	 Windows10BlueWhale, Windows10ClearDay)
	+ Translation improvements
	+ "Always Use Sockets" IDE option is True by default (#938)
* *Issues addressed:*
	+ *#311*, *#941*, *#955*



#### Version 3.6 (January 12, 2019)

* *New features:*
	+ Much faster Remote Engine using asynchronous Windows named
	 pipes if pywin32 is available.
	+ IDE option to force the use of sockets for connection to the
	 Python server now defaults to False
	+ Enhancements to the SSH Engine- now compatible with PuTTY
	+ Execute system commands in the interpreter with ! - supports
	 parameter substitution
	+ Clickable status panels with Python version and engine type
	+ Text drag & drop between PyScripter and other applications
	 (#554)
	+ Triple-click selects line and Quadraple-click selects all
	+ Double-click drag selects whole words - triple-click drag
	 selects whole lines
	+ Consistent syntax color themes accross supported languages
	 (#855)
	+ New IDE option "Trim trailing spaces when saving files" (#667)
	+ New IDE Option 'Step into open files only'. Defaults to False.
	 (#510)
	+ Localization of the installer
* *Issues addressed:*
	+ *#624*, *#743*, *#857*, *#904*, *#922*,
	 *#927*, *#928*, *#929*, *#936*



#### Version 3.5 (November 15, 2018)

* *New features:*
	+ [Work with remote files](remotefiles) from Windows and Linux machines as if they were local
	+ [Run/debug scipts remotely on Windows and Linux servers](pythonengines.md#ssh-engine)
	+ Python 3 type hints used in code completion
	+ Connection to python server with Windows named pipes. Avoids
	 firewall issues. Requires the installation of pywin32 (pip
	 install pywin32).
	+ IDE option to force the use of sockets for connection to the
	 python server. (default True)
	+ New Editor commands Copy Line Up/Down (Shift+Alt+Up/Down) and
	 Move Line Up/Down (Alt + Up/Down) as in Visual Studio
	+ PyScripter icons given a facelift by Salim Saddaquzzaman
	+ Upgraded rpyc to 4.x. As a result Python 2.5 is no longer
	 supported.
* *Issues addressed:*
	+ *#501*, *#682*, *#907*



#### Version 3.4.2 (September 9, 2018)

* *New features:*
	+ New Edit Command Read Only (#883)
	+ Files opened by PyScripter from the Python directory during
	debugging are read only by default to prevent accidental changes.
	+ Close All to the Right Editor command added (#866)
	+ New editor parameter $[-CurLineNumber] (#864)
	+ New IDE Option "File Explorer background processing'. Set to
	false if you get File Explorer errors.
	+ Console output including multiprocessing is now shown in
	interpreter #891

* *Issues addressed:*
	+ *#645, #672, #722, #762, #793, #800, #869, #879, #889, #890,
	#893, #896, #898, #899, #906*


#### Version 3.4 (May 5, 2018)

* *New features:*
	+ Switch Python Engines without exiting PyScripter
	+ Faster loading times
	+ Initial support for running Jupyter notebooks inside
	 PyScripter
	+ Syntax highlighting for JSON files
	+ New IDE option "Style Main Window Border"
	+ Find in Files and ToDo folders can include parameters (#828)
* *Issues addressed:*
	+ *#627, #852, #858, #862, #868, #872*


#### Version 3.3 (March 14, 2018)

* *New features:*
	+ Thread debugging (#455)
	+ Form Layout and placement stored in PyScripter.local.ini
* *Issues addressed:*
	+ *#659, #827, #848, #849*


#### Version 3.2 (January 14, 2018)

* *New features:*
	+ Dpi awareness (Issue 769)
* *Issues addressed:*
	+ *#705, #711, #717, #748*


#### Version 3.1 (December 31, 2017)

* *New features:*
	+ Code folding
	+ Indentation lines
	+ New IDE option "Compact line numbers"
	+ pip tool added
	+ Internal Interpreter is hidden by default
	+ KabyleTranslation added
* *Issues addressed:*
	+ *#16, #571, #685, #690, #718, #721, #765, #814, #836*


#### Version 3.0 (October 17, 2017)

* *New features:*
	+ Python 3.5, 3.6 and 3.7 support
	+ New Style Engine (VCL Styles) with high quality choices
	+ Visual Style Preview and selection (View, Select Style)
	+ Visual Source highlighter theme selection (Editor Options,
	 Select theme)
	+ German Translation added


#### Version 2.6 (March 20, 2015)

* *New features:*
	+ Python 3.4 support added


#### Version 2.5 (March 19, 2012)

* *New features:*
	+ This is the first joint 32-bit and 64-bit version release
	+ Python 3.3 support added
	+ Recent Projects menu item added
	+ Expandable lists and tuples in the Variables window (#583)
	+ Expandable watches as in the Variables window (#523)
	+ Basic support for Cython files added (#542)
	+ New interpreter action Paste & Execute (#500) Replaces
	 Paste with Prompt
	+ New PyIDE option "Display package names in editor tabs"
	 default True (#115)
	+ New search option "Auto Case Sensitive" (case insensitive when
	 search text is lower case)
	+ The Abort command raises a KeyboardInterrupt at the Remote
	 Engine (#618)
	+ Incremental search in the Project Explorer matches any part of
	 a filename (#623)
	+ New IDE option "File line limit for syntax check as you type"
	 default 1000
* *Issues addressed:*
	+ #516, #348, #549, #563, #564, #568, #576, #587, #591, #592,
	 #594, #597, #598, #599, #612, #613, #615


#### Version 2.4.3 (September 20, 2011)

* New features:
	+ 100% portable by placing PyScripter.ini in the PyScripter exe
	 directory
	+ Ctrl+Mousewheel for zooming the interpreter (#475)
	+ Show docstrings during completion list (#274)
	+ New IDE Option "File Change Notification" introduced with
	 possible values Full, NoMappedDrives(default), Disabled (#470)
	+ Background color for Matching and Unbalanced braces (#472)
	+ New IDE option "Case Sensitive Code Completion" (default True)
	+ New IDE option "Complete Python keywords" (default True)
	+ New IDE option "Complete as you type" (default True, #473)
	+ New IDE option "Complete with word-break chars" (default True)
	+ New IDE option "Auto-complete with one entry" (default True,
	 #452)
* *Issues addressed:*
	+ Command line history not saved
	+ Editing a watch to an empty string crashes PyScripter
	+ Replace in Find-in-Files now supports subexpression
	 substitution (#332)
	+ Import statement completion does not include builtin module
	 names
	+ #461, #463, #468, #471, #474, #478, #488, #496, #504, #508,
	 #509, #511, #512, #515, #525, #526, #527, #528, #532, #559, #560


#### Version 2.4.1 (December 12, 2010)

* *New features*:
	+ Side-by-side file editing (#214)
	+ Enhanced regular expression window (findall - #161)
	+ Open file at a specific line:column (#447)
* *Issues addressed:*
	+ Reduced flicker when resizing form and panels
	+ #415, #437, #449


#### Version 2.3.4 (November 25, 2010)

* *New features:*
	+ Compatibility with Python 3.1.3rc, 3.2a4
	+ Add watches by dragging and dropping text
	+ Ctrl + Mouse scroll scrolls whole pages in print preview
	+ Search for custom skins first in the Skins subdirectory of the
	 Exe file if it exists
* *Issues addressed:*
	+ #430, #434, #435, #439, #440, #441, #443, #446


#### Version 2.3.3 (October 16, 2010)

* *New features:*
	+ Native unicode strings throughtout (speed improvements on XP)
	+ Revamped Code Explorer (#192, #163, #213, #225)
	+ Improvements to Code completion
		- Auto-completion for the import statement in python 2.5 and
		 later (#230)
		- Processing of function return statements
		- Background module parsing and caching of parsed modules
	+ Start-up python scripts pyscripter\_init.py and python\_init.py.
	 See help file for details.
	+ Imporved "Match Brace" (#426) and New Editor Command "Select
	 to brace"
	+ Italian translation by Vincenzo Demasi added
	+ Russian translation by Aleksander Dragunkin added
	+ New IDE option "Highlight selected word" (#404)
	+ New IDE option "Use Python colors in IDE"
	+ New Edit command "Copy File Name" available at the contex menu
	 of the tab bar
	+ New commands "Previous Frame", "Next Frame" to change frame
	 using the keyboard (#399)
	+ JavaScript and PHP Syntax Highlighters added
* *Issues addressed:*
	+ #103, #239, #267, #270, #271, #294, #317, #324, #343, #378,
	 #395, #403, #405, #407, #411, #412, #413, #419, #421, #422,
	 #425, #432


#### Version 2.1.1 (August 20, 2010)

* *New features:*
	+ Support for Python 3.2
	+ New IDE Option added "Jump to error on Exception" (#130)
	+ New IDE Option added "File template for new python scirpts"
	 (#385)
	+ New IDE Option added "Auto completion font" (#365)
	+ French translation by Groupe AmiensPython added
* *Bug fixes:*
	+ #297, #307, #346, #354, #358, #371, #375, #376, #382, #384,
	 #387, #389

  
#### Version 2.0 (July 30, 2010)

* *New features:*
	+ Support for Python 2.7
	+ Moved to Rpyc v3.07, now bundled with PyScripter
	+ IDE Option "Reinitialize before run" was added defaulting to
	 True
	+ The default Python engine is now the remote engine
	+ Spanish translation by Javier Pim s (incomplete) was added
* *Bug fixes:*
	+ #236, #304, #322, #333, #334


#### Version 1.9.9.7 (May 20, 2009)

* *New features:*
	+ Updated theme engine with customizable themes
	+ Python 3.1 support
* *Bug fixes:*
	+ #269, #273, #278, #291, #292
  

#### Version 1.9.9.6 (Feb 16, 2009)

* *New features:*
	+ Remote interpreter and Debugger
	+ Python 2.6 and 3.0 support
	+ Project Explorer supporting multiple run configurations with
	 advanced options
	+ New debugger command: Pause
	+ Execute selection command added (Ctrl-F7)
	+ Interpreter command history improvements:
		- Delete duplicates
		- Filter history by typing the first few command characters
		- Up|Down keys at the prompt recall commands from history
	+ Code Explorer shows imported names for (from ... import)
	 syntax (12)
	+ Improved sort order in code completion
	+ Save modified files dialog on exit
	+ Finer control on whether the UTF-8 BOM is written
		- Three file encodings supported (Ansi, UTF-8, UTF-8 without
		 BOM)
	+ IDE option to detect UTF-8 encoding (useful for non-Python
	 files)
	+ IDE options for default linebreaks and encoding for new files
	+ Warning when file encoding results in information loss
	+ IDE option to position the editor tabs at the top
	+ IDE Windows navigation shortcuts
	+ Pretty print intperpreter output option (on by default)
	+ Pyscripter is now Vista ready
	+ Docking window improvements
	+ PYTHONDLLPATH command line option so that Pyscripter can work
	 with unregistered Python
	+ Watches Window: DblClick on empty space adds a watch, pressing
	 Delete deletes (45)
	+ Wrapping in Search & Replace (38)
	+ New IDE Option "Save Environment Before Run" (50)
	+ New IDE command Restore Editor pair to Maximize Editor (both
	 work by double clicking the Tabbar)
	+ New IDE Option "Smart Next Previous Tab" (z-Order) on by
	 default (20)
	+ Word Wrap option exposed in Editor Options
	+ New File Reload command
	+ Import/Export Settings (Shortcuts, Highlighter schemes)
	+ New IDE option "Auto-reload changed files" on by default (25)
	+ New menu command to show/hide the menu bar. The shortcut is
	 Shift-F10 (63)
	+ New command line option --DPIAWARE (-D) to avoid scaling in
	 VISTA high DPI displays (77)
	+ New command line option --NEWINSTANCE (-N) to start a new
	 instance of PyScripter
	+ You can disable a breakpoint by Ctrl+Clicking in the gutter
	+ Syntax Errors are indicated by icon in the tabbar (93)
	+ Command to jump to the first syntax error (Shift+Ctrl+E)
	+ New Firefox-like search/replace interface
	+ Incremental Search (100)
	+ New command "Highlight search text" (Shft+Ctrl+H)
	+ New command line option --DEBUG (-B) to use debug version of
	 Python dll (108)
	+ New command "Word wrap" visible in the Editor toolbar (112)
	+ New command "Go to Debugger Position" (118)
	+ The size of the auto completion list is now persisted
	+ Split Editor View (31)
	+ New parameter $CmdLineArgs that returns the active command
	 line arguments and can be used with external tools
	+ New IDE options "Editor code completion" and "Interpreter code
	 completion" which can be used to disable code completion
	+ New IDE option "Show Tab Close Button"
	+ New debugger command "Post mortem" (26)
	+ New IDE option "Post mortem on exception"
	+ Auto-resizing the fields of list views by double clicking on
	 column separators
	+ Advanced search and replace external tool added (uses re.sub)
	+ Enhanced Execute Selection command (73)
	+ Two new IDE options added (Dock Animation Interval and Dock
	 Animation Move Width - 134)
	+ Toolbar customization
	+ Two new IDE options added ("Interpreter History Size" and
	 "Save Command History") (#131)
	+ Cut and copy without selection now cut and copy the current
	 line (as in Visual Studio, #64)
	+ Removed the Interpeter options "Clean up Namespace" and "Clean
	 up sys.modules"
	+ Improved HTML, XML highlighting with code completion and Web
	 preview
	+ C/C++ highlighting added
	+ Two new interpreter commands added: Copy without prompts, and
	 Paste with prompts (#183)
	+ Localization using gettext (Japanese, Chinese and Greek
	 translations added)
	+ YAML highlighter added
* *Bug fixes*
	+ Shell Integration - Error when opening multiple files
	+ Configure External Run - ParseTraceback not saved properly
	+ Order of tabs not preserved in minimised docked forms
	+ sys.argv contained unicode strings instead of ansi strings
	+ Bug fixes and improvements in Editor Options Keystrokes tab
	 (#6)
	+ Better error handling of File Open and File Save
	+ Page Setup Header and Footer not saved (#7)
	+ Hidden Tabbed windows reappearing when restarting
	+ Duplicate two-key editor command not detected
	+ "Clean up namespace" and "Clean up sys modules" settings
	 become effective after restarting PyScripter
	+ Exception when setting the Active Line Color in Editor Options
	 dialog
	+ Raw\_input does not accept unicode strings
	+ Error in docstring extraction (#11)
	+ Fixed some problems with the toggle comment command
	+ Fixed rare bug in restoring layout
	+ Code tips wrong if comments are present among parameters (#15)
	+ Notification of file changes can miss files (#17)
	+ Certain syntax coloring options were not saved
	+ ToDo List did not support encoded files and unicode
	+ ToDo List did not support multiline comments (#14)
	+ Fixed bug in IDE Shortcuts dialog
	+ Swapped the positions of the indent/dedent buttons (#23)
	+ Syntax highlighter changes to the interpreter are not
	 persisted
	+ Multiple target assignments are now parsed correctly
	+ Gutter gradient setting not saved
	+ Disabling a breakpoint had no effect
	+ Tab order not preserved when restarting PyScripter
	+ Disassembly and Documentation views not working with remote
	 engines
	+ More robust "Reinitialize" of remote Python engines (Issues
	 143, 145)
	+ Shift-Tab does not work well with the Trim Trailing Spaces
	 editor option
	+ #28, #32, #39, #40, #41, #46, #47, #48, #49, #52, #55, #56,
	 #57, #65, #66, #67, #70, #71, #72, #74, #75, #76, #81, #82, #83,
	 #86, #88, #90, #91, #92, #94, #96, #98, #99, #100, #102, #105,
	 #106, #107, #109, #113, #117, #119, #120, #120, #122, #123,
	 #125, #132, #134, #135, #136, #137, #138, #139, #140, #141,
	 #146, #147, #150, #153, #155, #160, #164, #165, #166, #167,
	 #168, #169, #171, #174, #178, #182, #186, #193, #195, #196,
	 #197, #198, #201, #202, #204, #206, #208, #212, #219, #226,
	 #228, #229, #234, #235, #237, #253, #261



#### Version 1.7.2 (Oct 26, 2006)

* *New features:*
	+ Store toolbar positions
	+ Improved bracket completion now also works with strings (#4)
* *Bug fixes:*
	+ Bracket highlighting with non default background
	+ Opening wrongly encoded UTF8 files results in empty module
	+ File Format (Line End) choice not respected
	+ Initial empty module was not syntax highlighted
	+ Save As dialog had no default extension set
	+ Unit Testing broken (regression)
	+ Gap in the default tool bar (#3)


#### Version 1.7.1 (Oct 15, 2006)

* *New features:*
	+ Repeat scrolling of editor tabs
	+ Massively improved start up time
	+ Faster Python source file scanning
* *Bug fixes:*
	+ Infinite loop with cyclical Python imports


#### Version 1.7 (Oct 14, 2006)

* *New features:*
	+ Unicode based editor and interactive interpreter
	+ Full support for Python source file encodings
	+ Support for Python version 2.5 and Current User installations
	+ Check syntax as you type and syntax hints (IDE option)
	+ Tab indents and Shift-Tab unindents (Editor Options - Tab
	 Indents)
	+ Editor Zoom in/out with keyboard Alt+- and Ctrl+mouse wheel
	+ Improved Debugger hints and completion in the interpreter
	+ work with expressions e.g. sys.path1.
	+ for debugger expression hints place the cursor on ')' or ']'
	+ Improved activation of code/debugger hints
	+ IDE options to Clean up Interpreter namespace and sys.modules
	 after run
	+ File Open can open multiple files
	+ Syntax highlighting scheme selection from the menu
	+ File filters for HTML, XML and CSS files can be customized
	+ Option to disable gutter Gradient (Editor Options - Gutter
	 Gradient)
	+ Option to disable theming of text selection (Editor Options -
	 theme selection)
	+ Option to hide the executable line marks
	+ Active Line Color Editor option added. Set to None to use
	 default background
	+ Files submenu in Tabs popup for easy open file selection
	+ Add Watch at Cursor added to the Run menu and the Waches
	 Window popup menu
	+ Pop up menu added to the External Process indicator to allow
	 easy termination of such processes
	+ If the Ini file exists in PyScripter directory it is used in
	 preference to the User Directory in order to allow USB storage
	 installations
	+ Editor options for each open file are persisted
	+ Auto close brackets in the editor
	+ Improved speed of painting the Interpreter window
	+ Interactive Interpreter Pop up menu with separately persisted
	 Editor Options
	+ Toggle comment (Ctrl+^) in addition to comment/uncomment
	+ File Explorer improvements (Favourites, Create New Folder)
	+ File Templates
	+ Windows Explorer file association (installation and IDE
	 option)
	+ Command line history
	+ Color coding of new and changed variables in the Variables
	 Window
* *Bug fixes:*
	+ Gutter glyphs painted when gutter is invisible
	+ Sticky bracket highlighting in the interpreter window
	+ Selecting lines by dragging mouse in the gutter sets
	 breakpoint
	+ Speed improvements and bugfixes related to layouts
	+ Error in Variable Windows when showing dictionaries with non
	 string keys
	+ File notification error for Novel network disks
	+ Wrong line number in External Run traceback message
	+ No horizontal scroll in output window
	+ Code completion Error with packages containing module with the
	 same name
	+ Problem with sys.stdin.readline() and partial line output
	 (stdout) statements
	+ Infinite loop when root of package is the top directory of a
	 drive


#### Version 1.5.1 (Mar 14, 2006)

* *New features:*
	+ Unit test integration (Automatic generation of tests, and
	 testing GUI)
	+ Added highlighting of HTML, XML and CSS files
	+ Command line parameters for scripts run internally or debugged
	+ IDE shortcut customization
	+ Conditional breakpoints
	+ Persistence of breakpoints, watches, bookmarks and file
	 positions
	+ Save and restore IDE windows layouts
	+ Generate stack information when untrapped exceptions occur and
	 give users the option to mail the generated report
	+ Running scripts does not polute the namespace of PyScripter
	+ Names in variables window are now sorted
	+ Allow only a single Instance of Pyscripter and open command
	 line files of additional invocations at new tabs
	+ Interpreter window is now searchable
	+ Added option to File Explorer to browse the directory of the
	 Active script
	+ New distinctive application icon thanks to Frank Mersmann and
	 and Tobias Hartwich
	+ File Explorer autorefreshes
	+ Improved bracket highlighting
	+ User customization (PyScripter.ini) is now stored in the
	 user's Application Data direcrory to support network
	 installations(breaking change). To restore old settings copy the
	 ini file to the new location.
* *Bug fixes:*
	+ Resolved problems with dropping files from File Explorer
	+ Restore open files options not taken into account
	+ Resolved problems with long Environment variables in Tools
	 Configure
	+ Resolved problems with help files
	+ Reduced problems with running wxPython scripts
	+ Changing the Python Open dialog filter did not affect syntax
	 highlighting
	+ CodeExplorer slow when InitiallyExpanded is set
	+ Help related issues
	+ Other fixes



#### Version 1.3 (Dec 18, 2005)

* *New features:*
	+ Code completion in the editor (Press Ctrl+Space while or
	 before typing a name)
	+ Parameter completion in the editor (Press Shift+Ctrl+Space)
	+ Find definition and find references independent of
	 BicycleRepairMan much faster and arguably better
	+ Find definition by clicking works for imported modules and
	 names
	+ A new feature-rich Python code parser was developed for
	 implementing the above
	+ Improved the Variable Window (shows interpreter globals when
	 not debugging and Doc strings)
	+ Improved code and parameter completion in the interactive
	 interpreter
	+ Integrated regular expression tester
	+ Code and debugger hints
	+ Set the current directory to the path of the running script
	+ Added IDE option MaskFUPExceptions for resolving problems in
	 importing Scipy
	+ Tested with FastMM4 for memory leaks etc. and fixed a couple
	 of related bugs
	+ Bug fixes and other improvements


#### Version 1.2 (Aug 28, 2005)

* *New features:*
	+ Extended code editor:
		- Context sensitive help on Python keywords
		- Parameterized Code Templates (Ctrl-J)
		- Accept files dropped from Explorer
		- File change notification
		- Detecting loading/saving UTF-8 encoded files
		- Converting line breaks (Windows, Unix, Mac)
	+ Editor Views
		- Disassembly
		- HTML Documentation (pydoc)
		- To Do List
		- Find and Replace in Files
		- Parameterized Code Templates
		- Choice of Python version to run via command line
		 parameters
		- Run Python Script externally (highly configurable)
		- External Tools (External run and capture output)
		- Integration with Python tools such as PyLint, TabNanny,
		 Profile etc.
		- Powerful parameter functionality for external tool
		 integration
		- Find Procedure
		- Find Definition/Find references using BicycleRepairMan
		- Find definition by clicking and browsing history
		- Modern GUI with docked forms and configurable
		 look&feel (themes)


#### Version 1.0 (Apr 13, 2005)

* Initial release


 



