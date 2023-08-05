{-----------------------------------------------------------------------------
 Unit Name: frmPyIDEMain
 Author:    Kiriakos Vlahos
 Date:      11-Feb-2005
 Purpose:   The main form of the Pytnon IDE

 History:   v 1.1
            Improved Python Syntax highlighting
            HTML documentation and disassembly views (Tools, Source Views menu)
            TODO list view
            Find and Replace in Files
            Powerful parameter functionality (see parameters.txt)
            Parameterized Code Templates (Ctrl-J)
            Accept files dropped from Explorer
            File change notification
            sys.stdin and raw_input implemented
            Choice of Python version to run via command line parameters
            External Tools (External run and caputure output)
            Integration with Python tools such as PyLint
            Run Python Script externally (highly configurable)
            Persist and optionally reopen open files
            Bug fixes
 History:   v 1.2
            Updated the User Interface using Themes
            Messages History
            Previous/Next identifier reference (as in GExperts)
            Find Definition/Find references using BicycleRepairMan
            Find definition by clicking as in Delphi
            Reduced flicker on start and exit and somewhat on resizing**
            Converting line breaks (Windows, Unix, Mac)
            Detecting loading/saving UTF-8 encoded files
            Help file and context sensitive Help
            Check for updates

 History:   v 1.3
            Code completion in the editor (Press Ctrl+Space while or before typing a name)
            Parameter completion in the editor (Press Shift+Ctrl+Space)
            Find definition and find references independent of
              BicycleRepairMan and arguably faster and better
            Find definition by clicking works for imported modules and names
            Revamped Code Explorer Window featuring incremental search, properties,
              global variables, docstrings in hints etc.
            A new feature-rich Python code parser was developed for implementing the above
            Improved the Variable Windows
              shows interpreter globals when not debugging and Doc strings
            Improved code and parameter completion in the interactive interpreter
            Integrated regular expression tester
            Code and debugger hints
            Set the current directory to the path of the running script
            Added IDE option MaskFUPExceptions for resolving problems in importing Scipy
            Tested with FastMM4 for memory leaks etc. and fixed a couple of related bugs

            Note on Code and Parameter completion:
              The code and parameter completion should be one of the best you can
              find in any Python IDE.  However,if you find that code and parameter
              completion is not very accurate for certain modules and packages
              such as wxPython and scipy you can achieve near perfect completion
              if you add these packages to the IDE option "Special Packages"
              (comma separated text). By default it is set to "wx, scipy". Special
              packages are imported to the interpreter instead of scanning their
              source code.

 History:   v 1.5
          New Features
            Unit test integration (Automatic generation of tests, and testing GUI)
            Added highlighting of HTML, XML and CSS files
            Command line parameters for scripts run internally or debugged
            Conditional breakpoints
            Persistence of breakpoints, watches, bookmarks and file positions
            Save and restore IDE windows layouts
            Generate stack information when untrapped exceptions occur and give
              users the option to mail the generated report
            Running scripts does not polute the namespace of PyScripter
            Names in variables window are now sorted
            Allow only a single Instance of Pyscripter and open command line
              files of additional invocations at new tabs
            Interpreter window is now searchable
            Added option to File Explorer to browse the directory of the Active script
            New distinctive application icon thanks to Frank Mersmann and Tobias Hartwich
            IDE shortcut customization
            File Explorer autorefreshes
            Improved bracket highlighting
            Copy to Clipboard Breakpoins, Watches and Messages
            User customization (PyScripter.ini) is now stored in the user's
              Application Data direcrory to support network installations(breaking change)
              To restore old settings copy the ini file to the new location.
            Bug fixes
              Resolved problems with dropping files from File Explorer
              Restore open files options not taken into account
              Resolved problems with long Environment variables in Tools Configure
              Resolved problems with help files
              Reduced problems with running wxPython scripts
              Changing the Python Open dialog filter did not affect syntax highlighting
              CodeExplorer slow when InitiallyExpanded is set
              Help related issues
              Other fixes.

 History:   v 1.7.1
          New Features
            Unicode based editor and interactive interpreter
            Full support for Python source file encodings
            Support for Python v2.5 and Current User installations
            Check syntax as you type and syntax hints (IDE option)
            Tab indents and Shift-Tab unindents (Editor Options - Tab Indents)
            Editor Zoom in/out with keyboard Alt+- and Ctrl+mouse wheel
            Improved Debugger hints and completion in the interpreter
               work with expressions e.g. sys.path[1].
               for debugger expression hints place the cursor on ')' or ']'
            Improved activation of code/debugger hints
            IDE options to Clean up Interpreter namespace and sys.modules after run
            File Open can open multiple files
            Syntax highlighting scheme selection from the menu
            File filters for HTML, XML and CSS files can be customized
            Option to disable gutter Gradient (Editor Options - Gutter Gradient)
            Option to disable theming of text selection (Editor Options - theme selection)
            Option to hide the executable line marks.
            Active Line Color Editor option added.  Set to None to use default background
            Files submenu in Tabs popup for easy open file selection
            Add Watch at Cursor added to the Run menu and the Waches Window popup menu
            Pop up menu added to the External Process indicator to allow easy termination of such processes
            If the PyScripter.ini file exists in PyScripter directory it is used in preference to the User Directory
              in order to allow USB storage installations
            Editor options for each open file are persisted
            Improved speed of painting the Interpreter window
            Auto close brackets
            Interactive Interpreter Pop up menu with separately persisted Editor Options
            Toggle comment (Ctrl+^) in addition to comment/uncomment
            File Explorer improvements (Favorites, Create New Folder)
            File Templates
            Windows Explorer file association (installation and IDE option)
            Command line history
            Color coding of new and changed variables in the Variables Window
            Repeat scrolling of editor tabs
            Massively improved start up time
            Faster Python source file scanning
          Bug fixes
            Gutter glyphs painted when gutter is invisible
            Bracket highlighting related bugs
            Selecting whole lines by dragging mouse in the gutter sets breakpoint
            Speed improvements and bugfixes related to layouts
            Error in Variable Windows when showing dictionaries with non string keys
            File notification error for Novell network disks
            Wrong line number in External Run traceback message
            No horizontal scroll in output window
            Code completion Error with packages containing module with the same name
            Problems with sys.stdin.readline() and partial line output (stdout) statements
            Infinite loop when root of package is the top directory of a drive
            Infinite loop with cyclical Python imports

 History:   v 1.7.2
          New Features
            Store toolbar positions
            Improved bracket completion now also works with strings (Issue #4)
          Bug fixes
            Bracket highlighting with non default background
            Opening wrongly encoded UTF8 files results in empty module
            File Format (Line End) choice not respected
            Initial Empty module was not syntax highlighted
            Save As dialog had no default extension set
            Unit Testing broken (regression)
            Gap in the default tool bar (Issue #3)

 History:   v 1.9.9.6
          New Features
            Remote interpreter and debugger
            Python 2.6, 3.0 and 3.1 support
            Project Explorer supporting multiple run configurations with advanced options
            New debugger command: Pause
            Execute selection command added (Ctrl-F7)
            Interpreter command history improvements:
              - Delete duplicates
              - Filter history by typing the first few command characters
              - Up|Down keys at the prompt recall commands from history
            Code Explorer shows imported names for (from ... import) syntax (Issue 12)
            Improved sort order in code completion
            Save modified files dialog on exit
            Finer control on whether the UTF-8 BOM is written
              - Three file encodings supported (ANSI, UTF-8, UTF-8 without BOM)
            IDE option to detect UTF-8 encoding (useful for non-Python files)
            IDE options for default line breaks and encoding for new files
            Warning when file encoding results in information loss
            IDE option to position the editor tabs at the top
            IDE window navigation shortcuts
            Pretty print interpreter output option (on by default)
            Pyscripter is now Vista ready
            Docking window improvements
            PYTHONDLLPATH command line option so that Pyscripter can work with unregistered Python
            Watches Window: DblClick on empty space adds a watch, pressing Delete deletes (Issue 45)
            Wrapping in Search & Replace (Issue 38)
            New IDE Option "Save Environment Before Run"  (Issue 50)
            New IDE command Restore Editor pair to Maximize Editor (both work by double clicking  the TabControl)
            New IDE Option "Smart Next Previous Tab" (z-Order) on by default (Issue 20)
            Word Wrap option exposed in Editor Options
            New File Reload command
            Import/Export Settings (Shortcuts, Highlighter schemes)
            New IDE option "Auto-reload changed files" on by default (Issue 25)
            New menu command to show/hide the menu bar.  The shortcut is Shift-F10 (Issue 63)
            New command line option --DPIAWARE (-D) to avoid scaling in VISTA high DPI displays (Issue 77)
            New command line option --NEWINSTANCE (-N) to start a new instance of PyScripter
            You can disable a breakpoint by Ctrl+Clicking in the gutter
            Syntax Errors are indicated by icon in the TabControl (Issue 93)
            Command to jump to the first syntax error (Shift+Ctrl+E)
            New Firefox-like search/replace interface
            Incremental Search (Issue 100)
            New command "Highlight search text" (Shift+Ctrl+H)
            New command line option --DEBUG (-B) to use debug version of Python dll (Issue 108)
            New command "Word wrap" visible in the Editor toolbar (Issue 112)
            New command "Go to Debugger Position" (Issue 118)
            The size of the auto completion list is now persisted
            Split Editor View (Issue 31)
            New parameter $[CmdLineArgs] that returns the active command line arguments
              and can be used with external tools
            New IDE options "Editor code completion" and "Interpreter code completion"
              which can be used to disable code completion
            New IDE option "Show Tab Close Button"
            New debugger command "Post mortem" (Issue 26)
            New IDE option "Post mortem on exception"
            Auto-resizing the fields of list views by double clicking on column separators
            Advanced search and replace external tool added (uses re.sub)
            Enhanced Execute Selection command (Issue 73)
            Two new IDE options added (Dock Animation Interval and Dock Animation Move Width - Issue 134)
            Toolbar customization
            Two new IDE options added ("Interpreter History Size" and "Save Command History") (Issue 131)
            Cut and copy without selection now cut and copy the current line (as in Visual Studio, Issue 64)
            Removed the Interpeter options "Clean up Namespace" and "Clean up sys.modules"
            Improved HTML, XML highlighting with code completion and Web preview
            C/C++ highlighting added
            Two new interpreter commands added: Copy without prompts, and Paste with prompts (Issue 183)
            Localization using gettext
            YAML highlighter added
            Ability to run initialization scripts (see help file)
          Bug fixes
            Shell Integration - Error when opening multiple files
            Configure External Run - ParseTraceback not saved properly
            Order of tabs not preserved in minimised docked forms
            sys.argv contained unicode strings instead of ansi strings
            Bug fixes and improvements in Editor Options Keystrokes tab (Issue 6)
            Better error handling of File Open and File Save
            Page Setup Header and Footer not saved  (Issue 7)
            Hidden Tabbed windows reappearing when restarting
            Duplicate two-key editor command not detected
            "Clean up namespace" and "Clean up sys modules" settings
              become effective after restarting PyScripter
            Exception when setting the Active Line Color in Editor Options dialog
            Raw_input does not accept unicode strings
            Error in docstring extraction (Issue 11)
            Fixed some problems with the toggle comment command
            Fixed rare bug in restoring layout
            Code tips wrong if comments are present among parameters (Issue 15)
            Notification of file changes can miss files (Issue 17)
            Certain syntax coloring options were not saved
            ToDo List did not support encoded files and unicode
            ToDo List did not support multiline comments (Issue 14)
            Fixed bug in IDE Shortcuts dialog
            Swapped the positions of the indent/dedent buttons (Issue 23)
            Syntax highlighter changes to the interpreter are not persisted
            Multiple target assignments are now parsed correctly
            Gutter gradient setting not saved
            Handling of string exceptions
            Disabling a breakpoint had no effect
            Tab order not preserved when restarting PyScripter
            Disassembly and Documentation views not working with remote engines
            PyScripter "freezes" when displaying modal dialogs when running GUI scripts with remote engines
            More robust "Reinitialize" of remote Python engines (Issues 143, 145)
            Shift-Tab does not work well with the Trim Trailing Spaces editor option
            Issues 28, 32, 39, 40, 41, 46, 47, 48, 49, 52, 55, 56, 57, 65, 66, 67, 70,
                   71, 72, 74, 75, 76, 81, 82, 83, 86, 88, (89), 90, 91, 92, 94, 96, 98, 99
                   100, 102, 105, 106, 107, 109, 113, 117, 119, 120, 122, 123, 125,
                   132, 134, 135, 136, 137, 138, 139, 140, 141, 146, 147, 150, 153, 155,
                   160, 164, 165, 166, 167, 168, 169, 171, 174, 178, (182), 186,
                   193, 195, 196, 197, 198, 201, 202, 204, 206, 208, 212, 219, 226,
                   228, 229, 234, 235, 237, 253, 261 fixed

 History:   v 1.9.9.7
          New Features
            Updated theme engine with customizable themes
            Python 3.1 support
          Bug fixes
            Issues  269, 273, 287, 291, 292

 History:   v 2.0
          New Features
            Support for Python 2.7
            Moved to Rpyc v3.07, now bundled with PyScripter
            IDE Option "Reinitialize before run" was added defaulting to True
            The default Python engine is now the remote engine
            Spanish translation by Javier Pim�s (incomplete) was added
          Bug fixes
            Issues  236, 304, 322, 333, 334
 History:   v 2.1.1
          New Features
            Support for Python 3.2
            New IDE Option added "Jump to error on Exception"  (Issue 130)
            New IDE Option added "File template for new Python scirpts"  (Issue 385)
            New IDE Option added "Auto completion font"  (Issue 365)
            French translation by Groupe AmiensPython added
          Bug fixes
            Issues  297, 307, 346, 354, 358, 371, 375, 376, 382, 384, 387, 389

  History:   v 2.3.3
          New Features
            Native unicode strings throught (speed improvements on XP)
            Revamped Code Explorer (Issues 192, 163, 213, 225)
            Improvements to Code completion
            -  Auto-completion for the import statement in Python 2.5 and later (Issue 230)
            -  Processing of function return statements
            -  Background module parsing and caching of parsed modules
            Start-up Python scripts pyscripter_init.py and python_init.py. See help file for details.
            Imporved "Match Brace" (Issue 426) and New Editor Command "Select to brace"
            Italian translation by Vincenzo Demasi added
            Russian translation by Aleksander Dragunkin added
            New IDE option "Highlight selected word" (Issue 404)
            New IDE option "Use Python colors in IDE"
            New Edit command "Copy File Name" available at the contex menu of the tab bar
            New commands "Previous Frame", "Next Frame" to change frame using the keyboard (Issue 399)
            JavaScript and PHP Syntax Highlighters added
          Issues addressed
             103, 239, 267, 270, 271, 294, 317, 324, 343, 378,
             395, 403, 405, 407, 411, 412, 413, 419, 421, 422,
             425, 432

  History:   v 2.3.4
          New Features
            Compatibility with Python 3.1.3rc, 3.2a4
            Add watches by dragging and dropping text
            Ctrl + Mouse scroll scrolls whole pages in print preview
            Search for custom skins first in the Skins subdirectory of the Exe file if it exists
          Issues addressed
              430, 434, 435, 439, 440, 441, 443, 446

  History:   v 2.4.1
          New Features
            Side-by-side file editing (Issue 214)
            Enhanced regular expression window (findall - Issue 161)
            Open file at a specific line:column (Issue 447)
          Issues addressed
            Reduced flicker when resizing form and panels
            415, 437, 449

  History:   v 2.4.3
          New Features
            100% portable by placing PyScripter.ini in the PyScripter exe directory
            Ctrl+Mousewheel for zooming the interpreter (Issue 475)
            Show docstrings during completion list (Issue 274)
            New IDE Option "File Change Notification" introduced with possible values Full, NoMappedDrives(default), Disabled (Issue 470)
            Background color for Matching and Unbalanced braces (Issue 472)
            New IDE option "Case Sensitive Code Completion" (default True)
            New IDE option "Complete Python keywords" (default True)
            New IDE option "Complete as you type" (default True, Issue 473)
            New IDE option "Complete with word-break chars" (default True)
            New IDE option "Auto-complete with one entry" (default True, Issue 452)
          Issues addressed
            Command line history not saved
            Editing a watch to an empty string crashes PyScripter
            Replace in Find-in-Files now supports subexpression substitution (Issue 332)
            Import statement completion does not include builtin module names
            461, 463, 468, 471, 474, 478, 488, 496, 504, 508,
            509, 511, 512, 515, 525, 526, 527, 528, 532, 559, 560

  History:   v 2.5.1
          New Features
            This is the first joint 32-bit and 64-bit version release
            Python 3.3 support added
            Recent Projects menu item added
            Expandable lists and tuples in the Variables window (Issue 583)
            Expandable watches as in the Variables window (Issue 523)
            Basic support for Cython files added (Issue 542)
            New interpreter action Paste & Execute (Issue 500) Replaces Paste with Prompt
            New PyIDE option "Display package names in editor tabs" default True (Issue 115)
            New search option "Auto Case Sensitive" (case insensitive when search text is lower case)
            The Abort command raises a KeyboardInterrupt at the Remote Engine (Issue 618)
            Incremental search in the Project Explorer matches any part of a filename (Issue 623)
            New IDE option "File line limit for syntax check as you type" default 1000
          Issues addressed
            516, 348, 549, 563, 564, 568, 576, 587, 591, 592,
            594, 597, 598, 599, 612, 613, 615

  History:   v 2.5.1
          New Features
          Issues addressed
            639, 657, 673
  History:   v 2.6
          New Features
            Compatibility with Python 3.4
  History:   v 3.0
          New Features
            Python 3.5, 3.6 and 3.7 support
            New Style Engine (VCL Styles) with high quality choices
            Visual Style Preview and selection (View, Select Style)
            Visual Source highligther theme selection (Editor Options, Select theme)
            German Translation added

  History:   v 3.1
          New Features
            Code folding
            Indentation lines
            New IDE option "Compact line numbers"
            pip tool added
            Internal Interpreter is hidden by default
            Kabyle language added
          Issues addressed
            16, 571, 685, 690, 718, 721, 765, 814, 836

  History:   v 3.2
          New Features
            Dpi awareness (Issue 769)
          Issues addressed
            #705 #711 #717, #748

  History:   v 3.3
          New Features
            Thread debugging (#455)
            Much faster Python output redirection
            Form Layout and placement stored in PyScripter.local.ini
          Issues addressed
            #659, #827, #848, #849

  History:   v 3.4
          New Features
            Switch Python Engines without exiting PyScripter
            Faster loading times
            Initial support for running Jupyter notebooks inside PyScripter
            Syntax highlighting for JSON files
            New IDE option "Style Main Window Border"
            Find in Files and ToDo folders can include parameters (#828)
          Issues addressed
            #627, #852, #858, #862, #868, #872

  History:   v 3.4.2
          New Features
            New Edit Command Read Only (#883)
            Files opened by PyScripter from the Python directory during debugging
              are read only by default to prevent accidental changes.
            Close All to the Right Editor command added (#866)
            New editor parameter [$-CurLineNumber] (#864)
            New IDE Option "File Explorer background processing'. Set to false
              if you get File Explorer errors
            Console output including multiprocessing is now shown in interpreter #891
          Issues addressed
            #645, #672, #722, #762, #793, #800, #869, #879, #889, #890,
            #893, #896, #898, #899, #906

  History:   v 3.5
          New Features
            Open and work with remote files from Windows and Linux machines as if they were local
            Run and Debug scripts on remote Windows and Linux machines using SSH
            Python 3 type hints used in code completion
            Connection to Python server with Windows named pipes. Avoids firewall issues.
              Requires the installation of pywin32 (pip install pywin32).
            IDE option to force the use of sockets for connection to the Python server.  (default True)
            New Editor commands Copy Line Up/Down (Shift+Alt+Up/Down) and
              Move Line Up/Down (Alt + Up/Down) as in Visual Studio
            PyScripter icons given a facelift by Salim Saddaquzzaman
            Upgraded rpyc to 4.x.  As a result Python 2.5 is no longer supported.
          Issues addressed
            #501, #682, #907

  History:   v 3.6
          New Features
            Much faster Remote Engine using asynchronous Windows named pipes if pywin32 is available.
            IDE option to force the use of sockets for connection to the Python
              server now defaults to False
            Enhancements to the SSH Engine - now compatible with PuTTY
            Execute system commands in the interpreter with !. Supports parameter substitution.
            Clickable status panels with Python version and engine type
            Text drag & drop between PyScripter and other applications (#554)
            Triple-click selects line and Quadraple-click selects all
            Double-click drag selects whole words - triple-click drag selects whole lines
            Consistent syntax color themes accross supported languages (#855)
            New IDE option "Trim trailing spaces when saving files" (#667)
            New IDE Option 'Step into open files only'.  Defaults to False. (#510)
            Localization of the installer
          Issues addressed
            #624, #743, #857, #904, #922, #927, 928, #929, #936

  History:   v 3.6.1
          New Features
            Python 3.8 support.  Dropped support for Python 3.0 and 3.1.
            Compatibility with conda distributions
            JSON and YAML file templates added
            Three new styles added (Windows10BlackPearl, Windows10BlueWhale, Windows10ClearDay)
            Translation improvements
            "Always Use Sockets" IDE option is True by default (#938)
          Issues addressed
            #311, #941, #955

  History:   v 3.6.2
          New Features
            Improved compatibility with venv virtual environments
            Restore code folding state when you start PyScripter (#973)
            Syntax for adding and removing parameters (#971)
              $[proc=?Question] adds parameter proc and $[proc=] removes it
            Highlighters and styles are now installed under ProgramData
            Improved DPI scaling
            Two new styles added (Calypso and Stellar)
          Issues addressed
            #948, #962, #966, #967, #968, #972

  History:   v 3.6.3
          New Features
            The status panel with text position info can now be clicked to
            show the "Go to line" dialog.
          Issues addressed
            #983, #985

  History:   v 3.6.4
          New Features
            Added support for Python 3.9 (and removed support for Python 2.6)
            Added support for virtualenv v20+.  Dropped support for earlier versions.
            Added support for font ligatures
          Issues addressed
            #998, #1001, #1003, #1008, #1009

  History:   v 4.0
          New Features
            Major redesign of the User Interface - Material icons and new logo
            Re-architecture the interaction with python, code-completion etc.
              It should result in a more responsive user experience without delays and freezes.
            Added support for Python 3.10
            Removed support for Python 2.7, 3.2
            Installer and executable are now code-signed
            Persian translation added
            New IDE option "Restore open project"
            New File Explorer command "Select Directory..." (#1034)
          Issues addressed
            #824, #990, #1031, #1035, #1038, #1039, #1040, #1105, #1109, #1111

  History:   v 4.1
          New Features
            - Implementation of the Language Server Protocol
            - Python language support provided by the Jedi language server
            - Two new styles added Windows11_Light and Windows11_Dark
            - Copy and paste code as html to Powerpoint and other applications
            - Removed support for python 3.3-3.5
            - Read only indicator on tabs
            - Added traditional Chinese translation
          Issues addressed
            #939, #951, #1116, #1118, #1119, #1122, #1123, #1125, #1129, #1133
            #1136

  History:   v 4.2
          New Features
            - Python 3.11 support added - Support for python 3.6 removed
            - Spell checking of comments and strings #84
            - Track changes bar as in Visual Studio
            - Editor Unicode handling improvements (emojis, bi-directional text, etc.)
            - Editor selection options (alpha blending, fill whole lines)
            - Portuguese translations (pt_PT, pt_BR) added
          Issues addressed
            #1140, #1146, #1149, #1151, #1163, #1165

  History:   v 4.2.2
          New Features
            - Internet Explorer replaced with the Edge browser
            - Added Format Selection external tool using the "black" module
            - New IDE option 'Automatic Restart' (#1188)
            - Recovery of unsaved files on system shutdown or application crash
            - New IDE command "Zoom Reset" Atl+Num 0 (#650)
            - Two new styles added: Windows 11 Polar Dark and Windows 11 Polar Light
          Issues addressed
            #1152, #1155, #1177, #1181, #1182, #1183, #1185, #1186, #1187, #1189

  History:   v 4.2.8
          New Features
            - Python 3.12 support added
            - Improved multi-monitor display (per monitor DPI awareness)
            - Customizable user interface content font size (#1209)
            - Screen reader support in the editor
          Issues addressed
            #1172, #1195, #1197, #1198, #1199, #1200, #1208, #1210, #1214

}

{ TODO : Review Search and Replace }
{ TODO : Auto PEP8 tool }
{ TODO: LiveTemplates features for Code Templates }


{------------------------------------------------------------------------------}

// Bugs and minor features
// TODO: Internal Tool as in pywin
// TODO: Find module expert
// TODO: Code helpers, automatically fill the self parameter in methods

// TODO: UML Editor View
// TODO: Refactorings using rope

// TODO: Plugin architecture
// TODO Package as an Application Scripter Component

unit frmPyIDEMain;

interface

uses
  WinAPI.Windows,
  WinAPI.Messages,
  WinApi.ActiveX,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Actions,
  System.Variants,
  System.ImageList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ImgList,
  Vcl.ActnList,
  Vcl.Menus,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  VCL.Styles,
  Vcl.VirtualImageList,
  Vcl.BaseImageCollection,
  SVGIconImageCollection,
  JclSysUtils,
  JvComponentBase,
  JvAppInst,
  JvDockControlForm,
  JvDockVSNetStyle,
  JvAppStorage,
  JvAppIniStorage,
  JvDSADialogs,
  TB2Dock,
  TB2Toolbar,
  TB2Item,
  TB2ExtItems,
  SpTBXCustomizer,
  SpTbxSkins,
  SpTBXItem,
  SpTBXEditors,
  SpTBXMDIMRU,
  SpTBXTabs,
  SpTBXDkPanels,
  SynEditTypes,
  SynEditMiscClasses,
  SynEdit,
  dmResources,
  dmCommands,
  dlgCustomShortcuts,
  uEditAppIntfs,
  uHighlighterProcs,
  cFileTemplates,
  cPySupportTypes,
  cPyBaseDebugger,
  cPyDebugger,
  cPyScripterSettings,
  cPyControl;

const
  WM_FINDDEFINITION  = WM_USER + 100;
  WM_SEARCHREPLACEACTION  = WM_USER + 130;

type
  { Trick to add functionality to TTSpTBXTabControl}
  TSpTBXTabControl = class(SpTBXTabs.TSPTBXTabControl)
  private
    zOrderPos : integer;
    zOrderProcessing : Boolean;
  public
    zOrder : TList;
    procedure WMDropFiles(var Msg: TMessage); message WM_DROPFILES;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TPyIDEMainForm = class(TForm, IIDELayouts, IPyIDEServices)
    DockServer: TJvDockServer;
    AppStorage: TJvAppIniFileStorage;
    BGPanel: TPanel;
    TBXDockTop: TSpTBXDock;
    MainMenu: TSpTBXToolbar;
    FileMenu: TSpTBXSubmenuItem;
    mnNewModule: TSpTBXItem;
    mnFileOpen: TSpTBXItem;
    N14: TSpTBXSeparatorItem;
    mnFileClose: TSpTBXItem;
    mnFileCloseAll: TSpTBXItem;
    N1: TSpTBXSeparatorItem;
    mnFileSave: TSpTBXItem;
    mnFileSaveAs: TSpTBXItem;
    mnFileSaveAll: TSpTBXItem;
    N2: TSpTBXSeparatorItem;
    PageSetup1: TSpTBXItem;
    PrinterSetup1: TSpTBXItem;
    PrintPreview1: TSpTBXItem;
    Print1: TSpTBXItem;
    N4: TSpTBXSeparatorItem;
    N3: TSpTBXItem;
    EditMenu: TSpTBXSubmenuItem;
    mnEditUndo: TSpTBXItem;
    mnEditRedo: TSpTBXItem;
    N5: TSpTBXSeparatorItem;
    mnEditCut: TSpTBXItem;
    mnEditCopy: TSpTBXItem;
    mnEditPaste: TSpTBXItem;
    mnEditDelete: TSpTBXItem;
    mnEditSelectAll: TSpTBXItem;
    N6: TSpTBXSeparatorItem;
    Parameters1: TSpTBXSubmenuItem;
    mnInsertParameter: TSpTBXItem;
    mnInsertModifier: TSpTBXItem;
    N16: TSpTBXSeparatorItem;
    mnReplaceParameter: TSpTBXItem;
    mnIsertCodeTemplate: TSpTBXItem;
    mnSourceCode: TSpTBXSubmenuItem;
    mnIndentBlock: TSpTBXItem;
    mnDedentBlock: TSpTBXItem;
    mnToggleComment: TSpTBXItem;
    mnTabify: TSpTBXItem;
    mnUnTabify: TSpTBXItem;
    SearchMenu: TSpTBXSubmenuItem;
    mnSearchFind: TSpTBXItem;
    mnSearchFindNext: TSpTBXItem;
    mnSearchFindPrevious: TSpTBXItem;
    mnSearchReplace: TSpTBXItem;
    N15: TSpTBXSeparatorItem;
    mnFindinFiles: TSpTBXItem;
    N7: TSpTBXSeparatorItem;
    mnGoToLine: TSpTBXItem;
    mnFindFunction: TSpTBXItem;
    N23: TSpTBXSeparatorItem;
    mnMatchingBrace: TSpTBXItem;
    RunMenu: TSpTBXSubmenuItem;
    mnSyntaxCheck: TSpTBXItem;
    mnImportModule: TSpTBXItem;
    N21: TSpTBXSeparatorItem;
    mnRun: TSpTBXItem;
    N22: TSpTBXSeparatorItem;
    mnExternalRun: TSpTBXItem;
    mnConfigureExternalRun: TSpTBXItem;
    N8: TSpTBXSeparatorItem;
    mnDebug: TSpTBXItem;
    mnRunToCursor: TSpTBXItem;
    mnStepInto: TSpTBXItem;
    mnStepOver: TSpTBXItem;
    mnStepOut: TSpTBXItem;
    mnAbortDebugging: TSpTBXItem;
    N9: TSpTBXSeparatorItem;
    mnTogglebreakpoint: TSpTBXItem;
    mnClearAllBreakpoints: TSpTBXItem;
    ToolsMenu: TSpTBXSubmenuItem;
    mnPythonPath: TSpTBXItem;
    N13: TSpTBXSeparatorItem;
    mnConfigureTools: TSpTBXItem;
    N20: TSpTBXSeparatorItem;
    OptionsMenu: TSpTBXSubmenuItem;
    mnIDEOptions: TSpTBXItem;
    mnEditorOptions: TSpTBXItem;
    mnCustomizeParameters: TSpTBXItem;
    mnCodeTemplates: TSpTBXItem;
    ViewMenu: TSpTBXSubmenuItem;
    mnNextEditor: TSpTBXItem;
    mnPreviousEditor: TSpTBXItem;
    N10: TSpTBXSeparatorItem;
    mnuToolbars: TSpTBXSubmenuItem;
    mnViewStatusBar: TSpTBXItem;
    mnViewII: TSpTBXItem;
    mnViewFileExplorer: TSpTBXItem;
    mnViewCodeExplorer: TSpTBXItem;
    mnViewToDoList: TSpTBXItem;
    mnViewFindResults: TSpTBXItem;
    mnViewOutput: TSpTBXItem;
    DebugWindows1: TSpTBXSubmenuItem;
    mnViewCallStack: TSpTBXItem;
    mnViewVariables: TSpTBXItem;
    mnViewBreakpoints: TSpTBXItem;
    mnViewWatches: TSpTBXItem;
    mnViewMessages: TSpTBXItem;
    HelpMenu: TSpTBXSubmenuItem;
    mnHelpPythonManuals: TSpTBXItem;
    N18: TSpTBXSeparatorItem;
    PyScripter1: TSpTBXSubmenuItem;
    mnHelpParameters: TSpTBXItem;
    mnHelpExternalTools: TSpTBXItem;
    N17: TSpTBXSeparatorItem;
    mnHelpAbout: TSpTBXItem;
    MainToolBar: TSpTBXToolbar;
    tbiFileNewModule: TSpTBXItem;
    tbiFileOpen: TSpTBXItem;
    tbiFileSave: TSpTBXItem;
    tbiFileSaveAll: TSpTBXItem;
    TBXSeparatorItem1: TSpTBXSeparatorItem;
    tbiFilePrint: TSpTBXItem;
    TBXSeparatorItem2: TSpTBXSeparatorItem;
    tbiEditCut: TSpTBXItem;
    tbiEditCopy: TSpTBXItem;
    tbiEditPaste: TSpTBXItem;
    TBXSeparatorItem3: TSpTBXSeparatorItem;
    tbiEditUndo: TSpTBXItem;
    tbiEditRedo: TSpTBXItem;
    TBXSeparatorItem4: TSpTBXSeparatorItem;
    tbiSearchFind: TSpTBXItem;
    tbiSearchFindNext: TSpTBXItem;
    tbiSearchReplace: TSpTBXItem;
    tbiFindInFiles: TSpTBXItem;
    TBXSeparatorItem5: TSpTBXSeparatorItem;
    tbiAbout: TSpTBXItem;
    DebugToolbar: TSpTBXToolbar;
    tbiRunRun: TSpTBXItem;
    TBXSeparatorItem6: TSpTBXSeparatorItem;
    tbiRunDebug: TSpTBXItem;
    tbiRunRunToCursor: TSpTBXItem;
    tbiRunStepInto: TSpTBXItem;
    tbiRunStepOver: TSpTBXItem;
    tbiRunStepOut: TSpTBXItem;
    tbiRunAbort: TSpTBXItem;
    TBXSeparatorItem7: TSpTBXSeparatorItem;
    tbiRunToggleBreakpoint: TSpTBXItem;
    tbiRunClearAllBreakpoints: TSpTBXItem;
    ViewToolbar: TSpTBXToolbar;
    TBXDockLeft: TSpTBXDock;
    TBXDockRight: TSpTBXDock;
    TBXDockBottom: TSpTBXDock;
    mnTools: TSpTBXSubmenuItem;
    TabControlPopupMenu: TSpTBXPopupMenu;
    mnNewModule2: TSpTBXItem;
    mnFileClose2: TSpTBXItem;
    mnFileCloseAll2: TSpTBXItem;
    N12: TSpTBXSeparatorItem;
    mnEditorOptions2: TSpTBXItem;
    RecentSubmenu: TSpTBXSubmenuItem;
    EditorViewsMenu: TSpTBXSubmenuItem;
    TBXSeparatorItem8: TSpTBXSeparatorItem;
    EditorToolbar: TSpTBXToolbar;
    tbiEditDedent: TSpTBXItem;
    tbiEditIndent: TSpTBXItem;
    TBXSeparatorItem10: TSpTBXSeparatorItem;
    tbiEditToggleComment: TSpTBXItem;
    TBXSeparatorItem11: TSpTBXSeparatorItem;
    tbiEditSpecialCharacters: TSpTBXItem;
    tbiEditLineNumbers: TSpTBXItem;
    mnFindPreviousReference: TSpTBXItem;
    mnFindNextReference: TSpTBXItem;
    mnFindDefinition: TSpTBXItem;
    TBXSeparatorItem9: TSpTBXSeparatorItem;
    TBXSubmenuItem3: TSpTBXSubmenuItem;
    mnEditLBMac: TSpTBXItem;
    mnEditLBUnix: TSpTBXItem;
    mnEditLBDos: TSpTBXItem;
    TBXSeparatorItem12: TSpTBXSeparatorItem;
    mnEditUtf8: TSpTBXItem;
    TBXSeparatorItem13: TSpTBXSeparatorItem;
    mnFindReferences: TSpTBXItem;
    tbiBrowseNext: TSpTBXSubmenuItem;
    tbiBrowsePrevious: TSpTBXSubmenuItem;
    TBXSeparatorItem14: TSpTBXSeparatorItem;
    mnHelpContents: TSpTBXItem;
    mnHelpEditorShortcuts: TSpTBXItem;
    TBXSeparatorItem15: TSpTBXSeparatorItem;
    mnCheckForUpdates: TSpTBXItem;
    mnViewRegExpTester: TSpTBXItem;
    mnCommandLineParams: TSpTBXItem;
    mnIDEShortCuts: TSpTBXItem;
    mnUnitTestWizard: TSpTBXItem;
    mnViewUnitTests: TSpTBXItem;
    TBXSeparatorItem16: TSpTBXSeparatorItem;
    mnLayouts: TSpTBXSubmenuItem;
    mnLayOutSeparator: TSpTBXSeparatorItem;
    tbiViewLayouts: TSpTBXSubmenuItem;
    TBXItem47: TSpTBXItem;
    TBXItem48: TSpTBXItem;
    TBXItem49: TSpTBXItem;
    mnMaximizeEditor: TSpTBXItem;
    TBXSeparatorItem17: TSpTBXSeparatorItem;
    TBXSeparatorItem18: TSpTBXSeparatorItem;
    TBXSubmenuItem4: TSpTBXSubmenuItem;
    TBXSeparatorItem19: TSpTBXSeparatorItem;
    mnNoSyntax: TSpTBXItem;
    TBXSeparatorItem20: TSpTBXSeparatorItem;
    TBXSeparatorItem21: TSpTBXSeparatorItem;
    mnSyntax: TSpTBXSubmenuItem;
    mnZoomOut: TSpTBXItem;
    mnZoomIn: TSpTBXItem;
    TBXSeparatorItem22: TSpTBXSeparatorItem;
    mnFiles: TSpTBXSubmenuItem;
    mnAddWatchAtCursor: TSpTBXItem;
    RunningProcessesPopUpMenu: TSpTBXPopupMenu;
    mnFileTemplates: TSpTBXItem;
    TBXSubmenuItem5: TSpTBXSubmenuItem;
    TBXSeparatorItem23: TSpTBXSeparatorItem;
    mnNewFile: TSpTBXItem;
    JvAppInstances: TJvAppInstances;
    mnEditAnsi: TSpTBXItem;
    mnEditUtf8NoBom: TSpTBXItem;
    mnuFindInFilesResults: TSpTBXItem;
    TBXSubmenuItem6: TSpTBXSubmenuItem;
    mnNavEditor: TSpTBXItem;
    TBXSeparatorItem24: TSpTBXSeparatorItem;
    mnNavCodeExplorer: TSpTBXItem;
    mnNavFileExplorer: TSpTBXItem;
    mnNavUnitTests: TSpTBXItem;
    mnNavOutput: TSpTBXItem;
    mnNavTodo: TSpTBXItem;
    TBXSeparatorItem25: TSpTBXSeparatorItem;
    mnNavMessages: TSpTBXItem;
    mnNavBreakpoints: TSpTBXItem;
    mnNavWatches: TSpTBXItem;
    mnNavVariables: TSpTBXItem;
    mnNavCallStack: TSpTBXItem;
    mnNavInterpreter: TSpTBXItem;
    tbiRunPause: TSpTBXItem;
    mnPause: TSpTBXItem;
    mnPythonEngines: TSpTBXSubmenuItem;
    mnEngineRemoteWx: TSpTBXItem;
    mnEngineRemoteTk: TSpTBXItem;
    mnEngineRemote: TSpTBXItem;
    mnEngineInternal: TSpTBXItem;
    mnReinitEngine: TSpTBXItem;
    TBXSeparatorItem26: TSpTBXSeparatorItem;
    TBXSeparatorItem27: TSpTBXSeparatorItem;
    mnExecSelection: TSpTBXItem;
    TBXItem77: TSpTBXItem;
    TBXSeparatorItem28: TSpTBXSeparatorItem;
    mnRestoreEditor2: TSpTBXItem;
    mnMaximizeEditor2: TSpTBXItem;
    mnFileReload: TSpTBXItem;
    TBXSeparatorItem29: TSpTBXSeparatorItem;
    TBXSubmenuItem7: TSpTBXSubmenuItem;
    mnImportShortcuts: TSpTBXItem;
    mnExportShortcuts: TSpTBXItem;
    TBXSeparatorItem30: TSpTBXSeparatorItem;
    mnImportHighlighters: TSpTBXItem;
    mnExportHighlighters: TSpTBXItem;
    mnViewMainMenu: TSpTBXItem;
    FindToolbar: TSpTBXToolbar;
    tbiFindLabel: TSpTBXLabelItem;
    tbiFindNext: TSpTBXItem;
    tbiFindPrevious: TSpTBXItem;
    tbiReplaceSeparator: TSpTBXSeparatorItem;
    tbiReplaceLabel: TSpTBXLabelItem;
    TBXSeparatorItem32: TSpTBXSeparatorItem;
    tbiSearchOptions: TSpTBXSubmenuItem;
    tbiWholeWords: TSpTBXItem;
    tbiSearchInSelection: TSpTBXItem;
    tbiRegExp: TSpTBXItem;
    tbiCaseSensitive: TSpTBXItem;
    tbiIncrementalSearch: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    tbiHighlight: TSpTBXItem;
    tbiReplaceExecute: TSpTBXItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    tbiSearchFromCaret: TSpTBXItem;
    TBXSeparatorItem31: TSpTBXSeparatorItem;
    mnGotoSyntaxError: TSpTBXItem;
    mnSearchHighlight: TSpTBXItem;
    tbiEditWordWrap: TSpTBXItem;
    mnGoToDebugLine: TSpTBXItem;
    mnSplitEditors: TSpTBXSubmenuItem;
    mnSplitEditorVer: TSpTBXItem;
    mnSplitEditorHor: TSpTBXItem;
    mnHideSecondEditor: TSpTBXItem;
    TBXSeparatorItem33: TSpTBXSeparatorItem;
    mnPostMortem: TSpTBXItem;
    SpTBXCustomizer: TSpTBXCustomizer;
    ToolbarPopupMenu: TSpTBXPopupMenu;
    SpTBXSeparatorItem3: TSpTBXSeparatorItem;
    mnViewCustomizeToolbars: TSpTBXItem;
    UserToolbar: TSpTBXToolbar;
    mnNavProjectExplorer: TSpTBXItem;
    mnViewProjectExplorer: TSpTBXItem;
    ProjectMenu: TSpTBXSubmenuItem;
    mnProjectSaveAs: TSpTBXItem;
    mnProjectSave: TSpTBXItem;
    mnProjectOpen: TSpTBXItem;
    mnProjectNew: TSpTBXItem;
    SpTBXSeparatorItem4: TSpTBXSeparatorItem;
    mnNavProjectExplorer2: TSpTBXItem;
    SpTBXSubmenuItem2: TSpTBXSubmenuItem;
    mnHelpProjectHome: TSpTBXItem;
    mnHelpWebSupport: TSpTBXItem;
    mnFileCloseAllOther: TSpTBXItem;
    mnEditUtf16BE: TSpTBXItem;
    mnEditUtf16LE: TSpTBXItem;
    StatusBar: TSpTBXStatusBar;
    SpTBXSeparatorItem5: TSpTBXSeparatorItem;
    lbStatusMessage: TSpTBXLabelItem;
    SpTBXRightAlignSpacerItem1: TSpTBXRightAlignSpacerItem;
    lbStatusCaret: TSpTBXLabelItem;
    SpTBXSeparatorItem6: TSpTBXSeparatorItem;
    lbStatusModified: TSpTBXLabelItem;
    SpTBXSeparatorItem7: TSpTBXSeparatorItem;
    lbStatusOverwrite: TSpTBXLabelItem;
    SpTBXSeparatorItem8: TSpTBXSeparatorItem;
    lbStatusCaps: TSpTBXLabelItem;
    SpTBXSeparatorItem9: TSpTBXSeparatorItem;
    mnMainToolbarVisibilityToggle: TSpTBXItem;
    mnDebugtoolbarVisibilityToggle: TSpTBXItem;
    mnEditorToolbarVisibilityToggle: TSpTBXItem;
    mnViewToolbarVisibilityToggle: TSpTBXItem;
    mnuUserToolbarVisibilityToggle: TSpTBXItem;
    mnLanguage: TSpTBXSubmenuItem;
    actlImmutable: TActionList;
    actViewPreviousEditor: TAction;
    actViewNextEditor: TAction;
    actlStandard: TActionList;
    actNavProjectExplorer: TAction;
    actViewProjectExplorer: TAction;
    actViewCustomizeToolbars: TAction;
    actPostMortem: TAction;
    actViewHideSecondEditor: TAction;
    actViewSplitEditorHor: TAction;
    actExecSelection: TAction;
    actNavEditor: TAction;
    actNavOutput: TAction;
    actNavUnitTests: TAction;
    actNavTodo: TAction;
    actNavCodeExplorer: TAction;
    actNavFileExplorer: TAction;
    actNavMessages: TAction;
    actNavCallStack: TAction;
    actNavVariables: TAction;
    actNavInterpreter: TAction;
    actNavBreakpoints: TAction;
    actNavWatches: TAction;
    actNewFile: TAction;
    actPythonRemoteWx: TAction;
    actPythonRemoteTk: TAction;
    actPythonRemote: TAction;
    actPythonInternal: TAction;
    actPythonReinitialize: TAction;
    actAddWatchAtCursor: TAction;
    actViewSplitEditorVer: TAction;
    actEditorZoomOut: TAction;
    actEditorZoomIn: TAction;
    actMaximizeEditor: TAction;
    actLayoutDebug: TAction;
    actLayoutsDelete: TAction;
    actLayoutSave: TAction;
    actViewRegExpTester: TAction;
    actBrowseForward: TAction;
    actBrowseBack: TAction;
    actFindReferences: TAction;
    actFindDefinition: TAction;
    actViewUnitTests: TAction;
    actViewOutput: TAction;
    actViewFindResults: TAction;
    actViewToDoList: TAction;
    actViewFileExplorer: TAction;
    actViewCodeExplorer: TAction;
    actViewII: TAction;
    actMessagesWin: TAction;
    actWatchesWin: TAction;
    actBreakPointsWin: TAction;
    actClearAllBreakpoints: TAction;
    actToggleBreakPoint: TAction;
    actRunLastScript: TAction;
    actRunLastScriptExternal: TAction;
    actDebugAbort: TAction;
    actDebugPause: TAction;
    actStepOut: TAction;
    actStepOver: TAction;
    actStepInto: TAction;
    actRunToCursor: TAction;
    actRestoreEditor: TAction;
    actDebug: TAction;
    actRunDebugLastScript: TAction;
    actExternalRunConfigure: TAction;
    actExternalRun: TAction;
    actViewStatusBar: TAction;
    actFileExit: TAction;
    actFileCloseAll: TAction;
    actFileOpen: TAction;
    actFileNewModule: TAction;
    actImportModule: TAction;
    actCommandLine: TAction;
    actRun: TAction;
    actSyntaxCheck: TAction;
    actVariablesWin: TAction;
    actCallStackWin: TAction;
    actViewMainMenu: TAction;
    tbiRecentFileList: TSpTBXMRUListItem;
    mnPreviousList: TSpTBXMRUListItem;
    mnNextList: TSpTBXMRUListItem;
    tbiSearchText: TSpTBXComboBox;
    TBControlItem2: TTBControlItem;
    tbiReplaceText: TSpTBXComboBox;
    TBControlItem4: TTBControlItem;
    TabControl1: TSpTBXTabControl;
    tbiRightAlign: TSpTBXRightAlignSpacerItem;
    tbiScrollLeft: TSpTBXItem;
    tbiTabClose: TSpTBXItem;
    tbiScrollRight: TSpTBXItem;
    tbiTabFiles: TSpTBXSubmenuItem;
    tbiTabSep: TSpTBXSeparatorItem;
    SpTBXSeparatorItem11: TSpTBXSeparatorItem;
    SpTBXItem1: TSpTBXItem;
    SpTBXSeparatorItem12: TSpTBXSeparatorItem;
    SpTBXItem2: TSpTBXItem;
    SpTBXItem3: TSpTBXItem;
    TabControl2: TSpTBXTabControl;
    SpTBXRightAlignSpacerItem2: TSpTBXRightAlignSpacerItem;
    SpTBXSeparatorItem13: TSpTBXSeparatorItem;
    tbiTabFiles2: TSpTBXSubmenuItem;
    tbiScrollLeft2: TSpTBXItem;
    tbiScrollRight2: TSpTBXItem;
    tbiTabClose2: TSpTBXItem;
    TabSplitter: TSpTBXSplitter;
    actViewSplitWorkspaceVer: TAction;
    actViewSplitWorkspaceHor: TAction;
    actViewHideSecondaryWorkspace: TAction;
    mnSplitWorkspace: TSpTBXSubmenuItem;
    SpTBXItem7: TSpTBXItem;
    SpTBXItem8: TSpTBXItem;
    SpTBXItem9: TSpTBXItem;
    SpTBXSeparatorItem14: TSpTBXSeparatorItem;
    SpTBXSeparatorItem15: TSpTBXSeparatorItem;
    SpTBXSeparatorItem16: TSpTBXSeparatorItem;
    SpTBXSubmenuItem1: TSpTBXSubmenuItem;
    tbiRecentProjects: TSpTBXMRUListItem;
    tbiAutoCaseSensitive: TSpTBXItem;
    actSelectStyle: TAction;
    tbiSelectStyle: TSpTBXItem;
    SpTBXItem5: TSpTBXItem;
    LocalAppStorage: TJvAppIniFileStorage;
    SpTBXSeparatorItem17: TSpTBXSeparatorItem;
    mnPythonVersions: TSpTBXSubmenuItem;
    tbiSelectPythonVersion: TSpTBXSubmenuItem;
    actPythonSetup: TAction;
    SpTBXSeparatorItem18: TSpTBXSeparatorItem;
    SpTBXItem4: TSpTBXItem;
    SpTBXItem6: TSpTBXItem;
    SpTBXSeparatorItem19: TSpTBXSeparatorItem;
    SpTBXItem10: TSpTBXItem;
    SpTBXSeparatorItem20: TSpTBXSeparatorItem;
    SpTBXItem11: TSpTBXItem;
    actRemoteFileOpen: TAction;
    SpTBXSeparatorItem21: TSpTBXSeparatorItem;
    SpTBXItem12: TSpTBXItem;
    SpTBXItem13: TSpTBXItem;
    actPythonSSH: TAction;
    mnPythonEngineSSH: TSpTBXItem;
    SpTBXItem14: TSpTBXItem;
    SpTBXSeparatorItem22: TSpTBXSeparatorItem;
    lbPythonVersion: TSpTBXLabelItem;
    SpTBXSeparatorItem23: TSpTBXSeparatorItem;
    lbPythonEngine: TSpTBXLabelItem;
    vilImages: TVirtualImageList;
    icIndicators: TSVGIconImageCollection;
    vilIndicators: TVirtualImageList;
    spiStatusLED: TSpTBXItem;
    spiExternalToolsLED: TSpTBXItem;
    SpTBXItem15: TSpTBXItem;
    spiLspLed: TSpTBXItem;
    vilTabDecorators: TVirtualImageList;
    SpTBXSeparatorItem10: TSpTBXSeparatorItem;
    mnSpelling: TSpTBXSubmenuItem;
    mnSpellCheckAdd: TSpTBXItem;
    mnSpellCheckDelete: TSpTBXItem;
    mnSpellCheckIgnore: TSpTBXItem;
    mnSpellCheckIgnoreOnce: TSpTBXItem;
    mnSpellCheckSecondSeparator: TSpTBXSeparatorItem;
    mnSpellCheckTopSeparator: TSpTBXSeparatorItem;
    SpTBXItem20: TSpTBXItem;
    SpTBXItem21: TSpTBXItem;
    SpTBXItem22: TSpTBXItem;
    SpTBXItem23: TSpTBXItem;
    SpTBXSeparatorItem24: TSpTBXSeparatorItem;
    SpTBXItem24: TSpTBXItem;
    SpTBXSeparatorItem25: TSpTBXSeparatorItem;
    SpTBXItem25: TSpTBXItem;
    actEditorZoomReset: TAction;
    mnResetZoom: TSpTBXItem;
    procedure mnFilesClick(Sender: TObject);
    procedure actEditorZoomInExecute(Sender: TObject);
    procedure actEditorZoomOutExecute(Sender: TObject);
    procedure mnSyntaxPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure actMaximizeEditorExecute(Sender: TObject);
    procedure actLayoutDebugExecute(Sender: TObject);
    procedure actLayoutsDeleteExecute(Sender: TObject);
    procedure actLayoutSaveExecute(Sender: TObject);
    procedure actViewUnitTestsExecute(Sender: TObject);
    procedure actCommandLineExecute(Sender: TObject);
    procedure JvAppInstancesCmdLineReceived(Sender: TObject; CmdLine: TStrings);
    procedure actViewRegExpTesterExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure TabContolContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure actSyntaxCheckExecute(Sender: TObject);
    procedure actRunExecute(Sender: TObject);
    procedure actToggleBreakPointExecute(Sender: TObject);
    procedure actClearAllBreakpointsExecute(Sender: TObject);
    procedure actDebugExecute(Sender: TObject);
    procedure actStepIntoExecute(Sender: TObject);
    procedure actStepOverExecute(Sender: TObject);
    procedure actStepOutExecute(Sender: TObject);
    procedure actRunToCursorExecute(Sender: TObject);
    procedure actDebugAbortExecute(Sender: TObject);
    procedure actViewIIExecute(Sender: TObject);
    procedure actMessagesWinExecute(Sender: TObject);
    procedure actNextEditorExecute(Sender: TObject);
    procedure actPreviousEditorExecute(Sender: TObject);
    procedure actCallStackWinExecute(Sender: TObject);
    procedure actVariablesWinExecute(Sender: TObject);
    procedure actBreakPointsWinExecute(Sender: TObject);
    procedure actWatchesWinExecute(Sender: TObject);
    procedure actViewCodeExplorerExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actViewStatusBarExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actFileNewModuleExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFileCloseAllExecute(Sender: TObject);
    procedure actViewFileExplorerExecute(Sender: TObject);
    procedure TabControlTabClosing(Sender: TObject; var Allow, CloseAndFree: Boolean);
    procedure DrawCloseButton(Sender: TObject; ACanvas: TCanvas;
        State: TSpTBXSkinStatesType; const PaintStage: TSpTBXPaintStage;
        var AImageList: TCustomImageList; var AImageIndex: Integer;
        var ARect: TRect; var PaintDefault: Boolean);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure actImportModuleExecute(Sender: TObject);
    procedure actViewToDoListExecute(Sender: TObject);
    procedure actViewFindResultsExecute(Sender: TObject);
    procedure actViewOutputExecute(Sender: TObject);
    procedure actExternalRunExecute(Sender: TObject);
    procedure actExternalRunConfigureExecute(Sender: TObject);
    procedure actFindDefinitionExecute(Sender: TObject);
    procedure actFindReferencesExecute(Sender: TObject);
    procedure PreviousListClick(Sender: TObject; S : string);
    procedure tbiBrowsePreviousClick(Sender: TObject);
    procedure NextListClick(Sender: TObject; S : string);
    procedure tbiBrowseNextClick(Sender: TObject);
    function ApplicationHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
    procedure FormShow(Sender: TObject);
    procedure actAddWatchAtCursorExecute(Sender: TObject);
    procedure actNewFileExecute(Sender: TObject);
    procedure actNavWatchesExecute(Sender: TObject);
    procedure actNavBreakpointsExecute(Sender: TObject);
    procedure actNavInterpreterExecute(Sender: TObject);
    procedure actNavVariablesExecute(Sender: TObject);
    procedure actNavCallStackExecute(Sender: TObject);
    procedure actNavMessagesExecute(Sender: TObject);
    procedure actNavFileExplorerExecute(Sender: TObject);
    procedure actNavCodeExplorerExecute(Sender: TObject);
    procedure actNavTodoExecute(Sender: TObject);
    procedure actNavUnitTestsExecute(Sender: TObject);
    procedure actNavOutputExecute(Sender: TObject);
    procedure actNavRETesterExecute(Sender: TObject);
    procedure actNavEditorExecute(Sender: TObject);
    procedure actDebugPauseExecute(Sender: TObject);
    procedure actEditorZoomResetExecute(Sender: TObject);
    procedure actPythonReinitializeExecute(Sender: TObject);
    procedure actPythonEngineExecute(Sender: TObject);
    procedure actExecSelectionExecute(Sender: TObject);
    procedure actRestoreEditorExecute(Sender: TObject);
    procedure actViewMainMenuExecute(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TabControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SearchOptionsChanged(Sender: TObject);
    procedure tbiSearchOptionsPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure tbiSearchTextChange(Sender: TObject);
    procedure tbiSearchTextKeyPress(Sender: TObject; var Key: Char);
    procedure tbiReplaceTextChange(Sender: TObject);
    procedure actViewSplitEditorVerExecute(Sender: TObject);
    procedure actViewSplitEditorHorExecute(Sender: TObject);
    procedure actViewHideSecondEditorExecute(Sender: TObject);
    procedure actPostMortemExecute(Sender: TObject);
    procedure FindToolbarVisibleChanged(Sender: TObject);
    procedure actViewCustomizeToolbarsExecute(Sender: TObject);
    procedure SpTBXCustomizerGetCustomizeForm(Sender: TObject;
      var CustomizeFormClass: TSpTBXCustomizeFormClass);
    procedure actViewProjectExplorerExecute(Sender: TObject);
    procedure actNavProjectExplorerExecute(Sender: TObject);
    procedure actRunLastScriptExternalExecute(Sender: TObject);
    procedure actRunLastScriptExecute(Sender: TObject);
    procedure actRunDebugLastScriptExecute(Sender: TObject);
    procedure EditorViewsMenuClick(Sender: TObject);
    procedure tbiRecentFileListClick(Sender: TObject; const Filename: string);
    procedure tbiSearchTextExit(Sender: TObject);
    procedure tbiReplaceTextKeyPress(Sender: TObject; var Key: Char);
    procedure TabControlActiveTabChange(Sender: TObject; TabIndex: Integer);
    procedure tbiScrollLeftClick(Sender: TObject);
    procedure tbiScrollRightClick(Sender: TObject);
    procedure actViewSplitWorkspaceVerExecute(Sender: TObject);
    procedure actViewSplitWorkspaceHorExecute(Sender: TObject);
    procedure actViewHideSecondaryWorkspaceExecute(Sender: TObject);
    procedure tbiRecentProjectsClick(Sender: TObject; const Filename: string);
    procedure actSelectStyleExecute(Sender: TObject);
    procedure mnPythonVersionsPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure PythonVersionsClick(Sender: TObject);
    procedure actPythonSetupExecute(Sender: TObject);
    procedure actRemoteFileOpenExecute(Sender: TObject);
    procedure lbPythonVersionClick(Sender: TObject);
    procedure lbPythonEngineClick(Sender: TObject);
    procedure lbStatusCaretClick(Sender: TObject);
    procedure mnSpellingPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure mnSyntaxClick(Sender : TObject);
  private
    DSAAppStorage: TDSAAppStorage;
    ShellExtensionFiles : TStringList;
//    function FindAction(var Key: Word; Shift: TShiftState) : TCustomAction;
    procedure DebugActiveScript(ActiveEditor: IEditor;
      InitStepIn : Boolean = False; RunToCursorLine : integer = -1);
    procedure SetupRunConfiguration(var RunConfig: TRunConfiguration; ActiveEditor: IEditor);
    procedure tbiSearchTextAcceptText(const NewText: string);
    procedure tbiReplaceTextAcceptText(const NewText: string);
    function GetActiveTabControl: TSpTBXCustomTabControl;
    procedure SetActiveTabControl(const Value: TSpTBXCustomTabControl);
    procedure OpenInitialFiles;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMWTSSessionChange (var Message: TMessage); message WM_WTSSESSION_CHANGE;
    procedure WMEndSession(var Msg: TWMEndSession); message WM_ENDSESSION;
    procedure WMQueryEndSession(var Msg : TWMQueryEndSession); message WM_QUERYENDSESSION;
  protected
    fCurrentBrowseInfo : string;
    function CmdLineOpenFiles(): boolean;
    function OpenCmdLineFile(FileName : string) : Boolean;
    procedure DebuggerBreakpointChange(Sender: TObject; Editor : IEditor; ALine: integer);
    procedure DebuggerCurrentPosChange(Sender: TObject; const OldPos, NewPos: TEditorPos);
    procedure DebuggerErrorPosChange(Sender: TObject; const OldPos, NewPos: TEditorPos);
    procedure UpdateStandardActions;
    procedure UpdateStatusBarPanels;
    procedure ApplicationOnHint(Sender: TObject);
    procedure ApplcationOnShowHint(var HintStr: string; var CanShow: Boolean;
      var HintInfo: Vcl.Controls.THintInfo);
    procedure ApplicationActionUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure ApplicationActionExecute(Action: TBasicAction; var Handled: Boolean);
    procedure TabToolBarDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TabToolbarlDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure WMFindDefinition(var Msg: TMessage); message WM_FINDDEFINITION;
    procedure WMSearchReplaceAction(var Msg: TMessage); message WM_SEARCHREPLACEACTION;
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    procedure SelectEditor(Sender : TObject);
    procedure mnLanguageClick(Sender: TObject);
    // Remote Desktop
    // See https://blogs.embarcadero.com/how-to-speed-up-remote-desktop-applications/
    procedure CreateWnd; override;
    // Browse MRU stuff
    procedure PrevClickHandler(Sender: TObject);
    procedure NextClickHandler(Sender: TObject);
    procedure PrevMRUAdd(S : string);
    procedure NextMRUAdd(S : string);
  private
    OldMonitorProfile : string;
    FShellImages: TCustomImageList;
    // IIDELayouts implementation
    function LayoutExists(const Layout: string): Boolean;
    procedure LoadLayout(const Layout : string);
    procedure SaveLayout(const Layout : string);
   // IPyIDEServices implementation
    function ReplaceParams(const AText: string): string;
    function GetActiveEditor : IEditor;
    function GetIsClosing: Boolean;
    procedure WriteStatusMsg(const S : string);
    function FileIsPythonSource(const FileName: string): Boolean;
    function ShowFilePosition(FileName : string; Line: integer = 0;
      Offset : integer = 1; SelLen : integer = 0;
      ForceToMiddle : Boolean = True; FocusEditor : Boolean = True): Boolean;
    procedure ClearPythonWindows;
    procedure SaveEnvironment;
    procedure SaveFileModules;
    procedure SetRunLastScriptHints(const ScriptName : string);
    function GetStoredScript(const Name: string): TStrings;
    function GetMessageServices: IMessageServices;
    function GetUnitTestServices: IUnitTestServices;
    function GetIDELayouts: IIDELayouts;
    function GetAppStorage: TJvCustomAppStorage;
    function GetLocalAppStorage: TJvCustomAppStorage;
    function GetLogger: TJclSimpleLog;
    procedure MRUAddEditor(Editor: IEditor);
  public
    ActiveTabControlIndex : integer;
    PythonKeywordHelpRequested : Boolean;
    MenuHelpRequested : Boolean;
    Layouts : TStringList;
    fLanguageList : TStringList;
    procedure StoreApplicationData;
    procedure RestoreApplicationData;
    procedure StoreLocalApplicationData;
    procedure RestoreLocalApplicationData;
    function DoOpenFile(AFileName: string; HighlighterName : string = '';
       TabControlIndex : integer = 1) : IEditor;
    function NewFileFromTemplate(FileTemplate : TFileTemplate;
       TabControlIndex : integer = 1) : IEditor;
    procedure UpdateDebugCommands(DebuggerState : TDebuggerState);
    procedure DebuggerStateChange(Sender: TObject; OldState,
      NewState: TDebuggerState);
    procedure ApplicationOnIdle(Sender: TObject; var Done: Boolean);
    procedure PyIDEOptionsChanged;
    procedure SetupCustomizer;
    procedure SetupLanguageMenu;
    procedure SetupToolsMenu;
    procedure SetupLayoutsMenu;
    procedure SetupSyntaxMenu;
    procedure SetupPythonVersionsMenu;
    procedure LayoutClick(Sender : TObject);
    procedure LoadToolbarLayout(const Layout: string);
    procedure LoadToolbarItems(const Path : string);
    procedure SaveToolbarLayout(const Layout: string);
    procedure SaveToolbarItems(const Path : string);
    function JumpToFilePosInfo(const FilePosInfo : string): Boolean;
    procedure FindDefinition(Editor : IEditor; TextCoord: TBufferCoord;
      ShowMessages, Silent, JumpToFirstMatch : Boolean; var FilePosInfo : string);
    procedure AdjustBrowserLists(FileName: string; Line: Integer; Col: Integer;
      FilePosInfo: string);
    procedure ThemeEditorGutter(Gutter : TSynGutter);
    procedure UpdateCaption;
    procedure ChangeLanguage(LangCode : string);
    function EditorFromTab(Tab : TSpTBXTabItem): IEditor;
    procedure SplitWorkspace(SecondTabsVisible: Boolean;
      Alignment : TAlign = alRight; Size : integer = -1);
    procedure MoveTab(Tab : TSpTBXTabItem; TabControl: TSpTBXTabControl;
      Index : integer = -1);
    function TabControl(TabControlIndex : integer = 1): TSpTBXTabControl;
    function TabControlIndex(TabControl : TSpTBXCustomTabControl) : integer;
    procedure ShowIDEDockForm(Form: TForm);
    property ActiveTabControl : TSpTBXCustomTabControl read GetActiveTabControl
      write SetActiveTabControl;
  end;

Const
  ctkRemember : TDSACheckTextKind = 100;
  FactoryToolbarItems = 'Factory Toolbar Items v1.0';

var
  PyIDEMainForm: TPyIDEMainForm;

implementation

uses
  Winapi.ShellAPI,
  System.Contnrs,
  System.Math,
  System.IniFiles,
  System.RegularExpressions,
  System.IOUtils,
  Vcl.Clipbrd,
  Vcl.StdActns,
  Vcl.Themes,
  JclSysInfo,
  JvJVCLUtils,
  SpTBXControls,
  VirtualTrees.BaseTree,
  VirtualTrees,
  VirtualExplorerTree,
  MPCommonObjects,
  SynHighlighterPython,
  SynEditHighlighter,
  SynEditKeyCmds,
  SynCompletionProposal,
  SynSpellCheck,
  PythonEngine,
  PythonVersions,
  JvGnugettext,
  StringResources,
  uCmdLine,
  uCommonFunctions,
  uSearchHighlighter,
  uParams,
  dlgNewFile,
  dlgCommandLine,
  dlgToolProperties,
  dlgStyleSelector,
  dlgPickList,
  frmEditor,
  frmIDEDockWin,
  frmCommandOutput,
  frmPythonII,
  frmProjectExplorer,
  frmMessages,
  frmCallStack,
  frmBreakPoints,
  frmVariables,
  frmWatches,
  frmCodeExplorer,
  frmFileExplorer,
  frmRegExpTester,
  frmUnitTests,
  frmToDo,
  frmFindResults,
  frmWebPreview,
  frmModSpTBXCustomize,
  cTools,
  cParameters,
  cFilePersist,
  cCodeHint,
  cPyRemoteDebugger,
  cProjectClasses,
  dlgPythonVersions,
  dlgRemoteFile,
  cSSHSupport,
  LspUtils,
  JediLspClient;

{$R *.DFM}

{ TWorkbookMainForm }

function TPyIDEMainForm.DoOpenFile(AFileName: string; HighlighterName : string = '';
  TabControlIndex : integer = 1) : IEditor;
Var
  IsRemote : Boolean;
  Server, FName : string;
  TabCtrl : TSpTBXTabControl;
begin
  Result := nil;
  tbiRecentFileList.MRURemove(AFileName);
  IsRemote :=  TSSHFileName.Parse(AFileName, Server, FName);

  // activate the editor if already open
  if IsRemote then
  begin
    Result :=  GI_EditorFactory.GetEditorByFileId(AFileName);
    if Assigned(Result) then begin
      Result.Activate;
      Exit;
    end;
  end
  else if AFileName <> '' then
  begin
    AFileName := GetLongFileName(ExpandFileName(AFileName));
    Result :=  GI_EditorFactory.GetEditorByName(AFileName);
    if Assigned(Result) then begin
      Result.Activate;
      Exit;
    end
    else if not FileExists(AFileName) then begin
      WriteStatusMsg(_(Format('File %s does not exist', [AFileName])));
      Exit;
    end;
  end;
  // create a new editor, add it to the editor list, open the file
  TabCtrl := TabControl(TabControlIndex);
  TabCtrl.Toolbar.BeginUpdate;
  try
    Result := GI_EditorFactory.NewEditor(TabControlIndex);
    if Result <> nil then begin
      try
        if IsRemote then
          Result.OpenRemoteFile(FName, Server)
        else
          Result.OpenFile(AFileName, HighlighterName);
        Result.Activate;
      except
        Result.Close;
        raise
      end;
      if (AFileName <> '') and (GI_EditorFactory.Count = 2) and
        (GI_EditorFactory.Editor[0].FileName = '') and
        (GI_EditorFactory.Editor[0].RemoteFileName = '') and
        not GI_EditorFactory.Editor[0].Modified
      then
        GI_EditorFactory.Editor[0].Close;
    end;
  finally
    TabCtrl.Toolbar.EndUpdate;
    if Assigned(TabCtrl.ActiveTab) then
      TabCtrl.MakeVisible(TabCtrl.ActiveTab);
    UpdateCaption;
  end;
end;

procedure TPyIDEMainForm.EditorViewsMenuClick(Sender: TObject);
begin
  GI_EditorFactory.UpdateEditorViewsMenu(EditorViewsMenu);
end;

type
  TTBCustomItemAccess = class(TTBCustomItem);

procedure TPyIDEMainForm.FormCreate(Sender: TObject);
Var
  TabHost : TJvDockTabHostForm;
  LocalOptionsFileName: string;
begin
  // Shell Images
  FShellImages := TCommonVirtualImageList.Create(Self);
  TCommonVirtualImageList(FShellImages).SourceImageList := SmallSysImages;
  FShellImages.SetSize(MulDiv(FShellImages.Width, FCurrentPPI, Screen.PixelsPerInch),
    MulDiv(FShellImages.Height, FCurrentPPI, Screen.PixelsPerInch));

  //Set the HelpFile
  Application.HelpFile := ExtractFilePath(Application.ExeName) + 'PyScripter.chm';
  Application.OnHelp := Self.ApplicationHelp;

  DockServer.DockStyle := ResourcesDataModule.DockStyle; // JvDockVSNetStyleSpTBX;

  // App Instances
  ShellExtensionFiles := TStringList.Create;
  if not CmdLineReader.readFlag('NEWINSTANCE') then begin
    JvAppInstances.Active := True;
    JvAppInstances.Check;
  end;

  //  Layout stuff
  Layouts := TStringList.Create;
  Layouts.Sorted := True;
  Layouts.Duplicates := dupError;

  // GI_PyIDEServices
  GI_PyIDEServices := Self;

  // Application Storage
  AppStorage.Encoding := TEncoding.UTF8;
  AppStorage.FileName := TPyScripterSettings.OptionsFileName;

  // LocalAppStorage
  LocalOptionsFileName := ChangeFileExt(TPath.GetFileName(Application.ExeName), '.local.ini');
  LocalAppStorage.FileName :=
    TPyScripterSettings.UserDataPath + LocalOptionsFileName;

  //OutputDebugString(PWideChar(Format('%s ElapsedTime %d ms', ['Before All Forms', StopWatch.ElapsedMilliseconds])));
  // Create and layout IDE windows
  PythonIIForm := TPythonIIForm.Create(self);
  PythonIIForm.PopupParent := Self;
  CallStackWindow := TCallStackWindow.Create(Self);
  CallStackWindow.PopupParent := Self;
  VariablesWindow := TVariablesWindow.Create(Self);
  VariablesWindow.PopupParent := Self;
  WatchesWindow := TWatchesWindow.Create(Self);
  WatchesWindow.PopupParent := Self;
  BreakPointsWindow := TBreakPointsWindow.Create(Self);
  BreakPointsWindow.PopupParent := Self;
  OutputWindow := TOutputWindow.Create(Self);
  OutputWindow.PopupParent := Self;
  MessagesWindow := TMessagesWindow.Create(Self);
  MessagesWindow.PopupParent := Self;
  CodeExplorerWindow := TCodeExplorerWindow.Create(Self);
  CodeExplorerWindow.PopupParent := Self;
  FileExplorerWindow := TFileExplorerWindow.Create(Self);
  FileExplorerWindow.PopupParent := Self;
  ToDoWindow := TToDoWindow.Create(Self);
  ToDoWindow.PopupParent := Self;
  RegExpTesterWindow := TRegExpTesterWindow.Create(Self);
  RegExpTesterWindow.PopupParent := Self;
  UnitTestWindow := TUnitTestWindow.Create(Self);
  UnitTestWindow.PopupParent := Self;
  FindResultsWindow := TFindResultsWindow.Create(Self);
  FindResultsWindow.PopupParent := Self;
  ProjectExplorerWindow := TProjectExplorerWindow.Create(Self);
  ProjectExplorerWindow.PopupParent := Self;

  // And now translate after all the docking forms have been created
  // They will be translated as well
  TP_GlobalIgnoreClass(TVirtualImageList);
  TranslateComponent(Self);
  //OutputDebugString(PWideChar(Format('%s ElapsedTime %d ms', ['After Translate', StopWatch.ElapsedMilliseconds])));

  // Setup Languages
  fLanguageList := TStringList.Create;
  SetUpLanguageMenu;

  // ActionLists
  TActionProxyCollection.ActionLists :=
    [actlStandard,
     CommandsDataModule.actlMain,
     PythonIIForm.InterpreterActionList,
     ProjectExplorerWindow.ProjectActionList,
     CallStackWindow.actlCallStack];

  // Notifications
  PyIDEOptions.OnChange.AddHandler(PyIDEOptionsChanged);
  SkinManager.AddSkinNotification(Self);
  SkinManager.BroadcastSkinNotification;

  // Read Settings from PyScripter.ini
  if FileExists(AppStorage.IniFile.FileName) then
    RestoreApplicationData
  else
    PyIDEOptions.Changed;

  // Application Restart
  if PyIDEOptions.AutoRestart then
    RegisterApplicationRestart;

  // Read Settings from PyScripter.local.ini
  if FileExists(LocalAppStorage.IniFile.FileName) then
    RestoreLocalApplicationData
  else
    WindowState := TWindowState.wsMaximized;

  // DSA stuff
  DSAAppStorage := TDSAAppStorage.Create(AppStorage, 'DSA');
  RegisterDSACheckMarkText(ctkRemember, _(SDSActkRememberText));
  RegisterDSA(dsaSearchFromStart, 'SearchFromStart', 'Search from start question', DSAAppStorage, ctkRemember);
  RegisterDSA(dsaReplaceFromStart, 'ReplaceFromStart', 'Replace from start question', DSAAppStorage, ctkRemember);
  RegisterDSA(dsaReplaceNumber, 'ReplaceNumber', 'Information about number of replacements', DSAAppStorage, ctkShow);
  RegisterDSA(dsaSearchStartReached, 'SearchStartReached', 'Information: search start reached', DSAAppStorage, ctkShow);
  RegisterDSA(dsaPostMortemInfo, 'PostMortemInfo', 'Instructions: Post Mortem', DSAAppStorage, ctkShow);
  RegisterDSA(dsaDictonaryNA, 'DictornayNA', 'DictionaryNA', DSAAppStorage, ctkShow);

  // Store Factory Settings
  if not AppStorage.PathExists(FactoryToolbarItems) then
    SaveToolbarItems(FactoryToolbarItems);

  if (OldMonitorProfile = MonitorProfile) and
     LocalAppStorage.PathExists('Layouts\Default\Forms') and
     LocalAppStorage.PathExists('Layouts\Current\Forms') then
  begin
    try
      //OutputDebugString(PWideChar(Format('%s ElapsedTime %d ms', ['Before LoadLayout', StopWatch.ElapsedMilliseconds])));
      LoadLayout('Current');
      //OutputDebugString(PWideChar(Format('%s ElapsedTime %d ms', ['After LoadLayout', StopWatch.ElapsedMilliseconds])));
    except
      LocalAppStorage.DeleteSubTree('Layouts\Default');
      if Layouts.IndexOf('Default') >= 0 then
        Layouts.Delete(Layouts.IndexOf('Default'));
      StyledMessageDlg(Format(_(SErrorLoadLayout),
        [LocalAppStorage.IniFile.FileName]), mtError, [mbOK], 0);
    end;
    LocalAppStorage.DeleteSubTree('Layouts\Current');
  end
  else
  begin
    TabHost := ManualTabDock(DockServer.LeftDockPanel, FileExplorerWindow, ProjectExplorerWindow);
    DockServer.LeftDockPanel.Width := PPIScale(250);
    ManualTabDockAddPage(TabHost, CodeExplorerWindow);
    ShowDockForm(FileExplorerWindow);
    //TJvDockVSNETPanel(DockServer.LeftDockPanel).DoAutoHideControl(TabHost);

    TabHost := ManualTabDock(DockServer.BottomDockPanel, CallStackWindow, VariablesWindow);
    DockServer.BottomDockPanel.Height := PPIScale(200);
    ManualTabDockAddPage(TabHost, WatchesWindow);
    ManualTabDockAddPage(TabHost, BreakPointsWindow);
    ManualTabDockAddPage(TabHost, OutputWindow);
    ManualTabDockAddPage(TabHost, MessagesWindow);
    ManualTabDockAddPage(TabHost, PythonIIForm);
    ShowDockForm(PythonIIForm);

    Application.ProcessMessages;
  end;

  Application.OnIdle := ApplicationOnIdle;
  Application.OnHint := ApplicationOnHint;
  Application.OnShowHint := ApplcationOnShowHint;
  Application.OnActionUpdate := ApplicationActionUpdate;
  Application.OnActionExecute := ApplicationActionExecute;

  //  Editor Views Menu
  GI_EditorFactory.SetupEditorViewsMenu(EditorViewsMenu, vilImages);

  //Update;

  // Tab Conrol Drag Drop and other TabControl events
  TabControl1.Toolbar.OnDragOver := TabToolbarDragOver;
  TabControl1.Toolbar.OnDragDrop := TabToolbarlDragDrop;
  TabControl2.Toolbar.OnDragOver := TabToolbarDragOver;
  TabControl2.Toolbar.OnDragDrop := TabToolbarlDragDrop;

  TabControl1.Toolbar.OnMouseDown := TabControlMouseDown;
  TabControl2.Toolbar.OnMouseDown := TabControlMouseDown;

  //Flicker
  MainMenu.DoubleBuffered := True;
  MainToolBar.DoubleBuffered := True;
  DebugToolbar.DoubleBuffered := True;
  ViewToolbar.DoubleBuffered := True;
  EditorToolbar.DoubleBuffered := True;
  UserToolbar.DoubleBuffered := True;
  FindToolbar.DoubleBuffered := True;
end;

procedure TPyIDEMainForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);

  procedure DelayedClose;
  begin
    TThread.ForceQueue(nil, procedure
    begin
      PostMessage(Application.Handle, WM_CLOSE, 0, 0);
    end, 1000);
  end;

begin
  if JvGlobalDockIsLoading then begin
    CanClose := False;
    DelayedClose;
    Exit;
  end else if PyControl.DebuggerState <> dsInactive then begin
    if StyledMessageDlg(_(SAbortDebugging), mtWarning, [mbYes, mbNo], 0) = mrYes then
    begin
      if (PyControl.DebuggerState in [dsPaused, dsPostMortem]) or
        (PyControl.ActiveDebugger is TPyInternalDebugger) then
      begin
        CanClose := False;
        PyControl.ActiveDebugger.Abort;
        DelayedClose;
        Exit;
      end else begin
        CanClose := False;
        PyControl.ActiveInterpreter.ReInitialize;
        DelayedClose;
        Exit;
      end;
    end else begin  // mrNo
       CanClose := False;
       Exit;
    end;
  end;

  if OutputWindow.IsRunning then
    if StyledMessageDlg(_(SKillExternalTool), mtConfirmation, [mbYes, mbCancel], 0) = mrYes
    then begin
      OutputWindow.actToolTerminateExecute(Self);
      CanClose := True;
    end else
      CanClose := False;

  // Ask about saving unsaved editor buffers
  if CanClose and (GI_EditorFactory <> nil) then
    CanClose := GI_EditorFactory.CanCloseAll;

  // Ask about saving unsaved project
  CanClose := CanClose and ProjectExplorerWindow.CanClose;

  if CanClose then begin
    // Shut down help
    Application.OnHelp := nil;
    // QC25183
    try
      Application.HelpCommand(HELP_QUIT, 0);
    except
    end;

    // Disconnect ChangeNotify
    FileExplorerWindow.FileExplorerTree.Active := False;
    FileExplorerWindow.ConfigureThreads(fcnDisabled, False);

    // Stop accepting files
    DragAcceptFiles(TabControl1.Handle, False);
    DragAcceptFiles(TabControl2.Handle, False);
    ClearPythonWindows;

    // Give the time to the treads to terminate
    Sleep(200);

    //  We need to do this here so that MRU and docking information are persisted
    try
      SaveEnvironment;
    except
      on E: EFileStreamError do
        StyledMessageDlg(Format(_(SFileSaveError), [AppStorage.FullFileName, E.Message]), mtError, [mbOK], 0);
    end;

    TabControl1.Toolbar.BeginUpdate;
    TabControl2.Toolbar.BeginUpdate;
    try
      if GI_EditorFactory <> nil then
        GI_EditorFactory.CloseAll;
    finally
      TabControl1.Toolbar.EndUpdate;
      TabControl2.Toolbar.EndUpdate;
    end;

    SkinManager.RemoveSkinNotification(Self);
  end;
end;

procedure TPyIDEMainForm.TabContolContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
Var
  IV: TTBItemViewer;
  TabCtrl : TSpTBXTabControl;
begin
  TabCtrl := Sender as TSpTBXTabControl;
  ActiveTabControl := TabCtrl;
  IV := TabCtrl.View.ViewerFromPoint(
    TabCtrl.Toolbar.ScreenToClient(TabCtrl.ClientToScreen(MousePos)));
  if Assigned(IV) and (IV.Item is TSpTBXTabItem) then
    IV.Item.Checked := True;
  //To update File Close
  CommandsDataModule.UpdateMainActions;
  Handled := False;
end;

procedure TPyIDEMainForm.actNavBreakpointsExecute(Sender: TObject);
begin
  ShowDockForm(BreakPointsWindow);
  BreakPointsWindow.FormActivate(Sender);
end;

procedure TPyIDEMainForm.actNavCallStackExecute(Sender: TObject);
begin
  ShowDockForm(CallStackWindow);
  CallStackWindow.FormActivate(Sender);
end;

procedure TPyIDEMainForm.actNavCodeExplorerExecute(Sender: TObject);
begin
  ShowDockForm(CodeExplorerWindow);
  CodeExplorerWindow.FormActivate(Sender);
  // only when activated by the menu or the keyboard - Will be reset by frmIDEDockWin
  ResourcesDataModule.DockStyle.ChannelOption.MouseleaveHide := False;
end;

procedure TPyIDEMainForm.actNavEditorExecute(Sender: TObject);
Var
  Editor : IEditor;
begin
  Editor := GetActiveEditor;
  if Assigned(Editor) then
    Editor.Activate;
end;

procedure TPyIDEMainForm.ShowIDEDockForm(Form: TForm);
begin
  ShowDockForm(Form as TIDEDockWindow);
  if Assigned(Form.OnActivate) then
    Form.OnActivate(Self);
  // only when activated by the menu or the keyboard - Will be reset by frmIDEDockWin
  ResourcesDataModule.DockStyle.ChannelOption.MouseleaveHide := False;
end;

procedure TPyIDEMainForm.ClearPythonWindows;
begin
  VariablesWindow.ClearAll;
  UnitTestWindow.ClearAll;
  CallStackWindow.ClearAll;
  RegExpTesterWindow.Clear;
end;

procedure TPyIDEMainForm.actNavFileExplorerExecute(Sender: TObject);
begin
  ShowIDEDockForm(FileExplorerWindow);
end;

procedure TPyIDEMainForm.actNavInterpreterExecute(Sender: TObject);
begin
  ShowIDEDockForm(PythonIIForm);
end;

procedure TPyIDEMainForm.actNavMessagesExecute(Sender: TObject);
begin
  ShowIDEDockForm(MessagesWindow);
end;

procedure TPyIDEMainForm.actNavOutputExecute(Sender: TObject);
begin
  ShowIDEDockForm(OutputWindow);
end;

procedure TPyIDEMainForm.actNavProjectExplorerExecute(Sender: TObject);
begin
  ShowIDEDockForm(ProjectExplorerWindow);
end;

procedure TPyIDEMainForm.actNavRETesterExecute(Sender: TObject);
begin
  ShowIDEDockForm(RegExpTesterWindow);
end;

procedure TPyIDEMainForm.actNavTodoExecute(Sender: TObject);
begin
  ShowIDEDockForm(ToDoWindow);
end;

procedure TPyIDEMainForm.actNavUnitTestsExecute(Sender: TObject);
begin
  ShowIDEDockForm(UnitTestWindow);
end;

procedure TPyIDEMainForm.actNavVariablesExecute(Sender: TObject);
begin
  ShowIDEDockForm(VariablesWindow);
end;

procedure TPyIDEMainForm.actNavWatchesExecute(Sender: TObject);
begin
  ShowIDEDockForm(WatchesWindow);
end;

procedure TPyIDEMainForm.actNewFileExecute(Sender: TObject);
begin
  with TNewFileDialog.Create(Self) do begin
    if ShowModal = mrOK then begin
      NewFileFromTemplate(SelectedTemplate);
    end;
    Free;
  end;
end;

procedure TPyIDEMainForm.actNextEditorExecute(Sender: TObject);
Var
  TabItem : TSpTBXTabItem;
  TabCtrl : TSpTBXTabControl;
begin
  TabCtrl := ActiveTabControl as TSpTBXTabControl;
  if TabCtrl.PagesCount <= 1 then Exit;
  TabItem := nil;
  if PyIDEOptions.SmartNextPrevPage then with TabCtrl do begin
    Repeat
      Inc(zOrderPos);
      if zOrderPos >= zOrder.Count then
        ZOrderPos := 0;
      while zOrderPos < zOrder.Count  do begin
        TabItem := zOrder[zOrderPos];
        if Items.IndexOf(TabItem) < 0 then begin
          zOrder.Delete(zOrderPos);
          TabItem := nil;
        end else
          break;
      end;
    Until Assigned(TabItem) or (ZOrder.Count = 0);
    KeyPreview := True;
    zOrderProcessing := True;
  end else begin
    if Assigned(TabCtrl.ActivePage) then
      TabItem := TabCtrl.ActivePage.Item.GetNextTab(True, sivtNormal)
    else
      TabItem := TabCtrl.Pages[0].Item;
  end;

  if not Assigned(TabItem) and (TabCtrl.PagesCount > 0) then
    TabItem := TabCtrl.Pages[0].Item;
  if Assigned(TabItem) then
    TabItem.Checked := True;
end;

procedure TPyIDEMainForm.actPostMortemExecute(Sender: TObject);
begin
  PyControl.ActiveDebugger.EnterPostMortem;
end;

procedure TPyIDEMainForm.actPreviousEditorExecute(Sender: TObject);
Var
  TabItem : TSpTBXTabItem;
  TabCtrl : TSpTBXTabControl;
begin
  TabCtrl := ActiveTabControl as TSpTBXTabControl;
  if TabCtrl.PagesCount <= 1 then Exit;
  TabItem := nil;
  if PyIDEOptions.SmartNextPrevPage then with TabCtrl do begin
    Repeat
      Dec(zOrderPos);
      if zOrderPos < 0 then
        zOrderPos := zOrder.Count - 1;
      while zOrderPos < zOrder.Count  do begin
        TabItem := zOrder[zOrderPos];
        if Items.IndexOf(TabItem) < 0 then begin
          zOrder.Delete(zOrderPos);
          TabItem := nil;
        end else
          break;
      end;
    Until Assigned(TabItem) or (ZOrder.Count = 0);
    KeyPreview := True;
    zOrderProcessing := True;
  end else begin
    if Assigned(TabCtrl.ActivePage) then
      TabItem := TabCtrl.ActivePage.Item.GetNextTab(False, sivtNormal)
    else
      TabItem := TabCtrl.Pages[TabCtrl.PagesCount-1].Item;
  end;
  if not Assigned(TabItem) then
    TabItem := TabCtrl.Pages[TabCtrl.PagesCount-1].Item;
  if Assigned(TabItem) then
    TabItem.Checked := True;
end;

procedure TPyIDEMainForm.actPythonEngineExecute(Sender: TObject);
Var
  EngineType : TPythonEngineType;
  SSHServer: string;
begin
  EngineType := TPythonEngineType((Sender as TAction).Tag);
  if EngineType = peSSH then begin
    SSHServer := SelectSSHServer;
    if SSHServer <> '' then
      PyControl.ActiveSSHServerName := SSHServer
    else
      Exit;
  end;
  PyControl.PythonEngineType := EngineType;
end;

procedure TPyIDEMainForm.actPythonReinitializeExecute(Sender: TObject);
begin
  if not GI_PyControl.Inactive then begin
    if StyledMessageDlg(_(STerminateInterpreter),
      mtWarning, [mbYes, mbNo], 0) = idNo then Exit;
  end;
  PyControl.ActiveInterpreter.ReInitialize;
end;

procedure TPyIDEMainForm.actPythonSetupExecute(Sender: TObject);
begin
  TThread.ForceQueue(nil, procedure
  begin
    with TPythonVersionsDialog.Create(Self) do
    begin
      ShowModal;
      Release;
      SetupPythonVersionsMenu;
    end;
  end);
end;

procedure TPyIDEMainForm.actSyntaxCheckExecute(Sender: TObject);
var
  ActiveEditor : IEditor;
  ErrorPos: TEditorPos;
begin
  ActiveEditor := GetActiveEditor;
  if not Assigned(ActiveEditor) then Exit;

  if TPyInternalInterpreter(PyControl.InternalInterpreter).SyntaxCheck(ActiveEditor, ErrorPos) then begin
    GI_PyIDEServices.Messages.AddMessage(Format(_(SSyntaxIsOK), [ActiveEditor.FileTitle]));
    ShowDockForm(MessagesWindow);
  end else
    ShowDockForm(PythonIIForm);
end;

procedure TPyIDEMainForm.actImportModuleExecute(Sender: TObject);
var
  ActiveEditor : IEditor;
begin
  ActiveEditor := GetActiveEditor;
  if not Assigned(ActiveEditor) then Exit;

  var Py := GI_PyControl.SafePyEngine;
  var PyModule := PyControl.ActiveInterpreter.ImportModule(ActiveEditor, True);
  VarClear(PyModule);

  GI_PyIDEServices.Messages.AddMessage(Format(_(SModuleImportedOK), [ActiveEditor.FileTitle]));
  ShowDockForm(MessagesWindow);
end;

procedure TPyIDEMainForm.actToggleBreakPointExecute(Sender: TObject);
var
  ActiveEditor : IEditor;
begin
  ActiveEditor := GetActiveEditor;
  if Assigned(ActiveEditor) and ActiveEditor.HasPythonFile then
    PyControl.ToggleBreakpoint(ActiveEditor, ActiveEditor.SynEdit.CaretY);
end;

procedure TPyIDEMainForm.actClearAllBreakpointsExecute(Sender: TObject);
begin
  PyControl.ClearAllBreakpoints;
end;

procedure TPyIDEMainForm.actCommandLineExecute(Sender: TObject);
begin
  TCommandLineDlg.Execute;
end;

procedure TPyIDEMainForm.actRunDebugLastScriptExecute(Sender: TObject);
begin
  if GI_PyControl.Inactive then
    PyControl.Debug(PyControl.RunConfig);
end;

procedure TPyIDEMainForm.actRunExecute(Sender: TObject);
var
  ActiveEditor : IEditor;
  RunConfig : TRunConfiguration;
begin
  Application.ProcessMessages;
  ActiveEditor := GetActiveEditor;
  if not Assigned(ActiveEditor) then Exit;

  RunConfig :=  TRunConfiguration.Create;
  try
    SetupRunConfiguration(RunConfig, ActiveEditor);
    PyControl.Run(RunConfig);
  finally
    RunConfig.Free;
  end;

  WriteStatusMsg(_(StrScriptRunOK));
end;

procedure TPyIDEMainForm.actRunLastScriptExecute(Sender: TObject);
begin
  if GI_PyControl.Inactive then
    PyControl.Run(PyControl.RunConfig);
end;

procedure TPyIDEMainForm.actRunLastScriptExternalExecute(Sender: TObject);
begin
  PyControl.ExternalRun(PyControl.RunConfig);
end;

procedure TPyIDEMainForm.actDebugExecute(Sender: TObject);
var
  ActiveEditor : IEditor;
begin
  Assert(GI_PyControl.PythonLoaded and not GI_PyControl.Running);
  ActiveEditor := GetActiveEditor;
  if Assigned(ActiveEditor) then begin
    if GI_PyControl.Inactive then
      DebugActiveScript(ActiveEditor)
    else if PyControl.DebuggerState = dsPaused then
      PyControl.ActiveDebugger.Resume;
  end;
end;

procedure TPyIDEMainForm.actDebugPauseExecute(Sender: TObject);
begin
  PyControl.ActiveDebugger.Pause;
end;

procedure TPyIDEMainForm.actSelectStyleExecute(Sender: TObject);
begin
   TStyleSelectorForm.Execute;
end;

procedure TPyIDEMainForm.actStepIntoExecute(Sender: TObject);
var
  ActiveEditor : IEditor;
begin
  Assert(GI_PyControl.PythonLoaded and not GI_PyControl.Running);
  ActiveEditor := GetActiveEditor;
  if Assigned(ActiveEditor) then begin
    if GI_PyControl.Inactive then
      DebugActiveScript(ActiveEditor, True)
    else if PyControl.DebuggerState = dsPaused then
      PyControl.ActiveDebugger.StepInto;
  end;
end;

procedure TPyIDEMainForm.actStepOverExecute(Sender: TObject);
begin
  PyControl.ActiveDebugger.StepOver;
end;

procedure TPyIDEMainForm.actStepOutExecute(Sender: TObject);
begin
  PyControl.ActiveDebugger.StepOut;
end;

procedure TPyIDEMainForm.actDebugAbortExecute(Sender: TObject);
begin
  PyControl.ActiveDebugger.Abort;
end;

procedure TPyIDEMainForm.actRunToCursorExecute(Sender: TObject);
var
  ActiveEditor : IEditor;
begin
  Application.ProcessMessages;
  ActiveEditor := GetActiveEditor;
  if Assigned(ActiveEditor) then begin
    if GI_PyControl.Inactive then
      DebugActiveScript(ActiveEditor, False, ActiveEditor.SynEdit.CaretY)
    else if PyControl.DebuggerState = dsPaused then
      PyControl.ActiveDebugger.RunToCursor(ActiveEditor, ActiveEditor.SynEdit.CaretY);
  end;
end;

procedure TPyIDEMainForm.DebuggerBreakpointChange(Sender: TObject; Editor : IEditor;
  ALine: integer);
begin
  if not Assigned(Editor) then Exit;
  if (ALine >= 1) and (ALine <= Editor.SynEdit.Lines.Count) then
  begin
    Editor.SynEdit.InvalidateGutterLine(ALine);
    Editor.SynEdit.InvalidateLine(ALine);
    Editor.SynEdit2.InvalidateGutterLine(ALine);
    Editor.SynEdit2.InvalidateLine(ALine);
  end
  else
    Editor.SynEdit.Invalidate;

  TThread.ForceQueue(nil, procedure
  begin
    BreakPointsWindow.UpdateWindow;
  end);
end;

procedure TPyIDEMainForm.UpdateCaption;
begin
  if GetIsClosing then Exit;

  TThread.ForceQueue(nil, procedure
    begin
      var Editor := GetActiveEditor;
      if Assigned(Editor) then
        Caption := Format('PyScripter - %s%s', [Editor.FileId,
                             iff(Editor.Modified, '*', '')])
      else
        Caption := 'PyScripter';
    end);
end;

procedure TPyIDEMainForm.SetupRunConfiguration(var RunConfig: TRunConfiguration; ActiveEditor: IEditor);
begin
  RunConfig.ScriptName := ActiveEditor.FileId;
  RunConfig.EngineType := PyControl.PythonEngineType;
  RunConfig.Parameters := CommandLineParams;
  RunConfig.ExternalRun.Assign(ExternalPython);
  RunConfig.ExternalRun.Parameters := Parameters.ReplaceInText(RunConfig.ExternalRun.Parameters);
  RunConfig.ReinitializeBeforeRun := PyIDEOptions.ReinitializeBeforeRun;
  RunConfig.WorkingDir := '';
end;

procedure TPyIDEMainForm.DebugActiveScript(ActiveEditor: IEditor;
  InitStepIn : Boolean = False; RunToCursorLine : integer = -1);
var
  RunConfig: TRunConfiguration;
begin
  Assert(GI_PyControl.Inactive);
  RunConfig := TRunConfiguration.Create;
  try
    SetupRunConfiguration(RunConfig, ActiveEditor);
    PyControl.Debug(RunConfig, InitStepIn, RunToCursorLine);
  finally
    RunConfig.Free;
  end;
end;

procedure TPyIDEMainForm.UpdateDebugCommands(DebuggerState : TDebuggerState);
var
  Editor : IEditor;
  PyFileActive : boolean;
begin
  Editor := GetActiveEditor;
  PyFileActive := Assigned(Editor) and
    (Editor.SynEdit.Highlighter = ResourcesDataModule.SynPythonSyn);

  actSyntaxCheck.Enabled := PyFileActive and GI_PyControl.Inactive;
  actRun.Enabled := PyFileActive and GI_PyControl.Inactive;
  actExternalRun.Enabled := PyFileActive and GI_PyControl.Inactive;
  actImportModule.Enabled := PyFileActive and GI_PyControl.Inactive;
  actDebug.Enabled := PyFileActive and (GI_PyControl.Inactive or (DebuggerState = dsPaused));
  actStepInto.Enabled := PyFileActive and (GI_PyControl.Inactive or (DebuggerState = dsPaused));
  actStepOut.Enabled := DebuggerState = dsPaused;
  actStepOver.Enabled := DebuggerState = dsPaused;
  actDebugAbort.Enabled := DebuggerState in [dsPaused, dsDebugging, dsRunning, dsPostMortem];
  actDebugPause.Enabled := DebuggerState = dsDebugging;
  actRunToCursor.Enabled := PyFileActive and (GI_PyControl.Inactive or (DebuggerState = dsPaused))
    and PyControl.IsExecutableLine(Editor, Editor.SynEdit.CaretY);
  actToggleBreakPoint.Enabled := PyFileActive;
  actClearAllBreakPoints.Enabled := PyFileActive;
  actAddWatchAtCursor.Enabled := PyFileActive;
  actExecSelection.Enabled := GI_PyControl.PythonLoaded and not GI_PyControl.Running and PyFileActive;
  actPythonReinitialize.Enabled := Assigned(PyControl.ActiveInterpreter) and
    (icReInitialize in PyControl.ActiveInterpreter.InterpreterCapabilities) and
    not (PyControl.DebuggerState in [dsPaused, dsPostMortem]);
  actPostMortem.Enabled := GI_PyControl.Inactive and
    Assigned(PyControl.ActiveDebugger) and PyControl.ActiveDebugger.PostMortemEnabled;
  if DebuggerState = dsPaused then begin
    actDebug.Caption := _(SResumeCaption);
    actDebug.Hint := _(SResumeHint);
  end else begin
    actDebug.Caption := _('Debug');
    actDebug.Hint := _(SDebugHint);
  end;
  actRunLastScript.Enabled := GI_PyControl.Inactive and (PyControl.RunConfig.ScriptName <> '');
  actRunDebugLastScript.Enabled := actRunLastScript.Enabled;
  actRunLastScriptExternal.Enabled := actRunLastScript.Enabled;

  CallStackWindow.actPreviousFrame.Enabled := (DebuggerState = dsPaused);
  CallStackWindow.actNextFrame.Enabled := (DebuggerState = dsPaused);

  //Refresh;
end;

procedure TPyIDEMainForm.SetActiveTabControl(const Value: TSpTBXCustomTabControl);
begin
  ActiveTabControlIndex := TabControlIndex(Value);
end;

procedure TPyIDEMainForm.SetRunLastScriptHints(const ScriptName: string);
Var
  S : string;
begin
   S := TPath.GetFileName(ScriptName);
   if S <> '' then
     S := Format(' - %s ', [S]);
   actRunLastScript.Hint := _(sHintRun) + S;
   actRunDebugLastScript.Hint := _(sHintDebug) + S;
   actRunLastScriptExternal.Hint := _(sHintExternalRun) + S;
end;

procedure TPyIDEMainForm.DebuggerErrorPosChange(Sender: TObject;
  const OldPos, NewPos: TEditorPos);
{  Invalidates old and/or new error line but does not Activate the Editor }
begin
  if GetIsClosing then Exit;

  if Assigned(OldPos.Editor)  and (OldPos.Line > 0) then begin
    // Remove possible error line
    OldPos.Editor.SynEdit.InvalidateLine(OldPos.Line);
    OldPos.Editor.SynEdit2.InvalidateLine(OldPos.Line);
  end;
  if Assigned(NewPos.Editor)  and (NewPos.Line > 0) then begin
    NewPos.Editor.SynEdit.InvalidateLine(NewPos.Line);
    NewPos.Editor.SynEdit2.InvalidateLine(NewPos.Line);
  end;
end;

procedure TPyIDEMainForm.DebuggerCurrentPosChange(Sender: TObject;
  const OldPos, NewPos: TEditorPos);
begin
  if GetIsClosing then Exit;

  if Assigned(OldPos.Editor)  and (OldPos.Line > 0) then
    // Remove possible current lines
    with OldPos.Editor do begin
      SynEdit.InvalidateGutterLine(OldPos.Line);
      SynEdit.InvalidateLine(OldPos.Line);
      SynEdit2.InvalidateGutterLine(OldPos.Line);
      SynEdit2.InvalidateLine(OldPos.Line);
    end;

  if not Assigned(NewPos.Editor) then Exit;

  if GetActiveEditor <> NewPos.Editor then
    NewPos.Editor.Activate;
  with NewPos.Editor.SynEdit do begin
    if (NewPos.Line > 0) and (CaretY <> NewPos.Line) then begin
      CaretXY := BufferCoord(1, NewPos.Line);
      EnsureCursorPosVisible;
    end;
    InvalidateGutterLine(NewPos.Line);
    InvalidateLine(NewPos.Line);
  end;
  NewPos.Editor.SynEdit2.InvalidateGutterLine(NewPos.Line);
  NewPos.Editor.SynEdit2.InvalidateLine(NewPos.Line);
end;

procedure TPyIDEMainForm.DebuggerStateChange(Sender: TObject; OldState,
  NewState: TDebuggerState);
var
  s: string;
begin
  if GetIsClosing then Exit;

  if GI_PyControl.PythonLoaded then
    case NewState of
      dsDebugging,
      dsRunning: begin
                   s := _('Running');
                   icIndicators.SVGIconItems[0].FixedColor := $4444E2;
                 end;
      dsPaused: begin
                  s := _('Paused');
                  icIndicators.SVGIconItems[0].FixedColor := $00CEFF;
                end;
      dsInactive: begin
                    s := _('Ready');
                    icIndicators.SVGIconItems[0].FixedColor := $22AA22;
                  end;
      dsPostMortem : begin
                       s := _('Post mortem');
                       icIndicators.SVGIconItems[0].FixedColor := clPurple;
                     end;
    end
  else
  begin
    s := _('Python not available');
    icIndicators.SVGIconItems[0].FixedColor := clGray;
  end;
  spiStatusLED.Hint := _('Debugger state: ') + s;
  lbStatusMessage.Caption := ' ' + s;
  StatusBar.Refresh;

  CallStackWindow.UpdateWindow(NewState, OldState);  // also updates Variables and Watches
  UpdateDebugCommands(NewState);
end;

procedure TPyIDEMainForm.SaveFileModules;
begin
  GI_EditorFactory.ApplyToEditors(procedure(Editor: IEditor)
  begin
    if ((Editor.FileName <> '') or (Editor.RemoteFileName <> ''))
      and Editor.Modified then
    begin
      var FileCommands := Editor as IFileCommands;
      if Assigned(FileCommands) then
        FileCommands.ExecSave;
    end;
  end);
end;

procedure TPyIDEMainForm.ApplicationOnIdle(Sender: TObject; var Done: Boolean);
Var
  i : integer;
begin
  UpdateStandardActions;
  CommandsDataModule.UpdateMainActions;
  PythonIIForm.UpdateInterpreterActions;
  UpdateStatusBarPanels;
  UpdateDebugCommands(PyControl.DebuggerState);
  if Assigned(GI_ActiveEditor) then
    TEditorForm(GI_ActiveEditor.Form).DoOnIdle;

  // If a Tk or Wx remote engine is active pump up event handling
  // This is for processing input output coming from event handlers
  if (PyControl.ActiveInterpreter is TPyRemoteInterpreter) and
     (GI_PyControl.Inactive)
  then
    with(TPyRemoteInterpreter(PyControl.ActiveInterpreter)) do begin
      if Connected and (EngineType in [peRemoteTk, peRemoteWx]) then
      try
        // Ignore exceptions here
        var Py := GI_PyControl.SafePyEngine;
        ServeConnection;
      except
      end;
    end;

  if ShellExtensionFiles.Count > 0 then begin
    for i := 0 to ShellExtensionFiles.Count - 1 do
      OpenCmdLineFile(ShellExtensionFiles[i]);
    ShellExtensionFiles.Clear;
  end;

  if not Application.Active then
  begin
    if CommandsDataModule.SynCodeCompletion.Form.Visible then
      CommandsDataModule.SynCodeCompletion.CancelCompletion;
    if CommandsDataModule.SynParamCompletion.Form.Visible then
      CommandsDataModule.SynParamCompletion.CancelCompletion;
  end;

  Done := True;
end;

procedure TPyIDEMainForm.ApplicationOnHint(Sender: TObject);
Var
  S : string;
begin
  S := StringReplace(GetLongHint(Application.Hint), #13#10, ' ', [rfReplaceAll]);
  WriteStatusMsg(S);
end;

procedure TPyIDEMainForm.ApplcationOnShowHint(var HintStr: string;
  var CanShow: Boolean; var HintInfo: Vcl.Controls.THintInfo);
begin
  if HintInfo.HintControl is TBaseVirtualTree then
    HintInfo.HideTimeout := 5000;
end;

function TPyIDEMainForm.ShowFilePosition(FileName : string; Line: integer = 0;
      Offset : integer = 1; SelLen : integer = 0;
      ForceToMiddle : Boolean = True; FocusEditor : Boolean = True): Boolean;
Var
  Editor : IEditor;
begin
  Result := False;
  if FileName <> '' then begin
    if (FileName[1] ='<') and (FileName[Length(FileName)] = '>') then
      FileName :=  Copy(FileName, 2, Length(FileName)-2);
    Editor := GI_EditorFactory.GetEditorByFileId(FileName);
    if not Assigned(Editor) and (FileName.StartsWith('ssh') or FileExists(FileName)) then begin
      try
        DoOpenFile(FileName, '', TabControlIndex(ActiveTabControl));
      except
        Exit;
      end;
      Editor := GI_EditorFactory.GetEditorByFileId(FileName);

      if GI_PyControl.PythonLoaded and
        Editor.FileName.StartsWith(PyControl.PythonVersion.InstallPath, True)
      then
        Editor.ReadOnly := True;
    end;
    if Assigned(Editor) then begin
      Result := True;
      // to deal with focus problems
      TThread.ForceQueue(nil, procedure
      begin
        // sets the focus to the editor
        if (Editor <> GetActiveEditor) or FocusEditor then
          Editor.Activate(False);
        if (Line > 0) then
          with Editor.ActiveSynEdit do begin
            CaretXY := BufferCoord(Offset,Line);
            EnsureCursorPosVisibleEx(ForceToMiddle);
            if SelLen > 0 then
               SelLength := SelLen;
          end;
      end);
    end;
  end;
end;

procedure TPyIDEMainForm.SpTBXCustomizerGetCustomizeForm(Sender: TObject;
  var CustomizeFormClass: TSpTBXCustomizeFormClass);
begin
  CustomizeFormClass := TSpTBXCustomizeFormMod;
end;

procedure TPyIDEMainForm.actViewSplitEditorHorExecute(Sender: TObject);
Var
  Editor : IEditor;
begin
  Editor := GetActiveEditor;
  if Assigned(Editor) then
    Editor.SplitEditorHorizontally
end;

procedure TPyIDEMainForm.actViewSplitEditorVerExecute(Sender: TObject);
Var
  Editor : IEditor;
begin
  Editor := GetActiveEditor;
  if Assigned(Editor) then
    Editor.SplitEditorVertrically;
end;

procedure TPyIDEMainForm.actViewSplitWorkspaceHorExecute(Sender: TObject);
begin
  SplitWorkspace(True, alBottom);
end;

procedure TPyIDEMainForm.actViewSplitWorkspaceVerExecute(Sender: TObject);
begin
  SplitWorkspace(True, alRight);
end;

procedure TPyIDEMainForm.actViewStatusBarExecute(Sender: TObject);
begin
  StatusBar.Visible := not StatusBar.Visible;
  //This is to avoid the Status bar appearing above Bottom Dock Tab
  if StatusBar.Visible then
    StatusBar.Top := Height - StatusBar.Height;
end;

procedure TPyIDEMainForm.actViewIIExecute(Sender: TObject);
begin
  if not PythonIIForm.Visible then
    ShowDockForm(PythonIIForm)
  else
    HideDockForm(PythonIIForm);
end;

procedure TPyIDEMainForm.actViewMainMenuExecute(Sender: TObject);
begin
  MainMenu.Visible := not MainMenu.Visible;
end;

procedure TPyIDEMainForm.actViewCodeExplorerExecute(Sender: TObject);
begin
  if not CodeExplorerWindow.Visible then
    ShowDockForm(CodeExplorerWindow)
  else
    HideDockForm(CodeExplorerWindow);
end;

procedure TPyIDEMainForm.actViewCustomizeToolbarsExecute(Sender: TObject);
begin
  SpTBXCustomizer.Show;
end;

procedure TPyIDEMainForm.actViewFileExplorerExecute(Sender: TObject);
begin
  if not FileExplorerWindow.Visible then
    ShowDockForm(FileExplorerWindow)
  else
    HideDockForm(FileExplorerWindow);
end;

procedure TPyIDEMainForm.actViewToDoListExecute(Sender: TObject);
begin
  if not ToDoWindow.Visible then begin
    ShowDockForm(ToDoWindow);
    ToDoWindow.ToDoView.SetFocus;
  end else
    HideDockForm(ToDoWindow);
end;

procedure TPyIDEMainForm.actViewRegExpTesterExecute(Sender: TObject);
begin
  if not RegExpTesterWindow.Visible then
    ShowDockForm(RegExpTesterWindow)
  else
    HideDockForm(RegExpTesterWindow);
end;

procedure TPyIDEMainForm.actViewUnitTestsExecute(Sender: TObject);
begin
  if not UnitTestWindow.Visible then
    ShowDockForm(UnitTestWindow)
  else
    HideDockForm(UnitTestWindow);
end;

procedure TPyIDEMainForm.actViewOutputExecute(Sender: TObject);
begin
  if not OutputWindow.Visible then
    ShowDockForm(OutputWindow)
  else
    HideDockForm(OutputWindow);
end;

procedure TPyIDEMainForm.actViewProjectExplorerExecute(Sender: TObject);
begin
  if not ProjectExplorerWindow.Visible then
    ShowDockForm(ProjectExplorerWindow)
  else
    HideDockForm(ProjectExplorerWindow);
end;

procedure TPyIDEMainForm.actViewFindResultsExecute(Sender: TObject);
begin
  if not FindResultsWindow.Visible then begin
    ShowDockForm(FindResultsWindow);
  end else
    HideDockForm(FindResultsWindow);
end;

procedure TPyIDEMainForm.actViewHideSecondaryWorkspaceExecute(Sender: TObject);
var
  I: Integer;
  IV: TTBItemViewer;
  List : TObjectList;
begin
  // Move all tabs to TabControl1
  // Note that the Pages property may have a different order than the
  // physical order of the tabs
  TabControl1.Toolbar.BeginUpdate;
  TabControl2.Toolbar.BeginUpdate;
  List := TObjectList.Create(False);
  try
    for I := 0 to TabControl2.View.ViewerCount - 1 do begin
      IV := TabControl2.View.Viewers[I];
      if IV.Item is TSpTBXTabItem then
        List.Add(IV.Item)
    end;

    for i := 0 to List.Count - 1 do
      MoveTab(TSpTBXTabItem(List[I]), TabControl1);
  finally
    TabControl1.Toolbar.EndUpdate;
    TabControl2.Toolbar.EndUpdate;
    List.Free;
  end;

  SplitWorkspace(False);
end;

procedure TPyIDEMainForm.actViewHideSecondEditorExecute(Sender: TObject);
Var
  Editor : IEditor;
begin
  Editor := GetActiveEditor;
  if Assigned(Editor) then with TEditorForm(Editor.Form) do begin
    EditorSplitter.Visible:= False;
    SynEdit2.Visible := False;
  end;
end;

procedure TPyIDEMainForm.actMessagesWinExecute(Sender: TObject);
begin
  if not MessagesWindow.Visible then
    ShowDockForm(MessagesWindow)
  else
    HideDockForm(MessagesWindow);
end;

procedure TPyIDEMainForm.actCallStackWinExecute(Sender: TObject);
begin
  if not CallStackWindow.Visible then
    ShowDockForm(CallStackWindow)
  else
    HideDockForm(CallStackWindow);
end;

procedure TPyIDEMainForm.actVariablesWinExecute(Sender: TObject);
begin
  if not VariablesWindow.Visible then
    ShowDockForm(VariablesWindow)
  else
    HideDockForm(VariablesWindow);
end;

procedure TPyIDEMainForm.actAddWatchAtCursorExecute(Sender: TObject);
var
  Editor : IEditor;
begin
  Editor := GetActiveEditor;
  if Assigned(Editor) then
    TEditorForm(Editor.Form).AddWatchAtCursor;
end;

procedure TPyIDEMainForm.actBreakPointsWinExecute(Sender: TObject);
begin
  if not BreakPointsWindow.Visible then
    ShowDockForm(BreakPointsWindow)
  else
    HideDockForm(BreakPointsWindow);
end;

procedure TPyIDEMainForm.actWatchesWinExecute(Sender: TObject);
begin
  if not WatchesWindow.Visible then
    ShowDockForm(WatchesWindow)
  else
    HideDockForm(WatchesWindow);
end;

function TPyIDEMainForm.GetActiveEditor : IEditor;
{
  Returns the active editor irrespective of whether it is has the focus
  If want the active editor with focus then use GI_ActiveEditor
}
Var
  ActivePage : TSpTBXTabSheet;
begin
  // Find Active Page
  ActivePage := ActiveTabControl.ActivePage;
  if not Assigned(ActivePage) then begin
    ActivePage := TabControl1.ActivePage;
    if not Assigned(ActivePage) then
      ActivePage := TabControl2.ActivePage;
  end;

  if Assigned(ActivePage) and (ActivePage.ComponentCount > 0) and
    (ActivePage.Components[0] is TEditorForm) then
    Result := TEditorForm(ActivePage.Components[0]).GetEditor
  else
    Result := nil;
end;

function TPyIDEMainForm.GetActiveTabControl: TSpTBXCustomTabControl;
begin
   if ActiveTabControlIndex = 2 then
     Result := TabControl2
   else
     Result := TabControl1;
end;

function TPyIDEMainForm.GetAppStorage: TJvCustomAppStorage;
begin
  Result := AppStorage;
end;

function TPyIDEMainForm.GetIDELayouts: IIDELayouts;
begin
  Result := Self;
end;

function TPyIDEMainForm.GetIsClosing: Boolean;
begin
  // Use Application.OnHelp to signal exit
  // Application.Help is set to nil as soon as we are about to close
  Result := not Assigned(Application.OnHelp);
end;

function TPyIDEMainForm.GetLocalAppStorage: TJvCustomAppStorage;
begin
  Result := LocalAppStorage;
end;

function TPyIDEMainForm.GetLogger: TJclSimpleLog;
begin
  Result := ResourcesDataModule.Logger;
end;

function TPyIDEMainForm.GetMessageServices: IMessageServices;
begin
  Result := MessagesWindow;
end;

function TPyIDEMainForm.GetStoredScript(const Name: string): TStrings;
begin
  Result := ResourcesDataModule.PythonScripts.StringsByName[Name];
  Result.WriteBOM := False;
end;

function TPyIDEMainForm.GetUnitTestServices: IUnitTestServices;
begin
  Result := UnitTestWindow;
end;

function TPyIDEMainForm.TabControl(TabControlIndex: integer): TSpTBXTabControl;
begin
  if TabControlIndex = 2 then
    Result := TabControl2
  else
    Result := TabControl1;
end;

function TPyIDEMainForm.TabControlIndex(
  TabControl: TSpTBXCustomTabControl): integer;
begin
  if TabControl = TabControl2 then
    Result := 2
  else
    Result := 1;
end;

procedure TPyIDEMainForm.UpdateStatusBarPanels;
var
  ptCaret: TPoint;
  Editor : IEditor;
begin
  Editor := GI_ActiveEditor;
  if Editor <> nil then begin
    ptCaret := Editor.GetCaretPos;
    if (ptCaret.X > 0) and (ptCaret.Y > 0) then
      lbStatusCaret.Caption := Format('%d:%d', [ptCaret.Y, ptCaret.X])
    else
      lbStatusCaret.Caption := '';
    if GI_ActiveEditor.GetModified then
      lbStatusModified.Caption := _(SModified)
    else
      lbStatusModified.Caption := ' ';
    lbStatusOverwrite.Caption := Editor.GetEditorState;
  end else begin
    lbStatusCaret.Caption := '';
    lbStatusModified.Caption := '';
    lbStatusOverwrite.Caption := '';
  end;
  if GetCapsLockKeyState then
    lbStatusCAPS.Caption := 'CAPS'
  else
    lbStatusCAPS.Caption := ' ';

  if GI_PyControl.PythonLoaded then begin
    lbPythonVersion.Caption := PyControl.PythonVersion.DisplayName;
    lbPythonEngine.Caption := _(EngineTypeName[PyControl.PythonEngineType]);
  end else begin
    lbPythonVersion.Caption := _('Python Not Available');
    lbPythonEngine.Caption := ' ';
  end;

  if TJedi.Ready then begin
    spiLspLed.Hint := _('Language Server') + ': ' + _('Ready');
    icIndicators.SVGIconItems[2].FixedColor := $1F5FFF;
  end else begin
    spiLspLed.Hint := _('Language Server') + ': ' + _('Not available');
    icIndicators.SVGIconItems[2].FixedColor := clGray;
  end;

  spiExternalToolsLED.Visible := OutputWindow.IsRunning;
end;

function TPyIDEMainForm.CmdLineOpenFiles(): boolean;
var
  i : integer;
begin
  Result := False;
  for i := Low(CmdLineReader.readNamelessString) to High(CmdLineReader.readNamelessString) do
    Result := OpenCmdLineFile(CmdLineReader.readNamelessString[i]) or Result;

  // Project Filename
  if CmdLineReader.readString('PROJECT') <> '' then
    ProjectExplorerWindow.DoOpenProjectFile(CmdLineReader.readString('PROJECT'));
end;

procedure TPyIDEMainForm.CreateWnd;
begin
  inherited;
  WTSRegisterSessionNotification(Handle, NOTIFY_FOR_THIS_SESSION);
end;

procedure TPyIDEMainForm.OpenInitialFiles;
begin
  TabControl1.Toolbar.BeginUpdate;
  TabControl2.Toolbar.BeginUpdate;
  try
    // Open Files on the command line
    // if there was no file on the command line try restoring open files
    if not CmdLineOpenFiles and PyIDEOptions.RestoreOpenFiles then
      TPersistFileInfo.ReadFromAppStorage(AppStorage, 'Open Files');

    //Recovered files
    GI_EditorFactory.RecoverFiles;

    // If we still have no open file then open an empty file
    if GI_EditorFactory.GetEditorCount = 0 then
      actFileNewModuleExecute(Self);
  finally
    TabControl1.Toolbar.EndUpdate;
    TabControl2.Toolbar.EndUpdate;
    if Assigned(TabControl1.ActiveTab) then
      TabControl1.MakeVisible(TabControl1.ActiveTab);
    if Assigned(TabControl2.ActiveTab) then
      TabControl2.MakeVisible(TabControl2.ActiveTab);
    if Assigned(GetActiveEditor()) then
      GetActiveEditor.Activate;
    UpdateCaption;
  end;
end;

procedure TPyIDEMainForm.FormDestroy(Sender: TObject);
begin
  GI_PyIDEServices := nil;
  SkinManager.RemoveSkinNotification(Self);
  PyIDEOptions.OnChange.RemoveHandler(PyIDEOptionsChanged);
  FreeAndNil(Layouts);
  FreeAndNil(fLanguageList);
  FreeAndNil(DSAAppStorage);
  FreeAndNil(ShellExtensionFiles);
end;

procedure TPyIDEMainForm.actFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TPyIDEMainForm.actFileNewModuleExecute(Sender: TObject);
Var
  TemplateName : string;
  FileTemplate : TFileTemplate;
begin
  FileTemplate := nil;
  TemplateName := PyIDEOptions.FileTemplateForNewScripts;
  if TemplateName <> '' then
    FileTemplate := FileTemplates.TemplateByName(TemplateName);

  if Assigned(FileTemplate) then
    NewFileFromTemplate(FileTemplate, TabControlIndex(ActiveTabControl))
  else
    DoOpenFile('', 'Python', TabControlIndex(ActiveTabControl));
end;

procedure TPyIDEMainForm.actFileOpenExecute(Sender: TObject);
Var
  i : integer;
  Editor : IEditor;
begin
  with ResourcesDataModule.dlgFileOpen do begin
    Title := _(SOpenFile);
    FileName := '';
    Filter := ResourcesDataModule.Highlighters.FileFilters + _(SFilterAllFiles);
    Editor := GetActiveEditor;
    if Assigned(Editor) and (Editor.FileName <> '') and
      (TPath.GetDirectoryName(Editor.FileName) <> '')
    then
      InitialDir := TPath.GetDirectoryName(Editor.FileName);

    Options := Options + [ofAllowMultiSelect];
    if Execute then begin
      for i := 0 to Files.Count - 1 do
        DoOpenFile(Files[i], '', TabControlIndex(ActiveTabControl));
    end;
    Options := Options - [ofAllowMultiSelect];
  end;
end;

procedure TPyIDEMainForm.actFileCloseAllExecute(Sender: TObject);
begin
  if GI_EditorFactory <> nil then begin
    if GI_EditorFactory.CanCloseAll then begin
      TabControl1.Toolbar.BeginUpdate;
      TabControl2.Toolbar.BeginUpdate;
      try
        GI_EditorFactory.CloseAll;
      finally
        TabControl1.Toolbar.EndUpdate;
        TabControl2.Toolbar.EndUpdate;
        UpdateCaption;
      end;
    end;
  end;
end;

procedure TPyIDEMainForm.PyIDEOptionsChanged;
begin
  Application.DefaultFont.Size := PyIDEOptions.UIContentFontSize;

  if PyIDEOptions.StyleMainWindowBorder then
    Self.StyleElements := Self.StyleElements + [seBorder]
  else
    Self.StyleElements := Self.StyleElements - [seBorder];

  MaskFPUExceptions(PyIDEOptions.MaskFPUExceptions);

  // Set Python engine
  actPythonInternal.Visible := not PyIDEOptions.InternalInterpreterHidden;
  if not actPythonInternal.Visible and
     (PyIDEOptions.PythonEngineType = peInternal)
  then
    PyIDEOptions.PythonEngineType := peRemote;

  PyControl.PythonEngineType := PyIDEOptions.PythonEngineType;

  if PyIDEOptions.ShowTabCloseButton then begin
    TabControl1.TabCloseButton := tcbAll;
    TabControl2.TabCloseButton := tcbAll;
  end else begin
    TabControl1.TabCloseButton := tcbNone;
    TabControl2.TabCloseButton := tcbNone;
  end;

  if TabControl1.TabPosition <> PyIDEOptions.EditorsTabPosition then
  begin
    if PyIDEOptions.EditorsTabPosition = ttpTop then
    begin
      TabControl1.TabPosition := ttpTop;
      TabControl2.TabPosition := ttpTop;
    end
    else  //ttpBottom:
    begin
      TabControl1.TabPosition := ttpBottom;
      TabControl2.TabPosition := ttpBottom;
    end;
  end;

  if not PyIDEOptions.AutoRestart then
    UnregisterApplicationRestart;

  tbiRecentFileList.MaxItems :=  PyIDEOptions.NoOfRecentFiles;
end;

procedure TPyIDEMainForm.StoreApplicationData;
begin
  var TempCursor := WaitCursor;
  var TempStringList := TSmartPtr.Make(TStringList.Create)();

  AppStorage.BeginUpdate;
  try
    AppStorage.StorageOptions.SetAsString := True;
    AppStorage.StorageOptions.StoreDefaultValues := True;
    AppStorage.WritePersistent('IDE Options', PyIDEOptions);
    AppStorage.StorageOptions.StoreDefaultValues := False;

    AppStorage.WriteString('PyScripter Version', ApplicationVersion);
    AppStorage.WriteString('Language', GetCurrentLanguage);

    TempStringList.AddStrings(['TrackChanges', 'SelectedColor', 'IndentGuides']);
    AppStorage.DeleteSubTree('Editor Options');
    AppStorage.WritePersistent('Editor Options', EditorOptions, True, TempStringList);

    AppStorage.WritePersistent('Editor Search Options', EditorSearchOptions);

    AppStorage.DeleteSubTree('Highlighters');
    // Store dock form settings
    for var I := 0 to Screen.FormCount - 1 do
      if Screen.Forms[I] is TIDEDockWindow then
        TIDEDockWindow(Screen.Forms[I]).StoreSettings(AppStorage);

    // Store Highlighters
    for var Highlighter in ResourcesDataModule.Highlighters do
      if ResourcesDataModule.IsHighlighterStored(Highlighter) then
        AppStorage.WritePersistent('Highlighters\' +
          Highlighter.FriendlyLanguageName, Highlighter);

    with CommandsDataModule do
    begin
      TempStringList.Clear;
      TempStringList.AddStrings(['Lines', 'Highlighter']);
      AppStorage.DeleteSubTree('Print Options');
      AppStorage.WritePersistent('Print Options', SynEditPrint, True, TempStringList);
      AppStorage.WriteString('Print Options\HeaderItems', SynEditPrint.Header.AsString);
      AppStorage.WriteString('Print Options\FooterItems', SynEditPrint.Footer.AsString);
    end;

    // File and Code Templates - Preserve leading-trailing whitespace
    AppStorage.StorageOptions.PreserveLeadingTrailingBlanks := True;
    AppStorage.DeleteSubTree('File Templates');
    AppStorage.WriteObjectList('File Templates', FileTemplates);

    AppStorage.DeleteSubTree('Code Templates');
    AppStorage.WriteStringList('Code Templates',
      ResourcesDataModule.CodeTemplatesCompletion.AutoCompleteList);
    AppStorage.StorageOptions.PreserveLeadingTrailingBlanks := False;

    AppStorage.WritePersistent('Secondary Tabs', TabsPersistsInfo);

    AppStorage.WriteStringList('Custom Params', CustomParams);
    AppStorage.DeleteSubTree('Tools');
    AppStorage.WriteCollection('Tools', ToolsCollection, 'Tool');
    AppStorage.DeleteSubTree('SSH');
    AppStorage.StorageOptions.StoreDefaultValues := True;
    AppStorage.WriteCollection('SSH', SSHServers, 'Server');
    AppStorage.StorageOptions.StoreDefaultValues := False;
    AppStorage.WritePersistent('Tools\External Run', ExternalPython);
    AppStorage.WriteBoolean('Status Bar', StatusBar.Visible);

    // Save Style Name
    AppStorage.WriteString('Style Name', TStyleSelectorForm.CurrentSkinName);

    // Save Toolbar Items
    SaveToolbarItems('Toolbar Items');

    //  Needed since save toolbar Items below does not save secondary shortcuts! Issue 307
    // Save IDE Shortcuts
    AppStorage.DeleteSubTree('IDE Shortcuts');
    var ActionProxyCollection := TActionProxyCollection.Create(apcctChanged);
    try
      AppStorage.WriteCollection('IDE Shortcuts', ActionProxyCollection, 'Action');
    finally
      ActionProxyCollection.Free;
    end;

    // Project Filename
    AppStorage.WriteString('Active Project', ActiveProject.FileName);

  finally
    AppStorage.EndUpdate;
  end;

  // Save MRU Lists
  tbiRecentFileList.SaveToIni(AppStorage.IniFile, 'MRU File List');
  tbiRecentProjects.SaveToIni(AppStorage.IniFile, 'MRU Project List');

  AppStorage.Flush;
end;

procedure TPyIDEMainForm.StoreLocalApplicationData;
begin
  LocalAppStorage.BeginUpdate;
  try
    LocalAppStorage.WriteString('PyScripter Version', ApplicationVersion);
    LocalAppStorage.WriteString('Monitor profile', MonitorProfile);

    LocalAppStorage.WriteStringList('Layouts', Layouts);

    // Store Python Versions
    PyControl.WriteToAppStorage(LocalAppStorage);
  finally
    LocalAppStorage.EndUpdate;
  end;
  LocalAppStorage.Flush;
end;

function TPyIDEMainForm.ReplaceParams(const AText: string): string;
begin
  Result := Parameters.ReplaceInText(AText);
end;

procedure TPyIDEMainForm.RestoreApplicationData;
Const
  DefaultHeader='$TITLE$\.1\.0\.-13\.Arial\.0\.96\.10\.0\.1\.2';
  DefaultFooter='$PAGENUM$\\.$PAGECOUNT$\.1\.0\.-13\.Arial\.0\.96\.10\.0\.1\.2';
begin
  var PyScripterVersion := AppStorage.ReadString('PyScripter Version', '1.0');
  AppStorage.StorageOptions.SetAsString :=
    CompareVersions(PyScripterVersion, '4.2.9') >= 0;

  // Change language
  ChangeLanguage(AppStorage.ReadString('Language', GetCurrentLanguage));

  // Remove since it is now stored in PyScripter.local.ini
  if AppStorage.PathExists('Layouts') then AppStorage.DeleteSubTree('Layouts');

  if AppStorage.PathExists('IDE Options') then begin
    AppStorage.ReadPersistent('IDE Options', PyIDEOptions);
    PyIDEOptions.Changed;
    AppStorage.DeleteSubTree('IDE Options');
  end;

  var TempStringList := TSmartPtr.Make(TStringList.Create)();
  if AppStorage.PathExists('Editor Options') then
  begin
    TempStringList.AddStrings(['TrackChanges', 'SelectedColor', 'IndentGuides']);
    AppStorage.ReadPersistent('Editor Options', EditorOptions, True, True, TempStringList);
    EditorOptions.Options := EditorOptions.Options + [eoBracketsHighlight, eoAccessibility];
  end;

  if AppStorage.PathExists('Editor Search Options') then begin
    AppStorage.ReadPersistent('Editor Search Options', EditorSearchOptions);
    tbiSearchText.Items.CommaText := EditorSearchOptions.SearchTextHistory;
    tbiReplaceText.Items.CommaText := EditorSearchOptions.ReplaceTextHistory;
  end;

  // Restore dock form settings
  for var I := 0 to Screen.FormCount - 1 do
    if Screen.Forms[I] is TIDEDockWindow then
      TIDEDockWindow(Screen.Forms[I]).RestoreSettings(AppStorage);

  // Restore highlighters
  for var Highlighter in ResourcesDataModule.Highlighters do
  begin
    Highlighter.BeginUpdate;
    try
      AppStorage.ReadPersistent('Highlighters\' +
        Highlighter.FriendlyLanguageName, Highlighter);
    finally
      Highlighter.EndUpdate;
    end;
  end;
  CommandsDataModule.ApplyEditorOptions;

  AppStorage.DeleteSubTree('Highlighters');

  with CommandsDataModule do begin
    AppStorage.ReadPersistent('Print Options', SynEditPrint);
    SynEditPrint.Header.AsString := AppStorage.ReadString('Print Options\HeaderItems', DefaultHeader);
    SynEditPrint.Footer.AsString := AppStorage.ReadString('Print Options\FooterItems', DefaultFooter);
  end;

  // File and Code Templates - Preserve leading-trailing whitespace
  AppStorage.StorageOptions.PreserveLeadingTrailingBlanks := True;
  if AppStorage.PathExists('File Templates') then
  begin
    AppStorage.ReadObjectList('File Templates', FileTemplates, FileTemplates.CreateListItem);
    FileTemplates.AddDefaultTemplates;
  end;

  if AppStorage.PathExists('Code Templates') then
    AppStorage.ReadStringList('Code Templates',
      ResourcesDataModule.CodeTemplatesCompletion.AutoCompleteList);
  AppStorage.StorageOptions.PreserveLeadingTrailingBlanks := False;

  AppStorage.ReadPersistent('Secondary Tabs', TabsPersistsInfo);

  AppStorage.ReadStringList('Custom Params', CustomParams);
  RegisterCustomParams;
  AppStorage.ReadCollection('Tools', ToolsCollection, True, 'Tool');
  AppStorage.ReadCollection('SSH', SSHServers, True, 'Server');
  AppStorage.ReadPersistent('Tools\External Run', ExternalPython);
  StatusBar.Visible := AppStorage.ReadBoolean('Status Bar');

  // Load Style Name
  TStyleSelectorForm.SetStyle(AppStorage.ReadString('Style Name', 'Windows10 SlateGray'));

  // Load IDE Shortcuts
  var ActionProxyCollection := TActionProxyCollection.Create(apcctEmpty);
  try
    AppStorage.ReadCollection('IDE Shortcuts', ActionProxyCollection, True, 'Action');
    ActionProxyCollection.ApplyShortCuts;
  finally
    ActionProxyCollection.Free;
  end;

  // Project Filename
  if (CmdLineReader.readString('PROJECT') = '') and PyIDEOptions.RestoreOpenProject then begin
    var FName := AppStorage.ReadString('Active Project');
    if FName <> '' then
      ProjectExplorerWindow.DoOpenProjectFile(FName);
  end;

  // Load MRU Lists
  tbiRecentFileList.LoadFromIni(AppStorage.IniFile, 'MRU File List');
  tbiRecentProjects.LoadFromIni(AppStorage.IniFile, 'MRU Project List');
end;

procedure TPyIDEMainForm.RestoreLocalApplicationData;
begin
  OldMonitorProfile := LocalAppStorage.ReadString('Monitor profile');

  LocalAppStorage.ReadStringList('Layouts', Layouts, True);
  if OldMonitorProfile <> MonitorProfile then begin
    LocalAppStorage.DeleteSubTree('Layouts\Default');
    if Layouts.IndexOf('Default') >= 0 then
      Layouts.Delete(Layouts.IndexOf('Default'));
    LocalAppStorage.DeleteSubTree('Layouts\Current');
  end;
end;

function TPyIDEMainForm.EditorFromTab(Tab : TSpTBXTabItem) : IEditor;
Var
  Sheet : TSpTBXTabSheet;
begin
  Result := nil;
  if Assigned(Tab) then begin
    Sheet := (Tab.Owner as TSpTBXTabControl).GetPage(Tab);
    if Assigned(Sheet) and (Sheet.ControlCount > 0) then
      Result := (Sheet.Controls[0] as TEditorForm).GetEditor;
  end;
end;

procedure TPyIDEMainForm.TabControlActiveTabChange(Sender: TObject;
  TabIndex: Integer);
Var
  Index : integer;
  TabCtrl : TSpTBXTabControl;
begin
  EditorSearchOptions.InitSearch;
  UpdateCaption;
  TabCtrl := Sender as TSpTBXTabControl;
  if Assigned(TabCtrl.ActivePage) and not GetIsClosing then
    // zOrder
    with TabCtrl do
      if not zOrderProcessing then begin
        Index := zOrder.IndexOf(TabCtrl.ActivePage.Item);
        if Index < 0 then
          zOrder.Insert(0, TabCtrl.ActivePage.Item)
        else
          zOrder.Move(Index, 0);
        zOrderPos := 0;
      end;
end;

procedure TPyIDEMainForm.TabControlMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  Editor : IEditor;
  IV: TTBItemViewer;
  TabItem : TSpTBXTabItem;
begin
  TabItem := nil;
  IV := (Sender as TSpTBXTabToolbar).View.ViewerFromPoint(Point(X,Y));
  if Assigned(IV) and (IV.Item is TSpTBXTabItem) then
    TabItem := TSpTBXTabItem(IV.Item);

  if Assigned(TabItem) and (Button = mbMiddle) then begin
    Editor := EditorFromTab(TabItem);
    if Assigned(Editor) then
      (Editor as IFileCommands).ExecClose;
  end else if (not Assigned(IV) or (IV.Item is TSpTBXRightAlignSpacerItem)) and (Shift = [ssLeft, ssDouble]) then begin
    if LocalAppStorage.PathExists('Layouts\BeforeZoom\Forms') then
      actRestoreEditorExecute(Sender)
    else
      actMaximizeEditorExecute(Sender);
  end;
end;

procedure TPyIDEMainForm.TabControlTabClosing(Sender: TObject; var Allow, CloseAndFree: Boolean);
begin
  Allow := False;
  TThread.ForceQueue(nil, procedure
  begin
    var Editor := EditorFromTab(Sender as TSpTBXTabItem);
    if Assigned(Editor) then
      (Editor as IFileCommands).ExecClose;
  end);
end;

procedure TPyIDEMainForm.TabToolbarlDragDrop(Sender, Source: TObject; X,
  Y: Integer);
Var
  Tab : TSpTBXTabItem;
  TargetTabControl : TSpTBXTabControl;
  IV : TTBItemViewer;
  Index : integer;
begin
  if (Source is TSpTBXTabItemDragObject) and
     (TSpTBXTabItemDragObject(Source).SouceItem is TSpTBXTabItem) and
     (Sender is TSpTBXTabToolbar) and
     (TSpTBXTabItemDragObject(Source).SourceControl <> Sender) then
  begin
    Tab := TSpTBXTabItemDragObject(Source).SouceItem as TSpTBXTabItem;
    TargetTabControl := TSpTBXTabToolbar(Sender).Owner as TSpTBXTabControl;
    IV := TSpTBXTabToolbar(Sender).View.ViewerFromPoint(Point(X,Y));
    if Assigned(IV) and (IV is TSpTBXTabItemViewer) then
      Index := TargetTabControl.Toolbar.Items.IndexOf(TSpTBXTabItemViewer(IV).Item)
    else
      Index := -1;
    MoveTab(Tab, TargetTabControl, Index);
  end;
end;

procedure TPyIDEMainForm.TabToolBarDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if (Source is TSpTBXTabItemDragObject) then begin
    if TSpTBXTabItemDragObject(Source).DragCursorAccept <> crDrag then begin
      TSpTBXTabItemDragObject(Source).DragCursorAccept := crDrag;
      TSpTBXTabItemDragObject(Source).DragCursorCancel := crNo;
    end;
    Accept := True;
  end;
end;

procedure TPyIDEMainForm.UpdateStandardActions;
Var
  L, R : Boolean;
begin
  actBreakPointsWin.Checked := BreakPointsWindow.Visible;
  actCallStackWin.Checked := CallStackWindow.Visible;
  actMessagesWin.Checked := MessagesWindow.Visible;
  actVariablesWin.Checked := VariablesWindow.Visible;
  actViewCodeExplorer.Checked := CodeExplorerWindow.Visible;
  actViewFileExplorer.Checked := FileExplorerWindow.Visible;
  actViewToDoList.Checked := ToDoWindow.Visible;
  actViewRegExpTester.Checked := RegExpTesterWindow.Visible;
  actViewUnitTests.Checked := UnitTestWindow.Visible;
  actViewOutput.Checked := OutputWindow.Visible;
  actViewFindResults.Checked := FindResultsWindow.Visible;
  actVIewII.Checked := PythonIIForm.Visible;
  actViewProjectExplorer.Checked := ProjectExplorerWindow.Visible;
  actViewSplitEditorHor.Enabled := Assigned(GI_ActiveEditor);
  actViewSplitEditorVer.Enabled := Assigned(GI_ActiveEditor);
  actViewHideSecondEditor.Enabled := Assigned(GI_ActiveEditor)
    and GI_ActiveEditor.SynEdit2.Visible;
  actViewHideSecondaryWorkspace.Enabled := TabControl2.Visible;

  actEditorZoomIn.Enabled := ActiveControl is TSynEdit;
  actEditorZoomOut.Enabled := actEditorZoomIn.Enabled;
  actEditorZoomReset.Enabled := actEditorZoomIn.Enabled;

  actWatchesWin.Checked := WatchesWindow.Visible;

  actViewStatusbar.Checked := StatusBar.Visible;
  actViewMainMenu .Checked := MainMenu.Visible;

  actFileNewModule.Enabled := GI_EditorFactory <> nil;
  actFileOpen.Enabled := GI_EditorFactory <> nil;
  actFileCloseAll.Enabled := (GI_EditorFactory <> nil)
    and (GI_EditorFactory.GetEditorCount > 0);

  actCommandLine.Checked := CommandLineParams <> '';

  // Refactoring
  actFindDefinition.Enabled := Assigned(GI_ActiveEditor) and
    GI_ActiveEditor.HasPythonFile;
  actFindReferences.Enabled := actFindDefinition.Enabled;
  actBrowseBack.Enabled := mnPreviousList.Count > 0;
  actBrowseForward.Enabled := mnNextList.Count > 0;

  // Python Engines
  case PyIDEOptions.PythonEngineType of
    peInternal :  actPythonInternal.Checked := True;
    peRemote : actPythonRemote.Checked := True;
    peRemoteTk : actPythonRemoteTk.Checked := True;
    peRemoteWx : actPythonRemoteWx.Checked := True;
    peSSH : actPythonSSH.Checked := True;
  end;

  // Scroll Buttons
  TabControl1.ScrollState(L, R);
  tbiScrollLeft.Enabled := L;
  tbiScrollRight.Enabled := R;
  TabControl2.ScrollState(L, R);
  tbiScrollLeft2.Enabled := L;
  tbiScrollRight2.Enabled := R;
end;

procedure TPyIDEMainForm.FormShortCut(var Msg: TWMKey;
  var Handled: Boolean);
begin
  Handled := CommandsDataModule.actlMain.IsShortCut(Msg);
end;

procedure TPyIDEMainForm.ChangeLanguage(LangCode: string);
begin
  if CompareText(GetCurrentLanguage, LangCode) <> 0 then begin
     UseLanguage(LangCode);

    RetranslateComponent(Self);
    RetranslateComponent(CommandsDataModule);

    SetupLanguageMenu;
    GI_EditorFactory.SetupEditorViewsMenu(EditorViewsMenu, vilImages);

    GI_EditorFactory.ApplyToEditors(procedure(Editor: IEditor)
    begin
      Editor.Retranslate;
    end);

    RegisterCustomParams;  // To get tranlations of descriptions
  end;
end;

procedure TPyIDEMainForm.LoadToolbarItems(const Path : string);
var
  MemIni: TMemIniFile;
  SL: TStringList;
begin
  if AppStorage.PathExists(Path) then begin
    MemIni := TMemIniFile.Create('');
    SL := TStringList.Create;
    try
      AppStorage.ReadStringList(Path, SL);
      MemIni.SetStrings(SL);
      SpLoadItems(Self, MemIni);
    finally
      MemIni.Free;
      SL.Free;
    end;
  end;
end;

procedure TPyIDEMainForm.SaveToolbarItems(const Path : string);
var
  MemIni: TMemIniFile;
  SL : TStringList;
begin
  AppStorage.DeleteSubTree(Path);
  MemIni := TMemIniFile.Create('');
  SL := TStringList.Create;
  try
    SpSaveItems(Self, MemIni);
    SL.Clear;
    MemIni.GetStrings(SL);
    AppStorage.WriteStringList(Path, SL);
  finally
    MemIni.Free;
    SL.Free;
  end;
end;

procedure TPyIDEMainForm.SaveToolbarLayout(const Layout: string);
var
  ToolbarLayout: TStringList;
begin
  ToolbarLayout := TStringList.Create;
  try
    SpTBXCustomizer.SaveLayout(ToolbarLayout, Layout);
    LocalAppStorage.WriteStringList('Layouts\' + Layout + '\Toolbars', ToolbarLayout);
  finally
    ToolbarLayout.Free;
  end;
end;

procedure TPyIDEMainForm.LoadToolbarLayout(const Layout: string);
var
  ToolbarLayout: TStringList;
  Path: string;
begin
  Path := 'Layouts\'+ Layout;
  if LocalAppStorage.PathExists(Path + '\Toolbars') then
  begin
    ToolbarLayout := TStringList.Create;
    try
      LocalAppStorage.ReadStringList(Path + '\Toolbars', ToolbarLayout);
      SpTBXCustomizer.LoadLayout(ToolbarLayout, Layout);
    finally
      ToolbarLayout.Free;
    end;
  end;
end;

procedure TPyIDEMainForm.SetupToolsMenu;
Var
  i : integer;
  MenuItem : TSpTBXItem;
  Action : TAction;
  Tool : TExternalTool;
begin
  // delete actions and menus added in previous calls
  mnTools.Clear;
  for i := actlStandard.ActionCount - 1 downto 0 do
    if actlStandard.Actions[i].Category = 'External Tools' then
      actlStandard.Actions[i].Free;
  for i := 0 to ToolsCollection.Count - 1 do begin
    Tool := (ToolsCollection.Items[i] as TToolItem).ExternalTool;
    if Tool.Caption <> '' then begin
      MenuItem := TSpTBXItem.Create(Self);
      Action := TExternalToolAction.CreateExtToolAction(Self, Tool);
      Action.ActionList := actlStandard;
      mnTools.Add(MenuItem);
      MenuItem.Action := Action;
      MenuItem.Images := FShellImages;
    end;
  end;
end;

procedure TPyIDEMainForm.SetupLanguageMenu;
Var
  MenuItem : TSpTBXItem;
  i : integer;
  CurrentLanguage : string;
  HaveLang : boolean;
begin
  mnLanguage.Clear;
  CurrentLanguage := GetCurrentLanguage;
  DefaultInstance.BindtextdomainToFile ('languagecodes',ExtractFilePath(Application.ExeName)+'locale\languagecodes.mo');
  DefaultInstance.GetListOfLanguages ('default',fLanguageList);
  fLanguageList.Insert(0, 'en');
  HaveLang := False;
  for i := 0 to fLanguageList.Count - 1 do begin
    MenuItem := TSpTBXItem.Create(Self);
    // Translate the language code to English language name and then to a localized language name
    MenuItem.Caption := dgettext('languages', dgettext('languagecodes', fLanguageList[i]));
    MenuItem.Tag := i;
    if fLanguageList[i] = CurrentLanguage then begin
      MenuItem.Checked := True;
      HaveLang := True;
    end;
    MenuItem.OnClick := mnLanguageClick;
    MenuItem.HelpContext := 360;
    mnLanguage.Add(MenuItem);
  end;
  if not HaveLang then
    mnLanguage.Items[0].Checked := True;
end;

procedure TPyIDEMainForm.SetupLayoutsMenu;
Var
  i : integer;
  MenuItem : TSpTBXItem;
begin
  // delete previous Layouts
  while mnLayouts.Items[0] <> mnLayOutSeparator do
    mnLayouts.Items[0].Free;

  for i := Layouts.Count - 1 downto 0 do begin
    MenuItem := TSpTBXItem.Create(Self);
    mnLayouts.Insert(0, MenuItem);
    MenuItem.Caption := Layouts[i];
    MenuItem.GroupIndex := 2;
    MenuItem.OnClick := LayoutClick;
    MenuItem.Hint := Format(_(SApplyLayout), [Layouts[i]]);
  end;
end;

procedure TPyIDEMainForm.mnSyntaxPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
Var
  i : integer;
  Editor : IEditor;
begin
  Editor := GetActiveEditor;
  for i := 0 to mnSyntax.Count - 3 do begin
    mnSyntax.Items[i].Enabled :=  Assigned(Editor);
    mnSyntax.Items[i].Checked := Assigned(Editor) and
      Assigned(Editor.Synedit.Highlighter) and
        (Editor.Synedit.Highlighter.FriendlyLanguageName = mnSyntax.Items[i].Caption);
  end;
  mnNoSyntax.Enabled := Assigned(Editor);
  mnNoSyntax.Checked := Assigned(Editor) and
    not Assigned(Editor.SynEdit.Highlighter);
end;

procedure TPyIDEMainForm.MoveTab(Tab: TSpTBXTabItem;
  TabControl: TSpTBXTabControl; Index: integer);
Var
  NewTab : TSpTBXTabItem;
  Sheet,
  NewSheet : TSpTBXTabSheet;
  EditorForm : TEditorForm;
begin
  if (Tab.Owner = TabControl) or not Assigned(Tab) then
    Exit;

  if Index >= 0 then
    NewTab := TabControl.Insert(Index, Tab.Caption)
  else
    NewTab := TabControl.Add(Tab.Caption);

  EditorForm := nil;
  Sheet := (Tab.Owner as TSpTBXTabControl).GetPage(Tab);
  if Assigned(Sheet) and (Sheet.ControlCount > 0) then
    EditorForm := Sheet.Controls[0] as TEditorForm;

  if Assigned(EditorForm) then begin
    EditorForm.Visible := False;
    NewSheet := (NewTab.Owner as TSpTBXTabControl).GetPage(NewTab);
    EditorForm.ParentTabItem := NewTab;
    EditorForm.ParentTabControl := TabControl;
    EditorForm.Parent := NewSheet;
    EditorForm.Align := alClient;
    NewSheet.InsertComponent(EditorForm);  // changes ownership
    NewTab.OnTabClosing := Tab.OnTabClosing;
    NewTab.OnDrawTabCloseButton := Tab.OnDrawTabCloseButton;
    EditorForm.Visible := True;
  end;

  Tab.Free;
  NewTab.Click;
end;

procedure TPyIDEMainForm.MRUAddEditor(Editor: IEditor);
begin
  if (Editor.FileName <> '') then
    tbiRecentFileList.MRUAdd(Editor.FileName)
  else if (Editor.RemoteFileName <> '') then
    tbiRecentFileList.MRUAdd(TSSHFileName.Format(Editor.SSHServer, Editor.RemoteFileName));
end;

procedure TPyIDEMainForm.mnSyntaxClick(Sender: TObject);
begin
  // Change Syntax sheme
  var Editor := GetActiveEditor;
  if Assigned(Editor) then begin
    var Highlighter := TSynCustomHighlighter((Sender as TTBCustomItem).Tag);
    if Assigned(Highlighter) then
      Editor.SetHighlighter(Highlighter.FriendlyLanguageName)
    else
      Editor.SetHighlighter('None');
  end;
end;

procedure TPyIDEMainForm.PythonVersionsClick(Sender: TObject);
begin
   PyControl.PythonVersionIndex := (Sender as TTBCustomItem).Tag;
end;

procedure TPyIDEMainForm.mnPythonVersionsPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
Var
  i : integer;
  PythonLoaded: Boolean;
begin
  PythonLoaded := GI_PyControl.PythonLoaded;
  for i := 0 to mnPythonVersions.Count - 3 do begin
    mnPythonVersions.Items[i].Enabled := PyControl.DebuggerState = dsInactive;
    mnPythonVersions.Items[i].Checked := PythonLoaded and
      (PyControl.PythonVersionIndex = mnPythonVersions.Items[i].Tag);
  end;
end;

procedure TPyIDEMainForm.SetupSyntaxMenu;
begin
  while mnSyntax.Count > 2 do
    mnSyntax.Delete(0);
  for var I := ResourcesDataModule.Highlighters.Count - 1 downto 0 do begin
    var MenuItem := TSpTBXItem.Create(Self);
    mnSyntax.Insert(0, MenuItem);
    MenuItem.Caption := _(ResourcesDataModule.Highlighters[i].FriendlyLanguageName);
    MenuItem.Tag := Integer(ResourcesDataModule.Highlighters[i]);
    MenuItem.GroupIndex := 3;
    MenuItem.OnClick := mnSyntaxClick;
    MenuItem.Hint := Format(_(SUseSyntax), [MenuItem.Caption]);
  end;
end;

procedure TPyIDEMainForm.SetupPythonVersionsMenu;
Var
  i : Integer;
  MenuItem : TTBCustomItem;
begin
  // remove previous versions
  while mnPythonVersions.Count > 2 do
    mnPythonVersions.Items[0].Free;
  // Add versions in reverse order
  for i := Length(PyControl.CustomPythonVersions) - 1 downto 0 do begin
    MenuItem := TSpTBXItem.Create(Self);
    mnPythonVersions.Insert(0, MenuItem);
    MenuItem.Caption := PyControl.CustomPythonVersions[i].DisplayName;
    MenuItem.GroupIndex := 3;
    MenuItem.Tag := -(i + 1);
    MenuItem.OnClick := PythonVersionsClick;
    MenuItem.Hint := Format(_(SSwitchtoVersion), [MenuItem.Caption]);
  end;
  if Length(PyControl.CustomPythonVersions) > 0 then begin
    MenuItem := TSpTBXSeparatorItem.Create(Self);
    MenuItem.Tag := MenuItem.Tag.MaxValue;
    mnPythonVersions.Insert(0, MenuItem);
  end;
  for i := Length(PyControl.RegPythonVersions) - 1 downto 0 do begin
    MenuItem := TSpTBXItem.Create(Self);
    mnPythonVersions.Insert(0, MenuItem);
    MenuItem.Caption := PyControl.RegPythonVersions[i].DisplayName;
    MenuItem.GroupIndex := 3;
    MenuItem.Tag := i;
    MenuItem.OnClick := PythonVersionsClick;
    MenuItem.Hint := Format(_(SSwitchtoVersion), [MenuItem.Caption]);
  end;
end;

procedure TPyIDEMainForm.SplitWorkspace(SecondTabsVisible : Boolean;
      Alignment : TAlign; Size : integer);
begin
  TabSplitter.Visible := False;
  TabControl2.Visible := False;
  ActiveTabControlIndex := 1;
  if SecondTabsVisible then begin
    TabControl2.Align := Alignment;
    if Alignment = alRight then
      TabControl2.Width := IfThen(Size >= 0, Size, (BGPanel.ClientWidth - 5) div 2)
    else
      TabControl2.Height := IfThen(Size >= 0, Size, (BGPanel.ClientHeight - 5) div 2);
    TabSplitter.Align := Alignment;
    TabControl2.Visible := True;
    TabSplitter.Visible := True;
  end;
end;

procedure TPyIDEMainForm.SetupCustomizer;
var
  K: Integer;
  ItemStyle: TTBItemStyle;
  ActionList: TActionList;
  Action: TContainedAction;
  ItemsList: TList;
  I: Integer;
  ParentItem: TTBCustomItem;
  C: TComponent;
  J: Integer;
  Item: TTBCustomItem;
begin
  SpTBXCustomizer.Items.Clear;
  ItemsList := TList.Create;
  try
    for I := 0 to ComponentCount - 1 do
    begin
      ParentItem := nil;
      C := Components[I];
      if C is TSpTBXToolbar and TSpTBXToolbar(C).Customizable then
        ParentItem := TSpTBXToolbar(C).Items;
      if Assigned(ParentItem) then
      begin
        for J := 0 to ParentItem.Count - 1 do
        begin
          Item := ParentItem[j];
          ItemStyle := TTBCustomItemAccess(Item).ItemStyle;
          // Exclude the submenus, separators, labels, groups and edit items
          if (ItemStyle * [tbisSubMenu, tbisSeparator, tbisEmbeddedGroup, tbisClicksTransparent] = []) and not (Item is TTBEditItem) then
            ItemsList.Add(Item);
        end;
      end;
    end;
    for I := Low(TActionProxyCollection.ActionLists) to High(TActionProxyCollection.ActionLists) do
    begin
      ActionList := TActionProxyCollection.ActionLists[I];
      for J := 0 to ActionList.ActionCount - 1 do
      begin
        Action := ActionList.Actions[J];
        for K := 0 to ItemsList.Count - 1 do
          if TTBCustomItem(ItemsList[K]).Action = Action then
          begin
            Action := nil;
            break;
          end;
        if Assigned(Action) then
        begin
          // Find items of External actions on UserToolbars
          Item := FindComponent('tb' + Action.Name) as TTBCustomItem;
          if not Assigned(Item) then begin
            Item := TSpTBXItem.Create(Self);
            Item.Name := 'tb' + Action.Name;
            if Action is TExternalToolAction then
              Item.Images := FShellImages;
            SpTBXCustomizer.Items.Add(Item);
          end;
          Item.Action := Action;
        end;
      end;
    end;
  finally
    ItemsList.Free;
  end;
end;

procedure TPyIDEMainForm.LoadLayout(const Layout: string);
Var
  Path : string;
  SaveActiveControl : TWinControl;
  TempCursor : IInterface;
begin
  Path := 'Layouts\'+ Layout;
  if LocalAppStorage.PathExists(Path + '\Forms') then begin
    TempCursor := WaitCursor;
    SaveActiveControl := ActiveControl;

    try
      // Now Load the DockTree
      LoadDockTreeFromAppStorage(LocalAppStorage, Path);
    finally
      for var I := 0 to Screen.FormCount - 1 do
        if Screen.Forms[I] is TIDEDockWindow then
          TIDEDockWindow(Screen.Forms[I]).FormDeactivate(Self);
    end;
    if CanActuallyFocus(SaveActiveControl)
    then
      try
        SaveActiveControl.SetFocus;
      except
      end;
  end;
  // Now Restore the toolbars
  LoadToolbarLayout(Layout);
end;

procedure TPyIDEMainForm.SaveLayout(const Layout: string);
begin
  LocalAppstorage.DeleteSubTree('Layouts\'+Layout);
  SaveDockTreeToAppStorage(LocalAppStorage, 'Layouts\'+ Layout);
  SaveToolbarLayout(Layout);
end;

procedure TPyIDEMainForm.LayoutClick(Sender: TObject);
begin
  LoadLayout(TSpTBXItem(Sender).Caption);
  TSpTBXItem(Sender).Checked := True;
end;

function TPyIDEMainForm.LayoutExists(const Layout: string): Boolean;
begin
  Result := Layouts.IndexOf(Layout) >= 0;
end;

procedure TPyIDEMainForm.lbPythonEngineClick(Sender: TObject);
var
  MousePos : TPoint;
begin
  GetCursorPos(MousePos);
  MousePos := ScreenToClient(MousePos);
  mnPythonEngines.Popup(MousePos.X, MousePos.Y, True);
end;

procedure TPyIDEMainForm.lbPythonVersionClick(Sender: TObject);
begin
  actPythonSetup.Execute;
end;

procedure TPyIDEMainForm.lbStatusCaretClick(Sender: TObject);
begin
   CommandsDataModule.actSearchGoToLineExecute(Self);
end;

procedure TPyIDEMainForm.actLayoutSaveExecute(Sender: TObject);
Var
  LayoutName : string;
  TempCursor : IInterface;
begin
  if InputQuery(_(SSaveCurrentLayout), _(SLayoutName), LayoutName) then begin
    TempCursor := WaitCursor;
    if Layouts.IndexOf(LayoutName) < 0 then begin
      Layouts.Add(LayoutName);
      SetupLayoutsMenu;
    end;
    SaveLayout(LayoutName);
  end;
end;

procedure TPyIDEMainForm.actLayoutsDeleteExecute(Sender: TObject);
Var
  LayoutName : string;
  i : integer;
begin
  with TPickListDialog.Create(Self) do begin
    Caption := _(SDeleteLayouts);
    lbMessage.Caption := _(SSelectLayouts);
    CheckListBox.Items.Assign(Layouts);
    if ShowModal = IdOK then begin
      for i := CheckListBox.Count - 1 downto 0 do begin
        if CheckListBox.Checked[i] then begin
          LayoutName := Layouts[i];
          LocalAppstorage.DeleteSubTree('Layouts\'+LayoutName);
          Layouts.Delete(i);
          SetupLayoutsMenu;
        end;
      end;
    end;
    Free;
  end;
end;

procedure TPyIDEMainForm.actLayoutDebugExecute(Sender: TObject);
begin
  if Layouts.IndexOf('Debug') < 0 then
    Layouts.Add('Debug');
  SaveLayout('Debug');
  SetupLayoutsMenu;
end;

procedure TPyIDEMainForm.actExternalRunExecute(Sender: TObject);
Var
  ActiveEditor : IEditor;
  RunConfig : TRunConfiguration;
begin
  ActiveEditor := GetActiveEditor;
  if Assigned(ActiveEditor) then begin
    ActiveEditor.Activate;
    RunConfig :=  TRunConfiguration.Create;
    try
      SetupRunConfiguration(RunConfig, ActiveEditor);
      PyControl.ExternalRun(RunConfig);
    finally
      RunConfig.Free;
    end;
  end;
end;

procedure TPyIDEMainForm.actExecSelectionExecute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) and GI_ActiveEditor.HasPythonFile and
    GI_ActiveEditor.SynEdit.SelAvail
  then
    GI_ActiveEditor.ExecuteSelection;
end;

procedure TPyIDEMainForm.actExternalRunConfigureExecute(Sender: TObject);
begin
  EditTool(ExternalPython, True);
end;

procedure TPyIDEMainForm.WriteStatusMsg(const S: string);
begin
  lbStatusMessage.Caption := S;
end;

procedure TPyIDEMainForm.ThemeEditorGutter(Gutter : TSynGutter);
Var
  GradColor: TColor;
begin
  Assert(SkinManager.GetSkinType(nil) <> sknSkin);
  if SkinManager.GetSkinType(nil) in [sknNone, sknWindows] then begin
    Gutter.GradientStartColor := clWindow;
    Gutter.GradientEndColor := clBtnFace;
    Gutter.Font.Color := clSilver;
    Exit;
  end;

  // Delphi Styles
  if not StyleServices.GetElementColor(StyleServices.GetElementDetails(ttTabItemNormal),
    ecFillColor, GradColor) or (GradColor = clNone)
  then
    GradColor := StyleServices.GetSystemColor(clBtnFace);
  Gutter.Font.Color :=  StyleServices.GetSystemColor(clGrayText);;

  with Gutter do begin
    BorderStyle := gbsNone;
    GradientStartColor := LightenColor(GradColor, 40);
    GradientEndColor := DarkenColor(GradColor, 20);
    Color := GradColor;
  end;
end;

procedure TPyIDEMainForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  with ActiveTabControl as TSpTBXTabControl do
    if (Key = VK_Control) and zOrderProcessing then
    begin
      zOrderProcessing := False;
      KeyPreview := False;
      if (zOrderPos > 0) and (zOrderPos < zOrder.Count) then begin
        zOrder.Move(zOrderPos, 0);
        zOrderPos := 0;
      end;
    end;
end;

procedure TPyIDEMainForm.AdjustBrowserLists(FileName: string;
  Line: Integer; Col: Integer; FilePosInfo: string);
begin
  if FilePosInfo <> '' then
  begin
    // Adjust previous/next menus
    if fCurrentBrowseInfo <> '' then
      PrevMRUAdd(fCurrentBrowseInfo);

    PrevMRUAdd(Format(FilePosInfoFormat, [FileName, Line, Col]));
    mnNextList.Clear;
    fCurrentBrowseInfo := FilePosInfo;
  end;
end;

procedure TPyIDEMainForm.actFindDefinitionExecute(Sender: TObject);
Var
  FilePosInfo : string;
  FileName : string;
  CaretXY : TBufferCoord;
begin
  Application.ProcessMessages;
  if Assigned(GI_ActiveEditor) then begin
    FileName := GI_ActiveEditor.FileId;
    CaretXY := GI_ActiveEditor.ActiveSynEdit.CaretXY;
    FindDefinition(GI_ActiveEditor, CaretXY, True, False, True, FilePosInfo);
    AdjustBrowserLists(FileName, CaretXY.Line, CaretXY.Char, FilePosInfo);
  end;
end;

function TPyIDEMainForm.FileIsPythonSource(const FileName: string): Boolean;
Var
  Ext: string;
begin
  Ext := ExtractFileExt(FileName);
  if Ext = '' then
    Exit(False);
  Result := FileExtInFileFilter(Ext, PyIDEOptions.PythonFileFilter);
end;

procedure TPyIDEMainForm.FindDefinition(Editor : IEditor; TextCoord: TBufferCoord;
  ShowMessages, Silent, JumpToFirstMatch: Boolean; var FilePosInfo : string);
var
  Defs : Variant;
  Token : string;
  FName, FileName: string;
  TokenType,
  Start: Integer;
  Attri: TSynHighlighterAttributes;
  TempCursor : IInterface;
  BC: TBufferCoord;
begin
  FilePosInfo := '';
  VarClear(Defs);
  if Assigned(Editor) and Editor.HasPythonFile then with Editor.ActiveSynEdit do
  begin
    if TextCoord.Line > Lines.Count then Exit;

    // Adjust TextCoord if we are at the end of an identifier (#1151)
    var Line := Lines[TextCoord.Line - 1];
    TextCoord.Char := Min(TextCoord.Char, Line.Length);
    if (TextCoord.Char > 1) and not IsIdentChar(Line[TextCoord.Char]) then
      Dec(TextCoord.Char);

    if GetHighlighterAttriAtRowColEx(TextCoord, Token, TokenType, Start, Attri) then begin
      case TokenType of
        Ord(tkFunctionName), Ord(tkClassName):
          begin
            if not Silent then
              StyledMessageDlg(Format(_(SFindDefinitionWarning), [Token]),
                mtInformation, [mbOK], 0);
            Exit;
          end;
        Ord(tkIdentifier), Ord(tkSystemDefined), Ord(tkNonKeyword):
          begin
            TempCursor := WaitCursor;

            FName := Editor.FileId;

            if ShowMessages then begin
              GI_PyIDEServices.Messages.ClearMessages;
              GI_PyIDEServices.Messages.AddMessage(_(SDefinitionsOf) + Token + '"');
            end;

            FileName := '';
            TJedi.FindDefinitionByCoordinates(FName, TextCoord, FileName, BC);

            if (FileName <> '') and ShowMessages then
              GI_PyIDEServices.Messages.AddMessage(_(SDefinitionFound), FileName, BC.Line, BC.Char);

            if ShowMessages then
              ShowDockForm(MessagesWindow);

            if FileName  <> '' then begin
              FilePosInfo := Format(FilePosInfoFormat, [Filename, BC.Line, BC.Char]);
              if JumpToFirstMatch then
                ShowFilePosition(Filename, BC.Line, BC.Char);
            end else begin
              if ShowMessages then
                GI_PyIDEServices.Messages.AddMessage(_(SDefinitionNotFound));
              MessageBeep(MB_ICONASTERISK);
            end;
          end;
      else if not Silent then
        StyledMessageDlg(_(SPlaceCursorOnName),
          mtError, [mbOK], 0);
      end;
    end;
  end;
end;

procedure TPyIDEMainForm.FindToolbarVisibleChanged(Sender: TObject);
Var
  SearchCommands : ISearchCommands;
begin
  if not FindToolbar.Visible then begin
    ClearAllHighlightedTerms;
    SearchCommands := CommandsDataModule.FindSearchTarget;
    if Assigned(SearchCommands) and CanActuallyFocus(SearchCommands.SearchTarget) then
      SearchCommands.SearchTarget.SetFocus;
  end;
end;

procedure TPyIDEMainForm.actFindReferencesExecute(Sender: TObject);
var
  Token : string;
  FName: string;
  TokenType,
  Start: Integer;
  Attri: TSynHighlighterAttributes;
  TempCursor: IInterface;
  FoundReferences: Boolean;
  Line: string;
  References: TArray<TDocPosition>;
begin
  Application.ProcessMessages;
  TempCursor := WaitCursor;

  if Assigned(GI_ActiveEditor) and GI_ActiveEditor.HasPythonFile then
  with GI_ActiveEditor.ActiveSynEdit do begin
    if GetHighlighterAttriAtRowColEx(CaretXY,  Token, TokenType, Start, Attri) then begin
      case TokenType of
        Ord(tkFunctionName), Ord(tkClassName), Ord(tkIdentifier) :
          begin
            FName := GI_ActiveEditor.FileName;
            if FName = '' then
              FName := GI_ActiveEditor.FileTitle;

            GI_PyIDEServices.Messages.ClearMessages;
            GI_PyIDEServices.Messages.AddMessage(_(SReferencesOf) + Token + '"');

            References := TJedi.FindReferencesByCoordinates(FName, CaretXY);
            FoundReferences := Length(References) > 0;
            for var DocPosition in References do
            begin
              Line := GetNthSourceLine(DocPosition.FileId, DocPosition.Line);
              GI_PyIDEServices.Messages.AddMessage(Line, DocPosition.FileId,
                DocPosition.Line, DocPosition.Char, Token.Length);
            end;

            ShowDockForm(MessagesWindow);
            if not FoundReferences then begin
              GI_PyIDEServices.Messages.AddMessage(_(SReferencesNotFound));
              MessageBeep(MB_ICONASTERISK);
            end;
          end;
      else
        StyledMessageDlg(_(SPlaceCursorOnName), mtError, [mbOK], 0);
      end;
    end;
  end;
end;

procedure TPyIDEMainForm.WMSpSkinChange(var Message: TMessage);
begin
  // Update EditorOptions
  ThemeEditorGutter(EditorOptions.Gutter);
  PyIDEOptions.CodeFolding.FolderBarLinesColor := EditorOptions.Gutter.Font.Color;

  // After updating to D11.3 Toolbar flicker was very visible on Style change
  // BeginUpdate/EndUpdate solved the problem
  MainToolBar.BeginUpdate;
  DebugToolbar.BeginUpdate;
  ViewToolbar.BeginUpdate;
  EditorToolbar.BeginUpdate;
  UserToolbar.BeginUpdate;
  FindToolbar.BeginUpdate;
  try
    ResourcesDataModule.UpdateImageCollections;
  finally
    MainToolBar.EndUpdate;
    DebugToolbar.EndUpdate;
    ViewToolbar.EndUpdate;
    EditorToolbar.EndUpdate;
    UserToolbar.EndUpdate;
    FindToolbar.EndUpdate;
  end;
//  BGPanel.Color := CurrentTheme.GetItemColor(GetItemInfo('inactive'));
//  Application.HintColor := CurrentTheme.GetViewColor(VT_DOCKPANEL);
end;

procedure TPyIDEMainForm.WMWTSSessionChange(var Message: TMessage);
begin
  case Message.wParam of
    WTS_SESSION_LOCK,
    WTS_REMOTE_DISCONNECT: Application.UpdateMetricSettings := False;
    WTS_REMOTE_CONNECT,
    WTS_SESSION_UNLOCK:
      TThread.ForceQueue(nil, procedure
      begin
        Application.UpdateMetricSettings := True;
      end, 30000);
  end;
end;

procedure TPyIDEMainForm.CMStyleChanged(var Message: TMessage);
begin
  SkinManager.BroadcastSkinNotification;
end;

procedure TPyIDEMainForm.WMFindDefinition(var Msg: TMessage);
Var
  FilePosInfo : string;
  FileName : string;
  Line, Col : integer;
begin
  if Assigned(GI_ActiveEditor) then begin
    FileName := GI_ActiveEditor.FileId;
    Line := Msg.LParam;
    Col := Msg.WParam;
    FindDefinition(GI_ActiveEditor, BufferCoord(Col, Line), False,
      True, True, FilePosInfo);
    AdjustBrowserLists(FileName, Line, Col, FilePosInfo);
  end;
end;

procedure TPyIDEMainForm.WMSearchReplaceAction(var Msg: TMessage);
Var
  Action : TCustomAction;
begin
  if Msg.LParam <> 0 then begin
    Action := TCustomAction(Msg.LParam);
    Action.Execute;
  end else begin
    if Msg.WParam = 2 then begin
      // incremental search
      CommandsDataModule.IncrementalSearch;
    end;
  end;
end;

function TPyIDEMainForm.JumpToFilePosInfo(const FilePosInfo : string): Boolean;
Var
  FileName : string;
  Line, Col : integer;
begin
  Result := False;
  with TRegEx.Match(FilePosInfo, FilePosInfoRegExpr) do
    if Success then begin
      FileName := GroupValue(1);
      Line := StrToInt(GroupValue(2));
      Col := StrToInt(GroupValue(3));

      Exit(ShowFilePosition(FileName, Line, Col));
    end;
end;

procedure TPyIDEMainForm.DrawCloseButton(Sender: TObject; ACanvas: TCanvas;
  State: TSpTBXSkinStatesType; const PaintStage: TSpTBXPaintStage;
  var AImageList: TCustomImageList; var AImageIndex: Integer; var ARect: TRect;
  var PaintDefault: Boolean);
Var
  Editor : IEditor;
  PatternColor: TColor;
  R : TRect;
begin
  Editor := EditorFromTab(TSpTBXTabItem(Sender));
  if not Assigned(Editor) then Exit;

  PaintDefault := False;
  AImageIndex := -1;

  if State = sknsHotTrack then begin
    R := ARect;
    InflateRect(R, -1, -1);
    SpDrawXPButton(nil, ACanvas, R, True, False, True, False, False, False, FCurrentPPI);
  end;
  PatternColor := CurrentSkin.GetTextColor(nil, skncToolbarItem, State);
  if Editor.Modified then
  begin
    R := SpCenterRect(ARect, PPIScale(3), PPIScale(3));
    ExcludeClipRect(ACanvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
  end;
  SpDrawGlyphPattern(ACanvas, ARect, gptClose, PatternColor, FCurrentPPI);
  if Editor.Modified then
    SelectClipRgn(ACanvas.Handle, 0);
end;

procedure TPyIDEMainForm.PrevClickHandler(Sender: TObject);
begin
  if Sender is TSpTBXMRUItem then begin
    var MRUItem := TSpTBXMRUItem(Sender);
    PreviousListClick(mnPreviousList, MRUItem.MRUString);
  end;
end;

procedure TPyIDEMainForm.PreviousListClick(Sender: TObject; S : string);
begin
  var Index := mnPreviousList.IndexOfMRU(S);
  if Index >= 0 then begin
    JumpToFilePosInfo(S);
    NextMRUAdd(fCurrentBrowseInfo);
    fCurrentBrowseInfo := S;
    for var I := 0 to Index - 1 do
      NextMRUAdd(TSpTBXMRUItem(mnPreviousList.Items[I]).MRUString);
    for var I := 0 to Index do
      mnPreviousList.MRURemove(TSpTBXMRUItem(mnPreviousList.Items[0]).MRUString);
  end;
end;

procedure TPyIDEMainForm.PrevMRUAdd(S : string);
begin
  mnPreviousList.MRUAdd(S);
  mnPreviousList.Items[0].OnClick := PrevClickHandler;
end;

procedure TPyIDEMainForm.tbiBrowsePreviousClick(Sender: TObject);
begin
  if mnPreviousList.Count > 0 then
    PreviousListClick(Sender, TSpTBXMRUItem(mnPreviousList.Items[0]).MRUString);
end;

function TPyIDEMainForm.NewFileFromTemplate(
  FileTemplate: TFileTemplate; TabControlIndex : integer): IEditor;
Var
  i, j : integer;
  TabCtrl : TSpTBXTabControl;
  Editor : IEditor;
  EditorView: IEditorView;
begin
  // create a new editor, add it to the editor list
  TabCtrl := TabControl(TabControlIndex);
  TabCtrl.Toolbar.BeginUpdate;
  try
    Result := GI_EditorFactory.NewEditor(TabControlIndex);;
    if Result <> nil then begin
      try
        Result.OpenFile('', FileTemplate.Highlighter);
        Result.Activate;
      except
        Result.Close;
        raise
      end;
      Result.SynEdit.SelText := Parameters.ReplaceInText(FileTemplate.Template);

      // Locate the caret symbol |
      for i := 0 to Result.SynEdit.Lines.Count - 1 do begin
        j := Result.SynEdit.Lines[i].IndexOf('|');
        if j >= 0 then begin
          Result.SynEdit.CaretXY := BufferCoord(j + 1, i + 1);
          Result.SynEdit.ExecuteCommand(ecDeleteChar, ' ', nil);
          break;
        end;
      end;

      Result.SynEdit.ClearUndo;
      Result.SynEdit.Modified := False;
      Result.RefreshSymbols;

      TEditorForm(Result.Form).DefaultExtension := FileTemplate.Extension;
      // Jupyter support
      if (LowerCase(FileTemplate.Extension) = 'ipynb') and
        not OutputWindow.IsRunning then
      begin
        Editor := Result;
        (Editor as IFileCommands).ExecSave;
        TThread.ForceQueue(nil, procedure
          begin
            EditorView := Editor.ActivateView(GI_EditorFactory.ViewFactory[WebPreviewFactoryIndex]);
            if Assigned(EditorView) then
              EditorView.UpdateView(Editor);
          end);
      end;
    end;
  finally
    TabCtrl.Toolbar.EndUpdate;
    if Assigned(TabCtrl.ActiveTab) then
      TabCtrl.MakeVisible(TabCtrl.ActiveTab);
    UpdateCaption;
  end;
end;

procedure TPyIDEMainForm.NextClickHandler(Sender: TObject);
begin
  if Sender is TSpTBXMRUItem then begin
    var MRUItem := TSpTBXMRUItem(Sender);
    NextListClick(mnNextList, MRUItem.MRUString);
  end;
end;

procedure TPyIDEMainForm.NextListClick(Sender: TObject; S : string);
begin
  var Index := mnNextList.IndexOfMRU(S);
  if Index >= 0 then begin
    JumpToFilePosInfo(S);
    PrevMRUAdd(fCurrentBrowseInfo);
    fCurrentBrowseInfo := S;
    for var I := 0 to Index - 1 do
      PrevMRUAdd(TSpTBXMRUItem(mnNextList.Items[I]).MRUString);
    for var I := 0 to Index do
      mnNextList.MRURemove(TSpTBXMRUItem(mnNextList.Items[0]).MRUString);
  end;
end;

procedure TPyIDEMainForm.NextMRUAdd(S : string);
begin
  mnNextList.MRUAdd(S);
  mnNextList.Items[0].OnClick := NextClickHandler;
end;

function TPyIDEMainForm.OpenCmdLineFile(FileName : string): Boolean;
begin
  // Try to see whether it contains line/char info
  Result := JumpToFilePosInfo(FileName);
  if not Result and FileExists(FileName) then
    Result := Assigned(DoOpenFile(FileName, '', TabControlIndex(ActiveTabControl)));
end;

procedure TPyIDEMainForm.tbiBrowseNextClick(Sender: TObject);
begin
  if mnNextList.Count > 0 then begin
    NextListClick(Sender, TSpTBXMRUItem(mnNextList.Items[0]).MRUString);
  end;
end;

procedure TPyIDEMainForm.ApplicationActionExecute(Action: TBasicAction;
  var Handled: Boolean);
Var
  Msg : Cardinal;
begin
  if (Action is TEditAction) and Assigned(Screen.ActiveControl) and
    (Screen.ActiveControl is TCombobox) and
    not TComboBox(Screen.ActiveControl).DroppedDown
  then begin
    Msg := 0;
    if Action is TEditCopy then
      Msg := WM_COPY
    else if Action is TEditCut then
      Msg := WM_CUT
    else if Action is TEditPaste then
      Msg := WM_PASTE;
    if Msg <> 0 then begin
      PostMessage(Screen.ActiveControl.Handle, Msg, 0, 0);
      Handled := True;
    end;
  end;
end;

procedure TPyIDEMainForm.ApplicationActionUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  if (Action is TEditAction) then
  begin
     if Assigned(Screen.ActiveControl) and (Screen.ActiveControl is TCombobox) and
      not TComboBox(Screen.ActiveControl).DroppedDown
    then begin
      TEditAction(Action).Enabled :=
       (Action is TEditCut) and (TComboBox(Screen.ActiveControl).SelLength > 0) or
       (Action is TEditCopy) and (TComboBox(Screen.ActiveControl).SelLength > 0) or
       (Action is TEditPaste) and Clipboard.HasFormat(CF_UNICODETEXT);
      Handled := (Action is TEditCut) or (Action is TEditCopy) or (Action is TEditPaste);
    end
    else if ((Action is TEditCopy) or (Action is TEditCut)) and Assigned(GI_ActiveEditor) then
    begin
      TEditAction(Action).Enabled := True;
      Handled := True;
    end;
  end;
end;

function TPyIDEMainForm.ApplicationHelp(Command: Word; Data: NativeInt;
  var CallHelp: Boolean): Boolean;
Var
  KeyWord : string;
begin
  CallHelp := True;
  Result := False;
  // We are not going to show popup help
  //if Command = HELP_SETPOPUP_POS then exit;
  if not PythonKeywordHelpRequested and not MenuHelpRequested and
     Active and (ActiveControl is TSynEdit) and
    (TSynEdit(ActiveControl).Highlighter = ResourcesDataModule.SynPythonSyn) then
  begin
    Keyword := TSynEdit(ActiveControl).WordAtCursor;
    if Keyword <> '' then begin
      CallHelp := not CommandsDataModule.ShowPythonKeywordHelp(KeyWord);
      Result := True;
    end;
  end;
end;

procedure TPyIDEMainForm.WMDestroy(var Message: TWMDestroy);
begin
  inherited;
  WTSUnRegisterSessionNotification(Handle);
end;

procedure TPyIDEMainForm.WMEndSession(var Msg: TWMEndSession);
begin
  if Msg.EndSession then
    GI_EditorFactory.CreateRecoveryFiles;
  Msg.Result := 0;
end;

procedure TPyIDEMainForm.WMQueryEndSession(var Msg: TWMQueryEndSession);
begin
  Msg.Result := 1;
end;

procedure TPyIDEMainForm.FormShow(Sender: TObject);
begin
  //OutputDebugString(PWideChar(Format('%s ElapsedTime %d ms', ['FormShow start', StopWatch.ElapsedMilliseconds])));
  // Do not execute again
  OnShow := nil;

  // fix for staturbar appearing above interpreter
  if StatusBar.Visible then StatusBar.Top := MaxInt;

  // Update Syntax and Layouts menu
  SetupLayoutsMenu;
  SetupSyntaxMenu;

  // Start accepting files
  DragAcceptFiles(TabControl1.Handle, True);
  DragAcceptFiles(TabControl2.Handle, True);

  TThread.ForceQueue(nil, procedure
  begin
    // Activate File Explorer
    FileExplorerWindow.FileExplorerTree.Active := True;
    //Application.ProcessMessages;

    // Load Python Engine and Assign Debugger Events
    PyControl.LoadPythonEngine;
    SetupPythonVersionsMenu;

    // Update External Tools
    SetupToolsMenu;  // After creating internal interpreter
    SetupCustomizer; // After setting up the Tools menu
    // Load Toolbar Items after setting up the Tools menu
    if FileExists(AppStorage.IniFile.FileName) then
      LoadToolbarItems('Toolbar Items');

    with PyControl do begin
      OnBreakpointChange := DebuggerBreakpointChange;
      OnCurrentPosChange := DebuggerCurrentPosChange;
      OnErrorPosChange := DebuggerErrorPosChange;
      OnStateChange := DebuggerStateChange;
    end;

    // This is needed to update the variables window
    PyControl.DebuggerState := dsInactive;

    // Open initial files after loading Python (#879)
    OpenInitialFiles;

    if Layouts.IndexOf('Default') < 0 then begin
      SaveLayout('Default');
      Layouts.Add('Default');
    end;

    TThread.ForceQueue(nil, procedure
    begin
      CommandsDataModule.AutoCheckForUpdates;
    end, 1000);

    if not GI_PyControl.PythonLoaded then
      actPythonSetupExecute(Self);
  end);
  //OutputDebugString(PWideChar(Format('%s ElapsedTime %d ms', ['FormShow end', StopWatch.ElapsedMilliseconds])));
end;

procedure TPyIDEMainForm.JvAppInstancesCmdLineReceived(Sender: TObject;
  CmdLine: TStrings);
var
  i : integer;
begin
  if JvAppInstances.AppInstances.InstanceIndex[GetCurrentProcessID] <> 0 then Exit;
  for i := 0 to CmdLine.Count - 1 do
    if (CmdLine[i][1] <> '-') then
      //DoOpenFile(CmdLine[i]);
      ShellExtensionFiles.Add(CmdLine[i])
end;

//function TPyIDEMainForm.FindAction(var Key: Word; Shift: TShiftState) : TCustomAction;
//var
//  ShortCut : TShortCut;
//  i, j : Integer;
//  Action : TContainedAction;
//  ActionList : TActionList;
//begin
//  Result := nil;
//  ShortCut := Menus.ShortCut(Key, Shift);
//  if ShortCut <> scNone then
//    for j := 0 to Length(TActionProxyCollection.ActionLists) do begin
//      if j = Length(TActionProxyCollection.ActionLists) then
//        ActionList := actlImmutable
//      else
//        ActionList := TActionProxyCollection.ActionLists[j];
//      for i := 0 to ActionList.ActionCount - 1 do
//      begin
//        Action := ActionList[I];
//        if (TCustomAction(Action).ShortCut = ShortCut) or
//           (TCustomAction(Action).SecondaryShortCuts.IndexOfShortCut(ShortCut) <> -1) then
//        begin
//          Result := TCustomAction(Action);
//          Exit;
//        end;
//      end;
//    end;
//end;

procedure TPyIDEMainForm.SaveEnvironment;
begin
  // Save the list of open files
  AppStorage.DeleteSubTree('Open Files');
  TPersistFileInfo.WriteToAppStorage(AppStorage, 'Open Files');
  // Delete BeforeZoom layout if it exists
  if LocalAppStorage.PathExists('Layouts\BeforeZoom\Forms') then
    LocalAppstorage.DeleteSubTree('Layouts\BeforeZoom');
  // Save Layout
  SaveLayout('Current');
  // Store other application data and flush AppStorage
  StoreApplicationData;
  StoreLocalApplicationData;
end;

procedure TPyIDEMainForm.actMaximizeEditorExecute(Sender: TObject);
var
  i : TJvDockPosition;
  Panel : TJvDockPanel;
begin
  SaveLayout('BeforeZoom');
  for i := Low(TJvDockPosition) to High(TJvDockPosition) do begin
    Panel := DockServer.DockPanel[i];
    if not (Panel is TJvDockVSNETPanel) then continue;
    while Panel.DockClientCount > 0 do
      TJvDockVSNETPanel(Panel).DoAutoHideControl(
          Panel.DockClients[Panel.DockClientCount-1] as TWinControl);
  end;
end;

procedure TPyIDEMainForm.actRemoteFileOpenExecute(Sender: TObject);
Var
  FileName, Server : string;
begin
  if ExecuteRemoteFileDialog(FileName, Server, rfdOpen) then
  begin
    DoOpenFile(TSSHFileName.Format(Server, FileName), '', TabControlIndex(ActiveTabControl));
  end;
end;

procedure TPyIDEMainForm.actRestoreEditorExecute(Sender: TObject);
begin
  if LocalAppStorage.PathExists('Layouts\BeforeZoom\Forms') then begin
    LoadLayout('BeforeZoom');
    LocalAppstorage.DeleteSubTree('Layouts\BeforeZoom');
  end;
end;

procedure TPyIDEMainForm.actEditorZoomOutExecute(Sender: TObject);
begin
  if ActiveControl is TSynEdit then begin
    TSynEdit(ActiveControl).Font.Size :=
      Max(TSynEdit(ActiveControl).Font.Size - 1, 4);
    TSynEdit(ActiveControl).Gutter.Font.Size :=
      TSynEdit(ActiveControl).Font.Size - 2;
  end;
end;

procedure TPyIDEMainForm.actEditorZoomInExecute(Sender: TObject);
begin
  if ActiveControl is TSynEdit then begin
    TSynEdit(ActiveControl).Font.Size :=
      TSynEdit(ActiveControl).Font.Size + 1;
    TSynEdit(ActiveControl).Gutter.Font.Size :=
      TSynEdit(ActiveControl).Font.Size - 2;
  end;
end;

procedure TPyIDEMainForm.actEditorZoomResetExecute(Sender: TObject);
begin
  if ActiveControl is TSynEdit then begin
    TSynEdit(ActiveControl).Font.Size := EditorOptions.Font.Size;
    TSynEdit(ActiveControl).Gutter.Font.Size := EditorOptions.Gutter.Font.Size;
  end;
end;

procedure TPyIDEMainForm.mnFilesClick(Sender: TObject);
//Fill in the Files submenu of the Tabs popup menu
Var
  Editor, ActiveEditor : IEditor;
  List : TStringList;
  MenuItem : TSpTBXItem;
  ModifiedImageIndex : Integer;
begin
  mnFiles.Clear;
  ActiveEditor := GetActiveEditor;
  List := TStringList.Create;
  List.Duplicates := dupAccept;
  List.Sorted := True;
  try
    GI_EditorFactory.ApplyToEditors(procedure(Ed: IEditor)
    begin
      List.AddObject(Ed.FileId, TObject(Ed.Form));
    end);
    ModifiedImageIndex := vilImages.GetIndexByName('Edit');
    for var I:= 0 to List.Count - 1 do begin
      Editor := TEditorForm(List.Objects[I]).GetEditor;
      MenuItem := TSpTBXItem.Create(Self);
      mnFiles.Add(MenuItem);
      MenuItem.Caption := List[I];
      MenuItem.GroupIndex := 3;
      MenuItem.Hint := Editor.FileId;
      MenuItem.Checked := Editor = ActiveEditor;
      if Editor.Modified then
        MenuItem.ImageIndex := ModifiedImageIndex;
      MenuItem.OnClick := SelectEditor;
    end;
  finally
    List.Free;
  end;
end;

procedure TPyIDEMainForm.mnLanguageClick(Sender: TObject);
begin
  ChangeLanguage(fLanguageList[(Sender as TSpTBXItem).Tag]);
  SetupSyntaxMenu;
  SetupToolsMenu;
end;

procedure TPyIDEMainForm.mnSpellingPopup(Sender: TTBCustomItem; FromLink:
    Boolean);
var
  Error: ISpellingError;
  CorrectiveAction: CORRECTIVE_ACTION;
  Replacement: PChar;
  MenuItem: TTBCustomItem;
  Action: TSynSpellErrorReplace;
  Suggestions: IEnumString;
  Suggestion: PWideChar;
  Fetched: LongInt;
  Indicator: TSynIndicator;
  AWord: string;
  HaveError: Boolean;
  Editor: TCustomSynEdit;
begin
  Editor := CommandsDataModule.SynSpellCheck.Editor;
  if not Assigned(Editor) then
    Exit;

  // Remove replacement menu items and actions;
  repeat
    MenuItem := mnSpelling.Items[0];
    if MenuItem.Action is TSynSpellErrorReplace then
    begin
      mnSpelling.Remove(MenuItem);
      MenuItem.Action.Free;
      MenuItem.Free;
    end
    else
      Break;
  until (False);

  if not Assigned(CommandsDataModule.SynSpellCheck.SpellChecker()) then
  begin
    mnSpelling.Visible := False;
    Exit;
  end;

  if Editor.Indicators.IndicatorAtPos(Editor.CaretXY,
   TSynSpellCheck.SpellErrorIndicatorId, Indicator)
  then
     AWord := Copy(Editor.Lines[Editor.CaretY - 1], Indicator.CharStart,
       Indicator.CharEnd - Indicator.CharStart)
  else
    AWord := '';

  CommandsDataModule.SynSpellCheck.Editor := Editor;
  Error := CommandsDataModule.SynSpellCheck.ErrorAtPos(Editor.CaretXY);
  HaveError := Assigned(Error) and (AWord <> '');

  mnSpellCheckTopSeparator.Visible := HaveError;
  mnSpellCheckSecondSeparator.Visible := HaveError;
  mnSpellCheckAdd.Visible := HaveError;
  mnSpellCheckIgnore.Visible := HaveError;
  mnSpellCheckIgnoreOnce.Visible := HaveError;
  mnSpellCheckDelete.Visible := HaveError;


  if HaveError then
  begin
    Error.Get_CorrectiveAction(CorrectiveAction);
    case CorrectiveAction of
      CORRECTIVE_ACTION_GET_SUGGESTIONS:
        begin
          CheckOSError(CommandsDataModule.SynSpellCheck.SpellChecker.Suggest(
            PChar(AWord), Suggestions));
          while Suggestions.Next(1, Suggestion, @Fetched) = S_OK do
          begin
            Action := TSynSpellErrorReplace.Create(Self);
            Action.Caption := Suggestion;
            MenuItem := TSpTBXItem.Create(Self);
            MenuItem.Action := Action;
            mnSpelling.Insert(mnSpelling.IndexOf(mnSpellCheckTopSeparator), MenuItem);
            CoTaskMemFree(Suggestion);
          end;
        end;
      CORRECTIVE_ACTION_REPLACE:
        begin
          Error.Get_Replacement(Replacement);
          Action := TSynSpellErrorReplace.Create(Self);
          Action.Caption := Replacement;
          MenuItem := TSpTBXItem.Create(Self);
          MenuItem.Action := Action;
          mnSpelling.Insert(0, MenuItem);
        end;
    end;
  end;
end;

procedure TPyIDEMainForm.tbiScrollLeftClick(Sender: TObject);
begin
  TabControl((Sender as TSPTBXItem).Tag).ScrollLeft;
end;

procedure TPyIDEMainForm.tbiScrollRightClick(Sender: TObject);
begin
  TabControl((Sender as TSPTBXItem).Tag).ScrollRight;
end;

procedure TPyIDEMainForm.tbiSearchOptionsPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
begin
  with EditorSearchOptions do begin
    tbiSearchFromCaret.Checked := SearchFromCaret;
    tbiSearchInSelection.Checked := SearchSelectionOnly;
    tbiWholeWords.Checked := SearchWholeWords;
    tbiRegExp.Checked := UseRegExp;
    tbiAutoCaseSensitive.Checked := SearchCaseSensitiveType = scsAuto;
    tbiCaseSensitive.Checked := SearchCaseSensitiveType = scsCaseSensitive;
    tbiCaseSensitive.Enabled := not tbiAutoCaseSensitive.Checked;
    tbiIncrementalSearch.Checked := IncrementalSearch;
  end;
end;

procedure TPyIDEMainForm.tbiSearchTextAcceptText(const NewText: string);
Var
  S : string;
  i: integer;
begin
  if NewText <> '' then begin
    // update Items
    i := tbiSearchText.Items.IndexOf(NewText);
    if i > -1 then
      tbiSearchText.Items.Delete(i);
    tbiSearchText.Items.Insert(0, NewText);
    tbiSearchText.Text := NewText;
    tbiSearchText.Perform(WM_KEYDOWN, VK_END, 0);

    // Update History
    S := '';
    for i := 0 to tbiSearchText.Items.Count - 1 do begin
      if i >= 10 then
        break;
      if i > 0 then
        S :=  S + ',';
      S := S + AnsiQuotedStr(tbiSearchText.Items[i], '"');
    end;
    EditorSearchOptions.SearchTextHistory := S;
  end;
end;

procedure TPyIDEMainForm.tbiSearchTextKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = Char(VK_ESCAPE)) and not tbiSearchText.DroppedDown then begin
    Key := #0;
    FindToolbar.Visible := False;
  end else if (Key = Char(VK_RETURN)) and not tbiSearchText.DroppedDown then begin
    Key := #0;
    tbiSearchTextAcceptText(tbiSearchText.Text);
    CommandsDataModule.actSearchFindNext.Execute;
  end;
end;

procedure TPyIDEMainForm.tbiSearchTextChange(Sender: TObject);
begin
  if EditorSearchOptions.SearchText <> tbiSearchText.Text then begin
    EditorSearchOptions.SearchText := tbiSearchText.Text;
    EditorSearchOptions.InitSearch;
    CommandsDataModule.UpdateMainActions;

    ClearAllHighlightedTerms;
    if CommandsDataModule.actSearchHighlight.Enabled and
      CommandsDataModule.actSearchHighlight.Checked
    then
      CommandsDataModule.actSearchHighlightExecute(Sender);

    if EditorSearchOptions.IncrementalSearch then
      PostMessage(Handle, WM_SEARCHREPLACEACTION, 2, 0);
  end;
end;

procedure TPyIDEMainForm.tbiSearchTextExit(Sender: TObject);
begin
  tbiSearchTextAcceptText(tbiSearchText.Text);
end;

procedure TPyIDEMainForm.tbiRecentFileListClick(Sender: TObject;
  const Filename: string);
Var
  S : string;
begin
  S := FileName;
  DoOpenFile(S, '', TabControlIndex(ActiveTabControl));
  // A bit problematic since it Frees the MRU Item which calls this click handler
  tbiRecentFileList.MRURemove(S);
end;

procedure TPyIDEMainForm.tbiRecentProjectsClick(Sender: TObject;
  const Filename: string);
begin
  ProjectExplorerWindow.DoOpenProjectFile(FileName);
  tbiRecentProjects.MRURemove(Filename);
end;

procedure TPyIDEMainForm.tbiReplaceTextAcceptText(const NewText: string);
Var
  S : string;
  i: integer;
begin
  if NewText <> '' then begin
    // update Items
    i := tbiReplaceText.Items.IndexOf(NewText);
    if i > -1 then
      tbiReplaceText.Items.Delete(i);
    tbiReplaceText.Items.Insert(0, NewText);
    tbiReplaceText.Text := NewText;
    tbiReplaceText.Perform(WM_KEYDOWN, VK_END, 0);

    // Update History
    S := '';
    for i := 0 to tbiReplaceText.Items.Count - 1 do begin
      if i >= 10 then
        break;
      if i > 0 then
        S := S + ',';
      S := S + tbiReplaceText.Items[i].QuotedString('"');
    end;
    EditorSearchOptions.ReplaceTextHistory := S;
  end;
end;

procedure TPyIDEMainForm.tbiReplaceTextChange(Sender: TObject);
begin
  EditorSearchOptions.ReplaceText := tbiReplaceText.Text;
  EditorSearchOptions.InitSearch;
  CommandsDataModule.UpdateMainActions;
end;

procedure TPyIDEMainForm.tbiReplaceTextKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = Char(VK_ESCAPE)) and not tbiReplaceText.DroppedDown then begin
    Key := #0;
    FindToolbar.Visible := False;
  end else if (Key = Char(VK_RETURN)) and not tbiReplaceText.DroppedDown then begin
    Key := #0;
    tbiReplaceTextAcceptText(tbiReplaceText.Text);
    CommandsDataModule.actSearchReplaceNow.Execute;
//    PostMessage(Handle, WM_SEARCHREPLACEACTION, 0, LPARAM(Action));
  end;
end;

procedure TPyIDEMainForm.SearchOptionsChanged(Sender: TObject);
begin
  with EditorSearchOptions do begin
    SearchFromCaret := tbiSearchFromCaret.Checked;
    SearchSelectionOnly := tbiSearchInSelection.Checked;
    SearchWholeWords := tbiWholeWords.Checked;
    UseRegExp := tbiRegExp.Checked;
    if tbiAutoCaseSensitive.Checked then
      SearchCaseSensitiveType := scsAuto
    else if tbiCaseSensitive.Checked then
      SearchCaseSensitiveType := scsCaseSensitive
    else
      SearchCaseSensitiveType := scsNotCaseSenitive;
    IncrementalSearch := tbiIncrementalSearch.Checked and not SearchWholeWords;
    InitSearch;
  end;
  ClearAllHighlightedTerms;
  if CommandsDataModule.actSearchHighlight.Enabled and
    CommandsDataModule.actSearchHighlight.Checked
  then
    CommandsDataModule.actSearchHighlightExecute(Sender);
end;

procedure TPyIDEMainForm.SelectEditor(Sender: TObject);
begin
    ShowFilePosition((Sender as TTBCustomItem).Hint, -1, -1);
end;

{ TTSpTBXTabControl }

procedure TSpTBXTabControl.WMDropFiles(var Msg: TMessage);
var
  i, iNumberDropped: Integer;
  FileName: array[0..MAX_PATH - 1] of Char;
begin
  try
    iNumberDropped := DragQueryFile(THandle(Msg.wParam), Cardinal(-1),
      nil, 0);

    for i := 0 to iNumberDropped - 1 do
    begin
      DragQueryFile(THandle(Msg.wParam), i, FileName, MAX_PATH);
      PyIDEMainForm.DoOpenFile(FileName, '', PyIDEMainForm.TabControlIndex(Self));
    end;
  finally
    Msg.Result := 0;
    DragFinish(THandle(Msg.wParam));
  end;
end;

constructor TSpTBXTabControl.Create(AOwner: TComponent);
begin
  inherited;
  zOrder := TList.Create;
end;

destructor TSpTBXTabControl.Destroy;
begin
  FreeAndNil(zOrder);
  inherited;
end;

end.


