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
            New IDE Option added "File template for new python scirpts"  (Issue 385)
            New IDE Option added "Auto completion font"  (Issue 365)
            French translation by Groupe AmiensPython added
          Bug fixes
            Issues  297, 307, 346, 354, 358, 371, 375, 376, 382, 384, 387, 389

  History:   v 2.3.3
          New Features
            Native unicode strings throught (speed improvements on XP)
            Revamped Code Explorer (Issues 192, 163, 213, 225)
            Improvements to Code completion
            -  Auto-completion for the import statement in python 2.5 and later (Issue 230)
            -  Processing of function return statements
            -  Background module parsing and caching of parsed modules
            Start-up python scripts pyscripter_init.py and python_init.py. See help file for details.
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
          Issues addressed
            #848
            { TODO : Python Engine change without exiting PyScripter }
            { TODO : Issues 501, 659, 667 }
            { TODO : Review Search and Replace }
            { TODO : Auto PEP8 tool }
            { TODO: Interpreter raw_input #311 }


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

{$I SynEdit.inc}

interface

uses
  System.Types, System.UITypes, WinAPI.Windows, WinAPI.Messages,
  System.SysUtils, System.Classes, Graphics, Controls, Forms,
  Variants, dmCommands, ActnList, Menus, uEditAppIntfs,
  JvDockControlForm, JvDockVIDStyle, JvDockVSNetStyle,
  SynEditTypes, SynEditMiscClasses, cPyBaseDebugger,
  cPyDebugger, JvAppStorage,  JvAppIniStorage, SynEdit,
  TB2Dock, TB2Toolbar, TB2Item, ExtCtrls, JvExControls,
  cRefactoring, dlgCustomShortcuts,   TB2ExtItems, JvDockTree,
  JvComponentBase, JvAppInst, uHighlighterProcs, cFileTemplates,
  JvDockVSNetStyleSpTBX, JvFormPlacement, SpTBXCustomizer,
  SpTbxSkins, SpTBXItem, SpTBXEditors, StdCtrls, JvDSADialogs, Dialogs,
  ActiveX, SpTBXMDIMRU, SpTBXTabs, ImgList, SpTBXDkPanels, System.Actions,
  VCL.Styles, Vcl.Styles.DPIAware, Vcl.ComCtrls, AMHLEDVecStd,
  cPyScripterSettings;

const
  WM_FINDDEFINITION  = WM_USER + 100;
  WM_CHECKFORUPDATES = WM_USER + 110;
  WM_UPDATEBREAKPOINTS  = WM_USER + 120;
  WM_SEARCHREPLACEACTION  = WM_USER + 130;
  WM_EXECCLOSE  = WM_USER + 140;

type
  { Trick to add functionality to TTSpTBXTabControl}
  TSpTBXTabControl = class(SpTBXTabs.TSPTBXTabControl)
  private
    zOrderPos : integer;
    zOrderProcessing : Boolean;
  public
    zOrder : TList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


  TPyIDEMainForm = class(TForm, IDropTarget)
    DockServer: TJvDockServer;
    AppStorage: TJvAppIniFileStorage;
    BGPanel: TPanel;
    CloseTimer: TTimer;
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
    JvFormStorage: TJvFormStorage;
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
    StatusLED: TAMHLEDVecStd;
    tbciStatusLed: TTBControlItem;
    ExternalToolsLED: TAMHLEDVecStd;
    tbciStatusExternal: TTBControlItem;
    SpTBXSeparatorItem10: TSpTBXSeparatorItem;
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
    JvDockVSNetStyleSpTBX: TJvDockVSNetStyleSpTBX;
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
    SpTBXItem4: TSpTBXItem;
    SpTBXItem5: TSpTBXItem;
    procedure mnFilesClick(Sender: TObject);
    procedure actEditorZoomInExecute(Sender: TObject);
    procedure actEditorZoomOutExecute(Sender: TObject);
    procedure mnNoSyntaxClick(Sender: TObject);
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
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure CloseTimerTimer(Sender: TObject);
    procedure actImportModuleExecute(Sender: TObject);
    procedure actViewToDoListExecute(Sender: TObject);
    procedure actViewFindResultsExecute(Sender: TObject);
    procedure actViewOutputExecute(Sender: TObject);
    procedure actExternalRunExecute(Sender: TObject);
    procedure actExternalRunConfigureExecute(Sender: TObject);
    procedure actFindDefinitionExecute(Sender: TObject);
    procedure actFindReferencesExecute(Sender: TObject);
    procedure PreviousListClick(Sender: TObject; S : WideString);
    procedure tbiBrowsePreviousClick(Sender: TObject);
    procedure NextListClick(Sender: TObject; S : WideString);
    procedure tbiBrowseNextClick(Sender: TObject);
    function ApplicationHelp(Command: Word; Data: {$IF CompilerVersion >= 23}NativeInt{$ELSE}integer{$IFEND};
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
    procedure tbiRecentFileListClick(Sender: TObject;
      const Filename: WideString);
    procedure tbiSearchTextExit(Sender: TObject);
    procedure tbiReplaceTextKeyPress(Sender: TObject; var Key: Char);
    procedure TabControlActiveTabChange(Sender: TObject; TabIndex: Integer);
    procedure tbiScrollLeftClick(Sender: TObject);
    procedure tbiScrollRightClick(Sender: TObject);
    procedure actViewSplitWorkspaceVerExecute(Sender: TObject);
    procedure actViewSplitWorkspaceHorExecute(Sender: TObject);
    procedure actViewHideSecondaryWorkspaceExecute(Sender: TObject);
    procedure tbiRecentProjectsClick(Sender: TObject;
      const Filename: WideString);
    procedure actSelectStyleExecute(Sender: TObject);
  private
    DSAAppStorage: TDSAAppStorage;
    ShellExtensionFiles : TStringList;
//    function FindAction(var Key: Word; Shift: TShiftState) : TCustomAction;
    procedure DebugActiveScript(ActiveEditor: IEditor;
      InitStepIn : Boolean = False; RunToCursorLine : integer = -1);
    procedure SetupRunConfiguration(var RunConfig: TRunConfiguration; ActiveEditor: IEditor);
    procedure tbiSearchTextAcceptText(const NewText: WideString);
    procedure tbiReplaceTextAcceptText(const NewText: WideString);
    procedure DrawCloseButton(Sender: TObject; ACanvas: TCanvas;
        State: TSpTBXSkinStatesType; const PaintStage: TSpTBXPaintStage;
        var AImageList: TCustomImageList; var AImageIndex: Integer;
        var ARect: TRect; var PaintDefault: Boolean);
    procedure SetupCustomizer;
    procedure RunInitScript;
    function GetActiveTabControl: TSpTBXCustomTabControl;
    procedure SetActiveTabControl(const Value: TSpTBXCustomTabControl);
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure ApplyIDEOptionsToEditor(Editor: IEditor);
  protected
    fCurrentLine : integer;
    fErrorLine : integer;
    fCurrentBrowseInfo : string;
    function DoCreateEditor(TabControl : TSpTBXTabControl): IEditor;
    function CmdLineOpenFiles(): boolean;
    function OpenCmdLineFile(FileName : string) : Boolean;
    procedure DebuggerBreakpointChange(Sender: TObject; Editor : IEditor; ALine: integer);
    procedure DebuggerCurrentPosChange(Sender: TObject);
    procedure UpdateStandardActions;
    procedure UpdateStatusBarPanels;
    procedure ApplicationOnHint(Sender: TObject);
    procedure ApplcationOnShowHint(var HintStr: string; var CanShow: Boolean;
      var HintInfo: Controls.THintInfo);
    procedure ApplicationActionUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure ApplicationActionExecute(Action: TBasicAction; var Handled: Boolean);
    procedure TabToolBarDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TabToolbarlDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure WMFindDefinition(var Msg: TMessage); message WM_FINDDEFINITION;
    procedure WMUpdateBreakPoints(var Msg: TMessage); message WM_UPDATEBREAKPOINTS;
    procedure WMSearchReplaceAction(var Msg: TMessage); message WM_SEARCHREPLACEACTION;
    procedure WMExecCLose(var Msg: TMessage); message WM_EXECCLOSE;
    procedure WMCheckForUpdates(var Msg: TMessage); message WM_CHECKFORUPDATES;
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    procedure SyntaxClick(Sender : TObject);
    procedure SelectEditor(Sender : TObject);
    procedure mnLanguageClick(Sender: TObject);
    // Browse MRU stuff
    procedure PrevClickHandler(Sender: TObject);
    procedure NextClickHandler(Sender: TObject);
    procedure PrevMRUAdd(S : WideString);
    procedure NextMRUAdd(S : WideString);
  protected
    fStoredEffect : Longint;
    OldScreenPPI : Integer;
    OldDesktopSize : string;
    // IDropTarget implementation
    function DragEnter(const dataObj: IDataObject; grfKeyState: Longint;
      pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function DropTargetDragOver(grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;
    function IDropTarget.DragOver= DropTargetDragOver;
    function DragLeave: HResult; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;
    procedure ScaleForCurrentDpi; override;
  public
    StyleDPIAwareness : TStyleDPIAwareness;
    ActiveTabControlIndex : integer;
    PythonKeywordHelpRequested : Boolean;
    MenuHelpRequested : Boolean;
    Layouts : TStringList;
    fLanguageList : TStringList;
    procedure SaveEnvironment;
    procedure StoreApplicationData;
    procedure RestoreApplicationData;
    function DoOpenFile(AFileName: string; HighlighterName : string = '';
       TabControlIndex : integer = 1) : IEditor;
    function NewFileFromTemplate(FileTemplate : TFileTemplate;
       TabControlIndex : integer = 1) : IEditor;
    function GetActiveEditor : IEditor;
    procedure SaveFileModules;
    procedure UpdateDebugCommands(DebuggerState : TDebuggerState);
    procedure SetRunLastScriptHints(ScriptName : string);
    function ShowFilePosition(FileName : string; Line, Offset : integer; SelLen : integer = 0;
         ForceToMiddle : boolean = True; FocusEditor : boolean = True) : boolean;
    procedure DebuggerStateChange(Sender: TObject; OldState,
      NewState: TDebuggerState);
    procedure ApplicationOnIdle(Sender: TObject; var Done: Boolean);
    procedure DebuggerYield(Sender: TObject; DoIdle : Boolean);
    procedure DebuggerErrorPosChange(Sender: TObject);
    procedure SetCurrentPos(Editor : IEditor; ALine: integer);
    procedure PyIDEOptionsChanged;
    procedure SetupLanguageMenu;
    procedure SetupToolsMenu;
    procedure SetupLayoutsMenu;
    procedure SetupSyntaxMenu;
    procedure LayoutClick(Sender : TObject);
    procedure LoadLayout(const Layout : string);
    procedure LoadToolbarLayout(const Layout: string);
    procedure LoadToolbarItems(const Path : string);
    procedure SaveLayout(const Layout : string);
    procedure SaveToolbarLayout(const Layout: string);
    procedure SaveToolbarItems(const Path : string);
    procedure WriteStatusMsg(S : string);
    function JumpToFilePosInfo(FilePosInfo : string) : boolean;
    procedure FindDefinition(Editor : IEditor; TextCoord : TBufferCoord;
      ShowMessages, Silent, JumpToFirstMatch : Boolean; var FilePosInfo : string);
    procedure AdjustBrowserLists(FileName: string; Line: Integer; Col: Integer;
      FilePosInfo: string);
    procedure ThemeEditorGutter(Gutter : TSynGutter);
    procedure UpdateCaption;
    procedure ChangeLanguage(LangCode : string);
    function EditorFromTab(Tab : TSpTBXTabItem) : IEditor;
    procedure SplitWorkspace(SecondTabsVisible : Boolean;
      Alignment : TAlign = alRight; Size : integer = -1);
    procedure MoveTab(Tab : TSpTBXTabItem; TabControl : TSpTBXTabControl;
      Index : integer = -1);
    function TabControl(TabControlIndex : integer = 1) : TSpTBXTabControl;
    function TabControlIndex(TabControl : TSpTBXCustomTabControl) : integer;
    procedure ConfigureFCN(FCN : TFileChangeNotificationType);
    property ActiveTabControl : TSpTBXCustomTabControl read GetActiveTabControl
      write SetActiveTabControl;
  end;

Const
  ctkRemember : TDSACheckTextKind = 100;
  dsaSearchFromStart = 1;
  dsaReplaceFromStart = 2;
  dsaReplaceNumber = 3;
  dsaSearchStartReached = 4;
  dsaPostMortemInfo = 5;

  FactoryToolbarItems = 'Factory Toolbar Items v1.0';

var
  PyIDEMainForm: TPyIDEMainForm;

implementation

uses
  frmPythonII, frmMessages, frmEditor,
  frmCallStack, frmBreakPoints, frmVariables, frmWatches,
  frmCodeExplorer, frmFileExplorer, JclFileUtils, frmToDo,
  frmFindResults, uParams, cTools, cParameters,
  frmCommandOutput, JvCreateProcess, dlgToolProperties, uCommonFunctions,
  SynHighlighterPython, SynEditHighlighter, SynRegExpr,
  JvJVCLUtils, DateUtils, cPythonSourceScanner, frmRegExpTester,
  StringResources, dlgCommandLine, frmUnitTests, cFilePersist, frmIDEDockWin,
  dlgPickList, VirtualTrees, VirtualExplorerTree, Math,
  cCodeHint, dlgNewFile, JclSysInfo, cPyRemoteDebugger,
  uCmdLine, uSearchHighlighter, frmModSpTBXCustomize, IniFiles,
  JclStrings, JclSysUtils, frmProjectExplorer, cProjectClasses,
  MPDataObject, JvGnugettext,
  SpTBXControls, SynEditKeyCmds, StdActns,
  PythonEngine, Contnrs, SynCompletionProposal, dlgStyleSelector,
  Vcl.Themes;

{$R *.DFM}

{ TWorkbookMainForm }

function TPyIDEMainForm.DoCreateEditor(TabControl : TSpTBXTabControl): IEditor;
begin
  if GI_EditorFactory <> nil then begin
    Result := GI_EditorFactory.CreateTabSheet(TabControl);
    Result.SynEdit.Assign(EditorOptions);
    Result.SynEdit2.Assign(EditorOptions);
    TEditorForm(Result.Form).ParentTabItem.OnTabClosing := TabControlTabClosing;
    TEditorForm(Result.Form).ParentTabItem.OnDrawTabCloseButton := DrawCloseButton;
    ApplyIDEOptionsToEditor(Result);
  end else
    Result := nil;
end;

function TPyIDEMainForm.DoOpenFile(AFileName: string; HighlighterName : string = '';
       TabControlIndex : integer = 1) : IEditor;
Var
  TabCtrl : TSpTBXTabControl;
begin
  Result := nil;
  AFileName := GetLongFileName(ExpandFileName(AFileName));
  if AFileName <> '' then begin
    // activate the editor if already open
    Assert(GI_EditorFactory <> nil);
    Result :=  GI_EditorFactory.GetEditorByName(AFileName);
    if Assigned(Result) then begin
      Result.Activate;
      Exit;
    end;
  end;
  // create a new editor, add it to the editor list, open the file
  TabCtrl := TabControl(TabControlIndex);
  TabCtrl.Toolbar.BeginUpdate;
  try
    Result := DoCreateEditor(TabCtrl);
    if Result <> nil then begin
      try
        Result.OpenFile(AFileName, HighlighterName);
        tbiRecentFileList.MRURemove(AFileName);
        Result.Activate;
      except
        Result.Close;
        raise
      end;
      if (AFileName <> '') and (GI_EditorFactory.Count = 2) and
        (GI_EditorFactory.Editor[0].FileName = '') and
        not GI_EditorFactory.Editor[0].Modified
      then
        GI_EditorFactory.Editor[0].Close;
      if (AFileName = '') and (HighlighterName = 'Python') then
        TEditorForm(Result.Form).DefaultExtension := 'py';
    end;
  finally
    TabCtrl.Toolbar.EndUpdate;
    if Assigned(TabCtrl.ActiveTab) then
      TabCtrl.MakeVisible(TabCtrl.ActiveTab);
    UpdateCaption;
  end;
end;

function TPyIDEMainForm.DragEnter(const dataObj: IDataObject;
  grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;

  function GetFormatEtc: TFormatEtc;
  begin
    Result.cfFormat := CF_HDROP; // This guy is always registered for all applications
    Result.ptd := nil;
    Result.dwAspect := DVASPECT_CONTENT;
    Result.lindex := -1;
    Result.tymed := TYMED_HGLOBAL
  end;

begin
  if Succeeded(dataObj.QueryGetData(GetFormatEtc)) then
    dwEffect := DROPEFFECT_COPY
  else
    dwEffect := DROPEFFECT_NONE;
  fStoredEffect := dwEffect;
  Result := S_OK;
end;

function TPyIDEMainForm.DragLeave: HResult;
begin
    Result := S_OK;
end;

function TPyIDEMainForm.DropTargetDragOver(grfKeyState: Integer; pt: TPoint;
  var dwEffect: Integer): HResult;
begin
  dwEffect := fStoredEffect;
  Result := S_OK;
end;

procedure TPyIDEMainForm.EditorViewsMenuClick(Sender: TObject);
begin
  GI_EditorFactory.UpdateEditorViewMenu;
end;

function TPyIDEMainForm.Drop(const dataObj: IDataObject; grfKeyState: Integer;
  pt: TPoint; var dwEffect: Integer): HResult;
Var
  CommonHDrop : TCommonHDrop;
  FileName : string;
  i : integer;
  TabCtrlIndx : integer;
begin
  Result := S_OK;
  if TabControl2.Visible then
    TabCtrlIndx :=
      IfThen(PtInRect(TabControl2.ClientRect, TabControl2.ScreenToClient(pt)), 2, 1)
  else
    TabCtrlIndx := 1;

  CommonHDrop := TCommonHDrop.Create;
  try
    if CommonHDrop.LoadFromDataObject(dataObj) then begin
      for i := 0 to CommonHDrop.FileCount - 1 do begin
        FileName := CommonHDrop.FileName(i);
        if FileExists(FileName) then  // checks it is not a directory
          try
            DoOpenFile(FileName, '', TabCtrlIndx);
          except
          end;
      end;
    end;
  finally
    CommonHDrop.Free;
  end;
end;

type
  TTBCustomItemAccess = class(TTBCustomItem);

procedure TPyIDEMainForm.FormCreate(Sender: TObject);
Var
  TabHost : TJvDockTabHostForm;
  OptionsFileName: string;
begin
  // Style DPI awareness
  StyleDPIAwareness := TStyleDPIAwareness.Create(Self);
  StyleDPIAwareness.Parent := Self;

  // App Instances
  ShellExtensionFiles := TStringList.Create;
  if not CmdLineReader.readFlag('NEWINSTANCE') then begin
    JvAppInstances.Active := True;
    JvAppInstances.Check;
  end;

// Trying to reduce flicker!
  ControlStyle := ControlStyle + [csOpaque];

  SkinManager.AddSkinNotification(Self);

  // JvDocking Fonts
  with JvDockVSNetStyleSpTBX.TabServerOption as TJvDockVIDTabServerOption do begin
    ActiveFont.Assign(ToolbarFont);
    InactiveFont.Assign(ToolbarFont);
  end;

  //  Layout stuff
  Layouts := TStringList.Create;
  Layouts.Sorted := True;
  Layouts.Duplicates := dupError;

  // Application Storage
  OptionsFileName := ChangeFileExt(ExtractFileName(Application.ExeName), '.ini');
  AppStorage.Encoding := TEncoding.UTF8;
  if FileExists(ChangeFileExt(Application.ExeName, '.ini')) then begin
    AppStorage.Location := flExeFile;
    AppStorage.FileName := OptionsFileName;
  end else if FileExists(IncludeTrailingPathDelimiter(GetAppdataFolder) + OptionsFileName) then begin
    AppStorage.Location := flUserFolder;
    AppStorage.FileName := OptionsFileName;
  end else  // default location
    AppStorage.FileName :=
      CommandsDataModule.UserDataPath + OptionsFileName;

  // DSA stuff
  DSAAppStorage := TDSAAppStorage.Create(AppStorage, 'DSA');
  RegisterDSACheckMarkText(ctkRemember, 'Remember answer and do not show again');
  RegisterDSA(dsaSearchFromStart, 'SearchFromStart', 'Search from start question', DSAAppStorage, ctkRemember);
  RegisterDSA(dsaReplaceFromStart, 'ReplaceFromStart', 'Replace srom start question', DSAAppStorage, ctkRemember);
  RegisterDSA(dsaReplaceNumber, 'ReplaceNumber', 'Information about number of replacements', DSAAppStorage, ctkShow);
  RegisterDSA(dsaSearchStartReached, 'SearchStartReached', 'Information: search start reached', DSAAppStorage, ctkShow);
  RegisterDSA(dsaPostMortemInfo, 'PostMortemInfo', 'Instructions: Post Mortem', DSAAppStorage, ctkShow);

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

  // Setup Languages
  fLanguageList := TStringList.Create;
  SetUpLanguageMenu;
  // And now translate after all the docking forms have been created
  // They will be translated as well
  TP_GlobalIgnoreClass(TJvFormStorage);
  TranslateComponent(Self);

  // Assign Debugger Events
  with PyControl do begin
    OnBreakpointChange := DebuggerBreakpointChange;
    OnCurrentPosChange := DebuggerCurrentPosChange;
    OnErrorPosChange := DebuggerErrorPosChange;
    OnStateChange := DebuggerStateChange;
    OnYield := DebuggerYield;
  end;

  // ActionLists
  TActionProxyCollection.ActionLists :=
    [actlStandard,
     CommandsDataModule.actlMain,
     PythonIIForm.InterpreterActionList,
     ProjectExplorerWindow.ProjectActionList,
     CallStackWindow.actlCallStack];

  // Store Factory Settings
  if not AppStorage.PathExists(FactoryToolbarItems) then
    SaveToolbarItems(FactoryToolbarItems);

  // Read Settings from PyScripter.ini
  if FileExists(AppStorage.IniFile.FileName) then begin
    RestoreApplicationData;
    if (OldScreenPPI = Screen.PixelsPerInch) and
       ((OldDesktopSize = '') or (OldDesktopSize = DesktopSizeString))
    then
      JvFormStorage.RestoreFormPlacement;
  end else
    PyIDEOptions.Changed;

  if (OldScreenPPI = Screen.PixelsPerInch) and
     ((OldDesktopSize = '') or (OldDesktopSize = DesktopSizeString)) and
     AppStorage.PathExists('Layouts\Current\Forms') then
  begin
    LoadLayout('Current');
    AppStorage.DeleteSubTree('Layouts\Current');
  end else
  begin
    TabHost := ManualTabDock(DockServer.LeftDockPanel, FileExplorerWindow, ProjectExplorerWindow);
    DockServer.LeftDockPanel.Width := PPIScaled(200);
    ManualTabDockAddPage(TabHost, CodeExplorerWindow);
    ShowDockForm(FileExplorerWindow);

    TabHost := ManualTabDock(DockServer.BottomDockPanel, CallStackWindow, VariablesWindow);
    DockServer.BottomDockPanel.Height := PPIScaled(150);
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

  UpdateDebugCommands(PyControl.DebuggerState);
  //  Editor Views Menu
  GI_EditorFactory.SetupEditorViewMenu;

  Update;

  // Tab Conrol Drag Drop
  TabControl1.Toolbar.OnDragOver := TabToolbarDragOver;
  TabControl1.Toolbar.OnDragDrop := TabToolbarlDragDrop;
  TabControl2.Toolbar.OnDragOver := TabToolbarDragOver;
  TabControl2.Toolbar.OnDragDrop := TabToolbarlDragDrop;

  TabControl1.Toolbar.BeginUpdate;
  TabControl2.Toolbar.BeginUpdate;
  try
    // Open Files on the command line
    // if there was no file on the command line try restoring open files
    if not CmdLineOpenFiles() and PyIDEOptions.RestoreOpenFiles then
      TPersistFileInfo.ReadFromAppStorage(AppStorage, 'Open Files');

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
    // Start the Python Code scanning thread
    CodeExplorerWindow.WorkerThread.Start;
  end;

  TabControl1.Toolbar.OnMouseDown := TabControlMouseDown;
  TabControl2.Toolbar.OnMouseDown := TabControlMouseDown;
  //Set the HelpFile
  Application.HelpFile := ExtractFilePath(Application.ExeName) + 'PyScripter.chm';
  Application.OnHelp := Self.ApplicationHelp;

  // Execute pyscripter_init.py
  RunInitScript;

  SkinManager.AddSkinNotification(Self);
  SkinManager.BroadcastSkinNotification;
end;

procedure TPyIDEMainForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if JvGlobalDockIsLoading then begin
    CanClose := False;
    CloseTimer.Enabled := True;
    Exit;
  end else if PyControl.DebuggerState <> dsInactive then begin
    if Dialogs.MessageDlg(_(SAbortDebugging), mtWarning, [mbYes, mbNo], 0) = mrYes then
    begin
      if (PyControl.DebuggerState in [dsPaused, dsPostMortem]) or
        (PyControl.ActiveDebugger is TPyInternalDebugger) then
      begin
        CanClose := False;
        PyControl.ActiveDebugger.Abort;
        CloseTimer.Enabled := True;
        Exit;
      end else begin
        CanClose := False;
        PyControl.ActiveInterpreter.ReInitialize;
        CloseTimer.Enabled := True;
        Exit;
      end;
    end else begin  // mrNo
       CanClose := False;
       Exit;
    end;
  end;

  if OutputWindow.JvCreateProcess.State <> psReady then
    if Dialogs.MessageDlg(_(SKillExternalTool), mtConfirmation, [mbYes, mbCancel], 0) = mrYes
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
    // Shut down CodeExplorerWindow Worker thread
    CodeExplorerWindow.ShutDownWorkerThread;

    // Disconnect ChangeNotify
    FileExplorerWindow.FileExplorerTree.Active := False;
    ConfigureFCN(fcnDisabled);

    // Disable CodeHint timer
    CodeHint.CancelHint;

    // Shut down help
    Application.OnHelp := nil;
    // QC25183
    try
      Application.HelpCommand(HELP_QUIT, 0);
    except
    end;

    // Stop DropTarget to make sure is unregistered
    RevokeDragDrop(TabControl1.Handle);
    RevokeDragDrop(TabControl2.Handle);


    VariablesWindow.ClearAll;
    UnitTestWindow.ClearAll;
    CallStackWindow.ClearAll;

    // Give the time to the treads to terminate
    Sleep(200);

    //  We need to do this here so that MRU and docking information are persisted
    try
      SaveEnvironment;
    except
      on E: EFileStreamError do
        Dialogs.MessageDlg(Format(_(SFileSaveError), [AppStorage.FullFileName, E.Message]), mtError, [mbOK], 0);
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
  PyIDEMainForm.JvDockVSNetStyleSpTBX.ChannelOption.MouseleaveHide := False;
end;

procedure TPyIDEMainForm.actNavEditorExecute(Sender: TObject);
Var
  Editor : IEditor;
begin
  Editor := GetActiveEditor;
  if Assigned(Editor) then
    Editor.Activate;
end;

procedure TPyIDEMainForm.actNavFileExplorerExecute(Sender: TObject);
begin
  ShowDockForm(FileExplorerWindow);
  FileExplorerWindow.FormActivate(Sender);
  // only when activated by the menu or the keyboard - Will be reset by frmIDEDockWin
  PyIDEMainForm.JvDockVSNetStyleSpTBX.ChannelOption.MouseleaveHide := False;
end;

procedure TPyIDEMainForm.actNavInterpreterExecute(Sender: TObject);
begin
  ShowDockForm(PythonIIForm);
  PythonIIForm.FormActivate(Sender);
  // only when activated by the menu or the keyboard - Will be reset by frmIDEDockWin
  PyIDEMainForm.JvDockVSNetStyleSpTBX.ChannelOption.MouseleaveHide := False;
end;

procedure TPyIDEMainForm.actNavMessagesExecute(Sender: TObject);
begin
  ShowDockForm(MessagesWindow);
  MessagesWindow.FormActivate(Sender);
  // only when activated by the menu or the keyboard - Will be reset by frmIDEDockWin
  PyIDEMainForm.JvDockVSNetStyleSpTBX.ChannelOption.MouseleaveHide := False;
end;

procedure TPyIDEMainForm.actNavOutputExecute(Sender: TObject);
begin
  ShowDockForm(OutputWindow);
  OutputWindow.FormActivate(Sender);
  // only when activated by the menu or the keyboard - Will be reset by frmIDEDockWin
  PyIDEMainForm.JvDockVSNetStyleSpTBX.ChannelOption.MouseleaveHide := False;
end;

procedure TPyIDEMainForm.actNavProjectExplorerExecute(Sender: TObject);
begin
  ShowDockForm(ProjectExplorerWindow);
  ProjectExplorerWindow.FormActivate(Sender);
  // only when activated by the menu or the keyboard - Will be reset by frmIDEDockWin
  PyIDEMainForm.JvDockVSNetStyleSpTBX.ChannelOption.MouseleaveHide := False;
end;

procedure TPyIDEMainForm.actNavRETesterExecute(Sender: TObject);
begin
  ShowDockForm(RegExpTesterWindow);
  RegExpTesterWindow.FormActivate(Sender);
  // only when activated by the menu or the keyboard - Will be reset by frmIDEDockWin
  PyIDEMainForm.JvDockVSNetStyleSpTBX.ChannelOption.MouseleaveHide := False;
end;

procedure TPyIDEMainForm.actNavTodoExecute(Sender: TObject);
begin
  ShowDockForm(ToDoWindow);
  ToDoWindow.FormActivate(Sender);
  // only when activated by the menu or the keyboard - Will be reset by frmIDEDockWin
  PyIDEMainForm.JvDockVSNetStyleSpTBX.ChannelOption.MouseleaveHide := False;
end;

procedure TPyIDEMainForm.actNavUnitTestsExecute(Sender: TObject);
begin
  ShowDockForm(UnitTestWindow);
  UnitTestWindow.FormActivate(Sender);
  // only when activated by the menu or the keyboard - Will be reset by frmIDEDockWin
  PyIDEMainForm.JvDockVSNetStyleSpTBX.ChannelOption.MouseleaveHide := False;
end;

procedure TPyIDEMainForm.actNavVariablesExecute(Sender: TObject);
begin
  ShowDockForm(VariablesWindow);
  VariablesWindow.FormActivate(Sender);
  // only when activated by the menu or the keyboard - Will be reset by frmIDEDockWin
  PyIDEMainForm.JvDockVSNetStyleSpTBX.ChannelOption.MouseleaveHide := False;
end;

procedure TPyIDEMainForm.actNavWatchesExecute(Sender: TObject);
begin
  ShowDockForm(WatchesWindow);
  WatchesWindow.FormActivate(Sender);
  // only when activated by the menu or the keyboard - Will be reset by frmIDEDockWin
  PyIDEMainForm.JvDockVSNetStyleSpTBX.ChannelOption.MouseleaveHide := False;
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
begin
  PyControl.PythonEngineType := TPythonEngineType((Sender as TAction).Tag);
end;

procedure TPyIDEMainForm.actPythonReinitializeExecute(Sender: TObject);
begin
  if PyControl.DebuggerState <> dsInactive then begin
    if Dialogs.MessageDlg(_(STerminateInterpreter),
      mtWarning, [mbYes, mbNo], 0) = idNo then Exit;
  end;
  PyControl.ActiveInterpreter.ReInitialize;
end;

procedure TPyIDEMainForm.actSyntaxCheckExecute(Sender: TObject);
var
  ActiveEditor : IEditor;
begin
  ActiveEditor := GetActiveEditor;
  if not Assigned(ActiveEditor) then Exit;

  if InternalInterpreter.SyntaxCheck(ActiveEditor) then begin
    MessagesWindow.AddMessage(Format(_(SSyntaxIsOK), [ActiveEditor.FileTitle]));
    ShowDockForm(MessagesWindow);
  end;
end;

procedure TPyIDEMainForm.actImportModuleExecute(Sender: TObject);
var
  ActiveEditor : IEditor;
begin
  ActiveEditor := GetActiveEditor;
  if not Assigned(ActiveEditor) then Exit;

  PyControl.ActiveInterpreter.ImportModule(ActiveEditor, True);

  MessagesWindow.AddMessage(Format(_(SModuleImportedOK), [ActiveEditor.FileTitle]));
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
  with TCommandLineDlg.Create(Self) do begin
    SynParameters.Text := PyIDEOptions.CommandLine;
    cbUseCommandLine.Checked := PyIDEOptions.UseCommandLine;
    if ShowModal = mrOk then begin
      PyIDEOptions.CommandLine := SynParameters.Text;
      PyIDEOptions.UseCommandLine := cbUseCommandLine.Checked;
    end;
    Release;
  end;
end;

procedure TPyIDEMainForm.actRunDebugLastScriptExecute(Sender: TObject);
begin
  if PyControl.DebuggerState = dsInactive then
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
  //MessageBeep(MB_ICONASTERISK);
end;

procedure TPyIDEMainForm.actRunLastScriptExecute(Sender: TObject);
begin
  if PyControl.DebuggerState = dsInactive then
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
  Assert(not PyControl.IsRunning);
  ActiveEditor := GetActiveEditor;
  if Assigned(ActiveEditor) then begin
    if PyControl.DebuggerState = dsInactive then
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
  Assert(not PyControl.IsRunning);
  ActiveEditor := GetActiveEditor;
  if Assigned(ActiveEditor) then begin
    if PyControl.DebuggerState = dsInactive then
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
    if PyControl.DebuggerState = dsInactive then
      DebugActiveScript(ActiveEditor, False, ActiveEditor.SynEdit.CaretY)
    else if PyControl.DebuggerState = dsPaused then
      PyControl.ActiveDebugger.RunToCursor(ActiveEditor, ActiveEditor.SynEdit.CaretY);
  end;
end;

procedure TPyIDEMainForm.DebuggerBreakpointChange(Sender: TObject; Editor : IEditor;
  ALine: integer);
begin
  PostMessage(Handle, WM_UPDATEBREAKPOINTS, 0, 0);
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
end;

procedure TPyIDEMainForm.UpdateCaption;
Var
  Editor : IEditor;
begin
  if TabControl1.Toolbar.IsUpdating or TabControl2.Toolbar.IsUpdating then
    Exit;

  Editor := GetActiveEditor;
  if Assigned(Editor) then
    Caption := Format('PyScripter - %s%s', [Editor.GetFileNameOrTitle,
                         iff(Editor.Modified, '*', '')])
  else
    Caption := 'PyScripter';
end;

procedure TPyIDEMainForm.SetupRunConfiguration(var RunConfig: TRunConfiguration; ActiveEditor: IEditor);
begin
  RunConfig.ScriptName := ActiveEditor.GetFileNameOrTitle;
  RunConfig.EngineType := PyControl.PythonEngineType;
  RunConfig.Parameters := iff(PyIDEOptions.UseCommandLine, PyIDEOptions.CommandLine, '');
  RunConfig.ExternalRun.Assign(ExternalPython);
  RunConfig.ExternalRun.Parameters := Parameters.ReplaceInText(RunConfig.ExternalRun.Parameters);
  RunConfig.ReinitializeBeforeRun := PyIDEOptions.ReinitializeBeforeRun;
end;

procedure TPyIDEMainForm.DebugActiveScript(ActiveEditor: IEditor;
  InitStepIn : Boolean = False; RunToCursorLine : integer = -1);
var
  RunConfig: TRunConfiguration;
begin
  Assert(PyControl.DebuggerState = dsInactive);
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
    (Editor.SynEdit.Highlighter = CommandsDataModule.SynPythonSyn);

  actSyntaxCheck.Enabled := PyFileActive and (DebuggerState = dsInactive);
  actRun.Enabled := PyFileActive and (DebuggerState = dsInactive);
  actExternalRun.Enabled := PyFileActive and (DebuggerState = dsInactive);
  actImportModule.Enabled := PyFileActive and (DebuggerState = dsInactive);
  actDebug.Enabled := PyFileActive and (DebuggerState in [dsInactive, dsPaused]);
  actStepInto.Enabled := PyFileActive and (DebuggerState in [dsInactive, dsPaused]);
  actStepOut.Enabled := DebuggerState = dsPaused;
  actStepOver.Enabled := DebuggerState = dsPaused;
  actDebugAbort.Enabled := DebuggerState in [dsPaused, dsDebugging, dsRunning, dsPostMortem];
  actDebugPause.Enabled := DebuggerState = dsDebugging;
  actRunToCursor.Enabled := PyFileActive and (DebuggerState in [dsInactive, dsPaused])
    and PyControl.IsExecutableLine(Editor, Editor.SynEdit.CaretY);
  actToggleBreakPoint.Enabled := PyFileActive;
  actClearAllBreakPoints.Enabled := PyFileActive;
  actAddWatchAtCursor.Enabled := PyFileActive;
  actExecSelection.Enabled := not PyControl.IsRunning and PyFileActive;
  actPythonReinitialize.Enabled := Assigned(PyControl.ActiveInterpreter) and
    (icReInitialize in PyControl.ActiveInterpreter.InterpreterCapabilities) and
    not (PyControl.DebuggerState in [dsPaused, dsPostMortem]);
  actPostMortem.Enabled := (PyControl.DebuggerState = dsInactive) and
    Assigned(PyControl.ActiveDebugger) and PyControl.ActiveDebugger.HaveTraceback;
  if DebuggerState = dsPaused then begin
    actDebug.Caption := _(SResumeCaption);
    actDebug.Hint := _(SResumeHint);
  end else begin
    actDebug.Caption := _('Debug');
    actDebug.Hint := _(SDebugHint);
  end;
  actRunLastScript.Enabled := (DebuggerState = dsInactive) and (PyControl.RunConfig.ScriptName <> '');
  actRunDebugLastScript.Enabled := actRunLastScript.Enabled;
  actRunLastScriptExternal.Enabled := actRunLastScript.Enabled;

  CallStackWindow.actPreviousFrame.Enabled := (DebuggerState = dsPaused);
  CallStackWindow.actNextFrame.Enabled := (DebuggerState = dsPaused);

  Refresh;
end;

procedure TPyIDEMainForm.SetActiveTabControl(const Value: TSpTBXCustomTabControl);
begin
  ActiveTabControlIndex := TabControlIndex(Value);
end;

procedure TPyIDEMainForm.SetCurrentPos(Editor : IEditor; ALine: integer);
Var
  ActiveEditor : IEditor;
begin
  ActiveEditor := GetActiveEditor;
  if not Assigned(ActiveEditor) then Exit;  //No editors!

  if (not Assigned(Editor) or (ActiveEditor = Editor)) and (fCurrentLine > 0) then
    // Remove possible current lines
    with ActiveEditor do begin
      SynEdit.InvalidateGutterLine(fCurrentLine);
      SynEdit.InvalidateLine(fCurrentLine);
      SynEdit2.InvalidateGutterLine(fCurrentLine);
      SynEdit2.InvalidateLine(fCurrentLine);
    end;

  fCurrentLine := ALine;  // Store
  if not Assigned(Editor) then Exit;

  if Editor <> ActiveEditor then
    Editor.Activate;

  with Editor.SynEdit do begin
    if (ALine > 0) and (CaretY <> ALine) then begin
      CaretXY := BufferCoord(1, ALine);
      EnsureCursorPosVisible;
    end;
    InvalidateGutterLine(ALine);
    InvalidateLine(ALine);
  end;
  Editor.SynEdit2.InvalidateGutterLine(ALine);
  Editor.SynEdit2.InvalidateLine(ALine);
end;

procedure TPyIDEMainForm.SetRunLastScriptHints(ScriptName: string);
Var
  S : string;
begin
   S := ExtractFileName(ScriptName);
   if S <> '' then
     S := Format(' - %s ', [S]);
   actRunLastScript.Hint := _(sHintRun) + S;
   actRunDebugLastScript.Hint := _(sHintDebug) + S;
   actRunLastScriptExternal.Hint := _(sHintExternalRun) + S;
end;

procedure TPyIDEMainForm.DebuggerCurrentPosChange(Sender: TObject);
begin
  if (PyControl.ActiveDebugger <> nil) and not PyControl.IsRunning then
    SetCurrentPos(PyControl.CurrentPos.Editor , PyControl.CurrentPos.Line)
  else
    SetCurrentPos(PyControl.CurrentPos.Editor, -1);
end;

procedure TPyIDEMainForm.DebuggerErrorPosChange(Sender: TObject);
{
  Invalidates old and/or new error line but does not Activate the Editor
}
var
  Editor : IEditor;
begin
  Editor := GetActiveEditor;
  if not Assigned(Editor) then Exit;  //No editors!

  if (not Assigned(PyControl.ErrorPos.Editor) or (PyControl.ErrorPos.Editor = Editor)) and
    (fErrorLine > 0)
  then begin
    // Remove possible error line
    Editor.SynEdit.InvalidateLine(fErrorLine);
    Editor.SynEdit2.InvalidateLine(fErrorLine);
  end;
  fErrorLine := PyControl.ErrorPos.Line;  // Store
  if (Editor = PyControl.ErrorPos.Editor) and (PyControl.ErrorPos.Line > 0) then begin
    Editor.SynEdit.InvalidateLine(PyControl.ErrorPos.Line);
    Editor.SynEdit2.InvalidateLine(PyControl.ErrorPos.Line);
  end;
end;

procedure TPyIDEMainForm.DebuggerStateChange(Sender: TObject; OldState,
  NewState: TDebuggerState);
var
  s: string;
begin
  if csDestroying in ComponentState then Exit;

  case NewState of
    dsDebugging,
    dsRunning: begin
                        s := 'Running';
                        if PyIDEOptions.PythonEngineType = peInternal then
                          Screen.Cursor := crHourGlass;
                        StatusLED.LEDColorOn := clRed;
                      end;
    dsPaused: begin
                s := 'Paused';
                Screen.Cursor := crDefault;
                StatusLED.LEDColorOn := clYellow;
              end;
    dsInactive: begin
                 s := 'Ready';
                 Screen.Cursor := crDefault;
                 StatusLED.LEDColorOn := clGreen;
               end;
    dsPostMortem : begin
                     s := 'Post mortem';
                     Screen.Cursor := crDefault;
                     StatusLED.LEDColorOn := clPurple;
                   end;
  end;
  StatusLED.Hint := 'Debugger state: ' +s;
  lbStatusMessage.Caption := ' ' + s;
  StatusBar.Refresh;

  CallStackWindow.UpdateWindow(NewState, OldState);  // also updates Variables and Watches
  UpdateDebugCommands(NewState);
end;

procedure TPyIDEMainForm.DebuggerYield(Sender: TObject; DoIdle : Boolean);
begin
  Application.ProcessMessages;
  // HandleMessage calls Application.Idle which yields control to other applications
  if DoIdle then
    // HandleMessage calls Application.Idle which yields control to other applications
    // and calls CheckSynchronize which runs synchronized methods initiated in threads
    //Application.HandleMessage
    Application.DoApplicationIdle
  else
    CheckSynchronize;
end;

procedure TPyIDEMainForm.SaveFileModules;
var
  i : integer;
  FileCommands : IFileCommands;
begin
  for i := 0 to GI_EditorFactory.Count -1 do
    if (GI_EditorFactory[i].FileName <> '') and GI_EditorFactory[i].Modified then begin
      FileCommands := GI_EditorFactory[i] as IFileCommands;
      if Assigned(FileCommands) then
        FileCommands.ExecSave;
    end;
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
  // TODO Modified stuff
//  for i := 0 to TabControl.PagesCount - 1 do
//    if i < TabControl.Tabs.Count then
//      TabControl.Tabs[i].Modified :=
//        TEditorForm(TJvStandardPage(TabControl.Tabs[i].Data).Components[0]).GetEditor.Modified;
  if Assigned(GI_ActiveEditor) then
    TEditorForm(GI_ActiveEditor.Form).DoOnIdle;
  PythonIIForm.DoOnIdle;

  // If a Tk or Wx remote engine is active pump up event handling
  // This is for processing input output coming from event handlers
  if (PyControl.ActiveInterpreter is TPyRemoteInterpreter) and
     (PyControl.DebuggerState = dsInactive)
  then
    with(TPyRemoteInterpreter(PyControl.ActiveInterpreter)) do begin
      if IsConnected and (EngineType in [peRemoteTk, peRemoteWx]) then
        // Ignore exceptions here
          ServeConnection;
    end;

  if ShellExtensionFiles.Count > 0 then begin
    for i := 0 to ShellExtensionFiles.Count - 1 do
      OpenCmdLineFile(ShellExtensionFiles[i]);
    ShellExtensionFiles.Clear;
  end;
  Done := True;
end;

procedure TPyIDEMainForm.ApplicationOnHint(Sender: TObject);
begin
  WriteStatusMsg(GetLongHint(Application.Hint));
end;

procedure TPyIDEMainForm.ApplcationOnShowHint(var HintStr: string;
  var CanShow: Boolean; var HintInfo: Controls.THintInfo);
begin
  if HintInfo.HintControl is TBaseVirtualTree then
    HintInfo.HideTimeout := 5000;
end;

function TPyIDEMainForm.ShowFilePosition(FileName: string; Line,
  Offset: integer; SelLen : integer = 0; ForceToMiddle : boolean = True;
  FocusEditor : boolean = True): boolean;
Var
  Editor : IEditor;
begin
  Result := False;
  if FileName <> '' then begin
    if (FileName[1] ='<') and (FileName[Length(FileName)] = '>') then
      FileName :=  Copy(FileName, 2, Length(FileName)-2);
    Editor := GI_EditorFactory.GetEditorByNameOrTitle(FileName);
    if not Assigned(Editor) and FileExists(FileName) then begin
      try
        DoOpenFile(FileName, '', TabControlIndex(ActiveTabControl));
      except
      end;
      Editor := GI_EditorFactory.GetEditorByNameOrTitle(FileName);
    end;
    if Assigned(Editor) then begin
      Result := True;
      Sleep(200);
      Application.ProcessMessages;  // to deal with focus problems
      // sets the focus to the editor
      if (Editor <> GetActiveEditor) or FocusEditor then
        Editor.Activate;
      if (Line > 0) then
        with Editor.SynEdit do begin
          CaretXY := BufferCoord(Offset,Line);
          EnsureCursorPosVisibleEx(ForceToMiddle);
          if SelLen > 0 then
             SelLength := SelLen;
        end;
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
begin
  if GI_ActiveEditor <> nil then begin
    ptCaret := GI_ActiveEditor.GetCaretPos;
    if (ptCaret.X > 0) and (ptCaret.Y > 0) then
      lbStatusCaret.Caption := Format(' %6d:%3d ', [ptCaret.Y, ptCaret.X])
    else
      lbStatusCaret.Caption := '';
    if GI_ActiveEditor.GetModified then
      lbStatusModified.Caption := _(SModified)
    else
      lbStatusModified.Caption := '';
    lbStatusOverwrite.Caption := GI_ActiveEditor.GetEditorState;
  end else begin
    lbStatusCaret.Caption := '';
    lbStatusModified.Caption := '';
    lbStatusOverwrite.Caption := '';
  end;
  if GetCapsLockKeyState then
    lbStatusCAPS.Caption := 'CAPS'
  else
    lbStatusCAPS.Caption := ' ';

  ExternalToolsLED.Visible := OutputWindow.JvCreateProcess.State <> psReady;
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

procedure TPyIDEMainForm.ConfigureFCN(FCN: TFileChangeNotificationType);
begin
  case FCN of
    fcnFull:
      with FileExplorerWindow.FileExplorerTree do begin
        TreeOptions.VETMiscOptions :=
          TreeOptions.VETMiscOptions + [toChangeNotifierThread, toTrackChangesInMappedDrives];
        // Connect ChangeNotify
        OnAfterShellNotify := CommandsDataModule.ProcessShellNotify;
      end;
    fcnNoMappedDrives:
      with FileExplorerWindow.FileExplorerTree do begin
        TreeOptions.VETMiscOptions :=
          TreeOptions.VETMiscOptions + [toChangeNotifierThread];
        TreeOptions.VETMiscOptions :=
          TreeOptions.VETMiscOptions - [toTrackChangesInMappedDrives];
        // Connect ChangeNotify
        OnAfterShellNotify := CommandsDataModule.ProcessShellNotify;
      end;
    fcnDisabled:
      with FileExplorerWindow.FileExplorerTree do begin
        TreeOptions.VETMiscOptions :=
          TreeOptions.VETMiscOptions - [toChangeNotifierThread, toTrackChangesInMappedDrives];
        // Connect ChangeNotify
        OnAfterShellNotify := nil;
      end;
  end;
end;

procedure TPyIDEMainForm.ApplyIDEOptionsToEditor(Editor: IEditor);
begin
  with TEditorForm(Editor.Form) do
  begin
    SynCodeCompletion.Options := PythonIIForm.SynCodeCompletion.Options;
    SynCodeCompletion.TriggerChars := PythonIIForm.SynCodeCompletion.TriggerChars;
    SynCodeCompletion.TimerInterval := PythonIIForm.SynCodeCompletion.TimerInterval;
    Synedit.CodeFolding.Assign(PyIDEOptions.CodeFolding);
    Synedit2.CodeFolding.Assign(PyIDEOptions.CodeFolding);

    if PyIDEOptions.CompactLineNumbers then
    begin
      SynEdit.OnGutterGetText := TEditorForm(Editor.Form).SynEditGutterGetText;
      SynEdit2.OnGutterGetText := TEditorForm(Editor.Form).SynEditGutterGetText;
    end
    else
    begin
      SynEdit.OnGutterGetText := nil;
      SynEdit2.OnGutterGetText := nil;
    end;
    SynEdit.InvalidateGutter;
    SynEdit2.InvalidateGutter;
  end;
end;

procedure TPyIDEMainForm.FormDestroy(Sender: TObject);
begin
  SkinManager.RemoveSkinNotification(Self);
  FreeAndNil(Layouts);
  FreeAndNil(fLanguageList);
  FreeAndNil(DSAAppStorage);
  FreeAndNil(ShellExtensionFiles);
  FreeAndNil(StyleDPIAwareness);
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
  with CommandsDataModule.dlgFileOpen do begin
    Title := _(SOpenFile);
    FileName := '';
    Filter := GetHighlightersFilter(CommandsDataModule.Highlighters) + _(SFilterAllFiles);
    Editor := GetActiveEditor;
    if Assigned(Editor) and (Editor.FileName <> '') and
      (ExtractFileDir(Editor.FileName) <> '')
    then
      InitialDir := ExtractFileDir(Editor.FileName);

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
var
  Editor : IEditor;
  i : integer;
  CaseSensitive,
  CompleteAsYouType,
  CompleteWithWordBreakChars : Boolean;
begin
  FileExplorerWindow.FileExplorerTree.RefreshTree;
  EditorSearchOptions.SearchTextAtCaret :=
    PyIDEOptions.SearchTextAtCaret;
  MaskFPUExceptions(PyIDEOptions.MaskFPUExceptions);
  CommandsDataModule.SynPythonSyn.DefaultFilter :=
    PyIDEOptions.PythonFileFilter;
  CommandsDataModule.SynCythonSyn.DefaultFilter :=
    PyIDEOptions.CythonFileFilter;
  CommandsDataModule.SynWebHTMLSyn.DefaultFilter :=
    PyIDEOptions.HTMLFileFilter;
  CommandsDataModule.SynWebXMLSyn.DefaultFilter :=
    PyIDEOptions.XMLFileFilter;
  CommandsDataModule.SynWebCssSyn.DefaultFilter :=
    PyIDEOptions.CSSFileFilter;
  CommandsDataModule.SynCppSyn.DefaultFilter :=
    PyIDEOptions.CPPFileFilter;
  CommandsDataModule.SynYAMLSyn.DefaultFilter :=
    PyIDEOptions.YAMLFileFilter;
  //  Dock animation parameters
  JvDockVSNetStyleSpTBX.SetAnimationInterval(PyIDEOptions.DockAnimationInterval);
  JvDockVSNetStyleSpTBX.SetAnimationMoveWidth(PyIDEOptions.DockAnimationMoveWidth);

  // Set Python engine
  actPythonInternal.Visible := not PyIDEOptions.InternalInterpreterHidden;
  if not actPythonInternal.Visible and
     (PyIDEOptions.PythonEngineType = peInternal)
  then
    PyIDEOptions.PythonEngineType := peRemote;

  PyControl.PythonEngineType := PyIDEOptions.PythonEngineType;

  // Command History Size
  PythonIIForm.CommandHistorySize := PyIDEOptions.InterpreterHistorySize;

  if PyIDEOptions.ShowTabCloseButton then begin
    TabControl1.TabCloseButton := tcbAll;
    TabControl2.TabCloseButton := tcbAll;
  end else begin
    TabControl1.TabCloseButton := tcbNone;
    TabControl2.TabCloseButton := tcbNone;
  end;
  if TabControl1.TabPosition <> PyIDEOptions.EditorsTabPosition then
    case PyIDEOptions.EditorsTabPosition of
      ttpTop:
        begin
          TabControl1.TabPosition := ttpTop;
          TabControl2.TabPosition := ttpTop;
          for i  := 0 to GI_EditorFactory.Count - 1 do
            TEditorForm(GI_EditorFactory.Editor[i].Form).ViewsTabControl.TabPosition := ttpBottom;
        end;
      ttpBottom:
        begin
          TabControl1.TabPosition := ttpBottom;
          TabControl2.TabPosition := ttpBottom;
          for i  := 0 to GI_EditorFactory.Count - 1 do
            TEditorForm(GI_EditorFactory.Editor[i].Form).ViewsTabControl.TabPosition := ttpTop;
        end;
    end;

  ConfigureFCN(PyIDEOptions.FileChangeNotification);

  // Code completion
  CaseSensitive := PyIDEOptions.CodeCompletionCaseSensitive;
  CompleteAsYouType := PyIDEOptions.CompleteAsYouType;
  CompleteWithWordBreakChars := PyIDEOptions.CompleteWithWordBreakChars;

  with PythonIIForm do begin
    with SynCodeCompletion do begin
      if CaseSensitive then Options := Options + [scoCaseSensitive]
      else Options := Options - [scoCaseSensitive];
      if CompleteWithWordBreakChars then Options := Options + [scoEndCharCompletion]
      else Options := Options - [scoEndCharCompletion];

      TriggerChars := '.';
      if CompleteAsYouType then begin
        for i := ord('a') to ord('z') do TriggerChars := TriggerChars + Chr(i);
        for i := ord('A') to ord('Z') do TriggerChars := TriggerChars + Chr(i);
        TimerInterval := 10;
      end
      else
        TimerInterval := 300;
    end;
  end;

  for i := 0 to GI_EditorFactory.Count - 1 do begin
    Editor := GI_EditorFactory.Editor[i];
    ApplyIDEOptionsToEditor(Editor);
  end;

  tbiRecentFileList.MaxItems :=  PyIDEOptions.NoOfRecentFiles;

  Editor := GetActiveEditor;
  if Assigned(Editor) then
    Editor.SynEdit.InvalidateGutter;
end;

procedure TPyIDEMainForm.StoreApplicationData;
Var
  TempStringList : TStringList;
  ActionProxyCollection : TActionProxyCollection;
  i : integer;
  TempCursor : IInterface;
begin
  TempCursor := WaitCursor;
  TempStringList := TStringList.Create;
  AppStorage.BeginUpdate;
  try
    AppStorage.WriteString('PyScripter Version', ApplicationVersion);
    AppStorage.WriteString('Language', GetCurrentLanguage);
    AppStorage.WriteInteger('Screen PPI', Screen.PixelsPerInch);
    AppStorage.WriteString('Desktop size', DeskTopSizeString);

    AppStorage.StorageOptions.StoreDefaultValues := True;
    // UnScale and Scale back
    PyIDEOptions.CodeFolding.GutterShapeSize :=
      PPIUnScaled(PyIDEOptions.CodeFolding.GutterShapeSize);
    AppStorage.WritePersistent('IDE Options', PyIDEOptions);
    PyIDEOptions.CodeFolding.GutterShapeSize :=
      PPIScaled(PyIDEOptions.CodeFolding.GutterShapeSize);
    AppStorage.StorageOptions.StoreDefaultValues := False;

    with CommandsDataModule do begin
      AppStorage.DeleteSubTree('Editor Options');
      AppStorage.WritePersistent('Editor Options', EditorOptions);

      AppStorage.DeleteSubTree('Highlighters');
      for i := 0 to Highlighters.Count - 1 do
        AppStorage.WritePersistent('Highlighters\'+Highlighters[i],
          TPersistent(Highlighters.Objects[i]));
      AppStorage.WritePersistent('Highlighters\Intepreter',
        PythonIIForm.SynEdit.Highlighter);

      AppStorage.DeleteSubTree('Interpreter Editor Options');
      TempStringList.Add('KeyStrokes');
      AppStorage.WritePersistent('Interpreter Editor Options',
        InterpreterEditorOptions, True, TempStringList);

      AppStorage.WritePersistent('Editor Search Options', EditorSearchOptions);

      TempStringList.Clear;
      TempStringList.Add('Lines');
      TempStringList.Add('Highlighter');
      AppStorage.DeleteSubTree('Print Options');
      AppStorage.WritePersistent('Print Options', SynEditPrint, True, TempStringList);
      AppStorage.WriteString('Print Options\HeaderItems', SynEditPrint.Header.AsString);
      AppStorage.WriteString('Print Options\FooterItems', SynEditPrint.Footer.AsString);

      AppStorage.StorageOptions.PreserveLeadingTrailingBlanks := True;
      AppStorage.DeleteSubTree('File Templates');
      AppStorage.WriteObjectList('File Templates', FileTemplates);

      TempStringList.Assign(CodeTemplatesCompletion.AutoCompleteList);
      AppStorage.WriteStringList('Code Templates', TempStringList);
      AppStorage.StorageOptions.PreserveLeadingTrailingBlanks := False;
    end;
    AppStorage.WritePersistent('Secondary Tabs', TabsPersistsInfo);
    AppStorage.WritePersistent('ToDo Options', ToDoExpert);
    AppStorage.DeleteSubTree('Find in Files Options');
    AppStorage.WritePersistent('Find in Files Options', FindResultsWindow.FindInFilesExpert);
    AppStorage.WritePersistent('Find in Files Results Options', FindResultsWindow);
    AppStorage.WritePersistent('Variables Window Options', VariablesWindow);
    AppStorage.WritePersistent('Call Stack Window Options', CallStackWindow);
    AppStorage.WritePersistent('Breakpoints Window Options', BreakPointsWindow);
    AppStorage.WritePersistent('Messages Window Options', MessagesWindow);
    AppStorage.WritePersistent('RegExp Tester Options', RegExpTesterWindow);
    AppStorage.WriteBoolean('File Explorer Filter', FileExplorerWindow.actEnableFilter.Checked);
    AppStorage.WriteString('File Explorer Path', FileExplorerWindow.ExplorerPath);
    AppStorage.WriteStringList('File Explorer Favorites', FileExplorerWindow.Favorites);
    AppStorage.WritePersistent('Code Explorer Options', CodeExplorerWindow);

    AppStorage.WriteStringList('Custom Params', CustomParams);
    AppStorage.DeleteSubTree('Tools');
    AppStorage.WriteCollection('Tools', ToolsCollection, 'Tool');
    AppStorage.WritePersistent('Tools\External Run', ExternalPython);
    AppStorage.WriteString('Output Window\Font Name', OutputWindow.lsbConsole.Font.Name);
    AppStorage.WriteInteger('Output Window\Font Size', OutputWindow.lsbConsole.Font.Size);
    AppStorage.WritePersistent('Watches', WatchesWindow);
    AppStorage.WriteBoolean('Status Bar', StatusBar.Visible);
    AppStorage.WriteStringList('Layouts', Layouts);

    // Save Style Name
    AppStorage.WriteString('Style Name', TStyleSelectorForm.CurrentSkinName);


    // Save Toolbar Items
    SaveToolbarItems('Toolbar Items');

    //  Needed since save toolbar Items below does not save secondary shortcuts! Issue 307
    // Save IDE Shortcuts
    AppStorage.DeleteSubTree('IDE Shortcuts');
    ActionProxyCollection := TActionProxyCollection.Create(apcctChanged);
    try
      AppStorage.WriteCollection('IDE Shortcuts', ActionProxyCollection, 'Action');
    finally
      ActionProxyCollection.Free;
    end;

    // Save Interpreter History
    TempStringList.Clear;
    for I := 0 to PythonIIForm.CommandHistory.Count - 1 do
      TempStringList.Add(StrStringToEscaped(PythonIIForm.CommandHistory[i]));
    AppStorage.StorageOptions.PreserveLeadingTrailingBlanks := True;
    AppStorage.WriteStringList('Command History', TempStringList);
    AppStorage.StorageOptions.PreserveLeadingTrailingBlanks := False;

    // Form Placement
    JvFormStorage.SaveFormPlacement;

  finally
    AppStorage.EndUpdate;
    TempStringList.Free;
  end;

  // Save MRU Lists
  tbiRecentFileList.SaveToIni(AppStorage.IniFile, 'MRU File List');
  tbiRecentProjects.SaveToIni(AppStorage.IniFile, 'MRU Project List');

  // Project Filename
  AppStorage.WriteString('Active Project', ActiveProject.FileName);

  AppStorage.Flush;
end;

procedure TPyIDEMainForm.RestoreApplicationData;
Const
  DefaultHeader='$TITLE$\.1\.0\.-13\.Arial\.0\.96\.10\.0\.1\.2';
  DefaultFooter='$PAGENUM$\\.$PAGECOUNT$\.1\.0\.-13\.Arial\.0\.96\.10\.0\.1\.2';
Var
  ActionProxyCollection : TActionProxyCollection;
  TempStringList : TStringList;
  i : Integer;
  FName : string;
  PyScripterVersion : string;
begin
  PyScripterVersion := AppStorage.ReadString('PyScripter Version', '1.0');

  // Change language
  ChangeLanguage(AppStorage.ReadString('Language', GetCurrentLanguage));

  OldScreenPPI := AppStorage.ReadInteger('Screen PPI', 96);
  OldDesktopSize := AppStorage.ReadString('Desktop size');

  if AppStorage.PathExists('IDE Options') then begin
    AppStorage.ReadPersistent('IDE Options', PyIDEOptions);
    PyIDEOptions.CodeFolding.GutterShapeSize :=
      PPIScaled(PyIDEOptions.CodeFolding.GutterShapeSize);
    PyIDEOptions.Changed;
  end;
  if AppStorage.PathExists('Editor Options') then
    with CommandsDataModule do begin
      EditorOptions.Gutter.Gradient := False;  //default value
      AppStorage.ReadPersistent('Editor Options', EditorOptions);
      if (CompareVersion(PyScripterVersion, '3.1') < 0) then
      begin
        if  (EditorOptions.Keystrokes.FindCommand(ecNone) < 0) then
        with EditorOptions.Keystrokes do begin
          AddKey(ecFoldAll, VK_OEM_MINUS, [ssCtrl, ssShift]);   {- _}
          AddKey(ecUnfoldAll,  VK_OEM_PLUS, [ssCtrl, ssShift]); {= +}
          AddKey(ecFoldNearest, VK_OEM_2, [ssCtrl]);  // Divide {'/'}
          AddKey(ecUnfoldNearest, VK_OEM_2, [ssCtrl, ssShift]);
          AddKey(ecFoldLevel1, ord('K'), [ssCtrl], Ord('1'), [ssCtrl]);
          AddKey(ecFoldLevel2, ord('K'), [ssCtrl], Ord('2'), [ssCtrl]);
          AddKey(ecFoldLevel3, ord('K'), [ssCtrl], Ord('3'), [ssCtrl]);
          AddKey(ecUnfoldLevel1, ord('K'), [ssCtrl, ssShift], Ord('1'), [ssCtrl, ssShift]);
          AddKey(ecUnfoldLevel2, ord('K'), [ssCtrl, ssShift], Ord('2'), [ssCtrl, ssShift]);
          AddKey(ecUnfoldLevel3, ord('K'), [ssCtrl, ssShift], Ord('3'), [ssCtrl, ssShift]);
        end;
        EditorOptions.Gutter.DigitCount := 2;
        EditorOptions.Options := EditorOptions.Options +
          [eoAutoIndent, eoAutoSizeMaxScrollWidth, eoScrollPastEol];
        EditorOptions.MaxScrollWidth := 100;
      end;

      //AppStorage.DeleteSubTree('Editor Options');

      for i := 0 to Highlighters.Count - 1 do
        AppStorage.ReadPersistent('Highlighters\'+Highlighters[i],
          TPersistent(Highlighters.Objects[i]));
      CommandsDataModule.ApplyEditorOptions;
      if AppStorage.PathExists('Highlighters\Intepreter') then
        AppStorage.ReadPersistent('Highlighters\Intepreter',
          PythonIIForm.SynEdit.Highlighter);

      AppStorage.DeleteSubTree('Highlighters');

      if AppStorage.PathExists('Interpreter Editor Options') then begin
        InterpreterEditorOptions.Gutter.Gradient := False;  //default value
        AppStorage.ReadPersistent('Interpreter Editor Options', InterpreterEditorOptions);
        InterpreterEditorOptions.Options := (InterpreterEditorOptions.Options -
          [eoTrimTrailingSpaces, eoScrollPastEol]) + [eoTabsToSpaces];
        PythonIIForm.SynEdit.Assign(InterpreterEditorOptions);
        PythonIIForm.RegisterHistoryCommands;
      end;

     //AppStorage.DeleteSubTree('Interpreter Editor Options');

      if AppStorage.PathExists('Editor Search Options') then begin
        AppStorage.ReadPersistent('Editor Search Options', EditorSearchOptions);
        tbiSearchText.Items.CommaText := EditorSearchOptions.SearchTextHistory;
        tbiReplaceText.Items.CommaText := EditorSearchOptions.ReplaceTextHistory;
      end;

      AppStorage.ReadPersistent('Print Options', SynEditPrint);
      SynEditPrint.Header.AsString := AppStorage.ReadString('Print Options\HeaderItems', DefaultHeader);
      SynEditPrint.Footer.AsString := AppStorage.ReadString('Print Options\FooterItems', DefaultFooter);

      AppStorage.StorageOptions.PreserveLeadingTrailingBlanks := True;
      if AppStorage.PathExists('File Templates') then
        AppStorage.ReadObjectList('File Templates', FileTemplates,
          FileTemplates.CreateListItem);

      TempStringList := TStringList.Create;
      try
        AppStorage.ReadStringList('Code Templates', TempStringList);
        CodeTemplatesCompletion.AutoCompleteList.Assign(TempStringList);
      finally
        TempStringList.Free;
      end;
      AppStorage.StorageOptions.PreserveLeadingTrailingBlanks := False;

    end;
  AppStorage.ReadPersistent('Secondary Tabs', TabsPersistsInfo);
  if AppStorage.PathExists('ToDo Options') then
    AppStorage.ReadPersistent('ToDo Options', ToDoExpert);
  AppStorage.ReadPersistent('Find in Files Options', FindResultsWindow.FindInFilesExpert);
  AppStorage.ReadPersistent('Find in Files Results Options', FindResultsWindow);
  AppStorage.ReadPersistent('Variables Window Options', VariablesWindow);
  AppStorage.ReadPersistent('Call Stack Window Options', CallStackWindow);
  AppStorage.ReadPersistent('Breakpoints Window Options', BreakPointsWindow);
  AppStorage.ReadPersistent('Messages Window Options', MessagesWindow);
  AppStorage.ReadPersistent('RegExp Tester Options', RegExpTesterWindow);
  FileExplorerWindow.actEnableFilter.Checked := AppStorage.ReadBoolean('File Explorer Filter', True);
  FileExplorerWindow.ExplorerPath := AppStorage.ReadString('File Explorer Path');
  AppStorage.ReadStringList('File Explorer Favorites', FileExplorerWindow.Favorites);
  AppStorage.ReadPersistent('Code Explorer Options', CodeExplorerWindow);

  AppStorage.ReadStringList('Custom Params', CustomParams);
  RegisterCustomParams;
  AppStorage.ReadCollection('Tools', ToolsCollection, True, 'Tool');
  AppStorage.ReadPersistent('Tools\External Run', ExternalPython);
  SetupToolsMenu;
  SetupCustomizer;
  OutputWindow.lsbConsole.Font.Name := AppStorage.ReadString('Output Window\Font Name', 'Courier New');
  OutputWindow.lsbConsole.Font.Size := AppStorage.ReadInteger('Output Window\Font Size', 9);
  OutputWindow.FontOrColorUpdated;
  AppStorage.ReadPersistent('Watches', WatchesWindow);
  StatusBar.Visible := AppStorage.ReadBoolean('Status Bar');

  if (OldScreenPPI = Screen.PixelsPerInch) and
     ((OldDesktopSize = '') or (OldDesktopSize = DesktopSizeString))
  then
    AppStorage.ReadStringList('Layouts', Layouts, True);

  // Load Style Name
  TStyleSelectorForm.SetStyle(AppStorage.ReadString('Style Name', 'Windows10'));

  // Load Toolbar Items
  LoadToolbarItems('Toolbar Items');

  // Load IDE Shortcuts
  ActionProxyCollection := TActionProxyCollection.Create(apcctEmpty);
  try
    AppStorage.ReadCollection('IDE Shortcuts', ActionProxyCollection, True, 'Action');
    ActionProxyCollection.ApplyShortCuts;
  finally
    ActionProxyCollection.Free;
  end;

  // Restore Interpreter History
  TempStringList := TStringList.Create;
  try
    AppStorage.StorageOptions.PreserveLeadingTrailingBlanks := True;
    AppStorage.ReadStringList('Command History', TempStringList);
    AppStorage.StorageOptions.PreserveLeadingTrailingBlanks := False;
    PythonIIForm.CommandHistory.Clear;

    for I := 0 to TempStringList.Count - 1 do
      PythonIIForm.CommandHistory.Add(StrEscapedToString(TempStringList[i]));
    PythonIIForm.CommandHistoryPointer := TempStringList.Count;  // one after the last one

  finally
    TempStringList.Free;
  end;

  // Load MRU Lists
  tbiRecentFileList.LoadFromIni(AppStorage.IniFile, 'MRU File List');
  tbiRecentProjects.LoadFromIni(AppStorage.IniFile, 'MRU Project List');

  // Project Filename
  if CmdLineReader.readString('PROJECT') = '' then begin
    FName := AppStorage.ReadString('Active Project');
    if FName <> '' then
      ProjectExplorerWindow.DoOpenProjectFile(FName);
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
  EditorForm : TEditorForm;
  Index : integer;
  TabCtrl : TSpTBXTabControl;
begin
  EditorSearchOptions.InitSearch;
  UpdateCaption;
  TabCtrl := Sender as TSpTBXTabControl;
  if Assigned(TabCtrl.ActivePage) and not (csDestroying in ComponentState) then begin
    if TabCtrl.ActivePage.ControlCount > 0 then
    begin
      EditorForm := TabCtrl.ActivePage.Controls[0] as TEditorForm;
      // Code hint stuff
      EditorForm.SetUpCodeHints;
    end;
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
  end else begin
    CodeHint.OnGetCodeHint := nil;
    CodeHint.OnHyperLinkClick := nil;
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
    if AppStorage.PathExists('Layouts\BeforeZoom\Forms') then
      actRestoreEditorExecute(Sender)
    else
      actMaximizeEditorExecute(Sender);
  end;
end;

procedure TPyIDEMainForm.TabControlTabClosing(Sender: TObject; var Allow, CloseAndFree: Boolean);
Var
  Editor : IEditor;
begin
  Editor := EditorFromTab(Sender as TSpTBXTabItem);
  if Assigned(Editor) then begin
    Allow := False;
    PostMessage(Handle, WM_EXECCLOSE, WPARAM(Editor), 0);
    //(Editor as IFileCommands).ExecClose;
  end;
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

  actWatchesWin.Checked := WatchesWindow.Visible;

  actViewStatusbar.Checked := StatusBar.Visible;
  actViewMainMenu .Checked := MainMenu.Visible;

  actFileNewModule.Enabled := GI_EditorFactory <> nil;
  actFileOpen.Enabled := GI_EditorFactory <> nil;
  actFileCloseAll.Enabled := (GI_EditorFactory <> nil)
    and (GI_EditorFactory.GetEditorCount > 0);

  actCommandLine.Checked := PyIDEOptions.UseCommandLine and
    (PyIDEOptions.CommandLine <> '');

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
Var
  i : integer;
begin
  if CompareText(GetCurrentLanguage, LangCode) <> 0 then begin
     UseLanguage(LangCode);

    RetranslateComponent(Self);
    RetranslateComponent(CommandsDataModule);

    SetupLanguageMenu;
    GI_EditorFactory.SetupEditorViewMenu;

    for i := 0 to GI_EditorFactory.Count - 1 do
      GI_EditorFactory.Editor[i].Retranslate;
  end;
end;

procedure TPyIDEMainForm.ScaleForCurrentDpi;
Var
  M, D: Integer;
begin
  inherited;
  M := Screen.PixelsPerInch;
  D := 96;
  if M <> D then begin
    ScaleImageList(CommandsDataModule.Images, M, D);
    ScaleImageList(CommandsDataModule.CodeImages, M, D);
    // Status bar
    StatusBar.Toolbar.Items.ViewBeginUpdate;
    try
      lbStatusCaret.CustomWidth := PPIScaled(lbStatusCaret.CustomWidth);
      lbStatusModified.CustomWidth := PPIScaled(lbStatusModified.CustomWidth);
      lbStatusOverwrite.CustomWidth := PPIScaled(lbStatusOverwrite.CustomWidth);
      lbStatusCaps.CustomWidth := PPIScaled(lbStatusCaps.CustomWidth);
    finally
      StatusBar.ToolBar.Items.ViewEndUpdate;
    end;
    // Completion
    CommandsDataModule.ParameterCompletion.ChangeScale(M, D);
    CommandsDataModule.ModifierCompletion.ChangeScale(M, D);
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
    AppStorage.WriteStringList('Layouts\' + Layout + '\Toolbars', ToolbarLayout);
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
  if AppStorage.PathExists(Path + '\Toolbars') then
  begin
    ToolbarLayout := TStringList.Create;
    try
      AppStorage.ReadStringList(Path + '\Toolbars', ToolbarLayout);
      SpTBXCustomizer.LoadLayout(ToolbarLayout, Layout);
    finally
      ToolbarLayout.Free;
    end;
  end;
end;

procedure TPyIDEMainForm.CloseTimerTimer(Sender: TObject);
begin
  PostMessage(Application.Handle, WM_CLOSE, 0, 0);
  CloseTimer.Enabled := False;
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
  // Delete Images added in previous calls
  for i := CommandsDataModule.Images.Count - 1 downto CommandsDataModule.NumberOfOriginalImages do
    CommandsDataModule.Images.Delete(i);
  for i := 0 to ToolsCollection.Count - 1 do begin
    Tool := (ToolsCollection.Items[i] as TToolItem).ExternalTool;
    if Tool.Caption <> '' then begin
      MenuItem := TSpTBXItem.Create(Self);
      Action := TExternalToolAction.CreateExtToolAction(Self, Tool);
      Action.ActionList := actlStandard;
      mnTools.Add(MenuItem);
      MenuItem.Action := Action;
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
    NewTab.OnTabClosing := TabControlTabClosing;
    NewTab.OnDrawTabCloseButton := DrawCloseButton;
    EditorForm.Visible := True;
  end;

  Tab.Free;
  NewTab.Click;
end;

procedure TPyIDEMainForm.SyntaxClick(Sender: TObject);
Var
  Editor : IEditor;
  Syntax : string;
begin
  // Change Syntax sheme
  Editor := GetActiveEditor;
  if Assigned(Editor) then begin
    Syntax := (Sender as TTBCustomItem).Caption;
    Editor.SynEdit.Highlighter :=
      GetHighlighterFromLanguageName(Syntax,CommandsDataModule.Highlighters);
    Editor.SynEdit2.Highlighter := Editor.SynEdit.Highlighter;
    TEditorForm(Editor.Form).DefaultExtension := '';
  end;
end;

procedure TPyIDEMainForm.mnNoSyntaxClick(Sender: TObject);
Var
  Editor : IEditor;
begin
  Editor := GetActiveEditor;
  if Assigned(Editor) then begin
    Editor.SynEdit.Highlighter := nil;
    Editor.SynEdit2.Highlighter := nil;
  end;
end;

procedure TPyIDEMainForm.SetupSyntaxMenu;
Var
  i : integer;
  MenuItem : TSpTBXItem;
begin
  for i := CommandsDataModule.Highlighters.Count - 1 downto 0 do begin
    MenuItem := TSpTBXItem.Create(Self);
    mnSyntax.Insert(0, MenuItem);
    MenuItem.Caption := CommandsDataModule.Highlighters[i];
    MenuItem.GroupIndex := 3;
    MenuItem.OnClick := SyntaxClick;
    MenuItem.Hint := Format(_(SUseSyntax), [MenuItem.Caption]);
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

procedure TPyIDEMainForm.RunInitScript;
// Execute pyscripter_init.py
Var
  FileName : String;
begin
  // Search first in the Exe directory and then in the user directory
  // Needs to be done after PyScripter loading is finished

  FileName := CommandsDataModule.UserDataPath + PyScripterInitFile;

  try
    InternalInterpreter.RunScript(FileName);
  except
    on E: Exception do
      Dialogs.MessageDlg(Format(_(SErrorInitScript),
        [PyScripterInitFile, E.Message]), mtError, [mbOK], 0);
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
          Item := TSpTBXItem.Create(Self);
          Item.Action := Action;
          Item.Name := 'tb' + Action.Name;
          SpTBXCustomizer.Items.Add(Item);
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
  i : integer;
  SaveActiveControl : TWinControl;
  TempCursor : IInterface;
begin
  Path := 'Layouts\'+ Layout;
  if AppStorage.PathExists(Path + '\Forms') then begin
    TempCursor := WaitCursor;
    SaveActiveControl := ActiveControl;

    try
      // Now Load the DockTree
      LoadDockTreeFromAppStorage(AppStorage, Path);
    finally
      for i := 0 to Screen.FormCount - 1 do begin
        if Screen.Forms[i] is TIDEDockWindow then
          TIDEDockWindow(Screen.Forms[i]).FormDeactivate(Self);
      end;
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
  Appstorage.DeleteSubTree('Layouts\'+Layout);
  SaveDockTreeToAppStorage(AppStorage, 'Layouts\'+ Layout);
  SaveToolbarLayout(Layout);
end;

procedure TPyIDEMainForm.LayoutClick(Sender: TObject);
begin
  LoadLayout(TSpTBXItem(Sender).Caption);
  TSpTBXItem(Sender).Checked := True;
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
  TempCursor : IInterface;
  i : integer;
begin
  TempCursor := nil;
  with TPickListDialog.Create(Self) do begin
    Caption := _(SDeleteLayouts);
    lbMessage.Caption := _(SSelectLayouts);
    CheckListBox.Items.Assign(Layouts);
    if ShowModal = IdOK then begin
      for i := CheckListBox.Count - 1 downto 0 do begin
        if CheckListBox.Checked[i] then begin
          LayoutName := Layouts[i];
          if not assigned(TempCursor) then TempCursor := WaitCursor;

          Appstorage.DeleteSubTree('Layouts\'+LayoutName);

          if Layouts.IndexOf(LayoutName) >= 0 then begin
            Layouts.Delete(Layouts.IndexOf(LayoutName));
            SetupLayoutsMenu;
          end;
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

procedure TPyIDEMainForm.WriteStatusMsg(S: string);
begin
  lbStatusMessage.Caption := S;
end;

procedure TPyIDEMainForm.ThemeEditorGutter(Gutter : TSynGutter);
Var
  GradColor, TextColor : TColor;
begin
  if SkinManager.GetSkinType in [sknNone, sknWindows] then begin
    Gutter.GradientStartColor := clWindow;
    Gutter.GradientEndColor := clBtnFace;
    Gutter.Font.Color := clSilver;
    Exit;
  end;

  if SkinManager.GetSkinType = sknSkin then begin
    GradColor := CurrentSkin.Options(skncToolbar, sknsNormal).Body.Color1;
    if GradColor = clNone then
      GradColor := CurrentSkin.Options(skncDock, sknsNormal).Body.Color1;
    Gutter.Font.Color := CurrentSkin.GetTextColor(skncTab, sknsNormal)
  end else begin  // Delphi Skins
    if not StyleServices.GetElementColor(StyleServices.GetElementDetails(ttTabItemNormal),
      ecFillColor, GradColor) or (GradColor = clNone)
    then
      GradColor := StyleServices.GetSystemColor(clBtnFace);

//    if not StyleServices.GetElementColor(StyleServices.GetElementDetails(ttTabItemNormal),
//      ecTextColor, TextColor) or (TextColor = clNone)
//    then
      TextColor := StyleServices.GetSystemColor(clGrayText);
    Gutter.Font.Color := TextColor;
  end;

  with Gutter do begin
    BorderStyle := gbsNone;
    GradientStartColor := LightenColor(GradColor, 40);
    GradientEndColor := DarkenColor(GradColor, 20);
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
    FileName := GI_ActiveEditor.GetFileNameOrTitle;
    CaretXY := GI_ActiveEditor.ActiveSynEdit.CaretXY;
    FindDefinition(GI_ActiveEditor, CaretXY, True, False, True, FilePosInfo);
    AdjustBrowserLists(FileName, CaretXY.Line, CaretXY.Char, FilePosInfo);
  end;
end;

procedure TPyIDEMainForm.FindDefinition(Editor : IEditor; TextCoord: TBufferCoord;
  ShowMessages, Silent, JumpToFirstMatch: Boolean; var FilePosInfo : string);
var
  Defs : Variant;
  Token : string;
  FName, FileName, ErrMsg: string;
  TokenType,
  Start, Line, Col: Integer;
  Attri: TSynHighlighterAttributes;
  TempCursor : IInterface;
  CE: TBaseCodeElement;
  ParsedModule : TParsedModule;
begin
  FilePosInfo := '';
  VarClear(Defs);
  if Assigned(Editor) and Editor.HasPythonFile then with Editor.SynEdit do
  begin
    if GetHighlighterAttriAtRowColEx(TextCoord, Token, TokenType, Start, Attri) then begin
      case TokenType of
        Ord(tkFunctionName), Ord(tkClassName):
          begin
            if not Silent then
              Dialogs.MessageDlg(Format(_(SFindDefinitionWarning), [Token]),
                mtInformation, [mbOK], 0);
            Exit;
          end;
        Ord(tkIdentifier) :
          begin
            TempCursor := WaitCursor;

            FName := Editor.GetFileNameOrTitle;

            if ShowMessages then begin
              MessagesWindow.ClearMessages;
              MessagesWindow.AddMessage(_(SDefinitionsOf) + Token + '"');
            end;

            FileName := '';
            Line := 0;
            Col := 1;

            CE := PyScripterRefactor.FindDefinitionByCoordinates(FName,
              CaretY, CaretX, ErrMsg);
            if Assigned(CE) and not CE.IsProxy then begin
              ParsedModule := CE.GetModule;
              FileName := ParsedModule.FileName;
              Line := CE.CodePos.LineNo;
              Col := CE.CodePos.CharOffset;
              if ShowMessages then
                MessagesWindow.AddMessage(_(SDefinitionFound), FileName, Line, Col);
            end;

            if ShowMessages then
              ShowDockForm(MessagesWindow);
            if FileName  <> '' then begin
              FilePosInfo := Format(FilePosInfoFormat, [Filename, Line, Col]);
              if JumpToFirstMatch then
                ShowFilePosition(Filename, Line, Col);
            end else begin
              if ShowMessages then
                MessagesWindow.AddMessage(_(SDefinitionNotFound));
              MessageBeep(MB_ICONASTERISK);
            end;
          end;
      else if not Silent then
        Dialogs.MessageDlg(_(SPlaceCursorOnName),
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
  FName, FileName, ErrMsg : string;
  TokenType,
  Start, Line, Col, i : Integer;
  Attri: TSynHighlighterAttributes;
  TempCursor : IInterface;
  FoundReferences : Boolean;
  ResultsList : TStringList;
  RegExpr : TRegExpr;
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

            MessagesWindow.ClearMessages;
            MessagesWindow.AddMessage(_(SReferencesOf) + Token + '"');

            ResultsList := TStringList.Create;
            try
              PyScripterRefactor.FindReferencesByCoordinates(FName,
                CaretY, CaretX, ErrMsg, ResultsList);
              FoundReferences := ResultsList.Count > 0;
              RegExpr := TRegExpr.Create;
              RegExpr.Expression := FilePosInfoRegExpr;
              try
                i := 0;
                while i  < ResultsList.Count -1 do begin
                  if RegExpr.Exec(ResultsList[i]) then begin
                    FileName := RegExpr.Match[1];
                    Line := StrToInt(RegExpr.Match[2]);
                    Col := StrToInt(RegExpr.Match[3]);
                    MessagesWindow.AddMessage(ResultsList[i+1],
                      Filename, Line, Col, Length(Token));
                  end;
                  Inc(i, 2);
                end;
              finally
                RegExpr.Free;
              end;
            finally
              ResultsList.Free;
            end;

            ShowDockForm(MessagesWindow);
            if not FoundReferences then begin
              MessagesWindow.AddMessage(_(SReferencesNotFound));
              MessageBeep(MB_ICONASTERISK);
            end;
          end;
      else
        Dialogs.MessageDlg(_(SPlaceCursorOnName), mtError, [mbOK], 0);
      end;
    end;
  end;
end;

procedure TPyIDEMainForm.WMSpSkinChange(var Message: TMessage);
begin
  // Update EditorOptions
  ThemeEditorGutter(EditorOptions.Gutter);
//  BGPanel.Color := CurrentTheme.GetItemColor(GetItemInfo('inactive'));
//  Application.HintColor := CurrentTheme.GetViewColor(VT_DOCKPANEL);
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
    FileName := GI_ActiveEditor.GetFileNameOrTitle;
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

function TPyIDEMainForm.JumpToFilePosInfo(FilePosInfo: string): boolean;
Var
  FileName : string;
  Line, Col : integer;
  RegExpr : TRegExpr;
begin
  Result := False;
  RegExpr := TRegExpr.Create;
  try
    RegExpr.Expression := FilePosInfoRegExpr;
    if RegExpr.Exec(FilePosInfo) then begin
      FileName := RegExpr.Match[1];
      Line := StrToInt(RegExpr.Match[2]);
      Col := StrToInt(RegExpr.Match[3]);

      Result := ShowFilePosition(FileName, Line, Col);
    end;
  finally
    RegExpr.Free;
  end;
end;

procedure TPyIDEMainForm.DrawCloseButton(Sender: TObject; ACanvas: TCanvas;
  State: TSpTBXSkinStatesType; const PaintStage: TSpTBXPaintStage;
  var AImageList: TCustomImageList; var AImageIndex: Integer; var ARect: TRect;
  var PaintDefault: Boolean);
Const
  ModClosePattern: array [0..15] of Byte    = ($C6, 0, $EE, 0, $6C, 0, 0, 0, $6C, 0, $EE, 0, $C6, 0, 0, 0);
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
    SpDrawXPButton(ACanvas, R, True, False, True, False, False, False);
  end;
  PatternColor := CurrentSkin.GetTextColor(skncToolbarItem, State);
  if Editor.Modified then
    SpDrawGlyphPattern(ACanvas.Handle, ARect, PPIScaled(8), PPIScaled(8), ModClosePattern, PatternColor)
  else
    SpDrawGlyphPattern(ACanvas, ARect, 0, PatternColor, PPIScaled(8), PPIScaled(8));
end;

procedure TPyIDEMainForm.PrevClickHandler(Sender: TObject);
var
  A: TSpTBXMRUItem;
begin
  if Sender is TSpTBXMRUItem then begin
    A := TSpTBXMRUItem(Sender);
    if Assigned(mnPreviousList.OnClick) then mnPreviousList.OnClick(mnPreviousList, A.MRUString);
  end;
end;

procedure TPyIDEMainForm.PreviousListClick(Sender: TObject; S : WideString);
Var
  i, Index : integer;
begin
  Index := mnPreviousList.IndexOfMRU(S);
  if (Index >= 0) and (Index < mnPreviousList.Count) then begin
    JumpToFilePosInfo(S);
    NextMRUAdd(fCurrentBrowseInfo);
    fCurrentBrowseInfo := S;
    for i := 0 to Index - 1 do
      NextMRUAdd(TSpTBXMRUItem(mnPreviousList.Items[i]).MRUString);
    for i := 0 to Index do
      mnPreviousList.MRURemove(TSpTBXMRUItem(mnPreviousList.Items[0]).MRUString);
  end;
end;

procedure TPyIDEMainForm.PrevMRUAdd(S: WideString);
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
begin
  // create a new editor, add it to the editor list
  TabCtrl := TabControl(TabControlIndex);
  TabCtrl.Toolbar.BeginUpdate;
  try
    Result := DoCreateEditor(TabCtrl);
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
        j := CharPos(Result.SynEdit.Lines[i], '|');
        if j > 0 then begin
          Result.SynEdit.CaretXY := BufferCoord(j + 1, i + 1);
          Result.SynEdit.ExecuteCommand(ecDeleteLastChar, ' ', nil);
          break;
        end;
      end;

      Result.SynEdit.ClearUndo;
      Result.SynEdit.Modified := False;

      TEditorForm(Result.Form).DefaultExtension := FileTemplate.Extension;
    end;
  finally
    TabCtrl.Toolbar.EndUpdate;
    if Assigned(TabCtrl.ActiveTab) then
      TabCtrl.MakeVisible(TabCtrl.ActiveTab);
    UpdateCaption;
  end;
end;

procedure TPyIDEMainForm.NextClickHandler(Sender: TObject);
var
  A: TSpTBXMRUItem;
begin
  if Sender is TSpTBXMRUItem then begin
    A := TSpTBXMRUItem(Sender);
    if Assigned(mnNextList.OnClick) then mnNextList.OnClick(mnNextList, A.MRUString);
  end;
end;

procedure TPyIDEMainForm.NextListClick(Sender: TObject; S : WideString);
Var
  i, Index : integer;
begin
  Index := mnNextList.IndexOfMRU(S);
  if (Index >= 0) and (Index < mnNextList.Count) then begin
    JumpToFilePosInfo(S);
    PrevMRUAdd(fCurrentBrowseInfo);
    fCurrentBrowseInfo := S;
    for i := 0 to Index - 1 do
      PrevMRUAdd(TSpTBXMRUItem(mnNextList.Items[i]).MRUString);
    for i := 0 to Index do
      mnNextList.MRURemove(TSpTBXMRUItem(mnNextList.Items[0]).MRUString);
  end;
end;

procedure TPyIDEMainForm.NextMRUAdd(S: WideString);
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
       (Action is TEditPaste) and ClipboardProvidesWideText;
      Handled := (Action is TEditCut) or (Action is TEditCopy) or (Action is TEditPaste);
    end
    else if ((Action is TEditCopy) or (Action is TEditCut)) and Assigned(GI_ActiveEditor) then
    begin
      TEditAction(Action).Enabled := True;
      Handled := True;
    end;
  end;
end;

function TPyIDEMainForm.ApplicationHelp(Command: Word; Data: {$IF CompilerVersion >= 23}NativeInt{$ELSE}integer{$IFEND};
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
    (TSynEdit(ActiveControl).Highlighter = CommandsDataModule.SynPythonSyn) then
  begin
    Keyword := TSynEdit(ActiveControl).WordAtCursor;
    if Keyword <> '' then begin
      CallHelp := not CommandsDataModule.ShowPythonKeywordHelp(KeyWord);
      Result := True;
    end;
  end;
end;

procedure TPyIDEMainForm.WMCheckForUpdates(var Msg: TMessage);
begin
  try
    CommandsDataModule.actCheckForUpdatesExecute(nil);  // nil so that we get no confirmation
  except
    // fail silently
  end;
end;

procedure TPyIDEMainForm.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TPyIDEMainForm.WMExecCLose(var Msg: TMessage);
begin
  (IEditor(Pointer(Msg.WParam)) as IFileCommands).ExecClose;
end;

procedure TPyIDEMainForm.FormShow(Sender: TObject);
begin
  if PyIDEOptions.AutoCheckForUpdates and
    (DaysBetween(Now, PyIDEOptions.DateLastCheckedForUpdates) >=
      PyIDEOptions.DaysBetweenChecks) and ConnectedToInternet
  then
    PostMessage(Handle, WM_CHECKFORUPDATES, 0, 0);

  // Repeat here to make sure it is set right
  MaskFPUExceptions(PyIDEOptions.MaskFPUExceptions);

  if Layouts.IndexOf('Default') < 0 then begin
    SaveLayout('Default');
    Layouts.Add('Default');
  end;

  // Update External Tools Syntax and Layouts menu
  if not FileExists(AppStorage.IniFile.FileName) then begin
    SetupToolsMenu;
    SetupCustomizer;
    // fix for staturbar appearing above interpreter
    StatusBar.Top := MaxInt;
  end;
  SetupLayoutsMenu;
  SetupSyntaxMenu;

  // Activate File Explorer
  FileExplorerWindow.FileExplorerTree.Active := True;
  ConfigureFCN(PyIDEOptions.FileChangeNotification);

  // Register drop target
  RegisterDragDrop(TabControl1.Handle, Self);
  RegisterDragDrop(TabControl2.Handle, Self);

  // This is needed to update the variables window
  PyControl.DoStateChange(dsInactive);
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

procedure TPyIDEMainForm.WMUpdateBreakPoints(var Msg: TMessage);
begin
  BreakPointsWindow.UpdateWindow;
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
  // Save Layout
  SaveLayout('Current');
  // Store other application data and flush AppStorage
  StoreApplicationData;
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
    while Panel.DockClientCount >0 do
      TJvDockVSNETPanel(Panel).DoAutoHideControl(
          Panel.DockClients[Panel.DockClientCount-1] as TWinControl);
  end;
end;

procedure TPyIDEMainForm.actRestoreEditorExecute(Sender: TObject);
begin
  if AppStorage.PathExists('Layouts\BeforeZoom\Forms') then begin
    LoadLayout('BeforeZoom');
    Appstorage.DeleteSubTree('Layouts\BeforeZoom');
  end;
end;

procedure TPyIDEMainForm.actEditorZoomOutExecute(Sender: TObject);
begin
  if ActiveControl is TSynEdit then begin
    TSynEdit(ActiveControl).Font.Size :=
      Max(TSynEdit(ActiveControl).Font.Size - 1, 2);
    TSynEdit(ActiveControl).Gutter.Font.Size :=
      Max(TSynEdit(ActiveControl).Font.Size - 2, 2);
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

procedure TPyIDEMainForm.mnFilesClick(Sender: TObject);
//Fill in the Files submenu of the Tabs popup menu
Var
  Editor, ActiveEditor : IEditor;
  i : integer;
  List : TStringList;
  MenuItem : TSpTBXItem;
begin
  mnFiles.Clear;
  ActiveEditor := GetActiveEditor;
  List := TStringList.Create;
  List.Duplicates := dupAccept;
  List.Sorted := True;
  try
    for i := 0 to GI_EditorFactory.Count - 1 do
      List.AddObject(GI_EditorFactory.Editor[i].GetFileNameOrTitle,
        TObject(GI_EditorFactory.Editor[i].Form));
    for i:= 0 to List.Count - 1 do begin
      Editor := TEditorForm(List.Objects[i]).GetEditor;
      MenuItem := TSpTBXItem.Create(Self);
      mnFiles.Add(MenuItem);
      MenuItem.Caption := List[i];
      MenuItem.GroupIndex := 3;
      MenuItem.Hint := Editor.GetFileNameOrTitle;
      MenuItem.Checked := Editor = ActiveEditor;
      if Editor.Modified then
        MenuItem.ImageIndex := 23;
      MenuItem.OnClick := SelectEditor;
    end;
  finally
    List.Free;
  end;
end;

procedure TPyIDEMainForm.mnLanguageClick(Sender: TObject);
begin
  ChangeLanguage(fLanguageList[(Sender as TSpTBXItem).Tag]);
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

procedure TPyIDEMainForm.tbiSearchTextAcceptText(const NewText: WideString);
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
  const Filename: WideString);
Var
  S : string;
begin
  S := FileName;
  DoOpenFile(S, '', TabControlIndex(ActiveTabControl));
  // A bit problematic since it Frees the MRU Item which calls this click handler
  tbiRecentFileList.MRURemove(S);
end;

procedure TPyIDEMainForm.tbiRecentProjectsClick(Sender: TObject;
  const Filename: WideString);
begin
  ProjectExplorerWindow.DoOpenProjectFile(FileName);
  tbiRecentProjects.MRURemove(Filename);
end;

procedure TPyIDEMainForm.tbiReplaceTextAcceptText(const NewText: WideString);
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


