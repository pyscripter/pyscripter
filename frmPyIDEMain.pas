{-----------------------------------------------------------------------------
 Unit Name: frmPyIDEMain
 Author:    Kiriakos Vlahos
 Date:      11-Feb-2005
 Purpose:   The main form of the Pytnon IDE
            Draws code from the SynEdit demos

            PyScripter was not designed to compete with other Python IDE tools
            but rather to serve the purpose of providing a strong scripting
            solution for Delphi Applications.  However it is a reasonably
            good stand-alone Python IDE.

 Features:  - Easy Integration with Delphi applications
            - Syntax Highlighting
            - Brace Highlighting
            - Python source code utilities ((un)tabify, (un)comment, (un)indent)
            - Code Explorer
            - File Explorer with filter
            - Easy configuration and browsing of the Python Path
            - Access to Python manuals through the Help menu
              and context sensitive help (press F1 on a Python keyword
              inside the editor)
            - Integrated Python Interpreter
              - Command History
                    - Alt-UP : previous command
                    - Alt-Down : next command
                    - Esc : clear command
              - Code Completion
              - Call Tips
            - Integrated Python Debugging
            - Debug Windows
              - Call Stack
              - Variables Window
              - Watches Window
              - BreakPoints Window
            - Editor Views
              - Disassembly
              - HTML Documentation
            - TODO list view
            - Find and Replace in Files
            - Parameterized Code Templates
            - Choice of Python version to run via command line parameters
            - Run Python Script externally (highly configurable)
            - External Tools (External run and caputure output)
            - Modern UI with docked forms and configurable look&feel (themes)
            - Persistent configurable IDE options

Limitations: Python scripts are executed in the main thread
             so it would be unwise to run multi-threaded
             scripts.  This is due to the fact that scripts
             may use wrapped Delphi objects which are not-
             thread safe.

 Credits:   Special thanks to the many great developers who,
            with their amazing work have made PyScripter
            possible.  PyScript makes use of the following
            components:
            - Python for Delphi (www.mmm-experts.com)
            - JVCL (jvcl.sf.net)
            - SynEdit (synedit.sf.net)
            - VirtualTreeView (www.delphi-gems.com)
            - VirtualShellTools (www.mustangpeak.net)
            - EC Software Help Suite (www.ec-software.com)
            - Syn Editor project (syn.sf.net)

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
              - Three file encodings supported (Ansi, UTF-8, UTF-8 without BOM)
            IDE option to detect UTF-8 encoding (useful for non-Python files)
            IDE options for default linebreaks and encoding for new files
            Warning when file encoding results in information loss
            IDE option to position the editor tabs at the top
            IDE window navigation shortcuts
            Pretty print intperpreter output option (on by default)
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
            New command "Highlight search text" (Shft+Ctrl+H)
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

 History:   v 1.9.9.8
          New Features
          Bug fixes
            Issues  236, 304


  Vista Compatibility issues (all resolved)
  -  Flip3D and Form preview (solved with LX)
  -  Dissapearring controls (solved with SpTXPLib)
  -  Dragging forms rectagle
  -  Flicker related to LockWindowsUpdate in loading layouts
  -  Common AVI not available
  -  Fonts
  -  VISTA compatible manifest

  Move to new version of SpTBXLib
  -  Replace TTBXStringList (a couple) (DONE)
  -  Replace TTBXMRULists (DONE)
  -  Theming of JvDocking  (DONE)
  -  Theming of VirtualStringTrees (DONE)
  -  Customizer form (DONE)
  -  Theming of various windows (DONE)
  -  Find Toolbar - Replace TSpTBXComboItem (DONE)
  -  Theming of JvTabbar (Replace with SpTBXTabControl) (DONE)
  -  Painting of ListViewItems (DONE)

-----------------------------------------------------------------------------}

// TODO: Customize Pyscripter with Setup python script run at startup

// Bugs and minor features
// TODO: Internal Tool as in pywin
// TODO: Interpreter raw_input
// TODO: Improve parameter completion with an option to provide more help (docstring)
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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Variants, dmCommands, ActnList, Menus, uEditAppIntfs,
  JvDockControlForm, JvDockVIDStyle, JvDockVSNetStyle,
  SynEditTypes, SynEditMiscClasses, cPyBaseDebugger,
  cPyDebugger, JvAppStorage,  JvAppIniStorage, JvLED, SynEdit,
  TB2Dock, TB2Toolbar, TB2Item, ExtCtrls, JvExControls,
  cRefactoring, dlgCustomShortcuts,
  TB2ExtItems, JvDockTree,
  JvComponentBase, JvAppInst, uHighlighterProcs, cFileTemplates, TntForms, TntLXForms,
  JvDockVSNetStyleSpTBX, TntActnList, JvFormPlacement, SpTBXCustomizer,
  SpTbxSkins, SpTBXItem, SpTBXEditors, StdCtrls, JvDSADialogs, Dialogs,
  TntStdCtrls, ActiveX, SpTBXMDIMRU, SpTBXTabs, ImgList;

const
  WM_FINDDEFINITION  = WM_USER + 100;
  WM_CHECKFORUPDATES = WM_USER + 110;
  WM_UPDATEBREAKPOINTS  = WM_USER + 120;
  WM_SEARCHREPLACEACTION  = WM_USER + 130;

type
  TPyIDEMainForm = class(TTntFormLX, IDropTarget)
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
    SourceCode1: TSpTBXSubmenuItem;
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
    tbiViewThemes: TSpTBXSubmenuItem;
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
    mnThemes: TSpTBXSubmenuItem;
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
    TBXSubmenuItem8: TSpTBXSubmenuItem;
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
    StatusLED: TJvLED;
    TBControlItem3: TTBControlItem;
    ExternalToolsLED: TJvLED;
    TBControlItem1: TTBControlItem;
    SpTBXSeparatorItem10: TSpTBXSeparatorItem;
    mnMainToolbarVisibilityToggle: TSpTBXItem;
    mnDebugtoolbarVisibilityToggle: TSpTBXItem;
    mnEditorToolbarVisibilityToggle: TSpTBXItem;
    mnViewToolbarVisibilityToggle: TSpTBXItem;
    mnuUserToolbarVisibilityToggle: TSpTBXItem;
    mnLanguage: TSpTBXSubmenuItem;
    actlImmutable: TTntActionList;
    actViewPreviousEditor: TTntAction;
    actViewNextEditor: TTntAction;
    actlStandard: TTntActionList;
    actNavProjectExplorer: TTntAction;
    actViewProjectExplorer: TTntAction;
    actViewCustomizeToolbars: TTntAction;
    actPostMortem: TTntAction;
    actViewHideSecondEditor: TTntAction;
    actViewSplitEditorHor: TTntAction;
    actExecSelection: TTntAction;
    actNavEditor: TTntAction;
    actNavOutput: TTntAction;
    actNavUnitTests: TTntAction;
    actNavTodo: TTntAction;
    actNavCodeExplorer: TTntAction;
    actNavFileExplorer: TTntAction;
    actNavMessages: TTntAction;
    actNavCallStack: TTntAction;
    actNavVariables: TTntAction;
    actNavInterpreter: TTntAction;
    actNavBreakpoints: TTntAction;
    actNavWatches: TTntAction;
    actNewFile: TTntAction;
    actPythonRemoteWx: TTntAction;
    actPythonRemoteTk: TTntAction;
    actPythonRemote: TTntAction;
    actPythonInternal: TTntAction;
    actPythonReinitialize: TTntAction;
    actAddWatchAtCursor: TTntAction;
    actViewSplitEditorVer: TTntAction;
    actEditorZoomOut: TTntAction;
    actEditorZoomIn: TTntAction;
    actMaximizeEditor: TTntAction;
    actLayoutDebug: TTntAction;
    actLayoutsDelete: TTntAction;
    actLayoutSave: TTntAction;
    actViewRegExpTester: TTntAction;
    actBrowseForward: TTntAction;
    actBrowseBack: TTntAction;
    actFindReferences: TTntAction;
    actFindDefinition: TTntAction;
    actViewUnitTests: TTntAction;
    actViewOutput: TTntAction;
    actViewFindResults: TTntAction;
    actViewToDoList: TTntAction;
    actViewFileExplorer: TTntAction;
    actViewCodeExplorer: TTntAction;
    actViewII: TTntAction;
    actMessagesWin: TTntAction;
    actWatchesWin: TTntAction;
    actBreakPointsWin: TTntAction;
    actClearAllBreakpoints: TTntAction;
    actToggleBreakPoint: TTntAction;
    actRunLastScript: TTntAction;
    actRunLastScriptExternal: TTntAction;
    actDebugAbort: TTntAction;
    actDebugPause: TTntAction;
    actStepOut: TTntAction;
    actStepOver: TTntAction;
    actStepInto: TTntAction;
    actRunToCursor: TTntAction;
    actRestoreEditor: TTntAction;
    actDebug: TTntAction;
    actRunDebugLastScript: TTntAction;
    actExternalRunConfigure: TTntAction;
    actExternalRun: TTntAction;
    actViewStatusBar: TTntAction;
    actFileExit: TTntAction;
    actFileCloseAll: TTntAction;
    actFileOpen: TTntAction;
    actFileNewModule: TTntAction;
    actImportModule: TTntAction;
    actCommandLine: TTntAction;
    actRun: TTntAction;
    actSyntaxCheck: TTntAction;
    actVariablesWin: TTntAction;
    actCallStackWin: TTntAction;
    actViewMainMenu: TTntAction;
    JvDockVSNetStyleSpTBX: TJvDockVSNetStyleSpTBX;
    tbiRecentFileList: TSpTBXMRUListItem;
    mnPreviousList: TSpTBXMRUListItem;
    mnNextList: TSpTBXMRUListItem;
    mnSkins: TSpTBXSkinGroupItem;
    tbiSearchText: TSpTBXComboBox;
    TBControlItem2: TTBControlItem;
    tbiReplaceText: TSpTBXComboBox;
    TBControlItem4: TTBControlItem;
    TabControl: TSpTBXTabControl;
    tbiRightAlign: TSpTBXRightAlignSpacerItem;
    tbiScrollLeft: TSpTBXItem;
    tbiTabClose: TSpTBXItem;
    tbiScrollRight: TSpTBXItem;
    tbiTabFiles: TSpTBXSubmenuItem;
    tbiTabSep: TSpTBXSeparatorItem;
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
    function ApplicationHelp(Command: Word; Data: Integer;
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
    procedure mnPythonEnginesPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure actExecSelectionExecute(Sender: TObject);
    procedure actRestoreEditorExecute(Sender: TObject);
    procedure actViewMainMenuExecute(Sender: TObject);
    procedure TntFormLXKeyUp(Sender: TObject; var Key: Word;
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
    procedure mnSkinsSkinChange(Sender: TObject);
    procedure tbiSearchTextExit(Sender: TObject);
    procedure tbiReplaceTextKeyPress(Sender: TObject; var Key: Char);
    procedure TabControlActiveTabChange(Sender: TObject; TabIndex: Integer);
    procedure tbiTabFilesClick(Sender: TObject);
    procedure tbiScrollLeftClick(Sender: TObject);
    procedure tbiScrollRightClick(Sender: TObject);
  private
    DSAAppStorage: TDSAAppStorage;
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
  protected
    fCurrentLine : integer;
    fErrorLine : integer;
    fCurrentBrowseInfo : string;
    function DoCreateEditor: IEditor;
    function CmdLineOpenFiles(): boolean;
    procedure DebuggerBreakpointChange(Sender: TObject; Editor : IEditor; ALine: integer);
    procedure DebuggerCurrentPosChange(Sender: TObject);
    procedure UpdateStandardActions;
    procedure UpdateStatusBarPanels;
    procedure ApplicationOnHint(Sender: TObject);
    procedure ApplcationOnShowHint(var HintStr: string; var CanShow: Boolean;
      var HintInfo: THintInfo);
    procedure WMFindDefinition(var Msg: TMessage); message WM_FINDDEFINITION;
    procedure WMUpdateBreakPoints(var Msg: TMessage); message WM_UPDATEBREAKPOINTS;
    procedure WMSearchReplaceAction(var Msg: TMessage); message WM_SEARCHREPLACEACTION;
    procedure WMCheckForUpdates(var Msg: TMessage); message WM_CHECKFORUPDATES;
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
    procedure SyntaxClick(Sender : TObject);
    procedure SelectEditor(Sender : TObject);
    procedure mnLanguageClick(Sender: TObject);
    // Browse MRU stuff
    procedure PrevClickHandler(Sender: TObject);
    procedure NextClickHandler(Sender: TObject);
    procedure PrevMRUAdd(S : WideString);
    procedure NextMRUAdd(S : WideString);
  protected
    // IDropTarget implementation
    function DragEnter(const dataObj: IDataObject; grfKeyState: Longint;
      pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function DropTargetDragOver(grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;
    function IDropTarget.DragOver= DropTargetDragOver;
    function DragLeave: HResult; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;
  public
    PythonKeywordHelpRequested : Boolean;
    MenuHelpRequested : Boolean;
    ActionListArray : TActionListArray;
    Layouts : TStringList;
    fLanguageList : TStringList;
    zOrder : TList;
    zOrderPos : integer;
    zOrderProcessing : Boolean;
    procedure SaveEnvironment;
    procedure StoreApplicationData;
    procedure RestoreApplicationData;
    function DoOpenFile(AFileName: WideString; HighlighterName : string = '') : IEditor;
    function NewFileFromTemplate(FileTemplate : TFileTemplate) : IEditor;
    function GetActiveEditor : IEditor;
    procedure SaveFileModules;
    procedure UpdateDebugCommands(DebuggerState : TDebuggerState);
    procedure SetRunLastScriptHints(ScriptName : WideString);
    function ShowFilePosition(FileName : WideString; Line, Offset : integer;
         ForceToMiddle : boolean = True) : boolean;
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
    procedure WriteStatusMsg(S : WideString);
    function JumpToFilePosInfo(FilePosInfo : string) : boolean;
    procedure FindDefinition(Editor : IEditor; TextCoord : TBufferCoord;
      ShowMessages, Silent, JumpToFirstMatch : Boolean; var FilePosInfo : string);
    procedure AdjustBrowserLists(FileName: string; Line: Integer; Col: Integer;
      FilePosInfo: string);
    procedure ThemeEditorGutter(Gutter : TSynGutter);
    procedure UpdateCaption;
    procedure ChangeLanguage(LangCode : string);
    function EditorFromTab(Tab : TSpTBXTabItem) : IEditor;
    procedure LoadAdditionalThemes;
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
  frmCommandOutput, JvCreateProcessW, dlgToolProperties, uCommonFunctions,
  SynHighlighterPython, SynEditHighlighter, SynRegExpr,
  JvJVCLUtils, DateUtils, cPythonSourceScanner, frmRegExpTester,
  StringResources, dlgCommandLine, frmUnitTests, cFilePersist, frmIDEDockWin,
  dlgPickList, VirtualTrees, VirtualExplorerTree, Math,
  cCodeHint, dlgNewFile, SynEditTextBuffer, JclSysInfo, cPyRemoteDebugger,
  uCmdLine, uSearchHighlighter, frmModSpTBXCustomize, IniFiles,
  JclStrings, JclSysUtils, frmProjectExplorer, cProjectClasses, TntSysUtils,
  MPDataObject, gnugettext, TntDialogs, WideStrUtils, WideStrings,
  SpTBXDefaultSkins, SpTBXControls, VirtualFileSearch;

{$R *.DFM}

{ TWorkbookMainForm }

function TPyIDEMainForm.DoCreateEditor: IEditor;
begin
  if GI_EditorFactory <> nil then begin
    Result := GI_EditorFactory.CreateTabSheet(TabControl);
    Result.SynEdit.Assign(CommandsDataModule.EditorOptions);
    Result.SynEdit2.Assign(CommandsDataModule.EditorOptions);
    TEditorForm(Result.Form).ParentTabItem.OnTabClosing := TabControlTabClosing;
    TEditorForm(Result.Form).ParentTabItem.OnDrawTabCloseButton := DrawCloseButton;
  end else
    Result := nil;
end;

function TPyIDEMainForm.DoOpenFile(AFileName: WideString; HighlighterName : string = '') : IEditor;
begin
  Result := nil;
  AFileName := GetLongFileName(WideExpandFileName(AFileName));
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
  TabControl.Toolbar.BeginUpdate;
  try
    Result := DoCreateEditor;
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
    TabControl.Toolbar.EndUpdate;
    if Assigned(TabControl.ActiveTab) then
      TabControl.MakeVisible(TabControl.ActiveTab);
    UpdateCaption;
  end;
end;

function TPyIDEMainForm.DragEnter(const dataObj: IDataObject;
  grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
begin
  dwEffect := DROPEFFECT_COPY;
  Result := S_OK;
end;

function TPyIDEMainForm.DragLeave: HResult;
begin
    Result := S_OK;
end;

function TPyIDEMainForm.DropTargetDragOver(grfKeyState: Integer; pt: TPoint;
  var dwEffect: Integer): HResult;
begin
  dwEffect := DROPEFFECT_COPY;
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
  FileName : WideString;
  i : integer;
begin
  Result := S_OK;
  CommonHDrop := TCommonHDrop.Create;
  try
    if CommonHDrop.LoadFromDataObject(dataObj) then begin
      for i := 0 to CommonHDrop.FileCount - 1 do begin
        FileName := CommonHDrop.FileName(i);
        if WideFileExists(FileName) then  // checks it is not a directory
          try
            DoOpenFile(FileName);
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
  ItemsList: TList;
  I, J, K : Integer;
  C: TComponent;
  ParentItem: TTBCustomItem;
  Action: TBasicAction;
  Item: TTBCustomItem;
  ItemStyle: TTBItemStyle;
  ActionList : TActionList;
begin
  // App Instances
  if not CmdLineReader.readFlag('NEWINSTANCE') then begin
    JvAppInstances.Active := True;
    JvAppInstances.Check;
  end;

  // Trying to reduce flicker!
//  ControlStyle := ControlStyle + [csOpaque];
//  BGPanel.ControlStyle := BGPanel.ControlStyle + [csOpaque];
//  DockServer.LeftDockPanel.ControlStyle := DockServer.LeftDockPanel.ControlStyle + [csOpaque];
//  DockServer.RightDockPanel.ControlStyle := DockServer.LeftDockPanel.ControlStyle + [csOpaque];
//  DockServer.TopDockPanel.ControlStyle := DockServer.LeftDockPanel.ControlStyle + [csOpaque];
//  DockServer.BottomDockPanel.ControlStyle := DockServer.LeftDockPanel.ControlStyle + [csOpaque];
//  StatusBar.ControlStyle := StatusBar.ControlStyle + [csOpaque];

  SetDesktopIconFonts(Self.Font);  // For Vista
  //SetDesktopIconFonts(ToolbarFont);

  SkinManager.AddSkinNotification(Self);

  Layouts := TStringList.Create;
  Layouts.Sorted := True;
  Layouts.Duplicates := dupError;

  zOrder := TList.Create;

  // Application Storage
  OptionsFileName := ChangeFileExt(ExtractFileName(Application.ExeName), '.ini');
  if FileExists(ChangeFileExt(Application.ExeName, '.ini')) then begin
    AppStorage.Location := flExeFile;
    AppStorage.FileName := OptionsFileName;
  end else if FileExists(IncludeTrailingPathDelimiter(GetAppdataFolder) + OptionsFileName) then begin
    AppStorage.Location := flUserFolder;
    AppStorage.FileName := OptionsFileName;
  end else  // default location
    AppStorage.FileName :=
      CommandsDataModule.UserDataDir + OptionsFileName;

  AppStorage.StorageOptions.StoreDefaultValues := False;

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
  TP_GlobalIgnoreClass(TJvFormStorage);
  // And now translate after all the docking forms have been created
  // They will be translated as well
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
  SetLength(ActionListArray, 4);
  ActionListArray[0] := actlStandard;
  ActionListArray[1] := CommandsDataModule.actlMain;
  ActionListArray[2] := PythonIIForm.InterpreterActionList;
  ActionListArray[3] := ProjectExplorerWindow.ProjectActionList;

  // Setup Customizer
  ItemsList := TList.Create;
  try
    for I := 0 to ComponentCount - 1 do begin
      ParentItem := nil;
      C := Components[I];
      if C is TSpTBXToolbar and TSpTBXToolbar(C).Customizable then
         ParentItem := TSpTBXToolbar(C).Items;
      if Assigned(ParentItem) then begin
        for J := 0 to ParentItem.Count - 1 do begin
          Item := ParentItem[j];
          ItemStyle := TTBCustomItemAccess(Item).ItemStyle;
          // Exclude the submenus, separators, labels, groups and edit items
          if (ItemStyle * [tbisSubMenu, tbisSeparator, tbisEmbeddedGroup, tbisClicksTransparent] = []) and
            not (Item is TTBEditItem) then
            ItemsList.Add(Item);
        end;
      end;
    end;
    for I := Low(ActionListArray) to High(ActionListArray) do begin
      ActionList := ActionListArray[I];
      for J := 0 to ActionList.ActionCount - 1 do begin
        Action := ActionList.Actions[J];
        for K := 0 to ItemsList.Count - 1 do
          if TTBCustomItem(ItemsList[K]).Action = Action then begin
            Action := nil;
            break;
          end;
        if Assigned(Action) then begin
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

  // Store Factory Settings
  if not AppStorage.PathExists(FactoryToolbarItems) then
    SaveToolbarItems(FactoryToolbarItems);

  // Load additional skins from skin folders
  LoadAdditionalThemes;

  // Read Settings from PyScripter.ini
  if FileExists(AppStorage.IniFile.FileName) then begin
    RestoreApplicationData;
    JvFormStorage.RestoreFormPlacement;
  end;

  AppStorage.ReadStringList('Layouts', Layouts, True);

  if AppStorage.PathExists('Layouts\Current\Forms') then begin
    LoadLayout('Current');
    AppStorage.ReadPersistent('Variables Window Options', VariablesWindow);
  end else begin
    SkinManager.SetSkin('Office 2003');
    TabHost := ManualTabDock(DockServer.LeftDockPanel, FileExplorerWindow, ProjectExplorerWindow);
    DockServer.LeftDockPanel.Width := 200;
    ManualTabDockAddPage(TabHost, CodeExplorerWindow);
    ShowDockForm(FileExplorerWindow);

    TabHost := ManualTabDock(DockServer.BottomDockPanel, CallStackWindow, VariablesWindow);
    DockServer.BottomDockPanel.Height := 150;

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

  UpdateDebugCommands(PyControl.DebuggerState);
  //  Editor Views Menu
  GI_EditorFactory.SetupEditorViewMenu;

  Update;

  TabControl.Toolbar.BeginUpdate;
  try
    // if there was no file on the command line try restoring open files
    if CommandsDataModule.PyIDEOptions.RestoreOpenFiles then
      TPersistFileInfo.ReadFromAppStorage(AppStorage, 'Open Files');

    // Open Files on the command line
    CmdLineOpenFiles();

    // If we still have no open file then open an empty file
    if GI_EditorFactory.GetEditorCount = 0 then
      DoOpenFile('', 'Python');
  finally
    TabControl.Toolbar.EndUpdate;
    if Assigned(TabControl.ActiveTab) then
      TabControl.MakeVisible(TabControl.ActiveTab);

    if Assigned(GetActiveEditor()) then
      GetActiveEditor.Activate;
    UpdateCaption;
    // Start the Python Code scanning thread
    CodeExplorerWindow.WorkerThread.Resume;
  end;

  TabControl.Toolbar.OnMouseDown := TabControlMouseDown;
  //Set the HelpFile
  Application.HelpFile := ExtractFilePath(Application.ExeName) + 'PyScripter.chm';
  Application.OnHelp := Self.ApplicationHelp;
end;

procedure TPyIDEMainForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if JvGlobalDockIsLoading then begin
    CanClose := False;
    CloseTimer.Enabled := True;
    Exit;
  end else if PyControl.DebuggerState <> dsInactive then begin
    if WideMessageDlg(_(SAbortDebugging), mtWarning, [mbYes, mbNo], 0) = mrYes then
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
    if WideMessageDlg(_(SKillExternalTool), mtConfirmation, [mbYes, mbCancel], 0) = mrYes
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
    FileExplorerWindow.FileExplorerTree.OnAfterShellNotify := nil;

    // Close FileExplorer ChangeNotify Thread
    FileExplorerWindow.FileExplorerTree.TreeOptions.VETMiscOptions :=
      FileExplorerWindow.FileExplorerTree.TreeOptions.VETMiscOptions
          - [toChangeNotifierThread];

    // Disable CodeHint timer
    CodeHint.CancelHint;

    // Shut down help
    Application.OnHelp := nil;
    // QC25183
//    try
//      Application.HelpCommand(HELP_QUIT, 0);
//    except
//    end;

    // Stop DropTarget to make sure tis unregistered
    RevokeDragDrop(TabControl.Handle);


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
        WideMessageDlg(WideFormat(_(SFileSaveError), [AppStorage.FullFileName, E.Message]), mtError, [mbOK], 0);
    end;

    TabControl.Toolbar.BeginUpdate;
    try
      if GI_EditorFactory <> nil then
        GI_EditorFactory.CloseAll;
    finally
      TabControl.Toolbar.EndUpdate;
    end;

    SkinManager.RemoveSkinNotification(Self);
  end;
end;

procedure TPyIDEMainForm.TabContolContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
Var
  IV: TTBItemViewer;
begin
  IV := TabControl.View.ViewerFromPoint(TabControl.Toolbar.ScreenToClient(TabControl.ClientToScreen(MousePos)));
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
begin
  if TabControl.PagesCount <= 1 then Exit;
  TabItem := nil;
  if CommandsDataModule.PyIDEOptions.SmartNextPrevPage then begin
    Repeat
      Inc(zOrderPos);
      if zOrderPos >= zOrder.Count then
        ZOrderPos := 0;
      while zOrderPos < zOrder.Count  do begin
        TabItem := zOrder[zOrderPos];
        if TabControl.Items.IndexOf(TabItem) < 0 then begin
          zOrder.Delete(zOrderPos);
          TabItem := nil;
        end else
          break;
      end;
    Until Assigned(TabItem) or (ZOrder.Count = 0);
    KeyPreview := True;
    zOrderProcessing := True;
  end else begin
    if Assigned(TabControl.ActivePage) then
      TabItem := TabControl.ActivePage.Item.GetNextTab(True, sivtNormal)
    else
      TabItem := TabControl.Pages[0].Item;
  end;

  if not Assigned(TabItem) and (TabControl.PagesCount > 0) then
    TabItem := TabControl.Pages[0].Item;
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
begin
  if TabControl.PagesCount <= 1 then Exit;
  TabItem := nil;
  if CommandsDataModule.PyIDEOptions.SmartNextPrevPage then begin
    Repeat
      Dec(zOrderPos);
      if zOrderPos < 0 then
        zOrderPos := zOrder.Count - 1;
      while zOrderPos < zOrder.Count  do begin
        TabItem := zOrder[zOrderPos];
        if TabControl.Items.IndexOf(TabItem) < 0 then begin
          zOrder.Delete(zOrderPos);
          TabItem := nil;
        end else
          break;
      end;
    Until Assigned(TabItem) or (ZOrder.Count = 0);
    KeyPreview := True;
    zOrderProcessing := True;
  end else begin
    if Assigned(TabControl.ActivePage) then
      TabItem := TabControl.ActivePage.Item.GetNextTab(False, sivtNormal)
    else
      TabItem := TabControl.Pages[TabControl.PagesCount-1].Item;
  end;
  if not Assigned(TabItem) then
    TabItem := TabControl.Pages[TabControl.PagesCount-1].Item;
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
    if WideMessageDlg(_(STerminateInterpreter),
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
    MessagesWindow.AddMessage(WideFormat(_(SSyntaxIsOK), [ActiveEditor.FileTitle]));
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

  MessagesWindow.AddMessage(WideFormat(_(SModuleImportedOK), [ActiveEditor.FileTitle]));
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
    SynParameters.Text := CommandsDataModule.PyIDEOptions.CommandLine;
    cbUseCommandLine.Checked := CommandsDataModule.PyIDEOptions.UseCommandLine;
    if ShowModal = mrOk then begin
      CommandsDataModule.PyIDEOptions.CommandLine := SynParameters.Text;
      CommandsDataModule.PyIDEOptions.UseCommandLine := cbUseCommandLine.Checked;
    end;
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
  Application.ProcessMessages;
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

procedure TPyIDEMainForm.actStepIntoExecute(Sender: TObject);
var
  ActiveEditor : IEditor;
begin
  Application.ProcessMessages;
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
  if TabControl.Toolbar.IsUpdating then Exit;  

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
  RunConfig.Parameters := iff(CommandsDataModule.PyIDEOptions.UseCommandLine, CommandsDataModule.PyIDEOptions.CommandLine, '');
  RunConfig.ExternalRun.Assign(ExternalPython);
  RunConfig.ExternalRun.Parameters := Parameters.ReplaceInText(RunConfig.ExternalRun.Parameters);
end;

procedure TPyIDEMainForm.DebugActiveScript(ActiveEditor: IEditor;
  InitStepIn : Boolean = False; RunToCursorLine : integer = -1);
var
  RunConfig: TRunConfiguration;
begin
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
  PyFileActive := Assigned(Editor) and Editor.HasPythonFile;

  actSyntaxCheck.Enabled := PyFileActive and (DebuggerState = dsInactive);
  actRun.Enabled := PyFileActive and (DebuggerState = dsInactive);
  actExternalRun.Enabled := PyFileActive and (DebuggerState = dsInactive);
  actImportModule.Enabled := PyFileActive and (DebuggerState = dsInactive);
  actDebug.Enabled := PyFileActive and (DebuggerState in [dsInactive, dsPaused]);
  actStepInto.Enabled := PyFileActive and (DebuggerState in [dsInactive, dsPaused]);
  actStepOut.Enabled := DebuggerState = dsPaused;
  actStepOver.Enabled := DebuggerState = dsPaused;
  actDebugAbort.Enabled := DebuggerState in [dsPaused, dsRunning, dsPostMortem];
  actDebugPause.Enabled := DebuggerState = dsRunning;
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

procedure TPyIDEMainForm.SetRunLastScriptHints(ScriptName: WideString);
Var
  S : WideString;
begin
   S := WideExtractFileName(ScriptName);
   if S <> '' then
     S := WideFormat(' - %s ', [S]);
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
  case NewState of
    dsRunning,
    dsRunningNoDebug: begin
                        s := 'Running';
                        if CommandsDataModule.PyIDEOptions.PythonEngineType = peInternal then
                          Screen.Cursor := crHourGlass;
                        StatusLED.ColorOn := clRed;
                      end;
    dsPaused: begin
                s := 'Paused';
                Screen.Cursor := crDefault;
                StatusLED.ColorOn := clYellow;
              end;
    dsInactive: begin
                 s := 'Ready';
                 Screen.Cursor := crDefault;
                 StatusLED.ColorOn := clLime;
               end;
    dsPostMortem : begin
                     s := 'Post mortem';
                     Screen.Cursor := crDefault;
                     StatusLED.ColorOn := clPurple;
                   end;
  end;
  StatusLED.Hint := 'Debugger state: ' +s;
  lbStatusMessage.Caption := ' ' + s;
  StatusBar.Refresh;

  CallStackWindow.UpdateWindow(NewState);  // also updates Variables and Watches
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
//Var
//  i : integer;
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
  Done := True;
end;

procedure TPyIDEMainForm.ApplicationOnHint(Sender: TObject);
begin
  WriteStatusMsg(GetLongHint(Application.Hint));
end;

procedure TPyIDEMainForm.ApplcationOnShowHint(var HintStr: string;
  var CanShow: Boolean; var HintInfo: THintInfo);
begin
  if HintInfo.HintControl is TBaseVirtualTree then
    HintInfo.HideTimeout := 5000;
end;

function TPyIDEMainForm.ShowFilePosition(FileName: WideString; Line,
  Offset: integer; ForceToMiddle : boolean = True): boolean;
Var
  Editor : IEditor;
begin
  Result := False;
  if FileName <> '' then begin
    if (FileName[1] ='<') and (FileName[Length(FileName)] = '>') then
      FileName :=  Copy(FileName, 2, Length(FileName)-2);
    Editor := GI_EditorFactory.GetEditorByNameOrTitle(FileName);
    if not Assigned(Editor) and WideFileExists(FileName) then begin
      try
        DoOpenFile(FileName);
      except
      end;
      Editor := GI_EditorFactory.GetEditorByNameOrTitle(FileName);
    end;
    if Assigned(Editor) then begin
      Result := True;
      Application.ProcessMessages;  // to deal with focus problems
      // sets the focus to the editor
      Editor.Activate;
      if Line <= 0 then
        Line := 1;
      if Offset <= 0 then
        Offset := 1;
      with Editor.SynEdit do begin
        CaretXY := BufferCoord(Offset,Line);
        EnsureCursorPosVisibleEx(ForceToMiddle);
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
begin
  if Assigned(TabControl.ActivePage) and (TabControl.ActivePage.ComponentCount > 0) and
    (TabControl.ActivePage.Components[0] is TEditorForm) then
    Result := TEditorForm(TabControl.ActivePage.Components[0]).GetEditor
  else
    Result := nil;
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
    lbStatusCAPS.Caption := '';

  ExternalToolsLED.Visible := OutputWindow.JvCreateProcess.State <> psReady;
end;

function TPyIDEMainForm.CmdLineOpenFiles(): boolean;
var
  i : integer;
begin
  Result := False;
  for i := Low(CmdLineReader.readNamelessString) to High(CmdLineReader.readNamelessString) do
    if WideFileExists(CmdLineReader.readNamelessString[i]) then
      Result := Result or Assigned(DoOpenFile(CmdLineReader.readNamelessString[i]));

  // Project Filename
  if CmdLineReader.readString('PROJECT') <> '' then
    ProjectExplorerWindow.DoOpenProjectFile(CmdLineReader.readString('PROJECT'));
end;

procedure TPyIDEMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Layouts);
  FreeAndNil(fLanguageList);
  FreeAndNil(zOrder);
  FreeAndNil(DSAAppStorage);
end;

procedure TPyIDEMainForm.actFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TPyIDEMainForm.actFileNewModuleExecute(Sender: TObject);
begin
  DoOpenFile('', 'Python')
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
        DoOpenFile(Files[i]);
    end;
    Options := Options - [ofAllowMultiSelect];
  end;
end;

procedure TPyIDEMainForm.actFileCloseAllExecute(Sender: TObject);
begin
  if GI_EditorFactory <> nil then begin
    if GI_EditorFactory.CanCloseAll then begin
      TabControl.Toolbar.BeginUpdate;
      try
        GI_EditorFactory.CloseAll;
      finally
        TabControl.Toolbar.EndUpdate;
        UpdateCaption;
      end;
    end;
  end;
end;

procedure TPyIDEMainForm.PyIDEOptionsChanged;
var
  Editor : IEditor;
  i : integer;
begin
  FileExplorerWindow.FileExplorerTree.RefreshTree;
  EditorSearchOptions.SearchTextAtCaret :=
    CommandsDataModule.PyIDEOptions.SearchTextAtCaret;
  MaskFPUExceptions(CommandsDataModule.PyIDEOptions.MaskFPUExceptions);
  CommandsDataModule.SynPythonSyn.DefaultFilter :=
    CommandsDataModule.PyIDEOptions.PythonFileFilter;
  CommandsDataModule.SynWebHTMLSyn.DefaultFilter :=
    CommandsDataModule.PyIDEOptions.HTMLFileFilter;
  CommandsDataModule.SynWebXMLSyn.DefaultFilter :=
    CommandsDataModule.PyIDEOptions.XMLFileFilter;
  CommandsDataModule.SynWebCssSyn.DefaultFilter :=
    CommandsDataModule.PyIDEOptions.CSSFileFilter;
  CommandsDataModule.SynCppSyn.DefaultFilter :=
    CommandsDataModule.PyIDEOptions.CPPFileFilter;
  CommandsDataModule.SynYAMLSyn.DefaultFilter :=
    CommandsDataModule.PyIDEOptions.YAMLFileFilter;
  //  Dock animation parameters
  JvDockVSNetStyleSpTBX.SetAnimationInterval(CommandsDataModule.PyIDEOptions.DockAnimationInterval);
  JvDockVSNetStyleSpTBX.SetAnimationMoveWidth(CommandsDataModule.PyIDEOptions.DockAnimationMoveWidth);

  // Set Python engine
  PyControl.PythonEngineType := CommandsDataModule.PyIDEOptions.PythonEngineType;

  // Command History Size
  PythonIIForm.CommandHistorySize := CommandsDataModule.PyIDEOptions.InterpreterHistorySize;

  if CommandsDataModule.PyIDEOptions.ShowTabCloseButton then
    TabControl.TabCloseButton := tcbAll
  else
    TabControl.TabCloseButton := tcbNone;
  if TabControl.TabPosition <> CommandsDataModule.PyIDEOptions.EditorsTabPosition then
    case CommandsDataModule.PyIDEOptions.EditorsTabPosition of
      ttpTop:
        begin
          TabControl.TabPosition := ttpTop;
          for i  := 0 to GI_EditorFactory.Count - 1 do
            TEditorForm(GI_EditorFactory.Editor[i].Form).ViewsTabControl.TabPosition := ttpBottom;
        end;
      ttpBottom:
        begin
          TabControl.TabPosition := ttpBottom;
          for i  := 0 to GI_EditorFactory.Count - 1 do
            TEditorForm(GI_EditorFactory.Editor[i].Form).ViewsTabControl.TabPosition := ttpTop;
        end;
    end;

  Editor := GetActiveEditor;
  if Assigned(Editor) then
    Editor.SynEdit.InvalidateGutter;
end;

procedure TPyIDEMainForm.StoreApplicationData;
Var
  TempStringList : TStringList;
//  ActionProxyCollection : TActionProxyCollection;
  i : integer;
  TempCursor : IInterface;
begin
  TempCursor := WaitCursor;
  TempStringList := TStringList.Create;
  AppStorage.BeginUpdate;
  try
    AppStorage.WriteString('PyScripter Version', ApplicationVersion);
    AppStorage.WriteString('Language', GetCurrentLanguage);
    AppStorage.WritePersistent('IDE Options', CommandsDataModule.PyIDEOptions);
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
      AppStorage.WriteWideString('Print Options\HeaderItems', SynEditPrint.Header.AsString);
      AppStorage.WriteWideString('Print Options\FooterItems', SynEditPrint.Footer.AsString);

      AppStorage.StorageOptions.PreserveLeadingTrailingBlanks := True;
      AppStorage.DeleteSubTree('File Templates');
      AppStorage.WriteObjectList('File Templates', FileTemplates);

      TempStringList.Assign(CodeTemplatesCompletion.AutoCompleteList);
      AppStorage.WriteStringList('Code Templates', TempStringList);
      AppStorage.StorageOptions.PreserveLeadingTrailingBlanks := False;
    end;
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
    AppStorage.WriteWideStringList('Custom Params', CustomParams);
    AppStorage.DeleteSubTree('Tools');
    AppStorage.WriteCollection('Tools', ToolsCollection, 'Tool');
    AppStorage.WritePersistent('Tools\External Run', ExternalPython);
    AppStorage.WritePersistent('Output Window\Font', OutputWindow.lsbConsole.Font);
    AppStorage.WriteInteger('Output Window\Color', Integer(OutputWindow.lsbConsole.Color));
    AppStorage.WritePersistent('Watches', WatchesWindow);
    AppStorage.WriteStringList('Layouts', Layouts);
    AppStorage.WriteBoolean('Status Bar', StatusBar.Visible);
    // Save Theme Name
    AppStorage.WriteString('Theme Name', SkinManager.CurrentSkinName);


    //  No longer needed since save toolbar Items below achieves the same!
//    // Save IDE Shortcuts
//    AppStorage.DeleteSubTree('IDE Shortcuts');
//    ActionProxyCollection := TActionProxyCollection.Create(ActionListArray);
//    try
//      AppStorage.WriteCollection('IDE Shortcuts', ActionProxyCollection, 'Action');
//    finally
//      ActionProxyCollection.Free;
//    end;

    // Save Toolbar Items
    SaveToolbarItems('Toolbar Items');

    // Save Interpreter History
    TempStringList.Clear;
    for I := 0 to PythonIIForm.CommandHistory.Count - 1 do
      TempStringList.Add(StrStringToEscaped(UTF8Encode(PythonIIForm.CommandHistory[i])));
    AppStorage.StorageOptions.PreserveLeadingTrailingBlanks := True;
    AppStorage.WriteStringList('Command History', TempStringList);
    AppStorage.StorageOptions.PreserveLeadingTrailingBlanks := False;

    // Form Placement
    JvFormStorage.SaveFormPlacement;

    // Project Filename
    AppStorage.DeleteSubTree('Active Project');
    AppStorage.WriteWideString('Active Project', ActiveProject.FileName);

  finally
    AppStorage.EndUpdate;
    TempStringList.Free;
  end;

  // Save MRU Lists
  tbiRecentFileList.SaveToIni(AppStorage.IniFile, 'MRU File List');

  AppStorage.Flush;
end;

procedure TPyIDEMainForm.RestoreApplicationData;
Const
  DefaultHeader='$TITLE$\.1\.0\.-13\.Arial\.0\.96\.10\.0\.1\.2';
  DefaultFooter='$PAGENUM$\\.$PAGECOUNT$\.1\.0\.-13\.Arial\.0\.96\.10\.0\.1\.2';
Var
  //ActionProxyCollection : TActionProxyCollection;
  TempStringList : TStringList;
  i : integer;
  FName : WideString;
begin
  // Change language
  ChangeLanguage(AppStorage.ReadString('Language', GetCurrentLanguage));

  if AppStorage.PathExists('IDE Options') then begin
    AppStorage.ReadPersistent('IDE Options', CommandsDataModule.PyIDEOptions);
    PyIDEOptionsChanged;
  end;
  if AppStorage.PathExists('Editor Options') then
    with CommandsDataModule do begin
      EditorOptions.Gutter.Gradient := False;  //default value
      AppStorage.ReadPersistent('Editor Options', EditorOptions);
      for i := 0 to Highlighters.Count - 1 do
        AppStorage.ReadPersistent('Highlighters\'+Highlighters[i],
          TPersistent(Highlighters.Objects[i]));
      CommandsDataModule.ApplyEditorOptions;
      if AppStorage.PathExists('Highlighters\Intepreter') then
        AppStorage.ReadPersistent('Highlighters\Intepreter',
          PythonIIForm.SynEdit.Highlighter);

      if AppStorage.PathExists('Interpreter Editor Options') then begin
        InterpreterEditorOptions.Gutter.Gradient := False;  //default value
        AppStorage.ReadPersistent('Interpreter Editor Options', InterpreterEditorOptions);
        InterpreterEditorOptions.Options := (InterpreterEditorOptions.Options -
          [eoTrimTrailingSpaces]) + [eoTabsToSpaces];
        PythonIIForm.SynEdit.Assign(InterpreterEditorOptions);
        PythonIIForm.RegisterHistoryCommands;
      end;

      if AppStorage.PathExists('Editor Search Options') then begin
        AppStorage.ReadPersistent('Editor Search Options', EditorSearchOptions);
        tbiSearchText.Items.CommaText := EditorSearchOptions.SearchTextHistory;
        tbiReplaceText.Items.CommaText := EditorSearchOptions.ReplaceTextHistory;
      end;

      AppStorage.ReadPersistent('Print Options', SynEditPrint);
      SynEditPrint.Header.AsString := AppStorage.ReadWideString('Print Options\HeaderItems', DefaultHeader);
      SynEditPrint.Footer.AsString := AppStorage.ReadWideString('Print Options\FooterItems', DefaultFooter);

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
  AppStorage.ReadWideStringList('Custom Params', CustomParams);
  RegisterCustomParams;
  AppStorage.ReadCollection('Tools', ToolsCollection, True, 'Tool');
  AppStorage.ReadPersistent('Tools\External Run', ExternalPython);
  SetupToolsMenu;
  AppStorage.ReadPersistent('Output Window\Font', OutputWindow.lsbConsole.Font);
  OutputWindow.lsbConsole.Color := TColor(AppStorage.ReadInteger('Output Window\Color', Integer(clWindow)));
  OutputWindow.FontOrColorUpdated;
  AppStorage.ReadPersistent('Watches', WatchesWindow);
  StatusBar.Visible := AppStorage.ReadBoolean('Status Bar');
  // Load Theme Name
  SkinManager.SetSkin(AppStorage.ReadString('Theme Name', 'Office2003'));

    //  No longer needed since save toolbar Items below achieves the same!
//  // Load IDE Shortcuts
//  ActionProxyCollection := TActionProxyCollection.Create(ActionListArray);
//  try
//    AppStorage.ReadCollection('IDE Shortcuts', ActionProxyCollection, True, 'Action');
//    ActionProxyCollection.ApplyShortCuts(ActionListArray);
//  finally
//    ActionProxyCollection.Free;
//  end;

  // Load Toolbar Items
  LoadToolbarItems('Toolbar Items');

  // Restore Interpreter History
  TempStringList := TStringList.Create;
  try
    AppStorage.StorageOptions.PreserveLeadingTrailingBlanks := True;
    AppStorage.ReadStringList('Command History', TempStringList);
    AppStorage.StorageOptions.PreserveLeadingTrailingBlanks := False;
    PythonIIForm.CommandHistory.Clear;

    for I := 0 to TempStringList.Count - 1 do
      PythonIIForm.CommandHistory.Add(UTF8Decode(StrEscapedToString(TempStringList[i])));
    PythonIIForm.CommandHistoryPointer := TempStringList.Count;  // one after the last one

    // Project Filename
    if CmdLineReader.readString('PROJECT') = '' then begin
      FName := AppStorage.ReadWideString('Active Project');
      if FName <> '' then
        ProjectExplorerWindow.DoOpenProjectFile(FName);
    end;

  finally
    TempStringList.Free;
  end;

  // Load MRU Lists
  tbiRecentFileList.LoadFromIni(AppStorage.IniFile, 'MRU File List');

end;

function TPyIDEMainForm.EditorFromTab(Tab : TSpTBXTabItem) : IEditor;
Var
  Sheet : TSpTBXTabSheet;
begin
  Result := nil;
  if Assigned(Tab) then begin
    Sheet := TabControl.GetPage(Tab);
    if Assigned(Sheet) and (Sheet.ControlCount > 0) then
      Result := (Sheet.Controls[0] as TEditorForm).GetEditor;
  end;
end;

procedure TPyIDEMainForm.TabControlActiveTabChange(Sender: TObject;
  TabIndex: Integer);
Var
//  WinControl : TWinControl;
  EditorForm : TEditorForm;
  Index : integer;
begin
  EditorSearchOptions.InitSearch;
  UpdateCaption;
  if Assigned(TabControl.ActivePage) and not (csDestroying in ComponentState) then begin
    if TabControl.ActivePage.ControlCount > 0 then
    begin
      EditorForm := TabControl.ActivePage.Controls[0] as TEditorForm;
      // Code hint stuff
      EditorForm.SetUpCodeHints;
    end;
    // zOrder
    if not zOrderProcessing then begin
      Index := zOrder.IndexOf(TabControl.ActivePage.Item);
      if Index < 0 then
        zOrder.Insert(0, TabControl.ActivePage.Item)
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
  IV := TabControl.View.ViewerFromPoint(Point(X,Y));
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
    (Editor as IFileCommands).ExecClose;
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
  actWatchesWin.Checked := WatchesWindow.Visible;

  actViewStatusbar.Checked := StatusBar.Visible;
  actViewMainMenu .Checked := MainMenu.Visible;

  actFileNewModule.Enabled := GI_EditorFactory <> nil;
  actFileOpen.Enabled := GI_EditorFactory <> nil;
  actFileCloseAll.Enabled := (GI_EditorFactory <> nil)
    and (GI_EditorFactory.GetEditorCount > 0);

  actCommandLine.Checked := CommandsDataModule.PyIDEOptions.UseCommandLine and
    (CommandsDataModule.PyIDEOptions.CommandLine <> '');

  // Refactoring
  actFindDefinition.Enabled := Assigned(GI_ActiveEditor) and
    GI_ActiveEditor.HasPythonFile;
  actFindReferences.Enabled := actFindDefinition.Enabled;
  actBrowseBack.Enabled := mnPreviousList.Count > 0;
  actBrowseForward.Enabled := mnNextList.Count > 0;

  // Scroll Buttons
  TabControl.ScrollState(L, R);
  tbiScrollLeft.Enabled := L;
  tbiScrollRight.Enabled := R;
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
    MenuItem.Hint := WideFormat(_(SApplyLayout), [Layouts[i]]);
  end;
end;

procedure TPyIDEMainForm.mnSkinsSkinChange(Sender: TObject);
begin
  if (CurrentSkin  is TSpTBXOffice2003Skin) and
    (TSpTBXOffice2003Skin(CurrentSkin).DefaultColorScheme = lusUnknown) then begin
      TSpTBXOffice2003Skin(CurrentSkin).DefaultColorScheme := lusBlue;
    SkinManager.BroadcastSkinNotification;
  end else if (CurrentSkin  is TSpTBXAluminumSkin) and
    (TSpTBXAluminumSkin(CurrentSkin).DefaultColorScheme = lusUnknown) then begin
      TSpTBXAluminumSkin(CurrentSkin).DefaultColorScheme := lusBlue;
    SkinManager.BroadcastSkinNotification;
  end;
  if (CurrentSkin.Options(skncListItem, sknsCheckedAndHotTrack).IsEmpty
     or CurrentSkin.Options(skncListItem, sknsChecked).IsEmpty) and
     not CurrentSkin.Options(skncListItem, sknsHotTrack).IsEmpty
  then with CurrentSkin do begin
    if Options(skncListItem, sknsCheckedAndHotTrack).IsEmpty then begin
      Options(skncListItem, sknsCheckedAndHotTrack).Assign(Options(skncListItem, sknsHotTrack));
      Options(skncListItem, sknsCheckedAndHotTrack).Body.Lighten(-20);
      Options(skncListItem, sknsCheckedAndHotTrack).Borders.Lighten(-20);
    end;
    if Options(skncListItem, sknsChecked).IsEmpty then begin
      Options(skncListItem, sknsChecked).Assign(Options(skncListItem, sknsHotTrack));
    end;
    Options(skncListItem, sknsHotTrack).Body.Lighten(20);
    Options(skncListItem, sknsHotTrack).Borders.Lighten(20);

    SkinManager.BroadcastSkinNotification;
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

procedure TPyIDEMainForm.mnPythonEnginesPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
begin
  case CommandsDataModule.PyIDEOptions.PythonEngineType of
    peInternal :  actPythonInternal.Checked := True;
    peRemote : actPythonRemote.Checked := True;
    peRemoteTk : actPythonRemoteTk.Checked := True;
    peRemoteWx : actPythonRemoteWx.Checked := True;
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
    MenuItem.Hint := WideFormat(_(SUseSyntax), [MenuItem.Caption]);
  end;
end;

procedure TPyIDEMainForm.LoadAdditionalThemes;
Var
  SkinList : TWideStringList;
  FileName : WideString;
begin
  if not WideDirectoryExists(CommandsDataModule.SkinFilesDir) then Exit;

  SkinList := TWideStringList.Create;
  try
    BuildFileList(CommandsDataModule.SkinFilesDir, '*.skn',  SkinList, False,
      [vsaArchive, vsaCompressed, vsaEncrypted, vsaNormal, vsaOffline, vsaReadOnly],
      [vsaDirectory, vsaHidden, vsaSystem, vsaTemporary]);
    for FileName in SkinList do
      SkinManager.SkinsList.AddSkinFromFile(FileName);
    mnSkins.Recreate;
  finally
    SkinList.Free;
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
  LayoutName : WideString;
  TempCursor : IInterface;
begin
  if WideInputQuery(_(SSaveCurrentLayout), _(SLayoutName), LayoutName) then begin
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

procedure TPyIDEMainForm.WriteStatusMsg(S: WideString);
begin
  lbStatusMessage.Caption := S;
end;

procedure TPyIDEMainForm.ThemeEditorGutter(Gutter : TSynGutter);
Var
  GradColor : TColor;
begin
  if SkinManager.IsDefaultSkin then begin
    Gutter.GradientStartColor := clWindow;
    Gutter.GradientEndColor := clBtnFace;
    Exit;
  end;

  GradColor := CurrentSkin.Options(skncToolbar, sknsNormal).Body.Color1;
  if GradColor = clNone then
    GradColor := CurrentSkin.Options(skncDock, sknsNormal).Body.Color1;

  with Gutter do begin
    BorderStyle := gbsNone;
    GradientStartColor := LightenColor(GradColor, 30);
    GradientEndColor := DarkenColor(GradColor, 10);
  end;
end;

procedure TPyIDEMainForm.TntFormLXKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if (Key = VK_Control) and zOrderProcessing then begin
    zOrderProcessing := False;
    KeyPreview := False;
    if (zOrderPos > 0) and (zOrderPos < zOrder.Count) then begin
      zOrder.Move(zOrderPos, 0);
      zOrderPos := 0;
    end;
  end;
end;

procedure TPyIDEMainForm.AdjustBrowserLists(FileName: string; Line: Integer; Col: Integer; FilePosInfo: string);
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
begin
  Application.ProcessMessages;
  if Assigned(GI_ActiveEditor) then
    FindDefinition(GI_ActiveEditor, GI_ActiveEditor.ActiveSynEdit.CaretXY, True,
      False, True, FilePosInfo);
end;

procedure TPyIDEMainForm.FindDefinition(Editor : IEditor; TextCoord: TBufferCoord;
  ShowMessages, Silent, JumpToFirstMatch: Boolean; var FilePosInfo : string);
var
  Defs : Variant;
  Token : WideString;
  FName, FileName, ErrMsg: WideString;
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
              WideMessageDlg(WideFormat(_(SFindDefinitionWarning), [Token]),
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
        WideMessageDlg(_(SPlaceCursorOnName),
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
  Token : WideString;
  FName, FileName, ErrMsg : WideString;
  TokenType,
  Start, Line, Col, i : Integer;
  Attri: TSynHighlighterAttributes;
  TempCursor : IInterface;
  FoundReferences : Boolean;
  ResultsList : WideStrings.TWideStringList;
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

            ResultsList := WideStrings.TWideStringList.Create;
            try
              PyScripterRefactor.FindReferencesByCoordinates(FName,
                CaretY, CaretX, ErrMsg, ResultsList);
              FoundReferences := ResultsList.Count > 0;
              RegExpr := TRegExpr.Create;
              try
                for i := 0 to ResultsList.Count -1 do begin
                  RegExpr.Expression := FilePosInfoRegExpr;
                  if RegExpr.Exec(ResultsList[i]) then begin
                    FileName := RegExpr.Match[1];
                    Line := StrToInt(RegExpr.Match[2]);
                    Col := StrToInt(RegExpr.Match[3]);
                    MessagesWindow.AddMessage(WideFormat(_(StrCertainty), ['100']), Filename, Line, Col);
                  end;
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
        WideMessageDlg(_(SPlaceCursorOnName), mtError, [mbOK], 0);
      end;
    end;
  end;
end;

procedure TPyIDEMainForm.WMSpSkinChange(var Message: TMessage);
begin
  // Update EditorOptions
  ThemeEditorGutter(CommandsDataModule.EditorOptions.Gutter);
//  BGPanel.Color := CurrentTheme.GetItemColor(GetItemInfo('inactive'));
//  Application.HintColor := CurrentTheme.GetViewColor(VT_DOCKPANEL);
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
    SpDrawXPButton(ACanvas, R, True, False, True, False, False, False, SkinManager.GetSkinType);
  end;
  PatternColor := CurrentSkin.GetTextColor(skncToolbarItem, State);
  if Editor.Modified then
    SpDrawGlyphPattern(ACanvas.Handle, ARect, 8, 8, ModClosePattern, PatternColor)
  else
    SpDrawGlyphPattern(ACanvas, ARect, 0, PatternColor);
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
  FileTemplate: TFileTemplate): IEditor;
begin
  // create a new editor, add it to the editor list
  Result := DoCreateEditor;
  if Result <> nil then begin
    try
      Result.OpenFile('', FileTemplate.Highlighter);
      Result.Activate;
    except
      Result.Close;
      raise
    end;
    TSynEditStringList(Result.SynEdit.Lines).InsertText(0,
      Parameters.ReplaceInText(FileTemplate.Template));
    TEditorForm(Result.Form).DefaultExtension := FileTemplate.Extension;
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

procedure TPyIDEMainForm.tbiBrowseNextClick(Sender: TObject);
begin
  if mnNextList.Count > 0 then begin
    NextListClick(Sender, TSpTBXMRUItem(mnNextList.Items[0]).MRUString);
  end;
end;

function TPyIDEMainForm.ApplicationHelp(Command: Word; Data: Integer;
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

procedure TPyIDEMainForm.FormShow(Sender: TObject);
begin
  if CommandsDataModule.PyIDEOptions.AutoCheckForUpdates and
    (DaysBetween(Now, CommandsDataModule.PyIDEOptions.DateLastCheckedForUpdates) >=
      CommandsDataModule.PyIDEOptions.DaysBetweenChecks) and ConnectedToInternet
  then
    PostMessage(Handle, WM_CHECKFORUPDATES, 0, 0);

  // Repeat here to make sure it is set right
  MaskFPUExceptions(CommandsDataModule.PyIDEOptions.MaskFPUExceptions);

  if Layouts.IndexOf('Default') < 0 then begin
    SaveLayout('Default');
    Layouts.Add('Default');
  end;

  // Update External Tools Syntax and Layouts menu
  SetupToolsMenu;
  SetupLayoutsMenu;
  SetupSyntaxMenu;

  // Activate File Explorer
  with FileExplorerWindow.FileExplorerTree do begin
    TreeOptions.VETMiscOptions :=
      TreeOptions.VETMiscOptions + [toChangeNotifierThread];
    Active := True;
    // Connect ChangeNotify
    OnAfterShellNotify := CommandsDataModule.ProcessShellNotify;
    // Register drop target
    RegisterDragDrop(TabControl.Handle, Self);
  end;

  // Execute pyscripter_init.py
  //InternalInterpreter.RunScript(CommandsDataModule.UserDataDir + PyScripterInitFile);

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
    if (CmdLine[i][1] <> '-') and FileExists(CmdLine[i]) then
      DoOpenFile(CmdLine[i]);
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
//    for j := 0 to Length(ActionListArray) do begin
//      if j = Length(ActionListArray) then
//        ActionList := actlImmutable
//      else
//        ActionList := ActionListArray[j];
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
  TabControl.ScrollLeft;
end;

procedure TPyIDEMainForm.tbiScrollRightClick(Sender: TObject);
begin
  TabControl.ScrollRight;
end;

procedure TPyIDEMainForm.tbiSearchOptionsPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
begin
  with EditorSearchOptions do begin
    tbiSearchFromCaret.Checked := SearchFromCaret;
    tbiSearchInSelection.Checked := SearchSelectionOnly;
    tbiWholeWords.Checked := SearchWholeWords;
    tbiRegExp.Checked := UseRegExp;
    tbiCaseSensitive.Checked := SearchCaseSensitive;
    tbiIncrementalSearch.Checked := IncrementalSearch;
  end;
end;

procedure TPyIDEMainForm.tbiSearchTextAcceptText(const NewText: WideString);
Var
  S : WideString;
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
      S := S + WideQuotedStr(tbiSearchText.Items[i], '"');
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
  end else if ((Key = #8) or (key > #32)) and EditorSearchOptions.IncrementalSearch then
    PostMessage(Handle, WM_SEARCHREPLACEACTION, 2, 0);
end;

procedure TPyIDEMainForm.tbiTabFilesClick(Sender: TObject);
begin
  TabControl.ScrollLeft;
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
  end;
end;

procedure TPyIDEMainForm.tbiSearchTextExit(Sender: TObject);
begin
  tbiSearchTextAcceptText(tbiSearchText.Text);
end;

procedure TPyIDEMainForm.tbiRecentFileListClick(Sender: TObject;
  const Filename: WideString);
Var
  S : WideString;
begin
  S := FileName;
  DoOpenFile(S);
  // A bit problematic since it Frees the MRU Item which calls this click handler
  tbiRecentFileList.MRURemove(S);
end;

procedure TPyIDEMainForm.tbiReplaceTextAcceptText(const NewText: WideString);
Var
  S : WideString;
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
      S := S + WideQuotedStr(tbiReplaceText.Items[i], '"');
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
//    PostMessage(Handle, WM_SEARCHREPLACEACTION, 0, Integer(Action));
  end;
end;

procedure TPyIDEMainForm.SearchOptionsChanged(Sender: TObject);
begin
  with EditorSearchOptions do begin
    SearchFromCaret := tbiSearchFromCaret.Checked;
    SearchSelectionOnly := tbiSearchInSelection.Checked;
    SearchWholeWords := tbiWholeWords.Checked;
    UseRegExp := tbiRegExp.Checked;
    SearchCaseSensitive := tbiCaseSensitive.Checked;
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
    DoOpenFile((Sender as TTBCustomItem).Hint);
end;

end.













