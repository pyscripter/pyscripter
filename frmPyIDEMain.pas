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
            File Explorer improvements (Favourites, Create New Folder)
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

 History:   v 1.9.3
          New Features
            Remote interpreter and debugger
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
            New IDE command Restore Editor pair to Maximize Editor (both work by double clicking  the Tabbar)
            New IDE Option "Smart Next Previous Tab" (z-Order) on by default (Issue 20)
            Word Wrap option exposed in Editor Options
            New File Reload command
            Import/Export Settings (Shortcuts, Highlighter schemes)
            New IDE option "Auto-reload changed files" on by default (Issue 25)
            New menu command to show/hide the menu bar.  The shortcut is Shift-F10 (Issue 63)
            New command line option --DPIAWARE (-D) to avoid scaling in VISTA high DPI displays (Issue 77)
            New command line option --NEWINSTANCE (-N) to start a new instance of PyScripter
            You can disable a breakpoint by Ctrl+Clicking in the gutter
            Syntax Errors are indicated by icon in the tabbar (Issue 93)
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
            More robust "Reinitialize" of remote Python engines (Issue 143)
            Shift-Tab does not work well with the Trim Trailing Spaces editor option
            Issues 28, (32), 39, 40, 41, 46, 47, 48, 49, 52, 55, 56, 57, 65, 66, 67, 70,
                   71, 72, 74, 75, 76, 81, 82, 83, 86, 88, 90, 91, 92, 94, 96, 98,
                   100, 102, 105, 106, 107, 109, 113, 117, 119, 120, 
                   122, 123, 125, 132, 134, 135, 136, 137, 138, 139, 140, 141 fixed

  Vista Compatibility issues (all resolved)
  -  Flip3D and Form preview (solved with LX)
  -  Dissapearring controls (solved with SpTXPLib)
  -  Dragging forms rectagle
  -  Flicker related to LockWindowsUpdate in loading layouts
  -  Common AVI not available
  -  Fonts
  -  VISTA compatible manifest

-----------------------------------------------------------------------------}

// TODO: Customize Pyscripter with Setup python script run at startup

// TODO: Project Manager
// Bugs and minor features
// TODO: Internal Tool as in pywin
// TODO: Interpreter raw_input
// TODO: Improve parameter completion with an option to provide more help (docstring)
// TODO: Find module expert

// TODO: UML Editor View
// TODO: Refactorings using BRM

// TODO: Plugin architecture
// TODO Package as an Application Scripter Component

unit frmPyIDEMain;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Variants, dmCommands, ComCtrls, StdActns, ActnList, Menus, uEditAppIntfs,
  JvDockControlForm, JvDockVIDStyle, JvDockVSNetStyle, JvComponent,
  SynEditTypes, SynEditMiscClasses, SynEditRegexSearch, cPyBaseDebugger,
  cPyDebugger, ToolWin,  ExtCtrls, JvExComCtrls, JvAppStorage,
  JvAppIniStorage, JvExControls, JvLED, SynEdit, JvTabBar,
  JvDragDrop, TB2Dock, TB2Toolbar, TBX, TBXSwitcher, TB2Item,
  TBXStatusBars, JvmbTBXTabBarPainter, JvDockVSNETStyleTBX,
  TB2MRU, TBXExtItems,  JvPageList, cRefactoring, dlgCustomShortcuts,
  // Themes
  TBXNexosXTheme, TBXOfficeXPTheme, TBXAluminumTheme, TBXWhidbeyTheme,
  TBXOffice2003Theme, TBXOffice2007Theme, TBXLists, TB2ExtItems, JvDockTree,
  JvComponentBase, JvAppInst, uHighlighterProcs, cFileTemplates, TntLXForms,
  SpTBXItem, SpTBXEditors, StdCtrls, JvDSADialogs, Dialogs, SpTBXCustomizer;

const
  WM_FINDDEFINITION  = WM_USER + 100;
  WM_CHECKFORUPDATES = WM_USER + 110;
  WM_UPDATEBREAKPOINTS  = WM_USER + 120;
  WM_SEARCHREPLACEACTION  = WM_USER + 130;

type
  TPyIDEMainForm = class(TTntFormLX)
    DockServer: TJvDockServer;
    actlStandard : TActionList;
    actRun: TAction;
    actRunToCursor: TAction;
    actStepInto: TAction;
    actStepOver: TAction;
    actStepOut: TAction;
    actToggleBreakPoint: TAction;
    actClearAllBreakpoints: TAction;
    actCallStackWin: TAction;
    actVariablesWin: TAction;
    actBreakPointsWin: TAction;
    actWatchesWin: TAction;
    actDebugAbort: TAction;
    actMessagesWin: TAction;
    actDebug: TAction;
    actViewII: TAction;
    actViewCodeExplorer: TAction;
    AppStorage: TJvAppIniFileStorage;
    actFileNewModule: TAction;
    actFileOpen: TAction;
    actFileCloseAll: TAction;
    actFileExit: TAction;
    actViewStatusBar: TAction;
    actViewFileExplorer: TAction;
    BGPanel: TPanel;
    TabBar: TJvTabBar;
    CloseTimer: TTimer;
    actImportModule: TAction;
    JvDropTarget: TJvDropTarget;
    actViewToDoList: TAction;
    actSyntaxCheck: TAction;
    actViewFindResults: TAction;
    actViewOutput: TAction;
    actExternalRun: TAction;
    actExternalRunConfigure: TAction;
    TBXSwitcher: TTBXSwitcher;
    TBXDockTop: TSpTBXDock;
    MainMenu: TSpTBXToolbar;
    FileMenu: TSpTBXSubmenuItem;
    New1: TSpTBXItem;
    Open1: TSpTBXItem;
    N14: TSpTBXSeparatorItem;
    Close1: TSpTBXItem;
    CloseAll2: TSpTBXItem;
    N1: TSpTBXSeparatorItem;
    Save1: TSpTBXItem;
    SaveAs1: TSpTBXItem;
    SaveAll1: TSpTBXItem;
    N2: TSpTBXSeparatorItem;
    PageSetup1: TSpTBXItem;
    PrinterSetup1: TSpTBXItem;
    PrintPreview1: TSpTBXItem;
    Print1: TSpTBXItem;
    N4: TSpTBXSeparatorItem;
    N3: TSpTBXItem;
    EditMenu: TSpTBXSubmenuItem;
    Undo1: TSpTBXItem;
    Redo1: TSpTBXItem;
    N5: TSpTBXSeparatorItem;
    Cut1: TSpTBXItem;
    Copy1: TSpTBXItem;
    IDEOptions1: TSpTBXItem;
    Delete1: TSpTBXItem;
    SelectAll1: TSpTBXItem;
    N6: TSpTBXSeparatorItem;
    Parameters1: TSpTBXSubmenuItem;
    PageSetup2: TSpTBXItem;
    Insertmodifier1: TSpTBXItem;
    N16: TSpTBXSeparatorItem;
    Replaceparameter1: TSpTBXItem;
    CodeTemplate1: TSpTBXItem;
    SourceCode1: TSpTBXSubmenuItem;
    IndentBlock1: TSpTBXItem;
    DedentBlock1: TSpTBXItem;
    Commentout1: TSpTBXItem;
    abify1: TSpTBXItem;
    Untabify1: TSpTBXItem;
    SearchMenu: TSpTBXSubmenuItem;
    Find1: TSpTBXItem;
    FindNext1: TSpTBXItem;
    FindPrevious1: TSpTBXItem;
    Replace1: TSpTBXItem;
    N15: TSpTBXSeparatorItem;
    FindinFiles1: TSpTBXItem;
    N7: TSpTBXSeparatorItem;
    Replace2: TSpTBXItem;
    FindinFiles2: TSpTBXItem;
    N23: TSpTBXSeparatorItem;
    MatchingBrace1: TSpTBXItem;
    RunMenu: TSpTBXSubmenuItem;
    SyntaxCheck1: TSpTBXItem;
    ImportModule1: TSpTBXItem;
    N21: TSpTBXSeparatorItem;
    Run2: TSpTBXItem;
    N22: TSpTBXSeparatorItem;
    ExternalRun1: TSpTBXItem;
    ConfigureExternalRun1: TSpTBXItem;
    N8: TSpTBXSeparatorItem;
    Debug1: TSpTBXItem;
    RunToCursor1: TSpTBXItem;
    StepInto1: TSpTBXItem;
    StepOver1: TSpTBXItem;
    StepOut1: TSpTBXItem;
    AbortDebugging1: TSpTBXItem;
    N9: TSpTBXSeparatorItem;
    ogglebreakpoint1: TSpTBXItem;
    ClearAllBreakpoints1: TSpTBXItem;
    ToolsMenu: TSpTBXSubmenuItem;
    PythonPath1: TSpTBXItem;
    N13: TSpTBXSeparatorItem;
    ConfigureTools1: TSpTBXItem;
    N20: TSpTBXSeparatorItem;
    Options1: TSpTBXSubmenuItem;
    IDEOptions2: TSpTBXItem;
    EditorOptions1: TSpTBXItem;
    CustomizeParameters1: TSpTBXItem;
    CodeTemplates1: TSpTBXItem;
    ViewMenu: TSpTBXSubmenuItem;
    NextEditor1: TSpTBXItem;
    PreviousEditor1: TSpTBXItem;
    N10: TSpTBXSeparatorItem;
    mnuToolbars: TSpTBXSubmenuItem;
    StatusBar1: TSpTBXItem;
    InteractiveInterpreter1: TSpTBXItem;
    FileExplorer1: TSpTBXItem;
    CodeExplorer1: TSpTBXItem;
    actViewToDoList1: TSpTBXItem;
    FindinFilesResults1: TSpTBXItem;
    actViewOutput1: TSpTBXItem;
    DebugWindows1: TSpTBXSubmenuItem;
    CallStack1: TSpTBXItem;
    Variables1: TSpTBXItem;
    Breakpoints1: TSpTBXItem;
    Watches1: TSpTBXItem;
    Messages1: TSpTBXItem;
    HelpMenu: TSpTBXSubmenuItem;
    PythonPath2: TSpTBXItem;
    N18: TSpTBXSeparatorItem;
    PyScripter1: TSpTBXSubmenuItem;
    CustomParameters1: TSpTBXItem;
    ExternalTools1: TSpTBXItem;
    N17: TSpTBXSeparatorItem;
    About1: TSpTBXItem;
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
    JvmbTBXTabBarPainter: TJvmbTBXTabBarPainter;
    ViewToolbar: TSpTBXToolbar;
    tbiViewThemes: TSpTBXSubmenuItem;
    TBXDockLeft: TSpTBXDock;
    StatusBar: TTBXStatusBar;
    StatusLED: TJvLED;
    ExternalToolsLED: TJvLED;
    TBXDockRight: TSpTBXDock;
    TBXDockBottom: TSpTBXDock;
    JvDockVSNetStyleTBX: TJvDockVSNetStyleTBX;
    mnTools: TSpTBXSubmenuItem;
    TabBarPopupMenu: TSpTBXPopupMenu;
    New2: TSpTBXItem;
    NewModule2: TSpTBXItem;
    CloseAll1: TSpTBXItem;
    N12: TSpTBXSeparatorItem;
    EditorOptions2: TSpTBXItem;
    TBXMRUList: TTBXMRUList;
    TBXMRUListItem: TTBXMRUListItem;
    RecentSubmenu: TSpTBXSubmenuItem;
    EditorViewsMenu: TSpTBXSubmenuItem;
    EditorsPageList: TJvPageList;
    TBXSeparatorItem8: TSpTBXSeparatorItem;
    mnThemes: TSpTBXSubmenuItem;
    ViewToolbarVisibilityToggle: TTBXVisibilityToggleItem;
    EditorToolbar: TSpTBXToolbar;
    tbiEditDedent: TSpTBXItem;
    tbiEditIndent: TSpTBXItem;
    TBXSeparatorItem10: TSpTBXSeparatorItem;
    tbiEditToggleComment: TSpTBXItem;
    TBXSeparatorItem11: TSpTBXSeparatorItem;
    tbiEditSpecialCharacters: TSpTBXItem;
    tbiEditLineNumbers: TSpTBXItem;
    EditorToolbarVisibilityToggle: TTBXVisibilityToggleItem;
    TBXItem25: TSpTBXItem;
    TBXItem26: TSpTBXItem;
    actFindDefinition: TAction;
    TBXItem33: TSpTBXItem;
    TBXSeparatorItem9: TSpTBXSeparatorItem;
    TBXSubmenuItem3: TSpTBXSubmenuItem;
    TBXItem34: TSpTBXItem;
    TBXItem35: TSpTBXItem;
    TBXItem36: TSpTBXItem;
    TBXSeparatorItem12: TSpTBXSeparatorItem;
    TBXItem37: TSpTBXItem;
    TBXSeparatorItem13: TSpTBXSeparatorItem;
    actFindReferences: TAction;
    TBXItem38: TSpTBXItem;
    tbiBrowseNext: TSpTBXSubmenuItem;
    tbiBrowsePrevious: TSpTBXSubmenuItem;
    TBXSeparatorItem14: TSpTBXSeparatorItem;
    PreviousList: TTBXStringList;
    NextList: TTBXStringList;
    actBrowseBack: TAction;
    actBrowseForward: TAction;
    TBXItem39: TSpTBXItem;
    TBXItem40: TSpTBXItem;
    TBXSeparatorItem15: TSpTBXSeparatorItem;
    TBXItem41: TSpTBXItem;
    actViewRegExpTester: TAction;
    TBXItem42: TSpTBXItem;
    actCommandLine: TAction;
    TBXItem43: TSpTBXItem;
    TBXItem44: TSpTBXItem;
    TBXItem45: TSpTBXItem;
    actViewUnitTests: TAction;
    TBXItem46: TSpTBXItem;
    TBXSeparatorItem16: TSpTBXSeparatorItem;
    mnLayouts: TSpTBXSubmenuItem;
    mnLayOutSeparator: TSpTBXSeparatorItem;
    tbiViewLayouts: TSpTBXSubmenuItem;
    actLayoutSave: TAction;
    actLayoutsDelete: TAction;
    actLayoutDebug: TAction;
    TBXItem47: TSpTBXItem;
    TBXItem48: TSpTBXItem;
    TBXItem49: TSpTBXItem;
    mnMaximizeEditor: TSpTBXItem;
    TBXSeparatorItem17: TSpTBXSeparatorItem;
    actMaximizeEditor: TAction;
    TBXSeparatorItem18: TSpTBXSeparatorItem;
    TBXSubmenuItem4: TSpTBXSubmenuItem;
    TBXSeparatorItem19: TSpTBXSeparatorItem;
    mnNoSyntax: TSpTBXItem;
    actEditorZoomIn: TAction;
    actEditorZoomOut: TAction;
    TBXSeparatorItem20: TSpTBXSeparatorItem;
    TBXSeparatorItem21: TSpTBXSeparatorItem;
    mnSyntax: TSpTBXSubmenuItem;
    TBXItem50: TSpTBXItem;
    TBXItem51: TSpTBXItem;
    TBXSeparatorItem22: TSpTBXSeparatorItem;
    mnFiles: TSpTBXSubmenuItem;
    actAddWatchAtCursor: TAction;
    TBXItem52: TSpTBXItem;
    RunningProcessesPopUpMenu: TSpTBXPopupMenu;
    TBXItem29: TSpTBXItem;
    TBXSubmenuItem5: TSpTBXSubmenuItem;
    TBXSeparatorItem23: TSpTBXSeparatorItem;
    actNewFile: TAction;
    TBXItem53: TSpTBXItem;
    JvAppInstances: TJvAppInstances;
    TBXItem54: TSpTBXItem;
    TBXItem55: TSpTBXItem;
    mnuFindInFilesResults: TSpTBXItem;
    actNavWatches: TAction;
    actNavBreakpoints: TAction;
    actNavInterpreter: TAction;
    actNavVariables: TAction;
    actNavCallStack: TAction;
    actNavMessages: TAction;
    actNavFileExplorer: TAction;
    actNavCodeExplorer: TAction;
    actNavTodo: TAction;
    actNavUnitTests: TAction;
    actNavOutput: TAction;
    actNavEditor: TAction;
    TBXSubmenuItem6: TSpTBXSubmenuItem;
    TBXItem57: TSpTBXItem;
    TBXSeparatorItem24: TSpTBXSeparatorItem;
    TBXItem58: TSpTBXItem;
    TBXItem59: TSpTBXItem;
    TBXItem60: TSpTBXItem;
    TBXItem62: TSpTBXItem;
    TBXItem63: TSpTBXItem;
    TBXSeparatorItem25: TSpTBXSeparatorItem;
    TBXItem64: TSpTBXItem;
    TBXItem65: TSpTBXItem;
    TBXItem66: TSpTBXItem;
    TBXItem67: TSpTBXItem;
    TBXItem68: TSpTBXItem;
    TBXItem69: TSpTBXItem;
    actDebugPause: TAction;
    tbiRunPause: TSpTBXItem;
    TBXItem70: TSpTBXItem;
    mnPythonEngines: TSpTBXSubmenuItem;
    actPythonReinitialize: TAction;
    actPythonInternal: TAction;
    actPythonRemote: TAction;
    actPythonRemoteTk: TAction;
    actPythonRemoteWx: TAction;
    TBXItem71: TSpTBXItem;
    TBXItem72: TSpTBXItem;
    TBXItem73: TSpTBXItem;
    TBXItem74: TSpTBXItem;
    TBXItem75: TSpTBXItem;
    TBXSeparatorItem26: TSpTBXSeparatorItem;
    actExecSelection: TAction;
    TBXSeparatorItem27: TSpTBXSeparatorItem;
    TBXItem76: TSpTBXItem;
    actRestoreEditor: TAction;
    TBXItem77: TSpTBXItem;
    TBXSeparatorItem28: TSpTBXSeparatorItem;
    TBXItem78: TSpTBXItem;
    TBXItem79: TSpTBXItem;
    TBXItem80: TSpTBXItem;
    TBXSeparatorItem29: TSpTBXSeparatorItem;
    TBXSubmenuItem7: TSpTBXSubmenuItem;
    TBXItem81: TSpTBXItem;
    TBXItem82: TSpTBXItem;
    TBXSeparatorItem30: TSpTBXSeparatorItem;
    TBXItem83: TSpTBXItem;
    TBXItem84: TSpTBXItem;
    DebugtoolbarVisibilityToggle: TTBXVisibilityToggleItem;
    TBXVisibilityToggleItem1: TTBXVisibilityToggleItem;
    actViewMainMenu: TAction;
    TBXItem85: TSpTBXItem;
    actlImmutable: TActionList;
    actViewNextEditor: TAction;
    actViewPreviousEditor: TAction;
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
    tbiIcrementalSearch: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    tbiHighlight: TSpTBXItem;
    tbiReplaceExecute: TSpTBXItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    tbiSearchFromCaret: TSpTBXItem;
    tbiReplaceText: TSpTBXComboBoxItem;
    tbiSearchText: TSpTBXComboBoxItem;
    TBXSeparatorItem31: TSpTBXSeparatorItem;
    TBXItem86: TSpTBXItem;
    TBXItem87: TSpTBXItem;
    tbiEditWordWrap: TSpTBXItem;
    TBXItem89: TSpTBXItem;
    actViewSplitEditorHor: TAction;
    actViewSplitEditorVer: TAction;
    TBXSubmenuItem8: TSpTBXSubmenuItem;
    TBXItem90: TSpTBXItem;
    TBXItem91: TSpTBXItem;
    actViewHideSecondEditor: TAction;
    TBXItem92: TSpTBXItem;
    actPostMortem: TAction;
    TBXSeparatorItem33: TSpTBXSeparatorItem;
    TBXItem93: TSpTBXItem;
    SpTBXCustomizer: TSpTBXCustomizer;
    actViewCustomizeToolbars: TAction;
    ToolbarPopupMenu: TSpTBXPopupMenu;
    SpTBXSeparatorItem3: TSpTBXSeparatorItem;
    SpTBXItem1: TSpTBXItem;
    UserToolbar: TSpTBXToolbar;
    mnuUserToolbarVisibilityToggle: TTBXVisibilityToggleItem;
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
    procedure TabbarContextPopup(Sender: TObject; MousePos: TPoint;
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
    procedure TabBarTabSelected(Sender: TObject; Item: TJvTabBarItem);
    procedure TabBarTabClosed(Sender: TObject; Item: TJvTabBarItem);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure CloseTimerTimer(Sender: TObject);
    procedure actImportModuleExecute(Sender: TObject);
    procedure JvDropTargetDragDrop(Sender: TJvDropTarget;
      var Effect: TJvDropEffect; Shift: TShiftState; X, Y: Integer);
    procedure actViewToDoListExecute(Sender: TObject);
    procedure actViewFindResultsExecute(Sender: TObject);
    procedure actViewOutputExecute(Sender: TObject);
    procedure actExternalRunExecute(Sender: TObject);
    procedure actExternalRunConfigureExecute(Sender: TObject);
    procedure TBXMRUListClick(Sender: TObject; const Filename: String);
    procedure actFindDefinitionExecute(Sender: TObject);
    procedure actFindReferencesExecute(Sender: TObject);
    procedure PreviousListClick(Sender: TObject);
    procedure tbiBrowsePreviousClick(Sender: TObject);
    procedure NextListClick(Sender: TObject);
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
    procedure TabBarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SearchOptionsChanged(Sender: TObject);
    procedure tbiSearchOptionsPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure tbiSearchTextChange(Sender: TObject; const Text: WideString);
    procedure tbiSearchTextAcceptText(Sender: TObject; var NewText: WideString;
      var Accept: Boolean);
    procedure tbiSearchReplaceTextBeginEdit(Sender: TTBEditItem;
      Viewer: TTBEditItemViewer; EditControl: TEdit);
    procedure tbiSearchTextKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tbiSearchTextKeyPress(Sender: TObject; var Key: Char);
    procedure tbiReplaceTextKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tbiReplaceTextAcceptText(Sender: TObject; var NewText: WideString;
      var Accept: Boolean);
    procedure tbiReplaceTextChange(Sender: TObject; const Text: WideString);
    procedure actViewSplitEditorVerExecute(Sender: TObject);
    procedure actViewSplitEditorHorExecute(Sender: TObject);
    procedure actViewHideSecondEditorExecute(Sender: TObject);
    procedure actPostMortemExecute(Sender: TObject);
    procedure FindToolbarVisibleChanged(Sender: TObject);
    procedure actViewCustomizeToolbarsExecute(Sender: TObject);
    procedure SpTBXCustomizerGetCustomizeForm(Sender: TObject;
      var CustomizeFormClass: TSpTBXCustomizeFormClass);
  private
    DSAAppStorage: TDSAAppStorage;
    function FindAction(var Key: Word; Shift: TShiftState) : TCustomAction;
  protected
    fCurrentLine : integer;
    fErrorLine : integer;
    fRefactoring : TBRMRefactor;
    fCurrentBrowseInfo : string;
    function DoCreateEditor: IEditor;
    function CmdLineOpenFiles(): boolean;
    procedure DebuggerBreakpointChange(Sender: TObject; Editor : IEditor; ALine: integer);
    procedure DebuggerCurrentPosChange(Sender: TObject);
    procedure UpdateStandardActions;
    procedure UpdateStatusBarPanels;
    procedure ApplicationOnIdle(Sender: TObject; var Done: Boolean);
    procedure ApplicationOnHint(Sender: TObject);
    procedure ApplcationOnShowHint(var HintStr: string; var CanShow: Boolean;
      var HintInfo: THintInfo);
    procedure WMFindDefinition(var Msg: TMessage); message WM_FINDDEFINITION;
    procedure WMUpdateBreakPoints(var Msg: TMessage); message WM_UPDATEBREAKPOINTS;
    procedure WMSearchReplaceAction(var Msg: TMessage); message WM_SEARCHREPLACEACTION;
    procedure WMCheckForUpdates(var Msg: TMessage); message WM_CHECKFORUPDATES;
    procedure TBMThemeChange(var Message: TMessage); message TBM_THEMECHANGE;
    procedure TBXThemeMenuOnClick(Sender: TObject);
    procedure SyntaxClick(Sender : TObject);
    procedure SelectEditor(Sender : TObject);
  public
    PythonKeywordHelpRequested : Boolean;
    MenuHelpRequested : Boolean;
    ActionListArray : TActionListArray;
    Layouts : TStringList;
    zOrder : TList;
    zOrderPos : integer;
    zOrderProcessing : Boolean;
    procedure SaveEnvironment;
    procedure StoreApplicationData;
    procedure RestoreApplicationData;
    function DoOpenFile(AFileName: string; HighlighterName : string = '') : IEditor;
    function NewFileFromTemplate(FileTemplate : TFileTemplate) : IEditor;
    function GetActiveEditor : IEditor;
    procedure SaveFileModules;
    procedure UpdateDebugCommands(DebuggerState : TDebuggerState);
    function ShowFilePosition(FileName : string; Line, Offset : integer;
         ForceToMiddle : boolean = True) : boolean;
    procedure DebuggerStateChange(Sender: TObject; OldState,
      NewState: TDebuggerState);
    procedure DebuggerYield(Sender: TObject; DoIdle : Boolean);
    procedure DebuggerErrorPosChange(Sender: TObject);
    procedure SetCurrentPos(Editor : IEditor; ALine: integer);
    procedure PyIDEOptionsChanged;
    procedure SetupToolsMenu;
    procedure SetupLayoutsMenu;
    procedure SetupSyntaxMenu;
    procedure LayoutClick(Sender : TObject);
    procedure LoadLayout(Layout : string);
    procedure SaveLayout(Layout : string);
    procedure WriteStatusMsg(S : String);
    function JumpToFilePosInfo(FilePosInfo : string) : boolean;
    procedure FindDefinition(Editor : IEditor; TextCoord : TBufferCoord;
      ShowMessages, Silent, JumpToFirstMatch : Boolean; var FilePosInfo : string);
    procedure AdjustBrowserLists(FileName: string; Line: Integer; Col: Integer;
      FilePosInfo: string);
    procedure ThemeEditorGutter(Gutter : TSynGutter);
    procedure FillTBXThemeMenu;
  end;

function EditorFromTab(Tab : TJvTabBarItem) : IEditor;

Const
  ctkRemember : TDSACheckTextKind = 100;
  dsaSearchFromStart = 1;
  dsaReplaceFromStart = 2;
  dsaReplaceNumber = 3;
  dsaSearchStartReached = 4;
  dsaPostMortemInfo = 5;
var
  PyIDEMainForm: TPyIDEMainForm;

implementation

uses
  frmPythonII, frmMessages, PythonEngine, frmEditor,
  frmCallStack, frmBreakPoints, frmVariables, frmWatches,
  frmCodeExplorer, frmFileExplorer, JclFileUtils, frmToDo,
  frmFindResults, uParams, cTools, cParameters,
  frmCommandOutput, JvCreateProcess, dlgToolProperties, uCommonFunctions,
  TBXThemes, SynHighlighterPython, SynEditHighlighter, VarPyth, SynRegExpr,
  JvJVCLUtils, DateUtils, cPythonSourceScanner, frmRegExpTester,
  StringResources, dlgCommandLine, frmUnitTests, cFilePersist, frmIDEDockWin,
  dlgPickList, VirtualTrees, VirtualExplorerTree, JvDockGlobals, Math,
  cCodeHint, dlgNewFile, SynEditTextBuffer, JclSysInfo, cPyRemoteDebugger,
  uCmdLine, SynUnicode, uSearchHighlighter, frmModSpTBXCustomize, IniFiles,
  JclStrings;

{$R *.DFM}

{ TWorkbookMainForm }

function TPyIDEMainForm.DoCreateEditor: IEditor;
begin
  if GI_EditorFactory <> nil then begin
    Result := GI_EditorFactory.CreateTabSheet(EditorsPageList);
    Result.SynEdit.Assign(CommandsDataModule.EditorOptions);
    Result.SynEdit2.Assign(CommandsDataModule.EditorOptions);
  end else
    Result := nil;
end;

function TPyIDEMainForm.DoOpenFile(AFileName: string; HighlighterName : string = '') : IEditor;
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
  Result := DoCreateEditor;
  if Result <> nil then begin
    try
      Result.OpenFile(AFileName, HighlighterName);
      TBXMRUList.Remove(AFileName);
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
  ControlStyle := ControlStyle + [csOpaque];
  BGPanel.ControlStyle := BGPanel.ControlStyle + [csOpaque];
  DockServer.LeftDockPanel.ControlStyle := DockServer.LeftDockPanel.ControlStyle + [csOpaque];
  DockServer.RightDockPanel.ControlStyle := DockServer.LeftDockPanel.ControlStyle + [csOpaque];
  DockServer.TopDockPanel.ControlStyle := DockServer.LeftDockPanel.ControlStyle + [csOpaque];
  DockServer.BottomDockPanel.ControlStyle := DockServer.LeftDockPanel.ControlStyle + [csOpaque];
  TabBar.ControlStyle := TabBar.ControlStyle + [csOpaque];
  StatusBar.ControlStyle := StatusBar.ControlStyle + [csOpaque];
  // so that it gets repainted when there are no open files and the theme changes
  EditorsPageList.ControlStyle := EditorsPageList.ControlStyle - [csOpaque];

  SetDesktopIconFonts(Self.Font);  // For Vista
  SetDesktopIconFonts(JvmbTBXTabBarPainter.Font);
  SetDesktopIconFonts(JvmbTBXTabBarPainter.SelectedFont);
  SetDesktopIconFonts(JvmbTBXTabBarPainter.DisabledFont);
  JvmbTBXTabBarPainter.DisabledFont.Color := clGrayText;
  SetDesktopIconFonts(TJvDockVSNETTabServerOption(JvDockVSNetStyleTBX.TabServerOption).ActiveFont);
  SetDesktopIconFonts(TJvDockVSNETTabServerOption(JvDockVSNetStyleTBX.TabServerOption).InactiveFont);
  TJvDockVSNETTabServerOption(JvDockVSNetStyleTBX.TabServerOption).InactiveFont.Color := 5395794;
  SetDesktopIconFonts(ToolbarFont);

  AddThemeNotification(Self);

  Layouts := TStringList.Create;
  Layouts.Sorted := True;
  Layouts.Duplicates := dupError;

  zOrder := TList.Create;

  // Application Storage
  OptionsFileName := ChangeFileExt(ExtractFileName(Application.ExeName), '.ini');
  if FileExists(ChangeFileExt(Application.ExeName, '.ini')) then begin
    AppStorage.Location := flExeFile;
    AppStorage.FileName := OptionsFileName;
  end else if FileExists(PathAddSeparator(GetAppdataFolder) + OptionsFileName) then begin
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
  FindResultsWindow := TFindResultsWindow.Create(PyIDEMainForm);
  FindResultsWindow.PopupParent := PyIDEMainForm;

  // Assign Debugger Events
  with PyControl do begin
    OnBreakpointChange := DebuggerBreakpointChange;
    OnCurrentPosChange := DebuggerCurrentPosChange;
    OnErrorPosChange := DebuggerErrorPosChange;
    OnStateChange := DebuggerStateChange;
    OnYield := DebuggerYield;
  end;

  // ActionLists
  SetLength(ActionListArray, 3);
  ActionListArray[0] := actlStandard;
  ActionListArray[1] := CommandsDataModule.actlMain;
  ActionListArray[2] := PythonIIForm.InterpreterActionList;

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
          if (TObject(ItemsList[K]) as TTBCustomItem).Action = Action then begin
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

  // Read Settings from PyScripter.ini
  if FileExists(AppStorage.IniFile.FileName) then begin
    RestoreApplicationData;
  end;

  AppStorage.ReadStringList('Layouts', Layouts, True);

  if AppStorage.PathExists('Layouts\Current\Forms') then begin
    //LoadDockTreeFromAppStorage(AppStorage, 'Layouts\Current')
    LoadLayout('Current');
    AppStorage.ReadPersistent('Variables Window Options', VariablesWindow);
  end else begin
    TBXSwitcher.Theme := 'Office 2003';
    TabHost := ManualTabDock(DockServer.LeftDockPanel, FileExplorerWindow, CodeExplorerWindow);
    DockServer.LeftDockPanel.Width := 200;
    ManualTabDockAddPage(TabHost, UnitTestWindow);
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

  // Create Refactoring Helper
  fRefactoring := TBRMRefactor.Create;

  UpdateDebugCommands(PyControl.DebuggerState);
  //  Editor Views Menu
  GI_EditorFactory.SetupEditorViewMenu;

  Update;
  SendMessage(EditorsPageList.Handle, WM_SETREDRAW, 0, 0);  // To avoid flicker
  try
    // Open Files on the command line
    CmdLineOpenFiles();

    // if there was no file on the command line try restoring open files
    if CommandsDataModule.PyIDEOptions.RestoreOpenFiles  and
       (GI_EditorFactory.GetEditorCount = 0)
    then
      TPersistFileInfo.ReadFromAppStorage(AppStorage, 'Open Files');

    // If we still have no open file then open an empty file
    if GI_EditorFactory.GetEditorCount = 0 then
      DoOpenFile('', 'Python');
  finally
    EditorsPageList.Visible := False;
    EditorsPageList.Visible := True;
    SendMessage(EditorsPageList.Handle, WM_SETREDRAW, 1, 0);
    EditorsPageList.Invalidate;
    if Assigned(GetActiveEditor()) then
      GetActiveEditor.Activate;
    // Start the Python Code scanning thread
    CodeExplorerWindow.WorkerThread.Resume;
  end;

  // To get round the XP drawing bug with ExternalToolLED
  StatusBar.DoubleBuffered := True;
  StatusBar.Top := Height - StatusBar.Height;  // make sure is shown at the bottom

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
    if Windows.MessageBox(Handle,
      'A debugging session is in process.  Do you want to abort the session and Exit?',
       PChar(Application.Title), MB_ICONWARNING or MB_YESNO) = idYes then
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
    end else begin  // idNo
       CanClose := False;
       Exit;
    end;
  end;

  if OutputWindow.JvCreateProcess.State <> psReady then
    if Dialogs.MessageDlg('An External Tool is still running.  Do you want to terminate it and exit?',
        mtConfirmation, [mbYes, mbCancel], 0) = mrYes
    then begin
      OutputWindow.actToolTerminateExecute(Self);
      CanClose := True;
    end else
      CanClose := False;

  if GI_EditorFactory <> nil then
    CanClose := GI_EditorFactory.CanCloseAll;

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
    try
      Application.HelpCommand(HELP_QUIT, 0);
    except
    end;

    VariablesWindow.ClearAll;
    UnitTestWindow.ClearAll;
    CallStackWindow.ClearAll;

    // Give the time to the treads to terminate
    Sleep(200);

    //  We need to do this here so that MRU and docking information are persisted
    SaveEnvironment;

    SendMessage(EditorsPageList.Handle, WM_SETREDRAW, 0, 0);  // To avoid flicker
    if GI_EditorFactory <> nil then
      GI_EditorFactory.CloseAll;
    SendMessage(EditorsPageList.Handle, WM_SETREDRAW, 1, 0);

    RemoveThemeNotification(Self);
  end;
end;

procedure TPyIDEMainForm.TabbarContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
Var
  Tab : TJvTabBarItem;
begin
  Tab := Tabbar.TabAt(MousePos.X, MousePos.Y);
  if Assigned(Tab) then
    Tab.Selected := True;
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
end;

procedure TPyIDEMainForm.actNavInterpreterExecute(Sender: TObject);
begin
  ShowDockForm(PythonIIForm);
  PythonIIForm.FormActivate(Sender);
end;

procedure TPyIDEMainForm.actNavMessagesExecute(Sender: TObject);
begin
  ShowDockForm(MessagesWindow);
  MessagesWindow.FormActivate(Sender);
end;

procedure TPyIDEMainForm.actNavOutputExecute(Sender: TObject);
begin
  ShowDockForm(OutputWindow);
  OutputWindow.FormActivate(Sender);
end;

procedure TPyIDEMainForm.actNavRETesterExecute(Sender: TObject);
begin
  ShowDockForm(RegExpTesterWindow);
  RegExpTesterWindow.FormActivate(Sender);
end;

procedure TPyIDEMainForm.actNavTodoExecute(Sender: TObject);
begin
  ShowDockForm(ToDoWindow);
  ToDoWindow.FormActivate(Sender);
end;

procedure TPyIDEMainForm.actNavUnitTestsExecute(Sender: TObject);
begin
  ShowDockForm(UnitTestWindow);
  UnitTestWindow.FormActivate(Sender);
end;

procedure TPyIDEMainForm.actNavVariablesExecute(Sender: TObject);
begin
  ShowDockForm(VariablesWindow);
  VariablesWindow.FormActivate(Sender);
end;

procedure TPyIDEMainForm.actNavWatchesExecute(Sender: TObject);
begin
  ShowDockForm(WatchesWindow);
  WatchesWindow.FormActivate(Sender);
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
  TabBarItem : TJvTabBarItem;
begin
  if TabBar.Tabs.Count <= 1 then Exit;
  TabBarItem := nil;
  if CommandsDataModule.PyIDEOptions.SmartNextPrevPage then begin
    Repeat
      Inc(zOrderPos);
      if zOrderPos >= zOrder.Count then
        ZOrderPos := 0;
      while zOrderPos < zOrder.Count  do begin
        TabBarItem := zOrder[zOrderPos];
        if TabBar.Tabs.IndexOf(TabBarItem) < 0 then begin
          zOrder.Delete(zOrderPos);
          TabBarItem := nil;
        end else
          break;
      end;
    Until Assigned(TabBarItem) or (ZOrder.Count = 0);
    KeyPreview := True;
    zOrderProcessing := True;
  end else begin
    if Assigned(TabBar.SelectedTab) then
      TabBarItem := TabBar.SelectedTab.GetNextVisible
    else
      TabBarItem := TabBar.Tabs.Items[0];
  end;

  if not Assigned(TabBarItem) then
    TabBarItem := TabBar.Tabs.Items[0];
  if Assigned(TabBarItem) then
    TabBarItem.Selected := True;
end;

procedure TPyIDEMainForm.actPostMortemExecute(Sender: TObject);
begin
  PyControl.ActiveDebugger.EnterPostMortem;
end;

procedure TPyIDEMainForm.actPreviousEditorExecute(Sender: TObject);
Var
  TabBarItem : TJvTabBarItem;
begin
  if TabBar.Tabs.Count <= 1 then Exit;
  TabBarItem := nil;
  if CommandsDataModule.PyIDEOptions.SmartNextPrevPage then begin
    Repeat
      Dec(zOrderPos);
      if zOrderPos < 0 then
        zOrderPos := zOrder.Count - 1;
      while zOrderPos < zOrder.Count  do begin
        TabBarItem := zOrder[zOrderPos];
        if TabBar.Tabs.IndexOf(TabBarItem) < 0 then begin
          zOrder.Delete(zOrderPos);
          TabBarItem := nil;
        end else
          break;
      end;
    Until Assigned(TabBarItem) or (ZOrder.Count = 0);
    KeyPreview := True;
    zOrderProcessing := True;
  end else begin
    if Assigned(TabBar.SelectedTab) then
      TabBarItem := TabBar.SelectedTab.GetPreviousVisible
    else
      TabBarItem := TabBar.Tabs.Items[TabBar.Tabs.Count-1];
  end;
  if not Assigned(TabBarItem) then
    TabBarItem := TabBar.Tabs.Items[TabBar.Tabs.Count-1];
  if Assigned(TabBarItem) then
    TabBarItem.Selected := True;
end;

procedure TPyIDEMainForm.actPythonEngineExecute(Sender: TObject);
Var
  PythonEngineType : TPythonEngineType;
  Msg : string;
begin
  PythonEngineType := TPythonEngineType((Sender as TAction).Tag);
  PythonIIForm.SetPythonEngineType(PythonEngineType);
  case CommandsDataModule.PyIDEOptions.PythonEngineType of
    peInternal :  Msg := Format(SEngineActive, ['Internal','']);
    peRemote : Msg := Format(SEngineActive, ['Remote','']);
    peRemoteTk : Msg := Format(SEngineActive, ['Remote','(Tkinter) ']);
    peRemoteWx : Msg := Format(SEngineActive, ['Remote','(wxPython) ']);
  end;
  PythonIIForm.AppendText(WideLineBreak + Msg);
  PythonIIForm.AppendPrompt;
end;

procedure TPyIDEMainForm.actPythonReinitializeExecute(Sender: TObject);
begin
  if PyControl.DebuggerState <> dsInactive then begin
    if Dialogs.MessageDlg('The Python interpreter is busy.  Are you sure you want to terminate it?',
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
    MessagesWindow.AddMessage(Format('Syntax of %s is OK!', [ActiveEditor.FileTitle]));
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

  MessagesWindow.AddMessage(Format('Module %s was imported successfully!', [ActiveEditor.FileTitle]));
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

procedure TPyIDEMainForm.actRunExecute(Sender: TObject);
var
  ActiveEditor : IEditor;
begin
  Application.ProcessMessages;
  ActiveEditor := GetActiveEditor;
  if not Assigned(ActiveEditor) then Exit;

  if CommandsDataModule.PyIDEOptions.SaveFilesBeforeRun then
    SaveFileModules;
  if CommandsDataModule.PyIDEOptions.SaveEnvironmentBeforeRun then
    SaveEnvironment;

  PyControl.ActiveInterpreter.RunNoDebug(ActiveEditor);

  WriteStatusMsg('Script run OK');
  MessageBeep(MB_ICONASTERISK);
end;

procedure TPyIDEMainForm.actDebugExecute(Sender: TObject);
var
  ActiveEditor : IEditor;
begin
  Application.ProcessMessages;
  ActiveEditor := GetActiveEditor;
  if not Assigned(ActiveEditor) then Exit;

  if CommandsDataModule.PyIDEOptions.SaveFilesBeforeRun then
    SaveFileModules;
  if CommandsDataModule.PyIDEOptions.SaveEnvironmentBeforeRun then
    SaveEnvironment;

  PyControl.ActiveDebugger.Run(ActiveEditor);
end;

procedure TPyIDEMainForm.actDebugPauseExecute(Sender: TObject);
begin
  PyControl.ActiveDebugger.Pause;
end;

procedure TPyIDEMainForm.actStepIntoExecute(Sender: TObject);
var
  Editor : IEditor;
begin
  Editor := GetActiveEditor;
  if Assigned(Editor) then
    PyControl.ActiveDebugger.StepInto(Editor);
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
  Editor : IEditor;
begin
  Editor := GetActiveEditor;
  PyControl.ActiveDebugger.RunToCursor(Editor, Editor.SynEdit.CaretY);
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
    actDebug.Caption := 'Resume';
    actDebug.Hint := 'Resume|Resume the running script';
  end else begin
    actDebug.Caption := 'Debug';
    actDebug.Hint := 'Debug|Debug active script';
  end;
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
  StatusBar.Panels[0].Caption := ' ' + s;
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
Var
  i : integer;
begin
  UpdateStandardActions;
  CommandsDataModule.UpdateMainActions;
  UpdateStatusBarPanels;
  UpdateDebugCommands(PyControl.DebuggerState);
  for i := 0 to EditorsPageList.PageCount - 1 do
    if i < TabBar.Tabs.Count then
      TabBar.Tabs[i].Modified :=
        TEditorForm(TJvStandardPage(TabBar.Tabs[i].Data).Components[0]).GetEditor.Modified;
  for i := 0 to EditorViewsMenu.Count - 1 do
    EditorViewsMenu.Items[i].Enabled := Assigned(GI_ActiveEditor);
  if Assigned(GI_ActiveEditor) then
    TEditorForm(GI_ActiveEditor.Form).DoOnIdle;

  // If a Tk or Wx remote engine is active pump up event handling
  // This is for processing input output coming from event handlers
  if (PyControl.ActiveInterpreter is TPyRemoteInterpreter) and
     (PyControl.DebuggerState = dsInactive)
  then
    with(TPyRemoteInterpreter(PyControl.ActiveInterpreter)) do begin
      if IsConnected and (ServerType in [stTkinter, stWxPython]) then
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

function TPyIDEMainForm.ShowFilePosition(FileName: string; Line,
  Offset: integer; ForceToMiddle : boolean = True): boolean;
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
  if Assigned(EditorsPageList.ActivePage)  and
    (EditorsPageList.ActivePage.Components[0] is TEditorForm) then
    Result := TEditorForm(EditorsPageList.ActivePage.Components[0]).GetEditor
  else
    Result := nil;
end;

procedure TPyIDEMainForm.UpdateStatusBarPanels;
resourcestring
  SModified = 'Modified';
var
  ptCaret: TPoint;
begin
  if GI_ActiveEditor <> nil then begin
    ptCaret := GI_ActiveEditor.GetCaretPos;
    if (ptCaret.X > 0) and (ptCaret.Y > 0) then
      StatusBar.Panels[1].Caption := Format(' %6d:%3d ', [ptCaret.Y, ptCaret.X])
    else
      StatusBar.Panels[1].Caption := '';
    if GI_ActiveEditor.GetModified then
      StatusBar.Panels[2].Caption := SModified
    else
      StatusBar.Panels[2].Caption := '';
    StatusBar.Panels[3].Caption := GI_ActiveEditor.GetEditorState;
  end else begin
    StatusBar.Panels[1].Caption := '';
    StatusBar.Panels[2].Caption := '';
    StatusBar.Panels[3].Caption := '';
  end;
  if GetCapsLockKeyState then
    StatusBar.Panels[4].Caption := 'CAPS'
  else
    StatusBar.Panels[4].Caption := '';

  ExternalToolsLED.Visible := OutputWindow.JvCreateProcess.State <> psReady;
end;

function TPyIDEMainForm.CmdLineOpenFiles(): boolean;
var
  i : integer;
begin
  Result := False;
  for i := Low(CmdLineReader.readNamelessString) to High(CmdLineReader.readNamelessString) do
    if FileExists(CmdLineReader.readNamelessString[i]) then
        Result := Result or Assigned(DoOpenFile(CmdLineReader.readNamelessString[i]));
end;

procedure TPyIDEMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fRefactoring);
  FreeAndNil(Layouts);
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
    Title := 'Open File';
    FileName := '';
    Filter := GetHighlightersFilter(CommandsDataModule.Highlighters) + SFilterAllFiles;
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
var
  i: integer;
begin
  if GI_EditorFactory <> nil then begin
    if not GI_EditorFactory.CanCloseAll then
      exit;
    i := GI_EditorFactory.GetEditorCount - 1;
    // close all editor childs
    while i >= 0 do begin
      GI_EditorFactory.GetEditor(i).Close;
      Dec(i);
    end;
  end;
end;

procedure TPyIDEMainForm.TBXMRUListClick(Sender: TObject;
  const Filename: String);
begin
  TBXMRUList.Remove(Filename);
  DoOpenFile(Filename);
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
  CommandsDataModule.SynHTMLSyn.DefaultFilter :=
    CommandsDataModule.PyIDEOptions.HTMLFileFilter;
  CommandsDataModule.SynXMLSyn.DefaultFilter :=
    CommandsDataModule.PyIDEOptions.XMLFileFilter;
  CommandsDataModule.SynCssSyn.DefaultFilter :=
    CommandsDataModule.PyIDEOptions.CSSFileFilter;
  //  Dock animation parameters
  JvDockVSNetStyleTBX.SetAnimationInterval(CommandsDataModule.PyIDEOptions.DockAnimationInterval);
  JvDockVSNetStyleTBX.SetAnimationMoveWidth(CommandsDataModule.PyIDEOptions.DockAnimationMoveWidth);

  // Set Python engine
  PythonIIForm.SetPythonEngineType(CommandsDataModule.PyIDEOptions.PythonEngineType);

  // Command History Size
  PythonIIForm.CommandHistorySize := CommandsDataModule.PyIDEOptions.InterpreterHistorySize;


  TabBar.CloseButton := CommandsDataModule.PyIDEOptions.ShowTabCloseButton;
  case CommandsDataModule.PyIDEOptions.EditorTabPosition of
    toTop:
      begin
        TabBar.Orientation := toTop;
        TabBar.Align := alTop;
        for i  := 0 to GI_EditorFactory.Count - 1 do
          with TEditorForm(GI_EditorFactory.Editor[i].Form).ViewsTabBar do begin
            Orientation := toBottom;
            Align := alBottom;
          end;
      end;
    toBottom:
      begin
        TabBar.Orientation := toBottom;
        TabBar.Align := alBottom;
        for i  := 0 to GI_EditorFactory.Count - 1 do
          with TEditorForm(GI_EditorFactory.Editor[i].Form).ViewsTabBar do begin
            Orientation := toTop;
            Align := alTop;
          end;
      end;
  end;

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
  MemIni: TMemIniFile;
begin
  TempCursor := WaitCursor;
  TempStringList := TStringList.Create;
  AppStorage.BeginUpdate;
  try
    AppStorage.WriteString('PyScripter Version', ApplicationVersion);
    AppStorage.WritePersistent('IDE Options', CommandsDataModule.PyIDEOptions);
    with CommandsDataModule do begin
      AppStorage.DeleteSubTree('Editor Options');
      AppStorage.WritePersistent('Editor Options', EditorOptions);
      AppStorage.DeleteSubTree('Highlighters');
      for i := 0 to Highlighters.Count - 1 do
        AppStorage.WritePersistent('Highlighters\'+Highlighters[i],
          TPersistent(Highlighters.Objects[i]));

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
    AppStorage.WritePersistent('Find in Files Options', FindResultsWindow.FindInFilesExpert);
    AppStorage.WritePersistent('Find in Files Results Options', FindResultsWindow);
    AppStorage.WritePersistent('Variables Window Options', VariablesWindow);
    AppStorage.WritePersistent('RegExp Tester Options', RegExpTesterWindow);
    AppStorage.WriteBoolean('File Explorer Filter', FileExplorerWindow.actEnableFilter.Checked);
    AppStorage.WriteString('File Explorer Path', FileExplorerWindow.ExplorerPath);
    AppStorage.WriteStringList('File Explorer Favourites', FileExplorerWindow.Favourites);
    AppStorage.WriteStringList('Custom Params', CustomParams);
    AppStorage.DeleteSubTree('Tools');
    AppStorage.WriteCollection('Tools', ToolsCollection, 'Tool');
    AppStorage.WritePersistent('Tools\External Run', ExternalPython);
    AppStorage.WritePersistent('Output Window\Font', OutputWindow.lsbConsole.Font);
    AppStorage.WriteInteger('Output Window\Color', Integer(OutputWindow.lsbConsole.Color));
    AppStorage.WritePersistent('Watches', WatchesWindow);
    AppStorage.WriteStringList('Layouts', Layouts);
    AppStorage.WriteBoolean('Status Bar', StatusBar.Visible);
    // Save Theme Name
    AppStorage.WriteString('Theme Name', TBXSwitcher.Theme);

    // Save IDE Shortcuts
    AppStorage.DeleteSubTree('IDE Shortcuts');
    ActionProxyCollection := TActionProxyCollection.Create(ActionListArray);
    try
      AppStorage.WriteCollection('IDE Shortcuts', ActionProxyCollection, 'Action');
    finally
      ActionProxyCollection.Free;
    end;

    // Save Toolbar Items
    AppStorage.DeleteSubTree('Toolbar Items');
    MemIni := TMemIniFile.Create('');
    try
      SpSaveItems(Self, MemIni);
      TempStringList.Clear;
      MemIni.GetStrings(TempStringList);
      AppStorage.WriteStringList('Toolbar Items', TempStringList);
    finally
      MemIni.Free;
    end;

    // Save Interpreter History
    TempStringList.Clear;
    for I := 0 to PythonIIForm.CommandHistory.Count - 1 do
      TempStringList.Add(StrStringToEscaped(UTF8Encode(PythonIIForm.CommandHistory[i])));
    AppStorage.StorageOptions.PreserveLeadingTrailingBlanks := True;
    AppStorage.WriteStringList('Command Histrory', TempStringList);
    AppStorage.StorageOptions.PreserveLeadingTrailingBlanks := False;

  finally
    AppStorage.EndUpdate;
    TempStringList.Free;
  end;

  AppStorage.Flush;

  // Save MRU Lists
  TBXMRUList.SaveToIni(AppStorage.IniFile, 'MRU File List');
  CommandsDataModule.CommandLineMRU.SaveToIni(AppStorage.IniFile, 'CommandLine MRU');
end;

procedure TPyIDEMainForm.RestoreApplicationData;
Const
  DefaultHeader='$TITLE$\.1\.0\.-13\.Arial\.0\.96\.10\.0\.1\.2';
  DefaultFooter='$PAGENUM$\\.$PAGECOUNT$\.1\.0\.-13\.Arial\.0\.96\.10\.0\.1\.2';
Var
  ActionProxyCollection : TActionProxyCollection;
  TempStringList : TStringList;
  i : integer;
  MemIni: TMemIniFile;
begin
  if AppStorage.IniFile.SectionExists('IDE Options') then begin
    AppStorage.ReadPersistent('IDE Options', CommandsDataModule.PyIDEOptions);
    PyIDEOptionsChanged;
  end;
  if AppStorage.IniFile.SectionExists('Editor Options') then
    with CommandsDataModule do begin
      EditorOptions.Gutter.Gradient := False;  //default value
      AppStorage.ReadPersistent('Editor Options', EditorOptions);
      for i := 0 to Highlighters.Count - 1 do
        AppStorage.ReadPersistent('Highlighters\'+Highlighters[i],
          TPersistent(Highlighters.Objects[i]));
      CommandsDataModule.ApplyEditorOptions;

      if AppStorage.IniFile.SectionExists('Interpreter Editor Options') then begin
        InterpreterEditorOptions.Gutter.Gradient := False;  //default value
        AppStorage.ReadPersistent('Interpreter Editor Options', InterpreterEditorOptions);
        PythonIIForm.SynEdit.Assign(InterpreterEditorOptions);
        PythonIIForm.RegisterHistoryCommands;
      end;

      if AppStorage.IniFile.SectionExists('Editor Search Options') then begin
        AppStorage.ReadPersistent('Editor Search Options', EditorSearchOptions);
        tbiSearchText.Strings.CommaText := EditorSearchOptions.SearchTextHistory;
        tbiReplaceText.Strings.CommaText := EditorSearchOptions.ReplaceTextHistory;
      end;

      AppStorage.ReadPersistent('Print Options', SynEditPrint);
      SynEditPrint.Header.AsString := AppStorage.ReadWideString('Print Options\HeaderItems', DefaultHeader);
      SynEditPrint.Footer.AsString := AppStorage.ReadWideString('Print Options\FooterItems', DefaultFooter);

      AppStorage.StorageOptions.PreserveLeadingTrailingBlanks := True;
      if AppStorage.IniFile.SectionExists('File Templates') then
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
  if AppStorage.IniFile.SectionExists('ToDo Options') then
    AppStorage.ReadPersistent('ToDo Options', ToDoExpert);
  AppStorage.ReadPersistent('Find in Files Options', FindResultsWindow.FindInFilesExpert);
  AppStorage.ReadPersistent('Find in Files Results Options', FindResultsWindow);
  AppStorage.ReadPersistent('Variables Window Options', VariablesWindow);
  AppStorage.ReadPersistent('RegExp Tester Options', RegExpTesterWindow);
  FileExplorerWindow.actEnableFilter.Checked := AppStorage.ReadBoolean('File Explorer Filter', True);
  FileExplorerWindow.ExplorerPath := AppStorage.ReadString('File Explorer Path');
  AppStorage.ReadStringList('File Explorer Favourites', FileExplorerWindow.Favourites);
  AppStorage.ReadStringList('Custom Params', CustomParams);
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
  TBXSwitcher.Theme := AppStorage.ReadString('Theme Name', 'Office2003');
  // Load IDE Shortcuts
  ActionProxyCollection := TActionProxyCollection.Create(ActionListArray);
  try
    AppStorage.ReadCollection('IDE Shortcuts', ActionProxyCollection, True, 'Action');
    ActionProxyCollection.ApplyShortCuts(ActionListArray);
  finally
    ActionProxyCollection.Free;
  end;

  // Load Toolbar Items
  MemIni := TMemIniFile.Create('');
  TempStringList := TStringList.Create;
  try
    AppStorage.ReadStringList('Toolbar Items', TempStringList);
    MemIni.SetStrings(TempStringList);
    SpLoadItems(Self, MemIni);
  finally
    MemIni.Free;
    TempStringList.Free;
  end;

  // Restore Interpreter History
  TempStringList := TStringList.Create;
  try
    AppStorage.StorageOptions.PreserveLeadingTrailingBlanks := True;
    AppStorage.ReadStringList('Command Histrory', TempStringList);
    AppStorage.StorageOptions.PreserveLeadingTrailingBlanks := False;
    PythonIIForm.CommandHistory.Clear;

    for I := 0 to TempStringList.Count - 1 do
      PythonIIForm.CommandHistory.Add(UTF8Decode(StrEscapedToString(TempStringList[i])));
    PythonIIForm.CommandHistoryPointer := TempStringList.Count;  // one after the last one
  finally
    TempStringList.Free;
  end;


  // Load MRU Lists
  TBXMRUList.LoadFromIni(AppStorage.IniFile, 'MRU File List');
  CommandsDataModule.CommandLineMRU.LoadFromIni(AppStorage.IniFile, 'CommandLine MRU');

end;

procedure TPyIDEMainForm.TabBarTabSelected(Sender: TObject;
  Item: TJvTabBarItem);
Var
  WinControl : TWinControl;
  EditorForm : TEditorForm;
  Index : integer;
begin
  EditorSearchOptions.InitSearch;
  if Assigned(Item) and not (csDestroying in ComponentState) then begin
    EditorsPageList.ActivePage := Item.Data as TJvStandardPage;
    if Assigned(EditorsPageList.ActivePage)
       and (EditorsPageList.ActivePage.ControlCount > 0) then
    begin
      EditorForm := EditorsPageList.ActivePage.Controls[0] as TEditorForm;
      WinControl := EditorForm.EditorViews.ActivePage.Controls[0] as TWinControl;
      if WinControl.Visible and WinControl.CanFocus then
        ActiveControl := WinControl;
      // Code hint stuff
      EditorForm.SetUpCodeHints;
    end;
    // zOrder
    if not zOrderProcessing then begin
      Index := zOrder.IndexOf(Item);
      if Index < 0 then
        zOrder.Insert(0, Item)
      else
        zOrder.Move(Index, 0);
      zOrderPos := 0;
    end;
  end else if not Assigned(Item) then begin
    CodeHint.OnGetCodeHint := nil;
    CodeHint.OnHyperLinkClick := nil;
  end;
end;

function EditorFromTab(Tab : TJvTabBarItem) : IEditor;
Var
  Page : TJvStandardPage;
begin
  Result := nil;
  if Assigned(Tab) then begin
    Page := Tab.Data as TJvStandardPage;
    if Assigned(Page) and (Page.ControlCount > 0) then
      Result := (Page.Controls[0] as TEditorForm).GetEditor;
  end;
end;

procedure TPyIDEMainForm.TabBarMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  Editor : IEditor;
begin
  if Button = mbMiddle then begin
    Editor := EditorFromTab(TabBar.TabAt(X, Y));
    if Assigned(Editor) then begin
      (Editor as IFileCommands).ExecClose;
      TabBarTabSelected(Sender, TabBar.SelectedTab);
    end;
  end else if not Assigned(TabBar.ClosingTab) and (Shift = [ssLeft, ssDouble]) then begin
    if AppStorage.PathExists('Layouts\BeforeZoom\Forms') then
      actRestoreEditorExecute(Sender)
    else
      actMaximizeEditorExecute(Sender);
    end;
end;

procedure TPyIDEMainForm.TabBarTabClosed(Sender: TObject;
  Item: TJvTabBarItem);
Var
  Editor : IEditor;
begin
  Editor := EditorFromTab(Item);
  if Assigned(Editor) then
    (Editor as IFileCommands).ExecClose;
end;

procedure TPyIDEMainForm.UpdateStandardActions;
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
  actBrowseBack.Enabled := PreviousList.Strings.Count > 0;
  actBrowseForward.Enabled := NextList.Strings.Count > 0;
end;

procedure TPyIDEMainForm.FormShortCut(var Msg: TWMKey;
  var Handled: Boolean);
begin
  Handled := CommandsDataModule.actlMain.IsShortCut(Msg);
end;

procedure TPyIDEMainForm.CloseTimerTimer(Sender: TObject);
begin
  PostMessage(Application.Handle, WM_CLOSE, 0, 0);
  CloseTimer.Enabled := False;
end;

procedure TPyIDEMainForm.JvDropTargetDragDrop(Sender: TJvDropTarget;
  var Effect: TJvDropEffect; Shift: TShiftState; X, Y: Integer);
Var
  List : TStringList;
  i : integer;
begin
  List := TStringList.Create;
  try
    JvDropTarget.GetFilenames(List);
    for i := 0 to List.Count - 1 do
      DoOpenFile(List[i]);
  finally
    List.Free;
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
    MenuItem.Hint := Format('Apply %s layout', [Layouts[i]]);
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
        (Editor.Synedit.Highlighter.LanguageName = mnSyntax.Items[i].Caption);
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
    MenuItem.Hint := Format('Use %s syntax', [MenuItem.Caption]);
  end;
end;

procedure TPyIDEMainForm.LoadLayout(Layout: string);
Var
  Path : string;
  i : integer;
  SaveActiveControl : TWinControl;
  TempCursor : IInterface;
  ToolbarLayout: TStringList;
begin
  Path := 'Layouts\'+ Layout;
  if AppStorage.PathExists(Path + '\Forms') then begin
    TempCursor := WaitCursor;
    SaveActiveControl := ActiveControl;

    SendMessage(EditorsPageList.Handle, WM_SETREDRAW, 0, 0);
    try
      for i := 0 to EditorsPageList.PageCount - 1 do
        TEditorForm(EditorsPageList.Pages[i].Components[0]).Visible := False;

      // Now Load the DockTree
      LoadDockTreeFromAppStorage(AppStorage, Path);
    finally
      SendMessage(EditorsPageList.Handle, WM_SETREDRAW, 1, 0);
      for i := 0 to EditorsPageList.PageCount - 1 do begin
        TEditorForm(EditorsPageList.Pages[i].Components[0]).Parent := nil;
        TEditorForm(EditorsPageList.Pages[i].Components[0]).Parent := EditorsPageList.Pages[i];
        TEditorForm(EditorsPageList.Pages[i].Components[0]).Visible := True;
      end;
      EditorsPageList.Invalidate;
      for i := 0 to Screen.FormCount - 1 do begin
        if Screen.Forms[i] is TIDEDockWindow then
          TIDEDockWindow(Screen.Forms[i]).FormDeactivate(Self);
      end;
    end;
    if Assigned(SaveActiveControl) and SaveActiveControl.Visible
      and GetParentForm(SaveActiveControl).Visible and SaveActiveControl.CanFocus
    then
      try
        SaveActiveControl.SetFocus;
      except
      end;
  end;

  // Now Restore the toolbars
  if AppStorage.PathExists(Path + '\Toolbars') then begin
    ToolbarLayout := TStringList.Create;
    try
      AppStorage.ReadStringList(Path + '\Toolbars', ToolbarLayout);
      SpTBXCustomizer.LoadLayout(ToolbarLayout, Layout);
    finally
      ToolbarLayout.Free;
    end;
  end;

end;

procedure TPyIDEMainForm.SaveLayout(Layout: string);
Var
  ToolbarLayout : TStringList;
begin
  Appstorage.DeleteSubTree('Layouts\'+Layout);
  SaveDockTreeToAppStorage(AppStorage, 'Layouts\'+ Layout);

  ToolbarLayout := TStringList.Create;
  try
    SpTBXCustomizer.SaveLayout(ToolbarLayout, Layout);
    AppStorage.WriteStringList('Layouts\'+ Layout + '\Toolbars', ToolbarLayout);
  finally
    ToolbarLayout.Free;
  end;
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
  if InputQuery('Save current Layout', 'Layout Name:', LayoutName) then begin
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
    Caption := 'Delete Layouts';
    lbMessage.Caption := 'Please select the layouts you want to delete and press the OK button:';
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
  Editor : IEditor;
begin
  Editor := GetActiveEditor;
  if Assigned(Editor) then
    Editor.Activate;
  if Assigned(GI_ActiveEditor) then
    OutputWindow.ExecuteTool(ExternalPython);
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
  EditTool(ExternalPython);
end;

procedure TPyIDEMainForm.WriteStatusMsg(S: String);
begin
  StatusBar.Panels[0].Caption := S;
end;

procedure TPyIDEMainForm.ThemeEditorGutter(Gutter : TSynGutter);
Var
  GradColor : TColor;
begin
  GradColor := CurrentTheme.GetViewColor(VT_TOOLBAR);

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
    if zOrderPos > 0 then begin
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
    PreviousList.Strings.Insert(0, Format(FilePosInfoFormat, [FileName, Line, Col]));
    NextList.Strings.Clear;
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
  FirstMatch : Variant;
  Defs : Variant;
  Token : WideString;
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
              Dialogs.MessageDlg('This is the definition of "'+ Token + '"',
                mtInformation, [mbOK], 0);
            Exit;
          end;
        Ord(tkIdentifier) :
          begin
            TempCursor := WaitCursor;

            FName := Editor.GetFileNameOrTitle;

            if ShowMessages then begin
              MessagesWindow.ClearMessages;
              MessagesWindow.AddMessage('Definition(s) of "'+ Token + '"');
            end;

            FileName := '';
            Line := 0;
            Col := 1;
            if CommandsDataModule.PyIDEOptions.UseBicycleRepairMan
              and fRefactoring.RefactoringIsAvailable then
            begin
              Defs := fRefactoring.FindDefinitionByCoordinates(FName,
                CaretY, CaretX - 1, ShowMessages, Silent); //ColNo zero based!!

              fRefactoring.ProcessBRMMatches(Defs, ShowMessages, Silent, FirstMatch);

              if not VarIsPython(FirstMatch) or VarIsNone(FirstMatch) then begin
              end else begin
                FileName := FirstMatch.filename;
                Line := FirstMatch.lineno;
                Col := FirstMatch.colno + 1;
              end;
            end else begin
              // use internal refactoring
              CE := PyScripterRefactor.FindDefinitionByCoordinates(FName,
                CaretY, CaretX, ErrMsg);
              if Assigned(CE) and not CE.IsProxy then begin
                ParsedModule := CE.GetModule;
                FileName := ParsedModule.FileName;
                Line := CE.CodePos.LineNo;
                Col := CE.CodePos.CharOffset;
                if ShowMessages then
                  MessagesWindow.AddMessage('  Definition found', FileName, Line, Col);
              end;
            end;

            if ShowMessages then
              ShowDockForm(MessagesWindow);
            if FileName  <> '' then begin
              FilePosInfo := Format(FilePosInfoFormat, [Filename, Line, Col]);
              if JumpToFirstMatch then
                ShowFilePosition(Filename, Line, Col);
            end else begin
              if ShowMessages then
                MessagesWindow.AddMessage('  Not found');
              MessageBeep(MB_ICONASTERISK);
            end;
          end;
      else if not Silent then
        Dialogs.MessageDlg('Please place the cursor on a function or a class name.',
          mtError, [mbOK], 0);
      end;
    end;
  end;
end;

procedure TPyIDEMainForm.FindToolbarVisibleChanged(Sender: TObject);
begin
  if not FindToolbar.Visible then
    ClearAllHighlightedTerms;
end;

procedure TPyIDEMainForm.actFindReferencesExecute(Sender: TObject);
var
  Defs, FirstMatch : Variant;
  Token : WideString;
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
            MessagesWindow.AddMessage('References of "'+ Token + '"');

            FoundReferences := False;
            if CommandsDataModule.PyIDEOptions.UseBicycleRepairMan
              and fRefactoring.RefactoringIsAvailable then
            begin
              Defs := fRefactoring.FindReferencesByCoordinates(FName,
                CaretY, CaretX - 1); //ColNo zero based!!
              fRefactoring.ProcessBRMMatches(Defs, True, False, FirstMatch);
              if VarIsPython(FirstMatch) and not VarIsNone(FirstMatch) then
                FoundReferences := True;
            end else begin
              ResultsList := TStringList.Create;
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
                      MessagesWindow.AddMessage('  Certainty 100%', Filename, Line, Col);
                    end;
                  end;
                finally
                  RegExpr.Free;
                end;
              finally
                ResultsList.Free;
              end;
            end;

            ShowDockForm(MessagesWindow);
            if not FoundReferences then begin
              MessagesWindow.AddMessage('  Not found');
              MessageBeep(MB_ICONASTERISK);
            end;
          end;
      else
        Dialogs.MessageDlg('Please place the cursor on a function/class name or identifier',
          mtError, [mbOK], 0);
      end;
    end;
  end;
end;

procedure TPyIDEMainForm.TBMThemeChange(var Message: TMessage);
begin
  if Message.WParam = TSC_VIEWCHANGE then begin
    // Update EditorOptions
    ThemeEditorGutter(CommandsDataModule.EditorOptions.Gutter);
    BGPanel.Color := CurrentTheme.GetItemColor(GetItemInfo('inactive'));
    Application.HintColor := CurrentTheme.GetViewColor(VT_DOCKPANEL);
  end;
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
    if Msg.WParam = 0 then begin
      SpFocusEditItem(tbiReplaceText, FindToolbar.View);
      tbiReplaceText.StartEditing(FindToolbar.View);
    end else if Msg.WParam = 1 then begin
      SpFocusEditItem(tbiSearchText, FindToolbar.View);
      tbiSearchText.StartEditing(FindToolbar.View);
    end else if Msg.WParam = 2 then begin
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

procedure TPyIDEMainForm.PreviousListClick(Sender: TObject);
Var
  i, Index : integer;
begin
  Index := PreviousList.ItemIndex;
  if (Index >= 0) and (Index < PreviousList.Strings.Count) then begin
    JumpToFilePosInfo(PreviousList.Strings[Index]);
    NextList.Strings.Insert(0 , fCurrentBrowseInfo);
    fCurrentBrowseInfo := PreviousList.Strings[Index];
    for i := 0 to Index - 1 do
      NextList.Strings.Insert(0, PreviousList.Strings[i]);
    for i := Index downto 0 do
      PreviousList.Strings.Delete(i);
  end;
end;

procedure TPyIDEMainForm.tbiBrowsePreviousClick(Sender: TObject);
begin
  if PreviousList.Strings.Count > 0 then begin
    PreviousList.ItemIndex := 0;
    PreviousListClick(Sender);
  end;
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

procedure TPyIDEMainForm.NextListClick(Sender: TObject);
Var
  i, Index : integer;
begin
  Index := NextList.ItemIndex;
  if (Index >= 0) and (Index < NextList.Strings.Count) then begin
    JumpToFilePosInfo(NextList.Strings[Index]);
    PreviousList.Strings.Insert(0 , fCurrentBrowseInfo);    fCurrentBrowseInfo := NextList.Strings[Index];
    for i := 0 to Index - 1 do
      PreviousList.Strings.Insert(0, NextList.Strings[i]);
    for i := Index downto 0 do
      NextList.Strings.Delete(i);
  end;
end;

procedure TPyIDEMainForm.tbiBrowseNextClick(Sender: TObject);
begin
  if NextList.Strings.Count > 0 then begin
    NextList.ItemIndex := 0;
    NextListClick(Sender);
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
  FillTBXThemeMenu;
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
    OnAfterShellNotify := CommandsDataModule.FileExplorerTreeAfterShellNotify;
  end;
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

procedure TPyIDEMainForm.TBXThemeMenuOnClick(Sender: TObject);
begin
  TBXSetTheme(TSpTBXItem(Sender).Caption);
  TSpTBXItem(Sender).Checked := True;
end;

procedure TPyIDEMainForm.FillTBXThemeMenu;
const
  ThemeHint = 'Set theme to "%s".';
var
  i: Integer;
  sl: TStringList;
  x: TSpTBXItem;
begin
  mnThemes.Clear;
  sl := TStringList.Create;
  try
    TBXThemes.GetAvailableTBXThemes(sl);
    sl.Sorted := True;
    for i := 0 to Pred(sl.Count) do
    begin
      x := TSpTBXItem.Create(Self);
      with x do
      begin
        Caption := sl[i];
        Hint := Format(ThemeHint, [sl[i]]);
        OnClick := TBXThemeMenuOnClick;
        AutoCheck := True;
        GroupIndex := 1;
        Checked := TBXCurrentTheme = sl[i];
      end;
      mnThemes.Add(x);
    end;
  finally
    sl.Free;
  end;
end;

function TPyIDEMainForm.FindAction(var Key: Word; Shift: TShiftState) : TCustomAction;
var
  ShortCut : TShortCut;
  i, j : Integer;
  Action : TContainedAction;
  ActionList : TActionList;
begin
  Result := nil;
  ShortCut := Menus.ShortCut(Key, Shift);
  if ShortCut <> scNone then
    for j := 0 to Length(ActionListArray) do begin
      if j = Length(ActionListArray) then
        ActionList := actlImmutable
      else
        ActionList := ActionListArray[j];
      for i := 0 to ActionList.ActionCount - 1 do
      begin
        Action := ActionList[I];
        if (TCustomAction(Action).ShortCut = ShortCut) or
           (TCustomAction(Action).SecondaryShortCuts.IndexOfShortCut(ShortCut) <> -1) then
        begin
          Result := TCustomAction(Action);
          Exit;
        end;
      end;
    end;
end;

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
          Panel.DockClients[Panel.DockClientCount-1]as TWinControl);
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
      MenuItem.OnClick := SelectEditor;
    end;
  finally
    List.Free;
  end;
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
    tbiIcrementalSearch.Checked := IncrementalSearch;
  end;
end;

procedure TPyIDEMainForm.tbiSearchTextAcceptText(Sender: TObject;
  var NewText: WideString; var Accept: Boolean);
Var
  S : WideString;
  i: integer;
begin
  Accept := True;
  if NewText <> '' then begin
    // update Items
    S := NewText;
    i := tbiSearchText.Strings.IndexOf(s);
    if i > -1 then begin
      tbiSearchText.Strings.Delete(i);
      tbiSearchText.Strings.Insert(0, S);
    end else
      tbiSearchText.Strings.Insert(0, s);
    // Update History
    S := '';
    for i := 0 to tbiSearchText.Strings.Count - 1 do begin
      if i >= 10 then
        break;
      if i > 0 then
        S :=  S + ',';
      S := S + WideQuotedStr(tbiSearchText.Strings[i], '"');
    end;
    EditorSearchOptions.SearchTextHistory := S;
  end;
end;

procedure TPyIDEMainForm.tbiSearchReplaceTextBeginEdit(Sender: TTBEditItem;
  Viewer: TTBEditItemViewer; EditControl: TEdit);
begin
  if Sender = tbiSearchText then begin
    EditControl.OnKeyDown := tbiSearchTextKeyDown;
    EditControl.OnKeyPress := tbiSearchTextKeyPress;
  end else if Sender = tbiReplaceText then
    EditControl.OnKeyDown := tbiReplaceTextKeyDown;
end;

procedure TPyIDEMainForm.tbiSearchTextKeyPress(Sender: TObject; var Key: Char);
begin
//  // Incremental Search
  if EditorSearchOptions.IncrementalSearch then
    PostMessage(Handle, WM_SEARCHREPLACEACTION, 2, 0);
end;

procedure TPyIDEMainForm.tbiSearchTextKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
Var
  Action : TCustomAction;
begin
  if Key = VK_ESCAPE then begin
    SendMessage((Sender as TEdit).Handle, WM_KILLFOCUS, 0, 0);
    Key := 0;
    FindToolbar.Visible := False;
  end else if Key = VK_RETURN then begin
    Key := 0;
    FindToolbar.View.CancelMode;
    PostMessage(Handle, WM_SEARCHREPLACEACTION, 0,
      Integer(CommandsDataModule.actSearchFindNext));
  end else if (Key = VK_TAB) and (Shift = []) and tbiReplaceText.Visible then begin
    PostMessage(Handle, WM_SEARCHREPLACEACTION, 0, 0);
    Key := 0;
  end else begin
    Action := FindAction(Key, Shift);
    if Assigned(Action) and not Action.HandlesTarget(Sender) then begin
      FindToolbar.View.CancelMode;
      Key := 0; // mark as handled
      PostMessage(Handle, WM_SEARCHREPLACEACTION, 0, Integer(Action));
    end;
  end;
end;

procedure TPyIDEMainForm.tbiSearchTextChange(Sender: TObject;
  const Text: WideString);
begin
  EditorSearchOptions.SearchText := Text;
  EditorSearchOptions.InitSearch;
  CommandsDataModule.UpdateMainActions;
  ClearAllHighlightedTerms;
  if CommandsDataModule.actSearchHighlight.Enabled and
    CommandsDataModule.actSearchHighlight.Checked
  then
    CommandsDataModule.actSearchHighlightExecute(Sender);
end;

procedure TPyIDEMainForm.tbiReplaceTextAcceptText(Sender: TObject;
  var NewText: WideString; var Accept: Boolean);
Var
  S : WideString;
  i: integer;
begin
  Accept := True;
  if NewText <> '' then begin
    // update Items
    S := NewText;
    i := tbiReplaceText.Strings.IndexOf(s);
    if i > -1 then begin
      tbiReplaceText.Strings.Delete(i);
      tbiReplaceText.Strings.Insert(0, S);
    end else
      tbiReplaceText.Strings.Insert(0, s);
    // Update History
    S := '';
    for i := 0 to tbiReplaceText.Strings.Count - 1 do begin
      if i >= 10 then
        break;
      if i > 0 then
        S := S + ',';
      S := S + WideQuotedStr(tbiReplaceText.Strings[i], '"');
    end;
    EditorSearchOptions.ReplaceTextHistory := S;
  end;
end;

procedure TPyIDEMainForm.tbiReplaceTextChange(Sender: TObject;
  const Text: WideString);
begin
  EditorSearchOptions.ReplaceText := Text;
  EditorSearchOptions.InitSearch;
  CommandsDataModule.UpdateMainActions;
end;

procedure TPyIDEMainForm.tbiReplaceTextKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then begin
    SendMessage((Sender as TEdit).Handle, WM_KILLFOCUS, 0, 0);
    Key := 0;
    FindToolbar.Visible := False;
  end else if Key = VK_RETURN then begin
    Key := 0;
    FindToolbar.View.CancelMode;
    PostMessage(Handle, WM_SEARCHREPLACEACTION, 0,
      Integer(CommandsDataModule.actSearchReplaceNow));
  end else if (Key = VK_TAB) and (Shift = [ssShift]) and tbiSearchText.Visible then begin
    PostMessage(Handle, WM_SEARCHREPLACEACTION, 1, 0);
    Key := 0;
  end else begin
    Action := FindAction(Key, Shift);
    if Assigned(Action) and not Action.HandlesTarget(Sender) then begin
      FindToolbar.View.CancelMode;
      Key := 0; // mark as handled
      PostMessage(Handle, WM_SEARCHREPLACEACTION, 0, Integer(Action));
    end;
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
    IncrementalSearch := tbiIcrementalSearch.Checked;
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


