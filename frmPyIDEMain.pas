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

 History:   v 1.7.2.2
          Bug fixes
            Shell Integration - Error when opening multiple files
            Configure External Run - ParseTraceback not saved properly

  ** Pyscripter flickers a lot when resizing.  I do not know what to do
     about it. In fact this may be a Windows problem.  Even in .NET try the
     following using C#.  Create a form, add a panel with a caption aligned
     to the bottom.  Run and resize fast. The label of the panel flickers.

-----------------------------------------------------------------------------}
// TODO: Remote interpreter and debugger
// TODO: Project Manager

// Bugs and minor features
// Internal Tool as in pywin
// TODO: Interpreter raw_input

// TODO: Option to move editor and docking tabs at the top.  (Maybe)
// TODO: sort order in code completion
// TODO: Improve parameter completion with an option to provide more help (docstring)

// TODO: Find module expert

// TODO: UML Editor View
// TODO: Refactorings using BRM

// TODO3: Pythonfile context option

// TODO: Plugin architecture
// TODO Package as an Application Scripter Component

unit frmPyIDEMain;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Variants, dmCommands, ComCtrls, StdActns, ActnList, Menus, uEditAppIntfs,
  JvDockControlForm, JvDockVIDStyle, JvDockVSNetStyle, JvComponent,
  SynEditTypes, SynEditMiscClasses, SynEditRegexSearch, cPyBaseDebugger,
  cPyDebugger, ToolWin,  ExtCtrls, JvExComCtrls, JvAppStorage,
  JvAppIniStorage, JvExControls, JvLED, JvFormPlacement, SynEdit, JvTabBar,
  JvDragDrop, XPMan, TB2Dock, TB2Toolbar, TBX, TBXSwitcher, TB2Item,
  TBXStatusBars, JvmbTBXTabBarPainter, JvDockVSNETStyleTBX,
  TB2MRU, TBXExtItems,  JvPageList, cRefactoring, dlgCustomShortcuts,
  // Themes
  TBXNexosXTheme, TBXOfficeXPTheme, TBXAluminumTheme, TBXWhidbeyTheme,
  TBXOffice2003Theme, TBXOffice2007Theme, TBXLists, TB2ExtItems, JvDockTree,
  JvComponentBase, JvAppInst, uHighlighterProcs, cFileTemplates;

const
  WM_FINDDEFINITION  = WM_USER + 100;
  WM_CHECKFORUPDATES = WM_USER + 110;
  WM_UPDATEBREAKPOINTS  = WM_USER + 120;

type
  TPyIDEMainForm = class(TForm)             
    DockServer: TJvDockServer;
    actlStandard : TActionList;
    actRun: TAction;
    actRunToCursor: TAction;
    actStepInto: TAction;
    actStepOver: TAction;
    actStepOut: TAction;
    actToggleBreakPoint: TAction;
    actClearAllBreakpoints: TAction;
    actViewDebugToolbar: TAction;
    actViewMainToolBar: TAction;
    actCallStackWin: TAction;
    actVariablesWin: TAction;
    actBreakPointsWin: TAction;
    actWatchesWin: TAction;
    actDebugAbort: TAction;
    actMessagesWin: TAction;
    actDebug: TAction;
    actViewII: TAction;
    actNextEditor: TAction;
    actPreviousEditor: TAction;
    actViewCodeExplorer: TAction;
    AppStorage: TJvAppIniFileStorage;
    actFileNewModule: TAction;
    actFileOpen: TAction;
    actFileCloseAll: TAction;
    actFileExit: TAction;
    actViewStatusBar: TAction;
    JvFormStorage: TJvFormStorage;
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
    TBXDockTop: TTBXDock;
    MainMenu: TTBXToolbar;
    FileMenu: TTBXSubmenuItem;
    New1: TTBXItem;
    Open1: TTBXItem;
    N14: TTBXSeparatorItem;
    Close1: TTBXItem;
    CloseAll2: TTBXItem;
    N1: TTBXSeparatorItem;
    Save1: TTBXItem;
    SaveAs1: TTBXItem;
    SaveAll1: TTBXItem;
    N2: TTBXSeparatorItem;
    PageSetup1: TTBXItem;
    PrinterSetup1: TTBXItem;
    PrintPreview1: TTBXItem;
    Print1: TTBXItem;
    N4: TTBXSeparatorItem;
    N3: TTBXItem;
    EditMenu: TTBXSubmenuItem;
    Undo1: TTBXItem;
    Redo1: TTBXItem;
    N5: TTBXSeparatorItem;
    Cut1: TTBXItem;
    Copy1: TTBXItem;
    IDEOptions1: TTBXItem;
    Delete1: TTBXItem;
    SelectAll1: TTBXItem;
    N6: TTBXSeparatorItem;
    Parameters1: TTBXSubmenuItem;
    PageSetup2: TTBXItem;
    Insertmodifier1: TTBXItem;
    N16: TTBXSeparatorItem;
    Replaceparameter1: TTBXItem;
    CodeTemplate1: TTBXItem;
    SourceCode1: TTBXSubmenuItem;
    IndentBlock1: TTBXItem;
    DedentBlock1: TTBXItem;
    Commentout1: TTBXItem;
    abify1: TTBXItem;
    Untabify1: TTBXItem;
    SearchMenu: TTBXSubmenuItem;
    Find1: TTBXItem;
    FindNext1: TTBXItem;
    FindPrevious1: TTBXItem;
    Replace1: TTBXItem;
    N15: TTBXSeparatorItem;
    FindinFiles1: TTBXItem;
    N7: TTBXSeparatorItem;
    Replace2: TTBXItem;
    FindinFiles2: TTBXItem;
    N23: TTBXSeparatorItem;
    MatchingBrace1: TTBXItem;
    RunMenu: TTBXSubmenuItem;
    SyntaxCheck1: TTBXItem;
    ImportModule1: TTBXItem;
    N21: TTBXSeparatorItem;
    Run2: TTBXItem;
    N22: TTBXSeparatorItem;
    ExternalRun1: TTBXItem;
    ConfigureExternalRun1: TTBXItem;
    N8: TTBXSeparatorItem;
    Debug1: TTBXItem;
    RunToCursor1: TTBXItem;
    StepInto1: TTBXItem;
    StepOver1: TTBXItem;
    StepOut1: TTBXItem;
    AbortDebugging1: TTBXItem;
    N9: TTBXSeparatorItem;
    ogglebreakpoint1: TTBXItem;
    ClearAllBreakpoints1: TTBXItem;
    ToolsMenu: TTBXSubmenuItem;
    PythonPath1: TTBXItem;
    N13: TTBXSeparatorItem;
    ConfigureTools1: TTBXItem;
    N20: TTBXSeparatorItem;
    Options1: TTBXSubmenuItem;
    IDEOptions2: TTBXItem;
    EditorOptions1: TTBXItem;
    CustomizeParameters1: TTBXItem;
    CodeTemplates1: TTBXItem;
    ViewMenu: TTBXSubmenuItem;
    NextEditor1: TTBXItem;
    PreviousEditor1: TTBXItem;
    N10: TTBXSeparatorItem;
    oolbars1: TTBXSubmenuItem;
    MainToolBar1: TTBXItem;
    DebugToolBar1: TTBXItem;
    StatusBar1: TTBXItem;
    InteractiveInterpreter1: TTBXItem;
    FileExplorer1: TTBXItem;
    CodeExplorer1: TTBXItem;
    actViewToDoList1: TTBXItem;
    FindinFilesResults1: TTBXItem;
    actViewOutput1: TTBXItem;
    DebugWindows1: TTBXSubmenuItem;
    CallStack1: TTBXItem;
    Variables1: TTBXItem;
    Breakpoints1: TTBXItem;
    Watches1: TTBXItem;
    Messages1: TTBXItem;
    HelpMenu: TTBXSubmenuItem;
    PythonPath2: TTBXItem;
    N18: TTBXSeparatorItem;
    PyScripter1: TTBXSubmenuItem;
    CustomParameters1: TTBXItem;
    ExternalTools1: TTBXItem;
    N17: TTBXSeparatorItem;
    About1: TTBXItem;
    MainToolBar: TTBXToolbar;
    TBXItem1: TTBXItem;
    TBXItem2: TTBXItem;
    TBXItem3: TTBXItem;
    TBXItem4: TTBXItem;
    TBXSeparatorItem1: TTBXSeparatorItem;
    TBXItem5: TTBXItem;
    TBXSeparatorItem2: TTBXSeparatorItem;
    TBXItem6: TTBXItem;
    TBXItem7: TTBXItem;
    TBXItem8: TTBXItem;
    TBXSeparatorItem3: TTBXSeparatorItem;
    TBXItem9: TTBXItem;
    TBXItem10: TTBXItem;
    TBXSeparatorItem4: TTBXSeparatorItem;
    TBXItem11: TTBXItem;
    TBXItem12: TTBXItem;
    TBXItem13: TTBXItem;
    TBXItem14: TTBXItem;
    TBXSeparatorItem5: TTBXSeparatorItem;
    TBXItem15: TTBXItem;
    DebugToolbar: TTBXToolbar;
    TBXItem16: TTBXItem;
    TBXSeparatorItem6: TTBXSeparatorItem;
    TBXItem22: TTBXItem;
    TBXItem21: TTBXItem;
    TBXItem20: TTBXItem;
    TBXItem18: TTBXItem;
    TBXItem19: TTBXItem;
    TBXItem17: TTBXItem;
    TBXSeparatorItem7: TTBXSeparatorItem;
    TBXItem24: TTBXItem;
    TBXItem23: TTBXItem;
    JvmbTBXTabBarPainter: TJvmbTBXTabBarPainter;
    ViewToolbar: TTBXToolbar;
    TBXSubmenuItem2: TTBXSubmenuItem;
    TBXDockLeft: TTBXDock;
    StatusBar: TTBXStatusBar;
    StatusLED: TJvLED;
    ExternalToolsLED: TJvLED;
    TBXDockRight: TTBXDock;
    TBXDockBottom: TTBXDock;
    JvDockVSNetStyleTBX: TJvDockVSNetStyleTBX;
    mnTools: TTBXSubmenuItem;
    TabBarPopupMenu: TTBXPopupMenu;
    New2: TTBXItem;
    NewModule2: TTBXItem;
    CloseAll1: TTBXItem;
    N12: TTBXSeparatorItem;
    EditorOptions2: TTBXItem;
    TBXMRUList: TTBXMRUList;
    TBXMRUListItem: TTBXMRUListItem;
    RecentSubmenu: TTBXSubmenuItem;
    EditorViewsMenu: TTBXSubmenuItem;
    EditorsPageList: TJvPageList;
    TBXSeparatorItem8: TTBXSeparatorItem;
    mnThemes: TTBXSubmenuItem;
    ThemesVisibilityToggle: TTBXVisibilityToggleItem;
    EditorToolbar: TTBXToolbar;
    TBXItem27: TTBXItem;
    TBXItem28: TTBXItem;
    TBXSeparatorItem10: TTBXSeparatorItem;
    TBXItem30: TTBXItem;
    TBXSeparatorItem11: TTBXSeparatorItem;
    TBXItem31: TTBXItem;
    TBXItem32: TTBXItem;
    TBXVisibilityToggleItem1: TTBXVisibilityToggleItem;
    TBXItem25: TTBXItem;
    TBXItem26: TTBXItem;
    actFindDefinition: TAction;
    TBXItem33: TTBXItem;
    TBXSeparatorItem9: TTBXSeparatorItem;
    TBXSubmenuItem3: TTBXSubmenuItem;
    TBXItem34: TTBXItem;
    TBXItem35: TTBXItem;
    TBXItem36: TTBXItem;
    TBXSeparatorItem12: TTBXSeparatorItem;
    TBXItem37: TTBXItem;
    TBXSeparatorItem13: TTBXSeparatorItem;
    actFindReferences: TAction;
    TBXItem38: TTBXItem;
    btnNext: TTBXSubmenuItem;
    btnPrevious: TTBXSubmenuItem;
    TBXSeparatorItem14: TTBXSeparatorItem;
    PreviousList: TTBXStringList;
    NextList: TTBXStringList;
    actBrowseBack: TAction;
    actBrowseForward: TAction;
    TBXItem39: TTBXItem;
    TBXItem40: TTBXItem;
    TBXSeparatorItem15: TTBXSeparatorItem;
    TBXItem41: TTBXItem;
    actViewRegExpTester: TAction;
    TBXItem42: TTBXItem;
    actCommandLine: TAction;
    TBXItem43: TTBXItem;
    TBXItem44: TTBXItem;
    TBXItem45: TTBXItem;
    actViewUnitTests: TAction;
    TBXItem46: TTBXItem;
    TBXSeparatorItem16: TTBXSeparatorItem;
    mnLayouts: TTBXSubmenuItem;
    mnLayOutSeparator: TTBXSeparatorItem;
    TBXSubmenuItem1: TTBXSubmenuItem;
    actLayoutSave: TAction;
    actLayoutsDelete: TAction;
    actLayoutDebug: TAction;
    TBXItem47: TTBXItem;
    TBXItem48: TTBXItem;
    TBXItem49: TTBXItem;
    mnMaximizeEditor: TTBXItem;
    TBXSeparatorItem17: TTBXSeparatorItem;
    actMaximizeEditor: TAction;
    TBXSeparatorItem18: TTBXSeparatorItem;
    TBXSubmenuItem4: TTBXSubmenuItem;
    TBXSeparatorItem19: TTBXSeparatorItem;
    mnNoSyntax: TTBXItem;
    actEditorZoomIn: TAction;
    actEditorZoomOut: TAction;
    TBXSeparatorItem20: TTBXSeparatorItem;
    TBXSeparatorItem21: TTBXSeparatorItem;
    mnSyntax: TTBXSubmenuItem;
    TBXItem50: TTBXItem;
    TBXItem51: TTBXItem;
    TBXSeparatorItem22: TTBXSeparatorItem;
    mnFiles: TTBXSubmenuItem;
    actAddWatchAtCursor: TAction;
    TBXItem52: TTBXItem;
    RunningProcessesPopUpMenu: TTBXPopupMenu;
    TBXItem29: TTBXItem;
    TBXSubmenuItem5: TTBXSubmenuItem;
    TBXSeparatorItem23: TTBXSeparatorItem;
    actNewFile: TAction;
    TBXItem53: TTBXItem;
    JvAppInstances: TJvAppInstances;
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
    procedure actViewMainToolBarExecute(Sender: TObject);
    procedure actViewDebugToolbarExecute(Sender: TObject);
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
    procedure JvFormStorageSavePlacement(Sender: TObject);
    procedure JvFormStorageRestorePlacement(Sender: TObject);
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
    procedure ExternalToolsUpdate(Sender: TObject);
    procedure actViewOutputExecute(Sender: TObject);
    procedure actExternalRunExecute(Sender: TObject);
    procedure actExternalRunConfigureExecute(Sender: TObject);
    procedure TBXMRUListClick(Sender: TObject; const Filename: String);
    procedure actFindDefinitionExecute(Sender: TObject);
    procedure actFindReferencesExecute(Sender: TObject);
    procedure PreviousListClick(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
    procedure NextListClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    function ApplicationHelp(Command: Word; Data: Integer;
      var CallHelp: Boolean): Boolean;
    procedure FormShow(Sender: TObject);
    procedure actAddWatchAtCursorExecute(Sender: TObject);
    procedure actNewFileExecute(Sender: TObject);
  protected
    fCurrentLine : integer;
    fErrorLine : integer;
    fRefactoring : TBRMRefactor;
    fCurrentBrowseInfo : string;
    function DoCreateEditor: IEditor;
    function CmdLineOpenFiles(AMultipleFiles: boolean): boolean;
    procedure DebuggerBreakpointChange(Sender: TObject; Editor : IEditor; ALine: integer);
    procedure SetCurrentPos(Editor : IEditor; ALine: integer);
    procedure DebuggerCurrentPosChange(Sender: TObject);
    procedure DebuggerYield(Sender: TObject; DoIdle : Boolean);
    procedure UpdateStandardActions;
    procedure UpdateStatusBarPanels;
    procedure ApplicationOnIdle(Sender: TObject; var Done: Boolean);
    procedure ApplicationOnHint(Sender: TObject);
    procedure ApplcationOnShowHint(var HintStr: string; var CanShow: Boolean;
      var HintInfo: THintInfo);
    procedure WMFindDefinition(var Msg: TMessage); message WM_FINDDEFINITION;
    procedure WMUpdateBreakPoints(var Msg: TMessage); message WM_UPDATEBREAKPOINTS;
    procedure WMCheckForUpdates(var Msg: TMessage); message WM_CHECKFORUPDATES;
    procedure TBMThemeChange(var Message: TMessage); message TBM_THEMECHANGE;
    procedure TBXThemeMenuOnClick(Sender: TObject);
    procedure SyntaxClick(Sender : TObject);
    procedure SelectEditor(Sender : TObject);
  public
    PyDebugger : TPyBaseDebugger;
    PythonKeywordHelpRequested : Boolean;
    MenuHelpRequested : Boolean;
    ActionListArray : TActionListArray;
    Layouts : TStringList;
    function DoOpenFile(AFileName: string; HighlighterName : string = '') : IEditor;
    function NewFileFromTemplate(FileTemplate : TFileTemplate) : IEditor;
    function GetActiveEditor : IEditor;
    procedure SaveFileModules;
    procedure UpdateDebugCommands(DebuggerState : TDebuggerState);
    function ShowFilePosition(FileName : string; Line, Offset : integer;
         ForceToMiddle : boolean = True) : boolean;
    procedure DebuggerStateChange(Sender: TObject; OldState,
      NewState: TDebuggerState);
    procedure DebuggerErrorPosChange(Sender: TObject);
    procedure PyIDEOptionsChanged;
    procedure SetupToolsMenu;
    procedure SetupLayoutsMenu;
    procedure SetupSyntaxMenu;
    procedure RunExternalTool(Sender : TObject);
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

var
  PyIDEMainForm: TPyIDEMainForm;

implementation

uses
  frmPythonII, frmMessages, PythonEngine, frmEditor,
  frmCallStack, frmBreakPoints, frmVariables, frmWatches,
  frmCodeExplorer, frmFileExplorer, JclFileUtils, frmToDo,
  frmFindResults, cFindInFiles, uParams, cTools, cParameters,
  frmCommandOutput, JvCreateProcess, dlgToolProperties, uCommonFunctions,
  TBXThemes, SynHighlighterPython, SynEditHighlighter, VarPyth, SynRegExpr,
  JvJVCLUtils, DateUtils, cPythonSourceScanner, frmRegExpTester,
  StringResources, dlgCommandLine, frmUnitTests, cFilePersist, frmIDEDockWin,
  dlgPickList, VirtualTrees, VirtualExplorerTree, JvDockGlobals, Math,
  cCodeHint, dlgNewFile;

{$R *.DFM}

{ TWorkbookMainForm }

function TPyIDEMainForm.DoCreateEditor: IEditor;
begin
  if GI_EditorFactory <> nil then begin
    Result := GI_EditorFactory.CreateTabSheet(EditorsPageList);
    Result.SynEdit.Assign(CommandsDataModule.EditorOptions);
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

procedure TPyIDEMainForm.FormCreate(Sender: TObject);
Var
  TabHost : TJvDockTabHostForm;
begin
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

  AddThemeNotification(Self);

  Layouts := TStringList.Create;
  Layouts.Sorted := True;
  Layouts.Duplicates := dupError;

  // ActionLists
  SetLength(ActionListArray, 2);
  ActionListArray[0] := actlStandard;
  ActionListArray[1] := CommandsDataModule.actlMain;

  // Application Storage
  if FileExists(ChangeFileExt(Application.ExeName, '.ini')) then
    AppStorage.Location := flExeFile
  else
    AppStorage.Location := flUserFolder;
  AppStorage.FileName := ChangeFileExt(ExtractFileName(Application.ExeName), '.ini');
  AppStorage.StorageOptions.StoreDefaultValues := False;  

  // Create and layout debugger windows

  PythonIIForm := TPythonIIForm.Create(self);
  CallStackWindow := TCallStackWindow.Create(Self);
  VariablesWindow := TVariablesWindow.Create(Self);
  WatchesWindow := TWatchesWindow.Create(Self);
  BreakPointsWindow := TBreakPointsWindow.Create(Self);
  OutputWindow := TOutputWindow.Create(Self);
  MessagesWindow := TMessagesWindow.Create(Self);
  CodeExplorerWindow := TCodeExplorerWindow.Create(Self);
  FileExplorerWindow := TFileExplorerWindow.Create(Self);
  ToDoWindow := TToDoWindow.Create(Self);
  RegExpTesterWindow := TRegExpTesterWindow.Create(Self);
  UnitTestWindow := TUnitTestWindow.Create(Self);
  // FindInFilesExpert creates FindResultsWindow
  FindInFilesExpert := TFindInFilesExpert.Create;

  // Create Debugger
  PyDebugger := TPyDebugger.Create;
  // Assign Debugger Events
  with PyDebugger do begin
    OnBreakpointChange := DebuggerBreakpointChange;
    OnCurrentPosChange := DebuggerCurrentPosChange;
    OnErrorPosChange := DebuggerErrorPosChange;
    OnStateChange := DebuggerStateChange;
    OnYield := DebuggerYield;
  end;

  // Note that the following will trigger the AppStorage to RestoreFormPlacement etc.
  // Otherwise this would have happened after exiting FormCreate when the Form is shown.
  AppStorage.ReadStringList('Layouts', Layouts, True);

  if AppStorage.PathExists('Layouts\Current\Forms') then
    LoadDockTreeFromAppStorage(AppStorage, 'Layouts\Current')
  else begin
    TBXSwitcher.Theme := 'Office2003';
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

  UpdateDebugCommands(PyDebugger.DebuggerState);
  //  Editor Views Menu
  GI_EditorFactory.SetupEditorViewMenu;

  Update;
  SendMessage(EditorsPageList.Handle, WM_SETREDRAW, 0, 0);  // To avoid flicker
  try
    // Open Files on the command line
    CmdLineOpenFiles(True);

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
//Var
//  i : integer;
begin
  if JvGlobalDockIsLoading then begin
    CanClose := False;
    CloseTimer.Enabled := True;
    Exit;
  end else if PyDebugger.DebuggerState <> dsInactive then begin
    CanClose := False;
    if Windows.MessageBox(Handle,
      'A debugging session is in process.  Do you want to abort the session and Exit?',
       PChar(Application.Title), MB_ICONWARNING or MB_YESNO) = idYes then
    begin
//    if (MessageDlg('A debugging session is in process.  Do you want to abort the session and Exit?',
//      mtWarning, [mbYes, mbNo], 0) = mrYes) then begin
      PyDebugger.Abort;
      CloseTimer.Enabled := True;
    end;
    Exit;
  end;

  if OutputWindow.JvCreateProcess.State <> psReady then
    if  MessageDlg('An External Tool is Still running.  Do you want to terminate it and exit?',
        mtConfirmation, [mbYes, mbCancel], 0) = mrYes
    then begin
      if OutputWindow.JvCreateProcess.State <> psReady then  // check again
        OutputWindow.JvCreateProcess.Terminate;
      CanClose := True;
    end else
      CanClose := False;

  if GI_EditorFactory <> nil then
    CanClose := GI_EditorFactory.CanCloseAll;

  if CanClose then begin
    // Shut down CodeExplorerWindow Worker thread
    CodeExplorerWindow.ShutDownWorkerThread;

    // Disconnect ChangeNotify
    CommandsDataModule.JvChangeNotify.OnChangeNotify := nil;
    CommandsDataModule.JvChangeNotify.Active := False;

    // Close FileExplorer ChangeNotify Thread
    FileExplorerWindow.FileExplorerTree.TreeOptions.VETMiscOptions :=
      FileExplorerWindow.FileExplorerTree.TreeOptions.VETMiscOptions
          - [toChangeNotifierThread];

    // Disable CodeHint timer
    CodeHint.CancelHint;

    // Give the time to the treads to terminate
    Sleep(200);

    VariablesWindow.ClearAll;
    UnitTestWindow.ClearAll;
    CallStackWindow.ClearAll;

    // Save the list of open files
    AppStorage.DeleteSubTree('Open Files');
    TPersistFileInfo.WriteToAppStorage(AppStorage, 'Open Files');

    //  We need to do these here so that MRU and docking information are persisted
    SaveLayout('Current');
    SendMessage(EditorsPageList.Handle, WM_SETREDRAW, 0, 0);  // To avoid flicker

    if GI_EditorFactory <> nil then
      GI_EditorFactory.CloseAll;
    SendMessage(EditorsPageList.Handle, WM_SETREDRAW, 1, 0);

    RemoveThemeNotification(Self);
    // Shut down help
    Application.HelpCommand(HELP_QUIT, 0);
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
  Handled := False;
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
  if TabBar.Tabs.Count = 0 then Exit;
  if Assigned(TabBar.SelectedTab) then
    TabBarItem := TabBar.SelectedTab.GetNextVisible
  else
    TabBarItem := TabBar.Tabs.Items[0];
  if not Assigned(TabBarItem) then
    TabBarItem := TabBar.Tabs.Items[0];
  if Assigned(TabBarItem) then
    TabBarItem.Selected := True;
end;

procedure TPyIDEMainForm.actPreviousEditorExecute(Sender: TObject);
Var
  TabBarItem : TJvTabBarItem;
begin
  if TabBar.Tabs.Count = 0 then Exit;
  if Assigned(TabBar.SelectedTab) then
    TabBarItem := TabBar.SelectedTab.GetPreviousVisible
  else
    TabBarItem := TabBar.Tabs.Items[TabBar.Tabs.Count-1];
  if not Assigned(TabBarItem) then
    TabBarItem := TabBar.Tabs.Items[TabBar.Tabs.Count-1];
  if Assigned(TabBarItem) then
    TabBarItem.Selected := True;
end;

procedure TPyIDEMainForm.actSyntaxCheckExecute(Sender: TObject);
var
  ActiveEditor : IEditor;
begin
  ActiveEditor := GetActiveEditor;
  if not Assigned(ActiveEditor) then Exit;

  if PyDebugger.SyntaxCheck(ActiveEditor) then begin
    MessagesWindow.AddMessage(Format('Syntax of %s is OK!', [ActiveEditor.FileTitle]));
    ShowDockForm(MessagesWindow);
  end;
end;

procedure TPyIDEMainForm.actImportModuleExecute(Sender: TObject);
var
  ActiveEditor : IEditor;
  ModuleName : string;
begin
  ActiveEditor := GetActiveEditor;
  if not Assigned(ActiveEditor) then Exit;

  PyDebugger.ImportModule(ActiveEditor);

  ModuleName := PathRemoveExtension(ActiveEditor.FileTitle);
  // add Module name to the locals() of the interpreter
  GetPythonEngine.ExecString('import ' + ModuleName);

  MessagesWindow.AddMessage(Format('Module %s was imported successfully!', [ActiveEditor.FileTitle]));
  ShowDockForm(MessagesWindow);
end;

procedure TPyIDEMainForm.actToggleBreakPointExecute(Sender: TObject);
var
  ActiveEditor : IEditor;
begin
  ActiveEditor := GetActiveEditor;
  if Assigned(ActiveEditor) and ActiveEditor.HasPythonFile then
    PyDebugger.ToggleBreakpoint(ActiveEditor, ActiveEditor.SynEdit.CaretY);
end;

procedure TPyIDEMainForm.actClearAllBreakpointsExecute(Sender: TObject);
begin
  PyDebugger.ClearAllBreakpoints;
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

  PyDebugger.RunNoDebug(ActiveEditor);

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

  PyDebugger.Run(ActiveEditor);
end;

procedure TPyIDEMainForm.actStepIntoExecute(Sender: TObject);
var
  Editor : IEditor;
begin
  Editor := GetActiveEditor;
  if Assigned(Editor) then
    PyDebugger.StepInto(Editor);
end;

procedure TPyIDEMainForm.actStepOverExecute(Sender: TObject);
begin
  PyDebugger.StepOver;
end;

procedure TPyIDEMainForm.actStepOutExecute(Sender: TObject);
begin
  PyDebugger.StepOut;
end;

procedure TPyIDEMainForm.actDebugAbortExecute(Sender: TObject);
begin
  PyDebugger.Abort;
end;

procedure TPyIDEMainForm.actRunToCursorExecute(Sender: TObject);
var
  Editor : IEditor;
begin
  Editor := GetActiveEditor;
  PyDebugger.RunToCursor(Editor, Editor.SynEdit.CaretY);
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
  actDebug.Enabled := not (DebuggerState in [dsRunning, dsRunningNoDebug])
                      and PyFileActive;
  actStepInto.Enabled := (DebuggerState = dsPaused) or
                         (PyFileActive and (DebuggerState = dsInactive));
  actStepOut.Enabled := DebuggerState = dsPaused;
  actStepOver.Enabled := DebuggerState = dsPaused;
  actDebugAbort.Enabled := DebuggerState in [dsPaused, dsRunning];
  actRunToCursor.Enabled := (not (DebuggerState in [dsRunning, dsRunningNoDebug])) and
    PyFileActive and PyDebugger.IsExecutableLine(Editor, Editor.SynEdit.CaretY);
  actToggleBreakPoint.Enabled := PyFileActive;
  actClearAllBreakPoints.Enabled := PyFileActive;
  actAddWatchAtCursor.Enabled := PyFileActive;
end;

procedure TPyIDEMainForm.SetCurrentPos(Editor : IEditor; ALine: integer);
Var
  ActiveEditor : IEditor;
begin
  ActiveEditor := GetActiveEditor;
  if not Assigned(ActiveEditor) then Exit;  //No editors!

  if (not Assigned(Editor) or (ActiveEditor = Editor)) and (fCurrentLine > 0) then
    // Remove possible current lines
    with ActiveEditor.SynEdit do begin
      InvalidateGutterLine(fCurrentLine);
      InvalidateLine(fCurrentLine);
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
end;

procedure TPyIDEMainForm.DebuggerCurrentPosChange(Sender: TObject);
begin
  if (PyDebugger <> nil) and not PyDebugger.IsRunning then
    SetCurrentPos(PyDebugger.CurrentPos.Editor , PyDebugger.CurrentPos.Line)
  else
    SetCurrentPos(PyDebugger.CurrentPos.Editor, -1);
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

  if (not Assigned(PyDebugger.ErrorPos.Editor) or (PyDebugger.ErrorPos.Editor = Editor)) and
    (fErrorLine > 0)
  then
    // Remove possible error line
    Editor.SynEdit.InvalidateLine(fErrorLine);

  fErrorLine := PyDebugger.ErrorPos.Line;  // Store
  if (Editor = PyDebugger.ErrorPos.Editor) and (PyDebugger.ErrorPos.Line > 0) then
    Editor.SynEdit.InvalidateLine(PyDebugger.ErrorPos.Line);
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
                        Screen.Cursor := crHourGlass;
                        StatusLED.ColorOn := clRed;
                      end;
    dsPaused: begin
                s := 'Paused';
                Screen.Cursor := crDefault;
                StatusLED.ColorOn := clRed;
                StatusLED.ColorOn := clYellow;
              end;
    dsInactive: begin
                 s := 'Ready';
                 Screen.Cursor := crDefault;
                 StatusLED.ColorOn := clLime;
               end;
  end;
  StatusLED.Hint := s;
  StatusBar.Panels[0].Caption := ' ' + s;
  StatusBar.Refresh;

  CallStackWindow.UpdateWindow(NewState);
  WatchesWindow.UpdateWindow(NewState);
  UpdateDebugCommands(NewState);
end;

procedure TPyIDEMainForm.DebuggerYield(Sender: TObject; DoIdle : Boolean);
begin
  Application.ProcessMessages;
  // HandleMessage calls Application.Idle which yields control to other applications
  if DoIdle then
    Application.HandleMessage;
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
  UpdateDebugCommands(PyDebugger.DebuggerState);
  for i := 0 to EditorsPageList.PageCount - 1 do
    if i < TabBar.Tabs.Count then
      TabBar.Tabs[i].Modified :=
        TEditorForm(TJvStandardPage(TabBar.Tabs[i].Data).Components[0]).GetEditor.Modified;
  for i := 0 to EditorViewsMenu.Count - 1 do
    EditorViewsMenu.Items[i].Enabled := Assigned(GI_ActiveEditor);
  if Assigned(GI_ActiveEditor) then
    TEditorForm(GI_ActiveEditor.Form).DoOnIdle;
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

procedure TPyIDEMainForm.actViewMainToolBarExecute(Sender: TObject);
begin
  MainToolBar.Visible := not MainToolBar.Visible;
end;

procedure TPyIDEMainForm.actViewDebugToolbarExecute(Sender: TObject);
begin
  DebugToolBar.Visible := not DebugToolBar.Visible;
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

procedure TPyIDEMainForm.actViewCodeExplorerExecute(Sender: TObject);
begin
  if not CodeExplorerWindow.Visible then
    ShowDockForm(CodeExplorerWindow)
  else
    HideDockForm(CodeExplorerWindow);
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
  if Lo(GetKeyState(VK_CAPITAL)) = 1 then
    StatusBar.Panels[4].Caption := 'CAPS'
  else
    StatusBar.Panels[4].Caption := '';

  ExternalToolsLED.Visible := OutputWindow.JvCreateProcess.State <> psReady;
end;

function TPyIDEMainForm.CmdLineOpenFiles(AMultipleFiles: boolean): boolean;
var
  i, Cnt: integer;
begin
  Cnt := ParamCount;
  if Cnt > 0 then begin
    if not AMultipleFiles and (Cnt > 1) then
      Cnt := 1;
    for i := 1 to Cnt do
      if (ParamStr(i)[1] <> '-') and FileExists(ParamStr(i)) then
        DoOpenFile(ParamStr(i));
    Result := TRUE;
  end else
    Result := FALSE;
end;

procedure TPyIDEMainForm.FormDestroy(Sender: TObject);
begin
  PyDebugger.Free;
  FindInFilesExpert.Free;
  fRefactoring.Free;
  Layouts.Free;
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
begin
  with CommandsDataModule.dlgFileOpen do begin
    Title := 'Open File';
    Filter := GetHighlightersFilter(CommandsDataModule.Highlighters) + SFilterAllFiles;
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
begin
  PythonIIForm.II.debugger.CleanupMaindict :=
    CommandsDataModule.PyIDEOptions.CleanupMainDict;
  PythonIIForm.II.debugger.CleanupSysModules :=
    CommandsDataModule.PyIDEOptions.CleanupSysModules;
  FileExplorerWindow.FileExplorerTree.RefreshTree;
  CommandsDataModule.EditorSearchOptions.SearchTextAtCaret :=
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
  Editor := GetActiveEditor;
  if Assigned(Editor) then
    Editor.SynEdit.InvalidateGutter;
end;

procedure TPyIDEMainForm.JvFormStorageSavePlacement(Sender: TObject);
Var
  TempStringList : TStringList;
  ActionProxyCollection : TActionProxyCollection;
  i : integer;
begin
  TempStringList := TStringList.Create;
  AppStorage.BeginUpdate;
  try
    AppStorage.WriteString('PyScripter Version', ApplicationVersion);
    AppStorage.WritePersistent('IDE Options', CommandsDataModule.PyIDEOptions);
    with CommandsDataModule do begin
      AppStorage.DeleteSubTree('Editor Options');
      AppStorage.WritePersistent('Editor Options', EditorOptions);
      TempStringList.Add('KeyStrokes');
      AppStorage.WritePersistent('Interpreter Editor Options',
        InterpreterEditorOptions, True, TempStringList);

      AppStorage.WritePersistent('Editor Search Options', EditorSearchOptions);

      TempStringList.Clear;
      TempStringList.Add('Lines');
      TempStringList.Add('Highlighter');
      AppStorage.WritePersistent('Print Options', SynEditPrint, True, TempStringList);

      AppStorage.StorageOptions.PreserveLeadingTrailingBlanks := True;
      AppStorage.DeleteSubTree('File Templates');
      AppStorage.WriteObjectList('File Templates', FileTemplates);

      TempStringList.Assign(CodeTemplatesCompletion.AutoCompleteList);
      AppStorage.WriteStringList('Code Templates', TempStringList);
      AppStorage.StorageOptions.PreserveLeadingTrailingBlanks := False;

      AppStorage.DeleteSubTree('Highlighters');
      for i := 0 to Highlighters.Count - 1 do
        AppStorage.WritePersistent('Highlighters\'+Highlighters[i], TPersistent(Highlighters.Objects[i]));
    end;
    AppStorage.WritePersistent('ToDo Options', ToDoExpert);
    AppStorage.WritePersistent('Find in Files Options', FindInFilesExpert);
    AppStorage.WritePersistent('Find in Files Results Options', FindResultsWindow);
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
    // Store Toolbar positions
    TBIniSavePositions(Self, AppStorage.FileName, 'Toolbars');
  finally
    AppStorage.EndUpdate;
    TempStringList.Free;
  end;
  // Save MRU Lists
  TBXMRUList.SaveToIni(AppStorage.IniFile, 'MRU File List');
  CommandsDataModule.CommandLineMRU.SaveToIni(AppStorage.IniFile, 'CommandLine MRU');
end;

procedure TPyIDEMainForm.JvFormStorageRestorePlacement(Sender: TObject);
Var
  ActionProxyCollection : TActionProxyCollection;
  TempStringList : TStringList;
  i : integer;
begin
  if AppStorage.IniFile.SectionExists('IDE Options') then begin
    AppStorage.ReadPersistent('IDE Options', CommandsDataModule.PyIDEOptions);
    PyIDEOptionsChanged;
  end;
  if AppStorage.IniFile.SectionExists('Editor Options') then
    with CommandsDataModule do begin
      AppStorage.ReadPersistent('Editor Options', EditorOptions);
      CommandsDataModule.ApplyEditorOptions;

      if AppStorage.IniFile.SectionExists('Interpreter Editor Options') then begin
        AppStorage.ReadPersistent('Interpreter Editor Options', InterpreterEditorOptions);
        PythonIIForm.SynEdit.Assign(InterpreterEditorOptions);
        PythonIIForm.RegisterHistoryCommands;
      end;

      if AppStorage.IniFile.SectionExists('Editor Search Options') then
        AppStorage.ReadPersistent('Editor Search Options', EditorSearchOptions);

      AppStorage.ReadPersistent('Print Options', SynEditPrint);

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

      for i := 0 to Highlighters.Count - 1 do
        AppStorage.ReadPersistent('Highlighters\'+Highlighters[i], TPersistent(Highlighters.Objects[i]));
    end;
  if AppStorage.IniFile.SectionExists('ToDo Options') then
    AppStorage.ReadPersistent('ToDo Options', ToDoExpert);
  AppStorage.ReadPersistent('Find in Files Options', FindInFilesExpert);
  AppStorage.ReadPersistent('Find in Files Results Options', FindResultsWindow);
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
  // Load Theme Name
  TBXSwitcher.Theme := AppStorage.ReadString('Theme Name', 'Office2003');
  // Load MRU Lists
  TBXMRUList.LoadFromIni(AppStorage.IniFile, 'MRU File List');
  CommandsDataModule.CommandLineMRU.LoadFromIni(AppStorage.IniFile, 'CommandLine MRU');

  ActionProxyCollection := TActionProxyCollection.Create(ActionListArray);
  try
    AppStorage.ReadCollection('IDE Shortcuts', ActionProxyCollection, True, 'Action');
    ActionProxyCollection.ApplyShortCuts(ActionListArray);
  finally
    ActionProxyCollection.Free;
  end;
  // Restore Toolbar positions
  TBIniLoadPositions(Self, AppStorage.FileName, 'Toolbars');
end;

procedure TPyIDEMainForm.TabBarTabSelected(Sender: TObject;
  Item: TJvTabBarItem);
Var
  WinControl : TWinControl;
begin
  if Assigned(Item) and not (csDestroying in ComponentState) then begin
    EditorsPageList.ActivePage := Item.Data as TJvStandardPage;
    if Assigned(EditorsPageList.ActivePage) then begin
      WinControl := (EditorsPageList.ActivePage.Controls[0] as
        TEditorForm).EditorViews.ActivePage.Controls[0] as TWinControl;
      if WinControl.CanFocus then
        ActiveControl := WinControl;
    end;
  end;
end;

procedure TPyIDEMainForm.TabBarTabClosed(Sender: TObject;
  Item: TJvTabBarItem);
begin
  if Assigned(GetActiveEditor()) then
    with GetActiveEditor do begin
      if AskSaveChanges then Close;
   end;
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
  actWatchesWin.Checked := WatchesWindow.Visible;

  actViewDebugToolBar.Checked := DebugToolBar.Visible;
  actViewMainToolBar.Checked := MainToolBar.Visible;
  actViewStatusbar.Checked := StatusBar.Visible;

  actFileNewModule.Enabled := GI_EditorFactory <> nil;
  actFileOpen.Enabled := GI_EditorFactory <> nil;
  actFileCloseAll.Enabled := (GI_EditorFactory <> nil)
    and (GI_EditorFactory.GetEditorCount > 0);
  ExternalToolsUpdate(nil);

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
  i, Index : integer;
  MenuItem : TTBXItem;
  Action : TAction;
  Tool : TExternalTool;
  AppFile, S : string;
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
      MenuItem := TTBXItem.Create(Self);
      Action := TAction.Create(Self);
      Action.Caption := Tool.Caption;
      S := StringReplace(Action.Caption, ' ', '', [rfReplaceAll]);
      S := StringReplace(S, '&', '', [rfReplaceAll]);
      if IsValidIdent(S) then
        Action.Name := S;
      Action.Category := 'External Tools';
      Action.ActionList := actlStandard;
      Action.ShortCut := Tool.ShortCut;
      Action.Hint := Tool.Description;
      Action.OnExecute := RunExternalTool;
      if (Tool.ApplicationName <> '') then begin
        AppFile := PrepareCommandLine(Tool.ApplicationName);
        if FileExists(AppFile) then begin
          Index := GetIconIndexFromFile(AppFile, True);
          CommandsDataModule.Images.AddImage(CommandsDataModule.imlShellIcon, Index);
          Action.ImageIndex := CommandsDataModule.Images.Count - 1;
        end;
      end;
      Action.Tag := Integer(Tool);
      mnTools.Add(MenuItem);
      MenuItem.Action := Action;
      MenuItem.Tag := Integer(Tool);
    end;
  end;
end;

procedure TPyIDEMainForm.SetupLayoutsMenu;
Var
  i : integer;
  MenuItem : TTBXItem;
begin
  // delete previous Layouts
  while mnLayouts.Items[0] <> mnLayOutSeparator do
    mnLayouts.Items[0].Free;

  for i := Layouts.Count - 1 downto 0 do begin
    MenuItem := TTBXItem.Create(Self);
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
    TEditorForm(Editor.Form).DefaultExtension := '';
  end;
end;

procedure TPyIDEMainForm.mnNoSyntaxClick(Sender: TObject);
Var
  Editor : IEditor;
begin
  Editor := GetActiveEditor;
  if Assigned(Editor) then
    Editor.SynEdit.Highlighter := nil;
end;

procedure TPyIDEMainForm.SetupSyntaxMenu;
Var
  i : integer;
  MenuItem : TTBXItem;
begin
  for i := CommandsDataModule.Highlighters.Count - 1 downto 0 do begin
    MenuItem := TTBXItem.Create(Self);
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
begin
  Path := 'Layouts\'+ Layout;
  if AppStorage.PathExists(Path + '\Forms') then begin
    TempCursor := WaitCursor;
    SaveActiveControl := ActiveControl;
    SendMessage(EditorsPageList.Handle, WM_SETREDRAW, 0, 0);
    // To avoid delays in creating and destroying FileExplorer's Change Notify thread     
    FileExplorerWindow.FileExplorerTree.TreeOptions.VETMiscOptions :=
      FileExplorerWindow.FileExplorerTree.TreeOptions.VETMiscOptions - [toChangeNotifierThread];

    try
      for i := 0 to EditorsPageList.PageCount - 1 do begin
        TEditorForm(EditorsPageList.Pages[i].Components[0]).Visible := False;
      end;
      LoadDockTreeFromAppStorage(AppStorage, Path);
    finally
      SendMessage(EditorsPageList.Handle, WM_SETREDRAW, 1, 0);
      for i := 0 to EditorsPageList.PageCount - 1 do begin
        TEditorForm(EditorsPageList.Pages[i].Components[0]).Parent := nil;
        TEditorForm(EditorsPageList.Pages[i].Components[0]).Parent := EditorsPageList.Pages[i];
        TEditorForm(EditorsPageList.Pages[i].Components[0]).Visible := True;
      end;
      EditorsPageList.Invalidate;
      SendMessage(EditorsPageList.Handle, WM_SETREDRAW, 1, 0);
      for i := 0 to Screen.FormCount - 1 do
        if Screen.Forms[i] is TIDEDockWindow then
          TIDEDockWindow(Screen.Forms[i]).FGPanelExit(Self);
      FileExplorerWindow.FileExplorerTree.TreeOptions.VETMiscOptions :=
        FileExplorerWindow.FileExplorerTree.TreeOptions.VETMiscOptions + [toChangeNotifierThread];
    end;
    if Assigned(SaveActiveControl) and SaveActiveControl.Visible
      and SaveActiveControl.CanFocus
    then
      try
        ActiveControl := SaveActiveControl;
      except
      end;
  end;
end;

procedure TPyIDEMainForm.SaveLayout(Layout: string);
begin
  Appstorage.DeleteSubTree('Layouts\'+Layout);
  SaveDockTreeToAppStorage(AppStorage, 'Layouts\'+ Layout);
end;

procedure TPyIDEMainForm.LayoutClick(Sender: TObject);
begin
  LoadLayout(TTBXItem(Sender).Caption);
  TTBXItem(Sender).Checked := True;
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
  with TPickListDialog.Create(Self) do begin
    Caption := 'Delete Layouts';
    lbMessage.Caption := 'Select layouts to delete:';
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

procedure TPyIDEMainForm.ExternalToolsUpdate(Sender: TObject);
Var
  i : integer;
begin
  for i := 0 to mnTools.Count - 1 do begin
    if not Assigned(mnTools.Items[i].OnClick) then
      raise Exception.Create('Internal Error: Tools');
    case TExternalTool(mnTools.Items[i].Tag).Context of
      tcAlwaysEnabled : TAction(mnTools.Items[i].Action).Enabled := True;
      tcActiveFile : TAction(mnTools.Items[i].Action).Enabled := Assigned(GI_ActiveEditor);
      tcSelectionAvailable : TAction(mnTools.Items[i].Action).Enabled :=
        Assigned(GI_ActiveEditor) and GI_ActiveEditor.SynEdit.SelAvail;
    end;
  end;
end;

procedure TPyIDEMainForm.RunExternalTool(Sender: TObject);
begin
   if Sender is TAction then begin
     OutputWindow.ExecuteTool(TExternalTool(TAction(Sender).Tag));
   end;
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
    FindDefinition(GI_ActiveEditor, GI_ActiveEditor.SynEdit.CaretXY, True,
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
              MessageDlg('This is the definition of "'+ Token + '"',
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
        MessageDlg('Please place the cursor on a function or a class name.',
          mtError, [mbOK], 0);
      end;
    end;
  end;
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
  with GI_ActiveEditor.SynEdit do begin
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
        MessageDlg('Please place the cursor on a function/class name or identifier',
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

procedure TPyIDEMainForm.btnPreviousClick(Sender: TObject);
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
    Result.SynEdit.Text := Parameters.ReplaceInText(FileTemplate.Template);
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

procedure TPyIDEMainForm.btnNextClick(Sender: TObject);
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
  VariablesWindow.UpdateWindow;
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
  TBXSetTheme(TTBXItem(Sender).Caption);
  TTBXItem(Sender).Checked := True;
end;

procedure TPyIDEMainForm.FillTBXThemeMenu;
const
  ThemeHint = 'Set theme to "%s".';
var
  i: Integer;
  sl: TStringList;
  x: TTBXItem;
begin
  mnThemes.Clear;
  sl := TStringList.Create;
  try
    TBXThemes.GetAvailableTBXThemes(sl);
    sl.Sorted := True;
    for i := 0 to Pred(sl.Count) do
    begin
      x := TTBXItem.Create(Self);
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

procedure TPyIDEMainForm.actMaximizeEditorExecute(Sender: TObject);
var
  i : TJvDockPosition;
  j : integer;
  Panel : TJvDockPanel;
begin
  for i := Low(TJvDockPosition) to High(TJvDockPosition) do begin
    Panel := DockServer.DockPanel[i];
    if not (Panel is TJvDockVSNETPanel) then continue;
    for j := 0 to Panel.DockClientCount - 1 do
      //  for some reason DoAutoHideControl changes DockClientCount
      if j < Panel.DockClientCount then
        TJvDockVSNETPanel(Panel).DoAutoHideControl(Panel.DockClients[j]as TWinControl)
      else
        break;
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
  MenuItem : TTBXItem;
begin
  mnFiles.Clear;
  ActiveEditor := GetActiveEditor;
  List := TStringList.Create;
  List.Duplicates := dupAccept;
  List.Sorted := True;
  try
    for i := 0 to GI_EditorFactory.Count - 1 do
      List.AddObject(GI_EditorFactory.Editor[i].FileTitle,
        TObject(GI_EditorFactory.Editor[i].Form));
    for i:= 0 to List.Count - 1 do begin
      Editor := TEditorForm(List.Objects[i]).GetEditor;
      MenuItem := TTBXItem.Create(Self);
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

procedure TPyIDEMainForm.SelectEditor(Sender: TObject);
begin
    DoOpenFile((Sender as TTBCustomItem).Hint);
end;

end.








