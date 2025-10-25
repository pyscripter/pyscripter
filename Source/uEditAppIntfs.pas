{-----------------------------------------------------------------------------
 Unit Name: uEditAppIntfs
 Author:    Kiriakos Vlahos
 Date:      09-Mar-2005
 Purpose:   Editor and IDE interfaces
 History:
-----------------------------------------------------------------------------}

unit uEditAppIntfs;

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.Contnrs,
  Vcl.Forms,
  Vcl.ImgList,
  JvAppStorage,
  JclSysUtils,
  PythonEngine,
  PythonVersions,
  SynEdit,
  SynEditMiscClasses,
  SpTBXItem,
  FileSystemMonitor;

type

  IEditor = interface;

  IEditorViewFactory = interface
  ['{680F6C4E-5EED-4684-A199-5A62E644D81B}']
    function CreateForm(Editor: IEditor; AOwner: TComponent): TCustomForm;
    function GetName: string;
    function GetTabCaption: string;
    function GetMenuCaption: string;
    function GetHint: string;
    function GetImageName: string;
    function GetShortCut: TShortCut;
    procedure GetContextHighlighters(List: TList);
    property Name: string read GetName;
    property TabCaption: string read GetTabCaption;
    property MenuCaption: string read GetMenuCaption;
    property Hint: string read GetHint;
    property ImageName: string read GetImageName;
    property ShortCut: TShortCut read GetShortCut;
  end;

  IEditorView = interface
  ['{E68438C1-CE7C-4831-A995-5E72F01AEFEC}']
    procedure UpdateView(Editor: IEditor);
  end;

  TFileSaveFormat = (sf_Ansi, sf_UTF8, sf_UTF8_NoBOM, sf_UTF16LE, sf_UTF16BE);

  IEditor = interface
  ['{15E8BD28-6E18-4D49-8499-1DB594AB88F7}']
    procedure Activate(Primary: Boolean = True);
    function ActivateView(ViewFactory: IEditorViewFactory): IEditorView;
    function AskSaveChanges: Boolean;
    procedure ApplyEditorOptions(EditorOptions: TSynEditorOptionsContainer);
    procedure Close;
    function GetSynEdit: TSynEdit;
    function GetSynEdit2: TSynEdit;
    function GetActiveSynEdit: TSynEdit;
    function GetBreakpoints: TObjectList;
    function GetCaretPos: TPoint;
    function GetEditorState: string;
    function GetFileName: string;
    function GetFileTitle: string;
    function GetFileId: string;
    function GetModified: Boolean;
    function GetFileEncoding: TFileSaveFormat;
    function GetForm: TForm;
    function GetDocSymbols: TObject;
    function GetEncodedText: AnsiString;
    function GetTabControlIndex: Integer;
    function GetReadOnly: Boolean;
    function GetRemoteFileName: string;
    function GetHasSearchHighlight: Boolean;
    function GetSSHServer: string;
    function GetVersion: Integer;
    procedure SetReadOnly(Value: Boolean);
    procedure SetHasSearchHighlight(Value: Boolean);
    procedure SetFileEncoding(FileEncoding: TFileSaveFormat);
    procedure SetHighlighter(const HighlighterName: string);
    procedure ShowRefactoringMenu;
    procedure OpenLocalFile(const AFileName: string; HighlighterName: string = '');
    procedure OpenRemoteFile(const FileName, ServerName: string);
    function SaveToRemoteFile(const FileName, ServerName: string): Boolean;
    function HasPythonFile: Boolean;
    procedure ExecuteSelection;
    procedure SplitEditorHorizontally;
    procedure SplitEditorVertrically;
    procedure SplitEditorHide;
    procedure Retranslate;
    procedure PullDiagnostics;
    property FileName: string read GetFileName;
    property RemoteFileName: string read GetRemoteFileName;
    property FileId: string read GetFileId;
    property SSHServer: string read GetSSHServer;
    property FileTitle: string read GetFileTitle;
    property Modified: Boolean read GetModified;
    property SynEdit: TSynEdit read GetSynEdit;
    property SynEdit2: TSynEdit read GetSynEdit2;
    property ActiveSynEdit: TSynEdit read GetActiveSynEdit;
    property Breakpoints: TObjectList read GetBreakpoints;
    property FileEncoding: TFileSaveFormat read GetFileEncoding write SetFileEncoding;
    property EncodedText: AnsiString read GetEncodedText;
    property Form: TForm read GetForm;
    property DocSymbols: TObject read GetDocSymbols;
    property HasSearchHighlight: Boolean read GetHasSearchHighlight
      write SetHasSearchHighlight;
    property TabControlIndex: Integer read GetTabControlIndex;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property Version: Integer read GetVersion;
  end;

  TInvalidationType = (itLine, itGutter, itBoth);

  IEditorFactory = interface
  ['{FDAE7FBD-4B61-4D7C-BEE6-DB7740A225E8}']
    function CanCloseAll: Boolean;
    procedure CloseAll;
    function OpenFile(AFileName: string; HighlighterName: string = '';
       TabControlIndex: Integer = 0): IEditor;
    function GetEditorCount: Integer;
    function GetEditor(Index: Integer): IEditor;
    function GetEditorByName(const Name: string): IEditor;
    function GetEditorByFileId(const Name: string): IEditor;
    function GetViewFactoryCount: Integer;
    function GetViewFactory(Index: Integer): IEditorViewFactory;
    procedure InvalidatePos(AFileName: string; ALine: Integer;
      AType: TInvalidationType);
    function NewEditor(TabControlIndex:Integer = 1): IEditor;
    procedure RemoveEditor(AEditor: IEditor);
    function RegisterViewFactory(ViewFactory: IEditorViewFactory): Integer;
    procedure SetupEditorViewsMenu(ViewsMenu: TSpTBXItem; IL: TCustomImageList);
    procedure UpdateEditorViewsMenu(ViewsMenu: TSpTBXItem);
    procedure CreateRecoveryFiles;
    procedure RecoverFiles;
    procedure LockList;
    procedure UnlockList;
    procedure ApplyToEditors(const Proc: TProc<IEditor>);
    function FirstEditorCond(const Predicate: TPredicate<IEditor>): IEditor;
    //procedure GetRegisteredViewFactory(ViewName: string):IEditorViewFactory;
    property Count: Integer read GetEditorCount;
    property Editor[Index: Integer]: IEditor read GetEditor;  default;
    property ViewFactoryCount: Integer read GetViewFactoryCount;
    property ViewFactory[Index: Integer]: IEditorViewFactory read GetViewFactory;
  end;

  IFileCommands = interface
  ['{C10F67B6-BE8D-4A0D-8FDA-05BBF8DEA08A}']
    function CanClose: Boolean;
    function CanSave: Boolean;
    function CanReload: Boolean;
    procedure ExecClose;
    procedure ExecPrint;
    procedure ExecPrintPreview;
    procedure ExecSave;
    procedure ExecSaveAs;
    procedure ExecSaveAsRemote;
    procedure ExecReload(Quiet: Boolean = False);
  end;

  ISearchCommands = interface
    ['{490F145F-01EB-486F-A326-07281AA86BFD}']
    function CanFind: Boolean;
    function CanFindNext: Boolean;
    function CanFindPrev: Boolean;
    function CanReplace: Boolean;
    function GetSearchTarget: TSynEdit;
    procedure ExecFind;
    procedure ExecFindNext;
    procedure ExecFindPrev;
    procedure ExecReplace;
    property SearchTarget: TSynEdit read GetSearchTarget;
  end;

  IMessagesService = interface
  ['{CF747CB1-A5C0-48DC-BE8E-7857074887AD}']
    procedure ShowWindow;
    procedure AddMessage(const Msg: string; const FileName: string = '';
       Line: Integer = 0; Offset: Integer = 0; SelLen: Integer = 0);
    procedure ClearMessages;
    procedure ShowPythonTraceback(Traceback: TPythonTraceback;
      SkipFrames: Integer = 1; ShowWindow: Boolean = False);
  end;

  IUnitTestsService = interface
  ['{E78B808E-5BFA-4480-BC22-72DC5069BB8A}']
    procedure StartTest(Test: Variant);
    procedure StopTest(Test: Variant);
    procedure AddError(Test, Err: Variant);
    procedure AddFailure(Test, Err: Variant);
    procedure AddSuccess(Test: Variant);
    procedure ClearAll;
  end;

  ISystemCommandService = interface
  ['{7BD27760-9522-49D7-9315-85EC305E0082}']
    function GetIsRunning: Boolean;
    function GetRunningTool: string;
    procedure Terminate;
    property IsRunning: Boolean read GetIsRunning;
    property RunningTool: string read GetRunningTool;
  end;

  IProjectService = interface
  ['{170C60D6-3DB9-4DAA-9190-F19BA6CB50DB}']
    function CanClose: Boolean;
    procedure OpenProject;
    procedure OpenProjectFile(const FileName: string);
    procedure NewProject;
    function SaveProject: Boolean;
    function SaveProjectAs: Boolean;
  end;

  IFileExplorer = interface
  ['{0244CC06-409A-4062-825E-917D94C6B262}']
    procedure SetActive(Value: Boolean);
    property Active:Boolean write SetActive;
  end;

  IRegExpService = interface
  ['{F3A4617B-AA4B-476B-BCF6-D7233E42AF3A}']
    procedure Clear;
  end;

  IIDELayouts = interface
  ['{506187EB-6438-4B0B-92B0-07112F812EE8}']
    function LayoutExists(const Layout: string): Boolean;
    procedure LoadLayout(const Layout: string);
    procedure SaveLayout(const Layout: string);
  end;

  IPyIDEServices = interface
  ['{F6E853D8-9527-4AF2-BF15-76DB1FF75F7A}']
    {
      Returns the active editor irrespective of whether it is has the focus
      If want the active editor with focus then use GI_ActiveEditor
    }
    function ReplaceParams(const AText: string): string;
    function GetActiveEditor: IEditor;
    function GetIsClosing: Boolean;
    procedure WriteStatusMsg(const Msg: string);
    function FileIsPythonSource(const FileName: string): Boolean;
    function ShowFilePosition(FileName: string; Line: Integer = 1;
      Offset: Integer = 1; SelLen: Integer = 0;
      ForceToMiddle: Boolean = True; FocusEditor: Boolean = True): Boolean;
    procedure ClearPythonWindows;
    procedure SaveEnvironment;
    procedure SaveFileModules;
    procedure SetRunLastScriptHints(const ScriptName: string);
    procedure SetActivityIndicator(TurnOn: Boolean; Hint: string = ''; OnClick: TNotifyEvent = nil);
    function GetStoredScript(const Name: string): TStrings;
    function GetIDELayouts: IIDELayouts;
    function GetAppStorage: TJvCustomAppStorage;
    function GetLocalAppStorage: TJvCustomAppStorage;
    function GetLogger: TJclSimpleLog;
    procedure MRUAddEditor(Editor: IEditor);
    procedure ShowIDEDockForm(Form: TForm; Activate: Boolean = True);
    property ActiveEditor: IEditor read GetActiveEditor;
    property IsClosing: Boolean read GetIsClosing;
    property Layouts: IIDELayouts read GetIDELayouts;
    property AppStorage: TJvCustomAppStorage read GetAppStorage;
    property LocalAppStorage: TJvCustomAppStorage read GetLocalAppStorage;
    property Logger: TJclSimpleLog read GetLogger;
  end;

  ISSHServices = interface
  ['{255E5E08-DCFD-481A-B0C3-F0AB0C5A1571}']
    function FormatFileName(Server, FileName: string): string;
    function ParseFileName(const Unc: string; out Server, FileName: string): Boolean;

    function Scp(const ScpCommand, FromFile, ToFile: string; out ErrorMsg: string;
       ScpOptions: string = ''): Boolean;
    function ScpUpload(const ServerName, LocalFile, RemoteFile: string; out ErrorMsg: string): Boolean;
    function ScpDownload(const ServerName, RemoteFile, LocalFile: string; out ErrorMsg: string): Boolean;
  end;

var
  GI_EditorFactory: IEditorFactory;
  GI_ActiveEditor: IEditor;

  GI_FileCmds: IFileCommands;
  GI_SearchCmds: ISearchCommands;

  GI_PyIDEServices: IPyIDEServices;
  GI_MessagesService: IMessagesService;
  GI_UnitTestsService: IUnitTestsService;
  GI_SystemCommandService: ISystemCommandService;
  GI_ProjectService: IProjectService;
  GI_FileExplorer: IFileExplorer;
  GI_RegExpService: IRegExpService;
  GI_SSHServices: ISSHServices;

  GI_FileSystemMonitor: IFileSystemMonitor;

implementation

initialization
  GI_FileSystemMonitor := CreateFileSystemMonitor;
finalization
  GI_FileSystemMonitor := nil;
end.




