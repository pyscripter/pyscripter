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
  JclNotify,
  JclSysUtils,
  PythonEngine,
  PythonVersions,
  SynEdit,
  SpTBXItem;

type
  TBreakPoint = class(TPersistent)
  private
    fLineNo : integer;
    fDisabled : Boolean;
    fCondition : string;
  published
    property LineNo : integer read fLineNo write fLineNo;
    property Disabled : Boolean read fDisabled write fDisabled;
    property Condition : string read fCondition write fCondition;
  end;

  IEditor = interface;

  IEditorViewFactory = interface
  ['{680F6C4E-5EED-4684-A199-5A62E644D81B}']
    function CreateForm(Editor: IEditor; AOwner : TComponent): TCustomForm;
    function GetName : string;
    function GetTabCaption : string;
    function GetMenuCaption : string;
    function GetHint : string;
    function GetImageName : string;
    function GetShortCut : TShortCut;
    procedure GetContextHighlighters(List : TList);
    property Name : string read GetName;
    property TabCaption : string read GetTabCaption;
    property MenuCaption : string read GetMenuCaption;
    property Hint : string read GetHint;
    property ImageName : string read GetImageName;
    property ShortCut : TShortCut read GetShortCut;
  end;

  IEditorView = interface
  ['{E68438C1-CE7C-4831-A995-5E72F01AEFEC}']
    procedure UpdateView(Editor : IEditor);
  end;

  TFileSaveFormat = (sf_Ansi, sf_UTF8, sf_UTF8_NoBOM, sf_UTF16LE, sf_UTF16BE);

  IEditor = interface
  ['{15E8BD28-6E18-4D49-8499-1DB594AB88F7}']
    procedure Activate(Primary : Boolean = True);
    function ActivateView(ViewFactory : IEditorViewFactory) : IEditorView;
    function AskSaveChanges: Boolean;
    function CanClose: Boolean;
    procedure Close;
    function GetSynEdit : TSynEdit;
    function GetSynEdit2 : TSynEdit;
    function GetActiveSynEdit : TSynEdit;
    function GetBreakPoints : TObjectList;
    function GetCaretPos: TPoint;
    function GetEditorState: string;
    function GetFileName: string;
    function GetFileTitle: string;
    function GetFileId: string;
    function GetModified: Boolean;
    function GetFileEncoding : TFileSaveFormat;
    function GetForm : TForm;
    function GetDocSymbols: TObject;
    function GetEncodedText : AnsiString;
    function GetTabControlIndex : integer;
    function GetReadOnly : Boolean;
    function GetRemoteFileName: string;
    function GetHasSearchHighlight: Boolean;
    function GetSSHServer: string;
    procedure SetReadOnly(Value : Boolean);
    procedure SetHasSearchHighlight(Value : Boolean);
    procedure SetFileEncoding(FileEncoding : TFileSaveFormat);
    procedure SetHighlighter(const HighlighterName: string);
    procedure OpenFile(const AFileName: string; HighlighterName : string = '');
    procedure OpenRemoteFile(const FileName, ServerName: string);
    function SaveToRemoteFile(const FileName, ServerName: string) : Boolean;
    function HasPythonFile : Boolean;
    procedure ExecuteSelection;
    procedure SplitEditorHorizontally;
    procedure SplitEditorVertrically;
    procedure Retranslate;
    procedure RefreshSymbols;
    property FileName: string read GetFileName;
    property RemoteFileName : string read GetRemoteFileName;
    property FileId: string read GetFileId;
    property SSHServer: string read GetSSHServer;
    property FileTitle: string read GetFileTitle;
    property Modified: Boolean read GetModified;
    property SynEdit: TSynEdit read GetSynEdit;
    property SynEdit2: TSynEdit read GetSynEdit2;
    property ActiveSynEdit: TSynEdit read GetActiveSynEdit;
    property BreakPoints: TObjectList read GetBreakPoints;
    property FileEncoding: TFileSaveFormat read GetFileEncoding write SetFileEncoding;
    property EncodedText: AnsiString read GetEncodedText;
    property Form: TForm read GetForm;
    property DocSymbols: TObject read GetDocSymbols;
    property HasSearchHighlight: Boolean read GetHasSearchHighlight
      write SetHasSearchHighlight;
    property TabControlIndex : integer read GetTabControlIndex;
    property ReadOnly : Boolean read GetReadOnly write SetReadOnly;
  end;

  IEditorFactory = interface
  ['{FDAE7FBD-4B61-4D7C-BEE6-DB7740A225E8}']
    function CanCloseAll: Boolean;
    procedure CloseAll;
    function NewEditor(TabControlIndex:Integer = 1): IEditor;
    function GetEditorCount: integer;
    function GetEditor(Index: integer): IEditor;
    function GetEditorByName(const Name : string): IEditor;
    function GetEditorByFileId(const Name : string): IEditor;
    procedure RemoveEditor(AEditor: IEditor);
    function RegisterViewFactory(ViewFactory : IEditorViewFactory): integer;
    function GetViewFactoryCount: integer;
    function GetViewFactory(Index: integer): IEditorViewFactory;
    procedure SetupEditorViewsMenu(ViewsMenu: TSpTBXItem; IL: TCustomImageList);
    procedure UpdateEditorViewsMenu(ViewsMenu: TSpTBXItem);
    procedure CreateRecoveryFiles;
    procedure RecoverFiles;
    procedure LockList;
    procedure UnlockList;
    procedure ApplyToEditors(const Proc: TProc<IEditor>);
    function FirstEditorCond(const Predicate: TPredicate<IEditor>): IEditor;
    //procedure GetRegisteredViewFactory(ViewName : string):IEditorViewFactory;
    property Count : integer read GetEditorCount;
    property Editor[Index: integer]: IEditor read GetEditor;  default;
    property ViewFactoryCount : integer read GetViewFactoryCount;
    property ViewFactory[Index: integer]: IEditorViewFactory read GetViewFactory;
  end;

  IEditCommands = interface
  ['{64397AD0-BA45-4F4A-B72E-2E4647B8ACB9}']
    function CanCopy: Boolean;
    function CanCut: Boolean;
    function CanDelete: Boolean;
    function CanPaste: Boolean;
    function CanRedo: Boolean;
    function CanSelectAll: Boolean;
    function CanUndo: Boolean;
    procedure ExecCopy;
    procedure ExecCut;
    procedure ExecDelete;
    procedure ExecPaste;
    procedure ExecRedo;
    procedure ExecSelectAll;
    procedure ExecUndo;
  end;

  IFileCommands = interface
  ['{C10F67B6-BE8D-4A0D-8FDA-05BBF8DEA08A}']
    function CanClose: Boolean;
    function CanPrint: Boolean;
    function CanSave: Boolean;
    function CanSaveAs: Boolean;
    function CanReload: Boolean;
    procedure ExecClose;
    procedure ExecPrint;
    procedure ExecPrintPreview;
    procedure ExecSave;
    procedure ExecSaveAs;
    procedure ExecSaveAsRemote;
    procedure ExecReload(Quiet : Boolean = False);
  end;

  ISearchCommands = interface
    ['{490F145F-01EB-486F-A326-07281AA86BFD}']
    function CanFind: Boolean;
    function CanFindNext: Boolean;
    function CanFindPrev: Boolean;
    function CanReplace: Boolean;
    function GetSearchTarget : TSynEdit;
    procedure ExecFind;
    procedure ExecFindNext;
    procedure ExecFindPrev;
    procedure ExecReplace;
    property SearchTarget : TSynEdit read GetSearchTarget;
  end;

  IMessageServices = interface
  ['{CF747CB1-A5C0-48DC-BE8E-7857074887AD}']
    procedure ShowWindow;
    procedure AddMessage(const Msg: string; const FileName : string = '';
       Line : integer = 0; Offset : integer = 0; SelLen : integer = 0);
    procedure ClearMessages;
    procedure ShowPythonTraceback(Traceback: TPythonTraceback; SkipFrames : integer = 1; ShowWindow : Boolean = False);
  end;

  IUnitTestServices = interface
  ['{E78B808E-5BFA-4480-BC22-72DC5069BB8A}']
    procedure StartTest(Test: Variant);
    procedure StopTest(Test: Variant);
    procedure AddError(Test, Err: Variant);
    procedure AddFailure(Test, Err: Variant);
    procedure AddSuccess(Test: Variant);
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
    function GetActiveEditor : IEditor;
    function GetIsClosing: Boolean;
    procedure WriteStatusMsg(const S: string);
    function FileIsPythonSource(const FileName: string): Boolean;
    function ShowFilePosition(FileName : string; Line: integer = 0;
      Offset : integer = 1; SelLen : integer = 0;
      ForceToMiddle : boolean = True; FocusEditor : boolean = True) : boolean;
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
    property ActiveEditor: IEditor read GetActiveEditor;
    property IsClosing: Boolean read GetIsClosing;
    property Messages: IMessageServices read GetMessageServices;
    property UnitTests: IUnitTestServices read GetUnitTestServices;
    property Layouts: IIDELayouts read GetIDELayouts;
    property AppStorage: TJvCustomAppStorage read GetAppStorage;
    property LocalAppStorage: TJvCustomAppStorage read GetLocalAppStorage;
    property Logger: TJclSimpleLog read GetLogger;
  end;

  IPyEngineAndGIL = interface
    function GetPyEngine: TPythonEngine;
    function GetThreadState: PPyThreadState;
    property PythonEngine: TPythonEngine read GetPyEngine;
    property ThreadState: PPyThreadState read GetThreadState;
  end;

  IPyControl = interface
  ['{DE1C1145-DC0F-4829-B36B-74EC818E168E}']
    function PythonLoaded: Boolean;
    function Running: boolean;
    function Inactive: boolean;
    function GetPythonVersion: TPythonVersion;
    function GetOnPythonVersionChange: TJclNotifyEventBroadcast;
    function AddPathToInternalPythonPath(const Path: string): IInterface;
    function SafePyEngine: IPyEngineAndGIL;
    procedure ThreadPythonExec(ExecuteProc : TProc; TerminateProc : TProc = nil;
      WaitToFinish: Boolean = False; ThreadExecMode : TThreadExecMode = emNewState);
    property PythonVersion: TPythonVersion read GetPythonVersion;
    property OnPythonVersionChange: TJclNotifyEventBroadcast
      read GetOnPythonVersionChange;
  end;

  TPyInterpreterPropmpt = (pipNormal, pipDebug, pipPostMortem);
  IPyInterpreter = interface
  ['{6BAAD187-B00E-4E2A-B01D-C47EED922E59}']
    procedure ShowWindow;
    procedure AppendPrompt;
    procedure RemovePrompt;
    procedure AppendText(const S: string);
    procedure PrintInterpreterBanner(AVersion: string = ''; APlatform: string = '');
    procedure WritePendingMessages;
    procedure ClearPendingMessages;
    procedure ClearDisplay;
    procedure ClearLastPrompt;
    function OutputSuppressor : IInterface;
    procedure StartOutputMirror(const AFileName : string; Append : Boolean);
    procedure StopFileMirror;
    procedure UpdatePythonKeywords;
    procedure SetPyInterpreterPrompt(Pip: TPyInterpreterPropmpt);
    procedure ReinitInterpreter;
    function GetPythonIO: TPythonInputOutput;
    function GetEditor: TCustomSynEdit;
    function GetShowOutput: boolean;
    procedure SetShowOutput(const Value: boolean);
    property Editor: TCustomSynEdit read GetEditor;
    property PythonIO: TPythonInputOutput read GetPythonIO;
    property ShowOutput: Boolean read GetShowOutput write SetShowOutput;
  end;

  ISSHServices = interface
  ['{255E5E08-DCFD-481A-B0C3-F0AB0C5A1571}']
    function FormatFileName(Server, FileName : string): string;
    function ParseFileName(Const Unc : string; out Server, FileName : string): boolean;

    function Scp(const ScpCommand, FromFile, ToFile: string; out ErrorMsg: string;
       ScpOptions : string = ''): Boolean;
    function ScpUpload(const ServerName, LocalFile, RemoteFile: string; out ErrorMsg: string): boolean;
    function ScpDownload(const ServerName, RemoteFile, LocalFile: string; out ErrorMsg: string): boolean;
  end;

var
  GI_EditorFactory: IEditorFactory;
  GI_ActiveEditor: IEditor;

  GI_EditCmds: IEditCommands;
  GI_FileCmds: IFileCommands;
  GI_SearchCmds: ISearchCommands;

  GI_PyIDEServices: IPyIDEServices;
  GI_PyControl: IPyControl;
  GI_PyInterpreter: IPyInterpreter;
  GI_SSHServices: ISSHServices;

implementation

end.




