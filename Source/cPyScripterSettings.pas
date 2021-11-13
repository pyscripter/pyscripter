{-----------------------------------------------------------------------------
 Unit Name: cPyScripterSettings
 Author:    Pyscripter
 Date:      22-Jan-2018
 Purpose:   Centralize the management of PyScripter settings
 History:
-----------------------------------------------------------------------------}

unit cPyScripterSettings;

interface
Uses
  System.Classes,
  Vcl.ImgList,
  Vcl.Graphics,
  JclNotify,
  SpTBXTabs,
  SynEditTypes,
  SynEditCodeFolding,
  SynEditKeyCmds,
  WrapDelphi,
  uEditAppIntfs,
  cPySupportTypes,
  dlgSynEditOptions;

Const
  dsaSearchFromStart = 1;
  dsaReplaceFromStart = 2;
  dsaReplaceNumber = 3;
  dsaSearchStartReached = 4;
  dsaPostMortemInfo = 5;

type
  TFileChangeNotificationType = (fcnFull, fcnNoMappedDrives, fcnDisabled);

{$METHODINFO ON}
  TBaseOptionsClass = class of TBaseOptions;
  TBaseOptions = class(TInterfacedPersistent)
    public
    constructor Create; virtual; abstract;
  end;

  {
    Persistent IDE Settings
    Note: TPythonIDEOptions is exposed to Python.
    IFreeNotification is implemented to make sure the object is not used
    by Python after its destruction (see WrapDelphi for details)
  }
  TPythonIDEOptions = class(TBaseOptions, IFreeNotification)
  private
    fFreeNotifyImpl : IFreeNotification;
    fOnChange: TJclNotifyEventBroadcast;
    fTimeOut : integer;
    fUndoAfterSave : Boolean;
    fSaveFilesBeforeRun : Boolean;
    fSaveEnvironmentBeforeRun : Boolean;
    fRestoreOpenFiles : Boolean;
    fRestoreOpenProject : Boolean;
    fCreateBackupFiles : Boolean;
    fExporerInitiallyExpanded : Boolean;
    fProjectExporerInitiallyExpanded : Boolean;
    fSearchTextAtCaret : Boolean;
    fPythonFileFilter : string;
    fCythonFileFilter : string;
    fHTMLFileFilter : string;
    fXMLFileFilter : string;
    fCSSFileFilter : string;
    fCPPFileFilter : string;
    fYAMLFileFilter : string;
    fJSFileFilter : string;
    fPHPFileFilter : string;
    fJSONFileFilter : string;
    fGeneralFileFilter : string;
    fFileExplorerFilter : string;
    fDateLastCheckedForUpdates : TDateTime;
    fAutoCheckForUpdates : boolean;
    fDaysBetweenChecks : integer;
    fMaskFPUExceptions : boolean;
    fSpecialPackages : string;
    fShowCodeHints : boolean;
    fShowDebuggerHints : boolean;
    fAutoCompleteBrackets : boolean;
    fCommandLine : string;
    fUseCommandLine : Boolean;
    fMarkExecutableLines : Boolean;
    fCheckSyntaxAsYouType : Boolean;
    fFileExplorerContextMenu : Boolean;
    fNewFileLineBreaks : TSynEditFileFormat;
    fNewFileEncoding : TFileSaveFormat;
    fDetectUTF8Encoding: Boolean;
    fEditorsTabPosition : TSpTBXTabPosition;
    fPythonEngineType : TPythonEngineType;
    fPrettyPrintOutput : Boolean;
    fSmartNextPrevPage : Boolean;
    fAutoReloadChangedFiles : Boolean;
    fClearOutputBeforeRun : Boolean;
    fAutoHideFindToolbar : Boolean;
    fCodeCompletionListSize : integer;
    fEditorCodeCompletion : Boolean;
    fInterpreterCodeCompletion : Boolean;
    fShowTabCloseButton : Boolean;
    fPostMortemOnException : Boolean;
    fDockAnimationInterval : integer;
    fDockAnimationMoveWidth : integer;
    fInterpreterHistorySize : integer;
    fSaveInterpreterHistory : Boolean;
    fReinitializeBeforeRun: Boolean;
    fJumpToErrorOnException : Boolean;
    fFileTemplateForNewScripts : string;
    fAutoCompletionFont : TFont;
    fHighlightSelectedWord : Boolean;
    fHighlightSelectedWordColor : TColor;
    fFileChangeNotification : TFileChangeNotificationType;
    fCodeCompletionCaseSensitive : Boolean;
    fCompleteKeywords : Boolean;
    fCompleteAsYouType: Boolean;
    fCompleteWithWordBreakChars: Boolean;
    fCompleteWithOneEntry:Boolean;
    fDisplayPackageNames:Boolean;
    fNoOfRecentFiles : integer;
    fCodeFoldingEnabled : boolean;
    fCodeFolding : TSynCodeFolding;
    fInternalInterpreterHidden : boolean;
    fCompactLineNumbers : Boolean;
    fStyleMainWindowBorder : Boolean;
    fFileExplorerBackgroundProcessing : Boolean;
    fSSHCommand : string;
    fSSHOptions: string;
    fScpCommand : string;
    fScpOptions: string;
    fSSHDisableVariablesWin: Boolean;
    fAlwaysUseSockets: Boolean;
    fTrimTrailingSpacesOnSave: Boolean;
    fTraceOnlyIntoOpenFiles: Boolean;
    fLspDebug: Boolean;
    function GetPythonFileExtensions: string;
    procedure SetAutoCompletionFont(const Value: TFont);
  protected
    property FreeNotifyImpl : IFreeNotification read fFreeNotifyImpl implements IFreeNotification;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Changed;
    property PythonFileExtensions : string read GetPythonFileExtensions;
    property OnChange: TJclNotifyEventBroadcast read fOnChange;
  published
    property CodeFolding : TSynCodeFolding read fCodeFolding
      write fCodeFolding;
    property TimeOut : integer read fTimeOut write fTimeOut default 0;
    property UndoAfterSave : boolean read fUndoAfterSave
      write fUndoAfterSave default True;
    property SaveFilesBeforeRun : boolean read fSaveFilesBeforeRun
      write fSaveFilesBeforeRun default True;
    property SaveEnvironmentBeforeRun : boolean read fSaveEnvironmentBeforeRun
      write fSaveEnvironmentBeforeRun default False;
    property RestoreOpenFiles : Boolean read fRestoreOpenFiles
      write fRestoreOpenFiles default True;
    property RestoreOpenProject : Boolean read fRestoreOpenProject
      write fRestoreOpenProject default True;
    property CreateBackupFiles : boolean read fCreateBackupFiles
      write fCreateBackupFiles default False;
    property ExporerInitiallyExpanded : boolean read fExporerInitiallyExpanded
      write fExporerInitiallyExpanded default False;
    property ProjectExporerInitiallyExpanded : boolean read fProjectExporerInitiallyExpanded
      write fProjectExporerInitiallyExpanded default True;
    property SearchTextAtCaret : Boolean read fSearchTextAtCaret
      write fSearchTextAtCaret stored False;
    property PythonFileFilter : string read fPythonFileFilter
      write fPythonFileFilter;
    property CythonFileFilter : string read fCythonFileFilter
      write fCythonFileFilter;
    property HTMLFileFilter : string read fHTMLFileFilter
      write fHTMLFileFilter;
    property XMLFileFilter : string read fXMLFileFilter
      write fXMLFileFilter;
    property CSSFileFilter : string read fCSSFileFilter
      write fCSSFileFilter;
    property CPPFileFilter : string read fCPPFileFilter
      write fCPPFileFilter;
    property YAMLFileFilter : string read fYAMLFileFilter
      write fYAMLFileFilter;
    property JSFileFilter : string read fJSFileFilter
      write fJSFileFilter;
    property PHPFileFilter : string read fPHPFileFilter
      write fPHPFileFilter;
    property JSONFileFilter : string read fJSONFileFilter
      write fJSONFileFilter;
    property GeneralFileFilter : string read fGeneralFileFilter
      write fGeneralFileFilter;
    property FileExplorerFilter : string read fFileExplorerFilter
      write fFileExplorerFilter;
    property DateLastCheckedForUpdates : TDateTime read fDateLastCheckedForUpdates
      write fDateLastCheckedForUpdates;
    property AutoCheckForUpdates : boolean read fAutoCheckForUpdates
      write fAutoCheckForUpdates default True;
    property DaysBetweenChecks : integer read fDaysBetweenChecks
      write fDaysBetweenChecks default 7;
    property MaskFPUExceptions : Boolean read fMaskFPUExceptions
      write fMaskFPUExceptions default True;
    property SpecialPackages : string read fSpecialPackages write fSpecialPackages;
    property ShowCodeHints : boolean read fShowCodeHints
      write fShowCodeHints default True;
    property ShowDebuggerHints : boolean read fShowDebuggerHints
      write fShowDebuggerHints default True;
    property AutoCompleteBrackets : boolean read fAutoCompleteBrackets
      write fAutoCompleteBrackets default True;
    property CommandLine : string read fCommandLine write fCommandLine;
    property UseCommandLine : Boolean read fUseCommandLine
      write fUseCommandLine default False;
    property MarkExecutableLines : Boolean read fMarkExecutableLines
      write fMarkExecutableLines default False;
    property CheckSyntaxAsYouType : Boolean read fCheckSyntaxAsYouType
      write fCheckSyntaxAsYouType default False;
    property FileExplorerContextMenu : Boolean read fFileExplorerContextMenu
      write fFileExplorerContextMenu default True;
    property NewFileLineBreaks : TSynEditFileFormat read fNewFileLineBreaks
      write fNewFileLineBreaks default sffDos;
    property NewFileEncoding : TFileSaveFormat read fNewFileEncoding
      write fNewFileEncoding default sf_UTF8_NoBOM;
    property DetectUTF8Encoding : Boolean read fDetectUTF8Encoding
      write fDetectUTF8Encoding default True;
    property EditorsTabPosition : TSpTBXTabPosition read fEditorsTabPosition
      write fEditorsTabPosition default ttpBottom;
    property PythonEngineType : TPythonEngineType read fPythonEngineType
      write fPythonEngineType default peRemote;
    property PrettyPrintOutput : Boolean read fPrettyPrintOutput
      write fPrettyPrintOutput default True;
    property SmartNextPrevPage : Boolean read fSmartNextPrevPage
      write fSmartNextPrevPage default True;
    property AutoReloadChangedFiles : Boolean read fAutoReloadChangedFiles
      write fAutoReloadChangedFiles default True;
    property ClearOutputBeforeRun : Boolean read fClearOutputBeforeRun
      write fClearOutputBeforeRun default False;
    property AutoHideFindToolbar : Boolean read fAutoHideFindToolbar
      write fAutoHideFindToolbar default False;
    property EditorCodeCompletion : Boolean read fEditorCodeCompletion
      write fEditorCodeCompletion default True;
    property InterpreterCodeCompletion : Boolean read fInterpreterCodeCompletion
      write fInterpreterCodeCompletion default True;
    property CodeCompletionListSize : integer read fCodeCompletionListSize
      write fCodeCompletionListSize default 10;
    property ShowTabCloseButton : Boolean read fShowTabCloseButton
      write fShowTabCloseButton default True;
    property PostMortemOnException : Boolean read fPostMortemOnException
      write fPostMortemOnException default False;
    property DockAnimationInterval : integer read fDockAnimationInterval
      write fDockAnimationInterval;
    property DockAnimationMoveWidth : integer read fDockAnimationMoveWidth
      write fDockAnimationMoveWidth;
    property InterpreterHistorySize : integer read fInterpreterHistorySize
      write fInterpreterHistorySize;
    property SaveInterpreterHistory : Boolean read fSaveInterpreterHistory
      write fSaveInterpreterHistory default True;
    property ReinitializeBeforeRun : Boolean read fReinitializeBeforeRun
      write fReinitializeBeforeRun default True;
    property JumpToErrorOnException : Boolean read fJumpToErrorOnException
      write fJumpToErrorOnException default True;
    property FileTemplateForNewScripts: string read fFileTemplateForNewScripts
      write fFileTemplateForNewScripts;
    property AutoCompletionFont : TFont read fAutoCompletionFont
      write SetAutoCompletionFont;
    property HighlightSelectedWord : boolean read fHighlightSelectedWord
      write fHighlightSelectedWord default True;
    property HighlightSelectedWordColor : TColor read fHighlightSelectedWordColor
      write fHighlightSelectedWordColor default clOlive;
    property FileChangeNotification : TFileChangeNotificationType read fFileChangeNotification
      write fFileChangeNotification default fcnDisabled;
    property CodeCompletionCaseSensitive : Boolean read fCodeCompletionCaseSensitive
      write fCodeCompletionCaseSensitive default True;
    property CompleteKeywords : Boolean read fCompleteKeywords
      write fCompleteKeywords default True;
    property CompleteAsYouType : Boolean read fCompleteAsYouType
      write fCompleteAsYouType default True;
    property CompleteWithWordBreakChars : Boolean read fCompleteWithWordBreakChars
      write fCompleteWithWordBreakChars default False;
    property CompleteWithOneEntry : Boolean read fCompleteWithOneEntry
      write fCompleteWithOneEntry default False;
    property DisplayPackageNames : Boolean read fDisplayPackageNames
      write fDisplayPackageNames default True;
    property NoOfRecentFiles : integer read fNoOfRecentFiles
      write fNoOfRecentFiles default 8;
    property CodeFoldingEnabled : Boolean read fCodeFoldingEnabled
      write fCodeFoldingEnabled default True;
    property InternalInterpreterHidden : Boolean read fInternalInterpreterHidden
      write fInternalInterpreterHidden default True;
    property CompactLineNumbers : Boolean read fCompactLineNumbers
      write fCompactLineNumbers default True;
    property StyleMainWindowBorder : Boolean read fStyleMainWindowBorder
      write fStyleMainWindowBorder default False;
    property FileExplorerBackgroundProcessing : Boolean read fFileExplorerBackgroundProcessing
      write fFileExplorerBackgroundProcessing default False;
    property SSHCommand : string read fSSHCommand write fSSHCommand;
    property SSHOptions : string read fSSHOptions write fSSHOptions;
    property ScpCommand : string read fScpCommand write fScpCommand;
     property ScpOptions : string read fScpOptions write fScpOptions;
    property SSHDisableVariablesWin : boolean read fSSHDisableVariablesWin
      write fSSHDisableVariablesWin default True;
    property AlwaysUseSockets : Boolean read fAlwaysUseSockets
      write fAlwaysUseSockets default False;
    property TrimTrailingSpacesOnSave : Boolean read fTrimTrailingSpacesOnSave
      write fTrimTrailingSpacesOnSave default True;
    property TraceOnlyIntoOpenFiles : Boolean read fTraceOnlyIntoOpenFiles
      write fTraceOnlyIntoOpenFiles default False;
    property LspDebug: Boolean read fLspDebug write fLspDebug default false;
  end;
{$METHODINFO OFF}

  TPyScripterSettings = class
    class var IsPortable: Boolean;
    class var UserDataPath: string;
    class var OptionsFileName: string;
    class var ColorThemesFilesDir: string;
    class var StylesFilesDir: string;
    class var LspServerPath: string;
    class var EngineInitFile: string;
    class var PyScripterInitFile: string;
    class var ShellImages: TCustomImageList;
    class var DefaultEditorKeyStrokes: TSynEditKeyStrokes;
    class procedure RegisterEditorUserCommands(Keystrokes: TSynEditKeyStrokes);
    class procedure CreateIDEOptions;
    class procedure CreateEditorOptions;
    class constructor CreateSettings;
    class destructor Destroy;
  end;

Const
  // Additional Synedit commands
  ecRecallCommandPrev = ecUserFirst + 100;
  ecRecallCommandNext = ecUserFirst + 101;
  ecRecallCommandEsc = ecUserFirst + 102;
  ecCodeCompletion = ecUserFirst + 103;
  ecParamCompletion = ecUserFirst + 104;
  ecSelMatchBracket = ecUserFirst + 105;


Var
  PyIDEOptions : TPythonIDEOptions;
  EditorOptions : TSynEditorOptionsContainer;
  InterpreterEditorOptions : TSynEditorOptionsContainer;

implementation

uses
  Winapi.Windows,
  System.UITypes,
  System.SysUtils,
  System.IOUtils,
  Vcl.Forms,
  Vcl.Dialogs,
  uHighlighterProcs,
  JvAppStorage,
  JvGnuGettext,
  SynEdit,
  SynEditStrConst,
  SynEditMiscClasses,
  SynHighlighterPython,
  SynHighlighterYAML,
  StringResources,
  uCommonFunctions;

{ TPythonIDEOptions }

procedure TPythonIDEOptions.Assign(Source: TPersistent);
begin
  if Source is TPythonIDEOptions then
    with TPythonIDEOptions(Source) do begin
      Self.fCodeFolding.Assign(CodeFolding);
      Self.fTimeOut := TimeOut;
      Self.fUndoAfterSave := UndoAfterSave;
      Self.fSaveFilesBeforeRun := SaveFilesBeforeRun;
      Self.fSaveEnvironmentBeforeRun := SaveEnvironmentBeforeRun;
      Self.fRestoreOpenFiles := fRestoreOpenFiles;
      Self.fRestoreOpenProject := fRestoreOpenProject;
      Self.fCreateBackUpFiles := CreateBackUpFiles;
      Self.fExporerInitiallyExpanded := ExporerInitiallyExpanded;
      Self.fProjectExporerInitiallyExpanded := ProjectExporerInitiallyExpanded;
      Self.fSearchTextAtCaret := SearchTextAtCaret;
      Self.fPythonFileFilter := PythonFileFilter;
      Self.fCythonFileFilter := CythonFileFilter;
      Self.fHTMLFileFilter := HTMLFileFilter;
      Self.fXMLFileFilter := XMLFileFilter;
      Self.fCSSFileFilter := CSSFileFilter;
      Self.fCPPFileFilter := CPPFileFilter;
      Self.fYAMLFileFilter := YAMLFileFilter;
      Self.fJSFileFilter := JSFileFilter;
      Self.fPHPFileFilter := PHPFileFilter;
      Self.fJSONFileFilter := JSONFileFilter;
      Self.fGeneralFileFilter := GeneralFileFilter;
      Self.fFileExplorerFilter := FileExplorerFilter;
      Self.fDateLastCheckedForUpdates := DateLastCheckedForUpdates;
      Self.fAutoCheckForUpdates := AutoCheckForUpdates;
      Self.fDaysBetweenChecks := DaysBetweenChecks;
      Self.fMaskFPUExceptions := MaskFPUExceptions;
      Self.fSpecialPackages := SpecialPackages;
      Self.fShowCodeHints := ShowCodeHints;
      Self.fShowDebuggerHints := ShowDebuggerHints;
      Self.fAutoCompleteBrackets := AutoCompleteBrackets;
      Self.fUseCommandLine := UseCommandLine;
      Self.fCommandLine := CommandLine;
      Self.fMarkExecutableLines := MarkExecutableLines;
      Self.fCheckSyntaxAsYouType := CheckSyntaxAsYouType;
      Self.fFileExplorerContextMenu := FileExplorerContextMenu;
      Self.fNewFileLineBreaks := NewFileLineBreaks;
      Self.fNewFileEncoding := NewFileEncoding;
      Self.fDetectUTF8Encoding := DetectUTF8Encoding;
      Self.fEditorsTabPosition := EditorsTabPosition;
      Self.fPythonEngineType := PythonEngineType;
      Self.fPrettyPrintOutput := PrettyPrintOutput;
      Self.fSmartNextPrevPage := SmartNextPrevPage;
      Self.fAutoReloadChangedFiles := AutoReloadChangedFiles;
      Self.fClearOutputBeforeRun := ClearOutputBeforeRun;
      Self.fAutoHideFindToolbar := AutoHideFindToolbar;
      Self.fEditorCodeCompletion := EditorCodeCompletion;
      Self.fInterpreterCodeCompletion := InterpreterCodeCompletion;
      Self.fCodeCompletionListSize := CodeCompletionListSize;
      Self.fShowTabCloseButton := ShowTabCloseButton;
      Self.fPostMortemOnException := PostMortemOnException;
      Self.fDockAnimationInterval := DockAnimationInterval;
      Self.fDockAnimationMoveWidth := DockAnimationMoveWidth;
      Self.fInterpreterHistorySize := InterpreterHistorySize;
      Self.fSaveInterpreterHistory := SaveInterpreterHistory;
      Self.fReinitializeBeforeRun := ReinitializeBeforeRun;
      Self.fJumpToErrorOnException := JumpToErrorOnException;
      Self.fFileTemplateForNewScripts := FileTemplateForNewScripts;
      Self.fAutoCompletionFont.Assign(AutoCompletionFont);
      Self.fHighlightSelectedWord := HighlightSelectedWord;
      Self.fHighlightSelectedWordColor := HighlightSelectedWordColor;
      Self.fFileChangeNotification := FileChangeNotification;
      Self.fCodeCompletionCaseSensitive := CodeCompletionCaseSensitive;
      Self.fCompleteKeywords := CompleteKeywords;
      Self.fCompleteAsYouType := CompleteAsYouType;
      Self.fCompleteWithWordBreakChars := CompleteWithWordBreakChars;
      Self.fCompleteWithOneEntry := CompleteWithOneEntry;
      Self.fDisplayPackageNames := DisplayPackageNames;
      Self.fNoOfRecentFiles := NoOfRecentFiles;
      Self.fCodeFoldingEnabled := CodeFoldingEnabled;
      Self.fInternalInterpreterHidden := InternalInterpreterHidden;
      Self.fCompactLineNumbers := CompactLineNumbers;
      Self.fStyleMainWindowBorder := StyleMainWindowBorder;
      Self.fFileExplorerBackgroundProcessing := FileExplorerBackgroundProcessing;
      Self.fSSHCommand := SSHCommand;
      Self.fSSHOptions := SSHOptions;
      Self.fScpCommand := ScpCommand;
      Self.fScpOptions := ScpOptions;
      Self.fSSHDisableVariablesWin := SSHDisableVariablesWin;
      Self.fAlwaysUseSockets := AlwaysUseSockets;
      Self.fTrimTrailingSpacesOnSave := TrimTrailingSpacesOnSave;
      Self.fTraceOnlyIntoOpenFiles := TraceOnlyIntoOpenFiles;
      Self.fLspDebug := LspDebug;
    end
  else
    inherited;
end;

procedure TPythonIDEOptions.Changed;
begin
  fOnChange.Notify(Self);
end;

constructor TPythonIDEOptions.Create;
begin
  fFreeNotifyImpl := TFreeNotificationImpl.Create(Self);
  fOnChange := TJclNotifyEventBroadcast.Create;

  fTimeOut := 0; // 5000;
  fUndoAfterSave := True;
  fSaveFilesBeforeRun := True;
  fSaveEnvironmentBeforeRun := False;
  fCreateBackupFiles := False;
  fExporerInitiallyExpanded := False;
  fProjectExporerInitiallyExpanded := True;
  fPythonFileFilter := _(sPythonFileFilter);
  fCythonFileFilter := _(sCythonFileFilter);
  fHTMLFileFilter := _(sHTMLFileFilter);
  fXMLFileFilter := _(sXMLFileFilter);
  fCSSFileFilter := _(sCSSFileFilter);
  fCPPFileFilter := _(sCPPFileFilter);
  fYAMLFileFilter := _(sYAMLFileFilter);
  fJSFileFilter := _(sJSFileFilter);
  fPHPFileFilter := _(sPHPFileFilter);
  fJSONFileFilter := _(sJSONFileFilter);
  fGeneralFileFilter := _(sGeneralFileFilter);
  fFileExplorerFilter := '*.py;*.pyw';
  fSearchTextAtCaret := True;
  fRestoreOpenFiles := True;
  fRestoreOpenProject := True;
  fDateLastCheckedForUpdates := MinDateTime;
  fAutoCheckForUpdates := True;
  fDaysBetweenChecks := 7;
  fMaskFPUExceptions := True;
  fSpecialPackages := 'os, numpy, pandas';
  fShowCodeHints := True;
  fShowDebuggerHints := True;
  fAutoCompleteBrackets := True;
  fMarkExecutableLines := False;
  fCheckSyntaxAsYouType := False;
  fFileExplorerContextMenu := True;
  fNewFileLineBreaks := sffDos;
  fNewFileEncoding := sf_UTF8_NoBOM;
  fDetectUTF8Encoding := True;
  fEditorsTabPosition := ttpBottom;
  fPythonEngineType := peRemote;
  fPrettyPrintOutput := True;
  fSmartNextPrevPage := True;
  fAutoReloadChangedFiles := True;
  fClearOutputBeforeRun := False;
  fAutoHideFindToolbar := False;
  fEditorCodeCompletion := True;
  fInterpreterCodeCompletion := True;
  fCodeCompletionListSize := 10;
  fShowTabCloseButton := True;
  fPostMortemOnException := False;
  fDockAnimationInterval := 20;
  fDockAnimationMoveWidth := 40;
  fInterpreterHistorySize := 50;
  fSaveInterpreterHistory := True;
  fReinitializeBeforeRun := True;
  fJumpToErrorOnException := True;
  fFileTemplateForNewScripts := _(SPythonTemplateName);
  fAutoCompletionFont := TFont.Create;
  SetDefaultUIFont(fAutoCompletionFont);
  fHighlightSelectedWord := True;
  fHighlightSelectedWordColor := clOlive;
  fFileChangeNotification := fcnDisabled;
  fCodeCompletionCaseSensitive := True;
  fCompleteKeywords := True;
  fCompleteAsYouType := True;
  fCompleteWithWordBreakChars := False;
  fCompleteWithOneEntry := False;
  fDisplayPackageNames := True;
  fNoOfRecentFiles := 8;
  fCodeFoldingEnabled := True;
  fInternalInterpreterHidden := True;
  fCompactLineNumbers := True;
  fStyleMainWindowBorder := False;
  fFileExplorerBackgroundProcessing := False;
  fSSHCommand := 'ssh';
  fSSHOptions := '-o PasswordAuthentication=no -o StrictHostKeyChecking=no';
  fScpCommand := 'scp';
  fScpOptions := '-T -o PasswordAuthentication=no -o StrictHostKeyChecking=no';
  fSSHDisableVariablesWin := True;
  fAlwaysUseSockets := True;
  fTrimTrailingSpacesOnSave := True;
  fTraceOnlyIntoOpenFiles := False;
  fLspDebug := False;
  fCodeFolding := TSynCodeFolding.Create;
  fCodeFolding.GutterShapeSize := 9;  // default value
end;

destructor TPythonIDEOptions.Destroy;
begin
  FreeAndNil(fAutoCompletionFont);
  FreeAndNil(fCodeFolding);
  FreeAndNil(fOnChange);
  inherited;
end;

function TPythonIDEOptions.GetPythonFileExtensions: string;
begin
  Result := FileMaskFromFileFilter(PythonFileFilter);
end;

procedure TPythonIDEOptions.SetAutoCompletionFont(const Value: TFont);
begin
  fAutoCompletionFont.Assign(Value);
end;


{$REGION 'AppStorage handlers' }

type
// Modify JvAppStorage handling of Fonts
// We want to store Font.Size and not Font.Height
TJvAppStorageFontPropertyEngine = class(TJvAppStoragePropertyBaseEngine)
  private
    class var FFontIgnoreProperties: TStringList;
public
  function Supports(AObject: TObject; AProperty: TObject): Boolean; override;
  procedure ReadProperty(AStorage: TJvCustomAppStorage; const APath: string; AObject: TObject; AProperty: TObject; const Recursive,
    ClearFirst: Boolean; const IgnoreProperties: TStrings = nil); override;
  procedure WriteProperty(AStorage: TJvCustomAppStorage; const APath: string; AObject: TObject; AProperty: TObject; const
    Recursive: Boolean; const IgnoreProperties: TStrings = nil); override;
  class property FontIgnoreProperties : TStringList read FFontIgnoreProperties;
strict private
  class constructor Create;
  class destructor Destroy;
end;


{ TJvAppStorageFontPropertyEngine }

function TJvAppStorageFontPropertyEngine.Supports(AObject,
  AProperty: TObject): Boolean;
begin
  Result := AProperty is TFont;
end;

class constructor TJvAppStorageFontPropertyEngine.Create;
begin
  TJvAppStorageFontPropertyEngine.FFontIgnoreProperties := TStringList.Create;
  with TJvAppStorageFontPropertyEngine.FFontIgnoreProperties do begin
    Add('Height');
    Add('Charset');
    Add('Orientation');
    Add('Pitch');
    Add('Quality');
  end;
end;

class destructor TJvAppStorageFontPropertyEngine.Destroy;
begin
  TJvAppStorageFontPropertyEngine.FFontIgnoreProperties.Free;
end;

procedure TJvAppStorageFontPropertyEngine.ReadProperty(
  AStorage: TJvCustomAppStorage; const APath: string; AObject,
  AProperty: TObject; const Recursive, ClearFirst: Boolean;
  const IgnoreProperties: TStrings);
begin
  TFont(AProperty).Style := []; // initialize
  // Font Size is a published property and is read
  AStorage.ReadPersistent(APath, AProperty as TFont, Recursive, ClearFirst,
    TJvAppStorageFontPropertyEngine.FFontIgnoreProperties);
  if Screen.Fonts.IndexOf(TFont(AProperty).Name) < 0 then
    TFont(AProperty).Name := DefaultCodeFontName;
end;

procedure TJvAppStorageFontPropertyEngine.WriteProperty(
  AStorage: TJvCustomAppStorage; const APath: string; AObject,
  AProperty: TObject; const Recursive: Boolean;
  const IgnoreProperties: TStrings);
Var
  Index : Integer;
begin
  // Do not save style if empty
  Index := - 1;
  if TFont(AProperty).Style = [] then
    Index := TJvAppStorageFontPropertyEngine.FFontIgnoreProperties.Add('Style');
  AStorage.WritePersistent(APath, AProperty as TFont, Recursive,
    TJvAppStorageFontPropertyEngine.FFontIgnoreProperties);
  AStorage.WriteInteger(APath + '\Size', (AProperty as TFont).Size);
  if Index >= 0 then
    TJvAppStorageFontPropertyEngine.FFontIgnoreProperties.Delete(Index);
end;

type
// Modify JvAppStorage handling of TSynGutter
// We want to PPI scale size properties
TJvAppStorageGutterPropertyEngine = class(TJvAppStoragePropertyBaseEngine)
  private
    class var FGutterIgnoreProperties: TStringList;
public
  function Supports(AObject: TObject; AProperty: TObject): Boolean; override;
  procedure ReadProperty(AStorage: TJvCustomAppStorage; const APath: string; AObject: TObject; AProperty: TObject; const Recursive,
    ClearFirst: Boolean; const IgnoreProperties: TStrings = nil); override;
  procedure WriteProperty(AStorage: TJvCustomAppStorage; const APath: string; AObject: TObject; AProperty: TObject; const
    Recursive: Boolean; const IgnoreProperties: TStrings = nil); override;
strict private
  class constructor Create;
  class destructor Destroy;
end;

{ TJvAppStorageGutterPropertyEngine }

function TJvAppStorageGutterPropertyEngine.Supports(AObject,
  AProperty: TObject): Boolean;
begin
  Result := AProperty is TSynGutter;
end;

class constructor TJvAppStorageGutterPropertyEngine.Create;
begin
  FGutterIgnoreProperties := TStringList.Create;
  FGutterIgnoreProperties.Add('Bands');
end;

class destructor TJvAppStorageGutterPropertyEngine.Destroy;
begin
  FGutterIgnoreProperties.Free;
end;

procedure TJvAppStorageGutterPropertyEngine.ReadProperty(
  AStorage: TJvCustomAppStorage; const APath: string; AObject,
  AProperty: TObject; const Recursive, ClearFirst: Boolean;
  const IgnoreProperties: TStrings);
Var
  FontSize : Integer;
begin
  AStorage.ReadPersistent(APath, AProperty as TSynGutter, Recursive, ClearFirst,
    FGutterIgnoreProperties);
  FontSize := TSynGutter(AProperty).Font.Size;
  TSynGutter(AProperty).ChangeScale(Screen.PixelsPerInch, 96);
  TSynGutter(AProperty).Font.Size := FontSize;
end;

procedure TJvAppStorageGutterPropertyEngine.WriteProperty(
  AStorage: TJvCustomAppStorage; const APath: string; AObject,
  AProperty: TObject; const Recursive: Boolean;
  const IgnoreProperties: TStrings);
Var
  FontSize : Integer;
begin
  FontSize := TSynGutter(AProperty).Font.Size;
  TSynGutter(AProperty).ChangeScale(96, Screen.PixelsPerInch);
  TSynGutter(AProperty).Font.Size := FontSize;
  AStorage.StorageOptions.StoreDefaultValues := True;
  AStorage.WritePersistent(APath, AProperty as TSynGutter, Recursive,
    FGutterIgnoreProperties);
  AStorage.StorageOptions.StoreDefaultValues := False;
  TSynGutter(AProperty).ChangeScale(Screen.PixelsPerInch, 96);
  TSynGutter(AProperty).Font.Size := FontSize;
end;

type
TJvAppStorageKeyStrokesPropertyEngine = class(TJvAppStoragePropertyBaseEngine)
public
  function Supports(AObject: TObject; AProperty: TObject): Boolean; override;
  procedure ReadProperty(AStorage: TJvCustomAppStorage; const APath: string; AObject: TObject; AProperty: TObject; const Recursive,
    ClearFirst: Boolean; const IgnoreProperties: TStrings = nil); override;
  procedure WriteProperty(AStorage: TJvCustomAppStorage; const APath: string; AObject: TObject; AProperty: TObject; const
    Recursive: Boolean; const IgnoreProperties: TStrings = nil); override;
end;

{ TJvAppStorageKeyStrokesPropertyEngine }

function TJvAppStorageKeyStrokesPropertyEngine.Supports(AObject,
  AProperty: TObject): Boolean;
begin
  Result := AProperty is TSynEditKeyStrokes;
end;

procedure TJvAppStorageKeyStrokesPropertyEngine.WriteProperty(
  AStorage: TJvCustomAppStorage; const APath: string; AObject,
  AProperty: TObject; const Recursive: Boolean;
  const IgnoreProperties: TStrings);
Var
  KeyStrokes: TSynEditKeyStrokes;

  procedure WriteAddedKeystrokes;
  var
    iDefaultKeys: TSynEditKeyStrokes;
    iAddedKeys: TSynEditKeyStrokes;
    cKey: Integer;
    iKey: TSynEditKeyStroke;
    iAddIndex: Integer;
  begin
    iDefaultKeys := TPyScripterSettings.DefaultEditorKeyStrokes;
    iAddedKeys := TSynEditKeyStrokes.Create(nil);
    try
      for cKey := 0 to Keystrokes.Count -1 do
      begin
        iKey := Keystrokes[cKey];
        iAddIndex := iDefaultKeys.FindShortcut2(iKey.ShortCut, iKey.ShortCut2);
        //if it's not a default keystroke, add it
        if (iAddIndex < 0) or (iDefaultKeys[iAddIndex].Command <> iKey.Command) then
          iAddedKeys.Add.Assign(iKey);
      end;
      AStorage.WriteCollection(APath, iAddedKeys);
    finally
      iAddedKeys.Free;
    end;
  end;

  procedure WriteRemovedKeystrokes;
  var
    iRemovedKeys: TSynEditKeyStrokes;
    cKey: Integer;
    iKey: TSynEditKeyStroke;
    iFoundAt: Integer;
  begin
    iRemovedKeys := TSynEditKeyStrokes.Create(nil);
    try
    iRemovedKeys.Assign(TPyScripterSettings.DefaultEditorKeyStrokes);
      cKey := 0;
      while cKey < iRemovedKeys.Count do
      begin
        iKey := iRemovedKeys[cKey];
        iFoundAt := Keystrokes.FindShortcut2(iKey.ShortCut, iKey.ShortCut2);
        if (iFoundAt >= 0) and (Keystrokes[iFoundAt].Command = iKey.Command) then
          iKey.Free //if exists in Keystrokes, then shouldn't be in "removed" list
        else
          Inc(cKey);
      end;
      AStorage.WriteCollection(APath + '\Removed', iRemovedKeys);
    finally
      iRemovedKeys.Free;
    end;
  end;

begin
  KeyStrokes := AProperty as TSynEditKeyStrokes;
  WriteAddedKeystrokes;
  WriteRemovedKeystrokes;
end;

procedure TJvAppStorageKeyStrokesPropertyEngine.ReadProperty(
  AStorage: TJvCustomAppStorage; const APath: string; AObject,
  AProperty: TObject; const Recursive, ClearFirst: Boolean;
  const IgnoreProperties: TStrings);
Var
  KeyStrokes: TSynEditKeyStrokes;

  procedure ReadAddedKeystrokes;
  var
    iDefaultKeys: TSynEditKeyStrokes;
    iKey: TSynEditKeyStroke;
    cKey: Integer;
    iAddIndex: Integer;
  begin
    AStorage.ReadCollection(APath, KeyStrokes, True);
    iDefaultKeys := TPyScripterSettings.DefaultEditorKeyStrokes;
    for cKey := 0 to iDefaultKeys.Count -1 do
    begin
      iKey := iDefaultKeys[cKey];
      iAddIndex := KeyStrokes.FindShortcut2(iKey.ShortCut, iKey.ShortCut2);
      //if it's not a default keystroke, add it
      if (iAddIndex < 0) or (KeyStrokes[iAddIndex].Command <> iKey.Command) then
        KeyStrokes.Add.Assign(iKey);
    end;
  end;

  procedure ReadRemovedKeystrokes;
  var
    iDelKeys: TSynEditKeyStrokes;
    cKey: Integer;
    iKey: TSynEditKeyStroke;
    iToDelete: Integer;
  begin
    iDelKeys := TSynEditKeyStrokes.Create(nil);
    try
      AStorage.ReadCollection(APath + '\Removed', iDelKeys);
      for cKey := 0 to iDelKeys.Count -1 do
      begin
        iKey := iDelKeys[cKey];
        iToDelete := Keystrokes.FindShortcut2(iKey.ShortCut, iKey.ShortCut2);
        if (iToDelete >= 0) and (Keystrokes[iToDelete].Command = iKey.Command) then
          Keystrokes[iToDelete].Free;
      end;
    finally
      iDelKeys.Free;
    end;
  end;

begin
  KeyStrokes := AProperty as TSynEditKeyStrokes;
  ReadAddedKeystrokes;
  ReadRemovedKeystrokes;
end;

{$ENDREGION 'AppStorage handlers' }


{ TPyScripterSettings }

class procedure TPyScripterSettings.CreateIDEOptions;
begin
  PyIDEOptions := TPythonIDEOptions.Create;
  // Register AppStorage handlers
  RegisterAppStoragePropertyEngine(TJvAppStorageFontPropertyEngine);
  RegisterAppStoragePropertyEngine(TJvAppStorageGutterPropertyEngine);
  RegisterAppStoragePropertyEngine(TJvAppStorageKeyStrokesPropertyEngine);
end;

class constructor TPyScripterSettings.CreateSettings;

  procedure CopyFileIfNeeded(const Source, Dest: string);
  begin
    if not FileExists(Dest) and FileExists(Source) then
    try
       TFile.Copy(Source, Dest);
    except
      on E:Exception do
        raise Exception.CreateFmt(_('Error in copying file "%s" to "%s"'
          + SLineBreak + 'Error Message: '), [Source, Dest, E.Message]);
    end;
  end;

var
  PublicPath: string;
begin
  OptionsFileName := ChangeFileExt(Application.ExeName, '.ini');
  IsPortable := FileExists(OptionsFileName);
  if IsPortable then begin
    // Portable version - nothing is stored in other directories
    UserDataPath :=   ExtractFilePath(Application.ExeName);
    ColorThemesFilesDir := TPath.Combine(UserDataPath, 'Highlighters');
    StylesFilesDir := TPath.Combine(UserDataPath, 'Styles');
    LspServerPath :=  TPath.Combine(UserDataPath, 'Lib\Lsp');
  end else begin
    UserDataPath := TPath.Combine(GetHomePath,  'PyScripter\');
    OptionsFileName := TPath.Combine(UserDataPath, 'PyScripter.ini');
    if not ForceDirectories(UserDataPath) then
      StyledMessageDlg(Format(SAccessAppDataDir, [UserDataPath]),
      mtWarning, [mbOK], 0);
    PublicPath := TPath.Combine(TPath.GetPublicPath, 'PyScripter\');
    ColorThemesFilesDir := TPath.Combine(PublicPath, 'Highlighters');
    StylesFilesDir := TPath.Combine(PublicPath, 'Styles');
    LspServerPath :=  TPath.Combine(PublicPath, 'Lsp');
    // First use setup
    CopyFileIfNeeded(TPath.Combine(PublicPath, 'PyScripter.ini'), OptionsFileName);
  end;
  EngineInitFile := TPath.Combine(UserDataPath, 'python_init.py');
  PyScripterInitFile := TPath.Combine(UserDataPath, 'pyscripter_init.py');
  if not IsPortable then begin
    // First use setup
    CopyFileIfNeeded(TPath.Combine(PublicPath, 'python_init.py'), EngineInitFile);
    CopyFileIfNeeded(TPath.Combine(PublicPath, 'pyscripter_init.py'), PyScripterInitFile);
  end;

  TPyScripterSettings.CreateIDEOptions;
  TPyScripterSettings.CreateEditorOptions;
  // Save Default editor keystrokes
  TPyScripterSettings.DefaultEditorKeyStrokes := TSynEditKeyStrokes.Create(nil);
  TPyScripterSettings.DefaultEditorKeyStrokes.Assign(EditorOPtions.Keystrokes);
end;

class procedure TPyScripterSettings.CreateEditorOptions;
begin
  EditorOptions := TSynEditorOptionsContainer.Create(nil);
  with EditorOptions do begin
    Font.Name := DefaultCodeFontName;
    Gutter.Font.Name := Font.Name;
    Gutter.Font.Color := clGrayText;
    Gutter.Gradient := False;
    Gutter.DigitCount := 2;
    Gutter.ShowLineNumbers := True;
    Gutter.Autosize := True;
    Gutter.ChangeScale(Screen.PixelsPerInch, 96);
    Font.Size := 10;
    Gutter.Font.Size := 9;

    Options := [eoDragDropEditing, eoEnhanceHomeKey,
                eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoKeepCaretX,
                eoShowScrollHint, eoSmartTabDelete, eoTabsToSpaces, eoTabIndent,
                eoTrimTrailingSpaces, eoAutoIndent];
    WantTabs := True;
    TabWidth := 4;
    MaxUndo := 0;
    // Scale BookmarkOptions
    BookMarkOptions.ChangeScale(Screen.PixelsPerInch, 96);

    TPyScripterSettings.RegisterEditorUserCommands(EditorOptions.Keystrokes);
  end;
  InterpreterEditorOptions := TSynEditorOptionsContainer.Create(nil);
  InterpreterEditorOptions.Assign(EditorOptions);
  with InterpreterEditorOptions do begin
    Options := Options - [eoTrimTrailingSpaces, eoScrollPastEol];
    WordWrap := True;
    Gutter.Visible := False;
    RightEdge := 0;
  end;
end;

class destructor TPyScripterSettings.Destroy;
begin
  PyIDEOptions.Free;
  EditorOptions.Free;
  InterpreterEditorOptions.Free;
  TPyScripterSettings.DefaultEditorKeyStrokes.Free;
end;

class procedure TPyScripterSettings.RegisterEditorUserCommands(
  Keystrokes: TSynEditKeyStrokes);
// Register User Commands and shortcuts
begin
  with Keystrokes do begin
    AddKey(ecCodeCompletion, VK_SPACE, [ssCtrl]);
    AddKey(ecParamCompletion, VK_SPACE, [ssCtrl, ssShift]);
    AddKey(ecSelMatchBracket  , 221, [ssCtrl, ssShift]);
    // 221 code for ]
    // Visual studio shortcut for Match Bracket
    Delete(FindCommand(ecMatchBracket));
    AddKey(ecMatchBracket, 221, [ssCtrl]);
    // #869
    Delete(FindCommand(ecDeleteLine));
    AddKey(ecDeleteLine, Ord('D'), [ssCtrl, ssShift]);
  end;
end;

end.
