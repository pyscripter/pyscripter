{-----------------------------------------------------------------------------
 Unit Name: cPyScripterSettings
 Author:    Pyscripter
 Date:      22-Jan-2018
 Purpose:   Centralize the management of PyScripter settings
 History:
-----------------------------------------------------------------------------}

unit cPyScripterSettings;

interface
uses
  System.Classes,
  System.Messaging,
  Vcl.Graphics,
  SpTBXTabs,
  SynEditTypes,
  SynEditCodeFolding,
  SynEditMiscClasses,
  SynEditKeyCmds,
  SynEdit,
  WrapDelphi,
  uEditAppIntfs,
  cPySupportTypes;

const
  dsaSearchFromStart = 1;
  dsaReplaceFromStart = 2;
  dsaReplaceNumber = 3;
  dsaSearchStartReached = 4;
  dsaPostMortemInfo = 5;
  dsaDictonaryNA = 6;

type
  TFileChangeNotificationType = (fcnFull, fcnNoMappedDrives, fcnDisabled);
  TIDEOptionsChangedMessage = class(System.Messaging.TMessage);

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
    FFreeNotifyImpl: IFreeNotification;
    FTimeOut: Integer;
    FUndoAfterSave: Boolean;
    FSaveFilesBeforeRun: Boolean;
    FSaveEnvironmentBeforeRun: Boolean;
    FRestoreOpenFiles: Boolean;
    FRestoreOpenProject: Boolean;
    FCreateBackupFiles: Boolean;
    FExplorerInitiallyExpanded: Boolean;
    FProjectExplorerInitiallyExpanded: Boolean;
    FSearchTextAtCaret: Boolean;
    FPythonFileFilter: string;
    FCythonFileFilter: string;
    FHTMLFileFilter: string;
    FXMLFileFilter: string;
    FCSSFileFilter: string;
    FCPPFileFilter: string;
    FYAMLFileFilter: string;
    FJSFileFilter: string;
    FPHPFileFilter: string;
    FJSONFileFilter: string;
    FGeneralFileFilter: string;
    FFileExplorerFilter: string;
    FAutoCheckForUpdates: Boolean;
    FDaysBetweenChecks: Integer;
    FMaskFPUExceptions: Boolean;
    FSpecialPackages: string;
    FShowCodeHints: Boolean;
    FShowDebuggerHints: Boolean;
    FAutoCompleteBrackets: Boolean;
    FMarkExecutableLines: Boolean;
    FCheckSyntaxAsYouType: Boolean;
    FFileExplorerContextMenu: Boolean;
    FNewFileLineBreaks: TSynEditFileFormat;
    FNewFileEncoding: TFileSaveFormat;
    FDetectUTF8Encoding: Boolean;
    FEditorsTabPosition: TSpTBXTabPosition;
    FPythonEngineType: TPythonEngineType;
    FPrettyPrintOutput: Boolean;
    FSmartNextPrevPage: Boolean;
    FAutoReloadChangedFiles: Boolean;
    FClearOutputBeforeRun: Boolean;
    FAutoHideFindToolbar: Boolean;
    FCodeCompletionListSize: Integer;
    FEditorCodeCompletion: Boolean;
    FInterpreterCodeCompletion: Boolean;
    FShowTabCloseButton: Boolean;
    FPostMortemOnException: Boolean;
    FDockAnimationInterval: Integer;
    FDockAnimationMoveWidth: Integer;
    FInterpreterHistorySize: Integer;
    FSaveInterpreterHistory: Boolean;
    FReinitializeBeforeRun: Boolean;
    FJumpToErrorOnException: Boolean;
    FFileTemplateForNewScripts: string;
    FAutoCompletionFont: TFont;
    FHighlightSelectedWord: Boolean;
    FHighlightSelectedWordColor: TColor;
    FFileChangeNotification: TFileChangeNotificationType;
    FCodeCompletionCaseSensitive: Boolean;
    FCompleteKeywords: Boolean;
    FCompleteAsYouType: Boolean;
    FCompleteWithWordBreakChars: Boolean;
    FCompleteWithOneEntry:Boolean;
    FDisplayPackageNames:Boolean;
    FNoOfRecentFiles: Integer;
    FCodeFoldingEnabled: Boolean;
    FCodeFolding: TSynCodeFolding;
    FInternalInterpreterHidden: Boolean;
    FCompactLineNumbers: Boolean;
    FStyleMainWindowBorder: Boolean;
    FFileExplorerBackgroundProcessing: Boolean;
    FSSHCommand: string;
    FSSHOptions: string;
    FScpCommand: string;
    FScpOptions: string;
    FSSHDisableVariablesWin: Boolean;
    FAlwaysUseSockets: Boolean;
    FTrimTrailingSpacesOnSave: Boolean;
    FTraceOnlyIntoOpenFiles: Boolean;
    FLspDebug: Boolean;
    FDictLanguage: string;
    FSpellCheckedTokens: string;
    FSpellCheckAsYouType: Boolean;
    FAutoRestart: Boolean;
    FLoggingEnabled: Boolean;
    FScrollbarAnnotation: Boolean;
    FAccessibilitySupport: Boolean;
    FUIContentFontSize: Integer;
    FPreferFreeThreaded: Boolean;
    FDiagnosticsOnOpen: Boolean;
    FDiagnosticsOnSave: Boolean;
    FTrackChanges: TSynTrackChanges;
    FSelectionColor: TSynSelectedColor;
    FIndentGuides: TSynIndentGuides;
    FDisplayFlowControl: TSynDisplayFlowControl;
    function GetPythonFileExtensions: string;
    procedure SetAutoCompletionFont(const Value: TFont);
    procedure SetCodeFolding(const Value: TSynCodeFolding);
    procedure SetTrackChanges(const Value: TSynTrackChanges);
    procedure SetSelectionColor(const Value: TSynSelectedColor);
    procedure SetIndentGuides(const Value: TSynIndentGuides);
    procedure SetDisplayFlowControl(const Value: TSynDisplayFlowControl);
  protected
    property FreeNotifyImpl: IFreeNotification read FFreeNotifyImpl implements IFreeNotification;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Changed;
    property PythonFileExtensions: string read GetPythonFileExtensions;
  published
    property CodeFolding: TSynCodeFolding read FCodeFolding
      write SetCodeFolding;
    property TrackChanges: TSynTrackChanges read FTrackChanges
      write SetTrackChanges;
    property SelectionColor: TSynSelectedColor read FSelectionColor
      write SetSelectionColor;
    property IndentGuides: TSynIndentGuides read FIndentGuides
      write SetIndentGuides;
    property DisplayFlowControl: TSynDisplayFlowControl read FDisplayFlowControl
      write SetDisplayFlowControl;
    property TimeOut: Integer read FTimeOut write FTimeOut default 0;
    property UndoAfterSave: Boolean read FUndoAfterSave
      write FUndoAfterSave default True;
    property SaveFilesBeforeRun: Boolean read FSaveFilesBeforeRun
      write FSaveFilesBeforeRun default True;
    property SaveEnvironmentBeforeRun: Boolean read FSaveEnvironmentBeforeRun
      write FSaveEnvironmentBeforeRun default False;
    property RestoreOpenFiles: Boolean read FRestoreOpenFiles
      write FRestoreOpenFiles default True;
    property RestoreOpenProject: Boolean read FRestoreOpenProject
      write FRestoreOpenProject default True;
    property CreateBackupFiles: Boolean read FCreateBackupFiles
      write FCreateBackupFiles default False;
    property ExplorerInitiallyExpanded: Boolean read FExplorerInitiallyExpanded
      write FExplorerInitiallyExpanded default False;
    property ProjectExplorerInitiallyExpanded: Boolean read FProjectExplorerInitiallyExpanded
      write FProjectExplorerInitiallyExpanded default True;
    property SearchTextAtCaret: Boolean read FSearchTextAtCaret
      write FSearchTextAtCaret stored False;
    property PythonFileFilter: string read FPythonFileFilter
      write FPythonFileFilter;
    property CythonFileFilter: string read FCythonFileFilter
      write FCythonFileFilter;
    property HTMLFileFilter: string read FHTMLFileFilter
      write FHTMLFileFilter;
    property XMLFileFilter: string read FXMLFileFilter
      write FXMLFileFilter;
    property CSSFileFilter: string read FCSSFileFilter
      write FCSSFileFilter;
    property CPPFileFilter: string read FCPPFileFilter
      write FCPPFileFilter;
    property YAMLFileFilter: string read FYAMLFileFilter
      write FYAMLFileFilter;
    property JSFileFilter: string read FJSFileFilter
      write FJSFileFilter;
    property PHPFileFilter: string read FPHPFileFilter
      write FPHPFileFilter;
    property JSONFileFilter: string read FJSONFileFilter
      write FJSONFileFilter;
    property GeneralFileFilter: string read FGeneralFileFilter
      write FGeneralFileFilter;
    property FileExplorerFilter: string read FFileExplorerFilter
      write FFileExplorerFilter;
    property AutoCheckForUpdates: Boolean read FAutoCheckForUpdates
      write FAutoCheckForUpdates default True;
    property DaysBetweenChecks: Integer read FDaysBetweenChecks
      write FDaysBetweenChecks default 7;
    property MaskFPUExceptions: Boolean read FMaskFPUExceptions
      write FMaskFPUExceptions default True;
    property SpecialPackages: string read FSpecialPackages write FSpecialPackages;
    property ShowCodeHints: Boolean read FShowCodeHints
      write FShowCodeHints default True;
    property ShowDebuggerHints: Boolean read FShowDebuggerHints
      write FShowDebuggerHints default True;
    property AutoCompleteBrackets: Boolean read FAutoCompleteBrackets
      write FAutoCompleteBrackets default True;
    property MarkExecutableLines: Boolean read FMarkExecutableLines
      write FMarkExecutableLines default False;
    property CheckSyntaxAsYouType: Boolean read FCheckSyntaxAsYouType
      write FCheckSyntaxAsYouType default False;
    property FileExplorerContextMenu: Boolean read FFileExplorerContextMenu
      write FFileExplorerContextMenu default True;
    property NewFileLineBreaks: TSynEditFileFormat read FNewFileLineBreaks
      write FNewFileLineBreaks default sffDos;
    property NewFileEncoding: TFileSaveFormat read FNewFileEncoding
      write FNewFileEncoding default sf_UTF8_NoBOM;
    property DetectUTF8Encoding: Boolean read FDetectUTF8Encoding
      write FDetectUTF8Encoding default True;
    property EditorsTabPosition: TSpTBXTabPosition read FEditorsTabPosition
      write FEditorsTabPosition default ttpBottom;
    property PythonEngineType: TPythonEngineType read FPythonEngineType
      write FPythonEngineType default peRemote;
    property PrettyPrintOutput: Boolean read FPrettyPrintOutput
      write FPrettyPrintOutput default True;
    property SmartNextPrevPage: Boolean read FSmartNextPrevPage
      write FSmartNextPrevPage default True;
    property AutoReloadChangedFiles: Boolean read FAutoReloadChangedFiles
      write FAutoReloadChangedFiles default True;
    property ClearOutputBeforeRun: Boolean read FClearOutputBeforeRun
      write FClearOutputBeforeRun default False;
    property AutoHideFindToolbar: Boolean read FAutoHideFindToolbar
      write FAutoHideFindToolbar default False;
    property EditorCodeCompletion: Boolean read FEditorCodeCompletion
      write FEditorCodeCompletion default True;
    property InterpreterCodeCompletion: Boolean read FInterpreterCodeCompletion
      write FInterpreterCodeCompletion default True;
    property CodeCompletionListSize: Integer read FCodeCompletionListSize
      write FCodeCompletionListSize default 10;
    property ShowTabCloseButton: Boolean read FShowTabCloseButton
      write FShowTabCloseButton default True;
    property PostMortemOnException: Boolean read FPostMortemOnException
      write FPostMortemOnException default False;
    property DockAnimationInterval: Integer read FDockAnimationInterval
      write FDockAnimationInterval;
    property DockAnimationMoveWidth: Integer read FDockAnimationMoveWidth
      write FDockAnimationMoveWidth;
    property InterpreterHistorySize: Integer read FInterpreterHistorySize
      write FInterpreterHistorySize;
    property SaveInterpreterHistory: Boolean read FSaveInterpreterHistory
      write FSaveInterpreterHistory default True;
    property ReinitializeBeforeRun: Boolean read FReinitializeBeforeRun
      write FReinitializeBeforeRun default True;
    property JumpToErrorOnException: Boolean read FJumpToErrorOnException
      write FJumpToErrorOnException default True;
    property FileTemplateForNewScripts: string read FFileTemplateForNewScripts
      write FFileTemplateForNewScripts;
    property AutoCompletionFont: TFont read FAutoCompletionFont
      write SetAutoCompletionFont;
    property HighlightSelectedWord: Boolean read FHighlightSelectedWord
      write FHighlightSelectedWord default True;
    property HighlightSelectedWordColor: TColor read FHighlightSelectedWordColor
      write FHighlightSelectedWordColor default clOlive;
    property FileChangeNotification: TFileChangeNotificationType read FFileChangeNotification
      write FFileChangeNotification default fcnDisabled;
    property CodeCompletionCaseSensitive: Boolean read FCodeCompletionCaseSensitive
      write FCodeCompletionCaseSensitive default True;
    property CompleteKeywords: Boolean read FCompleteKeywords
      write FCompleteKeywords default True;
    property CompleteAsYouType: Boolean read FCompleteAsYouType
      write FCompleteAsYouType default True;
    property CompleteWithWordBreakChars: Boolean read FCompleteWithWordBreakChars
      write FCompleteWithWordBreakChars default False;
    property CompleteWithOneEntry: Boolean read FCompleteWithOneEntry
      write FCompleteWithOneEntry default False;
    property DisplayPackageNames: Boolean read FDisplayPackageNames
      write FDisplayPackageNames default True;
    property NoOfRecentFiles: Integer read FNoOfRecentFiles
      write FNoOfRecentFiles default 12;
    property CodeFoldingEnabled: Boolean read FCodeFoldingEnabled
      write FCodeFoldingEnabled default True;
    property InternalInterpreterHidden: Boolean read FInternalInterpreterHidden
      write FInternalInterpreterHidden default True;
    property CompactLineNumbers: Boolean read FCompactLineNumbers
      write FCompactLineNumbers default True;
    property StyleMainWindowBorder: Boolean read FStyleMainWindowBorder
      write FStyleMainWindowBorder default False;
    property FileExplorerBackgroundProcessing: Boolean read FFileExplorerBackgroundProcessing
      write FFileExplorerBackgroundProcessing default False;
    property SSHCommand: string read FSSHCommand write FSSHCommand;
    property SSHOptions: string read FSSHOptions write FSSHOptions;
    property ScpCommand: string read FScpCommand write FScpCommand;
     property ScpOptions: string read FScpOptions write FScpOptions;
    property SSHDisableVariablesWin: Boolean read FSSHDisableVariablesWin
      write FSSHDisableVariablesWin default True;
    property AlwaysUseSockets: Boolean read FAlwaysUseSockets
      write FAlwaysUseSockets default False;
    property TrimTrailingSpacesOnSave: Boolean read FTrimTrailingSpacesOnSave
      write FTrimTrailingSpacesOnSave default True;
    property TraceOnlyIntoOpenFiles: Boolean read FTraceOnlyIntoOpenFiles
      write FTraceOnlyIntoOpenFiles default False;
    property LspDebug: Boolean read FLspDebug write FLspDebug default False;
    property DictLanguage: string read FDictLanguage write FDictLanguage;
    property SpellCheckedTokens: string read FSpellCheckedTokens write FSpellCheckedTokens;
    property SpellCheckAsYouType: Boolean read FSpellCheckAsYouType
      write FSpellCheckAsYouType default False;
    property AutoRestart: Boolean read FAutoRestart write FAutoRestart default True;
    property LoggingEnabled: Boolean read FLoggingEnabled write FLoggingEnabled default False;
    property ScrollbarAnnotation: Boolean read FScrollbarAnnotation write FScrollbarAnnotation default True;
    property AccessibilitySupport: Boolean read FAccessibilitySupport write FAccessibilitySupport default True;
    property UIContentFontSize: Integer read FUIContentFontSize write FUIContentFontSize default 9;
    property PreferFreeThreaded: Boolean read FPreferFreeThreaded write FPreferFreeThreaded default False;
    property DiagnosticsOnOpen: Boolean read FDiagnosticsOnOpen write FDiagnosticsOnOpen default True;
    property DiagnosticsOnSave: Boolean read FDiagnosticsOnSave write FDiagnosticsOnSave default True;
  end;
{$METHODINFO OFF}

  TSearchCaseSensitiveType = (scsAuto, scsNotCaseSenitive, scsCaseSensitive);

  TEditorSearchOptions = class(TPersistent)
  private
    FSearchCaseSensitiveType: TSearchCaseSensitiveType;
    FSearchFromCaret: Boolean;
    FSearchSelectionOnly: Boolean;
    FSearchTextAtCaret: Boolean;
    FSearchWholeWords: Boolean;
    FUseRegExp: Boolean;
    FIncrementalSearch: Boolean;
    FSearchTextHistory: string;
    FReplaceTextHistory: string;
  public
    SearchText: string;
    ReplaceText: string;
    TempSearchFromCaret: Boolean;
    TempSelectionOnly: Boolean;
    NoReplaceCount: Integer;
    LastReplaceAction: TSynReplaceAction;
    CanWrapSearch: Boolean;
    WrappedSearch: Boolean;
    BackwardSearch: Boolean;
    InitCaretXY: TBufferCoord;
    InterpreterIsSearchTarget: Boolean;
    SelStorage: TSynSelStorage;
    procedure Assign(Source: TPersistent); override;
    procedure InitSearch;
    procedure NewSearch(SynEdit: TCustomSynEdit; ABackwards: Boolean);
  published
    property SearchTextHistory: string read FSearchTextHistory write FSearchTextHistory;
    property ReplaceTextHistory: string read FReplaceTextHistory write FReplaceTextHistory;
    property SearchSelectionOnly: Boolean read FSearchSelectionOnly write FSearchSelectionOnly;
    property SearchCaseSensitiveType: TSearchCaseSensitiveType
      read FSearchCaseSensitiveType write FSearchCaseSensitiveType;
    property SearchFromCaret: Boolean read FSearchFromCaret write FSearchFromCaret;
    property SearchTextAtCaret: Boolean read FSearchTextAtCaret write FSearchTextAtCaret;
    property SearchWholeWords: Boolean read FSearchWholeWords write FSearchWholeWords;
    property UseRegExp: Boolean read FUseRegExp write FUseRegExp;
    property IncrementalSearch: Boolean read FIncrementalSearch write FIncrementalSearch;
  end;

  TPyScripterSettings = class
  private
    class procedure ApplyPyIDEOptions;
  public
    class var IsPortable: Boolean;
    class var UserDataPath: string;
    class var OptionsFileName: string;
    class var ColorThemesFilesDir: string;
    class var StylesFilesDir: string;
    class var UserDebugInspectorsDir: string;
    class var AppDebugInspectorsDir: string;
    class var LspServerPath: string;
    class var RecoveryDir: string;
    class var EngineInitFile: string;
    class var PyScripterInitFile: string;
    class var PyScripterLogFile: string;
    class var DefaultEditorKeyStrokes: TSynEditKeyStrokes;
    class procedure RegisterEditorUserCommands(Keystrokes: TSynEditKeyStrokes);
    class procedure CreateIDEOptions;
    class procedure CreateEditorOptions;
    class procedure CreateSearchOptions;
    class constructor CreateSettings;
    class destructor Destroy;
  end;

const
  // Additional Synedit commands
  ecRecallCommandPrev = ecUserFirst + 100;
  ecRecallCommandNext = ecUserFirst + 101;
  ecRecallCommandEsc = ecUserFirst + 102;
  ecCodeCompletion = ecUserFirst + 103;
  ecParamCompletion = ecUserFirst + 104;

var
  PyIDEOptions: TPythonIDEOptions;
  EditorOptions: TSynEditorOptionsContainer;
  EditorSearchOptions: TEditorSearchOptions;

implementation

uses
  Winapi.Windows,
  System.UITypes,
  System.SysUtils,
  System.IOUtils,
  System.Math,
  Vcl.Forms,
  Vcl.Dialogs,
  uHighlighterProcs,
  JvAppStorage,
  JvGnugettext,
  SynUnicode,
  SynEditRegexSearch,
  StringResources,
  uCommonFunctions,
  cAppPaths;

{ TPythonIDEOptions }

procedure TPythonIDEOptions.Assign(Source: TPersistent);
begin
  if Source is TPythonIDEOptions then
    with TPythonIDEOptions(Source) do begin
      Self.FCodeFolding.Assign(CodeFolding);
      Self.FTrackChanges.Assign(TrackChanges);
      Self.FSelectionColor.Assign(SelectionColor);
      Self.FIndentGuides.Assign(IndentGuides);
      Self.FDisplayFlowControl.Assign(DisplayFlowControl);
      Self.FTimeOut := TimeOut;
      Self.FUndoAfterSave := UndoAfterSave;
      Self.FSaveFilesBeforeRun := SaveFilesBeforeRun;
      Self.FSaveEnvironmentBeforeRun := SaveEnvironmentBeforeRun;
      Self.FRestoreOpenFiles := FRestoreOpenFiles;
      Self.FRestoreOpenProject := FRestoreOpenProject;
      Self.FCreateBackupFiles := CreateBackupFiles;
      Self.FExplorerInitiallyExpanded := ExplorerInitiallyExpanded;
      Self.FProjectExplorerInitiallyExpanded := ProjectExplorerInitiallyExpanded;
      Self.FSearchTextAtCaret := SearchTextAtCaret;
      Self.FPythonFileFilter := PythonFileFilter;
      Self.FCythonFileFilter := CythonFileFilter;
      Self.FHTMLFileFilter := HTMLFileFilter;
      Self.FXMLFileFilter := XMLFileFilter;
      Self.FCSSFileFilter := CSSFileFilter;
      Self.FCPPFileFilter := CPPFileFilter;
      Self.FYAMLFileFilter := YAMLFileFilter;
      Self.FJSFileFilter := JSFileFilter;
      Self.FPHPFileFilter := PHPFileFilter;
      Self.FJSONFileFilter := JSONFileFilter;
      Self.FGeneralFileFilter := GeneralFileFilter;
      Self.FFileExplorerFilter := FileExplorerFilter;
      Self.FAutoCheckForUpdates := AutoCheckForUpdates;
      Self.FDaysBetweenChecks := DaysBetweenChecks;
      Self.FMaskFPUExceptions := MaskFPUExceptions;
      Self.FSpecialPackages := SpecialPackages;
      Self.FShowCodeHints := ShowCodeHints;
      Self.FShowDebuggerHints := ShowDebuggerHints;
      Self.FAutoCompleteBrackets := AutoCompleteBrackets;
      Self.FMarkExecutableLines := MarkExecutableLines;
      Self.FCheckSyntaxAsYouType := CheckSyntaxAsYouType;
      Self.FFileExplorerContextMenu := FileExplorerContextMenu;
      Self.FNewFileLineBreaks := NewFileLineBreaks;
      Self.FNewFileEncoding := NewFileEncoding;
      Self.FDetectUTF8Encoding := DetectUTF8Encoding;
      Self.FEditorsTabPosition := EditorsTabPosition;
      Self.FPythonEngineType := PythonEngineType;
      Self.FPrettyPrintOutput := PrettyPrintOutput;
      Self.FSmartNextPrevPage := SmartNextPrevPage;
      Self.FAutoReloadChangedFiles := AutoReloadChangedFiles;
      Self.FClearOutputBeforeRun := ClearOutputBeforeRun;
      Self.FAutoHideFindToolbar := AutoHideFindToolbar;
      Self.FEditorCodeCompletion := EditorCodeCompletion;
      Self.FInterpreterCodeCompletion := InterpreterCodeCompletion;
      Self.FCodeCompletionListSize := CodeCompletionListSize;
      Self.FShowTabCloseButton := ShowTabCloseButton;
      Self.FPostMortemOnException := PostMortemOnException;
      Self.FDockAnimationInterval := DockAnimationInterval;
      Self.FDockAnimationMoveWidth := DockAnimationMoveWidth;
      Self.FInterpreterHistorySize := InterpreterHistorySize;
      Self.FSaveInterpreterHistory := SaveInterpreterHistory;
      Self.FReinitializeBeforeRun := ReinitializeBeforeRun;
      Self.FJumpToErrorOnException := JumpToErrorOnException;
      Self.FFileTemplateForNewScripts := FileTemplateForNewScripts;
      Self.FAutoCompletionFont.Assign(AutoCompletionFont);
      Self.FHighlightSelectedWord := HighlightSelectedWord;
      Self.FHighlightSelectedWordColor := HighlightSelectedWordColor;
      Self.FFileChangeNotification := FileChangeNotification;
      Self.FCodeCompletionCaseSensitive := CodeCompletionCaseSensitive;
      Self.FCompleteKeywords := CompleteKeywords;
      Self.FCompleteAsYouType := CompleteAsYouType;
      Self.FCompleteWithWordBreakChars := CompleteWithWordBreakChars;
      Self.FCompleteWithOneEntry := CompleteWithOneEntry;
      Self.FDisplayPackageNames := DisplayPackageNames;
      Self.FNoOfRecentFiles := NoOfRecentFiles;
      Self.FCodeFoldingEnabled := CodeFoldingEnabled;
      Self.FInternalInterpreterHidden := InternalInterpreterHidden;
      Self.FCompactLineNumbers := CompactLineNumbers;
      Self.FStyleMainWindowBorder := StyleMainWindowBorder;
      Self.FFileExplorerBackgroundProcessing := FileExplorerBackgroundProcessing;
      Self.FSSHCommand := SSHCommand;
      Self.FSSHOptions := SSHOptions;
      Self.FScpCommand := ScpCommand;
      Self.FScpOptions := ScpOptions;
      Self.FSSHDisableVariablesWin := SSHDisableVariablesWin;
      Self.FAlwaysUseSockets := AlwaysUseSockets;
      Self.FTrimTrailingSpacesOnSave := TrimTrailingSpacesOnSave;
      Self.FTraceOnlyIntoOpenFiles := TraceOnlyIntoOpenFiles;
      Self.FLspDebug := LspDebug;
      Self.FDictLanguage := DictLanguage;
      Self.FSpellCheckedTokens := SpellCheckedTokens;
      Self.FSpellCheckAsYouType := SpellCheckAsYouType;
      Self.FAutoRestart := AutoRestart;
      Self.FLoggingEnabled := LoggingEnabled;
      Self.FScrollbarAnnotation := ScrollbarAnnotation;
      Self.FAccessibilitySupport := AccessibilitySupport;
      Self.FUIContentFontSize := UIContentFontSize;
      Self.FPreferFreeThreaded := PreferFreeThreaded;
      Self.FDiagnosticsOnOpen := DiagnosticsOnOpen;
      Self.FDiagnosticsOnSave := DiagnosticsOnSave;
    end
  else
    inherited;
end;

procedure TPythonIDEOptions.Changed;
begin
  TPyScripterSettings.ApplyPyIDEOptions;
  TMessageManager.DefaultManager.SendMessage(Self,
    TIDEOptionsChangedMessage.Create);
end;

constructor TPythonIDEOptions.Create;
begin
  FFreeNotifyImpl := TFreeNotificationImpl.Create(Self);

  FTimeOut := 0; // 5000;
  FUndoAfterSave := True;
  FSaveFilesBeforeRun := True;
  FSaveEnvironmentBeforeRun := False;
  FCreateBackupFiles := False;
  FExplorerInitiallyExpanded := False;
  FProjectExplorerInitiallyExpanded := True;
  FPythonFileFilter := _(sPythonFileFilter);
  FCythonFileFilter := _(sCythonFileFilter);
  FHTMLFileFilter := _(sHTMLFileFilter);
  FXMLFileFilter := _(sXMLFileFilter);
  FCSSFileFilter := _(sCSSFileFilter);
  FCPPFileFilter := _(sCPPFileFilter);
  FYAMLFileFilter := _(sYAMLFileFilter);
  FJSFileFilter := _(sJSFileFilter);
  FPHPFileFilter := _(sPHPFileFilter);
  FJSONFileFilter := _(sJSONFileFilter);
  FGeneralFileFilter := _(sGeneralFileFilter);
  FFileExplorerFilter := '*.py;*.pyw';
  FSearchTextAtCaret := True;
  FRestoreOpenFiles := True;
  FRestoreOpenProject := True;
  FAutoCheckForUpdates := True;
  FDaysBetweenChecks := 7;
  FMaskFPUExceptions := True;
  FSpecialPackages := 'os, numpy, pandas';
  FShowCodeHints := True;
  FShowDebuggerHints := True;
  FAutoCompleteBrackets := True;
  FMarkExecutableLines := False;
  FCheckSyntaxAsYouType := False;
  FFileExplorerContextMenu := True;
  FNewFileLineBreaks := sffDos;
  FNewFileEncoding := sf_UTF8_NoBOM;
  FDetectUTF8Encoding := True;
  FEditorsTabPosition := ttpBottom;
  FPythonEngineType := peRemote;
  FPrettyPrintOutput := True;
  FSmartNextPrevPage := True;
  FAutoReloadChangedFiles := True;
  FClearOutputBeforeRun := False;
  FAutoHideFindToolbar := False;
  FEditorCodeCompletion := True;
  FInterpreterCodeCompletion := True;
  FCodeCompletionListSize := 10;
  FShowTabCloseButton := True;
  FPostMortemOnException := False;
  FDockAnimationInterval := 20;
  FDockAnimationMoveWidth := 40;
  FInterpreterHistorySize := 50;
  FSaveInterpreterHistory := True;
  FReinitializeBeforeRun := True;
  FJumpToErrorOnException := True;
  FFileTemplateForNewScripts := _(SPythonTemplateName);
  FAutoCompletionFont := TFont.Create;
  SetDefaultUIFont(FAutoCompletionFont);
  FHighlightSelectedWord := True;
  FHighlightSelectedWordColor := TColors.Dodgerblue;
  FFileChangeNotification := fcnDisabled;
  FCodeCompletionCaseSensitive := True;
  FCompleteKeywords := True;
  FCompleteAsYouType := True;
  FCompleteWithWordBreakChars := False;
  FCompleteWithOneEntry := False;
  FDisplayPackageNames := True;
  FNoOfRecentFiles := 12;
  FCodeFoldingEnabled := True;
  FInternalInterpreterHidden := True;
  FCompactLineNumbers := True;
  FStyleMainWindowBorder := False;
  FFileExplorerBackgroundProcessing := False;
  FSSHCommand := 'ssh';
  FSSHOptions := '-o PasswordAuthentication=no -o StrictHostKeyChecking=no';
  FScpCommand := 'scp';
  FScpOptions := '-T -o PasswordAuthentication=no -o StrictHostKeyChecking=no';
  // https://pyscripter.blogspot.com/2024/11/ssh-engines-broken-by-november-12-2024.html
  if (TOSVersion.Major > 10) or (TOSVersion.Major = 10) and (TOSVersion.Build >= 26100) then
    FScpOptions := '-O ' + FScpOptions;
  FSSHDisableVariablesWin := True;
  FAlwaysUseSockets := True;
  FTrimTrailingSpacesOnSave := True;
  FTraceOnlyIntoOpenFiles := False;
  FLspDebug := False;
  FDictLanguage := UserLocaleName;
  FSpellCheckedTokens := 'Comment, Text, String, Multi-Line String, Documentation';
  FSpellCheckAsYouType := False;
  FAutoRestart := True;
  FLoggingEnabled := False;
  FScrollbarAnnotation := True;
  FAccessibilitySupport := True;
  FDiagnosticsOnOpen := True;
  FDiagnosticsOnSave := True;
  FUIContentFontSize := 9;
  FCodeFolding := TSynCodeFolding.Create;
  FCodeFolding.GutterShapeSize := 9;  // default value
  FTrackChanges := TSynTrackChanges.Create(nil);
  FTrackChanges.Visible := True;
  FTrackChanges.Width := 3;
  FSelectionColor := TSynSelectedColor.Create;
  FSelectionColor.FillWholeLines := False;
  FSelectionColor.Background := TColors.Dodgerblue;
  FIndentGuides := TSynIndentGuides.Create;
  FDisplayFlowControl := TSynDisplayFlowControl.Create;
end;

destructor TPythonIDEOptions.Destroy;
begin
  FreeAndNil(FAutoCompletionFont);
  FreeAndNil(FCodeFolding);
  FreeAndNil(FTrackChanges);
  FreeAndNil(FSelectionColor);
  FreeAndNil(FIndentGuides);
  FreeAndNil(FDisplayFlowControl);
  inherited;
end;

function TPythonIDEOptions.GetPythonFileExtensions: string;
begin
  Result := FileMaskFromFileFilter(PythonFileFilter);
end;

procedure TPythonIDEOptions.SetAutoCompletionFont(const Value: TFont);
begin
  FAutoCompletionFont.Assign(Value);
end;


procedure TPythonIDEOptions.SetCodeFolding(const Value: TSynCodeFolding);
begin
  FCodeFolding.Assign(Value);
end;

procedure TPythonIDEOptions.SetDisplayFlowControl(
  const Value: TSynDisplayFlowControl);
begin
  FDisplayFlowControl.Assign(Value);
end;

procedure TPythonIDEOptions.SetIndentGuides(const Value: TSynIndentGuides);
begin
  FIndentGuides.Assign(Value);
end;

procedure TPythonIDEOptions.SetSelectionColor(const Value: TSynSelectedColor);
begin
  FSelectionColor.Assign(Value);
end;

procedure TPythonIDEOptions.SetTrackChanges(const Value: TSynTrackChanges);
begin
  FTrackChanges.Assign(Value);
end;

{ TEditorSearchOptions }

procedure TEditorSearchOptions.Assign(Source: TPersistent);
begin
  if Source is TEditorSearchOptions then
    with TEditorSearchOptions(Source) do begin
      Self.SearchCaseSensitiveType := SearchCaseSensitiveType;
      Self.SearchFromCaret := SearchFromCaret;
      Self.TempSearchFromCaret := TempSearchFromCaret;
      Self.SearchSelectionOnly := SearchSelectionOnly;
      Self.SearchTextAtCaret := SearchTextAtCaret;
      Self.SearchWholeWords := SearchWholeWords;
      Self.UseRegExp := UseRegExp;
      Self.IncrementalSearch := IncrementalSearch;

      Self.SearchText := SearchText;
      Self.SearchTextHistory := SearchTextHistory;
      Self.ReplaceText := ReplaceText;
      Self.ReplaceTextHistory := ReplaceTextHistory;
    end
  else
    inherited;
end;

procedure TEditorSearchOptions.InitSearch;
begin
  TempSearchFromCaret := SearchFromCaret;
  LastReplaceAction := raReplace;
  InitCaretXY := TBufferCoord.Invalid;
  SelStorage.Selections := [];
end;

procedure TEditorSearchOptions.NewSearch(SynEdit: TCustomSynEdit; ABackwards:
    Boolean);

  function FindTextInBlock(Strings: TStrings; BB, BE: TBufferCoord): Boolean;
  var
    StartChar, StopChar: Integer;
  begin
    Result := False;
    // preconditions start
    Assert(BB.Line <= Strings.Count, 'FindTextInBlock');
    Assert(BE.Line <= Strings.Count, 'FindTextInBlock');
    Assert(BB.Line <= BE.Line, 'FindTextInBlock');
    if BB.Line <= 0 then Exit;
    if BE.Line <= 0 then Exit;
    // preconditions end

    for var Line := BB.Line to BE.Line do
    begin
      var LineText := Strings[Line - 1];
      StartChar := IfThen(Line = BB.Line, BB.Char, 1);
      StopChar := IfThen(Line = BE.Line, BE.Char, LineText.Length + 1);
      Result := SynEdit.SearchEngine.FindAll(LineText, StartChar, StopChar) > 0;
      if Result then Break;
    end;
  end;

var
  SearchOptions: TSynSearchOptions;
  BB: TBufferCoord;
  BE: TBufferCoord;
begin
  InitSearch;
  BackwardSearch := ABackwards;
  WrappedSearch := False;
  InitCaretXY := SynEdit.CaretXY;
  TempSelectionOnly := SearchSelectionOnly and not SynEdit.Selections.IsEmpty;
  if TempSelectionOnly then
    SynEdit.Selections.Store(SelStorage)
  else
  begin
    BB := BufferCoord(1, 1);
    BE  := BufferCoord(Length(SynEdit.Lines[SynEdit.Lines.Count - 1]) + 1,
      SynEdit.Lines.Count);
  end;

  CanWrapSearch := SearchFromCaret and not TempSelectionOnly and
    (ABackwards and (BE > InitCaretXY) or
    (not ABackwards and (InitCaretXY > BB)));
  if CanWrapSearch then begin
    SearchOptions := [];

    case SearchCaseSensitiveType of
      scsAuto:
        if LowerCase(SearchText) <> SearchText then
          Include(SearchOptions, ssoMatchCase);
      scsCaseSensitive :
        Include(SearchOptions, ssoMatchCase);
    end;
    if SearchWholeWords then
      Include(SearchOptions, ssoWholeWord);

    SynEdit.SearchEngine.Options := SearchOptions;
    try
      SynEdit.SearchEngine.Pattern := ''; //  To deal with case sensitivity
      SynEdit.SearchEngine.Pattern := SearchText;
      SynEdit.SearchEngine.IsWordBreakFunction := SynEdit.IsWordBreakChar;
      if ABackwards then
        CanWrapSearch := FindTextInBlock(SynEdit.Lines, InitCaretXY, BE)
      else
        CanWrapSearch := FindTextInBlock(SynEdit.Lines, BB, InitCaretXY);
    except
      on E: ESynRegEx do begin
        CanWrapSearch := False;
      end;
    end;
  end;
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
  class property FontIgnoreProperties: TStringList read FFontIgnoreProperties;
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
  TJvAppStorageFontPropertyEngine.FFontIgnoreProperties.AddStrings(
    ['Height', 'Charset', 'Orientation', 'Pitch', 'Quality']);
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
var
  Index: Integer;
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
  FGutterIgnoreProperties.AddStrings(['Bands', 'TrackChanges']);
end;

class destructor TJvAppStorageGutterPropertyEngine.Destroy;
begin
  FGutterIgnoreProperties.Free;
end;

procedure TJvAppStorageGutterPropertyEngine.ReadProperty(
  AStorage: TJvCustomAppStorage; const APath: string; AObject,
  AProperty: TObject; const Recursive, ClearFirst: Boolean;
  const IgnoreProperties: TStrings);
var
  FontSize: Integer;
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
var
  FontSize: Integer;
begin
  FontSize := TSynGutter(AProperty).Font.Size;
  TSynGutter(AProperty).ChangeScale(96, Screen.PixelsPerInch);
  TSynGutter(AProperty).Font.Size := FontSize;
  AStorage.WritePersistent(APath, AProperty as TSynGutter, Recursive,
    FGutterIgnoreProperties);
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
var
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
var
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
  if AStorage.PathExists(APath) then
  begin
    KeyStrokes := AProperty as TSynEditKeyStrokes;
    ReadAddedKeystrokes;
    ReadRemovedKeystrokes;
  end;
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

class procedure TPyScripterSettings.CreateSearchOptions;
begin
  EditorSearchOptions := TEditorSearchOptions.Create;
  EditorSearchOptions.FSearchTextAtCaret := True;
  EditorSearchOptions.FSearchFromCaret := True;
  EditorSearchOptions.FIncrementalSearch := True;
  EditorSearchOptions.InitSearch;
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
          + sLineBreak + 'Error Message: '), [Source, Dest, E.Message]);
    end;
  end;

var
  PublicPath: string;
  AppName, AppININame, AppRootPath: string;
begin
  AppName := GetAppName;
  AppININame := AppName + '.ini';
  AppRootPath := GetAppRootPath;

  OptionsFileName := TPath.Combine(AppRootPath, AppININame);
  IsPortable := FileExists(OptionsFileName);
  if IsPortable then
  begin
    // Portable version - nothing is stored in other directories
    UserDataPath := AppRootPath;
    ColorThemesFilesDir := TPath.Combine(UserDataPath, 'Highlighters');
    StylesFilesDir := TPath.Combine(UserDataPath, 'Styles');
    LspServerPath :=  TPath.Combine(UserDataPath, 'Lib', 'Lsp');
    UserDebugInspectorsDir :=  TPath.Combine(UserDataPath, 'Variable Inspectors');
  end
  else
  begin
    UserDataPath := TPath.Combine(GetHomePath,  AppName);
    OptionsFileName := TPath.Combine(UserDataPath, AppININame);
    if not ForceDirectories(UserDataPath) or
       not ForceDirectories(TPath.Combine(UserDataPath, 'Lsp'))
    then
      StyledMessageDlg(Format(SAccessAppDataDir, [UserDataPath]),
      mtWarning, [mbOK], 0);
    PublicPath := TPath.Combine(TPath.GetPublicPath, AppName);
    ColorThemesFilesDir := TPath.Combine(PublicPath, 'Highlighters');
    StylesFilesDir := TPath.Combine(PublicPath, 'Styles');
    LspServerPath :=  TPath.Combine(PublicPath, 'Lsp');
    UserDebugInspectorsDir :=  TPath.Combine(UserDataPath, 'Variable Inspectors');
    AppDebugInspectorsDir := TPath.Combine(PublicPath, 'Variable Inspectors');
    // First use setup
    CopyFileIfNeeded(TPath.Combine(PublicPath, AppININame), OptionsFileName);
  end;
  ForceDirectories(UserDebugInspectorsDir);

  EngineInitFile := TPath.Combine(UserDataPath, 'python_init.py');
  PyScripterInitFile := TPath.Combine(UserDataPath, 'pyscripter_init.py');
  PyScripterLogFile := TPath.Combine(UserDataPath, 'pyscripter.log');
  RecoveryDir := TPath.Combine(UserDataPath, 'Recovery');
  if not IsPortable then
  begin
    // First use ruff setup
    ForceDirectories(TPath.Combine(UserDataPath, 'Lsp', 'Ruff'));
    CopyFileIfNeeded(TPath.Combine(PublicPath, 'Lsp', 'Ruff', 'ruff.toml'),
      TPath.Combine(UserDataPath, 'Lsp', 'Ruff', 'ruff.toml'));
    // Also copy start-up scripts
    CopyFileIfNeeded(TPath.Combine(PublicPath, 'python_init.py'), EngineInitFile);
    CopyFileIfNeeded(TPath.Combine(PublicPath, 'pyscripter_init.py'), PyScripterInitFile);
  end;

  CreateIDEOptions;
  CreateEditorOptions;
  CreateSearchOptions;
  ApplyPyIDEOptions;
  // Save Default editor keystrokes
  TPyScripterSettings.DefaultEditorKeyStrokes := TSynEditKeyStrokes.Create(nil);
  TPyScripterSettings.DefaultEditorKeyStrokes.Assign(EditorOptions.Keystrokes);
end;

class procedure TPyScripterSettings.ApplyPyIDEOptions;
begin
  EditorOptions.Gutter.TrackChanges.Assign(PyIDEOptions.TrackChanges);
  EditorOptions.SelectedColor.Assign(PyIDEOptions.SelectionColor);
  EditorOptions.IndentGuides.Assign(PyIDEOptions.IndentGuides);
  EditorOptions.DisplayFlowControl.Assign(PyIDEOptions.DisplayFlowControl);
  if PyIDEOptions.AccessibilitySupport then
    EditorOptions.Options := EditorOptions.Options + [eoAccessibility]
  else
    EditorOptions.Options := EditorOptions.Options - [eoAccessibility];
  EditorSearchOptions.SearchTextAtCaret := PyIDEOptions.SearchTextAtCaret;
end;

class procedure TPyScripterSettings.CreateEditorOptions;
begin
  EditorOptions := TSynEditorOptionsContainer.Create(nil);
  with EditorOptions do
  begin
    Font.Name := DefaultCodeFontName;
    Font.Size := 10;
    Gutter.Font.Name := Font.Name;
    Gutter.Font.Color := clGrayText;
    Gutter.Gradient := False;
    Gutter.DigitCount := 2;
    Gutter.ShowLineNumbers := True;
    Gutter.AutoSize := True;
    Gutter.ChangeScale(Screen.PixelsPerInch, 96);
    Gutter.Font.Size := 9;
    Gutter.BorderStyle := gbsNone;

    Options := [eoDragDropEditing, eoEnhanceHomeKey, eoShowLigatures,
                eoEnhanceEndKey, eoGroupUndo, eoKeepCaretX,
                eoSmartTabDelete, eoTabsToSpaces, eoTabIndent,
                eoTrimTrailingSpaces, eoAutoIndent, eoBracketsHighlight,
                eoAccessibility, eoCompleteBrackets, eoCompleteQuotes];
    ScrollOptions := [eoHideShowScrollbars, eoShowScrollHint];
    WantTabs := True;
    TabWidth := 4;
    MaxUndo := 0;
    // ActiveLineColor and RightEdgeColor for dark backgrounds.
    // Will be adjusted automatically when switching to light background themes.
    ActiveLineColor := $333333;
    RightEdgeColor := TColors.DimGray;
    RightEdge := 88;  // same as ruff and black

    RegisterEditorUserCommands(EditorOptions.Keystrokes);
  end;
end;

class destructor TPyScripterSettings.Destroy;
begin
  EditorSearchOptions.Free;
  PyIDEOptions.Free;
  EditorOptions.Free;
  TPyScripterSettings.DefaultEditorKeyStrokes.Free;
end;

class procedure TPyScripterSettings.RegisterEditorUserCommands(
  Keystrokes: TSynEditKeyStrokes);
// Register User Commands and shortcuts
begin
  with Keystrokes do begin
    AddKey(ecCodeCompletion, VK_SPACE, [ssCtrl]);
    AddKey(ecParamCompletion, VK_SPACE, [ssCtrl, ssShift]);
    // #869
    Delete(FindCommand(ecDeleteLine));
    AddKey(ecDeleteLine, Ord('D'), [ssCtrl, ssShift]);
  end;
end;

end.
