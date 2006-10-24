{-----------------------------------------------------------------------------
 Unit Name: dmCommands
 Author:    Kiriakos Vlahos
 Date:      09-Mar-2005
 Purpose:   Data Module of PyScripter
 History:
-----------------------------------------------------------------------------}

unit dmCommands;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, SynEdit, SynEditHighlighter, SynHighlighterPython, SynRegExpr,
  ImgList, dlgSynEditOptions, SynEditPrint, StdActns,
  JvComponent, JvChangeNotify, SynCompletionProposal, TB2Item,
  SynEditRegexSearch, SynEditMiscClasses, SynEditSearch, VirtualTrees,
  SynEditTextBuffer, SynEditKeyCmds, JvComponentBase, SynHighlighterXML,
  SynHighlighterCSS, SynHighlighterHtml, JvProgramVersionCheck, JvPropertyStore,
  SynHighlighterIni, TB2MRU, TBXExtItems;

type
  TPythonIDEOptions = class(TPersistent)
  private
    fTimeOut : integer;
    fUndoAfterSave : Boolean;
    fSaveFilesBeforeRun : Boolean;
    fRestoreOpenFiles : Boolean;
    fCreateBackupFiles : Boolean;
    fExporerInitiallyExpanded : Boolean;
    fSearchTextAtCaret : Boolean;
    fPythonFileFilter : string;
    fHTMLFileFilter : string;
    fXMLFileFilter : string;
    fCSSFileFilter : string;
    fFileExplorerFilter : string;
    fDateLastCheckedForUpdates : TDateTime;
    fAutoCheckForUpdates : boolean;
    fDaysBetweenChecks : integer;
    fMaskFPUExceptions : boolean;
    fUseBicycleRepairMan : boolean;
    fSpecialPackages : string;
    fUTF8inInterpreter : boolean;
    fShowCodeHints : boolean;
    fShowDebuggerHints : boolean;
    fAutoCompleteBrackets : boolean;
    fCommandLine : string;
    fUseCommandLine : Boolean;
    fMarkExecutableLines : Boolean;
    fCleanupMainDict : Boolean;
    fCleanupSysModules : Boolean;
    fCheckSyntaxAsYouType : Boolean;
    fFileExplorerContextMenu : Boolean;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property TimeOut : integer read fTimeOut write fTimeOut;
    property UndoAfterSave : boolean read fUndoAfterSave
      write fUndoAfterSave;
    property SaveFilesBeforeRun : boolean read fSaveFilesBeforeRun
      write fSaveFilesBeforeRun;
    property RestoreOpenFiles : Boolean read fRestoreOpenFiles
      write fRestoreOpenFiles;
    property CreateBackupFiles : boolean read fCreateBackupFiles
      write fCreateBackupFiles;
    property ExporerInitiallyExpanded : boolean read fExporerInitiallyExpanded
      write fExporerInitiallyExpanded;
    property SearchTextAtCaret : Boolean read fSearchTextAtCaret
      write fSearchTextAtCaret stored False;
    property PythonFileFilter : string read fPythonFileFilter
      write fPythonFileFilter;
    property HTMLFileFilter : string read fHTMLFileFilter
      write fHTMLFileFilter;
    property XMLFileFilter : string read fXMLFileFilter
      write fXMLFileFilter;
    property CSSFileFilter : string read fCSSFileFilter
      write fCSSFileFilter;
    property FileExplorerFilter : string read fFileExplorerFilter
      write fFileExplorerFilter;
    property DateLastCheckedForUpdates : TDateTime read fDateLastCheckedForUpdates
      write fDateLastCheckedForUpdates;
    property AutoCheckForUpdates : boolean read fAutoCheckForUpdates
      write fAutoCheckForUpdates;
    property DaysBetweenChecks : integer read fDaysBetweenChecks
      write fDaysBetweenChecks;
    property MaskFPUExceptions : Boolean read fMaskFPUExceptions
      write fMaskFPUExceptions;
    property UseBicycleRepairMan : boolean read fUseBicycleRepairMan
      write fUseBicycleRepairMan;
    property SpecialPackages : string read fSpecialPackages write fSpecialPackages;
    property UTF8inInterpreter : boolean read fUTF8inInterpreter
      write fUTF8inInterpreter;
    property ShowCodeHints : boolean read fShowCodeHints write fShowCodeHints;
    property ShowDebuggerHints : boolean
      read fShowDebuggerHints write fShowDebuggerHints;
    property AutoCompleteBrackets : boolean
      read fAutoCompleteBrackets write fAutoCompleteBrackets;
    property CommandLine : string read fCommandLine write fCommandLine;
    property UseCommandLine : Boolean read fUseCommandLine write fUseCommandLine;
    property MarkExecutableLines : Boolean read fMarkExecutableLines
      write fMarkExecutableLines;
    property CleanupMainDict : Boolean read fCleanupMainDict write fCleanupMainDict;
    property CleanupSysModules : Boolean read fCleanupSysModules write fCleanupSysModules;
    property CheckSyntaxAsYouType : Boolean read fCheckSyntaxAsYouType
      write fCheckSyntaxAsYouType;
    property FileExplorerContextMenu : Boolean read fFileExplorerContextMenu
      write fFileExplorerContextMenu;

  end;

  TEditorSearchOptions = class(TPersistent)
  private
    fSearchBackwards: boolean;
    fSearchCaseSensitive: boolean;
    fSearchFromCaret: boolean;
    fSearchSelectionOnly: boolean;
    fSearchTextAtCaret: boolean;
    fSearchWholeWords: boolean;
    fUseRegExp: boolean;

    fSearchText: string;
    fSearchTextHistory: string;
    fReplaceText: string;
    fReplaceTextHistory: string;
  public
    procedure Assign(Source: TPersistent); override;
    property SearchBackwards: boolean read fSearchBackwards write fSearchBackwards;
    property SearchSelectionOnly: boolean read fSearchSelectionOnly write fSearchSelectionOnly;
    property SearchText: string read fSearchText write fSearchText;
    property ReplaceText: string read fReplaceText write fReplaceText;
    property SearchTextHistory: string read fSearchTextHistory write fSearchTextHistory;
    property ReplaceTextHistory: string read fReplaceTextHistory write fReplaceTextHistory;
  published
    property SearchCaseSensitive: boolean read fSearchCaseSensitive write fSearchCaseSensitive;
    property SearchFromCaret: boolean read fSearchFromCaret write fSearchFromCaret;
    property SearchTextAtCaret: boolean read fSearchTextAtCaret write fSearchTextAtCaret;
    property SearchWholeWords: boolean read fSearchWholeWords write fSearchWholeWords;
    property UseRegExp: boolean read fUseRegExp write fUseRegExp;
  end;

  TCommandsDataModule = class(TDataModule)
    dlgFileOpen: TOpenDialog;
    actlMain: TActionList;
    actFileSave: TAction;
    actFileSaveAs: TAction;
    actFileClose: TAction;
    actFilePrint: TAction;
    actEditDelete: TAction;
    actEditRedo: TAction;
    actSearchFind: TAction;
    actSearchFindNext: TAction;
    actSearchFindPrev: TAction;
    actSearchReplace: TAction;
    dlgFileSave: TSaveDialog;
    SynPythonSyn: TSynPythonSyn;
    actFileSaveAll: TAction;
    SynEditPrint: TSynEditPrint;
    PrintDialog: TPrintDialog;
    PrinterSetupDialog: TPrinterSetupDialog;
    actPrinterSetup: TAction;
    actPrintPreview: TAction;
    actPageSetup: TAction;
    actEditorOptions: TAction;
    actIDEOptions: TAction;
    actEditIndent: TAction;
    actEditDedent: TAction;
    actEditCommentOut: TAction;
    actEditUncomment: TAction;
    actSearchMatchingBrace: TAction;
    actEditTabify: TAction;
    actEditUntabify: TAction;
    CodeImages: TImageList;
    actPythonPath: TAction;
    actPythonManuals: THelpContents;
    actSearchGoToLine: TAction;
    JvChangeNotify: TJvChangeNotify;
    actHelpContents: THelpContents;
    actAbout: TAction;
    actFindInFiles: TAction;
    ParameterCompletion: TSynCompletionProposal;
    ModifierCompletion: TSynCompletionProposal;
    actParameterCompletion: TAction;
    actModifierCompletion: TAction;
    actReplaceParameters: TAction;
    actHelpParameters: TAction;
    CodeTemplatesCompletion: TSynAutoComplete;
    actInsertTemplate: TAction;
    actCustomizeParameters: TAction;
    actCodeTemplates: TAction;
    actConfigureTools: TAction;
    imlShellIcon: TImageList;
    actHelpExternalTools: TAction;
    actFindFunction: TAction;
    Images: TTBImageList;
    DisabledImages: TImageList;
    actEditLineNumbers: TAction;
    actEditShowSpecialChars: TAction;
    actFindPreviousReference: TAction;
    actFindNextReference: TAction;
    SynEditSearch: TSynEditSearch;
    SynEditRegexSearch: TSynEditRegexSearch;
    actEditLBDos: TAction;
    actEditLBUnix: TAction;
    actEditLBMac: TAction;
    actEditUTF8: TAction;
    actHelpEditorShortcuts: TAction;
    actCheckForUpdates: TAction;
    actEditCut: TEditCut;
    actEditCopy: TEditCopy;
    actEditPaste: TEditPaste;
    actEditSelectAll: TEditSelectAll;
    actEditUndo: TEditUndo;
    SynHTMLSyn: TSynHTMLSyn;
    SynCssSyn: TSynCssSyn;
    SynXMLSyn: TSynXMLSyn;
    actIDEShortcuts: TAction;
    actUnitTestWizard: TAction;
    ProgramVersionCheck: TJvProgramVersionCheck;
    ProgramVersionHTTPLocation: TJvProgramVersionHTTPLocation;
    actInterpreterEditorOptions: TAction;
    actEditToggleComment: TAction;
    actFileTemplates: TAction;
    SynIniSyn: TSynIniSyn;
    CommandLineMRU: TTBXMRUList;
    function ProgramVersionHTTPLocationLoadFileFromRemote(
      AProgramVersionLocation: TJvProgramVersionHTTPLocation; const ARemotePath,
      ARemoteFileName, ALocalPath, ALocalFileName: string): string;
    procedure actCheckForUpdatesExecute(Sender: TObject);
    procedure actUnitTestWizardExecute(Sender: TObject);
    procedure actIDEShortcutsExecute(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure actFilePrintExecute(Sender: TObject);
    procedure actFileCloseExecute(Sender: TObject);
    procedure actEditCutExecute(Sender: TObject);
    procedure actEditCopyExecute(Sender: TObject);
    procedure actEditPasteExecute(Sender: TObject);
    procedure actEditDeleteExecute(Sender: TObject);
    procedure actEditSelectAllExecute(Sender: TObject);
    procedure actEditRedoExecute(Sender: TObject);
    procedure actEditUndoExecute(Sender: TObject);
    procedure actSearchFindExecute(Sender: TObject);
    procedure actSearchFindNextExecute(Sender: TObject);
    procedure actSearchFindPrevExecute(Sender: TObject);
    procedure actSearchReplaceExecute(Sender: TObject);
    procedure actFileSaveAllExecute(Sender: TObject);
    procedure actPrinterSetupExecute(Sender: TObject);
    procedure actPageSetupExecute(Sender: TObject);
    procedure actPrintPreviewExecute(Sender: TObject);
    procedure actEditorOptionsExecute(Sender: TObject);
    procedure actEditIndentExecute(Sender: TObject);
    procedure actEditDedentExecute(Sender: TObject);
    procedure actEditCommentOutExecute(Sender: TObject);
    procedure actEditUncommentExecute(Sender: TObject);
    procedure actSearchMatchingBraceExecute(Sender: TObject);
    procedure actEditTabifyExecute(Sender: TObject);
    procedure actEditUntabifyExecute(Sender: TObject);
    procedure actIDEOptionsExecute(Sender: TObject);
    procedure actPythonPathExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actPythonManualsExecute(Sender: TObject);
    procedure UpdateMainActions;
    procedure actSearchGoToLineExecute(Sender: TObject);
    procedure JvChangeNotifyChangeNotify(Sender: TObject; Dir: String;
      Actions: TJvChangeActions);
    procedure actFindInFilesExecute(Sender: TObject);
    procedure actHelpContentsExecute(Sender: TObject);
    procedure ParameterCompletionCodeCompletion(Sender: TObject;
      var Value: WideString; Shift: TShiftState; Index: Integer;
      EndToken: WideChar);
    procedure ModifierCompletionCodeCompletion(Sender: TObject;
      var Value: WideString; Shift: TShiftState; Index: Integer;
      EndToken: WideChar);
    procedure actParameterCompletionExecute(Sender: TObject);
    procedure actModifierCompletionExecute(Sender: TObject);
    procedure actReplaceParametersExecute(Sender: TObject);
    procedure actHelpParametersExecute(Sender: TObject);
    procedure actInsertTemplateExecute(Sender: TObject);
    procedure actCustomizeParametersExecute(Sender: TObject);
    procedure actCodeTemplatesExecute(Sender: TObject);
    procedure actConfigureToolsExecute(Sender: TObject);
    procedure actHelpExternalToolsExecute(Sender: TObject);
    procedure actFindFunctionExecute(Sender: TObject);
    procedure actEditLineNumbersExecute(Sender: TObject);
    procedure actEditShowSpecialCharsExecute(Sender: TObject);
    procedure actFindNextReferenceExecute(Sender: TObject);
    procedure actEditLBExecute(Sender: TObject);
    procedure actEditUTF8Execute(Sender: TObject);
    procedure actHelpEditorShortcutsExecute(Sender: TObject);
    procedure actInterpreterEditorOptionsExecute(Sender: TObject);
    procedure actEditToggleCommentExecute(Sender: TObject);
    procedure actFileTemplatesExecute(Sender: TObject);
  private
    fHighlighters: TStrings;
    fUntitledNumbers: TBits;
    fChangeNotifyFileNames : TStringList;
    fSearchFromCaret: boolean;
  public
    BlockOpenerRE : TRegExpr;
    BlockCloserRE : TRegExpr;
    EOLCleanerRE : TRegExpr;
    CommentLineRE : TRegExpr;
    NumberOfOriginalImages : integer;
    NonExecutableLineRE : TRegExpr;
    EditorOptions : TSynEditorOptionsContainer;
    InterpreterEditorOptions : TSynEditorOptionsContainer;
    PyIDEOptions : TPythonIDEOptions;
    EditorSearchOptions : TEditorSearchOptions;
    ExcludedFileNotificationdDrives : TStringList;
    function CleanEOLs(S : string) : string;
    function IsBlockOpener(S : string) : Boolean;
    function IsBlockCloser(S : string) : Boolean;
    function IsExecutableLine(Line : string) : Boolean;
    function GetHighlighterForFile(AFileName: string): TSynCustomHighlighter;
    procedure SynEditOptionsDialogGetHighlighterCount(Sender: TObject;
                var Count: Integer);
    procedure SynEditOptionsDialogGetHighlighter(Sender: TObject;
                Index: Integer; var SynHighlighter: TSynCustomHighlighter);
    procedure SynEditOptionsDialogSetHighlighter(Sender: TObject;
                Index: Integer; SynHighlighter: TSynCustomHighlighter);
    function GetSaveFileName(var ANewName: string;
      AHighlighter: TSynCustomHighlighter; DefaultExtension : string): boolean;
    function GetUntitledNumber: integer;
    procedure ReleaseUntitledNumber(ANumber: integer);
    procedure PaintMatchingBrackets(Canvas : TCanvas; SynEdit : TSynEdit;
      TransientType: TTransientType);
    function ShowPythonKeywordHelp(KeyWord : string) : Boolean;
    procedure UpdateChangeNotify;
    procedure PrepareParameterCompletion;
    procedure PrepareModifierCompletion;
    procedure VirtualStringTreeAdvancedHeaderDraw(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
    procedure VirtualStringTreeDrawQueryElements(
      Sender: TVTHeader; var PaintInfo: THeaderPaintInfo;
      var Elements: THeaderPaintElements);
    procedure GetEditorUserCommand(AUserCommand: Integer; var ADescription: String);
    procedure DoSearchReplaceText(SynEdit : TSynEdit;
      AReplace: boolean;  ABackwards: boolean);
    procedure ShowSearchReplaceDialog(SynEdit : TSynEdit; AReplace: boolean);
    procedure ApplyEditorOptions;
    property Highlighters : TStrings read fHighlighters;
  end;

Const
  ecRecallCommandPrev : word = ecUserFirst + 100;
  ecRecallCommandNext : word = ecUserFirst + 101;
  ecRecallCommandEsc : word = ecUserFirst + 102;
  ecCodeCompletion : word = ecUserFirst + 103;
  ecParamCompletion : word = ecUserFirst + 104;

var
  CommandsDataModule: TCommandsDataModule = nil;

implementation

{$R *.DFM}

uses
  uEditAppIntfs, SynEditTypes, dlgSynPageSetup, uHighlighterProcs,
  dlgOptionsEditor, frmPythonII, dlgDirectoryList, VarPyth,
  dlgAboutPyScripter, frmPyIDEMain, JclFileUtils, SHDocVw, Variants,
  JclStrings, frmEditor, frmFindResults, cParameters, dlgCustomParams,
  uParams, dlgCodeTemplates, dlgConfigureTools, cTools, 
  frmFunctionList, StringResources, TBXThemes, TBX, uCommonFunctions,
  StoHtmlHelp, {uMMMXP_MainService, }JvJCLUtils, Menus, SynEditStrConst,
  dlgSearchText, dlgReplaceText, dlgConfirmReplace, dlgCustomShortcuts,
  dlgUnitTestWizard, WinInet, Math, SynUnicode, Registry, ShlObj, ShellAPI,
  dlgFileTemplates;

{ TPythonIDEOptions }

procedure TPythonIDEOptions.Assign(Source: TPersistent);
begin
  if Source is TPythonIDEOptions then
    with TPythonIDEOptions(Source) do begin
      Self.fTimeOut := TimeOut;
      Self.fUndoAfterSave := UndoAfterSave;
      Self.fSaveFilesBeforeRun := SaveFilesBeforeRun;
      Self.fRestoreOpenFiles := fRestoreOpenFiles;
      Self.fCreateBackUpFiles := CreateBackUpFiles;
      Self.fExporerInitiallyExpanded := ExporerInitiallyExpanded;
      Self.fSearchTextAtCaret := SearchTextAtCaret;
      Self.fPythonFileFilter := PythonFileFilter;
      Self.fHTMLFileFilter := HTMLFileFilter;
      Self.fXMLFileFilter := XMLFileFilter;
      Self.fCSSFileFilter := CSSFileFilter;
      Self.fFileExplorerFilter := FileExplorerFilter;
      Self.fDateLastCheckedForUpdates := DateLastCheckedForUpdates;
      Self.fAutoCheckForUpdates := AutoCheckForUpdates;
      Self.fDaysBetweenChecks := DaysBetweenChecks;
      Self.fMaskFPUExceptions := MaskFPUExceptions;
      Self.fUseBicycleRepairMan := UseBicycleRepairMan;
      Self.fSpecialPackages := SpecialPackages;
      Self.fUTF8inInterpreter := UTF8inInterpreter;
      Self.fShowCodeHints := ShowCodeHints;
      Self.fShowDebuggerHints := ShowDebuggerHints;
      Self.fAutoCompleteBrackets := AutoCompleteBrackets;
      Self.fUseCommandLine := UseCommandLine;
      Self.fCommandLine := CommandLine;
      Self.fMarkExecutableLines := MarkExecutableLines;
      Self.fCleanupMainDict := CleanupMainDict;
      Self.fCleanupSysModules := CleanupSysModules;
      Self.fCheckSyntaxAsYouType := CheckSyntaxAsYouType;
      Self.fFileExplorerContextMenu := FileExplorerContextMenu;
    end
  else
    inherited;
end;

constructor TPythonIDEOptions.Create;
begin
  fTimeOut := 0; // 5000;
  fSaveFilesBeforeRun := True;
  fCreateBackupFiles := False;
  fExporerInitiallyExpanded := False;
  fPythonFileFilter := SYNS_FilterPython;
  fHTMLFileFilter := SYNS_FilterHTML;
  fXMLFileFilter := SYNS_FilterXML;
  fCSSFileFilter := SYNS_FilterCSS;
  fFileExplorerFilter := '*.py';
  fSearchTextAtCaret := True;
  fRestoreOpenFiles := True;
  fDateLastCheckedForUpdates := MinDateTime;
  fAutoCheckForUpdates := True;
  fDaysBetweenChecks := 7;
  fMaskFPUExceptions := True;
  fUseBicycleRepairMan := False;
  fSpecialPackages := 'os, wx, scipy';
  fUTF8inInterpreter := True;
  fShowCodeHints := True;
  fShowDebuggerHints := True;
  fAutoCompleteBrackets := True;
  fMarkExecutableLines := True;
  fCleanupMainDict := True;
  fCleanupSysModules := True;
  fCheckSyntaxAsYouType := True;
  fFileExplorerContextMenu := True;
end;

{ TEditorSearchOptions }

procedure TEditorSearchOptions.Assign(Source: TPersistent);
begin
  if Source is TEditorSearchOptions then
    with TEditorSearchOptions(Source) do begin
      Self.fSearchBackwards := SearchBackwards;
      Self.fSearchCaseSensitive := SearchCaseSensitive;
      Self.fSearchFromCaret := SearchFromCaret;
      Self.fSearchSelectionOnly := SearchSelectionOnly;
      Self.fSearchTextAtCaret := SearchTextAtCaret;
      Self.fSearchWholeWords := SearchWholeWords;
      Self.fUseRegExp := UseRegExp;

      Self.fSearchText := SearchText;
      Self.fSearchTextHistory := SearchTextHistory;
      Self.fReplaceText := ReplaceText;
      Self.fReplaceTextHistory := ReplaceTextHistory;
    end
  else
    inherited;
end;

{ TCommandsDataModule }

procedure TCommandsDataModule.DataModuleCreate(Sender: TObject);
var
  SHFileInfo: TSHFileInfo;
  Index : integer;
begin
  // Setup Highlighters
  fHighlighters := TStringList.Create;
  TStringList(fHighlighters).CaseSensitive := False;
  GetHighlighters(Self, fHighlighters, False);
  TStringList(fHighlighters).Sort;
  //  Place Python first
  Index := fHighlighters.IndexOf(SynPythonSyn.LanguageName);
  if Index >= 0 then fHighlighters.Delete(Index);
  fHighlighters.InsertObject(0, SynPythonSyn.LanguageName, SynPythonSyn);

  // DefaultOptions
  PyIDEOptions := TPythonIDEOptions.Create;
  MaskFPUExceptions(PyIDEOptions.fMaskFPUExceptions);
  dlgFileOpen.Filter := PyIDEOptions.PythonFileFilter;
  ExcludedFileNotificationdDrives := TStringList.Create;

  BlockOpenerRE := TRegExpr.Create;
  BlockOpenerRE.Expression := ':\s*(#.*)?$';
  BlockCloserRE := TRegExpr.Create;
  BlockCloserRE.Expression := '\s*(return|break|continue|raise|pass)\b';
  NonExecutableLineRE := TRegExpr.Create;
  NonExecutableLineRE.Expression := '(^\s*(class|def)\b)|(^\s*#)|(^\s*$)';
  EOLCleanerRE := TRegExpr.Create;
  EOLCleanerRE.Expression := '(\r\n)|\r|(\n\r)';
  EOLCleanerRE.ModifierM := True;
  EOLCleanerRE.ModifierS := False;
  CommentLineRE := TRegExpr.Create;
  CommentLineRE.Expression := '^##';
  CommentLineRE.ModifierM := True;

  EditorOptions := TSynEditorOptionsContainer.Create(Self);
  with EditorOptions do begin
    Font.Height := -13;
    Font.Name := 'Courier New';
    Gutter.Font.Height := -11;
    Gutter.Font.Name := 'Courier New';
    Gutter.Gradient := True;
    Options := [eoAutoSizeMaxScrollWidth, eoDragDropEditing, eoEnhanceHomeKey,
                eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoKeepCaretX,
                eoShowScrollHint, eoSmartTabDelete, eoTabsToSpaces, eoTabIndent,
                eoTrimTrailingSpaces, eoAutoIndent];
    TabWidth := 4;
    WantTabs := True;
    TabWidth := 4;

    SelectedColor.Background := SelectionBackgroundColor;
    Keystrokes.Delete(Keystrokes.FindCommand(ecMatchBracket));
    // Register the CodeCompletion Command
    with Keystrokes.Add do begin
      ShortCut := Menus.ShortCut(VK_SPACE, [ssCtrl]);
      Command := ecCodeCompletion;
    end;
    // Register the ParamCompletion Command
    with Keystrokes.Add do begin
      ShortCut := Menus.ShortCut(VK_SPACE, [ssCtrl, ssShift]);
      Command := ecParamCompletion;
    end;
  end;
  InterpreterEditorOptions := TSynEditorOptionsContainer.Create(Self);
  InterpreterEditorOptions.Assign(EditorOptions);
  InterpreterEditorOptions.Options := InterpreterEditorOptions.Options -
    [eoTrimTrailingSpaces, eoTabsToSpaces];

  EditorSearchOptions := TEditorSearchOptions.Create;
  EditorSearchOptions.fSearchTextAtCaret := True;

  with SynEditPrint.Header do begin
    Add('$TITLE$', nil, taCenter, 2);
  end;
  with SynEditPrint.Footer do begin
    Add('$PAGENUM$/$PAGECOUNT$', nil, taCenter, 1);
  end;

  // Support for ChangeNotify
    fChangeNotifyFileNames := TStringList.Create;

  // Parameter Completion
  PrepareParameterCompletion;
  PrepareModifierCompletion;

  // Setup the ShellIcon imagelist
  imlShellIcon.Handle := SHGetFileInfo('', 0, SHFileInfo, SizeOf(SHFileInfo),
    SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  NumberOfOriginalImages := Images.Count;

  // Help file
  //StoHelpViewer.HtmlExt := '.html';

  //Program Version Check
  ProgramVersionCheck.ThreadDialog.DialogOptions.ShowModal := False;
  ProgramVersionCheck.ThreadDialog.DialogOptions.Caption := 'Downloading...';
  ProgramVersionCheck.LocalDirectory := ExtractFilePath(Application.ExeName)+ 'Updates';
end;

procedure TCommandsDataModule.DataModuleDestroy(Sender: TObject);
begin
  fHighlighters.Free;
  fUntitledNumbers.Free;
  CommandsDataModule := nil;
  BlockOpenerRE.Free;
  BlockCloserRE.Free;
  NonExecutableLineRE.Free;
  EOLCleanerRE.Free;
  CommentLineRE.Free;
  PyIDEOptions.Free;
  ExcludedFileNotificationdDrives.Free;
  EditorSearchOptions.Free;
  fChangeNotifyFileNames.Free;
  imlShellIcon.Handle := 0;
end;

// implementation

function TCommandsDataModule.GetHighlighterForFile(
  AFileName: string): TSynCustomHighlighter;
begin
  if AFileName <> '' then
    Result := GetHighlighterFromFileExt(fHighlighters, ExtractFileExt(AFileName))
  else
    Result := nil;
end;

procedure TCommandsDataModule.SynEditOptionsDialogGetHighlighterCount(Sender: TObject;
  var Count: Integer);
begin
   count := fHighlighters.Count;
end;

procedure TCommandsDataModule.SynEditOptionsDialogGetHighlighter(Sender: TObject;
  Index: Integer; var SynHighlighter: TSynCustomHighlighter);
begin
   if (Index >= 0) and (Index < fHighlighters.Count) then
      SynHighlighter := fHighlighters.Objects[Index] as TSynCustomHighlighter
   else
     SynHighlighter := nil;
end;

procedure TCommandsDataModule.SynEditOptionsDialogSetHighlighter(Sender: TObject;
  Index: Integer; SynHighlighter: TSynCustomHighlighter);
begin
   if (Index >= 0) and (Index < fHighlighters.Count) then
     (fHighlighters.Objects[Index] as TSynCustomHighlighter).Assign(SynHighlighter);
end;

function TCommandsDataModule.GetSaveFileName(var ANewName: string;
  AHighlighter: TSynCustomHighlighter; DefaultExtension : string): boolean;
begin
  with dlgFileSave do begin
    if ANewName <> '' then begin
      InitialDir := ExtractFileDir(ANewName);
      FileName := ExtractFileName(ANewName);
      Title := Format('Save "%s" As', [FileName]);
    end else begin
      InitialDir := '';
      FileName := '';
      Title := 'Save File As';
    end;
    if AHighlighter <> nil then
      Filter := AHighlighter.DefaultFilter
    else
      Filter := SFilterAllFiles;

    DefaultExt := DefaultExtension;
    //  Make the current file extension the default extension 
    if DefaultExt = '' then
      DefaultExt := ExtractFileExt(ANewName);
    
    if Execute then begin
      ANewName := FileName;
      Result := TRUE;
    end else
      Result := FALSE;
  end;
end;

function TCommandsDataModule.GetUntitledNumber: integer;
begin
  if fUntitledNumbers = nil then
    fUntitledNumbers := TBits.Create;
  Result := fUntitledNumbers.OpenBit;
  if Result = fUntitledNumbers.Size then
    fUntitledNumbers.Size := fUntitledNumbers.Size + 32;
  fUntitledNumbers[Result] := TRUE;
  Inc(Result);
end;

procedure TCommandsDataModule.ReleaseUntitledNumber(ANumber: integer);
begin
  Dec(ANumber);
  if (fUntitledNumbers <> nil) and (ANumber >= 0)
    and (ANumber < fUntitledNumbers.Size)
  then
    fUntitledNumbers[ANumber] := FALSE;
end;

procedure TCommandsDataModule.actFileSaveExecute(Sender: TObject);
begin
  if GI_FileCmds <> nil then
    GI_FileCmds.ExecSave;
end;

procedure TCommandsDataModule.actFileSaveAsExecute(Sender: TObject);
begin
  if GI_FileCmds <> nil then
    GI_FileCmds.ExecSaveAs;
end;

procedure TCommandsDataModule.actFileSaveAllExecute(Sender: TObject);
var
  i : integer;
  FileCommands : IFileCommands;
begin
  for i := 0 to GI_EditorFactory.Count -1 do begin
    FileCommands := GI_EditorFactory[i] as IFileCommands;
    if Assigned(FileCommands) then
      FileCommands.ExecSave;
  end;
end;

procedure TCommandsDataModule.actFilePrintExecute(Sender: TObject);
begin
  if GI_FileCmds <> nil then
    GI_FileCmds.ExecPrint;
end;

procedure TCommandsDataModule.actPrintPreviewExecute(Sender: TObject);
begin
  if GI_FileCmds <> nil then
    GI_FileCmds.ExecPrintPreview;
end;

procedure TCommandsDataModule.actPrinterSetupExecute(Sender: TObject);
begin
  PrinterSetupDialog.Execute;
end;

procedure TCommandsDataModule.actPageSetupExecute(Sender: TObject);
begin
  with TPageSetupDlg.Create(Self) do begin
    SetValues(SynEditPrint);
    if ShowModal = mrOk then
      GetValues(SynEditPrint);
    Release;
  end;
end;

procedure TCommandsDataModule.actFileCloseExecute(Sender: TObject);
begin
  if GI_FileCmds <> nil then
    GI_FileCmds.ExecClose;          
end;

procedure TCommandsDataModule.actEditCutExecute(Sender: TObject);
begin
  if GI_EditCmds <> nil then
    GI_EditCmds.ExecCut;
end;

procedure TCommandsDataModule.actEditCopyExecute(Sender: TObject);
begin
  if GI_EditCmds <> nil then
    GI_EditCmds.ExecCopy;
end;

procedure TCommandsDataModule.actEditPasteExecute(Sender: TObject);
begin
  if GI_EditCmds <> nil then
    GI_EditCmds.ExecPaste;
end;

procedure TCommandsDataModule.actEditDeleteExecute(Sender: TObject);
begin
  if GI_EditCmds <> nil then
    GI_EditCmds.ExecDelete;
end;

procedure TCommandsDataModule.actEditSelectAllExecute(Sender: TObject);
begin
  if GI_EditCmds <> nil then
    GI_EditCmds.ExecSelectAll;
end;

procedure TCommandsDataModule.actEditRedoExecute(Sender: TObject);
begin
  if GI_EditCmds <> nil then
    GI_EditCmds.ExecRedo;
end;

procedure TCommandsDataModule.actEditUndoExecute(Sender: TObject);
begin
  if GI_EditCmds <> nil then
    GI_EditCmds.ExecUndo;
end;

procedure TCommandsDataModule.actSearchFindExecute(Sender: TObject);
begin
  if GI_SearchCmds <> nil then
    GI_SearchCmds.ExecFind
  else if PythonIIForm.HasFocus then
    PythonIIForm.ExecFind;
end;

procedure TCommandsDataModule.actSearchFindNextExecute(Sender: TObject);
begin
  if GI_SearchCmds <> nil then
    GI_SearchCmds.ExecFindNext
  else if PythonIIForm.HasFocus then
    PythonIIForm.ExecFindNext;
end;

procedure TCommandsDataModule.actSearchFindPrevExecute(Sender: TObject);
begin
  if GI_SearchCmds <> nil then
    GI_SearchCmds.ExecFindPrev
  else if PythonIIForm.HasFocus then
    PythonIIForm.ExecFindPrev;
end;

procedure TCommandsDataModule.actSearchReplaceExecute(Sender: TObject);
begin
  if GI_SearchCmds <> nil then
    GI_SearchCmds.ExecReplace
  else if PythonIIForm.HasFocus then
    PythonIIForm.ExecReplace;
end;

procedure TCommandsDataModule.actSearchGoToLineExecute(Sender: TObject);
Var
  Line : String;
  LineNo : integer;
begin
  if Assigned(GI_ActiveEditor) then
    if InputQuery('Go to line number', 'Enter line number:', Line) then begin
      LineNo := StrToInt(Line);
      GI_ActiveEditor.SynEdit.CaretXY := BufferCoord(1, LineNo);
    end;
end;

procedure TCommandsDataModule.actFindInFilesExecute(Sender: TObject);
begin
  if Assigned(FindResultsWindow) then
    FindResultsWindow.Execute(False)
end;

procedure TCommandsDataModule.actUnitTestWizardExecute(Sender: TObject);
Var
  Tests : string;
  Editor : IEditor;
begin
  if Assigned(GI_ActiveEditor) and GI_ActiveEditor.HasPythonFile then begin
    Tests := TUnitTestWizard.GenerateTests(GI_ActiveEditor.GetFileNameOrTitle,
      GI_ActiveEditor.SynEdit.Text);
    if Tests <> '' then begin
      Editor := PyIDEMainForm.DoOpenFile('', 'Python');
      if Assigned(Editor) then begin
        Editor.SynEdit.Text := Tests;
        Editor.SynEdit.Modified := True;
      end;
    end;
  end;
end;

procedure TCommandsDataModule.ApplyEditorOptions;
// Assign Editor Options to all open editors
var
  i : integer;
begin
  for i := 0 to GI_EditorFactory.Count - 1 do
    GI_EditorFactory.Editor[i].SynEdit.Assign(EditorOptions);

  InterpreterEditorOptions.Assign(EditorOptions);
  InterpreterEditorOptions.Options := InterpreterEditorOptions.Options -
    [eoTrimTrailingSpaces, eoTabsToSpaces];
  PythonIIForm.SynEdit.Assign(InterpreterEditorOptions);
  PythonIIForm.RegisterHistoryCommands;
  PythonIIForm.SynEdit.Highlighter.Assign(CommandsDataModule.SynPythonSyn);
end;

procedure TCommandsDataModule.actEditorOptionsExecute(Sender: TObject);
var
  TempEditorOptions : TSynEditorOptionsContainer;
begin
  TempEditorOptions := TSynEditorOptionsContainer.Create(Self);
  try
    with TSynEditOptionsDialog.Create(Self) do begin
      if Assigned(GI_ActiveEditor) then begin
        TempEditorOptions.Assign(GI_ActiveEditor.SynEdit);
        Form.cbApplyToAll.Checked := False;
        Form.cbApplyToAll.Enabled := True;
      end else begin
        TempEditorOptions.Assign(EditorOptions);
        Form.cbApplyToAll.Checked := True;
        Form.cbApplyToAll.Enabled := False;
      end;
      OnGetHighlighterCount := SynEditOptionsDialogGetHighlighterCount;
      OnGetHighlighter := SynEditOptionsDialogGetHighlighter;
      OnSetHighlighter := SynEditOptionsDialogSetHighlighter;
      VisiblePages := [soDisplay, soOptions, soKeystrokes, soColor];
      GetUserCommand := GetEditorUserCommand;
      UseExtendedStrings := True;
      if Execute(TempEditorOptions) then begin
        UpdateHighlighters;
        if Form.cbApplyToAll.Checked then begin
          EditorOptions.Assign(TempEditorOptions);
          ApplyEditorOptions;
        end else if Assigned(GI_ActiveEditor) then
          GI_ActiveEditor.SynEdit.Assign(TempEditorOptions);
      end;
      Free;
    end;
  finally
    TempEditorOptions.Free;
  end;
end;

procedure TCommandsDataModule.actInterpreterEditorOptionsExecute(
  Sender: TObject);
var
  TempEditorOptions : TSynEditorOptionsContainer;
begin
  TempEditorOptions := TSynEditorOptionsContainer.Create(Self);
  try
    with TSynEditOptionsDialog.Create(Self) do begin
      TempEditorOptions.Assign(PythonIIForm.SynEdit);
      Form.cbApplyToAll.Checked := False;
      Form.cbApplyToAll.Enabled := False;
      Form.Caption := 'Interpreter Editor Options';
      OnGetHighlighterCount := SynEditOptionsDialogGetHighlighterCount;
      OnGetHighlighter := SynEditOptionsDialogGetHighlighter;
      OnSetHighlighter := SynEditOptionsDialogSetHighlighter;
      VisiblePages := [soDisplay, soOptions];
      if Execute(TempEditorOptions) then begin
        InterpreterEditorOptions.Assign(TempEditorOptions);
        InterpreterEditorOptions.Options := InterpreterEditorOptions.Options -
          [eoTrimTrailingSpaces, eoTabsToSpaces];
        PythonIIForm.SynEdit.Assign(InterpreterEditorOptions);
      end;
      Free;
    end;
  finally
    TempEditorOptions.Free;
  end;
end;

procedure TCommandsDataModule.actEditIndentExecute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    GI_ActiveEditor.SynEdit.ExecuteCommand(ecBlockIndent, ' ', nil);
end;

procedure TCommandsDataModule.actEditDedentExecute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    GI_ActiveEditor.SynEdit.ExecuteCommand(ecBlockUnIndent, ' ', nil);
end;

procedure TCommandsDataModule.actEditToggleCommentExecute(Sender: TObject);
var
  i : integer;
  BlockIsCommented : Boolean;
begin
  if Assigned(GI_ActiveEditor) then with GI_ActiveEditor.SynEdit do begin
    BlockIsCommented := True;
    for i  := BlockBegin.Line to BlockEnd.Line do
      if Copy(Lines[i-1], 1, 2) <> '##' then begin
        BlockIsCommented := False;
        Break;
      end;

    if BlockIsCommented then
      actEditUncommentExecute(Sender)
    else
      actEditCommentOutExecute(Sender);
  end;
end;

procedure TCommandsDataModule.actEditCommentOutExecute(Sender: TObject);
var
  S: WideString;
  Offset: integer;
  OldBlockBegin, OldBlockEnd : TBufferCoord;
begin
  if Assigned(GI_ActiveEditor) then with GI_ActiveEditor.SynEdit do begin
    OldBlockBegin := BlockBegin;
    OldBlockEnd := BlockEnd;
    if SelAvail then begin // has selection
      OldBlockBegin := BufferCoord(1, OldBlockBegin.Line);
      BlockBegin := OldBlockBegin;
      BlockEnd := OldBlockEnd;
      BeginUpdate;
      S:='##'+SelText;
      Offset:=0;
      if S[Length(S)]=#10 then begin // if the selection ends with a newline, eliminate it
        if S[Length(S)-1]=#13 then // do we ignore 1 or 2 chars?
          Offset:=2
        else
          Offset:=1;
        S:=Copy(S, 1, Length(S)-Offset);
      end;
      S := WideStringReplace(S, #10, #10'##', [rfReplaceAll]);
      if Offset=1 then
        S:=S+#10
      else if Offset=2 then
        S:=S + WideLineBreak;
      SelText := S;
      EndUpdate;
      BlockBegin := OldBlockBegin;
      BlockEnd := BufferCoord(OldBlockEnd.Char + 2, OldBlockEnd.Line);
    end
    else  begin // no selection; easy stuff ;)
      // Do with selection to be able to undo
      //LineText:='##'+LineText;
      CaretXY := BufferCoord(1, CaretY);
      SelText := '##';
      CaretXY := BufferCoord(OldBlockEnd.Char + 2, OldBlockEnd.Line);
    end;
    UpdateCaret;
  end;
end;

procedure TCommandsDataModule.actEditUncommentExecute(Sender: TObject);
Var
  OldBlockBegin, OldBlockEnd : TBufferCoord;
begin
  if Assigned(GI_ActiveEditor) then with GI_ActiveEditor.SynEdit do begin
    OldBlockBegin := BlockBegin;
    OldBlockEnd := BlockEnd;
    if SelAvail then
    begin
      OldBlockBegin := BufferCoord(1, OldBlockBegin.Line);
      BlockBegin := OldBlockBegin;
      BlockEnd := OldBlockEnd;
      SelText := CommentLineRE.Replace(SelText, '', False);
      BlockBegin := OldBlockBegin;
      BlockEnd := BufferCoord(OldBlockEnd.Char - 2, OldBlockEnd.Line);
    end else begin
      BlockBegin := BufferCoord(1, CaretY);
      BlockEnd := BufferCoord(Length(LineText), CaretY);
      SelText := CommentLineRE.Replace(SelText, '', False);
      CaretXY := BufferCoord(OldBlockEnd.Char - 2, OldBlockEnd.Line);
    end;
    UpdateCaret;
  end;
end;

procedure TCommandsDataModule.actEditTabifyExecute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then with GI_ActiveEditor.SynEdit do begin
    if SelAvail then
    begin
       SelText :=  WideStringReplace(SelText,
         StringOfChar(' ',GI_ActiveEditor.SynEdit.TabWidth), #9, [rfReplaceAll]);
       UpdateCaret;
    end;
  end;
end;

procedure TCommandsDataModule.actEditUntabifyExecute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then with GI_ActiveEditor.SynEdit do begin
    if SelAvail then
    begin
       SelText :=  WideStringReplace(SelText, #9,
         StringOfChar(' ',GI_ActiveEditor.SynEdit.TabWidth), [rfReplaceAll]);
       UpdateCaret;
    end;
  end;
end;

procedure TCommandsDataModule.actEditLBExecute(Sender: TObject);
begin
  if (Sender is TAction) and Assigned(GI_ActiveEditor) then begin
    (GI_ActiveEditor.SynEdit.Lines as TSynEditStringList).FileFormat :=
      TSynEditFileFormat(TAction(Sender).Tag);
    GI_ActiveEditor.SynEdit.Modified := True;
  end;
end;

procedure TCommandsDataModule.actEditUTF8Execute(Sender: TObject);
begin
  if (Sender is TAction) and Assigned(GI_ActiveEditor) then begin
    if TAction(Sender).Checked then
      GI_ActiveEditor.FileEncoding := seAnsi
    else
      GI_ActiveEditor.FileEncoding := seUTF8;
    GI_ActiveEditor.SynEdit.Modified := True;
  end;
end;

procedure TCommandsDataModule.actSearchMatchingBraceExecute(
  Sender: TObject);
Var
  P : TBufferCoord;
begin
  if Assigned(GI_ActiveEditor) then with GI_ActiveEditor.SynEdit do begin
    P := GetMatchingBracket;
    if (P.Char > 0) and (P.Line > 0) then
      CaretXY := P;
  end;
end;

function TCommandsDataModule.IsBlockCloser(S: string): Boolean;
begin
  Result := BlockCloserRE.Exec(S);
end;

function TCommandsDataModule.IsBlockOpener(S: string): Boolean;
begin
  Result := BlockOpenerRE.Exec(S);
end;

function TCommandsDataModule.IsExecutableLine(Line: string): Boolean;
begin
  Result := not ((Line = '') or NonExecutableLineRE.Exec(Line));
end;

function TCommandsDataModule.CleanEOLs(S: string): string;
begin
  //Result := EOLCleanerRE.Replace(S, #10, False);
  Result := AdjustLineBreaks(S, tlbsLF)
end;

procedure TCommandsDataModule.PaintMatchingBrackets(Canvas : TCanvas;
  SynEdit : TSynEdit; TransientType: TTransientType);
{-----------------------------------------------------------------------------
  Based on code from devcpp (dev-cpp.sf.net)
-----------------------------------------------------------------------------}
const
  Brackets: array[0..5] of char = ('(', ')', '[', ']', '{', '}');

  function CharToPixels(P: TBufferCoord): TPoint;
  begin
    Result:= SynEdit.RowColumnToPixels(SynEdit.BufferToDisplayPos(P));
  end;

  procedure GetMatchingBrackets(P : TBufferCoord; out PM : TBufferCoord;
    out IsBracket, HasMatchingBracket : Boolean; out BracketCh, MatchCh : Char;
    out Attri: TSynHighlighterAttributes);
  var
    S: WideString;
    I: Integer;
  begin
    IsBracket := False;
    HasMatchingBracket := False;
    PM := BufferCoord(0,0);
    SynEdit.GetHighlighterAttriAtRowCol(P, S, Attri);
    if Assigned(Attri) and (SynEdit.Highlighter.SymbolAttribute = Attri) and
        (SynEdit.CaretX<=length(SynEdit.LineText) + 1) then begin
      for i := Low(Brackets) to High(Brackets) do
        if S = Brackets[i] then begin
          BracketCh := Brackets[i];
          IsBracket := True;
          PM := SynEdit.GetMatchingBracketEx(P);
          if (PM.Char > 0) then begin
            HasMatchingBracket := True;
            MatchCh := Brackets[i xor 1];
          end;
          break;
        end;
    end;
  end;

var
  P, PM: TBufferCoord;
  PD, PMD : TDisplayCoord;
  Pix: TPoint;
  BracketCh, MatchCh : Char;
  IsBracket, HasMatchingBracket : Boolean;
  Attri: TSynHighlighterAttributes;
begin
  P := SynEdit.CaretXY;

  // First Look at the previous character like Site
  if P.Char > 1 then Dec(P.Char);
  GetMatchingBrackets(P, PM, IsBracket, HasMatchingBracket, BracketCh, MatchCh, Attri);

  //if it is not a bracket then look at the next character;
  if not IsBracket and (SynEdit.CaretX > 1) then begin
    Inc(P.Char);
    GetMatchingBrackets(P, PM, IsBracket, HasMatchingBracket, BracketCh, MatchCh, Attri);
  end;

  if IsBracket then begin
    PD := SynEdit.BufferToDisplayPos(P);
    Pix := SynEdit.RowColumnToPixels(PD);
    // Calculate here as a workaround to UniSynEdit quirk in which
    // BufferToDisplayPos resets the font
    if HasMatchingBracket then
      PMD := SynEdit.BufferToDisplayPos(PM);

    Canvas.Brush.Style := bsSolid;
    Canvas.Font.Assign(SynEdit.Font);
    Canvas.Font.Style := Attri.Style;

    if SynEdit.IsPointInSelection(P) then
      Canvas.Brush.Color := SynEdit.SelectedColor.Background
    else if (Synedit.ActiveLineColor <> clNone) and (SynEdit.CaretY = P.Line) then
      Canvas.Brush.Color := SynEdit.ActiveLineColor
    else if Attri.Background <> clNone then
      Canvas.Brush.Color := Attri.Background
    else if SynPythonSyn.SpaceAttri.Background <> clNone then
      Canvas.Brush.Color := SynPythonSyn.SpaceAttri.Background
    else
      Canvas.Brush.Color := Synedit.Color;

    if (TransientType = ttAfter) then begin
      if HasMatchingBracket then
        Canvas.Font.Color:= clBlue
      else
        Canvas.Font.Color:= clRed;

      Canvas.Font.Style := Canvas.Font.Style + [fsBold];
    end
    else begin
      Canvas.Font.Style := Attri.Style;
      Canvas.Font.Color:= Attri.Foreground;;
    end;

    if (PD.Column >= SynEdit.LeftChar) and
      (PD.Column < SynEdit.LeftChar + SynEdit.CharsInWindow) and
      (PD.Row > 0)and (PD.Row >= SynEdit.TopLine) and
      (PD.Row < SynEdit.TopLine + SynEdit.LinesInWindow) then
    Canvas.TextOut(Pix.X, Pix.Y, BracketCh);

    if HasMatchingBracket and (PMD.Column >= SynEdit.LeftChar) and
      (PMD.Column < SynEdit.LeftChar + SynEdit.CharsInWindow) and
      (PMD.Row > 0)and (PMD.Row >= SynEdit.TopLine) and
      (PMD.Row < SynEdit.TopLine + SynEdit.LinesInWindow) then
    begin
      if SynEdit.IsPointInSelection(PM) then
        Canvas.Brush.Color := SynEdit.SelectedColor.Background
      else if (Synedit.ActiveLineColor <> clNone) and (SynEdit.CaretY = PM.Line) then
        Canvas.Brush.Color := SynEdit.ActiveLineColor
      else if Attri.Background <> clNone then
        Canvas.Brush.Color := Attri.Background
      else if SynPythonSyn.SpaceAttri.Background <> clNone then
        Canvas.Brush.Color := SynPythonSyn.SpaceAttri.Background
      else
        Canvas.Brush.Color := Synedit.Color;
      Pix := SynEdit.RowColumnToPixels(PMD);
      Canvas.TextOut(Pix.X, Pix.Y, MatchCh);
    end;
  end;

  Canvas.Brush.Style := bsSolid;
end;


procedure TCommandsDataModule.UpdateChangeNotify;
Var
  i, j : integer;
  Editor : IEditor;
  SR: TSearchRec;
  DirIsMonitored : Boolean;
  FileDrive : string;
begin
  if not Assigned(JvChangeNotify.OnChangeNotify) then
    Exit;  // JvChangeNotify was disconnected
  JvChangeNotify.Active := False;
  fChangeNotifyFileNames.Clear;
  JvChangeNotify.Notifications.BeginUpdate;
  try
    JvChangeNotify.Notifications.Clear;
    for i := 0 to GI_EditorFactory.Count - 1 do begin
      Editor := GI_EditorFactory[i];
      if (Editor.FileName <> '') and
        (ExcludedFileNotificationdDrives.IndexOf(ExtractFileDrive(Editor.FileName)) < 0) then
      begin
        if FindFirst(Editor.FileName, faAnyFile, SR) = 0 then begin
          fChangeNotifyFileNames.AddObject(Editor.FileName, TObject(SR.Time));
          DirIsMonitored := False;
          for j := 0 to JvChangeNotify.Notifications.Count - 1 do
            if JvChangeNotify.Notifications[j].Directory = ExtractFileDir(Editor.FileName) then begin
              DirIsMonitored := True;
              break;
            end;
          if not DirIsMonitored then with JvChangeNotify.Notifications.Add do begin
            Directory := ExtractFileDir(Editor.FileName);
            Actions := [caChangeFileName, caChangeDirName, caChangeLastWrite];
            //Actions := [caChangeDirName, caChangeFileName];
            IncludeSubTrees := False;
          end;
        end;
        FindClose(SR);
      end;
    end;
  finally
    JvChangeNotify.Notifications.EndUpdate;
  end;
  try
    JvChangeNotify.Active := JvChangeNotify.Notifications.Count > 0;
  except
    on E: EJVCLChangeNotifyException do begin
      FileDrive := ExtractFileDrive(E.ErrorDirectory);
      if ExcludedFileNotificationdDrives.IndexOf(FileDrive) < 0 then begin
        MessageDlg('File change notification not available for drive ' + FileDrive,
          mtWarning, [mbOK], 0);
        ExcludedFileNotificationdDrives.Add(FileDrive);
        // try again - recursive call
        UpdateChangeNotify;
      end;
    end;
  end;
end;

procedure TCommandsDataModule.JvChangeNotifyChangeNotify(Sender: TObject;
  Dir: String; Actions: TJvChangeActions);
Var
  i : integer;
  SR: TSearchRec;
  P : TBufferCoord;
  Editor : IEditor;
begin
  if not Assigned(JvChangeNotify.OnChangeNotify) then
    Exit;  // JvChangeNotify was disconnected
  // Iterate downwords so that we can remove items
  for i := fChangeNotifyFileNames.Count - 1 downto 0 do
    if ExtractFileDir(fChangeNotifyFileNames[i]) = Dir then begin
      Editor := GI_EditorFactory.GetEditorByName(fChangeNotifyFileNames[i]);
      if not Assigned(Editor) then continue;
      if FindFirst(fChangeNotifyFileNames[i], faAnyFile, SR) <> 0 then begin
        // File or directory has been moved or deleted
        MessageDlg(fChangeNotifyFileNames[i]+' has been renamed or deleted.', mtWarning, [mbOK], 0);
        // Mark as modified so that we try to save it
        Editor.SynEdit.Modified := True;
        // Delete form ChangeNotifyFileNames list to prevent further notifications
        fChangeNotifyFileNames.Delete(i);
      end else if Integer(fChangeNotifyFileNames.Objects[i]) <> SR.Time then begin
        // Timestamp changed
        if MessageDlg(fChangeNotifyFileNames[i]+' has changed. Reload from disk?',
          mtConfirmation, [mbYes, mbNo], 0)=mrYes
        then begin
          P := Editor.Synedit.CaretXY;
          Editor.OpenFile(fChangeNotifyFileNames[i]);
          if (P.Line <= Editor.SynEdit.Lines.Count) then
            Editor.SynEdit.CaretXY := P;
        end else begin
          // Mark as modified so that we try to save it
          Editor.SynEdit.Modified := True;
          // Prevent further notifications on this file !?;
          fChangeNotifyFileNames.Objects[i] := TObject(SR.Time);
        end;
      end;
      FindClose(SR);
    end;
end;

procedure TCommandsDataModule.PrepareParameterCompletion;
var
  i : integer;
  ParamName, ParamValue : string;
begin
  with ParameterCompletion do begin
    ItemList.Clear;
    InsertList.Clear;
    for i := 0 to Parameters.Count - 1 do begin
      Parameters.Split(i, ParamName, ParamValue, False);
      ItemList.Add(Format('\color{clBlue}%s\color{clBlack}\column{}%s',
         [ParamName, StringReplace(ParamValue, '\', '\\', [rfReplaceAll])]));
      InsertList.Add(ParamName);
    end;
  end;
end;

procedure TCommandsDataModule.PrepareModifierCompletion;
var
  i : integer;
  ModName, ModComment : string;
begin
  with ModifierCompletion do begin
    ItemList.Clear;
    InsertList.Clear;
    for i := 0 to Parameters.Modifiers.Count - 1 do begin
      ModName := Parameters.Modifiers.Names[i];
      ModComment := Parameters.Modifiers.Values[ModName];
      ItemList.Add(Format('\color{clBlue}%s\color{clBlack}\column{}%s', [ModName, ModComment]));
      InsertList.Add(ModName);
    end;
  end;
end;

procedure TCommandsDataModule.actIDEOptionsExecute(Sender: TObject);
Var
  Categories : array of TOptionCategory;
  Reg : TRegistry;
  IsRegistered : Boolean;
  Key : string;
begin
  SetLength(Categories, 7);
  with Categories[0] do begin
    DisplayName := 'IDE';
    SetLength(Options, 4);
    Options[0].PropertyName := 'AutoCheckForUpdates';
    Options[0].DisplayName := 'Check for updates automatically';
    Options[1].PropertyName := 'DaysBetweenChecks';
    Options[1].DisplayName := 'Days between update checks';
    Options[2].PropertyName := 'MaskFPUExceptions';
    Options[2].DisplayName := 'Mask FPU Exceptions';
    Options[3].PropertyName := 'UseBicycleRepairMan';
    Options[3].DisplayName := 'Use BicycleRepairMan';
  end;
  with Categories[1] do begin
    DisplayName := 'Python Interpreter';
    SetLength(Options, 5);
    Options[0].PropertyName := 'SaveFilesBeforeRun';
    Options[0].DisplayName := 'Save files before run';
    Options[1].PropertyName := 'TimeOut';
    Options[1].DisplayName := 'Timeout for running scripts in ms';
    Options[2].PropertyName := 'UTF8inInterpreter';
    Options[2].DisplayName := 'UTF8 in interactive interpreter';
    Options[3].PropertyName := 'CleanupMainDict';
    Options[3].DisplayName := 'Clean up namespace after run';
    Options[4].PropertyName := 'CleanupSysModules';
    Options[4].DisplayName := 'Clean up sys.modules after run';
  end;
  with Categories[2] do begin
    DisplayName := 'Code Explorer';
    SetLength(Options, 1);
    Options[0].PropertyName := 'ExporerInitiallyExpanded';
    Options[0].DisplayName := 'Initially expanded';
  end;
  with Categories[3] do begin
    DisplayName := 'File Filters';
    SetLength(Options, 5);
    Options[0].PropertyName := 'PythonFileFilter';
    Options[0].DisplayName := 'Open Dialog Python Filter';
    Options[1].PropertyName := 'HTMLFileFilter';
    Options[1].DisplayName := 'Open Dialog HTML Filter';
    Options[2].PropertyName := 'XMLFileFilter';
    Options[2].DisplayName := 'Open Dialog XML Filter';
    Options[3].PropertyName := 'CSSFileFilter';
    Options[3].DisplayName := 'Open Dialog CSS Filter';
    Options[4].PropertyName := 'FileExplorerFilter';
    Options[4].DisplayName := 'File Explorer Filter';
  end;
  with Categories[4] do begin
    DisplayName := 'Editor';
    SetLength(Options, 9);
    Options[0].PropertyName := 'RestoreOpenFiles';
    Options[0].DisplayName := 'Restore open files';
    Options[1].PropertyName := 'SearchTextAtCaret';
    Options[1].DisplayName := 'Search text at caret';
    Options[2].PropertyName := 'CreateBackupFiles';
    Options[2].DisplayName := 'Create backup files';
    Options[3].PropertyName := 'UndoAfterSave';
    Options[3].DisplayName := 'Undo after save';
    Options[4].PropertyName := 'ShowCodeHints';
    Options[4].DisplayName := 'Show code hints';
    Options[5].PropertyName := 'ShowDebuggerHints';
    Options[5].DisplayName := 'Show debugger hints';
    Options[6].PropertyName := 'AutoCompleteBrackets';
    Options[6].DisplayName := 'Auto-complete brackets';
    Options[7].PropertyName := 'MarkExecutableLines';
    Options[7].DisplayName := 'Show executable line marks';
    Options[8].PropertyName := 'CheckSyntaxAsYouType';
    Options[8].DisplayName := 'Check syntax as you type';
  end;
  with Categories[5] do begin
    DisplayName := 'Code Completion';
    SetLength(Options, 1);
    Options[0].PropertyName := 'SpecialPackages';
    Options[0].DisplayName := 'Special Packages';
  end;
  with Categories[6] do begin
    DisplayName := 'Shell Integration';
    SetLength(Options, 1);
    Options[0].PropertyName := 'FileExplorerContextMenu';
    Options[0].DisplayName := 'File Explorer Context Menu';
  end;

  // Shell Integration
  IsRegistered := False;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Key := 'Python.File\shell\Edit with PyScripter';
    IsRegistered := Reg.KeyExists(Key);
  except
  end;
  PyIDEOptions.FileExplorerContextMenu := IsRegistered;

  PyIDEOptions.SearchTextAtCaret := EditorSearchOptions.SearchTextAtCaret;

  if InspectOptions(PyIDEOptions, Categories, 'IDE Options', 610) then begin
    PyIDEMainForm.PyIDEOptionsChanged;
    if PyIDEOptions.FileExplorerContextMenu <> IsRegistered then begin
      try
        if IsRegistered then begin
          Reg.DeleteKey(Key)
        end else begin
          Reg.OpenKey(Key, True);
          Reg.CloseKey;
          Reg.OpenKey(Key + '\command', True);
          Reg.WriteString('', '"'+ Application.ExeName+ '" "%1"') ;
          Reg.CloseKey;
        end;

        SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
      except
        MessageDlg('Registry access denied. Could not change the file association.',
          mtError, [mbOK], 0);
      end;
    end;
  end;
  Reg.Free;
end;

procedure TCommandsDataModule.actIDEShortcutsExecute(Sender: TObject);
begin
  with TfrmCustomKeyboard.Create(Self) do begin
    Execute(PyIDEMainForm.ActionListArray);
    Release;
  end;
end;

procedure TCommandsDataModule.actPythonPathExecute(Sender: TObject);
Var
  Paths : TStringList;
  i : integer;
  PythonPath : Variant;
begin
  Paths := TStringList.Create;
  try
    for i := 0 to Len(SysModule.path) - 1  do
      Paths.Add(SysModule.path.GetItem(i));
    if EditFolderList(Paths, 'Python Path', 870) then begin
      PythonPath := NewPythonList;
      for i := 0 to Paths.Count - 1 do
        PythonPath.append(Paths[i]);
      SysModule.path := PythonPath;
    end;
  finally
    Paths.Free;
  end;
end;

procedure TCommandsDataModule.actAboutExecute(Sender: TObject);
begin
  with TAboutBox.Create(Self) do begin
    ShowModal;
    Release;
  end;
end;

procedure TCommandsDataModule.actPythonManualsExecute(Sender: TObject);
Var
  OldHelpFile : string;
begin
  if PythonIIForm.PythonHelpFile <> '' then begin
    OldHelpFile := Application.HelpFile;
    Application.HelpFile := PythonIIForm.PythonHelpFile;
    PyIDEMainForm.MenuHelpRequested := True;
    try
      Application.HelpCommand(HELP_CONTENTS, 0);
    finally
      Application.HelpFile := OldHelpFile;
      PyIDEMainForm.MenuHelpRequested := False;
    end;
  end;
end;

function TCommandsDataModule.ShowPythonKeywordHelp(KeyWord : string): Boolean;
Var
  OldHelpFile : string;
begin
  Result := False;
  if PythonIIForm.PythonHelpFile <> '' then begin
    OldHelpFile := Application.HelpFile;
    Application.HelpFile := PythonIIForm.PythonHelpFile;
    PyIDEMainForm.PythonKeywordHelpRequested := True;
    try
      Result := Application.HelpKeyword(KeyWord);
    finally
      PyIDEMainForm.PythonKeywordHelpRequested := False;
      Application.HelpFile := OldHelpFile;
    end;
  end;
end;

procedure TCommandsDataModule.UpdateMainActions;
Var
  i : integer;
  SelAvail : Boolean;
  SaveAll : Boolean;
begin
  // Edit actions
//  actEditCut.Enabled := (GI_EditCmds <> nil) and GI_EditCmds.CanCut;
//  actEditCopy.Enabled := (GI_EditCmds <> nil) and GI_EditCmds.CanCopy;
//  actEditPaste.Enabled := (GI_EditCmds <> nil) and GI_EditCmds.CanPaste;
  actEditDelete.Enabled := (GI_EditCmds <> nil) and GI_EditCmds.CanDelete;
//  actEditSelectAll.Enabled := (GI_EditCmds <> nil) and GI_EditCmds.CanSelectAll;
//  actEditUndo.Enabled := (GI_EditCmds <> nil) and GI_EditCmds.CanUndo;
  actEditRedo.Enabled := (GI_EditCmds <> nil) and GI_EditCmds.CanRedo;
  actEditLBDos.Checked := Assigned(GI_ActiveEditor) and
    ((GI_ActiveEditor.SynEdit.Lines as TSynEditStringList).FileFormat = sffDos);
  actEditLBUnix.Checked := Assigned(GI_ActiveEditor) and
    ((GI_ActiveEditor.SynEdit.Lines as TSynEditStringList).FileFormat = sffUnix);
  actEditLBMac.Checked := Assigned(GI_ActiveEditor) and
    ((GI_ActiveEditor.SynEdit.Lines as TSynEditStringList).FileFormat = sffMac);
  actEditUTF8.Checked := Assigned(GI_ActiveEditor) and
    (GI_ActiveEditor.FileEncoding = seUTF8);

  SelAvail := Assigned(GI_ActiveEditor) and GI_ActiveEditor.SynEdit.SelAvail;
  // Source Code Actions
  actEditIndent.Enabled := SelAvail;
  actEditDedent.Enabled := SelAvail;
  actEditTabify.Enabled := SelAvail;
  actEditUnTabify.Enabled := SelAvail;
  actEditToggleComment.Enabled := Assigned(GI_ActiveEditor);
  actEditCommentOut.Enabled := Assigned(GI_ActiveEditor);
  actEditUncomment.Enabled := Assigned(GI_ActiveEditor);
  actEditLineNumbers.Enabled := Assigned(GI_ActiveEditor);
  actEditShowSpecialChars.Enabled := Assigned(GI_ActiveEditor);
  if Assigned(GI_ActiveEditor) then begin
    actEditLineNumbers.Checked := GI_ActiveEditor.SynEdit.Gutter.ShowLineNumbers;
    actEditShowSpecialChars.Checked := eoShowSpecialChars in GI_ActiveEditor.SynEdit.Options;
  end else begin
    actEditLineNumbers.Checked := False;
    actEditShowSpecialChars.Checked := False;
  end;

  // File Actions
  actFileClose.Enabled := (GI_FileCmds <> nil) and GI_FileCmds.CanClose;
  actFilePrint.Enabled := (GI_FileCmds <> nil) and GI_FileCmds.CanPrint;
  actPrintPreview.Enabled := actFilePrint.Enabled;
  actFileSave.Enabled := (GI_FileCmds <> nil) and GI_FileCmds.CanSave;
  actFileSaveAs.Enabled := (GI_FileCmds <> nil) and GI_FileCmds.CanSaveAs;
  // Lesson to remember do not change the Enabled state of an Action from false to true
  // within an Update or OnIdle handler. The result is 100% CPU utilisation.
  // Therefore I introduced here a new boolean variable.
  // actFileSaveAll.Enabled := False;
  SaveAll := False;
  for i := 0 to GI_EditorFactory.Count -1 do
    if (GI_EditorFactory[i] as IFileCommands).CanSave then begin
      SaveAll := True;
      break;
    end;
  actFileSaveAll.Enabled := SaveAll;

  // Search Actions
  actSearchFind.Enabled := ((GI_SearchCmds <> nil) and GI_SearchCmds.CanFind) or
    (PythonIIForm.HasFocus and PythonIIForm.CanFind);
  actSearchFindNext.Enabled := ((GI_SearchCmds <> nil) and GI_SearchCmds.CanFindNext) or
    (PythonIIForm.HasFocus  and PythonIIForm.CanFindNext);
  actSearchFindPrev.Enabled := actSearchFindNext.Enabled;
  actSearchReplace.Enabled := ((GI_SearchCmds <> nil) and GI_SearchCmds.CanReplace) or
    (PythonIIForm.HasFocus  and PythonIIForm.CanReplace);

  actSearchMatchingBrace.Enabled := Assigned(GI_ActiveEditor);
  actSearchGoToLine.Enabled := Assigned(GI_ActiveEditor);

  if Assigned(GI_ActiveEditor) and GI_ActiveEditor.HasPythonFile then begin
    actFindFunction.Enabled := True;
    actUnitTestWizard.Enabled := True;
  end else begin
    actFindFunction.Enabled := False;
    actUnitTestWizard.Enabled := False;
  end;

  // Parameter and Code Template Actions
  if Assigned(Screen.ActiveControl) and (Screen.ActiveControl is TSynEdit) then begin
    actParameterCompletion.Enabled := True;
    actModifierCompletion.Enabled := True;
    actReplaceParameters.Enabled := True;
    actInsertTemplate.Enabled := True;        
  end else begin
    actParameterCompletion.Enabled := False;
    actModifierCompletion.Enabled := False;
    actReplaceParameters.Enabled := False;
    actInsertTemplate.Enabled := False;
  end;
end;

procedure TCommandsDataModule.actHelpContentsExecute(Sender: TObject);
begin
  PyIDEMainForm.MenuHelpRequested := True;
  Application.HelpCommand(HELP_CONTENTS, 0);
  PyIDEMainForm.MenuHelpRequested := False;
end;

procedure TCommandsDataModule.ParameterCompletionCodeCompletion(
  Sender: TObject; var Value: WideString; Shift: TShiftState; Index: Integer;
  EndToken: WideChar);
begin
  if ssCtrl in Shift then
    Value := Parameters.Values[Value]
  else
    Value := Parameters.MakeParameter(Value);
end;

procedure TCommandsDataModule.ModifierCompletionCodeCompletion(
  Sender: TObject; var Value: WideString; Shift: TShiftState; Index: Integer;
  EndToken: WideChar);
var
  L: Integer;
begin
  if Assigned(ModifierCompletion.Editor) then
    with ModifierCompletion.Editor do begin
      SelText := '';
      L:= Length(Parameters.StopMask);
      if (CaretX > 0) and StrSame(Copy(LineText, CaretX-L, L), Parameters.StopMask) then begin
        CaretX:= CaretX - L;
        Value := '-' + Value;
      end else if not ((CaretX > 1) and (Lines[CaretY-1][CaretX-1] = '-')) then begin
        L:= StrLastPos(Parameters.StopMask, LineText);
        if L > 0 then CaretX:= L;
        Value := '-' + Value;
      end;
    end;
end;

procedure TCommandsDataModule.actParameterCompletionExecute(
  Sender: TObject);
begin
  if Assigned(Screen.ActiveControl) and (Screen.ActiveControl is TSynedit) then begin
    ParameterCompletion.Editor := TSynEdit(Screen.ActiveControl);
    ParameterCompletion.ActivateCompletion;
  end;
end;

procedure TCommandsDataModule.actModifierCompletionExecute(
  Sender: TObject);
begin
  if Assigned(Screen.ActiveControl) and (Screen.ActiveControl is TSynedit) then begin
    ModifierCompletion.Editor := TSynEdit(Screen.ActiveControl);
    ModifierCompletion.ActivateCompletion;
  end;
end;

procedure TCommandsDataModule.actReplaceParametersExecute(Sender: TObject);
var
  i, j: Integer;
  S : string;
  OldCaret : TBufferCoord;
begin
  if Assigned(Screen.ActiveControl) and (Screen.ActiveControl is TSynedit) then begin
    with TSynEdit(Screen.ActiveControl) do begin
      OldCaret := CaretXY;
      if SelAvail then
        SelText:= Parameters.ReplaceInText(SelText)
      else try
        BeginUpdate;
        with Parameters do begin
          for i:= 0 to Lines.Count-1 do begin
            S:= Lines[i];
            j:= AnsiPos(StartMask, S);
            if j > 0 then begin
              BeginUndoBlock;
              try
                BlockBegin:= BufferCoord(j, i+1);
                BlockEnd:= BufferCoord(Length(S)+1, i+1);
                SelText:= ReplaceInText(Copy(S, j, MaxInt));
              finally
                EndUndoBlock;
              end;
            end;
          end;
        end;
      finally
        EndUpdate;
      end;
      CaretXY := OldCaret;
    end;
  end;
end;

type
  TCrackSynAutoComplete = class(TSynAutoComplete)
  end;

procedure TCommandsDataModule.actInsertTemplateExecute(Sender: TObject);
Var
  SynEdit : TSynEdit;
begin
  if Assigned(Screen.ActiveControl) and (Screen.ActiveControl is TSynedit) then begin
    SynEdit := TSynEdit(Screen.ActiveControl);
    CodeTemplatesCompletion.Execute(TCrackSynAutoComplete(CodeTemplatesCompletion).
      GetPreviousToken(SynEdit), SynEdit);
  end;
end;

procedure TCommandsDataModule.actHelpParametersExecute(Sender: TObject);
begin
  PyIDEMainForm.MenuHelpRequested := True;
  Application.HelpJump('parameters');
  PyIDEMainForm.MenuHelpRequested := False;
end;

procedure TCommandsDataModule.actHelpExternalToolsExecute(Sender: TObject);
begin
  PyIDEMainForm.MenuHelpRequested := True;
  Application.HelpJump('externaltools');
  PyIDEMainForm.MenuHelpRequested := False;
end;

procedure TCommandsDataModule.actHelpEditorShortcutsExecute(
  Sender: TObject);
begin
  PyIDEMainForm.MenuHelpRequested := True;
  Application.HelpJump('editorshortcuts');
  PyIDEMainForm.MenuHelpRequested := False;
end;

procedure TCommandsDataModule.actCustomizeParametersExecute(
  Sender: TObject);
begin
  with TCustomizeParams.Create(Self) do begin
    SetItems(CustomParams);
    if ShowModal = mrOK then begin
      GetItems(CustomParams);
      RegisterCustomParams;
    end;
    Free;
  end;
end;

procedure TCommandsDataModule.actCodeTemplatesExecute(Sender: TObject);
begin
  with TCodeTemplates.Create(Self) do begin
    //SetItems(CodeTemplatesCompletion.AutoCompleteList);
    CodeTemplateText := CodeTemplatesCompletion.AutoCompleteList.Text;
    if ShowModal = mrOK then
      CodeTemplatesCompletion.AutoCompleteList.Text := CodeTemplateText;
    Free;
  end;
end;

procedure TCommandsDataModule.actFileTemplatesExecute(Sender: TObject);
begin
  with TFileTemplatesDialog.Create(Self) do begin
    SetItems;
    if ShowModal = mrOK then
      GetItems;
    Free;
  end;
end;

procedure TCommandsDataModule.actConfigureToolsExecute(Sender: TObject);
begin
  if ConfigureTools(ToolsCollection) then
    PyIDEMainForm.SetupToolsMenu;
end;

procedure TCommandsDataModule.actFindFunctionExecute(Sender: TObject);
begin
  JumpToFunction;
end;

procedure TCommandsDataModule.actEditLineNumbersExecute(Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    GI_ActiveEditor.SynEdit.Gutter.ShowLineNumbers := not
      GI_ActiveEditor.SynEdit.Gutter.ShowLineNumbers;
end;

procedure TCommandsDataModule.actEditShowSpecialCharsExecute(
  Sender: TObject);
begin
  if Assigned(GI_ActiveEditor) then
    if eoShowSpecialChars in GI_ActiveEditor.SynEdit.Options then
      GI_ActiveEditor.SynEdit.Options := GI_ActiveEditor.SynEdit.Options - [eoShowSpecialChars]
    else
      GI_ActiveEditor.SynEdit.Options := GI_ActiveEditor.SynEdit.Options + [eoShowSpecialChars]
end;

procedure TCommandsDataModule.actFindNextReferenceExecute(
  Sender: TObject);
var
  SearchOptions: TSynSearchOptions;
  SearchText : String;
  OldCaret : TBufferCoord;
begin
  if Assigned(GI_ActiveEditor) then with GI_ActiveEditor.SynEdit do begin
    SearchText :=  GetWordAtRowCol(CaretXY);
    if SearchText <> '' then begin
      OldCaret := CaretXY;

      SearchEngine := SynEditSearch;
      SearchOptions := [];
      if (Sender as TComponent).Tag = 1 then
        Include(SearchOptions, ssoBackwards)
      else
        CaretX := CaretX + 1;  //  So that we find the next identifier
      Include(SearchOptions, ssoMatchCase);
      Include(SearchOptions, ssoWholeWord);
      if SearchReplace(SearchText, '', SearchOptions) = 0 then begin
        CaretXY := OldCaret;
        MessageBeep(MB_ICONASTERISK);
        PyIDEMainForm.WriteStatusMsg(Format(SNotFound, [SearchText]));
      end else begin
        CaretXY := BlockBegin;
      end;
    end;
  end;
end;

procedure TCommandsDataModule.VirtualStringTreeAdvancedHeaderDraw(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);

  procedure PaintSeparator(Canvas: TCanvas; R: TRect; si: TTBXItemInfo);
  var
   Bmp: TBitmap;
  begin
   Bmp:= TBitmap.Create;
   try
    Bmp.PixelFormat := pf32Bit;
    Bmp.Width := R.Right - R.Left;
    Bmp.Height := R.Bottom - R.Top;

    Bmp.Canvas.CopyRect(Bmp.Canvas.ClipRect, Canvas, R);

    CurrentTheme.PaintSeparator(Bmp.Canvas, Bmp.Canvas.ClipRect, si, false, true);

    Canvas.Draw(R.Left, R.Top, Bmp);
   finally
    Bmp.Free;
   end;
  end;

Var
  R : TRect;
  ii: TTBXItemInfo;
begin
  R := PaintInfo.PaintRectangle;
  if (PaintInfo.Column = nil) and (hpeBackground in Elements) then begin
    CurrentTheme.PaintBackgnd(PaintInfo.TargetCanvas, R, R, R,
      CurrentTheme.GetViewColor(TVT_NORMALTOOLBAR), false, VT_TOOLBAR);
    //CurrentTheme.PaintDock(PaintInfo.TargetCanvas, R, R, DP_TOP);
  end else begin
    with PaintInfo do begin
      if hpeBackground in Elements then begin
        if IsHoverIndex and IsDownIndex then  ii := GetItemInfo('hot')
        else if IsHoverIndex then ii := GetItemInfo('active')
        else ii := GetItemInfo('inactive');

        //CurrentTheme.PaintBackgnd(TargetCanvas, R, R, R,
        //  CurrentTheme.GetViewColor(TVT_NORMALTOOLBAR), false, TVT_NORMALTOOLBAR);
        //CurrentTheme.PaintFrame(TargetCanvas, R, ii);
        if IsHoverIndex or IsDownIndex then begin
          Inc(R.Left, 1);
          Dec(R.Right,2);
          if IsDownIndex then
            CurrentTheme.PaintButton(TargetCanvas, R, ii);
            //CurrentTheme.PaintFrame(TargetCanvas, R, ii);
          TargetCanvas.Pen.Color := GetBorderColor('hot');
          TargetCanvas.Pen.Width := 2;
          TargetCanvas.MoveTo(R.Left, R.Bottom-2);
          TargetCanvas.LineTo(R.Right - 1 - 1, R.Bottom-2);
        end;// else
        // CurrentTheme.PaintFrame(TargetCanvas, R, ii);
        //CurrentTheme.PaintButton(TargetCanvas, R, ii);


        if ShowRightBorder then begin
          R := PaintInfo.PaintRectangle;
          R.Left := R.Right - 2;
          Inc(R.Right, 1);
          Dec(R.Top, 1);
          ii := GetItemInfo('inactive');
          PaintSeparator(TargetCanvas, R, ii);
        end;
      end;
    end;
  end;
end;

procedure TCommandsDataModule.VirtualStringTreeDrawQueryElements(
  Sender: TVTHeader; var PaintInfo: THeaderPaintInfo;
  var Elements: THeaderPaintElements);
begin
  Elements := [hpeBackground];
end;

procedure TCommandsDataModule.actCheckForUpdatesExecute(Sender: TObject);
//  If Sender is nil it is called from AutoCheck
//  Provide no confirmation
//var
//  _service : MainServiceSoap;
//  _result : ArrayOfString;
//  CurrentVersion : string;
//begin
//  _service := GetMainServiceSoap;
//  _result := _service.GetLatestVersionOf('PyScripter', CurrentVersion);
//  CurrentVersion := ApplicationVersion;
//  if CompareVersion(_result[0], CurrentVersion) > 0 then
//  begin
//    if MessageDlg('There is a newer version of PyScripter available at http://mmm-experts.com.'#13+
//                  'Do you want to see the details?', mtWarning, [mbOK, mbCancel], 0) = mrOK then
//      OpenObject(_result[1]);
//  end
//  else if Assigned(Sender) then
//    MessageDlg('Current version is uptodate!', mtInformation, [mbOK], 0);
begin
  try
    ProgramVersionCheck.Execute;
  except
    if Assigned(Sender) then
      raise
    else
      Exit;
  end;

  if Assigned(Sender) and not ProgramVersionCheck.IsRemoteProgramVersionNewer then
    if ProgramVersionCheck.DownloadError <> '' then
      MessageDlg('Error while downloading: ' +
        ProgramVersionCheck.DownloadError, mtError, [mbOK], 0)
    else
      MessageDlg('Current version is up-to-date!', mtInformation, [mbOK], 0);
  PyIDEOptions.DateLastCheckedForUpdates := Now;
end;

procedure TCommandsDataModule.GetEditorUserCommand(AUserCommand: Integer;
  var ADescription: String);
begin
  if AUserCommand = ecCodeCompletion then
    ADescription := 'Code Completion'
  else if AUserCommand = ecParamCompletion then
    ADescription := 'Param Completion';
end;

procedure TCommandsDataModule.DoSearchReplaceText(SynEdit : TSynEdit;
  AReplace: boolean;  ABackwards: boolean);
var
  Options: TSynSearchOptions;
begin
  if AReplace then
    Options := [ssoPrompt, ssoReplace, ssoReplaceAll]
  else
    Options := [];
  if ABackwards then
    Include(Options, ssoBackwards);
  if EditorSearchOptions.SearchCaseSensitive then
    Include(Options, ssoMatchCase);
  if not fSearchFromCaret then
    Include(Options, ssoEntireScope);
  if EditorSearchOptions.SearchSelectionOnly then
    Include(Options, ssoSelectedOnly);
  if EditorSearchOptions.SearchWholeWords then
    Include(Options, ssoWholeWord);
  if EditorSearchOptions.UseRegExp then
    SynEdit.SearchEngine := CommandsDataModule.SynEditRegexSearch
  else
    SynEdit.SearchEngine := CommandsDataModule.SynEditSearch;
  if SynEdit.SearchReplace(EditorSearchOptions.SearchText,
    EditorSearchOptions.ReplaceText, Options) = 0
  then
  begin
    MessageBeep(MB_ICONASTERISK);
    PyIDEMainForm.WriteStatusMsg(Format(SNotFound, [EditorSearchOptions.SearchText]));
    if ssoBackwards in Options then
      SynEdit.BlockEnd := SynEdit.BlockBegin
    else
      SynEdit.BlockBegin := SynEdit.BlockEnd;
    SynEdit.CaretXY := SynEdit.BlockBegin;
  end;

  if ConfirmReplaceDialog <> nil then
    ConfirmReplaceDialog.Free;
end;

procedure TCommandsDataModule.ShowSearchReplaceDialog(SynEdit : TSynEdit; AReplace: boolean);
var
  dlg: TTextSearchDialog;
begin
  if AReplace then
    dlg := TTextReplaceDialog.Create(Self)
  else
    dlg := TTextSearchDialog.Create(Self);
  with dlg do try
    // assign search options
    SearchBackwards := EditorSearchOptions.SearchBackwards;
    SearchCaseSensitive := EditorSearchOptions.SearchCaseSensitive;
    SearchFromCursor := EditorSearchOptions.SearchFromCaret;
    SearchInSelectionOnly := EditorSearchOptions.SearchSelectionOnly;
    // start with last search text
    SearchText := EditorSearchOptions.SearchText;
    if EditorSearchOptions.SearchTextAtCaret then begin
      // if something is selected search for that text
      if SynEdit.SelAvail and (SynEdit.BlockBegin.Line = SynEdit.BlockEnd.Line)
      then
        SearchText := SynEdit.SelText
      else
        SearchText := SynEdit.GetWordAtRowCol(SynEdit.CaretXY);
    end;
    SearchTextHistory := EditorSearchOptions.SearchTextHistory;
    if AReplace then with dlg as TTextReplaceDialog do begin
      ReplaceText := EditorSearchOptions.ReplaceText;
      ReplaceTextHistory := EditorSearchOptions.ReplaceTextHistory;
    end;
    SearchWholeWords := EditorSearchOptions.SearchWholeWords;
    SearchRegularExpression := EditorSearchOptions.UseRegExp;
    if ShowModal = mrOK then begin
      EditorSearchOptions.SearchBackwards := SearchBackwards;
      EditorSearchOptions.SearchCaseSensitive := SearchCaseSensitive;
      EditorSearchOptions.SearchFromCaret := SearchFromCursor;
      EditorSearchOptions.SearchSelectionOnly := SearchInSelectionOnly;
      EditorSearchOptions.SearchWholeWords := SearchWholeWords;
      EditorSearchOptions.UseRegExp := SearchRegularExpression;
      EditorSearchOptions.SearchText := SearchText;
      EditorSearchOptions.SearchTextHistory := SearchTextHistory;
      if AReplace then with dlg as TTextReplaceDialog do begin
        EditorSearchOptions.ReplaceText := ReplaceText;
        EditorSearchOptions.ReplaceTextHistory := ReplaceTextHistory;
      end;
      fSearchFromCaret := EditorSearchOptions.SearchFromCaret;
      if EditorSearchOptions.SearchText <> '' then begin
        DoSearchReplaceText(SynEdit, AReplace, EditorSearchOptions.SearchBackwards);
        fSearchFromCaret := TRUE;
      end;
    end;
  finally
    dlg.Free;
  end;
end;

resourcestring
  S_ERROR_NO_ERROR_STRING = 'Unspecified WinInet error';

function TCommandsDataModule.ProgramVersionHTTPLocationLoadFileFromRemote(
  AProgramVersionLocation: TJvProgramVersionHTTPLocation; const ARemotePath,
  ARemoteFileName, ALocalPath, ALocalFileName: string): string;

  function FileExistsNoDir(AFileName: string): Boolean;
  begin
    Result := FileExists(AFileName) and not DirectoryExists(AFileName);
  end;

  function GetLastErrorString: string;
  {
    Returns last error string, strangely enough.
  }
  var
    nLastError,
    nBufLength:   DWORD;
    pLastError:   PChar;
  begin
    nBufLength := INTERNET_MAX_PATH_LENGTH;
    pLastError := StrAlloc(INTERNET_MAX_PATH_LENGTH);
    try
      If not(InternetGetLastResponseInfo(nLastError, pLastError, nBufLength))
      or (StrLen(pLastError) = 0) then
        result := S_ERROR_NO_ERROR_STRING
      else
        result := StrPas(pLastError);
    finally
      StrDispose(pLastError);
    end;
  end;

  procedure CheckWinInetError(Error: Boolean);
  begin
    if Error then
      raise Exception.Create(GetLastErrorString);
  end;

var
  ResultStream: TFileStream;
  LocalFileName, URL : string;
  hNet, hUrl: HINTERNET;
  dwBytesAvail,
  dwBytesRead:  DWORD;
  Buffer: string;
begin
  Result := '';
  if (DirectoryExists(ALocalPath) or (ALocalPath = '')) then
    if ALocalFileName = '' then
      LocalFileName := PathAppend(ALocalPath, ARemoteFileName)
    else
      LocalFileName := PathAppend(ALocalPath, ALocalFileName)
  else
    Exit;

  if Copy(ARemotePath, Length(ARemotePath), 1) <> '/' then
    URL := ARemotePath + '/' + ARemoteFileName
  else
    URL := ARemotePath + ARemoteFileName;

  try
    ResultStream := TFileStream.Create(LocalFileName, fmCreate);
    try
      // initialize WinInet
      hNet := InternetOpen(
        'PyScripter',
        INTERNET_OPEN_TYPE_PRECONFIG,
        nil, nil, 0);
      CheckWinInetError(hNet = nil);

      try
        // open the file
        hUrl := InternetOpenUrl(hNet,
          PChar(URL), nil, 0,
          INTERNET_FLAG_PRAGMA_NOCACHE, 0);
        CheckWinInetError(hUrl = nil);

        try
          repeat
            CheckWinInetError(not InternetQueryDataAvailable(
              hUrl, dwBytesAvail, 0, 0));
            if dwBytesAvail <> 0 then begin
              SetLength(Buffer, dwBytesAvail);
              CheckWinInetError(not InternetReadFile(hUrl,
                PChar(Buffer),
                dwBytesAvail, dwBytesRead));
              CheckWinInetError(dwBytesAvail <> dwBytesRead);
              ResultStream.write(Buffer[1], dwBytesRead);
            end;
          until (dwBytesAvail = 0) or ((ProgramVersionCheck.Thread.Count > 0) and
            ProgramVersionCheck.Thread.Terminated);
        finally
          InternetCloseHandle(hUrl);
        end;

      finally
        InternetCloseHandle(hNet);
      end;
    finally
      ResultStream.Free;
    end;
  except
    on E: Exception do
      ProgramVersionHTTPLocation.DownloadError := E.Message;
  end;

  if FileExists(LocalFileName) then
    if (ProgramVersionHTTPLocation.DownloadError <> '') or
         ((ProgramVersionCheck.Thread.Count > 0) and
            ProgramVersionCheck.Thread.Terminated)
    then
      FileDelete(LocalFileName)
    else
      Result := LocalFileName;
end;

end.

















