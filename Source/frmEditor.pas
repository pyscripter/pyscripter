{ -----------------------------------------------------------------------------
  Unit Name: frmEditor
  Author:    Kiriakos Vlahos
  Date:      23-Feb-2005
  Purpose:
  History:   Origianlly Based on SynEdit Demo
  ----------------------------------------------------------------------------- }

unit frmEditor;

interface

uses
  Winapi.Messages,
  Winapi.D2D1,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Contnrs,
  System.ImageList,
  System.Threading,
  System.Messaging,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Menus,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  TB2Item,
  SpTBXItem,
  SpTBXSkins,
  SpTBXDkPanels,
  SpTBXTabs,
  SynEdit,
  SynEditTypes,
  SynEditMiscClasses,
  SynCompletionProposal,
  SynEditLsp,
  VirtualResources,
  FileSystemMonitor,
  uEditAppIntfs,
  cPySupportTypes;

type
  TEditor = class;

  THotIdentInfo = record
    HaveHotIdent: Boolean;
    StartCoord: TBufferCoord;
  end;

  TEditorForm = class(TForm)
    pmnuEditor: TSpTBXPopupMenu;
    pmnuViewsTab: TSpTBXPopupMenu;
    mnCloseTab: TSpTBXItem;
    SynEdit: TSynEdit;
    SynEdit2: TSynEdit;
    EditorSplitter: TSpTBXSplitter;
    mnUpdateView: TSpTBXItem;
    ViewsTabControl: TSpTBXTabControl;
    tabSource: TSpTBXTabItem;
    tbshSource: TSpTBXTabSheet;
    SpTBXRightAlignSpacerItem1: TSpTBXRightAlignSpacerItem;
    tbiUpdateView: TSpTBXItem;
    tbiCloseTab: TSpTBXItem;
    mnEditUndo: TSpTBXItem;
    mnEditRedo: TSpTBXItem;
    N5: TSpTBXSeparatorItem;
    mnEditCut: TSpTBXItem;
    mnEditCopy: TSpTBXItem;
    mnEditPaste: TSpTBXItem;
    TBXSeparatorItem9: TSpTBXSeparatorItem;
    mnSourceCode: TSpTBXSubmenuItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    mnSearch: TSpTBXSubmenuItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    mnMaximizeEditor2: TSpTBXItem;
    mnRestoreEditor2: TSpTBXItem;
    N12: TSpTBXSeparatorItem;
    mnEditorOptions: TSpTBXItem;
    BGPanel: TPanel;
    mnFoldVisible: TSpTBXItem;
    SpTBXSeparatorItem4: TSpTBXSeparatorItem;
    mnFold: TSpTBXSubmenuItem;
    mnUnfold: TSpTBXSubmenuItem;
    mnFoldAll: TSpTBXItem;
    mnUnfoldAll: TSpTBXItem;
    mnFoldNearest: TSpTBXItem;
    mnUnfoldNearest: TSpTBXItem;
    mnFoldRegions: TSpTBXItem;
    SpTBXSeparatorItem3: TSpTBXSeparatorItem;
    mnFoldLevel1: TSpTBXItem;
    mnUnfoldRegions: TSpTBXItem;
    SpTBXSeparatorItem5: TSpTBXSeparatorItem;
    mnUnfoldLevel1: TSpTBXItem;
    mnFoldLevel2: TSpTBXItem;
    mnFoldLevel3: TSpTBXItem;
    mnUnfoldLevel2: TSpTBXItem;
    mnUnfoldLevel3: TSpTBXItem;
    SpTBXSeparatorItem6: TSpTBXSeparatorItem;
    mnFoldFunctions: TSpTBXItem;
    mnFoldClasses: TSpTBXItem;
    SpTBXSeparatorItem7: TSpTBXSeparatorItem;
    mnUnfoldFunctions: TSpTBXItem;
    mnUnfoldClasses: TSpTBXItem;
    vilGutterGlyphs: TVirtualImageList;
    vilCodeImages: TVirtualImageList;
    SpTBXSubmenuItem1: TSpTBXSubmenuItem;
    SpTBXSeparatorItem8: TSpTBXSeparatorItem;
    mniAssistant: TSpTBXSubmenuItem;
    pmnuBreakpoint: TSpTBXPopupMenu;
    spiBreakpointEnabled: TSpTBXItem;
    spiBreakpointProperties: TSpTBXItem;
    spiSeparatorItem: TSpTBXSeparatorItem;
    spiBreakpointClear: TSpTBXItem;
    spiSeparator: TSpTBXSeparatorItem;
    spiEditorViews: TSpTBXSubmenuItem;
    vilEditorMarks: TVirtualImageList;
    pmnuDiagnostics: TSpTBXPopupMenu;
    mnIgnoreIssue: TSpTBXItem;
    mnFixIssue: TSpTBXItem;
    pmnuRefactor: TSpTBXPopupMenu;
    mnOrganizeImports: TSpTBXItem;
    SpTBXSeparatorItem9: TSpTBXSeparatorItem;
    mnRename: TSpTBXItem;
    procedure FormDestroy(Sender: TObject);
    procedure SynEditChange(Sender: TObject);
    procedure SynEditEnter(Sender: TObject);
    procedure SynEditExit(Sender: TObject);
    procedure SynEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure FormCreate(Sender: TObject);
    procedure SynEditSpecialLineColors(Sender: TObject; Line: Integer;
      var Special: Boolean; var FG, BG: TColor);
    procedure SynEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SynEditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FGPanelEnter(Sender: TObject);
    procedure FGPanelExit(Sender: TObject);
    procedure mnCloseTabClick(Sender: TObject);
    procedure SynEditMouseCursor(Sender: TObject;
      const ALineCharPos: TBufferCoord; var ACursor: TCursor);
    procedure SynEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SynCodeCompletionExecute(Kind: SynCompletionType;
      Sender: TObject; var CurrentInput: string; var X, Y: Integer;
      var CanExecute: Boolean);
    procedure SynCodeCompletionClose(Sender: TObject);
    procedure SynWebCompletionExecute(Kind: SynCompletionType; Sender: TObject;
      var CurrentInput: string; var X, Y: Integer; var CanExecute: Boolean);
    procedure SynWebCompletionAfterCodeCompletion(Sender: TObject;
      const Value: string; Shift: TShiftState; Index: Integer;
      EndToken: WideChar);
    procedure mnUpdateViewClick(Sender: TObject);
    procedure ViewsTabControlContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure ViewsTabControlActiveTabChange(Sender: TObject;
      TabIndex: Integer);
    procedure SynEditDblClick(Sender: TObject);
    procedure SynCodeCompletionAfterCodeCompletion(Sender: TObject;
      const Value: string; Shift: TShiftState; Index: Integer; EndToken: Char);
    procedure SynEditGutterGetText(Sender: TObject; ALine: Integer;
      var AText: string);
    procedure SynEditDebugInfoPaintLines(RT: ID2D1RenderTarget; ClipR:
        TRect; const FirstRow, LastRow: Integer; var DoDefaultPainting: Boolean);
    procedure SynEditGutterDebugInfoCLick(Sender: TObject; Button: TMouseButton;
        X, Y, Row, Line: Integer);
    procedure SynEditGutterDebugInfoMouseCursor(Sender: TObject; X, Y, Row, Line:
        Integer; var Cursor: TCursor);
    procedure EditorShowHint(var HintStr: string; var CanShow: Boolean; var
        HintInfo: Vcl.Controls.THintInfo);
    procedure BreakpointContextPopup(Sender: TObject; MousePos:
        TPoint; Row, Line: Integer; var Handled: Boolean);
    function FormHelp(Command: Word; Data: THelpEventData; var CallHelp: Boolean):
        Boolean;
    procedure mnFixIssueClick(Sender: TObject);
    procedure mnIgnoreIssueClick(Sender: TObject);
    procedure pmnuRefactorClosePopup(Sender: TObject);
    procedure pmnuRefactorPopup(Sender: TObject);
    procedure spiBreakpointClearClick(Sender: TObject);
    procedure spiBreakpointEnabledClick(Sender: TObject);
    procedure spiBreakpointPropertiesClick(Sender: TObject);
    procedure SynEditGutterMarksMouseCursor(Sender: TObject; X, Y, Row, Line:
        Integer; var Cursor: TCursor);
    procedure SynEditGutterMarksCLick(Sender: TObject; Button: TMouseButton;
        X, Y, Row, Line: Integer);
    procedure SynEditTSynGutterBands0ContextPopup(Sender: TObject; MousePos:
        TPoint; Row, Line: Integer; var Handled: Boolean);
    class procedure SynParamCompletionExecute(Kind: SynCompletionType;
      Sender: TObject; var CurrentInput: string; var X, Y: Integer;
      var CanExecute: Boolean);
  private
    const HotIdentIndicatorSpec: TGUID = '{8715589E-C990-4423-978F-F00F26041AEF}';
  private
    FEditor: TEditor;
    FActiveSynEdit: TSynEdit;
    FAutoCompleteActive: Boolean;
    FHotIdentInfo: THotIdentInfo;
    FNeedToSyncCodeExplorer: Boolean;
    FOldCaretY: Integer;
    // Hints
    FHintFuture: IFuture<string>;
    FHintCursorRect: TRect;
    function DoAskSaveChanges: Boolean;
    procedure DoAssignInterfacePointer(AActive: Boolean);
    function DoSave: Boolean;
    function DoSaveFile: Boolean;
    function DoSaveAs: Boolean;
    function DoSaveAsRemote: Boolean;
    procedure DoUpdateCaption;
    procedure DoUpdateHighlighter(HighlighterName: string = '');
    procedure AutoCompleteBeforeExecute(Sender: TObject);
    procedure AutoCompleteAfterExecute(Sender: TObject);
    procedure SynCodeCompletionCodeItemInfo(Sender: TObject;
      AIndex: Integer; var Info: string);
    procedure ApplyPyIDEOptions(const Sender: TObject; const Msg:
        System.Messaging.TMessage);
    procedure ScrollbarAnnotationGetInfo(Sender: TObject; AnnType:
      TSynScrollbarAnnType; var Rows: TArray<Integer>; var Colors: TArray<TColor>);
    procedure UpdateTabImage;
    class var FOldEditorForm: TEditorForm;
    class procedure DoCodeCompletion(Editor: TSynEdit; Caret: TBufferCoord);
    class procedure SymbolsChanged(Sender: TObject);
    class procedure CodeHintLinkHandler(Sender: TObject; LinkName: string);
  protected
    procedure Retranslate;
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  public
    BorderHighlight: TColor;
    BorderNormal: TColor;
    BreakPoints: TBreakpointList;
    HasFocus: Boolean;
    FileTime: TDateTime;
    DefaultExtension: string;
    ParentTabItem: TSpTBXTabItem;
    ParentTabControl: TSpTBXCustomTabControl;
    HasSearchHighlight: Boolean;
    procedure DoActivate;
    procedure DoActivateEditor(Primary: Boolean = True);
    function DoActivateView(ViewFactory: IEditorViewFactory): IEditorView;
    function GetEditor: IEditor;
    procedure EditorCommandHandler(Sender: TObject; AfterProcessing: Boolean;
      var Handled: Boolean; var Command: TSynEditorCommand;
      var AChar: WideChar; Data: Pointer; HandlerData: Pointer);
    procedure DoOnIdle;
    procedure SyncCodeExplorer;
    procedure AddWatchAtCursor;
  end;

  TEditor = class(TInterfacedObject, IUnknown, IEditor,  IFileCommands,
    ISearchCommands)
  private
    // IEditor implementation
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
    function HasSyntaxError: Boolean;
    function HasIssues: Boolean;
    function HasFixableIssues: Boolean;
    procedure GoToSyntaxError;
    procedure ExecuteSelection;
    procedure SplitEditorHorizontally;
    procedure SplitEditorVertrically;
    procedure SplitEditorHide;
    procedure Retranslate;
    procedure PullDiagnostics;
    // IFileCommands implementation
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
    // ISearchCommands implementation
    function CanFind: Boolean;
    function CanFindNext: Boolean;
    function ISearchCommands.CanFindPrev = CanFindNext;
    function ISearchCommands.GetSearchTarget = GetActiveSynEdit;
    function CanReplace: Boolean;
    procedure ExecFind;
    procedure ExecFindNext;
    procedure ExecFindPrev;
    procedure ExecReplace;
  private
    FFileName: string;
    FRemoteFileName: string;
    FSSHServer: string;
    Form: TEditorForm;
    FUntitledNumber: Integer;
    FSynLsp: TLspSynEditPlugin;
    function IsEmpty: Boolean;
    procedure DoSetFileName(AFileName: string);
    function GetEncodedTextEx(var EncodedText: AnsiString;
      InformationLossWarning: Boolean): Boolean;
    procedure FileChanged(Sender: TObject; const Path: string;
      ChangeType: TFileChangeType);
  public
    // Diagnostics
    procedure ClearDiagnostics;
    procedure NextDiagnostic;
    procedure PreviousDiagnostic;
    procedure OnDiagnosticsUpdate(UpdateTypes: TDiagnosticUpdateTypes);
  private
    class var UntitledNumbers: TBits;
    class var FChangedFiles: TArray<string>;
    class function GetUntitledNumber: Integer;
  public
    constructor Create(AForm: TEditorForm);
    destructor Destroy; override;
    class constructor Create;
    class destructor Destroy;
  end;

implementation

{$R *.DFM}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Math,
  System.IOUtils,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  System.Generics.Defaults,
  PythonEngine,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  JclFileUtils,
  SynEditHighlighter,
  SynEditKeyCmds,
  SynEditTextBuffer,
  SynHighlighterWebMisc,
  SynHighlighterWeb,
  SynHighlighterPython,
  SynEditMiscProcs,
  SynDWrite,
  JvGnugettext,
  StringResources,
  dmResources,
  dmCommands,
  uPythonItfs,
  uHighlighterProcs,
  dlgSynPrintPreview,
  dlgPickList,
  dlgRemoteFile,
  frmPyIDEMain,
  frmCodeExplorer,
  uCommonFunctions,
  uSearchHighlighter,
  cCodeCompletion,
  cCodeHint,
  cPyScripterSettings,
  cSSHSupport,
  cLspClients;

const
  WM_DELETETHIS = WM_USER + 42;

{$REGION 'TDebugSupportPlugin'}

type
  TDebugSupportPlugin = class(TSynEditPlugin)
  protected
    FForm: TEditorForm;
    procedure LinesInserted(FirstLine, Count: Integer); override;
    procedure LinesDeleted(FirstLine, Count: Integer); override;
  public
    constructor Create(AForm: TEditorForm);
  end;

constructor TDebugSupportPlugin.Create(AForm: TEditorForm);
begin
  inherited Create(AForm.SynEdit);
  FHandlers := [phLinesInserted, phLinesDeleted];
  FForm := AForm;
end;

procedure TDebugSupportPlugin.LinesInserted(FirstLine, Count: Integer);
begin
  with FForm do
  begin
    for var I := 0 to Breakpoints.Count - 1 do
      if TBreakpoint(Breakpoints[I]).LineNo >= FirstLine then
      begin
        TBreakpoint(Breakpoints[I]).LineNo := TBreakpoint(Breakpoints[I])
          .LineNo + Count;
        GI_BreakpointManager.BreakpointsChanged := True;
      end;
  end;
end;

procedure TDebugSupportPlugin.LinesDeleted(FirstLine, Count: Integer);
begin
  with FForm do
  begin
    for var I := Breakpoints.Count - 1 downto 0 do
      if TBreakpoint(Breakpoints[I]).LineNo >= FirstLine + Count then
      begin
        TBreakpoint(Breakpoints[I]).LineNo := TBreakpoint(Breakpoints[I])
          .LineNo - Count;
        GI_BreakpointManager.BreakpointsChanged := True;
      end
      else if TBreakpoint(Breakpoints[I]).LineNo >= FirstLine then
      begin
        Breakpoints.Delete(I);
        GI_BreakpointManager.BreakpointsChanged := True;
      end;
  end;
end;

{$ENDREGION 'TDebugSupportPlugin'}

{$REGION 'TEditor'}

constructor TEditor.Create(AForm: TEditorForm);
begin
  inherited Create;
  Form := AForm;
  FUntitledNumber := -1;
  SetFileEncoding(PyIDEOptions.NewFileEncoding);
  FSynLsp := TLspSynEditPlugin.Create(Form.SynEdit);
  FSynLsp.OnDiagnosticsUpdate := OnDiagnosticsUpdate;
  FSynLsp.DocSymbols.OnNotify := Form.SymbolsChanged;
end;

procedure TEditor.Activate(Primary: Boolean = True);
begin
  if Assigned(Form) then
    Form.DoActivateEditor(Primary);
end;

function TEditor.ActivateView(ViewFactory: IEditorViewFactory): IEditorView;
begin
  if Assigned(Form) then
    Result := Form.DoActivateView(ViewFactory);
end;

procedure TEditor.ApplyEditorOptions(EditorOptions: TSynEditorOptionsContainer);
begin
  var OldBookMarksFirst := Form.SynEdit.BookmarkOptions.DrawBookmarksFirst;
  Form.SynEdit.Assign(EditorOptions);
  Form.SynEdit2.Assign(EditorOptions);
  Form.SynEdit.BookmarkOptions.DrawBookmarksFirst := OldBookMarksFirst;

  Form.SynEdit.BookMarkOptions.BookmarkImages := Form.vilEditorMarks;
  Form.SynEdit2.BookMarkOptions.BookmarkImages := Form.vilEditorMarks;

  Form.SynEdit.BracketsHighlight.SetFontColorsAndStyle(
    ResourcesDataModule.SynPythonSyn.MatchingBraceAttri.Foreground,
    ResourcesDataModule.SynPythonSyn.UnbalancedBraceAttri.Foreground, [fsBold]);
  Form.SynEdit2.BracketsHighlight.SetFontColorsAndStyle(
    ResourcesDataModule.SynPythonSyn.MatchingBraceAttri.Foreground,
    ResourcesDataModule.SynPythonSyn.UnbalancedBraceAttri.Foreground, [fsBold]);
end;

function TEditor.AskSaveChanges: Boolean;
begin
  if Assigned(Form) then
    Result := Form.DoAskSaveChanges
  else
    Result := True;
end;

procedure TEditor.ClearDiagnostics;
begin
  FSynLsp.ClearDiagnostics;
  GI_MessagesService.ClearMessages;
end;

procedure TEditor.Close;
// Closes without asking
begin
  FSynLsp.FileClosed;
  if FUntitledNumber <> -1 then
    UntitledNumbers[FUntitledNumber] := False
  else
    GI_PyIDEServices.FilesMRUAdd(GetFileId);
  // Unregister existing File Notification
  if FFileName <> '' then
    GI_FileSystemMonitor.RemoveFile(FFileName, FileChanged);

  if Assigned(Form) then
  begin
    Form.DoAssignInterfacePointer(False);
    GI_EditorFactory.RemoveEditor(Self);

    var TabSheet := (Form.Parent as TSpTBXTabSheet);
    var TabControl := TabSheet.TabControl;
    TabControl.Toolbar.BeginUpdate;
    try
      (Form.ParentTabControl as TSpTBXTabControl).zOrder.Remove(TabSheet.Item);
      Form := nil;
      // The form is owned by the tabshhet and it is also destroyed
      // The SynEdit plugin FSynLsp will also be destroyed
      TabSheet.Free;
      if not GI_PyIDEServices.IsClosing then
        TabControl.Toolbar.MakeVisible(TabControl.ActiveTab);
    finally
      TabControl.Toolbar.EndUpdate;
    end;
  end;

  if GI_EditorFactory.Count = 0 then
    PyIDEMainForm.UpdateCaption;
end;

class constructor TEditor.Create;
begin
  UntitledNumbers := TBits.Create;
  UntitledNumbers[0] := True;  // do not use 0
end;

destructor TEditor.Destroy;
begin
  inherited;
end;

class destructor TEditor.Destroy;
begin
  UntitledNumbers.Free;
end;

procedure TEditor.DoSetFileName(AFileName: string);
begin
  if ((AFileName <> '') or (FRemoteFileName <> '')) and (FUntitledNumber <> -1) then
  begin
    UntitledNumbers[FUntitledNumber] := False;
    FUntitledNumber := -1;
  end;
  if AFileName <> FFileName then
  begin
    // Unregister existing File Notification
    if FFileName <> '' then
      GI_FileSystemMonitor.RemoveFile(FFileName, FileChanged);

    FFileName := AFileName;
    if AFileName <> '' then
    begin
      FRemoteFileName := '';
      FSSHServer := '';
    end;

    // Register File Notification
    if FFileName <> '' then
      GI_FileSystemMonitor.AddFile(FFileName, FileChanged);
  end;
end;

function TEditor.GetSynEdit: TSynEdit;
begin
  Result := Form.SynEdit;
end;

function TEditor.GetSynEdit2: TSynEdit;
begin
  Result := Form.SynEdit2;
end;

function TEditor.GetTabControlIndex: Integer;
begin
  Result := PyIDEMainForm.TabControlIndex(Form.ParentTabControl);
end;

class function TEditor.GetUntitledNumber: Integer;
begin
  Result := UntitledNumbers.OpenBit;
  UntitledNumbers[Result] := True;
end;

function TEditor.GetVersion: Integer;
begin
  Result := FSynLsp.Version;
end;

procedure TEditor.GoToSyntaxError;
begin
  if HasSyntaxError then
    Form.SynEdit.CaretXY :=
      BufferCoordFromLspPosition(FSynLsp.Diagnostics[0].range.start);
end;

function TEditor.GetActiveSynEdit: TSynEdit;
begin
  if Form.SynEdit2.Visible and (Form.FActiveSynEdit = Form.SynEdit2) then
    Result := Form.SynEdit2
  else
    Result := Form.SynEdit;
end;

function TEditor.GetBreakpoints: TObjectList;
begin
  Result := Form.Breakpoints;
end;

function TEditor.GetCaretPos: TPoint;
begin
  if Assigned(Form) then
  begin
    Result := TPoint(GetActiveSynEdit.CaretXY);
  end
  else
    Result := Point(-1, -1);
end;

function TEditor.GetDocSymbols: TObject;
begin
  Result := FSynLsp.DocSymbols;
end;

function TEditor.GetEditorState: string;
begin
  if Assigned(Form) then
  begin
    if Form.SynEdit.ReadOnly then
      Result := _(SReadOnly)
    else if Form.SynEdit.InsertMode then
      Result := _(SInsert)
    else
      Result := _(SOverwrite);
  end
  else
    Result := '';
end;

function TEditor.GetEncodedText: AnsiString;
begin
  GetEncodedTextEx(Result, False);
end;

function TEditor.GetEncodedTextEx(var EncodedText: AnsiString;
  InformationLossWarning: Boolean): Boolean;
begin
  Result := WideStringsToEncodedText(GetFileId, Form.SynEdit.Lines,
    EncodedText, InformationLossWarning, HasPythonFile);
end;

function TEditor.GetFileName: string;
begin
  Result := FFileName;
end;

function TEditor.GetFileTitle: string;
begin
  if FFileName <> '' then
  begin
    if PyIDEOptions.DisplayPackageNames and FileIsPythonPackage(FFileName) then
      Result := FileNameToModuleName(FFileName)
    else
      Result := TPath.GetFileName(FFileName);
  end
  else if FSSHServer <> '' then
    Result := TSSHFileName.Format(FSSHServer, FRemoteFileName)
  else
  begin
    if FUntitledNumber = -1 then
      FUntitledNumber := GetUntitledNumber;
    if Form.SynEdit.Highlighter = ResourcesDataModule.SynPythonSyn then
      Result := _(SNonamePythonFileTitle) + IntToStr(FUntitledNumber)
    else
      Result := _(SNonameFileTitle) + IntToStr(FUntitledNumber);
  end;
end;

function TEditor.GetFileId: string;
begin
  if FFileName <> '' then
    Result := FFileName
  else
    Result := GetFileTitle;
end;

function TEditor.GetModified: Boolean;
begin
  if Assigned(Form) then
    Result := Form.SynEdit.Modified
  else
    Result := False;
end;

function TEditor.GetReadOnly: Boolean;
begin
  Result := GetSynEdit.ReadOnly;
end;

function TEditor.GetRemoteFileName: string;
begin
  Result := FRemoteFileName;
end;

function TEditor.GetSSHServer: string;
begin
  Result := FSSHServer;
end;

function TEditor.GetFileEncoding: TFileSaveFormat;
begin
  with Form.SynEdit.Lines do
  begin
    if Encoding = nil then Exit(sf_Ansi);

    if Encoding = TEncoding.UTF8 then
    begin
      if WriteBOM then
        Result := sf_UTF8
      else
        Result := sf_UTF8_NoBOM;
    end else if Encoding = TEncoding.Unicode then
      Result := sf_UTF16LE
    else if Encoding = TEncoding.BigEndianUnicode then
      Result := sf_UTF16BE
    else
      Result := sf_Ansi;
  end;
end;

procedure TEditor.SetFileEncoding(FileEncoding: TFileSaveFormat);
begin
  with TSynEditStringList(Form.SynEdit.Lines) do
  begin
    case FileEncoding of
      sf_Ansi: SetEncoding(Encoding.ANSI);
      sf_UTF8,
      sf_UTF8_NoBOM: SetEncoding(TEncoding.UTF8);
      sf_UTF16LE: SetEncoding(TEncoding.Unicode);
      sf_UTF16BE: SetEncoding(TEncoding.BigEndianUnicode);
    end;
    if FileEncoding = sf_UTF8_NoBOM then
      WriteBOM := False
    else
      WriteBOM := True;
  end;
end;

procedure TEditor.SetHasSearchHighlight(Value: Boolean);
begin
  Form.HasSearchHighlight :=  Value;
end;

procedure TEditor.SetHighlighter(const HighlighterName: string);
begin
  Form.DoUpdateHighlighter(HighlighterName);
end;

procedure TEditor.SetReadOnly(Value: Boolean);
begin
  GetSynEdit.ReadOnly := Value;
  GetSynEdit2.ReadOnly := Value;
  Form.UpdateTabImage;
end;

procedure TEditor.ShowRefactoringMenu;
begin
  var Editor := GetActiveSynEdit;
  Editor.EnsureCursorPosVisibleEx(True);
  var P := Editor.BufferToPixels(Editor.CaretXY);
  Inc(P.Y, Editor.LineHeight);
  P := Editor.ClientToScreen(P);
  Form.pmnuRefactor.Popup(P.X, P.Y);
end;

procedure TEditor.SplitEditorHorizontally;
begin
  with Form do
  begin
    if not SynEdit2.IsChained then
      SynEdit2.SetLinesPointer(SynEdit);
    EditorSplitter.Visible := False;
    SynEdit2.Visible := False;
    SynEdit2.Align := alBottom;
    SynEdit2.Height := (ClientHeight - 5) div 2;
    EditorSplitter.Align := alBottom;
    SynEdit2.Visible := True;
    EditorSplitter.Visible := True;
  end;
end;

procedure TEditor.SplitEditorVertrically;
begin
  with Form do
  begin
    if not SynEdit2.IsChained then
      SynEdit2.SetLinesPointer(SynEdit);
    EditorSplitter.Visible := False;
    SynEdit2.Visible := False;
    SynEdit2.Align := alRight;
    SynEdit2.Width := (ClientWidth - 5) div 2;
    EditorSplitter.Align := alRight;
    SynEdit2.Visible := True;
    EditorSplitter.Visible := True;
  end;
end;

procedure TEditor.SplitEditorHide;
begin
  with Form do
  begin
    EditorSplitter.Visible:= False;
    SynEdit2.Visible := False;
    if SynEdit2.IsChained then
      SynEdit2.RemoveLinesPointer;
  end;
end;

procedure TEditor.OnDiagnosticsUpdate(UpdateTypes: TDiagnosticUpdateTypes);
begin
  Form.UpdateTabImage;
  Form.DoUpdateCaption;  // Update the hint
  Form.SynEdit.UpdateScrollBars;

  if datInvoked in UpdateTypes then
  begin
   GI_PyIDEServices.WriteStatusMsg(FSynLsp.Diagnostics.Summary);
   FSynLsp.Diagnostics.ShowInMessages(GetFileId);
   if HasSyntaxError then
     GoToSyntaxError;
  end;
end;

procedure TEditor.OpenLocalFile(const AFileName: string;
  HighlighterName: string = '');
// If AFilename is empty set up a new untitled file
begin
  if Form = nil then Abort;

  if (AFileName <> '') and FileExists(AFileName) then
  begin
    Form.SynEdit.LockUndo;
    try
      if LoadFileIntoWideStrings(AFileName, Form.SynEdit.Lines) then
      begin
        if not FileAge(AFileName, Form.FileTime) then
          Form.FileTime := 0;
      end
      else
        Abort;
    finally
      Form.SynEdit.UnlockUndo;
    end;
  end
  else
  begin
    Form.SynEdit.Lines.Clear;
    if AFileName = '' then
    begin
      // Default settings for new files
      if PyIDEOptions.NewFileLineBreaks <> sffUnicode then
        (Form.SynEdit.Lines as TSynEditStringList).FileFormat :=
          PyIDEOptions.NewFileLineBreaks;
    end;
  end;

  DoSetFileName(AFileName);
  Form.DoUpdateHighlighter(HighlighterName);
  Form.DoUpdateCaption;

  Form.SynEdit.Modified := False;
  Form.SynEdit.UseCodeFolding := PyIDEOptions.CodeFoldingEnabled;
  Form.SynEdit2.UseCodeFolding := Form.SynEdit.UseCodeFolding;

  if HasPythonFile then
    FSynLsp.FileOpened(GetFileId, lidPython)
  else
    FSynLsp.FileOpened(GetFileId, lidNone);
end;

procedure TEditor.OpenRemoteFile(const FileName, ServerName: string);
begin
  if (Form = nil) or (FileName = '') or (ServerName = '') then Abort;

  // CopyRemoteFileToTemp aborts on failure
  var TempFileName := CopyRemoteFileToTemp(FileName, ServerName);

  Form.SynEdit.LockUndo;
  try
    if not LoadFileIntoWideStrings(TempFileName, Form.SynEdit.Lines) then
      Abort;
  finally
    DeleteFile(TempFileName);
    Form.SynEdit.UnlockUndo;
  end;

  FRemoteFileName := FileName;
  FSSHServer := ServerName;
  DoSetFileName('');

  Form.SynEdit.Modified := False;
  Form.SynEdit.UseCodeFolding := PyIDEOptions.CodeFoldingEnabled;
  Form.SynEdit2.UseCodeFolding := Form.SynEdit.UseCodeFolding;

  Form.DoUpdateHighlighter('');
  Form.DoUpdateCaption;
  if HasPythonFile then
    FSynLsp.FileOpened(GetFileId, lidPython)
  else
    FSynLsp.FileOpened(GetFileId, lidNone);
end;

procedure TEditor.PreviousDiagnostic;
var
  Index: Integer;
begin
  if not (GetActiveSynEdit = Form.SynEdit) then Exit;

  var BCArray := FSynLsp.Diagnostics.ToBCArray;
  if Length(BCArray) = 0 then Exit;

  var Comparer := TComparer<TBufferCoord>.Construct(
    function(const Left, Right: TBufferCoord): Integer
    begin
      if Left > Right then
        Result := 1
      else if Left = Right then
        Result := 0
      else
        Result := -1;
    end);

  TArray.Sort<TBufferCoord>(BCArray, Comparer);
  TArray.BinarySearch<TBufferCoord>(BCArray, Form.SynEdit.CaretXY,
                                    Index, Comparer);
  Dec(Index);
  // Skip diagnostics with the same start
  while (Index >= 0) and (BCArray[Index] = BCArray[Index - 1]) do
    Dec(Index);
  if Index = 0 then
    Index := High(BCArray);
  Form.SynEdit.CaretXY := BCArray[Index];
end;

procedure TEditor.PullDiagnostics;
begin
  if HasPythonFile then
    FSynLsp.PullDiagnostics(True);
end;

function TEditor.SaveToRemoteFile(const FileName, ServerName: string): Boolean;
var
  TempFileName: string;
  ErrorMsg: string;
begin
  if (Form = nil)  or (FileName = '') or (ServerName = '') then  Abort;

  TempFileName := FileGetTempName('PyScripter');
  Result := SaveWideStringsToFile(TempFileName, Form.SynEdit.Lines, False);
  if Result then begin
    Result := GI_SSHServices.ScpUpload(ServerName, TempFileName, FileName, ErrorMsg);
    DeleteFile(TempFileName);
    if not Result then
      StyledMessageDlg(Format(_(SFileSaveError), [FileName, ErrorMsg]), mtError, [mbOK], 0);
  end;
end;

procedure TEditor.Retranslate;
begin
  Form.Retranslate;
end;

function TEditor.HasFixableIssues: Boolean;
begin
  // When we have fixable issues the quick fix marks are shown first
  Result := not Form.SynEdit.BookmarkOptions.DrawBookmarksFirst;
end;

function TEditor.HasIssues: Boolean;
begin
  Result := Length(FSynLsp.Diagnostics) > 0;
end;

function TEditor.HasPythonFile: Boolean;
begin
  Result := GetSynEdit.Highlighter is TSynPythonSyn;
end;

function TEditor.HasSyntaxError: Boolean;
begin
  Result := HasPythonFile and FSynLsp.Diagnostics.HasSyntaxError;
end;

function TEditor.GetForm: TForm;
begin
  Result := Form;
end;

function TEditor.GetHasSearchHighlight: Boolean;
begin
  Result := Form.HasSearchHighlight;
end;

// IEditCommands implementation

function TEditor.CanReload: Boolean;
begin
  Result := FFileName <> '';
end;

procedure TEditor.ExecReload(Quiet: Boolean = False);
var
  BC: TBufferCoord;
begin
  if Quiet or not GetModified or (StyledMessageDlg(_(SFileReloadingWarning),
      mtWarning, [mbYes, mbNo], 0) = mrYes) then
  begin
    BC := GetSynEdit.CaretXY;
    if FFileName <> '' then
      OpenLocalFile(FFileName)
    else if FRemoteFileName <> '' then
      OpenRemoteFile(FRemoteFileName, FSSHServer)
    else
      Exit;
    if (BC.Line <= GetSynEdit.Lines.Count) then
      GetSynEdit.CaretXY := BC;
  end;
end;

procedure TEditor.ExecuteSelection;
var
  ExecType: string;
  Source: string;
  Editor: TSynEdit;
begin
  if not HasPythonFile or not GI_PyControl.PythonLoaded or GI_PyControl.Running then
  begin
    // it is dangerous to execute code while running scripts
    // so just beep and do nothing
    MessageBeep(MB_ICONERROR);
    Exit;
  end;

  Editor := GetActiveSynEdit;

  ExecType := 'exec';

  // If nothing is selected then try to eval the word at cursor
  if Editor.SelAvail then
  begin
    Source := Editor.SelText;
    // if a single line or part of a line is selected then eval the selection
    if Editor.BlockBegin.Line = Editor.BlockEnd.Line then
      ExecType := 'single'
    else
      Source := Source + sLineBreak; // issue 291
  end
  else
  begin
    Source := Editor.WordAtCursor;
    if Source <> '' then
      ExecType := 'single'
    else
      Exit;
  end;

  // Dedent the selection
  Source := Dedent(Source);

  GI_PyInterpreter.ShowWindow;
  GI_PyInterpreter.AppendText(sLineBreak);
  Source := CleanEOLs(Source);

  ThreadPythonExec(procedure
  begin
    // RunSource
    case GI_PyControl.DebuggerState of
      dsInactive:
        GI_PyControl.ActiveInterpreter.RunSource(Source, '<editor selection>',
          ExecType);
      dsPaused, dsPostMortem:
        GI_PyControl.ActiveDebugger.RunSource(Source, '<editor selection>',
          ExecType);
    end;
  end,
  procedure
  begin
    GI_PyInterpreter.AppendPrompt;
    Activate(False);
  end);
end;

procedure TEditor.FileChanged(Sender: TObject; const Path: string;
  ChangeType: TFileChangeType);
var
  FileTime: TDateTime;
begin
  if FFileName = ''  then Exit;

  if (ChangeType in [fcRemoved, fcRenamedOld]) and
    not TFile.Exists(FFileName) and (Form.FileTime <> 0) then
  begin
    Form.SynEdit.Modified := True;
    // Set FileTime to zero to prevent further notifications
    Form.FileTime := 0;
    StyledMessageDlg(Format(_(SFileRenamedOrDeleted), [FFileName]),
      mtWarning, [mbOK], 0);
  end
  else if FileAge(FFileName, FileTime) and not SameDateTime(FileTime, Form.FileTime) then
  begin
    // Prevent further notifications on this file
    Form.FileTime := FileTime;
    if PyIDEOptions.AutoReloadChangedFiles and not Form.SynEdit.Modified then
      // Reload with a short delay
      TThread.ForceQueue(nil,
        procedure
        begin
          ExecReload(True);
          MessageBeep(MB_ICONASTERISK);
          GI_PyIDEServices.WriteStatusMsg(_(SChangedFilesReloaded));
        end, 500)
    else
    begin
      if TArray.IndexOf(FChangedFiles, FFileName) < 0 then
      begin
        FChangedFiles := FChangedFiles + [FFileName];
        if Length(FChangedFiles) = 1 then
          // Execute in the main thread with a delay in case more files
          // are changed in the meantime.
          TThread.ForceQueue(nil,
            procedure
            var
              Editor: IEditor;
            begin
              with TPickListDialog.Create(Application.MainForm) do
              begin
                Caption := _(SFileChangeNotification);
                lbMessage.Caption := _(SFileReloadWarning);
                CheckListBox.Items.AddStrings(FChangedFiles);
                SetScrollWidth;
                CheckListBox.CheckAll(cbChecked);
                if ShowModal = idOK then
                  for var I := CheckListBox.Count - 1 downto 0 do
                  begin
                    if CheckListBox.Checked[I] then
                    begin
                      Editor := GI_EditorFactory.GetEditorByName(CheckListBox.Items[I]);
                      if Assigned(Editor) then
                        (Editor as IFileCommands).ExecReload(True);
                    end;
                  end;
                Release;
              end;
              FChangedFiles := [];
            end, 1000);  // 1 second delay
      end;
    end;
    Form.SynEdit.Modified := True;  // so that we will be prompted to save changes
  end;
end;

// IFileCommands implementation

function TEditor.CanSave: Boolean;
begin
  Result := Assigned(Form) and (GetModified or ((FFileName = '') and (FRemoteFileName = '')));
end;

function TEditor.CanClose: Boolean;
begin
  Result := Assigned(Form);
end;

procedure TEditor.ExecClose;
// Close only after asking
begin
  if AskSaveChanges then
    Close;
end;

procedure TEditor.ExecPrint;
begin
  if Assigned(Form) then
    with ResourcesDataModule, CommandsDataModule do
    begin
      SynEditPrint.SynEdit := Form.SynEdit;
      PrintDialog.MinPage := 1;
      PrintDialog.MaxPage := SynEditPrint.PageCount;
      PrintDialog.FromPage := 1;
      PrintDialog.ToPage := PrintDialog.MaxPage;
      PrintDialog.PrintRange := prAllPages;

      if PrintDialog.Execute then
      begin
        SynEditPrint.Title := GetFileTitle;
        if PrintDialog.PrintRange = prAllPages then
          SynEditPrint.Print
        else
          SynEditPrint.PrintRange(PrintDialog.FromPage, PrintDialog.ToPage);
      end;
    end;
end;

procedure TEditor.ExecPrintPreview;
begin
  CommandsDataModule.SynEditPrint.SynEdit := Form.SynEdit;
  CommandsDataModule.SynEditPrint.Title := GetFileTitle;
  with TPrintPreviewDlg.Create(Application.MainForm) do
  begin
    SynEditPrintPreview.SynEditPrint := CommandsDataModule.SynEditPrint;
    ShowModal;
    Release;
  end;
end;

procedure TEditor.ExecSave;
begin
  if Assigned(Form) then
  begin
    if (FFileName <> '') or (FRemoteFileName <> '') then
      Form.DoSave
    else
      ExecSaveAs;
  end;
end;

procedure TEditor.ExecSaveAs;
begin
  if Assigned(Form) then
   Form.DoSaveAs;
end;

procedure TEditor.ExecSaveAsRemote;
begin
  if Assigned(Form) then
    Form.DoSaveAsRemote;
end;

// ISearchCommands implementation

function TEditor.CanFind: Boolean;
begin
  Result := Assigned(Form) and not IsEmpty;
end;

function TEditor.CanFindNext: Boolean;
begin
  Result := Assigned(Form) and not IsEmpty and
    (EditorSearchOptions.SearchText <> '');
end;

function TEditor.CanReplace: Boolean;
begin
  Result := Assigned(Form) and not GetReadOnly and not IsEmpty;
end;

procedure TEditor.ExecFind;
begin
  if Assigned(Form) then
    CommandsDataModule.ShowSearchReplaceDialog(GetActiveSynEdit, False);
end;

procedure TEditor.ExecFindNext;
begin
  if Assigned(Form) then
    CommandsDataModule.DoSearchReplaceText(GetActiveSynEdit, False, False);
end;

procedure TEditor.ExecFindPrev;
begin
  if Assigned(Form) then
    CommandsDataModule.DoSearchReplaceText(GetActiveSynEdit, False, True);
end;

procedure TEditor.ExecReplace;
begin
  if Assigned(Form) then
    CommandsDataModule.ShowSearchReplaceDialog(GetActiveSynEdit, True);
end;

function TEditor.IsEmpty: Boolean;
begin
  Result := (Form.SynEdit.Lines.Count = 0) or
    ((Form.SynEdit.Lines.Count = 1) and (Form.SynEdit.Lines[0] = ''));
end;

procedure TEditor.NextDiagnostic;
var
  Index: Integer;
begin
  if not (GetActiveSynEdit = Form.SynEdit) then Exit;

  var BCArray := FSynLsp.Diagnostics.ToBCArray;
  if Length(BCArray) = 0 then Exit;

  var Comparer := TComparer<TBufferCoord>.Construct(
    function(const Left, Right: TBufferCoord): Integer
    begin
      if Left > Right then
        Result := 1
      else if Left = Right then
        Result := 0
      else
        Result := -1;
    end);

  TArray.Sort<TBufferCoord>(BCArray, Comparer);
  var Found := TArray.BinarySearch<TBufferCoord>(BCArray, Form.SynEdit.CaretXY,
                                                 Index, Comparer);
  if Found then
  begin
    Inc(Index);
    // Skip diagnostics with the same start
    while (Index <= High(BCArray)) and (BCArray[Index] = BCArray[Index - 1]) do
      Inc(Index);
  end;
  if Index = Length(BCArray) then
    Index := 0;
  Form.SynEdit.CaretXY := BCArray[Index];
end;

{$ENDREGION 'TEditor'}

{$REGION 'TEditorFactory'}

type
  TEditorFactory = class(TInterfacedObject, IEditorFactory)
  private
    FEditors: TInterfaceList;
    FEditorViewFactories: TInterfaceList;
    // IEditorFactory implementation
    function CanCloseAll: Boolean;
    procedure CloseAll;
    function OpenFile(AFileName: string; HighlighterName: string = '';
       TabControlIndex: Integer = 0): IEditor;
    function GetEditorCount: Integer;
    function GetEditorByName(const Name: string): IEditor;
    function GetEditorByFileId(const FileId: string): IEditor;
    function GetEditor(Index: Integer): IEditor;
    function GetViewFactoryCount: Integer;
    function GetViewFactory(Index: Integer): IEditorViewFactory;
    function NewEditor(TabControlIndex:Integer = 1): IEditor;
    procedure InvalidatePos(const AFileId: string; ALine: Integer; AType:
        TInvalidationType);
    procedure RemoveEditor(AEditor: IEditor);
    function RegisterViewFactory(ViewFactory: IEditorViewFactory): Integer;
    procedure SetupEditorViewsMenu(ViewsMenu: TSpTBXItem; ImgList: TCustomImageList);
    procedure UpdateEditorViewsMenu(ViewsMenu: TSpTBXItem);
    procedure CreateRecoveryFiles;
    procedure RecoverFiles;
    procedure LockList;
    procedure UnlockList;
    procedure ApplyToEditors(const Proc: TProc<IEditor>);
    function FirstEditorCond(const Predicate: TPredicate<IEditor>): IEditor;
  public
    constructor Create;
    destructor Destroy; override;
    procedure OnEditorViewClick(Sender: TObject);
  end;

constructor TEditorFactory.Create;
begin
  inherited Create;
  FEditors := TInterfaceList.Create;
  FEditorViewFactories := TInterfaceList.Create;
end;

procedure TEditorFactory.CreateRecoveryFiles;
begin
  var RecoveryDir := TPyScripterSettings.RecoveryDir;
  if TDirectory.Exists(RecoveryDir) then
    TDirectory.Delete(RecoveryDir, True);
  TDirectory.CreateDirectory(RecoveryDir);

  ApplyToEditors(procedure(Ed: IEditor)
  begin
    if not Ed.Modified then Exit;

    if (Ed.FileName <> '') or (Ed.RemoteFileName <> '') then
      (Ed as IFileCommands).ExecSave
    else
    with TEditorForm(Ed.Form) do
    begin
      // save module1 as 1.py etc.
      var FName := (Ed as TEditor).FUntitledNumber.ToString;
      FName := TPath.Combine(RecoveryDir, ChangeFileExt(FName, DefaultExtension));
      SaveWideStringsToFile(FName, SynEdit.Lines, False);
    end;
  end);
end;

destructor TEditorFactory.Destroy;
begin
  FEditors.Free;
  FEditorViewFactories.Free;
  inherited Destroy;
end;

procedure TEditorFactory.ApplyToEditors(const Proc: TProc<IEditor>);
begin
  FEditors.Lock;
  try
    for var I := 0 to FEditors.Count - 1 do
      Proc(FEditors[I] as IEditor);
  finally
    FEditors.Unlock;
  end;
end;

function TEditorFactory.FirstEditorCond(const Predicate: TPredicate<IEditor>): IEditor;
begin
  FEditors.Lock;
  try
    Result := nil;
    for var I := 0 to FEditors.Count - 1 do
    begin
      var Editor := FEditors[I] as IEditor;
      if Predicate(Editor) then
        Exit(Editor);
    end;
  finally
    FEditors.Unlock;
  end;
end;

function TEditorFactory.CanCloseAll: Boolean;
begin
  Result := False;
  with TPickListDialog.Create(Application.MainForm) do
  begin
    Caption := _(SSaveModifiedFiles);
    lbMessage.Caption := _(SSelectModifiedFiles);
    ApplyToEditors(procedure(Editor: IEditor)
    begin
      if Editor.Modified then
        CheckListBox.Items.AddObject(Editor.FileId, Editor.Form);
    end);
    SetScrollWidth;
    mnSelectAllClick(nil);
    if CheckListBox.Items.Count = 0 then
      Result := True
    else if CheckListBox.Items.Count = 1 then
      Result := TEditorForm(CheckListBox.Items.Objects[0]).DoAskSaveChanges
    else if ShowModal = IDOK then
    begin
      Result := True;
      for var I := CheckListBox.Count - 1 downto 0 do
      begin
        if CheckListBox.Checked[I] then
        begin
          if not TEditorForm(CheckListBox.Items.Objects[I]).DoSave then
          begin
            Result := False;
            Break;
          end;
        end;
      end;
    end;
    Free;
  end;
end;

procedure TEditorFactory.CloseAll;
var
  I: Integer;
begin
  FEditors.Lock;
  try
    I := FEditors.Count - 1;
    while I >= 0 do
    begin
      (FEditors[I] as IEditor).Close;
      Dec(I);
    end;
  finally
    FEditors.Unlock;
  end;
end;

function TEditorFactory.NewEditor(TabControlIndex:Integer = 1): IEditor;
var
  Sheet: TSpTBXTabSheet;
  LForm: TEditorForm;
  TabItem: TSpTBXTabItem;
begin
  var TabControl := PyIDEMainForm.TabControl(TabControlIndex);

  TabItem := TabControl.Add('');
  TabItem.Images := PyIDEMainForm.vilTabDecorators;
  Sheet := TabControl.GetPage(TabItem);
  try
    LForm := TEditorForm.Create(Sheet);
    with LForm do
    begin
      Visible := False;
      FEditor := TEditor.Create(LForm);
      ParentTabItem := TabItem;
      ParentTabItem.OnTabClosing := PyIDEMainForm.TabControlTabClosing;
      ParentTabItem.OnDrawTabCloseButton := PyIDEMainForm.DrawCloseButton;
      ParentTabControl := TabControl;
      Result := FEditor;
      Result.ApplyEditorOptions(EditorOptions);
      BorderStyle := bsNone;
      Parent := Sheet;
      Align := alClient;
      Visible := True;
      ScaleForPPI(Sheet.CurrentPPI);
      ApplyPyIDEOptions(PyIDEOptions, nil);
    end;
    if Result <> nil then
    begin
      FEditors.Add(Result);
    end;
  except
    Sheet.Free;
  end;
end;

function TEditorFactory.GetEditorCount: Integer;
begin
  Result := FEditors.Count;
end;

function TEditorFactory.GetViewFactory(Index: Integer): IEditorViewFactory;
begin
  FEditorViewFactories.Lock;
  try
    Result := FEditorViewFactories[Index] as IEditorViewFactory;
  finally
    FEditorViewFactories.Unlock;
  end;
end;

function TEditorFactory.GetViewFactoryCount: Integer;
begin
  Result := FEditorViewFactories.Count;
end;

procedure TEditorFactory.InvalidatePos(const AFileId: string; ALine: Integer;
  AType: TInvalidationType);

  procedure ProcessEditor(SynEd: TSynEdit);
  begin
    if ALine > 0 then
      case AType of
        itLine: SynEd.InvalidateLine(ALine);
        itGutter: SynEd.InvalidateGutterLine(ALine);
        itBoth:
         begin
           SynEd.InvalidateLine(ALine);
           SynEd.InvalidateGutterLine(ALine);
         end;
      end
    else
      case AType of
        itLine: SynEd.InvalidateLines(-1, -1);
        itGutter: SynEd.InvalidateGutterLines(-1, -1);
        itBoth: SynEd.Invalidate;
      end
  end;

begin
  if AFileId = '' then Exit;

  var Editor := GetEditorByFileId(AFileId);
  if Assigned(Editor) then
  begin
    ProcessEditor(Editor.SynEdit);
    ProcessEditor(Editor.SynEdit2);
  end;
end;

procedure TEditorFactory.LockList;
begin
  FEditors.Lock;
end;

function TEditorFactory.GetEditorByName(const Name: string): IEditor;
var
  FullName: string;
begin
  // The Name may contain invalid characters and ExpandFileName will raise an
  // exception.  This is the case with exceptions raised by pywin32 COM objects
  try
    FullName := NormalizePath(Name);
  except
    Exit(nil);
  end;
  Result := FirstEditorCond(function(Editor: IEditor): Boolean
  begin
    Result := AnsiSameText(Editor.GetFileName, FullName);
  end);
end;

function TEditorFactory.GetEditorByFileId(const FileId: string): IEditor;
begin
  Result := GetEditorByName(FileId);
  if not Assigned(Result) then
    Result := FirstEditorCond(function(Editor: IEditor): Boolean
    begin
      Result := (Editor.FileName = '') and AnsiSameText(Editor.GetFileTitle, FileId);
    end);
end;

function TEditorFactory.GetEditor(Index: Integer): IEditor;
begin
  Result := FEditors[Index] as IEditor;
end;

procedure TEditorFactory.RemoveEditor(AEditor: IEditor);
begin
  var Index := FEditors.IndexOf(AEditor);
  if Index >= 0 then
    FEditors.Delete(Index);
end;

procedure TEditorFactory.RecoverFiles;
var
  UntitledNumber: Integer;
begin
  var RecoveryDir := TPyScripterSettings.RecoveryDir;
  if not TDirectory.Exists(RecoveryDir) then Exit;

  var RecoveredFiles := TDirectory.GetFiles(RecoveryDir);
  for var RecoveredFile in RecoveredFiles do
  begin
    var FName := TPath.GetFileNameWithoutExtension(RecoveredFile);
    if not TryStrToInt(FName, UntitledNumber) then Continue;

    var Ed := NewEditor;
    Ed.OpenLocalFile(RecoveredFile);

    var Editor := Ed as TEditor;
    Editor.FUntitledNumber := UntitledNumber;
    Editor.UntitledNumbers[UntitledNumber] := True;
    Editor.DoSetFileName('');
    Editor.Form.DoUpdateCaption;
    if Editor.HasPythonFile then
      Editor.FSynLsp.FileSavedAs(Editor.GetFileId, lidPython);
  end;
  TDirectory.Delete(RecoveryDir, True);
end;

function TEditorFactory.RegisterViewFactory(ViewFactory: IEditorViewFactory): Integer;
begin
  Result := FEditorViewFactories.Add(ViewFactory);
end;

procedure TEditorFactory.OnEditorViewClick(Sender: TObject);
var
  ViewFactory: IEditorViewFactory;
  EditorView: IEditorView;
  Editor: IEditor;
  Index: Integer;
begin
  Editor := GI_PyIDEServices.ActiveEditor;
  if not Assigned(Editor) then
    Exit;
  Index := (Sender as TSpTBXItem).Tag;
  if (Index >= 0) and (Index < FEditorViewFactories.Count) then
  begin
    ViewFactory := FEditorViewFactories[Index] as IEditorViewFactory;
    EditorView := Editor.ActivateView(ViewFactory);
    if Assigned(EditorView) then
      EditorView.UpdateView(Editor);
  end;
end;

function TEditorFactory.OpenFile(AFileName, HighlighterName: string;
  TabControlIndex: Integer): IEditor;
var
  IsRemote: Boolean;
  Server, FName: string;
  TabCtrl: TSpTBXCustomTabControl;
begin
  Result := nil;
  PyIDEMainForm.tbiRecentFileList.MRURemove(AFileName);
  IsRemote :=  TSSHFileName.Parse(AFileName, Server, FName);

  // activate the editor if already open
  if IsRemote then
  begin
    Result :=  GetEditorByFileId(AFileName);
    if Assigned(Result) then begin
      Result.Activate;
      Exit;
    end;
  end
  else if AFileName <> '' then
  begin
    AFileName := NormalizePath(AFileName);
    Result :=  GetEditorByName(AFileName);
    if Assigned(Result) then begin
      Result.Activate;
      Exit;
    end
    else if not FileExists(AFileName) then begin
      GI_PyIDEServices.WriteStatusMsg(_(Format('File %s does not exist', [AFileName])));
      Exit;
    end;
  end;
  // create a new editor, add it to the editor list, open the file
  TabCtrl := PyIDEMainForm.TabControl(TabControlIndex);
  TabCtrl.Toolbar.BeginUpdate;
  try
    Result := NewEditor(TabControlIndex);
    if Result <> nil then
    begin
      try
        if IsRemote then
          Result.OpenRemoteFile(FName, Server)
        else
          Result.OpenLocalFile(AFileName, HighlighterName);
        Result.Activate;
      except
        Result.Close;
        raise;
      end;
      if (AFileName <> '') and (GetEditorCount = 2) and
        (GetEditor(0).FileName = '') and
        (GetEditor(0).RemoteFileName = '') and
        not GetEditor(0).Modified
      then
        GetEditor(0).Close;
    end;
  finally
    TabCtrl.Toolbar.EndUpdate;
    if Assigned(TabCtrl.ActiveTab) then
      TabCtrl.MakeVisible(TabCtrl.ActiveTab);
    PyIDEMainForm.UpdateCaption;
  end;
end;

procedure TEditorFactory.SetupEditorViewsMenu(ViewsMenu: TSpTBXItem; ImgList: TCustomImageList);
var
  MenuItem: TSpTBXItem;
  ViewFactory: IEditorViewFactory;
begin
  ViewsMenu.Clear;
  FEditorViewFactories.Lock;
  try
    ViewsMenu.Enabled := FEditorViewFactories.Count > 0;
    for var I := 0 to FEditorViewFactories.Count - 1 do
    begin
      ViewFactory := FEditorViewFactories[I] as IEditorViewFactory;

      // Add MenuItem
      MenuItem := TSpTBXItem.Create(nil); // will be freed by the Parent Item
      MenuItem.Hint := ViewFactory.Hint;
      MenuItem.ImageIndex := ImgList.GetIndexByName(ViewFactory.ImageName);
      MenuItem.Caption := ViewFactory.MenuCaption;
      MenuItem.ShortCut := ViewFactory.ShortCut;
      MenuItem.OnClick := OnEditorViewClick;
      MenuItem.Tag := I;

      ViewsMenu.Add(MenuItem);
    end;
  finally
    FEditorViewFactories.Unlock;
  end;
end;

procedure TEditorFactory.UnlockList;
begin
  FEditors.Unlock;
end;

procedure TEditorFactory.UpdateEditorViewsMenu(ViewsMenu: TSpTBXItem);
var
  Editor: IEditor;
  ViewFactory: IEditorViewFactory;
  List: TList;
  Enabled: Boolean;
begin
  FEditorViewFactories.Lock;
  List := TList.Create;
  try
    for var I := 0 to FEditorViewFactories.Count - 1 do
    begin
      Editor := GI_PyIDEServices.ActiveEditor;
      Enabled := Assigned(Editor);
      if Enabled then
      begin
        ViewFactory := FEditorViewFactories[I] as IEditorViewFactory;
        ViewFactory.GetContextHighlighters(List);
        if List.Count > 0 then
        begin
          Enabled := False;
          for var Highlighter in List do
          begin
            if Highlighter = Editor.SynEdit.Highlighter then
            begin
              Enabled := True;
              Break;
            end;
          end;
        end;
        List.Clear;
      end;
      ViewsMenu[I].Enabled := Enabled;
    end;
  finally
    List.Free;
    FEditorViewFactories.Unlock;
  end;
end;

{$ENDREGION 'TEditorFactory'}

{$REGION 'TEditorForm'}

procedure TEditorForm.FormDestroy(Sender: TObject);
begin
  if SynEdit2.IsChained then
    SynEdit2.RemoveLinesPointer;

  if Breakpoints.Count > 0 then
    GI_BreakpointManager.BreakpointsChanged := True;
  Breakpoints.Free;

  // Remove notifications
  TMessageManager.DefaultManager.Unsubscribe(TIDEOptionsChangedMessage,
    ApplyPyIDEOptions);
  SkinManager.RemoveSkinNotification(Self);
end;

procedure TEditorForm.SynEditChange(Sender: TObject);
begin
  if GI_PyControl.ErrorPos.FileId = GetEditor.FileId then
    GI_PyControl.ErrorPos := TEditorPos.EmptyPos;

  ClearSearchHighlight(FEditor);
end;

procedure TEditorForm.SynEditDblClick(Sender: TObject);
var
  PtMouse: TPoint;
  ASynEdit: TSynEdit;
begin
  ASynEdit := Sender as TSynEdit;
  GetCursorPos(PtMouse);
  PtMouse := ASynEdit.ScreenToClient(PtMouse);
  if (PtMouse.X >= ASynEdit.GutterWidth + 2)
    and ASynEdit.SelAvail and PyIDEOptions.HighlightSelectedWord
  then
    CommandsDataModule.HighlightWordInActiveEditor(ASynEdit.SelText);
end;

procedure TEditorForm.SynEditEnter(Sender: TObject);
var
  ASynEdit: TSynEdit;
begin
  EditorSearchOptions.InitSearch;
  ASynEdit := Sender as TSynEdit;
  FActiveSynEdit := ASynEdit;
  FOldCaretY := ASynEdit.CaretY;
  PyIDEMainForm.ActiveTabControl := ParentTabControl;
  DoAssignInterfacePointer(True);
  with ResourcesDataModule.CodeTemplatesCompletion do
  begin
    Editor := ASynEdit;
    OnBeforeExecute := AutoCompleteBeforeExecute;
    OnAfterExecute := AutoCompleteAfterExecute;
  end;

  // Spell Checking
  CommandsDataModule.SynSpellCheck.Editor := ASynEdit;

  if ASynEdit.Highlighter is TSynWebBase then
  begin
    // SynCodeCompletion
    CommandsDataModule.SynCodeCompletion.Editor := nil;
    CommandsDataModule.SynCodeCompletion.OnExecute := nil;
    CommandsDataModule.SynCodeCompletion.OnAfterCodeCompletion := nil;
    CommandsDataModule.SynCodeCompletion.OnAfterCodeCompletion := nil;
    CommandsDataModule.SynCodeCompletion.OnCodeItemInfo := nil;
    CommandsDataModule.SynCodeCompletion.Images := vilCodeImages;
    // SynParamCompletion
    CommandsDataModule.SynParamCompletion.Editor := nil;
    CommandsDataModule.SynParamCompletion.OnExecute := nil;
    // SynWebCompletion
    CommandsDataModule.SynWebCompletion.Editor := ASynEdit;
    CommandsDataModule.SynWebCompletion.OnExecute := SynWebCompletionExecute;
    CommandsDataModule.SynWebCompletion.OnAfterCodeCompletion := SynWebCompletionAfterCodeCompletion;
  end
  else
  begin
    // SynCodeCompletion
    CommandsDataModule.SynCodeCompletion.Editor := ASynEdit;
    CommandsDataModule.SynCodeCompletion.OnExecute := SynCodeCompletionExecute;
    CommandsDataModule.SynCodeCompletion.OnAfterCodeCompletion := SynCodeCompletionAfterCodeCompletion;
    CommandsDataModule.SynCodeCompletion.OnClose := SynCodeCompletionClose;
    CommandsDataModule.SynCodeCompletion.OnCodeItemInfo := SynCodeCompletionCodeItemInfo;
    CommandsDataModule.SynCodeCompletion.Images := vilCodeImages;
    // SynParamCompletion
    CommandsDataModule.SynParamCompletion.Editor := ASynEdit;
    CommandsDataModule.SynParamCompletion.OnExecute := SynParamCompletionExecute;
    // SynWebCompletion
    CommandsDataModule.SynWebCompletion.Editor := nil;
    CommandsDataModule.SynWebCompletion.OnExecute := nil;
    CommandsDataModule.SynWebCompletion.OnAfterCodeCompletion := nil;
  end;

  if (FOldEditorForm <> Self) and not GI_PyIDEServices.IsClosing then
    CodeExplorerWindow.UpdateWindow(FEditor.FSynLsp.DocSymbols, ceuEditorEnter);
  FOldEditorForm := Self;

  // Search and Replace Target
  EditorSearchOptions.InterpreterIsSearchTarget := False;

  PyIDEMainForm.UpdateCaption;
end;

procedure TEditorForm.SynEditExit(Sender: TObject);
begin
  // To make sure Jedi has the latest version
  if not (csDestroying in ComponentState) then
    FEditor.FSynLsp.RefreshSymbols;

  DoAssignInterfacePointer(False);

  if FHotIdentInfo.HaveHotIdent then
  begin
    FHotIdentInfo.HaveHotIdent := False;
    (Sender as TCustomSynEdit).Indicators.Clear(HotIdentIndicatorSpec, True,
      FHotIdentInfo.StartCoord.Line);
    SetCursor(TCustomSynEdit(Sender).Cursor);
  end;
end;

procedure TEditorForm.SynEditStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
var
  ASynEdit: TSynEdit;
  NewCaretY: Integer;
begin
  ASynEdit := Sender as TSynEdit;
  Assert(FEditor <> nil, 'TEditorForm.SynEditStatusChange');
  if scModified in Changes then
  begin
    PyIDEMainForm.UpdateCaption;
    ParentTabItem.Invalidate;
  end;
  if scCaretY in Changes then
  begin
    FNeedToSyncCodeExplorer := True;
    // We refresh symbols and Pull Diagnostics only when the user finishes
    // editing a line and moves the cursor to another one.  This is so that
    // there is no slowdown while typing.
    // Symbols and Diagnostics are actually updated only if needed
    if FEditor.HasPythonFile then
    begin
      FEditor.FSynLsp.RefreshSymbols;
      if PyIDEOptions.CheckSyntaxAsYouType then
        FEditor.FSynLsp.PullDiagnostics;
    end;
  end;
  if (scCaretY in Changes) and ASynEdit.Gutter.Visible
    and ASynEdit.Gutter.ShowLineNumbers
    and PyIDEOptions.CompactLineNumbers then
  begin
    NewCaretY := ASynEdit.CaretY;
    ASynEdit.InvalidateGutterLine(FOldCaretY);
    ASynEdit.InvalidateGutterLine(NewCaretY);
    FOldCaretY := NewCaretY;
  end;
  if scTopLine in Changes then
    Application.CancelHint;
  if ASynEdit.Selection.IsEmpty then
   ClearSearchHighlight(FEditor);
end;

procedure TEditorForm.DoActivate;
begin
  ParentTabItem.Checked := True;
end;

procedure TEditorForm.DoActivateEditor(Primary: Boolean = True);
var
  ASynEdit: TSynEdit;
begin
  DoActivate;
  ViewsTabControl.ActiveTabIndex := 0;
  if Primary then
    ASynEdit := SynEdit
  else
    ASynEdit := FEditor.GetActiveSynEdit;
  if CanActuallyFocus(ASynEdit) then
    ASynEdit.SetFocus;
end;

function TEditorForm.DoActivateView(ViewFactory: IEditorViewFactory): IEditorView;
var
  Form: TCustomForm;
  Tab: TSpTBXTabItem;
  TabSheet: TSpTBXTabSheet;
begin
  Result := nil;
  DoActivate;
  // Does the EditorView tab exist?
  Result := nil;
  for var I := 0 to ViewsTabControl.PagesCount - 1 do
    if ViewsTabControl.Pages[I].Caption = ViewFactory.TabCaption then
    begin
      ViewsTabControl.ActiveTabIndex := I;
      Result := ViewsTabControl.Pages[I].Components[0] as IEditorView;
      Break;
    end;
  if not Assigned(Result) then
  begin
    // Editor View does not exist - Create
    Tab := ViewsTabControl.Add(ViewFactory.TabCaption);
    TabSheet := ViewsTabControl.GetPage(Tab);
    try
      Form := ViewFactory.CreateForm(FEditor, TabSheet);
      TranslateComponent(Form);
      with Form do
      begin
        BorderStyle := bsNone;
        Parent := TabSheet;
        Align := alClient;
        Visible := True;
        // Form.SetFocus;
        Result := Form as IEditorView;
      end;
      // fix for Delphi 4 (???)
      // Form.Realign;
      Tab.Checked := True;
    except
      Tab.Free;
      raise ;
    end;
  end;

  ViewsTabControl.TabVisible := True;
end;

function TEditorForm.DoAskSaveChanges: Boolean;
var
  Msg: string;
begin
  // this is necessary to prevent second confirmation when closing tabs
  if SynEdit.Modified then
  begin
    DoActivateEditor;
    MessageBeep(MB_ICONQUESTION);
    Assert(FEditor <> nil, 'TEditorForm.DoAskSaveChanges');
    Msg := Format(_(SAskSaveChanges), [TPath.GetFileName(FEditor.GetFileTitle)]);

    case StyledMessageDlg(Msg, mtConfirmation, [mbYes, mbNo, mbCancel], 0,
      mbYes) of
      mrYes:
        Result := DoSave;
      mrNo:
        Result := True;
    else
      Result := False;
    end;
  end
  else
    Result := True;
end;

procedure TEditorForm.DoAssignInterfacePointer(AActive: Boolean);
begin
  if AActive then
  begin
    GI_ActiveEditor := FEditor;
    GI_FileCmds := FEditor;
    GI_SearchCmds := FEditor;
  end
  else if GI_ActiveEditor = FEditor as IEditor then
  begin
    GI_ActiveEditor := nil;
    GI_FileCmds := nil;
    GI_SearchCmds := nil;
  end;
end;

function TEditorForm.DoSave: Boolean;
begin
  Assert(FEditor <> nil, 'TEditorForm.DoSave');
  if (FEditor.FFileName <> '') or (FEditor.FRemoteFileName <> '') then
  begin
    Result := DoSaveFile;
    if Result then
      FEditor.FSynLsp.FileSaved;
  end
  else
    Result := DoSaveAs;
end;

function TEditorForm.DoSaveFile: Boolean;
var
  Line, TrimmedLine: string;
begin
  // Trim all lines just in case (Issue 196)
  if (SynEdit.Lines.Count > 0) and ((eoTrimTrailingSpaces in SynEdit.Options) or
    PyIDEOptions.TrimTrailingSpacesOnSave)  then
  begin
    SynEdit.BeginUpdate;
    try
      for var I := 0 to SynEdit.Lines.Count - 1 do
      begin
        Line := SynEdit.Lines[I];
        TrimmedLine := TrimRight(Line);
        if Line <> TrimmedLine then
          SynEdit.Lines[I] := TrimmedLine;
      end;
    finally
      SynEdit.EndUpdate;
    end;
  end;
  Result := False;
  if FEditor.FFileName <> '' then begin
    Result := SaveWideStringsToFile(FEditor.FFileName, SynEdit.Lines,
      PyIDEOptions.CreateBackupFiles);
    if Result then
      if not FileAge(FEditor.FFileName, FileTime) then
        FileTime := 0;
  end else if FEditor.FRemoteFileName <> '' then
     Result := FEditor.SaveToRemoteFile(FEditor.FRemoteFileName, FEditor.FSSHServer);
  if Result then
  begin
    if not PyIDEOptions.UndoAfterSave then
      SynEdit.ClearUndo;
    SynEdit.MarkSaved;
    SynEdit.Modified := False;
  end;
end;

function TEditorForm.DoSaveAs: Boolean;
var
  NewName: string;
  Edit: IEditor;
begin
  Assert(FEditor <> nil, 'TEditorForm.DoSaveAs');
  NewName := FEditor.GetFileId;
  if (FEditor.GetFileName = '') and (DefaultExtension <> '') and
    (ExtractFileExt(NewName) = '') then
    NewName := NewName + '.' + DefaultExtension;
  if ResourcesDataModule.GetSaveFileName(NewName, SynEdit.Highlighter,
    DefaultExtension) then
  begin
    Edit := GI_EditorFactory.GetEditorByName(NewName);
    if Assigned(Edit) and (Edit <> Self.FEditor as IEditor) then
    begin
      StyledMessageDlg(_(SFileAlreadyOpen), mtError, [mbAbort], 0);
      Result := False;
      Exit;
    end;
    FEditor.DoSetFileName(NewName);
    DoUpdateHighlighter;
    Result := DoSaveFile;
    DoUpdateCaption;

    if FEditor.HasPythonFile then
      FEditor.FSynLsp.FileSavedAs(FEditor.GetFileId, lidPython)
    else
      FEditor.FSynLsp.FileSavedAs(FEditor.GetFileId, lidNone);
  end
  else
    Result := False;
end;

function TEditorForm.DoSaveAsRemote: Boolean;
var
  FileName, Server: string;
  Edit: IEditor;
begin
  Assert(FEditor <> nil, 'TEditorForm.DoSaveAsRemote');
  if FEditor.FFileName <> '' then
    FileName := TPath.GetFileName(FEditor.FFileName)
  else if FEditor.FRemoteFileName <> '' then
    FileName := FEditor.FRemoteFileName;
  if ExecuteRemoteFileDialog(FileName, Server, rfdSave) then
  begin
    Edit := GI_EditorFactory.GetEditorByName(TSSHFileName.Format(Server, FileName));
    if Assigned(Edit) and (Edit <> Self.FEditor as IEditor) then
    begin
      StyledMessageDlg(_(SFileAlreadyOpen), mtError, [mbAbort], 0);
      Result := False;
      Exit;
    end;
    FEditor.FRemoteFileName := FileName;
    FEditor.FSSHServer := Server;
    FEditor.DoSetFileName('');
    DoUpdateHighlighter;
    Result := DoSaveFile;
    DoUpdateCaption;

    if FEditor.HasPythonFile then
      FEditor.FSynLsp.FileSavedAs(FEditor.GetFileId, lidPython)
    else
      FEditor.FSynLsp.FileSavedAs(FEditor.GetFileId, lidNone);
  end
  else
    Result := False;
end;

procedure TEditorForm.DoUpdateCaption;
var
  TabCaption: string;
begin
  Assert(FEditor <> nil, 'TEditorForm.DoUpdateCaption');
  if FEditor.FRemoteFileName <> '' then
    TabCaption := TPath.GetFileName(FEditor.FRemoteFileName)
  else
    TabCaption := FEditor.GetFileTitle;

  SynEdit.AccessibleName := TabCaption;
  SynEdit2.AccessibleName := TabCaption;

  with ParentTabItem do
  begin
    Caption := StringReplace(TabCaption, '&', '&&', [rfReplaceAll]);
    Hint := FEditor.GetFileId;
    if Length(FEditor.FSynLsp.Diagnostics) > 0 then
      Hint := Hint + sLineBreak + FEditor.FSynLsp.Diagnostics.Summary;
  end;
  PyIDEMainForm.UpdateCaption;
end;

procedure TEditorForm.DoUpdateHighlighter(HighlighterName: string = '');
begin
  Assert(FEditor <> nil, 'TEditorForm.DoUpdateHighlighter');
  if HighlighterName <> '' then
    SynEdit.Highlighter :=
      ResourcesDataModule.Highlighters.HighlighterFromFriendlyName(HighlighterName)
  else if FEditor.FFileName <> '' then
    SynEdit.Highlighter :=
      ResourcesDataModule.Highlighters.HighlighterFromFileName(FEditor.FFileName)
  else if FEditor.FRemoteFileName <> '' then
    SynEdit.Highlighter :=
      ResourcesDataModule.Highlighters.HighlighterFromFileName(FEditor.FRemoteFileName)
  else // No highlighter otherwise
    SynEdit.Highlighter := nil;

  SynEdit2.Highlighter := SynEdit.Highlighter;

  // Set DefaultExtension
  if SynEdit.Highlighter <> nil then
    DefaultExtension := DefaultExtensionFromFilter(SynEdit.Highlighter.DefaultFilter)
  else
    DefaultExtension := '';
end;

procedure TEditorForm.EditorCommandHandler(Sender: TObject;
  AfterProcessing: Boolean; var Handled: Boolean;
  var Command: TSynEditorCommand; var AChar: WideChar;
  Data, HandlerData: Pointer);
var
  SynEd: TSynEdit;
  PrevLine: string;
  Caret: TBufferCoord;
begin
  if Handled then Exit;

  SynEd := Sender as TSynEdit;
  if (Command <> ecLostFocus) and (Command <> ecGotFocus) then
    EditorSearchOptions.InitSearch;

  if not AfterProcessing then
  begin
    if (Command <> ecCancelSelections) and (SynEd.Selections.Count > 1) then
      Exit;

    case Command of
      ecCodeCompletion:
        if SynEd.Highlighter is TSynPythonSyn then
        begin
          if CommandsDataModule.SynCodeCompletion.Form.Visible then
            CommandsDataModule.SynCodeCompletion.CancelCompletion;
          DoCodeCompletion(SynEd, SynEd.CaretXY);
          Handled := True;
        end else if SynEd.Highlighter is TSynWebBase then
          CommandsDataModule.SynWebCompletion.ActivateCompletion;
      ecParamCompletion:
        if SynEd.Highlighter is TSynPythonSyn then
        begin
          if CommandsDataModule.SynParamCompletion.Form.Visible then
            CommandsDataModule.SynParamCompletion.CancelCompletion;
          CommandsDataModule.SynParamCompletion.ActivateCompletion;
          Handled := True;
        end;
      ecCancelSelections:
        ClearSearchHighlight(FEditor);
    end;
  end
  else
  begin // AfterProcessing
    case Command of
      ecLineBreak: // Python Mode
        if SynEd.InsertMode and (eoAutoIndent in SynEd.Options)
          and (SynEd.Highlighter is TSynPythonSyn)
          and (SynEd.Selections.Count = 1)
          and not FAutoCompleteActive
        then
        begin
          { CaretY should never be less than 2 right after ecLineBreak, so there's
            no need for a check }
          PrevLine := TrimRight(SynEd.Lines[SynEd.CaretY - 2]);

          //BC := BufferCoord(Length(PrevLine), SynEd.CaretY - 1);
          // Indent on: if a: # success?
          //if SynEd.GetHighlighterAttriAtRowCol(BC, DummyToken, Attr) and not
          //  ( // (attr = SynEd.Highlighter.StringAttribute) or
          //  (Attr = SynEd.Highlighter.CommentAttribute) or
          //    (Attr = TSynPythonSyn(SynEd.Highlighter).CodeCommentAttri) { or
          //    (attr = ResourcesDataModule.SynPythonSyn.DocStringAttri) } ) then
          //begin
          if TPyRegExpr.IsBlockOpener(PrevLine) then
            SynEd.ExecuteCommand(ecTab, #0, nil)
          else if TPyRegExpr.IsBlockCloser(PrevLine) then
            SynEd.ExecuteCommand(ecShiftTab, #0, nil);
          //end;
        end;
      ecChar:
        // Trigger auto-complection on completion trigger chars
        begin
          if PyIDEOptions.EditorCodeCompletion and (SynEd.Selections.Count = 1) then
          begin
            if (TIDECompletion.CompletionInfo.Editor = nil)
              and (Pos(AChar, CommandsDataModule.SynCodeCompletion.TriggerChars) > 0)
              and not ResourcesDataModule.CodeTemplatesCompletion.Executing
            then
            begin
              Caret := SynEd.CaretXY;
              TThread.ForceQueue(nil, procedure
                begin
                  DoCodeCompletion(SynEd, Caret);
                end, IfThen(AChar = '.', 200,
                CommandsDataModule.SynCodeCompletion.TimerInterval));
            end;
          end;
        end;
      ecSelWord:
        if SynEd.SelAvail and PyIDEOptions.HighlightSelectedWord then
          CommandsDataModule.HighlightWordInActiveEditor(SynEd.SelText);
      ecLostFocus:
        if not (CommandsDataModule.SynCodeCompletion.Form.Visible or SynEdit.Focused or SynEdit2.Focused) then
          CommandsDataModule.SynParamCompletion.CancelCompletion;
    end;
  end;
end;

procedure TEditorForm.ScrollbarAnnotationGetInfo(Sender: TObject;
  AnnType: TSynScrollbarAnnType; var Rows: TArray<Integer>;
  var Colors: TArray<TColor>);
begin
  var Editor := Sender as TCustomSynEdit;

  Rows := [];
  Colors := [];
  if AnnType = sbaCustom1 then
  begin
    for var Diagnostic in FEditor.FSynLsp.Diagnostics do
      case Diagnostic.severity of
        0,1 :
          begin
            Rows := Rows + [Editor.LineToRow(Diagnostic.range.start.line + 1)];
            Colors := Colors + [TColors.Crimson];
          end;
        2:
          begin
            Rows := Rows + [Editor.LineToRow(Diagnostic.range.start.line + 1)];
            Colors := Colors + [TColors.Darkorange];
          end;
      end;
  end
  else if AnnType = sbaCustom2 then
  begin
    for var Pair in SynEdit.Indicators.GetById(SearchHighlightIndicatorId) do
      Rows := Rows + [Editor.LineToRow(Pair.Key)];
    Colors := [PyIDEOptions.HighlightSelectedWordColor];
  end;
end;

class procedure TEditorForm.SymbolsChanged(Sender: TObject);
begin
  CodeExplorerWindow.UpdateWindow(Sender as TDocSymbols, ceuSymbolsChanged);
end;

procedure TEditorForm.SyncCodeExplorer;
begin
  if FNeedToSyncCodeExplorer and GetEditor.HasPythonFile then
  begin
    CodeExplorerWindow.ShowEditorCodeElement;
    FNeedToSyncCodeExplorer := False;
  end;
end;

procedure TEditorForm.Retranslate;
begin
  RetranslateComponent(Self);
  for var Page := 1 to ViewsTabControl.PagesCount - 1 do
  begin
    RetranslateComponent(ViewsTabControl.Pages[Page].Controls[0]);
    ViewsTabControl.Pages[Page].Caption :=
      (ViewsTabControl.Pages[Page].Controls[0] as TCustomForm).Caption;
  end;
end;

procedure TEditorForm.FormCreate(Sender: TObject);
begin
  StyledBorderColors(BorderNormal, BorderHighlight);
  FGPanelExit(Self);

  SynEdit.OnReplaceText := CommandsDataModule.SynEditReplaceText;

  FHotIdentInfo.HaveHotIdent := False;

  ViewsTabControl.TabVisible := False;
  case PyIDEOptions.EditorsTabPosition of
    ttpTop:
      ViewsTabControl.TabPosition := ttpBottom;
    ttpBottom:
      ViewsTabControl.TabPosition := ttpTop;
  end;

  //  Custom command handling
  SynEdit.RegisterCommandHandler(EditorCommandHandler, nil);
  SynEdit2.RegisterCommandHandler(EditorCommandHandler, nil);

  Breakpoints := TBreakpointList.Create(True);
  TDebugSupportPlugin.Create(Self); // No need to free

  // Indicators
  var IndicatorSpec :=
    TSynIndicatorSpec.New(sisTextDecoration, clNoneF, clNoneF, [fsUnderline]);
  SynEdit.Indicators.RegisterSpec(HotIdentIndicatorSpec, IndicatorSpec);
  SynEdit2.Indicators.RegisterSpec(HotIdentIndicatorSpec, IndicatorSpec);

  PyIDEMainForm.ThemeEditorGutter(SynEdit.Gutter);

  // Setup notifications
  TMessageManager.DefaultManager.SubscribeToMessage(TIDEOptionsChangedMessage,
    ApplyPyIDEOptions);
  SkinManager.AddSkinNotification(Self);

  TranslateComponent(Self);
end;

procedure TEditorForm.SynEditGutterGetText(Sender: TObject; ALine: Integer;
  var AText: string);
begin
  if ALine = TSynEdit(Sender).CaretY then
    Exit;

  if ALine mod 10 <> 0 then
    if ALine mod 5 <> 0 then
      AText := '·'
    else
      AText := '-';
end;

procedure TEditorForm.SynEditSpecialLineColors(Sender: TObject; Line: Integer;
  var Special: Boolean; var FG, BG: TColor);
begin
  if GI_PyControl.PythonLoaded then
  begin
    if GI_PyControl.CurrentPos.PointsTo(FEditor.GetFileId, Line) then
    begin
      Special := True;     { TODO: Allow customization of these colors }
      FG := clWhite;
      BG := $FF901E; // Dodger Blue
    end
    else if GI_PyControl.ErrorPos.PointsTo(FEditor.GetFileId, Line) then
    begin
      Special := True;
      FG := clWhite;
      BG := $4763FF; // Tomato Red
    end;
  end;
end;

function TEditorForm.GetEditor: IEditor;
begin
  Result := FEditor;
end;

procedure TEditorForm.SynEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Cancel Code Hint when the Ctrl key is depressed
  Application.CancelHint;
end;

procedure TEditorForm.SynEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if FHotIdentInfo.HaveHotIdent then
  begin
    FHotIdentInfo.HaveHotIdent := False;
    (Sender as TCustomSynEdit).Indicators.Clear(HotIdentIndicatorSpec, True,
      FHotIdentInfo.StartCoord.Line);
    SetCursor(TCustomSynEdit(Sender).Cursor);
  end;
end;

procedure TEditorForm.SynEditMouseCursor(Sender: TObject;
  const ALineCharPos: TBufferCoord; var ACursor: TCursor);
var
  TokenType, Start: Integer;
  Token: string;
  Attri: TSynHighlighterAttributes;
  OldHotIdent: Boolean;
  OldStartCoord: TBufferCoord;
  ASynEdit: TSynEdit;
begin
  ASynEdit := Sender as TSynEdit;
  OldHotIdent := FHotIdentInfo.HaveHotIdent;
  OldStartCoord := FHotIdentInfo.StartCoord;

  FHotIdentInfo.HaveHotIdent := False;
  if ASynEdit.Focused and (HiWord(GetAsyncKeyState(VK_CONTROL)) > 0)
    and FEditor.HasPythonFile and
    not ASynEdit.IsPointInSelection(ALineCharPos)
  then
    with ASynEdit do
    begin
      GetHighlighterAttriAtRowColEx(ALineCharPos, Token, TokenType, Start,
        Attri);
      if (Attri = TSynPythonSyn(Highlighter).IdentifierAttri) or
        (Attri = TSynPythonSyn(Highlighter).NonKeyAttri) or
        (Attri = TSynPythonSyn(Highlighter).SystemAttri) then
      begin
        ACursor := crHandPoint;
        with FHotIdentInfo do
        begin
          HaveHotIdent := True;
          StartCoord := BufferCoord(Start, ALineCharPos.Line);
        end;
      end;
    end;
    if (OldHotIdent <> FHotIdentInfo.HaveHotIdent) or
      (OldStartCoord <> FHotIdentInfo.StartCoord) then
    begin
      if OldHotIdent then
        ASynEdit.Indicators.Clear(HotIdentIndicatorSpec, True,
          OldStartCoord.Line);
      if FHotIdentInfo.HaveHotIdent then
        ASynEdit.Indicators.Add(FHotIdentInfo.StartCoord.Line,
          TSynIndicator.New(HotIdentIndicatorSpec,
            FHotIdentInfo.StartCoord.Char,
            FHotIdentInfo.StartCoord.Char + Token.Length));
    end;
end;

procedure TEditorForm.SynEditMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  EditorSearchOptions.InitSearch;
  if GI_PyControl.ErrorPos.FileId = GetEditor.FileId then
    GI_PyControl.ErrorPos := TEditorPos.EmptyPos;

  if FHotIdentInfo.HaveHotIdent then
  begin
    // fHodIdentInfo is reset in the KeyUp handler
    PostMessage(Application.MainForm.Handle, WM_FINDDEFINITION,
      FHotIdentInfo.StartCoord.Char, FHotIdentInfo.StartCoord.Line);
  end;
  if CommandsDataModule.SynParamCompletion.Form.Visible then
    CommandsDataModule.SynParamCompletion.CancelCompletion;
end;

procedure TEditorForm.FGPanelEnter(Sender: TObject);
begin
  HasFocus := True;
  BGPanel.Color := BorderHighlight;
end;

procedure TEditorForm.FGPanelExit(Sender: TObject);
begin
  HasFocus := False;
  BGPanel.Color := BorderNormal;
end;

procedure TEditorForm.mnCloseTabClick(Sender: TObject);
begin
  if ViewsTabControl.ActivePage <> tbshSource then
  begin
    ViewsTabControl.ActivePage.Free;
    if ViewsTabControl.PagesCount = 1 then
    begin
      ViewsTabControl.TabVisible := False;
    end;
  end;
end;

procedure TEditorForm.AddWatchAtCursor;
var
  Token, LineTxt, DottedIdent: string;
  Attri: TSynHighlighterAttributes;
  BC: TBufferCoord;
begin
  BC := SynEdit.CaretXY;
  if SynEdit.Highlighter <> ResourcesDataModule.SynPythonSyn then
    Exit;

  with SynEdit do
  if SelAvail and (BlockBegin.Line = BlockEnd.Line) then
    GI_WatchManager.AddWatch(SelText)
  else
  begin
    GetHighlighterAttriAtRowCol(BC, Token, Attri);
    if (Attri = ResourcesDataModule.SynPythonSyn.IdentifierAttri) or
      (Attri = ResourcesDataModule.SynPythonSyn.NonKeyAttri) or
      (Attri = ResourcesDataModule.SynPythonSyn.SystemAttri) or
      ((Token = ')') or (Token = ']')) then
    begin
      LineTxt := Lines[BC.Line - 1];
      DottedIdent := GetWordAtPos(LineTxt, BC.Char,
        True, True, False, True);
      DottedIdent := DottedIdent + GetWordAtPos(LineTxt,
        BC.Char + 1, False, False, True);
      if (DottedIdent <> '') and Assigned(GI_WatchManager) then
        GI_WatchManager.AddWatch(DottedIdent);
    end;
  end;
end;

procedure TEditorForm.mnUpdateViewClick(Sender: TObject);
var
  TabCaption: string;
  ViewFactory: IEditorViewFactory;
  EditorView: IEditorView;
begin
  if ViewsTabControl.ActivePage <> tbshSource then
  begin
    TabCaption := ViewsTabControl.ActivePage.Caption;
    ViewFactory := nil;
    for var I := 0 to GI_EditorFactory.ViewFactoryCount - 1 do
    begin
      if GI_EditorFactory.ViewFactory[I].TabCaption = TabCaption then
      begin
        ViewFactory := GI_EditorFactory.ViewFactory[I];
        Break;
      end;
    end;
    if Assigned(ViewFactory) then
    begin
      EditorView := FEditor.ActivateView(ViewFactory);
      if Assigned(EditorView) then
        EditorView.UpdateView(FEditor);
    end;
  end;
end;

procedure TEditorForm.ApplyPyIDEOptions(const Sender: TObject;
  const Msg: System.Messaging.TMessage);

  procedure  ApplyOptionsToEditor(Editor: TCustomSynEdit);
  begin
    Editor.CodeFolding.Assign(PyIDEOptions.CodeFolding);
    Editor.SelectedColor.Assign(PyIDEOptions.SelectionColor);
    Editor.IndentGuides.Assign(PyIDEOptions.IndentGuides);
    Editor.DisplayFlowControl.Assign(PyIDEOptions.DisplayFlowControl);
    Editor.Gutter.TrackChanges.Assign(PyIDEOptions.TrackChanges);

    if PyIDEOptions.CompactLineNumbers then
      Editor.OnGutterGetText := SynEditGutterGetText
    else
      Editor.OnGutterGetText := nil;
    Editor.InvalidateGutter;

    if PyIDEOptions.ScrollbarAnnotation then
    begin
      Editor.ScrollbarAnnotations.SetDefaultAnnotations;
      // Diagnostics
      with Editor.ScrollbarAnnotations.Add as TSynScrollbarAnnItem do
      begin
        AnnType := sbaCustom1;
        AnnPos := sbpFullWidth;
        FullRow := False;
        OnGetInfo := ScrollbarAnnotationGetInfo;
      end;
      // Search highlight
      with Editor.ScrollbarAnnotations.Add as TSynScrollbarAnnItem do
      begin
        AnnType := sbaCustom2;
        AnnPos := sbpSecondLeft;
        FullRow := False;
        OnGetInfo := ScrollbarAnnotationGetInfo;
      end;
    end
    else
      Editor.ScrollbarAnnotations.Clear;

    if PyIDEOptions.AutoCompleteBrackets then
      Editor.Options := Editor.Options + [eoCompleteBrackets, eoCompleteQuotes]
    else
      Editor.Options := Editor.Options - [eoCompleteBrackets, eoCompleteQuotes];
    if PyIDEOptions.AccessibilitySupport then
      Editor.Options := Editor.Options + [eoAccessibility]
    else
      Editor.Options := Editor.Options - [eoAccessibility];
  end;

begin
  RegisterSearchHighlightIndicatorSpec(FEditor);

  // Tab position
  if PyIDEOptions.EditorsTabPosition = ttpTop then
    ViewsTabControl.TabPosition := ttpBottom
  else  //ttpBottom:
    ViewsTabControl.TabPosition := ttpTop;

  ApplyOptionsToEditor(SynEdit);
  ApplyOptionsToEditor(SynEdit2);
end;

procedure TEditorForm.AutoCompleteAfterExecute(Sender: TObject);
begin
  FAutoCompleteActive := False;
  CommandsDataModule.actReplaceParametersExecute(nil);
end;

procedure TEditorForm.AutoCompleteBeforeExecute(Sender: TObject);
begin
  FAutoCompleteActive := True;
end;

procedure TEditorForm.WMSpSkinChange(var Message: TMessage);
begin
  StyledBorderColors(BorderNormal, BorderHighlight);
  if HasFocus then
    BGPanel.Color := BorderHighlight
  else
    BGPanel.Color := BorderNormal;

  PyIDEMainForm.ThemeEditorGutter(SynEdit.Gutter);
  SynEdit.CodeFolding.FolderBarLinesColor := SynEdit.Gutter.Font.Color;
  SynEdit.InvalidateGutter;

  PyIDEMainForm.ThemeEditorGutter(SynEdit2.Gutter);
  SynEdit2.CodeFolding.FolderBarLinesColor := SynEdit2.Gutter.Font.Color;
  SynEdit2.InvalidateGutter;
  Invalidate;
end;

procedure TEditorForm.SynCodeCompletionAfterCodeCompletion(Sender: TObject;
  const Value: string; Shift: TShiftState; Index: Integer; EndToken: Char);
var
  Editor: TSynEdit;
begin
  Editor := FActiveSynEdit;
  if Value.EndsWith('()') then
  begin
    // if the next char is an opening bracket remove the added brackets
    if (EndToken = '(') or ((Editor.CaretX <= Editor.LineText.Length) and
      IsOpeningBracket(Editor.LineText[Editor.CaretX], Editor.Brackets)) then
    begin
      Editor.BeginUpdate;
      try
        Editor.ExecuteCommand(ecDeleteLastChar, #0, nil);
        Editor.ExecuteCommand(ecDeleteLastChar, #0, nil);
      finally
        Editor.EndUpdate;
      end;
    end
    else
    begin
      Editor.CaretX:= Editor.CaretX - 1;
      EndToken := '(';
    end;
  end;
  if EndToken = '(' then
    TThread.ForceQueue(nil, procedure
    begin
      if Assigned(GI_ActiveEditor) and (GI_ActiveEditor.ActiveSynEdit = Editor) then
        CommandsDataModule.SynParamCompletion.ActivateCompletion;
    end);
end;

procedure TEditorForm.SynCodeCompletionClose(Sender: TObject);
begin
  PyIDEOptions.CodeCompletionListSize :=
    CommandsDataModule.SynCodeCompletion.NbLinesInWindow;
  TIDECompletion.CompletionInfo.CleanUp;
end;

procedure TEditorForm.SynCodeCompletionCodeItemInfo(Sender: TObject;
  AIndex: Integer; var Info: string);
begin
  if not TIDECompletion.CompletionLock.TryEnter then Exit;
  try
    if Assigned(TIDECompletion.CompletionInfo.CompletionHandler) then
    begin
      var CCItem := (Sender as TSynCompletionProposal).InsertList[AIndex];
      Info := TIDECompletion.CompletionInfo.CompletionHandler.GetInfo(CCItem);
    end;
  finally
    TIDECompletion.CompletionLock.Leave;
  end;
end;

class procedure TEditorForm.DoCodeCompletion(Editor: TSynEdit; Caret: TBufferCoord);
var
  Locline: string;
  Attr: TSynHighlighterAttributes;
  Highlighter: TSynCustomHighlighter;
  FileName, DummyToken: string;
  DisplayText, InsertText: string;
begin
  //Exit if cursor has moved
  if not Assigned(GI_ActiveEditor) or (GI_ActiveEditor.ActiveSynEdit <> Editor)
    or Editor.ReadOnly or (Caret <> Editor.CaretXY)
  then
    Exit;

  if not (GI_ActiveEditor.HasPythonFile and
    GI_PyControl.PythonLoaded and not GI_PyControl.Running and
    PyIDEOptions.EditorCodeCompletion)
  then
    Exit;

  Highlighter := Editor.Highlighter;
  FileName := GI_ActiveEditor.FileId;

  Dec(Caret.Char);
  Editor.GetHighlighterAttriAtRowCol(Caret, DummyToken, Attr);
  // to deal with trim trailing spaces
  Locline := Editor.LineText.PadRight(Caret.Char);
  Inc(Caret.Char);

  var CC := TIDECompletion.EditorCodeCompletion;
  if not TIDECompletion.CompletionLock.TryEnter then Exit;
  try
    TIDECompletion.CompletionInfo.CleanUp;

    var Skipped := False;
    for var I := 0 to CC.SkipHandlers.Count -1 do
    begin
      var SkipHandler := CC.SkipHandlers[I] as TBaseCodeCompletionSkipHandler;
      Skipped := SkipHandler.SkipCodeCompletion(Locline, FileName, Caret, Highlighter, Attr);
      if Skipped then Break;
    end;

    if not Skipped then
      // There is one CompletionHandler (LSP)!
      // Completion will be activated by the LSP completion handler later
      for var I := 0 to CC.CompletionHandlers.Count - 1 do
      begin
        var CompletionHandler := CC.CompletionHandlers[I] as TBaseCodeCompletionHandler;
        CompletionHandler.Initialize;
        // We ignore the result of HandleCodeCompletion
        CompletionHandler.HandleCodeCompletion(Locline, FileName,
          Caret, Highlighter, Attr, InsertText, DisplayText);
        TIDECompletion.CompletionInfo.CompletionHandler := CompletionHandler;
        TIDECompletion.CompletionInfo.Editor := Editor;
        TIDECompletion.CompletionInfo.CaretXY := Caret;
      end;
  finally
    TIDECompletion.CompletionLock.Leave;
  end;
end;

procedure TEditorForm.SynCodeCompletionExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: string; var X, Y: Integer;
  var CanExecute: Boolean);
begin
  var CI := TIDECompletion.CompletionInfo;
  var CP := TSynCompletionProposal(Sender);

  CanExecute := False;
  if TIDECompletion.CompletionLock.TryEnter then
  try
    CanExecute := Assigned(GI_ActiveEditor) and
      (GI_ActiveEditor.ActiveSynEdit = CI.Editor) and
      Application.Active and
      (GetParentForm(CI.Editor).ActiveControl = CI.Editor) and
      (TIDECompletion.CompletionInfo.CaretXY = TIDECompletion.CompletionInfo.Editor.CaretXY);

    if CanExecute then
    begin
      CP.Font := PyIDEOptions.AutoCompletionFont;
      CP.ItemList.Text := CI.DisplayText;
      CP.InsertList.Text := CI.InsertText;
      CP.NbLinesInWindow := PyIDEOptions.CodeCompletionListSize;
      CP.CurrentString := CurrentInput;

      if CP.Form.AssignedList.Count = 0 then
      begin
        CanExecute := False;
        TIDECompletion.CompletionInfo.CleanUp;
      end
      else
      if PyIDEOptions.CompleteWithOneEntry and (CP.Form.AssignedList.Count = 1) then
      begin
        // Auto-complete with one entry without showing the form
        CanExecute := False;
        CP.OnValidate(CP.Form, [], #0);
        TIDECompletion.CompletionInfo.CleanUp;
      end;
    end
    else
    begin
      CP.ItemList.Clear;
      CP.InsertList.Clear;
      TIDECompletion.CompletionInfo.CleanUp;
    end;
  finally
    TIDECompletion.CompletionLock.Leave;
  end;
end;

class procedure TEditorForm.SynParamCompletionExecute(Kind: SynCompletionType;
    Sender: TObject; var CurrentInput: string; var X, Y: Integer; var
    CanExecute: Boolean);
var
  P: TPoint;
  CP: TSynCompletionProposal;
  Editor: IEditor;
begin
   Editor := GI_ActiveEditor;
   CP := Sender as TSynCompletionProposal;

  TIDECompletion.SignatureHelpInfo.Lock;
  try
    CanExecute := Assigned(Editor) and Editor.HasPythonFile and TPyLspClient.MainLspClient.Ready
      and (Editor.ActiveSynEdit = CP.Editor) and PyIDEOptions.EditorCodeCompletion;

    // This function is called
    // a) from the editor using trigger char or editor command
    // b) From TSynCompletionProposal.HookEditorCommand
    // c) from TJedi ParamCompletionHandler only then RequestId <> 0

    if not TIDECompletion.SignatureHelpInfo.Handled then
    begin
      TPyLspClient.MainLspClient.SignatureHelp(Editor.FileId, CP.Editor);

      if CanExecute and CP.Form.Visible then
      begin
        // Keep showing the form at the same position
        X := CP.Form.Left;
        Y := CP.Form.Top - MulDiv(2, Editor.Form.CurrentPPI, Screen.DefaultPixelsPerInch);
      end
      else
        CanExecute := False;

      Exit;
    end;

    // ParamCompletionInfo  request was handled.  Make sure is still valid
    CanExecute := CanExecute and TIDECompletion.SignatureHelpInfo.Succeeded and
      (TIDECompletion.SignatureHelpInfo.FileId = Editor.FileId) and
      (TIDECompletion.SignatureHelpInfo.CurrentLine = CP.Editor.LineText) and
      (TIDECompletion.SignatureHelpInfo.Caret = CP.Editor.CaretXY);

    if CanExecute then
    begin
      var DisplayString := TIDECompletion.SignatureHelpInfo.DisplayString;
      CP.FormatParams := not (DisplayString = '');
      if not CP.FormatParams then
        DisplayString :=  '\style{~B}' + _(SNoParameters) + '\style{~B}';

      var DocString := TIDECompletion.SignatureHelpInfo.DocString;
      if (DocString <> '') then
      begin
        DisplayString := DisplayString + sLineBreak;
        DocString := GetLineRange(DocString, 1, 20); // 20 lines max
      end;

      CP.Font := PyIDEOptions.AutoCompletionFont;
      CP.Form.CurrentIndex := TIDECompletion.SignatureHelpInfo.ActiveParameter;
      CP.ItemList.Text := DisplayString + DocString;

      // position the hint window at and just below the opening bracket
      P := CP.Editor.ClientToScreen(CP.Editor.RowColumnToPixels(
          CP.Editor.BufferToDisplayPos(
          BufferCoord(TIDECompletion.SignatureHelpInfo.StartX, CP.Editor.CaretY))));
      Inc(P.Y, CP.Editor.LineHeight);
      X := P.X;
      Y := P.Y;
    end
    else
      CP.ItemList.Clear;

    // Mark request as not handled even if you cannot execute
    // It will be marked again as handled by the asynchronous request handler
    TIDECompletion.SignatureHelpInfo.Handled := False;

  finally
    TIDECompletion.SignatureHelpInfo.UnLock;
  end;
end;

procedure TEditorForm.SynWebCompletionAfterCodeCompletion(Sender: TObject;
  const Value: string; Shift: TShiftState; Index: Integer;
  EndToken: WideChar);
var
    SynEdit: TCustomSynEdit;

  function CaretBetween(AStr: string): Boolean;
  var
    I: Integer;
  begin
    I := Pos(AStr, Value);
    Result := I > 0;
    if Result then
      SynEdit.CaretX := SynEdit.CaretX - (Length(Value) - I);
  end;

begin
  SynEdit := TSynCompletionProposal(Sender).Editor;
  if Pos('>>', SynEdit.Lines[SynEdit.CaretY-1]) >= 1 then
    SynEdit.ExecuteCommand(ecDeleteChar, ' ', nil);

  if not CaretBetween('()') then
    if not CaretBetween('><') then
      if not CaretBetween('""') then
        CaretBetween(' ;');
end;

procedure TEditorForm.SynWebCompletionExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: string; var X, Y: Integer;
  var CanExecute: Boolean);
var
  SynEdit: TCustomSynEdit;
begin
  SynEdit := TSynCompletionProposal(Sender).Editor;
  SynWebFillCompletionProposal(SynEdit, ResourcesDataModule.SynWebHtmlSyn,
    CommandsDataModule.SynWebCompletion, CurrentInput);
  TSynCompletionProposal(Sender).Font := PyIDEOptions.AutoCompletionFont;
end;

procedure TEditorForm.ViewsTabControlActiveTabChange(Sender: TObject;
  TabIndex: Integer);
begin
  tbiUpdateView.Enabled := ViewsTabControl.ActivePage <> tbshSource;
  tbiCloseTab.Enabled := ViewsTabControl.ActivePage <> tbshSource;
end;

procedure TEditorForm.ViewsTabControlContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
  IV: TTBItemViewer;
  P: TPoint;
begin
  IV := ViewsTabControl.View.ViewerFromPoint(MousePos);
  if Assigned(IV) and (IV.Item is TSpTBXTabItem) then
  begin
    IV.Item.Checked := True;
    P := ClientToScreen(MousePos);
    mnCloseTab.Enabled := not(ViewsTabControl.GetPage(TSpTBXTabItem(IV.Item))
        = tbshSource);
    pmnuViewsTab.Popup(P.X, P.Y);
  end;
  Handled := True;
end;

class procedure TEditorForm.CodeHintLinkHandler(Sender: TObject; LinkName: string);
begin
  var Editor := GI_PyIDEServices.ActiveEditor;
  if not Assigned(Editor) then Exit;

  if LinkName.StartsWith('http') then
    Exit //already handled
  else if LinkName = 'QuickFix' then
    (Editor as TEditor).FSynLsp.PerformQuickFix
  else if LinkName = 'Ignore' then
    (Editor as TEditor).FSynLsp.PerformNoqaEdit
  else
  begin
    var SynEd := Editor.ActiveSynEdit;
    var P := TEditorForm(Editor.Form).FHintCursorRect.TopLeft;
    var DC := SynEd.PixelsToNearestRowColumn(P.X, P.Y);
    var BC := SynEd.DisplayToBufferPos(DC);
    PyIDEMainForm.JumpToFilePosInfo(LinkName);
    PyIDEMainForm.AdjustBrowserLists(Editor.GetFileId, BC.Line, BC.Char, LinkName);
  end;
end;

procedure TEditorForm.DoOnIdle;
begin
  SyncCodeExplorer;
end;

procedure TEditorForm.SynEditDebugInfoPaintLines(RT: ID2D1RenderTarget; ClipR:
    TRect; const FirstRow, LastRow: Integer; var DoDefaultPainting: Boolean);
var
  LH, Y: Integer;
  ImgIndex: Integer;
  Row, Line: Integer;
  Breakpoint: TBreakpoint;
  HasBP, HasDisabledBP: Boolean;
begin
  DoDefaultPainting := False;
  if not (SynEdit.Highlighter = ResourcesDataModule.SynPythonSyn) then Exit;

  if (GI_PyControl.ActiveDebugger <> nil) and SynEdit.Gutter.Visible then
  begin
    LH := SynEdit.LineHeight;

    for Row := FirstRow to LastRow do
    begin
      Line := SynEdit.RowToLine(Row);
      if Row <> SynEdit.LineToRow(Line) then Continue;  //Wrapped line

      Y := (LH - vilGutterGlyphs.Height) div 2 + LH *
        (Row - SynEdit.TopLine);

      HasBP := False;
      HasDisabledBP := False;
      if Breakpoints.FindBreakpoint(Line, Breakpoint) then
      begin
        if Breakpoint.Disabled then
          HasDisabledBP := True
        else
          HasBP := True;
      end;

      if GI_PyControl.CurrentPos.PointsTo(FEditor.GetFileId, Line) then
      begin
        if HasBP then
          ImgIndex := 2
        else
          ImgIndex := 1;
      end
      else if TPyRegExpr.IsExecutableLine(SynEdit.Lines[Line - 1]) then
      begin
        if HasBP then
          ImgIndex := 3
        else if HasDisabledBP then
          ImgIndex := 5
        else if PyIDEOptions.MarkExecutableLines then
          ImgIndex := 0
        else
          ImgIndex := -1;
      end
      else
      begin
        if HasBP then
          ImgIndex := 4
        else if HasDisabledBP then
          ImgIndex := 5
        else
          ImgIndex := -1;
      end;
      if ImgIndex >= 0 then
        ImageListDraw(RT, vilGutterGlyphs, ClipR.Left +
          MulDiv(TSynGutterBand.MarginX, FCurrentPPI, 96), Y, ImgIndex);
    end;
  end;
end;

procedure TEditorForm.SynEditGutterDebugInfoCLick(Sender: TObject; Button:
    TMouseButton; X, Y, Row, Line: Integer);
var
  ASynEdit: TSynEdit;
begin
  ASynEdit := Sender as TSynEdit;
  if (ASynEdit.Highlighter = ResourcesDataModule.SynPythonSyn) and
    (GI_PyControl.ActiveDebugger <> nil)
  then
    GI_BreakpointManager.ToggleBreakpoint(FEditor.GetFileId, Line,
      GetKeyState(VK_CONTROL) < 0);
end;

procedure TEditorForm.SynEditGutterDebugInfoMouseCursor(Sender: TObject; X, Y,
    Row, Line: Integer; var Cursor: TCursor);
begin
  Cursor := crHandPoint;
end;

procedure TEditorForm.EditorShowHint(var HintStr: string; var CanShow:
    Boolean; var HintInfo: Vcl.Controls.THintInfo);

  function CursorRect(SynEd: TCustomSynEdit; const BC1, BC2: TBufferCoord;
    out HintPos: TPoint): TRect;
  begin
    var P1 := SynEd.RowColumnToPixels(SynEd.BufferToDisplayPos(BC1));
    var P2 := SynEd.RowColumnToPixels(SynEd.BufferToDisplayPos(BC2));
    Inc(P2.Y, SynEd.LineHeight);

    HintPos := SynEd.ClientToScreen(Point(P1.X, P2.Y));
    Result := TRect.Create(P1,P2);
  end;

var
  Indicator: TSynIndicator;
  BC1, BC2: TBufferCoord;
  TokenType, Start: Integer;
  Token, DottedIdent: string;
  Attri: TSynHighlighterAttributes;
begin
  CanShow := False;

  // No hints for Gutter
  var SynEd := HintInfo.HintControl as TCustomSynEdit;
  if (SynEd.Gutter.Visible) and (HintInfo.CursorPos.X < SynEd.GutterWidth) then
    Exit;
  if (HintInfo.CursorPos.Y div SynEd.LineHeight) > (SynEd.DisplayRowCount - SynEd.TopLine) then
    Exit;

  var Highlighter := TSynPythonSyn(SynEd.Highlighter);

  var DC := SynEd.PixelsToNearestRowColumn(HintInfo.CursorPos.X, HintInfo.CursorPos.Y);
  var BC := SynEd.DisplayToBufferPos(DC);

  // Diagnostic errors hints first
  if (Length(FEditor.FSynLsp.Diagnostics) > 0) and
    SynEd.Indicators.IndicatorAtPos(BC,
    FEditor.FSynLsp.DiagnosticsIndicatorIds, Indicator)
  then
  begin
    CanShow := True;
    BC1 := BufferCoord(Indicator.CharStart, BC.Line);
    BC2 := BufferCoord(Indicator.CharEnd, BC.Line);
    // Setting HintInfo.CursorRect is important.  Otherwise no other hint
    // will be shown unlessmouse leaves and reenters the control
    HintInfo.CursorRect := CursorRect(SynEd, BC1, BC2, HintInfo.HintPos);
    HintStr := FEditor.FSynLsp.Diagnostics[Indicator.Tag].Hint(SynEdit.ReadOnly);
    FEditor.FSynLsp.DiagnosticHintIndex := Indicator.Tag;
  end
  else if FEditor.HasPythonFile and not SynEd.IsPointInSelection(BC) and
    SynEd.GetHighlighterAttriAtRowColEx(BC, Token, TokenType, Start, Attri) and
    (((GI_PyControl.DebuggerState in [dsPaused, dsPostMortem]) and
       PyIDEOptions.ShowDebuggerHints) or
       (GI_PyControl.Inactive and PyIDEOptions.ShowCodeHints)) and
    ((Attri = Highlighter.IdentifierAttri) or
     (Attri = Highlighter.NonKeyAttri) or
     (Attri = Highlighter.SystemAttri) or
      // bracketed debugger expression
     ((Attri = Highlighter.SymbolAttri) and
      (GI_PyControl.DebuggerState in [dsPaused, dsPostMortem]) and
      ((Token = ')') or (Token = ']')))) then
  begin
    // LSP or debugger hints
    if GI_PyControl.Inactive then
    begin
      BC1 := BufferCoord(Start, BC.Line);
      BC2 := BufferCoord(Start + Token.Length, BC.Line);
    end
    else
    begin
      var LineTxt := SynEd.Lines[BC.Line - 1];
      DottedIdent := GetWordAtPos(LineTxt, BC.Char,
        True, True, False, True);
      BC1 := BufferCoord(BC.Char - Length(DottedIdent) + 1, BC.Line);
      DottedIdent := DottedIdent + GetWordAtPos(LineTxt,
        BC.Char + 1, False, False, True);
      BC2 := BufferCoord(BC1.Char + DottedIdent.Length, BC.Line);
    end;
    HintInfo.CursorRect := CursorRect(SynEd, BC1, BC2, HintInfo.HintPos);

    if (FHintCursorRect = HintInfo.CursorRect) and Assigned(FHintFuture) and
      (FHintFuture.Status = TTaskStatus.Completed) then
    begin
      HintStr := FHintFuture.Value;
      CanShow := HintStr <> '';
      FHintFuture := nil;
    end
    else
    begin
      CanShow := False;
      FHintCursorRect := HintInfo.CursorRect;
      HintInfo.ReshowTimeout := 200;

      if GI_PyControl.Inactive then
      begin
        // LSP code hints
        FHintFuture := TFuture<string>.Create(nil, nil,
          function: string
          begin
            Result := TPyLspClient.CodeHintAtCoordinates(
              FEditor.GetFileId, BC1, Token);
          end, TThreadPool.Default).Start;
      end
      else
      begin
        // Debugger hints
        FHintFuture := TFuture<string>.Create(nil, nil,
          function: string
          var
            ObjectValue, ObjectType: string;
          begin
            GI_PyControl.ActiveDebugger.Evaluate(DottedIdent, ObjectType,
              ObjectValue);
            if ObjectValue <> _(SNotAvailable) then
            begin
              ObjectValue := HTMLSafe(ObjectValue);
              ObjectType := HTMLSafe(ObjectType);
              Result := Format(_(SDebuggerHintFormat),
                [DottedIdent, ObjectType, ObjectValue]);
            end
            else
              Result := '';
          end, TThreadPool.Default).Start;
      end;
    end;
  end
  else
    FHintFuture := nil;

  if CanShow then
  begin
    HintInfo.HintData := Pointer(NativeInt(SynEd.LineHeight));
    HintInfo.HintWindowClass := TCodeHintWindow;
    FHintFuture := nil;
  end;
end;

procedure TEditorForm.BreakpointContextPopup(Sender: TObject;
    MousePos: TPoint; Row, Line: Integer; var Handled: Boolean);
var
  Breakpoint: TBreakpoint;
begin
  if Breakpoints.FindBreakpoint(Line, Breakpoint) then
  begin
    pmnuBreakpoint.Tag := Integer(Breakpoint);
    spiBreakpointEnabled.Checked := not Breakpoint.Disabled;
    MousePos := ClientToScreen(MousePos);
    pmnuBreakpoint.Popup(MousePos.X, MousePos.Y);
    Handled := True;
  end;
end;

function TEditorForm.FormHelp(Command: Word; Data: THelpEventData; var
    CallHelp: Boolean): Boolean;
begin
  CallHelp := True;
  Result := False;

  if (Command = HELP_CONTEXT) and (Data = SynEdit.HelpContext) then
  begin
    var Keyword := SynEdit.WordAtCursor;
    if Keyword <> '' then
    begin
      CallHelp := not CommandsDataModule.ShowPythonKeywordHelp(Keyword);
      Result := True;
    end
  end;
end;

procedure TEditorForm.mnFixIssueClick(Sender: TObject);
begin
  // Use the mechanism of the diagnostic hint
  TLspSynEditPlugin.DiagnosticHintIndex := pmnuDiagnostics.Tag;
  FEditor.FSynLsp.PerformQuickFix;
end;

procedure TEditorForm.mnIgnoreIssueClick(Sender: TObject);
begin
  // Use the mechanism of the diagnostic hint
  TLspSynEditPlugin.DiagnosticHintIndex := pmnuDiagnostics.Tag;
  FEditor.FSynLsp.PerformNoqaEdit;
end;

procedure TEditorForm.pmnuRefactorClosePopup(Sender: TObject);
begin
  CleanUpRefactoringMenu(pmnuRefactor.Items);
end;

procedure TEditorForm.pmnuRefactorPopup(Sender: TObject);
begin
  SetupRefactoringMenu(FEditor.GetFileId, FEditor.GetActiveSynEdit.BlockBegin,
    FEditor.GetActiveSynEdit.BlockEnd, pmnuRefactor.Items);
end;

procedure TEditorForm.spiBreakpointClearClick(Sender: TObject);
begin
  GI_BreakpointManager.ToggleBreakpoint(FEditor.GetFileId,
    TBreakpoint(pmnuBreakpoint.Tag).LineNo);
end;

procedure TEditorForm.spiBreakpointEnabledClick(Sender: TObject);
begin
  with TBreakpoint(pmnuBreakpoint.Tag) do
    GI_BreakpointManager.SetBreakpoint(fEditor.GetFileId, LineNo,
      not spiBreakpointEnabled.Checked, Condition, IgnoreCount);
end;

procedure TEditorForm.spiBreakpointPropertiesClick(Sender: TObject);
var
  Condition: string;
  IgnoreCount: Integer;
begin
  var Breakpoint := TBreakpoint(pmnuBreakpoint.Tag);
  Condition := Breakpoint.Condition;
  IgnoreCount := Breakpoint.IgnoreCount;

  if GI_BreakpointManager.EditProperties(Condition, IgnoreCount) then
  begin
    GI_BreakpointManager.SetBreakpoint(FEditor.GetFileId, Breakpoint.LineNo,
      Breakpoint.Disabled, Condition, IgnoreCount);
  end;
end;

procedure TEditorForm.SynEditGutterMarksMouseCursor(Sender: TObject; X, Y, Row,
    Line: Integer; var Cursor: TCursor);
var
  Marks: TSynEditMarks;
begin
  SynEdit.Marks.GetMarksForLine(Line, Marks);
  for var I := Low(Marks) to High(Marks) do
  begin
    if Marks[I] = nil then Exit;
    if not Marks[I].IsBookmark then
    begin
      Cursor := crHandPoint;
      Exit;
    end;
  end;
end;

procedure TEditorForm.SynEditGutterMarksCLick(Sender: TObject; Button:
    TMouseButton; X, Y, Row, Line: Integer);
var
  Marks: TSynEditMarks;
  HasFix, HasIgnore: Boolean;
  Title: string;
begin
  pmnuDiagnostics.Tag := -1;
  SynEdit.Marks.GetMarksForLine(Line, Marks);

  if (Length(Marks) = 0) or Marks[0].IsBookmark or
    not InRange(Marks[0].Tag, 0, High(FEditor.FSynLsp.Diagnostics))
  then
    Exit;

  var Diagnostic := FEditor.FSynLsp.Diagnostics[Marks[0].Tag];
  Diagnostic.ProcessData(HasFix, HasIgnore, Title);

  if HasFix or HasIgnore then
  begin
    pmnuDiagnostics.Tag := Marks[0].Tag;
    mnFixIssue.Enabled := HasFix;
    mnFixIssue.Hint := Title;
    mnIgnoreIssue.Enabled := HasIgnore;
    var ScreenP := ClientToScreen(Point(X, Y));
    pmnuDiagnostics.Popup(ScreenP.X, ScreenP.Y);
  end;
end;

procedure TEditorForm.SynEditTSynGutterBands0ContextPopup(Sender: TObject;
    MousePos: TPoint; Row, Line: Integer; var Handled: Boolean);
begin
  SynEditGutterMarksCLick(Sender, TMouseButton.mbLeft, MousePos.X, MousePos.Y,
    Row, Line);
  Handled := pmnuDiagnostics.Tag >= 0;
end;

procedure TEditorForm.UpdateTabImage;
begin
  if SynEdit.ReadOnly then
    ParentTabItem.ImageIndex := PyIDEMainForm.vilTabDecorators.GetIndexByName('Lock')
  else if FEditor.FSynLsp.Diagnostics.HasErrors then
    ParentTabItem.ImageIndex := PyIDEMainForm.vilTabDecorators.GetIndexByName('Bug')
  else if FEditor.FSynLsp.Diagnostics.HasWarnings then
    ParentTabItem.ImageIndex := PyIDEMainForm.vilTabDecorators.GetIndexByName('Alert')
  else
    ParentTabItem.ImageIndex := -1;
end;

{$ENDREGION 'TEditorForm'}


initialization
  GI_EditorFactory := TEditorFactory.Create;
  TCodeHintWindow.OnHyperLinkClick := TEditorForm.CodeHintLinkHandler;

finalization
  GI_EditorFactory := nil;
end.
