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
  WinApi.Windows,
  WinApi.Messages,
  Winapi.D2D1,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Contnrs,
  System.ImageList,
  System.Threading,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Menus,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ImgList,
  Vcl.Dialogs,
  Vcl.VirtualImageList,
  TB2Item,
  SpTBXItem,
  SpTBXSkins,
  SpTBXDkPanels,
  SpTBXTabs,
  SpTBXControls,
  SynEdit,
  SynEditTypes,
  SynEditHighlighter,
  SynEditMiscClasses,
  SynEditKeyCmds,
  SynCompletionProposal,
  SynEditLsp,
  VirtualResources,
  uCommonFunctions,
  frmCodeExplorer,
  JediLspClient,
  uEditAppIntfs,
  cPyControl,
  cCodeCompletion,
  cPyBaseDebugger,
  cPySupportTypes;

type
  TEditor = class;

  THotIdentInfo = record
    HaveHotIdent: boolean;
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
    mnEditDelete: TSpTBXItem;
    mnEditSelectAll: TSpTBXItem;
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
    class procedure SynParamCompletionExecute(Kind: SynCompletionType;
      Sender: TObject; var CurrentInput: string; var X, Y: Integer;
      var CanExecute: boolean);
    procedure FormDestroy(Sender: TObject);
    procedure SynEditChange(Sender: TObject);
    procedure SynEditEnter(Sender: TObject);
    procedure SynEditExit(Sender: TObject);
    procedure SynEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure FormCreate(Sender: TObject);
    procedure SynEditSpecialLineColors(Sender: TObject; Line: Integer;
      var Special: boolean; var FG, BG: TColor);
    procedure SynEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SynEditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FGPanelEnter(Sender: TObject);
    procedure FGPanelExit(Sender: TObject);
    procedure mnCloseTabClick(Sender: TObject);
    procedure SynEditMouseCursor(Sender: TObject;
      const aLineCharPos: TBufferCoord; var aCursor: TCursor);
    procedure SynEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SynCodeCompletionExecute(Kind: SynCompletionType;
      Sender: TObject; var CurrentInput: string; var X, Y: Integer;
      var CanExecute: boolean);
    procedure SynEditMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: boolean);
    procedure SynEditMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: boolean);
    procedure SynCodeCompletionClose(Sender: TObject);
    procedure SynWebCompletionExecute(Kind: SynCompletionType; Sender: TObject;
      var CurrentInput: string; var X, Y: Integer; var CanExecute: boolean);
    procedure SynWebCompletionAfterCodeCompletion(Sender: TObject;
      const Value: string; Shift: TShiftState; Index: Integer;
      EndToken: WideChar);
    procedure mnUpdateViewClick(Sender: TObject);
    procedure ViewsTabControlContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: boolean);
    procedure ViewsTabControlActiveTabChange(Sender: TObject;
      TabIndex: Integer);
    procedure SynEditDblClick(Sender: TObject);
    procedure SynCodeCompletionAfterCodeCompletion(Sender: TObject;
      const Value: string; Shift: TShiftState; Index: Integer; EndToken: Char);
    procedure SynEditGutterGetText(Sender: TObject; aLine: Integer;
      var aText: string);
    procedure SynEditDebugInfoPaintLines(RT: ID2D1RenderTarget; ClipR:
        TRect; const FirstRow, LastRow: Integer; var DoDefaultPainting: Boolean);
    procedure SynEditGutterDebugInfoCLick(Sender: TObject; Button: TMouseButton;
        X, Y, Row, Line: Integer);
    procedure SynEditGutterDebugInfoMouseCursor(Sender: TObject; X, Y, Row, Line:
        Integer; var Cursor: TCursor);
    procedure EditorShowHint(var HintStr: string; var CanShow: Boolean; var
        HintInfo: Vcl.Controls.THintInfo);
  private
    const HotIdentIndicatorSpec: TGUID = '{8715589E-C990-4423-978F-F00F26041AEF}';
  private
    fEditor: TEditor;
    fActiveSynEdit: TSynEdit;
    fAutoCompleteActive: boolean;
    fHotIdentInfo: THotIdentInfo;
    fNeedToSyncCodeExplorer: boolean;
    fCloseBracketChar: WideChar;
    fOldCaretY : Integer;
    // Hints
    FHintFuture: IFuture<string>;
    FHintCursorRect: TRect;
    function DoAskSaveChanges: boolean;
    procedure DoAssignInterfacePointer(AActive: boolean);
    function DoSave: boolean;
    function DoSaveFile: boolean;
    function DoSaveAs: boolean;
    function DoSaveAsRemote: boolean;
    procedure DoUpdateCaption;
    procedure DoUpdateHighlighter(HighlighterName: string = '');
    procedure AutoCompleteBeforeExecute(Sender: TObject);
    procedure AutoCompleteAfterExecute(Sender: TObject);
    procedure WMFolderChangeNotify(var Msg: TMessage); message WM_FOLDERCHANGENOTIFY;
    procedure SynCodeCompletionCodeItemInfo(Sender: TObject;
      AIndex: Integer; var Info : string);
    procedure ApplyPyIDEOptions;
    class procedure DoCodeCompletion(Editor: TSynEdit; Caret: TBufferCoord);
    class procedure SymbolsChanged(Sender: TObject);
    class var fOldEditorForm: TEditorForm;
    class procedure CodeHintLinkHandler(Sender: TObject; LinkName: string);
  protected
    procedure Retranslate;
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
    procedure EditorZoom(theZoom: Integer);
    procedure EditorMouseWheel(theDirection: Integer; Shift: TShiftState);
  public
    BorderHighlight : TColor;
    BorderNormal : TColor;
    BreakPoints: TObjectList;
    HasFocus: boolean;
    FileTime: TDateTime;
    DefaultExtension: string;
    ParentTabItem: TSpTBXTabItem;
    ParentTabControl: TSpTBXCustomTabControl;
    HasSearchHighlight: Boolean;
    procedure ApplyEditorOptions;
    procedure DoActivate;
    procedure DoActivateEditor(Primary: boolean = True);
    function DoActivateView(ViewFactory: IEditorViewFactory): IEditorView;
    function GetEditor: IEditor;
    procedure EditorCommandHandler(Sender: TObject; AfterProcessing: boolean;
      var Handled: boolean; var Command: TSynEditorCommand;
      var AChar: WideChar; Data: Pointer; HandlerData: Pointer);
    procedure DoOnIdle;
    procedure SyncCodeExplorer;
    procedure AddWatchAtCursor;
    function HasSyntaxError: boolean;
    procedure GoToSyntaxError;
  end;

  TEditor = class(TInterfacedObject, IUnknown, IEditor, IEditCommands,
    IFileCommands, ISearchCommands)
  private
    // IEditor implementation
    procedure Activate(Primary: boolean = True);
    function ActivateView(ViewFactory: IEditorViewFactory): IEditorView;
    function AskSaveChanges: boolean;
    procedure Close;
    function GetCaretPos: TPoint;
    function GetSynEdit: TSynEdit;
    function GetSynEdit2: TSynEdit;
    function GetActiveSynEdit: TSynEdit;
    function GetBreakPoints: TObjectList;
    function GetEditorState: string;
    function GetFileName: string;
    function GetFileTitle: string;
    function GetFileId: string;
    function GetModified: boolean;
    function GetHasSearchHighlight: Boolean;
    function GetFileEncoding: TFileSaveFormat;
    procedure SetFileEncoding(FileEncoding: TFileSaveFormat);
    procedure SetHighlighter(const HighlighterName: string);
    function GetEncodedText: AnsiString;
    procedure OpenFile(const AFileName: string; HighlighterName: string = '');
    procedure OpenRemoteFile(const FileName, ServerName: string);
    function SaveToRemoteFile(const FileName, ServerName: string) : boolean;
    function HasPythonFile: boolean;
    function GetReadOnly : Boolean;
    procedure SetReadOnly(Value : Boolean);
    procedure SetHasSearchHighlight(Value : Boolean);
    procedure ExecuteSelection;
    procedure SplitEditorHorizontally;
    procedure SplitEditorVertrically;
    procedure RefreshSymbols;
    procedure Retranslate;
    function GetForm: TForm;
    function GetDocSymbols: TObject;
    function GetTabControlIndex: Integer;
    function GetRemoteFileName: string;
    function GetSSHServer: string;
    // IEditCommands implementation
    function CanCopy: boolean;
    function CanCut: boolean;
    function IEditCommands.CanDelete = CanCut;
    function CanPaste: boolean;
    function CanRedo: boolean;
    function CanSelectAll: boolean;
    function CanUndo: boolean;
    procedure ExecCopy;
    procedure ExecCut;
    procedure ExecDelete;
    procedure ExecPaste;
    procedure ExecRedo;
    procedure ExecSelectAll;
    procedure ExecUndo;
    // IFileCommands implementation
    function CanClose: boolean;
    function CanPrint: boolean;
    function CanSave: boolean;
    function CanSaveAs: boolean;
    function CanReload: boolean;
    procedure ExecClose;
    procedure ExecPrint;
    procedure ExecPrintPreview;
    procedure ExecSave;
    procedure ExecSaveAs;
    procedure ExecSaveAsRemote;
    procedure ExecReload(Quiet: boolean = False);
    // ISearchCommands implementation
    function CanFind: boolean;
    function CanFindNext: boolean;
    function ISearchCommands.CanFindPrev = CanFindNext;
    function ISearchCommands.GetSearchTarget = GetActiveSynEdit;
    function CanReplace: boolean;
    procedure ExecFind;
    procedure ExecFindNext;
    procedure ExecFindPrev;
    procedure ExecReplace;
  private
    fFileName: string;
    fRemoteFileName : string;
    fSSHServer : string;
    fForm: TEditorForm;
    fHasSelection: boolean;
    fUntitledNumber: Integer;
    FSynLsp: TLspSynEditPlugin;
    function IsEmpty: boolean;
    procedure DoSetFileName(AFileName: string);
    function GetEncodedTextEx(var EncodedText: AnsiString;
      InformationLossWarning: boolean): boolean;
  public
    constructor Create(AForm: TEditorForm);
    destructor Destroy; override;
    class var UntitledNumbers: TBits;
    class function GetUntitledNumber: Integer;
    class constructor Create;
    class destructor Destroy;
  end;

implementation

{$R *.DFM}

uses
  System.Math,
  System.IOUtils,
  VirtualShellNotifier,
  PythonEngine,
  VarPyth,
  Vcl.Themes,
  JclFileUtils,
  SynUnicode,
  SynEditTextBuffer,
  SynHighlighterWebMisc,
  SynHighlighterWeb,
  SynHighlighterPython,
  SynDWrite,
  JvDockControlForm,
  JvGnugettext,
  StringResources,
  dmResources,
  dmCommands,
  uHighlighterProcs,
  dlgSynPrintPreview,
  dlgPickList,
  frmPyIDEMain,
  frmBreakPoints,
  frmPythonII,
  frmWatches,
  uSearchHighlighter,
  cPyDebugger,
  cCodeHint,
  cPyScripterSettings,
  cSSHSupport,
  dlgRemoteFile;

const
  WM_DELETETHIS = WM_USER + 42;

  { TGutterMarkDrawPlugin }

type
  TDebugSupportPlugin = class(TSynEditPlugin)
  protected
    fForm: TEditorForm;
    procedure LinesInserted(FirstLine, Count: Integer); override;
    procedure LinesDeleted(FirstLine, Count: Integer); override;
  public
    constructor Create(AForm: TEditorForm);
  end;

constructor TDebugSupportPlugin.Create(AForm: TEditorForm);
begin
  inherited Create(AForm.SynEdit);
  FHandlers := [phLinesInserted, phLinesDeleted];
  fForm := AForm;
end;

procedure TDebugSupportPlugin.LinesInserted(FirstLine, Count: Integer);
var
  i: Integer;
begin
  with fForm do
  begin
    for i := 0 to BreakPoints.Count - 1 do
      if TBreakPoint(BreakPoints[i]).LineNo >= FirstLine then
      begin
        TBreakPoint(BreakPoints[i]).LineNo := TBreakPoint(BreakPoints[i])
          .LineNo + Count;
        PyControl.BreakPointsChanged := True;
        BreakPointsWindow.UpdateWindow;
      end;
  end;
end;

procedure TDebugSupportPlugin.LinesDeleted(FirstLine, Count: Integer);
var
  i: Integer;
begin
  with fForm do
  begin
    for i := BreakPoints.Count - 1 downto 0 do
      if TBreakPoint(BreakPoints[i]).LineNo >= FirstLine + Count then
      begin
        TBreakPoint(BreakPoints[i]).LineNo := TBreakPoint(BreakPoints[i])
          .LineNo - Count;
        PyControl.BreakPointsChanged := True;
        BreakPointsWindow.UpdateWindow;
      end
      else if TBreakPoint(BreakPoints[i]).LineNo >= FirstLine then
      begin
        BreakPoints.Delete(i);
        PyControl.BreakPointsChanged := True;
        BreakPointsWindow.UpdateWindow;
      end;
  end;
end;

{ TEditor }

constructor TEditor.Create(AForm: TEditorForm);
begin
  Assert(AForm <> nil);
  inherited Create;
  FForm := AForm;
  FUntitledNumber := -1;
  SetFileEncoding(PyIDEOptions.NewFileEncoding);
  FSynLsp := TLspSynEditPlugin.Create(fForm.SynEdit);
  FSynLsp.DocSymbols.OnNotify := FForm.SymbolsChanged;
end;

procedure TEditor.Activate(Primary: boolean = True);
begin
  if fForm <> nil then
    fForm.DoActivateEditor(Primary);
end;

function TEditor.ActivateView(ViewFactory: IEditorViewFactory): IEditorView;
begin
  if fForm <> nil then
    Result := fForm.DoActivateView(ViewFactory);
end;

function TEditor.AskSaveChanges: boolean;
begin
  if fForm <> nil then
    Result := fForm.DoAskSaveChanges
  else
    Result := True;
end;

procedure TEditor.Close;
// Closes without asking
Var
  TabSheet: TSpTBXTabSheet;
  TabControl: TSpTBXCustomTabControl;
begin
  if (fForm <> nil) then
  begin
    FSynLsp.FileClosed;
    GI_PyIDEServices.MRUAddEditor(Self);
    if fUntitledNumber <> -1 then
      UntitledNumbers[fUntitledNumber] := False;

    fForm.DoAssignInterfacePointer(False);
    Assert(GI_EditorFactory <> nil);
    GI_EditorFactory.RemoveEditor(Self);
    if GI_EditorFactory.Count = 0 then
      PyIDEMainForm.UpdateCaption;

    TabSheet := (fForm.Parent as TSpTBXTabSheet);
    TabControl := TabSheet.TabControl;
    TabControl.View.BeginUpdate;
    try
      (fForm.ParentTabControl as TSpTBXTabControl).zOrder.Remove(TabSheet.Item);
      fForm := nil;
      // The form is owned by the tabshhet and it is also destroyed
      // The SynEdit plugin FSynLsp will also be destroyed
      TabSheet.Free;
      if Assigned(TabControl) then
        TabControl.Toolbar.MakeVisible(TabControl.ActiveTab);
    finally
      TabControl.View.EndUpdate;
    end;
  end;
end;

class constructor TEditor.Create;
begin
  UntitledNumbers := TBits.Create;
  UntitledNumbers[0] := True;  // do not use 0
end;

destructor TEditor.Destroy;
begin
  // Kept for dubugging
  inherited;
end;

class destructor TEditor.Destroy;
begin
  UntitledNumbers.Free;
end;

procedure TEditor.DoSetFileName(AFileName: string);
begin
  if AFileName <> fFileName then
  begin
    fFileName := AFileName;
    fRemoteFileName := '';
    fSSHServer := '';
    if ((AFileName <> '') or (fRemoteFileName <> '')) and (fUntitledNumber <> -1) then
    begin
      UntitledNumbers[fUntitledNumber] := False;
      fUntitledNumber := -1;
    end;
    // Kernel change notification
    if (fFileName <> '') and FileExists(fFileName) then
      ChangeNotifier.NotifyWatchFolder(fForm, TPath.GetDirectoryName(fFileName))
    else
      ChangeNotifier.NotifyWatchFolder(fForm, '');
  end;
end;

function TEditor.GetSynEdit: TSynEdit;
begin
  Result := fForm.SynEdit;
end;

function TEditor.GetSynEdit2: TSynEdit;
begin
  Result := fForm.SynEdit2;
end;

function TEditor.GetTabControlIndex: Integer;
begin
  Result := PyIDEMainForm.TabControlIndex(fForm.ParentTabControl);
end;

class function TEditor.GetUntitledNumber: Integer;
begin
  Result := UntitledNumbers.OpenBit;
  UntitledNumbers[Result] := True;
end;

function TEditor.GetActiveSynEdit: TSynEdit;
begin
  if fForm.SynEdit2.Visible and (fForm.fActiveSynEdit = fForm.SynEdit2) then
    Result := fForm.SynEdit2
  else
    Result := fForm.SynEdit;
end;

function TEditor.GetBreakPoints: TObjectList;
begin
  Result := fForm.BreakPoints;
end;

function TEditor.GetCaretPos: TPoint;
begin
  if fForm <> nil then
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
  if fForm <> nil then
  begin
    if fForm.SynEdit.ReadOnly then
      Result := _(SReadOnly)
    else if fForm.SynEdit.InsertMode then
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
  InformationLossWarning: boolean): boolean;
begin
  Result := WideStringsToEncodedText(GetFileId, fForm.SynEdit.Lines,
    EncodedText, InformationLossWarning, HasPythonFile);
end;

function TEditor.GetFileName: string;
begin
  Result := fFileName;
end;

function TEditor.GetFileTitle: string;
begin
  if fFileName <> '' then
  begin
    if PyIDEOptions.DisplayPackageNames and
      FileIsPythonPackage(fFileName)
    then
      Result := FileNameToModuleName(fFileName)
    else
      Result := TPath.GetFileName(fFileName);
  end
  else if fSSHServer <> '' then
    Result := TSSHFileName.Format(fSSHServer, fRemoteFileName)
  else
  begin
    if fUntitledNumber = -1 then
      fUntitledNumber := GetUntitledNumber;
    if fForm.SynEdit.Highlighter = ResourcesDataModule.SynPythonSyn then
      Result := _(SNonamePythonFileTitle) + IntToStr(fUntitledNumber)
    else
      Result := _(SNonameFileTitle) + IntToStr(fUntitledNumber);
  end;
end;

function TEditor.GetFileId: string;
begin
  if fFileName <> '' then
    Result := fFileName
  else
    Result := GetFileTitle;
end;

function TEditor.GetModified: boolean;
begin
  if fForm <> nil then
    Result := fForm.SynEdit.Modified
  else
    Result := False;
end;

function TEditor.GetReadOnly: Boolean;
begin
  Result := GetSynEdit.ReadOnly;
end;

function TEditor.GetRemoteFileName: string;
begin
  Result := fRemoteFileName;
end;

function TEditor.GetSSHServer: string;
begin
  Result := fSSHServer;
end;

function TEditor.GetFileEncoding: TFileSaveFormat;
begin
  with fForm.SynEdit.Lines do
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
  with TSynEditStringList(fForm.SynEdit.Lines) do
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
      WriteBom := True;
  end;
end;

procedure TEditor.SetHasSearchHighlight(Value: Boolean);
begin
  fForm.HasSearchHighlight :=  Value;
end;

procedure TEditor.SetHighlighter(const HighlighterName: string);
begin
  fForm.DoUpdateHighlighter(HighlighterName);
end;

procedure TEditor.SetReadOnly(Value: Boolean);
begin
  GetSynEdit.ReadOnly := Value;
  GetSynEdit2.ReadOnly := Value;
end;

procedure TEditor.SplitEditorHorizontally;
begin
  with fForm do
  begin
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
  with fForm do
  begin
    EditorSplitter.Visible := False;
    SynEdit2.Visible := False;
    SynEdit2.Align := alRight;
    SynEdit2.Width := (ClientWidth - 5) div 2;
    EditorSplitter.Align := alRight;
    SynEdit2.Visible := True;
    EditorSplitter.Visible := True;
  end;
end;

procedure TEditor.OpenFile(const AFileName: string;
  HighlighterName: string = '');
begin
  if FForm = nil then Abort;

  if (AFileName <> '') and FileExists(AFileName) then
  begin
    fForm.SynEdit.LockUndo;
    try
      if LoadFileIntoWideStrings(AFileName, fForm.SynEdit.Lines) then
      begin
        if not FileAge(AFileName, fForm.FileTime) then
          fForm.FileTime := 0;
      end
      else
        Abort;
    finally
      fForm.SynEdit.UnlockUndo;
    end;
  end
  else
  begin
    fForm.SynEdit.Lines.Clear;
    if AFileName = '' then
    begin
      // Default settings for new files
      if PyIDEOptions.NewFileLineBreaks <> sffUnicode then
        (fForm.SynEdit.Lines as TSynEditStringList).FileFormat :=
          PyIDEOptions.NewFileLineBreaks;
    end;
  end;

  DoSetFileName(AFileName);
  fForm.DoUpdateHighlighter(HighlighterName);
  fForm.DoUpdateCaption;

  fForm.SynEdit.Modified := False;
  fForm.Synedit.UseCodeFolding := PyIDEOptions.CodeFoldingEnabled;
  fForm.Synedit2.UseCodeFolding := fForm.Synedit.UseCodeFolding;

  if HasPythonFile then
    FSynLsp.FileOpened(GetFileId, lidPython)
  else
    FSynLsp.FileOpened(GetFileId, lidNone);
end;

procedure TEditor.OpenRemoteFile(const FileName, ServerName: string);
Var
  TempFileName : string;
  ErrorMsg : string;
begin
  if (fForm = nil) or (FileName = '') or (ServerName = '') then Abort;

  TempFileName := ChangeFileExt(FileGetTempName('PyScripter'), ExtractFileExt(FileName));
  if not GI_SSHServices.ScpDownload(ServerName, FileName, TempFileName, ErrorMsg) then begin
    StyledMessageDlg(Format(_(SFileOpenError), [FileName, ErrorMsg]), mtError, [mbOK], 0);
    Abort;
  end else
  begin
    fForm.SynEdit.LockUndo;
    try
      if not LoadFileIntoWideStrings(TempFileName, fForm.SynEdit.Lines) then
        Abort
      else
        DeleteFile(TempFileName);
    finally
      fForm.SynEdit.UnlockUndo
    end;
  end;

  fRemoteFileName := FileName;
  fSSHServer := ServerName;
  DoSetFileName('');

  fForm.SynEdit.Modified := False;
  fForm.Synedit.UseCodeFolding := PyIDEOptions.CodeFoldingEnabled;
  fForm.Synedit2.UseCodeFolding := fForm.Synedit.UseCodeFolding;

  fForm.DoUpdateHighlighter('');
  fForm.DoUpdateCaption;
  if HasPythonFile then
    FSynLsp.FileOpened(GetFileId, lidPython)
  else
    FSynLsp.FileOpened(GetFileId, lidNone);
end;

function TEditor.SaveToRemoteFile(const FileName, ServerName: string): boolean;
Var
  TempFileName : string;
  ErrorMsg : string;
begin
  if (fForm = nil)  or (FileName = '') or (ServerName = '') then  Abort;

  TempFileName := FileGetTempName('PyScripter');
  Result := SaveWideStringsToFile(TempFileName, fForm.SynEdit.Lines, False);
  if Result then begin
    Result := GI_SSHServices.ScpUpload(ServerName, TempFileName, FileName, ErrorMsg);
    DeleteFile(TempFileName);
    if not Result then
      StyledMessageDlg(Format(_(SFileSaveError), [FileName, ErrorMsg]), mtError, [mbOK], 0);
  end;
end;

procedure TEditor.RefreshSymbols;
begin
  if FSynLsp.NeedToRefreshSymbols then
    FSynLsp.RefreshSymbols(GetFileId);
end;

procedure TEditor.Retranslate;
begin
  fForm.Retranslate;
end;

function TEditor.HasPythonFile: boolean;
begin
  Result := GetSynEdit.Highlighter is TSynPythonSyn;
end;

function TEditor.GetForm: TForm;
begin
  Result := fForm;
end;

function TEditor.GetHasSearchHighlight: Boolean;
begin
  Result := fForm.HasSearchHighlight;
end;

// IEditCommands implementation

function TEditor.CanCopy: boolean;
begin
  Result := GetActiveSynEdit.SelAvail or (GetActiveSynEdit.LineText <> '');
end;

function TEditor.CanCut: boolean;
begin
  Result := (fForm <> nil) and not GetReadOnly;
end;

function TEditor.CanPaste: boolean;
begin
  Result := (fForm <> nil) and GetActiveSynEdit.CanPaste;
end;

function TEditor.CanRedo: boolean;
begin
  Result := (fForm <> nil) and fForm.SynEdit.CanRedo;
end;

function TEditor.CanReload: boolean;
begin
  Result := fFileName <> '';
end;

function TEditor.CanSelectAll: boolean;
begin
  Result := fForm <> nil;
end;

function TEditor.CanUndo: boolean;
begin
  Result := (fForm <> nil) and fForm.SynEdit.CanUndo;
end;

procedure TEditor.ExecCopy;
begin
  if fForm <> nil then
    GetActiveSynEdit.CopyToClipboard;
end;

procedure TEditor.ExecCut;
begin
  if fForm <> nil then
    GetActiveSynEdit.CutToClipboard;
end;

procedure TEditor.ExecDelete;
begin
  if fForm <> nil then
    GetActiveSynEdit.SelText := '';
end;

procedure TEditor.ExecPaste;
begin
  if fForm <> nil then
    GetActiveSynEdit.PasteFromClipboard;
end;

procedure TEditor.ExecRedo;
begin
  if fForm <> nil then
    fForm.SynEdit.Redo;
end;

procedure TEditor.ExecReload(Quiet: boolean = False);
Var
  P: TBufferCoord;
begin
  if Quiet or not GetModified or (StyledMessageDlg(_(SFileReloadingWarning),
      mtWarning, [mbYes, mbNo], 0) = mrYes) then
  begin
    P := GetSynEdit.CaretXY;
    OpenFile(GetFileName);
    if (P.Line <= GetSynEdit.Lines.Count) then
      GetSynEdit.CaretXY := P;
  end;
end;

procedure TEditor.ExecSelectAll;
begin
  if fForm <> nil then
    GetActiveSynEdit.SelectAll;
end;

procedure TEditor.ExecUndo;
begin
  if fForm <> nil then
    fForm.SynEdit.Undo;
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
  if not fHasSelection then
  begin
    Source := Editor.WordAtCursor;
    if Source <> '' then
      ExecType := 'single'
    else
      Exit;
  end
  else
  begin
    Source := Editor.SelText;
    // if a single line or part of a line is selected then eval the selection
    if Editor.BlockBegin.Line = Editor.BlockEnd.Line then
      ExecType := 'single'
    else
      Source := Source + sLineBreak; // issue 291
  end;

  // Dedent the selection
  Source := Dedent(Source);

  GI_PyInterpreter.ShowWindow;
  GI_PyInterpreter.AppendText(sLineBreak);
  PythonIIForm.SynEdit.ExecuteCommand(ecEditorBottom, ' ', nil);
  Source := CleanEOLs(Source);

  GI_PyControl.ThreadPythonExec(procedure
  begin
    // RunSource
    case PyControl.DebuggerState of
      dsInactive:
        PyControl.ActiveInterpreter.RunSource(Source, '<editor selection>',
          ExecType);
      dsPaused, dsPostMortem:
        PyControl.ActiveDebugger.RunSource(Source, '<editor selection>',
          ExecType);
    end;
  end,
  procedure
  begin
    GI_PyInterpreter.AppendPrompt;
    Activate(False);
  end);
end;

// IFileCommands implementation

function TEditor.CanPrint: boolean;
begin
  Result := True;
end;

function TEditor.CanSave: boolean;
begin
  Result := (fForm <> nil) and (GetModified or ((fFileName = '') and (fRemoteFileName = '')));
end;

function TEditor.CanSaveAs: boolean;
begin
  Result := fForm <> nil;
end;

function TEditor.CanClose: boolean;
begin
  Result := fForm <> nil;
end;

procedure TEditor.ExecClose;
// Close only after asking
begin
  if AskSaveChanges then
    Close;
end;

procedure TEditor.ExecPrint;
begin
  if fForm <> nil then
    with ResourcesDataModule, CommandsDataModule do
    begin
      SynEditPrint.SynEdit := fForm.SynEdit;
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
  CommandsDataModule.SynEditPrint.SynEdit := fForm.SynEdit;
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
  if fForm <> nil then
  begin
    if (fFileName <> '') or (fRemoteFileName <> '') then
      fForm.DoSave
    else
      ExecSaveAs
  end;
end;

procedure TEditor.ExecSaveAs;
begin
  if (fForm <> nil) then
   fForm.DoSaveAs;
end;

procedure TEditor.ExecSaveAsRemote;
begin
  if (fForm <> nil) then
    fForm.DoSaveAsRemote;
end;

// ISearchCommands implementation

function TEditor.CanFind: boolean;
begin
  Result := (fForm <> nil) and not IsEmpty;
end;

function TEditor.CanFindNext: boolean;
begin
  Result := (fForm <> nil) and not IsEmpty and
    (EditorSearchOptions.SearchText <> '');
end;

function TEditor.CanReplace: boolean;
begin
  Result := (fForm <> nil) and not GetReadOnly and not IsEmpty;
end;

procedure TEditor.ExecFind;
begin
  if fForm <> nil then
    CommandsDataModule.ShowSearchReplaceDialog(GetActiveSynEdit, False);
end;

procedure TEditor.ExecFindNext;
begin
  if fForm <> nil then
    CommandsDataModule.DoSearchReplaceText(GetActiveSynEdit, False, False);
end;

procedure TEditor.ExecFindPrev;
begin
  if fForm <> nil then
    CommandsDataModule.DoSearchReplaceText(GetActiveSynEdit, False, True);
end;

procedure TEditor.ExecReplace;
begin
  if fForm <> nil then
    CommandsDataModule.ShowSearchReplaceDialog(GetActiveSynEdit, True);
end;

function TEditor.IsEmpty: boolean;
begin
  Result := (fForm.SynEdit.Lines.Count = 0) or
    ((fForm.SynEdit.Lines.Count = 1) and (fForm.SynEdit.Lines[0] = ''));
end;

{ TEditorFactory }

type
  TEditorFactory = class(TInterfacedObject, IEditorFactory)
  private
    // IEditorFactory implementation
    function CanCloseAll: boolean;
    procedure CloseAll;
    function NewEditor(TabControlIndex:Integer = 1): IEditor;
    function GetEditorCount: Integer;
    function GetEditorByName(const Name: string): IEditor;
    function GetEditorByFileId(const Name: string): IEditor;
    function GetEditor(Index: Integer): IEditor;
    procedure RemoveEditor(AEditor: IEditor);
    function RegisterViewFactory(ViewFactory: IEditorViewFactory): Integer;
    procedure SetupEditorViewsMenu(ViewsMenu: TSpTBXItem; IL: TCustomImageList);
    procedure UpdateEditorViewsMenu(ViewsMenu: TSpTBXItem);
    procedure CreateRecoveryFiles;
    procedure RecoverFiles;
    function GetViewFactoryCount: Integer;
    function GetViewFactory(Index: Integer): IEditorViewFactory;
    procedure LockList;
    procedure UnlockList;
    procedure ApplyToEditors(const Proc: TProc<IEditor>);
    function FirstEditorCond(const Predicate: TPredicate<IEditor>): IEditor;
  private
    fEditors: TInterfaceList;
    fEditorViewFactories: TInterfaceList;
    constructor Create;
    destructor Destroy; override;
    procedure OnEditorViewClick(Sender: TObject);
  end;

constructor TEditorFactory.Create;
begin
  inherited Create;
  fEditors := TInterfaceList.Create;
  fEditorViewFactories := TInterfaceList.Create;
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
      var FName := (Ed as TEditor).fUntitledNumber.ToString;
      FName := TPath.Combine(RecoveryDir, TPath.ChangeExtension(FName, DefaultExtension));
      SaveWideStringsToFile(FName, SynEdit.Lines, False);
    end;
  end);
end;

destructor TEditorFactory.Destroy;
begin
  fEditors.Free;
  fEditorViewFactories.Free;
  inherited Destroy;
end;

procedure TEditorFactory.ApplyToEditors(const Proc: TProc<IEditor>);
begin
  fEditors.Lock;
  try
    for var I := 0 to fEditors.Count - 1 do
      Proc(IEditor(fEditors[I]));
  finally
    fEditors.Unlock;
  end;
end;

function TEditorFactory.FirstEditorCond(const Predicate: TPredicate<IEditor>): IEditor;
Var
  Editor: IEditor;
begin
  fEditors.Lock;
  try
    Result := nil;
    for var I := 0 to fEditors.Count - 1 do
    begin
      Editor := IEditor(fEditors[I]);
      if Predicate(Editor) then
        Exit(Editor);
    end;
  finally
    fEditors.Unlock;
  end;
end;

function TEditorFactory.CanCloseAll: boolean;
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
    else if ShowModal = IdOK then
    begin
      Result := True;
      for var I := CheckListBox.Count - 1 downto 0 do
      begin
        if CheckListBox.Checked[I] then
        begin
          if not TEditorForm(CheckListBox.Items.Objects[I]).DoSave then
          begin
            Result := False;
            break;
          end;
        end;
      end;
    end;
    Free;
  end;
end;

procedure TEditorFactory.CloseAll;
var
  i: Integer;
begin
  fEditors.Lock;
  try
    i := fEditors.Count - 1;
    while i >= 0 do
    begin
      IEditor(fEditors[i]).Close;
      Dec(i);
    end;
  finally
    fEditors.Unlock;
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
      fEditor := TEditor.Create(LForm);
      ParentTabItem := TabItem;
      ParentTabItem.OnTabClosing := PyIDEMainForm.TabControlTabClosing;
      ParentTabItem.OnDrawTabCloseButton := PyIDEMainForm.DrawCloseButton;
      ParentTabControl := TabControl;
      Result := fEditor;
      BorderStyle := bsNone;
      Parent := Sheet;
      Align := alClient;
      Visible := True;
      ScaleForPPI(Sheet.CurrentPPI);
      ApplyEditorOptions;
      ApplyPyIDEOptions;
    end;
    if Result <> nil then
    begin
      fEditors.Add(Result);
    end;
  except
    Sheet.Free;
  end;
end;

function TEditorFactory.GetEditorCount: Integer;
begin
  Result := fEditors.Count;
end;

function TEditorFactory.GetViewFactory(Index: Integer): IEditorViewFactory;
begin
  fEditorViewFactories.Lock;
  try
    Result := fEditorViewFactories[Index] as IEditorViewFactory;
  finally
    fEditorViewFactories.UnLock;
  end;
end;

function TEditorFactory.GetViewFactoryCount: Integer;
begin
  Result := fEditorViewFactories.Count;
end;

procedure TEditorFactory.LockList;
begin
  fEditors.Lock;
end;

function TEditorFactory.GetEditorByName(const Name: string): IEditor;
Var
  FullName: string;
begin
  FullName := GetLongFileName(ExpandFileName(Name));
  Result := FirstEditorCond(function(Editor: IEditor): boolean
  begin
    Result := AnsiSameText(Editor.GetFileName, FullName);
  end);
end;

function TEditorFactory.GetEditorByFileId(const Name: string): IEditor;
begin
  Result := GetEditorByName(Name);
  if not Assigned(Result) then
    Result := FirstEditorCond(function(Editor: IEditor): Boolean
    begin
      Result := (Editor.FileName = '') and AnsiSameText(Editor.GetFileTitle, Name);
    end);
end;

function TEditorFactory.GetEditor(Index: Integer): IEditor;
begin
  Result := IEditor(fEditors[Index]);
end;

procedure TEditorFactory.RemoveEditor(AEditor: IEditor);
begin
  var Index := fEditors.IndexOf(AEditor);
  if Index >= 0 then
    fEditors.Delete(Index);
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
    Ed.OpenFile(RecoveredFile);

    var Editor := Ed as TEditor;
    Editor.fUntitledNumber := UntitledNumber;
    Editor.UntitledNumbers.Bits[UntitledNumber] := True;
    Editor.DoSetFileName('');
    Editor.fForm.DoUpdateCaption;
    if Editor.HasPythonFile then
      Editor.FSynLsp.FileSavedAs(Editor.GetFileId, lidPython)
  end;
  TDirectory.Delete(RecoveryDir, True);
end;

function TEditorFactory.RegisterViewFactory(ViewFactory: IEditorViewFactory): integer;
begin
  Result := fEditorViewFactories.Add(ViewFactory);
end;

procedure TEditorFactory.OnEditorViewClick(Sender: TObject);
Var
  ViewFactory: IEditorViewFactory;
  EditorView: IEditorView;
  Editor: IEditor;
  Index: Integer;
begin
  Editor := GI_PyIDEServices.ActiveEditor;
  if not Assigned(Editor) then
    Exit;
  Index := (Sender as TSpTBXItem).Tag;
  if (Index >= 0) and (Index < fEditorViewFactories.Count) then
  begin
    ViewFactory := fEditorViewFactories[Index] as IEditorViewFactory;
    EditorView := Editor.ActivateView(ViewFactory);
    if Assigned(EditorView) then
      EditorView.UpdateView(Editor);
  end;
end;

procedure TEditorFactory.SetupEditorViewsMenu(ViewsMenu: TSpTBXItem; IL: TCustomImageList);
Var
  MenuItem: TSpTBXItem;
  i: Integer;
  ViewFactory: IEditorViewFactory;
begin
  ViewsMenu.Clear;
  fEditorViewFactories.Lock;
  try
    ViewsMenu.Enabled := fEditorViewFactories.Count > 0;
    for i := 0 to fEditorViewFactories.Count - 1 do
    begin
      ViewFactory := fEditorViewFactories[i] as IEditorViewFactory;

      // Add MenuItem
      MenuItem := TSpTBXItem.Create(nil); // will be freed by the Parent Item
      MenuItem.Hint := ViewFactory.Hint;
      MenuItem.ImageIndex := IL.GetIndexByName(ViewFactory.ImageName);
      MenuItem.Caption := ViewFactory.MenuCaption;
      MenuItem.ShortCut := ViewFactory.ShortCut;
      MenuItem.OnClick := OnEditorViewClick;
      MenuItem.Tag := i;

      ViewsMenu.Add(MenuItem);
    end;
  finally
    fEditorViewFactories.UnLock;
  end;
end;

procedure TEditorFactory.UnlockList;
begin
  fEditors.UnLock;
end;

procedure TEditorFactory.UpdateEditorViewsMenu(ViewsMenu: TSpTBXItem);
Var
  i, j: Integer;
  Editor: IEditor;
  ViewFactory: IEditorViewFactory;
  List: TList;
  Enabled: boolean;
begin
  fEditorViewFactories.Lock;
  List := TList.Create;
  try
    for i := 0 to fEditorViewFactories.Count - 1 do
    begin
      Editor := GI_PyIDEServices.ActiveEditor;
      Enabled := Assigned(Editor);
      if Enabled then
      begin
        ViewFactory := fEditorViewFactories[i] as IEditorViewFactory;
        ViewFactory.GetContextHighlighters(List);
        if List.Count > 0 then
        begin
          Enabled := False;
          for j := 0 to List.Count - 1 do
          begin
            if List[j] = Editor.SynEdit.Highlighter then
            begin
              Enabled := True;
              break;
            end;
          end;
        end;
        List.Clear;
      end;
      ViewsMenu.Items[i].Enabled := Enabled;
    end;
  finally
    List.Free;
    fEditorViewFactories.UnLock;
  end;
end;

{ TEditorForm }

procedure TEditorForm.FormDestroy(Sender: TObject);
begin
  // PyIDEOptions change notification
  PyIDEOptions.OnChange.RemoveHandler(ApplyPyIDEOptions);

  SynEdit2.RemoveLinesPointer;

  if BreakPoints.Count > 0 then
  begin
    PyControl.BreakPointsChanged := True;
    BreakPointsWindow.UpdateWindow;
  end;
  BreakPoints.Free;

  // Unregister kernel notification
  ChangeNotifier.UnRegisterKernelChangeNotify(Self);

  SkinManager.RemoveSkinNotification(Self);
end;

procedure TEditorForm.SynEditChange(Sender: TObject);
begin
  if PyControl.ErrorPos.Editor = GetEditor then
    PyControl.ErrorPos := TEditorPos.EmptyPos;

  ClearSearchHighlight(FEditor);
end;

procedure TEditorForm.SynEditDblClick(Sender: TObject);
var
  ptMouse: TPoint;
  ASynEdit: TSynEdit;
begin
  ASynEdit := Sender as TSynEdit;
  GetCursorPos(ptMouse);
  ptMouse := ASynEdit.ScreenToClient(ptMouse);
  if (ptMouse.X >= ASynEdit.GutterWidth + 2)
    and ASynEdit.SelAvail and PyIDEOptions.HighlightSelectedWord
  then
    CommandsDataModule.HighlightWordInActiveEditor(ASynEdit.SelText);
end;

procedure TEditorForm.SynEditEnter(Sender: TObject);
Var
  ASynEdit: TSynEdit;
begin
  ASynEdit := Sender as TSynEdit;
  fActiveSynEdit := ASynEdit;
  fOldCaretY := ASynEdit.CaretY;
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

  if (fOldEditorForm <> Self) and not GI_PyIDEServices.IsClosing then
    CodeExplorerWindow.UpdateWindow(fEditor.FSynLsp.DocSymbols, ceuEditorEnter);
  fOldEditorForm := Self;

  // Search and Replace Target
  EditorSearchOptions.InterpreterIsSearchTarget := False;

  PyIDEMainForm.UpdateCaption;
end;

procedure TEditorForm.SynEditExit(Sender: TObject);
begin
  DoAssignInterfacePointer(False);

  if fHotIdentInfo.HaveHotIdent then
  begin
    fHotIdentInfo.HaveHotIdent := False;
    (Sender as TCustomSynEdit).Indicators.Clear(HotIdentIndicatorSpec, True,
      fHotIdentInfo.StartCoord.Line);
    SetCursor(TCustomSynEdit(Sender).Cursor);
  end;
end;

procedure TEditorForm.SynEditStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
Var
  ASynEdit: TSynEdit;
  NewCaretY: integer;
begin
  ASynEdit := Sender as TSynEdit;
  Assert(fEditor <> nil);
  if Changes * [scAll, scSelection] <> [] then
    fEditor.fHasSelection := ASynEdit.SelAvail;
  if scModified in Changes then
  begin
    PyIDEMainForm.UpdateCaption;
    ParentTabItem.Invalidate;
  end;
  if scCaretY in Changes then begin
    fNeedToSyncCodeExplorer := True;
    fCloseBracketChar := #0;
    fEditor.RefreshSymbols;
  end;
  if (scCaretY in Changes) and ASynEdit.Gutter.Visible
    and ASynEdit.Gutter.ShowLineNumbers
    and PyIDEOptions.CompactLineNumbers then
  begin
    NewCaretY := ASynEdit.CaretY;
    ASynEdit.InvalidateGutterLine(fOldCaretY);
    ASynEdit.InvalidateGutterLine(NewCaretY);
    fOldCaretY := NewCaretY;
  end;
  if scTopLine in Changes then
    Application.CancelHint;
end;

procedure TEditorForm.DoActivate;
begin
  ParentTabItem.Checked := True;
end;

procedure TEditorForm.DoActivateEditor(Primary: boolean = True);
Var
  ASynEdit: TSynEdit;
begin
  DoActivate;
  ViewsTabControl.ActiveTabIndex := 0;
  if Primary then
    ASynEdit := SynEdit
  else
    ASynEdit := fEditor.GetActiveSynEdit;
  if CanActuallyFocus(ASynEdit) then
    ASynEdit.SetFocus;
end;

function TEditorForm.DoActivateView(ViewFactory: IEditorViewFactory)
  : IEditorView;
var
  i: Integer;
  Form: TCustomForm;
  Tab: TSpTBXTabItem;
  TabSheet: TSpTBXTabSheet;
begin
  Result := nil;
  DoActivate;
  // Does the EditorView tab exist?
  Result := nil;
  for i := 0 to ViewsTabControl.PagesCount - 1 do
    if ViewsTabControl.Pages[i].Caption = ViewFactory.TabCaption then
    begin
      ViewsTabControl.ActiveTabIndex := i;
      Result := ViewsTabControl.Pages[i].Components[0] as IEditorView;
      break;
    end;
  if not Assigned(Result) then
  begin
    // Editor View does not exist - Create
    Tab := ViewsTabControl.Add(ViewFactory.TabCaption);
    TabSheet := ViewsTabControl.GetPage(Tab);
    try
      Form := ViewFactory.CreateForm(fEditor, TabSheet);
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

function TEditorForm.DoAskSaveChanges: boolean;
var
  S: string;
begin
  // this is necessary to prevent second confirmation when closing tabs
  if SynEdit.Modified then
  begin
    DoActivateEditor;
    MessageBeep(MB_ICONQUESTION);
    Assert(fEditor <> nil);
    S := Format(_(SAskSaveChanges), [TPath.GetFileName(fEditor.GetFileTitle)]);

    case StyledMessageDlg(S, mtConfirmation, [mbYes, mbNo, mbCancel], 0,
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

procedure TEditorForm.DoAssignInterfacePointer(AActive: boolean);
begin
  if AActive then
  begin
    GI_ActiveEditor := fEditor;
    GI_EditCmds := fEditor;
    GI_FileCmds := fEditor;
    GI_SearchCmds := fEditor;
  end
  else
  begin
    if GI_ActiveEditor = IEditor(fEditor) then
      GI_ActiveEditor := nil;
    if GI_EditCmds = IEditCommands(fEditor) then
      GI_EditCmds := nil;
    if GI_FileCmds = IFileCommands(fEditor) then
      GI_FileCmds := nil;
    if GI_SearchCmds = ISearchCommands(fEditor) then
      GI_SearchCmds := nil;
  end;
end;

function TEditorForm.DoSave: boolean;
begin
  Assert(fEditor <> nil);
  if (fEditor.fFileName <> '') or (fEditor.fRemoteFileName <> '') then
  begin
    Result := DoSaveFile;
    if Result then
      FEditor.FSynLsp.FileSaved;
  end
  else
    Result := DoSaveAs;
end;

function TEditorForm.DoSaveFile: boolean;
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
  if fEditor.fFileName <> '' then begin
    Result := SaveWideStringsToFile(fEditor.fFileName, SynEdit.Lines,
      PyIDEOptions.CreateBackupFiles);
    if Result then
      if not FileAge(fEditor.fFileName, FileTime) then
        FileTime := 0;
  end else if fEditor.fRemoteFileName <> '' then
     Result := fEditor.SaveToRemoteFile(fEditor.fRemoteFileName, fEditor.fSSHServer);
  if Result then
  begin
    if not PyIDEOptions.UndoAfterSave then
      SynEdit.ClearUndo;
    SynEdit.MarkSaved;
    SynEdit.Modified := False;
  end;
end;

function TEditorForm.DoSaveAs: boolean;
var
  NewName: string;
  Edit: IEditor;
begin
  Assert(fEditor <> nil);
  NewName := fEditor.GetFileId;
  if (fEditor.GetFileName = '') and (DefaultExtension <> '') and
    (ExtractFileExt(NewName) = '') then
    NewName := NewName + '.' + DefaultExtension;
  if ResourcesDataModule.GetSaveFileName(NewName, SynEdit.Highlighter,
    DefaultExtension) then
  begin
    Edit := GI_EditorFactory.GetEditorByName(NewName);
    if Assigned(Edit) and (Edit <> Self.fEditor as IEditor) then
    begin
      StyledMessageDlg(_(SFileAlreadyOpen), mtError, [mbAbort], 0);
      Result := False;
      Exit;
    end;
    fEditor.DoSetFileName(NewName);
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

function TEditorForm.DoSaveAsRemote: boolean;
Var
  FileName, Server : string;
  Edit : IEditor;
begin
  Assert(fEditor <> nil);
  if ExecuteRemoteFileDialog(FileName, Server, rfdSave) then
  begin
    Edit := GI_EditorFactory.GetEditorByName(TSSHFileName.Format(Server, FileName));
    if Assigned(Edit) and (Edit <> Self.fEditor as IEditor) then
    begin
      StyledMessageDlg(_(SFileAlreadyOpen), mtError, [mbAbort], 0);
      Result := False;
      Exit;
    end;
    fEditor.fRemoteFileName := FileName;
    fEditor.fSSHServer := Server;
    fEditor.DoSetFileName('');
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
Var
  TabCaption : string;
begin
  Assert(fEditor <> nil);
  if fEditor.fRemoteFileName <> '' then
    TabCaption := TPath.GetFileName(fEditor.fRemoteFileName)
  else
    TabCaption := fEditor.GetFileTitle;

  SynEdit.AccessibleName := TabCaption;
  SynEdit2.AccessibleName := TabCaption;

  with ParentTabItem do
  begin
    Caption := StringReplace(TabCaption, '&', '&&', [rfReplaceAll]);
    Hint := fEditor.GetFileId;
  end;
  PyIDEMainForm.UpdateCaption;
end;

procedure TEditorForm.DoUpdateHighlighter(HighlighterName: string = '');
begin
  Assert(fEditor <> nil);
  if HighlighterName <> '' then
    SynEdit.Highlighter :=
      ResourcesDataModule.Highlighters.HighlighterFromFriendlyName(HighlighterName)
  else if fEditor.fFileName <> '' then
    SynEdit.Highlighter :=
      ResourcesDataModule.Highlighters.HighlighterFromFileName(fEditor.fFileName)
  else if fEditor.fRemoteFileName <> '' then
    SynEdit.Highlighter :=
      ResourcesDataModule.Highlighters.HighlighterFromFileName(fEditor.fRemoteFileName)
  else // No highlighter otherwise
    SynEdit.Highlighter := nil;

  SynEdit2.Highlighter := SynEdit.Highlighter;

  // Set DefaultExtension
  if SynEdit.Highlighter <> nil then
    DefaultExtension := DefaultExtensionFromFilter(SynEdit.Highlighter.DefaultFilter)
  else
    DefaultExtension := '';
end;

procedure TEditorForm.EditorMouseWheel(theDirection: Integer;
  Shift: TShiftState);
Var
  ASynEdit: TSynEdit;

  function OwnScroll(Shift: TShiftState; LinesInWindow: Integer): Integer;
  begin
    if (ssShift in Shift) or (Mouse.WheelScrollLines = -1) then
      Result := LinesInWindow shr Ord(eoHalfPageScroll in ASynEdit.Options)
    else
      Result := Mouse.WheelScrollLines;
  end;

//
begin
  { *
    Manage Zoom in and out, Page up and down, Line scroll - with the Mouse Wheel
    * }
  if ssCtrl in Shift then
    EditorZoom(theDirection)
  else
  begin
    ASynEdit := fEditor.GetActiveSynEdit;

    ASynEdit.TopLine := ASynEdit.TopLine + (theDirection * OwnScroll(Shift,
        ASynEdit.LinesInWindow));
  end;
end;

procedure TEditorForm.EditorZoom(theZoom: Integer);
begin
  if (theZoom <= 1) and (SynEdit.Font.Size > 3) then
  begin
    SynEdit.Font.Size := Max(SynEdit.Font.Size - theZoom, 4);
    SynEdit.Gutter.Font.Size := Max(SynEdit.Font.Size - 2, 1);
    SynEdit2.Font.Size := SynEdit.Font.Size;
    SynEdit2.Gutter.Font.Size := SynEdit.Gutter.Font.Size;
  end;
end;

procedure TEditorForm.EditorCommandHandler(Sender: TObject;
  AfterProcessing: boolean; var Handled: boolean;
  var Command: TSynEditorCommand; var AChar: WideChar;
  Data, HandlerData: Pointer);
var
  ASynEdit: TSynEdit;
  iPrevLine: string;
  Position, Len: Integer;
  OpenBrackets, CloseBrackets: string;
  OpenBracketPos: Integer;
  Line: string;
  CharRight, CharLeft: WideChar;
  Attr: TSynHighlighterAttributes;
  DummyToken: string;
  Caret, BC: TBufferCoord;
begin
  ASynEdit := Sender as TSynEdit;
  if (Command <> ecLostFocus) and (Command <> ecGotFocus) then
    EditorSearchOptions.InitSearch;
  if Command <> ecChar then
    fCloseBracketChar := #0;

  if not AfterProcessing then
  begin
    case Command of
      ecCut:
        if not ASynEdit.SelAvail then
          with ASynEdit do
          begin
            // Cut the current line to the Clipboard
            BeginUpdate;
            try
              ExecuteCommand(ecLineStart, AChar, Data);
              ExecuteCommand(ecSelLineEnd, AChar, Data);
              ActiveSelectionMode := smLine;
              ExecuteCommand(ecCut, AChar, Data);
              ActiveSelectionMode := SelectionMode;
            finally
              EndUpdate;
            end;
            Handled := True;
          end;
      ecCopy:
        if not ASynEdit.SelAvail then
          with ASynEdit do
          begin
            // Copy the current line to the Clipboard
            BC := ASynEdit.CaretXY;
            BeginUpdate;
            try
              ExecuteCommand(ecLineStart, AChar, Data);
              ExecuteCommand(ecSelLineEnd, AChar, Data);
              ActiveSelectionMode := smLine;
              ExecuteCommand(ecCopy, AChar, Data);
              SetCaretAndSelection(BC, BC, BC);
              ActiveSelectionMode := SelectionMode;
            finally
              EndUpdate;
            end;
            Handled := True;
          end;
      ecCodeCompletion:
        if ASynEdit.Highlighter is TSynPythonSyn then
        begin
          if CommandsDataModule.SynCodeCompletion.Form.Visible then
            CommandsDataModule.SynCodeCompletion.CancelCompletion;
          DoCodeCompletion(ASynEdit, ASynEdit.CaretXY);
          Handled := True;
        end else if ASynEdit.Highlighter is TSynWebBase then
          CommandsDataModule.SynWebCompletion.ActivateCompletion;
      ecParamCompletion:
        if ASynEdit.Highlighter is TSynPythonSyn then
        begin
          if CommandsDataModule.SynParamCompletion.Form.Visible then
            CommandsDataModule.SynParamCompletion.CancelCompletion;
          CommandsDataModule.SynParamCompletion.ActivateCompletion;
          Handled := True;
        end;
      ecLeft: // Implement Visual Studio like behaviour when selection is available
        if ASynEdit.SelAvail then
          with ASynEdit do
          begin
            CaretXY := BlockBegin;
            Handled := True;
          end;
      ecRight: // Implement Visual Studio like behaviour when selection is available
        if ASynEdit.SelAvail then
          with ASynEdit do
          begin
            CaretXY := BlockEnd;
            Handled := True;
          end;
    end;
  end
  else
  begin // AfterProcessing
    case Command of
      ecLineBreak: // Python Mode
        if ASynEdit.InsertMode and (eoAutoIndent in ASynEdit.Options) and
          (ASynEdit.Highlighter is TSynPythonSyn)
          and not fAutoCompleteActive then
        begin
          { CaretY should never be lesser than 2 right after ecLineBreak, so there's
            no need for a check }
          iPrevLine := TrimRight(ASynEdit.Lines[ASynEdit.CaretY - 2]);

          //BC := BufferCoord(Length(iPrevLine), ASynEdit.CaretY - 1);
          // Indent on: if a: # success?
          //if ASynEdit.GetHighlighterAttriAtRowCol(BC, DummyToken, Attr) and not
          //  ( // (attr = ASynEdit.Highlighter.StringAttribute) or
          //  (Attr = ASynEdit.Highlighter.CommentAttribute) or
          //    (Attr = TSynPythonSyn(ASynEdit.Highlighter).CodeCommentAttri) { or
          //    (attr = ResourcesDataModule.SynPythonSyn.DocStringAttri) } ) then
          //begin
          if TPyRegExpr.IsBlockOpener(iPrevLine) then
            ASynEdit.ExecuteCommand(ecTab, #0, nil)
          else if TPyRegExpr.IsBlockCloser(iPrevLine) then
            ASynEdit.ExecuteCommand(ecShiftTab, #0, nil);
          //end;
        end;
      ecChar: // Autocomplete brackets
        begin
          if PyIDEOptions.EditorCodeCompletion then
          begin
            if (TIDECompletion.EditorCodeCompletion.CompletionInfo.Editor = nil)
              and (Pos(AChar, CommandsDataModule.SynCodeCompletion.TriggerChars) > 0)
            then
            begin
              Caret := ASynEdit.CaretXY;
              TThread.ForceQueue(nil, procedure
                begin
                  DoCodeCompletion(ASynEdit, Caret);
                end, IfThen(AChar = '.', 200,
                CommandsDataModule.SynCodeCompletion.TimerInterval));
            end;
          end;

          if not fAutoCompleteActive and PyIDEOptions.AutoCompleteBrackets then
            with ASynEdit do
            begin
              if ASynEdit.Highlighter is TSynPythonSyn then
              begin
                OpenBrackets := '([{"''';
                CloseBrackets := ')]}"''';
              end
              else if (ASynEdit.Highlighter = ResourcesDataModule.SynWebHTMLSyn)
                or (ASynEdit.Highlighter = ResourcesDataModule.SynWebXMLSyn) or
                (ASynEdit.Highlighter = ResourcesDataModule.SynWebCssSyn) then
              begin
                OpenBrackets := '<"''';
                CloseBrackets := '>"''';
              end
              else
                Exit;

              Line := LineText;
              Len := Length(LineText);

              if AChar = fCloseBracketChar then
              begin
                if InsertMode and (CaretX <= Len) and
                  (Line[CaretX] = fCloseBracketChar) then
                  ExecuteCommand(ecDeleteChar, WideChar(#0), nil);
                fCloseBracketChar := #0;
              end
              else if CharInSet(AChar, [')', ']', '}']) then
              begin
                fCloseBracketChar := #0;
                Position := CaretX;
                if Position <= Len then
                  CharRight := Line[Position]
                else
                  CharRight := WideNull;
                if (AChar = CharRight) and (GetMatchingBracket.Line <= 0) then
                  ExecuteCommand(ecDeleteChar, #0, nil);
              end
              else
              begin
                //fCloseBracketChar := #0;
                OpenBracketPos := Pos(AChar, OpenBrackets);

                BC := CaretXY;
                Dec(BC.Char, 2);
                if (BC.Char >= 1) and GetHighlighterAttriAtRowCol(BC, DummyToken,
                  Attr) and ((Attr = Highlighter.StringAttribute) or
                    (Attr = Highlighter.CommentAttribute) or
                    ((Highlighter is TSynPythonSyn) and
                      (Attr = TSynPythonSyn(Highlighter).CodeCommentAttri) or
                      (Attr = TSynPythonSyn(Highlighter).MultiLineStringAttri) or
                      (Attr = TSynPythonSyn(Highlighter).DocStringAttri))) then
                  OpenBracketPos := 0; // Do not auto complete brakets inside strings or comments

                if (OpenBracketPos > 0) then
                begin
                  CharRight := WideNull;
                  Position := CaretX;
                  while (Position <= Len) and Highlighter.IsWhiteChar
                    (LineText[Position]) do
                    Inc(Position);
                  if Position <= Len then
                    CharRight := Line[Position];

                  CharLeft := WideNull;
                  Position := CaretX - 2;
                  while (Position >= 1) and Highlighter.IsWhiteChar
                    (LineText[Position]) do
                    Dec(Position);
                  if Position >= 1 then
                    CharLeft := Line[Position];

                  if CharInSet(CharRight, [WideNull, ')', ']', '}', ',']) and not
                    (CharInSet(AChar, ['"', '''']) and
                      (Highlighter.IsIdentChar(CharLeft) or (CharLeft = AChar)))
                  then
                  begin
                    ExecuteCommand(ecChar, CloseBrackets[OpenBracketPos], nil);
                    CaretX := CaretX - 1;
                    if not CharInSet(AChar, [')', ']', '}']) then
                      fCloseBracketChar := CloseBrackets[OpenBracketPos];
                  end;
                end;
              end;
            end;
        end;
      ecSelWord:
        if ASynEdit.SelAvail and PyIDEOptions.HighlightSelectedWord then
          CommandsDataModule.HighlightWordInActiveEditor(ASynEdit.SelText);
      ecLostFocus:
        if not (CommandsDataModule.SynCodeCompletion.Form.Visible or SynEdit.Focused or SynEdit2.Focused) then
          CommandsDataModule.SynParamCompletion.CancelCompletion;
    end;
  end;
end;

class procedure TEditorForm.SymbolsChanged(Sender: TObject);
begin
  CodeExplorerWindow.UpdateWindow(Sender as TDocSymbols, ceuSymbolsChanged);
end;

procedure TEditorForm.SyncCodeExplorer;
begin
  if fNeedToSyncCodeExplorer and GetEditor.HasPythonFile then
  begin
    CodeExplorerWindow.ShowEditorCodeElement;
    fNeedToSyncCodeExplorer := False;
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

  fHotIdentInfo.HaveHotIdent := False;

  ViewsTabControl.TabVisible := False;
  case PyIDEOptions.EditorsTabPosition of
    ttpTop:
      ViewsTabControl.TabPosition := ttpBottom;
    ttpBottom:
      ViewsTabControl.TabPosition := ttpTop;
  end;

  SynEdit2.SetLinesPointer(SynEdit);

  //  Custom command handling
  SynEdit.RegisterCommandHandler(EditorCommandHandler, nil);
  SynEdit2.RegisterCommandHandler(EditorCommandHandler, nil);

  BreakPoints := TObjectList.Create(True);
  TDebugSupportPlugin.Create(Self); // No need to free

  // Indicators
  var IndicatorSpec :=
    TSynIndicatorSpec.New(sisTextDecoration, clNoneF, clNoneF, [fsUnderline]);
  SynEdit.Indicators.RegisterSpec(HotIdentIndicatorSpec, IndicatorSpec);
  SynEdit2.Indicators.RegisterSpec(HotIdentIndicatorSpec, IndicatorSpec);

  // PyIDEOptions change notification
  PyIDEOptions.OnChange.AddHandler(ApplyPyIDEOptions);

  // Register Kernel Notification
  ChangeNotifier.RegisterKernelChangeNotify(Self, [vkneFileName, vkneDirName,
    vkneLastWrite, vkneCreation]);

  SkinManager.AddSkinNotification(Self);

  PyIDEMainForm.ThemeEditorGutter(SynEdit.Gutter);

  TranslateComponent(Self);
end;

procedure TEditorForm.SynEditGutterGetText(Sender: TObject; aLine: Integer;
  var aText: string);
begin
  if aLine = TSynEdit(Sender).CaretY then
    Exit;

  if aLine mod 10 <> 0 then
    if aLine mod 5 <> 0 then
      aText := '·'
    else
      aText := '-';
end;

procedure TEditorForm.SynEditSpecialLineColors(Sender: TObject; Line: Integer;
  var Special: boolean; var FG, BG: TColor);
var
  LI: TDebuggerLineInfos;
begin
  if PyControl.ActiveDebugger <> nil then
  begin
    LI := PyControl.GetLineInfos(fEditor, Line);
    if dlCurrentLine in LI then
    begin
      Special := True;     { TODO : Allow customization of these colors }
      FG := clWhite;
      BG := clBlue;
    end
    else if (dlErrorLine in LI) then
    begin
      Special := True;
      FG := clWhite;
      BG := clRed
    end;
  end;
end;

function TEditorForm.GetEditor: IEditor;
begin
  Result := fEditor;
end;

procedure TEditorForm.GoToSyntaxError;
begin
  if HasSyntaxError then
  begin
    var List := FEditor.FSynLsp.Diagnostics;
    if List.Count > 0 then
      SynEdit.CaretXY := List[0].BlockBegin;
  end;
end;

function TEditorForm.HasSyntaxError: boolean;
begin
  Result := fEditor.HasPythonFile and (FEditor.FSynLsp.Diagnostics.Count > 0);
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
  if fHotIdentInfo.HaveHotIdent then
  begin
    fHotIdentInfo.HaveHotIdent := False;
    (Sender as TCustomSynEdit).Indicators.Clear(HotIdentIndicatorSpec, True,
      fHotIdentInfo.StartCoord.Line);
    SetCursor(TCustomSynEdit(Sender).Cursor);
  end;
end;

procedure TEditorForm.SynEditMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: boolean);
begin
  EditorMouseWheel(+1, Shift);
  Handled := True;
end;

procedure TEditorForm.SynEditMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: boolean);
begin
  EditorMouseWheel(-1, Shift);
  Handled := True;
end;

procedure TEditorForm.SynEditMouseCursor(Sender: TObject;
  const aLineCharPos: TBufferCoord; var aCursor: TCursor);
var
  TokenType, Start: Integer;
  Token: string;
  Attri: TSynHighlighterAttributes;
  OldHotIdent: Boolean;
  OldStartCoord: TBufferCoord;
  ASynEdit: TSynEdit;
begin
  ASynEdit := Sender as TSynEdit;
  OldHotIdent := fHotIdentInfo.HaveHotIdent;
  OldStartCoord := fHotIdentInfo.StartCoord;

  fHotIdentInfo.HaveHotIdent := False;
  if ASynEdit.Focused and (HiWord(GetAsyncKeyState(VK_CONTROL)) > 0)
    and fEditor.HasPythonFile and not ASynEdit.IsPointInSelection
    (aLineCharPos) then
    with ASynEdit do
    begin
      GetHighlighterAttriAtRowColEx(aLineCharPos, Token, TokenType, Start,
        Attri);
      if (Attri = TSynPythonSyn(Highlighter).IdentifierAttri) or
        (Attri = TSynPythonSyn(Highlighter).NonKeyAttri) or
        (Attri = TSynPythonSyn(Highlighter).SystemAttri) then
      begin
        aCursor := crHandPoint;
        with fHotIdentInfo do
        begin
          HaveHotIdent := True;
          StartCoord := BufferCoord(Start, aLineCharPos.Line);
        end;
      end;
    end;
    if (OldHotIdent <> fHotIdentInfo.HaveHotIdent) or
      (OldStartCoord <> fHotIdentInfo.StartCoord) then
    begin
      if OldHotIdent then
        ASynEdit.Indicators.Clear(HotIdentIndicatorSpec, True,
          OldStartCoord.Line);
      if fHotIdentInfo.HaveHotIdent then
        ASynEdit.Indicators.Add(fHotIdentInfo.StartCoord.Line,
          TSynIndicator.New(HotIdentIndicatorSpec,
            fHotIdentInfo.StartCoord.Char,
            fHotIdentInfo.StartCoord.Char + Token.Length));
    end;
end;

procedure TEditorForm.SynEditMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  EditorSearchOptions.InitSearch;
  if PyControl.ErrorPos.Editor = GetEditor then
    PyControl.ErrorPos := TEditorPos.EmptyPos;

  if fHotIdentInfo.HaveHotIdent then
  begin
    // fHodIdentInfo is reset in the KeyUp handler
    PostMessage(Application.MainForm.Handle, WM_FINDDEFINITION,
      fHotIdentInfo.StartCoord.Char, fHotIdentInfo.StartCoord.Line);
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

procedure TEditorForm.WMFolderChangeNotify(var Msg: TMessage);
begin
    TThread.ForceQueue(nil, procedure
  begin
    CommandsDataModule.ProcessFolderChange(TPath.GetDirectoryName(fEditor.fFileName));
  end, 200);
end;

procedure TEditorForm.AddWatchAtCursor;
var
  Token, LineTxt, DottedIdent: string;
  Attri: TSynHighlighterAttributes;
  aLineCharPos: TBufferCoord;
begin
  aLineCharPos := SynEdit.CaretXY;
  if SynEdit.Highlighter = ResourcesDataModule.SynPythonSyn then
    with SynEdit do
    begin
      GetHighlighterAttriAtRowCol(aLineCharPos, Token, Attri);
      if (Attri = ResourcesDataModule.SynPythonSyn.IdentifierAttri) or
        (Attri = ResourcesDataModule.SynPythonSyn.NonKeyAttri) or
        (Attri = ResourcesDataModule.SynPythonSyn.SystemAttri) or
        ((Token = ')') or (Token = ']')) then
      begin
        LineTxt := Lines[aLineCharPos.Line - 1];
        DottedIdent := GetWordAtPos(LineTxt, aLineCharPos.Char,
          True, True, False, True);
        DottedIdent := DottedIdent + GetWordAtPos(LineTxt,
          aLineCharPos.Char + 1, False, False, True);
        if DottedIdent <> '' then
          WatchesWindow.AddWatch(DottedIdent);
      end;
    end;
end;

procedure TEditorForm.mnUpdateViewClick(Sender: TObject);
var
  TabCaption: string;
  i: Integer;
  ViewFactory: IEditorViewFactory;
  EditorView: IEditorView;
begin
  if ViewsTabControl.ActivePage <> tbshSource then
  begin
    TabCaption := ViewsTabControl.ActivePage.Caption;
    ViewFactory := nil;
    for i := 0 to GI_EditorFactory.ViewFactoryCount - 1 do
    begin
      if GI_EditorFactory.ViewFactory[i].TabCaption = TabCaption then
      begin
        ViewFactory := GI_EditorFactory.ViewFactory[i];
        break;
      end;
    end;
    if Assigned(ViewFactory) then
    begin
      EditorView := fEditor.ActivateView(ViewFactory);
      if Assigned(EditorView) then
        EditorView.UpdateView(fEditor);
    end;
  end;
end;

procedure TEditorForm.ApplyEditorOptions;
begin
  SynEdit.Assign(EditorOptions);
  SynEdit2.Assign(EditorOptions);

  SynEdit.BracketsHighlight.SetFontColorsAndStyle(
    ResourcesDataModule.SynPythonSyn.MatchingBraceAttri.Foreground,
    ResourcesDataModule.SynPythonSyn.UnbalancedBraceAttri.Foreground, [fsBold]);
  SynEdit2.BracketsHighlight.SetFontColorsAndStyle(
    ResourcesDataModule.SynPythonSyn.MatchingBraceAttri.Foreground,
    ResourcesDataModule.SynPythonSyn.UnbalancedBraceAttri.Foreground, [fsBold]);
end;

procedure TEditorForm.ApplyPyIDEOptions;
begin
  Synedit.CodeFolding.Assign(PyIDEOptions.CodeFolding);
  Synedit2.CodeFolding.Assign(PyIDEOptions.CodeFolding);

  Synedit.SelectedColor.Assign(PyIDEOptions.SelectionColor);
  Synedit2.SelectedColor.Assign(PyIDEOptions.SelectionColor);

  Synedit.IndentGuides.Assign(PyIDEOptions.IndentGuides);
  Synedit2.IndentGuides.Assign(PyIDEOptions.IndentGuides);

  SynEdit.Gutter.TrackChanges.Assign(PyIDEOptions.TrackChanges);
  SynEdit2.Gutter.TrackChanges.Assign(PyIDEOptions.TrackChanges);

  RegisterSearchHighlightIndicatorSpec(fEditor);

  if PyIDEOptions.CompactLineNumbers then
  begin
    SynEdit.OnGutterGetText := SynEditGutterGetText;
    SynEdit2.OnGutterGetText := SynEditGutterGetText;
  end
  else
  begin
    SynEdit.OnGutterGetText := nil;
    SynEdit2.OnGutterGetText := nil;
  end;
  SynEdit.InvalidateGutter;
  SynEdit2.InvalidateGutter;

  // Tab position
  if PyIDEOptions.EditorsTabPosition = ttpTop then
    ViewsTabControl.TabPosition := ttpBottom
  else  //ttpBottom:
    ViewsTabControl.TabPosition := ttpTop;
end;

procedure TEditorForm.AutoCompleteAfterExecute(Sender: TObject);
begin
  fAutoCompleteActive := False;
  CommandsDataModule.actReplaceParametersExecute(nil);
end;

procedure TEditorForm.AutoCompleteBeforeExecute(Sender: TObject);
begin
  fAutoCompleteActive := True;
end;

procedure TEditorForm.WMSpSkinChange(var Message: TMessage);
begin
  StyledBorderColors(BorderNormal, BorderHighlight);

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
  Editor := fActiveSynEdit;
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
  TIDECompletion.EditorCodeCompletion.CleanUp;
end;

procedure TEditorForm.SynCodeCompletionCodeItemInfo(Sender: TObject;
  AIndex: Integer; var Info: string);
begin
  var CC := TIDECompletion.EditorCodeCompletion;
  if not CC.Lock.TryEnter then Exit;
  try
    if Assigned(CC.CompletionInfo.CompletionHandler) then
      Info := CC.CompletionInfo.CompletionHandler.GetInfo(
        (Sender as TSynCompletionProposal).InsertList[AIndex]);
  finally
    CC.Lock.Leave;
  end;
end;

class procedure TEditorForm.DoCodeCompletion(Editor: TSynEdit; Caret: TBufferCoord);
var
  locline: string;
  Attr: TSynHighlighterAttributes;
  Highlighter: TSynCustomHighlighter;
  FileName, DummyToken: string;
begin
  //Exit if cursor has moved
  if not Assigned(GI_ActiveEditor) or (GI_ActiveEditor.ActiveSynEdit <> Editor)
    or (Editor.ReadOnly) or (Caret <> Editor.CaretXY)
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
  locline := Editor.LineText.PadRight(Caret.Char);
  Inc(Caret.Char);

  var CC := TIDECompletion.EditorCodeCompletion;
  if not CC.Lock.TryEnter then Exit;
  try
    // Exit if busy
    if CC.CompletionInfo.Editor <> nil then Exit;
    CC.CleanUp;
    CC.CompletionInfo.Editor := Editor;
    CC.CompletionInfo.CaretXY := Caret;
  finally
    CC.Lock.Leave;
  end;

  TTask.Create(procedure
  var
    DisplayText, InsertText: string;
  begin
    var CC := TIDECompletion.EditorCodeCompletion;
    if not CC.Lock.TryEnter then Exit;
    try
      var Skipped := False;
      for var I := 0 to CC.SkipHandlers.Count -1 do
      begin
        var SkipHandler := CC.SkipHandlers[I] as TBaseCodeCompletionSkipHandler;
        Skipped := SkipHandler.SkipCodeCompletion(locline, FileName, Caret, Highlighter, Attr);
        if Skipped then Break;
      end;

      var Handled := False;
      if not Skipped then
      begin
        for var I := 0 to CC.CompletionHandlers.Count - 1 do
        begin
          var CompletionHandler := CC.CompletionHandlers[I] as TBaseCodeCompletionHandler;
          CompletionHandler.Initialize;
          try
            Handled := CompletionHandler.HandleCodeCompletion(locline, FileName,
              Caret, Highlighter, Attr, InsertText, DisplayText);
          except
          end;
          if Handled then begin
            //CompletionHandler will be finalized in the Cleanup call
            CC.CompletionInfo.CompletionHandler := CompletionHandler;
            CC.CompletionInfo.InsertText := InsertText;
            CC.CompletionInfo.DisplayText := DisplayText;
            Break;
          end
          else
            CompletionHandler.Finalize;
        end;
      end;

      if not Skipped and Handled and (InsertText <> '') then
        TThread.Queue(nil, procedure
        begin
          if Assigned(GI_ActiveEditor) and (GI_ActiveEditor.FileId = FileName) and
            (CommandsDataModule.SynCodeCompletion.Editor = GI_ActiveEditor.ActiveSynEdit)
          then
            CommandsDataModule.SynCodeCompletion.ActivateCompletion;
        end)
      else
        CC.CleanUp;
    finally
      CC.Lock.Leave;
    end;
  end).Start;
end;

procedure TEditorForm.SynCodeCompletionExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: string; var X, Y: Integer;
  var CanExecute: boolean);
begin
  var CC := TIDECompletion.EditorCodeCompletion;
  var CP := TSynCompletionProposal(Sender);

  CanExecute := False;
  if CC.Lock.TryEnter then
  try
    CanExecute := Assigned(GI_ActiveEditor) and
      (GI_ActiveEditor.ActiveSynEdit = CC.CompletionInfo.Editor) and
      Application.Active and
      (GetParentForm(CC.CompletionInfo.Editor).ActiveControl = CC.CompletionInfo.Editor) and
      (CC.CompletionInfo.CaretXY = CC.CompletionInfo.Editor.CaretXY);

    if CanExecute then
    begin
      CP.Font := PyIDEOptions.AutoCompletionFont;
      CP.ItemList.Text := CC.CompletionInfo.DisplayText;
      CP.InsertList.Text := CC.CompletionInfo.InsertText;
      CP.NbLinesInWindow := PyIDEOptions.CodeCompletionListSize;
      CP.CurrentString := CurrentInput;

      if CP.Form.AssignedList.Count = 0 then
      begin
        CanExecute := False;
        CC.CleanUp;
      end
      else
      if PyIDEOptions.CompleteWithOneEntry and (CP.Form.AssignedList.Count = 1) then
      begin
        // Auto-complete with one entry without showing the form
        CanExecute := False;
        CP.OnValidate(CP.Form, [], #0);
        CC.CleanUp;
      end;
    end else begin
      CP.ItemList.Clear;
      CP.InsertList.Clear;
      CC.CleanUp;
    end;
  finally
    CC.Lock.Leave;
  end;
end;

class procedure TEditorForm.SynParamCompletionExecute(Kind: SynCompletionType;
    Sender: TObject; var CurrentInput: string; var X, Y: Integer; var
    CanExecute: boolean);
var
  P : TPoint;
  CP: TSynCompletionProposal;
  Editor: IEditor;
begin
   Editor := GI_ActiveEditor;
   CP := Sender as TSynCompletionProposal;

  TJedi.ParamCompletionInfo.Lock;
  try
    CanExecute := Assigned(Editor) and Editor.HasPythonFile and TJedi.Ready
      and (Editor.ActiveSynEdit = CP.Editor) and PyIDEOptions.EditorCodeCompletion;

    // This function is called
    // a) from the editor using trigger char or editor command
    // b) From TSynCompletionProposal.HookEditorCommand
    // c) for TJedi ParamCompletionHandler Only then RequestId <> 0

    if not TJedi.ParamCompletionInfo.Handled then
    begin
      TJedi.RequestParamCompletion(Editor.FileId, CP.Editor);

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
    CanExecute := CanExecute and TJedi.ParamCompletionInfo.Succeeded and
      (TJedi.ParamCompletionInfo.FileId = Editor.FileId) and
      (TJedi.ParamCompletionInfo.CurrentLine = CP.Editor.LineText) and
      (TJedi.ParamCompletionInfo.Caret = CP.Editor.CaretXY);

    if CanExecute then
    begin
      var DisplayString := TJedi.ParamCompletionInfo.DisplayString;
      CP.FormatParams := not (DisplayString = '');
      if not CP.FormatParams then
        DisplayString :=  '\style{~B}' + _(SNoParameters) + '\style{~B}';

      var DocString := TJedi.ParamCompletionInfo.DocString;
      if (DocString <> '') then
      begin
        DisplayString := DisplayString + sLineBreak;
        DocString := GetLineRange(DocString, 1, 20) // 20 lines max
      end;

      CP.Form.CurrentIndex := TJedi.ParamCompletionInfo.ActiveParameter;
      CP.ItemList.Text := DisplayString + DocString;

      // position the hint window at and just below the opening bracket
      P := CP.Editor.ClientToScreen(CP.Editor.RowColumnToPixels(
          CP.Editor.BufferToDisplayPos(
          BufferCoord(TJedi.ParamCompletionInfo.StartX, CP.Editor.CaretY))));
      Inc(P.Y, CP.Editor.LineHeight);
      X := P.X;
      Y := P.Y;
    end
    else
      CP.ItemList.Clear;

    // Mark request as not handled even if you cannot execute
    // It will be marked again as handled by the asynchronous request handler
    TJedi.ParamCompletionInfo.Handled := False;

  finally
    TJedi.ParamCompletionInfo.UnLock;
  end;
end;

procedure TEditorForm.SynWebCompletionAfterCodeCompletion(Sender: TObject;
  const Value: string; Shift: TShiftState; Index: Integer;
  EndToken: WideChar);
Var
    SynEdit: TCustomSynEdit;

  function CaretBetween(AStr: string): boolean;
  var
    i: Integer;
  begin
    i := Pos(AStr, Value);
    Result := i > 0;
    if Result then
      SynEdit.CaretX := SynEdit.CaretX - (Length(Value) - i);
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
  var CanExecute: boolean);
Var
  SynEdit: TCustomSynEdit;
begin
  SynEdit := TSynCompletionProposal(Sender).Editor;
  SynWebFillCompletionProposal(SynEdit, ResourcesDataModule.SynWebHTMLSyn,
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
  MousePos: TPoint; var Handled: boolean);
Var
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
  if Assigned(Editor) then
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
  if FEditor.HasPythonFile then
    FEditor.FSynLsp.ApplyNewDiagnostics;

  if SynEdit.ReadOnly then
    ParentTabItem.ImageIndex := PyIDEMainForm.vilTabDecorators.GetIndexByName('Lock')
  else if HasSyntaxError then
    ParentTabItem.ImageIndex := PyIDEMainForm.vilTabDecorators.GetIndexByName('Bug')
  else
    ParentTabItem.ImageIndex := -1;
end;

procedure TEditorForm.SynEditDebugInfoPaintLines(RT: ID2D1RenderTarget; ClipR:
    TRect; const FirstRow, LastRow: Integer; var DoDefaultPainting: Boolean);
var
  LH, Y: Integer;
  LI: TDebuggerLineInfos;
  ImgIndex: Integer;
  Row, Line: Integer;
begin
  DoDefaultPainting := False;
  if not (SynEdit.Highlighter = ResourcesDataModule.SynPythonSyn) then Exit;

  if (PyControl.ActiveDebugger <> nil) and SynEdit.Gutter.Visible then
  begin
    LH := SynEdit.LineHeight;

    for Row := FirstRow to LastRow do
    begin
      Line := SynEdit.RowToLine(Row);
      if Row <> SynEdit.LineToRow(Line) then Continue;  //Wrapped line

      Y := (LH - vilGutterGlyphs.Height) div 2 + LH *
        (Row - SynEdit.TopLine);
      LI := PyControl.GetLineInfos(fEditor, Line);
      if dlCurrentLine in LI then
      begin
        if dlBreakpointLine in LI then
          ImgIndex := 2
        else
          ImgIndex := 1;
      end
      else if dlExecutableLine in LI then
      begin
        if dlBreakpointLine in LI then
          ImgIndex := 3
        else if dlDisabledBreakpointLine in LI then
          ImgIndex := 5
        else if PyIDEOptions.MarkExecutableLines then
          ImgIndex := 0
        else
          ImgIndex := -1
      end
      else
      begin
        if dlBreakpointLine in LI then
          ImgIndex := 4
        else if dlDisabledBreakpointLine in LI then
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
Var
  ASynEdit: TSynEdit;
begin
  ASynEdit := Sender as TSynEdit;
  if (ASynEdit.Highlighter = ResourcesDataModule.SynPythonSyn) and
    (PyControl.ActiveDebugger <> nil)
  then
    PyControl.ToggleBreakpoint(fEditor, Line, GetKeyState(VK_CONTROL) < 0);
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
  var Highlighter := TSynPythonSyn(SynEd.Highlighter);

  var DC := SynEd.PixelsToNearestRowColumn(HintInfo.CursorPos.X, HintInfo.CursorPos.Y);
  var BC := SynEd.DisplayToBufferPos(DC);

  // Diagnostic errors hints first
  if (FEditor.FSynLsp.Diagnostics.Count > 0) and
    SynEd.Indicators.IndicatorAtPos(BC,
    FEditor.FSynLsp.DiagnosticsErrorIndicatorSpec, Indicator)
  then
  begin
    CanShow := True;
    BC1 := BufferCoord(Indicator.CharStart, BC.Line);
    BC2 := BufferCoord(Indicator.CharEnd, BC.Line);
    // Setting HintInfo.CursorRect is important.  Otherwise no other hint
    // will be shown unlessmouse leaves and reenters the control
    HintInfo.CursorRect := CursorRect(SynEd, BC1, BC2, HintInfo.HintPos);
    HintStr := FEditor.FSynLsp.Diagnostics[Indicator.Tag].Msg;
  end
  else if fEditor.HasPythonFile and not SynEd.IsPointInSelection(BC) and
    SynEd.GetHighlighterAttriAtRowColEx(BC, Token, TokenType, Start, Attri) and
    (((PyControl.DebuggerState in [dsPaused, dsPostMortem]) and
       PyIDEOptions.ShowDebuggerHints) or
       (GI_PyControl.Inactive and PyIDEOptions.ShowCodeHints)) and
    ((Attri = Highlighter.IdentifierAttri) or
     (Attri = Highlighter.NonKeyAttri) or
     (Attri = Highlighter.SystemAttri) or
      // bracketed debugger expression
     ((Attri = Highlighter.SymbolAttri) and
      (PyControl.DebuggerState in [dsPaused, dsPostMortem]) and
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
          function : string
          begin
            Result := TJedi.CodeHintAtCoordinates(FEditor.GetFileId,
              BC1, Token);
          end, TThreadPool.Default).Start;
      end
      else
      begin
        // Debugger hints
        FHintFuture := TFuture<string>.Create(nil, nil,
          function : string
          var
            ObjectValue, ObjectType: string;
          begin
            PyControl.ActiveDebugger.Evaluate(DottedIdent, ObjectType,
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

initialization
  GI_EditorFactory := TEditorFactory.Create;
  TCodeHintWindow.OnHyperLinkClick := TEditorForm.CodeHintLinkHandler;

finalization
  GI_EditorFactory := nil;
end.
