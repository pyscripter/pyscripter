{ -----------------------------------------------------------------------------
  Unit Name: frmEditor
  Author:    Kiriakos Vlahos
  Date:      23-Feb-2005
  Purpose:
  History:   Origianlly Based on SynEdit Demo
  ----------------------------------------------------------------------------- }

unit frmEditor;
{$I SynEdit.inc}

interface

uses
  Types, System.UITypes, Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Contnrs, Forms,
  StdCtrls, uEditAppIntfs, JclStrings, SynEdit, SynEditTypes,
  SynEditHighlighter, SynEditMiscClasses,
  SynEditKeyCmds, ImgList, Dialogs, ExtCtrls,
  TB2Item, uCommonFunctions,
  SynCompletionProposal, cPyBaseDebugger, SpTBXItem,
  VirtualResources, SpTBXSkins, SpTBXDkPanels, Menus, SpTBXTabs, SynRegExpr,
  cPythonSourceScanner, frmCodeExplorer, cCodeCompletion, SpTBXControls,
  System.ImageList;

const
  WM_PARAMCOMPLETION = WM_USER +1040;

type
  TEditor = class;

  THotIdentInfo = record
    SynEdit: TSynEdit;
    HaveHotIdent: boolean;
    IdentArea: TRect;
    Ident: string;
    DottedIdent: string;
    StartCoord: TBufferCoord;
    SynToken: string;
    SynAttri: TSynHighlighterAttributes;
  end;

  TCodeCompletionType = (cctNone, cctModule, cctFrom, cctNameSpace);

  TEditorForm = class(TForm)
    imglGutterGlyphs: TImageList;
    pmnuEditor: TSpTBXPopupMenu;
    pmnuViewsTab: TSpTBXPopupMenu;
    mnCloseTab: TSpTBXItem;
    FGPanel: TPanel;
    SynEdit: TSynEdit;
    SynCodeCompletion: TSynCompletionProposal;
    SynParamCompletion: TSynCompletionProposal;
    SynEdit2: TSynEdit;
    EditorSplitter: TSpTBXSplitter;
    SynWebCompletion: TSynCompletionProposal;
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
    BGPanel: TSpTBXPanel;
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
    procedure SynEditMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure SynParamCompletionExecute(Kind: SynCompletionType;
      Sender: TObject; var CurrentInput: string; var X, Y: Integer;
      var CanExecute: boolean);
    procedure FormDestroy(Sender: TObject);
    procedure SynEditChange(Sender: TObject);
    procedure SynEditEnter(Sender: TObject);
    procedure SynEditExit(Sender: TObject);
    procedure SynEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure FormCreate(Sender: TObject);
    procedure SynEditGutterClick(Sender: TObject; Button: TMouseButton;
      X, Y, Line: Integer; Mark: TSynEditMark);
    procedure SynEditSpecialLineColors(Sender: TObject; Line: Integer;
      var Special: boolean; var FG, BG: TColor);
    procedure SynEditPaintTransient(Sender: TObject; Canvas: TCanvas;
      TransientType: TTransientType);
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
    procedure EditorViewsChange(Sender: TObject);
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
  private
    fEditor: TEditor;
    fActiveSynEdit: TSynEdit;
    fAutoCompleteActive: boolean;
    fHotIdentInfo: THotIdentInfo;
    fHintIdentInfo: THotIdentInfo;
    fNeedToCheckSyntax: boolean;
    fNeedToParseModule: boolean;
    fNeedToSyncCodeExplorer: boolean;
    fSyntaxErrorPos: TEditorPos;
    fCloseBracketChar: WideChar;
    fCompletionHandler : TBaseCodeCompletionHandler;
    fOldCaretY : Integer;
    procedure CleanupCodeCompletion;
    function DoAskSaveChanges: boolean;
    procedure DoAssignInterfacePointer(AActive: boolean);
    function DoSave: boolean;
    function DoSaveFile: boolean;
    function DoSaveAs: boolean;
    procedure DoUpdateCaption;
    procedure DoUpdateHighlighter(HighlighterName: string = '');
    procedure AutoCompleteBeforeExecute(Sender: TObject);
    procedure AutoCompleteAfterExecute(Sender: TObject);
    procedure WMShellNotify(var Msg: TMessage); message WM_SHELLNOTIFY;
    class var fOldEditorForm: TEditorForm;
    procedure SynCodeCompletionCodeItemInfo(Sender: TObject;
      AIndex: Integer; var Info : string);
  protected
    procedure Retranslate;
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
    procedure EditorZoom(theZoom: Integer);
    procedure EditorMouseWheel(theDirection: Integer; Shift: TShiftState);
    procedure CodeHintEventHandler(Sender: TObject; AArea: TRect;
      var CodeHint: string);
    procedure CodeHintLinkHandler(Sender: TObject; LinkName: string);
    procedure WMPARAMCOMPLETION(var Message: TMessage); message WM_PARAMCOMPLETION;
  public
    BreakPoints: TObjectList;
    FoundSearchItems: TObjectList;
    HasFocus: boolean;
    FileTime: TDateTime;
    DefaultExtension: string;
    ParentTabItem: TSpTBXTabItem;
    ParentTabControl: TSpTBXCustomTabControl;
    SourceScanner: IAsyncSourceScanner;
    procedure DoActivate;
    procedure DoActivateEditor(Primary: boolean = True);
    function DoActivateView(ViewFactory: IEditorViewFactory): IEditorView;
    function GetEditor: IEditor;
    procedure EditorCommandHandler(Sender: TObject; AfterProcessing: boolean;
      var Handled: boolean; var Command: TSynEditorCommand;
      var AChar: WideChar; Data: Pointer; HandlerData: Pointer);
    procedure PaintGutterGlyphs(ACanvas: TCanvas; AClip: TRect;
      FirstLine, LastLine: Integer);
    procedure DoOnIdle;
    function ReparseIfNeeded: boolean;
    procedure SyncCodeExplorer;
    procedure AddWatchAtCursor;
    procedure SetUpCodeHints;
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
    function GetFileNameOrTitle: string;
    function GetModified: boolean;
    function GetFileEncoding: TFileSaveFormat;
    procedure SetFileEncoding(FileEncoding: TFileSaveFormat);
    function GetEncodedText: AnsiString;
    procedure OpenFile(const AFileName: string; HighlighterName: string = '');
    function HasPythonFile: boolean;
    procedure ExecuteSelection;
    procedure SplitEditorHorizontally;
    procedure SplitEditorVertrically;
    procedure Retranslate;
    function GetForm: TForm;
    function GetSourceScanner: IAsyncSourceScanner;
    function GetCodeExplorerData: ICodeExplorerData;
    function GetTabControlIndex: Integer;
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
    fForm: TEditorForm;
    fHasSelection: boolean;
    fIsReadOnly: boolean;
    fUntitledNumber: Integer;
    fFileEncoding: TFileSaveFormat;
    fCodeExplorerData: ICodeExplorerData;
    function IsEmpty: boolean;
    constructor Create(AForm: TEditorForm);
    procedure DoSetFileName(AFileName: string);
    function GetEncodedTextEx(var EncodedText: AnsiString;
      InformationLossWarning: boolean): boolean;
  end;

implementation

{$R *.DFM}

uses
  frmPyIDEMain, dlgSynPrintPreview,
  frmBreakPoints, Variants, dmCommands,
  StringResources, VarPyth, cRefactoring,
  cCodeHint, frmPythonII, Math,
  frmWatches, PythonEngine,
  SynEditTextBuffer, cPyDebugger, dlgPickList, JvDockControlForm,
  uSearchHighlighter, VirtualShellNotifier,
  SynHighlighterWebMisc, SynHighlighterWeb, JvGnugettext,
  SynUnicode, frmIDEDockWin, StrUtils, SynHighlighterPython, Vcl.Themes,
  cPyScripterSettings;

const
  WM_DELETETHIS = WM_USER + 42;

  { TGutterMarkDrawPlugin }

type
  TDebugSupportPlugin = class(TSynEditPlugin)
  protected
    fForm: TEditorForm;
    procedure AfterPaint(ACanvas: TCanvas; const AClip: TRect;
      FirstLine, LastLine: Integer); override;
    procedure LinesInserted(FirstLine, Count: Integer); override;
    procedure LinesDeleted(FirstLine, Count: Integer); override;
  public
    constructor Create(AForm: TEditorForm);
  end;

constructor TDebugSupportPlugin.Create(AForm: TEditorForm);
begin
  inherited Create(AForm.SynEdit);
  fForm := AForm;
end;

Type
  TUnderlineStyle = (usCorelWordPerfect, usMicrosoftWord);

procedure TDebugSupportPlugin.AfterPaint(ACanvas: TCanvas; const AClip: TRect;
  FirstLine, LastLine: Integer);
Var
  TP: TPoint;
  MaxX, LH: Integer;

  procedure PaintUnderLine;
  Const
    UnderlineStyle: TUnderlineStyle = usMicrosoftWord;
  var
    NewPoint, NewY: Integer;

    procedure DrawPoint;
    begin
      // Do not draw on gutter.
      // This happens when a word is underlined and part of it is "hidden" under
      // the gutter.
      if TP.X <= Editor.Gutter.RealGutterWidth(Editor.CharWidth) then
        Exit;
      with ACanvas do
      begin
        if NewY = TP.Y - 1 then
          Pen.Color := fForm.SynEdit.Color
        else
          Pen.Color := clRed;
        Pixels[TP.X, NewY] := Pen.Color;
      end;
    end;

  const
    // Microsoft Word style
    // MW_POINTS: array[0..6] of ShortInt = (1, 2, 2, 1, 0, 0, 0);
    MW_POINTS: array [0 .. 3] of ShortInt = (0, 1, 2, 1);
    // Corel Word Perfect style
    // WP_POINTS: array[0..4] of ShortInt = (3, 2, 1, -1, -1);
    WP_POINTS: array [0 .. 3] of ShortInt = (2, 1, 0, -1);

  begin
    Inc(TP.Y, LH - 3);
    NewPoint := 0;
    if UnderlineStyle = usMicrosoftWord then
      NewY := TP.Y + MW_POINTS[NewPoint]
    else
      NewY := TP.Y + WP_POINTS[NewPoint];
    DrawPoint;
    while TP.X <= MaxX do
    begin
      DrawPoint;
      Inc(NewPoint);
      if UnderlineStyle = usMicrosoftWord then
      begin
        if NewPoint > High(MW_POINTS) then
          NewPoint := 0
      end
      else
      begin
        if NewPoint > High(WP_POINTS) then
          NewPoint := 0;
      end;
      DrawPoint;
      Inc(TP.X);
      if UnderlineStyle = usMicrosoftWord then
        NewY := TP.Y + MW_POINTS[NewPoint]
      else
        NewY := TP.Y + WP_POINTS[NewPoint];
    end;
  end;

begin
  if fForm.HasSyntaxError then
    with fForm.fSyntaxErrorPos do
      if Math.InRange(Line, FirstLine, LastLine)
        and not (fForm.SynEdit.UseCodeFolding and
          fForm.SynEdit.AllFoldRanges.FoldHidesLine(Line)) then
      begin
        LH := fForm.SynEdit.LineHeight;
        TP := fForm.SynEdit.RowColumnToPixels
          (fForm.SynEdit.BufferToDisplayPos(BufferCoord(1, Line)));
        if TP.X <= ACanvas.ClipRect.Right - ACanvas.ClipRect.Left then
        begin
          MaxX := fForm.SynEdit.RowColumnToPixels
            (fForm.SynEdit.BufferToDisplayPos(BufferCoord(Char, Line))).X;
          PaintUnderLine;
        end;
      end;

  if fForm.SynEdit.Highlighter = CommandsDataModule.SynPythonSyn then
    fForm.PaintGutterGlyphs(ACanvas, AClip, FirstLine, LastLine);
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
  fForm := AForm;
  fUntitledNumber := -1;
  fFileEncoding := sf_Ansi;
  fCodeExplorerData := TCodeExplorerData.Create;
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
    CodeExplorerWindow.UpdateWindow(ceuExit);

    if (fFileName <> '') and (CommandsDataModule <> nil) then
      PyIDEMainForm.tbiRecentFileList.MRUAdd(fFileName);
    if fUntitledNumber <> -1 then
      CommandsDataModule.ReleaseUntitledNumber(fUntitledNumber);

    if fForm.BreakPoints.Count > 0 then
    begin
      PyControl.BreakPointsChanged := True;
      BreakPointsWindow.UpdateWindow;
    end;

    TabSheet := (fForm.Parent as TSpTBXTabSheet);
    TabControl := TabSheet.TabControl;
    (fForm.ParentTabControl as TSpTBXTabControl).zOrder.Remove(TabSheet.Item);
    fForm.DoAssignInterfacePointer(False);
    // fForm.Close;
    TabSheet.Free;
    if Assigned(TabControl) then
      TabControl.Toolbar.MakeVisible(TabControl.ActiveTab);
  end;
end;

procedure TEditor.DoSetFileName(AFileName: string);
begin
  if AFileName <> fFileName then
  begin
    fFileName := AFileName;
    if fUntitledNumber <> -1 then
    begin
      CommandsDataModule.ReleaseUntitledNumber(fUntitledNumber);
      fUntitledNumber := -1;
    end;
    // Kernel change notification
    if (fFileName <> '') and FileExists(fFileName) then
      ChangeNotifier.NotifyWatchFolder(fForm, ExtractFileDir(fFileName))
    else
      ChangeNotifier.NotifyWatchFolder(fForm, '');

    fForm.fNeedToParseModule := True;
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

function TEditor.GetCodeExplorerData: ICodeExplorerData;
begin
  Result := fCodeExplorerData;
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
  Result := WideStringsToEncodedText(GetFileNameOrTitle, fForm.SynEdit.Lines,
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
      Result := ExtractFileName(fFileName);
  end
  else
  begin
    if fUntitledNumber = -1 then
      fUntitledNumber := CommandsDataModule.GetUntitledNumber;
    if fForm.SynEdit.Highlighter = CommandsDataModule.SynPythonSyn then
      Result := _(SNonamePythonFileTitle) + IntToStr(fUntitledNumber)
    else
      Result := _(SNonameFileTitle) + IntToStr(fUntitledNumber);
  end;
end;

function TEditor.GetFileNameOrTitle: string;
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

function TEditor.GetSourceScanner: IAsyncSourceScanner;
begin
  fForm.ReparseIfNeeded;
  Result := fForm.SourceScanner;
end;

function TEditor.GetFileEncoding: TFileSaveFormat;
begin
  Result := fFileEncoding;
end;

procedure TEditor.SetFileEncoding(FileEncoding: TFileSaveFormat);
begin
  fFileEncoding := FileEncoding;
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
  DoSetFileName(AFileName);

  if fForm <> nil then
  begin
    if (AFileName <> '') and FileExists(AFileName) then
    begin
      fForm.SynEdit2.RemoveLinesPointer;
      try
        if LoadFileIntoWideStrings(AFileName, fForm.SynEdit.Lines,
          fFileEncoding) then
        begin
          if not FileAge(AFileName, fForm.FileTime) then
            fForm.FileTime := 0;
        end
        else
          Abort;
      finally
        fForm.SynEdit2.SetLinesPointer(fForm.SynEdit);
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

        fFileEncoding := PyIDEOptions.NewFileEncoding;
      end;
    end;

    fForm.SynEdit.Modified := False;
    fForm.DoUpdateHighlighter(HighlighterName);
    fForm.DoUpdateCaption;
    fForm.fOldEditorForm := fForm;
    fForm.Synedit.UseCodeFolding := PyIDEOptions.CodeFoldingEnabled;
    fForm.Synedit2.UseCodeFolding := fForm.Synedit.UseCodeFolding;
  end;
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

// IEditCommands implementation

function TEditor.CanCopy: boolean;
begin
  Result := (fForm <> nil);
end;

function TEditor.CanCut: boolean;
begin
  Result := (fForm <> nil) and not fIsReadOnly;
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
  if Quiet or not GetModified or (Dialogs.MessageDlg(_(SFileReloadingWarning),
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
  EncodedSource: AnsiString;
  ExecType: string;
  Source: string;
  Editor: TSynEdit;
begin
  if not HasPythonFile or PyControl.IsRunning then
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

  ShowDockForm(PythonIIForm);
  PythonIIForm.SynEdit.ExecuteCommand(ecEditorBottom, ' ', nil);
  PythonIIForm.AppendText(sLineBreak);
  Source := CleanEOLs(Source);
  EncodedSource := UTF8BOMString + Utf8Encode(Source);

  if ExecType = 'exec' then
    EncodedSource := EncodedSource + #10;
  // RunSource
  case PyControl.DebuggerState of
    dsInactive:
      if GetPythonEngine.IsPython3000 then
        PyControl.ActiveInterpreter.RunSource(Source, '<editor selection>',
          ExecType)
      else
        PyControl.ActiveInterpreter.RunSource(EncodedSource,
          '<editor selection>', ExecType);
    dsPaused, dsPostMortem:
      if GetPythonEngine.IsPython3000 then
        PyControl.ActiveDebugger.RunSource(Source, '<editor selection>',
          ExecType)
      else
        PyControl.ActiveDebugger.RunSource(EncodedSource, '<editor selection>',
          ExecType);
  end;

  PythonIIForm.WritePendingMessages;
  PythonIIForm.AppendPrompt;
  Activate(False);
end;

// IFileCommands implementation

function TEditor.CanPrint: boolean;
begin
  Result := True;
end;

function TEditor.CanSave: boolean;
begin
  Result := (fForm <> nil) and (GetModified or (fFileName = ''));
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
    with CommandsDataModule do
    begin
      PrintDialog.PrintRange := prAllPages;
      if PrintDialog.Execute then
      begin
        SynEditPrint.SynEdit := fForm.SynEdit;
        SynEditPrint.Title := GetFileTitle;
        SynEditPrint.Highlighter := fForm.SynEdit.Highlighter;
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
  CommandsDataModule.SynEditPrint.Highlighter := fForm.SynEdit.Highlighter;
  with TPrintPreviewDlg.Create(PyIDEMainForm) do
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
    if fFileName <> '' then
      fForm.DoSave
    else
      fForm.DoSaveAs
  end;
end;

procedure TEditor.ExecSaveAs;
begin
  if fForm <> nil then
    fForm.DoSaveAs;
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
  Result := (fForm <> nil) and not fIsReadOnly and not IsEmpty;
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
    function CreateTabSheet(AOwner: TSpTBXCustomTabControl): IEditor;
    function GetEditorCount: Integer;
    function GetEditorByName(const Name: string): IEditor;
    function GetEditorByNameOrTitle(const Name: string): IEditor;
    function GetEditor(Index: Integer): IEditor;
    procedure RemoveEditor(AEditor: IEditor);
    procedure RegisterViewFactory(ViewFactory: IEditorViewFactory);
    procedure SetupEditorViewMenu;
    procedure UpdateEditorViewMenu;
    function GetViewFactoryCount: Integer;
    function GetViewFactory(Index: Integer): IEditorViewFactory;
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

destructor TEditorFactory.Destroy;
begin
  fEditors.Free;
  fEditorViewFactories.Free;
  inherited Destroy;
end;

function TEditorFactory.CanCloseAll: boolean;
var
  i: Integer;
begin
  Result := False;
  with TPickListDialog.Create(Application.MainForm) do
  begin
    Caption := _(SSaveModifiedFiles);
    lbMessage.Caption := _(SSelectModifiedFiles);
    for i := 0 to fEditors.Count - 1 do
      if IEditor(fEditors[i]).Modified then
        CheckListBox.Items.AddObject(IEditor(fEditors[i]).GetFileNameOrTitle,
          IEditor(fEditors[i]).Form);
    SetScrollWidth;
    mnSelectAllClick(nil);
    if CheckListBox.Items.Count = 0 then
      Result := True
    else if CheckListBox.Items.Count = 1 then
      Result := TEditorForm(CheckListBox.Items.Objects[0]).DoAskSaveChanges
    else if ShowModal = IdOK then
    begin
      Result := True;
      for i := CheckListBox.Count - 1 downto 0 do
      begin
        if CheckListBox.Checked[i] then
        begin
          if not TEditorForm(CheckListBox.Items.Objects[i]).DoSave then
          begin
            Result := False;
            break;
          end;
        end;
      end;
    end;
    Free;
  end;

  // i := fEditors.Count - 1;
  // while i >= 0 do begin
  // LEditor := IEditor(fEditors[i]);
  // if not LEditor.AskSaveChanges then begin
  // Result := FALSE;
  // exit;
  // end;
  // Dec(i);
  // end;
  // Result := TRUE;

end;

procedure TEditorFactory.CloseAll;
var
  i: Integer;
begin
  i := fEditors.Count - 1;
  while i >= 0 do
  begin
    IEditor(fEditors[i]).Close;
    Dec(i);
  end;
end;

function TEditorFactory.CreateTabSheet(AOwner: TSpTBXCustomTabControl): IEditor;
var
  Sheet: TSpTBXTabSheet;
  LForm: TEditorForm;
  TabItem: TSpTBXTabItem;
begin
  TabItem := AOwner.Add('');
  Sheet := AOwner.GetPage(TabItem);
  try
    LForm := TEditorForm.Create(Sheet);
    with LForm do
    begin
      Visible := False;
      fEditor := TEditor.Create(LForm);
      ParentTabItem := TabItem;
      ParentTabControl := AOwner;
      Result := fEditor;
      BorderStyle := bsNone;
      Parent := Sheet;
      Align := alClient;
      Visible := True;
    end;
    if Result <> nil then
      fEditors.Add(Result);
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

function TEditorFactory.GetEditorByName(const Name: string): IEditor;
Var
  i: Integer;
begin
  Result := nil;
  for i := 0 to fEditors.Count - 1 do
    if AnsiSameText(IEditor(fEditors[i]).GetFileName,
      GetLongFileName(ExpandFileName(Name))) then
    begin
      Result := IEditor(fEditors[i]);
      break;
    end;
end;

function TEditorFactory.GetEditorByNameOrTitle(const Name: string): IEditor;
Var
  i: Integer;
begin
  Result := GetEditorByName(Name);
  if not Assigned(Result) then
    for i := 0 to fEditors.Count - 1 do
      if (IEditor(fEditors[i]).FileName = '') and AnsiSameText
        (IEditor(fEditors[i]).GetFileTitle, Name) then
      begin
        Result := IEditor(fEditors[i]);
        break;
      end;
end;

function TEditorFactory.GetEditor(Index: Integer): IEditor;
begin
  Result := IEditor(fEditors[Index]);
end;

procedure TEditorFactory.RemoveEditor(AEditor: IEditor);
var
  i: Integer;
begin
  i := fEditors.IndexOf(AEditor);
  if i > -1 then
    fEditors.Delete(i);
end;

procedure TEditorFactory.RegisterViewFactory(ViewFactory: IEditorViewFactory);
begin
  fEditorViewFactories.Add(ViewFactory);
end;

procedure TEditorFactory.OnEditorViewClick(Sender: TObject);
Var
  ViewFactory: IEditorViewFactory;
  EditorView: IEditorView;
  Editor: IEditor;
  Index: Integer;
begin
  Editor := GI_ActiveEditor;
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

procedure TEditorFactory.SetupEditorViewMenu;
Var
  MenuItem: TSpTBXItem;
  i: Integer;
  ViewFactory: IEditorViewFactory;
begin
  PyIDEMainForm.EditorViewsMenu.Clear;
  fEditorViewFactories.Lock;
  try
    PyIDEMainForm.EditorViewsMenu.Enabled := fEditorViewFactories.Count > 0;
    for i := 0 to fEditorViewFactories.Count - 1 do
    begin
      ViewFactory := fEditorViewFactories[i] as IEditorViewFactory;

      // Add MenuItem
      MenuItem := TSpTBXItem.Create(PyIDEMainForm);
      MenuItem.Hint := ViewFactory.Hint;
      MenuItem.ImageIndex := ViewFactory.ImageIndex;
      MenuItem.Caption := ViewFactory.MenuCaption;
      MenuItem.ShortCut := ViewFactory.ShortCut;
      MenuItem.OnClick := OnEditorViewClick;
      MenuItem.Tag := i;

      PyIDEMainForm.EditorViewsMenu.Add(MenuItem);
    end;
  finally
    fEditorViewFactories.UnLock;
  end;
end;

procedure TEditorFactory.UpdateEditorViewMenu;
Var
  i, j: Integer;
  ViewFactory: IEditorViewFactory;
  List: TList;
  Enabled: boolean;
begin
  fEditorViewFactories.Lock;
  List := TList.Create;
  try
    for i := 0 to fEditorViewFactories.Count - 1 do
    begin
      Enabled := Assigned(GI_ActiveEditor);
      if Enabled then
      begin
        ViewFactory := fEditorViewFactories[i] as IEditorViewFactory;
        ViewFactory.GetContextHighlighters(List);
        if List.Count > 0 then
        begin
          Enabled := False;
          for j := 0 to List.Count - 1 do
          begin
            if List[j] = GI_ActiveEditor.SynEdit.Highlighter then
            begin
              Enabled := True;
              break;
            end;
          end;
        end;
        List.Clear;
      end;
      PyIDEMainForm.EditorViewsMenu.Items[i].Enabled := Enabled;
    end;
  finally
    List.Free;
    fEditorViewFactories.UnLock;
  end;
end;

{ TEditorForm }

procedure TEditorForm.FormDestroy(Sender: TObject);
var
  LEditor: IEditor;
begin
  if Assigned(SourceScanner) then
    SourceScanner.StopScanning;
  SourceScanner := nil;

  SynEdit2.RemoveLinesPointer;
  LEditor := fEditor;
  Assert(fEditor <> nil);
  fEditor.fForm := nil;
  Assert(GI_EditorFactory <> nil);
  GI_EditorFactory.RemoveEditor(LEditor);
  if GI_EditorFactory.Count = 0 then
    PyIDEMainForm.UpdateCaption;
  BreakPoints.Free;
  FoundSearchItems.Free;
  fSyntaxErrorPos.Free;

  // Unregister kernel notification
  ChangeNotifier.UnRegisterKernelChangeNotify(Self);

  SkinManager.RemoveSkinNotification(Self);
end;

procedure TEditorForm.SynEditChange(Sender: TObject);
begin
  with PyControl.ErrorPos do
    if Editor = GetEditor then
    begin
      Clear;
      PyIDEMainForm.DebuggerErrorPosChange(Self);
    end;
  fSyntaxErrorPos.Clear;
  fNeedToCheckSyntax := True;

  if Assigned(SourceScanner) then
    SourceScanner.StopScanning;
  fNeedToParseModule := True;

  InvalidateHighlightedTerms(SynEdit, FoundSearchItems);
  InvalidateHighlightedTerms(SynEdit2, FoundSearchItems);
  FoundSearchItems.Clear;
end;

procedure TEditorForm.SynEditDblClick(Sender: TObject);
var
  ptMouse: TPoint;
  ASynEdit: TSynEdit;
begin
  ASynEdit := Sender as TSynEdit;
  GetCursorPos(ptMouse);
  ptMouse := ASynEdit.ScreenToClient(ptMouse);
  if (ptMouse.X >= ASynEdit.Gutter.Width + 2)
    and ASynEdit.SelAvail and PyIDEOptions.
    HighlightSelectedWord then
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
  CommandsDataModule.ParameterCompletion.Editor := ASynEdit;
  CommandsDataModule.ModifierCompletion.Editor := ASynEdit;
  CommandsDataModule.CodeTemplatesCompletion.Editor := ASynEdit;
  CommandsDataModule.CodeTemplatesCompletion.OnBeforeExecute :=
    AutoCompleteBeforeExecute;
  CommandsDataModule.CodeTemplatesCompletion.OnAfterExecute :=
    AutoCompleteAfterExecute;

  if ASynEdit.Highlighter is TSynWebBase then
  begin
    SynCodeCompletion.Editor := nil;
    SynWebCompletion.Editor := ASynEdit;
  end
  else
  begin
    SynCodeCompletion.Editor := ASynEdit;
    SynWebCompletion.Editor := nil;
  end;

  if fOldEditorForm <> Self then
    CodeExplorerWindow.UpdateWindow(ceuEnter);
  fOldEditorForm := Self;

  // Search and Replace Target
  EditorSearchOptions.InterpreterIsSearchTarget := False;

  PyIDEMainForm.UpdateCaption;
end;

procedure TEditorForm.SynEditExit(Sender: TObject);
begin
  // The following create problems
  // CommandsDataModule.ParameterCompletion.Editor := nil;
  // CommandsDataModule.ModifierCompletion.Editor := nil;
  // CommandsDataModule.CodeTemplatesCompletion.Editor := nil;
  DoAssignInterfacePointer(False);
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
  if Changes * [scAll, scReadOnly] <> [] then
    fEditor.fIsReadOnly := ASynEdit.ReadOnly;
  if scModified in Changes then
  begin
    PyIDEMainForm.UpdateCaption;
    ParentTabItem.Invalidate;
  end;
  if scCaretY in Changes then begin
    fNeedToSyncCodeExplorer := True;
    fCloseBracketChar := #0;
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
  // this is necessary to prevent second confirmation when closing MDI childs
  if SynEdit.Modified then
  begin
    DoActivateEditor;
    MessageBeep(MB_ICONQUESTION);
    Assert(fEditor <> nil);
    S := Format(_(SAskSaveChanges), [ExtractFileName(fEditor.GetFileTitle)]);

    case Dialogs.MessageDlg(S, mtConfirmation, [mbYes, mbNo, mbCancel], 0,
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
  if fEditor.fFileName <> '' then
    Result := DoSaveFile
  else
    Result := DoSaveAs;
end;

function TEditorForm.DoSaveFile: boolean;
var
  i: Integer;
begin
  // Trim all lines just in case (Issue 196)
  if (eoTrimTrailingSpaces in SynEdit.Options) and (SynEdit.Lines.Count > 0)
    then
  begin
    SynEdit.BeginUpdate;
    try
      for i := 0 to SynEdit.Lines.Count - 1 do
        SynEdit.Lines[i] := TrimRight(SynEdit.Lines[i]);
    finally
      SynEdit.EndUpdate;
    end;
  end;
  Result := SaveWideStringsToFile(fEditor.fFileName, SynEdit.Lines,
    fEditor.fFileEncoding, PyIDEOptions.CreateBackupFiles);
  if Result then
  begin
    if not FileAge(fEditor.fFileName, FileTime) then
      FileTime := 0;
    if not PyIDEOptions.UndoAfterSave then
      SynEdit.ClearUndo;
    SynEdit.Modified := False;
  end;
end;

function TEditorForm.DoSaveAs: boolean;
var
  NewName: string;
  Edit: IEditor;
begin
  Assert(fEditor <> nil);
  NewName := fEditor.GetFileNameOrTitle;
  if (fEditor.GetFileName = '') and (DefaultExtension <> '') and
    (ExtractFileExt(NewName) = '') then
    NewName := NewName + '.' + DefaultExtension;
  if CommandsDataModule.GetSaveFileName(NewName, SynEdit.Highlighter,
    DefaultExtension) then
  begin
    Edit := GI_EditorFactory.GetEditorByName(NewName);
    if Assigned(Edit) and (Edit <> Self.fEditor as IEditor) then
    begin
      Dialogs.MessageDlg(_(SFileAlreadyOpen), mtError, [mbAbort], 0);
      Result := False;
      Exit;
    end;
    fEditor.DoSetFileName(NewName);
    DoUpdateHighlighter;
    DoUpdateCaption; // Do it twice in case the following statement fails
    Result := DoSaveFile;
    DoUpdateCaption;
  end
  else
    Result := False;
end;

procedure TEditorForm.DoUpdateCaption;
begin
  Assert(fEditor <> nil);
  with ParentTabItem do
  begin
    Caption := StringReplace(fEditor.GetFileTitle, '&', '&&', [rfReplaceAll]);
    Hint := fEditor.GetFileName;
  end;
  PyIDEMainForm.UpdateCaption;
end;

procedure TEditorForm.DoUpdateHighlighter(HighlighterName: string = '');
var
  Index: Integer;
begin
  Assert(fEditor <> nil);
  if fEditor.fFileName <> '' then
    SynEdit.Highlighter := CommandsDataModule.GetHighlighterForFile
      (fEditor.fFileName)
  else if HighlighterName <> '' then
  begin
    Index := CommandsDataModule.Highlighters.IndexOf(HighlighterName);
    if Index < 0 then
      SynEdit.Highlighter := nil
    else
      SynEdit.Highlighter := CommandsDataModule.Highlighters.Objects[Index]
        as TSynCustomHighlighter;
  end
  else // No highlighter otherwise
    SynEdit.Highlighter := nil;
  SynEdit2.Highlighter := SynEdit.Highlighter;
  SynEdit.RegisterCommandHandler(EditorCommandHandler, nil);
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

procedure TEditorForm.EditorViewsChange(Sender: TObject);
begin
  SetUpCodeHints;
end;

procedure TEditorForm.EditorZoom(theZoom: Integer);
begin
  if not((theZoom < 1) and (SynEdit.Font.Size <= 2)) then
  begin
    SynEdit.Font.Size := SynEdit.Font.Size + theZoom;
    SynEdit.Gutter.Font.Size := Max(SynEdit.Font.Size - 2, 1);
    SynEdit2.Font.Size := SynEdit.Font.Size;
    SynEdit2.Gutter.Font.Size := SynEdit2.Gutter.Font.Size;
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
  BC: TBufferCoord;
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
          if SynCodeCompletion.Form.Visible then
            SynCodeCompletion.CancelCompletion;
          // SynCodeCompletion.DefaultType := ctCode;
          SynCodeCompletion.ActivateCompletion;
          Handled := True;
        end else if ASynEdit.Highlighter is TSynWebBase then
          SynWebCompletion.ActivateCompletion;
      ecParamCompletion:
        if ASynEdit.Highlighter is TSynPythonSyn then
        begin
          if SynParamCompletion.Form.Visible then
            SynParamCompletion.CancelCompletion;
          // SynCodeCompletion.DefaultType := ctParams;
          SynParamCompletion.ActivateCompletion;
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
      ecWordRight, ecSelWordRight: // Implement Visual Studio like behaviour
        begin
          BC := VSNextWordPos(ASynEdit, ASynEdit.CaretXY);
          if Command = ecWordRight then
            ASynEdit.CaretXY := BC
          else
          begin
            if (ASynEdit.BlockEnd.Line = ASynEdit.CaretXY.Line) and
              (ASynEdit.BlockEnd.Char = ASynEdit.CaretXY.Char) then
              ASynEdit.SetCaretAndSelection(BC, ASynEdit.BlockBegin, BC)
            else
              ASynEdit.SetCaretAndSelection(BC, ASynEdit.BlockEnd, BC);
          end;
          Handled := True;
        end;
      ecWordLeft, ecSelWordLeft: // Implement Visual Studio like behaviour
        begin
          BC := VSPrevWordPos(ASynEdit, ASynEdit.CaretXY);
          if Command = ecWordLeft then
            ASynEdit.CaretXY := BC
          else
          begin
            if (ASynEdit.BlockEnd.Line = ASynEdit.CaretXY.Line) and
              (ASynEdit.BlockEnd.Char = ASynEdit.CaretXY.Char) then
              ASynEdit.SetCaretAndSelection(BC, ASynEdit.BlockBegin, BC)
            else
              ASynEdit.SetCaretAndSelection(BC, ASynEdit.BlockEnd, BC);
          end;
          Handled := True;
        end;
      ecMatchBracket:
        begin
          BC := GetMatchingBracket(ASynEdit);
          if BC.Char > 0 then
            ASynEdit.CaretXY := BC;
          Handled := True;
        end;
      ecSelMatchBracket:
        begin
          BC := GetMatchingBracket(ASynEdit);
          if BC.Char > 0 then
            ASynEdit.SetCaretAndSelection(BC, ASynEdit.CaretXY, BC);
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
          //    (attr = CommandsDataModule.SynPythonSyn.DocStringAttri) } ) then
          //begin
          ASynEdit.UndoList.BeginBlock;
            if CommandsDataModule.IsBlockOpener(iPrevLine) then
              ASynEdit.ExecuteCommand(ecTab, #0, nil)
            else if CommandsDataModule.IsBlockCloser(iPrevLine) then
              ASynEdit.ExecuteCommand(ecShiftTab, #0, nil);
          ASynEdit.UndoList.EndBlock;
          //end;
        end;
      ecChar: // Autocomplete brackets
        if not fAutoCompleteActive and PyIDEOptions.
          AutoCompleteBrackets then
          with ASynEdit do
          begin
            if ASynEdit.Highlighter is TSynPythonSyn then
            begin
              OpenBrackets := '([{"''';
              CloseBrackets := ')]}"''';
            end
            else if (ASynEdit.Highlighter = CommandsDataModule.SynWebHTMLSyn)
              or (ASynEdit.Highlighter = CommandsDataModule.SynWebXMLSyn) or
              (ASynEdit.Highlighter = CommandsDataModule.SynWebCssSyn) then
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
                  SelText := CloseBrackets[OpenBracketPos];
                  CaretX := CaretX - 1;
                  if not CharInSet(AChar, [')', ']', '}']) then
                    fCloseBracketChar := CloseBrackets[OpenBracketPos];
                end;
              end;
            end;
          end;
      ecSelWord:
        if ASynEdit.SelAvail and PyIDEOptions.
          HighlightSelectedWord then
          CommandsDataModule.HighlightWordInActiveEditor(ASynEdit.SelText);
      ecLostFocus:
        if not (SynCodeCompletion.Form.Visible or SynEdit.Focused) then
          SynParamCompletion.CancelCompletion;
    end;
  end;
end;

procedure TEditorForm.PaintGutterGlyphs(ACanvas: TCanvas; AClip: TRect;
  FirstLine, LastLine: Integer);
var
  LH, X, Y: Integer;
  LI: TDebuggerLineInfos;
  ImgIndex: Integer;
  Line: Integer;
begin
  if (PyControl.ActiveDebugger <> nil) and SynEdit.Gutter.Visible then
  begin
    FirstLine := SynEdit.RowToLine(FirstLine);
    LastLine := SynEdit.RowToLine(LastLine);
    X := SynEdit.BookMarkOptions.Xoffset + SynEdit.Gutter.RightMargin;
    LH := SynEdit.LineHeight;

    for Line := FirstLine to LastLine do
    begin
      if SynEdit.AllFoldRanges.FoldHidesLine(Line) then
        continue;
      Y := (LH - imglGutterGlyphs.Height) div 2 + LH *
        (SynEdit.LineToRow(Line) - SynEdit.TopLine);
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
        imglGutterGlyphs.Draw(ACanvas, X, Y, ImgIndex);
    end;
  end;
end;

procedure TEditorForm.SyncCodeExplorer;
begin
  if fNeedToSyncCodeExplorer and GetEditor.HasPythonFile then
  begin
    CodeExplorerWindow.ShowEditorCodeElement;
    fNeedToSyncCodeExplorer := False;
  end;
end;

function TEditorForm.ReparseIfNeeded: boolean;
begin
  Result := False;
  if fNeedToParseModule then
  begin
    if GetEditor.HasPythonFile then
    begin
      if Assigned(SourceScanner) then
        SourceScanner.StopScanning;
      SourceScanner := AsynchSourceScannerFactory.CreateAsynchSourceScanner
        (fEditor.GetFileNameOrTitle, SynEdit.Text);
      Result := True;
    end
    else
      SourceScanner := nil;
    fNeedToParseModule := False;
    CodeExplorerWindow.UpdateWindow(ceuChange);
  end;
end;

procedure TEditorForm.Retranslate;
begin
  Assert(ViewsTabControl.PagesCount > 0);
  ViewsTabControl.Pages[0].Caption := _(SSourceTabCaption);
end;

procedure TEditorForm.FormCreate(Sender: TObject);
begin
  // Scale ImageList
  ScaleImageList(imglGutterGlyphs, Screen.PixelsPerInch, 96);

  FGPanelExit(Self);

  SynEdit.OnReplaceText := CommandsDataModule.SynEditReplaceText;

  fHotIdentInfo.HaveHotIdent := False;
  fSyntaxErrorPos := TEditorPos.Create;

  ViewsTabControl.TabVisible := False;
  case PyIDEOptions.EditorsTabPosition of
    ttpTop:
      ViewsTabControl.TabPosition := ttpBottom;
    ttpBottom:
      ViewsTabControl.TabPosition := ttpTop;
  end;

  SynEdit2.SetLinesPointer(SynEdit);

  FoundSearchItems := TObjectList.Create(True);
  THighlightSearchPlugin.Create(SynEdit, FoundSearchItems); // No need to free
  THighlightSearchPlugin.Create(SynEdit2, FoundSearchItems); // No need to free

  BreakPoints := TObjectList.Create(True);
  TDebugSupportPlugin.Create(Self); // No need to free

  // Register Kernel Notification
  ChangeNotifier.RegisterKernelChangeNotify(Self, [vkneFileName, vkneDirName,
    vkneLastWrite, vkneCreation]);

  SkinManager.AddSkinNotification(Self);

  PyIDEMainForm.ThemeEditorGutter(SynEdit.Gutter);

  SynCodeCompletion.EndOfTokenChr := WordBreakString;
  SynParamCompletion.EndOfTokenChr := WordBreakString;
  SynCodeCompletion.Options:=PythonIIForm.SynCodeCompletion.Options;
  SynCodeCompletion.TriggerChars:=PythonIIForm.SynCodeCompletion.TriggerChars;
  SynCodeCompletion.TimerInterval := PythonIIForm.SynCodeCompletion.TimerInterval;
  SynCodeCompletion.OnCodeItemInfo := SynCodeCompletionCodeItemInfo;

  // Scale
  SynCodeCompletion.ChangeScale(Screen.PixelsPerInch, 96);
  SynParamCompletion.ChangeScale(Screen.PixelsPerInch, 96);
  SynWebCompletion.ChangeScale(Screen.PixelsPerInch, 96);

  Retranslate;
end;

procedure TEditorForm.SynEditGutterClick(Sender: TObject; Button: TMouseButton;
  X, Y, Line: Integer; Mark: TSynEditMark);
Var
  ASynEdit: TSynEdit;
begin
  ASynEdit := Sender as TSynEdit;
  if (ASynEdit.Highlighter = CommandsDataModule.SynPythonSyn) and
    (PyControl.ActiveDebugger <> nil) and not
    (sfGutterDragging in ASynEdit.StateFlags) then
    PyControl.ToggleBreakpoint(fEditor, Line, GetKeyState(VK_CONTROL) < 0);
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
    SynEdit.CaretXY := BufferCoord(fSyntaxErrorPos.Char, fSyntaxErrorPos.Line);
end;

function TEditorForm.HasSyntaxError: boolean;
begin
  Result :=
    PyIDEOptions.CheckSyntaxAsYouType and fEditor.
    HasPythonFile and fSyntaxErrorPos.IsSyntax and
    (fSyntaxErrorPos.Editor = GetEditor) and
    (fSyntaxErrorPos.Line < SynEdit.Lines.Count);
end;

procedure TEditorForm.SynEditPaintTransient(Sender: TObject; Canvas: TCanvas;
  TransientType: TTransientType);
Var
  Pix: TPoint;
  ASynEdit: TSynEdit;
begin
  ASynEdit := Sender as TSynEdit;
  if (not Assigned(ASynEdit.Highlighter)) then
    Exit;
  if fHotIdentInfo.HaveHotIdent and (fHotIdentInfo.SynEdit = ASynEdit) and
    (TransientType = ttAfter) then
  begin
    Pix := ASynEdit.RowColumnToPixels(ASynEdit.BufferToDisplayPos
        (fHotIdentInfo.StartCoord));
    Canvas.Font.Assign(ASynEdit.Font);
    Canvas.Font.Style := fHotIdentInfo.SynAttri.Style + [fsUnderline];
    Canvas.Font.Color := clHotlight;
    if fHotIdentInfo.SynAttri.Background <> clNone then
      Canvas.Brush.Color := fHotIdentInfo.SynAttri.Background
    else
      Canvas.Brush.Color := ASynEdit.Color;
    Canvas.Brush.Style := bsSolid;
    SetTextCharacterExtra(Canvas.Handle,
      ASynEdit.CharWidth - Canvas.TextWidth('W'));
    Canvas.TextOut(Pix.X, Pix.Y, fHotIdentInfo.SynToken);
  end;
  CommandsDataModule.PaintMatchingBrackets(Canvas, ASynEdit, TransientType);
end;

procedure TEditorForm.SynEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Cancel Code Hint
  CodeHint.CancelHint;
end;

procedure TEditorForm.SynEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if fHotIdentInfo.HaveHotIdent then
  begin
    fHotIdentInfo.HaveHotIdent := False;
    fHotIdentInfo.SynEdit.InvalidateLine(fHotIdentInfo.StartCoord.Line);
    SetCursor(SynEdit.Cursor);
  end;
end;

procedure TEditorForm.SynEditMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  TokenType, Start, ExpStart: Integer;
  Token, LineTxt: string;
  Attri: TSynHighlighterAttributes;
  Pix: TPoint;
  aLineCharPos: TBufferCoord;
  ASynEdit: TSynEdit;
begin
  ASynEdit := Sender as TSynEdit;
  if (ASynEdit.Gutter.Visible) and (X < ASynEdit.Gutter.Width) then
    Exit;
  aLineCharPos := ASynEdit.DisplayToBufferPos(ASynEdit.PixelsToRowColumn(X, Y));
  // Syntax error hints
  if HasSyntaxError and (aLineCharPos.Line = fSyntaxErrorPos.Line) and
    (aLineCharPos.Char <= fSyntaxErrorPos.Char) then
    with ASynEdit do
    begin
      Pix := ClientToScreen(RowColumnToPixels(BufferToDisplayPos(BufferCoord(1,
              aLineCharPos.Line))));
      fHintIdentInfo.IdentArea.TopLeft := Pix;
      Pix := ClientToScreen(RowColumnToPixels(BufferToDisplayPos
            (BufferCoord(fSyntaxErrorPos.Char, aLineCharPos.Line))));
      fHintIdentInfo.IdentArea.Right := Pix.X;
      fHintIdentInfo.IdentArea.Bottom := Pix.Y + LineHeight + 3;
      Pix := ClientToScreen(RowColumnToPixels(BufferToDisplayPos(aLineCharPos))
        );
      Pix.Y := Pix.Y + LineHeight;
      fHintIdentInfo.SynToken := 'Syntax Error';
      CodeHint.ActivateHintAt(fHintIdentInfo.IdentArea, Pix);
    end
    else if fEditor.HasPythonFile and (HiWord(GetAsyncKeyState(VK_CONTROL))
        = 0) and not ASynEdit.IsPointInSelection(aLineCharPos) and
    // (FindVCLWindow(ASynedit.ClientToScreen(Point(X,Y))) = ASynedit) and
      (((PyControl.DebuggerState in [dsPaused,
          dsPostMortem])
          and PyIDEOptions.ShowDebuggerHints) or
        ((PyControl.DebuggerState = dsInactive)
          and PyIDEOptions.ShowCodeHints)) then
      with ASynEdit do
      begin
        // Code and debugger hints
        GetHighlighterAttriAtRowColEx(aLineCharPos, Token, TokenType, Start,
          Attri);
        if (Attri = TSynPythonSyn(Highlighter).IdentifierAttri) or
          (Attri = TSynPythonSyn(Highlighter).NonKeyAttri) or
          (Attri = TSynPythonSyn(Highlighter).SystemAttri) or
          ((PyControl.DebuggerState in [dsPaused, dsPostMortem]) and
            ((Token = ')') or (Token = ']'))) then
        begin
          with fHintIdentInfo do
          begin
            LineTxt := Lines[aLineCharPos.Line - 1];
            SynToken := '';
            Ident := Token;
            DottedIdent := GetWordAtPos(LineTxt, aLineCharPos.Char,
              IdentChars + ['.'], True, False, True);
            ExpStart := aLineCharPos.Char - Length(DottedIdent) + 1;
            DottedIdent := DottedIdent + GetWordAtPos(LineTxt,
              aLineCharPos.Char + 1, IdentChars, False, True);
            // Determine the hint area
            StartCoord := BufferCoord(Start, aLineCharPos.Line);
            Pix := ClientToScreen
              (RowColumnToPixels(BufferToDisplayPos(StartCoord)));
            IdentArea.TopLeft := Pix;
            aLineCharPos := WordEndEx(aLineCharPos);
            if (Token = ']') or (Token = ')') then
              Inc(aLineCharPos.Char);
            Pix := ClientToScreen
              (RowColumnToPixels(BufferToDisplayPos(aLineCharPos)));
            IdentArea.Right := Pix.X;
            IdentArea.Bottom := Pix.Y + LineHeight + 3;
            // Determine where the hint should be shown (beginning of the expression)
            if PyControl.DebuggerState in [dsPaused, dsPostMortem] then
              aLineCharPos := BufferCoord(ExpStart, aLineCharPos.Line)
            else
              aLineCharPos := StartCoord;
            Pix := ClientToScreen
              (RowColumnToPixels(BufferToDisplayPos(aLineCharPos)));
            Pix.Y := Pix.Y + LineHeight;
            // Activate the hint
            CodeHint.ActivateHintAt(IdentArea, Pix);
          end;
        end;
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
  ASynEdit: TSynEdit;
begin
  ASynEdit := Sender as TSynEdit;
  if fHotIdentInfo.HaveHotIdent then
  begin
    fHotIdentInfo.HaveHotIdent := False;
    fHotIdentInfo.SynEdit.InvalidateLine(fHotIdentInfo.StartCoord.Line);
  end;
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
          SynEdit := ASynEdit;
          HaveHotIdent := True;
          SynAttri := Attri;
          SynToken := Token;
          StartCoord := BufferCoord(Start, aLineCharPos.Line);
          SynEdit.InvalidateLine(aLineCharPos.Line);
        end;
      end;
    end;
end;

procedure TEditorForm.SynEditMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  EditorSearchOptions.InitSearch;
  with PyControl.ErrorPos do
    if Editor = GetEditor then
    begin
      Clear;
      PyIDEMainForm.DebuggerErrorPosChange(Self);
    end;

  if fHotIdentInfo.HaveHotIdent then
  begin
    fHotIdentInfo.HaveHotIdent := False;
    fHotIdentInfo.SynEdit.InvalidateLine(fHotIdentInfo.StartCoord.Line);
    PostMessage(PyIDEMainForm.Handle, WM_FINDDEFINITION,
      fHotIdentInfo.StartCoord.Char, fHotIdentInfo.StartCoord.Line);
  end;
  if SynParamCompletion.Form.Visible then
    SynParamCompletion.CancelCompletion;
end;

procedure TEditorForm.FGPanelEnter(Sender: TObject);
begin
  HasFocus := True;
  BGPanel.Color := frmIDEDockWin.BorderHighlight;
  // FGPanel.Margins.SetBounds(2,2,2,2);
end;

procedure TEditorForm.FGPanelExit(Sender: TObject);
begin
  HasFocus := False;
  BGPanel.Color := frmIDEDockWin.BorderNormal;
  // FGPanel.Margins.SetBounds(0,0,0,0);
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

procedure TEditorForm.WMPARAMCOMPLETION(var Message: TMessage);
begin
  SynParamCompletion.ActivateCompletion;
end;

procedure TEditorForm.WMShellNotify(var Msg: TMessage);
{
  Does nothing except for releasing the ShellEventList
  All the processing takes place from the FileExplorer's
  OnAfterShellNotify Event
  Note the File Explorer receives the Kernel notifications
  registered by the editors
}
var
  ShellEventList: TVirtualShellEventList;
  // ShellEvent : TVirtualShellEvent;
  // List: TList;
  // Count: integer;
  // i : integer;
begin
  // if csDestroying in ComponentState then Exit;
  //
  ShellEventList := TVirtualShellEventList(Msg.wParam);
  // List := ShellEventList.LockList;
  // try
  // begin
  // Count := List.Count;
  // for i := 0 to Count - 1 do
  // begin
  // ShellEvent := TVirtualShellEvent(List.Items[i]);
  // CommandsDataModule.ProcessShellNotify(ShellEvent);
  // end;
  // end;
  // finally
  // ShellEventList.UnlockList;
  ShellEventList.Release;
  // end;
end;

procedure TEditorForm.AddWatchAtCursor;
var
  TokenType, Start: Integer;
  Token, LineTxt, DottedIdent: string;
  Attri: TSynHighlighterAttributes;
  aLineCharPos: TBufferCoord;
begin
  aLineCharPos := SynEdit.CaretXY;
  if SynEdit.Highlighter = CommandsDataModule.SynPythonSyn then
    with SynEdit do
    begin
      GetHighlighterAttriAtRowColEx(aLineCharPos, Token, TokenType, Start,
        Attri);
      if (Attri = CommandsDataModule.SynPythonSyn.IdentifierAttri) or
        (Attri = CommandsDataModule.SynPythonSyn.NonKeyAttri) or
        (Attri = CommandsDataModule.SynPythonSyn.SystemAttri) or
        ((Token = ')') or (Token = ']')) then
      begin
        LineTxt := Lines[aLineCharPos.Line - 1];
        DottedIdent := GetWordAtPos(LineTxt, aLineCharPos.Char,
          IdentChars + ['.'], True, False, True);
        DottedIdent := DottedIdent + GetWordAtPos(LineTxt,
          aLineCharPos.Char + 1, IdentChars, False, True);
        if DottedIdent <> '' then
          WatchesWindow.AddWatch(DottedIdent);
      end;
    end;
end;

procedure TEditorForm.SetUpCodeHints;
begin
  if (ViewsTabControl.ActivePage = tbshSource) and fEditor.HasPythonFile then
  begin
    CodeHint.OnGetCodeHint := CodeHintEventHandler;
    CodeHint.OnHyperLinkClick := CodeHintLinkHandler;
  end
  else
  begin
    CodeHint.OnGetCodeHint := nil;
    CodeHint.OnHyperLinkClick := nil;
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
  if HasFocus then
  begin
    BGPanel.Color := frmIDEDockWin.BorderHighlight;
    // FGPanel.Margins.SetBounds(2,2,2,2);
  end
  else
  begin
    BGPanel.Color := frmIDEDockWin.BorderNormal;
    // FGPanel.Margins.SetBounds(0,0,0,0);
  end;

  PyIDEMainForm.ThemeEditorGutter(SynEdit.Gutter);
  SynEdit.InvalidateGutter;
  PyIDEMainForm.ThemeEditorGutter(SynEdit2.Gutter);
  SynEdit2.InvalidateGutter;
  Invalidate;
end;

procedure TEditorForm.SynCodeCompletionAfterCodeCompletion(Sender: TObject;
  const Value: string; Shift: TShiftState; Index: Integer; EndToken: Char);
begin
  if EndToken = '(' then
    PostMessage(Handle, WM_PARAMCOMPLETION, 0, 0);
end;

procedure TEditorForm.CleanupCodeCompletion;
begin
  if Assigned(fCompletionHandler) then
    fCompletionHandler.Finalize;
  fCompletionHandler := nil;
end;

procedure TEditorForm.SynCodeCompletionClose(Sender: TObject);
begin
  PyIDEOptions.CodeCompletionListSize :=
    SynCodeCompletion.NbLinesInWindow;
  CleanupCodeCompletion;
end;

procedure TEditorForm.SynCodeCompletionCodeItemInfo(Sender: TObject;
  AIndex: Integer; var Info: string);
begin
  if Assigned(fCompletionHandler) then
    Info := fCompletionHandler.GetInfo((Sender as TSynCompletionProposal).InsertList[AIndex]);
end;

procedure TEditorForm.SynCodeCompletionExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: string; var X, Y: Integer;
  var CanExecute: boolean);
Var
  i : integer;
  Skipped, Handled : Boolean;
  locline: string;
  DisplayText, InsertText: string;
  FileName : string;
  Attr: TSynHighlighterAttributes;
  DummyToken: string;
  BC: TBufferCoord;
  SkipHandler : TBaseCodeCompletionSkipHandler;
begin
  if not fEditor.HasPythonFile or PyControl.IsRunning or
    not PyIDEOptions.EditorCodeCompletion then
  begin
    CanExecute := False;
    Exit;
  end;

  with TSynCompletionProposal(Sender).Editor do
  begin
    BC := CaretXY;
    Dec(BC.Char);
    GetHighlighterAttriAtRowCol(BC, DummyToken, Attr);

    FileName := GetEditor.GetFileNameOrTitle;
    locline := StrPadRight(LineText, CaretX - 1, ' ');
    // to deal with trim trailing spaces

    Skipped := False;
    for I := 0 to TIDECompletion.EditorCodeCompletion.SkipHandlers.Count -1 do
    begin
      SkipHandler := TIDECompletion.EditorCodeCompletion.SkipHandlers[i] as
        TBaseCodeCompletionSkipHandler;
      Skipped := SkipHandler.SkipCodeCompletion(locline, FileName, CaretXY, Highlighter, Attr);
      if Skipped then Break;
    end;

    Handled := False;
    if not Skipped then
    begin
      for I := 0 to TIDECompletion.EditorCodeCompletion.CompletionHandlers.Count -1 do
      begin
        fCompletionHandler := TIDECompletion.EditorCodeCompletion.CompletionHandlers[i] as
          TBaseCodeCompletionHandler;
        Handled := fCompletionHandler.HandleCodeCompletion(locline, FileName,
          CaretXY, Highlighter, Attr, InsertText, DisplayText);
        if Handled then Break;
      end;
    end;

    CanExecute := not Skipped and Handled and (InsertText <> '');
  end;

  with TSynCompletionProposal(Sender) do
    if CanExecute then
    begin
      Font := PyIDEOptions.AutoCompletionFont;
      ItemList.Text := DisplayText;
      InsertList.Text := InsertText;
      NbLinesInWindow := PyIDEOptions.CodeCompletionListSize;

      // Auto-complete with one entry without showing the form
      if PyIDEOptions.CompleteWithOneEntry then begin
        CurrentString := CurrentInput;
        if Form.AssignedList.Count = 1 then
        begin
          CanExecute := False;
          OnValidate(Form, [], #0);
          CleanupCodeCompletion;
        end;
      end;
    end
    else
    begin
      ItemList.Clear;
      InsertList.Clear;
      CleanupCodeCompletion;
    end;
end;

type
  TParamCompletionData = record
    lookup, DisplayText, Doc: string;
  end;

Var
  OldParamCompetionData: TParamCompletionData;

procedure TEditorForm.SynParamCompletionExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: string; var X, Y: Integer;
  var CanExecute: boolean);
Var
  locline, lookup: string;
  TmpX, StartX, ParenCounter, TmpLocation: Integer;
  FoundMatch: boolean;
  FName, DisplayText, ErrMsg, Doc: string;
  P: TPoint;
  Scope: TCodeElement;
  Def: TBaseCodeElement;
  ParsedModule: TParsedModule;
  PythonPathAdder: IInterface;
  TokenType, Start: Integer;
  Token: string;
  Attri: TSynHighlighterAttributes;
  AlreadyActive: boolean;
  Attr: TSynHighlighterAttributes;
  DummyToken: string;
  BC: TBufferCoord;
begin
  if not fEditor.HasPythonFile or PyControl.IsRunning or
    not PyIDEOptions.EditorCodeCompletion then
  begin
    CanExecute := False;
    Exit;
  end;

  with TSynCompletionProposal(Sender).Editor do
  begin
    BC := CaretXY;
    Dec(BC.Char);
    if GetHighlighterAttriAtRowCol(BC, DummyToken, Attr) and (
      { (attr = Highlighter.StringAttribute) or }
      (Attr = Highlighter.CommentAttribute) or
        (Attr = TSynPythonSyn(Highlighter).CodeCommentAttri) or
        (Attr = TSynPythonSyn(Highlighter).DocStringAttri)) then
    begin
      // Do not code complete inside strings or comments
      CanExecute := False;
      Exit;
    end;

    AlreadyActive := TSynCompletionProposal(Sender).Form.Visible;

    locline := LineText;

    // go back from the cursor and find the first open paren
    TmpX := CaretX;
    StartX := CaretX;
    if TmpX > Length(locline) then
      TmpX := Length(locline)
    else
      Dec(TmpX);
    FoundMatch := False;
    TmpLocation := 0;

    while (TmpX > 0) and not(FoundMatch) do
    begin
      if locline[TmpX] = ',' then
      begin
        Inc(TmpLocation);
        Dec(TmpX);
      end
      else if locline[TmpX] = ')' then
      begin
        // We found a close, go till it's opening paren
        ParenCounter := 1;
        Dec(TmpX);
        while (TmpX > 0) and (ParenCounter > 0) do
        begin
          if locline[TmpX] = ')' then
            Inc(ParenCounter)
          else if locline[TmpX] = '(' then
            Dec(ParenCounter);
          Dec(TmpX);
        end;
      end
      else if locline[TmpX] = '(' then
      begin
        // we have a valid open paren, lets see what the word before it is
        StartX := TmpX;
        while (TmpX > 0) and not CharInSet(locline[TmpX], IdentChars + ['.'])
          do // added [.]
          Dec(TmpX);
        if TmpX > 0 then
        begin
          DisplayText := '';

          GetHighlighterAttriAtRowColEx(BufferCoord(TmpX, CaretY), Token,
            TokenType, Start, Attri);
          if (Attri = TSynPythonSyn(Highlighter).IdentifierAttri) or
            (Attri = TSynPythonSyn(Highlighter).NonKeyAttri) or
            (Attri = TSynPythonSyn(Highlighter).SystemAttri) then
          begin
            lookup := GetWordAtPos(locline, TmpX, IdentChars + ['.'], True,
              False);
            // string constant completion
            if (lookup <> '') and (lookup[1] = '.') and
               (TmpX > Length(lookup)) and
               CharInSet(locline[TmpX - Length(lookup)], ['''', '"'])
            then
              lookup := 'str' + lookup;

            if AlreadyActive and (lookup = OldParamCompetionData.lookup) then
            begin
              DisplayText := OldParamCompetionData.DisplayText;
              Doc := OldParamCompetionData.Doc;
              FoundMatch := True;
            end
            else
            begin
              FName := GetEditor.GetFileNameOrTitle;
              // Add the file path to the Python path - Will be automatically removed
              PythonPathAdder := InternalInterpreter.AddPathToPythonPath
                (ExtractFileDir(FName));

              if PyScripterRefactor.InitializeQuery then
              begin
                // GetParsedModule
                ParsedModule := PyScripterRefactor.GetParsedModule(FName, None);
                Scope := nil;
                if Assigned(ParsedModule) then
                  Scope := ParsedModule.GetScopeForLine(CaretY);
                if Assigned(ParsedModule) and Assigned(Scope) then
                begin
                  Def := PyScripterRefactor.FindDottedDefinition(lookup,
                    ParsedModule, Scope, ErrMsg);

                  if Assigned(Def) and (Def is TParsedClass) then
                    Def := TParsedClass(Def).GetConstructor;

                  if Assigned(Def) and (Def is TParsedFunction) then
                  begin
                    DisplayText := TParsedFunction(Def).ArgumentsString;
                    // Remove self arguments from methods
                    if StrIsLeft(PWideChar(DisplayText), 'self') then
                      Delete(DisplayText, 1, 4);
                    if StrIsLeft(PWideChar(DisplayText), ', ') then
                      Delete(DisplayText, 1, 2);
                    Doc := TParsedFunction(Def).DocString;

                    OldParamCompetionData.lookup := lookup;
                    OldParamCompetionData.DisplayText := DisplayText;
                    OldParamCompetionData.Doc := Doc;
                    FoundMatch := True;
                  end;
                end;
                PyScripterRefactor.FinalizeQuery;
              end;
            end;
          end;

          if not(FoundMatch) then
          begin
            TmpX := StartX;
            Dec(TmpX);
          end;
        end;
      end
      else
        Dec(TmpX)
    end;
  end;

  if FoundMatch then
  begin
    // CanExecute := (DisplayText <> '') or (Doc <> '');
    CanExecute := True;
  end
  else
    CanExecute := False;

  if CanExecute then
  begin
    with TSynCompletionProposal(Sender) do
    begin
      Font := PyIDEOptions.AutoCompletionFont;
      if DisplayText = '' then
      begin
        FormatParams := False;
        DisplayText := '\style{~B}' + _(SNoParameters) + '\style{~B}';
      end
      else
      begin
        FormatParams := True;
      end;
      if (Doc <> '') then
      begin
        DisplayText := DisplayText + sLineBreak;
        Doc := GetLineRange(Doc, 1, 20) // 20 lines max
      end;

      Form.CurrentIndex := TmpLocation;
      ItemList.Text := DisplayText + Doc;
    end;

    // position the hint window at and just below the opening bracket
    P := SynEdit.ClientToScreen(SynEdit.RowColumnToPixels
        (SynEdit.BufferToDisplayPos(BufferCoord(Succ(StartX), SynEdit.CaretY)))
      );
    Inc(P.Y, SynEdit.LineHeight);
    X := P.X;
    Y := P.Y;
  end
  else
  begin
    TSynCompletionProposal(Sender).ItemList.Clear;
    TSynCompletionProposal(Sender).InsertList.Clear;
  end;
end;

procedure TEditorForm.SynWebCompletionAfterCodeCompletion(Sender: TObject;
  const Value: string; Shift: TShiftState; Index: Integer;
  EndToken: WideChar);
Var
    SynEdit: TCustomSynEdit;

  function CaretBetween(AStr: String): boolean;
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
  if Pos('>>', SynEdit.Lines[SynEdit.CaretY]) >= 0 then
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
  SynWebFillCompletionProposal(SynEdit, CommandsDataModule.SynWebHTMLSyn,
    SynWebCompletion, CurrentInput);
  TSynCompletionProposal(Sender).Font :=
    PyIDEOptions.AutoCompletionFont;
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

procedure TEditorForm.CodeHintEventHandler(Sender: TObject; AArea: TRect;
  var CodeHint: string);
Var
  ObjectValue, ObjectType: string;
  ErrMsg: string;
  CE: TBaseCodeElement;
begin
  if CompareMem(@fHintIdentInfo.IdentArea, @AArea, SizeOf(TRect)) then
  begin
    if (fHintIdentInfo.SynToken = 'Syntax Error') and HasSyntaxError then
    begin
      // Syntax hint
      CodeHint := 'Syntax Error: ' + fSyntaxErrorPos.ErrorMsg;
    end
    else if (PyControl.DebuggerState in [dsPaused, dsPostMortem])
      and PyIDEOptions.ShowDebuggerHints then
    begin
      // Debugger hints
      PyControl.ActiveDebugger.Evaluate(fHintIdentInfo.DottedIdent, ObjectType,
        ObjectValue);
      if ObjectValue <> _(SNotAvailable) then
      begin
        ObjectValue := HTMLSafe(ObjectValue);
        ObjectType := HTMLSafe(ObjectType);
        CodeHint := Format(_(SDebuggerHintFormat), [fHintIdentInfo.DottedIdent,
          ObjectType, ObjectValue]);
      end
      else
        CodeHint := '';
    end
    else if (PyControl.DebuggerState = dsInactive)
      and PyIDEOptions.ShowCodeHints then
    begin
      // Code hints
      CE := PyScripterRefactor.FindDefinitionByCoordinates
        (fEditor.GetFileNameOrTitle, fHintIdentInfo.StartCoord.Line,
        fHintIdentInfo.StartCoord.Char, ErrMsg);
      if Assigned(CE) then
      begin
        CodeHint := CE.CodeHint;
      end
      else
        CodeHint := '';
    end;
  end
  else
    CodeHint := '';
end;

procedure TEditorForm.CodeHintLinkHandler(Sender: TObject; LinkName: string);
begin
  CodeHint.CancelHint;
  PyIDEMainForm.JumpToFilePosInfo(LinkName);
  PyIDEMainForm.AdjustBrowserLists(fEditor.GetFileNameOrTitle,
    fHintIdentInfo.StartCoord.Line, fHintIdentInfo.StartCoord.Char, LinkName);
end;

procedure TEditorForm.DoOnIdle;
begin
  if not ReparseIfNeeded then
    SyncCodeExplorer;

  if (SynEdit.Highlighter = CommandsDataModule.SynPythonSyn) and
    fNeedToCheckSyntax and  PyIDEOptions.CheckSyntaxAsYouType and
    (SynEdit.Lines.Count <= PyIDEOptions.CheckSyntaxLineLimit)
  // do not syntax check very long files
  then
  begin
    InternalInterpreter.SyntaxCheck(GetEditor, True);
    fSyntaxErrorPos.Assign(PyControl.ErrorPos);
    PyControl.ErrorPos.Clear;
    fNeedToCheckSyntax := False;
  end;
  if HasSyntaxError then
    ParentTabItem.ImageIndex := 123
  else
    ParentTabItem.ImageIndex := -1;

  if not Application.Active then
  begin
    if SynCodeCompletion.Form.Visible then
      SynCodeCompletion.CancelCompletion;
    if SynParamCompletion.Form.Visible then
      SynParamCompletion.CancelCompletion;
  end;
end;

initialization

GI_EditorFactory := TEditorFactory.Create;

finalization

GI_EditorFactory := nil;

end.
