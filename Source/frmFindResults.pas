{-----------------------------------------------------------------------------
 Unit Name: frmFindResults
 Author:    Kiriakos Vlahos
 Date:      30-May-2005
 Purpose:   Find in files results window
 History:   Based on GExperts code (www.gexperts.org) and covered by its licence

GExperts License Agreement
GExperts is copyright 1996-2005 by GExperts, Inc, Erik Berry, and several other
authors who have submitted their code for inclusion. This license agreement only covers code written by GExperts, Inc and Erik Berry. You should contact the other authors concerning their respective copyrights and conditions.

The rules governing the use of GExperts and the GExperts source code are derived
from the official Open Source Definition, available at http://www.opensource.org.
The conditions and limitations are as follows:

    * Usage of GExperts binary distributions is permitted for all developers.
      You may not use the GExperts source code to develop proprietary or
      commercial products including plugins or libraries for those products.
      You may use the GExperts source code in an Open Source project, under the
      terms listed below.
    * You may not use the GExperts source code to create and distribute custom
      versions of GExperts under the "GExperts" name. If you do modify and
      distribute custom versions of GExperts, the binary distribution must be
      named differently and clearly marked so users can tell they are not using
      the official GExperts distribution. A visible and unmodified version of
      this license must appear in any modified distribution of GExperts.
    * Custom distributions of GExperts must include all of the custom changes
      as a patch file that can be applied to the original source code. This
      restriction is in place to protect the integrity of the original author's
      source code. No support for modified versions of GExperts will be provided
      by the original authors or on the GExperts mailing lists.
    * All works derived from GExperts must be distributed under a license
      compatible with this license and the official Open Source Definition,
      which can be obtained from http://www.opensource.org/.
    * Please note that GExperts, Inc. and the other contributing authors hereby
      state that this package is provided "as is" and without any express or
      implied warranties, including, but not without limitation, the implied
      warranties of merchantability and fitness for a particular purpose. In
      other words, we accept no liability for any damage that may result from
      using GExperts or programs that use the GExperts source code.

-----------------------------------------------------------------------------}

unit frmFindResults;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Actions,
  System.ImageList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.ActnList,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  JvAppStorage,
  JvDockControlForm,
  TB2Item,
  TB2Dock,
  TB2Toolbar,
  JvComponentBase,
  SpTBXSkins,
  SpTBXItem,
  SpTBXEditors,
  SpTBXDkPanels,
  SpTBXControls,
  SynEdit,
  SynEditMiscClasses,
  frmIDEDockWin,
  cFindInFiles;

type
  TFindResultsWindow = class(TIDEDockWindow)
    pnlMain: TPanel;
    Splitter: TSpTBXSplitter;
    TBXDock1: TSpTBXDock;
    ToolBar: TSpTBXToolbar;
    tbiRefresh: TSpTBXItem;
    tbiSearch: TSpTBXItem;
    TBXSeparatorItem1: TSpTBXSeparatorItem;
    tbiAbort: TSpTBXItem;
    TBXSeparatorItem2: TSpTBXSeparatorItem;
    tbiGoToSelected: TSpTBXItem;
    TBXSeparatorItem3: TSpTBXSeparatorItem;
    tbiSave: TSpTBXItem;
    tbiCopy: TSpTBXItem;
    tbiPrint: TSpTBXItem;
    TBXSeparatorItem4: TSpTBXSeparatorItem;
    tbiContract: TSpTBXItem;
    tbiExpand: TSpTBXItem;
    TBXSeparatorItem5: TSpTBXSeparatorItem;
    tbiReplaceAll: TSpTBXItem;
    tbiReplaceSelected: TSpTBXItem;
    TBXSeparatorItem6: TSpTBXSeparatorItem;
    tbiOptions: TSpTBXItem;
    TBXPopupMenu: TSpTBXPopupMenu;
    mitFileSearch1: TSpTBXItem;
    mitFileRefresh1: TSpTBXItem;
    mitFileAbort1: TSpTBXItem;
    N5: TSpTBXSeparatorItem;
    mitFilePrint1: TSpTBXItem;
    mitFileSave1: TSpTBXItem;
    N2: TSpTBXSeparatorItem;
    mitViewToolBar1: TSpTBXItem;
    StatusBar1: TSpTBXItem;
    miViewShowMatchContext1: TSpTBXItem;
    N1: TSpTBXSeparatorItem;
    mitReplaceReplaceAll1: TSpTBXItem;
    mitReplaceSelected1: TSpTBXItem;
    N3: TSpTBXSeparatorItem;
    mitViewOptions1: TSpTBXItem;
    N4: TSpTBXSeparatorItem;
    TBXSeparatorItem8: TSpTBXSeparatorItem;
    tbiHelp: TSpTBXItem;
    mitHelp: TSpTBXItem;
    StatusBar: TSpTBXStatusBar;
    StatusLeftLabel: TSpTBXLabelItem;
    SpTBXRightAlignSpacerItem1: TSpTBXRightAlignSpacerItem;
    StatusRightLabel: TSpTBXLabelItem;
    lbResults: TSpTBXListBox;
    Actions: TActionList;
    actReplaceAll: TAction;
    actViewOptions: TAction;
    actViewStatusBar: TAction;
    actViewToolBar: TAction;
    actFileCopy: TAction;
    actFileSave: TAction;
    actViewShowContext: TAction;
    actHelpHelp: TAction;
    actListExpand: TAction;
    actListContract: TAction;
    actListGotoSelected: TAction;
    actFilePrint: TAction;
    actFileAbort: TAction;
    actFileRefresh: TAction;
    actFileSearch: TAction;
    actReplaceSelected: TAction;
    vilImages: TVirtualImageList;
    reContext: TSynEdit;
    procedure FormResize(Sender: TObject);
    procedure lbResultsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbResultsKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure lbResultsDrawItem(Sender: TObject; ACanvas: TCanvas;
      var ARect: TRect; Index: Integer; const State: TOwnerDrawState;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
    procedure actFileSearchExecute(Sender: TObject);
    procedure actFileRefreshExecute(Sender: TObject);
    procedure actFileAbortExecute(Sender: TObject);
    procedure actFilePrintExecute(Sender: TObject);
    procedure actFileCopyExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actListGotoSelectedExecute(Sender: TObject);
    procedure actListContractExecute(Sender: TObject);
    procedure actListExpandExecute(Sender: TObject);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure lbResultsClick(Sender: TObject);
    procedure actViewShowContextExecute(Sender: TObject);
    procedure actViewToolBarExecute(Sender: TObject);
    procedure actViewOptionsExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actReplaceAllExecute(Sender: TObject);
    procedure actReplaceSelectedExecute(Sender: TObject);
    procedure actViewStatusBarExecute(Sender: TObject);
    procedure actHelpHelpExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure reContextSpecialLineColors(Sender: TObject; Line: Integer; var
        Special: Boolean; var FG, BG: TColor);
  private
    FSearchInProgress: Boolean;
    FReplaceInProgress: Boolean;
    FGrepSettings: TGrepSettings;
    FSearcher: TGrepSearchRunner;
    FShowContext: Boolean;
    FDoSearchReplace: Boolean;
    FSearchResults : TStrings;
    FREMatchLineNo: Integer;
    procedure RefreshContextLines;
    procedure SetShowContext(Value: Boolean);
    procedure HighlightMemo(FileMatches: TFileResult; StartLine: Integer);
    procedure StartFileSearch(Sender: TObject; const FileName: string);
    procedure ToggleFileResultExpanded(ListBoxIndex: Integer);
    procedure ExpandList;
    procedure ContractList;
    procedure ResizeListBox;
    procedure GotoHighlightedListEntry;
    procedure ClearResultsListbox;
    function QueryUserForGrepOptions: Boolean;
    function QueryUserForReplaceOptions(const ReplaceInString: string): Boolean;
    procedure Abort;
    procedure SetStatusString(const StatusStr: string);
    procedure SetMatchString(const MatchStr: string);
    procedure ExpandOrContractList(Expand: Boolean);
  protected
    const FBasePath = 'Find in Files Results Options'; // Used for storing settings
    const FBoldIndicatorID: TGUID = '{D27761BF-2B7F-4D21-A398-B9BD29D8D6DA}';
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
    procedure AssignSettingsToForm;
  public
    FindInFilesExpert : TFindInFilesExpert;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(DoRefresh: Boolean);
    function DoingSearchOrReplace: Boolean;
    // Storage
    procedure StoreSettings(AppStorage: TJvCustomAppStorage); override;
    procedure RestoreSettings(AppStorage: TJvCustomAppStorage); override;

    property ShowContext: Boolean read FShowContext write SetShowContext;
    property DoSearchReplace: Boolean read FDoSearchReplace write FDoSearchReplace;
  end;

// Replace all matches in all files
function ReplaceAll(ResultList: TStrings; GrepSettings: TGrepSettings): Integer;
// Replace all matches in a single file
function ReplaceAllInFile(FileResult: TFileResult; GrepSettings: TGrepSettings): Integer;
// Replace all matches on a single line
function ReplaceLine(LineResult: TLineResult; GrepSettings: TGrepSettings): Integer;

type
  TGrepOutputMode = (grPrint, grCopy, grFile);

procedure PrintGrepResults(Owner: TWinControl; Results: TStrings;
  Where: TGrepOutputMode);

var
  FindResultsWindow: TFindResultsWindow  = nil;

implementation

uses
  System.UITypes,
  System.RegularExpressions,
  System.Math,
  Vcl.Themes,
  JclFileUtils,
  JvJVCLUtils,
  JvGnugettext,
  StringResources,
  dmResources,
  dlgFindInFiles,
  dlgReplaceInFiles,
  SynEditTypes,
  SynDWrite,
  uEditAppIntfs,
  uCommonFunctions;

{$R *.dfm}

procedure GoToMatchLine(MatchLine: TLineResult; SourceEditorInMiddle: Boolean);
var
  MatchFileName: string;
begin
  MatchFileName := TFileResult(MatchLine.Collection).FileName;
  GI_PyIDEServices.ShowFilePosition(MatchFileName, MatchLine.LineNo,
                       MatchLine.Matches[0].SPos,
                       MatchLine.Matches[0].EPos - MatchLine.Matches[0].SPos + 1,
                       SourceEditorInMiddle);
end;

{ TFindResultsWindow }

procedure TFindResultsWindow.FormResize(Sender: TObject);
begin
  inherited;
  FGPanel.Realign;
//  StatusBar.Panels.Items[0].Size := StatusBar.Width - 70;
  lbResults.Invalidate;
  lbResults.Refresh;
end;

procedure TFindResultsWindow.lbResultsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ClickedEntry: Integer;
begin
  if Button = mbLeft then
  begin
    ClickedEntry := lbResults.ItemAtPos(Point(X, Y), True);
    if (ClickedEntry <> -1) and
       (lbResults.Items.Objects[ClickedEntry] is TFileResult) then
    begin
      ToggleFileResultExpanded(ClickedEntry);
    end;
  end;
end;

procedure TFindResultsWindow.lbResultsKeyPress(Sender: TObject;
  var Key: Char);
begin
  case Key of

    '+', '-':
      ToggleFileResultExpanded(lbResults.ItemIndex);

    #13:
      GotoHighlightedListEntry;
  end;
end;

procedure TFindResultsWindow.FormActivate(Sender: TObject);
begin
  inherited;
  if lbResults.CanFocus then
    lbResults.SetFocus;
end;

procedure TFindResultsWindow.FormKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if Key = #27 then
    if FSearchInProgress then
      Self.Abort;
end;

function LeftTrimChars(var Value: string; const TrimChars: TSysCharSet = [#9, #32]): Integer;
begin
  Result := 0;
  while (Length(Value) > Result) and CharInSet(Value[Result+1], TrimChars) do
    Inc(Result);

  Delete(Value, 1, Result);
end;

procedure TFindResultsWindow.lbResultsDrawItem(Sender: TObject;
  ACanvas: TCanvas; var ARect: TRect; Index: Integer;
  const State: TOwnerDrawState; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
var
  TopColor: TColor;
  BottomColor: TColor;
  ResultsCanvas: TCanvas;
  c: Integer;
  p: Integer;
  i: Integer;
  TempString, S: string;
  sb: TColor;
  sf: TColor;
  nb: TColor;
  nf: TColor;
  MIndx: Integer; // Matches Index number
  AMatchResult: TMatchResult;
  ALineResult: TLineResult;
  FileString: string;
  FileResult: TFileResult;
begin
  if PaintStage <> pstPrePaint then Exit;
  PaintDefault := False;

  ResultsCanvas := lbResults.Canvas;
  TopColor := clBtnHighlight;
  BottomColor := clBtnShadow;

  if lbResults.Items.Objects[Index] is TFileResult then
  begin
    FileResult := TFileResult(lbResults.Items.Objects[Index]);

    // Paint an expandable search file header
    //ARect.Right := ARect.Right + 2;
    if odSelected in State then begin
      CurrentSkin.PaintBackground(ResultsCanvas, ARect, skncListItem, sknsChecked, True, True);
      ResultsCanvas.Font.Color := CurrentSkin.GetTextColor(nil, skncListItem, sknsChecked);
    end else begin
      CurrentSkin.PaintWindowFrame(ResultsCanvas, ARect, False, True, 0);
      ResultsCanvas.Font.Color := CurrentSkin.GetTextColor(nil, skncListItem, sknsNormal);
      Frame3D(ResultsCanvas, ARect, TopColor, BottomColor, 1);
    end;

    ResultsCanvas.Brush.Style := bsClear;
    i := ResultsCanvas.TextWidth('+');
    FileString := FileResult.RelativeFileName;
    ResultsCanvas.TextOut(ARect.Left + i + PPIScale(8), ARect.Top, FileString);
    //c:=ARect.Top+((ARect.Bottom-ARect.Top) div 2);

    if FileResult.Expanded then
      ResultsCanvas.TextOut(ARect.Left + PPIScale(3), ARect.Top, '-')
    else
      ResultsCanvas.TextOut(ARect.Left + PPIScale(3), ARect.Top, '+');

    TempString := Format(_(SItemMatch), [FileResult.TotalMatches]);

    p := ResultsCanvas.TextWidth(TempString) + PPIScale(10);
    if (ResultsCanvas.TextWidth(FileString) + i + PPIScale(10)) <= ARect.Right - p then
      ResultsCanvas.TextOut(lbResults.ClientWidth - p, ARect.Top, TempString);
  end
  else
  begin
    // Paint a search match line number and highlighted match
    ALineResult := lbResults.Items.Objects[Index] as TLineResult;

    if odSelected in State then
    begin
      nb := StyleServices.GetSystemColor(clHighlight);
      nf := StyleServices.GetSystemColor(clHighLightText);
      sb := StyleServices.GetSystemColor(clWindow);
      sf := StyleServices.GetSystemColor(clWindowText);
    end
    else
    begin
      sb := StyleServices.GetSystemColor(clHighlight);
      sf := StyleServices.GetSystemColor(clHighLightText);
      nb := StyleServices.GetSystemColor(clWindow);
      nf := StyleServices.GetSystemColor(clWindowText);
    end;

    ResultsCanvas.Brush.Color := nb;
    ResultsCanvas.Font.Color := nf;
    ResultsCanvas.FillRect(ARect);
    ResultsCanvas.TextOut(ARect.Left + PPIScale(10), ARect.Top + PPIScale(1), IntToStr(ALineResult.LineNo));

    TempString := lbResults.Items[Index];
    c := LeftTrimChars(TempString);

    p := ARect.Left + PPIScale(60);
    i := 1;
    for MIndx := 0 to ALineResult.Matches.Count-1 do begin
      AMatchResult := ALineResult.Matches[MIndx];
      ResultsCanvas.Font.Color := nf;
      ResultsCanvas.Brush.Color := nb;
      S := Copy(TempString, i, AMatchResult.SPos - c - i);
      ResultsCanvas.TextOut(p, ARect.Top + PPIScale(1), S);
      p := ResultsCanvas.PenPos.X;

      ResultsCanvas.Font.Color := sf;
      ResultsCanvas.Brush.Color := sb;
      S := Copy(TempString, AMatchResult.SPos - c, AMatchResult.EPos - AMatchResult.SPos + 1);
      ResultsCanvas.TextOut(p, ARect.Top + PPIScale(1), S);
      p := ResultsCanvas.PenPos.X;

      i := AMatchResult.EPos - c + 1;
    end;
    ResultsCanvas.Font.Color := nf;
    ResultsCanvas.Brush.Color := nb;
    S := Copy(TempString, i, Length(TempString) -  i + 1);
    ResultsCanvas.TextOut(p, ARect.Top + PPIScale(1), S);
  end;
end;

procedure TFindResultsWindow.actFileSearchExecute(Sender: TObject);
begin
  Execute(False);
end;

procedure TFindResultsWindow.actFileRefreshExecute(Sender: TObject);
begin
  Execute(True);
  RefreshContextLines;
end;

procedure TFindResultsWindow.actFileAbortExecute(Sender: TObject);
begin
  Self.Abort;
end;

procedure TFindResultsWindow.actFilePrintExecute(Sender: TObject);
begin
  PrintGrepResults(Self, lbResults.Items, grPrint);
end;

procedure TFindResultsWindow.actFileCopyExecute(Sender: TObject);
begin
  if (ActiveControl = reContext) and (reContext.SelLength > 0) then
    reContext.CopyToClipboard
  else
    PrintGrepResults(Self, lbResults.Items, grCopy);
end;

procedure TFindResultsWindow.actFileSaveExecute(Sender: TObject);
begin
  PrintGrepResults(Self, lbResults.Items, grFile);
end;

procedure TFindResultsWindow.actListGotoSelectedExecute(Sender: TObject);
begin
  GotoHighlightedListEntry;
end;

procedure TFindResultsWindow.actListContractExecute(Sender: TObject);
begin
  ContractList;
end;

procedure TFindResultsWindow.actListExpandExecute(Sender: TObject);
begin
  ExpandList;
end;

procedure TFindResultsWindow.ActionsUpdate(Action: TBasicAction;
  var Handled: Boolean);
var
  HaveItems: Boolean;
  Processing: Boolean;
begin
  HaveItems := (lbResults.Items.Count > 0);
  Processing := DoingSearchOrReplace;
  actFileSearch.Enabled := not Processing;
  actFileRefresh.Enabled := not Processing;
  actViewOptions.Enabled := not Processing;
  actFilePrint.Enabled := not Processing and HaveItems;
  actFileSave.Enabled := not Processing and HaveItems;
  actFileCopy.Enabled := not Processing and HaveItems;
  actListGotoSelected.Enabled := not Processing and HaveItems;
  actListContract.Enabled := not Processing and HaveItems;
  actListExpand.Enabled := not Processing and HaveItems;
  actFileAbort.Enabled := Processing;
  actViewShowContext.Checked := ShowContext;
  actViewToolBar.Checked := ToolBar.Visible;
  actViewStatusBar.Checked := StatusBar.Visible;
  actReplaceSelected.Enabled := not Processing and HaveItems;
  actReplaceAll.Enabled := not Processing and HaveItems;
end;

procedure TFindResultsWindow.lbResultsClick(Sender: TObject);
begin
  RefreshContextLines;
end;

procedure TFindResultsWindow.actViewShowContextExecute(Sender: TObject);
begin
  ShowContext := not ShowContext;
end;

procedure TFindResultsWindow.actViewToolBarExecute(Sender: TObject);
begin
  ToolBar.Visible := not ToolBar.Visible;
end;

procedure TFindResultsWindow.actViewStatusBarExecute(Sender: TObject);
begin
  StatusBar.Visible := not StatusBar.Visible;
end;

procedure TFindResultsWindow.actViewOptionsExecute(Sender: TObject);
begin
  FindInFilesExpert.Configure;
  AssignSettingsToForm;
  ResizeListBox;
  RefreshContextLines;
end;

procedure TFindResultsWindow.FormShow(Sender: TObject);
begin
  inherited;
  AssignSettingsToForm;
  ResizeListBox;
end;

procedure TFindResultsWindow.actReplaceAllExecute(Sender: TObject);
var
  TimeStart: TDateTime;
  MatchesFound: Integer;
  Cursor: IInterface;
begin
  Assert(not DoingSearchOrReplace);

  if not QueryUserForReplaceOptions(_(SAllMatchedFiles)) then
    Exit;

  FReplaceInProgress := True;
  try
    Cursor := WaitCursor;
    TimeStart := Now;
    SetStatusString('');
    MatchesFound := ReplaceAll(lbResults.Items, FGrepSettings);
    SetStatusString(Format(_(SGrepReplaceStats), [MatchesFound, (Now - TimeStart) * 24*60*60]));
  finally
    FReplaceInProgress := False;
  end;
  RefreshContextLines;
end;

procedure TFindResultsWindow.actReplaceSelectedExecute(Sender: TObject);
var
  TimeStart: TDateTime;
  MatchesFound: Integer;
  CurrentLine: TLineResult;
  ResultIndex: Integer;
  FileResult: TFileResult;
  MatchFile: string;
  ResultObject: TObject;
  Cursor: IInterface;
begin
  Assert(not DoingSearchOrReplace);

  ResultIndex := lbResults.ItemIndex;
  if ResultIndex < 0 then
    Exit;

  ResultObject := lbResults.Items.Objects[ResultIndex];
  FReplaceInProgress := True;
  Application.DoApplicationIdle;  // To update related actions
  Application.ProcessMessages;
  try
    SetStatusString('');
    if ResultObject is TFileResult then
    begin
      FileResult := TFileResult(ResultObject);
      if not QueryUserForReplaceOptions(FileResult.FileName) then
        Exit;
      Cursor := WaitCursor;
      TimeStart := Now;
      MatchesFound := ReplaceAllInFile(FileResult, FGrepSettings);
    end
    else if ResultObject is TLineResult then
    begin
      CurrentLine := ResultObject as TLineResult;
      MatchFile := TFileResult(CurrentLine.Collection).FileName;
      if not QueryUserForReplaceOptions(MatchFile + sLineBreak + _(SOnLine) + IntToStr(CurrentLine.LineNo)) then
        Exit;
      Cursor := WaitCursor;
      TimeStart := Now;
      MatchesFound := ReplaceLine(CurrentLine, FGrepSettings);
    end
    else
      raise Exception.Create('Internal Error: Unknown result type');
    SetStatusString(Format(_(SGrepReplaceStats), [MatchesFound, (Now - TimeStart) * 24*60*60]));
  finally
    FReplaceInProgress := False;
  end;
  RefreshContextLines;
end;

procedure TFindResultsWindow.Abort;
begin
  if FSearcher <> nil then
    FSearcher.AbortSignalled := True;
end;

procedure TFindResultsWindow.AssignSettingsToForm;
begin
  Assert(Assigned(FindInFilesExpert));
  reContext.Font.Assign(FindInFilesExpert.ContextFont);
  reContext.Font.Color := StyleServices.GetSystemColor(clWindowText);
  reContext.Color := StyleServices.GetSystemColor(clWindow);
  lbResults.Font.Assign(FindInFilesExpert.ListFont);
  lbResults.Font.Color := StyleServices.GetSystemColor(clWindowText);
end;

procedure TFindResultsWindow.ClearResultsListbox;
var
  i: Integer;
begin
  if not (csDestroying in ComponentState) then  // Wierd crash on Exit
    lbResults.Clear;
  for i := 0 to FSearchResults.Count - 1 do
    if FSearchResults.Objects[i] is TFileResult then
      FSearchResults.Objects[i].Free;
  FSearchResults.Clear;
end;

procedure TFindResultsWindow.ContractList;
begin
  ExpandOrContractList(False);
end;

constructor TFindResultsWindow.Create(AOwner: TComponent);
begin
  ImageName := 'FindResults';
  inherited;
  FSearchResults := TStringList.Create;
  FSearchInProgress := False;
//  lbResults.DoubleBuffered := True;
  ShowContext := True;
  ResizeListBox;
  FindInFilesExpert := TFindInFilesExpert.Create;
  var FBoldIndicatorSpec := TSynIndicatorSpec.Create(sisTextDecoration,
    clNoneF, clNoneF, [fsBold]);
  reContext.Indicators.RegisterSpec(FBoldIndicatorId, FBoldIndicatorSpec);
end;

destructor TFindResultsWindow.Destroy;
begin
  Self.Abort;

  ClearResultsListbox;

  FSearchResults.Free;
  inherited Destroy;

  FindResultsWindow := nil;
  FreeAndNil(FindInFilesExpert);
end;

function TFindResultsWindow.DoingSearchOrReplace: Boolean;
begin
  Result := FSearchInProgress or FReplaceInProgress;
end;

procedure TFindResultsWindow.Execute(DoRefresh: Boolean);
var
  TimeStart: TDateTime;
  FilesSearched: Cardinal;
  MatchesFound: Cardinal;
  //Cursor: IInterface;
begin
  if DoingSearchOrReplace then begin
    StyledMessageDlg(_(SGrepActive), mtError, [mbOK], 0);
    Exit;
  end;

  if not (DoRefresh and FGrepSettings.CanRefresh) then
    if not QueryUserForGrepOptions then
      Exit;

  reContext.Clear;

  SetStatusString('');
  SetMatchString('');
  ClearResultsListbox;

  ShowDockForm(Self);

  TimeStart := Now;
  //Cursor := WaitCursor;
  FSearcher := TGrepSearchRunner.Create(FGrepSettings, lbResults.Items);
  try
    FSearcher.OnSearchFile := StartFileSearch;
    FSearchInProgress := True;
    Application.DoApplicationIdle;  // To update related actions
    Application.ProcessMessages;
    FSearcher.Execute;
    FilesSearched := FSearcher.FileSearchCount;
    MatchesFound := FSearcher.MatchCount;
  finally
    FreeAndNil(FSearcher);
    FSearchInProgress := False;
    FSearchResults.Assign(lbResults.Items);
  end;

  SetStatusString(Format(_(SGrepSearchStats), [FilesSearched, (Now - TimeStart) * 24*60*60]));

  lbResults.Sorted := True;  // There is no Sort method
  lbResults.Sorted := False;
  if (lbResults.Items.Count = 1) or FindInFilesExpert.GrepExpandAll then
  begin
    lbResults.ItemIndex := 0;
    ExpandList;
  end;

  lbResults.Refresh;
  SetMatchString(Format(_(SMatches), [MatchesFound]));
end;

procedure TFindResultsWindow.ExpandList;
begin
  ExpandOrContractList(True);
end;

procedure TFindResultsWindow.ExpandOrContractList(Expand: Boolean);

  function ExpandFileResult(ListBoxIndex: Integer): Integer;
  var
    FileResult: TFileResult;
    t: Integer;
  begin
    FileResult := lbResults.Items.Objects[ListBoxIndex] as TFileResult;

    for t := FileResult.Count - 1 downto 0 do
      lbResults.Items.InsertObject(ListBoxIndex + 1, FileResult.Items[t].Line, FileResult.Items[t]);

    FileResult.Expanded := True;
    Result := ListBoxIndex + FileResult.Count - 1;
  end;

var
  i: Integer;
begin
  lbResults.Items.BeginUpdate;
  try
    RefreshContextLines;

    i := 0;
    while i <= lbResults.Items.Count - 1 do
    begin
      if Expand then
      begin
        if lbResults.Items.Objects[i] is TFileResult then
        begin
          if not TFileResult(lbResults.Items.Objects[i]).Expanded then
            i := ExpandFileResult(i);
        end;

        Inc(i);
      end
      else // Contract
      begin
       if lbResults.Items.Objects[i] is TLineResult then
          lbResults.Items.Delete(i)
        else
        begin
          (lbResults.Items.Objects[i] as TFileResult).Expanded := False;
          Inc(i);
        end;
      end;
    end;
  finally
    lbResults.Items.EndUpdate;
  end;
end;

procedure TFindResultsWindow.GotoHighlightedListEntry;
var
  CurrentLine: TLineResult;
  ResultIndex: Integer;
begin
  ResultIndex := lbResults.ItemIndex;
  if ResultIndex < 0 then
    Exit;

  if lbResults.Items.Objects[ResultIndex] is TFileResult then
  begin
    ToggleFileResultExpanded(ResultIndex);
    Exit;
  end;

  CurrentLine := lbResults.Items.Objects[ResultIndex] as TLineResult;
  if CurrentLine = nil then
    Exit;

  GoToMatchLine(CurrentLine, FindInFilesExpert.GrepMiddle);
end;

procedure TFindResultsWindow.HighlightMemo(FileMatches: TFileResult; StartLine: Integer);
var
  Matches: TMatchArray;
begin
  for var I := StartLine + 1 to StartLine + reContext.Lines.Count do
  begin
    FileMatches.GetMatchesOnLine(I, Matches);
    for var Match in Matches do
    begin
      if Match.ShowBold then
      begin
        var Indicator := TSynIndicator.Create(FBoldIndicatorID, Match.SPos, Match.EPos + 1);
        reContext.Indicators.Add(I - StartLine, Indicator);
      end;
    end;
  end;
  reContext.CaretXY := BufferCoord(1, 1);
end;

function TFindResultsWindow.QueryUserForGrepOptions: Boolean;
var
  Dlg: TFindInFilesDialog;
begin
  Result := False;
  Dlg := TFindInFilesDialog.Create(nil);
  try
    if Dlg.ShowModal <> mrOk then
      Exit;
    FGrepSettings.CanRefresh := True;
    SetMatchString('');
    Dlg.RetrieveSettings(FGrepSettings);
    Result := True;
  finally
    FreeAndNil(Dlg);
  end;
end;

function TFindResultsWindow.QueryUserForReplaceOptions(
  const ReplaceInString: string): Boolean;
var
  Dlg: TReplaceInFilesDialog;
begin
  Result := False;
  Dlg := TReplaceInFilesDialog.Create(nil);
  try
    Dlg.ReplaceInString := ReplaceInString;
    Dlg.SearchString := FGrepSettings.Pattern;
    if Dlg.ShowModal <> mrOk then
      Exit;
    SetMatchString('');
    Dlg.RetrieveSettings(FGrepSettings);
    Result := True;
  finally
    FreeAndNil(Dlg);
  end;
end;

function GetFileAsText(const FileName: string; Lines: TStrings): Boolean;
Var
  Editor : IEditor;
begin
  Editor := GI_EditorFactory.GetEditorByFileId(FileName);
  if Assigned(Editor) then begin
    Lines.Assign(Editor.SynEdit.Lines);
    Result := True;
  end else
    Result := LoadFileIntoWideStrings(FileName, Lines);
end;

procedure TFindResultsWindow.RefreshContextLines;
var
  CurrentLine: TLineResult;
  MatchLineNo, BeginLineNo, EndLineNo: Integer;
  FileLines: TStringList;
  FileName: string;
  i: Integer;
begin
  if not ShowContext then
    Exit;

  reContext.Lines.BeginUpdate;
  try
    reContext.Clear;
    if (lbResults.ItemIndex < 0) then
      Exit;
    if (ShowContext) and (FindInFilesExpert.NumContextLines > 0) then
    begin
      if (lbResults.Items.Objects[lbResults.ItemIndex] is TLineResult) then
      begin
        CurrentLine := TLineResult(lbResults.Items.Objects[lbResults.ItemIndex]);
        FileName := TFileResult(CurrentLine.Collection).FileName;
        MatchLineNo := CurrentLine.LineNo - 1;

        FileLines := TStringList.Create;
        try
          if not GetFileAsText(FileName, FileLines) then
          begin
            reContext.Lines.Text := _(SMatchContextNotAvail);
            Exit;
          end;

          BeginLineNo := MatchLineNo - FindInFilesExpert.NumContextLines;
          BeginLineNo := Max(BeginLineNo, 0);
          EndLineNo := MatchLineNo + FindInFilesExpert.NumContextLines;
          EndLineNo := Min(EndLineNo, FileLines.Count - 1);

          FREMatchLineNo := 0;
          //reContext.SelStart := reContext.GetTextLen;
          reContext.Lines.Clear;
          for i := BeginLineNo to EndLineNo do
          begin
            reContext.Lines.Add(FileLines[i]);
            if i = MatchLineNo then
              FREMatchLineNo := reContext.Lines.Count;
          end;
        finally
          FreeAndNil(FileLines);
        end;
        HighlightMemo(TFileResult(CurrentLine.Collection), BeginLineNo);
      end;
    end;
  finally
    reContext.Lines.EndUpdate;
  end;
end;

procedure TFindResultsWindow.ResizeListBox;
Const
  SAllAlphaNumericChars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890';
begin
  lbResults.Canvas.Font.PixelsPerInch := FCurrentPPI;
  lbResults.Canvas.Font.Assign(lbResults.Font);
  lbResults.ItemHeight := lbResults.Canvas.TextHeight(SAllAlphaNumericChars) +
    MulDiv(3, FCurrentPPI, Screen.DefaultPixelsPerInch);
  lbResults.Refresh;
end;

procedure TFindResultsWindow.SetMatchString(const MatchStr: string);
begin
  StatusRightLabel.Caption := MatchStr;
end;

procedure TFindResultsWindow.SetShowContext(Value: Boolean);
begin
  FShowContext := Value;
  reContext.Visible := ShowContext;
  Splitter.Visible := ShowContext;
  RefreshContextLines;
end;

procedure TFindResultsWindow.SetStatusString(const StatusStr: string);
begin
  StatusLeftLabel.Caption := StatusStr;
end;

procedure TFindResultsWindow.StartFileSearch(Sender: TObject;
  const FileName: string);
var
  Dummy: Boolean;
begin
  SetStatusString(Format(_(SProcessing), [FileName]));
  ActionsUpdate(nil, Dummy);
  Application.ProcessMessages;
end;

procedure TFindResultsWindow.ToggleFileResultExpanded(
  ListBoxIndex: Integer);
var
  AFileResult: TFileResult;
  i: Integer;
begin
  if FSearchInProgress or
     (ListBoxIndex < 0) or (ListBoxIndex >= lbResults.Items.Count) then
  begin
    Exit;
  end;

  if lbResults.Items.Objects[ListBoxIndex] is TFileResult then
  begin
    AFileResult := TFileResult(lbResults.Items.Objects[ListBoxIndex]);

    lbResults.Items.BeginUpdate;
    try
      if AFileResult.Expanded then
      begin
        while (ListBoxIndex + 1 <= lbResults.Items.Count - 1) and
              (not (lbResults.Items.Objects[ListBoxIndex + 1] is TFileResult)) do
        begin
          lbResults.Items.Delete(ListBoxIndex + 1);
        end;
        AFileResult.Expanded := False;
      end
      else
      begin
        for i := AFileResult.Count - 1 downto 0 do
          lbResults.Items.InsertObject(ListBoxIndex + 1, AFileResult.Items[i].Line, AFileResult.Items[i]);
        AFileResult.Expanded := True;
      end
    finally
      lbResults.Items.EndUpdate;
    end;
  end;
end;

type
  ESkipFileReplaceException = class(Exception);

// Replaces the string between SPos and EPos with the replace string from TGrepSettings
function ReplacePatternInString(CurrentLine: TLineResult; GrepSettings: TGrepSettings; RegEx: TRegEx): string;
var
  i: Integer;
  FindPos: Integer;
  FindLen: Integer;
  CurrentMatch: TMatchResult;
begin
  if GrepSettings.RegEx then begin
    Result := RegEx.Replace(CurrentLine.Line, GrepSettings.Replace);
   	for i := CurrentLine.Matches.Count - 1 downto 0 do
     	CurrentLine.Matches[i].ShowBold := False;
  end else begin
    Result := CurrentLine.Line;
    for i := CurrentLine.Matches.Count - 1 downto 0 do
    begin
      CurrentMatch := CurrentLine.Matches.Items[i];
      FindPos := CurrentMatch.SPos;
      FindLen := CurrentMatch.EPos - CurrentMatch.SPos + 1;
      Delete(Result, FindPos, FindLen);
      Insert(GrepSettings.Replace, Result, FindPos);
      CurrentMatch.ShowBold := False;
    end;
  end;
end;

function ReplaceAll(ResultList: TStrings; GrepSettings: TGrepSettings): Integer;
var
  i: Integer;
  Replaced: Integer;
begin
  Result := 0;
  for i := 0 to ResultList.Count - 1 do
  begin
    if ResultList.Objects[i] is TFileResult then
     begin
       Replaced := ReplaceAllInFile(ResultList.Objects[i] as TFileResult, GrepSettings);
       Inc(Result, Replaced);
     end;
  end;
end;

function InternalReplace(LineMode: Boolean; ALineResult: TLineResult; AFileResult: TFileResult; GrepSettings: TGrepSettings): Integer;
var
  TempString: string;
  MatchFile: string;
  TempFile: TStrings;
  LineResult : TLineResult;
  RegEx: TRegEx;
  Options: TRegExOptions;

  procedure DoReplacement;
  var
    i: Integer;
    FileLine: string;
  begin
    if LineMode then
    begin
      i := ALineResult.LineNo;
        Assert(TempFile.Count >= (LineResult.LineNo - 1));
      FileLine := TempFile[LineResult.LineNo - 1];
      if LineResult.Line <> FileLine then begin
        StyledMessageDlg(Format(_(SFileChangedAbort), [MatchFile, LineResult.Line, FileLine]),
          mtError, [mbAbort], 0);
        Abort;
      end;

      TempString := ReplacePatternInString(LineResult, GrepSettings, RegEx);
      TempFile.Strings[i -1] := TempString;
      Inc(Result, LineResult.Matches.Count);
    end
    else
    begin
      for i := AFileResult.Count - 1 downto 0 do
      begin
        LineResult := AFileResult.Items[i];
        Inc(Result, LineResult.Matches.Count);
        Assert(TempFile.Count >= (LineResult.LineNo - 1));
        FileLine := TempFile[LineResult.LineNo - 1];
        if LineResult.Line <> FileLine then begin
          StyledMessageDlg(Format(_(SFileChangedAbort), [MatchFile, LineResult.Line, FileLine]),
            mtError, [mbAbort], 0);
          Abort;
        end;

        TempString := ReplacePatternInString(LineResult, GrepSettings, RegEx);
        TempFile.Strings[LineResult.LineNo - 1] := TempString;
      end;
    end;
  end;

  procedure WriteResults;
  var
    Editor : IEditor;
    OldCaretXY : TBufferCoord;
  begin
    Editor := GI_EditorFactory.GetEditorByFileId(MatchFile);
    if Assigned(Editor) then with Editor.SynEdit do begin
      //  We replace selection so that changes can be undone
      OldCaretXY := CaretXY;
      SelectAll;
      SelText := TempFile.Text;
      CaretXY := OldCaretXY;
    end else begin
      if GrepSettings.BackupModified then
        try
          FileBackup(MatchFile);
        except
          if StyledMessageDlg(Format(_(SCouldNotBackup), [MatchFile]), mtWarning, [mbOK, mbCancel], 0) = mrCancel then
            Abort
          else
            Exit;
        end;
      SaveWideStringsToFile(MatchFile, TempFile, False);
    end;
  end;

begin
  Result := 0;
  if LineMode then
  begin
    LineResult := ALineResult;
    MatchFile := TFileResult(LineResult.Collection).FileName;
  end
  else
    MatchFile := AFileResult.FileName;

  TempFile := TXStringList.Create;
  try
    if GrepSettings.RegEx then
    begin
      Options := [roNotEmpty];
      if not GrepSettings.CaseSensitive then
        Include(Options, roIgnoreCase);
      RegEx := CompiledRegEx(GrepSettings.Pattern, Options);
    end;

    if not GetFileAsText(MatchFile, TempFile) then begin
      if StyledMessageDlg(_(SFileSkipped) + MatchFile, mtWarning, [mbOK, mbCancel], 0) = mrCancel then
        Abort
      else
        Exit;
    end;
    DoReplacement;
    WriteResults;
  finally
    FreeAndNil(TempFile);
  end;
end;

function ReplaceAllInFile(FileResult: TFileResult; GrepSettings: TGrepSettings): Integer;
begin
  Result := InternalReplace(False, nil, FileResult, GrepSettings);
end;

function ReplaceLine(LineResult: TLineResult; GrepSettings: TGrepSettings): Integer;
begin
  Result := InternalReplace(True, LineResult, nil, GrepSettings);
end;

procedure SaveResults(RichEdit: TRichEdit);
var
  SaveDlg: TSaveDialog;
begin
  RichEdit.PlainText := True;
  SaveDlg := TSaveDialog.Create(nil);
  try
    SaveDlg.DefaultExt := 'txt';
    SaveDlg.Filter := 'Text Files (*.txt, *.log)|*.txt;*.log|All Files (*.*)|*.*';
    SaveDlg.Options := SaveDlg.Options + [ofOverwritePrompt];
    if SaveDlg.Execute then
      RichEdit.Lines.SaveToFile(SaveDlg.FileName);
  finally
    FreeAndNil(SaveDlg);
  end;
end;

procedure PrintGrepResults(Owner: TWinControl; Results: TStrings; Where: TGrepOutputMode);
var
  RichEdit: TRichEdit;
  FileResult: TFileResult;
  Line: string;
  i, j, c: Integer;
  LinePos: Integer;
  AMatchResult: TMatchResult;
  MIndx: Integer;
begin
  RichEdit := TRichEdit.Create(Owner);
  try
    RichEdit.Visible := False;
    RichEdit.Parent := Owner;
    RichEdit.Font.Name := 'Arial';
    RichEdit.Font.Size := 10;
    RichEdit.Clear;
    RichEdit.Lines.BeginUpdate;
    try
      for i := 0 to Results.Count - 1 do
      begin
        if Results.Objects[i] is TFileResult then
        begin
          RichEdit.Lines.Add('');  // space between file results

          FileResult := TFileResult(Results.Objects[i]);

          RichEdit.SelAttributes.Style := [fsBold];
          RichEdit.Lines.Add(FileResult.FileName);
          RichEdit.SelAttributes.Style := [];

          for j := 0 to FileResult.Count - 1 do
          begin
            LinePos := RichEdit.GetTextLen;
            Line := FileResult.Items[j].Line;
            c := LeftTrimChars(Line);
            with RichEdit do
            begin
              Lines.Add(Format('  %5d'#9, [FileResult.Items[j].LineNo]) + Line);
              // Now make the found Text bold
              for MIndx := 0 to  FileResult.Items[j].Matches.Count-1 do
              begin
                AMatchResult := FileResult.Items[j].Matches[MIndx];
                SelStart := LinePos + 7 - c + AMatchResult.SPos;
                SelLength := AMatchResult.EPos - AMatchResult.SPos + 1;
                SelAttributes.Style := [fsBold];
                SelLength := 0;
                SelAttributes.Style := [];
              end;
            end;
          end;
        end;
      end;
    finally
      RichEdit.Lines.EndUpdate;
    end;
    case Where of
      grPrint: RichEdit.Print('GExperts - Grep Search Results');
      grCopy:
        begin
          RichEdit.SelectAll;
          RichEdit.CopyToClipboard;
        end;
      grFile: SaveResults(RichEdit);
    end;
  finally
    FreeAndNil(RichEdit);
  end;
end;

procedure TFindResultsWindow.RestoreSettings(AppStorage: TJvCustomAppStorage);
begin
  inherited;
  lbResults.Height :=
    PPIScale(AppStorage.ReadInteger(FBasePath+'\ResultsHeight', lbResults.Height));
  ToolBar.Visible := AppStorage.ReadBoolean(FBasePath+'\ShowToolBar', ToolBar.Visible);
  StatusBar.Visible := AppStorage.ReadBoolean(FBasePath+'\ShowStatusBar', StatusBar.Visible);
  ShowContext := AppStorage.ReadBoolean(FBasePath+'\ShowContext', True);
  AppStorage.ReadPersistent('Find in Files Options', FindInFilesExpert);
end;

procedure TFindResultsWindow.StoreSettings(AppStorage: TJvCustomAppStorage);
begin
  inherited;
  AppStorage.WriteInteger(FBasePath+'\ResultsHeight', PPIUnScale(lbResults.Height));
  AppStorage.WriteBoolean(FBasePath+'\ShowToolBar', ToolBar.Visible);
  AppStorage.WriteBoolean(FBasePath+'\ShowStatusBar', StatusBar.Visible);
  AppStorage.WriteBoolean(FBasePath+'\ShowContext', ShowContext);
  AppStorage.WritePersistent('Find in Files Options', FindInFilesExpert);
end;

procedure TFindResultsWindow.WMSpSkinChange(var Message: TMessage);
begin
  inherited;
  reContext.Font.Color := StyleServices.GetSystemColor(clWindowText);
  reContext.Color := StyleServices.GetSystemColor(clWindow);
  lbResults.Invalidate;
end;

procedure TFindResultsWindow.actHelpHelpExecute(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TFindResultsWindow.reContextSpecialLineColors(Sender: TObject; Line:
    Integer; var Special: Boolean; var FG, BG: TColor);
begin
  BG := clNone;
  Special :=  Line = FREMatchLineNo;
  if Special then
    FG := StyleServices.GetSystemColor(FindInFilesExpert.ContextMatchColor)
  else
    FG := clNone;
end;

end.
