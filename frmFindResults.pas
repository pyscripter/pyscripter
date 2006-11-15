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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmIDEDockWin, JvComponent, JvDockControlForm, ExtCtrls, Menus,
  ActnList, ComCtrls, StdCtrls, cFindInFiles, JvAppStorage,
  TB2Item, TBX, TBXThemes, TB2Dock, TB2Toolbar, TBXStatusBars, JvComponentBase;

type
  TFindResultsWindow = class(TIDEDockWindow, IJvAppStorageHandler)
    pnlMain: TPanel;
    Splitter: TSplitter;
    lbResults: TListBox;
    reContext: TRichEdit;
    Actions: TActionList;
    actReplaceSelected: TAction;
    actFileSearch: TAction;
    actFileRefresh: TAction;
    actFileAbort: TAction;
    actFilePrint: TAction;
    actViewStayOnTop: TAction;
    actListGotoSelected: TAction;
    actListContract: TAction;
    actListExpand: TAction;
    actHelpHelp: TAction;
    actViewShowContext: TAction;
    actFileSave: TAction;
    actFileCopy: TAction;
    actViewToolBar: TAction;
    actViewOptions: TAction;
    actReplaceAll: TAction;
    actViewStatusBar: TAction;
    TBXDock1: TTBXDock;
    ToolBar: TTBXToolbar;
    TBXItem1: TTBXItem;
    TBXItem2: TTBXItem;
    TBXSeparatorItem1: TTBXSeparatorItem;
    TBXItem3: TTBXItem;
    TBXSeparatorItem2: TTBXSeparatorItem;
    TBXItem4: TTBXItem;
    TBXSeparatorItem3: TTBXSeparatorItem;
    TBXItem5: TTBXItem;
    TBXItem6: TTBXItem;
    TBXItem7: TTBXItem;
    TBXSeparatorItem4: TTBXSeparatorItem;
    TBXItem8: TTBXItem;
    TBXItem9: TTBXItem;
    TBXSeparatorItem5: TTBXSeparatorItem;
    TBXItem10: TTBXItem;
    TBXItem11: TTBXItem;
    TBXSeparatorItem6: TTBXSeparatorItem;
    TBXItem12: TTBXItem;
    TBXSeparatorItem7: TTBXSeparatorItem;
    TBXItem13: TTBXItem;
    TBXPopupMenu: TTBXPopupMenu;
    mitFileSearch1: TTBXItem;
    mitFileRefresh1: TTBXItem;
    mitFileAbort1: TTBXItem;
    N5: TTBXSeparatorItem;
    mitFilePrint1: TTBXItem;
    mitFileSave1: TTBXItem;
    N2: TTBXSeparatorItem;
    mitViewToolBar1: TTBXItem;
    StatusBar1: TTBXItem;
    miViewShowMatchContext1: TTBXItem;
    N1: TTBXSeparatorItem;
    mitReplaceReplaceAll1: TTBXItem;
    mitReplaceSelected1: TTBXItem;
    N3: TTBXSeparatorItem;
    mitViewOptions1: TTBXItem;
    N4: TTBXSeparatorItem;
    mitViewStayOnTop1: TTBXItem;
    StatusBar: TTBXStatusBar;
    TBXSeparatorItem8: TTBXSeparatorItem;
    TBXItem14: TTBXItem;
    TBXSeparatorItem9: TTBXSeparatorItem;
    TBXItem15: TTBXItem;
    procedure FormResize(Sender: TObject);
    procedure lbResultsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbResultsKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure lbResultsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure actFileSearchExecute(Sender: TObject);
    procedure actFileRefreshExecute(Sender: TObject);
    procedure actFileAbortExecute(Sender: TObject);
    procedure actFilePrintExecute(Sender: TObject);
    procedure actFileCopyExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actViewStayOnTopExecute(Sender: TObject);
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
    procedure FormActivate(Sender: TObject);
    procedure actViewStatusBarExecute(Sender: TObject);
    procedure actHelpHelpExecute(Sender: TObject);
  private
    FSearchInProgress: Boolean;
    FReplaceInProgress: Boolean;
    FGrepSettings: TGrepSettings;
    FSearcher: TGrepSearchRunner;
    FShowContext: Boolean;
    FDoSearchReplace: Boolean;
    fSearchResults : TStrings;
    procedure RefreshContextLines;
    procedure SetShowContext(Value: Boolean);
    procedure HighlightMemo(FileMatches: TFileResult; StartLine, MatchLineNo: Integer);
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
    function DoingSearchOrReplace: Boolean;
    procedure ExpandOrContractList(Expand: Boolean);
  protected
    procedure TBMThemeChange(var Message: TMessage); message TBM_THEMECHANGE;
    procedure AssignSettingsToForm;
    // IJvAppStorageHandler implementation
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
  public
    FindInFilesExpert : TFindInFilesExpert;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(DoRefresh: Boolean);
    property ShowContext: Boolean read FShowContext write SetShowContext;
    property DoSearchReplace: Boolean read FDoSearchReplace write FDoSearchReplace;
  end;

// Replace all matches in all files
function ReplaceAll(ResultList: TStrings; GrepSettings: TGrepSettings): Integer;
// Replace all matches in a single file
function ReplaceAllInFiles(FileResult: TFileResult; GrepSettings: TGrepSettings): Integer;
// Replace all matches on a single line
function ReplaceLine(LineResult: TLineResult; GrepSettings: TGrepSettings): Integer;

type
  TGrepOutputMode = (grPrint, grCopy, grFile);

procedure PrintGrepResults(Owner: TWinControl; Results: TStrings; Where: TGrepOutputMode);

var
  FindResultsWindow: TFindResultsWindow  = nil;

implementation

uses dmCommands, dlgFindInFiles, Math, frmPyIDEMain, uEditAppIntfs,
  dlgReplaceInFiles, SynEdit, SynEditTypes, JclFileUtils, uCommonFunctions,
  JvJVCLUtils, JvDockGlobals;

{$R *.dfm}

resourcestring
  SGrepReplaceStats = 'Replaced %d occurrence(s) in %.2f seconds';

procedure GoToMatchLine(MatchLine: TLineResult; SourceEditorInMiddle: Boolean);
var
  MatchFileName: string;
begin
  MatchFileName := TFileResult(MatchLine.Collection).FileName;
  PyIDEMainForm.ShowFilePosition(MatchFileName, MatchLine.LineNo,
                       MatchLine.Matches[0].SPos, SourceEditorInMiddle);
end;

{ TFindResultsWindow }

procedure TFindResultsWindow.FormResize(Sender: TObject);
begin
  inherited;
  FGPanel.Realign;
//  StatusBar.Panels.Items[0].Size := StatusBar.Width - 70;
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
  while (Length(Value) > Result) and (Value[Result+1] in TrimChars) do
    Inc(Result);

  Delete(Value, 1, Result);
end;

procedure TFindResultsWindow.lbResultsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
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
resourcestring
  //SItemMatch = '%5d matches on %5d lines';
  SItemMatch = '%5d matches';
begin
  ResultsCanvas := lbResults.Canvas;
  TopColor := clBtnHighlight;
  BottomColor := clBtnShadow;

  if lbResults.Items.Objects[Index] is TFileResult then
  begin
    FileResult := TFileResult(lbResults.Items.Objects[Index]);
    // Paint an expandable search file header (gray)

    CurrentTheme.PaintBackgnd(ResultsCanvas, Rect, Rect, Rect,
      CurrentTheme.GetViewColor(TVT_NORMALTOOLBAR), false, VT_TOOLBAR);

    Rect.Right := Rect.Right + 2;
    if odSelected in State then begin
      ResultsCanvas.Font.Color := CurrentTheme.GetItemTextColor(GetItemInfo('active'));
      CurrentTheme.PaintFrame(ResultsCanvas, Rect, GetItemInfo('active'))
      //Frame3D(ResultsCanvas, Rect, BottomColor, TopColor, 1)
    end else begin
      ResultsCanvas.Font.Color := CurrentTheme.GetItemTextColor(GetItemInfo('inactive'));
      CurrentTheme.PaintFrame(ResultsCanvas, Rect, GetItemInfo('inactive'));
      Frame3D(ResultsCanvas, Rect, TopColor, BottomColor, 1);
    end;

    ResultsCanvas.Brush.Style := bsClear;
    i := ResultsCanvas.TextWidth('+');
    FileString := FileResult.RelativeFileName;
    ResultsCanvas.TextOut(Rect.Left + i + 8, Rect.Top, FileString);
    //c:=Rect.Top+((Rect.Bottom-Rect.Top) div 2);

    if FileResult.Expanded then
      ResultsCanvas.TextOut(Rect.Left + 3, Rect.Top, '-')
    else
      ResultsCanvas.TextOut(Rect.Left + 3, Rect.Top, '+');

    TempString := Format(SItemMatch, [FileResult.TotalMatches]);

    p := ResultsCanvas.TextWidth(TempString) + 10;
    if (ResultsCanvas.TextWidth(FileString) + i + 10) <= Rect.Right - p then
      ResultsCanvas.TextOut(lbResults.ClientWidth - p, Rect.Top, TempString);
  end
  else
  begin
    // Paint a search match line number and highlighted match
    ALineResult := lbResults.Items.Objects[Index] as TLineResult;

    if odSelected in State then
    begin
      nb := SelectionBackgroundColor;
      nf := clHighLightText;
      sb := clWindow;
      sf := clWindowText;
    end
    else
    begin
      sb := SelectionBackgroundColor;
      sf := clHighLightText;
      nb := clWindow;
      nf := clWindowText;
    end;

    ResultsCanvas.Brush.Color := nb;
    ResultsCanvas.Font.Color := nf;
    ResultsCanvas.FillRect(Rect);
    ResultsCanvas.TextOut(Rect.Left + 10, Rect.Top + 1, IntToStr(ALineResult.LineNo));

    TempString := lbResults.Items[Index];
    c := LeftTrimChars(TempString);

    p := Rect.Left + 60;
    i := 1;
    for MIndx := 0 to ALineResult.Matches.Count-1 do begin
      AMatchResult := ALineResult.Matches[MIndx];
      ResultsCanvas.Font.Color := nf;
      ResultsCanvas.Brush.Color := nb;
      S := Copy(TempString, i, AMatchResult.SPos - c - i);
      ResultsCanvas.TextOut(p, Rect.Top + 1, S);
      p := ResultsCanvas.PenPos.X;

      ResultsCanvas.Font.Color := sf;
      ResultsCanvas.Brush.Color := sb;
      S := Copy(TempString, AMatchResult.SPos - c, AMatchResult.EPos - AMatchResult.SPos + 1);
      ResultsCanvas.TextOut(p, Rect.Top + 1, S);
      p := ResultsCanvas.PenPos.X;

      i := AMatchResult.EPos - c + 1;
    end;
    ResultsCanvas.Font.Color := nf;
    ResultsCanvas.Brush.Color := nb;
    S := Copy(TempString, i, Length(TempString) -  i + 1);
    ResultsCanvas.TextOut(p, Rect.Top + 1, S);
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

procedure TFindResultsWindow.actViewStayOnTopExecute(Sender: TObject);
begin
  if FormStyle = fsNormal then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
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
  actViewStayOnTop.Enabled := not Processing;
  actFilePrint.Enabled := not Processing and HaveItems;
  actFileSave.Enabled := not Processing and HaveItems;
  actFileCopy.Enabled := not Processing and HaveItems;
  actListGotoSelected.Enabled := not Processing and HaveItems;
  actListContract.Enabled := not Processing and HaveItems;
  actListExpand.Enabled := not Processing and HaveItems;
  actFileAbort.Enabled := Processing;
  actViewStayOnTop.Checked := FormStyle = fsStayOnTop;
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
resourcestring
  SReplaceAllQuery = 'Replace "%s" with "%s" in all files?';
var
  TimeStart: TDateTime;
  MatchesFound: Integer;
  Cursor: IInterface;
begin
  Assert(not DoingSearchOrReplace);

  if not QueryUserForReplaceOptions('All matched files') then
    Exit;

  FReplaceInProgress := True;
  try
    Cursor := WaitCursor;
    TimeStart := Now;
    SetStatusString('');
    MatchesFound := ReplaceAll(lbResults.Items, FGrepSettings);
    SetStatusString(Format(SGrepReplaceStats, [MatchesFound, (Now - TimeStart) * 24*60*60]));
  finally
    FReplaceInProgress := False;
  end;
  RefreshContextLines;
end;

procedure TFindResultsWindow.actReplaceSelectedExecute(Sender: TObject);
resourcestring
  SReplaceLine = sLineBreak + 'On line: ';
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
  try
    SetStatusString('');
    if ResultObject is TFileResult then
    begin
      FileResult := TFileResult(ResultObject);
      if not QueryUserForReplaceOptions(FileResult.FileName) then
        Exit;
      Cursor := WaitCursor;
      TimeStart := Now;
      MatchesFound := ReplaceAllInFiles(FileResult, FGrepSettings);
    end
    else if ResultObject is TLineResult then
    begin
      CurrentLine := ResultObject as TLineResult;
      MatchFile := TFileResult(CurrentLine.Collection).FileName;
      if not QueryUserForReplaceOptions(MatchFile + SReplaceLine + IntToStr(CurrentLine.LineNo)) then
        Exit;
      Cursor := WaitCursor;
      TimeStart := Now;
      MatchesFound := ReplaceLine(CurrentLine, FGrepSettings);
    end
    else
      raise Exception.Create('Internal Error: Unknown result type');
    SetStatusString(Format(SGrepReplaceStats, [MatchesFound, (Now - TimeStart) * 24*60*60]));
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
  lbResults.Font.Assign(FindInFilesExpert.ListFont);
end;

procedure TFindResultsWindow.ClearResultsListbox;
var
  i: Integer;
begin
  lbResults.Clear;
  for i := 0 to fSearchResults.Count - 1 do
    if fSearchResults.Objects[i] is TFileResult then
      fSearchResults.Objects[i].Free;
  fSearchResults.Clear;
end;

procedure TFindResultsWindow.ContractList;
begin
  ExpandOrContractList(False);
end;

constructor TFindResultsWindow.Create(AOwner: TComponent);
begin
  inherited;
  fSearchResults := TStringList.Create;
  FSearchInProgress := False;
  //lbResults.DoubleBuffered := True;
  ShowContext := True;
  ResizeListBox;
end;

destructor TFindResultsWindow.Destroy;
begin
  Self.Abort;

  ClearResultsListbox;

  fSearchResults.Free;
  inherited Destroy;

  FindResultsWindow := nil;
end;

function TFindResultsWindow.DoingSearchOrReplace: Boolean;
begin
  Result := FSearchInProgress or FReplaceInProgress;
end;

procedure TFindResultsWindow.Execute(DoRefresh: Boolean);
resourcestring
  SGrepActive = 'A Grep search is currently active; either abort it or wait until it is finished.';
  SGrepSearchStats = 'Searched %d files in %.2f seconds';
  SMatches = '%d matches';
var
  TimeStart: TDateTime;
  FilesSearched: Cardinal;
  MatchesFound: Cardinal;
  //Cursor: IInterface;
begin
  if FSearchInProgress then
    raise Exception.Create(SGrepActive);

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
    FSearcher.Execute;
    FilesSearched := FSearcher.FileSearchCount;
    MatchesFound := FSearcher.MatchCount;
  finally
    FreeAndNil(FSearcher);
    FSearchInProgress := False;
    FSearchResults.Assign(lbResults.Items);
  end;

  SetStatusString(Format(SGrepSearchStats, [FilesSearched, (Now - TimeStart) * 24*60*60]));

  lbResults.Sorted := True;  // There is no Sort method
  lbResults.Sorted := False;
  if (lbResults.Items.Count = 1) or FindInFilesExpert.GrepExpandAll then
  begin
    lbResults.ItemIndex := 0;
    ExpandList;
  end;

  lbResults.Refresh;
  SetMatchString(Format(SMatches, [MatchesFound]));
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

procedure TFindResultsWindow.HighlightMemo(FileMatches: TFileResult;
  StartLine, MatchLineNo: Integer);
var
  Matches: TMatchArray;
  i, j: Integer;
begin
  reContext.SelStart := 0;
  reContext.SelLength := Length(reContext.Lines.Text);
  reContext.SelAttributes.Name := reContext.DefAttributes.Name;
  reContext.SelAttributes.Size := reContext.DefAttributes.Size;
  reContext.SelAttributes.Style := [];

  // Highlight the matched line
  reContext.SelStart := reContext.Perform(EM_LINEINDEX, MatchLineNo, 0);
  reContext.SelLength := Length(reContext.Lines[MatchLineNo]);
  reContext.SelAttributes.Color := FindInFilesExpert.ContextMatchColor;

  for i := StartLine + 1 to StartLine + reContext.Lines.Count + 1 do
  begin
    FileMatches.GetMatchesOnLine(i, Matches);
    for j := 0 to Length(Matches) - 1 do
    begin
      if Matches[j].ShowBold then
      begin
        reContext.SelStart := reContext.Perform(EM_LINEINDEX, i - StartLine - 1, 0) + Matches[j].SPos - 1;
        reContext.SelLength := Matches[j].EPos - Matches[j].SPos + 1;
        reContext.SelAttributes.Style := [fsBold];
      end;
    end;
  end;
  reContext.SelStart := 0;
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
  Editor := GI_EditorFactory.GetEditorByNameOrTitle(FileName);
  if Assigned(Editor) then begin
    Lines.Assign(Editor.SynEdit.Lines);
    Result := True;
  end else
    try
      Lines.LoadFromFile(FileName);
      Result := True;
    except
      Result := False;
    end;
end;

procedure TFindResultsWindow.RefreshContextLines;
resourcestring
  SMatchContextNotAvail = 'Unable to load match context lines';
var
  CurrentLine: TLineResult;
  MatchLineNo, BeginLineNo, EndLineNo, REMatchLineNo: Integer;
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
            reContext.Lines.Text := SMatchContextNotAvail;
            Exit;
          end;

          BeginLineNo := MatchLineNo - FindInFilesExpert.NumContextLines;
          BeginLineNo := Max(BeginLineNo, 0);
          EndLineNo := MatchLineNo + FindInFilesExpert.NumContextLines;
          EndLineNo := Min(EndLineNo, FileLines.Count - 1);

          REMatchLineNo := 0;
          reContext.SelStart := reContext.GetTextLen;
          for i := BeginLineNo to EndLineNo do
          begin
            reContext.SelText := FileLines[i] + sLineBreak;
            if i = MatchLineNo then
              REMatchLineNo := reContext.Lines.Count - 1;
          end;
        finally
          FreeAndNil(FileLines);
        end;
        HighlightMemo(TFileResult(CurrentLine.Collection), BeginLineNo, REMatchLineNo);
      end;
    end;
  finally
    reContext.Lines.EndUpdate;
  end;
end;

procedure TFindResultsWindow.ResizeListBox;
resourcestring
  SAllAlphaNumericChars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890';
begin
  lbResults.Canvas.Font.Assign(lbResults.Font);
  lbResults.ItemHeight := lbResults.Canvas.TextHeight(SAllAlphaNumericChars) + 3;
  lbResults.Refresh;
end;

procedure TFindResultsWindow.SetMatchString(const MatchStr: string);
begin
  StatusBar.Panels.Items[1].Caption := MatchStr;
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
  StatusBar.Panels.Items[0].Caption := StatusStr;
end;

procedure TFindResultsWindow.StartFileSearch(Sender: TObject;
  const FileName: string);
resourcestring
  SProcessing = 'Processing %s';
var
  Dummy: Boolean;
begin
  SetStatusString(Format(SProcessing, [FileName]));
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

resourcestring
  SFileChangedAbort = '%s' + sLineBreak + 'has changed since it was searched.  Replacement aborted.'
    + sLineBreak + 'Expected: %s' + sLineBreak + 'Found: %s';
  SUnableToOpen  = 'Unable to open ';
  SNoOpenForms   = 'Replacing strings in open forms is not possible.  Please close the form first.';
  SFileSkipped   = 'The following file will be skipped: ';
  SCouldNotBackup= 'Could not backup file "%s" and will skip it';

// Replaces the string between SPos and EPos with the replace string from TGrepSettings
function ReplacePatternInString(CurrentLine: TLineResult; GrepSettings: TGrepSettings): string;
var
  i: Integer;
  FindPos: Integer;
  FindLen: Integer;
  CurrentMatch: TMatchResult;
begin
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
       Replaced := ReplaceAllInFiles(ResultList.Objects[i] as TFileResult, GrepSettings);
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

  procedure DoReplacement;
  var
    i: Integer;
    FileLine: string;
  begin
    if LineMode then
    begin
      i := ALineResult.LineNo;
        Assert(TempFile.Count >= (LineResult.LineNo - 1));
      FileLine := TempFile.Strings[LineResult.LineNo - 1];
      if LineResult.Line <> FileLine then
        raise Exception.CreateResFmt(@SFileChangedAbort, [MatchFile, LineResult.Line, FileLine]);

      TempString := ReplacePatternInString(LineResult, GrepSettings);
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
        FileLine := TempFile.Strings[LineResult.LineNo - 1];
        if LineResult.Line <> FileLine then
          raise Exception.CreateFmt(SFileChangedAbort, [MatchFile, LineResult.Line, FileLine]);

        TempString := ReplacePatternInString(LineResult, GrepSettings);
        TempFile.Strings[LineResult.LineNo - 1] := TempString;
      end;
    end;
  end;

  procedure WriteResults;
  var
    Editor : IEditor;
    OldCaretXY : TBufferCoord;
  begin
    Editor := GI_EditorFactory.GetEditorByNameOrTitle(MatchFile);
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
          if MessageDlg(Format(SCouldNotBackup, [MatchFile]), mtWarning, [mbOK, mbCancel], 0) = mrCancel then
            Abort
          else
            Exit;
        end;
      TempFile.SaveToFile(MatchFile);
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

  TempFile := TStringList.Create;
  try
    if not GetFileAsText(MatchFile, TempFile) then begin
      if MessageDlg(SFileSkipped + MatchFile, mtWarning, [mbOK, mbCancel], 0) = mrCancel then
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

function ReplaceAllInFiles(FileResult: TFileResult; GrepSettings: TGrepSettings): Integer;
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

procedure TFindResultsWindow.FormActivate(Sender: TObject);
begin
  inherited;
  if not HasFocus then begin
    FGPanelEnter(Self);
    PostMessage(lbResults.Handle, WM_SETFOCUS, 0, 0);
  end;
end;

procedure TFindResultsWindow.ReadFromAppStorage(
  AppStorage: TJvCustomAppStorage; const BasePath: string);
begin
  AppStorage.WriteInteger(BasePath+'\ResultsHeight', lbResults.Height);
  AppStorage.WriteBoolean(BasePath+'\ShowToolBar', ToolBar.Visible);
  AppStorage.WriteBoolean(BasePath+'\ShowStatusBar', StatusBar.Visible);
  AppStorage.WriteBoolean(BasePath+'\ShowContext', ShowContext);
end;

procedure TFindResultsWindow.WriteToAppStorage(
  AppStorage: TJvCustomAppStorage; const BasePath: string);
begin
  lbResults.Height := AppStorage.ReadInteger(BasePath+'\ResultsHeight', lbResults.Height);
  ToolBar.Visible := AppStorage.ReadBoolean(BasePath+'\ShowToolBar', ToolBar.Visible);
  StatusBar.Visible := AppStorage.ReadBoolean(BasePath+'\ShowStatusBar', StatusBar.Visible);
  ShowContext := AppStorage.ReadBoolean(BasePath+'\ShowContext', True);
end;

procedure TFindResultsWindow.TBMThemeChange(var Message: TMessage);
begin
  inherited;
  if Message.WParam = TSC_VIEWCHANGE then begin
    Splitter.Color := CurrentTheme.GetViewColor(TVT_NORMALTOOLBAR);
    lbResults.Invalidate;
  end;
end;

procedure TFindResultsWindow.actHelpHelpExecute(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

end.

