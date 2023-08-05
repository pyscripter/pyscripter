{-----------------------------------------------------------------------------
 Unit Name: frmRegExpTester
 Author:    Kiriakos Vlahos
 Date:      08-Dec-2005
 Purpose:   Integrated regular expression development and testing
 History:
-----------------------------------------------------------------------------}

unit frmRegExpTester;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.ImageList,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  JvDockControlForm,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  TB2Item,
  TB2Dock,
  TB2Toolbar,
  SpTBXItem,
  SpTBXControls,
  SpTBXDkPanels,
  SpTBXSkins,
  SpTBXEditors,
  JvAppStorage,
  JvComponentBase,
  VirtualTrees.Types,
  VirtualTrees.BaseTree,
  VirtualTrees.BaseAncestorVCL,
  VirtualTrees.AncestorVCL,
  VirtualTrees,
  SynEdit,
  frmIDEDockWin;

type
  TRegExpTesterWindow = class(TIDEDockWindow)
    TBXDock: TSpTBXDock;
    RegExpTesterToolbar: TSpTBXToolbar;
    TBXSubmenuItem2: TSpTBXSubmenuItem;
    CI_DOTALL: TSpTBXItem;
    CI_IGNORECASE: TSpTBXItem;
    CI_LOCALE: TSpTBXItem;
    CI_MULTILINE: TSpTBXItem;
    TBXSeparatorItem1: TSpTBXSeparatorItem;
    TIExecute: TSpTBXItem;
    TBXSeparatorItem2: TSpTBXSeparatorItem;
    CI_UNICODE: TSpTBXItem;
    CI_VERBOSE: TSpTBXItem;
    RI_Match: TSpTBXItem;
    RI_Search: TSpTBXItem;
    tiHelp: TSpTBXItem;
    TBXSeparatorItem3: TSpTBXSeparatorItem;
    TiClear: TSpTBXItem;
    TBXSeparatorItem4: TSpTBXSeparatorItem;
    CI_AutoExecute: TSpTBXItem;
    StatusBar: TSpTBXStatusBar;
    lbStatusBar: TSpTBXLabelItem;
    dpGroupsView: TPanel;
    TBXLabel1: TSpTBXLabel;
    GroupsView: TVirtualStringTree;
    dpMatchText: TPanel;
    lblMatch: TSpTBXLabel;
    SpTBXPanel3: TPanel;
    dpRegExpText: TPanel;
    TBXLabel3: TSpTBXLabel;
    SpTBXPanel1: TPanel;
    dpSearchText: TPanel;
    TBXLabel4: TSpTBXLabel;
    SpTBXPanel2: TPanel;
    SpTBXSplitter1: TSpTBXSplitter;
    SpTBXSplitter2: TSpTBXSplitter;
    pnlMiddle: TPanel;
    SpTBXSplitter3: TSpTBXSplitter;
    pnlBackground: TPanel;
    RI_findall: TSpTBXItem;
    SpinMatches: TSpTBXSpinEdit;
    vilImages: TVirtualImageList;
    RegExpText: TSynEdit;
    SearchText: TSynEdit;
    MatchText: TSynEdit;
    procedure TiClearClick(Sender: TObject);
    procedure GroupsViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TIExecuteClick(Sender: TObject);
    procedure tiHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RegExpTextChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SpinMatchesValueChanged(Sender: TObject);
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  private
    OldRegExp : string;
    OldSearchText : string;
    RegExp : Variant;
    MatchObject : Variant;
    MatchList : TList<Variant>;
  protected
    const FBasePath = 'RegExp Tester Options'; // Used for storing settings
    const FHighlightIndicatorID: TGUID = '{10FBEC66-4210-49F5-9F7D-189B6252080B}';
  public
    procedure Clear;
    procedure HighlightMatches;
    procedure ClearHighlight;
    // AppStorage
    procedure StoreSettings(AppStorage: TJvCustomAppStorage); override;
    procedure RestoreSettings(AppStorage: TJvCustomAppStorage); override;
  end;

var
  RegExpTesterWindow: TRegExpTesterWindow;

implementation

uses
  Vcl.Themes,
  JvGnugettext,
  JvAppIniStorage,
  VarPyth,
  SynDWrite,
  SynEditMiscClasses,
  dmResources,
  dmCommands,
  PythonEngine,
  uEditAppIntfs,
  uCommonFunctions;

{$R *.dfm}

procedure TRegExpTesterWindow.ClearHighlight;
begin
  SearchText.Indicators.Clear(FHighlightIndicatorID);
end;

procedure TRegExpTesterWindow.Clear;
begin
  if not (csDestroying in ComponentState) then
  begin
    ClearHighlight;
    MatchText.Clear;
    GroupsView.Clear;
    lblMatch.Caption := _('Match');
    SpinMatches.Value := 1;
    SpinMatches.Enabled := False;
    with lbStatusBar do
    begin
      ImageIndex := 3;
      Caption := 'Not executed';
    end;
  end;
  if GI_PyControl.PythonLoaded then
  begin
    var Py := GI_PyControl.SafePyEngine;
    VarClear(RegExp);
    VarClear(MatchObject);
    MatchList.Clear;
  end;
end;

procedure TRegExpTesterWindow.FormActivate(Sender: TObject);
begin
  inherited;
  if RegExpText.CanFocus then
    RegExpText.SetFocus;
//    PostMessage(RegExpText.Handle, WM_SETFOCUS, 0, 0);
 end;

procedure TRegExpTesterWindow.FormCreate(Sender: TObject);
begin
  ImageName := 'RegExp';
  inherited;
  MatchList := TList<Variant>.Create;

  GroupsView.NodeDataSize := 0;
end;

procedure TRegExpTesterWindow.FormDestroy(Sender: TObject);
begin
  Clear;
  FreeAndNil(MatchList);
  inherited;
end;

procedure TRegExpTesterWindow.WMSpSkinChange(var Message: TMessage);
begin
  inherited;
  RegExpText.Font.Color := StyleServices.GetSystemColor(clWindowText);
  RegExpText.Color := StyleServices.GetSystemColor(clWindow);
  SearchText.Font.Color := StyleServices.GetSystemColor(clWindowText);
  SearchText.Color := StyleServices.GetSystemColor(clWindow);
  MatchText.Font.Color := StyleServices.GetSystemColor(clWindowText);
  MatchText.Color := StyleServices.GetSystemColor(clWindow);

  var FHighlightIndicatorSpec := TSynIndicatorSpec.Create(sisTextDecoration,
    clNoneF, D2D1ColorF(StyleServices.GetSystemColor(clHighlight)), [fsBold]);
  SearchText.Indicators.RegisterSpec(FHighlightIndicatorID, FHighlightIndicatorSpec);
end;

procedure TRegExpTesterWindow.StoreSettings(AppStorage: TJvCustomAppStorage);
Var
  SearchType : integer;
begin
  inherited;
  if RI_findall.Checked then
    SearchType := 0
  else if RI_Search.Checked then
    SearchType := 1
  else
    SearchType := 2;
  var AppIniStorageOptions := TJvAppIniStorageOptions(AppStorage.StorageOptions);
  var OldReplaceCRLF := AppIniStorageOptions.ReplaceCRLF;
  var OldPreserveLeadingTrailingBlanks := AppIniStorageOptions.PreserveLeadingTrailingBlanks;
  AppIniStorageOptions.ReplaceCRLF := True;
  AppIniStorageOptions.PreserveLeadingTrailingBlanks := True;
  AppStorage.WriteString(FBasePath+'\Regular Expression', RegExpText.Text);
  AppStorage.WriteString(FBasePath+'\Search Text', SearchText.Text);
  AppIniStorageOptions.ReplaceCRLF := OldReplaceCRLF;
  AppIniStorageOptions.PreserveLeadingTrailingBlanks := OldPreserveLeadingTrailingBlanks;

  AppStorage.WriteBoolean(FBasePath+'\DOTALL', CI_DOTALL.Checked);
  AppStorage.WriteBoolean(FBasePath+'\IGNORECASE', CI_IGNORECASE.Checked);
  AppStorage.WriteBoolean(FBasePath+'\LOCALE', CI_LOCALE.Checked);
  AppStorage.WriteBoolean(FBasePath+'\MULTILINE', CI_MULTILINE.Checked);
  AppStorage.WriteBoolean(FBasePath+'\UNICODE', CI_UNICODE.Checked);
  AppStorage.WriteBoolean(FBasePath+'\VERBOSE', CI_VERBOSE.Checked);
  AppStorage.WriteInteger(FBasePath+'\SearchType', SearchType);
  AppStorage.WriteBoolean(FBasePath+'\AutoExec', CI_AutoExecute.Checked);
  AppStorage.WriteInteger(FBasePath+'\RegExpHeight', PPIUnScale(dpRegExpText.Height));
  AppStorage.WriteInteger(FBasePath+'\GroupsHeight', PPIUnScale(dpGroupsView.Height));
  AppStorage.WriteInteger(FBasePath+'\SearchHeight', PPIUnScale(dpSearchText.Height));
  TJvAppIniStorageOptions(AppStorage.StorageOptions).ReplaceCRLF := False;
end;

procedure TRegExpTesterWindow.RestoreSettings(AppStorage: TJvCustomAppStorage);
begin
  inherited;
  var AppIniStorageOptions := TJvAppIniStorageOptions(AppStorage.StorageOptions);
  var OldReplaceCRLF := AppIniStorageOptions.ReplaceCRLF;
  var OldPreserveLeadingTrailingBlanks := AppIniStorageOptions.PreserveLeadingTrailingBlanks;
  AppIniStorageOptions.ReplaceCRLF := True;
  AppIniStorageOptions.PreserveLeadingTrailingBlanks := True;
  RegExpText.Text := AppStorage.ReadString(FBasePath+'\Regular Expression');
  SearchText.Text := AppStorage.ReadString(FBasePath+'\Search Text');
  AppIniStorageOptions.ReplaceCRLF := OldReplaceCRLF;
  AppIniStorageOptions.PreserveLeadingTrailingBlanks := OldPreserveLeadingTrailingBlanks;

  CI_DOTALL.Checked := AppStorage.ReadBoolean(FBasePath+'\DOTALL', False);
  CI_IGNORECASE.Checked := AppStorage.ReadBoolean(FBasePath+'\IGNORECASE', False);
  CI_LOCALE.Checked := AppStorage.ReadBoolean(FBasePath+'\LOCALE', False);
  CI_MULTILINE.Checked := AppStorage.ReadBoolean(FBasePath+'\MULTILINE', False);
  CI_UNICODE.Checked := AppStorage.ReadBoolean(FBasePath+'\UNICODE', False);
  CI_VERBOSE.Checked := AppStorage.ReadBoolean(FBasePath+'\VERBOSE', False);
  var SearchType := AppStorage.ReadInteger(FBasePath+'\SearchType');
  case SearchType of
    1: RI_Search.Checked := True;
    2: RI_Match.Checked := True;
  else
    RI_findall.Checked := True;
  end;
  dpRegExpText.Height :=
    PPIScale(AppStorage.ReadInteger(FBasePath+'\RegExpHeight', dpRegExpText.Height));
  dpGroupsView.Height :=
    PPIScale(AppStorage.ReadInteger(FBasePath+'\GroupsHeight', dpGroupsView.Height));
  dpSearchText.Height :=
    PPIScale(AppStorage.ReadInteger(FBasePath+'\SearchHeight', dpSearchText.Height));
  CI_AutoExecute.Checked := AppStorage.ReadBoolean(FBasePath+'\AutoExec', False);
end;

procedure TRegExpTesterWindow.RegExpTextChange(Sender: TObject);
//  This is fired not only when the text is changed but for other
//  various changes.  So we need to check that the text has changed
begin
  if Visible and
     ((OldRegExp <> RegExpText.Text) or (OldSearchText <> SearchText.Text))
  then
    ClearHighlight;
  if CI_AutoExecute.Checked and (RegExpText.Text <> '') and
    (SearchText.Text <> '') and Visible and
    ((OldRegExp <> RegExpText.Text) or (OldSearchText <> SearchText.Text))
  then
    TIExecuteClick(Self);
end;

procedure TRegExpTesterWindow.SpinMatchesValueChanged(Sender: TObject);
Var
  Py: IPyEngineAndGIL;
  Index : Integer;
begin
  Py := GI_PyControl.SafePyEngine;
  Index := Trunc(SpinMatches.Value);
  if (Index > 0) and (Index <= MatchList.Count) then begin
    GroupsView.Clear;
    MatchObject := MatchList[Index-1];
    MatchText.Text := MatchObject.group();
    GroupsView.RootNodeCount := len(MatchObject.groups());
    lblMatch.Caption := Format(_('Match %d of %d'), [Index, MatchList.Count]);
  end;
end;

procedure TRegExpTesterWindow.tiHelpClick(Sender: TObject);
begin
  if not CommandsDataModule.ShowPythonKeywordHelp('re (module)') then
    Application.HelpContext(HelpContext);
end;

procedure TRegExpTesterWindow.TIExecuteClick(Sender: TObject);
Var
  Py: IPyEngineAndGIL;
  re: Variant;
  Flags: integer;
  FindIter: Variant;
  AdjSearchText: string;
  OutputSuppressor: IInterface;
begin
  if not GI_PyControl.Inactive then Exit;
  if RegExpText.Text = '' then Exit;
  if SearchText.Text = '' then Exit;

  Clear;

  Py := GI_PyControl.SafePyEngine;

  re := Import('re');
  Flags := 0;
  if CI_DOTALL.Checked then
    Flags := re.DOTALL;
  if CI_IGNORECASE.Checked then
    Flags := Flags or re.IGNORECASE;
  if CI_LOCALE.Checked then
    Flags := Flags or re.LOCALE;
  if CI_MULTILINE.Checked then
    Flags := Flags or re.MULTILINE;
  if CI_UNICODE.Checked then
    Flags := Flags or re.UNICODE;
  if CI_VERBOSE.Checked then
    Flags := Flags or re.VERBOSE;

  // Save text values
  OldRegExp := RegExpText.Text;
  OldSearchText := SearchText.Text;

  // Compile Regular Expression
  try
    OutputSuppressor := GI_PyInterpreter.OutputSuppressor;
    RegExp := re.compile(RegExpText.Text, Flags);
  except
    on E: Exception do begin
      with lbStatusBar do begin
        ImageIndex := 3;
        Caption := E.Message;
      end;
      Exit;
    end;
  end;

  // Execute regular expression
  AdjSearchText := AdjustLineBreaks(SearchText.Text, tlbsLF);
  try
    if RI_Search.Checked then begin
      MatchObject := RegExp.search(AdjSearchText, 0);
      if VarIsPython(MatchObject) and not VarIsNone(MatchObject) then
        MatchList.Add(MatchObject);
    end else if RI_Match.Checked then begin
      MatchObject := RegExp.match(AdjSearchText);
      if VarIsPython(MatchObject) and not VarIsNone(MatchObject) then
        MatchList.Add(MatchObject);
    end else begin
      FindIter := RegExp.finditer(AdjSearchText);
      try
        while True do begin
          MatchObject := FindIter.__next__();
          Py.PythonEngine.CheckError(True);
          MatchList.Add(MatchObject);
        end;
      except
        on EPyStopIteration do begin end;
      end;
    end;
  except
    on E: Exception do begin
      with lbStatusBar do begin
        ImageIndex := 3;
        Caption := E.Message;
      end;
      Exit;
    end;
  end;

  if MatchList.Count > 0 then
    MatchObject := MatchList[0]
  else
    MatchObject := None;

  SpinMatches.Value := 1;
  SpinMatches.Enabled := False;
  if MatchList.Count > 1 then begin
    SpinMatches.SpinOptions.MinValue := 1;
    SpinMatches.SpinOptions.MaxValue := MatchList.Count;
    SpinMatches.Enabled := True;
    lblMatch.Caption := Format(_('Match %d of %d'), [1, MatchList.Count]);
  end;

  if (not VarIsPython(MatchObject)) or VarIsNone(MatchObject) then begin
    with lbStatusBar do begin
      ImageIndex := 3;
      Caption := 'Search text did not match';
    end;
  end else begin
    with lbStatusBar do begin
      ImageIndex := 2;
      Caption := 'Search text matched';
    end;
    MatchText.Text := MatchObject.group();
    GroupsView.RootNodeCount := len(MatchObject.groups());
    HighlightMatches;
  end;
end;

procedure TRegExpTesterWindow.GroupsViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
Var
  Py: IPyEngineAndGIL;
  GroupDict, Keys : Variant;
  i : integer;
begin
  Py := GI_PyControl.SafePyEngine;
  Assert(VarIsPython(MatchObject) and not VarIsNone(MatchObject));
  Assert(Integer(Node.Index) < len(MatchObject.groups()));
  case Column of
    0:  CellText := IntToStr(Node.Index + 1);
    1:  begin
          CellText := '';
          GroupDict := RegExp.groupindex;
          Keys := BuiltinModule.list(Groupdict.keys());

          for i := 0 to len(Keys) - 1 do
            if Groupdict.__getitem__(Keys.__getitem__(i)) = Node.Index + 1 then begin
              CellText := Keys.__getitem__(i);
              break;
            end;
        end;
    2:  if VarIsNone(MatchObject.groups(Node.Index+1)) then
          CellText := ''
        else
          CellText := MatchObject.group(Node.Index+1);
  end;
end;

procedure TRegExpTesterWindow.HighlightMatches;

  procedure HighlightText(StartIndex, EndIndex: Integer);
  var
    Indicator: TSynIndicator;
  begin
    var StartCoord := SearchText.CharIndexToRowCol(StartIndex, #10);
    var EndCoord := SearchText.CharIndexToRowCol(EndIndex, #10);

    for var I := StartCoord.Line to EndCoord.Line do
    begin
      var StartChar := StartCoord.Char;
      var EndChar := EndCoord.Char;

      if I > StartCoord.Line then
        StartChar := 1;
      if I < EndCoord.Line then
        EndChar := SearchText.Lines[I - 1].Length + 1;

      Indicator := TSynIndicator.Create(FHighlightIndicatorID, StartChar, EndChar);
      SearchText.Indicators.Add(I, Indicator);
    end;
  end;

var
  VMatch : Variant;
begin
  var Py := GI_PyControl.SafePyEngine;
  for VMatch in MatchList do
    HighlightText(VMatch.start(), VMatch.end());
end;

procedure TRegExpTesterWindow.TiClearClick(Sender: TObject);
begin
  RegExpText.Clear;
  SearchText.Clear;
  Clear;
end;

end.
