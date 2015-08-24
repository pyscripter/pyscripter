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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmIDEDockWin, JvDockControlForm, ExtCtrls, StdCtrls,
  VirtualTrees, SpTBXDkPanels, TB2Item,
  TB2Dock, TB2Toolbar, JvAppStorage,
  JvComponentBase, SpTBXItem, ComCtrls,  SpTBXControls, SpTBXSkins,
  Generics.Collections, SpTBXEditors;

type
  TRegExpTesterWindow = class(TIDEDockWindow, IJvAppStorageHandler)
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
    RegExpText: TRichEdit;
    SearchText: TRichEdit;
    MatchText: TRichEdit;
    RI_findall: TSpTBXItem;
    SpinMatches: TSpTBXSpinEdit;
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
  private
    { Private declarations }
    OldRegExp : string;
    OldSearchText : string;
    RegExp : Variant;
    MatchObject : Variant;
    MatchList : TList<Variant>;
  protected
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
    // IJvAppStorageHandler implementation
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
  public
    { Public declarations }
    procedure HighlightMatches;
    procedure ClearHighlight;
  end;

var
  RegExpTesterWindow: TRegExpTesterWindow;

implementation

uses dmCommands, VarPyth, frmPythonII,
  PythonEngine, gnugettext, JvAppIniStorage, uCommonFunctions, RichEdit,
  cThemedVirtualStringTree;

{$R *.dfm}

procedure RE_SetSelBgColor(RichEdit: TRichEdit; SelectionOnly: Boolean; AColor: TColor);
var
  Format: TCharFormat2;
  WParam : integer;
begin
  if SelectionOnly then
    WParam := SCF_SELECTION
  else
    WParam := SCF_ALL;

  FillChar(Format, SizeOf(Format), 0);
  with Format do
  begin
    cbSize := SizeOf(Format);
    dwMask := CFM_BACKCOLOR;
    crBackColor := ColorToRGB(AColor);
    Richedit.Perform(EM_SETCHARFORMAT, WParam, Longint(@Format));
  end;
end;

procedure TRegExpTesterWindow.ClearHighlight;
begin
  RE_SetSelBgColor(SearchText, False, MatchText.Color);
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
  inherited;
  MatchList := TList<Variant>.Create;

  GroupsView.NodeDataSize := 0;
  GroupsView.Header.Height :=
    MulDiv(GroupsView.Header.Height, Screen.PixelsPerInch, 96);
end;

procedure TRegExpTesterWindow.FormDestroy(Sender: TObject);
begin
  inherited;
  GroupsView.Clear;
  FreeAndNil(MatchList);
end;

procedure TRegExpTesterWindow.WMSpSkinChange(var Message: TMessage);
begin
  inherited;
  GroupsView.SkinTree;
end;

procedure TRegExpTesterWindow.WriteToAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
Var
  SearchType : integer;
begin
  if RI_findall.Checked then
    SearchType := 0
  else if RI_Search.Checked then
    SearchType := 1
  else
    SearchType := 2;
  TJvAppIniStorageOptions(AppStorage.StorageOptions).ReplaceCRLF := True;
  TJvAppIniStorageOptions(AppStorage.StorageOptions).PreserveLeadingTrailingBlanks := True;
  AppStorage.WriteString(BasePath+'\Regular Expression', RegExpText.Text);
  AppStorage.WriteString(BasePath+'\Search Text', SearchText.Text);
  TJvAppIniStorageOptions(AppStorage.StorageOptions).ReplaceCRLF := False;
  TJvAppIniStorageOptions(AppStorage.StorageOptions).PreserveLeadingTrailingBlanks := False;

  AppStorage.WriteBoolean(BasePath+'\DOTALL', CI_DOTALL.Checked);
  AppStorage.WriteBoolean(BasePath+'\IGNORECASE', CI_IGNORECASE.Checked);
  AppStorage.WriteBoolean(BasePath+'\LOCALE', CI_LOCALE.Checked);
  AppStorage.WriteBoolean(BasePath+'\MULTILINE', CI_MULTILINE.Checked);
  AppStorage.WriteBoolean(BasePath+'\UNICODE', CI_UNICODE.Checked);
  AppStorage.WriteBoolean(BasePath+'\VERBOSE', CI_VERBOSE.Checked);
  AppStorage.WriteInteger(BasePath+'\SearchType', SearchType);
  AppStorage.WriteBoolean(BasePath+'\AutoExec', CI_AutoExecute.Checked);
  AppStorage.WriteInteger(BasePath+'\RegExpHeight', dpRegExpText.Height);
  AppStorage.WriteInteger(BasePath+'\GroupsHeight', dpGroupsView.Height);
  AppStorage.WriteInteger(BasePath+'\SearchHeight', dpSearchText.Height);
  TJvAppIniStorageOptions(AppStorage.StorageOptions).ReplaceCRLF := False;
end;

procedure TRegExpTesterWindow.ReadFromAppStorage(
  AppStorage: TJvCustomAppStorage; const BasePath: string);
Var
  SearchType : integer;
begin
  TJvAppIniStorageOptions(AppStorage.StorageOptions).ReplaceCRLF := True;
  TJvAppIniStorageOptions(AppStorage.StorageOptions).PreserveLeadingTrailingBlanks := True;
  RegExpText.Text := AppStorage.ReadString(BasePath+'\Regular Expression');
  SearchText.Text := AppStorage.ReadString(BasePath+'\Search Text');
  TJvAppIniStorageOptions(AppStorage.StorageOptions).ReplaceCRLF := False;
  TJvAppIniStorageOptions(AppStorage.StorageOptions).PreserveLeadingTrailingBlanks := False;

  CI_DOTALL.Checked := AppStorage.ReadBoolean(BasePath+'\DOTALL', False);
  CI_IGNORECASE.Checked := AppStorage.ReadBoolean(BasePath+'\IGNORECASE', False);
  CI_LOCALE.Checked := AppStorage.ReadBoolean(BasePath+'\LOCALE', False);
  CI_MULTILINE.Checked := AppStorage.ReadBoolean(BasePath+'\MULTILINE', False);
  CI_UNICODE.Checked := AppStorage.ReadBoolean(BasePath+'\UNICODE', False);
  CI_VERBOSE.Checked := AppStorage.ReadBoolean(BasePath+'\VERBOSE', False);
  SearchType := AppStorage.ReadInteger(BasePath+'\SearchType');
  case SearchType of
    1: RI_Search.Checked := True;
    2: RI_Match.Checked := True;
  else
    RI_findall.Checked := True;
  end;
  dpRegExpText.Height := AppStorage.ReadInteger(BasePath+'\RegExpHeight', dpRegExpText.Height);
  dpGroupsView.Height := AppStorage.ReadInteger(BasePath+'\GroupsHeight', dpGroupsView.Height);
  dpSearchText.Height := AppStorage.ReadInteger(BasePath+'\SearchHeight', dpSearchText.Height);
  CI_AutoExecute.Checked := AppStorage.ReadBoolean(BasePath+'\AutoExec', False);
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
  Index : Integer;
begin
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
  re: Variant;
  Flags : integer;
  FindIter : Variant;
  AdjSearchText : string;
begin
  MatchText.Clear;
  GroupsView.Clear;
  VarClear(RegExp);
  VarClear(MatchObject);
  MatchList.Clear;
  ClearHighlight;
  lblMatch.Caption := _('Match');

  if RegExpText.Text = '' then Exit;
  if SearchText.Text = '' then Exit;

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
    PythonIIForm.ShowOutput := false;
    RegExp := re.compile(RegExpText.Text, Flags);
  except
    on E: Exception do begin
      with lbStatusBar do begin
        ImageIndex := 21;
        Caption := E.Message;
      end;
      PythonIIForm.ShowOutput := True;
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
          if GetPythonEngine.IsPython3000 then
            MatchObject := FindIter.__next__()
          else
            MatchObject := FindIter.next();
          GetPythonEngine.CheckError(True);
          MatchList.Add(MatchObject);
        end;
      except
        on EPyStopIteration do begin end;
      end;
    end;
  except
    on E: Exception do begin
      with lbStatusBar do begin
        ImageIndex := 21;
        Caption := E.Message;
      end;
      PythonIIForm.ShowOutput := True;
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
      ImageIndex := 21;
      Caption := 'Search text did not match';
    end;
  end else begin
    with lbStatusBar do begin
      ImageIndex := 20;
      Caption := 'Search text matched';
    end;
    MatchText.Text := MatchObject.group();
    GroupsView.RootNodeCount := len(MatchObject.groups());
    HighlightMatches;
  end;

  PythonIIForm.ShowOutput := True;
end;

procedure TRegExpTesterWindow.GroupsViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
Var
  GroupDict, Keys : Variant;
  var
    i : integer;
begin
  Assert(VarIsPython(MatchObject) and not VarIsNone(MatchObject));
  Assert(Integer(Node.Index) < len(MatchObject.groups()));
  case Column of
    0:  CellText := IntToStr(Node.Index + 1);
    1:  begin
          CellText := '';
          GroupDict := RegExp.groupindex;
          Keys := Groupdict.keys(); 
          if GetPythonEngine.IsPython3000 then
            Keys := BuiltinModule.list(Keys);

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
Var
  OldSelStart,
  OldSelLen : integer;
  VMatch : Variant;
begin
  OldSelStart := SearchText.SelStart;
  OldSelLen := SearchText.SelLength;
  SearchText.Lines.BeginUpdate;
  try
    for VMatch in MatchList do
    begin
      SearchText.SelStart := VMatch.start();
      SearchText.SelLength := VMatch.end() - SearchText.SelStart;
      RE_SetSelBgColor(SearchText, True, ColorToRGB(clHighlight));
    end;
  finally
    SearchText.SelStart := OldSelStart;
    SearchText.SelLength := OldSelLen;
    SearchText.Lines.EndUpdate;
  end;
end;

procedure TRegExpTesterWindow.TiClearClick(Sender: TObject);
begin
  RegExpText.Clear;
  SearchText.Clear;
  ClearHighlight;
  MatchText.Clear;
  GroupsView.Clear;
  VarClear(RegExp);
  VarClear(MatchObject);
  MatchList.Clear;
  lblMatch.Caption := _('Match');
  SpinMatches.Value := 1;
  SpinMatches.Enabled := False;
  with lbStatusBar do begin
    ImageIndex := 21;
    Caption := 'Not executed';
  end;
end;

end.
