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
  Dialogs, frmIDEDockWin, JvComponent, JvDockControlForm, ExtCtrls, StdCtrls,
  JvExStdCtrls, JvRichEdit, mbTBXJvRichEdit, VirtualTrees, TBXDkPanels, TB2Item,
  TBX, TBXThemes, TB2Dock, TB2Toolbar, TBXStatusBars, JvAppStorage,
  JvComponentBase;

type
  TRegExpTesterWindow = class(TIDEDockWindow, IJvAppStorageHandler)
    StatusBar: TTBXStatusBar;
    TBXDock: TTBXDock;
    RegExpTesterToolbar: TTBXToolbar;
    TBXSubmenuItem2: TTBXSubmenuItem;
    CI_DOTALL: TTBXItem;
    CI_IGNORECASE: TTBXItem;
    CI_LOCALE: TTBXItem;
    CI_MULTILINE: TTBXItem;
    TBXSeparatorItem1: TTBXSeparatorItem;
    TIExecute: TTBXItem;
    TBXMultiDock: TTBXMultiDock;
    TBXDockablePanel3: TTBXDockablePanel;
    TBXLabel1: TTBXLabel;
    GroupsView: TVirtualStringTree;
    TBXDockablePanel1: TTBXDockablePanel;
    TBXLabel2: TTBXLabel;
    MatchText: TmbTBXJvRichEdit;
    TBXDockablePanel2: TTBXDockablePanel;
    TBXLabel3: TTBXLabel;
    RegExpText: TmbTBXJvRichEdit;
    TBXDockablePanel4: TTBXDockablePanel;
    TBXLabel4: TTBXLabel;
    SearchText: TmbTBXJvRichEdit;
    TBXSeparatorItem2: TTBXSeparatorItem;
    CI_UNICODE: TTBXItem;
    CI_VERBOSE: TTBXItem;
    RI_Match: TTBXItem;
    RI_Search: TTBXItem;
    tiHelp: TTBXItem;
    TBXSeparatorItem3: TTBXSeparatorItem;
    TiClear: TTBXItem;
    TBXSeparatorItem4: TTBXSeparatorItem;
    CI_AutoExecute: TTBXItem;
    procedure TiClearClick(Sender: TObject);
    procedure GroupsViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure TIExecuteClick(Sender: TObject);
    procedure tiHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure RegExpTextChange(Sender: TObject);
  private
    { Private declarations }
    RegExp : Variant;
    MatchObject : Variant;
  protected
    procedure TBMThemeChange(var Message: TMessage); message TBM_THEMECHANGE;
    // IJvAppStorageHandler implementation
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
  public
    { Public declarations }
  end;

var
  RegExpTesterWindow: TRegExpTesterWindow;

implementation

uses dmCommands, uCommonFunctions, frmPyIDEMain, VarPyth, frmPythonII;

{$R *.dfm}

procedure TRegExpTesterWindow.FormResize(Sender: TObject);
begin
  inherited;
  TBXMultiDock.ResizeVisiblePanels(FGPanel.ClientWidth-4);
end;

procedure TRegExpTesterWindow.FormCreate(Sender: TObject);
begin
  inherited;
  GroupsView.NodeDataSize := 0;
  GroupsView.OnAdvancedHeaderDraw :=
    CommandsDataModule.VirtualStringTreeAdvancedHeaderDraw;
  GroupsView.OnHeaderDrawQueryElements :=
    CommandsDataModule.VirtualStringTreeDrawQueryElements;
end;

procedure TRegExpTesterWindow.TBMThemeChange(var Message: TMessage);
begin
  inherited;
  if Message.WParam = TSC_VIEWCHANGE then begin
    GroupsView.Header.Invalidate(nil, True);
    GroupsView.Colors.HeaderHotColor :=
      CurrentTheme.GetItemTextColor(GetItemInfo('active'));
  end;
end;

procedure TRegExpTesterWindow.WriteToAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
begin
  AppStorage.WriteString(BasePath+'\Regular Expression', RegExpText.Text);
  AppStorage.WriteString(BasePath+'\Search Text', SearchText.Text);
  AppStorage.WriteBoolean(BasePath+'\DOTALL', CI_DOTALL.Checked);
  AppStorage.WriteBoolean(BasePath+'\IGNORECASE', CI_IGNORECASE.Checked);
  AppStorage.WriteBoolean(BasePath+'\LOCALE', CI_LOCALE.Checked);
  AppStorage.WriteBoolean(BasePath+'\MULTILINE', CI_MULTILINE.Checked);
  AppStorage.WriteBoolean(BasePath+'\UNICODE', CI_UNICODE.Checked);
  AppStorage.WriteBoolean(BasePath+'\VERBOSE', CI_VERBOSE.Checked);
  AppStorage.WriteBoolean(BasePath+'\Search', RI_Search.Checked);
  AppStorage.WriteBoolean(BasePath+'\AutoExec', CI_AutoExecute.Checked);
end;

procedure TRegExpTesterWindow.ReadFromAppStorage(
  AppStorage: TJvCustomAppStorage; const BasePath: string);
begin
  RegExpText.Text := AppStorage.ReadString(BasePath+'\Regular Expression');
  SearchText.Text := AppStorage.ReadString(BasePath+'\Search Text');
  CI_DOTALL.Checked := AppStorage.ReadBoolean(BasePath+'\DOTALL', False);
  CI_IGNORECASE.Checked := AppStorage.ReadBoolean(BasePath+'\IGNORECASE', False);
  CI_LOCALE.Checked := AppStorage.ReadBoolean(BasePath+'\LOCALE', False);
  CI_MULTILINE.Checked := AppStorage.ReadBoolean(BasePath+'\MULTILINE', False);
  CI_UNICODE.Checked := AppStorage.ReadBoolean(BasePath+'\UNICODE', False);
  CI_VERBOSE.Checked := AppStorage.ReadBoolean(BasePath+'\VERBOSE', False);
  RI_Search.Checked := AppStorage.ReadBoolean(BasePath+'\Search', True);
  CI_AutoExecute.Checked := AppStorage.ReadBoolean(BasePath+'\AutoExec', True);
end;

procedure TRegExpTesterWindow.RegExpTextChange(Sender: TObject);
begin
  if CI_AutoExecute.Checked and (RegExpText.Text <> '') and
    (SearchText.Text <> '')
  then
    TIExecuteClick(Self);
end;

procedure TRegExpTesterWindow.tiHelpClick(Sender: TObject);
begin
  if not CommandsDataModule.ShowPythonKeywordHelp('re (standard module)') then
    Application.HelpContext(HelpContext);
end;

procedure TRegExpTesterWindow.TIExecuteClick(Sender: TObject);
Var
  re: Variant;
  Flags : integer;
begin
  MatchText.Clear;
  GroupsView.Clear;
  VarClear(RegExp);
  VarClear(MatchObject);

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
  // Compile Regular Expression
  try
    PythonIIForm.ShowOutput := false;
    RegExp := re.compile(RegExpText.Text, Flags);
  except
    on E: Exception do begin
      with StatusBar.Panels[0] do begin
        ImageIndex := 21;
        Caption := E.Message;
      end;
      PythonIIForm.ShowOutput := True;
      Exit;
    end;
  end;

  // Execute regular expression
  try
    if RI_Search.Checked then
      MatchObject := regexp.search(SearchText.Text, 0)
    else
      MatchObject := RegExp.match(SearchText.Text);
  except
    on E: Exception do begin
      with StatusBar.Panels[0] do begin
        ImageIndex := 21;
        Caption := E.Message;
      end;
      PythonIIForm.ShowOutput := True;
      Exit;
    end;
  end;

  if (not VarIsPython(MatchObject)) or VarIsNone(MatchObject) then begin
    with StatusBar.Panels[0] do begin
      ImageIndex := 21;
      Caption := 'Search text did not match';
    end;
  end else begin
    with StatusBar.Panels[0] do begin
      ImageIndex := 20;
      Caption := 'Search text matched';
    end;
    MatchText.Text := MatchObject.group();
    GroupsView.RootNodeCount := len(MatchObject.groups());
  end;

  PythonIIForm.ShowOutput := True;
end;

procedure TRegExpTesterWindow.GroupsViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
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
          for i := 0 to len(Keys) - 1 do
            if Groupdict.getitem(Keys.getitem(i)) = Node.Index + 1 then begin
              CellText := Keys.getitem(i);
              break;
            end;
        end;
    2:  if VarIsNone(MatchObject.groups(Node.Index+1)) then
          CellText := ''
        else
          CellText := MatchObject.group(Node.Index+1);
  end;
end;

procedure TRegExpTesterWindow.TiClearClick(Sender: TObject);
begin
  RegExpText.Clear;
  SearchText.Clear;
  MatchText.Clear;
  GroupsView.Clear;
  VarClear(RegExp);
  VarClear(MatchObject);
  with StatusBar.Panels[0] do begin
    ImageIndex := 21;
    Caption := 'Not executed';
  end;
end;

end.
