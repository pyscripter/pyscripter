{-----------------------------------------------------------------------------
 Unit Name: dlgToolProperties
 Author:    Kiriakos Vlahos
 Date:      04-Jun-2005
 Purpose:   Dialog for specifying command-line tool properties
 History:
-----------------------------------------------------------------------------}

unit dlgToolProperties;

interface

uses
  System.Types, Windows, Messages, SysUtils, Variants, Classes, Controls, Forms,
  System.UITypes, Dialogs, cTools, StdCtrls, SynEdit, Menus,
  ActnList, SpTBXControls, SpTBXEditors, SpTBXItem, SpTBXTabs, TB2Item, 
  dlgPyIDEBase, ComCtrls, System.Actions, Vcl.Samples.Spin, Vcl.ExtCtrls,
  SynEditMiscClasses, System.ImageList, Vcl.ImgList, Vcl.VirtualImageList;

type
  TToolProperties = class(TPyIDEDlgBase)
    Panel1: TPanel;
    FormatsPopup: TPopupMenu;
    Filename1: TMenuItem;
    Linenumber1: TMenuItem;
    Columnnumber1: TMenuItem;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    SynApplication: TSynEdit;
    SynParameters: TSynEdit;
    SynWorkDir: TSynEdit;
    GroupBox4: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    btnOK: TButton;
    btnCancel: TButton;
    btnAppDir: TButton;
    btnWorkDir: TButton;
    btnStdFormats: TButton;
    btnHelp: TButton;
    cbCaptureOutput: TCheckBox;
    cbParseMessages: TCheckBox;
    cbParseTraceback: TCheckBox;
    cbHideConsole: TCheckBox;
    cbUseCustomEnv: TCheckBox;
    btnAdd: TButton;
    btnDelete: TButton;
    btnUpdate: TButton;
    ActionList: TActionList;
    actUpdateItem: TAction;
    actDeleteItem: TAction;
    actAddItem: TAction;
    Label1: TLabel;
    Label5: TLabel;
    Label17: TLabel;
    Label2: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label3: TLabel;
    lbShortcut: TLabel;
    lbContext: TLabel;
    Label13: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    edName: TEdit;
    edDescription: TEdit;
    edMessagesFormat: TEdit;
    edEnvName: TEdit;
    edEnvValue: TEdit;
    cbContext: TComboBox;
    cbSaveFiles: TComboBox;
    cbStandardInput: TComboBox;
    cbStandardOutput: TComboBox;
    TabControl: TSpTBXTabControl;
    SpTBXTabItem1: TSpTBXTabItem;
    tabProperties: TSpTBXTabSheet;
    SpTBXTabItem2: TSpTBXTabItem;
    tabEnvironment: TSpTBXTabSheet;
    lvItems: TListview;
    vilImages: TVirtualImageList;
    cbUTF8IO: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure Filename1Click(Sender: TObject);
    procedure SynApplicationEnter(Sender: TObject);
    procedure SynParametersEnter(Sender: TObject);
    procedure SynWorkDirEnter(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnWorkDirClick(Sender: TObject);
    procedure btnAppDirClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actAddItemExecute(Sender: TObject);
    procedure actDeleteItemExecute(Sender: TObject);
    procedure actUpdateItemExecute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure btnHelpClick(Sender: TObject);
    procedure cbParseMessagesClick(Sender: TObject);
    procedure btnStdFormatsClick(Sender: TObject);
    procedure lvItemsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    { Private declarations }
    fEnvStrings : TStrings;
    hkShortCut: TSynHotKey;
  public
    { Public declarations }
  end;

  function EditTool(Tool : TExternalTool; IsExternalRun : Boolean = False) : Boolean;
  function EditToolItem(Item : TCollectionItem) : Boolean;

implementation

uses
  Vcl.Graphics,
  Vcl.Themes,
  Vcl.FileCtrl,
  dmCommands,
  JclSysInfo,
  JvGnugettext,
  uCommonFunctions,
  StringResources;

{$R *.dfm}

function EditTool(Tool : TExternalTool; IsExternalRun : Boolean = False) : Boolean;
Var
  i : integer;
begin
  Result := False;
  if not Assigned(Tool) then Exit;
  with TToolProperties.Create(Application) do
  try
    with Tool do begin
      edName.Text := Caption;
      edDescription.Text := Description;
      SynApplication.Text := ApplicationName;
      SynParameters.Text := Parameters;
      SynWorkDir.Text := WorkingDirectory;
      cbContext.ItemIndex := Integer(Context);
      hkShortCut.HotKey := ShortCut;
      cbSaveFiles.ItemIndex := Integer(SaveFiles);
      cbStandardInput.ItemIndex := Integer(ProcessInput);
      cbStandardOutput.ItemIndex := Integer(ProcessOutput);
      cbCaptureOutput.Checked := CaptureOutput;
      cbHideConsole.Checked := ConsoleHidden;
      cbParseMessages.Checked := ParseMessages;
      cbParseTraceback.Checked := ParseTraceback;
      edMessagesFormat.Text := MessagesFormat;
      cbUTF8IO.Checked := Utf8IO;
      cbUseCustomEnv.Checked := UseCustomEnvironment;
      if UseCustomEnvironment then
        fEnvStrings.Assign(Environment)
      else
        GetEnvironmentVars(fEnvStrings);
    end;
    if IsExternalRun then begin
      Caption := _('External Run Properties');
      hkShortCut.Enabled := False;
      lbShortcut.Enabled := False;
      cbContext.Enabled := False;
      lbContext.Enabled := False;
    end;
    Result := (ShowModal = mrOK) and (edName.Text <> '');
    if Result then with Tool do begin
      Caption := edName.Text;
      Description := edDescription.Text;
      ApplicationName := SynApplication.Text;
      Parameters := SynParameters.Text;
      WorkingDirectory := SynWorkDir.Text;
      Context := TToolContext(cbContext.ItemIndex);
      ShortCut := hkShortCut.HotKey;
      SaveFiles := TSaveFiles(cbSaveFiles.ItemIndex);
      ProcessInput := TProcessStdInputOption(cbStandardInput.ItemIndex);
      ProcessOutput := TProcessStdOutputOption(cbStandardOutput.ItemIndex);
      CaptureOutput := cbCaptureOutput.Checked;
      ConsoleHidden := cbHideConsole.Checked;
      ParseMessages := cbParseMessages.Checked;
      ParseTraceback := cbParseTraceback.Checked;
      MessagesFormat := edMessagesFormat.Text;
      Utf8IO := cbUTF8IO.Checked;
      UseCustomEnvironment := cbUseCustomEnv.Checked;
      Environment.Clear;
      if UseCustomEnvironment then begin
        for i := 0 to lvItems.Items.Count - 1 do
          Environment.Add(lvItems.Items[i].Caption + '=' + lvItems.Items[i].SubItems[0]);
      end;
    end;
  finally
    Release;
  end;
end;

function EditToolItem(Item : TCollectionItem) : Boolean;
begin
  Result := EditTool((Item as TToolItem).ExternalTool);
end;

procedure TToolProperties.Filename1Click(Sender: TObject);
begin
  case (Sender as TMenuItem).Tag of
    0: edMessagesFormat.SelText := GrepFileNameParam;
    1: edMessagesFormat.SelText := GrepLineNumberParam;
    2: edMessagesFormat.SelText := GrepColumnNumberParam;
  end;
  edMessagesFormat.SetFocus;
end;

procedure TToolProperties.SynApplicationEnter(Sender: TObject);
begin
  CommandsDataModule.ParameterCompletion.Editor := SynApplication;
  CommandsDataModule.ModifierCompletion.Editor := SynApplication;
end;

procedure TToolProperties.SynParametersEnter(Sender: TObject);
begin
  CommandsDataModule.ParameterCompletion.Editor := SynParameters;
  CommandsDataModule.ModifierCompletion.Editor := SynParameters;
end;

procedure TToolProperties.SynWorkDirEnter(Sender: TObject);
begin
  CommandsDataModule.ParameterCompletion.Editor := SynWorkDir;
  CommandsDataModule.ModifierCompletion.Editor := SynWorkDir;
end;

procedure TToolProperties.FormCreate(Sender: TObject);
begin
  inherited;
  fEnvStrings := TStringList.Create;

  hkShortCut := TSynHotKey.Create(Self);
  with hkShortCut do
  begin
    Name := 'hkShortCut';
    Parent := GroupBox4;
    Left := PPIScale(86);
    Top := PPIScale(15);
    Width := PPIScale(125);
    Height := PPIScale(19);
    Hint := 'Allows you to specify a menu shortcut for the tool';
    HotKey := 0;
    InvalidKeys := [hcNone];
    Modifiers := [];
    TabOrder := 0;
    Color := StyleServices.GetSystemColor(clWindow);
    Font.Color := StyleServices.GetSystemColor(clWindowText);
  end;

  SynApplication.Color := StyleServices.GetSystemColor(clWindow);
  SynApplication.Font.Color := StyleServices.GetSystemColor(clWindowText);
  SynParameters.Color := StyleServices.GetSystemColor(clWindow);
  SynParameters.Font.Color := StyleServices.GetSystemColor(clWindowText);
  SynWorkDir.Color := StyleServices.GetSystemColor(clWindow);
  SynWorkDir.Font.Color := StyleServices.GetSystemColor(clWindowText);
end;

procedure TToolProperties.FormDestroy(Sender: TObject);
begin
  fEnvStrings.Free;
  CommandsDataModule.ParameterCompletion.Editor := nil;
  CommandsDataModule.ModifierCompletion.Editor := nil;
end;

procedure TToolProperties.btnWorkDirClick(Sender: TObject);
var
  Directories : TArray<string>;
begin
  if SelectDirectory('', Directories, [], _('Select working directory:')) then
  begin
    SynWorkDir.SelectAll;
    SynWorkDir.SelText := Directories[0];
    SynWorkDir.SetFocus;
  end;
end;

procedure TToolProperties.cbParseMessagesClick(Sender: TObject);
begin
  edMessagesFormat.Enabled := cbParseMessages.Checked;
  btnStdFormats.Enabled := cbParseMessages.Checked;
end;

procedure TToolProperties.btnAppDirClick(Sender: TObject);
begin
  with CommandsDataModule.dlgFileOpen do begin
    Title := _(SSelectApplication);
    Filter := 'Executable Files (*.exe;*.bat;*.cmd)|*.exe;*.bat;*.cmd|All files|*.*|';
    FileName := '';
    if Execute then begin
      SynApplication.SelectAll;
      SynApplication.SelText := FileName;
      SynApplication.SetFocus;
    end;
  end;
end;

procedure TToolProperties.actAddItemExecute(Sender: TObject);
Var
  Item : TListItem;
  i : Integer;
begin
  if edEnvName.Text <> '' then begin
    for i := 0 to lvItems.Items.Count - 1 do
      if CompareText(lvItems.Items[i].Caption, EdEnvName.Text) = 0 then begin
        Item := lvItems.Items[i];
        Item.Caption := EdEnvName.Text;
        Item.SubItems[0] := EdEnvValue.Text;
        Item.Selected := True;
        Item.MakeVisible(False);
        Exit;
      end;

    with lvItems.Items.Add() do begin
      Caption := edEnvName.Text;
      SubItems.Add(edEnvValue.Text);
      Selected := True;
      MakeVisible(False);
    end;
  end;end;

procedure TToolProperties.actDeleteItemExecute(Sender: TObject);
begin
  if lvItems.ItemIndex >= 0 then
    lvItems.Items.Delete(lvItems.ItemIndex);
end;

procedure TToolProperties.actUpdateItemExecute(Sender: TObject);
Var
  i : integer;
begin
  if (edEnvName.Text <> '') and (lvItems.ItemIndex >= 0) then begin
    for i := 0 to lvItems.Items.Count - 1 do
      if (CompareText(lvItems.Items[i].Caption, edEnvName.Text) = 0) and
         (i <> lvItems.ItemIndex) then
      begin
        Dialogs.MessageDlg(_(SSameName), mtError, [mbOK], 0);
        Exit;
      end;
    with lvItems.Items[lvItems.ItemIndex] do begin
      Caption := EdEnvName.Text;
      SubItems[0] := EdEnvValue.Text;
    end;
  end;
end;

procedure TToolProperties.lvItemsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Selected then begin
    edEnvName.Text := Item.Caption;
    edEnvValue.Text := Item.SubItems[0];
  end;
end;

procedure TToolProperties.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  actDeleteItem.Enabled := lvItems.ItemIndex >= 0;
  actAddItem.Enabled := edName.Text <> '';
  actUpdateItem.Enabled := (edName.Text <> '') and (lvItems.ItemIndex >= 0);
  Handled := True;
end;

procedure TToolProperties.btnHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TToolProperties.btnStdFormatsClick(Sender: TObject);
var
  button: TControl;
  lowerLeft: TPoint;
begin
  if Sender is TControl then
  begin
    button := TControl(Sender);
    lowerLeft := Point(0, button.Height);
    lowerLeft := button.ClientToScreen(lowerLeft);
    FormatsPopup.Popup(lowerLeft.X, lowerLeft.Y);
  end;
end;

procedure TToolProperties.FormShow(Sender: TObject);
Var
  i : integer;
begin
  lvItems.Items.Clear;
  lvItems.Items.BeginUpdate;
  try
    for i := 0 to fEnvStrings.Count - 1 do
      if fEnvStrings.Names[i] <> '' then
        with lvItems.Items.Add() do begin
          Caption := fEnvStrings.Names[i];
          SubItems.Add(fEnvStrings.Values[Caption]);
        end;
  finally
    lvItems.Items.EndUpdate;
  end;
end;

end.
