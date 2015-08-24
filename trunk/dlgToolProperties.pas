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
  Windows, Messages, SysUtils, Variants, Classes, Controls, Forms,
  Dialogs, cTools, StdCtrls, SynEdit, Menus,
  ActnList, SpTBXControls, SpTBXEditors,
  dlgPyIDEBase, SpTBXItem, SpTBXTabs, TB2Item, MPCommonObjects,
  EasyListview, MPCommonUtilities, ComCtrls, System.Actions;

type
  TToolProperties = class(TPyIDEDlgBase)
    Panel1: TSpTBXPanel;
    FormatsPopup: TPopupMenu;
    Filename1: TMenuItem;
    Linenumber1: TMenuItem;
    Columnnumber1: TMenuItem;
    GroupBox1: TSpTBXGroupBox;
    GroupBox2: TSpTBXGroupBox;
    SynApplication: TSynEdit;
    SynParameters: TSynEdit;
    SynWorkDir: TSynEdit;
    GroupBox4: TSpTBXGroupBox;
    GroupBox3: TSpTBXGroupBox;
    GroupBox5: TSpTBXGroupBox;
    GroupBox6: TSpTBXGroupBox;
    btnOK: TSpTBXButton;
    btnCancel: TSpTBXButton;
    btnAppDir: TSpTBXButton;
    btnWorkDir: TSpTBXButton;
    btnStdFormats: TSpTBXButton;
    btnHelp: TSpTBXButton;
    cbCaptureOutput: TSpTBXCheckBox;
    cbParseMessages: TSpTBXCheckBox;
    cbParseTraceback: TSpTBXCheckBox;
    cbHideConsole: TSpTBXCheckBox;
    cbWaitForTermination: TSpTBXCheckBox;
    cbUseCustomEnv: TSpTBXCheckBox;
    btnAdd: TSpTBXButton;
    btnDelete: TSpTBXButton;
    btnMoveUp: TSpTBXButton;
    btnMoveDown: TSpTBXButton;
    btnUpdate: TSpTBXButton;
    ActionList: TActionList;
    actUpdateItem: TAction;
    actMoveDown: TAction;
    actMoveUp: TAction;
    actDeleteItem: TAction;
    actAddItem: TAction;
    Label1: TSpTBXLabel;
    Label5: TSpTBXLabel;
    Label17: TSpTBXLabel;
    Label2: TSpTBXLabel;
    Label6: TSpTBXLabel;
    Label7: TSpTBXLabel;
    Label3: TSpTBXLabel;
    lbShortcut: TSpTBXLabel;
    lbContext: TSpTBXLabel;
    Label13: TSpTBXLabel;
    Label10: TSpTBXLabel;
    Label11: TSpTBXLabel;
    Label12: TSpTBXLabel;
    Label9: TSpTBXLabel;
    Label15: TSpTBXLabel;
    Label16: TSpTBXLabel;
    hkShortCut: THotKey;
    seTimeout: TSpTBXSpinEdit;
    edName: TSpTBXEdit;
    edDescription: TSpTBXEdit;
    edMessagesFormat: TSpTBXEdit;
    edEnvName: TSpTBXEdit;
    edEnvValue: TSpTBXEdit;
    cbContext: TSpTBXComboBox;
    cbSaveFiles: TSpTBXComboBox;
    cbStandardInput: TSpTBXComboBox;
    cbStandardOutput: TSpTBXComboBox;
    TabControl: TSpTBXTabControl;
    SpTBXTabItem1: TSpTBXTabItem;
    tabProperties: TSpTBXTabSheet;
    SpTBXTabItem2: TSpTBXTabItem;
    tabEnvironment: TSpTBXTabSheet;
    lvItems: TEasyListview;
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
    procedure actMoveUpExecute(Sender: TObject);
    procedure actMoveDownExecute(Sender: TObject);
    procedure actUpdateItemExecute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure btnHelpClick(Sender: TObject);
    procedure cbParseMessagesClick(Sender: TObject);
    procedure lvItemsItemSelectionsChanged(Sender: TCustomEasyListview);
    procedure lvItemsColumnClick(Sender: TCustomEasyListview;
      Button: TCommonMouseButton; ShiftState: TShiftState;
      const Column: TEasyColumn);
  private
    { Private declarations }
    fEnvStrings : TStrings;
  public
    { Public declarations }
  end;

  function EditTool(Tool : TExternalTool; IsExternalRun : Boolean = False) : Boolean;

implementation

uses dmCommands, JvBrowseFolder, JclSysInfo, gnugettext, StringResources;

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
      seTimeOut.Value := TimeOut;
      cbWaitForTermination.Checked := WaitForTerminate;
      cbHideConsole.Checked := ConsoleHidden;
      cbParseMessages.Checked := ParseMessages;
      cbParseTraceback.Checked := ParseTraceback;
      edMessagesFormat.Text := MessagesFormat;
      cbUseCustomEnv.Checked := UseCustomEnvironment;
      if UseCustomEnvironment then
        fEnvStrings.Assign(Environment)
      else
        GetEnvironmentVars(fEnvStrings);
    end;
    if IsExternalRun then begin
      Caption := 'External Run Properties';
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
      TimeOut := Trunc(seTimeOut.Value);
      WaitForTerminate := cbWaitForTermination.Checked;
      ConsoleHidden := cbHideConsole.Checked;
      ParseMessages := cbParseMessages.Checked;
      ParseTraceback := cbParseTraceback.Checked;
      MessagesFormat := edMessagesFormat.Text;
      UseCustomEnvironment := cbUseCustomEnv.Checked;
      Environment.Clear;
      if UseCustomEnvironment then begin
        for i := 0 to lvItems.Items.Count - 1 do
          Environment.Add(lvItems.Items[i].Caption + '=' + lvItems.Items[i].Captions[1]);
      end;
    end;
  finally
    Release;
  end;
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
end;

procedure TToolProperties.FormDestroy(Sender: TObject);
begin
  fEnvStrings.Free;
  CommandsDataModule.ParameterCompletion.Editor := nil;
  CommandsDataModule.ModifierCompletion.Editor := nil;
end;

procedure TToolProperties.btnWorkDirClick(Sender: TObject);
var
  S: string;
begin
  S := '';
  if BrowseDirectory(S, 'Select working directory:', 0) then begin
    SynWorkDir.SelectAll;
    SynWorkDir.SelText := S;
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
  Item : TEasyItem;
  i : Integer;
begin
  if edEnvName.Text <> '' then begin
    for i := 0 to lvItems.Items.Count - 1 do
      if CompareText(lvItems.Items[i].Caption, EdEnvName.Text) = 0 then begin
        Item := lvItems.Items[i];
        Item.Caption := EdEnvName.Text;
        Item.Captions[1] := EdEnvValue.Text;
        Item.Selected := True;
        Item.MakeVisible(emvAuto);
        Exit;
      end;

    with lvItems.Items.Add() do begin
      Caption := edEnvName.Text;
      Captions[1] := edEnvValue.Text;
      Selected := True;
      MakeVisible(emvAuto);
    end;
  end;end;

procedure TToolProperties.actDeleteItemExecute(Sender: TObject);
begin
  if Assigned(lvItems.Selection.First()) then
    lvItems.Items.Delete(lvItems.Selection.First.Index);
end;

procedure TToolProperties.actMoveUpExecute(Sender: TObject);
Var
  Name, Value : string;
  Index : integer;
begin
  if Assigned(lvItems.Selection.First()) and (lvItems.Selection.First.Index >= 1) then begin
    lvItems.Header.Columns[0].SortDirection := esdNone;

    Index := lvItems.Selection.First.Index;
    Name := lvItems.Items[Index].Caption;
    Value := lvItems.Items[Index].Captions[1];
    lvItems.Items.Delete(Index);

    with lvItems.Items.Insert(Index - 1) do begin
      Caption := Name;
      Captions[1] := Value;
      Selected := True;
    end;
  end;
end;

procedure TToolProperties.actMoveDownExecute(Sender: TObject);
Var
  Name, Value : string;
  Index : integer;
begin
  if Assigned(lvItems.Selection.First()) and
    (lvItems.Selection.First.Index < lvItems.Items.Count - 1) then
  begin
    lvItems.Header.Columns[0].SortDirection := esdNone;

    Index := lvItems.Selection.First.Index;
    Name := lvItems.Items[Index].Caption;
    Value := lvItems.Items[Index].Captions[1];
    lvItems.Items.Delete(Index);

    with lvItems.Items.Insert(Index + 1) do begin
      Caption := Name;
      Captions[1] := Value;
      Selected := True;
    end;
  end;
end;

procedure TToolProperties.actUpdateItemExecute(Sender: TObject);
Var
  i : integer;
begin
  if (edEnvName.Text <> '') and Assigned(lvItems.Selection.First())  then begin
    for i := 0 to lvItems.Items.Count - 1 do
      if (CompareText(lvItems.Items[i].Caption, edEnvName.Text) = 0) and
         (i <> lvItems.Selection.First.Index) then
      begin
        Dialogs.MessageDlg(_(SSameName), mtError, [mbOK], 0);
        Exit;
      end;
    with lvItems.Items[lvItems.Selection.First.Index] do begin
      Caption := EdEnvName.Text;
      Captions[1] := EdEnvValue.Text;
    end;
  end;
end;

procedure TToolProperties.lvItemsColumnClick(Sender: TCustomEasyListview;
  Button: TCommonMouseButton; ShiftState: TShiftState;
  const Column: TEasyColumn);
begin
  if Column.Clickable then lvItems.Sort.SortAll;
end;

procedure TToolProperties.lvItemsItemSelectionsChanged(
  Sender: TCustomEasyListview);
begin
 if Assigned(lvItems.Selection.First()) then begin
    edEnvName.Text := lvItems.Selection.First.Caption;
    edEnvValue.Text := lvItems.Selection.First.Captions[1];
  end;
end;

procedure TToolProperties.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  actDeleteItem.Enabled := Assigned(lvItems.Selection.First());
  actMoveUp.Enabled :=  Assigned(lvItems.Selection.First()) and  (lvItems.Selection.First.Index >= 1);
  actMoveDown.Enabled := Assigned(lvItems.Selection.First()) and
                         (lvItems.Selection.First.Index < lvItems.Items.Count - 1);
  actAddItem.Enabled := edName.Text <> '';
  actUpdateItem.Enabled := (edName.Text <> '') and Assigned(lvItems.Selection.First());
  Handled := True;
end;

procedure TToolProperties.btnHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TToolProperties.FormShow(Sender: TObject);
Var
  i : integer;
begin
  lvItems.Items.Clear;
  lvItems.BeginUpdate;
  try
    for i := 0 to fEnvStrings.Count - 1 do
      if fEnvStrings.Names[i] <> '' then
        with lvItems.Items.Add() do begin
          Caption := fEnvStrings.Names[i];
          Captions[1] := fEnvStrings.Values[Caption];
        end;
  finally
    lvItems.EndUpdate;
  end;
end;

end.
