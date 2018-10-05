unit dlgRunConfiguration;

interface

uses
  WinApi.Windows,
  WinApi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  SpTBXEditors,
  SpTBXControls,
  SpTBXItem,
  cPySupportTypes,
  cPyBaseDebugger,
  SynEdit,
  dlgPyIDEBase;

type
  TRunConfigurationForm = class(TPyIDEDlgBase)
    Panel1: TSpTBXPanel;
    Bevel1: TBevel;
    btnOK: TSpTBXButton;
    btnCancel: TSpTBXButton;
    btnHelp: TSpTBXButton;
    GroupBox1: TSpTBXGroupBox;
    GroupBox2: TSpTBXGroupBox;
    SynFileName: TSynEdit;
    SynParameters: TSynEdit;
    SynWorkDir: TSynEdit;
    gbRemoteEngine: TSpTBXGroupBox;
    cbReinitializeBeforeRun: TSpTBXCheckBox;
    GroupBox3: TSpTBXGroupBox;
    btnExternalRun: TSpTBXButton;
    gbSaveOutput: TSpTBXGroupBox;
    cbAppendToFile: TSpTBXCheckBox;
    SynOutputFileName: TSynEdit;
    cbSaveOutput: TSpTBXCheckBox;
    Label5: TSpTBXLabel;
    Label2: TSpTBXLabel;
    Label6: TSpTBXLabel;
    Label7: TSpTBXLabel;
    Label3: TSpTBXLabel;
    Label1: TSpTBXLabel;
    Label4: TSpTBXLabel;
    cbEngineType: TSpTBXComboBox;
    edDescription: TSpTBXEdit;
    btnFileName: TButton;
    btnWorkDir: TButton;
    btnOutputFileName: TButton;
    btnRemoteFileName: TButton;
    procedure btnExternalRunClick(Sender: TObject);
    procedure SynEditEnter(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnFileNameClick(Sender: TObject);
    procedure btnWorkDirClick(Sender: TObject);
    procedure btnOutputFileNameClick(Sender: TObject);
    procedure cbEngineTypeChange(Sender: TObject);
    procedure cbSaveOutputClick(Sender: TObject);
    procedure btnRemoteFileNameClick(Sender: TObject);
  private
    { Private declarations }
    fRunConfig : TRunConfiguration;
  public
    { Public declarations }
  end;

function EditRunConfiguration(ARunConfig : TRunConfiguration) : Boolean;

implementation

uses
  Math, dlgToolProperties, dmCommands, uHighlighterProcs, cProjectClasses,
  StringResources, JvGnugettext, Vcl.Themes, Vcl.FileCtrl,
  dlgRemoteFile,
  cSSHSupport;

{$R *.dfm}

{ TRunConfigurationForm }

function EditRunConfiguration(ARunConfig : TRunConfiguration) : Boolean;
begin
  Result := False;
  if not Assigned(ARunConfig) then Exit;

  with TRunConfigurationForm.Create(Application) do
  try
    fRunConfig.Assign(ARunConfig);
    with fRunConfig do begin
      edDescription.Text := Description;
      SynFileName.Text := ScriptName;
      SynParameters.Text := Parameters;
      SynWorkDir.Text := WorkingDir;
      cbEngineType.ItemIndex := Integer(EngineType);
      cbReinitializeBeforeRun.Checked := ReinitializeBeforeRun;
      cbEngineTypeChange(nil);
      cbSaveOutput.Checked := WriteOutputToFile;
      SynOutputFileName.Text := OutputFileName;
      cbAppendToFile.Checked := AppendToFile;
      cbSaveOutputClick(nil);
    end;
    Result := (ShowModal = mrOK);
    if Result then with fRunConfig do begin
      Description := edDescription.Text;
      ScriptName := SynFileName.Text;
      Parameters := SynParameters.Text;
      WorkingDir := SynWorkDir.Text;
      EngineType := TPythonEngineType(Max(cbEngineType.ItemIndex, 0));
      ReinitializeBeforeRun := cbReinitializeBeforeRun.Checked;
      WriteOutputToFile := cbSaveOutput.Checked;
      OutputFileName := SynOutputFileName.Text;
      AppendToFile := cbAppendToFile.Checked;

      ARunConfig.Assign(fRunConfig);
    end;
  finally
    Release;
  end;
end;

procedure TRunConfigurationForm.btnExternalRunClick(Sender: TObject);
begin
  EditTool(fRunConfig.ExternalRun, True);
end;

procedure TRunConfigurationForm.btnFileNameClick(Sender: TObject);
begin
  with CommandsDataModule.dlgFileOpen do begin
    Title := _(SSelectPythonScript);
    Filter := GetHighlightersFilter(CommandsDataModule.Highlighters) + _(SFilterAllFiles);
    FileName := '';
    if ActiveProject.FileName <> '' then
      InitialDir := ExtractFileDir(ActiveProject.FileName);
    if Execute then begin
      SynFileName.SelectAll;
      SynFileName.Text := FileName;
      SynFileName.SetFocus;
    end;
  end;
end;

procedure TRunConfigurationForm.btnHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TRunConfigurationForm.btnOutputFileNameClick(Sender: TObject);
Var
  OldOpenOptions : TOpenOptions;
begin
  with CommandsDataModule.dlgFileOpen do begin
    Title := _(SSelectOutputFile);
    Filter := _(SFilterAllFiles);
    FileName := 'output.log';
    if ActiveProject.FileName <> '' then
      InitialDir := ExtractFileDir(ActiveProject.FileName);
    OldOpenOptions := Options;
    Options := Options - [ofFileMustExist];
    try
      if Execute then begin
        SynOutputFileName.SelectAll;
        SynOutputFileName.Text := FileName;
        SynOutputFileName.SetFocus;
      end;
    finally
      Options := OldOpenOptions;
    end;
  end;
end;

procedure TRunConfigurationForm.btnRemoteFileNameClick(Sender: TObject);
Var
  Server, FileName: string;
begin
  if ExecuteRemoteFileDialog(FileName, Server, rfdSelect) then begin
    SynFileName.SelectAll;
    SynFileName.Text := TUnc.Format(Server, FileName);
    SynFileName.SetFocus;
  end;
end;

procedure TRunConfigurationForm.btnWorkDirClick(Sender: TObject);
var
  S: string;
begin
  if ActiveProject.FileName <> '' then
    S := ExtractFileDir(ActiveProject.FileName);
  if SelectDirectory(_('Select working directory:'), '', S, [], Self) then begin
    SynWorkDir.SelectAll;
    SynWorkDir.SelText := S;
    SynWorkDir.SetFocus;
  end;
end;

procedure TRunConfigurationForm.cbEngineTypeChange(Sender: TObject);
begin
  cbReinitializeBeforeRun.Enabled := cbEngineType.ItemIndex > 0;
end;

procedure TRunConfigurationForm.cbSaveOutputClick(Sender: TObject);
begin
  SynOutputFileName.Enabled := cbSaveOutput.Checked;
  cbAppendToFile.Enabled := cbSaveOutput.Checked;
  btnOutputFileName.Enabled := cbSaveOutput.Checked;
end;

procedure TRunConfigurationForm.FormCreate(Sender: TObject);
Var
  SynEditArray : TArray<TSynEdit>;
  SynEdit : TSynEdit;
begin
  inherited;
  fRunConfig := TRunConfiguration.Create;

  SynEditArray := [SynFileName, SynParameters, SynWorkDir, SynOutputFileName];
  for SynEdit in SynEditArray do
  begin
    SynEdit.Color := StyleServices.GetSystemColor(clWindow);
    SynEdit.Font.Color := StyleServices.GetSystemColor(clWindowText);
  end;
end;

procedure TRunConfigurationForm.FormDestroy(Sender: TObject);
begin
  fRunConfig.Free;
  CommandsDataModule.ParameterCompletion.Editor := nil;
  CommandsDataModule.ModifierCompletion.Editor := nil;
end;

procedure TRunConfigurationForm.SynEditEnter(Sender: TObject);
var
  ASynEdit : TSynEdit;
begin
  ASynEdit := Sender as TSynEdit;
  CommandsDataModule.ParameterCompletion.Editor := ASynEdit;
  CommandsDataModule.ModifierCompletion.Editor := ASynEdit;
end;

end.
