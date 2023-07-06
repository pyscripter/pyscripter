unit dlgRunConfiguration;

interface

uses
  WinApi.Windows,
  WinApi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  System.ImageList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  SynEdit,
  cPySupportTypes,
  cPyBaseDebugger,
  dlgPyIDEBase;

type
  TRunConfigurationForm = class(TPyIDEDlgBase)
    Panel1: TPanel;
    Bevel1: TBevel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    SynFileName: TSynEdit;
    SynParameters: TSynEdit;
    SynWorkDir: TSynEdit;
    gbRemoteEngine: TGroupBox;
    GroupBox3: TGroupBox;
    gbSaveOutput: TGroupBox;
    SynOutputFileName: TSynEdit;
    btnFileName: TButton;
    btnWorkDir: TButton;
    btnOutputFileName: TButton;
    btnRemoteFileName: TButton;
    cbReinitializeBeforeRun: TCheckBox;
    cbAppendToFile: TCheckBox;
    cbSaveOutput: TCheckBox;
    cbEngineType: TComboBox;
    Label5: TLabel;
    Label2: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label3: TLabel;
    Label1: TLabel;
    Label4: TLabel;
    edDescription: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    btnHelp: TButton;
    btnExternalRun: TButton;
    vilImages: TVirtualImageList;
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
  System.Math,
  System.IOUtils,
  Vcl.Themes,
  Vcl.FileCtrl,
  JvGnugettext,
  dlgToolProperties,
  dmResources,
  uHighlighterProcs,
  cProjectClasses,
  StringResources,
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
  with ResourcesDataModule.dlgFileOpen do begin
    Title := _(SSelectPythonScript);
    Filter := ResourcesDataModule.Highlighters.FileFilters + _(SFilterAllFiles);
    FileName := '';
    if ActiveProject.FileName <> '' then
      InitialDir := TPath.GetDirectoryName(ActiveProject.FileName);
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
  with ResourcesDataModule.dlgFileOpen do begin
    Title := _(SSelectOutputFile);
    Filter := _(SFilterAllFiles);
    FileName := 'output.log';
    if ActiveProject.FileName <> '' then
      InitialDir := TPath.GetDirectoryName(ActiveProject.FileName);
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
    SynFileName.Text := TSSHFileName.Format(Server, FileName);
    SynFileName.SetFocus;
  end;
end;

procedure TRunConfigurationForm.btnWorkDirClick(Sender: TObject);
var
  S: string;
  Directories : TArray<string>;
begin
  if ActiveProject.FileName <> '' then
    S := TPath.GetDirectoryName(ActiveProject.FileName);
  if SelectDirectory(S, Directories, [], _('Select working directory:')) then
  begin
    SynWorkDir.SelectAll;
    SynWorkDir.SelText := Directories[0];
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
end;

procedure TRunConfigurationForm.SynEditEnter(Sender: TObject);
begin
  ResourcesDataModule.ParameterCompletion.Editor := Sender as TSynEdit;
  ResourcesDataModule.ModifierCompletion.Editor := TSynEdit(Sender);
end;

end.
