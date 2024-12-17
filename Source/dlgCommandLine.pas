unit dlgCommandLine;

interface

uses
  System.Classes,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.StdCtrls,
  SynEdit,
  TB2Item,
  SpTBXControls,
  SpTBXItem,
  SpTBXMDIMRU,
  dlgPyIDEBase;

type
  TCommandLineDlg = class(TPyIDEDlgBase)
    Panel: TPanel;
    SynParameters: TSynEdit;
    TBXButton1: TSpTBXButton;
    TBXPopupHistory: TSpTBXPopupMenu;
    EmptyHistoryPopupItem: TSpTBXItem;
    mnCommandHistoryMRU: TSpTBXMRUListItem;
    cbUseCommandLine: TCheckBox;
    Label1: TLabel;
    Label3: TLabel;
    OKButton: TButton;
    btnCancel: TButton;
    btnHelp: TButton;
    procedure btnHelpClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SynParametersEnter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure TBXPopupHistoryPopup(Sender: TObject);
    procedure mnCommandHistoryMRUClick(Sender: TObject;
      const Filename: string);
  private
    const FBasePath = 'Command Line';
    class var FParameters: string;
    class var FInUse: Boolean;
    class var FReadFromStorage: Boolean;
  public
    class procedure ReadFromAppStorage;
    class function Execute: Boolean;
  end;

function CommandLineParams: string;

implementation

uses
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Themes,
  JvAppIniStorage,
  dmResources,
  uEditAppIntfs;

{$R *.dfm}

procedure TCommandLineDlg.SynParametersEnter(Sender: TObject);
begin
  ResourcesDataModule.ParameterCompletion.Editor := SynParameters;
  ResourcesDataModule.ModifierCompletion.Editor := SynParameters;
end;

procedure TCommandLineDlg.TBXPopupHistoryPopup(Sender: TObject);
begin
  if mnCommandHistoryMRU.Count > 0 then
    EmptyHistoryPopupItem.Visible := False;
end;

class function TCommandLineDlg.Execute: Boolean;
begin
  with TCommandLineDlg.Create(Application.MainForm) do begin
    ReadFromAppStorage;
    SynParameters.Text := FParameters;
    cbUseCommandLine.Checked := FInUse;

    Result := ShowModal = mrOk;
    if Result then begin
      FParameters := SynParameters.Text;
      FInUse := cbUseCommandLine.Checked;
      GI_PyIDEServices.AppStorage.WriteString(FBasePath + '\Parameters', FParameters);
      GI_PyIDEServices.AppStorage.WriteBoolean(FBasePath + '\InUse', FInUse);
    end;
    Free;
  end;
end;

procedure TCommandLineDlg.FormCreate(Sender: TObject);
begin
  inherited;
  mnCommandHistoryMRU.LoadFromIni(
    (GI_PyIDEServices.AppStorage as TJvAppIniFileStorage).IniFile,
      'CommandLine MRU');
  SynParameters.Font.Color := StyleServices.GetSystemColor(clWindowText);
  SynParameters.Color := StyleServices.GetSystemColor(clWindow);
end;

procedure TCommandLineDlg.FormDestroy(Sender: TObject);
begin
  mnCommandHistoryMRU.SaveToIni(
    (GI_PyIDEServices.AppStorage as TJvAppIniFileStorage).IniFile,
      'CommandLine MRU');
end;

procedure TCommandLineDlg.mnCommandHistoryMRUClick(Sender: TObject;
  const Filename: string);
begin
  SynParameters.Text := Filename;
  SynParameters.SetFocus;
end;

procedure TCommandLineDlg.OKButtonClick(Sender: TObject);
begin
  if (SynParameters.Text <> '') and cbUseCommandLine.Checked then
    mnCommandHistoryMRU.MRUAdd(SynParameters.Text);
end;

class procedure TCommandLineDlg.ReadFromAppStorage;
begin
  if not FReadFromStorage then
  begin
    FParameters :=
      GI_PyIDEServices.AppStorage.ReadString(FBasePath + '\Parameters', '');
    FInUse :=
      GI_PyIDEServices.AppStorage.ReadBoolean(FBasePath + '\InUse', False);
    FReadFromStorage := True;
  end;
end;

procedure TCommandLineDlg.btnHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

function CommandLineParams: string;
begin
  TCommandLineDlg.ReadFromAppStorage;
  if TCommandLineDlg.FInUse then
    Result := TCommandLineDlg.FParameters
  else
    Result := '';
end;

end.
