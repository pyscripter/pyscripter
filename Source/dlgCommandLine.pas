unit dlgCommandLine;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Buttons,
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
  public
    class var Parameters: string;
    class var InUse: Boolean;
    class var ReadFromStorage: Boolean;
    class procedure ReadFromAppStorage;
    class function Execute: Boolean;
  end;

function CommandLineParams: string;

implementation

uses
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
    SynParameters.Text := Parameters;
    cbUseCommandLine.Checked := InUse;

    Result := ShowModal = mrOk;
    if Result then begin
      Parameters := SynParameters.Text;
      InUse := cbUseCommandLine.Checked;
      GI_PyIDEServices.AppStorage.WriteString(FBasePath + '\Parameters', Parameters);
      GI_PyIDEServices.AppStorage.WriteBoolean(FBasePath + '\InUse', InUse);
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
  if not ReadFromStorage then
  begin
    Parameters :=
      GI_PyIDEServices.AppStorage.ReadString(FBasePath + '\Parameters', '');
    InUse :=
      GI_PyIDEServices.AppStorage.ReadBoolean(FBasePath + '\InUse', False);
    ReadFromStorage := True;
  end;
end;

procedure TCommandLineDlg.btnHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

function CommandLineParams: string;
begin
  TCommandLineDlg.ReadFromAppStorage;
  if TCommandLineDlg.InUse then
    Result := TCommandLineDlg.Parameters
  else
    Result := '';
end;

end.
