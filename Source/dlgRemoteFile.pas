unit dlgRemoteFile;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.UITypes,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  dlgPyIDEBase,
  dmCommands, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TRemoteFileDialog = class(TPyIDEDlgBase)
    Panel1: TPanel;
    Label1: TLabel;
    edFileName: TEdit;
    Label2: TLabel;
    cbSSHConfigs: TComboBox;
    Panel2: TPanel;
    OKButton: TButton;
    CancelButton: TButton;
    HelpButton: TButton;
    btnSSHServersSetup: TButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnSSHServersSetupClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TRemoteFileDialogType = (rfdOpen, rfdSave, rfdAdd, rfdSelect);

  function ExecuteRemoteFileDialog(var FileName, SSHServerName: string;
    DialogType: TRemoteFileDialogType): boolean;

implementation

Uses
  StringResources,
  cSSHSupport,
  JvGnugettext;

{$R *.dfm}


function ExecuteRemoteFileDialog(var FileName, SSHServerName: string;
  DialogType: TRemoteFileDialogType): boolean;
begin
  with TRemoteFileDialog.Create(Application.MainForm) do begin
    edFileName.Text := FileName;
    FillSSHConfigNames(cbSSHConfigs.Items);
    if SSHServerName <> '' then
      cbSSHConfigs.ItemIndex := cbSSHConfigs.Items.IndexOf(SSHServerName);
    case DialogType of
      rfdOpen: Caption := SRemoteFileOpen;
      rfdSave: Caption := SRemoteFileSave;
      rfdAdd: Caption := SRemoteFileAdd;
      rfdSelect: Caption := SRemoteFileSelect;
    end;

    Result := ShowModal = mrOK;

    if Result then begin
      FileName := edFileName.Text;
      if cbSSHConfigs.ItemIndex >= 0 then
        SSHServerName := cbSSHConfigs.Items[cbSSHConfigs.ItemIndex];
    end;

    Release;
  end;
end;

procedure TRemoteFileDialog.btnSSHServersSetupClick(Sender: TObject);
begin
  if EditSSHServers then
    FillSSHConfigNames(cbSSHConfigs.Items);
end;

procedure TRemoteFileDialog.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := False;
  if (ModalResult = mrOK) and (edFileName.Text = '') then
    Vcl.Dialogs.MessageDlg(_(SErrorEmptyPath), mtError, [mbAbort], 0)
  else if (ModalResult = mrOK) and (cbSSHConfigs.ItemIndex < 0) then
    Vcl.Dialogs.MessageDlg(_(SErrorEmptySSH), mtError, [mbAbort], 0)
  else
    CanClose := True;
end;

procedure TRemoteFileDialog.HelpButtonClick(Sender: TObject);
begin
  if HelpContext <> 0 then
    Application.HelpContext(HelpContext);
end;

end.
