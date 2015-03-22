unit dlgCommandLine;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  SynEdit, Buttons, TB2Item, 
  Menus, SpTBXControls, SpTBXItem, dlgPyIDEBase, SpTBXMDIMRU;

type
  TCommandLineDlg = class(TPyIDEDlgBase)
    Panel: TSpTBXPanel;
    SynParameters: TSynEdit;
    OKButton: TSpTBXButton;
    BitBtn2: TSpTBXButton;
    HelpButton: TSpTBXButton;
    TBXButton1: TSpTBXButton;
    TBXPopupHistory: TSpTBXPopupMenu;
    EmptyHistoryPopupItem: TSpTBXItem;
    cbUseCommandLine: TSpTBXCheckBox;
    Label1: TSpTBXLabel;
    Label3: TSpTBXLabel;
    mnCommandHistoryMRU: TSpTBXMRUListItem;
    procedure btnHelpClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SynParametersEnter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure TBXPopupHistoryPopup(Sender: TObject);
    procedure mnCommandHistoryMRUClick(Sender: TObject;
      const Filename: WideString);
  public
    { Public declarations }
  end;

implementation

uses dmCommands, frmPyIDEMain;

{$R *.dfm}

procedure TCommandLineDlg.SynParametersEnter(Sender: TObject);
begin
  CommandsDataModule.ParameterCompletion.Editor := SynParameters;
  CommandsDataModule.ModifierCompletion.Editor := SynParameters;
end;

procedure TCommandLineDlg.TBXPopupHistoryPopup(Sender: TObject);
begin
  if mnCommandHistoryMRU.Count > 0 then
    EmptyHistoryPopupItem.Visible := False;
end;

procedure TCommandLineDlg.FormCreate(Sender: TObject);
begin
  inherited;
  mnCommandHistoryMRU.LoadFromIni(PyIDEMainForm.AppStorage.IniFile, 'CommandLine MRU');
end;

procedure TCommandLineDlg.FormDestroy(Sender: TObject);
begin
  CommandsDataModule.ParameterCompletion.Editor := nil;
  CommandsDataModule.ModifierCompletion.Editor := nil;
  mnCommandHistoryMRU.SaveToIni(PyIDEMainForm.AppStorage.IniFile, 'CommandLine MRU');
end;

procedure TCommandLineDlg.mnCommandHistoryMRUClick(Sender: TObject;
  const Filename: WideString);
begin
  SynParameters.Text := Filename;
  SynParameters.SetFocus;
end;

procedure TCommandLineDlg.OKButtonClick(Sender: TObject);
begin
  if (SynParameters.Text <> '') and cbUseCommandLine.Checked then
    mnCommandHistoryMRU.MRUAdd(SynParameters.Text);
end;

procedure TCommandLineDlg.btnHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

end.
