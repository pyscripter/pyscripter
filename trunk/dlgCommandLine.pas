unit dlgCommandLine;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SynEdit, StdCtrls, ExtCtrls, Buttons, TB2Item, TBX, TBXExtItems,
  Menus, TBXDkPanels, SpTBXControls, SpTBXItem, dlgPyIDEBase;

type
  TCommandLineDlg = class(TPyIDEDlgBase)
    Panel: TSpTBXPanel;
    SynParameters: TSynEdit;
    OKButton: TSpTBXButton;
    BitBtn2: TSpTBXButton;
    HelpButton: TSpTBXButton;
    TBXButton1: TSpTBXButton;
    TBXPopupHistory: TSpTBXPopupMenu;
    PopupHistoryItem: TTBXMRUListItem;
    EmptyHistoryPopupItem: TSpTBXItem;
    cbUseCommandLine: TSpTBXCheckBox;
    Label1: TSpTBXLabel;
    Label3: TSpTBXLabel;
    procedure btnHelpClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SynParametersEnter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure TBXPopupHistoryPopup(Sender: TObject);
  private
    { Private declarations }
    procedure CommandLineMRUClick(Sender: TObject;  const Filename: string);
  public
    { Public declarations }
  end;

implementation

uses dmCommands;

{$R *.dfm}

procedure TCommandLineDlg.SynParametersEnter(Sender: TObject);
begin
  CommandsDataModule.ParameterCompletion.Editor := SynParameters;
  CommandsDataModule.ModifierCompletion.Editor := SynParameters;
end;

procedure TCommandLineDlg.TBXPopupHistoryPopup(Sender: TObject);
begin
  if PopupHistoryItem.MRUList.Items.Count > 0 then
    EmptyHistoryPopupItem.Visible := False;
end;

procedure TCommandLineDlg.FormCreate(Sender: TObject);
begin
  inherited;
  PopupHistoryItem.MRUList.OnClick := CommandLineMRUClick;
end;

procedure TCommandLineDlg.FormDestroy(Sender: TObject);
begin
  CommandsDataModule.ParameterCompletion.Editor := nil;
  CommandsDataModule.ModifierCompletion.Editor := nil;
  PopupHistoryItem.MRUList.OnClick := nil;
end;

procedure TCommandLineDlg.OKButtonClick(Sender: TObject);
begin
  if (SynParameters.Text <> '') and cbUseCommandLine.Checked then
    CommandsDataModule.CommandLineMRU.Add(SynParameters.Text);
end;

procedure TCommandLineDlg.btnHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TCommandLineDlg.CommandLineMRUClick(Sender: TObject;
  const Filename: string);
begin
  SynParameters.Text := Filename;
  SynParameters.SetFocus;
end;

end.
