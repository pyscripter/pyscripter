{-----------------------------------------------------------------------------
 Unit Name: dlgPickList
 Author:    Kiriakos Vlahos
 Date:      10-Mar-2006
 Purpose:   Generic Pick List dialog
 History:
-----------------------------------------------------------------------------}
unit dlgPickList;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.ImageList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  Vcl.CheckLst,
  Vcl.Menus,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  dlgPyIDEBase;

type
  TPickListDialog = class(TPyIDEDlgBase)
    PickListPopUp: TPopupMenu;
    mnSelectAll: TMenuItem;
    mnDeselectAll: TMenuItem;
    imgIcon: TImage;
    Panel2: TPanel;
    Bevel1: TBevel;
    Panel1: TPanel;
    btnSelectAll: TButton;
    btnDeselectAll: TButton;
    btnOk: TButton;
    btnCancel: TButton;
    lbMessage: TLabel;
    CheckListBox: TCheckListBox;
    vilImages: TVirtualImageList;
    procedure mnDeselectAllClick(Sender: TObject);
    procedure mnSelectAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetScrollWidth;
  end;

var
  PickListDialog: TPickListDialog;

implementation

uses
  System.Math,
  dmResources;

{$R *.dfm}

procedure TPickListDialog.mnSelectAllClick(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to CheckListBox.Items.Count - 1 do
    CheckListBox.Checked[i] := True;
end;

procedure TPickListDialog.SetScrollWidth;
var
  i: integer;
  ItemMaxWidth: integer;
begin
  ItemMaxWidth := 0;
  with CheckListBox do
  begin
    //  Calculate the Max Length
    for i := 0 to CheckListBox.Items.Count - 1 do
      ItemMaxWidth := Max(CheckListBox.Canvas.TextWidth(CheckListBox.Items[i]),
        ItemMaxWidth);
    ScrollWidth := ItemMaxWidth + 5;
  end;
end;

procedure TPickListDialog.FormCreate(Sender: TObject);
begin
  inherited;
  imgIcon.Picture.Icon.Handle := LoadIcon(0, IDI_INFORMATION);
end;

procedure TPickListDialog.mnDeselectAllClick(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to CheckListBox.Items.Count - 1 do
    CheckListBox.Checked[i] := False;
end;

end.
