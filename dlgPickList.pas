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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, CheckLst, Menus;

type
  TPickListDialog = class(TForm)
    CheckListBox: TCheckListBox;
    Panel2: TPanel;
    OKButton: TBitBtn;
    BitBtn2: TBitBtn;
    lbMessage: TLabel;
    PickListPopUp: TPopupMenu;
    mnSelectAll: TMenuItem;
    mnDeselectAll: TMenuItem;
    procedure mnDeselectAllClick(Sender: TObject);
    procedure mnSelectAllClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PickListDialog: TPickListDialog;

implementation

{$R *.dfm}

procedure TPickListDialog.mnSelectAllClick(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to CheckListBox.Items.Count - 1 do
    CheckListBox.Checked[i] := True;
end;

procedure TPickListDialog.mnDeselectAllClick(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to CheckListBox.Items.Count - 1 do
    CheckListBox.Checked[i] := False;
end;

end.
