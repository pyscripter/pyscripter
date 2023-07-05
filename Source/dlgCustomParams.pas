unit dlgCustomParams;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.UITypes,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Actions,
  System.ImageList,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ActnList,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  SynEdit,
  dlgPyIDEBase;

type
  TCustomizeParams = class(TPyIDEDlgBase)
    Panel: TPanel;
    TBXButton1: TButton;
    TBXButton3: TButton;
    TBXButton4: TButton;
    TBXButton5: TButton;
    TBXButton2: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    ActionList: TActionList;
    actUpdateItem: TAction;
    actMoveDown: TAction;
    actMoveUp: TAction;
    actDeleteItem: TAction;
    actAddItem: TAction;
    Label3: TLabel;
    Label4: TLabel;
    GroupBox1: TGroupBox;
    SynValue: TSynEdit;
    Label1: TLabel;
    Label2: TLabel;
    edName: TEdit;
    lvItems: TListview;
    vilImages: TVirtualImageList;
    procedure FormShow(Sender: TObject);
    procedure edNameKeyPress(Sender: TObject; var Key: Char);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actAddItemExecute(Sender: TObject);
    procedure actDeleteItemExecute(Sender: TObject);
    procedure actUpdateItemExecute(Sender: TObject);
    procedure actMoveUpExecute(Sender: TObject);
    procedure actMoveDownExecute(Sender: TObject);
    procedure lvItemsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetItems(List : TStrings);
    procedure GetItems(List : TStrings);
  end;

implementation

uses
  Vcl.Themes,
  Vcl.Graphics,
  JvGnugettext,
  dmResources,
  StringResources,
  uCommonFunctions;

{$R *.dfm}

procedure TCustomizeParams.FormShow(Sender: TObject);
begin
  ResourcesDataModule.ParameterCompletion.Editor := SynValue;
  ResourcesDataModule.ModifierCompletion.Editor := SynValue;
  // Styling
  SynValue.Font.Color := StyleServices.GetSystemColor(clWindowText);
  SynValue.Color := StyleServices.GetSystemColor(clWindow);
end;

procedure TCustomizeParams.edNameKeyPress(Sender: TObject; var Key: Char);
begin
  if not CharInset(Key, ['a'..'z', 'A'..'Z', '0'..'9', #8]) then
    Key := #0;
  inherited;
end;

procedure TCustomizeParams.GetItems(List: TStrings);
Var
 i : integer;
begin
  List.Clear;
  List.BeginUpdate;
  try
    for i := 0 to lvItems.Items.Count - 1 do
      List.Add(lvItems.Items[i].Caption + '=' + lvItems.Items[i].SubItems[0]);
  finally
    List.EndUpdate;
  end;
end;

procedure TCustomizeParams.SetItems(List: TStrings);
Var
 i : integer;
begin
  lvItems.Items.Clear;
  for i := 0 to List.Count - 1 do
    with lvItems.Items.Add() do begin
      Caption := List.Names[i];
      SubItems.Add(List.Values[List.Names[i]]);
    end;
end;

procedure TCustomizeParams.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  actDeleteItem.Enabled := lvItems.ItemIndex >= 0;
  actMoveUp.Enabled := lvItems.ItemIndex >= 1;
  actMoveDown.Enabled := (lvItems.ItemIndex >= 0) and
                         (lvItems.ItemIndex < lvItems.Items.Count - 1);
  actAddItem.Enabled := edName.Text <> '';
  actUpdateItem.Enabled := (edName.Text <> '') and (lvItems.ItemIndex >= 0);
  Handled := True;
end;

procedure TCustomizeParams.actAddItemExecute(Sender: TObject);
Var
  Item : TListItem;
  i : Integer;
begin
  if edName.Text <> '' then begin
    for i := 0 to lvItems.Items.Count - 1 do
      if CompareText(lvItems.Items[i].Caption, edName.Text) = 0 then begin
        Item := lvItems.Items[i];
        Item.Caption := EdName.Text;
        Item.SubItems[0] := SynValue.Text;
        Item.Selected := True;
        Item.MakeVisible(False);
        Exit;
      end;

    with lvItems.Items.Add() do begin
      Caption := edName.Text;
      SubItems.Add(SynValue.Text);
      Selected := True;
      MakeVisible(False);
    end;
  end;
end;

procedure TCustomizeParams.actDeleteItemExecute(Sender: TObject);
begin
  if lvItems.ItemIndex >= 0 then
    lvItems.Items.Delete(lvItems.ItemIndex);
end;

procedure TCustomizeParams.actUpdateItemExecute(Sender: TObject);
Var
  i : integer;
begin
  if (edName.Text <> '') and (lvItems.ItemIndex >= 0) then begin
    for i := 0 to lvItems.Items.Count - 1 do
      if (CompareText(lvItems.Items[i].Caption, edName.Text) = 0) and
         (i <> lvItems.ItemIndex) then
      begin
        StyledMessageDlg(_(SSameName), mtError, [mbOK], 0);
        Exit;
      end;
    with lvItems.Items[lvItems.ItemIndex] do begin
      Caption := EdName.Text;
      SubItems[0] := SynValue.Text;
    end;
  end;
end;

procedure TCustomizeParams.lvItemsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Selected then begin
    edName.Text := Item.Caption;
    SynValue.Text := Item.SubItems[0];
  end else begin
    edName.Text := '';
    SynValue.Text := '';
  end;
end;

procedure TCustomizeParams.actMoveUpExecute(Sender: TObject);
Var
  Name, Value : string;
  Index : integer;
begin
  if lvItems.ItemIndex > 0 then begin
    Index := lvItems.ItemIndex;
    Name := lvItems.Items[Index].Caption;
    Value := lvItems.Items[Index].SubItems[0];
    lvItems.Items.Delete(Index);

    with lvItems.Items.Insert(Index - 1) do begin
      Caption := Name;
      SubItems.Add(Value);
      Selected := True;
    end;
  end;
end;

procedure TCustomizeParams.actMoveDownExecute(Sender: TObject);
Var
  Name, Value : string;
  Index : integer;
begin
  if (lvItems.ItemIndex >= 0) and
    (lvItems.ItemIndex < lvItems.Items.Count - 1) then
  begin
    Index := lvItems.ItemIndex;
    Name := lvItems.Items[Index].Caption;
    Value := lvItems.Items[Index].SubItems[0];
    lvItems.Items.Delete(Index);

    with lvItems.Items.Insert(Index + 1) do begin
      Caption := Name;
      SubItems.Add(Value);
      Selected := True;
    end;
  end;
end;

end.
