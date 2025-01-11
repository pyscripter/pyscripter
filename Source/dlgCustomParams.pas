unit dlgCustomParams;

interface

uses
  System.Classes,
  System.Actions,
  System.ImageList,
  Vcl.Controls,
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
    lvItems: TListView;
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
  public
    procedure SetItems(List: TStrings);
    procedure GetItems(List: TStrings);
  end;

implementation

uses
  System.SysUtils,
  Vcl.Dialogs,
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
  if not CharInSet(Key, ['a'..'z', 'A'..'Z', '0'..'9', #8]) then
    Key := #0;
end;

procedure TCustomizeParams.GetItems(List: TStrings);
begin
  List.Clear;
  List.BeginUpdate;
  try
    for var I := 0 to lvItems.Items.Count - 1 do
      List.Add(lvItems.Items[I].Caption + '=' + lvItems.Items[I].SubItems[0]);
  finally
    List.EndUpdate;
  end;
end;

procedure TCustomizeParams.SetItems(List: TStrings);
begin
  lvItems.Items.Clear;
  for var I := 0 to List.Count - 1 do
    with lvItems.Items.Add do begin
      Caption := List.Names[I];
      SubItems.Add(List.Values[List.Names[I]]);
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
var
  Item: TListItem;
begin
  if edName.Text <> '' then begin
    for var I := 0 to lvItems.Items.Count - 1 do
      if CompareText(lvItems.Items[I].Caption, edName.Text) = 0 then begin
        Item := lvItems.Items[I];
        Item.Caption := edName.Text;
        Item.SubItems[0] := SynValue.Text;
        Item.Selected := True;
        Item.MakeVisible(False);
        Exit;
      end;

    with lvItems.Items.Add do begin
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
begin
  if (edName.Text <> '') and (lvItems.ItemIndex >= 0) then begin
    for var I := 0 to lvItems.Items.Count - 1 do
      if (CompareText(lvItems.Items[I].Caption, edName.Text) = 0) and
         (I <> lvItems.ItemIndex) then
      begin
        StyledMessageDlg(_(SSameName), mtError, [mbOK], 0);
        Exit;
      end;
    with lvItems.Items[lvItems.ItemIndex] do begin
      Caption := edName.Text;
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
var
  Name, Value: string;
  Index: Integer;
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
var
  Name, Value: string;
  Index: Integer;
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
