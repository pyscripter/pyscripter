unit dlgCustomParams;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, JvEdit,
  SynEdit, ActnList, TBXDkPanels, SpTBXControls, TntActnList, dlgPyIDEBase,
  TntComCtrls, SpTBXEditors, WideStrings;

type
  TCustomizeParams = class(TPyIDEDlgBase)
    Panel: TSpTBXPanel;
    TBXButton1: TSpTBXButton;
    TBXButton3: TSpTBXButton;
    TBXButton4: TSpTBXButton;
    TBXButton5: TSpTBXButton;
    TBXButton2: TSpTBXButton;
    btnOK: TSpTBXButton;
    btnCancel: TSpTBXButton;
    ActionList: TTntActionList;
    actUpdateItem: TTntAction;
    actMoveDown: TTntAction;
    actMoveUp: TTntAction;
    actDeleteItem: TTntAction;
    actAddItem: TTntAction;
    Label3: TSpTBXLabel;
    Label4: TSpTBXLabel;
    GroupBox1: TSpTBXGroupBox;
    SynValue: TSynEdit;
    Label1: TSpTBXLabel;
    Label2: TSpTBXLabel;
    edName: TSpTBXEdit;
    lvItems: TTntListView;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edNameKeyPress(Sender: TObject; var Key: Char);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actAddItemExecute(Sender: TObject);
    procedure actDeleteItemExecute(Sender: TObject);
    procedure actUpdateItemExecute(Sender: TObject);
    procedure lvItemsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure actMoveUpExecute(Sender: TObject);
    procedure actMoveDownExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetItems(List : TWideStrings);
    procedure GetItems(List : TWideStrings);
  end;

implementation

uses dmCommands, gnugettext, StringResources, TntDialogs;

{$R *.dfm}

procedure TCustomizeParams.FormDestroy(Sender: TObject);
begin
  CommandsDataModule.ParameterCompletion.RemoveEditor(SynValue);
  CommandsDataModule.ModifierCompletion.RemoveEditor(SynValue);
end;

procedure TCustomizeParams.FormShow(Sender: TObject);
begin
  CommandsDataModule.ParameterCompletion.Editor := SynValue;
  CommandsDataModule.ModifierCompletion.Editor := SynValue;
end;

procedure TCustomizeParams.edNameKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in ['a'..'z', 'A'..'Z', '0'..'9', #8]) then
    Key := #0;
  inherited;
end;

procedure TCustomizeParams.GetItems(List: TWideStrings);
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

procedure TCustomizeParams.SetItems(List: TWideStrings);
Var
 i : integer;
begin
  lvItems.Clear;
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
        Exit;
      end;

    with lvItems.Items.Add() do begin
      Caption := edName.Text;
      SubItems.Add(SynValue.Text);
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
        WideMessageDlg(_(SSameName), mtError, [mbOK], 0);
        Exit;
      end;
    with lvItems.Items[lvItems.ItemIndex] do begin
      Caption := EdName.Text;
      SubItems[0] := SynValue.Text;
    end;
  end;
end;

procedure TCustomizeParams.lvItemsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if Item.Selected then begin
    edName.Text := Item.Caption;
    SynValue.Text := Item.SubItems[0];
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
