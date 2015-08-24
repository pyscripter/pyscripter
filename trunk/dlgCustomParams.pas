unit dlgCustomParams;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Controls, Forms,
  Dialogs, StdCtrls,
  SynEdit, ActnList, SpTBXControls, dlgPyIDEBase,
  SpTBXEditors, SpTBXItem, MPCommonObjects,
  EasyListview, MPCommonUtilities, System.Actions;

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
    ActionList: TActionList;
    actUpdateItem: TAction;
    actMoveDown: TAction;
    actMoveUp: TAction;
    actDeleteItem: TAction;
    actAddItem: TAction;
    Label3: TSpTBXLabel;
    Label4: TSpTBXLabel;
    GroupBox1: TSpTBXGroupBox;
    SynValue: TSynEdit;
    Label1: TSpTBXLabel;
    Label2: TSpTBXLabel;
    edName: TSpTBXEdit;
    lvItems: TEasyListview;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edNameKeyPress(Sender: TObject; var Key: Char);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actAddItemExecute(Sender: TObject);
    procedure actDeleteItemExecute(Sender: TObject);
    procedure actUpdateItemExecute(Sender: TObject);
    procedure actMoveUpExecute(Sender: TObject);
    procedure actMoveDownExecute(Sender: TObject);
    procedure lvItemsItemSelectionsChanged(Sender: TCustomEasyListview);
    procedure lvItemsColumnClick(Sender: TCustomEasyListview;
      Button: TCommonMouseButton; ShiftState: TShiftState;
      const Column: TEasyColumn);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetItems(List : TStrings);
    procedure GetItems(List : TStrings);
  end;

implementation

uses dmCommands, gnugettext, StringResources;

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
      List.Add(lvItems.Items[i].Caption + '=' + lvItems.Items[i].Captions[1]);
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
      Captions[1] := List.Values[List.Names[i]];
    end;
end;

procedure TCustomizeParams.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  actDeleteItem.Enabled := Assigned(lvItems.Selection.First());
  actMoveUp.Enabled :=  Assigned(lvItems.Selection.First()) and  (lvItems.Selection.First.Index >= 1);
  actMoveDown.Enabled := Assigned(lvItems.Selection.First()) and
                         (lvItems.Selection.First.Index < lvItems.Items.Count - 1);
  actAddItem.Enabled := edName.Text <> '';
  actUpdateItem.Enabled := (edName.Text <> '') and Assigned(lvItems.Selection.First());
  Handled := True;
end;

procedure TCustomizeParams.actAddItemExecute(Sender: TObject);
Var
  Item : TEasyItem;
  i : Integer;
begin
  if edName.Text <> '' then begin
    for i := 0 to lvItems.Items.Count - 1 do
      if CompareText(lvItems.Items[i].Caption, edName.Text) = 0 then begin
        Item := lvItems.Items[i];
        Item.Caption := EdName.Text;
        Item.Captions[1] := SynValue.Text;
        Item.Selected := True;
        Item.MakeVisible(emvAuto);
        Exit;
      end;

    with lvItems.Items.Add() do begin
      Caption := edName.Text;
      Captions[1] := SynValue.Text;
      MakeVisible(emvAuto);
    end;
  end;
end;

procedure TCustomizeParams.actDeleteItemExecute(Sender: TObject);
begin
  if Assigned(lvItems.Selection.First()) then
    lvItems.Items.Delete(lvItems.Selection.First.Index);
end;

procedure TCustomizeParams.actUpdateItemExecute(Sender: TObject);
Var
  i : integer;
begin
  if (edName.Text <> '') and Assigned(lvItems.Selection.First()) then begin
    for i := 0 to lvItems.Items.Count - 1 do
      if (CompareText(lvItems.Items[i].Caption, edName.Text) = 0) and
         (i <> lvItems.Selection.First.Index) then
      begin
        Dialogs.MessageDlg(_(SSameName), mtError, [mbOK], 0);
        Exit;
      end;
    lvItems.Header.Columns[0].SortDirection := esdNone;
    with lvItems.Items[lvItems.Selection.First.Index] do begin
      Caption := EdName.Text;
      Captions[1] := SynValue.Text;
    end;
  end;
end;

procedure TCustomizeParams.lvItemsColumnClick(Sender: TCustomEasyListview;
  Button: TCommonMouseButton; ShiftState: TShiftState;
  const Column: TEasyColumn);
begin
  if Column.Clickable then lvItems.Sort.SortAll;
end;

procedure TCustomizeParams.lvItemsItemSelectionsChanged(
  Sender: TCustomEasyListview);
begin
  if Assigned(lvItems.Selection.First()) then with lvItems.Selection.First do begin
    edName.Text := Caption;
    SynValue.Text := Captions[1];
  end;
end;

procedure TCustomizeParams.actMoveUpExecute(Sender: TObject);
Var
  Name, Value : string;
  Index : integer;
begin
  if Assigned(lvItems.Selection.First()) and  (lvItems.Selection.First.Index >= 1) then begin
    lvItems.Header.Columns[0].SortDirection := esdNone;
    Index := lvItems.Selection.First.Index;
    Name := lvItems.Items[Index].Caption;
    Value := lvItems.Items[Index].Captions[1];
    lvItems.Items.Delete(Index);

    with lvItems.Items.Insert(Index - 1) do begin
      Caption := Name;
      Captions[1] := Value;
      Selected := True;
    end;
  end;
end;

procedure TCustomizeParams.actMoveDownExecute(Sender: TObject);
Var
  Name, Value : string;
  Index : integer;
begin
  if Assigned(lvItems.Selection.First()) and (lvItems.Selection.First.Index < lvItems.Items.Count - 1) then
  begin
    lvItems.Header.Columns[0].SortDirection := esdNone;
    Index := lvItems.Selection.First.Index;
    Name := lvItems.Items[Index].Caption;
    Value := lvItems.Items[Index].Captions[1];
    lvItems.Items.Delete(Index);

    with lvItems.Items.Insert(Index + 1) do begin
      Caption := Name;
      Captions[1] := Value;
      Selected := True;
    end;
  end;
end;

end.
