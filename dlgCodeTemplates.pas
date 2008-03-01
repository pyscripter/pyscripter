{-----------------------------------------------------------------------------
 Unit Name: dlgCodeTemplates
 Author:    Kiriakos Vlahos
 Date:      08-Aug-2006
 Purpose:   Customization dialog for Code Templates
 History:
-----------------------------------------------------------------------------}

unit dlgCodeTemplates;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, JvEdit,
  SynEdit,  ActnList, TBXDkPanels, SpTBXControls;

type
  TCodeTemplates = class(TForm)
    Panel: TPanel;
    lvItems: TListView;
    GroupBox: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    SynTemplate: TSynEdit;
    edShortcut: TEdit;
    ActionList: TActionList;
    actAddItem: TAction;
    actDeleteItem: TAction;
    actMoveUp: TAction;
    actMoveDown: TAction;
    actUpdateItem: TAction;
    Label5: TLabel;
    edDescription: TEdit;
    Label4: TLabel;
    Label3: TLabel;
    TBXButton1: TSpTBXButton;
    TBXButton3: TSpTBXButton;
    TBXButton4: TSpTBXButton;
    TBXButton5: TSpTBXButton;
    TBXButton2: TSpTBXButton;
    btnCancel: TSpTBXButton;
    btnOK: TSpTBXButton;
    btnHelp: TSpTBXButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edShortcutKeyPress(Sender: TObject; var Key: Char);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actAddItemExecute(Sender: TObject);
    procedure actDeleteItemExecute(Sender: TObject);
    procedure actUpdateItemExecute(Sender: TObject);
    procedure lvItemsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure actMoveUpExecute(Sender: TObject);
    procedure actMoveDownExecute(Sender: TObject);
    procedure lvItemsDeletion(Sender: TObject; Item: TListItem);
    procedure btnHelpClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    CodeTemplateText : String;
    procedure SetItems;
    procedure GetItems;
  end;

implementation

uses dmCommands;

{$R *.dfm}

procedure TCodeTemplates.FormDestroy(Sender: TObject);
begin
  CommandsDataModule.ParameterCompletion.RemoveEditor(SynTemplate);
  CommandsDataModule.ModifierCompletion.RemoveEditor(SynTemplate);
  SynTemplate.Highlighter := nil;
end;

procedure TCodeTemplates.FormShow(Sender: TObject);
begin
  SynTemplate.Highlighter := CommandsDataModule.SynPythonSyn;
  CommandsDataModule.ParameterCompletion.Editor := SynTemplate;
  CommandsDataModule.ModifierCompletion.Editor := SynTemplate;
  SetItems;
end;

procedure TCodeTemplates.edShortcutKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in ['a'..'z', 'A'..'Z', '0'..'9', #8]) then
    Key := #0;
  inherited;
end;

procedure TCodeTemplates.GetItems;
Var
 i, j : integer;
begin
  CodeTemplateText := '';
  for i := 0 to lvItems.Items.Count - 1 do begin
    CodeTemplateText := CodeTemplateText + lvItems.Items[i].Caption + sLineBreak;
    if lvItems.Items[i].SubItems[0] <> '' then
      CodeTemplateText := CodeTemplateText +'|' + lvItems.Items[i].SubItems[0] + sLineBreak;
    for j := 0 to TStringList(lvItems.Items[i].Data).Count - 1 do
      CodeTemplateText := CodeTemplateText + '=' +
        TStringList(lvItems.Items[i].Data)[j] + sLineBreak;
  end;
end;

procedure TCodeTemplates.SetItems;
Var
 i, Count : integer;
 List : TStringList;
begin
  lvItems.Clear;
  i := 0;
  Count := 0;
  List := TStringList.Create;
  try
    List.Text := CodeTemplateText;
    while i < List.Count do begin
      if not (List[i][1] in ['|', '=']) then begin
        Inc(Count);
        with lvItems.Items.Add() do begin
          Caption := List[i];
          Data := TStringList.Create;
          Inc(i);
          if List[i][1] = '|' then begin
            SubItems.Add(Copy(List[i], 2, MaxInt));
            Inc(i);
          end else
            SubItems.Add('');
        end;
      end else begin
        if (Count > 0) and (List[i][1] = '=') then
          TStringList(lvItems.Items[Count-1].Data).Add(Copy(List[i], 2, MaxInt));
        Inc(i);
      end;
    end;
  finally
    List.Free;
  end;
end;

procedure TCodeTemplates.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  actDeleteItem.Enabled := lvItems.ItemIndex >= 0;
  actMoveUp.Enabled := lvItems.ItemIndex >= 1;
  actMoveDown.Enabled := (lvItems.ItemIndex >= 0) and
                         (lvItems.ItemIndex < lvItems.Items.Count - 1);
  actAddItem.Enabled := edShortCut.Text <> '';
  actUpdateItem.Enabled := (edShortCut.Text <> '') and (lvItems.ItemIndex >= 0);
  Handled := True;
end;

procedure TCodeTemplates.actAddItemExecute(Sender: TObject);
Var
  Item : TListItem;
  i : Integer;
begin
  if edShortCut.Text <> '' then begin
    SynTemplate.Modified := False;
    for i := 0 to lvItems.Items.Count - 1 do
      if CompareText(lvItems.Items[i].Caption, edShortCut.Text) = 0 then begin
        Item := lvItems.Items[i];
        Item.Caption := edShortCut.Text;
        Item.SubItems[0] := edDescription.Text;
        TStringList(Item.Data).Assign(SynTemplate.Lines);
        Item.Selected := True;
        Exit;
      end;

    with lvItems.Items.Add() do begin
      Caption := edShortCut.Text;
      SubItems.Add(edDescription.Text);
      Data := Pointer(TStringList.Create);
      TStringList(Data).Assign(SynTemplate.Lines);
    end;
  end;
end;

procedure TCodeTemplates.actDeleteItemExecute(Sender: TObject);
begin
  if lvItems.ItemIndex >= 0 then
    lvItems.Items.Delete(lvItems.ItemIndex);
end;

procedure TCodeTemplates.actUpdateItemExecute(Sender: TObject);
Var
  i : integer;
begin
  if (edShortCut.Text <> '') and (lvItems.ItemIndex >= 0) then begin
    for i := 0 to lvItems.Items.Count - 1 do
      if (CompareText(lvItems.Items[i].Caption, edShortCut.Text) = 0) and
         (i <> lvItems.ItemIndex) then
      begin
        MessageDlg('Another item has the same name', mtError, [mbOK], 0);
        Exit;
      end;
    with lvItems.Items[lvItems.ItemIndex] do begin
      Caption := edShortCut.Text;
      SubItems[0] := edDescription.Text;
      TStringList(Data).Assign(SynTemplate.Lines);
      SynTemplate.Modified := False;
    end;
  end;
end;

procedure TCodeTemplates.btnHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TCodeTemplates.lvItemsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if Item.Selected then begin
    edShortCut.Text := Item.Caption;
    edDescription.Text := Item.SubItems[0];
    SynTemplate.Lines.Assign(TStringList(Item.Data));
    SynTemplate.Modified := False;
  end;
end;

procedure TCodeTemplates.actMoveUpExecute(Sender: TObject);
Var
  Name, Value : string;
  P : Pointer;
  Index : integer;
begin
  if lvItems.ItemIndex > 0 then begin
    Index := lvItems.ItemIndex;
    Name := lvItems.Items[Index].Caption;
    Value := lvItems.Items[Index].SubItems[0];
    P := lvItems.Items[Index].Data;
    lvItems.Items[Index].Data := nil;  // so that it does not get freed
    lvItems.Items.Delete(Index);

    with lvItems.Items.Insert(Index - 1) do begin
      Caption := Name;
      SubItems.Add(Value);
      Data := P;
      Selected := True;
    end;
  end;
end;

procedure TCodeTemplates.actMoveDownExecute(Sender: TObject);
Var
  Name, Value : string;
  P : Pointer;
  Index : integer;
begin
  if lvItems.ItemIndex < lvItems.Items.Count - 1 then begin
    Index := lvItems.ItemIndex;
    Name := lvItems.Items[Index].Caption;
    Value := lvItems.Items[Index].SubItems[0];
    P := lvItems.Items[Index].Data;
    lvItems.Items[Index].Data := nil;  // so that it does not get freed
    lvItems.Items.Delete(Index);

    with lvItems.Items.Insert(Index + 1) do begin
      Caption := Name;
      SubItems.Add(Value);
      Data := P;
      Selected := True;
    end;
  end;
end;

procedure TCodeTemplates.lvItemsDeletion(Sender: TObject; Item: TListItem);
begin
  if Assigned(Item.Data) then
    TStringList(Item.Data).Free;
end;

procedure TCodeTemplates.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (edShortcut.Text <> '') and SynTemplate.Modified then begin
    if (MessageDlg('The template has been modified.  Do you want to update it with the new definition?',
      mtConfirmation, [mbYes, mbNo], 0) = idYes)
    then
      actUpdateItemExecute(Sender);
  end;
  GetItems;
end;

end.
