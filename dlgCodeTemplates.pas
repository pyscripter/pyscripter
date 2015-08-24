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
  System.UITypes, Windows, Messages, SysUtils, Variants, Classes,
  Controls, Forms, Dialogs, StdCtrls, SynEdit,  ActnList, SpTBXControls,
  dlgPyIDEBase, SpTBXEditors, SpTBXItem,
  MPCommonObjects, EasyListview, System.Actions;

type
  TCodeTemplates = class(TPyIDEDlgBase)
    ActionList: TActionList;
    actUpdateItem: TAction;
    actMoveDown: TAction;
    actMoveUp: TAction;
    actDeleteItem: TAction;
    actAddItem: TAction;
    Panel: TSpTBXPanel;
    btnAdd: TSpTBXButton;
    btnDelete: TSpTBXButton;
    btnMoveup: TSpTBXButton;
    btnMoveDown: TSpTBXButton;
    btnUpdate: TSpTBXButton;
    btnCancel: TSpTBXButton;
    btnOK: TSpTBXButton;
    btnHelp: TSpTBXButton;
    GroupBox: TSpTBXGroupBox;
    SynTemplate: TSynEdit;
    Label1: TSpTBXLabel;
    Label2: TSpTBXLabel;
    Label5: TSpTBXLabel;
    Label4: TSpTBXLabel;
    Label3: TSpTBXLabel;
    edDescription: TSpTBXEdit;
    edShortcut: TSpTBXEdit;
    lvItems: TEasyListview;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edShortcutKeyPress(Sender: TObject; var Key: Char);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actAddItemExecute(Sender: TObject);
    procedure actDeleteItemExecute(Sender: TObject);
    procedure actUpdateItemExecute(Sender: TObject);
    procedure actMoveUpExecute(Sender: TObject);
    procedure actMoveDownExecute(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure lvItemsItemSelectionsChanged(Sender: TCustomEasyListview);
    procedure lvItemsItemFreeing(Sender: TCustomEasyListview; Item: TEasyItem);
  private
    { Private declarations }
  public
    { Public declarations }
    CodeTemplateText : string;
    procedure SetItems;
    procedure GetItems;
  end;

implementation

uses dmCommands, gnugettext,
  StringResources;

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
  if not CharInSet(Key, ['a'..'z', 'A'..'Z', '0'..'9', #8]) then
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
    if lvItems.Items[i].Captions[1] <> '' then
      CodeTemplateText := CodeTemplateText +'|' + lvItems.Items[i].Captions[1] + sLineBreak;
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
  lvItems.Items.Clear;
  i := 0;
  Count := 0;
  List := TStringList.Create;
  try
    List.Text := CodeTemplateText;
    while i < List.Count do begin
      if Length(List[i]) <= 0 then // Delphi's string list adds a blank line at the end
        Inc(i)
      else if not CharInSet(List[i][1], ['|', '=']) then begin
        Inc(Count);
        with lvItems.Items.Add() do begin
          Caption := List[i];
          Data := TStringList.Create;
          Inc(i);
          if List[i][1] = '|' then begin
            Captions[1] := Copy(List[i], 2, MaxInt);
            Inc(i);
          end else
            Captions[1] := '';
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
  actDeleteItem.Enabled := Assigned(lvItems.Selection.First());
  actMoveUp.Enabled :=  Assigned(lvItems.Selection.First()) and  (lvItems.Selection.First.Index >= 1);
  actMoveDown.Enabled := Assigned(lvItems.Selection.First()) and
                         (lvItems.Selection.First.Index < lvItems.Items.Count - 1);
  actAddItem.Enabled := edShortCut.Text <> '';
  actUpdateItem.Enabled := (edShortCut.Text <> '') and Assigned(lvItems.Selection.First());
  Handled := True;
end;

procedure TCodeTemplates.actAddItemExecute(Sender: TObject);
Var
  Item : TEasyItem;
  i : Integer;
begin
  if edShortCut.Text <> '' then begin
    SynTemplate.Modified := False;
    for i := 0 to lvItems.Items.Count - 1 do
      if CompareText(lvItems.Items[i].Caption, edShortCut.Text) = 0 then begin
        Item := lvItems.Items[i];
        Item.Caption := edShortCut.Text;
        Item.Captions[1] := edDescription.Text;
        TStringList(Item.Data).Assign(SynTemplate.Lines);
        Item.Selected := True;
        Exit;
      end;

    with lvItems.Items.Add() do begin
      Caption := edShortCut.Text;
      Captions[1] := edDescription.Text;
      Data := Pointer(TStringList.Create);
      TStringList(Data).Assign(SynTemplate.Lines);
    end;
  end;
end;

procedure TCodeTemplates.actDeleteItemExecute(Sender: TObject);
begin
  if Assigned(lvItems.Selection.First()) then
    lvItems.Items.Delete(lvItems.Selection.First.Index);
end;

procedure TCodeTemplates.actUpdateItemExecute(Sender: TObject);
Var
  i : integer;
begin
  if (edShortCut.Text <> '') and Assigned(lvItems.Selection.First()) then begin
    for i := 0 to lvItems.Items.Count - 1 do
      if (CompareText(lvItems.Items[i].Caption, edShortCut.Text) = 0) and
         (i <> lvItems.Selection.First.Index) then
      begin
        Dialogs.MessageDlg(_(SSameName), mtError, [mbOK], 0);
        Exit;
      end;
    with lvItems.Selection.First do begin
      Caption := edShortCut.Text;
      Captions[1] := edDescription.Text;
      TStringList(Data).Assign(SynTemplate.Lines);
      SynTemplate.Modified := False;
    end;
  end;
end;

procedure TCodeTemplates.btnHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TCodeTemplates.actMoveUpExecute(Sender: TObject);
Var
  Name, Value : string;
  P : Pointer;
  Index : integer;
begin
  if Assigned(lvItems.Selection.First()) and  (lvItems.Selection.First.Index >= 1) then begin
    Index := lvItems.Selection.First.Index;
    Name := lvItems.Items[Index].Caption;
    Value := lvItems.Items[Index].Captions[1];
    P := lvItems.Items[Index].Data;
    lvItems.Items[Index].Data := nil;  // so that it does not get freed
    lvItems.Items.Delete(Index);

    with lvItems.Items.Insert(Index - 1) do begin
      Caption := Name;
      Captions[1] := Value;
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
  if Assigned(lvItems.Selection.First()) and (lvItems.Selection.First.Index < lvItems.Items.Count - 1) then
  begin
    Index := lvItems.Selection.First.Index;
    Name := lvItems.Items[Index].Caption;
    Value := lvItems.Items[Index].Captions[1];
    P := lvItems.Items[Index].Data;
    lvItems.Items[Index].Data := nil;  // so that it does not get freed
    lvItems.Items.Delete(Index);

    with lvItems.Items.Insert(Index + 1) do begin
      Caption := Name;
      Captions[1] := Value;
      Data := P;
      Selected := True;
    end;
  end;
end;

procedure TCodeTemplates.lvItemsItemFreeing(Sender: TCustomEasyListview;
  Item: TEasyItem);
begin
  if Assigned(Item.Data) then
    TStringList(Item.Data).Free;
end;

procedure TCodeTemplates.lvItemsItemSelectionsChanged(
  Sender: TCustomEasyListview);
begin
  if Assigned(lvItems.Selection.First()) then with lvItems.Selection.First do begin
    edShortCut.Text := Caption;
    edDescription.Text := Captions[1];
    SynTemplate.Lines.Assign(TStringList(lvItems.Selection.First.Data));
    SynTemplate.Modified := False;
  end;
end;

procedure TCodeTemplates.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (edShortcut.Text <> '') and SynTemplate.Modified and
    Assigned(lvItems.Selection.First()) then
  begin
    if (Dialogs.MessageDlg(_(SCodeTemplateModified),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes)
    then
      actUpdateItemExecute(Sender);
  end;
  GetItems;
end;

end.
