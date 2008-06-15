{-----------------------------------------------------------------------------
 Unit Name: dlgCodeTemplates
 Author:    Kiriakos Vlahos
 Date:      08-Aug-2006
 Purpose:   Customization dialog for Code Templates
 History:
-----------------------------------------------------------------------------}

unit dlgFileTemplates;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, JvEdit,
  SynEdit, ActnList, TBXDkPanels, cFileTemplates, SpTBXControls, TntActnList,
  TntComCtrls, dlgPyIDEBase, TntStdCtrls, SpTBXEditors;

type
  TFileTemplatesDialog = class(TPyIDEDlgBase)
    Panel: TSpTBXPanel;
    GroupBox: TSpTBXGroupBox;
    SynTemplate: TSynEdit;
    edName: TSpTBXEdit;
    edCategory: TSpTBXEdit;
    edExtension: TSpTBXEdit;
    CBHighlighters: TSpTBXComboBox;
    TBXButton1: TSpTBXButton;
    TBXButton3: TSpTBXButton;
    TBXButton4: TSpTBXButton;
    TBXButton5: TSpTBXButton;
    TBXButton2: TSpTBXButton;
    btnCancel: TSpTBXButton;
    btnOK: TSpTBXButton;
    btnHelp: TSpTBXButton;
    ActionList: TTntActionList;
    actUpdateItem: TTntAction;
    actMoveDown: TTntAction;
    actMoveUp: TTntAction;
    actDeleteItem: TTntAction;
    actAddItem: TTntAction;
    Label1: TSpTBXLabel;
    Label2: TSpTBXLabel;
    Label5: TSpTBXLabel;
    Label4: TSpTBXLabel;
    Label3: TSpTBXLabel;
    Label6: TSpTBXLabel;
    Label7: TSpTBXLabel;
    lvItems: TTntListView;
    procedure FormDestroy(Sender: TObject);
    procedure edNameKeyPress(Sender: TObject; var Key: Char);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actAddItemExecute(Sender: TObject);
    procedure actDeleteItemExecute(Sender: TObject);
    procedure actUpdateItemExecute(Sender: TObject);
    procedure lvItemsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure actMoveUpExecute(Sender: TObject);
    procedure actMoveDownExecute(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBHighlightersChange(Sender: TObject);
  private
    procedure StoreFieldsToFileTemplate(FileTemplate: TFileTemplate);
    { Private declarations }
  public
    { Public declarations }
    TempFileTemplates : TFileTemplates;
    procedure SetItems;
    procedure GetItems;
  end;

implementation

uses dmCommands, SynEditHighlighter, gnugettext, StringResources, TntDialogs;

{$R *.dfm}

procedure TFileTemplatesDialog.FormCreate(Sender: TObject);
var
  i : integer;
begin
  inherited;
  TempFileTemplates := TFileTemplates.Create;
  for i := 0 to CommandsDataModule.Highlighters.Count - 1 do
    cbHighlighters.Items.AddObject(CommandsDataModule.Highlighters[i],
      CommandsDataModule.Highlighters.Objects[i]);
  SynTemplate.Highlighter := nil;
  CommandsDataModule.ParameterCompletion.Editor := SynTemplate;
  CommandsDataModule.ModifierCompletion.Editor := SynTemplate;
end;

procedure TFileTemplatesDialog.FormDestroy(Sender: TObject);
begin
  CommandsDataModule.ParameterCompletion.RemoveEditor(SynTemplate);
  CommandsDataModule.ModifierCompletion.RemoveEditor(SynTemplate);
  SynTemplate.Highlighter := nil;
  TempFileTemplates.Free;
end;

procedure TFileTemplatesDialog.edNameKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in ['a'..'z', 'A'..'Z', '0'..'9', #8]) then
    Key := #0;
  inherited;
end;

procedure TFileTemplatesDialog.GetItems;
begin
  FileTemplates.Assign(TempFileTemplates);
end;

procedure TFileTemplatesDialog.StoreFieldsToFileTemplate(FileTemplate: TFileTemplate);
begin
  FileTemplate.Name := edName.Text;
  FileTemplate.Extension := edExtension.Text;
  FileTemplate.Category := edCategory.Text;
  FileTemplate.Highlighter := CBHighlighters.Text;
  FileTemplate.Template := SynTemplate.Text;
end;

procedure TFileTemplatesDialog.SetItems;
Var
 i : integer;
begin
  TempFileTemplates.Assign(FileTemplates);
  lvItems.Clear;
  for i := 0 to TempFileTemplates.Count - 1 do
    with lvItems.Items.Add() do begin
      Caption := (TempFileTemplates[i] as TFileTemplate).Name;
      Data := TempFileTemplates[i];
      SubItems.Add(TFileTemplate(TempFileTemplates[i]).Category);
    end;
end;

procedure TFileTemplatesDialog.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  actDeleteItem.Enabled := lvItems.ItemIndex >= 0;
  actMoveUp.Enabled := lvItems.ItemIndex >= 1;
  actMoveDown.Enabled := (lvItems.ItemIndex >= 0) and
                         (lvItems.ItemIndex < lvItems.Items.Count - 1);
  actAddItem.Enabled := (edName.Text <> '') and (edCategory.Text <> '');
  actUpdateItem.Enabled := (edName.Text <> '') and (lvItems.ItemIndex >= 0);
  Handled := True;
end;

procedure TFileTemplatesDialog.actAddItemExecute(Sender: TObject);
Var
  Item : TListItem;
  FileTemplate : TFileTemplate;
  i : Integer;
begin
  if (edName.Text <> '') and (edCategory.Text <> '') then begin
    for i := 0 to lvItems.Items.Count - 1 do
      if (CompareText(lvItems.Items[i].Caption, edName.Text) = 0) and
         (CompareText(lvItems.Items[i].SubItems[0], edCategory.Text) = 0) then
      begin
        Item := lvItems.Items[i];
        FileTemplate := TFileTemplate(Item.Data);
        StoreFieldsToFileTemplate(FileTemplate);
        Item.Caption := edName.Text;
        Item.SubItems[0] := edCategory.Text;
        Item.Selected := True;
        Exit;
      end;

    FileTemplate := TFileTemplate.Create;
    TempFileTemplates.Add(FileTemplate);
    StoreFieldsToFileTemplate(FileTemplate);
    with lvItems.Items.Add() do begin
      Caption := edName.Text;
      SubItems.Add(edCategory.Text);
      Data := FileTemplate;
      Selected := True;
    end;
  end;
end;

procedure TFileTemplatesDialog.actDeleteItemExecute(Sender: TObject);
begin
  if lvItems.ItemIndex >= 0 then begin
    TempFileTemplates.Delete(lvItems.ItemIndex);
    lvItems.Items.Delete(lvItems.ItemIndex);
  end;
end;

procedure TFileTemplatesDialog.actUpdateItemExecute(Sender: TObject);
Var
  i : integer;
begin
  if (edName.Text <> '') and (lvItems.ItemIndex >= 0) then begin
    for i := 0 to lvItems.Items.Count - 1 do
      if (CompareText(lvItems.Items[i].Caption, edName.Text) = 0) and
         (CompareText(lvItems.Items[i].SubItems[0], edCategory.Text) = 0) and
         (i <> lvItems.ItemIndex) then
      begin
        WideMessageDlg(_(SSameName), mtError, [mbOK], 0);
        Exit;
      end;
    with lvItems.Items[lvItems.ItemIndex] do begin
      StoreFieldsToFileTemplate(TFileTemplate(Data));
      Caption := edName.Text;
      SubItems[0] := edCategory.Text;
    end;
  end;
end;

procedure TFileTemplatesDialog.btnHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TFileTemplatesDialog.CBHighlightersChange(Sender: TObject);
begin
  if CBHighlighters.ItemIndex < 0 then
    SynTemplate.Highlighter := nil
  else
    SynTemplate.Highlighter :=
      CBHighlighters.Items.Objects[CBHighlighters.ItemIndex] as TSynCustomHighlighter;
end;

procedure TFileTemplatesDialog.lvItemsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  FileTemplate : TFileTemplate;
begin
  if Item.Selected then begin
    FileTemplate := TFileTemplate(Item.Data);
    edName.Text := FileTemplate.Name;
    edCategory.Text := FileTemplate.Category;
    edExtension.Text := FileTemplate.Extension;
    CBHighlighters.ItemIndex := CBHighlighters.Items.IndexOf(FileTemplate.Highlighter);
    SynTemplate.Text := FileTemplate.Template;
    CBHighlightersChange(Self);
  end else begin
    edName.Text := '';
    edCategory.Text := '';
    edExtension.Text := '';
    CBHighlighters.ItemIndex := -1;
    SynTemplate.Text := '';
  end;
end;

procedure TFileTemplatesDialog.actMoveUpExecute(Sender: TObject);
Var
  Name, Value : WideString;
  P : Pointer;
  Index : integer;
begin
  if lvItems.ItemIndex > 0 then begin
    Index := lvItems.ItemIndex;
    Name := lvItems.Items[Index].Caption;
    Value := lvItems.Items[Index].SubItems[0];
    P := lvItems.Items[Index].Data;
    lvItems.Items.Delete(Index);

    with lvItems.Items.Insert(Index - 1) do begin
      Caption := Name;
      SubItems.Add(Value);
      Data := P;
      Selected := True;
    end;
    TempFileTemplates.Move(Index, Index - 1);
  end;
end;

procedure TFileTemplatesDialog.actMoveDownExecute(Sender: TObject);
Var
  Name, Value : WideString;
  P : Pointer;
  Index : integer;
begin
  if lvItems.ItemIndex < lvItems.Items.Count - 1 then begin
    Index := lvItems.ItemIndex;
    Name := lvItems.Items[Index].Caption;
    Value := lvItems.Items[Index].SubItems[0];
    P := lvItems.Items[Index].Data;
    lvItems.Items.Delete(Index);

    with lvItems.Items.Insert(Index + 1) do begin
      Caption := Name;
      SubItems.Add(Value);
      Data := P;
      Selected := True;
    end;
    TempFileTemplates.Move(Index, Index + 1);
  end;
end;

end.
