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
  System.UITypes, Windows, Messages, SysUtils, Variants, Classes, Controls,
  Forms, Dialogs, System.Contnrs, StdCtrls,
  SynEdit, ActnList, cFileTemplates, SpTBXControls,
  dlgPyIDEBase, SpTBXEditors, SpTBXItem, EasyListview,
  MPCommonObjects, MPCommonUtilities, System.Actions;

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
    ActionList: TActionList;
    actUpdateItem: TAction;
    actMoveDown: TAction;
    actMoveUp: TAction;
    actDeleteItem: TAction;
    actAddItem: TAction;
    Label1: TSpTBXLabel;
    Label2: TSpTBXLabel;
    Label5: TSpTBXLabel;
    Label4: TSpTBXLabel;
    Label3: TSpTBXLabel;
    Label6: TSpTBXLabel;
    Label7: TSpTBXLabel;
    lvItems: TEasyListview;
    procedure FormDestroy(Sender: TObject);
    procedure edNameKeyPress(Sender: TObject; var Key: Char);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actAddItemExecute(Sender: TObject);
    procedure actDeleteItemExecute(Sender: TObject);
    procedure actUpdateItemExecute(Sender: TObject);
    procedure actMoveUpExecute(Sender: TObject);
    procedure actMoveDownExecute(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBHighlightersChange(Sender: TObject);
    procedure lvItemsItemSelectionsChanged(Sender: TCustomEasyListview);
    procedure lvItemsColumnClick(Sender: TCustomEasyListview;
      Button: TCommonMouseButton; ShiftState: TShiftState;
      const Column: TEasyColumn);
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

uses dmCommands, SynEditHighlighter, gnugettext, StringResources;

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
  if not CharInSet(Key, ['a'..'z', 'A'..'Z', '0'..'9', #8]) then
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
  lvItems.BeginUpdate;
  try
    lvItems.Items.Clear;
    for i := 0 to TempFileTemplates.Count - 1 do
      with lvItems.Items.Add() do begin
        Caption := (TempFileTemplates[i] as TFileTemplate).Name;
        Data := TempFileTemplates[i];
        Captions[1] := TFileTemplate(TempFileTemplates[i]).Category;
      end;
  finally
    lvItems.EndUpdate;
  end;
end;

procedure TFileTemplatesDialog.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  actDeleteItem.Enabled := Assigned(lvItems.Selection.First());
  actMoveUp.Enabled :=  Assigned(lvItems.Selection.First()) and  (lvItems.Selection.First.Index >= 1);
  actMoveDown.Enabled := Assigned(lvItems.Selection.First()) and
                         (lvItems.Selection.First.Index < lvItems.Items.Count - 1);
  actAddItem.Enabled := (edName.Text <> '') and (edCategory.Text <> '');
  actUpdateItem.Enabled := (edName.Text <> '') and Assigned(lvItems.Selection.First());
  Handled := True;
end;

procedure TFileTemplatesDialog.actAddItemExecute(Sender: TObject);
Var
  Item : TEasyItem;
  FileTemplate : TFileTemplate;
  i : Integer;
begin
  if (edName.Text <> '') and (edCategory.Text <> '') then begin
    for i := 0 to lvItems.Items.Count - 1 do
      if (CompareText(lvItems.Items[i].Caption, edName.Text) = 0) and
         (CompareText(lvItems.Items[i].Captions[1], edCategory.Text) = 0) then
      begin
        Item := lvItems.Items[i];
        FileTemplate := TFileTemplate(Item.Data);
        StoreFieldsToFileTemplate(FileTemplate);
        Item.Caption := edName.Text;
        Item.Captions[1] := edCategory.Text;
        Item.Selected := True;
        Item.MakeVisible(emvAuto);
        Exit;
      end;

    FileTemplate := TFileTemplate.Create;
    TempFileTemplates.Add(FileTemplate);
    StoreFieldsToFileTemplate(FileTemplate);
    with lvItems.Items.Add() do begin
      Caption := edName.Text;
      Captions[1] := edCategory.Text;
      Data := FileTemplate;
      Selected := True;
      MakeVisible(emvAuto);
    end;
  end;
end;

procedure TFileTemplatesDialog.actDeleteItemExecute(Sender: TObject);
begin
  if Assigned(lvItems.Selection.First()) then begin
    TempFileTemplates.Delete(lvItems.Selection.First.Index);
    lvItems.Items.Delete(lvItems.Selection.First.Index);
  end;
end;

procedure TFileTemplatesDialog.actUpdateItemExecute(Sender: TObject);
Var
  i : integer;
begin
  if (edName.Text <> '') and Assigned(lvItems.Selection.First()) then begin
    for i := 0 to lvItems.Items.Count - 1 do
      if (CompareText(lvItems.Items[i].Caption, edName.Text) = 0) and
         (CompareText(lvItems.Items[i].Captions[1], edCategory.Text) = 0) and
         (i <> lvItems.Selection.First.Index) then
      begin
        Dialogs.MessageDlg(_(SSameName), mtError, [mbOK], 0);
        Exit;
      end;
    with lvItems.Items[lvItems.Selection.First.Index] do begin
      StoreFieldsToFileTemplate(TFileTemplate(Data));
      Caption := edName.Text;
      Captions[1] := edCategory.Text;
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

procedure TFileTemplatesDialog.lvItemsColumnClick(Sender: TCustomEasyListview;
  Button: TCommonMouseButton; ShiftState: TShiftState;
  const Column: TEasyColumn);
begin
  if Column.Clickable then lvItems.Sort.SortAll;
end;

procedure TFileTemplatesDialog.lvItemsItemSelectionsChanged(
  Sender: TCustomEasyListview);
var
  FileTemplate : TFileTemplate;
begin
  if Assigned(lvItems.Selection.First())  then begin
    FileTemplate := TFileTemplate(lvItems.Selection.First.Data);
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
  Name, Value : string;
  P : Pointer;
  Index : integer;
begin
  if Assigned(lvItems.Selection.First()) and (lvItems.Selection.First.Index >= 1) then begin
    lvItems.Header.Columns[0].SortDirection := esdNone;
    lvItems.Header.Columns[1].SortDirection := esdNone;

    Index := lvItems.Selection.First.Index;
    Name := lvItems.Items[Index].Caption;
    Value := lvItems.Items[Index].Captions[1];
    P := lvItems.Items[Index].Data;
    lvItems.Items.Delete(Index);

    with lvItems.Items.Insert(Index - 1) do begin
      Caption := Name;
      Captions[1] := Value;
      Data := P;
      Selected := True;
    end;
    TempFileTemplates.Move(Index, Index - 1);
  end;
end;

procedure TFileTemplatesDialog.actMoveDownExecute(Sender: TObject);
Var
  Name, Value : string;
  P : Pointer;
  Index : integer;
begin
  if Assigned(lvItems.Selection.First()) and
    (lvItems.Selection.First.Index < lvItems.Items.Count - 1) then
  begin
    lvItems.Header.Columns[0].SortDirection := esdNone;
    lvItems.Header.Columns[1].SortDirection := esdNone;

    Index := lvItems.Selection.First.Index;
    Name := lvItems.Items[Index].Caption;
    Value := lvItems.Items[Index].Captions[1];
    P := lvItems.Items[Index].Data;
    lvItems.Items.Delete(Index);

    with lvItems.Items.Insert(Index + 1) do begin
      Caption := Name;
      Captions[1] := Value;
      Data := P;
      Selected := True;
    end;
    TempFileTemplates.Move(Index, Index + 1);
  end;
end;

end.
