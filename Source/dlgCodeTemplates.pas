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
  Winapi.Windows,
  Winapi.Messages,
  System.UITypes,
  System.ImageList,
  System.Actions,
  System.SysUtils,
  System.Variants,
  System.Classes,
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
  TCodeTemplates = class(TPyIDEDlgBase)
    ActionList: TActionList;
    actUpdateItem: TAction;
    actMoveDown: TAction;
    actMoveUp: TAction;
    actDeleteItem: TAction;
    actAddItem: TAction;
    Panel: TPanel;
    btnAdd: TButton;
    btnDelete: TButton;
    btnMoveup: TButton;
    btnMoveDown: TButton;
    btnUpdate: TButton;
    btnCancel: TButton;
    btnOK: TButton;
    btnHelp: TButton;
    GroupBox: TGroupBox;
    SynTemplate: TSynEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    Label4: TLabel;
    Label3: TLabel;
    edDescription: TEdit;
    edShortcut: TEdit;
    lvItems: TListview;
    vilImages: TVirtualImageList;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure edShortcutKeyPress(Sender: TObject; var Key: Char);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actAddItemExecute(Sender: TObject);
    procedure actDeleteItemExecute(Sender: TObject);
    procedure actUpdateItemExecute(Sender: TObject);
    procedure actMoveUpExecute(Sender: TObject);
    procedure actMoveDownExecute(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure lvItemsDeletion(Sender: TObject; Item: TListItem);
    procedure lvItemsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    { Private declarations }
    FOldIndex: Integer;
    procedure AskToUpdate(Sender: TObject);
  public
    { Public declarations }
    CodeTemplateText : string;
    procedure SetItems;
    procedure GetItems;
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

procedure TCodeTemplates.FormShow(Sender: TObject);
begin
  SynTemplate.Highlighter := ResourcesDataModule.SynPythonSyn;
  ResourcesDataModule.ParameterCompletion.Editor := SynTemplate;
  ResourcesDataModule.ModifierCompletion.Editor := SynTemplate;
  SetItems;
  // Styling
  SynTemplate.Color := StyleServices.GetSystemColor(clWindow);
  SynTemplate.Font.Color := StyleServices.GetSystemColor(clWindowText);
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
    if lvItems.Items[i].SubItems[0] <> '' then
      CodeTemplateText := CodeTemplateText +'|' + lvItems.Items[i].SubItems[0] + sLineBreak;
    for j := 0 to TStringList(lvItems.Items[i].Data).Count - 1 do
      CodeTemplateText := CodeTemplateText + '=' +
        TStringList(lvItems.Items[i].Data)[j] + sLineBreak;
  end;
end;

procedure TCodeTemplates.AskToUpdate(Sender: TObject);
begin
  if (edShortcut.Text <> '') and SynTemplate.Modified then
  begin
    if (StyledMessageDlg(_(SCodeTemplateModified), mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      actUpdateItemExecute(Sender);
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
      Selected := True;
      MakeVisible(False);
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
  if (edShortCut.Text <> '') and (FOldIndex >= 0) then begin
    for i := 0 to lvItems.Items.Count - 1 do
      if (CompareText(lvItems.Items[i].Caption, edShortCut.Text) = 0) and
         (i <> FOldIndex) then
      begin
        StyledMessageDlg(_(SSameName), mtError, [mbOK], 0);
        Exit;
      end;
    with lvItems.Items[FOldIndex] do begin
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
      MakeVisible(True);
    end;
  end;
end;

procedure TCodeTemplates.actMoveDownExecute(Sender: TObject);
Var
  Name, Value : string;
  P : Pointer;
  Index : integer;
begin
  if lvItems.ItemIndex < lvItems.Items.Count - 1 then
  begin
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
      MakeVisible(True);
    end;
  end;
end;

procedure TCodeTemplates.lvItemsDeletion(Sender: TObject; Item: TListItem);
begin
  if Assigned(Item.Data) then
    TStringList(Item.Data).Free;
end;

procedure TCodeTemplates.lvItemsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Selected then begin
    FOldIndex := Item.Index;
    edShortCut.Text := Item.Caption;
    edDescription.Text := Item.SubItems[0];
    SynTemplate.Lines.Assign(TStringList(Item.Data));
  end else begin
    AskToUpdate(Sender);
    FOldIndex := -1;
    edShortCut.Text := '';
    edDescription.Text := '';
    SynTemplate.Text := '';
  end;
  SynTemplate.Modified := False;
end;

procedure TCodeTemplates.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (ModalResult = mrOk) then
    AskToUpdate(Sender);
  GetItems;
end;

end.
