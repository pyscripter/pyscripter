{-----------------------------------------------------------------------------
 Unit Name: dlgCollectionEditor
 Author:    Kiriakos Vlahos
 Date:      03-Jun-2005
 Purpose:   Geeric Dialog for Editing Collections
 History:
-----------------------------------------------------------------------------}
unit dlgCollectionEditor;

interface

uses
  System.UITypes,
  System.Classes,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  dlgPyIDEBase;

type
  TItemEditFunction = function(Item : TCollectionItem): Boolean;

  TCEDialogType = (cetEdit, cetSelect);

  TCollectionEditor = class(TPyIDEDlgBase)
    Panel1: TPanel;
    ItemList: TListBox;
    AddBtn: TButton;
    RemoveBtn: TButton;
    ModifyBtn: TButton;
    OKBtn: TButton;
    CancelBtn: TButton;
    MoveUpBtn: TButton;
    MoveDownBtn: TButton;
    procedure AddBtnClick(Sender: TObject);
    procedure ModifyBtnClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure ItemListClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ItemListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ItemListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure MoveDownBtnClick(Sender: TObject);
    procedure MoveUpBtnClick(Sender: TObject);
    procedure ItemDoubleClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FCollection : TCollection;
    FItemEdit : TItemEditFunction;
    FDialogType : TCEDialogType;
    procedure CheckButtons;
    procedure UpdateList;
  end;

function EditCollection(ACollection: TCollection;
   ItemClass: TCollectionItemClass; ACaption : string;
   ItemEditFunction: TItemEditFunction;
   AHelpContext: Integer): Boolean;

function SelectFromCollection(ACollection: TCollection;
   ItemClass: TCollectionItemClass; ACaption : string;
   ItemEditFunction: TItemEditFunction;
   AHelpContext: Integer; var SelectedIndex : Integer;
   ADialogType : TCEDialogType = cetSelect): Boolean;

implementation

uses
  System.Math,
  System.Types,
  Vcl.Forms,
  Vcl.Dialogs,
  JvBoxProcs,
  JvGnugettext,
  uCommonFunctions;

{$R *.dfm}

function EditCollection(ACollection: TCollection;
   ItemClass: TCollectionItemClass; ACaption : string;
   ItemEditFunction: TItemEditFunction;
   AHelpContext: Integer): Boolean;
var
  SelectedIndex : Integer;
begin
  Result := SelectFromCollection(ACollection, ItemClass, ACaption,
    ItemEditFunction, AHelpContext, SelectedIndex, cetEdit);
end;

function SelectFromCollection(ACollection: TCollection;
   ItemClass: TCollectionItemClass; ACaption : string;
   ItemEditFunction: TItemEditFunction;
   AHelpContext: Integer; var SelectedIndex : Integer;
   ADialogType : TCEDialogType = cetSelect): Boolean;
begin
  Result := False;
  SelectedIndex := -1;
  if not (Assigned(ACollection) and Assigned(ItemEditFunction)) then Exit;

  with TCollectionEditor.Create(Application) do
  try
    FCollection := TCollection.Create(ItemClass);
    FDialogType := ADialogType;
    if FDialogType = cetSelect then
      ItemList.OnDblClick := ItemDoubleClick;
    Caption := ACaption;
    HelpContext := AHelpContext;
    FItemEdit := ItemEditFunction;
    FCollection.Assign(ACollection);
    UpdateList;
    Result := ShowModal = mrOk;
    if Result then
    begin
      ACollection.Assign(FCollection);
      SelectedIndex := ItemList.ItemIndex;
      Assert((ADialogType = cetEdit) or (SelectedIndex >= 0));
    end;
  finally
    Release;
  end;

end;

//=== { TCollectionEditor } =============================================

procedure TCollectionEditor.CheckButtons;
begin
  ModifyBtn.Enabled := (ItemList.Items.Count > 0) and
    (ItemList.ItemIndex >= 0);
  RemoveBtn.Enabled := ModifyBtn.Enabled;
  MoveUpBtn.Enabled := ItemList.ItemIndex > 0;
  MoveDownBtn.Enabled := InRange(ItemList.ItemIndex, 0, ItemList.Count - 2);
end;

procedure TCollectionEditor.AddBtnClick(Sender: TObject);
begin
  if FItemEdit(FCollection.Add) then begin
    UpdateList;
    ItemList.ItemIndex := ItemList.Count - 1;
    CheckButtons;
  end else
    FCollection.Delete(FCollection.Count - 1);
end;

procedure TCollectionEditor.ModifyBtnClick(Sender: TObject);
var
  Index : Integer;
begin
  Index := ItemList.ItemIndex;
  if (Index >= 0) and FItemEdit(FCollection.FindItemID(
      NativeInt(ItemList.Items.Objects[Index])))
  then begin
    UpdateList;
    ItemList.ItemIndex := Index;
    CheckButtons;
  end;
end;

procedure TCollectionEditor.RemoveBtnClick(Sender: TObject);
begin
  if ItemList.ItemIndex >= 0 then begin
    FCollection.Delete(FCollection.FindItemID(
      NativeInt(ItemList.Items.Objects[ItemList.ItemIndex])).Index);
    UpdateList;
  end;
end;

procedure TCollectionEditor.ItemDoubleClick(Sender: TObject);
begin
  if ItemList.ItemIndex >= 0 then ModalResult := mrOk;
end;

procedure TCollectionEditor.ItemListClick(Sender: TObject);
begin
  CheckButtons;
end;

procedure TCollectionEditor.FormShow(Sender: TObject);
begin
  CheckButtons;
end;

procedure TCollectionEditor.ItemListDragDrop(Sender, Source: TObject;
  X, Y: Integer);
begin
  if ItemList.ItemIndex >= 0 then begin
    FCollection.Items[ItemList.ItemIndex].Index :=
      ItemList.ItemAtPos(Point(X, Y), True);
    UpdateList;
  end;
end;

procedure TCollectionEditor.ItemListDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  BoxDragOver(ItemList, Source, X, Y, State, Accept, ItemList.Sorted);
  CheckButtons;
end;

procedure TCollectionEditor.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
   CanClose := (FDialogType = cetEdit) or (ModalResult = mrCancel) or (ItemList.ItemIndex >= 0);
   if not CanClose then
      StyledMessageDlg(_('Please make a selection'), mtError, [mbOK], 0);
end;

procedure TCollectionEditor.FormDestroy(Sender: TObject);
begin
  FCollection.Free;
end;

procedure TCollectionEditor.UpdateList;
begin
  ItemList.Clear;
  for var I := 0 to FCollection.Count - 1 do
    ItemList.Items.AddObject(FCollection.Items[I].DisplayName,
      TObject(FCollection.Items[I].ID));
  CheckButtons;
end;

procedure TCollectionEditor.MoveDownBtnClick(Sender: TObject);
var
  Index : Integer;
begin
  Index := ItemList.ItemIndex;
  if InRange(Index, 0, ItemList.Count - 2) then begin
    FCollection.Items[Index].Index := FCollection.Items[Index].Index + 1;
    UpdateList;
    ItemList.ItemIndex := Index + 1;
    CheckButtons;
  end;
end;

procedure TCollectionEditor.MoveUpBtnClick(Sender: TObject);
var
  Index : Integer;
begin
  Index := ItemList.ItemIndex;
  if Index > 0 then begin
    FCollection.Items[Index].Index := FCollection.Items[Index].Index - 1;
    UpdateList;
    ItemList.ItemIndex := Index - 1;
    CheckButtons;
  end;
end;

end.


