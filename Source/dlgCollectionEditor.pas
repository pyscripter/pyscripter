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
  WinApi.Windows,
  System.Types,
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  dlgPyIDEBase;

type
  TItemEditFunction = function(Item : TCollectionItem): Boolean;

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
  private
    fCollection : TCollection;
    fItemEdit : TItemEditFunction;
    procedure CheckButtons;
    procedure UpdateList;
  end;

 function EditCollection(ACollection: TCollection;
   ItemClass: TCollectionItemClass; ACaption : string;
   ItemEditFunction: TItemEditFunction;
   AHelpContext: integer): Boolean;

implementation

uses
  System.Math,
  JVBoxProcs;

{$R *.dfm}

function EditCollection(ACollection: TCollection;
   ItemClass: TCollectionItemClass; ACaption : string;
   ItemEditFunction: TItemEditFunction;
   AHelpContext: integer): Boolean;
begin
  Result := False;
  if not (Assigned(ACollection) and Assigned(ItemEditFunction)) then Exit;

  with TCollectionEditor.Create(Application) do
  try
    fCollection := TCollection.Create(ItemClass);
    Caption := ACaption;
    HelpContext := AHelpContext;
    fItemEdit := ItemEditFunction;
    fCollection.Assign(ACollection);
    UpdateList;
    Result := ShowModal = mrOK;
    if Result then
      ACollection.Assign(fCollection);
  finally
    Release;
  end;
end;

//=== { TConfigureTools } =============================================

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
  if fItemEdit(fCollection.Add) then begin
    UpdateList;
    ItemList.ItemIndex := ItemList.Count - 1;
    CheckButtons;
  end else
    fCollection.Delete(fCollection.Count - 1);
end;

procedure TCollectionEditor.ModifyBtnClick(Sender: TObject);
Var
  Index : integer;
begin
  Index := ItemList.ItemIndex;
  if (Index >= 0) and fItemEdit(fCollection.FindItemID(
      Integer(ItemList.Items.Objects[Index])))
  then begin
    UpdateList;
    ItemList.ItemIndex := Index;
    CheckButtons;
  end;
end;

procedure TCollectionEditor.RemoveBtnClick(Sender: TObject);
begin
  if ItemList.ItemIndex >= 0 then begin
    fCollection.Delete(fCollection.FindItemID(
      Integer(ItemList.Items.Objects[ItemList.ItemIndex])).Index);
    UpdateList;
  end;
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
    fCollection.Items[ItemList.ItemIndex].Index :=
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

procedure TCollectionEditor.FormDestroy(Sender: TObject);
begin
  fCollection.Free;
end;

procedure TCollectionEditor.UpdateList;
var
  I: Integer;
begin
  ItemList.Clear;
  for i := 0 to fCollection.Count - 1 do
    ItemList.Items.AddObject(fCollection.Items[i].DisplayName,
      TObject(fCollection.Items[i].ID));
  CheckButtons;
end;

procedure TCollectionEditor.MoveDownBtnClick(Sender: TObject);
Var
  Index : integer;
begin
  Index := ItemList.ItemIndex;
  if InRange(Index, 0, ItemList.Count - 2) then begin
    fCollection.Items[Index].Index := fCollection.Items[Index].Index + 1;
    UpdateList;
    ItemList.ItemIndex := Index + 1;
    CheckButtons;
  end;
end;

procedure TCollectionEditor.MoveUpBtnClick(Sender: TObject);
Var
  Index : integer;
begin
  Index := ItemList.ItemIndex;
  if Index > 0 then begin
    fCollection.Items[Index].Index := fCollection.Items[Index].Index - 1;
    UpdateList;
    ItemList.ItemIndex := Index - 1;
    CheckButtons;
  end;
end;

end.


