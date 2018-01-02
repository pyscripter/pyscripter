{-----------------------------------------------------------------------------
 Unit Name: dlgConfigureTools
 Author:    Kiriakos Vlahos
 Date:      03-Jun-2005
 Purpose:   Dialog for Configuring Tools
 History:
-----------------------------------------------------------------------------}
unit dlgConfigureTools;

interface

uses
  Types, SysUtils, Classes, Windows, Controls, Forms, StdCtrls,
  SpTBXControls, SpTBXEditors, dlgPyIDEBase, SpTBXItem, Vcl.ExtCtrls;

type
  TConfigureTools = class(TPyIDEDlgBase)
    SpTBXPanel1: TSpTBXPanel;
    AddBtn: TSpTBXButton;
    RemoveBtn: TSpTBXButton;
    ModifyBtn: TSpTBXButton;
    OKBtn: TSpTBXButton;
    CancelBtn: TSpTBXButton;
    MoveUpBtn: TSpTBXButton;
    MoveDownBtn: TSpTBXButton;
    ToolsList: TSpTBXListBox;
    procedure AddBtnClick(Sender: TObject);
    procedure ModifyBtnClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure ToolsListClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ToolsListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ToolsListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MoveDownBtnClick(Sender: TObject);
    procedure MoveUpBtnClick(Sender: TObject);
  private
    fTools : TCollection;
    procedure CheckButtons;
    procedure UpdateList;
  end;

function ConfigureTools(Tools : TCollection): Boolean;

implementation

uses
  JVBoxProcs, cTools, dlgToolProperties,
  Math;

{$R *.dfm}

function ConfigureTools(Tools : TCollection): Boolean;
begin
  Result := False;
  if not Assigned(Tools) then Exit;

  with TConfigureTools.Create(Application) do
  try
    fTools.Assign(Tools);
    UpdateList;
    Result := ShowModal = mrOK;
    if Result and Assigned(Tools) then
      Tools.Assign(fTools);
  finally
    Release;
  end;
end;

//=== { TConfigureTools } =============================================

procedure TConfigureTools.CheckButtons;
begin
  ModifyBtn.Enabled := (ToolsList.Items.Count > 0) and
    (ToolsList.ItemIndex >= 0);
  RemoveBtn.Enabled := ModifyBtn.Enabled;
  MoveUpBtn.Enabled := ToolsList.ItemIndex > 0;
  MoveDownBtn.Enabled := InRange(ToolsList.ItemIndex, 0, ToolsList.Count - 2);
end;

procedure TConfigureTools.AddBtnClick(Sender: TObject);
begin
  if EditTool((fTools.Add as TToolItem).ExternalTool) then begin
    UpdateList;
    ToolsList.ItemIndex := ToolsList.Count - 1;
    CheckButtons;
  end else
    fTools.Delete(fTools.Count - 1);
end;

procedure TConfigureTools.ModifyBtnClick(Sender: TObject);
Var
  Index : integer;
begin
  Index := ToolsList.ItemIndex;
  if (Index >= 0) and EditTool((fTools.FindItemID(
      Integer(ToolsList.Items.Objects[Index]))
      as TToolItem).ExternalTool)
  then begin
    UpdateList;
    ToolsList.ItemIndex := Index;
    CheckButtons;
  end;
end;

procedure TConfigureTools.RemoveBtnClick(Sender: TObject);
begin
  if ToolsList.ItemIndex >= 0 then begin
    fTools.Delete(fTools.FindItemID(
      Integer(ToolsList.Items.Objects[ToolsList.ItemIndex])).Index);
    UpdateList;
  end;
end;

procedure TConfigureTools.ToolsListClick(Sender: TObject);
begin
  CheckButtons;
end;

procedure TConfigureTools.FormShow(Sender: TObject);
begin
  CheckButtons;
end;

procedure TConfigureTools.ToolsListDragDrop(Sender, Source: TObject;
  X, Y: Integer);
begin
  if ToolsList.ItemIndex >= 0 then begin
    fTools.Items[ToolsList.ItemIndex].Index :=
      ToolsList.ItemAtPos(Point(X, Y), True);
    UpdateList;
  end;
end;

procedure TConfigureTools.ToolsListDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  BoxDragOver(ToolsList, Source, X, Y, State, Accept, ToolsList.Sorted);
  CheckButtons;
end;

procedure TConfigureTools.FormCreate(Sender: TObject);
begin
  inherited;
  fTools := TCollection.Create(TToolItem);
end;

procedure TConfigureTools.FormDestroy(Sender: TObject);
begin
  fTools.Free;
end;

procedure TConfigureTools.UpdateList;
var
  I: Integer;
begin
  ToolsList.Clear;
  for i := 0 to fTools.Count - 1 do
    ToolsList.Items.AddObject(ftools.Items[i].DisplayName,
      TObject(fTools.Items[i].ID));
  CheckButtons;
end;

procedure TConfigureTools.MoveDownBtnClick(Sender: TObject);
Var
  Index : integer;
begin
  Index := ToolsList.ItemIndex;
  if InRange(Index, 0, ToolsList.Count - 2) then begin
    fTools.Items[Index].Index := fTools.Items[Index].Index + 1;
    UpdateList;
    ToolsList.ItemIndex := Index + 1;
    CheckButtons;
  end;
end;

procedure TConfigureTools.MoveUpBtnClick(Sender: TObject);
Var
  Index : integer;
begin
  Index := ToolsList.ItemIndex;
  if Index > 0 then begin
    fTools.Items[Index].Index := fTools.Items[Index].Index - 1;
    UpdateList;
    ToolsList.ItemIndex := Index - 1;
    CheckButtons;
  end;
end;

end.


