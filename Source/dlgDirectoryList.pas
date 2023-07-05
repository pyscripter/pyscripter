{-----------------------------------------------------------------------------
 Unit Name: dlgDirectoryList
 Author:    Kiriakos Vlahos
 Date:      03-09-2008
 Purpose:   Dialog for editing a list of paths
 History:
-----------------------------------------------------------------------------}

unit dlgDirectoryList;

interface

uses
  Winapi.Windows,
  System.Types,
  System.SysUtils,
  System.Classes,
  System.ImageList,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  dlgPyIDEBase;

type
  TDirectoryListDialog = class(TPyIDEDlgBase)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    btnMoveUp: TButton;
    btnMoveDown: TButton;
    OKBtn: TButton;
    CancelBtn: TButton;
    btnAdd: TButton;
    btnReplace: TButton;
    btnDelete: TButton;
    Label1: TLabel;
    DirectoryList: TListBox;
    edPath: TButtonedEdit;
    vilImages: TVirtualImageList;
    procedure btnAddClick(Sender: TObject);
    procedure BtnPathClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure DirectoryListClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DirectoryListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DirectoryListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure edPathChange(Sender: TObject);
  private
    procedure CheckButtons;
  end;

function EditFolderList(Folders: TStrings; FormCaption : string = 'Directory List';
  HelpCntxt : integer = 0): Boolean;

implementation

uses
  Winapi.ShLwApi,
  Vcl.FileCtrl,
  Vcl.Themes,
  Vcl.Graphics,
  System.Math,
  JVBoxProcs,
  JvGnuGetText,
  dmResources;

{$R *.dfm}

function EditFolderList(Folders: TStrings; FormCaption : string = 'Directory List';
  HelpCntxt : integer = 0): Boolean; overload;
begin
  Assert(Assigned(Folders));
  with TDirectoryListDialog.Create(Application) do
  try
    Caption := FormCaption;
    HelpContext := HelpCntxt;
    DirectoryList.Items.Assign(Folders);
    SHAutoComplete(edPath.Handle, SHACF_FILESYSTEM or SHACF_AUTOAPPEND_FORCE_ON or
      SHACF_AUTOSUGGEST_FORCE_OFF);
    Result := ShowModal = mrOK;
    if Result then
      Folders.Assign(DirectoryList.Items);
  finally
    Free;
  end;
end;


//=== { TJvDirectoryListDialog } =============================================

procedure TDirectoryListDialog.CheckButtons;
Var
  ItemSelected : Boolean;
begin
  btnAdd.Enabled := edPath.Text <> '';

  ItemSelected := DirectoryList.ItemIndex >= 0;
  btnAdd.Enabled := edPath.Text <> '';
  btnReplace.Enabled := ItemSelected and (edPath.Text <> '');
  btnDelete.Enabled := ItemSelected;
  btnMoveDown.Enabled := ItemSelected and
    (DirectoryList.ItemIndex < DirectoryList.Items.Count - 1);
  btnMoveUp.Enabled := DirectoryList.ItemIndex >= 1;
end;

procedure TDirectoryListDialog.btnAddClick(Sender: TObject);
begin
  if edPath.Text <> '' then begin
    if DirectoryList.Items.IndexOf(edPath.Text) < 0 then begin
      DirectoryList.Items.Add(edPath.Text);
      DirectoryList.ItemIndex := DirectoryList.Count -1;
      CheckButtons;
    end;
  end;
end;

procedure TDirectoryListDialog.BtnPathClick(Sender: TObject);
var
  NewDir: string;
  Directories : TArray<string>;
begin
  NewDir := edPath.Text;
  if SelectDirectory(NewDir, Directories, [], _('Select directory')) then
    edPath.Text := Directories[0];
end;

procedure TDirectoryListDialog.btnDeleteClick(Sender: TObject);
var
  I: Integer;
begin
  if DirectoryList.ItemIndex < 0 then
    Exit;
  I := DirectoryList.ItemIndex;
  DirectoryList.Items.Delete(I);
  DirectoryList.ItemIndex := Max(0, I -1);
  CheckButtons;
end;

procedure TDirectoryListDialog.btnMoveDownClick(Sender: TObject);
Var
  Index : integer;
begin
  Index := DirectoryList.ItemIndex;
  if (Index >= 0) and (Index < DirectoryList.Items.Count - 1) then begin
    DirectoryList.Items.Move(Index, Index + 1);
    DirectoryList.ItemIndex := Index + 1;
    CheckButtons;
  end;
end;

procedure TDirectoryListDialog.btnMoveUpClick(Sender: TObject);
Var
  Index : integer;
begin
  Index := DirectoryList.ItemIndex;
  if Index > 0 then begin
    DirectoryList.Items.Move(Index, Index - 1);
    DirectoryList.ItemIndex := Index - 1;
    CheckButtons;
  end;
end;

procedure TDirectoryListDialog.btnReplaceClick(Sender: TObject);
var
  I: Integer;
begin
  I := DirectoryList.ItemIndex;
  if (I < 0) or (edPath.Text = '') or
    (DirectoryList.Items.IndexOf(edPath.Text) >= 0)
  then
    Exit;

  DirectoryList.Items[I] := edPath.Text;
  CheckButtons;
end;

procedure TDirectoryListDialog.DirectoryListClick(Sender: TObject);
begin
  if DirectoryList.ItemIndex >=0 then
    edPath.Text := DirectoryList.Items[DirectoryList.ItemIndex];

  CheckButtons;
end;

procedure TDirectoryListDialog.FormShow(Sender: TObject);
begin
  vilImages.GetIcon(3, Icon);
  CheckButtons;
end;

procedure TDirectoryListDialog.DirectoryListDragDrop(Sender, Source: TObject;
  X, Y: Integer);
begin
  BoxMoveFocusedItem(DirectoryList, DirectoryList.ItemAtPos(Point(X, Y), True));
  CheckButtons;
end;

procedure TDirectoryListDialog.DirectoryListDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  BoxDragOver(DirectoryList, Source, X, Y, State, Accept, DirectoryList.Sorted);
  CheckButtons;
end;

procedure TDirectoryListDialog.edPathChange(Sender: TObject);
begin
  CheckButtons;
end;

end.

