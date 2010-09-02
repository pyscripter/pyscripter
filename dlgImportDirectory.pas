unit dlgImportDirectory;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Controls, Forms,
  Dialogs, StdCtrls, 
  SpTBXControls, dlgPyIDEBase, SpTBXItem, SpTBXEditors;

type
  TImportDirectoryForm = class(TPyIDEDlgBase)
    Panel1: TSpTBXPanel;
    ebMask: TEdit;
    cbRecursive: TCheckBox;
    Label1: TSpTBXLabel;
    Label2: TSpTBXLabel;
    Button1: TSpTBXButton;
    Button2: TSpTBXButton;
    DirectoryEdit: TSpTBXButtonEdit;
    procedure DirectoryEditBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    class var
      FileMasks : string;
      Directory : string;
      Recursive : Boolean;
    class function Execute: Boolean;
  end;


implementation

uses FileCtrl;

{$R *.dfm}

{ TImportDirectoryForm }

procedure TImportDirectoryForm.DirectoryEditBtnClick(Sender: TObject);
var
  NewDir: string;
begin
  NewDir := DirectoryEdit.Text;
  if SelectDirectory('Select Directory:', '', NewDir) then
    DirectoryEdit.Text := NewDir;
end;

class function TImportDirectoryForm.Execute: Boolean;
Var
  Owner : TCustomForm;
begin
  if Assigned(Screen.ActiveCustomForm) then
    Owner := Screen.ActiveCustomForm
  else
    Owner := Application.MainForm;
  with TImportDirectoryForm.Create(Owner) do begin
    Result := False;
    DirectoryEdit.Text := Directory;
    ebMask.Text := FileMasks;
    cbRecursive.Checked := Recursive;
    if ShowModal = mrOK then begin
      Result := True;
      FileMasks := ebMask.Text;
      Directory := DirectoryEdit.Text;
      Recursive := cbRecursive.Checked;
    end;
  end;
end;

initialization
  TImportDirectoryForm.FileMasks := '*.py;*.pyw';
  TImportDirectoryForm.Recursive := True;
end.
