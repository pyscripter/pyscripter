unit dlgImportDirectory;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.ImageList,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  dlgPyIDEBase;

type
  TImportDirectoryForm = class(TPyIDEDlgBase)
    Panel1: TPanel;
    ebMask: TEdit;
    cbRecursive: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    DirectoryEdit: TButtonedEdit;
    vilImages: TVirtualImageList;
    procedure DirectoryEditBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    class var FileMasks : string;
    class var Directory : string;
    class var Recursive : Boolean;
    class function Execute: Boolean;
  end;


implementation

uses
  Winapi.ShLwApi,
  Vcl.FileCtrl,
  Vcl.Graphics,
  Vcl.Themes,
  JvGnuGetText,
  dmResources;

{$R *.dfm}

{ TImportDirectoryForm }

procedure TImportDirectoryForm.DirectoryEditBtnClick(Sender: TObject);
var
  NewDir: string;
  Directories : TArray<string>;
begin
  NewDir := DirectoryEdit.Text;
  if SelectDirectory(NewDir, Directories, [], _('Select directory')) then
    DirectoryEdit.Text := Directories[0];
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
    SHAutoComplete(DirectoryEdit.Handle, SHACF_FILESYSTEM or SHACF_AUTOAPPEND_FORCE_ON or
      SHACF_AUTOSUGGEST_FORCE_OFF);
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
