unit dlgImportDirectory;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, JvExMask, JvToolEdit, ExtCtrls;

type
  TImportDirectoryForm = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    DirectoryEdit: TJvDirectoryEdit;
    Label2: TLabel;
    ebMask: TEdit;
    Button1: TButton;
    Button2: TButton;
    cbRecursive: TCheckBox;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    class var
      FileMask : string;
      Directory : string;
      Recursive : Boolean;
    class function Execute: Boolean;
  end;


implementation

uses uCommonFunctions;

{$R *.dfm}

{ TImportDirectoryForm }

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
    DirectoryEdit.InitialDir := Directory;
    DirectoryEdit.Text := Directory;
    ebMask.Text := FileMask;
    cbRecursive.Checked := Recursive;
    if ShowModal = mrOK then begin
      Result := True;
      FileMask := ebMask.Text;
      Directory := DirectoryEdit.Text;
      Recursive := cbRecursive.Checked;
    end;
  end;
end;

procedure TImportDirectoryForm.FormCreate(Sender: TObject);
begin
  SetDesktopIconFonts(Font);
end;

initialization
  TImportDirectoryForm.FileMask := '*.py';
  TImportDirectoryForm.Recursive := True;
end.
