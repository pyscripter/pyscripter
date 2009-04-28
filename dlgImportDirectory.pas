unit dlgImportDirectory;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, JvExMask, JvToolEdit, ExtCtrls, SpTBXDkPanels,
  SpTBXControls, dlgPyIDEBase;

type
  TImportDirectoryForm = class(TPyIDEDlgBase)
    Panel1: TSpTBXPanel;
    DirectoryEdit: TJvDirectoryEdit;
    ebMask: TEdit;
    cbRecursive: TCheckBox;
    Label1: TSpTBXLabel;
    Label2: TSpTBXLabel;
    Button1: TSpTBXButton;
    Button2: TSpTBXButton;
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

uses uCommonFunctions, dmCommands, uHighlighterProcs;

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
