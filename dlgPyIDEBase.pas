unit dlgPyIDEBase;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TntForms;

type
  TPyIDEDlgBase = class(TTntForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PyIDEDlgBase: TPyIDEDlgBase;

implementation

uses gnugettext, uCommonFunctions;

{$R *.dfm}

procedure TPyIDEDlgBase.FormCreate(Sender: TObject);
begin
  //SetDesktopIconFonts(Self.Font);
  TranslateComponent(Self);
end;

end.
