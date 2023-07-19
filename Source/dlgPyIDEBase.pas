unit dlgPyIDEBase;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TPyIDEDlgBase = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PyIDEDlgBase: TPyIDEDlgBase;

implementation

uses
  uCommonFunctions,
  JvGnugettext;

{$R *.dfm}

procedure TPyIDEDlgBase.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  var LFont := TFont.Create;
  try
    SetDefaultUIFont(LFont);
    LFont.Height := MulDiv(LFont.Height, FCurrentPPI, Font.PixelsPerInch);
    Font.Assign(LFont);
  finally
    LFont.Free;
  end;
end;

end.
