unit dlgPyIDEBase;

interface

uses
  Vcl.Forms;

type
  TPyIDEDlgBase = class(TForm)
    procedure FormCreate(Sender: TObject);
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
  Font.PixelsPerInch := FCurrentPPI;
  SetDefaultUIFont(Font);
  {$IF CompilerVersion >= 36}
  Font.IsDPIRelated := True;
  {$ELSE}
  Font.PixelsPerInch := Screen.PixelsPerInch;
  {$ENDIF}
end;

end.
