{-----------------------------------------------------------------------------
 Unit Name: dlgAboutPyScripter
 Author:    Kiriakos Vlahos
 Date:      09-Mar-2005
 Purpose:   PyScripter About box
 History:
-----------------------------------------------------------------------------}

unit dlgAboutPyScripter;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ComCtrls,  JvLinkLabel, JvExControls, JvComponent, ExtCtrls;

type
  TAboutBox = class(TForm)
    Panel2: TPanel;                  
    PageControl: TPageControl;
    AboutTab: TTabSheet;
    Panel1: TPanel;
    CreditsTab: TTabSheet;
    ScrollBox: TScrollBox;
    JvLinkLabel: TJvLinkLabel;
    ProgramIcon: TImage;
    Copyright: TLabel;
    Comments: TLabel;
    Version: TLabel;
    ProductName: TLabel;
    procedure Panel1Click(Sender: TObject);
    procedure JvLinkLabelLinkClick(Sender: TObject; LinkNumber: Integer;
      LinkText, LinkParam: String);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBox: TAboutBox;

implementation

uses
  uCommonFunctions, JvJCLUtils;

{$R *.dfm}

procedure TAboutBox.Panel1Click(Sender: TObject);
begin
  Close;
end;

procedure TAboutBox.JvLinkLabelLinkClick(Sender: TObject;
  LinkNumber: Integer; LinkText, LinkParam: String);
begin
  OpenObject('http://'+LinkText);
end;

procedure TAboutBox.FormCreate(Sender: TObject);
begin
  Version.Caption := 'Version ' + ApplicationVersion;
end;

end.

