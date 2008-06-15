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
  Buttons, ComCtrls,  JvLinkLabel, JvExControls, JvComponent, ExtCtrls,
  dlgPyIDEBase, TBXDkPanels, SpTBXControls;

type
  TAboutBox = class(TPyIDEDlgBase)
    Panel2: TPanel;                  
    PageControl: TPageControl;
    AboutTab: TTabSheet;
    CreditsTab: TTabSheet;
    ScrollBox: TScrollBox;
    JvLinkLabel: TJvLinkLabel;
    TabSheet1: TTabSheet;
    ScrollBox1: TScrollBox;
    JvLinkLabel1: TJvLinkLabel;
    Panel1: TSpTBXPanel;
    ProgramIcon: TImage;
    Copyright: TSpTBXLabel;
    Comments: TSpTBXLabel;
    Version: TSpTBXLabel;
    ProductName: TSpTBXLabel;
    procedure Panel1Click(Sender: TObject);
    procedure JvLinkLabelLinkClick(Sender: TObject; LinkNumber: Integer;
      LinkText, LinkParam: String);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBox: TAboutBox;

implementation

uses
  uCommonFunctions, JvJCLUtils, gnugettext;

{$R *.dfm}

procedure TAboutBox.Panel1Click(Sender: TObject);
begin
  Close;
end;

procedure TAboutBox.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    Close;
end;

procedure TAboutBox.JvLinkLabelLinkClick(Sender: TObject;
  LinkNumber: Integer; LinkText, LinkParam: String);
begin
  OpenObject('http://'+LinkText);
end;

procedure TAboutBox.FormCreate(Sender: TObject);
begin
  inherited;
  Version.Caption := 'Version ' + ApplicationVersion;
end;

initialization
  TP_GlobalIgnoreClass (TJvLinkLabel);
  TP_GlobalIgnoreClassProperty(TAboutBox, 'Copyright');
  TP_GlobalIgnoreClassProperty(TAboutBox, 'Version');
  TP_GlobalIgnoreClassProperty(TAboutBox, 'ProductName');
end.

