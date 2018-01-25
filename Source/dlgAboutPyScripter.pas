{-----------------------------------------------------------------------------
 Unit Name: dlgAboutPyScripter
 Author:    Kiriakos Vlahos
 Date:      09-Mar-2005
 Purpose:   PyScripter About box
 History:
-----------------------------------------------------------------------------}

unit dlgAboutPyScripter;

interface

uses
  Winapi.Windows, Winapi.Messages, SysUtils, Classes, Graphics, Forms, Controls,
  Buttons, ExtCtrls, dlgPyIDEBase, SpTBXControls, SpTBXItem, SpTBXTabs, TB2Item,
  SpTBXPageScroller, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TRichEdit = class(Vcl.ComCtrls.TRichEdit)
  private
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
  protected
    procedure CreateWnd; override;
  end;

  TAboutBox = class(TPyIDEDlgBase)
    SpTBXTabControl1: TSpTBXTabControl;
    tbAbout: TSpTBXTabItem;
    SpTBXTabSheet1: TSpTBXTabSheet;
    tbCredits: TSpTBXTabItem;
    SpTBXTabSheet2: TSpTBXTabSheet;
    tbLinks: TSpTBXTabItem;
    SpTBXTabSheet3: TSpTBXTabSheet;
    ScrollBox: TSpTBXPageScroller;
    Panel1: TPanel;
    ProgramIcon: TImage;
    Copyright: TLabel;
    Version: TLabel;
    ProductName: TLabel;
    Comments: TLabel;
    reLinks: TRichEdit;
    reCredits: TRichEdit;
    Panel2: TPanel;
    procedure Panel1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure reCreditsResizeRequest(Sender: TObject; Rect: TRect);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBox: TAboutBox;

implementation

uses
  Winapi.ShellAPI, Winapi.RichEdit, uCommonFunctions, JvJCLUtils, JvGnugettext;

{$R *.dfm}

resourcestring
  SAboutBoxCreditsIntro =
    'Special thanks to the many great developers who,'+
    'with their amazing work, have made PyScripter'+
    'possible.  PyScripter makes use of the following'+
    'components and projects:'+ SLineBreak;
  SAboutBoxCredits =
    'Python for Delphi (www.github.com/pyscripter/python4delphi)'+ SLineBreak +
    'Rpyc (www.github.com/tomerfiliba/rpyc)'+ SLineBreak +
    'JVCL (www.github.com/project-jedi/jvcl)'+ SLineBreak +
    'SynEdit (www.sourceforge.net/projects/synedit)'+ SLineBreak +
    'VirtualTreeView (www.github.com/Virtual-TreeView/)'+ SLineBreak +
    'VirtualShellTools (www.github.com/pyscripter/mustangpeakvirtualshelltools)'+ SLineBreak +
    'GExperts (www.gexperts.org)'+ SLineBreak +
    'Syn Editor (www.sourceforge.net/projects/syn)'+ SLineBreak +
    'Syn Web highlighters (www.github.com/KrystianBigaj/synweb)' + SLineBreak +
    'Toolbar2000 (www.jrsoftware.org/tb2k.php)'+ SLineBreak +
    'SpTBXLib(www.silverpointdevelopment.com)' + SLineBreak +
    'TCommandLineReader(www.benibela.de)'+ SLineBreak +
    'Silk icons(www.famfamfam.com)'+ SLineBreak;

  SAboutBoxCreditsTranslations =
    'Translation manager: Lübbe Onken'+ SLineBreak +
    'Chinese translation by "Love China"'+ SLineBreak +
    'French translation by Groupe AmiensPython'+ SLineBreak +
    'Japanese translation by Tokibito'+ SLineBreak +
    'Russian translation by Aleksander Dragunkin'+ SLineBreak +
    'Slovak translation by Marian Denes'+ SLineBreak +
    'Spanish translation by Javier Pimas'+ SLineBreak +
    'German translation by Daniel Frost'+ SLineBreak;

   SAboutBoxCreditsThemeDesign =
     'Tanmaya Meher (www.github.com/tanmayameher)'+ SLineBreak +
     'jprzywoski (www.github.com/jprzywoski)'+ SLineBreak;

   SAboutBoxLinks =
    'The project home, Issue Tracker and source code repository are hosted at Github (www.github.com/pyscripter/pyscripter)'+
    SLineBreak+
    'Internet group support is available at https://groups.google.com/group/PyScripter'+
    SLineBreak +
    'Please submit bug reports and questions about PyScripter to pyscripter@gmail.com.';

const
  AURL_ENABLEURL = 1;
  AURL_ENABLEEAURLS = 8;

procedure TRichEdit.CreateWnd;
var
  mask: LResult;
begin
  inherited;
  mask := SendMessage(Handle, EM_GETEVENTMASK, 0, 0);
  SendMessage(Handle, EM_SETEVENTMASK, 0, mask or ENM_LINK);
  SendMessage(Handle, EM_AUTOURLDETECT, AURL_ENABLEURL, 0);
end;

procedure TRichEdit.CNNotify(var Message: TWMNotify);
type
  PENLink = ^TENLink;
var
  p: PENLink;
  tr: TEXTRANGE;
  url: array of Char;
begin
  if (Message.NMHdr.code = EN_LINK) then begin
    p := PENLink(Message.NMHdr);
    if (p.Msg = WM_LBUTTONDOWN) then begin
      { optionally, enable this:
      if CheckWin32Version(6, 2) then begin
        // on Windows 8+, returning EN_LINK_DO_DEFAULT directs
        // the RichEdit to perform the default action...
        Message.Result :=  EN_LINK_DO_DEFAULT;
        Exit;
      end;
      }
      try
        SetLength(url, p.chrg.cpMax - p.chrg.cpMin + 1);
        tr.chrg := p.chrg;
        tr.lpstrText := PChar(url);
        SendMessage(Handle, EM_GETTEXTRANGE, 0, LPARAM(@tr));
        ShellExecute(Handle, nil, PChar(url), nil, nil, SW_SHOWNORMAL);
      except
        {ignore}
      end;
      Exit;
    end;
  end;
  inherited;
end;

procedure TAboutBox.Panel1Click(Sender: TObject);
begin
  Close;
end;

procedure TAboutBox.reCreditsResizeRequest(Sender: TObject; Rect: TRect);
begin
  reCredits.BoundsRect := Rect;
end;

procedure TAboutBox.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    Close;
end;

procedure TAboutBox.FormCreate(Sender: TObject);
Var
  winplatform : string;
begin
  {$IFDEF WIN64}
  winplatform := 'x64';
  {$ELSE}
  winplatform := 'x86';
  {$ENDIF}
  inherited;
  Version.Caption := Format('Version %s %s', [ApplicationVersion, winplatform]);

  AddFormatText(reLinks, _('Links') + SLineBreak, [fsBold]);
  reLinks.Paragraph.Numbering := nsBullet;
  AddFormatText(reLinks,SAboutBoxLinks);
  reLinks.ReadOnly := True;

  AddFormatText(reCredits, _('Credits') + SLineBreak, [fsBold]);
  AddFormatText(reCredits,SAboutBoxCreditsIntro);
  reCredits.Paragraph.Numbering := nsBullet;
  AddFormatText(reCredits,SAboutBoxCredits);
  reCredits.Paragraph.Numbering := nsNone;
  AddFormatText(reCredits,_('Translations') + ':' + SLineBreak, [fsItalic]);
  reCredits.Paragraph.Numbering := nsBullet;
  AddFormatText(reCredits,SAboutBoxCreditsTranslations);
  reCredits.Paragraph.Numbering := nsNone;
  AddFormatText(reCredits,_('Theme design') + ':' + SLineBreak, [fsItalic]);
  reCredits.Paragraph.Numbering := nsBullet;
  AddFormatText(reCredits,SAboutBoxCreditsThemeDesign);
  reCredits.Paragraph.Numbering := nsNone;

  reCredits.SelStart := 0;
  reCredits.SelLength := 0;
  reCredits.ReadOnly := True;
end;

initialization
  TP_GlobalIgnoreClassProperty(TAboutBox, 'Copyright');
  TP_GlobalIgnoreClassProperty(TAboutBox, 'Version');
  TP_GlobalIgnoreClassProperty(TAboutBox, 'ProductName');
end.

