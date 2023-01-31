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
  SpTBXPageScroller, Vcl.StdCtrls, Vcl.ComCtrls, SVGIconImage;

type
  TRichEdit = class(Vcl.ComCtrls.TRichEdit)
  private
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
  protected
    procedure CreateWnd; override;
  end;

  TAboutBox = class(TPyIDEDlgBase)
    SpTBXTabControl: TSpTBXTabControl;
    tbAbout: TSpTBXTabItem;
    SpTBXTabSheet1: TSpTBXTabSheet;
    tbCredits: TSpTBXTabItem;
    SpTBXTabSheet2: TSpTBXTabSheet;
    tbLinks: TSpTBXTabItem;
    SpTBXTabSheet3: TSpTBXTabSheet;
    ScrollBox: TSpTBXPageScroller;
    Panel1: TPanel;
    Copyright: TLabel;
    Version: TLabel;
    ProductName: TLabel;
    Comments: TLabel;
    reLinks: TRichEdit;
    reCredits: TRichEdit;
    Panel2: TPanel;
    SVGIconImage1: TSVGIconImage;
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
const
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
    'SpTBXLib (www.silverpointdevelopment.com)' + SLineBreak +
    'SVGIconImageList (https://github.com/EtheaDev/SVGIconImageList)' + SLineBreak +
    'zControls/Detours library (https://github.com/MahdiSafsafi)' + SLineBreak +
    'TCommandLineReader (www.benibela.de)';

resourcestring
  SAboutBoxCreditsIntro =
    'Special thanks to the many great developers who,'+
    'with their amazing work, have made PyScripter '+
    'possible. PyScripter makes use of the following '+
    'components and projects:';

  SAboutBoxCreditsTranslationArabic  = 'Arabic: %s';
  SAboutBoxCreditsTranslationChinese = 'Chinese: %s';
  SAboutBoxCreditsTranslationFrench  = 'French: %s';
  SAboutBoxCreditsTranslationGerman  = 'German: %s';
  SAboutBoxCreditsTranslationGreek   = 'Greek: %s';
  SAboutBoxCreditsTranslationItalian = 'Italian: %s';
  SAboutBoxCreditsTranslationJapanese = 'Japanese: %s';
  SAboutBoxCreditsTranslationKabyle = 'Taqbaylit: %s';
  SAboutBoxCreditsTranslationPortugueseBR = 'Portuguese (Brazil): %s';
  SAboutBoxCreditsTranslationPortuguesePT = 'Portuguese (Portugal): %s';
  SAboutBoxCreditsTranslationRussian = 'Russian: %s';
  SAboutBoxCreditsTranslationSlovak  = 'Slovak: %s';
  SAboutBoxCreditsTranslationSpanish = 'Spanish: %s';

  SAboutBoxCreditsDonations =
    'Donations from numerous users have provided a strong incentive to develop this project.' + SLineBreak +
    'Financial and moral support from Embarcadero (https://www.embarcadero.com/) makers of Delphi, ' +
    'the programming environment with which PyScripter is built, and generous support '+
    'from Tranquil IT (https://www.tranquil.it) makers of WAPT (apt-get for Windows) '+
    '(https://www.wapt.fr) is thankfully acknowledged.';

  SAboutBoxLinks =
    'The project home, Issue Tracker and source code repository are hosted at Github (www.github.com/pyscripter/pyscripter)'+
  SLineBreak+
    'Internet group support is available at https://groups.google.com/group/PyScripter'+
  SLineBreak +
    'Please submit bug reports and questions about PyScripter to pyscripter@gmail.com.';

const
  AURL_ENABLEURL = 1;
  AURL_ENABLEEAURLS = 8;

type
  // in alphabetical order of the full English languages names!!
  // not in order of the gettext abbreviations.
  ECreditLanguages = (ar,zh,fr,de,el,it,ja,kab,pt_BR,pt_PT,ru,sk,es);

const
  cAboutLanguages : array[ECreditLanguages] of string = (
    SAboutBoxCreditsTranslationArabic,
    SAboutBoxCreditsTranslationChinese,
    SAboutBoxCreditsTranslationFrench,
    SAboutBoxCreditsTranslationGerman,
    SAboutBoxCreditsTranslationGreek,
    SAboutBoxCreditsTranslationItalian,
    SAboutBoxCreditsTranslationJapanese,
    SAboutBoxCreditsTranslationKabyle,
    SAboutBoxCreditsTranslationPortugueseBR,
    SAboutBoxCreditsTranslationPortuguesePT,
    SAboutBoxCreditsTranslationRussian,
    SAboutBoxCreditsTranslationSlovak,
    SAboutBoxCreditsTranslationSpanish
  );
  cAboutTranslationManager = 'Lübbe Onken';
  cAboutTranslators : array[ECreditLanguages] of string = (
    'Mohammed Nasman, Raouf Rahiche',
    '"Love China"',
    'Groupe AmiensPython, Vincent Maille, Phil Prost',
    'Daniel Frost, Lübbe Onken',
    'Kiriakos Vlahos',
    'Vincenzo Campanella, bovirus (bovirus@gmail.com)',
    'Tokibito',
    'Mohammed Belkacem',
    'Eric Szczepanik',
    'Gustavo Carreno',
    'Aleksander Dragunkin, Andrei Aleksandrov, Dmitry Arefiev',
    'Marian Denes',
    'Pedro Luis Larrosa, Victor Alberto Gil, Juan Carlos Cilleruelo'
  );

  cAboutBoxCreditsThemeDesign =
    'Adriana Díaz - Aumenta Software (https://aumenta.mx/)' + SLineBreak +
    'Salim Saddaquzzaman (https://github.com/sk-Prime)'+ SLineBreak +
    'Tanmaya Meher (www.github.com/tanmayameher)'+ SLineBreak +
    'jprzywoski (www.github.com/jprzywoski)';

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
  AboutBoxCreditsTranslations : string;
  Language : ECreditLanguages;
begin
  {$IFDEF WIN64}
  winplatform := 'x64';
  {$ELSE}
  winplatform := 'x86';
  {$ENDIF}
  inherited;
  Copyright.Caption := #$00A9' Kiriakos Vlahos 2005-' + CurrentYear.ToString;
  Version.Caption := Format('Version %s %s', [ApplicationVersion, winplatform]);

  AddFormatText(reLinks, _('Links') + SLineBreak, [fsBold]);
  reLinks.Paragraph.Numbering := nsBullet;
  AddFormatText(reLinks,SAboutBoxLinks);
  reLinks.ReadOnly := True;

  AddFormatText(reCredits, _('Credits') + SLineBreak, [fsBold]);
  AddFormatText(reCredits,SAboutBoxCreditsIntro + SLineBreak);
  reCredits.Paragraph.Numbering := nsBullet;
  AddFormatText(reCredits,SAboutBoxCredits + SLineBreak);

  reCredits.Paragraph.Numbering := nsNone;
  AddFormatText(reCredits, SLineBreak + _('Translation manager') + ':' + SLineBreak, [fsItalic]);
  reCredits.Paragraph.Numbering := nsBullet;
  AddFormatText(reCredits,cAboutTranslationManager + SLineBreak);

  reCredits.Paragraph.Numbering := nsNone;
  AddFormatText(reCredits, SLineBreak + _('Translators') + ':' + SLineBreak, [fsItalic]);

  for Language := Low(ECreditLanguages) to High(ECreditLanguages) do
    AboutBoxCreditsTranslations := AboutBoxCreditsTranslations +
      Format(cAboutLanguages[Language],[cAboutTranslators[Language]]) + sLineBreak;
  reCredits.Paragraph.Numbering := nsBullet;
  AddFormatText(reCredits,AboutBoxCreditsTranslations);

  reCredits.Paragraph.Numbering := nsNone;
  AddFormatText(reCredits, SLineBreak + _('Artwork and theme design') + ':' + SLineBreak, [fsItalic]);
  reCredits.Paragraph.Numbering := nsBullet;
  AddFormatText(reCredits, cAboutBoxCreditsThemeDesign + SLineBreak);
  reCredits.Paragraph.Numbering := nsNone;

  reCredits.Paragraph.Numbering := nsNone;
  AddFormatText(reCredits, SLineBreak + _('Donations') + ':' + SLineBreak, [fsItalic]);
  reCredits.Paragraph.Numbering := nsBullet;
  AddFormatText(reCredits,SAboutBoxCreditsDonations + SLineBreak);
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

