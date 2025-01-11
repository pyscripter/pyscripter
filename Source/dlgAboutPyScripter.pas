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
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  dlgPyIDEBase,
  SpTBXItem,
  SpTBXTabs,
  TB2Item,
  SpTBXPageScroller,
  SVGIconImage;

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
  end;

var
  AboutBox: TAboutBox;

implementation

uses
  Winapi.ShellAPI,
  Winapi.RichEdit,
  System.SysUtils,
  Vcl.Graphics,
  Vcl.Forms,
  uCommonFunctions,
  JvGnugettext;

{$R *.dfm}
const
  SAboutBoxCredits =
    'Python for Delphi (www.github.com/pyscripter/python4delphi)'+ sLineBreak +
    'Rpyc (www.github.com/tomerfiliba/rpyc)'+ sLineBreak +
    'JVCL (www.github.com/project-jedi/jvcl)'+ sLineBreak +
    'SynEdit (www.sourceforge.net/projects/synedit)'+ sLineBreak +
    'VirtualTreeView (www.github.com/Virtual-TreeView/)'+ sLineBreak +
    'VirtualShellTools (www.github.com/pyscripter/mustangpeakvirtualshelltools)'+ sLineBreak +
    'GExperts (www.gexperts.org)'+ sLineBreak +
    'Syn Editor (www.sourceforge.net/projects/syn)'+ sLineBreak +
    'Syn Web highlighters (www.github.com/KrystianBigaj/synweb)' + sLineBreak +
    'Toolbar2000 (www.jrsoftware.org/tb2k.php)'+ sLineBreak +
    'SpTBXLib (www.silverpointdevelopment.com)' + sLineBreak +
    'SVGIconImageList (https://github.com/EtheaDev/SVGIconImageList)' + sLineBreak +
    'zControls/Detours library (https://github.com/MahdiSafsafi)' + sLineBreak +
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
    'Donations from numerous users have provided a strong incentive to develop this project.' + sLineBreak +
    'Financial and moral support from Embarcadero (https://www.embarcadero.com/) makers of Delphi, ' +
    'the programming environment with which PyScripter is built, and generous support '+
    'from Tranquil IT (https://www.tranquil.it) makers of WAPT (apt-get for Windows) '+
    '(https://www.wapt.fr) is thankfully acknowledged.';

  SAboutBoxLinks =
    'The project home, Issue Tracker and source code repository are hosted at Github (www.github.com/pyscripter/pyscripter)'+
  sLineBreak+
    'Internet group support is available at https://groups.google.com/group/PyScripter'+
  sLineBreak +
    'Please submit bug reports and questions about PyScripter to pyscripter@gmail.com.';

const
  AURL_ENABLEURL = 1;
  AURL_ENABLEEAURLS = 8;

type
  // in alphabetical order of the full English languages names!!
  // not in order of the gettext abbreviations.
  ECreditLanguages = (ar,zh,fr,de,el,it,ja,kab,pt_BR,pt_PT,ru,sk,es);

const
  cAboutLanguages: array[ECreditLanguages] of string = (
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
  cAboutTranslators: array[ECreditLanguages] of string = (
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
    'Adriana Díaz - Aumenta Software (https://aumenta.mx/)' + sLineBreak +
    'Salim Saddaquzzaman (https://github.com/sk-Prime)'+ sLineBreak +
    'Tanmaya Meher (www.github.com/tanmayameher)'+ sLineBreak +
    'jprzywoski (www.github.com/jprzywoski)';

procedure TRichEdit.CreateWnd;
var
  Μask: LRESULT;
begin
  inherited;
  Μask := SendMessage(Handle, EM_GETEVENTMASK, 0, 0);
  SendMessage(Handle, EM_SETEVENTMASK, 0, Μask or ENM_LINK);
  SendMessage(Handle, EM_AUTOURLDETECT, AURL_ENABLEURL, 0);
end;

procedure TRichEdit.CNNotify(var Message: TWMNotify);
type
  PENLink = ^TENLink;
var
  PenLnk: PENLink;
  TR: TEXTRANGE;
  Url: array of Char;
begin
  if (Message.NMHdr.code = EN_LINK) then begin
    PenLnk := PENLink(Message.NMHdr);
    if (PenLnk.Msg = WM_LBUTTONDOWN) then begin
      { optionally, enable this:
      if CheckWin32Version(6, 2) then begin
        // on Windows 8+, returning EN_LINK_DO_DEFAULT directs
        // the RichEdit to perform the default action...
        Message.Result :=  EN_LINK_DO_DEFAULT;
        Exit;
      end;
      }
      try
        SetLength(Url, PenLnk.chrg.cpMax - PenLnk.chrg.cpMin + 1);
        TR.chrg := PenLnk.chrg;
        TR.lpstrText := PChar(Url);
        SendMessage(Handle, EM_GETTEXTRANGE, 0, LPARAM(@TR));
        ShellExecute(Handle, nil, PChar(Url), nil, nil, SW_SHOWNORMAL);
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
var
  Winplatform: string;
  AboutBoxCreditsTranslations: string;
  Language: ECreditLanguages;
begin
  {$IFDEF WIN64}
  Winplatform := 'x64';
  {$ELSE}
  Winplatform := 'x86';
  {$ENDIF}
  inherited;
  Copyright.Caption := #$00A9' Kiriakos Vlahos 2005-' + CurrentYear.ToString;
  Version.Caption := Format('Version %s %s', [ApplicationVersion, Winplatform]);

  AddFormatText(reLinks, _('Links') + sLineBreak, [fsBold]);
  reLinks.Paragraph.Numbering := nsBullet;
  AddFormatText(reLinks,SAboutBoxLinks);
  reLinks.ReadOnly := True;

  AddFormatText(reCredits, _('Credits') + sLineBreak, [fsBold]);
  AddFormatText(reCredits,SAboutBoxCreditsIntro + sLineBreak);
  reCredits.Paragraph.Numbering := nsBullet;
  AddFormatText(reCredits,SAboutBoxCredits + sLineBreak);

  reCredits.Paragraph.Numbering := nsNone;
  AddFormatText(reCredits, sLineBreak + _('Translation manager') + ':' + sLineBreak, [fsItalic]);
  reCredits.Paragraph.Numbering := nsBullet;
  AddFormatText(reCredits,cAboutTranslationManager + sLineBreak);

  reCredits.Paragraph.Numbering := nsNone;
  AddFormatText(reCredits, sLineBreak + _('Translators') + ':' + sLineBreak, [fsItalic]);

  for Language := Low(ECreditLanguages) to High(ECreditLanguages) do
    AboutBoxCreditsTranslations := AboutBoxCreditsTranslations +
      Format(cAboutLanguages[Language],[cAboutTranslators[Language]]) + sLineBreak;
  reCredits.Paragraph.Numbering := nsBullet;
  AddFormatText(reCredits,AboutBoxCreditsTranslations);

  reCredits.Paragraph.Numbering := nsNone;
  AddFormatText(reCredits, sLineBreak + _('Artwork and theme design') + ':' + sLineBreak, [fsItalic]);
  reCredits.Paragraph.Numbering := nsBullet;
  AddFormatText(reCredits, cAboutBoxCreditsThemeDesign + sLineBreak);
  reCredits.Paragraph.Numbering := nsNone;

  reCredits.Paragraph.Numbering := nsNone;
  AddFormatText(reCredits, sLineBreak + _('Donations') + ':' + sLineBreak, [fsItalic]);
  reCredits.Paragraph.Numbering := nsBullet;
  AddFormatText(reCredits,SAboutBoxCreditsDonations + sLineBreak);
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

