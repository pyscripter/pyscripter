{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: DPageSetup.pas, released 2000-06-01.

The Original Code is part of the TestPP project, written by
Morten J. Skovrup for the SynEdit component suite.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: DPageSetup.pas,v 1.2 2000/11/22 08:37:05 mghie Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit dlgSynPageSetup;

{$I SynEdit.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, ComCtrls, ToolWin, ImgList, ActnList, Dialogs,
  SynEditPrintTypes, SynEditPrint, SynEditPrintMargins,
  SynEditPrintHeaderFooter, SpTBXControls, SpTBXDkPanels, TntActnList, TB2Item,
  SpTBXItem, TB2Dock, TB2Toolbar, TntStdCtrls, SpTBXEditors, 
  TntComCtrls, dlgPyIDEBase;

type
  TPageSetupDlg = class(TPyIDEDlgBase)
    PageControl: TPageControl;
    Margins: TTabSheet;
    HeaderFooter: TTabSheet;
    Image1: TImage;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    PBHeaderLine: TPaintBox;
    PBHeaderShadow: TPaintBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    PBFooterLine: TPaintBox;
    PBFooterShadow: TPaintBox;
    ImageList: TImageList;
    FontDialog: TFontDialog;
    ColorDialog: TColorDialog;
    HeaderLineColorBtn: TSpTBXButton;
    HeaderShadowColorBtn: TSpTBXButton;
    FooterLineColorBtn: TSpTBXButton;
    FooterShadowColorBtn: TSpTBXButton;
    OKBtn: TSpTBXButton;
    CancelBtn: TSpTBXButton;
    CBMirrorMargins: TSpTBXCheckBox;
    CBLineNumbers: TSpTBXCheckBox;
    CBLineNumbersInMargin: TSpTBXCheckBox;
    CBHighlight: TSpTBXCheckBox;
    CBColors: TSpTBXCheckBox;
    CBWrap: TSpTBXCheckBox;
    CBHeaderMirror: TSpTBXCheckBox;
    CBHeaderLine: TSpTBXCheckBox;
    CBHeaderBox: TSpTBXCheckBox;
    CBHeaderShadow: TSpTBXCheckBox;
    CBFooterMirror: TSpTBXCheckBox;
    CBFooterLine: TSpTBXCheckBox;
    CBFooterBox: TSpTBXCheckBox;
    CBFooterShadow: TSpTBXCheckBox;
    ActionList1: TTntActionList;
    UnderlineCmd: TTntAction;
    ItalicCmd: TTntAction;
    BoldCmd: TTntAction;
    FontCmd: TTntAction;
    TitleCmd: TTntAction;
    DateCmd: TTntAction;
    TimeCmd: TTntAction;
    PagesCmd: TTntAction;
    PageNumCmd: TTntAction;
    Label1: TSpTBXLabel;
    Label2: TSpTBXLabel;
    Label3: TSpTBXLabel;
    Label4: TSpTBXLabel;
    Label5: TSpTBXLabel;
    Label6: TSpTBXLabel;
    Label7: TSpTBXLabel;
    Label8: TSpTBXLabel;
    Label9: TSpTBXLabel;
    Label10: TSpTBXLabel;
    Label11: TSpTBXLabel;
    Label12: TSpTBXLabel;
    Label13: TSpTBXLabel;
    Label14: TSpTBXLabel;
    Label15: TSpTBXLabel;
    Label16: TSpTBXLabel;
    Label17: TSpTBXLabel;
    ToolbarDock: TSpTBXDock;
    Toolbar: TSpTBXToolbar;
    tbiUdnerline: TSpTBXItem;
    tbiItalic: TSpTBXItem;
    tbiBold: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    tbiTitle: TSpTBXItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    tbiFont: TSpTBXItem;
    tbiDate: TSpTBXItem;
    tbiTime: TSpTBXItem;
    tbiPgNumber: TSpTBXItem;
    tbiPages: TSpTBXItem;
    EditLeft: TSpTBXEdit;
    EditRight: TSpTBXEdit;
    EditTop: TSpTBXEdit;
    EditBottom: TSpTBXEdit;
    EditGutter: TSpTBXEdit;
    EditHeader: TSpTBXEdit;
    EditFooter: TSpTBXEdit;
    EditHFInternalMargin: TSpTBXEdit;
    EditLeftHFTextIndent: TSpTBXEdit;
    EditRightHFTextIndent: TSpTBXEdit;
    CBUnits: TSpTBXComboBox;
    REHeaderLeft: TTntRichEdit;
    REHeaderCenter: TTntRichEdit;
    REHeaderRight: TTntRichEdit;
    REFooterLeft: TTntRichEdit;
    REFooterCenter: TTntRichEdit;
    REFooterRight: TTntRichEdit;
    procedure PageNumCmdExecute(Sender: TObject);
    procedure PagesCmdExecute(Sender: TObject);
    procedure TimeCmdExecute(Sender: TObject);
    procedure DateCmdExecute(Sender: TObject);
    procedure FontCmdExecute(Sender: TObject);
    procedure BoldCmdExecute(Sender: TObject);
    procedure ItalicCmdExecute(Sender: TObject);
    procedure UnderlineCmdExecute(Sender: TObject);
    procedure REHeaderLeftEnter(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CBFooterLineEnter(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure HeaderLineColorBtnClick(Sender: TObject);
    procedure PBHeaderLinePaint(Sender: TObject);
    procedure HeaderShadowColorBtnClick(Sender: TObject);
    procedure FooterLineColorBtnClick(Sender: TObject);
    procedure FooterShadowColorBtnClick(Sender: TObject);
    procedure REHeaderLeftSelectionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CBUnitsChange(Sender: TObject);
    procedure TitleCmdExecute(Sender: TObject);
  private
    { Private declarations }
    Editor: TCustomRichEdit;
    CharPos: TPoint;
    OldStart: Integer;
    FMargins: TSynEditPrintMargins;
    FInternalCall: Boolean;
    procedure SetOptions;
    procedure UpdateCursorPos;
    procedure SelectLine(LineNum: Integer);
    function CurrText: TTextAttributes;
    procedure SetMargins(SynEditMargins: TSynEditPrintMargins);
    procedure GetMargins(SynEditMargins: TSynEditPrintMargins);
    procedure AddLines(HeadFoot: THeaderFooter; AEdit: TCustomRichEdit;
      Al: TALignment);
    procedure SelectNone;
  public
    { Public declarations }
    procedure SetValues(SynEditPrint: TSynEditPrint);
    procedure GetValues(SynEditPrint: TSynEditPrint);
  end;

var
  PageSetupDlg: TPageSetupDlg;

implementation

uses
  RichEdit, ShellAPI, Messages, TntDialogs, gnugettext, StringResources;

{$R *.DFM}

procedure TPageSetupDlg.FormCreate(Sender: TObject);
begin
  inherited;
  FMargins := TSynEditPrintMargins.Create;
  FInternalCall := False;
end;

procedure TPageSetupDlg.FormDestroy(Sender: TObject);
begin
  FMargins.Free;
end;

procedure TPageSetupDlg.FormShow(Sender: TObject);
begin
  Editor := REHeaderLeft;
  PageControl.ActivePage := Margins;
  SetOptions;
  UpdateCursorPos;
end;

procedure TPageSetupDlg.SetOptions;
begin
  PageNumCmd.Enabled := Editor.Focused;
  PagesCmd.Enabled := Editor.Focused;
  TimeCmd.Enabled := Editor.Focused;
  DateCmd.Enabled := Editor.Focused;
  TitleCmd.Enabled := Editor.Focused;
  FontCmd.Enabled := Editor.Focused;
  BoldCmd.Enabled := Editor.Focused;
  ItalicCmd.Enabled := Editor.Focused;
  UnderlineCmd.Enabled := Editor.Focused;
end;

procedure TPageSetupDlg.REHeaderLeftEnter(Sender: TObject);
begin
  Editor := Sender as TCustomRichEdit;
  SetOptions;
end;

procedure TPageSetupDlg.CBFooterLineEnter(Sender: TObject);
begin
  SetOptions;
end;

procedure TPageSetupDlg.REHeaderLeftSelectionChange(Sender: TObject);
begin
  UpdateCursorPos;
end;

procedure TPageSetupDlg.UpdateCursorPos;
begin
  CharPos.Y := SendMessage(Editor.Handle, EM_EXLINEFROMCHAR, 0, Editor.SelStart);
  CharPos.X := (Editor.SelStart - SendMessage(Editor.Handle, EM_LINEINDEX, CharPos.Y, 0));
end;

procedure TPageSetupDlg.SelectLine(LineNum: Integer);
begin
  OldStart := Editor.SelStart;
  Editor.SelStart := SendMessage(Editor.Handle, EM_LINEINDEX, LineNum, 0);
  Editor.SelLength := Length(Editor.Lines[LineNum]);
end;

procedure TPageSetupDlg.SelectNone;
begin
  Editor.SelStart := OldStart;
  Editor.SelLength := 0;
end;

function TPageSetupDlg.CurrText: TTextAttributes;
begin
  Result := Editor.SelAttributes;
end;

procedure TPageSetupDlg.PageNumCmdExecute(Sender: TObject);
begin
  Editor.SelText := '$PAGENUM$';
end;

procedure TPageSetupDlg.PagesCmdExecute(Sender: TObject);
begin
  Editor.SelText := '$PAGECOUNT$';
end;

procedure TPageSetupDlg.TimeCmdExecute(Sender: TObject);
begin
  Editor.SelText := '$TIME$';
end;

procedure TPageSetupDlg.DateCmdExecute(Sender: TObject);
begin
  Editor.SelText := '$DATE$';
end;

procedure TPageSetupDlg.TitleCmdExecute(Sender: TObject);
begin
  Editor.SelText := '$TITLE$';
end;

procedure TPageSetupDlg.FontCmdExecute(Sender: TObject);
begin
  SelectLine(CharPos.y);
  FontDialog.Font.Assign(CurrText);
  if FontDialog.Execute then
    CurrText.Assign(FontDialog.Font);
  SelectNone;
end;

procedure TPageSetupDlg.BoldCmdExecute(Sender: TObject);
begin
  SelectLine(CharPos.y);
  if fsBold in CurrText.Style then
    CurrText.Style := CurrText.Style - [fsBold]
  else
    CurrText.Style := CurrText.Style + [fsBold];
  SelectNone;
end;

procedure TPageSetupDlg.ItalicCmdExecute(Sender: TObject);
begin
  SelectLine(CharPos.y);
  if fsItalic in CurrText.Style then
    CurrText.Style := CurrText.Style - [fsItalic]
  else
    CurrText.Style := CurrText.Style + [fsItalic];
  SelectNone;
end;

procedure TPageSetupDlg.UnderlineCmdExecute(Sender: TObject);
begin
  SelectLine(CharPos.y);
  if fsUnderLine in CurrText.Style then
    CurrText.Style := CurrText.Style - [fsUnderLine]
  else
    CurrText.Style := CurrText.Style + [fsUnderLine];
  SelectNone;
end;

procedure TPageSetupDlg.PageControlChange(Sender: TObject);
begin
  if PageControl.ActivePage = HeaderFooter then
    SetOptions;
end;

procedure TPageSetupDlg.PBHeaderLinePaint(Sender: TObject);
begin
  with (Sender as TPaintBox).Canvas do begin
    Brush.Color := (Sender as TPaintBox).Color;
    FillRect((Sender as TPaintBox).ClientRect);
    Pen.Style := psDot;
    Brush.Style := bsClear;
    Rectangle(0, 0, (Sender as TPaintBox).Width, (Sender as TPaintBox).Height);
  end;
end;

procedure TPageSetupDlg.HeaderLineColorBtnClick(Sender: TObject);
begin
  ColorDialog.Color := PBHeaderLine.Color;
  if ColorDialog.Execute then
    PBHeaderLine.Color := ColorDialog.Color;
end;

procedure TPageSetupDlg.HeaderShadowColorBtnClick(Sender: TObject);
begin
  ColorDialog.Color := PBHeaderShadow.Color;
  if ColorDialog.Execute then
    PBHeaderShadow.Color := ColorDialog.Color;
end;

procedure TPageSetupDlg.FooterLineColorBtnClick(Sender: TObject);
begin
  ColorDialog.Color := PBFooterLine.Color;
  if ColorDialog.Execute then
    PBFooterLine.Color := ColorDialog.Color;
end;

procedure TPageSetupDlg.FooterShadowColorBtnClick(Sender: TObject);
begin
  ColorDialog.Color := PBFooterShadow.Color;
  if ColorDialog.Execute then
    PBFooterShadow.Color := ColorDialog.Color;
end;

procedure TPageSetupDlg.GetMargins(SynEditMargins: TSynEditPrintMargins);
var
  CurEdit: TEdit;
  function StringToFloat(Edit: TEdit): Double;
  begin
    CurEdit := Edit;
    Result := StrToFloat(Edit.Text);
  end;
begin
  with SynEditMargins do begin
    if not FInternalCall then
      UnitSystem := TUnitSystem(CBUnits.ItemIndex);
    try
      Left := StringToFloat(EditLeft);
      Right := StringToFloat(EditRight);
      Top := StringToFloat(EditTop);
      Bottom := StringToFloat(EditBottom);
      Gutter := StringToFloat(EditGutter);
      Header := StringToFloat(EditHeader);
      Footer := StringToFloat(EditFooter);
      LeftHFTextIndent := StringToFloat(EditLeftHFTextIndent);
      RightHFTextIndent := StringToFloat(EditRightHFTextIndent);
      HFInternalMargin := StringToFloat(EditHFInternalMargin);
    except
      WideMessageDlg(_(SInvalidNumber), mtError, [mbOk], 0);
      CurEdit.SetFocus;
    end;
    MirrorMargins := CBMirrorMargins.Checked;
  end;
end;

procedure TPageSetupDlg.SetMargins(SynEditMargins: TSynEditPrintMargins);
begin
  with SynEditMargins do begin
    CBUnits.ItemIndex := Ord(UnitSystem);
    EditLeft.Text := FloatToStr(Left);
    EditRight.Text := FloatToStr(Right);
    EditTop.Text := FloatToStr(Top);
    EditBottom.Text := FloatToStr(Bottom);
    EditGutter.Text := FloatToStr(Gutter);
    EditHeader.Text := FloatToStr(Header);
    EditFooter.Text := FloatToStr(Footer);
    EditLeftHFTextIndent.Text := FloatToStr(LeftHFTextIndent);
    EditRightHFTextIndent.Text := FloatToStr(RightHFTextIndent);
    EditHFInternalMargin.Text := FloatToStr(HFInternalMargin);
    CBMirrorMargins.Checked := MirrorMargins;
  end;
end;

procedure TPageSetupDlg.CBUnitsChange(Sender: TObject);
begin
  FInternalCall := True;
  GetMargins(FMargins);
  FInternalCall := False;
  FMargins.UnitSystem := TUnitSystem(CBUnits.ItemIndex);
  SetMargins(FMargins);
end;

procedure TPageSetupDlg.AddLines(HeadFoot: THeaderFooter; AEdit: TCustomRichEdit;
  Al: TALignment);
var
  i: Integer;
  AFont: TFont;
begin
  Editor := AEdit;
  AFont := TFont.Create;
  for i := 0 to Editor.Lines.Count - 1 do begin
    SelectLine(i);
    AFont.Assign(CurrText);
    HeadFoot.Add(Editor.Lines[i], AFont, Al, i + 1);
  end;
  AFont.Free;
end;

procedure TPageSetupDlg.GetValues(SynEditPrint: TSynEditPrint);
begin
  GetMargins(SynEditPrint.Margins);
  SynEditPrint.LineNumbers := CBLineNumbers.Checked;
  SynEditPrint.LineNumbersInMargin := CBLineNumbersInMargin.Checked;
  SynEditPrint.Highlight := CBHighlight.Checked;
  SynEditPrint.Colors := CBColors.Checked;
  SynEditPrint.Wrap := CBWrap.Checked;

  SynEditPrint.Header.FrameTypes := [];
  if CBHeaderLine.Checked then
    SynEditPrint.Header.FrameTypes := SynEditPrint.Header.FrameTypes + [ftLine];
  if CBHeaderBox.Checked then
    SynEditPrint.Header.FrameTypes := SynEditPrint.Header.FrameTypes + [ftBox];
  if CBHeaderShadow.Checked then
    SynEditPrint.Header.FrameTypes := SynEditPrint.Header.FrameTypes + [ftShaded];
  SynEditPrint.Header.LineColor := PBHeaderLine.Color;
  SynEditPrint.Header.ShadedColor := PBHeaderShadow.Color;
  SynEditPrint.Header.MirrorPosition := CBHeaderMirror.Checked;

  SynEditPrint.Footer.FrameTypes := [];
  if CBFooterLine.Checked then
    SynEditPrint.Footer.FrameTypes := SynEditPrint.Footer.FrameTypes + [ftLine];
  if CBFooterBox.Checked then
    SynEditPrint.Footer.FrameTypes := SynEditPrint.Footer.FrameTypes + [ftBox];
  if CBFooterShadow.Checked then
    SynEditPrint.Footer.FrameTypes := SynEditPrint.Footer.FrameTypes + [ftShaded];
  SynEditPrint.Footer.LineColor := PBFooterLine.Color;
  SynEditPrint.Footer.ShadedColor := PBFooterShadow.Color;
  SynEditPrint.Footer.MirrorPosition := CBFooterMirror.Checked;

  SynEditPrint.Header.Clear;
  AddLines(SynEditPrint.Header, REHeaderLeft, taLeftJustify);
  AddLines(SynEditPrint.Header, REHeaderCenter, taCenter);
  AddLines(SynEditPrint.Header, REHeaderRight, taRightJustify);

  SynEditPrint.Footer.Clear;
  AddLines(SynEditPrint.Footer, REFooterLeft, taLeftJustify);
  AddLines(SynEditPrint.Footer, REFooterCenter, taCenter);
  AddLines(SynEditPrint.Footer, REFooterRight, taRightJustify);
end;

procedure TPageSetupDlg.SetValues(SynEditPrint: TSynEditPrint);
var
  i: Integer;
  AItem: THeaderFooterItem;
  LNum: Integer;
begin
  REHeaderLeft.Lines.Clear;
  REHeaderCenter.Lines.Clear;
  REHeaderRight.Lines.Clear;
  REFooterLeft.Lines.Clear;
  REFooterCenter.Lines.Clear;
  REFooterRight.Lines.Clear;
  SetMargins(SynEditPrint.Margins);
  CBLineNumbers.Checked := SynEditPrint.LineNumbers;
  CBLineNumbersInMargin.Checked := SynEditPrint.LineNumbersInMargin;
  CBHighlight.Checked := SynEditPrint.Highlight;
  CBColors.Checked := SynEditPrint.Colors;
  CBWrap.Checked := SynEditPrint.Wrap;

  REHeaderLeft.Font := SynEditPrint.Header.DefaultFont;
  REHeaderCenter.Font := SynEditPrint.Header.DefaultFont;
  REHeaderRight.Font := SynEditPrint.Header.DefaultFont;
  REFooterLeft.Font := SynEditPrint.Footer.DefaultFont;
  REFooterCenter.Font := SynEditPrint.Footer.DefaultFont;
  REFooterRight.Font := SynEditPrint.Footer.DefaultFont;

  CBHeaderLine.Checked := ftLine in SynEditPrint.Header.FrameTypes;
  CBHeaderBox.Checked := ftBox in SynEditPrint.Header.FrameTypes;
  CBHeaderShadow.Checked := ftShaded in SynEditPrint.Header.FrameTypes;
  PBHeaderLine.Color := SynEditPrint.Header.LineColor;
  PBHeaderShadow.Color := SynEditPrint.Header.ShadedColor;
  CBHeaderMirror.Checked := SynEditPrint.Header.MirrorPosition;

  CBFooterLine.Checked := ftLine in SynEditPrint.Footer.FrameTypes;
  CBFooterBox.Checked := ftBox in SynEditPrint.Footer.FrameTypes;
  CBFooterShadow.Checked := ftShaded in SynEditPrint.Footer.FrameTypes;
  PBFooterLine.Color := SynEditPrint.Footer.LineColor;
  PBFooterShadow.Color := SynEditPrint.Footer.ShadedColor;
  CBFooterMirror.Checked := SynEditPrint.Footer.MirrorPosition;

  SynEditPrint.Header.FixLines;
  for i := 0 to SynEditPrint.Header.Count - 1 do begin
    AItem := SynEditPrint.Header.Get(i);
    case AItem.Alignment of
      taLeftJustify: Editor := REHeaderLeft;
      taCenter: Editor := REHeaderCenter;
      taRightJustify: Editor := REHeaderRight;
    end;
    LNum := Editor.Lines.Add(AItem.Text);
    SelectLine(LNum);
    CurrText.Assign(AItem.Font);
    SelectNone;
  end;

  SynEditPrint.Footer.FixLines;
  for i := 0 to SynEditPrint.Footer.Count - 1 do begin
    AItem := SynEditPrint.Footer.Get(i);
    case AItem.Alignment of
      taLeftJustify: Editor := REFooterLeft;
      taCenter: Editor := REFooterCenter;
      taRightJustify: Editor := REFooterRight;
    end;
    LNum := Editor.Lines.Add(AItem.Text);
    SelectLine(LNum);
    CurrText.Assign(AItem.Font);
    SelectNone;
  end;
end;

end.

