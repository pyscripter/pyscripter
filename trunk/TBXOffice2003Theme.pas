unit TBXOffice2003Theme;

// TBX Package
// Copyright 2001-2004 Alex A. Denisov. All Rights Reserved
// See TBX.chm for license and installation instructions
//
// Office2003 theme
// Copyright (c) Yury Plashenkov (feb 2005)
// mailto:plashenkov@mail.ru
//
// Version for TBX version 2.1

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
(*
*  2006-08-23
*  The Office 2003 theme.
*
*  Fully rewrited by Vladimir Bochkarev <boxa@mail.ru>
*  Have a lot changes, improvements and bug fixes.
*
*  For correct work required patched TBX 2.1.beta (fourth edition patch).
*
*  You can change a kind and behaviour of this theme, using defines below.
*
*  You also can set style used by default in run-time using
*  "TBXOffice2003Theme_Default_Color_Scheme" global variable.
*  For example: TBXOffice2003Theme_Default_Color_Scheme := osMetallic;
*)
{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}

interface

// TPNGImage (written by Gustavo Daud) allows you to work with semitransparent PNG-images
// I advise you to get it from http://pngdelphi.sourceforge.net
// after downloading, install it and TPNGImageList component from PNGImgList.pas
// uncomment next string if you have TPNGImage and TPNGImageList installed
{x$DEFINE PNGIMAGELIST}

{ Comment next string if you not want to see highlighted icons }
{$DEFINE HIGHLIGHT_TOOLBAR_ICONS}

{ Miscellaneous }
{.$DEFINE AUTO_ADAPT_TO_MS_LUNA_SCHEME}
{$DEFINE XP_THEMED_CHECKBOX_AND_RADIOBUTTON}

{ Popup shadow }
{$DEFINE USE_XP_POPUP_SHADOW_WHEN_POSSIBLE}

{ Close buttons }
{$DEFINE SMALL_CLOSE_BUTTON_ON_DOCKED_TOOLBAR}
{$DEFINE SMALL_CLOSE_BUTTON_ON_DOCKED_DOCKPANEL}
{$DEFINE SMALL_CLOSE_BUTTON_ON_FLOATING_XXX}

{ Gradients }
{$DEFINE GRADIENT_FILL_BACKGROUND}
{$DEFINE GRADIENT_FILL_STATUSBAR}
{$DEFINE VERTICAL_GRADIENT_FILL_FLOATING_TOOLBAR}

{$I TB2Ver.inc}
{$I TBX.inc}

uses
  Windows, Messages, Graphics, ImgList, TBXThemes
  {$IFDEF PNGIMAGELIST}, PNGImgList{$ENDIF};

type
  TItemPart = (ipBody, ipText, ipFrame);
  TBtnItemState = (bisNormal, bisDisabled, bisSelected, bisPressed, bisHot,
    bisDisabledHot, bisSelectedHot, bisPopupParent);
  TMenuItemState = (misNormal, misDisabled, misHot, misDisabledHot);
  TWinFramePart = (wfpBorder, wfpCaption, wfpCaptionText);
  TOffice2003Scheme = (osBlue, osMetallic, osGreen, osAuto);

  TTBXOffice2003Theme = class(TTBXTheme)
  private
    FMaxWindowWidth: Integer;
    procedure TBXSysCommand(var Message: TMessage); message TBX_SYSCOMMAND;
  protected
    DockColor1: TColor;
    DockColor2: TColor;
    ToolbarColor1: TColor;
    ToolbarColor2: TColor;
    ToolbarFrameColor1: TColor;
    ToolbarFrameColor2: TColor;
    SeparatorColor1: TColor;
    SeparatorColor2: TColor;
    DragHandleColor1: TColor;
    DragHandleColor2: TColor;

    EmbeddedColor: TColor;
    EmbeddedFrameColor: TColor;
    EmbeddedDisabledColor: TColor;

    PopupColor: TColor;
    PopupFrameColor: TColor;

    DockPanelColor: TColor;

    WinFrameColors: array[TWinFramePart] of TColor;
    MenuItemColors: array[TMenuItemState, TItemPart] of TColor;
    BtnItemColors: array[TBtnItemState, ipText..ipFrame] of TColor;
    BtnBodyColors: array[TBtnItemState, Boolean] of TColor;

    StatusPanelFrameColor: TColor;
    FMDIAreaColor: TColor;
    FFaceColor: TColor;
    FWindowColor: TColor;

    procedure DrawCloseButton(DC: HDC; R: TRect; BtnItemState: TBtnItemState;
      Gradient, Small: Boolean; Color: TColor); virtual;
    function  GetBtnColor(const ItemInfo: TTBXItemInfo; ItemPart: TItemPart; GradColor2: Boolean = False): TColor;
    procedure GetGradientColors(X1, X2: Integer; out Color1, Color2: TColor);
    function  GetPartColor(const ItemInfo: TTBXItemInfo; ItemPart: TItemPart): TColor;
    procedure SetupColorCache; virtual;
  public
    constructor Create(const AName: String); override;
    destructor Destroy; override;

    function  GetBooleanMetrics(Index: Integer): Boolean; override;
    function  GetIntegerMetrics(Index: Integer): Integer; override;
    procedure GetMargins(MarginID: Integer; out Margins: TTBXMargins); override;
    function  GetImageOffset(Canvas: TCanvas; const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList): TPoint; override;
    function  GetItemColor(const ItemInfo: TTBXItemInfo): TColor; override;
    function  GetItemTextColor(const ItemInfo: TTBXItemInfo): TColor; override;
    function  GetItemImageBackground(const ItemInfo: TTBXItemInfo): TColor; override;
    function  GetPopupShadowType: Integer; override;
    procedure GetViewBorder(ViewType: Integer; out Border: TPoint); override;
    function  GetViewColor(AViewType: Integer): TColor; override;
    procedure GetViewMargins(ViewType: Integer; out Margins: TTBXMargins); override;

    procedure PaintBackgnd(Canvas: TCanvas; const ADockRect, ARect, AClipRect: TRect; AColor: TColor; Transparent: Boolean; AViewType: Integer); override;
    procedure PaintButton(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintCaption(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo; const ACaption: string; AFormat: Cardinal; Rotated: Boolean); override;
    procedure PaintCheckMark(Canvas: TCanvas; ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintChevron(Canvas: TCanvas; ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintDock(Canvas: TCanvas; const ClientRect, DockRect: TRect; DockPosition: Integer); override;
    procedure PaintDockPanelNCArea(Canvas: TCanvas; R: TRect; const DockPanelInfo: TTBXDockPanelInfo); override;
    procedure PaintDropDownArrow(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintEditButton(Canvas: TCanvas; const ARect: TRect; var ItemInfo: TTBXItemInfo; ButtonInfo: TTBXEditBtnInfo); override;
    procedure PaintEditFrame(Canvas: TCanvas; const ARect: TRect; var ItemInfo: TTBXItemInfo; const EditInfo: TTBXEditInfo); override;
    procedure PaintFloatingBorder(Canvas: TCanvas; const ARect: TRect; const WindowInfo: TTBXWindowInfo); override;
    procedure PaintFrame(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintFrameControl(Canvas: TCanvas; R: TRect; Kind, State: Integer; Params: Pointer); override;
    procedure PaintImage(Canvas: TCanvas; ARect: TRect; const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList; ImageIndex: Integer); override;
    procedure PaintMDIButton(Canvas: TCanvas; ARect: TRect; const ItemInfo: TTBXItemInfo; ButtonKind: Cardinal); override;
    procedure PaintMenuItem(Canvas: TCanvas; const ARect: TRect; var ItemInfo: TTBXItemInfo); override;
    procedure PaintMenuItemFrame(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintPageScrollButton(Canvas: TCanvas; const ARect: TRect; ButtonType: Integer; Hot: Boolean); override;
    procedure PaintPopupNCArea(Canvas: TCanvas; R: TRect; const PopupInfo: TTBXPopupInfo); override;
    procedure PaintSeparator(Canvas: TCanvas; ARect: TRect; ItemInfo: TTBXItemInfo; Horizontal, LineSeparator: Boolean); override;
    procedure PaintToolbarNCArea(Canvas: TCanvas; R: TRect; const ToolbarInfo: TTBXToolbarInfo); override;
    procedure PaintStatusBar(Canvas: TCanvas; R: TRect; Part: Integer); override;

    property MDIAreaColor: TColor read FMDIAreaColor;
  end;

var
  TBXOffice2003Theme_Default_Color_Scheme: TOffice2003Scheme = osBlue;

function  GetOffice2003Scheme: TOffice2003Scheme;
function  GetMDIWorkspaceColor: TColor;
procedure PaintGradient(DC: HDC; const ARect: TRect; Color1, Color2: TColor; Horz: Boolean);
procedure PaintIrregularGradient(DC: HDC; const ARect: TRect; Color1, Color2: TColor; Horz: Boolean);

implementation

uses Controls, SysUtils, CommCtrl, Forms, Types,
     TB2Common, TB2Item, TBXUxThemes, TBXUtils;

(******************************************************************************)
{ Misc. }
(******************************************************************************)

//{$IFDEF AUTO_ADAPT_TO_MS_LUNA_SCHEME}
function GetOffice2003Scheme: TOffice2003Scheme;
var
  ThemeFileName: array[0..MAX_PATH] of WideChar;
  ColorSchemeName: array[0..15] of WideChar;
  ColorSchemeNameA: String;
begin
  if (TBXOffice2003Theme_Default_Color_Scheme = osAuto) and
    USE_THEMES and (GetCurrentThemeName(ThemeFileName, High(ThemeFileName),
    ColorSchemeName, High(ColorSchemeName), nil, 0) = S_OK) and
      SameText(ExtractFileName(ThemeFileName), 'Luna.MSStyles') then
  begin
    ColorSchemeNameA := ColorSchemeName;
    if SameText(ColorSchemeNameA, 'NormalColor') then
      Result := osBlue
    else if SameText(ColorSchemeNameA, 'Metallic') then
      Result := osMetallic
    else if SameText(ColorSchemeNameA, 'HomeStead') then
      Result := osGreen
    else Result := osAuto;
  end
  else Result := TBXOffice2003Theme_Default_Color_Scheme;
end;
//{$ELSE}
//function GetOffice2003Scheme: TOffice2003Scheme;
//begin
//  Result := TBXOffice2003Theme_Default_Color_Scheme;
//end;
//{$ENDIF}

function GetMDIWorkspaceColor: TColor;
const
  MDIColors: array[TOffice2003Scheme] of TColor =
    ($AE9990, $B39A9B, $7BA097, clBtnShadow);
begin
  Result := MDIColors[GetOffice2003Scheme];
end;

procedure PaintGradient(DC: HDC; const ARect: TRect; Color1, Color2: TColor;
  Horz: Boolean);
begin
  GradFill(DC, ARect, Color1, Color2, TGradientKind(not Horz));
end;

var
  IrregularGradientCenterColor: TColor;

procedure PaintIrregularGradient(DC: HDC; const ARect: TRect;
  Color1, Color2: TColor; Horz: Boolean);
var
  CC: TColor;
  R: TRect;
begin
  if IsRectEmpty(ARect) then Exit;
  if not SameColors(Color1, Color2) then
  begin
    CC := IrregularGradientCenterColor;
    R := ARect;
    if Horz then
    begin
      R.Right := (R.Left + R.Right - 1) div 2;
      GradFill(DC, R, Color1, CC, gkHorz);

      R.Left := R.Right; R.Right := R.Left+ 1;
      FillRectEx(DC, R, CC);

      R.Left := R.Right; R.Right := ARect.Right;
      GradFill(DC, R, CC, Color2, gkHorz);
    end
    else begin
      R.Bottom := (R.Top + R.Bottom - 1) div 2;
      GradFill(DC, R, Color1, CC, gkVert);

      R.Top := R.Bottom; R.Bottom := R.Top+ 1;
      FillRectEx(DC, R, CC);

      R.Top := R.Bottom; R.Bottom := ARect.Bottom;
      GradFill(DC, R, CC, Color2, gkVert);
    end;
  end
  else FillRectEx(DC, ARect, Color1);
end;

procedure DrawSmCaptionText(DC: HDC; Text: PChar; const R: TRect;
  TextColor: TColor; Vertical: Boolean);
const
  DrawTextFlags = DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS or DT_NOPREFIX;
var
  OldFont: HFONT;
  OldBkMode: Integer;
  OldTextColor: TColorRef;
begin
  if TextColor < 0 then TextColor := GetSysColor(TextColor and $FF);
  OldFont := SelectObject(DC, SmCaptionFont.Handle);
  OldBkMode := SetBkMode(DC, TRANSPARENT);
  OldTextColor := SetTextColor(DC, TextColor);
  if not Vertical
    then DrawTextEx(DC, Text, -1, PRect(@R)^, DrawTextFlags, nil)
    else DrawRotatedText(DC, Text, R, DrawTextFlags);
  SetTextColor(DC, OldTextColor);
  SetBkMode(DC, OldBkMode);
  SelectObject(DC, OldFont);
end;

function GetCloseBtnItemState(BtnState: Integer): TBtnItemState;
begin
  if (BtnState and CDBS_PRESSED) <> 0
    then Result := bisPressed
    else if (BtnState and CDBS_HOT) <> 0
      then Result := bisHot
      else Result := bisNormal;
end;

var
  StockImgList: TCustomImageList;

function GetGlyphsImList: TCustomImageList;
begin
  Result := StockImgList;
  if not Assigned(Result) then
  begin
    Result := TImageList.Create(nil);
    Result.Handle := ImageList_LoadBitmap(HInstance, 'TBXGLYPHS', 16, 0, clWhite);
    StockImgList := Result;
  end;
end;

procedure FinalizeStock;
begin
  FreeAndNil(StockImgList);
end;

(******************************************************************************)
{ TTBXOffice2003Theme }
(******************************************************************************)

constructor TTBXOffice2003Theme.Create(const AName: String);
begin
  inherited;
  AddTBXSysChangeNotification(Self);
  SetupColorCache;
end;

destructor TTBXOffice2003Theme.Destroy;
begin
  RemoveTBXSysChangeNotification(Self);
  FinalizeStock;
  inherited;
end;

function TTBXOffice2003Theme.GetBooleanMetrics(Index: Integer): Boolean;
begin
  Result := Index in [
    TMB_OFFICEXPPOPUPALIGNMENT,
    TMB_EDITMENUFULLSELECT,
    TMB_PAINTDOCKBACKGROUND,
    {$IFNDEF GRADIENT_FILL_BACKGROUND}
    TMB_SOLIDTOOLBARNCAREA,
    {$ENDIF}
    TMB_SOLIDTOOLBARCLIENTAREA];
end;

function TTBXOffice2003Theme.GetIntegerMetrics(Index: Integer): Integer;
begin
  case Index of
    TMI_SPLITBTN_ARROWWIDTH:  Result := 12;

    TMI_DROPDOWN_ARROWWIDTH:  Result := 8;
    TMI_DROPDOWN_ARROWMARGIN: Result := 3;

    TMI_MENU_IMGTEXTSPACE:    Result := 5;
    TMI_MENU_LCAPTIONMARGIN:  Result := 4;
    TMI_MENU_RCAPTIONMARGIN:  Result := 3;
    TMI_MENU_SEPARATORSIZE:   Result := 3;
    TMI_MENU_MDI_DW:          Result := 2;
    TMI_MENU_MDI_DH:          Result := 2;

    TMI_TLBR_SEPARATORSIZE:   Result := 6;

    TMI_EDIT_FRAMEWIDTH:      Result := 1;
    TMI_EDIT_TEXTMARGINHORZ:  Result := 2;
    TMI_EDIT_TEXTMARGINVERT:  Result := 2;
    TMI_EDIT_BTNWIDTH:        Result := 14;
    TMI_EDIT_MENURIGHTINDENT: Result := 1;
  else
    Result := -1;
  end;
end;

procedure TTBXOffice2003Theme.GetMargins(MarginID: Integer; out Margins: TTBXMargins);
const
  CMargins: array[0..MID_STATUSPANE] of record L, R, T, B: Byte end = (
    (L:0;R:0;T:0;B:0),(L:2;R:2;T:2;B:2),(L:1;R:1;T:3;B:3),(L:1;R:1;T:1;B:3));
begin
  if not (MarginID in [MID_TOOLBARITEM,MID_MENUITEM,MID_STATUSPANE]) then
    MarginID := 0;
  with Margins, CMargins[MarginID] do
  begin
    LeftWidth    := L;
    RightWidth   := R;
    TopHeight    := T;
    BottomHeight := B;
  end;
end;

function TTBXOffice2003Theme.GetImageOffset(Canvas: TCanvas;
  const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList): TPoint;
begin
  SetPoint(Result, 0, 0);
end;

function TTBXOffice2003Theme.GetItemColor(const ItemInfo: TTBXItemInfo): TColor;
begin
  Result := GetPartColor(ItemInfo, ipBody);
  if Result = clNone then Result := GetViewColor(ItemInfo.ViewType);
end;

function TTBXOffice2003Theme.GetItemTextColor(const ItemInfo: TTBXItemInfo): TColor;
begin
  Result := GetPartColor(ItemInfo, ipText);
end;

function TTBXOffice2003Theme.GetItemImageBackground(const ItemInfo: TTBXItemInfo): TColor;
begin
  Result := GetBtnColor(ItemInfo, ipBody);
  if Result = clNone then Result := GetViewColor(ItemInfo.ViewType);
end;

function TTBXOffice2003Theme.GetPopupShadowType: Integer;
begin
  {$IFDEF USE_XP_POPUP_SHADOW_WHEN_POSSIBLE}
  if USE_THEMES then
    Result := PST_WINDOWSXP
  else
  {$ENDIF}
    Result := PST_OFFICEXP;
end;

procedure TTBXOffice2003Theme.GetViewBorder(ViewType: Integer; out Border: TPoint);
const
  XMetrics: array[Boolean] of Byte = (SM_CXDLGFRAME, SM_CXFRAME);
  YMetrics: array[Boolean] of Byte = (SM_CYDLGFRAME, SM_CYFRAME);
var
  Resizable: Boolean;
begin
  if (ViewType and (VT_TOOLBAR or VT_DOCKPANEL)) <> 0 then
  begin
    if (ViewType and TVT_FLOATING) <> 0 then
    begin
      Resizable := (ViewType and TVT_RESIZABLE) <> 0;
      Border.X := GetSystemMetrics(XMetrics[Resizable])- 1;
      Border.Y := GetSystemMetrics(YMetrics[Resizable])- 1;
    end
    else SetPoint(Border, 2, 2);
  end
  else if (ViewType and VT_POPUP) <> 0 then
  begin
    Border.X := Ord((ViewType and PVT_POPUPMENU) <> PVT_POPUPMENU)+ 1; {1, 2}
    Border.Y := 2;
  end
  else SetPoint(Border, 0, 0);
end;

function TTBXOffice2003Theme.GetViewColor(AViewType: Integer): TColor;
begin
  if (AViewType and VT_TOOLBAR) <> 0 then
  begin
    if (AViewType and TVT_MENUBAR) = TVT_MENUBAR
      then Result := DockColor1
      else Result := ToolbarColor1;
  end
  else if (AViewType and VT_POPUP) <> 0 then
  begin
    if (AViewType and PVT_LISTBOX) = PVT_LISTBOX
      then Result := FWindowColor
      else Result := PopupColor;
  end
  else if (AViewType and VT_DOCKPANEL) <> 0
    then Result := DockPanelColor
    else Result := DockColor1;
end;

procedure TTBXOffice2003Theme.GetViewMargins(ViewType: Integer;
  out Margins: TTBXMargins);
var
  I: Integer;
begin
  I := 0;
  with Margins do
  begin
    LeftWidth := I; RightWidth := I;
    TopHeight := I; BottomHeight := I;
  end;
end;

procedure TTBXOffice2003Theme.PaintBackgnd(Canvas: TCanvas;
  const ADockRect, ARect, AClipRect: TRect; AColor: TColor;
  Transparent: Boolean; AViewType: Integer);
var
  DC: HDC;
  R: TRect;
  IsToolbar, Horz: Boolean;
begin
  if Transparent or not IntersectRect(R, ARect, AClipRect) then Exit;
  DC := Canvas.Handle;
  IsToolbar := (AViewType and TVT_NORMALTOOLBAR) = TVT_NORMALTOOLBAR;
  if (IsToolbar or ((AViewType and TVT_TOOLWINDOW) = TVT_TOOLWINDOW)) and
    ((AViewType and TVT_EMBEDDED) = 0) then
  begin
    {$IFDEF VERTICAL_GRADIENT_FILL_FLOATING_TOOLBAR}
    if IsToolbar and ((AViewType and TVT_FLOATING) <> 0) then
      Horz := True
    else
    {$ENDIF}
      if (AViewType and TVT_FLOATING) <> 0
        then Horz := ARect.Right > ARect.Bottom
        else Horz := ARect.Right- ARect.Left > ARect.Bottom- ARect.Top;
    PaintIrregularGradient(DC, ARect, ToolbarColor1, ToolbarColor2, not Horz);
  end
  else begin
    {$IFDEF GRADIENT_FILL_BACKGROUND}
    if (AViewType and TVT_MENUBAR) = TVT_MENUBAR then
    begin
      if (AViewType and TVT_FLOATING) = 0
        then PaintDock(Canvas, ARect, ADockRect, DP_TOP)
        else PaintDock(Canvas, ARect, ARect, DP_TOP);
    end
    else
    {$ENDIF}
      FillRectEx(DC, R, AColor);
  end;
end;

procedure TTBXOffice2003Theme.PaintButton(Canvas: TCanvas; const ARect: TRect;
  const ItemInfo: TTBXItemInfo);
var
  DC: HDC;
  R: TRect;
  C1, C2: TColor;
begin
  R := ARect;
  DC := Canvas.Handle;
  with ItemInfo do
  begin
    if ((ItemOptions and IO_DESIGNING) <> 0) and not Selected then
    begin
      if ComboPart = cpSplitRight then Dec(R.Left);
      FrameRectEx(DC, R, NearestBlendedColor(ToolbarColor2, clBlack, 90), False);
    end
    else begin
      FrameRectEx(DC, R, GetBtnColor(ItemInfo, ipFrame), True);

      if ComboPart = cpSplitRight
        then Dec(R.Left)
        else if (ComboPart = cpSplitLeft) and IsPopupParent then Inc(R.Right);

      C1 := GetBtnColor(ItemInfo, ipBody);
      if C1 <> clNone then
      begin
        if ((ViewType and (VT_TOOLBAR or VT_POPUP)) <> 0) and
          ((ViewType and TVT_EMBEDDED) <> 0)
        then
          FillRectEx(DC, R, C1)
        else begin
          C2 := GetBtnColor(ItemInfo, ipBody, True);
          if C2 <> clNone then PaintGradient(DC, R, C1, C2, IsVertical);
        end;
      end;
    end;
    if ComboPart = cpSplitRight then PaintDropDownArrow(Canvas, R, ItemInfo);
  end;
end;

procedure TTBXOffice2003Theme.PaintCaption(Canvas: TCanvas;
  const ARect: TRect; const ItemInfo: TTBXItemInfo; const ACaption: string;
  AFormat: Cardinal; Rotated: Boolean);
var
  DC: HDC;
  OldBkMode: Integer;
begin
  if Canvas.Font.Color = clNone then
    Canvas.Font.Color := GetPartColor(ItemInfo, ipText);
  DC := Canvas.Handle;
  OldBkMode := SetBkMode(DC, TRANSPARENT);
  if not Rotated
    then DrawTextEx(DC, PChar(ACaption), Length(ACaption), PRect(@ARect)^, AFormat, nil)
    else DrawRotatedText(DC, ACaption, ARect, AFormat);
  SetBkMode(DC, OldBkMode);
end;

procedure TTBXOffice2003Theme.PaintCheckMark(Canvas: TCanvas; ARect: TRect;
  const ItemInfo: TTBXItemInfo);
const
  CheckMarkPattern: array[0..6] of TPoint = (
    (X:-3; Y:-1), (X:-1; Y: 1), (X: 3; Y:-3), (X: 3; Y:-2),
    (X:-1; Y: 2), (X:-3; Y: 0), (X:-3; Y:-1));
var
  DC: HDC;
  X, Y: Integer;
  C1, C2: TColor;
begin
  GetRectCenter(ARect, X, Y);
  C1 := GetBtnColor(ItemInfo, ipText);
  DC := Canvas.Handle;
  if (ItemInfo.ItemOptions and IO_RADIO) <> 0 then
  begin
    if ItemInfo.Enabled
      then C2 := NearestMixedColor(C1, GetBtnColor(ItemInfo, ipBody), 32)
      else C2 := NearestMixedColor(ToolbarColor1, ToolbarColor2, 127);
    ARect := Rect(X-2, Y-2, X+3, Y+3);
    FrameRectEx(DC, ARect, C2, False);
    RoundRectEx(DC, ARect, 5, 5, C1, C1);
  end
  else DrawPattern(DC, X, Y, CheckMarkPattern, C1, False);
end;

procedure TTBXOffice2003Theme.PaintChevron(Canvas: TCanvas; ARect: TRect;
  const ItemInfo: TTBXItemInfo);
const
  ChevronPattern: array[Boolean, 0..15] of Byte = (
    ($CC, 0, $66, 0, $33, 0, $66, 0, $CC, 0, 0, 0, 0, 0, 0, 0),
    ($88, 0, $D8, 0, $70, 0, $20, 0, $88, 0, $D8, 0, $70, 0, $20, 0));
begin
  with ItemInfo do
    if Selected or Pushed or (HoverKind <> hkNone) then
      PaintButton(Canvas, ARect, ItemInfo);
  if not ItemInfo.IsVertical then
  begin
    ARect.Left := (ARect.Left + ARect.Right) div 2 - 4;
    ARect.Top := ARect.Top + 4;
  end
  else begin
    ARect.Left := ARect.Right - 9;
    ARect.Top := (ARect.Top + ARect.Bottom) div 2 - 4;
  end;
  DrawGlyph(Canvas.Handle, ARect.Left, ARect.Top,
    ChevronPattern[ItemInfo.IsVertical], GetPartColor(ItemInfo, ipText));
end;

procedure TTBXOffice2003Theme.PaintDock(Canvas: TCanvas; const ClientRect,
  DockRect: TRect; DockPosition: Integer);
var
  DC: HDC;
  R: TRect;
  {$IFDEF GRADIENT_FILL_BACKGROUND}
  P: TPoint;
  C1, C2: TColor;
  {$ENDIF}
begin
  DC := Canvas.Handle;
  if (GetClipBox(DC, R) in [SIMPLEREGION, COMPLEXREGION]) and
    RectsIntersect(DockRect, R) then
    {$IFDEF GRADIENT_FILL_BACKGROUND}
    { Get the dock position }
    if GetCurrentPositionEx(DC, @P)
      then GetGradientColors(P.X and $FFFF, P.Y and $FFFF, C1, C2)
      else begin
        C1 := DockColor1;
        C2 := DockColor2;
      end;
    PaintGradient(DC, DockRect, C1, C2, True);
    {$ELSE}
    FillRectEx(DC, R, DockColor1);
    {$ENDIF}
end;

procedure TTBXOffice2003Theme.PaintDockPanelNCArea(Canvas: TCanvas; R: TRect;
  const DockPanelInfo: TTBXDockPanelInfo);
var
  DC: HDC;
  CI, CapSize: Integer;
  R2: TRect;
  C: TColor;
  BtnItemState: TBtnItemState;
begin
  CI := ColorIntensity(FFaceColor);
  R2 := R;
  DC := Canvas.Handle;
  with DockPanelInfo do
  begin
    { Frame }
    if not TBXLoColor and (CI in [64..250]) then
    begin
      C := ToolbarColor2;
      FrameRectEx(DC, R2, C, True);
      FrameRectEx(DC, R2, EffectiveColor, False);
    end
    else begin
      FrameRectEx(DC, R2, EffectiveColor, True);
      if CI < 64 then C := clWhite else C := clBtnShadow;
      DitherFrame(DC, R2, EffectiveColor, C);
      C := EffectiveColor;
    end;
    with R2 do
    begin
      SetPixelV(DC, Left, Top, C);
      if IsVertical then Left := Right-1 else Top := Bottom-1;
      SetPixelV(DC, Left, Top, C);
    end;

    { Caption }
    if not ShowCaption then Exit;
    { Background }
    R2 := R;
    InflateRect(R2, -BorderSize.X, -BorderSize.Y);
    CapSize := GetSystemMetrics(SM_CYSMCAPTION)- 1;
    if IsVertical then
    begin
      R2.Bottom := R2.Top + CapSize;
      DrawLineEx(DC, R2.Left, R2.Bottom, R2.Right, R2.Bottom, EffectiveColor);
    end
    else begin
      R2.Right := R2.Left + CapSize;
      DrawLineEx(DC, R2.Right, R2.Top, R2.Right, R2.Bottom, EffectiveColor);
    end;
    PaintGradient(DC, R2, ToolbarColor1, ToolbarColor2, not IsVertical);
    { Close button }
    if (CloseButtonState and CDBS_VISIBLE) <> 0 then
    begin
      R := R2;
      if IsVertical then
      begin
        R.Left := R.Right - CapSize;
        R2.Right := R.Left + 2;
      end
      else begin
        R.Top := R.Bottom - CapSize;
        R2.Bottom := R.Top + 2;
      end;
      BtnItemState := GetCloseBtnItemState(CloseButtonState);
      {$IFDEF SMALL_CLOSE_BUTTON_ON_DOCKED_DOCKPANEL}
      DrawCloseButton(DC, R, BtnItemState, True, True, BtnItemColors[BtnItemState, ipText]);
      {$ELSE}
      DrawCloseButton(DC, R, BtnItemState, True, False, BtnItemColors[BtnItemState, ipText]);
      {$ENDIF}
    end;
    { Text }
    if IsVertical then InflateRectHorz(R2, -4) else InflateRectVert(R2, -4);
    DrawSmCaptionText(DC, Caption, R2, SmCaptionFont.Color, not IsVertical);
  end;
end;

procedure TTBXOffice2003Theme.PaintDropDownArrow(Canvas: TCanvas;
  const ARect: TRect; const ItemInfo: TTBXItemInfo);
const
  ArrowPattern: array[Boolean, 0..2] of TPoint = (
    ((X:-2; Y:-1), (X: 2; Y:-1), (X: 0; Y: 1)),
    ((X: 0; Y: 1), (X: 0; Y:-3), (X:-2; Y:-1)));
begin
  DrawPattern(Canvas.Handle, GetRectCenter(ARect),
    ArrowPattern[ItemInfo.IsVertical], GetPartColor(ItemInfo, ipText), True);
end;

procedure TTBXOffice2003Theme.PaintEditButton(Canvas: TCanvas; const ARect: TRect;
  var ItemInfo: TTBXItemInfo; ButtonInfo: TTBXEditBtnInfo);
var
  DC: HDC;
  BR: TRect;
  State: set of (OnToolbar, Embedded, Enabled, Hot, Pressed);

  procedure CalcButtonRect(DownArrow: Boolean);
  begin
    BR := ARect;
    if (State * [Embedded, Hot, Pressed] <> []) or
      not (OnToolbar in State) then
    begin
      InflateRect(BR, 1, 1);
      Inc(BR.Left);
    end;
    if ButtonInfo.ButtonType = EBT_SPIN then
    begin
      if DownArrow
        then BR.Top := (BR.Top + BR.Bottom) div 2
        else BR.Bottom := (BR.Top + BR.Bottom + 1) div 2;
    end;
  end;

  procedure DrawButtonBody;
  var
    SavePushed: Boolean;
  begin
    if State * [Embedded, Hot, Pressed] = [] then
    begin
      if OnToolbar in State then
      begin
        DrawLineEx(DC, BR.Left-1, BR.Top, BR.Left-1, BR.Bottom, FWindowColor);
        FrameRectEx(DC, BR, FWindowColor, False);
      end;
    end
    else begin
      SavePushed := ItemInfo.Pushed;
      ItemInfo.Pushed := Pressed in State;
      PaintButton(Canvas, BR, ItemInfo);
      ItemInfo.Pushed := SavePushed;
    end;
  end;

  procedure DrawButton(DownArrow: Boolean);
  const
    ArrowPattern: array[Boolean, 0..2] of TPoint = (
      ((X:-2; Y: 1), (X: 2; Y: 1), (X: 0; Y:-1)),
      ((X:-2; Y:-1), (X: 2; Y:-1), (X: 0; Y: 1)));
  begin
    CalcButtonRect(DownArrow);
    if Enabled in State then DrawButtonBody;
    if not DownArrow then Dec(BR.Bottom);
    DrawPattern(DC, GetRectCenter(BR), ArrowPattern[DownArrow],
      GetPartColor(ItemInfo, ipText), True);
  end;

begin
  State := [Enabled];
  if (ItemInfo.ViewType and VT_TOOLBAR) <> 0 then
  begin
    Include(State, OnToolbar);
    if ((ItemInfo.ViewType and TVT_EMBEDDED) <> 0) then Include(State, Embedded);
  end;
  DC := Canvas.Handle;
  with ButtonInfo do
  begin
    if ButtonType = EBT_DROPDOWN then
    begin
      if (ButtonState and EBDS_DISABLED) <> 0 then Exclude(State, Enabled);
      if (ButtonState and EBDS_HOT) <> 0 then Include(State, Hot);
      if (ButtonState and EBDS_PRESSED) <> 0 then Include(State, Pressed);
      DrawButton(True);
    end
    else if ButtonType = EBT_SPIN then
    begin
      if (ButtonState and EBSS_DISABLED) <> 0 then Exclude(State, Enabled);
      if (ButtonState and EBSS_HOT) <> 0 then Include(State, Hot);
      { Upper button }
      if (ButtonState and EBSS_UP) <> 0 then Include(State, Pressed);
      DrawButton(False);
      Exclude(State, Pressed);
      { Lower button }
      if (ButtonState and EBSS_DOWN) <> 0 then Include(State, Pressed);
      DrawButton(True);
    end;
  end;
end;

procedure TTBXOffice2003Theme.PaintEditFrame(Canvas: TCanvas; const ARect: TRect;
  var ItemInfo: TTBXItemInfo; const EditInfo: TTBXEditInfo);
var
  DC: HDC;
  IsToolbar, Embedded: Boolean;
  R: TRect;
  C: TColor;
  W: Integer;
begin
  IsToolbar := (ItemInfo.ViewType and VT_TOOLBAR) <> 0;
  Embedded := IsToolbar and ((ItemInfo.ViewType and TVT_EMBEDDED) <> 0);

  R := ARect;
  DC := Canvas.Handle;
  with ItemInfo do
  begin
    if ((ViewType and TVT_FLOATING) <> 0) and Enabled and
      not (Pushed or Selected or (HoverKind <> hkNone))
    then
      RectangleEx(DC, R, ToolbarColor2, GetPartColor(ItemInfo, ipBody))
    else begin
      if ((ItemOptions and IO_DESIGNING) <> 0) and not Selected
        then C := NearestBlendedColor(ToolbarColor2, clBlack, 90)
        else C := GetPartColor(ItemInfo, ipFrame);
      FrameRectEx(DC, R, C, False);
    end;
  end;

  W := EditFrameWidth;
  InflateRect(R, -W, -W);

  if not (ItemInfo.Enabled or Embedded) then
    FrameRectEx(DC, R, ToolbarColor2, False);

  with EditInfo do
    if RightBtnWidth > 0 then Dec(R.Right, RightBtnWidth - 2);

  if ItemInfo.Enabled then
  begin
    if not IsToolbar and (GetPartColor(ItemInfo, ipFrame) = clNone) then
    begin
      RectangleEx(DC, R, ToolbarColor2, FWindowColor);
      if EditInfo.RightBtnWidth > 0 then
      begin
        R := ARect;
        InflateRect(R, -1, -1);
        R.Left := R.Right - EditInfo.RightBtnWidth + 1;
        FrameRectEx(DC, R, ToolbarColor2, False);
      end;
    end
    else FillRectEx(DC, R, FWindowColor);
  end;

  if EditInfo.RightBtnWidth > 0 then
  begin
    R := ARect;
    InflateRect(R, -W, -W);
    R.Left := R.Right - EditInfo.RightBtnWidth;
    PaintEditButton(Canvas, R, ItemInfo, EditInfo.RightBtnInfo);
  end;
end;

procedure TTBXOffice2003Theme.PaintFloatingBorder(Canvas: TCanvas;
  const ARect: TRect; const WindowInfo: TTBXWindowInfo);
var
  DC: HDC;
  R, CR: TRect;
  Sz: TPoint;
  CapSize: Integer;
  C, LineColor, CapColor, TextColor: TColor;
  BtnItemState: TBtnItemState;
begin
  DC := Canvas.Handle;

  if (WindowInfo.RedrawPart and WRP_BORDER) <> 0 then
  begin
    R := ARect;
    Sz := WindowInfo.FloatingBorderSize;

    SaveDC(DC);
    with R, Sz do
      ExcludeClipRect(DC, Left+X, Top+Y, Right-X, Bottom-Y);
    FillRectEx(DC, R, WinFrameColors[wfpBorder]);
    RestoreDC(DC, -1);

    OffsetPoint(Sz, -1, -1);
    InflateRect(R, -Sz.X, -Sz.Y);

    {$IFDEF GRADIENT_FILL_BACKGROUND}
    if (WindowInfo.ViewType and TVT_MENUBAR) = TVT_MENUBAR then
    begin
      with R do
      begin
        GetGradientColors(0, Right, LineColor, C);
        DrawLineEx(DC, Left, Top+1, Left, Bottom-1, DockColor1);
        PaintGradient(DC, Rect(Left+1, Top, Right-1, Top+1), DockColor1, C, True);
        PaintGradient(DC, Rect(Left+1, Bottom-1, Right-1, Bottom), DockColor1, C, True);
        DrawLineEx(DC, Right-1, Top+1, Right-1, Bottom-1, C);
      end;
    end
    else
    {$ENDIF}
    begin
      if (WindowInfo.ViewType and VT_DOCKPANEL) <> 0
        then C := WindowInfo.EffectiveColor
        else C := DockColor1;
      RoundFrameEx(DC, R, 2, 2, C);
    end;
  end;

  if not WindowInfo.ShowCaption then Exit;

  if ((WindowInfo.RedrawPart and WRP_CAPTION) <> 0) or
    ((WindowInfo.CloseButtonState and CDBS_VISIBLE) <> 0) then
  begin
    CapSize := GetSystemMetrics(SM_CYSMCAPTION)- 1;
    CR := Rect(0, 0, WindowInfo.ClientWidth, CapSize);
    with WindowInfo.FloatingBorderSize do OffsetRect(CR, X, Y);

    CapColor := WinFrameColors[wfpCaption];
    TextColor := WinFrameColors[wfpCaptionText];

    if ((WindowInfo.ViewType and TVT_MENUBAR) = TVT_MENUBAR) or
      ((WindowInfo.ViewType and TVT_TOOLWINDOW) = TVT_TOOLWINDOW)
    then
      LineColor := DockColor1
    else if (WindowInfo.ViewType and VT_DOCKPANEL) <> 0
      then LineColor := WindowInfo.EffectiveColor
      else LineColor := ToolbarColor1;

    { Caption }
    if (WindowInfo.RedrawPart and WRP_CAPTION) <> 0 then
    begin
      R := CR;
      if (WindowInfo.CloseButtonState and CDBS_VISIBLE) <> 0 then
        Dec(R.Right, CapSize);

      FillRectEx(DC, R, CapColor);
      DrawLineEx(DC, R.Left, R.Bottom, R.Right, R.Bottom, LineColor);

      Inc(R.Left, 2); Dec(R.Right, 1);
      DrawSmCaptionText(DC, WindowInfo.Caption, R, TextColor, False);
    end;
    { Close button }
    if ((WindowInfo.CloseButtonState and CDBS_VISIBLE) <> 0) and
      ((WindowInfo.RedrawPart and WRP_CLOSEBTN) <> 0) then
    begin
      R := CR;
      R.Left := R.Right- CapSize;

      FillRectEx(DC, R, CapColor);
      DrawLineEx(DC, R.Left, R.Bottom, R.Right, R.Bottom, LineColor);

      BtnItemState := GetCloseBtnItemState(WindowInfo.CloseButtonState);
      if BtnItemState = bisNormal
        then C := TextColor
        else C := BtnItemColors[BtnItemState, ipText];
      {$IFDEF SMALL_CLOSE_BUTTON_ON_FLOATING_XXX}
      DrawCloseButton(DC, R, BtnItemState, False, True, C);
      {$ELSE}
      DrawCloseButton(DC, R, BtnItemState, False, False, C);
      {$ENDIF}
    end;
  end;
end;

procedure TTBXOffice2003Theme.PaintFrame(Canvas: TCanvas; const ARect: TRect;
  const ItemInfo: TTBXItemInfo);
begin
  RectangleEx(Canvas.Handle, ARect, GetPartColor(ItemInfo, ipFrame),
    GetPartColor(ItemInfo, ipBody));
end;

procedure TTBXOffice2003Theme.PaintFrameControl(Canvas: TCanvas; R: TRect;
  Kind, State: Integer; Params: Pointer);
const
  CheckMarkPattern: array[0..6] of TPoint = (
    (X:-3; Y: 1), (X:-1; Y: 3), (X: 3; Y:-1), (X: 3; Y:-3),
    (X:-1; Y: 1), (X:-3; Y:-1), (X:-3; Y: 1));
  XPPartId: array[Boolean] of Byte = (BP_RADIOBUTTON, BP_CHECKBOX);

   function GetXPStateFlags: Integer;
   begin
     if Kind = PFC_CHECKBOX then
       if (State and PFS_MIXED) <> 0 then Result := CBS_MIXEDNORMAL else
       if (State and PFS_CHECKED) <> 0 then Result := CBS_CHECKEDNORMAL else
         Result := CBS_UNCHECKEDNORMAL
     else
       if (State and PFS_CHECKED) <> 0
         then Result := RBS_CHECKEDNORMAL
         else Result := RBS_UNCHECKEDNORMAL;

     if (State and PFS_DISABLED) <> 0 then Inc(Result, 3) else
     if (State and PFS_PUSHED) <> 0 then Inc(Result, 2) else
     if (State and PFS_HOT) <> 0 then Inc(Result);
  end;

  procedure GetColors(out OutlineColor, FillColor, MarkColor: TColor);
  begin
    if (State and PFS_DISABLED) <> 0 then
    begin
      OutlineColor := clBtnShadow;
      FillColor := NearestMixedColor(FWindowColor, FFaceColor, 127);
      MarkColor := BtnItemColors[bisDisabled, ipText];
    end
    else if (State and PFS_PUSHED) <> 0 then
    begin
      OutlineColor := BtnItemColors[bisPressed, ipFrame];
      FillColor := BtnBodyColors[bisPressed, False];
      MarkColor := BtnItemColors[bisPressed, ipText];
    end
    else if (State and PFS_HOT) <> 0 then
    begin
      OutlineColor := BtnItemColors[bisHot, ipFrame];
      FillColor := BtnBodyColors[bisHot, False];
      MarkColor := BtnItemColors[bisHot, ipText];
    end
    else if (State and PFS_MIXED) <> 0 then
    begin
      OutlineColor := EmbeddedFrameColor;
      FillColor := NearestMixedColor(FWindowColor, FFaceColor, 127);
      MarkColor := clBtnShadow;
    end
    else begin
      OutlineColor := EmbeddedFrameColor;
      FillColor := clNone;
      MarkColor := BtnItemColors[bisNormal, ipText];
    end;
  end;

var
  DC: HDC;
  OutlineColor, FillColor, MarkColor: TColor;
begin
  DC := Canvas.Handle;
  {$IFDEF XP_THEMED_CHECKBOX_AND_RADIOBUTTON}
  if USE_THEMES then
  begin
//    DrawThemeBackground(XPTheme(tcButton), DC, XPPartId[Kind = PFC_CHECKBOX],
//      GetXPStateFlags, R, nil); KV
    DrawThemeBackground(BUTTON_THEME, DC, XPPartId[Kind = PFC_CHECKBOX],
      GetXPStateFlags, R, nil);
  end
  else
  {$ENDIF}
  begin
    InflateRect(R, -1, -1);
    GetColors(OutlineColor, FillColor, MarkColor);
    case Kind of
      PFC_CHECKBOX:
        begin
          RectangleEx(DC, R, OutlineColor, FillColor);
          if (State and PFS_MIXED) <> 0 then
          begin
            InflateRect(R, -3, -3);
            FillRectEx(DC, R, MarkColor);
          end
          else if (State and PFS_CHECKED) <> 0 then
            DrawPattern(DC, GetRectCenter(R), CheckMarkPattern, MarkColor, True);
        end;
      PFC_RADIOBUTTON:
        begin
          EllipseEx(DC, R, OutlineColor, FillColor);
          if (State and PFS_CHECKED) <> 0 then
          begin
            InflateRect(R, -3, -3);
            EllipseEx(DC, R, MarkColor, MarkColor);
          end;
        end;
    end;
  end;
end;

procedure TTBXOffice2003Theme.PaintImage(Canvas: TCanvas; ARect: TRect;
  const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList; ImageIndex: Integer);
var
  HiContrast: Boolean;
begin
  with ItemInfo do
  begin
    {$IFDEF PNGIMAGELIST}
    if ImageList is TPNGImageList then
    begin
      if Enabled
        then TPNGImageList(ImageList).Images[ImageIndex].Image.Draw(Canvas, ARect)
        else TPNGImageList(ImageList).Images[ImageIndex].DisabledImage.Draw(Canvas, ARect);
      Exit;
    end;
    {$ENDIF}
    if ImageList is TTBCustomImageList then
    begin
      TTBCustomImageList(ImageList).DrawState(Canvas, ARect.Left, ARect.Top,
        ImageIndex, Enabled, (HoverKind <> hkNone), Selected);
      Exit;
    end;

    HiContrast := ColorIntensity(GetItemImageBackground(ItemInfo)) < 80;
    if Enabled then
    begin
      {$IFDEF HIGHLIGHT_TOOLBAR_ICONS}
      if Selected or Pushed or (HoverKind <> hkNone) or HiContrast or TBXNoBlending
        then DrawTBXIcon(Canvas, ARect, ImageList, ImageIndex, HiContrast)
        else HighlightTBXIcon(Canvas, ARect, ImageList, ImageIndex, FWindowColor, 178);
      {$ELSE}
      DrawTBXIcon(Canvas, ARect, ImageList, ImageIndex, HiContrast);
      {$ENDIF}
    end
    else if HiContrast
      then DrawTBXIconFlatShadow(Canvas, ARect, ImageList, ImageIndex, SeparatorColor1)
      else DrawTBXIconShadow(Canvas, ARect, ImageList, ImageIndex, 0);
  end;
end;

procedure TTBXOffice2003Theme.PaintMDIButton(Canvas: TCanvas; ARect: TRect;
  const ItemInfo: TTBXItemInfo; ButtonKind: Cardinal);
var
  Index: Integer;
begin  
  PaintButton(Canvas, ARect, ItemInfo);
  case ButtonKind of
    DFCS_CAPTIONCLOSE:   Index := 0;
    DFCS_CAPTIONMIN:     Index := 2;
    DFCS_CAPTIONMAX:     Index := 1;
    DFCS_CAPTIONRESTORE: Index := 3;
  else
    Exit;
  end;
  Dec(ARect.Bottom);
  DrawGlyph(Canvas.Handle, ARect, GetGlyphsImList, Index,
    GetPartColor(ItemInfo, ipText));
end;

procedure TTBXOffice2003Theme.PaintMenuItem(Canvas: TCanvas; const ARect: TRect;
  var ItemInfo: TTBXItemInfo);
const
  ArrowPattern: array[0..2] of TPoint =
   ((X: 0; Y:-3), (X: 0; Y: 3), (X: 3; Y: 0));
var
  DC: HDC;
  R: TRect;
  ArrowWidth, X: Integer;
  C: TColor;
begin
  R := ARect;
  PaintMenuItemFrame(Canvas, R, ItemInfo);
  DC := Canvas.Handle;
  with ItemInfo do
  begin
    if (ItemOptions and (IO_COMBO or IO_SUBMENUITEM)) <> 0 then
    begin
      ArrowWidth := GetSystemMetrics(SM_CXMENUCHECK);
      C := GetPartColor(ItemInfo, ipText);

      if (ItemOptions and IO_SUBMENUITEM) <> 0 then
        DrawPattern(DC, R.Right - ArrowWidth * 2 div 3 - 1, R.Bottom div 2,
          ArrowPattern, C, True);

      if (ItemOptions and IO_COMBO) <> 0 then
      begin
        X := R.Right- ArrowWidth- 1;
        if Enabled then
          if HoverKind = hkMouseHover
            then C := GetPartColor(ItemInfo, ipFrame)
            else C := PopupFrameColor;
        DrawLineEx(DC, X, R.Top+ 1, X, R.Bottom- 1, C);
      end;
    end;

    if Enabled and Selected then
    begin
      Inc(R.Left);
      R.Right := R.Left + PopupMargin;
      InflateRect(R, -1, -1);
      RectangleEx(DC, R, GetBtnColor(ItemInfo, ipFrame),
        GetBtnColor(ItemInfo, ipBody));
    end;
  end;
end;

procedure TTBXOffice2003Theme.PaintMenuItemFrame(Canvas: TCanvas;
  const ARect: TRect; const ItemInfo: TTBXItemInfo);
var
  R: TRect;
begin
  R := ARect;
  with ItemInfo do
  begin
    if (ViewType and PVT_TOOLBOX) <> PVT_TOOLBOX then
    begin
      R.Right := R.Left + PopupMargin + 2;
      PaintIrregularGradient(Canvas.Handle, R, ToolbarColor1, ToolbarColor2, True);
    end;
    if (HoverKind <> hkNone) and (Enabled or (HoverKind = hkKeyboardHover)) then
    begin
      R.Right := ARect.Right;
      InflateRectHorz(R, -1);
      PaintFrame(Canvas, R, ItemInfo);
    end;
  end;
end;

procedure TTBXOffice2003Theme.PaintPageScrollButton(Canvas: TCanvas;
  const ARect: TRect; ButtonType: Integer; Hot: Boolean);
const
  BtnItemState: array[Boolean] of TBtnItemState = (bisNormal, bisHot);
var
  DC: HDC;
  FrameColor, FillColor: TColor;
  X, Y, Sz: Integer;
  ArrowPoints: array[0..2] of TPoint;
begin
  DC := Canvas.Handle;
  { Frame & body }
  if Hot then
  begin
    FrameColor := BtnItemColors[bisHot, ipFrame];
    FillColor := BtnBodyColors[bisHot, False];
  end
  else begin
    FrameColor := EmbeddedFrameColor;
    FillColor := EmbeddedColor;
  end;
  RectangleEx(DC, ARect, FrameColor, FillColor);
  { Arrow }
  FillLongWord(ArrowPoints[0], SizeOf(ArrowPoints) div SizeOf(Integer), 0);
  GetRectCenter(ARect, X, Y);
  Sz := Min(X - ARect.Left, Y - ARect.Top) * 3 div 4;
  if ButtonType in [PSBT_UP, PSBT_DOWN] then
  begin
    if ButtonType = PSBT_UP then Sz := -Sz else Dec(Y);
    ArrowPoints[0].X := -Sz;
    ArrowPoints[1].X := Sz;
    ArrowPoints[2].Y := Sz;
    Dec(Y, Sz div 2);
  end
  else begin
    if ButtonType = PSBT_LEFT then Sz := -Sz else Dec(X);
    ArrowPoints[0].Y := -Sz;
    ArrowPoints[1].Y := Sz;
    ArrowPoints[2].X := Sz;
    Dec(X, Sz div 2);
  end;
  DrawPattern(DC, X, Y, ArrowPoints, BtnItemColors[BtnItemState[Hot], ipText], True);
end;

procedure TTBXOffice2003Theme.PaintPopupNCArea(Canvas: TCanvas; R: TRect;
  const PopupInfo: TTBXPopupInfo);
var
  DC: HDC;
  PR: TRect;
begin
  DC := Canvas.Handle;
  RectangleEx(DC, R, PopupFrameColor, PopupColor);
  PR := PopupInfo.ParentRect;
  if IsRectEmpty(PR) then Exit;
  InflateRect(R, -1, -1);
  with PR do
  begin
    if Bottom = R.Top then
    begin
      if Left <= R.Left then Left := R.Left else Inc(Left);
      if Right >= R.Right then Right := R.Right else Dec(Right);
      Dec(Bottom);
      Top := Bottom;
    end
    else if Top = R.Bottom then
    begin
      if Left <= R.Left then Left := R.Left else Inc(Left);
      if Right >= R.Right then Right := R.Right else Dec(Right);
      Bottom := Top;
    end;
    if Right = R.Left then
    begin
      if Top <= R.Top then Top := R.Top else Inc(Top);
      if Bottom >= R.Bottom then Bottom := R.Bottom else Dec(Bottom);
      Dec(Right);
      Left := Right;
    end
    else if Left = R.Right then
    begin
      if Top <= R.Top then Top := R.Top else Inc(Top);
      if Bottom >= R.Bottom then Bottom := R.Bottom else Dec(Bottom);
      Right := Left;
    end;
    DrawLineEx(DC, Left, Top, Right, Bottom, PopupColor);
  end;
end;

procedure TTBXOffice2003Theme.PaintSeparator(Canvas: TCanvas; ARect: TRect;
  ItemInfo: TTBXItemInfo; Horizontal, LineSeparator: Boolean);
var
  DC: HDC;
  I: Integer;
begin
  { Note: for blank separators, Enabled = False }
  DC := Canvas.Handle;
  with ItemInfo, ARect do
  begin
    if (ViewType and PVT_POPUPMENU) = PVT_POPUPMENU then
    begin
      { on popup menu }
      I := Right;
      Right := PopupMargin + 2;
      PaintIrregularGradient(DC, ARect, ToolbarColor1, ToolbarColor2, True);
      if Enabled then
      begin
        Top := (Top + Bottom) div 2;
        DrawLineEx(DC, Left + PopupMargin + 9, Top, I, Top, SeparatorColor1);
      end;
    end
    else if Enabled then
    begin
      if Horizontal then
        if (ViewType and TVT_NORMALTOOLBAR) = TVT_NORMALTOOLBAR then
        begin
          { on horizontal toolbar }
          if LineSeparator
            then I := -1 { on multirow (tbsmWrap or floating) toolbar }
            else I := -Round((Right - Left - 2) * 0.2);
          InflateRect(ARect, I, -((Bottom - Top - 1) div 2));
          DrawLineEx(DC, Left, Top, Right, Top, SeparatorColor1);
          OffsetRect(ARect, 1, 1);
          DrawLineEx(DC, Left, Top, Right, Top, SeparatorColor2);
        end
        else begin
          { on listbox and... }
          InflateRectVert(ARect, -((Bottom - Top - 1) div 2));
          DrawLineEx(DC, Left, Top, Right, Top, SeparatorColor1);
        end
      else { Vertical }
        if (ViewType and TVT_NORMALTOOLBAR) = TVT_NORMALTOOLBAR then
        begin
          { on vertical toolbar }
          if LineSeparator
            then I := -1 { on multicolumn (tbsmWrap or floating) toolbar }
            else I := -Round((Bottom - Top - 2) * 0.2);
          InflateRect(ARect, -((Right - Left - 1) div 2), I);
          DrawLineEx(DC, Left, Top, Left, Bottom, SeparatorColor1);
          OffsetRect(ARect, 1, 1);
          DrawLineEx(DC, Left, Top, Left, Bottom, SeparatorColor2);
        end
        else begin
          { on listbox and... }
          InflateRectHorz(ARect, -((Right - Left - 1) div 2));
          DrawLineEx(DC, Left, Top, Left, Bottom, SeparatorColor1);
        end;
    end;
  end;
end;

procedure TTBXOffice2003Theme.PaintToolbarNCArea(Canvas: TCanvas; R: TRect;
  const ToolbarInfo: TTBXToolbarInfo);
const
  DragHandleOffsets: array[Boolean, DHS_DOUBLE..DHS_SINGLE] of Integer =
    ((3, 0, 2), (6, 0, 6));

  procedure DrawDots(DC: HDC; R: TRect; Color: TColor; Horz: Boolean);
  var
    Fin: Integer;
    Brush: HBRUSH;
  begin
    if Horz then Fin := R.Right else Fin := R.Bottom;
    R.Right := R.Left+ 2;
    R.Bottom := R.Top+ 2;
    Brush := CreateBrushEx(Color);
    if Horz then
      while R.Left < Fin do
      begin
        FillRect(DC, R, Brush);
        OffsetRectHorz(R, 4);
      end
    else
      while R.Top < Fin do
      begin
        FillRect(DC, R, Brush);
        OffsetRectVert(R, 4);
      end;
    DeleteObject(Brush);
  end;

var
  DC: HDC;
  Horz, IsMenuBar, BtnVisible: Boolean;
  Sz, Offset: Integer;
  R2: TRect;
  BtnItemState: TBtnItemState;
  Brush: HBRUSH;
  {$IFNDEF GRADIENT_FILL_BACKGROUND}
  C: TColor;
  {$ENDIF}
begin
  DC := Canvas.Handle;
  Horz := not ToolbarInfo.IsVertical;
  IsMenuBar := ToolbarInfo.ViewType and TVT_MENUBAR = TVT_MENUBAR;

  if (ToolbarInfo.BorderStyle = bsSingle) and not IsMenuBar then
  begin
    if not TBXLoColor and (ColorIntensity(FFaceColor) in [50..254]) then
    begin
      InflateRect(R, -1, -1);
      with R do
      begin
        if Horz then
        begin
          {left}
          {$IFNDEF GRADIENT_FILL_BACKGROUND}
          DrawLineEx(DC, Left-1, Top-1, Left-1, Bottom+1, DockColor1);
          {$ENDIF}
          PaintIrregularGradient(DC, Rect(Left, Top+2, Left+1, Bottom-2), ToolbarColor1, ToolbarColor2, False);
          {top}
          {$IFNDEF GRADIENT_FILL_BACKGROUND}
          DrawLineEx(DC, Left, Top-1, Right+1, Top-1, DockColor1);
          {$ENDIF}
          if (ToolbarInfo.ViewType and TVT_TOOLWINDOW) <> TVT_TOOLWINDOW
            then DrawLineEx(DC, Left+2, Top, Right-1, Top, ToolbarColor1)
            else PaintIrregularGradient(DC, Rect(Left+2, Top, Right-1, Top+1), ToolbarColor1, ToolbarColor2, True);
          {right}
          if (ToolbarInfo.ViewType and TVT_TOOLWINDOW) <> TVT_TOOLWINDOW
            then PaintIrregularGradient(DC, Rect(Right-1, Top+1, Right, Bottom-1), ToolbarColor1, ToolbarColor2, False)
            else DrawLineEx(DC, Right-1, Top+1, Right-1, Bottom-1, ToolbarColor2);
          PaintGradient(DC, Rect(Right, Top+3, Right+1, Bottom-1), ToolbarColor2, ToolbarFrameColor1, False);
          {bottom}
          DrawLineEx(DC, Left+2, Bottom-1, Right-1, Bottom-1, ToolbarColor2);
          DrawLineEx(DC, Left+3, Bottom, Right-1, Bottom, ToolbarFrameColor1);
          {pixels}
          {$IFNDEF GRADIENT_FILL_BACKGROUND}
          SetPixelV(DC, Left, Top, DockColor1);
          PolyLineEx(DC, [Point(Left, Bottom-1), Point(Left, Bottom), Point(Left+2, Bottom)], DockColor1);
          SetPixelV(DC, Right, Top, DockColor1);
          SetPixelV(DC, Right, Bottom, DockColor1);
          C := NearestMixedColor(DockColor1, ToolbarColor1, 127);
          SetPixelV(DC, Left, Top+1, C);
          SetPixelV(DC, Left+1, Top, C);
          SetPixelV(DC, Right-1, Top, C);
          SetPixelV(DC, Right, Top+1, C);
          C := NearestMixedColor(ToolbarColor2, DockColor1, 165);
          SetPixelV(DC, Left, Bottom-2, C);
          SetPixelV(DC, Left+1, Bottom-1, C);
          SetPixelV(DC, Left+2, Bottom, NearestMixedColor(ToolbarFrameColor1, C, 64));
          SetPixelV(DC, Right, Top+2, C);
          SetPixelV(DC, Right-1, Bottom-1, ToolbarFrameColor1);
          SetPixelV(DC, Right, Bottom-1, C);
          SetPixelV(DC, Right-1, Bottom, C);
          {$ELSE}
          SetPixelEx(DC, Left, Top+1, ToolbarColor1, 118);
          SetPixelEx(DC, Left+1, Top, ToolbarColor1, 118);
          SetPixelEx(DC, Right-1, Top, ToolbarColor1, 118);
          SetPixelEx(DC, Right, Top+1, ToolbarColor1, 118);
          SetPixelEx(DC, Left, Bottom-2, ToolbarColor2, 136);
          SetPixelEx(DC, Left+1, Bottom-1, ToolbarColor2, 136);
          SetPixelEx(DC, Left+2, Bottom, ToolbarFrameColor1, 127);
          SetPixelEx(DC, Right, Top+2, ToolbarColor2, 136);
          SetPixelV(DC, Right-1, Bottom-1, ToolbarFrameColor1);
          SetPixelEx(DC, Right, Bottom-1, ToolbarColor2, 136);
          SetPixelEx(DC, Right-1, Bottom, ToolbarColor2, 136);
          {$ENDIF}
        end
        else begin
          {left}
          {$IFNDEF GRADIENT_FILL_BACKGROUND}
          DrawLineEx(DC, Left-1, Top, Left-1, Bottom+1, DockColor1);
          {$ENDIF}
          DrawLineEx(DC, Left, Top+2, Left, Bottom-1, ToolbarColor1);
          {top}
          {$IFNDEF GRADIENT_FILL_BACKGROUND}
          DrawLineEx(DC, Left-1, Top-1, Right+1, Top-1, DockColor1);
          {$ENDIF}
          PaintIrregularGradient(DC, Rect(Left+2, Top, Right-1, Top+1), ToolbarColor1, ToolbarColor2, True);
          {right}
          DrawLineEx(DC, Right-1, Top+1, Right-1, Bottom-1, ToolbarColor2);
          DrawLineEx(DC, Right, Top+3, Right, Bottom-1, ToolbarFrameColor1);
          {bottom}
          PaintIrregularGradient(DC, Rect(Left+1, Bottom-1, Right-1, Bottom), ToolbarColor1, ToolbarColor2, True);
          PaintGradient(DC, Rect(Left+3, Bottom, Right-1, Bottom+1), ToolbarColor2, ToolbarFrameColor1, True);
          {pixels}
          {$IFNDEF GRADIENT_FILL_BACKGROUND}
          SetPixelV(DC, Left, Top, DockColor1);
          SetPixelV(DC, Left, Bottom, DockColor1);
          SetPixelV(DC, Right, Bottom, DockColor1);
          SetPixelV(DC, Right, Top, DockColor1);
          C := NearestMixedColor(DockColor1, ToolbarColor1, 127);
          SetPixelV(DC, Left, Top+1, C);
          SetPixelV(DC, Left+1, Top, C);
          SetPixelV(DC, Left, Bottom-1, C);
          SetPixelV(DC, Left+1, Bottom, C);
          C := NearestMixedColor(ToolbarColor2, DockColor1, 165);
          SetPixelV(DC, Left+2, Bottom, C);
          SetPixelV(DC, Right-1, Top, C);
          SetPixelV(DC, Right, Top+1, C);
          SetPixelV(DC, Right, Top+2, NearestMixedColor(ToolbarFrameColor1, C, 127));
          SetPixelV(DC, Right-1, Bottom-1, ToolbarFrameColor1);
          SetPixelV(DC, Right, Bottom-1, C);
          SetPixelV(DC, Right-1, Bottom, C);
          {$ELSE}
          SetPixelEx(DC, Left, Top+1, ToolbarColor1, 118);
          SetPixelEx(DC, Left+1, Top, ToolbarColor1, 118);
          SetPixelEx(DC, Left, Bottom-1, ToolbarColor1, 118);
          SetPixelEx(DC, Left+1, Bottom, ToolbarColor1, 118);
          SetPixelEx(DC, Left+2, Bottom, ToolbarColor2, 136);
          SetPixelEx(DC, Right-1, Top, ToolbarColor2, 136);
          SetPixelEx(DC, Right, Top+1, ToolbarColor2, 136);
          SetPixelEx(DC, Right, Top+2, ToolbarFrameColor1, 127);
          SetPixelV(DC, Right-1, Bottom-1, ToolbarFrameColor1);
          SetPixelEx(DC, Right, Bottom-1, ToolbarColor2, 136);
          SetPixelEx(DC, Right-1, Bottom, ToolbarColor2, 136);
          {$ENDIF}
        end;
      end;
      OffsetPoint(R.TopLeft, 1, 1);
    end
    else begin
      Brush := CreateDitheredBrush(ToolbarColor1, clBtnShadow);
      with R do
      begin
        FillRect(DC, Rect(Left+1, Top, Right-1, Top+1), Brush);
        FillRect(DC, Rect(Left+1, Bottom-1, Right-1, Bottom), Brush);
        FillRect(DC, Rect(Left, Top+1, Left+1, Bottom-1), Brush);
        FillRect(DC, Rect(Right-1, Top+1, Right, Bottom-1), Brush);
      end;
      DeleteObject(Brush);
      InflateRect(R, -1, -1);
      FillRectEx(DC, R, ToolbarColor1);
    end;
  end
  else begin
    {$IFDEF GRADIENT_FILL_BACKGROUND}
    if not IsMenuBar then
    {$ENDIF}
      FillRectEx(DC, R, DockColor1);
    InflateRect(R, -1, -1);
  end;

  { Drag Handle }
  if not ToolbarInfo.AllowDrag then Exit;

  InflateRect(R, -1, -1);
  BtnVisible := (ToolbarInfo.CloseButtonState and CDBS_VISIBLE) <> 0;

  if (ToolbarInfo.DragHandleStyle <> DHS_NONE) then
  begin
    Sz := GetTBXDragHandleSize(ToolbarInfo);
    if Horz then R.Right := R.Left + Sz else R.Bottom := R.Top + Sz;
    if not IsMenuBar then
      PaintIrregularGradient(DC, Rect(R.Left-1, R.Top-1, R.Right, R.Bottom),
        ToolbarColor1, ToolbarColor2, not Horz);
    { Gripper }
    R2 := R;
    Offset := DragHandleOffsets[BtnVisible, ToolbarInfo.DragHandleStyle];
    if Horz then
    begin
      Inc(R2.Left, Offset + Ord(IsMenuBar));
      InflateRectVert(R2, -(Sz div 2));
      if IsMenuBar then Inc(R2.Top, 2);
      if BtnVisible then Inc(R2.Top, Sz-2);
    end
    else begin
      Inc(R2.Top, Offset + Ord(IsMenuBar));
      InflateRectHorz(R2, -(Sz div 2));
      if IsMenuBar then Inc(R2.Left, 2);
      if BtnVisible then Dec(R2.Right, Sz-2);
    end;
    DrawDots(DC, R2, DragHandleColor2, not Horz);
    OffsetRect(R2, -1, -1);
    DrawDots(DC, R2, DragHandleColor1, not Horz);
  end;
  { Close button }
  if BtnVisible then
  begin
    R2 := R;
    if Horz then
    begin
      Dec(R2.Right);
      R2.Bottom := R2.Top + R2.Right - R2.Left;
    end
    else begin
      Dec(R2.Bottom);
      R2.Left := R2.Right - R2.Bottom + R2.Top;
    end;
    BtnItemState := GetCloseBtnItemState(ToolbarInfo.CloseButtonState);
    {$IFDEF SMALL_CLOSE_BUTTON_ON_DOCKED_TOOLBAR}
    DrawCloseButton(DC, R2, BtnItemState, True, True, BtnItemColors[BtnItemState, ipText]);
    {$ELSE}
    DrawCloseButton(DC, R2, BtnItemState, True, False, BtnItemColors[BtnItemState, ipText]);
    {$ENDIF}
  end;
end;

procedure TTBXOffice2003Theme.PaintStatusBar(Canvas: TCanvas; R: TRect; Part: Integer);

  procedure DrawSizeGrip(DC: HDC; const P: TPoint; Color: TColor);
  var
    R: TRect;
    Brush: HBRUSH;
    X, Y: Integer;
  begin
    R := Rect(P.X-2, P.Y-2, P.X, P.Y);
    Brush := CreateBrushEx(Color);
    for X := 3 downto 1 do
    begin
      for Y := X downto 1 do
      begin
        FillRect(DC, R, Brush);
        OffsetRectVert(R, -4);
      end;
      OffsetRect(R, -4, 4 * X);
    end;
    DeleteObject(Brush);
  end;

var
  DC: HDC;
begin
  DC := Canvas.Handle;
  case Part of
    SBP_BODY:
      {$IFDEF GRADIENT_FILL_STATUSBAR}
      PaintIrregularGradient(DC, R, ToolbarColor1, ToolbarColor2, False);
      {$ELSE}
      FillRectEx(DC, R, FFaceColor);
      {$ENDIF}
    SBP_PANE, SBP_LASTPANE:
      begin
        if Part = SBP_PANE then Dec(R.Right, 2);
        FrameRectEx(DC, R, StatusPanelFrameColor, False);
      end;
    SBP_GRIPPER:
      begin
        OffsetPoint(R.BottomRight, -1, -1);
        DrawSizeGrip(DC, R.BottomRight, DragHandleColor2);
        OffsetPoint(R.BottomRight, -1, -1);
        DrawSizeGrip(DC, R.BottomRight, DragHandleColor1);
      end;
  end;
end;

{------------------------------------------------------------------------------}
{ Internal }
{------------------------------------------------------------------------------}

procedure TTBXOffice2003Theme.DrawCloseButton(DC: HDC; R: TRect;
  BtnItemState: TBtnItemState; Gradient, Small: Boolean; Color: TColor);
const
  CloseBtnPattern: array[Boolean, 0..15] of Byte = (
    (0, 0, $C6, 0, $6C, 0, $38, 0, $38, 0, $6C, 0, $C6, 0, 0, 0),
    ($C3, 0, $66, 0, $3C, 0, $18, 0, $3C, 0, $66, 0, $C3, 0, 0, 0));
var
  FrameColor, BodyColor: TColor;
begin
  FrameColor := BtnItemColors[BtnItemState, ipFrame];
  BodyColor := BtnBodyColors[BtnItemState, False];
  if (FrameColor <> clNone) and (BodyColor <> clNone) then
    if Gradient then
    begin
      FrameRectEx(DC, R, FrameColor, True);
      PaintGradient(DC, R, BodyColor, BtnBodyColors[BtnItemState, True], False);
    end
    else RectangleEx(DC, R, FrameColor, BodyColor);
  DrawGlyph(DC, R, 8, 8, CloseBtnPattern[not Small], Color);
end;

function TTBXOffice2003Theme.GetBtnColor(const ItemInfo: TTBXItemInfo;
  ItemPart: TItemPart; GradColor2: Boolean = False): TColor;
const
  BFlags1: array[Boolean] of TBtnItemState =
    (bisDisabled, bisDisabledHot);
  BFlags2: array[Boolean, Boolean] of TBtnItemState =
    ((bisNormal, bisHot), (bisSelected, bisSelectedHot));
var
  B: TBtnItemState;
  Embedded: Boolean;
begin
  with ItemInfo do
  begin
    if not Enabled then B := BFlags1[HoverKind = hkKeyboardHover] else
    if IsPopupParent then B := bisPopupParent else
    if Pushed then B := bisPressed else
      B := BFlags2[Selected, HoverKind <> hkNone];

    if ItemPart = ipBody
      then Result := BtnBodyColors[B, GradColor2]
      else Result := BtnItemColors[B, ItemPart];

    Embedded := ((ViewType and VT_TOOLBAR) <> 0) and
      ((ViewType and TVT_EMBEDDED) <> 0);
    if Embedded and (Result = clNone) then
    begin
      if ItemPart = ipBody
        then Result := EmbeddedColor
        else if (ItemPart = ipFrame) and not Selected then Result := EmbeddedFrameColor;
    end;
  end;
end;

procedure TTBXOffice2003Theme.GetGradientColors(X1, X2: Integer;
  out Color1, Color2: TColor);

  function CalcColor(XPos, FullWidth: Integer): TColor;
  var
    Weight1: Integer;
  begin
    Weight1 := XPos * 1000 div FullWidth;
    if Weight1 < 100 then Weight1 := Weight1 * 100 div 1000;
    Result := NearestBlendedColor(DockColor2, DockColor1, Weight1);
  end;

var
  FullWidth: Integer;
begin
  FullWidth := Max(FMaxWindowWidth, X2 - X1);
  Color1 := CalcColor(X1, FullWidth);
  Color2 := CalcColor(X2, FullWidth);
end;

function TTBXOffice2003Theme.GetPartColor(const ItemInfo: TTBXItemInfo;
  ItemPart: TItemPart): TColor;
const
  MFlags1: array[Boolean] of TMenuItemState = (misDisabled, misDisabledHot);
  MFlags2: array[Boolean] of TMenuItemState = (misNormal, misHot);
var
  M: TMenuItemState;
begin
  with ItemInfo do
  begin
    if ((ViewType and PVT_POPUPMENU) = PVT_POPUPMENU) and
      ((ItemOptions and IO_TOOLBARSTYLE) = 0) then
    begin
      if not Enabled
        then M := MFlags1[HoverKind = hkKeyboardHover]
        else M := MFlags2[HoverKind <> hkNone];
      Result := MenuItemColors[M, ItemPart];
    end
    else Result := GetBtnColor(ItemInfo, ItemPart, False);
  end;
end;

procedure TTBXOffice2003Theme.SetupColorCache;
const
  Office2003Colors: array[osBlue..osGreen, 0..29] of TColor = (
    ($F5BE9E, $FEECDD, $E2A981, $9C613B, $FAD5BD, $CB8C6A, $FFF9F1, $764127,
     $FFFFFF, $F0C7A9, $B99D7F, $DEDEDE, $F6F6F6, $962D00, $000000, $8D8D8D,
     $800000, $C9662A, $8CD5FF, $55ADFF, $4E91FE, $8ED3FF, $CCF4FF, $91D0FF,
     $FFEFE3, $E7B593, $C2EEFF, $AE9990, $F9DAC3, $FEECDD),
    ($E5D7D7, $FAF4F3, $B59799, $947C7C, $EFE5E5, $8F6D6E, $FFFFFF, $755454,
     $FFFFFF, $D3C0C0, $B2ACA5, $DEDEDE, $FFFAFD, $947C7C, $000000, $8D8D8D,
     $6F4B4B, $99797A, $8CD5FF, $55ADFF, $4E91FE, $8ED3FF, $CCF4FF, $91D0FF,
     $F1E9E8, $CDB9BA, $C2EEFF, $B39A9B, $F7F3F3, $F4EEEE),
    ($A7D9D9, $DEF7F4, $91C6B7, $588060, $C0E7E5, $588060, $DEF7F4, $335E51,
     $FFFFFF, $9FD4C5, $7FB9A4, $DEDEDE, $EEF4F4, $5E8D75, $000000, $8D8D8D,
     $385D3F, $5E8674, $8CD5FF, $55ADFF, $4E91FE, $8ED3FF, $CCF4FF, $91D0FF,
     $D5F0EC, $9FCEC2, $C2EEFF, $7BA097, $E4F0F2, $E7F2F3)
  );
  IrregularGradientColor1Weight: array[TOffice2003Scheme] of Integer =
    (196, 196, 36, 127);
  IrregularGradientCCExtrusion: array[TOffice2003Scheme] of Integer =
    (5, 5, 16, 16);

var
  DC: HDC;
  C, MenuItemFrame, EnabledText, DisabledText, HotBtnFace: TColor;
  Scheme: TOffice2003Scheme;

  procedure Undither(var C: TColor);
  begin
    if C <> clNone then
    begin
      if C < 0 then C := GetSysColor(C and $FF);
      C := GetNearestColor(DC, C);
    end;
  end;

  procedure ColorToColorRef(var C: TColor);
  begin
    if C < 0 then C := GetSysColor(C and $FF);
  end;

begin
  FMaxWindowWidth := GetSystemMetrics(SM_CXSCREEN);
  DC := StockCompatibleBitmap.Canvas.Handle;

  if TBXLoColor then
  begin
    DockColor1            := clBtnFace;
    DockColor2            := clBtnFace;
    ToolbarColor1         := clBtnFace;
    ToolbarColor2         := clBtnFace;
    ToolbarFrameColor1    := clBtnShadow;
    ToolbarFrameColor2    := clBtnShadow;
    SeparatorColor1       := clBtnShadow;
    SeparatorColor2       := clBtnHighlight;
    DragHandleColor1      := clBtnText;
    DragHandleColor2      := clBtnHighlight;

    EmbeddedColor         := clBtnFace;
    EmbeddedFrameColor    := clBtnShadow;
    EmbeddedDisabledColor := clBtnFace;

    PopupColor            := clWindow;
    PopupFrameColor       := clBtnText;

    DockPanelColor        := clWindow;

    DisabledText          := clBtnShadow;
    MenuItemFrame         := clHighlight;

    WinFrameColors[wfpBorder]      := clBtnShadow;
    WinFrameColors[wfpCaption]     := clBtnShadow;
    WinFrameColors[wfpCaptionText] := clBtnHighlight;

    BtnItemColors[bisNormal, ipText]        := clBtnText;
    BtnItemColors[bisNormal, ipFrame]       := clNone;
    BtnItemColors[bisDisabled, ipText]      := DisabledText;
    BtnItemColors[bisDisabled, ipFrame]     := clNone;
    BtnItemColors[bisSelected, ipText]      := clWindowText;
    BtnItemColors[bisSelected, ipFrame]     := MenuItemFrame;
    BtnItemColors[bisPressed, ipText]       := clHighlightText;
    BtnItemColors[bisPressed, ipFrame]      := MenuItemFrame;
    BtnItemColors[bisHot, ipText]           := clWindowText;
    BtnItemColors[bisHot, ipFrame]          := MenuItemFrame;
    BtnItemColors[bisDisabledHot, ipText]   := DisabledText;
    BtnItemColors[bisDisabledHot, ipFrame]  := MenuItemFrame;
    BtnItemColors[bisSelectedHot, ipText]   := clHighlightText;
    BtnItemColors[bisSelectedHot, ipFrame]  := MenuItemFrame;
    BtnItemColors[bisPopupParent, ipText]   := clBtnText;
    BtnItemColors[bisPopupParent, ipFrame]  := PopupFrameColor;

    BtnBodyColors[bisNormal, False]         := clNone;
    BtnBodyColors[bisNormal, True]          := clNone;
    BtnBodyColors[bisDisabled, False]       := clNone;
    BtnBodyColors[bisDisabled, True]        := clNone;
    BtnBodyColors[bisSelected, False]       := clWindow;
    BtnBodyColors[bisSelected, True]        := clWindow;
    BtnBodyColors[bisPressed, False]        := clHighlight;
    BtnBodyColors[bisPressed, True]         := clHighlight;
    BtnBodyColors[bisHot, False]            := clWindow;
    BtnBodyColors[bisHot, True]             := clWindow;
    BtnBodyColors[bisDisabledHot, False]    := clWindow;
    BtnBodyColors[bisDisabledHot, True]     := clWindow;
    BtnBodyColors[bisSelectedHot, False]    := clHighlight;
    BtnBodyColors[bisSelectedHot, True]     := clHighlight;
    BtnBodyColors[bisPopupParent, False]    := clBtnFace;
    BtnBodyColors[bisPopupParent, True]     := clBtnFace;

    MenuItemColors[misNormal, ipBody]       := clNone;
    MenuItemColors[misNormal, ipText]       := clWindowText;
    MenuItemColors[misNormal, ipFrame]      := clNone;
    MenuItemColors[misDisabled, ipBody]     := clNone;
    MenuItemColors[misDisabled, ipText]     := clGrayText;
    MenuItemColors[misDisabled, ipFrame]    := clNone;
    MenuItemColors[misHot, ipBody]          := clWindow;
    MenuItemColors[misHot, ipText]          := clWindowText;
    MenuItemColors[misHot, ipFrame]         := MenuItemFrame;
    MenuItemColors[misDisabledHot, ipBody]  := PopupColor;
    MenuItemColors[misDisabledHot, ipText]  := clGrayText;
    MenuItemColors[misDisabledHot, ipFrame] := MenuItemFrame;

    StatusPanelFrameColor := clBtnShadow;
    FMDIAreaColor := clBtnShadow;
    FFaceColor := clBtnFace;
    FWindowColor := clWindow;

    ColorToColorRef(FFaceColor);
    ColorToColorRef(FWindowColor);
  end
  else begin
    Scheme := GetOffice2003Scheme;
    if Scheme <> osAuto then
    begin
      DockColor1            := Office2003Colors[Scheme, 0];
      DockColor2            := Office2003Colors[Scheme, 28];
      ToolbarColor1         := Office2003Colors[Scheme, 1];
      ToolbarColor2         := Office2003Colors[Scheme, 2];
      ToolbarFrameColor1    := Office2003Colors[Scheme, 3];
      ToolbarFrameColor2    := Office2003Colors[Scheme, 4];
      SeparatorColor1       := Office2003Colors[Scheme, 5];
      SeparatorColor2       := Office2003Colors[Scheme, 6];
      DragHandleColor1      := Office2003Colors[Scheme, 7];
      DragHandleColor2      := Office2003Colors[Scheme, 8];

      EmbeddedColor         := Office2003Colors[Scheme, 9];
      EmbeddedFrameColor    := Office2003Colors[Scheme, 10];
      EmbeddedDisabledColor := Office2003Colors[Scheme, 11];

      PopupColor            := Office2003Colors[Scheme, 12];
      PopupFrameColor       := Office2003Colors[Scheme, 13];

      DockPanelColor        := Office2003Colors[Scheme, 29];

      EnabledText           := Office2003Colors[Scheme, 14];
      DisabledText          := Office2003Colors[Scheme, 15];
      MenuItemFrame         := Office2003Colors[Scheme, 16];

      WinFrameColors[wfpBorder]      := Office2003Colors[Scheme, 17];
      WinFrameColors[wfpCaption]     := WinFrameColors[wfpBorder];
      WinFrameColors[wfpCaptionText] := clWhite;

      BtnItemColors[bisNormal, ipText]        := EnabledText;
      BtnItemColors[bisNormal, ipFrame]       := clNone;
      BtnItemColors[bisDisabled, ipText]      := DisabledText;
      BtnItemColors[bisDisabled, ipFrame]     := clNone;
      BtnItemColors[bisSelected, ipText]      := EnabledText;
      BtnItemColors[bisSelected, ipFrame]     := MenuItemFrame;
      BtnItemColors[bisPressed, ipText]       := EnabledText;
      BtnItemColors[bisPressed, ipFrame]      := MenuItemFrame;
      BtnItemColors[bisHot, ipText]           := EnabledText;
      BtnItemColors[bisHot, ipFrame]          := MenuItemFrame;
      BtnItemColors[bisDisabledHot, ipText]   := DisabledText;
      BtnItemColors[bisDisabledHot, ipFrame]  := MenuItemFrame;
      BtnItemColors[bisSelectedHot, ipText]   := EnabledText;
      BtnItemColors[bisSelectedHot, ipFrame]  := MenuItemFrame;
      BtnItemColors[bisPopupParent, ipText]   := EnabledText;
      BtnItemColors[bisPopupParent, ipFrame]  := PopupFrameColor;

      BtnBodyColors[bisNormal, False]         := clNone;
      BtnBodyColors[bisNormal, True]          := clNone;
      BtnBodyColors[bisDisabled, False]       := clNone;
      BtnBodyColors[bisDisabled, True]        := clNone;
      BtnBodyColors[bisSelected, False]       := Office2003Colors[Scheme, 18];
      BtnBodyColors[bisSelected, True]        := Office2003Colors[Scheme, 19];
      BtnBodyColors[bisPressed, False]        := Office2003Colors[Scheme, 20];
      BtnBodyColors[bisPressed, True]         := Office2003Colors[Scheme, 21];
      BtnBodyColors[bisHot, False]            := Office2003Colors[Scheme, 22];
      BtnBodyColors[bisHot, True]             := Office2003Colors[Scheme, 23];
      BtnBodyColors[bisDisabledHot, False]    := BtnBodyColors[bisHot, False];
      BtnBodyColors[bisDisabledHot, True]     := BtnBodyColors[bisHot, True];
      BtnBodyColors[bisSelectedHot, False]    := BtnBodyColors[bisPressed, False];
      BtnBodyColors[bisSelectedHot, True]     := BtnBodyColors[bisPressed, True];
      BtnBodyColors[bisPopupParent, False]    := Office2003Colors[Scheme, 24];
      BtnBodyColors[bisPopupParent, True]     := Office2003Colors[Scheme, 25];

      MenuItemColors[misNormal, ipBody]       := clNone;
      MenuItemColors[misNormal, ipText]       := EnabledText;
      MenuItemColors[misNormal, ipFrame]      := clNone;
      MenuItemColors[misDisabled, ipBody]     := clNone;
      MenuItemColors[misDisabled, ipText]     := DisabledText;
      MenuItemColors[misDisabled, ipFrame]    := clNone;
      MenuItemColors[misHot, ipBody]          := Office2003Colors[Scheme, 26];
      MenuItemColors[misHot, ipText]          := MenuItemColors[misNormal, ipText];
      MenuItemColors[misHot, ipFrame]         := MenuItemFrame;
      MenuItemColors[misDisabledHot, ipBody]  := PopupColor;
      MenuItemColors[misDisabledHot, ipText]  := DisabledText;
      MenuItemColors[misDisabledHot, ipFrame] := MenuItemColors[misHot, ipFrame];

      {$IFDEF GRADIENT_FILL_STATUSBAR}
      StatusPanelFrameColor := Blend(ToolbarColor2, clBlack, 90);
      {$ELSE}
      StatusPanelFrameColor := Blend(clBtnFace, clBlack, 89);
      {$ENDIF}
      FMDIAreaColor := Office2003Colors[Scheme, 27];
    end
    else begin { osAuto }
      DockColor1            := clBtnFace;
      DockColor2            := Blend(clBtnFace, clWindow, 222);
      ToolbarColor1         := clWindow;
      ToolbarColor2         := Blend(clBtnShadow, clBtnFace, 48);
      ToolbarFrameColor1    := Blend(clBtnShadow, clWindow, 85);
      ToolbarFrameColor2    := Blend(clBtnFace, clWindow, 62);
      SeparatorColor1       := Blend(clBtnShadow, clWindow, 70);
      SeparatorColor2       := clWhite;

      DragHandleColor1 := Blend(clBtnShadow, clWindow, 75);
      SetContrast(DragHandleColor1, Blend(ToolbarColor1, ToolbarColor2, 127), 180);
      DragHandleColor2 := clWindow;

      EmbeddedColor         := clBtnFace;
      EmbeddedFrameColor    := clBtnShadow;
      EmbeddedDisabledColor := clBtnFace;

      PopupColor            := Blend(clBtnFace, clWindow, 15);
      PopupFrameColor       := Blend(clBtnText, clBtnShadow, 20);

      DockPanelColor        := Blend(clBtnFace, clWindow, 50);

      C := Blend(clWindow, clBtnFace, 165);
      EnabledText := clBtnText;
      SetContrast(EnabledText, C, 180);
      DisabledText := Blend(clBtnshadow, clWindow, 90);

      MenuItemFrame := clHighlight;

      HotBtnFace := Blend(clHighlight, clWindow, 30);
      SetContrast(HotBtnFace, Blend(clWindow, clBtnFace, 165), 50);

      WinFrameColors[wfpBorder]               := Blend(clBtnText, clBtnShadow, 15);
      WinFrameColors[wfpCaption]              := clBtnShadow;
      WinFrameColors[wfpCaptionText]          := clBtnHighlight;

      BtnBodyColors[bisNormal, False]         := clNone;
      BtnBodyColors[bisNormal, True]          := clNone;
      BtnBodyColors[bisDisabled, False]       := clNone;
      BtnBodyColors[bisDisabled, True]        := clNone;
      BtnBodyColors[bisSelected, False]       := Blend(clHighlight, Blend(clBtnFace, clWindow, 50), 10);

      BtnItemColors[bisNormal, ipText]        := EnabledText;
      BtnItemColors[bisNormal, ipFrame]       := clNone;
      BtnItemColors[bisDisabled, ipText]      := DisabledText;
      BtnItemColors[bisDisabled, ipFrame]     := clNone;
      BtnItemColors[bisSelected, ipText]      := EnabledText;
      BtnItemColors[bisSelected, ipFrame]     := MenuItemFrame;
      BtnItemColors[bisPressed, ipText]       := EnabledText;
      BtnItemColors[bisPressed, ipFrame]      := MenuItemFrame;
      BtnItemColors[bisHot, ipText]           := EnabledText;
      SetContrast(BtnItemColors[bisHot, ipText], HotBtnFace, 180);
      BtnItemColors[bisHot, ipFrame]          := MenuItemFrame;
      BtnItemColors[bisDisabledHot, ipText]   := DisabledText;
      BtnItemColors[bisDisabledHot, ipFrame]  := MenuItemFrame;
      BtnItemColors[bisSelectedHot, ipText]   := EnabledText;
      BtnItemColors[bisSelectedHot, ipFrame]  := MenuItemFrame;
      BtnItemColors[bisPopupParent, ipText]   := EnabledText;
      BtnItemColors[bisPopupParent, ipFrame]  := PopupFrameColor;

      BtnBodyColors[bisSelected, True]        := BtnBodyColors[bisSelected, False];
      BtnBodyColors[bisPressed, False]        := Blend(clHighlight, clWindow, 50);
      BtnBodyColors[bisPressed, True]         := BtnBodyColors[bisPressed, False];
      BtnBodyColors[bisHot, False]            := HotBtnFace;
      BtnBodyColors[bisHot, True]             := HotBtnFace;
      BtnBodyColors[bisDisabledHot, False]    := BtnBodyColors[bisHot, False];
      BtnBodyColors[bisDisabledHot, True]     := BtnBodyColors[bisHot, True];
      BtnBodyColors[bisSelectedHot, False]    := BtnBodyColors[bisPressed, False];
      BtnBodyColors[bisSelectedHot, True]     := BtnBodyColors[bisPressed, True];
      BtnBodyColors[bisPopupParent, False]    := Blend(clBtnFace, clWindow, 16);
      BtnBodyColors[bisPopupParent, True]     := Blend(clBtnFace, clWindow, 42);

      MenuItemColors[misNormal, ipBody]       := clNone;
      MenuItemColors[misNormal, ipText]       := EnabledText;
      MenuItemColors[misNormal, ipFrame]      := clNone;
      MenuItemColors[misDisabled, ipBody]     := clNone;
      MenuItemColors[misDisabled, ipText]     := DisabledText;
      MenuItemColors[misDisabled, ipFrame]    := clNone;
      MenuItemColors[misHot, ipBody]          := BtnBodyColors[bisHot, False];
      MenuItemColors[misHot, ipText]          := EnabledText;
      SetContrast(MenuItemColors[misHot, ipText], HotBtnFace, 180);
      MenuItemColors[misHot, ipFrame]         := MenuItemFrame;
      MenuItemColors[misDisabledHot, ipBody]  := PopupColor;
      MenuItemColors[misDisabledHot, ipText]  := DisabledText;
      MenuItemColors[misDisabledHot, ipFrame] := MenuItemColors[misHot, ipFrame];

      {$IFDEF GRADIENT_FILL_STATUSBAR}
      StatusPanelFrameColor := Blend(ToolbarColor2, clBlack, 90);
      {$ELSE}
      StatusPanelFrameColor := Blend(clBtnFace, clBlack, 89);
      {$ENDIF}
      SetContrast(StatusPanelFrameColor, clBtnFace, 30);
      FMDIAreaColor := clBtnShadow;
    end;
    FFaceColor := clBtnFace;
    FWindowColor := clWindow;

    Undither(DockColor1);
    Undither(DockColor2);
    Undither(ToolbarColor1);
    Undither(ToolbarColor2);
    Undither(ToolbarFrameColor1);
    Undither(ToolbarFrameColor2);
    Undither(SeparatorColor1);
    Undither(SeparatorColor2);
    Undither(DragHandleColor1);
    Undither(DragHandleColor2);

    Undither(EmbeddedColor);
    Undither(EmbeddedFrameColor);
    Undither(EmbeddedDisabledColor);

    Undither(PopupColor);
    Undither(PopupFrameColor);

    Undither(DockPanelColor);

    Undither(WinFrameColors[wfpBorder]);
    Undither(WinFrameColors[wfpCaption]);
    Undither(WinFrameColors[wfpCaptionText]);

    Undither(BtnItemColors[bisNormal, ipText]);
    Undither(BtnItemColors[bisNormal, ipFrame]);
    Undither(BtnItemColors[bisDisabled, ipText]);
    Undither(BtnItemColors[bisDisabled, ipFrame]);
    Undither(BtnItemColors[bisSelected, ipText]);
    Undither(BtnItemColors[bisSelected, ipFrame]);
    Undither(BtnItemColors[bisPressed, ipText]);
    Undither(BtnItemColors[bisPressed, ipFrame]);
    Undither(BtnItemColors[bisHot, ipText]);
    Undither(BtnItemColors[bisHot, ipFrame]);
    Undither(BtnItemColors[bisDisabledHot, ipText]);
    Undither(BtnItemColors[bisDisabledHot, ipFrame]);
    Undither(BtnItemColors[bisSelectedHot, ipText]);
    Undither(BtnItemColors[bisSelectedHot, ipFrame]);
    Undither(BtnItemColors[bisPopupParent, ipText]);
    Undither(BtnItemColors[bisPopupParent, ipFrame]);

    Undither(BtnBodyColors[bisNormal, False]);
    Undither(BtnBodyColors[bisNormal, True]);
    Undither(BtnBodyColors[bisDisabled, False]);
    Undither(BtnBodyColors[bisDisabled, True]);
    Undither(BtnBodyColors[bisSelected, False]);
    Undither(BtnBodyColors[bisSelected, True]);
    Undither(BtnBodyColors[bisPressed, False]);
    Undither(BtnBodyColors[bisPressed, True]);
    Undither(BtnBodyColors[bisHot, False]);
    Undither(BtnBodyColors[bisHot, True]);
    Undither(BtnBodyColors[bisDisabledHot, False]);
    Undither(BtnBodyColors[bisDisabledHot, True]);
    Undither(BtnBodyColors[bisSelectedHot, False]);
    Undither(BtnBodyColors[bisSelectedHot, True]);
    Undither(BtnBodyColors[bisPopupParent, False]);
    Undither(BtnBodyColors[bisPopupParent, True]);

    Undither(MenuItemColors[misNormal, ipBody]);
    Undither(MenuItemColors[misNormal, ipText]);
    Undither(MenuItemColors[misNormal, ipFrame]);
    Undither(MenuItemColors[misDisabled, ipBody]);
    Undither(MenuItemColors[misDisabled, ipText]);
    Undither(MenuItemColors[misDisabled, ipFrame]);
    Undither(MenuItemColors[misHot, ipBody]);
    Undither(MenuItemColors[misHot, ipText]);
    Undither(MenuItemColors[misHot, ipFrame]);
    Undither(MenuItemColors[misDisabledHot, ipBody]);
    Undither(MenuItemColors[misDisabledHot, ipText]);
    Undither(MenuItemColors[misDisabledHot, ipFrame]);

    Undither(StatusPanelFrameColor);
    Undither(FMDIAreaColor);
    Undither(FFaceColor);
    Undither(FWindowColor);

    IrregularGradientCenterColor :=
      Lighten(MixColors(ToolbarColor1, ToolbarColor2,
        IrregularGradientColor1Weight[Scheme]),
        IrregularGradientCCExtrusion[Scheme]);
    Undither(IrregularGradientCenterColor);
  end;
end;

procedure TTBXOffice2003Theme.TBXSysCommand(var Message: TMessage);
begin
  if Message.WParam = TSC_VIEWCHANGE then SetupColorCache;
end;

initialization
  RegisterTBXTheme('Office 2003', TTBXOffice2003Theme);
end.
