{-----------------------------------------------------------------------------
 Unit Name: VCL.Styles.PyScripter
 Author:    PyScripter
 Date:      25-Dec-2017
 Purpose:   Selectively use features of Vcl-Styles-utils project.
 History:

 The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
 you may not use this file except in compliance with the License. You may obtain a copy of the
 License at http://www.mozilla.org/MPL/

 Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
 ANY KIND, either express or implied. See the License for the specific language governing rights
 and limitations under the License.

 The Original Code is Vcl.Styles.Hooks.pas.

 The Initial Developer of the Original Code is Rodrigo Ruz V.

 Portions created by Rodrigo Ruz V. are Copyright (C) 2013-2017 Rodrigo Ruz V.

 Contributor(s): Mahdi Safsafi.

 All Rights Reserved.
-----------------------------------------------------------------------------}

unit VCL.Styles.PyScripter;

interface
uses
  WinApi.Windows,
  Winapi.UxTheme,
  System.Types,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Themes;

type
  /// <summary> The <c>TVclStylesPreview</c> class, is a control for display  a preview of any Vcl style loaded
  /// </summary>
  /// <remarks>
  /// sample of use
  /// <code>
  /// var <para></para>
  ///   StyleName : string;<para></para>
  ///   SourceInfo: TSourceInfo;<para></para>
  ///   LStyle    : TCustomStyleServices;<para></para>
  ///   FPreview  : TVclStylesPreview;<para></para>
  /// begin<para></para>
  ///    FPreview:=TVclStylesPreview.Create(Self);<para></para>
  ///    FPreview.Parent:=PanelPreview;<para></para>
  ///    FPreview.BoundsRect := PanelPreview.ClientRect;<para></para>
  ///    StyleName:='Carbon';<para></para>
  ///    if (StyleName &lt;&gt;'') and (not SameText(StyleName, 'Windows')) then<para></para>
  ///    begin<para></para>
  ///      TStyleManager.StyleNames;//call DiscoverStyleResources<para></para>
  ///      LStyle:=TStyleManager.Style[StyleName];<para></para>
  ///      FPreview.Caption:=StyleName;<para></para>
  ///      FPreview.Style:=LStyle;<para></para>
  ///      TVclStylesPreviewClass(FPreview).Paint;<para></para>
  ///    end;<para></para>
  ///    ....<para></para>
  /// end;<para></para>
  /// </code>
  /// </remarks>
  TVclStylesPreview = class(TCustomControl)
  private
    FStyle: TCustomStyleServices;//TCustomStyle;
    FIcon: HICON;
    FCaption: TCaption;
    FRegion : HRGN;
    FBitmap: TBitmap;
    FMenuHeight: Integer;
    FMenuCaptionWidth: Integer;
    FButtonWidth: Integer;
    FButtonHeight: Integer;
    FMargin: Integer;
  protected
    procedure Paint; override;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
  public
    property Icon:HICON read FIcon Write FIcon;
    property Style:TCustomStyleServices read FStyle Write FStyle;
    property Caption : TCaption read FCaption write FCaption;
    property BitMap : TBitmap read FBitmap write FBitmap;
    constructor Create(AControl: TComponent); override;
    destructor Destroy; override;
  end;


implementation
uses
  System.SysUtils,
  Vcl.Forms,
  SynEdit;

{ TVclStylePreview }

procedure TVclStylesPreview.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  FMenuHeight := MulDiv(FMenuHeight, M, D);
  FMenuCaptionWidth := MulDiv(FMenuCaptionWidth, M, D);
  FButtonWidth := MulDiv(FButtonWidth, M, D);
  FButtonHeight := MulDiv(FButtonHeight, M, D);
  FMargin := MulDiv(FMargin, M, D);

  inherited;
end;

constructor TVclStylesPreview.Create(AControl: TComponent);
begin
  inherited;
  FRegion := 0;
  FStyle:=nil;
  FCaption:='';
  FIcon:=0;

  FMenuHeight := 20;
  FMenuCaptionWidth := 30;
  FButtonWidth := 75;
  FButtonHeight := 25;
  FMargin := 10;

  FBitmap:=TBitmap.Create;
  FBitmap.PixelFormat:=pf32bit;
end;

destructor TVclStylesPreview.Destroy;
begin
  if FRegion <> 0 then
  begin
    DeleteObject(FRegion);
    FRegion := 0;
  end;
  FBitmap.Free;
  inherited;
end;

procedure TVclStylesPreview.Paint;
var
  LDetails        : TThemedElementDetails;
  CaptionDetails  : TThemedElementDetails;
  IconDetails     : TThemedElementDetails;
  IconRect        : TRect;
  BorderRect      : TRect;
  CaptionRect     : TRect;
  ButtonRect      : TRect;
  TextRect        : TRect;
  CaptionBitmap   : TBitmap;
  //LBitmap         : TBitmap;
  ThemeTextColor  : TColor;
  ARect           : TRect;
  LRect           : TRect;
  //BlendFunction   : TBlendFunction;
  LRegion         : HRgn;
  i               : Integer;

    function GetBorderSize: TRect;
    var
      Size: TSize;
      Details: TThemedElementDetails;
      Detail: TThemedWindow;
    begin
      Result  := Rect(0, 0, 0, 0);
      Detail  := twCaptionActive;
      Details := Style.GetElementDetails(Detail);
      Style.GetElementSize(0, Details, esActual, Size);
      Result.Top := Size.cy;
      Detail := twFrameLeftActive;
      Details := Style.GetElementDetails(Detail);
      Style.GetElementSize(0, Details, esActual, Size);
      Result.Left := Size.cx;
      Detail := twFrameRightActive;
      Details := Style.GetElementDetails(Detail);
      Style.GetElementSize(0, Details, esActual, Size);
      Result.Right := Size.cx;
      Detail := twFrameBottomActive;
      Details := Style.GetElementDetails(Detail);
      Style.GetElementSize(0, Details, esActual, Size);
      Result.Bottom := Size.cy;
    end;

    function RectVCenter(var R: TRect; Bounds: TRect): TRect;
    begin
      OffsetRect(R, -R.Left, -R.Top);
      OffsetRect(R, 0, (Bounds.Height - R.Height) div 2);
      OffsetRect(R, Bounds.Left, Bounds.Top);
      Result := R;
    end;

begin
  if FStyle=nil then Exit;

  BorderRect := GetBorderSize;
  ARect:=ClientRect;
  CaptionBitmap := TBitmap.Create;
  try
    CaptionBitmap.SetSize(ARect.Width, BorderRect.Top);
        {
    LBitmap:=TBitmap.Create;
    LBitmap.PixelFormat:=pf32bit;
    }
    FBitmap.SetSize(ClientRect.Width, ClientRect.Height);

    //Draw background
    LDetails.Element := teWindow;
    LDetails.Part := 0;
    Style.DrawElement(FBitmap.Canvas.Handle, LDetails, ARect);

    //Draw caption border
    CaptionRect := Rect(0, 0, CaptionBitmap.Width, CaptionBitmap.Height);
    LDetails := Style.GetElementDetails(twCaptionActive);

    LRegion := FRegion;
    try
      Style.GetElementRegion(LDetails, ARect, FRegion);
      SetWindowRgn(Handle, FRegion, True);
    finally
      if LRegion <> 0 then
        DeleteObject(LRegion);
    end;

         {
    Style.GetElementRegion(LDetails, ARect, Region);
    SetWindowRgn(Handle, Region, True);
        }

    Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, CaptionRect);
    TextRect := CaptionRect;
    CaptionDetails := LDetails;

    //Draw icon
    IconDetails := Style.GetElementDetails(twSysButtonNormal);
    if not Style.GetElementContentRect(0, IconDetails, CaptionRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);
    IconRect := Rect(0, 0, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON));
    RectVCenter(IconRect, ButtonRect);
    if ButtonRect.Width > 0 then
     if FIcon <> 0 then
      DrawIconEx(CaptionBitmap.Canvas.Handle, IconRect.Left, IconRect.Top, FIcon,
        IconRect.Right - IconRect.Left, IconRect.Bottom - IconRect.Top,
        0, 0, DI_NORMAL);

    Inc(TextRect.Left, ButtonRect.Width + 5);

    //Draw buttons

    //Close button
    LDetails := Style.GetElementDetails(twCloseButtonNormal);
    if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
     Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect);

    //Maximize button
    LDetails := Style.GetElementDetails(twMaxButtonNormal);
    if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
      Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect);

    //Minimize button
    LDetails := Style.GetElementDetails(twMinButtonNormal);

    if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
      Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect);

    //Help button
    LDetails := Style.GetElementDetails(twHelpButtonNormal);
    if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
      Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect);

    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;

    //Draw text
    Style.DrawText(CaptionBitmap.Canvas.Handle, CaptionDetails, FCaption, TextRect, [tfLeft, tfSingleLine, tfVerticalCenter]);

    //Draw caption
    FBitmap.Canvas.Draw(0, 0, CaptionBitmap);

  finally
    CaptionBitmap.Free;
  end;

  //Draw left border
  CaptionRect := Rect(0, BorderRect.Top, BorderRect.Left, ARect.Height - BorderRect.Bottom);
  LDetails := Style.GetElementDetails(twFrameLeftActive);
  if CaptionRect.Bottom - CaptionRect.Top > 0 then
    Style.DrawElement(FBitmap.Canvas.Handle, LDetails, CaptionRect);

  //Draw right border
  CaptionRect := Rect(ARect.Width - BorderRect.Right, BorderRect.Top, ARect.Width, ARect.Height - BorderRect.Bottom);
  LDetails := Style.GetElementDetails(twFrameRightActive);
  Style.DrawElement(FBitmap.Canvas.Handle, LDetails, CaptionRect);

  //Draw Bottom border
  CaptionRect := Rect(0, ARect.Height - BorderRect.Bottom, ARect.Width, ARect.Height);
  LDetails := Style.GetElementDetails(twFrameBottomActive);
  Style.DrawElement(FBitmap.Canvas.Handle, LDetails, CaptionRect);

  //Draw Main Menu
  LDetails:= Style.GetElementDetails(tmMenuBarBackgroundActive);
  LRect:=Rect(BorderRect.Left, BorderRect.Top+1, ARect.Width-BorderRect.Left,BorderRect.Top + 1 + FMenuHeight);
  Style.DrawElement(FBitmap.Canvas.Handle, LDetails, LRect);

  LDetails := Style.GetElementDetails(tmMenuBarItemNormal);
  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);

//    function DrawText(DC: HDC; Details: TThemedElementDetails;
//      const S: string; var R: TRect; Flags: TTextFormat; Color: TColor = clNone): Boolean; overload;
//    function DrawText(DC: HDC; Details: TThemedElementDetails;
//      const S: string; var R: TRect; Flags: TTextFormat; Options: TStyleTextOptions): Boolean; overload;

  CaptionRect := Rect(LRect.Left + FMargin,LRect.Top+3, LRect.Right ,LRect.Bottom);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'File', CaptionRect, [tfLeft], ThemeTextColor);
  CaptionRect := Rect(LRect.Left + FMargin + FMenuCaptionWidth,LRect.Top+3, LRect.Right ,LRect.Bottom);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'Edit', CaptionRect,  [tfLeft], ThemeTextColor);
  CaptionRect := Rect(LRect.Left + FMargin + FMenuCaptionWidth*2,LRect.Top+3, LRect.Right ,LRect.Bottom);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'View', CaptionRect,  [tfLeft], ThemeTextColor);
  CaptionRect := Rect(LRect.Left + FMargin + FMenuCaptionWidth*3,LRect.Top+3, LRect.Right ,LRect.Bottom);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'Help', CaptionRect,  [tfLeft], ThemeTextColor);


  //Draw ToolButtons
  for i := 1 to 3 do
  begin
    LDetails := Style.GetElementDetails(ttbButtonNormal);
    ButtonRect.Left:=BorderRect.Left+ FMargin div 2 + (i-1) * FButtonWidth;
    ButtonRect.Top:=LRect.Top+FMenuHeight + FMargin;
    ButtonRect.Width:= FButtonWidth;
    ButtonRect.Height:=FButtonHeight;
    Style.DrawElement(FBitmap.Canvas.Handle, LDetails, ButtonRect);

    Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
    Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'ToolButton'+IntToStr(i), ButtonRect, TTextFormatFlags(DT_VCENTER or DT_CENTER), ThemeTextColor);
  end;

  //Draw Normal
  LDetails := Style.GetElementDetails(tbPushButtonNormal);
  ButtonRect.Left:=BorderRect.Left + FMargin;
  ButtonRect.Top:=ARect.Height- 2 * FButtonHeight;
  ButtonRect.Width:=FButtonWidth;
  ButtonRect.Height:=FButtonHeight;
  Style.DrawElement(FBitmap.Canvas.Handle, LDetails, ButtonRect);

  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'Normal', ButtonRect, TTextFormatFlags(DT_VCENTER or DT_CENTER), ThemeTextColor);

  //Draw Hot
  LDetails := Style.GetElementDetails(tbPushButtonHot);
  ButtonRect.Left:=BorderRect.Left + 2 * FMargin + FButtonWidth;
  ButtonRect.Top:=ARect.Height - 2 * FButtonHeight;
  ButtonRect.Width:=FButtonWidth;
  ButtonRect.Height:=FButtonHeight;
  Style.DrawElement(FBitmap.Canvas.Handle, LDetails, ButtonRect);

  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'Hot', ButtonRect, TTextFormatFlags(DT_VCENTER or DT_CENTER), ThemeTextColor);

  //Draw Pressed
  LDetails := Style.GetElementDetails(tbPushButtonPressed);
  ButtonRect.Left:=BorderRect.Left+  3 * FMargin + 2 * FButtonWidth;
  ButtonRect.Top:=ARect.Height- 2 * FButtonHeight;
  ButtonRect.Width:=FButtonWidth;
  ButtonRect.Height:=FButtonHeight;
  Style.DrawElement(FBitmap.Canvas.Handle, LDetails, ButtonRect);

  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'Pressed', ButtonRect, TTextFormatFlags(DT_VCENTER or DT_CENTER), ThemeTextColor);

  //Draw Disabled
  LDetails := Style.GetElementDetails(tbPushButtonDisabled);
  ButtonRect.Left:=BorderRect.Left + 4 * FMargin + 3 * FButtonWidth;
  ButtonRect.Top:=ARect.Height-2*FButtonHeight;
  ButtonRect.Width:=FButtonWidth;
  ButtonRect.Height:=FButtonHeight;
  Style.DrawElement(FBitmap.Canvas.Handle, LDetails, ButtonRect);

  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'Disabled', ButtonRect, TTextFormatFlags(DT_VCENTER or DT_CENTER), ThemeTextColor);

  Canvas.Draw(0,0,FBitmap);
end;

initialization
  TCustomStyleEngine.RegisterStyleHook(TCustomSynEdit, TScrollingStyleHook);

finalization
  TCustomStyleEngine.UnRegisterStyleHook(TCustomSynEdit, TScrollingStyleHook);
end.
