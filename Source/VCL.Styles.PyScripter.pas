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
  FBitmap.Canvas.Font.Height := MulDiv(FBitmap.Canvas.Font.Height, M, D);

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
  FBitmap.Canvas.Font.Height := MulDiv(FBitmap.Canvas.Font.Height,
    Screen.DefaultPixelsPerInch, Screen.PixelsPerInch);
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
  DPI             : Integer;
  LDetails        : TThemedElementDetails;
  CaptionDetails  : TThemedElementDetails;
  IconDetails     : TThemedElementDetails;
  IconRect        : TRect;
  BorderRect      : TRect;
  CaptionRect     : TRect;
  ButtonRect      : TRect;
  TextRect        : TRect;
  CaptionBitmap   : TBitmap;
  ThemeTextColor  : TColor;
  ARect           : TRect;
  LRect           : TRect;
  //BlendFunction   : TBlendFunction;
  LRegion         : HRgn;
  i               : Integer;

    procedure ScaleLeftContentRect(var R: TRect);
    begin
      R.Top := MulDiv(R.Top, DPI, 96);
      R.Left := MulDiv(R.Left, DPI, 96);
      R.Right := MulDiv(R.Right, DPI, 96);
      R.Bottom := MulDiv(R.Bottom, DPI, 96);
    end;

    procedure ScaleRightContentRect(var R: TRect; const OutsideR: TRect);
      begin
      R.Top := MulDiv(R.Top, DPI, 96);
      R.Left := OutsideR.Right - MulDiv(OutsideR.Right - R.Left, DPI, 96);
      R.Right := OutsideR.Right - MulDiv(OutsideR.Right - R.Right, DPI, 96);
      R.Bottom := MulDiv(R.Bottom, DPI, 96);
    end;

    function GetBorderSize: TRect;
    var
      Size: TSize;
      Details: TThemedElementDetails;
      Detail: TThemedWindow;
    begin
      Result  := Rect(0, 0, 0, 0);
      Detail  := twCaptionActive;
      Details := Style.GetElementDetails(Detail);
      Style.GetElementSize(0, Details, esActual, Size, DPI);
      Result.Top := Size.cy;
      Detail := twFrameLeftActive;
      Details := Style.GetElementDetails(Detail);
      Style.GetElementSize(0, Details, esActual, Size, DPI);
      Result.Left := Size.cx;
      Detail := twFrameRightActive;
      Details := Style.GetElementDetails(Detail);
      Style.GetElementSize(0, Details, esActual, Size, DPI);
      Result.Right := Size.cx;
      Detail := twFrameBottomActive;
      Details := Style.GetElementDetails(Detail);
      Style.GetElementSize(0, Details, esActual, Size, DPI);
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

  DPI := CurrentPPI;
  BorderRect := GetBorderSize;
  ARect:=ClientRect;
  CaptionBitmap := TBitmap.Create;
  try
    CaptionBitmap.SetSize(ARect.Width, BorderRect.Top);
    FBitmap.SetSize(ClientRect.Width, ClientRect.Height);

    //Draw background
    LDetails.Element := teWindow;
    LDetails.Part := 0;
    Style.DrawElement(FBitmap.Canvas.Handle, LDetails, ARect, ARect, DPI);

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

    Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, CaptionRect, CaptionRect,DPI);
    TextRect := CaptionRect;
    CaptionDetails := LDetails;

    //Draw icon
    IconDetails := Style.GetElementDetails(twSysButtonNormal);
    if not Style.GetElementContentRect(0, IconDetails, CaptionRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);
    ScaleLeftContentRect(ButtonRect);
    IconRect := Rect(0, 0, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON));
    RectVCenter(IconRect, ButtonRect);
    if (ButtonRect.Width > 0) and (FIcon <> 0) then
      DrawIconEx(CaptionBitmap.Canvas.Handle, IconRect.Left, IconRect.Top, FIcon,
        IconRect.Right - IconRect.Left, IconRect.Bottom - IconRect.Top,
        0, 0, DI_NORMAL);

    Inc(TextRect.Left, ButtonRect.Width + MulDiv(5, DPI, 96));

    //Draw buttons

    //Close button
    LDetails := Style.GetElementDetails(twCloseButtonNormal);
    if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
    begin
      ScaleRightContentRect(ButtonRect, CaptionRect);
      Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect, ButtonRect,DPI);
    end;
    //Maximize button
    LDetails := Style.GetElementDetails(twMaxButtonNormal);
    if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then begin
      ScaleRightContentRect(ButtonRect, CaptionRect);
      Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect, ButtonRect,DPI);
    end;
    //Minimize button
    LDetails := Style.GetElementDetails(twMinButtonNormal);

    if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then begin
      ScaleRightContentRect(ButtonRect, CaptionRect);
      Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect,  ButtonRect,DPI);
    end;
    //Help button
    LDetails := Style.GetElementDetails(twHelpButtonNormal);
    if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then begin
      ScaleRightContentRect(ButtonRect, CaptionRect);
      Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect,  ButtonRect,DPI);
    end;

    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;

    //Draw text
    // The DPI parameter in DrawText is only used if the element is twWindow.
    // Otherwise is just ignored!  Below it takes effect and we do not need to
    // scale the font.
    Style.DrawText(CaptionBitmap.Canvas.Handle, CaptionDetails, FCaption,
      TextRect, [tfLeft, tfSingleLine, tfVerticalCenter], clNone, DPI);

    //Draw caption
    FBitmap.Canvas.Draw(0, 0, CaptionBitmap);

  finally
    CaptionBitmap.Free;
  end;

  //Draw left border
  CaptionRect := Rect(0, BorderRect.Top, BorderRect.Left, ARect.Height - BorderRect.Bottom);
  LDetails := Style.GetElementDetails(twFrameLeftActive);
  if CaptionRect.Bottom - CaptionRect.Top > 0 then
    Style.DrawElement(FBitmap.Canvas.Handle, LDetails, CaptionRect, CaptionRect, DPI);

  //Draw right border
  CaptionRect := Rect(ARect.Width - BorderRect.Right, BorderRect.Top, ARect.Width, ARect.Height - BorderRect.Bottom);
  LDetails := Style.GetElementDetails(twFrameRightActive);
  Style.DrawElement(FBitmap.Canvas.Handle, LDetails, CaptionRect, CaptionRect, DPI);

  //Draw Bottom border
  CaptionRect := Rect(0, ARect.Height - BorderRect.Bottom, ARect.Width, ARect.Height);
  LDetails := Style.GetElementDetails(twFrameBottomActive);
  Style.DrawElement(FBitmap.Canvas.Handle, LDetails, CaptionRect, CaptionRect, DPI);

  //Draw Main Menu
  LDetails := Style.GetElementDetails(tmMenuBarBackgroundActive);
  LRect:= Rect(BorderRect.Left, BorderRect.Top+1, ARect.Width-BorderRect.Left,BorderRect.Top + 1 + FMenuHeight);
  Style.DrawElement(FBitmap.Canvas.Handle, LDetails, LRect, LRect, DPI);

  LDetails := Style.GetElementDetails(tmMenuBarItemNormal);
  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  // For some reason menu text size is not scaled.  So we use toolbar painting instead.
  if FCurrentPPI <> Screen.PixelsPerInch then
    LDetails := Style.GetElementDetails(ttbButtonNormal);

  var MenuTop := LRect.Top + MulDiv(3, FCurrentPPI, 96);
  CaptionRect := Rect(LRect.Left + FMargin, MenuTop, LRect.Right ,LRect.Bottom);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'File', CaptionRect, [tfLeft], ThemeTextColor, DPI);
  CaptionRect := Rect(LRect.Left + FMargin + FMenuCaptionWidth, MenuTop, LRect.Right ,LRect.Bottom);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'Edit', CaptionRect,  [tfLeft], ThemeTextColor, DPI);
  CaptionRect := Rect(LRect.Left + FMargin + FMenuCaptionWidth * 2, MenuTop, LRect.Right ,LRect.Bottom);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'View', CaptionRect,  [tfLeft], ThemeTextColor, DPI);
  CaptionRect := Rect(LRect.Left + FMargin + FMenuCaptionWidth * 3, MenuTop, LRect.Right ,LRect.Bottom);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'Help', CaptionRect,  [tfLeft], ThemeTextColor, DPI);

  //Draw ToolButtons
  for i := 1 to 3 do
  begin
    LDetails := Style.GetElementDetails(ttbButtonNormal);
    ButtonRect.Left := BorderRect.Left+ FMargin div 2 + (i-1) * FButtonWidth;
    ButtonRect.Top := LRect.Top+FMenuHeight + FMargin;
    ButtonRect.Width := FButtonWidth;
    ButtonRect.Height :=FButtonHeight;
    Style.DrawElement(FBitmap.Canvas.Handle, LDetails, ButtonRect, ButtonRect, DPI);

    Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
    Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'ToolButton' + IntToStr(i),
      ButtonRect, [tfVerticalCenter, tfCenter], ThemeTextColor, DPI);
  end;

  //Draw Normal
  LDetails := Style.GetElementDetails(tbPushButtonNormal);
  ButtonRect.Left := BorderRect.Left + FMargin;
  ButtonRect.Top := ARect.Height - 2 * FButtonHeight;
  ButtonRect.Width := FButtonWidth;
  ButtonRect.Height := FButtonHeight;
  Style.DrawElement(FBitmap.Canvas.Handle, LDetails, ButtonRect, ButtonRect, DPI);

  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'Normal', ButtonRect,
    [tfVerticalCenter, tfCenter], ThemeTextColor, DPI);

  //Draw Hot
  LDetails := Style.GetElementDetails(tbPushButtonHot);
  ButtonRect.Left := BorderRect.Left + 2 * FMargin + FButtonWidth;
  ButtonRect.Top := ARect.Height - 2 * FButtonHeight;
  ButtonRect.Width := FButtonWidth;
  ButtonRect.Height := FButtonHeight;
  Style.DrawElement(FBitmap.Canvas.Handle, LDetails, ButtonRect, ButtonRect, DPI);

  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'Hot', ButtonRect,
    [tfVerticalCenter, tfCenter], ThemeTextColor, DPI);

  //Draw Pressed
  LDetails := Style.GetElementDetails(tbPushButtonPressed);
  ButtonRect.Left := BorderRect.Left + 3 * FMargin + 2 * FButtonWidth;
  ButtonRect.Top := ARect.Height - 2 * FButtonHeight;
  ButtonRect.Width := FButtonWidth;
  ButtonRect.Height := FButtonHeight;
  Style.DrawElement(FBitmap.Canvas.Handle, LDetails, ButtonRect, ButtonRect, DPI);

  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'Pressed', ButtonRect,
    [tfVerticalCenter, tfCenter], ThemeTextColor, DPI);

  //Draw Disabled
  LDetails := Style.GetElementDetails(tbPushButtonDisabled);
  ButtonRect.Left := BorderRect.Left + 4 * FMargin + 3 * FButtonWidth;
  ButtonRect.Top := ARect.Height - 2 * FButtonHeight;
  ButtonRect.Width := FButtonWidth;
  ButtonRect.Height := FButtonHeight;
  Style.DrawElement(FBitmap.Canvas.Handle, LDetails, ButtonRect, ButtonRect, DPI);

  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'Disabled', ButtonRect,
    [tfVerticalCenter, tfCenter], ThemeTextColor, DPI);

  Canvas.Draw(0,0,FBitmap);
end;

end.
