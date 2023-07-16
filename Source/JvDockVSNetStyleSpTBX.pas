{-----------------------------------------------------------------------------
 Unit Name: TJvDockVSNETTBX
 Author:    Kiriakos Vlahos
 Date:      01-May-2009
 Purpose:   SpTBX themed TJvDockVSNETTree docking style for JvDocking
 History:
-----------------------------------------------------------------------------}
unit JvDockVSNetStyleSpTBX;

{$I jvcl.inc}

interface

uses
  Winapi.Messages,
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  SpTBXSkins,
  JvDockTree,
  JvDockControlForm,
  JvDockSupportControl,
  JvDockVSNetStyle;

Type
  TJvDockVSNETSpTBXConjoinServerOption = class(TJvDockVSNETConjoinServerOption)
  protected
    procedure UpdateDefaultSystemCaptionInfo; override;
    procedure Changed; override;
  public
    constructor Create(ADockStyle: TJvDockObservableStyle); override;
  published
    property GrabbersSize stored IsNotSystemInfo;
    property SplitterWidth stored IsNotSystemInfo;
  end;

  TJvDockVSNETTreeSpTBX = class(TJvDockVSNETTree)
  protected
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
    procedure DrawDockGrabber(Control: TWinControl; const ARect: TRect); override;
    procedure DrawAutoHideButton(Zone: TJvDockZone;
      Left, Top: Integer); override;
    procedure DrawCloseButton(Canvas: TCanvas;
     Zone: TJvDockZone; Left, Top: Integer); override;
    //procedure GetCaptionRect(var Rect: TRect); override;
    procedure DrawSplitterRect(const ARect: TRect); override;
    procedure SyncWithStyle; override;
  public
    constructor Create(DockSite: TWinControl; DockZoneClass: TJvDockZoneClass;
      ADockStyle: TJvDockObservableStyle); override;
    destructor Destroy; override;
  end;

  TJvDockVSNETTabPanelSpTBX = class(TJvDockVSNETTabPanel)
  protected
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TJvDockVSNETTabSheetSpTBX = class(TJvDockVSNETTabSheet)
  private
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
  end;

  TJvDockVSNETTabPageControlSpTBX = class(TJvDockVSNETTabPageControl)
  private
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  protected
    procedure CreatePanel; override;
    procedure AdjustClientRect(var Rect: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TJvDockVSNETPanelSpTBX = class(TJvDockVSNETPanel)
  protected
    procedure AddDockServer(ADockServer: TJvDockServer); override;
    procedure OnPaintSplitter(Sender : TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TJvDockVSChannelSpTBX = class(TJvDockVSChannel)
  protected
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
    procedure Paint; override;
    procedure PopupPaneChanged; override;
    procedure ResetFontAngle; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TMouseLoc = (mlNone, mlOver, mlDown);

  TJvDockVSNETSplitterSpTBX = class(TJvDockVSNETSplitter)
  private
   FMouseLoc: TMouseLoc;
   FMouseOver: boolean;
   FMouseDown: boolean;
  protected
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
  published
   property ResizeStyle default rsUpdate;
  end;

  TJvDockVSNetStyleSpTBX = class(TJvDockVSNetStyle)
  protected
    procedure CreateServerOption; override; { AfterConstruction }
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetDockBaseControl(IsCreate: Boolean; DockBaseControl: TJvDockBaseControl); override;
  end;

procedure Register;

implementation

Uses
  Winapi.CommCtrl,
  System.Math,
  System.Types,
  Vcl.Forms,
  Vcl.Themes,
  JvDockVIDStyle,
  SpTBXDkPanels,
  SpTBXItem,
  JvDockGlobals,
  JvDockSupportProc,
  SpTBXTabs,
  TB2Common,
  TB2Item;

procedure Register;
begin
  RegisterComponents('PyScripter Custom', [TJvDockVSNetStyleSpTBX]);
end;

const
  TabSheetBorderSize = 2;  // to match SpTBX TabSheet border size

procedure ResizeBitmap(Bitmap: TBitmap; const NewWidth, NewHeight: integer);
var
  buffer: TBitmap;
begin
  buffer := TBitmap.Create;
  try
    buffer.SetSize(NewWidth, NewHeight);
    buffer.Canvas.StretchDraw(Rect(0, 0, NewWidth, NewHeight), Bitmap);
    Bitmap.SetSize(NewWidth, NewHeight);
    Bitmap.Canvas.Draw(0, 0, buffer);
  finally
    buffer.Free;
  end;
end;

procedure DrawGlyphPattern(DC: HDC; const R: TRect; Width, Height: Integer;
  const PatternBits; PatternColor: TColor);
const
  ROP_DSPDxax = $00E20746;
var
  B: TBitmap;
  OldTextColor, OldBkColor: Longword;
  OldBrush, Brush: HBrush;
  BitmapWidth, BitmapHeight: Integer;
begin
  OldTextColor := SetTextColor(DC, clBlack);
  OldBkColor := SetBkColor(DC, clWhite);
  B := TBitmap.Create;
  try
    BitmapWidth := 8;
//    if Width > BitmapWidth then BitmapWidth := Width;
    BitmapHeight := 8;
//    if Height > BitmapHeight then BitmapHeight := Height;
    B.Handle := CreateBitmap(BitmapWidth, BitmapHeight, 1, 1, @PatternBits);

    if (Width > 8) or (Height > 8) then
      ResizeBitmap(B, Max(Width, Height), Max(Width, Height));
    if PatternColor < 0 then Brush := GetSysColorBrush(PatternColor and $FF)
    else Brush := CreateSolidBrush(PatternColor);
    OldBrush := SelectObject(DC, Brush);
    BitBlt(DC, (R.Left + R.Right + 1 - Width) div 2, (R.Top + R.Bottom  + 1 - Height) div 2,
      Width, Height, B.Canvas.Handle, 0, 0, ROP_DSPDxax);
    SelectObject(DC, OldBrush);
    if PatternColor >= 0 then DeleteObject(Brush);
  finally
    SetTextColor(DC, OldTextColor);
    SetBkColor(DC, OldBkColor);
    B.Free;
  end;
end;


{ TJvDockVSNETTreeSpTBX }

constructor TJvDockVSNETTreeSpTBX.Create(DockSite: TWinControl;
  DockZoneClass: TJvDockZoneClass; ADockStyle: TJvDockObservableStyle);
begin
  inherited Create(DockSite, DockZoneClass, ADockStyle);;
  GrabberSize := 22;
  SplitterWidth := 5;
  ButtonHeight := 14;
  ButtonWidth := 14; //15
  LeftOffset := 0; //0
  RightOffset := 3;
  TopOffset := 2; //4
  BottomOffset := 2;
  ButtonSplitter := 2;
  CaptionLeftOffset := 5;
  CaptionRightOffset := 5;
  GrabberBottomEdgeColor := clNone;
  SkinManager.AddSkinNotification(Self);
end;

Type
  TCrackJvDockVSNETZone = class(TJvDockVSNETZone)
  end;

  TCrackJvDockVSBlock = class(TJvDockVSBlock)
  end;

destructor TJvDockVSNETTreeSpTBX.Destroy;
begin
  SkinManager.RemoveSkinNotification(Self);
  inherited;
end;

procedure TJvDockVSNETTreeSpTBX.DrawAutoHideButton(Zone: TJvDockZone; Left,
  Top: Integer);
const
  AutoHidePattern1: array [0..15] of Byte  = ($3E, 0, $26, 0, $26, 0, $26, 0, $7F, 0, $08, 0, $08, 0, $08, 0);
  AutoHidePattern2: array [0..15] of Byte  = ($10, 0, $1F, 0, $11, 0, $F1, 0, $F1, 0, $1F, 0, $1F, 0, $10, 0);
var
  AZone: TCrackJvDockVSNETZone;
  ADockClient: TJvDockClient;
  PatternColor: TColor;
  SkinState : TSpTBXSkinStatesType;
  R : TRect;
begin
  if Zone <> nil then
  begin
    ADockClient := FindDockClient(Zone.ChildControl);
    if (ADockClient <> nil) and not ADockClient.EnableCloseButton then
      Left := Left + ButtonWidth; // move the auto hide button to the Close Button's location

    AZone := TCrackJvDockVSNETZone(Zone);
    R := Rect(Left, Top, Left + ButtonWidth, Top + ButtonHeight);
    if AZone.AutoHideBtnState = bsDown then
      R.Offset(PPIScale(1), PPIScale(1));

    if AZone.AutoHideBtnState = TJvDockBtnState.bsNormal then begin
      PatternColor := CurrentSkin.GetTextColor(nil, skncDockablePanelTitleBar, sknsNormal);
      if PatternColor = clNone then
        PatternColor := CurrentSkin.GetTextColor(nil, skncToolbarItem, sknsNormal);
      if PatternColor = clNone then
        PatternColor := clCaptionText;
    end else begin
      SkinState := CurrentSkin.GetState(True, AZone.AutoHideBtnState = bsDown, AZone.AutoHideBtnState = bsUp, False);
      //CurrentSkin.PaintBackground(Canvas, R, skncToolbarItem, SkinState, True, True);
      SpDrawXPToolbarButton(nil, Canvas, R, SkinState, cpNone, FCurrentPPI);
      PatternColor := CurrentSkin.GetTextColor(nil, skncToolbarItem, SkinState);
    end;

    if DockSite.Align in [alLeft, alRight, alTop, alBottom] then
      DrawGlyphPattern(Canvas.Handle, R, PPIScale(8),
        PPIScale(8), AutoHidePattern1[0], PatternColor)
    else if DockSite.Align in [alNone] then
      DrawGlyphPattern(Canvas.Handle, R, PPIScale(8),
        PPIScale(8), AutoHidePattern2[0], PatternColor);
  end;
end;

procedure TJvDockVSNETTreeSpTBX.DrawCloseButton(Canvas: TCanvas;
  Zone: TJvDockZone; Left, Top: Integer);
var
  AZone: TCrackJvDockVSNETZone;
  ADockClient: TJvDockClient;
  AForm: TForm;
  PatternColor: TColor;
  SkinState : TSpTBXSkinStatesType;
  R : TRect;
begin
  if Zone <> nil then
  begin
    ADockClient := FindDockClient(Zone.ChildControl);
    if (ADockClient <> nil) and not ADockClient.EnableCloseButton then
      Exit;
    if Zone.ChildControl is TJvDockTabHostForm then
    begin
      AForm := TJvDockTabHostForm(Zone.ChildControl).GetActiveDockForm;
      if AForm <> nil then
      begin
        ADockClient := FindDockClient(AForm);
        if (ADockClient <> nil) and not ADockClient.EnableCloseButton then
          Exit;
      end;
    end;
    AZone := TCrackJvDockVSNETZone(Zone);
    R := Rect(Left, Top, Left + ButtonWidth, Top + ButtonHeight);
    if AZone.CloseBtnState = bsDown then
      R.Offset(PPIScale(1), PPIScale(1));

    if AZone.CloseBtnState = TJvDockBtnState.bsNormal then begin
      PatternColor := CurrentSkin.GetTextColor(nil, skncDockablePanelTitleBar, sknsNormal);
      if PatternColor = clNone then
        PatternColor := CurrentSkin.GetTextColor(nil, skncToolbarItem, sknsNormal);
      if PatternColor = clNone then
        PatternColor := clCaptionText;
    end else begin
      SkinState := CurrentSkin.GetState(True, AZone.CloseBtnState = bsDown, AZone.CloseBtnState = bsUp, False);
      SpDrawXPToolbarButton(nil, Canvas, R, SkinState, cpNone, FCurrentPPI);
      //CurrentSkin.PaintBackground(Canvas, R, skncToolbarItem, SkinState, True, True);
      PatternColor := CurrentSkin.GetTextColor(nil, skncToolbarItem, SkinState);
    end;

    SpDrawGlyphPattern(Canvas, R, gptClose, PatternColor, FCurrentPPI);
  end;
end;

procedure TJvDockVSNETTreeSpTBX.DrawDockGrabber(Control: TWinControl;
  const ARect: TRect);
var
  DrawRect: TRect;
//  IsActive : boolean;
  PanelCaption: string;
  CRect: TRect;
  ShowCloseButtonOnGrabber: Boolean;
  TabServerOption:TJvDockVIDTabServerOption;
  State : TSpTBXSkinStatesType;
  TextColor: TColor;

begin
  Assert(Assigned(Control));
  ShowCloseButtonOnGrabber := True;

  if Assigned(DockStyle) then
   if Control is TJvDockTabHostForm then
   begin
     TabServerOption := TJvDockVIDTabServerOption(TJvDockBasicStyle(DockStyle).TabServerOption);
     ShowCloseButtonOnGrabber := TabServerOption.ShowCloseButtonOnGrabber;
   end;
  // Defaults should work if DockStyle is not set!
  // -> Assert(DontAssume(Assigned(DockStyle)));

//  IsActive := Assigned(Screen.ActiveControl) and Screen.ActiveControl.Focused and
//    Control.ContainsControl(Screen.ActiveControl);

  if GrabbersPosition <> gpTop then
  begin
    {$IFDEF JVDOCK_DEBUG}
    OutputDebugString('Not supported GrabbersPosition');
    {$ENDIF JVDOCK_DEBUG}
    Exit;
  end;

  DrawRect := ARect;
  Inc(DrawRect.Top, PPIScale(1));
  DrawRect.Bottom := DrawRect.Top + GrabberSize - PPIScale(2);
  CRect := DrawRect;

  // Draw Background
  SpDrawXPDock(nil, Canvas, ARect, False, FCurrentPPI);
  SpDrawXPDockablePanelTitleBar(nil, Canvas, DrawRect, False, False, FCurrentPPI);

  if ShowCloseButtonOnGrabber then
    DrawCloseButton(Canvas, FindControlZone(Control),
     ARect.Right - RightOffset - ButtonWidth - PPIScale(1),
     ARect.Top + TopOffset);

  if DockSite.Align <> alClient then
  begin
    DrawAutoHideButton(FindControlZone(Control),
      ARect.Right - RightOffset - 2 * ButtonWidth - ButtonSplitter,
      ARect.Top + TopOffset)
  end;

  //  Now Draw Caption
  Canvas.Font.Assign(ToolbarFont); // was SmCaptionFont
  Canvas.Font.Height := MulDiv(ToolbarFont.Height, FCurrentPPI, ToolbarFont.PixelsPerInch);
  GetCaptionRect( CRect );
  PanelCaption := TForm(Control).Caption;

  Canvas.Brush.Style := bsClear; // body already painted
  State := CurrentSkin.GetState(True, False, False, False);
  TextColor := CurrentSkin.GetTextColor(nil, skncDockablePanelTitleBar, State);
  if TextColor = clNone then
    TextColor := CurrentSkin.GetTextColor(nil, skncToolbarItem, State);
  if TextColor = clNone then
    TextColor := clCaptionText;
  Canvas.Font.Color := TextColor;

  JvDrawTextWithMiddleEllipsis(Canvas, PanelCaption,
    CRect, DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
//  DrawText(Canvas.Handle, PChar(PanelCaption), Length(PanelCaption), CRect,
//    DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX or DT_END_ELLIPSIS);
end;

procedure PaintSplitterFrame(Canvas : TCanvas; IsVertical: Boolean; var R: TRect);
var
  FrameBrush: HBRUSH;
begin
  //Exit;
  if IsVertical then
    InflateRect(R, -1, 2)
  else
    InflateRect(R, 2, -1);
  OffsetRect(R, 1, 1);
  FrameBrush := CreateSolidBrush(ColorToRGB(StyleServices.GetSystemColor(clBtnHighlight)));
  FrameRect(Canvas.Handle, R, FrameBrush);
  DeleteObject(FrameBrush);
  OffsetRect(R, -2, -2);
  FrameBrush := CreateSolidBrush(ColorToRGB(StyleServices.GetSystemColor(clBtnShadow)));
  FrameRect(Canvas.Handle, R, FrameBrush);
  DeleteObject(FrameBrush);
end;

procedure TJvDockVSNETTreeSpTBX.DrawSplitterRect(const ARect: TRect);

  function GetGripRect(R : TRect; IsVertical : Boolean) : TRect;
  const
    GripSize = 50;
  begin
    Result := R;
    if IsVertical then
      Result := Bounds(R.Left, (R.Bottom - R.Top - GripSize) div 2, R.Right - R.Left, GripSize)
    else
      Result := Bounds((R.Right - R.Left - GripSize) div 2, R.Top, GripSize, R.Bottom - R.Top);
  end;

var
  IsVertical: Boolean;
  R, DragHandleR : TRect;
  C1, C2 : TColor;

begin
  IsVertical := ARect.Right - ARect.Left < ARect.Bottom - ARect.Top;

  // Paint background
  Canvas.Brush.Color := StyleServices.GetSystemColor(clBtnFace);
  Canvas.FillRect(ARect);
  PaintSplitterFrame(Canvas, IsVertical, R);

  // Paint grip
  R := GetGripRect(ARect, IsVertical);
  DragHandleR := R;
  if IsVertical then
    InflateRect(DragHandleR, -1, -10)
  else
    InflateRect(DragHandleR, -10, -1);

  C1 := StyleServices.GetSystemColor(clBtnShadow);
  C2 := StyleServices.GetSystemColor(clWindow);
  SpDrawXPGrip(Canvas, DragHandleR, C1, C2, FCurrentPPI);
end;

procedure TJvDockVSNETTreeSpTBX.SyncWithStyle;
begin
  inherited;
  //Save unscaled
  TopOffset :=  MulDiv((GrabberSize - ButtonHeight) div 2, 96, FCurrentPPI);
end;

{
procedure TJvDockVSNETTreeSpTBX.GetCaptionRect(var Rect: TRect);
var
  ADockClient: TJvDockClient;
begin
  if DockSite.Align = alClient then
    inherited GetCaptionRect(Rect)
  else
  begin
    Inc(Rect.Left, 2 + CaptionLeftOffset);
    ADockClient := FindDockClient(DockSite);
    Inc(Rect.Top, 1);
    if (ADockClient = nil) or ADockClient.EnableCloseButton then
      Dec(Rect.Right, 3 * ButtonWidth + ButtonSplitter + CaptionRightOffset - 1)
    else
      Dec(Rect.Right, 2 * ButtonWidth + ButtonSplitter + CaptionRightOffset - 1);
    Dec(Rect.Bottom, 2);
  end;
end;
}

procedure TJvDockVSNETTreeSpTBX.WMSpSkinChange(var Message: TMessage);
begin
  UpdateAll;
  DockSite.Invalidate;
end;

{ TJvDockVSNetStyleSpTBX }

constructor TJvDockVSNetStyleSpTBX.Create(AOwner: TComponent);
begin
  inherited;
  ConjoinPanelTreeClass := TJvDockVSNETTreeSpTBX;
  DockPanelTreeClass := TJvDockVSNETTreeSpTBX;
  TabDockClass := TJvDockVSNETTabPageControlSpTBX;
  DockPanelClass := TJvDockVSNETPanelSpTBX;
  DockSplitterClass := TJvDockVSNETSplitterSpTBX;
  ConjoinServerOptionClass := TJvDockVSNETSpTBXConjoinServerOption;
end;


procedure TJvDockVSNetStyleSpTBX.CreateServerOption;
begin
  inherited;
  TabServerOption.HotTrack := True;
  ChannelOption.ActivePaneSize := 250;
end;

procedure TJvDockVSNetStyleSpTBX.SetDockBaseControl(IsCreate: Boolean;
  DockBaseControl: TJvDockBaseControl);
Var
  DP : TJvDockPosition;
begin
  inherited;
  if (DockBaseControl is TJvDockServer) and
    not (csDestroying in DockBaseControl.ComponentState)
  then
    for DP := Low(TJvDockPosition) to High(TJvDockPosition) do begin
      TJvDockServer(DockBaseControl).SplitterStyle[DP].Size := 5;
      TJvDockServer(DockBaseControl).SplitterStyle[DP].ResizeStyle := rsUpdate;
    end;
end;

{ TJvDockVSNETTabPageControlSpTBX }

procedure TJvDockVSNETTabPageControlSpTBX.AdjustClientRect(var Rect: TRect);
begin
  Rect := ClientRect;
  if (Parent is TJvDockTabHostForm) and (VisibleDockClientCount = 1) then
    Exit;
  case TabPosition of
    tpTop:
      Inc(Rect.Top, (Panel as TJvDockVSNETTabPanelSpTBX).TabHeight);
    tpBottom:
      Dec(Rect.Bottom, (Panel as TJvDockVSNETTabPanelSpTBX).TabHeight);
    tpLeft:
      Inc(Rect.Left, (Panel as TJvDockVSNETTabPanelSpTBX).TabHeight);
    tpRight:
      Dec(Rect.Right, (Panel as TJvDockVSNETTabPanelSpTBX).TabHeight);
  end;
end;

constructor TJvDockVSNETTabPageControlSpTBX.Create(AOwner: TComponent);
begin
  inherited;
  TabPanelClass := TJvDockVSNETTabPanelSpTBX;
  TabSheetClass := TJvDockVSNETTabSheetSpTBX;
  if Assigned(Images) then
    Images.SetSize(PPIScale(20), PPIScale(20));
  SkinManager.AddSkinNotification(Self);
end;

procedure TJvDockVSNETTabPageControlSpTBX.CreatePanel;
begin
  inherited;
  with Panel as TJvDockVSNETTabPanelSpTBX do begin
    TabBottomOffset := 2;; // changed from 3
    TabTopOffset := 2;
    TabLeftOffset := 2;
    TabRightOffset := 2;
    TabSplitterWidth := 0;
    Canvas.Font.Assign(ToolbarFont);
    TabHeight :=
      Max(MulDiv(Abs(ToolbarFont.Height), 96, ToolbarFont.PixelsPerInch) +  10, 29);
  end;
end;

destructor TJvDockVSNETTabPageControlSpTBX.Destroy;
begin
  SkinManager.RemoveSkinNotification(Self);
  inherited;
end;

procedure TJvDockVSNETTabPageControlSpTBX.WMSpSkinChange(var Message: TMessage);
begin
  if Assigned(ActivePage) then begin
    ActivePage.Invalidate;
    ActivePage.Realign;
  end;
  for var I := 0 to Count - 1 do
  begin
    var Page := Pages[i];
    if (Page.Controls[0] is TForm) and TForm(Page.Controls[0]).Icon.HandleAllocated
      and (Page.ImageIndex >= 0) then
    begin
      ImageList_ReplaceIcon(Images.Handle, Page.ImageIndex,
        CopyIcon(TForm(Page.Controls[0]).Icon.Handle));
    end;
  end;
  Panel.Invalidate;
end;

{ TJvDockVSNETTabPanelSpTBX }

Type
  TCrackJvDockVIDTabSheet = class(TJvDockVIDTabSheet)
  end;

constructor TJvDockVSNETTabPanelSpTBX.Create(AOwner: TComponent);
begin
  inherited;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  BevelEdges := [];
  BorderWidth := 0;

  CaptionTopOffset := 5;
  CaptionLeftOffset := 5;
  CaptionRightOffset := 5;

  SkinManager.AddSkinNotification(Self);
  ControlStyle := ControlStyle + [csOpaque];
  DoubleBuffered := True;  // To avoid flicker
  TabLeftOffset := 2;
end;

destructor TJvDockVSNETTabPanelSpTBX.Destroy;
begin
  SkinManager.RemoveSkinNotification(Self);
  inherited;
end;

procedure TJvDockVSNETTabPanelSpTBX.Paint;
var
  ARect, R, BgRect, DockRect : TRect;
  CurrTabWidth: Integer;
  I, CompleteWidth: Integer;
  ImageWidth: Integer;
  CaptionString: string;
  //  Edge: TSpTBXTabEdge;
  Position: TSpTBXTabPosition;
  IsActive, IsHot : Boolean;
  State: TSpTBXSkinStatesType;
  TextColor: TColor;
  Format: UINT;
  FontHeight : Integer;
  TM: TTextMetric;
  ShowImage: Boolean;
begin
  if Page = nil then
    Exit;

  if (Page.Images <> nil) and (Page.ShowTabImages) then
    ImageWidth := Page.Images.Width
  else
    ImageWidth := 0;

  // Paint the background of the panel

  Assert(Page.TabPosition in [tpTop, tpBottom], RsEDockCannotSetTabPosition);
  ARect := Rect(0, 0, PanelWidth, PanelHeight);

  BgRect := ARect;
  DockRect := ARect;
  if Page.TabPosition = tpBottom then begin
    Position := ttpBottom;
    BgRect.Bottom := TabSheetBorderSize;
    Dec(BgRect.Top);    // This is needed for SpDrawXPTabControlBackground to work
    DockRect.Top := TabSheetBorderSize;
  end else begin
    Position := ttpTop;
    BgRect.Top := ARect.Bottom - TabSheetBorderSize;
    Inc(BgRect.Bottom);    // This is needed for SpDrawXPTabControlBackground to work
    Dec(DockRect.Bottom, TabSheetBorderSize);
  end;

  // Set and scale Canvas font
  Canvas.Font.Height := MulDiv(ToolbarFont.Height, FCurrentPPI, ToolbarFont.PixelsPerInch);
  if GetTextMetrics(Canvas.Handle, TM) then
    FontHeight := TM.tmHeight
  else begin
    FontHeight := Abs(Canvas.Font.Height);
    if Canvas.Font.Height < 0 then
      Inc(FontHeight, Page.PPIScale(3)); //tmInternalLeading
  end;

  // Paint the toolbar background
  SpDrawXPTabControlBackground(nil, Canvas, DockRect, clBtnFace, Position = ttpBottom);

  CompleteWidth := TabLeftOffset;

  // Paint all the tabs
  //  Edge := tedLeft;  // Only for the first visible tab

  for I := 0 to Page.Count - 1 do
  begin
    if not Page.Pages[I].TabVisible then
      Continue;

    CurrTabWidth := TCrackJvDockVIDTabSheet(Page.Pages[I]).ShowTabWidth;
    ShowImage :=  ShowTabImages and (Page.Images <> nil) and
        (Page.Pages[I].ImageIndex >= 0) and
        (CurrTabWidth > ImageWidth + 2 * CaptionLeftOffset);

    //  Calculate Button area
    case Page.TabPosition of
      tpTop:
        R := Rect(CompleteWidth, TabTopOffset,
          CompleteWidth + CurrTabWidth, PanelHeight - TabBottomOffset);
      tpBottom:
        R := Rect(CompleteWidth, TabBottomOffset,
            CompleteWidth + CurrTabWidth, PanelHeight - TabTopOffset);
    end;

    IsActive := Page.ActivePageIndex = I;
    IsHot := Page.HotTrack and (FSelectHotIndex = I);

    // Button overflows into the Tabsheet area;
    case Position of
      ttpTop:
        begin
          Inc(R.Bottom, Page.PPIScale(4));
          // maintain background border
          if IsActive then begin
            SpDrawXPTab(nil, Canvas, R, True, IsActive, False, False, Position, FCurrentPPI);
            ExcludeClipRect(Canvas.Handle, R.Left + 1, ARect.Bottom - TabSheetBorderSize, R.Right - 1, ARect.Bottom);
          end;
          Dec(R.Bottom, Page.PPIScale(3));
        end;
      ttpBottom:
        begin
          Dec(R.Top, Page.PPIScale(4));
          if IsActive then begin
            SpDrawXPTab(nil, Canvas, R, True, IsActive, False, False, Position, FCurrentPPI);
            ExcludeClipRect(Canvas.Handle, R.Left + 1, 0, R.Right - 1, TabSheetBorderSize);
          end;
          Inc(R.Top, Page.PPIScale(3));
        end;
    end;
    if not IsActive then
      case Position of
        ttpTop: Inc(R.Top, Page.PPIScale(TabSheetBorderSize));
        ttpBottom: Dec(R.Bottom, Page.PPIScale(TabSheetBorderSize));
      end;
    SpDrawXPTab(nil, Canvas, R, True, IsActive, IsHot, False, Position, FCurrentPPI);

    // now paint the caption
    Format := DT_LEFT or DT_SINGLELINE{ or DT_END_ELLIPSIS};
    case Page.TabPosition of
      tpTop:
        begin
          R := Rect(CompleteWidth + CaptionLeftOffset +
            Integer(ShowImage) * (ImageWidth + CaptionLeftOffset),
            TabTopOffset + CaptionTopOffset,
            CompleteWidth + CurrTabWidth - CaptionRightOffset,
            PanelHeight);
          if IsActive then OffsetRect(R, 0, - Page.PPIScale(2));
          Format := Format or DT_TOP;
        end;
      tpBottom:
        begin
          R := Rect(CompleteWidth + CaptionLeftOffset +
            Integer(ShowImage) * (ImageWidth + CaptionLeftOffset),
            0, CompleteWidth + CurrTabWidth - CaptionRightOffset,
            PanelHeight - TabTopOffset - CaptionTopOffset);
          if IsActive then OffsetRect(R, 0, Page.PPIScale(2));
          Format := Format or DT_BOTTOM;
        end;
    end;

    CaptionString := Page.Pages[I].Caption;
    State := CurrentSkin.GetState(True, False, IsHot, IsActive);
    TextColor := CurrentSkin.GetTextColor(nil, skncTab, State);
    if TextColor = clNone then
      TextColor := clBtnText;
    Canvas.Font.Color := TextColor;
    Canvas.Brush.Style := bsClear;
//    DrawText(Canvas.Handle, PChar(CaptionString), Length(CaptionString),
//      R, Format);
    JvDrawTextWithMiddleEllipsis(Canvas, CaptionString, R, Format);

    // finally paint the image
    if ShowImage then
    begin
      case Page.TabPosition of
        tpTop:
          begin
            R := Rect(CompleteWidth + CaptionLeftOffset,
              TabTopOffset + CaptionTopOffset,
              CompleteWidth + CaptionLeftOffset + ImageWidth,
              TabTopOffset + CaptionTopOffset + FontHeight);
            if IsActive then OffsetRect(R, 0, - Page.PPIScale(2));
          end;
        tpBottom:
          begin
            R := Rect(CompleteWidth + CaptionLeftOffset,
              PanelHeight - TabTopOffset - CaptionTopOffset - FontHeight + 1,
              CompleteWidth + CaptionLeftOffset + ImageWidth,
              PanelHeight - TabTopOffset - CaptionTopOffset);
            if IsActive then OffsetRect(R, 0, Page.PPIScale(2));
          end;
      end;
      R := SpCenterRectVert(R, Page.Images.Height);

      SpDrawVirtualImageList(Canvas, R, Page.Images, Page.Pages[I].ImageIndex, True);
    end;

    Inc(CompleteWidth, CurrTabWidth + TabSplitterWidth);
  end;
  // Paint one side of the TabSheet background after drawing the tabs;
  SpDrawXPTabControlBackground(nil, Canvas, BgRect, Color, Page.TabPosition = tpBottom);
end;

procedure TJvDockVSNETTabPanelSpTBX.WMSpSkinChange(var Message: TMessage);
begin
  Invalidate;
end;


{ TJvDockVSNETPanelSpTBX }

procedure TJvDockVSNETPanelSpTBX.AddDockServer(ADockServer: TJvDockServer);
begin
  inherited;
  if Assigned(VSChannel) then begin
    VSChannel.VSPopupPanelSplitter.ResizeStyle := rsUpdate;
    VSChannel.VSPopupPanelSplitter.SplitWidth := PPIScale(5);
    VSChannel.VSPopupPanelSplitter.OnPaint := OnPaintSplitter;
  end;
end;

constructor TJvDockVSNETPanelSpTBX.Create(AOwner: TComponent);
begin
  inherited;
  VSChannelClass := TJvDockVSChannelSpTBX;
end;

procedure TJvDockVSNETPanelSpTBX.OnPaintSplitter(Sender: TObject);
Var
  R : TRect;
  IsVertical : Boolean;
begin
  if Assigned(Sender) and (Sender is TJvDockVSPopupPanelSplitter) then
    with TJvDockVSPopupPanelSplitter(Sender) do begin
      R := ClientRect;
      IsVertical := R.Right - R.Left < R.Bottom - R.Top;

      // Paint background
      Canvas.Brush.Color := StyleServices.GetSystemColor(clBtnFace);
      Canvas.FillRect(R);

      PaintSplitterFrame(Canvas, IsVertical, R);
    end;
end;

{ TJvDockVSChannelSpTBX }

constructor TJvDockVSChannelSpTBX.Create(AOwner: TComponent);
begin
  inherited;
  FBlockImageListSize := 20;
  FInactiveBlockWidth := 28;
  ControlStyle := ControlStyle + [csParentBackground];
  DoubleBuffered := True;
  SkinManager.AddSkinNotification(Self);
  Canvas.Font.Assign(ToolbarFont);
  ChannelWidth :=  // unscaled
     Max(MulDiv(Abs(ToolbarFont.Height), 96, ToolbarFont.PixelsPerInch) +  10, 26);
end;

destructor TJvDockVSChannelSpTBX.Destroy;
begin
  SkinManager.RemoveSkinNotification(Self);
  inherited;
end;

procedure TJvDockVSChannelSpTBX.Paint;
var
  I: Integer;
  IsVertical : Boolean;
  FontHeight : Integer;
  TM: TTextMetric;

  procedure DrawSingleBlock(Block: TJvDockVSBlock);
  var
    DrawRect, R: TRect;
    I: Integer;
    OldGraphicsMode: Integer;
    VisiblePaneCount: Integer;
    State : TSpTBXSkinStatesType;
    TextColor: TColor;
    IsChecked, IsHot: Boolean;
    Flags : Integer;

    procedure AdjustImagePos;
    begin
      R := DrawRect;
      if Align in [alLeft, alRight] then begin
        Inc(R.Top, PPIScale(4));
        R.Bottom := R.Top + Block.ImageList.Height;
        R := SpCenterRectHoriz(R, Block.ImageList.Width);
      end else begin
        Inc(R.Left, PPIScale(4));
        R.Right := R.Left + Block.ImageList.Width;
        R := SpCenterRectVert(R, Block.ImageList.Height);
      end;

//      if Align = alLeft then
//      begin
//        Inc(DrawRect.Left, 3);
//        Inc(DrawRect.Top, 4);
//      end
//      else
//      if Align = alTop then
//      begin
//        Inc(DrawRect.Left, 4);
//        Inc(DrawRect.Top, 2);
//      end
//      else
//      if Align = alRight then
//      begin
//        Inc(DrawRect.Left, 4);
//        Inc(DrawRect.Top, 4);
//      end
//      else
//      if Align = alBottom then
//      begin
//        Inc(DrawRect.Left, 4);
//        Inc(DrawRect.Top, 3);
//      end;
    end;

  begin
    VisiblePaneCount := 0;
    for I := 0 to Block.VSPaneCount - 1 do
    begin
      if not Block.VSPane[I].Visible then
        Continue;

      GetBlockRect(Block, I, DrawRect);

      IsHot := TCrackJvDockVSBlock(Block).ActiveDockControl = Block.VSPane[I].DockForm;
      IsChecked := Block.VSPane[I].Active;
      State := CurrentSkin.GetState(True, False, IsHot, IsChecked);
      SpDrawXPButton(nil, Canvas, DrawRect, True, False, IsHot, IsChecked, False, False, FCurrentPPI);

      AdjustImagePos;
      SpDrawVirtualImageList(Canvas, R, Block.ImageList, I, True);

      if IsHot then
      begin
        Flags := DT_END_ELLIPSIS or DT_NOCLIP or DT_SINGLELINE;
        // draw the Caption
        if Align in [alTop, alBottom] then begin
          Inc(DrawRect.Left, PPIScale(2) + TCrackJvDockVSBlock(Block).InactiveBlockWidth);
          Dec(DrawRect.Right, PPIScale(2));
          Flags := Flags or DT_VCENTER;
        end else
        if Align in [alLeft, alRight] then
        begin
          Inc(DrawRect.Top, PPIScale(2) + TCrackJvDockVSBlock(Block).InactiveBlockWidth);
          DrawRect.Left := DrawRect.Right + PPIScale(1) - ((ChannelWidth - FontHeight) div 2);
          Dec(DrawRect.Bottom, PPIScale(2));
          DrawRect.Right := DrawRect.Left + (DrawRect.Bottom - DrawRect.Top);
        end;

        OldGraphicsMode := SetGraphicsMode(Canvas.Handle, GM_ADVANCED);
        Canvas.Brush.Style := bsClear;

        TextColor := CurrentSkin.GetTextColor(nil, skncButton, State);
        if TextColor = clNone then
          TextColor := clBtnText;
        Canvas.Font.Color := TextColor;

        DrawText(Canvas.Handle, PChar(Block.VSPane[I].DockForm.Caption), -1,
          DrawRect, Flags);
        SetGraphicsMode(Canvas.Handle, OldGraphicsMode);
      end;
      Inc(VisiblePaneCount);
    end;
    if VisiblePaneCount > 0 then
      CurrentPos := CurrentPos + BlockInterval;
  end;

begin
  // Scale Canvas font
  Canvas.Font.Height := MulDiv(ToolbarFont.Height, FCurrentPPI, ToolbarFont.PixelsPerInch);
  if GetTextMetrics(Canvas.Handle, TM) then
    FontHeight := TM.tmHeight
  else begin
    FontHeight := Abs(Canvas.Font.Height);
    if Canvas.Font.Height < 0 then
      Inc(FontHeight, PPIScale(3)); //tmInternalLeading
  end;

  IsVertical := Align in [alLeft, alRight];
  SpDrawXPDock(nil, Canvas, ClientRect, IsVertical, FCurrentPPI);
  CurrentSkin.PaintBackground(Canvas, ClientRect, skncDock, sknsNormal, True, True, IsVertical);

  CurrentPos := BlockStartOffset;
  for I := 0 to BlockCount - 1 do
    DrawSingleBlock(Block[I]);
end;

procedure TJvDockVSChannelSpTBX.PopupPaneChanged;
begin
  inherited;
  Invalidate;
end;

procedure TJvDockVSChannelSpTBX.ResetFontAngle;
begin
  if Align in [alLeft, alRight] then
    Canvas.Font.Handle := SpCreateRotatedFont(Canvas.Handle);
end;

procedure TJvDockVSChannelSpTBX.WMSpSkinChange(var Message: TMessage);
begin
  for var I := 0 to BlockCount - 1 do
  begin
    var Block := Block[I];
    for var J := 0 to Block.VSPaneCount - 1 do
    begin
      var VSPane := Block.VSPane[J];
      if TForm(VSPane.DockForm).Icon.HandleAllocated then
        ImageList_ReplaceIcon(Block.ImageList.Handle, J,
          CopyIcon(TForm(VSPane.DockForm).Icon.Handle));
    end;
  end;

  Invalidate;
end;

{ TJvDockVSNETSplitterSpTBX }

constructor TJvDockVSNETSplitterSpTBX.Create(AOwner: TComponent);
begin
  inherited;
  ResizeStyle := rsUpdate;
  Width := 5;
  FMouseLoc := mlNone;
  FMouseOver := false;
  FMouseDown := false;
  ControlStyle := ControlStyle + [csOpaque];
  //DoubleBuffered := True;
  SkinManager.AddSkinNotification(Self);
end;

procedure TJvDockVSNETSplitterSpTBX.CMMouseLeave(var Message: TMessage);
begin
  FMouseOver := false;
  if not FMouseDown then
  begin
    FMouseLoc := mlNone;
    Invalidate;
  end;
end;

procedure TJvDockVSNETSplitterSpTBX.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseDown := false;
  if FMouseOver then
    FMouseLoc := mlOver
  else
    FMouseLoc := mlNone;
  inherited;
  Invalidate;
end;

procedure TJvDockVSNETSplitterSpTBX.CMMouseEnter(var Message: TMessage);
begin
  if not FMouseDown then
  begin
    FMouseOver := true;
    FMouseLoc := mlOver;
    Invalidate;
  end;
end;

destructor TJvDockVSNETSplitterSpTBX.Destroy;
begin
  SkinManager.RemoveSkinNotification(Self);
  inherited;
end;

procedure TJvDockVSNETSplitterSpTBX.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseDown := true;
  FMouseLoc := mlDown;
  inherited;
  Invalidate;
end;

procedure TJvDockVSNETSplitterSpTBX.Paint;

  function GetGripRect(IsVertical : Boolean) : TRect;
  const
    GripSize = 50;
  begin
    Result := Rect(0, 0, 0, 0);
    if IsVertical then
      Result := Bounds(0, (Height - PPIScale(GripSize)) div 2, Width, PPIScale(GripSize))
    else
      Result := Bounds((Width - PPIScale(GripSize)) div 2, 0, PPIScale(GripSize), Height);
  end;

var
  R, DragHandleR : TRect;
  IsVertical : Boolean;
  C1, C2 : TColor;
begin
  IsVertical := Align in [alLeft, alRight];

  // Paint background
  Canvas.Brush.Color := StyleServices.GetSystemColor(clBtnFace);
  Canvas.FillRect(ClientRect);

  PaintSplitterFrame(Canvas, IsVertical, R);

  // Paint grip
  R := GetGripRect(IsVertical);
  DragHandleR := R;
  if IsVertical then
    InflateRect(DragHandleR, -PPIScale(1), -PPIScale(10))
  else
    InflateRect(DragHandleR, -PPIScale(10), -PPIScale(1));

    C1 := StyleServices.GetSystemColor(clBtnShadow);
    C2 := StyleServices.GetSystemColor(clWindow);
    SpDrawXPGrip(Canvas, DragHandleR, C1, C2, FCurrentPPI);

//  //uncomment this is you want the dotted frame drawn at designtime
//  if csDesigning in ComponentState then
//   with Canvas do
//    begin
//      ExcludeClipRect(Canvas.Handle, 1, 1, Width - 1, Height - 1);
//      FMouseLoc := mlNone;
//      Pen.Style := psDot;
//      Pen.Mode := pmXor;
//      Pen.Color := $00FFD8CE;
//      Brush.Style := bsClear;
//      Rectangle(ClientRect);
//    end;

  if Assigned(OnPaint) then
   OnPaint(Self);
end;

procedure TJvDockVSNETSplitterSpTBX.WMSpSkinChange(var Message: TMessage);
begin
  Invalidate;
end;

{ TJvDockVSNETSpTBXConjoinServerOption }

procedure TJvDockVSNETSpTBXConjoinServerOption.Changed;
// Hack to call the grandfather version
// This is to avoid reseting the SystemInfo property.  Bad design...
Var
  GrandFatherChange : procedure of object;
begin
  TMethod(GrandFatherChange).Code := @TJvDockBasicServerOption.Changed;
  TMethod(GrandFatherChange).Data := Self;
  GrandFatherChange;
  TJvDockVSNetStyleSpTBX(DockStyle).DoSystemInfoChange(SystemInfo);
end;

constructor TJvDockVSNETSpTBXConjoinServerOption.Create(
  ADockStyle: TJvDockObservableStyle);
begin
  inherited;
end;

procedure TJvDockVSNETSpTBXConjoinServerOption.UpdateDefaultSystemCaptionInfo;
var
  NonClientMetrics: TNonClientMetrics;
begin
  inherited;
  SplitterWidth := 5;
  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if (TOSVersion.Major >= 10) and (TOSVersion.Build >= 14393) and
    SystemParametersInfoForDpi(SPI_GETNONCLIENTMETRICS,
    NonClientMetrics.cbSize, @NonClientMetrics, 0, 96)
  then
    GrabbersSize := Max(Abs(NonClientMetrics.lfSmCaptionFont.lfHeight) + 12, 28)
  else
    GrabbersSize := 28;
end;

{ TJvDockVSNETTabSheetSpTBX }

procedure TJvDockVSNETTabSheetSpTBX.AdjustClientRect(var Rect: TRect);
begin
  if Assigned(PageControl) then begin
    InflateRect(Rect, -TabSheetBorderSize, -TabSheetBorderSize);
    if (PageControl as TJvDockVSNETTabPageControlSpTBX).TabPosition = tpBottom then
      Inc(Rect.Bottom, TabSheetBorderSize)
    else
      Dec(Rect.Top, TabSheetBorderSize);
  end else
    inherited AdjustClientRect(Rect);
end;

procedure TJvDockVSNETTabSheetSpTBX.WMEraseBkgnd(var Message: TMessage);
var
  R: TRect;
  ACanvas: TCanvas;
begin
  if not DoubleBuffered or (Message.wParam = WPARAM(Message.lParam)) then begin
    if not (csDestroying in ComponentState) then begin
      if Assigned(PageControl) and Visible then begin
        ACanvas := TCanvas.Create;
        ACanvas.Handle := TWMEraseBkgnd(Message).DC;
        try
          R := ClientRect;
          ACanvas.Brush.Color := clWindow;
          ACanvas.FillRect(R);

          // Part of the background will be painted by the TabPanel, since we want the
          // tabs to be "merged" into the background
          if (PageControl as TJvDockVSNETTabPageControlSpTBX).TabPosition = tpBottom then
            Inc(R.Bottom, TabSheetBorderSize)
          else
            Dec(R.Top, TabSheetBorderSize);

          SpDrawXPTabControlBackground(nil, ACanvas, R, Color, (PageControl as TJvDockVSNETTabPageControlSpTBX).TabPosition = tpBottom);
        finally
          ACanvas.Handle := 0;
          ACanvas.Free;
        end;
      end;
    end;
  end;
  Message.Result := 1;
end;

end.
