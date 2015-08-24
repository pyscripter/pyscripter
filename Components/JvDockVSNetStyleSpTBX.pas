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

uses Messages, Windows, Sysutils, Controls, Classes, Graphics, ComCtrls,
     SpTBXSkins, JvDockTree, JvDockControlForm, JvDockSupportControl,
     JvDockVSNetStyle, ExtCtrls;

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
  private
    // unfortunately the parent class does not expose this
    // so I had to replicate the methods CMMouseLeave and
    // MouseMove
    FSelectHotIndex: Integer;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
  protected
    FontHeight : Integer;
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    property SelectedHotIndex : integer read FSelectHotIndex;
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
    FontHeight : Integer;
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
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetDockBaseControl(IsCreate: Boolean; DockBaseControl: TJvDockBaseControl); override;
  end;

procedure Register;

implementation

Uses
  Forms, JvDockVIDStyle, SpTBXDkPanels, SpTBXItem, JvDockGlobals,
  SpTBXTabs, TB2Item, Math, Types;

procedure Register;
begin
  RegisterComponents('PyScripter Custom', [TJvDockVSNetStyleSpTBX]);
end;

const
  TabSheetBorderSize = 2;  // to match SpTBX TabSheet border size

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
  ButtonSplitter := 0;
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
//  if not (SkinManager.GetSkinType in [sknSkin, sknDelphiStyle]) then begin
//    inherited;
//    Exit;
//  end;

  if Zone <> nil then
  begin
    ADockClient := FindDockClient(Zone.ChildControl);
    if (ADockClient <> nil) and not ADockClient.EnableCloseButton then
      Left := Left + ButtonWidth; // move the auto hide button to the Close Button's location

    AZone := TCrackJvDockVSNETZone(Zone);
    R := Rect(Left, Top, Left + ButtonWidth, Top + ButtonHeight);

    if AZone.AutoHideBtnState = TJvDockBtnState.bsNormal then begin
      PatternColor := CurrentSkin.GetTextColor(skncDockablePanelTitleBar, sknsNormal);
      if PatternColor = clNone then
        PatternColor := CurrentSkin.GetTextColor(skncToolbarItem, sknsNormal);
      if PatternColor = clNone then
        PatternColor := clCaptionText;
    end else begin
      SkinState := CurrentSkin.GetState(True, AZone.AutoHideBtnState = bsDown, AZone.AutoHideBtnState = bsUp, False);
      //CurrentSkin.PaintBackground(Canvas, R, skncToolbarItem, SkinState, True, True);
      SpDrawXPToolbarButton(Canvas, R, SkinState);
      PatternColor := CurrentSkin.GetTextColor(skncToolbarItem, SkinState);
    end;

    if DockSite.Align in [alLeft, alRight, alTop, alBottom] then
      SpDrawGlyphPattern(Canvas.Handle, R, 8, 8, AutoHidePattern1[0], PatternColor)
    else
    if DockSite.Align in [alNone] then
      SpDrawGlyphPattern(Canvas.Handle, R, 8, 8, AutoHidePattern2[0], PatternColor);
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
//  if not (SkinManager.GetSkinType in [sknSkin, sknDelphiStyle]) then begin
//    inherited;
//    Exit;
//  end;

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

    if AZone.CloseBtnState = TJvDockBtnState.bsNormal then begin
      PatternColor := CurrentSkin.GetTextColor(skncDockablePanelTitleBar, sknsNormal);
      if PatternColor = clNone then
        PatternColor := CurrentSkin.GetTextColor(skncToolbarItem, sknsNormal);
      if PatternColor = clNone then
        PatternColor := clCaptionText;
    end else begin
      SkinState := CurrentSkin.GetState(True, AZone.CloseBtnState = bsDown, AZone.CloseBtnState = bsUp, False);
      SpDrawXPToolbarButton(Canvas, R, SkinState);
      //CurrentSkin.PaintBackground(Canvas, R, skncToolbarItem, SkinState, True, True);
      PatternColor := CurrentSkin.GetTextColor(skncToolbarItem, SkinState);
    end;
    SpDrawGlyphPattern(Canvas, R, 0, PatternColor);
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
//  if not (SkinManager.GetSkinType in [sknSkin, sknDelphiStyle]) then begin
//    inherited;
//    Exit;
//  end;

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
  Inc(DrawRect.Top, 1);
  DrawRect.Bottom := DrawRect.Top + GrabberSize - 2;
  CRect := DrawRect;

  // Draw Background
  SpDrawXPDock(Canvas, ARect);
  SpDrawXPDockablePanelTitleBar(Canvas, DrawRect, False, False);

  if ShowCloseButtonOnGrabber then
    DrawCloseButton(Canvas, FindControlZone(Control), ARect.Right - RightOffset - ButtonWidth - 1, ARect.Top + TopOffset);

  if DockSite.Align <> alClient then
  begin
    DrawAutoHideButton(FindControlZone(Control),
      ARect.Right - RightOffset - 2 * ButtonWidth - ButtonSplitter,
      ARect.Top + TopOffset)
  end;

  //  Now Draw Caption
  Canvas.Font.Assign(SmCaptionFont);
  GetCaptionRect( CRect );
  PanelCaption := TForm(Control).Caption;

  Canvas.Brush.Style := bsClear; // body already painted
  State := CurrentSkin.GetState(True, False, False, False);
  TextColor := CurrentSkin.GetTextColor(skncDockablePanelTitleBar, State);
  if TextColor = clNone then
    TextColor := CurrentSkin.GetTextColor(skncToolbarItem, State);
  if TextColor = clNone then
    TextColor := clCaptionText;
  Canvas.Font.Color := TextColor;

  DrawText(Canvas.Handle, PChar(PanelCaption), Length(PanelCaption), CRect,
    DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX or DT_END_ELLIPSIS);
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
  FrameBrush := CreateSolidBrush(ColorToRGB(CurrentSkin.GetThemedSystemColor(clBtnHighlight)));
  FrameRect(Canvas.Handle, R, FrameBrush);
  DeleteObject(FrameBrush);
  OffsetRect(R, -2, -2);
  FrameBrush := CreateSolidBrush(ColorToRGB(CurrentSkin.GetThemedSystemColor(clBtnShadow)));
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
//  if not (SkinManager.GetSkinType in [sknSkin, sknDelphiStyle]) then begin
//    inherited;
//    Exit;
//  end;

  IsVertical := ARect.Right - ARect.Left < ARect.Bottom - ARect.Top;

  // Paint background
  if SkinManager.GetSkinType = sknSkin then
    CurrentSkin.PaintBackground(Canvas, ARect, skncSplitter, sknsNormal, True,
      False, IsVertical)
  else begin
    Canvas.Brush.Color := CurrentSkin.GetThemedSystemColor(clBtnFace);
    Canvas.FillRect(ARect);
    PaintSplitterFrame(Canvas, IsVertical, R);
  end;


  // Paint grip
  R := GetGripRect(ARect, IsVertical);
  DragHandleR := R;
  if IsVertical then
    InflateRect(DragHandleR, -1, -10)
  else
    InflateRect(DragHandleR, -10, -1);

  if SkinManager.GetSkinType = sknSkin then begin
    if Assigned(SizingZone) then
      CurrentSkin.PaintBackground(Canvas, R, skncButton, sknsNormal, True, True, False, [akLeft, akTop, akRight, akBottom]);
    C1 := SkinManager.CurrentSkin.Options(skncToolbarGrip).Body.Color1;
    C2 := SkinManager.CurrentSkin.Options(skncToolbarGrip).Body.Color2;
  end
  else begin
    C1 := CurrentSkin.GetThemedSystemColor(clBtnShadow);
    C2 := CurrentSkin.GetThemedSystemColor(clWindow);
  end;
  SpDrawXPGrip(Canvas, DragHandleR, C1, C2);

end;

procedure TJvDockVSNETTreeSpTBX.SyncWithStyle;
begin
  inherited;
  TopOffset := (GrabberSize - ButtonHeight) div 2;
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
  if not (SkinManager.GetSkinType in [sknSkin, sknDelphiStyle]) then
    ButtonWidth := 16
  else
    ButtonWidth := 14
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


procedure TJvDockVSNetStyleSpTBX.SetDockBaseControl(IsCreate: Boolean;
  DockBaseControl: TJvDockBaseControl);
Var
  DP : TJvDockPosition;
begin
  inherited;
  if DockBaseControl is TJvDockServer then begin
    for DP := Low(TJvDockPosition) to High(TJvDockPosition) do begin
      TJvDockServer(DockBaseControl).SplitterStyle[DP].Size := 5;
      TJvDockServer(DockBaseControl).SplitterStyle[DP].ResizeStyle := rsUpdate;
    end;
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
  ControlStyle := ControlStyle + [csOpaque];
  TabPanelClass := TJvDockVSNETTabPanelSpTBX;
  TabSheetClass := TJvDockVSNETTabSheetSpTBX;
  SkinManager.AddSkinNotification(Self);
end;

procedure TJvDockVSNETTabPageControlSpTBX.CreatePanel;
Var
  TM: TTextMetric;
begin
  inherited;
  with Panel as TJvDockVSNETTabPanelSpTBX do begin
    TabBottomOffset := 2; // changed from 3
    TabTopOffset := 2;
    TabLeftOffset := 2;
    TabRightOffset := 2;
    TabSplitterWidth := 0;

    Canvas.Font.Assign(ToolbarFont);
    if GetTextMetrics(Canvas.Handle, TM) then
      FontHeight := TM.tmHeight
    else begin
      FontHeight := Abs(ToolbarFont.Height);
      if ToolbarFont.Height < 0 then
        Inc(FontHeight, 3); //tmInternalLeading
    end;
    TabHeight := Max(TJvDockVSNETTabPanelSpTBX(Panel).FontHeight + 10, 25);
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
end;

{ TJvDockVSNETTabPanelSpTBX }

Type
  TCrackJvDockVIDTabSheet = class(TJvDockVIDTabSheet)
  end;

procedure TJvDockVSNETTabPanelSpTBX.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  if FSelectHotIndex <> -1 then
  begin
    FSelectHotIndex := -1;
    Invalidate;
  end;
end;

constructor TJvDockVSNETTabPanelSpTBX.Create(AOwner: TComponent);
begin
  inherited;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  BevelEdges := [];
  BorderWidth := 0;

  FSelectHotIndex := -11;
  CaptionTopOffset := 5;
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

procedure TJvDockVSNETTabPanelSpTBX.MouseMove(Shift: TShiftState; X, Y: Integer);
Var
  Index : integer;
begin
  inherited;
  Index := GetPageIndexFromMousePos(X, Y);
  if Page.HotTrack and (Index <> FSelectHotIndex) then
  begin
    FSelectHotIndex := Index;
    Invalidate;
  end;
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
  State : TSpTBXSkinStatesType;
  TextColor: TColor;
  Format : UINT;
begin
//  if not (SkinManager.GetSkinType in [sknSkin, sknDelphiStyle]) then begin
//    inherited;
//    Exit;
//  end;

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

  // Paint the toolbar background
  //CurrentSkin.PaintBackground(Canvas, DockRect, skncDock, sknsNormal, True, False);
  //CurrentSkin.PaintBackground(Canvas, DockRect, skncToolbar, sknsNormal, True, False);
  SpDrawXPTabControlBackground(Canvas, DockRect, clBtnFace, Position = ttpBottom);

  CompleteWidth := TabLeftOffset;

  // Paint all the tabs
  Canvas.Font.Assign(ToolbarFont);
//  Edge := tedLeft;  // Only for the first visible tab

  for I := 0 to Page.Count - 1 do
  begin
    if not Page.Pages[I].TabVisible then
      Continue;

    CurrTabWidth := TCrackJvDockVIDTabSheet(Page.Pages[I]).ShowTabWidth;

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

    if IsActive or IsHot or (SkinManager.GetSkinType = sknDelphiStyle) or
      not CurrentSkin.Options(skncTab, sknsNormal).Body.IsEmpty or
      not CurrentSkin.Options(skncTab, sknsNormal).Borders.IsEmpty then
    begin
      // Button overflows into the Tabsheet area;
      case Position of
        ttpTop:
          begin
            Inc(R.Bottom, 5);
            // maintain background border
            if IsActive then begin
              SpDrawXPTab(Canvas, R, True, IsActive, False, False, Position, tedNone);
              ExcludeClipRect(Canvas.Handle, R.Left+1, ARect.Bottom - TabSheetBorderSize, R.Right-1, ARect.Bottom);
            end;
          end;
        ttpBottom:
          begin
            Dec(R.Top, 5);
            if IsActive then begin
              SpDrawXPTab(Canvas, R, True, IsActive, False, False, Position, tedNone);
              ExcludeClipRect(Canvas.Handle, R.Left+1, 0, R.Right-1, TabSheetBorderSize);
            end;
          end;
      end;
      SpDrawXPTab(Canvas, R, True, IsActive, IsHot, False, Position, tedNone);
    end
    else begin
      // Draw the separators
      if (I < Page.ActivePageIndex - 1) or (I > Page.ActivePageIndex) and
         (I < Page.Count - 1)
      then
        SpDrawXPMenuSeparator(Canvas, Rect(CompleteWidth + CurrTabWidth -4,
              TabTopOffset,
              CompleteWidth + CurrTabWidth + 4,
              PanelHeight - TabBottomOffset), False, True);
    end;
//    Edge := tedNone;

    // now paint the caption
    Format := DT_LEFT or DT_SINGLELINE or DT_END_ELLIPSIS;
    case Page.TabPosition of
      tpTop:
        begin
          R := Rect(CompleteWidth + CaptionLeftOffset +
            Integer(ShowTabImages) * (ImageWidth + CaptionLeftOffset),
            TabTopOffset + CaptionTopOffset,
            CompleteWidth + CurrTabWidth - CaptionRightOffset,
            PanelHeight);
          if IsActive then OffsetRect(R, 0, -2);
          Format := Format or DT_TOP;
        end;
      tpBottom:
        begin
          R := Rect(CompleteWidth + CaptionLeftOffset +
            Integer(ShowTabImages) * (ImageWidth + CaptionLeftOffset),
            0, CompleteWidth + CurrTabWidth - CaptionRightOffset,
            PanelHeight - TabTopOffset - CaptionTopOffset);
          if IsActive then OffsetRect(R, 0, 2);
          Format := Format or DT_BOTTOM;
        end;
    end;

    CaptionString := Page.Pages[I].Caption;
    State := CurrentSkin.GetState(True, False, IsHot, IsActive);
    TextColor := CurrentSkin.GetTextColor(skncTab, State);
    if TextColor = clNone then
      TextColor := clBtnText;
    Canvas.Font.Color := TextColor;
    Canvas.Brush.Style := bsClear;
    DrawText(Canvas.Handle, PChar(CaptionString), Length(CaptionString),
      R, Format);

    // finally paint the image
    if ShowTabImages and (Page.Images <> nil) and (CurrTabWidth > ImageWidth + 2 * CaptionLeftOffset) then
    begin
      case Page.TabPosition of
        tpTop:
          begin
            R := Rect(CompleteWidth + CaptionLeftOffset,
              TabTopOffset + CaptionTopOffset,
              CompleteWidth + CaptionLeftOffset + ImageWidth,
              TabTopOffset + CaptionTopOffset + FontHeight);
            if IsActive then OffsetRect(R, 0, -2);
          end;
        tpBottom:
          begin
            R := Rect(CompleteWidth + CaptionLeftOffset,
              PanelHeight - TabTopOffset - CaptionTopOffset - FontHeight + 1,
              CompleteWidth + CaptionLeftOffset + ImageWidth,
              PanelHeight - TabTopOffset - CaptionTopOffset);
            if IsActive then OffsetRect(R, 0, 2);
          end;
      end;
      R := SpCenterRectVert(R, Page.Images.Height);

      SpDrawImageList(Canvas, R, Page.Images, Page.Pages[I].ImageIndex, True, False);
    end;

    Inc(CompleteWidth, CurrTabWidth + TabSplitterWidth);
  end;
  // Paint one side of the TabSheet background after drawing the tabs;
  SpDrawXPTabControlBackground(Canvas, BgRect, Color, Page.TabPosition = tpBottom);
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
    VSChannel.VSPopupPanelSplitter.SplitWidth := 5;
    VSChannel.VSPopupPanelSplitter.OnPaint := OnPaintSplitter;
  end;
end;

constructor TJvDockVSNETPanelSpTBX.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque];
  VSChannelClass := TJvDockVSChannelSpTBX;
end;

procedure TJvDockVSNETPanelSpTBX.OnPaintSplitter(Sender: TObject);
Var
  R : TRect;
  IsVertical : Boolean;
begin
  //if not (SkinManager.GetSkinType in [sknSkin, sknDelphiStyle]) then Exit;

  if Assigned(Sender) and (Sender is TJvDockVSPopupPanelSplitter) then
    with TJvDockVSPopupPanelSplitter(Sender) do begin
      R := ClientRect;
      IsVertical := R.Right - R.Left < R.Bottom - R.Top;

      // Paint background
      if SkinManager.GetSkinType = sknSkin then
        CurrentSkin.PaintBackground(Canvas, R, skncSplitter, sknsNormal,
                                           True, False, IsVertical)
      else begin
        Canvas.Brush.Color := CurrentSkin.GetThemedSystemColor(clBtnFace);
        Canvas.FillRect(R);

        PaintSplitterFrame(Canvas, IsVertical, R);
      end;
    end;
end;

{ TJvDockVSChannelSpTBX }

constructor TJvDockVSChannelSpTBX.Create(AOwner: TComponent);
var
  NonClientMetrics: TNonClientMetrics;
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque];
  DoubleBuffered := True;
  SkinManager.AddSkinNotification(Self);
  Canvas.Font.Assign(ToolbarFont);
  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    PInteger(@(Self.ChannelWidth))^ :=
      Max(Abs(NonClientMetrics.lfSmCaptionFont.lfHeight) +
        MulDiv(10, Screen.PixelsPerInch, 96), 22);  // Access private property
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
        Inc(R.Top, 4);
        R.Bottom := R.Top + Block.ImageList.Height;
        R := SpCenterRectHoriz(R, Block.ImageList.Width);
      end else begin
        Inc(R.Left, 4);
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
      SpDrawXPButton(Canvas, DrawRect, True, False, IsHot, IsChecked, False, False);

      AdjustImagePos;
      SpDrawImageList(Canvas, R, Block.ImageList, I, True, False);

      if IsHot then
      begin
        Flags := DT_END_ELLIPSIS or DT_NOCLIP or DT_SINGLELINE;
        // draw the Caption
        if Align in [alTop, alBottom] then begin
          Inc(DrawRect.Left, 2 + TCrackJvDockVSBlock(Block).InactiveBlockWidth);
          Dec(DrawRect.Right, 2);
          Flags := Flags or DT_VCENTER;
        end else
        if Align in [alLeft, alRight] then
        begin
          Inc(DrawRect.Top, 2 + TCrackJvDockVSBlock(Block).InactiveBlockWidth);
          DrawRect.Left := DrawRect.Right + 1 - ((ChannelWidth - FontHeight) div 2);
          Dec(DrawRect.Bottom, 2);
          DrawRect.Right := DrawRect.Left + (DrawRect.Bottom - DrawRect.Top);
        end;

        OldGraphicsMode := SetGraphicsMode(Canvas.Handle, GM_ADVANCED);
        Canvas.Brush.Style := bsClear;

        TextColor := CurrentSkin.GetTextColor(skncButton, State);
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
//  if not (SkinManager.GetSkinType in [sknSkin, sknDelphiStyle]) then begin
//    inherited;
//    Exit;
//  end;

  IsVertical := Align in [alLeft, alRight];
  SpDrawXPDock(Canvas, ClientRect, IsVertical);
  //CurrentSkin.PaintBackground(Canvas, ClientRect, skncDock, sknsNormal, True, True, IsVertical);

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
Var
  TM: TTextMetric;
begin
  if GetTextMetrics(Canvas.Handle, TM) then
    FontHeight := TM.tmHeight
  else begin
    FontHeight := Abs(Canvas.Font.Height);
    if Canvas.Font.Height < 0 then
      Inc(FontHeight, 3); //tmInternalLeading
  end;
  if Align in [alLeft, alRight] then
    Canvas.Font.Handle := SpCreateRotatedFont(Canvas.Handle);
end;

procedure TJvDockVSChannelSpTBX.WMSpSkinChange(var Message: TMessage);
begin
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
      Result := Bounds(0, (Height - GripSize) div 2, Width, GripSize)
    else
      Result := Bounds((Width - GripSize) div 2, 0, GripSize, Height);
  end;

var
  R, DragHandleR : TRect;
  IsVertical : Boolean;
  C1, C2 : TColor;
begin
//  if not (SkinManager.GetSkinType in [sknSkin, sknDelphiStyle]) then begin
//    inherited;
//    Exit;
//  end;

  IsVertical := Align in [alLeft, alRight];

  // Paint background
  if SkinManager.GetSkinType = sknSkin then
    CurrentSkin.PaintBackground(Canvas, ClientRect, skncSplitter, sknsNormal,
     True, False, IsVertical)
  else begin
    Canvas.Brush.Color := CurrentSkin.GetThemedSystemColor(clBtnFace);
    Canvas.FillRect(ClientRect);

    PaintSplitterFrame(Canvas, IsVertical, R);
  end;

  // Paint grip
  R := GetGripRect(IsVertical);
  DragHandleR := R;
  if IsVertical then
    InflateRect(DragHandleR, -1, -10)
  else
    InflateRect(DragHandleR, -10, -1);

    if SkinManager.GetSkinType = sknSkin then begin
      if FMouseLoc in [mlOver, mlDown]  then
        CurrentSkin.PaintBackground(Canvas, R, skncButton, sknsNormal, True, True, False, [akLeft, akTop, akRight, akBottom]);
      C1 := SkinManager.CurrentSkin.Options(skncToolbarGrip).Body.Color1;
      C2 := SkinManager.CurrentSkin.Options(skncToolbarGrip).Body.Color2;
    end
    else begin
      C1 := CurrentSkin.GetThemedSystemColor(clBtnShadow);
      C2 := CurrentSkin.GetThemedSystemColor(clWindow);
    end;
    SpDrawXPGrip(Canvas, DragHandleR, C1, C2);

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
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    GrabbersSize := Max(Abs(NonClientMetrics.lfSmCaptionFont.lfHeight) +
      MulDiv(8, Screen.PixelsPerInch, 96), 18);
end;

{ TJvDockVSNETTabSheetSpTBX }

procedure TJvDockVSNETTabSheetSpTBX.AdjustClientRect(var Rect: TRect);
begin
  if Assigned(PageControl) {and (SkinManager.GetSkinType in [sknSkin, sknDelphiStyle])} then begin
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
//  if not (SkinManager.GetSkinType in [sknSkin, sknDelphiStyle]) then begin
//    inherited;
//    Exit;
//  end;

  if not DoubleBuffered or (Message.wParam = WPARAM(Message.lParam)) then begin
    if not (csDestroying in ComponentState) then begin
      if Assigned(PageControl) and Visible then begin
        ACanvas := TCanvas.Create;
        ACanvas.Handle := TWMEraseBkgnd(Message).DC;
        try
          R := ClientRect;
          ACanvas.Brush.Color := clWhite;
          ACanvas.FillRect(R);

          // Part of the background will be painted by the TabPanel, since we want the
          // tabs to be "merged" into the background
          if (PageControl as TJvDockVSNETTabPageControlSpTBX).TabPosition = tpBottom then
            Inc(R.Bottom, TabSheetBorderSize)
          else
            Dec(R.Top, TabSheetBorderSize);

          SpDrawXPTabControlBackground(ACanvas, R, Color, (PageControl as TJvDockVSNETTabPageControlSpTBX).TabPosition = tpBottom);
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
