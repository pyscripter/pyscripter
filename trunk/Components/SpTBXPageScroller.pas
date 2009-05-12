{-----------------------------------------------------------------------------
 Unit Name: SpTBXPageScroller
 Author:    Kiriakos
 Date:      04-May-2009
 Purpose:   Port of TBXPageScroller to SpTBXLib
 History:
-----------------------------------------------------------------------------}

unit SpTBXPageScroller;

interface
{$I TB2Ver.inc}

uses
  Windows, Messages, Classes, Graphics, Controls, StdCtrls, ExtCtrls, Forms,
  TB2Dock, TB2Item, SpTBXSkins;


const
  { New hit test constants for page scrollers }
  HTSCROLLPREV = 30;
  HTSCROLLNEXT = 31;

  { Page scroll button types }
  PSBT_UP                    = 1;
  PSBT_DOWN                  = 2;
  PSBT_LEFT                  = 3;
  PSBT_RIGHT                 = 4;

type
  TSpTBXPageScrollerOrientation = (tpsoVertical, tpsoHorizontal);
  TSpTBXPageScrollerButtons = set of (tpsbPrev, tpsbNext);

  TSpTBXCustomPageScroller = class(TWinControl)
  private
    FAutoRangeCount: Integer;
    FAutoRange: Boolean;
    FAutoScroll: Boolean;
    FButtonSize: Integer;
    FMargin: Integer;
    FOrientation: TSpTBXPageScrollerOrientation;
    FPosition: Integer;
    FPosRange: Integer;
    FRange: Integer;
    FScrollDirection: Integer;
    FScrollCounter: Integer;
    FScrollPending: Boolean;
    FScrollTimer: TTimer;
    FUpdatingButtons: Boolean;
    FVisibleButtons: TSpTBXPageScrollerButtons;
    procedure CalcAutoRange;
    function  IsRangeStored: Boolean;
    procedure ScrollTimerTimer(Sender: TObject);
    procedure SetButtonSize(Value: Integer);
    procedure SetAutoRange(Value: Boolean);
    procedure SetOrientation(Value: TSpTBXPageScrollerOrientation);
    procedure SetPosition(Value: Integer);
    procedure SetRange(Value: Integer);
    procedure StopScrolling;
    procedure ValidatePosition(var NewPos: Integer);
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMNCMouseLeave(var Message: TMessage); message $2A2 {WM_NCMOUSELEAVE};
    procedure WMNCMouseMove(var Message: TWMNCMouseMove); message WM_NCMOUSEMOVE;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure AlignControls(AControl: TControl; var ARect: TRect); override;
    function  AutoScrollEnabled: Boolean; virtual;
    procedure BeginScrolling(HitTest: Integer);
    function  CalcClientArea: TRect;
    function  CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure ConstrainedResize(var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoSetRange(Value: Integer); virtual;
    procedure DrawNCArea(const DrawToDC: Boolean; const ADC: HDC; const Clip: HRGN); virtual;
    procedure HandleScrollTimer; virtual;
    procedure Loaded; override;
    procedure RecalcNCArea;
    procedure Resizing; virtual;
    procedure UpdateButtons;
    property AutoScroll: Boolean read FAutoScroll write FAutoScroll default True;
    property ButtonSize: Integer read FButtonSize write SetButtonSize default 10;
    property Orientation: TSpTBXPageScrollerOrientation read FOrientation write SetOrientation default tpsoVertical;
    property Position: Integer read FPosition write SetPosition default 0;
    property Margin: Integer read FMargin write FMargin default 0;
    property Range: Integer read FRange write SetRange stored IsRangeStored;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DisableAutoRange;
    procedure EnableAutoRange;
    procedure ScrollToCenter(ARect: TRect); overload;
    procedure ScrollToCenter(AControl: TControl); overload;
    property AutoRange: Boolean read FAutoRange write SetAutoRange default False;
  end;

  TSpTBXPageScroller = class(TSpTBXCustomPageScroller)
  public
    property Position;
  published
    property Align;
    property Anchors;
    property AutoRange;
    property AutoScroll;
    property ButtonSize;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DoubleBuffered;
    property Enabled;
    property Ctl3D;
    property Font;
    property Margin;
    property Orientation;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Range;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
{$IFDEF JR_D5}
    property OnContextPopup;
{$ENDIF}
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;


procedure Register;

implementation
Uses
  SysUtils, TB2Common, UxTheme, Themes;

procedure Register;
begin
  RegisterComponents('PyScripter Custom', [TSpTBXPageScroller]);
end;

const
  ScrollDelay = 300;
  ScrollInterval = 75;{ TSpTBXCustomPageScroller }

function GetMinControlHeight(Control: TControl): Integer;
begin
  if Control.Align = alClient then
    Result := Control.Constraints.MinHeight
  else
    Result := Control.Height;
end;

function GetMinControlWidth(Control: TControl): Integer;
begin
  if Control.Align = alClient then
    Result := Control.Constraints.MinWidth
  else
    Result := Control.Width;
end;

procedure SpTBXPaintPageScrollButton(Canvas: TCanvas;
  const ARect: TRect; ButtonType: Integer; Hot: Boolean);
var
  R: TRect;
  Flags: Integer;
  X, Y, Sz: Integer;
begin
  R := ARect;
  case SkinManager.GetSkinType of
    sknNone:
      begin
        if Hot then Flags := DFCS_FLAT
        else Flags := 0;
        case ButtonType of
          PSBT_UP: Flags := Flags or DFCS_SCROLLUP;
          PSBT_DOWN: Flags := Flags or DFCS_SCROLLDOWN;
          PSBT_LEFT: Flags := Flags or DFCS_SCROLLLEFT;
          PSBT_RIGHT: Flags := Flags or DFCS_SCROLLRIGHT;
        end;
        Windows.DrawFrameControl(Canvas.Handle, R, DFC_SCROLL, Flags);
      end;
    sknWindows:
      begin
        if Hot then Flags := TS_PRESSED
        else Flags := TS_HOT;
        DrawThemeBackground(ThemeServices.Theme[teToolBar], Canvas.Handle, TP_BUTTON, Flags, ARect, nil);
        if Hot then Canvas.Pen.Color := clBtnText;
      end;
    sknSkin :
      begin
        SpDrawXPButton(Canvas, R, True, False, Hot, False, False, False, sknSkin);
        if Hot then
          Canvas.Pen.Color := CurrentSkin.GetTextColor(skncButton, sknsHotTrack, sknSkin)
        else
          Canvas.Pen.Color := CurrentSkin.GetTextColor(skncButton, sknsNormal, sknSkin);
      end;
  end;
  if SkinManager.GetSkinType in [sknWindows, sknSkin] then begin
    X := (R.Left + R.Right) div 2;
    Y := (R.Top + R.Bottom) div 2;
    Sz := Min(X - R.Left, Y - R.Top) * 3 div 4;
    Canvas.Brush.Color := Canvas.Pen.Color;
    case ButtonType of
      PSBT_UP:
        begin
          Inc(Y, Sz div 2);
          Canvas.Polygon([Point(X + Sz, Y), Point(X, Y - Sz), Point(X - Sz, Y)]);
        end;
      PSBT_DOWN:
        begin
          Y := (R.Top + R.Bottom - 1) div 2;
          Dec(Y, Sz div 2);
          Canvas.Polygon([Point(X + Sz, Y), Point(X, Y + Sz), Point(X - Sz, Y)]);
        end;
      PSBT_LEFT:
        begin
          Inc(X, Sz div 2);
          Canvas.Polygon([Point(X, Y + Sz), Point(X - Sz, Y), Point(X, Y - Sz)]);
        end;
      PSBT_RIGHT:
        begin
          X := (R.Left + R.Right - 1) div 2;
          Dec(X, Sz div 2);
          Canvas.Polygon([Point(X, Y + Sz), Point(X + Sz, Y), Point(X, Y - Sz)]);
        end;
    end;
  end;
end;


procedure TSpTBXCustomPageScroller.AdjustClientRect(var Rect: TRect);
begin
  if Orientation = tpsoVertical then
  begin
    if tpsbPrev in FVisibleButtons then Dec(Rect.Top, ButtonSize);
    if tpsbNext in FVisibleButtons then Inc(Rect.Bottom, ButtonSize);
    OffsetRect(Rect, 0, -Position);
    if Range > Rect.Bottom - Rect.Top then Rect.Bottom := Rect.Top + Range;
  end
  else
  begin
    if tpsbPrev in FVisibleButtons then Dec(Rect.Left, ButtonSize);
    if tpsbNext in FVisibleButtons then Inc(Rect.Right, ButtonSize);
    OffsetRect(Rect, -Position, 0);
    if Range > Rect.Right - Rect.Left then Rect.Right := Rect.Left + Range;
  end;
end;

procedure TSpTBXCustomPageScroller.AlignControls(AControl: TControl; var ARect: TRect);
begin
  CalcAutoRange;
  UpdateButtons;
  ARect := ClientRect;
  inherited AlignControls(AControl, ARect);
end;

function TSpTBXCustomPageScroller.AutoScrollEnabled: Boolean;
begin
  Result := not AutoSize and not (DockSite and UseDockManager);
end;

procedure TSpTBXCustomPageScroller.BeginScrolling(HitTest: Integer);
var
  Msg: TMsg;
begin
  if HitTest = HTSCROLLPREV then FScrollDirection := -1 else FScrollDirection := 1;
  try
    SetCapture(Handle);
    FScrollCounter := FScrollDirection * 8;
    FScrollPending := True;
    FScrollTimer.Enabled := True;
    DrawNCArea(False, 0, 0);
    HandleScrollTimer;
    FScrollPending := True;
    FScrollTimer.Interval := ScrollDelay;

    while GetCapture = Handle do
    begin
      case Integer(GetMessage(Msg, 0, 0, 0)) of
        -1: Break;
        0: begin
             PostQuitMessage(Msg.WParam);
             Break;
           end;
      end;
      case Msg.Message of
        WM_KEYDOWN, WM_KEYUP: if Msg.WParam = VK_ESCAPE then Break;
        WM_LBUTTONDOWN, WM_LBUTTONDBLCLK: begin
            Break;
          end;
        WM_LBUTTONUP:
          begin
            Break;
          end;
        WM_RBUTTONDOWN..WM_MBUTTONDBLCLK:;
        WM_TIMER:
          begin
            HandleScrollTimer;
          end;
      else
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
    end;
  finally
    StopScrolling;
    if GetCapture = Handle then ReleaseCapture;
  end;
end;

procedure TSpTBXCustomPageScroller.CalcAutoRange;
var
  I: Integer;
  Bias: Integer;
  NewRange, AlignMargin: Integer;
  CW, CH: Integer;
  Control: TControl;
begin
  if (FAutoRangeCount <= 0) and AutoRange then
  begin
    if AutoScrollEnabled then
    begin
      NewRange := 0;
      AlignMargin := 0;
      if Position > 0 then Bias := ButtonSize
      else Bias := 0;
      CW := ClientWidth;
      CH := ClientHeight;
      DisableAlign;
      for I := 0 to ControlCount - 1 do
      begin
        Control := Controls[I];
        if Control.Visible or (csDesigning in Control.ComponentState) and
          not (csNoDesignVisible in Control.ControlStyle) then
        begin
          if Orientation = tpsoVertical then
          begin
            if Control.Align in [alTop, alBottom, alClient] then
              Control.Width := CW;
            case Control.Align of
              alTop, alNone:
                if (Control.Align = alTop) or (Control.Anchors * [akTop, akBottom] = [akTop]) then
                  NewRange := Max(NewRange, Position + Control.Top + Control.Height + Bias);
              alBottom: Inc(AlignMargin, Control.Height);
              alClient: Inc(AlignMargin, GetMinControlHeight(Control));
            end
          end
          else
          begin
            if Control.Align in [alLeft, alRight, alClient] then
              Control.Height := CH;
            case Control.Align of
              alLeft, alNone:
                if (Control.Align = alLeft) or (Control.Anchors * [akLeft, akRight] = [akLeft]) then
                  NewRange := Max(NewRange, Position + Control.Left + Control.Width + Bias);
              alRight: Inc(AlignMargin, Control.Width);
              alClient: Inc(AlignMargin, GetMinControlWidth(Control));
            end;
          end;
        end;
      end;
      EnableAlign;
      DoSetRange(NewRange + AlignMargin + Margin);
    end
    else DoSetRange(0);
  end;
end;

function TSpTBXCustomPageScroller.CalcClientArea: TRect;
begin
  Result := ClientRect;
  if Orientation = tpsoVertical then
  begin
    if tpsbPrev in FVisibleButtons then Dec(Result.Top, ButtonSize);
    if tpsbNext in FVisibleButtons then Inc(Result.Bottom, ButtonSize);
  end
  else
  begin
    if tpsbPrev in FVisibleButtons then Dec(Result.Left, ButtonSize);
    if tpsbNext in FVisibleButtons then Inc(Result.Right, ButtonSize);
  end;
end;

function TSpTBXCustomPageScroller.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := NewHeight > FButtonSize * 3;
end;

procedure TSpTBXCustomPageScroller.ConstrainedResize(var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer);
begin
  // do not call inherited here
end;

constructor TSpTBXCustomPageScroller.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls, csClickEvents, csDoubleClicks];
  FAutoScroll := True;
  FButtonSize := 10;
  FScrollTimer := TTimer.Create(Self);
  FScrollTimer.Enabled := False;
  FScrollTimer.Interval := 60;
  FScrollTimer.OnTimer := ScrollTimerTimer;
  Width := 64;
  Height := 64;
  SkinManager.AddSkinNotification(Self);
end;

procedure TSpTBXCustomPageScroller.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do style := style and not (CS_HREDRAW or CS_VREDRAW);
end;

destructor TSpTBXCustomPageScroller.Destroy;
begin
  SkinManager.RemoveSkinNotification(Self);
  inherited;
end;

procedure TSpTBXCustomPageScroller.DisableAutoRange;
begin
  Inc(FAutoRangeCount);
end;

procedure TSpTBXCustomPageScroller.DoSetRange(Value: Integer);
begin
  FRange := Value;
  if FRange < 0 then FRange := 0;
  UpdateButtons;
end;

procedure TSpTBXCustomPageScroller.DrawNCArea(const DrawToDC: Boolean;
  const ADC: HDC; const Clip: HRGN);
const
  CBtns: array [TSpTBXPageScrollerOrientation, Boolean] of Integer =
    ((PSBT_UP, PSBT_DOWN), (PSBT_LEFT, PSBT_RIGHT));
var
  DC: HDC;
  R, CR, BR: TRect;
  ACanvas: TCanvas;
  PrevBtnSize, NextBtnSize: Integer;
begin
  if FVisibleButtons = [] then Exit;
  if not DrawToDC then DC := GetWindowDC(Handle)
  else DC := ADC;
  try
    GetWindowRect(Handle, R);
    OffsetRect(R, -R.Left, -R.Top);
    if not DrawToDC then
    begin
      SelectNCUpdateRgn(Handle, DC, Clip);
      CR := R;
      PrevBtnSize := 0;
      NextBtnSize := 0;
      if tpsbPrev in FVisibleButtons then PrevBtnSize := ButtonSize;
      if tpsbNext in FVisibleButtons then NextBtnSize := ButtonSize;
      if Orientation = tpsoVertical then
      begin
        Inc(CR.Top, PrevBtnSize);
        Dec(CR.Bottom, NextBtnSize);
      end
      else
      begin
        Inc(CR.Left, PrevBtnSize);
        Dec(CR.Right, NextBtnSize);
      end;
      with CR do ExcludeClipRect(DC, Left, Top, Right, Bottom);
    end;

    ACanvas := TCanvas.Create;
    try
      ACanvas.Handle := DC;
      ACanvas.Brush.Color := Color;
      ACanvas.FillRect(R);

      if tpsbPrev in FVisibleButtons then
      begin
        BR := R;
        if Orientation = tpsoVertical then BR.Bottom := BR.Top + ButtonSize
        else BR.Right := BR.Left + ButtonSize;
        SpTBXPaintPageScrollButton(ACanvas, BR, CBtns[Orientation, False],
          FScrollDirection < 0);
      end;
      if tpsbNext in FVisibleButtons then
      begin
        BR := R;
        if Orientation = tpsoVertical then BR.Top := BR.Bottom - ButtonSize
        else BR.Left := BR.Right - ButtonSize;
        SpTBXPaintPageScrollButton(ACanvas, BR, CBtns[Orientation, True],
          FScrollDirection > 0);
      end;
    finally
      ACanvas.Handle := 0;
      ACanvas.Free;
    end;
  finally
    if not DrawToDC then ReleaseDC(Handle, DC);
  end;
end;

procedure TSpTBXCustomPageScroller.EnableAutoRange;
begin
  if FAutoRangeCount > 0 then
  begin
    Dec(FAutoRangeCount);
    if FAutoRangeCount = 0 then CalcAutoRange;
  end;
end;

procedure TSpTBXCustomPageScroller.HandleScrollTimer;
var
  Pt: TPoint;
  R: TRect;
  OldPosition: Integer;
  OldDirection: Integer;
begin
  GetCursorPos(Pt);
  GetWindowRect(Handle, R);
  if not PtInRect(R, Pt) then
  begin
    StopScrolling;
  end
  else if FScrollDirection = 0 then
  begin
    FScrollTimer.Enabled := False;
    FScrollCounter := 0;
  end
  else
  begin
    OldPosition := Position;
    OldDirection := FScrollDirection;
    if ((FScrollDirection > 0) and (FScrollCounter < 0)) or
      ((FScrollDirection < 0) and (FScrollCounter > 0)) then FScrollCounter := 0;
    if FScrollDirection > 0 then Inc(FScrollCounter)
    else Dec(FScrollCounter);
    Position := Position + FScrollCounter;
    if Position = OldPosition then
    begin
      ReleaseCapture;
      FScrollTimer.Enabled := False;
      DrawNCArea(False, 0, 0);
    end
    else
    begin
      if FScrollPending or (FScrollDirection * OldDirection <= 0) or
        (FScrollDirection * OldDirection <= 0) then
        DrawNCArea(False, 0, 0);
    end;
  end;
  if FScrollPending then FScrollTimer.Interval := ScrollInterval;
  FScrollPending := False;
end;

function TSpTBXCustomPageScroller.IsRangeStored: Boolean;
begin
  Result := not AutoRange;
end;

procedure TSpTBXCustomPageScroller.Loaded;
begin
  inherited;
  UpdateButtons;
end;

procedure TSpTBXCustomPageScroller.RecalcNCArea;
begin
  SetWindowPos(Handle, 0, 0, 0, 0, 0,
    SWP_FRAMECHANGED or SWP_NOACTIVATE or SWP_NOZORDER or SWP_NOMOVE or SWP_NOSIZE);
end;

procedure TSpTBXCustomPageScroller.Resizing;
begin
  // do nothing by default
end;

procedure TSpTBXCustomPageScroller.ScrollTimerTimer(Sender: TObject);
begin
  HandleScrollTimer;
end;

procedure TSpTBXCustomPageScroller.ScrollToCenter(ARect: TRect);
var
  X, Y: Integer;
begin
  if Orientation = tpsoVertical then
  begin
    if ARect.Bottom - ARect.Top < Range then Y := (ARect.Top + ARect.Bottom) div 2
    else Y := ARect.Top;
    Position := Position + Y - Height div 2;
  end
  else
  begin
    if ARect.Right - ARect.Left < Range then X := (ARect.Left + ARect.Right) div 2
    else X := ARect.Left;
    Position := Position + X - Width div 2;
  end;
end;

procedure TSpTBXCustomPageScroller.ScrollToCenter(AControl: TControl);
var
  R: TRect;
begin
  R := AControl.ClientRect;
  R.TopLeft := ScreenToClient(AControl.ClientToScreen(R.TopLeft));
  R.BottomRight := ScreenToClient(AControl.ClientToScreen(R.BottomRight));
  ScrollToCenter(R);
end;

procedure TSpTBXCustomPageScroller.SetAutoRange(Value: Boolean);
begin
  if FAutoRange <> Value then
  begin
    FAutoRange := Value;
    if Value then CalcAutoRange else Range := 0;
  end;
end;

procedure TSpTBXCustomPageScroller.SetButtonSize(Value: Integer);
begin
  if FButtonSize <> Value then
  begin
    FButtonSize := Value;
    UpdateButtons;
  end;
end;

procedure TSpTBXCustomPageScroller.SetOrientation(Value: TSpTBXPageScrollerOrientation);
begin
  if Orientation <> Value then
  begin
    FOrientation := Value;
    Realign;
  end;
end;

procedure TSpTBXCustomPageScroller.SetPosition(Value: Integer);
var
  OldPos: Integer;
begin
  if csReading in ComponentState then FPosition := Value
  else
  begin
    ValidatePosition(Value);
    if FPosition <> Value then
    begin
      OldPos := FPosition;
      FPosition := Value;

      if OldPos > 0 then Inc(OldPos, ButtonSize);
      if Value > 0 then Inc(Value, ButtonSize);

      if Orientation = tpsoHorizontal then ScrollBy(OldPos - Value, 0)
      else ScrollBy(0, OldPos - Value);
      UpdateButtons;
    end;
  end;
end;

procedure TSpTBXCustomPageScroller.SetRange(Value: Integer);
begin
  FAutoRange := False;
  DoSetRange(Value);
end;

procedure TSpTBXCustomPageScroller.StopScrolling;
begin
  if (FScrollDirection <> 0) or (FScrollCounter <> 0) or (FScrollTimer.Enabled) then
  begin
    FScrollDirection := 0;
    FScrollCounter := 0;
    FScrollTimer.Enabled := False;
    if HandleAllocated and IsWindowVisible(Handle) then DrawNCArea(False, 0, 0);
  end;
end;

procedure TSpTBXCustomPageScroller.UpdateButtons;
var
  Sz: Integer;
  OldVisibleButtons: TSpTBXPageScrollerButtons;
  RealignNeeded: Boolean;
begin
  RealignNeeded := False;
  if not FUpdatingButtons and HandleAllocated then
  try
    FUpdatingButtons := True;
    if Orientation = tpsoHorizontal then Sz := Width
    else Sz := Height;
    OldVisibleButtons := FVisibleButtons;
    FVisibleButtons := [];

    FPosRange := Range - Sz;
    if FPosRange < 0 then FPosRange := 0;
    if FPosition > FPosRange - 1 then
    begin
      FPosition := FPosRange;
      RealignNeeded := True;
    end;

    if Sz > ButtonSize * 3 then
    begin
      if Position > 0 then Include(FVisibleButtons, tpsbPrev);
      if Range - Position > Sz then Include(FVisibleButtons, tpsbNext);
    end;
    if FVisibleButtons <> OldVisibleButtons then
    begin
      RecalcNCArea;
      RealignNeeded := True;
    end;
  finally
    FUpdatingButtons := False;
    if RealignNeeded then Realign;
  end;
end;

procedure TSpTBXCustomPageScroller.ValidatePosition(var NewPos: Integer);
begin
  if NewPos < 0 then NewPos := 0;
  if NewPos > FPosRange then NewPos := FPosRange;
end;

procedure TSpTBXCustomPageScroller.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  if Color = clNone then
  begin
    SpDrawParentBackground(Self, Message.DC, ClientRect);
    Message.Result := 1;
  end
  else inherited;
end;

procedure TSpTBXCustomPageScroller.WMMouseMove(var Message: TWMMouseMove);
begin
  if AutoScroll then StopScrolling;
  inherited;
end;

procedure TSpTBXCustomPageScroller.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  with Message.CalcSize_Params^ do
  begin
    if Orientation = tpsoVertical then
    begin
      if tpsbPrev in FVisibleButtons then Inc(rgrc[0].Top, ButtonSize);
      if tpsbNext in FVisibleButtons then Dec(rgrc[0].Bottom, ButtonSize);
    end
    else
    begin
      if tpsbPrev in FVisibleButtons then Inc(rgrc[0].Left, ButtonSize);
      if tpsbNext in FVisibleButtons then Dec(rgrc[0].Right, ButtonSize);
    end;
    Message.Result := 0;
  end;
end;

procedure TSpTBXCustomPageScroller.WMNCHitTest(var Message: TWMNCHitTest);
var
  Pt: TPoint;
  R: TRect;
begin
  DefaultHandler(Message);
  with Message do if Result <> HTCLIENT then
  begin
    Pt := SmallPointToPoint(Pos);
    GetWindowRect(Handle, R);
    if PtInRect(R, Pt) then
    begin
      if (tpsbPrev in FVisibleButtons) then
      begin
        if Orientation = tpsoVertical then
        begin
          if Pt.Y < R.Top + ButtonSize then Result := HTSCROLLPREV
        end
        else
        begin
          if Pt.X < R.Left + ButtonSize then Result := HTSCROLLPREV
        end;
      end;
      if (tpsbNext in FVisibleButtons) then
      begin
        if Orientation = tpsoVertical then
        begin
          if Pt.Y >= R.Bottom - ButtonSize then Result := HTSCROLLNEXT;
        end
        else
        begin
          if Pt.X >= R.Right - ButtonSize then Result := HTSCROLLNEXT;
        end;
      end;
    end;
  end;
end;

procedure TSpTBXCustomPageScroller.WMNCLButtonDown(var Message: TWMNCLButtonDown);
begin
  if (Win32MajorVersion >= 5) or
     (Win32MajorVersion = 4) and (Win32MinorVersion >= 10) then
    CallTrackMouseEvent(Handle, TME_LEAVE or $10 {TME_NONCLIENT});

  if not AutoScroll and (Message.HitTest in [HTSCROLLPREV, HTSCROLLNEXT]) then
    BeginScrolling(Message.HitTest)
  else
    inherited;
end;

procedure TSpTBXCustomPageScroller.WMNCMouseLeave(var Message: TMessage);
begin
  if AutoScroll then StopScrolling;
  inherited;
end;

procedure TSpTBXCustomPageScroller.WMNCMouseMove(var Message: TWMNCMouseMove);
var
  OldScrollDirection: Integer;
begin
  if (Win32MajorVersion >= 5) or
     (Win32MajorVersion = 4) and (Win32MinorVersion >= 10) then
    CallTrackMouseEvent(Handle, TME_LEAVE or $10 {TME_NONCLIENT});

  if AutoScroll then
  begin
    OldScrollDirection := FScrollDirection;
    case Message.HitTest of
      HTSCROLLPREV: FScrollDirection := -1;
      HTSCROLLNEXT: FScrollDirection := 1;
    else
      StopScrolling;
      inherited;
      Exit;
    end;
    if OldScrollDirection <> FScrollDirection then
    begin
      FScrollCounter := 0;
      FScrollPending := True;
      FScrollTimer.Interval := ScrollDelay;
      FScrollTimer.Enabled := True;
      DrawNCArea(False, 0, 0);
    end;
  end;
end;

procedure TSpTBXCustomPageScroller.WMNCPaint(var Message: TMessage);
begin
  DrawNCArea(False, 0, HRGN(Message.WParam));
end;

procedure TSpTBXCustomPageScroller.WMSize(var Message: TWMSize);
begin
  FUpdatingButtons := True;
  try
    CalcAutoRange;
  finally
    FUpdatingButtons := False;
  end;
  Inc(FAutoRangeCount);
  inherited;
  Resizing;
  Dec(FAutoRangeCount);
end;


procedure TSpTBXCustomPageScroller.WMSpSkinChange(var Message: TMessage);
begin
   RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_FRAME);
end;

end.
