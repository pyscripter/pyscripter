{****************************
 *                          *
 * Autor: Adrian M. Hanslik *
 * Date 02. May 1999        *
 * Version: 1.0             *
 *                          *
 ****************************}

unit AMHLEDVecStd;

interface

uses
  System.Types, System.UITypes, Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs;

type
  TBorderStyle = (bsNone, bsStd, esPro);

  TLEDStyle = (lsEllipse, lsRectangle);

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  TAMHLEDVecStd = class(TGraphicControl)
  private
    { Private-Deklarationen }
    fBorderStyle: TBorderStyle;
    fColorEffect: Boolean;
    fColorEffectOff: TColor;
    fColorEffectOn: TColor;
    fColorLeftTop: TColor;
    fColorRightBottom: TColor;
    fLEDActive: Boolean;
    fLEDColorOff: TColor;
    fLEDColorOn: TColor;
    fLEDStyle: TLEDStyle;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetColorEffect(Value: Boolean);
    procedure SetColorEffectOff(Value: TColor);
    procedure SetColorEffectOn(Value: TColor);
    procedure SetColorLeftTop(Value: TColor);
    procedure SetColorRightBottom(Value: TColor);
    procedure SetLEDActive(Value: Boolean);
    procedure SetLEDColorOff(Value: TColor);
    procedure SetLEDColorOn(Value: TColor);
    procedure SetLEDStyle(Value: TLEDStyle);
  protected
    { Protected-Deklarationen }
    procedure DrawEllipseBorder;
    procedure DrawEllipseEffect;
    procedure DrawRectangleBorder;
    procedure DrawRectangleEffect;
    procedure Paint; override;
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published-Deklarationen }
    property BorderStyle: TBorderStyle read fBorderStyle write SetBorderStyle;
    property ColorEffect: Boolean read fColorEffect write SetColorEffect;
    property ColorEffectOff: TColor read fColorEffectOff write SetColorEffectOff;
    property ColorEffectOn: TColor read fColorEffectOn write SetColorEffectOn;
    property ColorLeftTop: TColor read fColorLeftTop write SetColorLeftTop;
    property ColorRightBottom: TColor read fColorRightBottom write SetColorRightBottom;
    property LEDActive: Boolean read fLEDActive write SetLEDActive;
    property LEDColorOff: TColor read fLEDColorOff write SetLEDColorOff;
    property LEDColorOn: TColor read fLEDColorOn write SetLEDColorOn;
    property LEDStyle: TLEDStyle read fLEDStyle write SetLEDStyle;
    property ShowHint;
    property Visible;
    property PopupMenu;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

procedure Register;

implementation

constructor TAMHLEDVecStd.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height := 16;
  Width := 16;
  fBorderStyle := bsNone;
  fColorEffect := True;
  fColorEffectOff := clLtGray;
  fColorEffectOn := clLtGray;
  fColorLeftTop := clLtGray;
  fColorRightBottom := clDkGray;
  fLEDActive := True;
  fLEDColorOn := clGreen;
  fLEDColorOff := clRed;
  fLEDStyle := lsEllipse;
end;

destructor TAMHLEDVecStd.Destroy;
begin
  inherited Destroy;
end;

procedure TAMHLEDVecStd.DrawEllipseBorder;
begin
  with Canvas do begin
       if fBorderStyle = bsNone then begin
          Pen.Color := Brush.Color;
          Arc(0, 0, Width, Height, 0, 0, 0, 0)
       end else if fBorderStyle = bsStd then begin
                   Pen.Color := clBlack;
                   Arc(0, 0, Width, Height, 0, 0, 0, 0)
       end else begin
                  Pen.Color := fColorLeftTop;
                  Arc(0, 0, Width, Height, Width, 0, 0, Height);
                  Pen.Color := fColorRightBottom;
                  Arc(0, 0, Width, Height, 0, Height, Width, 0);
       end;
  end;
end;

procedure TAMHLEDVecStd.DrawEllipseEffect;
var
  R: TRect;
begin
  with Canvas do begin
       if fColorEffect = True then begin
          if fLEDActive = True then
             Brush.Color := fColorEffectOn
          else
              Brush.Color := fColorEffectOff;
          Pen.Color := Brush.Color;
          R := GetClientRect;
          InflateRect (R, -Width DIV 5, -Height DIV 5);
          Chord(R.Left, R.Top, R.Right, R.Bottom,
                Width DIV 2, Height DIV 5, Width DIV 5, Height DIV 2);
       end;
  end;
end;

procedure TAMHLEDVecStd.DrawRectangleBorder;
begin
  with Canvas do begin
       if fBorderStyle = bsNone then begin
          Pen.Color := Brush.Color;
          PolyLine([Point(0, 0), Point(Width - 1, 0),
                    Point(Width - 1, Height - 1), Point(0, Height - 1), Point(0, 0)]);
       end else if fBorderStyle = bsStd then begin
                   Pen.Color := clBlack;
                   PolyLine([Point(0, 0), Point(Width - 1, 0),
                             Point(Width - 1, Height - 1), Point(0, Height - 1), Point(0, 0)]);
       end else begin
                  Pen.Color := fColorLeftTop;
                  MoveTo(0, 0);
                  LineTo(Width, 0);
                  MoveTo(0, 0);
                  LineTo(0, Height);
                  Pen.Color := fColorRightBottom;
                  MoveTo(Width, Height);
                  LineTo(Width, 1);
                  MoveTo(Width, Height);
                  LineTo(1, Height);
       end;
  end;
end;

procedure TAMHLEDVecStd.DrawRectangleEffect;
var
  R: TRect;
begin
  with Canvas do begin
       if fColorEffect = True then begin
          if fLEDActive = True then
             Brush.Color := fColorEffectOn
          else
              Brush.Color := fColorEffectOff;
          Pen.Color := Brush.Color;
          R := GetClientRect;
          InflateRect (R, -Width DIV 5, -Height DIV 5);
          Polygon([Point(R.Left, R.Top), Point(R.Right div 2, R.Top),
                   Point(R.Left, R.Bottom div 2)]);
       end;
  end;
end;

procedure TAMHLEDVecStd.Paint;
begin
  inherited Paint;
  with Canvas do begin
       if fLEDActive = True then
          Brush.Color := fLEDColorOn
       else
           Brush.Color := fLEDColorOff;
  end;
  case fLEDStyle of
       lsEllipse: begin
                    Canvas.Ellipse(0, 0, Width, Height);
                    DrawEllipseBorder;
                    if fColorEffect <> False then
                       DrawEllipseEffect;
                  end;
       lsRectangle: begin
                      Canvas.Rectangle(0, 0, Width, Height);
                      DrawRectangleBorder;
                      if fColorEffect <> False then
                         DrawRectangleEffect;
                    end;
  end;
end;

procedure TAMHLEDVecStd.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> fBorderStyle then
     fBorderStyle := Value;
  Invalidate;
end;

procedure TAMHLEDVecStd.SetColorEffect(Value: boolean);
begin
  if Value <> fColorEffect then
     fColorEffect := Value;
  Invalidate;
end;

procedure TAMHLEDVecStd.SetColorEffectOff(Value: TColor);
begin
  if Value <> fColorEffectOff then
     fColorEffectOff := Value;
  Invalidate;
end;

procedure TAMHLEDVecStd.SetColorEffectOn(Value: TColor);
begin
  if Value <> fColorEffectOn then
     fColorEffectOn := Value;
  Invalidate;
end;

procedure TAMHLEDVecStd.SetColorLeftTop(Value: TColor);
begin
  if Value <> fColorLeftTop then
     fColorLeftTop := Value;
  Invalidate;
end;

procedure TAMHLEDVecStd.SetColorRightBottom(Value: TColor);
begin
  if Value <> fColorRightBottom then
     fColorRightBottom := Value;
  Invalidate;
end;

procedure TAMHLEDVecStd.SetLEDActive(Value: Boolean);
begin
  if Value <> fLEDActive then
     fLEDActive := Value;
  Invalidate;
end;

procedure TAMHLEDVecStd.SetLEDColorOff(Value: TColor);
begin
  if Value <> fLEDColorOff then
     fLEDColorOff := Value;
  Invalidate;
end;

procedure TAMHLEDVecStd.SetLEDColorOn(Value: TColor);
begin
  if Value <> fLEDColorOn then
     fLEDColorOn := Value;
  Invalidate;
end;

procedure TAMHLEDVecStd.SetLEDStyle(Value: TLEDStyle);
begin
  if Value <> fLEDStyle then
     fLEDStyle := Value;
  Invalidate;
end;

procedure Register;
begin
  RegisterComponents('PyScripter Custom', [TAMHLEDVecStd]);
end;

end.
