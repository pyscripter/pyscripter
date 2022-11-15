{-----------------------------------------------------------------------------
 Unit Name: cCodeHint
 Author:    Kiriakos Vlahos
 Date:      08-Dec-2005
 Purpose:   Classes to support code hints with HTML formating
 History:
-----------------------------------------------------------------------------}

unit cCodeHint;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.Graphics,
  JvHTControls,
  JvTypes;

type
  TCodeHintWindow = class(THintWindow)
  class var FHintFont: TFont;
  public
    HtLabel: TJvHTLabel;
    class var OnHyperLinkClick: TJvHyperLinkClickEvent;
  private
    class procedure SetHintFont(const Value: TFont); static;
  protected
    procedure HyperLinkClick(Sender: TObject; LinkName: string);
  public
    constructor Create(AOwner: TComponent); override;
    function CalcHintRect(MaxWidth: Integer;
      const AHint: THintString; AData: Pointer): TRect; override;
    function ShouldHideHint: Boolean; override;
    procedure ActivateHintData(Rect: TRect; const AHint: string; AData: TCustomData); override;
    class property HintFont: TFont read FHintFont write SetHintFont;
    class constructor Create;
    class destructor Destroy;
  end;

implementation

uses
  Winapi.Messages,
  System.Types,
  System.Math,
  Vcl.Themes,
  uCommonFunctions;

//=== { TJvHTHintWindow } ====================================================

constructor TCodeHintWindow.Create(AOwner: TComponent);
begin
  Application.HintHidePause := 6000;
  inherited Create(AOwner);
  HtLabel := TJvHTLabel.Create(Self);
  HtLabel.Parent := Self;
  HtLabel.SetBounds(2, 2, 0, 0);
  HtLabel.OnHyperLinkClick := HyperLinkClick;

  HtLabel.Transparent := True;
end;

procedure TCodeHintWindow.HyperLinkClick(Sender: TObject; LinkName: string);
begin
  ShowWindow(Handle, SW_HIDE);
  Visible := False;
  Application.CancelHint;
  if Assigned(OnHyperLinkClick) then
    TThread.ForceQueue(nil, procedure
    begin
      OnHyperLinkClick(Sender, LinkName);
    end);
end;

class procedure TCodeHintWindow.SetHintFont(const Value: TFont);
begin
  FHintFont.Assign(Value);
end;

function TCodeHintWindow.ShouldHideHint: Boolean;
begin
  Result := not (HandleAllocated and IsWindowVisible(Handle) and
    BoundsRect.Contains(Mouse.CursorPos));
end;

procedure TCodeHintWindow.ActivateHintData(Rect: TRect; const AHint: string;
  AData: TCustomData);
begin
  var Monitor := Screen.MonitorFromPoint(Point(Rect.Left, Rect.Top));
  var LineHeight := NativeInt(AData);

  if Rect.Bottom > Monitor.Top + Monitor.Height then
    Rect.Offset(0, - Rect.Height - LineHeight);
  Rect.Top := Max(Monitor.Top, Rect.Top);
  Rect.Bottom := Min(Rect.Bottom, Monitor.Top + Monitor.Height);

  inherited ActivateHintData(Rect, '', AData);
end;

function TCodeHintWindow.CalcHintRect(MaxWidth: Integer;
  const AHint: THintString; AData: Pointer): TRect;
begin
  HtLabel.Font.Assign(FHintFont);
  if Screen.ActiveCustomForm <> nil then
    HtLabel.Font.Height := Muldiv(HtLabel.Font.Height,
      Screen.ActiveCustomForm.CurrentPPI, Screen.PixelsPerInch);

  var LStyle := StyleServices;
  var LDetails := LStyle.GetElementDetails(thHintNormal);
  var LColor: TColor;
  if LStyle.GetElementColor(LDetails, ecTextColor, LColor) and (LColor <> clNone) then
    HtLabel.Font.Color := LColor
  else
    HtLabel.Font.Color := Screen.HintFont.Color;

  HtLabel.Canvas.Pen.Color := HtLabel.Font.Color;
  HtLabel.Caption := AHint;
  Result := Bounds(0, 0, Min(HtLabel.Width, MaxWidth) + 6, HtLabel.Height + 2);
end;

class constructor TCodeHintWindow.Create;
begin
  FHintFont := TFont.Create;
  FHintFont.Name := DefaultCodeFontName;
  FHintFont.Size := 10;
end;

class destructor TCodeHintWindow.Destroy;
begin
  FHintFont.Free;
end;

end.
