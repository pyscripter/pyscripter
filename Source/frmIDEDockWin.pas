{-----------------------------------------------------------------------------
 Unit Name: frmIDEDockWin
 Author:    Kiriakos Vlahos
 Date:      18-Mar-2005
 Purpose:   Base form for docked windows
 History:
-----------------------------------------------------------------------------}

unit frmIDEDockWin;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  JvComponentBase,
  JvDockControlForm,
  JvAppStorage,
  SpTBXSkins;

type
  TIDEDockWindow = class(TForm)
    DockClient: TJvDockClient;
    FGPanel: TPanel;
    BGPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DockClientTabHostFormCreated(DockClient: TJvDockClient;
      TabHost: TJvDockTabHostForm);
    procedure DockClientConjoinHostFormCreated(DockClient: TJvDockClient;
      ConjoinHost: TJvDockConjoinHostForm);
    procedure FormDeactivate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure CMParentFontChanged(var Message: TCMParentFontChanged); message CM_PARENTFONTCHANGED;
  protected
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  public
    BorderHighlight: TColor;
    BorderNormal: TColor;
    HasFocus: Boolean;
    ImageName: string;
    procedure CreateFormIcon;
    procedure StoreSettings(AppStorage: TJvCustomAppStorage); virtual;
    procedure RestoreSettings(AppStorage: TJvCustomAppStorage); virtual;
  end;

implementation

uses
  System.Types,
  Vcl.Themes,
  Vcl.SysStyles,
  SVGInterfaces,
  dmResources,
  uCommonFunctions;

{$R *.dfm}

function SvgToIcon(SVG: ISVG; Size: Integer; FixedColor: TColor): HICON;
begin
  var LBitmap := TSmartPtr.Make(TBitmap.Create)();
  LBitmap.PixelFormat := TPixelFormat.pf32bit;   // 32bit bitmap
  LBitmap.AlphaFormat := TAlphaFormat.afDefined; // Enable alpha channel

  LBitmap.SetSize(Size, Size);

  // Fill background with transparent
  LBitmap.Canvas.Brush.Color := clNone;
  LBitmap.Canvas.FillRect(Rect(0, 0, Size, Size));

  SVG.FixedColor := SvgFixedColor(FixedColor);
  SVG.ApplyFixedColorToRootOnly := True;
  SVG.PaintTo(LBitmap.Canvas.Handle, TRectF.Create(0, 0, Size, Size));
  Result := BmpToIcon(LBitmap.Handle);
end;

procedure TIDEDockWindow.WMSpSkinChange(var Message: TMessage);
begin
  CreateFormIcon;

  StyledBorderColors(BorderNormal, BorderHighlight);
  if HasFocus then
    BGPanel.Color := BorderHighlight
  else
    BGPanel.Color := BorderNormal;
  Invalidate;
end;

procedure TIDEDockWindow.FormActivate(Sender: TObject);
begin
  HasFocus := True;
  BGPanel.Color := BorderHighlight;
end;

procedure TIDEDockWindow.FormCreate(Sender: TObject);
begin
  PopupParent := Application.MainForm;

  DockClient.DockStyle := ResourcesDataModule.DockStyle;
  StyledBorderColors(BorderNormal, BorderHighlight);

  CreateFormIcon;

  SkinManager.AddSkinNotification(Self, True);

  Font.Assign(Application.DefaultFont);
  Font.Height := MulDiv(Font.Height, FCurrentPPI, Font.PixelsPerInch);

  DockClient.OnConjoinHostFormCreated := DockClientConjoinHostFormCreated;
end;

procedure TIDEDockWindow.FormDeactivate(Sender: TObject);
begin
  HasFocus := False;
  BGPanel.Color := BorderNormal;
  //FGPanel.Margins.SetBounds(0,0,0,0);
  // Set the MouseleaveHide option
  // It may have been reset when a dock form is shown via the keyboard or menu
  ResourcesDataModule.DockStyle.ChannelOption.MouseleaveHide := True;
end;

procedure TIDEDockWindow.FormDestroy(Sender: TObject);
begin
  SkinManager.RemoveSkinNotification(Self);
end;

procedure TIDEDockWindow.RestoreSettings(AppStorage: TJvCustomAppStorage);
begin
  // Empty at the base class
end;

procedure TIDEDockWindow.StoreSettings(AppStorage: TJvCustomAppStorage);
begin
  // Empty at the base class
end;

procedure TIDEDockWindow.DockClientTabHostFormCreated(
  DockClient: TJvDockClient; TabHost: TJvDockTabHostForm);
begin
  TabHost.TBDockHeight := DockClient.TBDockHeight;
  TabHost.LRDockWidth := DockClient.LRDockWidth;
  TabHost.FormStyle := fsNormal;
  TabHost.PopupMode := pmExplicit;
  TabHost.PopupParent := Application.MainForm;
end;

procedure TIDEDockWindow.CMParentFontChanged(var Message: TCMParentFontChanged);
{ Invoked when Application.DefaultFont changes }
begin
  Font.Height := MulDiv(Application.DefaultFont.Height, FCurrentPPI,
    Screen.PixelsPerInch);
end;

procedure TIDEDockWindow.CreateFormIcon;
begin
   if ImageName <> '' then
   begin
     var SVGItem := ResourcesDataModule.icSVGImages.SVGIconItems.GetIconByName(ImageName);
     var Details := StyleServices.GetElementDetails(ttTabItemNormal);
     var Color: TColor;
     if not StyleServices.GetElementColor(Details, ecTextColor, Color) then
       Color := StyleServices.GetSystemColor(clBtnText);
     Icon.Handle := SvgToIcon(SVGItem.SVG, PPIScale(20), Color);
   end;
end;

procedure TIDEDockWindow.DockClientConjoinHostFormCreated(
  DockClient: TJvDockClient; ConjoinHost: TJvDockConjoinHostForm);
begin
  ConjoinHost.FormStyle := fsNormal;
  ConjoinHost.PopupMode := pmExplicit;
  ConjoinHost.PopupParent := Application.MainForm;
end;

end.
