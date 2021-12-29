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
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  JvComponentBase,
  JvDockControlForm,
  SpTBXSkins,
  SpTBXItem;

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
  private
    { Private declarations }
  protected
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  public
    { Public declarations }
    HasFocus : Boolean;
    ImageName: string;
    procedure CreateFormIcon;
    procedure ScaleForPPI(NewPPI: Integer); override;
  end;

var
  IDEDockWindow: TIDEDockWindow;
  BorderHighlight : TColor;
  BorderNormal : TColor;

implementation

uses
  Vcl.Themes,
  SVG,
  SVGIconImageCollection,
  frmPyIDEMain,
  uCommonFunctions;

{$R *.dfm}

function SvgToIcon(SVGText: string; Size: integer; FixedColor: TColor): HIcon;
begin
   var SvgIcon := TSVG.Create;
   try
     SvgIcon.FixedColor := SvgFixedColor(FixedColor);
     SvgIcon.ApplyFixedColorToRootOnly := True;
     SvgIcon.LoadFromText(SvgText);
     Result := SvgIcon.RenderToIcon(Size);
   finally
      SvgIcon.Free;
   end;
end;

procedure TIDEDockWindow.WMSpSkinChange(var Message: TMessage);
begin
  Assert(SkinManager.GetSkinType(nil) <> sknSkin);
  CreateFormIcon;

  if IsStyledWindowsColorDark then begin
    BorderHighlight := StyleServices.GetSystemColor(clBtnHighlight);
    BorderNormal := StyleServices.GetSystemColor(clBtnFace);
  end else begin
    BorderHighlight := StyleServices.GetSystemColor(clBtnShadow);
    BorderNormal := StyleServices.GetSystemColor(clBtnFace);
  end;
  if HasFocus then begin
    BGPanel.Color := BorderHighlight;
    //FGPanel.Margins.SetBounds(2,2,2,2);
  end else begin
    BGPanel.Color := BorderNormal;
    //FGPanel.Margins.SetBounds(0,0,0,0);
  end;
  Invalidate;
end;

procedure TIDEDockWindow.FormActivate(Sender: TObject);
begin
  HasFocus := True;
  BGPanel.Color := BorderHighlight;
  //FGPanel.Margins.SetBounds(2,2,2,2);
end;

procedure TIDEDockWindow.FormCreate(Sender: TObject);
begin
  DockClient.DockStyle := PyIDEMainForm.DockServer.DockStyle;
  BorderHighlight := StyleServices.GetSystemColor(clBtnHighlight);
  BorderNormal := StyleServices.GetSystemColor(clBtnShadow);

  CreateFormIcon;

  SkinManager.AddSkinNotification(Self, True);

  DockClient.OnConjoinHostFormCreated := DockClientConjoinHostFormCreated;
end;

procedure TIDEDockWindow.FormDeactivate(Sender: TObject);
begin
  HasFocus := False;
  BGPanel.Color := BorderNormal;
  //FGPanel.Margins.SetBounds(0,0,0,0);
  // Set the MouseleaveHide option
  // It may have been reset when a dock form is shown via the keyboard or menu
  PyIDEMainForm.JvDockVSNetStyleSpTBX.ChannelOption.MouseleaveHide := True;
end;

procedure TIDEDockWindow.FormDestroy(Sender: TObject);
begin
  SkinManager.RemoveSkinNotification(Self);
end;

procedure TIDEDockWindow.ScaleForPPI(NewPPI: Integer);
begin
  DockClient.LRDockWidth := MulDiv(DockClient.LRDockWidth, NewPPI, FCurrentPPI);
  DockClient.TBDockHeight := MulDiv(DockClient.TBDockHeight, NewPPI, FCurrentPPI);
  inherited;
end;

procedure TIDEDockWindow.DockClientTabHostFormCreated(
  DockClient: TJvDockClient; TabHost: TJvDockTabHostForm);
begin
  TabHost.TBDockHeight := DockClient.TBDockHeight;
  TabHost.LRDockWidth := DockClient.LRDockWidth;
  TabHost.FormStyle := fsNormal;
  TabHost.PopupMode := pmExplicit;
  TabHost.PopupParent := PyIDEMainForm;
end;

procedure TIDEDockWindow.CreateFormIcon;
begin
   if ImageName <> '' then
   begin
     var SVGItem := (PyIDEMainForm.vilImages.ImageCollection
       as TSVGIconImageCollection).SVGIconItems.GetIconByName(ImageName);
     var Details := StyleServices.GetElementDetails(ttTabItemNormal);
     var Color: TColor;
     if not StyleServices.GetElementColor(Details, ecTextColor, Color) then
       Color := StyleServices.GetSystemColor(clBtnText);
     Icon.Handle := SvgToIcon(SVGItem.SVGText, PPIScale(20), Color);
   end;
end;

procedure TIDEDockWindow.DockClientConjoinHostFormCreated(
  DockClient: TJvDockClient; ConjoinHost: TJvDockConjoinHostForm);
begin
  ConjoinHost.FormStyle := fsNormal;
  ConjoinHost.PopupMode := pmExplicit;
  ConjoinHost.PopupParent := PyIDEMainForm;
end;

end.
