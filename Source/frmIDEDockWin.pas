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
  // The available kinds of IDE dock Windows
  TIDEDockWindowKind = (
    ideInterpreter,
    ideCallStack,
    ideVariables,
    ideWatches,
    ideBreakpoints,
    ideCommandOutput,
    ideMessages,
    ideCodeExplorer,
    ideFileExplorer,
    ideTodo,
    ideRegExp,
    ideUnitTests,
    ideSearchResults,
    ideProjectExplorer,
    ideLLMChat);

  TIDEDockWindow = class;
  TIDEDockWindowType = class of TIDEDockWindow;
  TRegisteredDockWinClasses = array[TIDEDockWindowKind] of TIDEDockWindowType;
  TIDEDockWindows = array[TIDEDockWindowKind] of TIDEDockWindow;

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
  private
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  private
    class var FRegisteredClasses: TRegisteredDockWinClasses;
    class var FIDEDocWindows: TIDEDockWindows;
  public
    BorderHighlight: TColor;
    BorderNormal: TColor;
    HasFocus: Boolean;
    ImageName: string;
    procedure CreateFormIcon;
    procedure StoreSettings(AppStorage: TJvCustomAppStorage); virtual;
    procedure RestoreSettings(AppStorage: TJvCustomAppStorage); virtual;
    // class methods
    class procedure RegisterDockWinClass(Kind: TIDEDockWindowKind;
      AClass: TIDEDockWindowType);
    class function CreateInstance: TIDEDockWindow; virtual; abstract;
    class procedure CreateDockForms(MainForm: TForm);
    class procedure FreeDockForms;
    class Procedure StoreFormSettings(AppStorage: TJvCustomAppStorage);
    class Procedure RestoreFormSettings(AppStorage: TJvCustomAppStorage);
    class function DockForm(Kind: TIDEDockWindowKind): TIDEDockWindow;
  end;

  // short form
  function IDEDockForm(Kind: TIDEDockWindowKind): TIDEDockWindow;

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

class procedure TIDEDockWindow.FreeDockForms;
begin
  for var Kind := High(TIDEDockWindowKind) downto Low(TIDEDockWindowKind) do
    if Assigned(FIDEDocWindows[Kind]) then
        FIDEDocWindows[Kind].Free;
end;

class procedure TIDEDockWindow.RegisterDockWinClass(Kind: TIDEDockWindowKind;
  AClass: TIDEDockWindowType);
begin
  FRegisteredClasses[Kind] := AClass;
end;

class procedure TIDEDockWindow.RestoreFormSettings(
  AppStorage: TJvCustomAppStorage);
begin
  for var Kind := Low(TIDEDockWindowKind) to High(TIDEDockWindowKind) do
    if Assigned(FIDEDocWindows[Kind]) then
      FIDEDocWindows[Kind].RestoreSettings(AppStorage);
end;

procedure TIDEDockWindow.RestoreSettings(AppStorage: TJvCustomAppStorage);
begin
  // Empty at the base class
end;

class procedure TIDEDockWindow.StoreFormSettings(
  AppStorage: TJvCustomAppStorage);
begin
  for var Kind := Low(TIDEDockWindowKind) to High(TIDEDockWindowKind) do
    if Assigned(FIDEDocWindows[Kind]) then
      FIDEDocWindows[Kind].StoreSettings(AppStorage);
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

class function TIDEDockWindow.DockForm(
  Kind: TIDEDockWindowKind): TIDEDockWindow;
begin
  Result := FIDEDocWindows[Kind];
end;

procedure TIDEDockWindow.CMParentFontChanged(var Message: TCMParentFontChanged);
{ Invoked when Application.DefaultFont changes }
begin
  Font.Height := MulDiv(Application.DefaultFont.Height, FCurrentPPI,
    Screen.PixelsPerInch);
end;

class procedure TIDEDockWindow.CreateDockForms(MainForm: TForm);
begin
  for var Kind := Low(TIDEDockWindowKind) to High(TIDEDockWindowKind) do
    if Assigned(FRegisteredClasses[Kind]) then
    begin
      FIDEDocWindows[Kind] := FRegisteredClasses[Kind].CreateInstance;
      FIDEDocWindows[Kind].PopupParent := MainForm;
    end;
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


function IDEDockForm(Kind: TIDEDockWindowKind): TIDEDockWindow;
begin
  Result := TIDEDockWindow.DockForm(Kind);
end;


end.
