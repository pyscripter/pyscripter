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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvDockControlForm, ExtCtrls, SpTBXSkins,
  JvComponentBase, SpTBXItem;

type
  TIDEDockWindow = class(TForm)
    DockClient: TJvDockClient;
    FGPanel: TPanel;
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
  end;

var
  IDEDockWindow: TIDEDockWindow;
  BorderHighlight : TColor;
  BorderNormal : TColor;

implementation

uses frmPyIDEMain, uCommonFunctions;

{$R *.dfm}

procedure TIDEDockWindow.WMSpSkinChange(var Message: TMessage);
begin
  if SkinManager.IsDefaultSkin then begin
    BorderHighlight := clBtnShadow;
    BorderNormal := clBtnFace;
  end else begin
    BorderHighlight := GetHotColor(CurrentSkin.Options(skncTab, sknsCheckedAndHotTrack).Body);
    BorderNormal := CurrentSkin.Options(skncTab, sknsChecked).Body.Color1;
    if BorderHighlight = clNone then
      BorderHighlight := CurrentSkin.Options(skncPanel, sknsNormal).Body.Color1;
    if BorderNormal = clNone then
      BorderNormal := CurrentSkin.Options(skncPanel, sknsNormal).Body.Color1;
  end;
  if HasFocus then begin
    Color := BorderHighlight;
    //FGPanel.Margins.SetBounds(2,2,2,2);
  end else begin
    Color := BorderNormal;
    //FGPanel.Margins.SetBounds(0,0,0,0);
  end;
  Invalidate;
end;

procedure TIDEDockWindow.FormActivate(Sender: TObject);
begin
  HasFocus := True;
  Color := BorderHighlight;
  //FGPanel.Margins.SetBounds(2,2,2,2);
end;

procedure TIDEDockWindow.FormCreate(Sender: TObject);
begin
  //SetVistaContentFonts(FGPanel.Font);
  FGPanel.ControlStyle := FGPanel.ControlStyle + [csOpaque];

  //FGPanelExit(Self);
  BorderHighlight := clBtnShadow;
  BorderNormal := clBtnFace;

  SkinManager.AddSkinNotification(Self);

  DockClient.OnConjoinHostFormCreated := DockClientConjoinHostFormCreated;
end;

procedure TIDEDockWindow.FormDeactivate(Sender: TObject);
begin
  HasFocus := False;
  Color := BorderNormal;
  //FGPanel.Margins.SetBounds(0,0,0,0);
  // Set the MouseleaveHide option
  // It may have been reset when a dock form is shown via the keyboard or menu
  PyIDEMainForm.JvDockVSNetStyleSpTBX.ChannelOption.MouseleaveHide := True;
end;

procedure TIDEDockWindow.FormDestroy(Sender: TObject);
begin
  SkinManager.RemoveSkinNotification(Self);
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

procedure TIDEDockWindow.DockClientConjoinHostFormCreated(
  DockClient: TJvDockClient; ConjoinHost: TJvDockConjoinHostForm);
begin
  ConjoinHost.FormStyle := fsNormal;
  ConjoinHost.PopupMode := pmExplicit;
  ConjoinHost.PopupParent := PyIDEMainForm;
end;

end.









