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
  Dialogs, JvComponent, JvDockControlForm, ExtCtrls, TBX, TBXThemes,
  JvComponentBase;

type
  TIDEDockWindow = class(TForm)
    DockClient: TJvDockClient;
    FGPanel: TPanel;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DockClientTabHostFormCreated(DockClient: TJvDockClient;
      TabHost: TJvDockTabHostForm);
    procedure FormDeactivate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure TBMThemeChange(var Message: TMessage); message TBM_THEMECHANGE;
  public
    { Public declarations }
    HasFocus : Boolean;
  end;

var
  IDEDockWindow: TIDEDockWindow;

implementation

uses frmPyIDEMain, uCommonFunctions, JvDockGlobals;

{$R *.dfm}

procedure TIDEDockWindow.FormResize(Sender: TObject);
begin
  with FGPanel do begin
    if (Top <> 3) or (Left <> 3) or (Width <> Self.ClientWidth - 6)
      or (Height <> Self.ClientHeight - 6)
    then begin
      Anchors :=[];
      Top := 3;
      Left := 3;
      Width := Self.ClientWidth - 6;
      Height := Self.ClientHeight - 6;
      Anchors :=[akLeft, akRight, akTop, akBottom];
    end;
  end;
end;

procedure TIDEDockWindow.TBMThemeChange(var Message: TMessage);
begin
  if Message.WParam = TSC_VIEWCHANGE then begin
    if HasFocus then
      Color := CurrentTheme.GetItemColor(GetItemInfo('hot'))
        //Color := GetBorderColor('active')
    else
      Color := GetBorderColor('inactive');
      //Color := CurrentTheme.GetItemColor(GetItemInfo('inactive'));
    Invalidate;
  end;
end;

procedure TIDEDockWindow.FormActivate(Sender: TObject);
begin
  HasFocus := True;
  Color := CurrentTheme.GetItemColor(GetItemInfo('hot'));
end;

procedure TIDEDockWindow.FormCreate(Sender: TObject);
begin
  SetDesktopIconFonts(Self.Font);  // For Vista
  SetVistaContentFonts(FGPanel.Font);
  FGPanel.ControlStyle := FGPanel.ControlStyle + [csOpaque];

  FormResize(Self);
  //FGPanelExit(Self);

  AddThemeNotification(Self);
end;

procedure TIDEDockWindow.FormDeactivate(Sender: TObject);
begin
  HasFocus := False;
  //Color := CurrentTheme.GetItemColor(GetItemInfo('inactive'));
  Color := GetBorderColor('inactive');
end;

procedure TIDEDockWindow.FormDestroy(Sender: TObject);
begin
  RemoveThemeNotification(Self);
end;

procedure TIDEDockWindow.DockClientTabHostFormCreated(
  DockClient: TJvDockClient; TabHost: TJvDockTabHostForm);
begin
  TabHost.TBDockHeight := DockClient.TBDockHeight;
  TabHost.LRDockWidth := DockClient.LRDockWidth;
end;

end.


