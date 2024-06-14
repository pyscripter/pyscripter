unit frmSuggest;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  TB2Dock,
  TB2Toolbar,
  SpTBXItem,
  SynEdit, TB2Item, System.ImageList, Vcl.ImgList, Vcl.VirtualImageList;

type
  TSuggestWindow = class(TForm)
    seSuggest: TSynEdit;
    SpTBXDock: TSpTBXDock;
    SpTBXToolbar: TSpTBXToolbar;
    vilImages: TVirtualImageList;
    spiAccept: TSpTBXItem;
    procedure FormCreate(Sender: TObject);
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
  end;

var
  SuggestWindow: TSuggestWindow;

implementation

{$R *.dfm}

uses
  dmResources;

procedure TSuggestWindow.FormCreate(Sender: TObject);
begin
  seSuggest.Highlighter := ResourcesDataModule.SynPythonSyn;
end;

procedure TSuggestWindow.WMNCHitTest(var Message: TWMNCHitTest);
//  Makes the form resizable
var
  D: Integer;
  P: TPoint;
begin
  D := GetSystemMetrics(SM_CXSIZEFRAME);

  P := ScreenToClient(Message.Pos);

  if P.Y < D then
  begin
    if P.X < D then
      Message.Result := HTTOPLEFT
    else if P.X > ClientWidth - D then
      Message.Result := HTTOPRIGHT
    else
      Message.Result := HTTOP;
  end
  else if P.Y > ClientHeight - D then
  begin
    if P.X < D then
      Message.Result := HTBOTTOMLEFT
    else if P.X > ClientWidth - D then
      Message.Result := HTBOTTOMRIGHT
    else
      Message.Result := HTBOTTOM;
  end
  else
  begin
    if P.X < D then
      Message.Result := HTLEFT
    else if P.X > ClientWidth - D then
      Message.Result := HTRIGHT
  end;

  if Message.Result = 0 then
    inherited;
end;

{ TSuggestWindow }

procedure TSuggestWindow.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
    {
      WS_THICKFRAME causes Windows 10 to display a 6 pixel title bar
      Also with VCL Styles and WS_BORDER the window is not resizable
      So we use WS_BORDER and make the window sizeable by handling WM_NCHITTEST
    }
    Style := WS_POPUP or WS_BORDER or WS_CLIPCHILDREN;
    ExStyle := WS_EX_TOOLWINDOW;

    // Only affects the first time you create the handle
    // https://stackoverflow.com/questions/44521877/window-class-style-cs-noclose-does-not-work-after-calling-to-recreatewnd
    WindowClass.style := WindowClass.style or CS_DROPSHADOW;
  end;

end;

end.
