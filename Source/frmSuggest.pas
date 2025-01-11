unit frmSuggest;

interface

uses
  Winapi.Messages,
  System.Classes,
  System.ImageList,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  TB2Item,
  TB2Dock,
  TB2Toolbar,
  SpTBXItem,
  SynEdit;

type
  TSuggestWindow = class(TForm)
    seSuggest: TSynEdit;
    SpTBXDock: TSpTBXDock;
    SpTBXToolbar: TSpTBXToolbar;
    vilImages: TVirtualImageList;
    spiAccept: TSpTBXItem;
    spiCancel: TSpTBXItem;
    SpTBXRightAlignSpacerItem1: TSpTBXRightAlignSpacerItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    spiAcceptLine: TSpTBXItem;
    spiAcceptWord: TSpTBXItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure spiAcceptClick(Sender: TObject);
    procedure spiAcceptLineClick(Sender: TObject);
    procedure spiAcceptWordClick(Sender: TObject);
    procedure spiCancelClick(Sender: TObject);
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    FEditor: TCustomSynEdit;
    procedure CreateParams(var Params: TCreateParams); override;
  end;

procedure ShowSuggestion(const Suggestion: string; Editor: TCustomSynEdit);

var
  SuggestWindow: TSuggestWindow;

implementation

{$R *.dfm}

uses
  Winapi.Windows,
  System.Types,
  System.Math,
  SynEditTypes,
  SynEditKeyCmds,
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
      Message.Result := HTRIGHT;
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

procedure TSuggestWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  SuggestWindow := nil;
end;

procedure TSuggestWindow.spiAcceptClick(Sender: TObject);
begin
  FEditor.SelText := seSuggest.Text;
  Close;
end;

procedure TSuggestWindow.spiAcceptLineClick(Sender: TObject);
begin
  var Line := seSuggest.Lines[0];
  if seSuggest.Lines.Count > 1 then
    Line := Line + sLineBreak;
  seSuggest.Lines.Delete(0);
  FEditor.SelText := Line;
  if seSuggest.Lines.Count = 0 then
    Close;
end;

procedure TSuggestWindow.spiAcceptWordClick(Sender: TObject);
begin
  seSuggest.BlockBegin := BufferCoord(0, 0);
  seSuggest.ExecuteCommand(ecSelWordRight, ' ', nil);
  var FirstWord := seSuggest.SelText;
  seSuggest.SelText := '';
  var TrimTrailingSpaces := eoTrimTrailingSpaces in FEditor.Options;
  FEditor.Options := FEditor.Options - [eoTrimTrailingSpaces];
  FEditor.SelText := FirstWord;
  if TrimTrailingSpaces then
    FEditor.Options := FEditor.Options + [eoTrimTrailingSpaces];

  if seSuggest.Text = '' then
    Close;
end;

procedure TSuggestWindow.spiCancelClick(Sender: TObject);
begin
  Close;
end;


procedure ShowSuggestion(const Suggestion: string; Editor: TCustomSynEdit);
const
  MaxLines = 10;
  BordersSize = 8;
begin
  if not Assigned(SuggestWindow) then
    SuggestWindow := TSuggestWindow.Create(Application.MainForm);

  SuggestWindow.FEditor := Editor;
  if Editor.SelAvail then
  begin
    SuggestWindow.spiAcceptWord.Visible := False;
    SuggestWindow.spiAcceptWord.Enabled := False;
    SuggestWindow.spiAcceptLine.Visible := False;
    SuggestWindow.spiAcceptLine.Enabled := False;
  end;


  var Monitor := Screen.MonitorFromWindow(Editor.Handle);
  var BC := Editor.BlockBegin;
  var DC := Editor.BufferToDisplayPos(BC);
  var Point := Editor.RowColumnToPixels(DC);
  var SP := Editor.ClientToScreen(Point);

  SuggestWindow.seSuggest.Text := Suggestion;
  var LineCount := Min(MaxLines, SuggestWindow.seSuggest.Lines.Count);

  SuggestWindow.ScaleForPPI(Editor.CurrentPPI);
  // Window size
  var Height := LineCount * SuggestWindow.seSuggest.LineHeight +
    SuggestWindow.SpTBXToolbar.Height + BordersSize;
  var Width := SuggestWindow.seSuggest.CharWidth * 80 + BordersSize;
  // Window position
  var Left := Min(SP.X, Monitor.Left + Monitor.Width - Width);
  var Top := SP.Y - Height;
  if Top < Monitor.Top then
     Top := SP.Y + Editor.LineHeight;
  SuggestWindow.SetBounds(Left, Top, Width, Height);

  SuggestWindow.ShowModal;
end;

end.
