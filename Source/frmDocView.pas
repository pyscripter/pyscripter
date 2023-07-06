{-----------------------------------------------------------------------------
 Unit Name: frmDocView
 Author:    Kiriakos Vlahos
 Date:      09-May-2005
 Purpose:   HTML documentation Editor View
 History:
-----------------------------------------------------------------------------}

unit frmDocView;

interface

uses
  WinApi.Windows,
  WinApi.Messages,
  WinApi.ActiveX,
  Winapi.WebView2,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.ImageList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.OleCtrls,
  Vcl.ImgList,
  Vcl.BaseImageCollection,
  Vcl.VirtualImageList,
  Vcl.Edge,
  TB2Item,
  TB2Dock,
  TB2Toolbar,
  SpTBXItem,
  uEditAppIntfs;

type
  TDocForm = class(TForm, IEditorView)
    TBXDock1: TSpTBXDock;
    TBXToolbar1: TSpTBXToolbar;
    ToolButtonForward: TSpTBXItem;
    ToolButtonBack: TSpTBXItem;
    TBXSeparatorItem1: TSpTBXSeparatorItem;
    TBXItem3: TSpTBXItem;
    TBXSeparatorItem2: TSpTBXSeparatorItem;
    TBXItem5: TSpTBXItem;
    TBXSeparatorItem4: TSpTBXSeparatorItem;
    TBXItem7: TSpTBXItem;
    BrowserImages: TVirtualImageList;
    WebBrowser: TEdgeBrowser;
    procedure FormCreate(Sender: TObject);
    procedure ToolButtonBackClick(Sender: TObject);
    procedure ToolButtonForwardClick(Sender: TObject);
    procedure ToolButtonStopClick(Sender: TObject);
    procedure ToolButtonPrintClick(Sender: TObject);
    procedure ToolButtonSaveClick(Sender: TObject);
    procedure WebBrowserCreateWebViewCompleted(Sender: TCustomEdgeBrowser; AResult:
        HRESULT);
    procedure WebBrowserExecuteScript(Sender: TCustomEdgeBrowser; AResult: HRESULT;
        const AResultObjectAsJson: string);
    procedure WebBrowserHistoryChanged(Sender: TCustomEdgeBrowser);
  private
    { Private declarations }
    FSaveFileName: string;
    procedure UpdateView(Editor : IEditor);
  public
    { Public declarations }
  end;


  TDocView = class(TInterfacedObject, IEditorViewFactory)
  private
    function CreateForm(Editor: IEditor; AOwner : TComponent): TCustomForm;
    function GetName : string;
    function GetTabCaption : string;
    function GetMenuCaption : string;
    function GetHint : string;
    function GetImageName : string;
    function GetShortCut : TShortCut;
    procedure GetContextHighlighters(List : TList);
  end;

  Var
    DocForm : TDocForm;

implementation

uses
  System.IOUtils,
  System.NetEncoding,
  JvJVCLUtils,
  JvGnugettext,
  StringResources,
  uCommonFunctions,
  cPyControl,
  cPyScripterSettings,
  dmResources;

{$R *.dfm}

procedure TDocForm.FormCreate(Sender: TObject);
begin
  WebBrowser.UserDataFolder := TPath.Combine(TPyScripterSettings.UserDataPath,
    'WebView2');
end;

procedure TDocForm.ToolButtonBackClick(Sender: TObject);
begin
  WebBrowser.GoBack;
end;

procedure TDocForm.ToolButtonForwardClick(Sender: TObject);
begin
  WebBrowser.GoForward;
end;

procedure TDocForm.ToolButtonStopClick(Sender: TObject);
begin
  WebBrowser.Stop;
end;

procedure TDocForm.ToolButtonPrintClick(Sender: TObject);
begin
  WebBrowser.ExecuteScript('window.print();');
end;

procedure TDocForm.ToolButtonSaveClick(Sender: TObject);
begin
  if ResourcesDataModule.GetSaveFileName(FSaveFileName, ResourcesDataModule.SynWebHtmlSyn, 'html') then
    WebBrowser.ExecuteScript('encodeURIComponent(document.documentElement.outerHTML)');
end;

procedure TDocForm.UpdateView(Editor: IEditor);
var
  Py: IPyEngineAndGIL;
  HTML : string;
  module : Variant;
  Cursor : IInterface;
begin
  if not Assigned(Editor) then Exit;

  Py := GI_PyControl.SafePyEngine;
  Cursor := WaitCursor;

  module := PyControl.ActiveInterpreter.ImportModule(Editor);
  HTML := PyControl.ActiveInterpreter.PyInteractiveInterpreter.htmldoc(module);

  WebBrowser.CreateWebView;
  while WebBrowser.BrowserControlState in [TEdgeBrowser.TBrowserControlState.None,
    TEdgeBrowser.TBrowserControlState.Creating]
  do
    Application.ProcessMessages;

  WebBrowser.NavigateToString(HTML);
end;

procedure TDocForm.WebBrowserCreateWebViewCompleted(Sender: TCustomEdgeBrowser;
    AResult: HRESULT);
begin
  if WebBrowser.BrowserControlState <> TEdgeBrowser.TBrowserControlState.Created then
    StyledMessageDlg(_(SWebView2Error), mtError, [mbOK], 0);
end;

procedure TDocForm.WebBrowserExecuteScript(Sender: TCustomEdgeBrowser; AResult:
    HRESULT; const AResultObjectAsJson: string);
begin
  if (FSaveFileName <> '') and (AResultObjectAsJson <> 'null') then
  begin
    var SL := TSmartPtr.Make(TStringList.Create);
    SL.Text := TNetEncoding.URL.Decode(AResultObjectAsJson.DeQuotedString('"'));
    SL.WriteBOM := False;
    SL.SaveToFile(FSaveFileName, TEncoding.UTF8);
    FSaveFileName := '';
  end;
end;

procedure TDocForm.WebBrowserHistoryChanged(Sender: TCustomEdgeBrowser);
begin
  ToolButtonBack.Enabled := WebBrowser.CanGoBack;
  ToolButtonForward.Enabled := WebBrowser.CanGoForward;
end;

{ TDocView }

function TDocView.CreateForm(Editor: IEditor; AOwner : TComponent): TCustomForm;
begin
  Result := TDocForm.Create(AOwner);
end;

function TDocView.GetImageName: string;
begin
  Result := 'PyDoc';
end;

procedure TDocView.GetContextHighlighters(List: TList);
begin
  List.Add(ResourcesDataModule.SynPythonSyn);
end;

function TDocView.GetHint: string;
begin
  Result := _(SDocumentationHint);
end;

function TDocView.GetMenuCaption: string;
begin
  Result := _(SDocumentation)
end;

function TDocView.GetName: string;
begin
  Result := 'Documentation';
end;

function TDocView.GetTabCaption: string;
begin
  Result := _(SDocTab);
end;

function TDocView.GetShortCut: TShortCut;
begin
  Result := 0;
end;

initialization
  //  This unit must be initialized after frmEditor
  if Assigned(GI_EditorFactory) then
    GI_EditorFactory.RegisterViewFactory(TDocView.Create as IEditorViewFactory);
  OleInitialize(nil);

finalization
  OleUninitialize;

end.

