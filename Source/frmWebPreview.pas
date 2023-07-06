{-----------------------------------------------------------------------------
 Unit Name: frmDocView
 Author:    Kiriakos Vlahos
 Date:      09-May-2005
 Purpose:   HTML documentation Editor View
 History:
-----------------------------------------------------------------------------}

unit frmWebPreview;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ActiveX,
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
  Vcl.VirtualImageList,
  Vcl.BaseImageCollection,
  Vcl.ImgList,
  Vcl.Edge,
  TB2Item,
  TB2Dock,
  TB2Toolbar,
  SpTBXItem,
  uEditAppIntfs,
  cTools;

type
  TWebPreviewForm = class(TForm, IEditorView)
    TBXDock1: TSpTBXDock;
    TBXToolbar1: TSpTBXToolbar;
    ToolButtonForward: TSpTBXItem;
    ToolButtonBack: TSpTBXItem;
    TBXSeparatorItem1: TSpTBXSeparatorItem;
    TBXItem3: TSpTBXItem;
    TBXSeparatorItem2: TSpTBXSeparatorItem;
    TBXItem5: TSpTBXItem;
    TBXItem7: TSpTBXItem;
    BrowserImages: TVirtualImageList;
    WebBrowser: TEdgeBrowser;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
    fEditor: IEditor;
    procedure UpdateView(Editor : IEditor);
  private
    FSaveFileName: string;
    class var FExternalTool: TExternalTool;
    class constructor Create;
    class destructor Destroy;
  public
    { Public declarations }
    const JupyterServerCaption = 'Jupyter Server';
    const JupyterServer = '$[PythonDir-Short]Scripts\Jupyter-notebook.exe';
  end;

  TWebPreviewView = class(TInterfacedObject, IEditorViewFactory)
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
  WebPreviewFactoryIndex : integer;

implementation

uses
  System.UITypes,
  System.IOUtils,
  System.NetEncoding,
  JvGnugettext,
  uCommonFunctions,
  StringResources,
  VarPyth,
  dmResources,
  frmCommandOutput,
  cPyScripterSettings;

{$R *.dfm}

class constructor TWebPreviewForm.Create;
begin
  FExternalTool := TExternalTool.Create;
  with FExternalTool do begin
    Caption := JupyterServerCaption;
    Description := Caption;
    ApplicationName := JupyterServer;
    Parameters := '--no-browser --NotebookApp.token=""';
    WorkingDirectory := '$[ActiveDoc-Short-Dir]';
    SaveFiles := sfActive;
    Context := tcAlwaysEnabled;
    ParseTraceback := False;
    CaptureOutput := True;
    ConsoleHidden := True;
  end;
end;

procedure TWebPreviewForm.FormCreate(Sender: TObject);
begin
  WebBrowser.UserDataFolder := TPath.Combine(TPyScripterSettings.UserDataPath,
    'WebView2');
end;

class destructor TWebPreviewForm.Destroy;
begin
  FExternalTool.Free;
end;

procedure TWebPreviewForm.FormDestroy(Sender: TObject);
begin
  inherited;
  if OutputWindow.IsRunning and (OutputWindow.RunningTool = FExternalTool.Caption) then
    OutputWindow.actToolTerminate.Execute;
end;

procedure TWebPreviewForm.ToolButtonBackClick(Sender: TObject);
begin
  WebBrowser.GoBack;
end;

procedure TWebPreviewForm.ToolButtonForwardClick(Sender: TObject);
begin
  WebBrowser.GoForward;
end;

procedure TWebPreviewForm.ToolButtonStopClick(Sender: TObject);
begin
  WebBrowser.Stop;
end;

procedure TWebPreviewForm.ToolButtonPrintClick(Sender: TObject);
begin
  WebBrowser.ExecuteScript('window.print();');
end;

procedure TWebPreviewForm.ToolButtonSaveClick(Sender: TObject);
begin
  if ResourcesDataModule.GetSaveFileName(FSaveFileName, ResourcesDataModule.SynWebHtmlSyn, 'html') then
    WebBrowser.ExecuteScript('encodeURIComponent(document.documentElement.outerHTML)');
end;

procedure TWebPreviewForm.UpdateView(Editor: IEditor);
begin
  fEditor := Editor;
  if Assigned(Editor.SynEdit.Highlighter) and
    (Editor.SynEdit.Highlighter = ResourcesDataModule.SynJSONSyn) then
  begin
    var FN := TPath.GetFileName(Editor.FileName);
    FN := StringReplace(FN, ' ', '%20', [rfReplaceAll]);
    WebBrowser.Navigate('http://localhost:8888/notebooks/'+FN);
  end else begin
    WebBrowser.CreateWebView;
    while WebBrowser.BrowserControlState in [TEdgeBrowser.TBrowserControlState.None,
      TEdgeBrowser.TBrowserControlState.Creating]
    do
      Application.ProcessMessages;
    WebBrowser.NavigateToString(Editor.SynEdit.Text);
  end;
end;

procedure TWebPreviewForm.WebBrowserCreateWebViewCompleted(Sender:
    TCustomEdgeBrowser; AResult: HRESULT);
begin
  if WebBrowser.BrowserControlState <> TEdgeBrowser.TBrowserControlState.Created then
    StyledMessageDlg(_(SWebView2Error), mtError, [mbOK], 0);
end;

procedure TWebPreviewForm.WebBrowserExecuteScript(Sender: TCustomEdgeBrowser;
    AResult: HRESULT; const AResultObjectAsJson: string);
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

procedure TWebPreviewForm.WebBrowserHistoryChanged(Sender: TCustomEdgeBrowser);
begin
  ToolButtonBack.Enabled := WebBrowser.CanGoBack;
  ToolButtonForward.Enabled := WebBrowser.CanGoForward;
end;

{ TDocView }

function TWebPreviewView.CreateForm(Editor: IEditor; AOwner : TComponent): TCustomForm;
begin
  if Assigned(Editor.SynEdit.Highlighter) and
    (Editor.SynEdit.Highlighter = ResourcesDataModule.SynJSONSyn) then
  begin
    if Editor.FileName = '' then
      (Editor as IFileCommands).ExecSave;
    if LowerCase(ExtractFileExt(Editor.FileName)) <> '.ipynb' then begin
      StyledMessageDlg(_(SOnlyJupyterFiles), mtError, [mbOK], 0);
      Abort;
    end;

    if not FileExists(GI_PyIDEServices.ReplaceParams(TWebPreviewForm.JupyterServer)) then
    begin
      StyledMessageDlg(_(SNoJupyter), mtError, [mbOK], 0);
      Abort;
    end;

    if OutputWindow.IsRunning then begin
      StyledMessageDlg(_(SExternalProcessRunning), mtError, [mbOK], 0);
      Abort;
    end;

    try
      OutputWindow.ExecuteTool(TWebPreviewForm.FExternalTool);
    except
      Abort;
    end;
  end;

  Result := TWebPreviewForm.Create(AOwner);
end;

function TWebPreviewView.GetImageName: string;
begin
  Result := 'Web';
end;

procedure TWebPreviewView.GetContextHighlighters(List: TList);
begin
  List.Add(ResourcesDataModule.SynWebHtmlSyn);
  List.Add(ResourcesDataModule.SynWebXmlSyn);
  List.Add(ResourcesDataModule.SynWebCssSyn);
  List.Add(ResourcesDataModule.SynJSONSyn);
end;

function TWebPreviewView.GetHint: string;
begin
  Result := _(SWebPreviewHint);
end;

function TWebPreviewView.GetMenuCaption: string;
begin
  Result := _(SWebPreview);
end;

function TWebPreviewView.GetName: string;
begin
  Result := 'Web Preview';
end;

function TWebPreviewView.GetTabCaption: string;
begin
  Result := _(SWebPreviewTab);
end;

function TWebPreviewView.GetShortCut: TShortCut;
begin
  Result := 0;
end;

initialization
  //  This unit must be initialized after frmEditor
  if Assigned(GI_EditorFactory) then
    WebPreviewFactoryIndex := GI_EditorFactory.RegisterViewFactory(TWebPreviewView.Create as IEditorViewFactory);
  OleInitialize(nil);

finalization
  OleUninitialize;

end.

