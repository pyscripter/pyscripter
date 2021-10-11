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
  SHDocVw,
  Vcl.ImgList,
  TB2Item,
  TB2Dock,
  TB2Toolbar,
  SpTBXItem,
  dmCommands,
  uEditAppIntfs,
  cTools;

type
  TWebPreviewForm = class(TForm, IEditorView)
    WebBrowser: TWebBrowser;
    TBXDock1: TSpTBXDock;
    TBXToolbar1: TSpTBXToolbar;
    ToolButtonForward: TSpTBXItem;
    ToolButtonBack: TSpTBXItem;
    TBXSeparatorItem1: TSpTBXSeparatorItem;
    TBXItem3: TSpTBXItem;
    TBXItem4: TSpTBXItem;
    TBXSeparatorItem2: TSpTBXSeparatorItem;
    TBXItem5: TSpTBXItem;
    TBXItem6: TSpTBXItem;
    TBXSeparatorItem4: TSpTBXSeparatorItem;
    TBXItem7: TSpTBXItem;
    BrowserImages: TVirtualImageList;
    procedure ToolButtonBackClick(Sender: TObject);
    procedure ToolButtonForwardClick(Sender: TObject);
    procedure ToolButtonStopClick(Sender: TObject);
    procedure ToolButtonPageSetupClick(Sender: TObject);
    procedure ToolButtonPrintPreviewClick(Sender: TObject);
    procedure ToolButtonPrintClick(Sender: TObject);
    procedure ToolButtonSaveClick(Sender: TObject);
    procedure WebBrowserCommandStateChange(Sender: TObject;
      Command: Integer; Enable: WordBool);
  private
    { Private declarations }
    fEditor: IEditor;
    SaveFileName : string;
    ExternalToolPtr : TFunc<TExternalTool>;
    procedure UpdateView(Editor : IEditor);
  public
    { Public declarations }
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
  MSHTML,
  JvGnugettext,
  uCommonFunctions,
  StringResources,
  VarPyth,
  frmCommandOutput,
  cParameters;

{$R *.dfm}

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

procedure TWebPreviewForm.ToolButtonPageSetupClick(Sender: TObject);
begin
  WebBrowser.ExecWB(OLECMDID_PAGESETUP, OLECMDEXECOPT_DODEFAULT);
end;

procedure TWebPreviewForm.ToolButtonPrintPreviewClick(Sender: TObject);
begin
  WebBrowser.ExecWB(OLECMDID_PRINTPREVIEW, OLECMDEXECOPT_DODEFAULT);
end;

procedure TWebPreviewForm.ToolButtonPrintClick(Sender: TObject);
begin
  WebBrowser.ExecWB(OLECMDID_PRINT, OLECMDEXECOPT_DODEFAULT);
end;

procedure TWebPreviewForm.ToolButtonSaveClick(Sender: TObject);
Var
  V : OleVariant;
begin
  V := SaveFileName;
  try
    WebBrowser.ExecWB(OLECMDID_SAVEAS, OLECMDEXECOPT_DONTPROMPTUSER, V);
  except
  end;
end;

procedure TWebPreviewForm.WebBrowserCommandStateChange(Sender: TObject;
  Command: Integer; Enable: WordBool);
begin
  case Command of
    CSC_NAVIGATEBACK: ToolButtonBack.Enabled := Enable;
    CSC_NAVIGATEFORWARD: ToolButtonForward.Enabled := Enable;
  end;
end;

procedure TWebPreviewForm.UpdateView(Editor: IEditor);
var
//  v: Variant;
  HTMLDocument: IHTMLDocument2;
  FN : string;
begin
  fEditor := Editor;
  WebBrowser.RegisterAsBrowser := True;
  WebBrowser.Silent := True;
  WebBrowser.Navigate('about:blank') ;
  while WebBrowser.ReadyState < READYSTATE_INTERACTIVE do begin
    Application.ProcessMessages;
    CheckSynchronize()
  end;
  if Assigned(Editor.SynEdit.Highlighter) and
    (Editor.SynEdit.Highlighter = CommandsDataModule.SynJSONSyn) then
  begin
    FN := ExtractFileName(Editor.FileName);
    FN := StringReplace(FN, ' ', '%20', [rfReplaceAll]);
    TThread.ForceQueue(nil, procedure
    begin
      Sleep(2000);
      WebBrowser.Navigate('http://localhost:8888/notebooks/'+FN);
    end);
  end else begin
    HTMLDocument := WebBrowser.Document as IHTMLDocument2;
    if not Assigned(HTMLDocument) then Exit;

    //  HTMLDocument.clear;
    OleVariant(HTMLDocument).Write(Editor.SynEdit.Text);
    //  v := VarArrayCreate([0, 0], varVariant);
    //  v[0] := Editor.SynEdit.Text;
    //  HTMLDocument.Write(PSafeArray(TVarData(v).VArray));
    HTMLDocument.Close;
  end;
end;

{ TDocView }

function TWebPreviewView.CreateForm(Editor: IEditor; AOwner : TComponent): TCustomForm;
var
  ExternalTool : TExternalTool;
begin
  ExternalTool := nil;

  if Assigned(Editor.SynEdit.Highlighter) and
    (Editor.SynEdit.Highlighter = CommandsDataModule.SynJSONSyn) then
  begin
    if Editor.FileName = '' then
      (Editor as IFileCommands).ExecSave;
    if LowerCase(ExtractFileExt(Editor.FileName)) <> '.ipynb' then begin
      StyledMessageDlg(_(SOnlyJupyterFiles), mtError, [mbOK], 0);
      Abort;
    end;
    try
      Import('jupyter');
    except
      StyledMessageDlg(_(SNoJupyter), mtError, [mbOK], 0);
      Abort;
    end;
    if OutputWindow.IsRunning then begin
      StyledMessageDlg(_(SExternalProcessRunning), mtError, [mbOK], 0);
      Abort;
    end;

    ExternalTool := TExternalTool.Create;
    with ExternalTool do begin
      Caption := 'Jupyter Server';
      Description := Caption;
      ApplicationName := ('$[PythonDir-Short]Scripts\Jupyter-notebook.exe');
      Parameters := cParameters.Parameters.ReplaceInText('--no-browser --NotebookApp.token=""');
      WorkingDirectory := cParameters.Parameters.ReplaceInText('$[ActiveDoc-Short-Dir]');
      SaveFiles := sfActive;
      Context := tcAlwaysEnabled;
      ParseTraceback := False;
      CaptureOutput := True;
      ConsoleHidden := True;
    end;
    OutputWindow.ExecuteTool(ExternalTool);
  end;

  Result := TWebPreviewForm.Create(AOwner);
  if Assigned(ExternalTool) then
    TWebPreviewForm(Result).ExternalToolPtr := TSmartPtr.Make(ExternalTool);
end;

function TWebPreviewView.GetImageName: string;
begin
  Result := 'Web';
end;

procedure TWebPreviewView.GetContextHighlighters(List: TList);
begin
  List.Add(CommandsDataModule.SynWebHtmlSyn);
  List.Add(CommandsDataModule.SynWebXmlSyn);
  List.Add(CommandsDataModule.SynWebCssSyn);
  List.Add(CommandsDataModule.SynJSONSyn);
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

