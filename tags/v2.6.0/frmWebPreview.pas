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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, OleCtrls, ActiveX, SHDocVw,
  ImgList, uEditAppIntfs, TB2Item, TB2Dock, TB2Toolbar, SpTBXItem;
                                                                 
type
  TWebPreviewForm = class(TForm, IEditorView)
    WebBrowser: TWebBrowser;
    Images: TImageList;
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
    function GetImageIndex : integer;
    function GetShortCut : TShortCut;
    procedure GetContextHighlighters(List : TList);
  end;


implementation

uses
  dmCommands,
  MSHTML, gnugettext,
  StringResources;

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
  v: Variant;
  HTMLDocument: IHTMLDocument2;
begin
  fEditor := Editor;
  WebBrowser.Navigate('about:blank') ;
  while WebBrowser.ReadyState < READYSTATE_INTERACTIVE do
    Application.ProcessMessages;

  HTMLDocument := WebBrowser.Document as IHTMLDocument2;
  v := VarArrayCreate([0, 0], varVariant);
  v[0] := Editor.SynEdit.Text;
  HTMLDocument.Write(PSafeArray(TVarData(v).VArray));
  HTMLDocument.Close;
end;

{ TDocView }

function TWebPreviewView.CreateForm(Editor: IEditor; AOwner : TComponent): TCustomForm;
begin
  Result := TWebPreviewForm.Create(AOwner);
end;

function TWebPreviewView.GetImageIndex: Integer;
begin
  Result := 148;
end;


procedure TWebPreviewView.GetContextHighlighters(List: TList);
begin
  List.Add(CommandsDataModule.SynWebHtmlSyn);
  List.Add(CommandsDataModule.SynWebXmlSyn);
  List.Add(CommandsDataModule.SynWebCssSyn);
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
    GI_EditorFactory.RegisterViewFactory(TWebPreviewView.Create as IEditorViewFactory);
  OleInitialize(nil);

finalization
  OleUninitialize;

end.

