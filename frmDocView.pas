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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, OleCtrls, ActiveX, SHDocVw,
  ImgList, uEditAppIntfs, TB2Item, TBX, TB2Dock, TB2Toolbar;
                                                                 
type
  TDocForm = class(TForm, IEditorView)
    WebBrowser: TWebBrowser;
    Images: TImageList;
    TBXDock1: TTBXDock;
    TBXToolbar1: TTBXToolbar;
    ToolButtonForward: TTBXItem;
    ToolButtonBack: TTBXItem;
    TBXSeparatorItem1: TTBXSeparatorItem;
    TBXItem3: TTBXItem;
    TBXItem4: TTBXItem;
    TBXSeparatorItem2: TTBXSeparatorItem;
    TBXItem5: TTBXItem;
    TBXItem6: TTBXItem;
    TBXSeparatorItem4: TTBXSeparatorItem;
    TBXItem7: TTBXItem;
    procedure ToolButtonBackClick(Sender: TObject);
    procedure ToolButtonForwardClick(Sender: TObject);
    procedure ToolButtonStopClick(Sender: TObject);
    procedure ToolButtonPageSetupClick(Sender: TObject);
    procedure ToolButtonPrintPreviewClick(Sender: TObject);
    procedure ToolButtonPrintClick(Sender: TObject);
    procedure ToolButtonSaveClick(Sender: TObject);
    procedure WebBrowserCommandStateChange(Sender: TObject;
      Command: Integer; Enable: WordBool);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    TempFileName : string;
    SaveFileName : string;
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
    function GetImageIndex : integer;
    function GetShortCut : TShortCut;
  end;


implementation

uses
  VarPyth, JclFileUtils, JclStrings, frmEditor, dmCommands,
  uCommonFunctions, JvJVCLUtils, cPyBaseDebugger;

{$R *.dfm}

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

procedure TDocForm.ToolButtonPageSetupClick(Sender: TObject);
begin
  WebBrowser.ExecWB(OLECMDID_PAGESETUP, OLECMDEXECOPT_DODEFAULT);
end;

procedure TDocForm.ToolButtonPrintPreviewClick(Sender: TObject);
begin
  WebBrowser.ExecWB(OLECMDID_PRINTPREVIEW, OLECMDEXECOPT_DODEFAULT);
end;

procedure TDocForm.ToolButtonPrintClick(Sender: TObject);
begin
  WebBrowser.ExecWB(OLECMDID_PRINT, OLECMDEXECOPT_DODEFAULT);
end;

procedure TDocForm.ToolButtonSaveClick(Sender: TObject);
Var
  V : OleVariant;
begin
  V := SaveFileName;
  try
    WebBrowser.ExecWB(OLECMDID_SAVEAS, OLECMDEXECOPT_DONTPROMPTUSER, V);
  except
  end;
end;

procedure TDocForm.WebBrowserCommandStateChange(Sender: TObject;
  Command: Integer; Enable: WordBool);
begin
  case Command of
    CSC_NAVIGATEBACK: ToolButtonBack.Enabled := Enable;
    CSC_NAVIGATEFORWARD: ToolButtonForward.Enabled := Enable;
  end;
end;

procedure TDocForm.UpdateView(Editor: IEditor);
var
  HTML : string;
  pydoc, HTMLDoc, module : Variant;
  Cursor : IInterface;
begin
  if not Assigned(Editor) then Exit;

  Cursor := WaitCursor;
  Application.ProcessMessages;

  module := PyControl.ActiveInterpreter.ImportModule(Editor);

  pydoc := Import('pydoc');
  HTMLDoc := pydoc.html;
  HTML := HTMLDoc.page(pydoc.describe(module), HTMLDoc.document(module));

  SaveFileName := PathRemoveExtension(Editor.FileName) + '.html';
  TempFileName := PathGetTempPath + PathRemoveExtension(Editor.FileTitle) + '.html';
  StringToFile(TempFileName, HTML);
  WebBrowser.Navigate(TempFileName);
end;

procedure TDocForm.FormDestroy(Sender: TObject);
begin
  if (TempFileName <> '') and FileExists(TempFileName) then
    DeleteFile(TempFileName);
end;

{ TDocView }

function TDocView.CreateForm(Editor: IEditor; AOwner : TComponent): TCustomForm;
begin
  Result := TDocForm.Create(AOwner);
end;

function TDocView.GetImageIndex: Integer;
begin
  Result := 31;
end;

function TDocView.GetHint: string;
begin
  Result := 'Documentation|Generate HTML documentation';
end;

function TDocView.GetMenuCaption: string;
begin
  Result := '&Documentation'
end;

function TDocView.GetName: string;
begin
  Result := 'Documentation';
end;

function TDocView.GetTabCaption: string;
begin
  Result := 'Doc'
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

