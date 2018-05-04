unit dlgPythonVersions;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  dlgPyIDEBase,
  dmCommands, VirtualTrees, Vcl.ExtCtrls;

type
  TPythonVersionsDialog = class(TPyIDEDlgBase)
    Panel1: TPanel;
    gbRegisteredVersions: TGroupBox;
    vstRegisteredVersions: TVirtualStringTree;
    Panel2: TPanel;
    gbNonRegisteredVersions: TGroupBox;
    VirtualStringTree1: TVirtualStringTree;
    btnHelp: TButton;
    btnCancel: TButton;
    btnOk: TButton;
    pnlNoRegisteredVersions: TPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PythonVersionsDialog: TPythonVersionsDialog;

implementation

{$R *.dfm}

end.
