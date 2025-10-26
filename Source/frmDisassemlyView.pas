{-----------------------------------------------------------------------------
 Unit Name: frmDisassemlyView
 Author:    Kiriakos Vlahos
 Date:      30-May-2005
 Purpose:   Disassembly Editor View
 History:
-----------------------------------------------------------------------------}

unit frmDisassemlyView;         

interface

uses
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  SynEdit,
  uEditAppIntfs;

type
  TDisForm = class(TForm, IEditorView)
    DisSynEdit: TSynEdit;
    procedure FormCreate(Sender: TObject);
  private
    procedure UpdateView(Editor: IEditor);
  end;

  TDisView = class(TInterfacedObject, IEditorViewFactory)
  private
    function CreateForm(Editor: IEditor; AOwner: TComponent): TCustomForm;
    function GetName: string;
    function GetTabCaption: string;
    function GetMenuCaption: string;
    function GetHint: string;
    function GetImageName: string;
    function GetShortCut: TShortCut;
    procedure GetContextHighlighters(List: TList);
  end;

implementation

uses
  System.SysUtils,
  JvJVCLUtils,
  JvGnugettext,
  PythonEngine,
  StringResources,
  dmResources,
  uPythonItfs,
  cPyScripterSettings;

{$R *.dfm}

procedure TDisForm.FormCreate(Sender: TObject);
begin
 DisSynEdit.Assign(EditorOptions);
 DisSynEdit.Highlighter := ResourcesDataModule.SynPythonSyn;
end;

{ TDisForm }

procedure TDisForm.UpdateView(Editor: IEditor);
var
  Py: IPyEngineAndGIL;
  getdis, module: Variant;
  Cursor: IInterface;
const
  Code =
  'def GetDis(m):'#10 +
	     #9'import dis'#10 +
	     #9'import sys'#10 +
       #9'StringIO = __import__("io").StringIO'#10 +
	     #9'sio = StringIO()'#10 +
       #9'dis.dis(m, file = sio)'#10 +
       #9'return sio.getvalue()'#10;
  Header = ''''''''#13#10#9+'Disassembly of %s'#13#10+''''''''#13#10#13#10;

begin
  if not Assigned(Editor) then Exit;

  Cursor := WaitCursor;
  Application.ProcessMessages;

  Py := SafePyEngine;

  module := GI_PyControl.ActiveInterpreter.ImportModule(Editor.FileId);
  GI_PyControl.ActiveInterpreter.RunSource(Code, '<Getdis>', 'exec');
  getdis := GI_PyControl.ActiveInterpreter.EvalCode('GetDis');

  DisSynEdit.Text := Format(Header,[Editor.FileTitle]) + getdis.__call__(module);
end;

{ TDisView }

function TDisView.CreateForm(Editor: IEditor;
  AOwner: TComponent): TCustomForm;
begin
  Result := TDisForm.Create(AOwner);
end;

procedure TDisView.GetContextHighlighters(List: TList);
begin
  List.Add(ResourcesDataModule.SynPythonSyn);
end;

function TDisView.GetHint: string;
begin
  Result := _(SDisassemblyHint);
end;

function TDisView.GetImageName: string;
begin
  Result := 'Assembly';
end;

function TDisView.GetMenuCaption: string;
begin
  Result := _(SDisassembly);
end;

function TDisView.GetName: string;
begin
  Result := 'Disassembly';
end;

function TDisView.GetShortCut: TShortCut;
begin
  Result := 0;
end;

function TDisView.GetTabCaption: string;
begin
  Result := _(SDisassemblyTab);
end;

initialization
  //  This unit must be initialized after frmEditor
  if Assigned(GI_EditorFactory) then
    GI_EditorFactory.RegisterViewFactory(TDisView.Create as IEditorViewFactory);
end.

