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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SynEditHighlighter, SynHighlighterPython, SynEdit, uEditAppIntfs;

type
  TDisForm = class(TForm, IEditorView)
    DisSynEdit: TSynEdit;
    SynPythonSyn: TSynPythonSyn;
  private
    { Private declarations }
    procedure UpdateView(Editor : IEditor);
  public
    { Public declarations }
  end;                           

  TDisView = class(TInterfacedObject, IEditorViewFactory)
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

uses VarPyth, PythonEngine, dmCommands, uCommonFunctions,
  JvJVCLUtils, cPyBaseDebugger;

{$R *.dfm}

{ TDisForm }

procedure TDisForm.UpdateView(Editor: IEditor);
var
  getdis, module : Variant;
  Cursor : IInterface;
Const
  Code =
  'def GetDis(m):'#10 +
	     #9'import dis'#10 +
	     #9'import sys'#10 +
	     #9'import StringIO'#10 +
	     #9'oldstdout = sys.stdout'#10 +
	     #9'sys.stdout = StringIO.StringIO()'#10 +
	     #9'try:'#10 +
		        #9#9'dis.dis(m)'#10 +
            #9#9'result = sys.stdout.getvalue()'#10 +
       #9'finally:'#10 +
		        #9#9'sys.stdout.close()'#10 +
            #9#9'sys.stdout = oldstdout'#10 +
       #9'return result'#10;
  Header = ''''''''#13#10#9+'Disassembly of %s'#13#10+''''''''#13#10#13#10;

begin
  if not Assigned(Editor) then Exit;

  Cursor := WaitCursor;
  Application.ProcessMessages;

  module := PyControl.ActiveInterpreter.ImportModule(Editor);
  GetPythonEngine.ExecString(Code);

  getdis := VarPythonEval('GetDis');
  DisSynEdit.Text := Format(Header,[Editor.FileTitle]) + getdis.__call__(module);
end;

{ TDisView }

function TDisView.CreateForm(Editor: IEditor;
  AOwner: TComponent): TCustomForm;
begin
  Result := TDisForm.Create(AOwner);
end;

function TDisView.GetHint: string;
begin
  Result := 'Disassembly|Disassembly View';
end;

function TDisView.GetImageIndex: integer;
begin
  Result := 110;
end;

function TDisView.GetMenuCaption: string;
begin
  Result := 'Dis&assembly'
end;

function TDisView.GetName: string;
begin
  Result := 'Disassembly'
end;

function TDisView.GetShortCut: TShortCut;
begin
  Result := 0;
end;

function TDisView.GetTabCaption: string;
begin
  Result := 'Disassembly'
end;

initialization
  //  This unit must be initialized after frmEditor
  if Assigned(GI_EditorFactory) then
    GI_EditorFactory.RegisterViewFactory(TDisView.Create as IEditorViewFactory);
end.

