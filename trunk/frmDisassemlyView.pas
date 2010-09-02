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
    procedure GetContextHighlighters(List : TList);
  end;


implementation

uses dmCommands, 
  JvJVCLUtils, cPyBaseDebugger, gnugettext, StringResources;

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
       #9'if sys.version_info[0]==3:'#10 +
            #9#9'StringIO = __import__("io").StringIO'#10 +
       #9'else:'#10 +
            #9#9'StringIO = __import__("StringIO").StringIO'#10 +
	     #9'oldstdout = sys.stdout'#10 +
	     #9'sys.stdout = StringIO()'#10 +
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
  PyControl.ActiveInterpreter.RunSource(Code, '<Getdis>', 'exec');
  getdis := PyControl.ActiveInterpreter.EvalCode('GetDis');

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
  List.Add(CommandsDataModule.SynPythonSyn);
end;

function TDisView.GetHint: string;
begin
  Result := _(SDisassemblyHint);
end;

function TDisView.GetImageIndex: integer;
begin
  Result := 110;
end;

function TDisView.GetMenuCaption: string;
begin
  Result := _(SDisassembly);
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
  Result := _(SDisassemblyTab);
end;

initialization
  //  This unit must be initialized after frmEditor
  if Assigned(GI_EditorFactory) then
    GI_EditorFactory.RegisterViewFactory(TDisView.Create as IEditorViewFactory);
end.

