{-----------------------------------------------------------------------------
 Unit Name: dlgOptionsEditor
 Author:    Kiriakos Vlahos
 Date:      10-Mar-2005
 Purpose:   Generic Options Editor based on JvInspector
 History:
-----------------------------------------------------------------------------}

unit dlgOptionsEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, dlgPyIDEBase, zBase,
  zObjInspector, Vcl.ExtCtrls, System.Generics.Collections,
  cPyScripterSettings, Vcl.StdCtrls;

type

  TOption = record
    PropertyName : string;
    DisplayName : string;
  end;

  TOptionCategory = record
    DisplayName : string;
    Options : array of TOption;
  end;

  TOptionsInspector = class(TPyIDEDlgBase)
    Panel1: TPanel;
    Panel2: TPanel;
    Inspector: TzObjectInspector;
    OKButton: TButton;
    BitBtn2: TButton;
    HelpButton: TButton;
    procedure OKButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    function InspectorGetItemFriendlyName(Sender: TControl;
      PItem: PPropItem): string;
  private
    { Private declarations }
    fOptionsObject,
    fTempOptionsObject : TPersistent;
    FriendlyNames : TDictionary<string, string>;
  public
    { Public declarations }
    procedure Setup(OptionsObject : TBaseOptions; Categories : array of TOptionCategory);
  end;


function InspectOptions(OptionsObject : TBaseOptions;
  Categories : array of TOptionCategory; FormCaption : string;
  HelpCntxt : integer = 0): boolean;

implementation

{$R *.dfm}


{ TIDEOptionsWindow }

procedure TOptionsInspector.Setup(OptionsObject: TBaseOptions;
  Categories: array of TOptionCategory);
var
  i, j : integer;
begin
  fOptionsObject := OptionsObject;
  fTempOptionsObject := TBaseOptionsClass(OptionsObject.ClassType).Create;
  fTempOptionsObject.Assign(fOptionsObject);
  Inspector.Component := fTempOptionsObject;
  for i := Low(Categories) to High(Categories) do
    with Categories[i] do begin
      for j := Low(Options) to High(Options) do begin
        Inspector.RegisterPropertyInCategory(Categories[i].DisplayName, Options[j].PropertyName);
        FriendlyNames.Add(Options[j].PropertyName, Options[j].DisplayName);
      end;
    end;
  Inspector.UpdateProperties;
end;

procedure TOptionsInspector.OKButtonClick(Sender: TObject);
begin
  fOptionsObject.Assign(fTempOptionsObject);
end;

procedure TOptionsInspector.FormCreate(Sender: TObject);
begin
  inherited;
  FriendlyNames := TDictionary<string,string>.Create;
end;

procedure TOptionsInspector.FormDestroy(Sender: TObject);
begin
  if Assigned(fTempOptionsObject) then
    FreeAndNil(fTempOptionsObject);
  FriendlyNames.Free;
end;

function InspectOptions(OptionsObject : TBaseOptions;
  Categories : array of TOptionCategory; FormCaption : string;
  HelpCntxt : integer = 0): boolean;
begin
  with TOptionsInspector.Create(Application) do begin
    Caption := FormCaption;
    HelpContext := HelpCntxt;
    Setup(OptionsObject, Categories);
    Result := ShowModal = mrOK;
    Release;
  end;
end;

procedure TOptionsInspector.HelpButtonClick(Sender: TObject);
begin
  if HelpContext <> 0 then
    Application.HelpContext(HelpContext);
end;

function TOptionsInspector.InspectorGetItemFriendlyName(Sender: TControl;
  PItem: PPropItem): string;
begin
  if FriendlyNames.ContainsKey(PItem.Name) then
    Result := FriendlyNames[PItem.Name]
  else
    Result := PItem.Name;
end;


end.

