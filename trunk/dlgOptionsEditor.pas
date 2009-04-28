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
  Dialogs, StdCtrls, Buttons, JvComponent, JvInspector, JvExControls,
  ExtCtrls, JvComponentBase, SpTBXDkPanels, SpTBXControls, dlgPyIDEBase;

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
    Panel1: TSpTBXPanel;
    Inspector: TJvInspector;
    Panel2: TSpTBXPanel;
    OKButton: TSpTBXButton;
    BitBtn2: TSpTBXButton;
    HelpButton: TSpTBXButton;
    procedure OKButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { Private declarations }
    fOptionsObject,
    fTempOptionsObject : TPersistent;
  public
    { Public declarations }
    procedure Setup(OptionsObject : TPersistent; Categories : array of TOptionCategory);
  end;


function InspectOptions(OptionsObject : TPersistent;
  Categories : array of TOptionCategory; FormCaption : string;
  HelpCntxt : integer = 0): boolean;

implementation

{$R *.dfm}

{ TIDEOptionsWindow }

procedure TOptionsInspector.Setup(OptionsObject: TPersistent;
  Categories: array of TOptionCategory);
var
  i, j : integer;
  InspCat: TJvInspectorCustomCategoryItem;
  Item : TJvCustomInspectorItem;
begin
  Inspector.Clear;
  fOptionsObject := OptionsObject;
  fTempOptionsObject := TPersistentClass(OptionsObject.ClassType).Create;
  fTempOptionsObject.Assign(fOptionsObject);

  for i := Low(Categories) to High(Categories) do
    with Categories[i] do begin
      InspCat := TJvInspectorCustomCategoryItem.Create(Inspector.Root, nil);
      InspCat.DisplayName := DisplayName;
      for j := Low(Options) to High(Options) do begin
        Item := TJvInspectorPropData.New(InspCat, fTempOptionsObject,
          Options[j].PropertyName);
        Item.DisplayName := Options[j].DisplayName;
        if Item is TJvInspectorBooleanItem then
          TJvInspectorBooleanItem(Item).ShowAsCheckbox := True;
      end;
      InspCat.Expanded := True;
    end;
  Inspector.Root.Sort;
end;

procedure TOptionsInspector.OKButtonClick(Sender: TObject);
begin
  Inspector.SaveValues;  // Save currently editing item
  fOptionsObject.Assign(fTempOptionsObject);
end;

procedure TOptionsInspector.FormDestroy(Sender: TObject);
begin
  if Assigned(fTempOptionsObject) then
    FreeAndNil(fTempOptionsObject);
end;

function InspectOptions(OptionsObject : TPersistent;
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

end.

