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
  Dialogs, Buttons, JvInspector, JvExControls,
  SpTBXControls, dlgPyIDEBase, SpTBXItem;

type

  TOption = record
    PropertyName : string;
    DisplayName : string;
  end;

  TOptionCategory = record
    DisplayName : string;
    Options : array of TOption;
  end;

  TBaseOptionsClass = class of TBaseOptions;
  TBaseOptions = class(TPersistent)
    public
    constructor Create; virtual; abstract;
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
    procedure Setup(OptionsObject : TBaseOptions; Categories : array of TOptionCategory);
    procedure ApplyVclStyle;
  end;


function InspectOptions(OptionsObject : TBaseOptions;
  Categories : array of TOptionCategory; FormCaption : string;
  HelpCntxt : integer = 0): boolean;

implementation

uses
  Vcl.Themes;

{$R *.dfm}

{ TIDEOptionsWindow }

procedure TOptionsInspector.Setup(OptionsObject: TBaseOptions;
  Categories: array of TOptionCategory);
var
  i, j : integer;
  InspCat: TJvInspectorCustomCategoryItem;
  Item : TJvCustomInspectorItem;
begin
  Inspector.Clear;
  fOptionsObject := OptionsObject;
  fTempOptionsObject := TBaseOptionsClass(OptionsObject.ClassType).Create;
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

procedure TOptionsInspector.ApplyVclStyle;
begin
  if Assigned(Inspector.ActivePainter) then
    with Inspector.ActivePainter do begin
      BackgroundColor := StyleServices.GetSystemColor(clWindow);
      CategoryColor := StyleServices.GetSystemColor(clBtnFace);
      CategoryFont.Color := StyleServices.GetSystemColor(clWindowText);
      DividerColor := StyleServices.GetSystemColor(clBtnFace);
      NameFont.Color := StyleServices.GetSystemColor(clWindowText);
      ValueFont.Color := StyleServices.GetSystemColor(clWindowText);
      HideSelectColor := StyleServices.GetSystemColor(clBtnFace);
      HideSelectFont.Color := StyleServices.GetSystemColor(clHighlightText);
      SelectedColor := StyleServices.GetSystemColor(clHighlight);
      SelectedFont.Color := StyleServices.GetSystemColor(clHighlightText);;
  end;
end;

procedure TOptionsInspector.FormDestroy(Sender: TObject);
begin
  if Assigned(fTempOptionsObject) then
    FreeAndNil(fTempOptionsObject);
end;

function InspectOptions(OptionsObject : TBaseOptions;
  Categories : array of TOptionCategory; FormCaption : string;
  HelpCntxt : integer = 0): boolean;
begin
  with TOptionsInspector.Create(Application) do begin
    Caption := FormCaption;
    HelpContext := HelpCntxt;
    Setup(OptionsObject, Categories);
    ApplyVclStyle;
    Result := ShowModal = mrOK;
    Release;
  end;
end;

procedure TOptionsInspector.HelpButtonClick(Sender: TObject);
begin
  if HelpContext <> 0 then
    Application.HelpContext(HelpContext);
end;

type
  TJvInspectorColorItem = class(TJvInspectorIntegerItem)
  protected
    procedure Edit; override;
    procedure SetFlags(const Value: TInspectorItemFlags); override;
  end;


{ TJvInspectorColorItem }

procedure TJvInspectorColorItem.Edit;
begin
  with TColorDialog.Create(GetParentForm(Inspector)) do
    try
      Color := Data.AsOrdinal;
      Options := [cdFullOpen, cdAnyColor];
      if Execute then
      begin
        Data.AsOrdinal := Color;
        //Data.InvalidateData;
      end;
    finally
      Free;
    end;
end;

procedure TJvInspectorColorItem.SetFlags(const Value: TInspectorItemFlags);
var
  NewValue: TInspectorItemFlags;
begin
  NewValue := Value + [iifEditButton];
  inherited SetFlags(NewValue);
end;

initialization
  if TJvCustomInspectorData.ItemRegister <> nil then
    with TJvCustomInspectorData.ItemRegister do
      Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorColorItem, TypeInfo(TColor)));  with TJvCustomInspectorData.ItemRegister do

end.

