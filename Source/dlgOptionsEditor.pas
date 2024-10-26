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
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  dlgPyIDEBase,
  zBase,
  zObjInspector,
  JvAppStorage,
  cPyScripterSettings;

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
    CancelButton: TButton;
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
    FIgnoreList: TArray<string>;
    function BeforeAddItem(Sender: TControl; PItem: PPropItem): Boolean;
  public
    { Public declarations }
    procedure StoreSettings(AppStorage: TJvCustomAppStorage);
    procedure RestoreSettings(AppStorage: TJvCustomAppStorage);
    procedure Setup(OptionsObject: TBaseOptions;
      Categories: array of TOptionCategory;
      AdditionalFriendlyNames: TStrings);
  end;

function InspectOptions(OptionsObject : TBaseOptions;
  Categories : array of TOptionCategory; FormCaption : string;
  IgnoredProperties: TArray<string>; AdditionalFriendlyNames: TStrings;
  HelpCntxt : integer = 0; ShowCategories: boolean = True): boolean;

implementation

uses
  uEditAppIntfs;

{$R *.dfm}

{ TIDEOptionsWindow }

procedure TOptionsInspector.Setup(OptionsObject: TBaseOptions;
  Categories: array of TOptionCategory; AdditionalFriendlyNames: TStrings);
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

  if Assigned(AdditionalFriendlyNames) then
  for I := 0 to AdditionalFriendlyNames.Count - 1 do
    FriendlyNames.Add(AdditionalFriendlyNames.Names[I],
      AdditionalFriendlyNames.ValueFromIndex[I]);

  Inspector.UpdateProperties;
end;

procedure TOptionsInspector.StoreSettings(AppStorage: TJvCustomAppStorage);
begin
  var H := Muldiv(Height, Screen.DefaultPixelsPerInch, FCurrentPPI);
  var W := Muldiv(Width, Screen.DefaultPixelsPerInch, FCurrentPPI);
  var SplitterPos := Muldiv(Inspector.SplitterPos, Screen.DefaultPixelsPerInch, FCurrentPPI);
  var Path := Caption + ' dialog';
  AppStorage.WriteInteger(Path + '\Height', H);
  AppStorage.WriteInteger(Path + '\Width', W);
  AppStorage.WriteInteger(Path + '\Splitter Position', SplitterPos);
end;

procedure TOptionsInspector.OKButtonClick(Sender: TObject);
begin
  fOptionsObject.Assign(fTempOptionsObject);
end;

procedure TOptionsInspector.RestoreSettings(AppStorage: TJvCustomAppStorage);
begin
  var Path := Caption + ' dialog';
  Height := MulDiv(AppStorage.ReadInteger(Path + '\Height', 340), FCurrentPPI,
    Screen.DefaultPixelsPerInch);
  Width := MulDiv(AppStorage.ReadInteger(Path + '\Width', 650), FCurrentPPI,
    Screen.DefaultPixelsPerInch);
  Inspector.SplitterPos := MulDiv(AppStorage.ReadInteger(
    Path + '\Splitter Position', 360),FCurrentPPI, Screen.DefaultPixelsPerInch);
end;

function TOptionsInspector.BeforeAddItem(Sender: TControl;
  PItem: PPropItem): Boolean;
begin
  Result := True;
  for var S in FIgnoreList do
    if LowerCase(S) = LowerCase(PItem^.Name) then
      Exit(False);
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

function InspectOptions(OptionsObject : TBaseOptions; Categories : array of
    TOptionCategory; FormCaption : string; IgnoredProperties: TArray<string>;
    AdditionalFriendlyNames: TStrings; HelpCntxt : integer = 0;
    ShowCategories: boolean = True): boolean;
begin
  with TOptionsInspector.Create(Application) do begin
    Inspector.OnBeforeAddItem := BeforeAddItem;
    FIgnoreList := IgnoredProperties;
    Caption := FormCaption;
    RestoreSettings(GI_PyIDEServices.AppStorage);
    HelpContext := HelpCntxt;
    Inspector.SortByCategory := ShowCategories;
    Setup(OptionsObject, Categories, AdditionalFriendlyNames);
    Result := ShowModal = mrOK;
    StoreSettings(GI_PyIDEServices.AppStorage);
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

