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
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Vcl.Controls,
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
    FOptionsObject,
    FTempOptionsObject : TPersistent;
    FFriendlyNames : TDictionary<string, string>;
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
  HelpCntxt : Integer = 0; ShowCategories: Boolean = True): Boolean;

implementation

uses
  Winapi.Windows,
  Vcl.Forms,
  uEditAppIntfs;

{$R *.dfm}

{ TIDEOptionsWindow }

procedure TOptionsInspector.Setup(OptionsObject: TBaseOptions;
  Categories: array of TOptionCategory; AdditionalFriendlyNames: TStrings);
var
  I, J : Integer;
begin
  FOptionsObject := OptionsObject;
  FTempOptionsObject := TBaseOptionsClass(OptionsObject.ClassType).Create;
  FTempOptionsObject.Assign(FOptionsObject);
  Inspector.Component := FTempOptionsObject;
  for I := Low(Categories) to High(Categories) do
    with Categories[I] do begin
      for J := Low(Options) to High(Options) do begin
        Inspector.RegisterPropertyInCategory(Categories[I].DisplayName, Options[J].PropertyName);
        FFriendlyNames.Add(Options[J].PropertyName, Options[J].DisplayName);
      end;
    end;

  if Assigned(AdditionalFriendlyNames) then
  for I := 0 to AdditionalFriendlyNames.Count - 1 do
    FFriendlyNames.Add(AdditionalFriendlyNames.Names[I],
      AdditionalFriendlyNames.ValueFromIndex[I]);

  Inspector.UpdateProperties;
end;

procedure TOptionsInspector.StoreSettings(AppStorage: TJvCustomAppStorage);
begin
  var H := MulDiv(Height, Screen.DefaultPixelsPerInch, FCurrentPPI);
  var W := MulDiv(Width, Screen.DefaultPixelsPerInch, FCurrentPPI);
  var SplitterPos := MulDiv(Inspector.SplitterPos, Screen.DefaultPixelsPerInch, FCurrentPPI);
  var Path := Caption + ' dialog';
  AppStorage.WriteInteger(Path + '\Height', H);
  AppStorage.WriteInteger(Path + '\Width', W);
  AppStorage.WriteInteger(Path + '\Splitter Position', SplitterPos);
end;

procedure TOptionsInspector.OKButtonClick(Sender: TObject);
begin
  FOptionsObject.Assign(FTempOptionsObject);
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
  for var Ignored in FIgnoreList do
    if CompareText(Ignored, PItem^.Name) = 0 then
      Exit(False);
end;

procedure TOptionsInspector.FormCreate(Sender: TObject);
begin
  inherited;
  FFriendlyNames := TDictionary<string,string>.Create;
end;

procedure TOptionsInspector.FormDestroy(Sender: TObject);
begin
  FTempOptionsObject.Free;
  FFriendlyNames.Free;
end;

function InspectOptions(OptionsObject : TBaseOptions; Categories : array of
    TOptionCategory; FormCaption : string; IgnoredProperties: TArray<string>;
    AdditionalFriendlyNames: TStrings; HelpCntxt : Integer = 0;
    ShowCategories: Boolean = True): Boolean;
begin
  with TOptionsInspector.Create(Application) do begin
    Inspector.OnBeforeAddItem := BeforeAddItem;
    FIgnoreList := IgnoredProperties;
    Caption := FormCaption;
    RestoreSettings(GI_PyIDEServices.AppStorage);
    HelpContext := HelpCntxt;
    Inspector.SortByCategory := ShowCategories;
    Setup(OptionsObject, Categories, AdditionalFriendlyNames);
    Result := ShowModal = mrOk;
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
  if FFriendlyNames.ContainsKey(PItem.Name) then
    Result := FFriendlyNames[PItem.Name]
  else
    Result := PItem.Name;
end;


end.

