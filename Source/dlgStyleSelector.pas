unit dlgStyleSelector;

interface

uses
  System.Classes,
  System.Actions,
  System.Generics.Collections,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ActnList,
  VCL.Styles.PyScripter,
  dlgPyIDEBase;

type
  TStyleSelectorForm = class(TPyIDEDlgBase)
    Label1: TLabel;
    Panel1: TPanel;
    Button2: TButton;
    LBStyleNames: TListBox;
    Label2: TLabel;
    Button1: TButton;
    ActionList: TActionList;
    actApplyStyle: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ActionApplyStyleUpdate(Sender: TObject);
    procedure ActionApplyStyleExecute(Sender: TObject);
    procedure LBStyleNamesClick(Sender: TObject);
  private
    FLoading: Boolean;
    FStylesPath: string;
    FPreview:TVclStylesPreview;
    FExternalStyleFilesDict:  TDictionary<string, string>;
    class var FLoadedStylesDict:  TDictionary<string, string>;
    procedure FillVclStylesList;
  public
    class var CurrentSkinName: string;
    class procedure Execute;
    class procedure SetStyle(StyleName: string);
  end;

implementation

uses
  System.Types,
  System.SysUtils,
  System.IOUtils,
  Vcl.Forms,
  Vcl.Themes,
  cPyScripterSettings;

type
 TVclStylesPreviewClass = class(TVclStylesPreview);

{$R *.dfm}

procedure TStyleSelectorForm.FormCreate(Sender: TObject);
begin
  inherited;
  FLoading:=False;
  LBStyleNames.Sorted := True;
  FExternalStyleFilesDict := TDictionary<string, string>.Create;
  FStylesPath := TPyScripterSettings.StylesFilesDir;
  FPreview:=TVclStylesPreview.Create(Self);
  FPreview.Parent:=Panel1;
  FPreview.Icon := Application.Icon.Handle;
  FPreview.BoundsRect := Panel1.ClientRect;
  FillVclStylesList;
end;

procedure TStyleSelectorForm.FormDestroy(Sender: TObject);
begin
  FExternalStyleFilesDict.Free;
  FPreview.Free;
end;

procedure TStyleSelectorForm.FormShow(Sender: TObject);
//  Todo Select active style
var
  Index: Integer;
begin
   if (LBStyleNames.Items.Count> 0) then
   begin
     Index := LBStyleNames.Items.IndexOf(TStyleManager.ActiveStyle.Name);
     if Index >= 0 then
       LBStyleNames.Selected[Index] :=  True
     else
       LBStyleNames.Selected[0] :=  True;
   end;
   LBStyleNamesClick(Self);
end;

procedure TStyleSelectorForm.LBStyleNamesClick(Sender: TObject);
var
  LStyle: TCustomStyleServices;
  FileName: string;
  StyleName: string;
begin
  LStyle:=nil;
  if LBStyleNames.ItemIndex >= 0 then
  begin
    StyleName := LBStyleNames.Items[LBStyleNames.ItemIndex];
    if NativeUInt(LBStyleNames.Items.Objects[LBStyleNames.ItemIndex]) = 1 then
    begin
      // FileName
      if not FLoading then
      begin
        FileName := FExternalStyleFilesDict[StyleName];
        TStyleManager.LoadFromFile(FileName);
        LStyle := TStyleManager.Style[StyleName];
        TStyleSelectorForm.FLoadedStylesDict.Add(StyleName, FileName);
        //  The Style is now loaded and registerd
        LBStyleNames.Items.Objects[LBStyleNames.ItemIndex] := nil;
      end;
    end
    else
    begin
         // Resource style
        LStyle := TStyleManager.Style[StyleName];
    end;
  end;

  if Assigned(LStyle) and not FLoading  then
  begin
    FPreview.Caption:=StyleName;
    FPreview.Style:=LStyle;
    TVclStylesPreviewClass(FPreview).Paint;
  end;

end;

class procedure TStyleSelectorForm.SetStyle(StyleName: string);
// StyleName can be either a resource of a file name
var
  SName: string;
  StyleInfo: TStyleInfo;
begin
  if CompareText(StyleName, TStyleManager.ActiveStyle.Name) = 0 then
    Exit;

  if CompareText(StyleName, 'Windows') = 0 then
  begin
    TStyleManager.SetStyle(TStyleManager.SystemStyle);
    TStyleSelectorForm.CurrentSkinName := 'Windows';
    Exit;
  end;

  for SName in TStyleManager.StyleNames do
    if SName = StyleName then
    begin
       // Resource style
      TStyleManager.SetStyle(StyleName);
      if TStyleSelectorForm.FLoadedStylesDict.ContainsKey(StyleName) then
        TStyleSelectorForm.CurrentSkinName := TStyleSelectorForm.FLoadedStylesDict[StyleName]
      else
        TStyleSelectorForm.CurrentSkinName := StyleName;
      Exit;
    end;

  // FileName
  if FileExists(StyleName) and TStyleManager.IsValidStyle(StyleName, StyleInfo) then
  begin
    if not TStyleManager.TrySetStyle(StyleInfo.Name, False) then
    begin
      TStyleManager.LoadFromFile(StyleName);
      TStyleSelectorForm.FLoadedStylesDict.Add(StyleInfo.Name, StyleName);
    end;
    TStyleManager.SetStyle(StyleInfo.Name);
    TStyleSelectorForm.CurrentSkinName := StyleName;
  end;
end;

procedure TStyleSelectorForm.ActionApplyStyleExecute(Sender: TObject);
var
  StyleName: string;
  FileName: string;
begin
  if LBStyleNames.ItemIndex >= 0 then begin
    StyleName := LBStyleNames.Items[LBStyleNames.ItemIndex];
    if NativeUInt(LBStyleNames.Items.Objects[LBStyleNames.ItemIndex]) = 1 then
    begin
      // FileName
      FileName := FExternalStyleFilesDict[StyleName];
      TStyleSelectorForm.SetStyle(FileName);
    end
    else
      // Resource style
     TStyleSelectorForm.SetStyle(StyleName);

    ModalResult := mrOk;
  end;
end;

procedure TStyleSelectorForm.ActionApplyStyleUpdate(Sender: TObject);
begin
  TCustomAction(Sender).Enabled:=(LBStyleNames.ItemIndex >= 0);
end;


class procedure TStyleSelectorForm.Execute;
var
  Owner: TCustomForm;
begin
  if Assigned(Screen.ActiveCustomForm) then
    Owner := Screen.ActiveCustomForm
  else
    Owner := Application.MainForm;

  with TStyleSelectorForm.Create(Owner) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TStyleSelectorForm.FillVclStylesList;
var
  FileName: string;
  StyleInfo:  TStyleInfo;
begin
   FLoading:=True;

   // First add resource styles
   LBStyleNames.Items.AddStrings(TStyleManager.StyleNames);
   // Remove Windows
   LBStyleNames.Items.Delete(LBStyleNames.Items.IndexOf('Windows'));

   // Then styles in files
    try
       for FileName in TDirectory.GetFiles(FStylesPath,'*.vsf') do
       begin
          if TStyleManager.IsValidStyle(FileName, StyleInfo) and
             (LBStyleNames.Items.IndexOf(StyleInfo.Name) < 0)
          then
          begin
            // TObject(1) denotes external file
            LBStyleNames.Items.AddObject(StyleInfo.Name, TObject(1));
            FExternalStyleFilesDict.Add(StyleInfo.Name, FileName);
          end;
       end;

    except
    end;

   FLoading:=False;
end;

initialization
  TStyleSelectorForm.CurrentSkinName := TStyleManager.ActiveStyle.Name;
  TStyleSelectorForm.FLoadedStylesDict := TDictionary<string, string>.Create;
finalization
  TStyleSelectorForm.FLoadedStylesDict.Free;
end.
