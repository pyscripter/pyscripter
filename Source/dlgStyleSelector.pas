unit dlgStyleSelector;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Types, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
  Vcl.ActnList, Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan, Vcl.ToolWin,
  System.Actions, dlgPyIDEBase, SpTBXItem, SpTBXControls,
  System.Generics.Collections, Vcl.Themes, Vcl.Styles,
  Vcl.Styles.PyScripter;

type
  TStyleSelectorForm = class(TPyIDEDlgBase)
    Label1: TLabel;
    ActionManager1: TActionManager;
    ActionApplyStyle: TAction;
    Panel1: TPanel;
    Button1: TSpTBXButton;
    Button2: TButton;
    LBStyleNames: TListBox;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ActionApplyStyleUpdate(Sender: TObject);
    procedure ActionApplyStyleExecute(Sender: TObject);
    procedure LBStyleNamesClick(Sender: TObject);
  private
    Loading : Boolean;
    FStylesPath : string;
    FPreview:TVclStylesPreview;
    ExternalStyleFilesDict :  TDictionary<string, string>;
    procedure FillVclStylesList;
  public
    class var LoadedStylesDict :  TDictionary<string, string>;
    class var CurrentSkinName : string;
    class procedure Execute;
    class procedure SetStyle(StyleName : string);
  end;


implementation

uses
  System.IOUtils,
  dmCommands,
  frmPyIDEMain;

type
 TVclStylesPreviewClass = class(TVclStylesPreview);

{$R *.dfm}

procedure TStyleSelectorForm.FormCreate(Sender: TObject);
begin
  inherited;
  Loading:=False;
  LBStyleNames.Sorted := True;
  ExternalStyleFilesDict := TDictionary<string, string>.Create;
  FStylesPath := CommandsDataModule.StylesFilesDir;
  FPreview:=TVclStylesPreview.Create(Self);
  FPreview.Parent:=Panel1;
  FPreview.Icon := Application.Icon.Handle;
  FPreview.BoundsRect := Panel1.ClientRect;
  FillVclStylesList;
end;

procedure TStyleSelectorForm.FormDestroy(Sender: TObject);
begin
  ExternalStyleFilesDict.Free;
  FPreview.Free;
end;

procedure TStyleSelectorForm.FormShow(Sender: TObject);
//  Todo Select active style
Var
  Index : integer;
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
  LStyle : TCustomStyleServices;
  FileName : string;
  StyleName : String;
begin
  LStyle:=nil;
  if LBStyleNames.ItemIndex >= 0 then
  begin
    StyleName := LBStyleNames.Items[LBStyleNames.ItemIndex];
    if Integer(LBStyleNames.Items.Objects[LbStylenames.ItemIndex]) = 1 then
    begin
      // FileName
      if not Loading then
      begin
        FileName := ExternalStyleFilesDict.Items[StyleName];
        TStyleManager.LoadFromFile(FileName);
        LStyle := TStyleManager.Style[StyleName];
        TStyleSelectorForm.LoadedStylesDict.Add(StyleName, FileName);
        //  The Style is now loaded and registerd
        LBStyleNames.Items.Objects[LbStylenames.ItemIndex] := nil;
      end;
    end
    else
    begin
         // Resource style
        LStyle := TStyleManager.Style[StyleName];
    end;
  end;

  if Assigned(LStyle) and not Loading  then
  begin
    PyIDEMainForm.StyleDPIAwareness.ScaleStyle(LStyle);
    FPreview.Caption:=StyleName;
    FPreview.Style:=LStyle;
    TVclStylesPreviewClass(FPreview).Paint;
  end;

end;

class procedure TStyleSelectorForm.SetStyle(StyleName: string);
// StyleName can be either a resource of a file name
var
  SName : string;
  StyleInfo : TStyleInfo;
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
      if TStyleSelectorForm.LoadedStylesDict.ContainsKey(StyleName) then
        TStyleSelectorForm.CurrentSkinName := TStyleSelectorForm.LoadedStylesDict[StyleName]
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
      TStyleSelectorForm.LoadedStylesDict.Add(StyleInfo.Name, StyleName);
    end;
    TStyleManager.SetStyle(StyleInfo.Name);
    TStyleSelectorForm.CurrentSkinName := StyleName;
  end;
end;

procedure TStyleSelectorForm.ActionApplyStyleExecute(Sender: TObject);
var
  StyleName : string;
  FileName : string;
begin
  if LBStyleNames.ItemIndex >= 0 then begin
    StyleName := LBStyleNames.Items[LBStyleNames.ItemIndex];
    if Integer(LBStyleNames.Items.Objects[LbStylenames.ItemIndex]) = 1 then
    begin
      // FileName
      FileName := ExternalStyleFilesDict.Items[StyleName];
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
Var
  Owner : TCustomForm;
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
Var
  FileName : string;
  StyleInfo:  TStyleInfo;
begin
   Loading:=True;

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
            ExternalStyleFilesDict.Add(StyleInfo.Name, FileName);
          end;
       end;

    except
    end;

   Loading:=False;
end;

initialization
  TStyleSelectorForm.CurrentSkinName := TStyleManager.ActiveStyle.Name;
  TStyleSelectorForm.LoadedStylesDict := TDictionary<string, string>.Create;
finalization
  TStyleSelectorForm.LoadedStylesDict.Free;
end.
