unit dlgStyleSelector;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, 
  System.Types, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Styles.Ext,
  Vcl.ActnList, Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan, Vcl.ToolWin,
  System.Actions, dlgPyIDEBase, SpTBXItem, SpTBXControls, 
  System.Generics.Collections, Vcl.Themes, Vcl.Styles;

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
    ExternalFilesDict :  TDictionary<String, string>;
    LastSelectedStyle : TCustomStyle;
    procedure FillVclStylesList;
  public
    class var CurrentSkinName : string;
    class procedure Execute;
    class procedure SetStyle(StyleName : string);
  end;


implementation
uses
  IOUtils,
  dmCommands;

type
 TVclStylesPreviewClass = class(TVclStylesPreview);

{$R *.dfm}

procedure TStyleSelectorForm.FormCreate(Sender: TObject);
begin
   Loading:=False;
   LBStyleNames.Sorted := True;
   ExternalFilesDict := TDictionary<String, string>.Create;
   FStylesPath := CommandsDataModule.StylesFilesDir;
   FPreview:=TVclStylesPreview.Create(Self);
   FPreview.Parent:=Panel1;
   FPreview.BoundsRect := Panel1.ClientRect;
   FillVclStylesList;
end;

procedure TStyleSelectorForm.FormDestroy(Sender: TObject);
begin
  ExternalFilesDict.Free;
  FPreview.Free;
  FreeAndNil(LastSelectedStyle);
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
  LStyle : TCustomStyle;
  FileName : string;
  SourceInfo: TSourceInfo;
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
        FileName := ExternalFilesDict.Items[StyleName];
        LStyle := TCustomStyleExt.Create(FileName);
      end;
    end
    else 
    begin
         // Resource style                                                              
        SourceInfo:=TStyleManager.StyleSourceInfo[StyleName];
        LStyle := TCustomStyleExt.Create(TStream(SourceInfo.Data));
    end;

  end;

  if Assigned(LStyle) and not Loading  then
  begin
    FPreview.Caption:=StyleName;
    FPreview.Style:=LStyle;
    TVclStylesPreviewClass(FPreview).Paint;
  end;

  FreeAndNil(LastSelectedStyle);
  LastSelectedStyle := LStyle;
end;


class procedure TStyleSelectorForm.SetStyle(StyleName: string);
// StyleName can be either a reousrce of a file name
var
  SName : string;
  LStyle : TCustomStyle;
  IsResource : boolean;
begin
  IsResource := False;
  for SName in TStyleManager.StyleNames do
  begin
    if SName = StyleName then 
    begin
      IsResource := True;
      break;
    end;
  end;     
    
  if IsResource then
  begin
       // Resource style                                                              
    TStyleManager.SetStyle(StyleName);
    TStyleSelectorForm.CurrentSkinName := StyleName;
  end
  else 
  begin
    // FileName
    if TStyleManager.IsValidStyle(StyleName) then
    begin
      LStyle := TCustomStyleExt.Create(StyleName);
      TStyleManager.SetStyle(LStyle);
      TStyleSelectorForm.CurrentSkinName := StyleName;
    end;
  end;

end;

procedure TStyleSelectorForm.ActionApplyStyleExecute(Sender: TObject);
var
  StyleName : String;
  FileName : string;
  LStyle : TCustomStyle;
  begin
  if LBStyleNames.ItemIndex >= 0 then begin
    StyleName := LBStyleNames.Items[LBStyleNames.ItemIndex];
    if Integer(LBStyleNames.Items.Objects[LbStylenames.ItemIndex]) = 1 then
    begin
      // FileName
      if not Loading then
      begin
        FileName := ExternalFilesDict.Items[StyleName];
        LStyle := TCustomStyleExt.Create(FileName);
        TStyleManager.SetStyle(LStyle);
        TStyleSelectorForm.CurrentSkinName := FileName;
      end;
    end
    else 
    begin
         // Resource style                                                              
      TStyleManager.SetStyle(StyleName);
      TStyleSelectorForm.CurrentSkinName := StyleName;
    end;
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
            ExternalFilesDict.Add(StyleInfo.Name, FileName);
          end;
       end;
    
    except 
    end;

   Loading:=False;
end;

Initialization

  TStyleSelectorForm.CurrentSkinName := TStyleManager.ActiveStyle.Name;

end.
