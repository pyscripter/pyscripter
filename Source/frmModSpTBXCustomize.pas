unit frmModSpTBXCustomize;

interface

uses
  Winapi.Windows,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.CheckLst,
  TB2Item,
  SpTBXCustomizerForm,
  SpTBXEditors,
  SpTBXControls,
  SpTBXItem,
  SpTBXTabs;

type
  TSpTBXCustomizeFormMod = class(TSpTBXCustomizeForm)
    SpTBXLabel2: TSpTBXLabel;
    SpTBXLabel5: TSpTBXLabel;
    lbCategories: TSpTBXListBox;
    Panel2: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure lbCommandsDrawItem(Sender: TObject;
      ACanvas: TCanvas; var ARect: TRect; Index: Integer;
      const State: TOwnerDrawState; const PaintStage: TSpTBXPaintStage;
      var PaintDefault: Boolean);
    procedure lbCommandsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbCommandsEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure lbCategoriesClick(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
  protected
    procedure DoFillCommands(ToolbarList, ItemList, ShortcutsList: TStringList); override;
  public
    procedure SortCommands;
  end;

const
  CategoryOther = 'Other';

var
  SpTBXCustomizeFormMod: TSpTBXCustomizeFormMod;

implementation

{$R *.dfm}

uses
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ActnList,
  JvGnugettext,
  SpTBXCustomizer,
  uCommonFunctions,
  frmPyIDEMain;

procedure TSpTBXCustomizeFormMod.FormCreate(Sender: TObject);
begin
  inherited;
  SetDefaultUIFont(Font);
  TranslateComponent(Self);
end;

{ TSpTBXCustomizeFormMod }

procedure TSpTBXCustomizeFormMod.DoFillCommands(ToolbarList, ItemList,
  ShortcutsList: TStringList);
var
  Item: TTBCustomItem;
begin
  inherited;

  lbCategories.Clear;
  for var I := 0 to ItemList.Count - 1 do begin
    Item := ItemList.Objects[I] as TTBCustomItem;
    if Assigned(Item) and Assigned(Item.Action) and (Item.Action is TCustomAction) then begin
      if lbCategories.Items.IndexOf(_((Item.Action as TCustomAction).Category)) < 0 then
        lbCategories.Items.Add(_((Item.Action as TCustomAction).Category));
    end;
  end;
  lbCategories.Items.Add(_(CategoryOther));

  lbCommands.Clear;
  if lbCategories.Count > 0 then begin
    lbCategories.ItemIndex := 0;
    lbCategoriesClick(Self);
  end;
end;

procedure TSpTBXCustomizeFormMod.lbCategoriesClick(Sender: TObject);
var
  Category: string;
  Item: TTBCustomItem;
begin
  if lbCategories.ItemIndex < 0 then Exit;
  Category := lbCategories.Items[lbCategories.ItemIndex];

  lbCommands.Clear;

  if Category = _(CategoryOther) then begin
    for var I := 1 to FItemList.Count - 1 do begin  // Skip the first item
      Item := FItemList.Objects[I] as TTBCustomItem;
      if not Assigned(Item) or not Assigned(Item.Action) or
        not (Item.Action is TCustomAction)
      then
        lbCommands.Items.AddObject(FItemList[I], Item);
    end;
  end else begin
    for var I := 1 to FItemList.Count - 1 do begin  // Skip the first item
      Item := FItemList.Objects[I] as TTBCustomItem;
      if Assigned(Item) and Assigned(Item.Action) and (Item.Action is TCustomAction) and
        (_((Item.Action as TCustomAction).Category) = Category)
      then
        lbCommands.Items.AddObject(FItemList[I], Item);
    end;
  end;
  if FItemList.Count > 0 then
    lbCommands.Items.InsertObject(0, FItemList[0], FItemList.Objects[0]);
  SortCommands;
end;

procedure TSpTBXCustomizeFormMod.lbCommandsDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  OrigItem: TTBCustomItem;
  Str: string;
  Category: string;
  Index: Integer;
begin
  if Assigned(Source) and (Source is TSpTBXItemDragObject) and
    (TSpTBXItemDragObject(Source).SourceControl <> Sender) then
  begin
    OrigItem := TSpTBXItemDragObject(Source).SouceItem;
    // Remove the item from its parent
    OrigItem.Parent.Remove(OrigItem);
    // Add the item to the Customizer.Items property
    Customizer.Items.Add(OrigItem);
    Str := SpCustomizerGetWideCaption(OrigItem);
    // Add the item entry in the commands list
    if OrigItem is TTBSeparatorItem then
      FSeparatorList.InsertObject(0, Str, OrigItem) // Insert the separator in the first position
    else begin
      FItemList.AddObject(Str, OrigItem);
      if Assigned(OrigItem.Action) and (OrigItem.Action is TCustomAction) then
         Category := (OrigItem.Action as TCustomAction).Category
      else
        Category := CategoryOther;
      Index := lbCategories.Items.IndexOf(Category);
      if Index < 0 then
        Index := lbCategories.Items.Add(Category);

      lbCategories.ItemIndex := Index;
    end;
  end;
end;

procedure TSpTBXCustomizeFormMod.lbCommandsDrawItem(Sender: TObject;
  ACanvas: TCanvas; var ARect: TRect; Index: Integer;
  const State: TOwnerDrawState; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
begin
  if PaintStage <> pstPrePaint then Exit;
  if Index = 0 then begin
    // Draw the separator
    var Rect := ARect;
    InflateRect(Rect, -20, -4);
    SpDrawXPMenuSeparator(nil, ACanvas, Rect, False, False, FCurrentPPI);
    PaintDefault := False;
  end
  else
    inherited;
end;

procedure TSpTBXCustomizeFormMod.lbCommandsEndDrag(Sender, Target: TObject; X,
  Y: Integer);
var
  Accepted: Boolean;
  I, J: Integer;
begin
  // When dropping an item on a toolbar we must remove the item from the list
  Accepted := Assigned(Target) and (Target <> Sender);
  if Accepted then begin
    I := lbCommands.ItemIndex;
    if I > -1 then
      if I = 0 then begin
        FSeparatorList.Delete(0);
      end
      else begin
        for J := 0 to FItemList.Count - 1 do
          if FItemList.Objects[J] = lbCommands.Items.Objects[I] then begin
            FItemList.Delete(J);
            Break;
          end;
        lbCategoriesClick(Self);
      end;
  end;
end;

procedure TSpTBXCustomizeFormMod.ResetButtonClick(Sender: TObject);
begin
  if StyledMessageDlg(
    _('This option will reset IDE toolbars and shortcuts to the factory settings.'
    +#13+#10+'Do you want to proceed?'),
    mtWarning, [mbOK, mbCancel], 0) = mrOk
  then
    PyIDEMainForm.LoadToolbarItems(FactoryToolbarItems);
end;

procedure TSpTBXCustomizeFormMod.SortCommands;
var
  Separator: string;
  Item: TObject;
begin
  if lbCommands.Count = 0 then Exit;
  Separator := lbCommands.Items[0];
  Item := lbCommands.Items.Objects[0];
  lbCommands.Items.Delete(0);
  lbCommands.Sorted := True;
  lbCommands.Sorted := False;
  lbCommands.Items.InsertObject(0, Separator, Item);
end;

end.
