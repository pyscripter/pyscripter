{-----------------------------------------------------------------------------
 Unit Name: cThemedVirtualStringTree
 Author:    Kiriakos
 Date:      06-March-2012
 Purpose:   SpTBXLib themeing of TVirtualStringTree
 History:
-----------------------------------------------------------------------------}

unit cThemedVirtualStringTree;

interface
uses
  Types, Windows, Classes, Graphics, VirtualTrees, VirtualTrees.Utils,
  SpTBXSkins;

type

  TThemedVirtualStringTree = class helper for TCustomVirtualStringTree
  private
    procedure VirtualStringTreeAdvancedHeaderDraw(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
    procedure VirtualStringTreeDrawQueryElements(
      Sender: TVTHeader; var PaintInfo: THeaderPaintInfo;
      var Elements: THeaderPaintElements);
    procedure VirtualStringTreeBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VirtualStringTreePaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
  public
    procedure SkinTree;
    procedure ReinitInitializedNode(Node: PVirtualNode; Recursive: Boolean);
    procedure ReinitInitializedChildren(Node: PVirtualNode; Recursive: Boolean);
  end;

implementation

Uses
  dmCommands;

type
  // to help us access protected methods
  TCrackedCustomStringTreeOptions = class(TCustomStringTreeOptions)
  end;

  TCrackedVirtualStringTree = class(TCustomVirtualStringTree)
  end;


{ TThemedVirtualStringTree }

procedure TThemedVirtualStringTree.ReinitInitializedChildren(Node: PVirtualNode;
  Recursive: Boolean);
// Forces all child nodes of Node to be reinitialized.
// If Recursive is True then also the grandchildren are reinitialized.
// Modified version to reinitialize only when the node is already initialized
var
  Run: PVirtualNode;
begin
  if Assigned(Node) then
  begin
    TCrackedVirtualStringTree(Self).InitChildren(Node);
    Run := Node.FirstChild;
  end
  else
  begin
    TCrackedVirtualStringTree(Self).InitChildren(RootNode);
    Run := RootNode.FirstChild;
  end;

  while Assigned(Run) do
  begin
    if vsInitialized in Run.States then
      ReinitInitializedNode(Run, Recursive);
    Run := Run.NextSibling;
  end;
end;

procedure TThemedVirtualStringTree.ReinitInitializedNode(Node: PVirtualNode;
  Recursive: Boolean);

// Forces the given node and all its children (if recursive is True) to be initialized again without
// modifying any data in the nodes nor deleting children (unless the application requests a different amount).

begin
  if Assigned(Node) and (Node <> RootNode) and (vsInitialized in Node.States) then
  begin
    // Remove dynamic styles.
    Node.States := Node.States - [vsChecking, vsCutOrCopy, vsDeleting, vsHeightMeasured];
    TCrackedVirtualStringTree(self).InitNode(Node);
  end;

  if Recursive and (not Assigned(Node) or Assigned(Node) and (vsInitialized in Node.States)) then
    ReinitInitializedChildren(Node, True);
end;

procedure TThemedVirtualStringTree.SkinTree;
begin
  TreeOptions.PaintOptions := TreeOptions.PaintOptions + [toThemeAware, toUseExplorerTheme];
  if SkinManager.GetSkinType = sknSkin then
  begin
    OnAdvancedHeaderDraw := VirtualStringTreeAdvancedHeaderDraw;
    OnHeaderDrawQueryElements := VirtualStringTreeDrawQueryElements;
    OnBeforeCellPaint := VirtualStringTreeBeforeCellPaint;
    OnPaintText := VirtualStringTreePaintText;
  end else begin
    OnAdvancedHeaderDraw := nil;
    OnHeaderDrawQueryElements := nil;
    OnBeforeCellPaint := nil;
    OnPaintText := nil;
  end;
  if Assigned(Header) then
    Header.Invalidate(nil, True);
  Invalidate;
//  Cannot remember why this was needed.
//  if SkinManager.GetSkinType in [sknNone, sknWindows] then
//    TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toAlwaysHideSelection]
//  else
//    TreeOptions.PaintOptions := TreeOptions.PaintOptions + [toAlwaysHideSelection];
end;

procedure TThemedVirtualStringTree.VirtualStringTreeAdvancedHeaderDraw(
  Sender: TVTHeader; var PaintInfo: THeaderPaintInfo;
  const Elements: THeaderPaintElements);
Var
  R : TRect;
  State: TSpTBXSkinStatesType;
  DrawFormat: Cardinal;
  Text : string;
  TextSpace: Integer;
  Size: TSize;
  DC : HDC;
begin
  with PaintInfo do begin
    State := CurrentSkin.GetState(IsEnabled, IsDownIndex, IsHoverIndex, False);
    if ShowSortGlyph and not(IsHoverIndex or IsDownIndex) then
      State := sknsChecked;
  end;

  if hpeBackground in Elements then begin
    R := PaintInfo.PaintRectangle;
    if (PaintInfo.Column = nil) then begin
      SpDrawXPHeader(PaintInfo.TargetCanvas, R, False, False);
    end else with PaintInfo do begin
      SpDrawXPHeader(PaintInfo.TargetCanvas, R, IsHoverIndex, IsDownIndex);
    end;
  end;
  if (hpeText in Elements) and Assigned(PaintInfo.Column) then begin
    R := PaintInfo.TextRectangle;
    DC := PaintInfo.TargetCanvas.Handle;
    Text := PaintInfo.Column.Text;
    GetTextExtentPoint32W(DC, PWideChar(Text), Length(Text), Size);
    TextSpace := R.Right - R.Left;
    if TextSpace < Size.cx then
      Text := ShortenString(DC, Text, TextSpace);
    SetTextColor(DC, ColorToRGB(CurrentSkin.GetTextColor(skncHeader, State)));
    DrawFormat := DT_LEFT or DT_TOP or DT_NOPREFIX;
    if Sender.TreeView.UseRightToLeftReading then
      DrawFormat := DrawFormat + DT_RTLREADING;
    SetBkMode(DC, TRANSPARENT);
    Windows.DrawTextW(DC, PWideChar(Text), Length(Text), R, DrawFormat);
  end;
end;

procedure TThemedVirtualStringTree.VirtualStringTreeBeforeCellPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
Var
  InnerRect, R: TRect;
  NodeWidth : integer;
  CurrentAlignment: TAlignment;
begin
  if SkinManager.IsDefaultSkin then Exit;
  if CellPaintMode <> cpmPaint then Exit;

  with TCustomVirtualStringTree(Sender), TargetCanvas do begin
    if (Column = FocusedColumn) or
       (toFullRowSelect in TCrackedCustomStringTreeOptions(TreeOptions).SelectionOptions)
    then
    begin
      if (vsSelected in Node.States) or (Node = HotNode) then
      begin
        if (toGridExtensions in TCrackedCustomStringTreeOptions(TreeOptions).MiscOptions) or
           (toFullRowSelect in TCrackedCustomStringTreeOptions(TreeOptions).SelectionOptions)
        then
          InnerRect := CellRect
        else begin
          InnerRect:= ContentRect;
          R := GetDisplayRect(Node, Column, True);
          NodeWidth := R.Right - R.Left;
          if Column <= NoColumn then
            CurrentAlignment := Alignment
          else
            CurrentAlignment := Header.Columns[Column].Alignment;
          case CurrentAlignment of
            taLeftJustify:
              with InnerRect do
                if Left + NodeWidth < Right then
                  Right := Left + NodeWidth;
            taCenter:
              with InnerRect do
                if (Right - Left) > NodeWidth then
                begin
                  Left := (Left + Right - NodeWidth) div 2;
                  Right := Left + NodeWidth;
                end;
            taRightJustify:
              with InnerRect do
                if (Right - Left) > NodeWidth then
                  Left := Right - NodeWidth;
          end;
        end;
        if not IsRectEmpty(InnerRect) then begin
          TargetCanvas.FillRect(InnerRect);
          SpDrawXPListItemBackground(TargetCanvas, InnerRect, vsSelected in Node.States, Node = HotNode,
            False, True, False);
        end;
      end;
    end;
  end;
end;

procedure TThemedVirtualStringTree.VirtualStringTreeDrawQueryElements(
  Sender: TVTHeader; var PaintInfo: THeaderPaintInfo;
  var Elements: THeaderPaintElements);
begin
  if SkinManager.IsDefaultSkin then
    Elements := []
  else
    Elements := [hpeBackground, hpeText];
end;

procedure TThemedVirtualStringTree.VirtualStringTreePaintText(
  Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType);
begin
  if SkinManager.IsDefaultSkin then Exit;
  with TVirtualStringTree(Sender) do begin
    if (Column = FocusedColumn) or (toFullRowSelect in TreeOptions.SelectionOptions) then
    begin
      if (Node = HotNode) or (vsSelected in Node.States) then
        TargetCanvas.Font.Color :=
          CurrentSkin.GetTextColor(skncListItem,
            CurrentSkin.GetState(True, False, Node = HotNode,
            vsSelected in Node.States))
      else if CommandsDataModule.PyIDEOptions.UsePythonColorsInIDE and (Color <> clWindow) then
        TargetCanvas.Font.Color := CommandsDataModule.SynPythonSyn.IdentifierAttri.Foreground;
    end else if CommandsDataModule.PyIDEOptions.UsePythonColorsInIDE and (Color <> clWindow) then
      TargetCanvas.Font.Color := CommandsDataModule.SynPythonSyn.IdentifierAttri.Foreground;
  end;
end;

end.
