{-----------------------------------------------------------------------------
 Unit Name: uSearchHighlighter
 Author:    Kiriakos Vlahos
 Date:      24-May-2007
 Purpose:   Classes and support routints for highlighting a search term
-----------------------------------------------------------------------------}

unit uSearchHighlighter;

interface

uses
  Windows, Classes, SysUtils, Contnrs, Graphics, Synedit,
  SynEditTypes, SynEditMiscClasses;

type
  TFoundItem = class
    Start : TBufferCoord;
    Length : Word;
  end;

  THighlightSearchPlugin = class(TSynEditPlugin)
  private
    fFoundItems: TObjectList;
  protected
    procedure AfterPaint(ACanvas: TCanvas; const AClip: TRect;
      FirstLine, LastLine: integer); override;
    procedure LinesInserted(FirstLine, Count: integer); override;
    procedure LinesDeleted(FirstLine, Count: integer); override;
  public
    constructor Create(ASynEdit: TSynEdit; AFoundItems: TObjectList);
  end;

  procedure FindSearchTerm(ATerm : string; SynEdit : TSynEdit;
    FoundItems : TObjectList; SearchEngine : TSynEditSearchCustom;
    SearchOptions : TSynSearchOptions);

  procedure InvalidateHighlightedTerms(SynEdit : TSynEdit; FoundItems : TObjectList);
  procedure ClearAllHighlightedTerms;


implementation
Uses
  Math, uEditAppIntfs, frmEditor, cPyScripterSettings;

{ THighlightSearchPlugin }

procedure THighlightSearchPlugin.AfterPaint(ACanvas: TCanvas;
  const AClip: TRect; FirstLine, LastLine: integer);

  procedure PaintHightlight(StartXY, EndXY : TBufferCoord);
  var
    Pix: TPoint;
    S : string;
  begin
    if StartXY.Char < EndXY.Char then begin
      Pix := Editor.RowColumnToPixels(Editor.BufferToDisplayPos(StartXY));
      ACanvas.Brush.Color := PyIDEOptions.HighlightSelectedWordColor;
      ACanvas.Brush.Style := bsSolid;
      SetTextCharacterExtra(ACanvas.Handle, Editor.CharWidth - ACanvas.TextWidth('W'));
      S := Copy(Editor.Lines[StartXY.Line-1],
             StartXY.Char, EndXY.Char - StartXY.Char);
      ACanvas.TextOut(Pix.X, Pix.Y, S);
    end;
  end;

var
  i : Integer;
  FoundItem : TFoundItem;
  StartXY, EndXY : TBufferCoord;
begin
  FirstLine := Editor.RowToLine(FirstLine);
  LastLine := Editor.RowToLine(LastLine);
  for i := 0 to fFoundItems.Count - 1 do begin
    FoundItem := fFoundItems[i] as TFoundItem;
    if InRange(FoundItem.Start.Line, FirstLine, LastLine) and not
      (Editor.UseCodefolding and Editor.AllFoldRanges.FoldHidesLine(FoundItem.Start.Line)) then
    begin
      // do not highlight selection
      // Highlight front part
      StartXY := FoundItem.Start;
      EndXY := StartXY;
      while not Editor.IsPointInSelection(EndXY) and
        (EndXY.Char < FoundItem.Start.Char + FoundItem.Length)
      do
        Inc(EndXY.Char);
      PaintHightlight(StartXY, EndXY);

      StartXY.Char := EndXY.Char;
      EndXY.Char := FoundItem.Start.Char + FoundItem.Length;
      // Skip Selection
      while Editor.IsPointInSelection(StartXY) and (StartXY.Char < EndXY.Char) do
        Inc(StartXY.Char);
      // Highlight end part
       PaintHightlight(StartXY, EndXY);
    end;
  end;
end;

constructor THighlightSearchPlugin.Create(ASynEdit: TSynEdit;
  AFoundItems: TObjectList);
begin
  inherited Create(ASynEdit);
  FHandlers := [phLinesInserted, phLinesDeleted, phAfterPaint];
  fFoundItems := AFoundItems;
end;

procedure THighlightSearchPlugin.LinesDeleted(FirstLine, Count: integer);
begin
  // Do nothing
end;

procedure THighlightSearchPlugin.LinesInserted(FirstLine, Count: integer);
begin
  // Do nothing
end;

procedure FindSearchTerm(ATerm : string; SynEdit : TSynEdit;
  FoundItems : TObjectList; SearchEngine : TSynEditSearchCustom;
  SearchOptions : TSynSearchOptions);
var
  i: Integer;
  j: Integer;
  FoundItem : TFoundItem;
begin
  InvalidateHighlightedTerms(SynEdit, FoundItems);
  FoundItems.Clear;

  if ATerm = '' then Exit;

  for i := 0 to SynEdit.Lines.Count - 1 do begin
    SearchEngine.Options := SearchOptions;
    SearchEngine.Pattern := ATerm;
    SearchEngine.FindAll(SynEdit.Lines[i]);
    for j := 0 to SearchEngine.ResultCount - 1 do begin
      FoundItem := TFoundItem.Create;
      FoundItem.Start :=  BufferCoord(SearchEngine.Results[j], i + 1);
      FoundItem.Length := SearchEngine.Lengths[j];
      FoundItems.Add(FoundItem);
      SynEdit.InvalidateLine(i+1);
    end;
  end;
end;

procedure InvalidateHighlightedTerms(SynEdit : TSynEdit; FoundItems : TObjectList);
var
  i: Integer;
  FoundItem : TFoundItem;
begin
  for i := 0 to FoundItems.Count - 1 do begin
    FoundItem := FoundItems[i] as TFoundItem;
    SynEdit.InvalidateLine(FoundItem.Start.Line);
  end;
end;

procedure ClearAllHighlightedTerms;
begin
  GI_EditorFactory.ApplyToEditors(procedure(Editor: IEditor)
  begin
    TEditorForm(Editor.Form).ClearSearchItems;
  end);
end;

end.
