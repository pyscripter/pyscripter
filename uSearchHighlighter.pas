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
    fSynEdit : TSynEdit;
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
  Math, uEditAppIntfs, frmEditor;

{ THighlightSearchPlugin }

procedure THighlightSearchPlugin.AfterPaint(ACanvas: TCanvas;
  const AClip: TRect; FirstLine, LastLine: integer);
var
  i : Integer;
  FoundItem : TFoundItem;
  Pix: TPoint;
  S : string;
begin
  for i := 0 to fFoundItems.Count - 1 do begin
    FoundItem := fFoundItems[i] as TFoundItem;
    if InRange(FoundItem.Start.Line, FirstLine, LastLine) then begin
      Pix := fSynEdit.RowColumnToPixels(fSynEdit.BufferToDisplayPos(FoundItem.Start));
      ACanvas.Brush.Color := clYellow;
      ACanvas.Brush.Style := bsSolid;
      SetTextCharacterExtra(ACanvas.Handle, fSynEdit.CharWidth - ACanvas.TextWidth('W'));
      S := Copy(fSynEdit.Lines[FoundItem.Start.Line-1],
             FoundItem.Start.Char, FoundItem.Length);
      ACanvas.TextOut(Pix.X, Pix.Y, S);
    end;
  end;
end;

constructor THighlightSearchPlugin.Create(ASynEdit: TSynEdit;
  AFoundItems: TObjectList);
begin
  inherited Create(ASynEdit);
  fSynEdit := ASynEdit;
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
var
  i: Integer;
  Editor : IEditor;
begin
  for i := 0 to GI_EditorFactory.Count - 1 do begin
    Editor := GI_EditorFactory.Editor[i];
    InvalidateHighlightedTerms(Editor.SynEdit,
      TEditorForm(Editor.Form).FoundSearchItems);
    InvalidateHighlightedTerms(Editor.SynEdit2,
      TEditorForm(Editor.Form).FoundSearchItems);
    TEditorForm(Editor.Form).FoundSearchItems.Clear;
  end;
end;

end.
