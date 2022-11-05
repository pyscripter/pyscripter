{-----------------------------------------------------------------------------
 Unit Name: uSearchHighlighter
 Author:    Kiriakos Vlahos
 Date:      24-May-2007
 Purpose:   Classes and support routints for highlighting a search term
-----------------------------------------------------------------------------}

unit uSearchHighlighter;

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  Vcl.Graphics,
  Synedit,
  SynEditTypes,
  SynEditMiscClasses,
  uEditAppIntfs;

  procedure RegisterSearchHighlightIndicatorSpec(Editor: IEditor);
  procedure HighligthtSearchTerm(ATerm : string; Editor: IEditor;
    SearchEngine : TSynEditSearchCustom; SearchOptions : TSynSearchOptions);
  procedure ClearSearchHighlight(Editor: IEditor);
  procedure ClearAllHighlightedTerms;

implementation

uses
  SynDWrite,
  cPyScripterSettings;

const SearchHighlightIndicatorId: TGUID  = '{A59BCD6A-02A6-4B34-B28C-D9EACA0C9F09}';


procedure ClearAllHighlightedTerms;
begin
  GI_EditorFactory.ApplyToEditors(procedure(Editor: IEditor)
  begin
    ClearSearchHighlight(Editor);
  end);
end;

procedure RegisterSearchHighlightIndicatorSpec(Editor: IEditor);
const
  Alpha = 0.3;  // could allow customization
begin
  var Spec := TSynIndicatorSpec.New(sisRoundedFilledRectangle, clNoneF,
    D2D1ColorF(PyIDEOptions.HighlightSelectedWordColor, Alpha), []);

  Editor.SynEdit.Indicators.RegisterSpec(SearchHighlightIndicatorId, Spec);
  Editor.SynEdit2.Indicators.RegisterSpec(SearchHighlightIndicatorId, Spec);
end;

procedure ClearSearchHighlight(Editor: IEditor);
begin
  if Editor.HasSearchHighlight then
  begin
    Editor.SynEdit.Indicators.Clear(SearchHighlightIndicatorId);
    Editor.SynEdit2.Indicators.Clear(SearchHighlightIndicatorId);
    Editor.HasSearchHighlight := False;
  end;
end;

procedure HighligthtSearchTerm(ATerm : string; Editor: IEditor;
  SearchEngine : TSynEditSearchCustom; SearchOptions : TSynSearchOptions);
var
  I: Integer;
  J: Integer;
  Indicator: TSynIndicator;
begin
  ClearSearchHighlight(Editor);
  if ATerm = '' then Exit;

  Indicator.Id := SearchHighlightIndicatorId;
  for I := 0 to Editor.SynEdit.Lines.Count - 1 do begin
    SearchEngine.Options := SearchOptions;
    SearchEngine.Pattern := ATerm;
    SearchEngine.FindAll(Editor.SynEdit.Lines[i]);

    for J := 0 to SearchEngine.ResultCount - 1 do begin
      Indicator.CharStart := SearchEngine.Results[j];
      Indicator.CharEnd := Indicator.CharStart + SearchEngine.Lengths[j];
      Editor.SynEdit.Indicators.Add(I + 1, Indicator);
      Editor.SynEdit2.Indicators.Add(I + 1, Indicator);
    end;
  end;
  Editor.HasSearchHighlight := True;
end;

end.
