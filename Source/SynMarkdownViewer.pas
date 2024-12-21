{-----------------------------------------------------------------------------
 Unit Name: SynMarkdownViewer
 Author:    PyScripter
 Date:      20-Dec-2024
 Purpose:   Component that turns SynEdit into a simple but limited Markdown
            viewer.
            Supports
              - headers,
              - emphasis,
              - links
              - horizontal rules,
              - bullets and
              - <br>.
-----------------------------------------------------------------------------}

unit SynMarkdownViewer;

interface

uses
  WinApi.Windows,
  System.Classes,
  Vcl.Controls,
  System.Generics.Collections,
  System.RegularExpressions,
  SynEdit,
  SynEditMiscClasses;

type
  TMarkdownIndicators = record
  private
    const IdHeading1: TGUID = '{36F82F10-1EE0-4CA2-88B7-1D1D3F3AB200}';
    const IdHeading2: TGUID = '{336DA2C9-2E1C-4910-B2FD-CED4C764567A}';
    const IdHeading3: TGUID = '{401785F5-562E-4CFE-A631-E62B09081E6C}';
    const IdHeadingOther: TGUID = '{A142C959-E569-4CB0-A4F7-7680D6C8687C}';
    const IdBold: TGUID = '{E64EA8C8-2B67-48A0-A056-2218D56A38D7}';
    const IdItalic: TGUID = '{760EC018-C99D-4B44-8F2C-E2F6B1FE34FB}';
    const IdBoldItalic: TGUID = '{39B9EFC7-9DEF-45B6-86E2-FBD20E119712}';
    const IdUnderline: TGUID = '{CB9B253C-94DA-4B6C-9E35-DC98A986EB08}';
    const IdLink: TGUID = '{55740423-8BA6-41C7-B0FA-0B768387796D}';
  public
    Heading1: TSynIndicatorSpec;
    Heading2: TSynIndicatorSpec;
    Heading3: TSynIndicatorSpec;
    HeadingOther: TSynIndicatorSpec;
    Bold: TSynIndicatorSpec;
    Italic: TSynIndicatorSpec;
    BoldItalic: TSynIndicatorSpec;
    Underline: TSynIndicatorSpec;
    Link: TSynIndicatorSpec;
    constructor Create(Editor: TCustomSynEdit);
  end;

  TMarkDownRegEx = record
    Bullets: TRegEx;
    MergeLines: TRegEx;
    SplitLines: TRegEx;
    Emphasis: TRegEx;
    HorzRule: TRegEx;
    Header: TRegEx;
    SoftLineBreak: TRegEx;
    Links: TRegEx;
    function New: TMarkDownRegEx;
  end;

  TSynOnClickLink = procedure(Sender: TObject; Link: string) of object;

  TSynMarkdownViewer = class(TSynEdit)
  private
    FHorzRules: TList<Integer>;
    FLinks: TList<string>;
    FOnClickLink: TSynOnClickLink;
    procedure SetMarkdown(Value: string);
  protected
    MarkdownIndicators: TMarkdownIndicators;
    procedure DoOnMouserCursor(const aLineCharPos: TBufferCoord;
      var aCursor: TCursor); override;
    procedure DoOnPaint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class constructor Create;
    property Markdown: string write SetMarkdown;
    class var RegExps: TMarkDownRegEx;
  published
    property OnCLickLink: TSynOnClickLink read FOnCLickLink write FOnClickLink;
    property ReadOnly default True;
  end;

implementation

uses
  Winapi.ShellAPI,
  System.UITypes,
  System.SysUtils,
  System.Math,
  System.RegularExpressionsCore,
  Vcl.Graphics,
  SynDWrite;

{ TSynMarkdownViewer }

constructor TSynMarkdownViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHorzRules := TList<Integer>.Create;
  FLinks := TList<string>.Create;

  ReadOnly := True;
  RightEdge := 0;

  Gutter.Visible := False;
  ScrollbarAnnotations.Clear;

  UseCodeFolding := False;
  WordWrap := True;
  LockUndo;

  MarkdownIndicators.Create(Self);
end;

class constructor TSynMarkdownViewer.Create;
begin
  RegExps := TSynMarkdownViewer.RegExps.New;
end;

destructor TSynMarkdownViewer.Destroy;
begin
  FHorzRules.Free;
  FLinks.Free;
  inherited;
end;

procedure TSynMarkdownViewer.DoOnMouserCursor(const aLineCharPos: TBufferCoord;
  var aCursor: TCursor);
var
  Indicator: TSynIndicator;
  Underline: TSynIndicator;
begin
  if Indicators.IndicatorAtPos(aLineCharPos, TMarkdownIndicators.IdLink, Indicator) then
  begin
    aCursor := crHandPoint;
    if not Indicators.IndicatorAtPos(aLineCharPos, TMarkdownIndicators.IdUnderline, Underline) then
      Indicators.Add(aLineCharPos.Line, TSynIndicator.New(
        TMarkdownIndicators.idUnderline, Indicator.CharStart, Indicator.CharEnd));
  end else
  begin
    Indicators.Clear(TMarkdownIndicators.IdUnderline);
    inherited;
  end;
end;

procedure TSynMarkdownViewer.DoOnPaint;
var
  Line, Row: Integer;
begin
  // paint horizontal rulers

  Canvas.Pen.Width := MulDiv(2, FCurrentPPI, 96);

  for Line in FHorzRules do
  begin
    if Line > Lines.Count then Continue;

    Row := LineToRow(Line + 1) - 1;
    if InRange(Row, TopLine, TopLine + LinesInWindow) then
    begin
       Canvas.MoveTo(TextMargin, (Row - TopLine + 1) * LineHeight - Canvas.Pen.Width);
       Canvas.LineTo(ClientWidth, (Row - TopLine + 1) * LineHeight - Canvas.Pen.Width);
    end;
  end;

  inherited;
end;

procedure TSynMarkdownViewer.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Indicator: TSynIndicator;
  BC: TBufferCoord;
begin
  BC := DisplayToBufferPos(PixelsToNearestRowColumn(X, Y));
  if (Button = mbLeft) and (Shift = [ssLeft]) and
    Indicators.IndicatorAtPos(BC, TMarkdownIndicators.IdLink, Indicator)
  then
  begin
    MouseCapture := False;
    if Assigned(FOnClickLink) then
      FOnClickLink(Self, FLinks[Indicator.Tag])
    else
      ShellExecute(0, nil, PChar(FLinks[Indicator.Tag]), nil, nil, 1);
  end
  else
    inherited;
end;

procedure TSynMarkdownViewer.SetMarkdown(Value: string);


  procedure FormatParagraphs(var Lines: TArray<string>);
  type
    TLineInfo = (liEmpty, liHeader, liRuler, liBullet, liCode, liLineBreak);
    TLineInfos = set of TLineInfo;

    procedure AddEmptyLineBefore(var LineNo: Integer);
    // Insert empty line before if needed
    begin
      if (LineNo > 0) and (Lines[LineNo - 1] <> '') then
      begin
        Insert('', Lines, LineNo);
        Inc(LineNo);
      end;
    end;

    procedure AddEmptyLineAfter(LineNo: Integer);
    // Insert empty line after if needed
    begin
      if not ((LineNo < High(Lines)) and (Lines[LineNo + 1] = '')) then
        Insert('', Lines, LineNo + 1);
    end;

  var
    PrevLI, CurrLI: TLineInfos;
    LineNo: Integer;
    Line: string;
    Index: Integer;
  begin
    PrevLI := [];
    LineNo := 0;

    while LineNo <= High(Lines) do
    begin
      Line := Lines[LineNo];
      CurrLI := [];

      // Skip code blocks
      if liCode in PrevLI then
      begin
        if Line.StartsWith('```') then
          // End of code block -
          AddEmptyLineAfter(LineNo);
        Inc(LineNo);
        Continue;
      end;

      if Line = '' then
        Include(CurrLI, liEmpty)
      else if RegExps.Header.IsMatch(Line) then
        Include(CurrLI, liHeader)
      else if RegExps.HorzRule.IsMatch(Line) then
        Include(CurrLI, liRuler)
      else if RegExps.Bullets.IsMatch(Line) then
        Include(CurrLI, liBullet)
      else if Line.StartsWith('```') then
      begin
        // Start of a code block
        PrevLI := [liCode];
        AddEmptyLineBefore(LineNo);
        Inc(LineNo);
        Continue;
      end
      else if RegExps.Bullets.IsMatch(Line) then
        Include(CurrLI, liBullet);

      with RegExps.SoftLineBreak.Match(Line) do
        if Success then
        begin
          Include(CurrLI, liLineBreak);
          Delete(Line, Line.Length - Length + 1, Length);
          Lines[LineNo] := Line;
        end;

      if (LineNo > 0) and (CurrLI - [liLineBreak] = []) and (PrevLI - [liBullet] = []) then
      begin
        // Merge Lines into a paragraph
        Lines[LineNo - 1] := Lines[LineNo - 1].TrimRight + ' ' + Line.TrimLeft;
        Delete(Lines, LineNo, 1);
        if liLineBreak in CurrLI then
          Include(PrevLI, liLineBreak);
        Continue;
      end;

      // Add empty line before if is header or first bullet
      if (liHeader in CurrLI) or ((liBullet in CurrLI) and not (liBullet in PrevLI)) then
        AddEmptyLineBefore(LineNo);

      // Add empty line after if is header
      if liHeader in CurrLI then
        AddEmptyLineAfter(LineNo);

      // Delete empty lines before/after ruler
      if liRuler in CurrLI then
      begin
        if (LineNo > 0) and (PrevLI = [liEmpty]) then
        begin
          Delete(Lines, LineNo - 1, 1);
          Dec(LineNo);
        end;
        if (LineNo < High(Lines)) and (Lines[LineNo + 1] = '') then
          Delete(Lines, LineNo + 1, 1);
      end;

      // Break Lines contains <br> in the middle
      Index := Pos('<br>', Line);
      if Index > 0 then
      begin
        //Add back the last break if any
        if liLineBreak in CurrLI then
          Line := Line + '<br>';
        Lines[LineNo] := Copy(Line, 1, Index + 3);
        Insert(Copy(Line, Index + 4), Lines, LineNo + 1);
        Continue;
      end;

      Inc(LineNo);
      PrevLI := CurrLI;
    end;
  end;

  procedure ProcessLine(Line: string);
  {
    This procedure handles the following markdown syntax
    - Headers
    - Horizontal rules
    - Bullets
    - Emphasis (bold, italic, bold-italic
    - Links
  }

  type
    TLink = record
      CharIdx: Integer;
      DisplayString: string;
      Link: string;
    end;

  var
    Links: TArray<TLink>;

  procedure AdjustLinks(Start, Stop, CharsRemoved: Integer);
  var
    Idx: Integer;
  begin
    for Idx := 0 to High(Links) do
    begin
      if Start < Links[Idx].CharIdx then
        Dec(Links[Idx].CharIdx, CharsRemoved);
      if Stop < Links[Idx].CharIdx then
        Dec(Links[Idx].CharIdx, CharsRemoved);
    end;
  end;

  var
    LineNo: Integer;
    HeaderGuid: TGuid;
    EmphasisGuid: TGuid;
    Start, Stop: Integer;
    Match: TMatch;
    Matches: TMatchCollection;
    DeletedChars: Integer;
    Link: TLink;
  begin
    // Headers
    HeaderGuid := TGuid.Empty;
    Match := RegExps.Header.Match(Line);
    if Match.Success and InRange(Match.Groups[1].Value.Length, 1, 6) then
    begin
      case Match.Groups[1].Value.Length of
        1: HeaderGuid := MarkdownIndicators.IdHeading1;
        2: HeaderGuid := MarkdownIndicators.IdHeading2;
        3: HeaderGuid := MarkdownIndicators.IdHeading3;
        4, 5, 6: HeaderGuid := MarkdownIndicators.IdHeadingOther;
      end;
      Line := Copy(Line, Match.Groups[1].Value.Length + 2);
      LineNo := Lines.Add(Line);
      Indicators.Add(LineNo + 1,
        TSynIndicator.Create(HeaderGuid, 1, Line.Length + 1), False);

      // Add Horizontal ruler to header 1
      if Match.Groups[1].Value.Length = 1 then
        FHorzRules.Add(LineNo + 1);

      Exit;
    end;

   // Horizontal Rules
   if RegExps.HorzRule.IsMatch(Line.Trim) then
   begin
     LineNo := Lines.Add('');
     FHorzRules.Add(LineNo + 1);
     Exit;
   end;

   // Bullets
   // Show a real bullet and indend by 2 spaces
   Line := RegExps.Bullets.Replace(Line, '  $1' + #$2022+ ' ');

   // Preprocess Links
   Links := [];
   Match := RegExps.Links.Match(Line);
   while Match.Success do
   begin
     // after the replacement the display string will
     // start at the start of the start of the match
     Link.CharIdx := Match.Index;
     Link.Link := Match.Groups[2].Value;
     // the display string may contain emphasis tags
     Link.DisplayString :=  RegExps.Emphasis.Replace(
       Match.Groups[1].Value, '$1$2$3');
     Links := Links + [Link];
     // Keep just the display string
     Line := RegExps.Links.Replace(Line, '$1', 1);
     Match := RegExps.Links.Match(Line);
   end;

   // Emphasis tags
   DeletedChars := 0;
   EmphasisGuid := TGuid.Empty;
   Matches := RegExps.Emphasis.Matches(Line);
   //Add the line without the *
   if Matches.Count > 0 then
     Line := RegExps.Emphasis.Replace(Line, '$1$2$3');
   LineNo := Lines.Add(Line);
   // Add the emphasis indicators
   for Match in Matches do
   begin
     Start := 0;  // To avoid compiler warning
     Stop := 0;
     if Match.Groups[1].Length > 0 then
     begin
       EmphasisGuid := MarkdownIndicators.IdBoldItalic;
       Start := Match.Groups[1].Index - DeletedChars - 3;
       Stop := Start + Match.Groups[1].Length;
       Inc(DeletedChars, 6);
       AdjustLinks(Start, Stop, 3);
     end
     else if Match.Groups[2].Length > 0 then
     begin
       EmphasisGuid := MarkdownIndicators.IdBold;
       Start := Match.Groups[2].Index - DeletedChars - 2;
       Stop := Start + Match.Groups[2].Length;
       Inc(DeletedChars, 4);
       AdjustLinks(Start, Stop, 2);
     end
     else if Match.Groups[3].Length > 0 then
     begin
       EmphasisGuid := MarkdownIndicators.IdItalic;
       Start := Match.Groups[3].Index - DeletedChars - 1;
       Stop := Start + Match.Groups[3].Length;
       Inc(DeletedChars, 2);
       AdjustLinks(Start, Stop, 1);
     end;
     Indicators.Add(LineNo + 1,
        TSynIndicator.New(EmphasisGuid, Start, Stop), False);
    end;

    // Finally process links
    for Link in Links do
      Indicators.Add(LineNo + 1, TSynIndicator.New(
        TMarkdownIndicators.IdLink,
        Link.CharIdx,
        Link.CharIdx + Link.DisplayString.Length,
        FLinks.Add(Link.Link)));
  end;

var
  LineArr: TArray<string>;
  Line: string;

begin
  ClearAll;
  FHorzRules.Clear;

  // Merge concequtive linebreaks
  Value := RegExps.MergeLines.Replace(Value, sLineBreak+ sLineBreak);
  // Split Value to lines
  LineArr := RegExps.SplitLines.Split(Value);
  // Do paragraph formatting according to Markdown rules
  FormatParagraphs(LineArr);

  // Final step: process each line for Markdown syntax
  BeginUpdate;
  try
    for Line in LineArr do
      ProcessLine(Line);
  finally
    EndUpdate;
  end;
end;

{ TMarkdownIndicators }

constructor TMarkdownIndicators.Create(Editor: TCustomSynEdit);
begin
  Heading1.Create(sisTextDecoration, clNoneF, clNoneF, [TFontStyle.fsBold, TFontStyle.fsItalic]);
  Heading2.Create(sisTextDecoration, clNoneF, clNoneF, [TFontStyle.fsBold, TFontStyle.fsItalic, TFontStyle.fsUnderline]);
  Heading3.Create(sisTextDecoration, clNoneF, clNoneF, [TFontStyle.fsBold, TFontStyle.fsUnderline]);
  HeadingOther.Create(sisTextDecoration, clNoneF, clNoneF, [TFontStyle.fsUnderline]);
  Bold.Create(sisTextDecoration, clNoneF, clNoneF, [TFontStyle.fsBold]);
  Italic.Create(sisTextDecoration, clNoneF, clNoneF, [TFontStyle.fsItalic]);
  BoldItalic.Create(sisTextDecoration, clNoneF, clNoneF, [TFontStyle.fsBold, TFontStyle.fsItalic]);
  Underline.Create(sisTextDecoration, clNoneF, clNoneF, [TFontStyle.fsUnderline]);
  Link.Create(sisTextDecoration, D2D1ColorF(TColor($E16941)), clNoneF, []); //Royal blue color

  Editor.Indicators.RegisterSpec(IdHeading1, Heading1);
  Editor.Indicators.RegisterSpec(IdHeading2, Heading2);
  Editor.Indicators.RegisterSpec(IdHeading3, Heading3);
  Editor.Indicators.RegisterSpec(IdHeadingOther, HeadingOther);
  Editor.Indicators.RegisterSpec(IdBold, Bold);
  Editor.Indicators.RegisterSpec(IdItalic, Italic);
  Editor.Indicators.RegisterSpec(IdBoldItalic, BoldItalic);
  Editor.Indicators.RegisterSpec(IdUnderline, Underline);
  Editor.Indicators.RegisterSpec(IdLink, Link);
end;

{ TMarkDownRegEx }

function TMarkDownRegEx.New: TMarkDownRegEx;
begin
  Result.Bullets := TRegEx.Create('^(\s*)[\-\*\+] ', [roCompiled]);
  Result.Bullets.Study([preJIT]);
  Result.MergeLines := TRegEx.Create('(\r?\n){3,}', [roCompiled]);
  Result.MergeLines.Study([preJIT]);
  Result.SplitLines := TRegEx.Create('\r?\n', [roCompiled]);
  Result.SplitLines.Study([preJIT]);
  Result.Emphasis := TRegEx.Create('\*\*\*(.*?)\*\*\*|\*\*(.*?)\*\*|\*(.*?)\*', [roCompiled]);
  Result.Emphasis.Study([preJIT]);
  Result.HorzRule := TRegEx.Create('^(\-\-\-|\*\*\*|___)$', [roCompiled]);
  Result.HorzRule.Study([preJIT]);
  Result.Header := TRegEx.Create('^(#{1,6}) .*', [roCompiled]);
  Result.Header.Study([preJIT]);
  Result.SoftLineBreak := TRegEx.Create('(  |\\|<br>)$', [roCompiled]);
  Result.SoftLineBreak.Study([preJIT]);
  Result.Links := TRegEx.Create('\[(.+?)\]\((.+?)\)', [roCompiled]);
  Result.Links.Study([preJIT]);
end;

end.
