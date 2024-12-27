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
              - code tags (backticks)
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
    const IdInlineCode: TGUID = '{ADA75BAF-9AEC-42A3-A328-9F655FEED90D}';
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
    InlineCode: TSynIndicatorSpec;
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
    Backticks: TRegex;
    Bullets: TRegEx;
    NumList: TRegEx;
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
// Process the Markdown input and set the control text
// to the processed input.
// Uses indicators to render much of the Markdown syntax.

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
        begin
          // End of code block -
          AddEmptyLineAfter(LineNo);
          PrevLI := [];
        end;
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
      else if RegExps.NumList.IsMatch(Line) then
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
    - Code tags (backticks)
  }

  type
    TTag = record
      Start, Stop: Integer;
      TagLen: Integer;
      Guid: TGuid;
      Link: string;
    end;

  var
    Tags: TArray<TTag>;

  procedure AdjustTags;
  var
    I, J: Integer;
  begin
    for I := Low(Tags) to High(Tags) do
    begin
      if Tags[I].TagLen = 0 then
        Continue;
      for J := Low(Tags) to High(Tags) do
      begin
        if J <> I then
        begin
          if Tags[J].Start >= Tags[I].Stop then
            Dec(Tags[J].Start, Tags[I].TagLen * 2)
          else if Tags[J].Start > Tags[I].Start then
            Dec(Tags[J].Start, Tags[I].TagLen);
          if Tags[J].Stop >= Tags[I].Stop then
            Dec(Tags[J].Stop, Tags[I].TagLen * 2)
          else if Tags[J].Stop >= Tags[I].Start then
            Dec(Tags[J].Stop, Tags[I].TagLen);
        end;
      end;
      Dec(Tags[I].Stop, 2 * Tags[I].TagLen);
    end;
  end;

  var
    LineNo: Integer;
    HeaderGuid: TGuid;
    Match: TMatch;
    EmphasisMatches: TMatchCollection;
    TickMatches: TMatchCollection;
    Tag: TTag;
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

    Tags := [];
    // Preprocess Links
    Match := RegExps.Links.Match(Line);
    while Match.Success do
    begin
      Tag.Guid := TMarkdownIndicators.IdLink;
      // after the replacement the display string will
      // start at the start of the start of the match
      Tag.Start := Match.Index;
      Tag.Stop := Tag.Start +  Match.Groups[1].Length;
      Tag.Link := Match.Groups[2].Value;
      Tag.TagLen := 0;
      Tags := Tags + [Tag];
      // Keep just the display string
      Line := RegExps.Links.Replace(Line, '$1', 1);
      Match := RegExps.Links.Match(Line);
    end;
    Tag.Link := '';

    // Emphasis
    EmphasisMatches := RegExps.Emphasis.Matches(Line);
    for Match in EmphasisMatches do
    begin
      Tag.Start := Match.Index;
      Tag.Stop := Tag.Start + Match.Length;
      if Match.Groups[1].Length > 0 then
      begin
        Tag.Guid := MarkdownIndicators.IdBoldItalic;
        Tag.TagLen := 3;
      end
      else if Match.Groups[2].Length > 0 then
      begin
        Tag.Guid := MarkdownIndicators.IdBold;
        Tag.TagLen := 2;
      end
      else if Match.Groups[3].Length > 0 then
      begin
        Tag.Guid := MarkdownIndicators.IdItalic;
        Tag.TagLen := 1;
      end;
      Tags := Tags + [Tag];
    end;

    // Backticks
    TickMatches := RegExps.Backticks.Matches(Line);
    for Match in TickMatches do
    begin
      Tag.Guid := MarkdownIndicators.IdInlineCode;
      Tag.Start := Match.Index;
      Tag.Stop := Tag.Start + Match.Length;
      Tag.TagLen := 1;
      Tags := Tags + [Tag];
    end;

    //Add the line without the tags
    if EmphasisMatches.Count > 0 then
      Line := RegExps.Emphasis.Replace(Line, '$1$2$3');
    if TickMatches.Count > 0 then
      Line := RegExps.Backticks.Replace(Line, '$1');
    LineNo := Lines.Add(Line);

    // Process Tags by adding appropriate indicators
    if Length(Tags) >0 then
    begin
      AdjustTags;
      for Tag in Tags do
        Indicators.Add(LineNo + 1, TSynIndicator.New(
          Tag.Guid, Tag.Start, Tag.Stop,
          IfThen(Tag.Guid = TMarkdownIndicators.IdLink,
          FLinks.Add(Tag.Link), 0)));
    end;
  end;

var
  LineArr: TArray<string>;
  Line: string;
  InCodeBlock: Boolean;
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
    InCodeBlock := False;
    for Line in LineArr do
    begin
      if Line.StartsWith('```') then
        InCodeBlock := not InCodeBlock;
      if InCodeBlock then
        Lines.Add(Line)
      else
        ProcessLine(Line);
    end;
  finally
    EndUpdate;
  end;
end;

{ TMarkdownIndicators }

constructor TMarkdownIndicators.Create(Editor: TCustomSynEdit);
begin
  InlineCode.Create(sisFilledRectangle, clNoneF, D2D1ColorF(clGray, 0.4), []);
  Heading1.Create(sisTextDecoration, clNoneF, clNoneF, [TFontStyle.fsBold, TFontStyle.fsItalic]);
  Heading2.Create(sisTextDecoration, clNoneF, clNoneF, [TFontStyle.fsBold, TFontStyle.fsItalic, TFontStyle.fsUnderline]);
  Heading3.Create(sisTextDecoration, clNoneF, clNoneF, [TFontStyle.fsBold, TFontStyle.fsUnderline]);
  HeadingOther.Create(sisTextDecoration, clNoneF, clNoneF, [TFontStyle.fsUnderline]);
  Bold.Create(sisTextDecoration, clNoneF, clNoneF, [TFontStyle.fsBold]);
  Italic.Create(sisTextDecoration, clNoneF, clNoneF, [TFontStyle.fsItalic]);
  BoldItalic.Create(sisTextDecoration, clNoneF, clNoneF, [TFontStyle.fsBold, TFontStyle.fsItalic]);
  Underline.Create(sisTextDecoration, clNoneF, clNoneF, [TFontStyle.fsUnderline]);
  Link.Create(sisTextDecoration, D2D1ColorF(TColor($E16941)), clNoneF, []); //Royal blue color

  Editor.Indicators.RegisterSpec(IdInlineCode, InlineCode);
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
  function NewRegEx(const Pattern: string): TRegEx;
  begin
    Result := TRegEx.Create(Pattern, [roCompiled]);
    {$IF (CompilerVersion >= 35)}
    Result.Study([preJIT]);
    {$ENDIF}
  end;

begin
  Result.Backticks := NewRegEx('`([^`]+?)`');
  Result.Bullets := NewRegEx('^(\s*)[\-\*\+] ');
  Result.NumList := NewRegEx('^(\s*)\d+\. ');
  Result.MergeLines := NewRegEx('(\r?\n){3,}');
  Result.SplitLines := NewRegEx('\r?\n');
  Result.Emphasis := NewRegEx('\*\*\*(.+?)\*\*\*|\*\*(.+?)\*\*|\*(.+?)\*');
  Result.HorzRule := NewRegEx('^(\-\-\-|\*\*\*|___)$');
  Result.Header := NewRegEx('^(#{1,6}) .*');
  Result.SoftLineBreak := NewRegEx('(  |\\|<br>)$');
  Result.Links := NewRegEx('\[(.+?)\]\((.+?)\)');
end;

end.
