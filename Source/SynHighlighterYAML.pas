{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Code template generated with SynGen.
The original code is: C:\Delphi\Components\UniSynEdit\SynGen\yaml.pas, released 2008-06-13.
Description: YAML Syntax Parser/Highlighter
The initial author of this file is Kiriakos.
Copyright (c) 2008, all rights reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

unit SynHighlighterYAML;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  SynEditHighlighter,
  StringResources;

//State constants
Const
  rsUnknown = 0;
  rsValue = 1;
  rsLiteralStart = 2;
  rsLiteral = 3;
  rsString1 = 4;
  rsString2 = 5;
  rsDirective = 6;
  rsDocDelimiter = 7;
type
  TtkTokenKind = (
    tkComment,
    tkDocDelimiter,
    tkKey,
    tkNull,
    tkNumericValue,
    tkSpace,
    tkSymbol,
    tkTag,
    tkLiteral,
    tkString,
    tkAnchor,
    tkDirective,
    tkUnknown);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynYAMLSyn = class(TSynCustomHighlighter)
  private
    fRange: LongWord;
    fTokenID: TtkTokenKind;
    fCommentAttri: TSynHighlighterAttributes;
    fDocDelimiterAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumericValueAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fTagAttri: TSynHighlighterAttributes;
    fTextValueAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fDirectiveAttri: TSynHighlighterAttributes;
    fAnchorAttri: TSynHighlighterAttributes;
    fErrorAttri: TSynHighlighterAttributes;
    fTempSpaceAttri : TSynHighlighterAttributes;
    procedure KeyProc;
    procedure LiteralProc;
    procedure LiteralMarkProc;
    procedure FoldedLiteralMarkProc;
    procedure ValueProc;
    procedure StringProc1;
    procedure StringProc2;
    procedure UnknownProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure CRProc;
    procedure LFProc;
    procedure ListItemProc;
    procedure TagProc;
    procedure CommentProc;
    procedure DirectiveProc;
    procedure AnchorProc;
    procedure DocDelimiterProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
    procedure DoSetLine(const Value: UnicodeString; LineNumber: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
    class function GetLanguageName: string; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property DocDelimiterAttri: TSynHighlighterAttributes read fDocDelimiterAttri write fDocDelimiterAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumericValueAttri: TSynHighlighterAttributes read fNumericValueAttri write fNumericValueAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri write fSymbolAttri;
    property TagAttri: TSynHighlighterAttributes read fTagAttri write fTagAttri;
    property TextValueAttri: TSynHighlighterAttributes read fTextValueAttri write fTextValueAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
    property DirectiveAttri: TSynHighlighterAttributes read fDirectiveAttri write fDirectiveAttri;
    property AnchorAttri: TSynHighlighterAttributes read fAnchorAttri write fAnchorAttri;
    property ErrorAttri: TSynHighlighterAttributes read fErrorAttri write fErrorAttri;
  end;

const
  SYNS_LangYAML = 'YAML';
  SYNS_AttrDocumentDelimiter = 'DocumentDelimiter';
  SYNS_AttrKey = 'Key';
  SYNS_AttrNumericValue = 'NumericValue';
  SYNS_AttrTextValue = 'TextValue';
  SYNS_AttrAnchor = 'Anchor';
  SYNS_AttrTag = 'Tag';
  SYNS_FilterYAML = sYAMLFileFilter;

resourcestring
  SYNS_FriendlyLangYAML = 'YAML';
  SYNS_FriendlyAttrDocumentDelimiter = 'Document Delimiter';
  SYNS_FriendlyAttrKey = 'Key';
  SYNS_FriendlyAttrNumericValue = 'Numeric Value';
  SYNS_FriendlyAttrTextValue = 'Text Value';
  SYNS_FriendlyAttrTag = 'Tag';
  SYNS_FriendlyAttrAnchor = 'Anchor';

implementation

uses
  System.Character,
  System.Math,
  Winapi.Windows,
  SynEditStrConst,
  uCommonFunctions;

procedure TSynYAMLSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynYAMLSyn.StringProc1;
begin
  fTokenID := tkString;
  while not IsLineEnd(Run) do begin
    if FLine[Run] = '"' then begin
      Inc(Run);
      LongRec(fRange).Lo := rsUnknown;
      Exit;
    end else if FLine[Run] = '\' then
      Inc(Run);
    Inc(Run)
  end;
  LongRec(fRange).Lo := rsString1;
end;

procedure TSynYAMLSyn.StringProc2;
begin
  fTokenID := tkString;
  while not IsLineEnd(Run) do begin
    if FLine[Run] = '''' then begin
      Inc(Run);
      if FLine[Run] <> '''' then begin
        LongRec(fRange).Lo := rsUnknown;
        Exit;
      end;
    end;
    Inc(Run)
  end;
  LongRec(fRange).Lo := rsString2;
end;

procedure TSynYAMLSyn.TagProc;
begin
  while not (IsLineEnd(Run) or (FLine[Run] <= #32)) do
    inc(Run);
  fTokenID := tkTag;
end;

procedure TSynYAMLSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynYAMLSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then
    inc(Run);
end;

destructor TSynYAMLSyn.Destroy;
begin
  fTempSpaceAttri.Free;
  inherited;
end;

procedure TSynYAMLSyn.DirectiveProc;
begin
  fTokenID := tkDirective;
  while not IsLineEnd(Run) do
    inc(Run);
  LongRec(fRange).Lo := rsUnknown;
end;

procedure TSynYAMLSyn.DocDelimiterProc;
begin
  fTokenID := tkDocDelimiter;
  while not IsLineEnd(Run) do
    inc(Run);
end;

procedure TSynYAMLSyn.DoSetLine(const Value: UnicodeString; LineNumber: Integer);
Const
  sDocStart : UnicodeString = '---';
  sDocEnd : UnicodeString = '...';
Var
  NewIndent : integer;
begin
  inherited;
  NewIndent := CalcIndent(fLineStr);

  if LongRec(fRange).Lo = rsDocDelimiter then
   LongRec(fRange).Lo := rsUnknown;

  if fLine^ = '%' then begin
    LongRec(fRange).Lo := rsDirective;
    LongRec(fRange).Hi := NewIndent;
  end else if StrIsLeft(FLine, PWideChar(sDocStart)) or StrIsLeft(FLine, PWideChar(sDocEnd)) then begin
    LongRec(fRange).Lo := rsDocDelimiter;
    LongRec(fRange).Hi := NewIndent;
  end else if (LongRec(fRange).Lo = rsLiteralStart) then begin
    LongRec(fRange).Lo := rsLiteral;
    LongRec(fRange).Hi := NewIndent;
  end else if (LongRec(fRange).Lo = rsLiteral) then begin
    if (LongRec(fRange).Hi > NewIndent) then begin
      LongRec(fRange).Lo := rsUnknown;
      LongRec(fRange).Hi := NewIndent;
    end else
      LongRec(fRange).Hi := Min(LongRec(fRange).Hi, NewIndent);
  end else begin
    if not (LongRec(fRange).Lo in [rsString1, rsString2]) then
      LongRec(fRange).Lo := rsUnknown;
    LongRec(fRange).Hi := NewIndent;
  end;
end;

procedure TSynYAMLSyn.FoldedLiteralMarkProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  LongRec(fRange).Lo := rsLiteralStart;
end;

procedure TSynYAMLSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynYAMLSyn.ListItemProc;
begin
  Inc(Run);
  if CharInSet(FLine[Run], [' ', #0, #13, #10]) then begin
    fTokenID := tkSymbol;
    LongRec(fRange).Lo := rsUnknown;
  end else
    Next;
end;

procedure TSynYAMLSyn.LiteralMarkProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] = '+' then
    Inc(Run)
  else
    while CharInSet(FLine[Run], ['0'..'9']) do
      Inc(Run);
  LongRec(fRange).Lo := rsLiteralStart;
end;

procedure TSynYAMLSyn.LiteralProc;
begin
  fTokenID := tkLiteral;
  while (not IsLineEnd(Run)) and (FLine[Run] <> '#') do
    inc(Run);
end;

procedure TSynYAMLSyn.AnchorProc;
begin
  while not (IsLineEnd(Run) or (FLine[Run] <= #32)) do
    inc(Run);
  fTokenID := tkAnchor;
end;

procedure TSynYAMLSyn.CommentProc;
begin
  fTokenID := tkComment;
  while not IsLineEnd(Run) do
    inc(Run);
end;

constructor TSynYAMLSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCaseSensitive := False;

  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Foreground := clGray;
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);

  fDocDelimiterAttri := TSynHighLighterAttributes.Create(SYNS_AttrDocumentDelimiter, SYNS_FriendlyAttrDocumentDelimiter);
  fDocDelimiterAttri.Style := [fsBold] ;
  fDocDelimiterAttri.Foreground := clWhite;
  fDocDelimiterAttri.Background := clPurple;
  AddAttribute(fDocDelimiterAttri);

  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrKey, SYNS_FriendlyAttrKey);
  fKeyAttri.Style := [fsBold];
  fKeyAttri.Foreground := clTeal;
  AddAttribute(fKeyAttri);

  fNumericValueAttri := TSynHighLighterAttributes.Create(SYNS_AttrNumericValue, SYNS_FriendlyAttrNumericValue);
  fNumericValueAttri.Foreground := RGB(88, 0, 0);
  AddAttribute(fNumericValueAttri);

  fSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);

  fSymbolAttri := TSynHighLighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  fSymbolAttri.Style := [fsBold];
  fSymbolAttri.Foreground := $FF8844;
  AddAttribute(fSymbolAttri);

  fTagAttri := TSynHighLighterAttributes.Create(SYNS_AttrTag, SYNS_FriendlyAttrTag);
  fTagAttri.Foreground := clNavy;
  fTagAttri.Style := [fsBold];
  AddAttribute(fTagAttri);

  fTextValueAttri := TSynHighLighterAttributes.Create(SYNS_AttrTextValue, SYNS_FriendlyAttrTextValue);
  AddAttribute(fTextValueAttri);

  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  fStringAttri.Foreground := $FF8844;
  AddAttribute(fStringAttri);

  fDirectiveAttri := TSynHighlighterAttributes.Create(SYNS_AttrDirective, SYNS_FriendlyAttrDirective);
  fDirectiveAttri.Foreground := clGreen;
  AddAttribute(fDirectiveAttri);

  fAnchorAttri := TSynHighlighterAttributes.Create(SYNS_AttrAnchor, SYNS_FriendlyAttrAnchor);
  fAnchorAttri.Foreground := clMaroon;
  fAnchorAttri.Style := [fsBold];
  AddAttribute(fAnchorAttri);

  fErrorAttri := TSynHighlighterAttributes.Create(SYNS_AttrSyntaxError, SYNS_FriendlyAttrSyntaxError);
  fErrorAttri.Foreground := clRed;
  AddAttribute(fErrorAttri);

  SetAttributesOnChange(DefHighlightChange);
  fDefaultFilter := SYNS_FilterYAML;
  LongRec(fRange).Lo := rsUnknown;

  // for coloring doc comment background
  fTempSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
end;

procedure TSynYAMLSyn.KeyProc;
begin
  fTokenID := tkLiteral;
  LongRec(fRange).Lo := rsValue;
  while not IsLineEnd(Run) do begin
    if (FLine[Run] = ':') and (CharInSet(FLine[Run+1], [' ', #0, #13, #10])) then begin
      Inc(Run);
      if FLine[Run] = ' ' then
        Inc(Run);
      fTokenID := tkKey;
      break;
    end else
      Inc(Run);
  end;
end;

procedure TSynYAMLSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynYAMLSyn.ValueProc;
Var
  Start : Integer;
  Val : UnicodeString;
  FloatVal : Extended;
begin
  Start := Run;
  fTokenID := tkLiteral;
  while (not IsLineEnd(Run)) and (FLine[Run] <> '#') do
    inc(Run);
  Val := Copy(FLineStr, Start, Run - Start + 1);
  if TryStrToFloat(Trim(Val), FloatVal) then
    fTokenId := tkNumericValue;
end;

procedure TSynYAMLSyn.Next;
begin
  fTokenPos := Run;
  case LongRec(fRange).Lo of
    rsDirective : DirectiveProc;
    rsString1 :
      case fLine[Run] of
        #0: NullProc;
        #10: LFProc;
        #13: CRProc;
      else
        StringProc1;
      end;
    rsString2 :
      case fLine[Run] of
        #0: NullProc;
        #10: LFProc;
        #13: CRProc;
      else
        StringProc2;
      end;
    rsDocDelimiter :
      case fLine[Run] of
        #0: NullProc;
        #10: LFProc;
        #13: CRProc;
      else
        DocDelimiterProc;
      end;
    rsLiteralStart :
      case fLine[Run] of
        #0: NullProc;
        #10: LFProc;
        #13: CRProc;
        '#': CommentProc;
      else
        UnknownProc;
      end;
    rsLiteral :
      case fLine[Run] of
        #0: NullProc;
        #10: LFProc;
        #13: CRProc;
        '#': CommentProc;
      else
        LiteralProc;
      end;
    rsValue :
      case fLine[Run] of
        #0: NullProc;
        #10: LFProc;
        #13: CRProc;
        '#': CommentProc;
        '|': LiteralMarkProc;
        '>': FoldedLiteralMarkProc;
        '-': ListItemProc;
        '&', '*' : AnchorProc;
        '"':
          begin
            Inc(Run);
            StringProc1;
          end;
        '''':
          begin
            Inc(Run);
            StringProc2;
          end;
        #1..#9, #11, #12, #14..#32: SpaceProc;
      else
        ValueProc;
      end;
  else
    case fLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      '#': CommentProc;
      '-': ListItemProc;
      '!': TagProc;
      '"':
        begin
          Inc(Run);
          StringProc1;
        end;
      '''':
        begin
          Inc(Run);
          StringProc2;
        end;
      #1..#9, #11, #12, #14..#32: SpaceProc;
    else
      KeyProc;
    end;
  end;
  inherited;
end;

function TSynYAMLSyn.GetDefaultAttribute(Index: Integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_WHITESPACE: 
      begin
        fTempSpaceAttri.Assign(fSpaceAttri);
        if LongRec(fRange).Lo = rsDocDelimiter then
          fTempSpaceAttri.Background := fDocDelimiterAttri.Background;
        Result := fTempSpaceAttri;
      end;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynYAMLSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynYAMLSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynYAMLSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkDocDelimiter: Result := fDocDelimiterAttri;
    tkKey: Result := fKeyAttri;
    tkNumericValue: Result := fNumericValueAttri;
    tkSpace: Result := fSpaceAttri;
    tkSymbol: Result := fSymbolAttri;
    tkTag: Result := fTagAttri;
    tkLiteral: Result := fTextValueAttri;
    tkString: Result := fStringAttri;
    tkDirective: Result := fDirectiveAttri;
    tkAnchor: Result := fAnchorAttri;
    tkUnknown: Result := fErrorAttri;
  else
    Result := nil;
  end;
end;

function TSynYAMLSyn.GetTokenKind: Integer;
begin
  Result := Ord(fTokenId);
end;

function TSynYAMLSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  Result := AChar.IsLetterOrDigit or (AChar = '_');
end;

function TSynYAMLSyn.GetSampleSource: UnicodeString;
begin
  Result :=
    '---'#13#10 +
    'receipt:    Oz-Ware Purchase Invoice'#13#10 +
    'date:        2007-08-06'#13#10 +
    'customer:'#13#10 +
    '    given:   Dorothy'#13#10 +
    '    family:  Gale'#13#10 +
    #13#10 +
    'items:'#13#10 +
    '    - part_no:   A4786'#13#10 +
    '      descrip:   !!str Water Bucket (Filled)'#13#10 +
    '      price:     1.47'#13#10 +
    '      quantity:  4'#13#10 +
    #13#10 +
    '    - part_no:   E1628'#13#10 +
    '      descrip:   "High Heeled \"Ruby\" Slippers"'#13#10 +
    '      price:     100.27'#13#10 +
    '      quantity:  1'#13#10 +
    #13#10 +
    'bill-to:  &id001'#13#10 +
    '    street: |'#13#10 +
    '            123 Tornado Alley'#13#10 +
    '            Suite 16'#13#10 +
    '    city:   East Westville'#13#10 +
    '    state:  KS'#13#10 +
    #13#10 +
    'ship-to:  *id001'#13#10 +
    #13#10 +
    'specialDelivery:  >'#13#10 +
    '    Follow the Yellow Brick'#13#10 +
    '    Road to the Emerald City.'#13#10 +
    '    Pay no attention to the'#13#10 +
    '    man behind the curtain.'#13#10 +
    '...';
end;

function TSynYAMLSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterYAML;
end;

class function TSynYAMLSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangYAML;
end;

class function TSynYAMLSyn.GetLanguageName: string;
begin
  Result := SYNS_LangYAML;
end;

procedure TSynYAMLSyn.ResetRange;
begin
  LongRec(fRange).Lo := rsUnknown;
end;

procedure TSynYAMLSyn.SetRange(Value: Pointer);
begin
  fRange := LongWord(Value);
end;

function TSynYAMLSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

initialization
  RegisterPlaceableHighlighter(TSynYAMLSyn);
end.
