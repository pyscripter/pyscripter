{-------------------------------------------------------------------------------
SynWeb
Copyright (C) 2005-2011  Krystian Bigaj

*** MPL
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is Krystian Bigaj.

Alternatively, the contents of this file may be used under the terms
of the GNU Lesser General Public license (the  "LGPL License"),
in which case the provisions of LGPL License are applicable instead of those
above. If you wish to allow use of your version of this file only
under the terms of the LGPL License and not to allow others to use
your version of this file under the MPL, indicate your decision by
deleting the provisions above and replace them with the notice and
other provisions required by the LGPL License. If you do not delete
the provisions above, a recipient may use your version of this file
under either the MPL or the LGPL License.

*** LGPL
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
***

You may retrieve the latest version of this file at the SynWeb home page,
located at http://sourceforge.net/projects/synweb

Contact: krystian.bigaj@gmail.com
Homepage: http://flatdev.ovh.org
-------------------------------------------------------------------------------}

{$IFNDEF QSYNHIGHLIGHTERWEBMISC}
unit SynHighlighterWebMisc;
{$ENDIF}

{$I SynWeb.inc}

interface

uses
  SysUtils,
  StrUtils,
  Graphics,
  Classes,
  Types,
  Windows,
{$IFDEF SYN_CLX}
  QSynEdit,
{$IFDEF UNISYNEDIT}
  QSynUnicode,
{$ENDIF}
  QSynEditTextBuffer,
  QSynEditHighlighter,
  QSynEditTypes,
  QSynCompletionProposal,
  QSynHighlighterWeb,
  QSynHighlighterWebData,
  QSynTokenMatch;
{$ELSE}
  SynEdit,
{$IFDEF UNISYNEDIT}
  SynUnicode,
{$ENDIF}
  SynEditTextBuffer,
  SynEditHighlighter,
  SynEditTypes,
  SynCompletionProposal,
  SynHighlighterWeb,
  SynHighlighterWebData,
  SynTokenMatch;
{$ENDIF}

const
  TSynWebTokenizedStartScanBefore = 20; { 20 lines before }

  TBufferCoordStart: TBufferCoord = (Char: 1; Line: 1);
  TSynWebTokenizerPhpWhitespaces = [stkPhpSpace, stkPhpComment, stkPhpDocComment, stkPhpDocCommentTag, stkNull];

type

{ TSynWebWordMarkerCustom }

  TSynWebWordMarkerMode = (swwmSelectedText, swwmSelectedWord,
    swwmCustomText, swwmCustomWord);

  TSynWebWordMarkerCustom = class(TSynEditPlugin)
  private  
    FIsWordSelected: Boolean;
    FIsMultiLineSelection: Boolean;
    FEnabled: Boolean;   
    FMode: TSynWebWordMarkerMode; 
    FCustomText: TSynWebString;
    FCaseSensitive: Boolean;

  protected
    function IsWordSelected: Boolean;
    function IsOverlapSelection(const ABufferStart, ABufferEnd: TBufferCoord): Boolean;

    function GetHighlightText: TSynWebString;

    procedure SetEnabled(const Value: Boolean);
    procedure SetMode(const Value: TSynWebWordMarkerMode);
    procedure SetCustomText(const Value: TSynWebString);
    procedure SetCaseSensitive(const Value: Boolean);

    procedure DoInvalidate;   

    procedure LinesInserted(FirstLine: Integer; Count: Integer); override;
    procedure LinesDeleted(FirstLine: Integer; Count: Integer); override;
    procedure AfterPaint(ACanvas: TCanvas; const AClip: TRect;
      FirstLine: Integer; LastLine: Integer); override;

    procedure DoPaintMarker(ACanvas: TCanvas; const ARect: TRect;
      const AText: TSynWebString; const ABufferStart, ABufferEnd: TBufferCoord); virtual; abstract;

  public               
    procedure AfterConstruction; override;

    procedure NotifySelChanged;

    property Enabled: Boolean read FEnabled write SetEnabled;  
    property Mode: TSynWebWordMarkerMode read FMode write SetMode;  
    property CustomText: TSynWebString read FCustomText write SetCustomText;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
  end;

{ TSynWebWordMarker }

  TSynWebWordMarkerPaintMode = (swwpFillRect, swwpFrameRect, swwpUnderline);

  TSynWebWordMarker = class(TSynWebWordMarkerCustom)
  protected
    FBGColor: TColor;
    FFGColor: TColor;
    FPaintMode: TSynWebWordMarkerPaintMode;

    procedure SetBGColor(const Value: TColor);   
    procedure SetFGColor(const Value: TColor);
    procedure SetPaintMode(const Value: TSynWebWordMarkerPaintMode);

    procedure DoPaintMarker(ACanvas: TCanvas; const ARect: TRect;
      const AText: TSynWebString; const ABufferStart, ABufferEnd: TBufferCoord); override;

  public        
    procedure AfterConstruction; override;

    property BGColor: TColor read FBGColor write SetBGColor;
    property FGColor: TColor read FFGColor write SetFGColor;
    property PaintMode: TSynWebWordMarkerPaintMode read FPaintMode write SetPaintMode;
  end;

{ TSynWebTokenizerInfo }

  TSynWebTokenizerSet = set of TSynWebTokenKind;
  TSynWebTokenizerMarker = (stlBefore, stlBegin, stlInside, stlEnd, stlAfter);

  TSynWebTokenizerInfo = record
    Kind: TSynWebTokenKind;
    Token: TSynWebString;
    Len: Integer;
    Marker: TSynWebTokenizerMarker;
    Range: Longword;
    PhpKeyId: Integer;
    PhpSymbolId: Integer;
    EsSymbolId: Integer;
    MLTagId: Integer;
    MLTagKind: Integer;
    case Boolean of
    False: (
      Char: Integer;
      Line: Integer;
    );

    True: (
      Pos: TBufferCoord;
    )
    end;

{ ISynWebTokenizer }

  ISynWebTokenizer = interface(IInterface)
  // protected
    function GetMarkerPos: TBufferCoord;
    procedure SetMarkerPos(const AMarkerPos: TBufferCoord);

  // public
    function Next: Boolean;
    function NextAndSkip(const ASkip: TSynWebTokenizerSet = TSynWebTokenizerPhpWhitespaces): Boolean;
    function Skip(const ASkip: TSynWebTokenizerSet = TSynWebTokenizerPhpWhitespaces): Boolean;

    function IsEol: Boolean;
    function IsEof: Boolean;

    function HL: TSynWebBase;

    function PreviousToken: TSynWebTokenizerInfo;
    function CurrentToken: TSynWebTokenizerInfo;

    property MarkerPos: TBufferCoord read GetMarkerPos write SetMarkerPos;
  end;

{ TSynWebTokenizer }

  TSynWebTokenizer = class(TInterfacedObject, ISynWebTokenizer)
  protected
    FLines: TSynWebStrings;
    FLine: Integer;
    FMarkerPos: TBufferCoord;
    FHL: TSynWebBase;

    FCurrentToken: TSynWebTokenizerInfo;
    FLastToken: TSynWebTokenizerInfo;

    procedure UpdateMaker(var ATokenInfo: TSynWebTokenizerInfo);
    procedure GetCurrentTokenInfo(var ATokenInfo: TSynWebTokenizerInfo);

    function GetMarkerPos: TBufferCoord;
    procedure SetMarkerPos(const AMarkerPos: TBufferCoord);
  public
    constructor Create(ALines: TSynWebStrings; AHighlighter: TSynWebBase;
      APos: TBufferCoord; ACustomRange: Pointer = nil); overload;
    constructor Create(ALines: TSynWebStrings; AHighlighter: TSynWebBase;
      ALine: Integer = 1); overload;

    constructor Create(AEditor: TCustomSynEdit; ALine: Integer = 1); overload;
    constructor Create(AEditor: TCustomSynEdit; APos: TBufferCoord); overload;

    class function GetTokenCompletionAtCaret(AEditor: TCustomSynEdit): TSynWebString;

    function Next: Boolean;
    function NextAndSkip(const ASkip: TSynWebTokenizerSet): Boolean;
    function Skip(const ASkip: TSynWebTokenizerSet): Boolean;

    function IsEof: Boolean;
    function IsEol: Boolean;  

    function HL: TSynWebBase;
    function CurrentToken: TSynWebTokenizerInfo;
    function PreviousToken: TSynWebTokenizerInfo;
  end;

{ TSynWebSimpleTokenizer }

  TSynWebSimpleTokenizer = class(TObject)
  protected
    FHL: TSynWebBase;
    FOwnHL: Boolean;
    FLines: TSynWebStrings;
    FLine: Integer;
  public
    constructor Create(AHighlighter: TSynWebBase; ALines: TSynWebStrings;
      AOwnHighlighter: Boolean = False; ALine: Integer = 0; ARange: Pointer = nil);
    destructor Destroy; override;

    procedure Next;
    function IsEof: Boolean;
    function IsEol: Boolean;

    property HL: TSynWebBase read FHL;
    property Line: Integer read FLine;
  end;

//

{
  SynEditGetMatchingToken(Ex) returns:
  -2 : Close and open token found
  -1 : Close token found
   0 : Kind not found
  +1 : Open token found
  +2 : Open and close token found
}

function SynEditGetMatchingTag(ASynEdit: TCustomSynEdit; APoint: TBufferCoord;
  var AMatch: TSynTokenMatched): Integer;

function SynEditGetMatchingTagEx(ASynEdit: TCustomSynEdit; APoint: TBufferCoord;
  var AMatch: TSynTokenMatched): Integer;

function SynWebGetHighlighterTypeAt(ASynEdit: TCustomSynEdit;
  ASynWeb: TSynWebBase; APos: TBufferCoord): TSynWebHighlighterTypes;

function SynWebGetHighlighterTypeAtCursor(ASynEdit: TCustomSynEdit;
  ASynWeb: TSynWebBase): TSynWebHighlighterTypes;

function SynWebUpdateActiveHighlighter(ASynEdit: TCustomSynEdit;
  ASynWeb: TSynWebBase): TSynWebHighlighterTypes;

function SynWebFillCompletionProposal(ASynEdit: TCustomSynEdit;
  ASynWeb: TSynWebBase; ACompletion: TSynCompletionProposal;
  var CurrentInput: TSynWebString): TSynWebHighlighterTypes;

type
  PSynWebUnmatchedTag = ^TSynWebUnmatchedTag;
  TSynWebUnmatchedTag = record
    Pos: TBufferCoord;
    Tag: TSynWebString;
  end;

  TSynWebUnmatchedTagArray = array of TSynWebUnmatchedTag;

function SynWebGetUnmatchedTags(ASynEdit: TCustomSynEdit;
  var AUnmatchedTags: TSynWebUnmatchedTagArray; AAllowDuplicates: Boolean): Boolean; overload;

function SynWebGetUnmatchedTags(ALines: TSynWebStrings; APos: TBufferCoord;
  ASynWeb: TSynWebMLSyn; var AUnmatchedTags: TSynWebUnmatchedTagArray; AAllowDuplicates: Boolean): Boolean; overload;

implementation

type
  TSynTokenBuf = record
    Pos: TBufferCoord;
    Token: TSynWebString;
  end;

var
  FMatchStack: array of TSynTokenBuf;

function SynEditGetMatchingTag(ASynEdit: TCustomSynEdit; APoint: TBufferCoord;
  var AMatch: TSynTokenMatched): Integer;
var
  TagID: Integer;
  Level, DeltaLevel, FMatchStackID: Integer;
  H: TSynWebMLSyn;
  bSpecial: Boolean;

  function ScanToEndOfSpecialTag: Boolean;
  begin
    Result := False;
    with ASynEdit, H do
    begin
      Next;
      while True do
      begin
        while not GetEol do
        begin
          if (GetTokenID = stkMLError) and (GetToken = '/') then
          begin
            Next;
            if GetEol then
              Break;
            if (GetTokenID = stkMLTag) and (GetToken = '>') then
            begin
              Result := True;
              Exit;
            end;
          end else
            if (GetTokenID in [stkMLTag, stkMLError]) and (GetToken = '>') then
            begin
              Next;
              if MLGetTagID = -1 then
                Exit;
              Continue;
            end;
          Next;
        end;
        Inc(APoint.Line);
        if APoint.Line >= Lines.Count then
          Break;
        SetLine(Lines[APoint.Line], APoint.Line);
      end;
    end;
  end;

  function CheckToken: Boolean;
  begin
    with H do
    begin
      if (GetTokenId = stkMLTagName) and (TagID = MLGetTagID) then
        Inc(Level, MLGetTagKind);
      if Level = 0 then
      begin
        SynEditGetMatchingTag := 2;
        AMatch.CloseToken := GetToken;
        AMatch.CloseTokenPos.Line := APoint.Line + 1;
        AMatch.CloseTokenPos.Char := GetTokenPos + 1;
        Result := True;
      end else
      begin
        Next;
        Result := False;
      end;
    end;
  end;

  procedure CheckTokenBack;
  var
    OldLine: Integer;
  begin
    with H do
    begin
      if (GetTokenId = stkMLTagName) and (TagID = MLGetTagID) then
        case MLGetTagKind of
        -1:
          begin
            Dec(Level);
            if FMatchStackID >= 0 then
              Dec(FMatchStackID);
          end;
        1:
          begin
            Inc(FMatchStackID);
            if FMatchStackID >= Length(FMatchStack) then
              SetLength(FMatchStack, Length(FMatchStack) + 32);
            FMatchStack[FMatchStackID].Token := GetToken;
            FMatchStack[FMatchStackID].Pos.Line := APoint.Line + 1;
            FMatchStack[FMatchStackID].Pos.Char := GetTokenPos + 1;
            if bSpecial then
            begin
              OldLine := APoint.Line;
              if ScanToEndOfSpecialTag then
              begin
                Dec(FMatchStackID);
                if OldLine <> APoint.Line then
                begin
                  APoint.Line := OldLine;
                  while not GetEol do
                    Next;
                end else
                  Next;
                Exit;
              end;
              if OldLine <> APoint.Line then
              begin
                APoint.Line := OldLine;
                while not GetEol do
                  Next;
                Exit;
              end;
            end;
            Inc(Level);
          end;
        end;
      Next;
    end;
  end;

begin
  Result := 0;
  if not (ASynEdit.Highlighter is TSynWebMLSyn) then
    Exit;
  H := TSynWebMLSyn(ASynEdit.Highlighter);
  with ASynEdit, H do
  begin
    if Engine = nil then
      Exit;
    Dec(APoint.Line);
    Dec(APoint.Char);
    if APoint.Line = 0 then
      ResetRange
    else
      SetRange(TSynEditStringList(Lines).Ranges[APoint.Line - 1]);
    SetLine(Lines[APoint.Line], APoint.Line);
    while not GetEol and (APoint.Char >= GetTokenPos + Length(GetToken)) do
      Next;
    TagID := MLGetTagID;
    if GetEol or (TagID = -1) or (GetTokenID <> stkMLTagName) or
      (TSynWeb_TagsData[TagID] and (1 shl 31) <> 0) then
      Exit;
    bSpecial := TagID in [MLTagID_Script, MLTagID_Style];
    case MLGetTagKind of
    1:
      begin
        Result := 1;
        AMatch.OpenToken := GetToken;
        AMatch.OpenTokenPos.Line := APoint.Line + 1;
        AMatch.OpenTokenPos.Char := GetTokenPos + 1;
      end;
    -1:
      begin
        Result := -1;
        AMatch.CloseToken := GetToken;
        AMatch.CloseTokenPos.Line := APoint.Line + 1;
        AMatch.CloseTokenPos.Char := GetTokenPos + 1;
      end;
    end;
    AMatch.TokenKind := GetTokenKind;
    AMatch.TokenAttri := GetTokenAttribute;
    if Result = 1 then
    begin
      if bSpecial and ScanToEndOfSpecialTag then
      begin
        Result := 0;
        Exit;
      end;
      Level := 1;
      Next;
      while True do
      begin
        while not GetEol do
          if CheckToken then
            Exit;
        Inc(APoint.Line);
        if APoint.Line >= ASynEdit.Lines.Count then
          Break;
        SetLine(Lines[APoint.Line], APoint.Line);
      end;
    end else
    begin
      if Length(FMatchStack) < 32 then
        SetLength(FMatchStack, 32);
      FMatchStackID := -1;
      Level := -1;
      if APoint.Line = 0 then
        ResetRange
      else
        SetRange(TSynEditStringList(Lines).Ranges[APoint.Line - 1]);
      SetLine(Lines[APoint.Line], APoint.Line);
      while not GetEol and (GetTokenPos < AMatch.CloseTokenPos.Char - 1) do
        CheckTokenBack;
      if FMatchStackID > -1 then
      begin
        Result := -2;
        AMatch.OpenToken := FMatchStack[FMatchStackID].Token;
        AMatch.OpenTokenPos := FMatchStack[FMatchStackID].Pos;
      end else
        while APoint.Line > 0 do
        begin
          DeltaLevel := -Level - 1;
          Dec(APoint.Line);
          if APoint.Line = 0 then
            ResetRange
          else
            SetRange(TSynEditStringList(Lines).Ranges[APoint.Line - 1]);
          SetLine(Lines[APoint.Line], APoint.Line);
          FMatchStackID := -1;
          while not GetEol do
            CheckTokenBack;
          if (DeltaLevel <= FMatchStackID) then
          begin
            Result := -2;
            AMatch.OpenToken := FMatchStack[FMatchStackID - DeltaLevel].Token;
            AMatch.OpenTokenPos := FMatchStack[FMatchStackID - DeltaLevel].Pos;
            Exit;
          end;
        end;
    end;
  end;
end;

function SynEditGetMatchingTagEx(ASynEdit: TCustomSynEdit; APoint: TBufferCoord;
  var AMatch: TSynTokenMatched): Integer;
begin
  Result := SynEditGetMatchingTag(ASynEdit, APoint, AMatch);
  if (Result = 0) and (APoint.Char > 1) then
  begin
    Dec(APoint.Char);
    Result := SynEditGetMatchingTag(ASynEdit, APoint, AMatch);
  end;
end;

function SynWebGetHighlighterTypeAt(ASynEdit: TCustomSynEdit;
  ASynWeb: TSynWebBase; APos: TBufferCoord): TSynWebHighlighterTypes;
begin
  with ASynEdit,ASynWeb do
  begin
    if APos.Line = 1 then
      ResetRange
    else
      SetRange(TSynEditStringList(Lines).Ranges[CaretY-2]);
    Result := GetActiveHighlighter(GetRange, Lines[CaretY-1], APos.Char, APos.Line);
  end;
end;

function SynWebGetHighlighterTypeAtCursor(ASynEdit: TCustomSynEdit;
  ASynWeb: TSynWebBase): TSynWebHighlighterTypes;
begin
  Result := SynWebGetHighlighterTypeAt(ASynEdit, ASynWeb, ASynEdit.CaretXY);
end;

function SynWebUpdateActiveHighlighter(ASynEdit: TCustomSynEdit;
  ASynWeb: TSynWebBase): TSynWebHighlighterTypes;
begin
  with ASynEdit,ASynWeb do
  begin
    Result := SynWebGetHighlighterTypeAtCursor(ASynEdit, ASynWeb);
    if Result <> ActiveHighlighters then
      if ActiveHighlighterSwitch then
      begin
        ActiveHighlighters := Result;
        Repaint;
      end;
  end;
end;

function SynWebFillCompletionProposal(ASynEdit: TCustomSynEdit;
  ASynWeb: TSynWebBase; ACompletion: TSynCompletionProposal;
  var CurrentInput: TSynWebString): TSynWebHighlighterTypes;
var
  ct: TSynWebString;
  ctk: TSynWebTokenKind;

  procedure ScanTo(APos: TBufferCoord);
  begin
    Dec(APos.Line);
    Dec(APos.Char);
    with ASynEdit, ASynWeb do
    begin
      if APos.Line = 0 then
        ResetRange
      else
        SetRange(TSynEditStringList(Lines).Ranges[APos.Line - 1]);

      SetLine(Lines[APos.Line], APos.Line + 1);

      while not GetEol and (GetTokenPos + GetTokenLen < APos.Char) do
        Next;
    end;
  end;

  procedure AddSimple(AInsert: String; AKind: String = ''; AKindColor: TColor = clWindowText);
  var
    c: String;
  begin
    c := ColorToString(AKindColor);
    if AKind <> '' then
      ACompletion.ItemList.Add(Format('\color{%s}%s\column{}\color{clWindowText}\style{+B}%s',[c, AKind, AInsert]))
    else
      ACompletion.ItemList.Add(AInsert);
    ACompletion.InsertList.Add(AInsert);
  end;

  function HtmlGetTagKind(AID: Integer): Integer;
  begin
    if TSynWeb_TagsData[AID] and
      (1 shl 31) = 0 then
      Result := 1
    else
      if ASynWeb.InternalInstanceData.FOptions.FXmlMode then
        Result := 0
      else
        Result := -1;
  end;

  procedure HtmlLoadTags(AClose: Boolean);
  var
    i: Integer;
    ver: Longword;
    lUnmatchedTag: TSynWebUnmatchedTagArray;
  begin
    if not AClose then
    begin
      if ASynWeb is TSynWebMLSyn then
        if SynWebGetUnmatchedTags(ASynEdit.Lines, ASynEdit.CaretXY, TSynWebMLSyn(ASynWeb), lUnmatchedTag, True) then
          for i := 0 to Length(lUnmatchedTag) - 1 do
            AddSimple('</' + String(lUnmatchedTag[i].Tag) + '>', 'tag*',
              ASynWeb.Engine.MLTagNameAttri.Foreground);
    end;

    ver := 1 shl Longword(ASynWeb.InternalInstanceData.FOptions.FMLVersion);
    for i := 0 to High(TSynWeb_TagsData) do
      if ((TSynWeb_TagsData[i] and ver) <> 0) and (not AClose or
        (AClose and (TSynWeb_TagsData[i] and (1 shl 31) = 0))) and
        (TSynWeb_TagsData[i] and (1 shl 29) = 0) then
      begin
        if AClose then
          AddSimple('</' + String(TSynWeb_Tags[i]) + '>', 'tag',
            ASynWeb.Engine.MLTagNameAttri.Foreground)
        else
          case HtmlGetTagKind(i) of
          -1:
            AddSimple('<' + String(TSynWeb_Tags[i]) + '>', 'tag',
              ASynWeb.Engine.MLTagNameAttri.Foreground);
          0:
            AddSimple('<' + String(TSynWeb_Tags[i]) + ' />', 'tag',
              ASynWeb.Engine.MLTagNameAttri.Foreground);
          1:
            AddSimple('<' + String(TSynWeb_Tags[i]) + '></' + String(TSynWeb_Tags[i]) + '>', 'tag',
               ASynWeb.Engine.MLTagNameAttri.Foreground);
          end;
      end;
  end;

  procedure HtmlLoadSpecailEntity;
  var
    i: Integer;
    ver: Longword;
  begin
    ver := 1 shl Longword(ASynWeb.InternalInstanceData.FOptions.FMLVersion);
    for i := 0 to High(TSynWeb_SpecialData) do
      if (TSynWeb_SpecialData[i] and ver) <> 0 then
        AddSimple('&' + String(TSynWeb_Special[i]) + ';', 'entity', ASynWeb.Engine.MLEscapeAttri.Foreground);
  end;

  procedure HtmlLoad;
  begin
    HtmlLoadTags(False);
    HtmlLoadTags(True);
    HtmlLoadSpecailEntity;
    AddSimple('<?php ; ?>', 'template');
    AddSimple('<?= ; ?>', 'template');
    AddSimple('<script language="php"></script>', 'template');
    AddSimple('<script language="javascript" type="text/javascript"></script>', 'template');
    AddSimple('<style type="text/css"></style>', 'template');
    AddSimple('<!-- -->', 'template');
  end;

  procedure HtmlLoadAttrs(ATag: Integer);
  var
    i: Integer;
    ver, ver2: Longword;
  begin
    if ATag = -1 then
      Exit;

    ver := Longword(ASynWeb.InternalInstanceData.FOptions.FMLVersion);
    ver2 := 1 shl (ATag mod 32);
    ATag := ATag div 32;

    for i := 0 to High(TSynWeb_AttrsData) do
      if (TSynWeb_AttrsData[i][ver][ATag] and ver2) <> 0 then
        AddSimple(String(TSynWeb_Attrs[i]) + '=""', 'attribute', ASynWeb.Engine.MLTagAttri.Foreground);
  end;

  procedure Html;
  begin
    with ASynEdit, ASynWeb do
    begin

      case ctk of
      stkMLTagName, stkMLTagNameUndef:
        begin
          HtmlLoad;
          case MLGetTagKind of
          -1:
            CurrentInput := '</' + Copy(ct, 1, CaretX - 1 - GetTokenPos);
          1:
            CurrentInput := '<' + Copy(ct, 1, CaretX - 1 - GetTokenPos);
          end;
        end;
      stkMLTag:
        begin
          HtmlLoad;
          case MLGetTagKind of
          -1:
            CurrentInput := '</';
          1:
            CurrentInput := '<';
          end;
        end;
      stkMLTagKey, stkMLTagKeyUndef:
        begin
          HtmlLoadAttrs(MLGetTagID);
          CurrentInput := Copy(ct, 1, CaretX - 1 - GetTokenPos);
        end;
      else // case
        case MLGetRange of
        srsMLText:
          HtmlLoad;
        srsMLTagKey, srsMLTagKeyEq:
          HtmlLoadAttrs(MLGetTagID);
        end;
      end;
    end;
  end;

  procedure CssLoadTags;
  var
    i: Integer;
    ver: Longword;
  begin
    ver := 1 shl Longword(ASynWeb.InternalInstanceData.FOptions.FMLVersion);

    for i := 0 to High(TSynWeb_TagsData) do
      if ((TSynWeb_TagsData[i] and ver) <> 0) and
        (TSynWeb_TagsData[i] and (1 shl 30) = 0) and
        (TSynWeb_TagsData[i] and (1 shl 29) = 0) then
        AddSimple(String(TSynWeb_Tags[i]), 'tag', ASynWeb.Engine.MLTagNameAttri.Foreground);
  end;

  procedure CssLoadProp;
  var
    i: Integer;
    ver: Longword;
  begin
    ver := 1 shl Longword(ASynWeb.InternalInstanceData.FOptions.FCssVersion);

    for i := 0 to High(TSynWeb_CssPropsData) do
      if (TSynWeb_CssPropsData[i] and ver) <> 0  then
        AddSimple(String(TSynWeb_CssProps[i]) + ':', 'property', ASynWeb.Engine.CssPropAttri.Foreground);
  end;

  procedure CssLoadVal(AProp: Integer);
  var
    i: Integer;
    ver, prop: Longword;
  begin
    if AProp = -1 then
      Exit;

    ver := Longword(ASynWeb.InternalInstanceData.FOptions.FCssVersion);
    prop := 1 shl (AProp mod 32);
    AProp := AProp div 32;

    for i := 0 to High(TSynWeb_CssValsData) do
      if (TSynWeb_CssValsData[i][ver][AProp] and prop) <> 0 then
        AddSimple(String(TSynWeb_CssVals[i]), 'value', ASynWeb.Engine.CssValAttri.Foreground);
  end;

  procedure Css;
  begin
    with ASynEdit, ASynWeb do
      case ctk of
      stkCssSelector, stkCssSelectorUndef:
        begin
          CssLoadTags;
          CurrentInput := Copy(ct, 1, CaretX - 1 - GetTokenPos);
        end;
      stkCssProp, stkCssPropUndef:
        begin
          CssLoadProp;
          CurrentInput := Copy(ct, 1, CaretX - 1 - GetTokenPos);
        end;
      stkCssVal, stkCssValUndef:
        begin
          CssLoadVal(CssGetPropertyId);
          CurrentInput := Copy(ct, 1, CaretX - 1 - GetTokenPos);
        end
      else // case
        case CssGetRange of
        srsCssRuleset:
          begin
            CssLoadTags;
            CurrentInput := '';
          end;
        srsCssProp:
          begin
            CssLoadProp;
            CurrentInput := '';
          end;
        srsCssPropVal:
          begin
            CssLoadVal(CssGetPropertyId);
            CurrentInput := '';
          end;
        end;
      end;
  end;

  procedure Php;
  var
    i: Integer;
    v: Longword;
    data: Longword;
  begin
    v := 1 shl Longword(ASynWeb.InternalInstanceData.FOptions.FPhpVersion);

    for i := 0 to High(TSynWeb_PhpKeywords) do // functions
    begin
      data := TSynWeb_PhpKeywordsData[i];
      if (Data and $0F = $08) and ((Data shr 16) and v <> 0) then
        AddSimple(String(TSynWeb_PhpKeywords[i]) + '()', 'function', ASynWeb.Engine.PhpFunctionAttri.Foreground);
    end;

    for i := 0 to High(TSynWeb_PhpKeywords) do // keywords
    begin
      data := TSynWeb_PhpKeywordsData[i];
      if (Data and $0F = $01) and ((Data shr 16) and v <> 0) then
        AddSimple(String(TSynWeb_PhpKeywords[i]), 'keyword', ASynWeb.Engine.PhpKeyAttri.Foreground);
    end;
    CurrentInput := ct;
  end;

begin
  Result := SynWebGetHighlighterTypeAtCursor(ASynEdit, ASynWeb);

  ACompletion.InsertList.Clear;
  ACompletion.ItemList.Clear;

  if ASynWeb.Engine = nil then
    Exit;

  with ASynEdit, ASynWeb do
  begin
    ScanTo(CaretXY);

    if (GetTokenPos = 0) and (CaretX = 1) then
    begin
      ct := '';
      ctk := stkNull;
    end else
    begin
      ct := GetToken;
      ctk := GetTokenID;
    end;
  end;

  ct := Trim(ct);

  if shtML in Result then
    Html
  else
    if shtCss in Result then
      Css
    else
      if [shtPhpInML, shtPhpInCss, shtPhpInEs] * Result <> [] then
        Php;
end;

type
  PSynWebTagItem = ^TSynWebTagItem;
  TSynWebTagItem = record
    Pos: TBufferCoord;
    Marker: TSynWebTokenizerMarker;
    Prev: PSynWebTagItem;
  end;

function SynWebSortUnmatched(T1, T2: Pointer): Integer;
var
  l1, l2: TBufferCoord;
begin
  l1 := PSynWebUnmatchedTag(T2).Pos;
  l2 := PSynWebUnmatchedTag(T1).Pos;

  if l1.Line < l2.Line then
    Result := -1
  else
    if l1.Line > l2.Line then
      Result := 1
    else
      if l1.Char < l2.Char then
        Result := -1
      else
        if l1.Char > l2.Char then
          Result := 1
        else
          Result := 0;
end;

function SynWebGetUnmatchedTags(ASynEdit: TCustomSynEdit;
  var AUnmatchedTags: TSynWebUnmatchedTagArray; AAllowDuplicates: Boolean): Boolean;
begin
  Result := (ASynEdit.Highlighter is TSynWebMLSyn) and
    SynWebGetUnmatchedTags(
      ASynEdit.Lines,
      ASynEdit.CaretXY,
      TSynWebMLSyn(ASynEdit.Highlighter),
      AUnmatchedTags,
      AAllowDuplicates
    );
end;

function SynWebGetUnmatchedTags(ALines: TSynWebStrings; APos: TBufferCoord;
  ASynWeb: TSynWebMLSyn; var AUnmatchedTags: TSynWebUnmatchedTagArray; AAllowDuplicates: Boolean): Boolean;
var
  lT: ISynWebTokenizer;
  lTags: TSynWebStringList;
  lTagLastIdx: Integer;

  procedure OpenTag;
  var
    lTagItem: PSynWebTagItem;
    lTagId: Integer;
  begin
    // If this is a HTML tag which doesn't need close tag, then simply don't open it
    if ASynWeb.InternalInstanceData.FOptions.FMLVersion <= smlhvHtml401Frameset then
    begin
      lTagId := TSynWebHtmlSyn(ASynWeb).MLGetTagID;
      if lTagId > -1 then
        // If HTML_TAG(lTagId) IS EMPTY then
        if TSynWeb_TagsData[lTagId] and (1 shl 31) <> 0 then
          Exit;
    end;

    New(lTagItem);
    lTagItem^.Marker := lT.CurrentToken.Marker;
    lTagItem^.Pos := lT.CurrentToken.Pos;

    if lTags.Find(lT.CurrentToken.Token, lTagLastIdx) then
    begin
      lTagItem^.Prev := PSynWebTagItem(lTags.Objects[lTagLastIdx]);

      lTags.Objects[lTagLastIdx] := TObject(lTagItem);
    end else
    begin
      lTagItem^.Prev := nil;

      lTags.InsertObject(lTagLastIdx, lT.CurrentToken.Token, TObject(lTagItem));
    end;
  end;

  procedure CloseLastTag;
  var
    lTagItem: PSynWebTagItem;
  begin
    if lTagLastIdx = -1 then
      Exit;

    lTagItem := PSynWebTagItem(lTags.Objects[lTagLastIdx]);

    if lTagItem^.Prev = nil then
      lTags.Delete(lTagLastIdx)
    else
      lTags.Objects[lTagLastIdx] := TObject(lTagItem^.Prev);

    lTagLastIdx := -1;

    Dispose(lTagItem);
  end;

  procedure CloseTag;
  begin
    if lTags.Find(lT.CurrentToken.Token, lTagLastIdx) then
      CloseLastTag
    else
      lTagLastIdx := -1;
  end;

  procedure FillUnmatched;
  var
    lTagIdx: Integer;       
    lTagItem, lTagItemDel: PSynWebTagItem;
    lAdded: Boolean;
    lUnmatched: TList;
    lUnmatchedTag: PSynWebUnmatchedTag;
  begin
    lUnmatched := TList.Create;
    try
      for lTagIdx := 0 to lTags.Count - 1 do
      begin
        lTagItem := PSynWebTagItem(lTags.Objects[lTagIdx]);

        lAdded := False;

        while lTagItem <> nil do
        begin
          // Add only tags which was opened before caret pos, so marker must be after token pos
          if not lAdded and (lTagItem^.Marker = stlAfter) then
          begin
            lAdded := not AAllowDuplicates;

            New(lUnmatchedTag);
            lUnmatchedTag^.Pos := lTagItem^.Pos;
            lUnmatchedTag^.Tag := lTags[lTagIdx];

            lUnmatched.Add(lUnmatchedTag);
          end;

          lTagItemDel := lTagItem;
          lTagItem := lTagItem^.Prev;
          Dispose(lTagItemDel);
        end;
      end;

      lUnmatched.Sort(SynWebSortUnmatched);

      SetLength(AUnmatchedTags, lUnmatched.Count);
      for lTagIdx := 0 to lUnmatched.Count - 1 do
      begin
        lUnmatchedTag := lUnmatched[lTagIdx];

        AUnmatchedTags[lTagIdx] := lUnmatchedTag^;

        Dispose(lUnmatchedTag);
      end;

    finally
      lUnmatched.Free;
    end;
  end;

begin
  lT := TSynWebTokenizer.Create(ALines, ASynWeb);
  lT.MarkerPos := APos;

  lTags := TSynWebStringList.Create;
  try
    lTagLastIdx := -1;

    while not lT.IsEof do
    begin
      case lT.CurrentToken.Kind of
      stkMLTag:
        if lT.CurrentToken.Token = '<' then
        begin

          if lT.Next and (lT.CurrentToken.Kind in [stkMLTagName, stkMLTagNameUndef]) then
            OpenTag
          else
            lTagLastIdx := -1;

        end else
          if lT.CurrentToken.Token = '</' then
          begin

            if lT.Next and (lT.CurrentToken.Kind in [stkMLTagName, stkMLTagNameUndef]) then
              CloseTag;

          end else
            if lT.CurrentToken.Token = '>' then
              lTagLastIdx := -1
            else
              if lT.CurrentToken.Token = '/>' then
                CloseLastTag;

      stkMLError:
        if lT.CurrentToken.Token = '/' then
          CloseLastTag;
      end;

      lT.Next;
    end;

  finally
    FillUnmatched;

    lTags.Free;
  end;

  Result := Length(AUnmatchedTags) > 0;
end;

{ TSynWebWordMarkerCustom }

procedure TSynWebWordMarkerCustom.AfterConstruction;
begin
  inherited AfterConstruction;

  FMode := swwmSelectedWord;
  FEnabled := True;
end;

function TSynWebWordMarkerCustom.IsWordSelected: Boolean;

  function IsSameBuffer(const A, B: TBufferCoord): Boolean;
  begin
    Result := (A.Line = B.Line) and (A.Char = B.Char);
  end;

begin
  Result := Editor.SelAvail and IsSameBuffer(Editor.BlockBegin, Editor.WordStart) and
    IsSameBuffer(Editor.BlockEnd, Editor.WordEnd);
end;

function TSynWebWordMarkerCustom.IsOverlapSelection(const ABufferStart, ABufferEnd: TBufferCoord): Boolean;
var
  lBegin, lEnd: TBufferCoord;
begin
  Result := False;
  if not Editor.SelAvail then
    Exit;

  Result := True;
  if Editor.IsPointInSelection(ABufferStart) then
    Exit;

  lBegin := Editor.BlockBegin;
  lEnd := Editor.BlockEnd;

  if (lBegin.Line = lEnd.Line) and (lBegin.Line = ABufferStart.Line) and
    (lBegin.Char >= ABufferStart.Char) and (lEnd.Char <= ABufferEnd.Char)
  then
    Exit;

  if Editor.IsPointInSelection(ABufferEnd) then
    Exit;

  Result := False;
end;

function TSynWebWordMarkerCustom.GetHighlightText: TSynWebString;
begin
  case FMode of
  swwmSelectedWord, swwmSelectedText:
    Result := Editor.SelText;

  swwmCustomWord, swwmCustomText:
    Result := FCustomText;
  else
    Result := '';
  end;
end;

procedure TSynWebWordMarkerCustom.SetEnabled(const Value: Boolean);
begin
  if Value = FEnabled then
    Exit;

  FEnabled := Value;
  Editor.Invalidate;
end;

procedure TSynWebWordMarkerCustom.SetMode(const Value: TSynWebWordMarkerMode);
begin
  if FMode = Value then
    Exit;

  FMode := Value;
  DoInvalidate;
end;

procedure TSynWebWordMarkerCustom.SetCustomText(const Value: TSynWebString);
begin
  if FCustomText = Value then
    Exit;

  FCustomText := Value;
  DoInvalidate;
end;

procedure TSynWebWordMarkerCustom.SetCaseSensitive(const Value: Boolean);
begin
  if FCaseSensitive = Value then
    Exit;

  FCaseSensitive := Value;
  DoInvalidate;
end;

procedure TSynWebWordMarkerCustom.DoInvalidate;
begin
  if Enabled then
    Editor.Invalidate;
end;

procedure TSynWebWordMarkerCustom.LinesInserted(FirstLine, Count: Integer);
begin
  // nothing
end;

procedure TSynWebWordMarkerCustom.LinesDeleted(FirstLine, Count: Integer);
begin
  // nothing
end;

procedure TSynWebWordMarkerCustom.AfterPaint(ACanvas: TCanvas; const AClip: TRect;
  FirstLine, LastLine: Integer);
var
  lDisplay: TDisplayCoord;
  lBufferStart, lBufferEnd: TBufferCoord;      

  lLineRow: Integer;
  lPrevLine: Integer;

  lLineText, lLineTextLower: TSynWebString;
  lText: TSynWebString;
  lPos: Integer;

  lRect: TRect;
  lRectClip: TRect;
  lMarginLeft: Integer;

  function IsSameDisplay(const A, B: TDisplayCoord): Boolean;
  begin
    Result := (A.Row = B.Row) and (A.Column = B.Column);
  end;

  function DoLowerStr(const AStr: TSynWebString): TSynWebString;
  begin
    {$IFDEF UNISYNEDIT}
    Result := SynUnicode.SynWideLowerCase(AStr);
    {$ELSE}
    Result := LowerCase(AStr);
    {$ENDIF}
  end;

  function DoIntersectRect(var ARect: TRect; const AClip: TRect): Boolean;
  var
    lClipH: HRGN;
  begin
    Result := IntersectRect(ARect, ARect, AClip);
    if not Result then
      Exit;

    lClipH := CreateRectRgn(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
    if lClipH <> 0 then
    begin
      SelectClipRgn(ACanvas.Handle, lClipH);
      DeleteObject(lClipH);
    end;
  end;

begin
  if not Enabled then
    Exit;

  case FMode of
  swwmSelectedWord:
    if not IsWordSelected then
      Exit;

  swwmSelectedText:
    if not Editor.SelAvail or (Editor.BlockBegin.Line <> Editor.BlockEnd.Line) then
      Exit;

  swwmCustomWord, swwmCustomText:
    if FCustomText = '' then
      Exit;
  end;

  lText := GetHighlightText;
  if lText = '' then
    Exit;

  if not FCaseSensitive then
    lText := DoLowerStr(lText);

  lPrevLine := -1;
  lLineText := '';
  lLineTextLower := '';

  lDisplay.Column := 1;
  lDisplay.Row := FirstLine;

  if Editor.Gutter.Visible then
    lMarginLeft := Editor.Gutter.RealGutterWidth(Editor.CharWidth) - 2
  else
    lMarginLeft := 2;

  for lLineRow := FirstLine to LastLine do
  begin
    lDisplay.Column := 1;
    lDisplay.Row := lLineRow;

    lBufferStart := Editor.DisplayToBufferPos(lDisplay);
    if lPrevLine = lBufferStart.Line then
      Continue;

    lPrevLine := lBufferStart.Line;
    lLineText := Editor.Lines[lPrevLine - 1];
    if not FCaseSensitive then
      lLineTextLower := DoLowerStr(lLineText);

    if FCaseSensitive then
      lPos := Pos(lText, lLineText)
    else
      lPos := Pos(lText, lLineTextLower);
    while lPos > 0 do
    begin
      if not (FMode in [swwmSelectedWord, swwmCustomWord]) or (
        ((lPos = 1) or Editor.IsWordBreakChar(lLineText[lPos - 1])) and
        ((lPos - 1 + Length(lText) = Length(lLineText)) or Editor.IsWordBreakChar(lLineText[lPos + Length(lText)]))
      ) then
      begin
        lBufferStart.Char := lPos;
        lBufferEnd.Line := lBufferStart.Line;
        lBufferEnd.Char := lBufferStart.Char + Length(lText) - 1;

        lDisplay := Editor.BufferToDisplayPos(lBufferStart);

        lRect.TopLeft := Editor.RowColumnToPixels(lDisplay);
        lRect.Right := lRect.Left + (Editor.CharWidth * Length(lText));
        lRect.Bottom := lRect.Top + Editor.LineHeight;

        lRectClip := lRect;

        if lRectClip.Left < lMarginLeft then
          lRectClip.Left := lMarginLeft;

        if Editor.WordWrap then
          if lRectClip.Right > Editor.ClientRect.Right then
          begin
            lRectClip.Right := Editor.ClientRect.Right;
            lRectClip.Right := lRectClip.Right - ((lRectClip.Right - lRectClip.Left) mod Editor.CharWidth);
          end;

        if DoIntersectRect(lRectClip, AClip) then
          if FCaseSensitive then          
            DoPaintMarker(ACanvas, lRect, lText, lBufferStart, lBufferEnd)
          else
            DoPaintMarker(ACanvas, lRect, Copy(lLineText, lPos, Length(lText)), lBufferStart, lBufferEnd);
      end;

      if FCaseSensitive then
        lPos := PosEx(lText, lLineText, lPos + Length(lText))
      else
        lPos := PosEx(lText, lLineTextLower, lPos + Length(lText));
    end;
  end;     

  // Clear clip region for canvas
  SelectClipRgn(ACanvas.Handle, 0);
end;

procedure TSynWebWordMarkerCustom.NotifySelChanged;
var
  lIsWordSelected: Boolean;
  lIsMultiLineSelection: Boolean;
begin
  lIsMultiLineSelection := Editor.BlockBegin.Line <> Editor.BlockEnd.Line;

  if not lIsMultiLineSelection or not FIsMultiLineSelection then
    case FMode of
    swwmSelectedWord:
      begin
        lIsWordSelected := IsWordSelected;

        if lIsWordSelected = FIsWordSelected then
          Exit;

        FIsWordSelected := lIsWordSelected;
        DoInvalidate;
      end;

    swwmSelectedText:
      DoInvalidate;
    end;

  FIsMultiLineSelection := lIsMultiLineSelection;
end;

{ TSynWebWordMarker }

procedure TSynWebWordMarker.AfterConstruction;
begin
  inherited AfterConstruction;

  FBGColor := clYellow;
  FFGColor := clBlack;
  FPaintMode := swwpFillRect;
end;

procedure TSynWebWordMarker.SetBGColor(const Value: TColor);
begin
  if FBGColor = Value then
    Exit;

  FBGColor := Value;
  DoInvalidate;
end;

procedure TSynWebWordMarker.SetFGColor(const Value: TColor);
begin
  if FFGColor = Value then
    Exit;

  FFGColor := Value;
  DoInvalidate;
end;

procedure TSynWebWordMarker.SetPaintMode(const Value: TSynWebWordMarkerPaintMode);
begin
  if FPaintMode = Value then
    Exit;

  FPaintMode := Value;
  DoInvalidate;
end;

procedure TSynWebWordMarker.DoPaintMarker(ACanvas: TCanvas; const ARect: TRect;
  const AText: TSynWebString; const ABufferStart, ABufferEnd: TBufferCoord);

  function DoGetPaintMode: TSynWebWordMarkerPaintMode;
  begin
    case FPaintMode of
    swwpFillRect:
      if IsOverlapSelection(ABufferStart, ABufferEnd) then
        Result := swwpFrameRect
      else
        Result := swwpFillRect;
    else
      Result := FPaintMode;
    end;
  end;

var
  lRect: TRect;
begin
  ACanvas.Brush.Color := FBGColor;

  case DoGetPaintMode of
  swwpFillRect:
    begin
      ACanvas.Font.Color := FFGColor;
      ACanvas.TextRect(ARect, ARect.Left, ARect.Top, AText)
    end;

  swwpFrameRect:
    ACanvas.FrameRect(ARect);

  swwpUnderline:
    begin
      lRect := ARect;
      lRect.Top := lRect.Bottom - 2;
      ACanvas.FillRect(lRect);
    end;
  end;
end;

{ TSynWebTokenizer }

constructor TSynWebTokenizer.Create(ALines: TSynWebStrings;
  AHighlighter: TSynWebBase; APos: TBufferCoord; ACustomRange: Pointer);
begin
  inherited Create;

  FLines := ALines;
  FHL := AHighlighter;

  FLine := APos.Line - 1;
  if FLine < 0 then
    FLine := 0;

  Dec(APos.Char);
  if APos.Char < 0 then
    APos.Char := 0;

  if not IsEof then
    with FHL do
    begin
      if ACustomRange <> nil then
        SetRange(ACustomRange)
      else
        if FLine = 0 then
          ResetRange
        else
          SetRange(TSynEditStringList(FLines).Ranges[FLine - 1]);

      SetLine(FLines[FLine], FLine + 1);

      GetCurrentTokenInfo(FCurrentToken);

      while not GetEol and (GetTokenPos + GetTokenLen <= APos.Char) do
        Self.Next;
    end;
end;

constructor TSynWebTokenizer.Create(AEditor: TCustomSynEdit;
  APos: TBufferCoord);
begin
  Create(AEditor.Lines, AEditor.Highlighter as TSynWebBase, APos);
end;

function TSynWebTokenizer.CurrentToken: TSynWebTokenizerInfo;
begin
  Result := FCurrentToken;
end;

constructor TSynWebTokenizer.Create(ALines: TSynWebStrings;
  AHighlighter: TSynWebBase; ALine: Integer);
var
  lPos: TBufferCoord;
begin
  lPos.Line := ALine;
  lPos.Char := 1;
  Create(ALines, AHighlighter, lPos);
end;

constructor TSynWebTokenizer.Create(AEditor: TCustomSynEdit; ALine: Integer);
var
  lPos: TBufferCoord;
begin
  lPos.Line := ALine;
  lPos.Char := 1;

  Create(AEditor, lPos);
end;

procedure TSynWebTokenizer.GetCurrentTokenInfo(
  var ATokenInfo: TSynWebTokenizerInfo);
begin
  with ATokenInfo do
  begin
    Kind := FHL.GetTokenID;
    Token := FHL.GetToken;
    Len := FHL.GetTokenLen;
    if Len = 0 then
      Len := 32768;

    Range := Longword(FHL.GetRange);

    Char := FHL.GetTokenPos + 1;
    Line := FLine + 1;

    PhpKeyId := FHL.PhpGetKeywordId;
    PhpSymbolId := FHL.PhpGetSymbolId;
    EsSymbolId := FHL.EsGetSymbolId;
    MLTagId := FHL.MLGetTagID;
    MLTagKind := FHL.MLGetTagKind;
  end;
  UpdateMaker(ATokenInfo);
end;

function TSynWebTokenizer.GetMarkerPos: TBufferCoord;
begin
  Result := FMarkerPos;
end;

class function TSynWebTokenizer.GetTokenCompletionAtCaret(
  AEditor: TCustomSynEdit): TSynWebString;
const
  cSTK_IDENTS = [stkEsIdentifier, stkEsKeyword, stkPhpIdentifier, stkPhpMethod, stkPhpConst, stkPhpFunction, stkPhpVariable, stkPhpKeyword];
var
  lT: ISynWebTokenizer;

  function IsIdentToken(const ATokenInfo: TSynWebTokenizerInfo): Boolean;
  begin
    Result := (ATokenInfo.Kind in cSTK_IDENTS) or
      (
        (ATokenInfo.Kind in [stkPhpError, stkPhpKeyword]) and
        ((ATokenInfo.Token = '$') or (ATokenInfo.Token = '$$'))
      );
  end;

begin
  lT := TSynWebTokenizer.Create(AEditor, AEditor.CaretXY);
  lT.MarkerPos := AEditor.CaretXY;

  if (lT.PreviousToken.Marker = stlEnd) and IsIdentToken(lT.PreviousToken) then
  begin
    Result := lT.PreviousToken.Token;

    if lT.PreviousToken.Kind = stkPhpVariable then
      Result := '$' + Result;

  end else
    if IsIdentToken(lT.CurrentToken) then
    begin
      Result := Copy(lT.CurrentToken.Token, 1, AEditor.CaretX - lT.CurrentToken.Char);

      if lT.CurrentToken.Kind = stkPhpVariable then
        Result := '$' + Result;
    end else
      Result := '';

  if Trim(Result) = '' then
    Result := '';
end;

function TSynWebTokenizer.HL: TSynWebBase;
begin
  Result := FHL;
end;

function TSynWebTokenizer.IsEof: Boolean;
begin
  Result := FLine >= FLines.Count;
end;

function TSynWebTokenizer.IsEol: Boolean;
begin
  Result := FHL.GetEol;
end;

function TSynWebTokenizer.PreviousToken: TSynWebTokenizerInfo;
begin
  Result := FLastToken;
end;

function TSynWebTokenizer.NextAndSkip(const ASkip: TSynWebTokenizerSet): Boolean;
begin
  Next;
  Skip(ASkip);
  Result := not IsEof;
end;

function TSynWebTokenizer.Next: Boolean;
begin
  Result := False;

  if IsEof then
    Exit;

  if IsEol then
  begin
    Inc(FLine);
    if IsEof then
      Exit;

    FLastToken := FCurrentToken;
    FHL.SetLine(FLines[FLine], FLine + 1);
    GetCurrentTokenInfo(FCurrentToken);
  end else
  begin
    FLastToken := FCurrentToken;
    FHL.Next;                          
    GetCurrentTokenInfo(FCurrentToken);
  end;

  Result := True;
end;

procedure TSynWebTokenizer.SetMarkerPos(const AMarkerPos: TBufferCoord);
begin
  FMarkerPos := AMarkerPos;

  UpdateMaker(FLastToken);
  UpdateMaker(FCurrentToken);
end;

function TSynWebTokenizer.Skip(const ASkip: TSynWebTokenizerSet): Boolean;
begin
  Result := False;

  while (CurrentToken.Kind in ASkip) and Next do
    Result := True;
end;

procedure TSynWebTokenizer.UpdateMaker(var ATokenInfo: TSynWebTokenizerInfo);
begin
  with ATokenInfo do
    if FMarkerPos.Line < Line then
      Marker := stlBefore
    else
      if FMarkerPos.Line > Line then
        Marker := stlAfter
      else
        if FMarkerPos.Char < Char then
          Marker := stlBefore
        else
          if FMarkerPos.Char = Char then
            Marker := stlBegin
          else
            if FMarkerPos.Char < Char + Len then
              Marker := stlInside
            else
              if FMarkerPos.Char = Char + Len then
                Marker := stlEnd
              else
                Marker := stlAfter;
end;

{ TSynWebSimpleTokenizer }

constructor TSynWebSimpleTokenizer.Create(AHighlighter: TSynWebBase;
  ALines: TSynWebStrings; AOwnHighlighter: Boolean;
  ALine: Integer; ARange: Pointer);
begin
  FHL := AHighlighter;
  FOwnHL := AOwnHighlighter;
  FLines := ALines;
  FLine := ALine;

  if FLine = 0 then
    FHL.ResetRange
  else
    FHL.SetRange(ARange);

  if not IsEof then
    FHL.SetLine(FLines[FLine], FLine + 1);
end;

destructor TSynWebSimpleTokenizer.Destroy;
begin
  if FOwnHL then
    FHL.Free;

  inherited Destroy;
end;

function TSynWebSimpleTokenizer.IsEof: Boolean;
begin
  Result := FLine >= FLines.Count;
end;

function TSynWebSimpleTokenizer.IsEol: Boolean;
begin
  Result := FHL.GetTokenID = stkNull;
end;

procedure TSynWebSimpleTokenizer.Next;
begin
  if IsEof then
    Exit;

  if IsEol then
  begin
    Inc(FLine);
    if IsEof then
      Exit;

    FHL.SetLine(FLines[FLine], FLine + 1);
  end else
    FHL.Next;
end;

end.

