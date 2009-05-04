{-------------------------------------------------------------------------------
SynWeb
Copyright (C) 2006  Krystian Bigaj

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

Known limitations:
- SynWeb highlighters support only single line SetLine (don't use more than one line).
- Doesn't support #13#10, #10 or #13 as new line. Always use #0 as line break.
- Php: Doesn't support multi-line encapsuled strings in String, only single line:
  eg. "somestring {$a["some array{$b['key'].... <- only single line encapsuled values
-------------------------------------------------------------------------------}
{
@abstract(Provides an web-files (Multi Html/XHtml/Wml/Xml/Css/ECMAScript/Php) highlighter for SynEdit
@author(Krystian Bigaj <krystian.bigaj@gmail.com>)
@created(2005-05-21)
@lastmod(2006-12-05)
The SynHighlighterWeb unit provides SynEdit with a Multi Html/XHtml/Wml/Xml/Css/ECMAScript/Php highlighter.
}

{$IFNDEF QSYNHIGHLIGHTERWEB}
unit SynHighlighterWeb;
{$ENDIF}

{$I SynWeb.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,  
  QSynHighlighterWebData,
{$ELSE}
  Graphics,
                  
  SynEditHighlighter, 
  SynHighlighterWebData,
{$ENDIF}
  Classes,
  SysUtils;

// Highlighter -----------------------------------------------------------------

type
  TSynWebEngine = class;
  TSynWebBase = class;

  PSynWebOptions = ^TSynWebOptions;

  TSynWebOptions = record
    FMLVersion: TSynWebMLVersion;
    FHtmlVersion: TSynWebHtmlVersion;
    FWmlVersion: TSynWebWmlVersion;
    FCssVersion: TSynWebCssVersion;
    FPhpVersion: TSynWebPhpVersion;
    FPhpShortOpenTag: Boolean;
    FPhpAspTags: Boolean;

    FPhpEmbeded: Boolean;
    FCssEmbeded: Boolean;
    FEsEmbeded: Boolean;
  end;

  PSynWebInstance = ^TSynWebInstance;

  TSynWebInstance = record
    FRun: Longint;
    FRange: Longword;
    FLine: PChar;
    FLineRef: String;
    FLineNumber: Integer;
    FTokenLastID: Integer;
    FTokenPos: Integer;
    FTokenID: TSynWebTokenKind;
    FStringLen, FStringLenClean: Integer;
    FTokenLastSymbolId: Integer;
    FToIdent: PChar;
    FHashTable: TSynWebHashTable;
    FNextClearBits: Boolean;
    FNextUseNextAH: Boolean;
    FUseNextAH: Boolean;
    FHighlighterType: TSynWebHighlighterType;
    FPrevHighlighterType, FNextHighlighterType: TSynWebHighlighterType;
    FHighlighterSW: Boolean;
    FHighlighterMode: TSynWebHighlighterMode;
    FCssMask: Longword;
    FNextProcTable: TSynWebProcTableProc;
    FSYN_ATTR_COMMENT: TSynHighlighterAttributes;
    FSYN_ATTR_STRING: TSynHighlighterAttributes;
    FSYN_ATTR_WHITESPACE: TSynHighlighterAttributes;
    FOptions: TSynWebOptions;
  end;

  TSynWebOptionsBase = class(TPersistent)
  private
    FOptions: PSynWebOptions;
    FEngineOptions: PSynWebOptions;
    FUseEngineOptions: Boolean;
    FOnChange: TNotifyEvent;
    function GetHtmlVersion: TSynWebHtmlVersion;
    procedure SetHtmlVersion(const Value: TSynWebHtmlVersion);
    function GetWmlVersion: TSynWebWmlVersion;
    procedure SetWmlVersion(const Value: TSynWebWmlVersion);
    function GetCssVersion: TSynWebCssVersion;
    procedure SetCssVersion(const Value: TSynWebCssVersion);
    function GetPhpVersion: TSynWebPhpVersion;
    procedure SetPhpVersion(const Value: TSynWebPhpVersion);
    function GetPhpAspTags: Boolean;
    procedure SetPhpAspTags(const Value: Boolean);
    function GetPhpShortOpenTag: Boolean;
    procedure SetPhpShortOpenTag(const Value: Boolean);

    function GetCssEmbeded: Boolean;
    procedure SetCssEmbeded(const Value: Boolean);
    function GetEsEmbeded: Boolean;
    procedure SetEsEmbeded(const Value: Boolean);
    function GetPhpEmbeded: Boolean;
    procedure SetPhpEmbeded(const Value: Boolean);

    procedure SetUseEngineOptions(const Value: Boolean);
    procedure SetEngineOptions(AEngine: PSynWebOptions);
    procedure DoOnChange;
    procedure UpdateOptions;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure UpdateMLOption; virtual;
    function CanUseEngineOptions: Boolean;

    property HtmlVersion: TSynWebHtmlVersion read GetHtmlVersion write SetHtmlVersion;
    property WmlVersion: TSynWebWmlVersion read GetWmlVersion write SetWmlVersion;
    property CssVersion: TSynWebCssVersion read GetCssVersion write SetCssVersion;
    property PhpVersion: TSynWebPhpVersion read GetPhpVersion write SetPhpVersion;
    property PhpShortOpenTag: Boolean read GetPhpShortOpenTag write SetPhpShortOpenTag;
    property PhpAspTags: Boolean read GetPhpAspTags write SetPhpAspTags;

    property CssEmbeded: Boolean read GetCssEmbeded write SetCssEmbeded;
    property PhpEmbeded: Boolean read GetPhpEmbeded write SetPhpEmbeded;
    property EsEmbeded: Boolean read GetEsEmbeded write SetEsEmbeded;

    property UseEngineOptions: Boolean read FUseEngineOptions write SetUseEngineOptions;
  public
    constructor Create(AOptions: PSynWebOptions);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TSynWebHtmlOptions = class(TSynWebOptionsBase)
  protected
    procedure UpdateMLOption; override;
  public
    constructor Create(AOptions: PSynWebOptions);
  published
    property HtmlVersion;
    property CssVersion;
    property PhpVersion;
    property PhpShortOpenTag;
    property PhpAspTags;
    property CssEmbeded;
    property PhpEmbeded;
    property EsEmbeded;
    property UseEngineOptions;
  end;

  TSynWebWmlOptions = class(TSynWebOptionsBase)  
  protected
    procedure UpdateMLOption; override;
  public
    constructor Create(AOptions: PSynWebOptions);
  published
    property WmlVersion;
    property PhpVersion;
    property PhpShortOpenTag;
    property PhpAspTags;
    property PhpEmbeded;
    property UseEngineOptions;
  end;

  TSynWebXmlOptions = class(TSynWebOptionsBase)
  protected
    procedure UpdateMLOption; override;
  public
    constructor Create(AOptions: PSynWebOptions);
  published
    property PhpVersion;
    property PhpShortOpenTag;
    property PhpAspTags;
    property PhpEmbeded;
    property UseEngineOptions;
  end;

  TSynWebCssOptions = class(TSynWebOptionsBase)
  published
    property HtmlVersion;
    property CssVersion;
    property PhpVersion;
    property PhpShortOpenTag;
    property PhpAspTags;
    property PhpEmbeded;
    property UseEngineOptions;
  end;

  TSynWebEsOptions = class(TSynWebOptionsBase)
  published
    property PhpVersion;
    property PhpShortOpenTag;
    property PhpAspTags;
    property PhpEmbeded;
    property UseEngineOptions;
  end;

  TSynWebPhpCliOptions = class(TSynWebOptionsBase)
  published
    property PhpVersion;
    property PhpShortOpenTag;
    property PhpAspTags;
    property UseEngineOptions;
  end;

  TSynWebEngineOptions = class(TSynWebOptionsBase)
  public
    constructor Create(AOptions: PSynWebOptions);
  published
    property HtmlVersion;
    property WmlVersion;
    property CssVersion;
    property PhpVersion;
    property PhpShortOpenTag;
    property PhpAspTags;
  end;
              
  TSynWebBaseClass = class of TSynWebBase;

  TSynWebBase = class(TSynCustomHighlighter)
  private
    FInstance: TSynWebInstance;
    FEngine: TSynWebEngine;
    FActiveHighlighter: Boolean;
    FActiveHighlighters: TSynWebHighlighterTypes;
    FOptions: TSynWebOptionsBase;
    procedure SetupActiveHighlighter; virtual; abstract;
    procedure SetActiveHighlighter(const Value: Boolean);
    procedure SetEngine(const Value: TSynWebEngine);
  protected
{$IFDEF UNISYNEDIT}
    procedure DoSetLine(const Value: WideString; LineNumber: Integer); override;
{$ENDIF}
    procedure DoDefHighlightChange;
    function GetAttribCount: Integer; override;
    function GetAttribute(idx: Integer): TSynHighlighterAttributes; override;
{$IFNDEF UNISYNEDIT}
    function GetIdentChars: TSynIdentChars; override;
{$ENDIF}
{$IFDEF UNISYNEDIT}
    function GetSampleSource: WideString; override;
{$ELSE}
    function GetSampleSource: String; override;
{$ENDIF}
  public
{$IFDEF UNISYNEDIT}
    class function GetFriendlyLanguageName: WideString; override;
    class function SynWebSample: WideString; virtual; abstract;
{$ELSE}
    class function SynWebSample: String; virtual; abstract;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
{$IFNDEF UNISYNEDIT}
    function GetToken: String; override;
{$ENDIF}
    function GetTokenLen: Integer;
    function GetTokenPos: Integer; override;
    function GetTokenID: TSynWebTokenKind;
    function GetTokenKind: Integer; override;
    function GetRange: Pointer; override;
    function GetEol: Boolean; override;
    function GetHighlighterType: TSynWebHighlighterType;

    function PhpGetKeywordId: Integer;
    function PhpGetFunctionId: Integer;
    function PhpGetSymbolId: Integer;
    function PhpGetRange: TSynWebPhpRangeState;

    function CssGetPropertyId: Integer;
    function CssGetRange: TSynWebCssRangeState;

    procedure SetRange(Value: Pointer); override;
{$IFNDEF UNISYNEDIT}
    procedure SetLine(NewValue: String; LineNumber: Integer); override;
{$ENDIF}
    procedure Next; override;
{$IFDEF UNISYNEDIT}
    function GetActiveHighlighter(ARange: Pointer; ALine: WideString;
      ACaretX, ACaretY: Integer): TSynWebHighlighterTypes;
{$ELSE}
    function GetActiveHighlighter(ARange: Pointer; ALine: String;
      ACaretX, ACaretY: Integer): TSynWebHighlighterTypes;
{$ENDIF}
    property InternalInstanceData: TSynWebInstance read FInstance;
    property ActiveHighlighters: TSynWebHighlighterTypes read FActiveHighlighters
      write FActiveHighlighters;
  published
    property ActiveHighlighterSwitch: Boolean
      read FActiveHighlighter write SetActiveHighlighter;
    property Engine: TSynWebEngine read FEngine write SetEngine;
  end;

  TSynWebMLSyn = class(TSynWebBase)
  private
    procedure SetupActiveHighlighter; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ResetRange; override;

    function GetTagID: Integer;
    function GetTagKind: Integer;
    function GetMLRange: TSynWebMLRangeState;
  end;

  TSynWebHtmlSynClass = class of TSynWebHtmlSyn;

  TSynWebHtmlSyn = class(TSynWebMLSyn)
  private
    function GetOptions: TSynWebHtmlOptions;
    procedure SetOptions(const AValue: TSynWebHtmlOptions);
  public
    class function GetLanguageName: string; override;
{$IFDEF UNISYNEDIT}
    class function SynWebSample: WideString; override;
{$ELSE}
    class function SynWebSample: String; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Options: TSynWebHtmlOptions read GetOptions write SetOptions;
  end;

  TSynWebWmlSynClass = class of TSynWebWmlSyn;

  TSynWebWmlSyn = class(TSynWebMLSyn)
  private
    function GetOptions: TSynWebWmlOptions;
    procedure SetOptions(const AValue: TSynWebWmlOptions);
  public
    class function GetLanguageName: string; override;
{$IFDEF UNISYNEDIT}
    class function SynWebSample: WideString; override;
{$ELSE}
    class function SynWebSample: String; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Options: TSynWebWmlOptions read GetOptions write SetOptions;
  end;

  TSynWebXmlSynClass = class of TSynWebXmlSyn;

  TSynWebXmlSyn = class(TSynWebMLSyn)
  private
    function GetOptions: TSynWebXmlOptions;
    procedure SetOptions(const AValue: TSynWebXmlOptions);
  public
    class function GetLanguageName: string; override;
{$IFDEF UNISYNEDIT}
    class function SynWebSample: WideString; override;
{$ELSE}
    class function SynWebSample: String; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Options: TSynWebXmlOptions read GetOptions write SetOptions;
  end;

  TSynWebCssSynClass = class of TSynWebCssSyn;

  TSynWebCssSyn = class(TSynWebBase)
  private
    procedure SetupActiveHighlighter; override;
    function GetOptions: TSynWebCssOptions;
    procedure SetOptions(const AValue: TSynWebCssOptions);
  public
    class function GetLanguageName: string; override;
{$IFDEF UNISYNEDIT}
    class function SynWebSample: WideString; override;
{$ELSE}
    class function SynWebSample: String; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    procedure ResetRange; override;
  published
    property Options: TSynWebCssOptions read GetOptions write SetOptions;
  end;

  TSynWebEsSynClass = class of TSynWebEsSyn;

  TSynWebEsSyn = class(TSynWebBase)
  private
    procedure SetupActiveHighlighter; override;
    function GetOptions: TSynWebEsOptions;
    procedure SetOptions(const AValue: TSynWebEsOptions);
  public
    class function GetLanguageName: string; override;
{$IFDEF UNISYNEDIT}
    class function SynWebSample: WideString; override;
{$ELSE}
    class function SynWebSample: String; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    procedure ResetRange; override;
  published
    property Options: TSynWebEsOptions read GetOptions write SetOptions;
  end;

  TSynWebPhpCliSynClass = class of TSynWebPhpCliSyn;

  TSynWebPhpCliSyn = class(TSynWebBase)
  private
    procedure SetupActiveHighlighter; override;
    function GetOptions: TSynWebPhpCliOptions;
    procedure SetOptions(const AValue: TSynWebPhpCliOptions);
  public
    class function GetLanguageName: string; override;  
{$IFDEF UNISYNEDIT}
    class function SynWebSample: WideString; override;
{$ELSE}
    class function SynWebSample: String; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    procedure ResetRange; override;
  published
    property Options: TSynWebPhpCliOptions read GetOptions write SetOptions;
  end;

  TSynWebEngine = class(TComponent)
  private
    // Global ------------------------------------------------------------------
    FNotifyList: TList;
    FInstance: PSynWebInstance;
    FAttributes: TStringList;
    FInactiveAttri: TSynHighlighterAttributes;
    FTokenAttributeTable: TSynWebTokenAttributeTable;
    FPhpHereDocList: TStringList;
    FEngineOptions: TSynWebOptions;
    FOptions: TSynWebEngineOptions;

    // Markup Language ---------------------------------------------------------
    FMLTagIdentFuncTable: array[0..MLTagMaxKeyHash] of TSynWebIdentFuncTableFunc;
    FMLAttrIdentFuncTable: array[0..MLAttrMaxKeyHash] of TSynWebIdentFuncTableFunc;
    FMLSpecialIdentFuncTable: array[0..MLSpecialMaxKeyHash] of TSynWebIdent2FuncTableFunc;
    FMLRangeProcTable: array[Low(TSynWebMLRangeState)..High(TSynWebMLRangeState)] of TSynWebProcTableProc;

    FMLWhitespaceAttri: TSynHighlighterAttributes;
    FMLCommentAttri: TSynHighlighterAttributes;
    FMLTextAttri: TSynHighlighterAttributes;
    FMLEscapeAttri: TSynHighlighterAttributes;
    FMLSymbolAttri: TSynHighlighterAttributes;
    FMLTagAttri: TSynHighlighterAttributes;
    FMLTagNameAttri: TSynHighlighterAttributes;
    FMLTagNameUndefAttri: TSynHighlighterAttributes;
    FMLTagKeyAttri: TSynHighlighterAttributes;
    FMLTagKeyUndefAttri: TSynHighlighterAttributes;
    FMLTagKeyValueAttri: TSynHighlighterAttributes;
    FMLTagKeyValueQuotedAttri: TSynHighlighterAttributes;
    FMLErrorAttri: TSynHighlighterAttributes;

    // Css ---------------------------------------------------------------------
    FCssProcTable: array[#0..#255] of TSynWebProcTableProc;
    FCssPropIdentFuncTable: array[0..CssPropMaxKeyHash] of TSynWebIdentFuncTableFunc;
    FCssValIdentFuncTable: array[0..CssValMaxKeyHash] of TSynWebIdentFuncTableFunc;
    FCssSpecialIdentFuncTable: array[0..CssSpecialMaxKeyHash] of TSynWebIdent2FuncTableFunc;
    FCssRangeProcTable: array[Low(TSynWebCssRangeState)..High(TSynWebCssRangeState)] of TSynWebProcTableProc;

    FCssWhitespaceAttri: TSynHighlighterAttributes;
    FCssRulesetWhitespaceAttri: TSynHighlighterAttributes;
    FCssSelectorAttri: TSynHighlighterAttributes;
    FCssSelectorUndefAttri: TSynHighlighterAttributes;
    FCssSelectorClassAttri: TSynHighlighterAttributes;
    FCssSelectorIdAttri: TSynHighlighterAttributes;
    FCssSpecialAttri: TSynHighlighterAttributes;
    FCssCommentAttri: TSynHighlighterAttributes;
    FCssPropAttri: TSynHighlighterAttributes;
    FCssPropUndefAttri: TSynHighlighterAttributes;
    FCssValAttri: TSynHighlighterAttributes;
    FCssValUndefAttri: TSynHighlighterAttributes;
    FCssValStringAttri: TSynHighlighterAttributes;
    FCssValNumberAttri: TSynHighlighterAttributes;
    FCssSymbolAttri: TSynHighlighterAttributes;
    FCssErrorAttri: TSynHighlighterAttributes;

    // ECMAScript --------------------------------------------------------------
    FEsProcTable: array[#0..#255] of TSynWebProcTableProc;
    FEsIdentFuncTable: array[0..EsKeywordsMaxKeyHash] of TSynWebIdentFuncTableFunc;
    FEsRangeProcTable: array[Low(TSynWebEsRangeState)..High(TSynWebEsRangeState)] of TSynWebProcTableProc;

    FEsWhitespaceAttri: TSynHighlighterAttributes;
    FEsIdentifierAttri: TSynHighlighterAttributes;
    FEsKeyAttri: TSynHighlighterAttributes;
    FEsCommentAttri: TSynHighlighterAttributes;
    FEsStringAttri: TSynHighlighterAttributes;
    FEsNumberAttri: TSynHighlighterAttributes;
    FEsSymbolAttri: TSynHighlighterAttributes;
    FEsErrorAttri: TSynHighlighterAttributes;

    // Php ---------------------------------------------------------------------
    FPhpProcTable: array[#0..#255] of TSynWebProcTableProc;
    FPhpIdentFuncTable: array[0..PhpKeywordsMaxKeyHash] of TSynWebIdentFuncTableFunc;
    FPhpRangeProcTable: array[Low(TSynWebPhpRangeState)..High(TSynWebPhpRangeState)] of TSynWebProcTableProc;

    FPhpWhitespaceAttri: TSynHighlighterAttributes;
    FPhpInlineTextAttri: TSynHighlighterAttributes;
    FPhpIdentifierAttri: TSynHighlighterAttributes;
    FPhpKeyAttri: TSynHighlighterAttributes;
    FPhpFunctionAttri: TSynHighlighterAttributes;
    FPhpVariableAttri: TSynHighlighterAttributes;
    FPhpConstAttri: TSynHighlighterAttributes;
    FPhpMethodAttri: TSynHighlighterAttributes;
    FPhpStringAttri: TSynHighlighterAttributes;
    FPhpStringSpecialAttri: TSynHighlighterAttributes;
    FPhpCommentAttri: TSynHighlighterAttributes;
    FPhpDocCommentAttri: TSynHighlighterAttributes;
    FPhpDocCommentTagAttri: TSynHighlighterAttributes;
    FPhpSymbolAttri: TSynHighlighterAttributes;
    FPhpNumberAttri: TSynHighlighterAttributes;
    FPhpErrorAttri: TSynHighlighterAttributes;

    // MarkupLanguage ----------------------------------------------------------
    procedure MLMakeMethodTables;
    procedure MLNext;
    function MLGetRange: TSynWebMLRangeState;
    procedure MLSetRange(const ARange: TSynWebMLRangeState);
    function MLGetTag: Integer;
    procedure MLSetTag(const ATag: Integer);
    function MLCheckNull(ADo: Boolean = True): Boolean;

    procedure MLSpaceProc;
    procedure MLAmpersandProc;
    procedure MLBraceOpenProc;
    procedure MLErrorProc;

    procedure MLRangeTextProc;
    procedure MLRangeCommentProc;
    procedure MLRangeCommentCloseProc;
    procedure MLRangeTagDOCTYPEProc;
    procedure MLRangeTagCDATAProc;
    procedure MLRangeTagProc;
    procedure MLRangeTagCloseProc;
    procedure MLRangeTagKeyProc;
    procedure MLRangeTagKeyEqProc;
    procedure MLRangeTagKeyValueProc;
    procedure MLRangeTagKeyValueQuoted1Proc;
    procedure MLRangeTagKeyValueQuoted2Proc;

    function MLTagKeyComp(const ID: Integer): Boolean;
    function MLTagCheck: TSynWebTokenKind;
    function MLTagUndef: TSynWebTokenKind;
    function MLTagFunc1: TSynWebTokenKind;
    function MLTagFunc2: TSynWebTokenKind;
    function MLTagFunc8: TSynWebTokenKind;
    function MLTagFunc9: TSynWebTokenKind;
    function MLTagFunc16: TSynWebTokenKind;
    function MLTagFunc17: TSynWebTokenKind;
    function MLTagFunc18: TSynWebTokenKind;
    function MLTagFunc19: TSynWebTokenKind;
    function MLTagFunc20: TSynWebTokenKind;
    function MLTagFunc21: TSynWebTokenKind;
    function MLTagFunc22: TSynWebTokenKind;
    function MLTagFunc23: TSynWebTokenKind;
    function MLTagFunc24: TSynWebTokenKind;
    function MLTagFunc25: TSynWebTokenKind;
    function MLTagFunc26: TSynWebTokenKind;
    function MLTagFunc27: TSynWebTokenKind;
    function MLTagFunc28: TSynWebTokenKind;
    function MLTagFunc29: TSynWebTokenKind;
    function MLTagFunc30: TSynWebTokenKind;
    function MLTagFunc31: TSynWebTokenKind;
    function MLTagFunc32: TSynWebTokenKind;
    function MLTagFunc33: TSynWebTokenKind;
    function MLTagFunc35: TSynWebTokenKind;
    function MLTagFunc36: TSynWebTokenKind;
    function MLTagFunc37: TSynWebTokenKind;
    function MLTagFunc38: TSynWebTokenKind;
    function MLTagFunc39: TSynWebTokenKind;
    function MLTagFunc40: TSynWebTokenKind;
    function MLTagFunc41: TSynWebTokenKind;
    function MLTagFunc42: TSynWebTokenKind;
    function MLTagFunc43: TSynWebTokenKind;
    function MLTagFunc46: TSynWebTokenKind;
    function MLTagFunc47: TSynWebTokenKind;
    function MLTagFunc48: TSynWebTokenKind;
    function MLTagFunc49: TSynWebTokenKind;
    function MLTagFunc50: TSynWebTokenKind;
    function MLTagFunc52: TSynWebTokenKind;
    function MLTagFunc53: TSynWebTokenKind;
    function MLTagFunc55: TSynWebTokenKind;
    function MLTagFunc56: TSynWebTokenKind;
    function MLTagFunc57: TSynWebTokenKind;
    function MLTagFunc59: TSynWebTokenKind;
    function MLTagFunc60: TSynWebTokenKind;
    function MLTagFunc61: TSynWebTokenKind;
    function MLTagFunc64: TSynWebTokenKind;
    function MLTagFunc65: TSynWebTokenKind;
    function MLTagFunc66: TSynWebTokenKind;
    function MLTagFunc70: TSynWebTokenKind;
    function MLTagFunc76: TSynWebTokenKind;
    function MLTagFunc78: TSynWebTokenKind;
    function MLTagFunc79: TSynWebTokenKind;
    function MLTagFunc80: TSynWebTokenKind;
    function MLTagFunc81: TSynWebTokenKind;
    function MLTagFunc82: TSynWebTokenKind;
    function MLTagFunc84: TSynWebTokenKind;
    function MLTagFunc85: TSynWebTokenKind;
    function MLTagFunc87: TSynWebTokenKind;
    function MLTagFunc89: TSynWebTokenKind;
    function MLTagFunc91: TSynWebTokenKind;
    function MLTagFunc92: TSynWebTokenKind;
    function MLTagFunc93: TSynWebTokenKind;
    function MLTagFunc94: TSynWebTokenKind;
    function MLTagFunc95: TSynWebTokenKind;
    function MLTagFunc106: TSynWebTokenKind;
    function MLTagFunc107: TSynWebTokenKind;
    function MLTagFunc114: TSynWebTokenKind;
    function MLTagFunc121: TSynWebTokenKind;
    function MLTagFunc128: TSynWebTokenKind;

    function MLAttrKeyComp(const ID: Integer): Boolean;
    function MLAttrCheck: TSynWebTokenKind;
    function MLAttrUndef: TSynWebTokenKind;
    function MLAttrFunc13: TSynWebTokenKind;
    function MLAttrFunc15: TSynWebTokenKind;
    function MLAttrFunc23: TSynWebTokenKind;
    function MLAttrFunc26: TSynWebTokenKind;
    function MLAttrFunc27: TSynWebTokenKind;
    function MLAttrFunc30: TSynWebTokenKind;
    function MLAttrFunc31: TSynWebTokenKind;
    function MLAttrFunc32: TSynWebTokenKind;
    function MLAttrFunc33: TSynWebTokenKind;
    function MLAttrFunc34: TSynWebTokenKind;
    function MLAttrFunc35: TSynWebTokenKind;
    function MLAttrFunc37: TSynWebTokenKind;
    function MLAttrFunc38: TSynWebTokenKind;
    function MLAttrFunc39: TSynWebTokenKind;
    function MLAttrFunc40: TSynWebTokenKind;
    function MLAttrFunc42: TSynWebTokenKind;
    function MLAttrFunc43: TSynWebTokenKind;
    function MLAttrFunc45: TSynWebTokenKind;
    function MLAttrFunc46: TSynWebTokenKind;
    function MLAttrFunc47: TSynWebTokenKind;
    function MLAttrFunc48: TSynWebTokenKind;
    function MLAttrFunc49: TSynWebTokenKind;
    function MLAttrFunc50: TSynWebTokenKind;
    function MLAttrFunc52: TSynWebTokenKind;
    function MLAttrFunc53: TSynWebTokenKind;
    function MLAttrFunc54: TSynWebTokenKind;
    function MLAttrFunc55: TSynWebTokenKind;
    function MLAttrFunc56: TSynWebTokenKind;
    function MLAttrFunc57: TSynWebTokenKind;
    function MLAttrFunc58: TSynWebTokenKind;
    function MLAttrFunc59: TSynWebTokenKind;
    function MLAttrFunc60: TSynWebTokenKind;
    function MLAttrFunc61: TSynWebTokenKind;
    function MLAttrFunc62: TSynWebTokenKind;
    function MLAttrFunc63: TSynWebTokenKind;
    function MLAttrFunc64: TSynWebTokenKind;
    function MLAttrFunc65: TSynWebTokenKind;
    function MLAttrFunc66: TSynWebTokenKind;
    function MLAttrFunc67: TSynWebTokenKind;
    function MLAttrFunc68: TSynWebTokenKind;
    function MLAttrFunc69: TSynWebTokenKind;
    function MLAttrFunc70: TSynWebTokenKind;
    function MLAttrFunc71: TSynWebTokenKind;
    function MLAttrFunc72: TSynWebTokenKind;
    function MLAttrFunc73: TSynWebTokenKind;
    function MLAttrFunc74: TSynWebTokenKind;
    function MLAttrFunc75: TSynWebTokenKind;
    function MLAttrFunc77: TSynWebTokenKind;
    function MLAttrFunc78: TSynWebTokenKind;
    function MLAttrFunc79: TSynWebTokenKind;
    function MLAttrFunc80: TSynWebTokenKind;
    function MLAttrFunc81: TSynWebTokenKind;
    function MLAttrFunc82: TSynWebTokenKind;
    function MLAttrFunc83: TSynWebTokenKind;
    function MLAttrFunc85: TSynWebTokenKind;
    function MLAttrFunc87: TSynWebTokenKind;
    function MLAttrFunc88: TSynWebTokenKind;
    function MLAttrFunc91: TSynWebTokenKind;
    function MLAttrFunc93: TSynWebTokenKind;
    function MLAttrFunc94: TSynWebTokenKind;
    function MLAttrFunc96: TSynWebTokenKind;
    function MLAttrFunc97: TSynWebTokenKind;
    function MLAttrFunc98: TSynWebTokenKind;
    function MLAttrFunc101: TSynWebTokenKind;
    function MLAttrFunc102: TSynWebTokenKind;
    function MLAttrFunc104: TSynWebTokenKind;
    function MLAttrFunc105: TSynWebTokenKind;
    function MLAttrFunc106: TSynWebTokenKind;
    function MLAttrFunc107: TSynWebTokenKind;
    function MLAttrFunc108: TSynWebTokenKind;
    function MLAttrFunc109: TSynWebTokenKind;
    function MLAttrFunc110: TSynWebTokenKind;
    function MLAttrFunc111: TSynWebTokenKind;
    function MLAttrFunc113: TSynWebTokenKind;
    function MLAttrFunc114: TSynWebTokenKind;
    function MLAttrFunc117: TSynWebTokenKind;
    function MLAttrFunc119: TSynWebTokenKind;
    function MLAttrFunc122: TSynWebTokenKind;
    function MLAttrFunc126: TSynWebTokenKind;
    function MLAttrFunc127: TSynWebTokenKind;
    function MLAttrFunc132: TSynWebTokenKind;
    function MLAttrFunc139: TSynWebTokenKind;
    function MLAttrFunc143: TSynWebTokenKind;
    function MLAttrFunc147: TSynWebTokenKind;
    function MLAttrFunc154: TSynWebTokenKind;
    function MLAttrFunc155: TSynWebTokenKind;
    function MLAttrFunc157: TSynWebTokenKind;
    function MLAttrFunc158: TSynWebTokenKind;
    function MLAttrFunc160: TSynWebTokenKind;
    function MLAttrFunc162: TSynWebTokenKind;
    function MLAttrFunc176: TSynWebTokenKind;

    function MLSpecialKeyComp(const ID: Integer): Boolean;
    function MLSpecialCheck(AStart, ALen: Integer): Integer;
    function MLSpecialUndef: Boolean;
    function MLSpecialFunc12: Boolean;
    function MLSpecialFunc16: Boolean;
    function MLSpecialFunc17: Boolean;
    function MLSpecialFunc19: Boolean;
    function MLSpecialFunc20: Boolean;
    function MLSpecialFunc22: Boolean;
    function MLSpecialFunc23: Boolean;
    function MLSpecialFunc25: Boolean;
    function MLSpecialFunc26: Boolean;
    function MLSpecialFunc27: Boolean;
    function MLSpecialFunc28: Boolean;
    function MLSpecialFunc30: Boolean;
    function MLSpecialFunc32: Boolean;
    function MLSpecialFunc33: Boolean;
    function MLSpecialFunc34: Boolean;
    function MLSpecialFunc35: Boolean;
    function MLSpecialFunc36: Boolean;
    function MLSpecialFunc38: Boolean;
    function MLSpecialFunc39: Boolean;
    function MLSpecialFunc40: Boolean;
    function MLSpecialFunc41: Boolean;
    function MLSpecialFunc42: Boolean;
    function MLSpecialFunc43: Boolean;
    function MLSpecialFunc44: Boolean;
    function MLSpecialFunc45: Boolean;
    function MLSpecialFunc46: Boolean;
    function MLSpecialFunc47: Boolean;
    function MLSpecialFunc48: Boolean;
    function MLSpecialFunc49: Boolean;
    function MLSpecialFunc50: Boolean;
    function MLSpecialFunc51: Boolean;
    function MLSpecialFunc52: Boolean;
    function MLSpecialFunc53: Boolean;
    function MLSpecialFunc54: Boolean;
    function MLSpecialFunc55: Boolean;
    function MLSpecialFunc56: Boolean;
    function MLSpecialFunc57: Boolean;
    function MLSpecialFunc58: Boolean;
    function MLSpecialFunc59: Boolean;
    function MLSpecialFunc61: Boolean;
    function MLSpecialFunc62: Boolean;
    function MLSpecialFunc63: Boolean;
    function MLSpecialFunc64: Boolean;
    function MLSpecialFunc65: Boolean;
    function MLSpecialFunc66: Boolean;
    function MLSpecialFunc67: Boolean;
    function MLSpecialFunc68: Boolean;
    function MLSpecialFunc69: Boolean;
    function MLSpecialFunc70: Boolean;
    function MLSpecialFunc71: Boolean;
    function MLSpecialFunc72: Boolean;
    function MLSpecialFunc73: Boolean;
    function MLSpecialFunc74: Boolean;
    function MLSpecialFunc75: Boolean;
    function MLSpecialFunc76: Boolean;
    function MLSpecialFunc77: Boolean;
    function MLSpecialFunc78: Boolean;
    function MLSpecialFunc79: Boolean;
    function MLSpecialFunc81: Boolean;
    function MLSpecialFunc83: Boolean;
    function MLSpecialFunc84: Boolean;
    function MLSpecialFunc85: Boolean;
    function MLSpecialFunc86: Boolean;
    function MLSpecialFunc87: Boolean;
    function MLSpecialFunc89: Boolean;
    function MLSpecialFunc90: Boolean;
    function MLSpecialFunc91: Boolean;
    function MLSpecialFunc95: Boolean;
    function MLSpecialFunc106: Boolean;
    function MLSpecialFunc111: Boolean;

    // Css ---------------------------------------------------------------------
    procedure CssMakeMethodTables;
    procedure CssNextBg;
    procedure CssNext;
    procedure CssUpdateBg;
    function CssGetRange: TSynWebCssRangeState;
    procedure CssSetRange(const ARange: TSynWebCssRangeState);
    function CssGetProp: Integer;
    procedure CssSetProp(const AProp: Integer);
    function CssCheckNull(ADo: Boolean = True): Boolean;

    procedure CssSpaceProc;
    procedure CssAtKeywordProc;
    procedure CssSlashProc;
    procedure CssBraceOpenProc;
    procedure CssCurlyBraceOpenProc;
    procedure CssCurlyBraceCloseProc;
    procedure CssChildAnySelectorProc;
    procedure CssAttribProc;
    procedure CssHashProc;
    procedure CssDotProc;
    procedure CssCommaProc;
    procedure CssColonProc;
    procedure CssSemiColonProc;
    procedure CssExclamationProc;
    procedure CssStringProc;
    procedure CssPlusProc;
    procedure CssMinusProc;
    procedure CssNumberProc;
    procedure CssNumberDefProc;
    procedure CssIdentProc;
    function CssIdentStartProc: Boolean;
    function CssCustomStringProc(AShl: Longword; ADo: Boolean = True): Boolean;
    function CssNotWhitespace: Boolean;
    procedure CssSymbolProc;
    procedure CssErrorProc;

    procedure CssRangeRulesetProc;
    procedure CssRangeSelectorAttribProc;
    procedure CssRangeSelectorPseudoProc;
    procedure CssRangeAtKeywordProc;
    procedure CssRangePropProc;
    procedure CssRangePropValProc;
    procedure CssRangePropValStrProc;
    procedure CssRangePropValRgbProc;
    procedure CssRangePropValFuncProc;
    procedure CssRangePropValSpecialProc;
    procedure CssRangePropValImportantProc;
    procedure CssRangePropValUrlProc;
    procedure CssRangePropValRectProc;
    procedure CssRangeCommentProc;

    function CssPropKeyComp(const ID: Integer): Boolean;
    function CssPropCheck: TSynWebTokenKind;
    function CssPropUndef: TSynWebTokenKind;
    function CssPropFunc29: TSynWebTokenKind;
    function CssPropFunc39: TSynWebTokenKind;
    function CssPropFunc40: TSynWebTokenKind;
    function CssPropFunc43: TSynWebTokenKind;
    function CssPropFunc51: TSynWebTokenKind;
    function CssPropFunc52: TSynWebTokenKind;
    function CssPropFunc54: TSynWebTokenKind;
    function CssPropFunc55: TSynWebTokenKind;
    function CssPropFunc56: TSynWebTokenKind;
    function CssPropFunc57: TSynWebTokenKind;
    function CssPropFunc62: TSynWebTokenKind;
    function CssPropFunc63: TSynWebTokenKind;
    function CssPropFunc64: TSynWebTokenKind;
    function CssPropFunc85: TSynWebTokenKind;
    function CssPropFunc86: TSynWebTokenKind;
    function CssPropFunc88: TSynWebTokenKind;
    function CssPropFunc91: TSynWebTokenKind;
    function CssPropFunc93: TSynWebTokenKind;
    function CssPropFunc94: TSynWebTokenKind;
    function CssPropFunc95: TSynWebTokenKind;
    function CssPropFunc96: TSynWebTokenKind;
    function CssPropFunc97: TSynWebTokenKind;
    function CssPropFunc98: TSynWebTokenKind;
    function CssPropFunc100: TSynWebTokenKind;
    function CssPropFunc103: TSynWebTokenKind;
    function CssPropFunc116: TSynWebTokenKind;
    function CssPropFunc117: TSynWebTokenKind;
    function CssPropFunc118: TSynWebTokenKind;
    function CssPropFunc120: TSynWebTokenKind;
    function CssPropFunc131: TSynWebTokenKind;
    function CssPropFunc133: TSynWebTokenKind;
    function CssPropFunc135: TSynWebTokenKind;
    function CssPropFunc136: TSynWebTokenKind;
    function CssPropFunc138: TSynWebTokenKind;
    function CssPropFunc139: TSynWebTokenKind;
    function CssPropFunc140: TSynWebTokenKind;
    function CssPropFunc143: TSynWebTokenKind;
    function CssPropFunc144: TSynWebTokenKind;
    function CssPropFunc147: TSynWebTokenKind;
    function CssPropFunc150: TSynWebTokenKind;
    function CssPropFunc151: TSynWebTokenKind;
    function CssPropFunc152: TSynWebTokenKind;
    function CssPropFunc153: TSynWebTokenKind;
    function CssPropFunc155: TSynWebTokenKind;
    function CssPropFunc158: TSynWebTokenKind;
    function CssPropFunc159: TSynWebTokenKind;
    function CssPropFunc162: TSynWebTokenKind;
    function CssPropFunc163: TSynWebTokenKind;
    function CssPropFunc164: TSynWebTokenKind;
    function CssPropFunc165: TSynWebTokenKind;
    function CssPropFunc167: TSynWebTokenKind;
    function CssPropFunc168: TSynWebTokenKind;
    function CssPropFunc169: TSynWebTokenKind;
    function CssPropFunc171: TSynWebTokenKind;
    function CssPropFunc172: TSynWebTokenKind;
    function CssPropFunc173: TSynWebTokenKind;
    function CssPropFunc174: TSynWebTokenKind;
    function CssPropFunc178: TSynWebTokenKind;
    function CssPropFunc179: TSynWebTokenKind;
    function CssPropFunc181: TSynWebTokenKind;
    function CssPropFunc183: TSynWebTokenKind;
    function CssPropFunc185: TSynWebTokenKind;
    function CssPropFunc187: TSynWebTokenKind;
    function CssPropFunc192: TSynWebTokenKind;
    function CssPropFunc193: TSynWebTokenKind;
    function CssPropFunc197: TSynWebTokenKind;
    function CssPropFunc198: TSynWebTokenKind;
    function CssPropFunc199: TSynWebTokenKind;
    function CssPropFunc201: TSynWebTokenKind;
    function CssPropFunc202: TSynWebTokenKind;
    function CssPropFunc211: TSynWebTokenKind;
    function CssPropFunc215: TSynWebTokenKind;
    function CssPropFunc231: TSynWebTokenKind;
    function CssPropFunc235: TSynWebTokenKind;
    function CssPropFunc239: TSynWebTokenKind;
    function CssPropFunc244: TSynWebTokenKind;
    function CssPropFunc245: TSynWebTokenKind;
    function CssPropFunc251: TSynWebTokenKind;
    function CssPropFunc252: TSynWebTokenKind;
    function CssPropFunc253: TSynWebTokenKind;
    function CssPropFunc262: TSynWebTokenKind;
    function CssPropFunc263: TSynWebTokenKind;
    function CssPropFunc264: TSynWebTokenKind;
    function CssPropFunc270: TSynWebTokenKind;
    function CssPropFunc281: TSynWebTokenKind;
    function CssPropFunc283: TSynWebTokenKind;
    function CssPropFunc286: TSynWebTokenKind;
    function CssPropFunc287: TSynWebTokenKind;
    function CssPropFunc304: TSynWebTokenKind;
    function CssPropFunc334: TSynWebTokenKind;

    function CssValKeyComp(const ID: Integer): Boolean;
    function CssValCheck: TSynWebTokenKind;
    function CssValUndef: TSynWebTokenKind;
    function CssValFunc26: TSynWebTokenKind;
    function CssValFunc27: TSynWebTokenKind;
    function CssValFunc29: TSynWebTokenKind;
    function CssValFunc31: TSynWebTokenKind;
    function CssValFunc32: TSynWebTokenKind;
    function CssValFunc33: TSynWebTokenKind;
    function CssValFunc35: TSynWebTokenKind;
    function CssValFunc36: TSynWebTokenKind;
    function CssValFunc37: TSynWebTokenKind;
    function CssValFunc38: TSynWebTokenKind;
    function CssValFunc39: TSynWebTokenKind;
    function CssValFunc40: TSynWebTokenKind;
    function CssValFunc41: TSynWebTokenKind;
    function CssValFunc42: TSynWebTokenKind;
    function CssValFunc43: TSynWebTokenKind;
    function CssValFunc44: TSynWebTokenKind;
    function CssValFunc45: TSynWebTokenKind;
    function CssValFunc46: TSynWebTokenKind;
    function CssValFunc47: TSynWebTokenKind;
    function CssValFunc48: TSynWebTokenKind;
    function CssValFunc49: TSynWebTokenKind;
    function CssValFunc50: TSynWebTokenKind;
    function CssValFunc51: TSynWebTokenKind;
    function CssValFunc52: TSynWebTokenKind;
    function CssValFunc53: TSynWebTokenKind;
    function CssValFunc54: TSynWebTokenKind;
    function CssValFunc55: TSynWebTokenKind;
    function CssValFunc56: TSynWebTokenKind;
    function CssValFunc57: TSynWebTokenKind;
    function CssValFunc59: TSynWebTokenKind;
    function CssValFunc60: TSynWebTokenKind;
    function CssValFunc61: TSynWebTokenKind;
    function CssValFunc62: TSynWebTokenKind;
    function CssValFunc63: TSynWebTokenKind;
    function CssValFunc65: TSynWebTokenKind;
    function CssValFunc67: TSynWebTokenKind;
    function CssValFunc68: TSynWebTokenKind;
    function CssValFunc69: TSynWebTokenKind;
    function CssValFunc72: TSynWebTokenKind;
    function CssValFunc73: TSynWebTokenKind;
    function CssValFunc75: TSynWebTokenKind;
    function CssValFunc76: TSynWebTokenKind;
    function CssValFunc78: TSynWebTokenKind;
    function CssValFunc79: TSynWebTokenKind;
    function CssValFunc80: TSynWebTokenKind;
    function CssValFunc81: TSynWebTokenKind;
    function CssValFunc82: TSynWebTokenKind;
    function CssValFunc83: TSynWebTokenKind;
    function CssValFunc85: TSynWebTokenKind;
    function CssValFunc86: TSynWebTokenKind;
    function CssValFunc87: TSynWebTokenKind;
    function CssValFunc88: TSynWebTokenKind;
    function CssValFunc92: TSynWebTokenKind;
    function CssValFunc93: TSynWebTokenKind;
    function CssValFunc94: TSynWebTokenKind;
    function CssValFunc95: TSynWebTokenKind;
    function CssValFunc96: TSynWebTokenKind;
    function CssValFunc97: TSynWebTokenKind;
    function CssValFunc100: TSynWebTokenKind;
    function CssValFunc101: TSynWebTokenKind;
    function CssValFunc102: TSynWebTokenKind;
    function CssValFunc104: TSynWebTokenKind;
    function CssValFunc105: TSynWebTokenKind;
    function CssValFunc106: TSynWebTokenKind;
    function CssValFunc108: TSynWebTokenKind;
    function CssValFunc110: TSynWebTokenKind;
    function CssValFunc112: TSynWebTokenKind;
    function CssValFunc114: TSynWebTokenKind;
    function CssValFunc115: TSynWebTokenKind;
    function CssValFunc117: TSynWebTokenKind;
    function CssValFunc118: TSynWebTokenKind;
    function CssValFunc119: TSynWebTokenKind;
    function CssValFunc122: TSynWebTokenKind;
    function CssValFunc125: TSynWebTokenKind;
    function CssValFunc127: TSynWebTokenKind;
    function CssValFunc128: TSynWebTokenKind;
    function CssValFunc129: TSynWebTokenKind;
    function CssValFunc131: TSynWebTokenKind;
    function CssValFunc132: TSynWebTokenKind;
    function CssValFunc134: TSynWebTokenKind;
    function CssValFunc135: TSynWebTokenKind;
    function CssValFunc137: TSynWebTokenKind;
    function CssValFunc139: TSynWebTokenKind;
    function CssValFunc141: TSynWebTokenKind;
    function CssValFunc143: TSynWebTokenKind;
    function CssValFunc144: TSynWebTokenKind;
    function CssValFunc145: TSynWebTokenKind;
    function CssValFunc146: TSynWebTokenKind;
    function CssValFunc148: TSynWebTokenKind;
    function CssValFunc149: TSynWebTokenKind;
    function CssValFunc151: TSynWebTokenKind;
    function CssValFunc152: TSynWebTokenKind;
    function CssValFunc156: TSynWebTokenKind;
    function CssValFunc157: TSynWebTokenKind;
    function CssValFunc158: TSynWebTokenKind;
    function CssValFunc159: TSynWebTokenKind;
    function CssValFunc162: TSynWebTokenKind;
    function CssValFunc165: TSynWebTokenKind;
    function CssValFunc166: TSynWebTokenKind;
    function CssValFunc167: TSynWebTokenKind;
    function CssValFunc170: TSynWebTokenKind;
    function CssValFunc172: TSynWebTokenKind;
    function CssValFunc173: TSynWebTokenKind;
    function CssValFunc175: TSynWebTokenKind;
    function CssValFunc192: TSynWebTokenKind;
    function CssValFunc233: TSynWebTokenKind;
    function CssValFunc234: TSynWebTokenKind;
    function CssValFunc237: TSynWebTokenKind;
    function CssValFunc239: TSynWebTokenKind;
    function CssValFunc249: TSynWebTokenKind;
    function CssValFunc271: TSynWebTokenKind;
    function CssValFunc272: TSynWebTokenKind;

    function CssSpecialKeyComp(const ID: Integer): Boolean;
    function CssSpecialCheck(AStart, ALen: Integer): Integer;
    function CssSpecialUndef: Boolean;
    function CssSpecialFunc16: Boolean;
    function CssSpecialFunc18: Boolean;
    function CssSpecialFunc19: Boolean;
    function CssSpecialFunc23: Boolean;
    function CssSpecialFunc25: Boolean;
    function CssSpecialFunc26: Boolean;
    function CssSpecialFunc29: Boolean;
    function CssSpecialFunc30: Boolean;
    function CssSpecialFunc32: Boolean;
    function CssSpecialFunc34: Boolean;
    function CssSpecialFunc36: Boolean;
    function CssSpecialFunc40: Boolean;
    function CssSpecialFunc42: Boolean;
    function CssSpecialFunc43: Boolean;
    function CssSpecialFunc45: Boolean;
    function CssSpecialFunc46: Boolean;
    function CssSpecialFunc50: Boolean;
    function CssSpecialFunc51: Boolean;
    function CssSpecialFunc56: Boolean;
    function CssSpecialFunc57: Boolean;
    function CssSpecialFunc59: Boolean;
    function CssSpecialFunc60: Boolean;
    function CssSpecialFunc62: Boolean;
    function CssSpecialFunc64: Boolean;
    function CssSpecialFunc65: Boolean;
    function CssSpecialFunc68: Boolean;
    function CssSpecialFunc72: Boolean;
    function CssSpecialFunc74: Boolean;
    function CssSpecialFunc77: Boolean;
    function CssSpecialFunc82: Boolean;
    function CssSpecialFunc88: Boolean;
    function CssSpecialFunc91: Boolean;
    function CssSpecialFunc108: Boolean;
    function CssSpecialFunc125: Boolean;
    function CssSpecialFunc126: Boolean;
    function CssSpecialFunc146: Boolean;
    function CssSpecialFunc150: Boolean;
    function CssSpecialFunc190: Boolean;

    // ECMAScript --------------------------------------------------------------
    procedure EsMakeMethodTables;
    procedure EsNext;
    function EsGetRange: TSynWebEsRangeState;
    procedure EsSetRange(const ARange: TSynWebEsRangeState);
    function EsCheckNull(ADo: Boolean = True): Boolean;

    procedure EsSpaceProc;
    procedure EsSlashProc;
    procedure EsLowerProc;
    procedure EsEqualNotProc;
    procedure EsGreaterProc;
    procedure EsAndProc;
    procedure EsPlusProc;
    procedure EsMinusProc;
    procedure EsOrProc;
    procedure EsMulModXorProc;
    procedure EsNumberProc;
    procedure EsString34Proc;
    procedure EsString39Proc;
    procedure EsSymbolProc;
    procedure EsIdentProc;
    procedure EsErrorProc;

    procedure EsRangeDefaultProc;
    procedure EsRangeCommentProc;
    procedure EsRangeCommentMultiProc;
    procedure EsRangeString34Proc;
    procedure EsRangeString39Proc;

    function EsKeywordComp(const ID: Integer): Boolean;
    function EsIdentCheck: TSynWebTokenKind;
    function EsKeywordIdent: TSynWebTokenKind;
    function EsKeywordFunc15: TSynWebTokenKind;
    function EsKeywordFunc19: TSynWebTokenKind;
    function EsKeywordFunc23: TSynWebTokenKind;
    function EsKeywordFunc28: TSynWebTokenKind;
    function EsKeywordFunc30: TSynWebTokenKind;
    function EsKeywordFunc35: TSynWebTokenKind;
    function EsKeywordFunc37: TSynWebTokenKind;
    function EsKeywordFunc39: TSynWebTokenKind;
    function EsKeywordFunc41: TSynWebTokenKind;
    function EsKeywordFunc42: TSynWebTokenKind;
    function EsKeywordFunc43: TSynWebTokenKind;
    function EsKeywordFunc44: TSynWebTokenKind;
    function EsKeywordFunc48: TSynWebTokenKind;
    function EsKeywordFunc50: TSynWebTokenKind;
    function EsKeywordFunc51: TSynWebTokenKind;
    function EsKeywordFunc52: TSynWebTokenKind;
    function EsKeywordFunc53: TSynWebTokenKind;
    function EsKeywordFunc54: TSynWebTokenKind;
    function EsKeywordFunc56: TSynWebTokenKind;
    function EsKeywordFunc57: TSynWebTokenKind;
    function EsKeywordFunc59: TSynWebTokenKind;
    function EsKeywordFunc60: TSynWebTokenKind;
    function EsKeywordFunc63: TSynWebTokenKind;
    function EsKeywordFunc64: TSynWebTokenKind;
    function EsKeywordFunc69: TSynWebTokenKind;
    function EsKeywordFunc71: TSynWebTokenKind;
    function EsKeywordFunc72: TSynWebTokenKind;
    function EsKeywordFunc79: TSynWebTokenKind;
    function EsKeywordFunc80: TSynWebTokenKind;
    function EsKeywordFunc81: TSynWebTokenKind;
    function EsKeywordFunc82: TSynWebTokenKind;
    function EsKeywordFunc84: TSynWebTokenKind;
    function EsKeywordFunc87: TSynWebTokenKind;
    function EsKeywordFunc91: TSynWebTokenKind;
    function EsKeywordFunc96: TSynWebTokenKind;
    function EsKeywordFunc98: TSynWebTokenKind;
    function EsKeywordFunc101: TSynWebTokenKind;
    function EsKeywordFunc102: TSynWebTokenKind;
    function EsKeywordFunc103: TSynWebTokenKind;
    function EsKeywordFunc106: TSynWebTokenKind;
    function EsKeywordFunc120: TSynWebTokenKind;
    function EsKeywordFunc126: TSynWebTokenKind;
    function EsKeywordFunc160: TSynWebTokenKind;

    // Php ---------------------------------------------------------------------
    procedure PhpMakeMethodTables;
    procedure PhpNext;
    procedure PhpCliNext;
    function PhpGetRange: TSynWebPhpRangeState;
    procedure PhpSetRange(const ARange: TSynWebPhpRangeState);
    procedure PhpSetSymbolId(ASymbolId: Integer);

    function PhpCheckBegin(ABegin: Boolean = True): Boolean;
    procedure PhpBegin(ATagKind: TSynWebPhpOpenTag);
    procedure PhpEnd(AMLTag: Boolean);

    procedure PhpSpaceProc;
    procedure PhpQuestionProc;
    procedure PhpNumberProc;
    function PhpCheckNumberProc: Boolean;
    procedure PhpString34Proc;
    procedure PhpString39Proc;
    procedure PhpStringShellProc;
    procedure PhpAndProc;
    procedure PhpOrProc;
    procedure PhpAtSymbolProc;
    procedure PhpEqualProc;
    procedure PhpGreaterProc;
    procedure PhpLowerProc;
    procedure PhpPlusProc;
    procedure PhpMinusProc;
    procedure PhpMulProc;
    procedure PhpDivProc;
    procedure PhpModProc;
    procedure PhpXorProc;
    procedure PhpSlashProc;
    procedure PhpPercentProc;
    procedure PhpHashProc;
    procedure PhpNotProc;
    procedure PhpDotProc;
    procedure PhpColonProc;
    procedure PhpParentheseOpenProc;
    procedure PhpParentheseCloseProc;
    procedure PhpBoxBracketOpenProc;
    procedure PhpBoxBracketCloseProc;
    procedure PhpBraceOpenProc;
    procedure PhpBraceCloseProc;
    procedure PhpTildeProc;
    procedure PhpCommaProc;
    procedure PhpSemiColonProc;
    procedure PhpVarProc;
    procedure PhpIdentProc;
    procedure PhpErrorProc;
    function PhpDoStringDouble(AIsHeredoc: Boolean = False;
      ARangeChar: Boolean = True): Boolean;

    procedure PhpSubProcProc;
    procedure PhpRangeDefaultProc;
    procedure PhpRangeCommentProc;
    procedure PhpRangeDocCommentProc;
    procedure PhpRangeString34Proc;
    procedure PhpRangeString39Proc;
    procedure PhpRangeStringShellProc;
    procedure PhpRangeHeredocProc;

    function PhpKeywordComp(const ID: Integer): Boolean;
    function PhpConstComp: Boolean;
    function PhpFunctionComp(const ID: Integer): Boolean;

    function PhpIdentCheck: TSynWebTokenKind;
    function PhpKeywordIdent: TSynWebTokenKind;
    function PhpKeywordFunc14: TSynWebTokenKind;
    function PhpKeywordFunc15: TSynWebTokenKind;
    function PhpKeywordFunc16: TSynWebTokenKind;
    function PhpKeywordFunc17: TSynWebTokenKind;
    function PhpKeywordFunc18: TSynWebTokenKind;
    function PhpKeywordFunc19: TSynWebTokenKind;
    function PhpKeywordFunc20: TSynWebTokenKind;
    function PhpKeywordFunc22: TSynWebTokenKind;
    function PhpKeywordFunc23: TSynWebTokenKind;
    function PhpKeywordFunc25: TSynWebTokenKind;
    function PhpKeywordFunc28: TSynWebTokenKind;
    function PhpKeywordFunc29: TSynWebTokenKind;
    function PhpKeywordFunc30: TSynWebTokenKind;
    function PhpKeywordFunc31: TSynWebTokenKind;
    function PhpKeywordFunc32: TSynWebTokenKind;
    function PhpKeywordFunc33: TSynWebTokenKind;
    function PhpKeywordFunc34: TSynWebTokenKind;
    function PhpKeywordFunc35: TSynWebTokenKind;
    function PhpKeywordFunc36: TSynWebTokenKind;
    function PhpKeywordFunc37: TSynWebTokenKind;
    function PhpKeywordFunc38: TSynWebTokenKind;
    function PhpKeywordFunc39: TSynWebTokenKind;
    function PhpKeywordFunc40: TSynWebTokenKind;
    function PhpKeywordFunc41: TSynWebTokenKind;
    function PhpKeywordFunc42: TSynWebTokenKind;
    function PhpKeywordFunc43: TSynWebTokenKind;
    function PhpKeywordFunc44: TSynWebTokenKind;
    function PhpKeywordFunc45: TSynWebTokenKind;
    function PhpKeywordFunc46: TSynWebTokenKind;
    function PhpKeywordFunc47: TSynWebTokenKind;
    function PhpKeywordFunc48: TSynWebTokenKind;
    function PhpKeywordFunc49: TSynWebTokenKind;
    function PhpKeywordFunc50: TSynWebTokenKind;
    function PhpKeywordFunc51: TSynWebTokenKind;
    function PhpKeywordFunc52: TSynWebTokenKind;
    function PhpKeywordFunc54: TSynWebTokenKind;
    function PhpKeywordFunc55: TSynWebTokenKind;
    function PhpKeywordFunc56: TSynWebTokenKind;
    function PhpKeywordFunc57: TSynWebTokenKind;
    function PhpKeywordFunc58: TSynWebTokenKind;
    function PhpKeywordFunc59: TSynWebTokenKind;
    function PhpKeywordFunc60: TSynWebTokenKind;
    function PhpKeywordFunc61: TSynWebTokenKind;
    function PhpKeywordFunc62: TSynWebTokenKind;
    function PhpKeywordFunc63: TSynWebTokenKind;
    function PhpKeywordFunc64: TSynWebTokenKind;
    function PhpKeywordFunc65: TSynWebTokenKind;
    function PhpKeywordFunc66: TSynWebTokenKind;
    function PhpKeywordFunc67: TSynWebTokenKind;
    function PhpKeywordFunc68: TSynWebTokenKind;
    function PhpKeywordFunc69: TSynWebTokenKind;
    function PhpKeywordFunc70: TSynWebTokenKind;
    function PhpKeywordFunc71: TSynWebTokenKind;
    function PhpKeywordFunc72: TSynWebTokenKind;
    function PhpKeywordFunc73: TSynWebTokenKind;
    function PhpKeywordFunc74: TSynWebTokenKind;
    function PhpKeywordFunc75: TSynWebTokenKind;
    function PhpKeywordFunc76: TSynWebTokenKind;
    function PhpKeywordFunc77: TSynWebTokenKind;
    function PhpKeywordFunc78: TSynWebTokenKind;
    function PhpKeywordFunc79: TSynWebTokenKind;
    function PhpKeywordFunc80: TSynWebTokenKind;
    function PhpKeywordFunc81: TSynWebTokenKind;
    function PhpKeywordFunc82: TSynWebTokenKind;
    function PhpKeywordFunc83: TSynWebTokenKind;
    function PhpKeywordFunc84: TSynWebTokenKind;
    function PhpKeywordFunc85: TSynWebTokenKind;
    function PhpKeywordFunc86: TSynWebTokenKind;
    function PhpKeywordFunc87: TSynWebTokenKind;
    function PhpKeywordFunc88: TSynWebTokenKind;
    function PhpKeywordFunc89: TSynWebTokenKind;
    function PhpKeywordFunc90: TSynWebTokenKind;
    function PhpKeywordFunc91: TSynWebTokenKind;
    function PhpKeywordFunc92: TSynWebTokenKind;
    function PhpKeywordFunc93: TSynWebTokenKind;
    function PhpKeywordFunc94: TSynWebTokenKind;
    function PhpKeywordFunc95: TSynWebTokenKind;
    function PhpKeywordFunc96: TSynWebTokenKind;
    function PhpKeywordFunc97: TSynWebTokenKind;
    function PhpKeywordFunc98: TSynWebTokenKind;
    function PhpKeywordFunc99: TSynWebTokenKind;
    function PhpKeywordFunc100: TSynWebTokenKind;
    function PhpKeywordFunc101: TSynWebTokenKind;
    function PhpKeywordFunc102: TSynWebTokenKind;
    function PhpKeywordFunc103: TSynWebTokenKind;
    function PhpKeywordFunc104: TSynWebTokenKind;
    function PhpKeywordFunc105: TSynWebTokenKind;
    function PhpKeywordFunc106: TSynWebTokenKind;
    function PhpKeywordFunc107: TSynWebTokenKind;
    function PhpKeywordFunc108: TSynWebTokenKind;
    function PhpKeywordFunc109: TSynWebTokenKind;
    function PhpKeywordFunc110: TSynWebTokenKind;
    function PhpKeywordFunc111: TSynWebTokenKind;
    function PhpKeywordFunc112: TSynWebTokenKind;
    function PhpKeywordFunc113: TSynWebTokenKind;
    function PhpKeywordFunc114: TSynWebTokenKind;
    function PhpKeywordFunc115: TSynWebTokenKind;
    function PhpKeywordFunc116: TSynWebTokenKind;
    function PhpKeywordFunc117: TSynWebTokenKind;
    function PhpKeywordFunc118: TSynWebTokenKind;
    function PhpKeywordFunc119: TSynWebTokenKind;
    function PhpKeywordFunc120: TSynWebTokenKind;
    function PhpKeywordFunc121: TSynWebTokenKind;
    function PhpKeywordFunc122: TSynWebTokenKind;
    function PhpKeywordFunc123: TSynWebTokenKind;
    function PhpKeywordFunc124: TSynWebTokenKind;
    function PhpKeywordFunc125: TSynWebTokenKind;
    function PhpKeywordFunc126: TSynWebTokenKind;
    function PhpKeywordFunc127: TSynWebTokenKind;
    function PhpKeywordFunc128: TSynWebTokenKind;
    function PhpKeywordFunc129: TSynWebTokenKind;
    function PhpKeywordFunc130: TSynWebTokenKind;
    function PhpKeywordFunc131: TSynWebTokenKind;
    function PhpKeywordFunc132: TSynWebTokenKind;
    function PhpKeywordFunc133: TSynWebTokenKind;
    function PhpKeywordFunc134: TSynWebTokenKind;
    function PhpKeywordFunc135: TSynWebTokenKind;
    function PhpKeywordFunc136: TSynWebTokenKind;
    function PhpKeywordFunc137: TSynWebTokenKind;
    function PhpKeywordFunc138: TSynWebTokenKind;
    function PhpKeywordFunc139: TSynWebTokenKind;
    function PhpKeywordFunc140: TSynWebTokenKind;
    function PhpKeywordFunc141: TSynWebTokenKind;
    function PhpKeywordFunc142: TSynWebTokenKind;
    function PhpKeywordFunc143: TSynWebTokenKind;
    function PhpKeywordFunc144: TSynWebTokenKind;
    function PhpKeywordFunc145: TSynWebTokenKind;
    function PhpKeywordFunc146: TSynWebTokenKind;
    function PhpKeywordFunc147: TSynWebTokenKind;
    function PhpKeywordFunc148: TSynWebTokenKind;
    function PhpKeywordFunc149: TSynWebTokenKind;
    function PhpKeywordFunc150: TSynWebTokenKind;
    function PhpKeywordFunc151: TSynWebTokenKind;
    function PhpKeywordFunc152: TSynWebTokenKind;
    function PhpKeywordFunc153: TSynWebTokenKind;
    function PhpKeywordFunc154: TSynWebTokenKind;
    function PhpKeywordFunc155: TSynWebTokenKind;
    function PhpKeywordFunc156: TSynWebTokenKind;
    function PhpKeywordFunc157: TSynWebTokenKind;
    function PhpKeywordFunc158: TSynWebTokenKind;
    function PhpKeywordFunc159: TSynWebTokenKind;
    function PhpKeywordFunc160: TSynWebTokenKind;
    function PhpKeywordFunc161: TSynWebTokenKind;
    function PhpKeywordFunc162: TSynWebTokenKind;
    function PhpKeywordFunc163: TSynWebTokenKind;
    function PhpKeywordFunc164: TSynWebTokenKind;
    function PhpKeywordFunc165: TSynWebTokenKind;
    function PhpKeywordFunc166: TSynWebTokenKind;
    function PhpKeywordFunc167: TSynWebTokenKind;
    function PhpKeywordFunc168: TSynWebTokenKind;
    function PhpKeywordFunc169: TSynWebTokenKind;
    function PhpKeywordFunc170: TSynWebTokenKind;
    function PhpKeywordFunc171: TSynWebTokenKind;
    function PhpKeywordFunc172: TSynWebTokenKind;
    function PhpKeywordFunc173: TSynWebTokenKind;
    function PhpKeywordFunc174: TSynWebTokenKind;
    function PhpKeywordFunc175: TSynWebTokenKind;
    function PhpKeywordFunc176: TSynWebTokenKind;
    function PhpKeywordFunc177: TSynWebTokenKind;
    function PhpKeywordFunc178: TSynWebTokenKind;
    function PhpKeywordFunc179: TSynWebTokenKind;
    function PhpKeywordFunc180: TSynWebTokenKind;
    function PhpKeywordFunc181: TSynWebTokenKind;
    function PhpKeywordFunc182: TSynWebTokenKind;
    function PhpKeywordFunc183: TSynWebTokenKind;
    function PhpKeywordFunc184: TSynWebTokenKind;
    function PhpKeywordFunc185: TSynWebTokenKind;
    function PhpKeywordFunc186: TSynWebTokenKind;
    function PhpKeywordFunc187: TSynWebTokenKind;
    function PhpKeywordFunc188: TSynWebTokenKind;
    function PhpKeywordFunc189: TSynWebTokenKind;
    function PhpKeywordFunc190: TSynWebTokenKind;
    function PhpKeywordFunc191: TSynWebTokenKind;
    function PhpKeywordFunc192: TSynWebTokenKind;
    function PhpKeywordFunc193: TSynWebTokenKind;
    function PhpKeywordFunc194: TSynWebTokenKind;
    function PhpKeywordFunc195: TSynWebTokenKind;
    function PhpKeywordFunc196: TSynWebTokenKind;
    function PhpKeywordFunc197: TSynWebTokenKind;
    function PhpKeywordFunc198: TSynWebTokenKind;
    function PhpKeywordFunc199: TSynWebTokenKind;
    function PhpKeywordFunc200: TSynWebTokenKind;
    function PhpKeywordFunc201: TSynWebTokenKind;
    function PhpKeywordFunc202: TSynWebTokenKind;
    function PhpKeywordFunc203: TSynWebTokenKind;
    function PhpKeywordFunc204: TSynWebTokenKind;
    function PhpKeywordFunc205: TSynWebTokenKind;
    function PhpKeywordFunc206: TSynWebTokenKind;
    function PhpKeywordFunc207: TSynWebTokenKind;
    function PhpKeywordFunc208: TSynWebTokenKind;
    function PhpKeywordFunc209: TSynWebTokenKind;
    function PhpKeywordFunc210: TSynWebTokenKind;
    function PhpKeywordFunc211: TSynWebTokenKind;
    function PhpKeywordFunc212: TSynWebTokenKind;
    function PhpKeywordFunc213: TSynWebTokenKind;
    function PhpKeywordFunc214: TSynWebTokenKind;
    function PhpKeywordFunc215: TSynWebTokenKind;
    function PhpKeywordFunc216: TSynWebTokenKind;
    function PhpKeywordFunc217: TSynWebTokenKind;
    function PhpKeywordFunc218: TSynWebTokenKind;
    function PhpKeywordFunc219: TSynWebTokenKind;
    function PhpKeywordFunc220: TSynWebTokenKind;
    function PhpKeywordFunc221: TSynWebTokenKind;
    function PhpKeywordFunc222: TSynWebTokenKind;
    function PhpKeywordFunc223: TSynWebTokenKind;
    function PhpKeywordFunc224: TSynWebTokenKind;
    function PhpKeywordFunc225: TSynWebTokenKind;
    function PhpKeywordFunc226: TSynWebTokenKind;
    function PhpKeywordFunc227: TSynWebTokenKind;
    function PhpKeywordFunc228: TSynWebTokenKind;
    function PhpKeywordFunc229: TSynWebTokenKind;
    function PhpKeywordFunc230: TSynWebTokenKind;
    function PhpKeywordFunc231: TSynWebTokenKind;
    function PhpKeywordFunc232: TSynWebTokenKind;
    function PhpKeywordFunc233: TSynWebTokenKind;
    function PhpKeywordFunc234: TSynWebTokenKind;
    function PhpKeywordFunc235: TSynWebTokenKind;
    function PhpKeywordFunc236: TSynWebTokenKind;
    function PhpKeywordFunc237: TSynWebTokenKind;
    function PhpKeywordFunc238: TSynWebTokenKind;
    function PhpKeywordFunc239: TSynWebTokenKind;
    function PhpKeywordFunc240: TSynWebTokenKind;
    function PhpKeywordFunc241: TSynWebTokenKind;
    function PhpKeywordFunc242: TSynWebTokenKind;
    function PhpKeywordFunc243: TSynWebTokenKind;
    function PhpKeywordFunc244: TSynWebTokenKind;
    function PhpKeywordFunc245: TSynWebTokenKind;
    function PhpKeywordFunc246: TSynWebTokenKind;
    function PhpKeywordFunc247: TSynWebTokenKind;
    function PhpKeywordFunc248: TSynWebTokenKind;
    function PhpKeywordFunc249: TSynWebTokenKind;
    function PhpKeywordFunc250: TSynWebTokenKind;
    function PhpKeywordFunc251: TSynWebTokenKind;
    function PhpKeywordFunc252: TSynWebTokenKind;
    function PhpKeywordFunc253: TSynWebTokenKind;
    function PhpKeywordFunc254: TSynWebTokenKind;
    function PhpKeywordFunc255: TSynWebTokenKind;
    function PhpKeywordFunc256: TSynWebTokenKind;
    function PhpKeywordFunc257: TSynWebTokenKind;
    function PhpKeywordFunc258: TSynWebTokenKind;
    function PhpKeywordFunc259: TSynWebTokenKind;
    function PhpKeywordFunc260: TSynWebTokenKind;
    function PhpKeywordFunc261: TSynWebTokenKind;
    function PhpKeywordFunc262: TSynWebTokenKind;
    function PhpKeywordFunc263: TSynWebTokenKind;
    function PhpKeywordFunc264: TSynWebTokenKind;
    function PhpKeywordFunc265: TSynWebTokenKind;
    function PhpKeywordFunc266: TSynWebTokenKind;
    function PhpKeywordFunc267: TSynWebTokenKind;
    function PhpKeywordFunc268: TSynWebTokenKind;
    function PhpKeywordFunc269: TSynWebTokenKind;
    function PhpKeywordFunc270: TSynWebTokenKind;
    function PhpKeywordFunc271: TSynWebTokenKind;
    function PhpKeywordFunc272: TSynWebTokenKind;
    function PhpKeywordFunc273: TSynWebTokenKind;
    function PhpKeywordFunc274: TSynWebTokenKind;
    function PhpKeywordFunc275: TSynWebTokenKind;
    function PhpKeywordFunc276: TSynWebTokenKind;
    function PhpKeywordFunc277: TSynWebTokenKind;
    function PhpKeywordFunc278: TSynWebTokenKind;
    function PhpKeywordFunc279: TSynWebTokenKind;
    function PhpKeywordFunc280: TSynWebTokenKind;
    function PhpKeywordFunc281: TSynWebTokenKind;
    function PhpKeywordFunc282: TSynWebTokenKind;
    function PhpKeywordFunc283: TSynWebTokenKind;
    function PhpKeywordFunc284: TSynWebTokenKind;
    function PhpKeywordFunc285: TSynWebTokenKind;
    function PhpKeywordFunc286: TSynWebTokenKind;
    function PhpKeywordFunc287: TSynWebTokenKind;
    function PhpKeywordFunc288: TSynWebTokenKind;
    function PhpKeywordFunc289: TSynWebTokenKind;
    function PhpKeywordFunc290: TSynWebTokenKind;
    function PhpKeywordFunc291: TSynWebTokenKind;
    function PhpKeywordFunc292: TSynWebTokenKind;
    function PhpKeywordFunc293: TSynWebTokenKind;
    function PhpKeywordFunc294: TSynWebTokenKind;
    function PhpKeywordFunc295: TSynWebTokenKind;
    function PhpKeywordFunc296: TSynWebTokenKind;
    function PhpKeywordFunc297: TSynWebTokenKind;
    function PhpKeywordFunc298: TSynWebTokenKind;
    function PhpKeywordFunc299: TSynWebTokenKind;
    function PhpKeywordFunc300: TSynWebTokenKind;
    function PhpKeywordFunc301: TSynWebTokenKind;
    function PhpKeywordFunc302: TSynWebTokenKind;
    function PhpKeywordFunc303: TSynWebTokenKind;
    function PhpKeywordFunc304: TSynWebTokenKind;
    function PhpKeywordFunc305: TSynWebTokenKind;
    function PhpKeywordFunc306: TSynWebTokenKind;
    function PhpKeywordFunc307: TSynWebTokenKind;
    function PhpKeywordFunc308: TSynWebTokenKind;
    function PhpKeywordFunc309: TSynWebTokenKind;
    function PhpKeywordFunc310: TSynWebTokenKind;
    function PhpKeywordFunc311: TSynWebTokenKind;
    function PhpKeywordFunc312: TSynWebTokenKind;
    function PhpKeywordFunc313: TSynWebTokenKind;
    function PhpKeywordFunc314: TSynWebTokenKind;
    function PhpKeywordFunc315: TSynWebTokenKind;
    function PhpKeywordFunc316: TSynWebTokenKind;
    function PhpKeywordFunc317: TSynWebTokenKind;
    function PhpKeywordFunc318: TSynWebTokenKind;
    function PhpKeywordFunc319: TSynWebTokenKind;
    function PhpKeywordFunc320: TSynWebTokenKind;
    function PhpKeywordFunc321: TSynWebTokenKind;
    function PhpKeywordFunc322: TSynWebTokenKind;
    function PhpKeywordFunc323: TSynWebTokenKind;
    function PhpKeywordFunc324: TSynWebTokenKind;
    function PhpKeywordFunc325: TSynWebTokenKind;
    function PhpKeywordFunc326: TSynWebTokenKind;
    function PhpKeywordFunc327: TSynWebTokenKind;
    function PhpKeywordFunc328: TSynWebTokenKind;
    function PhpKeywordFunc329: TSynWebTokenKind;
    function PhpKeywordFunc330: TSynWebTokenKind;
    function PhpKeywordFunc331: TSynWebTokenKind;
    function PhpKeywordFunc332: TSynWebTokenKind;
    function PhpKeywordFunc333: TSynWebTokenKind;
    function PhpKeywordFunc334: TSynWebTokenKind;
    function PhpKeywordFunc335: TSynWebTokenKind;
    function PhpKeywordFunc336: TSynWebTokenKind;
    function PhpKeywordFunc337: TSynWebTokenKind;
    function PhpKeywordFunc338: TSynWebTokenKind;
    function PhpKeywordFunc339: TSynWebTokenKind;
    function PhpKeywordFunc340: TSynWebTokenKind;
    function PhpKeywordFunc341: TSynWebTokenKind;
    function PhpKeywordFunc342: TSynWebTokenKind;
    function PhpKeywordFunc343: TSynWebTokenKind;
    function PhpKeywordFunc344: TSynWebTokenKind;
    function PhpKeywordFunc345: TSynWebTokenKind;
    function PhpKeywordFunc346: TSynWebTokenKind;
    function PhpKeywordFunc347: TSynWebTokenKind;
    function PhpKeywordFunc348: TSynWebTokenKind;
    function PhpKeywordFunc349: TSynWebTokenKind;
    function PhpKeywordFunc350: TSynWebTokenKind;
    function PhpKeywordFunc351: TSynWebTokenKind;
    function PhpKeywordFunc352: TSynWebTokenKind;
    function PhpKeywordFunc353: TSynWebTokenKind;
    function PhpKeywordFunc354: TSynWebTokenKind;
    function PhpKeywordFunc355: TSynWebTokenKind;
    function PhpKeywordFunc356: TSynWebTokenKind;
    function PhpKeywordFunc357: TSynWebTokenKind;
    function PhpKeywordFunc358: TSynWebTokenKind;
    function PhpKeywordFunc359: TSynWebTokenKind;
    function PhpKeywordFunc360: TSynWebTokenKind;
    function PhpKeywordFunc361: TSynWebTokenKind;
    function PhpKeywordFunc362: TSynWebTokenKind;
    function PhpKeywordFunc363: TSynWebTokenKind;
    function PhpKeywordFunc364: TSynWebTokenKind;
    function PhpKeywordFunc365: TSynWebTokenKind;
    function PhpKeywordFunc366: TSynWebTokenKind;
    function PhpKeywordFunc367: TSynWebTokenKind;
    function PhpKeywordFunc368: TSynWebTokenKind;
    function PhpKeywordFunc369: TSynWebTokenKind;
    function PhpKeywordFunc370: TSynWebTokenKind;
    function PhpKeywordFunc371: TSynWebTokenKind;
    function PhpKeywordFunc372: TSynWebTokenKind;
    function PhpKeywordFunc373: TSynWebTokenKind;
    function PhpKeywordFunc374: TSynWebTokenKind;
    function PhpKeywordFunc376: TSynWebTokenKind;
    function PhpKeywordFunc377: TSynWebTokenKind;
    function PhpKeywordFunc378: TSynWebTokenKind;
    function PhpKeywordFunc379: TSynWebTokenKind;
    function PhpKeywordFunc380: TSynWebTokenKind;
    function PhpKeywordFunc381: TSynWebTokenKind;
    function PhpKeywordFunc382: TSynWebTokenKind;
    function PhpKeywordFunc383: TSynWebTokenKind;
    function PhpKeywordFunc384: TSynWebTokenKind;
    function PhpKeywordFunc385: TSynWebTokenKind;
    function PhpKeywordFunc386: TSynWebTokenKind;
    function PhpKeywordFunc387: TSynWebTokenKind;
    function PhpKeywordFunc388: TSynWebTokenKind;
    function PhpKeywordFunc389: TSynWebTokenKind;
    function PhpKeywordFunc390: TSynWebTokenKind;
    function PhpKeywordFunc391: TSynWebTokenKind;
    function PhpKeywordFunc392: TSynWebTokenKind;
    function PhpKeywordFunc393: TSynWebTokenKind;
    function PhpKeywordFunc394: TSynWebTokenKind;
    function PhpKeywordFunc396: TSynWebTokenKind;
    function PhpKeywordFunc397: TSynWebTokenKind;
    function PhpKeywordFunc398: TSynWebTokenKind;
    function PhpKeywordFunc399: TSynWebTokenKind;
    function PhpKeywordFunc400: TSynWebTokenKind;
    function PhpKeywordFunc401: TSynWebTokenKind;
    function PhpKeywordFunc402: TSynWebTokenKind;
    function PhpKeywordFunc403: TSynWebTokenKind;
    function PhpKeywordFunc404: TSynWebTokenKind;
    function PhpKeywordFunc405: TSynWebTokenKind;
    function PhpKeywordFunc406: TSynWebTokenKind;
    function PhpKeywordFunc407: TSynWebTokenKind;
    function PhpKeywordFunc408: TSynWebTokenKind;
    function PhpKeywordFunc409: TSynWebTokenKind;
    function PhpKeywordFunc410: TSynWebTokenKind;
    function PhpKeywordFunc412: TSynWebTokenKind;
    function PhpKeywordFunc413: TSynWebTokenKind;
    function PhpKeywordFunc414: TSynWebTokenKind;
    function PhpKeywordFunc415: TSynWebTokenKind;
    function PhpKeywordFunc417: TSynWebTokenKind;
    function PhpKeywordFunc418: TSynWebTokenKind;
    function PhpKeywordFunc419: TSynWebTokenKind;
    function PhpKeywordFunc420: TSynWebTokenKind;
    function PhpKeywordFunc421: TSynWebTokenKind;
    function PhpKeywordFunc423: TSynWebTokenKind;
    function PhpKeywordFunc424: TSynWebTokenKind;
    function PhpKeywordFunc425: TSynWebTokenKind;
    function PhpKeywordFunc426: TSynWebTokenKind;
    function PhpKeywordFunc427: TSynWebTokenKind;
    function PhpKeywordFunc428: TSynWebTokenKind;
    function PhpKeywordFunc430: TSynWebTokenKind;
    function PhpKeywordFunc431: TSynWebTokenKind;
    function PhpKeywordFunc433: TSynWebTokenKind;
    function PhpKeywordFunc434: TSynWebTokenKind;
    function PhpKeywordFunc435: TSynWebTokenKind;
    function PhpKeywordFunc436: TSynWebTokenKind;
    function PhpKeywordFunc437: TSynWebTokenKind;
    function PhpKeywordFunc438: TSynWebTokenKind;
    function PhpKeywordFunc439: TSynWebTokenKind;
    function PhpKeywordFunc440: TSynWebTokenKind;
    function PhpKeywordFunc441: TSynWebTokenKind;
    function PhpKeywordFunc442: TSynWebTokenKind;
    function PhpKeywordFunc447: TSynWebTokenKind;
    function PhpKeywordFunc449: TSynWebTokenKind;
    function PhpKeywordFunc450: TSynWebTokenKind;
    function PhpKeywordFunc452: TSynWebTokenKind;
    function PhpKeywordFunc453: TSynWebTokenKind;
    function PhpKeywordFunc454: TSynWebTokenKind;
    function PhpKeywordFunc455: TSynWebTokenKind;
    function PhpKeywordFunc457: TSynWebTokenKind;
    function PhpKeywordFunc458: TSynWebTokenKind;
    function PhpKeywordFunc459: TSynWebTokenKind;
    function PhpKeywordFunc460: TSynWebTokenKind;
    function PhpKeywordFunc463: TSynWebTokenKind;
    function PhpKeywordFunc464: TSynWebTokenKind;
    function PhpKeywordFunc468: TSynWebTokenKind;
    function PhpKeywordFunc470: TSynWebTokenKind;
    function PhpKeywordFunc472: TSynWebTokenKind;
    function PhpKeywordFunc474: TSynWebTokenKind;
    function PhpKeywordFunc477: TSynWebTokenKind;
    function PhpKeywordFunc478: TSynWebTokenKind;
    function PhpKeywordFunc479: TSynWebTokenKind;
    function PhpKeywordFunc480: TSynWebTokenKind;
    function PhpKeywordFunc483: TSynWebTokenKind;
    function PhpKeywordFunc487: TSynWebTokenKind;
    function PhpKeywordFunc503: TSynWebTokenKind;
    function PhpKeywordFunc505: TSynWebTokenKind;
    function PhpKeywordFunc513: TSynWebTokenKind;
    function PhpKeywordFunc515: TSynWebTokenKind;
    function PhpKeywordFunc517: TSynWebTokenKind;
    function PhpKeywordFunc519: TSynWebTokenKind;
    function PhpKeywordFunc521: TSynWebTokenKind;
    function PhpKeywordFunc529: TSynWebTokenKind;
    function PhpKeywordFunc532: TSynWebTokenKind;
    function PhpKeywordFunc533: TSynWebTokenKind;
    function PhpKeywordFunc539: TSynWebTokenKind;
    function PhpKeywordFunc541: TSynWebTokenKind;
    function PhpKeywordFunc549: TSynWebTokenKind;
    function PhpKeywordFunc553: TSynWebTokenKind;
    function PhpKeywordFunc555: TSynWebTokenKind;
    function PhpKeywordFunc559: TSynWebTokenKind;
    function PhpKeywordFunc561: TSynWebTokenKind;
    function PhpKeywordFunc568: TSynWebTokenKind;
    function PhpKeywordFunc575: TSynWebTokenKind;
    function PhpKeywordFunc587: TSynWebTokenKind;
    function PhpKeywordFunc590: TSynWebTokenKind;
    function PhpKeywordFunc635: TSynWebTokenKind;
    function PhpKeywordFunc644: TSynWebTokenKind;

    // Other -------------------------------------------------------------------
    procedure AddAttribute(AAttrib: TSynHighlighterAttributes);
    procedure AddToNotifyList(ASynWeb: TSynWebBase);
    procedure RemoveFromNotifyList(ASynWeb: TSynWebBase);

    procedure SetAttributesOnChange(AEvent: TNotifyEvent);
    procedure DefHighlightChange(Sender: TObject);

    function GetCrc8String(AString: String): byte;
    function GetRangeBit(ABit: Longword): Boolean;
    procedure SetRangeBit(ABit: Longword; AVal: Boolean);
    function GetRangeInt(ALen, APos: Longword): Longword;
    procedure SetRangeInt(ALen, APos, AVal: Longword);

    procedure NullProc;
    procedure NextSetHighlighterType;
    procedure SetHighlighterType(const AHighlighterType: TSynWebHighlighterType;
      AClearBits: Boolean; ASetAtNextToken: Boolean; AUseNextAH: Boolean);
    procedure SetupHighlighterType(AClearBits: Boolean = False);
    procedure SetLine(NewValue: String; LineNumber: Integer);
    procedure Next;
    function GetToken: String;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // Global
    property InactiveAttri: TSynHighlighterAttributes
      read FInactiveAttri write FInactiveAttri;
    property Options: TSynWebEngineOptions read FOptions write FOptions;

    // ML
    property MLWhitespaceAttri: TSynHighlighterAttributes
      read FMLWhitespaceAttri write FMLWhitespaceAttri;
    property MLCommentAttri: TSynHighlighterAttributes
      read FMLCommentAttri write FMLCommentAttri;
    property MLTextAttri: TSynHighlighterAttributes
      read FMLTextAttri write FMLTextAttri;
    property MLEscapeAttri: TSynHighlighterAttributes
      read FMLEscapeAttri write FMLEscapeAttri;
    property MLSymbolAttri: TSynHighlighterAttributes
      read FMLSymbolAttri write FMLSymbolAttri;
    property MLTagAttri: TSynHighlighterAttributes
      read FMLTagAttri write FMLTagAttri;
    property MLTagNameAttri: TSynHighlighterAttributes
      read FMLTagNameAttri write FMLTagNameAttri;
    property MLTagNameUndefAttri: TSynHighlighterAttributes
      read FMLTagNameUndefAttri write FMLTagNameUndefAttri;
    property MLTagKeyAttri: TSynHighlighterAttributes
      read FMLTagKeyAttri write FMLTagKeyAttri;
    property MLTagKeyUndefAttri: TSynHighlighterAttributes
      read FMLTagKeyUndefAttri write FMLTagKeyUndefAttri;
    property MLTagKeyValueAttri: TSynHighlighterAttributes
      read FMLTagKeyValueAttri write FMLTagKeyValueAttri;
    property MLTagKeyValueQuotedAttri: TSynHighlighterAttributes
      read FMLTagKeyValueQuotedAttri write FMLTagKeyValueQuotedAttri;
    property MLErrorAttri: TSynHighlighterAttributes
      read FMLErrorAttri write FMLErrorAttri;

    // Css
    property CssWhitespaceAttri: TSynHighlighterAttributes
      read FCssWhitespaceAttri write FCssWhitespaceAttri;
    property CssRulesetWhitespaceAttri: TSynHighlighterAttributes
      read FCssRulesetWhitespaceAttri write FCssRulesetWhitespaceAttri;
    property CssSelectorAttri: TSynHighlighterAttributes
      read FCssSelectorAttri write FCssSelectorAttri;
    property CssSelectorUndefAttri: TSynHighlighterAttributes
      read FCssSelectorUndefAttri write FCssSelectorUndefAttri;
    property CssSelectorClassAttri: TSynHighlighterAttributes
      read FCssSelectorClassAttri write FCssSelectorClassAttri;
    property CssSelectorIdAttri: TSynHighlighterAttributes
      read FCssSelectorIdAttri write FCssSelectorIdAttri;
    property CssSpecialAttri: TSynHighlighterAttributes
      read FCssSpecialAttri write FCssSpecialAttri;
    property CssCommentAttri: TSynHighlighterAttributes
      read FCssCommentAttri write FCssCommentAttri;
    property CssPropAttri: TSynHighlighterAttributes
      read FCssPropAttri write FCssPropAttri;
    property CssPropUndefAttri: TSynHighlighterAttributes
      read FCssPropUndefAttri write FCssPropUndefAttri;
    property CssValAttri: TSynHighlighterAttributes
      read FCssValAttri write FCssValAttri;
    property CssValUndefAttri: TSynHighlighterAttributes
      read FCssValUndefAttri write FCssValUndefAttri;
    property CssValStringAttri: TSynHighlighterAttributes
      read FCssValStringAttri write FCssValStringAttri;
    property CssValNumberAttri: TSynHighlighterAttributes
      read FCssValNumberAttri write FCssValNumberAttri;
    property CssSymbolAttri: TSynHighlighterAttributes
      read FCssSymbolAttri write FCssSymbolAttri;
    property CssErrorAttri: TSynHighlighterAttributes
      read FCssErrorAttri write FCssErrorAttri;

    // ECMAScript
    property EsWhitespaceAttri: TSynHighlighterAttributes
      read FEsWhitespaceAttri write FEsWhitespaceAttri;
    property EsIdentifierAttri: TSynHighlighterAttributes
      read FEsIdentifierAttri write FEsIdentifierAttri;
    property EsKeyAttri: TSynHighlighterAttributes
      read FEsKeyAttri write FEsKeyAttri;
    property EsCommentAttri: TSynHighlighterAttributes
      read FEsCommentAttri write FEsCommentAttri;
    property EsStringAttri: TSynHighlighterAttributes
      read FEsStringAttri write FEsStringAttri;
    property EsNumberAttri: TSynHighlighterAttributes
      read FEsNumberAttri write FEsNumberAttri;
    property EsSymbolAttri: TSynHighlighterAttributes
      read FEsSymbolAttri write FEsSymbolAttri;
    property EsErrorAttri: TSynHighlighterAttributes
      read FEsErrorAttri write FEsErrorAttri;

    // Php
    property PhpHereDocList: TStringList read FPhpHereDocList;
    property PhpWhitespaceAttri: TSynHighlighterAttributes
      read FPhpWhitespaceAttri write FPhpWhitespaceAttri;
    property PhpCliInlineTextAttri: TSynHighlighterAttributes
      read FPhpInlineTextAttri write FPhpInlineTextAttri;
    property PhpIdentifierAttri: TSynHighlighterAttributes
      read FPhpIdentifierAttri write FPhpIdentifierAttri;
    property PhpKeyAttri: TSynHighlighterAttributes
      read FPhpKeyAttri write FPhpKeyAttri;
    property PhpFunctionAttri: TSynHighlighterAttributes
      read FPhpFunctionAttri write FPhpFunctionAttri;
    property PhpVariableAttri: TSynHighlighterAttributes
      read FPhpVariableAttri write FPhpVariableAttri;
    property PhpConstAttri: TSynHighlighterAttributes
      read FPhpConstAttri write FPhpConstAttri;
    property PhpMethodAttri: TSynHighlighterAttributes
      read FPhpMethodAttri write FPhpMethodAttri;
    property PhpStringAttri: TSynHighlighterAttributes
      read FPhpStringAttri write FPhpStringAttri;
    property PhpStringSpecialAttri: TSynHighlighterAttributes
      read FPhpStringSpecialAttri write FPhpStringSpecialAttri;
    property PhpCommentAttri: TSynHighlighterAttributes
      read FPhpCommentAttri write FPhpCommentAttri;
    property PhpDocCommentAttri: TSynHighlighterAttributes
      read FPhpDocCommentAttri write FPhpDocCommentAttri;
    property PhpDocCommentTagAttri: TSynHighlighterAttributes
      read FPhpDocCommentTagAttri write FPhpDocCommentTagAttri;
    property PhpSymbolAttri: TSynHighlighterAttributes
      read FPhpSymbolAttri write FPhpSymbolAttri;
    property PhpNumberAttri: TSynHighlighterAttributes
      read FPhpNumberAttri write FPhpNumberAttri;
    property PhpErrorAttri: TSynHighlighterAttributes
      read FPhpErrorAttri write FPhpErrorAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst, StrUtils;
{$ELSE}
  SynEditStrConst, Controls;
{$ENDIF}

{ TSynWebOptionsBase }

constructor TSynWebOptionsBase.Create(AOptions: PSynWebOptions);
begin
  FOnChange := nil;
  FEngineOptions := nil;
  FOptions := AOptions;
  FUseEngineOptions := False;

  FOptions^.FHtmlVersion := shvXHtml10Transitional;
  FOptions^.FWmlVersion := swvWml13;
  FOptions^.FCssVersion := scvCss21;
  FOptions^.FPhpVersion := spvPhp5;
  FOptions^.FPhpShortOpenTag := True;
  FOptions^.FPhpAspTags := False;

  FOptions^.FPhpEmbeded := False;
  FOptions^.FCssEmbeded := False;
  FOptions^.FEsEmbeded := False;
end;

function TSynWebOptionsBase.GetHtmlVersion: TSynWebHtmlVersion;
begin
  Result := FOptions^.FHtmlVersion;
end;

procedure TSynWebOptionsBase.SetHtmlVersion(const Value: TSynWebHtmlVersion);
begin
  if CanUseEngineOptions then
    Exit;
  FOptions^.FHtmlVersion := Value;
  UpdateMLOption;
  DoOnChange;
end;

function TSynWebOptionsBase.GetWmlVersion: TSynWebWmlVersion;
begin
  Result := FOptions^.FWmlVersion;
end;

procedure TSynWebOptionsBase.SetWmlVersion(const Value: TSynWebWmlVersion);
begin
  if CanUseEngineOptions then
    Exit;
  FOptions^.FWmlVersion := Value;
  UpdateMLOption;
  DoOnChange;
end;

function TSynWebOptionsBase.GetCssVersion: TSynWebCssVersion;
begin
  Result := FOptions^.FCssVersion;
end;

procedure TSynWebOptionsBase.SetCssVersion(const Value: TSynWebCssVersion);
begin
  if CanUseEngineOptions then
    Exit;
  FOptions^.FCssVersion := Value;
  DoOnChange;
end;

function TSynWebOptionsBase.GetPhpVersion: TSynWebPhpVersion;
begin
  Result := FOptions^.FPhpVersion;
end;

procedure TSynWebOptionsBase.SetPhpVersion(const Value: TSynWebPhpVersion);
begin
  if CanUseEngineOptions then
    Exit;
  FOptions^.FPhpVersion := Value;
  DoOnChange;
end;

function TSynWebOptionsBase.GetPhpAspTags: Boolean;
begin
  Result := FOptions^.FPhpAspTags;
end;

procedure TSynWebOptionsBase.SetPhpAspTags(const Value: Boolean);
begin
  if CanUseEngineOptions then
    Exit;
  FOptions^.FPhpAspTags := Value;
  DoOnChange;
end;

function TSynWebOptionsBase.GetPhpShortOpenTag: Boolean;
begin
  Result := FOptions^.FPhpShortOpenTag;
end;

procedure TSynWebOptionsBase.SetPhpShortOpenTag(const Value: Boolean);
begin
  if CanUseEngineOptions then
    Exit;
  FOptions^.FPhpShortOpenTag := Value;
  DoOnChange;
end;

function TSynWebOptionsBase.GetCssEmbeded: Boolean;
begin
  Result := FOptions^.FCssEmbeded;
end;

procedure TSynWebOptionsBase.SetCssEmbeded(const Value: Boolean);
begin
  FOptions^.FCssEmbeded := Value;
  DoOnChange;
end;

function TSynWebOptionsBase.GetEsEmbeded: Boolean;
begin
  Result := FOptions^.FEsEmbeded;
end;

procedure TSynWebOptionsBase.SetEsEmbeded(const Value: Boolean);
begin
  FOptions^.FEsEmbeded := Value;
  DoOnChange;
end;

function TSynWebOptionsBase.GetPhpEmbeded: Boolean;
begin
  Result := FOptions^.FPhpEmbeded;
end;

procedure TSynWebOptionsBase.SetPhpEmbeded(const Value: Boolean);
begin
  FOptions^.FPhpEmbeded := Value;
  DoOnChange;
end;

procedure TSynWebOptionsBase.SetUseEngineOptions(const Value: Boolean);
begin
  if FUseEngineOptions = Value then
    Exit;
  FUseEngineOptions := Value;
  UpdateOptions;
  DoOnChange;
end;

procedure TSynWebOptionsBase.SetEngineOptions(AEngine: PSynWebOptions);
begin
  if AEngine = FEngineOptions then
    Exit;
  FEngineOptions := AEngine;
  if CanUseEngineOptions then 
  begin
    UpdateOptions;
    DoOnChange;
  end;
end;

procedure TSynWebOptionsBase.DoOnChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSynWebOptionsBase.UpdateOptions;
begin
  if CanUseEngineOptions then
  begin
    FOptions^.FHtmlVersion := FEngineOptions^.FHtmlVersion;
    FOptions^.FWmlVersion := FEngineOptions^.FWmlVersion;
    UpdateMLOption;
    FOptions^.FCssVersion := FEngineOptions^.FCssVersion;
    FOptions^.FPhpVersion := FEngineOptions^.FPhpVersion;
    FOptions^.FPhpShortOpenTag := FEngineOptions^.FPhpShortOpenTag;
    FOptions^.FPhpAspTags := FEngineOptions^.FPhpAspTags;
  end;
end;

procedure TSynWebOptionsBase.UpdateMLOption;
begin
  //
end;

function TSynWebOptionsBase.CanUseEngineOptions: Boolean;
begin
  Result := FUseEngineOptions and (FEngineOptions <> nil);
end;

procedure TSynWebOptionsBase.AssignTo(Dest: TPersistent);
begin
  if Dest is TSynWebOptionsBase then
    with TSynWebOptionsBase(Dest) do
    begin
      HtmlVersion := Self.HtmlVersion;
      WmlVersion := Self.WmlVersion;
      CssVersion := Self.CssVersion;
      PhpVersion := Self.PhpVersion;
      PhpShortOpenTag := Self.PhpShortOpenTag;
      PhpAspTags := Self.PhpAspTags;
      PhpEmbeded := Self.PhpEmbeded;
      CssEmbeded := Self.CssEmbeded;
      EsEmbeded := Self.EsEmbeded;
      UseEngineOptions := Self.UseEngineOptions;
    end;
end;

{ TSynWebHtmlOptions }

constructor TSynWebHtmlOptions.Create(AOptions: PSynWebOptions);
begin
  inherited Create(AOptions);
  UpdateMLOption;
end;

procedure TSynWebHtmlOptions.UpdateMLOption;
begin
  FOptions^.FMLVersion := TSynWebMLVersion(FOptions^.FHtmlVersion);
end;

{ TSynWebWmlOptions }

constructor TSynWebWmlOptions.Create(AOptions: PSynWebOptions);
begin
  inherited Create(AOptions);
  UpdateMLOption;
end;

procedure TSynWebWmlOptions.UpdateMLOption;
begin
  FOptions^.FMLVersion := TSynWebMLVersion(Integer(smlwvWml11) + Integer(FOptions^.FWmlVersion));
end;

{ TSynWebXmlOptions }

constructor TSynWebXmlOptions.Create(AOptions: PSynWebOptions);
begin
  inherited Create(AOptions);
  UpdateMLOption;
end;

procedure TSynWebXmlOptions.UpdateMLOption;
begin
  FOptions^.FMLVersion := smlwvXML;
end;

{ TSynWebEngineOptions }

constructor TSynWebEngineOptions.Create(AOptions: PSynWebOptions);
begin
  inherited Create(AOptions);
  FUseEngineOptions := False;
end;

{ TSynWebBase }

procedure TSynWebBase.Assign(Source: TPersistent);
begin
  if Source is TSynWebBase then begin
    Self.Engine := TSynWebBase(Source).Engine;
    Self.FOptions.Assign(TSynWebBase(Source).FOptions);
  end;
  inherited;
end;

constructor TSynWebBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner); 
  FOptions.FOnChange := DefHighlightChange;
  FEngine := nil;
  FDefaultFilter := '';
  FActiveHighlighter := False;
  FActiveHighlighters := [shtML, shtCss, shtEs, shtPhpInML,
    shtPhpInCss, shtPhpInEs];
  ResetRange;
  DoDefHighlightChange;
end;

destructor TSynWebBase.Destroy;
begin
  Engine := nil;
  if FOptions <> nil then
    FOptions.Free;
  inherited Destroy;
end;

procedure TSynWebBase.SetActiveHighlighter(const Value: Boolean);
begin
  FActiveHighlighter := Value;
  if Value then
    SetupActiveHighlighter
  else
    FActiveHighlighters := [Low(TSynWebHighlighterType)..High(TSynWebHighlighterType)];
  DefHighlightChange(Self);
end;

procedure TSynWebBase.SetEngine(const Value: TSynWebEngine);
begin
  if FEngine <> nil then
    FEngine.RemoveFromNotifyList(Self);
  FEngine := Value;
  if FEngine = nil then
    FOptions.SetEngineOptions(nil)
  else
  begin
    FEngine.AddToNotifyList(Self);
    FOptions.SetEngineOptions(@FEngine.FEngineOptions);
  end;
end;

{$IFDEF UNISYNEDIT}
procedure TSynWebBase.DoSetLine(const Value: WideString; LineNumber: Integer);
var
  s: String;
  p: PChar;
  w: PWideChar;
  j, i: Integer;
begin
  inherited DoSetLine(Value, LineNumber);
  if FEngine = nil then
    Exit;
  FEngine.FInstance := @FInstance;
  j := Length(Value);
  if j > 0 then
  begin
    SetLength(S, j);
    p := @s[1];
    w := @Value[1];
    for i := 1 to j do
    begin
      if Word(w^) > 255 then
        p^ := '?'
      else
        p^ := Char(w^);
      Inc(p);
      Inc(w);
    end;
  end else
    s := '';
  FEngine.SetLine(s, LineNumber);
end;
{$ENDIF}

procedure TSynWebBase.DoDefHighlightChange;
begin
  FOptions.UpdateOptions;
  if FInstance.FOptions.FMLVersion >= smlhvXHtml10Strict then
    FInstance.FHashTable := TSynWebSensitiveHashTable
  else
    FInstance.FHashTable := TSynWebInsensitiveHashTable;
  DefHighlightChange(Self);
end;

function TSynWebBase.GetAttribCount: Integer;
begin
  if FEngine = nil then
    Result := 0
  else
    Result := FEngine.FAttributes.Count;
end;

function TSynWebBase.GetAttribute(idx: Integer): TSynHighlighterAttributes;
begin
  Result := nil;
  if (FEngine <> nil) and (idx >= 0) and (idx < FEngine.FAttributes.Count) then
    Result := TSynHighlighterAttributes(FEngine.FAttributes.Objects[idx]);
end;

{$IFNDEF UNISYNEDIT}
function TSynWebBase.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;
{$ENDIF}

{$IFDEF UNISYNEDIT}
function TSynWebBase.GetSampleSource: WideString;
{$ELSE}
function TSynWebBase.GetSampleSource: String;
{$ENDIF}
begin
  Result := SynWebSample;
end;

{$IFDEF UNISYNEDIT}
class function TSynWebBase.GetFriendlyLanguageName: WideString;
begin
  Result := GetLanguageName;
end;    
{$ENDIF}

function TSynWebBase.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  if FEngine = nil then
    Result := nil
  else
    case Index of
    // SYN_ATTR_IDENTIFIER: ??
    // SYN_ATTR_KEYWORD: ??
    // SYN_ATTR_SYMBOL: ??
    SYN_ATTR_WHITESPACE:
      begin
        Result := FInstance.fSYN_ATTR_WHITEsPACE;
        if not Enabled then
          case FInstance.FHighlighterMode of
          shmML:
            Result := fEngine.FMLWhitespaceAttri;
          shmCss:
            Result := fEngine.FCssWhitespaceAttri;
          shmEs:
            Result := fEngine.FEsWhitespaceAttri;
          shmPhpCli:
            Result := fEngine.FPhpInlineTextAttri;
          end;
      end;
    SYN_ATTR_COMMENT:
      Result := FInstance.fSYN_ATTR_COMMENT;
    SYN_ATTR_STRING:
      Result := FInstance.fSYN_ATTR_STRING;
    else // case
      Result := nil;
    end;
end;

function TSynWebBase.GetTokenAttribute: TSynHighlighterAttributes;
begin
  if FEngine = nil then
    Result := nil
  else
    if (FInstance.FHighlighterType in FActiveHighlighters) then
      Result := FEngine.FTokenAttributeTable[FInstance.FTokenID]
    else
      Result := FEngine.FInactiveAttri;
end;

{$IFNDEF UNISYNEDIT}
function TSynWebBase.GetToken: String;
var
  Len: longint;
begin
  Len := FInstance.FRun - FInstance.FTokenPos;
  SetString(Result, (FInstance.FLine + FInstance.FTokenPos), Len);
end;
{$ENDIF}

function TSynWebBase.GetTokenLen: Integer;
begin
  Result := FInstance.FRun - FInstance.FTokenPos;
end;

function TSynWebBase.GetTokenPos: Integer;
begin
  Result := FInstance.FTokenPos;
end;

function TSynWebBase.GetTokenID: TSynWebTokenKind;
begin
  Result := FInstance.FTokenID;
end;

function TSynWebBase.GetTokenKind: Integer;
begin
  Result := Ord(FInstance.FTokenID);
end;

function TSynWebBase.GetRange: Pointer;
begin
  Result := Pointer(FInstance.FRange);
end;

function TSynWebBase.GetEol: Boolean;
begin
  Result := FInstance.FTokenID = stkNull;
end;

function TSynWebBase.GetHighlighterType: TSynWebHighlighterType;
begin
  Result := FInstance.FHighlighterType;
end;

function TSynWebBase.PhpGetKeywordId: Integer;
begin
  if FInstance.FTokenID <> stkPhpKeyword then
    Result := -1
  else
    Result := FInstance.FTokenLastID;
end;

function TSynWebBase.PhpGetFunctionId: Integer;
begin
  if FInstance.FTokenID <> stkPhpFunction then
    Result := -1
  else
    Result := FInstance.FTokenLastID;
end;

function TSynWebBase.PhpGetSymbolId: Integer;
begin
  if FInstance.FTokenID <> stkPhpSymbol then
    Result := -1
  else
    Result := FInstance.FTokenLastSymbolId;
end;

function TSynWebBase.PhpGetRange: TSynWebPhpRangeState;
begin
  if FEngine = nil then
    Result := srsPhpDefault
  else
    Result := FEngine.PhpGetRange;
end;

function TSynWebBase.CssGetPropertyId: Integer;
begin
  if (FEngine <> nil) and (FInstance.FHighlighterType = shtCss) and
    (FEngine.CssGetRange in [srsCssPropVal, srsCssPropValImportant,
    srsCssPropValStr, srsCssPropValRgb, srsCssPropValFunc, srsCssPropValSpecial,
    srsCssPropValUrl, srsCssPropValRect]) then
    Result := FEngine.CssGetProp - 1
  else
    Result := -1;
end;

function TSynWebBase.CssGetRange: TSynWebCssRangeState;
begin
  if FEngine = nil then
    Result := srsCssRuleset
  else
    Result := FEngine.CssGetRange;
end;

procedure TSynWebBase.SetRange(Value: Pointer);
begin
  FInstance.FRange := Longword(Value);
end;

{$IFNDEF UNISYNEDIT}
procedure TSynWebBase.SetLine(NewValue: String; LineNumber: Integer);
begin
  if FEngine = nil then
    Exit;
  FEngine.FInstance := @FInstance;
  FEngine.SetLine(NewValue, LineNumber);
end;
{$ENDIF}

procedure TSynWebBase.Next;
begin
  if FEngine = nil then
    FInstance.FTokenID := stkNull
  else
  begin
    FEngine.FInstance := @FInstance;
    FEngine.Next;
{$IFDEF UNISYNEDIT}
    Run := FInstance.FRun;
    fTokenPos := FInstance.FTokenPos;
    inherited Next;
{$ENDIF}
  end;
end;

{$IFDEF UNISYNEDIT}
function TSynWebBase.GetActiveHighlighter(ARange: Pointer;
  ALine: WideString; ACaretX, ACaretY: Integer): TSynWebHighlighterTypes;
{$ELSE}
function TSynWebBase.GetActiveHighlighter(ARange: Pointer;
  ALine: String; ACaretX, ACaretY: Integer): TSynWebHighlighterTypes;
{$ENDIF}
var
  lPos, lLen: Integer;
  lHinghlighter, fActiveHL: TSynWebHighlighterType;
begin
  Dec(ACaretX);
  SetRange(ARange);
  lHinghlighter := TSynWebHighlighterType((FInstance.FRange shr 29) and not ($FFFFFFFF shl 3));
  SetLine(ALine, ACaretY);
  lPos := GetTokenPos;
  lLen := GetTokenLen;
  while (GetTokenPos < ACaretX) and not GetEol do
  begin
    lHinghlighter := FInstance.FHighlighterType;
    lPos := GetTokenPos;
    lLen := GetTokenLen;
    Next;
  end;
  if FInstance.FUseNextAH and (ACaretX >= lPos + lLen) then
    fActiveHL := FInstance.FHighlighterType
  else
    if FInstance.FHighlighterSW and (ACaretX >= lPos + lLen) then
      fActiveHL := FInstance.FPrevHighlighterType
    else
      fActiveHL := lHinghlighter;
  if fActiveHL >= shtPhpInML then
    Result := [shtPhpInML, shtPhpInCss, shtPhpInEs]
  else
    Result := [fActiveHL];
end;

{ TSynWebMLSyn }

constructor TSynWebMLSyn.Create(AOwner: TComponent);
begin          
  FInstance.FHighlighterMode := shmML;
  inherited Create(AOwner);
end;

procedure TSynWebMLSyn.SetupActiveHighlighter;
begin       
  FActiveHighlighters := [shtML];
end;

procedure TSynWebMLSyn.ResetRange;
begin
  FInstance.FRange := $00000000;
end;

function TSynWebMLSyn.GetTagID: Integer;
begin
  if (FEngine <> nil) and (FInstance.FHighlighterType = shtML) and
    (FEngine.MLGetRange in [srsMLTag, srsMLTagClose, srsMLTagKey, srsMLTagKeyEq,
    srsMLTagKeyValue, srsMLTagKeyValueQuoted1, srsMLTagKeyValueQuoted2]) then
    Result := FEngine.MLGetTag - 1
  else
    Result := -1;
end;

function TSynWebMLSyn.GetTagKind: Integer;
begin
  if (FEngine = nil) or (FInstance.FHighlighterType <> shtML) then
    Result := 0
  else
    if FEngine.MLGetRange = srsMLTagClose then
      Result := -1
    else
      Result := 1;
end;

function TSynWebMLSyn.GetMLRange: TSynWebMLRangeState;
begin
  if FEngine = nil then
    Result := srsMLText
  else
    Result := FEngine.MLGetRange;
end;

{ TSynWebHtmlSyn }

constructor TSynWebHtmlSyn.Create(AOwner: TComponent);
begin
  FOptions := TSynWebHtmlOptions.Create(@FInstance.FOptions);
  inherited Create(AOwner);
  FOptions.PhpEmbeded := True;
  FOptions.CssEmbeded := True;
  FOptions.EsEmbeded := True;
end;

function TSynWebHtmlSyn.GetOptions: TSynWebHtmlOptions;
begin
  Result := TSynWebHtmlOptions(FOptions);
end;

procedure TSynWebHtmlSyn.SetOptions(const AValue: TSynWebHtmlOptions);
begin
  FOptions := AValue;
end;

class function TSynWebHtmlSyn.GetLanguageName: String;
begin
  //Result := 'TSynWeb: HTML/XHTML (+CSS, +ES, +PHP)';
  Result := 'HTML';
end;

{$IFDEF UNISYNEDIT}
class function TSynWebHtmlSyn.SynWebSample: WideString;
{$ELSE}
class function TSynWebHtmlSyn.SynWebSample: String;
{$ENDIF}
begin
  Result := '<!DOCTYPE html public "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'#13#10 +
    '<html xmlns="http://www.w3.org/1999/xhtml">'#13#10 +
    '<head>'#13#10 +
    '  <title><!-- comment > -- >TSynWeb<!-- space after two ''-'' allowed --></title>'#13#10 +
    '<style type="text/css">'#13#10 +
    #13#10 +
    TSynWebCssSyn.SynWebSample +
    #13#10 +
    '<?php // php works also in css ?>'#13#10 +
    #13#10 +
    '/* <?php // in comments ?> */'#13#10 +
    #13#10 +
    'span {'#13#10 +
    '  background-image: url("<?= $secure ? ''https://'' : ''http://'''#13#10 +
    '  // php in css-string ?>www.example.com/img.png"); }'#13#10 +
    #13#10 +
    '</style>'#13#10 +
    '  '#13#10 +
    '</head>'#13#10 +
    #13#10 +
    '<body>'#13#10 +
    #13#10 +
    '<![CDATA['#13#10 +
    '  <a href="test"> CDATA Support </a> Warning! CDATA supported only in XHTML'#13#10 +
    '    <?php // no html highlight in CDATA,everything goes here as plain texte, except PHP of course ?>'#13#10 +
    ']]>'#13#10 +
    #13#10 +
    '&amp; &copy;'#13#10 +
    '&earth; &copy <!-- invalid amp-tags, ''earth'' not supported and '';'' missed -->'#13#10 +
    #13#10 +
    '<script language="php">  // php long open tag (html)'#13#10 +
    #13#10 +
    '$b = ''ple'';'#13#10 +
    '$a = <<< my_custom_heredoc'#13#10 +
    'exam$b'#13#10 +
    'my_custom_heredoc;'#13#10 +
    #13#10 +
    '</script>'#13#10 +
    #13#10 +
    '<a href="http://www.<?= $a; ?>.com">Example.com</a>'#13#10 +
    '<br />'#13#10 +
    #13#10 +
    '<div href="whoops" style="someDiv">'#13#10 +
    #13#10 +
    '</div>'#13#10 +
    #13#10 +
    TSynWebPhpCliSyn.SynWebSample +
    #13#10 +
    '<script type="text/javascript" language="javascript">'#13#10 +
    #13#10 +
    TSynWebEsSyn.SynWebSample +
    #13#10 +
    '// comm<?php'#13#10 +
    '?>'#13#10 +
    'ent'#13#10 +
    #13#10 +
    '/* comment <?= ''2''; ?> */'#13#10 +
    #13#10 +
    'new s = "test <?= ''3''; ?>";'#13#10 +
    #13#10 +
    '</script>'#13#10 +
    #13#10 +
    '</body>'#13#10 +
    '</html>'#13#10;
end;

{ TSynWebWmlSyn }

constructor TSynWebWmlSyn.Create(AOwner: TComponent);
begin
  FOptions := TSynWebWmlOptions.Create(@FInstance.FOptions);
  inherited Create(AOwner);
  FOptions.PhpEmbeded := True;
  FOptions.CssEmbeded := False;
  FOptions.EsEmbeded := False;
end;

function TSynWebWmlSyn.GetOptions: TSynWebWmlOptions;
begin
  Result := TSynWebWmlOptions(FOptions);
end;

procedure TSynWebWmlSyn.SetOptions(const AValue: TSynWebWmlOptions);
begin
  FOptions := AValue;
end;

class function TSynWebWmlSyn.GetLanguageName: String;
begin
  Result := 'TSynWeb: WML (+PHP)';
end;

{$IFDEF UNISYNEDIT}
class function TSynWebWmlSyn.SynWebSample: WideString;
{$ELSE}
class function TSynWebWmlSyn.SynWebSample: String;
{$ENDIF}
begin
  Result :=
    '<?xml version="1.0"?>'#13#10 +
    '<!DOCTYPE wml PUBLIC "-//WAPFORUM//DTD WML 1.3//EN"'#13#10 +
    '  "http://www.wapforum.org/DTD/wml13.dtd">'#13#10 +
    '<wml>'#13#10 +
    '  <card id="page1" title="Page 1">'#13#10 +
    '   <p align="center">'#13#10 +
    '  Hello world from WML!'#13#10 +
    '    <a href="#page2">Go to page 2</a><br/>'#13#10 +
    '   </p>'#13#10 +
    '  </card>'#13#10 +
    #13#10 +
    '  <card id="page2"  title="Page 2">'#13#10 +
    '   <p align="center">'#13#10 +
    '  Hello world from <?php echo ''PHP''; ?>!'#13#10 +
    '    <a href="#page1">Go to page 1</a><br/>'#13#10 +
    '   </p>'#13#10 +
    '  </card>'#13#10 +
    '</wml>'#13#10;
end;

{ TSynWebXmlSyn }

constructor TSynWebXmlSyn.Create(AOwner: TComponent);
begin
  FOptions := TSynWebXmlOptions.Create(@FInstance.FOptions);
  inherited Create(AOwner);
  FOptions.PhpShortOpenTag := False;
  FOptions.PhpEmbeded := True;
  FOptions.CssEmbeded := False;
  FOptions.EsEmbeded := False;
  FOptions.UseEngineOptions := False;
end;

function TSynWebXmlSyn.GetOptions: TSynWebXmlOptions;
begin
  Result := TSynWebXmlOptions(FOptions);
end;

procedure TSynWebXmlSyn.SetOptions(const AValue: TSynWebXmlOptions);
begin
  FOptions := AValue;
end;

class function TSynWebXmlSyn.GetLanguageName: String;
begin
//  Result := 'TSynWeb: XML (+PHP)';
  Result := 'XML';
end;

{$IFDEF UNISYNEDIT}
class function TSynWebXmlSyn.SynWebSample: WideString;
{$ELSE}
class function TSynWebXmlSyn.SynWebSample: String;
{$ENDIF}
begin
  Result := 
    '<?xml version="1.0"?>'#13#10 +
    '<?xml-stylesheet type="text/css" href="nutrition.css"?>'#13#10 +
    '<nutrition>'#13#10 +
    #13#10 +
    '<dailyvalues>'#13#10 +
    '	<totalfat units="g">65</totalfat>'#13#10 +
    '	<saturatedfat units="g">20</saturatedfat>'#13#10 +
    '	<cholesterol units="mg">300</cholesterol>'#13#10 +
    '	<sodium units="mg">2400</sodium>'#13#10 +
    '	<carb units="g">300</carb>'#13#10 +
    '	<fiber units="g">25</fiber>'#13#10 +
    '	<protein units="g">50</protein>'#13#10 +
    '</dailyvalues>'#13#10 +
    #13#10+
    #13#10+
    '<!--'#13#10 +
    '<food>'#13#10 +
    '	<serving units="g"></serving>'#13#10 +
    '	<calories total="" fat=""/>'#13#10 +
    '</food>'#13#10 +
    '-->';
end;

{ TSynWebCssSyn }

constructor TSynWebCssSyn.Create(AOwner: TComponent);
begin
  FOptions := TSynWebCssOptions.Create(@FInstance.FOptions);
  FInstance.FHighlighterMode := shmCss;
  inherited Create(AOwner);
  FOptions.PhpEmbeded := False;
  FOptions.CssEmbeded := False;
  FOptions.EsEmbeded := False;
end;

procedure TSynWebCssSyn.SetupActiveHighlighter;
begin
  FActiveHighlighters := [shtCss];
end;

function TSynWebCssSyn.GetOptions: TSynWebCssOptions;
begin
  Result := TSynWebCssOptions(FOptions);
end;

procedure TSynWebCssSyn.SetOptions(const AValue: TSynWebCssOptions);
begin
  FOptions := AValue;
end;

class function TSynWebCssSyn.GetLanguageName: String;
begin
//  Result := 'TSynWeb: CSS (+PHP)';
  Result := 'CSS';
end;

{$IFDEF UNISYNEDIT}
class function TSynWebCssSyn.SynWebSample: WideString;
{$ELSE}
class function TSynWebCssSyn.SynWebSample: String;
{$ENDIF}
begin
  Result := '@import url(style.css);'#13#10 +
    ''#13#10 +
    '@media all, invalid {'#13#10 +
    '  #some-id:first-child,'#13#10 +
    '  .some-class:second-child /* invalid pseudo class */,'#13#10 +
    '    div:hover, /* html-tag */'#13#10 +
    '    #attrib[title~="test"] {'#13#10 +
    '    border-top :  1px solid black;'#13#10 +
    '    border-left: -1px solid rgb(0,0,0); /* // negative value not supported here */'#13#10 +
    '    margin:      -1px -1px; /* negative supported in margins */'#13#10 +
    '    background-color: #222 ! important;'#13#10 +
    '    background-image: url(style.css);'#13#10 +
    '    color:     1px solid #fffff  important;  /* errors */'#13#10 +
    '    something: 1px solid #222222 url("invalid tag ''something''");'#13#10 +
    '  }'#13#10 +
    '}'#13#10;
end;

procedure TSynWebCssSyn.ResetRange;
begin
  FInstance.FRange := $00000000 or (Longword(shtCss) shl 29);
end;

{ TSynWebEsSyn }

constructor TSynWebEsSyn.Create(AOwner: TComponent);
begin
  FOptions := TSynWebEsOptions.Create(@FInstance.FOptions);
  FInstance.FHighlighterMode := shmEs;
  inherited Create(AOwner);
  FOptions.PhpEmbeded := False;
  FOptions.CssEmbeded := False;
  FOptions.EsEmbeded := False;
end;

procedure TSynWebEsSyn.SetupActiveHighlighter;
begin
  FActiveHighlighters := [shtEs];
end;

function TSynWebEsSyn.GetOptions: TSynWebEsOptions;
begin
  Result := TSynWebEsOptions(FOptions);
end;

procedure TSynWebEsSyn.SetOptions(const AValue: TSynWebEsOptions);
begin
  FOptions := AValue;
end;

class function TSynWebEsSyn.GetLanguageName: String;
begin
  Result := 'TSynWeb: JS/ES (+PHP)';
end;

{$IFDEF UNISYNEDIT}
class function TSynWebEsSyn.SynWebSample: WideString;
{$ELSE}
class function TSynWebEsSyn.SynWebSample: String;
{$ENDIF}
begin
  Result := '// comment'#13#10 +
    '/* comment 2 */'#13#10 +
    ''#13#10 +
    'function test()'#13#10 +
    '{'#13#10 +
    '  window.location.href = ''http://www.example.com'';'#13#10 +
    '  new b = 22;'#13#10 +
    '  return b;'#13#10 +
    '}'#13#10;
end;

procedure TSynWebEsSyn.ResetRange;
begin
  FInstance.FRange := $00000000 or (Longword(shtEs) shl 29);
end;

{ TSynWebPhpCliSyn }

constructor TSynWebPhpCliSyn.Create(AOwner: TComponent);
begin
  FOptions := TSynWebPhpCliOptions.Create(@FInstance.FOptions);
  FInstance.FHighlighterMode := shmPhpCli;
  inherited Create(AOwner);
  FOptions.PhpEmbeded := True;
  FOptions.CssEmbeded := False;
  FOptions.EsEmbeded := False;
end;      

procedure TSynWebPhpCliSyn.SetupActiveHighlighter;
begin
  FActiveHighlighters := [shtML];
end;

function TSynWebPhpCliSyn.GetOptions: TSynWebPhpCliOptions;
begin
  Result := TSynWebPhpCliOptions(FOptions);
end;

procedure TSynWebPhpCliSyn.SetOptions(const AValue: TSynWebPhpCliOptions);
begin
  FOptions := AValue;
end;

class function TSynWebPhpCliSyn.GetLanguageName: String;
begin
  Result := 'TSynWeb: PHP-Cli';
end;

{$IFDEF UNISYNEDIT}
class function TSynWebPhpCliSyn.SynWebSample: WideString;
{$ELSE}
class function TSynWebPhpCliSyn.SynWebSample: String;
{$ENDIF}
begin
  Result := '<?php'#13#10 +
    ''#13#10 +
    'echo ''<?xml version="1.0" encoding="iso-8859-2"?>'';'#13#10 +
    ''#13#10 +
    '// single line comment 1'#13#10 +
    '# single line comment 2'#13#10 +
    '/* multi line comment - ?> */'#13#10 +
    '/** doc-style comment - ?> */'#13#10 +
    ''#13#10 +
    'if( SOME_CONSTANT ) // indet''s typed upper-case are become as constants attrib'#13#10 +
    '{'#13#10 +
    '  $a = ''single quote string \n'';'#13#10 +
    '  $b = "double quote string; $someobject->result->a[$b]    ?>'#13#10 +
    '    {$someobject->result->a[$b]} $t[2]; $t[3 error in string; \n octal \222  \2222222 ";'#13#10 +
    '  $x = ''-a'';'#13#10 +
    '  $c = `ls $x`;'#13#10 +
    '  $z = 2. ; // error'#13#10 +
    '  $z = 2.1 + .2;  // numbers'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    '  // custom tags supported, spaces/tabs allowed between ''<<<'' and HEREDOC ident'#13#10 +
    '  $output = <<< what_ever_you_TYPE'#13#10 +
    'what_ever_you_type;'#13#10 +
    'what_ever_you_TYPE ;'#13#10 +
    ' what_ever_you_TYPE;'#13#10 +
    'what_ever_you_TYPE;;'#13#10 +
    'what_ever_you_TYPE;'#13#10 +
    ''#13#10 +
    'while( my_function($arg) && mysql_query($query) )'#13#10 +
    '{'#13#10 +
    '/// do sth'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    '?>'#13#10;
end;

procedure TSynWebPhpCliSyn.ResetRange;
begin
  FInstance.FRange := $00000000;
end;

{ TSynWebEngine }

constructor TSynWebEngine.Create(AOwner: TComponent);

  function CreateAttrib(const AName:String): TSynHighlighterAttributes;
  begin
{$IFDEF UNISYNEDIT}
    Result := TSynHighlighterAttributes.Create(AName, AName);
{$ELSE}                                                      
    Result := TSynHighlighterAttributes.Create(AName);
{$ENDIF}
  end;

begin
  inherited Create(AOwner);
  FOptions := TSynWebEngineOptions.Create(@FEngineOptions);
  FOptions.FOnChange := DefHighlightChange;
  FNotifyList := TList.Create;
  FPhpHereDocList := TStringList.Create;
  with FPhpHereDocList do
  begin
    Text :=
      'EOF'#13#10+
      'eof'#13#10+
      'EOT'#13#10+
      'eot'#13#10+
      'EOL'#13#10+
      'eol'#13#10+
      'EOD'#13#10+
      'eod'#13#10+
      'HTML'#13#10+
      'html'#13#10+
      'CONTENT'#13#10+
      'content'#13#10+
      'HEREDOC'#13#10+
      'heredoc'#13#10+
      'OUT'#13#10+
      'out'#13#10+
      'STRING'#13#10+
      'string';
{$IFDEF SYN_COMPILER_6_UP}
    CaseSensitive := True;
{$ENDIF}
    Sorted := True;
  end;

  FAttributes := TStringList.Create;
  FAttributes.Duplicates := dupError;
  FAttributes.Sorted := True;

  // Markup Language
  MLMakeMethodTables;

  FMLWhitespaceAttri := CreateAttrib('ML: Whitespace');
  AddAttribute(FMLWhitespaceAttri);

  FMLCommentAttri := CreateAttrib('ML: Comment');
  FMLCommentAttri.Foreground := $A4A0A0;
  AddAttribute(FMLCommentAttri);

  FMLTextAttri := CreateAttrib('ML: Text');
  AddAttribute(FMLTextAttri);

  FMLEscapeAttri := CreateAttrib('ML: Escaped amps');
  FMLEscapeAttri.Foreground := clTeal;
  AddAttribute(FMLEscapeAttri);

  FMLSymbolAttri := CreateAttrib('ML: Symbol'); 
  FMLSymbolAttri.Foreground := clBlack;
  AddAttribute(FMLSymbolAttri);

  FMLTagAttri := CreateAttrib('ML: Tag');
  FMLTagAttri.Foreground := clNavy;
  AddAttribute(FMLTagAttri);

  FMLTagNameAttri := CreateAttrib('ML: Tag name');  
  FMLTagNameAttri.Foreground := clBlue;
  AddAttribute(FMLTagNameAttri);

  FMLTagNameUndefAttri := CreateAttrib('ML: Undefined tag name');
  FMLTagNameUndefAttri.Foreground := clBlue;
  FMLTagNameUndefAttri.Style := [fsUnderline];
  AddAttribute(FMLTagNameUndefAttri);

  FMLTagKeyAttri := CreateAttrib('ML: Key'); 
  FMLTagKeyAttri.Foreground := clRed;
  AddAttribute(FMLTagKeyAttri);

  FMLTagKeyUndefAttri := CreateAttrib('ML: Undefined key');  
  FMLTagKeyUndefAttri.Foreground := clRed;
  FMLTagKeyUndefAttri.Style := [fsUnderline];
  AddAttribute(FMLTagKeyUndefAttri);

  FMLTagKeyValueAttri := CreateAttrib('ML: Value'); 
  FMLTagKeyValueAttri.Foreground := clFuchsia;
  AddAttribute(FMLTagKeyValueAttri);

  FMLTagKeyValueQuotedAttri := CreateAttrib('ML: Quoted value'); 
  FMLTagKeyValueQuotedAttri.Foreground := clFuchsia;
  AddAttribute(FMLTagKeyValueQuotedAttri);

  FMLErrorAttri := CreateAttrib('ML: Error');
  FMLErrorAttri.Foreground := clRed;
  FMLErrorAttri.Style := [fsBold, fsUnderline];
  AddAttribute(FMLErrorAttri);

  FTokenAttributeTable[stkMLSpace] := FMLWhitespaceAttri;
  FTokenAttributeTable[stkMLComment] := FMLCommentAttri;
  FTokenAttributeTable[stkMLText] := FMLTextAttri;
  FTokenAttributeTable[stkMLEscape] := FMLEscapeAttri;
  FTokenAttributeTable[stkMLSymbol] := FMLSymbolAttri;
  FTokenAttributeTable[stkMLTag] := FMLTagAttri;
  FTokenAttributeTable[stkMLTagName] := FMLTagNameAttri;
  FTokenAttributeTable[stkMLTagNameUndef] := FMLTagNameUndefAttri;
  FTokenAttributeTable[stkMLTagKey] := FMLTagKeyAttri;
  FTokenAttributeTable[stkMLTagKeyUndef] := FMLTagKeyUndefAttri;
  FTokenAttributeTable[stkMLTagKeyValue] := FMLTagKeyValueAttri;
  FTokenAttributeTable[stkMLTagKeyValueQuoted] := FMLTagKeyValueQuotedAttri;
  FTokenAttributeTable[stkMLError] := FMLErrorAttri;

  // Css
  CssMakeMethodTables;

  FCssWhitespaceAttri := CreateAttrib('Css: Whitespace');    
  FCssWhitespaceAttri.Background := $F0FFFF;
  AddAttribute(FCssWhitespaceAttri);

  FCssRulesetWhitespaceAttri := CreateAttrib('Css: Ruleset whitespace'); 
  FCssRulesetWhitespaceAttri.Background := clInfoBk;
  AddAttribute(FCssRulesetWhitespaceAttri);

  FCssSelectorAttri := CreateAttrib('Css: Selector'); 
  FCssSelectorAttri.Foreground := clBlue;
  FCssSelectorAttri.Style := [fsBold];
  AddAttribute(FCssSelectorAttri);

  FCssSelectorUndefAttri := CreateAttrib('Css: Undefined selector');   
  FCssSelectorUndefAttri.Foreground := clBlue;
  FCssSelectorUndefAttri.Style := [fsBold, fsUnderline];
  AddAttribute(FCssSelectorUndefAttri);

  FCssSelectorClassAttri := CreateAttrib('Css: Class selector');   
  FCssSelectorClassAttri.Foreground := $C08000;
  FCssSelectorClassAttri.Style := [fsBold];
  AddAttribute(FCssSelectorClassAttri);

  FCssSelectorIdAttri := CreateAttrib('Css: Id selector'); 
  FCssSelectorIdAttri.Foreground := clGreen;
  FCssSelectorIdAttri.Style := [fsBold];
  AddAttribute(FCssSelectorIdAttri);

  FCssSpecialAttri := CreateAttrib('Css: Special');
  FCssSpecialAttri.Foreground := clNavy;
  AddAttribute(FCssSpecialAttri);

  FCssCommentAttri := CreateAttrib('Css: Comment');
  FCssCommentAttri.Foreground := $A4A0A0;
  FCssCommentAttri.Style := [fsItalic];
  AddAttribute(FCssCommentAttri);

  FCssPropAttri := CreateAttrib('Css: Property'); 
  FCssPropAttri.Foreground := clBlue;
  AddAttribute(FCssPropAttri);

  FCssPropUndefAttri := CreateAttrib('Css: Undefined property'); 
  FCssPropUndefAttri.Foreground := clBlue;
  FCssPropUndefAttri.Style := [fsUnderline];
  AddAttribute(FCssPropUndefAttri);

  FCssValAttri := CreateAttrib('Css: Value');   
  FCssValAttri.Foreground := clRed;
  AddAttribute(FCssValAttri);

  FCssValUndefAttri := CreateAttrib('Css: Undefined value'); 
  FCssValUndefAttri.Foreground := clRed;
  FCssValUndefAttri.Style := [fsUnderline];
  AddAttribute(FCssValUndefAttri);

  FCssValStringAttri := CreateAttrib('Css: String value');
  FCssValStringAttri.Foreground := clFuchsia ;
  AddAttribute(FCssValStringAttri);

  FCssValNumberAttri := CreateAttrib('Css: Number value');   
  FCssValNumberAttri.Foreground := clGreen;
  AddAttribute(FCssValNumberAttri);

  FCssSymbolAttri := CreateAttrib('Css: Symbol');   
  FCssSymbolAttri.Foreground := clBlack;
  AddAttribute(FCssSymbolAttri);

  FCssErrorAttri := CreateAttrib('Css: Error');    
  FCssErrorAttri.Foreground := clRed;
  FCssErrorAttri.Style := [fsBold, fsUnderline];
  AddAttribute(FCssErrorAttri);

  FTokenAttributeTable[stkCssSpace] := FCssWhitespaceAttri;
  FTokenAttributeTable[stkCssSelector] := FCssSelectorAttri;
  FTokenAttributeTable[stkCssSelectorUndef] := FCssSelectorUndefAttri;
  FTokenAttributeTable[stkCssSelectorClass] := FCssSelectorClassAttri;
  FTokenAttributeTable[stkCssSelectorId] := FCssSelectorIdAttri;
  FTokenAttributeTable[stkCssSpecial] := FCssSpecialAttri;
  FTokenAttributeTable[stkCssComment] := FCssCommentAttri;
  FTokenAttributeTable[stkCssProp] := FCssPropAttri;
  FTokenAttributeTable[stkCssPropUndef] := FCssPropUndefAttri;
  FTokenAttributeTable[stkCssVal] := FCssValAttri;
  FTokenAttributeTable[stkCssValUndef] := FCssValUndefAttri;
  FTokenAttributeTable[stkCssValString] := FCssValStringAttri;
  FTokenAttributeTable[stkCssValNumber] := FCssValNumberAttri;
  FTokenAttributeTable[stkCssSymbol] := FCssSymbolAttri;
  FTokenAttributeTable[stkCssError] := FCssErrorAttri;

  // ECMAScript
  EsMakeMethodTables;

  FEsWhitespaceAttri := CreateAttrib('Es: Whitespace'); 
  FEsWhitespaceAttri.Background := $FFF0F0;
  AddAttribute(FEsWhitespaceAttri);

  FEsIdentifierAttri := CreateAttrib('Es: Identifier');
  FEsIdentifierAttri.Foreground := clBlue;
  AddAttribute(FEsIdentifierAttri);

  FEsKeyAttri := CreateAttrib('Es: Key'); 
  FEsKeyAttri.Style := [fsBold];
  AddAttribute(FEsKeyAttri);

  FEsCommentAttri := CreateAttrib('Es: Comment');    
  FEsCommentAttri.Foreground := clGreen;
  AddAttribute(FEsCommentAttri);

  FEsStringAttri := CreateAttrib('Es: String');
  FEsStringAttri.Foreground := clRed;
  AddAttribute(FEsStringAttri);

  FEsNumberAttri := CreateAttrib('Es: Number');
  FEsNumberAttri.Foreground := clFuchsia;
  AddAttribute(FEsNumberAttri);

  FEsSymbolAttri := CreateAttrib('Es: Symbol');
  AddAttribute(FEsSymbolAttri);

  FEsErrorAttri := CreateAttrib('Es: Error');  
  FEsErrorAttri.Foreground := clRed;
  FEsErrorAttri.Style := [fsBold, fsUnderline];
  AddAttribute(FEsErrorAttri);

  FTokenAttributeTable[stkEsSpace] := FEsWhitespaceAttri;
  FTokenAttributeTable[stkEsIdentifier] := FEsIdentifierAttri;
  FTokenAttributeTable[stkEsKeyword] := FEsKeyAttri;
  FTokenAttributeTable[stkEsComment] := FEsCommentAttri;
  FTokenAttributeTable[stkEsString] := FEsStringAttri;
  FTokenAttributeTable[stkEsNumber] := FEsNumberAttri;
  FTokenAttributeTable[stkEsSymbol] := FEsSymbolAttri;
  FTokenAttributeTable[stkEsError] := FEsErrorAttri;

  // Php
  PhpMakeMethodTables;

  FPhpWhitespaceAttri := CreateAttrib('Php: Whitespace');   
  FPhpWhitespaceAttri.Background := $F5F5F5;
  AddAttribute(FPhpWhitespaceAttri);

  FPhpInlineTextAttri := CreateAttrib('PhpCli: Inline text');
  AddAttribute(FPhpInlineTextAttri);

  FPhpIdentifierAttri := CreateAttrib('Php: Identifier');  
  FPhpIdentifierAttri.Foreground := clMaroon;
  AddAttribute(FPhpIdentifierAttri);

  FPhpKeyAttri := CreateAttrib('Php: Keyword'); 
  FPhpKeyAttri.Foreground := clBlue;
  AddAttribute(FPhpKeyAttri);

  FPhpFunctionAttri := CreateAttrib('Php: Function');  
  FPhpFunctionAttri.Foreground := clRed;
  AddAttribute(FPhpFunctionAttri);

  FPhpVariableAttri := CreateAttrib('Php: Variable'); 
  FPhpVariableAttri.Foreground := clTeal;
  AddAttribute(FPhpVariableAttri);

  FPhpConstAttri := CreateAttrib('Php: Constant');  
  FPhpConstAttri.Foreground := $0080FF;
  AddAttribute(FPhpConstAttri);

  FPhpMethodAttri := CreateAttrib('Php: Method');
  FPhpMethodAttri.Foreground := $00FF8000;
  AddAttribute(FPhpMethodAttri);

  FPhpStringAttri := CreateAttrib('Php: String'); 
  FPhpStringAttri.Foreground := clFuchsia;
  AddAttribute(FPhpStringAttri);

  FPhpStringSpecialAttri := CreateAttrib('Php: String special');
  FPhpStringSpecialAttri.Background := $EAEAEA;
  FPhpStringSpecialAttri.Foreground := clFuchsia;
  AddAttribute(FPhpStringSpecialAttri);

  FPhpCommentAttri := CreateAttrib('Php: Comment');  
  FPhpCommentAttri.Foreground := clGreen;
  FPhpCommentAttri.Style := [fsItalic];
  AddAttribute(FPhpCommentAttri);

  FPhpDocCommentAttri := CreateAttrib('Php: DocComment');
  FPhpDocCommentAttri.Foreground := clGreen;
  FPhpDocCommentAttri.Style := [fsBold, fsItalic];
  AddAttribute(FPhpDocCommentAttri);

  FPhpDocCommentTagAttri := CreateAttrib('Php: DocComment Tag');
  FPhpDocCommentTagAttri.Foreground := clBlue;
  FPhpDocCommentTagAttri.Style := [fsBold, fsItalic];
  AddAttribute(FPhpDocCommentTagAttri);

  FPhpSymbolAttri := CreateAttrib('Php: Symbol');
  AddAttribute(FPhpSymbolAttri);

  FPhpNumberAttri := CreateAttrib('Php: Number'); 
  FPhpNumberAttri.Foreground := clPurple;
  AddAttribute(FPhpNumberAttri);

  FPhpErrorAttri := CreateAttrib('Php: Error'); 
  FPhpErrorAttri.Foreground := clRed;
  FPhpErrorAttri.Style := [fsBold, fsUnderline];
  AddAttribute(FPhpErrorAttri);

  FTokenAttributeTable[stkPhpSpace] := FMLWhitespaceAttri;
  FTokenAttributeTable[stkPhpIdentifier] := FPhpIdentifierAttri;
  FTokenAttributeTable[stkPhpKeyword] := FPhpKeyAttri;
  FTokenAttributeTable[stkPhpFunction] := FPhpFunctionAttri;
  FTokenAttributeTable[stkPhpVariable] := FPhpVariableAttri;
  FTokenAttributeTable[stkPhpConst] := FPhpConstAttri;
  FTokenAttributeTable[stkPhpString] := FPhpStringAttri;
  FTokenAttributeTable[stkPhpStringSpecial] := FPhpStringSpecialAttri;
  FTokenAttributeTable[stkPhpComment] := FPhpCommentAttri;
  FTokenAttributeTable[stkPhpMethod] := FPhpMethodAttri;
  FTokenAttributeTable[stkPhpDocComment] := FPhpDocCommentAttri;
  FTokenAttributeTable[stkPhpDocCommentTag] := FPhpDocCommentTagAttri;
  FTokenAttributeTable[stkPhpSymbol] := FPhpSymbolAttri;
  FTokenAttributeTable[stkPhpNumber] := FPhpNumberAttri;
  FTokenAttributeTable[stkPhpError] := FPhpErrorAttri;

  // PhpCli
  FTokenAttributeTable[stkPhpInlineText] := FPhpInlineTextAttri;

  // Global
  FInactiveAttri := CreateAttrib('Global: Inactive');
  FInactiveAttri.Foreground := clInactiveCaptionText;
  AddAttribute(FInactiveAttri);

  FTokenAttributeTable[stkNull] := nil;
  SetAttributesOnChange(DefHighlightChange);
end;

destructor TSynWebEngine.Destroy;
var
  i: Integer;
begin
  for i := FNotifyList.Count - 1 downto 0 do
    TSynWebBase(FNotifyList[i]).Engine := nil;
  for i := FAttributes.Count - 1 downto 0 do
    TSynHighlighterAttributes(FAttributes.Objects[i]).Free;                                        
  FAttributes.Free;
  FOptions.Free;
  FNotifyList.Free;
  FPhpHereDocList.Free;
  inherited Destroy;
end;

// Markup Language -------------------------------------------------------------

procedure TSynWebEngine.MLMakeMethodTables;
var
  i: Integer;
  pF: PSynWebIdentFuncTableFunc;
  pF2: PSynWebIdent2FuncTableFunc;
begin
  FMLRangeProcTable[srsMLText] := MLRangeTextProc;
  FMLRangeProcTable[srsMLComment] := MLRangeCommentProc;
  FMLRangeProcTable[srsMLCommentClose] := MLRangeCommentCloseProc;
  FMLRangeProcTable[srsMLTag] := MLRangeTagProc;
  FMLRangeProcTable[srsMLTagClose] := MLRangeTagCloseProc;
  FMLRangeProcTable[srsMLTagDOCTYPE] := MLRangeTagDOCTYPEProc;
  FMLRangeProcTable[srsMLTagCDATA] := MLRangeTagCDATAProc;
  FMLRangeProcTable[srsMLTagKey] := MLRangeTagKeyProc;
  FMLRangeProcTable[srsMLTagKeyEq] := MLRangeTagKeyEqProc;
  FMLRangeProcTable[srsMLTagKeyValue] := MLRangeTagKeyValueProc;
  FMLRangeProcTable[srsMLTagKeyValueQuoted1] := MLRangeTagKeyValueQuoted1Proc;
  FMLRangeProcTable[srsMLTagKeyValueQuoted2] := MLRangeTagKeyValueQuoted2Proc;

  pF := PSynWebIdentFuncTableFunc(@FMLTagIdentFuncTable);
  for I := Low(FMLTagIdentFuncTable) to High(FMLTagIdentFuncTable) do
  begin
    pF^ := MLTagUndef;
    Inc(pF);
  end;
  FMLTagIdentFuncTable[1]:=MLTagFunc1;
  FMLTagIdentFuncTable[2]:=MLTagFunc2;
  FMLTagIdentFuncTable[8]:=MLTagFunc8;
  FMLTagIdentFuncTable[9]:=MLTagFunc9;
  FMLTagIdentFuncTable[16]:=MLTagFunc16;
  FMLTagIdentFuncTable[17]:=MLTagFunc17;
  FMLTagIdentFuncTable[18]:=MLTagFunc18;
  FMLTagIdentFuncTable[19]:=MLTagFunc19;
  FMLTagIdentFuncTable[20]:=MLTagFunc20;
  FMLTagIdentFuncTable[21]:=MLTagFunc21;
  FMLTagIdentFuncTable[22]:=MLTagFunc22;
  FMLTagIdentFuncTable[23]:=MLTagFunc23;
  FMLTagIdentFuncTable[24]:=MLTagFunc24;
  FMLTagIdentFuncTable[25]:=MLTagFunc25;
  FMLTagIdentFuncTable[26]:=MLTagFunc26;
  FMLTagIdentFuncTable[27]:=MLTagFunc27;
  FMLTagIdentFuncTable[28]:=MLTagFunc28;
  FMLTagIdentFuncTable[29]:=MLTagFunc29;
  FMLTagIdentFuncTable[30]:=MLTagFunc30;
  FMLTagIdentFuncTable[31]:=MLTagFunc31;
  FMLTagIdentFuncTable[32]:=MLTagFunc32;
  FMLTagIdentFuncTable[33]:=MLTagFunc33;
  FMLTagIdentFuncTable[35]:=MLTagFunc35;
  FMLTagIdentFuncTable[36]:=MLTagFunc36;
  FMLTagIdentFuncTable[37]:=MLTagFunc37;
  FMLTagIdentFuncTable[38]:=MLTagFunc38;
  FMLTagIdentFuncTable[39]:=MLTagFunc39;
  FMLTagIdentFuncTable[40]:=MLTagFunc40;
  FMLTagIdentFuncTable[41]:=MLTagFunc41;
  FMLTagIdentFuncTable[42]:=MLTagFunc42;
  FMLTagIdentFuncTable[43]:=MLTagFunc43;
  FMLTagIdentFuncTable[46]:=MLTagFunc46;
  FMLTagIdentFuncTable[47]:=MLTagFunc47;
  FMLTagIdentFuncTable[48]:=MLTagFunc48;
  FMLTagIdentFuncTable[49]:=MLTagFunc49;
  FMLTagIdentFuncTable[50]:=MLTagFunc50;
  FMLTagIdentFuncTable[52]:=MLTagFunc52;
  FMLTagIdentFuncTable[53]:=MLTagFunc53;
  FMLTagIdentFuncTable[55]:=MLTagFunc55;
  FMLTagIdentFuncTable[56]:=MLTagFunc56;
  FMLTagIdentFuncTable[57]:=MLTagFunc57;
  FMLTagIdentFuncTable[59]:=MLTagFunc59;
  FMLTagIdentFuncTable[60]:=MLTagFunc60;
  FMLTagIdentFuncTable[61]:=MLTagFunc61;
  FMLTagIdentFuncTable[64]:=MLTagFunc64;
  FMLTagIdentFuncTable[65]:=MLTagFunc65;
  FMLTagIdentFuncTable[66]:=MLTagFunc66;
  FMLTagIdentFuncTable[70]:=MLTagFunc70;
  FMLTagIdentFuncTable[76]:=MLTagFunc76;
  FMLTagIdentFuncTable[78]:=MLTagFunc78;
  FMLTagIdentFuncTable[79]:=MLTagFunc79;
  FMLTagIdentFuncTable[80]:=MLTagFunc80;
  FMLTagIdentFuncTable[81]:=MLTagFunc81;
  FMLTagIdentFuncTable[82]:=MLTagFunc82;
  FMLTagIdentFuncTable[84]:=MLTagFunc84;
  FMLTagIdentFuncTable[85]:=MLTagFunc85;
  FMLTagIdentFuncTable[87]:=MLTagFunc87;
  FMLTagIdentFuncTable[89]:=MLTagFunc89;
  FMLTagIdentFuncTable[91]:=MLTagFunc91;
  FMLTagIdentFuncTable[92]:=MLTagFunc92;
  FMLTagIdentFuncTable[93]:=MLTagFunc93;
  FMLTagIdentFuncTable[94]:=MLTagFunc94;
  FMLTagIdentFuncTable[95]:=MLTagFunc95;
  FMLTagIdentFuncTable[106]:=MLTagFunc106;
  FMLTagIdentFuncTable[107]:=MLTagFunc107;
  FMLTagIdentFuncTable[114]:=MLTagFunc114;
  FMLTagIdentFuncTable[121]:=MLTagFunc121;
  FMLTagIdentFuncTable[128]:=MLTagFunc128;

  pF := PSynWebIdentFuncTableFunc(@FMLAttrIdentFuncTable);
  for I := Low(FMLTagIdentFuncTable) to High(FMLAttrIdentFuncTable) do
  begin
    pF^ := MLAttrUndef;
    Inc(pF);
  end;
  FMLAttrIdentFuncTable[13]:=MLAttrFunc13;
  FMLAttrIdentFuncTable[15]:=MLAttrFunc15;
  FMLAttrIdentFuncTable[23]:=MLAttrFunc23;
  FMLAttrIdentFuncTable[26]:=MLAttrFunc26;
  FMLAttrIdentFuncTable[27]:=MLAttrFunc27;
  FMLAttrIdentFuncTable[30]:=MLAttrFunc30;
  FMLAttrIdentFuncTable[31]:=MLAttrFunc31;
  FMLAttrIdentFuncTable[32]:=MLAttrFunc32;
  FMLAttrIdentFuncTable[33]:=MLAttrFunc33;
  FMLAttrIdentFuncTable[34]:=MLAttrFunc34;
  FMLAttrIdentFuncTable[35]:=MLAttrFunc35;
  FMLAttrIdentFuncTable[37]:=MLAttrFunc37;
  FMLAttrIdentFuncTable[38]:=MLAttrFunc38;
  FMLAttrIdentFuncTable[39]:=MLAttrFunc39;
  FMLAttrIdentFuncTable[40]:=MLAttrFunc40;
  FMLAttrIdentFuncTable[42]:=MLAttrFunc42;
  FMLAttrIdentFuncTable[43]:=MLAttrFunc43;
  FMLAttrIdentFuncTable[45]:=MLAttrFunc45;
  FMLAttrIdentFuncTable[46]:=MLAttrFunc46;
  FMLAttrIdentFuncTable[47]:=MLAttrFunc47;
  FMLAttrIdentFuncTable[48]:=MLAttrFunc48;
  FMLAttrIdentFuncTable[49]:=MLAttrFunc49;
  FMLAttrIdentFuncTable[50]:=MLAttrFunc50;
  FMLAttrIdentFuncTable[52]:=MLAttrFunc52;
  FMLAttrIdentFuncTable[53]:=MLAttrFunc53;
  FMLAttrIdentFuncTable[54]:=MLAttrFunc54;
  FMLAttrIdentFuncTable[55]:=MLAttrFunc55;
  FMLAttrIdentFuncTable[56]:=MLAttrFunc56;
  FMLAttrIdentFuncTable[57]:=MLAttrFunc57;
  FMLAttrIdentFuncTable[58]:=MLAttrFunc58;
  FMLAttrIdentFuncTable[59]:=MLAttrFunc59;
  FMLAttrIdentFuncTable[60]:=MLAttrFunc60;
  FMLAttrIdentFuncTable[61]:=MLAttrFunc61;
  FMLAttrIdentFuncTable[62]:=MLAttrFunc62;
  FMLAttrIdentFuncTable[63]:=MLAttrFunc63;
  FMLAttrIdentFuncTable[64]:=MLAttrFunc64;
  FMLAttrIdentFuncTable[65]:=MLAttrFunc65;
  FMLAttrIdentFuncTable[66]:=MLAttrFunc66;
  FMLAttrIdentFuncTable[67]:=MLAttrFunc67;
  FMLAttrIdentFuncTable[68]:=MLAttrFunc68;
  FMLAttrIdentFuncTable[69]:=MLAttrFunc69;
  FMLAttrIdentFuncTable[70]:=MLAttrFunc70;
  FMLAttrIdentFuncTable[71]:=MLAttrFunc71;
  FMLAttrIdentFuncTable[72]:=MLAttrFunc72;
  FMLAttrIdentFuncTable[73]:=MLAttrFunc73;
  FMLAttrIdentFuncTable[74]:=MLAttrFunc74;
  FMLAttrIdentFuncTable[75]:=MLAttrFunc75;
  FMLAttrIdentFuncTable[77]:=MLAttrFunc77;
  FMLAttrIdentFuncTable[78]:=MLAttrFunc78;
  FMLAttrIdentFuncTable[79]:=MLAttrFunc79;
  FMLAttrIdentFuncTable[80]:=MLAttrFunc80;
  FMLAttrIdentFuncTable[81]:=MLAttrFunc81;
  FMLAttrIdentFuncTable[82]:=MLAttrFunc82;
  FMLAttrIdentFuncTable[83]:=MLAttrFunc83;
  FMLAttrIdentFuncTable[85]:=MLAttrFunc85;
  FMLAttrIdentFuncTable[87]:=MLAttrFunc87;
  FMLAttrIdentFuncTable[88]:=MLAttrFunc88;
  FMLAttrIdentFuncTable[91]:=MLAttrFunc91;
  FMLAttrIdentFuncTable[93]:=MLAttrFunc93;
  FMLAttrIdentFuncTable[94]:=MLAttrFunc94;
  FMLAttrIdentFuncTable[96]:=MLAttrFunc96;
  FMLAttrIdentFuncTable[97]:=MLAttrFunc97;
  FMLAttrIdentFuncTable[98]:=MLAttrFunc98;
  FMLAttrIdentFuncTable[101]:=MLAttrFunc101;
  FMLAttrIdentFuncTable[102]:=MLAttrFunc102;
  FMLAttrIdentFuncTable[104]:=MLAttrFunc104;
  FMLAttrIdentFuncTable[105]:=MLAttrFunc105;
  FMLAttrIdentFuncTable[106]:=MLAttrFunc106;
  FMLAttrIdentFuncTable[107]:=MLAttrFunc107;
  FMLAttrIdentFuncTable[108]:=MLAttrFunc108;
  FMLAttrIdentFuncTable[109]:=MLAttrFunc109;
  FMLAttrIdentFuncTable[110]:=MLAttrFunc110;
  FMLAttrIdentFuncTable[111]:=MLAttrFunc111;
  FMLAttrIdentFuncTable[113]:=MLAttrFunc113;
  FMLAttrIdentFuncTable[114]:=MLAttrFunc114;
  FMLAttrIdentFuncTable[117]:=MLAttrFunc117;
  FMLAttrIdentFuncTable[119]:=MLAttrFunc119;
  FMLAttrIdentFuncTable[122]:=MLAttrFunc122;
  FMLAttrIdentFuncTable[126]:=MLAttrFunc126;
  FMLAttrIdentFuncTable[127]:=MLAttrFunc127;
  FMLAttrIdentFuncTable[132]:=MLAttrFunc132;
  FMLAttrIdentFuncTable[139]:=MLAttrFunc139;
  FMLAttrIdentFuncTable[143]:=MLAttrFunc143;
  FMLAttrIdentFuncTable[147]:=MLAttrFunc147;
  FMLAttrIdentFuncTable[154]:=MLAttrFunc154;
  FMLAttrIdentFuncTable[155]:=MLAttrFunc155;
  FMLAttrIdentFuncTable[157]:=MLAttrFunc157;
  FMLAttrIdentFuncTable[158]:=MLAttrFunc158;
  FMLAttrIdentFuncTable[160]:=MLAttrFunc160;
  FMLAttrIdentFuncTable[162]:=MLAttrFunc162;
  FMLAttrIdentFuncTable[176]:=MLAttrFunc176;

  pF2 := PSynWebIdent2FuncTableFunc(@FMLSpecialIdentFuncTable);
  for I := Low(FMLSpecialIdentFuncTable) to High(FMLSpecialIdentFuncTable) do
  begin
    pF2^ := MLSpecialUndef;
    Inc(pF2);
  end;
  FMLSpecialIdentFuncTable[12]:=MLSpecialFunc12;
  FMLSpecialIdentFuncTable[16]:=MLSpecialFunc16;
  FMLSpecialIdentFuncTable[17]:=MLSpecialFunc17;
  FMLSpecialIdentFuncTable[19]:=MLSpecialFunc19;
  FMLSpecialIdentFuncTable[20]:=MLSpecialFunc20;
  FMLSpecialIdentFuncTable[22]:=MLSpecialFunc22;
  FMLSpecialIdentFuncTable[23]:=MLSpecialFunc23;
  FMLSpecialIdentFuncTable[25]:=MLSpecialFunc25;
  FMLSpecialIdentFuncTable[26]:=MLSpecialFunc26;
  FMLSpecialIdentFuncTable[27]:=MLSpecialFunc27;
  FMLSpecialIdentFuncTable[28]:=MLSpecialFunc28;
  FMLSpecialIdentFuncTable[30]:=MLSpecialFunc30;
  FMLSpecialIdentFuncTable[32]:=MLSpecialFunc32;
  FMLSpecialIdentFuncTable[33]:=MLSpecialFunc33;
  FMLSpecialIdentFuncTable[34]:=MLSpecialFunc34;
  FMLSpecialIdentFuncTable[35]:=MLSpecialFunc35;
  FMLSpecialIdentFuncTable[36]:=MLSpecialFunc36;
  FMLSpecialIdentFuncTable[38]:=MLSpecialFunc38;
  FMLSpecialIdentFuncTable[39]:=MLSpecialFunc39;
  FMLSpecialIdentFuncTable[40]:=MLSpecialFunc40;
  FMLSpecialIdentFuncTable[41]:=MLSpecialFunc41;
  FMLSpecialIdentFuncTable[42]:=MLSpecialFunc42;
  FMLSpecialIdentFuncTable[43]:=MLSpecialFunc43;
  FMLSpecialIdentFuncTable[44]:=MLSpecialFunc44;
  FMLSpecialIdentFuncTable[45]:=MLSpecialFunc45;
  FMLSpecialIdentFuncTable[46]:=MLSpecialFunc46;
  FMLSpecialIdentFuncTable[47]:=MLSpecialFunc47;
  FMLSpecialIdentFuncTable[48]:=MLSpecialFunc48;
  FMLSpecialIdentFuncTable[49]:=MLSpecialFunc49;
  FMLSpecialIdentFuncTable[50]:=MLSpecialFunc50;
  FMLSpecialIdentFuncTable[51]:=MLSpecialFunc51;
  FMLSpecialIdentFuncTable[52]:=MLSpecialFunc52;
  FMLSpecialIdentFuncTable[53]:=MLSpecialFunc53;
  FMLSpecialIdentFuncTable[54]:=MLSpecialFunc54;
  FMLSpecialIdentFuncTable[55]:=MLSpecialFunc55;
  FMLSpecialIdentFuncTable[56]:=MLSpecialFunc56;
  FMLSpecialIdentFuncTable[57]:=MLSpecialFunc57;
  FMLSpecialIdentFuncTable[58]:=MLSpecialFunc58;
  FMLSpecialIdentFuncTable[59]:=MLSpecialFunc59;
  FMLSpecialIdentFuncTable[61]:=MLSpecialFunc61;
  FMLSpecialIdentFuncTable[62]:=MLSpecialFunc62;
  FMLSpecialIdentFuncTable[63]:=MLSpecialFunc63;
  FMLSpecialIdentFuncTable[64]:=MLSpecialFunc64;
  FMLSpecialIdentFuncTable[65]:=MLSpecialFunc65;
  FMLSpecialIdentFuncTable[66]:=MLSpecialFunc66;
  FMLSpecialIdentFuncTable[67]:=MLSpecialFunc67;
  FMLSpecialIdentFuncTable[68]:=MLSpecialFunc68;
  FMLSpecialIdentFuncTable[69]:=MLSpecialFunc69;
  FMLSpecialIdentFuncTable[70]:=MLSpecialFunc70;
  FMLSpecialIdentFuncTable[71]:=MLSpecialFunc71;
  FMLSpecialIdentFuncTable[72]:=MLSpecialFunc72;
  FMLSpecialIdentFuncTable[73]:=MLSpecialFunc73;
  FMLSpecialIdentFuncTable[74]:=MLSpecialFunc74;
  FMLSpecialIdentFuncTable[75]:=MLSpecialFunc75;
  FMLSpecialIdentFuncTable[76]:=MLSpecialFunc76;
  FMLSpecialIdentFuncTable[77]:=MLSpecialFunc77;
  FMLSpecialIdentFuncTable[78]:=MLSpecialFunc78;
  FMLSpecialIdentFuncTable[79]:=MLSpecialFunc79;
  FMLSpecialIdentFuncTable[81]:=MLSpecialFunc81;
  FMLSpecialIdentFuncTable[83]:=MLSpecialFunc83;
  FMLSpecialIdentFuncTable[84]:=MLSpecialFunc84;
  FMLSpecialIdentFuncTable[85]:=MLSpecialFunc85;
  FMLSpecialIdentFuncTable[86]:=MLSpecialFunc86;
  FMLSpecialIdentFuncTable[87]:=MLSpecialFunc87;
  FMLSpecialIdentFuncTable[89]:=MLSpecialFunc89;
  FMLSpecialIdentFuncTable[90]:=MLSpecialFunc90;
  FMLSpecialIdentFuncTable[91]:=MLSpecialFunc91;
  FMLSpecialIdentFuncTable[95]:=MLSpecialFunc95;
  FMLSpecialIdentFuncTable[106]:=MLSpecialFunc106;
  FMLSpecialIdentFuncTable[111]:=MLSpecialFunc111;
end;

procedure TSynWebEngine.MLNext;
begin
  FInstance^.FTokenPos := FInstance^.FRun;
  FMLRangeProcTable[MLGetRange];
end;

function TSynWebEngine.MLGetRange: TSynWebMLRangeState;
begin
  Result := TSynWebMLRangeState(GetRangeInt(4, 13));
end;

procedure TSynWebEngine.MLSetRange(const ARange: TSynWebMLRangeState);
begin
  SetRangeInt(4, 13, Longword(ARange));
end;

function TSynWebEngine.MLGetTag: Integer;
begin
  Result := GetRangeInt(7, 0);
end;

procedure TSynWebEngine.MLSetTag(const ATag: Integer);
begin
  SetRangeInt(7, 0, Longword(ATag));
end;

function TSynWebEngine.MLCheckNull(ADo: Boolean = True): Boolean;
begin
  if FInstance^.FLine[FInstance^.FRun] = #0 then
  begin
    Result := True;
    if ADo then
      NullProc;
  end else
    Result := False;
end;

procedure TSynWebEngine.MLSpaceProc;
begin
  repeat
    Inc(FInstance^.FRun);
  until not (FInstance^.FLine[FInstance^.FRun] in [#1..#32]);
  FInstance^.FTokenID := stkMLSpace;
end;

procedure TSynWebEngine.MLAmpersandProc;
begin
  Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkMLEscape;
  if FInstance^.FLine[FInstance^.FRun] = '#' then
  begin
    Inc(FInstance^.FRun);
    if FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun]] = FInstance^.FHashTable['x'] then
    begin
      Inc(FInstance^.FRun);
      // if not (FInstance^.FLine[FInstance^.FRun] in ['a'..'f', 'A'..'F', '0'..'9']) then
      if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 10) = 0 then
        FInstance^.FTokenID := stkMLError
      else
        repeat
          Inc(FInstance^.FRun)
        until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 10) = 0;
        // until not (FInstance^.FLine[FInstance^.FRun] in ['a'..'f', 'A'..'F', '0'..'9']);
    end else
      if not (FInstance^.FLine[FInstance^.FRun] in ['0'..'9']) then
        FInstance^.FTokenID := stkMLError
      else
        repeat
          Inc(FInstance^.FRun)
        until not (FInstance^.FLine[FInstance^.FRun] in ['0'..'9']);
  end else
    // if not (FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z'] then
    if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 0) = 0 then
      FInstance^.FTokenID := stkMLError
    else
    begin
      repeat
        Inc(FInstance^.FRun)
      until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 16) = 0;
      // until not (FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '_', '0'..'9'];
      if MLSpecialCheck(FInstance^.FTokenPos + 1, FInstance^.FRun -
        FInstance^.FTokenPos - 1) = -1 then
        FInstance^.FTokenID := stkMLError;
    end;
  if FInstance^.FLine[FInstance^.FRun] = ';' then
    Inc(FInstance^.FRun)
  else
    FInstance^.FTokenID := stkMLError;
end;

procedure TSynWebEngine.MLBraceOpenProc;
begin
  if PhpCheckBegin then
    Exit;
  Inc(FInstance^.FRun);
  case FInstance^.FLine[FInstance^.FRun] of
  '/':
    begin
      Inc(FInstance^.FRun);
      SetRangeBit(12, True);
    end;
  '?':
    begin
      if FInstance^.FOptions.FMLVersion >= smlhvXHtml10Strict then
        Inc(FInstance^.FRun);
      SetRangeBit(12, False);
    end;
  '!':
    begin
      Inc(FInstance^.FRun);
      if (FInstance^.FLine[FInstance^.FRun] = '-') and
        (FInstance^.FLine[FInstance^.FRun + 1] = '-') then
      begin
        Inc(FInstance^.FRun, 2);
        MLSetRange(srsMLComment);
        if (FInstance^.FLine[FInstance^.FRun] = #0) or PhpCheckBegin(False) then
          FInstance^.FTokenID := stkMLComment
        else
          MLRangeCommentProc;
      end else
        if (FInstance^.FOptions.FMLVersion >= smlhvXHtml10Strict) and
          (FInstance^.FLine[FInstance^.FRun] = '[') and
          (FInstance^.FLine[FInstance^.FRun + 1] = 'C') and
          (FInstance^.FLine[FInstance^.FRun + 2] = 'D') and
          (FInstance^.FLine[FInstance^.FRun + 3] = 'A') and
          (FInstance^.FLine[FInstance^.FRun + 4] = 'T') and
          (FInstance^.FLine[FInstance^.FRun + 5] = 'A') and
          (FInstance^.FLine[FInstance^.FRun + 6] = '[') then
        begin
          Inc(FInstance^.FRun, 7);
          FInstance^.FTokenID := stkMLTag;
          MLSetRange(srsMLTagCDATA);
        end else
          if (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun]] =
            FInstance^.FHashTable['D']) and
            (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 1]] =
            FInstance^.FHashTable['O']) and
            (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 2]] =
            FInstance^.FHashTable['C']) and
            (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 3]] =
            FInstance^.FHashTable['T']) and
            (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 4]] =
            FInstance^.FHashTable['Y']) and
            (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 5]] =
            FInstance^.FHashTable['P']) and
            (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 6]] =
            FInstance^.FHashTable['E']) and
            // (not (FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z'])) then
            (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 7]] and (1 shl 0) = 0) then
          begin
            FInstance^.FTokenID := stkMLTag;
            SetRangeInt(2, 7, 0);
            MLSetRange(srsMLTagDOCTYPE);
          end else
            FInstance^.FTokenID := stkMLError;
      Exit;
    end;
  else // case
    SetRangeBit(12, False);
  end;
  // if FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z'] then
  if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 0) <> 0 then
  begin
    FInstance^.FTokenID := stkMLTag;
    MLSetRange(srsMLTag);
  end else
  begin
    FInstance^.FTokenID := stkMLError;
    SetRangeBit(12, False);
  end;
end;

procedure TSynWebEngine.MLErrorProc;
begin
  Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkMLError;
end;

procedure TSynWebEngine.MLRangeTextProc;
begin
  case FInstance^.FLine[FInstance^.FRun] of
  #0:
    NullProc;
  #1..#32:
    MLSpaceProc;
  '<':
    MLBraceOpenProc;
  '>':
    MLErrorProc;
  '&':
    MLAmpersandProc;
  else // case
    repeat
      Inc(FInstance^.FRun);
    until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 6) <> 0;
    // until FInstance^.FLine[FInstance^.FRun] In [#0..#32, '<', '>', '&'];
    FInstance^.FTokenID := stkMLText;
  end;
end;

procedure TSynWebEngine.MLRangeCommentProc;
begin
  if MLCheckNull or PhpCheckBegin then
    Exit;
  repeat
    // while not (FInstance^.FLine[FInstance^.FRun) in [#0, '-', '<']) do
    while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 19) = 0 do
      Inc(FInstance^.FRun);
    case FInstance^.FLine[FInstance^.FRun] of
    #0:
      Break;
    '-':
      begin
        Inc(FInstance^.FRun);
        if FInstance^.FLine[FInstance^.FRun] = '-' then
        begin
          Inc(FInstance^.FRun);
          if FInstance^.FLine[FInstance^.FRun] = '>' then
          begin
            Inc(FInstance^.FRun);
            MLSetRange(srsMLText);
          end else
          begin
            MLSetRange(srsMLCommentClose);
            if (FInstance^.FLine[FInstance^.FRun] <> #0) and not PhpCheckBegin(False) then
            begin
              MLRangeCommentCloseProc;
              Exit;
            end;
          end;
          Break;
        end;
      end;
    '<':
      if PhpCheckBegin(False) then
        Break
      else
        Inc(FInstance^.FRun);
    end;
  until False;
  FInstance^.FTokenID := stkMLComment;
end;

procedure TSynWebEngine.MLRangeCommentCloseProc;
begin
  if MLCheckNull or PhpCheckBegin then
    Exit;
  repeat
    // while not (FInstance^.FLine[FInstance^.FRun) in [#0, '<', '>']) do
    while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 20) = 0 do
      Inc(FInstance^.FRun);
    case FInstance^.FLine[FInstance^.FRun] of
    #0:
      Break;
    '>':
      begin
        Inc(FInstance^.FRun);
        MLSetRange(srsMLText);
        Break;
      end;
    '<':
      if PhpCheckBegin(False) then
        Break
      else
        Inc(FInstance^.FRun);
    end;
  until False;
  FInstance^.FTokenID := stkMLComment;
end;

procedure TSynWebEngine.MLRangeTagDOCTYPEProc;
begin
  case GetRangeInt(2, 7) of
  0:
    begin
      Inc(FInstance^.FRun, 7);
      FInstance^.FTokenID := stkMLTagName;
      SetRangeInt(2, 7, 1);
    end;
  1:
    if not MLCheckNull and not PhpCheckBegin then
      case FInstance^.FLine[FInstance^.FRun] of
      #1..#32:
        begin
          MLSpaceProc;
          Exit;
        end;
      '>':
        begin
          Inc(FInstance^.FRun);
          FInstance^.FTokenID := stkMLTag;
          SetRangeInt(2, 7, 0);
          MLSetRange(srsMLText);
          Exit;
        end;
      #39:
        begin
          Inc(FInstance^.FRun);
          if FInstance^.FLine[FInstance^.FRun] = #0 then
            FInstance^.FTokenID := stkMLError
          else
          begin
            SetRangeInt(2, 7, 2);
            if PhpCheckBegin(False) then
              FInstance^.FTokenID := stkMLTagKeyValueQuoted
            else
              MLRangeTagDOCTYPEProc;
          end;
        end;
      '"':
        begin
          Inc(FInstance^.FRun);
          if FInstance^.FLine[FInstance^.FRun] = #0 then
            FInstance^.FTokenID := stkMLError
          else
          begin
            SetRangeInt(2, 7, 3);
            if PhpCheckBegin(False) then
              FInstance^.FTokenID := stkMLTagKeyValueQuoted
            else
              MLRangeTagDOCTYPEProc;
          end;
        end;
      else // case
        // if not (FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z']) then
        if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 0) = 0 then
        begin
          Inc(FInstance^.FRun);
          FInstance^.FTokenID := stkMLError;
          Exit;
        end;
        repeat
          Inc(FInstance^.FRun);
        until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 0) = 0;
        // until not (FInstance^.FLine[FInstance^.FRun] In ['a'..'z', 'A'..'Z']);
        FInstance^.FTokenID := stkMLTagKey;
      end;
  2:
    begin
      if not MLCheckNull then
        if PhpCheckBegin then
          Exit
        else
          repeat
            // while not (FInstance^.FLine[FInstance^.FRun] in [#0, #39, '<']) do
            while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 21) = 0 do
              Inc(FInstance^.FRun);
            case FInstance^.FLine[FInstance^.FRun] of
            #0:
              begin
                FInstance^.FTokenID := stkMLError;
                Break;
              end;
            '<':
              if PhpCheckBegin(False) then
              begin
                FInstance^.FTokenID := stkMLTagKeyValueQuoted;
                Exit;
              end else
                Inc(FInstance^.FRun);
            #39:
              begin
                Inc(FInstance^.FRun);
                FInstance^.FTokenID := stkMLTagKeyValueQuoted;
                Break;
              end;
            end;
          until False;
      SetRangeInt(2, 7, 1);
    end;
  3:
    begin
      if not MLCheckNull then
        if PhpCheckBegin then
          Exit
        else
          repeat
            // while not (FInstance^.FLine[FInstance^.FRun] in [#0, '"', '<']) do
            while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 22) = 0 do
              Inc(FInstance^.FRun);
            case FInstance^.FLine[FInstance^.FRun] of
            #0:
              begin
                FInstance^.FTokenID := stkMLError;
                Break;
              end;
            '<':
              if PhpCheckBegin(False) then
              begin
                FInstance^.FTokenID := stkMLTagKeyValueQuoted;
                Exit;
              end else
                Inc(FInstance^.FRun);
            '"':
              begin
                Inc(FInstance^.FRun);
                FInstance^.FTokenID := stkMLTagKeyValueQuoted;
                Break;
              end;
            end;
          until False;
      SetRangeInt(2, 7, 1);
    end;
  end;
end;

procedure TSynWebEngine.MLRangeTagCDATAProc;
begin
  if MLCheckNull or PhpCheckBegin then
    Exit;
  if FInstance^.FLine[FInstance^.FRun] in [#1..#32] then
  begin
    MLSpaceProc;
    Exit;
  end else
    if (FInstance^.FLine[FInstance^.FRun] = ']') and
      (FInstance^.FLine[FInstance^.FRun + 1] = ']') and
      (FInstance^.FLine[FInstance^.FRun + 2] = '>') then
    begin
      Inc(FInstance^.FRun, 3);
      FInstance^.FTokenID := stkMLTag;
      MLSetRange(srsMLText);
    end else
    begin
      repeat
        repeat
          Inc(FInstance^.FRun);
        until TSynWebIdentTable2[FInstance^.FLine[FInstance^.FRun]] and (1 shl 1) <> 0;
        // until FInstance^.FLine[FInstance^.FRun] in [#0..#32, '<', ']'];
        case FInstance^.FLine[FInstance^.FRun] of
        #0..#32, ']':
          Break;
        '<':
          if PhpCheckBegin(False) then
            Break;
        end;
      until False;
      FInstance^.FTokenID := stkMLText;
    end;
end;

procedure TSynWebEngine.MLRangeTagProc;
var
  ID: Integer;
begin
  repeat
    Inc(FInstance^.FRun);
  until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 16) = 0;
  // until not (FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '_', '0'..'9']);

  if FInstance^.FLine[FInstance^.FRun] = ':' then
    Inc(FInstance^.FRun);

  // while FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '_', '0'..'9'] do
  while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 16) <> 0 do
    Inc(FInstance^.FRun);

  if FInstance^.FOptions.FMLVersion = smlwvXML then
  begin
    FInstance^.FTokenID := stkMLTagName;
    FInstance^.FTokenLastID := -1;    
    if GetRangeBit(12) then
      MLSetRange(srsMLTagClose)
    else           
      MLSetRange(srsMLTagKey);
  end else
  begin
    FInstance^.FTokenID := MLTagCheck;
    ID := MLGetTag - 1;
    if GetRangeBit(12) then
    begin
      if (ID <> -1) and (TSynWeb_TagsData[ID] and (1 shl 31) <> 0) then
        FInstance^.FTokenID := stkMLError;
      MLSetRange(srsMLTagClose);
    end else
    begin
      if (ID <> -1) and ((FInstance^.FLine[FInstance^.FTokenPos - 1] = '?') xor
        (TSynWeb_TagsData[ID] and (1 shl 29) <> 0)) then
        FInstance^.FTokenID := stkMLError;
      MLSetRange(srsMLTagKey);
    end;
  end;
end;

procedure TSynWebEngine.MLRangeTagCloseProc;
begin
  if MLCheckNull or PhpCheckBegin then
    Exit;
  case FInstance^.FLine[FInstance^.FRun] of
  #1..#32:
    MLSpaceProc;
  '>':
    begin
      Inc(FInstance^.FRun);
      FInstance^.FTokenID := stkMLTag;
      MLSetRange(srsMLText);
    end;
  else // case
    FInstance^.FTokenID := stkMLError;
    repeat
      repeat
        Inc(FInstance^.FRun);
      until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 1) <> 0;
      // until not (FInstance^.FLine[FInstance^.FRun] In [#0..#32, '<', '>']) do
      if (FInstance^.FLine[FInstance^.FRun] = '<') and not PhpCheckBegin(False) then
        Continue
      else
        Break;
    until False;
  end;
end;

procedure TSynWebEngine.MLRangeTagKeyProc;
var
  ID: Integer;
begin
  if MLCheckNull or PhpCheckBegin then
    Exit;

  if FInstance^.FOptions.FMLVersion = smlwvXML then
  begin
    if (FInstance^.FLine[FInstance^.FRun] = '?') and
      (FInstance^.FLine[FInstance^.FRun + 1] = '>') then
    begin
      Inc(FInstance^.FRun, 2);
      FInstance^.FTokenID := stkMLTag;
      MLSetRange(srsMLText);
      Exit;
    end;

    case FInstance^.FLine[FInstance^.FRun] of
    #1..#32:
      MLSpaceProc;
    '/':
      if not GetRangeBit(12) and (FInstance^.FLine[FInstance^.FRun + 1] = '>') then
      begin
        Inc(FInstance^.FRun, 2);
        FInstance^.FTokenID := stkMLTag;
        MLSetRange(srsMLText);
      end else
      begin
        Inc(FInstance^.FRun);
        FInstance^.FTokenID := stkMLError;
      end;
    '>':
      begin
        Inc(FInstance^.FRun);
        FInstance^.FTokenID := stkMLTag;
        MLSetRange(srsMLText);
      end;
    else // case
      // if not (FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z']) then
      if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 0) = 0 then
        MLErrorProc
      else
      begin
        repeat
          Inc(FInstance^.FRun);
        until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 7) = 0;
        // until not(FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', ':', '-']);
        FInstance^.FTokenID := stkMLTagKey;
        FInstance^.FTokenLastID := -1;
      end;
      MLSetRange(srsMLTagKeyEq);
    end;
  end else
  begin
    ID := MLGetTag - 1;
    if (ID <> -1) and (TSynWeb_TagsData[ID] and (1 shl 29) <> 0) then
      if (FInstance^.FLine[FInstance^.FRun] = '?') and
        (FInstance^.FLine[FInstance^.FRun + 1] = '>') then
      begin
        Inc(FInstance^.FRun, 2);
        FInstance^.FTokenID := stkMLTag;
        MLSetRange(srsMLText);
        Exit;
      end else
        if FInstance^.FLine[FInstance^.FRun] = '>' then
        begin
          Inc(FInstance^.FRun);
          FInstance^.FTokenID := stkMLError;
          MLSetRange(srsMLText);
          Exit;
        end;
    case FInstance^.FLine[FInstance^.FRun] of
    #1..#32:
      MLSpaceProc;
    '/':
      if not GetRangeBit(12) and (FInstance^.FLine[FInstance^.FRun + 1] = '>') and
        (FInstance^.FOptions.FMLVersion >= smlhvXHtml10Strict) and
        (TSynWeb_TagsData[ID] and (1 shl 31) <> 0) then
      begin
        Inc(FInstance^.FRun, 2);
        FInstance^.FTokenID := stkMLTag;
        MLSetRange(srsMLText);
      end else
      begin
        Inc(FInstance^.FRun);
        FInstance^.FTokenID := stkMLError;
      end;
    '>':
      begin
        Inc(FInstance^.FRun);
        FInstance^.FTokenID := stkMLTag;
        if (ID <> -1) and (TSynWeb_TagsData[ID] and (1 shl 31) <> 0) and
          (FInstance^.FOptions.FMLVersion >= smlhvXHtml10Strict) then
          FInstance^.FTokenID := stkMLError
        else
          if not GetRangeBit(12) and ((FInstance^.FRun < 2) or
            (FInstance^.FLine[FInstance^.FRun - 2] <> '/')) then
            if (ID = MLTagID_Style) and FInstance^.FOptions.FCssEmbeded then
            begin
              SetHighlighterType(shtCss, True, True, True);
              Exit;
            end else
              if (ID = MLTagID_Script) then
                if GetRangeBit(28) and FInstance^.FOptions.FPhpEmbeded then
                begin
                  SetRangeInt(17, 0, 0);
                  PhpBegin(spotML);
                  Exit;
                end else
                  if FInstance^.FOptions.FEsEmbeded then
                  begin
                    SetHighlighterType(shtEs, True, True, True);
                    Exit;
                  end;
        MLSetRange(srsMLText);
      end;
    else // case
      // if not (FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z']) then
      if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 0) = 0 then
        MLErrorProc
      else
      begin
        repeat
          Inc(FInstance^.FRun);
        until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 7) = 0;
        // until not(FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', ':', '-']);
        if ID = -1 then
          FInstance^.FTokenID := stkMLTagKeyUndef
        else
        begin
          FInstance^.FTokenID := MLAttrCheck;
          if ID = MLTagID_Script then
            SetRangeBit(27, FInstance^.FTokenLastID = MLAttrID_Language);
        end;
      end;
      MLSetRange(srsMLTagKeyEq);
    end;
  end;
end;

procedure TSynWebEngine.MLRangeTagKeyEqProc;
begin
  if MLCheckNull or PhpCheckBegin then
    Exit;
  case FInstance^.FLine[FInstance^.FRun] of
  #1..#32:
    MLSpaceProc;
  '=':
    begin
      Inc(FInstance^.FRun);
      FInstance^.FTokenID := stkMLSymbol;
      MLSetRange(srsMLTagKeyValue);
    end;
  else // case
    MLSetRange(srsMLTagKey);
    MLRangeTagKeyProc;
    if FInstance^.FOptions.FMLVersion >= smlhvXHtml10Strict then
      FInstance^.FTokenID := stkMLError;
  end;
end;

procedure TSynWebEngine.MLRangeTagKeyValueProc;
var
  ID: Integer;
begin
  if MLCheckNull or PhpCheckBegin then
    Exit;
  case FInstance^.FLine[FInstance^.FRun] of
  #1..#32:
    MLSpaceProc;
  #39:
    begin
      Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] = #0 then
      begin
        MLSetRange(srsMLTagKey);
        FInstance^.FTokenID := stkMLError;
      end else
      begin
        MLSetRange(srsMLTagKeyValueQuoted1);
        if PhpCheckBegin(False) then
          FInstance^.FTokenID := stkMLTagKeyValueQuoted
        else
          MLRangeTagKeyValueQuoted1Proc;
      end;
    end;
  '"':
    begin
      Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] = #0 then
      begin
        MLSetRange(srsMLTagKey);
        FInstance^.FTokenID := stkMLError;
      end else
      begin
        MLSetRange(srsMLTagKeyValueQuoted2);
        if PhpCheckBegin(False) then
          FInstance^.FTokenID := stkMLTagKeyValueQuoted
        else
          MLRangeTagKeyValueQuoted2Proc;
      end;
    end;
  else // case
    if (FInstance^.FLine[FInstance^.FRun] = '>') or
      ((FInstance^.FOptions.FMLVersion >= smlhvXHtml10Strict) and
      (FInstance^.FLine[FInstance^.FRun] = '/') and
      (FInstance^.FLine[FInstance^.FRun + 1] = '>')) then
    begin
      if FInstance^.FLine[FInstance^.FRun] = '/' then
        Inc(FInstance^.FRun, 2)
      else
        Inc(FInstance^.FRun);
      FInstance^.FTokenID := stkMLError;
      if not GetRangeBit(12) and ((FInstance^.FRun = 0) or
        (FInstance^.FLine[FInstance^.FRun - 2] <> '/')) then
      begin
        ID := MLGetTag - 1;
        if (ID = MLTagID_Style) and FInstance^.FOptions.FCssEmbeded then
        begin
          SetHighlighterType(shtCss, True, True, True);
          Exit;
        end else
          if (ID = MLTagID_Script) then
            if GetRangeBit(28) and FInstance^.FOptions.FPhpEmbeded then
            begin
              SetRangeInt(17, 0, 0);
              PhpBegin(spotML);
              Exit;
            end else
              if FInstance^.FOptions.FEsEmbeded then
              begin
                SetHighlighterType(shtEs, True, True, True);
                Exit;
              end;
      end;
      MLSetRange(srsMLText);
    end else
    begin
      repeat
        repeat
          Inc(FInstance^.FRun);
        until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 23) <> 0;
        // until FInstance^.FLine[FInstance^.FRun] in [#0..#32, '<', '>', '/'];
        case FInstance^.FLine[FInstance^.FRun] of
        '/':
          if (FInstance^.FLine[FInstance^.FRun + 1] = '>') and
            (FInstance^.FOptions.FMLVersion >= smlhvXHtml10Strict) then
            Break;
        '<':
          if PhpCheckBegin(False) then
            Break
          else
            Inc(FInstance^.FRun);
        else // case
          Break;
        end;
      until False;
      if FInstance^.FOptions.FMLVersion >= smlhvXHtml10Strict then
        FInstance^.FTokenID := stkMLError
      else
        FInstance^.FTokenID := stkMLTagKeyValue;
      if GetRangeBit(27) then
        SetRangeBit(28, UpperCase(GetToken) = 'PHP');
      MLSetRange(srsMLTagKey);
    end;
  end;
end;

procedure TSynWebEngine.MLRangeTagKeyValueQuoted1Proc;
begin
  if not MLCheckNull then
    if PhpCheckBegin then
      Exit
    else
      repeat
        // while not (FInstance^.FLine[FInstance^.FRun] in [#0, #39, '<']) do
        while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 21) = 0 do
          Inc(FInstance^.FRun);
        case FInstance^.FLine[FInstance^.FRun] of
        #0:
          begin
            FInstance^.FTokenID := stkMLError;
            Break;
          end;
        '<':
          if PhpCheckBegin(False) then
          begin
            FInstance^.FTokenID := stkMLTagKeyValueQuoted;
            Exit;
          end else
            Inc(FInstance^.FRun);
        #39:
          begin
            Inc(FInstance^.FRun);
            FInstance^.FTokenID := stkMLTagKeyValueQuoted;
            if GetRangeBit(27) then
              SetRangeBit(28, UpperCase(GetToken) = #39'PHP'#39);
            Break;
          end;
        end;
      until False;
  MLSetRange(srsMLTagKey);
end;

procedure TSynWebEngine.MLRangeTagKeyValueQuoted2Proc;
begin
  if not MLCheckNull then
    if PhpCheckBegin then
      Exit
    else
      repeat
        // while not (FInstance^.FLine[FInstance^.FRun] in [#0, '"', '<']) do
        while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 22) = 0 do
          Inc(FInstance^.FRun);
        case FInstance^.FLine[FInstance^.FRun] of
        #0:
          begin
            FInstance^.FTokenID := stkMLError;
            Break;
          end;
        '<':
          if PhpCheckBegin(False) then
          begin
            FInstance^.FTokenID := stkMLTagKeyValueQuoted;
            Exit;
          end else
            Inc(FInstance^.FRun);
        '"':
          begin
            Inc(FInstance^.FRun);
            FInstance^.FTokenID := stkMLTagKeyValueQuoted;
            if GetRangeBit(27) then
              SetRangeBit(28, UpperCase(GetToken) = '"PHP"');
            Break;
          end;
        end;
      until False;
  MLSetRange(srsMLTagKey);
end;

function TSynWebEngine.MLTagKeyComp(const ID: Integer): Boolean;
var
  I: Integer;
  Temp: PChar;
  aKey: String;
begin
  if TSynWeb_TagsData[ID] and (1 shl Longword(FInstance^.FOptions.FMLVersion)) = 0 then
  begin
    Result := False;
    Exit;
  end;
  aKey := TSynWeb_Tags[ID];
  Temp := FInstance^.FToIdent;
  if Length(aKey) = FInstance^.FStringLen then
  begin
    for i := 1 to FInstance^.FStringLen do
    begin
      if FInstance^.FHashTable[Temp^] <> FInstance^.FHashTable[aKey[i]] then
      begin
        Result := False;
        Exit;
      end;
      Inc(Temp);
    end;
    FInstance^.FTokenLastID := ID;
    Result := True;
  end else
    Result := False;
end;

function TSynWebEngine.MLTagCheck: TSynWebTokenKind;
var
  HashKey: Longword;

  procedure KeyHash(ToHash: PChar);
  var
    i: Integer;
  begin
    HashKey := 0;
    FInstance^.FStringLen := FInstance^.FRun - FInstance^.FTokenPos;
    for i := 0 to FInstance^.FStringLen - 1 do
    begin
      Inc(HashKey, TSynWebIdentHashTable[ToHash^]);
      Inc(ToHash);
    end;
  end;

begin
  FInstance^.FToIdent := @FInstance^.FLine[FInstance^.FTokenPos];
  KeyHash(FInstance^.FToIdent);
  FInstance^.FTokenLastID := -1;
  if HashKey <= MLTagMaxKeyHash then
    Result := FMLTagIdentFuncTable[HashKey]
  else
    Result := stkMLTagNameUndef;
  MLSetTag(FInstance^.FTokenLastID + 1);
end;

function TSynWebEngine.MLTagUndef: TSynWebTokenKind;
begin
  Result:=stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc1: TSynWebTokenKind;
begin
  if  MLTagKeyComp(0) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc2: TSynWebTokenKind;
begin
  if  MLTagKeyComp(8) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc8: TSynWebTokenKind;
begin
  if  MLTagKeyComp(24) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc9: TSynWebTokenKind;
begin
  if  MLTagKeyComp(48) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc16: TSynWebTokenKind;
begin
  if  MLTagKeyComp(29) or
      MLTagKeyComp(70) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc17: TSynWebTokenKind;
begin
  if  MLTagKeyComp(54) or
      MLTagKeyComp(75) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc18: TSynWebTokenKind;
begin
  if  MLTagKeyComp(12) or
      MLTagKeyComp(32) or
      MLTagKeyComp(45) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc19: TSynWebTokenKind;
begin
  if  MLTagKeyComp(30) or
      MLTagKeyComp(77) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc20: TSynWebTokenKind;
begin
  if  MLTagKeyComp(15) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc21: TSynWebTokenKind;
begin
  if  MLTagKeyComp(11) or
      MLTagKeyComp(25) or
      MLTagKeyComp(57) or
      MLTagKeyComp(101) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc22: TSynWebTokenKind;
begin
  if  MLTagKeyComp(38) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc23: TSynWebTokenKind;
begin
  if  MLTagKeyComp(1) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc24: TSynWebTokenKind;
begin
  if  MLTagKeyComp(26) or
      MLTagKeyComp(31) or
      MLTagKeyComp(91) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc25: TSynWebTokenKind;
begin
  if  MLTagKeyComp(7) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc26: TSynWebTokenKind;
begin
  if  MLTagKeyComp(18) or
      MLTagKeyComp(46) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc27: TSynWebTokenKind;
begin
  if  MLTagKeyComp(9) or
      MLTagKeyComp(21) or
      MLTagKeyComp(66) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc28: TSynWebTokenKind;
begin
  if  MLTagKeyComp(95) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc29: TSynWebTokenKind;
begin
  if  MLTagKeyComp(50) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc30: TSynWebTokenKind;
begin
  if  MLTagKeyComp(22) or
      MLTagKeyComp(59) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc31: TSynWebTokenKind;
begin
  if  MLTagKeyComp(27) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc32: TSynWebTokenKind;
begin
  if  MLTagKeyComp(55) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc33: TSynWebTokenKind;
begin
  if  MLTagKeyComp(102) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc35: TSynWebTokenKind;
begin
  if  MLTagKeyComp(28) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc36: TSynWebTokenKind;
begin
  if  MLTagKeyComp(39) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc37: TSynWebTokenKind;
begin
  if  MLTagKeyComp(20) or
      MLTagKeyComp(40) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc38: TSynWebTokenKind;
begin
  if  MLTagKeyComp(41) or
      MLTagKeyComp(96) or
      MLTagKeyComp(99) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc39: TSynWebTokenKind;
begin
  if  MLTagKeyComp(42) or
      MLTagKeyComp(61) or
      MLTagKeyComp(73) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc40: TSynWebTokenKind;
begin
  if  MLTagKeyComp(43) or
      MLTagKeyComp(89) or
      MLTagKeyComp(100) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc41: TSynWebTokenKind;
begin
  if  MLTagKeyComp(44) or
      MLTagKeyComp(103) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc42: TSynWebTokenKind;
begin
  if  MLTagKeyComp(52) or
      MLTagKeyComp(87) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc43: TSynWebTokenKind;
begin
  if  MLTagKeyComp(36) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc46: TSynWebTokenKind;
begin
  if  MLTagKeyComp(14) or
      MLTagKeyComp(58) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc47: TSynWebTokenKind;
begin
  if  MLTagKeyComp(56) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc48: TSynWebTokenKind;
begin
  if  MLTagKeyComp(104) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc49: TSynWebTokenKind;
begin
  if  MLTagKeyComp(71) or
      MLTagKeyComp(78) or
      MLTagKeyComp(105) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc50: TSynWebTokenKind;
begin
  if  MLTagKeyComp(2) or
      MLTagKeyComp(83) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc52: TSynWebTokenKind;
begin
  if  MLTagKeyComp(35) or
      MLTagKeyComp(49) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc53: TSynWebTokenKind;
begin
  if  MLTagKeyComp(47) or
      MLTagKeyComp(60) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc55: TSynWebTokenKind;
begin
  if  MLTagKeyComp(34) or
      MLTagKeyComp(65) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc56: TSynWebTokenKind;
begin
  if  MLTagKeyComp(88) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc57: TSynWebTokenKind;
begin
  if  MLTagKeyComp(82) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc59: TSynWebTokenKind;
begin
  if  MLTagKeyComp(5) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc60: TSynWebTokenKind;
begin
  if  MLTagKeyComp(63) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc61: TSynWebTokenKind;
begin
  if  MLTagKeyComp(74) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc64: TSynWebTokenKind;
begin
  if  MLTagKeyComp(80) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc65: TSynWebTokenKind;
begin
  if  MLTagKeyComp(19) or
      MLTagKeyComp(97) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc66: TSynWebTokenKind;
begin
  if  MLTagKeyComp(90) or
      MLTagKeyComp(98) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc70: TSynWebTokenKind;
begin
  if  MLTagKeyComp(4) or
      MLTagKeyComp(6) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc76: TSynWebTokenKind;
begin
  if  MLTagKeyComp(94) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc78: TSynWebTokenKind;
begin
  if  MLTagKeyComp(17) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc79: TSynWebTokenKind;
begin
  if  MLTagKeyComp(76) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc80: TSynWebTokenKind;
begin
  if  MLTagKeyComp(33) or
      MLTagKeyComp(51) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc81: TSynWebTokenKind;
begin
  if  MLTagKeyComp(86) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc82: TSynWebTokenKind;
begin
  if  MLTagKeyComp(10) or
      MLTagKeyComp(84) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc84: TSynWebTokenKind;
begin
  if  MLTagKeyComp(53) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc85: TSynWebTokenKind;
begin
  if  MLTagKeyComp(79) or
      MLTagKeyComp(81) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc87: TSynWebTokenKind;
begin
  if  MLTagKeyComp(37) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc89: TSynWebTokenKind;
begin
  if  MLTagKeyComp(3) or
      MLTagKeyComp(69) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc91: TSynWebTokenKind;
begin
  if  MLTagKeyComp(62) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc92: TSynWebTokenKind;
begin
  if  MLTagKeyComp(16) or
      MLTagKeyComp(92) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc93: TSynWebTokenKind;
begin
  if  MLTagKeyComp(85) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc94: TSynWebTokenKind;
begin
  if  MLTagKeyComp(93) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc95: TSynWebTokenKind;
begin
  if  MLTagKeyComp(67) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc106: TSynWebTokenKind;
begin
  if  MLTagKeyComp(72) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc107: TSynWebTokenKind;
begin
  if  MLTagKeyComp(23) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc114: TSynWebTokenKind;
begin
  if  MLTagKeyComp(64) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc121: TSynWebTokenKind;
begin
  if  MLTagKeyComp(13) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLTagFunc128: TSynWebTokenKind;
begin
  if  MLTagKeyComp(68) then
    Result := stkMLTagName
  else
    Result := stkMLTagNameUndef;
end;

function TSynWebEngine.MLAttrKeyComp(const ID: Integer): Boolean;
var
  I, tag: Integer;
  Temp: PChar;
  aKey: String;
begin
  tag := MLGetTag - 1;
  if (tag = -1) or (TSynWeb_AttrsData[ID][Longword(FInstance^.FOptions.FMLVersion)]
    [tag div 32] and (1 shl (tag mod 32)) = 0) then
  begin
    Result := False;
    Exit;
  end;
  aKey := TSynWeb_Attrs[ID];
  Temp := FInstance^.FToIdent;
  if Length(aKey) = FInstance^.FStringLen then
  begin
    for i := 1 to FInstance^.FStringLen do
    begin
      if FInstance^.FHashTable[Temp^] <> FInstance^.FHashTable[aKey[i]] then
      begin
        Result := False;
        Exit;
      end;
      Inc(Temp);
    end;
    FInstance^.FTokenLastID := ID;
    Result := True;
  end else
    Result := False;
end;

function TSynWebEngine.MLAttrCheck: TSynWebTokenKind;
var
  HashKey: Longword;

  procedure KeyHash(ToHash: PChar);
  var
    i: Integer;
  begin
    HashKey := 0;
    FInstance^.FStringLen := FInstance^.FRun - FInstance^.FTokenPos;
    for i := 0 to FInstance^.FStringLen - 1 do
    begin
      Inc(HashKey, TSynWebIdentHashTable[ToHash^]);
      Inc(ToHash);
    end;
  end;

begin
  FInstance^.FToIdent := @FInstance^.FLine[FInstance^.FTokenPos];
  KeyHash(FInstance^.FToIdent);
  FInstance^.FTokenLastID := -1;
  if HashKey <= MLAttrMaxKeyHash then
    Result := FMLAttrIdentFuncTable[HashKey]
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrUndef: TSynWebTokenKind;
begin
  Result:=stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc13: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(61) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc15: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(49) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc23: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(0) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc26: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(34) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc27: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(24) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc30: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(16) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc31: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(42) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc32: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(65) or
      MLAttrKeyComp(74) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc33: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(7) or
      MLAttrKeyComp(78) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc34: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(66) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc35: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(113) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc37: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(20) or
      MLAttrKeyComp(57) or
      MLAttrKeyComp(76) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc38: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(41) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc39: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(19) or
      MLAttrKeyComp(23) or
      MLAttrKeyComp(50) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc40: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(126) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc42: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(62) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc43: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(5) or
      MLAttrKeyComp(53) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc45: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(109) or
      MLAttrKeyComp(114) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc46: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(68) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc47: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(6) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc48: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(1) or
      MLAttrKeyComp(35) or
      MLAttrKeyComp(40) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc49: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(28) or
      MLAttrKeyComp(123) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc50: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(125) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc52: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(59) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc53: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(9) or
      MLAttrKeyComp(118) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc54: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(21) or
      MLAttrKeyComp(25) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc55: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(84) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc56: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(43) or
      MLAttrKeyComp(44) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc57: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(17) or
      MLAttrKeyComp(56) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc58: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(63) or
      MLAttrKeyComp(119) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc59: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(124) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc60: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(55) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc61: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(52) or
      MLAttrKeyComp(95) or
      MLAttrKeyComp(138) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc62: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(4) or
      MLAttrKeyComp(12) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc63: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(27) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc64: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(143) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc65: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(75) or
      MLAttrKeyComp(137) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc66: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(8) or
      MLAttrKeyComp(38) or
      MLAttrKeyComp(48) or
      MLAttrKeyComp(80) or
      MLAttrKeyComp(82) or
      MLAttrKeyComp(134) or
      MLAttrKeyComp(135) or
      MLAttrKeyComp(142) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc67: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(22) or
      MLAttrKeyComp(86) or
      MLAttrKeyComp(87) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc68: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(67) or
      MLAttrKeyComp(101) or
      MLAttrKeyComp(141) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc69: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(108) or
      MLAttrKeyComp(133) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc70: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(64) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc71: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(31) or
      MLAttrKeyComp(46) or
      MLAttrKeyComp(58) or
      MLAttrKeyComp(132) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc72: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(11) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc73: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(51) or
      MLAttrKeyComp(121) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc74: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(18) or
      MLAttrKeyComp(33) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc75: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(115) or
      MLAttrKeyComp(117) or
      MLAttrKeyComp(136) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc77: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(39) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc78: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(128) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc79: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(70) or
      MLAttrKeyComp(131) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc80: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(29) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc81: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(110) or
      MLAttrKeyComp(129) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc82: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(85) or
      MLAttrKeyComp(146) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc83: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(69) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc85: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(88) or
      MLAttrKeyComp(127) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc87: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(14) or
      MLAttrKeyComp(83) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc88: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(47) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc91: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(3) or
      MLAttrKeyComp(32) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc93: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(26) or
      MLAttrKeyComp(91) or
      MLAttrKeyComp(103) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc94: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(105) or
      MLAttrKeyComp(112) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc96: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(10) or
      MLAttrKeyComp(102) or
      MLAttrKeyComp(106) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc97: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(30) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc98: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(111) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc101: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(15) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc102: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(107) or
      MLAttrKeyComp(140) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc104: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(73) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc105: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(45) or
      MLAttrKeyComp(54) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc106: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(116) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc107: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(94) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc108: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(77) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc109: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(120) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc110: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(130) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc111: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(81) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc113: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(104) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc114: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(37) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc117: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(122) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc119: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(36) or
      MLAttrKeyComp(71) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc122: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(144) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc126: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(72) or
      MLAttrKeyComp(92) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc127: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(139) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc132: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(145) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc139: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(100) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc143: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(79) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc147: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(93) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc154: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(89) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc155: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(13) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc157: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(97) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc158: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(96) or
      MLAttrKeyComp(98) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc160: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(2) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc162: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(99) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLAttrFunc176: TSynWebTokenKind;
begin
  if  MLAttrKeyComp(60) or
      MLAttrKeyComp(90) then
    Result := stkMLTagKey
  else
    Result := stkMLTagKeyUndef;
end;

function TSynWebEngine.MLSpecialKeyComp(const ID: Integer): Boolean;
var
  I: Integer;
  Temp: PChar;
  aKey: String;
begin
  if TSynWeb_SpecialData[ID] and (1 shl Longword(FInstance^.FOptions.FMLVersion)) = 0 then
  begin
    Result := False;
    Exit;
  end;
  aKey := TSynWeb_Special[ID];
  Temp := FInstance^.FToIdent;
  if Length(aKey) = FInstance^.FStringLen then
  begin
    for i := 1 to FInstance^.FStringLen do
    begin
      if Temp^ <> aKey[i] then
      begin
        Result := False;
        Exit;
      end;
      Inc(Temp);
    end;
    FInstance^.FTokenLastID := ID;
    Result := True;
  end else
    Result := False;
end;

function TSynWebEngine.MLSpecialCheck(AStart, ALen: Integer): Integer;
var
  HashKey: Longword;

  procedure KeyHash(ToHash: PChar);
  var
    i: Integer;
  begin
    HashKey := 0;
    FInstance^.FStringLen := ALen;
    for i := 0 to ALen - 1 do
    begin
      Inc(HashKey, TSynWebIdentHashTable[ToHash^]);
      Inc(ToHash);
    end;
  end;

begin              
  if FInstance^.FOptions.FMLVersion = smlwvXML then
  begin
    Result := 0;
    FInstance^.FTokenLastID := -1;
    Exit;
  end;
  FInstance^.FToIdent := @FInstance^.FLine[AStart];
  KeyHash(FInstance^.FToIdent);
  if (HashKey > MLSpecialMaxKeyHash) or not FMLSpecialIdentFuncTable[HashKey] then
    FInstance^.FTokenLastID := -1;
  Result := FInstance^.FTokenLastID;
end;

function TSynWebEngine.MLSpecialUndef: Boolean;
begin
  Result:=False;
end;

function TSynWebEngine.MLSpecialFunc12: Boolean;
begin
  if  MLSpecialKeyComp(79) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc16: Boolean;
begin
  if  MLSpecialKeyComp(46) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc17: Boolean;
begin
  if  MLSpecialKeyComp(111) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc19: Boolean;
begin
  if  MLSpecialKeyComp(13) or
      MLSpecialKeyComp(129) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc20: Boolean;
begin
  if  MLSpecialKeyComp(28) or
      MLSpecialKeyComp(33) or
      MLSpecialKeyComp(34) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc22: Boolean;
begin
  if  MLSpecialKeyComp(14) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc23: Boolean;
begin
  if  MLSpecialKeyComp(130) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc25: Boolean;
begin
  if  MLSpecialKeyComp(168) or
      MLSpecialKeyComp(169) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc26: Boolean;
begin
  if  MLSpecialKeyComp(63) or
      MLSpecialKeyComp(64) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc27: Boolean;
begin
  if  MLSpecialKeyComp(80) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc28: Boolean;
begin
  if  MLSpecialKeyComp(24) or
      MLSpecialKeyComp(25) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc30: Boolean;
begin
  if  MLSpecialKeyComp(12) or
      MLSpecialKeyComp(126) or
      MLSpecialKeyComp(188) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc32: Boolean;
begin
  if  MLSpecialKeyComp(118) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc33: Boolean;
begin
  if  MLSpecialKeyComp(31) or
      MLSpecialKeyComp(35) or
      MLSpecialKeyComp(65) or
      MLSpecialKeyComp(66) or
      MLSpecialKeyComp(103) or
      MLSpecialKeyComp(104) or
      MLSpecialKeyComp(152) or
      MLSpecialKeyComp(166) or
      MLSpecialKeyComp(167) or
      MLSpecialKeyComp(242) or
      MLSpecialKeyComp(243) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc34: Boolean;
begin
  if  MLSpecialKeyComp(2) or
      MLSpecialKeyComp(3) or
      MLSpecialKeyComp(5) or
      MLSpecialKeyComp(6) or
      MLSpecialKeyComp(105) or
      MLSpecialKeyComp(124) or
      MLSpecialKeyComp(125) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc35: Boolean;
begin
  if  MLSpecialKeyComp(77) or
      MLSpecialKeyComp(78) or
      MLSpecialKeyComp(92) or
      MLSpecialKeyComp(119) or
      MLSpecialKeyComp(136) or
      MLSpecialKeyComp(137) or
      MLSpecialKeyComp(180) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc36: Boolean;
begin
  if  MLSpecialKeyComp(29) or
      MLSpecialKeyComp(30) or
      MLSpecialKeyComp(162) or
      MLSpecialKeyComp(187) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc38: Boolean;
begin
  if  MLSpecialKeyComp(10) or
      MLSpecialKeyComp(11) or
      MLSpecialKeyComp(53) or
      MLSpecialKeyComp(54) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc39: Boolean;
begin
  if  MLSpecialKeyComp(37) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc40: Boolean;
begin
  if  MLSpecialKeyComp(40) or
      MLSpecialKeyComp(181) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc41: Boolean;
begin
  if  MLSpecialKeyComp(44) or
      MLSpecialKeyComp(45) or
      MLSpecialKeyComp(71) or
      MLSpecialKeyComp(109) or
      MLSpecialKeyComp(147) or
      MLSpecialKeyComp(148) or
      MLSpecialKeyComp(190) or
      MLSpecialKeyComp(191) or
      MLSpecialKeyComp(204) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc42: Boolean;
begin
  if  MLSpecialKeyComp(32) or
      MLSpecialKeyComp(42) or
      MLSpecialKeyComp(43) or
      MLSpecialKeyComp(47) or
      MLSpecialKeyComp(48) or
      MLSpecialKeyComp(87) or
      MLSpecialKeyComp(88) or
      MLSpecialKeyComp(206) or
      MLSpecialKeyComp(215) or
      MLSpecialKeyComp(216) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc43: Boolean;
begin
  if  MLSpecialKeyComp(94) or
      MLSpecialKeyComp(115) or
      MLSpecialKeyComp(153) or
      MLSpecialKeyComp(192) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc44: Boolean;
begin
  if  MLSpecialKeyComp(177) or
      MLSpecialKeyComp(178) or
      MLSpecialKeyComp(246) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc45: Boolean;
begin
  if  MLSpecialKeyComp(81) or
      MLSpecialKeyComp(82) or
      MLSpecialKeyComp(95) or
      MLSpecialKeyComp(96) or
      MLSpecialKeyComp(101) or
      MLSpecialKeyComp(102) or
      MLSpecialKeyComp(120) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc46: Boolean;
begin
  if  MLSpecialKeyComp(49) or
      MLSpecialKeyComp(128) or
      MLSpecialKeyComp(235) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc47: Boolean;
begin
  if  MLSpecialKeyComp(21) or
      MLSpecialKeyComp(22) or
      MLSpecialKeyComp(27) or
      MLSpecialKeyComp(170) or
      MLSpecialKeyComp(185) or
      MLSpecialKeyComp(199) or
      MLSpecialKeyComp(207) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc48: Boolean;
begin
  if  MLSpecialKeyComp(140) or
      MLSpecialKeyComp(141) or
      MLSpecialKeyComp(142) or
      MLSpecialKeyComp(143) or
      MLSpecialKeyComp(226) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc49: Boolean;
begin
  if  MLSpecialKeyComp(16) or
      MLSpecialKeyComp(17) or
      MLSpecialKeyComp(107) or
      MLSpecialKeyComp(108) or
      MLSpecialKeyComp(131) or
      MLSpecialKeyComp(201) or
      MLSpecialKeyComp(202) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc50: Boolean;
begin
  if  MLSpecialKeyComp(4) or
      MLSpecialKeyComp(154) or
      MLSpecialKeyComp(224) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc51: Boolean;
begin
  if  MLSpecialKeyComp(0) or
      MLSpecialKeyComp(1) or
      MLSpecialKeyComp(15) or
      MLSpecialKeyComp(19) or
      MLSpecialKeyComp(20) or
      MLSpecialKeyComp(67) or
      MLSpecialKeyComp(68) or
      MLSpecialKeyComp(98) or
      MLSpecialKeyComp(127) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc52: Boolean;
begin
  if  MLSpecialKeyComp(93) or
      MLSpecialKeyComp(200) or
      MLSpecialKeyComp(249) or
      MLSpecialKeyComp(250) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc53: Boolean;
begin
  if  MLSpecialKeyComp(50) or
      MLSpecialKeyComp(58) or
      MLSpecialKeyComp(89) or
      MLSpecialKeyComp(114) or
      MLSpecialKeyComp(175) or
      MLSpecialKeyComp(208) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc54: Boolean;
begin
  if  MLSpecialKeyComp(7) or
      MLSpecialKeyComp(8) or
      MLSpecialKeyComp(59) or
      MLSpecialKeyComp(218) or
      MLSpecialKeyComp(219) or
      MLSpecialKeyComp(231) or
      MLSpecialKeyComp(232) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc55: Boolean;
begin
  if  MLSpecialKeyComp(51) or
      MLSpecialKeyComp(52) or
      MLSpecialKeyComp(99) or
      MLSpecialKeyComp(100) or
      MLSpecialKeyComp(146) or
      MLSpecialKeyComp(163) or
      MLSpecialKeyComp(165) or
      MLSpecialKeyComp(183) or
      MLSpecialKeyComp(184) or
      MLSpecialKeyComp(203) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc56: Boolean;
begin
  if  MLSpecialKeyComp(76) or
      MLSpecialKeyComp(133) or
      MLSpecialKeyComp(209) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc57: Boolean;
begin
  if  MLSpecialKeyComp(36) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc58: Boolean;
begin
  if  MLSpecialKeyComp(39) or
      MLSpecialKeyComp(55) or
      MLSpecialKeyComp(56) or
      MLSpecialKeyComp(121) or
      MLSpecialKeyComp(198) or
      MLSpecialKeyComp(229) or
      MLSpecialKeyComp(230) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc59: Boolean;
begin
  if  MLSpecialKeyComp(23) or
      MLSpecialKeyComp(38) or
      MLSpecialKeyComp(69) or
      MLSpecialKeyComp(85) or
      MLSpecialKeyComp(86) or
      MLSpecialKeyComp(251) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc61: Boolean;
begin
  if  MLSpecialKeyComp(160) or
      MLSpecialKeyComp(161) or
      MLSpecialKeyComp(173) or
      MLSpecialKeyComp(174) or
      MLSpecialKeyComp(213) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc62: Boolean;
begin
  if  MLSpecialKeyComp(84) or
      MLSpecialKeyComp(90) or
      MLSpecialKeyComp(91) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc63: Boolean;
begin
  if  MLSpecialKeyComp(26) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc64: Boolean;
begin
  if  MLSpecialKeyComp(72) or
      MLSpecialKeyComp(134) or
      MLSpecialKeyComp(135) or
      MLSpecialKeyComp(205) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc65: Boolean;
begin
  if  MLSpecialKeyComp(122) or
      MLSpecialKeyComp(138) or
      MLSpecialKeyComp(139) or
      MLSpecialKeyComp(157) or
      MLSpecialKeyComp(158) or
      MLSpecialKeyComp(176) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc66: Boolean;
begin
  if  MLSpecialKeyComp(106) or
      MLSpecialKeyComp(225) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc67: Boolean;
begin
  if  MLSpecialKeyComp(239) or
      MLSpecialKeyComp(240) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc68: Boolean;
begin
  if  MLSpecialKeyComp(144) or
      MLSpecialKeyComp(145) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc69: Boolean;
begin
  if  MLSpecialKeyComp(110) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc70: Boolean;
begin
  if  MLSpecialKeyComp(172) or
      MLSpecialKeyComp(196) or
      MLSpecialKeyComp(197) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc71: Boolean;
begin
  if  MLSpecialKeyComp(83) or
      MLSpecialKeyComp(227) or
      MLSpecialKeyComp(228) or
      MLSpecialKeyComp(247) or
      MLSpecialKeyComp(248) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc72: Boolean;
begin
  if  MLSpecialKeyComp(132) or
      MLSpecialKeyComp(182) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc73: Boolean;
begin
  if  MLSpecialKeyComp(164) or
      MLSpecialKeyComp(179) or
      MLSpecialKeyComp(214) or
      MLSpecialKeyComp(236) or
      MLSpecialKeyComp(252) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc74: Boolean;
begin
  if  MLSpecialKeyComp(18) or
      MLSpecialKeyComp(62) or
      MLSpecialKeyComp(155) or
      MLSpecialKeyComp(156) or
      MLSpecialKeyComp(195) or
      MLSpecialKeyComp(233) or
      MLSpecialKeyComp(234) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc75: Boolean;
begin
  if  MLSpecialKeyComp(186) or
      MLSpecialKeyComp(222) or
      MLSpecialKeyComp(223) or
      MLSpecialKeyComp(244) or
      MLSpecialKeyComp(245) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc76: Boolean;
begin
  if  MLSpecialKeyComp(123) or
      MLSpecialKeyComp(241) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc77: Boolean;
begin
  if  MLSpecialKeyComp(70) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc78: Boolean;
begin
  if  MLSpecialKeyComp(112) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc79: Boolean;
begin
  if  MLSpecialKeyComp(41) or
      MLSpecialKeyComp(57) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc81: Boolean;
begin
  if  MLSpecialKeyComp(9) or
      MLSpecialKeyComp(159) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc83: Boolean;
begin
  if  MLSpecialKeyComp(151) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc84: Boolean;
begin
  if  MLSpecialKeyComp(117) or
      MLSpecialKeyComp(189) or
      MLSpecialKeyComp(210) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc85: Boolean;
begin
  if  MLSpecialKeyComp(73) or
      MLSpecialKeyComp(116) or
      MLSpecialKeyComp(211) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc86: Boolean;
begin
  if  MLSpecialKeyComp(212) or
      MLSpecialKeyComp(221) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc87: Boolean;
begin
  if  MLSpecialKeyComp(74) or
      MLSpecialKeyComp(149) or
      MLSpecialKeyComp(150) or
      MLSpecialKeyComp(217) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc89: Boolean;
begin
  if  MLSpecialKeyComp(75) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc90: Boolean;
begin
  if  MLSpecialKeyComp(60) or
      MLSpecialKeyComp(61) or
      MLSpecialKeyComp(113) or
      MLSpecialKeyComp(194) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc91: Boolean;
begin
  if  MLSpecialKeyComp(97) or
      MLSpecialKeyComp(193) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc95: Boolean;
begin
  if  MLSpecialKeyComp(171) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc106: Boolean;
begin
  if  MLSpecialKeyComp(237) or
      MLSpecialKeyComp(238) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.MLSpecialFunc111: Boolean;
begin
  if  MLSpecialKeyComp(220) then
    Result := True
  else
    Result := False;
end;

// Css -------------------------------------------------------------------------

procedure TSynWebEngine.CssMakeMethodTables;
var
  c: char;
  i: Integer;
  pF: PSynWebIdentFuncTableFunc;
  pF2: PSynWebIdent2FuncTableFunc;
begin
  for c := #0 to #255 do
    case c of
    #0:
      FCssProcTable[c] := NullProc;
    #1..#32:
      FCssProcTable[c] := CssSpaceProc;
    '@':
      FCssProcTable[c] := CssAtKeywordProc;
    '/':
      FCssProcTable[c] := CssSlashProc;
    '<':
      FCssProcTable[c] := CssBraceOpenProc;
    '{':
      FCssProcTable[c] := CssCurlyBraceOpenProc;
    '}':
      FCssProcTable[c] := CssCurlyBraceCloseProc;
    '*', '>':
      FCssProcTable[c] := CssChildAnySelectorProc;
    '[':
      FCssProcTable[c] := CssAttribProc;
    '#':
      FCssProcTable[c] := CssHashProc;
    '.':
      FCssProcTable[c] := CssDotProc;
    ',':
      FCssProcTable[c] := CssCommaProc;
    ':':
      FCssProcTable[c] := CssColonProc;
    ';':
      FCssProcTable[c] := CssSemiColonProc;
    '!':
      FCssProcTable[c] := CssExclamationProc;
    #39, '"':
      FCssProcTable[c] := CssStringProc;
    '+':
      FCssProcTable[c] := CssPlusProc;
    '-':
      FCssProcTable[c] := CssMinusProc;
    '0'..'9':
      FCssProcTable[c] := CssNumberProc;
    'a'..'z', 'A'..'Z', '\':
      FCssProcTable[c] := CssIdentProc;
    else // case
      FCssProcTable[c] := CssErrorProc;
    end;

  FCssRangeProcTable[srsCssRuleset] := CssRangeRulesetProc;
  FCssRangeProcTable[srsCssSelectorAttrib] := CssRangeSelectorAttribProc;
  FCssRangeProcTable[srsCssSelectorPseudo] := CssRangeSelectorPseudoProc;
  FCssRangeProcTable[srsCssAtKeyword] := CssRangeAtKeywordProc;
  FCssRangeProcTable[srsCssComment] := CssRangeCommentProc;
  FCssRangeProcTable[srsCssProp] := CssRangePropProc;
  FCssRangeProcTable[srsCssPropVal] := CssRangePropValProc;
  FCssRangeProcTable[srsCssPropValStr] := CssRangePropValStrProc;
  FCssRangeProcTable[srsCssPropValRgb] := CssRangePropValRgbProc;
  FCssRangeProcTable[srsCssPropValSpecial] := CssRangePropValSpecialProc;
  FCssRangeProcTable[srsCssPropValImportant] := CssRangePropValImportantProc;
  FCssRangeProcTable[srsCssPropValUrl] := CssRangePropValUrlProc;
  FCssRangeProcTable[srsCssPropValRect] := CssRangePropValRectProc;
  FCssRangeProcTable[srsCssPropValFunc] := CssRangePropValFuncProc;

  pF := PSynWebIdentFuncTableFunc(@FCssPropIdentFuncTable);
  for I := Low(FCssPropIdentFuncTable) to High(FCssPropIdentFuncTable) do
  begin
    pF^ := CssPropUndef;
    Inc(pF);
  end;
  FCssPropIdentFuncTable[29]:=CssPropFunc29;
  FCssPropIdentFuncTable[39]:=CssPropFunc39;
  FCssPropIdentFuncTable[40]:=CssPropFunc40;
  FCssPropIdentFuncTable[43]:=CssPropFunc43;
  FCssPropIdentFuncTable[51]:=CssPropFunc51;
  FCssPropIdentFuncTable[52]:=CssPropFunc52;
  FCssPropIdentFuncTable[54]:=CssPropFunc54;
  FCssPropIdentFuncTable[55]:=CssPropFunc55;
  FCssPropIdentFuncTable[56]:=CssPropFunc56;
  FCssPropIdentFuncTable[57]:=CssPropFunc57;
  FCssPropIdentFuncTable[62]:=CssPropFunc62;
  FCssPropIdentFuncTable[63]:=CssPropFunc63;
  FCssPropIdentFuncTable[64]:=CssPropFunc64;
  FCssPropIdentFuncTable[85]:=CssPropFunc85;
  FCssPropIdentFuncTable[86]:=CssPropFunc86;
  FCssPropIdentFuncTable[88]:=CssPropFunc88;
  FCssPropIdentFuncTable[91]:=CssPropFunc91;
  FCssPropIdentFuncTable[93]:=CssPropFunc93;
  FCssPropIdentFuncTable[94]:=CssPropFunc94;
  FCssPropIdentFuncTable[95]:=CssPropFunc95;
  FCssPropIdentFuncTable[96]:=CssPropFunc96;
  FCssPropIdentFuncTable[97]:=CssPropFunc97;
  FCssPropIdentFuncTable[98]:=CssPropFunc98;
  FCssPropIdentFuncTable[100]:=CssPropFunc100;
  FCssPropIdentFuncTable[103]:=CssPropFunc103;
  FCssPropIdentFuncTable[116]:=CssPropFunc116;
  FCssPropIdentFuncTable[117]:=CssPropFunc117;
  FCssPropIdentFuncTable[118]:=CssPropFunc118;
  FCssPropIdentFuncTable[120]:=CssPropFunc120;
  FCssPropIdentFuncTable[131]:=CssPropFunc131;
  FCssPropIdentFuncTable[133]:=CssPropFunc133;
  FCssPropIdentFuncTable[135]:=CssPropFunc135;
  FCssPropIdentFuncTable[136]:=CssPropFunc136;
  FCssPropIdentFuncTable[138]:=CssPropFunc138;
  FCssPropIdentFuncTable[139]:=CssPropFunc139;
  FCssPropIdentFuncTable[140]:=CssPropFunc140;
  FCssPropIdentFuncTable[143]:=CssPropFunc143;
  FCssPropIdentFuncTable[144]:=CssPropFunc144;
  FCssPropIdentFuncTable[147]:=CssPropFunc147;
  FCssPropIdentFuncTable[150]:=CssPropFunc150;
  FCssPropIdentFuncTable[151]:=CssPropFunc151;
  FCssPropIdentFuncTable[152]:=CssPropFunc152;
  FCssPropIdentFuncTable[153]:=CssPropFunc153;
  FCssPropIdentFuncTable[155]:=CssPropFunc155;
  FCssPropIdentFuncTable[158]:=CssPropFunc158;
  FCssPropIdentFuncTable[159]:=CssPropFunc159;
  FCssPropIdentFuncTable[162]:=CssPropFunc162;
  FCssPropIdentFuncTable[163]:=CssPropFunc163;
  FCssPropIdentFuncTable[164]:=CssPropFunc164;
  FCssPropIdentFuncTable[165]:=CssPropFunc165;
  FCssPropIdentFuncTable[167]:=CssPropFunc167;
  FCssPropIdentFuncTable[168]:=CssPropFunc168;
  FCssPropIdentFuncTable[169]:=CssPropFunc169;
  FCssPropIdentFuncTable[171]:=CssPropFunc171;
  FCssPropIdentFuncTable[172]:=CssPropFunc172;
  FCssPropIdentFuncTable[173]:=CssPropFunc173;
  FCssPropIdentFuncTable[174]:=CssPropFunc174;
  FCssPropIdentFuncTable[178]:=CssPropFunc178;
  FCssPropIdentFuncTable[179]:=CssPropFunc179;
  FCssPropIdentFuncTable[181]:=CssPropFunc181;
  FCssPropIdentFuncTable[183]:=CssPropFunc183;
  FCssPropIdentFuncTable[185]:=CssPropFunc185;
  FCssPropIdentFuncTable[187]:=CssPropFunc187;
  FCssPropIdentFuncTable[192]:=CssPropFunc192;
  FCssPropIdentFuncTable[193]:=CssPropFunc193;
  FCssPropIdentFuncTable[197]:=CssPropFunc197;
  FCssPropIdentFuncTable[198]:=CssPropFunc198;
  FCssPropIdentFuncTable[199]:=CssPropFunc199;
  FCssPropIdentFuncTable[201]:=CssPropFunc201;
  FCssPropIdentFuncTable[202]:=CssPropFunc202;
  FCssPropIdentFuncTable[211]:=CssPropFunc211;
  FCssPropIdentFuncTable[215]:=CssPropFunc215;
  FCssPropIdentFuncTable[231]:=CssPropFunc231;
  FCssPropIdentFuncTable[235]:=CssPropFunc235;
  FCssPropIdentFuncTable[239]:=CssPropFunc239;
  FCssPropIdentFuncTable[244]:=CssPropFunc244;
  FCssPropIdentFuncTable[245]:=CssPropFunc245;
  FCssPropIdentFuncTable[251]:=CssPropFunc251;
  FCssPropIdentFuncTable[252]:=CssPropFunc252;
  FCssPropIdentFuncTable[253]:=CssPropFunc253;
  FCssPropIdentFuncTable[262]:=CssPropFunc262;
  FCssPropIdentFuncTable[263]:=CssPropFunc263;
  FCssPropIdentFuncTable[264]:=CssPropFunc264;
  FCssPropIdentFuncTable[270]:=CssPropFunc270;
  FCssPropIdentFuncTable[281]:=CssPropFunc281;
  FCssPropIdentFuncTable[283]:=CssPropFunc283;
  FCssPropIdentFuncTable[286]:=CssPropFunc286;
  FCssPropIdentFuncTable[287]:=CssPropFunc287;
  FCssPropIdentFuncTable[304]:=CssPropFunc304;
  FCssPropIdentFuncTable[334]:=CssPropFunc334;

  pF := PSynWebIdentFuncTableFunc(@FCssValIdentFuncTable);
  for I := Low(FCssValIdentFuncTable) to High(FCssValIdentFuncTable) do
  begin
    pF^ := CssValUndef;
    Inc(pF);
  end;
  FCssValIdentFuncTable[26]:=CssValFunc26;
  FCssValIdentFuncTable[27]:=CssValFunc27;
  FCssValIdentFuncTable[29]:=CssValFunc29;
  FCssValIdentFuncTable[31]:=CssValFunc31;
  FCssValIdentFuncTable[32]:=CssValFunc32;
  FCssValIdentFuncTable[33]:=CssValFunc33;
  FCssValIdentFuncTable[35]:=CssValFunc35;
  FCssValIdentFuncTable[36]:=CssValFunc36;
  FCssValIdentFuncTable[37]:=CssValFunc37;
  FCssValIdentFuncTable[38]:=CssValFunc38;
  FCssValIdentFuncTable[39]:=CssValFunc39;
  FCssValIdentFuncTable[40]:=CssValFunc40;
  FCssValIdentFuncTable[41]:=CssValFunc41;
  FCssValIdentFuncTable[42]:=CssValFunc42;
  FCssValIdentFuncTable[43]:=CssValFunc43;
  FCssValIdentFuncTable[44]:=CssValFunc44;
  FCssValIdentFuncTable[45]:=CssValFunc45;
  FCssValIdentFuncTable[46]:=CssValFunc46;
  FCssValIdentFuncTable[47]:=CssValFunc47;
  FCssValIdentFuncTable[48]:=CssValFunc48;
  FCssValIdentFuncTable[49]:=CssValFunc49;
  FCssValIdentFuncTable[50]:=CssValFunc50;
  FCssValIdentFuncTable[51]:=CssValFunc51;
  FCssValIdentFuncTable[52]:=CssValFunc52;
  FCssValIdentFuncTable[53]:=CssValFunc53;
  FCssValIdentFuncTable[54]:=CssValFunc54;
  FCssValIdentFuncTable[55]:=CssValFunc55;
  FCssValIdentFuncTable[56]:=CssValFunc56;
  FCssValIdentFuncTable[57]:=CssValFunc57;
  FCssValIdentFuncTable[59]:=CssValFunc59;
  FCssValIdentFuncTable[60]:=CssValFunc60;
  FCssValIdentFuncTable[61]:=CssValFunc61;
  FCssValIdentFuncTable[62]:=CssValFunc62;
  FCssValIdentFuncTable[63]:=CssValFunc63;
  FCssValIdentFuncTable[65]:=CssValFunc65;
  FCssValIdentFuncTable[67]:=CssValFunc67;
  FCssValIdentFuncTable[68]:=CssValFunc68;
  FCssValIdentFuncTable[69]:=CssValFunc69;
  FCssValIdentFuncTable[72]:=CssValFunc72;
  FCssValIdentFuncTable[73]:=CssValFunc73;
  FCssValIdentFuncTable[75]:=CssValFunc75;
  FCssValIdentFuncTable[76]:=CssValFunc76;
  FCssValIdentFuncTable[78]:=CssValFunc78;
  FCssValIdentFuncTable[79]:=CssValFunc79;
  FCssValIdentFuncTable[80]:=CssValFunc80;
  FCssValIdentFuncTable[81]:=CssValFunc81;
  FCssValIdentFuncTable[82]:=CssValFunc82;
  FCssValIdentFuncTable[83]:=CssValFunc83;
  FCssValIdentFuncTable[85]:=CssValFunc85;
  FCssValIdentFuncTable[86]:=CssValFunc86;
  FCssValIdentFuncTable[87]:=CssValFunc87;
  FCssValIdentFuncTable[88]:=CssValFunc88;
  FCssValIdentFuncTable[92]:=CssValFunc92;
  FCssValIdentFuncTable[93]:=CssValFunc93;
  FCssValIdentFuncTable[94]:=CssValFunc94;
  FCssValIdentFuncTable[95]:=CssValFunc95;
  FCssValIdentFuncTable[96]:=CssValFunc96;
  FCssValIdentFuncTable[97]:=CssValFunc97;
  FCssValIdentFuncTable[100]:=CssValFunc100;
  FCssValIdentFuncTable[101]:=CssValFunc101;
  FCssValIdentFuncTable[102]:=CssValFunc102;
  FCssValIdentFuncTable[104]:=CssValFunc104;
  FCssValIdentFuncTable[105]:=CssValFunc105;
  FCssValIdentFuncTable[106]:=CssValFunc106;
  FCssValIdentFuncTable[108]:=CssValFunc108;
  FCssValIdentFuncTable[110]:=CssValFunc110;
  FCssValIdentFuncTable[112]:=CssValFunc112;
  FCssValIdentFuncTable[114]:=CssValFunc114;
  FCssValIdentFuncTable[115]:=CssValFunc115;
  FCssValIdentFuncTable[117]:=CssValFunc117;
  FCssValIdentFuncTable[118]:=CssValFunc118;
  FCssValIdentFuncTable[119]:=CssValFunc119;
  FCssValIdentFuncTable[122]:=CssValFunc122;
  FCssValIdentFuncTable[125]:=CssValFunc125;
  FCssValIdentFuncTable[127]:=CssValFunc127;
  FCssValIdentFuncTable[128]:=CssValFunc128;
  FCssValIdentFuncTable[129]:=CssValFunc129;
  FCssValIdentFuncTable[131]:=CssValFunc131;
  FCssValIdentFuncTable[132]:=CssValFunc132;
  FCssValIdentFuncTable[134]:=CssValFunc134;
  FCssValIdentFuncTable[135]:=CssValFunc135;
  FCssValIdentFuncTable[137]:=CssValFunc137;
  FCssValIdentFuncTable[139]:=CssValFunc139;
  FCssValIdentFuncTable[141]:=CssValFunc141;
  FCssValIdentFuncTable[143]:=CssValFunc143;
  FCssValIdentFuncTable[144]:=CssValFunc144;
  FCssValIdentFuncTable[145]:=CssValFunc145;
  FCssValIdentFuncTable[146]:=CssValFunc146;
  FCssValIdentFuncTable[148]:=CssValFunc148;
  FCssValIdentFuncTable[149]:=CssValFunc149;
  FCssValIdentFuncTable[151]:=CssValFunc151;
  FCssValIdentFuncTable[152]:=CssValFunc152;
  FCssValIdentFuncTable[156]:=CssValFunc156;
  FCssValIdentFuncTable[157]:=CssValFunc157;
  FCssValIdentFuncTable[158]:=CssValFunc158;
  FCssValIdentFuncTable[159]:=CssValFunc159;
  FCssValIdentFuncTable[162]:=CssValFunc162;
  FCssValIdentFuncTable[165]:=CssValFunc165;
  FCssValIdentFuncTable[166]:=CssValFunc166;
  FCssValIdentFuncTable[167]:=CssValFunc167;
  FCssValIdentFuncTable[170]:=CssValFunc170;
  FCssValIdentFuncTable[172]:=CssValFunc172;
  FCssValIdentFuncTable[173]:=CssValFunc173;
  FCssValIdentFuncTable[175]:=CssValFunc175;
  FCssValIdentFuncTable[192]:=CssValFunc192;
  FCssValIdentFuncTable[233]:=CssValFunc233;
  FCssValIdentFuncTable[234]:=CssValFunc234;
  FCssValIdentFuncTable[237]:=CssValFunc237;
  FCssValIdentFuncTable[239]:=CssValFunc239;
  FCssValIdentFuncTable[249]:=CssValFunc249;
  FCssValIdentFuncTable[271]:=CssValFunc271;
  FCssValIdentFuncTable[272]:=CssValFunc272;

  pF2 := PSynWebIdent2FuncTableFunc(@FCssSpecialIdentFuncTable);
  for I := Low(FCssSpecialIdentFuncTable) to High(FCssSpecialIdentFuncTable) do
  begin
    pF2^ := CssSpecialUndef;
    Inc(pF2);
  end;
  FCssSpecialIdentFuncTable[16]:=CssSpecialFunc16;
  FCssSpecialIdentFuncTable[18]:=CssSpecialFunc18;
  FCssSpecialIdentFuncTable[19]:=CssSpecialFunc19;
  FCssSpecialIdentFuncTable[23]:=CssSpecialFunc23;
  FCssSpecialIdentFuncTable[25]:=CssSpecialFunc25;
  FCssSpecialIdentFuncTable[26]:=CssSpecialFunc26;
  FCssSpecialIdentFuncTable[29]:=CssSpecialFunc29;
  FCssSpecialIdentFuncTable[30]:=CssSpecialFunc30;
  FCssSpecialIdentFuncTable[32]:=CssSpecialFunc32;
  FCssSpecialIdentFuncTable[34]:=CssSpecialFunc34;
  FCssSpecialIdentFuncTable[36]:=CssSpecialFunc36;
  FCssSpecialIdentFuncTable[40]:=CssSpecialFunc40;
  FCssSpecialIdentFuncTable[42]:=CssSpecialFunc42;
  FCssSpecialIdentFuncTable[43]:=CssSpecialFunc43;
  FCssSpecialIdentFuncTable[45]:=CssSpecialFunc45;
  FCssSpecialIdentFuncTable[46]:=CssSpecialFunc46;
  FCssSpecialIdentFuncTable[50]:=CssSpecialFunc50;
  FCssSpecialIdentFuncTable[51]:=CssSpecialFunc51;
  FCssSpecialIdentFuncTable[56]:=CssSpecialFunc56;
  FCssSpecialIdentFuncTable[57]:=CssSpecialFunc57;
  FCssSpecialIdentFuncTable[59]:=CssSpecialFunc59;
  FCssSpecialIdentFuncTable[60]:=CssSpecialFunc60;
  FCssSpecialIdentFuncTable[62]:=CssSpecialFunc62;
  FCssSpecialIdentFuncTable[64]:=CssSpecialFunc64;
  FCssSpecialIdentFuncTable[65]:=CssSpecialFunc65;
  FCssSpecialIdentFuncTable[68]:=CssSpecialFunc68;
  FCssSpecialIdentFuncTable[72]:=CssSpecialFunc72;
  FCssSpecialIdentFuncTable[74]:=CssSpecialFunc74;
  FCssSpecialIdentFuncTable[77]:=CssSpecialFunc77;
  FCssSpecialIdentFuncTable[82]:=CssSpecialFunc82;
  FCssSpecialIdentFuncTable[88]:=CssSpecialFunc88;
  FCssSpecialIdentFuncTable[91]:=CssSpecialFunc91;
  FCssSpecialIdentFuncTable[108]:=CssSpecialFunc108;
  FCssSpecialIdentFuncTable[125]:=CssSpecialFunc125;
  FCssSpecialIdentFuncTable[126]:=CssSpecialFunc126;
  FCssSpecialIdentFuncTable[146]:=CssSpecialFunc146;
  FCssSpecialIdentFuncTable[150]:=CssSpecialFunc150;
  FCssSpecialIdentFuncTable[190]:=CssSpecialFunc190;
end;

procedure TSynWebEngine.CssNextBg;
begin
  CssUpdateBg;
  FInstance^.FNextProcTable := CssNext;
  CssNext;
end;

procedure TSynWebEngine.CssNext;
begin
  FInstance^.FTokenPos := FInstance^.FRun;
  FCssRangeProcTable[CssGetRange];
end;

procedure TSynWebEngine.CssUpdateBg;
begin
  if TSynWebCssRangeState(GetRangeInt(4, 13)) in
    [TSynWebCssRangeStateRulesetBegin..TSynWebCssRangeStateRulesetEnd] then
    FInstance^.FSYN_ATTR_WHITESPACE := FCssRulesetWhitespaceAttri
  else
    FInstance^.FSYN_ATTR_WHITESPACE := FCssWhitespaceAttri;
  FTokenAttributeTable[stkCssSpace] := FInstance^.FSYN_ATTR_WHITESPACE;
end;

function TSynWebEngine.CssGetRange: TSynWebCssRangeState;
begin
  if GetRangeBit(12) then
    Result := srsCssComment
  else
    Result := TSynWebCssRangeState(GetRangeInt(4, 13));
end;

procedure TSynWebEngine.CssSetRange(const ARange: TSynWebCssRangeState);
begin
  if ARange = srsCssComment then
    SetRangeBit(12, True)
  else
  begin
    if not (ARange in [TSynWebCssRangeStateRulesetBegin..
      TSynWebCssRangeStateRulesetEnd]) and
      (TSynWebCssRangeState(GetRangeInt(4, 13)) in
      [TSynWebCssRangeStateRulesetBegin..TSynWebCssRangeStateRulesetEnd]) then
    begin
      SetRangeInt(4, 13, Longword(ARange));
      FInstance^.FNextProcTable := CssNextBg;
    end else
    begin
      SetRangeInt(4, 13, Longword(ARange));
      CssUpdateBg;
    end;
    if ARange = srsCssRuleset then
      SetRangeInt(11, 0, 0);
  end;
end;

function TSynWebEngine.CssGetProp: Integer;
begin
  Result := GetRangeInt(8, 0);
end;

procedure TSynWebEngine.CssSetProp(const AProp: Integer);
begin
  SetRangeInt(8, 0, Longword(AProp));
end;

function TSynWebEngine.CssCheckNull(ADo: Boolean = True): Boolean;
begin
  case FInstance^.FLine[FInstance^.FRun] of
  #0:
    begin
      Result := True;
      if ADo then
        NullProc;
    end;
  '<':
    if (FInstance^.FLine[FInstance^.FRun + 1] = '/') and
      (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 2]] =
      FInstance^.FHashTable['s']) and
      (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 3]] =
      FInstance^.FHashTable['t']) and
      (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 4]] =
      FInstance^.FHashTable['y']) and
      (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 5]] =
      FInstance^.FHashTable['l']) and
      (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 6]] =
      FInstance^.FHashTable['e']) and
      (TSynWebIdentTable2[FInstance^.FLine[FInstance^.FRun + 7]] and (1 shl 0) <> 0) and
      // (FInstance^.FLine[FInstance^.FRun+7] in [#0..#32, '>']) and
      (FInstance^.FHighlighterMode = shmML) then
    begin
      Result := True;
      if ADo then
      begin
        SetHighlighterType(shtML, True, False, False);
        Next;
      end;
    end else
      Result := False;
  else // case
    Result := False;
  end;
end;

procedure TSynWebEngine.CssSpaceProc;
begin
  repeat
    Inc(FInstance^.FRun);
  until not (FInstance^.FLine[FInstance^.FRun] in [#1..#32]);
  FInstance^.FTokenID := stkCssSpace;
end;

procedure TSynWebEngine.CssAtKeywordProc;
begin
  // if not (FInstance^.FLine[FInstance^.FRun+1] in ['a'..'z', 'A'..'Z']) then
  if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 1]] and (1 shl 0) = 0 then
    CssErrorProc
  else
  begin
    CssSymbolProc;
    CssSetRange(srsCssAtKeyword);
  end;
end;

procedure TSynWebEngine.CssSlashProc;
begin
  if FInstance^.FLine[FInstance^.FRun + 1] = '*' then
  begin
    Inc(FInstance^.FRun, 2);
    SetRangeBit(12, True); // CssSetRange(srsCssComment);
    if CssCheckNull(False) or PhpCheckBegin(False) then
      FInstance^.FTokenID := stkCssComment
    else
      CssRangeCommentProc;
  end else
    if (CssGetRange = srsCssPropVal) and GetRangeBit(8) then
    begin
      SetRangeBit(8, False);
      CssSymbolProc;
    end else
      CssErrorProc;
end;

procedure TSynWebEngine.CssBraceOpenProc;
begin
  if CssCheckNull or PhpCheckBegin then
    Exit;
  if (FInstance^.FLine[FInstance^.FRun + 1] = '!') and
    (FInstance^.FLine[FInstance^.FRun + 2] = '-') and
    (FInstance^.FLine[FInstance^.FRun + 3] = '-') then
  begin
    Inc(FInstance^.FRun, 4);
    FInstance^.FTokenID := stkMLComment;
  end else
    CssErrorProc;
end;

procedure TSynWebEngine.CssCurlyBraceOpenProc;
begin
  CssSymbolProc;
  CssSetRange(srsCssProp);
end;

procedure TSynWebEngine.CssCurlyBraceCloseProc;
begin
  if CssGetRange = srsCssPropVal then
  begin
    CssSymbolProc;
    CssSetRange(srsCssRuleset);
  end else
    if GetRangeBit(11) then
    begin
      SetRangeBit(11, False);
      CssSymbolProc;
    end else
      CssErrorProc;
end;

procedure TSynWebEngine.CssChildAnySelectorProc;
begin
  if FInstance^.FOptions.FCssVersion = scvCss21 then
    CssSymbolProc
  else
    CssErrorProc;
end;

procedure TSynWebEngine.CssAttribProc;
begin
  if FInstance^.FOptions.FCssVersion = scvCss1 then
    CssErrorProc
  else
  begin
    CssSymbolProc;
    CssSetRange(srsCssSelectorAttrib);
  end;
end;

procedure TSynWebEngine.CssHashProc;
begin
  if CssGetRange = srsCssPropVal then
  begin
    // if FInstance^.FLine[FInstance^.FRun+1] in ['a'..'f', 'A'..'F', '0'..'9'] and
    if (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 1]] and (1 shl 10) <> 0) and
      // FInstance^.FLine[FInstance^.FRun+2] in ['a'..'f', 'A'..'F', '0'..'9'] and
      (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 2]] and (1 shl 10) <> 0) and
      // FInstance^.FLine[FInstance^.FRun+3] in ['a'..'f', 'A'..'F', '0'..'9'] then
      (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 3]] and (1 shl 10) <> 0) then
    begin
      CssSymbolProc;
      CssSetRange(srsCssPropValSpecial);
    end else
      CssErrorProc;
  end else
    // if not (FInstance^.FLine[FInstance^.FRun+1] in ['a'..'z', 'A'..'Z', '\']) or
    if (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 1]] and (1 shl 8) = 0) or
      ((FInstance^.FLine[FInstance^.FRun + 1] = '\') and
      (FInstance^.FLine[FInstance^.FRun + 2] in [#0..#31])) then
      CssErrorProc
    else
    begin
      CssSymbolProc;
      SetRangeBit(8, True);
    end;
end;

procedure TSynWebEngine.CssDotProc;
begin
  if CssGetRange = srsCssPropVal then
  begin
    if FInstance^.FLine[FInstance^.FRun + 1] in ['0'..'9'] then
    begin
      FInstance^.FCssMask := $F5000000;
      CssNumberDefProc;
    end else
      CssErrorProc;
  end else
  begin
    // if not (FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '\']) or
    if (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 1]] and (1 shl 8) = 0) or
      ((FInstance^.FLine[FInstance^.FRun + 1] = '\') and
      (FInstance^.FLine[FInstance^.FRun + 2] in [#0..#31])) then
    begin
      CssErrorProc;
      Exit;
    end;
    CssSymbolProc;
    SetRangeBit(9, True);
  end;
end;

procedure TSynWebEngine.CssCommaProc;
var
  prop: Integer;
begin
  if CssGetRange = srsCssPropVal then
  begin
    prop := CssGetProp - 1;
    if (prop = -1) or (TSynWeb_CssPropsData[prop] and (1 shl 16) = 0) then
    begin
      CssErrorProc;
      Exit;
    end;
  end;
  CssSymbolProc;
end;

procedure TSynWebEngine.CssColonProc;
begin
  // if not (FInstance^.FLine[FInstance^.FRun+1] in ['a'..'z', 'A'..'Z']) then
  if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 1]] and (1 shl 0) = 0 then
    CssErrorProc
  else
  begin
    CssSymbolProc;
    CssSetRange(srsCssSelectorPseudo);
  end;
end;

procedure TSynWebEngine.CssSemiColonProc;
begin
  if CssGetRange = srsCssPropVal then
  begin
    CssSymbolProc;
    CssSetRange(srsCssProp);
  end else
    CssErrorProc;
end;

procedure TSynWebEngine.CssExclamationProc;
begin
  if CssGetRange = srsCssPropVal then
  begin
    CssSymbolProc;
    CssSetRange(srsCssPropValImportant);
    SetRangeBit(8, False);
  end else
    CssErrorProc;
end;

procedure TSynWebEngine.CssStringProc;
var
  prop: Integer;
begin
  if CssGetRange = srsCssPropVal then
  begin
    FInstance^.FTokenID := stkCssValString;
    if FInstance^.FLine[FInstance^.FRun] = #39 then
    begin
      Inc(FInstance^.FRun);
      if not CssCustomStringProc(TSynWebCssString39, False) then
      begin
        CssSetRange(srsCssPropValStr);
        SetRangeBit(8, True);
      end;
    end else
    begin
      Inc(FInstance^.FRun);
      if not CssCustomStringProc(TSynWebCssString34, False) then
      begin
        CssSetRange(srsCssPropValStr);
        SetRangeBit(9, True);
      end;
    end;
    if FInstance^.FTokenID = stkCssValString then
    begin
      prop := CssGetProp - 1;
      if (prop = -1) or (TSynWeb_CssPropsData[prop] and (1 shl 19) = 0) then
        FInstance^.FTokenID := stkCssValUndef;
    end;
  end else
    CssErrorProc;
end;

procedure TSynWebEngine.CssPlusProc;
begin
  if CssGetRange = srsCssPropVal then
  begin
    Inc(FInstance^.FRun);
    // if FInstance^.FLine[FInstance^.FRun] in ['0'..'9', '.'] then
    if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 13) <> 0 then
    begin
      FInstance^.FCssMask := $F5400000;
      CssNumberDefProc;
    end else
      FInstance^.FTokenID := stkCssError;
  end else
    if FInstance^.FOptions.FCssVersion = scvCss21 then
      CssSymbolProc
    else
      CssErrorProc;
end;

procedure TSynWebEngine.CssMinusProc;
begin
  if CssGetRange = srsCssPropVal then
  begin
    Inc(FInstance^.FRun);
    // if FInstance^.FLine[FInstance^.FRun] in ['0'..'9', '.'] then
    if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 13) <> 0 then
    begin
      FInstance^.FCssMask := $8AA00000;
      CssNumberDefProc;
    end else
      FInstance^.FTokenID := stkCssError;
  end else
    if (CssGetRange = srsCssRuleset) and (FInstance^.FLine[FInstance^.FRun + 1] = '-') and
      (FInstance^.FLine[FInstance^.FRun + 2] = '>') then
    begin
      Inc(FInstance^.FRun, 3);
      FInstance^.FTokenID := stkMLComment;
    end else
      CssErrorProc;
end;

procedure TSynWebEngine.CssNumberProc;
begin
  if CssGetRange = srsCssPropVal then
  begin
    FInstance^.FCssMask := $F5400000;
    CssNumberDefProc;
  end else
    CssErrorProc;
end;

procedure TSynWebEngine.CssNumberDefProc;
var
  prop, OldRun: Integer;

  procedure CheckOther;
  begin
    if (FInstance^.FRun - FInstance^.FTokenPos = 1) and
      (FInstance^.FLine[FInstance^.FRun - 1] = '0') then
      FInstance^.FCssMask := FInstance^.FCssMask and $F5400000
    else
      FInstance^.FCssMask := FInstance^.FCssMask and $01E00000;
    if (FInstance^.FTokenPos > 1) and ((FInstance^.FLine[FInstance^.FTokenPos - 1] = '/') and
      (FInstance^.FLine[FInstance^.FTokenPos - 2] <> '*')) then
      FInstance^.FCssMask := FInstance^.FCssMask or $18000000;
  end;

begin
  while FInstance^.FLine[FInstance^.FRun] in ['0'..'9'] do
    Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = '.' then
  begin
    FInstance^.FCssMask := FInstance^.FCssMask and $FF800000;
    Inc(FInstance^.FRun);
    if FInstance^.FLine[FInstance^.FRun] in ['0'..'9'] then
      repeat
        Inc(FInstance^.FRun);
      until not (FInstance^.FLine[FInstance^.FRun] in ['0'..'9'])
    else
    begin
      FInstance^.FTokenID := stkCssError;
      Exit;
    end;
  end;
  if (FInstance^.FLine[FInstance^.FRun] = '%') then
  begin
    FInstance^.FCssMask := FInstance^.FCssMask and $06000000;
    CssSetRange(srsCssPropValSpecial);
  end else
  begin
    OldRun := FInstance^.FRun;
    if CssIdentStartProc then
    begin
      prop := CssSpecialCheck(OldRun, FInstance^.FRun - OldRun);
      if prop <> -1 then
      begin
        FInstance^.FCssMask := FInstance^.FCssMask and TSynWeb_CssSpecialData[prop];
        CssSetRange(srsCssPropValSpecial);
        if (FInstance^.FLine[FInstance^.FRun] = '/') and
          (FInstance^.FLine[FInstance^.FRun + 1] <> '*') then
          SetRangeBit(8, True);
        FInstance^.FRun := OldRun;
      end else
        if FInstance^.FOptions.FCssVersion = scvCss1 then
        begin
          FInstance^.FRun := OldRun;
          CheckOther;
        end else
        begin
          FInstance^.FTokenID := stkCssError;
          Exit;
        end;
    end else
      CheckOther;
  end;
  prop := CssGetProp - 1;
  if (prop = -1) or (TSynWeb_CssPropsData[prop] and FInstance^.FCssMask = 0) then
    FInstance^.FTokenID := stkCssValUndef
  else
    FInstance^.FTokenID := stkCssValNumber;
end;

procedure TSynWebEngine.CssIdentProc;
begin
  if CssIdentStartProc then
  begin
    if (MLTagCheck = stkMLTagName) and
      (TSynWeb_TagsData[MLGetTag - 1] and (1 shl 30) = 0) then
      FInstance^.FTokenID := stkCssSelector
    else
      FInstance^.FTokenID := stkCssSelectorUndef;
  end else
    CssErrorProc;
end;

function TSynWebEngine.CssIdentStartProc: Boolean;
begin
  // if not (FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '\']) or
  if (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 8) = 0) or
    ((FInstance^.FLine[FInstance^.FRun] = '\') and
    (FInstance^.FLine[FInstance^.FRun + 1] in [#0..#31])) then
  begin
    Result := False;
    Exit;
  end;
  FInstance^.FStringLenClean := 0;
  repeat
    if FInstance^.FLine[FInstance^.FRun] <> '\' then
      Inc(FInstance^.FRun)
    else
      if not (FInstance^.FLine[FInstance^.FRun + 1] in [#0..#31]) then
      begin
        Inc(FInstance^.FStringLenClean);
        Inc(FInstance^.FRun, 2);
      end else
        Break;
  until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 9) = 0;
  // until not(FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '\', '0'..'9', '-', '_']);
  FInstance^.FStringLenClean := FInstance^.FRun - FInstance^.FTokenPos -
    FInstance^.FStringLenClean;
  Result := True;
end;

function TSynWebEngine.CssCustomStringProc(AShl: Longword; ADo: Boolean): Boolean;
begin
  if CssCheckNull(ADo) then
  begin
    if (FInstance^.FOptions.FCssVersion > scvCss1) and (FInstance^.FRun > 0) and
      (FInstance^.FLine[FInstance^.FRun - 1] = '\') then
    begin
      Result := False;
      Exit;
    end;
    if not ADo then
      FInstance^.FTokenID := stkCssError;
    Result := True;
    Exit;
  end else
    if PhpCheckBegin(ADo) then
    begin
      if not ADo then
        FInstance^.FTokenID := stkCssValString;
      Result := False;
      Exit;
    end;
  Result := True;
  AShl := 1 shl AShl;
  repeat
    // while not (FInstance^.FLine[FInstance^.FRun] in [#0, AChar, '\', '<']) do
    while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and AShl = 0 do
      Inc(FInstance^.FRun);
    case FInstance^.FLine[FInstance^.FRun] of
    #39, '"':
      begin
        Inc(FInstance^.FRun);
        FInstance^.FTokenID := stkCssValString;
        Exit;
      end;
    '\':
      begin
        Inc(FInstance^.FRun);
        if FInstance^.FLine[FInstance^.FRun] = #0 then
        begin
          if FInstance^.FOptions.FCssVersion = scvCss1 then
          begin
            FInstance^.FTokenID := stkCssError;
            Exit;
          end else
          begin
            FInstance^.FTokenID := stkCssValString;
            Result := False;
            Exit;
          end;
        end else
          if not CssCheckNull(False) and not PhpCheckBegin(False) then
            Inc(FInstance^.FRun);
      end;
    else // case
      if CssCheckNull(False) then
      begin
        FInstance^.FTokenID := stkCssError;
        Exit;
      end else
        if PhpCheckBegin(False) then
        begin
          FInstance^.FTokenID := stkCssValString;
          Result := False;
          Exit;
        end else
          Inc(FInstance^.FRun);
    end;
  until False;
end;

function TSynWebEngine.CssNotWhitespace: Boolean;
begin
  Result := False;
  if CssCheckNull or PhpCheckBegin then
    Exit;
  // if FInstance^.FLine[FInstance^.FRun] in [#0..#32, '/'] then
  if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 11) <> 0 then
    FCssProcTable[FInstance^.FLine[FInstance^.FRun]]
  else
    Result := True;
end;

procedure TSynWebEngine.CssSymbolProc;
begin
  Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkCssSymbol;
end;

procedure TSynWebEngine.CssErrorProc;
begin
  Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkCssError;
end;

procedure TSynWebEngine.CssRangeRulesetProc;
begin
  if GetRangeBit(8) then
  begin
    SetRangeBit(8, False);
    CssIdentStartProc;
    FInstance^.FTokenID := stkCssSelectorId;
  end else
    if GetRangeBit(9) then
    begin
      SetRangeBit(9, False);
      CssIdentStartProc;
      FInstance^.FTokenID := stkCssSelectorClass;
    end else
      FCssProcTable[FInstance^.FLine[FInstance^.FRun]];
end;

procedure TSynWebEngine.CssRangeSelectorAttribProc;

  procedure DoError;
  begin
    CssSetRange(srsCssRuleset);
    FCssProcTable[FInstance^.FLine[FInstance^.FRun]];
    FInstance^.FTokenID := stkCssError;
  end;

  procedure DoEndAttrib;
  begin
    CssSymbolProc;
    CssSetRange(srsCssRuleset);
  end;

begin
  case GetRangeInt(3, 8) of
  0:
    if CssNotWhitespace then
      if CssIdentStartProc then
      begin
        FInstance^.FTokenID := stkCssVal;
        SetRangeInt(3, 8, 1);
      end else
        DoError;
  1:
    if CssNotWhitespace then
      case FInstance^.FLine[FInstance^.FRun] of
      '=':
        begin
          CssSymbolProc;
          SetRangeInt(3, 8, 2);
        end;
      '|', '~':
        begin
          SetRangeInt(3, 8, 2);
          if FInstance^.FLine[FInstance^.FRun + 1] = '=' then
          begin
            Inc(FInstance^.FRun, 2);
            FInstance^.FTokenID := stkCssSymbol;
          end else
            CssErrorProc;
        end;
      ']':
          DoEndAttrib;
      else // case
        DoError;
      end;
  2:
    if CssNotWhitespace then
      case FInstance^.FLine[FInstance^.FRun] of
      #39:
        begin
          Inc(FInstance^.FRun);
          if CssCustomStringProc(TSynWebCssString39, False) then
            SetRangeInt(3, 8, 5)
          else
            SetRangeInt(3, 8, 3);
        end;
      '"':
        begin
          Inc(FInstance^.FRun);
          if CssCustomStringProc(TSynWebCssString34, False) then
            SetRangeInt(3, 8, 5)
          else
            SetRangeInt(3, 8, 4);
        end;
      else // case
        if CssIdentStartProc then
        begin
          FInstance^.FTokenID := stkCssValString;
          SetRangeInt(3, 8, 5);
        end else
          DoError;
      end;
  3:
    if CssCustomStringProc(TSynWebCssString39) then
      SetRangeInt(3, 8, 5);
  4:
    if CssCustomStringProc(TSynWebCssString34) then
      SetRangeInt(3, 8, 5);
  5:
    if CssNotWhitespace then
      if FInstance^.FLine[FInstance^.FRun] = ']' then
        DoEndAttrib
      else
        DoError;
  end;
end;

procedure TSynWebEngine.CssRangeSelectorPseudoProc;
var
  prop: Integer;
begin
  if not GetRangeBit(10) then
  begin
    repeat
      Inc(FInstance^.FRun);
    until TSynWebIdentTable2[FInstance^.FLine[FInstance^.FRun]] and (1 shl 6) = 0;
    // until not(FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '-']);
    prop := CssSpecialCheck(FInstance^.FTokenPos, FInstance^.FRun - FInstance^.FTokenPos);
    if (prop = -1) or (TSynWeb_CssSpecialData[prop] and
      (1 shl (15 - Longword(FInstance^.FOptions.FCssVersion))) = 0) then
    begin
      FInstance^.FTokenID := stkCssError;
      CssSetRange(srsCssRuleset);
    end else
      if (prop <> CssSpecialID_Lang) then
      begin
        FInstance^.FTokenID := stkCssSpecial;
        CssSetRange(srsCssRuleset);
      end else
        if (FInstance^.FLine[FInstance^.FRun] = '(') then
        begin
          FInstance^.FTokenID := stkCssSpecial;
          SetRangeBit(10, True);
        end else
        begin
          FInstance^.FTokenID := stkCssError;
          CssSetRange(srsCssRuleset);
        end;
  end else
    if not GetRangeBit(9) then
    begin
      CssSymbolProc;
      SetRangeBit(9, True);
    end else
      if CssNotWhitespace then
        case FInstance^.FLine[FInstance^.FRun] of
        ',':
          if GetRangeBit(8) then
          begin
            SetRangeBit(8, False);
            CssSymbolProc;
          end else
            CssErrorProc;
        ')':
          begin
            if GetRangeBit(8) then
              CssSymbolProc
            else
              CssErrorProc;
            CssSetRange(srsCssRuleset);
          end;
        else // case
          if CssIdentStartProc then
            if GetRangeBit(8) then
              FInstance^.FTokenID := stkCssError
            else
            begin
              FInstance^.FTokenID := stkCssVal;
              SetRangeBit(8, True);
            end else
          begin
            CssSetRange(srsCssRuleset);
            FCssProcTable[FInstance^.FLine[FInstance^.FRun]];
            FInstance^.FTokenID := stkCssError;
          end;
        end;
end;

procedure TSynWebEngine.CssRangeAtKeywordProc;
var
  prop: Integer;

  procedure DoError;
  begin
    CssSetRange(srsCssRuleset);
    FCssProcTable[FInstance^.FLine[FInstance^.FRun]];
    FInstance^.FTokenID := stkCssError;
  end;

  procedure AtImport;

    procedure AtImport_Medium(ASimple: Boolean);
    begin
      if CssNotWhitespace then
        if not ASimple and (FInstance^.FLine[FInstance^.FRun] = ';') then
        begin
          CssSymbolProc;
          CssSetRange(srsCssRuleset);
        end else
          if CssIdentStartProc then
          begin
            prop := CssSpecialCheck(FInstance^.FTokenPos, FInstance^.FRun -
              FInstance^.FTokenPos);
            if (prop = -1) or (TSynWeb_CssSpecialData[prop] and (1 shl 13) = 0) then
              FInstance^.FTokenID := stkCssValUndef
            else
              FInstance^.FTokenID := stkCssVal;
            SetRangeInt(4, 4, 9);
          end else
            DoError;
    end;

  begin
    case GetRangeInt(4, 4) of
    0:
      if CssNotWhitespace then
        case FInstance^.FLine[FInstance^.FRun] of
        #39:
          begin
            Inc(FInstance^.FRun);
            if CssCustomStringProc(TSynWebCssString39, False) then
              SetRangeInt(4, 4, 8)
            else
              SetRangeInt(4, 4, 1);
          end;
        '"':
          begin
            Inc(FInstance^.FRun);
            if CssCustomStringProc(TSynWebCssString34, False) then
              SetRangeInt(4, 4, 8)
            else
              SetRangeInt(4, 4, 2);
          end;
        else // case
          if not CssIdentStartProc then
            DoError
          else
            if (CssSpecialCheck(FInstance^.FTokenPos, FInstance^.FRun -
              FInstance^.FTokenPos) = CssSpecialID_Url) and
              (FInstance^.FLine[FInstance^.FRun] = '(') then
            begin
              FInstance^.FTokenID := stkCssVal;
              SetRangeInt(4, 4, 3);
            end else
            begin
              FInstance^.FTokenID := stkCssValUndef;
              SetRangeInt(4, 4, 8);
            end;
        end;
    1:
      if CssCustomStringProc(TSynWebCssString39) then
        SetRangeInt(4, 4, 8);
    2:
      if CssCustomStringProc(TSynWebCssString34) then
        SetRangeInt(4, 4, 8);
    3:
      if FInstance^.FLine[FInstance^.FRun + 1] = #0 then
        DoError
      else
      begin
        CssSymbolProc;
        SetRangeInt(4, 4, 4);
      end;
    4:
      case FInstance^.FLine[FInstance^.FRun] of
      #39:
        begin
          Inc(FInstance^.FRun);
          if CssCustomStringProc(TSynWebCssString39, False) then
            SetRangeInt(4, 4, 7)
          else
            SetRangeInt(4, 4, 5);
        end;
      '"':
        begin
          Inc(FInstance^.FRun);
          if CssCustomStringProc(TSynWebCssString34, False) then
            SetRangeInt(4, 4, 7)
          else
            SetRangeInt(4, 4, 6);
        end;
      #1..#32:
        CssSpaceProc;
      else // case
        if (FInstance^.FLine[FInstance^.FRun] = '/') and
          (FInstance^.FLine[FInstance^.FRun + 1] = '*') then
          CssSlashProc
        else
        begin
          if CssCheckNull or PhpCheckBegin then
            Exit;
          repeat
            // while not (FInstance^.FLine[FInstance^.FRun] in [#0..#32, '(', ')', ',', '\', '<']) do
            while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and
              (1 shl 14) = 0 do
              Inc(FInstance^.FRun);
            case FInstance^.FLine[FInstance^.FRun] of
             '\':
              begin
                Inc(FInstance^.FRun);
                if FInstance^.FLine[FInstance^.FRun] <> #0 then
                  Inc(FInstance^.FRun)
                else
                  Break;
              end;
            '<':
              if CssCheckNull(False) or PhpCheckBegin(False) then
                Break
              else
                Inc(FInstance^.FRun);
            else // case
              Break;
            end;
          until False;
          FInstance^.FTokenID := stkCssValString;
          SetRangeInt(4, 4, 7);
        end;
      end;
    5:
      if CssCustomStringProc(TSynWebCssString39) then
        SetRangeInt(4, 4, 7);
    6:
      if CssCustomStringProc(TSynWebCssString34) then
        SetRangeInt(4, 4, 7);
    7:
      if CssNotWhitespace then
        if FInstance^.FLine[FInstance^.FRun] = ')' then
        begin
          CssSymbolProc;
          SetRangeInt(4, 4, 8);
        end else
          DoError;
    8:
      AtImport_Medium(False);
    9:
      if CssNotWhitespace then
        case FInstance^.FLine[FInstance^.FRun] of
        ';':
          begin
            CssSymbolProc;
            CssSetRange(srsCssRuleset);
          end;
        ',':
          begin
            CssSymbolProc;
            SetRangeInt(4, 4, 10);
          end;
        else // case
          DoError;
        end;
    10:
      AtImport_Medium(True);
    end;
  end;

  procedure AtMedia;
  var
    prop: Integer;
  begin
    if CssNotWhitespace then
      if GetRangeBit(7) then
      begin
        SetRangeBit(7, False);
        case FInstance^.FLine[FInstance^.FRun] of
        ',':
          CssSymbolProc;
        '{':
          begin
            CssSymbolProc;
            SetRangeBit(11, True);
            CssSetRange(srsCssRuleset);
          end;
        else // case
          DoError;
        end;
      end else
        if CssIdentStartProc then
        begin
          prop := CssSpecialCheck(FInstance^.FTokenPos, FInstance^.FRun -
            FInstance^.FTokenPos);
          if (prop = -1) or (TSynWeb_CssSpecialData[prop] and (1 shl 13) = 0) then
            FInstance^.FTokenID := stkCssValUndef
          else
            FInstance^.FTokenID := stkCssVal;
          SetRangeBit(7, True);
        end else
          DoError;
  end;

  procedure AtPage;
  var
    prop: Integer;

    procedure AtPage_Declaration;
    begin
      SetRangeInt(11, 0, 0);
      CssSymbolProc;
      CssSetRange(srsCssProp);
    end;

  begin
    case GetRangeInt(2, 6) of
    0:
      if CssNotWhitespace then
        case FInstance^.FLine[FInstance^.FRun] of
        '{':
          AtPage_Declaration;
        ':':
          // if not (FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '\']) or
          if (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 1]] and
            (1 shl 8) = 0) or
            ((FInstance^.FLine[FInstance^.FRun + 1] = '\') and
            (FInstance^.FLine[FInstance^.FRun + 2] in [#0..#31])) then
            DoError
          else
          begin
            SetRangeInt(2, 6, 1);
            CssSymbolProc;
          end;
        else // case
          DoError;
        end;
    1:
      begin
        CssIdentStartProc;
        prop := CssSpecialCheck(FInstance^.FTokenPos, FInstance^.FRun - FInstance^.FTokenPos);
        if (prop = -1) or (TSynWeb_CssSpecialData[prop] and (1 shl 11) = 0) then
          FInstance^.FTokenID := stkCssError
        else
          FInstance^.FTokenID := stkCssSpecial;
        SetRangeInt(2, 6, 2);
      end;
    2:
      if CssNotWhitespace then
        if FInstance^.FLine[FInstance^.FRun] = '{' then
          AtPage_Declaration
        else
          DoError;
    end;
  end;

  procedure AtCharset;
  begin
    case GetRangeInt(2, 6) of
    0:
      if CssNotWhitespace then
        case FInstance^.FLine[FInstance^.FRun] of
        #39:
          begin
            Inc(FInstance^.FRun);
            if CssCustomStringProc(TSynWebCssString39, False) then
              SetRangeInt(2, 6, 3)
            else
              SetRangeBit(6, True);
          end;
        '"':
          begin
            Inc(FInstance^.FRun);
            if CssCustomStringProc(TSynWebCssString34, False) then
              SetRangeInt(2, 6, 3)
            else
              SetRangeBit(7, True);
          end;
        else // case
          DoError;
        end;
    1:
      if CssCustomStringProc(TSynWebCssString39) then
        SetRangeInt(2, 6, 3);
    2:
      if CssCustomStringProc(TSynWebCssString34) then
        SetRangeInt(2, 6, 3);
    3:
      if CssNotWhitespace then
        if FInstance^.FLine[FInstance^.FRun] = ';' then
        begin
          CssSymbolProc;
          CssSetRange(srsCssRuleset);
        end else
          DoError;
    end;
  end;

begin
  if not GetRangeBit(10) then
  begin
    SetRangeBit(10, True);
    repeat
      Inc(FInstance^.FRun);
    until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 0) = 0;
    // until not(FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z']);
    if GetRangeBit(11) then
    begin
      FInstance^.FTokenID := stkCssError;
      CssSetRange(srsCssRuleset);
    end else
      case CssSpecialCheck(FInstance^.FTokenPos, FInstance^.FRun - FInstance^.FTokenPos) of
      CssSpecialID_Import:
        begin
          SetRangeInt(2, 8, 0);
          FInstance^.FTokenID := stkCssSpecial;
        end;
      CssSpecialID_Media:
        if FInstance^.FOptions.FCssVersion = scvCss1 then
        begin
          FInstance^.FTokenID := stkCssError;
          CssSetRange(srsCssRuleset);
        end else
        begin
          SetRangeInt(2, 8, 1);
          FInstance^.FTokenID := stkCssSpecial;
        end;
      CssSpecialID_Page:
        if FInstance^.FOptions.FCssVersion = scvCss1 then
        begin
          FInstance^.FTokenID := stkCssError;
          CssSetRange(srsCssRuleset);
        end else
        begin
          SetRangeInt(2, 8, 2);
          FInstance^.FTokenID := stkCssSpecial;
        end;
      CssSpecialID_Charset:
        if FInstance^.FOptions.FCssVersion = scvCss1 then
        begin
          FInstance^.FTokenID := stkCssError;
          CssSetRange(srsCssRuleset);
        end else
        begin
          SetRangeInt(2, 8, 3);
          FInstance^.FTokenID := stkCssSpecial;
        end;
      else // case
        FInstance^.FTokenID := stkCssError;
        CssSetRange(srsCssRuleset);
      end;
  end else
    case GetRangeInt(2, 8) of
    0:
      AtImport;
    1:
      AtMedia;
    2:
      AtPage;
    3:
      AtCharset;
    end;
end;

procedure TSynWebEngine.CssRangePropProc;
begin
  if GetRangeBit(8) then
  begin
    if CssNotWhitespace then
      case FInstance^.FLine[FInstance^.FRun] of
      '}':
        begin
          CssErrorProc;
          CssSetRange(srsCssRuleset);
        end;
      ':':
        begin
          CssSymbolProc;
          CssSetRange(srsCssPropVal);
          SetRangeBit(8, False);
        end;
      else // case
        CssErrorProc;
      end;
  end else
    if CssNotWhitespace then
      if CssIdentStartProc then
      begin
        FInstance^.FTokenID := CssPropCheck;
        SetRangeBit(8, True);
      end else
      begin
        CssSetProp(0);
        case FInstance^.FLine[FInstance^.FRun] of
        '}':
          begin
            CssSetRange(srsCssRuleset);
            CssSymbolProc;
            Exit;
          end;
        ':':
          CssSetRange(srsCssPropVal);
        end;
        CssErrorProc;
      end;
end;

procedure TSynWebEngine.CssRangePropValProc;
begin
  // if FInstance^.FLine[FInstance^.FRun] in [#0..#32, '/', '#', '!', ';', '}', '+', '-', '0'..'9', '.', ',', '"', #39, '<'] then
  if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 12) <> 0 then
    FCssProcTable[FInstance^.FLine[FInstance^.FRun]]
  else
    if CssIdentStartProc then
    begin
      FInstance^.FTokenID := CssValCheck;
      if TSynWeb_CssValsData[FInstance^.FTokenLastID][Longword(FInstance^.FOptions.FCssVersion)]
        [3] and (1 shl 31) <> 0 then
        if FInstance^.FLine[FInstance^.FRun] = '(' then
        begin
          SetRangeInt(3, 8, 0);
          case FInstance^.FTokenLastID of
          CssValID_Rgb:
            CssSetRange(srsCssPropValRgb);
          CssValID_Url:
            CssSetRange(srsCssPropValUrl);
          CssValID_Rect:
            CssSetRange(srsCssPropValRect);
          else // case
            CssSetRange(srsCssPropValFunc);
          end;
        end else
          FInstance^.FTokenID := stkCssValUndef;
    end else
      CssErrorProc;
end;

procedure TSynWebEngine.CssRangePropValStrProc;
var
  prop: Integer;
begin
  if GetRangeBit(8) then
  begin
    if CssCustomStringProc(TSynWebCssString39) then
    begin
      CssSetRange(srsCssPropVal);
      SetRangeBit(8, False);
    end;
  end else
    if CssCustomStringProc(TSynWebCssString34) then
    begin
      CssSetRange(srsCssPropVal);
      SetRangeBit(9, False);
    end;
  if FInstance^.FTokenID = stkCssValString then
  begin
    prop := CssGetProp - 1;
    if (prop = -1) or (TSynWeb_CssPropsData[prop] and (1 shl 19) = 0) then
      FInstance^.FTokenID := stkCssValUndef;
  end;
end;

procedure TSynWebEngine.CssRangePropValRgbProc;

  procedure NumberProc;
  begin
    if GetRangeBit(8) then
      FInstance^.FTokenID := stkCssError
    else
      FInstance^.FTokenID := stkCssValNumber;
    SetRangeBit(8, True);
    if FInstance^.FLine[FInstance^.FRun] = '+' then
      if FInstance^.FLine[FInstance^.FRun + 1] in ['0'..'9', '.'] then
        Inc(FInstance^.FRun)
      else
      begin
        CssErrorProc;
        Exit;
      end;
    while FInstance^.FLine[FInstance^.FRun] in ['0'..'9'] do
      Inc(FInstance^.FRun);
    if FInstance^.FLine[FInstance^.FRun] = '.' then
    begin
      Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] in ['0'..'9'] then
      begin
        repeat
          Inc(FInstance^.FRun);
        until not (FInstance^.FLine[FInstance^.FRun] in ['0'..'9']);
        if FInstance^.FLine[FInstance^.FRun] = '%' then
          Exit;
      end;
      FInstance^.FTokenID := stkCssError;
    end;
  end;

begin
  if GetRangeBit(10) then
  begin
    if CssNotWhitespace then
      case FInstance^.FLine[FInstance^.FRun] of
      ',':
        if GetRangeBit(8) then
        begin
          SetRangeBit(8, False);
          CssSymbolProc;
        end else
          CssErrorProc;
      '0'..'9', '.', '+':
        NumberProc;
      '%':
        if (FInstance^.FRun > 0) and (FInstance^.FLine[FInstance^.FRun - 1] in
          ['0'..'9']) then
          CssSymbolProc
        else
          CssErrorProc;
      ';':
        begin
          CssErrorProc;
          CssSetRange(srsCssProp);
          SetRangeInt(3, 8, 0);
        end;
      '}':
        begin
          CssErrorProc;
          CssSetRange(srsCssRuleset);
        end;
      ')':
        begin
          if GetRangeBit(8) then
            CssSymbolProc
          else
            CssErrorProc;
          CssSetRange(srsCssPropVal);
          SetRangeInt(3, 8, 0);
        end;
      else // case
        CssSetRange(srsCssPropVal);
        SetRangeInt(3, 8, 0);
        CssRangePropValProc;
        FInstance^.FTokenID := stkCssError;
      end;
  end else
  begin
    CssSymbolProc;
    SetRangeBit(10, True);
  end;
end;

procedure TSynWebEngine.CssRangePropValFuncProc;
begin
  if GetRangeBit(10) then
  begin
    case GetRangeInt(2, 8) of
    0:
      if CssNotWhitespace then
        case FInstance^.FLine[FInstance^.FRun] of
        #39:
          begin
            Inc(FInstance^.FRun);
            if not CssCustomStringProc(TSynWebCssString39, False) then
              SetRangeBit(8, True);
          end;
        '"':
          begin
            Inc(FInstance^.FRun);
            if not CssCustomStringProc(TSynWebCssString34, False) then
              SetRangeBit(9, True);
          end;
        ',':
          CssSymbolProc;
        ';':
          begin
            CssErrorProc;
            CssSetRange(srsCssProp);
            SetRangeInt(3, 8, 0);
          end;
        '}':
          begin
            CssErrorProc;
            CssSetRange(srsCssRuleset);
          end;
        ')':
          begin
            CssSymbolProc;
            CssSetRange(srsCssPropVal);
            SetRangeInt(3, 8, 0);
          end;
        else // case
          if CssIdentStartProc then
            FInstance^.FTokenID := stkCssVal
          else
            CssErrorProc;
        end;
    1:
      if CssCustomStringProc(TSynWebCssString39) then
        SetRangeBit(8, False);
    2:
      if CssCustomStringProc(TSynWebCssString34) then
        SetRangeBit(9, False);
    end;
  end else
  begin
    CssSymbolProc;
    SetRangeBit(10, True);
  end;
end;

procedure TSynWebEngine.CssRangePropValSpecialProc;
var
  prop: Integer;
begin
  if FInstance^.FLine[FInstance^.FRun] = '%' then
    CssSymbolProc
  else
    if (FInstance^.FRun > 0) and (FInstance^.FLine[FInstance^.FRun - 1] = '#') then
    begin
      Inc(FInstance^.FRun, 3);
      // if (FInstance^.FLine[FInstance^.FRun] in ['a'..'f', 'A'..'F', '0'..'9']) and
      if (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 10) <> 0) and
        // if (FInstance^.FLine[FInstance^.FRun+1] in ['a'..'f', 'A'..'F', '0'..'9']) and
        (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 1]] and (1 shl 10) <> 0) and
        // if (FInstance^.FLine[FInstance^.FRun+2] in ['a'..'f', 'A'..'F', '0'..'9']) then
        (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 2]] and (1 shl 10) <> 0) then
        Inc(FInstance^.FRun, 3);
      prop := CssGetProp - 1;
      if (prop = -1) or (TSynWeb_CssPropsData[prop] and (1 shl 18) = 0) then
        FInstance^.FTokenID := stkCssValUndef
      else
        FInstance^.FTokenID := stkCssValNumber;
    end else
    begin
      CssIdentStartProc;
      FInstance^.FTokenID := stkCssSymbol;
    end;
  CssSetRange(srsCssPropVal);
end;

procedure TSynWebEngine.CssRangePropValImportantProc;

  procedure DoSymbol;
  begin
    if GetRangeBit(8) then
    begin
      SetRangeBit(8, False);
      CssSymbolProc;
    end else
      CssErrorProc;
  end;

begin
  if CssNotWhitespace then
    case FInstance^.FLine[FInstance^.FRun] of
    ';':
      begin
        DoSymbol;
        CssSetRange(srsCssProp);
      end;
    '}':
      begin
        DoSymbol;
        CssSetRange(srsCssRuleset);
      end;
    else // case
      if CssIdentStartProc then
      begin
        if GetRangeBit(8) then
          FInstance^.FTokenID := stkCssError
        else
        begin
          CssSpecialCheck(FInstance^.FTokenPos, FInstance^.FRun - FInstance^.FTokenPos);
          if FInstance^.FTokenLastID = CssSpecialID_Important then
            FInstance^.FTokenID := stkCssSpecial
          else
            FInstance^.FTokenID := stkCssError;
          SetRangeBit(8, True);
        end;
      end else
        CssErrorProc;
    end;
end;

procedure TSynWebEngine.CssRangePropValUrlProc;
begin
  if GetRangeBit(10) then
  begin
    case GetRangeInt(2, 8) of
    0:
      case FInstance^.FLine[FInstance^.FRun] of
      #39:
        begin
          Inc(FInstance^.FRun);
          if CssCustomStringProc(TSynWebCssString39, False) then
            SetRangeInt(2, 8, 3)
          else
            SetRangeBit(8, True);
        end;
      '"':
        begin
          Inc(FInstance^.FRun);
          if CssCustomStringProc(TSynWebCssString34, False) then
            SetRangeInt(2, 8, 3)
          else
            SetRangeBit(9, True);
        end;
      #1..#32:
        CssSpaceProc;
      ';':
        begin
          CssErrorProc;
          CssSetRange(srsCssProp);
          SetRangeInt(3, 8, 0);
        end;
      '}':
        begin
          CssErrorProc;
          CssSetRange(srsCssRuleset);
        end;
      ')':
        begin
          CssErrorProc;
          CssSetRange(srsCssPropVal);
          SetRangeInt(3, 8, 0);
        end;
      else // case
        if (FInstance^.FLine[FInstance^.FRun] = '/') and
          (FInstance^.FLine[FInstance^.FRun + 1] = '*') then
          CssSlashProc
        else
        begin
          if CssCheckNull or PhpCheckBegin then
            Exit;
          repeat
            // while not (FInstance^.FLine[FInstance^.FRun] in [#0..#32, '(', ')', ',', '\', '<']) do
            while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and
              (1 shl 14) = 0 do
              Inc(FInstance^.FRun);
            case FInstance^.FLine[FInstance^.FRun] of
            '\':
              begin
                Inc(FInstance^.FRun);
                if FInstance^.FLine[FInstance^.FRun] <> #0 then
                  Inc(FInstance^.FRun)
                else
                  Break;
              end;
            '<':
              if CssCheckNull(False) or PhpCheckBegin(False) then
                Break
              else
                Inc(FInstance^.FRun);
            else // case
              Break;
            end;
          until False;
          FInstance^.FTokenID := stkCssValString;
          SetRangeInt(2, 8, 3);
        end;
      end;
    1:
      if CssCustomStringProc(TSynWebCssString39) then
        SetRangeInt(2, 8, 3);
    2:
      if CssCustomStringProc(TSynWebCssString34) then
        SetRangeInt(2, 8, 3);
    3:
      if FInstance^.FLine[FInstance^.FRun] = ')' then
      begin
        CssSymbolProc;
        SetRangeInt(3, 8, 0);
        CssSetRange(srsCssPropVal);
      end else
        if CssNotWhitespace then
        begin
          SetRangeInt(3, 8, 0);
          CssSetRange(srsCssPropVal);
          CssRangePropValProc;
          FInstance^.FTokenID := stkCssError;
        end;
    end;
  end else
  begin
    CssSymbolProc;
    SetRangeBit(10, True);
  end;
end;

procedure TSynWebEngine.CssRangePropValRectProc;

  procedure Shape_LengthProc;
  var
    prop, OldRun: Integer;

    procedure CheckOther;
    begin
      if GetRangeBit(8) then
        FInstance^.FTokenID := stkCssError
      else
        if (FInstance^.FRun - FInstance^.FTokenPos = 1) and
          (FInstance^.FLine[FInstance^.FRun - 1] = '0') then
          FInstance^.FTokenID := stkCssValNumber
        else
          FInstance^.FTokenID := stkCssValUndef;
      SetRangeBit(8, True);
    end;

  begin
    while FInstance^.FLine[FInstance^.FRun] in ['0'..'9'] do
      Inc(FInstance^.FRun);
    if FInstance^.FLine[FInstance^.FRun] = '.' then
    begin
      Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] in ['0'..'9'] then
        repeat
          Inc(FInstance^.FRun);
        until not (FInstance^.FLine[FInstance^.FRun] in ['0'..'9'])
      else
      begin
        FInstance^.FTokenID := stkCssError;
        Exit;
      end;
    end;
    OldRun := FInstance^.FRun;
    if CssIdentStartProc then
    begin
      prop := CssSpecialCheck(OldRun, FInstance^.FRun - OldRun);
      if prop <> -1 then
      begin
        FInstance^.FRun := OldRun;
        SetRangeBit(9, True);
        if (TSynWeb_CssSpecialData[prop] and (1 shl 28) = 0) or GetRangeBit(8) then
          FInstance^.FTokenID := stkCssError
        else
          FInstance^.FTokenID := stkCssValNumber;
      end else
        if FInstance^.FOptions.FCssVersion = scvCss1 then
        begin
          FInstance^.FRun := OldRun;
          CheckOther;
        end else
        begin
          FInstance^.FTokenID := stkCssError;
          Exit;
        end;
    end else
      CheckOther;
  end;

begin
  if not GetRangeBit(10) then
  begin
    CssSymbolProc;
    SetRangeBit(10, True);
  end else
    if GetRangeBit(9) then
    begin
      CssIdentStartProc;
      if GetRangeBit(8) then
        FInstance^.FTokenID := stkCssError
      else
        FInstance^.FTokenID := stkCssSymbol;
      SetRangeBit(9, False);
      SetRangeBit(8, True);
    end else
      if CssNotWhitespace then
        case FInstance^.FLine[FInstance^.FRun] of
        ',':
          if GetRangeBit(8) then
          begin
            SetRangeBit(8, False);
            CssSymbolProc;
          end else
            CssErrorProc;
        '0'..'9', '.':
          Shape_LengthProc;
        ')':
          begin
            if GetRangeBit(8) then
              CssSymbolProc
            else
              CssErrorProc;
            CssSetRange(srsCssPropVal);
            SetRangeInt(3, 8, 0);
          end;
        ';':
          begin
            CssErrorProc;
            CssSetRange(srsCssProp);
            SetRangeInt(3, 8, 0);
          end;
        '}':
          begin
            CssErrorProc;
            CssSetRange(srsCssRuleset);
            SetRangeInt(3, 8, 0);
          end;
        else // case
          if not CssIdentStartProc then
            CssErrorProc
          else
          begin
            if GetRangeBit(8) then
              FInstance^.FTokenID := stkCssError
            else
              if CssSpecialCheck(FInstance^.FTokenPos, FInstance^.FRun -
                FInstance^.FTokenPos) = CssSpecialID_Auto then
                FInstance^.FTokenID := stkCssVal
              else
                FInstance^.FTokenID := stkCssValUndef;
            SetRangeBit(8, True);
          end;
        end;
end;

procedure TSynWebEngine.CssRangeCommentProc;
begin
  if CssCheckNull or PhpCheckBegin then
    Exit;
  repeat
    // while not (FInstance^.FLine[FInstance^.FRun] in [#0, '*', '<']) do
    while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 26) = 0 do
      Inc(FInstance^.FRun);
    case FInstance^.FLine[FInstance^.FRun] of
    #0:
      Break;
    '<':
      if CssCheckNull(False) or PhpCheckBegin(False) then
        Break
      else
        Inc(FInstance^.FRun);
    '*':
      begin
        Inc(FInstance^.FRun);
        if FInstance^.FLine[FInstance^.FRun] = '/' then
        begin
          Inc(FInstance^.FRun);
          SetRangeBit(12, False);
          Break;
        end;
      end;
    end;
  until False;
  FInstance^.FTokenID := stkCssComment;
end;

function TSynWebEngine.CssPropKeyComp(const ID: Integer): Boolean;
var
  I: Integer;
  Temp: PChar;
  aKey: String;
begin
  aKey := TSynWeb_CssProps[ID];
  Temp := FInstance^.FToIdent;
  if Length(aKey) = FInstance^.FStringLenClean then
  begin
    if FInstance^.FStringLenClean = FInstance^.FStringLen then
      for i := 1 to FInstance^.FStringLen do
      begin
        if TSynWebInsensitiveHashTable[Temp^] <>
          TSynWebInsensitiveHashTable[aKey[i]] then
        begin
          Result := False;
          Exit;
        end;
        Inc(Temp);
      end else
      for i := 1 to FInstance^.FStringLenClean do
      begin
        if Temp^ = '\' then
          Inc(Temp);
        if TSynWebInsensitiveHashTable[Temp^] <>
          TSynWebInsensitiveHashTable[aKey[i]] then
        begin
          Result := False;
          Exit;
        end;
        Inc(Temp);
      end;
    FInstance^.FTokenLastID := ID;
    Result := True;
  end else
    Result := False;
end;

function TSynWebEngine.CssPropCheck: TSynWebTokenKind;
var
  HashKey: Longword;

  procedure KeyHash(ToHash: PChar);
  var
    i: Integer;
  begin
    HashKey := 0;
    FInstance^.FStringLen := FInstance^.FRun - FInstance^.FTokenPos;
    for i := 0 to FInstance^.FStringLen - 1 do
    begin
      Inc(HashKey, TSynWebIdentHashTable[ToHash^]);
      Inc(ToHash);
    end;
  end;

begin
  FInstance^.FToIdent := @FInstance^.FLine[FInstance^.FTokenPos];
  KeyHash(FInstance^.FToIdent);
  FInstance^.FTokenLastID := -1;
  if HashKey <= CssPropMaxKeyHash then
  begin
    Result := FCssPropIdentFuncTable[HashKey];
    if (FInstance^.FTokenLastID <> -1) and
      (TSynWeb_CssPropsData[FInstance^.FTokenLastID] and
      (1 shl Longword(FInstance^.FOptions.FCssVersion)) = 0) then
      Result := stkCssPropUndef;
  end else
    Result := stkCssPropUndef;
  CssSetProp(FInstance^.FTokenLastID + 1);
end;

function TSynWebEngine.CssPropUndef: TSynWebTokenKind;
begin
  Result:=stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc29: TSynWebTokenKind;
begin
  if  CssPropKeyComp(37) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc39: TSynWebTokenKind;
begin
  if  CssPropKeyComp(31) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc40: TSynWebTokenKind;
begin
  if  CssPropKeyComp(32) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc43: TSynWebTokenKind;
begin
  if  CssPropKeyComp(53) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc51: TSynWebTokenKind;
begin
  if  CssPropKeyComp(104) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc52: TSynWebTokenKind;
begin
  if  CssPropKeyComp(93) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc54: TSynWebTokenKind;
begin
  if  CssPropKeyComp(45) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc55: TSynWebTokenKind;
begin
  if  CssPropKeyComp(46) or
      CssPropKeyComp(75) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc56: TSynWebTokenKind;
begin
  if  CssPropKeyComp(86) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc57: TSynWebTokenKind;
begin
  if  CssPropKeyComp(52) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc62: TSynWebTokenKind;
begin
  if  CssPropKeyComp(7) or
      CssPropKeyComp(60) or
      CssPropKeyComp(83) or
      CssPropKeyComp(92) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc63: TSynWebTokenKind;
begin
  if  CssPropKeyComp(33) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc64: TSynWebTokenKind;
begin
  if  CssPropKeyComp(112) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc85: TSynWebTokenKind;
begin
  if  CssPropKeyComp(29) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc86: TSynWebTokenKind;
begin
  if  CssPropKeyComp(42) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc88: TSynWebTokenKind;
begin
  if  CssPropKeyComp(109) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc91: TSynWebTokenKind;
begin
  if  CssPropKeyComp(34) or
      CssPropKeyComp(69) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc93: TSynWebTokenKind;
begin
  if  CssPropKeyComp(111) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc94: TSynWebTokenKind;
begin
  if  CssPropKeyComp(40) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc95: TSynWebTokenKind;
begin
  if  CssPropKeyComp(91) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc96: TSynWebTokenKind;
begin
  if  CssPropKeyComp(1) or
      CssPropKeyComp(70) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc97: TSynWebTokenKind;
begin
  if  CssPropKeyComp(41) or
      CssPropKeyComp(90) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc98: TSynWebTokenKind;
begin
  if  CssPropKeyComp(0) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc100: TSynWebTokenKind;
begin
  if  CssPropKeyComp(98) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc103: TSynWebTokenKind;
begin
  if  CssPropKeyComp(43) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc116: TSynWebTokenKind;
begin
  if  CssPropKeyComp(74) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc117: TSynWebTokenKind;
begin
  if  CssPropKeyComp(38) or
      CssPropKeyComp(89) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc118: TSynWebTokenKind;
begin
  if  CssPropKeyComp(39) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc120: TSynWebTokenKind;
begin
  if  CssPropKeyComp(114) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc131: TSynWebTokenKind;
begin
  if  CssPropKeyComp(67) or
      CssPropKeyComp(94) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc133: TSynWebTokenKind;
begin
  if  CssPropKeyComp(65) or
      CssPropKeyComp(105) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc135: TSynWebTokenKind;
begin
  if  CssPropKeyComp(55) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc136: TSynWebTokenKind;
begin
  if  CssPropKeyComp(77) or
      CssPropKeyComp(107) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc138: TSynWebTokenKind;
begin
  if  CssPropKeyComp(68) or
      CssPropKeyComp(97) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc139: TSynWebTokenKind;
begin
  if  CssPropKeyComp(87) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc140: TSynWebTokenKind;
begin
  if  CssPropKeyComp(66) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc143: TSynWebTokenKind;
begin
  if  CssPropKeyComp(14) or
      CssPropKeyComp(62) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc144: TSynWebTokenKind;
begin
  if  CssPropKeyComp(79) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc147: TSynWebTokenKind;
begin
  if  CssPropKeyComp(110) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc150: TSynWebTokenKind;
begin
  if  CssPropKeyComp(84) or
      CssPropKeyComp(100) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc151: TSynWebTokenKind;
begin
  if  CssPropKeyComp(24) or
      CssPropKeyComp(64) or
      CssPropKeyComp(85) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc152: TSynWebTokenKind;
begin
  if  CssPropKeyComp(48) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc153: TSynWebTokenKind;
begin
  if  CssPropKeyComp(30) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc155: TSynWebTokenKind;
begin
  if  CssPropKeyComp(78) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc158: TSynWebTokenKind;
begin
  if  CssPropKeyComp(108) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc159: TSynWebTokenKind;
begin
  if  CssPropKeyComp(47) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc162: TSynWebTokenKind;
begin
  if  CssPropKeyComp(18) or
      CssPropKeyComp(63) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc163: TSynWebTokenKind;
begin
  if  CssPropKeyComp(13) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc164: TSynWebTokenKind;
begin
  if  CssPropKeyComp(28) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc165: TSynWebTokenKind;
begin
  if  CssPropKeyComp(51) or
      CssPropKeyComp(88) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc167: TSynWebTokenKind;
begin
  if  CssPropKeyComp(113) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc168: TSynWebTokenKind;
begin
  if  CssPropKeyComp(44) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc169: TSynWebTokenKind;
begin
  if  CssPropKeyComp(4) or
      CssPropKeyComp(22) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc171: TSynWebTokenKind;
begin
  if  CssPropKeyComp(106) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc172: TSynWebTokenKind;
begin
  if  CssPropKeyComp(99) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc173: TSynWebTokenKind;
begin
  if  CssPropKeyComp(102) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc174: TSynWebTokenKind;
begin
  if  CssPropKeyComp(49) or
      CssPropKeyComp(95) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc178: TSynWebTokenKind;
begin
  if  CssPropKeyComp(50) or
      CssPropKeyComp(76) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc179: TSynWebTokenKind;
begin
  if  CssPropKeyComp(56) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc181: TSynWebTokenKind;
begin
  if  CssPropKeyComp(23) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc183: TSynWebTokenKind;
begin
  if  CssPropKeyComp(12) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc185: TSynWebTokenKind;
begin
  if  CssPropKeyComp(8) or
      CssPropKeyComp(61) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc187: TSynWebTokenKind;
begin
  if  CssPropKeyComp(54) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc192: TSynWebTokenKind;
begin
  if  CssPropKeyComp(80) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc193: TSynWebTokenKind;
begin
  if  CssPropKeyComp(81) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc197: TSynWebTokenKind;
begin
  if  CssPropKeyComp(3) or
      CssPropKeyComp(71) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc198: TSynWebTokenKind;
begin
  if  CssPropKeyComp(73) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc199: TSynWebTokenKind;
begin
  if  CssPropKeyComp(6) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc201: TSynWebTokenKind;
begin
  if  CssPropKeyComp(36) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc202: TSynWebTokenKind;
begin
  if  CssPropKeyComp(82) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc211: TSynWebTokenKind;
begin
  if  CssPropKeyComp(101) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc215: TSynWebTokenKind;
begin
  if  CssPropKeyComp(72) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc231: TSynWebTokenKind;
begin
  if  CssPropKeyComp(103) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc235: TSynWebTokenKind;
begin
  if  CssPropKeyComp(35) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc239: TSynWebTokenKind;
begin
  if  CssPropKeyComp(2) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc244: TSynWebTokenKind;
begin
  if  CssPropKeyComp(15) or
      CssPropKeyComp(96) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc245: TSynWebTokenKind;
begin
  if  CssPropKeyComp(17) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc251: TSynWebTokenKind;
begin
  if  CssPropKeyComp(5) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc252: TSynWebTokenKind;
begin
  if  CssPropKeyComp(25) or
      CssPropKeyComp(57) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc253: TSynWebTokenKind;
begin
  if  CssPropKeyComp(27) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc262: TSynWebTokenKind;
begin
  if  CssPropKeyComp(16) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc263: TSynWebTokenKind;
begin
  if  CssPropKeyComp(19) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc264: TSynWebTokenKind;
begin
  if  CssPropKeyComp(21) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc270: TSynWebTokenKind;
begin
  if  CssPropKeyComp(26) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc281: TSynWebTokenKind;
begin
  if  CssPropKeyComp(20) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc283: TSynWebTokenKind;
begin
  if  CssPropKeyComp(59) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc286: TSynWebTokenKind;
begin
  if  CssPropKeyComp(9) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc287: TSynWebTokenKind;
begin
  if  CssPropKeyComp(11) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc304: TSynWebTokenKind;
begin
  if  CssPropKeyComp(10) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssPropFunc334: TSynWebTokenKind;
begin
  if  CssPropKeyComp(58) then
    Result := stkCssProp
  else
    Result := stkCssPropUndef;
end;

function TSynWebEngine.CssValKeyComp(const ID: Integer): Boolean;
var
  I: Integer;
  Temp: PChar;
  aKey: String;
begin
  aKey := TSynWeb_CssVals[ID];
  Temp := FInstance^.FToIdent;
  if Length(aKey) = FInstance^.FStringLenClean then
  begin
    if FInstance^.FStringLenClean = FInstance^.FStringLen then
      for i := 1 to FInstance^.FStringLen do
      begin
        if TSynWebInsensitiveHashTable[Temp^] <>
          TSynWebInsensitiveHashTable[aKey[i]] then
        begin
          Result := False;
          Exit;
        end;
        Inc(Temp);
      end else
      for i := 1 to FInstance^.FStringLenClean do
      begin
        if Temp^ = '\' then
          Inc(Temp);
        if TSynWebInsensitiveHashTable[Temp^] <>
          TSynWebInsensitiveHashTable[aKey[i]] then
        begin
          Result := False;
          Exit;
        end;
        Inc(Temp);
      end;
    FInstance^.FTokenLastID := ID;
    Result := True;
  end else
    Result := False;
end;

function TSynWebEngine.CssValCheck: TSynWebTokenKind;
var
  HashKey: Longword;
  prop: Integer;

  procedure KeyHash(ToHash: PChar);
  var
    i: Integer;
  begin
    HashKey := 0;
    FInstance^.FStringLen := FInstance^.FRun - FInstance^.FTokenPos;
    for i := 0 to FInstance^.FStringLen - 1 do
    begin
      Inc(HashKey, TSynWebIdentHashTable[ToHash^]);
      Inc(ToHash);
    end;
  end;

begin
  FInstance^.FToIdent := @FInstance^.FLine[FInstance^.FTokenPos];
  KeyHash(FInstance^.FToIdent);
  FInstance^.FTokenLastID := -1;
  if HashKey <= CssValMaxKeyHash then
  begin
    Result := FCssValIdentFuncTable[HashKey];
    if Result = stkCssVal then
    begin
      prop := CssGetProp - 1;
      if (prop = -1) or (TSynWeb_CssValsData[FInstance^.FTokenLastID]
        [Longword(FInstance^.FOptions.FCssVersion)][prop div 32] and (1 shl (prop mod 32)) = 0) then
        Result := stkCssValUndef;
    end;
  end else
    Result := stkCssValUndef;
  if Result = stkCssValUndef then
  begin
    prop := CssGetProp - 1;
    if (prop <> -1) and (TSynWeb_CssPropsData[prop] and (1 shl 20) <> 0) then
      Result := stkCssSymbol;
  end;
end;

function TSynWebEngine.CssValUndef: TSynWebTokenKind;
begin
  Result:=stkCssValUndef;
end;

function TSynWebEngine.CssValFunc26: TSynWebTokenKind;
begin
  if  CssValKeyComp(59) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc27: TSynWebTokenKind;
begin
  if  CssValKeyComp(28) or
      CssValKeyComp(125) or
      CssValKeyComp(130) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc29: TSynWebTokenKind;
begin
  if  CssValKeyComp(12) or
      CssValKeyComp(43) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc31: TSynWebTokenKind;
begin
  if  CssValKeyComp(91) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc32: TSynWebTokenKind;
begin
  if  CssValKeyComp(60) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc33: TSynWebTokenKind;
begin
  if  CssValKeyComp(16) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc35: TSynWebTokenKind;
begin
  if  CssValKeyComp(40) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc36: TSynWebTokenKind;
begin
  if  CssValKeyComp(25) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc37: TSynWebTokenKind;
begin
  if  CssValKeyComp(112) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc38: TSynWebTokenKind;
begin
  if  CssValKeyComp(170) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc39: TSynWebTokenKind;
begin
  if  CssValKeyComp(79) or
      CssValKeyComp(119) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc40: TSynWebTokenKind;
begin
  if  CssValKeyComp(3) or
      CssValKeyComp(15) or
      CssValKeyComp(161) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc41: TSynWebTokenKind;
begin
  if  CssValKeyComp(35) or
      CssValKeyComp(57) or
      CssValKeyComp(62) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc42: TSynWebTokenKind;
begin
  if  CssValKeyComp(9) or
      CssValKeyComp(50) or
      CssValKeyComp(158) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc43: TSynWebTokenKind;
begin
  if  CssValKeyComp(14) or
      CssValKeyComp(72) or
      CssValKeyComp(74) or
      CssValKeyComp(131) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc44: TSynWebTokenKind;
begin
  if  CssValKeyComp(58) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc45: TSynWebTokenKind;
begin
  if  CssValKeyComp(0) or
      CssValKeyComp(18) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc46: TSynWebTokenKind;
begin
  if  CssValKeyComp(48) or
      CssValKeyComp(97) or
      CssValKeyComp(124) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc47: TSynWebTokenKind;
begin
  if  CssValKeyComp(36) or
      CssValKeyComp(96) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc48: TSynWebTokenKind;
begin
  if  CssValKeyComp(13) or
      CssValKeyComp(51) or
      CssValKeyComp(103) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc49: TSynWebTokenKind;
begin
  if  CssValKeyComp(55) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc50: TSynWebTokenKind;
begin
  if  CssValKeyComp(26) or
      CssValKeyComp(83) or
      CssValKeyComp(90) or
      CssValKeyComp(135) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc51: TSynWebTokenKind;
begin
  if  CssValKeyComp(7) or
      CssValKeyComp(54) or
      CssValKeyComp(174) or
      CssValKeyComp(175) or
      CssValKeyComp(176) or
      CssValKeyComp(183) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc52: TSynWebTokenKind;
begin
  if  CssValKeyComp(82) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc53: TSynWebTokenKind;
begin
  if  CssValKeyComp(94) or
      CssValKeyComp(185) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc54: TSynWebTokenKind;
begin
  if  CssValKeyComp(70) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc55: TSynWebTokenKind;
begin
  if  CssValKeyComp(61) or
      CssValKeyComp(99) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc56: TSynWebTokenKind;
begin
  if  CssValKeyComp(17) or
      CssValKeyComp(77) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc57: TSynWebTokenKind;
begin
  if  CssValKeyComp(6) or
      CssValKeyComp(10) or
      CssValKeyComp(141) or
      CssValKeyComp(147) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc59: TSynWebTokenKind;
begin
  if  CssValKeyComp(5) or
      CssValKeyComp(42) or
      CssValKeyComp(152) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc60: TSynWebTokenKind;
begin
  if  CssValKeyComp(68) or
      CssValKeyComp(114) or
      CssValKeyComp(151) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc61: TSynWebTokenKind;
begin
  if  CssValKeyComp(73) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc62: TSynWebTokenKind;
begin
  if  CssValKeyComp(100) or
      CssValKeyComp(132) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc63: TSynWebTokenKind;
begin
  if  CssValKeyComp(64) or
      CssValKeyComp(111) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc65: TSynWebTokenKind;
begin
  if  CssValKeyComp(22) or
      CssValKeyComp(93) or
      CssValKeyComp(127) or
      CssValKeyComp(142) or
      CssValKeyComp(186) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc67: TSynWebTokenKind;
begin
  if  CssValKeyComp(8) or
      CssValKeyComp(52) or
      CssValKeyComp(67) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc68: TSynWebTokenKind;
begin
  if  CssValKeyComp(39) or
      CssValKeyComp(41) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc69: TSynWebTokenKind;
begin
  if  CssValKeyComp(38) or
      CssValKeyComp(49) or
      CssValKeyComp(145) or
      CssValKeyComp(171) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc72: TSynWebTokenKind;
begin
  if  CssValKeyComp(156) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc73: TSynWebTokenKind;
begin
  if  CssValKeyComp(84) or
      CssValKeyComp(106) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc75: TSynWebTokenKind;
begin
  if  CssValKeyComp(4) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc76: TSynWebTokenKind;
begin
  if  CssValKeyComp(53) or
      CssValKeyComp(92) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc78: TSynWebTokenKind;
begin
  if  CssValKeyComp(21) or
      CssValKeyComp(184) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc79: TSynWebTokenKind;
begin
  if  CssValKeyComp(78) or
      CssValKeyComp(138) or
      CssValKeyComp(143) or
      CssValKeyComp(159) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc80: TSynWebTokenKind;
begin
  if  CssValKeyComp(150) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc81: TSynWebTokenKind;
begin
  if  CssValKeyComp(2) or
      CssValKeyComp(110) or
      CssValKeyComp(154) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc82: TSynWebTokenKind;
begin
  if  CssValKeyComp(56) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc83: TSynWebTokenKind;
begin
  if  CssValKeyComp(29) or
      CssValKeyComp(63) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc85: TSynWebTokenKind;
begin
  if  CssValKeyComp(19) or
      CssValKeyComp(139) or
      CssValKeyComp(144) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc86: TSynWebTokenKind;
begin
  if  CssValKeyComp(45) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc87: TSynWebTokenKind;
begin
  if  CssValKeyComp(107) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc88: TSynWebTokenKind;
begin
  if  CssValKeyComp(69) or
      CssValKeyComp(123) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc92: TSynWebTokenKind;
begin
  if  CssValKeyComp(126) or
      CssValKeyComp(146) or
      CssValKeyComp(198) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc93: TSynWebTokenKind;
begin
  if  CssValKeyComp(116) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc94: TSynWebTokenKind;
begin
  if  CssValKeyComp(189) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc95: TSynWebTokenKind;
begin
  if  CssValKeyComp(1) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc96: TSynWebTokenKind;
begin
  if  CssValKeyComp(31) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc97: TSynWebTokenKind;
begin
  if  CssValKeyComp(34) or
      CssValKeyComp(118) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc100: TSynWebTokenKind;
begin
  if  CssValKeyComp(115) or
      CssValKeyComp(117) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc101: TSynWebTokenKind;
begin
  if  CssValKeyComp(86) or
      CssValKeyComp(98) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc102: TSynWebTokenKind;
begin
  if  CssValKeyComp(20) or
      CssValKeyComp(178) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc104: TSynWebTokenKind;
begin
  if  CssValKeyComp(180) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc105: TSynWebTokenKind;
begin
  if  CssValKeyComp(190) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc106: TSynWebTokenKind;
begin
  if  CssValKeyComp(46) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc108: TSynWebTokenKind;
begin
  if  CssValKeyComp(76) or
      CssValKeyComp(188) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc110: TSynWebTokenKind;
begin
  if  CssValKeyComp(33) or
      CssValKeyComp(71) or
      CssValKeyComp(163) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc112: TSynWebTokenKind;
begin
  if  CssValKeyComp(192) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc114: TSynWebTokenKind;
begin
  if  CssValKeyComp(136) or
      CssValKeyComp(191) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc115: TSynWebTokenKind;
begin
  if  CssValKeyComp(32) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc117: TSynWebTokenKind;
begin
  if  CssValKeyComp(120) or
      CssValKeyComp(122) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc118: TSynWebTokenKind;
begin
  if  CssValKeyComp(75) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc119: TSynWebTokenKind;
begin
  if  CssValKeyComp(194) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc122: TSynWebTokenKind;
begin
  if  CssValKeyComp(195) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc125: TSynWebTokenKind;
begin
  if  CssValKeyComp(44) or
      CssValKeyComp(47) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc127: TSynWebTokenKind;
begin
  if  CssValKeyComp(128) or
      CssValKeyComp(134) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc128: TSynWebTokenKind;
begin
  if  CssValKeyComp(129) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc129: TSynWebTokenKind;
begin
  if  CssValKeyComp(196) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc131: TSynWebTokenKind;
begin
  if  CssValKeyComp(193) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc132: TSynWebTokenKind;
begin
  if  CssValKeyComp(105) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc134: TSynWebTokenKind;
begin
  if  CssValKeyComp(108) or
      CssValKeyComp(148) or
      CssValKeyComp(168) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc135: TSynWebTokenKind;
begin
  if  CssValKeyComp(121) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc137: TSynWebTokenKind;
begin
  if  CssValKeyComp(133) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc139: TSynWebTokenKind;
begin
  if  CssValKeyComp(101) or
      CssValKeyComp(155) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc141: TSynWebTokenKind;
begin
  if  CssValKeyComp(66) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc143: TSynWebTokenKind;
begin
  if  CssValKeyComp(187) or
      CssValKeyComp(197) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc144: TSynWebTokenKind;
begin
  if  CssValKeyComp(65) or
      CssValKeyComp(140) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc145: TSynWebTokenKind;
begin
  if  CssValKeyComp(81) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc146: TSynWebTokenKind;
begin
  if  CssValKeyComp(23) or
      CssValKeyComp(177) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc148: TSynWebTokenKind;
begin
  if  CssValKeyComp(95) or
      CssValKeyComp(137) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc149: TSynWebTokenKind;
begin
  if  CssValKeyComp(85) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc151: TSynWebTokenKind;
begin
  if  CssValKeyComp(30) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc152: TSynWebTokenKind;
begin
  if  CssValKeyComp(179) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc156: TSynWebTokenKind;
begin
  if  CssValKeyComp(162) or
      CssValKeyComp(164) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc157: TSynWebTokenKind;
begin
  if  CssValKeyComp(87) or
      CssValKeyComp(109) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc158: TSynWebTokenKind;
begin
  if  CssValKeyComp(11) or
      CssValKeyComp(153) or
      CssValKeyComp(173) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc159: TSynWebTokenKind;
begin
  if  CssValKeyComp(157) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc162: TSynWebTokenKind;
begin
  if  CssValKeyComp(160) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc165: TSynWebTokenKind;
begin
  if  CssValKeyComp(24) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc166: TSynWebTokenKind;
begin
  if  CssValKeyComp(113) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc167: TSynWebTokenKind;
begin
  if  CssValKeyComp(88) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc170: TSynWebTokenKind;
begin
  if  CssValKeyComp(27) or
      CssValKeyComp(181) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc172: TSynWebTokenKind;
begin
  if  CssValKeyComp(89) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc173: TSynWebTokenKind;
begin
  if  CssValKeyComp(149) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc175: TSynWebTokenKind;
begin
  if  CssValKeyComp(80) or
      CssValKeyComp(182) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc192: TSynWebTokenKind;
begin
  if  CssValKeyComp(172) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc233: TSynWebTokenKind;
begin
  if  CssValKeyComp(104) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc234: TSynWebTokenKind;
begin
  if  CssValKeyComp(167) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc237: TSynWebTokenKind;
begin
  if  CssValKeyComp(102) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc239: TSynWebTokenKind;
begin
  if  CssValKeyComp(37) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc249: TSynWebTokenKind;
begin
  if  CssValKeyComp(169) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc271: TSynWebTokenKind;
begin
  if  CssValKeyComp(165) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssValFunc272: TSynWebTokenKind;
begin
  if  CssValKeyComp(166) then
    Result := stkCssVal
  else
    Result := stkCssValUndef;
end;

function TSynWebEngine.CssSpecialKeyComp(const ID: Integer): Boolean;
var
  I: Integer;
  Temp: PChar;
  aKey: String;
begin
  aKey := TSynWeb_CssSpecial[ID];
  Temp := FInstance^.FToIdent;
  if Length(aKey) = FInstance^.FStringLen then
  begin
    for i := 1 to FInstance^.FStringLen do
    begin
      if TSynWebInsensitiveHashTable[Temp^] <> TSynWebInsensitiveHashTable[aKey[i]] then
      begin
        Result := False;
        Exit;
      end;
      Inc(Temp);
    end;
    FInstance^.FTokenLastID := ID;
    Result := True;
  end else
    Result := False;
end;

function TSynWebEngine.CssSpecialCheck(AStart, ALen: Integer): Integer;
var
  HashKey: Longword;

  procedure KeyHash(ToHash: PChar);
  var
    i: Integer;
  begin
    HashKey := 0;
    FInstance^.FStringLen := ALen;
    for i := 0 to ALen - 1 do
    begin
      Inc(HashKey, TSynWebIdentHashTable[ToHash^]);
      Inc(ToHash);
    end;
  end;

begin
  FInstance^.FToIdent := @FInstance^.FLine[AStart];
  KeyHash(FInstance^.FToIdent);
  if (HashKey > CssSpecialMaxKeyHash) or not FCssSpecialIdentFuncTable[HashKey] then
    FInstance^.FTokenLastID := -1;
  Result := FInstance^.FTokenLastID;
end;

function TSynWebEngine.CssSpecialUndef: Boolean;
begin
  Result:=False;
end;

function TSynWebEngine.CssSpecialFunc16: Boolean;
begin
  if  CssSpecialKeyComp(7) or
      CssSpecialKeyComp(8) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc18: Boolean;
begin
  if  CssSpecialKeyComp(9) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc19: Boolean;
begin
  if  CssSpecialKeyComp(33) or
      CssSpecialKeyComp(40) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc23: Boolean;
begin
  if  CssSpecialKeyComp(24) or
      CssSpecialKeyComp(38) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc25: Boolean;
begin
  if  CssSpecialKeyComp(2) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc26: Boolean;
begin
  if  CssSpecialKeyComp(30) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc29: Boolean;
begin
  if  CssSpecialKeyComp(11) or
      CssSpecialKeyComp(32) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc30: Boolean;
begin
  if  CssSpecialKeyComp(18) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc32: Boolean;
begin
  if  CssSpecialKeyComp(29) or
      CssSpecialKeyComp(31) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc34: Boolean;
begin
  if  CssSpecialKeyComp(21) or
      CssSpecialKeyComp(26) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc36: Boolean;
begin
  if  CssSpecialKeyComp(36) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc40: Boolean;
begin
  if  CssSpecialKeyComp(37) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc42: Boolean;
begin
  if  CssSpecialKeyComp(44) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc43: Boolean;
begin
  if  CssSpecialKeyComp(27) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc45: Boolean;
begin
  if  CssSpecialKeyComp(25) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc46: Boolean;
begin
  if  CssSpecialKeyComp(28) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc50: Boolean;
begin
  if  CssSpecialKeyComp(1) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc51: Boolean;
begin
  if  CssSpecialKeyComp(4) or
      CssSpecialKeyComp(45) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc56: Boolean;
begin
  if  CssSpecialKeyComp(19) or
      CssSpecialKeyComp(42) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc57: Boolean;
begin
  if  CssSpecialKeyComp(3) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc59: Boolean;
begin
  if  CssSpecialKeyComp(5) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc60: Boolean;
begin
  if  CssSpecialKeyComp(0) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc62: Boolean;
begin
  if  CssSpecialKeyComp(39) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc64: Boolean;
begin
  if  CssSpecialKeyComp(16) or
      CssSpecialKeyComp(41) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc65: Boolean;
begin
  if  CssSpecialKeyComp(43) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc68: Boolean;
begin
  if  CssSpecialKeyComp(20) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc72: Boolean;
begin
  if  CssSpecialKeyComp(12) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc74: Boolean;
begin
  if  CssSpecialKeyComp(6) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc77: Boolean;
begin
  if  CssSpecialKeyComp(34) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc82: Boolean;
begin
  if  CssSpecialKeyComp(10) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc88: Boolean;
begin
  if  CssSpecialKeyComp(46) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc91: Boolean;
begin
  if  CssSpecialKeyComp(22) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc108: Boolean;
begin
  if  CssSpecialKeyComp(17) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc125: Boolean;
begin
  if  CssSpecialKeyComp(35) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc126: Boolean;
begin
  if  CssSpecialKeyComp(23) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc146: Boolean;
begin
  if  CssSpecialKeyComp(13) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc150: Boolean;
begin
  if  CssSpecialKeyComp(15) then
    Result := True
  else
    Result := False;
end;

function TSynWebEngine.CssSpecialFunc190: Boolean;
begin
  if  CssSpecialKeyComp(14) then
    Result := True
  else
    Result := False;
end;

// ECMAScript ------------------------------------------------------------------

procedure TSynWebEngine.EsMakeMethodTables;
var
  c: char;
  i: Integer;
  pF: PSynWebIdentFuncTableFunc;
begin
  for c := #0 to #255 do
    case c of
    #0:
      FEsProcTable[c] := NullProc;
    #1..#32:
      FEsProcTable[c] := EsSpaceProc;
    '/':
      FEsProcTable[c] := EsSlashProc;
    '<':
      FEsProcTable[c] := EsLowerProc;
    '=', '!':
      FEsProcTable[c] := EsEqualNotProc;
    '>':
      FEsProcTable[c] := EsGreaterProc;
    '&':
      FEsProcTable[c] := EsAndProc;
    '+':
      FEsProcTable[c] := EsPlusProc;
    '-':
      FEsProcTable[c] := EsMinusProc;
    '|':
      FEsProcTable[c] := EsOrProc;
    '*', '%', '^':
      FEsProcTable[c] := EsMulModXorProc;
    '0'..'9':
      FEsProcTable[c] := EsNumberProc;
    '"':
      FEsProcTable[c] := EsString34Proc;
    #39:
      FEsProcTable[c] := EsString39Proc;
    '{', '}', '[', ']', '(', ')', '.', ';', ',', '?', ':', '~':
      FEsProcTable[c] :=
        EsSymbolProc;
    '$', 'a'..'z', 'A'..'Z', '_':
      FEsProcTable[c] := EsIdentProc;
    else // case
      FEsProcTable[c] := EsErrorProc;
    end;

  FEsRangeProcTable[srsEsDefault] := EsRangeDefaultProc;
  FEsRangeProcTable[srsEsComment] := EsRangeCommentProc;
  FEsRangeProcTable[srsEsCommentMulti] := EsRangeCommentMultiProc;
  FEsRangeProcTable[srsEsString34] := EsRangeString34Proc;
  FEsRangeProcTable[srsEsString39] := EsRangeString39Proc;

  pF := PSynWebIdentFuncTableFunc(@FEsIdentFuncTable);
  for I := Low(FEsIdentFuncTable) to High(FEsIdentFuncTable) do
  begin
    pF^ := EsKeywordIdent;
    Inc(pF);
  end;
  FEsIdentFuncTable[15]:=EsKeywordFunc15;
  FEsIdentFuncTable[19]:=EsKeywordFunc19;
  FEsIdentFuncTable[23]:=EsKeywordFunc23;
  FEsIdentFuncTable[28]:=EsKeywordFunc28;
  FEsIdentFuncTable[30]:=EsKeywordFunc30;
  FEsIdentFuncTable[35]:=EsKeywordFunc35;
  FEsIdentFuncTable[37]:=EsKeywordFunc37;
  FEsIdentFuncTable[39]:=EsKeywordFunc39;
  FEsIdentFuncTable[41]:=EsKeywordFunc41;
  FEsIdentFuncTable[42]:=EsKeywordFunc42;
  FEsIdentFuncTable[43]:=EsKeywordFunc43;
  FEsIdentFuncTable[44]:=EsKeywordFunc44;
  FEsIdentFuncTable[48]:=EsKeywordFunc48;
  FEsIdentFuncTable[50]:=EsKeywordFunc50;
  FEsIdentFuncTable[51]:=EsKeywordFunc51;
  FEsIdentFuncTable[52]:=EsKeywordFunc52;
  FEsIdentFuncTable[53]:=EsKeywordFunc53;
  FEsIdentFuncTable[54]:=EsKeywordFunc54;
  FEsIdentFuncTable[56]:=EsKeywordFunc56;
  FEsIdentFuncTable[57]:=EsKeywordFunc57;
  FEsIdentFuncTable[59]:=EsKeywordFunc59;
  FEsIdentFuncTable[60]:=EsKeywordFunc60;
  FEsIdentFuncTable[63]:=EsKeywordFunc63;
  FEsIdentFuncTable[64]:=EsKeywordFunc64;
  FEsIdentFuncTable[69]:=EsKeywordFunc69;
  FEsIdentFuncTable[71]:=EsKeywordFunc71;
  FEsIdentFuncTable[72]:=EsKeywordFunc72;
  FEsIdentFuncTable[79]:=EsKeywordFunc79;
  FEsIdentFuncTable[80]:=EsKeywordFunc80;
  FEsIdentFuncTable[81]:=EsKeywordFunc81;
  FEsIdentFuncTable[82]:=EsKeywordFunc82;
  FEsIdentFuncTable[84]:=EsKeywordFunc84;
  FEsIdentFuncTable[87]:=EsKeywordFunc87;
  FEsIdentFuncTable[91]:=EsKeywordFunc91;
  FEsIdentFuncTable[96]:=EsKeywordFunc96;
  FEsIdentFuncTable[98]:=EsKeywordFunc98;
  FEsIdentFuncTable[101]:=EsKeywordFunc101;
  FEsIdentFuncTable[102]:=EsKeywordFunc102;
  FEsIdentFuncTable[103]:=EsKeywordFunc103;
  FEsIdentFuncTable[106]:=EsKeywordFunc106;
  FEsIdentFuncTable[120]:=EsKeywordFunc120;
  FEsIdentFuncTable[126]:=EsKeywordFunc126;
  FEsIdentFuncTable[160]:=EsKeywordFunc160;
end;

procedure TSynWebEngine.EsNext;
begin
  FInstance^.FTokenPos := FInstance^.FRun;
  FEsRangeProcTable[EsGetRange];
end;

function TSynWebEngine.EsGetRange: TSynWebEsRangeState;
begin
  Result := TSynWebEsRangeState(GetRangeInt(3, 14));
end;

procedure TSynWebEngine.EsSetRange(const ARange: TSynWebEsRangeState);
begin
  SetRangeInt(3, 14, Longword(ARange));
end;

function TSynWebEngine.EsCheckNull(ADo: Boolean = True): Boolean;
begin
  case FInstance^.FLine[FInstance^.FRun] of
  #0:
    begin
      Result := True;
      if ADo then
        NullProc;
    end;
  '<':
    if (FInstance^.FLine[FInstance^.FRun + 1] = '/') and
      (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 2]] =
      FInstance^.FHashTable['s']) and
      (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 3]] =
      FInstance^.FHashTable['c']) and
      (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 4]] =
      FInstance^.FHashTable['r']) and
      (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 5]] =
      FInstance^.FHashTable['i']) and
      (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 6]] =
      FInstance^.FHashTable['p']) and
      (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 7]] =
      FInstance^.FHashTable['t']) and
      (TSynWebIdentTable2[FInstance^.FLine[FInstance^.FRun + 8]] and (1 shl 0) <> 0) and                    
      // (FInstance^.FLine[FInstance^.FRun+8] in [#0..#32, '>']) and
      (FInstance^.FHighlighterMode = shmML) then
    begin
      Result := True;
      if ADo then
      begin
        SetHighlighterType(shtML, True, False, False);
        Next;
      end;
    end else
      Result := False;
  else // case
    Result := False;
  end;
end;

procedure TSynWebEngine.EsSpaceProc;
begin
  repeat
    Inc(FInstance^.FRun);
  until not (FInstance^.FLine[FInstance^.FRun] in [#1..#32]);
  FInstance^.FTokenID := stkEsSpace;
end;

procedure TSynWebEngine.EsSlashProc;
begin
  Inc(FInstance^.FRun);
  case FInstance^.FLine[FInstance^.FRun] of
  '*':
    begin
      Inc(FInstance^.FRun);
      EsSetRange(srsEsCommentMulti);
      if EsCheckNull(False) or PhpCheckBegin(False) then
        FInstance^.FTokenID := stkEsComment
      else
        EsRangeCommentMultiProc;
      Exit;
    end;
  '=':
    Inc(FInstance^.FRun);
  '/':
    begin
      Inc(FInstance^.FRun);
      EsSetRange(srsEsComment);
      if EsCheckNull(False) or PhpCheckBegin(False) then
        FInstance^.FTokenID := stkEsComment
      else
        EsRangeCommentProc;
      Exit;
    end;
  end;
  FInstance^.FTokenID := stkEsSymbol;
end;

procedure TSynWebEngine.EsLowerProc;
begin
  if EsCheckNull or PhpCheckBegin then
    Exit;
  Inc(FInstance^.FRun);
  case FInstance^.FLine[FInstance^.FRun] of
  '=':
    Inc(FInstance^.FRun);
  '<':
    begin
      Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] = '=' then
        Inc(FInstance^.FRun);
    end;
  end;
  FInstance^.FTokenID := stkEsSymbol;
end;

procedure TSynWebEngine.EsEqualNotProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = '=' then
  begin
    Inc(FInstance^.FRun);
    if FInstance^.FLine[FInstance^.FRun] = '=' then
      Inc(FInstance^.FRun);
  end;
  FInstance^.FTokenID := stkEsSymbol;
end;

procedure TSynWebEngine.EsGreaterProc;
begin
  Inc(FInstance^.FRun);
  case FInstance^.FLine[FInstance^.FRun] of
  '=':
    Inc(FInstance^.FRun);
  '>':
    begin
      Inc(FInstance^.FRun);
      case FInstance^.FLine[FInstance^.FRun] of
      '=':
        Inc(FInstance^.FRun);
      '>':
        begin
          Inc(FInstance^.FRun);
          if FInstance^.FLine[FInstance^.FRun] = '=' then
            Inc(FInstance^.FRun);
        end;
      end;
    end;
  end;
  FInstance^.FTokenID := stkEsSymbol;
end;

procedure TSynWebEngine.EsAndProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] in ['=', '&'] then
    Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkEsSymbol;
end;

procedure TSynWebEngine.EsPlusProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] in ['=', '+'] then
    Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkEsSymbol;
end;

procedure TSynWebEngine.EsMinusProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] in ['=', '-'] then
    Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkEsSymbol;
end;

procedure TSynWebEngine.EsOrProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] in ['=', '|'] then
    Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkEsSymbol;
end;

procedure TSynWebEngine.EsMulModXorProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = '=' then
    Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkEsSymbol;
end;

procedure TSynWebEngine.EsNumberProc;
begin
  FInstance^.FTokenID := stkEsError;
  if (FInstance^.FLine[FInstance^.FRun] = '0') and
    (FInstance^.FLine[FInstance^.FRun + 1] in ['x', 'X']) then
  begin
    Inc(FInstance^.FRun, 2);
    // if FInstance^.FLine[FInstance^.FRun] in ['a'..'f', 'A'..'F', '0'..'9'] then
    if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 10) <> 0 then
      repeat
        Inc(FInstance^.FRun);
      until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 10) = 0
      // until not (FInstance^.FLine[FInstance^.FRun] in ['a'..'f', 'A'..'F', '0'..'9'])
    else
      Exit;
  end else
  begin
    while FInstance^.FLine[FInstance^.FRun] in ['0'..'9'] do
      Inc(FInstance^.FRun);
    if FInstance^.FLine[FInstance^.FRun] = '.' then
    begin
      Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] in ['0'..'9'] then
        repeat
          Inc(FInstance^.FRun);
        until not (FInstance^.FLine[FInstance^.FRun] in ['0'..'9'])
      else
        Exit;
    end;
    if FInstance^.FLine[FInstance^.FRun] in ['e', 'E'] then
    begin
      Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] in ['+', '-'] then
        Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] in ['0'..'9'] then
        repeat
          Inc(FInstance^.FRun);
        until not (FInstance^.FLine[FInstance^.FRun] in ['0'..'9'])
      else
        Exit;
    end;
  end;
  FInstance^.FTokenID := stkEsNumber;
end;

procedure TSynWebEngine.EsString34Proc;
begin
  Inc(FInstance^.FRun);
  if EsCheckNull(False) then
    FInstance^.FTokenID := stkEsError
  else
  begin
    EsSetRange(srsEsString34);
    if PhpCheckBegin(False) then
      FInstance^.FTokenID := stkEsString
    else
      EsRangeString34Proc;
  end;
end;

procedure TSynWebEngine.EsString39Proc;
begin
  Inc(FInstance^.FRun);
  if EsCheckNull(False) then
    FInstance^.FTokenID := stkEsError
  else
  begin
    EsSetRange(srsEsString39);
    if PhpCheckBegin(False) then
      FInstance^.FTokenID := stkEsString
    else
      EsRangeString39Proc;
  end;
end;

procedure TSynWebEngine.EsSymbolProc;
begin
  Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkEsSymbol;
end;

procedure TSynWebEngine.EsIdentProc;
begin
  repeat
    Inc(FInstance^.FRun);
  until TSynWebIdentTable2[FInstance^.FLine[FInstance^.FRun]] and (1 shl 2) = 0;
  // until not(FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '_', '0'..'9', '$']);
  FInstance^.FTokenID := EsIdentCheck;
end;

procedure TSynWebEngine.EsErrorProc;
begin
  Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkEsError;
end;

procedure TSynWebEngine.EsRangeDefaultProc;
begin
  FEsProcTable[FInstance^.FLine[FInstance^.FRun]];
end;

procedure TSynWebEngine.EsRangeCommentProc;
begin
  if not EsCheckNull then
    if PhpCheckBegin then
      Exit
    else
      repeat
        repeat
          Inc(FInstance^.FRun);
        until FInstance^.FLine[FInstance^.FRun] in [#0, '<'];
        case FInstance^.FLine[FInstance^.FRun] of
        #0:
          begin
            FInstance^.FTokenID := stkEsComment;
            Break;
          end;
        '<':
          if PhpCheckBegin(False) then
          begin
            FInstance^.FTokenID := stkEsComment;
            Exit;
          end else
            if EsCheckNull(False) then
            begin
              FInstance^.FTokenID := stkEsComment;
              Break;
            end else
            begin
              Inc(FInstance^.FRun);
              if FInstance^.FLine[FInstance^.FRun] = #0 then
              begin
                FInstance^.FTokenID := stkEsComment;
                Break;
              end;
            end;
        end;
      until False;
  EsSetRange(srsEsDefault);
end;

procedure TSynWebEngine.EsRangeCommentMultiProc;
begin
  if EsCheckNull or PhpCheckBegin then
    Exit;
  repeat
    // while not (FInstance^.FLine[FInstance^.FRun] in [#0, '*', '<']) do
    while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 26) = 0 do
      Inc(FInstance^.FRun);
    case FInstance^.FLine[FInstance^.FRun] of
    #0:
      Break;
    '<':
      if EsCheckNull(False) or PhpCheckBegin(False) then
        Break
      else
        Inc(FInstance^.FRun);
    '*':
      begin
        Inc(FInstance^.FRun);
        if FInstance^.FLine[FInstance^.FRun] = '/' then
        begin
          Inc(FInstance^.FRun);
          EsSetRange(srsEsDefault);
          Break;
        end;
      end;
    end;
  until False;
  FInstance^.FTokenID := stkEsComment;
end;

procedure TSynWebEngine.EsRangeString34Proc;
begin
  if not MLCheckNull then
    if PhpCheckBegin then
      Exit
    else
      repeat
        // while not (FInstance^.FLine[FInstance^.FRun] in [#0, #34, '<', '\']) do
        while TSynWebIdentTable2[FInstance^.FLine[FInstance^.FRun]] and (1 shl 3) = 0 do
          Inc(FInstance^.FRun);
        case FInstance^.FLine[FInstance^.FRun] of
        #0:
          begin
            FInstance^.FTokenID := stkEsError;
            Break;
          end;
        '<':
          if PhpCheckBegin(False) then
          begin
            FInstance^.FTokenID := stkEsString;
            Exit;
          end else
            Inc(FInstance^.FRun);
        #34:
          begin
            Inc(FInstance^.FRun);
            FInstance^.FTokenID := stkEsString;
            Break;
          end;
        '\':
          begin
            Inc(FInstance^.FRun);
            if FInstance^.FLine[FInstance^.FRun] = #34 then
              Inc(FInstance^.FRun);
          end;
        end;
      until False;
  EsSetRange(srsEsDefault);
end;

procedure TSynWebEngine.EsRangeString39Proc;
begin
  if not MLCheckNull then
    if PhpCheckBegin then
      Exit
    else
      repeat
        // while not (FInstance^.FLine[FInstance^.FRun] in [#0, #39, '<', '\']) do
        while TSynWebIdentTable2[FInstance^.FLine[FInstance^.FRun]] and (1 shl 4) = 0 do
          Inc(FInstance^.FRun);
        case FInstance^.FLine[FInstance^.FRun] of
        #0:
          begin
            FInstance^.FTokenID := stkEsError;
            Break;
          end;
        '<':
            if PhpCheckBegin(False) then
            begin
              FInstance^.FTokenID := stkEsString;
              Exit;
            end else
              Inc(FInstance^.FRun);
        #39:
          begin
            Inc(FInstance^.FRun);
            FInstance^.FTokenID := stkEsString;
            Break;
          end;
        '\':
          begin
            Inc(FInstance^.FRun);
            if FInstance^.FLine[FInstance^.FRun] = #39 then
              Inc(FInstance^.FRun);
          end;
        end;
      until False;
  EsSetRange(srsEsDefault);
end;

function TSynWebEngine.EsKeywordComp(const ID: Integer): Boolean;
var
  I: Integer;
  Temp: PChar;
  aKey: String;
begin
  aKey := TSynWeb_EsKeywords[ID];
  Temp := FInstance^.FToIdent;
  if Length(aKey) = FInstance^.FStringLen then
  begin
    for i := 1 to FInstance^.FStringLen do
    begin
      if TSynWebInsensitiveHashTable[Temp^] <> TSynWebInsensitiveHashTable[aKey[i]] then
      begin
        Result := False;
        Exit;
      end;
      Inc(Temp);
    end;
    FInstance^.FTokenLastID := ID;
    Result := True;
  end else
    Result := False;
end;

function TSynWebEngine.EsIdentCheck: TSynWebTokenKind;
var
  HashKey: Longword;

  procedure KeyHash(ToHash: PChar);
  var
    i: Integer;
  begin
    HashKey := 0;
    FInstance^.FStringLen := FInstance^.FRun - FInstance^.FTokenPos;
    for i := 0 to FInstance^.FStringLen - 1 do
    begin
      Inc(HashKey, TSynWebIdentHashTable[ToHash^]);
      Inc(ToHash);
    end;
  end;

begin
  FInstance^.FToIdent := @FInstance^.FLine[FInstance^.FTokenPos];
  KeyHash(FInstance^.FToIdent);
  FInstance^.FTokenLastID := -1;
  if HashKey <= EsKeywordsMaxKeyHash then
    Result := FEsIdentFuncTable[HashKey]
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordIdent: TSynWebTokenKind;
begin
  Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc15: TSynWebTokenKind;
begin
  if  EsKeywordComp(12) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc19: TSynWebTokenKind;
begin
  if  EsKeywordComp(6) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc23: TSynWebTokenKind;
begin
  if  EsKeywordComp(13) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc28: TSynWebTokenKind;
begin
  if  EsKeywordComp(1) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc30: TSynWebTokenKind;
begin
  if  EsKeywordComp(30) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc35: TSynWebTokenKind;
begin
  if  EsKeywordComp(2) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc37: TSynWebTokenKind;
begin
  if  EsKeywordComp(0) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc39: TSynWebTokenKind;
begin
  if  EsKeywordComp(10) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc41: TSynWebTokenKind;
begin
  if  EsKeywordComp(7) or
      EsKeywordComp(23) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc42: TSynWebTokenKind;
begin
  if  EsKeywordComp(15) then
    Result := stkEsKeyword
  else
    if  EsKeywordComp(38) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc43: TSynWebTokenKind;
begin
  if  EsKeywordComp(8) then
    Result := stkEsKeyword
  else
    if  EsKeywordComp(43) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc44: TSynWebTokenKind;
begin
  if  EsKeywordComp(47) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc48: TSynWebTokenKind;
begin
  if  EsKeywordComp(45) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc50: TSynWebTokenKind;
begin
  if  EsKeywordComp(24) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc51: TSynWebTokenKind;
begin
  if  EsKeywordComp(5) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc52: TSynWebTokenKind;
begin
  if  EsKeywordComp(29) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc53: TSynWebTokenKind;
begin
  if  EsKeywordComp(35) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc54: TSynWebTokenKind;
begin
  if  EsKeywordComp(31) or
      EsKeywordComp(39) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc56: TSynWebTokenKind;
begin
  if  EsKeywordComp(18) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc57: TSynWebTokenKind;
begin
  if  EsKeywordComp(25) then
    Result := stkEsKeyword
  else
    if  EsKeywordComp(40) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc59: TSynWebTokenKind;
begin
  if  EsKeywordComp(34) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc60: TSynWebTokenKind;
begin
  if  EsKeywordComp(26) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc63: TSynWebTokenKind;
begin
  if  EsKeywordComp(21) then
    Result := stkEsKeyword
  else
    if  EsKeywordComp(50) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc64: TSynWebTokenKind;
begin
  if  EsKeywordComp(20) then
    Result := stkEsKeyword
  else
    if  EsKeywordComp(28) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc69: TSynWebTokenKind;
begin
  if  EsKeywordComp(4) then
    Result := stkEsKeyword
  else
    if  EsKeywordComp(33) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc71: TSynWebTokenKind;
begin
  if  EsKeywordComp(32) or
      EsKeywordComp(46) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc72: TSynWebTokenKind;
begin
  if  EsKeywordComp(52) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc79: TSynWebTokenKind;
begin
  if  EsKeywordComp(9) then
    Result := stkEsKeyword
  else
    if  EsKeywordComp(53) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc80: TSynWebTokenKind;
begin
  if  EsKeywordComp(51) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc81: TSynWebTokenKind;
begin
  if  EsKeywordComp(44) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc82: TSynWebTokenKind;
begin
  if  EsKeywordComp(17) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc84: TSynWebTokenKind;
begin
  if  EsKeywordComp(19) then
    Result := stkEsKeyword
  else
    if  EsKeywordComp(27) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc87: TSynWebTokenKind;
begin
  if  EsKeywordComp(22) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc91: TSynWebTokenKind;
begin
  if  EsKeywordComp(37) or
      EsKeywordComp(42) or
      EsKeywordComp(48) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc96: TSynWebTokenKind;
begin
  if  EsKeywordComp(16) then
    Result := stkEsKeyword
  else
    if  EsKeywordComp(57) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc98: TSynWebTokenKind;
begin
  if  EsKeywordComp(36) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc101: TSynWebTokenKind;
begin
  if  EsKeywordComp(3) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc102: TSynWebTokenKind;
begin
  if  EsKeywordComp(11) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc103: TSynWebTokenKind;
begin
  if  EsKeywordComp(55) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc106: TSynWebTokenKind;
begin
  if  EsKeywordComp(14) then
    Result := stkEsKeyword
  else
    if  EsKeywordComp(49) then
      Result := stkEsKeyword
    else
      Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc120: TSynWebTokenKind;
begin
  if  EsKeywordComp(56) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc126: TSynWebTokenKind;
begin
  if  EsKeywordComp(41) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

function TSynWebEngine.EsKeywordFunc160: TSynWebTokenKind;
begin
  if  EsKeywordComp(54) then
    Result := stkEsKeyword
  else
    Result := stkEsIdentifier;
end;

// Php -------------------------------------------------------------------------

procedure TSynWebEngine.PhpMakeMethodTables;
var
  c: char;
  i: Integer;
  pF: PSynWebIdentFuncTableFunc;
begin
  for c := #0 to #255 do
    case c of
    #0:
      FPhpProcTable[c] := NullProc;
    #1..#32:
      FPhpProcTable[c] := PhpSpaceProc;
    '?':
      FPhpProcTable[c] := PhpQuestionProc;
    '0'..'9':
      FPhpProcTable[c] := PhpNumberProc;
    '"':
      FPhpProcTable[c] := PhpString34Proc;
    #39:
      FPhpProcTable[c] := PhpString39Proc;
    '`':
      FPhpProcTable[c] := PhpStringShellProc;
    '&':
      FPhpProcTable[c] := PhpAndProc;
    '|':
      FPhpProcTable[c] := PhpOrProc;
    '@':
      FPhpProcTable[c] := PhpAtSymbolProc;
    '=':
      FPhpProcTable[c] := PhpEqualProc;
    '>':
      FPhpProcTable[c] := PhpGreaterProc;
    '<':
      FPhpProcTable[c] := PhpLowerProc;
    '+':
      FPhpProcTable[c] := PhpPlusProc;
    '-':
      FPhpProcTable[c] := PhpMinusProc;
    '*':
      FPhpProcTable[c] := PhpMulProc;
    '^':
      FPhpProcTable[c] := PhpXorProc;
    '/':
      FPhpProcTable[c] := PhpSlashProc;
    '%':
      FPhpProcTable[c] := PhpPercentProc;
    '#':
      FPhpProcTable[c] := PhpHashProc;
    '!':
      FPhpProcTable[c] := PhpNotProc;
    '.':
      FPhpProcTable[c] := PhpDotProc;
    ':':
      FPhpProcTable[c] := PhpColonProc;
      
    '(':
      FPhpProcTable[c] := PhpParentheseOpenProc;
    ')':
      FPhpProcTable[c] := PhpParentheseCloseProc;
    '[':
      FPhpProcTable[c] := PhpBoxBracketOpenProc;
    ']':
      FPhpProcTable[c] := PhpBoxBracketCloseProc;
    '{':
      FPhpProcTable[c] := PhpBraceOpenProc;
    '}':
      FPhpProcTable[c] := PhpBraceCloseProc;
    '~':
      FPhpProcTable[c] := PhpTildeProc;
    ',':
      FPhpProcTable[c] := PhpCommaProc;
    ';':
      FPhpProcTable[c] := PhpSemiColonProc;
    '$':
      FPhpProcTable[c] := PhpVarProc;
    'a'..'z', 'A'..'Z', '_', #$7F..#$FF:
      FPhpProcTable[c] := PhpIdentProc;
    else // case
      FPhpProcTable[c] := PhpErrorProc;
    end;

  FPhpRangeProcTable[srsPhpSubProc] := PhpSubProcProc;
  FPhpRangeProcTable[srsPhpDefault] := PhpRangeDefaultProc;
  FPhpRangeProcTable[srsPhpComment] := PhpRangeCommentProc;
  FPhpRangeProcTable[srsPhpDocComment] := PhpRangeDocCommentProc;
  FPhpRangeProcTable[srsPhpString34] := PhpRangeString34Proc;
  FPhpRangeProcTable[srsPhpString39] := PhpRangeString39Proc;
  FPhpRangeProcTable[srsPhpStringShell] := PhpRangeStringShellProc;
  FPhpRangeProcTable[srsPhpHeredoc] := PhpRangeHeredocProc;

  pF := PSynWebIdentFuncTableFunc(@FPhpIdentFuncTable);
  for I := Low(FPhpIdentFuncTable) to High(FPhpIdentFuncTable) do
  begin
    pF^ := PhpKeywordIdent;
    Inc(pF);
  end;
  FPhpIdentFuncTable[14]:=PhpKeywordFunc14;
  FPhpIdentFuncTable[15]:=PhpKeywordFunc15;
  FPhpIdentFuncTable[16]:=PhpKeywordFunc16;
  FPhpIdentFuncTable[17]:=PhpKeywordFunc17;
  FPhpIdentFuncTable[18]:=PhpKeywordFunc18;
  FPhpIdentFuncTable[19]:=PhpKeywordFunc19;
  FPhpIdentFuncTable[20]:=PhpKeywordFunc20;
  FPhpIdentFuncTable[22]:=PhpKeywordFunc22;
  FPhpIdentFuncTable[23]:=PhpKeywordFunc23;
  FPhpIdentFuncTable[25]:=PhpKeywordFunc25;
  FPhpIdentFuncTable[28]:=PhpKeywordFunc28;
  FPhpIdentFuncTable[29]:=PhpKeywordFunc29;
  FPhpIdentFuncTable[30]:=PhpKeywordFunc30;
  FPhpIdentFuncTable[31]:=PhpKeywordFunc31;
  FPhpIdentFuncTable[32]:=PhpKeywordFunc32;
  FPhpIdentFuncTable[33]:=PhpKeywordFunc33;
  FPhpIdentFuncTable[34]:=PhpKeywordFunc34;
  FPhpIdentFuncTable[35]:=PhpKeywordFunc35;
  FPhpIdentFuncTable[36]:=PhpKeywordFunc36;
  FPhpIdentFuncTable[37]:=PhpKeywordFunc37;
  FPhpIdentFuncTable[38]:=PhpKeywordFunc38;
  FPhpIdentFuncTable[39]:=PhpKeywordFunc39;
  FPhpIdentFuncTable[40]:=PhpKeywordFunc40;
  FPhpIdentFuncTable[41]:=PhpKeywordFunc41;
  FPhpIdentFuncTable[42]:=PhpKeywordFunc42;
  FPhpIdentFuncTable[43]:=PhpKeywordFunc43;
  FPhpIdentFuncTable[44]:=PhpKeywordFunc44;
  FPhpIdentFuncTable[45]:=PhpKeywordFunc45;
  FPhpIdentFuncTable[46]:=PhpKeywordFunc46;
  FPhpIdentFuncTable[47]:=PhpKeywordFunc47;
  FPhpIdentFuncTable[48]:=PhpKeywordFunc48;
  FPhpIdentFuncTable[49]:=PhpKeywordFunc49;
  FPhpIdentFuncTable[50]:=PhpKeywordFunc50;
  FPhpIdentFuncTable[51]:=PhpKeywordFunc51;
  FPhpIdentFuncTable[52]:=PhpKeywordFunc52;
  FPhpIdentFuncTable[54]:=PhpKeywordFunc54;
  FPhpIdentFuncTable[55]:=PhpKeywordFunc55;
  FPhpIdentFuncTable[56]:=PhpKeywordFunc56;
  FPhpIdentFuncTable[57]:=PhpKeywordFunc57;
  FPhpIdentFuncTable[58]:=PhpKeywordFunc58;
  FPhpIdentFuncTable[59]:=PhpKeywordFunc59;
  FPhpIdentFuncTable[60]:=PhpKeywordFunc60;
  FPhpIdentFuncTable[61]:=PhpKeywordFunc61;
  FPhpIdentFuncTable[62]:=PhpKeywordFunc62;
  FPhpIdentFuncTable[63]:=PhpKeywordFunc63;
  FPhpIdentFuncTable[64]:=PhpKeywordFunc64;
  FPhpIdentFuncTable[65]:=PhpKeywordFunc65;
  FPhpIdentFuncTable[66]:=PhpKeywordFunc66;
  FPhpIdentFuncTable[67]:=PhpKeywordFunc67;
  FPhpIdentFuncTable[68]:=PhpKeywordFunc68;
  FPhpIdentFuncTable[69]:=PhpKeywordFunc69;
  FPhpIdentFuncTable[70]:=PhpKeywordFunc70;
  FPhpIdentFuncTable[71]:=PhpKeywordFunc71;
  FPhpIdentFuncTable[72]:=PhpKeywordFunc72;
  FPhpIdentFuncTable[73]:=PhpKeywordFunc73;
  FPhpIdentFuncTable[74]:=PhpKeywordFunc74;
  FPhpIdentFuncTable[75]:=PhpKeywordFunc75;
  FPhpIdentFuncTable[76]:=PhpKeywordFunc76;
  FPhpIdentFuncTable[77]:=PhpKeywordFunc77;
  FPhpIdentFuncTable[78]:=PhpKeywordFunc78;
  FPhpIdentFuncTable[79]:=PhpKeywordFunc79;
  FPhpIdentFuncTable[80]:=PhpKeywordFunc80;
  FPhpIdentFuncTable[81]:=PhpKeywordFunc81;
  FPhpIdentFuncTable[82]:=PhpKeywordFunc82;
  FPhpIdentFuncTable[83]:=PhpKeywordFunc83;
  FPhpIdentFuncTable[84]:=PhpKeywordFunc84;
  FPhpIdentFuncTable[85]:=PhpKeywordFunc85;
  FPhpIdentFuncTable[86]:=PhpKeywordFunc86;
  FPhpIdentFuncTable[87]:=PhpKeywordFunc87;
  FPhpIdentFuncTable[88]:=PhpKeywordFunc88;
  FPhpIdentFuncTable[89]:=PhpKeywordFunc89;
  FPhpIdentFuncTable[90]:=PhpKeywordFunc90;
  FPhpIdentFuncTable[91]:=PhpKeywordFunc91;
  FPhpIdentFuncTable[92]:=PhpKeywordFunc92;
  FPhpIdentFuncTable[93]:=PhpKeywordFunc93;
  FPhpIdentFuncTable[94]:=PhpKeywordFunc94;
  FPhpIdentFuncTable[95]:=PhpKeywordFunc95;
  FPhpIdentFuncTable[96]:=PhpKeywordFunc96;
  FPhpIdentFuncTable[97]:=PhpKeywordFunc97;
  FPhpIdentFuncTable[98]:=PhpKeywordFunc98;
  FPhpIdentFuncTable[99]:=PhpKeywordFunc99;
  FPhpIdentFuncTable[100]:=PhpKeywordFunc100;
  FPhpIdentFuncTable[101]:=PhpKeywordFunc101;
  FPhpIdentFuncTable[102]:=PhpKeywordFunc102;
  FPhpIdentFuncTable[103]:=PhpKeywordFunc103;
  FPhpIdentFuncTable[104]:=PhpKeywordFunc104;
  FPhpIdentFuncTable[105]:=PhpKeywordFunc105;
  FPhpIdentFuncTable[106]:=PhpKeywordFunc106;
  FPhpIdentFuncTable[107]:=PhpKeywordFunc107;
  FPhpIdentFuncTable[108]:=PhpKeywordFunc108;
  FPhpIdentFuncTable[109]:=PhpKeywordFunc109;
  FPhpIdentFuncTable[110]:=PhpKeywordFunc110;
  FPhpIdentFuncTable[111]:=PhpKeywordFunc111;
  FPhpIdentFuncTable[112]:=PhpKeywordFunc112;
  FPhpIdentFuncTable[113]:=PhpKeywordFunc113;
  FPhpIdentFuncTable[114]:=PhpKeywordFunc114;
  FPhpIdentFuncTable[115]:=PhpKeywordFunc115;
  FPhpIdentFuncTable[116]:=PhpKeywordFunc116;
  FPhpIdentFuncTable[117]:=PhpKeywordFunc117;
  FPhpIdentFuncTable[118]:=PhpKeywordFunc118;
  FPhpIdentFuncTable[119]:=PhpKeywordFunc119;
  FPhpIdentFuncTable[120]:=PhpKeywordFunc120;
  FPhpIdentFuncTable[121]:=PhpKeywordFunc121;
  FPhpIdentFuncTable[122]:=PhpKeywordFunc122;
  FPhpIdentFuncTable[123]:=PhpKeywordFunc123;
  FPhpIdentFuncTable[124]:=PhpKeywordFunc124;
  FPhpIdentFuncTable[125]:=PhpKeywordFunc125;
  FPhpIdentFuncTable[126]:=PhpKeywordFunc126;
  FPhpIdentFuncTable[127]:=PhpKeywordFunc127;
  FPhpIdentFuncTable[128]:=PhpKeywordFunc128;
  FPhpIdentFuncTable[129]:=PhpKeywordFunc129;
  FPhpIdentFuncTable[130]:=PhpKeywordFunc130;
  FPhpIdentFuncTable[131]:=PhpKeywordFunc131;
  FPhpIdentFuncTable[132]:=PhpKeywordFunc132;
  FPhpIdentFuncTable[133]:=PhpKeywordFunc133;
  FPhpIdentFuncTable[134]:=PhpKeywordFunc134;
  FPhpIdentFuncTable[135]:=PhpKeywordFunc135;
  FPhpIdentFuncTable[136]:=PhpKeywordFunc136;
  FPhpIdentFuncTable[137]:=PhpKeywordFunc137;
  FPhpIdentFuncTable[138]:=PhpKeywordFunc138;
  FPhpIdentFuncTable[139]:=PhpKeywordFunc139;
  FPhpIdentFuncTable[140]:=PhpKeywordFunc140;
  FPhpIdentFuncTable[141]:=PhpKeywordFunc141;
  FPhpIdentFuncTable[142]:=PhpKeywordFunc142;
  FPhpIdentFuncTable[143]:=PhpKeywordFunc143;
  FPhpIdentFuncTable[144]:=PhpKeywordFunc144;
  FPhpIdentFuncTable[145]:=PhpKeywordFunc145;
  FPhpIdentFuncTable[146]:=PhpKeywordFunc146;
  FPhpIdentFuncTable[147]:=PhpKeywordFunc147;
  FPhpIdentFuncTable[148]:=PhpKeywordFunc148;
  FPhpIdentFuncTable[149]:=PhpKeywordFunc149;
  FPhpIdentFuncTable[150]:=PhpKeywordFunc150;
  FPhpIdentFuncTable[151]:=PhpKeywordFunc151;
  FPhpIdentFuncTable[152]:=PhpKeywordFunc152;
  FPhpIdentFuncTable[153]:=PhpKeywordFunc153;
  FPhpIdentFuncTable[154]:=PhpKeywordFunc154;
  FPhpIdentFuncTable[155]:=PhpKeywordFunc155;
  FPhpIdentFuncTable[156]:=PhpKeywordFunc156;
  FPhpIdentFuncTable[157]:=PhpKeywordFunc157;
  FPhpIdentFuncTable[158]:=PhpKeywordFunc158;
  FPhpIdentFuncTable[159]:=PhpKeywordFunc159;
  FPhpIdentFuncTable[160]:=PhpKeywordFunc160;
  FPhpIdentFuncTable[161]:=PhpKeywordFunc161;
  FPhpIdentFuncTable[162]:=PhpKeywordFunc162;
  FPhpIdentFuncTable[163]:=PhpKeywordFunc163;
  FPhpIdentFuncTable[164]:=PhpKeywordFunc164;
  FPhpIdentFuncTable[165]:=PhpKeywordFunc165;
  FPhpIdentFuncTable[166]:=PhpKeywordFunc166;
  FPhpIdentFuncTable[167]:=PhpKeywordFunc167;
  FPhpIdentFuncTable[168]:=PhpKeywordFunc168;
  FPhpIdentFuncTable[169]:=PhpKeywordFunc169;
  FPhpIdentFuncTable[170]:=PhpKeywordFunc170;
  FPhpIdentFuncTable[171]:=PhpKeywordFunc171;
  FPhpIdentFuncTable[172]:=PhpKeywordFunc172;
  FPhpIdentFuncTable[173]:=PhpKeywordFunc173;
  FPhpIdentFuncTable[174]:=PhpKeywordFunc174;
  FPhpIdentFuncTable[175]:=PhpKeywordFunc175;
  FPhpIdentFuncTable[176]:=PhpKeywordFunc176;
  FPhpIdentFuncTable[177]:=PhpKeywordFunc177;
  FPhpIdentFuncTable[178]:=PhpKeywordFunc178;
  FPhpIdentFuncTable[179]:=PhpKeywordFunc179;
  FPhpIdentFuncTable[180]:=PhpKeywordFunc180;
  FPhpIdentFuncTable[181]:=PhpKeywordFunc181;
  FPhpIdentFuncTable[182]:=PhpKeywordFunc182;
  FPhpIdentFuncTable[183]:=PhpKeywordFunc183;
  FPhpIdentFuncTable[184]:=PhpKeywordFunc184;
  FPhpIdentFuncTable[185]:=PhpKeywordFunc185;
  FPhpIdentFuncTable[186]:=PhpKeywordFunc186;
  FPhpIdentFuncTable[187]:=PhpKeywordFunc187;
  FPhpIdentFuncTable[188]:=PhpKeywordFunc188;
  FPhpIdentFuncTable[189]:=PhpKeywordFunc189;
  FPhpIdentFuncTable[190]:=PhpKeywordFunc190;
  FPhpIdentFuncTable[191]:=PhpKeywordFunc191;
  FPhpIdentFuncTable[192]:=PhpKeywordFunc192;
  FPhpIdentFuncTable[193]:=PhpKeywordFunc193;
  FPhpIdentFuncTable[194]:=PhpKeywordFunc194;
  FPhpIdentFuncTable[195]:=PhpKeywordFunc195;
  FPhpIdentFuncTable[196]:=PhpKeywordFunc196;
  FPhpIdentFuncTable[197]:=PhpKeywordFunc197;
  FPhpIdentFuncTable[198]:=PhpKeywordFunc198;
  FPhpIdentFuncTable[199]:=PhpKeywordFunc199;
  FPhpIdentFuncTable[200]:=PhpKeywordFunc200;
  FPhpIdentFuncTable[201]:=PhpKeywordFunc201;
  FPhpIdentFuncTable[202]:=PhpKeywordFunc202;
  FPhpIdentFuncTable[203]:=PhpKeywordFunc203;
  FPhpIdentFuncTable[204]:=PhpKeywordFunc204;
  FPhpIdentFuncTable[205]:=PhpKeywordFunc205;
  FPhpIdentFuncTable[206]:=PhpKeywordFunc206;
  FPhpIdentFuncTable[207]:=PhpKeywordFunc207;
  FPhpIdentFuncTable[208]:=PhpKeywordFunc208;
  FPhpIdentFuncTable[209]:=PhpKeywordFunc209;
  FPhpIdentFuncTable[210]:=PhpKeywordFunc210;
  FPhpIdentFuncTable[211]:=PhpKeywordFunc211;
  FPhpIdentFuncTable[212]:=PhpKeywordFunc212;
  FPhpIdentFuncTable[213]:=PhpKeywordFunc213;
  FPhpIdentFuncTable[214]:=PhpKeywordFunc214;
  FPhpIdentFuncTable[215]:=PhpKeywordFunc215;
  FPhpIdentFuncTable[216]:=PhpKeywordFunc216;
  FPhpIdentFuncTable[217]:=PhpKeywordFunc217;
  FPhpIdentFuncTable[218]:=PhpKeywordFunc218;
  FPhpIdentFuncTable[219]:=PhpKeywordFunc219;
  FPhpIdentFuncTable[220]:=PhpKeywordFunc220;
  FPhpIdentFuncTable[221]:=PhpKeywordFunc221;
  FPhpIdentFuncTable[222]:=PhpKeywordFunc222;
  FPhpIdentFuncTable[223]:=PhpKeywordFunc223;
  FPhpIdentFuncTable[224]:=PhpKeywordFunc224;
  FPhpIdentFuncTable[225]:=PhpKeywordFunc225;
  FPhpIdentFuncTable[226]:=PhpKeywordFunc226;
  FPhpIdentFuncTable[227]:=PhpKeywordFunc227;
  FPhpIdentFuncTable[228]:=PhpKeywordFunc228;
  FPhpIdentFuncTable[229]:=PhpKeywordFunc229;
  FPhpIdentFuncTable[230]:=PhpKeywordFunc230;
  FPhpIdentFuncTable[231]:=PhpKeywordFunc231;
  FPhpIdentFuncTable[232]:=PhpKeywordFunc232;
  FPhpIdentFuncTable[233]:=PhpKeywordFunc233;
  FPhpIdentFuncTable[234]:=PhpKeywordFunc234;
  FPhpIdentFuncTable[235]:=PhpKeywordFunc235;
  FPhpIdentFuncTable[236]:=PhpKeywordFunc236;
  FPhpIdentFuncTable[237]:=PhpKeywordFunc237;
  FPhpIdentFuncTable[238]:=PhpKeywordFunc238;
  FPhpIdentFuncTable[239]:=PhpKeywordFunc239;
  FPhpIdentFuncTable[240]:=PhpKeywordFunc240;
  FPhpIdentFuncTable[241]:=PhpKeywordFunc241;
  FPhpIdentFuncTable[242]:=PhpKeywordFunc242;
  FPhpIdentFuncTable[243]:=PhpKeywordFunc243;
  FPhpIdentFuncTable[244]:=PhpKeywordFunc244;
  FPhpIdentFuncTable[245]:=PhpKeywordFunc245;
  FPhpIdentFuncTable[246]:=PhpKeywordFunc246;
  FPhpIdentFuncTable[247]:=PhpKeywordFunc247;
  FPhpIdentFuncTable[248]:=PhpKeywordFunc248;
  FPhpIdentFuncTable[249]:=PhpKeywordFunc249;
  FPhpIdentFuncTable[250]:=PhpKeywordFunc250;
  FPhpIdentFuncTable[251]:=PhpKeywordFunc251;
  FPhpIdentFuncTable[252]:=PhpKeywordFunc252;
  FPhpIdentFuncTable[253]:=PhpKeywordFunc253;
  FPhpIdentFuncTable[254]:=PhpKeywordFunc254;
  FPhpIdentFuncTable[255]:=PhpKeywordFunc255;
  FPhpIdentFuncTable[256]:=PhpKeywordFunc256;
  FPhpIdentFuncTable[257]:=PhpKeywordFunc257;
  FPhpIdentFuncTable[258]:=PhpKeywordFunc258;
  FPhpIdentFuncTable[259]:=PhpKeywordFunc259;
  FPhpIdentFuncTable[260]:=PhpKeywordFunc260;
  FPhpIdentFuncTable[261]:=PhpKeywordFunc261;
  FPhpIdentFuncTable[262]:=PhpKeywordFunc262;
  FPhpIdentFuncTable[263]:=PhpKeywordFunc263;
  FPhpIdentFuncTable[264]:=PhpKeywordFunc264;
  FPhpIdentFuncTable[265]:=PhpKeywordFunc265;
  FPhpIdentFuncTable[266]:=PhpKeywordFunc266;
  FPhpIdentFuncTable[267]:=PhpKeywordFunc267;
  FPhpIdentFuncTable[268]:=PhpKeywordFunc268;
  FPhpIdentFuncTable[269]:=PhpKeywordFunc269;
  FPhpIdentFuncTable[270]:=PhpKeywordFunc270;
  FPhpIdentFuncTable[271]:=PhpKeywordFunc271;
  FPhpIdentFuncTable[272]:=PhpKeywordFunc272;
  FPhpIdentFuncTable[273]:=PhpKeywordFunc273;
  FPhpIdentFuncTable[274]:=PhpKeywordFunc274;
  FPhpIdentFuncTable[275]:=PhpKeywordFunc275;
  FPhpIdentFuncTable[276]:=PhpKeywordFunc276;
  FPhpIdentFuncTable[277]:=PhpKeywordFunc277;
  FPhpIdentFuncTable[278]:=PhpKeywordFunc278;
  FPhpIdentFuncTable[279]:=PhpKeywordFunc279;
  FPhpIdentFuncTable[280]:=PhpKeywordFunc280;
  FPhpIdentFuncTable[281]:=PhpKeywordFunc281;
  FPhpIdentFuncTable[282]:=PhpKeywordFunc282;
  FPhpIdentFuncTable[283]:=PhpKeywordFunc283;
  FPhpIdentFuncTable[284]:=PhpKeywordFunc284;
  FPhpIdentFuncTable[285]:=PhpKeywordFunc285;
  FPhpIdentFuncTable[286]:=PhpKeywordFunc286;
  FPhpIdentFuncTable[287]:=PhpKeywordFunc287;
  FPhpIdentFuncTable[288]:=PhpKeywordFunc288;
  FPhpIdentFuncTable[289]:=PhpKeywordFunc289;
  FPhpIdentFuncTable[290]:=PhpKeywordFunc290;
  FPhpIdentFuncTable[291]:=PhpKeywordFunc291;
  FPhpIdentFuncTable[292]:=PhpKeywordFunc292;
  FPhpIdentFuncTable[293]:=PhpKeywordFunc293;
  FPhpIdentFuncTable[294]:=PhpKeywordFunc294;
  FPhpIdentFuncTable[295]:=PhpKeywordFunc295;
  FPhpIdentFuncTable[296]:=PhpKeywordFunc296;
  FPhpIdentFuncTable[297]:=PhpKeywordFunc297;
  FPhpIdentFuncTable[298]:=PhpKeywordFunc298;
  FPhpIdentFuncTable[299]:=PhpKeywordFunc299;
  FPhpIdentFuncTable[300]:=PhpKeywordFunc300;
  FPhpIdentFuncTable[301]:=PhpKeywordFunc301;
  FPhpIdentFuncTable[302]:=PhpKeywordFunc302;
  FPhpIdentFuncTable[303]:=PhpKeywordFunc303;
  FPhpIdentFuncTable[304]:=PhpKeywordFunc304;
  FPhpIdentFuncTable[305]:=PhpKeywordFunc305;
  FPhpIdentFuncTable[306]:=PhpKeywordFunc306;
  FPhpIdentFuncTable[307]:=PhpKeywordFunc307;
  FPhpIdentFuncTable[308]:=PhpKeywordFunc308;
  FPhpIdentFuncTable[309]:=PhpKeywordFunc309;
  FPhpIdentFuncTable[310]:=PhpKeywordFunc310;
  FPhpIdentFuncTable[311]:=PhpKeywordFunc311;
  FPhpIdentFuncTable[312]:=PhpKeywordFunc312;
  FPhpIdentFuncTable[313]:=PhpKeywordFunc313;
  FPhpIdentFuncTable[314]:=PhpKeywordFunc314;
  FPhpIdentFuncTable[315]:=PhpKeywordFunc315;
  FPhpIdentFuncTable[316]:=PhpKeywordFunc316;
  FPhpIdentFuncTable[317]:=PhpKeywordFunc317;
  FPhpIdentFuncTable[318]:=PhpKeywordFunc318;
  FPhpIdentFuncTable[319]:=PhpKeywordFunc319;
  FPhpIdentFuncTable[320]:=PhpKeywordFunc320;
  FPhpIdentFuncTable[321]:=PhpKeywordFunc321;
  FPhpIdentFuncTable[322]:=PhpKeywordFunc322;
  FPhpIdentFuncTable[323]:=PhpKeywordFunc323;
  FPhpIdentFuncTable[324]:=PhpKeywordFunc324;
  FPhpIdentFuncTable[325]:=PhpKeywordFunc325;
  FPhpIdentFuncTable[326]:=PhpKeywordFunc326;
  FPhpIdentFuncTable[327]:=PhpKeywordFunc327;
  FPhpIdentFuncTable[328]:=PhpKeywordFunc328;
  FPhpIdentFuncTable[329]:=PhpKeywordFunc329;
  FPhpIdentFuncTable[330]:=PhpKeywordFunc330;
  FPhpIdentFuncTable[331]:=PhpKeywordFunc331;
  FPhpIdentFuncTable[332]:=PhpKeywordFunc332;
  FPhpIdentFuncTable[333]:=PhpKeywordFunc333;
  FPhpIdentFuncTable[334]:=PhpKeywordFunc334;
  FPhpIdentFuncTable[335]:=PhpKeywordFunc335;
  FPhpIdentFuncTable[336]:=PhpKeywordFunc336;
  FPhpIdentFuncTable[337]:=PhpKeywordFunc337;
  FPhpIdentFuncTable[338]:=PhpKeywordFunc338;
  FPhpIdentFuncTable[339]:=PhpKeywordFunc339;
  FPhpIdentFuncTable[340]:=PhpKeywordFunc340;
  FPhpIdentFuncTable[341]:=PhpKeywordFunc341;
  FPhpIdentFuncTable[342]:=PhpKeywordFunc342;
  FPhpIdentFuncTable[343]:=PhpKeywordFunc343;
  FPhpIdentFuncTable[344]:=PhpKeywordFunc344;
  FPhpIdentFuncTable[345]:=PhpKeywordFunc345;
  FPhpIdentFuncTable[346]:=PhpKeywordFunc346;
  FPhpIdentFuncTable[347]:=PhpKeywordFunc347;
  FPhpIdentFuncTable[348]:=PhpKeywordFunc348;
  FPhpIdentFuncTable[349]:=PhpKeywordFunc349;
  FPhpIdentFuncTable[350]:=PhpKeywordFunc350;
  FPhpIdentFuncTable[351]:=PhpKeywordFunc351;
  FPhpIdentFuncTable[352]:=PhpKeywordFunc352;
  FPhpIdentFuncTable[353]:=PhpKeywordFunc353;
  FPhpIdentFuncTable[354]:=PhpKeywordFunc354;
  FPhpIdentFuncTable[355]:=PhpKeywordFunc355;
  FPhpIdentFuncTable[356]:=PhpKeywordFunc356;
  FPhpIdentFuncTable[357]:=PhpKeywordFunc357;
  FPhpIdentFuncTable[358]:=PhpKeywordFunc358;
  FPhpIdentFuncTable[359]:=PhpKeywordFunc359;
  FPhpIdentFuncTable[360]:=PhpKeywordFunc360;
  FPhpIdentFuncTable[361]:=PhpKeywordFunc361;
  FPhpIdentFuncTable[362]:=PhpKeywordFunc362;
  FPhpIdentFuncTable[363]:=PhpKeywordFunc363;
  FPhpIdentFuncTable[364]:=PhpKeywordFunc364;
  FPhpIdentFuncTable[365]:=PhpKeywordFunc365;
  FPhpIdentFuncTable[366]:=PhpKeywordFunc366;
  FPhpIdentFuncTable[367]:=PhpKeywordFunc367;
  FPhpIdentFuncTable[368]:=PhpKeywordFunc368;
  FPhpIdentFuncTable[369]:=PhpKeywordFunc369;
  FPhpIdentFuncTable[370]:=PhpKeywordFunc370;
  FPhpIdentFuncTable[371]:=PhpKeywordFunc371;
  FPhpIdentFuncTable[372]:=PhpKeywordFunc372;
  FPhpIdentFuncTable[373]:=PhpKeywordFunc373;
  FPhpIdentFuncTable[374]:=PhpKeywordFunc374;
  FPhpIdentFuncTable[376]:=PhpKeywordFunc376;
  FPhpIdentFuncTable[377]:=PhpKeywordFunc377;
  FPhpIdentFuncTable[378]:=PhpKeywordFunc378;
  FPhpIdentFuncTable[379]:=PhpKeywordFunc379;
  FPhpIdentFuncTable[380]:=PhpKeywordFunc380;
  FPhpIdentFuncTable[381]:=PhpKeywordFunc381;
  FPhpIdentFuncTable[382]:=PhpKeywordFunc382;
  FPhpIdentFuncTable[383]:=PhpKeywordFunc383;
  FPhpIdentFuncTable[384]:=PhpKeywordFunc384;
  FPhpIdentFuncTable[385]:=PhpKeywordFunc385;
  FPhpIdentFuncTable[386]:=PhpKeywordFunc386;
  FPhpIdentFuncTable[387]:=PhpKeywordFunc387;
  FPhpIdentFuncTable[388]:=PhpKeywordFunc388;
  FPhpIdentFuncTable[389]:=PhpKeywordFunc389;
  FPhpIdentFuncTable[390]:=PhpKeywordFunc390;
  FPhpIdentFuncTable[391]:=PhpKeywordFunc391;
  FPhpIdentFuncTable[392]:=PhpKeywordFunc392;
  FPhpIdentFuncTable[393]:=PhpKeywordFunc393;
  FPhpIdentFuncTable[394]:=PhpKeywordFunc394;
  FPhpIdentFuncTable[396]:=PhpKeywordFunc396;
  FPhpIdentFuncTable[397]:=PhpKeywordFunc397;
  FPhpIdentFuncTable[398]:=PhpKeywordFunc398;
  FPhpIdentFuncTable[399]:=PhpKeywordFunc399;
  FPhpIdentFuncTable[400]:=PhpKeywordFunc400;
  FPhpIdentFuncTable[401]:=PhpKeywordFunc401;
  FPhpIdentFuncTable[402]:=PhpKeywordFunc402;
  FPhpIdentFuncTable[403]:=PhpKeywordFunc403;
  FPhpIdentFuncTable[404]:=PhpKeywordFunc404;
  FPhpIdentFuncTable[405]:=PhpKeywordFunc405;
  FPhpIdentFuncTable[406]:=PhpKeywordFunc406;
  FPhpIdentFuncTable[407]:=PhpKeywordFunc407;
  FPhpIdentFuncTable[408]:=PhpKeywordFunc408;
  FPhpIdentFuncTable[409]:=PhpKeywordFunc409;
  FPhpIdentFuncTable[410]:=PhpKeywordFunc410;
  FPhpIdentFuncTable[412]:=PhpKeywordFunc412;
  FPhpIdentFuncTable[413]:=PhpKeywordFunc413;
  FPhpIdentFuncTable[414]:=PhpKeywordFunc414;
  FPhpIdentFuncTable[415]:=PhpKeywordFunc415;
  FPhpIdentFuncTable[417]:=PhpKeywordFunc417;
  FPhpIdentFuncTable[418]:=PhpKeywordFunc418;
  FPhpIdentFuncTable[419]:=PhpKeywordFunc419;
  FPhpIdentFuncTable[420]:=PhpKeywordFunc420;
  FPhpIdentFuncTable[421]:=PhpKeywordFunc421;
  FPhpIdentFuncTable[423]:=PhpKeywordFunc423;
  FPhpIdentFuncTable[424]:=PhpKeywordFunc424;
  FPhpIdentFuncTable[425]:=PhpKeywordFunc425;
  FPhpIdentFuncTable[426]:=PhpKeywordFunc426;
  FPhpIdentFuncTable[427]:=PhpKeywordFunc427;
  FPhpIdentFuncTable[428]:=PhpKeywordFunc428;
  FPhpIdentFuncTable[430]:=PhpKeywordFunc430;
  FPhpIdentFuncTable[431]:=PhpKeywordFunc431;
  FPhpIdentFuncTable[433]:=PhpKeywordFunc433;
  FPhpIdentFuncTable[434]:=PhpKeywordFunc434;
  FPhpIdentFuncTable[435]:=PhpKeywordFunc435;
  FPhpIdentFuncTable[436]:=PhpKeywordFunc436;
  FPhpIdentFuncTable[437]:=PhpKeywordFunc437;
  FPhpIdentFuncTable[438]:=PhpKeywordFunc438;
  FPhpIdentFuncTable[439]:=PhpKeywordFunc439;
  FPhpIdentFuncTable[440]:=PhpKeywordFunc440;
  FPhpIdentFuncTable[441]:=PhpKeywordFunc441;
  FPhpIdentFuncTable[442]:=PhpKeywordFunc442;
  FPhpIdentFuncTable[447]:=PhpKeywordFunc447;
  FPhpIdentFuncTable[449]:=PhpKeywordFunc449;
  FPhpIdentFuncTable[450]:=PhpKeywordFunc450;
  FPhpIdentFuncTable[452]:=PhpKeywordFunc452;
  FPhpIdentFuncTable[453]:=PhpKeywordFunc453;
  FPhpIdentFuncTable[454]:=PhpKeywordFunc454;
  FPhpIdentFuncTable[455]:=PhpKeywordFunc455;
  FPhpIdentFuncTable[457]:=PhpKeywordFunc457;
  FPhpIdentFuncTable[458]:=PhpKeywordFunc458;
  FPhpIdentFuncTable[459]:=PhpKeywordFunc459;
  FPhpIdentFuncTable[460]:=PhpKeywordFunc460;
  FPhpIdentFuncTable[463]:=PhpKeywordFunc463;
  FPhpIdentFuncTable[464]:=PhpKeywordFunc464;
  FPhpIdentFuncTable[468]:=PhpKeywordFunc468;
  FPhpIdentFuncTable[470]:=PhpKeywordFunc470;
  FPhpIdentFuncTable[472]:=PhpKeywordFunc472;
  FPhpIdentFuncTable[474]:=PhpKeywordFunc474;
  FPhpIdentFuncTable[477]:=PhpKeywordFunc477;
  FPhpIdentFuncTable[478]:=PhpKeywordFunc478;
  FPhpIdentFuncTable[479]:=PhpKeywordFunc479;
  FPhpIdentFuncTable[480]:=PhpKeywordFunc480;
  FPhpIdentFuncTable[483]:=PhpKeywordFunc483;
  FPhpIdentFuncTable[487]:=PhpKeywordFunc487;
  FPhpIdentFuncTable[503]:=PhpKeywordFunc503;
  FPhpIdentFuncTable[505]:=PhpKeywordFunc505;
  FPhpIdentFuncTable[513]:=PhpKeywordFunc513;
  FPhpIdentFuncTable[515]:=PhpKeywordFunc515;
  FPhpIdentFuncTable[517]:=PhpKeywordFunc517;
  FPhpIdentFuncTable[519]:=PhpKeywordFunc519;
  FPhpIdentFuncTable[521]:=PhpKeywordFunc521;
  FPhpIdentFuncTable[529]:=PhpKeywordFunc529;
  FPhpIdentFuncTable[532]:=PhpKeywordFunc532;
  FPhpIdentFuncTable[533]:=PhpKeywordFunc533;
  FPhpIdentFuncTable[539]:=PhpKeywordFunc539;
  FPhpIdentFuncTable[541]:=PhpKeywordFunc541;
  FPhpIdentFuncTable[549]:=PhpKeywordFunc549;
  FPhpIdentFuncTable[553]:=PhpKeywordFunc553;
  FPhpIdentFuncTable[555]:=PhpKeywordFunc555;
  FPhpIdentFuncTable[559]:=PhpKeywordFunc559;
  FPhpIdentFuncTable[561]:=PhpKeywordFunc561;
  FPhpIdentFuncTable[568]:=PhpKeywordFunc568;
  FPhpIdentFuncTable[575]:=PhpKeywordFunc575;
  FPhpIdentFuncTable[587]:=PhpKeywordFunc587;
  FPhpIdentFuncTable[590]:=PhpKeywordFunc590;
  FPhpIdentFuncTable[635]:=PhpKeywordFunc635;
  FPhpIdentFuncTable[644]:=PhpKeywordFunc644;
end;

procedure TSynWebEngine.PhpNext;
begin
  FInstance^.FTokenPos := FInstance^.FRun;
  if FInstance^.FLine[FInstance^.FRun] = #0 then
    NullProc
  else
    FPhpRangeProcTable[PhpGetRange];
end;

procedure TSynWebEngine.PhpCliNext;
begin
  FInstance^.FTokenID := stkPhpInlineText;
  FInstance^.FTokenPos := FInstance^.FRun;
  if MLCheckNull or PhpCheckBegin then
    Exit;
  repeat
    while not (FInstance^.FLine[FInstance^.FRun] in [#0, '<']) do
      Inc(FInstance^.FRun);
    case FInstance^.FLine[FInstance^.FRun] of
    #0:
      Break;
    '<':
      if PhpCheckBegin(False) then
        Break
      else
        Inc(FInstance^.FRun);
    end;
  until False;
end;

function TSynWebEngine.PhpGetRange: TSynWebPhpRangeState;
begin
  if GetRangeBit(26) then
    Result := srsPhpHeredoc
  else
    Result := TSynWebPhpRangeState(GetRangeInt(3, 23));
end;

procedure TSynWebEngine.PhpSetRange(const ARange: TSynWebPhpRangeState);
begin
  if ARange = srsPhpHeredoc then
    SetRangeBit(26, True)
  else
  begin
    SetRangeBit(26, False);
    SetRangeBit(17, False);
    SetRangeInt(3, 23, Longword(ARange));
  end;
end;

procedure TSynWebEngine.PhpSetSymbolId(ASymbolId: Integer);
begin
  FInstance^.FTokenID := stkPhpSymbol;
  FInstance^.FTokenLastSymbolId := ASymbolId;
end;

function TSynWebEngine.PhpCheckBegin(ABegin: Boolean): Boolean;
begin
  Result := False;
  if (FInstance^.FLine[FInstance^.FRun] = '<') and FInstance^.FOptions.FPhpEmbeded then
  begin
    case FInstance^.FLine[FInstance^.FRun + 1] of
    '?':
      if (UpCase(FInstance^.FLine[FInstance^.FRun + 2]) = 'P') and
        (UpCase(FInstance^.FLine[FInstance^.FRun + 3]) = 'H') and
        (UpCase(FInstance^.FLine[FInstance^.FRun + 4]) = 'P') and
        (FInstance^.FLine[FInstance^.FRun + 5] <= #32) then
      begin
        if ABegin then
          PhpBegin(spotPhp);
      end else
        if FInstance^.FOptions.FPhpShortOpenTag then
        begin
          if ABegin then
            PhpBegin(spotPhpShort);
        end else
          Exit;
    '%':
      if FInstance^.FOptions.FPhpAspTags then
      begin
        if ABegin then
          PhpBegin(spotASP);
      end else
        Exit;
    else // case
      Exit;
    end;
  end else
    Exit;
  Result := True;
end;

procedure TSynWebEngine.PhpBegin(ATagKind: TSynWebPhpOpenTag);
begin
  SetHighlighterType(
    TSynWebHighlighterType(Longword(FInstance^.FHighlighterType) + Longword(shtPhpInML)),
    False,
    True,
    ATagKind = spotML);
  SetRangeInt(12, 17, 0);
  if ATagKind = spotML then
    PhpSetRange(srsPhpDefault)
  else
  begin
    if ATagKind = spotPhp then
      SetRangeBit(19, True);
    Next;
  end;
end;

procedure TSynWebEngine.PhpEnd(AMLTag: Boolean);
begin
  SetRangeInt(12, 17, 0);
  if FInstance^.FLine[FInstance^.FRun] = #0 then
    SetRangeInt(3, 29, Longword(FInstance^.FHighlighterType) - Longword(shtPhpInML))
  else
  begin
    SetHighlighterType(
      TSynWebHighlighterType(Longword(FInstance^.FHighlighterType) - Longword(shtPhpInML)),
      AMLTag,
      True, not AMLTag);
    if AMLTag then
      Next;
  end;
end;

procedure TSynWebEngine.PhpSpaceProc;
begin
  repeat
    Inc(FInstance^.FRun);
  until not (FInstance^.FLine[FInstance^.FRun] in [#1..#32]);
  FInstance^.FTokenID := stkPhpSpace;
end;

procedure TSynWebEngine.PhpQuestionProc;
begin
  Inc(FInstance^.FRun);
  if (FInstance^.FLine[FInstance^.FRun] = '>') and FInstance^.FOptions.FPhpEmbeded then
  begin
    Inc(FInstance^.FRun);
    FInstance^.FTokenID := stkMLTag;
    PhpEnd(False);
  end else
    PhpSetSymbolId(PhpSymbolID_Question);
end;

procedure TSynWebEngine.PhpNumberProc;
begin
  if PhpCheckNumberProc then
    FInstance^.FTokenID := stkPhpNumber
  else
    FInstance^.FTokenID := stkPhpError;
end;

function TSynWebEngine.PhpCheckNumberProc: Boolean;
begin
  Result := False;
  if (FInstance^.FLine[FInstance^.FRun] = '0') and
    (FInstance^.FLine[FInstance^.FRun + 1] = 'x') then
  begin
    Inc(FInstance^.FRun, 2);
    // if FInstance^.FLine[FInstance^.FRun] in ['a'..'f', 'A'..'F', '0'..'9'] then
    if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 10) <> 0 then
      repeat
        Inc(FInstance^.FRun);
      until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 10) = 0
      // until not (FInstance^.FLine[FInstance^.FRun] in ['a'..'f', 'A'..'F', '0'..'9'])
    else
      Exit;
  end else
  begin
    while FInstance^.FLine[FInstance^.FRun] in ['0'..'9'] do
      Inc(FInstance^.FRun);
    if FInstance^.FLine[FInstance^.FRun] = '.' then
    begin
      Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] in ['0'..'9'] then
        repeat
          Inc(FInstance^.FRun);
        until not (FInstance^.FLine[FInstance^.FRun] in ['0'..'9'])
      else
        Exit;
    end;
    if FInstance^.FLine[FInstance^.FRun] in ['e', 'E'] then
    begin
      Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] in ['+', '-'] then
        Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] in ['0'..'9'] then
        repeat
          Inc(FInstance^.FRun);
        until not (FInstance^.FLine[FInstance^.FRun] in ['0'..'9'])
      else
        Exit;
    end;
  end;
  Result := True;
end;

procedure TSynWebEngine.PhpString34Proc;
begin
  Inc(FInstance^.FRun);
  PhpSetRange(srsPhpString34);
  // if FInstance^.FLine[FInstance^.FRun] in [#0, '\', '{', '$'] then
  if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 30) <> 0 then
    FInstance^.FTokenID := stkPhpString
  else
    PhpRangeString34Proc;
end;

procedure TSynWebEngine.PhpString39Proc;
begin
  Inc(FInstance^.FRun);
  PhpSetRange(srsPhpString39);
  if FInstance^.FLine[FInstance^.FRun] in [#0, '\'] then
    FInstance^.FTokenID := stkPhpString
  else
    PhpRangeString39Proc;
end;

procedure TSynWebEngine.PhpStringShellProc;
begin
  Inc(FInstance^.FRun);
  PhpSetRange(srsPhpStringShell);
  if FInstance^.FLine[FInstance^.FRun] in [#0, '`'] then
    FInstance^.FTokenID := stkPhpString
  else
    PhpRangeStringShellProc;
end;

procedure TSynWebEngine.PhpAndProc;
begin
  Inc(FInstance^.FRun);
  case FInstance^.FLine[FInstance^.FRun] of
  '=':
    begin
      Inc(FInstance^.FRun);
      PhpSetSymbolId(PhpSymbolID_BitwiseAndAssign);
    end;
  '&':
    begin
      Inc(FInstance^.FRun);
      PhpSetSymbolId(PhpSymbolID_LogicAnd);
    end;
  else // case 
    PhpSetSymbolId(PhpSymbolID_BitwiseAnd);
  end;
end;

procedure TSynWebEngine.PhpOrProc;
begin
  Inc(FInstance^.FRun);
  case FInstance^.FLine[FInstance^.FRun] of
  '=':
    begin
      Inc(FInstance^.FRun);
      PhpSetSymbolId(PhpSymbolID_BitwiseOrAssign);
    end;
  '|':
    begin
      Inc(FInstance^.FRun);
      PhpSetSymbolId(PhpSymbolID_LogicOr);
    end;
  else // case 
    PhpSetSymbolId(PhpSymbolID_BitwiseOr);
  end;
end;

procedure TSynWebEngine.PhpAtSymbolProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = '@' then
    Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkPhpKeyword;
  FInstance^.FTokenLastID := PhpKeyID_Special_At;
end;

procedure TSynWebEngine.PhpEqualProc;
begin
  Inc(FInstance^.FRun);
  case FInstance^.FLine[FInstance^.FRun] of
  '=':
    begin
      Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] = '=' then
      begin
        Inc(FInstance^.FRun);
        PhpSetSymbolId(PhpSymbolID_Identical);
      end else
        PhpSetSymbolId(PhpSymbolID_Equal);
    end;
  '>':
    begin
      Inc(FInstance^.FRun);
      PhpSetSymbolId(PhpSymbolID_Arrow);
    end;
  else // case
    PhpSetSymbolId(PhpSymbolID_Assign);
  end;
end;

procedure TSynWebEngine.PhpGreaterProc;
begin
  Inc(FInstance^.FRun);
  case FInstance^.FLine[FInstance^.FRun] of
  '=':
    begin
      Inc(FInstance^.FRun);
      PhpSetSymbolId(PhpSymbolID_GreaterEqual);
    end;
  '>':
    begin
      Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] = '=' then
      begin
        Inc(FInstance^.FRun);
        PhpSetSymbolId(PhpSymbolID_ShiftRightAssign);
      end else
        PhpSetSymbolId(PhpSymbolID_ShiftRight);
    end;
  else // case
    PhpSetSymbolId(PhpSymbolID_Greater);
  end;
end;

procedure TSynWebEngine.PhpLowerProc;
var
  tmpRun: Longword;
begin
  Inc(FInstance^.FRun);
  case FInstance^.FLine[FInstance^.FRun] of
  '/':
    if (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 1]] =
      FInstance^.FHashTable['s']) and
      (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 2]] =
      FInstance^.FHashTable['c']) and
      (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 3]] =
      FInstance^.FHashTable['r']) and
      (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 4]] =
      FInstance^.FHashTable['i']) and
      (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 5]] =
      FInstance^.FHashTable['p']) and
      (FInstance^.FHashTable[FInstance^.FLine[FInstance^.FRun + 6]] =
      FInstance^.FHashTable['t']) and
      (TSynWebIdentTable2[FInstance^.FLine[FInstance^.FRun + 7]] and (1 shl 0) <> 0) then
      // (FInstance^.FLine[FInstance^.FRun+7] in [#0..#32, '>']) then
    begin
      Dec(FInstance^.FRun);
      PhpEnd(True);
      Exit;
    end else
      PhpSetSymbolId(PhpSymbolID_Lower);
  '=':
    begin
      Inc(FInstance^.FRun);
      PhpSetSymbolId(PhpSymbolID_LowerEqual);
    end;
  '<':
    begin
      Inc(FInstance^.FRun);
      case FInstance^.FLine[FInstance^.FRun] of
      '=':
        begin
          Inc(FInstance^.FRun);
          PhpSetSymbolId(PhpSymbolID_ShiftLeftAssign);
        end;
      '<':
        begin
          Inc(FInstance^.FRun);
          tmpRun := FInstance^.FRun;
          while FInstance^.FLine[tmpRun] in [#1..#32] do
            Inc(tmpRun);
          // if not (FInstance^.FLine[tmpRun] in ['a'..'z', 'A'..'Z', '_', #$7F..#$FF]) then
          if TSynWebIdentTable[FInstance^.FLine[tmpRun]] and (1 shl 28) = 0 then
          begin
            FInstance^.FTokenID := stkPhpError;
            Exit;
          end;
          PhpSetRange(srsPhpSubProc);
          SetRangeInt(3, 20, 2);
          PhpSetSymbolId(PhpSymbolID_Heredoc);
        end;
      else // case
        PhpSetSymbolId(PhpSymbolID_ShiftLeft);
      end;
    end;
  else // case
    PhpSetSymbolId(PhpSymbolID_Lower);
  end;
end;

procedure TSynWebEngine.PhpPlusProc;
begin
  Inc(FInstance^.FRun);
  case FInstance^.FLine[FInstance^.FRun] of
  '=':
    begin
      Inc(FInstance^.FRun);
      PhpSetSymbolId(PhpSymbolID_AddAssign);
    end;
  '+':
    begin
      Inc(FInstance^.FRun);
      PhpSetSymbolId(PhpSymbolID_Increment);
    end;
  else // case 
    PhpSetSymbolId(PhpSymbolID_Add);
  end;
end;

procedure TSynWebEngine.PhpMinusProc;
begin               
  Inc(FInstance^.FRun);
  case FInstance^.FLine[FInstance^.FRun] of
  '=':
    begin
      Inc(FInstance^.FRun);
      PhpSetSymbolId(PhpSymbolID_DecAssign);
    end;
  '-':
    begin
      Inc(FInstance^.FRun);
      PhpSetSymbolId(PhpSymbolID_Decrement);
    end;
  '>':
    begin
      Inc(FInstance^.FRun);
      PhpSetSymbolId(PhpSymbolID_ObjectMethod);
      SetRangeBit(17, True);
    end;
  else // case 
    PhpSetSymbolId(PhpSymbolID_Dec);
  end;
end;

procedure TSynWebEngine.PhpMulProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = '=' then
  begin
    Inc(FInstance^.FRun);
    PhpSetSymbolId(PhpSymbolID_MulAssign);
  end else
    PhpSetSymbolId(PhpSymbolID_Mul);
end;

procedure TSynWebEngine.PhpDivProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = '=' then
  begin
    Inc(FInstance^.FRun);
    PhpSetSymbolId(PhpSymbolID_DivAssign);
  end else
    PhpSetSymbolId(PhpSymbolID_Div);
end;

procedure TSynWebEngine.PhpModProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = '=' then
  begin
    Inc(FInstance^.FRun);
    PhpSetSymbolId(PhpSymbolID_ModAssign);
  end else
    PhpSetSymbolId(PhpSymbolID_Mod);
end;

procedure TSynWebEngine.PhpXorProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = '=' then
  begin
    Inc(FInstance^.FRun);
    PhpSetSymbolId(PhpSymbolID_XorAssign);
  end else
    PhpSetSymbolId(PhpSymbolID_Xor);
end;

procedure TSynWebEngine.PhpSlashProc;
begin
  case FInstance^.FLine[FInstance^.FRun + 1] of
  '/':
    begin
      Inc(FInstance^.FRun);
      PhpHashProc;
    end;
  '*':
    begin
      Inc(FInstance^.FRun, 2);
      if (FInstance^.FLine[FInstance^.FRun] = '*') and
        (FInstance^.FLine[FInstance^.FRun + 1] <= #32) then
      begin
        Inc(FInstance^.FRun);    
        PhpSetRange(srsPhpDocComment);
        SetRangeBit(19, False);
        if FInstance^.FLine[FInstance^.FRun] <> #0 then
          PhpRangeDocCommentProc
        else
          FInstance^.FTokenID := stkPhpDocComment;
      end else
      begin
        PhpSetRange(srsPhpComment);
        if FInstance^.FLine[FInstance^.FRun] <> #0 then
          PhpRangeCommentProc
        else
          FInstance^.FTokenID := stkPhpComment;
      end;
    end;
  else // case
    PhpDivProc;
  end;
end;

procedure TSynWebEngine.PhpPercentProc;
begin
  if (FInstance^.FLine[FInstance^.FRun + 1] = '>') and FInstance^.FOptions.FPhpEmbeded then
  begin
    Inc(FInstance^.FRun, 2);
    if FInstance^.FOptions.FPhpAspTags then
    begin
      FInstance^.FTokenID := stkMLTag;
      PhpEnd(False);
    end else
      FInstance^.FTokenID := stkPhpError;
  end else
    PhpModProc;
end;

procedure TSynWebEngine.PhpHashProc;
begin
  FInstance^.FTokenID := stkPhpComment;
  repeat
    repeat
      Inc(FInstance^.FRun)
    until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 17) <> 0;
    // until FInstance^.FLine[FInstance^.FRun] in [#0, #10, #13, '%', '?'];
    case FInstance^.FLine[FInstance^.FRun] of
    #0:
      Exit;
    '?':
      if (FInstance^.FLine[FInstance^.FRun + 1] = '>') and
        FInstance^.FOptions.FPhpEmbeded then
        Exit;
    '%':
      if (FInstance^.FLine[FInstance^.FRun + 1] = '>') and
        FInstance^.FOptions.FPhpAspTags and FInstance^.FOptions.FPhpEmbeded then
        Exit;
    else // case
      Exit;
    end;
  until False;
end;

procedure TSynWebEngine.PhpNotProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = '=' then
  begin
    Inc(FInstance^.FRun);
    if FInstance^.FLine[FInstance^.FRun] = '=' then
    begin
      Inc(FInstance^.FRun);
      PhpSetSymbolId(PhpSymbolID_NotIdentical)
    end else
      PhpSetSymbolId(PhpSymbolID_NotEqual);
  end else
    PhpSetSymbolId(PhpSymbolID_Not);
end;

procedure TSynWebEngine.PhpDotProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = '=' then
  begin
    Inc(FInstance^.FRun);
    PhpSetSymbolId(PhpSymbolID_ConcatAssign);
  end else
    if FInstance^.FLine[FInstance^.FRun] in ['0'..'9'] then
      PhpNumberProc
    else
      PhpSetSymbolId(PhpSymbolID_Concat);
end;

procedure TSynWebEngine.PhpColonProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = ':' then
  begin
    Inc(FInstance^.FRun);
    PhpSetSymbolId(PhpSymbolID_ClassMethod);
    SetRangeBit(17, True);
  end else
    PhpSetSymbolId(PhpSymbolID_Colon);
end;

procedure TSynWebEngine.PhpParentheseOpenProc;
begin 
  Inc(FInstance^.FRun);
  PhpSetSymbolId(PhpSymbolID_ParentheseOpen);
end;

procedure TSynWebEngine.PhpParentheseCloseProc;
begin   
  Inc(FInstance^.FRun);
  PhpSetSymbolId(PhpSymbolID_ParentheseClose);
end;

procedure TSynWebEngine.PhpBoxBracketOpenProc;
begin  
  Inc(FInstance^.FRun);
  PhpSetSymbolId(PhpSymbolID_BoxBracketOpen);
end;

procedure TSynWebEngine.PhpBoxBracketCloseProc;
begin   
  Inc(FInstance^.FRun);
  PhpSetSymbolId(PhpSymbolID_BoxBracketClose);
end;

procedure TSynWebEngine.PhpBraceOpenProc;
begin  
  Inc(FInstance^.FRun);
  PhpSetSymbolId(PhpSymbolID_BraceOpen);
end;

procedure TSynWebEngine.PhpBraceCloseProc;
begin
  Inc(FInstance^.FRun);
  PhpSetSymbolId(PhpSymbolID_BraceClose);
end;

procedure TSynWebEngine.PhpTildeProc;   
begin
  Inc(FInstance^.FRun);
  PhpSetSymbolId(PhpSymbolID_Tilde);
end;

procedure TSynWebEngine.PhpCommaProc;   
begin
  Inc(FInstance^.FRun);
  PhpSetSymbolId(PhpSymbolID_Comma);
end;

procedure TSynWebEngine.PhpSemiColonProc; 
begin
  Inc(FInstance^.FRun);
  PhpSetSymbolId(PhpSymbolID_SemiColon);
end;

procedure TSynWebEngine.PhpVarProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = '$' then
    Inc(FInstance^.FRun);
  // if FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '_', #$7F..#$FF] then
  if (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 28) <> 0)
    or (FInstance^.FLine[FInstance^.FRun] = '{') then
    FInstance^.FTokenID := stkPhpKeyword
  else
    FInstance^.FTokenID := stkPhpError;
  FInstance^.FTokenLastID := PhpKeyID_Special_Variable;
  SetRangeBit(17, False);
end;

procedure TSynWebEngine.PhpIdentProc;
begin
  repeat
    Inc(FInstance^.FRun);
  until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 29) = 0;
  // until not(FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '_', '0'..'9', #$7F..#$FF]);
  if (FInstance^.FTokenPos > 0) and (FInstance^.FLine[FInstance^.FTokenPos - 1] = '$') then
    FInstance^.FTokenID := stkPhpVariable
  else
    FInstance^.FTokenID := PhpIdentCheck;
end;

procedure TSynWebEngine.PhpErrorProc;
begin
  Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkPhpError;
end;

function TSynWebEngine.PhpDoStringDouble(AIsHeredoc: Boolean;
  ARangeChar: Boolean): Boolean;
var
  StringChar: char;

  procedure TryDoSpace;
  begin
    while FInstance^.FLine[FInstance^.FRun] in [#1..#32] do
      Inc(FInstance^.FRun);
  end;

  procedure DoIdent;
  begin
    repeat
      Inc(FInstance^.FRun);
    until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 29) = 0;
    // until not(FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '_', '0'..'9', #$7F..#$FF]);
  end;

  function TryDoIdent: Boolean;
  begin
    // if FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '_', #$7F..#$FF] then
    if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 28) <> 0 then
    begin
      DoIdent;
      Result := True;
    end else
      Result := False;
  end;

  function DoStringSingle: Boolean;
  begin
    Result := True;
    repeat
      if FInstance^.FLine[FInstance^.FRun] = '\' then
      begin
        Inc(FInstance^.FRun);
        if FInstance^.FLine[FInstance^.FRun] in [#39, '\'] then
          Inc(FInstance^.FRun);
      end;
      // while not(FInstance^.FLine[FInstance^.FRun] in [#0, #39, '\'] do
      while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 24) = 0 do
        Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] = '\' then
        Continue
      else
      begin
        if FInstance^.FLine[FInstance^.FRun] <> #39 then
          Result := False;
        Exit;
      end;
    until False;
  end;

  function DoStringObject(AAllowSpaces: Boolean = True): Boolean;
  begin
    Inc(FInstance^.FRun, 2);
    if AAllowSpaces then
    begin
      TryDoSpace;
      Result := TryDoIdent;
      TryDoSpace;
    end else
      Result := TryDoIdent;
  end;

  function DoStringVar: Boolean;
  begin
    Inc(FInstance^.FRun);
    Result := TryDoIdent;
  end;

  function DoStringVar2: Boolean;
  begin
    TryDoSpace;
    Result := True;
    case FInstance^.FLine[FInstance^.FRun] of
    '-':
      if (FInstance^.FLine[FInstance^.FRun + 1] = '>') and
        ((not DoStringObject) or ((FInstance^.FLine[FInstance^.FRun] in ['[', '-']) and
        not DoStringVar2)) then
        Result := False;
    '[':
      begin
        Inc(FInstance^.FRun);
        TryDoSpace;
        case FInstance^.FLine[FInstance^.FRun] of
        '$':
          if (not DoStringVar) or ((FInstance^.FLine[FInstance^.FRun] in ['[', '-']) and
            not DoStringVar2) then
            Result := False;
        '0'..'9', '.':
          if not PhpCheckNumberProc then
            Result := False;
        #39:
          begin
            Inc(FInstance^.FRun);
            if DoStringSingle then
              Inc(FInstance^.FRun)
            else
              Result := False;
          end;
        '"':
          begin
            Inc(FInstance^.FRun);
            while not PhpDoStringDouble(False, False) and
              (FInstance^.FLine[FInstance^.FRun] <> #0) and (FInstance^.FTokenID <>
                stkPhpError) do
              ; // Empty
            if (FInstance^.FLine[FInstance^.FRun] = '"') and
              (FInstance^.FTokenID <> stkPhpError) then
            begin
              FInstance^.FTokenID := stkPhpStringSpecial;
              Inc(FInstance^.FRun);
            end else
              Result := False;
          end;
          else
            if not TryDoIdent then
              Result := False;
        end;
        TryDoSpace;
        if not Result or (FInstance^.FLine[FInstance^.FRun] <> ']') then
          Result := False
        else
        begin
          Inc(FInstance^.FRun);
          TryDoSpace;
          if (FInstance^.FLine[FInstance^.FRun] in ['[', '-']) and not DoStringVar2 then
            Result := False;
        end;
      end;
    end;
  end;

begin
  if not ARangeChar or (PhpGetRange = srsPhpString34) then
    StringChar := #34
  else
    StringChar := '`';
  Result := False;
  FInstance^.FTokenID := stkPhpStringSpecial;
  case FInstance^.FLine[FInstance^.FRun] of
  '$':
    begin
      Inc(FInstance^.FRun);
      if TryDoIdent then
      begin
        case FInstance^.FLine[FInstance^.FRun] of
        '-':
          if FInstance^.FLine[FInstance^.FRun + 1] = '>' then
            if not DoStringObject(False) then
              FInstance^.FTokenID := stkPhpError;
        '[':
          begin
            Inc(FInstance^.FRun);
            case FInstance^.FLine[FInstance^.FRun] of
              '$':
                if not DoStringVar then
                  FInstance^.FTokenID := stkPhpError;
              '0'..'9', '.':
                if not PhpCheckNumberProc then
                  FInstance^.FTokenID := stkPhpError;
              else
                if not TryDoIdent then
                  FInstance^.FTokenID := stkPhpError;
            end;
            if FInstance^.FLine[FInstance^.FRun] = ']' then
              Inc(FInstance^.FRun)
            else
              FInstance^.FTokenID := stkPhpError;
          end;
        end;
        Exit;
      end else
        if FInstance^.FLine[FInstance^.FRun] = '{' then
        begin
          Inc(FInstance^.FRun);
          if not TryDoIdent or not DoStringVar2 or
            (FInstance^.FLine[FInstance^.FRun] <> '}') then
            FInstance^.FTokenID := stkPhpError
          else
            Inc(FInstance^.FRun);
          Exit;
        end;
    end;
  '{':
    begin
      Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] = '$' then
      begin
        Inc(FInstance^.FRun);
        if not TryDoIdent or not DoStringVar2 or
          (FInstance^.FLine[FInstance^.FRun] <> '}') then
          FInstance^.FTokenID := stkPhpError
        else
          Inc(FInstance^.FRun);
        Exit;
      end;
    end;
  '\':
    begin
      Inc(FInstance^.FRun);
      // if FInstance^.FLine[FInstance^.FRun] in ['n', 'r', 't', '\', '$', #34, '0'..'7', 'x'] then
      if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 18) <> 0 then
      begin
        Inc(FInstance^.FRun);
        case FInstance^.FLine[FInstance^.FRun - 1] of
        '0'..'7':
          begin
            if FInstance^.FLine[FInstance^.FRun] in ['0'..'7'] then
            begin
              Inc(FInstance^.FRun);
              if FInstance^.FLine[FInstance^.FRun] in ['0'..'7'] then
                Inc(FInstance^.FRun);
            end;
            Exit;
          end;
        'x':
          // if FInstance^.FLine[FInstance^.FRun] in ['a'..'f', 'A'..'F', '0'..'9'] then
          if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 10) <> 0 then
          begin
            Inc(FInstance^.FRun);
            // if FInstance^.FLine[FInstance^.FRun] in ['a'..'f', 'A'..'F', '0'..'9'] then
            if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and
              (1 shl 10) <> 0 then
              Inc(FInstance^.FRun);
            Exit;
          end;
        else // case
          Exit;
        end;
      end;
    end;
  end;
  FInstance^.FTokenID := stkPhpString;
  repeat
    if StringChar = #34 then
      // while not(FInstance^.FLine[FInstance^.FRun] in [#0, #34, '\', '{', '$'] do
      while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 25) = 0 do
        Inc(FInstance^.FRun)
    else
      // while not(FInstance^.FLine[FInstance^.FRun] in [#0, '`', '\', '{', '$'] do
      while TSynWebIdentTable2[FInstance^.FLine[FInstance^.FRun]] and (1 shl 5) = 0 do
        Inc(FInstance^.FRun);
    if FInstance^.FLine[FInstance^.FRun] = StringChar then
      if AIsHeredoc then
      begin
        Inc(FInstance^.FRun);
        Continue;
      end else
        Result := True;
    Exit;
  until False;
end;

procedure TSynWebEngine.PhpSubProcProc;
var
  s: String;
  i: Integer;

  procedure DoDefault;
  begin
    SetRangeInt(3, 20, 0);
    PhpSetRange(srsPhpDefault);
  end;

begin
  case GetRangeInt(3, 20) of
  0:
    begin
      Inc(FInstance^.FRun, 2);
      FInstance^.FTokenID := stkMLTag;
      SetRangeInt(3, 20, 1);
    end;
  1:
    begin
      DoDefault;
      if GetRangeBit(19) then
      begin
        SetRangeBit(19, False);
        Inc(FInstance^.FRun, 3);
        FInstance^.FTokenID := stkPhpKeyword;
        FInstance^.FTokenLastID := PhpKeyID_Special_PhpTag;
      end else
        if (FInstance^.FLine[FInstance^.FRun] = '=') and (FInstance^.FOptions.FPhpShortOpenTag) then
        begin
          Inc(FInstance^.FRun);
          FInstance^.FTokenID := stkPhpKeyword;
          FInstance^.FTokenLastID := PhpKeyID_Special_PhpTagEcho;
        end else
          PhpRangeDefaultProc;
    end;
  2:
    begin
      if FInstance^.FLine[FInstance^.FRun] in [#1..#32] then
      begin
        repeat
          Inc(FInstance^.FRun);
        until not (FInstance^.FLine[FInstance^.FRun] in [#1..#32]);
        FInstance^.FTokenID := stkPhpSpace;
        Exit;
      end;
      repeat
        Inc(FInstance^.FRun);
      until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 29) = 0;
      // until not(FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '_', '0'..'9', #$7F..#$FF]);
      if FInstance^.FLine[FInstance^.FRun] <> #0 then
      begin
        FInstance^.FTokenID := stkPhpError;
        PhpSetRange(srsPhpDefault);
        Exit;
      end;
      FInstance^.FTokenID := stkPhpKeyword;
      FInstance^.FTokenLastID := PhpKeyID_Special_HeredocBegin;
      PhpSetRange(srsPhpHeredoc);
      s := GetToken;
      i := FPhpHereDocList.IndexOf(s);
      if i in [0..255] then
      begin
        SetRangeInt(8, 17, i);
        SetRangeBit(25, True);
      end else
      begin
        SetRangeInt(8, 17, GetCrc8String(s));
        SetRangeBit(25, False);
      end;
    end;
  end;
end;

procedure TSynWebEngine.PhpRangeDefaultProc;
begin
  FPhpProcTable[FInstance^.FLine[FInstance^.FRun]];
end;

procedure TSynWebEngine.PhpRangeCommentProc;
begin
  repeat
    if (FInstance^.FLine[FInstance^.FRun] = '*') and
      (FInstance^.FLine[FInstance^.FRun + 1] = '/') then
    begin
      Inc(FInstance^.FRun, 2);
      PhpSetRange(srsPhpDefault);
      Break;
    end;
    Inc(FInstance^.FRun);
  until FInstance^.FLine[FInstance^.FRun] = #0;
  FInstance^.FTokenID := stkPhpComment;
end;

procedure TSynWebEngine.PhpRangeDocCommentProc;

  function CheckDoc: Integer;
  var
    i: Integer;
  begin
    i := 0;
    while FInstance^.FLine[i] in [' ', #9] do
      Inc(i);
    if FInstance^.FLine[i] <> '*' then
    begin
      Result := 0;
      Exit;
    end;
    Inc(i);
    while FInstance^.FLine[i] in [' ', #9] do
      Inc(i);
    if i = FInstance^.FTokenPos then
      Result := 1
    else
      Result := 2;
  end;

begin
  if GetRangeBit(19) then
  begin
    if (FInstance^.FLine[FInstance^.FRun] = '*') and
      (FInstance^.FLine[FInstance^.FRun + 1] = '/') then
      begin
        Inc(FInstance^.FRun, 2);
        PhpSetRange(srsPhpDefault);
        FInstance^.FTokenID := stkPhpDocComment;
        Exit;
      end;

    while not (FInstance^.FLine[FInstance^.FRun] in [#0, '}']) do
    begin
      if (FInstance^.FLine[FInstance^.FRun] = '*') and
        (FInstance^.FLine[FInstance^.FRun + 1] = '/') then
        Break;
      Inc(FInstance^.FRun);
    end;

    FInstance^.FTokenID := stkPhpDocCommentTag;
    case FInstance^.FLine[FInstance^.FRun] of
    '}':
      begin
        Inc(FInstance^.FRun);
        SetRangeBit(19, False);
        Exit;
      end;
    #0:
      ; // Nothing
    else // case
      SetRangeBit(19, False);
      Exit;
    end;         
  end else
  begin
    case FInstance^.FLine[FInstance^.FRun] of
    '{':
      if (FInstance^.FLine[FInstance^.FRun + 1] = '@') and
        // (FInstance^.FLine[FInstance^.FRun + 2] in ['a'..'z', 'A'..'Z']) then
        (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 2]] and (1 shl 0) <> 0) then
      begin
        Inc(FInstance^.FRun, 3);
        while not (FInstance^.FLine[FInstance^.FRun] in [#0, '}']) do
        begin
          if (FInstance^.FLine[FInstance^.FRun] = '*') and
            (FInstance^.FLine[FInstance^.FRun + 1] = '/') then
            Break;
          Inc(FInstance^.FRun);
        end;
        case FInstance^.FLine[FInstance^.FRun] of
        '}':
          Inc(FInstance^.FRun);
        #0:
          SetRangeBit(19, True);
        end;
        if CheckDoc <> 0 then
        begin
          FInstance^.FTokenID := stkPhpDocCommentTag;
          Exit;
        end else
        begin
          SetRangeBit(19, False);
          Dec(FInstance^.FRun);
        end;
      end;
    '@':
      // if FInstance^.FLine[FInstance^.FRun + 1] in ['a'..'z', 'A'..'Z'] then
      if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 1]] and (1 shl 0) <> 0 then
      begin
        Inc(FInstance^.FRun, 2);
        while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 0) <> 0 do
        begin
          if (FInstance^.FLine[FInstance^.FRun] = '*') and
            (FInstance^.FLine[FInstance^.FRun + 1] = '/') then
            Break;
          Inc(FInstance^.FRun);
        end;
        if CheckDoc = 1 then
        begin
          FInstance^.FTokenID := stkPhpDocCommentTag;
          Exit;
        end else
          Dec(FInstance^.FRun);
      end;
    end;
  end;

  repeat
    case FInstance^.FLine[FInstance^.FRun] of
    '*':
      if FInstance^.FLine[FInstance^.FRun + 1] = '/' then
      begin
        Inc(FInstance^.FRun, 2);
        PhpSetRange(srsPhpDefault);
        Break;
      end;
    '{':
      if (FInstance^.FLine[FInstance^.FRun + 1] = '@') and
      // (FInstance^.FLine[FInstance^.FRun + 2] in ['a'..'z', 'A'..'Z'] then
        (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 2]] and (1 shl 0) <> 0) then
        Break;
    '@':
      // if FInstance^.FLine[FInstance^.FRun + 1] in ['a'..'z', 'A'..'Z'] then
      if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 1]] and (1 shl 0) <> 0 then
        Break;
    end;
    Inc(FInstance^.FRun);
  until FInstance^.FLine[FInstance^.FRun] = #0;
  FInstance^.FTokenID := stkPhpDocComment;
end;

procedure TSynWebEngine.PhpRangeString34Proc;
begin
  if PhpDoStringDouble then
  begin
    Inc(FInstance^.FRun);
    PhpSetRange(srsPhpDefault);
  end;
end;

procedure TSynWebEngine.PhpRangeString39Proc;
begin
  if FInstance^.FLine[FInstance^.FRun] = '\' then
  begin
    Inc(FInstance^.FRun);
    if FInstance^.FLine[FInstance^.FRun] in [#39, '\'] then
    begin
      Inc(FInstance^.FRun);
      FInstance^.FTokenID := stkPhpStringSpecial;
      Exit;
    end;
  end;
  FInstance^.FTokenID := stkPhpString;
  repeat
    // while not(FInstance^.FLine[FInstance^.FRun] in [#0, #39, '\'] do
    while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 24) = 0 do
      Inc(FInstance^.FRun);
    if FInstance^.FLine[FInstance^.FRun] <> #39 then
      Exit
    else
    begin
      Inc(FInstance^.FRun);
      PhpSetRange(srsPhpDefault);
      Exit;
    end;
  until False;
  EsSetRange(srsEsDefault);
end;

procedure TSynWebEngine.PhpRangeStringShellProc;
begin
  if PhpDoStringDouble then
  begin
    Inc(FInstance^.FRun);
    PhpSetRange(srsPhpDefault);
  end;
end;

procedure TSynWebEngine.PhpRangeHeredocProc;
var
  OldRun: longint;
  s: String;
begin
  // if FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '_', #$7F..#$FF] then
  if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 28) <> 0 then
  begin
    OldRun := FInstance^.FRun;
    repeat
      Inc(FInstance^.FRun);
    until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 29) = 0;
    // until not(FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '_', '0'..'9', #$7F..#$FF]);
    if ((FInstance^.FLine[FInstance^.FRun] = ';') and
      (FInstance^.FLine[FInstance^.FRun + 1] = #0)) or
      (FInstance^.FLine[FInstance^.FRun] = #0) then
    begin
      s := GetToken;
      if (GetRangeBit(25) and (s = FPhpHereDocList[GetRangeInt(8, 17)])) or
        (not GetRangeBit(25) and (GetRangeInt(8, 17) = GetCrc8String(GetToken))) then
      begin
        FInstance^.FTokenID := stkPhpKeyword;
        FInstance^.FTokenLastID := PhpKeyID_Special_HeredocEnd;
        PhpSetRange(srsPhpDefault);
        Exit;
      end;
    end;
    FInstance^.FRun := OldRun;
  end;
  if PhpDoStringDouble(True) then
  begin
    Inc(FInstance^.FRun);
    PhpSetRange(srsPhpDefault);
  end;
end;

function TSynWebEngine.PhpKeywordComp(const ID: Integer): Boolean;
var
  I: Integer;
  Temp: PChar;
  aKey: String;
  Data: Longword;
begin
  Data := TSynWeb_PhpKeywordsData[ID];
  if (Data and $0F <> $01) or ((Data shr 16) and
    (1 shl Longword(FInstance^.FOptions.FPhpVersion)) = 0) then
  begin
    Result := False;
    Exit;
  end;
  aKey := TSynWeb_PhpKeywords[ID];
  Temp := FInstance^.FToIdent;
  if Length(aKey) = FInstance^.FStringLen then
  begin
    for i := 1 to FInstance^.FStringLen do
    begin
      if TSynWebInsensitiveHashTable[Temp^] <> TSynWebInsensitiveHashTable[aKey[i]] then
      begin
        Result := False;
        Exit;
      end;
      Inc(Temp);
    end;
    FInstance^.FTokenLastID := ID;
    Result := True;
  end else
    Result := False;
end;

function TSynWebEngine.PhpConstComp: Boolean;
var
  I: Integer;
  Temp: PChar;
begin
  Temp := FInstance^.FToIdent;
  for i := 1 to FInstance^.FStringLen do
  begin
    if UpCase(Temp^) <> Temp^ then
    begin
      Result := False;
      Exit;
    end;
    Inc(Temp);
  end;
  Result := True;
end;

function TSynWebEngine.PhpFunctionComp(const ID: Integer): Boolean;
var
  I: Integer;
  Temp: PChar;
  aKey: String;
  Data: Longword;
begin
  Data := TSynWeb_PhpKeywordsData[ID];
  if (Data and $0F <> $08) or ((Data shr 16) and
    (1 shl Longword(FInstance^.FOptions.FPhpVersion)) = 0) then
  begin
    Result := False;
    Exit;
  end;
  aKey := TSynWeb_PhpKeywords[ID];
  Temp := FInstance^.FToIdent;
  if Length(aKey) = FInstance^.FStringLen then
  begin
    for i := 1 to FInstance^.FStringLen do
    begin
      if TSynWebInsensitiveHashTable[Temp^] <> TSynWebInsensitiveHashTable[aKey[i]] then
      begin
        Result := False;
        Exit;
      end;
      Inc(Temp);
    end;
    FInstance^.FTokenLastID := ID;
    Result := True;
  end else
    Result := False;
end;

function TSynWebEngine.PhpIdentCheck: TSynWebTokenKind;
var
  HashKey: Longword;

  procedure KeyHash(ToHash: PChar);
  var
    i: Integer;
  begin
    HashKey := 0;
    FInstance^.FStringLen := FInstance^.FRun - FInstance^.FTokenPos;
    for i := 0 to FInstance^.FStringLen - 1 do
    begin
      Inc(HashKey, TSynWebIdentHashTable[ToHash^]);
      Inc(ToHash);
    end;
  end;

begin
  FInstance^.FTokenLastID := -1;
  if GetRangeBit(17) then
  begin
    Result := stkPhpMethod;
    SetRangeBit(17, False);
    Exit;
  end;
  FInstance^.FToIdent := @FInstance^.FLine[FInstance^.FTokenPos];
  KeyHash(FInstance^.FToIdent);
  if HashKey <= PhpKeywordsMaxKeyHash then
    Result := FPhpIdentFuncTable[HashKey]
  else
    Result := stkPhpIdentifier;
  if Result = stkPhpIdentifier then
    if PhpConstComp then
      Result := stkPhpConst;
end;

function TSynWebEngine.PhpKeywordIdent: TSynWebTokenKind;
begin
  Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc14: TSynWebTokenKind;
begin
  if  PhpFunctionComp(195) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc15: TSynWebTokenKind;
begin
  if  PhpKeywordComp(45) then
    Result := stkPhpKeyword
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc16: TSynWebTokenKind;
begin
  if  PhpFunctionComp(626) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc17: TSynWebTokenKind;
begin
  if  PhpFunctionComp(644) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc18: TSynWebTokenKind;
begin
  if  PhpKeywordComp(21) then
    Result := stkPhpKeyword
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc19: TSynWebTokenKind;
begin
  if  PhpKeywordComp(6) or
      PhpKeywordComp(22) then
    Result := stkPhpKeyword
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc20: TSynWebTokenKind;
begin
  if  PhpKeywordComp(8) then
    Result := stkPhpKeyword
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc22: TSynWebTokenKind;
begin
  if  PhpFunctionComp(81) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc23: TSynWebTokenKind;
begin
  if  PhpFunctionComp(673) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc25: TSynWebTokenKind;
begin
  if  PhpFunctionComp(3252) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc28: TSynWebTokenKind;
begin
  if  PhpKeywordComp(12) then
    Result := stkPhpKeyword
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc29: TSynWebTokenKind;
begin
  if  PhpFunctionComp(257) or
      PhpFunctionComp(267) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc30: TSynWebTokenKind;
begin
  if  PhpFunctionComp(435) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc31: TSynWebTokenKind;
begin
  if  PhpKeywordComp(24) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(648) or
        PhpFunctionComp(2968) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc32: TSynWebTokenKind;
begin
  if  PhpFunctionComp(855) or
      PhpFunctionComp(871) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc33: TSynWebTokenKind;
begin
  if  PhpKeywordComp(59) then
    Result := stkPhpKeyword
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc34: TSynWebTokenKind;
begin
  if  PhpFunctionComp(915) or
      PhpFunctionComp(916) or
      PhpFunctionComp(1805) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc35: TSynWebTokenKind;
begin
  if  PhpKeywordComp(13) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(674) or
        PhpFunctionComp(1853) or
        PhpFunctionComp(4058) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc36: TSynWebTokenKind;
begin
  if  PhpKeywordComp(65) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(185) or
        PhpFunctionComp(1027) or
        PhpFunctionComp(1113) or
        PhpFunctionComp(2106) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc37: TSynWebTokenKind;
begin
  if  PhpKeywordComp(11) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(198) or
        PhpFunctionComp(221) or
        PhpFunctionComp(308) or
        PhpFunctionComp(598) or
        PhpFunctionComp(685) or
        PhpFunctionComp(2943) or
        PhpFunctionComp(3492) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc38: TSynWebTokenKind;
begin
  if  PhpKeywordComp(31) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(82) or
        PhpFunctionComp(908) or
        PhpFunctionComp(1868) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc39: TSynWebTokenKind;
begin
  if  PhpKeywordComp(41) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(592) or
        PhpFunctionComp(1285) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc40: TSynWebTokenKind;
begin
  if  PhpKeywordComp(34) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(197) or
        PhpFunctionComp(684) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc41: TSynWebTokenKind;
begin
  if  PhpKeywordComp(25) or
      PhpKeywordComp(77) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(865) or
        PhpFunctionComp(1123) or
        PhpFunctionComp(1743) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc42: TSynWebTokenKind;
begin
  if  PhpKeywordComp(39) or
      PhpKeywordComp(55) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(260) or
        PhpFunctionComp(265) or
        PhpFunctionComp(856) or
        PhpFunctionComp(3663) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc43: TSynWebTokenKind;
begin
  if  PhpKeywordComp(38) or
      PhpKeywordComp(50) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(176) or
        PhpFunctionComp(264) or
        PhpFunctionComp(601) or
        PhpFunctionComp(4059) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc44: TSynWebTokenKind;
begin
  if  PhpKeywordComp(9) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(187) or
        PhpFunctionComp(676) or
        PhpFunctionComp(1745) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc45: TSynWebTokenKind;
begin
  if  PhpKeywordComp(76) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(214) or
        PhpFunctionComp(309) or
        PhpFunctionComp(692) or
        PhpFunctionComp(3493) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc46: TSynWebTokenKind;
begin
  if  PhpFunctionComp(83) or
      PhpFunctionComp(921) or
      PhpFunctionComp(1398) or
      PhpFunctionComp(1800) or
      PhpFunctionComp(3253) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc47: TSynWebTokenKind;
begin
  if  PhpFunctionComp(216) or
      PhpFunctionComp(603) or
      PhpFunctionComp(905) or
      PhpFunctionComp(4093) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc48: TSynWebTokenKind;
begin
  if  PhpKeywordComp(19) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(1728) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc49: TSynWebTokenKind;
begin
  if  PhpKeywordComp(16) or
      PhpKeywordComp(44) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(599) or
        PhpFunctionComp(920) or
        PhpFunctionComp(1128) or
        PhpFunctionComp(2091) or
        PhpFunctionComp(2099) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc50: TSynWebTokenKind;
begin
  if  PhpFunctionComp(600) or
      PhpFunctionComp(1028) or
      PhpFunctionComp(2807) or
      PhpFunctionComp(3256) or
      PhpFunctionComp(3522) or
      PhpFunctionComp(3664) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc51: TSynWebTokenKind;
begin
  if  PhpFunctionComp(177) or
      PhpFunctionComp(199) or
      PhpFunctionComp(605) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc52: TSynWebTokenKind;
begin
  if  PhpFunctionComp(196) or
      PhpFunctionComp(263) or
      PhpFunctionComp(925) or
      PhpFunctionComp(1646) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc54: TSynWebTokenKind;
begin
  if  PhpKeywordComp(15) or
      PhpKeywordComp(40) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(281) or
        PhpFunctionComp(3292) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc55: TSynWebTokenKind;
begin
  if  PhpKeywordComp(57) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(924) or
        PhpFunctionComp(2112) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc56: TSynWebTokenKind;
begin
  if  PhpKeywordComp(26) or
      PhpKeywordComp(42) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(231) or
        PhpFunctionComp(758) or
        PhpFunctionComp(910) or
        PhpFunctionComp(3527) or
        PhpFunctionComp(3642) or
        PhpFunctionComp(3767) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc57: TSynWebTokenKind;
begin
  if  PhpKeywordComp(78) or
      PhpKeywordComp(79) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(867) or
        PhpFunctionComp(1346) or
        PhpFunctionComp(1400) or
        PhpFunctionComp(3667) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc58: TSynWebTokenKind;
begin
  if  PhpKeywordComp(36) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(691) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc59: TSynWebTokenKind;
begin
  if  PhpKeywordComp(23) or
      PhpKeywordComp(56) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(212) or
        PhpFunctionComp(307) or
        PhpFunctionComp(1098) or
        PhpFunctionComp(3501) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc60: TSynWebTokenKind;
begin
  if  PhpKeywordComp(54) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(194) or
        PhpFunctionComp(261) or
        PhpFunctionComp(819) or
        PhpFunctionComp(1126) or
        PhpFunctionComp(1802) or
        PhpFunctionComp(3502) or
        PhpFunctionComp(3797) or
        PhpFunctionComp(4108) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc61: TSynWebTokenKind;
begin
  if  PhpFunctionComp(528) or
      PhpFunctionComp(1107) or
      PhpFunctionComp(2144) or
      PhpFunctionComp(3302) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc62: TSynWebTokenKind;
begin
  if  PhpKeywordComp(29) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(1004) or
        PhpFunctionComp(1005) or
        PhpFunctionComp(3536) or
        PhpFunctionComp(3768) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc63: TSynWebTokenKind;
begin
  if  PhpKeywordComp(7) or
      PhpKeywordComp(64) or
      PhpKeywordComp(74) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(126) or
        PhpFunctionComp(266) or
        PhpFunctionComp(1127) or
        PhpFunctionComp(1266) or
        PhpFunctionComp(2691) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc64: TSynWebTokenKind;
begin
  if  PhpKeywordComp(10) or
      PhpKeywordComp(73) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(620) or
        PhpFunctionComp(1749) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc65: TSynWebTokenKind;
begin
  if  PhpFunctionComp(186) or
      PhpFunctionComp(909) or
      PhpFunctionComp(1099) or
      PhpFunctionComp(1347) or
      PhpFunctionComp(2773) or
      PhpFunctionComp(4137) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc66: TSynWebTokenKind;
begin
  if  PhpFunctionComp(524) or
      PhpFunctionComp(906) or
      PhpFunctionComp(907) or
      PhpFunctionComp(923) or
      PhpFunctionComp(1683) or
      PhpFunctionComp(3255) or
      PhpFunctionComp(4143) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc67: TSynWebTokenKind;
begin
  if  PhpFunctionComp(3529) or
      PhpFunctionComp(4106) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc68: TSynWebTokenKind;
begin
  if  PhpKeywordComp(47) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(604) or
        PhpFunctionComp(1100) or
        PhpFunctionComp(3466) or
        PhpFunctionComp(3504) or
        PhpFunctionComp(3576) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc69: TSynWebTokenKind;
begin
  if  PhpKeywordComp(20) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(532) or
        PhpFunctionComp(1095) or
        PhpFunctionComp(2785) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc70: TSynWebTokenKind;
begin
  if  PhpFunctionComp(526) or
      PhpFunctionComp(891) or
      PhpFunctionComp(2977) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc71: TSynWebTokenKind;
begin
  if  PhpKeywordComp(17) or
      PhpKeywordComp(28) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(86) or
        PhpFunctionComp(298) or
        PhpFunctionComp(2113) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc72: TSynWebTokenKind;
begin
  if  PhpKeywordComp(53) or
      PhpKeywordComp(69) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(864) or
        PhpFunctionComp(1408) or
        PhpFunctionComp(1715) or
        PhpFunctionComp(1809) or
        PhpFunctionComp(1810) or
        PhpFunctionComp(3538) or
        PhpFunctionComp(3712) or
        PhpFunctionComp(3866) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc73: TSynWebTokenKind;
begin
  if  PhpFunctionComp(178) or
      PhpFunctionComp(297) or
      PhpFunctionComp(310) or
      PhpFunctionComp(525) or
      PhpFunctionComp(1006) or
      PhpFunctionComp(1109) or
      PhpFunctionComp(1403) or
      PhpFunctionComp(3534) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc74: TSynWebTokenKind;
begin
  if  PhpFunctionComp(299) or
      PhpFunctionComp(1386) or
      PhpFunctionComp(1434) or
      PhpFunctionComp(1614) or
      PhpFunctionComp(3517) or
      PhpFunctionComp(3537) or
      PhpFunctionComp(3766) or
      PhpFunctionComp(4140) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc75: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1399) or
      PhpFunctionComp(1405) or
      PhpFunctionComp(1750) or
      PhpFunctionComp(2695) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc76: TSynWebTokenKind;
begin
  if  PhpFunctionComp(868) or
      PhpFunctionComp(3722) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc77: TSynWebTokenKind;
begin
  if  PhpKeywordComp(61) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(1650) or
        PhpFunctionComp(3303) or
        PhpFunctionComp(3658) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc78: TSynWebTokenKind;
begin
  if  PhpKeywordComp(51) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(230) or
        PhpFunctionComp(1427) or
        PhpFunctionComp(1648) or
        PhpFunctionComp(1807) or
        PhpFunctionComp(3548) or
        PhpFunctionComp(4153) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc79: TSynWebTokenKind;
begin
  if  PhpKeywordComp(27) or
      PhpKeywordComp(30) or
      PhpKeywordComp(75) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(215) or
        PhpFunctionComp(268) or
        PhpFunctionComp(533) or
        PhpFunctionComp(651) or
        PhpFunctionComp(878) or
        PhpFunctionComp(1097) or
        PhpFunctionComp(1428) or
        PhpFunctionComp(1752) or
        PhpFunctionComp(3301) or
        PhpFunctionComp(4146) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc80: TSynWebTokenKind;
begin
  if  PhpKeywordComp(33) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(591) or
        PhpFunctionComp(875) or
        PhpFunctionComp(2818) or
        PhpFunctionComp(3665) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc81: TSynWebTokenKind;
begin
  if  PhpKeywordComp(52) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(695) or
        PhpFunctionComp(882) or
        PhpFunctionComp(965) or
        PhpFunctionComp(2854) or
        PhpFunctionComp(2878) or
        PhpFunctionComp(3521) or
        PhpFunctionComp(4103) or
        PhpFunctionComp(4142) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc82: TSynWebTokenKind;
begin
  if  PhpKeywordComp(71) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(183) or
        PhpFunctionComp(223) or
        PhpFunctionComp(390) or
        PhpFunctionComp(866) or
        PhpFunctionComp(876) or
        PhpFunctionComp(914) or
        PhpFunctionComp(1031) or
        PhpFunctionComp(1110) or
        PhpFunctionComp(4061) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc83: TSynWebTokenKind;
begin
  if  PhpFunctionComp(388) or
      PhpFunctionComp(686) or
      PhpFunctionComp(1017) or
      PhpFunctionComp(1104) or
      PhpFunctionComp(1747) or
      PhpFunctionComp(2853) or
      PhpFunctionComp(3336) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc84: TSynWebTokenKind;
begin
  if  PhpKeywordComp(5) or
      PhpKeywordComp(72) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(1101) or
        PhpFunctionComp(1202) or
        PhpFunctionComp(2879) or
        PhpFunctionComp(3250) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc85: TSynWebTokenKind;
begin
  if  PhpFunctionComp(282) or
      PhpFunctionComp(3000) or
      PhpFunctionComp(3723) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc86: TSynWebTokenKind;
begin
  if  PhpFunctionComp(500) or
      PhpFunctionComp(696) or
      PhpFunctionComp(1096) or
      PhpFunctionComp(2235) or
      PhpFunctionComp(2798) or
      PhpFunctionComp(3463) or
      PhpFunctionComp(3879) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc87: TSynWebTokenKind;
begin
  if  PhpKeywordComp(70) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(1042) or
        PhpFunctionComp(1093) or
        PhpFunctionComp(1146) or
        PhpFunctionComp(1371) or
        PhpFunctionComp(1971) or
        PhpFunctionComp(4148) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc88: TSynWebTokenKind;
begin
  if  PhpFunctionComp(283) or
      PhpFunctionComp(314) or
      PhpFunctionComp(2791) or
      PhpFunctionComp(3937) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc89: TSynWebTokenKind;
begin
  if  PhpFunctionComp(120) or
      PhpFunctionComp(904) or
      PhpFunctionComp(912) or
      PhpFunctionComp(1433) or
      PhpFunctionComp(1806) or
      PhpFunctionComp(2715) or
      PhpFunctionComp(2976) or
      PhpFunctionComp(3880) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc90: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1013) or
      PhpFunctionComp(1801) or
      PhpFunctionComp(1804) or
      PhpFunctionComp(2792) or
      PhpFunctionComp(2925) or
      PhpFunctionComp(3547) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc91: TSynWebTokenKind;
begin
  if  PhpKeywordComp(37) or
      PhpKeywordComp(62) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(174) or
        PhpFunctionComp(213) or
        PhpFunctionComp(217) or
        PhpFunctionComp(698) or
        PhpFunctionComp(889) or
        PhpFunctionComp(1029) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc92: TSynWebTokenKind;
begin
  if  PhpFunctionComp(85) or
      PhpFunctionComp(879) or
      PhpFunctionComp(966) or
      PhpFunctionComp(1032) or
      PhpFunctionComp(2946) or
      PhpFunctionComp(3639) or
      PhpFunctionComp(3956) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc93: TSynWebTokenKind;
begin
  if  PhpKeywordComp(66) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(241) or
        PhpFunctionComp(610) or
        PhpFunctionComp(3503) or
        PhpFunctionComp(4154) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc94: TSynWebTokenKind;
begin
  if  PhpFunctionComp(229) or
      PhpFunctionComp(507) or
      PhpFunctionComp(642) or
      PhpFunctionComp(836) or
      PhpFunctionComp(1366) or
      PhpFunctionComp(1700) or
      PhpFunctionComp(3349) or
      PhpFunctionComp(3676) or
      PhpFunctionComp(4109) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc95: TSynWebTokenKind;
begin
  if  PhpFunctionComp(84) or
      PhpFunctionComp(498) or
      PhpFunctionComp(1030) or
      PhpFunctionComp(1294) or
      PhpFunctionComp(2300) or
      PhpFunctionComp(3955) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc96: TSynWebTokenKind;
begin
  if  PhpKeywordComp(68) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(175) or
        PhpFunctionComp(647) or
        PhpFunctionComp(1689) or
        PhpFunctionComp(1981) or
        PhpFunctionComp(4110) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc97: TSynWebTokenKind;
begin
  if  PhpFunctionComp(235) or
      PhpFunctionComp(1692) or
      PhpFunctionComp(2097) or
      PhpFunctionComp(3155) or
      PhpFunctionComp(4056) or
      PhpFunctionComp(4149) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc98: TSynWebTokenKind;
begin
  if  PhpFunctionComp(226) or
      PhpFunctionComp(497) or
      PhpFunctionComp(890) or
      PhpFunctionComp(1026) or
      PhpFunctionComp(1117) or
      PhpFunctionComp(1784) or
      PhpFunctionComp(2821) or
      PhpFunctionComp(3435) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc99: TSynWebTokenKind;
begin
  if  PhpFunctionComp(108) or
      PhpFunctionComp(420) or
      PhpFunctionComp(522) or
      PhpFunctionComp(1015) or
      PhpFunctionComp(1052) or
      PhpFunctionComp(1753) or
      PhpFunctionComp(3001) or
      PhpFunctionComp(3149) or
      PhpFunctionComp(3881) or
      PhpFunctionComp(3957) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc100: TSynWebTokenKind;
begin
  if  PhpFunctionComp(287) or
      PhpFunctionComp(291) or
      PhpFunctionComp(841) or
      PhpFunctionComp(1103) or
      PhpFunctionComp(2781) or
      PhpFunctionComp(2917) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc101: TSynWebTokenKind;
begin
  if  PhpKeywordComp(18) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(1024) or
        PhpFunctionComp(1642) or
        PhpFunctionComp(1705) or
        PhpFunctionComp(1746) or
        PhpFunctionComp(2783) or
        PhpFunctionComp(2802) or
        PhpFunctionComp(4057) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc102: TSynWebTokenKind;
begin
  if  PhpKeywordComp(43) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(227) or
        PhpFunctionComp(1348) or
        PhpFunctionComp(1649) or
        PhpFunctionComp(1803) or
        PhpFunctionComp(1808) or
        PhpFunctionComp(2733) or
        PhpFunctionComp(3047) or
        PhpFunctionComp(3638) or
        PhpFunctionComp(3713) or
        PhpFunctionComp(3724) or
        PhpFunctionComp(3946) or
        PhpFunctionComp(4376) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc103: TSynWebTokenKind;
begin
  if  PhpFunctionComp(232) or
      PhpFunctionComp(262) or
      PhpFunctionComp(384) or
      PhpFunctionComp(434) or
      PhpFunctionComp(838) or
      PhpFunctionComp(881) or
      PhpFunctionComp(918) or
      PhpFunctionComp(1021) or
      PhpFunctionComp(1041) or
      PhpFunctionComp(1102) or
      PhpFunctionComp(1190) or
      PhpFunctionComp(1722) or
      PhpFunctionComp(3013) or
      PhpFunctionComp(3091) or
      PhpFunctionComp(3941) or
      PhpFunctionComp(3951) or
      PhpFunctionComp(4053) or
      PhpFunctionComp(4111) or
      PhpFunctionComp(4187) or
      PhpFunctionComp(4375) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc104: TSynWebTokenKind;
begin
  if  PhpFunctionComp(505) or
      PhpFunctionComp(510) or
      PhpFunctionComp(530) or
      PhpFunctionComp(922) or
      PhpFunctionComp(1016) or
      PhpFunctionComp(1035) or
      PhpFunctionComp(1395) or
      PhpFunctionComp(2796) or
      PhpFunctionComp(3535) or
      PhpFunctionComp(3583) or
      PhpFunctionComp(3942) or
      PhpFunctionComp(3945) or
      PhpFunctionComp(4136) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc105: TSynWebTokenKind;
begin
  if  PhpKeywordComp(14) or
      PhpKeywordComp(32) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(325) or
        PhpFunctionComp(511) or
        PhpFunctionComp(606) or
        PhpFunctionComp(611) or
        PhpFunctionComp(823) or
        PhpFunctionComp(1012) or
        PhpFunctionComp(1034) or
        PhpFunctionComp(1050) or
        PhpFunctionComp(1115) or
        PhpFunctionComp(1387) or
        PhpFunctionComp(1392) or
        PhpFunctionComp(2104) or
        PhpFunctionComp(3075) or
        PhpFunctionComp(4205) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc106: TSynWebTokenKind;
begin
  if  PhpKeywordComp(49) or
      PhpKeywordComp(63) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(116) or
        PhpFunctionComp(284) or
        PhpFunctionComp(303) or
        PhpFunctionComp(317) or
        PhpFunctionComp(346) or
        PhpFunctionComp(1054) or
        PhpFunctionComp(1108) or
        PhpFunctionComp(1550) or
        PhpFunctionComp(2740) or
        PhpFunctionComp(2800) or
        PhpFunctionComp(3217) or
        PhpFunctionComp(3579) or
        PhpFunctionComp(3679) or
        PhpFunctionComp(3949) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc107: TSynWebTokenKind;
begin
  if  PhpFunctionComp(706) or
      PhpFunctionComp(822) or
      PhpFunctionComp(880) or
      PhpFunctionComp(913) or
      PhpFunctionComp(1020) or
      PhpFunctionComp(1651) or
      PhpFunctionComp(2227) or
      PhpFunctionComp(2414) or
      PhpFunctionComp(3236) or
      PhpFunctionComp(3577) or
      PhpFunctionComp(3943) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc108: TSynWebTokenKind;
begin
  if  PhpFunctionComp(107) or
      PhpFunctionComp(589) or
      PhpFunctionComp(960) or
      PhpFunctionComp(1036) or
      PhpFunctionComp(1112) or
      PhpFunctionComp(1663) or
      PhpFunctionComp(1696) or
      PhpFunctionComp(2916) or
      PhpFunctionComp(2928) or
      PhpFunctionComp(3350) or
      PhpFunctionComp(4125) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc109: TSynWebTokenKind;
begin
  if  PhpFunctionComp(462) or
      PhpFunctionComp(877) or
      PhpFunctionComp(1106) or
      PhpFunctionComp(1686) or
      PhpFunctionComp(1720) or
      PhpFunctionComp(3086) or
      PhpFunctionComp(3680) or
      PhpFunctionComp(3882) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc110: TSynWebTokenKind;
begin
  if  PhpFunctionComp(509) or
      PhpFunctionComp(1608) or
      PhpFunctionComp(2784) or
      PhpFunctionComp(3090) or
      PhpFunctionComp(3641) or
      PhpFunctionComp(3931) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc111: TSynWebTokenKind;
begin
  if  PhpKeywordComp(35) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(701) or
        PhpFunctionComp(936) or
        PhpFunctionComp(1188) or
        PhpFunctionComp(1455) or
        PhpFunctionComp(1578) or
        PhpFunctionComp(1669) or
        PhpFunctionComp(1698) or
        PhpFunctionComp(3150) or
        PhpFunctionComp(3366) or
        PhpFunctionComp(4186) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc112: TSynWebTokenKind;
begin
  if  PhpFunctionComp(296) or
      PhpFunctionComp(338) or
      PhpFunctionComp(1182) or
      PhpFunctionComp(3355) or
      PhpFunctionComp(3385) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc113: TSynWebTokenKind;
begin
  if  PhpFunctionComp(344) or
      PhpFunctionComp(870) or
      PhpFunctionComp(1048) or
      PhpFunctionComp(1385) or
      PhpFunctionComp(1410) or
      PhpFunctionComp(1597) or
      PhpFunctionComp(1645) or
      PhpFunctionComp(1667) or
      PhpFunctionComp(1688) or
      PhpFunctionComp(1699) or
      PhpFunctionComp(1704) or
      PhpFunctionComp(2929) or
      PhpFunctionComp(3012) or
      PhpFunctionComp(4359) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc114: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1200) or
      PhpFunctionComp(1418) or
      PhpFunctionComp(1821) or
      PhpFunctionComp(3033) or
      PhpFunctionComp(3140) or
      PhpFunctionComp(3376) or
      PhpFunctionComp(3950) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc115: TSynWebTokenKind;
begin
  if  PhpFunctionComp(218) or
      PhpFunctionComp(527) or
      PhpFunctionComp(609) or
      PhpFunctionComp(859) or
      PhpFunctionComp(1068) or
      PhpFunctionComp(1116) or
      PhpFunctionComp(2693) or
      PhpFunctionComp(2810) or
      PhpFunctionComp(3205) or
      PhpFunctionComp(3871) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc116: TSynWebTokenKind;
begin
  if  PhpFunctionComp(316) or
      PhpFunctionComp(334) or
      PhpFunctionComp(933) or
      PhpFunctionComp(1011) or
      PhpFunctionComp(1261) or
      PhpFunctionComp(1383) or
      PhpFunctionComp(1725) or
      PhpFunctionComp(1732) or
      PhpFunctionComp(2047) or
      PhpFunctionComp(2772) or
      PhpFunctionComp(3934) or
      PhpFunctionComp(4396) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc117: TSynWebTokenKind;
begin
  if  PhpFunctionComp(934) or
      PhpFunctionComp(1003) or
      PhpFunctionComp(1014) or
      PhpFunctionComp(1569) or
      PhpFunctionComp(1721) or
      PhpFunctionComp(1726) or
      PhpFunctionComp(2940) or
      PhpFunctionComp(3014) or
      PhpFunctionComp(3465) or
      PhpFunctionComp(3878) or
      PhpFunctionComp(4141) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc118: TSynWebTokenKind;
begin
  if  PhpFunctionComp(521) or
      PhpFunctionComp(1343) or
      PhpFunctionComp(1511) or
      PhpFunctionComp(2092) or
      PhpFunctionComp(2141) or
      PhpFunctionComp(2734) or
      PhpFunctionComp(3249) or
      PhpFunctionComp(3381) or
      PhpFunctionComp(3646) or
      PhpFunctionComp(4230) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc119: TSynWebTokenKind;
begin
  if  PhpFunctionComp(114) or
      PhpFunctionComp(279) or
      PhpFunctionComp(436) or
      PhpFunctionComp(607) or
      PhpFunctionComp(627) or
      PhpFunctionComp(901) or
      PhpFunctionComp(1051) or
      PhpFunctionComp(1351) or
      PhpFunctionComp(1694) or
      PhpFunctionComp(1707) or
      PhpFunctionComp(1838) or
      PhpFunctionComp(2941) or
      PhpFunctionComp(3021) or
      PhpFunctionComp(3384) or
      PhpFunctionComp(3448) or
      PhpFunctionComp(3523) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc120: TSynWebTokenKind;
begin
  if  PhpFunctionComp(318) or
      PhpFunctionComp(514) or
      PhpFunctionComp(534) or
      PhpFunctionComp(608) or
      PhpFunctionComp(893) or
      PhpFunctionComp(1254) or
      PhpFunctionComp(1417) or
      PhpFunctionComp(1419) or
      PhpFunctionComp(1432) or
      PhpFunctionComp(1681) or
      PhpFunctionComp(1703) or
      PhpFunctionComp(1748) or
      PhpFunctionComp(1879) or
      PhpFunctionComp(1987) or
      PhpFunctionComp(2016) or
      PhpFunctionComp(2718) or
      PhpFunctionComp(2852) or
      PhpFunctionComp(3135) or
      PhpFunctionComp(3254) or
      PhpFunctionComp(3339) or
      PhpFunctionComp(3944) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc121: TSynWebTokenKind;
begin
  if  PhpFunctionComp(151) or
      PhpFunctionComp(253) or
      PhpFunctionComp(581) or
      PhpFunctionComp(625) or
      PhpFunctionComp(928) or
      PhpFunctionComp(951) or
      PhpFunctionComp(1345) or
      PhpFunctionComp(1729) or
      PhpFunctionComp(1758) or
      PhpFunctionComp(2936) or
      PhpFunctionComp(3123) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc122: TSynWebTokenKind;
begin
  if  PhpFunctionComp(246) or
      PhpFunctionComp(513) or
      PhpFunctionComp(590) or
      PhpFunctionComp(926) or
      PhpFunctionComp(929) or
      PhpFunctionComp(949) or
      PhpFunctionComp(1064) or
      PhpFunctionComp(1143) or
      PhpFunctionComp(1401) or
      PhpFunctionComp(1406) or
      PhpFunctionComp(1425) or
      PhpFunctionComp(1548) or
      PhpFunctionComp(1588) or
      PhpFunctionComp(1662) or
      PhpFunctionComp(2801) or
      PhpFunctionComp(2975) or
      PhpFunctionComp(3184) or
      PhpFunctionComp(3354) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc123: TSynWebTokenKind;
begin
  if  PhpFunctionComp(531) or
      PhpFunctionComp(825) or
      PhpFunctionComp(927) or
      PhpFunctionComp(978) or
      PhpFunctionComp(1616) or
      PhpFunctionComp(1687) or
      PhpFunctionComp(1724) or
      PhpFunctionComp(1727) or
      PhpFunctionComp(1824) or
      PhpFunctionComp(2764) or
      PhpFunctionComp(2809) or
      PhpFunctionComp(3362) or
      PhpFunctionComp(3936) or
      PhpFunctionComp(4382) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc124: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1430) or
      PhpFunctionComp(1467) or
      PhpFunctionComp(1566) or
      PhpFunctionComp(1690) or
      PhpFunctionComp(1701) or
      PhpFunctionComp(1754) or
      PhpFunctionComp(1787) or
      PhpFunctionComp(1816) or
      PhpFunctionComp(1849) or
      PhpFunctionComp(2797) or
      PhpFunctionComp(3228) or
      PhpFunctionComp(3497) or
      PhpFunctionComp(3939) or
      PhpFunctionComp(4063) or
      PhpFunctionComp(4206) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc125: TSynWebTokenKind;
begin
  if  PhpFunctionComp(99) or
      PhpFunctionComp(131) or
      PhpFunctionComp(502) or
      PhpFunctionComp(612) or
      PhpFunctionComp(683) or
      PhpFunctionComp(1045) or
      PhpFunctionComp(1119) or
      PhpFunctionComp(1424) or
      PhpFunctionComp(1468) or
      PhpFunctionComp(1641) or
      PhpFunctionComp(1906) or
      PhpFunctionComp(1909) or
      PhpFunctionComp(2918) or
      PhpFunctionComp(3117) or
      PhpFunctionComp(3238) or
      PhpFunctionComp(3643) or
      PhpFunctionComp(3948) or
      PhpFunctionComp(4007) or
      PhpFunctionComp(4062) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc126: TSynWebTokenKind;
begin
  if  PhpKeywordComp(46) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(228) or
        PhpFunctionComp(249) or
        PhpFunctionComp(444) or
        PhpFunctionComp(454) or
        PhpFunctionComp(707) or
        PhpFunctionComp(1010) or
        PhpFunctionComp(1587) or
        PhpFunctionComp(1668) or
        PhpFunctionComp(1785) or
        PhpFunctionComp(2228) or
        PhpFunctionComp(3356) or
        PhpFunctionComp(4367) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc127: TSynWebTokenKind;
begin
  if  PhpFunctionComp(618) or
      PhpFunctionComp(826) or
      PhpFunctionComp(1057) or
      PhpFunctionComp(1114) or
      PhpFunctionComp(1207) or
      PhpFunctionComp(1217) or
      PhpFunctionComp(1529) or
      PhpFunctionComp(1555) or
      PhpFunctionComp(1917) or
      PhpFunctionComp(2094) or
      PhpFunctionComp(2786) or
      PhpFunctionComp(3241) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc128: TSynWebTokenKind;
begin
  if  PhpFunctionComp(188) or
      PhpFunctionComp(406) or
      PhpFunctionComp(911) or
      PhpFunctionComp(1198) or
      PhpFunctionComp(1553) or
      PhpFunctionComp(1673) or
      PhpFunctionComp(1684) or
      PhpFunctionComp(1693) or
      PhpFunctionComp(1997) or
      PhpFunctionComp(3120) or
      PhpFunctionComp(3295) or
      PhpFunctionComp(3439) or
      PhpFunctionComp(3494) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc129: TSynWebTokenKind;
begin
  if  PhpFunctionComp(503) or
      PhpFunctionComp(939) or
      PhpFunctionComp(989) or
      PhpFunctionComp(1674) or
      PhpFunctionComp(3119) or
      PhpFunctionComp(3173) or
      PhpFunctionComp(3218) or
      PhpFunctionComp(3293) or
      PhpFunctionComp(3498) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc130: TSynWebTokenKind;
begin
  if  PhpFunctionComp(147) or
      PhpFunctionComp(190) or
      PhpFunctionComp(712) or
      PhpFunctionComp(722) or
      PhpFunctionComp(932) or
      PhpFunctionComp(1025) or
      PhpFunctionComp(1069) or
      PhpFunctionComp(1070) or
      PhpFunctionComp(1289) or
      PhpFunctionComp(1396) or
      PhpFunctionComp(1518) or
      PhpFunctionComp(1552) or
      PhpFunctionComp(1577) or
      PhpFunctionComp(1599) or
      PhpFunctionComp(1774) or
      PhpFunctionComp(2575) or
      PhpFunctionComp(2782) or
      PhpFunctionComp(2937) or
      PhpFunctionComp(3644) or
      PhpFunctionComp(3645) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc131: TSynWebTokenKind;
begin
  if  PhpFunctionComp(378) or
      PhpFunctionComp(496) or
      PhpFunctionComp(682) or
      PhpFunctionComp(1730) or
      PhpFunctionComp(2640) or
      PhpFunctionComp(3046) or
      PhpFunctionComp(3248) or
      PhpFunctionComp(3436) or
      PhpFunctionComp(3796) or
      PhpFunctionComp(3940) or
      PhpFunctionComp(4160) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc132: TSynWebTokenKind;
begin
  if  PhpFunctionComp(179) or
      PhpFunctionComp(529) or
      PhpFunctionComp(675) or
      PhpFunctionComp(956) or
      PhpFunctionComp(1184) or
      PhpFunctionComp(1440) or
      PhpFunctionComp(1545) or
      PhpFunctionComp(1590) or
      PhpFunctionComp(1606) or
      PhpFunctionComp(2048) or
      PhpFunctionComp(2729) or
      PhpFunctionComp(2932) or
      PhpFunctionComp(3304) or
      PhpFunctionComp(3636) or
      PhpFunctionComp(4157) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc133: TSynWebTokenKind;
begin
  if  PhpFunctionComp(295) or
      PhpFunctionComp(596) or
      PhpFunctionComp(930) or
      PhpFunctionComp(1464) or
      PhpFunctionComp(1466) or
      PhpFunctionComp(1794) or
      PhpFunctionComp(1907) or
      PhpFunctionComp(2596) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc134: TSynWebTokenKind;
begin
  if  PhpFunctionComp(111) or
      PhpFunctionComp(242) or
      PhpFunctionComp(256) or
      PhpFunctionComp(861) or
      PhpFunctionComp(862) or
      PhpFunctionComp(940) or
      PhpFunctionComp(1664) or
      PhpFunctionComp(1685) or
      PhpFunctionComp(1793) or
      PhpFunctionComp(2814) or
      PhpFunctionComp(2933) or
      PhpFunctionComp(3142) or
      PhpFunctionComp(3403) or
      PhpFunctionComp(3462) or
      PhpFunctionComp(3496) or
      PhpFunctionComp(3545) or
      PhpFunctionComp(3947) or
      PhpFunctionComp(4365) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc135: TSynWebTokenKind;
begin
  if  PhpFunctionComp(245) or
      PhpFunctionComp(382) or
      PhpFunctionComp(597) or
      PhpFunctionComp(645) or
      PhpFunctionComp(863) or
      PhpFunctionComp(953) or
      PhpFunctionComp(1873) or
      PhpFunctionComp(2413) or
      PhpFunctionComp(2815) or
      PhpFunctionComp(2819) or
      PhpFunctionComp(2847) or
      PhpFunctionComp(4105) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc136: TSynWebTokenKind;
begin
  if  PhpFunctionComp(224) or
      PhpFunctionComp(247) or
      PhpFunctionComp(858) or
      PhpFunctionComp(938) or
      PhpFunctionComp(948) or
      PhpFunctionComp(1043) or
      PhpFunctionComp(1196) or
      PhpFunctionComp(1571) or
      PhpFunctionComp(1573) or
      PhpFunctionComp(1601) or
      PhpFunctionComp(1711) or
      PhpFunctionComp(3654) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc137: TSynWebTokenKind;
begin
  if  PhpFunctionComp(155) or
      PhpFunctionComp(902) or
      PhpFunctionComp(947) or
      PhpFunctionComp(1033) or
      PhpFunctionComp(1061) or
      PhpFunctionComp(1680) or
      PhpFunctionComp(2152) or
      PhpFunctionComp(2735) or
      PhpFunctionComp(3204) or
      PhpFunctionComp(3406) or
      PhpFunctionComp(3681) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc138: TSynWebTokenKind;
begin
  if  PhpFunctionComp(258) or
      PhpFunctionComp(582) or
      PhpFunctionComp(957) or
      PhpFunctionComp(1060) or
      PhpFunctionComp(1062) or
      PhpFunctionComp(2736) or
      PhpFunctionComp(2738) or
      PhpFunctionComp(2768) or
      PhpFunctionComp(2775) or
      PhpFunctionComp(3069) or
      PhpFunctionComp(3405) or
      PhpFunctionComp(3542) or
      PhpFunctionComp(3697) or
      PhpFunctionComp(4369) or
      PhpFunctionComp(4384) or
      PhpFunctionComp(4395) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc139: TSynWebTokenKind;
begin
  if  PhpFunctionComp(136) or
      PhpFunctionComp(236) or
      PhpFunctionComp(442) or
      PhpFunctionComp(588) or
      PhpFunctionComp(827) or
      PhpFunctionComp(1263) or
      PhpFunctionComp(1409) or
      PhpFunctionComp(1416) or
      PhpFunctionComp(1465) or
      PhpFunctionComp(2050) or
      PhpFunctionComp(2672) or
      PhpFunctionComp(2979) or
      PhpFunctionComp(3102) or
      PhpFunctionComp(3201) or
      PhpFunctionComp(3219) or
      PhpFunctionComp(3340) or
      PhpFunctionComp(3499) or
      PhpFunctionComp(3540) or
      PhpFunctionComp(3580) or
      PhpFunctionComp(3683) or
      PhpFunctionComp(3895) or
      PhpFunctionComp(3953) or
      PhpFunctionComp(4145) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc140: TSynWebTokenKind;
begin
  if  PhpFunctionComp(113) or
      PhpFunctionComp(189) or
      PhpFunctionComp(440) or
      PhpFunctionComp(499) or
      PhpFunctionComp(593) or
      PhpFunctionComp(615) or
      PhpFunctionComp(1058) or
      PhpFunctionComp(1120) or
      PhpFunctionComp(1344) or
      PhpFunctionComp(1457) or
      PhpFunctionComp(1672) or
      PhpFunctionComp(1761) or
      PhpFunctionComp(2002) or
      PhpFunctionComp(2055) or
      PhpFunctionComp(2841) or
      PhpFunctionComp(3206) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc141: TSynWebTokenKind;
begin
  if  PhpFunctionComp(331) or
      PhpFunctionComp(535) or
      PhpFunctionComp(583) or
      PhpFunctionComp(584) or
      PhpFunctionComp(677) or
      PhpFunctionComp(900) or
      PhpFunctionComp(954) or
      PhpFunctionComp(1094) or
      PhpFunctionComp(1141) or
      PhpFunctionComp(1224) or
      PhpFunctionComp(1384) or
      PhpFunctionComp(1404) or
      PhpFunctionComp(1531) or
      PhpFunctionComp(1539) or
      PhpFunctionComp(1731) or
      PhpFunctionComp(1755) or
      PhpFunctionComp(1934) or
      PhpFunctionComp(2694) or
      PhpFunctionComp(3200) or
      PhpFunctionComp(3908) or
      PhpFunctionComp(4161) or
      PhpFunctionComp(4383) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc142: TSynWebTokenKind;
begin
  if  PhpKeywordComp(48) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(366) or
        PhpFunctionComp(689) or
        PhpFunctionComp(699) or
        PhpFunctionComp(857) or
        PhpFunctionComp(935) or
        PhpFunctionComp(1144) or
        PhpFunctionComp(1157) or
        PhpFunctionComp(1361) or
        PhpFunctionComp(1367) or
        PhpFunctionComp(1445) or
        PhpFunctionComp(1500) or
        PhpFunctionComp(1510) or
        PhpFunctionComp(1543) or
        PhpFunctionComp(1778) or
        PhpFunctionComp(1790) or
        PhpFunctionComp(2059) or
        PhpFunctionComp(3089) or
        PhpFunctionComp(3240) or
        PhpFunctionComp(3251) or
        PhpFunctionComp(4350) or
        PhpFunctionComp(4355) or
        PhpFunctionComp(4374) or
        PhpFunctionComp(4387) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc143: TSynWebTokenKind;
begin
  if  PhpFunctionComp(139) or
      PhpFunctionComp(408) or
      PhpFunctionComp(977) or
      PhpFunctionComp(1049) or
      PhpFunctionComp(1092) or
      PhpFunctionComp(1358) or
      PhpFunctionComp(1437) or
      PhpFunctionComp(1697) or
      PhpFunctionComp(1723) or
      PhpFunctionComp(2056) or
      PhpFunctionComp(2139) or
      PhpFunctionComp(3156) or
      PhpFunctionComp(3298) or
      PhpFunctionComp(3337) or
      PhpFunctionComp(3725) or
      PhpFunctionComp(4353) or
      PhpFunctionComp(4370) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc144: TSynWebTokenKind;
begin
  if  PhpFunctionComp(97) or
      PhpFunctionComp(181) or
      PhpFunctionComp(289) or
      PhpFunctionComp(392) or
      PhpFunctionComp(541) or
      PhpFunctionComp(729) or
      PhpFunctionComp(1155) or
      PhpFunctionComp(1199) or
      PhpFunctionComp(1584) or
      PhpFunctionComp(1756) or
      PhpFunctionComp(1762) or
      PhpFunctionComp(1839) or
      PhpFunctionComp(2822) or
      PhpFunctionComp(2922) or
      PhpFunctionComp(3174) or
      PhpFunctionComp(3640) or
      PhpFunctionComp(4159) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc145: TSynWebTokenKind;
begin
  if  PhpFunctionComp(225) or
      PhpFunctionComp(342) or
      PhpFunctionComp(402) or
      PhpFunctionComp(457) or
      PhpFunctionComp(679) or
      PhpFunctionComp(899) or
      PhpFunctionComp(1252) or
      PhpFunctionComp(1421) or
      PhpFunctionComp(1547) or
      PhpFunctionComp(2777) or
      PhpFunctionComp(2926) or
      PhpFunctionComp(2939) or
      PhpFunctionComp(3185) or
      PhpFunctionComp(3678) or
      PhpFunctionComp(4123) or
      PhpFunctionComp(4251) or
      PhpFunctionComp(4362) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc146: TSynWebTokenKind;
begin
  if  PhpFunctionComp(163) or
      PhpFunctionComp(460) or
      PhpFunctionComp(488) or
      PhpFunctionComp(572) or
      PhpFunctionComp(771) or
      PhpFunctionComp(869) or
      PhpFunctionComp(950) or
      PhpFunctionComp(1218) or
      PhpFunctionComp(1493) or
      PhpFunctionComp(2191) or
      PhpFunctionComp(2231) or
      PhpFunctionComp(2603) or
      PhpFunctionComp(3104) or
      PhpFunctionComp(3221) or
      PhpFunctionComp(4163) or
      PhpFunctionComp(4324) or
      PhpFunctionComp(4337) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc147: TSynWebTokenKind;
begin
  if  PhpFunctionComp(152) or
      PhpFunctionComp(172) or
      PhpFunctionComp(243) or
      PhpFunctionComp(641) or
      PhpFunctionComp(764) or
      PhpFunctionComp(1044) or
      PhpFunctionComp(1065) or
      PhpFunctionComp(1201) or
      PhpFunctionComp(1220) or
      PhpFunctionComp(1245) or
      PhpFunctionComp(1393) or
      PhpFunctionComp(1420) or
      PhpFunctionComp(1484) or
      PhpFunctionComp(1515) or
      PhpFunctionComp(2739) or
      PhpFunctionComp(2971) or
      PhpFunctionComp(2978) or
      PhpFunctionComp(3373) or
      PhpFunctionComp(3418) or
      PhpFunctionComp(3518) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc148: TSynWebTokenKind;
begin
  if  PhpFunctionComp(148) or
      PhpFunctionComp(160) or
      PhpFunctionComp(327) or
      PhpFunctionComp(506) or
      PhpFunctionComp(756) or
      PhpFunctionComp(897) or
      PhpFunctionComp(1007) or
      PhpFunctionComp(1063) or
      PhpFunctionComp(1603) or
      PhpFunctionComp(1682) or
      PhpFunctionComp(1702) or
      PhpFunctionComp(1775) or
      PhpFunctionComp(1825) or
      PhpFunctionComp(3394) or
      PhpFunctionComp(3578) or
      PhpFunctionComp(4147) or
      PhpFunctionComp(4176) or
      PhpFunctionComp(4378) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc149: TSynWebTokenKind;
begin
  if  PhpFunctionComp(290) or
      PhpFunctionComp(367) or
      PhpFunctionComp(617) or
      PhpFunctionComp(723) or
      PhpFunctionComp(1234) or
      PhpFunctionComp(1592) or
      PhpFunctionComp(2716) or
      PhpFunctionComp(2725) or
      PhpFunctionComp(3511) or
      PhpFunctionComp(3774) or
      PhpFunctionComp(4124) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc150: TSynWebTokenKind;
begin
  if  PhpFunctionComp(399) or
      PhpFunctionComp(725) or
      PhpFunctionComp(750) or
      PhpFunctionComp(1208) or
      PhpFunctionComp(1290) or
      PhpFunctionComp(1295) or
      PhpFunctionComp(1372) or
      PhpFunctionComp(1431) or
      PhpFunctionComp(1498) or
      PhpFunctionComp(1617) or
      PhpFunctionComp(1618) or
      PhpFunctionComp(2938) or
      PhpFunctionComp(3216) or
      PhpFunctionComp(3609) or
      PhpFunctionComp(4379) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc151: TSynWebTokenKind;
begin
  if  PhpFunctionComp(423) or
      PhpFunctionComp(483) or
      PhpFunctionComp(843) or
      PhpFunctionComp(1046) or
      PhpFunctionComp(1089) or
      PhpFunctionComp(1356) or
      PhpFunctionComp(1439) or
      PhpFunctionComp(1507) or
      PhpFunctionComp(1544) or
      PhpFunctionComp(1695) or
      PhpFunctionComp(1886) or
      PhpFunctionComp(2013) or
      PhpFunctionComp(2612) or
      PhpFunctionComp(2639) or
      PhpFunctionComp(2840) or
      PhpFunctionComp(3052) or
      PhpFunctionComp(3125) or
      PhpFunctionComp(3383) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc152: TSynWebTokenKind;
begin
  if  PhpFunctionComp(87) or
      PhpFunctionComp(429) or
      PhpFunctionComp(739) or
      PhpFunctionComp(1183) or
      PhpFunctionComp(1242) or
      PhpFunctionComp(1397) or
      PhpFunctionComp(1438) or
      PhpFunctionComp(1709) or
      PhpFunctionComp(2146) or
      PhpFunctionComp(2805) or
      PhpFunctionComp(2980) or
      PhpFunctionComp(3042) or
      PhpFunctionComp(3194) or
      PhpFunctionComp(3581) or
      PhpFunctionComp(3701) or
      PhpFunctionComp(3938) or
      PhpFunctionComp(4231) or
      PhpFunctionComp(4366) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc153: TSynWebTokenKind;
begin
  if  PhpFunctionComp(105) or
      PhpFunctionComp(162) or
      PhpFunctionComp(333) or
      PhpFunctionComp(536) or
      PhpFunctionComp(587) or
      PhpFunctionComp(616) or
      PhpFunctionComp(713) or
      PhpFunctionComp(931) or
      PhpFunctionComp(946) or
      PhpFunctionComp(1225) or
      PhpFunctionComp(1502) or
      PhpFunctionComp(1512) or
      PhpFunctionComp(1533) or
      PhpFunctionComp(1660) or
      PhpFunctionComp(1998) or
      PhpFunctionComp(2970) or
      PhpFunctionComp(3074) or
      PhpFunctionComp(3911) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc154: TSynWebTokenKind;
begin
  if  PhpFunctionComp(158) or
      PhpFunctionComp(222) or
      PhpFunctionComp(381) or
      PhpFunctionComp(646) or
      PhpFunctionComp(830) or
      PhpFunctionComp(1008) or
      PhpFunctionComp(1132) or
      PhpFunctionComp(1394) or
      PhpFunctionComp(1829) or
      PhpFunctionComp(1874) or
      PhpFunctionComp(2766) or
      PhpFunctionComp(3873) or
      PhpFunctionComp(4138) or
      PhpFunctionComp(4164) or
      PhpFunctionComp(4174) or
      PhpFunctionComp(4381) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc155: TSynWebTokenKind;
begin
  if  PhpFunctionComp(394) or
      PhpFunctionComp(463) or
      PhpFunctionComp(549) or
      PhpFunctionComp(747) or
      PhpFunctionComp(1105) or
      PhpFunctionComp(1125) or
      PhpFunctionComp(1167) or
      PhpFunctionComp(1311) or
      PhpFunctionComp(1713) or
      PhpFunctionComp(1875) or
      PhpFunctionComp(1887) or
      PhpFunctionComp(1924) or
      PhpFunctionComp(2090) or
      PhpFunctionComp(2788) or
      PhpFunctionComp(2816) or
      PhpFunctionComp(2984) or
      PhpFunctionComp(3176) or
      PhpFunctionComp(3223) or
      PhpFunctionComp(3395) or
      PhpFunctionComp(3628) or
      PhpFunctionComp(3649) or
      PhpFunctionComp(4155) or
      PhpFunctionComp(4178) or
      PhpFunctionComp(4329) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc156: TSynWebTokenKind;
begin
  if  PhpFunctionComp(103) or
      PhpFunctionComp(156) or
      PhpFunctionComp(234) or
      PhpFunctionComp(332) or
      PhpFunctionComp(395) or
      PhpFunctionComp(730) or
      PhpFunctionComp(1022) or
      PhpFunctionComp(1226) or
      PhpFunctionComp(1517) or
      PhpFunctionComp(1826) or
      PhpFunctionComp(2004) or
      PhpFunctionComp(2149) or
      PhpFunctionComp(2185) or
      PhpFunctionComp(2415) or
      PhpFunctionComp(2696) or
      PhpFunctionComp(2803) or
      PhpFunctionComp(2845) or
      PhpFunctionComp(3736) or
      PhpFunctionComp(4010) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc157: TSynWebTokenKind;
begin
  if  PhpFunctionComp(128) or
      PhpFunctionComp(244) or
      PhpFunctionComp(501) or
      PhpFunctionComp(508) or
      PhpFunctionComp(1124) or
      PhpFunctionComp(1129) or
      PhpFunctionComp(1414) or
      PhpFunctionComp(1769) or
      PhpFunctionComp(2154) or
      PhpFunctionComp(2232) or
      PhpFunctionComp(3146) or
      PhpFunctionComp(3677) or
      PhpFunctionComp(3972) or
      PhpFunctionComp(3982) or
      PhpFunctionComp(4167) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc158: TSynWebTokenKind;
begin
  if  PhpFunctionComp(104) or
      PhpFunctionComp(203) or
      PhpFunctionComp(1194) or
      PhpFunctionComp(1449) or
      PhpFunctionComp(1613) or
      PhpFunctionComp(1759) or
      PhpFunctionComp(2040) or
      PhpFunctionComp(2719) or
      PhpFunctionComp(2813) or
      PhpFunctionComp(3106) or
      PhpFunctionComp(3181) or
      PhpFunctionComp(3412) or
      PhpFunctionComp(3653) or
      PhpFunctionComp(3682) or
      PhpFunctionComp(3698) or
      PhpFunctionComp(4122) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc159: TSynWebTokenKind;
begin
  if  PhpFunctionComp(115) or
      PhpFunctionComp(311) or
      PhpFunctionComp(340) or
      PhpFunctionComp(387) or
      PhpFunctionComp(571) or
      PhpFunctionComp(731) or
      PhpFunctionComp(1243) or
      PhpFunctionComp(1251) or
      PhpFunctionComp(1413) or
      PhpFunctionComp(1426) or
      PhpFunctionComp(1542) or
      PhpFunctionComp(1561) or
      PhpFunctionComp(2003) or
      PhpFunctionComp(2100) or
      PhpFunctionComp(2449) or
      PhpFunctionComp(2767) or
      PhpFunctionComp(2790) or
      PhpFunctionComp(3300) or
      PhpFunctionComp(3429) or
      PhpFunctionComp(3520) or
      PhpFunctionComp(3652) or
      PhpFunctionComp(3760) or
      PhpFunctionComp(3872) or
      PhpFunctionComp(3991) or
      PhpFunctionComp(4268) or
      PhpFunctionComp(4357) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc160: TSynWebTokenKind;
begin
  if  PhpFunctionComp(146) or
      PhpFunctionComp(1162) or
      PhpFunctionComp(1180) or
      PhpFunctionComp(1221) or
      PhpFunctionComp(1264) or
      PhpFunctionComp(1305) or
      PhpFunctionComp(1422) or
      PhpFunctionComp(1714) or
      PhpFunctionComp(1776) or
      PhpFunctionComp(1918) or
      PhpFunctionComp(2051) or
      PhpFunctionComp(2426) or
      PhpFunctionComp(2713) or
      PhpFunctionComp(3131) or
      PhpFunctionComp(3368) or
      PhpFunctionComp(3411) or
      PhpFunctionComp(3461) or
      PhpFunctionComp(3747) or
      PhpFunctionComp(3989) or
      PhpFunctionComp(3990) or
      PhpFunctionComp(4172) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc161: TSynWebTokenKind;
begin
  if  PhpFunctionComp(129) or
      PhpFunctionComp(191) or
      PhpFunctionComp(219) or
      PhpFunctionComp(368) or
      PhpFunctionComp(404) or
      PhpFunctionComp(424) or
      PhpFunctionComp(553) or
      PhpFunctionComp(1040) or
      PhpFunctionComp(1047) or
      PhpFunctionComp(1140) or
      PhpFunctionComp(1375) or
      PhpFunctionComp(1388) or
      PhpFunctionComp(1570) or
      PhpFunctionComp(3297) or
      PhpFunctionComp(3363) or
      PhpFunctionComp(3867) or
      PhpFunctionComp(3994) or
      PhpFunctionComp(3997) or
      PhpFunctionComp(4169) or
      PhpFunctionComp(4372) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc162: TSynWebTokenKind;
begin
  if  PhpFunctionComp(159) or
      PhpFunctionComp(200) or
      PhpFunctionComp(482) or
      PhpFunctionComp(748) or
      PhpFunctionComp(1037) or
      PhpFunctionComp(1312) or
      PhpFunctionComp(1462) or
      PhpFunctionComp(1609) or
      PhpFunctionComp(2246) or
      PhpFunctionComp(2502) or
      PhpFunctionComp(3018) or
      PhpFunctionComp(3105) or
      PhpFunctionComp(3177) or
      PhpFunctionComp(3208) or
      PhpFunctionComp(3209) or
      PhpFunctionComp(3242) or
      PhpFunctionComp(3375) or
      PhpFunctionComp(3651) or
      PhpFunctionComp(3687) or
      PhpFunctionComp(3720) or
      PhpFunctionComp(3926) or
      PhpFunctionComp(4029) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc163: TSynWebTokenKind;
begin
  if  PhpFunctionComp(385) or
      PhpFunctionComp(550) or
      PhpFunctionComp(726) or
      PhpFunctionComp(776) or
      PhpFunctionComp(976) or
      PhpFunctionComp(1039) or
      PhpFunctionComp(1241) or
      PhpFunctionComp(1456) or
      PhpFunctionComp(1523) or
      PhpFunctionComp(1620) or
      PhpFunctionComp(1643) or
      PhpFunctionComp(1691) or
      PhpFunctionComp(2794) or
      PhpFunctionComp(2996) or
      PhpFunctionComp(3087) or
      PhpFunctionComp(3212) or
      PhpFunctionComp(3470) or
      PhpFunctionComp(3471) or
      PhpFunctionComp(3870) or
      PhpFunctionComp(3907) or
      PhpFunctionComp(4354) or
      PhpFunctionComp(4356) or
      PhpFunctionComp(4358) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc164: TSynWebTokenKind;
begin
  if  PhpFunctionComp(154) or
      PhpFunctionComp(161) or
      PhpFunctionComp(192) or
      PhpFunctionComp(944) or
      PhpFunctionComp(1459) or
      PhpFunctionComp(1486) or
      PhpFunctionComp(1560) or
      PhpFunctionComp(1675) or
      PhpFunctionComp(1737) or
      PhpFunctionComp(1847) or
      PhpFunctionComp(1848) or
      PhpFunctionComp(2109) or
      PhpFunctionComp(2425) or
      PhpFunctionComp(2707) or
      PhpFunctionComp(2778) or
      PhpFunctionComp(2983) or
      PhpFunctionComp(3157) or
      PhpFunctionComp(3196) or
      PhpFunctionComp(3279) or
      PhpFunctionComp(3685) or
      PhpFunctionComp(4006) or
      PhpFunctionComp(4060) or
      PhpFunctionComp(4183) or
      PhpFunctionComp(4234) or
      PhpFunctionComp(4334) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc165: TSynWebTokenKind;
begin
  if  PhpFunctionComp(319) or
      PhpFunctionComp(405) or
      PhpFunctionComp(447) or
      PhpFunctionComp(537) or
      PhpFunctionComp(574) or
      PhpFunctionComp(872) or
      PhpFunctionComp(1067) or
      PhpFunctionComp(1494) or
      PhpFunctionComp(1983) or
      PhpFunctionComp(2005) or
      PhpFunctionComp(2167) or
      PhpFunctionComp(2827) or
      PhpFunctionComp(2942) or
      PhpFunctionComp(3935) or
      PhpFunctionComp(3952) or
      PhpFunctionComp(4156) or
      PhpFunctionComp(4162) or
      PhpFunctionComp(4173) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc166: TSynWebTokenKind;
begin
  if  PhpFunctionComp(339) or
      PhpFunctionComp(746) or
      PhpFunctionComp(765) or
      PhpFunctionComp(816) or
      PhpFunctionComp(845) or
      PhpFunctionComp(917) or
      PhpFunctionComp(1009) or
      PhpFunctionComp(1310) or
      PhpFunctionComp(1480) or
      PhpFunctionComp(1496) or
      PhpFunctionComp(1996) or
      PhpFunctionComp(2008) or
      PhpFunctionComp(3008) or
      PhpFunctionComp(3165) or
      PhpFunctionComp(3590) or
      PhpFunctionComp(3932) or
      PhpFunctionComp(4168) or
      PhpFunctionComp(4171) or
      PhpFunctionComp(4181) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc167: TSynWebTokenKind;
begin
  if  PhpKeywordComp(67) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(391) or
        PhpFunctionComp(407) or
        PhpFunctionComp(487) or
        PhpFunctionComp(614) or
        PhpFunctionComp(766) or
        PhpFunctionComp(777) or
        PhpFunctionComp(806) or
        PhpFunctionComp(831) or
        PhpFunctionComp(896) or
        PhpFunctionComp(1118) or
        PhpFunctionComp(1133) or
        PhpFunctionComp(1349) or
        PhpFunctionComp(1546) or
        PhpFunctionComp(1836) or
        PhpFunctionComp(1919) or
        PhpFunctionComp(2168) or
        PhpFunctionComp(2275) or
        PhpFunctionComp(2447) or
        PhpFunctionComp(2828) or
        PhpFunctionComp(3031) or
        PhpFunctionComp(3072) or
        PhpFunctionComp(3109) or
        PhpFunctionComp(3397) or
        PhpFunctionComp(3585) or
        PhpFunctionComp(3655) or
        PhpFunctionComp(3765) or
        PhpFunctionComp(3887) or
        PhpFunctionComp(3996) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc168: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1142) or
      PhpFunctionComp(1390) or
      PhpFunctionComp(1463) or
      PhpFunctionComp(1558) or
      PhpFunctionComp(1666) or
      PhpFunctionComp(2000) or
      PhpFunctionComp(2188) or
      PhpFunctionComp(2230) or
      PhpFunctionComp(2443) or
      PhpFunctionComp(2741) or
      PhpFunctionComp(2836) or
      PhpFunctionComp(2934) or
      PhpFunctionComp(3207) or
      PhpFunctionComp(3214) or
      PhpFunctionComp(3586) or
      PhpFunctionComp(3599) or
      PhpFunctionComp(3794) or
      PhpFunctionComp(3933) or
      PhpFunctionComp(3954) or
      PhpFunctionComp(3976) or
      PhpFunctionComp(4175) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc169: TSynWebTokenKind;
begin
  if  PhpFunctionComp(112) or
      PhpFunctionComp(456) or
      PhpFunctionComp(975) or
      PhpFunctionComp(1178) or
      PhpFunctionComp(1223) or
      PhpFunctionComp(1516) or
      PhpFunctionComp(1557) or
      PhpFunctionComp(1596) or
      PhpFunctionComp(1644) or
      PhpFunctionComp(1706) or
      PhpFunctionComp(1972) or
      PhpFunctionComp(2211) or
      PhpFunctionComp(2289) or
      PhpFunctionComp(3755) or
      PhpFunctionComp(4067) or
      PhpFunctionComp(4270) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc170: TSynWebTokenKind;
begin
  if  PhpKeywordComp(58) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(138) or
        PhpFunctionComp(269) or
        PhpFunctionComp(323) or
        PhpFunctionComp(371) or
        PhpFunctionComp(942) or
        PhpFunctionComp(1446) or
        PhpFunctionComp(1589) or
        PhpFunctionComp(2466) or
        PhpFunctionComp(2579) or
        PhpFunctionComp(2747) or
        PhpFunctionComp(3040) or
        PhpFunctionComp(3053) or
        PhpFunctionComp(3101) or
        PhpFunctionComp(3257) or
        PhpFunctionComp(3476) or
        PhpFunctionComp(3612) or
        PhpFunctionComp(3869) or
        PhpFunctionComp(3876) or
        PhpFunctionComp(4132) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc171: TSynWebTokenKind;
begin
  if  PhpFunctionComp(110) or
      PhpFunctionComp(280) or
      PhpFunctionComp(519) or
      PhpFunctionComp(538) or
      PhpFunctionComp(1391) or
      PhpFunctionComp(1411) or
      PhpFunctionComp(1508) or
      PhpFunctionComp(1530) or
      PhpFunctionComp(1534) or
      PhpFunctionComp(2189) or
      PhpFunctionComp(2192) or
      PhpFunctionComp(2314) or
      PhpFunctionComp(2779) or
      PhpFunctionComp(2923) or
      PhpFunctionComp(3358) or
      PhpFunctionComp(3396) or
      PhpFunctionComp(3875) or
      PhpFunctionComp(3973) or
      PhpFunctionComp(4177) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc172: TSynWebTokenKind;
begin
  if  PhpFunctionComp(446) or
      PhpFunctionComp(554) or
      PhpFunctionComp(567) or
      PhpFunctionComp(619) or
      PhpFunctionComp(688) or
      PhpFunctionComp(728) or
      PhpFunctionComp(1212) or
      PhpFunctionComp(1514) or
      PhpFunctionComp(2147) or
      PhpFunctionComp(2155) or
      PhpFunctionComp(2927) or
      PhpFunctionComp(3083) or
      PhpFunctionComp(3183) or
      PhpFunctionComp(3264) or
      PhpFunctionComp(3346) or
      PhpFunctionComp(3382) or
      PhpFunctionComp(3500) or
      PhpFunctionComp(3804) or
      PhpFunctionComp(4009) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc173: TSynWebTokenKind;
begin
  if  PhpFunctionComp(393) or
      PhpFunctionComp(681) or
      PhpFunctionComp(832) or
      PhpFunctionComp(1213) or
      PhpFunctionComp(1244) or
      PhpFunctionComp(1407) or
      PhpFunctionComp(1435) or
      PhpFunctionComp(1575) or
      PhpFunctionComp(1818) or
      PhpFunctionComp(1831) or
      PhpFunctionComp(1923) or
      PhpFunctionComp(2011) or
      PhpFunctionComp(2611) or
      PhpFunctionComp(2708) or
      PhpFunctionComp(2924) or
      PhpFunctionComp(3127) or
      PhpFunctionComp(3141) or
      PhpFunctionComp(3169) or
      PhpFunctionComp(3197) or
      PhpFunctionComp(3605) or
      PhpFunctionComp(3714) or
      PhpFunctionComp(3729) or
      PhpFunctionComp(3784) or
      PhpFunctionComp(3925) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc174: TSynWebTokenKind;
begin
  if  PhpFunctionComp(193) or
      PhpFunctionComp(248) or
      PhpFunctionComp(341) or
      PhpFunctionComp(552) or
      PhpFunctionComp(1002) or
      PhpFunctionComp(1147) or
      PhpFunctionComp(1605) or
      PhpFunctionComp(1744) or
      PhpFunctionComp(2169) or
      PhpFunctionComp(2326) or
      PhpFunctionComp(2730) or
      PhpFunctionComp(2820) or
      PhpFunctionComp(2831) or
      PhpFunctionComp(2906) or
      PhpFunctionComp(3193) or
      PhpFunctionComp(3291) or
      PhpFunctionComp(3372) or
      PhpFunctionComp(3524) or
      PhpFunctionComp(3700) or
      PhpFunctionComp(3854) or
      PhpFunctionComp(3978) or
      PhpFunctionComp(4152) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc175: TSynWebTokenKind;
begin
  if  PhpFunctionComp(717) or
      PhpFunctionComp(1156) or
      PhpFunctionComp(1304) or
      PhpFunctionComp(1472) or
      PhpFunctionComp(2001) or
      PhpFunctionComp(2432) or
      PhpFunctionComp(2480) or
      PhpFunctionComp(3351) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc176: TSynWebTokenKind;
begin
  if  PhpFunctionComp(250) or
      PhpFunctionComp(252) or
      PhpFunctionComp(383) or
      PhpFunctionComp(694) or
      PhpFunctionComp(718) or
      PhpFunctionComp(749) or
      PhpFunctionComp(961) or
      PhpFunctionComp(1018) or
      PhpFunctionComp(1111) or
      PhpFunctionComp(1148) or
      PhpFunctionComp(1175) or
      PhpFunctionComp(1303) or
      PhpFunctionComp(1355) or
      PhpFunctionComp(1377) or
      PhpFunctionComp(1452) or
      PhpFunctionComp(1473) or
      PhpFunctionComp(1604) or
      PhpFunctionComp(2126) or
      PhpFunctionComp(2348) or
      PhpFunctionComp(2351) or
      PhpFunctionComp(2431) or
      PhpFunctionComp(2877) or
      PhpFunctionComp(3190) or
      PhpFunctionComp(3361) or
      PhpFunctionComp(3398) or
      PhpFunctionComp(3728) or
      PhpFunctionComp(4158) or
      PhpFunctionComp(4179) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc177: TSynWebTokenKind;
begin
  if  PhpFunctionComp(430) or
      PhpFunctionComp(512) or
      PhpFunctionComp(543) or
      PhpFunctionComp(1281) or
      PhpFunctionComp(1301) or
      PhpFunctionComp(1376) or
      PhpFunctionComp(2007) or
      PhpFunctionComp(2226) or
      PhpFunctionComp(2239) or
      PhpFunctionComp(2749) or
      PhpFunctionComp(2793) or
      PhpFunctionComp(2811) or
      PhpFunctionComp(3188) or
      PhpFunctionComp(3262) or
      PhpFunctionComp(3281) or
      PhpFunctionComp(3912) or
      PhpFunctionComp(4000) or
      PhpFunctionComp(4150) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc178: TSynWebTokenKind;
begin
  if  PhpFunctionComp(437) or
      PhpFunctionComp(470) or
      PhpFunctionComp(578) or
      PhpFunctionComp(1160) or
      PhpFunctionComp(1163) or
      PhpFunctionComp(1300) or
      PhpFunctionComp(1380) or
      PhpFunctionComp(1974) or
      PhpFunctionComp(2360) or
      PhpFunctionComp(2516) or
      PhpFunctionComp(2668) or
      PhpFunctionComp(2789) or
      PhpFunctionComp(3038) or
      PhpFunctionComp(3189) or
      PhpFunctionComp(3203) or
      PhpFunctionComp(4080) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc179: TSynWebTokenKind;
begin
  if  PhpFunctionComp(396) or
      PhpFunctionComp(401) or
      PhpFunctionComp(539) or
      PhpFunctionComp(562) or
      PhpFunctionComp(802) or
      PhpFunctionComp(846) or
      PhpFunctionComp(1209) or
      PhpFunctionComp(1373) or
      PhpFunctionComp(1474) or
      PhpFunctionComp(1607) or
      PhpFunctionComp(1624) or
      PhpFunctionComp(1837) or
      PhpFunctionComp(2166) or
      PhpFunctionComp(2455) or
      PhpFunctionComp(2557) or
      PhpFunctionComp(2723) or
      PhpFunctionComp(3081) or
      PhpFunctionComp(3151) or
      PhpFunctionComp(3393) or
      PhpFunctionComp(3900) or
      PhpFunctionComp(4184) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc180: TSynWebTokenKind;
begin
  if  PhpKeywordComp(1) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(171) or
        PhpFunctionComp(251) or
        PhpFunctionComp(398) or
        PhpFunctionComp(629) or
        PhpFunctionComp(1038) or
        PhpFunctionComp(1370) or
        PhpFunctionComp(1653) or
        PhpFunctionComp(1814) or
        PhpFunctionComp(2670) or
        PhpFunctionComp(2717) or
        PhpFunctionComp(2799) or
        PhpFunctionComp(3130) or
        PhpFunctionComp(3280) or
        PhpFunctionComp(3449) or
        PhpFunctionComp(3598) or
        PhpFunctionComp(3757) or
        PhpFunctionComp(4232) or
        PhpFunctionComp(4236) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc181: TSynWebTokenKind;
begin
  if  PhpFunctionComp(540) or
      PhpFunctionComp(556) or
      PhpFunctionComp(690) or
      PhpFunctionComp(952) or
      PhpFunctionComp(1158) or
      PhpFunctionComp(1174) or
      PhpFunctionComp(1497) or
      PhpFunctionComp(2019) or
      PhpFunctionComp(2242) or
      PhpFunctionComp(2606) or
      PhpFunctionComp(2931) or
      PhpFunctionComp(2997) or
      PhpFunctionComp(3166) or
      PhpFunctionComp(3175) or
      PhpFunctionComp(3271) or
      PhpFunctionComp(3380) or
      PhpFunctionComp(3543) or
      PhpFunctionComp(4117) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc182: TSynWebTokenKind;
begin
  if  PhpFunctionComp(400) or
      PhpFunctionComp(687) or
      PhpFunctionComp(714) or
      PhpFunctionComp(1205) or
      PhpFunctionComp(1470) or
      PhpFunctionComp(1621) or
      PhpFunctionComp(2114) or
      PhpFunctionComp(2123) or
      PhpFunctionComp(2248) or
      PhpFunctionComp(2453) or
      PhpFunctionComp(2829) or
      PhpFunctionComp(2919) or
      PhpFunctionComp(3077) or
      PhpFunctionComp(3261) or
      PhpFunctionComp(3415) or
      PhpFunctionComp(3441) or
      PhpFunctionComp(3666) or
      PhpFunctionComp(3752) or
      PhpFunctionComp(4030) or
      PhpFunctionComp(4072) or
      PhpFunctionComp(4335) or
      PhpFunctionComp(4351) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc183: TSynWebTokenKind;
begin
  if  PhpFunctionComp(397) or
      PhpFunctionComp(542) or
      PhpFunctionComp(801) or
      PhpFunctionComp(834) or
      PhpFunctionComp(1066) or
      PhpFunctionComp(1441) or
      PhpFunctionComp(1622) or
      PhpFunctionComp(1625) or
      PhpFunctionComp(1640) or
      PhpFunctionComp(2122) or
      PhpFunctionComp(2176) or
      PhpFunctionComp(2295) or
      PhpFunctionComp(2714) or
      PhpFunctionComp(3016) or
      PhpFunctionComp(3172) or
      PhpFunctionComp(3213) or
      PhpFunctionComp(3607) or
      PhpFunctionComp(3656) or
      PhpFunctionComp(3657) or
      PhpFunctionComp(3975) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc184: TSynWebTokenKind;
begin
  if  PhpFunctionComp(254) or
      PhpFunctionComp(272) or
      PhpFunctionComp(336) or
      PhpFunctionComp(547) or
      PhpFunctionComp(1878) or
      PhpFunctionComp(1973) or
      PhpFunctionComp(2184) or
      PhpFunctionComp(2349) or
      PhpFunctionComp(2450) or
      PhpFunctionComp(2465) or
      PhpFunctionComp(2985) or
      PhpFunctionComp(3006) or
      PhpFunctionComp(3041) or
      PhpFunctionComp(3276) or
      PhpFunctionComp(3284) or
      PhpFunctionComp(3686) or
      PhpFunctionComp(3894) or
      PhpFunctionComp(4008) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc185: TSynWebTokenKind;
begin
  if  PhpFunctionComp(473) or
      PhpFunctionComp(564) or
      PhpFunctionComp(710) or
      PhpFunctionComp(849) or
      PhpFunctionComp(1215) or
      PhpFunctionComp(1363) or
      PhpFunctionComp(1436) or
      PhpFunctionComp(1485) or
      PhpFunctionComp(2844) or
      PhpFunctionComp(2952) or
      PhpFunctionComp(2999) or
      PhpFunctionComp(3002) or
      PhpFunctionComp(3078) or
      PhpFunctionComp(3202) or
      PhpFunctionComp(3560) or
      PhpFunctionComp(3711) or
      PhpFunctionComp(3756) or
      PhpFunctionComp(4094) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc186: TSynWebTokenKind;
begin
  if  PhpFunctionComp(417) or
      PhpFunctionComp(545) or
      PhpFunctionComp(1138) or
      PhpFunctionComp(1450) or
      PhpFunctionComp(1458) or
      PhpFunctionComp(1483) or
      PhpFunctionComp(1504) or
      PhpFunctionComp(2006) or
      PhpFunctionComp(2308) or
      PhpFunctionComp(2787) or
      PhpFunctionComp(2907) or
      PhpFunctionComp(3277) or
      PhpFunctionComp(3289) or
      PhpFunctionComp(3451) or
      PhpFunctionComp(3546) or
      PhpFunctionComp(3773) or
      PhpFunctionComp(3812) or
      PhpFunctionComp(4336) or
      PhpFunctionComp(4364) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc187: TSynWebTokenKind;
begin
  if  PhpFunctionComp(169) or
      PhpFunctionComp(270) or
      PhpFunctionComp(433) or
      PhpFunctionComp(452) or
      PhpFunctionComp(551) or
      PhpFunctionComp(884) or
      PhpFunctionComp(894) or
      PhpFunctionComp(1085) or
      PhpFunctionComp(1090) or
      PhpFunctionComp(1369) or
      PhpFunctionComp(1389) or
      PhpFunctionComp(1423) or
      PhpFunctionComp(1444) or
      PhpFunctionComp(1563) or
      PhpFunctionComp(1678) or
      PhpFunctionComp(1786) or
      PhpFunctionComp(1956) or
      PhpFunctionComp(2890) or
      PhpFunctionComp(3070) or
      PhpFunctionComp(3328) or
      PhpFunctionComp(3647) or
      PhpFunctionComp(3659) or
      PhpFunctionComp(3702) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc188: TSynWebTokenKind;
begin
  if  PhpKeywordComp(3) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(720) or
        PhpFunctionComp(769) or
        PhpFunctionComp(792) or
        PhpFunctionComp(805) or
        PhpFunctionComp(1272) or
        PhpFunctionComp(1658) or
        PhpFunctionComp(1928) or
        PhpFunctionComp(2183) or
        PhpFunctionComp(2467) or
        PhpFunctionComp(2527) or
        PhpFunctionComp(3026) or
        PhpFunctionComp(3096) or
        PhpFunctionComp(3269) or
        PhpFunctionComp(3437) or
        PhpFunctionComp(3670) or
        PhpFunctionComp(3930) or
        PhpFunctionComp(4128) or
        PhpFunctionComp(4386) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc189: TSynWebTokenKind;
begin
  if  PhpFunctionComp(561) or
      PhpFunctionComp(775) or
      PhpFunctionComp(803) or
      PhpFunctionComp(817) or
      PhpFunctionComp(945) or
      PhpFunctionComp(1284) or
      PhpFunctionComp(1296) or
      PhpFunctionComp(1357) or
      PhpFunctionComp(1598) or
      PhpFunctionComp(1811) or
      PhpFunctionComp(1822) or
      PhpFunctionComp(1916) or
      PhpFunctionComp(2470) or
      PhpFunctionComp(2721) or
      PhpFunctionComp(2722) or
      PhpFunctionComp(2756) or
      PhpFunctionComp(3015) or
      PhpFunctionComp(3179) or
      PhpFunctionComp(3282) or
      PhpFunctionComp(3696) or
      PhpFunctionComp(3865) or
      PhpFunctionComp(3974) or
      PhpFunctionComp(3981) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc190: TSynWebTokenKind;
begin
  if  PhpFunctionComp(804) or
      PhpFunctionComp(1230) or
      PhpFunctionComp(1402) or
      PhpFunctionComp(1960) or
      PhpFunctionComp(2265) or
      PhpFunctionComp(3058) or
      PhpFunctionComp(3259) or
      PhpFunctionComp(3296) or
      PhpFunctionComp(3347) or
      PhpFunctionComp(3442) or
      PhpFunctionComp(3582) or
      PhpFunctionComp(3998) or
      PhpFunctionComp(4091) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc191: TSynWebTokenKind;
begin
  if  PhpFunctionComp(389) or
      PhpFunctionComp(613) or
      PhpFunctionComp(821) or
      PhpFunctionComp(1216) or
      PhpFunctionComp(1352) or
      PhpFunctionComp(1415) or
      PhpFunctionComp(2129) or
      PhpFunctionComp(2193) or
      PhpFunctionComp(2484) or
      PhpFunctionComp(2486) or
      PhpFunctionComp(2505) or
      PhpFunctionComp(2576) or
      PhpFunctionComp(2808) or
      PhpFunctionComp(3039) or
      PhpFunctionComp(3057) or
      PhpFunctionComp(3148) or
      PhpFunctionComp(3263) or
      PhpFunctionComp(3306) or
      PhpFunctionComp(3377) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc192: TSynWebTokenKind;
begin
  if  PhpFunctionComp(88) or
      PhpFunctionComp(157) or
      PhpFunctionComp(555) or
      PhpFunctionComp(996) or
      PhpFunctionComp(1023) or
      PhpFunctionComp(1381) or
      PhpFunctionComp(1487) or
      PhpFunctionComp(2266) or
      PhpFunctionComp(2383) or
      PhpFunctionComp(2774) or
      PhpFunctionComp(3004) or
      PhpFunctionComp(3112) or
      PhpFunctionComp(3168) or
      PhpFunctionComp(3246) or
      PhpFunctionComp(3258) or
      PhpFunctionComp(3759) or
      PhpFunctionComp(4373) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc193: TSynWebTokenKind;
begin
  if  PhpFunctionComp(419) or
      PhpFunctionComp(471) or
      PhpFunctionComp(735) or
      PhpFunctionComp(829) or
      PhpFunctionComp(1056) or
      PhpFunctionComp(1077) or
      PhpFunctionComp(1084) or
      PhpFunctionComp(1172) or
      PhpFunctionComp(1211) or
      PhpFunctionComp(1368) or
      PhpFunctionComp(1379) or
      PhpFunctionComp(1982) or
      PhpFunctionComp(2071) or
      PhpFunctionComp(2148) or
      PhpFunctionComp(2186) or
      PhpFunctionComp(2249) or
      PhpFunctionComp(2294) or
      PhpFunctionComp(3066) or
      PhpFunctionComp(3305) or
      PhpFunctionComp(3692) or
      PhpFunctionComp(4020) or
      PhpFunctionComp(4126) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc194: TSynWebTokenKind;
begin
  if  PhpFunctionComp(721) or
      PhpFunctionComp(1359) or
      PhpFunctionComp(1897) or
      PhpFunctionComp(1932) or
      PhpFunctionComp(2132) or
      PhpFunctionComp(2153) or
      PhpFunctionComp(2848) or
      PhpFunctionComp(2935) or
      PhpFunctionComp(2995) or
      PhpFunctionComp(3139) or
      PhpFunctionComp(3161) or
      PhpFunctionComp(3220) or
      PhpFunctionComp(3341) or
      PhpFunctionComp(3348) or
      PhpFunctionComp(3378) or
      PhpFunctionComp(3799) or
      PhpFunctionComp(3983) or
      PhpFunctionComp(3993) or
      PhpFunctionComp(4048) or
      PhpFunctionComp(4055) or
      PhpFunctionComp(4101) or
      PhpFunctionComp(4180) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc195: TSynWebTokenKind;
begin
  if  PhpFunctionComp(324) or
      PhpFunctionComp(448) or
      PhpFunctionComp(623) or
      PhpFunctionComp(693) or
      PhpFunctionComp(853) or
      PhpFunctionComp(943) or
      PhpFunctionComp(962) or
      PhpFunctionComp(1189) or
      PhpFunctionComp(1210) or
      PhpFunctionComp(1293) or
      PhpFunctionComp(1365) or
      PhpFunctionComp(1532) or
      PhpFunctionComp(1639) or
      PhpFunctionComp(1830) or
      PhpFunctionComp(1999) or
      PhpFunctionComp(2233) or
      PhpFunctionComp(2692) or
      PhpFunctionComp(2825) or
      PhpFunctionComp(2833) or
      PhpFunctionComp(3034) or
      PhpFunctionComp(3110) or
      PhpFunctionComp(3163) or
      PhpFunctionComp(3445) or
      PhpFunctionComp(3528) or
      PhpFunctionComp(3587) or
      PhpFunctionComp(4026) or
      PhpFunctionComp(4107) or
      PhpFunctionComp(4338) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc196: TSynWebTokenKind;
begin
  if  PhpFunctionComp(466) or
      PhpFunctionComp(486) or
      PhpFunctionComp(716) or
      PhpFunctionComp(770) or
      PhpFunctionComp(1091) or
      PhpFunctionComp(1179) or
      PhpFunctionComp(1292) or
      PhpFunctionComp(1890) or
      PhpFunctionComp(2297) or
      PhpFunctionComp(2504) or
      PhpFunctionComp(2750) or
      PhpFunctionComp(2795) or
      PhpFunctionComp(2826) or
      PhpFunctionComp(3029) or
      PhpFunctionComp(3068) or
      PhpFunctionComp(3160) or
      PhpFunctionComp(3286) or
      PhpFunctionComp(3960) or
      PhpFunctionComp(3977) or
      PhpFunctionComp(4104) or
      PhpFunctionComp(4116) or
      PhpFunctionComp(4363) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc197: TSynWebTokenKind;
begin
  if  PhpFunctionComp(153) or
      PhpFunctionComp(170) or
      PhpFunctionComp(431) or
      PhpFunctionComp(785) or
      PhpFunctionComp(1053) or
      PhpFunctionComp(1235) or
      PhpFunctionComp(1299) or
      PhpFunctionComp(1412) or
      PhpFunctionComp(1506) or
      PhpFunctionComp(1760) or
      PhpFunctionComp(1842) or
      PhpFunctionComp(2240) or
      PhpFunctionComp(2250) or
      PhpFunctionComp(2448) or
      PhpFunctionComp(2677) or
      PhpFunctionComp(2969) or
      PhpFunctionComp(3114) or
      PhpFunctionComp(3265) or
      PhpFunctionComp(3272) or
      PhpFunctionComp(3326) or
      PhpFunctionComp(3389) or
      PhpFunctionComp(3533) or
      PhpFunctionComp(3544) or
      PhpFunctionComp(3730) or
      PhpFunctionComp(3979) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc198: TSynWebTokenKind;
begin
  if  PhpFunctionComp(285) or
      PhpFunctionComp(372) or
      PhpFunctionComp(504) or
      PhpFunctionComp(520) or
      PhpFunctionComp(715) or
      PhpFunctionComp(818) or
      PhpFunctionComp(1812) or
      PhpFunctionComp(1846) or
      PhpFunctionComp(2053) or
      PhpFunctionComp(2098) or
      PhpFunctionComp(2107) or
      PhpFunctionComp(2229) or
      PhpFunctionComp(2428) or
      PhpFunctionComp(2477) or
      PhpFunctionComp(2556) or
      PhpFunctionComp(2563) or
      PhpFunctionComp(2744) or
      PhpFunctionComp(2824) or
      PhpFunctionComp(2994) or
      PhpFunctionComp(3007) or
      PhpFunctionComp(3287) or
      PhpFunctionComp(3357) or
      PhpFunctionComp(3795) or
      PhpFunctionComp(4047) or
      PhpFunctionComp(4212) or
      PhpFunctionComp(4233) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc199: TSynWebTokenKind;
begin
  if  PhpFunctionComp(356) or
      PhpFunctionComp(786) or
      PhpFunctionComp(1275) or
      PhpFunctionComp(1461) or
      PhpFunctionComp(1535) or
      PhpFunctionComp(1638) or
      PhpFunctionComp(1777) or
      PhpFunctionComp(1867) or
      PhpFunctionComp(2244) or
      PhpFunctionComp(2267) or
      PhpFunctionComp(2498) or
      PhpFunctionComp(2500) or
      PhpFunctionComp(2712) or
      PhpFunctionComp(2745) or
      PhpFunctionComp(3153) or
      PhpFunctionComp(3170) or
      PhpFunctionComp(3199) or
      PhpFunctionComp(3369) or
      PhpFunctionComp(3984) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc200: TSynWebTokenKind;
begin
  if  PhpFunctionComp(182) or
      PhpFunctionComp(375) or
      PhpFunctionComp(517) or
      PhpFunctionComp(740) or
      PhpFunctionComp(808) or
      PhpFunctionComp(1081) or
      PhpFunctionComp(1165) or
      PhpFunctionComp(1443) or
      PhpFunctionComp(1540) or
      PhpFunctionComp(1585) or
      PhpFunctionComp(1659) or
      PhpFunctionComp(2170) or
      PhpFunctionComp(2196) or
      PhpFunctionComp(2642) or
      PhpFunctionComp(2743) or
      PhpFunctionComp(2832) or
      PhpFunctionComp(2850) or
      PhpFunctionComp(3210) or
      PhpFunctionComp(3285) or
      PhpFunctionComp(3588) or
      PhpFunctionComp(3595) or
      PhpFunctionComp(3980) or
      PhpFunctionComp(4015) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc201: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1073) or
      PhpFunctionComp(2150) or
      PhpFunctionComp(2181) or
      PhpFunctionComp(2838) or
      PhpFunctionComp(3805) or
      PhpFunctionComp(3821) or
      PhpFunctionComp(3965) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc202: TSynWebTokenKind;
begin
  if  PhpKeywordComp(0) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(351) or
        PhpFunctionComp(445) or
        PhpFunctionComp(451) or
        PhpFunctionComp(586) or
        PhpFunctionComp(1819) or
        PhpFunctionComp(2093) or
        PhpFunctionComp(2115) or
        PhpFunctionComp(2119) or
        PhpFunctionComp(2161) or
        PhpFunctionComp(2322) or
        PhpFunctionComp(2419) or
        PhpFunctionComp(2566) or
        PhpFunctionComp(2948) or
        PhpFunctionComp(3159) or
        PhpFunctionComp(3371) or
        PhpFunctionComp(4113) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc203: TSynWebTokenKind;
begin
  if  PhpFunctionComp(133) or
      PhpFunctionComp(697) or
      PhpFunctionComp(815) or
      PhpFunctionComp(1079) or
      PhpFunctionComp(1149) or
      PhpFunctionComp(1240) or
      PhpFunctionComp(1291) or
      PhpFunctionComp(1503) or
      PhpFunctionComp(1556) or
      PhpFunctionComp(1581) or
      PhpFunctionComp(1623) or
      PhpFunctionComp(1898) or
      PhpFunctionComp(2220) or
      PhpFunctionComp(2445) or
      PhpFunctionComp(2476) or
      PhpFunctionComp(2492) or
      PhpFunctionComp(2751) or
      PhpFunctionComp(2823) or
      PhpFunctionComp(2835) or
      PhpFunctionComp(3132) or
      PhpFunctionComp(3152) or
      PhpFunctionComp(3283) or
      PhpFunctionComp(4049) or
      PhpFunctionComp(4239) or
      PhpFunctionComp(4352) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc204: TSynWebTokenKind;
begin
  if  PhpFunctionComp(628) or
      PhpFunctionComp(1488) or
      PhpFunctionComp(1526) or
      PhpFunctionComp(1554) or
      PhpFunctionComp(1736) or
      PhpFunctionComp(1827) or
      PhpFunctionComp(2018) or
      PhpFunctionComp(2162) or
      PhpFunctionComp(2264) or
      PhpFunctionComp(2427) or
      PhpFunctionComp(2762) or
      PhpFunctionComp(3032) or
      PhpFunctionComp(3245) or
      PhpFunctionComp(3343) or
      PhpFunctionComp(3399) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc205: TSynWebTokenKind;
begin
  if  PhpFunctionComp(666) or
      PhpFunctionComp(844) or
      PhpFunctionComp(1161) or
      PhpFunctionComp(1164) or
      PhpFunctionComp(1350) or
      PhpFunctionComp(1442) or
      PhpFunctionComp(1652) or
      PhpFunctionComp(1957) or
      PhpFunctionComp(1992) or
      PhpFunctionComp(2187) or
      PhpFunctionComp(2309) or
      PhpFunctionComp(2495) or
      PhpFunctionComp(2503) or
      PhpFunctionComp(2698) or
      PhpFunctionComp(2817) or
      PhpFunctionComp(3005) or
      PhpFunctionComp(3076) or
      PhpFunctionComp(3100) or
      PhpFunctionComp(3198) or
      PhpFunctionComp(3266) or
      PhpFunctionComp(3708) or
      PhpFunctionComp(3758) or
      PhpFunctionComp(3874) or
      PhpFunctionComp(4213) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc206: TSynWebTokenKind;
begin
  if  PhpFunctionComp(467) or
      PhpFunctionComp(630) or
      PhpFunctionComp(745) or
      PhpFunctionComp(787) or
      PhpFunctionComp(788) or
      PhpFunctionComp(814) or
      PhpFunctionComp(1495) or
      PhpFunctionComp(1763) or
      PhpFunctionComp(1965) or
      PhpFunctionComp(1968) or
      PhpFunctionComp(2310) or
      PhpFunctionComp(2323) or
      PhpFunctionComp(2367) or
      PhpFunctionComp(2452) or
      PhpFunctionComp(2657) or
      PhpFunctionComp(2700) or
      PhpFunctionComp(2742) or
      PhpFunctionComp(3985) or
      PhpFunctionComp(4001) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc207: TSynWebTokenKind;
begin
  if  PhpFunctionComp(443) or
      PhpFunctionComp(468) or
      PhpFunctionComp(558) or
      PhpFunctionComp(559) or
      PhpFunctionComp(919) or
      PhpFunctionComp(1228) or
      PhpFunctionComp(1273) or
      PhpFunctionComp(1325) or
      PhpFunctionComp(1580) or
      PhpFunctionComp(2219) or
      PhpFunctionComp(2507) or
      PhpFunctionComp(2565) or
      PhpFunctionComp(2567) or
      PhpFunctionComp(2987) or
      PhpFunctionComp(3345) or
      PhpFunctionComp(3446) or
      PhpFunctionComp(3450) or
      PhpFunctionComp(3589) or
      PhpFunctionComp(3737) or
      PhpFunctionComp(3815) or
      PhpFunctionComp(3958) or
      PhpFunctionComp(4003) or
      PhpFunctionComp(4339) or
      PhpFunctionComp(4349) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc208: TSynWebTokenKind;
begin
  if  PhpFunctionComp(313) or
      PhpFunctionComp(465) or
      PhpFunctionComp(568) or
      PhpFunctionComp(780) or
      PhpFunctionComp(800) or
      PhpFunctionComp(959) or
      PhpFunctionComp(1082) or
      PhpFunctionComp(1187) or
      PhpFunctionComp(1229) or
      PhpFunctionComp(1307) or
      PhpFunctionComp(1513) or
      PhpFunctionComp(1567) or
      PhpFunctionComp(1591) or
      PhpFunctionComp(1823) or
      PhpFunctionComp(2281) or
      PhpFunctionComp(2424) or
      PhpFunctionComp(2628) or
      PhpFunctionComp(2920) or
      PhpFunctionComp(2930) or
      PhpFunctionComp(3049) or
      PhpFunctionComp(3147) or
      PhpFunctionComp(3631) or
      PhpFunctionComp(4214) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc209: TSynWebTokenKind;
begin
  if  PhpFunctionComp(237) or
      PhpFunctionComp(315) or
      PhpFunctionComp(432) or
      PhpFunctionComp(475) or
      PhpFunctionComp(573) or
      PhpFunctionComp(1121) or
      PhpFunctionComp(1195) or
      PhpFunctionComp(1227) or
      PhpFunctionComp(1610) or
      PhpFunctionComp(2290) or
      PhpFunctionComp(2514) or
      PhpFunctionComp(2678) or
      PhpFunctionComp(2998) or
      PhpFunctionComp(3268) or
      PhpFunctionComp(3270) or
      PhpFunctionComp(3444) or
      PhpFunctionComp(3868) or
      PhpFunctionComp(3959) or
      PhpFunctionComp(4018) or
      PhpFunctionComp(4082) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc210: TSynWebTokenKind;
begin
  if  PhpFunctionComp(566) or
      PhpFunctionComp(577) or
      PhpFunctionComp(733) or
      PhpFunctionComp(1315) or
      PhpFunctionComp(1453) or
      PhpFunctionComp(1475) or
      PhpFunctionComp(1568) or
      PhpFunctionComp(1657) or
      PhpFunctionComp(1820) or
      PhpFunctionComp(1834) or
      PhpFunctionComp(2127) or
      PhpFunctionComp(2459) or
      PhpFunctionComp(2478) or
      PhpFunctionComp(2806) or
      PhpFunctionComp(3088) or
      PhpFunctionComp(3134) or
      PhpFunctionComp(3604) or
      PhpFunctionComp(3699) or
      PhpFunctionComp(4069) or
      PhpFunctionComp(4185) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc211: TSynWebTokenKind;
begin
  if  PhpFunctionComp(180) or
      PhpFunctionComp(474) or
      PhpFunctionComp(492) or
      PhpFunctionComp(569) or
      PhpFunctionComp(579) or
      PhpFunctionComp(643) or
      PhpFunctionComp(734) or
      PhpFunctionComp(784) or
      PhpFunctionComp(860) or
      PhpFunctionComp(963) or
      PhpFunctionComp(1086) or
      PhpFunctionComp(1258) or
      PhpFunctionComp(1297) or
      PhpFunctionComp(1314) or
      PhpFunctionComp(1870) or
      PhpFunctionComp(2042) or
      PhpFunctionComp(2136) or
      PhpFunctionComp(2163) or
      PhpFunctionComp(2164) or
      PhpFunctionComp(2361) or
      PhpFunctionComp(2632) or
      PhpFunctionComp(2988) or
      PhpFunctionComp(3111) or
      PhpFunctionComp(4031) or
      PhpFunctionComp(4139) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc212: TSynWebTokenKind;
begin
  if  PhpFunctionComp(305) or
      PhpFunctionComp(480) or
      PhpFunctionComp(565) or
      PhpFunctionComp(732) or
      PhpFunctionComp(1130) or
      PhpFunctionComp(1256) or
      PhpFunctionComp(1362) or
      PhpFunctionComp(1574) or
      PhpFunctionComp(1712) or
      PhpFunctionComp(1933) or
      PhpFunctionComp(2023) or
      PhpFunctionComp(2221) or
      PhpFunctionComp(2697) or
      PhpFunctionComp(2709) or
      PhpFunctionComp(3059) or
      PhpFunctionComp(3335) or
      PhpFunctionComp(3648) or
      PhpFunctionComp(3721) or
      PhpFunctionComp(3992) or
      PhpFunctionComp(4023) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc213: TSynWebTokenKind;
begin
  if  PhpKeywordComp(4) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(140) or
        PhpFunctionComp(363) or
        PhpFunctionComp(767) or
        PhpFunctionComp(892) or
        PhpFunctionComp(903) or
        PhpFunctionComp(981) or
        PhpFunctionComp(987) or
        PhpFunctionComp(1139) or
        PhpFunctionComp(1192) or
        PhpFunctionComp(1238) or
        PhpFunctionComp(1255) or
        PhpFunctionComp(1268) or
        PhpFunctionComp(1708) or
        PhpFunctionComp(1963) or
        PhpFunctionComp(2017) or
        PhpFunctionComp(2157) or
        PhpFunctionComp(2177) or
        PhpFunctionComp(2288) or
        PhpFunctionComp(2417) or
        PhpFunctionComp(2444) or
        PhpFunctionComp(2461) or
        PhpFunctionComp(2491) or
        PhpFunctionComp(2776) or
        PhpFunctionComp(2780) or
        PhpFunctionComp(3750) or
        PhpFunctionComp(4027) or
        PhpFunctionComp(4385) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc214: TSynWebTokenKind;
begin
  if  PhpFunctionComp(255) or
      PhpFunctionComp(300) or
      PhpFunctionComp(328) or
      PhpFunctionComp(373) or
      PhpFunctionComp(464) or
      PhpFunctionComp(768) or
      PhpFunctionComp(813) or
      PhpFunctionComp(1382) or
      PhpFunctionComp(1451) or
      PhpFunctionComp(1509) or
      PhpFunctionComp(1647) or
      PhpFunctionComp(1845) or
      PhpFunctionComp(1961) or
      PhpFunctionComp(2138) or
      PhpFunctionComp(2451) or
      PhpFunctionComp(2493) or
      PhpFunctionComp(2549) or
      PhpFunctionComp(2569) or
      PhpFunctionComp(2993) or
      PhpFunctionComp(3023) or
      PhpFunctionComp(3122) or
      PhpFunctionComp(3124) or
      PhpFunctionComp(3195) or
      PhpFunctionComp(3278) or
      PhpFunctionComp(3392) or
      PhpFunctionComp(3515) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc215: TSynWebTokenKind;
begin
  if  PhpFunctionComp(478) or
      PhpFunctionComp(563) or
      PhpFunctionComp(636) or
      PhpFunctionComp(794) or
      PhpFunctionComp(840) or
      PhpFunctionComp(1631) or
      PhpFunctionComp(1892) or
      PhpFunctionComp(1894) or
      PhpFunctionComp(2325) or
      PhpFunctionComp(2517) or
      PhpFunctionComp(2921) or
      PhpFunctionComp(3045) or
      PhpFunctionComp(3121) or
      PhpFunctionComp(3187) or
      PhpFunctionComp(3192) or
      PhpFunctionComp(3434) or
      PhpFunctionComp(3624) or
      PhpFunctionComp(3830) or
      PhpFunctionComp(4050) or
      PhpFunctionComp(4202) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc216: TSynWebTokenKind;
begin
  if  PhpFunctionComp(453) or
      PhpFunctionComp(743) or
      PhpFunctionComp(1019) or
      PhpFunctionComp(1308) or
      PhpFunctionComp(1716) or
      PhpFunctionComp(1895) or
      PhpFunctionComp(1908) or
      PhpFunctionComp(1969) or
      PhpFunctionComp(2160) or
      PhpFunctionComp(2418) or
      PhpFunctionComp(2474) or
      PhpFunctionComp(2726) or
      PhpFunctionComp(2804) or
      PhpFunctionComp(2991) or
      PhpFunctionComp(3095) or
      PhpFunctionComp(3402) or
      PhpFunctionComp(3626) or
      PhpFunctionComp(3806) or
      PhpFunctionComp(3988) or
      PhpFunctionComp(4118) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc217: TSynWebTokenKind;
begin
  if  PhpFunctionComp(271) or
      PhpFunctionComp(379) or
      PhpFunctionComp(380) or
      PhpFunctionComp(1262) or
      PhpFunctionComp(1840) or
      PhpFunctionComp(1889) or
      PhpFunctionComp(1967) or
      PhpFunctionComp(2009) or
      PhpFunctionComp(2049) or
      PhpFunctionComp(2178) or
      PhpFunctionComp(2458) or
      PhpFunctionComp(2479) or
      PhpFunctionComp(2541) or
      PhpFunctionComp(2759) or
      PhpFunctionComp(3044) or
      PhpFunctionComp(3048) or
      PhpFunctionComp(3968) or
      PhpFunctionComp(3995) or
      PhpFunctionComp(4002) or
      PhpFunctionComp(4073) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc218: TSynWebTokenKind;
begin
  if  PhpFunctionComp(653) or
      PhpFunctionComp(886) or
      PhpFunctionComp(1181) or
      PhpFunctionComp(2241) or
      PhpFunctionComp(2292) or
      PhpFunctionComp(2362) or
      PhpFunctionComp(2704) or
      PhpFunctionComp(2846) or
      PhpFunctionComp(3037) or
      PhpFunctionComp(3167) or
      PhpFunctionComp(3180) or
      PhpFunctionComp(3230) or
      PhpFunctionComp(3290) or
      PhpFunctionComp(3473) or
      PhpFunctionComp(3733) or
      PhpFunctionComp(4005) or
      PhpFunctionComp(4025) or
      PhpFunctionComp(4070) or
      PhpFunctionComp(4083) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc219: TSynWebTokenKind;
begin
  if  PhpFunctionComp(132) or
      PhpFunctionComp(515) or
      PhpFunctionComp(632) or
      PhpFunctionComp(991) or
      PhpFunctionComp(1288) or
      PhpFunctionComp(1541) or
      PhpFunctionComp(1564) or
      PhpFunctionComp(1710) or
      PhpFunctionComp(1772) or
      PhpFunctionComp(1871) or
      PhpFunctionComp(1994) or
      PhpFunctionComp(2247) or
      PhpFunctionComp(2494) or
      PhpFunctionComp(3273) or
      PhpFunctionComp(3323) or
      PhpFunctionComp(3458) or
      PhpFunctionComp(3693) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc220: TSynWebTokenKind;
begin
  if  PhpFunctionComp(635) or
      PhpFunctionComp(761) or
      PhpFunctionComp(885) or
      PhpFunctionComp(887) or
      PhpFunctionComp(980) or
      PhpFunctionComp(1173) or
      PhpFunctionComp(1257) or
      PhpFunctionComp(1926) or
      PhpFunctionComp(2010) or
      PhpFunctionComp(2024) or
      PhpFunctionComp(2173) or
      PhpFunctionComp(2194) or
      PhpFunctionComp(2673) or
      PhpFunctionComp(3211) or
      PhpFunctionComp(3400) or
      PhpFunctionComp(4340) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc221: TSynWebTokenKind;
begin
  if  PhpFunctionComp(441) or
      PhpFunctionComp(763) or
      PhpFunctionComp(997) or
      PhpFunctionComp(1454) or
      PhpFunctionComp(1833) or
      PhpFunctionComp(1914) or
      PhpFunctionComp(2026) or
      PhpFunctionComp(2182) or
      PhpFunctionComp(2548) or
      PhpFunctionComp(2562) or
      PhpFunctionComp(2574) or
      PhpFunctionComp(2839) or
      PhpFunctionComp(2842) or
      PhpFunctionComp(2856) or
      PhpFunctionComp(2949) or
      PhpFunctionComp(3060) or
      PhpFunctionComp(3137) or
      PhpFunctionComp(3559) or
      PhpFunctionComp(4038) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc222: TSynWebTokenKind;
begin
  if  PhpFunctionComp(273) or
      PhpFunctionComp(306) or
      PhpFunctionComp(576) or
      PhpFunctionComp(773) or
      PhpFunctionComp(833) or
      PhpFunctionComp(1059) or
      PhpFunctionComp(1137) or
      PhpFunctionComp(1634) or
      PhpFunctionComp(2421) or
      PhpFunctionComp(2446) or
      PhpFunctionComp(2462) or
      PhpFunctionComp(2908) or
      PhpFunctionComp(2950) or
      PhpFunctionComp(2967) or
      PhpFunctionComp(3136) or
      PhpFunctionComp(3191) or
      PhpFunctionComp(3229) or
      PhpFunctionComp(3539) or
      PhpFunctionComp(3903) or
      PhpFunctionComp(3987) or
      PhpFunctionComp(4264) or
      PhpFunctionComp(4332) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc223: TSynWebTokenKind;
begin
  if  PhpFunctionComp(546) or
      PhpFunctionComp(557) or
      PhpFunctionComp(705) or
      PhpFunctionComp(760) or
      PhpFunctionComp(824) or
      PhpFunctionComp(970) or
      PhpFunctionComp(1270) or
      PhpFunctionComp(1283) or
      PhpFunctionComp(1477) or
      PhpFunctionComp(1505) or
      PhpFunctionComp(1549) or
      PhpFunctionComp(1850) or
      PhpFunctionComp(1888) or
      PhpFunctionComp(2058) or
      PhpFunctionComp(2118) or
      PhpFunctionComp(2204) or
      PhpFunctionComp(2724) or
      PhpFunctionComp(2849) or
      PhpFunctionComp(2851) or
      PhpFunctionComp(3186) or
      PhpFunctionComp(3344) or
      PhpFunctionComp(3408) or
      PhpFunctionComp(3410) or
      PhpFunctionComp(3438) or
      PhpFunctionComp(3467) or
      PhpFunctionComp(3694) or
      PhpFunctionComp(3738) or
      PhpFunctionComp(3817) or
      PhpFunctionComp(3986) or
      PhpFunctionComp(3999) or
      PhpFunctionComp(4035) or
      PhpFunctionComp(4084) or
      PhpFunctionComp(4119) or
      PhpFunctionComp(4203) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc224: TSynWebTokenKind;
begin
  if  PhpFunctionComp(135) or
      PhpFunctionComp(286) or
      PhpFunctionComp(835) or
      PhpFunctionComp(842) or
      PhpFunctionComp(1222) or
      PhpFunctionComp(1354) or
      PhpFunctionComp(2117) or
      PhpFunctionComp(2223) or
      PhpFunctionComp(2366) or
      PhpFunctionComp(2422) or
      PhpFunctionComp(2501) or
      PhpFunctionComp(2763) or
      PhpFunctionComp(3028) or
      PhpFunctionComp(3085) or
      PhpFunctionComp(3182) or
      PhpFunctionComp(3365) or
      PhpFunctionComp(3407) or
      PhpFunctionComp(4019) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc225: TSynWebTokenKind;
begin
  if  PhpFunctionComp(427) or
      PhpFunctionComp(1274) or
      PhpFunctionComp(2015) or
      PhpFunctionComp(2268) or
      PhpFunctionComp(2471) or
      PhpFunctionComp(2545) or
      PhpFunctionComp(2558) or
      PhpFunctionComp(2685) or
      PhpFunctionComp(3231) or
      PhpFunctionComp(3274) or
      PhpFunctionComp(3299) or
      PhpFunctionComp(3495) or
      PhpFunctionComp(3744) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc226: TSynWebTokenKind;
begin
  if  PhpFunctionComp(292) or
      PhpFunctionComp(386) or
      PhpFunctionComp(649) or
      PhpFunctionComp(1197) or
      PhpFunctionComp(1219) or
      PhpFunctionComp(1239) or
      PhpFunctionComp(1378) or
      PhpFunctionComp(1586) or
      PhpFunctionComp(2137) or
      PhpFunctionComp(2142) or
      PhpFunctionComp(2243) or
      PhpFunctionComp(2286) or
      PhpFunctionComp(2600) or
      PhpFunctionComp(2770) or
      PhpFunctionComp(3073) or
      PhpFunctionComp(3178) or
      PhpFunctionComp(3324) or
      PhpFunctionComp(3364) or
      PhpFunctionComp(4207) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc227: TSynWebTokenKind;
begin
  if  PhpFunctionComp(184) or
      PhpFunctionComp(349) or
      PhpFunctionComp(358) or
      PhpFunctionComp(727) or
      PhpFunctionComp(782) or
      PhpFunctionComp(828) or
      PhpFunctionComp(839) or
      PhpFunctionComp(883) or
      PhpFunctionComp(1166) or
      PhpFunctionComp(1246) or
      PhpFunctionComp(1259) or
      PhpFunctionComp(1501) or
      PhpFunctionComp(1896) or
      PhpFunctionComp(2044) or
      PhpFunctionComp(2151) or
      PhpFunctionComp(2172) or
      PhpFunctionComp(2259) or
      PhpFunctionComp(2416) or
      PhpFunctionComp(2732) or
      PhpFunctionComp(2834) or
      PhpFunctionComp(2858) or
      PhpFunctionComp(2964) or
      PhpFunctionComp(3050) or
      PhpFunctionComp(3126) or
      PhpFunctionComp(3600) or
      PhpFunctionComp(3801) or
      PhpFunctionComp(4343) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc228: TSynWebTokenKind;
begin
  if  PhpFunctionComp(259) or
      PhpFunctionComp(523) or
      PhpFunctionComp(595) or
      PhpFunctionComp(783) or
      PhpFunctionComp(793) or
      PhpFunctionComp(1481) or
      PhpFunctionComp(1489) or
      PhpFunctionComp(1491) or
      PhpFunctionComp(1771) or
      PhpFunctionComp(1920) or
      PhpFunctionComp(1930) or
      PhpFunctionComp(2515) or
      PhpFunctionComp(2992) or
      PhpFunctionComp(3071) or
      PhpFunctionComp(3413) or
      PhpFunctionComp(3440) or
      PhpFunctionComp(3637) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc229: TSynWebTokenKind;
begin
  if  PhpFunctionComp(95) or
      PhpFunctionComp(458) or
      PhpFunctionComp(624) or
      PhpFunctionComp(724) or
      PhpFunctionComp(744) or
      PhpFunctionComp(779) or
      PhpFunctionComp(992) or
      PhpFunctionComp(1253) or
      PhpFunctionComp(1320) or
      PhpFunctionComp(1353) or
      PhpFunctionComp(1471) or
      PhpFunctionComp(1499) or
      PhpFunctionComp(1551) or
      PhpFunctionComp(1770) or
      PhpFunctionComp(2130) or
      PhpFunctionComp(2260) or
      PhpFunctionComp(2298) or
      PhpFunctionComp(2753) or
      PhpFunctionComp(3011) or
      PhpFunctionComp(3063) or
      PhpFunctionComp(3593) or
      PhpFunctionComp(3783) or
      PhpFunctionComp(3852) or
      PhpFunctionComp(4077) or
      PhpFunctionComp(4193) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc230: TSynWebTokenKind;
begin
  if  PhpFunctionComp(239) or
      PhpFunctionComp(326) or
      PhpFunctionComp(544) or
      PhpFunctionComp(548) or
      PhpFunctionComp(704) or
      PhpFunctionComp(751) or
      PhpFunctionComp(1232) or
      PhpFunctionComp(1611) or
      PhpFunctionComp(1670) or
      PhpFunctionComp(2199) or
      PhpFunctionComp(2205) or
      PhpFunctionComp(2293) or
      PhpFunctionComp(2555) or
      PhpFunctionComp(2990) or
      PhpFunctionComp(3054) or
      PhpFunctionComp(3480) or
      PhpFunctionComp(3594) or
      PhpFunctionComp(3822) or
      PhpFunctionComp(3961) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc231: TSynWebTokenKind;
begin
  if  PhpFunctionComp(403) or
      PhpFunctionComp(622) or
      PhpFunctionComp(762) or
      PhpFunctionComp(1185) or
      PhpFunctionComp(1538) or
      PhpFunctionComp(1600) or
      PhpFunctionComp(1852) or
      PhpFunctionComp(2513) or
      PhpFunctionComp(2953) or
      PhpFunctionComp(3010) or
      PhpFunctionComp(3138) or
      PhpFunctionComp(3456) or
      PhpFunctionComp(3632) or
      PhpFunctionComp(4086) or
      PhpFunctionComp(4218) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc232: TSynWebTokenKind;
begin
  if  PhpFunctionComp(330) or
      PhpFunctionComp(481) or
      PhpFunctionComp(678) or
      PhpFunctionComp(757) or
      PhpFunctionComp(789) or
      PhpFunctionComp(1088) or
      PhpFunctionComp(1214) or
      PhpFunctionComp(1249) or
      PhpFunctionComp(1360) or
      PhpFunctionComp(1492) or
      PhpFunctionComp(1841) or
      PhpFunctionComp(1944) or
      PhpFunctionComp(2158) or
      PhpFunctionComp(2200) or
      PhpFunctionComp(2702) or
      PhpFunctionComp(2728) or
      PhpFunctionComp(3003) or
      PhpFunctionComp(3084) or
      PhpFunctionComp(3226) or
      PhpFunctionComp(3312) or
      PhpFunctionComp(4016) or
      PhpFunctionComp(4078) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc233: TSynWebTokenKind;
begin
  if  PhpFunctionComp(201) or
      PhpFunctionComp(680) or
      PhpFunctionComp(737) or
      PhpFunctionComp(798) or
      PhpFunctionComp(941) or
      PhpFunctionComp(1984) or
      PhpFunctionComp(2070) or
      PhpFunctionComp(2159) or
      PhpFunctionComp(2296) or
      PhpFunctionComp(2475) or
      PhpFunctionComp(2496) or
      PhpFunctionComp(2720) or
      PhpFunctionComp(2755) or
      PhpFunctionComp(3103) or
      PhpFunctionComp(3154) or
      PhpFunctionComp(3275) or
      PhpFunctionComp(3308) or
      PhpFunctionComp(3650) or
      PhpFunctionComp(4017) or
      PhpFunctionComp(4089) or
      PhpFunctionComp(4238) or
      PhpFunctionComp(4331) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc234: TSynWebTokenKind;
begin
  if  PhpFunctionComp(166) or
      PhpFunctionComp(850) or
      PhpFunctionComp(998) or
      PhpFunctionComp(1159) or
      PhpFunctionComp(1374) or
      PhpFunctionComp(1469) or
      PhpFunctionComp(1579) or
      PhpFunctionComp(1602) or
      PhpFunctionComp(1883) or
      PhpFunctionComp(2604) or
      PhpFunctionComp(2701) or
      PhpFunctionComp(3107) or
      PhpFunctionComp(3162) or
      PhpFunctionComp(3239) or
      PhpFunctionComp(3452) or
      PhpFunctionComp(3474) or
      PhpFunctionComp(3574) or
      PhpFunctionComp(3597) or
      PhpFunctionComp(3703) or
      PhpFunctionComp(3709) or
      PhpFunctionComp(4004) or
      PhpFunctionComp(4065) or
      PhpFunctionComp(4255) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc235: TSynWebTokenKind;
begin
  if  PhpFunctionComp(312) or
      PhpFunctionComp(719) or
      PhpFunctionComp(754) or
      PhpFunctionComp(778) or
      PhpFunctionComp(964) or
      PhpFunctionComp(1206) or
      PhpFunctionComp(1318) or
      PhpFunctionComp(1582) or
      PhpFunctionComp(1832) or
      PhpFunctionComp(2020) or
      PhpFunctionComp(2313) or
      PhpFunctionComp(2473) or
      PhpFunctionComp(2687) or
      PhpFunctionComp(2866) or
      PhpFunctionComp(2947) or
      PhpFunctionComp(2989) or
      PhpFunctionComp(3113) or
      PhpFunctionComp(3374) or
      PhpFunctionComp(3745) or
      PhpFunctionComp(3782) or
      PhpFunctionComp(4241) or
      PhpFunctionComp(4394) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc236: TSynWebTokenKind;
begin
  if  PhpFunctionComp(137) or
      PhpFunctionComp(634) or
      PhpFunctionComp(700) or
      PhpFunctionComp(774) or
      PhpFunctionComp(854) or
      PhpFunctionComp(971) or
      PhpFunctionComp(983) or
      PhpFunctionComp(1287) or
      PhpFunctionComp(1309) or
      PhpFunctionComp(1313) or
      PhpFunctionComp(1524) or
      PhpFunctionComp(1537) or
      PhpFunctionComp(1767) or
      PhpFunctionComp(1791) or
      PhpFunctionComp(1817) or
      PhpFunctionComp(2261) or
      PhpFunctionComp(2262) or
      PhpFunctionComp(2434) or
      PhpFunctionComp(2706) or
      PhpFunctionComp(3367) or
      PhpFunctionComp(3591) or
      PhpFunctionComp(3963) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc237: TSynWebTokenKind;
begin
  if  PhpFunctionComp(369) or
      PhpFunctionComp(633) or
      PhpFunctionComp(937) or
      PhpFunctionComp(1001) or
      PhpFunctionComp(1828) or
      PhpFunctionComp(2165) or
      PhpFunctionComp(2490) or
      PhpFunctionComp(2760) or
      PhpFunctionComp(3478) or
      PhpFunctionComp(4081) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc238: TSynWebTokenKind;
begin
  if  PhpFunctionComp(374) or
      PhpFunctionComp(477) or
      PhpFunctionComp(711) or
      PhpFunctionComp(1237) or
      PhpFunctionComp(1286) or
      PhpFunctionComp(1478) or
      PhpFunctionComp(1576) or
      PhpFunctionComp(2179) or
      PhpFunctionComp(2254) or
      PhpFunctionComp(2282) or
      PhpFunctionComp(2409) or
      PhpFunctionComp(2571) or
      PhpFunctionComp(2665) or
      PhpFunctionComp(3025) or
      PhpFunctionComp(3043) or
      PhpFunctionComp(3235) or
      PhpFunctionComp(3353) or
      PhpFunctionComp(3370) or
      PhpFunctionComp(3620) or
      PhpFunctionComp(4011) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc239: TSynWebTokenKind;
begin
  if  PhpFunctionComp(123) or
      PhpFunctionComp(1233) or
      PhpFunctionComp(1572) or
      PhpFunctionComp(1619) or
      PhpFunctionComp(1656) or
      PhpFunctionComp(1679) or
      PhpFunctionComp(1988) or
      PhpFunctionComp(2116) or
      PhpFunctionComp(2371) or
      PhpFunctionComp(2423) or
      PhpFunctionComp(2546) or
      PhpFunctionComp(2865) or
      PhpFunctionComp(3719) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc240: TSynWebTokenKind;
begin
  if  PhpFunctionComp(134) or
      PhpFunctionComp(164) or
      PhpFunctionComp(293) or
      PhpFunctionComp(411) or
      PhpFunctionComp(489) or
      PhpFunctionComp(585) or
      PhpFunctionComp(667) or
      PhpFunctionComp(958) or
      PhpFunctionComp(1134) or
      PhpFunctionComp(1267) or
      PhpFunctionComp(1447) or
      PhpFunctionComp(1612) or
      PhpFunctionComp(1788) or
      PhpFunctionComp(1922) or
      PhpFunctionComp(2021) or
      PhpFunctionComp(2156) or
      PhpFunctionComp(2358) or
      PhpFunctionComp(2572) or
      PhpFunctionComp(3352) or
      PhpFunctionComp(3491) or
      PhpFunctionComp(4115) or
      PhpFunctionComp(4392) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc241: TSynWebTokenKind;
begin
  if  PhpFunctionComp(742) or
      PhpFunctionComp(1247) or
      PhpFunctionComp(1321) or
      PhpFunctionComp(1479) or
      PhpFunctionComp(1536) or
      PhpFunctionComp(1858) or
      PhpFunctionComp(1915) or
      PhpFunctionComp(2258) or
      PhpFunctionComp(2433) or
      PhpFunctionComp(2454) or
      PhpFunctionComp(2457) or
      PhpFunctionComp(2618) or
      PhpFunctionComp(2671) or
      PhpFunctionComp(2737) or
      PhpFunctionComp(3098) or
      PhpFunctionComp(3562) or
      PhpFunctionComp(3710) or
      PhpFunctionComp(3800) or
      PhpFunctionComp(4194) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc242: TSynWebTokenKind;
begin
  if  PhpFunctionComp(96) or
      PhpFunctionComp(274) or
      PhpFunctionComp(490) or
      PhpFunctionComp(656) or
      PhpFunctionComp(738) or
      PhpFunctionComp(1177) or
      PhpFunctionComp(1191) or
      PhpFunctionComp(1302) or
      PhpFunctionComp(1946) or
      PhpFunctionComp(2143) or
      PhpFunctionComp(2283) or
      PhpFunctionComp(2441) or
      PhpFunctionComp(2463) or
      PhpFunctionComp(2830) or
      PhpFunctionComp(2951) or
      PhpFunctionComp(3468) or
      PhpFunctionComp(4036) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc243: TSynWebTokenKind;
begin
  if  PhpFunctionComp(80) or
      PhpFunctionComp(449) or
      PhpFunctionComp(476) or
      PhpFunctionComp(1071) or
      PhpFunctionComp(1628) or
      PhpFunctionComp(1869) or
      PhpFunctionComp(2965) or
      PhpFunctionComp(3443) or
      PhpFunctionComp(3606) or
      PhpFunctionComp(3689) or
      PhpFunctionComp(4037) or
      PhpFunctionComp(4134) or
      PhpFunctionComp(4190) or
      PhpFunctionComp(4377) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc244: TSynWebTokenKind;
begin
  if  PhpFunctionComp(575) or
      PhpFunctionComp(752) or
      PhpFunctionComp(979) or
      PhpFunctionComp(1248) or
      PhpFunctionComp(1364) or
      PhpFunctionComp(1629) or
      PhpFunctionComp(1938) or
      PhpFunctionComp(2131) or
      PhpFunctionComp(3056) or
      PhpFunctionComp(3401) or
      PhpFunctionComp(3460) or
      PhpFunctionComp(3751) or
      PhpFunctionComp(4022) or
      PhpFunctionComp(4034) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc245: TSynWebTokenKind;
begin
  if  PhpFunctionComp(795) or
      PhpFunctionComp(1236) or
      PhpFunctionComp(1782) or
      PhpFunctionComp(1893) or
      PhpFunctionComp(2125) or
      PhpFunctionComp(2277) or
      PhpFunctionComp(2336) or
      PhpFunctionComp(2381) or
      PhpFunctionComp(2559) or
      PhpFunctionComp(2748) or
      PhpFunctionComp(3082) or
      PhpFunctionComp(3424) or
      PhpFunctionComp(3746) or
      PhpFunctionComp(3823) or
      PhpFunctionComp(4292) or
      PhpFunctionComp(4360) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc246: TSynWebTokenKind;
begin
  if  PhpFunctionComp(98) or
      PhpFunctionComp(580) or
      PhpFunctionComp(1265) or
      PhpFunctionComp(1835) or
      PhpFunctionComp(1929) or
      PhpFunctionComp(2052) or
      PhpFunctionComp(2287) or
      PhpFunctionComp(3294) or
      PhpFunctionComp(3469) or
      PhpFunctionComp(3482) or
      PhpFunctionComp(3519) or
      PhpFunctionComp(4099) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc247: TSynWebTokenKind;
begin
  if  PhpKeywordComp(60) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(150) or
        PhpFunctionComp(753) or
        PhpFunctionComp(888) or
        PhpFunctionComp(1136) or
        PhpFunctionComp(1851) or
        PhpFunctionComp(1899) or
        PhpFunctionComp(2303) or
        PhpFunctionComp(2329) or
        PhpFunctionComp(2464) or
        PhpFunctionComp(2472) or
        PhpFunctionComp(2560) or
        PhpFunctionComp(2855) or
        PhpFunctionComp(3243) or
        PhpFunctionComp(3770) or
        PhpFunctionComp(3962) or
        PhpFunctionComp(4090) or
        PhpFunctionComp(4121) or
        PhpFunctionComp(4361) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc248: TSynWebTokenKind;
begin
  if  PhpFunctionComp(741) or
      PhpFunctionComp(873) or
      PhpFunctionComp(898) or
      PhpFunctionComp(1083) or
      PhpFunctionComp(1186) or
      PhpFunctionComp(1260) or
      PhpFunctionComp(1278) or
      PhpFunctionComp(1527) or
      PhpFunctionComp(1630) or
      PhpFunctionComp(1635) or
      PhpFunctionComp(2045) or
      PhpFunctionComp(2096) or
      PhpFunctionComp(2488) or
      PhpFunctionComp(2521) or
      PhpFunctionComp(2522) or
      PhpFunctionComp(2727) or
      PhpFunctionComp(3267) or
      PhpFunctionComp(3776) or
      PhpFunctionComp(3833) or
      PhpFunctionComp(4045) or
      PhpFunctionComp(4273) or
      PhpFunctionComp(4311) or
      PhpFunctionComp(4344) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc249: TSynWebTokenKind;
begin
  if  PhpFunctionComp(91) or
      PhpFunctionComp(94) or
      PhpFunctionComp(301) or
      PhpFunctionComp(796) or
      PhpFunctionComp(955) or
      PhpFunctionComp(1176) or
      PhpFunctionComp(1636) or
      PhpFunctionComp(1781) or
      PhpFunctionComp(2103) or
      PhpFunctionComp(2577) or
      PhpFunctionComp(2887) or
      PhpFunctionComp(3017) or
      PhpFunctionComp(3971) or
      PhpFunctionComp(4024) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc250: TSynWebTokenKind;
begin
  if  PhpKeywordComp(2) then
    Result := stkPhpKeyword
  else
    if  PhpFunctionComp(428) or
        PhpFunctionComp(1076) or
        PhpFunctionComp(1322) or
        PhpFunctionComp(2110) or
        PhpFunctionComp(2174) or
        PhpFunctionComp(2542) or
        PhpFunctionComp(2871) or
        PhpFunctionComp(3319) or
        PhpFunctionComp(3390) or
        PhpFunctionComp(3430) or
        PhpFunctionComp(3814) or
        PhpFunctionComp(4033) or
        PhpFunctionComp(4192) then
      Result := stkPhpFunction
    else
      Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc251: TSynWebTokenKind;
begin
  if  PhpFunctionComp(109) or
      PhpFunctionComp(343) or
      PhpFunctionComp(1282) or
      PhpFunctionComp(1319) or
      PhpFunctionComp(1627) or
      PhpFunctionComp(2043) or
      PhpFunctionComp(2128) or
      PhpFunctionComp(2201) or
      PhpFunctionComp(2489) or
      PhpFunctionComp(2688) or
      PhpFunctionComp(2757) or
      PhpFunctionComp(2758) or
      PhpFunctionComp(3051) or
      PhpFunctionComp(3222) or
      PhpFunctionComp(3288) or
      PhpFunctionComp(3315) or
      PhpFunctionComp(3966) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc252: TSynWebTokenKind;
begin
  if  PhpFunctionComp(302) or
      PhpFunctionComp(1080) or
      PhpFunctionComp(1271) or
      PhpFunctionComp(1780) or
      PhpFunctionComp(2057) or
      PhpFunctionComp(2202) or
      PhpFunctionComp(2245) or
      PhpFunctionComp(2270) or
      PhpFunctionComp(2754) or
      PhpFunctionComp(2956) or
      PhpFunctionComp(3009) or
      PhpFunctionComp(4196) or
      PhpFunctionComp(4253) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc253: TSynWebTokenKind;
begin
  if  PhpFunctionComp(472) or
      PhpFunctionComp(799) or
      PhpFunctionComp(1072) or
      PhpFunctionComp(1122) or
      PhpFunctionComp(1298) or
      PhpFunctionComp(1317) or
      PhpFunctionComp(1986) or
      PhpFunctionComp(2073) or
      PhpFunctionComp(2135) or
      PhpFunctionComp(2198) or
      PhpFunctionComp(2483) or
      PhpFunctionComp(2699) or
      PhpFunctionComp(2705) or
      PhpFunctionComp(3741) or
      PhpFunctionComp(3964) or
      PhpFunctionComp(4088) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc254: TSynWebTokenKind;
begin
  if  PhpFunctionComp(345) or
      PhpFunctionComp(594) or
      PhpFunctionComp(999) or
      PhpFunctionComp(1677) or
      PhpFunctionComp(2054) or
      PhpFunctionComp(2175) or
      PhpFunctionComp(2337) or
      PhpFunctionComp(2533) or
      PhpFunctionComp(2765) or
      PhpFunctionComp(2957) or
      PhpFunctionComp(3079) or
      PhpFunctionComp(3561) or
      PhpFunctionComp(3629) or
      PhpFunctionComp(3885) or
      PhpFunctionComp(4095) or
      PhpFunctionComp(4235) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc255: TSynWebTokenKind;
begin
  if  PhpFunctionComp(414) or
      PhpFunctionComp(450) or
      PhpFunctionComp(493) or
      PhpFunctionComp(759) or
      PhpFunctionComp(1316) or
      PhpFunctionComp(1738) or
      PhpFunctionComp(2456) or
      PhpFunctionComp(2583) or
      PhpFunctionComp(2961) or
      PhpFunctionComp(3455) or
      PhpFunctionComp(4021) or
      PhpFunctionComp(4064) or
      PhpFunctionComp(4199) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc256: TSynWebTokenKind;
begin
  if  PhpFunctionComp(807) or
      PhpFunctionComp(1131) or
      PhpFunctionComp(2120) or
      PhpFunctionComp(2121) or
      PhpFunctionComp(2203) or
      PhpFunctionComp(2206) or
      PhpFunctionComp(2410) or
      PhpFunctionComp(2837) or
      PhpFunctionComp(2884) or
      PhpFunctionComp(3118) or
      PhpFunctionComp(3584) or
      PhpFunctionComp(3818) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc257: TSynWebTokenKind;
begin
  if  PhpFunctionComp(410) or
      PhpFunctionComp(781) or
      PhpFunctionComp(1087) or
      PhpFunctionComp(1931) or
      PhpFunctionComp(1950) or
      PhpFunctionComp(1962) or
      PhpFunctionComp(2134) or
      PhpFunctionComp(2217) or
      PhpFunctionComp(2256) or
      PhpFunctionComp(2440) or
      PhpFunctionComp(2506) or
      PhpFunctionComp(2526) or
      PhpFunctionComp(2601) or
      PhpFunctionComp(3164) or
      PhpFunctionComp(4170) or
      PhpFunctionComp(4393) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc258: TSynWebTokenKind;
begin
  if  PhpFunctionComp(984) or
      PhpFunctionComp(1766) or
      PhpFunctionComp(1958) or
      PhpFunctionComp(2180) or
      PhpFunctionComp(2257) or
      PhpFunctionComp(2276) or
      PhpFunctionComp(3338) or
      PhpFunctionComp(3454) or
      PhpFunctionComp(4244) or
      PhpFunctionComp(4272) or
      PhpFunctionComp(4368) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc259: TSynWebTokenKind;
begin
  if  PhpFunctionComp(516) or
      PhpFunctionComp(790) or
      PhpFunctionComp(837) or
      PhpFunctionComp(1154) or
      PhpFunctionComp(1448) or
      PhpFunctionComp(1460) or
      PhpFunctionComp(1855) or
      PhpFunctionComp(2197) or
      PhpFunctionComp(2253) or
      PhpFunctionComp(2539) or
      PhpFunctionComp(2553) or
      PhpFunctionComp(2602) or
      PhpFunctionComp(3065) or
      PhpFunctionComp(3414) or
      PhpFunctionComp(3426) or
      PhpFunctionComp(3753) or
      PhpFunctionComp(3837) or
      PhpFunctionComp(3853) or
      PhpFunctionComp(4066) or
      PhpFunctionComp(4130) or
      PhpFunctionComp(4371) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc260: TSynWebTokenKind;
begin
  if  PhpFunctionComp(659) or
      PhpFunctionComp(1941) or
      PhpFunctionComp(2140) or
      PhpFunctionComp(2145) or
      PhpFunctionComp(2497) or
      PhpFunctionComp(3055) or
      PhpFunctionComp(3314) or
      PhpFunctionComp(3318) or
      PhpFunctionComp(3420) or
      PhpFunctionComp(3596) or
      PhpFunctionComp(3684) or
      PhpFunctionComp(3807) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc261: TSynWebTokenKind;
begin
  if  PhpFunctionComp(165) or
      PhpFunctionComp(560) or
      PhpFunctionComp(1145) or
      PhpFunctionComp(1324) or
      PhpFunctionComp(1632) or
      PhpFunctionComp(1765) or
      PhpFunctionComp(1783) or
      PhpFunctionComp(1844) or
      PhpFunctionComp(2014) or
      PhpFunctionComp(2102) or
      PhpFunctionComp(2237) or
      PhpFunctionComp(2746) or
      PhpFunctionComp(2812) or
      PhpFunctionComp(3310) or
      PhpFunctionComp(3661) or
      PhpFunctionComp(3739) or
      PhpFunctionComp(4388) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc262: TSynWebTokenKind;
begin
  if  PhpFunctionComp(106) or
      PhpFunctionComp(1340) or
      PhpFunctionComp(1525) or
      PhpFunctionComp(1876) or
      PhpFunctionComp(2263) or
      PhpFunctionComp(2302) or
      PhpFunctionComp(2487) or
      PhpFunctionComp(2561) or
      PhpFunctionComp(2608) or
      PhpFunctionComp(2875) or
      PhpFunctionComp(3447) or
      PhpFunctionComp(3481) or
      PhpFunctionComp(3778) or
      PhpFunctionComp(3813) or
      PhpFunctionComp(3967) or
      PhpFunctionComp(4201) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc263: TSynWebTokenKind;
begin
  if  PhpFunctionComp(972) or
      PhpFunctionComp(1562) or
      PhpFunctionComp(1626) or
      PhpFunctionComp(2284) or
      PhpFunctionComp(2382) or
      PhpFunctionComp(2499) or
      PhpFunctionComp(2843) or
      PhpFunctionComp(2885) or
      PhpFunctionComp(3379) or
      PhpFunctionComp(4215) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc264: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1055) or
      PhpFunctionComp(1942) or
      PhpFunctionComp(1979) or
      PhpFunctionComp(1991) or
      PhpFunctionComp(2171) or
      PhpFunctionComp(2379) or
      PhpFunctionComp(2981) or
      PhpFunctionComp(3171) or
      PhpFunctionComp(3704) or
      PhpFunctionComp(3742) or
      PhpFunctionComp(3858) or
      PhpFunctionComp(3877) or
      PhpFunctionComp(4079) or
      PhpFunctionComp(4291) or
      PhpFunctionComp(4341) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc265: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1671) or
      PhpFunctionComp(1739) or
      PhpFunctionComp(1978) or
      PhpFunctionComp(2252) or
      PhpFunctionComp(2524) or
      PhpFunctionComp(2703) or
      PhpFunctionComp(2897) or
      PhpFunctionComp(3027) or
      PhpFunctionComp(3404) or
      PhpFunctionComp(3505) or
      PhpFunctionComp(3786) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc266: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1862) or
      PhpFunctionComp(1877) or
      PhpFunctionComp(1985) or
      PhpFunctionComp(2331) or
      PhpFunctionComp(2333) or
      PhpFunctionComp(2343) or
      PhpFunctionComp(2509) or
      PhpFunctionComp(3224) or
      PhpFunctionComp(3342) or
      PhpFunctionComp(3621) or
      PhpFunctionComp(3789) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc267: TSynWebTokenKind;
begin
  if  PhpFunctionComp(355) or
      PhpFunctionComp(416) or
      PhpFunctionComp(479) or
      PhpFunctionComp(1583) or
      PhpFunctionComp(1977) or
      PhpFunctionComp(2334) or
      PhpFunctionComp(2350) or
      PhpFunctionComp(2523) or
      PhpFunctionComp(2535) or
      PhpFunctionComp(3775) or
      PhpFunctionComp(3802) or
      PhpFunctionComp(3832) or
      PhpFunctionComp(3913) or
      PhpFunctionComp(3920) or
      PhpFunctionComp(4286) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc268: TSynWebTokenKind;
begin
  if  PhpFunctionComp(459) or
      PhpFunctionComp(982) or
      PhpFunctionComp(1168) or
      PhpFunctionComp(1280) or
      PhpFunctionComp(1339) or
      PhpFunctionComp(2328) or
      PhpFunctionComp(3316) or
      PhpFunctionComp(3592) or
      PhpFunctionComp(3690) or
      PhpFunctionComp(3826) or
      PhpFunctionComp(3883) or
      PhpFunctionComp(4012) or
      PhpFunctionComp(4046) or
      PhpFunctionComp(4283) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc269: TSynWebTokenKind;
begin
  if  PhpFunctionComp(233) or
      PhpFunctionComp(461) or
      PhpFunctionComp(484) or
      PhpFunctionComp(1476) or
      PhpFunctionComp(1637) or
      PhpFunctionComp(1949) or
      PhpFunctionComp(2469) or
      PhpFunctionComp(2958) or
      PhpFunctionComp(3067) or
      PhpFunctionComp(4182) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc270: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1338) or
      PhpFunctionComp(1528) or
      PhpFunctionComp(1757) or
      PhpFunctionComp(2305) or
      PhpFunctionComp(2544) or
      PhpFunctionComp(2761) or
      PhpFunctionComp(2869) or
      PhpFunctionComp(3022) or
      PhpFunctionComp(3791) or
      PhpFunctionComp(4028) or
      PhpFunctionComp(4281) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc271: TSynWebTokenKind;
begin
  if  PhpFunctionComp(491) or
      PhpFunctionComp(495) or
      PhpFunctionComp(1661) or
      PhpFunctionComp(1796) or
      PhpFunctionComp(1866) or
      PhpFunctionComp(2105) or
      PhpFunctionComp(2108) or
      PhpFunctionComp(2234) or
      PhpFunctionComp(2369) or
      PhpFunctionComp(2662) or
      PhpFunctionComp(2867) or
      PhpFunctionComp(3602) or
      PhpFunctionComp(3902) or
      PhpFunctionComp(3916) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc272: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1135) or
      PhpFunctionComp(1490) or
      PhpFunctionComp(2356) or
      PhpFunctionComp(2547) or
      PhpFunctionComp(2686) or
      PhpFunctionComp(2873) or
      PhpFunctionComp(3541) or
      PhpFunctionComp(3601) or
      PhpFunctionComp(3790) or
      PhpFunctionComp(3816) or
      PhpFunctionComp(4131) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc273: TSynWebTokenKind;
begin
  if  PhpFunctionComp(100) or
      PhpFunctionComp(322) or
      PhpFunctionComp(654) or
      PhpFunctionComp(672) or
      PhpFunctionComp(874) or
      PhpFunctionComp(1943) or
      PhpFunctionComp(2031) or
      PhpFunctionComp(2573) or
      PhpFunctionComp(2689) or
      PhpFunctionComp(2859) or
      PhpFunctionComp(2901) or
      PhpFunctionComp(3115) or
      PhpFunctionComp(3331) or
      PhpFunctionComp(3625) or
      PhpFunctionComp(3695) or
      PhpFunctionComp(3811) or
      PhpFunctionComp(3849) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc274: TSynWebTokenKind;
begin
  if  PhpFunctionComp(145) or
      PhpFunctionComp(207) or
      PhpFunctionComp(335) or
      PhpFunctionComp(986) or
      PhpFunctionComp(1151) or
      PhpFunctionComp(1559) or
      PhpFunctionComp(1768) or
      PhpFunctionComp(1813) or
      PhpFunctionComp(2327) or
      PhpFunctionComp(2529) or
      PhpFunctionComp(2660) or
      PhpFunctionComp(3333) or
      PhpFunctionComp(4013) or
      PhpFunctionComp(4039) or
      PhpFunctionComp(4075) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc275: TSynWebTokenKind;
begin
  if  PhpFunctionComp(337) or
      PhpFunctionComp(1964) or
      PhpFunctionComp(2111) or
      PhpFunctionComp(2278) or
      PhpFunctionComp(2339) or
      PhpFunctionComp(3024) or
      PhpFunctionComp(3093) or
      PhpFunctionComp(4112) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc276: TSynWebTokenKind;
begin
  if  PhpFunctionComp(376) or
      PhpFunctionComp(1000) or
      PhpFunctionComp(1323) or
      PhpFunctionComp(1332) or
      PhpFunctionComp(1337) or
      PhpFunctionComp(1429) or
      PhpFunctionComp(1913) or
      PhpFunctionComp(2046) or
      PhpFunctionComp(2485) or
      PhpFunctionComp(2550) or
      PhpFunctionComp(2644) or
      PhpFunctionComp(2982) or
      PhpFunctionComp(3035) or
      PhpFunctionComp(3143) or
      PhpFunctionComp(3144) or
      PhpFunctionComp(3423) or
      PhpFunctionComp(3896) or
      PhpFunctionComp(4129) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc277: TSynWebTokenKind;
begin
  if  PhpFunctionComp(810) or
      PhpFunctionComp(820) or
      PhpFunctionComp(1482) or
      PhpFunctionComp(1959) or
      PhpFunctionComp(2218) or
      PhpFunctionComp(3030) or
      PhpFunctionComp(3549) or
      PhpFunctionComp(3569) or
      PhpFunctionComp(3611) or
      PhpFunctionComp(3705) or
      PhpFunctionComp(4076) or
      PhpFunctionComp(4327) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc278: TSynWebTokenKind;
begin
  if  PhpFunctionComp(101) or
      PhpFunctionComp(985) or
      PhpFunctionComp(1078) or
      PhpFunctionComp(1717) or
      PhpFunctionComp(1795) or
      PhpFunctionComp(1990) or
      PhpFunctionComp(2335) or
      PhpFunctionComp(2437) or
      PhpFunctionComp(2510) or
      PhpFunctionComp(2534) or
      PhpFunctionComp(2972) or
      PhpFunctionComp(3475) or
      PhpFunctionComp(3554) or
      PhpFunctionComp(3688) or
      PhpFunctionComp(4085) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc279: TSynWebTokenKind;
begin
  if  PhpFunctionComp(102) or
      PhpFunctionComp(708) or
      PhpFunctionComp(1912) or
      PhpFunctionComp(2190) or
      PhpFunctionComp(2222) or
      PhpFunctionComp(2280) or
      PhpFunctionComp(2363) or
      PhpFunctionComp(2374) or
      PhpFunctionComp(3360) or
      PhpFunctionComp(3453) or
      PhpFunctionComp(3691) or
      PhpFunctionComp(3731) or
      PhpFunctionComp(3754) or
      PhpFunctionComp(3831) or
      PhpFunctionComp(4068) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc280: TSynWebTokenKind;
begin
  if  PhpFunctionComp(202) or
      PhpFunctionComp(1910) or
      PhpFunctionComp(2436) or
      PhpFunctionComp(2674) or
      PhpFunctionComp(2872) or
      PhpFunctionComp(3313) or
      PhpFunctionComp(3603) or
      PhpFunctionComp(3706) or
      PhpFunctionComp(4014) or
      PhpFunctionComp(4133) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc281: TSynWebTokenKind;
begin
  if  PhpFunctionComp(127) or
      PhpFunctionComp(650) or
      PhpFunctionComp(2518) or
      PhpFunctionComp(3309) or
      PhpFunctionComp(3334) or
      PhpFunctionComp(3834) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc282: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1843) or
      PhpFunctionComp(2022) or
      PhpFunctionComp(2074) or
      PhpFunctionComp(2124) or
      PhpFunctionComp(2210) or
      PhpFunctionComp(2540) or
      PhpFunctionComp(2631) or
      PhpFunctionComp(2669) or
      PhpFunctionComp(2752) or
      PhpFunctionComp(3036) or
      PhpFunctionComp(3133) or
      PhpFunctionComp(3508) or
      PhpFunctionComp(3551) or
      PhpFunctionComp(3748) or
      PhpFunctionComp(3785) or
      PhpFunctionComp(3970) or
      PhpFunctionComp(4127) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc283: TSynWebTokenKind;
begin
  if  PhpFunctionComp(211) or
      PhpFunctionComp(348) or
      PhpFunctionComp(1911) or
      PhpFunctionComp(2207) or
      PhpFunctionComp(2285) or
      PhpFunctionComp(2394) or
      PhpFunctionComp(2460) or
      PhpFunctionComp(2690) or
      PhpFunctionComp(2868) or
      PhpFunctionComp(2955) or
      PhpFunctionComp(2986) or
      PhpFunctionComp(3108) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc284: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1735) or
      PhpFunctionComp(1740) or
      PhpFunctionComp(2667) or
      PhpFunctionComp(2771) or
      PhpFunctionComp(3387) or
      PhpFunctionComp(3514) or
      PhpFunctionComp(3787) or
      PhpFunctionComp(3904) or
      PhpFunctionComp(4100) or
      PhpFunctionComp(4246) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc285: TSynWebTokenKind;
begin
  if  PhpFunctionComp(421) or
      PhpFunctionComp(852) or
      PhpFunctionComp(1734) or
      PhpFunctionComp(2080) or
      PhpFunctionComp(2236) or
      PhpFunctionComp(2954) or
      PhpFunctionComp(3619) or
      PhpFunctionComp(3743) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc286: TSynWebTokenKind;
begin
  if  PhpFunctionComp(370) or
      PhpFunctionComp(418) or
      PhpFunctionComp(973) or
      PhpFunctionComp(1779) or
      PhpFunctionComp(2528) or
      PhpFunctionComp(3317) or
      PhpFunctionComp(3917) or
      PhpFunctionComp(4087) or
      PhpFunctionComp(4237) or
      PhpFunctionComp(4326) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc287: TSynWebTokenKind;
begin
  if  PhpFunctionComp(895) or
      PhpFunctionComp(1565) or
      PhpFunctionComp(1773) or
      PhpFunctionComp(2255) or
      PhpFunctionComp(2376) or
      PhpFunctionComp(2659) or
      PhpFunctionComp(2959) or
      PhpFunctionComp(2974) or
      PhpFunctionComp(3020) or
      PhpFunctionComp(3532) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc288: TSynWebTokenKind;
begin
  if  PhpFunctionComp(797) or
      PhpFunctionComp(2468) or
      PhpFunctionComp(2876) or
      PhpFunctionComp(2962) or
      PhpFunctionComp(3566) or
      PhpFunctionComp(3839) or
      PhpFunctionComp(4243) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc289: TSynWebTokenKind;
begin
  if  PhpFunctionComp(278) or
      PhpFunctionComp(288) or
      PhpFunctionComp(811) or
      PhpFunctionComp(2088) or
      PhpFunctionComp(2269) or
      PhpFunctionComp(3322) or
      PhpFunctionComp(3464) or
      PhpFunctionComp(3824) or
      PhpFunctionComp(4135) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc290: TSynWebTokenKind;
begin
  if  PhpFunctionComp(130) or
      PhpFunctionComp(321) or
      PhpFunctionComp(1633) or
      PhpFunctionComp(1865) or
      PhpFunctionComp(1955) or
      PhpFunctionComp(2624) or
      PhpFunctionComp(2769) or
      PhpFunctionComp(2860) or
      PhpFunctionComp(3483) or
      PhpFunctionComp(3513) or
      PhpFunctionComp(3886) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc291: TSynWebTokenKind;
begin
  if  PhpFunctionComp(142) or
      PhpFunctionComp(974) or
      PhpFunctionComp(993) or
      PhpFunctionComp(1152) or
      PhpFunctionComp(2072) or
      PhpFunctionComp(2359) or
      PhpFunctionComp(2365) or
      PhpFunctionComp(2633) or
      PhpFunctionComp(2874) or
      PhpFunctionComp(3061) or
      PhpFunctionComp(3563) or
      PhpFunctionComp(3630) or
      PhpFunctionComp(3788) or
      PhpFunctionComp(3793) or
      PhpFunctionComp(4054) or
      PhpFunctionComp(4092) or
      PhpFunctionComp(4198) or
      PhpFunctionComp(4333) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc292: TSynWebTokenKind;
begin
  if  PhpFunctionComp(275) or
      PhpFunctionComp(294) or
      PhpFunctionComp(1074) or
      PhpFunctionComp(1789) or
      PhpFunctionComp(1882) or
      PhpFunctionComp(1891) or
      PhpFunctionComp(2357) or
      PhpFunctionComp(3227) or
      PhpFunctionComp(3735) or
      PhpFunctionComp(3901) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc293: TSynWebTokenKind;
begin
  if  PhpFunctionComp(409) or
      PhpFunctionComp(1902) or
      PhpFunctionComp(1940) or
      PhpFunctionComp(2081) or
      PhpFunctionComp(2304) or
      PhpFunctionComp(2398) or
      PhpFunctionComp(3311) or
      PhpFunctionComp(3330) or
      PhpFunctionComp(3419) or
      PhpFunctionComp(3489) or
      PhpFunctionComp(4217) or
      PhpFunctionComp(4250) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc294: TSynWebTokenKind;
begin
  if  PhpFunctionComp(122) or
      PhpFunctionComp(494) or
      PhpFunctionComp(988) or
      PhpFunctionComp(1900) or
      PhpFunctionComp(1993) or
      PhpFunctionComp(2101) or
      PhpFunctionComp(2133) or
      PhpFunctionComp(2238) or
      PhpFunctionComp(2301) or
      PhpFunctionComp(2537) or
      PhpFunctionComp(2616) or
      PhpFunctionComp(3215) or
      PhpFunctionComp(3244) or
      PhpFunctionComp(3307) or
      PhpFunctionComp(3707) or
      PhpFunctionComp(4216) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc295: TSynWebTokenKind;
begin
  if  PhpFunctionComp(422) or
      PhpFunctionComp(809) or
      PhpFunctionComp(1171) or
      PhpFunctionComp(1279) or
      PhpFunctionComp(1306) or
      PhpFunctionComp(1327) or
      PhpFunctionComp(1764) or
      PhpFunctionComp(2027) or
      PhpFunctionComp(2389) or
      PhpFunctionComp(3064) or
      PhpFunctionComp(3325) or
      PhpFunctionComp(3509) or
      PhpFunctionComp(4330) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc296: TSynWebTokenKind;
begin
  if  PhpFunctionComp(204) or
      PhpFunctionComp(709) or
      PhpFunctionComp(1976) or
      PhpFunctionComp(2251) or
      PhpFunctionComp(2332) or
      PhpFunctionComp(2636) or
      PhpFunctionComp(3359) or
      PhpFunctionComp(3431) or
      PhpFunctionComp(3819) or
      PhpFunctionComp(3842) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc297: TSynWebTokenKind;
begin
  if  PhpFunctionComp(347) or
      PhpFunctionComp(1975) or
      PhpFunctionComp(2373) or
      PhpFunctionComp(3914) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc298: TSynWebTokenKind;
begin
  if  PhpFunctionComp(772) or
      PhpFunctionComp(2338) or
      PhpFunctionComp(2578) or
      PhpFunctionComp(2710) or
      PhpFunctionComp(3158) or
      PhpFunctionComp(3564) or
      PhpFunctionComp(3897) or
      PhpFunctionComp(3899) or
      PhpFunctionComp(3906) or
      PhpFunctionComp(4391) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc299: TSynWebTokenKind;
begin
  if  PhpFunctionComp(220) or
      PhpFunctionComp(276) or
      PhpFunctionComp(994) or
      PhpFunctionComp(1733) or
      PhpFunctionComp(1885) or
      PhpFunctionComp(2597) or
      PhpFunctionComp(4074) or
      PhpFunctionComp(4277) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc300: TSynWebTokenKind;
begin
  if  PhpFunctionComp(3099) or
      PhpFunctionComp(3457) or
      PhpFunctionComp(3922) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc301: TSynWebTokenKind;
begin
  if  PhpFunctionComp(426) or
      PhpFunctionComp(1966) or
      PhpFunctionComp(2512) or
      PhpFunctionComp(2530) or
      PhpFunctionComp(2661) or
      PhpFunctionComp(2857) or
      PhpFunctionComp(3571) or
      PhpFunctionComp(3803) or
      PhpFunctionComp(4269) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc302: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1075) or
      PhpFunctionComp(2629) or
      PhpFunctionComp(3421) or
      PhpFunctionComp(4114) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc303: TSynWebTokenKind;
begin
  if  PhpFunctionComp(360) or
      PhpFunctionComp(602) or
      PhpFunctionComp(670) or
      PhpFunctionComp(1655) or
      PhpFunctionComp(1925) or
      PhpFunctionComp(1953) or
      PhpFunctionComp(2307) or
      PhpFunctionComp(2531) or
      PhpFunctionComp(2564) or
      PhpFunctionComp(2675) or
      PhpFunctionComp(3062) or
      PhpFunctionComp(3472) or
      PhpFunctionComp(3490) or
      PhpFunctionComp(3627) or
      PhpFunctionComp(3888) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc304: TSynWebTokenKind;
begin
  if  PhpFunctionComp(173) or
      PhpFunctionComp(304) or
      PhpFunctionComp(362) or
      PhpFunctionComp(2029) or
      PhpFunctionComp(2711) or
      PhpFunctionComp(3409) or
      PhpFunctionComp(4208) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc305: TSynWebTokenKind;
begin
  if  PhpFunctionComp(149) or
      PhpFunctionComp(1742) or
      PhpFunctionComp(1936) or
      PhpFunctionComp(2208) or
      PhpFunctionComp(2324) or
      PhpFunctionComp(2966) or
      PhpFunctionComp(3247) or
      PhpFunctionComp(3320) or
      PhpFunctionComp(3726) or
      PhpFunctionComp(3779) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc306: TSynWebTokenKind;
begin
  if  PhpFunctionComp(702) or
      PhpFunctionComp(2347) or
      PhpFunctionComp(2429) or
      PhpFunctionComp(2481) or
      PhpFunctionComp(2684) or
      PhpFunctionComp(2893) or
      PhpFunctionComp(3092) or
      PhpFunctionComp(3329) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc307: TSynWebTokenKind;
begin
  if  PhpFunctionComp(141) or
      PhpFunctionComp(1335) or
      PhpFunctionComp(2551) or
      PhpFunctionComp(3332) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc308: TSynWebTokenKind;
begin
  if  PhpFunctionComp(812) or
      PhpFunctionComp(2375) or
      PhpFunctionComp(2403) or
      PhpFunctionComp(2508) or
      PhpFunctionComp(2570) or
      PhpFunctionComp(2963) or
      PhpFunctionComp(3425) or
      PhpFunctionComp(3550) or
      PhpFunctionComp(3761) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc309: TSynWebTokenKind;
begin
  if  PhpFunctionComp(238) or
      PhpFunctionComp(357) or
      PhpFunctionComp(455) or
      PhpFunctionComp(1231) or
      PhpFunctionComp(1872) or
      PhpFunctionComp(1980) or
      PhpFunctionComp(2411) or
      PhpFunctionComp(2511) or
      PhpFunctionComp(3145) or
      PhpFunctionComp(3391) or
      PhpFunctionComp(3459) or
      PhpFunctionComp(4191) or
      PhpFunctionComp(4240) or
      PhpFunctionComp(4380) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc310: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1169) or
      PhpFunctionComp(1269) or
      PhpFunctionComp(2482) or
      PhpFunctionComp(2889) or
      PhpFunctionComp(3530) or
      PhpFunctionComp(3565) or
      PhpFunctionComp(3732) or
      PhpFunctionComp(3923) or
      PhpFunctionComp(4397) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc311: TSynWebTokenKind;
begin
  if  PhpFunctionComp(277) or
      PhpFunctionComp(485) or
      PhpFunctionComp(570) or
      PhpFunctionComp(1970) or
      PhpFunctionComp(2391) or
      PhpFunctionComp(2595) or
      PhpFunctionComp(3080) or
      PhpFunctionComp(3717) or
      PhpFunctionComp(3850) or
      PhpFunctionComp(4220) or
      PhpFunctionComp(4271) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc312: TSynWebTokenKind;
begin
  if  PhpFunctionComp(144) or
      PhpFunctionComp(350) or
      PhpFunctionComp(413) or
      PhpFunctionComp(736) or
      PhpFunctionComp(1881) or
      PhpFunctionComp(2216) or
      PhpFunctionComp(2880) or
      PhpFunctionComp(3233) or
      PhpFunctionComp(3422) or
      PhpFunctionComp(3610) or
      PhpFunctionComp(3616) or
      PhpFunctionComp(4219) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc313: TSynWebTokenKind;
begin
  if  PhpFunctionComp(365) or
      PhpFunctionComp(1193) or
      PhpFunctionComp(2311) or
      PhpFunctionComp(2525) or
      PhpFunctionComp(3780) or
      PhpFunctionComp(3792) or
      PhpFunctionComp(4290) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc314: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2380) or
      PhpFunctionComp(2543) or
      PhpFunctionComp(2960) or
      PhpFunctionComp(3771) or
      PhpFunctionComp(4197) or
      PhpFunctionComp(4295) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc315: TSynWebTokenKind;
begin
  if  PhpFunctionComp(657) or
      PhpFunctionComp(703) or
      PhpFunctionComp(1903) or
      PhpFunctionComp(2392) or
      PhpFunctionComp(2538) or
      PhpFunctionComp(3386) or
      PhpFunctionComp(3634) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc316: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2039) or
      PhpFunctionComp(3512) or
      PhpFunctionComp(3662) or
      PhpFunctionComp(4322) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc317: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2312) or
      PhpFunctionComp(3734) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc318: TSynWebTokenKind;
begin
  if  PhpFunctionComp(668) or
      PhpFunctionComp(1904) or
      PhpFunctionComp(2095) or
      PhpFunctionComp(2438) or
      PhpFunctionComp(2439) or
      PhpFunctionComp(2532) or
      PhpFunctionComp(2641) or
      PhpFunctionComp(2663) or
      PhpFunctionComp(3969) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc319: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1250) or
      PhpFunctionComp(4310) or
      PhpFunctionComp(4316) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc320: TSynWebTokenKind;
begin
  if  PhpFunctionComp(631) or
      PhpFunctionComp(2402) or
      PhpFunctionComp(3526) or
      PhpFunctionComp(3660) or
      PhpFunctionComp(3863) or
      PhpFunctionComp(4314) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc321: TSynWebTokenKind;
begin
  if  PhpFunctionComp(208) or
      PhpFunctionComp(2915) or
      PhpFunctionComp(3432) or
      PhpFunctionComp(3898) or
      PhpFunctionComp(4200) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc322: TSynWebTokenKind;
begin
  if  PhpFunctionComp(755) or
      PhpFunctionComp(2012) or
      PhpFunctionComp(2372) or
      PhpFunctionComp(3116) or
      PhpFunctionComp(3633) or
      PhpFunctionComp(4188) or
      PhpFunctionComp(4254) or
      PhpFunctionComp(4284) or
      PhpFunctionComp(4304) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc323: TSynWebTokenKind;
begin
  if  PhpFunctionComp(124) or
      PhpFunctionComp(2666) or
      PhpFunctionComp(3862) or
      PhpFunctionComp(3921) or
      PhpFunctionComp(4301) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc324: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2393) or
      PhpFunctionComp(2870) or
      PhpFunctionComp(3094) or
      PhpFunctionComp(4120) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc325: TSynWebTokenKind;
begin
  if  PhpFunctionComp(354) or
      PhpFunctionComp(377) or
      PhpFunctionComp(652) or
      PhpFunctionComp(3558) or
      PhpFunctionComp(3810) or
      PhpFunctionComp(3919) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc326: TSynWebTokenKind;
begin
  if  PhpFunctionComp(412) or
      PhpFunctionComp(1331) or
      PhpFunctionComp(1519) or
      PhpFunctionComp(2620) or
      PhpFunctionComp(2683) or
      PhpFunctionComp(3727) or
      PhpFunctionComp(3848) or
      PhpFunctionComp(4098) or
      PhpFunctionComp(4297) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc327: TSynWebTokenKind;
begin
  if  PhpFunctionComp(425) or
      PhpFunctionComp(469) or
      PhpFunctionComp(1921) or
      PhpFunctionComp(2355) or
      PhpFunctionComp(2435) or
      PhpFunctionComp(2731) or
      PhpFunctionComp(2913) or
      PhpFunctionComp(4325) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc328: TSynWebTokenKind;
begin
  if  PhpFunctionComp(117) or
      PhpFunctionComp(143) or
      PhpFunctionComp(167) or
      PhpFunctionComp(1203) or
      PhpFunctionComp(2038) or
      PhpFunctionComp(2077) or
      PhpFunctionComp(2412) or
      PhpFunctionComp(2621) or
      PhpFunctionComp(2625) or
      PhpFunctionComp(2904) or
      PhpFunctionComp(3555) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc329: TSynWebTokenKind;
begin
  if  PhpFunctionComp(121) or
      PhpFunctionComp(2430) or
      PhpFunctionComp(2568) or
      PhpFunctionComp(2895) or
      PhpFunctionComp(3762) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc330: TSynWebTokenKind;
begin
  if  PhpFunctionComp(240) or
      PhpFunctionComp(2076) or
      PhpFunctionComp(2354) or
      PhpFunctionComp(3232) or
      PhpFunctionComp(3820) or
      PhpFunctionComp(4229) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc331: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1326) or
      PhpFunctionComp(1859) or
      PhpFunctionComp(2025) or
      PhpFunctionComp(2352) or
      PhpFunctionComp(3237) or
      PhpFunctionComp(3516) or
      PhpFunctionComp(3716) or
      PhpFunctionComp(3781) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc332: TSynWebTokenKind;
begin
  if  PhpFunctionComp(640) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc333: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2442) or
      PhpFunctionComp(2581) or
      PhpFunctionComp(2623) or
      PhpFunctionComp(2627) or
      PhpFunctionComp(2863) or
      PhpFunctionComp(2973) or
      PhpFunctionComp(3388) or
      PhpFunctionComp(3777) or
      PhpFunctionComp(3855) or
      PhpFunctionComp(4052) or
      PhpFunctionComp(4071) or
      PhpFunctionComp(4323) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc334: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2069) or
      PhpFunctionComp(2353) or
      PhpFunctionComp(2622) or
      PhpFunctionComp(2680) or
      PhpFunctionComp(4195) or
      PhpFunctionComp(4285) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc335: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1150) or
      PhpFunctionComp(1153) or
      PhpFunctionComp(1815) or
      PhpFunctionComp(1863) or
      PhpFunctionComp(2272) or
      PhpFunctionComp(2886) or
      PhpFunctionComp(3416) or
      PhpFunctionComp(3417) or
      PhpFunctionComp(3477) or
      PhpFunctionComp(3556) or
      PhpFunctionComp(4040) or
      PhpFunctionComp(4259) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc336: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2271) or
      PhpFunctionComp(2864) or
      PhpFunctionComp(4345) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc337: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1170) or
      PhpFunctionComp(1995) or
      PhpFunctionComp(2580) or
      PhpFunctionComp(2861) or
      PhpFunctionComp(3097) or
      PhpFunctionComp(3840) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc338: TSynWebTokenKind;
begin
  if  PhpFunctionComp(353) or
      PhpFunctionComp(359) or
      PhpFunctionComp(621) or
      PhpFunctionComp(1333) or
      PhpFunctionComp(2078) or
      PhpFunctionComp(2658) or
      PhpFunctionComp(2888) or
      PhpFunctionComp(2905) or
      PhpFunctionComp(3321) or
      PhpFunctionComp(4227) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc339: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2279) or
      PhpFunctionComp(3225) or
      PhpFunctionComp(3552) or
      PhpFunctionComp(3669) or
      PhpFunctionComp(3763) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc340: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1204) or
      PhpFunctionComp(1719) or
      PhpFunctionComp(2075) or
      PhpFunctionComp(4260) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc341: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1797) or
      PhpFunctionComp(2408) or
      PhpFunctionComp(4242) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc342: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2225) or
      PhpFunctionComp(2598) or
      PhpFunctionComp(2894) or
      PhpFunctionComp(3827) or
      PhpFunctionComp(3905) or
      PhpFunctionComp(4223) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc343: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1334) or
      PhpFunctionComp(2317) or
      PhpFunctionComp(2330) or
      PhpFunctionComp(3428) or
      PhpFunctionComp(3433) or
      PhpFunctionComp(3668) or
      PhpFunctionComp(3857) or
      PhpFunctionComp(3864) or
      PhpFunctionComp(4151) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc344: TSynWebTokenKind;
begin
  if  PhpFunctionComp(209) or
      PhpFunctionComp(2028) or
      PhpFunctionComp(2209) or
      PhpFunctionComp(2342) or
      PhpFunctionComp(2390) or
      PhpFunctionComp(2610) or
      PhpFunctionComp(2654) or
      PhpFunctionComp(2862) or
      PhpFunctionComp(3764) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc345: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2340) or
      PhpFunctionComp(3327) or
      PhpFunctionComp(3484) or
      PhpFunctionComp(3618) or
      PhpFunctionComp(4347) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc346: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1522) or
      PhpFunctionComp(3718) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc347: TSynWebTokenKind;
begin
  if  PhpFunctionComp(361) or
      PhpFunctionComp(1276) or
      PhpFunctionComp(1718) or
      PhpFunctionComp(3531) or
      PhpFunctionComp(3557) or
      PhpFunctionComp(3671) or
      PhpFunctionComp(3798) or
      PhpFunctionComp(3924) or
      PhpFunctionComp(3928) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc348: TSynWebTokenKind;
begin
  if  PhpFunctionComp(205) or
      PhpFunctionComp(1330) or
      PhpFunctionComp(1520) or
      PhpFunctionComp(2299) or
      PhpFunctionComp(2607) or
      PhpFunctionComp(2891) or
      PhpFunctionComp(3570) or
      PhpFunctionComp(3772) or
      PhpFunctionComp(3909) or
      PhpFunctionComp(3927) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc349: TSynWebTokenKind;
begin
  if  PhpFunctionComp(168) or
      PhpFunctionComp(438) or
      PhpFunctionComp(1948) or
      PhpFunctionComp(2582) or
      PhpFunctionComp(2613) or
      PhpFunctionComp(3572) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc350: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1947) or
      PhpFunctionComp(2321) or
      PhpFunctionComp(3615) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc351: TSynWebTokenKind;
begin
  if  PhpFunctionComp(658) or
      PhpFunctionComp(1937) or
      PhpFunctionComp(2079) or
      PhpFunctionComp(2664) or
      PhpFunctionComp(3553) or
      PhpFunctionComp(3675) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc352: TSynWebTokenKind;
begin
  if  PhpFunctionComp(847) or
      PhpFunctionComp(1901) or
      PhpFunctionComp(1935) or
      PhpFunctionComp(2914) or
      PhpFunctionComp(3260) or
      PhpFunctionComp(3479) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc353: TSynWebTokenKind;
begin
  if  PhpFunctionComp(89) or
      PhpFunctionComp(1927) or
      PhpFunctionComp(4278) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc354: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2368) or
      PhpFunctionComp(2406) or
      PhpFunctionComp(3715) or
      PhpFunctionComp(3835) or
      PhpFunctionComp(3856) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc355: TSynWebTokenKind;
begin
  if  PhpFunctionComp(3674) or
      PhpFunctionComp(3838) or
      PhpFunctionComp(4144) or
      PhpFunctionComp(4282) or
      PhpFunctionComp(4294) or
      PhpFunctionComp(4346) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc356: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1945) or
      PhpFunctionComp(2387) or
      PhpFunctionComp(2605) or
      PhpFunctionComp(2617) or
      PhpFunctionComp(2902) or
      PhpFunctionComp(4211) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc357: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2273) or
      PhpFunctionComp(3893) or
      PhpFunctionComp(4051) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc358: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2318) or
      PhpFunctionComp(2892) or
      PhpFunctionComp(3844) or
      PhpFunctionComp(4256) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc359: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1615) or
      PhpFunctionComp(3427) or
      PhpFunctionComp(4097) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc360: TSynWebTokenKind;
begin
  if  PhpFunctionComp(660) or
      PhpFunctionComp(1336) or
      PhpFunctionComp(2274) or
      PhpFunctionComp(2306) or
      PhpFunctionComp(2676) or
      PhpFunctionComp(4209) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc361: TSynWebTokenKind;
begin
  if  PhpFunctionComp(439) or
      PhpFunctionComp(3486) or
      PhpFunctionComp(3769) or
      PhpFunctionComp(3808) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc362: TSynWebTokenKind;
begin
  if  PhpFunctionComp(415) or
      PhpFunctionComp(3128) or
      PhpFunctionComp(3129) or
      PhpFunctionComp(3617) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc363: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1654) or
      PhpFunctionComp(2316) or
      PhpFunctionComp(2883) or
      PhpFunctionComp(2900) or
      PhpFunctionComp(3019) or
      PhpFunctionComp(4224) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc364: TSynWebTokenKind;
begin
  if  PhpFunctionComp(352) or
      PhpFunctionComp(2536) or
      PhpFunctionComp(3507) or
      PhpFunctionComp(3623) or
      PhpFunctionComp(3915) or
      PhpFunctionComp(4245) or
      PhpFunctionComp(4348) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc365: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2619) or
      PhpFunctionComp(3613) or
      PhpFunctionComp(4320) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc366: TSynWebTokenKind;
begin
  if  PhpFunctionComp(320) or
      PhpFunctionComp(2224) or
      PhpFunctionComp(2344) or
      PhpFunctionComp(2903) or
      PhpFunctionComp(3510) or
      PhpFunctionComp(4342) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc367: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2590) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc368: TSynWebTokenKind;
begin
  if  PhpFunctionComp(90) or
      PhpFunctionComp(637) or
      PhpFunctionComp(2614) or
      PhpFunctionComp(4308) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc369: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2291) or
      PhpFunctionComp(2345) or
      PhpFunctionComp(2385) or
      PhpFunctionComp(2609) or
      PhpFunctionComp(3608) or
      PhpFunctionComp(3843) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc370: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2519) or
      PhpFunctionComp(2520) or
      PhpFunctionComp(2898) or
      PhpFunctionComp(3860) or
      PhpFunctionComp(4257) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc371: TSynWebTokenKind;
begin
  if  PhpFunctionComp(4102) or
      PhpFunctionComp(4328) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc372: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1341) or
      PhpFunctionComp(2944) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc373: TSynWebTokenKind;
begin
  if  PhpFunctionComp(995) or
      PhpFunctionComp(2068) or
      PhpFunctionComp(2630) or
      PhpFunctionComp(2634) or
      PhpFunctionComp(3506) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc374: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2615) or
      PhpFunctionComp(3829) or
      PhpFunctionComp(3884) or
      PhpFunctionComp(4315) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc376: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1329) or
      PhpFunctionComp(1905) or
      PhpFunctionComp(2399) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc377: TSynWebTokenKind;
begin
  if  PhpFunctionComp(655) or
      PhpFunctionComp(3485) or
      PhpFunctionComp(4166) or
      PhpFunctionComp(4276) or
      PhpFunctionComp(4302) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc378: TSynWebTokenKind;
begin
  if  PhpFunctionComp(791) or
      PhpFunctionComp(2364) or
      PhpFunctionComp(4096) or
      PhpFunctionComp(4252) or
      PhpFunctionComp(4288) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc379: TSynWebTokenKind;
begin
  if  PhpFunctionComp(663) or
      PhpFunctionComp(990) or
      PhpFunctionComp(1277) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc380: TSynWebTokenKind;
begin
  if  PhpFunctionComp(364) or
      PhpFunctionComp(1952) or
      PhpFunctionComp(3859) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc381: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2386) or
      PhpFunctionComp(3622) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc382: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2030) or
      PhpFunctionComp(2089) or
      PhpFunctionComp(2637) or
      PhpFunctionComp(2656) or
      PhpFunctionComp(4222) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc383: TSynWebTokenKind;
begin
  if  PhpFunctionComp(3929) or
      PhpFunctionComp(4389) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc384: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2681) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc385: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1521) or
      PhpFunctionComp(2584) or
      PhpFunctionComp(2651) or
      PhpFunctionComp(3635) or
      PhpFunctionComp(4210) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc386: TSynWebTokenKind;
begin
  if  PhpFunctionComp(518) or
      PhpFunctionComp(661) or
      PhpFunctionComp(2064) or
      PhpFunctionComp(3861) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc387: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1951) or
      PhpFunctionComp(2062) or
      PhpFunctionComp(2395) or
      PhpFunctionComp(2635) or
      PhpFunctionComp(3234) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc388: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2554) or
      PhpFunctionComp(3614) or
      PhpFunctionComp(3836) or
      PhpFunctionComp(4189) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc389: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1594) or
      PhpFunctionComp(3889) or
      PhpFunctionComp(4303) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc390: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1342) or
      PhpFunctionComp(1856) or
      PhpFunctionComp(3825) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc391: TSynWebTokenKind;
begin
  if  PhpFunctionComp(119) or
      PhpFunctionComp(2899) or
      PhpFunctionComp(2911) or
      PhpFunctionComp(3575) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc392: TSynWebTokenKind;
begin
  if  PhpFunctionComp(206) or
      PhpFunctionComp(1939) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc393: TSynWebTokenKind;
begin
  if  PhpFunctionComp(664) or
      PhpFunctionComp(665) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc394: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1860) or
      PhpFunctionComp(2650) or
      PhpFunctionComp(4165) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc396: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2682) or
      PhpFunctionComp(3910) or
      PhpFunctionComp(3918) or
      PhpFunctionComp(4032) or
      PhpFunctionComp(4204) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc397: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2063) or
      PhpFunctionComp(4289) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc398: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1751) or
      PhpFunctionComp(1792) or
      PhpFunctionComp(3740) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc399: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1665) or
      PhpFunctionComp(1741) or
      PhpFunctionComp(1954) or
      PhpFunctionComp(2061) or
      PhpFunctionComp(3749) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc400: TSynWebTokenKind;
begin
  if  PhpFunctionComp(669) or
      PhpFunctionComp(2035) or
      PhpFunctionComp(2401) or
      PhpFunctionComp(2655) or
      PhpFunctionComp(3892) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc401: TSynWebTokenKind;
begin
  if  PhpFunctionComp(671) or
      PhpFunctionComp(2400) or
      PhpFunctionComp(3525) or
      PhpFunctionComp(4293) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc402: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2388) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc403: TSynWebTokenKind;
begin
  if  PhpFunctionComp(967) or
      PhpFunctionComp(2032) or
      PhpFunctionComp(2341) or
      PhpFunctionComp(2384) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc404: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2370) or
      PhpFunctionComp(4041) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc405: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2195) or
      PhpFunctionComp(2647) or
      PhpFunctionComp(3487) or
      PhpFunctionComp(4287) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc406: TSynWebTokenKind;
begin
  if  PhpFunctionComp(851) or
      PhpFunctionComp(1884) or
      PhpFunctionComp(2552) or
      PhpFunctionComp(3488) or
      PhpFunctionComp(4258) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc407: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1989) or
      PhpFunctionComp(2396) or
      PhpFunctionComp(2646) or
      PhpFunctionComp(4312) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc408: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2599) or
      PhpFunctionComp(2648) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc409: TSynWebTokenKind;
begin
  if  PhpFunctionComp(969) or
      PhpFunctionComp(2585) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc410: TSynWebTokenKind;
begin
  if  PhpFunctionComp(92) or
      PhpFunctionComp(1676) or
      PhpFunctionComp(3567) or
      PhpFunctionComp(4043) or
      PhpFunctionComp(4299) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc412: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1593) or
      PhpFunctionComp(2643) or
      PhpFunctionComp(3846) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc413: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2212) or
      PhpFunctionComp(2319) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc414: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1328) or
      PhpFunctionComp(1799) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc415: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1595) or
      PhpFunctionComp(2397) or
      PhpFunctionComp(2645) or
      PhpFunctionComp(4042) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc417: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2652) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc418: TSynWebTokenKind;
begin
  if  PhpFunctionComp(3851) or
      PhpFunctionComp(4296) or
      PhpFunctionComp(4390) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc419: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1880) or
      PhpFunctionComp(2214) or
      PhpFunctionComp(3573) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc420: TSynWebTokenKind;
begin
  if  PhpFunctionComp(3809) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc421: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1861) or
      PhpFunctionComp(2087) or
      PhpFunctionComp(3891) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc423: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1798) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc424: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2213) or
      PhpFunctionComp(2882) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc425: TSynWebTokenKind;
begin
  if  PhpFunctionComp(93) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc426: TSynWebTokenKind;
begin
  if  PhpFunctionComp(3847) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc427: TSynWebTokenKind;
begin
  if  PhpFunctionComp(638) or
      PhpFunctionComp(2346) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc428: TSynWebTokenKind;
begin
  if  PhpFunctionComp(3890) or
      PhpFunctionComp(4044) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc430: TSynWebTokenKind;
begin
  if  PhpFunctionComp(4318) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc431: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2405) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc433: TSynWebTokenKind;
begin
  if  PhpFunctionComp(125) or
      PhpFunctionComp(968) or
      PhpFunctionComp(2036) or
      PhpFunctionComp(2910) or
      PhpFunctionComp(4306) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc434: TSynWebTokenKind;
begin
  if  PhpFunctionComp(3828) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc435: TSynWebTokenKind;
begin
  if  PhpFunctionComp(4263) or
      PhpFunctionComp(4321) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc436: TSynWebTokenKind;
begin
  if  PhpFunctionComp(4247) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc437: TSynWebTokenKind;
begin
  if  PhpFunctionComp(639) or
      PhpFunctionComp(2215) or
      PhpFunctionComp(2594) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc438: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2037) or
      PhpFunctionComp(2404) or
      PhpFunctionComp(4309) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc439: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2066) or
      PhpFunctionComp(2420) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc440: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2638) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc441: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2649) or
      PhpFunctionComp(4226) or
      PhpFunctionComp(4249) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc442: TSynWebTokenKind;
begin
  if  PhpFunctionComp(3672) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc447: TSynWebTokenKind;
begin
  if  PhpFunctionComp(329) or
      PhpFunctionComp(848) or
      PhpFunctionComp(2378) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc449: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2945) or
      PhpFunctionComp(4319) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc450: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2407) or
      PhpFunctionComp(4280) or
      PhpFunctionComp(4298) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc452: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2060) or
      PhpFunctionComp(4307) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc453: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2881) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc454: TSynWebTokenKind;
begin
  if  PhpFunctionComp(118) or
      PhpFunctionComp(2377) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc455: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2592) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc457: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2320) or
      PhpFunctionComp(3841) or
      PhpFunctionComp(4317) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc458: TSynWebTokenKind;
begin
  if  PhpFunctionComp(3568) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc459: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1857) or
      PhpFunctionComp(2679) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc460: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2041) or
      PhpFunctionComp(2586) or
      PhpFunctionComp(4305) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc463: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2589) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc464: TSynWebTokenKind;
begin
  if  PhpFunctionComp(4261) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc468: TSynWebTokenKind;
begin
  if  PhpFunctionComp(3673) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc470: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2315) or
      PhpFunctionComp(2896) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc472: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2591) or
      PhpFunctionComp(4274) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc474: TSynWebTokenKind;
begin
  if  PhpFunctionComp(662) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc477: TSynWebTokenKind;
begin
  if  PhpFunctionComp(4313) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc478: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1864) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc479: TSynWebTokenKind;
begin
  if  PhpFunctionComp(4248) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc480: TSynWebTokenKind;
begin
  if  PhpFunctionComp(4300) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc483: TSynWebTokenKind;
begin
  if  PhpFunctionComp(4225) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc487: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2085) or
      PhpFunctionComp(3845) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc503: TSynWebTokenKind;
begin
  if  PhpFunctionComp(4221) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc505: TSynWebTokenKind;
begin
  if  PhpFunctionComp(1854) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc513: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2067) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc515: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2626) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc517: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2083) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc519: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2082) or
      PhpFunctionComp(4266) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc521: TSynWebTokenKind;
begin
  if  PhpFunctionComp(210) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc529: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2909) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc532: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2912) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc533: TSynWebTokenKind;
begin
  if  PhpFunctionComp(4275) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc539: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2033) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc541: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2587) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc549: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2034) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc553: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2593) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc555: TSynWebTokenKind;
begin
  if  PhpFunctionComp(4267) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc559: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2653) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc561: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2086) or
      PhpFunctionComp(4262) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc568: TSynWebTokenKind;
begin
  if  PhpFunctionComp(4228) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc575: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2588) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc587: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2065) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc590: TSynWebTokenKind;
begin
  if  PhpFunctionComp(4265) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc635: TSynWebTokenKind;
begin
  if  PhpFunctionComp(2084) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

function TSynWebEngine.PhpKeywordFunc644: TSynWebTokenKind;
begin
  if  PhpFunctionComp(4279) then
    Result := stkPhpFunction
  else
    Result := stkPhpIdentifier;
end;

// Other -----------------------------------------------------------------------

procedure TSynWebEngine.AddAttribute(AAttrib: TSynHighlighterAttributes);
begin
  FAttributes.AddObject(AAttrib.Name, AAttrib);
end;

procedure TSynWebEngine.AddToNotifyList(ASynWeb: TSynWebBase);
begin
  FNotifyList.Add(ASynWeb);
end;

procedure TSynWebEngine.RemoveFromNotifyList(ASynWeb: TSynWebBase);
begin
  FNotifyList.Remove(ASynWeb);
end;

procedure TSynWebEngine.SetAttributesOnChange(AEvent: TNotifyEvent);
var
  i: Integer;
  Attri: TSynHighlighterAttributes;
begin
  for i := FAttributes.Count - 1 downto 0 do
  begin
    Attri := TSynHighlighterAttributes(FAttributes.Objects[i]);
    if Attri <> nil then
    begin
      Attri.OnChange := AEvent;
      Attri.InternalSaveDefaultValues;
    end;
  end;
end;

procedure TSynWebEngine.DefHighlightChange(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to FNotifyList.Count - 1 do
    TSynWebBase(FNotifyList[i]).DoDefHighlightChange;
end;

function TSynWebEngine.GetCrc8String(AString: String): byte;
var
  i: Integer;
begin
  Result := Length(AString);
  for i := 1 to Length(AString) do
    Result := TCrc8Table[Result xor Byte(AString[i])];
end;

function TSynWebEngine.GetRangeBit(ABit: Longword): Boolean;
begin
  Result := FInstance^.FRange and (1 shl ABit) <> 0;
end;

procedure TSynWebEngine.SetRangeBit(ABit: Longword; AVal: Boolean);
begin
  if AVal then
    FInstance^.FRange := FInstance^.FRange or (1 shl ABit)
  else
    FInstance^.FRange := FInstance^.FRange and not (1 shl ABit);
end;

function TSynWebEngine.GetRangeInt(ALen, APos: Longword): Longword;
begin
  Result := (FInstance^.FRange shr APos) and not ($FFFFFFFF shl ALen);
end;

procedure TSynWebEngine.SetRangeInt(ALen, APos, AVal: Longword);
var
  i: Longword;
begin
  i := $FFFFFFFF shl ALen;
  //todo: Does it work in CLX? Should be [EBX].APos? I don't know :(
  asm
    mov ecx, APos
    rol i, cl
  end;
  FInstance^.FRange := (FInstance^.FRange and i) or ((AVal shl APos) and not i);
end;

procedure TSynWebEngine.NullProc;
begin
  FInstance^.FTokenID := stkNull;
end;

procedure TSynWebEngine.NextSetHighlighterType;
var
  OldRun: Integer;
begin
  SetHighlighterType(FInstance^.FNextHighlighterType, FInstance^.FNextClearBits,
    False, FInstance^.FNextUseNextAH);
  OldRun := FInstance^.FRun;
  Next;
  if OldRun = FInstance^.FRun then
    Next;
  FInstance^.FHighlighterSW := True;

end;

procedure TSynWebEngine.SetHighlighterType(const AHighlighterType: TSynWebHighlighterType;
  AClearBits: Boolean; ASetAtNextToken: Boolean; AUseNextAH: Boolean);
begin
  if ASetAtNextToken then
  begin
    FInstance^.FNextUseNextAH := AUseNextAH;
    FInstance^.FNextHighlighterType := AHighlighterType;
    FInstance^.FNextClearBits := AClearBits;
    FInstance^.FNextProcTable := NextSetHighlighterType;
  end else
  begin
    FInstance^.FUseNextAH := AUseNextAH;
    FInstance^.FHighlighterSW := True;
    FInstance^.FPrevHighlighterType := FInstance^.FHighlighterType;
    FInstance^.FHighlighterType := AHighlighterType;
    SetRangeInt(3, 29, Longword(AHighlighterType));
    SetupHighlighterType(AClearBits);
  end;
end;

procedure TSynWebEngine.SetupHighlighterType(AClearBits: Boolean);
begin
  case FInstance^.FHighlighterType of
  shtML:
    if FInstance^.FHighlighterMode = shmPhpCli then
    begin
      if AClearBits then
        SetRangeInt(17, 0, 0);
      FInstance^.FSYN_ATTR_COMMENT := FPhpInlineTextAttri;
      FInstance^.FSYN_ATTR_STRING := FPhpInlineTextAttri;
      FInstance^.FSYN_ATTR_WHITESPACE := FPhpInlineTextAttri;
      FInstance^.FNextProcTable := PhpCliNext;
    end else
    begin
      if AClearBits then
        SetRangeInt(17, 0, 0);
      FInstance^.FSYN_ATTR_COMMENT := FMLCommentAttri;
      FInstance^.FSYN_ATTR_STRING := FMLTagKeyValueQuotedAttri;
      FInstance^.FSYN_ATTR_WHITESPACE := FMLWhitespaceAttri;
      FInstance^.FNextProcTable := MLNext;
    end;
  shtCss:
    begin
      if AClearBits then
        SetRangeInt(17, 0, 0);
      FInstance^.FSYN_ATTR_COMMENT := FCssCommentAttri;
      FInstance^.FSYN_ATTR_STRING := FCssValStringAttri;
      CssUpdateBg;
      FInstance^.FNextProcTable := CssNext;
    end;
  shtEs:
    begin
      if AClearBits then
        SetRangeInt(17, 0, 0);
      FInstance^.FSYN_ATTR_COMMENT := FEsCommentAttri;
      FInstance^.FSYN_ATTR_STRING := FEsStringAttri;
      FInstance^.FSYN_ATTR_WHITESPACE := FEsWhitespaceAttri;
      FInstance^.FNextProcTable := EsNext;
    end;
  else // case
    if AClearBits then
      SetRangeInt(12, 17, 0);
    FInstance^.FSYN_ATTR_COMMENT := FPhpCommentAttri;
    FInstance^.FSYN_ATTR_STRING := FPhpStringAttri;
    FInstance^.FSYN_ATTR_WHITESPACE := FPhpWhitespaceAttri;
    FInstance^.FNextProcTable := PhpNext;
  end;
end;

procedure TSynWebEngine.SetLine(NewValue: String; LineNumber: Integer);
{$IFDEF SYNWEB_FIXNULL}
var
  s, e: PChar;
  i:Integer;
{$ENDIF}
begin
  FInstance^.FLineRef := NewValue;
{$IFDEF SYNWEB_FIXNULL}
  i := Length(FInstance^.FLineRef);
  if i > 0  then
  begin
    s := @FInstance^.FLineRef[1];
    e := s + i;
    repeat
      if s^ = #0 then
        s^ := #32;
      Inc(s);
    until s = e;
  end;
{$ENDIF}
  FInstance^.FLine := PChar(FInstance^.FLineRef);
  FInstance^.FRun := 0;
  FInstance^.FLineNumber := LineNumber;
  FInstance^.FHighlighterType := TSynWebHighlighterType(GetRangeInt(3, 29));
  FInstance^.FPrevHighlighterType := FInstance^.FHighlighterType;
  FInstance^.FHighlighterSW := False;
  SetupHighlighterType;
{$IFNDEF UNISYNEDIT}
  FInstance^.FNextProcTable;
{$ENDIF}
end;

procedure TSynWebEngine.Next;
begin
  FInstance^.FHighlighterSW := False;
  FInstance^.FNextProcTable;
end;

function TSynWebEngine.GetToken: String;
var
  Len: longint;
begin
  Len := FInstance^.FRun - FInstance^.FTokenPos;
  SetString(Result, (FInstance^.FLine + FInstance^.FTokenPos), Len);
end;

initialization

{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynWebHtmlSyn);
  RegisterPlaceableHighlighter(TSynWebWmlSyn);
  RegisterPlaceableHighlighter(TSynWebXmlSyn);
  RegisterPlaceableHighlighter(TSynWebPhpCliSyn);
  RegisterPlaceableHighlighter(TSynWebCssSyn);
  RegisterPlaceableHighlighter(TSynWebEsSyn);
{$ENDIF}

end.

