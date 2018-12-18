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

(*
Known limitations:
- SynWeb highlighters support only single line SetLine (don't use more than one line).
- Doesn't support #13#10, #10 or #13 as new line. Always use #0 as line break.
- Php: Doesn't support multi-line encapsuled strings in String, only single line:
  eg. "somestring {$a["some array{$b['key'].... <- only single line encapsuled values
- TSynWebSmartySyn limitations:
  - Options.SmartyLDelim MUST begin with '{' and Options.SmartyRDelim MUST begin with '}'.
  - Smarty highlighter doesn't support {literal} ... {/literal}
*)

{
@abstract(Provides an web-files (Multi Html/XHtml/Wml/Xml/Xslt/Css/ECMAScript/Php/Smarty) highlighter for SynEdit
@author(Krystian Bigaj <krystian.bigaj@gmail.com>)
@created(2005-05-21)
The SynHighlighterWeb unit provides SynEdit with a Multi Html/XHtml/Wml/Xml/Xslt/Css/ECMAScript/Php highlighter.
}

unit SynHighlighterWeb;

{$I SynWeb.inc}

interface

uses
  Graphics,
  SynUnicode,
  SynEditTypes,
  SynEditHighlighter,
  SynHighlighterWebData,
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
    FHtml5XmlMode: Boolean;
    FWmlVersion: TSynWebWmlVersion;
    FXsltVersion: TSynWebXSLTVersion;
    FCssVersion: TSynWebCssVersion;
    FPhpVersion: TSynWebPhpVersion;
    FPhpShortOpenTag: Boolean;
    FPhpAspTags: Boolean;
    FAllowASPTags: Boolean;
    FXmlMode: Boolean;

    FPhpEmbeded: Boolean;
    FCssEmbeded: Boolean;
    FEsEmbeded: Boolean;

    FSmartyLDelim: AnsiString;
    FSmartyRDelim: AnsiString;
  end;

  PSynWebInstance = ^TSynWebInstance;

  TSynWebInstance = record
    FRun: Longint;
    FRange: Longword;
    FLine: PAnsiChar;
    FLineRef: AnsiString;
    FLineNumber: Integer;
    FTokenLastID: Integer;
    FTokenPos: Integer;
    FTokenID: TSynWebTokenKind;
    FStringLen, FStringLenClean: Integer;
    FTokenLastSymbolId: Integer;
    FToIdent: PAnsiChar;
    FHashTable: PSynWebHashTable;
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
    FHighlither: TSynWebBase;
    FCssVendorPropertyId: Integer;
    FCssInstancePropertyId: Integer;
  end;

{ TSynWebOptionsBase }

  TSynWebOptionsBase = class(TPersistent)
  private
    FOptions: PSynWebOptions;
    FEngineOptions: PSynWebOptions;
    FUseEngineOptions: Boolean;
    FOnChange: TNotifyEvent;

    function GetHtml5XmlMode: Boolean;
    procedure SetHtml5XmlMode(const Value: Boolean);
    function GetHtmlVersion: TSynWebHtmlVersion;
    procedure SetHtmlVersion(const Value: TSynWebHtmlVersion);
    function GetWmlVersion: TSynWebWmlVersion;
    procedure SetWmlVersion(const Value: TSynWebWmlVersion);
    function GetXsltVersion: TSynWebXSLTVersion;
    procedure SetXsltVersion(const Value: TSynWebXSLTVersion);
    function GetCssVersion: TSynWebCssVersion;
    procedure SetCssVersion(const Value: TSynWebCssVersion);
    function GetPhpVersion: TSynWebPhpVersion;
    procedure SetPhpVersion(const Value: TSynWebPhpVersion);
    function GetPhpAspTags: Boolean;
    procedure SetPhpAspTags(const Value: Boolean);
    function GetPhpShortOpenTag: Boolean;
    procedure SetPhpShortOpenTag(const Value: Boolean);
    function GetAllowASPTags: Boolean;
    procedure SetAllowASPTags(const Value: Boolean);

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
    function UsingEngineOptions: Boolean;

    property Html5XmlMode: Boolean read GetHtml5XmlMode write SetHtml5XmlMode default True;
    property HtmlVersion: TSynWebHtmlVersion read GetHtmlVersion write SetHtmlVersion default shvXHtml10Transitional;
    property WmlVersion: TSynWebWmlVersion read GetWmlVersion write SetWmlVersion default swvWml13;
    property XsltVersion: TSynWebXSLTVersion read GetXsltVersion write SetXsltVersion default swvXslt20;
    property CssVersion: TSynWebCssVersion read GetCssVersion write SetCssVersion default scvCss21;
    property PhpVersion: TSynWebPhpVersion read GetPhpVersion write SetPhpVersion default spvPhp5;
    property PhpShortOpenTag: Boolean read GetPhpShortOpenTag write SetPhpShortOpenTag default True;
    property PhpAspTags: Boolean read GetPhpAspTags write SetPhpAspTags default False;
    property AllowASPTags: Boolean read GetAllowASPTags write SetAllowASPTags default True;

    property CssEmbeded: Boolean read GetCssEmbeded write SetCssEmbeded default False;
    property PhpEmbeded: Boolean read GetPhpEmbeded write SetPhpEmbeded default False;
    property EsEmbeded: Boolean read GetEsEmbeded write SetEsEmbeded default False;

    property UseEngineOptions: Boolean read FUseEngineOptions write SetUseEngineOptions default False;
  public
    constructor Create(AOptions: PSynWebOptions);

    function IsXmlMode: Boolean;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{ TSynWebHtmlOptions }

  TSynWebHtmlOptions = class(TSynWebOptionsBase)
  protected
    procedure UpdateMLOption; override;
  public
    constructor Create(AOptions: PSynWebOptions);
  published
    property Html5XmlMode;
    property HtmlVersion;
    property CssVersion;
    property PhpVersion;
    property PhpShortOpenTag;
    property PhpAspTags;
    property AllowASPTags;
    property CssEmbeded default True;
    property PhpEmbeded default True;
    property EsEmbeded default True;
    property UseEngineOptions;
  end;

{ TSynWebSmartyOptions }

  TSynWebSmartyOptions = class(TSynWebOptionsBase)
  private
    function GetSmartyLDelim: String;
    procedure SetSmartyLDelim(const Value: String);
    function GetSmartyRDelim: String;  protected
    procedure SetSmartyRDelim(const Value: String);

  protected
    procedure UpdateMLOption; override;
  public
    constructor Create(AOptions: PSynWebOptions);
  published
    property SmartyLDelim: String read GetSmartyLDelim write SetSmartyLDelim;
    property SmartyRDelim: String read GetSmartyRDelim write SetSmartyRDelim;
    property Html5XmlMode;
    property HtmlVersion;
    property CssVersion;
    property PhpVersion;
    property CssEmbeded default True;
    property EsEmbeded default True;
    property UseEngineOptions;
  end;

{ TSynWebWmlOptions }

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
    property AllowASPTags;
    property PhpEmbeded default True;
    property UseEngineOptions;
  end;

{ TSynWebXsltOptions }

  TSynWebXsltOptions = class(TSynWebOptionsBase)
  protected
    procedure UpdateMLOption; override;
  public
    constructor Create(AOptions: PSynWebOptions);
  published
    property XsltVersion;
    property PhpVersion;
    property PhpShortOpenTag;
    property PhpAspTags;
    property AllowASPTags;
    property PhpEmbeded default True;
    property UseEngineOptions;
  end;

{ TSynWebXmlOptions }

  TSynWebXmlOptions = class(TSynWebOptionsBase)
  protected
    procedure UpdateMLOption; override;
  public
    constructor Create(AOptions: PSynWebOptions);
  published
    property PhpVersion;
    property PhpShortOpenTag default False;
    property PhpAspTags;
    property PhpEmbeded default True;
    property UseEngineOptions;
  end;

{ TSynWebCssOptions }

  TSynWebCssOptions = class(TSynWebOptionsBase)
  published
    property Html5XmlMode;
    property HtmlVersion;
    property CssVersion;
    property PhpVersion;
    property PhpShortOpenTag;
    property PhpAspTags;
    property PhpEmbeded;
    property UseEngineOptions;
  end;

{ TSynWebEsOptions }

  TSynWebEsOptions = class(TSynWebOptionsBase)
  published
    property PhpVersion;
    property PhpShortOpenTag;
    property PhpAspTags;
    property PhpEmbeded;
    property UseEngineOptions;
  end;

{ TSynWebPhpCliOptions }

  TSynWebPhpCliOptions = class(TSynWebOptionsBase)
  published
    property PhpVersion;
    property PhpShortOpenTag;
    property PhpAspTags;
    property UseEngineOptions;
  end;

{ TSynWebPhpPlainOptions }

  TSynWebPhpPlainOptions = class(TSynWebOptionsBase)
  published
    property PhpVersion;
    property UseEngineOptions;
  end;

{ TSynWebEngineOptions }

  TSynWebEngineOptions = class(TSynWebOptionsBase)
  public
    constructor Create(AOptions: PSynWebOptions);
  published
    property Html5XmlMode;
    property HtmlVersion;
    property WmlVersion;
    property XsltVersion;
    property CssVersion;
    property PhpVersion;
    property PhpShortOpenTag;
    property PhpAspTags;
  end;

{ TSynWebBase }

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
    procedure DoSetLine(const Value: UnicodeString; LineNumber: Integer); override;
{$ENDIF}
    procedure DoDefHighlightChange;
    function GetAttribCount: Integer; override;
    function GetAttribute(idx: Integer): TSynHighlighterAttributes; override;
{$IFNDEF UNISYNEDIT}
    function GetIdentChars: TSynIdentChars; override;
{$ENDIF}
{$IFDEF UNISYNEDIT}
    function GetSampleSource: UnicodeString; override;
{$ELSE}
    function GetSampleSource: String; override;
{$ENDIF}
  public
    procedure Assign(Source: TPersistent); override;
{$IFDEF UNISYNEDIT}
    class function GetFriendlyLanguageName: UnicodeString; override;
    class function SynWebSample: UnicodeString; virtual; abstract;
{$ELSE}
    class function SynWebSample: String; virtual; abstract;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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

    function GetCharBeforeToken: AnsiChar;
    function GetCharAfterToken: AnsiChar;

    function PhpGetKeywordId: Integer;
    function PhpGetFunctionId: Integer;
    function PhpGetSymbolId: Integer;
    function PhpGetRange: TSynWebPhpRangeState;

    function MLGetTagID: Integer;
    function MLGetTagKind: Integer;
    function MLGetRange: TSynWebMLRangeState;

    function CssGetPropertyId: Integer;
    function CssGetRange: TSynWebCssRangeState;

    function EsGetSymbolId: Integer;
    function EsGetRange: TSynWebEsRangeState;

    procedure SetRange(Value: Pointer); override;
{$IFNDEF UNISYNEDIT}
    procedure SetLine(NewValue: String; LineNumber: Integer); override;
{$ENDIF}
    procedure Next; override;
{$IFDEF UNISYNEDIT}
    function GetActiveHighlighter(ARange: Pointer; const ALine: UnicodeString;
      ACaretX, ACaretY: Integer): TSynWebHighlighterTypes;
{$ELSE}
    function GetActiveHighlighter(ARange: Pointer; const ALine: String;
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

{ TSynWebMLSyn }

  TSynWebMLSyn = class(TSynWebBase)
  private
    procedure SetupActiveHighlighter; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ResetRange; override;
  end;

{ TSynWebHtmlSyn }

  TSynWebHtmlSynClass = class of TSynWebHtmlSyn;

  TSynWebHtmlSyn = class(TSynWebMLSyn)
  private
    function GetOptions: TSynWebHtmlOptions;
    procedure SetOptions(const AValue: TSynWebHtmlOptions);
  public
    class function GetLanguageName: string; override;
{$IFDEF UNISYNEDIT}
    class function GetFriendlyLanguageName: UnicodeString; override;
    class function SynWebSample: UnicodeString; override;
{$ELSE}
    class function SynWebSample: String; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Options: TSynWebHtmlOptions read GetOptions write SetOptions;
  end;

{ TSynWebSmartySyn }

  TSynWebSmartySynClass = class of TSynWebSmartySyn;

  TSynWebSmartySyn = class(TSynWebMLSyn)
  private
    function GetOptions: TSynWebSmartyOptions;
    procedure SetOptions(const AValue: TSynWebSmartyOptions);
  public
    class function GetLanguageName: string; override;
{$IFDEF UNISYNEDIT}
    class function SynWebSample: UnicodeString; override;
{$ELSE}
    class function SynWebSample: String; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Options: TSynWebSmartyOptions read GetOptions write SetOptions;
  end;

{ TSynWebWmlSyn }

  TSynWebWmlSynClass = class of TSynWebWmlSyn;

  TSynWebWmlSyn = class(TSynWebMLSyn)
  private
    function GetOptions: TSynWebWmlOptions;
    procedure SetOptions(const AValue: TSynWebWmlOptions);
  public
    class function GetLanguageName: string; override;
{$IFDEF UNISYNEDIT}
    class function SynWebSample: UnicodeString; override;
{$ELSE}
    class function SynWebSample: String; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Options: TSynWebWmlOptions read GetOptions write SetOptions;
  end;

{ TSynWebXsltSyn }

  TSynWebXsltSynClass = class of TSynWebXsltSyn;

  TSynWebXsltSyn = class(TSynWebMLSyn)
  private
    function GetOptions: TSynWebXsltOptions;
    procedure SetOptions(const AValue: TSynWebXsltOptions);
  public
    class function GetLanguageName: string; override;
{$IFDEF UNISYNEDIT}
    class function SynWebSample: UnicodeString; override;
{$ELSE}
    class function SynWebSample: String; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Options: TSynWebXsltOptions read GetOptions write SetOptions;
  end;

{ TSynWebXmlSyn }

  TSynWebXmlSynClass = class of TSynWebXmlSyn;

  TSynWebXmlSyn = class(TSynWebMLSyn)
  private
    function GetOptions: TSynWebXmlOptions;
    procedure SetOptions(const AValue: TSynWebXmlOptions);
  public
    class function GetLanguageName: string; override;
{$IFDEF UNISYNEDIT}
    class function GetFriendlyLanguageName: UnicodeString; override;
    class function SynWebSample: UnicodeString; override;
{$ELSE}
    class function SynWebSample: String; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Options: TSynWebXmlOptions read GetOptions write SetOptions;
  end;

{ TSynWebCssSyn }

  TSynWebCssSynClass = class of TSynWebCssSyn;

  TSynWebCssSyn = class(TSynWebBase)
  private
    procedure SetupActiveHighlighter; override;
    function GetOptions: TSynWebCssOptions;
    procedure SetOptions(const AValue: TSynWebCssOptions);
  public
    class function GetLanguageName: string; override;
{$IFDEF UNISYNEDIT}
    class function GetFriendlyLanguageName: UnicodeString; override;
    class function SynWebSample: UnicodeString; override;
{$ELSE}
    class function SynWebSample: String; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    procedure ResetRange; override;
  published
    property Options: TSynWebCssOptions read GetOptions write SetOptions;
  end;

{ TSynWebEsSyn }

  TSynWebEsSynClass = class of TSynWebEsSyn;

  TSynWebEsSyn = class(TSynWebBase)
  private
    procedure SetupActiveHighlighter; override;
    function GetOptions: TSynWebEsOptions;
    procedure SetOptions(const AValue: TSynWebEsOptions);
  public
    class function GetLanguageName: string; override;
{$IFDEF UNISYNEDIT}
    class function GetFriendlyLanguageName: UnicodeString; override;
    class function SynWebSample: UnicodeString; override;
{$ELSE}
    class function SynWebSample: String; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    procedure ResetRange; override;
  published
    property Options: TSynWebEsOptions read GetOptions write SetOptions;
  end;

{ TSynWebPhpCliSyn }

  TSynWebPhpCliSynClass = class of TSynWebPhpCliSyn;

  TSynWebPhpCliSyn = class(TSynWebBase)
  private
    procedure SetupActiveHighlighter; override;
    function GetOptions: TSynWebPhpCliOptions;
    procedure SetOptions(const AValue: TSynWebPhpCliOptions);
  public
    class function GetLanguageName: string; override;
{$IFDEF UNISYNEDIT}
    class function SynWebSample: UnicodeString; override;
{$ELSE}
    class function SynWebSample: String; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    procedure ResetRange; override;
  published
    property Options: TSynWebPhpCliOptions read GetOptions write SetOptions;
  end;

{ TSynWebPhpPlainSyn }

  TSynWebPhpPlainSynClass = class of TSynWebPhpPlainSyn;

  TSynWebPhpPlainSyn = class(TSynWebBase)
  private
    procedure SetupActiveHighlighter; override;
    function GetOptions: TSynWebPhpPlainOptions;
    procedure SetOptions(const AValue: TSynWebPhpPlainOptions);
  public
    class function GetLanguageName: string; override;
{$IFDEF UNISYNEDIT}
    class function GetFriendlyLanguageName: UnicodeString; override;
    class function SynWebSample: UnicodeString; override;
{$ELSE}
    class function SynWebSample: String; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    procedure ResetRange; override;
  published
    property Options: TSynWebPhpPlainOptions read GetOptions write SetOptions;
  end;

{ TSynWebEngineSpecialAttributes }

  TSynWebSpecialAttriute = (swsaPhpVarPrefix, swsaPhpMarker, swsaTagScript, swsaTagStyle);
  TSynWebSpecialAttriutes = set of TSynWebSpecialAttriute;

  TSynWebEngineSpecialAttributes = class(TPersistent)
  private
    FEngine: TSynWebEngine;
    FOptions: TSynWebSpecialAttriutes;
    FInactiveOptions: TSynWebSpecialAttriutes;
    FAttributes: array[TSynWebSpecialAttriute] of TSynHighlighterAttributes;

    procedure SetOptions(AOptions: TSynWebSpecialAttriutes);
    procedure SetInactiveOptions(AOptions: TSynWebSpecialAttriutes);
    procedure SetAttribute(AIndex: Integer; const AAttribute: TSynHighlighterAttributes);
    function GetAttribute(AIndex: Integer): TSynHighlighterAttributes;
  public
    constructor Create(AOwner: TSynWebEngine);
  published
    property Options: TSynWebSpecialAttriutes read FOptions write SetOptions default [swsaPhpMarker];
    property InactiveOptions: TSynWebSpecialAttriutes read FInactiveOptions write SetInactiveOptions default [swsaPhpMarker];

    property PhpVarPrefix: TSynHighlighterAttributes index 0 read GetAttribute write SetAttribute;
    property PhpMarker: TSynHighlighterAttributes index 1 read GetAttribute write SetAttribute;
    property TagScript: TSynHighlighterAttributes index 2 read GetAttribute write SetAttribute;
    property TagStyle: TSynHighlighterAttributes index 3 read GetAttribute write SetAttribute;
  end;

{ TSynWebEngine }

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
    FSpecialAttri: TSynWebEngineSpecialAttributes;
    FIsSpecialAttribute: Boolean;
    FSpecialAttribute: TSynWebSpecialAttriute;

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
    FCssProcTable: array[AnsiChar] of TSynWebProcTableProc;
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
    
    FOnCssCheckVendorProperty: TSynWebCssCheckVendorPropertyEvent;
    FOnCssCheckVendorValue: TSynWebCssCheckVendorValueEvent;
    FOnCssGetVendorPropertyFlags: TSynWebCssGetVendorPropertyFlagsEvent;

    // ECMAScript --------------------------------------------------------------
    FEsProcTable: array[AnsiChar] of TSynWebProcTableProc;
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
    FPhpProcTable: array[AnsiChar] of TSynWebProcTableProc;
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
    procedure MLBraces;

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
    {$I SynHighlighterWeb_TagsFuncList.inc}

    function MLAttrKeyComp(const ID: Integer): Boolean;
    function MLAttrCheck: TSynWebTokenKind;
    {$I SynHighlighterWeb_AttrsFuncList.inc}

    function MLSpecialKeyComp(const ID: Integer): Boolean;
    function MLSpecialCheck(AStart, ALen: Integer): Integer;
    {$I SynHighlighterWeb_SpecialFuncList.inc}

    // Css ---------------------------------------------------------------------
    procedure CssMakeMethodTables;
    procedure CssNextBg;
    procedure CssNext;
    procedure CssUpdateBg;
    function CssGetRange: TSynWebCssRangeState;
    procedure CssSetRange(const ARange: TSynWebCssRangeState);
    function CssGetProp: Integer;
    function CssIsPropVendor: Boolean;
    procedure CssSetProp(const AProp: Integer);
    procedure CssSetPropVendor;
    function CssCheckPropData(ABit: Byte): Boolean;
    function CssCheckNull(ADo: Boolean = True): Boolean;
    procedure CssCheckVendor(var AIsVendor: Boolean);

    procedure CssSpaceProc;
    procedure CssAtKeywordProc;
    procedure CssSlashProc;
    procedure CssBraceOpenProc;
    procedure CssCurlyBraceOpenProc;
    procedure CssCurlyBraceCloseProc;
    procedure CssSelectorsProc;
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
    procedure CssNumberDefProc(APropValue: Boolean = True);
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
    {$I SynHighlighterWeb_CssPropsFuncList.inc}

    function CssValKeyComp(const ID: Integer): Boolean;
    function CssValCheck: TSynWebTokenKind;
    {$I SynHighlighterWeb_CssValsFuncList.inc}

    function CssSpecialKeyComp(const ID: Integer): Boolean;
    function CssSpecialCheck(AStart, ALen: Integer): Integer;
    {$I SynHighlighterWeb_CssSpecialFuncList.inc}

    // ECMAScript --------------------------------------------------------------
    procedure EsMakeMethodTables;
    procedure EsNext;
    function EsGetRange: TSynWebEsRangeState;
    procedure EsSetRange(const ARange: TSynWebEsRangeState);
    function EsCheckNull(ADo: Boolean = True): Boolean;
    procedure EsSetSymbolId(ASymbolId: Integer);

    procedure EsSpaceProc;
    procedure EsSlashProc;
    procedure EsLowerProc;
    procedure EsEqualProc;
    procedure EsNotProc;
    procedure EsGreaterProc;
    procedure EsAndProc;
    procedure EsPlusProc;
    procedure EsMinusProc;
    procedure EsOrProc;
    procedure EsMulProc;
    procedure EsModProc;
    procedure EsXorProc;
    procedure EsCurlyBraceOpenProc;
    procedure EsCurlyBraceCloseProc;
    procedure EsBoxBracketOpenProc;
    procedure EsBoxBracketCloseProc;
    procedure EsParentheseOpenProc;
    procedure EsParentheseCloseProc;
    procedure EsObjAccessProc;
    procedure EsSemiColonProc;
    procedure EsCommaProc;
    procedure EsQuestionProc;
    procedure EsColonProc;
    procedure EsTildeProc;
    procedure EsBackSlashProc;
    procedure EsNumberProc;
    procedure EsString34Proc;
    procedure EsString39Proc;
    procedure EsIdentProc;
    procedure EsErrorProc;

    procedure EsRangeDefaultProc;
    procedure EsRangeCommentProc;
    procedure EsRangeCommentMultiProc;
    procedure EsRangeString34Proc;
    procedure EsRangeString39Proc;
    procedure EsRangeRegExpProc;

    function EsKeywordComp(const ID: Integer): Boolean;
    function EsIdentCheck: TSynWebTokenKind;
    {$I SynHighlighterWeb_EsKeywordsFuncList.inc}

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

    function CheckSmartyBegin: Boolean;
    function CheckSmartyEnd: Boolean;
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
    procedure PhpBackslashProc;
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
    {$I SynHighlighterWeb_PhpKeywordsFuncList.inc}

    // Other -------------------------------------------------------------------
    procedure AddAttribute(AAttrib: TSynHighlighterAttributes);
    procedure AddToNotifyList(ASynWeb: TSynWebBase);
    procedure RemoveFromNotifyList(ASynWeb: TSynWebBase);

    procedure SetAttributesOnChange(AEvent: TNotifyEvent);
    procedure DefHighlightChange(Sender: TObject);

    function GetCrc8String(const AString: AnsiString): Byte;
    function GetRangeBit(ABit: Longword): Boolean;
    procedure SetRangeBit(ABit: Longword; AVal: Boolean);
    function GetRangeInt(ALen, APos: Longword): Longword;
    procedure SetRangeInt(ALen, APos, AVal: Longword);

    procedure SetSpecialAttribute(AType: TSynWebSpecialAttriute);

    procedure NullProc;
    procedure NextSetHighlighterType;
    procedure SetHighlighterType(const AHighlighterType: TSynWebHighlighterType;
      AClearBits: Boolean; ASetAtNextToken: Boolean; AUseNextAH: Boolean);
    procedure SetupHighlighterType(AClearBits: Boolean = False);
    procedure SetLine(const NewValue: AnsiString; LineNumber: Integer);
    procedure Next;
    function GetToken: AnsiString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // Global
    property InactiveAttri: TSynHighlighterAttributes
      read FInactiveAttri write FInactiveAttri;
    property Options: TSynWebEngineOptions read FOptions write FOptions;
    property SpecialAttri: TSynWebEngineSpecialAttributes read FSpecialAttri write FSpecialAttri;

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

    property OnCssCheckVendorProperty: TSynWebCssCheckVendorPropertyEvent
      read FOnCssCheckVendorProperty write FOnCssCheckVendorProperty;

    property OnCssCheckVendorValue: TSynWebCssCheckVendorValueEvent
      read FOnCssCheckVendorValue write FOnCssCheckVendorValue;

    property OnCssGetVendorPropertyFlags: TSynWebCssGetVendorPropertyFlagsEvent
      read FOnCssGetVendorPropertyFlags write FOnCssGetVendorPropertyFlags;

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
  AnsiStrings,
  Types,
  SynEditStrConst,
  StrUtils,
  Controls;

{ TSynWebOptionsBase }

constructor TSynWebOptionsBase.Create(AOptions: PSynWebOptions);
begin
  FOnChange := nil;
  FEngineOptions := nil;
  FOptions := AOptions;
  FUseEngineOptions := False;

  FOptions^.FHtmlVersion := shvXHtml10Transitional;
  FOptions^.FHtml5XmlMode := True;
  FOptions^.FXmlMode := True;
  FOptions^.FAllowASPTags := True;
  FOptions^.FWmlVersion := swvWml13;
  FOptions^.FXsltVersion := swvXslt20;
  FOptions^.FCssVersion := scvCss21;
  FOptions^.FPhpVersion := spvPhp5;
  FOptions^.FPhpShortOpenTag := True;
  FOptions^.FPhpAspTags := False;

  FOptions^.FPhpEmbeded := False;
  FOptions^.FCssEmbeded := False;
  FOptions^.FEsEmbeded := False;
end;

function TSynWebOptionsBase.GetHtml5XmlMode: Boolean;
begin
  Result := FOptions^.FHtml5XmlMode;
end;

procedure TSynWebOptionsBase.SetHtml5XmlMode(const Value: Boolean);
begin
  if UsingEngineOptions then
    Exit;
  FOptions^.FHtml5XmlMode := Value;
  UpdateMLOption;
  DoOnChange;
end;

function TSynWebOptionsBase.GetHtmlVersion: TSynWebHtmlVersion;
begin
  Result := FOptions^.FHtmlVersion;
end;

procedure TSynWebOptionsBase.SetHtmlVersion(const Value: TSynWebHtmlVersion);
begin
  if UsingEngineOptions then
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
  if UsingEngineOptions then
    Exit;
  FOptions^.FWmlVersion := Value;
  UpdateMLOption;
  DoOnChange;
end;

function TSynWebOptionsBase.GetXsltVersion: TSynWebXsltVersion;
begin
  Result := FOptions^.FXsltVersion;
end;

procedure TSynWebOptionsBase.SetXsltVersion(const Value: TSynWebXsltVersion);
begin
  if UsingEngineOptions then
    Exit;
  FOptions^.FXsltVersion := Value;
  UpdateMLOption;
  DoOnChange;
end;

function TSynWebOptionsBase.GetCssVersion: TSynWebCssVersion;
begin
  Result := FOptions^.FCssVersion;
end;

procedure TSynWebOptionsBase.SetCssVersion(const Value: TSynWebCssVersion);
begin
  if UsingEngineOptions then
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
  if UsingEngineOptions then
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
  if UsingEngineOptions then
    Exit;
  FOptions^.FPhpAspTags := Value;
  DoOnChange;
end;

function TSynWebOptionsBase.GetAllowASPTags: Boolean;
begin
  Result := FOptions^.FAllowASPTags;
end;

procedure TSynWebOptionsBase.SetAllowASPTags(const Value: Boolean);
begin
  if UsingEngineOptions then
    Exit;
  FOptions^.FAllowASPTags := Value;
  DoOnChange;
end;

function TSynWebOptionsBase.GetPhpShortOpenTag: Boolean;
begin
  Result := FOptions^.FPhpShortOpenTag;
end;

procedure TSynWebOptionsBase.SetPhpShortOpenTag(const Value: Boolean);
begin
  if UsingEngineOptions then
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
  if UsingEngineOptions then
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
  if UsingEngineOptions then
  begin
    FOptions^.FHtmlVersion := FEngineOptions^.FHtmlVersion;
    FOptions^.FHtml5XmlMode := FEngineOptions^.FHtml5XmlMode;
    FOptions^.FWmlVersion := FEngineOptions^.FWmlVersion;
    FOptions^.FXsltVersion := FEngineOptions^.FXsltVersion;
    UpdateMLOption;
    FOptions^.FCssVersion := FEngineOptions^.FCssVersion;
    FOptions^.FPhpVersion := FEngineOptions^.FPhpVersion;
    FOptions^.FPhpShortOpenTag := FEngineOptions^.FPhpShortOpenTag;
    FOptions^.FPhpAspTags := FEngineOptions^.FPhpAspTags;
  end;
end;

procedure TSynWebOptionsBase.UpdateMLOption;
begin
  case FOptions^.FMLVersion of
  smlhvHtml401Strict..smlhvHtml401Frameset:
    FOptions^.FXmlMode := False;

  smlhvHtml5:
    FOptions^.FXmlMode := FOptions^.FHtml5XmlMode;
  else
    FOptions^.FXmlMode := True;
  end;
end;

function TSynWebOptionsBase.UsingEngineOptions: Boolean;
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
      XsltVersion := Self.XsltVersion;
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

function TSynWebOptionsBase.IsXmlMode: Boolean;
begin
  Result := FOptions^.FXmlMode;
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
  inherited UpdateMLOption;
end;

{ TSynWebSmartyOptions }

constructor TSynWebSmartyOptions.Create(AOptions: PSynWebOptions);
begin
  inherited Create(AOptions);
  FOptions^.FSmartyLDelim := '{';
  FOptions^.FSmartyRDelim := '}';
  UpdateMLOption;
end;

function TSynWebSmartyOptions.GetSmartyLDelim: String;
begin
  Result := String(FOptions^.FSmartyLDelim);
end;

function TSynWebSmartyOptions.GetSmartyRDelim: String;
begin
  Result := String(FOptions^.FSmartyRDelim);
end;

procedure TSynWebSmartyOptions.SetSmartyLDelim(const Value: String);
begin
  if LeftStr(Value, 1) <> '{' then
    FOptions^.FSmartyLDelim := '{' + AnsiString(Value)
  else
    FOptions^.FSmartyLDelim := AnsiString(Value);
end;

procedure TSynWebSmartyOptions.SetSmartyRDelim(const Value: String);
begin
  if LeftStr(Value, 1) <> '}' then
    FOptions^.FSmartyRDelim := '}' + AnsiString(Value)
  else
    FOptions^.FSmartyRDelim := AnsiString(Value);
end;

procedure TSynWebSmartyOptions.UpdateMLOption;
begin
  FOptions^.FMLVersion := TSynWebMLVersion(FOptions^.FHtmlVersion);
  inherited UpdateMLOption;
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
  inherited UpdateMLOption;
end;

{ TSynWebXsltOptions }

constructor TSynWebXsltOptions.Create(AOptions: PSynWebOptions);
begin
  inherited Create(AOptions);
  UpdateMLOption;
end;

procedure TSynWebXsltOptions.UpdateMLOption;
begin
  FOptions^.FMLVersion := TSynWebMLVersion(Integer(smlwvXslt10) + Integer(FOptions^.FXsltVersion));
  inherited UpdateMLOption;
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
  inherited UpdateMLOption;
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
  if (Source <> nil) and (Source is TSynWebBase) then begin
    Engine := TSynWebBase(Source).Engine;
    fOptions.Assign(TSynWebBase(Source).fOptions);
  end;

  inherited;
end;

constructor TSynWebBase.Create(AOwner: TComponent);
begin
  FInstance.FHighlither := Self;
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
procedure TSynWebBase.DoSetLine(const Value: UnicodeString; LineNumber: Integer);
var
  s: AnsiString;
  p: PAnsiChar;
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
    p := PAnsiChar(s);
    w := PWideChar(Value);
    for i := 1 to j do
    begin
      if Word(w^) > 127 then
        p^ := 'a' // convert all 'high' Unicode charaters to 'a' to pass it as Identifier character
      else
        p^ := AnsiChar(w^);
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
  if FInstance.FOptions.FXmlMode then
    FInstance.FHashTable := @TSynWebSensitiveHashTable
  else
    FInstance.FHashTable := @TSynWebInsensitiveHashTable;
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
function TSynWebBase.GetSampleSource: UnicodeString;
{$ELSE}
function TSynWebBase.GetSampleSource: String;
{$ENDIF}
begin
  Result := SynWebSample;
end;

{$IFDEF UNISYNEDIT}
class function TSynWebBase.GetFriendlyLanguageName: UnicodeString;
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
    with FEngine do
      if (FInstance.FHighlighterType in FActiveHighlighters) then
      begin
        if FIsSpecialAttribute and (FSpecialAttribute in FSpecialAttri.Options) then
          Result := FSpecialAttri.FAttributes[FSpecialAttribute]
        else
          Result := FTokenAttributeTable[FInstance.FTokenID];
      end else
        if FIsSpecialAttribute and (FSpecialAttribute in FSpecialAttri.InactiveOptions) then
          Result := FSpecialAttri.FAttributes[FSpecialAttribute]
        else
          Result := FInactiveAttri;
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

function TSynWebBase.GetCharAfterToken: AnsiChar;
var
  lNextPos: Integer;
begin
  lNextPos := GetTokenPos + GetTokenLen + 1;
  if lNextPos < Length(FInstance.FLineRef) then
    Result := FInstance.FLineRef[lNextPos]
  else
    Result := #0;
end;

function TSynWebBase.GetCharBeforeToken: AnsiChar;
var
  lPrevPos: Integer;
begin
  lPrevPos := GetTokenPos;
  if lPrevPos >= 1 then
    Result := FInstance.FLineRef[lPrevPos]
  else
    Result := #0;
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

function TSynWebBase.MLGetTagID: Integer;
begin
  if (FEngine <> nil) and (FInstance.FHighlighterType = shtML) and
    (FEngine.MLGetRange in [srsMLTag, srsMLTagClose, srsMLTagKey, srsMLTagKeyEq,
    srsMLTagKeyValue, srsMLTagKeyValueQuoted1, srsMLTagKeyValueQuoted2]) then
    Result := FEngine.MLGetTag - 1
  else
    Result := -1;
end;

function TSynWebBase.MLGetTagKind: Integer;
begin
  if (FEngine = nil) or (FInstance.FHighlighterType <> shtML) then
    Result := 0
  else
    if FEngine.MLGetRange = srsMLTagClose then
      Result := -1
    else
      Result := 1;
end;

function TSynWebBase.MLGetRange: TSynWebMLRangeState;
begin
  if FEngine = nil then
    Result := srsMLText
  else
    Result := FEngine.MLGetRange;
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

function TSynWebBase.EsGetSymbolId: Integer;
begin
  if FInstance.FTokenID <> stkEsSymbol then
    Result := -1
  else
    Result := FInstance.FTokenLastSymbolId;
end;

function TSynWebBase.EsGetRange: TSynWebEsRangeState;
begin
  if FEngine = nil then
    Result := srsEsDefault
  else
    Result := FEngine.EsGetRange;
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
  const ALine: UnicodeString; ACaretX, ACaretY: Integer): TSynWebHighlighterTypes;
{$ELSE}
function TSynWebBase.GetActiveHighlighter(ARange: Pointer;
  const ALine: String; ACaretX, ACaretY: Integer): TSynWebHighlighterTypes;
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
  FInstance.FRange := CSYNWEB_RANGE_HTML;
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
  Result := 'TSynWeb: HTML/XHTML (+CSS, +ES, +PHP)';
end;

{$IFDEF UNISYNEDIT}
class function TSynWebHtmlSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangHTML;
end;

class function TSynWebHtmlSyn.SynWebSample: UnicodeString;
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

{ TSynWebSmartySyn }

constructor TSynWebSmartySyn.Create(AOwner: TComponent);
begin
  FOptions := TSynWebSmartyOptions.Create(@FInstance.FOptions);
  inherited Create(AOwner);
  FOptions.PhpEmbeded := False;
  FOptions.CssEmbeded := True;
  FOptions.EsEmbeded := True;
end;

function TSynWebSmartySyn.GetOptions: TSynWebSmartyOptions;
begin
  Result := TSynWebSmartyOptions(FOptions);
end;

procedure TSynWebSmartySyn.SetOptions(const AValue: TSynWebSmartyOptions);
begin
  FOptions := AValue;
end;

class function TSynWebSmartySyn.GetLanguageName: String;
begin
  Result := 'TSynWeb: Smarty (+CSS, +ES)';
end;

{$IFDEF UNISYNEDIT}
class function TSynWebSmartySyn.SynWebSample: UnicodeString;
{$ELSE}
class function TSynWebSmartySyn.SynWebSample: String;
{$ENDIF}
begin
  Result := '<!DOCTYPE html public "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'#13#10 +
    '<html xmlns="http://www.w3.org/1999/xhtml">'#13#10 +
    '<head>'#13#10 +
    '  <title>{$title}</title>'#13#10 +
    '</head>'#13#10 +
    #13#10 +
    '<body>'#13#10 +
    '{* My location here *}'#13#10 +
    '  <a href="{$location}">My location</a>'#13#10 +
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
class function TSynWebWmlSyn.SynWebSample: UnicodeString;
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

{ TSynWebXsltSyn }

constructor TSynWebXsltSyn.Create(AOwner: TComponent);
begin
  FOptions := TSynWebXsltOptions.Create(@FInstance.FOptions);
  inherited Create(AOwner);
  FOptions.PhpEmbeded := True;
  FOptions.CssEmbeded := False;
  FOptions.EsEmbeded := False;
end;

function TSynWebXsltSyn.GetOptions: TSynWebXsltOptions;
begin
  Result := TSynWebXsltOptions(FOptions);
end;

procedure TSynWebXsltSyn.SetOptions(const AValue: TSynWebXsltOptions);
begin
  FOptions := AValue;
end;

class function TSynWebXsltSyn.GetLanguageName: String;
begin
  Result := 'TSynWeb: XSLT (+PHP)';
end;

{$IFDEF UNISYNEDIT}
class function TSynWebXsltSyn.SynWebSample: UnicodeString;
{$ELSE}
class function TSynWebXsltSyn.SynWebSample: String;
{$ENDIF}
begin
  Result :=
    '<xsl:stylesheet version="2.0"'#13#10 +
    '                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">'#13#10 +
    #13#10 +
    '<xsl:template name="expand">'#13#10 +
    '  <xsl:element name="xsl:stylesheet">'#13#10 +
    '    <xsl:attribute name="version" select="@xsl:version"/>'#13#10 +
    '    <xsl:element name="xsl:template">'#13#10 +
    '      <xsl:attribute name="match">/</xsl:attribute>'#13#10 +
    '      <xsl:copy-of select="."/>'#13#10 +
    '    </xsl:element>'#13#10 +
    '  </xsl:element>'#13#10 +
    '</xsl:template>'#13#10 +
    #13#10 +
    '</xsl:stylesheet>';
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
  Result := 'TSynWeb: XML (+PHP)';
end;

{$IFDEF UNISYNEDIT}
class function TSynWebXmlSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangXML;
end;

class function TSynWebXmlSyn.SynWebSample: UnicodeString;
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
  Result := 'TSynWeb: CSS (+PHP)';
end;

{$IFDEF UNISYNEDIT}
class function TSynWebCssSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := 'CSS';
end;

class function TSynWebCssSyn.SynWebSample: UnicodeString;
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
  FInstance.FRange := CSYNWEB_RANGE_CSS;
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
class function TSynWebEsSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangJScript;
end;

class function TSynWebEsSyn.SynWebSample: UnicodeString;
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
  FInstance.FRange := CSYNWEB_RANGE_ES;
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
class function TSynWebPhpCliSyn.SynWebSample: UnicodeString;
{$ELSE}
class function TSynWebPhpCliSyn.SynWebSample: String;
{$ENDIF}
begin
  Result := '<?php'#13#10 +
    ''#13#10 +
    TSynWebPhpPlainSyn.SynWebSample +
    ''#13#10 +
    '?>'#13#10;
end;

procedure TSynWebPhpCliSyn.ResetRange;
begin
  FInstance.FRange := CSYNWEB_RANGE_HTML; // Same as ML/HTML
end;

{ TSynWebPhpPlainSyn }

constructor TSynWebPhpPlainSyn.Create(AOwner: TComponent);
begin
  FOptions := TSynWebPhpPlainOptions.Create(@FInstance.FOptions);
  FInstance.FHighlighterMode := shmML;
  inherited Create(AOwner);
  FOptions.PhpEmbeded := False;
  FOptions.CssEmbeded := False;
  FOptions.EsEmbeded := False;
end;

procedure TSynWebPhpPlainSyn.SetupActiveHighlighter;
begin
  FActiveHighlighters := [shtPhpInML];
end;

function TSynWebPhpPlainSyn.GetOptions: TSynWebPhpPlainOptions;
begin
  Result := TSynWebPhpPlainOptions(FOptions);
end;

procedure TSynWebPhpPlainSyn.SetOptions(const AValue: TSynWebPhpPlainOptions);
begin
  FOptions := AValue;
end;

class function TSynWebPhpPlainSyn.GetLanguageName: String;
begin
  Result := 'TSynWeb: PHP (Plain)';
end;

{$IFDEF UNISYNEDIT}
class function TSynWebPhpPlainSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangPHP;
end;

class function TSynWebPhpPlainSyn.SynWebSample: UnicodeString;
{$ELSE}
class function TSynWebPhpPlainSyn.SynWebSample: String;
{$ENDIF}
begin
  Result :=
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
    '}'#13#10;
end;

procedure TSynWebPhpPlainSyn.ResetRange;
begin
  FInstance.FRange := CSYNWEB_RANGE_PHPPLAIN;
end;

{ TSynWebEngineSpecialAttributes }

constructor TSynWebEngineSpecialAttributes.Create(AOwner: TSynWebEngine);

  function CreateAttrib(const AName:String): TSynHighlighterAttributes;
  begin
{$IFDEF UNISYNEDIT}
    Result := TSynHighlighterAttributes.Create(AName, AName);
{$ELSE}
    Result := TSynHighlighterAttributes.Create(AName);
{$ENDIF}
  end;

begin
  FEngine := AOwner;

  FOptions := [swsaPhpMarker];
  FInactiveOptions := [swsaPhpMarker];

  FAttributes[swsaPhpVarPrefix] := CreateAttrib('Special: Php Variable prefix');
  FEngine.AddAttribute(FAttributes[swsaPhpVarPrefix]);

  FAttributes[swsaPhpMarker] := CreateAttrib('Special: Php marker');
  FAttributes[swsaPhpMarker].Style := [fsBold];
  FAttributes[swsaPhpMarker].Foreground := clNavy;
  FEngine.AddAttribute(FAttributes[swsaPhpMarker]);

  FAttributes[swsaTagScript] := CreateAttrib('Special: Script tag');
  FEngine.AddAttribute(FAttributes[swsaTagScript]);

  FAttributes[swsaTagStyle] := CreateAttrib('Special: Style tag');
  FEngine.AddAttribute(FAttributes[swsaTagStyle]);
end;

procedure TSynWebEngineSpecialAttributes.SetOptions(AOptions: TSynWebSpecialAttriutes);
begin
  FOptions := AOptions;

  FEngine.DefHighlightChange(Self);
end;

procedure TSynWebEngineSpecialAttributes.SetInactiveOptions(AOptions: TSynWebSpecialAttriutes);
begin
  FInactiveOptions := AOptions;

  FEngine.DefHighlightChange(Self);
end;

procedure TSynWebEngineSpecialAttributes.SetAttribute(AIndex: Integer; const AAttribute: TSynHighlighterAttributes);
begin
  if (AIndex >= Integer(Low(TSynWebSpecialAttriute))) and
    (AIndex <= Integer(High(TSynWebSpecialAttriute)))
  then
    FAttributes[TSynWebSpecialAttriute(AIndex)] := AAttribute;
end;

function TSynWebEngineSpecialAttributes.GetAttribute(AIndex: Integer): TSynHighlighterAttributes;
begin
  if (AIndex >= Integer(Low(TSynWebSpecialAttriute))) and
    (AIndex <= Integer(High(TSynWebSpecialAttriute)))
  then
    Result := FAttributes[TSynWebSpecialAttriute(AIndex)]
  else
    Result := nil;
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

  FTokenAttributeTable[stkPhpSpace] := FPhpWhitespaceAttri;
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

  // Special
  FSpecialAttri := TSynWebEngineSpecialAttributes.Create(Self);

  SetAttributesOnChange(DefHighlightChange);
end;

destructor TSynWebEngine.Destroy;
var
  i: Integer;
begin
  FSpecialAttri.Free;
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
  {$I SynHighlighterWeb_TagsFuncTable.inc}

  pF := PSynWebIdentFuncTableFunc(@FMLAttrIdentFuncTable);
  for I := Low(FMLTagIdentFuncTable) to High(FMLAttrIdentFuncTable) do
  begin
    pF^ := MLAttrUndef;
    Inc(pF);
  end;
  {$I SynHighlighterWeb_AttrsFuncTable.inc}

  pF2 := PSynWebIdent2FuncTableFunc(@FMLSpecialIdentFuncTable);
  for I := Low(FMLSpecialIdentFuncTable) to High(FMLSpecialIdentFuncTable) do
  begin
    pF2^ := MLSpecialUndef;
    Inc(pF2);
  end;
  {$I SynHighlighterWeb_SpecialFuncTable.inc}
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
  Result := GetRangeInt(8, 0);
end;

procedure TSynWebEngine.MLSetTag(const ATag: Integer);
begin
  SetRangeInt(8, 0, Longword(ATag));
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
      if FInstance^.FOptions.FXmlMode then
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
        if (FInstance^.FOptions.FXmlMode) and
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
          if (TSynWebInsensitiveHashTable[FInstance^.FLine[FInstance^.FRun]] =
            TSynWebInsensitiveHashTable['D']) and
            (TSynWebInsensitiveHashTable[FInstance^.FLine[FInstance^.FRun + 1]] =
            TSynWebInsensitiveHashTable['O']) and
            (TSynWebInsensitiveHashTable[FInstance^.FLine[FInstance^.FRun + 2]] =
            TSynWebInsensitiveHashTable['C']) and
            (TSynWebInsensitiveHashTable[FInstance^.FLine[FInstance^.FRun + 3]] =
            TSynWebInsensitiveHashTable['T']) and
            (TSynWebInsensitiveHashTable[FInstance^.FLine[FInstance^.FRun + 4]] =
            TSynWebInsensitiveHashTable['Y']) and
            (TSynWebInsensitiveHashTable[FInstance^.FLine[FInstance^.FRun + 5]] =
            TSynWebInsensitiveHashTable['P']) and
            (TSynWebInsensitiveHashTable[FInstance^.FLine[FInstance^.FRun + 6]] =
            TSynWebInsensitiveHashTable['E']) and
            // (not (FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z'])) then
            (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun + 7]] and (1 shl 0) = 0) then
          begin
            FInstance^.FTokenID := stkMLTag;
            SetRangeInt(2, 8, 0);
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

procedure TSynWebEngine.MLBraces;
begin
  if PhpCheckBegin then
    Exit;
  Inc(FInstance^.FRun);
  FInstance^.FTokenID := stkMLText;
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
  '{':
    MLBraces;
  '&':
    MLAmpersandProc;
  else // case
    repeat
      Inc(FInstance^.FRun);
    until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 6) <> 0;
    // until FInstance^.FLine[FInstance^.FRun] In [#0..#32, '<', '>', '&', '{'];
    FInstance^.FTokenID := stkMLText;
  end;
end;

procedure TSynWebEngine.MLRangeCommentProc;
begin
  if MLCheckNull or PhpCheckBegin then
    Exit;
  repeat
    // while not (FInstance^.FLine[FInstance^.FRun) in [#0, '-', '<', '{']) do
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
    '<', '{':
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
    // while not (FInstance^.FLine[FInstance^.FRun) in [#0, '<', '>', '{']) do
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
    '<', '{':
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
  case GetRangeInt(2, 8) of
  0:
    begin
      Inc(FInstance^.FRun, 7);
      FInstance^.FTokenID := stkMLTagName;
      SetRangeInt(2, 8, 1);
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
          SetRangeInt(2, 8, 0);
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
            SetRangeInt(2, 8, 2);
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
            SetRangeInt(2, 8, 3);
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
            // while not (FInstance^.FLine[FInstance^.FRun] in [#0, #39, '<', '{']) do
            while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 21) = 0 do
              Inc(FInstance^.FRun);
            case FInstance^.FLine[FInstance^.FRun] of
            #0:
              begin
                FInstance^.FTokenID := stkMLError;
                Break;
              end;
            '<', '{':
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
      SetRangeInt(2, 8, 1);
    end;
  3:
    begin
      if not MLCheckNull then
        if PhpCheckBegin then
          Exit
        else
          repeat
            // while not (FInstance^.FLine[FInstance^.FRun] in [#0, '"', '<', '{']) do
            while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 22) = 0 do
              Inc(FInstance^.FRun);
            case FInstance^.FLine[FInstance^.FRun] of
            #0:
              begin
                FInstance^.FTokenID := stkMLError;
                Break;
              end;
            '<', '{':
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
      SetRangeInt(2, 8, 1);
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
        // until FInstance^.FLine[FInstance^.FRun] in [#0..#32, '<', ']', '{'];
        case FInstance^.FLine[FInstance^.FRun] of
        #0..#32, ']':
          Break;
        '<', '{':
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
  until TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 7) = 0;
  // until not (FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '0'..'9', ':', '-', '_']);

  if FInstance^.FLine[FInstance^.FRun] = ':' then
    Inc(FInstance^.FRun);

  // while FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '0'..'9', ':', '-', '_'] do
  while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 7) <> 0 do
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

    case ID of
    MLTagID_Script:
      SetSpecialAttribute(swsaTagScript);

    MLTagID_Style:
      SetSpecialAttribute(swsaTagStyle);
    end;

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
      // until not (FInstance^.FLine[FInstance^.FRun] In [#0..#32, '<', '>', '{']) do
      if (FInstance^.FLine[FInstance^.FRun] in ['<', '{']) and not PhpCheckBegin(False) then
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
        // until not(FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '0'..'9', ':', '-', '_']);
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
        (FInstance^.FOptions.FXmlMode) and
        ((ID = -1) or (TSynWeb_TagsData[ID] and (1 shl 31) <> 0)) then
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
          (FInstance^.FOptions.FXmlMode) then
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
        // until not(FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '0'..'9', ':', '-', '_']);
        if ID = -1 then
          FInstance^.FTokenID := stkMLTagKey{Undef} // TODO: Rethink?
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
    if FInstance^.FOptions.FXmlMode then
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
        MLSetRange(srsMLTagKeyValueQuoted1);
        FInstance^.FTokenID := stkMLTagKeyValueQuoted;
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
        MLSetRange(srsMLTagKeyValueQuoted2);
        FInstance^.FTokenID := stkMLTagKeyValueQuoted;
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
      ((FInstance^.FOptions.FXmlMode) and
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
        // until FInstance^.FLine[FInstance^.FRun] in [#0..#32, '<', '>', '/', '{'];
        case FInstance^.FLine[FInstance^.FRun] of
        '/':
          if (FInstance^.FLine[FInstance^.FRun + 1] = '>') and
            (FInstance^.FOptions.FXmlMode) then
            Break;
        '<', '{':
          if PhpCheckBegin(False) then
            Break
          else
            Inc(FInstance^.FRun);
        else // case
          Break;
        end;
      until False;
      if FInstance^.FOptions.FXmlMode then
        FInstance^.FTokenID := stkMLError
      else
        FInstance^.FTokenID := stkMLTagKeyValue;
      if GetRangeBit(27) then
        SetRangeBit(28, AnsiStrings.UpperCase(GetToken) = 'PHP');
      MLSetRange(srsMLTagKey);
    end;
  end;
end;

procedure TSynWebEngine.MLRangeTagKeyValueQuoted1Proc;
begin
  if MLCheckNull or PhpCheckBegin then
    Exit;

  repeat
    // while not (FInstance^.FLine[FInstance^.FRun] in [#0, #39, '<', '{']) do
    while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 21) = 0 do
      Inc(FInstance^.FRun);
    case FInstance^.FLine[FInstance^.FRun] of
    #0:
      begin
        FInstance^.FTokenID := stkMLTagKeyValueQuoted;
        Exit;
      end;
    '<', '{':
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
          SetRangeBit(28, AnsiStrings.UpperCase(GetToken) = #39'PHP'#39);
        Break;
      end;
    end;
  until False;

  MLSetRange(srsMLTagKey);
end;

procedure TSynWebEngine.MLRangeTagKeyValueQuoted2Proc;
begin
  if MLCheckNull or PhpCheckBegin then
    Exit;

  repeat
    // while not (FInstance^.FLine[FInstance^.FRun] in [#0, '"', '<', '{']) do
    while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 22) = 0 do
      Inc(FInstance^.FRun);
    case FInstance^.FLine[FInstance^.FRun] of
    #0:
      begin
        FInstance^.FTokenID := stkMLTagKeyValueQuoted;
        Exit;
      end;

    '<', '{':
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
          SetRangeBit(28, AnsiStrings.UpperCase(GetToken) = '"PHP"');
        Break;
      end;
    end;
  until False;

  MLSetRange(srsMLTagKey);
end;

function TSynWebEngine.MLTagKeyComp(const ID: Integer): Boolean;
var
  I: Integer;
  Temp: PAnsiChar;
  aKey: AnsiString;
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

  procedure KeyHash(ToHash: PAnsiChar);
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

  if (Result = stkMLTagNameUndef) and FInstance^.FOptions.FAllowASPTags then
    if (FInstance^.FStringLen > 4) and (FInstance^.FLine[FInstance^.FTokenPos + 3] = ':') then
      if (FInstance^.FHashTable[FInstance^.FToIdent[0]] = FInstance^.FHashTable['a']) and
         (FInstance^.FHashTable[FInstance^.FToIdent[1]] = FInstance^.FHashTable['s']) and
         (FInstance^.FHashTable[FInstance^.FToIdent[2]] = FInstance^.FHashTable['p'])
      then
        Result := stkMLTagName;

  MLSetTag(FInstance^.FTokenLastID + 1);
end;

{$I SynHighlighterWeb_TagsFunc.inc}

function TSynWebEngine.MLAttrKeyComp(const ID: Integer): Boolean;
var
  I, tag: Integer;
  Temp: PAnsiChar;
  aKey: AnsiString;
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

  procedure KeyHash(ToHash: PAnsiChar);
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

  if (Result = stkMLTagKeyUndef) and FInstance^.FOptions.FAllowASPTags then
    if FInstance^.FStringLen = 5 then
      if (FInstance^.FHashTable[FInstance^.FToIdent[0]] = FInstance^.FHashTable['r']) and
         (FInstance^.FHashTable[FInstance^.FToIdent[1]] = FInstance^.FHashTable['u']) and
         (FInstance^.FHashTable[FInstance^.FToIdent[2]] = FInstance^.FHashTable['n']) and
         (FInstance^.FHashTable[FInstance^.FToIdent[3]] = FInstance^.FHashTable['a']) and
         (FInstance^.FHashTable[FInstance^.FToIdent[4]] = FInstance^.FHashTable['t'])
      then
        Result := stkMLTagKey;
end;

{$I SynHighlighterWeb_AttrsFunc.inc}

function TSynWebEngine.MLSpecialKeyComp(const ID: Integer): Boolean;
var
  I: Integer;
  Temp: PAnsiChar;
  aKey: AnsiString;
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

  procedure KeyHash(ToHash: PAnsiChar);
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

{$I SynHighlighterWeb_SpecialFunc.inc}

// Css -------------------------------------------------------------------------

procedure TSynWebEngine.CssMakeMethodTables;
var
  c: AnsiChar;
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
    '*', '>', '~':
      FCssProcTable[c] := CssSelectorsProc;
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
  {$I SynHighlighterWeb_CssPropsFuncTable.inc}

  pF := PSynWebIdentFuncTableFunc(@FCssValIdentFuncTable);
  for I := Low(FCssValIdentFuncTable) to High(FCssValIdentFuncTable) do
  begin
    pF^ := CssValUndef;
    Inc(pF);
  end;
  {$I SynHighlighterWeb_CssValsFuncTable.inc}

  pF2 := PSynWebIdent2FuncTableFunc(@FCssSpecialIdentFuncTable);
  for I := Low(FCssSpecialIdentFuncTable) to High(FCssSpecialIdentFuncTable) do
  begin
    pF2^ := CssSpecialUndef;
    Inc(pF2);
  end;
  {$I SynHighlighterWeb_CssSpecialFuncTable.inc}
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
    if not (ARange in [TSynWebCssRangeStateRulesetBegin..TSynWebCssRangeStateRulesetEnd]) and
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
  case Result of
  TSynWebCssPropVendor:
    Result := 0;

  TSynWebCssPropInstance:
    Result := FInstance^.FCssInstancePropertyId;
  end;
end;

function TSynWebEngine.CssIsPropVendor: Boolean;
begin
  Result := GetRangeInt(8, 0) = TSynWebCssPropVendor;
end;

procedure TSynWebEngine.CssSetProp(const AProp: Integer);
begin
  FInstance^.FCssInstancePropertyId := AProp;

  if AProp >= TSynWebCssPropInstance then
    SetRangeInt(8, 0, TSynWebCssPropInstance)
  else
    SetRangeInt(8, 0, Longword(AProp));
end;

procedure TSynWebEngine.CssSetPropVendor;
begin
  SetRangeInt(8, 0, TSynWebCssPropVendor);
end;

function TSynWebEngine.CssCheckPropData(ABit: Byte): Boolean;
var
  lProp: Integer;
  lFlags: Cardinal;
begin
  if CssIsPropVendor then
  begin
    lFlags := $FFFFFFFF;

    if Assigned(FOnCssGetVendorPropertyFlags) then
    begin
      FOnCssGetVendorPropertyFlags(FInstance^.FCssVendorPropertyId, lFlags);
      lFlags := lFlags or $07; // Set first three bits (CSS1, CSS2.1, CSS3)
    end;

    Result := lFlags and (1 shl ABit) <> 0;
  end else
  begin
    lProp := CssGetProp;
    
    if lProp = 0 then
      Result := True // dont mark values for unknown properties as errors
    else
      Result := TSynWeb_CssPropsData[lProp - 1] and (1 shl ABit) <> 0;
  end;
end;

function TSynWebEngine.CssCheckNull(ADo: Boolean): Boolean;
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

procedure TSynWebEngine.CssCheckVendor(var AIsVendor: Boolean);
var
  lProperty: AnsiString;
begin
  lProperty := Copy(FInstance^.FLineRef, FInstance^.FTokenPos + 1, FInstance^.FStringLen);
  FInstance^.FCssVendorPropertyId := -1;

  FOnCssCheckVendorProperty(lProperty, AIsVendor, FInstance^.FCssVendorPropertyId);
  if not AIsVendor then
    FInstance^.FCssVendorPropertyId := -1;
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
    if
      (CssGetRange = srsCssPropVal)
      and
      (
        CssIsPropVendor
        or
        (
          (CssGetProp - 1 = CssPropID_Font)
          and
          (
            GetRangeBit(8) or
            (FInstance^.FTokenLastID = CssValID_Smaller) or
            (FInstance^.FTokenLastID = CssValID_Larger) or
            (FInstance^.FTokenLastID = CssValID_XX_Large) or
            (FInstance^.FTokenLastID = CssValID_X_Large) or
            (FInstance^.FTokenLastID = CssValID_Large) or
            (FInstance^.FTokenLastID = CssValID_Medium) or
            (FInstance^.FTokenLastID = CssValID_Small) or
            (FInstance^.FTokenLastID = CssValID_X_Small) or
            (FInstance^.FTokenLastID = CssValID_XX_Small)
          )
        )
      ) then
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
  if PhpCheckBegin then
    Exit;
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
    if GetRangeBit(11) then // Media range
    begin
      SetRangeBit(11, False);
      CssSymbolProc;
    end else
      CssErrorProc;
end;

procedure TSynWebEngine.CssSelectorsProc;
begin
  case FInstance^.FOptions.FCssVersion of
  scvCss1:
    CssErrorProc;

  scvCss21:
    if FInstance^.FLine[FInstance^.FRun] = '~' then
      CssErrorProc
    else
      CssSymbolProc;
  else
    CssSymbolProc;
  end;
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
begin
  if (CssGetRange = srsCssPropVal) and not CssCheckPropData(16{,}) then
    CssErrorProc
  else
    CssSymbolProc;
end;

procedure TSynWebEngine.CssColonProc;
begin
  if (FInstance^.FOptions.FCssVersion = scvCss3) and (FInstance^.FLine[FInstance^.FRun + 1] = ':') then
    Inc(FInstance^.FRun);
    
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
    SetRangeBit(8, False);
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
    if (FInstance^.FTokenID = stkCssValString) and not CssCheckPropData(19{String}) then
      FInstance^.FTokenID := stkCssValUndef;
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
    if FInstance^.FOptions.FCssVersion = scvCss1 then
      CssErrorProc
    else
      CssSymbolProc;
end;

procedure TSynWebEngine.CssMinusProc;
begin
  if CssGetRange = srsCssPropVal then
  begin
    Inc(FInstance^.FRun);

    // Vendor specific value like -moz-pre-wrap
    if CssIdentStartProc then
    begin
      FInstance^.FTokenID := stkCssVal;
      Exit;
    end;

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
    FInstance^.FCssMask := $F5420000;
    CssNumberDefProc;
  end else
    CssErrorProc;
end;

procedure TSynWebEngine.CssNumberDefProc(APropValue: Boolean);
var
  prop, OldRun: Integer;
  lFlags: Cardinal;

  procedure CheckOther;
  begin
    if (FInstance^.FRun - FInstance^.FTokenPos = 1) and
      (FInstance^.FLine[FInstance^.FRun - 1] = '0') then
      FInstance^.FCssMask := FInstance^.FCssMask and $F5400000
    else
      FInstance^.FCssMask := FInstance^.FCssMask and $1E20000;
    if (FInstance^.FTokenPos > 1) and ((FInstance^.FLine[FInstance^.FTokenPos - 1] = '/') and
      (FInstance^.FLine[FInstance^.FTokenPos - 2] <> '*')) then
      FInstance^.FCssMask := FInstance^.FCssMask or $18000000;
  end;

  function Check100_900: Boolean;
  begin
    Result := (FInstance^.FRun - FInstance^.FTokenPos = 3) and
      (FInstance^.FLine[FInstance^.FRun - 3] in ['1'..'9']) and
      (FInstance^.FLine[FInstance^.FRun - 2] = '0') and
      (FInstance^.FLine[FInstance^.FRun - 1] = '0');
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
    if APropValue then
    begin
      CssSetRange(srsCssPropValSpecial);
      if CssGetProp - 1 = CssPropID_Font then
        SetRangeBit(8, True); // allow next slash char in -> font: <'font-size as percent'> [ / <'line-height'> ]?
    end else
      Inc(FInstance^.FRun);
  end else
  begin
    OldRun := FInstance^.FRun;

    if (FInstance^.FOptions.FCssVersion = scvCss3) and not APropValue and (FInstance^.FLine[FInstance^.FRun] = 'n') then
      Inc(FInstance^.FRun)
    else
      if CssIdentStartProc then
      begin
        prop := CssSpecialCheck(OldRun, FInstance^.FRun - OldRun);
        if prop <> -1 then
        begin
          FInstance^.FCssMask := FInstance^.FCssMask and TSynWeb_CssSpecialData[prop];
          if APropValue then
            CssSetRange(srsCssPropValSpecial);

          if APropValue and (FInstance^.FLine[FInstance^.FRun] = '/') and
            (FInstance^.FLine[FInstance^.FRun + 1] <> '*')
          then
            SetRangeBit(8, True);

          if APropValue then
            FInstance^.FRun := OldRun;
        end else
          if FInstance^.FOptions.FCssVersion = scvCss1 then
          begin             
            if APropValue then
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

  if (FInstance^.FCssMask and (1 shl 17) <> 0) and not Check100_900 then
    FInstance^.FCssMask := FInstance^.FCssMask and not (1 shl 17);

  lFlags := $FFFFFFFF;
  if APropValue then
    if CssIsPropVendor then
    begin
      if Assigned(FOnCssGetVendorPropertyFlags) then
      begin
        FOnCssGetVendorPropertyFlags(FInstance^.FCssVendorPropertyId, lFlags);
        lFlags := lFlags or $07; // Set first three bits (CSS1, CSS2.1, CSS3)
      end;
    end else
    begin
      prop := CssGetProp - 1;
      if prop > -1 then
        lFlags := TSynWeb_CssPropsData[prop];
    end;

  if lFlags and FInstance^.FCssMask = 0 then
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
    // while not (FInstance^.FLine[FInstance^.FRun] in [#0, AChar, '\', '<', '{']) do
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
      '^', '$', '*':
        begin
          SetRangeInt(3, 8, 2);
          if FInstance^.FLine[FInstance^.FRun + 1] = '=' then
          begin
            Inc(FInstance^.FRun, 2);
            if FInstance^.FOptions.FCssVersion = scvCss3 then
              FInstance^.FTokenID := stkCssSymbol
            else
              FInstance^.FTokenID := stkCssError;
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
    if (prop = -1) or (TSynWeb_CssSpecialData[prop] and (1 shl 14{css pseudo}) = 0) or
      (TSynWeb_CssSpecialData[prop] and (1 shl Byte(FInstance^.FOptions.FCssVersion)) = 0) then
    begin
      FInstance^.FTokenID := stkCssError;
      CssSetRange(srsCssRuleset);
    end else
      if TSynWeb_CssSpecialData[prop] and (1 shl 10{pseudo with param}) = 0 then
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
        '+', '-':
          if FInstance^.FOptions.FCssVersion = scvCss3 then
          begin
            CssSymbolProc;  
            SetRangeBit(8, False);
          end else
            CssErrorProc;
        '0'..'9':
          if not GetRangeBit(8) then
          begin
            FInstance^.FCssMask := 1 shl 22{Integer};
            CssNumberDefProc(False);   
            SetRangeBit(8, True);
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
          if (FInstance^.FOptions.FCssVersion = scvCss3) and (FInstance^.FLine[FInstance^.FRun] = 'n') then
          begin
            Inc(FInstance^.FRun);
            FInstance^.FTokenID := stkCssValNumber;
            SetRangeBit(8, True);
          end else
            if CssIdentStartProc then
            begin
              if GetRangeBit(8) then
                FInstance^.FTokenID := stkCssError
              else
              begin
                FInstance^.FTokenID := stkCssVal;
                SetRangeBit(8, True);
              end;
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
            if not TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and
              (1 shl 14) = 0 then
            begin
              DoError;
              Exit;
            end;

            // while not (FInstance^.FLine[FInstance^.FRun] in [#0..#32, '(', ')', ',', '\', '<', '{']) do
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
            '<', '{':
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
      if FInstance^.FTokenLastID > -1 then
      begin
        if TSynWeb_CssValsData[FInstance^.FTokenLastID][Longword(FInstance^.FOptions.FCssVersion)]
          [9{TODO: must be last byte of array}] and (1 shl 31) <> 0 then
        begin
          if FInstance^.FLine[FInstance^.FRun] = '(' then
          begin
            SetRangeInt(3, 8, 0);
            case FInstance^.FTokenLastID of
            CssValID_Rgb, CssValID_Rgba, CssValID_Hsl, CssValID_Hsla:
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
        end;
      end;
    end else
      if CssIsPropVendor and (FInstance^.FLine[FInstance^.FRun] in ['(', ')', '=']) then
        CssSymbolProc
      else
        CssErrorProc;
end;

procedure TSynWebEngine.CssRangePropValStrProc;
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
  if (FInstance^.FTokenID = stkCssValString) and not CssCheckPropData(19{String}) then
    FInstance^.FTokenID := stkCssValUndef;
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
      if FInstance^.FOptions.FCssVersion = scvCss3 then
        FInstance^.FTokenID := stkCssValNumber
      else
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
        '0'..'9', '-', '+':
          begin
            FInstance^.FCssMask := $FFFFFFFF;
            CssNumberDefProc(False);
          end
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

      if CssCheckPropData(18{Color}) then
        FInstance^.FTokenID := stkCssValNumber
      else
        FInstance^.FTokenID := stkCssValUndef;
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

  procedure DoError;
  begin
    CssSetRange(srsCssRuleset);
    FCssProcTable[FInstance^.FLine[FInstance^.FRun]];
    FInstance^.FTokenID := stkCssError;
  end;

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
            if TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and
              (1 shl 14) <> 0 then
            begin
              DoError;
              Exit;
            end;

            // while not (FInstance^.FLine[FInstance^.FRun] in [#0..#32, '(', ')', ',', '\', '<', '{']) do
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
            '<', '{':
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
    // while not (FInstance^.FLine[FInstance^.FRun] in [#0, '*', '<', '{']) do
    while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 26) = 0 do
      Inc(FInstance^.FRun);
    case FInstance^.FLine[FInstance^.FRun] of
    #0:
      Break;
    '<', '{':
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
  Temp: PAnsiChar;
  aKey: AnsiString;
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
  lIsVendor: Boolean;

  procedure KeyHash(ToHash: PAnsiChar);
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
    if (FInstance^.FTokenLastID > -1) and
      (TSynWeb_CssPropsData[FInstance^.FTokenLastID] and
      (1 shl Longword(FInstance^.FOptions.FCssVersion)) = 0)
    then
      Result := stkCssPropUndef;
  end else
    Result := stkCssPropUndef;

  // Vendor specific tags
  if Result = stkCssPropUndef then
  begin
    lIsVendor := (FInstance^.FLine[FInstance^.FTokenPos] = '-') or (
      (FInstance^.FLine[FInstance^.FTokenPos] = 'm') and
      (FInstance^.FLine[FInstance^.FTokenPos + 1] = 's') and
      (FInstance^.FLine[FInstance^.FTokenPos + 2] = 'o') and
      (FInstance^.FLine[FInstance^.FTokenPos + 3] = '-')
    );
    if Assigned(FOnCssCheckVendorProperty) then
      CssCheckVendor(lIsVendor);

    if lIsVendor then
    begin
      Result := stkCssProp;
      FInstance^.FTokenLastID := -1;
      CssSetPropVendor;
      Exit;
    end;
  end;

  CssSetProp(FInstance^.FTokenLastID + 1);
end;

{$I SynHighlighterWeb_CssPropsFunc.inc}

function TSynWebEngine.CssValKeyComp(const ID: Integer): Boolean;
var
  I: Integer;
  Temp: PAnsiChar;
  aKey: AnsiString;
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

  procedure KeyHash(ToHash: PAnsiChar);
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

  function CheckVendorPropValue: Boolean;
  begin
    Result := True;
    if Assigned(FOnCssCheckVendorValue) then
      FOnCssCheckVendorValue(FInstance^.FCssVendorPropertyId, GetToken, Result);
  end;

begin
  if CssIsPropVendor then
  begin
    if CheckVendorPropValue then
      Result := stkCssVal
    else
      Result := stkCssValUndef;
  end else
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
        if prop > -1 then
          if (TSynWeb_CssValsData[FInstance^.FTokenLastID]
            [Longword(FInstance^.FOptions.FCssVersion)][prop div 32] and (1 shl (prop mod 32)) = 0)
          then
            Result := stkCssValUndef;
      end;
    end else
      Result := stkCssValUndef;
  end;

  if (Result = stkCssValUndef) and CssCheckPropData(20{identifier}) then
    Result := stkCssSymbol;
end;

{$I SynHighlighterWeb_CssValsFunc.inc}

function TSynWebEngine.CssSpecialKeyComp(const ID: Integer): Boolean;
var
  I: Integer;
  Temp: PAnsiChar;
  aKey: AnsiString;
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

  procedure KeyHash(ToHash: PAnsiChar);
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

{$I SynHighlighterWeb_CssSpecialFunc.inc}

// ECMAScript ------------------------------------------------------------------

procedure TSynWebEngine.EsMakeMethodTables;
var
  c: AnsiChar;
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
    '=':
      FEsProcTable[c] := EsEqualProc;
    '!':
      FEsProcTable[c] := EsNotProc;
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
    '*':
      FEsProcTable[c] := EsMulProc;
    '%':
      FEsProcTable[c] := EsModProc;
    '^':
      FEsProcTable[c] := EsXorProc;
    '0'..'9':
      FEsProcTable[c] := EsNumberProc;
    '"':
      FEsProcTable[c] := EsString34Proc;
    #39:
      FEsProcTable[c] := EsString39Proc;
    '{':
      FEsProcTable[c] := EsCurlyBraceOpenProc;
    '}':
      FEsProcTable[c] := EsCurlyBraceCloseProc;
    '[':
      FEsProcTable[c] := EsBoxBracketOpenProc;
    ']':
      FEsProcTable[c] := EsBoxBracketCloseProc;
    '(':
      FEsProcTable[c] := EsParentheseOpenProc;
    ')':
      FEsProcTable[c] := EsParentheseCloseProc;
    '.':
      FEsProcTable[c] := EsObjAccessProc;
    ';':
      FEsProcTable[c] := EsSemiColonProc;
    ',':
      FEsProcTable[c] := EsCommaProc;
    '?':
      FEsProcTable[c] := EsQuestionProc;
    ':':
      FEsProcTable[c] := EsColonProc;
    '~':
      FEsProcTable[c] := EsTildeProc;
    '\':
      FEsProcTable[c] := EsBackSlashProc;
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
  FEsRangeProcTable[srsEsRegExp] := EsRangeRegExpProc;

  pF := PSynWebIdentFuncTableFunc(@FEsIdentFuncTable);
  for I := Low(FEsIdentFuncTable) to High(FEsIdentFuncTable) do
  begin
    pF^ := EsKeywordIdent;
    Inc(pF);
  end;
  {$I SynHighlighterWeb_EsKeywordsFuncTable.inc}
end;

procedure TSynWebEngine.EsNext;
begin
  FInstance^.FTokenPos := FInstance^.FRun;
  FEsRangeProcTable[EsGetRange];

  case FInstance^.FTokenID of
  stkEsIdentifier, stkEsNumber, stkEsString:
    SetRangeBit(13, True);
  stkEsKeyword, stkEsError:
    SetRangeBit(13, False);
  stkEsSymbol:
    SetRangeBit(13, GetToken = ')');
  end;
end;

function TSynWebEngine.EsGetRange: TSynWebEsRangeState;
begin
  Result := TSynWebEsRangeState(GetRangeInt(3, 14));
end;

procedure TSynWebEngine.EsSetRange(const ARange: TSynWebEsRangeState);
begin
  SetRangeInt(3, 14, Longword(ARange));
end;

procedure TSynWebEngine.EsSetSymbolId(ASymbolId: Integer);
begin
  FInstance^.FTokenID := stkEsSymbol;
  FInstance^.FTokenLastSymbolId := ASymbolId;
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
    end;
  '=':
    begin
      EsSetSymbolId(EsSymbolID_DivAssign);
      Inc(FInstance^.FRun);
    end;
  '/':
    begin
      Inc(FInstance^.FRun);
      EsSetRange(srsEsComment);
      if EsCheckNull(False) or PhpCheckBegin(False) then
        FInstance^.FTokenID := stkEsComment
      else
        EsRangeCommentProc;
    end;
  else
    if not GetRangeBit(13) then // Allow regExpr
    begin
      EsSetSymbolId(EsSymbolID_RegExprInlineStart);
      SetRangeInt(2, 11, 0);
      EsSetRange(srsEsRegExp);
    end else
      EsSetSymbolId(EsSymbolID_Div);
  end;
end;

procedure TSynWebEngine.EsLowerProc;
begin
  if EsCheckNull or PhpCheckBegin then
    Exit;

  Inc(FInstance^.FRun);
  case FInstance^.FLine[FInstance^.FRun] of
  '=':
    begin
      EsSetSymbolId(EsSymbolID_LowerEqual);
      Inc(FInstance^.FRun);
    end;
  '<':
    if not PhpCheckBegin(False) then
    begin
      Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] = '=' then
      begin
        EsSetSymbolId(EsSymbolID_ShiftLeftAssign);
        Inc(FInstance^.FRun);
      end else
        EsSetSymbolId(EsSymbolID_ShiftLeft);
    end else
      EsSetSymbolId(EsSymbolID_Lower);
  else
    EsSetSymbolId(EsSymbolID_Lower);
  end;
end;

procedure TSynWebEngine.EsEqualProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = '=' then
  begin
    Inc(FInstance^.FRun);
    if FInstance^.FLine[FInstance^.FRun] = '=' then
    begin
      EsSetSymbolId(EsSymbolID_Identical);
      Inc(FInstance^.FRun);
    end else
      EsSetSymbolId(EsSymbolID_Equal);
  end else
    EsSetSymbolId(EsSymbolID_Assign);
end;

procedure TSynWebEngine.EsNotProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = '=' then
  begin
    Inc(FInstance^.FRun);
    if FInstance^.FLine[FInstance^.FRun] = '=' then
    begin
      Inc(FInstance^.FRun);
      EsSetSymbolId(EsSymbolID_NotIdentical)
    end else
      EsSetSymbolId(EsSymbolID_NotEqual);
  end else
    EsSetSymbolId(EsSymbolID_Not);
end;

procedure TSynWebEngine.EsGreaterProc;
begin
  Inc(FInstance^.FRun);
  case FInstance^.FLine[FInstance^.FRun] of
  '=':
    begin
      EsSetSymbolId(EsSymbolID_GreaterEqual);
      Inc(FInstance^.FRun);
    end;
  '>':
    begin
      Inc(FInstance^.FRun);
      if FInstance^.FLine[FInstance^.FRun] = '=' then
      begin
        EsSetSymbolId(EsSymbolID_ShiftRightAssign);
        Inc(FInstance^.FRun);
      end else
        EsSetSymbolId(EsSymbolID_ShiftRight);
    end;
  else
    EsSetSymbolId(EsSymbolID_Greater);
  end;
end;

procedure TSynWebEngine.EsAndProc;
begin
  Inc(FInstance^.FRun);
  case FInstance^.FLine[FInstance^.FRun] of
  '=':
    begin
      EsSetSymbolId(EsSymbolID_BitwiseAndAssign);
      Inc(FInstance^.FRun);
    end;
  '&':
    begin
      EsSetSymbolId(EsSymbolID_LogicAnd);
      Inc(FInstance^.FRun);
    end;
  else
    EsSetSymbolId(EsSymbolID_BitwiseAnd);
  end;
end;

procedure TSynWebEngine.EsPlusProc;
begin
  Inc(FInstance^.FRun);
  case FInstance^.FLine[FInstance^.FRun] of
  '=':
    begin
      EsSetSymbolId(EsSymbolID_AddAssign);
      Inc(FInstance^.FRun);
    end;
  '+':
    begin
      EsSetSymbolId(EsSymbolID_Increment);
      Inc(FInstance^.FRun);
    end;
  else
    EsSetSymbolId(EsSymbolID_Add);
  end;
end;

procedure TSynWebEngine.EsMinusProc;
begin
  Inc(FInstance^.FRun);
  case FInstance^.FLine[FInstance^.FRun] of
  '=':
    begin
      EsSetSymbolId(EsSymbolID_DecAssign);
      Inc(FInstance^.FRun);
    end;
  '-':
    begin
      EsSetSymbolId(EsSymbolID_Decrement);
      Inc(FInstance^.FRun);
    end;
  else
    EsSetSymbolId(EsSymbolID_Dec);
  end;
end;

procedure TSynWebEngine.EsOrProc;
begin
  Inc(FInstance^.FRun);
  case FInstance^.FLine[FInstance^.FRun] of
  '=':
    begin
      EsSetSymbolId(EsSymbolID_BitwiseOrAssign);
      Inc(FInstance^.FRun);
    end;
  '|':
    begin
      EsSetSymbolId(EsSymbolID_LogicOr);
      Inc(FInstance^.FRun);
    end;
  else
    EsSetSymbolId(EsSymbolID_BitwiseOR);
  end;
end;

procedure TSynWebEngine.EsMulProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = '=' then
  begin
    Inc(FInstance^.FRun);
    EsSetSymbolId(EsSymbolID_MulAssign);
  end else
    EsSetSymbolId(EsSymbolID_Mul);
end;

procedure TSynWebEngine.EsModProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = '=' then
  begin
    Inc(FInstance^.FRun);
    EsSetSymbolId(EsSymbolID_ModAssign);
  end else
    EsSetSymbolId(EsSymbolID_Mod);
end;

procedure TSynWebEngine.EsXorProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = '=' then
  begin
    Inc(FInstance^.FRun);
    EsSetSymbolId(EsSymbolID_XorAssign);
  end else
    EsSetSymbolId(EsSymbolID_Xor);
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

procedure TSynWebEngine.EsCurlyBraceOpenProc;
begin
  if PhpCheckBegin then
    Exit;
  Inc(FInstance^.FRun);
  EsSetSymbolId(EsSymbolID_BraceOpen);
end;

procedure TSynWebEngine.EsCurlyBraceCloseProc;
begin
  Inc(FInstance^.FRun);
  EsSetSymbolId(EsSymbolID_BraceClose);
end;

procedure TSynWebEngine.EsBoxBracketOpenProc;
begin
  Inc(FInstance^.FRun);
  EsSetSymbolId(EsSymbolID_BoxBracketOpen);
end;

procedure TSynWebEngine.EsBoxBracketCloseProc;
begin
  Inc(FInstance^.FRun);
  EsSetSymbolId(EsSymbolID_BoxBracketClose);
end;

procedure TSynWebEngine.EsParentheseOpenProc;
begin
  Inc(FInstance^.FRun);
  EsSetSymbolId(EsSymbolID_ParentheseOpen);
end;

procedure TSynWebEngine.EsParentheseCloseProc;
begin
  Inc(FInstance^.FRun);
  EsSetSymbolId(EsSymbolID_ParentheseClose);
end;

procedure TSynWebEngine.EsObjAccessProc;
begin
  Inc(FInstance^.FRun);
  EsSetSymbolId(EsSymbolID_ObjAccess);
end;

procedure TSynWebEngine.EsSemiColonProc;
begin
  Inc(FInstance^.FRun);
  EsSetSymbolId(EsSymbolID_SemiColon);
end;

procedure TSynWebEngine.EsCommaProc;
begin
  Inc(FInstance^.FRun);
  EsSetSymbolId(EsSymbolID_Comma);
end;

procedure TSynWebEngine.EsQuestionProc;
begin
  Inc(FInstance^.FRun);
  EsSetSymbolId(EsSymbolID_Question);
end;

procedure TSynWebEngine.EsColonProc;
begin
  Inc(FInstance^.FRun);
  EsSetSymbolId(EsSymbolID_Colon);
end;

procedure TSynWebEngine.EsTildeProc;
begin
  Inc(FInstance^.FRun);
  EsSetSymbolId(EsSymbolID_Tilde);
end;

procedure TSynWebEngine.EsBackSlashProc;
begin
  Inc(FInstance^.FRun);
  EsSetSymbolId(EsSymbolID_BackSlash);
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
        until FInstance^.FLine[FInstance^.FRun] in [#0, '<', '{'];
        case FInstance^.FLine[FInstance^.FRun] of
        #0:
          begin
            FInstance^.FTokenID := stkEsComment;
            Break;
          end;
        '<', '{':
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
    // while not (FInstance^.FLine[FInstance^.FRun] in [#0, '*', '<', '{']) do
    while TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 26) = 0 do
      Inc(FInstance^.FRun);
    case FInstance^.FLine[FInstance^.FRun] of
    #0:
      Break;
    '<', '{':
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
  if MLCheckNull then
  begin
    if (FInstance^.FRun > 0) and (EsGetRange = srsEsString34) then
      Exit;
  end else
    if PhpCheckBegin then
      Exit
    else
      repeat
        // while not (FInstance^.FLine[FInstance^.FRun] in [#0, #34, '<', '\', '{']) do
        while TSynWebIdentTable2[FInstance^.FLine[FInstance^.FRun]] and (1 shl 3) = 0 do
          Inc(FInstance^.FRun);
        case FInstance^.FLine[FInstance^.FRun] of
        #0:
          begin
            FInstance^.FTokenID := stkEsError;
            Break;
          end;
        '<', '{':
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
            if FInstance^.FLine[FInstance^.FRun] in [#34, '\'] then
              Inc(FInstance^.FRun)
            else
              if FInstance^.FLine[FInstance^.FRun] = #0 then
              begin
                FInstance^.FTokenID := stkEsString;
                EsSetRange(srsEsString34);
                Exit;
              end;
          end;
        end;
      until False;
  EsSetRange(srsEsDefault);
end;

procedure TSynWebEngine.EsRangeString39Proc;
begin
  if  MLCheckNull then
  begin
    if (FInstance^.FRun > 0) and (EsGetRange = srsEsString39) then
      Exit;
  end else
    if PhpCheckBegin then
      Exit
    else
      repeat
        // while not (FInstance^.FLine[FInstance^.FRun] in [#0, #39, '<', '\', '{']) do
        while TSynWebIdentTable2[FInstance^.FLine[FInstance^.FRun]] and (1 shl 4) = 0 do
          Inc(FInstance^.FRun);
        case FInstance^.FLine[FInstance^.FRun] of
        #0:
          begin
            FInstance^.FTokenID := stkEsError;
            Break;
          end;
        '<', '{':
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
            if FInstance^.FLine[FInstance^.FRun] in [#39, '\'] then
              Inc(FInstance^.FRun)
            else
              if FInstance^.FLine[FInstance^.FRun] = #0 then
              begin
                FInstance^.FTokenID := stkEsString;
                EsSetRange(srsEsString39);
                Exit;
              end;
          end;
        end;
      until False;
  EsSetRange(srsEsDefault);
end;

procedure TSynWebEngine.EsRangeRegExpProc;
var
  lBrace: Integer;

  procedure SkipSpace;
  begin
    while FInstance^.FLine[FInstance^.FRun] in [#1..#32] do
      Inc(FInstance^.FRun);
  end;

  procedure RegExpInvalid;
  begin
    FInstance^.FTokenID := stkEsError;
  end;

  function ScanNumber: Boolean;
  begin
    Result := FInstance^.FLine[FInstance^.FRun] in ['0'..'9'];
    if Result then
      repeat
        Inc(FInstance^.FRun);
      until not (FInstance^.FLine[FInstance^.FRun] in ['0'..'9']);
  end;

  procedure BraceDelta(ADelta: Integer);
  begin
    Inc(FInstance^.FRun);
    Inc(lBrace, ADelta);
    if lBrace < 0 then
      RegExpInvalid;
  end;

  procedure ScanBackslash;

    function IsHex(const AChar: AnsiChar): Boolean;
    begin
      Result := AChar in ['0'..'9', 'a'..'f', 'A'..'F'];
    end;

  begin
    Inc(FInstance^.FRun);

    case FInstance^.FLine[FInstance^.FRun] of
    #0:
      RegExpInvalid;

    'c':
      begin
        Inc(FInstance^.FRun);
        if FInstance^.FLine[FInstance^.FRun] in ['A'..'Z'] then
          Inc(FInstance^.FRun);
      end;

    'x':
      begin
        Inc(FInstance^.FRun);
        if IsHex(FInstance^.FLine[FInstance^.FRun]) and
          IsHex(FInstance^.FLine[FInstance^.FRun + 1])
        then
          Inc(FInstance^.FRun, 2)
        else
          RegExpInvalid;
      end;

    'u':
      begin
        Inc(FInstance^.FRun);
        if IsHex(FInstance^.FLine[FInstance^.FRun]) and
          IsHex(FInstance^.FLine[FInstance^.FRun + 1]) and
          IsHex(FInstance^.FLine[FInstance^.FRun + 2]) and
          IsHex(FInstance^.FLine[FInstance^.FRun + 3])
        then
          Inc(FInstance^.FRun, 4);
      end;

    else
      Inc(FInstance^.FRun);
    end;
  end;

begin
  SetRangeBit(13, True);

  case GetRangeInt(2, 11) of
  1:
    begin
      Inc(FInstance^.FRun);
      EsSetSymbolId(EsSymbolID_RegExprInlineEnd);

      if FInstance^.FLine[FInstance^.FRun] in ['i', 'g', 'm'] then
        SetRangeInt(2, 11, 2)
      else
      begin
        EsSetRange(srsEsDefault);
        SetRangeInt(2, 11, 1);
      end;

      Exit;
    end;

  2:
    begin
      repeat
        Inc(FInstance^.FRun);
      until not (FInstance^.FLine[FInstance^.FRun] in ['i', 'g', 'm']);

      FInstance^.FTokenID := stkEsString;
      EsSetRange(srsEsDefault);
      SetRangeInt(2, 11, 0);
      Exit;
    end;
  end;

  lBrace := 0;
  FInstance^.FTokenID := stkEsString;

  while True do
  begin
    SkipSpace;

    case FInstance^.FLine[FInstance^.FRun] of
    '/', #0:
      Break;

    '(':
      BraceDelta(1);

    ')':
      BraceDelta(-1);

    '\':
      ScanBackslash;

    '{':
      begin
        Inc(FInstance^.FRun);
        SkipSpace;

        if ScanNumber then
        begin
          SkipSpace;
          if FInstance^.FLine[FInstance^.FRun] = ',' then
          begin
            Inc(FInstance^.FRun);
            SkipSpace;
            if ScanNumber then
              SkipSpace;
          end;
        end else
        begin
          RegExpInvalid;
          while not (FInstance^.FLine[FInstance^.FRun] in [#0, '}', '/']) do
            Inc(FInstance^.FRun);
        end;

        if FInstance^.FLine[FInstance^.FRun] = '}' then
          Inc(FInstance^.FRun)
        else
          RegExpInvalid;
      end;

    '[':
      begin
        Inc(FInstance^.FRun);
        while True do
          case FInstance^.FLine[FInstance^.FRun] of
          '\':
            ScanBackslash;
          #0, ']':
            Break;
          else
            Inc(FInstance^.FRun);
          end;

        if FInstance^.FLine[FInstance^.FRun] = ']' then
          Inc(FInstance^.FRun)
        else
          RegExpInvalid;
      end;
    else // case
      Inc(FInstance^.FRun);
    end;
  end;

  if lBrace <> 0 then
    RegExpInvalid;

  if FInstance^.FLine[FInstance^.FRun] = '/' then
    SetRangeInt(2, 11, 1)
  else
  begin
    RegExpInvalid;
    EsSetRange(srsEsDefault);
  end;
end;

function TSynWebEngine.EsKeywordComp(const ID: Integer): Boolean;
var
  I: Integer;
  Temp: PAnsiChar;
  aKey: AnsiString;
begin
  aKey := TSynWeb_EsKeywords[ID];
  Temp := FInstance^.FToIdent;
  if Length(aKey) = FInstance^.FStringLen then
  begin
    for i := 1 to FInstance^.FStringLen do
    begin
      if TSynWebSensitiveHashTable[Temp^] <> TSynWebSensitiveHashTable[aKey[i]] then
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

  procedure KeyHash(ToHash: PAnsiChar);
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

{$I SynHighlighterWeb_EsKeywordsFunc.inc}

// Php -------------------------------------------------------------------------

procedure TSynWebEngine.PhpMakeMethodTables;
var
  c: AnsiChar;
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
    '\':
      FPhpProcTable[c] := PhpBackslashProc;
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
  {$I SynHighlighterWeb_PhpKeywordsFuncTable.inc}
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
    SetRangeInt(3, 17, 0);
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
  case FInstance^.FLine[FInstance^.FRun] of
  '{':
    if (FInstance^.FHighlither is TSynWebSmartySyn) and CheckSmartyBegin then
    begin
      if ABegin then
        PhpBegin(spotPhpShort);
    end else
      Exit;
  '<':
    if FInstance^.FOptions.FPhpEmbeded then
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
  else // case
    Exit;
  end;

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

function TSynWebEngine.CheckSmartyBegin: Boolean;
var
  i: Integer;
  s: AnsiString;
  p: PAnsiChar;
begin
  s := FInstance^.FOptions.FSmartyLDelim;
  p := @FInstance^.FLine[FInstance^.FRun];
  for i := 1 to Length(s) do
    if p^ = s[i] then
      Inc(p)
    else
    begin
      Result := False;
      Exit;
    end;

  Result := True;
end;

function TSynWebEngine.CheckSmartyEnd: Boolean;
var
  i: Integer;
  s: AnsiString;
  p: PAnsiChar;
begin
  s := FInstance^.FOptions.FSmartyRDelim;
  p := @FInstance^.FLine[FInstance^.FRun];
  for i := 1 to Length(s) do
    if p^ = s[i] then
      Inc(p)
    else
    begin
      Result := False;
      Exit;
    end;

  Result := True;
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
    SetSpecialAttribute(swsaPhpMarker);
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
  '>':
    begin
      Inc(FInstance^.FRun);
      PhpSetSymbolId(PhpSymbolID_NotEqual2);
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
  if FInstance^.FHighlither is TSynWebSmartySyn then
    PhpDivProc
  else
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

procedure TSynWebEngine.PhpBackslashProc;
begin
  Inc(FInstance^.FRun);
  PhpSetSymbolId(PhpSymbolID_Backslash);

  SetRangeBit(18, True); // Next token str is identifier
end;

procedure TSynWebEngine.PhpPercentProc;
begin
  if (FInstance^.FLine[FInstance^.FRun + 1] = '>') and FInstance^.FOptions.FPhpEmbeded then
  begin
    Inc(FInstance^.FRun, 2);
    if FInstance^.FOptions.FPhpAspTags then
    begin
      FInstance^.FTokenID := stkMLTag;
      SetSpecialAttribute(swsaPhpMarker);
      PhpEnd(False);
    end else
      FInstance^.FTokenID := stkPhpError;
  end else
    PhpModProc;
end;

procedure TSynWebEngine.PhpHashProc;
begin
  if FInstance^.FHighlither is TSynWebSmartySyn then
  begin
    Inc(FInstance^.FRun);
    FInstance^.FTokenID := stkPhpSymbol;
    Exit;
  end;
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
  SetRangeInt(3, 17, 0);
end;

procedure TSynWebEngine.PhpBraceCloseProc;
begin
  if (FInstance^.FHighlither is TSynWebSmartySyn) and CheckSmartyEnd then
  begin
    Inc(FInstance^.FRun, Length(FInstance^.FOptions.FSmartyRDelim));
    FInstance^.FTokenID := stkMLTag;
    SetSpecialAttribute(swsaPhpMarker);
    PhpEnd(False);
  end else
  begin
    Inc(FInstance^.FRun);
    PhpSetSymbolId(PhpSymbolID_BraceClose);
  end;
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
  if GetRangeBit(19) then
    SetRangeBit(18, True);
end;

procedure TSynWebEngine.PhpSemiColonProc;
begin
  Inc(FInstance^.FRun);
  PhpSetSymbolId(PhpSymbolID_SemiColon);
  SetRangeInt(3, 17, 0);
end;

procedure TSynWebEngine.PhpVarProc;
begin
  Inc(FInstance^.FRun);
  if FInstance^.FLine[FInstance^.FRun] = '$' then
    Inc(FInstance^.FRun);
  // if FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '_', #$7F..#$FF] then
  if (TSynWebIdentTable[FInstance^.FLine[FInstance^.FRun]] and (1 shl 28) <> 0)
    or (FInstance^.FLine[FInstance^.FRun] = '{') then
  begin
    SetSpecialAttribute(swsaPhpVarPrefix);
    FInstance^.FTokenID := stkPhpKeyword;
  end else
    FInstance^.FTokenID := stkPhpError;
  FInstance^.FTokenLastID := PhpKeyID_Special_Variable;
  SetRangeInt(3, 17, 0);
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
  StringChar: AnsiChar;

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
  s: AnsiString;
  i: Integer;

  procedure DoDefault(ARange: TSynWebPhpRangeState = srsPhpDefault);
  begin
    SetRangeInt(3, 20, 0);
    PhpSetRange(ARange);
  end;

begin
  case GetRangeInt(3, 20) of
  0:
    begin
      if FInstance^.FHighlither is TSynWebSmartySyn then
      begin
        Inc(FInstance^.FRun, Length(FInstance^.FOptions.FSmartyLDelim));
        if FInstance^.FLine[FInstance^.FRun] = '*' then
        begin
          Inc(FInstance^.FRun);
          FInstance^.FTokenID := stkPhpComment;
          DoDefault(srsPhpComment);
          Exit;
        end else
          DoDefault;
      end else
      begin
        Inc(FInstance^.FRun, 2);
        SetRangeInt(3, 20, 1);
      end;
      SetSpecialAttribute(swsaPhpMarker);
      FInstance^.FTokenID := stkMLTag;
    end;
  1:
    begin
      if GetRangeBit(19) then
      begin
        DoDefault;
        SetRangeBit(19, False);
        Inc(FInstance^.FRun, 3);
        SetSpecialAttribute(swsaPhpMarker);
        FInstance^.FTokenID := stkPhpKeyword;
        FInstance^.FTokenLastID := PhpKeyID_Special_PhpTag;
      end else
      begin
        DoDefault;
        if (FInstance^.FLine[FInstance^.FRun] = '=') and (FInstance^.FOptions.FPhpShortOpenTag) then
        begin
          Inc(FInstance^.FRun);
          SetSpecialAttribute(swsaPhpMarker);
          FInstance^.FTokenID := stkPhpKeyword;
          FInstance^.FTokenLastID := PhpKeyID_Special_PhpTagEcho;
        end else
          PhpRangeDefaultProc;
      end;
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
      i := FPhpHereDocList.IndexOf(String(s));
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
  if FInstance^.FHighlither is TSynWebSmartySyn then
  begin
    repeat
      while not (FInstance^.FLine[FInstance^.FRun] in [#0, '*'])  do
        Inc(FInstance^.FRun);
      case FInstance^.FLine[FInstance^.FRun] of
      #0:
        Break;
      '*':
        begin
          Inc(FInstance^.FRun);
          if CheckSmartyEnd then
          begin
            Inc(FInstance^.FRun, Length(FInstance^.FOptions.FSmartyRDelim));
            PhpEnd(False);
            Break;
          end else
            Inc(FInstance^.FRun);
        end;
      end;
    until False;
  end else
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
      // if FInstance^.FLine[FInstance^.FRun + 1] in ['a'..'z', 'A'..'Z', '-'] then
      if TSynWebIdentTable2[FInstance^.FLine[FInstance^.FRun + 1]] and (1 shl 6) <> 0 then
      begin
        Inc(FInstance^.FRun, 2);
        // while FInstance^.FLine[FInstance^.FRun] in ['a'..'z', 'A'..'Z', '-'] do
        while TSynWebIdentTable2[FInstance^.FLine[FInstance^.FRun]] and (1 shl 6) <> 0 do
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
  s: AnsiString;
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
      if (GetRangeBit(25) and (String(s) = FPhpHereDocList[GetRangeInt(8, 17)])) or
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
  Temp: PAnsiChar;
  aKey: AnsiString;
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
    SetRangeInt(2, 18, Data shr 30);
    Result := True;
  end else
    Result := False;
end;

function TSynWebEngine.PhpConstComp: Boolean;
var
  I: Integer;
  Temp: PAnsiChar;
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
  Temp: PAnsiChar;
  aKey: AnsiString;
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

  procedure KeyHash(ToHash: PAnsiChar);
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
  if GetRangeBit(18) then
  begin
    Result := stkPhpIdentifier;
    SetRangeBit(18, False);
    Exit;
  end;
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

{$I SynHighlighterWeb_PhpKeywordsFunc.inc}

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

function TSynWebEngine.GetCrc8String(const AString: AnsiString): Byte;
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
  {$IFDEF CPUX64}
  i:= (i shl APos) or (i shr (32-APos));
  {$ELSE}
  asm
    mov ecx, APos
    rol i, cl
  end;
  {$ENDIF}
  FInstance^.FRange := (FInstance^.FRange and i) or ((AVal shl APos) and not i);
end;

procedure TSynWebEngine.SetSpecialAttribute(AType: TSynWebSpecialAttriute);
begin
  FIsSpecialAttribute := True;
  FSpecialAttribute := AType;
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

procedure TSynWebEngine.SetLine(const NewValue: AnsiString; LineNumber: Integer);
{$IFDEF SYNWEB_FIXNULL}
var
  s, e: PAnsiChar;
  i:Integer;
{$ENDIF}
begin
  FInstance^.FLineRef := NewValue;
{$IFDEF SYNWEB_FIXNULL}
  i := Length(FInstance^.FLineRef);
  if i > 0  then
  begin
    s := PAnsiChar(FInstance^.FLineRef);
    e := s + i;
    repeat
      if s^ = #0 then
        s^ := #32;
      Inc(s);
    until s = e;
  end;
{$ENDIF}
  FInstance^.FLine := PAnsiChar(FInstance^.FLineRef);
  FInstance^.FRun := 0;
  FInstance^.FLineNumber := LineNumber;
  FInstance^.FHighlighterType := TSynWebHighlighterType(GetRangeInt(3, 29));
  FInstance^.FPrevHighlighterType := FInstance^.FHighlighterType;
  FInstance^.FHighlighterSW := False;
  FInstance^.FCssVendorPropertyId := -1;
  SetupHighlighterType;
{$IFNDEF UNISYNEDIT}
  FInstance^.FNextProcTable;
{$ENDIF}
end;

procedure TSynWebEngine.Next;
begin
  FIsSpecialAttribute := False;
  FInstance^.FHighlighterSW := False;
  FInstance^.FNextProcTable;
end;

function TSynWebEngine.GetToken: AnsiString;
var
  Len: longint;
begin
  Len := FInstance^.FRun - FInstance^.FTokenPos;
  SetString(Result, (FInstance^.FLine + FInstance^.FTokenPos), Len);
end;

initialization

{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynWebHtmlSyn);
  RegisterPlaceableHighlighter(TSynWebCssSyn);
  RegisterPlaceableHighlighter(TSynWebEsSyn);
  RegisterPlaceableHighlighter(TSynWebWmlSyn);
  RegisterPlaceableHighlighter(TSynWebXsltSyn);
  RegisterPlaceableHighlighter(TSynWebXmlSyn);
  RegisterPlaceableHighlighter(TSynWebPhpCliSyn);
  RegisterPlaceableHighlighter(TSynWebPhpPlainSyn);
{$ENDIF}

end.

