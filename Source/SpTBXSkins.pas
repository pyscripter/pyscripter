unit SpTBXSkins;

{==============================================================================
Version 2.5.2

The contents of this file are subject to the SpTBXLib License; you may
not use or distribute this file except in compliance with the
SpTBXLib License.
A copy of the SpTBXLib License may be found in SpTBXLib-LICENSE.txt or at:
  http://www.silverpointdevelopment.com/sptbxlib/SpTBXLib-LICENSE.htm

Alternatively, the contents of this file may be used under the terms of the
Mozilla Public License Version 1.1 (the "MPL v1.1"), in which case the provisions
of the MPL v1.1 are applicable instead of those in the SpTBXLib License.
A copy of the MPL v1.1 may be found in MPL-LICENSE.txt or at:
  http://www.mozilla.org/MPL/

If you wish to allow use of your version of this file only under the terms of
the MPL v1.1 and not to allow others to use your version of this file under the
SpTBXLib License, indicate your decision by deleting the provisions
above and replace them with the notice and other provisions required by the
MPL v1.1. If you do not delete the provisions above, a recipient may use your
version of this file under either the SpTBXLib License or the MPL v1.1.

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The initial developer of this code is Robert Lee.

Requirements:
  - Jordan Russell's Toolbar 2000
    http://www.jrsoftware.org

Development notes:
  - All the Windows and Delphi bugs fixes are marked with '[Bugfix]'.
  - All the theme changes and adjustments are marked with '[Theme-Change]'.
  - DT_END_ELLIPSIS and DT_PATH_ELLIPSIS doesn't work with rotated text
    http://support.microsoft.com/kb/249678
  - Vista theme elements are defined on the Themes unit on Delphi XE2 and up.
    For older versions of Delphi we should fill the element details manually.
    When XE2 Styles are used menus and toolbar elements (teMenu and teToolbar)
    are painted by TCustomStyleMenuElements.DrawElement.
    State is passed by TCustomStyle.GetElementDetails(Detail: TThemedMenu) as:
    State = Integer(Detail), Part is not used:
    function TCustomStyle.GetElementDetails(Detail: TThemedMenu): TThemedElementDetails;
    begin
      Result.Element := teMenu;
      Result.Part := 0;
      Result.State := Integer(Detail);
    end;
    All this adjustments are marked with '[Old-Themes]'

History:
28 October 2014 - version 2.5.2
  - No changes.

28 May 2014 - version 2.5.1
  - No changes.

18 March 2014 - version 2.5
  - Minor fixes.

15 April 2013 - version 2.4.8
  - No changes.

7 February 2012 - version 2.4.7
  - Minor bug fixes.
  - Added support for Delphi XE2.
  - Added support for 64 bit Delphi compiler.

25 June 2011 - version 2.4.6
  - No changes.

12 March 2010 - version 2.4.5
  - No changes.

2 December 2009 - version 2.4.4
  - Renamed the OfficeMenuSeparator skin option to OfficeMenu.

13 September 2009 - version 2.4.3
  - Improved the gradient painting performance, it's 2x faster on
    Vista/Win7, thanks to Kyan and Jim Kueneman for the code donation.
  - Fixed CurrentSkin.GetTextColor bug, it didn't return the
    correct skncDockablePanelTitleBar text color when using
    the EOS skin, thanks to Aaron Taylor for reporting this.
  - Fixed CurrentSkin.GetTextColor bug, it didn't return the
    correct skncButton disabled text color on Windows Vista,
    thanks to Arvid for reporting this.

8 May 2009 - version 2.4.2
  - No changes.

15 March 2009 - version 2.4.1
  - Added OnSkinChange event to TSpTBXSkinManager.

17 January 2009 - version 2.4
  - Minor Fixes.

26 September 2008 - version 2.3
  - Fixed incorrect skin loading when the Aluminum skin was used,
    thanks to Costas Stergiou for reporting this.

29 July 2008 - version 2.2
  - Fixed incorrect menu items painting on Vista when the Windows
    themes was disabled, thanks to Arvid for reporting this.

26 June 2008 - version 2.1
  - Added Windows Vista specific constants to support Vista
    themes on Delphi versions prior to 2007, thanks to Wolf B.
    for his contribution.

3 May 2008 - version 2.0
  - Renamed TSpTBXSkinOptions.TitleBarBorderSize to
    FloatingWindowBorderSize.

2 April 2008 - version 1.9.5
  - No changes.

3 February 2008 - version 1.9.4
  - Added TitleBarBorderSize to the skins options.

19 January 2008 - version 1.9.3
  - No changes.

26 December 2007 - version 1.9.2
  - New gradient skin style added to mimic Vista toolbar gradients, use
    9 or 10 gradient style to paint vertically or horizontally.

1 December 2007 - version 1.9.1
  - Added Header and Tabs Toolbar skinning. skncHeader and skncTabToolbar
    skin elements were added to the skin components type.
  - Added SpDrawXPHeader utility function to paint the header controls.

20 November 2007 - version 1.9
  - Initial release.

==============================================================================}

interface

{$BOOLEVAL OFF} // Unit depends on short-circuit boolean evaluation

uses
  Windows, Messages, Classes, SysUtils, Graphics, Controls, StdCtrls,
  ImgList, IniFiles, Types,
  {$IF CompilerVersion >= 25} // for Delphi XE4 and up
  System.UITypes,
  {$IFEND}
  Themes;

const
  WM_SPSKINCHANGE = WM_APP + 2007;   // Skin change notification message

type
  { Skins }

  TSpTBXSkinType = (
    sknNone,         // No themes
    sknWindows,      // Use Windows themes
    sknSkin,         // Use Skins
    sknDelphiStyle   // Use Delphi Custom Styles
  );

  TSpTBXLunaScheme = (
    lusBlue,
    lusMetallic,
    lusGreen,
    lusUnknown
  );

  TSpTBXSkinComponentsType = (
    skncDock,
    skncDockablePanel,
    skncDockablePanelTitleBar,
    skncGutter,
    skncMenuBar,
    skncOpenToolbarItem,
    skncPanel,
    skncPopup,
    skncSeparator,
    skncSplitter,
    skncStatusBar,
    skncStatusBarGrip,
    skncTabBackground,
    skncTabToolbar,
    skncToolbar,
    skncToolbarGrip,
    skncWindow,
    skncWindowTitleBar,

    // Multiple States
    skncMenuBarItem,
    skncMenuItem,
    skncToolbarItem,
    skncButton,
    skncCheckBox,
    skncEditButton,
    skncEditFrame,
    skncHeader,
    skncLabel,
    skncListItem,
    skncProgressBar,
    skncRadioButton,
    skncTab,
    skncTrackBar,
    skncTrackBarButton
  );
  TSpTBXSkinStatesType = (sknsNormal, sknsDisabled, sknsHotTrack, sknsPushed, sknsChecked, sknsCheckedAndHotTrack);

  TSpTBXSkinStatesSet = set of TSpTBXSkinStatesType;

  TSpTBXSkinPartsType = (sknpBody, sknpBorders, sknpText);

  TSpTBXSkinComponentsIdentEntry = record
    Name: string;
    States: TSpTBXSkinStatesSet;
  end;

const
  SpTBXSkinMultiStateComponents: set of TSpTBXSkinComponentsType = [skncMenuBarItem..High(TSpTBXSkinComponentsType)];

  CSpTBXSkinAllStates = [Low(TSpTBXSkinStatesType)..High(TSpTBXSkinStatesType)];
  CSpTBXSkinComponents: array [TSpTBXSkinComponentsType] of TSpTBXSkinComponentsIdentEntry = (
    // Single state Components
    (Name: 'Dock';                  States: [sknsNormal]),
    (Name: 'DockablePanel';         States: [sknsNormal]),
    (Name: 'DockablePanelTitleBar'; States: [sknsNormal]),
    (Name: 'Gutter';                States: [sknsNormal]),
    (Name: 'MenuBar';               States: [sknsNormal]),
    (Name: 'OpenToolbarItem';       States: [sknsNormal]),
    (Name: 'Panel';                 States: [sknsNormal]),
    (Name: 'Popup';                 States: [sknsNormal]),
    (Name: 'Separator';             States: [sknsNormal]),
    (Name: 'Splitter';              States: [sknsNormal]),
    (Name: 'StatusBar';             States: [sknsNormal]),
    (Name: 'StatusBarGrip';         States: [sknsNormal]),
    (Name: 'TabBackground';         States: [sknsNormal]),
    (Name: 'TabToolbar';            States: [sknsNormal]),
    (Name: 'Toolbar';               States: [sknsNormal]),
    (Name: 'ToolbarGrip';           States: [sknsNormal]),
    (Name: 'Window';                States: [sknsNormal]),
    (Name: 'WindowTitleBar';        States: [sknsNormal]),
    // Multi state Components
    (Name: 'MenuBarItem';           States: CSpTBXSkinAllStates),
    (Name: 'MenuItem';              States: CSpTBXSkinAllStates),
    (Name: 'ToolbarItem';           States: CSpTBXSkinAllStates),
    (Name: 'Button';                States: CSpTBXSkinAllStates),
    (Name: 'CheckBox';              States: CSpTBXSkinAllStates),
    (Name: 'EditButton';            States: CSpTBXSkinAllStates),
    (Name: 'EditFrame';             States: [sknsNormal, sknsDisabled, sknsHotTrack]),
    (Name: 'Header';                States: [sknsNormal, sknsDisabled, sknsHotTrack, sknsPushed]),
    (Name: 'Label';                 States: [sknsNormal, sknsDisabled]),
    (Name: 'ListItem';              States: CSpTBXSkinAllStates),
    (Name: 'ProgressBar';           States: [sknsNormal, sknsHotTrack]),
    (Name: 'RadioButton';           States: CSpTBXSkinAllStates),
    (Name: 'Tab';                   States: CSpTBXSkinAllStates),
    (Name: 'TrackBar';              States: [sknsNormal, sknsHotTrack]),
    (Name: 'TrackBarButton';        States: [sknsNormal, sknsPushed])
  );

  SSpTBXSkinStatesString: array [TSpTBXSkinStatesType] of string = ('Normal', 'Disabled', 'HotTrack', 'Pushed', 'Checked', 'CheckedAndHotTrack');
  SSpTBXSkinDisplayStatesString: array [TSpTBXSkinStatesType] of string = ('Normal', 'Disabled', 'Hot', 'Pushed', 'Checked', 'Checked && Hot');  

type
  { Text }

  TSpTextRotationAngle = (
    tra0,                      // No rotation
    tra90,                     // 90 degree rotation
    tra270                     // 270 degree rotation
  );

  TSpTBXTextInfo = record
    Text: string;
    TextAngle: TSpTextRotationAngle;
    TextFlags: Cardinal;
    TextSize: TSize;
    IsCaptionShown: Boolean;
    IsTextRotated: Boolean;
  end;

  TSpGlyphLayout = (
    ghlGlyphLeft,                 // Glyph icon on the left of the caption
    ghlGlyphTop                   // Glyph icon on the top of the caption
  );

  TSpGlowDirection = (
    gldNone,                      // No glow
    gldAll,                       // Glow on Left, Top, Right and Bottom of the text
    gldTopLeft,                   // Glow on Top-Left of the text
    gldBottomRight                // Glow on Bottom-Right of the text
  );

  { MenuItem }

  TSpTBXComboPart = (cpNone, cpCombo, cpSplitLeft, cpSplitRight);
  TSpTBXMenuItemMarginsInfo = record
    Margins: TRect;               // MenuItem margins
    GutterSize: Integer;          // Size of the gutter
    LeftCaptionMargin: Integer;   // Left margin of the caption
    RightCaptionMargin: Integer;  // Right margin of the caption
    ImageTextSpace: Integer;      // Space between the Icon and the caption
  end;

  TSpTBXMenuItemInfo = record
    Enabled: Boolean;
    HotTrack: Boolean;
    Pushed: Boolean;
    Checked: Boolean;
    HasArrow: Boolean;
    ImageShown: Boolean;
    ImageOrCheckShown: Boolean;
    ImageSize: TSize;
    RightImageSize: TSize;
    IsDesigning: Boolean;
    IsOnMenuBar: Boolean;
    IsOnToolbox: Boolean;
    IsOpen: Boolean;
    IsSplit: Boolean;
    IsSunkenCaption: Boolean;
    IsVertical: Boolean;
    MenuMargins: TSpTBXMenuItemMarginsInfo; // Used only on menu items
    ComboPart: TSpTBXComboPart;
    ComboRect: TRect;
    ComboState: TSpTBXSkinStatesType;
    ToolbarStyle: Boolean;
    State: TSpTBXSkinStatesType;
    SkinType: TSpTBXSkinType;
  end;

  { Colors }

  TSpTBXColorTextType = (
    cttDefault,        // Default format (clWhite, $FFFFFF)
    cttHTML,           // HTML format (#FFFFFF)
    cttIdentAndHTML    // Use color idents (clWhite), if not possible use HTML format
  );

  { TSpTBXSkinOptions }

  TSpTBXSkinOptionEntry = class(TPersistent)
  private
    FSkinType: Integer;
    FColor1, FColor2, FColor3, FColor4: TColor;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    procedure Fill(ASkinType: Integer; AColor1, AColor2, AColor3, AColor4: TColor);
    procedure ReadFromString(S: string);
    function WriteToString: string;
    function IsEmpty: Boolean;
    function IsEqual(AOptionEntry: TSpTBXSkinOptionEntry): Boolean;
    procedure Lighten(Amount: Integer);
    procedure Reset;
  published
    property SkinType: Integer read FSkinType write FSkinType;
    property Color1: TColor read FColor1 write FColor1;
    property Color2: TColor read FColor2 write FColor2;
    property Color3: TColor read FColor3 write FColor3;
    property Color4: TColor read FColor4 write FColor4;
  end;

  TSpTBXSkinOptionCategory = class(TPersistent)
  private
    FBody: TSpTBXSkinOptionEntry;
    FBorders: TSpTBXSkinOptionEntry;
    FTextColor: TColor;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function IsEmpty: Boolean;
    procedure Reset;
    procedure LoadFromIni(MemIni: TMemIniFile; Section, Ident: string);
    procedure SaveToIni(MemIni: TMemIniFile; Section, Ident: string);
  published
    property Body: TSpTBXSkinOptionEntry read FBody write FBody;
    property Borders: TSpTBXSkinOptionEntry read FBorders write FBorders;
    property TextColor: TColor read FTextColor write FTextColor;
  end;

  TSpTBXSkinOptions = class(TPersistent)
  private
    FColorBtnFace: TColor;
    FFloatingWindowBorderSize: Integer;
    FOptions: array [TSpTBXSkinComponentsType, TSpTBXSkinStatesType] of TSpTBXSkinOptionCategory;
    FOfficeIcons: Boolean;
    FOfficeMenu: Boolean;
    FOfficeStatusBar: Boolean;
    FSkinAuthor: string;
    FSkinName: string;
    function GetOfficeIcons: Boolean;
    function GetOfficeMenu: Boolean;
    function GetOfficePopup: Boolean;
    function GetOfficeStatusBar: Boolean;
    function GetFloatingWindowBorderSize: Integer;
    procedure SetFloatingWindowBorderSize(const Value: Integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure BroadcastChanges;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure CopyOptions(AComponent, ToComponent: TSpTBXSkinComponentsType);
    procedure FillOptions; virtual;
    function Options(Component: TSpTBXSkinComponentsType; State: TSpTBXSkinStatesType): TSpTBXSkinOptionCategory; overload;
    function Options(Component: TSpTBXSkinComponentsType): TSpTBXSkinOptionCategory; overload;
    procedure LoadFromFile(Filename: string);
    procedure LoadFromStrings(L: TStrings); virtual;
    procedure SaveToFile(Filename: string);
    procedure SaveToStrings(L: TStrings); virtual;
    procedure SaveToMemIni(MemIni: TMemIniFile); virtual;
    procedure Reset(ForceResetSkinProperties: Boolean = False);

    // Metrics
    procedure GetDropDownArrowSize(out DropDownArrowSize, DropDownArrowMargin, SplitBtnArrowSize: Integer); virtual;
    procedure GetMenuItemMargins(ACanvas: TCanvas; ImgSize: Integer; out MarginsInfo: TSpTBXMenuItemMarginsInfo); virtual;
    function GetState(Enabled, Pushed, HotTrack, Checked: Boolean): TSpTBXSkinStatesType; overload;
    procedure GetState(State: TSpTBXSkinStatesType; out Enabled, Pushed, HotTrack, Checked: Boolean); overload;
    function GetTextColor(Component: TSpTBXSkinComponentsType; State: TSpTBXSkinStatesType): TColor; virtual;
    function GetThemedElementDetails(Component: TSpTBXSkinComponentsType; Enabled, Pushed, HotTrack, Checked, Focused, Defaulted, Grayed: Boolean; out Details: TThemedElementDetails): Boolean; overload;
    function GetThemedElementDetails(Component: TSpTBXSkinComponentsType; State: TSpTBXSkinStatesType; out Details: TThemedElementDetails): Boolean; overload;
    function GetThemedElementSize(ACanvas: TCanvas; Details: TThemedElementDetails): TSize;
    procedure GetThemedElementTextColor(Details: TThemedElementDetails; out AColor: TColor);
    function GetThemedSystemColor(AColor: TColor): TColor;

    // Skin Paint
    procedure PaintBackground(ACanvas: TCanvas; ARect: TRect; Component: TSpTBXSkinComponentsType; State: TSpTBXSkinStatesType; Background, Borders: Boolean; Vertical: Boolean = False; ForceRectBorders: TAnchors = []); virtual;
    procedure PaintThemedElementBackground(ACanvas: TCanvas; ARect: TRect; Details: TThemedElementDetails); overload;
    procedure PaintThemedElementBackground(ACanvas: TCanvas; ARect: TRect; Component: TSpTBXSkinComponentsType; State: TSpTBXSkinStatesType); overload;
    procedure PaintThemedElementBackground(ACanvas: TCanvas; ARect: TRect; Component: TSpTBXSkinComponentsType; Enabled, Pushed, HotTrack, Checked, Focused, Defaulted, Grayed: Boolean); overload;

    // Element Paint
    procedure PaintMenuCheckMark(ACanvas: TCanvas; ARect: TRect; Checked, Grayed, MenuItemStyle: Boolean; State: TSpTBXSkinStatesType); virtual;
    procedure PaintMenuRadioMark(ACanvas: TCanvas; ARect: TRect; Checked, MenuItemStyle: Boolean; State: TSpTBXSkinStatesType); virtual;
    procedure PaintWindowFrame(ACanvas: TCanvas; ARect: TRect; IsActive, DrawBody: Boolean; BorderSize: Integer = 4); virtual;

    // Properties
    property ColorBtnFace: TColor read FColorBtnFace write FColorBtnFace;
    property FloatingWindowBorderSize: Integer read GetFloatingWindowBorderSize write SetFloatingWindowBorderSize;
    property OfficeIcons: Boolean read GetOfficeIcons write FOfficeIcons;
    property OfficeMenu: Boolean read GetOfficeMenu write FOfficeMenu;
    property OfficePopup: Boolean read GetOfficePopup;
    property OfficeStatusBar: Boolean read GetOfficeStatusBar write FOfficeStatusBar;
    property SkinAuthor: string read FSkinAuthor write FSkinAuthor;
    property SkinName: string read FSkinName write FSkinName;
  end;

  TSpTBXSkinOptionsClass = class of TSpTBXSkinOptions;

  { TSpTBXSkinsList }

  TSpTBXSkinsListEntry = class
  public
    SkinClass: TSpTBXSkinOptionsClass;
    SkinStrings: TStringList;
    destructor Destroy; override;
  end;

  TSpTBXSkinsList = class(TStringList)
  private
    function GetSkinOption(Index: Integer): TSpTBXSkinsListEntry;
  public
    procedure Delete(Index: Integer); override;
    destructor Destroy; override;
    function AddSkin(SkinName: string; SkinClass: TSpTBXSkinOptionsClass): Integer; overload;
    function AddSkin(SkinOptions: TStrings): Integer; overload;
    function AddSkinFromFile(Filename: string): Integer;
    procedure AddSkinsFromFolder(Folder: string);
    procedure GetSkinNames(SkinNames: TStrings);
    property SkinOptions[Index: Integer]: TSpTBXSkinsListEntry read GetSkinOption;
  end;

  { TSpTBXSkinManager }

  TSpTBXSkinManager = class
  private
    FCurrentSkin: TSpTBXSkinOptions;
    FNotifies: TList;
    FSkinsList: TSpTBXSkinsList;
    FOnSkinChange: TNotifyEvent;
    procedure Broadcast;
    procedure ResetDelphiStyle;
    function GetCurrentSkinName: string;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function GetSkinType: TSpTBXSkinType;
    function IsDefaultSkin: Boolean;
    function IsXPThemesEnabled: Boolean;

    procedure AddSkinNotification(AObject: TObject);
    procedure RemoveSkinNotification(AObject: TObject);
    procedure BroadcastSkinNotification;

    procedure LoadFromFile(Filename: string);
    procedure SaveToFile(Filename: string);

    procedure SetToDefaultSkin;
    procedure SetSkin(SkinName: string);

    property CurrentSkin: TSpTBXSkinOptions read FCurrentSkin;
    property CurrentSkinName: string read GetCurrentSkinName;
    property SkinsList: TSpTBXSkinsList read FSkinsList;
    property OnSkinChange: TNotifyEvent read FOnSkinChange write FOnSkinChange;
  end;

  { TSpTBXSkinSwitcher }

  TSpTBXSkinSwitcher = class(TComponent)
  private
    FOnSkinChange: TNotifyEvent;
    function GetSkin: string;
    procedure SetSkin(const Value: string);
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Skin: string read GetSkin write SetSkin;
    property OnSkinChange: TNotifyEvent read FOnSkinChange write FOnSkinChange;
  end;

  { TSpTBXThemeServices }
  {$IF CompilerVersion >= 23} //for Delphi XE2 and up
  TSpTBXThemeServices = TCustomStyleServices;
  {$ELSE}
  TSpTBXThemeServices = TThemeServices;
  {$IFEND}

  { TSpPrintWindow }
  // Use SpPrintWindow instead of PaintTo as many controls will not render
  // properly (no text on editors, no scrollbars, incorrect borders, etc)
  // http://msdn2.microsoft.com/en-us/library/ms535695.aspx
  TSpPrintWindow = function(Hnd: HWND; HdcBlt: HDC; nFlags: UINT): BOOL; stdcall;

{ Themes }
function SpTBXThemeServices: TSpTBXThemeServices;
function SkinManager: TSpTBXSkinManager;
function CurrentSkin: TSpTBXSkinOptions;
function SpGetLunaScheme: TSpTBXLunaScheme;
procedure SpFillGlassRect(ACanvas: TCanvas; ARect: TRect);
function SpIsGlassPainting(AControl: TControl): Boolean;
procedure SpDrawParentBackground(Control: TControl; DC: HDC; R: TRect);

{ WideString helpers }
function SpCreateRotatedFont(DC: HDC; Orientation: Integer = 2700): HFONT;
function SpDrawRotatedText(const DC: HDC; AText: string; var ARect: TRect; const AFormat: Cardinal; RotationAngle: TSpTextRotationAngle = tra270): Integer;
function SpCalcXPText(ACanvas: TCanvas; ARect: TRect; Caption: string; CaptionAlignment: TAlignment; Flags: Cardinal; GlyphSize, RightGlyphSize: TSize; Layout: TSpGlyphLayout; PushedCaption: Boolean; out ACaptionRect, AGlyphRect, ARightGlyphRect: TRect; RotationAngle: TSpTextRotationAngle = tra0): Integer;
function SpDrawXPGlassText(ACanvas: TCanvas; Caption: string; var ARect: TRect; Flags: Cardinal; CaptionGlowSize: Integer): Integer;
function SpDrawXPText(ACanvas: TCanvas; Caption: string; var ARect: TRect; Flags: Cardinal; CaptionGlow: TSpGlowDirection = gldNone; CaptionGlowColor: TColor = clYellow; RotationAngle: TSpTextRotationAngle = tra0): Integer; overload;
function SpDrawXPText(ACanvas: TCanvas; ARect: TRect; Caption: string; CaptionGlow: TSpGlowDirection; CaptionGlowColor: TColor; CaptionAlignment: TAlignment; Flags: Cardinal; GlyphSize: TSize; Layout: TSpGlyphLayout; PushedCaption: Boolean; out ACaptionRect, AGlyphRect: TRect; RotationAngle: TSpTextRotationAngle = tra0): Integer; overload;
function SpDrawXPText(ACanvas: TCanvas; ARect: TRect; Caption: string; CaptionGlow: TSpGlowDirection; CaptionGlowColor: TColor; CaptionAlignment: TAlignment; Flags: Cardinal; IL: TCustomImageList; ImageIndex: Integer; Layout: TSpGlyphLayout; Enabled, PushedCaption, DisabledIconCorrection: Boolean; out ACaptionRect, AGlyphRect: TRect; RotationAngle: TSpTextRotationAngle = tra0): Integer; overload;
function SpGetTextSize(DC: HDC; S: string; NoPrefix: Boolean): TSize;
function SpGetControlTextHeight(AControl: TControl; AFont: TFont): Integer;
function SpGetControlTextSize(AControl: TControl; AFont: TFont; S: string): TSize;
function SpStripAccelChars(S: string): string;
function SpStripShortcut(S: string): string;
function SpStripTrailingPunctuation(S: string): string;
function SpRectToString(R: TRect): string;
function SpStringToRect(S: string; out R: TRect): Boolean;

{ Color helpers }
function SpColorToHTML(const Color: TColor): string;
function SpColorToString(const Color: TColor; TextType: TSpTBXColorTextType = cttDefault): string;
function SpStringToColor(S: string; out Color: TColor): Boolean;
procedure SpGetRGB(Color: TColor; out R, G, B: Integer);
function SpRGBToColor(R, G, B: Integer): TColor;
function SpLighten(Color: TColor; Amount: Integer): TColor;
function SpBlendColors(TargetColor, BaseColor: TColor; Percent: Integer): TColor;
function SpMixColors(TargetColor, BaseColor: TColor; Amount: Byte): TColor;

{ Painting helpers }
function SpCenterRect(Parent: TRect; ChildWidth, ChildHeight: Integer): TRect; overload;
function SpCenterRect(Parent, Child: TRect): TRect; overload;
function SpCenterRectHoriz(Parent: TRect; ChildWidth: Integer): TRect;
function SpCenterRectVert(Parent: TRect; ChildHeight: Integer): TRect;
procedure SpFillRect(ACanvas: TCanvas; const ARect: TRect; BrushColor: TColor; PenColor: TColor = clNone);
procedure SpDrawLine(ACanvas: TCanvas; X1, Y1, X2, Y2: Integer; Color: TColor);
procedure SpDrawRectangle(ACanvas: TCanvas; ARect: TRect; CornerSize: Integer; ColorTL, ColorBR: TColor; ColorTLInternal: TColor = clNone; ColorBRInternal: TColor = clNone; ForceRectBorders: TAnchors = []); overload;
procedure SpDrawRectangle(ACanvas: TCanvas; ARect: TRect; CornerSize: Integer; ColorL, ColorT, ColorR, ColorB, InternalColorL, InternalColorT, InternalColorR, InternalColorB: TColor; ForceRectBorders: TAnchors = []); overload;
procedure SpAlphaBlend(SrcDC, DstDC: HDC; SrcR, DstR: TRect; Alpha: Byte; SrcHasAlphaChannel: Boolean = False);
procedure SpPaintTo(WinControl: TWinControl; ACanvas: TCanvas; X, Y: Integer);

{ ImageList painting }
procedure SpDrawIconShadow(ACanvas: TCanvas; const ARect: TRect; ImageList: TCustomImageList; ImageIndex: Integer);
procedure SpDrawImageList(ACanvas: TCanvas; const ARect: TRect; ImageList: TCustomImageList; ImageIndex: Integer; Enabled, DisabledIconCorrection: Boolean);

{ Gradients }
procedure SpGradient(ACanvas: TCanvas; const ARect: TRect; StartPos, EndPos, ChunkSize: Integer; C1, C2: TColor; const Vertical: Boolean);
procedure SpGradientFill(ACanvas: TCanvas; const ARect: TRect; const C1, C2: TColor; const Vertical: Boolean);
procedure SpGradientFillMirror(ACanvas: TCanvas; const ARect: TRect; const C1, C2, C3, C4: TColor; const Vertical: Boolean);
procedure SpGradientFillMirrorTop(ACanvas: TCanvas; const ARect: TRect; const C1, C2, C3, C4: TColor; const Vertical: Boolean);
procedure SpGradientFillGlass(ACanvas: TCanvas; const ARect: TRect; const C1, C2, C3, C4: TColor; const Vertical: Boolean);

{ Element painting }
procedure SpDrawArrow(ACanvas: TCanvas; X, Y: Integer; AColor: TColor; Vertical, Reverse: Boolean; Size: Integer);
procedure SpDrawDropMark(ACanvas: TCanvas; DropMark: TRect);
procedure SpDrawFocusRect(ACanvas: TCanvas; const ARect: TRect);
procedure SpDrawGlyphPattern(DC: HDC; const R: TRect; Width, Height: Integer; const PatternBits; PatternColor: TColor); overload;
procedure SpDrawGlyphPattern(ACanvas: TCanvas; ARect: TRect; PatternIndex: Integer; PatternColor: TColor); overload;
procedure SpDrawXPButton(ACanvas: TCanvas; ARect: TRect; Enabled, Pushed, HotTrack, Checked, Focused, Defaulted: Boolean);
procedure SpDrawXPCheckBoxGlyph(ACanvas: TCanvas; ARect: TRect; Enabled: Boolean; State: TCheckBoxState; HotTrack, Pushed: Boolean);
procedure SpDrawXPRadioButtonGlyph(ACanvas: TCanvas; ARect: TRect; Enabled, Checked, HotTrack, Pushed: Boolean);
procedure SpDrawXPEditFrame(ACanvas: TCanvas; ARect: TRect; Enabled, HotTrack: Boolean; ClipContent: Boolean = False; AutoAdjust: Boolean = False); overload;
procedure SpDrawXPEditFrame(AWinControl: TWinControl; HotTracking: Boolean; AutoAdjust: Boolean = False; HideFrame: Boolean = False); overload;
procedure SpDrawXPGrip(ACanvas: TCanvas; ARect: TRect; LoC, HiC: TColor);
procedure SpDrawXPHeader(ACanvas: TCanvas; ARect: TRect; HotTrack, Pushed: Boolean);
procedure SpDrawXPListItemBackground(ACanvas: TCanvas; ARect: TRect; Selected, HotTrack, Focused: Boolean; ForceRectBorders: Boolean = False; Borders: Boolean = True);

{ Skins painting }
procedure SpPaintSkinBackground(ACanvas: TCanvas; ARect: TRect; SkinOption: TSpTBXSkinOptionCategory; Vertical: Boolean);
procedure SpPaintSkinBorders(ACanvas: TCanvas; ARect: TRect; SkinOption: TSpTBXSkinOptionCategory; ForceRectBorders: TAnchors = []);

{ Misc }
function SpIsWinVistaOrUp: Boolean;
function SpGetDirectories(Path: string; L: TStringList): Boolean;

{ Stock Objects }
var
  StockBitmap: TBitmap;
  SpPrintWindow: TSpPrintWindow = nil;

implementation

uses
  UxTheme, Forms, Math, TypInfo,
  SpTBXDefaultSkins;

const
  ROP_DSPDxax = $00E20746;

type
  TControlAccess = class(TControl);

var
  FInternalSkinManager: TSpTBXSkinManager = nil;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Themes }

function SpTBXThemeServices: TSpTBXThemeServices;
begin
  {$IF CompilerVersion >= 23} //for Delphi XE2 and up
  Result := StyleServices;
  {$ELSE}
  Result := ThemeServices;
  {$IFEND}
end;

function SkinManager: TSpTBXSkinManager;
begin
  if not Assigned(FInternalSkinManager) then
    FInternalSkinManager := TSpTBXSkinManager.Create;
  Result := FInternalSkinManager;
end;

function CurrentSkin: TSpTBXSkinOptions;
begin
  Result := SkinManager.CurrentSkin;
end;

function SpGetLunaScheme: TSpTBXLunaScheme;
const
  MaxChars = 1024;
var
  pszThemeFileName, pszColorBuff, pszSizeBuf: PWideChar;
  S: string;
begin
  Result := lusUnknown;

  if SkinManager.IsXPThemesEnabled then begin
    GetMem(pszThemeFileName, 2 * MaxChars);
    GetMem(pszColorBuff,     2 * MaxChars);
    GetMem(pszSizeBuf,       2 * MaxChars);
    try
      if not Failed(GetCurrentThemeName(pszThemeFileName, MaxChars, pszColorBuff, MaxChars, pszSizeBuf, MaxChars)) then
        if UpperCase(ExtractFileName(pszThemeFileName)) = 'LUNA.MSSTYLES' then begin
          S := UpperCase(pszColorBuff);
          if S = 'NORMALCOLOR' then
            Result := lusBlue
          else if S = 'METALLIC' then
            Result := lusMetallic
          else if S = 'HOMESTEAD' then
            Result := lusGreen;
        end;
    finally
      FreeMem(pszSizeBuf);
      FreeMem(pszColorBuff);
      FreeMem(pszThemeFileName);
    end;
  end;
end;

procedure SpFillGlassRect(ACanvas: TCanvas; ARect: TRect);
var
  MemDC: HDC;
  PaintBuffer: HPAINTBUFFER;
begin
  PaintBuffer := BeginBufferedPaint(ACanvas.Handle, ARect, BPBF_TOPDOWNDIB, nil, MemDC);
  try
    FillRect(MemDC, ARect, ACanvas.Brush.Handle);
    BufferedPaintMakeOpaque(PaintBuffer, @ARect);
  finally
    EndBufferedPaint(PaintBuffer, True);
  end;
end;

function SpIsGlassPainting(AControl: TControl): Boolean;
var
  LParent: TWinControl;
begin
  Result := csGlassPaint in AControl.ControlState;
  if Result then begin
    LParent := AControl.Parent;
    while (LParent <> nil) and not LParent.DoubleBuffered do
      LParent := LParent.Parent;
    Result := (LParent = nil) or not LParent.DoubleBuffered or (LParent is TCustomForm);
  end;
end;

procedure SpDrawParentBackground(Control: TControl; DC: HDC; R: TRect);
// Delphi 2007 and Vista compatible
var
  Parent: TWinControl;
  P: TPoint;
  Brush: HBRUSH;
begin
  Parent := Control.Parent;
  if Parent = nil then begin
    Brush := CreateSolidBrush(ColorToRGB(clBtnFace));
    Windows.FillRect(DC, R, Brush);
    DeleteObject(Brush);
  end
  else
    if Parent.HandleAllocated then begin
      if not Parent.DoubleBuffered and (Control is TWinControl) and SkinManager.IsXPThemesEnabled then
        UxTheme.DrawThemeParentBackground(TWinControl(Control).Handle, DC, @R)
      else begin
        // Same as Controls.PerformEraseBackground
        GetWindowOrgEx(DC, P);
        SetWindowOrgEx(DC, P.X + Control.Left, P.Y + Control.Top, nil);
        Parent.Perform(WM_ERASEBKGND, WPARAM(DC), LPARAM(DC));
        SetWindowOrgEx(DC, P.X, P.Y, nil);
      end;
    end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ WideString helpers}

function EnumFontsProc(const lplf: TLogFont; const lptm: TTextMetric;
  dwType: DWORD; lpData: LPARAM): Integer; stdcall;
begin
  Boolean(Pointer(lpData)^) := True;
  Result := 0;
end;

function SpCreateRotatedFont(DC: HDC; Orientation: Integer = 2700): HFONT;
var
  LogFont: TLogFont;
  TM: TTextMetric;
  VerticalFontName: array[0..LF_FACESIZE-1] of Char;
  VerticalFontExists: Boolean;
begin
  if GetObject(GetCurrentObject(DC, OBJ_FONT), SizeOf(LogFont),
     @LogFont) = 0 then begin
    { just in case... }
    Result := 0;
    Exit;
  end;
  LogFont.lfEscapement := Orientation;
  LogFont.lfOrientation := Orientation;
  LogFont.lfOutPrecision := OUT_TT_ONLY_PRECIS;  { needed for Win9x }

  { Don't let a random TrueType font be substituted when MS Sans Serif or
    Microsoft Sans Serif are used. On Windows 2000 and later, hard-code Tahoma
    because Arial can't display Japanese or Thai Unicode characters (on Windows
    2000 at least). On earlier versions, hard-code Arial since NT 4.0 doesn't
    ship with Tahoma, and 9x doesn't do Unicode. }
  if (StrIComp(LogFont.lfFaceName, 'MS Sans Serif') = 0) or
     (StrIComp(LogFont.lfFaceName, 'Microsoft Sans Serif') = 0) then begin
    if Win32MajorVersion >= 5 then
      StrPCopy(LogFont.lfFaceName, 'Tahoma')
    else
      StrPCopy(LogFont.lfFaceName, 'Arial');
    { Set lfHeight to the actual height of the current font. This is needed
      to work around a Windows 98 issue: on a clean install of the OS,
      SPI_GETNONCLIENTMETRICS returns -5 for lfSmCaptionFont.lfHeight. This is
      wrong; it should return -11 for an 8 pt font. With normal, unrotated text
      this actually displays correctly, since MS Sans Serif doesn't support
      sizes below 8 pt. However, when we change to a TrueType font like Arial,
      this becomes a problem because it'll actually create a font that small. }
    if GetTextMetrics(DC, TM) then begin
      { If the original height was negative, keep it negative }
      if LogFont.lfHeight <= 0 then
        LogFont.lfHeight := -(TM.tmHeight - TM.tmInternalLeading)
      else
        LogFont.lfHeight := TM.tmHeight;
    end;
  end;

  { Use a vertical font if available so that Asian characters aren't drawn
    sideways }
  if StrLen(LogFont.lfFaceName) < SizeOf(VerticalFontName)-1 then begin
    VerticalFontName[0] := '@';
    StrCopy(@VerticalFontName[1], LogFont.lfFaceName);
    VerticalFontExists := False;
    EnumFonts(DC, VerticalFontName, @EnumFontsProc, @VerticalFontExists);
    if VerticalFontExists then
      StrCopy(LogFont.lfFaceName, VerticalFontName);
  end;

  Result := CreateFontIndirect(LogFont);
end;

function SpDrawRotatedText(const DC: HDC; AText: string; var ARect: TRect; const AFormat: Cardinal; RotationAngle: TSpTextRotationAngle = tra270): Integer;
{ The format flag this function respects are
  DT_CALCRECT, DT_NOPREFIX, DT_HIDEPREFIX, DT_CENTER, DT_END_ELLIPSIS, DT_NOCLIP }
var
  RotatedFont, SaveFont: HFONT;
  TextMetrics: TTextMetric;
  X, Y, P, I, SU, FU, W: Integer;
  SaveAlign: UINT;
  Clip: Boolean;
  Pen, SavePen: HPEN;
  Sz: TSize;
  Orientation: Integer;
begin
  Result := 0;
  if Length(AText) = 0 then Exit;

  Orientation := 0;
  case RotationAngle of
    tra90: Orientation := 900;   // 90 degrees
    tra270: Orientation := 2700; // 270 degrees
  end;
  RotatedFont := SpCreateRotatedFont(DC, Orientation);
  SaveFont := SelectObject(DC, RotatedFont);

  GetTextMetrics(DC, TextMetrics);
  X := ARect.Left + (ARect.Right - ARect.Left - TextMetrics.tmHeight) div 2;

  Clip := AFormat and DT_NOCLIP = 0;

  { Find the index of the character that should be underlined. Delete '&'
    characters from the string. Like DrawText, only the last prefixed character
    will be underlined. }
  P := 0;
  I := 1;
  if AFormat and DT_NOPREFIX = 0 then
    while I <= Length(AText) do
    begin
      if AText[I] = '&' then
      begin
        Delete(AText, I, 1);
        if PWideChar(AText)[I - 1] <> '&' then P := I;
      end;
      Inc(I);
    end;

  if AFormat and DT_END_ELLIPSIS <> 0 then
  begin
    if (Length(AText) > 1) and (SpGetTextSize(DC, AText, False).cx > ARect.Bottom - ARect.Top) then
    begin
      W := ARect.Bottom - ARect.Top;
      if W > 2 then
      begin
        Delete(AText, Length(AText), 1);
        while (Length(AText) > 1) and (SpGetTextSize(DC, AText + '...', False).cx > W) do
          Delete(AText, Length(AText), 1);
      end
      else AText := AText[1];
      if P > Length(AText) then P := 0;
      AText := AText + '...';
    end;
  end;

  Sz := SpGetTextSize(DC, AText, False);
  Result := Sz.cy;

  if AFormat and DT_CALCRECT <> 0 then begin
    ARect.Right := ARect.Left + Sz.cy;
    ARect.Bottom := ARect.Top + Sz.cx;
  end
  else begin
    if AFormat and DT_CENTER <> 0 then
      Y := ARect.Top + (ARect.Bottom - ARect.Top - Sz.cx) div 2
    else
      Y := ARect.Top;

    if Clip then
    begin
      SaveDC(DC);
      with ARect do IntersectClipRect(DC, Left, Top, Right, Bottom);
    end;

    case RotationAngle of
      tra90: SaveAlign := SetTextAlign(DC, TA_RIGHT);
      tra270: SaveAlign := SetTextAlign(DC, TA_BOTTOM);
    else
      SaveAlign := SetTextAlign(DC, TA_LEFT);
    end;

    Windows.TextOut(DC, X, Y, PWideChar(AText), Length(AText));
    SetTextAlign(DC, SaveAlign);

    { Underline }
    if (P > 0) and (AFormat and DT_HIDEPREFIX = 0) then
    begin
      SU := SpGetTextSize(DC, Copy(AText, 1, P - 1), False).cx;
      FU := SU + SpGetTextSize(DC, PWideChar(AText)[P - 1], False).cx;
      Inc(X, TextMetrics.tmDescent - 2);
      Pen := CreatePen(PS_SOLID, 1, GetTextColor(DC));
      SavePen := SelectObject(DC, Pen);
      MoveToEx(DC, X, Y + SU, nil);
      LineTo(DC, X, Y + FU);
      SelectObject(DC, SavePen);
      DeleteObject(Pen);
    end;

    if Clip then RestoreDC(DC, -1);
  end;

  SelectObject(DC, SaveFont);
  DeleteObject(RotatedFont);
end;

function SpCalcXPText(ACanvas: TCanvas; ARect: TRect; Caption: string;
  CaptionAlignment: TAlignment; Flags: Cardinal; GlyphSize, RightGlyphSize: TSize;
  Layout: TSpGlyphLayout; PushedCaption: Boolean; out ACaptionRect, AGlyphRect, ARightGlyphRect: TRect;
  RotationAngle: TSpTextRotationAngle = tra0): Integer;
var
  R: TRect;
  TextOffset, Spacing, RightSpacing: TPoint;
  CaptionSz: TSize;
begin
  Result := 0;
  ACaptionRect := Rect(0, 0, 0, 0);
  AGlyphRect := Rect(0, 0, 0, 0);
  ARightGlyphRect := Rect(0, 0, 0, 0);
  TextOffset := Point(0, 0);
  Spacing := Point(0, 0);
  RightSpacing := Point(0, 0);
  if (Caption <> '') and (GlyphSize.cx > 0) and (GlyphSize.cy > 0) then
    Spacing := Point(4, 1);
  if (Caption <> '') and (RightGlyphSize.cx > 0) and (RightGlyphSize.cy > 0) then
    RightSpacing := Point(4, 1);

  Flags := Flags and not DT_CENTER;
  Flags := Flags and not DT_VCENTER;
  if CaptionAlignment = taRightJustify then
    Flags := Flags or DT_RIGHT;

  // DT_END_ELLIPSIS and DT_PATH_ELLIPSIS doesn't work with rotated text
  // http://support.microsoft.com/kb/249678
  // Revert the ARect if the text is rotated, from now on work on horizontal text !!!
  if RotationAngle <> tra0 then
    ARect := Rect(ARect.Top, ARect.Left, ARect.Bottom, ARect.Right);

  // Get the caption size
  if ((Flags and DT_WORDBREAK) <> 0) or ((Flags and DT_END_ELLIPSIS) <> 0) or ((Flags and DT_PATH_ELLIPSIS) <> 0) then begin
    if Layout = ghlGlyphLeft then  // Glyph on left or right side
      R := Rect(0, 0, ARect.Right - ARect.Left - GlyphSize.cx - Spacing.X - RightGlyphSize.cx - RightSpacing.X + 2, 1)
    else  // Glyph on top
      R := Rect(0, 0, ARect.Right - ARect.Left + 2, 1);
  end
  else
    R := Rect(0, 0, 1, 1);

  if (fsBold in ACanvas.Font.Style) and (RotationAngle = tra0) and (((Flags and DT_END_ELLIPSIS) <> 0) or ((Flags and DT_PATH_ELLIPSIS) <> 0)) then begin
    // [Bugfix] Windows bug:
    // When the Font is Bold and DT_END_ELLIPSIS or DT_PATH_ELLIPSIS is used
    // DrawTextW returns an incorrect size if the string is unicode.
    // The R.Right is reduced by 3 which cuts down the string and
    // adds the ellipsis.
    // We have to obtain the real size and check if it fits in the Rect.
    CaptionSz := SpGetTextSize(ACanvas.Handle, Caption, True);
    if CaptionSz.cx <= R.Right then begin
      R := Rect(0, 0, CaptionSz.cx, CaptionSz.cy);
      Result := CaptionSz.cy;
    end;
  end;

  if Result <= 0 then begin
    Result := SpDrawXPText(ACanvas, Caption, R, Flags or DT_CALCRECT, gldNone, clYellow);
    CaptionSz.cx := R.Right;
    CaptionSz.cy := R.Bottom;
  end;

  // ACaptionRect
  if Result > 0 then begin
    R.Top := ARect.Top + (ARect.Bottom - ARect.Top - CaptionSz.cy) div 2; // Vertically centered
    R.Bottom := R.Top + CaptionSz.cy;
    case CaptionAlignment of
      taCenter:
        R.Left := ARect.Left + (ARect.Right - ARect.Left - CaptionSz.cx) div 2; // Horizontally centered
      taLeftJustify:
        R.Left := ARect.Left;
      taRightJustify:
        R.Left := ARect.Right - CaptionSz.cx;
    end;
    R.Right := R.Left + CaptionSz.cx;

    // Since DT_END_ELLIPSIS and DT_PATH_ELLIPSIS doesn't work with rotated text
    // try to fix it by padding the text 8 pixels to the right
    if (RotationAngle <> tra0) and (R.Right + 8 < ARect.Right) then
      if ((Flags and DT_END_ELLIPSIS) <> 0) or ((Flags and DT_PATH_ELLIPSIS) <> 0) then
        R.Right := R.Right + 8;

    if PushedCaption then
      OffsetRect(R, 1, 1);

    ACaptionRect := R;
  end;

  // AGlyphRect
  if (GlyphSize.cx > 0) and (GlyphSize.cy > 0) then begin
    R := ARect;

    // If ghlGlyphTop is used the glyph should be centered
    if Layout = ghlGlyphTop then
      CaptionAlignment := taCenter;

    case CaptionAlignment of
      taCenter:
        begin
          // Total width = Icon + Space + Text
          if Layout = ghlGlyphLeft then begin
            AGlyphRect.Left := R.Left + (R.Right - R.Left - (GlyphSize.cx + Spacing.X + CaptionSz.cx)) div 2;
            TextOffset.X := (GlyphSize.cx + Spacing.X) div 2;
          end
          else
            AGlyphRect.Left := R.Left + (R.Right - R.Left - GlyphSize.cx) div 2;
        end;
      taLeftJustify:
        begin
          AGlyphRect.Left := R.Left;
          TextOffset.X := GlyphSize.cx + Spacing.X;
        end;
      taRightJustify:
        begin
          AGlyphRect.Left := R.Right - GlyphSize.cx;
          TextOffset.X := - Spacing.X - GlyphSize.cx;
        end;
    end;

    if Layout = ghlGlyphLeft then
      AGlyphRect.Top := R.Top + (R.Bottom - R.Top - GlyphSize.cy) div 2
    else begin
      AGlyphRect.Top := R.Top + (R.Bottom - R.Top - (GlyphSize.cy + Spacing.Y + CaptionSz.cy)) div 2;
      Inc(TextOffset.Y, (GlyphSize.cy + Spacing.Y) div 2);
    end;

    AGlyphRect.Right := AGlyphRect.Left + GlyphSize.cx;
    AGlyphRect.Bottom := AGlyphRect.Top + GlyphSize.cy;

    if PushedCaption then
      OffsetRect(AGlyphRect, 1, 1);
  end;

  // Move the text according to the icon position
  if Result > 0 then
    OffsetRect(ACaptionRect, TextOffset.X, TextOffset.Y);

  // ARightGlyphRect, it's valid only when using taLeftJustify
  if (RightGlyphSize.cx > 0) and (RightGlyphSize.cy > 0) then
    if CaptionAlignment = taLeftJustify then begin
      R := ARect;
      ARightGlyphRect.Left := R.Right - RightGlyphSize.cx;
      ARightGlyphRect.Right := ARightGlyphRect.Left + RightGlyphSize.cx;
      ARightGlyphRect.Top := R.Top + (R.Bottom - R.Top - RightGlyphSize.cy) div 2;
      ARightGlyphRect.Bottom := ARightGlyphRect.Top + RightGlyphSize.cy;
      if (Result > 0) and (ACaptionRect.Right > ARightGlyphRect.Left - RightSpacing.X) then
        ACaptionRect.Right := ARightGlyphRect.Left - RightSpacing.X;
    end;

  // Revert back, normalize when the text is rotated
  if RotationAngle <> tra0 then begin
    ACaptionRect := Rect(ACaptionRect.Top, ACaptionRect.Left, ACaptionRect.Bottom, ACaptionRect.Right);
    AGlyphRect := Rect(AGlyphRect.Top, AGlyphRect.Left, AGlyphRect.Bottom, AGlyphRect.Right);
    ARightGlyphRect := Rect(ARightGlyphRect.Top, ARightGlyphRect.Left, ARightGlyphRect.Bottom, ARightGlyphRect.Right);
  end;
end;

function SpDrawXPGlassText(ACanvas: TCanvas; Caption: string; var ARect: TRect;
  Flags: Cardinal; CaptionGlowSize: Integer): Integer;

  function InternalDraw(C: TCanvas; var R: TRect): Integer;
  var
    Options: TDTTOpts;
  begin
    FillChar(Options, SizeOf(Options), 0);
    Options.dwSize := SizeOf(Options);
    Options.dwFlags := DTT_TEXTCOLOR or DTT_COMPOSITED or DTT_GLOWSIZE;
    if Flags and DT_CALCRECT = DT_CALCRECT then
      Options.dwFlags := Options.dwFlags or DTT_CALCRECT;
    Options.crText := ColorToRGB(C.Font.Color);
    Options.iGlowSize := CaptionGlowSize;
    DrawThemeTextEx(SpTBXThemeServices.Theme[teWindow], C.Handle, WP_CAPTION, CS_ACTIVE,
      PWideChar(WideString(Caption)), Length(Caption), Flags, @R, Options);


    Result := R.Bottom - R.Top;
  end;

var
  B: TBitmap;
  RBitmap: TRect;
begin
  if Flags and DT_CALCRECT = 0 then begin
    B := TBitmap.Create;
    try
      RBitmap := Rect(0, 0, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
      B.PixelFormat := pf32bit;
      // Negative Height because DrawThemeTextEx uses top-down DIBs when DTT_COMPOSITED is used
      // If biHeight is positive, the bitmap is a bottom-up DIB and its origin is the lower left corner.
      // If biHeight is negative, the bitmap is a top-down DIB and its origin is the upper left corner.
      B.SetSize(RBitmap.Right, -RBitmap.Bottom);
      B.Canvas.Font.Assign(ACanvas.Font);
      B.Canvas.Brush.Color := clBlack;  // Fill with black to make it transparent on Glass
      B.Canvas.FillRect(RBitmap);

      Result := InternalDraw(B.Canvas, RBitmap);
      SpAlphaBlend(B.Canvas.Handle, ACanvas.Handle, RBitmap, ARect, 255, True);
    finally
      B.Free;
    end;
  end
  else
    Result := InternalDraw(ACanvas, ARect);
end;

function SpDrawXPText(ACanvas: TCanvas; Caption: string; var ARect: TRect;
  Flags: Cardinal; CaptionGlow: TSpGlowDirection = gldNone;
  CaptionGlowColor: TColor = clYellow; RotationAngle: TSpTextRotationAngle = tra0): Integer; overload;

  function IsCalcRect: Boolean;
  begin
    Result := Flags and DT_CALCRECT = DT_CALCRECT;
  end;

  function InternalDraw(var R: TRect): Integer;
  begin
    Result := 0;
    case RotationAngle of
      tra0:
        Result := Windows.DrawText(ACanvas.Handle, PWideChar(Caption), -1, R, Flags);
      tra90, tra270:
        Result := SpDrawRotatedText(ACanvas.Handle, Caption, R, Flags, RotationAngle);
    end;
  end;

var
  BS: TBrushStyle;
  GlowR: TRect;
  C, FC: TColor;
begin
  BS := ACanvas.Brush.Style;
  C := ACanvas.Brush.Color;
  try
    ACanvas.Brush.Style := bsClear;

    if (CaptionGlow <> gldNone) and not IsCalcRect then begin
      FC := ACanvas.Font.Color;
      ACanvas.Font.Color := CaptionGlowColor;
      case CaptionGlow of
        gldAll:
          begin
            GlowR := ARect; OffsetRect(GlowR, 0, -1);
            InternalDraw(GlowR);
            GlowR := ARect; OffsetRect(GlowR, 0, 1);
            InternalDraw(GlowR);
            GlowR := ARect; OffsetRect(GlowR, -1, 0);
            InternalDraw(GlowR);
            GlowR := ARect; OffsetRect(GlowR, 1, 0);
          end;
        gldTopLeft:
          begin
            GlowR := ARect; OffsetRect(GlowR, -1, -1);
            InternalDraw(GlowR);
          end;
        gldBottomRight:
          begin
            GlowR := ARect; OffsetRect(GlowR, 1, 1);
            InternalDraw(GlowR);
          end;
      end;
      ACanvas.Font.Color := FC;
    end;

    Result := InternalDraw(ARect);

    if IsRectEmpty(ARect) then
      Result := 0
    else
      if IsCalcRect then begin
        // [Bugfix] Windows bug:
        // When DT_CALCRECT is used and the font is italic the
        // resulting rect is incorrect
        if fsItalic in ACanvas.Font.Style then
          ARect.Right := ARect.Right + 1 + (ACanvas.Font.Size div 8) * 2;
      end;

  finally
    ACanvas.Brush.Style := BS;
    ACanvas.Brush.Color := C;
  end;
end;

function SpDrawXPText(ACanvas: TCanvas; ARect: TRect; Caption: string;
  CaptionGlow: TSpGlowDirection; CaptionGlowColor: TColor; CaptionAlignment: TAlignment;
  Flags: Cardinal; GlyphSize: TSize; Layout: TSpGlyphLayout; PushedCaption: Boolean;
  out ACaptionRect, AGlyphRect: TRect;
  RotationAngle: TSpTextRotationAngle = tra0): Integer; overload;
var
  DummyRightGlyphSize: TSize;
  DummyRightGlyphRect: TRect;
begin
  DummyRightGlyphSize.cx := 0;
  DummyRightGlyphSize.cy := 0;
  DummyRightGlyphRect := Rect(0, 0, 0, 0);
  Result := SpCalcXPText(ACanvas, ARect, Caption, CaptionAlignment, Flags, GlyphSize, DummyRightGlyphSize,
    Layout, PushedCaption, ACaptionRect, AGlyphRect, DummyRightGlyphRect, RotationAngle);
  SpDrawXPText(ACanvas, Caption, ACaptionRect, Flags and not DT_CALCRECT, CaptionGlow, CaptionGlowColor, RotationAngle);
end;

function SpDrawXPText(ACanvas: TCanvas; ARect: TRect; Caption: string;
  CaptionGlow: TSpGlowDirection; CaptionGlowColor: TColor; CaptionAlignment: TAlignment;
  Flags: Cardinal; IL: TCustomImageList; ImageIndex: Integer; Layout: TSpGlyphLayout;
  Enabled, PushedCaption, DisabledIconCorrection: Boolean; out ACaptionRect, AGlyphRect: TRect;
  RotationAngle: TSpTextRotationAngle = tra0): Integer; overload;
var
  GlyphSize, DummyRightGlyphSize: TSize;
  DummyRightGlyphRect: TRect;
begin
  GlyphSize.cx := 0;
  GlyphSize.cy := 0;
  DummyRightGlyphSize.cx := 0;
  DummyRightGlyphSize.cy := 0;
  DummyRightGlyphRect := Rect(0, 0, 0, 0);

  if Assigned(IL) and (ImageIndex > -1) and (ImageIndex < IL.Count) then begin
    GlyphSize.cx := IL.Width;
    GlyphSize.cy := IL.Height;
  end;

  Result := SpCalcXPText(ACanvas, ARect, Caption, CaptionAlignment, Flags, GlyphSize, DummyRightGlyphSize,
    Layout, PushedCaption, ACaptionRect, AGlyphRect, DummyRightGlyphRect, RotationAngle);

  SpDrawXPText(ACanvas, Caption, ACaptionRect, Flags and not DT_CALCRECT, CaptionGlow, CaptionGlowColor, RotationAngle);

  if Assigned(IL) and (ImageIndex > -1) and (ImageIndex < IL.Count) then
    SpDrawImageList(ACanvas, AGlyphRect, IL, ImageIndex, Enabled, DisabledIconCorrection);
end;

function SpGetTextSize(DC: HDC; S: string; NoPrefix: Boolean): TSize;
// Returns the size of the string, if NoPrefix is True, it first removes "&"
// characters as necessary.
// This procedure is 10x faster than using DrawText with the DT_CALCRECT flag
begin
  Result.cx := 0;
  Result.cy := 0;
  if NoPrefix then
    S := SpStripAccelChars(S);
  Windows.GetTextExtentPoint32W(DC, S, Length(S), Result);
end;

function SpGetControlTextHeight(AControl: TControl; AFont: TFont): Integer;
// Returns the control text height based on the font
var
  Sz: TSize;
begin
  Sz := SpGetControlTextSize(AControl, AFont, 'WQqJ');
  Result := Sz.cy;
end;

function SpGetControlTextSize(AControl: TControl; AFont: TFont; S: string): TSize;
// Returns the control text size based on the font
var
  ACanvas: TControlCanvas;
begin
  ACanvas := TControlCanvas.Create;
  try
    ACanvas.Control := AControl;
    ACanvas.Font.Assign(AFont);
    Result := SpGetTextSize(ACanvas.Handle, S, False);
  finally
    ACanvas.Free;
  end;
end;

function SpStripAccelChars(S: string): string;
var
  I: Integer;
begin
  Result := S;
  I := 1;
  while I <= Length(Result) do begin
    if (Result[I] = '&') and not ((I + 1 <= Length(Result)) and (Result[I + 1] = '&')) then
      System.Delete(Result, I, 1);  // Don't delete double &&
    Inc(I);
  end;
end;

function SpStripShortcut(S: string): string;
var
  P: Integer;
begin
  Result := S;
  P := Pos(#9, Result);
  if P <> 0 then
    SetLength(Result, P - 1);
end;

function SpStripTrailingPunctuation(S: string): string;
// Removes any colon (':') or ellipsis ('...') from the end of S and returns
// the resulting string
var
  L: Integer;
begin
  Result := S;
  L := Length(Result);
  if (L > 1) and (Result[L] = ':') then
    SetLength(Result, L-1)
  else if (L > 3) and (Result[L-2] = '.') and (Result[L-1] = '.') and
     (Result[L] = '.') then
    SetLength(Result, L-3);
end;

function SpRectToString(R: TRect): string;
begin
  Result := Format('%d, %d, %d, %d', [R.Left, R.Top, R.Right, R.Bottom]);
end;

function SpStringToRect(S: string; out R: TRect): Boolean;
var
  L: TStringList;
begin
  Result := False;
  R := Rect(0, 0, 0, 0);
  L := TStringList.Create;
  try
    L.CommaText := S;
    if L.Count = 4 then begin
      R.Left := StrToIntDef(L[0], 0);
      R.Top := StrToIntDef(L[1], 0);
      R.Right := StrToIntDef(L[2], 0);
      R.Bottom := StrToIntDef(L[3], 0);
      Result := True;
    end;
  finally
    L.Free;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Color Helpers }

function SpColorToHTML(const Color: TColor): string;
var
  R: TColorRef;
begin
  R := ColorToRGB(Color);
  Result := Format('#%.2x%.2x%.2x', [GetRValue(R), GetGValue(R), GetBValue(R)]);
end;

function SpColorToString(const Color: TColor; TextType: TSpTBXColorTextType = cttDefault): string;
begin
  case TextType of
    cttDefault:
      Result := ColorToString(Color);
    cttHTML:
      Result := SpColorToHTML(Color);
    cttIdentAndHTML:
      begin
        Result := ColorToString(Color);
        if (Length(Result) > 0) and (Result[1] = '$') then
          Result := SpColorToHTML(Color);
      end;
  end;
end;

function SpStringToColor(S: string; out Color: TColor): Boolean;
var
  L, C: Integer;
begin
  Result := False;
  Color := clDefault;
  L := Length(S);
  if L < 2 then Exit;

  if (S[1] = '#') and (L = 7) then begin
    Delete(S, 1, 1); // strip the # char
    if TryStrToInt(S, C) then begin
      S := Format('$00%s%s%s', [Copy(S, 5, 2), Copy(S, 3, 2), Copy(S, 1, 2)]);
      Color := StringToColor(S);
      Result := True;
    end;
  end
  else
    if (S[1] = '$') and (L >= 7) and (L <= 9) then begin
      if TryStrToInt(S, C) then begin
        Color := C;
        Result := True;
      end;
    end
    else
      Result := IdentToColor(S, Longint(Color));
end;

procedure SpGetRGB(Color: TColor; out R, G, B: Integer);
begin
  Color := ColorToRGB(Color);
  R := GetRValue(Color);
  G := GetGValue(Color);
  B := GetBValue(Color);
end;

function SpRGBToColor(R, G, B: Integer): TColor;
begin
  if R < 0 then R := 0 else if R > 255 then R := 255;
  if G < 0 then G := 0 else if G > 255 then G := 255;
  if B < 0 then B := 0 else if B > 255 then B := 255;
  Result := TColor(RGB(R, G, B));
end;

function SpLighten(Color: TColor; Amount: Integer): TColor;
var
  R, G, B: Integer;
begin
  Color := ColorToRGB(Color);
  R := GetRValue(Color) + Amount;
  G := GetGValue(Color) + Amount;
  B := GetBValue(Color) + Amount;
  Result := SpRGBToColor(R, G, B);
end;

function SpBlendColors(TargetColor, BaseColor: TColor; Percent: Integer): TColor;
// Blend 2 colors with a predefined percent (0..100 or 0..1000)
// If Percent is 0 the result will be BaseColor,
// If Percent is 100 the result will be TargetColor.
// Any other value will return a color between base and target.
// For example if you want to add 70% of yellow ($0000FFFF) to a color:
// NewColor := SpBlendColor($0000FFFF, BaseColor, 70);
// The result will have 70% of yellow and 30% of BaseColor
var
  Percent2, D, F: Integer;
  R, G, B, R2, G2, B2: Integer;
begin
  SpGetRGB(TargetColor, R, G, B);
  SpGetRGB(BaseColor, R2, G2, B2);

  if Percent >= 100 then D := 1000
  else D := 100;
  Percent2 := D - Percent;
  F := D div 2;

  R := (R * Percent + R2 * Percent2 + F) div D;
  G := (G * Percent + G2 * Percent2 + F) div D;
  B := (B * Percent + B2 * Percent2 + F) div D;

  Result := SpRGBToColor(R, G, B);
end;

function SpMixColors(TargetColor, BaseColor: TColor; Amount: Byte): TColor;
// Mix 2 colors with a predefined amount (0..255).
// If Amount is 0 the result will be BaseColor,
// If Amount is 255 the result will be TargetColor.
// Any other value will return a color between base and target.
// For example if you want to add 50% of yellow ($0000FFFF) to a color:
// NewColor := SpMixColors($0000FFFF, BaseColor, 128);
// The result will be BaseColor + 50% of yellow
var
  R1, G1, B1: Integer;
  R2, G2, B2: Integer;
begin
  SpGetRGB(BaseColor, R1, G1, B1);
  SpGetRGB(TargetColor, R2, G2, B2);

  R1 := (R2 - R1) * Amount div 255 + R1;
  G1 := (G2 - G1) * Amount div 255 + G1;
  B1 := (B2 - B1) * Amount div 255 + B1;

  Result := SpRGBToColor(R1, G1, B1);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Painting Helpers }

function SpCenterRect(Parent: TRect; ChildWidth, ChildHeight: Integer): TRect;
begin
  Result.Left := Parent.Left + (Parent.Right - Parent.Left - ChildWidth) div 2;
  Result.Top := Parent.Top + (Parent.Bottom - Parent.Top - ChildHeight) div 2;
  Result.Right := Result.Left + ChildWidth;
  Result.Bottom := Result.Top + ChildHeight;
end;

function SpCenterRect(Parent, Child: TRect): TRect;
begin
  Result := SpCenterRect(Parent, Child.Right - Child.Left, Child.Bottom - Child.Top);
end;

function SpCenterRectHoriz(Parent: TRect; ChildWidth: Integer): TRect;
begin
  Result.Left := Parent.Left + (Parent.Right - Parent.Left - ChildWidth) div 2;
  Result.Right := Result.Left + ChildWidth;
  Result.Top := Parent.Top;
  Result.Bottom := Parent.Bottom;
end;

function SpCenterRectVert(Parent: TRect; ChildHeight: Integer): TRect;
begin
  Result.Left := Parent.Left;
  Result.Right := Parent.Right;
  Result.Top := Parent.Top + (Parent.Bottom - Parent.Top - ChildHeight) div 2;
  Result.Bottom := Result.Top + ChildHeight;
end;

procedure SpFillRect(ACanvas: TCanvas; const ARect: TRect; BrushColor: TColor; PenColor: TColor = clNone);
var
  C, C2: TColor;
begin
  if BrushColor <> clNone then begin
    C := ACanvas.Brush.Color;
    C2 := ACanvas.Pen.Color;
    ACanvas.Brush.Color := BrushColor;
    ACanvas.Pen.Color := PenColor;
    if PenColor = clNone then
      ACanvas.FillRect(ARect)
    else
      ACanvas.Rectangle(ARect);
    ACanvas.Brush.Color := C;
    ACanvas.Pen.Color := C2;
  end;
end;

procedure SpDrawLine(ACanvas: TCanvas; X1, Y1, X2, Y2: Integer; Color: TColor);
var
  C: TColor;
begin
  if Color <> clNone then begin
    C := ACanvas.Pen.Color;
    ACanvas.Pen.Color := Color;
    ACanvas.MoveTo(X1, Y1);
    ACanvas.LineTo(X2, Y2);
    ACanvas.Pen.Color := C;
  end;
end;

procedure SpDrawRectangle(ACanvas: TCanvas; ARect: TRect; CornerSize: Integer;
  ColorL, ColorT, ColorR, ColorB,
  InternalColorL, InternalColorT, InternalColorR, InternalColorB: TColor;
  ForceRectBorders: TAnchors = []);
// Draws 2 beveled borders.
// CornerSize can be 0, 1 or 2.
// Color: left, top, right, bottom external border color
// InternalColor: left, top, right, bottom internal border color
// ForceRectBorders: forces the borders to be rect
var
  Color: TColor;
  CornerSizeTL, CornerSizeTR, CornerSizeBL, CornerSizeBR: Integer;
begin
  Color := ACanvas.Pen.Color;

  if CornerSize < 0 then CornerSize := 0;
  if CornerSize > 2 then CornerSize := 2;
  CornerSizeTL := CornerSize;
  CornerSizeTR := CornerSize;
  CornerSizeBL := CornerSize;
  CornerSizeBR := CornerSize;
  if akLeft in ForceRectBorders then begin
    CornerSizeTL := 0;
    CornerSizeBL := 0;
  end;
  if akRight in ForceRectBorders then begin
    CornerSizeTR := 0;
    CornerSizeBR := 0;
  end;
  if akTop in ForceRectBorders then begin
    CornerSizeTL := 0;
    CornerSizeTR := 0;
  end;
  if akBottom in ForceRectBorders then begin
    CornerSizeBL := 0;
    CornerSizeBR := 0;
  end;

  with ARect do begin
    Dec(Right);
    Dec(Bottom);

    // Internal borders
    InflateRect(ARect, -1, -1);
    if InternalColorL <> clNone then begin
      ACanvas.Pen.Color := InternalColorL;
      ACanvas.PolyLine([Point(Left, Bottom), Point(Left, Top)]);
    end;
    if InternalColorT <> clNone then begin
      ACanvas.Pen.Color := InternalColorT;
      ACanvas.PolyLine([Point(Left, Top), Point(Right, Top)]);
    end;
    if InternalColorR <> clNone then begin
      ACanvas.Pen.Color := InternalColorR;
      ACanvas.PolyLine([Point(Right, Bottom), Point(Right, Top - 1)]);
    end;
    if InternalColorB <> clNone then begin
      ACanvas.Pen.Color := InternalColorB;
      ACanvas.PolyLine([Point(Left, Bottom), Point(Right, Bottom)]);
    end;

    // External borders
    InflateRect(ARect, 1, 1);
    if ColorL <> clNone then begin
      ACanvas.Pen.Color := ColorL;
      ACanvas.PolyLine([
        Point(Left, Bottom - CornerSizeBL),
        Point(Left, Top + CornerSizeTL)
      ]);
    end;
    if ColorT <> clNone then begin
      ACanvas.Pen.Color := ColorT;
      ACanvas.PolyLine([
        Point(Left, Top + CornerSizeTL),
        Point(Left + CornerSizeTL, Top),
        Point(Right - CornerSizeTR + 1, Top),
        Point(Right, Top + CornerSizeTR)
      ]);
    end;
    if ColorR <> clNone then begin
      ACanvas.Pen.Color := ColorR;
      ACanvas.PolyLine([
        Point(Right, Top + CornerSizeTR),
        Point(Right , Bottom - CornerSizeBR)
      ]);
    end;
    if ColorB <> clNone then begin
      ACanvas.Pen.Color := ColorB;
      ACanvas.PolyLine([
        Point(Right, Bottom - CornerSizeBR),
        Point(Right - CornerSizeBR, Bottom),
        Point(Left + CornerSizeBL, Bottom),
        Point(Left, Bottom - CornerSizeBL)
      ]);
    end;
  end;

  ACanvas.Pen.Color := Color;
end;


procedure SpDrawRectangle(ACanvas: TCanvas; ARect: TRect;
  CornerSize: Integer; ColorTL, ColorBR, ColorTLInternal, ColorBRInternal: TColor;
  ForceRectBorders: TAnchors);
// Draws 2 beveled borders.
// CornerSize can be 0, 1 or 2.
// TLColor, ColorBR: external border color
// InternalTL, ColorBRInternal: internal border color
// ForceRectBorders: forces the borders to be rect
var
  Color: TColor;
  CornerSizeTL, CornerSizeTR, CornerSizeBL, CornerSizeBR: Integer;
begin
  Color := ACanvas.Pen.Color;

  if CornerSize < 0 then CornerSize := 0;
  if CornerSize > 2 then CornerSize := 2;
  CornerSizeTL := CornerSize;
  CornerSizeTR := CornerSize;
  CornerSizeBL := CornerSize;
  CornerSizeBR := CornerSize;
  if akLeft in ForceRectBorders then begin
    CornerSizeTL := 0;
    CornerSizeBL := 0;
  end;
  if akRight in ForceRectBorders then begin
    CornerSizeTR := 0;
    CornerSizeBR := 0;
  end;
  if akTop in ForceRectBorders then begin
    CornerSizeTL := 0;
    CornerSizeTR := 0;
  end;
  if akBottom in ForceRectBorders then begin
    CornerSizeBL := 0;
    CornerSizeBR := 0;
  end;

  with ARect do begin
    Dec(Right);
    Dec(Bottom);

    // Internal borders
    InflateRect(ARect, -1, -1);
    if ColorTLInternal <> clNone then begin
      ACanvas.Pen.Color := ColorTLInternal;
      ACanvas.PolyLine([
        Point(Left, Bottom),
        Point(Left, Top),
        Point(Right, Top)
      ]);
    end;
    if ColorBRInternal <> clNone then begin
      ACanvas.Pen.Color := ColorBRInternal;
      ACanvas.PolyLine([
        Point(Left, Bottom),
        Point(Right, Bottom),
        Point(Right, Top - 1)
      ]);
    end;

    // External borders
    InflateRect(ARect, 1, 1);
    if ColorTL <> clNone then begin
      ACanvas.Pen.Color := ColorTL;
      ACanvas.PolyLine([
        Point(Left + CornerSizeBL, Bottom),
        Point(Left, Bottom - CornerSizeBL),
        Point(Left, Top + CornerSizeTL),
        Point(Left + CornerSizeTL, Top),
        Point(Right - CornerSizeTR, Top),
        Point(Right, Top + CornerSizeTR)
      ]);
    end;
    if ColorBR <> clNone then begin
      ACanvas.Pen.Color := ColorBR;
      ACanvas.PolyLine([
        Point(Right, Top + CornerSizeTR),
        Point(Right , Bottom - CornerSizeBR),
        Point(Right - CornerSizeBR, Bottom),
        Point(Left + CornerSizeBL - 1, Bottom)
      ]);
    end;
  end;

  ACanvas.Pen.Color := Color;
end;

procedure SpAlphaBlend(SrcDC, DstDC: HDC; SrcR, DstR: TRect; Alpha: Byte;
  SrcHasAlphaChannel: Boolean = False);
// NOTE: AlphaBlend does not work on Windows 95 and Windows NT
var
  BF: TBlendFunction;
begin
  BF.BlendOp := AC_SRC_OVER;
  BF.BlendFlags := 0;
  BF.SourceConstantAlpha := Alpha;
  if SrcHasAlphaChannel then
    BF.AlphaFormat := AC_SRC_ALPHA
  else
    BF.AlphaFormat := 0;
  Windows.AlphaBlend(DstDC, DstR.Left, DstR.Top, DstR.Right - DstR.Left, DstR.Bottom - DstR.Top,
    SrcDC, SrcR.Left, SrcR.Top, SrcR.Right - SrcR.Left, SrcR.Bottom - SrcR.Top, BF);
end;

procedure SpPaintTo(WinControl: TWinControl; ACanvas: TCanvas; X, Y: Integer);
// NOTE: PrintWindow does not work if the control is not visible
var
  B: TBitmap;
  PrevTop: Integer;
begin
  // Use SpPrintWindow instead of PaintTo as many controls will not render
  // properly (no text on editors, no scrollbars, incorrect borders, etc)
  // http://msdn2.microsoft.com/en-us/library/ms535695.aspx
  if Assigned(SpPrintWindow) then begin
    ACanvas.Lock;
    try
      // It doesn't work if the control is not visible !!!
      // Show it and move it offscreen
      if not WinControl.Visible then begin
        PrevTop := WinControl.Top;
        WinControl.Top := 10000;  // Move it offscreen
        WinControl.Visible := True;
        SpPrintWindow(WinControl.Handle, ACanvas.Handle, 0);
        WinControl.Visible := False;
        WinControl.Top := PrevTop;
      end
      else
        SpPrintWindow(WinControl.Handle, ACanvas.Handle, 0);
    finally
      ACanvas.UnLock;
    end;
  end
  else begin
    // If SpPrintWindow is not available use PaintTo
    // If the Control is a Form use GetFormImage instead
    if WinControl is TCustomForm then begin
      B := TCustomForm(WinControl).GetFormImage;
      try
        ACanvas.Draw(X, Y, B);
      finally
        B.Free;
      end;
    end
    else
      WinControl.PaintTo(ACanvas, X, Y);
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ ImageList painting }

procedure SpDrawIconShadow(ACanvas: TCanvas; const ARect: TRect;
  ImageList: TCustomImageList; ImageIndex: Integer);
var
  ImageWidth, ImageHeight: Integer;
  I, J: Integer;
  Src, Dst: ^Cardinal;
  S, C, CBRB, CBG: Cardinal;
  B1, B2: TBitmap;
begin
  ImageWidth := ARect.Right - ARect.Left;
  ImageHeight := ARect.Bottom - ARect.Top;
  with ImageList do
  begin
    if Width < ImageWidth then ImageWidth := Width;
    if Height < ImageHeight then ImageHeight :=  Height;
  end;

  B1 := TBitmap.Create;
  B2 := TBitmap.Create;
  try
    B1.PixelFormat := pf32bit;
    B2.PixelFormat := pf32bit;
    B1.Width := ImageWidth;
    B1.Height := ImageHeight;
    B2.Width := ImageWidth;
    B2.Height := ImageHeight;

    BitBlt(B1.Canvas.Handle, 0, 0, ImageWidth, ImageHeight, ACanvas.Handle, ARect.Left, ARect.Top, SRCCOPY);
    BitBlt(B2.Canvas.Handle, 0, 0, ImageWidth, ImageHeight, ACanvas.Handle, ARect.Left, ARect.Top, SRCCOPY);
    ImageList.Draw(B2.Canvas, 0, 0, ImageIndex, True);

    for J := 0 to ImageHeight - 1 do
    begin
      Src := B2.ScanLine[J];
      Dst := B1.ScanLine[J];
      for I := 0 to ImageWidth - 1 do
      begin
        S := Src^;
        if S <> Dst^ then
        begin
          CBRB := Dst^ and $00FF00FF;
          CBG  := Dst^ and $0000FF00;
          C := ((S and $00FF0000) shr 16 * 29 + (S and $0000FF00) shr 8 * 150 +
            (S and $000000FF) * 76) shr 8;
          C := (C div 3) + (255 - 255 div 3);
          Dst^ := ((CBRB * C and $FF00FF00) or (CBG * C and $00FF0000)) shr 8;
        end;
        Inc(Src);
        Inc(Dst);
      end;
    end;
    BitBlt(ACanvas.Handle, ARect.Left, ARect.Top, ImageWidth, ImageHeight, B1.Canvas.Handle, 0, 0, SRCCOPY);
  finally
    B1.Free;
    B2.Free;
  end;
end;

procedure SpDrawImageList(ACanvas: TCanvas; const ARect: TRect; ImageList: TCustomImageList;
  ImageIndex: Integer; Enabled, DisabledIconCorrection: Boolean);
begin
  if Assigned(ImageList) and (ImageIndex > -1) and (ImageIndex < ImageList.Count) then
    if Enabled then
      ImageList.Draw(ACanvas, ARect.Left, ARect.Top, ImageIndex)
    else
      if DisabledIconCorrection then
        SpDrawIconShadow(ACanvas, ARect, ImageList, ImageIndex)
      else
        ImageList.Draw(ACanvas, ARect.Left, ARect.Top, ImageIndex, False);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Gradients }

procedure SpGradient(ACanvas: TCanvas; const ARect: TRect;
  StartPos, EndPos, ChunkSize: Integer; C1, C2: TColor; const Vertical: Boolean);
// StartPos: start position relative to ARect, usually 0
// EndPos: end position relative to ARect, usually ARect.Bottom - ARect.Top
// ChunkSize: size of the chunk of the gradient we need to paint
  procedure SetVertex(var AVertex: TTriVertex; const APoint: TPoint; ARGBColor: DWORD);
  begin
    AVertex.X := APoint.X;
    AVertex.Y := APoint.Y;
    AVertex.Red := MakeWord(0, GetRValue(ARGBColor));
    AVertex.Green := MakeWord(0, GetGValue(ARGBColor));
    AVertex.Blue := MakeWord(0, GetBValue(ARGBColor));
    AVertex.Alpha := MakeWord(0, 255);
  end;
const
  AModesMap: array[Boolean] of DWORD = (GRADIENT_FILL_RECT_H, GRADIENT_FILL_RECT_V);
var
  AVertices: array[0..1] of TTriVertex;
  AGradientRect: TGradientRect;
  ARGBColor1, ARGBColor2: DWORD;
  Vertex: TPoint;
begin
  ARGBColor1 := ColorToRGB(C1);
  ARGBColor2 := ColorToRGB(C2);
  Vertex := ARect.TopLeft;
  if Vertical then
    Vertex.Y := Vertex.Y + StartPos
  else
    Vertex.X := Vertex.X + StartPos;
  SetVertex(AVertices[0], Vertex, ARGBColor1);
  Vertex := ARect.BottomRight;
  if Vertical then
    Vertex.Y := ARect.Top + EndPos + 1
  else
    Vertex.X := ARect.Left + EndPos + 1;
  SetVertex(AVertices[1], Vertex, ARGBColor2);
  AGradientRect.UpperLeft := 0;
  AGradientRect.LowerRight := 1;
  GradientFill(ACanvas.Handle, @AVertices, 2, @AGradientRect, 1, AModesMap[Vertical]);
end;

procedure SpGradientFill(ACanvas: TCanvas; const ARect: TRect;
  const C1, C2: TColor; const Vertical: Boolean);
var
  GSize: Integer;
begin
  if Vertical then
    GSize := (ARect.Bottom - ARect.Top) - 1
  else
    GSize := (ARect.Right - ARect.Left) - 1;

  SpGradient(ACanvas, ARect, 0, GSize, GSize, C1, C2, Vertical);
end;

procedure SpGradientFillMirror(ACanvas: TCanvas; const ARect: TRect;
  const C1, C2, C3, C4: TColor; const Vertical: Boolean);
var
  GSize, ChunkSize, d1, d2: Integer;
begin
  if Vertical then
    GSize := (ARect.Bottom - ARect.Top) - 1
  else
    GSize := (ARect.Right - ARect.Left) - 1;

  ChunkSize := GSize div 2;
  if ChunkSize = 0 then ChunkSize := 1;
  d1 := ChunkSize;
  d2 := GSize;

  SpGradient(ACanvas, ARect, 0, d1, ChunkSize, C1, C2, Vertical);
  SpGradient(ACanvas, ARect, d1, d2, ChunkSize, C3, C4, Vertical);
end;

procedure SpGradientFillMirrorTop(ACanvas: TCanvas; const ARect: TRect;
  const C1, C2, C3, C4: TColor; const Vertical: Boolean);
var
  GSize, d1, d2: Integer;
begin
  if Vertical then
    GSize := (ARect.Bottom - ARect.Top) - 1
  else
    GSize := (ARect.Right - ARect.Left) - 1;

  d1 := GSize div 3;
  d2 := GSize;

  SpGradient(ACanvas, ARect, 0, d1, d1, C1, C2, Vertical);
  SpGradient(ACanvas, ARect, d1, d2, d2 - d1, C3, C4, Vertical);
end;

procedure SpGradientFillGlass(ACanvas: TCanvas; const ARect: TRect;
  const C1, C2, C3, C4: TColor; const Vertical: Boolean);
var
  GSize, ChunkSize, d1, d2, d3: Integer;
begin
  if Vertical then
    GSize := (ARect.Bottom - ARect.Top) - 1
  else
    GSize := (ARect.Right - ARect.Left) - 1;

  ChunkSize := GSize div 3;
  if ChunkSize = 0 then ChunkSize := 1;
  d1 := ChunkSize;
  d2 := ChunkSize * 2;
  d3 := GSize;

  SpGradient(ACanvas, ARect, 0, d1, ChunkSize, C1, C2, Vertical);
  SpGradient(ACanvas, ARect, d1, d2, ChunkSize, C2, C3, Vertical);
  SpGradient(ACanvas, ARect, d2, d3, ChunkSize, C3, C4, Vertical);
end;

procedure SpGradientFill9pixels(ACanvas: TCanvas; const ARect: TRect;
  const C1, C2, C3, C4: TColor; const Vertical: Boolean);
// Mimics Vista menubar/toolbar blue gradient
var
  GSize, d1, d2: Integer;
begin
  if Vertical then
    GSize := (ARect.Bottom - ARect.Top) - 1
  else
    GSize := (ARect.Right - ARect.Left) - 1;

  d1 := GSize div 3;
  if d1 > 9 then d1 := 9;
  d2 := GSize;

  SpGradient(ACanvas, ARect, 0, d1, d1, C1, C2, Vertical);
  SpGradient(ACanvas, ARect, d1, d2, d2 - d1, C3, C4, Vertical);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Element painting }

procedure SpDrawArrow(ACanvas: TCanvas; X, Y: Integer; AColor: TColor; Vertical, Reverse: Boolean; Size: Integer);
var
  C1, C2: TColor;
begin
  C1 := ACanvas.Pen.Color;
  C2 := ACanvas.Brush.Color;
  ACanvas.Pen.Color := AColor;
  ACanvas.Brush.Color := AColor;

  if Vertical then
    if Reverse then
      ACanvas.Polygon([Point(X, Y), Point(X - Size, Y + Size), Point(X + Size, Y + Size)])
    else
      ACanvas.Polygon([Point(X - Size, Y), Point(X + Size, Y), Point(X, Y + Size)])
  else
    if Reverse then
      ACanvas.Polygon([Point(X, Y), Point(X + Size, Y + Size), Point(X + Size, Y - Size)])
    else
      ACanvas.Polygon([Point(X, Y - Size), Point(X, Y + Size), Point(X + Size, Y)]);

  ACanvas.Pen.Color := C1;
  ACanvas.Brush.Color := C2;
end;

procedure SpDrawDropMark(ACanvas: TCanvas; DropMark: TRect);
var
  C: TColor;
  R: TRect;
begin
  if IsRectEmpty(DropMark) then Exit;
  C := ACanvas.Brush.Color;

  R := Rect(DropMark.Left + 1, DropMark.Top, DropMark.Right - 1, DropMark.Top + 2);
  ACanvas.Rectangle(R);
  R := Rect(DropMark.Left + 1, DropMark.Bottom - 2, DropMark.Right - 1, DropMark.Bottom);
  ACanvas.Rectangle(R);

  R := Rect(DropMark.Left, DropMark.Top + 1, DropMark.Right, DropMark.Top + 3);
  ACanvas.Rectangle(R);
  R := Rect(DropMark.Left, DropMark.Bottom - 3, DropMark.Right, DropMark.Bottom - 1);
  ACanvas.Rectangle(R);

  R := Rect(DropMark.Left + 1, DropMark.Top + 4, DropMark.Right - 1, DropMark.Bottom - 4);
  ACanvas.Rectangle(R);

    {
    // Standard DropMark

    R := Rect(DropMark.Left, DropMark.Top, DropMark.Right - 1, DropMark.Bottom - 1);
    if IsRectEmpty(R) then Exit;
    ACanvas.Brush.Color := clBlack;
    ACanvas.Polygon([
      Point(R.Left, R.Top),
      Point(R.Left + 2, R.Top + 2),
      Point(R.Left + 2, R.Bottom - 2),
      Point(R.Left, R.Bottom),
      Point(R.Right, R.Bottom),
      Point(R.Right - 2, R.Bottom - 2),
      Point(R.Right - 2, R.Top + 2),
      Point(R.Right, R.Top)
    ]);
    }
  ACanvas.Brush.Color := C;
end;

procedure SpDrawFocusRect(ACanvas: TCanvas; const ARect: TRect);
var
  DC: HDC;
  C1, C2: TColor;
begin
  if not IsRectEmpty(ARect) then begin
    DC := ACanvas.Handle;
    C1 := SetTextColor(DC, clBlack);
    C2 := SetBkColor(DC, clWhite);
    ACanvas.DrawFocusRect(ARect);
    SetTextColor(DC, C1);
    SetBkColor(DC, C2);
  end;
end;

procedure SpDrawGlyphPattern(DC: HDC; const R: TRect; Width, Height: Integer;
  const PatternBits; PatternColor: TColor);
var
  B: TBitmap;
  OldTextColor, OldBkColor: Longword;
  OldBrush, Brush: HBrush;
  BitmapWidth, BitmapHeight: Integer;
begin
  OldTextColor := SetTextColor(DC, clBlack);
  OldBkColor := SetBkColor(DC, clWhite);
  B := TBitmap.Create;
  try
    BitmapWidth := 8;
    if Width > BitmapWidth then BitmapWidth := Width;
    BitmapHeight := 8;
    if Height > BitmapHeight then BitmapHeight := Height;
    
    B.Handle := CreateBitmap(BitmapWidth, BitmapHeight, 1, 1, @PatternBits);
    if PatternColor < 0 then Brush := GetSysColorBrush(PatternColor and $FF)
    else Brush := CreateSolidBrush(PatternColor);
    OldBrush := SelectObject(DC, Brush);
    BitBlt(DC, (R.Left + R.Right + 1 - Width) div 2, (R.Top + R.Bottom  + 1 - Height) div 2,
      Width, Height, B.Canvas.Handle, 0, 0, ROP_DSPDxax);
    SelectObject(DC, OldBrush);
    if PatternColor >= 0 then DeleteObject(Brush);
  finally
    SetTextColor(DC, OldTextColor);
    SetBkColor(DC, OldBkColor);
    B.Free;
  end;
end;

procedure SpDrawGlyphPattern(ACanvas: TCanvas; ARect: TRect; PatternIndex: Integer; PatternColor: TColor);
// The pattern is a 8x8 bitmap
// The array has 16 elements, only the odd elements are used
// The first value of an element represents the bits from the 4 first horizontal pixels,
// and the next value represents the bits of the 4 last horizontal pixels.
// For example: 0   represents --------
//              $FF represents xxxxxxxx
//              $C6 represents xx---xx-
const
  ClosePattern: array [0..15] of Byte    = ($C6, 0, $EE, 0, $7C, 0, $38, 0, $7C, 0, $EE, 0, $C6, 0, 0, 0);
  MaximizePattern: array [0..15] of Byte = ($FF, 0, $FF, 0, $81, 0, $81, 0, $81, 0, $81, 0, $81, 0, $FF, 0);
  MinimizePattern: array [0..15] of Byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, $7E, 0, $7E, 0, 0, 0);
  RestorePattern: array [0..17] of Byte = ($3F, 0, $3F, 0, $21, 0, $FD, 0, $FD, 0, $87, 0, $84, 0, $84, 0, $FC, 0);
begin
  case PatternIndex of
    0: SpDrawGlyphPattern(ACanvas.Handle, ARect, 8, 8, ClosePattern[0], PatternColor);
    1: SpDrawGlyphPattern(ACanvas.Handle, ARect, 8, 8, MaximizePattern[0], PatternColor);
    2: SpDrawGlyphPattern(ACanvas.Handle, ARect, 8, 8, MinimizePattern[0], PatternColor);
    3: SpDrawGlyphPattern(ACanvas.Handle, ARect, 8, 9, RestorePattern[0], PatternColor);
  end;
end;

procedure SpDrawXPButton(ACanvas: TCanvas; ARect: TRect; Enabled, Pushed,
  HotTrack, Checked, Focused, Defaulted: Boolean);
var
  C: TColor;
  State: TSpTBXSkinStatesType;
begin
  case SkinManager.GetSkinType of
    sknNone:
      begin
        C := ACanvas.Brush.Color;
        ACanvas.Brush.Color := clBtnFace;
        ACanvas.FillRect(ARect);
        if Defaulted or Focused then begin
          ACanvas.Brush.Color := clWindowFrame;
          ACanvas.FrameRect(ARect);
          InflateRect(ARect, -1, -1);  // Reduce the Rect for the focus rect
        end;
        if Pushed or Checked then begin
          ACanvas.Brush.Color := clBtnShadow;
          ACanvas.FrameRect(ARect);
        end
        else
          DrawFrameControl(ACanvas.Handle, ARect, DFC_BUTTON, DFCS_BUTTONPUSH);
        ACanvas.Brush.Color := C;
      end;
    sknWindows, sknDelphiStyle:
      CurrentSkin.PaintThemedElementBackground(ACanvas, ARect, skncButton, Enabled, Pushed, HotTrack, Checked, Focused, Defaulted, False);
    sknSkin:
      begin
        State := CurrentSkin.GetState(Enabled, Pushed, HotTrack, Checked);
        CurrentSkin.PaintBackground(ACanvas, ARect, skncButton, State, True, True);
      end;
  end;

  if Focused then begin
    InflateRect(ARect, -3, -3);
    SpDrawFocusRect(ACanvas, ARect);
  end;
end;

procedure SpDrawXPCheckBoxGlyph(ACanvas: TCanvas; ARect: TRect; Enabled: Boolean;
  State: TCheckBoxState; HotTrack, Pushed: Boolean);
var
  Flags: Cardinal;
  SknState: TSpTBXSkinStatesType;
begin
  Flags := 0;
  case SkinManager.GetSkinType of
    sknNone:
      begin
        case State of
          cbChecked: Flags := DFCS_BUTTONCHECK or DFCS_CHECKED;
          cbGrayed: Flags := DFCS_BUTTON3STATE or DFCS_CHECKED;
          cbUnChecked: Flags := DFCS_BUTTONCHECK;
        end;
        if not Enabled then
          Flags := Flags or DFCS_INACTIVE;
        if Pushed then
          Flags := Flags or DFCS_PUSHED;
        DrawFrameControl(ACanvas.Handle, ARect, DFC_BUTTON, Flags);
      end;
    sknWindows, sknDelphiStyle:
      CurrentSkin.PaintThemedElementBackground(ACanvas, ARect, skncCheckBox, Enabled, Pushed, HotTrack, State = cbChecked, False, False, State = cbGrayed);
    sknSkin:
      begin
        SknState := CurrentSkin.GetState(Enabled, Pushed, HotTrack, State in [cbChecked, cbGrayed]);
        CurrentSkin.PaintMenuCheckMark(ACanvas, ARect, State = cbChecked, State = cbGrayed, False, SknState);
      end;
  end;
end;

procedure SpDrawXPRadioButtonGlyph(ACanvas: TCanvas; ARect: TRect; Enabled: Boolean;
  Checked, HotTrack, Pushed: Boolean);
var
  Flags: Integer;
  SknState: TSpTBXSkinStatesType;
begin
  case SkinManager.GetSkinType of
    sknNone:
      begin
        Flags := DFCS_BUTTONRADIO;
        if Checked then
          Flags := Flags or DFCS_CHECKED;
        if not Enabled then
          Flags := Flags or DFCS_INACTIVE;
        if Pushed then
          Flags := Flags or DFCS_PUSHED;
        DrawFrameControl(ACanvas.Handle, ARect, DFC_BUTTON, Flags);
      end;
    sknWindows, sknDelphiStyle:
      CurrentSkin.PaintThemedElementBackground(ACanvas, ARect, skncRadioButton, Enabled, Pushed, HotTrack, Checked, False, False, False);
    sknSkin:
      begin
        SknState := CurrentSkin.GetState(Enabled, Pushed, HotTrack, Checked);
        CurrentSkin.PaintMenuRadioMark(ACanvas, ARect, Checked, False, SknState);
      end;
  end;
end;

procedure SpDrawXPEditFrame(ACanvas: TCanvas; ARect: TRect; Enabled, HotTrack: Boolean;
  ClipContent: Boolean; AutoAdjust: Boolean);
var
  PartID, Flags: Integer;
  BorderR: TRect;
  State: TSpTBXSkinStatesType;
  Entry: TSpTBXSkinOptionEntry;
begin
  if ClipContent then begin
    BorderR := ARect;
    if HotTrack then
      InflateRect(BorderR, -1, -1)
    else
      InflateRect(BorderR, -2, -2);
    ExcludeClipRect(ACanvas.Handle, BorderR.Left, BorderR.Top, BorderR.Right, BorderR.Bottom);
  end;
  try
    case SkinManager.GetSkinType of
      sknNone:
        if HotTrack then
          SpDrawRectangle(ACanvas, ARect, 0, clBtnShadow, clBtnHighlight, clBtnFace, clBtnFace)
        else
          SpDrawRectangle(ACanvas, ARect, 0, clBtnFace, clBtnFace, clBtnFace, clBtnFace);
      sknWindows, sknDelphiStyle:
        begin
          if SpIsWinVistaOrUp then begin
            // Use the new API on Windows Vista
            PartID := CP_BORDER;
            if not Enabled then Flags := CBXS_DISABLED
            else if HotTrack then Flags := CBXS_HOT
            else Flags := CBXS_NORMAL;
          end
          else begin
            PartID := 0;
            Flags := 0;
          end;
          DrawThemeBackground(SpTBXThemeServices.Theme[teComboBox], ACanvas.Handle, PartID, Flags, ARect, nil);
        end;
      sknSkin:
        begin
          State := CurrentSkin.GetState(Enabled, False, HotTrack, False);
          // Try to adjust the borders if only the internal borders are specified,
          // used by some controls that need to paint the edit frames like
          // TSpTBXPanel (HotTrack=True), TSpTBXListBox, TSpTBXCheckListBox, etc
          if AutoAdjust then begin
            Entry := SkinManager.CurrentSkin.Options(skncEditFrame, State).Borders;
            if (Entry.Color1 = clNone) and (Entry.Color2 = clNone) and
              (Entry.Color3 <> clNone) and (Entry.Color4 <> clNone) then
            begin
              CurrentSkin.PaintBackground(ACanvas, ARect, skncEditFrame, State, True, False);
              SpDrawRectangle(ACanvas, ARect, Entry.SkinType, Entry.Color3, Entry.Color4);
              Exit;
            end;
          end;

          CurrentSkin.PaintBackground(ACanvas, ARect, skncEditFrame, State, True, True);
        end;
    end;
  finally
    if ClipContent then
      SelectClipRgn(ACanvas.Handle, 0);
  end;
end;

procedure SpDrawXPEditFrame(AWinControl: TWinControl; HotTracking: Boolean;
  AutoAdjust, HideFrame: Boolean);
var
  R: TRect;
  DC: HDC;
  ACanvas: TCanvas;
begin
  DC := GetWindowDC(AWinControl.Handle);
  try
    ACanvas := TCanvas.Create;
    try
      ACanvas.Handle := DC;
      GetWindowRect(AWinControl.Handle, R);
      OffsetRect(R, -R.Left, -R.Top);
      with R do
        ExcludeClipRect(DC, Left + 2, Top + 2, Right - 2, Bottom - 2);

      if HideFrame then begin
        ACanvas.Brush.Color := TControlAccess(AWinControl).Color;
        ACanvas.FillRect(R);
      end
      else begin
        // Don't use SpDrawParentBackground to paint the background it doesn't get
        // the correct WindowOrg in this particular case
//  Commented because of conflict when painting on Glass
//        PerformEraseBackground(AWinControl, ACanvas.Handle);
        SpDrawParentBackground(AWinControl, ACanvas.Handle, R);

        SpDrawXPEditFrame(ACanvas, R, AWinControl.Enabled, HotTracking, False, AutoAdjust);
      end;
    finally
      ACanvas.Handle := 0;
      ACanvas.Free;
    end;
  finally
    ReleaseDC(AWinControl.Handle, DC);
  end;
end;

procedure SpDrawXPGrip(ACanvas: TCanvas; ARect: TRect; LoC, HiC: TColor);
var
  I, J: Integer;
  XCellCount, YCellCount: Integer;
  R: TRect;
  C: TColor;
begin
  //  4 x 4 cells (Grey, White, Null)
  //  GG--
  //  GGW-
  //  -WW-
  //  ----

  C := ACanvas.Brush.Color;
  XCellCount := (ARect.Right - ARect.Left) div 4;
  YCellCount := (ARect.Bottom - ARect.Top) div 4;
  if XCellCount = 0 then XCellCount := 1;
  if YCellCount = 0 then YCellCount := 1;
  
  for J := 0 to YCellCount - 1 do
    for I := 0 to XCellCount - 1 do begin
      R.Left := ARect.Left + (I * 4) + 1;
      R.Right := R.Left + 2;
      R.Top := ARect.Top + (J * 4) + 1;
      R.Bottom := R.Top + 2;

      ACanvas.Brush.Color := HiC;
      ACanvas.FillRect(R);
      OffsetRect(R, -1, -1);
      ACanvas.Brush.Color := LoC;
      ACanvas.FillRect(R);
    end;
  ACanvas.Brush.Color := C;
end;

procedure SpDrawXPHeader(ACanvas: TCanvas; ARect: TRect; HotTrack, Pushed: Boolean);
var
//  Flags: Cardinal;
  State: TSpTBXSkinStatesType;
begin
  case SkinManager.GetSkinType of
    sknNone:
      begin
        DrawEdge(ACanvas.Handle, ARect, BDR_RAISEDINNER or BDR_RAISEDOUTER, BF_RECT or BF_SOFT);
      end;
    sknWindows, sknDelphiStyle:
      begin
        CurrentSkin.PaintThemedElementBackground(ACanvas, ARect, skncHeader, True,
          Pushed, HotTrack, False, False, False, False);
//        if Pushed then Flags := HIS_PRESSED
//        else if HotTrack then Flags := HIS_HOT
//        else Flags := HIS_NORMAL;
//        DrawThemeBackground(SpTBXThemeServices.Theme[teHeader], ACanvas.Handle, HP_HEADERITEM, Flags, ARect, nil);
      end;
    sknSkin:
      begin
        State := CurrentSkin.GetState(True, Pushed, HotTrack, False);
        if (State = sknsPushed) and CurrentSkin.Options(skncHeader, State).IsEmpty then
          State := sknsHotTrack;
        CurrentSkin.PaintBackground(ACanvas, ARect, skncHeader, State, True, True);
      end;
  end;
end;

procedure SpDrawXPListItemBackground(ACanvas: TCanvas; ARect: TRect; Selected, HotTrack, Focused: Boolean;
  ForceRectBorders: Boolean; Borders: Boolean);
var
  State: TSpTBXSkinStatesType;
begin
  State := CurrentSkin.GetState(True, False, HotTrack, Selected);
  ACanvas.Font.Color := CurrentSkin.GetTextColor(skncListItem, State);

  if SkinManager.GetSkinType = sknSkin then begin
    ACanvas.FillRect(ARect);
    if HotTrack or Selected then begin
      if ForceRectBorders then
        CurrentSkin.PaintBackground(ACanvas, ARect, skncListItem, State, True, Borders, False, [akLeft, akTop, akRight, akBottom])
      else
        CurrentSkin.PaintBackground(ACanvas, ARect, skncListItem, State, True, Borders);
    end;
  end
  else begin
    if SkinManager.GetSkinType = sknDelphiStyle then begin
      {$IF CompilerVersion >= 23} // for Delphi XE2 and up
      if Selected then
        ACanvas.Brush.Color := SpTBXThemeServices.GetSystemColor(clHighlight)
      else
        ACanvas.Brush.Color := SpTBXThemeServices.GetStyleColor(scListBox);
      {$IFEND}
    end
    else
      if Selected then
        ACanvas.Brush.Color := clHighlight;
    ACanvas.FillRect(ARect);
    if Focused then
      SpDrawFocusRect(ACanvas, ARect);
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Skins painting }

procedure SpPaintSkinBackground(ACanvas: TCanvas; ARect: TRect; SkinOption: TSpTBXSkinOptionCategory; Vertical: Boolean);
var
  Part: TSpTBXSkinOptionEntry;
  SkinType: Integer;
begin
  Part := SkinOption.Body;
  SkinType := SkinOption.Body.SkinType;

  if Vertical then
    case SkinType of
      1: SkinType := 2;  // Vertical Gradient to Horizontal
      2: SkinType := 1;  // Horizontal Gradient to Vertical

      3: SkinType := 4;  // Vertical Glass Gradient to Horizontal
      4: SkinType := 3;  // Horizontal Glass Gradient to Vertical

      5: SkinType := 6;  // Vertical Mirror Gradient to Horizontal
      6: SkinType := 5;  // Horizontal Mirror Gradient to Vertical

      7: SkinType := 8;  // Vertical MirrorTop Gradient to Horizontal
      8: SkinType := 7;  // Horizontal MirrorTop Gradient to Vertical

      9: SkinType := 10; // Vertical 9Pixels Gradient to Horizontal
      10: SkinType := 9; // Horizontal 9Pixels Gradient to Vertical
    end;

  case SkinType of
    0: begin  // Solid
         SpFillRect(ACanvas, ARect, Part.Color1);
       end;
    1: begin  // Vertical Gradient
         SpGradientFill(ACanvas, ARect, Part.Color1, Part.Color2, True);
       end;
    2: begin  // Horizontal Gradient
         SpGradientFill(ACanvas, ARect, Part.Color1, Part.Color2, False);
       end;
    3: begin  // Vertical Glass Gradient
         SpGradientFillGlass(ACanvas, ARect, Part.Color1, Part.Color2, Part.Color3, Part.Color4, True);
       end;
    4: begin  // Horizontal Glass Gradient
         SpGradientFillGlass(ACanvas, ARect, Part.Color1, Part.Color2, Part.Color3, Part.Color4, False);
       end;
    5: begin  // Vertical Mirror Gradient
         SpGradientFillMirror(ACanvas, ARect, Part.Color1, Part.Color2, Part.Color3, Part.Color4, True);
       end;
    6: begin  // Horizontal Mirror Gradient
         SpGradientFillMirror(ACanvas, ARect, Part.Color1, Part.Color2, Part.Color3, Part.Color4, False);
       end;
    7: begin  // Vertical MirrorTop Gradient
         SpGradientFillMirrorTop(ACanvas, ARect, Part.Color1, Part.Color2, Part.Color3, Part.Color4, True);
       end;
    8: begin  // Horizontal MirrorTop Gradient
         SpGradientFillMirrorTop(ACanvas, ARect, Part.Color1, Part.Color2, Part.Color3, Part.Color4, False);
       end;
    9: begin  // Vertical 9Pixels Gradient
         SpGradientFill9Pixels(ACanvas, ARect, Part.Color1, Part.Color2, Part.Color3, Part.Color4, True);
       end;
    10:begin  // Horizontal 9Pixels Gradient
         SpGradientFill9Pixels(ACanvas, ARect, Part.Color1, Part.Color2, Part.Color3, Part.Color4, False);
       end;
  end;
end;

procedure SpPaintSkinBorders(ACanvas: TCanvas; ARect: TRect; SkinOption: TSpTBXSkinOptionCategory;
  ForceRectBorders: TAnchors = []);
var
  Part: TSpTBXSkinOptionEntry;
begin
  Part := SkinOption.Borders;
  case Part.SkinType of
    0, 1, 2: // Rectangle, Simple Rounded and Double Rounded Border
      begin
        SpDrawRectangle(ACanvas, ARect, Part.SkinType, Part.Color1, Part.Color2, Part.Color3, Part.Color4, ForceRectBorders);
      end;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Misc }

function SpIsWinVistaOrUp: Boolean;
begin
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 6);
end;

function SpGetDirectories(Path: string; L: TStringList): Boolean;
var
  SearchRec: TSearchRec;
begin
  Result := False;
  if DirectoryExists(Path) then begin
    Path := IncludeTrailingPathDelimiter(Path) + '*.*';
    if FindFirst(Path, faDirectory, SearchRec) = 0 then begin
      try
        repeat
          if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
            L.Add(SearchRec.Name);
        until FindNext(SearchRec) <> 0;
        Result := True;
      finally
        FindClose(SearchRec);
      end;
    end;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXSkinOptionEntry }

procedure TSpTBXSkinOptionEntry.AssignTo(Dest: TPersistent);
begin
  if Dest is TSpTBXSkinOptionEntry then
    with TSpTBXSkinOptionEntry(Dest) do begin
      SkinType := Self.SkinType;
      Color1 := Self.Color1;
      Color2 := Self.Color2;
      Color3 := Self.Color3;
      Color4 := Self.Color4;
    end
  else inherited AssignTo(Dest);
end;

constructor TSpTBXSkinOptionEntry.Create;
begin
  inherited;
  Reset;
end;

procedure TSpTBXSkinOptionEntry.Fill(ASkinType: Integer; AColor1, AColor2,
  AColor3, AColor4: TColor);
begin
  FSkinType := ASkinType;
  FColor1 := AColor1;
  FColor2 := AColor2;
  FColor3 := AColor3;
  FColor4 := AColor4;
end;

function TSpTBXSkinOptionEntry.IsEmpty: Boolean;
begin
  Result := (FColor1 = clNone) and (FColor2 = clNone) and (FColor3 = clNone) and (FColor4 = clNone);
end;

function TSpTBXSkinOptionEntry.IsEqual(AOptionEntry: TSpTBXSkinOptionEntry): Boolean;
begin
  Result := (FSkinType = AOptionEntry.SkinType) and
    (FColor1 = AOptionEntry.Color1) and (FColor2 = AOptionEntry.Color2) and
    (FColor3 = AOptionEntry.Color3) and (FColor4 = AOptionEntry.Color4);
end;

procedure TSpTBXSkinOptionEntry.Lighten(Amount: Integer);
begin
  if FColor1 <> clNone then FColor1 := SpLighten(FColor1, Amount);
  if FColor2 <> clNone then FColor2 := SpLighten(FColor2, Amount);
  if FColor3 <> clNone then FColor3 := SpLighten(FColor3, Amount);
  if FColor4 <> clNone then FColor4 := SpLighten(FColor4, Amount);
end;

procedure TSpTBXSkinOptionEntry.Reset;
begin
  FSkinType := 0;
  FColor1 := clNone;
  FColor2 := clNone;
  FColor3 := clNone;
  FColor4 := clNone;
end;

procedure TSpTBXSkinOptionEntry.ReadFromString(S: string);
var
  L: TStringList;
begin
  Reset;
  L := TStringList.Create;
  try
    L.CommaText := S;
    try
      if L.Count > 0 then FSkinType := StrToIntDef(L[0], 0);
      if L.Count > 1 then FColor1 := StringToColor(L[1]);
      if L.Count > 2 then FColor2 := StringToColor(L[2]);
      if L.Count > 3 then FColor3 := StringToColor(L[3]);
      if L.Count > 4 then FColor4 := StringToColor(L[4]);
    except
      // do nothing
    end;
  finally
    L.Free;
  end;
end;

function TSpTBXSkinOptionEntry.WriteToString: string;
begin
  Result := Format('%d, %s, %s, %s, %s', [FSkinType,
    SpColorToString(FColor1), SpColorToString(FColor2),
    SpColorToString(FColor3), SpColorToString(FColor4)]);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXThemeOptionCategory }

procedure TSpTBXSkinOptionCategory.AssignTo(Dest: TPersistent);
begin
  if Dest is TSpTBXSkinOptionCategory then
    with TSpTBXSkinOptionCategory(Dest) do begin
      Body.Assign(Self.Body);
      Borders.Assign(Self.Borders);
      TextColor := Self.TextColor;
    end
  else inherited AssignTo(Dest);
end;

constructor TSpTBXSkinOptionCategory.Create;
begin
  inherited;
  FBody := TSpTBXSkinOptionEntry.Create;
  FBorders := TSpTBXSkinOptionEntry.Create;
  FTextColor := clNone;
end;

destructor TSpTBXSkinOptionCategory.Destroy;
begin
  FreeAndNil(FBody);
  FreeAndNil(FBorders);
  inherited;
end;

function TSpTBXSkinOptionCategory.IsEmpty: Boolean;
begin
  Result := FBody.IsEmpty and FBorders.IsEmpty and (FTextColor = clNone);
end;

procedure TSpTBXSkinOptionCategory.Reset;
begin
  FBody.Reset;
  FBorders.Reset;
  FTextColor := clNone;
end;

procedure TSpTBXSkinOptionCategory.SaveToIni(MemIni: TMemIniFile; Section, Ident: string);
begin
  MemIni.WriteString(Section, Ident + '.Body', Body.WriteToString);
  MemIni.WriteString(Section, Ident + '.Borders', Borders.WriteToString);
  MemIni.WriteString(Section, Ident + '.TextColor', SpColorToString(TextColor));
end;

procedure TSpTBXSkinOptionCategory.LoadFromIni(MemIni: TMemIniFile; Section, Ident: string);
begin
  Reset;
  if Ident = '' then Ident := SSpTBXSkinStatesString[sknsNormal];

  Body.ReadFromString(MemIni.ReadString(Section, Ident + '.Body', ''));
  Borders.ReadFromString(MemIni.ReadString(Section, Ident + '.Borders', ''));
  TextColor := StringToColor(MemIni.ReadString(Section, Ident + '.TextColor', 'clNone'));
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXThemeOptions }

procedure TSpTBXSkinOptions.AssignTo(Dest: TPersistent);
var
  C: TSpTBXSkinComponentsType;
  S: TSpTBXSkinStatesType;
  DestOp: TSpTBXSkinOptions;
begin
  if Dest is TSpTBXSkinOptions then begin
    DestOp := TSpTBXSkinOptions(Dest);
    for C := Low(C) to High(C) do
      for S := Low(S) to High(S) do
        DestOp.FOptions[C, S].Assign(Options(C, S));
    DestOp.ColorBtnFace := FColorBtnFace;
    DestOp.FloatingWindowBorderSize := FFloatingWindowBorderSize;
    DestOp.OfficeIcons := FOfficeIcons;
    DestOp.OfficeMenu := FOfficeMenu;
    DestOp.OfficeStatusBar := FOfficeStatusBar;
    DestOp.FSkinAuthor := FSkinAuthor;
    DestOp.FSkinName := FSkinName;
  end
  else inherited AssignTo(Dest);
end;

procedure TSpTBXSkinOptions.BroadcastChanges;
begin
  if Self = SkinManager.CurrentSkin then
    SkinManager.BroadcastSkinNotification;
end;

procedure TSpTBXSkinOptions.CopyOptions(AComponent, ToComponent: TSpTBXSkinComponentsType);
var
  S: TSpTBXSkinStatesType;
begin
  for S := Low(S) to High(S) do
    FOptions[AComponent, S].AssignTo(FOptions[ToComponent, S]);
end;

constructor TSpTBXSkinOptions.Create;
var
  C: TSpTBXSkinComponentsType;
  S: TSpTBXSkinStatesType;
begin
  inherited;
  FSkinName := 'Default';
  FColorBtnFace := clBtnFace;
  FFloatingWindowBorderSize := 4;
  for C := Low(C) to High(C) do
    for S := Low(S) to High(S) do
      FOptions[C, S] := TSpTBXSkinOptionCategory.Create;

  FillOptions;
end;

destructor TSpTBXSkinOptions.Destroy;
var
  C: TSpTBXSkinComponentsType;
  S: TSpTBXSkinStatesType;
begin
  for C := Low(C) to High(C) do
    for S := Low(S) to High(S) do
      FreeAndNil(FOptions[C, S]);

  inherited;
end;

procedure TSpTBXSkinOptions.FillOptions;
begin
  // Used by descendants to fill the skin options
end;

procedure TSpTBXSkinOptions.Reset(ForceResetSkinProperties: Boolean);
var
  C: TSpTBXSkinComponentsType;
  S: TSpTBXSkinStatesType;
begin
  if ForceResetSkinProperties then begin
    FColorBtnFace := clBtnFace;
    FFloatingWindowBorderSize := 4;
    FOfficeIcons := False;
    FOfficeMenu := False;
    FOfficeStatusBar := False;
  end;

  for C := Low(C) to High(C) do
    for S := Low(S) to High(S) do
      FOptions[C, S].Reset;
end;

function TSpTBXSkinOptions.Options(Component: TSpTBXSkinComponentsType; State: TSpTBXSkinStatesType): TSpTBXSkinOptionCategory;
begin
  if CSpTBXSkinComponents[Component].States = [sknsNormal] then
    State := sknsNormal;
  Result := FOptions[Component, State];
end;

function TSpTBXSkinOptions.Options(Component: TSpTBXSkinComponentsType): TSpTBXSkinOptionCategory;
begin
  Result := FOptions[Component, sknsNormal];
end;

procedure TSpTBXSkinOptions.SaveToFile(Filename: string);
var
  MemIni: TMemIniFile;
begin
  MemIni := TMemIniFile.Create(Filename);
  try
    SaveToMemIni(MemIni);
    MemIni.UpdateFile;
  finally
    MemIni.Free;
  end;
end;

procedure TSpTBXSkinOptions.SaveToMemIni(MemIni: TMemIniFile);
var
  C: TSpTBXSkinComponentsType;
  S: TSpTBXSkinStatesType;
begin
  MemIni.WriteString('Skin', 'Name', FSkinName);
  MemIni.WriteString('Skin', 'Author', FSkinAuthor);
  MemIni.WriteString('Skin', 'ColorBtnFace', SpColorToString(FColorBtnFace));
  MemIni.WriteInteger('Skin', 'FloatingWindowBorderSize', FFloatingWindowBorderSize);
  MemIni.WriteBool('Skin', 'OfficeIcons', FOfficeIcons);
  MemIni.WriteBool('Skin', 'OfficeMenu', FOfficeMenu);
  MemIni.WriteBool('Skin', 'OfficeStatusBar', FOfficeStatusBar);

  for C := Low(C) to High(C) do begin
    for S := Low(S) to High(S) do
      if S in CSpTBXSkinComponents[C].States then
        FOptions[C, S].SaveToIni(MemIni, CSpTBXSkinComponents[C].Name, SSpTBXSkinStatesString[S]);
  end;
end;

procedure TSpTBXSkinOptions.SaveToStrings(L: TStrings);
var
  MemIni: TMemIniFile;
begin
  MemIni := TMemIniFile.Create('');
  try
    MemIni.SetStrings(L); // Transfer L contents to MemIni
    SaveToMemIni(MemIni);
    L.Clear;
    MemIni.GetStrings(L); // Transfer MemIni contents to L
  finally
    MemIni.Free;
  end;
end;

procedure TSpTBXSkinOptions.LoadFromFile(Filename: string);
var
  L: TStringList;
begin
  if FileExists(Filename) then begin
    L := TStringList.Create;
    try
      L.LoadFromFile(Filename);
      LoadFromStrings(L);
    finally
      L.Free;
    end;
  end;
end;

procedure TSpTBXSkinOptions.LoadFromStrings(L: TStrings);
var
  MemIni: TMemIniFile;
  C: TSpTBXSkinComponentsType;
  S: TSpTBXSkinStatesType;
begin
  MemIni := TMemIniFile.Create('');
  try
    MemIni.SetStrings(L);

    FSkinName := MemIni.ReadString('Skin', 'Name', '');
    FSkinAuthor := MemIni.ReadString('Skin', 'Author', '');
    FColorBtnFace := StringToColor(MemIni.ReadString('Skin', 'ColorBtnFace', 'clBtnFace'));
    FFloatingWindowBorderSize := MemIni.ReadInteger('Skin', 'FloatingWindowBorderSize', 4);
    FOfficeIcons := MemIni.ReadBool('Skin', 'OfficeIcons', False);
    FOfficeMenu := MemIni.ReadBool('Skin', 'OfficeMenu', False);
    FOfficeStatusBar := MemIni.ReadBool('Skin', 'OfficeStautsBar', False);

    for C := Low(C) to High(C) do begin
      for S := Low(S) to High(S) do
        if S in CSpTBXSkinComponents[C].States then
          FOptions[C, S].LoadFromIni(MemIni, CSpTBXSkinComponents[C].Name, SSpTBXSkinStatesString[S]);
    end;

    BroadcastChanges;
  finally
    MemIni.Free;
  end;
end;

function TSpTBXSkinOptions.GetOfficeIcons: Boolean;
// OfficeIcons is used to paint the menu items icons with Office XP shadows.
begin
  Result := FOfficeIcons and (SkinManager.GetSkinType = sknSkin);
end;

function TSpTBXSkinOptions.GetOfficeMenu: Boolean;
// When OfficeMenu is True the height of the separators on popup menus
// is 6 pixels, otherwise the size is 10 pixels.
// And when the item is disabled the hottrack is not painted.
begin
  Result := FOfficeMenu and (SkinManager.GetSkinType = sknSkin);
end;

function TSpTBXSkinOptions.GetOfficePopup: Boolean;
// OfficePopup is used to paint the PopupWindow with Office XP style.
// It is also used to paint the opened toolbar item with shadows.
begin
  Result := (SkinManager.GetSkinType = sknSkin) and not Options(skncOpenToolbarItem).IsEmpty;
end;

function TSpTBXSkinOptions.GetOfficeStatusBar: Boolean;
// OfficeStatusBar is used to paint the StatusBar panels with Office XP style.
var
  T: TSpTBXSkinType;
begin
  T := SkinManager.GetSkinType;
  Result := (FOfficeStatusBar and (T = sknSkin)) or (T = sknNone);
end;

function TSpTBXSkinOptions.GetFloatingWindowBorderSize: Integer;
begin
  if SkinManager.GetSkinType = sknSkin then
    Result := FFloatingWindowBorderSize
  else
    Result := 4;
end;

procedure TSpTBXSkinOptions.SetFloatingWindowBorderSize(const Value: Integer);
begin
  FFloatingWindowBorderSize := Value;
  if FFloatingWindowBorderSize < 0 then FFloatingWindowBorderSize := 0;
  if FFloatingWindowBorderSize > 4 then FFloatingWindowBorderSize := 4;
end;

procedure TSpTBXSkinOptions.GetDropDownArrowSize(out DropDownArrowSize,
  DropDownArrowMargin, SplitBtnArrowSize: Integer);
begin
  DropDownArrowSize := 8; // TB2Item.tbDropdownArrowWidth
  DropDownArrowMargin := 3; // TB2Item.tbDropdownArrowMargin

  SplitBtnArrowSize := 12; // TB2Item.tbDropdownComboArrowWidth + 1
  if SkinManager.GetSkinType in [sknWindows, sknDelphiStyle] then
    SplitBtnArrowSize := SplitBtnArrowSize + 1;
end;

procedure TSpTBXSkinOptions.GetMenuItemMargins(ACanvas: TCanvas; ImgSize: Integer;
  out MarginsInfo: TSpTBXMenuItemMarginsInfo);
var
  TextMetric: TTextMetric;
  H, M2: Integer;
  SkinType: TSpTBXSkinType;
begin
  if ImgSize = 0 then
    ImgSize := 16;

  FillChar(MarginsInfo, SizeOf(MarginsInfo), 0);
  SkinType := SkinManager.GetSkinType;

  if ((SkinType = sknWindows) and SpIsWinVistaOrUp) or (SkinType = sknDelphiStyle) then begin
    // Vista-like spacing
    MarginsInfo.Margins := Rect(1, 3, 1, 3); // MID_MENUITEM
    MarginsInfo.ImageTextSpace := 5 + 1;     // TMI_MENU_IMGTEXTSPACE
    MarginsInfo.LeftCaptionMargin := 3;      // TMI_MENU_LCAPTIONMARGIN
    MarginsInfo.RightCaptionMargin := 3;     // TMI_MENU_RCAPTIONMARGIN
  end
  else
    if (SkinType = sknSkin) then begin
      // Office-like spacing
      MarginsInfo.Margins := Rect(1, 3, 1, 3); // MID_MENUITEM
      MarginsInfo.ImageTextSpace := 5;         // TMI_MENU_IMGTEXTSPACE
      MarginsInfo.LeftCaptionMargin := 3;      // TMI_MENU_LCAPTIONMARGIN
      MarginsInfo.RightCaptionMargin := 3;     // TMI_MENU_RCAPTIONMARGIN
    end
    else begin
      MarginsInfo.Margins := Rect(0, 2, 0, 2); // MID_MENUITEM
      MarginsInfo.ImageTextSpace := 1;         // TMI_MENU_IMGTEXTSPACE
      MarginsInfo.LeftCaptionMargin := 2;      // TMI_MENU_LCAPTIONMARGIN
      MarginsInfo.RightCaptionMargin := 2;     // TMI_MENU_RCAPTIONMARGIN
    end;

  GetTextMetrics(ACanvas.Handle, TextMetric);
  M2 := MarginsInfo.Margins.Top + MarginsInfo.Margins.Bottom;
  MarginsInfo.GutterSize := TextMetric.tmHeight + TextMetric.tmExternalLeading + M2;
  H := ImgSize + M2;
  if H > MarginsInfo.GutterSize then MarginsInfo.GutterSize := H;
  MarginsInfo.GutterSize := (ImgSize + M2) * MarginsInfo.GutterSize div H;  // GutterSize = GetPopupMargin = ItemInfo.PopupMargin
end;

function TSpTBXSkinOptions.GetState(Enabled, Pushed, HotTrack, Checked: Boolean): TSpTBXSkinStatesType;
begin
  Result := sknsNormal;
  if not Enabled then Result := sknsDisabled
  else begin
    if Pushed then Result := sknsPushed
    else
      if HotTrack and Checked then Result := sknsCheckedAndHotTrack
      else
        if HotTrack then Result := sknsHotTrack
        else
          if Checked then Result := sknsChecked;
  end;
end;

procedure TSpTBXSkinOptions.GetState(State: TSpTBXSkinStatesType; out Enabled,
  Pushed, HotTrack, Checked: Boolean);
begin
  Enabled := State <> sknsDisabled;
  Pushed := State = sknsPushed;
  HotTrack := (State = sknsCheckedAndHotTrack) or (State = sknsHotTrack);
  Checked := (State = sknsCheckedAndHotTrack) or (State = sknsChecked);
end;

function TSpTBXSkinOptions.GetTextColor(Component: TSpTBXSkinComponentsType;
  State: TSpTBXSkinStatesType): TColor;
var
  Details: TThemedElementDetails;
  SkinType: TSpTBXSkinType;
begin
  Result := clNone;
  SkinType := SkinManager.GetSkinType;

  if SkinType = sknSkin then begin
    if State in CSpTBXSkinComponents[Component].States then begin
      Result := Options(Component, State).TextColor;
      if Result <> clNone then
        Exit; // Text color is specified by the skin
    end
    else
      Exit; // Exit if the State is not valid
  end;

  if SkinType = sknDelphiStyle then
  begin
    if State = sknsDisabled then Result :=  SpTBXThemeServices.GetSystemColor(clGrayText)
    else Result := SpTBXThemeServices.GetSystemColor(clBtnText);
  end
  else
  begin
    if State = sknsDisabled then Result := clGrayText
    else Result := clBtnText;
  end;

  case Component of
    skncMenuItem:
      if ((SkinType = sknWindows) and SpIsWinVistaOrUp) or (SkinType = sknDelphiStyle) then begin
        // Use the new API on Windows Vista
        if CurrentSkin.GetThemedElementDetails(Component, State, Details) then
          CurrentSkin.GetThemedElementTextColor(Details, Result);
      end
      else
        if State <> sknsDisabled then begin
          Result := clMenuText;
          if SkinType <> sknSkin then
            if State in [sknsHotTrack, sknsCheckedAndHotTrack, sknsPushed] then
              Result := clHighlightText;
        end;
    skncMenuBarItem:
      if ((SkinType = sknWindows) and SpIsWinVistaOrUp) or (SkinType = sknDelphiStyle) then begin
        // Use the new API on Windows Vista
        if CurrentSkin.GetThemedElementDetails(Component, State, Details) then
          CurrentSkin.GetThemedElementTextColor(Details, Result);
      end
      else
        if State <> sknsDisabled then begin
          Result := clMenuText;
          if SkinType = sknWindows then
            if State in [sknsHotTrack, sknsPushed, sknsChecked, sknsCheckedAndHotTrack] then
              Result := clHighlightText;
        end;
    skncToolbarItem:
      if SkinType = sknDelphiStyle then begin
        if CurrentSkin.GetThemedElementDetails(Component, State, Details) then
          CurrentSkin.GetThemedElementTextColor(Details, Result);
      end
      else
        if State <> sknsDisabled then Result := clMenuText;
    skncButton, skncCheckBox, skncRadioButton, skncLabel, skncTab, skncEditButton:
      if (SkinType = sknWindows) or (SkinType = sknDelphiStyle) then
        if CurrentSkin.GetThemedElementDetails(Component, State, Details) then
          CurrentSkin.GetThemedElementTextColor(Details, Result);
    skncListItem:
      if SkinType <> sknSkin then
        if SkinType = sknDelphiStyle then begin
          {$IF CompilerVersion >= 23} // for Delphi XE2 and up
          if State in [sknsChecked, sknsCheckedAndHotTrack] then
            Result := SpTBXThemeServices.GetStyleFontColor(sfListItemTextSelected)
          else
            Result := SpTBXThemeServices.GetStyleFontColor(sfListItemTextNormal);
          {$IFEND}
        end
        else
          if State in [sknsChecked, sknsCheckedAndHotTrack] then
            Result := clHighlightText;
    skncDockablePanelTitleBar:
      if SkinType = sknSkin then
        Result := GetTextColor(skncToolbarItem, State) // Use skncToolbarItem to get the default text color
      else
        if (SkinType = sknWindows) or (SkinType = sknDelphiStyle) then
          if CurrentSkin.GetThemedElementDetails(Component, State, Details) then
            CurrentSkin.GetThemedElementTextColor(Details, Result);
    skncStatusBar:
      if SkinType = sknSkin then
        Result := GetTextColor(skncToolbarItem, State) // Use skncToolbarItem to get the default text color
      else
        if (SkinType = sknWindows) or (SkinType = sknDelphiStyle) then begin
          Details := SpTBXThemeServices.GetElementDetails(tsPane);
          CurrentSkin.GetThemedElementTextColor(Details, Result);
        end;
    skncTabToolbar:
      if SkinType = sknSkin then
        Result := GetTextColor(skncToolbarItem, State) // Use skncToolbarItem to get the default text color
      else
        if (SkinType = sknWindows) or (SkinType = sknDelphiStyle) then
          Result := GetTextColor(skncTab, State);
    skncWindowTitleBar:
      if SkinType = sknSkin then
        Result := GetTextColor(skncToolbarItem, State)  // Use skncToolbarItem to get the default text color
      else
        if (SkinType = sknWindows) or (SkinType = sknDelphiStyle) then begin
          if CurrentSkin.GetThemedElementDetails(Component, State, Details) then
            CurrentSkin.GetThemedElementTextColor(Details, Result);
        end;
  end;
end;

function TSpTBXSkinOptions.GetThemedElementDetails(Component: TSpTBXSkinComponentsType;
  Enabled, Pushed, HotTrack, Checked, Focused, Defaulted, Grayed: Boolean; out Details: TThemedElementDetails): Boolean;
var
  SkinType: TSpTBXSkinType;
begin
  Result := False;
  SkinType := SkinManager.GetSkinType;

  case Component of
    skncDock:
      begin
        if SkinType = sknDelphiStyle then
          Details := SpTBXThemeServices.GetElementDetails(ttbToolBarDontCare)
        else
          Details := SpTBXThemeServices.GetElementDetails(trRebarDontCare);
        Result := True;
      end;
    skncDockablePanel:
      begin
        {$IF CompilerVersion >= 23}
        // tcpBackground is defined only on XE2 and up
        Details := SpTBXThemeServices.GetElementDetails(tcpBackground);
        Result := True;
        {$IFEND}
      end;
    skncDockablePanelTitleBar:
      begin
        {$IF CompilerVersion >= 23}
        // tcpThemedHeader is defined only on XE2 and up
        Details := SpTBXThemeServices.GetElementDetails(tcpThemedHeader);
        Result := True;
        {$IFEND}
      end;
    skncGutter:
      begin
        // [Old-Themes]
        {$IF CompilerVersion >= 23} //for Delphi XE2 and up
        Details := SpTBXThemeServices.GetElementDetails(tmPopupGutter);
        {$ELSE}
        Details.Element := teMenu;
        Details.Part := MENU_POPUPGUTTER;
        Details.State := 0;
        {$IFEND}
        Result := True;
      end;
    {
    skncMenuBar: ;
    skncOpenToolbarItem: ;
    }
    skncPanel:
      begin
        if Enabled then
          Details := SpTBXThemeServices.GetElementDetails(tbGroupBoxNormal)
        else
          Details := SpTBXThemeServices.GetElementDetails(tbGroupBoxDisabled);
        Result := True;
      end;
    skncPopup:
      begin
        // [Old-Themes]
        {$IF CompilerVersion >= 23} //for Delphi XE2 and up
        Details := SpTBXThemeServices.GetElementDetails(tmPopupBackground);
        {$ELSE}
        Details.Element := teMenu;
        Details.Part := MENU_POPUPBACKGROUND;
        Details.State := 0;
        {$IFEND}
        Result := True;
      end;
    skncSeparator:
      begin
        if Enabled then  // Enabled = Vertical
          Details := SpTBXThemeServices.GetElementDetails(ttbSeparatorNormal)
        else
          Details := SpTBXThemeServices.GetElementDetails(ttbSeparatorVertNormal);
        Result := True;
      end;
    {
    skncSplitter: ;
    }
    skncStatusBar:
      begin
        Details := SpTBXThemeServices.GetElementDetails(tsStatusRoot);
        Result := True;
      end;
    skncStatusBarGrip:
      begin
        Details := SpTBXThemeServices.GetElementDetails(tsGripper);
        Result := True;
      end;
    {
    skncTabBackground: ;
    skncTabToolbar: ;
    }
    skncToolbar:
      begin
        // On older versions is trBandNormal on XE2 is trBand
        {$IF CompilerVersion >= 23} // for Delphi XE2 and up
        Details := SpTBXThemeServices.GetElementDetails(trBand);
        {$ELSE}
        Details := SpTBXThemeServices.GetElementDetails(trBandNormal);
        {$IFEND}
        Result := True;
      end;
    skncToolbarGrip:
      begin
        if Enabled then  // Enabled = Vertical
          Details := SpTBXThemeServices.GetElementDetails(trGripperVert)
        else
          Details := SpTBXThemeServices.GetElementDetails(trGripper);
        Result := True;
      end;
    skncWindow: ;
    skncWindowTitleBar:
      begin
        if SkinType = sknDelphiStyle then begin
          if Enabled then
            Details := SpTBXThemeServices.GetElementDetails(twCaptionActive)
          else
            Details := SpTBXThemeServices.GetElementDetails(twCaptionInActive);
        end
        else
          // On WinXP when twCaptionActive is used instead of twSmallCaptionActive the top borders are rounded
          if Enabled then
            Details := SpTBXThemeServices.GetElementDetails(twSmallCaptionActive)
          else
            Details := SpTBXThemeServices.GetElementDetails(twSmallCaptionInActive);
        Result := True;
      end;
    skncMenuBarItem:
      begin
        if SpIsWinVistaOrUp or (SkinType = sknDelphiStyle) then begin
          // [Old-Themes]
          {$IF CompilerVersion >= 23} //for Delphi XE2 and up
          if not Enabled then Details := SpTBXThemeServices.GetElementDetails(tmMenuBarItemDisabled)
          else if Pushed then Details := SpTBXThemeServices.GetElementDetails(tmMenuBarItemPushed)
          else if HotTrack then Details := SpTBXThemeServices.GetElementDetails(tmMenuBarItemHot)
          else Details := SpTBXThemeServices.GetElementDetails(tmMenuBarItemNormal);
          {$ELSE}
          Details.Element := teMenu;
          Details.Part := MENU_BARITEM;
          if not Enabled then Details.State := MBI_DISABLED
          else if Pushed then Details.State := MBI_PUSHED
          else if HotTrack then Details.State := MBI_HOT
          else Details.State := MBI_NORMAL;
          {$IFEND}
        end
        else
          Details := SpTBXThemeServices.GetElementDetails(tmMenuBarItem);
        Result := True;
      end;
    skncMenuItem:
      begin
        if SpIsWinVistaOrUp or (SkinType = sknDelphiStyle) then begin
          // [Old-Themes]
          {$IF CompilerVersion >= 23} //for Delphi XE2 and up
          if not Enabled and HotTrack then Details := SpTBXThemeServices.GetElementDetails(tmPopupItemDisabledHot)
          else if not Enabled then Details := SpTBXThemeServices.GetElementDetails(tmPopupItemDisabled)
          else if HotTrack then Details := SpTBXThemeServices.GetElementDetails(tmPopupItemHot)
          else Details := SpTBXThemeServices.GetElementDetails(tmPopupItemNormal);
          {$ELSE}
          Details.Element := teMenu;
          Details.Part := MENU_POPUPITEM;
          if not Enabled and HotTrack then Details.State := MPI_DISABLEDHOT
          else if not Enabled then Details.State := MPI_DISABLED
          else if HotTrack then Details.State := MPI_HOT
          else Details.State := MPI_NORMAL;
          {$IFEND}
        end
        else begin
          if HotTrack then
            Details := SpTBXThemeServices.GetElementDetails(tmMenuItemSelected)
          else
            Details := SpTBXThemeServices.GetElementDetails(tmMenuItemNormal);
        end;
        Result := True;
      end;
    skncToolbarItem:
      begin
        if not Enabled then Details := SpTBXThemeServices.GetElementDetails(ttbButtonDisabled)
        else if Pushed then Details := SpTBXThemeServices.GetElementDetails(ttbButtonPressed)
        else if HotTrack and Checked then Details := SpTBXThemeServices.GetElementDetails(ttbButtonCheckedHot)
        else if HotTrack then Details := SpTBXThemeServices.GetElementDetails(ttbButtonHot)
        else if Checked then Details := SpTBXThemeServices.GetElementDetails(ttbButtonChecked)
        else Details := SpTBXThemeServices.GetElementDetails(ttbButtonNormal);
        Result := True;
      end;
    skncButton, skncEditButton:
      begin
        if not Enabled then Details := SpTBXThemeServices.GetElementDetails(tbPushButtonDisabled)
        else if Pushed or Checked then Details := SpTBXThemeServices.GetElementDetails(tbPushButtonPressed)
        else if HotTrack then Details := SpTBXThemeServices.GetElementDetails(tbPushButtonHot)
        else if Defaulted or Focused then Details := SpTBXThemeServices.GetElementDetails(tbPushButtonDefaulted)
        else Details := SpTBXThemeServices.GetElementDetails(tbPushButtonNormal);
        Result := True;
      end;
    skncCheckBox:
      begin
        if Grayed then begin
          if not Enabled then Details := SpTBXThemeServices.GetElementDetails(tbCheckBoxMixedDisabled)
          else if Pushed then Details := SpTBXThemeServices.GetElementDetails(tbCheckBoxMixedPressed)
          else if HotTrack then Details := SpTBXThemeServices.GetElementDetails(tbCheckBoxMixedHot)
          else Details := SpTBXThemeServices.GetElementDetails(tbCheckBoxMixedNormal);
        end
        else
          if Checked then begin
            if not Enabled then Details := SpTBXThemeServices.GetElementDetails(tbCheckBoxCheckedDisabled)
            else if Pushed then Details := SpTBXThemeServices.GetElementDetails(tbCheckBoxCheckedPressed)
            else if HotTrack then Details := SpTBXThemeServices.GetElementDetails(tbCheckBoxCheckedHot)
            else Details := SpTBXThemeServices.GetElementDetails(tbCheckBoxCheckedNormal);
          end
          else begin
            if not Enabled then Details := SpTBXThemeServices.GetElementDetails(tbCheckBoxUncheckedDisabled)
            else if Pushed then Details := SpTBXThemeServices.GetElementDetails(tbCheckBoxUncheckedPressed)
            else if HotTrack then Details := SpTBXThemeServices.GetElementDetails(tbCheckBoxUncheckedHot)
            else Details := SpTBXThemeServices.GetElementDetails(tbCheckBoxUncheckedNormal);
          end;
        Result := True;
      end;
    {
    skncEditFrame: ;
    skncHeader: ;
    }
    skncHeader:
      begin
        if Pushed then Details := SpTBXThemeServices.GetElementDetails(thHeaderItemPressed)
        else if HotTrack then Details := SpTBXThemeServices.GetElementDetails(thHeaderItemHot)
        else Details := SpTBXThemeServices.GetElementDetails(thHeaderItemNormal);
        Result := True;
      end;
    skncLabel:
      begin
        if Enabled then
          Details := SpTBXThemeServices.GetElementDetails(tbCheckBoxUncheckedNormal)
        else
          Details := SpTBXThemeServices.GetElementDetails(tbCheckBoxUncheckedDisabled);
        Result := True;
      end;
    {
    skncListItem: ;
    }
    skncRadioButton:
      begin
        if Checked then begin
          if not Enabled then Details := SpTBXThemeServices.GetElementDetails(tbRadioButtonCheckedDisabled)
          else if Pushed then Details := SpTBXThemeServices.GetElementDetails(tbRadioButtonCheckedPressed)
          else if HotTrack then Details := SpTBXThemeServices.GetElementDetails(tbRadioButtonCheckedHot)
          else Details := SpTBXThemeServices.GetElementDetails(tbRadioButtonCheckedNormal);
        end
        else begin
          if not Enabled then Details := SpTBXThemeServices.GetElementDetails(tbRadioButtonUncheckedDisabled)
          else if Pushed then Details := SpTBXThemeServices.GetElementDetails(tbRadioButtonUncheckedPressed)
          else if HotTrack then Details := SpTBXThemeServices.GetElementDetails(tbRadioButtonUncheckedHot)
          else Details := SpTBXThemeServices.GetElementDetails(tbRadioButtonUncheckedNormal);
        end;
        Result := True;
      end;
    skncTab:
      begin
        if not Enabled then Details := SpTBXThemeServices.GetElementDetails(ttTabItemDisabled)
        else if Pushed or (HotTrack and Checked) or Checked then Details := SpTBXThemeServices.GetElementDetails(ttTabItemSelected)
        else if HotTrack then Details := SpTBXThemeServices.GetElementDetails(ttTabItemHot)
        else Details := SpTBXThemeServices.GetElementDetails(ttTabItemNormal);
        Result := True;
      end;
    skncProgressBar:    ; // not used
    skncTrackBar:       ; // not used
    skncTrackBarButton: ; // not used
  end;
end;

function TSpTBXSkinOptions.GetThemedElementDetails(Component: TSpTBXSkinComponentsType;
  State: TSpTBXSkinStatesType; out Details: TThemedElementDetails): Boolean;
var
  Enabled, Pushed, HotTrack, Checked: Boolean;
begin
  GetState(State, Enabled, Pushed, HotTrack, Checked);
  Result := GetThemedElementDetails(Component, Enabled, Pushed, HotTrack, Checked, False, False, False, Details);
end;

function TSpTBXSkinOptions.GetThemedElementSize(ACanvas: TCanvas; Details: TThemedElementDetails): TSize;
begin
  {$IF CompilerVersion >= 23} // for Delphi XE2 and up
  SpTBXThemeServices.GetElementSize(ACanvas.Handle, Details, esActual, Result);
  {$ELSE}
  GetThemePartSize(SpTBXThemeServices.Theme[Details.Element], ACanvas.Handle, Details.Part, Details.State, nil, TS_TRUE, Result);
  {$IFEND}
end;

procedure TSpTBXSkinOptions.GetThemedElementTextColor(Details: TThemedElementDetails; out AColor: TColor);
begin
  {$IF CompilerVersion >= 23} // for Delphi XE2 and up
  SpTBXThemeServices.GetElementColor(Details, ecTextColor, AColor);
  {$ELSE}
  GetThemeColor(SpTBXThemeServices.Theme[Details.Element], Details.Part, Details.State,
      TMT_TEXTCOLOR, TColorRef(AColor));
  {$IFEND}
end;

function TSpTBXSkinOptions.GetThemedSystemColor(AColor: TColor): TColor;
begin
  {$IF CompilerVersion >= 23} // for Delphi XE2 and up
  Result := SpTBXThemeServices.GetSystemColor(AColor);
  {$ELSE}
  Result := AColor;
  {$IFEND}
end;

procedure TSpTBXSkinOptions.PaintBackground(ACanvas: TCanvas; ARect: TRect;
  Component: TSpTBXSkinComponentsType; State: TSpTBXSkinStatesType;
  Background, Borders: Boolean; Vertical: Boolean = False;
  ForceRectBorders: TAnchors = []);
var
  BackgroundRect: TRect;
  Op: TSpTBXSkinOptionCategory;
begin
  Op := Options(Component, State);

  if Op.Borders.IsEmpty then
    Borders := False;
  if Op.Body.IsEmpty then
    Background := False;

  if Background then begin
    BackgroundRect := ARect;
    if Borders then
      InflateRect(BackgroundRect, -1, -1);
    SpPaintSkinBackground(ACanvas, BackgroundRect, Op, Vertical);
  end;

  if Borders then
    SpPaintSkinBorders(ACanvas, ARect, Op, ForceRectBorders);
end;

procedure TSpTBXSkinOptions.PaintThemedElementBackground(ACanvas: TCanvas;
  ARect: TRect; Details: TThemedElementDetails);
var
  SaveIndex: Integer;
begin
  SaveIndex := SaveDC(ACanvas.Handle);  // XE2 Styles changes the font
  try
    SpTBXThemeServices.DrawElement(ACanvas.Handle, Details, ARect, nil);
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;
end;

procedure TSpTBXSkinOptions.PaintThemedElementBackground(ACanvas: TCanvas;
  ARect: TRect; Component: TSpTBXSkinComponentsType; Enabled, Pushed, HotTrack,
  Checked, Focused, Defaulted, Grayed: Boolean);
var
  Details: TThemedElementDetails;
begin
  if CurrentSkin.GetThemedElementDetails(Component, Enabled, Pushed, HotTrack, Checked, Focused, Defaulted, Grayed, Details) then
    PaintThemedElementBackground(ACanvas, ARect, Details);
end;

procedure TSpTBXSkinOptions.PaintThemedElementBackground(ACanvas: TCanvas;
  ARect: TRect; Component: TSpTBXSkinComponentsType;
  State: TSpTBXSkinStatesType);
var
  Details: TThemedElementDetails;
begin
  if CurrentSkin.GetThemedElementDetails(Component, State, Details) then
    PaintThemedElementBackground(ACanvas, ARect, Details);
end;

procedure TSpTBXSkinOptions.PaintMenuCheckMark(ACanvas: TCanvas; ARect: TRect;
  Checked, Grayed, MenuItemStyle: Boolean; State: TSpTBXSkinStatesType);
var
  X, Y: Integer;
  PenC, BrushC, CheckColor: TColor;
  VistaCheckSize: TSize;
  Details: TThemedElementDetails;
  SkinType: TSpTBXSkinType;
begin
  SkinType := SkinManager.GetSkinType;
  if MenuItemStyle and ((SkinType = sknWindows) and SpIsWinVistaOrUp) or (SkinType = sknDelphiStyle) then begin
    // [Old-Themes]
    {$IF CompilerVersion >= 23} //for Delphi XE2 and up
    if State = sknsDisabled then Details := SpTBXThemeServices.GetElementDetails(tmPopupCheckDisabled)
    else Details := SpTBXThemeServices.GetElementDetails(tmPopupCheckNormal);
    {$ELSE}
    Details.Element := teMenu;
    Details.Part := MENU_POPUPCHECK;
    if State = sknsDisabled then Details.State := MC_CHECKMARKDISABLED
    else Details.State := MC_CHECKMARKNORMAL;
    {$IFEND}
    VistaCheckSize := CurrentSkin.GetThemedElementSize(ACanvas, Details);
    ARect := SpCenterRect(ARect, VistaCheckSize.cx, VistaCheckSize.cy);
    CurrentSkin.PaintThemedElementBackground(ACanvas, ARect, Details);
  end
  else begin
    X := ARect.Left + (ARect.Right - ARect.Left) div 2 - 1;
    Y := ARect.Top + (ARect.Bottom - ARect.Top) div 2 + 1;

    PenC := ACanvas.Pen.Color;
    BrushC := ACanvas.Brush.Color;
    try
      if MenuItemStyle then begin
        CheckColor := clMenuText; // On sknNone it's clMenuText even when disabled
        case SkinManager.GetSkinType of
          sknWindows:
            CheckColor := GetTextColor(skncCheckBox, State);
          sknSkin:
            CheckColor := GetTextColor(skncMenuItem, State);
        end;
        ACanvas.Brush.Color := CheckColor;
        ACanvas.Pen.Color := CheckColor;
        ACanvas.Polygon([Point(X - 3, Y - 2), Point(X - 1, Y), Point(X + 3, Y - 4),
          Point(X + 3, Y - 3), Point(X - 1, Y + 1), Point(X - 3, Y - 1), Point(X - 3, Y -2)]);
      end
      else begin
        CheckColor := GetTextColor(skncCheckBox, State);
        ACanvas.Brush.Color := CheckColor;
        ACanvas.Pen.Color := CheckColor;
        PaintBackground(ACanvas, ARect, skncCheckBox, State, True, True);
        if Checked then
          ACanvas.Polygon([Point(X - 2, Y), Point(X, Y + 2), Point(X + 4, Y - 2),
            Point(X + 4, Y - 4), Point(X, Y), Point(X - 2, Y - 2), Point(X - 2, Y)])
        else
          if Grayed then begin
            InflateRect(ARect, -3, -3);
            // ACanvas.Brush.Color := Options(skncCheckBox, sknsChecked).Borders.Color1;
            ACanvas.FillRect(ARect);
          end;
      end;
    finally
      ACanvas.Pen.Color := PenC;
      ACanvas.Brush.Color := BrushC;
    end;
  end;
end;

procedure TSpTBXSkinOptions.PaintMenuRadioMark(ACanvas: TCanvas; ARect: TRect;
  Checked, MenuItemStyle: Boolean; State: TSpTBXSkinStatesType);
var
  X, Y: Integer;
  PenC, BrushC, CheckColor, FrameColor: TColor;
  VistaCheckSize: TSize;
  Details: TThemedElementDetails;
  SkinType: TSpTBXSkinType;
begin
  SkinType := SkinManager.GetSkinType;
  if MenuItemStyle and ((SkinType = sknWindows) and SpIsWinVistaOrUp) or (SkinType = sknDelphiStyle) then begin
    // [Old-Themes]
    {$IF CompilerVersion >= 23} //for Delphi XE2 and up
    if State = sknsDisabled then Details := SpTBXThemeServices.GetElementDetails(tmPopupBulletDisabled)
    else Details := SpTBXThemeServices.GetElementDetails(tmPopupBulletNormal);
    {$ELSE}
    Details.Element := teMenu;
    Details.Part := MENU_POPUPCHECK;
    if State = sknsDisabled then Details.State := MC_BULLETDISABLED
    else Details.State := MC_BULLETNORMAL;
    {$IFEND}
    VistaCheckSize := CurrentSkin.GetThemedElementSize(ACanvas, Details);
    ARect := SpCenterRect(ARect, VistaCheckSize.cx, VistaCheckSize.cy);
    CurrentSkin.PaintThemedElementBackground(ACanvas, ARect, Details);
  end
  else begin
    PenC := ACanvas.Pen.Color;
    BrushC := ACanvas.Brush.Color;
    try
      if MenuItemStyle then begin
        CheckColor := clMenuText; // On sknNone it's clMenuText even when disabled
        case SkinManager.GetSkinType of
          sknWindows:
            CheckColor := GetTextColor(skncRadioButton, State);
          sknSkin:
            CheckColor := GetTextColor(skncMenuItem, State);
        end;
        ACanvas.Brush.Color := CheckColor;
        ACanvas.Pen.Color := CheckColor;
        X := ARect.Left + (ARect.Right - ARect.Left) div 2;
        Y := ARect.Top + (ARect.Bottom - ARect.Top) div 2;
        ACanvas.RoundRect(X - 3, Y - 3, X + 3, Y + 3, 2, 2);
      end
      else begin
        CheckColor := GetTextColor(skncRadioButton, State);
        FrameColor := Options(skncRadioButton, State).Borders.Color1;
        if not Checked then CheckColor := clNone;

        // Keep it simple make the radio 13x13
        ARect.Left := ARect.Left + (ARect.Right - ARect.Left - 13) div 2;
        ARect.Right := ARect.Left + 13;
        ARect.Top := ARect.Top + (ARect.Bottom - ARect.Top - 13) div 2;
        ARect.Bottom := ARect.Top + 13;
        X := ARect.Left;
        Y := ARect.Top;

        // Background
        BeginPath(ACanvas.Handle);
        ACanvas.Polyline([Point(X, Y + 8), Point(X, Y + 4), Point(X + 1, Y + 3),
            Point(X + 1, Y + 2), Point(X + 2, Y + 1), Point(X + 3, Y + 1),
              Point(X + 4, Y), Point(X + 8, Y), Point(X + 9, Y + 1),
              Point(X + 10, Y + 1), Point(X + 11, Y + 2), Point(X + 11, Y + 3),
              Point(X + 12, Y + 4), Point(X + 12, Y + 8), Point(X + 11, Y + 9),
              Point(X + 11, Y + 10), Point(X + 10, Y + 11), Point(X + 9, Y + 11),
              Point(X + 8, Y + 12), Point(X + 4, Y + 12), Point(X + 3, Y + 11),
              Point(X + 2, Y + 11), Point(X + 1, Y + 10), Point(X + 1, Y + 8)]);
        EndPath(ACanvas.Handle);
        SelectClipPath(ACanvas.Handle, RGN_COPY);
        PaintBackground(ACanvas, ARect, skncRadioButton, State, True, False);
        SelectClipPath(ACanvas.Handle, 0);
        SelectClipRgn(ACanvas.Handle, 0);

        // Frame
        ACanvas.Brush.Color := FrameColor;
        ACanvas.Pen.Color := FrameColor;
        ACanvas.Polyline([Point(X, Y + 8), Point(X, Y + 4), Point(X + 1, Y + 3),
            Point(X + 1, Y + 2), Point(X + 2, Y + 1), Point(X + 3, Y + 1),
              Point(X + 4, Y), Point(X + 8, Y), Point(X + 9, Y + 1),
              Point(X + 10, Y + 1), Point(X + 11, Y + 2), Point(X + 11, Y + 3),
              Point(X + 12, Y + 4), Point(X + 12, Y + 8), Point(X + 11, Y + 9),
              Point(X + 11, Y + 10), Point(X + 10, Y + 11), Point(X + 9, Y + 11),
              Point(X + 8, Y + 12), Point(X + 4, Y + 12), Point(X + 3, Y + 11),
              Point(X + 2, Y + 11), Point(X + 1, Y + 10), Point(X + 1, Y + 8)]);

        // Radio
        if CheckColor <> clNone then begin
          ACanvas.Brush.Color := CheckColor;
          ACanvas.Pen.Color := CheckColor;
          X := (ARect.Left + ARect.Right) div 2;
          Y := (ARect.Top + ARect.Bottom) div 2 + 1;
          ACanvas.RoundRect(X - 2, Y - 3, X + 3, Y + 2, 2, 2);
        end;
      end;
    finally
      ACanvas.Pen.Color := PenC;
      ACanvas.Brush.Color := BrushC;
    end;
  end;
end;

procedure TSpTBXSkinOptions.PaintWindowFrame(ACanvas: TCanvas; ARect: TRect;
  IsActive, DrawBody: Boolean; BorderSize: Integer = 4);
var
  C: TColor;
  R: TRect;
  I: Integer;
  State: TSpTBXSkinStatesType;
  Op: TSpTBXSkinOptionEntry;
begin
  if IsActive then
    State := sknsNormal
  else
    if Options(skncWindow, sknsDisabled).IsEmpty then
      State := sknsNormal
    else
      State := sknsDisabled;

  C := ACanvas.Brush.Color;
  if DrawBody then
    PaintBackground(ACanvas, ARect, skncWindow, State, True, False);
  R := ARect;
  Op := Options(skncWindow, State).Borders;
  for I := 1 to BorderSize do begin
    if I = 1 then ACanvas.Brush.Color := Op.Color1
    else if I = 2 then ACanvas.Brush.Color := Op.Color2
    else if I = 3 then ACanvas.Brush.Color := Op.Color3
    else if I >= 4 then ACanvas.Brush.Color := Op.Color4;
    ACanvas.FrameRect(R);
    InflateRect(R, -1, -1);
  end;
  ACanvas.Brush.Color := C;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXSkinsListEntry }

destructor TSpTBXSkinsListEntry.Destroy;
begin
  SkinClass := nil;
  FreeAndNil(SkinStrings);

  inherited;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXSkinsList }

destructor TSpTBXSkinsList.Destroy;
begin
  // Free all the skins options
  while Count > 0 do
    Delete(0);
  inherited;
end;

function TSpTBXSkinsList.AddSkin(SkinName: string; SkinClass: TSpTBXSkinOptionsClass): Integer;
var
  K: TSpTBXSkinsListEntry;
begin
  Result := -1;
  if (SkinName <> '') and (IndexOf(SkinName) = -1) then begin
    K := TSpTBXSkinsListEntry.Create;
    try
      K.SkinClass := SkinClass;
      Result := AddObject(SkinName, K);  // the list owns K
    except
      K.Free;
    end;
  end;
end;

function TSpTBXSkinsList.AddSkin(SkinOptions: TStrings): Integer;
var
  K: TSpTBXSkinsListEntry;
  S: string;
begin
  Result := -1;
  K := TSpTBXSkinsListEntry.Create;
  try
    K.SkinStrings := TStringList.Create;
    S := SkinOptions.Values['Name '];
    if S = '' then
      S := SkinOptions.Values['Name'];
    S := Trim(S);
    if (S <> '') and  (IndexOf(S) = -1) then begin
      K.SkinStrings.Assign(SkinOptions);
      Result := AddObject(S, K);  // the list owns K
    end
    else
      K.Free;
  except
    K.Free;
  end;
end;

function TSpTBXSkinsList.AddSkinFromFile(Filename: string): Integer;
var
  L: TStringList;
begin
  L := TStringList.Create;
  try
    L.LoadFromFile(Filename);
    Result := AddSkin(L);
  finally
    L.Free;
  end;
end;

procedure TSpTBXSkinsList.AddSkinsFromFolder(Folder: string);
var
  L: TStringList;
  I: Integer;
  S: string;
begin
  L := TStringList.Create;
  try
    if SpGetDirectories(Folder, L) then begin
      for I := 0 to L.Count - 1 do begin
        S := IncludeTrailingPathDelimiter(Folder) + L[I] + '\Skin.ini';
        if FileExists(S) then
          AddSkinFromFile(S);
      end;
    end;
  finally
    L.Free;
  end;
end;

procedure TSpTBXSkinsList.Delete(Index: Integer);
begin
  if (Index > -1) and (Index < Count) then
    SkinOptions[Index].Free;
  inherited Delete(Index);
end;

procedure TSpTBXSkinsList.GetSkinNames(SkinNames: TStrings);
var
  I: Integer;
begin
  SkinNames.BeginUpdate;
  try
    SkinNames.Clear;
    SkinNames.Add('Default');
    for I := 0 to Count - 1 do
      SkinNames.Add(Strings[I]);
  finally
    SkinNames.EndUpdate;
  end;
end;

function TSpTBXSkinsList.GetSkinOption(Index: Integer): TSpTBXSkinsListEntry;
begin
  Result := TSpTBXSkinsListEntry(Objects[Index]);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXSkinManager }

constructor TSpTBXSkinManager.Create;
begin
  FNotifies := TList.Create;
  FCurrentSkin := TSpTBXSkinOptions.Create;
  FSkinsList := TSpTBXSkinsList.Create;
end;

destructor TSpTBXSkinManager.Destroy;
begin
  FreeAndNil(FNotifies);
  FreeAndNil(FCurrentSkin);
  FreeAndNil(FSkinsList);
  inherited;
end;

procedure TSpTBXSkinManager.AddSkinNotification(AObject: TObject);
begin
  if FNotifies.IndexOf(AObject) < 0 then FNotifies.Add(AObject);
end;

procedure TSpTBXSkinManager.RemoveSkinNotification(AObject: TObject);
begin
  FNotifies.Remove(AObject);
end;

procedure TSpTBXSkinManager.ResetDelphiStyle;
begin
  {$IF CompilerVersion >= 23} // for Delphi XE2 and up
  if TStyleManager.IsCustomStyleActive then
    if TStyleManager.ActiveStyle <> TStyleManager.SystemStyle then
      TStyleManager.SetStyle(TStyleManager.SystemStyle);
  {$IFEND}
end;

procedure TSpTBXSkinManager.Broadcast;
var
  Msg: TMessage;
  I: Integer;
begin
  if FNotifies.Count > 0 then begin
    Msg.Msg := WM_SPSKINCHANGE;
    Msg.WParam := 0;
    Msg.LParam := 0;
    Msg.Result := 0;
    for I := 0 to FNotifies.Count - 1 do
      TObject(FNotifies[I]).Dispatch(Msg);
  end;
  if Assigned(FOnSkinChange) then FOnSkinChange(Self);
end;

procedure TSpTBXSkinManager.BroadcastSkinNotification;
begin
  Broadcast;
end;

procedure TSpTBXSkinManager.LoadFromFile(Filename: string);
begin
  FCurrentSkin.LoadFromFile(Filename);
end;

procedure TSpTBXSkinManager.SaveToFile(Filename: string);
begin
  FCurrentSkin.SaveToFile(Filename);
end;

function TSpTBXSkinManager.GetCurrentSkinName: string;
begin
  Result := FCurrentSkin.SkinName;
end;

function TSpTBXSkinManager.GetSkinType: TSpTBXSkinType;
begin
  Result := sknSkin;
  if (Result = sknSkin) and IsDefaultSkin then begin
    Result := sknWindows;
    {$IF CompilerVersion >= 23} // for Delphi XE2 and up
    if TStyleManager.IsCustomStyleActive then
      Result := sknDelphiStyle;
    {$IFEND}
  end;

  if (Result = sknWindows) and not SkinManager.IsXPThemesEnabled then
    Result := sknNone;
end;

function TSpTBXSkinManager.IsDefaultSkin: Boolean;
begin
  Result := CurrentSkinName = 'Default';
end;

function TSpTBXSkinManager.IsXPThemesEnabled: Boolean;
begin
  {$IF CompilerVersion >= 23} //for Delphi XE2 and up
  Result := StyleServices.Enabled;
  {$ELSE}
  Result := ThemeServices.ThemesAvailable and UxTheme.UseThemes;
  {$IFEND}
end;

procedure TSpTBXSkinManager.SetSkin(SkinName: string);
var
  I: Integer;
  K: TSpTBXSkinsListEntry;
begin
  if not SameText(SkinName, CurrentSkinName) then
    if (SkinName = '') or SameText(SkinName, 'Default') then
      SetToDefaultSkin
    else begin
      I := FSkinsList.IndexOf(SkinName);
      if I > -1 then begin
        K := FSkinsList.SkinOptions[I];
        if Assigned(K.SkinClass) then begin
          FCurrentSkin.Free;
          FCurrentSkin := K.SkinClass.Create;
          ResetDelphiStyle;
          Broadcast;
        end
        else
          if Assigned(K.SkinStrings) then begin
            FCurrentSkin.Free;
            FCurrentSkin := TSpTBXSkinOptions.Create;
            FCurrentSkin.LoadFromStrings(K.SkinStrings);
            ResetDelphiStyle;
            Broadcast;
          end;
      end;
    end;
end;

procedure TSpTBXSkinManager.SetToDefaultSkin;
begin
  if not IsDefaultSkin then begin
    FCurrentSkin.Free;
    FCurrentSkin := TSpTBXSkinOptions.Create;
    ResetDelphiStyle;
    Broadcast;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXSkinSwitcher }

constructor TSpTBXSkinSwitcher.Create(AOwner: TComponent);
begin
  inherited;
  SkinManager.AddSkinNotification(Self);
end;

destructor TSpTBXSkinSwitcher.Destroy;
begin
  SkinManager.RemoveSkinNotification(Self);
  inherited;
end;

function TSpTBXSkinSwitcher.GetSkin: string;
begin
  Result := SkinManager.CurrentSkinName;
end;

procedure TSpTBXSkinSwitcher.SetSkin(const Value: string);
begin
  SkinManager.SetSkin(Value);
end;

procedure TSpTBXSkinSwitcher.WMSpSkinChange(var Message: TMessage);
begin
  if Assigned(FOnSkinChange) then FOnSkinChange(Self);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Stock Objects }

procedure InitializeStock;
begin
  StockBitmap := TBitmap.Create;
  StockBitmap.Width := 8;
  StockBitmap.Height := 8;

  @SpPrintWindow := GetProcAddress(GetModuleHandle(user32), 'PrintWindow');

  if not Assigned(FInternalSkinManager) then
    FInternalSkinManager := TSpTBXSkinManager.Create;
end;

procedure FinalizeStock;
begin
  FreeAndNil(StockBitmap);
  FreeAndNil(FInternalSkinManager);
end;

initialization
  InitializeStock;

finalization
  FinalizeStock;

end.
