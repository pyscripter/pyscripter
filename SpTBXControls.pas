unit SpTBXControls;

{==============================================================================
Version 1.8.3

The contents of this file are subject to the SpTBXLib License; you may
not use or distribute this file except in compliance with the
SpTBXLib License.
A copy of the SpTBXLib License may be found in SpTBXLib-LICENSE.txt or at:
  http://club.telepolis.com/silverpointdev/sptbxlib/SpTBXLib-LICENSE.htm

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
  - Alex Denisov's TBX
    http://g32.org
  - Troy Wolbrink's TNT Unicode Controls
    http://www.tntware.com/delphicontrols/unicode/

Wish list for TBX:

Development notes:
  - All the TBX theme changes and adjustments are marked with '[TBXTheme-Change]'.

History:
8 February 2007 - version 1.8.3
  - Added GripHotTrack property to TSpTBXSplitter.

17 December 2006 - version 1.8.2
  - Added AutoSize property to TSpTBXPanel.
  - Fixed incorrect resizing behavior on TSpTBXSplitter when a
    DockablePanel was adjacent. 

24 November 2006 - version 1.8.1
  - Improved TSpTBXPanel painting, thanks to Jim Kueneman for
    his code donation.
  - Fixed incorrect focus behavior on TSpTBXRadioButton when used
    on a groupbox, thanks to Andrew for reporting this.

27 August 2006 - version 1.8
  - Added DropDownArrow property to TSpTBXButton and TSpTBXSpeedButton.
  - Fixed incorrect TSpTBXGroupBox painting when changing the
    Enabled property, thanks to Tomaz Kunaver for reporting this.

15 June 2006 - version 1.7
  - Fixed incorrect TSpTBXButton painting when using a bitmap
    skin and the DropDownMenu is shown, thanks to Boris Yankov
    for reporting this.

4 May 2006 - version 1.6
  - New component added, TSpTBXRadioGroup.

12 April 2006 - version 1.5
  - No changes.

27 February 2006 - version 1.4
  - Added GroupIndex property to TSpTBXButton and TSpTBXSpeedButton.

10 February 2006 - version 1.3
  - New component added, TSpTBXSpeedButton.
  - New component added, TSpTBXSplitter.
  - Fixed incorrect TSpTBXButton behavior when trying to close the
    DropDownMenu clicking the button, thanks to Alexey Naumov for
    reporting this.

==============================================================================}

interface

{$BOOLEVAL OFF} // Unit depends on short-circuit boolean evaluation

uses
  Windows, Messages, Classes, SysUtils, Controls, Graphics, ImgList, Forms,
  Menus, StdCtrls, ExtCtrls, ComCtrls, ActnList,
  TB2Dock, TB2Toolbar, TB2Item, TBX, TBXDkPanels, TBXThemes,
  SpTBXItem, SpTBXDkPanels,
  TntClasses, TntControls;

const
  ConstStatesCount = 4;        // Buttons have 4 states (normal, hottrack, pushed, disabled)
  ConstInitRepeatPause = 400;  // Delay of the first repeated click (ms)
  ConstRepeatPause     = 100;  // Interval of the repeated clicks (ms)
  CM_SPGROUPINDEXUPDATE = CM_BASE + 2222;  // Message sent to the controls to update its state based on the GroupIndex
  CM_SPTBXCONTROLSINVALIDATE = CM_BASE + 3333;  // Message sent to TBX controls to invalidate the background

type
  TSpTBXTextObject = class;

  TSpTBXPanelBorder = (
    pbrRaised,
    pbrDoubleRaised,
    pbrSunken,
    pbrDoubleSunken,
    pbrBumped,
    pbrEtched,
    pbrFramed
  );

  TSpTBXProgressCaption = (
    pctNone,
    pctDefault,
    pctPercentage,
    pctProgress
  );

  TSpTBXTickMark = (
    tmxBottomRight,
    tmxTopLeft,
    tmxBoth,
    tmxCenter
  );

  TSpTBXCanResizeEvent = procedure(Sender: TObject; var NewSize: Integer; var Accept: Boolean) of object;

  { TSpTBXPanel }

  TSpTBXCustomPanel = class(TCustomControl)
  private
    FBackground: TBitmap;
    FBackgroundValid: Boolean;
    FBorders: Boolean;
    FBorderType: TSpTBXPanelBorder;
    FMargins: TTBXControlMargins;
    FTBXStyleBackground: Boolean;
    FThemeType: TSpTBXThemeType;
    FOnDrawBackground: TSpTBXDrawEvent;
    procedure EraseTBXBackground(ACanvas: TCanvas; ARect: TRect);
    function GetCaption: TWideCaption;
    function GetHint: WideString;
    function IsCaptionStored: Boolean;
    function IsHintStored: Boolean;
    procedure MarginsChangeHandler(Sender: TObject);
    procedure SetBorders(const Value: Boolean);
    procedure SetBorderType(const Value: TSpTBXPanelBorder);
    procedure SetCaption(const Value: TWideCaption);
    procedure SetHint(const Value: WideString);
    procedure SetMargins(Value: TTBXControlMargins);
    procedure SetTBXStyleBackground(const Value: Boolean);
    procedure SetThemeType(const Value: TSpTBXThemeType);
    procedure CMSpTBXControlsInvalidate(var Message: TMessage); message CM_SPTBXCONTROLSINVALIDATE;
    procedure TBMThemeChange(var Message: TMessage); message TBM_THEMECHANGE;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DrawBackground(ACanvas: TCanvas; ARect: TRect); virtual;
    procedure DoDrawBackground(ACanvas: TCanvas; ARect: TRect;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    property Borders: Boolean read FBorders write SetBorders default True;
    property BorderType: TSpTBXPanelBorder read FBorderType write SetBorderType default pbrEtched;
    property Margins: TTBXControlMargins read FMargins write SetMargins;
    property TBXStyleBackground: Boolean read FTBXStyleBackground write SetTBXStyleBackground default False;
    property OnDrawBackground: TSpTBXDrawEvent read FOnDrawBackground write FOnDrawBackground;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InvalidateBackground(InvalidateChildren: Boolean = True); virtual;
  published
    // Don't let the streaming system store the WideStrings, use DefineProperties instead
    property Caption: TWideCaption read GetCaption write SetCaption stored IsCaptionStored; // Hides the inherited Caption
    property Hint: WideString read GetHint write SetHint stored IsHintStored; // Hides the inherited Hint
    property ThemeType: TSpTBXThemeType read FThemeType write SetThemeType default thtTBX;
  end;

  TSpTBXPanel = class(TSpTBXCustomPanel)
  private
    FHotTracking: Boolean;
    FHotTrack: Boolean;
    FChildFocused: Boolean;
    procedure SetHotTrack(const Value: Boolean);
    procedure SetHotTracking(const Value: Boolean);

    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure DrawBackground(ACanvas: TCanvas; ARect: TRect); override;
  public
    property HotTracking: Boolean read FHotTracking;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Color;
    property Constraints;
    property UseDockManager;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    // TSpTBXCustomPanel properties
    property Borders;
    property BorderType;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property Margins;
    property TBXStyleBackground;
    property OnDrawBackground;
  end;

  { TSpTBXGroupBox }

  TSpTBXCustomGroupBox = class(TSpTBXCustomPanel)
  private
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure DrawBackground(ACanvas: TCanvas; ARect: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSpTBXGroupBox = class(TSpTBXCustomGroupBox)
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property UseDockManager;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    // TSpTBXCustomPanel properties
    property Borders;
    property BorderType;
    property Margins;
    property TBXStyleBackground;
    property OnDrawBackground;
  end;

  { TSpTBXSplitter }

  TSpTBXCustomSplitter = class(TSpTBXCustomPanel, ISpTBXSplitter)
  private
    FMouseSplitControl: TControl;
    FMouseActiveControl: TWinControl;
    FMouseBrush: TBrush;
    FMouseDownPos: TPoint;
    FMouseDownOnGrip: Boolean;
    FMouseOverGrip: Boolean;
    FMouseLineDC: HDC;
    FMouseLineVisible: Boolean;
    FMousePrevBrush: HBrush;
    FMinSize: Integer;
    FMaxSize: Integer;
    FNewSize: Integer;
    FOldSize: Integer;
    FSplit: Integer;
    FRestorePos: Integer;
    FMoving: Boolean;
    FGripSize: Integer;
    FGripHotTrack: Boolean;
    FOldKeyDown: TKeyEvent;
    FResizeStyle: TResizeStyle;
    FOnMoving: TSpTBXCanResizeEvent;
    FOnMoved: TNotifyEvent;
    function GetGripRect: TRect;
    function GetMinimized: Boolean;
    procedure SetGripSize(const Value: Integer);
    procedure SetMinSize(const Value: integer);
    procedure UpdateControlSize(SplitControl: TControl);
    procedure MouseCalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
    procedure MouseAllocateLineDC;
    procedure MouseReleaseLineDC;
    procedure MouseDrawLine;
    procedure MouseFocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CMMouseleave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure DrawBackground(ACanvas: TCanvas; ARect: TRect); override;
    procedure DoDrawBackground(ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); override;
    procedure DoMoved; virtual;
    function DoMoving(var NewSize: Integer): Boolean; virtual;
    function IsVertical: Boolean;
    procedure MouseStopSizing; dynamic;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure RequestAlign; override;
    property GripSize: Integer read FGripSize write SetGripSize default 50;
    property GripHotTrack: Boolean read FGripHotTrack write FGripHotTrack default True;
    property MinSize: Integer read FMinSize write SetMinSize default 0;
    property ResizeStyle: TResizeStyle read FResizeStyle write FResizeStyle default rsUpdate;
    property OnMoving: TSpTBXCanResizeEvent read FOnMoving write FOnMoving;
    property OnMoved: TNotifyEvent read FOnMoved write FOnMoved;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ValidateSplitControl(var SplitControl: TControl): Boolean;  // For ISpTBXSplitter
    procedure ChangeSplitControlSize(NewControlSize: Integer);
    procedure InvalidateGrip;
    procedure Minimize; // For ISpTBXSplitter
    procedure Restore;  // For ISpTBXSplitter
    procedure Toggle;   // For ISpTBXSplitter
    property GripRect: TRect read GetGripRect;
    property Minimized: Boolean read GetMinimized;
    property MouseOverGrip: Boolean read FMouseOverGrip;
    property Moving: Boolean read FMoving;
  published
    property Align default alLeft;
    property TabStop default False;
    property Width default 5;
  end;

  TSpTBXSplitter = class(TSpTBXCustomSplitter)
  published
    property Align;
    property Color;
    property Constraints;
    property ParentColor;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    // TSpTBXCustomSplitter properties
    property GripSize;
    property MinSize;
    property ResizeStyle;
    property OnDrawBackground;
    property OnMoving;
    property OnMoved;
  end;

  { TSpTBXTextObjectActionLink }

  TSpTBXTextObjectActionLink = class(TControlActionLink)
  protected
    FUnicodeClient: TSpTBXTextObject;
    procedure AssignClient(AClient: TObject); override;
    function IsCheckedLinked: Boolean; override;
    procedure SetCaption(const Value: String); override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetHint(const Value: String); override;
    procedure SetImageIndex(Value: Integer); override;
  end;

  { TSpTBXTextObject }

  TSpTBXTextObject = class(TTBXTextObject)
  private
    FAlignment: TAlignment;
    FCaptionGlow: TSpGlowDirection;
    FCaptionGlowColor: TColor;
    FChecked: Boolean;
    FDisabledIconCorrection: Boolean;
    FDrawPushedCaption: Boolean;
    FImageChangeLink: TChangeLink;
    FImageIndex: TImageIndex;
    FImages: TCustomImageList;
    FThemeType: TSpTBXThemeType;
    FLinkText: WideString;
    FLinkTextParams: WideString;
    FLinkFont: TFont;
    FOnDraw: TSpTBXDrawEvent;
    FOnDrawCaption: TSpTBXDrawTextEvent;
    FOnDrawHint: TSpTBXDrawHintEvent;
    FOnGetImageIndex: TSpTBXGetImageIndexEvent;
    function GetCaption: TWideCaption;
    function GetHint: WideString;
    procedure ImageListChange(Sender: TObject);
    function IsCaptionStored: Boolean;
    function IsHintStored: Boolean;
    procedure SetCaption(const Value: TWideCaption);
    procedure SetHint(const Value: WideString);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetCaptionGlow(const Value: TSpGlowDirection);
    procedure SetCaptionGlowColor(const Value: TColor);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetLinkFont(const Value: TFont);
    procedure SetThemeType(const Value: TSpTBXThemeType);
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  protected
    // Painting
    procedure AdjustFont(AFont: TFont); override;
    procedure DoDrawHint(AHintBitmap: TBitmap; var AHint: Widestring; var PaintDefault: Boolean); virtual;
    function DoDrawItem(ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage): Boolean; virtual;
    function DoDrawText(ACanvas: TCanvas; var Rect: TRect; Flags: Longint): Integer; override;
    procedure DoGetImageIndex(var AImageList: TCustomImageList; var AImageIndex: integer); virtual;
    function GetFocusRect(R, TextR, GlyphR: TRect): TRect; reintroduce; virtual;
    function GetTextAlignment: TAlignment; override;
    function GetTextMargins: TRect; override;
    procedure Paint; override;

    // Sizing
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure DoAdjustHeight(ACanvas: TCanvas; var NewHeight: Integer); override;

    // Mouse
    function GetFocused: Boolean; virtual;
    function GetPushed: Boolean; virtual;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override; // Fix a TBX bug, TTBXTextObject can't recieve focus if it's inside a Panel

    // Component
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ExecuteLink; virtual;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetChecked: Boolean; virtual;
    procedure SetChecked(Value: Boolean); virtual;

    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify; // Hides the inherited Alignment
    property AutoSize default True;
    property Checked: Boolean read GetChecked write SetChecked default False;
    property CaptionGlow: TSpGlowDirection read FCaptionGlow write SetCaptionGlow default gldNone;
    property CaptionGlowColor: TColor read FCaptionGlowColor write SetCaptionGlowColor default clYellow;
    property DrawPushedCaption: Boolean read FDrawPushedCaption write FDrawPushedCaption default False;
    property DisabledIconCorrection: Boolean read FDisabledIconCorrection write FDisabledIconCorrection default True;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property LinkText: WideString read FLinkText write FLinkText;
    property LinkTextParams: WideString read FLinkTextParams write FLinkTextParams;
    property LinkFont: TFont read FLinkFont write SetLinkFont;
    property ThemeType: TSpTBXThemeType read FThemeType write SetThemeType default thtTBX;
    property OnDraw: TSpTBXDrawEvent read FOnDraw write FOnDraw;
    property OnDrawCaption: TSpTBXDrawTextEvent read FOnDrawCaption write FOnDrawCaption;
    property OnDrawHint: TSpTBXDrawHintEvent read FOnDrawHint write FOnDrawHint;
    property OnGetImageIndex: TSpTBXGetImageIndexEvent read FOnGetImageIndex write FOnGetImageIndex;
    property SmartFocus default True;
    property TabStop default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CanFocus: Boolean; override;
    procedure Click; override;
    function GetControlsAlignment: TAlignment; override;
    procedure GetSize(out TotalR, TextR, GlyphR: TRect); virtual;
    function GetTextFlags: Cardinal;
    function IsImageShown: Boolean;
    property MouseInControl;
    property Pushed: Boolean read GetPushed;
  published
    // Don't let the streaming system store the WideStrings, use DefineProperties instead
    property Caption: TWideCaption read GetCaption write SetCaption stored IsCaptionStored; // Hides the inherited Caption
    property Hint: WideString read GetHint write SetHint stored IsHintStored; // Hides the inherited Hint
  end;

  { TSpTBXLabel }

  TSpTBXCustomLabel = class(TSpTBXTextObject)
  private
    FFocusControl: TWinControl;
    FUnderline: Boolean;
    FUnderlineColor: TColor;
    procedure SetFocusControl(const Value: TWinControl);
    procedure SetUnderline(const Value: Boolean);
    procedure SetUnderlineColor(const Value: TColor);
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    function DoDrawItem(ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage): Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property Underline: Boolean read FUnderline write SetUnderline default False;
    property UnderlineColor: TColor read FUnderlineColor write SetUnderlineColor default clBtnShadow;
  public
    constructor Create(AOwner: TComponent); override;
    procedure GetSize(out TotalR: TRect; out TextR: TRect; out GlyphR: TRect); override;
  end;

  TSpTBXLabel = class(TSpTBXCustomLabel)
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Margins;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Visible;
    property Wrapping;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    // TSpTBXCustomLabel properties
    property Alignment;
    property CaptionGlow;
    property CaptionGlowColor;
    property FocusControl;
    property Images;
    property ImageIndex;
    property LinkText;
    property LinkTextParams;
    property LinkFont;
    property Underline;
    property UnderlineColor;
    property OnDraw;
    property OnDrawCaption;
    property OnDrawHint;
    property OnGetImageIndex;
  end;

  { TSpTBXButtonControl }

  TSpTBXButtonControl = class(TSpTBXTextObject)
  private
    FGroupIndex: Integer;
    FStateChanged: Boolean;
    procedure UpdateExclusive;
    procedure SetGroupIndex(const Value: Integer);
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMSPGroupIndexUpdate(var Message: TMessage); message CM_SPGROUPINDEXUPDATE;
  protected
    procedure Toggle; virtual;
    procedure SetChecked(Value: Boolean); override;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property StateChanged: Boolean read FStateChanged write FStateChanged;
  public
    constructor Create(AOwner: TComponent); override;
    function CanFocus: Boolean; override;
  end;

  { TSpTBXCheckBox }

  TSpTBXCustomCheckBox = class(TSpTBXButtonControl)
  private
    FAllowGrayed: Boolean;
    FState: TCheckBoxState;
    procedure SetState(const Value: TCheckBoxState);
  protected
    procedure DoGetImageIndex(var AImageList: TCustomImageList; var AImageIndex: integer); override;
    function GetChecked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
    procedure Toggle; override;
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
    property State: TCheckBoxState read FState write SetState default cbUnchecked;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    procedure GetSize(out TotalR, TextR, GlyphR: TRect); override;
  end;

  TSpTBXCheckBox = class(TSpTBXCustomCheckBox)
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Margins;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property Wrapping;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    // TSpTBXCustomCheckBox properties
    property Alignment;
    property AllowGrayed;
    property CaptionGlow;
    property CaptionGlowColor;
    property Checked;
    property State;
    property ThemeType;
    property OnDraw;
    property OnDrawCaption;
    property OnDrawHint;
    property OnGetImageIndex;
  end;

  { TSpTBXRadioButton }

  TSpTBXCustomRadioButton = class(TSpTBXButtonControl)
  private
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
  protected
    procedure DoGetImageIndex(var AImageList: TCustomImageList; var AImageIndex: integer); override;
    procedure SetChecked(Value: Boolean); override;
    procedure Toggle; override;
    property TabStop default False;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    procedure GetSize(out TotalR, TextR, GlyphR: TRect); override;
  end;

  TSpTBXRadioButton = class(TSpTBXCustomRadioButton)
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Margins;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property Wrapping;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    // TSpTBXCustomRadioButton properties
    property Alignment;
    property CaptionGlow;
    property CaptionGlowColor;
    property Checked;
    property GroupIndex;
    property ThemeType;
    property OnDraw;
    property OnDrawCaption;
    property OnDrawHint;
    property OnGetImageIndex;
  end;

  { TSpTBXRadioGroup }

  TSpTBXCustomRadioGroup = class(TSpTBXCustomGroupBox)
  private
    FButtons: TList;
    FItems: TTntStrings;
    FItemIndex: Integer;
    FColumns: Integer;
    FReading: Boolean;
    FUpdating: Boolean;
    function GetButtons(Index: Integer): TSpTBXRadioButton;
    procedure ArrangeButtons;
    procedure ButtonClick(Sender: TObject);
    procedure ItemsChange(Sender: TObject);
    procedure SetButtonCount(Value: Integer);
    procedure SetColumns(Value: Integer);
    procedure SetItemIndex(Value: Integer);
    procedure SetItems(Value: TTntStrings);
    procedure UpdateButtons;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  protected
    procedure Loaded; override;
    procedure ReadState(Reader: TReader); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    property Columns: Integer read FColumns write SetColumns default 1;
    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;
    property Items: TTntStrings read FItems write SetItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FlipChildren(AllLevels: Boolean); override;
    procedure InvalidateBackground(InvalidateChildren: Boolean = True); override;
    procedure SetFocus; override;
    property Buttons[Index: Integer]: TSpTBXRadioButton read GetButtons;
  end;

  TSpTBXRadioGroup = class(TSpTBXCustomRadioGroup)
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property UseDockManager;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    // TSpTBXCustomPanel properties
    property Borders;
    property BorderType;
    property Margins;
    property TBXStyleBackground;
    property OnDrawBackground;
    // TSpTBXCustomRadioGroup properties
    property Columns;
    property ItemIndex;
    property Items;
  end;

  { TSpTBXButton }

  TSpTBXCustomButton = class(TSpTBXButtonControl)
  private
    FBitmap: TBitmap;
    FBitmapTransparent: Boolean;
    FActive: Boolean;
    FCancel: Boolean;
    FDefault: Boolean;
    FDropDownArrow: Boolean;
    FDropDownMenu: TPopupMenu;
    FDropDownMenuVisible: Boolean;
    FModalResult: TModalResult;
    FRepeating: Boolean;
    FRepeatTimer: TTimer;
    procedure BitmapChanged(Sender: TObject);
    procedure RepeatTimerHandler(Sender: TObject);
    procedure SetBitmap(const Value: TBitmap);
    procedure SetDefault(const Value: Boolean);
    procedure SetDropDownArrow(const Value: Boolean);
    procedure SetDropdownMenu(Value: TPopupMenu);
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CMSPPopupClose(var Message: TMessage); message CM_SPPOPUPCLOSE;
    procedure WMCancelMode(var Message: TWMCancelMode); message WM_CANCELMODE;
  protected
    FPopupControl: TControl;
    procedure AdjustFont(AFont: TFont); override;
    function BitmapValid: boolean;
    function DoDrawDropDownArrow(ACanvas: TCanvas; ARect: TRect): Boolean; virtual;
    function DoDrawItem(ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage): Boolean; override;
    function GetFocusRect(R, TextR, GlyphR: TRect): TRect; override;
    function GetFocused: Boolean; override;    
    function GetPushed: Boolean; override;
    function GetTextMargins: TRect; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property Alignment default taCenter;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property BitmapTransparent: Boolean read FBitmapTransparent write FBitmapTransparent default True;
    property DrawPushedCaption default True;
    property Cancel: Boolean read FCancel write FCancel default False;
    property Default: Boolean read FDefault write SetDefault default False;
    property DropDownArrow: Boolean read FDropDownArrow write SetDropDownArrow default True;
    property DropDownMenu: TPopupMenu read FDropDownMenu write SetDropDownMenu;
    property ModalResult: TModalResult read FModalResult write FModalResult default 0;
    property Repeating: Boolean read FRepeating write FRepeating default False;
    property ThemeType default thtWindows;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    function GetSkinStateRect: TRect;
    function IsDroppedDown: Boolean;
    procedure StopRepeat; virtual;
  end;

  TSpTBXButton = class(TSpTBXCustomButton)
  published
    property Action;
    property Align;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Margins;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property Wrapping;
    property OnClick;
    property OnContextPopup;
    // property OnDblClick; Buttons don't have OnDblClick events
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    // TSpTBXCustomButton properties
    property Alignment;
    property Bitmap;
    property BitmapTransparent;
    property Cancel;
    property CaptionGlow;
    property CaptionGlowColor;
    property Checked;
    property Default;
    property DrawPushedCaption;
    property DropDownArrow;
    property DropDownMenu;
    property GroupIndex;
    property Images;
    property ImageIndex;
    property LinkText;
    property LinkTextParams;
    property LinkFont;
    property ModalResult;
    property ThemeType;
    property Repeating;
    property OnDraw;
    property OnDrawCaption;
    property OnDrawHint;
    property OnGetImageIndex;
  end;

  { TSpTBXSpeedButton }

  TSpTBXCustomSpeedButton = class(TSpTBXCustomButton)
  public
    constructor Create(AOwner: TComponent); override;
    function CanFocus: Boolean; override;
    procedure Click; override;
  end;

  TSpTBXSpeedButton = class(TSpTBXCustomSpeedButton)
  published
    property Action;
    property Align;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Margins;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    // property TabOrder; SpeedButtons don't have TabStops
    // property TabStop; SpeedButtons don't have TabStops
    property Visible;
    property Wrapping;
    property OnClick;
    property OnContextPopup;
    // property OnDblClick; SpeedButtons don't have OnDblClick events
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    // TSpTBXCustomButton properties
    property Alignment;
    property Bitmap;
    property BitmapTransparent;
    property Cancel;
    property CaptionGlow;
    property CaptionGlowColor;
    property Checked;
    property Default;
    property DrawPushedCaption;
    property DropDownArrow;
    property DropDownMenu;
    property GroupIndex;
    property Images;
    property ImageIndex;
    property LinkText;
    property LinkTextParams;
    property LinkFont;
    // property ModalResult;
    property ThemeType;
    property Repeating;
    property OnDraw;
    property OnDrawCaption;
    property OnDrawHint;
    property OnGetImageIndex;
  end;

  { TSpTBXProgressBar }

  TSpTBXProgressBarChangeEvent = procedure(Sender: TObject; NewPosition: Integer) of object;

  TSpTBXCustomProgressBar = class(TSpTBXTextObject)
  private
    FMin: Integer;
    FMax: Integer;
    FPosition: Integer;
    FProgressVisible: Boolean;
    FSmooth: Boolean;
    FVertical: Boolean;
    FCaptionType: TSpTBXProgressCaption;
    FOnProgressChange: TSpTBXProgressBarChangeEvent;
    procedure SetMax(const Value: integer);
    procedure SetMin(const Value: integer);
    procedure SetPosition(Value: integer);
    procedure SetSmooth(const Value: Boolean);
    procedure SetVertical(const Value: Boolean);
    procedure SetCaptionType(const Value: TSpTBXProgressCaption);
    procedure SetProgressVisible(const Value: Boolean);
  protected
    function DoDrawItem(ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage): Boolean; override;
    procedure DoProgressChange; virtual;
    function GetTextMargins: TRect; override;
    property Alignment default taCenter;
    property CaptionGlow default gldAll;
    property CaptionType: TSpTBXProgressCaption read FCaptionType write SetCaptionType default pctPercentage;
    property Max: Integer read FMax write SetMax default 100;
    property Min: Integer read FMin write SetMin default 0;
    property Position: Integer read FPosition write SetPosition default 0;
    property ProgressVisible: Boolean read FProgressVisible write SetProgressVisible default True;
    property Smooth: Boolean read FSmooth write SetSmooth default False;
    property Vertical: Boolean read FVertical write SetVertical default False;
    property ThemeType default thtWindows;
    property OnProgressChange: TSpTBXProgressBarChangeEvent read FOnProgressChange write FOnProgressChange;
  public
    constructor Create(AOwner: TComponent); override;
    procedure StepIt(Delta: Integer = 1);
  end;

  TSpTBXProgressBar = class(TSpTBXCustomProgressBar)
  published
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    // TSpTBXCustomProgressBar properties
    property Alignment;
    property CaptionGlow;
    property CaptionGlowColor;
    property CaptionType;
    property Max;
    property Min;
    property Position;
    property Smooth;
    property Vertical;
    property ThemeType;
    property OnDraw;
    property OnDrawCaption;
    property OnDrawHint;
    property OnProgressChange;
  end;

  { TSpTBXTrackBar }

  TSpTBXTrackBar = class(TTrackBar)
  private
    FThemeType: TSpTBXThemeType;
    FTickMarks: TSpTBXTickMark;
    FOnDrawChannel: TSpTBXDrawEvent;
    FOnDrawChannelTics: TSpTBXDrawPosEvent;
    FOnDrawThumb: TSpTBXDrawEvent;
    procedure SetThemeType(const Value: TSpTBXThemeType);
    procedure SetTickMarks(const Value: TSpTBXTickMark);
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure TBMThemeChange(var Message: TMessage); message TBM_THEMECHANGE;
  protected
    function DoDrawChannel(ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage): Boolean; virtual;
    function DoDrawChannelTics(ACanvas: TCanvas; X, Y: Integer): Boolean; virtual;
    function DoDrawThumb(ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage): Boolean; virtual;
    procedure DrawChannelTics(ACanvas: TCanvas); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ChannelRect: TRect;
    function MouseInThumb: Boolean;
    procedure ForceRepaint;
  published
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property ThemeType: TSpTBXThemeType read FThemeType write SetThemeType default thtWindows;
    property TickMarks: TSpTBXTickMark read FTickMarks write SetTickMarks default tmxBottomRight;
    property OnDrawChannel: TSpTBXDrawEvent read FOnDrawChannel write FOnDrawChannel;
    property OnDrawChannelTics: TSpTBXDrawPosEvent read FOnDrawChannelTics write FOnDrawChannelTics;
    property OnDrawThumb: TSpTBXDrawEvent read FOnDrawThumb write FOnDrawThumb;
  end;

{ Painting helpers }
procedure SpDrawXPPanel(ACanvas: TCanvas; ARect: TRect; Enabled, TBXStyleBackground: Boolean; ThemeType: TSpTBXThemeType; Border: TSpTBXPanelBorder);
procedure SpDrawXPPanelBorder(ACanvas: TCanvas; ARect: TRect; Border: TSpTBXPanelBorder);
procedure SpDrawXPGroupBox(ACanvas: TCanvas; ARect: TRect; ACaption: WideString; TextFlags: Cardinal; Enabled, TBXStyleBackground: Boolean; ThemeType: TSpTBXThemeType);
procedure SpDrawXPProgressBar(ACanvas: TCanvas; ARect: TRect; Min, Max, Position: Integer; Back, Fore: TBitmap); overload;
function SpDrawXPProgressBar(ACanvas: TCanvas; ARect: TRect; Vertical, Smooth, DrawProgress: Boolean; Min, Max, Position: Integer; ThemeType: TSpTBXThemeType): Integer; overload;
procedure SpDrawXPTrackBar(ACanvas: TCanvas; ARect: TRect; Part: Cardinal; Vertical, Pushed: Boolean; TickMark: TSpTBXTickMark; Min, Max, SelStart, SelEnd: Integer; ThemeType: TSpTBXThemeType);
procedure SpInvalidateTBXControl(AControl: TWinControl; InvalidateChildren, OnlyTBXControls: Boolean);

implementation

uses
  TBXUtils, TBXUxThemes, CommCtrl, ShellAPI,
  TntActnList;

var
  SpStockImageList: TImageList;

type
  TWinControlAccess = class(TWinControl);

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Helpers }

procedure SpDrawXPPanel(ACanvas: TCanvas; ARect: TRect; Enabled, TBXStyleBackground: Boolean;
  ThemeType: TSpTBXThemeType; Border: TSpTBXPanelBorder);
var
  ClientR: TRect;
  Flags: Integer;
  ItemInfo: TTBXItemInfo;
begin
  case SpXPThemeType(ThemeType) of
    thtWindows:
      begin
        if Enabled then Flags := GBS_NORMAL
        else Flags := GBS_DISABLED;
        DrawThemeBackground(BUTTON_THEME, ACanvas.Handle, BP_GROUPBOX, Flags, ARect, nil);
      end;
    thtNone:
      SpDrawXPPanelBorder(ACanvas, ARect, Border);
    thtTBX:
      begin
        if TBXStyleBackground and (TBXCurrentTheme <> 'Default') then begin
          SpFillItemInfo(Enabled, False, False, True, ItemInfo);
          CurrentTheme.PaintButton(ACanvas, ARect, ItemInfo);
        end
        else begin
          // Exclude the client area
          ClientR := ARect;
          InflateRect(ClientR, -2, -2);
          ExcludeClipRect(ACanvas.Handle, ClientR.Left, ClientR.Top, ClientR.Right, ClientR.Bottom);
          try
            SpFillItemInfo(Enabled, True, True, False, ItemInfo);
            CurrentTheme.PaintFrame(ACanvas, ARect, ItemInfo);
          finally
            SelectClipRgn(ACanvas.Handle, 0);
          end;
        end;
      end;
  end;
end;

procedure SpDrawXPPanelBorder(ACanvas: TCanvas; ARect: TRect; Border: TSpTBXPanelBorder);
const
  Edge: array [TSpTBXPanelBorder] of Cardinal = (BDR_RAISEDINNER, EDGE_RAISED,
    BDR_SUNKENOUTER, EDGE_SUNKEN, EDGE_BUMP, EDGE_ETCHED, 0);
begin
  if Border = pbrFramed then begin
    ACanvas.Brush.Color := clBtnFace;
    ACanvas.FrameRect(ARect);
  end
  else
    DrawEdge(ACanvas.Handle, ARect, Edge[Border], BF_RECT);
end;

procedure SpDrawXPGroupBox(ACanvas: TCanvas; ARect: TRect; ACaption: WideString;
  TextFlags: Cardinal; Enabled, TBXStyleBackground: Boolean; ThemeType: TSpTBXThemeType);
var
  Width: Integer;
  R: TRect;
  Flags: Integer;
  CaptionRect: TRect;
  XPThemes: Boolean;
begin
  Flags := 0;
  XPThemes := SpXPThemeType(ThemeType) = thtWindows;
  Width := ARect.Right - ARect.Left;

  if ACaption <> '' then begin
    CaptionRect := Rect(0, 0, 1, 1);

    if Enabled then Flags := GBS_NORMAL
    else Flags := GBS_DISABLED;
    if XPThemes then
      GetThemeTextExtent(BUTTON_THEME, ACanvas.Handle, BP_GROUPBOX, Flags,
        PWideChar(ACaption), Length(ACaption), DT_LEFT, nil, CaptionRect)
    else
      SpDrawXPText(ACanvas, ACaption, CaptionRect, TextFlags or DT_CALCRECT);

    if (TextFlags and DT_RTLREADING) = 0 then
      OffsetRect(CaptionRect, 8, 0)
    else
      OffsetRect(CaptionRect, Width - 8 - CaptionRect.Right, 0);
  end
  else
    CaptionRect := Rect(0, 0, 0, 0);

  R := ARect;
  R.Top := (CaptionRect.Bottom - CaptionRect.Top) div 2;
  with CaptionRect do
    ExcludeClipRect(ACanvas.Handle, Left, Top, Right, Bottom);
  try
    SpDrawXPPanel(ACanvas, R, Enabled, TBXStyleBackground, ThemeType, pbrEtched);
  finally
    SelectClipRgn(ACanvas.Handle, 0);
  end;

  if ACaption <> '' then
    if XPThemes then
      DrawThemeText(BUTTON_THEME, ACanvas.Handle, BP_GROUPBOX, Flags, PWideChar(ACaption), -1, TextFlags, 0, CaptionRect)
    else
      SpDrawXPText(ACanvas, ACaption, CaptionRect, TextFlags);
end;

procedure SpDrawXPProgressBar(ACanvas: TCanvas; ARect: TRect;
  Min, Max, Position: Integer; Back, Fore: TBitmap);
var
  Percent, Delta: Integer;
  DeltaR, R: TRect;
begin
  if Position < Min then Position := Min
  else if Position > Max then Position := Max;

  // Get the delta
  if (Max > Min) and (Position > Min) then begin
    Percent := (Position * 100) div (Max - Min);
    DeltaR := ARect;
    R := Rect(0, 0, Back.Width, Back.Height);
    if Back.Height > Back.Width then begin
      Delta := (Back.Height * Percent) div 100;
      DeltaR.Bottom := DeltaR.Top + Delta;
      R.Bottom := R.Top + Delta;
    end
    else begin
      Delta := (Back.Width * Percent) div 100;
      DeltaR.Right := DeltaR.Left + Delta;
      R.Right := R.Left + Delta;
    end;
  end
  else
    Delta := 0;

  ACanvas.Draw(ARect.Left, ARect.Top, Back);
  if Delta > 0 then
    ACanvas.CopyRect(DeltaR, Fore.Canvas, R);
end;

function SpDrawXPProgressBar(ACanvas: TCanvas; ARect: TRect;
  Vertical, Smooth, DrawProgress: Boolean; Min, Max, Position: Integer;
  ThemeType: TSpTBXThemeType): Integer;
const
  PartID: array [Boolean] of Integer = (PP_BAR, PP_BARVERT);
  ChunkID: array [Boolean] of Integer = (PP_CHUNK, PP_CHUNKVERT);
var
  ItemInfo: TTBXItemInfo;
  ChunkPaint: Boolean;
  I: Integer;
  DeltaR, R: TRect;
  B: TBitmap;
  Percentage: Double;
begin
  Result := 0;
  ChunkPaint := False;
  if Position < Min then Position := Min
  else if Position > Max then Position := Max;
  ThemeType := SpXPThemeType(ThemeType);

  // Get the delta
  if (Max > Min) and (Position > Min) then begin
    DeltaR := ARect;
    case ThemeType of
      thtWindows:
        if Vertical then InflateRect(DeltaR, -3, -4)
        else InflateRect(DeltaR, -4, -3);
      thtNone: InflateRect(DeltaR, -2, -2);
    end;
    // Cast Position to a Double real type, otherwise Percentage * 100
    // returns a negative value, e.g. 30000000 * 100
    Percentage := Position;
    Percentage := (Percentage * 100) / (Max - Min);
    Result := Round(Percentage);
    if Vertical then
      DeltaR.Top := DeltaR.Bottom - (((DeltaR.Bottom - DeltaR.Top) * Result) div 100)
    else
      DeltaR.Right := DeltaR.Left + (((DeltaR.Right - DeltaR.Left) * Result) div 100);
  end
  else
    DeltaR := Rect(0, 0, 0, 0);

  B := TBitmap.Create;
  try
    case ThemeType of
      thtWindows:
        begin
          DrawThemeBackground(SP_PROGRESS_THEME, ACanvas.Handle, PartID[Vertical], 0, ARect, nil);
          if DrawProgress and not IsRectEmpty(DeltaR) then begin
            // [TBXTheme-Change]
            // Another Windows API bug, Windows XP progress bar chunks are 8 x 11,
            // but DrawThemeBackground draws 10 x 11 chunks. We must draw the chunks manually.
            if Vertical then begin
              B.Width := DeltaR.Right - DeltaR.Left;
              B.Height := 8;
              R := Rect(0, 2, B.Width, B.Height);
            end
            else begin
              B.Width := 8;
              B.Height := DeltaR.Bottom - DeltaR.Top;
              R := Rect(0, 0, B.Width - 2, B.Height);
            end;
            DrawThemeBackground(SP_PROGRESS_THEME, B.Canvas.Handle, ChunkID[Vertical], 0, R, nil);
            ChunkPaint := True;
          end;
        end;
      thtNone:
        begin
          DrawEdge(ACanvas.Handle, ARect, BDR_SUNKENOUTER, BF_RECT);
          if DrawProgress and not IsRectEmpty(DeltaR) then
            if Smooth then begin
              ACanvas.Brush.Color := clHighlight;
              ACanvas.FillRect(DeltaR);
            end
            else begin
              // Chunks are 10 x 13
              if Vertical then begin
                B.Width := DeltaR.Right - DeltaR.Left;
                B.Height := 10;
                R := Rect(0, 2, B.Width, B.Height);
              end
              else begin
                B.Width := 10;
                B.Height := DeltaR.Bottom - DeltaR.Top;
                R := Rect(0, 0, B.Width - 2, B.Height);
              end;
              B.Canvas.Brush.Color := clBtnFace;
              B.Canvas.FillRect(Rect(0, 0, B.Width, B.Height));
              B.Canvas.Brush.Color := clHighlight;
              B.Canvas.FillRect(R);
              ChunkPaint := True;
            end;
        end;
      thtTBX:
        begin
          SpFillItemInfo(True, True, True, False, ItemInfo);
          CurrentTheme.PaintButton(ACanvas, ARect, ItemInfo);
          if DrawProgress and not IsRectEmpty(DeltaR) then begin
            ItemInfo.Pushed := False;
            ItemInfo.HoverKind := hkMouseHover;
            B.Width := ARect.Right - ARect.Left;
            B.Height := ARect.Bottom - ARect.Top;
            R := Rect(0, 0, B.Width, B.Height);
            B.Canvas.CopyRect(R, ACanvas, ARect); // B is transparent

            // [TBXTheme-Change]
            // On the Dream theme the Menu selection looks better than the
            // button painting
            if TBXCurrentTheme = 'Dream' then
              CurrentTheme.PaintMenuItem(B.Canvas, R, ItemInfo)
            else
              CurrentTheme.PaintButton(B.Canvas, R, ItemInfo);

            if Vertical then
              R.Top := R.Bottom - (DeltaR.Bottom - DeltaR.Top)
            else
              R.Right := DeltaR.Right - DeltaR.Left;
            ACanvas.CopyRect(DeltaR, B.Canvas, R);
          end;
        end;
    end;

    if ChunkPaint then begin
      if Vertical then begin
        ExcludeClipRect(ACanvas.Handle, ARect.Left, ARect.Top, ARect.Right, ARect.Top + 2);
        I := DeltaR.Bottom - B.Height;
        while I > DeltaR.Top - B.Height do begin
          ACanvas.Draw(DeltaR.Left, I, B);
          Dec(I, B.Height);
        end;
      end
      else begin
        ExcludeClipRect(ACanvas.Handle, ARect.Right - 2, ARect.Top, ARect.Right, ARect.Bottom);
        I := DeltaR.Left;
        while I < DeltaR.Right do begin
          ACanvas.Draw(I, DeltaR.Top, B);
          Inc(I, B.Width);
        end;
      end;
      SelectClipRgn(ACanvas.Handle, 0);
    end;
  finally
    B.Free;
  end;
end;

procedure SpDrawXPTrackBar(ACanvas: TCanvas; ARect: TRect; Part: Cardinal;
  Vertical, Pushed: Boolean; TickMark: TSpTBXTickMark;
  Min, Max, SelStart, SelEnd: Integer; ThemeType: TSpTBXThemeType);
var
  ItemInfo: TTBXItemInfo;
  Flags: Integer;

  procedure DrawChannelSelection(ChannelR: TRect);
  var
    I: Integer;
    Step : Single;
  begin
    I := Max - Min;
    if (I > 0) and (SelEnd > SelStart) then begin
      if ThemeType = thtTBX then
        InflateRect(ChannelR, -2, -2)
      else
        InflateRect(ChannelR, -1, -1);
      Step := (ChannelR.Right - ChannelR.Left) / I;
      ChannelR.Right := ChannelR.Left + Round(SelEnd * Step);
      ChannelR.Left := ChannelR.Left  + Round(SelStart * Step);

      if ThemeType = thtTBX then begin
        SpFillItemInfo(True, True, False, True, ItemInfo);
        CurrentTheme.PaintButton(ACanvas, ChannelR, ItemInfo);
      end
      else begin
        ACanvas.Brush.Color := clHighlight;
        ACanvas.FillRect(ChannelR);
      end;
    end;
  end;

begin
  ThemeType := SpXPThemeType(ThemeType);
  Case ThemeType of
    thtWindows:
      if Part = TBCD_THUMB then begin
        if Pushed then Flags := TUS_HOT
        else Flags := TUS_NORMAL;
        Case TickMark of
          tmxBottomRight:
            if Vertical then Part := TKP_THUMBRIGHT
            else Part := TKP_THUMBBOTTOM;
          tmxTopLeft:
            if Vertical then Part := TKP_THUMBLEFT
            else Part := TKP_THUMBTOP;
          tmxBoth, tmxCenter:
            if Vertical then Part := TKP_THUMBVERT
            else Part := TKP_THUMB;
        end;
        DrawThemeBackground(SP_TRACKBAR_THEME, ACanvas.Handle, Part, Flags, ARect, nil);
      end
      else if Part = TBCD_CHANNEL then begin
        if Vertical then Part := TKP_TRACKVERT
        else Part := TKP_TRACK;
        DrawThemeBackground(SP_TRACKBAR_THEME, ACanvas.Handle, Part, TKS_NORMAL, ARect, nil);
        DrawChannelSelection(ARect);
      end;
    thtNone:
      if Part = TBCD_THUMB then begin
        ACanvas.Brush.Color := clBtnFace;
        ACanvas.FillRect(ARect);
        DrawFrameControl(ACanvas.Handle, ARect, DFC_BUTTON, DFCS_BUTTONPUSH);
      end
      else if Part = TBCD_CHANNEL then begin
        ACanvas.Brush.Color := clWindow;
        ACanvas.FillRect(ARect);
        ExtCtrls.Frame3D(ACanvas, ARect, clBtnShadow, clBtnHighlight, 1);
        ExtCtrls.Frame3D(ACanvas, ARect, cl3DDkShadow, clBtnFace, 1);
        DrawChannelSelection(ARect);
      end;
    thtTBX:
      if Part = TBCD_THUMB then begin
        SpFillItemInfo(True, False, True and not Pushed, Pushed, ItemInfo);
        CurrentTheme.PaintButton(ACanvas, ARect, ItemInfo);
      end
      else if Part = TBCD_CHANNEL then begin
        SpFillItemInfo(True, True, True, False, ItemInfo);
        CurrentTheme.PaintButton(ACanvas, ARect, ItemInfo);
        DrawChannelSelection(ARect);
      end;
  end;
end;

procedure SpInvalidateTBXControl(AControl: TWinControl; InvalidateChildren, OnlyTBXControls: Boolean);
var
  I: Integer;
  ChildW: TWinControl;
begin
  // Invalidate will not fire WM_ERASEBKGND, because csOpaque is setted
  if Assigned(AControl) and not (csDestroying in AControl.ComponentState) and
    AControl.HandleAllocated then
  begin
    if InvalidateChildren then begin
      if OnlyTBXControls then begin
        RedrawWindow(AControl.Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE);
        // Only invalidate TBXControls
        for I := 0 to AControl.ControlCount - 1 do
          if Assigned(AControl.Controls[I]) and (AControl.Controls[I] is TWinControl) then begin
            ChildW := AControl.Controls[I] as TWinControl;
            if ChildW is TTBXPanelObject then
              RedrawWindow(ChildW.Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE)
            else
              PostMessage(ChildW.Handle, CM_SPTBXCONTROLSINVALIDATE, ChildW.Width, ChildW.Height);
          end;
      end
      else
        RedrawWindow(AControl.Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN);
    end
    else
      RedrawWindow(AControl.Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE);
  end;
end;

procedure ApplyMargins(var R: TRect; const Margins: TTBXControlMargins); overload;
begin
  with Margins do
  begin
    Inc(R.Left, Left); Inc(R.Top, Top);
    Dec(R.Right, Right); Dec(R.Bottom, Bottom);
  end;
end;

procedure ApplyMargins(var R: TRect; const Margins: TRect); overload;
begin
  with Margins do
  begin
    Inc(R.Left, Left); Inc(R.Top, Top);
    Dec(R.Right, Right); Dec(R.Bottom, Bottom);
  end;
end;

function GetRealAlignment(Control: TControl): TAlignment;
const
  ReverseAlignment: array [TAlignment] of TAlignment = (taRightJustify, taLeftJustify, taCenter);
begin
  Result := Control.GetControlsAlignment;
  if Control.UseRightToLeftAlignment then Result := ReverseAlignment[Result];
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomPanel }

constructor TSpTBXCustomPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];

  FBackground := TBitmap.Create;

  FMargins := TTBXControlMargins.Create;
  FMargins.OnChange := MarginsChangeHandler;
  FBorders := True;
  FBorderType := pbrEtched;
  FThemeType := thtTBX;
  if not ParentColor then
    Color := clNone;
  AddThemeNotification(Self);
end;

destructor TSpTBXCustomPanel.Destroy;
begin
  RemoveThemeNotification(Self);
  FreeAndNil(FMargins);
  FreeAndNil(FBackground);
  inherited;
end;

procedure TSpTBXCustomPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if not (csDesigning in ComponentState) then
    with Params.WindowClass do
      Style := Style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TSpTBXCustomPanel.CreateWindowHandle(const Params: TCreateParams);
begin
  CreateUnicodeHandle(Self, Params, '');
end;

procedure TSpTBXCustomPanel.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  InflateRect(Rect, -1, -1);
  with Margins do
  begin
    Inc(Rect.Left, Left);
    Inc(Rect.Top, Top);
    Dec(Rect.Right, Right);
    Dec(Rect.Bottom, Bottom);
  end;
end;

procedure TSpTBXCustomPanel.DefineProperties(Filer: TFiler);
begin
  inherited;
  // Don't let the streaming system store the WideStrings,
  // we need to store them manually
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

procedure TSpTBXCustomPanel.InvalidateBackground(InvalidateChildren: Boolean);
begin
  FBackgroundValid := False;
  // Force background repaint
  SpInvalidateTBXControl(Self, InvalidateChildren, True);
end;

procedure TSpTBXCustomPanel.SetBorders(const Value: Boolean);
begin
  if FBorders <> Value then begin
    FBorders := Value;
    InvalidateBackground;
  end;
end;

procedure TSpTBXCustomPanel.SetBorderType(const Value: TSpTBXPanelBorder);
begin
  if FBorderType <> Value then begin
    FBorderType := Value;
    InvalidateBackground;
  end;
end;

function TSpTBXCustomPanel.IsCaptionStored: Boolean;
begin
  Result := TntControl_IsCaptionStored(Self);
end;

function TSpTBXCustomPanel.GetCaption: TWideCaption;
begin
  Result := TntControl_GetText(Self);
end;

procedure TSpTBXCustomPanel.SetCaption(const Value: TWideCaption);
begin
  TntControl_SetText(Self, Value);
end;

function TSpTBXCustomPanel.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self);
end;

function TSpTBXCustomPanel.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self);
end;

procedure TSpTBXCustomPanel.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TSpTBXCustomPanel.MarginsChangeHandler(Sender: TObject);
begin
  Realign;
  Invalidate;
end;

procedure TSpTBXCustomPanel.SetMargins(Value: TTBXControlMargins);
begin
  FMargins.Assign(Value);
end;

procedure TSpTBXCustomPanel.SetThemeType(const Value: TSpTBXThemeType);
begin
  if Value <> FThemeType then begin
    FThemeType := Value;
    InvalidateBackground(False);
  end;
end;

procedure TSpTBXCustomPanel.SetTBXStyleBackground(const Value: Boolean);
begin
  if FTBXStyleBackground <> Value then begin
    FTBXStyleBackground := Value;
    InvalidateBackground;
  end;
end;

procedure TSpTBXCustomPanel.DoDrawBackground(ACanvas: TCanvas;
  ARect: TRect; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawBackground) then FOnDrawBackground(Self, ACanvas, ARect,
    PaintStage, PaintDefault);
end;

procedure TSpTBXCustomPanel.DrawBackground(ACanvas: TCanvas; ARect: TRect);
begin
  SpDrawXPPanel(ACanvas, ARect, True, FTBXStyleBackground, FThemeType, FBorderType);
end;

procedure TSpTBXCustomPanel.EraseTBXBackground(ACanvas: TCanvas; ARect: TRect);
var
  PaintDefault: Boolean;
  R, R2: TRect;
  B: TBitmap;
begin
  if (csDestroying in ComponentState) or IsRectEmpty(ARect) then Exit;

  B := TBitmap.Create;
  try
    R := Rect(0, 0, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);

    B.Canvas.Lock;
    B.Width := R.Right;
    B.Height := R.Bottom;

    if Color = clNone then begin
      B.Canvas.Brush.Color := clWhite;
      B.Canvas.FillRect(R);
      SpDrawParentBackground(Self, B.Canvas.Handle, R);
    end
    else begin
      FillRectEx(B.Canvas.Handle, R, Color);
    end;

    B.Canvas.Font.Assign(Self.Font);
    B.Canvas.Brush.Color := Self.Color;

    PaintDefault := True;
    DoDrawBackground(B.Canvas, R, pstPrePaint, PaintDefault);
    if PaintDefault then begin
      if not FBorders then begin
        R2 := R;
        InflateRect(R2, 3, 3);
        DrawBackground(B.Canvas, R2);
      end
      else
        DrawBackground(B.Canvas, R);
    end;
    PaintDefault := True;
    DoDrawBackground(B.Canvas, R, pstPostPaint, PaintDefault);

    ACanvas.Draw(ARect.Left, ARect.Top, B);
  finally
    B.Canvas.UnLock;
    B.Free;
  end;
end;

procedure TSpTBXCustomPanel.CMSpTBXControlsInvalidate(var Message: TMessage);
begin
  InvalidateBackground;
  Message.Result := 1;
end;

procedure TSpTBXCustomPanel.WMEraseBkgnd(var Message: TMessage);
begin
  if not DoubleBuffered or (Message.wParam = Message.lParam) then begin
    if not FBackgroundValid then begin
      FBackground.Width := ClientWidth;
      FBackground.Height := ClientHeight;
      EraseTBXBackground(FBackground.Canvas, ClientRect);
      FBackgroundValid := True;
    end;

    BitBlt(TWMEraseBkgnd(Message).DC, 0, 0, ClientWidth, ClientHeight, FBackground.Canvas.Handle, 0, 0, SRCCOPY)
  end;
  Message.Result := 1;
end;

procedure TSpTBXCustomPanel.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  InvalidateBackground;
end;

procedure TSpTBXCustomPanel.TBMThemeChange(var Message: TMessage);
begin
  inherited;
  if Message.WParam = TSC_AFTERVIEWCHANGE then
    InvalidateBackground;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXPanel }

procedure TSpTBXPanel.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  InflateRect(Rect, -1, -1);
end;

procedure TSpTBXPanel.DrawBackground(ACanvas: TCanvas; ARect: TRect);
begin
  if (SpXPThemeType(ThemeType) <> thtNone) and not TBXStyleBackground and FHotTrack then
    SpDrawXPEditFrame(ACanvas, ARect, Enabled, FHotTracking, ThemeType, True)
  else
    inherited;
end;

procedure TSpTBXPanel.CMFocusChanged(var Message: TCMFocusChanged);
begin
  inherited;
  if FHotTrack and Assigned(Message.Sender) then begin
    FChildFocused := SpFindControl(Self, Message.Sender) > -1;
    if FChildFocused <> FHotTracking then
      SetHotTracking(FChildFocused);
  end;
end;

procedure TSpTBXPanel.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if FHotTrack and not FHotTracking then
    SetHotTracking(True);
end;

procedure TSpTBXPanel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FHotTrack and FHotTracking and not FChildFocused then
    SetHotTracking(False);
end;

procedure TSpTBXPanel.SetHotTrack(const Value: Boolean);
begin
  if FHotTrack <> Value then begin
    FHotTrack := Value;
    InvalidateBackground(False);
  end;
end;

procedure TSpTBXPanel.SetHotTracking(const Value: Boolean);
begin
  if SpXPThemeType(ThemeType) <> thtWindows then begin
    FHotTracking := Value;
    InvalidateBackground(False);
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomGroupBox }

constructor TSpTBXCustomGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csSetCaption];
  Width := 185;
  Height := 105;
end;

procedure TSpTBXCustomGroupBox.AdjustClientRect(var Rect: TRect);
var
  R: TRect;
  H: Integer;
begin
  inherited AdjustClientRect(Rect);
  Canvas.Font := Font;
  R := Rect;
  H := SpDrawXPText(Canvas, '0', R, DT_SINGLELINE or DT_CALCRECT);
  Inc(Rect.Top, H);
  InflateRect(Rect, -1, -1);
  if Ctl3d then InflateRect(Rect, -1, -1);
end;

procedure TSpTBXCustomGroupBox.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and SpCanFocus(Self) then begin
      SelectFirst;
      Result := 1;
    end
    else
      inherited;
end;

procedure TSpTBXCustomGroupBox.CMTextChanged(var Message: TMessage);
begin
  inherited;
  InvalidateBackground(False);
  Realign;
end;

procedure TSpTBXCustomGroupBox.DrawBackground(ACanvas: TCanvas; ARect: TRect);
var
  Flags: Cardinal;
begin
  Flags := DT_SINGLELINE;
  if UseRightToLeftAlignment then
    Flags := Flags or DT_RTLREADING;
  SpDrawXPGroupBox(ACanvas, ARect, Caption, Flags, True, TBXStyleBackground, ThemeType);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXSplitter }

constructor TSpTBXCustomSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 5;
  Height := 100;
  Align := alLeft;
  TabStop := False;
  Cursor := crSizeWE;
  Borders := False;
  FResizeStyle := rsUpdate;
  FOldSize := -1;
  FMinSize := 0;
  FGripSize := 50;
  FGripHotTrack := True;
end;

destructor TSpTBXCustomSplitter.Destroy;
begin
  FreeAndNil(FMouseBrush);
  inherited Destroy;
end;

procedure TSpTBXCustomSplitter.RequestAlign;
begin
  inherited;
  if Align in [alTop, alBottom] then Cursor:= crSizeNS
  else Cursor:= crSizeWE;
end;

function TSpTBXCustomSplitter.IsVertical: Boolean;
begin
  Result := not (Align in [alTop, alBottom]);
end;

function TSpTBXCustomSplitter.ValidateSplitControl(var SplitControl: TControl): Boolean;
// Find the control that the splitter must resize
var
  P: TPoint;
  I: Integer;
  R: TRect;
  C: TControl;
begin
  Result := False;
  SplitControl := nil;

  P := Point(Left, Top);
  case Align of
    alLeft: Dec(P.X);
    alRight: Inc(P.X, Width);
    alTop: Dec(P.Y);
    alBottom: Inc(P.Y, Height);
  else
    Exit;
  end;

  for I := 0 to Parent.ControlCount - 1 do begin
    C := Parent.Controls[I];
    // Not a Splitter nor a regular Toolbar Dock
    if C.Visible and not ((C is TSpTBXCustomSplitter) or (C is TSplitter))
      and not ((C is TTBDock) and not (C is TTBXMultiDock)) then
    begin
      R := C.BoundsRect;
      if (R.Right - R.Left) = 0 then
        if Align in [alTop, alLeft] then Dec(R.Left)
        else Inc(R.Right);
      if (R.Bottom - R.Top) = 0 then
        if Align in [alTop, alLeft] then Dec(R.Top)
        else Inc(R.Bottom);

      if PtInRect(R, P) then begin
        SplitControl := C;
        Result := True;
        Break;
      end;
    end;
  end;
end;

procedure TSpTBXCustomSplitter.MouseAllocateLineDC;
begin
  FMouseLineDC := GetDCEx(Parent.Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS or DCX_LOCKWINDOWUPDATE);
  if ResizeStyle = rsPattern then begin
    if FMouseBrush = nil then begin
      FMouseBrush:= TBrush.Create;
      FMouseBrush.Bitmap:= AllocPatternBitmap(clBlack,clWhite);
    end;
    FMousePrevBrush := SelectObject(FMouseLineDC, FMouseBrush.Handle);
  end;
end;

procedure TSpTBXCustomSplitter.MouseReleaseLineDC;
begin
  if FMousePrevBrush <> 0 then SelectObject(FMouseLineDC, FMousePrevBrush);
  ReleaseDC(Parent.Handle, FMouseLineDC);
  if FMouseBrush <> nil then
    FreeAndNil(FMouseBrush);
end;

procedure TSpTBXCustomSplitter.MouseDrawLine;
var
  P: TPoint;
begin
  FMouseLineVisible := not FMouseLineVisible;
  P := Point(Left, Top);
  if IsVertical then
    P.X := Left + FSplit
  else
    P.Y := Top + FSplit;
  PatBlt(FMouseLineDC, P.X, P.Y, Width, Height, PATINVERT);
end;

procedure TSpTBXCustomSplitter.MouseCalcSplitSize(X, Y: Integer; var NewSize,
  Split: Integer);
var
  I: Integer;
begin
  if Assigned(FMouseSplitControl) then begin
    if IsVertical then
      Split := X - FMouseDownPos.X
    else
      Split := Y - FMouseDownPos.Y;

    I := 0;
    case Align of
      alLeft:   I := FMouseSplitControl.Width + Split;
      alRight:  I := FMouseSplitControl.Width - Split;
      alTop:    I := FMouseSplitControl.Height + Split;
      alBottom: I := FMouseSplitControl.Height - Split;
    end;

    NewSize := I;
    if I < FMinSize then
      NewSize := FMinSize
    else
      if I > FMaxSize then  // Use the Maximum Size
        NewSize := FMaxSize;

    if I <> NewSize then begin
      if Align in [alRight, alBottom] then
        I := I - NewSize
      else
        I := NewSize - I;
      Inc(Split, I);
    end;
  end;
end;

procedure TSpTBXCustomSplitter.MouseStopSizing;
begin
  if Assigned(FMouseSplitControl) then begin
    if FMouseLineVisible then MouseDrawLine;
    FMouseSplitControl := nil;
    MouseReleaseLineDC;
    if Assigned(FMouseActiveControl) then begin
      TWinControlAccess(FMouseActiveControl).OnKeyDown := FOldKeyDown;
      FMouseActiveControl := nil;
    end;
  end;
  DoMoved;
  FMoving:= False;
end;

procedure TSpTBXCustomSplitter.MouseFocusKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then MouseStopSizing
  else if Assigned(FOldKeyDown) then FOldKeyDown(Sender,Key,Shift);
end;

procedure TSpTBXCustomSplitter.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  C: TControl;
  F: TCustomForm;
  P: TPoint;
  I: Integer;
begin
  inherited;

  if (Button = mbLeft) and not (ssDouble in Shift) and ValidateSplitControl(C) then begin
    P := Point(X,Y);
    if (FGripSize > 0) and PtInRect(GripRect, P) then
      FMouseDownOnGrip := True;

    FMouseSplitControl := C;
    FMouseDownPos := P;

    if Assigned(FMouseSplitControl) then begin
      if Align in [alLeft, alRight] then begin
        FMaxSize := Parent.ClientWidth - FMinSize;
        for I := 0 to Parent.ControlCount - 1 do
          with Parent.Controls[I] do
            if Visible and (Align in [alLeft, alRight]) then Dec(FMaxSize, Width);
        Inc(FMaxSize, FMouseSplitControl.Width);
      end
      else begin
        FMaxSize := Parent.ClientHeight - FMinSize;
        for I := 0 to Parent.ControlCount - 1 do
          with Parent.Controls[I] do
            if Align in [alTop, alBottom] then Dec(FMaxSize, Height);
        Inc(FMaxSize, FMouseSplitControl.Height);
      end;
    end;

    MouseCalcSplitSize(X, Y, FNewSize, FSplit);
    MouseAllocateLineDC;

    // When the ESC key is pressed we must abort the moving with StopSizing,
    // for that we must intercept the key event from the Active control.}
    F := ValidParentForm(Self);
    if Assigned(F) then
      if F.ActiveControl <> nil then begin
        FMouseActiveControl := F.ActiveControl;
        FOldKeyDown := TWinControlAccess(FMouseActiveControl).OnKeyDown;
        TWinControlAccess(FMouseActiveControl).OnKeyDown := MouseFocusKeyDown;
      end;

    if ResizeStyle in [rsLine, rsPattern] then MouseDrawLine;
  end;
end;

procedure TSpTBXCustomSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I, Split: Integer;
  GripR: TRect;
  MouseInGrip: Boolean;
begin
  inherited;

  if (ssLeft in Shift) and not (ssDouble in Shift) and Assigned(FMouseSplitControl) then begin
    MouseCalcSplitSize(X, Y, I, Split);
    if DoMoving(I) then begin
      FMoving := True;
      if ResizeStyle in [rsLine, rsPattern] then MouseDrawLine;
      FNewSize := I;
      FSplit := Split;
      case ResizeStyle of
        rsUpdate: UpdateControlSize(FMouseSplitControl);
        rsLine, rsPattern: MouseDrawLine;
      end;
    end;
  end;

  // Track the mouse to invalidate the Grip when the mouse enters or leaves the grip zone
  if (FGripSize > 0) and not FMoving then begin
    GripR := GripRect;
    MouseInGrip := PtInRect(GripR, Point(X, Y));
    if (MouseInGrip <> FMouseOverGrip) then begin
      FMouseOverGrip := MouseInGrip;
      InvalidateGrip;
    end;
  end;
end;

procedure TSpTBXCustomSplitter.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  inherited;

  if (Button = mbLeft) and not (ssDouble in Shift) and Assigned(FMouseSplitControl) then begin
    P := Point(X, Y);
    if ResizeStyle in [rsLine, rsPattern] then MouseDrawLine;
    UpdateControlSize(FMouseSplitControl);
    MouseStopSizing;

    if (FGripSize > 0) and FMouseDownOnGrip then begin
      if (P.X = FMouseDownPos.X) and (P.Y = FMouseDownPos.Y) and PtInRect(GripRect, P) then
        Toggle;
      FMouseDownOnGrip := False;
      FMouseOverGrip := False;
      InvalidateGrip;
    end;
  end;
end;

procedure TSpTBXCustomSplitter.CMMouseleave(var Message: TMessage);
begin
  inherited;
  if FMouseOverGrip and not FMoving then begin
    FMouseOverGrip := False;
    InvalidateGrip;
  end;
end;

procedure TSpTBXCustomSplitter.ChangeSplitControlSize(NewControlSize: Integer);
var
  C: TControl;
begin
  if not (csDesigning in ComponentState) and ValidateSplitControl(C) then begin
    if NewControlSize < FMinSize then
      NewControlSize := FMinSize;
    if DoMoving(NewControlSize) then begin
      // If minimizing save restore position
      if NewControlSize = FMinSize then begin
        if IsVertical then FRestorePos := C.Width
        else FRestorePos := C.Height;
      end;

      FMoving := True;
      FNewSize := NewControlSize;
      UpdateControlSize(C);
    end;
    FMoving := False;
  end;
end;

function TSpTBXCustomSplitter.GetMinimized: Boolean;
var
  I, MinW, MinH: Integer;
  C: TControl;
begin
  Result := False;
  if ValidateSplitControl(C) then begin
    MinW := C.Constraints.MinWidth;
    MinH := C.Constraints.MinHeight;

    if FMinSize > MinW then
      MinW := FMinSize;
    if FMinSize > MinH then
      MinH := FMinSize;

    if IsVertical then begin
      I := C.Width;
      Result := I = MinW;
    end
    else begin
      I := C.Height;
      Result := I = MinH;
    end;
  end;
end;

procedure TSpTBXCustomSplitter.Minimize;
begin
  ChangeSplitControlSize(0);
end;

procedure TSpTBXCustomSplitter.Restore;
begin
  if Minimized then
    ChangeSplitControlSize(FRestorePos);
end;

procedure TSpTBXCustomSplitter.Toggle;
begin
  if Minimized then
    Restore
  else
    Minimize;
end;

procedure TSpTBXCustomSplitter.DoMoved;
begin
  if Assigned(FOnMoved) then FOnMoved(Self);
end;

function TSpTBXCustomSplitter.DoMoving(var NewSize: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnMoving) then FOnMoving(Self, NewSize, Result);
  if Result and (NewSize < FMinSize) then NewSize := 0;
end;

procedure TSpTBXCustomSplitter.SetMinSize(const Value: integer);
begin
  if (Value <> FMinSize) and (Value >= 0) then FMinSize := Value;
end;

procedure TSpTBXCustomSplitter.UpdateControlSize(SplitControl: TControl);
begin
  { Delphi bug: the controls aligning is based on the Controls array when they
    have the same position.
    For example:
    Form1.DisableAlign;
    try
      Control1.Left:= 10;  Control1.Top:= 10;
      Control1.Align:= alLeft;
      Control2.Left:= 10;  Control2.Top:= 10;
      Control2.Align:= alLeft;
      // Control1 has the aligning priority because it has lower index in the Controls array
    finally
      Form1.EnableAlign;
    end;

    This will affect the splitter when the splitter is Minimized or Maximazed to 0.
    Thats why the standard Borland TSplitter must minimize to a value higher than 0.

    Solution: we must move the splitter before or after the other control
    based on the Align property, for that we use SendToBack and BringToFront methods.
    SendToBack and BringToFront besides of changing the "z-order" changes the
    order of the control in the Controls array.

    When Align is alLeft or alTop we should make the splitter to have the
    aligning priority using BringToFront.
    And when Align is alRight or alBottom we will use SendToBack. }

  if (FOldSize <> FNewSize) and Assigned(SplitControl) then begin
    case Align of
      alLeft:
        begin
          BringToFront;
          if SplitControl is TTBXMultiDock then
            SpDkMultiDockResize(TTBXMultiDock(SplitControl), FNewSize)
          else
            SplitControl.Width := FNewSize;
          FOldSize := SplitControl.Width;
        end;
      alTop:
        begin
          BringToFront;
          if SplitControl is TTBXMultiDock then
            SpDkMultiDockResize(TTBXMultiDock(SplitControl), FNewSize)
          else
            SplitControl.Height := FNewSize;
          FOldSize := SplitControl.Height;
        end;
      alRight:
        begin
          Parent.DisableAlign;
          try
            SendToBack;
            if SplitControl is TTBXMultiDock then
              SpDkMultiDockResize(TTBXMultiDock(SplitControl), FNewSize)
            else
              SplitControl.Width := FNewSize;
            FOldSize := SplitControl.Width;
          finally
            Parent.EnableAlign;
          end;
        end;
      alBottom:
        begin
          Parent.DisableAlign;
          try
            SendToBack;
            if SplitControl is TTBXMultiDock then
              SpDkMultiDockResize(TTBXMultiDock(SplitControl), FNewSize)
            else
              SplitControl.Height := FNewSize;
            FOldSize := SplitControl.Height;
          finally
            Parent.EnableAlign;
          end;
        end;
    end;
    Update;
    DoMoved;
  end;
end;

function TSpTBXCustomSplitter.GetGripRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if FGripSize > 0 then
    if IsVertical then
      Result := Bounds(0, (Height - FGripSize) div 2, Width, FGripSize)
    else
      Result := Bounds((Width - FGripSize) div 2, 0, FGripSize, Height);
end;

procedure TSpTBXCustomSplitter.SetGripSize(const Value: Integer);
begin
  if FGripSize <> Value then begin
    if Value < 0 then FGripSize := 0
    else FGripSize := Value;
    InvalidateGrip;
  end;
end;

procedure TSpTBXCustomSplitter.InvalidateGrip;
var
  R: TRect;
begin
  if FGripHotTrack then begin
    R := GripRect;
    InvalidateBackground(False);
  end;
end;

procedure TSpTBXCustomSplitter.DrawBackground(ACanvas: TCanvas; ARect: TRect);
var
  EffectiveColor: TColor;
  T: TSpTBXThemeType;
begin
  T := SpXPThemeType(ThemeType);

  if T = thtTBX then begin
    EffectiveColor := CurrentTheme.GetViewColor(TVT_NORMALTOOLBAR);
    // [TBXTheme-Change]
    // For Stripes, Roma and Nexos themes draw the dock background
    if (TBXCurrentTheme = 'Stripes') or (TBXCurrentTheme = 'Roma') or (Pos('Nexos', TBXCurrentTheme) > 0) then begin
      ACanvas.Brush.Color := EffectiveColor;
      ACanvas.FillRect(ARect);
    end
    else
      CurrentTheme.PaintBackgnd(ACanvas, ARect, ARect, ARect, EffectiveColor, False, TVT_NORMALTOOLBAR);
  end;
end;

procedure TSpTBXCustomSplitter.DoDrawBackground(ACanvas: TCanvas;
  ARect: TRect; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
var
  R, DragHandleR: TRect;
begin
  inherited DoDrawBackground(ACanvas, ARect, PaintStage, PaintDefault);

  if PaintDefault and (PaintStage = pstPostPaint) and (ThemeType = thtTBX) then begin
    R := GripRect;

    // [TBXTheme-Change]
    // Don't paint the grip hottrack when using the Default theme
    if FMouseOverGrip and (TBXCurrentTheme <> 'Default') then
      SpDrawXPButton(ACanvas, R, True, False, True, False, False, False, thtTBX);

    DragHandleR := R;
    if IsVertical then
      InflateRect(DragHandleR, -1, -10)
    else
      InflateRect(DragHandleR, -10, -1);
    SpDrawXPGrip(ACanvas, DragHandleR, True, IsVertical);
  end;
end;


//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXTextObjectActionLink }

procedure TSpTBXTextObjectActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FUnicodeClient := AClient as TSpTBXTextObject;
end;

function TSpTBXTextObjectActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and
    (FUnicodeClient.Checked = (Action as TCustomAction).Checked);
end;

procedure TSpTBXTextObjectActionLink.SetCaption(const Value: String);
begin
  if IsCaptionLinked then
    if Action is TTntAction then
      FUnicodeClient.Caption := TntActnList.TntAction_GetNewCaption(Action as TCustomAction, Value)
    else
      FUnicodeClient.Caption := Value;
end;

procedure TSpTBXTextObjectActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then FUnicodeClient.Checked := Value;
end;

procedure TSpTBXTextObjectActionLink.SetHint(const Value: String);
begin
  if IsHintLinked then
    if Action is TTntAction then
      FUnicodeClient.Hint := TntActnList.TntAction_GetNewHint(Action as TCustomAction, Value)
    else
      FUnicodeClient.Hint := Value;
end;

procedure TSpTBXTextObjectActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then FUnicodeClient.ImageIndex := Value;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXTextObject }

constructor TSpTBXTextObject.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csAcceptsControls] + [csDoubleClicks];

  FImageIndex := -1;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;

  FLinkFont := TFont.Create;
  FLinkFont.Color := clBlue;
  FLinkFont.Style := [fsUnderline];

  FCaptionGlowColor := clYellow;
  FDisabledIconCorrection := True;

  Autosize := True;
  TabStop := True;
  SmartFocus := True;
  SpaceAsClick := True;
  FAlignment := taLeftJustify;
  FDrawPushedCaption := False;
  FThemeType := thtTBX;
end;

destructor TSpTBXTextObject.Destroy;
begin
  FImageChangeLink.Free;
  FLinkFont.Free;

  inherited;
end;

procedure TSpTBXTextObject.CreateWindowHandle(const Params: TCreateParams);
begin
  CreateUnicodeHandle(Self, Params, '');
end;

procedure TSpTBXTextObject.DefineProperties(Filer: TFiler);
begin
  inherited;
  // Don't let the streaming system store the WideStrings,
  // we need to store them manually
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TSpTBXTextObject.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TSpTBXTextObjectActionLink;
end;

procedure TSpTBXTextObject.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited;

  if Action is TTntAction then
    with TTntAction(Sender) do begin
      if not CheckDefaults or (Self.Caption = '') then
        Self.Caption := Caption;
      if not CheckDefaults or (Self.Hint = '') then
        Self.Hint := Hint;
    end;

  if Sender is TCustomAction then
    with TCustomAction(Sender) do begin
      if not CheckDefaults or (Self.Checked = False) then
        Self.Checked := Checked;
      if not CheckDefaults or (Self.ImageIndex = -1) then
        Self.ImageIndex := ImageIndex;
    end;
end;

procedure TSpTBXTextObject.AdjustFont(AFont: TFont);
begin
  if (FLinkText <> '') and MouseInControl then
    AFont.Assign(FLinkFont)
  else
    inherited;
end;

function TSpTBXTextObject.CanFocus: Boolean;
begin
  Result := False;
end;

procedure TSpTBXTextObject.Click;
begin
  if not (csLoading in ComponentState) then begin
    Invalidate;
    inherited;
    ExecuteLink;
  end;
end;

function TSpTBXTextObject.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
var
  R, R1, R2: TRect;
begin
  if ([csReading, csLoading] * ComponentState = []) and AutoSize then begin
    GetSize(R, R1, R2);
    NewHeight := R.Bottom - R.Top;
    if Wrapping = twNone then
      NewWidth := R.Right - R.Left;
    Result := True;
  end
  else
    Result := False;
end;

procedure TSpTBXTextObject.DoAdjustHeight(ACanvas: TCanvas; var NewHeight: Integer);
var
  R, R1, R2: TRect;
begin
  // DoAdjustHeight changes the Height of the control when Autosize is setted.
  // There should be an AdjustBounds method instead, like TCustomLabel.
  // We use this method to change the Width and the Height, NewHeight
  // should be the current Height

  GetSize(R, R1, R2);
  NewHeight := R.Bottom - R.Top; // the height is setted by AdjustHeight
  if Wrapping = twNone then
    Width := R.Right - R.Left; // set the width
end;

procedure TSpTBXTextObject.DoDrawHint(AHintBitmap: TBitmap;
  var AHint: Widestring; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawHint) then FOnDrawHint(Self, AHintBitmap, AHint, PaintDefault);
end;

function TSpTBXTextObject.DoDrawItem(ACanvas: TCanvas; ARect: TRect;
  const PaintStage: TSpTBXPaintStage): Boolean;
begin
  Result := True;
  if Assigned(FOnDraw) then FOnDraw(Self, ACanvas, ARect, PaintStage, Result);
end;

function TSpTBXTextObject.DoDrawText(ACanvas: TCanvas; var Rect: TRect;
  Flags: Longint): Integer;
var
  PaintDefault: Boolean;
  I: integer;
  IL: TCustomImageList;
  ILSize: TPoint;
  R, R1, R2: TRect;
  WS: WideString;
  TextFlags: Cardinal;
begin
  Result := 0;
  WS := Caption;
  TextFlags := Flags;
  if TextFlags and DT_CALCRECT = 0 then begin
    ACanvas.Brush.Style := bsClear;
    PaintDefault := True;
    if Assigned(FOnDrawCaption) then
      FOnDrawCaption(Self, ACanvas, ClientRect, WS, Rect, TextFlags, False, pstPrePaint, PaintDefault);
    if PaintDefault then begin
      IL := FImages;
      I := FImageIndex;
      DoGetImageIndex(IL, I);
      if Assigned(IL) and (I > -1) and (I < IL.Count) then
        ILSize := Point(IL.Width, IL.Height)
      else
        ILSize := Point(0, 0);
      if Enabled then
        Result := SpDrawXPText(ACanvas, Rect, WS, FCaptionGlow, FCaptionGlowColor,
          GetRealAlignment(Self), TextFlags, IL, I, ghlGlyphLeft, Enabled, DrawPushedCaption and Pushed, FDisabledIconCorrection, R1, R2)
      else begin
        if SpXPThemeType(FThemeType) = thtNone then begin
          ACanvas.Font.Color := clBtnHighlight;
          SpCalcXPText(ACanvas, Rect, WS, GetRealAlignment(Self), TextFlags, ILSize.X, ILSize.Y, ghlGlyphLeft, DrawPushedCaption and Pushed, R1, R2);
          OffsetRect(R1, 1, 1);
          SpDrawXPText(ACanvas, WS, R1, TextFlags);
          ACanvas.Font.Color := clBtnShadow;
        end
        else
          ACanvas.Font.Color := clBtnShadow;
        Result := SpDrawXPText(ACanvas, Rect, WS, gldNone, clNone,
          GetRealAlignment(Self), TextFlags, IL, I, ghlGlyphLeft, Enabled, DrawPushedCaption and Pushed, FDisabledIconCorrection, R1, R2);
      end;
    end;

    PaintDefault := True;
    if Assigned(FOnDrawCaption) then
      FOnDrawCaption(Self, ACanvas, ClientRect, WS, Rect, TextFlags, False, pstPostPaint, PaintDefault);
    if PaintDefault then
      if GetFocused then begin
        R := ClientRect;
        ApplyMargins(R, Margins);
        SpDrawFocusRect(ACanvas, GetFocusRect(R, R1, R2));
      end;
  end
  else
    Result := SpDrawXPText(ACanvas, WS, Rect, TextFlags);
end;

procedure TSpTBXTextObject.DoGetImageIndex(var AImageList: TCustomImageList; var AImageIndex: integer);
begin
  if Assigned(FOnGetImageIndex) then FOnGetImageIndex(Self, AImageList, AImageIndex);
end;

procedure TSpTBXTextObject.DoMouseEnter;
begin
  inherited;
  Invalidate;
end;

procedure TSpTBXTextObject.DoMouseLeave;
begin
  inherited;
  Invalidate;
end;

procedure TSpTBXTextObject.ExecuteLink;
var
  FilenameS, ParamsS: String;
begin
  if FLinkText <> '' then begin
    if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then begin
      FilenameS := FLinkText;
      ParamsS := FLinkTextParams;
      ShellExecute(Application.Handle, 'open', PChar(FilenameS), PChar(ParamsS), '', SW_SHOWNORMAL);
    end
    else
      ShellExecuteW(Application.Handle, 'open', PWideChar(FLinkText), PWideChar(FLinkTextParams), '', SW_SHOWNORMAL);
  end;
end;

function TSpTBXTextObject.GetControlsAlignment: TAlignment;
begin
  Result := FAlignment;
end;

function TSpTBXTextObject.GetTextAlignment: TAlignment;
begin
  Result := FAlignment;
end;

function TSpTBXTextObject.GetTextFlags: Cardinal;
const
  Alignments: array [TAlignment] of Integer = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array [TTextWrapping] of Integer = (DT_SINGLELINE,
    DT_SINGLELINE or DT_END_ELLIPSIS,
    DT_SINGLELINE or DT_PATH_ELLIPSIS, DT_WORDBREAK);
  ShowAccelChars: array [Boolean] of Integer = (DT_NOPREFIX, 0);
begin
  Result := DT_EXPANDTABS or WordWraps[Wrapping] or
    Alignments[GetRealAlignment(Self)] or ShowAccelChars[ShowAccelChar];
  Result := DrawTextBiDiModeFlags(Result);
end;

function TSpTBXTextObject.GetFocusRect(R, TextR, GlyphR: TRect): TRect;
begin
  if Caption = '' then
    Result := Rect(0, 0, 0, 0)
  else begin
    InflateRect(TextR, 1, 1);
    Result := TextR;
  end;
end;

function TSpTBXTextObject.GetFocused: Boolean;
begin
  Result := Focused;
end;

function TSpTBXTextObject.GetPushed: Boolean;
begin
  Result := (inherited Pushed) and MouseInControl;
end;

procedure TSpTBXTextObject.GetSize(out TotalR, TextR, GlyphR: TRect);
var
  I: integer;
  IL: TCustomImageList;
  R: TRect;
begin
  // Size of Text + Glyph + TextMargin + Margins
  IL := FImages;
  I := FImageIndex;
  DoGetImageIndex(IL, I);

  R := ClientRect;
  ApplyMargins(R, Margins);
  ApplyMargins(R, GetTextMargins);

  Canvas.Font.Assign(Font);
  AdjustFont(Canvas.Font);

  SpDrawXPText(Canvas, R, Caption, FCaptionGlow, FCaptionGlowColor, GetRealAlignment(Self), GetTextFlags,
    IL, I, ghlGlyphLeft, Enabled, DrawPushedCaption and Pushed, FDisabledIconCorrection, TextR, GlyphR, True);

  UnionRect(TotalR, TextR, GlyphR);

  if Autosize then
    with Margins do begin
      Inc(TotalR.Right, Left + Right);
      Inc(TotalR.Bottom, Top + Bottom);
    end;
end;

function TSpTBXTextObject.GetTextMargins: TRect;
begin
  Result := Rect(0, 0, 0, 0);
end;

procedure TSpTBXTextObject.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // Fix a TBX bug, TTBXTextObject can't recieve focus if it's inside a Panel
  inherited;
  if SmartFocus and CanFocus then
    SetFocus;
end;

procedure TSpTBXTextObject.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = Images then SetImages(nil);
end;

procedure TSpTBXTextObject.Paint;
var
  R, TextR: TRect;
begin
  R := ClientRect;
  ApplyMargins(R, Margins);
  Canvas.Font.Assign(Font);
  AdjustFont(Canvas.Font);
  // Draw the background
  DoDrawItem(Canvas, R, pstPrePaint);
  // Draw the text
  TextR := R;
  ApplyMargins(TextR, GetTextMargins);
  DoDrawText(Canvas, TextR, GetTextFlags);
  // Draw the Focus, Icon and Text
  DoDrawItem(Canvas, R, pstPostPaint);
end;

procedure TSpTBXTextObject.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then begin
    FAlignment := Value;
    Invalidate;
  end;
end;

function TSpTBXTextObject.IsImageShown: Boolean;
var
  I: integer;
  IL: TCustomImageList;
begin
  I := FImageIndex;
  IL := FImages;
  DoGetImageIndex(IL, I);
  Result := Assigned(IL) and (I > -1) and (I < IL.Count);
end;

function TSpTBXTextObject.IsCaptionStored: Boolean;
begin
  Result := TntControl_IsCaptionStored(Self);
end;

function TSpTBXTextObject.GetCaption: TWideCaption;
begin
  Result := TntControl_GetText(Self);
end;

procedure TSpTBXTextObject.SetCaption(const Value: TWideCaption);
begin
  TntControl_SetText(Self, Value);
end;

procedure TSpTBXTextObject.SetCaptionGlow(const Value: TSpGlowDirection);
begin
  if FCaptionGlow <> Value then begin
    FCaptionGlow := Value;
    Invalidate;
  end;
end;

procedure TSpTBXTextObject.SetCaptionGlowColor(const Value: TColor);
begin
  if FCaptionGlowColor <> Value then begin
    FCaptionGlowColor := Value;
    Invalidate;
  end;
end;

function TSpTBXTextObject.GetChecked: Boolean;
begin
  Result := FChecked;
end;

procedure TSpTBXTextObject.SetChecked(Value: Boolean);
begin
  if Value <> FChecked then begin
    FChecked := Value;
    Invalidate;
  end;
end;

function TSpTBXTextObject.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self);
end;

function TSpTBXTextObject.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self);
end;

procedure TSpTBXTextObject.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TSpTBXTextObject.ImageListChange(Sender: TObject);
begin
  if Sender = Images then begin
    Invalidate;
    AdjustHeight;
  end;
end;

procedure TSpTBXTextObject.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then begin
    FImageIndex := Value;
    if Assigned(Images) then Invalidate;
    AdjustHeight;
  end;
end;

procedure TSpTBXTextObject.SetImages(const Value: TCustomImageList);
begin
  if FImages <> nil then FImages.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if FImages <> nil then begin
    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
  end;
  Invalidate;
  AdjustHeight;
end;

procedure TSpTBXTextObject.SetLinkFont(const Value: TFont);
begin
  FLinkFont.Assign(Value);
end;

procedure TSpTBXTextObject.SetThemeType(const Value: TSpTBXThemeType);
begin
  if Value <> FThemeType then begin
    FThemeType := Value;
    Invalidate;
  end;
end;

procedure TSpTBXTextObject.CMHintShow(var Message: TCMHintShow);
// Handle the CM_HINTSHOW message to show unicode hints using
// a custom THintWindow.
var
  HintInfo: PHintInfo;
  WideHint: Widestring;
  R, TextR: TRect;
  PaintDefault: Boolean;
begin
  if Assigned(FOnDrawHint) then begin
    WideHint := Hint;
    // Init HintBitmap
    SpStockHintBitmap.Canvas.Font.Assign(Self.Font);
    SpStockHintBitmap.Canvas.Font.Color := clInfoText;
    SpStockHintBitmap.Canvas.Pen.Color := clBlack;
    SpStockHintBitmap.Canvas.Brush.Color := clInfoBk;
    TextR := Rect(0, 0, 1, 1);
    SpDrawXPText(SpStockHintBitmap.Canvas, WideHint, TextR, DT_NOPREFIX or DT_CALCRECT);
    SpStockHintBitmap.Width := TextR.Right + 8;
    SpStockHintBitmap.Height := TextR.Bottom + 4;
    // Draw the hint in the HintBitmap
    PaintDefault := True;
    DoDrawHint(SpStockHintBitmap, WideHint, PaintDefault);
    if PaintDefault then
      inherited
    else begin
      // Prepare the HintInfo
      HintInfo := Message.HintInfo;
      HintInfo.HintStr := '';
      HintInfo.CursorRect := ClientRect;
      HintInfo.HintWindowClass := TBitmapHint;   //custom HintWindow class
      HintInfo.HintData := SpStockHintBitmap;  //TApplication.ActivateHint will pass the data to the HintWindow
      HintInfo.HintStr := WideHint;
      //HintInfo.HideTimeout := 60000; //1 minute

      R := Rect(0, 0, SpStockHintBitmap.Width, SpStockHintBitmap.Height);
      SpStockHintBitmap.Canvas.FillRect(R);
      OffsetRect(TextR, ((R.Right - TextR.Right) div 2) - 2, (R.Bottom - TextR.Bottom) div 2);
      SpDrawXPText(SpStockHintBitmap.Canvas, WideHint, TextR, DT_NOPREFIX);
    end;
  end
  else
    inherited;
end;

procedure TSpTBXTextObject.WMEraseBkgnd(var Message: TMessage);
begin
  if not DoubleBuffered or (Message.wParam = Message.lParam) then
  begin
    if Color = clNone then
      SpDrawParentBackground(Self, TWMEraseBkgnd(Message).DC, ClientRect)
    else
      FillRectEx(TWMEraseBkgnd(Message).DC, ClientRect, Color);
  end;
  Message.Result := 1;
end;

procedure TSpTBXTextObject.WMSetCursor(var Message: TWMSetCursor);
begin
  if not (csDesigning in ComponentState) and (Message.CursorWnd = Handle) and
    (FLinkText <> '') and MouseInControl and (Screen.Cursor = crDefault) then
  begin
    // Apply Hand cursor only if there is no other cursor assigned.
    if SpStockHandCursor <> 0 then
      Windows.SetCursor(SpStockHandCursor)
    else
      Windows.SetCursor(Screen.Cursors[Cursor]);
    Message.Result := 1;
  end
  else
    inherited;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomLabel }

procedure TSpTBXCustomLabel.CMDialogChar(var Message: TCMDialogChar);
begin
  if Assigned(FFocusControl) and IsAccel(Message.CharCode, Caption) and SpCanFocus(FFocusControl) then begin
    FFocusControl.SetFocus;
    Message.Result := 1;
  end
  else
    inherited;
end;

constructor TSpTBXCustomLabel.Create(AOwner: TComponent);
begin
  inherited;
  FUnderlineColor := clBtnShadow;
  TabStop := False;
end;

function TSpTBXCustomLabel.DoDrawItem(ACanvas: TCanvas; ARect: TRect;
  const PaintStage: TSpTBXPaintStage): Boolean;
var
  C: TColor;
begin
  Result := inherited DoDrawItem(ACanvas, ARect, PaintStage);
  if Result and (PaintStage = pstPrePaint) and FUnderline then begin
    C := ACanvas.Pen.Color;
    try
      ACanvas.Pen.Color := UnderlineColor;
      ACanvas.MoveTo(ARect.Left, ARect.Bottom - 1);
      ACanvas.LineTo(ARect.Right, ARect.Bottom - 1);
    finally
      ACanvas.Pen.Color := C;
    end;
  end;
end;

procedure TSpTBXCustomLabel.GetSize(out TotalR, TextR, GlyphR: TRect);
begin
  inherited GetSize(TotalR, TextR, GlyphR);
  if FUnderline then
    Inc(TotalR.Bottom);
end;

procedure TSpTBXCustomLabel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FocusControl then SetFocusControl(nil);
end;

procedure TSpTBXCustomLabel.SetFocusControl(const Value: TWinControl);
begin
  if FFocusControl <> Value then
  begin
    FFocusControl := Value;
    if FFocusControl <> nil then FFocusControl.FreeNotification(Self);
  end;
end;

procedure TSpTBXCustomLabel.SetUnderline(const Value: Boolean);
begin
  if Value <> FUnderline then begin
    FUnderline := Value;
    Invalidate;
    AdjustHeight;
  end;
end;

procedure TSpTBXCustomLabel.SetUnderlineColor(const Value: TColor);
begin
  FUnderlineColor := Value;
  Invalidate;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXButtonControl }

constructor TSpTBXButtonControl.Create(AOwner: TComponent);
begin
  inherited;
  FGroupIndex := 0;
end;

function TSpTBXButtonControl.CanFocus: Boolean;
var
  Control: TWinControl;
  Form: TCustomForm;
begin
  Result := False;
  Form := GetParentForm(Self);
  if (Form <> nil) and Form.Visible and Form.Enabled then begin
    Control := Self;
    while Control <> Form do
    begin
      if not (Control.Visible and Control.Enabled) then Exit;
      Control := Control.Parent;
    end;
    Result := True;
  end;
end;

procedure TSpTBXButtonControl.Toggle;
begin
//
end;

procedure TSpTBXButtonControl.UpdateExclusive;
var
  Msg: TMessage;
begin
  if (FGroupIndex <> -1) and Assigned(Parent) then begin
    Msg.Msg := CM_SPGROUPINDEXUPDATE;
    Msg.WParam := FGroupIndex;
    Msg.LParam := Longint(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

procedure TSpTBXButtonControl.SetGroupIndex(const Value: Integer);
begin
  if FGroupIndex <> Value then begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

procedure TSpTBXButtonControl.SetChecked(Value: Boolean);
begin
  inherited;
  if Value then UpdateExclusive;
end;

procedure TSpTBXButtonControl.CMDialogChar(var Message: TCMDialogChar);
begin
  if Enabled and ShowAccelChar and IsAccel(Message.CharCode, Caption) and
    CanFocus and Visible then
  begin
    SetFocus;
    Click;
    Message.Result := 1;
  end
  else
    inherited;
end;

procedure TSpTBXButtonControl.CMDialogKey(var Message: TCMDialogKey);
begin
//  if (Message.CharCode = VK_RETURN) and Focused and
//    (KeyDataToShiftState(Message.KeyData) = []) then
//  begin
//    Click;
//    Message.Result := 1;
//  end
//  else
    inherited;
end;

procedure TSpTBXButtonControl.CMSPGroupIndexUpdate(var Message: TMessage);
var
  Sender: TComponent;
  SenderButton: TSpTBXButtonControl;
begin
  if Message.WParam = FGroupIndex then begin
    Sender := TComponent(Message.LParam);
    if (Sender <> Self) and (Sender is TSpTBXButtonControl) and (Sender.ClassType = Self.ClassType) then begin
      SenderButton := Sender as TSpTBXButtonControl;
      if SenderButton.Checked and Checked then
        Checked := False;
    end;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomCheckBox }

constructor TSpTBXCustomCheckBox.Create(AOwner: TComponent);
begin
  inherited;
  FAllowGrayed := False;
  FState := cbUnchecked;
  FGroupIndex := -1;
end;

procedure TSpTBXCustomCheckBox.Click;
begin
  if StateChanged then
    inherited
  else
    Toggle; // Toggle calls OnClick
end;

procedure TSpTBXCustomCheckBox.DoGetImageIndex(var AImageList: TCustomImageList;
  var AImageIndex: integer);
var
  B: TBitmap;
  R, TR, GR: TRect;
  MaskColor: TColor;
begin
  inherited DoGetImageIndex(AImageList, AImageIndex);
  if not Assigned(AImageList) then begin
    B := TBitmap.Create;
    try
      B.Width := 13;
      B.Height := 13;

      SpStockImageList.Clear;
      SpStockImageList.Width := B.Width;
      SpStockImageList.Height := B.Height;

      // The WindowsXP checkbox glyph blends the background
      if Color = clNone then begin
        R := ClientRect;
        ApplyMargins(R, Margins);
        ApplyMargins(R, GetTextMargins);
        B.Canvas.Font.Assign(Font);
        SpDrawXPText(Canvas, R, Caption, gldNone, clWhite, GetRealAlignment(Self), GetTextFlags,
          B, ghlGlyphLeft, DrawPushedCaption and Pushed, TR, GR, True);
        MaskColor := Canvas.Pixels[GR.Left, GR.Top];
      end
      else
        MaskColor := Color;

      R := Rect(0, 0, B.Width, B.Height);
      B.Canvas.Brush.Color := MaskColor;
      B.Canvas.FillRect(R);
      DisabledIconCorrection := False;
      SpDrawXPCheckBoxGlyph(B.Canvas, R, Enabled, State, MouseInControl, Pushed, Focused, ThemeType);

      SpStockImageList.AddMasked(B, MaskColor);
      AImageList := SpStockImageList;
      AImageIndex := 0;
    finally
      B.Free;
    end;
  end;
end;

function TSpTBXCustomCheckBox.GetChecked: Boolean;
begin
  Result := FState = cbChecked;
end;

procedure TSpTBXCustomCheckBox.SetChecked(Value: Boolean);
begin
  if Checked <> Value then begin
    inherited;
    if Value then SetState(cbChecked)
    else SetState(cbUnchecked);
  end;
end;

procedure TSpTBXCustomCheckBox.SetState(const Value: TCheckBoxState);
begin
  if (FState <> Value) then begin
    FState := Value;

    // When State is changed OnClick must be fired
    StateChanged := True;
    try
      Click;
    finally
      StateChanged := False;
    end;
  end;
end;

procedure TSpTBXCustomCheckBox.Toggle;
begin
  case State of
    cbUnchecked: if AllowGrayed then SetState(cbGrayed) else SetState(cbChecked);
    cbChecked: SetState(cbUnchecked);
    cbGrayed: SetState(cbChecked);
  end;
end;

procedure TSpTBXCustomCheckBox.GetSize(out TotalR, TextR, GlyphR: TRect);
begin
  inherited GetSize(TotalR, TextR, GlyphR);
  // Inc TotalR for the FocusRect
  if Autosize then begin
    Inc(TotalR.Right);
    Inc(TotalR.Bottom, 2);
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomRadioButton }

constructor TSpTBXCustomRadioButton.Create(AOwner: TComponent);
begin
  inherited;
  TabStop := False;
end;

procedure TSpTBXCustomRadioButton.Click;
begin
  if StateChanged then
    inherited
  else
    if not Checked then Toggle; // Toggle calls OnClick
end;

procedure TSpTBXCustomRadioButton.CMFocusChanged(var Message: TCMFocusChanged);
begin
  inherited;
  if Focused then
    Toggle;
end;

procedure TSpTBXCustomRadioButton.DoGetImageIndex(var AImageList: TCustomImageList;
  var AImageIndex: integer);
var
  B: TBitmap;
  R, TR, GR: TRect;
  MaskColor: TColor;
begin
  inherited DoGetImageIndex(AImageList, AImageIndex);
  if not Assigned(AImageList) then begin
    B := TBitmap.Create;
    try
      B.Width := 13;
      B.Height := 13;

      SpStockImageList.Clear;
      SpStockImageList.Width := B.Width;
      SpStockImageList.Height := B.Height;

      // The WindowsXP radiobutton glyph blends the background
      if Color = clNone then begin
        R := ClientRect;
        ApplyMargins(R, Margins);
        ApplyMargins(R, GetTextMargins);
        B.Canvas.Font.Assign(Font);
        SpDrawXPText(Canvas, R, Caption, gldNone, clWhite, GetRealAlignment(Self), GetTextFlags,
          B, ghlGlyphLeft, DrawPushedCaption and Pushed, TR, GR, True);
        MaskColor := Canvas.Pixels[GR.Left, GR.Top];
      end
      else
        MaskColor := Color;

      R := Rect(0, 0, B.Width, B.Height);
      B.Canvas.Brush.Color := MaskColor;
      B.Canvas.FillRect(R);
      SpDrawXPRadioButtonGlyph(B.Canvas, R, Enabled, Checked, MouseInControl,
        Pushed, Focused, ThemeType);

      SpStockImageList.AddMasked(B, MaskColor);
      AImageList := SpStockImageList;
      AImageIndex := 0;
    finally
      B.Free;
    end;
  end;
end;

procedure TSpTBXCustomRadioButton.GetSize(out TotalR, TextR, GlyphR: TRect);
begin
  inherited GetSize(TotalR, TextR, GlyphR);
  // Inc TotalR for the focusrect
  if Autosize then begin
    Inc(TotalR.Right);
    Inc(TotalR.Bottom, 2);
  end;
end;

procedure TSpTBXCustomRadioButton.SetChecked(Value: Boolean);
var
  WasChecked: Boolean;
begin
  WasChecked := Checked;
  inherited;
  TabStop := Value;
  // When Checked is true OnClick must be fired
  if not WasChecked and Value then begin
    StateChanged := True;
    try
      Click;
    finally
      StateChanged := False;
    end;
  end;
end;

procedure TSpTBXCustomRadioButton.Toggle;
begin
  if not Checked then Checked := True;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXRadioGroupButton }

type
  TSpTBXRadioGroupButton = class(TSpTBXRadioButton)
  public
    constructor InternalCreate(RadioGroup: TSpTBXCustomRadioGroup);
    destructor Destroy; override;
  end;

constructor TSpTBXRadioGroupButton.InternalCreate(RadioGroup: TSpTBXCustomRadioGroup);
begin
  inherited Create(RadioGroup);
  RadioGroup.FButtons.Add(Self);
  Parent := RadioGroup;
  AutoSize := False;
  Visible := False;
  Enabled := RadioGroup.Enabled;
  ParentShowHint := False;
  TabStop := False;
  OnClick := RadioGroup.ButtonClick;
end;

destructor TSpTBXRadioGroupButton.Destroy;
begin
  TSpTBXCustomRadioGroup(Owner).FButtons.Remove(Self);
  inherited Destroy;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomRadioGroup }

constructor TSpTBXCustomRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls];
  FButtons := TList.Create;
  FItems := TTntStringList.Create;
  TTntStringList(FItems).OnChange := ItemsChange;
  FItemIndex := -1;
  FColumns := 1;
end;

destructor TSpTBXCustomRadioGroup.Destroy;
begin
  SetButtonCount(0);
  TTntStringList(FItems).OnChange := nil;
  FItems.Free;
  FButtons.Free;
  inherited Destroy;
end;

procedure TSpTBXCustomRadioGroup.FlipChildren(AllLevels: Boolean);
begin
  { The radio buttons are flipped using BiDiMode }
end;

procedure TSpTBXCustomRadioGroup.ArrangeButtons;
var
  ButtonsPerCol, ButtonWidth, ButtonHeight, TopMargin, I: Integer;
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
  DeferHandle: THandle;
  ALeft: Integer;
begin
  if (FButtons.Count <> 0) and not FReading then
  begin
    DC := GetDC(0);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
    ReleaseDC(0, DC);
    ButtonsPerCol := (FButtons.Count + FColumns - 1) div FColumns;
    ButtonWidth := (Width - 10) div FColumns;
    I := Height - Metrics.tmHeight - 5;
    ButtonHeight := I div ButtonsPerCol;
    TopMargin := Metrics.tmHeight + 1 + (I mod ButtonsPerCol) div 2;
    DeferHandle := BeginDeferWindowPos(FButtons.Count);
    try
      for I := 0 to FButtons.Count - 1 do
        with TSpTBXRadioGroupButton(FButtons[I]) do
        begin
          BiDiMode := Self.BiDiMode;
          ALeft := (I div ButtonsPerCol) * ButtonWidth + 8;
          if UseRightToLeftAlignment then
            ALeft := Self.ClientWidth - ALeft - ButtonWidth;
          DeferHandle := DeferWindowPos(DeferHandle, Handle, 0,
            ALeft,
            (I mod ButtonsPerCol) * ButtonHeight + TopMargin,
            ButtonWidth, ButtonHeight,
            SWP_NOZORDER or SWP_NOACTIVATE);
          Visible := True;
        end;
    finally
      EndDeferWindowPos(DeferHandle);
    end;
  end;
end;

procedure TSpTBXCustomRadioGroup.ButtonClick(Sender: TObject);
begin
  if not FUpdating then begin
    FItemIndex := FButtons.IndexOf(Sender);
    Changed;
    Click;
  end;
end;

procedure TSpTBXCustomRadioGroup.InvalidateBackground(InvalidateChildren: Boolean);
var
  I: Integer;
  T: TSpTBXThemeType;
begin
  inherited;

  if not InvalidateChildren and not (csDestroying in ComponentState) then
    if HandleAllocated then begin
      T := ThemeType;
      for I := 0 to FButtons.Count - 1 do
        Buttons[I].ThemeType := T;
    end;
end;

procedure TSpTBXCustomRadioGroup.ItemsChange(Sender: TObject);
begin
  if not FReading then begin
    if FItemIndex >= FItems.Count then FItemIndex := FItems.Count - 1;
    UpdateButtons;
  end;
end;

procedure TSpTBXCustomRadioGroup.Loaded;
begin
  inherited Loaded;
  ArrangeButtons;
end;

procedure TSpTBXCustomRadioGroup.ReadState(Reader: TReader);
begin
  FReading := True;
  try
    inherited ReadState(Reader);
  finally
    FReading := False;
  end;
  UpdateButtons;
end;

procedure TSpTBXCustomRadioGroup.SetButtonCount(Value: Integer);
begin
  while FButtons.Count < Value do
    TSpTBXRadioGroupButton.InternalCreate(Self);
  while FButtons.Count > Value do
    TSpTBXRadioGroupButton(FButtons.Last).Free;
end;

procedure TSpTBXCustomRadioGroup.SetColumns(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if Value > 16 then Value := 16;
  if FColumns <> Value then begin
    FColumns := Value;
    ArrangeButtons;
    Invalidate;
  end;
end;

procedure TSpTBXCustomRadioGroup.SetFocus;
begin
  inherited;
  if Enabled and (FItemIndex > -1) then
    GetButtons(FItemIndex).SetFocus;
end;

procedure TSpTBXCustomRadioGroup.SetItemIndex(Value: Integer);
begin
  if FReading then
    FItemIndex := Value
  else begin
    if Value < -1 then Value := -1;
    if Value >= FButtons.Count then Value := FButtons.Count - 1;
    if FItemIndex <> Value then
    begin
      if FItemIndex >= 0 then
        GetButtons(FItemIndex).Checked := False;
      FItemIndex := Value;
      if FItemIndex >= 0 then
        GetButtons(FItemIndex).Checked := True;
    end;
  end;
end;

procedure TSpTBXCustomRadioGroup.SetItems(Value: TTntStrings);
begin
  FItems.Assign(Value);
end;

procedure TSpTBXCustomRadioGroup.UpdateButtons;
var
  I: Integer;
begin
  SetButtonCount(FItems.Count);
  for I := 0 to FButtons.Count - 1 do
    Buttons[I].Caption := FItems[I];
  if FItemIndex >= 0 then begin
    FUpdating := True;
    try
      GetButtons(FItemIndex).Checked := True;
    finally
      FUpdating := False;
    end;
  end;
  ArrangeButtons;
  Invalidate;
end;

procedure TSpTBXCustomRadioGroup.CMEnabledChanged(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  for I := 0 to FButtons.Count - 1 do
    GetButtons(I).Enabled := Enabled;
end;

procedure TSpTBXCustomRadioGroup.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ArrangeButtons;
end;

procedure TSpTBXCustomRadioGroup.WMSize(var Message: TWMSize);
begin
  inherited;
  ArrangeButtons;
end;

procedure TSpTBXCustomRadioGroup.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  // Do nothing
end;

function TSpTBXCustomRadioGroup.GetButtons(Index: Integer): TSpTBXRadioButton;
begin
  Result := TSpTBXRadioButton(FButtons[Index]);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomButton }

constructor TSpTBXCustomButton.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csDoubleClicks];

  FBitmapTransparent := True;
  FBitmap := TBitmap.Create;
  FBitmap.OnChange := BitmapChanged;

  FPopupControl := Self;
  FDropDownArrow := True;
  Alignment := taCenter;
  Autosize := False;
  DrawPushedCaption := True;
  ThemeType := thtWindows;
  Width := 75;
  Height := 25;
end;

destructor TSpTBXCustomButton.Destroy;
begin
  StopRepeat;
  FBitmap.Free;
  inherited;
end;

procedure TSpTBXCustomButton.AdjustFont(AFont: TFont);
var
  ItemInfo: TTBXItemInfo;
begin
  inherited;
  if FThemeType = thtTBX then begin
    SpFillItemInfo(Enabled, Pushed, MouseInControl, Checked, ItemInfo);
    Canvas.Font.Color := CurrentTheme.GetItemTextColor(ItemInfo);
  end;
end;

procedure TSpTBXCustomButton.BitmapChanged(Sender: TObject);
begin
  Invalidate;
end;

function TSpTBXCustomButton.BitmapValid: boolean;
begin
  Result := (Bitmap <> nil) and (not Bitmap.Empty) and (Bitmap.Height mod ConstStatesCount = 0);
end;

procedure TSpTBXCustomButton.Click;
var
  P: TPoint;
  R: TRect;
  Form: TCustomForm;
  SpTBXPopup: ISpTBXPopupMenu;
  Dummy: TTBCustomItem;

  procedure RemoveClicks;
  var
    RepostList: TList;
    Repost: Boolean;
    I: Integer;
    Msg: TMsg;
    P: TPoint;
  begin
    RepostList := TList.Create;
    try
      while PeekMessage(Msg, 0, WM_LBUTTONDOWN, WM_MBUTTONDBLCLK, PM_REMOVE) do
        with Msg do
        begin
          Repost := True;
          case Message of
            WM_QUIT: begin
                { Throw back any WM_QUIT messages }
                PostQuitMessage(wParam);
                Break;
              end;
            WM_LBUTTONDOWN, WM_LBUTTONDBLCLK,
            WM_RBUTTONDOWN, WM_RBUTTONDBLCLK,
            WM_MBUTTONDOWN, WM_MBUTTONDBLCLK: begin
                P := SmallPointToPoint(TSmallPoint(lParam));
                Windows.ClientToScreen(hwnd, P);
                if FindDragTarget(P, True) = Self then Repost := False;
              end;
          end;
          if Repost then
          begin
            RepostList.Add(AllocMem(SizeOf(TMsg)));
            PMsg(RepostList.Last)^ := Msg;
          end;
        end;

      for I := 0 to RepostList.Count-1 do
      begin
        with PMsg(RepostList[I])^ do PostMessage(hwnd, message, wParam, lParam);
        FreeMem(RepostList[I]);
      end;
    finally
      RepostList.Free;
    end;
  end;

begin
  if not FRepeating then begin
    if Assigned(FDropdownMenu) then begin
      FDropDownMenuVisible := True;
      try
        MouseLeft;
        MouseCapture := False;
        FDropDownMenu.PopupComponent := Self;

        if FDropDownMenu.GetInterface(ISpTBXPopupMenu, SpTBXPopup) then begin
          if not SpTBXPopup.InternalPopup(0, 0, Dummy, FPopupControl) then
            FDropDownMenuVisible := False;
        end
        else begin
          if FDropdownMenu is TTBXPopupMenu then begin
            R := FPopupControl.ClientRect;
            R.TopLeft := ClientToScreen(R.TopLeft);
            R.BottomRight := ClientToScreen(R.BottomRight);
            TTBXPopupMenu(DropDownMenu).PopupEx(R);
          end
          else begin
            P := ClientToScreen(Point(0, Height));
            DropDownMenu.Popup(P.X, P.Y);
          end;
          FDropDownMenuVisible := False;
        end;
      finally
        Invalidate;
        RemoveClicks;
      end;
      Exit; // don't call the Click handler if the DropDownMenu is shown
    end
    else begin
      Form := GetParentForm(Self);
      if Assigned(Form) then Form.ModalResult := FModalResult;
    end;
  end;
  inherited;
end;

function TSpTBXCustomButton.DoDrawDropDownArrow(ACanvas: TCanvas;
  ARect: TRect): Boolean;
var
  ItemInfo: TTBXItemInfo;
  TR: TRect;
begin
  Result := True;
  if FDropDownArrow and Assigned(FDropDownMenu) then begin
    SpFillItemInfo(Enabled, Pushed, True, Checked, ItemInfo);
    TR := ARect;
    TR.Left := TR.Right - GetTextMargins.Right;
    if not DrawPushedCaption then
      ItemInfo.Pushed := False;
    CurrentTheme.PaintDropDownArrow(ACanvas, TR, ItemInfo);
  end;
end;

function TSpTBXCustomButton.DoDrawItem(ACanvas: TCanvas; ARect: TRect;
  const PaintStage: TSpTBXPaintStage): Boolean;
var
  B: TBitmap;
  T: TSpTBXThemeType;
begin
  T := SpXPThemeType(ThemeType);

  Result := inherited DoDrawItem(ACanvas, ARect, PaintStage);
  if Result and (PaintStage = pstPrePaint) then begin
    if BitmapValid then begin
      B := TBitmap.Create;
      try
        B.Width := ARect.Right - ARect.Left;
        B.Height := ARect.Bottom - ARect.Top;
        SetStretchBltMode(B.Canvas.Handle, COLORONCOLOR);
        B.Canvas.CopyRect(ARect, Bitmap.Canvas, GetSkinStateRect);
        if FBitmapTransparent then
          B.Transparent := True;
        ACanvas.Draw(0, 0, B);
      finally
        B.Free;
      end;
    end
    else begin
      case T of
        thtNone, thtWindows:
          SpDrawXPButton(ACanvas, ARect, Enabled, Pushed, MouseInControl, Checked, False, FDefault or Focused, T);
        thtTBX:
          SpDrawXPButton(ACanvas, ARect, Enabled, Pushed, MouseInControl, Checked, False, False, T);
      end;
    end;
  end;

  // Draw the button arrow
  if Result and (PaintStage = pstPostPaint) then
    DoDrawDropDownArrow(ACanvas, ARect);
end;

function TSpTBXCustomButton.GetFocusRect(R, TextR, GlyphR: TRect): TRect;
begin
  Result := R;
  if SpXPThemeType(ThemeType) = thtNone then
    InflateRect(Result, -4, -4)
  else
    InflateRect(Result, -3, -3);
end;

function TSpTBXCustomButton.GetFocused: Boolean;
begin
  Result := Focused and (IsDroppedDown or (inherited GetFocused));
end;

function TSpTBXCustomButton.GetPushed: Boolean;
begin
  Result := IsDroppedDown or (inherited GetPushed);
end;

function TSpTBXCustomButton.GetTextMargins: TRect;
begin
  Result := Rect(8, 2, 8, 2);
  if FDropDownArrow and Assigned(FDropdownMenu) then
    Inc(Result.Right, CurrentTheme.DropDownArrowWidth + 1);
end;

function TSpTBXCustomButton.IsDroppedDown: Boolean;
begin
  Result := FDropDownMenuVisible;
end;

function TSpTBXCustomButton.GetSkinStateRect: TRect;
var
  W, H: integer;
begin
  // Finds the skin rect based on the button state
  Result := Rect(0, 0, 0, 0);

  if BitmapValid then begin
    W := Bitmap.Width;
    H := (Bitmap.Height div ConstStatesCount); // 4 states
    if not Enabled then
      Result := Bounds(0, H * 3, W, H)  // 4th state (disabled)
    else begin
      if Checked or Pushed then
        Result := Bounds(0, H * 2, W, H)  // 3rd state (down)
      else
        if MouseInControl then
          Result := Bounds(0, H * 1, W, H)  // 2nd state (hottrack)
        else
          Result := Bounds(0, H * 0, W, H); // 1st state (up)
    end;
  end;
end;

procedure TSpTBXCustomButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Enabled and (Button = mbLeft) then begin
    if Repeating then begin
      Click;
      ControlState := ControlState - [csClicked];
      if not Assigned(FRepeatTimer) then FRepeatTimer := TTimer.Create(Self);
      FRepeatTimer.Interval := ConstInitRepeatPause;
      FRepeatTimer.OnTimer := RepeatTimerHandler;
      FRepeatTimer.Enabled := True;
    end;
  end;
end;

procedure TSpTBXCustomButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then StopRepeat;
end;

procedure TSpTBXCustomButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = DropdownMenu then DropdownMenu := nil;
end;

procedure TSpTBXCustomButton.RepeatTimerHandler(Sender: TObject);
begin
  FRepeatTimer.Interval := ConstRepeatPause;
  if Repeating then begin
    if Pushed then
      Click;
  end
  else
    StopRepeat;
end;

procedure TSpTBXCustomButton.StopRepeat;
begin
  if Assigned(FRepeatTimer) then begin
    FRepeatTimer.Free;
    FRepeatTimer := nil;
  end;
end;

procedure TSpTBXCustomButton.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
  Invalidate;
end;

procedure TSpTBXCustomButton.SetDefault(const Value: Boolean);
begin
  if Value <> FDefault then begin
    FDefault := Value;
    Invalidate;
  end;
end;

procedure TSpTBXCustomButton.SetDropDownArrow(const Value: Boolean);
begin
  if FDropDownArrow <> Value then begin
    FDropDownArrow := Value;
    Invalidate;
  end;
end;

procedure TSpTBXCustomButton.SetDropDownMenu(Value: TPopupMenu);
begin
  if FDropDownMenu <> Value then begin
    if Assigned(FDropDownMenu) then RemoveFreeNotification(FDropDownMenu);
    FDropDownMenu := Value;
    if Assigned(FDropDownMenu) then FreeNotification(FDropDownMenu);
    Invalidate;
  end;
end;

procedure TSpTBXCustomButton.CMDialogKey(var Message: TCMDialogKey);
begin
  with Message do
    if (((CharCode = VK_RETURN) and FActive) or
      ((CharCode = VK_ESCAPE) and FCancel)) and
      (KeyDataToShiftState(Message.KeyData) = []) and CanFocus then
    begin
      Click;
      Result := 1;
    end
    else
      inherited;
end;

procedure TSpTBXCustomButton.CMFocusChanged(var Message: TCMFocusChanged);
begin
  with Message do
    if Sender is TSpTBXCustomButton then
      FActive := Sender = Self
    else
      FActive := FDefault;
  inherited;
end;

procedure TSpTBXCustomButton.CMSPPopupClose(var Message: TMessage);
begin
  FDropDownMenuVisible := False;
  Invalidate;
  inherited;
end;

procedure TSpTBXCustomButton.WMCancelMode(var Message: TWMCancelMode);
begin
  inherited;
  StopRepeat;
  MouseLeft;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomSpeedButton }

constructor TSpTBXCustomSpeedButton.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csSetCaption];
  SetBounds(0, 0, 23, 22);
end;

function TSpTBXCustomSpeedButton.CanFocus: Boolean;
begin
  Result := False;
end;

procedure TSpTBXCustomSpeedButton.Click;
begin
  if FGroupIndex <> 0 then
    Checked := not Checked;
  inherited
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomProgressBar }

constructor TSpTBXCustomProgressBar.Create(AOwner: TComponent);
begin
  inherited;
  FMax := 100;
  FMin := 0;
  FPosition := 0;
  FProgressVisible := True;
  FCaptionGlow := gldAll;
  FCaptionType := pctPercentage;

  Alignment := taCenter;
  Autosize := False;
  Width := 150;
  Height := 17;
  ThemeType := thtWindows;
  Font.Style := Font.Style + [fsBold];
  TabStop := False;
end;

function TSpTBXCustomProgressBar.DoDrawItem(ACanvas: TCanvas; ARect: TRect;
  const PaintStage: TSpTBXPaintStage): Boolean;
var
  I: Integer;
  T: TSpTBXThemeType;
begin
  Result := inherited DoDrawItem(ACanvas, ARect, PaintStage);
  if Result and (PaintStage = pstPrePaint) then begin
    T := SpXPThemeType(ThemeType);
    I := SpDrawXPProgressBar(ACanvas, ARect, FVertical, FSmooth, FProgressVisible, FMin, FMax, FPosition, T);
    case FCaptionType of
      pctNone: Caption := '';
      pctPercentage: Caption := IntToStr(I) + '%';
      pctProgress: Caption := IntToStr(FPosition);
    end;
  end;
end;

procedure TSpTBXCustomProgressBar.DoProgressChange;
begin
  if Assigned(FOnProgressChange) then FOnProgressChange(Self, Position);
end;

function TSpTBXCustomProgressBar.GetTextMargins: TRect;
begin
  Result := Rect(8, 2, 8, 2);
end;

procedure TSpTBXCustomProgressBar.SetCaptionType(const Value: TSpTBXProgressCaption);
begin
  if FCaptionType <> Value then begin
    FCaptionType := Value;
    if Value <> pctDefault then Caption := '';
    Invalidate;
  end;
end;

procedure TSpTBXCustomProgressBar.SetMax(const Value: integer);
begin
  if FMax <> Value then begin
    FMax := Value;
    Invalidate;
  end;
end;

procedure TSpTBXCustomProgressBar.SetMin(const Value: integer);
begin
  if FMin <> Value then begin
    FMin := Value;
    Invalidate;
  end;
end;

procedure TSpTBXCustomProgressBar.SetPosition(Value: integer);
begin
  if Value > FMax then Value := FMax
  else if Value < FMin then Value := FMin;
  if FPosition <> Value then begin
    FPosition := Value;
    Invalidate;
    DoProgressChange;
  end;
end;

procedure TSpTBXCustomProgressBar.SetProgressVisible(const Value: Boolean);
begin
  if FProgressVisible <> Value then begin
    FProgressVisible := Value;
    Invalidate;
  end;
end;

procedure TSpTBXCustomProgressBar.SetSmooth(const Value: Boolean);
begin
  if FSmooth <> Value then begin
    FSmooth := Value;
    Invalidate;
  end;
end;

procedure TSpTBXCustomProgressBar.SetVertical(const Value: Boolean);
begin
  if FVertical <> Value then begin
    FVertical := Value;
    SetBounds(Left, Top, Height, Width);
    Invalidate;
  end;
end;

procedure TSpTBXCustomProgressBar.StepIt(Delta: Integer = 1);
begin
  SetPosition(FPosition + Delta);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXTrackBar }

constructor TSpTBXTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  FThemeType := thtWindows;
  FTickMarks := tmxBottomRight;
  AddThemeNotification(Self);
end;

destructor TSpTBXTrackBar.Destroy;
begin
  RemoveThemeNotification(Self);
  inherited;
end;

function TSpTBXTrackBar.ChannelRect: TRect;
var
  R: TRect;
begin
  // TBM_GETCHANNELRECT allways returns the horizontal channel rect, even
  // when the Orientation is vertical.
  SendMessage(Handle, TBM_GETCHANNELRECT, 0, Integer(@Result));
  if Orientation = trVertical then begin
    R := Result;
    Result := Rect(R.Top, R.Left, R.Bottom, R.Right);
  end;
end;

function TSpTBXTrackBar.DoDrawChannel(ACanvas: TCanvas; ARect: TRect;
  const PaintStage: TSpTBXPaintStage): Boolean;
begin
  Result := True;
  if Assigned(FOnDrawChannel) then FOnDrawChannel(Self, ACanvas, ARect, PaintStage, Result);
end;

function TSpTBXTrackBar.DoDrawChannelTics(ACanvas: TCanvas; X, Y: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnDrawChannelTics) then FOnDrawChannelTics(Self, ACanvas, X, Y, Result);
end;

function TSpTBXTrackBar.DoDrawThumb(ACanvas: TCanvas; ARect: TRect;
  const PaintStage: TSpTBXPaintStage): Boolean;
begin
  Result := True;
  if Assigned(FOnDrawThumb) then FOnDrawThumb(Self, ACanvas, ARect, PaintStage, Result);
end;

procedure TSpTBXTrackBar.DrawChannelTics(ACanvas: TCanvas);
var
  PosArray: array of Integer;
  I, Count, Y, iStart, iEnd: Integer;
  ChannelR, ThumbR: TRect;
begin
  // Returns the position of the tics on the client area
  // Check if Max - Min > 2 to see if the tics array is valid.
  Count := Max - Min;
  if Count < 2 then
    Count := 2
  else
    Count := Count + 1;
  SetLength(PosArray, Count);

  // Fill the array, the first and last tics are not included in the tics array:
  // http://msdn.microsoft.com/library/en-us/shellcc/platform/commctls/trackbar/messages/tbm_getticpos.asp?frame=true
  // First we need to get the middle tics
  //  0 1 2 3 4 5 6 7 8 9    // Tick positions seen on the trackbar.
  //    1 2 3 4 5 6 7 8      // Tick positions whose position can be identified.
  //    0 1 2 3 4 5 6 7      // Index numbers for the identifiable positions.
  if Count >= 2 then begin
    iStart := 1;
    iEnd := Count - 1 - 1;
    for I := iStart to iEnd do
      PosArray[I] := SendMessage(Self.Handle, TBM_GETTICPOS, I - 1, 0);
  end;

  // Calculate the first and last tics position
  SendMessage(Self.Handle, TBM_GETTHUMBRECT, 0, Integer(@ThumbR));
  ChannelR := ChannelRect;
  if Orientation = trHorizontal then begin
    I := (ThumbR.Right - ThumbR.Left) div 2;
    PosArray[0] := ChannelR.Left + I;
    PosArray[Count - 1] := ChannelR.Right - I - 1;
    Y := ChannelR.Top + (ChannelR.Bottom - ChannelR.Top) div 2;
    for I := 0 to Count - 1 do
      if DoDrawChannelTics(ACanvas, PosArray[I], Y) then
        ACanvas.Pixels[PosArray[I], Y] := clBtnShadow;
  end
  else begin
    I := (ThumbR.Bottom - ThumbR.Top) div 2;
    PosArray[0] := ChannelR.Top + I;
    PosArray[Count - 1] := ChannelR.Bottom - I - 1;
    Y := ChannelR.Left + (ChannelR.Right - ChannelR.Left) div 2;
    for I := 0 to Count - 1 do
      if DoDrawChannelTics(ACanvas, Y, PosArray[I]) then
        ACanvas.Pixels[Y, PosArray[I]] := clBtnShadow;
  end;
end;

function TSpTBXTrackBar.MouseInThumb: Boolean;
var
  P: TPoint;
  R: TRect;
begin
  if csDesigning in ComponentState then
    Result := False
  else begin
    SendMessage(Handle, TBM_GETTHUMBRECT, 0, Integer(@R));
    GetCursorPos(P);
    P := ScreenToClient(P);
    Result := PtInRect(R, P)
  end;

  if SpXPThemeType(ThemeType) = thtWindows then begin
    if Focused then Result := not (GetCaptureControl = Self);
  end
  else
    Result := GetCaptureControl = Self;
end;

procedure TSpTBXTrackBar.ForceRepaint;
begin
  // NM_CUSTOMDRAW messages are not sent when calling Invalidate, we must
  // recreate the window
  if HandleAllocated then RecreateWnd;
end;

procedure TSpTBXTrackBar.SetThemeType(const Value: TSpTBXThemeType);
begin
  if Value <> FThemeType then begin
    FThemeType := Value;
    ForceRepaint;
  end;
end;

procedure TSpTBXTrackBar.SetTickMarks(const Value: TSpTBXTickMark);
const
  A: array [TSpTBXTickMark] of TTickMark = (tmBottomRight, tmTopLeft, tmBoth, tmBoth);
begin
  if Value <> FTickMarks then begin
    if A[FTickMarks] = A[Value] then begin
      FTickMarks := Value;
      inherited TickMarks := A[Value];
      RecreateWnd;
    end
    else begin
      FTickMarks := Value;
      inherited TickMarks := A[Value];
    end;
  end;
end;

procedure TSpTBXTrackBar.CNNotify(var Message: TWMNotify);
var
  Info: PNMCustomDraw;
  ACanvas: TCanvas;
  R: TRect;
  Rgn: HRGN;
  Offset: Integer;
begin
  if Message.NMHdr.code = NM_CUSTOMDRAW then begin
    Message.Result := CDRF_DODEFAULT;
    Info := Pointer(Message.NMHdr);
    case Info.dwDrawStage of
      CDDS_PREPAINT:
        Message.Result := CDRF_NOTIFYITEMDRAW;
      CDDS_ITEMPREPAINT:
        begin
          ACanvas := TCanvas.Create;
          ACanvas.Lock;
          try
            ACanvas.Handle := Info.hdc;
            Case Info.dwItemSpec of
              TBCD_TICS:
                begin
                  R := ClientRect;
                  SpDrawParentBackground(Self, ACanvas.Handle, R);
                  if Focused then
                    SpDrawFocusRect(ACanvas, R);
                  if FTickMarks = tmxCenter then
                    Message.Result := CDRF_SKIPDEFAULT;
                end;
              TBCD_THUMB:
                begin
                  SendMessage(Handle, TBM_GETTHUMBRECT, 0, Integer(@R));
                  if DoDrawThumb(ACanvas, R, pstPrePaint) then
                    SpDrawXPTrackBar(ACanvas, R, TBCD_THUMB, Orientation = trVertical, MouseInThumb, FTickMarks, Min, Max, SelStart, SelEnd, FThemeType);
                  DoDrawThumb(ACanvas, R, pstPostPaint);
                  Message.Result := CDRF_SKIPDEFAULT;
                end;
              TBCD_CHANNEL:
                begin
                  SendMessage(Handle, TBM_GETTHUMBRECT, 0, Integer(@R));
                  Offset := 0;
                  if Focused then
                    Inc(Offset);
                  if Orientation = trHorizontal then begin
                    R.Left := ClientRect.Left + Offset;
                    R.Right := ClientRect.Right - Offset;
                  end
                  else begin
                    R.Top := ClientRect.Top + Offset;
                    R.Bottom := ClientRect.Bottom - Offset;
                  end;
                  with R do
                    Rgn := CreateRectRgn(Left, Top, Right, Bottom);
                  SelectClipRgn(Info.hDC, Rgn);
                  try
                    SpDrawParentBackground(Self, ACanvas.Handle, ClientRect);
                    R := ChannelRect;

                    if DoDrawChannel(ACanvas, R, pstPrePaint) then
                      SpDrawXPTrackBar(ACanvas, R, TBCD_CHANNEL, Orientation = trVertical, False, FTickMarks, Min, Max, SelStart, SelEnd, FThemeType);
                    DoDrawChannel(ACanvas, R, pstPostPaint);

                    // Draw channel tics
                    if FTickMarks = tmxCenter then
                      DrawChannelTics(ACanvas);
                  finally
                    DeleteObject(Rgn);
                    SelectClipRgn(Info.hDC, 0);
                  end;
                  Message.Result := CDRF_SKIPDEFAULT;
                end;
            end;
          finally
            ACanvas.Unlock;
            ACanvas.Handle := 0;
            ACanvas.Free;
          end;
        end;
    end;
  end;
end;

procedure TSpTBXTrackBar.TBMThemeChange(var Message: TMessage);
begin
  inherited;
  if Message.WParam = TSC_AFTERVIEWCHANGE then
    ForceRepaint;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM

procedure InitializeStock;
begin
  SpStockImageList := TImageList.Create(nil);
end;

procedure FinalizeStock;
begin
  SpStockImageList.Free;
end;

initialization
  InitializeStock;

finalization
  FinalizeStock;

end.
