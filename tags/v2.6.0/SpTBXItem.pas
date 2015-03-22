unit SpTBXItem;

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

Wish list for TB2K:
  - tboSameHeight option for toolbar items, used to stretch the item
    to the highest possible size.
  - It would be nice to have a way to get the TTBItemViewer width and height
    even when the Item is not visible (TTBItemViewer.BoundsRect is not valid
    when the item is not visible).
    As a workaround we should use TTBItemViewer.CalcSize method.
  - It would be nice to allow component writers to override
    TTBCustomToolbar.CreateWrapper so they can create custom TTBControlItem
    items descendants as the dropped control wrappers.
  - It would be nice to have access to the TTBPopupMenu Viewer before the
    popup is showed to initialize the items, for example to set the focus
    on a TTBEditItem.

Development notes:
  - All the Windows and Delphi bugs fixes are marked with '[Bugfix]'.
  - All the theme changes and adjustments are marked with '[Theme-Change]'.
  - All the compatibility changes are marked with '[Backward-Compatibility]'.
  - TSpTBXCompoundItemsControl is used as the base class for TB2K items enabled
    Controls, it uses the ITBItems interface and streams the items to the DFM.
  - When a control is dropped on the toolbar a TTBControlItem is created by
    TTBCustomToolbar.CreateWrapper, unfortunately it is created with the
    Toolbar.Owner instead of the Form (Owner.Owner for TSpTBXCompoundItemsControl
    like the TSpTBXTabSet or TSpTBXStatusBar). The workaround is to handle the
    CM_CONTROLCHANGE message in the compound toolbar, stream all the
    TTBControlItem.Control to the DFM (the TSpTBXCompoundItemsControl must be
    the parent in the DFM), and finally reset the TTBControlItem.Control
    parentship to the Toolbar in TSpTBXCompoundItemsControl.Loaded.

To Do:
  -

History:
28 October 2014 - version 2.5.2
  - Added support for Delphi XE7

28 May 2014 - version 2.5.1
  - Added support for Delphi XE6

18 March 2014 - version 2.5
  - Added support for Delphi XE4 and XE5
  - Removed support for older versions of Delphi, SpTBXLib
    supports Delphi 2009 or newer.

15 April 2013 - version 2.4.8
  - Minor bug fixes.
  - Removed SkinType property from all components.
  - Added support for Delphi XE3.

7 February 2012 - version 2.4.7
  - Minor bug fixes.
  - Added support for Delphi XE2.
  - Added support for 64 bit Delphi compiler.
  - Added support for Delphi Styles.

25 June 2011 - version 2.4.6
  - Fixed TSpTBXTitleBar bug, OnSystemMenuPopup was not fired
    when the system menu was showed, thanks to Sebastien for
    reporting this.

12 March 2010 - version 2.4.5
  - Fixed TSpTBXToolbar flicker when changing the floating toolbar
    caption, thanks to Albert Wiersch for reporting this.
  - Fixed incorrect TSpTBXDockablePanel floating border
    painting on Aero when Resizable was set to false, thanks to
    Erwin Denissen for reporting this.

2 December 2009 - version 2.4.4
  - Fixed bug in SpSetFormWindowState utility function, the Form
    was not restored correctly, thanks to Alex Yagolnik for
    reporting this.
  - Fixed incorrect TSpTBXPopupWindow items margins, thanks to
    Pedro Vugluskra for reporting this.

13 September 2009 - version 2.4.3
  - Fixed incorrect menu items painting on Vista, thanks to
    Maël Hörz and Sertac Akyuz for reporting this.
  - Fixed TSpTBXToolbar flicker when adding/deleting items.
  - Fixed incorrect TSpTBXStatusBar painting when XP theming was
    disabled, thanks to Warren Postma for reporting this.
  - Fixed incorrect TSpTBXDock painting when the Background
    property was used.
  - Made the NC area of the TSpTBXTitleBar clickeable when the
    form is maximized.

8 May 2009 - version 2.4.2
  - Added Dutch translation for the Customizer demo, thanks to
    Alfred Vink.
  - Added Tooltips Vista theming.

15 March 2009 - version 2.4.1
  - Fixed incorrect TntAction checking, thanks to Costas Stergiou
    for reporting this.

17 January 2009 - version 2.4
  - Replaced the default Windows taskbar PopupMenu with a skninned
    PopupMenu when TSpTBXTitleBar is used on the Main form.
  - Fixed incorrect menu separator painting when it was used on
    a ToolBoxPopup submenu, thanks to Yury Plashenkov for
    reporting this.
  - Fixed incorrect menu item margins, thanks to Eduardo Mauro
    for reporting this.

26 September 2008 - version 2.3
  - New Toolbar item added, TSpTBXColorItem.
  - Fixed incorrect Alt-Space handling on TSpTBXTitleBar, the
    System PopupMenu was still showed even when SystemMenu was
    setted to False, thanks to Ilya Zaytsev for reporting this.
  - Fixed incorrect items text sizing when SystemFont was setted
    to False, thanks to Minoru Yoshida for reporting this.
  - Fixed a Delphi 7 bug, D7 hints didn't support multi-monitors,
    thanks to Costas Stergiou for reporting this.

29 July 2008 - version 2.2
  - Fixed AV raised when Screen.MonitorFromWindow failed, thanks
    to Andrew Denton for reporting this.
  - Added OnClosePopup event to TSpTBXPopupMenu.

26 June 2008 - version 2.1
  - Added Turkish translation for the Customizer demo, thanks to
    Zylar.
  - Fixed incorrect keyboard handling on TSpTBXTitleBar, Alt+Space
    didn't popup the system menu, thanks to Costas Stergiou for
    reporting this.

3 May 2008 - version 2.0
  - SpTBXLib is now decoupled from TBX and uses the latest version
    of TB2K, with NO PATCHES REQUIRED.
  - Added HideEmptyPopup property to TSpTBXSubmenuItem, when
    the submenu has no items and this property is set the
    popup window will not be showed.
  - Added Swedish translation for the Customizer demo, thanks to
    Mattias Andersson.

2 April 2008 - version 1.9.5
  - Fixed incorrect item stretching when the toolbar was
    vertical and tboImageAboveCaption was set, thanks to
    John for reporting this.
  - Fixed incorrect item painting when no themes were
    used, thanks to Denis for reporting this.
  - Fixed incorrect TSpTBXToolPalette.Images handling, thanks
    to Minoru Yoshida for reporting this.
  - Fixed incorrect menu gutter painting when hints were showed,
    thanks to Michele and Beta Xiong for reporting this.
  - Fixed incorrect floating border painting when the default
    size was changed thanks to Costas Stergiou and Serge
    for reporting this.
  - Fixed incorrect floating border painting on Vista,
    the borders should not be transparent, thanks to
    Costas Stergiou for reporting this.

3 February 2008 - version 1.9.4
  - No changes.

19 January 2008 - version 1.9.3
  - Completed the Brazilian Portuguese translation of the Customizer
    demo, thanks to Mauricio Magnani.
  - Added TSpTBXToolWindow component.
  - Fixed incorrect gutter painting on the popup menus.
  - Fixed item stretching problems

26 December 2007 - version 1.9.2
  - New component added: TSpTBXToolWindow, a fully customizable 
    ToolWindow with unicode support.
  - Added State parameter to the toolbar items OnDrawCaption event.
  - Fixed AV on Delphi 2005, Margins and Paddings were introduced
    on Delphi 2006, thanks to Stefan for reporting this.
  - Minor bug fixes.

1 December 2007 - version 1.9.1
  - Added Invalidate method to TSpTBXItem.
  - Added OnClosePopup event to TSpTBXSubmenuItem.
  - Fixed tboNoRotation use on vertical toolbar items, when
    tboImageAboveCaption is not set the glyph should appear
    aligned with the caption.

20 November 2007 - version 1.9
  - New theme engine.
  - Added SpTBXHintWindowClass global variable, it specifies which
    THintWindow class is used to show the hints.
  - Added TSpTBXToolPalette and TSpTBXColorPalette items.
  - Added TSpTBXToolWindow component.
  - Removed TSpTBXComboBoxItem, TSpTBXDropDownItem, TSpTBXStringList
    and TSpTBXUndoList.

8 February 2007 - version 1.8.3
  - No changes.

17 December 2006 - version 1.8.2
  - Added Toolbar public property to TSpTBXStatusBar.

24 November 2006 - version 1.8.1
  - Added properties to TSpTBXRightAlignSpacer: ImageIndex, Images,
    OnAdjustFont, OnClick, OnDrawHint, OnDrawImage, OnDrawItem.
  - Added unicode shortcut-text support for menu items, thanks
    Steve for reporting this.
  - Fixed incorrect TSpTBXTitleBar positioning when the
    taskbar is moved around the screen, thanks to
    Costas Stergiou for reporting this.
  - Fixed incorrect TSpTBXStatusBar size grip painting when
    Windows XP themes are disabled, thanks to Alexey Naumov
    for reporting this.

27 August 2006 - version 1.8
  - Fixed incorrect DropdownCombo item painting, thanks to
    François Rivierre for reporting this.
  - Fixed incorrect TSpTBXTitleBar SystemMenu painting,
    GetMenuStringW doesn't work correctly on Win2K/WinXP,
    when a DBCS code page is active (e.g. Japanese), thanks
    to Jordan Russell for reporting this.
    http://news.jrsoftware.org/read/article.php?id=12268&group=jrsoftware.toolbar2000.thirdparty
  - Fixed bug in TSpTBXLabelItem, clicking a TSpTBXLabelItem
    on a popup menu causes the menu to close, thanks to
    Piotr Janus for reporting this.

15 June 2006 - version 1.7
  - Added vertical caption painting to toolbar items.
  - Added CaptionGlow and CaptionGlowColor properties to
    toolbar Items.
  - Added Margins property to toolbar items.
  - Fixed incorrect TSpTBXTitleBar resizing when the form is
    maximized and the titlebar is activated and deactivated
    multiple times, thanks to Costas Stergiou for reporting this.

4 May 2006 - version 1.6
  - Fixed incorrect TSpTBXStatusBar behavior, the size grip
    disappeared when the parent was a TSpTBXTitleBar, thanks to
    Costas Stergiou for reporting this.
  - Fixed incorrect TSpTBXStatusBar's size grip painting when the
    Default theme was used.
  - Fixed incorrect TSpTBXLabelItem painting when the label was used
    in a submenu, thanks to Costas Stergiou for reporting this.
  - Added OnSystemMenuPopup event to TSpTBXTitleBar.

12 April 2006 - version 1.5
  - Fixed incorrect TSpTBXStatusBar behavior, it didn't resized
    the form if the mouse click was on the non-client area of the
    status bar, thanks to Frank de Groot for reporting this.
  - Fixed incorrect mouse handling in TSpTBXTitlebar, thanks to
    Marten Pape for reporting this.
  - Fixed incorrect TSpTBXLabelItem alignment on menus, thanks to
    Costas Stergiou for reporting this.
  - Added ClickedItem parameter to TSpTBXSubmenuItem.OnClosePopup
    event.

27 February 2006 - version 1.4
  - Added SizeGrip property to TSpTBXStatusBar.
  - Added FullScreenMaximize property to TSpTBXTitleBar.

10 February 2006 - version 1.3
  - Fixed AV in TSpTBXTitleBar at designtime, thanks to
    Alexey Naumov for reporting this.
  - Fixed incorrect system popupmenu visibility in TSpTBXTitleBar.
  - Added Active property to TSpTBXTitleBar.
  - Added OnDrawBackground event to TSpTBXTitleBar.
  - Added OnClosePopup event to TSpTBXSubmenuItem.

28 December 2005 - version 1.2
  - Fixed incorrect items anchoring.
  - Fixed range check errrors.

18 October 2005 - version 1.1
  - Fixed incorrect TSpTBXStatusBar margins when the form is
    maximazed.
  - Fixed incorrect accel char handling in TSpTBXToolbar.
  - Fixed incorrect TntAction support, the previous version of
    TntActions didn't supported unicode enabled ActionLinks.
  - Fixed incorrect tab stop chars handling in TSpTBXItem.
  - Added MaxSize property to TSpTBXToolbar, determines
    the maximum height the toolbar can have.
  - Added TBX themes support to TSpTBXTitleBar's system menu.
  - Added radio item painting support.

18 August 2005 - version 1.0
  - Added DisplayOptions property to TSpTBXToolbar, determines
    whether the item's image is to be displayed.
  - Added Customizable property to TSpTBXToolbar, determines
    whether the toolbar is customizable or not.
  - Added TitleBarSize property to the Options of the
    TSpTBXTitlebar.

10 June 2005 - version 0.9
  - SpTBXLib may now alternatively, at your option, be used and/or
    distributed under the terms of the SpTBXLib License.
    Please see the updated LICENSE.TXT file for more information.

20 May 2005 - version 0.8
  - Fixed incorrect caption centering in TSpTBXItem.
  - Fixed incorrect TSpTBXItem hint when accessing TntApplication,
    thanks to Erik Maly for reporting this.
  - Changed the Options property of TSpTBXTitleBar to use a base class
    for default buttons.
  - Added ChevronVertical property to TSpTBXToolbar, it changes the
    layout of the chevron popup to be vertical.
  - Added Wrapping property to TSpTBXItem, it determines the wrapping
    type of the item's caption.
  - Added FixedSize property to TSpTBXTitleBar, it determines if
    the TitleBar can be resized.

16 February 2005 - version 0.7
  - Fixed TSpTBXThemeGroupItem theme sync bug, it now correctly selects
    the current TBX theme.
  - Fixed unicode support in W9x.
  - Fixed TSpTBXTitleBar painting flicker.
  - Fixed TSpTBXStatusBar right align margin.
  - Added TBXStyleBackground property to TSpTBXTitleBar, when setted to
    true it paints a TBX style background.
  - Added AutoCheck property to TSpTBXItem.

23 December 2004 - version 0.6
  - Fixed hint bug, ampersands were not removed in auto-generated hints.
  - Fixed incorrect caption painting when the font is italic, this bug
    is present in TBX items, TLabel, TBitBtn, TSpeedButton, TGroupBox,
    TRadioGroup, and any other control that uses DrawText to draw the
    caption. To reproduce this, drop a TBitBtn, change the caption to
    'WWW' and the font to italic, the last W is cropped.
  - Fixed incorrect caption painting when the Default theme is used,
    the caption was not painted in a down state when the toolbarstyle
    item was pushed, thanks Daniel Rikowski for reporting this.
  - Changed the default value of DisplayMode to nbdmDefault.
  - New component added, TSpTBXTitleBar: a fully customizable
    TitleBar with Unicode text and TBX themes support.
  - New component added, TSpTBXPopupMenu: a TTBXPopupMenu descendant
    with an OnPopupMenuInit event to setup the items before the popup
    is showed, it could be used for example to set the focus to an
    EditItem.
  - Added SpChangeThemeType utility function, this makes it easier to
    switch the theme type of any given control and its children.

30 August 2004 - version 0.5
  - Reworked the hint show event of the items.

21 July 2004 - version 0.4
  - Fixed TTBControlItem.Control streaming bug on TSpTBXStatusBar.
  - Fixed bad sync of the items unicode caption and hint properties
    when an Action was assigned.

12 July 2004 - version 0.3.1
  - Unchanged.

9 July 2004 - version 0.3
  - Fixed incorrect TSpTBXItem caption painting when DisplayMode
    was nbdmDefault, thanks to Cyril for reporting this.
  - Added anchors support for TTBControlItem items, if the associated
    Control is client aligned or has akRight in its Anchors property.
  - The theme items in TSpTBXThemeGroupItem are now sorted.
  - Added OnUpdate event to TSpTBXThemeGroupItem, this event is fired
    every time the theme items list is recreated, use this event
    to sort or change the items properties.

27 June 2004 - version 0.2
  - Fixed Toolbar custom painting event.
  - Fixed incorrect Shortcut painting in submenus.
  - Fixed incorrect Shortcut hint painting.
  - Removed thtBitmapSkin from TSpTBXThemeType.
  - Published more properties for TSpTBXLabelItem.
  - New Toolbar item added, TSpTBXSeparator.
  - New component added, TSpTBXStatusBar: a fully customizable
    StatusBar with Unicode text and TBX themes support.

22 June 2004 - version 0.1
  - Initial release.

==============================================================================}

interface

{$BOOLEVAL OFF} // Unit depends on short-circuit boolean evaluation

uses
  Windows, Messages, Classes, SysUtils, Forms, Controls, Graphics, ImgList,
  Menus, StdCtrls, ActnList,
  TB2Item, TB2Dock, TB2Toolbar, TB2ToolWindow,
  SpTBXSkins;

const
  C_SpTBXRadioGroupIndex = 8888;      // Default GroupItem of TSpTBXRadioGroupItem
  CM_SPPOPUPCLOSE = CM_BASE + 1111;   // Message sent to the PopupControl to update its state after the Popup is closed
  rvSpTBXDisplayMode = 'DisplayMode'; // Constant used to save the Toolbar DisplayMode with the Customizer. Do not localize!
  CPDefaultCols = 8;                  // ColorPalette constant
  CPDefaultRows = 5;                  // ColorPalette constant

type
  TSpTBXCustomItem = class;
  TSpTBXToolbar = class;
  TSpTBXStatusToolbar = class;
  TSpTBXPopupMenu = class;

  TSpTBXPaintStage = (
    pstPrePaint,               // Pre paint stage
    pstPostPaint               // Post paint stage
  );

  TSpTBXToolbarDisplayMode = (
    tbdmSelectiveCaption,      // The caption is displayed if the Item.DisplayMode = nbdmImageAndText
    tbdmImageOnly,             // Only the images are displayed
    tbdmImageAboveCaption,     // The images are displayed above the caption
    tbdmTextOnly               // Show the caption only
  );

  TSpTBXToolbarState = (
    tstResizing,               // The toolbar is being resized
    tstRightAligning,          // The toolbar items are being right aligned
    tstAnchoring               // The toolbar items are being anchored
  );

  TSpTBXToolbarStates = set of TSpTBXToolbarState;

  TSpBorderIcon = (
    briSystemMenu,             // SystemMenu item on the title bar
    briMinimize,               // Minimize item on the title bar
    briMaximize,               // Maximize item on the title bar
    briClose                   // Close item on the title bar
  );

  TSpBorderIcons = set of TSpBorderIcon;

  TTextWrapping = (
    twNone,                    // No wrapping
    twEndEllipsis,             // End ellipsis '...'
    twPathEllipsis,            // Path ellipsis '\..\..'
    twWrap                     // Word wrap
  );

  TSpTBXSearchItemViewerType = (
    sivtNormal,                // Normal search
    sivtInmediate,             // Search for the inmediate ItemViewer
    sivtInmediateSkipNonVisible// Search for the next inmediate ItemViewer, skipping non visible ones
  );

  TSpTBXGetImageIndexEvent = procedure(Sender: TObject;
    var AImageList: TCustomImageList; var AItemIndex: Integer) of object;

  TSpTBXDrawEvent = procedure(Sender: TObject; ACanvas: TCanvas;
    ARect: TRect; const PaintStage: TSpTBXPaintStage;
    var PaintDefault: Boolean) of object;

  TSpTBXDrawImageEvent = procedure(Sender: TObject; ACanvas: TCanvas;
    State: TSpTBXSkinStatesType; const PaintStage: TSpTBXPaintStage;
    var AImageList: TCustomImageList; var AImageIndex: Integer;
    var ARect: TRect; var PaintDefault: Boolean) of object;

  TSpTBXDrawItemEvent = procedure(Sender: TObject; ACanvas: TCanvas;
    ARect: TRect; ItemInfo: TSpTBXMenuItemInfo; const PaintStage: TSpTBXPaintStage;
    var PaintDefault: Boolean) of object;

  TSpTBXDrawPosEvent = procedure(Sender: TObject; ACanvas: TCanvas;
    X, Y: Integer; var PaintDefault: Boolean) of object;

  TSpTBXDrawTextEvent = procedure(Sender: TObject; ACanvas: TCanvas;
    ClientAreaRect: TRect; State: TSpTBXSkinStatesType; var ACaption: WideString;
    var CaptionRect: TRect; var CaptionFormat: Cardinal; IsTextRotated: Boolean;
    const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean) of object;

  TSpTBXDrawHintEvent = procedure(Sender: TObject; AHintBitmap: TBitmap;
    var AHint: WideString; var PaintDefault: Boolean) of object;

  TSpTBXItemNotificationEvent = procedure(Sender: TObject;
    Ancestor: TTBCustomItem; Relayed: Boolean; Action: TTBItemChangedAction;
    Index: Integer; Item: TTBCustomItem) of object;

  TSpTBXRadioGroupFillStringsEvent = procedure(Sender: TObject;
    Strings: TStringList) of object;

  TSpTBXPopupEvent = procedure(Sender: TObject; PopupView: TTBView) of object;

  { TSpTBXFontSettings }

  TSpTBXFontSize = 25..1000;

  TSpTBXFontSettings = class(TPersistent)
  private
    FColor: TColor;
    FName: TFontName;
    FSize: TSpTBXFontSize;
    FStyle: TFontStyles;
    FOnChange: TNotifyEvent;
    procedure SetColor(Value: TColor);
    procedure SetName(const Value: TFontName);
    procedure SetSize(Value: TSpTBXFontSize);
    procedure SetStyle(const Value: TFontStyles);
  protected
    procedure Modified;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create;
    procedure Apply(AFont: TFont);
    procedure Assign(Src: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clNone;
    property Name: TFontName read FName write SetName;   // default ''
    property Size: TSpTBXFontSize read FSize write SetSize default 100;  // Size Percent
    property Style: TFontStyles read FStyle write SetStyle default [];
  end;

  { TSpTBXCustomDragObject }

  TSpTBXCustomDragObject = class(TDragObjectEx)
  private
    FDragCursorAccept: TCursor;
    FDragCursorCancel: TCursor;
    FSourceControl: TControl;
    FSourceItem: TTBCustomItem;
  protected
    function GetDragCursor(Accepted: Boolean; X: Integer; Y: Integer): TCursor; override;
    procedure Finished(Target: TObject; X, Y: Integer; Accepted: Boolean); override;
  public
    constructor Create(ASourceControl: TControl; AItem: TTBCustomItem); virtual;
    property DragCursorAccept: TCursor read FDragCursorAccept write FDragCursorAccept;
    property DragCursorCancel: TCursor read FDragCursorCancel write FDragCursorCancel;
    property SouceItem: TTBCustomItem read FSourceItem;
    property SourceControl: TControl read FSourceControl;
  end;

  { TSpTBXItemDragObject }

  TSpTBXItemDragObject = class(TSpTBXCustomDragObject);

  { TSpTBXCustomItemActionLink }

  TSpTBXCustomItemActionLink = class(TTBCustomItemActionLink);

  { TSpTBXCustomControl }

  TSpTBXCustomControl = class(TCustomControl);

  { TSpTBXItem }

  TSpTBXCustomItem = class(TTBCustomItem)
  private
    FCaptionGlow: TSpGlowDirection;
    FCaptionGlowColor: TColor;
    FAlignment: TAlignment;
    FAnchored: Boolean;
    FControl: TControl;
    FCustomWidth: Integer;
    FCustomHeight: Integer;
    FFontSettings: TSpTBXFontSettings;
    FMargins: Integer;
    FMinHeight: Integer;
    FMinWidth: Integer;
    FStretch: Boolean;
    FToolbarStylePopup: Boolean;
    FToolBoxPopup: Boolean;
    FWrapping: TTextWrapping;
    FOnClosePopup: TNotifyEvent;
    FOnInitPopup: TSpTBXPopupEvent;
    FOnDrawCaption: TSpTBXDrawTextEvent;
    FOnDrawHint: TSpTBXDrawHintEvent;
    FOnDrawItem: TSpTBXDrawItemEvent;
    FOnDrawImage: TSpTBXDrawImageEvent;
    procedure FontSettingsChanged(Sender: TObject);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetAnchored(const Value: Boolean);
    procedure SetCaptionGlow(const Value: TSpGlowDirection);
    procedure SetCaptionGlowColor(const Value: TColor);
    procedure SetControl(const Value: TControl);
    procedure SetCustomWidth(Value: Integer);
    procedure SetCustomHeight(Value: Integer);
    procedure SetFontSettings(const Value: TSpTBXFontSettings);
    procedure SetMargins(Value: Integer);
    procedure SetMinHeight(const Value: Integer);
    procedure SetMinWidth(const Value: Integer);
    procedure SetStretch(const Value: Boolean);
    procedure SetToolBoxPopup(const Value: Boolean);
    procedure SetWrapping(const Value: TTextWrapping);
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function DialogChar(CharCode: Word): Boolean; virtual;
    procedure DoDrawAdjustFont(AFont: TFont; State: TSpTBXSkinStatesType); virtual;
    procedure DoDrawHint(AHintBitmap: TBitmap; var AHint: Widestring; var PaintDefault: Boolean); virtual;
    procedure DoDrawButton(ACanvas: TCanvas; ARect: TRect; ItemInfo: TSpTBXMenuItemInfo;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    procedure DoDrawCaption(ACanvas: TCanvas; ClientAreaRect: TRect; State: TSpTBXSkinStatesType;
      var ACaption: WideString; var CaptionRect: TRect; var CaptionFormat: Cardinal;
      IsTextRotated: Boolean; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    procedure DoDrawImage(ACanvas: TCanvas; State: TSpTBXSkinStatesType;
      const PaintStage: TSpTBXPaintStage; var AImageList: TCustomImageList;
      var AImageIndex: Integer; var ARect: TRect; var PaintDefault: Boolean); virtual;
    procedure DoPopupShowingChanged(APopupWindow: TTBPopupWindow; IsVisible: Boolean); virtual;
    function GetActionLinkClass: TTBCustomItemActionLinkClass; override;
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
    function GetPopupWindowClass: TTBPopupWindowClass; override;
    procedure ToggleControl; virtual;
    procedure UpdateProps; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property Anchored: Boolean read FAnchored write SetAnchored default False;
    property CaptionGlow: TSpGlowDirection read FCaptionGlow write SetCaptionGlow default gldNone;
    property CaptionGlowColor: TColor read FCaptionGlowColor write SetCaptionGlowColor default clYellow;
    property Control: TControl read FControl write SetControl;
    property CustomWidth: Integer read FCustomWidth write SetCustomWidth default -1;
    property CustomHeight: Integer read FCustomHeight write SetCustomHeight default -1;
    property FontSettings: TSpTBXFontSettings read FFontSettings write SetFontSettings;
    property Margins: Integer read FMargins write SetMargins default 0;
    property MinHeight: Integer read FMinHeight write SetMinHeight default 0;
    property MinWidth: Integer read FMinWidth write SetMinWidth default 0;
    property ToolbarStylePopup: Boolean read FToolbarStylePopup write FToolbarStylePopup default False;  // Used on submenus
    property ToolBoxPopup: Boolean read FToolBoxPopup write SetToolBoxPopup default False;  // Used on submenus
    property Stretch: Boolean read FStretch write SetStretch default True;  // Hidden, all items are stretched by default
    property OnClosePopup: TNotifyEvent read FOnClosePopup write FOnClosePopup;
    property OnInitPopup: TSpTBXPopupEvent read FOnInitPopup write FOnInitPopup;
    property OnDrawCaption: TSpTBXDrawTextEvent read FOnDrawCaption write FOnDrawCaption;
    property OnDrawHint: TSpTBXDrawHintEvent read FOnDrawHint write FOnDrawHint;
    property OnDrawImage: TSpTBXDrawImageEvent read FOnDrawImage write FOnDrawImage;
    property OnDrawItem: TSpTBXDrawItemEvent read FOnDrawItem write FOnDrawItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    function GetShortCutText: WideString; // Reintroduce to support unicode shortcut text
    procedure InitiateAction; override;
    procedure Invalidate;
  published
    property Caption;
    property Hint;
    property Wrapping: TTextWrapping read FWrapping write SetWrapping default twWrap;
  end;

  TSpTBXItemViewer = class(TTBItemViewer)
  private
    function GetItem: TSpTBXCustomItem;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure InternalCalcSize(const Canvas: TCanvas; CalcStretch: Boolean; var AWidth, AHeight: Integer);
  protected
    FAnchorSize: TPoint;
    FAnchorDelta: Integer;

    function IsOnToolBoxPopup: Boolean;

    // Custom Painting methods
    procedure DoDrawAdjustFont(AFont: TFont; State: TSpTBXSkinStatesType); virtual;
    procedure DoDrawButton(ACanvas: TCanvas; ARect: TRect; ItemInfo: TSpTBXMenuItemInfo;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    procedure DoDrawCaption(ACanvas: TCanvas; ClientAreaRect: TRect;
      State: TSpTBXSkinStatesType; var ACaption: WideString; var CaptionRect: TRect;
      var CaptionFormat: Cardinal; IsTextRotated: Boolean; const PaintStage: TSpTBXPaintStage;
      var PaintDefault: Boolean); virtual;
    procedure DoDrawImage(ACanvas: TCanvas; State: TSpTBXSkinStatesType;
      const PaintStage: TSpTBXPaintStage; var AImageList: TCustomImageList;
      var AImageIndex: Integer; var ARect: TRect; var PaintDefault: Boolean); virtual;
    procedure DoDrawHint(AHintBitmap: TBitmap; CursorPos: TPoint; var CursorRect: TRect; var AHint: Widestring; var PaintDefault: Boolean); virtual;

    // Painting methods
    function CaptionShown: Boolean; override;
    function GetImageShown: Boolean; virtual;
    function GetImageSize: TSize; virtual;
    function GetRightImageSize: TSize; virtual;
    function GetTextColor(State: TSpTBXSkinStatesType): TColor; virtual;
    procedure DrawItemImage(ACanvas: TCanvas; ARect: TRect; ItemInfo: TSpTBXMenuItemInfo; ImgIndex: Integer); virtual;
    procedure DrawItemRightImage(ACanvas: TCanvas; ARect: TRect; ItemInfo: TSpTBXMenuItemInfo); virtual;
    procedure CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer); override;
    procedure GetTextInfo(ACanvas: TCanvas; State: TSpTBXSkinStatesType; out TextInfo: TSpTBXTextInfo);
    procedure Paint(const Canvas: TCanvas; const ClientAreaRect: TRect; IsSelected, IsPushed, UseDisabledShadow: Boolean); override;

    // Hints
    procedure InternalMouseMove(Shift: TShiftState; X, Y: Integer); virtual;
  public
    function GetCaptionText: WideString; reintroduce; virtual; // Hides the inherited TB2K GetCaptionText function
    function GetHintText: Widestring; reintroduce; virtual; // Hides the inherited TB2K GetHintText function
    function IsToolbarStyle: Boolean; // Hides the inherited TB2K IsToolbarStyle function
    property Item: TSpTBXCustomItem read GetItem; // Hides the inherited TB2K Item property
  end;

  TSpTBXItem = class(TSpTBXCustomItem)
  published
    property Action;
    property AutoCheck;
    property Checked;
    property DisplayMode;
    property Enabled;
    property GroupIndex;
    property HelpContext;
    property ImageIndex;
    property Images;
    property InheritOptions;
    property MaskOptions;
    property Options;
    property RadioItem;
    property ShortCut;
    property Visible;
    // property OnDrawImage; use custom OnDrawImage
    property OnClick;
    property OnSelect;
    // TSpTBXCustomItem properties
    property Alignment;
    property Anchored;
    property CaptionGlow;
    property CaptionGlowColor;
    property Control;
    property CustomWidth;
    property CustomHeight;
    property FontSettings;
    property Margins;
    property MinHeight;
    property MinWidth;
    property OnDrawCaption;
    property OnDrawHint;
    property OnDrawImage;
    property OnDrawItem;
  end;

  { TSpTBXRootItem }

  TSpTBXRootItem = class(TTBRootItem)
  private
    FToolBoxPopup: Boolean;
    FOnInitPopup: TSpTBXPopupEvent;
    FOnClosePopup: TNotifyEvent;
    procedure SetToolBoxPopup(const Value: Boolean);
  protected
    procedure DoPopupShowingChanged(APopupWindow: TTBPopupWindow; IsVisible: Boolean); virtual;
    function GetPopupWindowClass: TTBPopupWindowClass; override;
    property ToolBoxPopup: Boolean read FToolBoxPopup write SetToolBoxPopup default False;
  public
    property OnInitPopup: TSpTBXPopupEvent read FOnInitPopup write FOnInitPopup;
    property OnClosePopup: TNotifyEvent read FOnClosePopup write FOnClosePopup;
  end;

  { TSpTBXSubmenuItem }

  TSpTBXSubmenuItem = class(TSpTBXItem)
  private
    FHideEmptyPopup: Boolean;
    function GetDropdownCombo: Boolean;
    procedure SetDropdownCombo(Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property DropdownCombo: Boolean read GetDropdownCombo write SetDropdownCombo default False;
    property HideEmptyPopup: Boolean read FHideEmptyPopup write FHideEmptyPopup default False;
    property LinkSubitems;
    property SubMenuImages;
    property ToolbarStylePopup;
    property ToolBoxPopup;
    property OnPopup;
    property OnClosePopup;
    property OnInitPopup;
  end;

  { TSpTBXColorItem }

  TSpTBXColorItem = class(TSpTBXCustomItem)
  private
    FColor: TColor;
    procedure SetColor(Value: TColor);
  protected
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Action;
    property AutoCheck;
    property Checked;
    property DisplayMode;
    property Enabled;
    property GroupIndex;
    property HelpContext;
    property InheritOptions;
    property MaskOptions;
    property Options;
    property ShortCut;
    property Visible;
    property OnClick;
    property OnSelect;
    // TSpTBXCustomItem properties
    property Alignment;
    property Anchored;
    property CaptionGlow;
    property CaptionGlowColor;
    property Control;
    property CustomWidth;
    property CustomHeight;
    property FontSettings;
    property Margins;
    property MinHeight;
    property MinWidth;
    property OnDrawCaption;
    property OnDrawHint;
    property OnDrawImage;
    property OnDrawItem;
    // TSpTBXColorItem properties
    property Color: TColor read FColor write SetColor default clWhite;
  end;

  TSpTBXColorItemViewer = class(TSpTBXItemViewer)
  protected
    procedure DoDrawImage(ACanvas: TCanvas; State: TSpTBXSkinStatesType;
      const PaintStage: TSpTBXPaintStage; var AImageList: TCustomImageList;
      var AImageIndex: Integer; var ARect: TRect; var PaintDefault: Boolean); override;
    function GetImageShown: Boolean; override;
    function GetImageSize: TSize; override;
  end;

  { TSpTBXLabelItem }

  TSpTBXCustomLabelItem = class(TSpTBXCustomItem)
  protected
    function DialogChar(CharCode: Word): Boolean; override;
    procedure DoDrawButton(ACanvas: TCanvas; ARect: TRect; ItemInfo: TSpTBXMenuItemInfo;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); override;
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
    procedure ToggleControl; override;
    procedure UpdateProps; override;
    property Alignment default taLeftJustify;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSpTBXLabelItemViewer = class(TSpTBXItemViewer)
  protected
    procedure CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer); override;
    function DoExecute: Boolean; override;
  end;

  TSpTBXLabelItem = class(TSpTBXCustomLabelItem)
  published
    property Enabled;
    property ImageIndex;
    property Images;
    property InheritOptions;
    property MaskOptions;
    property Options;
    property Visible;
    property OnClick;
    // TSpTBXCustomItem properties
    property Alignment;
    property Anchored;
    property CaptionGlow;
    property CaptionGlowColor;
    property Control;
    property CustomWidth;
    property CustomHeight;
    property FontSettings;
    property Margins;
    property MinHeight;
    property MinWidth;
    property OnDrawCaption;
    property OnDrawHint;
    property OnDrawImage;
    property OnDrawItem;
  end;

  { TSpTBXSeparatorItem }

  TSpTBXSeparatorItem = class(TTBSeparatorItem)
  protected
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
  end;

  TSpTBXSeparatorItemViewer = class(TTBSeparatorItemViewer)
  protected
    procedure CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer); override;
    function IsStatusBarSeparator: Boolean;
    procedure Paint(const Canvas: TCanvas; const ClientAreaRect: TRect; IsSelected, IsPushed, UseDisabledShadow: Boolean); override;
  end;

  { TSpTBXRightAlignSpacerItem }

  TSpTBXRightAlignSpacerItem = class(TSpTBXCustomLabelItem)
  published
    property ImageIndex;
    property Images;
    property MaskOptions;
    property Options;
    property OnClick;
    // TSpTBXCustomItem properties
    property Alignment;
    property CaptionGlow;
    property CaptionGlowColor;
    property CustomWidth;
    property CustomHeight;
    property FontSettings;
    property OnDrawCaption;
    property OnDrawHint;
    property OnDrawImage;
    property OnDrawItem;
  end;

  { TSpTBXRadioGroupItem }

  TSpTBXRadioGroupItem = class(TTBGroupItem)
  private
    FDefaultIndex: Integer;
    FLastClickedIndex: Integer;
    FOnClick: TNotifyEvent;
    FOnFillStrings: TSpTBXRadioGroupFillStringsEvent;
    FOnUpdate: TNotifyEvent;
  protected
    FStrings: TStringList;
    procedure Loaded; override;
    procedure ItemClickEvent(Sender: TObject); virtual;
    procedure DoClick(AItem: TSpTBXItem); virtual;
    procedure DoFillStrings; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Recreate;
    property DefaultIndex: Integer read FDefaultIndex write FDefaultIndex;
    property LastClickedIndex: Integer read FLastClickedIndex;
  published
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnFillStrings: TSpTBXRadioGroupFillStringsEvent read FOnFillStrings write FOnFillStrings;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  end;

  { TSpTBXSkinGroupItem }

  TSpTBXSkinGroupItem = class(TSpTBXRadioGroupItem)
  private
    FOnSkinChange: TNotifyEvent;
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  protected
    procedure DoClick(AItem: TSpTBXItem); override;
    procedure DoSkinChange; virtual;
    procedure DoFillStrings; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnSkinChange: TNotifyEvent read FOnSkinChange write FOnSkinChange;
  end;

  { TSpTBXSystemMenuItem }

  TSpTBXSystemMenuItem = class(TSpTBXCustomItem)
  private
    FMDISystemMenu: Boolean;
    FShowSize: Boolean;
    procedure CommandClick(Sender: TObject);
  protected
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
    function GetSystemMenuParentForm: TCustomForm;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  published
    property MDISystemMenu: Boolean read FMDISystemMenu write FMDISystemMenu default False;
    property ShowSize: Boolean read FShowSize write FShowSize default True;
  end;

  TSpTBXSystemMenuItemViewer = class(TSpTBXItemViewer)
  protected
    procedure CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer); override;
    procedure Paint(const Canvas: TCanvas; const ClientAreaRect: TRect; IsSelected, IsPushed, UseDisabledShadow: Boolean); override;
  end;

  { TSpTBXToolPalette }

  TSpTBXRowColCount = 1..100;

  TSpTBXTPGetCellHint = procedure(Sender: TObject; ACol, ARow: Integer; var AHint: WideString) of object;

  TSpTBXTPDrawCellImage = procedure(Sender: TObject; ACanvas: TCanvas;
    ARect: TRect; ACol, ARow: Integer; Selected, HotTrack, Enabled: Boolean) of object;

  TSpTBXTPCellClick = procedure(Sender: TObject; ACol, ARow: Integer; var Allow: Boolean) of object;

  TSpTBXCPGetColorInfo = procedure(Sender: TObject; ACol, ARow: Integer; var Color: TColor; var Name: WideString) of object;

  TSpTBXCustomToolPalette = class(TSpTBXCustomItem)
  private
    FCustomImages: Boolean;
    FColCount: TSpTBXRowColCount;
    FRowCount: TSpTBXRowColCount;
    FSelectedCell: TPoint;
    FOnChange: TNotifyEvent;
    FOnCellClick: TSpTBXTPCellClick;
    FOnDrawCellImage: TSpTBXTPDrawCellImage;
    FOnGetCellHint: TSpTBXTPGetCellHint;
    procedure SetSelectedCell(Value: TPoint);
  protected
    function DoCellClick(ACol, ARow: Integer): Boolean; virtual;
    procedure DoChange; virtual;
    procedure DoDrawCellImage(ACanvas: TCanvas; const ARect: TRect; ACol, ARow: Integer; ItemInfo: TSpTBXMenuItemInfo); virtual;
    procedure DoGetCellHint(ACol, ARow: Integer; var AHint: WideString); virtual;
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
    procedure HandleClickCell(ACol, ARow: Integer); virtual;
    procedure SetColCount(Value: TSpTBXRowColCount); virtual;
    procedure SetRowCount(Value: TSpTBXRowColCount); virtual;
    property CustomImages: Boolean read FCustomImages write FCustomImages;
    property ColCount: TSpTBXRowColCount read FColCount write SetColCount default 1;
    property RowCount: TSpTBXRowColCount read FRowCount write SetRowCount default 1;
    property SelectedCell: TPoint read FSelectedCell write SetSelectedCell;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnCellClick: TSpTBXTPCellClick read FOnCellClick write FOnCellClick;
    property OnDrawCellImage: TSpTBXTPDrawCellImage read FOnDrawCellImage write FOnDrawCellImage;
    property OnGetCellHint: TSpTBXTPGetCellHint read FOnGetCellHint write FOnGetCellHint;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSpTBXToolPalette = class(TSpTBXCustomToolPalette)
  public
    property SelectedCell;
  published
    property ColCount;
    property HelpContext;
    property Images;
    property Options;
    property RowCount;
    property Visible;
    property OnChange;
    property OnCellClick;
    property OnDrawCellImage;
    property OnGetCellHint;
  end;

  TSpTBXToolViewer = class(TSpTBXItemViewer)
  private
    FCellHeight: Integer;
    FCellWidth: Integer;
    FColCount: Integer;
    FRowCount: Integer;
    FHotCell: TPoint;
    function GetItem: TSpTBXCustomToolPalette;
  protected
    FIndent: Integer;
    FMouseIsDown: Boolean;
    procedure CalcCellSize(ACanvas: TCanvas; var AWidth, AHeight: Integer); virtual;
    procedure CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer); override;
    function GetImageIndex(Col, Row: Integer): Integer;
    function GetImageSize: TSize; override;
    function GetImageShown: Boolean; override;
    function GetCellAt(X, Y: Integer; out Col, Row: Integer): Boolean;
    function GetCellRect(ClientAreaRect: TRect; Col, Row: Integer): TRect; virtual;
    function GetCellHint(Col, Row: Integer): WideString;
    procedure DoDrawHint(AHintBitmap: TBitmap; CursorPos: TPoint; var CursorRect: TRect; var AHint: Widestring; var PaintDefault: Boolean); override;
    procedure DrawCellImage(ACanvas: TCanvas; const ARect: TRect; Col, Row: Integer; ItemInfo: TSpTBXMenuItemInfo); virtual;
    procedure Entering; override;
    procedure InvalidateCell(ACol, ARow: Integer);
    function  IsCellVisible(Cell: TPoint): Boolean; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Shift: TShiftState; X, Y: Integer;var MouseDownOnMenu: Boolean); override;
    procedure MouseMove(X, Y: Integer); override;
    procedure MouseUp(X, Y: Integer; MouseWasDownOnMenu: Boolean); override;
    procedure Paint(const Canvas: TCanvas; const ClientAreaRect: TRect; IsSelected, IsPushed, UseDisabledShadow: Boolean); override;
  public
    constructor Create(AView: TTBView; AItem: TTBCustomItem; AGroupLevel: Integer); override;
    property Item: TSpTBXCustomToolPalette read GetItem;  // Hides the inherited TB2K Item property
  end;

  { TSpTBXColorPalette }

  TSpTBXColorPalette = class(TSpTBXCustomToolPalette)
  private
    FColor: TColor;
    FCustomColors: Boolean;
    FOnGetColor: TSpTBXCPGetColorInfo;
    procedure SetColor(Value: TColor);
    procedure SetCustomColors(const Value: Boolean);
  protected
    procedure DoChange; override;
    procedure DoGetCellHint(ACol, ARow: Integer; var AHint: WideString); override;
    procedure DoDrawCellImage(ACanvas: TCanvas; const ARect: TRect; ACol, ARow: Integer; ItemInfo: TSpTBXMenuItemInfo); override;
    function GetCellColor(ACol, ARow: Integer): TColor;
    procedure GetCellInfo(ACol, ARow: Integer; out AColor: TColor; out AName: WideString);
    procedure SetColCount(Value: TSpTBXRowColCount); override;
    procedure SetRowCount(Value: TSpTBXRowColCount); override;
  public
    constructor Create(AOwner: TComponent); override;
    function FindCell(AColor: TColor): TPoint;
  published
    property CustomColors: Boolean read FCustomColors write SetCustomColors default False; // Must be published before ColCount
    property ColCount default CPDefaultCols;
    property Color: TColor read FColor write SetColor default clNone;
    property HelpContext;
    property InheritOptions;
    property MaskOptions;
    property Options default [tboShowHint];
    property RowCount default CPDefaultRows;
    property Visible;
    property OnChange;
    property OnCellClick;
    property OnGetCellHint;
    property OnGetColor: TSpTBXCPGetColorInfo read FOnGetColor write FOnGetColor;
  end;

  { TSpTBXItemCacheList }

  TSpTBXItemCache = class(TCollectionItem)
  private
    FDock: TTBDock;
    FName: TComponentName;
    FItem: TTBCustomItem;
    FWidth, FHeight: Integer;
    FParentWidth, FParentHeight: Integer;
    function GetName: TComponentName;
  public
    procedure Assign(Source: TPersistent); override;
    property Item: TTBCustomItem read FItem write FItem;
  published
    property Dock: TTBDock read FDock write FDock;
    property Name: TComponentName read GetName write FName;
    property Width: Integer read FWidth write FWidth default 0;
    property Height: Integer read FHeight write FHeight default 0;
    property ParentWidth: Integer read FParentWidth write FParentWidth default 0;
    property ParentHeight: Integer read FParentHeight write FParentHeight default 0;
  end;

  TSpTBXItemCacheCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TSpTBXItemCache;
    procedure SetItem(Index: Integer; const Value: TSpTBXItemCache);
  public
    function Add(AItem: TTBCustomItem): Integer; virtual;
    function IndexOf(AItem: TTBCustomItem): Integer;
    property Items[Index: Integer]: TSpTBXItemCache read GetItem write SetItem; default;
  end;

  { TSpTBXDock }

  TSpTBXDock = class(TTBDock)
  private
    FMoving: Boolean;
    FResizing: Boolean;
    FPrevWidth: Integer;
    FPrevHeight: Integer;
    FOnDrawBackground: TSpTBXDrawEvent;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  protected
    function CanResize(var NewWidth: Integer; var NewHeight: Integer): Boolean; override;
    procedure DrawBackground(DC: HDC; const DrawRect: TRect); override;
    procedure DoDrawBackground(ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    procedure Resize; override;
    function UsingBackground: Boolean; override;
    function UsingBitmap: Boolean;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property PrevWidth: Integer read FPrevWidth;
    property PrevHeight: Integer read FPrevHeight;
  published
    property Color default clNone;
    property OnCanResize;
    property OnDrawBackground: TSpTBXDrawEvent read FOnDrawBackground write FOnDrawBackground;
  end;

  TSpTBXDockClass = class of TSpTBXDock;

  { TSpTBXToolbar }

  TSpTBXToolbarView = class(TTBToolbarView)
  private
    FMaxSize: Integer;
    procedure SetMaxSize(const Value: Integer);
  protected
    FTallestItemSize: Integer;
    FUpdating: Integer;
    procedure DoUpdatePositions(var ASize: TPoint); override;
  public
    constructor Create(AOwner: TComponent; AParentView: TTBView;
      AParentItem: TTBCustomItem; AWindow: TWinControl;
      AIsToolbar, ACustomizing, AUsePriorityList: Boolean); override;
    procedure BeginUpdate; virtual; // Hides inherited BeginUpdate
    procedure EndUpdate; virtual; // Hides inherited EndUpdate
    function IsUpdating: Boolean;
    property MaxSize: Integer read FMaxSize write SetMaxSize;
  end;

  TSpTBXToolbar = class(TTBCustomToolbar)
  private
    FChevronVertical: Boolean;
    FCompoundToolbar: Boolean;
    FCustomizable: Boolean;
    FCustomizingCount: Integer;
    FItemMovingCount: Integer;
    FDisplayMode: TSpTBXToolbarDisplayMode;
    FLastDropMark: TRect;
    FLastSelectableWidth: Integer;
    FMenuBar: Boolean;
    FOnDrawBackground: TSpTBXDrawEvent;
    FOnItemNotification: TSpTBXItemNotificationEvent;
    procedure SetDisplayMode(const Value: TSpTBXToolbarDisplayMode);
    function GetMaxSize: Integer;
    procedure SetMaxSize(const Value: Integer);
    procedure SetMenuBar(const Value: Boolean);
    function CreateWrapper(Index: Integer; Ctl: TControl): TTBControlItem;
    function IsAnchoredControlItem(Item: TTBCustomItem): TTBControlItem;
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMMouseleave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
    procedure SetCustomizable(const Value: Boolean);
  protected
    FBeginDragIV: TTBItemViewer;
    FAnchoredControlItems: TSpTBXItemCacheCollection;
    FState: TSpTBXToolbarStates;
    FDefaultToolbarBorderSize: Integer;

    // Component
    procedure Resize; override;
    procedure AnchorItems(UpdateControlItems: Boolean = True); virtual;
    procedure RightAlignItems; virtual;

    // Painting
    procedure Paint; override;
    procedure DrawNCArea(const DrawToDC: Boolean; const ADC: HDC; const Clip: HRGN); override;
    function GetItemsTextColor(State: TSpTBXSkinStatesType): TColor; virtual;
    procedure InternalDrawBackground(ACanvas: TCanvas; ARect: TRect; PaintOnNCArea: Boolean; PaintBorders: Boolean = True); virtual;
    procedure DoDrawBackground(ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;

    // Get class
    function GetChevronItemClass: TTBChevronItemClass; override;
    function GetFloatingWindowParentClass: TTBFloatingWindowParentClass; override;
    function GetRightAlignMargin: Integer; virtual;
    function GetViewClass: TTBToolbarViewClass; override;

    // Hints
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    // Customizer
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    function CanDragCustomize(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragOver(Source: TObject; X: Integer; Y: Integer; State: TDragState; var Accept: Boolean); override;

    // Misc
    function CanItemClick(Item: TTBCustomItem; Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    procedure DoItemClick(Item: TTBCustomItem; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoItemNotification(Ancestor: TTBCustomItem; Relayed: Boolean; Action: TTBItemChangedAction; Index: Integer; Item: TTBCustomItem); virtual;

    property CompoundToolbar: Boolean read FCompoundToolbar write FCompoundToolbar;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X: Integer; Y: Integer); override;
    procedure ReadPositionData(const Data: TTBReadPositionData); override;
    procedure WritePositionData(const Data: TTBWritePositionData); override;
    procedure BeginUpdate; // Hides inherited BeginUpdate
    procedure EndUpdate; // Hides inherited EndUpdate
    function IsUpdating: Boolean;
    procedure BeginCustomize;
    procedure EndCustomize;
    procedure BeginItemMove;
    procedure EndItemMove;
    function GetFloatingBorderSize: TPoint; override;
    function IsCustomizing: Boolean;
    function IsItemMoving: Boolean;
    function IsVertical: Boolean;
    property DefaultToolbarBorderSize: Integer read FDefaultToolbarBorderSize;
    property MaxSize: Integer read GetMaxSize write SetMaxSize default -1;
  published
    property ActivateParent;
    property Align;
    property AutoResize;
    property BorderStyle;
    property ChevronHint;
    property ChevronMoveItems;
    property ChevronPriorityForNewItems;
    property CloseButton;
    property CloseButtonWhenDocked;
    property CurrentDock;
    property DefaultDock;
    property DockableTo;
    property DockMode;
    property DockPos;
    property DockRow;
    property DragHandleStyle;
    property FloatingMode;
    property Font;
    property FullSize;
    property HideWhenInactive;
    property Images;
    property Items;
    property LastDock;
    property LinkSubitems;
    property Options;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ProcessShortCuts;
    property Resizable;
    property ShowCaption;
    property ShowHint;
    property ShrinkMode;
    property SmoothDrag;
    property Stretch;
    property SystemFont;
    property TabOrder;
    property TabStop;
    property UpdateActions;
    property UseLastDock;
    property Visible;
    property Color default clNone;
    property OnClose;
    property OnCloseQuery;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMove;
    property OnRecreated;
    property OnRecreating;
    property OnDockChanged;
    property OnDockChanging;
    property OnDockChangingHidden;
    property OnResize;
    property OnShortCut;
    property OnVisibleChanged;
    property Caption;
    property Hint;
    property ChevronVertical: Boolean read FChevronVertical write FChevronVertical default True;
    property Customizable: Boolean read FCustomizable write SetCustomizable default True;
    property DisplayMode: TSpTBXToolbarDisplayMode read FDisplayMode write SetDisplayMode default tbdmSelectiveCaption;
    property MenuBar: Boolean read FMenuBar write SetMenuBar default False; // Hides the inherited MenuBar
    property OnDrawBackground: TSpTBXDrawEvent read FOnDrawBackground write FOnDrawBackground;
    property OnItemNotification: TSpTBXItemNotificationEvent read FOnItemNotification write FOnItemNotification;
  end;

  TSpTBXToolbarClass = class of TSpTBXToolbar;

  { TSpTBXToolWindow }

  TSpTBXCustomToolWindow = class(TTBCustomDockableWindow)
  private
    FMinClientWidth, FMinClientHeight, FMaxClientWidth, FMaxClientHeight: Integer;
    FOnDrawBackground: TSpTBXDrawEvent;
    function GetClientAreaWidth: Integer;
    procedure SetClientAreaWidth(Value: Integer);
    function GetClientAreaHeight: Integer;
    procedure SetClientAreaHeight(Value: Integer);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
  protected
    FBarSize: TSize;
    FDefaultToolbarBorderSize: Integer;
    function GetFloatingWindowParentClass: TTBFloatingWindowParentClass; override;

    // Sizing
    function CalcSize(ADock: TTBDock): TPoint; virtual;
    function DoArrange(CanMoveControls: Boolean; PreviousDockType: TTBDockType; NewFloating: Boolean; NewDock: TTBDock): TPoint; override;
    procedure GetBaseSize(var ASize: TPoint); override;
    procedure GetMinMaxSize(var AMinClientWidth, AMinClientHeight, AMaxClientWidth, AMaxClientHeight: Integer); override;
    procedure SetClientAreaSize(AWidth, AHeight: Integer); virtual;
    procedure SizeChanging(const AWidth, AHeight: Integer); override;

    // Painting
    procedure Paint; override;
    procedure DrawNCArea(const DrawToDC: Boolean; const ADC: HDC; const Clip: HRGN); override;
    procedure InternalDrawBackground(ACanvas: TCanvas; ARect: TRect; PaintOnNCArea: Boolean; PaintBorders: Boolean = True); virtual;
    procedure DoDrawBackground(ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;

    property ClientAreaHeight: Integer read GetClientAreaHeight write SetClientAreaHeight;
    property ClientAreaWidth: Integer read GetClientAreaWidth write SetClientAreaWidth;
    property MaxClientHeight: Integer read FMaxClientHeight write FMaxClientHeight default 0;
    property MaxClientWidth: Integer read FMaxClientWidth write FMaxClientWidth default 0;
    property MinClientHeight: Integer read FMinClientHeight write FMinClientHeight default 32;
    property MinClientWidth: Integer read FMinClientWidth write FMinClientWidth default 32;
    property OnDrawBackground: TSpTBXDrawEvent read FOnDrawBackground write FOnDrawBackground;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetFloatingBorderSize: TPoint; override;
    procedure InvalidateBackground(InvalidateChildren: Boolean = True); virtual;
    function IsVertical: Boolean;
    procedure ReadPositionData(const Data: TTBReadPositionData); override;
    procedure WritePositionData(const Data: TTBWritePositionData); override;
    property DefaultToolbarBorderSize: Integer read FDefaultToolbarBorderSize;
  published
    property Caption;
    property Color default clNone;
  end;

  TSpTBXToolWindow = class(TSpTBXCustomToolWindow)
  published
    property ActivateParent;
    property Align;
    property Anchors;
    property BorderStyle;
    property Caption;
    property Color;
    property CloseButton;
    property CloseButtonWhenDocked;
    property CurrentDock;
    property DefaultDock;
    property DockableTo;
    property DockMode;
    property DockPos;
    property DockRow;
    property DragHandleStyle;
    property FloatingMode;
    property Font;
    property FullSize;
    property HideWhenInactive;
    property LastDock;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Resizable;
    property ShowCaption;
    property ShowHint;
    property Stretch;
    property SmoothDrag;
    property TabOrder;
    property UseLastDock;
    property Visible;
    property OnClose;
    property OnCloseQuery;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDockChanged;
    property OnDockChanging;
    property OnDockChangingHidden;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMove;
    property OnRecreated;
    property OnRecreating;
    property OnResize;
    property OnVisibleChanged;
    // TSpTBXCustomToolWindow properties
    property ClientAreaHeight;
    property ClientAreaWidth;
    property MaxClientHeight;
    property MaxClientWidth;
    property MinClientHeight;
    property MinClientWidth;
    property OnDrawBackground;
  end;

  { TSpTBXFloatingWindowParent }

  TSpTBXFloatingWindowParent = class(TTBFloatingWindowParent)
  private
    FCloseButtonHover: Boolean;
    FCloseOnAltF4: Boolean;
    procedure UpdateDwmNCSize;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMActivateApp(var Message: TWMActivateApp); message WM_ACTIVATEAPP;
    procedure WMClose(var Message: TWMClose); message WM_CLOSE;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMNCMouseLeave(var Message: TMessage); message WM_NCMOUSELEAVE;
    procedure WMNCMouseMove(var Message: TWMNCMouseMove); message WM_NCMOUSEMOVE;
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  protected
    procedure CancelNCHover;
    procedure CreateWnd; override;
    procedure DrawNCArea(const DrawToDC: Boolean; const ADC: HDC; const Clip: HRGN; RedrawWhat: TTBToolWindowNCRedrawWhat); override;
    procedure RedrawCloseButton;
    procedure VisibleChanging; override;
    property CloseButtonHover: Boolean read FCloseButtonHover;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    destructor Destroy; override;
    property CloseOnAltF4: Boolean read FCloseOnAltF4 write FCloseOnAltF4;
  end;

  { TSpTBXPopupWindow }

  TSpTBXPopupWindow = class(TTBPopupWindow)
  private
    FPaintingClientArea: Boolean;
    FMaximumImageSize: TSize;
    function CanDrawGutter: Boolean;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure CMShowingchanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMPrint(var Message: TMessage); message WM_PRINT;
  protected
    function GetViewClass: TTBViewClass; override;
    procedure DoPopupShowingChanged(IsVisible: Boolean); virtual;
    procedure PaintBackground(ACanvas: TCanvas; ARect: TRect); virtual;
  public
    constructor CreatePopupWindow(AOwner: TComponent; const AParentView: TTBView;
      const AItem: TTBCustomItem; const ACustomizing: Boolean); override;
    destructor Destroy; override;
    property MaximumImageSize: TSize read FMaximumImageSize;
  end;

  TSpTBXPopupWindowView = class(TTBPopupView)
  protected
    procedure AutoSize(AWidth, AHeight: Integer); override;
  public
    procedure SetIsToolbar(const Value: Boolean);
  published
    property IsToolbar;
  end;

  { TSpTBXChevronItem }

  TSpTBXChevronItem = class(TTBChevronItem)
  protected
    function GetPopupWindowClass: TTBPopupWindowClass; override;
  public
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
  end;

  TSpTBXChevronItemViewer = class(TTBItemViewer)
  protected
    procedure Paint(const Canvas: TCanvas; const ClientAreaRect: TRect;
      IsSelected, IsPushed, UseDisabledShadow: Boolean); override;
  public
    function GetTextColor(State: TSpTBXSkinStatesType): TColor; virtual;
  end;

  TSpTBXChevronPopupWindow = class(TSpTBXPopupWindow)
  private
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
  end;

  { TSpTBXPopupMenu }

  ISpTBXPopupMenu = interface
    ['{C576A225-6E42-49F6-96E5-712510C5D85C}']
    function InternalPopup(X, Y: Integer; ForceFocus: Boolean; PopupControl: TControl = nil): Boolean;
  end;

  TSpTBXPopupMenu = class(TTBPopupMenu, ISpTBXPopupMenu)
  private
    FClickedItem: TTBCustomItem;
    FReturnClickedItemOnly: Boolean;
    FToolBoxPopup: Boolean;
    function GetOnInitPopup: TSpTBXPopupEvent;
    procedure SetOnInitPopup(const Value: TSpTBXPopupEvent);
    function GetOnClosePopup: TNotifyEvent;
    procedure SetOnClosePopup(const Value: TNotifyEvent);
  protected
    function InternalPopup(X, Y: Integer; ForceFocus: Boolean; PopupControl: TControl = nil): Boolean; virtual;
    function GetRootItemClass: TTBRootItemClass; override;
  public
    procedure Popup(X: Integer; Y: Integer); override;
    function PopupEx(X, Y: Integer; PopupControl: TControl = nil; ReturnClickedItemOnly: Boolean = False): TTBCustomItem; virtual;
  published
    property ToolBoxPopup: Boolean read FToolBoxPopup write FToolBoxPopup default False;
    property OnInitPopup: TSpTBXPopupEvent read GetOnInitPopup write SetOnInitPopup;
    property OnClosePopup: TNotifyEvent read GetOnClosePopup write SetOnClosePopup;
  end;

  { TSpTBXCompoundItemsControl }

  TSpTBXCompoundItemsControl = class(TSpTBXCustomControl, ITBItems)
  private
    procedure DockRequestDock(Sender: TObject; Bar: TTBCustomDockableWindow; var Accept: Boolean);
    function GetRootItems: TTBRootItem;
    function GetView: TSpTBXToolbarView;
    function GetImages: TCustomImageList;
    procedure SetImages(const Value: TCustomImageList);
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  protected
    FDock: TSpTBXDock;
    FToolbar: TSpTBXToolbar;
    procedure CreateParams(var Params: TCreateParams); override;
    function GetDockClass: TSpTBXDockClass; virtual;
    function GetToolbarClass: TSpTBXToolbarClass; virtual;
    function GetItems: TTBCustomItem; virtual;  // For ITBItems interface
    procedure Loaded; override;
    procedure SetName(const Value: TComponentName); override;
    property Images: TCustomImageList read GetImages write SetImages;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;  // For ITBItems interface
    procedure InvalidateBackground(InvalidateChildren: Boolean = True); virtual;
    property View: TSpTBXToolbarView read GetView;
  published
    property Items: TTBRootItem read GetRootItems;
  end;

  { TSpTBXCompoundBar }

  TSpTBXCompoundBar = class(TSpTBXCompoundItemsControl)
  private
    FOnDrawDockBackground: TSpTBXDrawEvent;
    procedure DrawDockBackground(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
    procedure DrawToolbarBackground(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
    procedure DockResize(Sender: TObject);
  protected
    procedure DoDrawDockBackground(ACanvas: TCanvas; ARect: TRect;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    property OnDrawDockBackground: TSpTBXDrawEvent read FOnDrawDockBackground write FOnDrawDockBackground;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TSpTBXButtonOptions }

  TSpTBXButtonOptions = class(TPersistent)
  private
    FEditableItems: TTBGroupItem;
    FCloseButton: TSpTBXItem;
    FMinimizeButton: TSpTBXItem;
    FMaximizeButton: TSpTBXItem;
    FRightAlignSpacer: TSpTBXRightAlignSpacerItem;
    FCaptionImageIndex: Integer;
    FCloseImageIndex: Integer;
    FMinimizeImageIndex: Integer;
    FMaximizeImageIndex: Integer;
    FRestoreImageIndex: Integer;
    FCaptionLabel: WideString;
    FCaption: Boolean;
    FClose: Boolean;
    FMinimize: Boolean;
    FMaximize: Boolean;
    FButtonBorders: Boolean;
    FTitleBarMaxSize: Integer;
    procedure SetCaptionImageIndex(Value: Integer);
    procedure SetCloseImageIndex(Value: Integer);
    procedure SetCaptionLabel(const Value: WideString);
    procedure SetMaximizeImageIndex(Value: Integer);
    procedure SetRestoreImageIndex(Value: Integer);
    procedure SetMinimizeImageIndex(Value: Integer);
    procedure SetCaption(const Value: Boolean);
    procedure SetClose(const Value: Boolean);
    procedure SetMaximize(const Value: Boolean);
    procedure SetMinimize(const Value: Boolean);
    procedure SetTitleBarMaxSize(const Value: Integer);
  protected
    FParentControl: TWinControl;
    FToolbar: TSpTBXToolbar;
    procedure ButtonsDrawImage(Sender: TObject; ACanvas: TCanvas;
      State: TSpTBXSkinStatesType; const PaintStage: TSpTBXPaintStage;
      var AImageList: TCustomImageList; var AImageIndex: Integer; var ARect:
      TRect; var PaintDefault: Boolean); virtual;
    procedure ButtonsDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
      ItemInfo: TSpTBXMenuItemInfo; const PaintStage: TSpTBXPaintStage;
      var PaintDefault: Boolean); virtual;
    procedure ButtonsClick(Sender: TObject); virtual; abstract;
    procedure CreateButtons; virtual;
    procedure UpdateButtonsVisibility; virtual;
    procedure SetupButton(B: TSpTBXCustomItem); virtual;
    function Restoring(B: TSpTBXCustomItem): Boolean; virtual; abstract;
  public
    constructor Create(AParent: TWinControl); virtual;
    procedure SetupButtonIcon(B: TSpTBXCustomItem); virtual;
    procedure MoveItemToTheLeft(B: TTBCustomItem);
    property EditableItems: TTBGroupItem read FEditableItems;
    property RightAlignSpacer: TSpTBXRightAlignSpacerItem read FRightAlignSpacer;
    property MinimizeButton: TSpTBXItem read FMinimizeButton;
    property MaximizeButton: TSpTBXItem read FMaximizeButton;
    property CloseButton: TSpTBXItem read FCloseButton;
    property CaptionLabel: WideString read FCaptionLabel write SetCaptionLabel;
  published
    property ButtonBorders: Boolean read FButtonBorders write FButtonBorders default True;
    property Caption: Boolean read FCaption write SetCaption default True;
    property Close: Boolean read FClose write SetClose default True;
    property Minimize: Boolean read FMinimize write SetMinimize default True;
    property Maximize: Boolean read FMaximize write SetMaximize default True;
    property CaptionImageIndex: Integer read FCaptionImageIndex write SetCaptionImageIndex default -1;
    property CloseImageIndex: Integer read FCloseImageIndex write SetCloseImageIndex default -1;
    property MinimizeImageIndex: Integer read FMinimizeImageIndex write SetMinimizeImageIndex default -1;
    property MaximizeImageIndex: Integer read FMaximizeImageIndex write SetMaximizeImageIndex default -1;
    property RestoreImageIndex: Integer read FRestoreImageIndex write SetRestoreImageIndex default -1;
    property TitleBarMaxSize: Integer read FTitleBarMaxSize write SetTitleBarMaxSize default 21;
  end;

  { TSpTBXStatusBar }

  TSpTBXStatusToolbar = class(TSpTBXToolbar)
  private
    FSizeGrip: Boolean;
    procedure SetSizeGrip(const Value: Boolean);
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
  protected
    FParentForm: TCustomForm;
    procedure DoItemNotification(Ancestor: TTBCustomItem; Relayed: Boolean; Action: TTBItemChangedAction; Index: Integer; Item: TTBCustomItem); override;
    function GetItemsTextColor(State: TSpTBXSkinStatesType): TColor; override;
    function GetRightAlignMargin: Integer; override;
    function GetParentFormWindowState: TWindowState;
    function IsPointInGrip(P: TPoint): Boolean;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetGripRect: TRect;
    function NeedsSeparatorRepaint: Boolean;
  published
    property SizeGrip: Boolean read FSizeGrip write SetSizeGrip default True;
  end;

  TSpTBXCustomStatusBar = class(TSpTBXCompoundBar)
  private
    function GetSizeGrip: Boolean;
    procedure SetSizeGrip(const Value: Boolean);
    function GetStatusToolbar: TSpTBXStatusToolbar;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    FPrevState: TWindowState;
    function CanResize(var NewWidth: Integer; var NewHeight: Integer): Boolean; override;
    procedure DoDrawDockBackground(ACanvas: TCanvas; ARect: TRect;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); override;
    procedure DrawSeparators(ACanvas: TCanvas; ARect: TRect); virtual;
    function GetToolbarClass: TSpTBXToolbarClass; override;
    property Align default alBottom;
    property SizeGrip: Boolean read GetSizeGrip write SetSizeGrip default True;
  public
    constructor Create(AOwner: TComponent); override;
    property Toolbar: TSpTBXStatusToolbar read GetStatusToolbar;
  end;

  TSpTBXStatusBar = class(TSpTBXCustomStatusBar)
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnCanResize;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    // TSpTBXCustomStatusBar properties
    property Images;
    property SizeGrip;
    property OnDrawDockBackground;
  end;

  { TSpTBXTitleBar }

  TSpTBXCustomTitleBar = class;

  TSpTBXTitleToolbar = class(TSpTBXToolbar)
  private
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
  protected
    function GetItemsTextColor(State: TSpTBXSkinStatesType): TColor; override;
    function GetTitleBar: TSpTBXCustomTitleBar;
    function GetRightAlignMargin: Integer; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
  end;

  TSpTBXTitleBarButtonOptions = class(TSpTBXButtonOptions)
  private
    FSystemMenu: Boolean;
    FSystemButton: TSpTBXSystemMenuItem;
    procedure SetSystemMenu(const Value: Boolean);
  protected
    FTitleBar: TSpTBXCustomTitleBar;
    procedure ButtonsDrawItem(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; ItemInfo: TSpTBXMenuItemInfo; const PaintStage: TSpTBXPaintStage;
      var PaintDefault: Boolean); override;
    procedure ButtonsClick(Sender: TObject); override;
    procedure CreateButtons; override;
    function Restoring(B: TSpTBXCustomItem): Boolean; override;
  public
    constructor Create(AParent: TWinControl); override;
    property SystemButton: TSpTBXSystemMenuItem read FSystemButton;
  published
    property SystemMenu: Boolean read FSystemMenu write SetSystemMenu default True;
  end;

  TSpTBXCustomTitleBar = class(TSpTBXCompoundBar)
  private
    FActive: Boolean;
    FFixedSize: Boolean;
    FFullScreenMaximize: Boolean;
    FMouseActive: Boolean;
    FOptions: TSpTBXTitleBarButtonOptions;
    FOldAppWndProc: Pointer;
    FNewAppWndProc: Pointer;
    FRegion: HRGN;
    FUpdateRegionCalled: Boolean;
    FOnDrawBackground: TSpTBXDrawEvent;
    FOldParentFormWndProc: TWndMethod;
    procedure AppWndProc(var Msg: TMessage);
    procedure NewParentFormWndProc(var Message: TMessage);
    procedure UpdateRegion;
    procedure SetActive(const Value: Boolean);
    procedure SetMouseActive(const Value: Boolean);
    procedure SetFullScreenMaximize(const Value: Boolean);
    function GetSystemMenuPopup: TSpTBXPopupEvent;
    procedure SetSystemMenuPopup(const Value: TSpTBXPopupEvent);
    function GetWindowState: TWindowState;
    procedure SetWindowState(const Value: TWindowState);
    {$IF CompilerVersion >= 23} //for Delphi XE2 and up
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    {$IFEND}
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
  protected
    FParentForm: TCustomForm;
    // Component
    procedure Loaded; override;
    function GetFloatingBorderSize: TPoint;
    function GetItems: TTBCustomItem; override;  // For ITBItems interface
    function GetToolbarClass: TSpTBXToolbarClass; override;
    procedure ChangeTitleBarState(Activate: Boolean);
    procedure UpdateSkinMetrics;

    // Painting
    procedure DoDrawDockBackground(ACanvas: TCanvas; ARect: TRect;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); override;
    procedure DoDrawBackground(ACanvas: TCanvas; ARect: TRect;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;

    // Sizing
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure GetSizeCursor(MousePos: TPoint; var SizeCursor, SizeCode: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;

    property Active: Boolean read FActive write SetActive default True;
    property Align default alClient;
    property FixedSize: Boolean read FFixedSize write FFixedSize default False;
    property FullScreenMaximize: Boolean read FFullScreenMaximize write SetFullScreenMaximize default False;
    property Options: TSpTBXTitleBarButtonOptions read FOptions write FOptions;
    property WindowState: TWindowState read GetWindowState write SetWindowState;
    property OnDrawBackground: TSpTBXDrawEvent read FOnDrawBackground write FOnDrawBackground;
    property OnSystemMenuPopup: TSpTBXPopupEvent read GetSystemMenuPopup write SetSystemMenuPopup;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetClientAreaRect: TRect;
    function IsActive: Boolean;
    property MouseActive: Boolean read FMouseActive write SetMouseActive default True;
    property Toolbar: TSpTBXToolbar read FToolbar;
  published
    property Caption;
  end;

  TSpTBXTitleBar = class(TSpTBXCustomTitleBar)
  published
    property Align;
    property Anchors;
    property Color;
    property BiDiMode;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnCanResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
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
    // TSpTBXCustomTitleBar properties
    property Active;
    property FixedSize;
    property FullScreenMaximize;
    property Images;
    property Options;
    property OnDrawBackground;
    property OnDrawDockBackground;
    property OnSystemMenuPopup;
  end;

  { TBitmapHint }

  TBitmapHint = class(THintWindow)
  private
    FHintBitmap: TBitmap;
    FActivating: Boolean;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    procedure Paint; override;
  public
    property Activating: Boolean read FActivating;
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
    procedure ActivateHintData(Rect: TRect; const AHint: string; AData: Pointer); override;
  end;

{ Item helpers }
procedure SpFillItemInfo(ACanvas: TCanvas; IV: TTBItemViewer; out ItemInfo: TSpTBXMenuItemInfo);
function SpGetBoundsRect(IV: TTBItemViewer; Root: TTBRootItem): TRect;
procedure SpGetAllItems(AParentItem: TTBCustomItem; ItemsList: TStringList; ClearFirst: Boolean = True);
function SpGetMenuMaximumImageSize(View: TTBView): TSize;
function SpGetItemViewerFromPoint(Root: TTBRootItem; View: TTBView; P: TPoint; ProcessGroupItems: Boolean = True): TTBItemViewer;
function SpGetNextItemSameEdge(View: TTBView; IV: TTBItemViewer; GoForward: Boolean; SearchType: TSpTBXSearchItemViewerType): TTBItemViewer;
function SpGetFirstRightAlignSpacer(View: TTBView): TSpTBXItemViewer;
function SpGetRightAlignedItems(View: TTBView; RightAlignedList: TList; IsRotated: Boolean; out VisibleTotalWidth, RightAlignedTotalWidth: Integer): TSpTBXItemViewer;
procedure SpInvalidateItem(View: TTBView; Item: TTBCustomItem);
function SpFindItemViewer(View: TTBView; Item: TTBCustomItem): TTBItemViewer;
function SpFindControlItem(Item: TTBCustomItem; Ctl: TControl; Recurse: Boolean = True): TTBControlItem;
procedure SpGetDropPosItemViewer(Root: TTBRootItem; View: TTBView; P: TPoint; out DestIV: TTBItemViewer; out DestItemPos: Integer; out DropMark: TRect); overload;
procedure SpGetDropPosItemViewer(Root: TTBRootItem; View: TTBView; P: TPoint; SourceItemPos: Integer; out DestIV: TTBItemViewer; out DestItemPos: Integer); overload;
function SpGetDragHandleSize(Toolbar: TTBCustomDockableWindow): Integer;
function SpIsVerticalToolbar(Toolbar: TTBCustomDockableWindow): Boolean;
function SpIsDockUsingBitmap(Dock: TTBDock): Boolean;

{ Painting helpers }
procedure SpDrawXPToolbarButton(ACanvas: TCanvas; ARect: TRect; State: TSpTBXSkinStatesType; ComboPart: TSpTBXComboPart = cpNone);
procedure SpDrawXPMenuItem(ACanvas: TCanvas; ARect: TRect; ItemInfo: TSpTBXMenuItemInfo);
procedure SpDrawXPMenuSeparator(ACanvas: TCanvas; ARect: TRect; MenuItemStyle, Vertical: Boolean);
procedure SpDrawXPMenuItemImage(ACanvas: TCanvas; ARect: TRect; const ItemInfo: TSpTBXMenuItemInfo; ImageList: TCustomImageList; ImageIndex: Integer);
procedure SpDrawXPMenuGutter(ACanvas: TCanvas; ARect: TRect);
procedure SpDrawXPMenuPopupWindow(ACanvas: TCanvas; ARect, OpenIVRect: TRect; DrawGutter: Boolean; ImageSize: Integer);
procedure SpDrawXPStatusBar(ACanvas: TCanvas; ARect, AGripRect: TRect);
procedure SpDrawXPTitleBar(ACanvas: TCanvas; ARect: TRect; IsActive: Boolean; DrawBorders: Boolean = True);
procedure SpDrawXPTitleBarBody(ACanvas: TCanvas; ARect: TRect; IsActive: Boolean; BorderSize: TPoint; DrawBody: Boolean = True);
procedure SpDrawXPDock(ACanvas: TCanvas; ARect: TRect; Vertical: Boolean = False);
procedure SpDrawXPToolbar(ACanvas: TCanvas; ARect: TRect; Docked, Floating, Vertical, PaintSkinBackground, PaintBorders: Boolean; SkinComponent: TSpTBXSkinComponentsType = skncToolbar); overload;
procedure SpDrawXPToolbar(W: TTBCustomDockableWindow; ACanvas: TCanvas; ARect: TRect; PaintOnNCArea: Boolean; PaintBorders: Boolean = True; SkinComponent: TSpTBXSkinComponentsType = skncToolbar); overload;
procedure SpDrawXPToolbarGrip(W: TTBCustomDockableWindow; ACanvas: TCanvas; ARect: TRect);
procedure SpDrawXPTooltipBackground(ACanvas: TCanvas; ARect: TRect);

{ Menu helpers }
function SpCalcPopupPosition(const X, Y, Width, Height: Integer; PopupControl: TControl = nil; IsVertical: Boolean = False): TPoint;
function SpHMenuGetCaption(Menu: HMenu; Index: Integer): WideString;
function SpHMenuToTBMenuItem(Menu: HMenu; ParentItem: TTBCustomItem): Boolean;
function SpShowSystemPopupMenu(ParentForm: TCustomForm; ScreenPos: TPoint; DoDefault: Boolean = True): Integer;
function SpFillSystemSpTBXPopup(ParentForm: TCustomForm; ParentItem: TTBCustomItem; ShowSize, ShowMinimize, ShowMaximize, ShowClose: Boolean; ClickEvent: TNotifyEvent = nil): Boolean;
function SpShowSystemSpTBXPopupMenu(ParentForm: TCustomForm; ScreenPos: TPoint; ShowSize, ShowMinimize, ShowMaximize, ShowClose: Boolean; PopupEvent: TSpTBXPopupEvent; DoDefault: Boolean = True): Integer;

{ Misc helpers }
procedure SpActivateDwmNC(WinControl: TWinControl; Activate: Boolean);
function SpIsDwmCompositionEnabled: Boolean;
function SpCanFocus(WinControl: TWinControl): Boolean;
function SpIsFocused(WinControl: TWinControl; out FocusedChild: TWinControl): Boolean;
function SpFocusFirstChild(WinControl: TWinControl): TWinControl;
function SpFindControl(Parent: TWinControl; Child: TControl): Integer;
function SpFindParent(Control: TControl; ParentClass: TClass): TWinControl;
function SpHasBorders(WinControl: TWinControl): Boolean;
function SpGetFormWindowState(F: TCustomForm; out RestoreBoundsRect: TRect): TWindowState;
procedure SpSetFormWindowState(F: TCustomForm; WindowState: TWindowState; RestoreBoundsRect: TRect);
function SpGetTaskBar(out State, Edge: Cardinal; out Bounds: TRect): Boolean;
procedure SpRecalcNCArea(WinControl: TWinControl);

{ Customizer helpers }
procedure SpCustomizeAllToolbars(AParentComponent: TComponent; Reset: Boolean);
procedure SpBeginUpdateAllToolbars(AParentComponent: TComponent);
procedure SpEndUpdateAllToolbars(AParentComponent: TComponent);

var
  SmCaptionFont: TFont;
  SpStockHintBitmap: TBitmap;
  MDIButtonsImgList: TImageList = nil;
  SpTBXHintWindowClass: THintWindowClass = TBitmapHint;

const
  CDefaultToolbarBorderSize = 2;       // Default size of Floating Toolbar borders
  crSpTBXNewHandPoint = 100;          // Cursor ID to replace crHandPoint for IDC_HAND
  crSpTBXCustomization = 101;         // Cursor ID used for item D&D customization accepted
  crSpTBXCustomizationCancel = 102;   // Cursor ID used for item D&D customization cancelled

implementation

{$R SpTBXGlyphs.res}

uses
  Themes, UxTheme,
  TypInfo, Types,
  {$IF CompilerVersion >= 25} // for Delphi XE4 and up
  System.UITypes,
  {$IFEND}
  ComCtrls, CommCtrl, ShellApi, DwmApi,
  TB2Anim, TB2Common;

const
  ROP_DSPDxax = $00E20746;
  // Constants for TSpTBXToolWindow registry values. Do not localize!
  rvClientWidth = 'ClientWidth';
  rvClientHeight = 'ClientHeight';
  // ColorPalette constants:
  CPDefaultColors: array[0..CPDefaultCols * CPDefaultRows - 1] of TIdentMapEntry = (
    (Value: $000000; Name: 'Black'),
    (Value: $003399; Name: 'Brown'),
    (Value: $003333; Name: 'Olive Green'),
    (Value: $003300; Name: 'Dark Green'),
    (Value: $663300; Name: 'Dark Teal'),
    (Value: $800000; Name: 'Dark blue'),
    (Value: $993333; Name: 'Indigo'),
    (Value: $333333; Name: 'Gray-80%'),

    (Value: $000080; Name: 'Dark Red'),
    (Value: $0066FF; Name: 'Orange'),
    (Value: $008080; Name: 'Dark Yellow'),
    (Value: $008000; Name: 'Green'),
    (Value: $808000; Name: 'Teal'),
    (Value: $FF0000; Name: 'Blue'),
    (Value: $996666; Name: 'Blue-Gray'),
    (Value: $808080; Name: 'Gray-50%'),

    (Value: $0000FF; Name: 'Red'),
    (Value: $0099FF; Name: 'Light Orange'),
    (Value: $00CC99; Name: 'Lime'),
    (Value: $669933; Name: 'Sea Green'),
    (Value: $CCCC33; Name: 'Aqua'),
    (Value: $FF6633; Name: 'Light Blue'),
    (Value: $800080; Name: 'Violet'),
    (Value: $969696; Name: 'Gray-40%'),

    (Value: $FF00FF; Name: 'Pink'),
    (Value: $00CCFF; Name: 'Gold'),
    (Value: $00FFFF; Name: 'Yellow'),
    (Value: $00FF00; Name: 'Bright Green'),
    (Value: $FFFF00; Name: 'Turquoise'),
    (Value: $FFCC00; Name: 'Sky Blue'),
    (Value: $663399; Name: 'Plum'),
    (Value: $C0C0C0; Name: 'Gray-25%'),

    (Value: $CC99FF; Name: 'Rose'),
    (Value: $99CCFF; Name: 'Tan'),
    (Value: $99FFFF; Name: 'Light Yellow'),
    (Value: $CCFFCC; Name: 'Light Green'),
    (Value: $FFFFCC; Name: 'Light Turquoise'),
    (Value: $FFCC99; Name: 'Pale Blue'),
    (Value: $FF99CC; Name: 'Lavender'),
    (Value: $FFFFFF; Name: 'White'));

type
  TTBCustomItemAccess = class(TTBCustomItem);
  TTBItemViewerAccess = class(TTBItemViewer);
  TTBViewAccess = class(TTBView);
  TTBDockAccess = class(TTBDock);
  TTBCustomDockableWindowAccess = class(TTBCustomDockableWindow);
  TTBBasicBackgroundAccess = class(TTBBasicBackground);
  TControlAccess = class(TControl);
  TWinControlAccess = class(TWinControl);
  TCustomFormAccess = class(TCustomForm);
  TActionLinkAccess = class(TActionLink);

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Item Helpers }

procedure SpFillItemInfo(ACanvas: TCanvas; IV: TTBItemViewer; out ItemInfo: TSpTBXMenuItemInfo);
var
  Item: TTBCustomItemAccess;
  View: TTBViewAccess;

  IsHoverItem, IsOpen, IsPushed: Boolean;
  IsSplit, IsComboPushed: Boolean;
  IsToolbarStyle, ImageIsShown: Boolean;
  ImgSize, RightImageSize: TSize;
begin
  Item := TTBCustomItemAccess(IV.Item);
  View := TTBViewAccess(IV.View);

  if IV is TSpTBXItemViewer then
    IsToolbarStyle := TSpTBXItemViewer(IV).IsToolbarStyle
  else
    IsToolbarStyle := IV.IsToolbarStyle;
  IsOpen := IV = View.OpenViewer;
  IsHoverItem := IV = View.Selected;
  IsPushed := IsHoverItem and (IsOpen or (View.MouseOverSelected and View.Capture and IsToolbarStyle));
  IsSplit := tbisCombo in Item.ItemStyle;
  IsComboPushed := IsSplit and IsPushed and not View.Capture;
  if IsComboPushed then IsPushed := False;

  ImgSize.cx := 0;
  ImgSize.cy := 0;
  RightImageSize.cx := 0;
  RightImageSize.cy := 0;
  ImageIsShown := False;
  if IV is TSpTBXItemViewer then begin
    if TSpTBXItemViewer(IV).GetImageShown then begin
      ImgSize := TSpTBXItemViewer(IV).GetImageSize;
      if (ImgSize.cx > 0) and (ImgSize.cy > 0) then
        ImageIsShown := True;
    end;
    RightImageSize := TSpTBXItemViewer(IV).GetRightImageSize;
  end;

  FillChar(ItemInfo, SizeOf(ItemInfo), 0);
  ItemInfo.Enabled := Item.Enabled or View.Customizing;
  ItemInfo.Pushed := IsPushed;
  ItemInfo.Checked := Item.Checked;
  ItemInfo.HotTrack := IsHoverItem;
  ItemInfo.ImageShown := ImageIsShown;
  ItemInfo.ImageOrCheckShown := ImageIsShown or (not IsToolbarStyle and Item.Checked);
  ItemInfo.ImageSize := ImgSize;
  ItemInfo.RightImageSize := RightImageSize;
  ItemInfo.IsDesigning := csDesigning in View.ComponentState;
  ItemInfo.IsOnMenuBar := vsMenuBar in View.Style;
  ItemInfo.IsVertical := (View.Orientation = tbvoVertical) and not IsSplit;
  ItemInfo.IsOnToolbox := False;
  if IV is TSpTBXItemViewer then
    ItemInfo.IsOnToolbox := TSpTBXItemViewer(IV).IsOnToolBoxPopup;
  ItemInfo.IsOpen := IsOpen;
  ItemInfo.IsSplit := IsSplit;
  ItemInfo.ComboRect := Rect(0, 0, 0, 0);
  // HasArrow = tboDropdownArrow or (Submenu and Combo)
  ItemInfo.HasArrow := (tboDropdownArrow in Item.Options) or
    ((tbisSubmenu in Item.ItemStyle) and (tbisCombo in Item.ItemStyle));
  ItemInfo.ToolbarStyle := IsToolbarStyle;

  if ItemInfo.ToolbarStyle then
  begin
    if ItemInfo.HasArrow then
      ItemInfo.ComboPart := cpCombo;
    if ItemInfo.IsSplit then
      ItemInfo.ComboPart := cpSplitLeft;
  end
  else begin
    // Only for menu items
    if View.Window is TSpTBXPopupWindow then
      CurrentSkin.GetMenuItemMargins(ACanvas, TSpTBXPopupWindow(View.Window).MaximumImageSize.cx, ItemInfo.MenuMargins)
    else
      CurrentSkin.GetMenuItemMargins(ACanvas, ImgSize.cx, ItemInfo.MenuMargins);
  end;

  if tbisClicksTransparent in Item.ItemStyle then
    ItemInfo.State := CurrentSkin.GetState(ItemInfo.Enabled, False, False, False)
  else
    ItemInfo.State := CurrentSkin.GetState(ItemInfo.Enabled, ItemInfo.Pushed, ItemInfo.HotTrack, ItemInfo.Checked);

  ItemInfo.ComboState := sknsNormal;
  if IsSplit then begin
    ItemInfo.ComboState := ItemInfo.State;
    if IsComboPushed then
      ItemInfo.ComboState := sknsPushed
    else
      case ItemInfo.State of
        sknsPushed: ItemInfo.ComboState := sknsHotTrack;
        sknsChecked: ItemInfo.ComboState := sknsNormal;
        sknsCheckedAndHotTrack: ItemInfo.ComboState := sknsHotTrack;
      end;
  end;

  ItemInfo.SkinType := SkinManager.GetSkinType;

  // [Theme-Change]
  // The Default theme paints the caption of the pushed button in a down
  // state, this only happens when the Item is on a toolbar and:
  // 1) Windows themes are enabled and the item is not on a menubar
  // 2) Windows themes are disabled
  ItemInfo.IsSunkenCaption := False;
  if (ItemInfo.Pushed or ItemInfo.Checked) and ItemInfo.ToolbarStyle then
    ItemInfo.IsSunkenCaption := (not ItemInfo.IsOnMenuBar and (ItemInfo.SkinType = sknWindows)) or
      (ItemInfo.SkinType = sknNone);
end;

function SpGetBoundsRect(IV: TTBItemViewer; Root: TTBRootItem): TRect;
// Returns the Bounds Rect of an ItemViewer.
// If the ItemViewer is a GroupItemViewer then it returns the sum
// of all the ItemViewers inside it.
var
  G: TTBItemViewer;
  V: TTBView;
  I, J: Integer;
  R: TRect;
  FirstItemFound: Boolean;
begin
  Result := Rect(0, 0, 0, 0);
  if Assigned(IV) then
    if IV.Item is TTBGroupItem then begin
      // Sum all the ItemViewers of the GroupItem
      V := IV.View;
      J := IV.Index + 1;
      FirstItemFound := False;
      for I := J to V.ViewerCount - 1 do begin
        G := V.Viewers[I];
        if (G.Item.Parent = Root) then
          Break
        else
          if G.Item.Visible and not (G.Item is TTBGroupItem) then
            if not FirstItemFound then begin
              FirstItemFound := True;
              Result := G.BoundsRect;
            end
            else begin
              R := G.BoundsRect;
              Result.Left := Min(Result.Left, R.Left);
              Result.Top := Min(Result.Top, R.Top);
              Result.Right  := Max(Result.Right, R.Right);
              Result.Bottom := Max(Result.Bottom, R.Bottom);
            end;
      end;
    end
    else
      Result := IV.BoundsRect;
end;

procedure SpGetAllItems(AParentItem: TTBCustomItem; ItemsList: TStringList; ClearFirst: Boolean = True);
// Returns a StringList with all the items, subitems and linked items from AParentItem.
// The ItemsList.Strings[] contains the items name
// The ItemsList.Objects[] contains the items reference

  procedure Iterate(AParentItem: TTBCustomItem; LinkDepth: Integer);
  var
    I: Integer;
    NewParentItem, Item: TTBCustomItem;
  begin
    NewParentItem := AParentItem;
    if Assigned(NewParentItem.LinkSubitems) then begin
      NewParentItem := NewParentItem.LinkSubitems;
      Inc(LinkDepth);
      if LinkDepth > 25 then
        Exit;  { prevent infinite link recursion }
    end;
    for I := 0 to NewParentItem.Count - 1 do begin
      Item := NewParentItem.Items[I];
      ItemsList.AddObject(Item.Name, Item);
      Iterate(Item, LinkDepth);
    end;
  end;

begin
  if ClearFirst then
    ItemsList.Clear;
  Iterate(AParentItem, 0);
end;

function SpGetMenuMaximumImageSize(View: TTBView): TSize;
// Iterates the viewers and returns the maximum image size
var
  I: Integer;
  IV: TTBItemViewer;
  IL: TCustomImageList;
  ImageShown: Boolean;
begin
  Result.cx := 0;
  Result.cy := 0;
  for I := 0 to View.ViewerCount - 1 do begin
    IV := View.Viewers[I];
    if IV is TSpTBXItemViewer then
      ImageShown := TSpTBXItemViewer(IV).GetImageShown
    else
      ImageShown := TTBItemViewerAccess(IV).ImageShown;

    if ImageShown then begin
      IL := TTBItemViewerAccess(IV).GetImageList;
      if Assigned(IL) then begin
        if IL.Width > Result.cx then
          Result.cx := IL.Width;
        if IL.Height > Result.cy then
          Result.cy := IL.Height;
      end;
    end;
  end;
end;

function SpGetItemViewerFromPoint(Root: TTBRootItem; View: TTBView; P: TPoint;
  ProcessGroupItems: Boolean = True): TTBItemViewer;
// Returns the ItemViewer at the given position
// If ProcessGroupItems is true and the ItemViewer is on a GroupItem return
// the GroupItem's ItemViewer instead.
var
  IV: TTBItemViewer;
  I, X: Integer;
  G: TTBItemViewer;
begin
  Result := nil;
  if Assigned(Root) and Assigned(View) then begin
    IV := View.ViewerFromPoint(P);

    // If the Item is not on the Root it must be part of a GroupItem
    if ProcessGroupItems and Assigned(IV) and not (IV.Item is TTBChevronItem) and (IV.Item.Parent <> Root) then begin
      // Get the parent GroupItem ItemViewer
      X := IV.Index;
      for I := X downto 0 do begin
        G := IV.View.Viewers[I];
        if G.Item is TTBGroupItem then begin
          Result := G;
          Break;
        end;
      end;
    end
    else
      Result := IV;
  end;
end;

function SpGetNextItemSameEdge(View: TTBView; IV: TTBItemViewer; GoForward: Boolean;
  SearchType: TSpTBXSearchItemViewerType): TTBItemViewer;
// Returns the left or right Tab item depending on GoForward
// SearchType can be:
//  sivtNormal: Normal search
//  sivtInmediate: Search for the inmediate ItemViewer
//  sivtInmediateSkipNonVisible: Search for the next inmediate ItemViewer, skipping non visible ones
var
  Temp: TTBItemViewer;
  I: Integer;
begin
  Result := nil;
  if IV = nil then
    Result := View.NextSelectable(nil, GoForward)
  else begin
    View.ValidatePositions;
    I := View.IndexOf(IV);
    while not Assigned(Result) do begin
      if GoForward then Inc(I)
      else Dec(I);

      if (I > View.ViewerCount - 1) or (I < 0) then
        Break
      else begin
        Temp := View.Viewers[I];
        // Skip non visible items, search for same edge items
        // Can't test for Temp.BoundsRect.Top = IV.BoundsRect.Top because
        // if the item doesn't have a caption BoundsRect.Top = 7
        if Temp.Item.Visible and (Temp.OffEdge = IV.OffEdge) {and (Temp.BoundsRect.Top = IV.BoundsRect.Top)} then begin
          Result := Temp;  // Found IV
          Break;
        end
        else begin
          case SearchType of
            sivtInmediate:
              Break;  // Inmediate not found, exit
            sivtInmediateSkipNonVisible:
              if Temp.Item.Visible then Break;  // Inmediate not found and visible, exit
          end;
        end;
      end;
    end;
  end;
end;

function SpGetFirstRightAlignSpacer(View: TTBView): TSpTBXItemViewer;
var
  I: Integer;
  IV: TTBItemViewer;
begin
  Result := nil;
  for I := 0 to View.ViewerCount - 1 do begin
    IV := View.Viewers[I];
    if IV.Item.Visible and (IV.Item is TSpTBXRightAlignSpacerItem) then
      Result := IV as TSpTBXItemViewer;
  end;
end;

function SpGetRightAlignedItems(View: TTBView; RightAlignedList: TList;
  IsRotated: Boolean; out VisibleTotalWidth, RightAlignedTotalWidth: Integer): TSpTBXItemViewer;

  function GetWidth(R: TRect): Integer;
  begin
    if IsRotated then
      Result := R.Bottom - R.Top
    else
      Result := R.Right - R.Left;
  end;

var
  I: Integer;
  IV: TTBItemViewer;
begin
  Result := nil;

  if Assigned(RightAlignedList) then
    RightAlignedList.Clear;
  VisibleTotalWidth := 0;
  RightAlignedTotalWidth := 0;

  for I := 0 to View.ViewerCount - 1 do begin
    IV := View.Viewers[I];
    if IV.Item.Visible then
      VisibleTotalWidth := VisibleTotalWidth + GetWidth(IV.BoundsRect);
    if not Assigned(Result) and (IV.Item.Visible) and (IV.Item is TSpTBXRightAlignSpacerItem) then
      Result := IV as TSpTBXItemViewer;
    if Assigned(Result) then begin
      if Assigned(RightAlignedList) then
        RightAlignedList.Add(IV);
      RightAlignedTotalWidth := RightAlignedTotalWidth + GetWidth(IV.BoundsRect);
    end;
  end;
end;

procedure SpInvalidateItem(View: TTBView; Item: TTBCustomItem);
var
  IV: TTBItemViewer;
begin
  IV := View.Find(Item);
  if Assigned(IV) then View.Invalidate(IV);
end;

function SpFindItemViewer(View: TTBView; Item: TTBCustomItem): TTBItemViewer;
var
  I: Integer;
begin
  Result := nil;
  if Assigned(Item) then
    for I := 0 to View.ViewerCount - 1 do
      if View.Viewers[I].Item = Item then begin
        Result := View.Viewers[I];
        Exit;
      end;
end;

function SpFindControlItem(Item: TTBCustomItem; Ctl: TControl; Recurse: Boolean): TTBControlItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Item.Count - 1 do begin
    if Recurse and (Item[I] is TTBGroupItem) then begin
      Result := SpFindControlItem(Item[I], Ctl, True);
      if Assigned(Result) then
        Break;
    end;
    if (Item[I] is TTBControlItem) and (TTBControlItem(Item[I]).Control = Ctl) then begin
      Result := TTBControlItem(Item[I]);
      Break;
    end;
  end;
end;

procedure SpGetDropPosItemViewer(Root: TTBRootItem; View: TTBView; P: TPoint;
  out DestIV: TTBItemViewer; out DestItemPos: Integer; out DropMark: TRect);
// Returns the ItemViewer and Item index at the given position for Drag & Drop
// operations that needs a DropMark rect.
// Use this when the items are dropped when the drag operation is finished.
var
  DestR: TRect;
const
  DropMarkSize = 4;
begin
  DestItemPos := -1;
  DestIV := SpGetItemViewerFromPoint(Root, View, P);
  DropMark := Rect(0, 0, 0, 0);
  if Assigned(DestIV) then begin
    // Get the destination item position
    DestItemPos := Root.IndexOf(DestIV.Item);
    DestR := SpGetBoundsRect(DestIV, Root);
    if View.Orientation = tbvoVertical then begin
      if P.Y > ((DestR.Bottom - DestR.Top) div 2) + DestR.Top then begin
        Inc(DestItemPos);
        DropMark := Rect(0, DestR.Bottom, View.BaseSize.X, DestR.Bottom + DropMarkSize);
      end
      else
        DropMark := Rect(0, DestR.Top, View.BaseSize.X, DestR.Top + DropMarkSize);
    end
    else
      if P.X > ((DestR.Right - DestR.Left) div 2) + DestR.Left then begin
        Inc(DestItemPos);
        DropMark := Rect(DestR.Right, 0, DestR.Right + DropMarkSize, View.BaseSize.Y);
      end
      else
        DropMark := Rect(DestR.Left, 0, DestR.Left + DropMarkSize, View.BaseSize.Y);
  end;
end;

procedure SpGetDropPosItemViewer(Root: TTBRootItem; View: TTBView; P: TPoint;
  SourceItemPos: Integer; out DestIV: TTBItemViewer; out DestItemPos: Integer);
// Returns the ItemViewer and Item index at the given position for inmediate
// Drag & Drop operations without a DropMark.
// Use this when the items are moved while the mouse is being dragged.
var
  DestR: TRect;
begin
  DestItemPos := -1;
  DestIV := SpGetItemViewerFromPoint(Root, View, P);
  if Assigned(DestIV) then begin
    // Get the destination item position
    DestItemPos := Root.IndexOf(DestIV.Item);
    DestR := SpGetBoundsRect(DestIV, Root);

    if View.Orientation = tbvoVertical then begin
      if P.Y > ((DestR.Bottom - DestR.Top) div 2) + DestR.Top then begin
        if DestItemPos - 1 <> SourceItemPos then Inc(DestItemPos);
      end
      else begin
        if DestItemPos - 1 = SourceItemPos then Dec(DestItemPos);
      end;
    end
    else
      if P.X > ((DestR.Right - DestR.Left) div 2) + DestR.Left then begin
        if DestItemPos - 1 <> SourceItemPos then Inc(DestItemPos);
      end
      else begin
        if DestItemPos - 1 = SourceItemPos then Dec(DestItemPos);
      end;
  end;
end;

function SpGetDragHandleSize(Toolbar: TTBCustomDockableWindow): Integer;
const
  DragHandleSizes: array [Boolean, 0..2] of Integer = ((9, 0, 6), (14, 14, 14));
var
  T: TTBCustomDockableWindowAccess;
begin
  Result := 0;
  if Assigned(Toolbar.CurrentDock) then
    if Toolbar.CurrentDock.AllowDrag then begin
      T := TTBCustomDockableWindowAccess(Toolbar);
      Result := DragHandleSizes[T.CloseButtonWhenDocked, Ord(T.DragHandleStyle)]
    end;
end;

function SpIsVerticalToolbar(Toolbar: TTBCustomDockableWindow): Boolean;
begin
  if Assigned(Toolbar.CurrentDock) then
    Result := Toolbar.CurrentDock.Position in [dpLeft, dpRight]
  else
    Result := False;
end;

function SpIsDockUsingBitmap(Dock: TTBDock): Boolean;
var
  Background: TTBBasicBackgroundAccess;
begin
  Background := TTBBasicBackgroundAccess(Dock.Background);
  Result := Assigned(Background) and Background.UsingBackground;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Painting helpers }

procedure SpDrawXPToolbarButton(ACanvas: TCanvas; ARect: TRect; State: TSpTBXSkinStatesType;
  ComboPart: TSpTBXComboPart = cpNone);
// Paints a toolbar button depending on the State and SkinType
var
  ForceRectBorders: TAnchors;
  Details: TThemedElementDetails;
begin
  case SkinManager.GetSkinType of
    sknNone:
      begin
        case State of
          sknsNormal, sknsDisabled: ; // Do nothing
          sknsHotTrack:
            Windows.DrawEdge(ACanvas.Handle, ARect, BDR_RAISEDINNER, BF_RECT);
          sknsPushed, sknsCheckedAndHotTrack:
            Windows.DrawEdge(ACanvas.Handle, ARect, BDR_SUNKENOUTER, BF_RECT);
          sknsChecked:
            begin
              ACanvas.Brush.Bitmap := AllocPatternBitmap(clBtnFace, clBtnHighlight);;
              ACanvas.Brush.Bitmap.HandleType := bmDDB;  // Needed for Win95, or else brush is solid white
              ACanvas.FillRect(ARect);
              ACanvas.Brush.Style := bsClear;
              Windows.DrawEdge(ACanvas.Handle, ARect, BDR_SUNKENOUTER, BF_RECT);
            end;
        end;
      end;
    sknWindows, sknDelphiStyle:
      begin
        case ComboPart of
          cpNone:
            CurrentSkin.GetThemedElementDetails(skncToolbarItem, State, Details);
          cpCombo:
            case State of
              sknsDisabled: Details := SpTBXThemeServices.GetElementDetails(ttbDropDownButtonDisabled);
              sknsHotTrack: Details := SpTBXThemeServices.GetElementDetails(ttbDropDownButtonHot);
              sknsPushed:   Details := SpTBXThemeServices.GetElementDetails(ttbDropDownButtonPressed);
              sknsChecked:  Details := SpTBXThemeServices.GetElementDetails(ttbDropDownButtonChecked);
              sknsCheckedAndHotTrack: Details := SpTBXThemeServices.GetElementDetails(ttbDropDownButtonCheckedHot);
            else
              Details := SpTBXThemeServices.GetElementDetails(ttbDropDownButtonNormal);
            end;
          cpSplitLeft:
            case State of
              sknsDisabled: Details := SpTBXThemeServices.GetElementDetails(ttbSplitButtonDisabled);
              sknsHotTrack: Details := SpTBXThemeServices.GetElementDetails(ttbSplitButtonHot);
              sknsPushed:   Details := SpTBXThemeServices.GetElementDetails(ttbSplitButtonPressed);
              sknsChecked:  Details := SpTBXThemeServices.GetElementDetails(ttbSplitButtonChecked);
              sknsCheckedAndHotTrack: Details := SpTBXThemeServices.GetElementDetails(ttbSplitButtonCheckedHot);
            else
              Details := SpTBXThemeServices.GetElementDetails(ttbSplitButtonNormal);
            end;
          cpSplitRight:
            case State of
              sknsDisabled: Details := SpTBXThemeServices.GetElementDetails(ttbSplitButtonDropDownDisabled);
              sknsHotTrack: Details := SpTBXThemeServices.GetElementDetails(ttbSplitButtonDropDownHot);
              sknsPushed:   Details := SpTBXThemeServices.GetElementDetails(ttbSplitButtonDropDownPressed);
              sknsChecked:  Details := SpTBXThemeServices.GetElementDetails(ttbSplitButtonDropDownChecked);
              sknsCheckedAndHotTrack: Details := SpTBXThemeServices.GetElementDetails(ttbSplitButtonDropDownCheckedHot);
            else
              Details := SpTBXThemeServices.GetElementDetails(ttbSplitButtonDropDownNormal);
            end;
        end;
        CurrentSkin.PaintThemedElementBackground(ACanvas, ARect, Details);
      end;
    sknSkin:
      begin
        ForceRectBorders := [];
        if ComboPart = cpSplitLeft then ForceRectBorders := [akRight]
        else
          if ComboPart = cpSplitRight then ForceRectBorders := [akLeft];
        CurrentSkin.PaintBackground(ACanvas, ARect, skncToolbarItem, State, True, True, False, ForceRectBorders);
      end;
  end;
end;

procedure SpDrawXPMenuItem(ACanvas: TCanvas; ARect: TRect; ItemInfo: TSpTBXMenuItemInfo);
// Paints a menu or toolbar item depending on the ItemInfo

  procedure ToolbarItemDraw(ARect: TRect);
  var
    ForceRectBorders: TAnchors;
  begin
    ForceRectBorders := [];
    if ItemInfo.IsSplit then
      if ItemInfo.ComboPart = cpSplitLeft then ForceRectBorders := [akRight]
      else
        if ItemInfo.ComboPart = cpSplitRight then ForceRectBorders := [akLeft];

    case ItemInfo.SkinType of
      sknNone:
        begin
          if ItemInfo.Checked then
            if not ItemInfo.HotTrack or (not ItemInfo.Enabled and ItemInfo.ToolbarStyle) then
              ItemInfo.State := sknsChecked;  // Paint Disabled&Checked as Checked

          if ItemInfo.State = sknsNormal then begin
            if ItemInfo.IsDesigning then
              Windows.DrawEdge(ACanvas.Handle, ARect, BDR_RAISEDINNER, BF_RECT);
          end
          else
            SpDrawXPToolbarButton(ACanvas, ARect, ItemInfo.State, ItemInfo.ComboPart);
        end;
      sknWindows, sknDelphiStyle:
        if ItemInfo.IsDesigning then
          SpDrawRectangle(ACanvas, ARect, 2, clBtnShadow, clBtnShadow, clNone, clNone, ForceRectBorders)
        else
          if ItemInfo.IsOnMenuBar then begin
            if SpIsWinVistaOrUp or (ItemInfo.SkinType = sknDelphiStyle) then begin
              if ItemInfo.State <> sknsNormal then
                CurrentSkin.PaintThemedElementBackground(ACanvas, ARect, skncMenuBarItem, ItemInfo.State);
            end
            else
              if ItemInfo.State in [sknsHotTrack, sknsPushed, sknsChecked, sknsCheckedAndHotTrack] then
                SpFillRect(ACanvas, ARect, clHighlight);
          end
          else
            SpDrawXPToolbarButton(ACanvas, ARect, ItemInfo.State, ItemInfo.ComboPart);
      sknSkin:
        if ItemInfo.IsOpen and CurrentSkin.OfficePopup then begin
          // Paints skncOpenToolbarItem skin, hide the bottom border
          ARect.Bottom := ARect.Bottom + 2;
          CurrentSkin.PaintBackground(ACanvas, ARect, skncOpenToolbarItem, sknsNormal, True, True)
        end
        else
          if ItemInfo.IsDesigning then
            SpDrawRectangle(ACanvas, ARect, 2, clBtnShadow, clBtnShadow, clNone, clNone, ForceRectBorders)
          else
            if ItemInfo.IsOnMenuBar then
              CurrentSkin.PaintBackground(ACanvas, ARect, skncMenuBarItem, ItemInfo.State, True, True, False, ForceRectBorders)
            else
              SpDrawXPToolbarButton(ACanvas, ARect, ItemInfo.State, ItemInfo.ComboPart);
    end;
  end;

  procedure MenuItemDraw(ARect: TRect);
  begin
    case ItemInfo.SkinType of
      sknNone:
        SpFillRect(ACanvas, ARect, clHighlight);
      sknWindows, sknDelphiStyle:
        if SpIsWinVistaOrUp or (ItemInfo.SkinType = sknDelphiStyle) then
          CurrentSkin.PaintThemedElementBackground(ACanvas, ARect, skncMenuItem, ItemInfo.Enabled, False, ItemInfo.HotTrack, False, False, False, False)
        else
          SpFillRect(ACanvas, ARect, clHighlight);
      sknSkin:
        CurrentSkin.PaintBackground(ACanvas, ARect, skncMenuItem, sknsHotTrack, True, True);
    end;
  end;

var
  R: TRect;
  DrawCheckBoxFrame: Boolean;
  C: TColor;
  Details: TThemedElementDetails;
begin
  if ItemInfo.ToolbarStyle then begin // Toolbar Item
    if ItemInfo.IsSplit then begin
      if ItemInfo.IsOpen and CurrentSkin.OfficePopup then begin
        // If it's Split, Open and OfficePopup paint the skncOpenToolbarItem skin
        ARect.Right := ItemInfo.ComboRect.Right;
        ToolbarItemDraw(ARect);
      end
      else begin
        // Draw the left side button
        if (ItemInfo.SkinType = sknSkin) or ItemInfo.IsDesigning then
          Inc(ARect.Right, 2);
        ItemInfo.ComboPart := cpSplitLeft;
        ToolbarItemDraw(ARect);

        // Draw the right side button
        ItemInfo.ComboPart := cpSplitRight;
        ItemInfo.State := ItemInfo.ComboState;
        ToolbarItemDraw(ItemInfo.ComboRect);
      end;
    end
    else
      ToolbarItemDraw(ARect);
  end
  else begin // Menu item
    // DrawCheckBoxFrame is true when the item is checked
    DrawCheckBoxFrame := False;
    case ItemInfo.SkinType of
      sknNone:
        DrawCheckBoxFrame := ItemInfo.Checked or ItemInfo.ImageOrCheckShown;
      sknWindows, sknDelphiStyle:
        if SpIsWinVistaOrUp or (ItemInfo.SkinType = sknDelphiStyle) then
          DrawCheckBoxFrame := ItemInfo.Enabled and ItemInfo.Checked
        else
          DrawCheckBoxFrame := ItemInfo.Enabled and (ItemInfo.Checked or ItemInfo.ImageOrCheckShown);
      sknSkin:
        DrawCheckBoxFrame := ItemInfo.Enabled and ItemInfo.Checked;
    end;

    R := ARect;

    // Draw the item selection rectangle hottrack
    // Office doesn't draw the hottrack when the item is disabled, check
    // if OfficeMenu is set.
    if ItemInfo.HotTrack and (ItemInfo.Enabled or not CurrentSkin.OfficeMenu) then begin
      // Don't draw the hottrack selection behind the checkbox frame
      // Make sure to add some margin to the left border
      if DrawCheckBoxFrame then begin
        case ItemInfo.SkinType of
          sknNone:
            Inc(R.Left, ItemInfo.MenuMargins.GutterSize + 1);
          sknWindows, sknDelphiStyle: ;
//            Inc(R.Left, ItemInfo.MenuMargins.GutterSize + 1);
          sknSkin:
            // Only if the CheckedAndHottrack state doesn't have external borders
            if CurrentSkin.Options(skncMenuItem, sknsCheckedAndHotTrack).Borders.Color1 <> clNone then
              Inc(R.Left, 3);
        end;
      end;

      MenuItemDraw(R);
    end;

    // Draw the checkbox frame (the checkbox glyph is painted in the ItemViewer.Paint method)
    if DrawCheckBoxFrame then begin
      R := ARect;
      R.Right := R.Left + ItemInfo.MenuMargins.GutterSize;


      case ItemInfo.SkinType of
        sknNone:
          ToolbarItemDraw(R);
        sknWindows, sknDelphiStyle:
          if SpIsWinVistaOrUp or (ItemInfo.SkinType = sknDelphiStyle) then begin
            // The checkbox frame is not painted
            // [Old-Themes]
            {$IF CompilerVersion >= 23} //for Delphi XE2 and up
            if ItemInfo.State = sknsDisabled then Details := SpTBXThemeServices.GetElementDetails(tmPopupCheckBackgroundDisabled)
            else if ItemInfo.ImageShown then Details := SpTBXThemeServices.GetElementDetails(tmPopupCheckBackgroundBitmap)
            else Details := SpTBXThemeServices.GetElementDetails(tmPopupCheckBackgroundNormal);
            {$ELSE}
            Details.Element := teMenu;
            Details.Part := MENU_POPUPCHECKBACKGROUND;
            if ItemInfo.State = sknsDisabled then Details.State := MCB_DISABLED
            else if ItemInfo.ImageShown then Details.State := MCB_BITMAP
            else Details.State := MCB_NORMAL;
            {$IFEND}
            CurrentSkin.PaintThemedElementBackground(ACanvas, R, Details);
          end
          else
            ToolbarItemDraw(R);
        sknSkin:
          CurrentSkin.PaintBackground(ACanvas, R, skncMenuItem, ItemInfo.State, True, True);
      end;
    end;

    // Draw the combo item separator
    if ItemInfo.IsSplit then begin
      if ItemInfo.SkinType = sknSkin then
        C := SkinManager.CurrentSkin.Options(skncMenuItem, ItemInfo.State).Borders.Color1
      else
        C := clBtnShadow;
      R := ARect;
      R.Left := ARect.Right - 10 - 4;
      SpDrawLine(ACanvas, R.Left, R.Top + 1, R.Left, R.Bottom - 1, C);
    end;
  end;
end;

procedure SpDrawXPMenuSeparator(ACanvas: TCanvas; ARect: TRect; MenuItemStyle, Vertical: Boolean);
const
  ToolbarPartFlags: array [Boolean] of Integer = (TP_SEPARATORVERT, TP_SEPARATOR);
var
  R: TRect;
  C: TColor;
  VistaSeparatorSize: TSize;
  Details: TThemedElementDetails;
  SkinType: TSpTBXSkinType;
begin
  R := ARect;
  SkinType := SkinManager.GetSkinType;
  case SkinType of
    sknNone:
      if not Vertical then begin
        R.Top := (R.Top + R.Bottom) div 2 - 1;
        DrawEdge(ACanvas.Handle, R, EDGE_ETCHED, BF_TOP);
      end
      else begin
        R.Left := (R.Left + R.Right) div 2 - 1;
        DrawEdge(ACanvas.Handle, R, EDGE_ETCHED, BF_LEFT);
      end;
    sknWindows, sknDelphiStyle:
      if MenuItemStyle then begin
        if SpIsWinVistaOrUp or (SkinType = sknDelphiStyle) then begin
          // [Old-Themes]
          {$IF CompilerVersion >= 23} //for Delphi XE2 and up
          Details := SpTBXThemeServices.GetElementDetails(tmPopupSeparator);
          {$ELSE}
          Details.Element := teMenu;
          Details.Part := MENU_POPUPSEPARATOR;
          Details.State := 0;
          {$IFEND}
          VistaSeparatorSize := CurrentSkin.GetThemedElementSize(ACanvas, Details);
          R := SpCenterRectVert(R, VistaSeparatorSize.cy);
          CurrentSkin.PaintThemedElementBackground(ACanvas, R, Details);
        end
        else
          if Vertical then begin
            R.Left := (R.Left + R.Right) div 2 - 1;
            Windows.DrawEdge(ACanvas.Handle, R, EDGE_ETCHED, BF_LEFT);
          end
          else begin
            R.Top := (R.Top + R.Bottom) div 2 - 1;
            Windows.DrawEdge(ACanvas.Handle, R, EDGE_ETCHED, BF_TOP);
          end;
      end
      else begin
        CurrentSkin.GetThemedElementDetails(skncSeparator, Vertical, False, False, False, False, False, False, Details);
        CurrentSkin.PaintThemedElementBackground(ACanvas, R, Details);
      end;
    sknSkin:
      if not Vertical then begin
        // ??? what happens when 2 items have different imagelist sizes? how is the gutter measured?
        R.Top := (R.Bottom + R.Top) div 2 - 1;
        R.Bottom := R.Top;
        C := SkinManager.CurrentSkin.Options(skncSeparator, sknsNormal).Body.Color1;
        SpDrawLine(ACanvas, R.Left, R.Top, R.Right, R.Bottom, C);
        OffsetRect(R, 0, 1);
        C := SkinManager.CurrentSkin.Options(skncSeparator, sknsNormal).Body.Color2;
        SpDrawLine(ACanvas, R.Left, R.Top, R.Right, R.Bottom, C);
      end
      else begin
        R.Left := (R.Right + R.Left) div 2 - 1;
        R.Right := R.Left;
        InflateRect(R, 0, -3);
        C := SkinManager.CurrentSkin.Options(skncSeparator, sknsNormal).Body.Color1;
        SpDrawLine(ACanvas, R.Left, R.Top, R.Right, R.Bottom, C);
        OffsetRect(R, 1, 0);
        C := SkinManager.CurrentSkin.Options(skncSeparator, sknsNormal).Body.Color2;
        SpDrawLine(ACanvas, R.Left, R.Top, R.Right, R.Bottom, C);
      end;
  end;
end;

procedure SpDrawXPMenuItemImage(ACanvas: TCanvas; ARect: TRect;
  const ItemInfo: TSpTBXMenuItemInfo; ImageList: TCustomImageList; ImageIndex: Integer);
begin
  if ImageList is TTBCustomImageList then begin
    if ItemInfo.IsSunkenCaption then
      OffsetRect(ARect, 1, 1);
    TTBCustomImageList(ImageList).DrawState(ACanvas, ARect.Left, ARect.Top,
      ImageIndex, ItemInfo.Enabled, ItemInfo.HotTrack, ItemInfo.Checked);
    Exit;
  end;

  if ItemInfo.Enabled and SkinManager.CurrentSkin.OfficeIcons then begin
    // Draw icon shadow
    if ItemInfo.HotTrack and not ItemInfo.Pushed then begin
      OffsetRect(ARect, 1, 1);
      SpDrawIconShadow(ACanvas, ARect, ImageList, ImageIndex);
      OffsetRect(ARect, -2, -2);
    end;
    SpDrawImageList(ACanvas, ARect, ImageList, ImageIndex, ItemInfo.Enabled, True);
  end
  else begin
    if ItemInfo.IsSunkenCaption then
      OffsetRect(ARect, 1, 1);
    SpDrawImageList(ACanvas, ARect, ImageList, ImageIndex, ItemInfo.Enabled, True);
  end;
end;

procedure SpDrawXPMenuGutter(ACanvas: TCanvas; ARect: TRect);
var
  Op: TSpTBXSkinOptionCategory;
  C: TColor;
  Details: TThemedElementDetails;
  SkinType: TSpTBXSkinType;
begin
  SkinType := SkinManager.GetSkinType;
  // If it's Windows theme and we're not on Vista do default painting
  if (SkinType = sknWindows) and not SpIsWinVistaOrUp and (SkinType <> sknDelphiStyle) then
    SkinType := sknNone;

  case SkinType of
    sknNone:; // No gutter on Windows 9x, 2000 and XP
    sknWindows, sknDelphiStyle: // Only Windows Vista painting
      if CurrentSkin.GetThemedElementDetails(skncGutter, sknsNormal, Details) then
        CurrentSkin.PaintThemedElementBackground(ACanvas, ARect, Details);
    sknSkin:
      begin
        Op := CurrentSkin.Options(skncGutter, sknsNormal);
        if not Op.Body.IsEmpty then
          SpPaintSkinBackground(ACanvas, ARect, Op, False);

        // Paint only the right side border, like a Separator line
        ARect.Left := ARect.Right;
        C := Op.Borders.Color2;
        if C <> clNone then begin
          SpDrawLine(ACanvas, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, C);
          OffsetRect(ARect, -1, 0);
        end;
        C := Op.Borders.Color1;
        if C <> clNone then
          SpDrawLine(ACanvas, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, C);
      end;
  end;
end;

procedure SpDrawXPMenuPopupWindow(ACanvas: TCanvas; ARect, OpenIVRect: TRect;
  DrawGutter: Boolean; ImageSize: Integer);
var
  GutterR: TRect;
  MarginsInfo: TSpTBXMenuItemMarginsInfo;
  SaveIndex: Integer;
  Details: TThemedElementDetails;
  SkinType: TSpTBXSkinType;
begin
  SkinType := SkinManager.GetSkinType;
  // If it's Windows theme and we're not on Vista do default painting
  if (SkinType = sknWindows) and not SpIsWinVistaOrUp and (SkinType <> sknDelphiStyle) then
    SkinType := sknNone;

  case SkinType of
    sknNone: // Windows 9x, 2000 and XP
      if not AreFlatMenusEnabled then begin
        DrawEdge(ACanvas.Handle, ARect, EDGE_RAISED, BF_RECT or BF_ADJUST);
        SpFillRect(ACanvas, ARect, clBtnFace);
      end
      else
        SpFillRect(ACanvas, ARect, clMenu, clBtnShadow);
    sknWindows, sknDelphiStyle: // Only Windows Vista painting, XP just fills the background
      begin
        SaveIndex := SaveDC(ACanvas.Handle);
        try
          CurrentSkin.PaintThemedElementBackground(ACanvas, ARect, skncPopup, sknsNormal);
          // Now paint the borders, clip the background
          ExcludeClipRect(ACanvas.Handle, ARect.Left + 2, ARect.Top + 2, ARect.Right - 2, ARect.Bottom - 2);
          // [Old-Themes]
          {$IF CompilerVersion >= 23} //for Delphi XE2 and up
          Details := SpTBXThemeServices.GetElementDetails(tmPopupBorders);
          {$ELSE}
          Details.Element := teMenu;
          Details.Part := MENU_POPUPBORDERS;
          Details.State := 0;
          {$IFEND}
          CurrentSkin.PaintThemedElementBackground(ACanvas, ARect, Details);
        finally
          RestoreDC(ACanvas.Handle, SaveIndex);
        end;
        // Don't paint the gutter on Windows
      end;
    sknSkin:
      begin
        // Paint the background, if OfficePopup is true just clip the top
        // border corresponding to the Open ItemViewer Rect
        if OpenIVRect.Top < 0 then begin
          OpenIVRect.Top := ARect.Top;
          OpenIVRect.Bottom := ARect.Top + 1;
          InflateRect(OpenIVRect, -1, 0);

          // First paint the background
          CurrentSkin.PaintBackground(ACanvas, ARect, skncPopup, sknsNormal, True, False);
          // Now paint the borders, clip the top border
          SaveIndex := SaveDC(ACanvas.Handle);
          try
            ExcludeClipRect(ACanvas.Handle, OpenIVRect.Left, OpenIVRect.Top, OpenIVRect.Right, OpenIVRect.Bottom);
            CurrentSkin.PaintBackground(ACanvas, ARect, skncPopup, sknsNormal, False, True);
          finally
            RestoreDC(ACanvas.Handle, SaveIndex);
          end;
        end
        else
          CurrentSkin.PaintBackground(ACanvas, ARect, skncPopup, sknsNormal, True, True);

        // Paint the gutter
        if DrawGutter and not CurrentSkin.Options(skncGutter, sknsNormal).IsEmpty then begin
          if ImageSize <= 0 then ImageSize := 16;
          CurrentSkin.GetMenuItemMargins(ACanvas, ImageSize, MarginsInfo);
          GutterR := ARect;
          InflateRect(GutterR, -1, -1);
          GutterR.Right := GutterR.Left + MarginsInfo.GutterSize + MarginsInfo.LeftCaptionMargin + 1; // +1 because the popup has 2 pixel border
          SpDrawXPMenuGutter(ACanvas, GutterR);
        end;
      end;
  end;
end;

procedure SpDrawXPStatusBar(ACanvas: TCanvas; ARect, AGripRect: TRect);
var
  R: TRect;
  C1, C2: TColor;
begin
  case SkinManager.GetSkinType of
    sknNone:
      begin
        if not IsRectEmpty(ARect) then begin
          SpFillRect(ACanvas, ARect, clBtnFace);
          SpDrawRectangle(ACanvas, ARect, 0, clBtnShadow, clWindow);
        end;
        if not IsRectEmpty(AGripRect) then begin
          InflateRect(AGripRect, 0, -1);
          DrawFrameControl(ACanvas.Handle, AGripRect, DFC_SCROLL, DFCS_SCROLLSIZEGRIP);
        end;
      end;
    sknWindows, sknDelphiStyle:
      begin
        if not IsRectEmpty(ARect) then
          CurrentSkin.PaintThemedElementBackground(ACanvas, ARect, skncStatusBar, True, False, False, False, False, False, False);
        if not IsRectEmpty(AGripRect) then
          CurrentSkin.PaintThemedElementBackground(ACanvas, AGripRect, skncStatusBarGrip, True, False, False, False, False, False, False);
      end;
    sknSkin:
      begin
        if not IsRectEmpty(ARect) then
          CurrentSkin.PaintBackground(ACanvas, ARect, skncStatusBar, sknsNormal, True, True);
        if not IsRectEmpty(AGripRect) then begin
          R := AGripRect;
          C1 := SkinManager.CurrentSkin.Options(skncStatusBarGrip).Body.Color1;
          C2 := SkinManager.CurrentSkin.Options(skncStatusBarGrip).Body.Color2;
          // Draw 3 cells at the bottom
          R.Left := R.Right - 12;
          R.Top := R.Bottom - 4;
          SpDrawXPGrip(ACanvas, R, C1, C2);
          // Draw 2 cells at the top
          R.Bottom := R.Top;
          R.Top := R.Bottom - 4;
          R.Left := R.Left + 4;
          SpDrawXPGrip(ACanvas, R, C1, C2);
          // Draw 1 cell at the top
          R.Bottom := R.Top;
          R.Top := R.Bottom - 4;
          R.Left := R.Left + 4;
          SpDrawXPGrip(ACanvas, R, C1, C2);
        end;
      end;
  end;
end;

procedure SpDrawXPTitleBar(ACanvas: TCanvas; ARect: TRect; IsActive: Boolean; DrawBorders: Boolean = True);
const
  W9xFlags: array [Boolean] of Integer = (0, DC_ACTIVE);
  W9xGradientFlag: array [Boolean] of Integer = (0, DC_GRADIENT);
var
  Gradient: Boolean;
  GradientBool: BOOL;
  B: TBitmap;
  Details: TThemedElementDetails;
  ElementSize: TSize;
begin
  case SkinManager.GetSkinType of
    sknNone:
      begin
        Gradient := SystemParametersInfo(SPI_GETGRADIENTCAPTIONS, 0, @GradientBool, 0) and GradientBool;
        Windows.DrawCaption(GetDesktopWindow, ACanvas.Handle, ARect, DC_TEXT or W9xFlags[IsActive] or W9xGradientFlag[Gradient]);
      end;
    sknWindows, sknDelphiStyle:
      if SkinManager.GetSkinType = sknDelphiStyle then begin
        // [Theme-Change]
        // Delphi Styles engine doesn't paint the borders stretched
        B := TBitmap.Create;
        try
          CurrentSkin.GetThemedElementDetails(skncWindowTitleBar, IsActive, False, False, False, False, False, False, Details);
          ElementSize := CurrentSkin.GetThemedElementSize(ACanvas, Details);
          B.Width := ARect.Right - ARect.Left;
          B.Height := ElementSize.cy;
          CurrentSkin.PaintThemedElementBackground(B.Canvas, Rect(0, 0, B.Width, B.Height), Details);
          ACanvas.StretchDraw(ARect, B);
        finally
          B.Free;
        end;
      end
      else
        CurrentSkin.PaintThemedElementBackground(ACanvas, ARect, skncWindowTitleBar, IsActive, False, False, False, False, False, False);
    sknSkin:
      CurrentSkin.PaintBackground(ACanvas, ARect, skncWindowTitleBar, sknsNormal, True, DrawBorders);
  end;
end;

procedure SpDrawXPTitleBarBody(ACanvas: TCanvas; ARect: TRect; IsActive: Boolean;
  BorderSize: TPoint; DrawBody: Boolean = True);
var
  R, MirrorR: TRect;
  SaveIndex: Integer;
  B: TBitmap;
  BottomBorder, LeftBorder, RightBorder: TThemedWindow;
  Details: TThemedElementDetails;
begin
  case SkinManager.GetSkinType of
    sknNone:
      begin
        if DrawBody then
          ACanvas.FillRect(ARect);
        SaveIndex := SaveDC(ACanvas.Handle);
        try
          R := ARect;
          InflateRect(R, -BorderSize.X, -BorderSize.Y);
          ExcludeClipRect(ACanvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
          if not DrawBody then
            ACanvas.FillRect(ARect);
          Windows.DrawEdge(ACanvas.Handle, ARect, EDGE_RAISED, BF_RECT);
        finally
          RestoreDC(ACanvas.Handle, SaveIndex);
        end;
      end;
    sknWindows, sknDelphiStyle:
      begin
        if SkinManager.GetSkinType = sknDelphiStyle then begin
          BottomBorder := twFrameBottomActive;
          LeftBorder := twFrameLeftActive;
          RightBorder := twFrameRightActive;
        end
        else begin
          // [Theme-Change]
          // Using twSmallFramexxx, otherwise the borders are rounded
          BottomBorder := twSmallFrameBottomActive;
          LeftBorder := twSmallFrameLeftActive;
          RightBorder := twSmallFrameRightActive;
        end;
        if not IsActive then begin
          BottomBorder := TThemedWindow(Ord(BottomBorder) + 1);
          LeftBorder := TThemedWindow(Ord(LeftBorder) + 1);
          RightBorder := TThemedWindow(Ord(RightBorder) + 1);
        end;

        R := ARect;
        Details := SpTBXThemeServices.GetElementDetails(LeftBorder);
        R.Top := ARect.Top + BorderSize.Y;
        R.Bottom := ARect.Bottom - BorderSize.Y;
        R.Right := R.Left + BorderSize.X;
        CurrentSkin.PaintThemedElementBackground(ACanvas, R, Details);

        Details := SpTBXThemeServices.GetElementDetails(RightBorder);
        R.Right := ARect.Right;
        R.Left := R.Right - BorderSize.X;
        CurrentSkin.PaintThemedElementBackground(ACanvas, R, Details);

        Details := SpTBXThemeServices.GetElementDetails(BottomBorder);
        R := ARect;
        R.Top := R.Bottom - BorderSize.Y;
        CurrentSkin.PaintThemedElementBackground(ACanvas, R, Details);

        // Don't know how to paint a captionless window frame
        // We have to mirror the bottom frame and paint it on the top
        B := TBitmap.Create;
        try
          R := ARect;
          R.Bottom := R.Top + BorderSize.Y;
          B.Width := R.Right - R.Left;
          B.Height := R.Bottom - R.Top;
          CurrentSkin.PaintThemedElementBackground(B.Canvas, Rect(0, 0, B.Width, B.Height), Details);
          // Mirror
          MirrorR := Rect(0, B.Height - 1, B.Width, -1);
          ACanvas.CopyRect(R, B.Canvas, MirrorR);
        finally
          B.Free;
        end;
      end;
    sknSkin:
      CurrentSkin.PaintWindowFrame(ACanvas, ARect, IsActive, DrawBody, BorderSize.X);
  end;
end;

procedure SpDrawXPDock(ACanvas: TCanvas; ARect: TRect; Vertical: Boolean = False);
begin
  case SkinManager.GetSkinType of
    sknNone:
      begin
        ACanvas.Brush.Color := clBtnFace;
        ACanvas.FillRect(ARect);
      end;
    sknWindows, sknDelphiStyle:
      begin
        if Vertical then Inc(ARect.Bottom, 1);  // Fix WindowsXP bug
        ACanvas.Brush.Color := CurrentSkin.GetThemedSystemColor(clBtnFace);
        ACanvas.FillRect(ARect);
        CurrentSkin.PaintThemedElementBackground(ACanvas, ARect, skncDock, Vertical, False, False, False, False, False, False);
      end;
    sknSkin:
      CurrentSkin.PaintBackground(ACanvas, ARect, skncDock, sknsNormal, True, True, Vertical);
  end;
end;

procedure SpDrawXPToolbar(ACanvas: TCanvas; ARect: TRect;
  Docked, Floating, Vertical, PaintSkinBackground, PaintBorders: Boolean;
  SkinComponent: TSpTBXSkinComponentsType = skncToolbar);
begin
  case SkinManager.GetSkinType of
    sknNone:
      if PaintBorders and Docked then
        Windows.DrawEdge(ACanvas.Handle, ARect, BDR_RAISEDINNER, BF_RECT or BF_ADJUST);
    sknWindows, sknDelphiStyle:
      if PaintBorders and Docked then  // Paint only the borders
        if SkinManager.GetSkinType = sknDelphiStyle then begin  // XE2 Styles
          SpDrawRectangle(ACanvas, ARect, 0, CurrentSkin.GetThemedSystemColor(cl3DLight), CurrentSkin.GetThemedSystemColor(cl3DDkShadow));
          // Paints the toolbar Style background
          // CurrentSkin.PaintThemedElementBackground(ACanvas, ARect, skncToolbar, True, False, False, False, False, False, False);
        end
        else
          SpDrawRectangle(ACanvas, ARect, 0, SpLighten(clBtnFace, 24), SpLighten(clBtnFace, -32));
    sknSkin:
      if Docked or Floating then begin
        if Floating and CurrentSkin.Options(SkinComponent, sknsNormal).Body.IsEmpty then // Floating and doesn't have a Body
          CurrentSkin.PaintBackground(ACanvas, ARect, skncDock, sknsNormal, True, False)
        else begin
          if Floating then PaintBorders := False;
          CurrentSkin.PaintBackground(ACanvas, ARect, SkinComponent, sknsNormal, PaintSkinBackground, PaintBorders, Vertical);
        end;
      end;
  end;
end;

procedure SpDrawXPToolbar(W: TTBCustomDockableWindow; ACanvas: TCanvas;
  ARect: TRect; PaintOnNCArea: Boolean; PaintBorders: Boolean = True;
  SkinComponent: TSpTBXSkinComponentsType = skncToolbar);
var
  R: TRect;
  PaintDefault, DrawSkinBody: Boolean;
  Toolbar: TTBCustomDockableWindowAccess;
  IsVertical: Boolean;
begin
  if CurrentSkin.Options(SkinComponent, sknsNormal).Body.IsEmpty then
    SkinComponent := skncToolbar;

  Toolbar := TTBCustomDockableWindowAccess(W);
  IsVertical := SpIsVerticalToolbar(Toolbar);
  DrawSkinBody := True;

  if Toolbar.Docked then begin
    // Prepare R
    R := Toolbar.CurrentDock.ClientRect;
    OffsetRect(R, -Toolbar.Left, -Toolbar.Top);
    if not PaintOnNCArea then begin
      if W is TSpTBXToolbar then
        OffsetRect(R, -TSpTBXToolbar(W).DefaultToolbarBorderSize, -TSpTBXToolbar(W).DefaultToolbarBorderSize)
      else
        OffsetRect(R, -CDefaultToolbarBorderSize, -CDefaultToolbarBorderSize);
      if IsVertical then
        Dec(R.Top, SpGetDragHandleSize(Toolbar))
      else
        Dec(R.Left, SpGetDragHandleSize(Toolbar));
    end;
    // Draw the Dock background
    if Toolbar.CurrentDock is TSpTBXDock then
      TSpTBXDock(Toolbar.CurrentDock).DrawBackground(ACanvas.Handle, R);

    DrawSkinBody := not (Toolbar.CurrentDock.BackgroundOnToolbars and SpIsDockUsingBitmap(Toolbar.CurrentDock));
  end
  else begin
    if Toolbar.Floating then begin
      if SkinManager.GetSkinType <> sknSkin then begin
        if Toolbar.Color = clNone then ACanvas.Brush.Color := CurrentSkin.GetThemedSystemColor(clBtnFace)
        else ACanvas.Brush.Color := Toolbar.Color;
        ACanvas.FillRect(ARect);
      end;
    end
    else begin
      // Draw the parent background if the toolbar is not docked nor floating
      // SpDrawParentBackground doesn't seem to work correctly here
      // (when a toolbar is inside a toolwindow), use PerformEraseBackground instead
      Controls.PerformEraseBackground(Toolbar, ACanvas.Handle);
    end;
  end;

  // Default painting
  PaintDefault := True;
  if W is TSpTBXToolbar then begin
    if TSpTBXToolbar(W).MenuBar then
      SkinComponent := skncMenuBar;
    TSpTBXToolbar(W).DoDrawBackground(ACanvas, ARect, pstPrePaint, PaintDefault);
  end
  else
    if W is TSpTBXCustomToolWindow then
      TSpTBXCustomToolWindow(W).DoDrawBackground(ACanvas, ARect, pstPrePaint, PaintDefault);

  if PaintDefault then begin
    R := ARect;
    if Toolbar.Color <> clNone then begin
      ACanvas.Brush.Color := Toolbar.Color;
      ACanvas.FillRect(R);
    end
    else
      SpDrawXPToolbar(ACanvas, R, Toolbar.Docked, Toolbar.Floating, IsVertical, DrawSkinBody, PaintBorders, SkinComponent);
  end;

  PaintDefault := True;
  if W is TSpTBXToolbar then
    TSpTBXToolbar(W).DoDrawBackground(ACanvas, ARect, pstPostPaint, PaintDefault)
  else
    if W is TSpTBXCustomToolWindow then
      TSpTBXCustomToolWindow(W).DoDrawBackground(ACanvas, ARect, pstPostPaint, PaintDefault);
end;

procedure SpDrawXPToolbarGrip(W: TTBCustomDockableWindow; ACanvas: TCanvas; ARect: TRect);
const
  Pattern: array [0..15] of Byte = (0, 0, $CC, 0, $78, 0, $30, 0, $78, 0, $CC, 0, 0, 0, 0, 0);
var
  GripR, CloseR: TRect;
  GripSize, Z: Integer;
  Vertical: Boolean;
  C1, C2, PatternColor: TColor;
  Flags: Integer;
  Toolbar: TTBCustomDockableWindowAccess;
  State: TSpTBXSkinStatesType;
  Details: TThemedElementDetails;
begin
  Toolbar := TTBCustomDockableWindowAccess(W);

  GripSize := SpGetDragHandleSize(Toolbar);
  if GripSize <= 0 then Exit;

  Vertical := SpIsVerticalToolbar(Toolbar);

  GripR := ARect;
  if Vertical then begin
    GripR.Bottom := GripR.Top + GripSize;
    InflateRect(GripR, -2, 0);
  end
  else begin
    GripR.Right := GripR.Left + GripSize;
    InflateRect(GripR, 0, -2);
  end;

  if Toolbar.DragHandleStyle <> dhNone then begin
    if Toolbar.CloseButtonWhenDocked then begin
      if Vertical then begin
        CloseR.Left := ARect.Right - GripSize;
        CloseR.Right := CloseR.Left + GripSize - 2;
        CloseR.Top := ARect.Top + 2;
        CloseR.Bottom := CloseR.Top + GripSize - 2;
        Dec(GripR.Right, GripSize - 1);
      end
      else begin
        CloseR.Left := ARect.Left + 2;
        CloseR.Right := CloseR.Left + GripSize - 2;
        CloseR.Top := ARect.Top + 2;
        CloseR.Bottom := CloseR.Top + GripSize - 2;
        Inc(GripR.Top, GripSize - 1);
      end;
    end;

    case SkinManager.GetSkinType of
      sknNone:
        begin
          OffsetRect(CloseR, -1, -1);
          if Vertical then begin
            if Toolbar.CloseButtonWhenDocked then
              if Toolbar.DragHandleStyle = dhDouble then Inc(GripR.Top, 1)
              else Inc(GripR.Top, 3);
            Inc(GripR.Top, 3);
            GripR.Bottom := GripR.Top + 3;
          end
          else begin
            if Toolbar.CloseButtonWhenDocked then
              if Toolbar.DragHandleStyle = dhDouble then Inc(GripR.Left, 1)
              else Inc(GripR.Left, 3);
            Inc(GripR.Left, 3);
            GripR.Right := GripR.Left + 3;
          end;
          Windows.DrawEdge(ACanvas.Handle, GripR, BDR_RAISEDINNER, BF_RECT);
          ACanvas.Pixels[GripR.Left, GripR.Bottom - 1] := clBtnHighlight;
          if Toolbar.DragHandleStyle = dhDouble then begin
            if Vertical then OffsetRect(GripR, 0, 3)
            else OffsetRect(GripR, 3, 0);
            Windows.DrawEdge(ACanvas.Handle, GripR, BDR_RAISEDINNER, BF_RECT);
            ACanvas.Pixels[GripR.Left, GripR.Bottom - 1] := clBtnHighlight;
          end;

          // Close button
          if Toolbar.CloseButtonWhenDocked then begin
            if Toolbar.CloseButtonDown then
              Windows.DrawEdge(ACanvas.Handle, CloseR, BDR_SUNKENOUTER, BF_RECT)
            else
              if Toolbar. CloseButtonHover then
                Windows.DrawEdge(ACanvas.Handle, CloseR, BDR_RAISEDINNER, BF_RECT);
            if Toolbar.CloseButtonDown then OffsetRect(CloseR, 1, 1);
            SpDrawGlyphPattern(ACanvas.Handle, CloseR, 7, 7, Pattern[0], clBtnText);
          end;
        end;
      sknWindows, sknDelphiStyle:
        if SkinManager.GetSkinType = sknDelphiStyle then begin  // XE2 Styles
          if Vertical then begin
            Details := SpTBXThemeServices.GetElementDetails(trGripperVert);
            OffsetRect(GripR, 1, 0);
          end
          else begin
            Details := SpTBXThemeServices.GetElementDetails(trGripper);
            OffsetRect(GripR, 0, 1);
          end;
          SpTBXThemeServices.DrawElement(ACanvas.Handle, Details, GripR);

          // Close button
          if Toolbar.CloseButtonWhenDocked then begin
            CurrentSkin.PaintThemedElementBackground(ACanvas, CloseR, skncToolbarItem, True, Toolbar.CloseButtonDown, Toolbar.CloseButtonHover, False, False, False, False);
            if Toolbar.CloseButtonDown then OffsetRect(CloseR, 1, 1);
            SpDrawGlyphPattern(ACanvas.Handle, CloseR, 7, 7, Pattern[0], CurrentSkin.GetThemedSystemColor(clBtnText));
          end;
        end
        else begin
          // Since GetThemePartSize does not seem to work properly, assume we use default
          // WindowsXP themes where the gripper pattern repeats itself every 4 pixels
          if Vertical then begin
            Details := SpTBXThemeServices.GetElementDetails(trGripperVert);
            OffsetRect(GripR, -1, 0);
            GripR := SpCenterRectVert(GripR, 6);
            Z := GripR.Right - GripR.Left;
            GripR.Left := GripR.Left - 1 + (Z and $3) shr 1;
            GripR.Right := GripR.Left + Z and not $3 + 2;
          end
          else begin
            Details := SpTBXThemeServices.GetElementDetails(trGripper);
            OffsetRect(GripR, 0, -1);
            GripR := SpCenterRectHoriz(GripR, 6);
            Z := GripR.Bottom - GripR.Top;
            GripR.Top := GripR.Top  - 1 + (Z and $3) shr 1;
            GripR.Bottom := GripR.Top + Z and not $3 + 1;
          end;
          SpTBXThemeServices.DrawElement(ACanvas.Handle, Details, GripR);

          // Close button
          if Toolbar.CloseButtonWhenDocked then begin
            Flags := TS_NORMAL;
            if Toolbar.CloseButtonDown then Flags := TS_PRESSED
            else if Toolbar.CloseButtonHover then Flags := TS_HOT;
            DrawThemeBackground(SpTBXThemeServices.Theme[teToolbar], ACanvas.Handle, TP_BUTTON, Flags, CloseR, nil);
            if Toolbar.CloseButtonDown then OffsetRect(CloseR, 1, 1);
            SpDrawGlyphPattern(ACanvas.Handle, CloseR, 7, 7, Pattern[0], clBtnText);
          end;
        end;
      sknSkin:
        begin
          if Vertical then begin
            InflateRect(GripR, -3, 0);
            OffsetRect(GripR, 0, 2);
            GripR := SpCenterRectVert(GripR, 4);
          end
          else begin
            InflateRect(GripR, 0, -3);
            OffsetRect(GripR, 2, 0);
            GripR := SpCenterRectHoriz(GripR, 4);
          end;
          C1 := SkinManager.CurrentSkin.Options(skncToolbarGrip).Body.Color1;
          C2 := SkinManager.CurrentSkin.Options(skncToolbarGrip).Body.Color2;
          SpDrawXPGrip(ACanvas, GripR, C1, C2);

          // Close button
          if Toolbar.CloseButtonWhenDocked then begin
            State := sknsNormal;
            if Toolbar.CloseButtonDown then State := sknsPushed
            else if Toolbar.CloseButtonHover then State := sknsHotTrack;
            CurrentSkin.PaintBackground(ACanvas, CloseR, skncToolbarItem, State, True, True);
            if Toolbar.CloseButtonDown then OffsetRect(CloseR, 1, 1);
            PatternColor := CurrentSkin.GetTextColor(skncToolbarItem, State);
            SpDrawGlyphPattern(ACanvas.Handle, CloseR, 7, 7, Pattern[0], PatternColor);
          end;
        end;
    end;
  end;
end;

procedure SpDrawXPTooltipBackground(ACanvas: TCanvas; ARect: TRect);
var
  ClipRect: TRect;
  Details: TThemedElementDetails;
  {$IF CompilerVersion >= 23} //for Delphi XE2 and up
  C, CGradientStart, CGradientEnd: TColor;
  {$IFEND}
begin
  if SkinManager.GetSkinType = sknDelphiStyle then begin
    // ThemeServices.DrawElement doesn't paint the tooltip background when
    // using Delphi Custom Styles.
    // We need to manually paint the gradients.
    // Taken from THintWindow.Paint:
    {$IF CompilerVersion >= 23} //for Delphi XE2 and up
    Details := SpTBXThemeServices.GetElementDetails(thHintNormal);
    if SpTBXThemeServices.GetElementColor(Details, ecGradientColor1, C) and (C <> clNone) then CGradientStart := C
    else CGradientStart := clInfoBk;
    if SpTBXThemeServices.GetElementColor(Details, ecGradientColor2, C) and (C <> clNone) then CGradientEnd := C
    else CGradientEnd := clInfoBk;
    SpGradientFill(ACanvas, ARect, CGradientStart, CGradientEnd, True);
    {$IFEND}
  end
  else
    if SpIsWinVistaOrUp and (SkinManager.GetSkinType = sknWindows) then begin
      // Paint Vista gradient background if themes enabled
      ClipRect := ARect;
      InflateRect(ARect, 4, 4);
      Details := SpTBXThemeServices.GetElementDetails(tttStandardNormal);
      SpTBXThemeServices.DrawElement(ACanvas.Handle, Details, ARect, @ClipRect);
    end
    else
      ACanvas.FillRect(ARect);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Menu helpers }

function SpCalcPopupPosition(const X, Y, Width, Height: Integer;
  PopupControl: TControl = nil; IsVertical: Boolean = False): TPoint;
var
  R, MonitorR: TRect;
begin
  if Assigned(PopupControl) then begin
    Result := Point(0, 0);
    if PopupControl.Parent = nil then Exit;

    R := PopupControl.BoundsRect;
    R.TopLeft := PopupControl.Parent.ClientToScreen(R.TopLeft);
    R.BottomRight := PopupControl.Parent.ClientToScreen(R.BottomRight);

    if IsVertical then
      Result := Point(R.Right, R.Top)
    else
      Result := Point(R.Left, R.Bottom);

    MonitorR := GetRectOfMonitorContainingPoint(Result, True);

    if IsVertical then begin
      if Result.X + Width > MonitorR.Right then
        Result.X := R.Left - Width;
      if Result.Y + Height > MonitorR.Bottom then
        if R.Bottom > MonitorR.Bottom then
          Result.Y := MonitorR.Bottom - Height
        else
          Result.Y := R.Bottom - Height;
    end
    else begin
      if Result.X + Width > MonitorR.Right then
        if R.Right > MonitorR.Right then
          Result.X := MonitorR.Right - Width
        else
          Result.X := R.Right - Width;
      if Result.Y + Height > MonitorR.Bottom then
        Result.Y := R.Top - Height;
    end;
  end
  else begin
    Result := Point(X, Y);
    MonitorR := GetRectOfMonitorContainingPoint(Result, True);
    if X + Width > MonitorR.Right then
      Result.X := X - Width;
    if Y + Height > MonitorR.Bottom then
      Result.Y := Y - Height;
  end;
end;

function SpHMenuGetCaption(Menu: HMenu; Index: Integer): WideString;
var
  WideBuf: array[0..MAX_PATH] of WideChar;
  Size: Integer;
begin
  Result := '';
  // [Bugfix] Windows bug:
  // GetMenuStringW when a DBCS code page is active (e.g. Japanese)
  // the result of the function is incorrect (it returns Size * 2)
  // http://news.jrsoftware.org/read/article.php?id=12268&group=jrsoftware.toolbar2000.thirdparty
  FillChar(WideBuf, MAX_PATH, #0);
  GetMenuStringW(Menu, Index, @WideBuf, MAX_PATH, MF_BYPOSITION);
  Size := lstrlenW(@WideBuf);
  Result := WideBuf;
  SetLength(Result, Size);
end;

function SpHMenuToTBMenuItem(Menu: HMenu; ParentItem: TTBCustomItem): Boolean;
var
  MenuInfo: TMenuItemInfo;
  I, C: Integer;
  Item: TSpTBXItem;
  HasSubMenu: Boolean;
begin
  Result := False;
  if not Assigned(ParentItem) or not IsMenu(Menu) then Exit;

  C := GetMenuItemCount(Menu);

  for I := 0 to C - 1 do begin
    FillChar(MenuInfo, SizeOf(MenuInfo), #0);
    MenuInfo.cbSize := SizeOf(MenuInfo);
    MenuInfo.fMask := MIIM_TYPE or MIIM_STATE or MIIM_ID or MIIM_SUBMENU;
    GetMenuItemInfo(Menu, I, True, MenuInfo);

    if MenuInfo.fType and MFT_SEPARATOR <> 0 then
      ParentItem.Add(TSpTBXSeparatorItem.Create(nil))
    else begin
      HasSubmenu := IsMenu(MenuInfo.hSubMenu);
      if HasSubmenu then
        Item := TSpTBXSubmenuItem.Create(nil)
      else
        Item := TSpTBXItem.Create(nil);

      Item.Caption := SpHMenuGetCaption(Menu, I);
      Item.Tag := MenuInfo.wID;
      if MenuInfo.fState and MFS_DISABLED <> 0 then
        Item.Enabled := False;
      if MenuInfo.fState and MFS_CHECKED <> 0 then
        Item.Checked := True;
      if MenuInfo.fState and MFS_DEFAULT <> 0 then
        Item.Options := Item.Options + [tboDefault];
      ParentItem.Add(Item);
    end;
  end;

  Result := True;
end;

function SpShowSystemPopupMenu(ParentForm: TCustomForm; ScreenPos: TPoint; DoDefault: Boolean = True): Integer;
var
  SysMenu: HMENU;
begin
  ReleaseCapture;
  SysMenu := GetSystemMenu(ParentForm.Handle, False);
  case ParentForm.WindowState of
    wsMaximized:
      begin
        EnableMenuItem(SysMenu, SC_RESTORE, MF_ENABLED);
        EnableMenuItem(SysMenu, SC_MAXIMIZE, MF_GRAYED);
        EnableMenuItem(SysMenu, SC_MOVE, MF_GRAYED);
        EnableMenuItem(SysMenu, SC_SIZE, MF_GRAYED);
      end;
    wsNormal:
      begin
        EnableMenuItem(SysMenu, SC_RESTORE, MF_GRAYED);
        EnableMenuItem(SysMenu, SC_MAXIMIZE, MF_ENABLED);
        EnableMenuItem(SysMenu, SC_MOVE, MF_ENABLED);
        EnableMenuItem(SysMenu, SC_SIZE, MF_ENABLED);
      end;
  end;
  Result := Integer(TrackPopupMenuEx(SysMenu, TPM_LEFTALIGN or TPM_RETURNCMD or
    TPM_RIGHTBUTTON or TPM_HORIZONTAL or TPM_VERTICAL, ScreenPos.X, ScreenPos.Y, ParentForm.Handle, nil));
  if DoDefault then
    case Result of
      SC_MAXIMIZE: ParentForm.WindowState := wsMaximized;
      SC_RESTORE: ParentForm.WindowState := wsNormal;
    else
      // WindowState := wsMinimized will not minimize the app correctly
      SendMessage(ParentForm.Handle, WM_SYSCOMMAND, Result, 0);
    end;
end;

function SpFillSystemSpTBXPopup(ParentForm: TCustomForm; ParentItem: TTBCustomItem;
  ShowSize, ShowMinimize, ShowMaximize, ShowClose: Boolean; ClickEvent: TNotifyEvent = nil): Boolean;
var
  Menu: HMENU;
  I: Integer;
  Item: TTBCustomItem;
begin
  Result := False;
  Menu := GetSystemMenu(ParentForm.Handle, False);

  if SpHMenuToTBMenuItem(Menu, ParentItem) then begin
    for I := 0 to ParentItem.Count - 1 do begin
      Item := ParentItem.Items[I];
      Item.OnClick := ClickEvent;      
      case Item.Tag of
        SC_MINIMIZE:
          begin
            Item.Visible := ShowMinimize;
            Item.Enabled := ParentForm.WindowState <> wsMinimized;
            Item.Images := MDIButtonsImgList;
            Item.ImageIndex := 2;
          end;
        SC_RESTORE:
          begin
            Item.Visible := ShowMaximize;
            Item.Enabled := ParentForm.WindowState <> wsNormal;
            Item.Images := MDIButtonsImgList;
            Item.ImageIndex := 3;
          end;
        SC_MAXIMIZE:
          begin
            Item.Visible := ShowMaximize;
            Item.Enabled := ParentForm.WindowState <> wsMaximized;
            Item.Images := MDIButtonsImgList;
            Item.ImageIndex := 1;
          end;
        SC_CLOSE:
          begin
            Item.Visible := ShowClose;
            Item.Options := Item.Options + [tboDefault];
            Item.Images := MDIButtonsImgList;
            Item.ImageIndex := 0;
          end;
        SC_MOVE:
          begin
            Item.Enabled := ParentForm.WindowState <> wsMaximized;
          end;
        SC_SIZE:
          begin
            Item.Visible := ShowSize;
            Item.Enabled := ParentForm.WindowState <> wsMaximized;            
          end;
      end;
    end;

    Result := True;
  end;
end;

function SpShowSystemSpTBXPopupMenu(ParentForm: TCustomForm; ScreenPos: TPoint;
  ShowSize, ShowMinimize, ShowMaximize, ShowClose: Boolean;
  PopupEvent: TSpTBXPopupEvent; DoDefault: Boolean = True): Integer;
var
  Popup: TSpTBXPopupMenu;
  ClickedItem: TTBCustomItem;
begin
  Result := 0;
  ReleaseCapture;

  Popup := TSpTBXPopupMenu.Create(ParentForm);
  try
    if SpFillSystemSpTBXPopup(ParentForm, Popup.Items, ShowSize, ShowMinimize, ShowMaximize, ShowClose) then begin
      if Assigned(PopupEvent) then
        Popup.OnInitPopup := PopupEvent;
      Popup.PopupComponent := ParentForm;
      ClickedItem := Popup.PopupEx(ScreenPos.X, ScreenPos.Y, nil, True);
      if Assigned(ClickedItem) then begin
        Result := ClickedItem.Tag;
        // If it's not a SystemMenu item fire the OnClick event of the item
        // We can't use PostClick because the Item will be destroyed by the
        // time the message is handled.
        if (Result < SC_SIZE) or (Result > SC_CONTEXTHELP) then
          ClickedItem.Click;
      end;
    end;
  finally
    Popup.Free;
  end;

  if DoDefault and (Result > 0) then
    case Result of
      SC_MAXIMIZE:
        ParentForm.WindowState := wsMaximized;
      SC_RESTORE:
        ParentForm.WindowState := wsNormal;
      SC_SIZE, SC_MOVE, SC_MINIMIZE, SC_CLOSE:
        begin
          // WindowState := wsMinimized will not minimize the app correctly
          SendMessage(ParentForm.Handle, WM_SYSCOMMAND, Result, 0);
        end;
    end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Misc helpers }

procedure SpActivateDwmNC(WinControl: TWinControl; Activate: Boolean);
var
  ncrp: Cardinal;
begin
  // Use the new API on Windows Vista
  if DwmCompositionEnabled and WinControl.HandleAllocated then begin
    if Activate then
      ncrp := DWMNCRP_USEWINDOWSTYLE
    else
      ncrp := DWMNCRP_DISABLED;
    DwmSetWindowAttribute(WinControl.Handle, DWMWA_NCRENDERING_POLICY, @ncrp, SizeOf(ncrp));
  end;
end;

function SpIsDwmCompositionEnabled: Boolean;
begin
  // Use the new API on Windows Vista
  Result := DwmCompositionEnabled;
end;

function SpCanFocus(WinControl: TWinControl): Boolean;
var
  Form: TCustomForm;
begin
  Result := False;
  if Assigned(WinControl) and not WinControl.Focused then begin
    Form := GetParentForm(WinControl);
    if Assigned(Form) and Form.Enabled and Form.Visible then
      Result := WinControl.CanFocus;
  end;
end;

function SpIsFocused(WinControl: TWinControl; out FocusedChild: TWinControl): Boolean;
var
  Form: TCustomForm;
begin
  Result := False;
  FocusedChild := nil;
  if WinControl.Focused then
    Result := True
  else begin
    Form := GetParentForm(WinControl);
    if Assigned(Form) and Form.Enabled and Form.Visible then
      if Assigned(Form.ActiveControl) and Form.ActiveControl.Focused then
        if IsChild(WinControl.Handle, Form.ActiveControl.Handle) then begin
          Result := True;
          FocusedChild := Form.ActiveControl;
        end;
  end;
end;

function SpFocusFirstChild(WinControl: TWinControl): TWinControl;
var
  Form: TCustomForm;
begin
  Result := nil;
  Form := GetParentForm(WinControl);
  if Assigned(Form) and Form.Enabled and Form.Visible then begin
    TWinControlAccess(WinControl).SelectFirst;
    Result := Form.ActiveControl;
  end;
end;

function SpFindControl(Parent: TWinControl; Child: TControl): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Parent.ControlCount - 1 do
    if Parent.Controls[I] = Child then begin
      Result := I;
      Break;
    end;
end;

function SpFindParent(Control: TControl; ParentClass: TClass): TWinControl;
var
  P: TWinControl;
begin
  Result := nil;
  if Assigned(Control) then begin
    P := Control.Parent;
    while Assigned(P) do
      if P is ParentClass then begin
        Result := P;
        Break;
      end
      else
        P := P.Parent;
  end;
end;

function SpHasBorders(WinControl: TWinControl): Boolean;
var
  Style, ExStyle: Integer;
begin
  Result := False;
  Style := GetWindowLong(WinControl.Handle, GWL_STYLE);
  ExStyle := GetWindowLong(WinControl.Handle, GWL_EXSTYLE);

  if (Style and WS_BORDER <> 0) or (ExStyle and WS_EX_CLIENTEDGE <> 0) then
    Result := True;
end;

function SpGetFormWindowState(F: TCustomForm; out RestoreBoundsRect: TRect): TWindowState;
// This method is more accurate than Form.WindowState
var
  P: TWindowPlacement;
begin
  Result := wsNormal;
  RestoreBoundsRect := Rect(0, 0, 0, 0);
  if Assigned(F) and (F.HandleAllocated) then begin
    P.Length := SizeOf(TWindowPlacement);
    if GetWindowPlacement(F.Handle, @P) then begin
      case P.showCmd of
        SW_SHOWMINIMIZED: Result := wsMinimized;
        SW_SHOWMAXIMIZED: Result := wsMaximized;
      end;
      // rcNormalPosition contains the window's coordinates when the window is in the restored position
      with P.rcNormalPosition do
        RestoreBoundsRect := Rect(Left, Top, Right - Left, Bottom - Top);
    end;
  end;
end;

procedure SpSetFormWindowState(F: TCustomForm; WindowState: TWindowState; RestoreBoundsRect: TRect);
// This method is more accurate than Form.WindowState
var
  P: TWindowPlacement;
  R: TRect;
begin
  if Assigned(F) and (F.HandleAllocated) then begin
    P.Length := SizeOf(TWindowPlacement);
    case WindowState of
      wsMinimized: P.showCmd := SW_SHOWMINIMIZED;
      wsMaximized: P.showCmd := SW_SHOWMAXIMIZED;
    else
      P.showCmd := SW_SHOWNORMAL;
    end;
    with RestoreBoundsRect do
      R := Bounds(Left, Top, Right, Bottom);
    // rcNormalPosition contains the window's coordinates when the window is in the restored position
    if not IsRectEmpty(R) then
      P.rcNormalPosition := R;

    SetWindowPlacement(F.Handle, @P);
  end;
end;

function SpGetTaskBar(out State, Edge: Cardinal; out Bounds: TRect): Boolean;
// Returns the TaskBar state and bounds
// State can be: 0, ABS_ALWAYSONTOP, ABS_AUTOHIDE
// Edge can be: ABE_LEFT, ABE_RIGHT, ABE_TOP, ABE_BOTTOM
// ABM_GETSTATE

var
	AppData: TAppBarData;
begin
  Result := False;
  State := 0;
  Edge := 0;
  Bounds := Rect(0, 0, 0, 0);

  // 'Shell_TrayWnd' is the name of the task bar's window
  AppData.Hwnd := FindWindow('Shell_TrayWnd', nil);
  if AppData.Hwnd <> 0 then begin
    AppData.cbSize := SizeOf(TAppBarData);
    if SHAppBarMessage(ABM_GETTASKBARPOS, AppData) <> 0 then begin
      Edge := AppData.uEdge;
      Bounds := AppData.rc;

      AppData.cbSize := SizeOf(TAppBarData);
      State := SHAppBarMessage(ABM_GETSTATE, AppData);

      Result := True;
    end;
  end;
end;

procedure SpRecalcNCArea(WinControl: TWinControl);
begin
  if WinControl.HandleAllocated then
    SetWindowPos(WinControl.Handle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or
      SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Customizer helpers }

procedure SpCustomizeAllToolbars(AParentComponent: TComponent; Reset: Boolean);
var
  I: Integer;
  TB: TSpTBXToolbar;
begin
  if Assigned(AParentComponent) then begin
    for I := 0 to AParentComponent.ComponentCount - 1 do
      if AParentComponent.Components[I] is TSpTBXToolbar then begin
        TB := AParentComponent.Components[I] as TSpTBXToolbar;
        if Reset then
          TB.EndCustomize
        else
          TB.BeginCustomize;
      end;
  end;
end;

procedure SpBeginUpdateAllToolbars(AParentComponent: TComponent);
var
  I: Integer;
  TB: TTBCustomToolbar;
begin
  if Assigned(AParentComponent) then begin
    for I := 0 to AParentComponent.ComponentCount - 1 do
      if AParentComponent.Components[I] is TTBCustomToolbar then begin
        TB := AParentComponent.Components[I] as TTBCustomToolbar;
        TB.BeginUpdate;
      end;
  end;
end;

procedure SpEndUpdateAllToolbars(AParentComponent: TComponent);
var
  I: Integer;
  TB: TTBCustomToolbar;
begin
  if Assigned(AParentComponent) then begin
    for I := 0 to AParentComponent.ComponentCount - 1 do
      if AParentComponent.Components[I] is TTBCustomToolbar then begin
        TB := AParentComponent.Components[I] as TTBCustomToolbar;
        TB.View.UpdatePositions;
        TB.EndUpdate;
        TB.Invalidate;
      end;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXFontSettings }

constructor TSpTBXFontSettings.Create;
begin
  FSize := 100;
  FColor := clNone;
end;

procedure TSpTBXFontSettings.Apply(AFont: TFont);
begin
  AFont.Charset := DEFAULT_CHARSET;
  AFont.Color := FColor;
  if FSize <> 100 then
    AFont.Size := (AFont.Size * FSize + 50) div 100;
  if FName <> '' then
    AFont.Name := Name;
  if FStyle <> [] then
    AFont.Style := FStyle;
end;

procedure TSpTBXFontSettings.Assign(Src: TPersistent);
var
  F: TSpTBXFontSettings;
begin
  if Src is TPersistent then begin
    F := TSpTBXFontSettings(Src);
    FColor := F.Color;
    FName := F.Name;
    FSize := F.Size;
    FStyle := F.Style;
    Modified;
  end
  else
    inherited;
end;

procedure TSpTBXFontSettings.Modified;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TSpTBXFontSettings.SetColor(Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    Modified;
  end;
end;

procedure TSpTBXFontSettings.SetName(const Value: TFontName);
begin
  if FName <> Value then begin
    FName := Value;
    Modified;
  end;
end;

procedure TSpTBXFontSettings.SetSize(Value: TSpTBXFontSize);
begin
  if FSize <> Value then begin
    FSize := Value;
    Modified;
  end;
end;

procedure TSpTBXFontSettings.SetStyle(const Value: TFontStyles);
begin
  if FStyle <> Value then begin
    FStyle := Value;
    Modified;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomDragObject }

constructor TSpTBXCustomDragObject.Create(ASourceControl: TControl; AItem: TTBCustomItem);
begin
  FSourceControl := ASourceControl;
  FSourceItem := AItem;
  FDragCursorAccept := crSpTBXCustomization;
  FDragCursorCancel := crNo;
end;

procedure TSpTBXCustomDragObject.Finished(Target: TObject; X, Y: Integer;
  Accepted: Boolean);
begin
  inherited;
  if not Accepted then begin
    if Assigned(FSourceControl) then
      TControlAccess(FSourceControl).DragCanceled;
    Target := nil;
  end;

  if Assigned(FSourceControl) then
    TControlAccess(FSourceControl).DoEndDrag(Target, X, Y);
end;

function TSpTBXCustomDragObject.GetDragCursor(Accepted: Boolean; X,
  Y: Integer): TCursor;
begin
  if Accepted then Result := FDragCursorAccept
  else Result := FDragCursorCancel;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomItem }

constructor TSpTBXCustomItem.Create(AOwner: TComponent);
begin
  inherited;
  FFontSettings := TSpTBXFontSettings.Create;
  FFontSettings.OnChange := FontSettingsChanged;
  FAlignment := taCenter;
  FCaptionGlowColor := clYellow;
  FCustomWidth := -1;
  FCustomHeight := -1;
  FMargins := 0;
  SetStretch(True);
  FWrapping := twWrap;
end;

destructor TSpTBXCustomItem.Destroy;
begin
  FreeAndNil(FFontSettings);
  inherited;
end;

procedure TSpTBXCustomItem.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Control) then Control := nil;
end;

procedure TSpTBXCustomItem.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  if Action is TCustomAction then
    with TCustomAction(Sender) do begin
      if not CheckDefaults or (Self.Caption = '') then
        Self.Caption := Caption;
      if not CheckDefaults or (Self.Hint = '') then
        Self.Hint := Hint;
    end;
  inherited;
end;

function TSpTBXCustomItem.DialogChar(CharCode: Word): Boolean;
begin
  Result := False;
end;

procedure TSpTBXCustomItem.DoDrawAdjustFont(AFont: TFont; State: TSpTBXSkinStatesType);
begin
  // Do nothing
end;

procedure TSpTBXCustomItem.DoDrawHint(AHintBitmap: TBitmap; var AHint: Widestring; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawHint) then FOnDrawHint(Self, AHintBitmap, AHint, PaintDefault);
end;

procedure TSpTBXCustomItem.DoDrawButton(ACanvas: TCanvas; ARect: TRect;
  ItemInfo: TSpTBXMenuItemInfo; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawItem) then FOnDrawItem(Self, ACanvas, ARect, ItemInfo, PaintStage, PaintDefault);
end;

procedure TSpTBXCustomItem.DoDrawCaption(ACanvas: TCanvas; ClientAreaRect: TRect;
  State: TSpTBXSkinStatesType; var ACaption: WideString; var CaptionRect: TRect; var CaptionFormat: Cardinal;
  IsTextRotated: Boolean; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawCaption) then FOnDrawCaption(Self, ACanvas, ClientAreaRect,
    State, ACaption, CaptionRect, CaptionFormat, IsTextRotated, PaintStage, PaintDefault);
end;

procedure TSpTBXCustomItem.DoDrawImage(ACanvas: TCanvas;
  State: TSpTBXSkinStatesType; const PaintStage: TSpTBXPaintStage;
  var AImageList: TCustomImageList; var AImageIndex: Integer;
  var ARect: TRect; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawImage) then FOnDrawImage(Self, ACanvas, State, PaintStage,
    AImageList, AImageIndex, ARect, PaintDefault);
end;

procedure TSpTBXCustomItem.DoPopupShowingChanged(APopupWindow: TTBPopupWindow; IsVisible: Boolean);
// This method is called by TSpTBXPopupWindow when the popup is Opened and Closed
begin
  if Assigned(APopupWindow) then begin
    if IsVisible then begin
      if Assigned(FOnInitPopup) then FOnInitPopup(Self, APopupWindow.View);
    end
    else begin
      if Assigned(FOnClosePopup) then FOnClosePopup(Self);
    end;
  end;
end;

procedure TSpTBXCustomItem.FontSettingsChanged(Sender: TObject);
begin
  Change(True);
end;

function TSpTBXCustomItem.GetActionLinkClass: TTBCustomItemActionLinkClass;
begin
  Result := TSpTBXCustomItemActionLink;
end;

function TSpTBXCustomItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result := TSpTBXItemViewer;
end;

function TSpTBXCustomItem.GetPopupWindowClass: TTBPopupWindowClass;
begin
  Result := TSpTBXPopupWindow;
end;

function TSpTBXCustomItem.GetShortCutText: WideString;
var
  P: Integer;
begin
  P := Pos(#9, Caption);
  if (P = 0) or (P = Length(Caption)) then begin
    if ShortCut <> 0 then
      Result := ShortCutToText(ShortCut)
    else
      Result := '';
  end
  else
    Result := Copy(Caption, P+1, Maxint);
end;

procedure TSpTBXCustomItem.Click;
begin
  if Assigned(FControl) then ToggleControl;
  inherited;
end;

procedure TSpTBXCustomItem.InitiateAction;
begin
  inherited;
  UpdateProps;
end;

procedure TSpTBXCustomItem.Invalidate;
begin
  Change(False);
end;

procedure TSpTBXCustomItem.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then begin
    FAlignment := Value;
    Change(False);
  end;
end;

procedure TSpTBXCustomItem.SetAnchored(const Value: Boolean);
begin
  if FAnchored <> Value then begin
    FAnchored := Value;
  end;
end;

procedure TSpTBXCustomItem.SetCaptionGlow(const Value: TSpGlowDirection);
begin
  if FCaptionGlow <> Value then begin
    FCaptionGlow := Value;
    Change(False);
  end;
end;

procedure TSpTBXCustomItem.SetCaptionGlowColor(const Value: TColor);
begin
  if FCaptionGlowColor <> Value then begin
    FCaptionGlowColor := Value;
    Change(False);
  end;
end;

procedure TSpTBXCustomItem.SetControl(const Value: TControl);
begin
  if FControl <> Value then
  begin
    FControl := Value;
    if Assigned(Value) then
      Value.FreeNotification(Self);
    UpdateProps;
  end;
end;

procedure TSpTBXCustomItem.SetCustomWidth(Value: Integer);
begin
  if Value < -1 then Value := -1;
  if FCustomWidth <> Value then begin
    FCustomWidth := Value;
    Change(True);
  end;
end;

procedure TSpTBXCustomItem.SetCustomHeight(Value: Integer);
begin
  if Value < -1 then Value := -1;
  if FCustomHeight <> Value then begin
    FCustomHeight := Value;
    Change(True);
  end;
end;

procedure TSpTBXCustomItem.SetFontSettings(const Value: TSpTBXFontSettings);
begin
  FFontSettings.Assign(Value);
end;

procedure TSpTBXCustomItem.SetMargins(Value: Integer);
begin
  if FMargins <> Value then begin
    FMargins := Value;
    Change(True);
  end;
end;

procedure TSpTBXCustomItem.SetMinHeight(const Value: Integer);
begin
  if Value <> FMinHeight then begin
    FMinHeight := Value;
    Change(True);
  end;
end;

procedure TSpTBXCustomItem.SetMinWidth(const Value: Integer);
begin
  if Value <> FMinWidth then begin
    FMinWidth := Value;
    Change(True);
  end;
end;

procedure TSpTBXCustomItem.SetStretch(const Value: Boolean);
begin
  if FStretch <> Value then begin
    FStretch := Value;
    Change(True);
  end;
end;

procedure TSpTBXCustomItem.SetToolBoxPopup(const Value: Boolean);
begin
  FToolBoxPopup := Value;
  if FToolBoxPopup then
    Options := Options + [tboToolbarStyle]
  else
    Options := Options - [tboToolbarStyle];
end;

procedure TSpTBXCustomItem.SetWrapping(const Value: TTextWrapping);
begin
  if FWrapping <> Value then begin
    FWrapping := Value;
    Change(False);
  end;
end;

procedure TSpTBXCustomItem.ToggleControl;
begin
  FControl.Visible := not FControl.Visible;
end;

procedure TSpTBXCustomItem.UpdateProps;
begin
  if Assigned(Control) then
    if (ComponentState * [csDesigning, csLoading, csDestroying] = []) then
      Checked := Control.Visible;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXItemViewer }

function TSpTBXItemViewer.CaptionShown: Boolean;
var
  T: TSpTBXToolbar;
begin
  Result := inherited CaptionShown;

  if Assigned(View) and Assigned(View.Owner) and (View.Owner is TSpTBXToolbar) then begin
    T := View.Owner as TSpTBXToolbar;
    case T.DisplayMode of
      tbdmImageOnly:
        if GetImageShown then Result := False;
      tbdmTextOnly:
        Result := True;
    end;
  end;
end;

function TSpTBXItemViewer.GetImageShown: Boolean;
begin
  Result := (Item.ImageIndex >= 0) and
    ((Item.DisplayMode in [nbdmDefault, nbdmImageAndText]) or
    (IsToolbarStyle and (Item.DisplayMode = nbdmTextOnlyInMenus)));

  if Assigned(View) and Assigned(View.Owner) and (View.Owner is TSpTBXToolbar) then
    if TSpTBXToolbar(View.Owner).DisplayMode = tbdmTextOnly then
      Result := False;
end;

function TSpTBXItemViewer.GetImageSize: TSize;
var
  IL: TCustomImageList;
begin
  IL := GetImageList;
  if Assigned(IL) then begin
    Result.cx := IL.Width;
    Result.cy := IL.Height;
  end
  else begin
    Result.cx := 0;
    Result.cy := 0;
  end;
end;

function TSpTBXItemViewer.GetRightImageSize: TSize;
begin
  Result.cx := 0;
  Result.cy := 0;
end;

procedure TSpTBXItemViewer.DoDrawButton(ACanvas: TCanvas; ARect: TRect;
  ItemInfo: TSpTBXMenuItemInfo; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  Item.DoDrawButton(ACanvas, ARect, ItemInfo, PaintStage, PaintDefault);
end;

procedure TSpTBXItemViewer.DoDrawCaption(ACanvas: TCanvas; ClientAreaRect: TRect;
  State: TSpTBXSkinStatesType; var ACaption: WideString; var CaptionRect: TRect; var CaptionFormat: Cardinal;
  IsTextRotated: Boolean; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  Item.DoDrawCaption(ACanvas, ClientAreaRect, State, ACaption, CaptionRect,
    CaptionFormat, IsTextRotated, PaintStage, PaintDefault);
end;

procedure TSpTBXItemViewer.DoDrawHint(AHintBitmap: TBitmap; CursorPos: TPoint;
  var CursorRect: TRect; var AHint: Widestring; var PaintDefault: Boolean);
begin
  Item.DoDrawHint(AHintBitmap, AHint, PaintDefault);
end;

procedure TSpTBXItemViewer.DoDrawImage(ACanvas: TCanvas;
  State: TSpTBXSkinStatesType; const PaintStage: TSpTBXPaintStage;
  var AImageList: TCustomImageList; var AImageIndex: Integer;
  var ARect: TRect; var PaintDefault: Boolean);
begin
  Item.DoDrawImage(ACanvas, State, PaintStage, AImageList, AImageIndex, ARect, PaintDefault);
end;

procedure TSpTBXItemViewer.DoDrawAdjustFont(AFont: TFont; State: TSpTBXSkinStatesType);
begin
  Item.FontSettings.Apply(AFont);

  if tboDefault in Item.EffectiveOptions then
    AFont.Style := AFont.Style + [fsBold];
  
  if AFont.Color = clNone then
    AFont.Color := GetTextColor(State);

  Item.DoDrawAdjustFont(AFont, State);
end;

procedure TSpTBXItemViewer.DrawItemImage(ACanvas: TCanvas; ARect: TRect;
  ItemInfo: TSpTBXMenuItemInfo; ImgIndex: Integer);
var
  PaintDefault: Boolean;
  ImgList: TCustomImageList;
  PatternColor: TColor;
begin
  ImgList := GetImageList;

  PaintDefault := True;
  DoDrawImage(ACanvas, ItemInfo.State, pstPrePaint, ImgList, ImgIndex, ARect, PaintDefault);
  if PaintDefault and Assigned(ImgList) then
    if ImgList = MDIButtonsImgList then begin
      if (Item.Enabled) and not IsToolbarStyle and (SkinManager.GetSkinType <> sknSkin) then
        PatternColor := clMenuText
      else
        PatternColor := GetTextColor(ItemInfo.State);
      SpDrawGlyphPattern(ACanvas, ARect, ImgIndex, PatternColor);
    end
    else
      if (ImgIndex >= 0) and (ImgIndex < ImgList.Count) then
        SpDrawXPMenuItemImage(ACanvas, ARect, ItemInfo, ImgList, ImgIndex);
  PaintDefault := True;
  DoDrawImage(ACanvas, ItemInfo.State, pstPostPaint, ImgList, ImgIndex, ARect, PaintDefault);
end;

procedure TSpTBXItemViewer.DrawItemRightImage(ACanvas: TCanvas; ARect: TRect;
  ItemInfo: TSpTBXMenuItemInfo);
begin
  // Do nothing
end;

procedure TSpTBXItemViewer.InternalCalcSize(const Canvas: TCanvas;
  CalcStretch: Boolean; var AWidth, AHeight: Integer);
var
  I, W, H: Integer;
  DropDownArrowSize, DropDownArrowMargin, SplitBtnArrowSize: Integer;
  ImgSize, RightImgSize: TSize;
  GlyphTop, ToolbarStyle: Boolean;
  WS: WideString;
  TextMetric: TTextMetric;
  MenuMargins: TSpTBXMenuItemMarginsInfo;
  State: TSpTBXSkinStatesType;
  IsHoverItem, IsOpen, IsPushed: Boolean;
  TextInfo: TSpTBXTextInfo;
  TB: TSpTBXToolbar;
begin
  CurrentSkin.GetDropDownArrowSize(DropDownArrowSize, DropDownArrowMargin, SplitBtnArrowSize);

  ToolbarStyle := IsToolbarStyle;

  ImgSize := GetImageSize;
  if (ImgSize.CX <= 0) or (ImgSize.CY <= 0) then begin
    ImgSize.CX := 0;
    ImgSize.CY := 0;
  end;
  RightImgSize := GetRightImageSize;
  if (RightImgSize.CX <= 0) or (RightImgSize.CY <= 0) then begin
    RightImgSize.CX := 0;
    RightImgSize.CY := 0;
  end;

  GlyphTop := False;
  if tboImageAboveCaption in Item.EffectiveOptions then
    GlyphTop := True;

  // Setup font and get the text info
  IsOpen := Self = View.OpenViewer;
  IsHoverItem := Self = View.Selected;
  IsPushed := IsHoverItem and (IsOpen or (View.MouseOverSelected and View.Capture));
  State := CurrentSkin.GetState(Item.Enabled, IsPushed, IsHoverItem, Item.Checked);
  GetTextInfo(Canvas, State, TextInfo);

  // Measure size
  if ToolbarStyle then begin
    AWidth := 6;
    AHeight := 6;

    if CaptionShown then begin
      Inc(AWidth, TextInfo.TextSize.CX);
      Inc(AHeight, TextInfo.TextSize.CY);
      if not TextInfo.IsTextRotated then Inc(AWidth, 4)
      else Inc(AHeight, 4);
    end;

    if GetImageShown and (ImgSize.CX > 0) and (ImgSize.CY > 0) then begin
      if not GlyphTop then begin
        if not TextInfo.IsTextRotated then begin
          Inc(AWidth, ImgSize.CX);
          Inc(AWidth);
          if AHeight < ImgSize.CY + 6 then AHeight := ImgSize.CY + 6;
        end
        else begin
          Inc(AHeight, ImgSize.CY);
          Inc(AHeight);
          if AWidth < ImgSize.CX + 6 then AWidth := ImgSize.CX + 6;
        end;
      end
      else begin
        Inc(AHeight, ImgSize.CY);
        if AWidth < ImgSize.CX + 7 then AWidth := ImgSize.CX + 7;
      end;
    end;

    if (RightImgSize.cx > 0) and (RightImgSize.cy > 0) then begin
      if View.Orientation = tbvoVertical then
        Inc(AHeight, 4 + RightImgSize.cy)
      else
        Inc(AWidth, 4 + RightImgSize.cx);
    end;

    if (tbisSubmenu in Item.ItemStyle) and (tbisCombo in Item.ItemStyle) then
      Inc(AWidth, SplitBtnArrowSize)
    else begin
      if tboDropdownArrow in Item.Options then
        if not GlyphTop or (ImgSize.CX = 0) or TextInfo.IsTextRotated then begin
          if View.Orientation = tbvoVertical then Inc(AHeight, DropDownArrowSize)
          else Inc(AWidth, DropDownArrowSize);
        end
        else
          if GlyphTop and (TextInfo.IsTextRotated xor (View.Orientation <> tbvoVertical)) then begin
            W := ImgSize.CX + DropDownArrowSize + 2;
            if W > AWidth - 7 then AWidth := W + 7;
          end
          else begin
            H := ImgSize.CY + DropDownArrowSize + 2;
            if H > AHeight - 7 then AHeight := H + 7;
          end;
    end;

    // Widen MenuBar SubMenuItems
    if (tbisSubmenu in Item.ItemStyle) and (vsMenuBar in View.Style) then
      Inc(AWidth, 6);

    // Toolbar.Stretch property doesn't work correctly, I don't know how to fix
    // it without changing the TB2K source.
    // http://news.jrsoftware.org/read/article.php?id=8176&group=jrsoftware.toolbar2000#8176
    if CalcStretch and Item.Stretch and (View is TSpTBXToolbarView) and (View.ViewerCount > 2) then begin
      if View.Orientation = tbvoVertical then begin
        if AWidth < TSpTBXToolbarView(View).FTallestItemSize then AWidth := TSpTBXToolbarView(View).FTallestItemSize;
      end
      else
        if AHeight < TSpTBXToolbarView(View).FTallestItemSize then AHeight := TSpTBXToolbarView(View).FTallestItemSize;
    end;
  end
  else begin // Menu Item
    GetTextMetrics(Canvas.Handle, TextMetric);
    Inc(TextInfo.TextSize.cy, TextMetric.tmExternalLeading);

    AWidth := TextInfo.TextSize.cx;
    AHeight := TextInfo.TextSize.cy;

    if ImgSize.cy = 0 then ImgSize.cy := 16;
    if AHeight < ImgSize.cy then AHeight := ImgSize.cy;

    if View.Window is TSpTBXPopupWindow then
      CurrentSkin.GetMenuItemMargins(Canvas, TSpTBXPopupWindow(View.Window).MaximumImageSize.cx, MenuMargins)
    else
      CurrentSkin.GetMenuItemMargins(Canvas, ImgSize.cx, MenuMargins);

    Inc(AWidth, MenuMargins.Margins.Left + MenuMargins.Margins.Right);
    Inc(AHeight, MenuMargins.Margins.Top + MenuMargins.Margins.Bottom);

    Inc(AWidth, MenuMargins.GutterSize + MenuMargins.ImageTextSpace + MenuMargins.LeftCaptionMargin + MenuMargins.RightCaptionMargin);
    WS := Item.GetShortCutText;
    if Length(WS) > 0 then
      Inc(AWidth, (AHeight - 6) + SpGetTextSize(Canvas.Handle, WS, True).cx);
    Inc(AWidth, AHeight); { Note: maybe this should be controlled by the theme }
  end;

  if AWidth < Item.MinWidth then AWidth := Item.MinWidth;
  if AHeight < Item.MinHeight then AHeight := Item.MinHeight;

  // Handle Custom size and anchors
  if IsRotated then begin
    // Reverse
    H := AWidth + Item.Margins;
    W := AHeight;
  end
  else begin
    W := AWidth + Item.Margins;
    H := AHeight;
  end;

  if Item.CustomWidth > -1 then
    W := Item.CustomWidth;
  if Item.CustomHeight > -1 then
    H := Item.CustomHeight;

  if IsToolbarStyle and Item.Anchored then
    W := W + FAnchorDelta;
  if W < Item.MinWidth then W := Item.MinWidth;
  if H < Item.MinHeight then H := Item.MinHeight;
  // Apply View.MaxSize to the height of the item
  if View.Window is TSpTBXToolbar then begin
    TB := View.Window as TSpTBXToolbar;
    I := TB.MaxSize - TB.NonClientHeight;
    if (I > -1) and (H > I) then
      H := I;
  end;

  if IsRotated then begin
    // Reverse
    AWidth := H;
    AHeight := W;
  end
  else begin
    AWidth := W;
    AHeight := H;
  end;
end;

procedure TSpTBXItemViewer.InternalMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  // Do nothing
end;

function TSpTBXItemViewer.IsOnToolBoxPopup: Boolean;
// Returns True if the item is on a submenu with ToolBoxPopup set to True.
begin
  Result := False;
  if Assigned(View) and Assigned(View.ParentItem) then
    if View.ParentItem is TSpTBXCustomItem then
      Result := TSpTBXCustomItem(View.ParentItem).ToolBoxPopup
    else
      if View.ParentItem is TSpTBXRootItem then
        Result := TSpTBXRootItem(View.ParentItem).ToolBoxPopup;
end;

function TSpTBXItemViewer.IsToolbarStyle: Boolean;
// Returns True if the item is on a toolbar or has tboToolbarStyle.
// We should return False if the item is a ToolBoxPopup and the parent doesn't have tboToolbarStyle,
// the main ToolBoxPopup should be painted as a menu item on submenus/popups.
begin
  Result := inherited IsToolbarStyle;
  // if ToolBoxPopup and tboToolbarStyle is set see if the item is the parent
  // ToolBox submenu.
  if Result and Item.ToolBoxPopup and not View.IsToolbar and Assigned(View) and Assigned(View.ParentItem) then
    if not (tboToolbarStyle in View.ParentItem.EffectiveOptions) then begin
      // The parent item doesn't have tboToolbarStyle, so the current item
      // is the main ToolBoxPopup submenu.
      Result := False;
    end;
end;

procedure TSpTBXItemViewer.CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer);
begin
  InternalCalcSize(Canvas, True, AWidth, AHeight);
end;

function TSpTBXItemViewer.GetCaptionText: WideString;
begin
  Result := SpStripShortcut(Item.Caption);
end;

function TSpTBXItemViewer.GetTextColor(State: TSpTBXSkinStatesType): TColor;
begin
  Result := Item.FontSettings.Color;
  if Result = clNone then begin
    if IsToolbarStyle then begin
      if View.Window is TSpTBXToolbar then
        Result := TSpTBXToolbar(View.Window).GetItemsTextColor(State);
      if Result = clNone then
        Result := CurrentSkin.GetTextColor(skncToolbarItem, State);
    end
    else
      Result := CurrentSkin.GetTextColor(skncMenuItem, State);
  end;
end;

procedure TSpTBXItemViewer.GetTextInfo(ACanvas: TCanvas; State: TSpTBXSkinStatesType; out TextInfo: TSpTBXTextInfo);
var
  ToolbarStyle: Boolean;
  I: Integer;
const
  WordWraps: array [TTextWrapping] of Cardinal = (0,
    DT_SINGLELINE or DT_END_ELLIPSIS,
    DT_SINGLELINE or DT_PATH_ELLIPSIS, DT_WORDBREAK);

  function GetRealTextSize(TextFlags: Cardinal): TSize;
  var
    R, CaptionRect: TRect;
  begin
    TextFlags := TextFlags and not DT_SINGLELINE;
    TextFlags := TextFlags and not (DT_WORDBREAK or DT_END_ELLIPSIS or DT_PATH_ELLIPSIS);
    if (TextFlags and (DT_WORDBREAK or DT_END_ELLIPSIS or DT_PATH_ELLIPSIS)) <> 0 then begin
      // will never get here, TextFlags doesn't have wrapping
      CaptionRect := BoundsRect;
      R := Rect(0, 0, CaptionRect.Right - CaptionRect.Left, 80);
    end
    else
      R := Rect(0, 0, 1, 1);
    SpDrawXPText(ACanvas, TextInfo.Text, R, TextFlags or DT_CALCRECT, gldNone, clYellow, TextInfo.TextAngle);
    Result.CX := R.Right;
    Result.CY := R.Bottom;
  end;

begin
  ToolbarStyle := IsToolbarStyle;
  FillChar(TextInfo, SizeOf(TextInfo), 0);

  // Setup Font
  ACanvas.Font.Assign(View.GetFont);
  DoDrawAdjustFont(ACanvas.Font, State);  // Let the Item adjust the font

  // Text Flags
  TextInfo.TextFlags := 0;
  if not AreKeyboardCuesEnabled and (vsUseHiddenAccels in View.Style) and
    not (vsShowAccels in View.State) then TextInfo.TextFlags := DT_HIDEPREFIX;
  TextInfo.TextFlags := TextInfo.TextFlags or DT_VCENTER or WordWraps[Item.Wrapping];

  TextInfo.IsCaptionShown := CaptionShown;
  if TextInfo.IsCaptionShown then begin
    TextInfo.Text := GetCaptionText;

    TextInfo.IsTextRotated := IsRotated;
    if TextInfo.IsTextRotated or not ToolbarStyle then
      TextInfo.TextFlags := TextInfo.TextFlags or DT_SINGLELINE;

    TextInfo.TextSize := GetRealTextSize(TextInfo.TextFlags);
    if TextInfo.IsTextRotated then begin
      I := TextInfo.TextSize.cx;
      TextInfo.TextSize.cx := TextInfo.TextSize.cy;
      TextInfo.TextSize.cy := I;
    end;
  end
  else begin
    TextInfo.Text := '';
    TextInfo.IsTextRotated := False;
    TextInfo.TextSize.cx := 0;
    TextInfo.TextSize.cy := 0;
  end;

  if TextInfo.IsTextRotated then
    TextInfo.TextAngle := tra270
  else
    TextInfo.TextAngle := tra0;
end;

procedure TSpTBXItemViewer.Paint(const Canvas: TCanvas; const ClientAreaRect: TRect;
  IsSelected, IsPushed, UseDisabledShadow: Boolean);
var
  View: TTBViewAccess;
  ItemInfo: TSpTBXMenuItemInfo;
  TextInfo: TSpTBXTextInfo;
  TextAlignment: TAlignment;
  TextMetrics: TTextMetric;
  PaintDefault, IsSpecialDropDown: Boolean;

  R, CaptionRect, ImageRect, RightImageRect: TRect;
  P: TPoint;

  DropDownArrowSize, DropDownArrowMargin, SplitBtnArrowSize, ImgAndArrowWidth: Integer;
  WS: WideString;
  TextC, DropDownC: TColor;

  GlyphLayout: TSpGlyphLayout;
const
  WordWraps: array [TTextWrapping] of Cardinal = (0,
    DT_SINGLELINE or DT_END_ELLIPSIS,
    DT_SINGLELINE or DT_PATH_ELLIPSIS, DT_WORDBREAK);
begin
  CaptionRect := Rect(0, 0, 0, 0);
  ImageRect := Rect(0, 0, 0, 0);
  RightImageRect := Rect(0, 0, 0, 0);
  CurrentSkin.GetDropDownArrowSize(DropDownArrowSize, DropDownArrowMargin, SplitBtnArrowSize);

  View := TTBViewAccess(Self.View);
  SpFillItemInfo(Canvas, Self, ItemInfo);

  GlyphLayout := ghlGlyphLeft;
  if tboImageAboveCaption in Item.EffectiveOptions then GlyphLayout := ghlGlyphTop;

  { Setup font and get the text info }
  GetTextInfo(Canvas, ItemInfo.State, TextInfo);
  TextC := Canvas.Font.Color;
  TextAlignment := Item.Alignment;

  // Special DropDown, toolbar item with arrow, image and text. The Image is above the caption
  // the arrow must be aligned with the image, above the text
  IsSpecialDropDown := ItemInfo.HasArrow and not ItemInfo.IsSplit and ItemInfo.ToolbarStyle and
    (tboImageAboveCaption in Item.EffectiveOptions) and
    (ItemInfo.ImageSize.cx > 0) and not (TextInfo.IsTextRotated) and (Length(Item.Caption) > 0);

  { Border & Arrows }
  R := ClientAreaRect;
  if ItemInfo.ToolbarStyle then begin
    if ItemInfo.HasArrow then begin
      if ItemInfo.IsSplit then begin
        ItemInfo.ComboRect := R;
        Dec(R.Right, SplitBtnArrowSize);
        ItemInfo.ComboRect.Left := R.Right;
      end
      else
        if not IsSpecialDropDown then begin
          if View.Orientation <> tbvoVertical then
            ItemInfo.ComboRect := Rect(R.Right - DropDownArrowSize - DropDownArrowMargin, 0,
              R.Right - DropDownArrowMargin, R.Bottom)
          else
            ItemInfo.ComboRect := Rect(0, R.Bottom - DropDownArrowSize - DropDownArrowMargin,
              R.Right, R.Bottom - DropDownArrowMargin);
        end
        else begin
          // Special DropDown, toolbar item with arrow, image and text. The Image is above the caption
          // the arrow must be aligned with the image, above the text
          ImgAndArrowWidth := ItemInfo.ImageSize.cx + DropDownArrowSize + 2;
          ItemInfo.ComboRect.Right := (R.Left + R.Right + ImgAndArrowWidth + 2) div 2;
          ItemInfo.ComboRect.Left := ItemInfo.ComboRect.Right - DropDownArrowSize;
          ItemInfo.ComboRect.Top := (R.Top + R.Bottom - ItemInfo.ImageSize.cy - 2 - TextInfo.TextSize.CY) div 2;
          ItemInfo.ComboRect.Bottom := ItemInfo.ComboRect.Top + ItemInfo.ImageSize.cy;
        end;
    end;

    PaintDefault := True;
    DoDrawButton(Canvas, R, ItemInfo, pstPrePaint, PaintDefault);
    if PaintDefault then
      SpDrawXPMenuItem(Canvas, R, ItemInfo);
    PaintDefault := True;
    DoDrawButton(Canvas, R, ItemInfo, pstPostPaint, PaintDefault);

    // Draw dropdown arrow
    if PaintDefault and ItemInfo.HasArrow then begin
      P.X := (ItemInfo.ComboRect.Left + ItemInfo.ComboRect.Right) div 2 - 1;
      P.Y := (ItemInfo.ComboRect.Top + ItemInfo.ComboRect.Bottom) div 2 - 1;
      // Don't draw the arrow if is a split button in Windows, it's
      // painted by the Windows theme.
      if not (ItemInfo.IsSplit and (ItemInfo.SkinType in [sknWindows, sknDelphiStyle])) then begin
        DropDownC := TextC;
        if ItemInfo.IsSplit and ItemInfo.Enabled then
          DropDownC := GetTextColor(ItemInfo.ComboState);
        if ItemInfo.IsSunkenCaption then
          P := Point(P.X + 1, P.Y + 1);
        SpDrawArrow(Canvas, P.X, P.Y, DropDownC, not ItemInfo.IsVertical, False, 2);
      end;
      if not ItemInfo.IsSplit and not IsSpecialDropDown then begin
        if View.Orientation <> tbvoVertical then Dec(R.Right, DropDownArrowSize)
        else Dec(R.Bottom, DropDownArrowSize);
      end;
    end;

    InflateRect(R, -4, -4);  // Adjust
  end
  else begin // Menu items
    PaintDefault := True;
    DoDrawButton(Canvas, R, ItemInfo, pstPrePaint, PaintDefault);
    if PaintDefault then
      SpDrawXPMenuItem(Canvas, R, ItemInfo);
    PaintDefault := True;
    DoDrawButton(Canvas, R, ItemInfo, pstPostPaint, PaintDefault);

    // Draw the submenu arrows
    if PaintDefault and (tbisSubmenu in Item.ItemStyle) then
      SpDrawArrow(Canvas, R.Right - 10, R.Bottom div 2, TextC, False, False, 3);

    // Don't apply the margins if the menu item has
    // tbisClicksTransparent itemstyle (like a SpTBXLabelItem)
    // the caption will be automatically centered.
    if not (tbisClicksTransparent in Item.ItemStyle) then begin
      Inc(R.Left, ItemInfo.MenuMargins.Margins.Left);
      Dec(R.Right, ItemInfo.MenuMargins.Margins.Right);
      Inc(R.Top, ItemInfo.MenuMargins.Margins.Top);
      Dec(R.Bottom, ItemInfo.MenuMargins.Margins.Bottom);
    end;
  end;

  { Caption }
  if TextInfo.IsCaptionShown then
  begin
    WS := GetCaptionText;

    if ItemInfo.ToolbarStyle then begin
      TextInfo.TextFlags := TextInfo.TextFlags and not DT_VCENTER;
      // When ItemInfo.RightImageSize is valid use taLeftJustify
      if (ItemInfo.RightImageSize.cx > 0) and (ItemInfo.RightImageSize.cy > 0) then
        TextAlignment := taLeftJustify;
      case TextAlignment of
        taCenter:
          if GlyphLayout = ghlGlyphTop then TextInfo.TextFlags := TextInfo.TextFlags or DT_CENTER;
        taRightJustify:
          TextInfo.TextFlags := TextInfo.TextFlags or DT_RIGHT;
      end;
      SpCalcXPText(Canvas, R, WS, TextAlignment, TextInfo.TextFlags, ItemInfo.ImageSize, ItemInfo.RightImageSize, GlyphLayout, False, CaptionRect, ImageRect, RightImageRect, TextInfo.TextAngle);

      if ItemInfo.IsSunkenCaption then
        OffsetRect(CaptionRect, 1, 1);
    end
    else begin
      if tbisClicksTransparent in Item.ItemStyle then begin
        // The caption should be centered on the menu popup if the item has
        // tbisClicksTransparent itemstyle (SpTBXLabelItem)
        TextInfo.TextFlags := TextInfo.TextFlags or DT_CENTER;
        CaptionRect := R;
      end
      else begin
        TextInfo.TextFlags := TextInfo.TextFlags or DT_LEFT or DT_VCENTER;
        GetTextMetrics(Canvas.Handle, TextMetrics);
        CaptionRect := R;
        Inc(CaptionRect.Left, ItemInfo.MenuMargins.GutterSize + ItemInfo.MenuMargins.ImageTextSpace + ItemInfo.MenuMargins.LeftCaptionMargin);
        if (CaptionRect.Bottom - CaptionRect.Top) - (TextMetrics.tmHeight + TextMetrics.tmExternalLeading) = ItemInfo.MenuMargins.Margins.Bottom then
          Dec(CaptionRect.Bottom);
        Inc(CaptionRect.Top, TextMetrics.tmExternalLeading);
        CaptionRect.Right := CaptionRect.Left + TextInfo.TextSize.CX;
      end;
    end;

    Canvas.Font.Color := TextC;
    PaintDefault := True;
    DoDrawCaption(Canvas, ClientAreaRect, ItemInfo.State, WS, CaptionRect, TextInfo.TextFlags, TextInfo.IsTextRotated, pstPrePaint, PaintDefault);
    if PaintDefault then begin
      if (not IsSelected or ItemInfo.ToolbarStyle) and
        (ItemInfo.State = sknsDisabled) and (ItemInfo.SkinType = sknNone) then
      begin
        OffsetRect(CaptionRect, 1, 1);
        Canvas.Font.Color := clBtnHighlight;
        SpDrawXPText(Canvas, WS, CaptionRect, TextInfo.TextFlags, Item.CaptionGlow, Item.CaptionGlowColor, TextInfo.TextAngle);
        OffsetRect(CaptionRect, -1, -1);
        Canvas.Font.Color := clGrayText;
      end;

      if SpIsGlassPainting(View.Window) then
        SpDrawXPGlassText(Canvas, WS, CaptionRect, TextInfo.TextFlags, 0)
      else
        SpDrawXPText(Canvas, WS, CaptionRect, TextInfo.TextFlags, Item.CaptionGlow, Item.CaptionGlowColor, TextInfo.TextAngle);
    end;
    PaintDefault := True;
    DoDrawCaption(Canvas, ClientAreaRect, ItemInfo.State, WS, CaptionRect, TextInfo.TextFlags, TextInfo.IsTextRotated, pstPostPaint, PaintDefault);
  end;

  { Shortcut }
  if not ItemInfo.ToolbarStyle then
  begin
    WS := Item.GetShortCutText;
    if Length(WS) > 0 then
    begin
      CaptionRect := R;
      CaptionRect.Left := CaptionRect.Right - (CaptionRect.Bottom - CaptionRect.Top) - SpGetTextSize(Canvas.Handle, WS, True).cx;
      if (CaptionRect.Bottom - CaptionRect.Top) - (TextMetrics.tmHeight + TextMetrics.tmExternalLeading) = ItemInfo.MenuMargins.Margins.Bottom then
        Dec(CaptionRect.Bottom);
      Inc(CaptionRect.Top, TextMetrics.tmExternalLeading);
      Canvas.Font.Color := TextC;
      PaintDefault := True;
      DoDrawCaption(Canvas, ClientAreaRect, ItemInfo.State, WS, CaptionRect, TextInfo.TextFlags, TextInfo.IsTextRotated, pstPrePaint, PaintDefault);
      if PaintDefault then
        SpDrawXPText(Canvas, WS, CaptionRect, TextInfo.TextFlags, Item.CaptionGlow, Item.CaptionGlowColor, TextInfo.TextAngle);
      PaintDefault := True;
      DoDrawCaption(Canvas, ClientAreaRect, ItemInfo.State, WS, CaptionRect, TextInfo.TextFlags, TextInfo.IsTextRotated, pstPostPaint, PaintDefault);
    end;
  end;

  { Image, or check box }
  if ItemInfo.ImageOrCheckShown then begin
    if ItemInfo.ToolBarStyle then begin
      if IsRectEmpty(ImageRect) then
        ImageRect := R;
      if IsSpecialDropDown then OffsetRect(ImageRect, (-DropDownArrowSize + 1) div 2, 0);
    end
    else begin
      ImageRect := R;
      ImageRect.Right := ImageRect.Left + ItemInfo.MenuMargins.GutterSize;
    end;

    if ItemInfo.ImageShown then begin
      ImageRect := SpCenterRect(ImageRect, ItemInfo.ImageSize.cx, ItemInfo.ImageSize.cy);
      DrawItemImage(Canvas, ImageRect, ItemInfo, Item.ImageIndex);
    end
    else begin
      if not ItemInfo.ToolbarStyle and Item.Checked then begin
        if Item.RadioItem then
          CurrentSkin.PaintMenuRadioMark(Canvas, ImageRect, True, True, ItemInfo.State)
        else
          CurrentSkin.PaintMenuCheckMark(Canvas, ImageRect, True, False, True, ItemInfo.State);
      end;
    end;
  end;

  { Right Image }
  if ItemInfo.ToolbarStyle and (ItemInfo.RightImageSize.cx > 0) and (ItemInfo.RightImageSize.cy > 0) then begin
    if IsRectEmpty(RightImageRect) then begin
      RightImageRect.Left := R.Right - ItemInfo.RightImageSize.cx;
      RightImageRect.Right := RightImageRect.Left + ItemInfo.RightImageSize.cx;
      RightImageRect.Top := R.Top + (R.Bottom - R.Top - ItemInfo.RightImageSize.cy) div 2;
      RightImageRect.Bottom := RightImageRect.Top + ItemInfo.RightImageSize.cy;
    end;
    DrawItemRightImage(Canvas, RightImageRect, ItemInfo);
  end;
end;

function TSpTBXItemViewer.GetItem: TSpTBXCustomItem;
var
  TBItem: TTBCustomItem;
begin
  TBItem := inherited Item;
  if Assigned(TBItem) then
    Result := TBItem as TSpTBXCustomItem
  else
    Result := nil;
end;

function TSpTBXItemViewer.GetHintText: Widestring;
var
  I: Integer;
  S: string;
begin
  // Get the short hint
  I := Pos('|', Item.Hint);
  if I = 0 then
    Result := Item.Hint
  else
    Result := Copy(Item.Hint, 1, I - 1);
  // Use the caption if there is no hint
  if (Result = '') and not(tboNoAutoHint in Item.EffectiveOptions) and
     (not(tbisSubmenu in Item.ItemStyle) or (tbisCombo in Item.ItemStyle) or
      not CaptionShown) then
  begin
    Result := SpStripAccelChars(SpStripTrailingPunctuation(Item.Caption));
  end;

  // Call associated action's OnHint event handler to post-process the hint
  if Assigned(Item.ActionLink) and (Item.ActionLink.Action is TCustomAction) and
    Assigned(TCustomAction(Item.ActionLink.Action).OnHint) then
  begin
    S := Result;
    if TCustomAction(Item.ActionLink.Action).DoHint(S) then
      Result := S
    else
      Result := '';
    // Note: TControlActionLink.DoShowHint actually misinterprets the result of DoHint, but we get it right...
  end;

  // Add shortcut text
  if (Result <> '') and Application.HintShortCuts and (Item.ShortCut <> scNone) then
    Result := Result + ' (' + ShortCutToText(Item.ShortCut) + ')';
end;

procedure TSpTBXItemViewer.CMHintShow(var Message: TMessage);
// Handle the CM_HINTSHOW message to show unicode hints using
// a custom THintWindow.
var
  HintInfo: PHintInfo;
  WideHint, PrevWideHint: Widestring;
  R, TextR, CursorR: TRect;
  PaintDefault: Boolean;
  {$IF CompilerVersion >= 23} //for Delphi XE2 and up
  Details: TThemedElementDetails;
  C: TColor;
  {$IFEND}
begin
  HintInfo := TCMHintShow(Message).HintInfo;
  WideHint := GetHintText;
  CursorR := BoundsRect;

  // Prepare the HintInfo
  HintInfo.HintStr := WideHint;
  HintInfo.CursorRect := CursorR;
  HintInfo.HintWindowClass := SpTBXHintWindowClass;   // Custom HintWindow class
  HintInfo.HintData := SpStockHintBitmap;  // TApplication.ActivateHint will pass the data to the HintWindow
  HintInfo.HideTimeout := 60000; // 1 minute

  // Prepare the HintBitmap
  SpStockHintBitmap.Canvas.Font.Assign(Screen.HintFont);
  SpStockHintBitmap.Canvas.Font.Color := clInfoText;
  {$IF CompilerVersion >= 23} //for Delphi XE2 and up
  if SkinManager.GetSkinType = sknDelphiStyle then begin
    Details := SpTBXThemeServices.GetElementDetails(thHintNormal);
    if SpTBXThemeServices.GetElementColor(Details, ecTextColor, C) and (C <> clNone) then
      SpStockHintBitmap.Canvas.Font.Color := C;
  end;
  {$IFEND}
  SpStockHintBitmap.Canvas.Pen.Color := clBlack;
  SpStockHintBitmap.Canvas.Brush.Color := clInfoBk;
  TextR := Rect(0, 0, 1, 1);
  SpDrawXPText(SpStockHintBitmap.Canvas, WideHint, TextR, DT_NOPREFIX or DT_CALCRECT);
  SpStockHintBitmap.Width := TextR.Right + 8;
  SpStockHintBitmap.Height := TextR.Bottom + 4;
  R := Rect(0, 0, SpStockHintBitmap.Width, SpStockHintBitmap.Height);
  SpDrawXPTooltipBackground(SpStockHintBitmap.Canvas, R);

  // Draw the hint in the HintBitmap
  PrevWideHint := WideHint;
  PaintDefault := True;
  DoDrawHint(SpStockHintBitmap, HintInfo.CursorPos, CursorR, WideHint, PaintDefault);
  if PaintDefault then begin
    HintInfo.HintStr := WideHint;
    HintInfo.CursorRect := CursorR;

    // Adjust the bounds and repaint the background if it's needed
    if WideHint <> PrevWideHint then begin
      TextR := Rect(0, 0, 1, 1);
      SpDrawXPText(SpStockHintBitmap.Canvas, WideHint, TextR, DT_NOPREFIX or DT_CALCRECT);
      SpStockHintBitmap.Width := TextR.Right + 8;
      SpStockHintBitmap.Height := TextR.Bottom + 4;
      R := Rect(0, 0, SpStockHintBitmap.Width, SpStockHintBitmap.Height);
      SpDrawXPTooltipBackground(SpStockHintBitmap.Canvas, R);
    end
    else
      R := Rect(0, 0, SpStockHintBitmap.Width, SpStockHintBitmap.Height);

    // Draw the hint
    OffsetRect(TextR, ((R.Right - TextR.Right) div 2) - 2, (R.Bottom - TextR.Bottom) div 2);
    SpDrawXPText(SpStockHintBitmap.Canvas, WideHint, TextR, DT_NOPREFIX);
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXRootItem }

procedure TSpTBXRootItem.DoPopupShowingChanged(APopupWindow: TTBPopupWindow; IsVisible: Boolean);
// This method is called by TSpTBXPopupWindow when the popup is Opened
begin
  if Assigned(APopupWindow) then begin
    if IsVisible then begin
      if Assigned(FOnInitPopup) then FOnInitPopup(Self, APopupWindow.View);
    end
    else begin
      if Assigned(FOnClosePopup) then FOnClosePopup(Self);
    end;
  end;
end;

function TSpTBXRootItem.GetPopupWindowClass: TTBPopupWindowClass;
begin
  Result := TSpTBXPopupWindow;
end;

procedure TSpTBXRootItem.SetToolBoxPopup(const Value: Boolean);
begin
  FToolBoxPopup := Value;
  if FToolBoxPopup then
    Options := Options + [tboToolbarStyle]
  else
    Options := Options - [tboToolbarStyle];
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXSubmenuItem }

constructor TSpTBXSubmenuItem.Create(AOwner: TComponent);
begin
  inherited;
  ItemStyle := ItemStyle + [tbisSubMenu, tbisSubitemsEditable];
end;

function TSpTBXSubmenuItem.GetDropdownCombo: Boolean;
begin
  Result := tbisCombo in ItemStyle;
end;

procedure TSpTBXSubmenuItem.SetDropdownCombo(Value: Boolean);
begin
  if (tbisCombo in ItemStyle) <> Value then begin
    if Value then ItemStyle := ItemStyle + [tbisCombo]
    else ItemStyle := ItemStyle - [tbisCombo];
    Change(True);
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXColorItem }

constructor TSpTBXColorItem.Create(AOwner: TComponent);
begin
  inherited;
  FColor := clWhite;
end;

function TSpTBXColorItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result := TSpTBXColorItemViewer;
end;

procedure TSpTBXColorItem.SetColor(Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    Change(False);
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXColorItemViewer }

procedure TSpTBXColorItemViewer.DoDrawImage(ACanvas: TCanvas;
  State: TSpTBXSkinStatesType; const PaintStage: TSpTBXPaintStage;
  var AImageList: TCustomImageList; var AImageIndex: Integer; var ARect: TRect;
  var PaintDefault: Boolean);
var
  C: TColor;
begin
  if PaintStage = pstPrePaint then begin
    if State = sknsDisabled then begin
      Inc(ARect.Right);
      Inc(ARect.Bottom);
      DrawEdge(ACanvas.Handle, ARect, BDR_SUNKENOUTER or BDR_RAISEDINNER, BF_RECT);
    end
    else begin
      if not IsToolbarStyle then
        InflateRect(ARect, -2, -2);
      C := TSpTBXColorItem(Item).Color;
      if C <> clNone then begin
        ACanvas.Brush.Color := clBtnShadow;
        ACanvas.FrameRect(ARect);
        InflateRect(ARect, -1, -1);
        ACanvas.Brush.Color := C;
        ACanvas.FillRect(ARect);
      end;
    end;
  end;
  inherited;
end;

function TSpTBXColorItemViewer.GetImageShown: Boolean;
begin
  Result := ((Item.DisplayMode in [nbdmDefault, nbdmImageAndText]) or
    (IsToolbarStyle and (Item.DisplayMode = nbdmTextOnlyInMenus)));
end;

function TSpTBXColorItemViewer.GetImageSize: TSize;
begin
  if IsToolbarStyle then begin
    Result.cx := 12;
    Result.cy := 12;
  end
  else begin
    Result.cx := 16;
    Result.cy := 16;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomLabelItem }

constructor TSpTBXCustomLabelItem.Create(AOwner: TComponent);
begin
  inherited;
  ItemStyle := ItemStyle - [tbisSelectable, tbisRedrawOnSelChange,
    tbisRedrawOnMouseOverChange] + [tbisClicksTransparent];
  Alignment := taLeftJustify;
  Stretch := False;
  DisplayMode := nbdmImageAndText;
end;

function TSpTBXCustomLabelItem.DialogChar(CharCode: Word): Boolean;
begin
  Result := inherited DialogChar(CharCode);
  if Enabled and Visible and Assigned(Control) and (Control is TWinControl) and
    IsAccel(CharCode, Caption) and SpCanFocus(TWinControl(Control)) then
  begin
    TWinControl(Control).SetFocus;
    Result := True;
  end;
end;

procedure TSpTBXCustomLabelItem.DoDrawButton(ACanvas: TCanvas; ARect: TRect;
  ItemInfo: TSpTBXMenuItemInfo; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  PaintDefault := True;
  inherited DoDrawButton(ACanvas, ARect, ItemInfo, PaintStage, PaintDefault);
  PaintDefault := False;
end;

function TSpTBXCustomLabelItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result := TSpTBXLabelItemViewer;
end;

procedure TSpTBXCustomLabelItem.ToggleControl;
begin
  // Do nothing, the Control property is not valid
end;

procedure TSpTBXCustomLabelItem.UpdateProps;
begin
  // Do nothing, the Control property is not valid
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXLabelItemViewer }

procedure TSpTBXLabelItemViewer.CalcSize(const Canvas: TCanvas; var AWidth,
  AHeight: Integer);
var
  TextMetrics: TTextMetric;
begin
  inherited CalcSize(Canvas, AWidth, AHeight);
  if not IsToolbarStyle and (Length(GetCaptionText) > 0) and (Item.CustomHeight <= -1) then begin
    GetTextMetrics(Canvas.Handle, TextMetrics);
    AHeight := TextMetrics.tmHeight;
  end;
end;

function TSpTBXLabelItemViewer.DoExecute: Boolean;
begin
  // Clicking a TSpTBXLabelItem on a popup menu causes the menu to close.
  // This is caused by TTBXItemViewer.MouseUp, which calls
  // TTBItemViewer.DoExecute
  // The TBXLabelItem doesn't fire the click because the ItemViewer descends
  // from TTBItemViewer instead of TTBXItemViewer.
  // TTBXItemViewer.MouseUp is the culprit of firing the DoExecute
  Result := False;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXSeparatorItem }

function TSpTBXSeparatorItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result := TSpTBXSeparatorItemViewer;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXSeparatorItemViewer }

procedure TSpTBXSeparatorItemViewer.CalcSize(const Canvas: TCanvas; var AWidth,
  AHeight: Integer);
begin
  if not IsToolbarStyle then begin
    if CurrentSkin.OfficeMenu then
      AHeight := 4  // For Office XP, 2003, 2007
    else
      AHeight := 10;
  end
  else begin
    AWidth := 6;
    AHeight := 6;
  end;
end;

function TSpTBXSeparatorItemViewer.IsStatusBarSeparator: Boolean;
var
  C: TComponent;
begin
  Result := False;
  C := Item.GetParentComponent;
  if Assigned(C) and (C is TSpTBXStatusToolbar) then
    Result := TSpTBXStatusToolbar(C).NeedsSeparatorRepaint;
end;

procedure TSpTBXSeparatorItemViewer.Paint(const Canvas: TCanvas; const ClientAreaRect: TRect;
  IsSelected, IsPushed, UseDisabledShadow: Boolean);
var
  Vertical, MenuItemStyle: Boolean;
  MarginsInfo: TSpTBXMenuItemMarginsInfo;
  R: TRect;
begin
  if TTBSeparatorItem(Item).Blank or IsStatusBarSeparator then
    Exit;

  R := ClientAreaRect;
  MenuItemStyle := View.IsPopup;
  if MenuItemStyle then begin
    Vertical := False;
    if SkinManager.GetSkinType = sknNone then begin
      // Add separator spacing when it's not on a ToolBoxPopup
      if not (tboToolbarStyle in Item.EffectiveOptions) then
        InflateRect(R, -tbMenuSeparatorOffset, 0);
    end
    else begin
      // Draw the separator from the gutter end if the separator is not on
      // a ToolBoxPopup and we are using the default Vista theme or the
      // skin has a gutter specified.
      if not (tboToolbarStyle in Item.EffectiveOptions) then
        if SpIsWinVistaOrUp or not CurrentSkin.Options(skncGutter, sknsNormal).IsEmpty then begin
          if View.Window is TSpTBXPopupWindow then
            CurrentSkin.GetMenuItemMargins(Canvas, TSpTBXPopupWindow(View.Window).MaximumImageSize.cx, MarginsInfo)
          else
            CurrentSkin.GetMenuItemMargins(Canvas, 0, MarginsInfo);
          if SpIsWinVistaOrUp then
            R.Left := MarginsInfo.GutterSize + MarginsInfo.ImageTextSpace
          else
            R.Left := MarginsInfo.GutterSize + MarginsInfo.ImageTextSpace + MarginsInfo.LeftCaptionMargin;
        end;
    end;
  end
  else
    Vertical := View.Orientation <> tbvoVertical;

  SpDrawXPMenuSeparator(Canvas, R, MenuItemStyle, Vertical);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXRadioGroupItem }

constructor TSpTBXRadioGroupItem.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultIndex := 0;
  FLastClickedIndex := 0;
  FStrings := TStringList.Create;
end;

destructor TSpTBXRadioGroupItem.Destroy;
begin
  FStrings.Free;
  inherited;
end;

procedure TSpTBXRadioGroupItem.DoClick(AItem: TSpTBXItem);
begin
  if Assigned(FOnClick) then FOnClick(AItem);
end;

procedure TSpTBXRadioGroupItem.DoFillStrings;
begin
  if Assigned(FOnFillStrings) then FOnFillStrings(Self, FStrings);
end;

procedure TSpTBXRadioGroupItem.ItemClickEvent(Sender: TObject);
var
  Item: TSpTBXItem;
begin
  Item := Sender as TSpTBXItem;
  if not Item.Checked and (Item.Tag > -1) and (Item.Tag < FStrings.Count) then
  begin
    Item.Checked := True;
    FLastClickedIndex := IndexOf(Item);
    DoClick(Item);
  end;
end;

procedure TSpTBXRadioGroupItem.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    Recreate;
end;

procedure TSpTBXRadioGroupItem.Recreate;
var
  I: Integer;
  A: TSpTBXItem;
begin
  // Delete FStrings items
  FStrings.Clear;
  for I := Count - 1 downto 0 do
    if Items[I].GroupIndex = C_SpTBXRadioGroupIndex then
      Delete(I);

  DoFillStrings;

  // Create group items
  for I := 0 to FStrings.Count - 1 do begin
    A := TSpTBXItem.Create(Self);
    A.Caption := FStrings[I];
    A.AutoCheck := False;
    A.GroupIndex := C_SpTBXRadioGroupIndex;
    A.Tag := I;
    A.OnClick := ItemClickEvent;
    Insert(I, A);
    if I = FDefaultIndex then A.Click;
  end;

  if Assigned(FOnUpdate) then FOnUpdate(Self);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXSkinGroupItem }

constructor TSpTBXSkinGroupItem.Create(AOwner: TComponent);
begin
  inherited;
  SkinManager.AddSkinNotification(Self);
end;

destructor TSpTBXSkinGroupItem.Destroy;
begin
  SkinManager.RemoveSkinNotification(Self);
  inherited;
end;

procedure TSpTBXSkinGroupItem.DoClick(AItem: TSpTBXItem);
begin
  SkinManager.SetSkin(FStrings[AItem.Tag]);
  inherited;
end;

procedure TSpTBXSkinGroupItem.DoSkinChange;
var
  I: Integer;
begin
  I := FStrings.IndexOf(SkinManager.CurrentSkinName);
  if I > -1 then
    Items[I].Click;

  if Assigned(FOnSkinChange) then FOnSkinChange(Self);
end;

procedure TSpTBXSkinGroupItem.DoFillStrings;
var
  I: Integer;
begin
  SkinManager.SkinsList.GetSkinNames(FStrings);

  // Sort the list and move the Default skin to the top
  FStrings.Sort;
  I := FStrings.IndexOf('Default');
  if I > -1 then FStrings.Move(I, 0);
  inherited;
  FDefaultIndex := FStrings.IndexOf(SkinManager.CurrentSkinName);
end;

procedure TSpTBXSkinGroupItem.WMSpSkinChange(var Message: TMessage);
begin
  DoSkinChange;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXSystemMenuItem }

constructor TSpTBXSystemMenuItem.Create(AOwner: TComponent);
begin
  inherited;
  ItemStyle := ItemStyle + [tbisSubMenu, tbisDontSelectFirst] - [tbisRedrawOnSelChange, tbisRedrawOnMouseOverChange];
  Caption := '&-';
  FShowSize := True;
end;

function TSpTBXSystemMenuItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result := TSpTBXSystemMenuItemViewer;
end;

function TSpTBXSystemMenuItem.GetSystemMenuParentForm: TCustomForm;
var
  C: TComponent;
begin
  Result := nil;
  C := GetParentComponent;
  if Assigned(C) and (C is TControl) then
    Result := GetParentForm(TControl(C));

  if not Assigned(Result) and Assigned(Application.MainForm) then begin
    if FMDISystemMenu then
      Result := Application.MainForm.ActiveMDIChild
    else
      Result := Application.MainForm;
  end;
end;

procedure TSpTBXSystemMenuItem.Click;
var
  Form: TCustomForm;
begin
  inherited;
  Clear;

  Form := GetSystemMenuParentForm;
  if Assigned(Form) then
    SpFillSystemSpTBXPopup(Form, Self, True, True, True, True, CommandClick);
end;

procedure TSpTBXSystemMenuItem.CommandClick(Sender: TObject);
var
  Form: TCustomForm;
  I: Integer;
begin
  Form := GetSystemMenuParentForm;
  if Assigned(Form) and Assigned(Sender) then begin
    I := TComponent(Sender).Tag;
    if I = SC_MAXIMIZE then
      Form.WindowState := wsMaximized
    else
      SendMessage(Form.Handle, WM_SYSCOMMAND, I, GetMessagePos);
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXSystemMenuItemViewer }

procedure TSpTBXSystemMenuItemViewer.CalcSize(const Canvas: TCanvas;
  var AWidth, AHeight: Integer);
begin
  AWidth := GetSystemMetrics(SM_CXSMICON) + 2;
  AHeight := GetSystemMetrics(SM_CYSMICON) + 2;
end;

procedure TSpTBXSystemMenuItemViewer.Paint(const Canvas: TCanvas;
  const ClientAreaRect: TRect; IsSelected, IsPushed, UseDisabledShadow: Boolean);

  function GetIconHandle: HICON;
  var
    Form: TForm;
  begin
    Result := 0;
    if Assigned(Application.MainForm) then begin
      if TSpTBXSystemMenuItem(Item).MDISystemMenu then
        Form := Application.MainForm.ActiveMDIChild
      else
        Form := Application.MainForm;
      if Assigned(Form) then
        Result := Form.Icon.Handle;
    end;
    if Result = 0 then
      Result := Application.Icon.Handle;
    if Result = 0 then
      Result := LoadIcon(0, IDI_APPLICATION);
  end;

var
  R: TRect;
  TempIcon: HICON;
begin
  R := ClientAreaRect;
  InflateRect(R, -1, -1);
  TempIcon := CopyImage(GetIconHandle, IMAGE_ICON, R.Right - R.Left, R.Bottom - R.Top, LR_COPYFROMRESOURCE);
  try
    DrawIconEx(Canvas.Handle, R.Left, R.Top, TempIcon, 0, 0, 0, 0, DI_NORMAL);
  finally
    DestroyIcon(TempIcon);
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomToolPalette }

constructor TSpTBXCustomToolPalette.Create(AOwner: TComponent);
begin
  inherited;
  FColCount := 1;
  FRowCount := 1;
  FSelectedCell.X := -1;
end;

function TSpTBXCustomToolPalette.DoCellClick(ACol, ARow: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnCellClick) then FOnCellClick(Self, ACol, ARow, Result);
end;

procedure TSpTBXCustomToolPalette.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TSpTBXCustomToolPalette.DoDrawCellImage(ACanvas: TCanvas;
  const ARect: TRect; ACol, ARow: Integer; ItemInfo: TSpTBXMenuItemInfo);
begin
  if Assigned(FOnDrawCellImage) then
    FOnDrawCellImage(Self, ACanvas, ARect, ACol, ARow, ItemInfo.Checked,
      ItemInfo.HotTrack, ItemInfo.Enabled);
end;

procedure TSpTBXCustomToolPalette.DoGetCellHint(ACol, ARow: Integer; var AHint: WideString);
begin
  if Assigned(FOnGetCellHint) then FOnGetCellHint(Self, ACol, ARow, AHint);
end;

function TSpTBXCustomToolPalette.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result := TSpTBXToolViewer;
end;

procedure TSpTBXCustomToolPalette.HandleClickCell(ACol, ARow: Integer);
begin
  if DoCellClick(ACol, ARow) then SetSelectedCell(Point(ACol, ARow));
end;

procedure TSpTBXCustomToolPalette.SetColCount(Value: TSpTBXRowColCount);
begin
  if FColCount <> Value then begin
    FColCount := Value;
    Change(True);
  end;
end;

procedure TSpTBXCustomToolPalette.SetRowCount(Value: TSpTBXRowColCount);
begin
  if FRowCount <> Value then begin
    FRowCount := Value;
    Change(True);
  end;
end;

procedure TSpTBXCustomToolPalette.SetSelectedCell(Value: TPoint);
begin
  FSelectedCell := Value;
  Change(True);
  DoChange;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXToolViewer }

constructor TSpTBXToolViewer.Create(AView: TTBView; AItem: TTBCustomItem;
  AGroupLevel: Integer);
begin
  inherited;
  FColCount := TSpTBXCustomToolPalette(AItem).ColCount;
  FRowCount := TSpTBXCustomToolPalette(AItem).RowCount;
end;

procedure TSpTBXToolViewer.CalcCellSize(ACanvas: TCanvas; var AWidth, AHeight: Integer);
var
  ImageSize: TSize;
begin
  ImageSize := GetImageSize;
  AWidth := ImageSize.cx + 6;
  AHeight := ImageSize.cy + 6;
end;

procedure TSpTBXToolViewer.CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer);
var
  W, H: Integer;
  MarginsInfo: TSpTBXMenuItemMarginsInfo;
begin
  if IsToolbarStyle then
    FIndent := 0
  else begin
    if View.Window is TSpTBXPopupWindow then
      CurrentSkin.GetMenuItemMargins(Canvas, TSpTBXPopupWindow(View.Window).MaximumImageSize.cx, MarginsInfo)
    else
      CurrentSkin.GetMenuItemMargins(Canvas, 0, MarginsInfo);
    FIndent := MarginsInfo.GutterSize + MarginsInfo.ImageTextSpace + MarginsInfo.LeftCaptionMargin - 3;
  end;

  FColCount := Item.ColCount;
  FRowCount := Item.RowCount;
  CalcCellSize(Canvas, W, H);
  AWidth := FIndent + W * FColCount;
  if not IsToolbarStyle then
    Inc(AWidth, MarginsInfo.RightCaptionMargin);
  AHeight := H * FRowCount;
  if AWidth < 8 then AWidth := 8;
  if AHeight < 8 then AHeight := 8;
end;

procedure TSpTBXToolViewer.DoDrawHint(AHintBitmap: TBitmap; CursorPos: TPoint;
  var CursorRect: TRect; var AHint: Widestring; var PaintDefault: Boolean);
var
  Col, Row: Integer;
begin
  if GetCellAt(CursorPos.X - BoundsRect.Left, CursorPos.Y - BoundsRect.Top, Col, Row) then begin
    AHint := GetCellHint(Col, Row);
    CursorRect := GetCellRect(CursorRect, Col, Row);
    inherited DoDrawHint(AHintBitmap, CursorPos, CursorRect, AHint, PaintDefault);
  end
  else
    PaintDefault := False;
end;

procedure TSpTBXToolViewer.DrawCellImage(ACanvas: TCanvas; const ARect: TRect;
  Col, Row: Integer; ItemInfo: TSpTBXMenuItemInfo);
var
  I: Integer;
  IL: TCustomImageList;
begin
  if not Item.CustomImages then begin
    I := GetImageIndex(Col, Row);
    IL := Item.Images;
    SpDrawImageList(ACanvas, ARect, IL, I, ItemInfo.Enabled, True);
  end;
  Item.DoDrawCellImage(ACanvas, ARect, Col, Row, ItemInfo);
end;

procedure TSpTBXToolViewer.Entering;
begin
  FHotCell := Point(-1, 0);
  if (View is TSpTBXPopupWindowView) and Assigned(View.Selected) then begin
    if View.Selected.Index > Index then begin
      FHotCell := Point(FColCount - 1, FRowCount - 1);
      while (FHotCell.X > 0) and not IsCellVisible(FHotCell) do
        Dec(FHotCell.X);
    end
    else
      if View.Selected.Index < Index then
        FHotCell := Point(0, 0);
  end;
  inherited;
end;

function TSpTBXToolViewer.GetCellAt(X, Y: Integer; out Col, Row: Integer): Boolean;
begin
  if (FCellWidth = 0) or (FCellHeight = 0) then begin
    Col := 0;
    Row := 0;
  end
  else begin
    Col := (X - FIndent) div FCellWidth;
    Row := Y div FCellHeight;
  end;
  Result := (Col >= 0) and (Row >= 0) and (Col < FColCount) and (Row < FRowCount);
end;

function TSpTBXToolViewer.GetCellHint(Col, Row: Integer): WideString;
begin
  Result := '';
  Item.DoGetCellHint(Col, Row, Result);
end;

function TSpTBXToolViewer.GetCellRect(ClientAreaRect: TRect; Col, Row: Integer): TRect;
begin
  Result := Bounds(ClientAreaRect.Left + FIndent + Col * FCellWidth, ClientAreaRect.Top + Row * FCellHeight, FCellWidth, FCellHeight);
end;

function TSpTBXToolViewer.GetImageIndex(Col, Row: Integer): Integer;
begin
  Result := Col + Row * FColCount;
end;

function TSpTBXToolViewer.GetImageShown: Boolean;
begin
  Result := True;
end;

function TSpTBXToolViewer.GetImageSize: TSize;
var
  IL: TCustomImageList;
begin
  if Item.CustomImages then
    IL := nil
  else
    IL := Item.Images;
    
  if Assigned(IL) then begin
    Result.cx := IL.Width;
    Result.cy := IL.Height;
  end
  else begin
    Result.cx := 12;
    Result.cy := 12;
  end;
end;

function TSpTBXToolViewer.GetItem: TSpTBXCustomToolPalette;
var
  TBItem: TTBCustomItem;
begin
  TBItem := inherited Item;
  if Assigned(TBItem) then
    Result := TBItem as TSpTBXCustomToolPalette
  else
    Result := nil;
end;

procedure TSpTBXToolViewer.InvalidateCell(ACol, ARow: Integer);
var
  R: TRect;
begin
  R := GetCellRect(BoundsRect, ACol, ARow);
  InvalidateRect(View.Window.Handle, @R, False);
end;

function TSpTBXToolViewer.IsCellVisible(Cell: TPoint): Boolean;
var
  IL: TCustomImageList;
begin
  Result := (Cell.X >= 0) and (Cell.Y >= 0) and (Cell.X < FColCount) and (Cell.Y < FRowCount);
  if Result and not Item.CustomImages then begin
    IL := Item.Images;
    if Assigned(IL) then
      Result := (Cell.X + Cell.Y * FColCount) < IL.Count;
  end;
end;

procedure TSpTBXToolViewer.KeyDown(var Key: Word; Shift: TShiftState);
var
  OldPos, Pos: TPoint;
begin
  if IsCellVisible(FHotCell) then
    OldPos := FHotCell
  else
    if IsCellVisible(Item.SelectedCell) then
      OldPos := Item.SelectedCell
    else
      OldPos.X := -1;

  if OldPos.X >= 0 then begin
    Pos := OldPos;
    case Key of
      VK_LEFT:
        begin
          Dec(Pos.X);
          if Pos.X < 0 then begin
            Pos.X := FColCount - 1;
            Dec(Pos.Y);
          end;
        end;
      VK_UP: Dec(Pos.Y);
      VK_RIGHT:
        begin
          Inc(Pos.X);
          if Pos.X >= FColCount then
          begin
            Pos.X := 0;
            Inc(Pos.Y);
          end;
        end;
      VK_DOWN: Inc(Pos.Y);
      VK_PRIOR: Pos.Y := 0;
      VK_NEXT: Pos.Y := FRowCount - 1;
      VK_HOME: Pos.X := 0;
      VK_END: Pos.Y := FColCount - 1;
      VK_RETURN:
        if IsCellVisible(FHotCell) then begin
          Item.HandleClickCell(FHotCell.X, FHotCell.Y);
          Exit;
        end;
    else
      inherited;
      Exit;
    end;
  end
  else begin
    OldPos := Point(-1, 0);
    Pos := Point(0, 0);
  end;

  if ((OldPos.X <> Pos.X) or (OldPos.Y <> Pos.Y)) and IsCellVisible(Pos) then begin
    Key := 0;
    FHotCell := Pos;
    Item.Change(False);
  end;
end;

procedure TSpTBXToolViewer.MouseDown(Shift: TShiftState; X, Y: Integer;
  var MouseDownOnMenu: Boolean);
begin
  FMouseIsDown := True;
  MouseMove(X, Y);
  inherited;
  View.SetCapture;
end;

procedure TSpTBXToolViewer.MouseMove(X, Y: Integer);
var
  OldHotCell: TPoint;
begin
  OldHotCell := FHotCell;
  if not GetCellAt(X, Y, FHotCell.X, FHotCell.Y) then
    FHotCell := Point(-1, 0);

  if (FHotCell.X <> OldHotCell.X) or (FHotCell.Y <> OldHotCell.Y) then
    if Show and not IsRectEmpty(BoundsRect) {and not (Item is TTBControlItem)} then begin
      Include(State, tbisInvalidated);
      InvalidateCell(OldHotCell.X, OldHotCell.Y);
      InvalidateCell(FHotCell.X, FHotCell.Y);
    end;
end;

procedure TSpTBXToolViewer.MouseUp(X, Y: Integer; MouseWasDownOnMenu: Boolean);
var
  Col, Row: Integer;
begin
  FMouseIsDown := False;
  if GetCellAt(X, Y, Col, Row) then
    Item.HandleClickCell(Col, Row);
  View.EndModalWithClick(Self);
  inherited;
end;

procedure TSpTBXToolViewer.Paint(const Canvas: TCanvas;
  const ClientAreaRect: TRect; IsSelected, IsPushed,
  UseDisabledShadow: Boolean);
var
  I, J: Integer;
  ItemInfo: TSpTBXMenuItemInfo;
  CellRect: TRect;
  ItemHotTrack: Boolean;
  ImageSize: TSize;
begin
  CalcCellSize(Canvas, FCellWidth, FCellHeight);

  SpFillItemInfo(Canvas, Self, ItemInfo);
  ItemInfo.ToolbarStyle := True;
  ItemHotTrack := False;
  if IsSelected then
    if not ItemInfo.Enabled and not View.MouseOverSelected then ItemHotTrack := True
    else if ItemInfo.Enabled then ItemHotTrack := True;

  for J := 0 to FRowCount - 1 do
    for I := 0 to FColCount - 1 do
      if IsCellVisible(Point(I, J)) then begin
        ItemInfo.HotTrack := False;
        ItemInfo.Pushed := False;
        if ItemHotTrack and (FHotCell.X = I) and (FHotCell.Y = J) then begin
          ItemInfo.HotTrack := True;
          if IsPushed then ItemInfo.Pushed := True
        end;
        if (Item.SelectedCell.X = I) and (Item.SelectedCell.Y = J) then
          ItemInfo.Checked := True
        else
          ItemInfo.Checked := False;
        ItemInfo.State := CurrentSkin.GetState(ItemInfo.Enabled, ItemInfo.Pushed, ItemInfo.HotTrack, ItemInfo.Checked);
        CellRect := GetCellRect(ClientAreaRect, I, J);

        // Paint the cell
        SpDrawXPMenuItem(Canvas, CellRect, ItemInfo);
        ImageSize := GetImageSize;
        CellRect := SpCenterRect(CellRect, ImageSize.cx, ImageSize.cy);
        DrawCellImage(Canvas, CellRect, I, J, ItemInfo);
      end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXColorPalette }

constructor TSpTBXColorPalette.Create(AOwner: TComponent);
begin
  inherited;
  CustomImages := True;
  FCustomColors := False;
  ColCount := CPDefaultCols;
  RowCount := CPDefaultRows; // 40 Default Colors
  Options := Options + [tboShowHint];
  FColor := clNone;
end;

procedure TSpTBXColorPalette.DoChange;
begin
  if SelectedCell.X >= 0 then
    FColor := GetCellColor(SelectedCell.X, SelectedCell.Y);
  inherited;
end;

procedure TSpTBXColorPalette.DoDrawCellImage(ACanvas: TCanvas;
  const ARect: TRect; ACol, ARow: Integer; ItemInfo: TSpTBXMenuItemInfo);
var
  R: TRect;
begin
  R := ARect;
  ACanvas.Brush.Color := clBtnShadow;
  ACanvas.FrameRect(R);
  InflateRect(R, -1, -1);
  if ItemInfo.Enabled then begin
    ACanvas.Brush.Color := GetCellColor(ACol, ARow);
    ACanvas.FillRect(R);
  end;
end;

procedure TSpTBXColorPalette.DoGetCellHint(ACol, ARow: Integer; var AHint: WideString);
var
  C: TColor;
begin
  GetCellInfo(ACol, ARow, C, AHint);
end;

function TSpTBXColorPalette.FindCell(AColor: TColor): TPoint;
var
  I, J: Integer;
  C: TColor;
begin
  if AColor <> clNone then
    AColor := ColorToRGB(AColor);
  for J := 0 to RowCount - 1 do
    for I := 0 to ColCount - 1 do begin
      C := GetCellColor(I, J);
      if C <> clNone then
        C := ColorToRGB(C);
      if C = AColor then begin
        Result.X := I;
        Result.Y := J;
        Exit;
      end;
    end;
  Result.X := -1;
  Result.Y := 0;
end;

function TSpTBXColorPalette.GetCellColor(ACol, ARow: Integer): TColor;
var
  W: WideString;
begin
  GetCellInfo(ACol, ARow, Result, W);
end;

procedure TSpTBXColorPalette.GetCellInfo(ACol, ARow: Integer;
  out AColor: TColor; out AName: WideString);
var
  I: Integer;
begin
  AColor := clNone;
  AName := '';
  if not FCustomColors then begin
    I := ACol + ARow * ColCount;
    if (I > -1) and (I < CPDefaultCols * CPDefaultRows) then begin
      AColor := CPDefaultColors[I].Value;
      AName := CPDefaultColors[I].Name;
    end;
  end;

  if Assigned(FOnGetColor) then FOnGetColor(Self, ACol, ARow, AColor, AName);
end;

procedure TSpTBXColorPalette.SetColor(Value: TColor);
begin
  FColor := Value;
  SelectedCell := FindCell(Value);
end;

procedure TSpTBXColorPalette.SetCustomColors(const Value: Boolean);
begin
  if FCustomColors <> Value then begin
    FCustomColors := Value;
    if not Value then begin
      RowCount := CPDefaultCols;
      ColCount := CPDefaultRows;
    end;
    Change(True);    
  end;
end;

procedure TSpTBXColorPalette.SetColCount(Value: TSpTBXRowColCount);
begin
  if FCustomColors then
    inherited SetColCount(Value)
  else
    inherited SetColCount(CPDefaultCols);
end;

procedure TSpTBXColorPalette.SetRowCount(Value: TSpTBXRowColCount);
begin
  if FCustomColors then
    inherited SetRowCount(Value)
  else
    inherited SetRowCount(CPDefaultRows);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXItemCache }

procedure TSpTBXItemCache.Assign(Source: TPersistent);
var
  C: TSpTBXItemCache;
begin
  if Source is TSpTBXItemCache then
  begin
    C := Source as TSpTBXItemCache;
    Dock := C.Dock;
    Item := C.Item;
    Width := C.Width;
    Height := C.Height;
    ParentWidth := C.ParentWidth;
    ParentHeight := C.ParentHeight;
  end
  else inherited Assign(Source);
end;

function TSpTBXItemCache.GetName: TComponentName;
begin
  if Assigned(FItem) then
    Result := FItem.Name
  else
    Result := FName;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXItemCacheCollection }

function TSpTBXItemCacheCollection.Add(AItem: TTBCustomItem): Integer;
var
  F: TSpTBXItemCache;
begin
  F := TSpTBXItemCache(inherited Add);
  F.Item := AItem;
  Result := F.Index;
end;

function TSpTBXItemCacheCollection.GetItem(Index: Integer): TSpTBXItemCache;
begin
  Result := TSpTBXItemCache(inherited Items[Index]);
end;

function TSpTBXItemCacheCollection.IndexOf(AItem: TTBCustomItem): Integer;
var
  I: Integer;
begin
  Result := -1;
  if Assigned(AItem) then
    for I := 0 to Count - 1 do
      if Items[I].Item = AItem then begin
        Result := I;
        Break;
      end;
end;

procedure TSpTBXItemCacheCollection.SetItem(Index: Integer;
  const Value: TSpTBXItemCache);
begin
  inherited Items[Index] := Value;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXDock }

constructor TSpTBXDock.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque];
  Color := clNone;
  SkinManager.AddSkinNotification(Self);
  DoubleBuffered := True;
end;

destructor TSpTBXDock.Destroy;
begin
  SkinManager.RemoveSkinNotification(Self);
  inherited;
end;

function TSpTBXDock.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
  FPrevWidth := Width;
  FPrevHeight := Height;
  Result := inherited CanResize(NewWidth, NewHeight);
end;

procedure TSpTBXDock.DoDrawBackground(ACanvas: TCanvas; ARect: TRect;
  const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawBackground) then FOnDrawBackground(Self, ACanvas, ARect, PaintStage, PaintDefault);
end;

procedure TSpTBXDock.DrawBackground(DC: HDC; const DrawRect: TRect);
var
  ACanvas: TCanvas;
  PaintDefault: Boolean;
begin
  // OnDrawBackground should be used to paint all the toolbars + dock, it is
  // used by TSpTBXStatusBar and TSpTBXDockablePanel to paint the whole client
  // area with custom painting.
  // OnDrawBackground is triggered by the Dock and by the docked Toolbar.
  if (csDestroying in ComponentState) then Exit;

  ACanvas := TCanvas.Create;
  try
    ACanvas.Handle := DC;
    ACanvas.Lock;

    PaintDefault := True;
    DoDrawBackground(ACanvas, DrawRect, pstPrePaint, PaintDefault);
    if PaintDefault then begin
      // Paint the Bitmap if it's assigned, or use the skin or the Color
      if UsingBitmap then
        inherited
      else
        if Color = clNone then
          SpDrawXPDock(ACanvas, DrawRect, Position in [dpLeft, dpRight])
        else begin
          ACanvas.Brush.Color := Color;
          ACanvas.FillRect(DrawRect);
        end;
    end;

    PaintDefault := True;
    DoDrawBackground(ACanvas, DrawRect, pstPostPaint, PaintDefault);
  finally
    ACanvas.Unlock;
    ACanvas.Handle := 0;
    ACanvas.Free;
  end;
end;

procedure TSpTBXDock.Paint;
begin
  DrawBackground(Canvas.Handle, ClientRect);
  inherited;
end;

procedure TSpTBXDock.Resize;
var
  I, J: Integer;
  ResizeToolbars: Boolean;
  V: TTBItemViewer;
  R: TRect;
begin
  inherited Resize;

  // For anchored and right aligned items
  if Position in [dpLeft, dpRight] then
    ResizeToolbars := Height < FPrevHeight
  else
    ResizeToolbars := Width < FPrevWidth;
  if ResizeToolbars then
    for I := 0 to ToolbarCount - 1 do
      if Toolbars[I] is TSpTBXToolbar then
        TSpTBXToolbar(Toolbars[I]).Resize;

  // Invalidate the dock and the toolbars
  for J := 0 to ToolbarCount - 1 do begin
    Invalidate;
    if Toolbars[J] is TSpTBXToolbar then begin
      with TSpTBXToolbar(Toolbars[J]) do begin
        // Invalidate Control Items
        for I := 0 to View.ViewerCount - 1 do begin
          V := View.Viewers[I];
          if V.Show and not IsRectEmpty(V.BoundsRect) and not (V.Item is TTBControlItem) then
            View.Invalidate(V);
        end;
        // Invalidate Toolbar
        Update;
        if HandleAllocated then
          RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_ERASE);
        // Invalidate Items
        for I := 0 to View.ViewerCount - 1 do begin
          V := View.Viewers[I];
          if V.Show and not IsRectEmpty(V.BoundsRect) and not (V.Item is TTBControlItem) then begin
            R := V.BoundsRect;
            ValidateRect(Handle, @R);
          end;
        end;
      end;
    end
    else
      if Toolbars[J] is TSpTBXCustomToolWindow then
        with TSpTBXCustomToolWindow(Toolbars[J]) do begin
          if HandleAllocated then
            RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_ERASE);
        end;
  end;
end;

function TSpTBXDock.UsingBackground: Boolean;
begin
  // UsingBackground is used by TTB2Dock to repaint the Dock and Toolbars
  // To check if the Dock is using a Bitmap use SpIsDockUsingBitmap instead
  Result := True;
end;

function TSpTBXDock.UsingBitmap: Boolean;
begin
  Result := SpIsDockUsingBitmap(Self);
end;

procedure TSpTBXDock.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  //DrawBackground(Message.DC, ClientRect);
  Message.Result := 1;
end;

procedure TSpTBXDock.WMMove(var Message: TWMMove);
begin
  FMoving := True;
  inherited;
  FMoving := False;
end;

procedure TSpTBXDock.WMSize(var Message: TWMSize);
begin
  FResizing := True;
  inherited;
  FResizing := False;
end;

procedure TSpTBXDock.WMSpSkinChange(var Message: TMessage);
begin
  Invalidate;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXToolbarView }

constructor TSpTBXToolbarView.Create(AOwner: TComponent; AParentView: TTBView;
  AParentItem: TTBCustomItem; AWindow: TWinControl;
  AIsToolbar, ACustomizing, AUsePriorityList: Boolean);
begin
  inherited;
  FMaxSize := -1;
end;

procedure TSpTBXToolbarView.DoUpdatePositions(var ASize: TPoint);
var
  I, W, H: Integer;
  CtlCanvas: TControlCanvas;
begin
  // Find the tallest item size so we can stretch the items
  // vertically (if Toolbar.Stretch is true)
  FTallestItemSize := 0;

  if Assigned(Window) and Window.HandleAllocated and
     (Window.ClientWidth > 0) and (Window.ClientHeight > 0) and
     (ViewerCount > 2) and not IsUpdating then
  begin
    CtlCanvas := TControlCanvas.Create;
    try
      CtlCanvas.Control := Window;
      for I := 0 to ViewerCount - 1 do begin
        W := 0;
        H := 0;
        if TTBCustomItem(Viewers[I].Item).Visible then
          if Viewers[I] is TSpTBXItemViewer then
            TSpTBXItemViewer(Viewers[I]).InternalCalcSize(CtlCanvas, False, W, H)
          else
            TTBItemViewerAccess(Viewers[I]).CalcSize(CtlCanvas, W, H);
        if Orientation = tbvoVertical then begin
          if W > FTallestItemSize then FTallestItemSize := W;
        end
        else
          if H > FTallestItemSize then FTallestItemSize := H;
      end;
    finally
      CtlCanvas.Free;
    end;
  end;

  inherited DoUpdatePositions(ASize);
end;

procedure TSpTBXToolbarView.BeginUpdate;
begin
  Inc(FUpdating);
  inherited BeginUpdate;
end;

procedure TSpTBXToolbarView.EndUpdate;
begin
  Dec(FUpdating);
  inherited EndUpdate;
  if FUpdating = 0 then
    if Assigned(Owner) and (Owner is TSpTBXToolbar) then begin
      TSpTBXToolbar(Owner).RightAlignItems;
      TSpTBXToolbar(Owner).AnchorItems;
    end;
end;

function TSpTBXToolbarView.IsUpdating: Boolean;
begin
  Result := FUpdating > 0;
end;

procedure TSpTBXToolbarView.SetMaxSize(const Value: Integer);
begin
  if FMaxSize <> Value then begin
    FMaxSize := Value;
    UpdatePositions;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXToolbar }

constructor TSpTBXToolbar.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque];
  Color := clNone;

  Items.RegisterNotification(DoItemNotification);
  FAnchoredControlItems := TSpTBXItemCacheCollection.Create(TSpTBXItemCache);
  FChevronVertical := True;
  FCustomizable := True;
  FDefaultToolbarBorderSize := CDefaultToolbarBorderSize;
  FDisplayMode := tbdmSelectiveCaption;
  SkinManager.AddSkinNotification(Self);
  DoubleBuffered := True;
end;

destructor TSpTBXToolbar.Destroy;
begin
  SkinManager.RemoveSkinNotification(Self);
  Items.UnRegisterNotification(DoItemNotification);
  FAnchoredControlItems.Free;
  inherited;
end;

procedure TSpTBXToolbar.DoDrawBackground(ACanvas: TCanvas; ARect: TRect;
  const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawBackground) then FOnDrawBackground(Self, ACanvas, ARect,
    PaintStage, PaintDefault);
end;

procedure TSpTBXToolbar.DoItemClick(Item: TTBCustomItem; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // Force OnClick event, by default tbisClicksTransparent Items doesn't get executed
  if (Button = mbLeft) and Item.Enabled then
    if (tbisClicksTransparent in TTBCustomItemAccess(Item).ItemStyle) then
      if Assigned(Item.OnClick) then Item.OnClick(Item);
end;

procedure TSpTBXToolbar.DoItemNotification(Ancestor: TTBCustomItem;
  Relayed: Boolean; Action: TTBItemChangedAction; Index: Integer;
  Item: TTBCustomItem);
var
  I: Integer;
begin
  if (csDestroying in ComponentState) or (csReading in ComponentState) then Exit;

  if not (tstResizing in FState) and not IsItemMoving then begin
    if Assigned(FOnItemNotification) then FOnItemNotification(Self, Ancestor, Relayed, Action, Index, Item);

    case Action of
      tbicInserted:
        begin
          RightAlignItems;
          AnchorItems(True);
        end;
      tbicDeleting:
        begin
          I := FAnchoredControlItems.IndexOf(Item);
          if I > -1 then
            FAnchoredControlItems.Delete(I);
          RightAlignItems;
          AnchorItems(True);
        end;
      tbicInvalidateAndResize:
        begin
          RightAlignItems;
        end;
    end;
  end;
end;

procedure TSpTBXToolbar.Resize;
begin
  FState := FState + [tstResizing];
  try
    RightAlignItems;
    AnchorItems;
  finally
    FState := FState - [tstResizing];
  end;
  inherited;
end;

procedure TSpTBXToolbar.AnchorItems(UpdateControlItems: Boolean);
var
  I, J, UpdatedDelta: Integer;
  SpIV: TSpTBXItemViewer;
  Size: TPoint;
  CI: TTBControlItem;
  IV: TTBItemViewer;
  IsRotated: Boolean;
begin
  if (csDestroying in ComponentState) or
    (tstAnchoring in FState) or not Assigned(CurrentDock) or
    (CurrentDock.Width = 0) or (CurrentDock.Height = 0) or
    not Stretch or (ShrinkMode <> tbsmNone) or IsUpdating then
      Exit;

  FState := FState + [tstAnchoring];
  View.BeginUpdate;
  try
    View.ValidatePositions;
    IsRotated := IsVertical;
    // Adjust the delta, only used when inserting/deleting an item on the toolbar
    UpdatedDelta := 0;
    if (FLastSelectableWidth > 0) and UpdateControlItems then begin
      IV := View.NextSelectable(nil, False);
      if Assigned(IV) then
        if IsRotated then
          UpdatedDelta := FLastSelectableWidth - IV.BoundsRect.Bottom
        else
          UpdatedDelta := FLastSelectableWidth - IV.BoundsRect.Right;
    end;

    // Calculate the Toolbar size
    Size := Point(CurrentDock.Width, CurrentDock.Height);

    // Resize the anchored items
    for I := 0 to View.ViewerCount - 1 do
      if View.Viewers[I] is TSpTBXItemViewer then begin
        SpIV := View.Viewers[I] as TSpTBXItemViewer;
        if SpIV.Item.Anchored then begin
          // Revalidate FAnchorSize and set FAnchorDelta
          if (SpIV.FAnchorSize.X = 0) and (SpIV.FAnchorSize.Y = 0) then
            SpIV.FAnchorSize := Size;

          // Adjust the delta, only used when inserting/deleting an item on
          // the toolbar and resize
          if IsRotated then begin
            SpIV.FAnchorSize.Y := SpIV.FAnchorSize.Y - UpdatedDelta;
            SpIV.FAnchorDelta := Size.Y - SpIV.FAnchorSize.Y;
          end
          else begin
            SpIV.FAnchorSize.X := SpIV.FAnchorSize.X - UpdatedDelta;
            SpIV.FAnchorDelta := Size.X - SpIV.FAnchorSize.X;
          end;
        end;
      end
      else begin
        // Client align TTBControlItem items if the associated Control is client
        // aligned or has akRight in its Anchors property.
        CI := IsAnchoredControlItem(View.Viewers[I].Item);
        J := FAnchoredControlItems.IndexOf(View.Viewers[I].Item);
        if Assigned(CI) then begin
          // Add the TTBControlItem item to the list if its not there
          if J = -1 then begin
            J := FAnchoredControlItems.Add(CI);
            FAnchoredControlItems[J].Width := CI.Control.Width;
            FAnchoredControlItems[J].Height := CI.Control.Height;
            FAnchoredControlItems[J].ParentWidth := Size.X;
            FAnchoredControlItems[J].ParentHeight := Size.Y;
            FAnchoredControlItems[J].Dock := CurrentDock;
          end;
          // Resize
          if FAnchoredControlItems[J].Dock = CurrentDock then begin
            FAnchoredControlItems[J].Width := FAnchoredControlItems[J].Width + UpdatedDelta;
            CI.Control.Width := FAnchoredControlItems[J].Width + (Size.X - FAnchoredControlItems[J].ParentWidth);
          end;
        end
        else
          // If ControlItem is not valid delete it from the list
          if J > -1 then
            FAnchoredControlItems.Delete(J);
      end;
    View.UpdatePositions;
  finally
    View.EndUpdate;
    FState := FState - [tstAnchoring];
  end;

  // We can't calculate the delta based on the IV.BoundsRect because
  // the IV is nil on tbicDeleting notification.
  // We have to keep track of the sum of the selectable items width
  IV := View.NextSelectable(nil, False);
  if Assigned(IV) then begin
    if IsRotated then
      FLastSelectableWidth := IV.BoundsRect.Bottom
    else
      FLastSelectableWidth := IV.BoundsRect.Right;
  end
  else
    FLastSelectableWidth := 0;
end;

function TSpTBXToolbar.IsAnchoredControlItem(Item: TTBCustomItem): TTBControlItem;
var
  CI: TTBControlItem;
begin
  Result := nil;
  if Assigned(CurrentDock) and (Item is TTBControlItem) then begin
    CI := Item as TTBControlItem;
    if Assigned(CI.Control) and
      ((CI.Control.Align = alClient) or (akRight in CI.Control.Anchors)) then
    begin
      Result := CI;
    end
    else
      Result := nil;
  end;
end;

procedure TSpTBXToolbar.RightAlignItems;
var
  I, VisibleWidth, RightAlignedWidth: Integer;
  Spacer: TSpTBXItemViewer;
  IsRotated: Boolean;
begin
  if (csDestroying in ComponentState) or (tstRightAligning in FState) or
    not Assigned(CurrentDock) or (Items.Count <= 0) or
    not Stretch or (ShrinkMode <> tbsmNone) or
    ((CurrentDock.Width <= 0) and (CurrentDock.Height <= 0)) or IsUpdating then
      Exit;

  FState := FState + [tstRightAligning];
  View.ValidatePositions;
  View.BeginUpdate;
  try
    // Find the spacer and the right aligned items
    IsRotated := IsVertical;
    Spacer := SpGetRightAlignedItems(View, nil, IsRotated, VisibleWidth, RightAlignedWidth);
    if Assigned(Spacer) then begin
      // Resize the spacer
      if IsRotated then
        I := CurrentDock.Height - GetRightAlignMargin - (VisibleWidth - (Spacer.BoundsRect.Bottom - Spacer.BoundsRect.Top))
      else
        I := CurrentDock.Width - GetRightAlignMargin - (VisibleWidth - (Spacer.BoundsRect.Right - Spacer.BoundsRect.Left));

      if I < 0 then I := 0;
      Spacer.Item.CustomWidth := I;
    end;
    View.UpdatePositions;
  finally
    View.EndUpdate;
    FState := FState - [tstRightAligning];
  end;
end;

function TSpTBXToolbar.GetChevronItemClass: TTBChevronItemClass;
begin
  Result := TSpTBXChevronItem;
end;

function TSpTBXToolbar.GetFloatingBorderSize: TPoint;
var
  Details: TThemedElementDetails;
  ElementSize: TSize;
begin
  Result := inherited GetFloatingBorderSize;

  case SkinManager.GetSkinType of
    sknSkin:
      Result := Point(CurrentSkin.FloatingWindowBorderSize, CurrentSkin.FloatingWindowBorderSize);
    sknDelphiStyle:
      begin
        Details := SpTBXThemeServices.GetElementDetails(twSmallFrameBottomActive);
        ElementSize := CurrentSkin.GetThemedElementSize(Canvas, Details);
        Result.Y := ElementSize.cy;

        Details := SpTBXThemeServices.GetElementDetails(twSmallFrameLeftActive);
        ElementSize := CurrentSkin.GetThemedElementSize(Canvas, Details);
        Result.X := ElementSize.cx;
      end;
  end;
end;

function TSpTBXToolbar.GetFloatingWindowParentClass: TTBFloatingWindowParentClass;
begin
  Result := TSpTBXFloatingWindowParent;
end;

function TSpTBXToolbar.GetRightAlignMargin: Integer;
begin
  if IsVertical then
    Result := NonClientHeight
  else
    Result := NonClientWidth;
end;

function TSpTBXToolbar.GetViewClass: TTBToolbarViewClass;
begin
  Result := TSpTBXToolbarView;
end;

function TSpTBXToolbar.GetItemsTextColor(State: TSpTBXSkinStatesType): TColor;
begin
  if vsMenuBar in View.Style then
    Result := CurrentSkin.GetTextColor(skncMenuBarItem, State)
  else
    Result := CurrentSkin.GetTextColor(skncToolbarItem, State);
end;

function TSpTBXToolbar.IsVertical: Boolean;
begin
  Result := SpIsVerticalToolbar(Self);
end;

procedure TSpTBXToolbar.InternalDrawBackground(ACanvas: TCanvas; ARect: TRect;
  PaintOnNCArea: Boolean; PaintBorders: Boolean = True);
begin
  SpDrawXPToolbar(Self, ACanvas, ARect, PaintOnNCArea, PaintBorders and (BorderStyle <> bsNone));
end;

procedure TSpTBXToolbar.DrawNCArea(const DrawToDC: Boolean; const ADC: HDC;
  const Clip: HRGN);
var
  DC: HDC;
  R, ExcludeR: TRect;
  GripSize: Integer;
  ACanvas: TCanvas;
begin
  if (csDestroying in ComponentState) or not Docked or not HandleAllocated then Exit;

  if not DrawToDC then DC := GetWindowDC(Handle)
  else DC := ADC;
  try
    GetWindowRect(Handle, R);
    OffsetRect(R, -R.Left, -R.Top);
    GripSize := SpGetDragHandleSize(Self);

    if not DrawToDC then begin
      SelectNCUpdateRgn(Handle, DC, Clip);
      ExcludeR := R;
      InflateRect(ExcludeR, -DefaultToolbarBorderSize, -DefaultToolbarBorderSize);
      if IsVertical then
        Inc(ExcludeR.Top, GripSize)
      else
        Inc(ExcludeR.Left, GripSize);
      ExcludeClipRect(DC, ExcludeR.Left, ExcludeR.Top, ExcludeR.Right, ExcludeR.Bottom);
    end;

    ACanvas := TCanvas.Create;
    try
      ACanvas.Handle := DC;

      // Paint the background and borders
      InternalDrawBackground(ACanvas, R, True);

      // Paint the grip and close button
      SpDrawXPToolbarGrip(Self, ACanvas, R);
    finally
      ACanvas.Handle := 0;
      ACanvas.Free;
    end;
  finally
    if not DrawToDC then ReleaseDC(Handle, DC);
  end;
end;

procedure TSpTBXToolbar.CMHintShow(var Message: TCMHintShow);
// Dispatch the message to the Item Viewer.
// TSpTBXItemViewer will handle CM_HINTSHOW message to show unicode hints using
// a custom THintWindow.
begin
  with Message.HintInfo^ do begin
    HintStr := '';
    if Assigned(View.Selected) then begin
      CursorRect := View.Selected.BoundsRect;
      HintStr := View.Selected.GetHintText;
      View.Selected.Dispatch(Message);
    end;
  end;
end;

procedure TSpTBXToolbar.CMControlChange(var Message: TCMControlChange);
begin
  // When a control is dropped on the toolbar a TTBControlItem is created by
  // TTBCustomToolbar.CreateWrapper, unfortunately it is created with the
  // Self.Owner instead of the Form (Owner.Owner for CompoundToolbars like
  // the TabToolbar or StatusToolbar).

  if CompoundToolbar and Message.Inserting and not(csLoading in ComponentState) and
    not (csUpdating in ComponentState) then
  begin
    CreateWrapper(Items.Count, Message.Control);
  end
  else
    inherited;
end;

function TSpTBXToolbar.CreateWrapper(Index: Integer; Ctl: TControl): TTBControlItem;
// CreateWrapper is used only when CompoundToolbar is true, otherwise the
// wrapper is created by TB2K
var
  I: Integer;
  S: String;
  C: TComponent;
  ItemsInterface: ITBItems;
begin
  Result := nil;
  if SpFindControlItem(Items, Ctl) <> nil then Exit;

  C := Owner.Owner;  // The Form
  Result := TTBControlItem.Create(C);
  Result.Control := Ctl;
  if (csDesigning in ComponentState) and Assigned(C) then begin
    { Needs a name for compatibility with form inheritance }
    I := 1;
    while True do begin
      S := Format('TBControlItem%d', [I]);
      if C.FindComponent(S) = nil then
        Break;
      Inc(I);
    end;
    Result.Name := S;
  end;

  if CompoundToolbar then begin
    if Assigned(Owner) and Owner.GetInterface(ITBItems, ItemsInterface) then begin
      if Index > ItemsInterface.GetItems.Count then
        Index := 0;
      ItemsInterface.GetItems.Insert(Index, Result)
    end;
  end
  else
    Items.Insert(Index, Result);
end;

procedure TSpTBXToolbar.CMDialogChar(var Message: TCMDialogChar);
var
  I: Integer;
begin
  if Enabled and Visible then
    for I := 0 to Items.Count - 1 do
      if Items[I] is TSpTBXCustomItem then
        if TSpTBXCustomItem(Items[I]).DialogChar(Message.CharCode) then begin
          Message.Result := 1;
          Exit;
        end;
  inherited;
end;

procedure TSpTBXToolbar.CMMouseleave(var Message: TMessage);
begin
  inherited;
  if IsCustomizing and FCustomizable then begin
    // Clear the last DropMark
    InvalidateRect(Handle, @FLastDropMark, True);
  end;
end;

procedure TSpTBXToolbar.CMTextChanged(var Message: TMessage);
begin
  inherited;
  if Floating then
    TSpTBXFloatingWindowParent(Parent).RedrawCloseButton;
end;

procedure TSpTBXToolbar.WMSpSkinChange(var Message: TMessage);
begin
  if HandleAllocated and not Floating then
    RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_FRAME);
end;

procedure TSpTBXToolbar.WMEraseBkgnd(var Message: TWMEraseBkgnd);
// Same as TSpTBXToolWindow.WMEraseBkgnd
//var
//  ACanvas: TCanvas;
//  R: TRect;
begin
  if (csDestroying in ComponentState) then Exit;

  Message.Result := 1;
//  ACanvas := TCanvas.Create;
//  ACanvas.Handle := Message.DC;
//  try
//    R := ClientRect;
//    if Docked then begin
//      InflateRect(R, DefaultToolbarBorderSize, DefaultToolbarBorderSize);
//      if IsVertical then
//        Dec(R.Top, SpGetDragHandleSize(Self))
//      else
//        Dec(R.Left, SpGetDragHandleSize(Self));
//    end;
//
//    InternalDrawBackground(ACanvas, R, False);
//  finally
//    ACanvas.Handle := 0;
//    ACanvas.Free;
//  end;
end;

procedure TSpTBXToolbar.WMSize(var Message: TWMSize);
var
  I: Integer;
  V: TTBItemViewer;
  R: TRect;
begin
  inherited;

  if Docked and ((CurrentDock is TSpTBXDock) and not TSpTBXDock(CurrentDock).FResizing) then begin
    for I := 0 to View.ViewerCount - 1 do begin
      V := View.Viewers[I];
      if V.Show and not IsRectEmpty(V.BoundsRect) and not (V.Item is TTBControlItem) then
        View.Invalidate(V);
    end;
    Self.Update;
    InvalidateRect(Handle, nil, True);
    for I := 0 to View.ViewerCount - 1 do begin
      V := View.Viewers[I];
      if V.Show and not IsRectEmpty(V.BoundsRect) and not (V.Item is TTBControlItem) then begin
        R := V.BoundsRect;
        ValidateRect(Handle, @R);
      end;
    end;
  end;
end;

procedure TSpTBXToolbar.SetCustomizable(const Value: Boolean);
begin
  if FCustomizable <> Value then begin
    if FMenuBar then
      FCustomizable := False
    else
      FCustomizable := Value;
  end;
end;

procedure TSpTBXToolbar.SetDisplayMode(const Value: TSpTBXToolbarDisplayMode);
begin
  if FDisplayMode <> Value then begin
    FDisplayMode := Value;
    if Value = tbdmImageAboveCaption then
      Options := Options + [tboImageAboveCaption, tboSameWidth]
    else
      Options := Options - [tboImageAboveCaption, tboSameWidth];
    View.UpdatePositions;
  end;
end;

function TSpTBXToolbar.GetMaxSize: Integer;
begin
  if Assigned(View) then
    Result := TSpTBXToolbarView(View).MaxSize
  else
    Result := -1;
end;

procedure TSpTBXToolbar.SetMaxSize(const Value: Integer);
begin
  if Assigned(View) then
    TSpTBXToolbarView(View).MaxSize := Value;
end;

procedure TSpTBXToolbar.SetMenuBar(const Value: Boolean);
begin
  inherited MenuBar := Value;
  FMenuBar := inherited MenuBar;
  FCustomizable := not FMenuBar;
end;

procedure TSpTBXToolbar.BeginUpdate;
begin
  TSpTBXToolbarView(View).BeginUpdate;
end;

procedure TSpTBXToolbar.EndUpdate;
begin
  TSpTBXToolbarView(View).EndUpdate;
end;

function TSpTBXToolbar.IsUpdating: Boolean;
begin
  Result := TSpTBXToolbarView(View).IsUpdating;
end;

procedure TSpTBXToolbar.BeginItemMove;
begin
  Inc(FItemMovingCount);
end;

procedure TSpTBXToolbar.EndItemMove;
begin
  Dec(FItemMovingCount);
  if FItemMovingCount < 0 then FItemMovingCount := 0;
end;

function TSpTBXToolbar.IsItemMoving: Boolean;
begin
  Result := FItemMovingCount > 0;
end;

procedure TSpTBXToolbar.BeginCustomize;
begin
  Inc(FCustomizingCount);
end;

procedure TSpTBXToolbar.EndCustomize;
begin
  Dec(FCustomizingCount);
  if FCustomizingCount < 0 then FCustomizingCount := 0;
end;

function TSpTBXToolbar.IsCustomizing: Boolean;
begin
  Result := FCustomizingCount > 0;
end;

function TSpTBXToolbar.CanDragCustomize(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
var
  IV: TTBItemViewer;
begin
  Result := False;
  FBeginDragIV := nil;

  if not (csDesigning in ComponentState) and IsCustomizing then begin
    Result := True;
    if FCustomizable then begin
      IV := SpGetItemViewerFromPoint(Items, View, Point(X, Y));
      if Assigned(IV) and Assigned(IV.Item) and not (IV.Item is TTBChevronItem) then begin
        FBeginDragIV := IV;
        BeginDrag(True);
      end;
    end;
  end;
end;

function TSpTBXToolbar.CanItemClick(Item: TTBCustomItem; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := True;
end;

procedure TSpTBXToolbar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Item: TTBCustomItem;
begin
  inherited;

  // Handle the Toolbar items hints
  // Set the Toolbar.Hint to change the Application.Hint when the
  // mouse is over the Item.
  // From TB2Toolbar.MouseMove
  if not (csDesigning in ComponentState) then begin
    if Assigned(View.Selected) then begin
      Item := View.Selected.Item;
      if not (tboLongHintInMenuOnly in Item.EffectiveOptions) then
        if Item is TSpTBXCustomItem then
          Hint := TSpTBXCustomItem(Item).Hint
        else
          Hint := Item.Hint;
      // Send MouseMove to the selected viewer (with TB2K MouseMove is only
      // called when the modal handler is active, we need this on the toolbar
      // for the tab close button)
      if View.Selected is TSpTBXItemViewer then
        TSpTBXItemViewer(View.Selected).InternalMouseMove(Shift, X, Y);
    end;
  end;
end;

procedure TSpTBXToolbar.Paint;
var
  R: TRect;
  I : Integer;
begin
  R := ClientRect;
  if Docked then begin
    InflateRect(R, DefaultToolbarBorderSize, DefaultToolbarBorderSize);
    if IsVertical then
      Dec(R.Top, SpGetDragHandleSize(Self))
    else
      Dec(R.Left, SpGetDragHandleSize(Self));
  end;
  InternalDrawBackground(Canvas, R, False);

  // the following is to avoid painting OffScreen in TTBView.DrawItem
  // this is not necessary since SpTBXToolbar is DoubleBuffered
  for I := 0 to View.ViewerCount - 1 do
    Exclude(View.Viewers[i].State, tbisInvalidated);
  inherited;
end;

procedure TSpTBXToolbar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  OldParent: TWinControl;
  CurrentPos, OldPos: TPoint;
  Item: TTBCustomItem;
begin
  if not (csDesigning in ComponentState) and not CanDragCustomize(Button, Shift, X, Y) then begin
    OldParent := Parent;
    OldPos := ClientToScreen(Point(Left, Top));
    if Assigned(View.Selected) then
      Item := View.Selected.Item
    else
      Item := nil;

    if CanItemClick(Item, Button, Shift, X, Y) then
      inherited;

    // Check if the Parent was changed due to the toolbar moving between docks
    if (Parent = OldParent) and Assigned(View.Selected) then begin
      // Check if the toolbar was moved across the screen
      CurrentPos := ClientToScreen(Point(Left, Top));
      if (CurrentPos.X = OldPos.X) and (CurrentPos.Y = OldPos.Y) then
        DoItemClick(View.Selected.Item, Button, Shift, X, Y); // Extra click processing
    end;
  end;
end;

procedure TSpTBXToolbar.DoStartDrag(var DragObject: TDragObject);
begin
  if IsCustomizing and FCustomizable and Assigned(FBeginDragIV) and Assigned(FBeginDragIV.Item) then begin
    // TSpTBXItemDragObject will be automatically destroyed since it's
    // a descendant of TDragObjectEx.
    // It's destroyed on Controls.DragDone
    DragObject := TSpTBXItemDragObject.Create(Self, FBeginDragIV.Item);
    inherited DoStartDrag(DragObject);
  end;
end;

procedure TSpTBXToolbar.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  DestIV: TTBItemViewer;
  DestPos: Integer;
  DropMark: TRect;
begin
  inherited DragOver(Source, X, Y, State, Accept);

  if IsCustomizing and FCustomizable then begin
    Accept := True;
    SpGetDropPosItemViewer(Items, View, Point(X, Y), DestIV, DestPos, DropMark);
    if not EqualRect(DropMark, FLastDropMark) then begin
      // Clear the last DropMark
      InvalidateRect(Handle, @FLastDropMark, True);
      // Draw the new DropMark
      SpDrawDropMark(Canvas, DropMark);
      FLastDropMark := DropMark;
    end;
  end;
end;

procedure TSpTBXToolbar.DragDrop(Source: TObject; X, Y: Integer);
var
  D: TSpTBXItemDragObject;
  DestIV: TTBItemViewer;
  OrigItem: TTBCustomItem;
  OrigPos, DestPos: Integer;
  DropMark: TRect;
begin
  if Assigned(Source) and (Source is TSpTBXItemDragObject) then begin
    D := Source as TSpTBXItemDragObject;
    OrigItem := D.SouceItem;
    OrigPos := OrigItem.Parent.IndexOf(OrigItem);

    // Get the destination item position
    if X < 0 then X := 0;
    if Y < 0 then Y := 0;
    SpGetDropPosItemViewer(Items, View, Point(X, Y), DestIV, DestPos, DropMark);
    if OrigItem.Parent = Items then begin
      if DestPos > OrigPos then
        dec(DestPos);
      if (OrigPos = DestPos) then begin
        // Clear the last DropMark
        InvalidateRect(Handle, @FLastDropMark, True);
        Exit;
      end;
    end;

    if Assigned(DestIV) and (DestPos < 0) then Exit;

    // Insert the dragging item to the destination toolbar
    OrigItem.Parent.Remove(OrigItem);
    try
      if Assigned(DestIV) then
        Items.Insert(DestPos, OrigItem)
      else
        Items.Add(OrigItem);

      if OrigItem is TTBControlItem then
        if D.SourceControl <> Self then
          TTBControlItem(OrigItem).Control.Parent := Self;

      OrigItem.Visible := True;
      FLastDropMark := Rect(0, 0, 0, 0);
    except
      OrigItem.Parent.Insert(OrigPos, OrigItem);
    end;
  end;

  inherited;
end;

procedure TSpTBXToolbar.ReadPositionData(const Data: TTBReadPositionData);
begin
  inherited;
  with Data do
    DisplayMode := TSpTBXToolbarDisplayMode(ReadIntProc(Name, rvSpTBXDisplayMode, 0, ExtraData));
end;

procedure TSpTBXToolbar.WritePositionData(const Data: TTBWritePositionData);
begin
  inherited;
  with Data do
    WriteIntProc(Name, rvSpTBXDisplayMode, Integer(DisplayMode), ExtraData);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomToolWindow }

constructor TSpTBXCustomToolWindow.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultToolbarBorderSize := CDefaultToolbarBorderSize;
  FMinClientWidth := 32;
  FMinClientHeight := 32;
  SetBounds(Left, Top, FMinClientWidth, FMinClientHeight);

  Color := clNone;
  SkinManager.AddSkinNotification(Self);
end;

destructor TSpTBXCustomToolWindow.Destroy;
begin
  SkinManager.RemoveSkinNotification(Self);
  inherited;
end;

function TSpTBXCustomToolWindow.CalcSize(ADock: TTBDock): TPoint;
begin
  Result.X := FBarSize.cx;
  Result.Y := FBarSize.cy;
  if Assigned(ADock) and (FullSize or Stretch) then begin
    // If docked and stretching, return the minimum size so that the
    // toolbar can shrink below FBarSize
    if SpIsVerticalToolbar(Self) then
      Result.Y := FMinClientHeight
    else
      Result.X := FMinClientWidth;    
  end;
end;

function TSpTBXCustomToolWindow.DoArrange(CanMoveControls: Boolean;
  PreviousDockType: TTBDockType; NewFloating: Boolean; NewDock: TTBDock): TPoint;
begin
  Result := CalcSize(NewDock);
end;

procedure TSpTBXCustomToolWindow.GetBaseSize(var ASize: TPoint);
begin
  ASize := CalcSize(CurrentDock);
end;

procedure TSpTBXCustomToolWindow.GetMinMaxSize(var AMinClientWidth, AMinClientHeight,
  AMaxClientWidth, AMaxClientHeight: Integer);
begin
  // GetMinMaxSize is used only when the window is floating
  AMinClientWidth := FMinClientWidth;
  AMinClientHeight := FMinClientHeight;
  AMaxClientWidth := FMaxClientWidth;
  AMaxClientHeight := FMaxClientHeight;
end;

function TSpTBXCustomToolWindow.IsVertical: Boolean;
begin
  Result := SpIsVerticalToolbar(Self);
end;

procedure TSpTBXCustomToolWindow.SizeChanging(const AWidth, AHeight: Integer);
begin
  FBarSize.cx := AWidth;
  FBarSize.cy := AHeight;
  if Assigned(Parent) then begin
    Dec(FBarSize.cx, Width - ClientWidth);
    Dec(FBarSize.cy, Height - ClientHeight);
  end;
end;

procedure TSpTBXCustomToolWindow.SetClientAreaSize(AWidth, AHeight: Integer);
var
  R: TRect;
begin
  if Assigned(Parent) then begin
    Windows.GetClientRect(Handle, R);
    SetBounds(Left, Top, Width - R.Right + AWidth, Height - R.Bottom + AHeight);
  end
  else
    SetBounds(Left, Top, AWidth, AHeight);
end;

procedure TSpTBXCustomToolWindow.Paint;
var
  R: TRect;
begin
  // Dotted border in design mode
  if csDesigning in ComponentState then begin
    R := ClientRect;
    Canvas.Pen.Style := psDot;
    Canvas.Pen.Color := clBtnShadow;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
    Canvas.Pen.Style := psSolid;
  end;
end;

procedure TSpTBXCustomToolWindow.InternalDrawBackground(ACanvas: TCanvas;
  ARect: TRect; PaintOnNCArea, PaintBorders: Boolean);
begin
  SpDrawXPToolbar(Self, ACanvas, ARect, PaintOnNCArea, PaintBorders and (BorderStyle <> bsNone));
end;

procedure TSpTBXCustomToolWindow.InvalidateBackground(InvalidateChildren: Boolean);
begin
  // Force background repaint
  if not (csDestroying in ComponentState) and HandleAllocated then
    if InvalidateChildren then
      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN)
    else
      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE);
end;

procedure TSpTBXCustomToolWindow.DrawNCArea(const DrawToDC: Boolean;
  const ADC: HDC; const Clip: HRGN);
var
  DC: HDC;
  R, ExcludeR: TRect;
  GripSize: Integer;
  ACanvas: TCanvas;
begin
  if (csDestroying in ComponentState) or not Docked or not HandleAllocated then Exit;

  if not DrawToDC then DC := GetWindowDC(Handle)
  else DC := ADC;
  try
    GetWindowRect(Handle, R);
    OffsetRect(R, -R.Left, -R.Top);
    GripSize := SpGetDragHandleSize(Self);

    if not DrawToDC then begin
      SelectNCUpdateRgn(Handle, DC, Clip);
      ExcludeR := R;
      InflateRect(ExcludeR, -DefaultToolbarBorderSize, -DefaultToolbarBorderSize);
      if IsVertical then
        Inc(ExcludeR.Top, GripSize)
      else
        Inc(ExcludeR.Left, GripSize);
      ExcludeClipRect(DC, ExcludeR.Left, ExcludeR.Top, ExcludeR.Right, ExcludeR.Bottom);
    end;

    ACanvas := TCanvas.Create;
    try
      ACanvas.Handle := DC;

      // Paint the background and borders
      InternalDrawBackground(ACanvas, R, True);

      // Paint the grip and close button
      SpDrawXPToolbarGrip(Self, ACanvas, R);
    finally
      ACanvas.Handle := 0;
      ACanvas.Free;
    end;
  finally
    if not DrawToDC then ReleaseDC(Handle, DC);
  end;
end;

procedure TSpTBXCustomToolWindow.DoDrawBackground(ACanvas: TCanvas;
  ARect: TRect; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawBackground) then FOnDrawBackground(Self, ACanvas, ARect, PaintStage, PaintDefault);
end;

procedure TSpTBXCustomToolWindow.ReadPositionData(const Data: TTBReadPositionData);
var
  W, H: Integer;
begin
  inherited;
  // Load ClientAreaWidth/ClientAreaHeight
  if Resizable then
    with Data do begin
      W := ReadIntProc(Name, rvClientWidth, FBarSize.cx, ExtraData);
      H := ReadIntProc(Name, rvClientHeight, FBarSize.cy, ExtraData);
      SetClientAreaSize(W, H);
    end;
end;

procedure TSpTBXCustomToolWindow.WritePositionData(const Data: TTBWritePositionData);
begin
  inherited;
  // Save ClientAreaWidth/ClientAreaHeight
  with Data do begin
    WriteIntProc(Name, rvClientWidth, ClientAreaWidth, ExtraData);
    WriteIntProc(Name, rvClientHeight, ClientAreaHeight, ExtraData);
  end;
end;

function TSpTBXCustomToolWindow.GetFloatingBorderSize: TPoint;
var
  Details: TThemedElementDetails;
  ElementSize: TSize;
begin
  Result := inherited GetFloatingBorderSize;

  case SkinManager.GetSkinType of
    sknSkin:
      Result := Point(CurrentSkin.FloatingWindowBorderSize, CurrentSkin.FloatingWindowBorderSize);
    sknDelphiStyle:
      begin
        Details := SpTBXThemeServices.GetElementDetails(twSmallFrameBottomActive);
        ElementSize := CurrentSkin.GetThemedElementSize(Canvas, Details);
        Result.Y := ElementSize.cy;

        Details := SpTBXThemeServices.GetElementDetails(twSmallFrameLeftActive);
        ElementSize := CurrentSkin.GetThemedElementSize(Canvas, Details);
        Result.X := ElementSize.cx;
      end;
  end;
end;

function TSpTBXCustomToolWindow.GetFloatingWindowParentClass: TTBFloatingWindowParentClass;
begin
  Result := TSpTBXFloatingWindowParent;
end;

function TSpTBXCustomToolWindow.GetClientAreaWidth: Integer;
begin
  if Assigned(Parent) then Result := ClientWidth
  else Result := Width;
end;

procedure TSpTBXCustomToolWindow.SetClientAreaWidth(Value: Integer);
begin
  SetClientAreaSize(Value, ClientAreaHeight);
end;

function TSpTBXCustomToolWindow.GetClientAreaHeight: Integer;
begin
  if Assigned(Parent) then Result := ClientHeight
  else Result := Height;
end;

procedure TSpTBXCustomToolWindow.SetClientAreaHeight(Value: Integer);
begin
  SetClientAreaSize(ClientAreaWidth, Value);
end;

procedure TSpTBXCustomToolWindow.CMTextChanged(var Message: TMessage);
begin
  inherited;
  if Floating then
    TSpTBXFloatingWindowParent(Parent).RedrawCloseButton;
end;

procedure TSpTBXCustomToolWindow.WMEraseBkgnd(var Message: TWMEraseBkgnd);
// Same as TSpTBXToolbar.WMEraseBkgnd
var
  ACanvas: TCanvas;
  R: TRect;
begin
  if (csDestroying in ComponentState) then Exit;

  Message.Result := 1;
  ACanvas := TCanvas.Create;
  ACanvas.Handle := Message.DC;
  try
    R := ClientRect;
    if Docked then begin
      InflateRect(R, DefaultToolbarBorderSize, DefaultToolbarBorderSize);
      if IsVertical then
        Dec(R.Top, SpGetDragHandleSize(Self))
      else
        Dec(R.Left, SpGetDragHandleSize(Self));
    end;

    InternalDrawBackground(ACanvas, R, False);
  finally
    ACanvas.Handle := 0;
    ACanvas.Free;
  end;
end;

procedure TSpTBXCustomToolWindow.WMSpSkinChange(var Message: TMessage);
begin
  if HandleAllocated and not Floating then
    RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_FRAME);
end;

procedure TSpTBXCustomToolWindow.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  InvalidateBackground;
  if (Message.WindowPos.flags and SWP_NOSIZE) = 0 then begin
    Realign;
    Update;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXFloatingWindowParent }

constructor TSpTBXFloatingWindowParent.CreateNew(AOwner: TComponent;
  Dummy: Integer);
begin
  inherited;
  ScreenSnap := True;
  SkinManager.AddSkinNotification(Self);
end;

procedure TSpTBXFloatingWindowParent.CreateWnd;
begin
  inherited;
  UpdateDwmNCSize;
end;

destructor TSpTBXFloatingWindowParent.Destroy;
begin
  SkinManager.RemoveSkinNotification(Self);
  inherited;
end;

procedure TSpTBXFloatingWindowParent.DrawNCArea(const DrawToDC: Boolean;
  const ADC: HDC; const Clip: HRGN; RedrawWhat: TTBToolWindowNCRedrawWhat);
const
  Pattern: array [0..15] of Byte = ($C6, 0, $EE, 0, $7C, 0, $38, 0, $7C, 0, $EE, 0, $C6, 0, 0, 0);
var
  DC: HDC;
  R, CaptionR, CloseR: TRect;
  ACanvas: TCanvas;
  DockWindow: TTBCustomDockableWindowAccess;
  FloatingBorderSize: TPoint;
  WideCaption: WideString;
  IsActive: Boolean;
  Details: TThemedElementDetails;
  CloseButtonWidth: Integer;
  SkinState: TSpTBXSkinStatesType;
  PatternColor: TColor;
begin
  if not HandleAllocated then Exit;
  if not DrawToDC then DC := GetWindowDC(Handle)
  else DC := ADC;
  try
    if not DrawToDC then SelectNCUpdateRgn(Handle, DC, Clip);
    GetWindowRect(Handle, R);
    OffsetRect(R, -R.Left, -R.Top);
    with R do IntersectClipRect(DC, Left, Top, Right, Bottom);
    ACanvas := TCanvas.Create;
    try
      ACanvas.Handle := DC;
      ACanvas.Brush.Color := Color; // SpDrawXPTitleBarBody needs it to paint the background
      GetWindowRect(Handle, R);
      OffsetRect(R, -R.Left, -R.Top);
      DockWindow := TTBCustomDockableWindowAccess(DockableWindow);
      FloatingBorderSize := DockWindow.GetFloatingBorderSize;
      IsActive := not DockWindow.InactiveCaption;

      // Borders
      if twrdBorder in RedrawWhat then
        SpDrawXPTitleBarBody(ACanvas, R, IsActive, FloatingBorderSize, False);

      // Caption
      if DockWindow.ShowCaption then begin
        R.Bottom := R.Top + FloatingBorderSize.Y + GetSystemMetrics(SM_CYSMCAPTION);
        if SkinManager.GetSkinType in [sknWindows, sknDelphiStyle] then begin
          if twrdBorder in RedrawWhat then
            SpDrawXPTitleBar(ACanvas, R, IsActive, False);
          InflateRect(R, -FloatingBorderSize.X, 0);
          R.Top := R.Top + FloatingBorderSize.Y;
        end
        else begin
          InflateRect(R, -FloatingBorderSize.X, 0);
          R.Top := R.Top + FloatingBorderSize.Y;
          if twrdBorder in RedrawWhat then
            SpDrawXPTitleBar(ACanvas, R, IsActive, False);
        end;

        // Text
        if twrdCaption in RedrawWhat then begin
          CaptionR := R;
          OffsetRect(CaptionR, 2, 0);
          if DockWindow.CloseButton then
            Dec(CaptionR.Right, GetSystemMetrics(SM_CYSMCAPTION) + 2);
          ACanvas.Brush.Style := bsClear;
          try
            ACanvas.Font.Assign(SmCaptionFont);
            if IsActive then
              ACanvas.Font.Color := CurrentSkin.GetTextColor(skncWindowTitleBar, sknsNormal)
            else
              ACanvas.Font.Color := CurrentSkin.GetTextColor(skncWindowTitleBar, sknsDisabled);

            if DockableWindow is TSpTBXToolbar then
              WideCaption := TSpTBXToolbar(DockWindow).Caption
            else
              if DockableWindow is TSpTBXCustomToolWindow then
                WideCaption := TSpTBXCustomToolWindow(DockWindow).Caption
              else
                WideCaption := '';
            SpDrawXPText(ACanvas, WideCaption, CaptionR, DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS or DT_HIDEPREFIX);
          finally
            ACanvas.Brush.Style := bsSolid;
          end;
        end;

        // Close button
        if (twrdCloseButton in RedrawWhat) and DockWindow.CloseButton then begin
          CloseR := R;
          Dec(CloseR.Bottom);
          CloseButtonWidth := (CloseR.Bottom - CloseR.Top) - 2 - 2;
          CloseR.Left := CloseR.Right - CloseButtonWidth - 2; // TB2Dock.GetCloseButtonRect
          CloseR.Right := CloseR.Left + CloseButtonWidth;
          CloseR.Top := CloseR.Top + 2;
          CloseR.Bottom := CloseR.Top + CloseButtonWidth;

          case SkinManager.GetSkinType of
            sknNone:
              begin
                if CloseButtonDown then
                  DrawFrameControl(ACanvas.Handle, CloseR, DFC_CAPTION, DFCS_CAPTIONCLOSE or DFCS_PUSHED)
                else
                  DrawFrameControl(ACanvas.Handle, CloseR, DFC_CAPTION, DFCS_CAPTIONCLOSE);
              end;
            sknWindows, sknDelphiStyle:
              begin
                if CloseButtonDown then Details := SpTBXThemeServices.GetElementDetails(twSmallCloseButtonPushed)
                else if FCloseButtonHover then Details := SpTBXThemeServices.GetElementDetails(twSmallCloseButtonHot)
                else Details := SpTBXThemeServices.GetElementDetails(twSmallCloseButtonNormal);
                CurrentSkin.PaintThemedElementBackground(ACanvas, CloseR, Details);
              end;
            sknSkin:
              begin
                SkinState := CurrentSkin.GetState(True, CloseButtonDown, FCloseButtonHover, False);
                PatternColor := CurrentSkin.GetTextColor(skncWindowTitleBar, SkinState);
                if PatternColor = clNone then
                  PatternColor := CurrentSkin.GetTextColor(skncToolbarItem, SkinState);
                CurrentSkin.PaintBackground(ACanvas, CloseR, skncToolbarItem, SkinState, True, True);
                SpDrawGlyphPattern(ACanvas, CloseR, 0, PatternColor);
              end;
          end;
        end;
      end;
    finally
      ACanvas.Handle := 0;
      ACanvas.Free;
    end;
  finally
    if not DrawToDC then ReleaseDC(Handle, DC);
  end;
end;

procedure TSpTBXFloatingWindowParent.RedrawCloseButton;
begin
  if HandleAllocated and IsWindowVisible(Handle) then
    if SkinManager.GetSkinType <> sknNone then
      DrawNCArea(False, 0, 0, [twrdBorder, twrdCaption, twrdCloseButton]);
end;

procedure TSpTBXFloatingWindowParent.UpdateDwmNCSize;
var
  Style: {$IF CompilerVersion >= 23} NativeInt {$ELSE} Integer {$IFEND};
begin
  if HandleAllocated then begin
    // Make sure WS_THICKFRAME is setted only when Windows themes are used with
    // DwmComposition, otherwise borders are incorrectly painted on Vista
    Style := GetWindowLong(Handle, GWL_STYLE);
    if TTBCustomDockableWindowAccess(DockableWindow).Resizable and (SkinManager.GetSkinType = sknWindows) and SpIsDwmCompositionEnabled then
      Style := Style or WS_THICKFRAME
    else
      Style := Style and not WS_THICKFRAME;
    SetWindowLong(Handle, GWL_STYLE, Style);

    // Update the NC area size, CurrentSkin.FloatingWindowBorderSize could have changed
    // Make sure to resize the toolbar
    SpRecalcNCArea(Self);
    if Assigned(DockableWindow) then
      TTBCustomDockableWindowAccess(DockableWindow).Arrange;
    RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_FRAME);
    SpActivateDwmNC(Self, SkinManager.GetSkinType = sknWindows);
  end;
end;

procedure TSpTBXFloatingWindowParent.CancelNCHover;
begin
  if FCloseButtonHover then begin
    FCloseButtonHover := False;
    RedrawCloseButton;
  end;
end;

procedure TSpTBXFloatingWindowParent.VisibleChanging;
begin
  inherited;
  Caption := '';
end;

procedure TSpTBXFloatingWindowParent.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  CancelNCHover;
end;

procedure TSpTBXFloatingWindowParent.WMActivateApp(var Message: TWMActivateApp);
var
  DockWindow: TTBCustomDockableWindowAccess;
begin
  inherited;

  // The floating window is not repainted correctly if HideWhenInactive is
  // false and the application is deactivated/activated.
  if HandleAllocated then begin
    DockWindow := TTBCustomDockableWindowAccess(DockableWindow);
    if not DockWindow.HideWhenInactive then
      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN);
  end;
end;

procedure TSpTBXFloatingWindowParent.WMClose(var Message: TWMClose);
begin
  if FCloseOnAltF4 then begin
    if Assigned(DockableWindow) then
      TTBCustomDockableWindowAccess(DockableWindow).Close;
  end
  else
    inherited;
end;

procedure TSpTBXFloatingWindowParent.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TSpTBXFloatingWindowParent.WMNCMouseLeave(var Message: TMessage);
begin
  if not MouseCapture then CancelNCHover;
  inherited;
end;

procedure TSpTBXFloatingWindowParent.WMNCMouseMove(var Message: TWMNCMouseMove);
var
  InArea: Boolean;
begin
  inherited;
  { Note: TME_NONCLIENT was introduced in Windows 98 and 2000 }
  if (Win32MajorVersion >= 5) or
     (Win32MajorVersion = 4) and (Win32MinorVersion >= 10) then
    CallTrackMouseEvent(Handle, TME_LEAVE or $10 {TME_NONCLIENT});
  InArea := Message.HitTest = 2001; {HT_TB2k_Close}
  if FCloseButtonHover <> InArea then begin
    FCloseButtonHover := InArea;
    RedrawCloseButton;
  end;
end;

procedure TSpTBXFloatingWindowParent.WMSpSkinChange(var Message: TMessage);
begin
  UpdateDwmNCSize;
  inherited;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXPopupWindow }

constructor TSpTBXPopupWindow.CreatePopupWindow(AOwner: TComponent;
  const AParentView: TTBView; const AItem: TTBCustomItem;
  const ACustomizing: Boolean);
begin
  inherited;
  if AItem is TSpTBXCustomItem then
    if Assigned(View) and (View is TSpTBXPopupWindowView) then begin
      if TSpTBXCustomItem(AItem).ToolbarStylePopup then
        TSpTBXPopupWindowView(View).SetIsToolbar(True);
    end;

  FMaximumImageSize := SpGetMenuMaximumImageSize(View);
end;

destructor TSpTBXPopupWindow.Destroy;
begin
  DoPopupShowingChanged(False);

  inherited;
end;

procedure TSpTBXPopupWindow.DoPopupShowingChanged(IsVisible: Boolean);
begin
  if View.ParentItem is TSpTBXCustomItem then
  begin
    TSpTBXCustomItem(View.ParentItem).DoPopupShowingChanged(Self, IsVisible);
    if HandleAllocated then
      RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_ERASE);
  end
  else
    if View.ParentItem is TSpTBXRootItem then
      TSpTBXRootItem(View.ParentItem).DoPopupShowingChanged(Self, IsVisible);
end;

function TSpTBXPopupWindow.GetViewClass: TTBViewClass;
begin
  Result := TSpTBXPopupWindowView;
end;

function TSpTBXPopupWindow.CanDrawGutter: Boolean;
begin
  if View.IsToolbar then
    Result := False
  else begin
    Result := True;
    // Is it a toolbox?
    if Assigned(View) and Assigned(View.ParentItem) then begin
      if View.ParentItem is TSpTBXCustomItem then
        Result := not TSpTBXCustomItem(View.ParentItem).ToolBoxPopup
      else
        if View.ParentItem is TSpTBXRootItem then
          Result := not TSpTBXRootItem(View.ParentItem).ToolBoxPopup;
    end;
  end;
end;

procedure PopupWindowNCPaintProc(Wnd: HWND; DC: HDC; AppData: TObject);
// Paints the NC area and the client background, used by WMEraseBkgnd, WMNCPaint, WMPrint
var
  ACanvas: TCanvas;
  R: TRect;
  PopupWindow: TSpTBXPopupWindow;
begin
  ACanvas := TCanvas.Create;
  try
    ACanvas.Handle := DC;
    GetWindowRect(Wnd, R);
    OffsetRect(R, -R.Left, -R.Top);

    PopupWindow := TSpTBXPopupWindow(AppData);
    // If it's used by WM_ERASEBKGND offset the rect
    if PopupWindow.FPaintingClientArea then begin
      PopupWindow.FPaintingClientArea := False;
      OffsetRect(R, -3, -3);
    end;

    PopupWindow.PaintBackground(ACanvas, R);
  finally
    ACanvas.Handle := 0;
    ACanvas.Free;
  end;
end;

procedure TSpTBXPopupWindow.PaintBackground(ACanvas: TCanvas; ARect: TRect);
// Paints the NC area and the client background, used by WMEraseBkgnd, WMNCPaint, WMPrint
var
  DrawGutter: Boolean;
  OpenIVRect: TRect;
  OpenIV: TTBItemViewer;
  OpenIVSize: Integer;
begin
  OpenIVRect := Rect(0, 0, 0, 0);
  DrawGutter := False;

  if Assigned(View) then begin
    ACanvas.Font.Assign(ToolbarFont);
    DrawGutter := CanDrawGutter;

    if Assigned(View.ParentView) and CurrentSkin.OfficePopup then begin
      OpenIV := TTBViewAccess(View.ParentView).OpenViewer;
      if Assigned(OpenIV) and OpenIV.IsToolbarStyle and
        ((OpenIV is TSpTBXItemViewer) or (OpenIV is TSpTBXChevronItemViewer)) then
      begin
        // Get the OpenIVRect in window coordinates
        OpenIVRect := OpenIV.BoundsRect;
        OpenIVRect.TopLeft := View.ParentView.Window.ClientToScreen(OpenIVRect.TopLeft);
        OpenIVRect.BottomRight := View.ParentView.Window.ClientToScreen(OpenIVRect.BottomRight);
        OpenIVRect.TopLeft := ScreenToClient(OpenIVRect.TopLeft);
        OpenIVRect.BottomRight := ScreenToClient(OpenIVRect.BottomRight);
        OffsetRect(OpenIVRect, 3, 3); // Offset to get it on window coordinates
        OpenIVSize := OpenIVRect.Right - OpenIVRect.Left;

        // Get the Clip rect based on OpenIVRect
        OpenIVRect.Left := OpenIVRect.Left + ARect.Left; // ARect.Left is -3 when called by WMEraseBkgnd and is 0 when called by WMNCPaint and WMPrint
        OpenIVRect.Right := OpenIVRect.Left + OpenIVSize;
      end;
    end;
  end;

  SpDrawXPMenuPopupWindow(ACanvas, ARect, OpenIVRect, DrawGutter, MaximumImageSize.cx);
end;

procedure TSpTBXPopupWindow.CMHintShow(var Message: TCMHintShow);
// Dispatch the message to the Item Viewer.
// TSpTBXItemViewer will handle CM_HINTSHOW message to show unicode hints using
// a custom THintWindow.
begin
  with Message.HintInfo^ do begin
    HintStr := '';
    if Assigned(View.Selected) then begin
      CursorRect := View.Selected.BoundsRect;
      HintStr := View.Selected.GetHintText;
      View.Selected.Dispatch(Message);
    end;
  end;
end;

procedure TSpTBXPopupWindow.CMShowingchanged(var Message: TMessage);
const
  ShowFlags: array[Boolean] of UINT = (
    SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE or SWP_HIDEWINDOW,
    SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE or SWP_SHOWWINDOW);
var
  HideEmptyPopup: Boolean;
begin
  if View.ParentItem is TSpTBXSubmenuItem then
    HideEmptyPopup := TSpTBXSubmenuItem(View.ParentItem).HideEmptyPopup
  else
    HideEmptyPopup := False;

  // When the View is empty the size is set to 0 on TSpTBXPopupWindowView.AutoSize
  // We should disable the animation because it can't animate an empty window
  if HideEmptyPopup and Showing and (View.ViewerCount = 0) then begin
    SetWindowPos(WindowHandle, 0, 0, 0, 0, 0, ShowFlags[Showing]);
  end
  else
    inherited;

  if Visible then
    DoPopupShowingChanged(True);
end;

procedure TSpTBXPopupWindow.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  case SkinManager.GetSkinType of
    sknNone:
      inherited;
    sknWindows, sknDelphiStyle:
      // If it's Windows theme and we're not on Vista do default painting
      if not SpIsWinVistaOrUp and (SkinManager.GetSkinType <> sknDelphiStyle) then
        inherited
      else begin
        Message.Result := 1;
        FPaintingClientArea := True;
        TBEndAnimation(WindowHandle);
        PopupWindowNCPaintProc(Handle, Message.DC, Self);
      end;
    sknSkin:
      begin
        Message.Result := 1;
        FPaintingClientArea := True;
        TBEndAnimation(WindowHandle);
        PopupWindowNCPaintProc(Handle, Message.DC, Self);
      end;
  end;
end;

procedure TSpTBXPopupWindow.WMNCPaint(var Message: TMessage);
var
  DC: HDC;
begin
  DC := GetWindowDC(Handle);
  try
    FPaintingClientArea := False;
    SelectNCUpdateRgn(Handle, DC, HRGN(Message.WParam));
    PopupWindowNCPaintProc(Handle, DC, Self);
  finally
    ReleaseDC(Handle, DC);
  end;
end;

procedure TSpTBXPopupWindow.WMPrint(var Message: TMessage);
begin
  FPaintingClientArea := False;
  HandleWMPrint(Handle, Message, PopupWindowNCPaintProc, Self);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXPopupWindowView }

procedure TSpTBXPopupWindowView.AutoSize(AWidth, AHeight: Integer);
begin
  // When the View is empty the size should be 0
  if (ViewerCount = 0) and (ParentItem is TSpTBXSubmenuItem) then
    if TSpTBXSubmenuItem(ParentItem).HideEmptyPopup then begin
      AWidth := -(PopupMenuWindowNCSize * 2);
      AHeight := -(PopupMenuWindowNCSize * 2);
    end;

  inherited AutoSize(AWidth, AHeight);
end;

procedure TSpTBXPopupWindowView.SetIsToolbar(const Value: Boolean);
begin
  // Change the readonly IsToolbar property using RTTI, the property must
  // be published.
  // Tip from: http://hallvards.blogspot.com/2004/05/hack-1-write-access-to-read-only.html
  PBoolean(Integer(Self) + (Integer(GetPropInfo(TSpTBXPopupWindowView, 'IsToolbar').GetProc) and $00FFFFFF))^ := Value;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXChevronItem }

function TSpTBXChevronItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result := TSpTBXChevronItemViewer;
end;

function TSpTBXChevronItem.GetPopupWindowClass: TTBPopupWindowClass;
begin
  Result := TSpTBXChevronPopupWindow;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXChevronItemViewer }

function TSpTBXChevronItemViewer.GetTextColor(State: TSpTBXSkinStatesType): TColor;
begin
  Result := clNone;
  if IsToolbarStyle then begin
    if View.Window is TSpTBXToolbar then
      Result := TSpTBXToolbar(View.Window).GetItemsTextColor(State);
    if Result = clNone then
      Result := CurrentSkin.GetTextColor(skncToolbarItem, State);
  end
  else
    Result := CurrentSkin.GetTextColor(skncMenuItem, State);
end;

procedure TSpTBXChevronItemViewer.Paint(const Canvas: TCanvas;
  const ClientAreaRect: TRect; IsSelected, IsPushed,
  UseDisabledShadow: Boolean);
const
  Pattern: array[Boolean, 0..15] of Byte = (
    ($CC, 0, $66, 0, $33, 0, $66, 0, $CC, 0, 0, 0, 0, 0, 0, 0),
    ($88, 0, $D8, 0, $70, 0, $20, 0, $88, 0, $D8, 0, $70, 0, $20, 0));
var
  ItemInfo: TSpTBXMenuItemInfo;
  R2: TRect;
  P: PByte;
  W, H: Integer;
  DC: HDC;
  PatternColor: TColor;
begin
  DC := Canvas.Handle;
  SpFillItemInfo(Canvas, Self, ItemInfo);
  SpDrawXPMenuItem(Canvas, ClientAreaRect, ItemInfo);

  // Chevron glyph
  R2 := ClientAreaRect;
  if not ItemInfo.IsVertical then begin
    Inc(R2.Top, 4);
    R2.Bottom := R2.Top + 5;
    W := 8;
    H := 5;
  end
  else begin
    R2.Left := R2.Right - 9;
    R2.Right := R2.Left + 5;
    W := 5;
    H := 8;
  end;
  if ItemInfo.Pushed then OffsetRect(R2, 1, 1);

  if SkinManager.GetSkinType = sknSkin then
    PatternColor := GetTextColor(ItemInfo.State)
  else
    PatternColor := clBtnText;
  P := @Pattern[ItemInfo.IsVertical][0];
  if ItemInfo.Enabled then
    SpDrawGlyphPattern(DC, R2, W, H, P^, PatternColor)
  else begin
    OffsetRect(R2, 1, 1);
    SpDrawGlyphPattern(DC, R2, W, H, P^, clBtnHighlight);
    OffsetRect(R2, -1, -1);
    SpDrawGlyphPattern(DC, R2, W, H, P^, clBtnShadow);
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXChevronPopupWindow }

procedure TSpTBXChevronPopupWindow.CMColorChanged(var Message: TMessage);
var
  V: TSpTBXPopupWindowView;
  PV: TTBView;
begin
  // The private FIsToolbar field of the ChevronItem is setted to True
  // in TTBCustomItem.CreatePopup, we need to reset it to False before
  // the Popup is showed.
  // TTBCustomItem.CreatePopup changes the PopupWindow color to clBtnFace
  // after it changes the FIsToolbar value (and before it is visible),
  // that's why we are trapping CM_COLORCHANGED to reset the field.

  inherited;
  if Assigned(View) and (View is TSpTBXPopupWindowView) then begin
    V := TSpTBXPopupWindowView(View);
    PV := V.ParentView;
    // Do we really need to change it?
    if (Color = clBtnFace) and V.IsToolbar and Assigned(PV) and
      Assigned(PV.Owner) and (PV.Owner is TSpTBXToolbar) and
      (TSpTBXToolbar(PV.Owner).ChevronVertical) then
    begin
      V.SetIsToolbar(False);
      Color := clMenu;
    end;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXPopupMenu }

function TSpTBXPopupMenu.GetOnClosePopup: TNotifyEvent;
begin
  Result := TSpTBXRootItem(Items).OnClosePopup;
end;

function TSpTBXPopupMenu.GetOnInitPopup: TSpTBXPopupEvent;
begin
  Result := TSpTBXRootItem(Items).OnInitPopup;
end;

function TSpTBXPopupMenu.GetRootItemClass: TTBRootItemClass;
begin
  Result := TSpTBXRootItem;
end;

function TSpTBXPopupMenu.InternalPopup(X, Y: Integer; ForceFocus: Boolean;
  PopupControl: TControl = nil): Boolean;
var
  P: TPoint;
  PopupControlRect: TRect;
  WinPopupControl: TWinControl;
  Msg: TMessage;
begin
  Result := True;
  FClickedItem := nil;
  P := Point(X, Y);
  SetPopupPoint(P);
  WinPopupControl := nil;
  if Assigned(PopupControl) and Assigned(PopupControl.Parent) then begin
    PopupControlRect := PopupControl.BoundsRect;
    PopupControlRect.TopLeft := PopupControl.Parent.ClientToScreen(PopupControlRect.TopLeft);
    PopupControlRect.BottomRight := PopupControl.Parent.ClientToScreen(PopupControlRect.BottomRight);
    if PopupControl is TWinControl then
      WinPopupControl := PopupControl as TWinControl;
  end
  else
    PopupControlRect := Rect(X, Y, X, Y);

  P.X := PopupControlRect.Left;
  P.Y := PopupControlRect.Bottom;
  TSpTBXRootItem(Items).ToolBoxPopup := ToolBoxPopup;
  FClickedItem := Items.Popup(P.X, P.Y, TrackButton = tbRightButton,
    TTBPopupAlignment(Alignment), FReturnClickedItemOnly);

  if Assigned(WinPopupControl) then begin
    // Send a message to the PopupControl and it's children controls
    // to inform that the Popup is closed.
    Msg.Msg := CM_SPPOPUPCLOSE;
    Msg.WParam := WPARAM(Self);
    Msg.LParam := 0;
    Msg.Result := 0;
    PostMessage(WinPopupControl.Handle, Msg.Msg, Msg.WParam, Msg.LParam);
    WinPopupControl.Broadcast(Msg);
  end;
end;

procedure TSpTBXPopupMenu.Popup(X, Y: Integer);
begin
  PopupEx(X, Y);
end;

function TSpTBXPopupMenu.PopupEx(X, Y: Integer; PopupControl: TControl = nil;
  ReturnClickedItemOnly: Boolean = False): TTBCustomItem;
begin
  FReturnClickedItemOnly := ReturnClickedItemOnly;
  try
    InternalPopup(X, Y, False, PopupControl);
    Result := FClickedItem;
  finally
    FReturnClickedItemOnly := False;
  end;
end;

procedure TSpTBXPopupMenu.SetOnClosePopup(const Value: TNotifyEvent);
begin
  TSpTBXRootItem(Items).OnClosePopup := Value;
end;

procedure TSpTBXPopupMenu.SetOnInitPopup(const Value: TSpTBXPopupEvent);
begin
  TSpTBXRootItem(Items).OnInitPopup := Value;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCompoundItemsControl }

constructor TSpTBXCompoundItemsControl.Create(AOwner: TComponent);
begin
  inherited;

  FDock := GetDockClass.Create(Self);
  FDock.Parent := Self;
  FDock.OnRequestDock := DockRequestDock;

  FToolbar := GetToolbarClass.Create(Self);
  FToolbar.CompoundToolbar := True;
  FToolbar.Parent := FDock;
  FToolbar.CurrentDock := FDock;
  FToolbar.Name := Name + 'Toolbar';
  FToolbar.Customizable := False;
  FToolbar.BorderStyle := bsNone;
  FToolbar.DockMode := dmCannotFloatOrChangeDocks;
  FToolbar.DragHandleStyle := dhNone;
  FToolbar.Stretch := True;
  FToolbar.ShrinkMode := tbsmNone;
  FToolbar.ShowCaption := False;

  SkinManager.AddSkinNotification(Self);
end;

procedure TSpTBXCompoundItemsControl.CreateParams(var Params: TCreateParams);
begin
  // Disable complete redraws when size changes. CS_HREDRAW and CS_VREDRAW
  // cause flicker and are not necessary for this control at run time
  // Invalidate in WMWindowPosChanged message instead.
  inherited CreateParams(Params);
  if not (csDesigning in ComponentState) then begin
    with Params do
      Style := Style or WS_CLIPCHILDREN;
    with Params do
      WindowClass.Style := WindowClass.Style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

destructor TSpTBXCompoundItemsControl.Destroy;
begin
  SkinManager.RemoveSkinNotification(Self);
  FToolbar.Free;
  FDock.Free;
  inherited;
end;

procedure TSpTBXCompoundItemsControl.Loaded;
var
  I: Integer;
  C: TControl;
  DesignerRootItem: TTBCustomItem;
begin
  inherited;

  // The parent of TTBControlItem.Control should be the toolbar, not Self
  // (as setted in GetChildren for dfm streaming).
  DesignerRootItem := GetItems;
  for I := 0 to DesignerRootItem.Count - 1 do
    if DesignerRootItem[I] is TTBControlItem then begin
      C := TTBControlItem(DesignerRootItem[I]).Control;
      if Assigned(C) and (C.Parent <> FToolbar) then
        C.Parent := FToolbar;
    end;
end;

procedure TSpTBXCompoundItemsControl.DockRequestDock(Sender: TObject;
  Bar: TTBCustomDockableWindow; var Accept: Boolean);
begin
  if Assigned(FToolbar) then Accept := Bar = FToolbar;
end;

procedure TSpTBXCompoundItemsControl.GetChildren(Proc: TGetChildProc;
  Root: TComponent);
var
  I: Integer;
  C: TControl;
  DesignerRootItem: TTBCustomItem;
begin
  // Needed to fake the DFM streaming system because the owner of the items
  // is the Form and not the Toolbar nor Self.
  // But the parent must be the Toolbar.
  // GetChildren is used to pass the children components of Self to the DFM
  // streaming system.
  // We also need to do the same with the controls of TTBControlItems.
  // More info on the Delphi help or Classes.TWriter.WriteData
  // Same as TSpTBXCompoundItemsControl and TSpTBXCustomDockablePanel

  DesignerRootItem := GetItems;
  TTBCustomItemAccess(DesignerRootItem).GetChildren(Proc, Root);
  for I := 0 to DesignerRootItem.Count - 1 do
    if (DesignerRootItem[I] is TTBControlItem) then begin
      C := TTBControlItem(DesignerRootItem[I]).Control;
      if Assigned(C) then
        if SpFindControl(Self, C) = -1 then Proc(C);
    end;
  inherited;
end;

function TSpTBXCompoundItemsControl.GetItems: TTBCustomItem;
begin
  Result := FToolbar.Items;
end;

function TSpTBXCompoundItemsControl.GetRootItems: TTBRootItem;
begin
  Result := FToolbar.Items;
end;

function TSpTBXCompoundItemsControl.GetDockClass: TSpTBXDockClass;
begin
  Result := TSpTBXDock;
end;

function TSpTBXCompoundItemsControl.GetToolbarClass: TSpTBXToolbarClass;
begin
  Result := TSpTBXToolbar;
end;

function TSpTBXCompoundItemsControl.GetView: TSpTBXToolbarView;
begin
  Result := FToolbar.View as TSpTBXToolbarView;
end;

function TSpTBXCompoundItemsControl.GetImages: TCustomImageList;
begin
  if Assigned(FToolbar) then
    Result := FToolbar.Images
  else
    Result := nil;
end;

procedure TSpTBXCompoundItemsControl.InvalidateBackground(InvalidateChildren: Boolean);
begin
  // Invalidate will not fire WM_ERASEBKGND, because csOpaque is setted
  if not (csDestroying in ComponentState) and HandleAllocated then
    if InvalidateChildren then
      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN)
    else
      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE);
end;

procedure TSpTBXCompoundItemsControl.SetImages(const Value: TCustomImageList);
begin
  if Assigned(FToolbar) then FToolbar.Images := Value;
end;

procedure TSpTBXCompoundItemsControl.SetName(const Value: TComponentName);
begin
  inherited;
  if Name = Value then
    if Assigned(FToolbar) then
      FToolbar.Name := Name + 'Toolbar';
end;

procedure TSpTBXCompoundItemsControl.WMSpSkinChange(var Message: TMessage);
begin
  InvalidateBackground;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCompoundBar }

constructor TSpTBXCompoundBar.Create(AOwner: TComponent);
begin
  inherited;
  Height := FDock.Height;
  FDock.OnDrawBackground := DrawDockBackground;
  FDock.OnResize := DockResize;
  FToolbar.OnDrawBackground := DrawToolbarBackground;
end;

procedure TSpTBXCompoundBar.DockResize(Sender: TObject);
begin
  if Assigned(FDock) then
    if Height <> FDock.Height then
      Height := FDock.Height;
end;

procedure TSpTBXCompoundBar.DoDrawDockBackground(ACanvas: TCanvas;
  ARect: TRect; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawDockBackground) then FOnDrawDockBackground(Self, ACanvas, ARect, PaintStage, PaintDefault);
end;

procedure TSpTBXCompoundBar.DrawDockBackground(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
var
  InternalPaintDefault: Boolean;
begin
  if PaintStage = pstPrePaint then begin
    // OnDrawBackground is triggered by the Dock and by the docked Toolbar.
    // The Toolbar triggers it only if Dock.ThemedBackground is true, which depends
    // on CurrentTheme.PaintDockBackground, this is done in
    // TTBXToolbar.WMEraseBkgnd.
    ACanvas.Brush.Color := clBtnFace;
    InternalPaintDefault := True;
    DoDrawDockBackground(ACanvas, ARect, pstPrePaint, InternalPaintDefault);
    PaintDefault := InternalPaintDefault;
    InternalPaintDefault := True;
    DoDrawDockBackground(ACanvas, ARect, pstPostPaint, InternalPaintDefault);
  end;
end;

procedure TSpTBXCompoundBar.DrawToolbarBackground(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
begin
  // Let the background be painted by the Dock
  if PaintStage = pstPrePaint then
    PaintDefault := False;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXButtonOptions }

constructor TSpTBXButtonOptions.Create(AParent: TWinControl);
begin
  inherited Create;

  FToolbar := nil;
  if AParent is TSpTBXCompoundItemsControl then
    FToolbar := TSpTBXCompoundItemsControl(AParent).FToolbar;
  FParentControl := AParent;

  FCaption := True;
  FClose := True;
  FMinimize := True;
  FMaximize := True;
  FCaptionImageIndex := -1;
  FCloseImageIndex := -1;
  FMinimizeImageIndex := -1;
  FMaximizeImageIndex := -1;
  FRestoreImageIndex := -1;
  FTitleBarMaxSize := 21;
  FButtonBorders := True;
  CreateButtons;
end;

procedure TSpTBXButtonOptions.CreateButtons;
begin
  FRightAlignSpacer := TSpTBXRightAlignSpacerItem.Create(nil);
  FRightAlignSpacer.FontSettings.Style := FRightAlignSpacer.FontSettings.Style + [fsBold];
  FRightAlignSpacer.Wrapping := twEndEllipsis;
  FToolbar.Items.Add(FRightAlignSpacer);

  FEditableItems := TTBGroupItem.Create(nil);
  FToolbar.Items.Add(FEditableItems);

  FMinimizeButton := TSpTBXItem.Create(nil);
  SetupButton(FMinimizeButton);
  FMinimizeButton.Visible := FMinimize;

  FMaximizeButton := TSpTBXItem.Create(nil);
  SetupButton(FMaximizeButton);
  FMaximizeButton.Visible := FMaximize;

  FCloseButton := TSpTBXItem.Create(nil);
  SetupButton(FCloseButton);
  FCloseButton.Visible := FClose;

  SetTitleBarMaxSize(FTitleBarMaxSize);
end;

procedure TSpTBXButtonOptions.MoveItemToTheLeft(B: TTBCustomItem);
var
  I: Integer;
begin
  I := EditableItems.IndexOf(B);
  if I > -1 then begin
    EditableItems.Delete(I);
    I := FToolbar.Items.IndexOf(RightAlignSpacer);
    FToolbar.Items.Insert(I, B);
  end;
end;

procedure TSpTBXButtonOptions.SetupButton(B: TSpTBXCustomItem);
begin
  B.CustomWidth := 17;
  B.CustomHeight := FTitleBarMaxSize;
  B.DisplayMode := nbdmImageAndText;
  B.OnDrawImage := ButtonsDrawImage;
  B.OnDrawItem := ButtonsDrawItem;
  B.OnClick := ButtonsClick;
  FToolbar.Items.Add(B);
  B.Visible := False;
  SetupButtonIcon(B);
end;

procedure TSpTBXButtonOptions.SetupButtonIcon(B: TSpTBXCustomItem);
var
  Index, GlyphIndex: Integer;
begin
  if Assigned(B) then begin
    Index := -1;
    GlyphIndex := -1;

    if B = FRightAlignSpacer then begin
      Index := FCaptionImageIndex;
    end else
    if B = FCloseButton then begin
      Index := FCloseImageIndex;
      GlyphIndex := 0;
    end else
    if B = FMaximizeButton then begin
      if Restoring(B) then begin
        Index := FRestoreImageIndex;
        GlyphIndex := 3;
      end
      else begin
        Index := FMaximizeImageIndex;
        GlyphIndex := 1;
      end;
    end else
    if B = FMinimizeButton then begin
      if Restoring(B) then begin
        Index := FRestoreImageIndex;
        GlyphIndex := 3;
      end
      else begin
        Index := FMinimizeImageIndex;
        GlyphIndex := 2;
      end;
    end;

    if Index = -1 then begin
      B.Images := MDIButtonsImgList;
      B.ImageIndex := GlyphIndex;
    end
    else begin
      B.Images := nil;
      B.ImageIndex := Index;
    end;
  end;
end;

procedure TSpTBXButtonOptions.UpdateButtonsVisibility;
begin
  FRightAlignSpacer.Visible := FCaption or FClose or FMaximize or FMinimize;
end;

procedure TSpTBXButtonOptions.ButtonsDrawImage(Sender: TObject;
  ACanvas: TCanvas; State: TSpTBXSkinStatesType;
  const PaintStage: TSpTBXPaintStage; var AImageList: TCustomImageList;
  var AImageIndex: Integer; var ARect: TRect; var PaintDefault: Boolean);
begin
  // Empty, useful for descendants
end;

procedure TSpTBXButtonOptions.ButtonsDrawItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; ItemInfo: TSpTBXMenuItemInfo; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
begin
  // [Theme-Change]
  // Don't draw the items background if ButtonBorders is False
  if (PaintStage = pstPrePaint) and not ButtonBorders then
    PaintDefault := False;
end;

procedure TSpTBXButtonOptions.SetCaptionLabel(const Value: WideString);
begin
  if FCaptionLabel <> Value then begin
    FCaptionLabel := Value;
    if Assigned(FRightAlignSpacer) then
      FRightAlignSpacer.Caption := Value;
  end;
end;

procedure TSpTBXButtonOptions.SetCaption(const Value: Boolean);
begin
  FCaption := Value;
  if Assigned(FRightAlignSpacer) then begin
    if Value then
      FRightAlignSpacer.Caption := CaptionLabel
    else
      FRightAlignSpacer.Caption := '';
    UpdateButtonsVisibility;
  end;
end;

procedure TSpTBXButtonOptions.SetClose(const Value: Boolean);
begin
  FClose := Value;
  if Assigned(FCloseButton) then begin
    FCloseButton.Visible := Value;
    UpdateButtonsVisibility;
  end;
end;

procedure TSpTBXButtonOptions.SetMaximize(const Value: Boolean);
begin
  FMaximize := Value;
  if Assigned(FMaximizeButton) then begin
    FMaximizeButton.Visible := Value;
    UpdateButtonsVisibility;
  end;
end;

procedure TSpTBXButtonOptions.SetMinimize(const Value: Boolean);
begin
  FMinimize := Value;
  if Assigned(FMinimizeButton) then begin
    FMinimizeButton.Visible := Value;
    UpdateButtonsVisibility;
  end;
end;

procedure TSpTBXButtonOptions.SetCaptionImageIndex(Value: Integer);
begin
  if Value < 0 then Value := -1;
  FCaptionImageIndex := Value;
  if Assigned(FRightAlignSpacer) then SetupButtonIcon(FRightAlignSpacer);
end;

procedure TSpTBXButtonOptions.SetCloseImageIndex(Value: Integer);
begin
  if Value < 0 then Value := -1;
  FCloseImageIndex := Value;
  if Assigned(FCloseButton) then SetupButtonIcon(FCloseButton);
end;

procedure TSpTBXButtonOptions.SetMinimizeImageIndex(Value: Integer);
begin
  if Value < 0 then Value := -1;
  FMinimizeImageIndex := Value;
  if Assigned(FMinimizeButton) then SetupButtonIcon(FMinimizeButton);
end;

procedure TSpTBXButtonOptions.SetMaximizeImageIndex(Value: Integer);
begin
  if Value < 0 then Value := -1;
  FMaximizeImageIndex := Value;
  if Assigned(FMaximizeButton) then SetupButtonIcon(FMaximizeButton);
end;

procedure TSpTBXButtonOptions.SetRestoreImageIndex(Value: Integer);
begin
  if Value < 0 then Value := -1;
  FRestoreImageIndex := Value;
  SetupButtonIcon(FMinimizeButton);
  SetupButtonIcon(FMaximizeButton);
end;

procedure TSpTBXButtonOptions.SetTitleBarMaxSize(const Value: Integer);
begin
  FTitleBarMaxSize := Value;
  TSpTBXToolbarView(FToolbar.View).MaxSize := Value;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXStatusToolbar }

constructor TSpTBXStatusToolbar.Create(AOwner: TComponent);
begin
  inherited;
  FSizeGrip := True;
end;

destructor TSpTBXStatusToolbar.Destroy;
begin
  FParentForm := nil;
  inherited;
end;

procedure TSpTBXStatusToolbar.DoItemNotification(Ancestor: TTBCustomItem;
  Relayed: Boolean; Action: TTBItemChangedAction; Index: Integer;
  Item: TTBCustomItem);
begin
  inherited;

  if not (csDestroying in ComponentState) and not (csReading in ComponentState) and
    not (tstResizing in FState) and not IsItemMoving then
  begin
    if (Action = tbicInvalidateAndResize) and Assigned(Owner) and (Owner is TSpTBXCustomStatusBar) then
      if NeedsSeparatorRepaint then
        TSpTBXCustomStatusBar(Owner).InvalidateBackground;
  end;
end;

function TSpTBXStatusToolbar.GetParentFormWindowState: TWindowState;
// This method is more accurate than FParentForm.WindowState
var
  R: TRect;
begin
  if not Assigned(FParentForm) then
    FParentForm := GetParentForm(Self);
  Result := SpGetFormWindowState(FParentForm, R);
end;

function TSpTBXStatusToolbar.IsPointInGrip(P: TPoint): Boolean;
var
  GR: TRect;
begin
  Result := False;
  GR := GetGripRect;
  if not IsRectEmpty(GR) and PtInRect(GR, P) then
    Result := True;
end;

function TSpTBXStatusToolbar.GetGripRect: TRect;
var
  C: TWinControl;
  FS: TWindowState;
  HasGrip: Boolean;
begin
  Result := Rect(0, 0, 0, 0);
  if not (csDestroying in ComponentState) and FSizeGrip and Assigned(CurrentDock) then begin
    FS := GetParentFormWindowState; // initializes FParentForm
    if Assigned(FParentForm) and FParentForm.HandleAllocated then begin
      C := SpFindParent(Self, TSpTBXTitleBar);
      if Assigned(C) and TSpTBXTitleBar(C).Active and not TSpTBXTitleBar(C).FixedSize then
        HasGrip := (FS = wsNormal) or not TSpTBXTitleBar(C).MouseActive
      else
        HasGrip := (FS = wsNormal) and (GetWindowLong(FParentForm.Handle, GWL_STYLE) and WS_THICKFRAME <> 0);
      if HasGrip then begin
        Result := CurrentDock.ClientRect;
        Result.Left := Result.Right - GetSystemMetrics(SM_CXVSCROLL);
      end;
    end;
  end;
end;

function TSpTBXStatusToolbar.GetItemsTextColor(State: TSpTBXSkinStatesType): TColor;
begin
  if Assigned(Owner) and (Owner is TSpTBXCustomStatusBar) then
    Result := CurrentSkin.GetTextColor(skncStatusBar, State)
  else
    Result := clNone;
end;

function TSpTBXStatusToolbar.GetRightAlignMargin: Integer;
var
  R: TRect;
begin
  R := GetGripRect;
  Result := R.Right - R.Left;
  if Result = 0 then
    Result := 4;
end;

function TSpTBXStatusToolbar.NeedsSeparatorRepaint: Boolean;
var
  SkinType: TSpTBXSkinType;
begin
  // [Theme-Change]
  // Office themes have rectangle panels, the separator needs
  // to be painted by the Toolbar.
  if Assigned(Owner) and (Owner is TSpTBXCustomStatusBar) then begin
    SkinType := SkinManager.GetSkinType;
    Result := (CurrentSkin.OfficeStatusBar and (SkinType = sknSkin)) or (SkinType = sknNone);
  end
  else
    Result := False;
end;

procedure TSpTBXStatusToolbar.SetSizeGrip(const Value: Boolean);
begin
  if FSizeGrip <> Value then begin
    FSizeGrip := Value;
    if Assigned(Owner) and (Owner is TSpTBXStatusBar) then
      TSpTBXStatusBar(Owner).InvalidateBackground;
  end;
end;

procedure TSpTBXStatusToolbar.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  // Resize the StatusBar if the parent is TSpTBXTitleBar
  if not (csDesigning in ComponentState) and (Button = mbLeft) and Assigned(FParentForm) then begin
    P := Point(X, Y);
    if IsPointInGrip(P) then begin
      ReleaseCapture;
      SendMessage(FParentForm.Handle, WM_SYSCOMMAND, $F008, 0);
      Exit;
    end;
  end;

  inherited;
end;

procedure TSpTBXStatusToolbar.WMNCLButtonDown(var Message: TWMNCLButtonDown);
var
  P: TPoint;
begin
  if not (csDesigning in ComponentState) and Assigned(FParentForm) then begin
    P := ScreenToClient(SmallPointToPoint(TSmallPoint(GetMessagePos())));
    if IsPointInGrip(P) then begin
      ReleaseCapture;
      SendMessage(FParentForm.Handle, WM_SYSCOMMAND, $F008, 0);
      Exit;
    end;
  end;

  inherited;
end;

procedure TSpTBXStatusToolbar.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
begin
  if not (csDesigning in ComponentState) and (Message.CursorWnd = Handle) and
    (Screen.Cursor = crDefault) and Assigned(FParentForm) then
  begin
    GetCursorPos(P);
    P := ScreenToClient(P);
    if IsPointInGrip(P) then begin
      Windows.SetCursor(Screen.Cursors[-8]);
      Message.Result := 1;
      Exit;
    end;
  end;

  inherited;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomStatusBar }

constructor TSpTBXCustomStatusBar.Create(AOwner: TComponent);
begin
  inherited;
  Align := alBottom;
end;

function TSpTBXCustomStatusBar.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
  if Assigned(FDock) and (NewHeight <> FDock.Height) then
    Result := False
  else
    Result := inherited CanResize(NewWidth, NewHeight);
end;

procedure TSpTBXCustomStatusBar.DoDrawDockBackground(ACanvas: TCanvas;
  ARect: TRect; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
var
  G: TRect;
  OfficeSeparators: Boolean;
begin
  inherited DoDrawDockBackground(ACanvas, ARect, PaintStage, PaintDefault);
  if (PaintStage = pstPrePaint) and PaintDefault then begin
    PaintDefault := False;
    G := Toolbar.GetGripRect;
    if not IsRectEmpty(G) then begin
      // When it's called by the Toolbar the Gripper position should be corrected
      if (ARect.Left = -2) and (ARect.Top = -2) then
        OffsetRect(G, -2, -2);
    end;

    OfficeSeparators := Toolbar.NeedsSeparatorRepaint;
    SpDrawXPStatusBar(ACanvas, ARect, G);
    if OfficeSeparators then
      DrawSeparators(ACanvas, ARect);
  end;
end;

procedure TSpTBXCustomStatusBar.DrawSeparators(ACanvas: TCanvas; ARect: TRect);
// Draws Office separators when the skin has OfficeStatusBar set to true
var
  I: Integer;
  IV: TTBItemViewer;
  R: TRect;
  SaveIndex: Integer;
begin
  SaveIndex := SaveDC(ACanvas.Handle);
  try
    for I := 0 to FToolbar.View.ViewerCount - 1 do begin
      IV := FToolbar.View.Viewers[I];
      if (IV is TSpTBXSeparatorItemViewer) and (not TSpTBXSeparatorItem(IV.Item).Blank) then begin
        R := IV.BoundsRect;
        if IsRectEmpty(R) then
          Continue;

        OffsetRect(R, ARect.Left + 2, ARect.Top + 2);
        R.Top := ARect.Top;
        R.Bottom := ARect.Bottom;
        R.Left := ((R.Right + R.Left) div 2) - 2;
        R.Right := R.Left + 3;

        if SkinManager.GetSkinType = sknNone then begin
          ACanvas.Brush.Color := clBtnFace;
          ACanvas.FillRect(R);
          SpDrawLine(ACanvas, R.Left, R.Top, R.Left, R.Bottom, clWindow);
          SpDrawLine(ACanvas, R.Right, R.Top, R.Right, R.Bottom, clBtnShadow);
        end
        else begin
          // For Office XP, when called by the Dock inc Top by 1
          if ARect.Top = 0 then Inc(R.Top);
          SpDrawLine(ACanvas, R.Left, R.Top, R.Left, R.Bottom, clWindow);
          SpDrawLine(ACanvas, R.Right, R.Top, R.Right, R.Bottom, clWindow);
          ExcludeClipRect(ACanvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
        end;
      end;
    end;

    if SkinManager.GetSkinType = sknSkin then begin
      Inc(ARect.Top);
      ACanvas.Brush.Color := clWindow;
      ACanvas.FrameRect(ARect);
    end;
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;
end;

function TSpTBXCustomStatusBar.GetStatusToolbar: TSpTBXStatusToolbar;
begin
  Result := FToolbar as TSpTBXStatusToolbar;
end;

function TSpTBXCustomStatusBar.GetToolbarClass: TSpTBXToolbarClass;
begin
  Result := TSpTBXStatusToolbar;
end;

function TSpTBXCustomStatusBar.GetSizeGrip: Boolean;
begin
  Result := Toolbar.SizeGrip;
end;

procedure TSpTBXCustomStatusBar.SetSizeGrip(const Value: Boolean);
begin
  Toolbar.SizeGrip := Value;
end;

procedure TSpTBXCustomStatusBar.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXTitleToolbar }

function TSpTBXTitleToolbar.GetItemsTextColor(State: TSpTBXSkinStatesType): TColor;
begin
  Result := CurrentSkin.GetTextColor(skncWindowTitleBar, State);
end;

function TSpTBXTitleToolbar.GetRightAlignMargin: Integer;
var
  TitleBar: TSpTBXCustomTitleBar;
begin
  Result := inherited GetRightAlignMargin;

  TitleBar := GetTitleBar;
  if Assigned(TitleBar) and (TitleBar.WindowState = wsMaximized) then
    Result := 0;
end;

function TSpTBXTitleToolbar.GetTitleBar: TSpTBXCustomTitleBar;
begin
  Result := CurrentDock.Parent as TSpTBXCustomTitleBar;
end;

procedure TSpTBXTitleToolbar.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TransparentClick: Boolean;
  F: TCustomForm;
  TitleBar: TSpTBXCustomTitleBar;
  P: TPoint;
  IV: TTBItemViewer;
begin
  // Move the Parent Form if the toolbar client area or an item with
  // tbisClicksTransparent itemstyle is clicked (like a TBXLabelItem)
  if not (csDesigning in ComponentState) then begin
    TitleBar := GetTitleBar;
    F := TitleBar.FParentForm;
    if not Assigned(F) or not Assigned(TitleBar) then Exit;
    if not TitleBar.IsActive then Exit;

    if Assigned(View.Selected) then
      TransparentClick := tbisClicksTransparent in TTBCustomItemAccess(View.Selected.Item).ItemStyle
    else
      TransparentClick := True;

    case Button of
      mbLeft:
        if TransparentClick then begin
          if ssDouble in Shift then begin
            // Maximize or restore when double clicking the toolbar
            if TitleBar.Options.Maximize and not TitleBar.FixedSize then
              TitleBar.Options.MaximizeButton.Click;
          end
          else
            if F.WindowState <> wsMaximized then begin
              // Drag the form when dragging the toolbar
              ReleaseCapture;
              SendMessage(F.Handle, WM_SYSCOMMAND, $F012, 0);
            end;
          Exit; // Do not process transparent clicks
        end
        else
          if (ssDouble in Shift) and TitleBar.Options.SystemMenu then begin
            // Close the form when the system menu button is double clicked
            IV := View.ViewerFromPoint(Point(X, Y));
            if Assigned(IV) and (IV.Item = TitleBar.Options.SystemButton) then begin
              F.Close;
              Exit; // Do not process transparent clicks
            end;
          end;
      mbRight:
        if TransparentClick and TitleBar.Options.SystemMenu then begin
          P := ClientToScreen(Point(X, Y));
          TitleBar.Options.SystemButton.Popup(P.X, P.Y, True);
          Exit; // Do not process transparent clicks
        end;
    end;
  end;

  inherited;
end;

procedure TSpTBXTitleToolbar.WMNCCalcSize(var Message: TWMNCCalcSize);
var
  TitleBar: TSpTBXCustomTitleBar;
begin
  inherited;

  FDefaultToolbarBorderSize := CDefaultToolbarBorderSize;
  if Docked then begin
    TitleBar := GetTitleBar;
    if Assigned(TitleBar) and (TitleBar.WindowState = wsMaximized) then begin
      InflateRect(Message.CalcSize_Params.rgrc[0], 2, 2);
      FDefaultToolbarBorderSize := 0;
    end;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXTitleBarButtonOptions }

constructor TSpTBXTitleBarButtonOptions.Create(AParent: TWinControl);
begin
  FSystemMenu := True;
  FTitleBar := AParent as TSpTBXCustomTitleBar;
  inherited Create(AParent);
end;

procedure TSpTBXTitleBarButtonOptions.CreateButtons;
begin
  FSystemButton := TSpTBXSystemMenuItem.Create(nil);
  SetupButton(FSystemButton);
  FSystemButton.Visible := FSystemMenu;

  inherited;
end;

procedure TSpTBXTitleBarButtonOptions.SetSystemMenu(const Value: Boolean);
begin
  FSystemMenu := Value;
  if Assigned(FSystemButton) then
    FSystemButton.Visible := Value;
end;

procedure TSpTBXTitleBarButtonOptions.ButtonsDrawItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; ItemInfo: TSpTBXMenuItemInfo; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
begin
  inherited;

  // [Theme-Change]
  // Don't draw the items background if ButtonBorders is False or Default theme is used
  if (PaintStage = pstPrePaint) and (not ButtonBorders or SkinManager.IsDefaultSkin) then
    PaintDefault := False;
end;

procedure TSpTBXTitleBarButtonOptions.ButtonsClick(Sender: TObject);
begin
  if not Assigned(FTitleBar.FParentForm) or not FTitleBar.IsActive then Exit;

  if Sender = FSystemButton then
    FSystemButton.ShowSize := not FTitleBar.FixedSize
  else
    if Sender = FMinimizeButton then
      FTitleBar.WindowState := wsMinimized
    else
      if Sender = FCloseButton then
        FTitleBar.FParentForm.Close
      else
        if Sender = FMaximizeButton then begin
          if FTitleBar.WindowState = wsNormal then
            FTitleBar.WindowState := wsMaximized
          else
            FTitleBar.WindowState := wsNormal;
        end;
end;

function TSpTBXTitleBarButtonOptions.Restoring(B: TSpTBXCustomItem): Boolean;
begin
  Result := False;
  if (B = MaximizeButton) and (FTitleBar.WindowState = wsMaximized) then Result := True;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomTitleBar }

constructor TSpTBXCustomTitleBar.Create(AOwner: TComponent);
begin
  inherited;

  FRegion := 0;

  FActive := True;
  FMouseActive := True;
  ControlStyle := ControlStyle + [csAcceptsControls];

  Align := alClient;
  FDock.OnResize := nil;

  FParentForm := GetParentForm(Self);
  FOptions := TSpTBXTitleBarButtonOptions.Create(Self);
  FOptions.CaptionLabel := Caption;
end;

destructor TSpTBXCustomTitleBar.Destroy;
begin
  ChangeTitleBarState(False);
  FOptions.Free;

  if Assigned(FParentForm) and Assigned(FOldParentFormWndProc) then begin
    FParentForm.WindowProc := FOldParentFormWndProc;
    FOldParentFormWndProc := nil;
  end;

  if Assigned(Application) and Assigned(FNewAppWndProc) then begin
    {$IF CompilerVersion >= 23}
    // XE2 and up
    // GetWindowLongPtr/SetWindowLongPtr is defined on Delphi 2009 and up,
    // but it's only needed when compiling on 64 bit.
    SetWindowLongPtr(Application.Handle, GWLP_WNDPROC, LONG_PTR(FOldAppWndProc));
    {$ELSE}
    SetWindowLong(Application.Handle, GWL_WNDPROC, Longint(FOldAppWndProc));
    {$IFEND}
    Classes.FreeObjectInstance(FNewAppWndProc);
    FNewAppWndProc := nil;
  end;

  if FRegion <> 0 then begin
    DeleteObject(FRegion);
    FRegion := 0;
  end;

  inherited;
end;

procedure TSpTBXCustomTitleBar.Loaded;
begin
  inherited;

  // Subclass the ParentForm and Application for the System menu handling
  if not (csDesigning in ComponentState) then begin
    FParentForm := GetParentForm(Self);
    // Hook the ParentForm
    if Assigned(FParentForm) then begin
      FOldParentFormWndProc :=  FParentForm.WindowProc;
      FParentForm.WindowProc := NewParentFormWndProc;
    end;

    // Hook the Application to trap the $0313 message, more info on AppWndProc
    if not (csDesigning in ComponentState) and (Application.Handle <> 0) and
      (Application.MainForm = nil) and (FOldAppWndProc = nil) then
    begin
      // When Application.MainForm asume FParentForm as the MainForm
      {$IF CompilerVersion >= 23}
      // XE2 and up
      // GetWindowLongPtr/SetWindowLongPtr is defined on Delphi 2009 and up,
      // but it's only needed when compiling on 64 bit.
      FOldAppWndProc := Pointer(GetWindowLongPtr(Application.Handle, GWL_WNDPROC));
      FNewAppWndProc := Classes.MakeObjectInstance(AppWndProc);
      SetWindowLongPtr(Application.Handle, GWLP_WNDPROC, LONG_PTR(FNewAppWndProc));
      {$ELSE}
      FOldAppWndProc := Pointer(GetWindowLong(Application.Handle, GWL_WNDPROC));
      FNewAppWndProc := Classes.MakeObjectInstance(AppWndProc);
      SetWindowLong(Application.Handle, GWL_WNDPROC, Longint(FNewAppWndProc));
      {$IFEND}
    end;

    ChangeTitleBarState(Active);
  end;
end;

procedure TSpTBXCustomTitleBar.UpdateSkinMetrics;
begin
  if HandleAllocated then begin
    // Update the NC area size, CurrentSkin.FloatingWindowBorderSize could have changed
    // Make sure to realign the toolbars
    SpRecalcNCArea(Self);
    Realign;
  end;
end;

procedure TSpTBXCustomTitleBar.UpdateRegion;
{$IF CompilerVersion >= 23} //for Delphi XE2 and up
var
  TempRegion: HRGN;
  R: TRect;
{$IFEND}
begin
  {$IF CompilerVersion >= 23} //for Delphi XE2 and up
  // Update the NC region of the Main form when using Delphi styles
  if not Assigned(FParentForm) then Exit;
  FUpdateRegionCalled := True;
  TempRegion := FRegion;
  try
    if (WindowState = wsMaximized) or (SkinManager.GetSkinType = sknSkin) then
      FRegion := 0
    else begin
      R := Rect(0, 0, Width, Height);
      SpTBXThemeServices.GetElementRegion(SpTBXThemeServices.GetElementDetails(twCaptionActive), R, FRegion);
    end;
    SetWindowRgn(FParentForm.Handle, FRegion, True);
  finally
    FUpdateRegionCalled := False;
    if TempRegion <> 0 then
      DeleteObject(TempRegion);
  end;
  {$IFEND}
end;

procedure TSpTBXCustomTitleBar.AppWndProc(var Msg: TMessage);
var
  SystemButtonP: TPoint;
begin
  // Handle undocumented $0313 message, this is sent when the
  // taskbar button of the application is right clicked.
  // When Application.MainFormOnTaskbar = True (Delphi 2007 and above)
  // the message is sent to the Main Form, otherwise it is sent
  // to the Application.
  // More info:
  // http://delphi.about.com/od/vclwriteenhance/a/ttaskbarmenu.htm
  if (Msg.Msg = $0313) and Assigned(FParentForm) and Assigned(Options) then begin
    GetCursorPos(SystemButtonP);
    FOptions.SystemButton.Popup(SystemButtonP.X, SystemButtonP.Y, True);
  end
  else
    if Assigned(FOldAppWndProc) then
      Msg.Result := CallWindowProc(FOldAppWndProc, Application.Handle, Msg.Msg, Msg.wParam, Msg.lParam);
end;

procedure TSpTBXCustomTitleBar.NewParentFormWndProc(var Message: TMessage);
var
  M: TWMSysCommand;
  I: Integer;
  HandleSpaceKey: Boolean;

  MMI: ^TMinMaxInfo;
  MonitorBounds, WorkArea, TaskBarBounds: TRect;
  TaskBarState, TaskBarEdge: Cardinal;
  SystemButtonIV: TTBItemViewer;
  SystemButtonP: TPoint;
begin
  if not Assigned(FParentForm) then Exit;

  case Message.Msg of
    $0313:
      begin
        // Handle undocumented $0313 message, this is sent when the
        // taskbar button of the application is right clicked.
        // When Application.MainFormOnTaskbar = True (Delphi 2007 and above)
        // the message is sent to the Main Form, otherwise it is sent
        // to the Application.
        // More info:
        // http://delphi.about.com/od/vclwriteenhance/a/ttaskbarmenu.htm
        GetCursorPos(SystemButtonP);
        FOptions.SystemButton.Popup(SystemButtonP.X, SystemButtonP.Y, True);
        Exit;
      end;
    WM_GETMINMAXINFO:
      if IsActive and not FFullScreenMaximize then begin
        MMI := Pointer(Message.lParam);
        WorkArea := GetRectOfMonitorContainingWindow(Handle, True);
        MonitorBounds := GetRectOfMonitorContainingWindow(Handle, False);

        // Calculate the Max position and size
        // http://news.jrsoftware.org/news/toolbar2000.thirdparty/msg13127.html
        MMI^.ptMaxPosition.X := WorkArea.Left - MonitorBounds.Left;
        MMI^.ptMaxPosition.Y := WorkArea.Top - MonitorBounds.Top;
        MMI^.ptMaxSize.X := WorkArea.Right - WorkArea.Left;
        MMI^.ptMaxSize.Y := WorkArea.Bottom - WorkArea.Top;

        // Reduce the Max Size if the TaskBar is AutoHidden
        if SpGetTaskBar(TaskBarState, TaskBarEdge, TaskBarBounds) then begin
          if (TaskBarState and ABS_AUTOHIDE) = ABS_AUTOHIDE then
            case TaskBarEdge of
              ABE_LEFT, ABE_RIGHT: MMI^.ptMaxSize.X :=  MMI^.ptMaxSize.X - 2;
              ABE_TOP, ABE_BOTTOM: MMI^.ptMaxSize.Y :=  MMI^.ptMaxSize.Y - 2;
            end;
        end;

        // Max size during window resize, change the ParentForm constraints
        // to make it work
        if WindowState = wsMaximized then begin
          MMI^.ptMaxTrackSize.X := MMI^.ptMaxSize.X;
          MMI^.ptMaxTrackSize.Y := MMI^.ptMaxSize.Y;
          FParentForm.Constraints.MaxWidth := MMI^.ptMaxSize.X;
          FParentForm.Constraints.MaxHeight := MMI^.ptMaxSize.Y;
        end
        else begin
          FParentForm.Constraints.MaxWidth := 0;
          FParentForm.Constraints.MaxHeight := 0;
        end;

        Message.Result := 0;
      end;
    WM_SYSCOMMAND:
      if IsActive and Options.SystemMenu then begin
        M := TWMSysCommand(Message);
        if M.CmdType and $FFF0 = SC_KEYMENU then
          case M.Key of
            VK_SPACE:
              begin
                // Show the custom SysMenu
                SystemButtonIV := FToolbar.View.Find(FOptions.SystemButton);
                if Assigned(SystemButtonIV) then begin
                  SystemButtonP.X := SystemButtonIV.BoundsRect.Left;
                  SystemButtonP.Y := SystemButtonIV.BoundsRect.Bottom;
                  SystemButtonP := FToolbar.ClientToScreen(SystemButtonP);
                  FOptions.SystemButton.Popup(SystemButtonP.X, SystemButtonP.Y, True);
                  Message.Result := 1;
                  Exit;
                end;
              end;
            0:
              if GetCapture = 0 then begin
                // When only the Alt key is pressed and a few seconds latter the Space
                // key is pressed the default SysMenu is showed, this only happens
                // when there are no menubars on the form.
                // In this case the WM_SYSCOMMAND is sent when the Alt key is
                // pressed (Key = 0), but not when the space key is pressed.
                // Apparently there's no way to override this, the only solution is to
                // handle the Alt key press (Key = 0).
                //
                // Message log when Alt [...] Space is pressed:
                //  WM_SYSKEYDOWN: VK_MENU
                //  WM_SYSKEYUP: VK_MENU
                //  WM_SYSCOMMAND: Key = 0
                //  WM_ENTERMENULOOP
                //  WM_INITMENU
                //  WM_KEYDOWN: VK_SPACE
                //  WM_CHAR: VK_SPACE
                //  WM_INITMENUPOPUP: system hmenu

                // If the form has a main menu VK_SPACE will be correctly handled
                HandleSpaceKey := True;
                if Assigned(FParentForm.Menu) then
                  HandleSpaceKey := False
                else
                  if Toolbar.MenuBar then
                    HandleSpaceKey := False
                  else
                    for I := 0 to FParentForm.ComponentCount - 1 do
                      if FParentForm.Components[I] is TTBCustomToolbar then
                        if TTBCustomToolbar(FParentForm.Components[I]).MenuBar then begin
                          HandleSpaceKey := False;
                          Break;
                        end;
                if HandleSpaceKey then begin
                  Message.Result := 1;
                  Exit;
                end;
              end;
          end;
      end;
  end;

  // Default processing
  if Assigned(FOldParentFormWndProc) then
    FOldParentFormWndProc(Message);
end;

procedure TSpTBXCustomTitleBar.ChangeTitleBarState(Activate: Boolean);
var
  FloatingBorderSize: TPoint;
  Style: {$IF CompilerVersion >= 23} NativeInt {$ELSE} Integer {$IFEND};
  RestoreR: TRect;
  WState: TWindowState;
  OnParentFormShow: TNotifyEvent;
begin
  if Assigned(FParentForm) and (FParentForm.HandleAllocated) and ([csDesigning, csDestroying] * FParentForm.ComponentState = []) then begin

    if FMouseActive then begin
      FloatingBorderSize := GetFloatingBorderSize;

      // Changing the BorderStyle of the form will recreate it,
      // causing it to call Form.OnShow everytime Active is changed
      // We need to disable the OnShow calling.
      OnParentFormShow := TCustomFormAccess(FParentForm).OnShow;
      TCustomFormAccess(FParentForm).OnShow := nil;
      try
        WState := SpGetFormWindowState(FParentForm, RestoreR);

        if Activate then begin
          // Remove the border and titlebar from the form, and add the sysmenu
          FParentForm.BorderStyle := bsNone;
          Style := GetWindowLong(FParentForm.Handle, GWL_STYLE);
          Style := Style or WS_SYSMENU;
          SetWindowLong(FParentForm.Handle, GWL_STYLE, Style);

          // Resize the form to retain the same size before it was activated.
          // This is needed to keep the designtime size
          if (WState <> wsMaximized) and (TCustomFormAccess(FParentForm).FormStyle <> fsMDIChild) then begin
            FParentForm.Height := FParentForm.Height - GetSystemMetrics(SM_CYCAPTION) - (FloatingBorderSize.Y * 2);
            FParentForm.Width := FParentForm.Width - (FloatingBorderSize.X * 2);
          end;
        end
        else begin
          FParentForm.BorderStyle := bsSizeable;

          // Resize the form to retain the same size before it was deactivated.
          // This is needed to keep the designtime size
          if (WState <> wsMaximized) and (TCustomFormAccess(FParentForm).FormStyle <> fsMDIChild) then begin
            FParentForm.Height := FParentForm.Height + GetSystemMetrics(SM_CYCAPTION) + (FloatingBorderSize.Y * 2);
            FParentForm.Width := FParentForm.Width + (FloatingBorderSize.X * 2);
          end;
        end;

        // When Active is changed the Form is recreated, we have to
        // reset the Restore size if the form is currently Maximized.
        if WState = wsMaximized then
          SpSetFormWindowState(FParentForm, WState, RestoreR);

        InvalidateBackground;
      finally
        TCustomFormAccess(FParentForm).OnShow := OnParentFormShow;
      end;
    end;

    FDock.Visible := FActive;
  end;
end;

procedure TSpTBXCustomTitleBar.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  Rect := GetClientAreaRect;
end;

procedure TSpTBXCustomTitleBar.DoDrawDockBackground(ACanvas: TCanvas;
  ARect: TRect; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
var
  FloatingBorderSize: TPoint;
begin
  inherited DoDrawDockBackground(ACanvas, ARect, PaintStage, PaintDefault);
  if (PaintStage = pstPrePaint) and PaintDefault then begin
    PaintDefault := False;

    // [Theme-Change]
    // On WindowsXP make sure we paint the titlebar on the NC area
    // TSpTBXCustomTitleBar.WMEraseBkgnd and TSpTBXCustomTitleBar.DoDrawDockBackground handles this issue
    if SkinManager.GetSkinType in [sknWindows, sknDelphiStyle] then begin
      FloatingBorderSize := GetFloatingBorderSize;
      InflateRect(ARect, FloatingBorderSize.X, FloatingBorderSize.Y);
    end;

    SpDrawXPTitleBar(ACanvas, ARect, True);
  end;
end;

procedure TSpTBXCustomTitleBar.DoDrawBackground(ACanvas: TCanvas; ARect: TRect;
  const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawBackground) then FOnDrawBackground(Self, ACanvas, ARect, PaintStage, PaintDefault);
end;

function TSpTBXCustomTitleBar.GetClientAreaRect: TRect;
var
  FloatingBorderSize: TPoint;
begin
  Result := ClientRect;
  if Active then begin
    FloatingBorderSize := GetFloatingBorderSize;
    if Assigned(FParentForm) then begin
      if not FMouseActive or (FParentForm.WindowState <> wsMaximized) then
        InflateRect(Result, -FloatingBorderSize.X, -FloatingBorderSize.Y);
    end
    else
      InflateRect(Result, -FloatingBorderSize.X, -FloatingBorderSize.Y);
  end;
end;

function TSpTBXCustomTitleBar.GetFloatingBorderSize: TPoint;
var
  Details: TThemedElementDetails;
  ElementSize: TSize;
begin
  Result.X := GetSystemMetrics(SM_CXFRAME);
  Result.Y := GetSystemMetrics(SM_CYFRAME);

  case SkinManager.GetSkinType of
    sknSkin:
      Result := Point(CurrentSkin.FloatingWindowBorderSize, CurrentSkin.FloatingWindowBorderSize);
    sknDelphiStyle:
      begin
        Details := SpTBXThemeServices.GetElementDetails(twFrameBottomActive);
        ElementSize := CurrentSkin.GetThemedElementSize(Canvas, Details);
        Result.Y := ElementSize.cy;

        Details := SpTBXThemeServices.GetElementDetails(twFrameLeftActive);
        ElementSize := CurrentSkin.GetThemedElementSize(Canvas, Details);
        Result.X := ElementSize.cx;
      end;
  end;
end;

function TSpTBXCustomTitleBar.GetItems: TTBCustomItem;
begin
  // The ToolbarEditor designer will open the editable items and
  // not the Toolbar.Items
  Result := Options.EditableItems;
end;

procedure TSpTBXCustomTitleBar.GetSizeCursor(MousePos: TPoint; var SizeCursor,
  SizeCode: Integer);
var
  R: TRect;
  Pt, FloatingBorderSize: TPoint;
const
  SC_SizeLeft      = $F001;
  SC_SizeRight     = $F002;
  SC_SizeUp        = $F003;
  SC_SizeUpLeft    = $F004;
  SC_SizeUpRight   = $F005;
  SC_SizeDown      = $F006;
  SC_SizeDownLeft  = $F007;
  SC_SizeDownRight = $F008;
begin
  SizeCursor := 0;
  SizeCode := 0;

  if not IsActive or (Assigned(FParentForm) and (FParentForm.WindowState = wsMaximized)) then Exit;

  R := ClientRect;
  FloatingBorderSize := GetFloatingBorderSize;
  InflateRect(R, -FloatingBorderSize.X, -FloatingBorderSize.Y);
  Pt := MousePos;
  if not PtInRect(R, Pt) then begin
    if (Pt.X < 10) and (Pt.Y < 10) then SizeCode := SC_SizeUpLeft
    else if (Pt.X > Width - 10) and (Pt.Y < 10) then SizeCode := SC_SizeUpRight
    else if (Pt.X < 10) and (Pt.Y > Height - 10) then SizeCode := SC_SizeDownLeft
    else if (Pt.X > Width - 10) and (Pt.Y > Height - 10) then SizeCode := SC_SizeDownRight
    else if (Pt.X > 10) and (Pt.X < Width - 10) and (Pt.Y < 10) then SizeCode := SC_SizeUp
    else if (Pt.X > 10) and (Pt.X < Width - 10) and (Pt.Y > Height - 10) then SizeCode := SC_SizeDown
    else if (Pt.Y > 10) and (Pt.Y < Height - 10) and (Pt.X < 10) then SizeCode := SC_SizeLeft
    else if (Pt.Y > 10) and (Pt.Y < Height - 10) and (Pt.X > Width - 10) then SizeCode := SC_SizeRight;

    case SizeCode of
      SC_SizeLeft, SC_SizeRight: SizeCursor := -9;
      SC_SizeUp, SC_SizeDown: SizeCursor := -7;
      SC_SizeUpLeft, SC_SizeDownRight: SizeCursor := -8;
      SC_SizeDownLeft, SC_SizeUpRight: SizeCursor := -6;
    end;
  end
end;

function TSpTBXCustomTitleBar.GetSystemMenuPopup: TSpTBXPopupEvent;
begin
  Result := FOptions.FSystemButton.OnInitPopup;
end;

function TSpTBXCustomTitleBar.GetToolbarClass: TSpTBXToolbarClass;
begin
  Result := TSpTBXTitleToolbar;
end;

function TSpTBXCustomTitleBar.GetWindowState: TWindowState;
begin
  if Assigned(FParentForm) then
    Result := FParentForm.WindowState
  else
    Result := wsNormal;
end;

function TSpTBXCustomTitleBar.IsActive: Boolean;
begin
  Result := FActive and FMouseActive;
end;

procedure TSpTBXCustomTitleBar.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Pt: TPoint;
  SizeCursor, SizeCode: Integer;
begin
  inherited;
  if not FixedSize and (Button = mbLeft) then begin
    Pt := Point(X, Y);
    GetSizeCursor(Pt, SizeCursor, SizeCode);
    if (SizeCode > 0) and Assigned(FParentForm) then begin
      ReleaseCapture;
      FParentForm.Perform(WM_SYSCOMMAND, SizeCode, 0);
    end;
  end;
end;

procedure TSpTBXCustomTitleBar.SetActive(const Value: Boolean);
begin
  if FActive <> Value then begin
    FActive := Value;
    ChangeTitleBarState(Value);
  end;
end;

procedure TSpTBXCustomTitleBar.SetFullScreenMaximize(const Value: Boolean);
begin
  if FFullScreenMaximize <> Value then begin
    FFullScreenMaximize := Value;
    if IsActive and Assigned(FParentForm) and FParentForm.HandleAllocated then begin
      FParentForm.Constraints.MaxWidth := 0;
      FParentForm.Constraints.MaxHeight := 0;
      if (WindowState = wsMaximized) and not (csDesigning in ComponentState) then
        TCustomFormAccess(FParentForm).RecreateWnd;
    end;
  end;
end;

procedure TSpTBXCustomTitleBar.SetMouseActive(const Value: Boolean);
begin
  if FMouseActive <> Value then begin
    FMouseActive := Value;
    ChangeTitleBarState(Value);
  end;
end;

procedure TSpTBXCustomTitleBar.SetSystemMenuPopup(const Value: TSpTBXPopupEvent);
begin
  FOptions.FSystemButton.OnInitPopup := Value;
end;

procedure TSpTBXCustomTitleBar.SetWindowState(const Value: TWindowState);
begin
  if Assigned(FParentForm) then begin
    case Value of
      wsMinimized:
        begin
          // WindowState := wsMinimized will not minimize the app correctly
          SendMessage(FParentForm.Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
        end;
      wsMaximized, wsNormal:
        FParentForm.WindowState := Value;
    end;
  end;
end;

procedure TSpTBXCustomTitleBar.WMSpSkinChange(var Message: TMessage);
begin
  UpdateSkinMetrics;
  inherited;
end;

{$IF CompilerVersion >= 23} //for Delphi XE2 and up
procedure TSpTBXCustomTitleBar.CMStyleChanged(var Message: TMessage);
begin
  inherited;
  // If the Windows theme or Delphi style is changed repaint the NC area
  if HandleAllocated then
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_ERASE);
end;
{$IFEND}

procedure TSpTBXCustomTitleBar.CMTextChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FOptions) then
    FOptions.CaptionLabel := Caption;
end;

procedure TSpTBXCustomTitleBar.WMEraseBkgnd(var Message: TMessage);
var
  ARect, DockAreaR: TRect;
  FloatingBorderSize: TPoint;
  Maximized, PaintDefault: Boolean;
  B: TBitmap;
begin
  Message.Result := 1;
  // Only erase background if we are not Doublebuffering or painting to memory
  // On Delphi XE2 WParam and LParam are different in size, WParam is
  // UINT_PTR (64 bit) and LParam is INT_PTR (32 bit)
  if not DoubleBuffered or (Message.wParam = WPARAM(Message.lParam)) then begin
    B := TBitmap.Create;
    try
      ARect := GetClientRect;
      B.Width := ARect.Right;
      B.Height := ARect.Bottom;
      B.Canvas.Brush.Color := Color; // SpDrawXPTitleBarBody needs it to paint the background
      B.Canvas.FillRect(ARect);

      PaintDefault := True;
      DoDrawBackground(B.Canvas, ARect, pstPrePaint, PaintDefault);
      if PaintDefault then begin
        Maximized := (WindowState = wsMaximized) and FMouseActive;
        if Maximized then
          InflateRect(ARect, 4, 4);
        if Active then begin
          FloatingBorderSize := GetFloatingBorderSize;
          SpDrawXPTitleBarBody(B.Canvas, ARect, True, FloatingBorderSize);

          // [Theme-Change]
          // On WindowsXP make sure we paint the titlebar on the NC area
          // TSpTBXCustomTitleBar.WMEraseBkgnd and TSpTBXCustomTitleBar.DoDrawDockBackground handles this issue
          if Assigned(FDock) and not Maximized and (SkinManager.GetSkinType in [sknWindows, sknDelphiStyle]) then begin
            DockAreaR := ARect;
            DockAreaR.Bottom := FDock.Height + FloatingBorderSize.Y; // don't multiply by 2
            SpDrawXPTitleBar(B.Canvas, DockAreaR, True);
          end;
        end;
      end;

      PaintDefault := True;
      DoDrawBackground(B.Canvas, ARect, pstPostPaint, PaintDefault);

      BitBlt(TWMEraseBkgnd(Message).DC, 0, 0, B.Width, B.Height, B.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      B.Free;
    end;
  end;
end;

procedure TSpTBXCustomTitleBar.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
  SizeCursor, SizeCode: Integer;
begin
  if not FixedSize and not (csDesigning in ComponentState) and
    (Message.CursorWnd = Handle) and (Screen.Cursor = crDefault) then
  begin
    GetCursorPos(P);
    P := ScreenToClient(P);
    GetSizeCursor(P, SizeCursor, SizeCode);
    if SizeCursor <> 0 then begin
      Windows.SetCursor(Screen.Cursors[SizeCursor]);
      Message.Result := 1;
    end
    else
      inherited;
  end
  else
    inherited;
end;

procedure TSpTBXCustomTitleBar.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;

  if not FUpdateRegionCalled then
    UpdateRegion;

  InvalidateBackground(False);
  if [csDesigning, csDestroying] * ComponentState = [] then begin
    if FOptions.Maximize then
      FOptions.SetupButtonIcon(FOptions.MaximizeButton);
    // Hide the Toolbar if the Form is a MDI child and it's maximized
    if Assigned(FParentForm) and (FParentForm.HandleAllocated) then begin
      if (TCustomFormAccess(FParentForm).FormStyle = fsMDIChild) then
        FDock.Visible := FParentForm.WindowState <> wsMaximized;
    end;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TBitmapHint }

procedure TBitmapHint.ActivateHint(Rect: TRect; const AHint: string);
var
  SaveActivating: Boolean;
begin
  SaveActivating := FActivating;
  try
    FActivating := True;
    inherited ActivateHint(Rect, AHint);
  finally
    FActivating := SaveActivating;
  end;
end;

procedure TBitmapHint.ActivateHintData(Rect: TRect; const AHint: string; AData: Pointer);
begin
  //The AData parameter is a bitmap
  FHintBitmap := TBitmap(AData);
  Rect.Right := Rect.Left + FHintBitmap.Width - 2;
  Rect.Bottom := Rect.Top + FHintBitmap.Height - 2;
  inherited ActivateHintData(Rect, AHint, AData);
end;

procedure TBitmapHint.CMTextChanged(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TBitmapHint.Paint;
begin
  if Assigned(FHintBitmap) then
    Canvas.Draw(0, 0, FHintBitmap);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Stock Objects }

procedure InitializeStock;
var
  NonClientMetrics: TNonClientMetrics;
begin
  // Small caption font
  SmCaptionFont := TFont.Create;
  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    SmCaptionFont.Handle := CreateFontIndirect(NonClientMetrics.lfSmCaptionFont);

  SpStockHintBitmap := TBitmap.Create;

  Screen.Cursors[crSpTBXNewHandPoint] := LoadCursor(0, IDC_HAND);
  Screen.Cursors[crSpTBXCustomization] := LoadCursor(HInstance, 'CZMOVE');
  Screen.Cursors[crSpTBXCustomizationCancel] := LoadCursor(HInstance, 'CZCANCEL');

  // Dummy ImageList, used by TSpTBXItemViewer and TSpTBXButtonOptions
  MDIButtonsImgList := TImageList.Create(nil);

  {$IF CompilerVersion >= 23}
  // XE2 and up
  // When Styles are used WM_NCHITTEST and WM_NCCALCSIZE are handled by
  // TFormStyleHook. We need to override the handling by re-registering
  // the hook by using an empty style hook (TStyleHook)
  TCustomStyleEngine.RegisterStyleHook(TSpTBXFloatingWindowParent, TStyleHook);
  {$IFEND}
end;

procedure FinalizeStock;
begin
  FreeAndNil(SmCaptionFont);
  FreeAndNil(SpStockHintBitmap);
  FreeAndNil(MDIButtonsImgList);
end;

initialization
  InitializeStock;

finalization
  FinalizeStock;

end.
