{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDockVIDStyle.pas, released on 2003-12-31.

The Initial Developer of the Original Code is luxiaoban.
Portions created by luxiaoban are Copyright (C) 2002,2003 luxiaoban.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDockVIDStyle;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Classes, Graphics, Controls, ComCtrls, ImgList,
  JvConsts, JvDockControlForm, JvDockSupportControl, JvDockTree,
  JvDockAdvTree, JvDockGlobals;

type
  TJvDockVIDConjoinServerOption = class(TJvDockBasicConjoinServerOption)
  private
    FTextEllipsis: Boolean;
    FTextAlignment: TAlignment;
    FInactiveTitleEndColor: TColor;
    FInactiveTitleStartColor: TColor;
    FInactiveTitleVerticalGradient: Boolean;
    FActiveTitleEndColor: TColor;
    FActiveTitleStartColor: TColor;
    FActiveTitleVerticalGradient: Boolean;
    FActiveDockGrabber: Boolean;
    FSystemInfo: Boolean;
    FActiveFont: TFont;
    FInactiveFont: TFont;
    procedure SetActiveTitleEndColor(const Value: TColor);
    procedure SetActiveTitleStartColor(const Value: TColor);
    procedure SetInactiveTitleEndColor(const Value: TColor);
    procedure SetInactiveTitleStartColor(const Value: TColor);
    procedure SetTextAlignment(const Value: TAlignment);
    procedure SetTextEllipsis(const Value: Boolean);
    procedure SetSystemInfo(const Value: Boolean);
    procedure SetActiveFont(Value: TFont);
    procedure SetInactiveFont(Value: TFont);
    procedure SetActiveTitleVerticalGradient(const Value: Boolean);
    procedure SetInactiveTitleVerticalGradient(const Value: Boolean);
    procedure SetActiveDockGrabber(const Value: Boolean);
  protected
    procedure FontChanged(Sender: TObject);
    function IsNotSystemInfo: Boolean;
    procedure SettingChange(Sender: TObject);
    procedure Changed; override;
    procedure UpdateDefaultSystemCaptionInfo; virtual;
    procedure SetDefaultSystemCaptionInfo;
  public
    constructor Create(ADockStyle: TJvDockObservableStyle); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ActiveFont: TFont read FActiveFont write SetActiveFont stored IsNotSystemInfo;
    property InactiveFont: TFont read FInactiveFont write SetInactiveFont stored IsNotSystemInfo;
    property TextAlignment: TAlignment read FTextAlignment write SetTextAlignment default taLeftJustify;
    property ActiveTitleStartColor: TColor read FActiveTitleStartColor write SetActiveTitleStartColor stored
      IsNotSystemInfo;
    property ActiveTitleEndColor: TColor read FActiveTitleEndColor write SetActiveTitleEndColor stored
      IsNotSystemInfo;
    property ActiveTitleVerticalGradient: Boolean read FActiveTitleVerticalGradient write
      SetActiveTitleVerticalGradient default False;
    property ActiveDockGrabber: Boolean read FActiveDockGrabber write SetActiveDockGrabber default False;
    property InactiveTitleStartColor: TColor read FInactiveTitleStartColor write SetInactiveTitleStartColor stored
      IsNotSystemInfo;
    property InactiveTitleEndColor: TColor read FInactiveTitleEndColor write SetInactiveTitleEndColor stored
      IsNotSystemInfo;
    property InactiveTitleVerticalGradient: Boolean read FInactiveTitleVerticalGradient write
      SetInactiveTitleVerticalGradient default False;
    property TextEllipsis: Boolean read FTextEllipsis write SetTextEllipsis default True;
    property SystemInfo: Boolean read FSystemInfo write SetSystemInfo default True;
    property GrabbersSize default VIDDefaultDockGrabbersSize;
    property SplitterWidth default VIDDefaultDockSplitterWidth;
  end;

  TJvDockVIDTabServerOption = class(TJvDockBasicTabServerOption)
  private
    FActiveFont: TFont;
    FActiveSheetColor: TColor;
    FHotTrackColor: TColor;
    FInactiveFont: TFont;
    FInactiveSheetColor: TColor;
    FShowTabImages: Boolean;
    FShowTabHints: Boolean;
    { NEW! if true, shows invididual close buttons on tabs. If false, you get the old VID behaviour. }
    FShowCloseButtonOnTabs: Boolean;
    {NEW! default is true, which is the old VID Style behaviour. False is a new behaviour added by Warren. }
    FShowCloseButtonOnGrabber: Boolean;
    procedure SetActiveFont(Value: TFont);
    procedure SetActiveSheetColor(const Value: TColor);
    procedure SetHotTrackColor(const Value: TColor);
    procedure SetInactiveFont(Value: TFont);
    procedure SetInactiveSheetColor(const Value: TColor);
    procedure SetShowTabImages(const Value: Boolean);
    procedure SetShowTabHints(const Value: Boolean);
    procedure SetShowCloseButtonOnGrabber(const Value: Boolean);
    procedure SetShowCloseButtonOnTabs(const Value: Boolean);
  protected
    procedure FontChanged(Sender: TObject);
  public
    constructor Create(ADockStyle: TJvDockObservableStyle); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetTabPosition(const Value: TTabPosition); override;
  published
    property ActiveSheetColor: TColor read FActiveSheetColor write SetActiveSheetColor default clBtnFace;
    property InactiveSheetColor: TColor read FInactiveSheetColor write SetInactiveSheetColor default clBtnShadow;
    property ActiveFont: TFont read FActiveFont write SetActiveFont;
    property InactiveFont: TFont read FInactiveFont write SetInactiveFont;
    property HotTrackColor: TColor read FHotTrackColor write SetHotTrackColor default clBlue;
    property ShowTabImages: Boolean read FShowTabImages write SetShowTabImages default False;
    { If true, shows the tab caption as a hint when you mouse over it }
    property ShowTabHints: Boolean read FShowTabHints write SetShowTabHints default False;
    property TabPosition default tpBottom;
    { NEW! If true, shows invididual close buttons on tabs.
           If false, you get the old VID behaviour. }
    property ShowCloseButtonOnTabs: Boolean read FShowCloseButtonOnTabs write SetShowCloseButtonOnTabs;
    {NEW! Default is true, which is the old VID Style behaviour.
          False is a new behaviour added by Warren. }
    property ShowCloseButtonOnGrabber: Boolean read FShowCloseButtonOnGrabber write
      SetShowCloseButtonOnGrabber default True;
  end;

  TJvDockSystemInfoChange = procedure(Value: Boolean) of object;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvDockVIDStyle = class(TJvDockAdvStyle)
  private
    FAlwaysShowGrabber: Boolean;
    FSystemInfoChange: TJvDockSystemInfoChange;
    procedure SetAlwaysShowGrabber(const Value: Boolean);
  protected
    function DockClientWindowProc(DockClient: TJvDockClient; var Msg: TMessage): Boolean; override;
    procedure FormDockDrop(DockClient: TJvDockClient;
      Source: TJvDockDragDockObject; X, Y: Integer); override;
    procedure FormGetSiteInfo(Source: TJvDockDragDockObject; DockClient: TJvDockClient;
      Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
      var CanDock: Boolean); override;
    procedure FormDockOver(DockClient: TJvDockClient; Source: TJvDockDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure FormStartDock(DockClient: TJvDockClient;
      var Source: TJvDockDragDockObject); override;
    procedure FormGetDockEdge(DockClient: TJvDockClient; Source: TJvDockDragDockObject;
      MousePos: TPoint; var DropAlign: TAlign); override;

    procedure DoSystemInfoChange(Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetDockBaseControl(IsCreate: Boolean; DockBaseControl: TJvDockBaseControl); override;
  published
    property AlwaysShowGrabber: Boolean read FAlwaysShowGrabber write SetAlwaysShowGrabber; {NEW}
    property SystemInfoChange: TJvDockSystemInfoChange read FSystemInfoChange write FSystemInfoChange;
    property ConjoinServerOption;
    property TabServerOption;
  end;

  TJvDockVIDSplitter = class(TJvDockSplitter);

  TJvDockVIDPanel = class(TJvDockAdvPanel)
  protected
    procedure CustomGetSiteInfo(Source: TJvDockDragDockObject;
      Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
      var CanDock: Boolean); override;
    procedure CustomStartDock(var Source: TJvDockDragDockObject); override;
    procedure CustomDockDrop(Source: TJvDockDragDockObject; X, Y: Integer); override;
    procedure CustomDockOver(Source: TJvDockDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure CustomGetDockEdge(Source: TJvDockDragDockObject; MousePos: TPoint;
      var DropAlign: TAlign); override;
  public
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
    procedure UpdateCaption(Exclude: TControl); override;
  end;

  TJvDockVIDConjoinPanel = class(TJvDockAdvConjoinPanel)
  protected
    procedure CustomGetSiteInfo(Source: TJvDockDragDockObject;
      Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
      var CanDock: Boolean); override;
    procedure CustomDockOver(Source: TJvDockDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure CustomGetDockEdge(Source: TJvDockDragDockObject; MousePos: TPoint; var DropAlign: TAlign); override;
    function CustomUnDock(Source: TJvDockDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; override;
    procedure CustomDockDrop(Source: TJvDockDragDockObject; X, Y: Integer); override;
  public
    procedure UpdateCaption(Exclude: TControl); override;
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
  end;

  TJvDockVIDZone = class(TJvDockAdvZone)
  protected
    function GetSplitterLimit(IsMin: Boolean): Integer; override;
  public
    destructor Destroy; override;
    procedure Insert(DockSize: Integer; Hide: Boolean); override;
    procedure Remove(DockSize: Integer; Hide: Boolean); override;
  end;

  TJvDockVIDTree = class(TJvDockAdvTree)
  private
    FDropOnZone: TJvDockZone;
    FLockDropDockSizeCount: Integer;
    FCaptionLeftOffset: Integer;
    FCaptionRightOffset: Integer;
    FShowCloseButtonOnGrabber: Boolean;
    FAlwaysShowGrabber: Boolean;
    procedure LockDropDockSize;
    procedure UnlockDropDockSize;
    function GetCaptionLeftOffset: Integer;
    function GetCaptionRightOffset: Integer;
    procedure SetCaptionLeftOffset(const Value: Integer);
    procedure SetCaptionRightOffset(const Value: Integer);
    procedure SetShowCloseButtonOnGrabber(const Value: Boolean);
    procedure SetAlwaysShowGrabber(const Value: Boolean);
    procedure InvalidateDockSite(const Client: TControl);
  protected
    procedure InsertControlFromConjoinHost(Control: TControl;
      InsertAt: TAlign; DropCtl: TControl); virtual;
    procedure IgnoreZoneInfor(Stream: TMemoryStream); virtual;

    { [ERROR] Method 'AdjustDockRect' not found in base class.
      if you get this error here, it is a Delphi compiler issue. }

    procedure AdjustDockRect(Control: TControl; var ARect: TRect); override;
    procedure WindowProc(var Msg: TMessage); override;
    procedure SplitterMouseUp; override;
    function GetTopGrabbersHTFlag(const MousePos: TPoint;
      out HTFlag: Integer; Zone: TJvDockZone): TJvDockZone; override;
    function GetDockGrabbersPosition: TJvDockGrabbersPosition; override;
    procedure GetSiteInfo(Client: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean); override;
    procedure InsertControl(Control: TControl; InsertAt: TAlign;
      DropCtl: TControl); override;
    procedure InsertSibling(NewZone, SiblingZone: TJvDockZone;
      InsertLast, Update: Boolean); override;
    procedure InsertNewParent(NewZone, SiblingZone: TJvDockZone;
      ParentOrientation: TDockOrientation; InsertLast, Update: Boolean); override;
    procedure DrawDockGrabber(Control: TWinControl; const ARect: TRect); override;
    procedure DrawSplitterRect(const ARect: TRect); override;
    procedure PaintDockGrabberRect(Canvas: TCanvas; Control: TWinControl;
      const ARect: TRect; PaintAlways: Boolean = False); virtual;
    procedure DrawCloseButton(Canvas: TCanvas; Zone: TJvDockZone;
      Left, Top: Integer); virtual;
    procedure ResetBounds(Force: Boolean); override;
    procedure DrawDockSiteRect; override;
    procedure PositionDockRect(Client, DropCtl: TControl; DropAlign: TAlign;
      var DockRect: TRect); override;
    function GetDockEdge(DockRect: TRect; MousePos: TPoint;
      var DropAlign: TAlign; Control: TControl): TControl; override;
    procedure RemoveZone(Zone: TJvDockZone; Hide: Boolean = True); override;
    procedure GetCaptionRect(var Rect: TRect); override;
    procedure SyncWithStyle; override;
    property CaptionLeftOffset: Integer read GetCaptionLeftOffset write SetCaptionLeftOffset;
    property CaptionRightOffset: Integer read GetCaptionRightOffset write SetCaptionRightOffset;
  public
    constructor Create(DockSite: TWinControl; DockZoneClass: TJvDockZoneClass;
      ADockStyle: TJvDockObservableStyle); override;
    property ShowCloseButtonOnGrabber: Boolean read FShowCloseButtonOnGrabber write SetShowCloseButtonOnGrabber;
    property AlwaysShowGrabber: Boolean read FAlwaysShowGrabber write SetAlwaysShowGrabber;
  end;

  TJvDockVIDTabPageControl = class;

  TJvDockVIDTabSheet = class(TJvDockTabSheet)
  private
    FTabWidth: Integer;
    FShowTabWidth: Integer;
    FIsSourceDockClient: Boolean;
    procedure SetTabWidth(const Value: Integer);
    procedure WMSetText(var Msg: TMessage); message WM_SETTEXT;
    procedure SetSheetSort(const CaptionStr: string);
  protected
    procedure SetPageControl(APageControl: TJvDockPageControl); override;
    property TabWidth: Integer read FTabWidth write SetTabWidth;
    property ShowTabWidth: Integer read FShowTabWidth;
    procedure Loaded; override;
    procedure UpdateTabShowing; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BorderWidth;
    property Caption;
    property DragMode;
    property Enabled;
    property Font;
    property Height stored False;
    property Highlighted;
    property ImageIndex;
    property Left stored False;
    property Constraints;
    property PageIndex;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabVisible;
    property Top stored False;
    property Visible stored False;
    property Width stored False;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnHide;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnShow;
    property OnStartDrag;
  end;

  TJvDockTabPanel = class(TCustomControl)
  private
    FDockPanel: TJvDockPanel; // If docked to a dock panel, this is it. nil is floating.
    FPage: TJvDockVIDTabPageControl;
    FActiveSheetColor: TColor;
    FHotTrackColor: TColor;
    FActiveFont: TFont;
    FInactiveFont: TFont;
    FTabLeftOffset: Integer;
    FTabRightOffset: Integer;
    FTabTopOffset: Integer;
    FTabBottomOffset: Integer;
    FCaptionLeftOffset: Integer;
    FCaptionRightOffset: Integer;
    FCaptionTopOffset: Integer;
    FTabSplitterWidth: Integer;
    FTabHeight: Integer;
    FSortList: TList;
    FSelectSheet: TJvDockVIDTabSheet;
    FTempPages: TList;
    FShowTabImages: Boolean;
    FShowTabHints: Boolean;
    procedure SetPage(const Value: TJvDockVIDTabPageControl);
    function GetTotalTabWidth: Integer;
    procedure SetTotalTabWidth(const Value: Integer);
    function GetMinTabWidth: TJvDockTabSheet;
    function GetMaxTabWidth: TJvDockTabSheet;
    function GetTabBottomOffset: Integer;
    function GetTabLeftOffset: Integer;
    function GetTabRightOffset: Integer;
    function GetTabTopOffset: Integer;
    function GetCaptionLeftOffset: Integer;
    function GetCaptionRightOffset: Integer;
    function GetCaptionTopOffset: Integer;
    function GetTabSplitterWidth: Integer;
    procedure SetTabBottomOffset(const Value: Integer);
    procedure SetTabLeftOffset(const Value: Integer);
    procedure SetTabRightOffset(const Value: Integer);
    procedure SetTabTopOffset(const Value: Integer);
    procedure SetCaptionLeftOffset(const Value: Integer);
    procedure SetCaptionRightOffset(const Value: Integer);
    procedure SetCaptionTopOffset(const Value: Integer);
    procedure SetTabSplitterWidth(const Value: Integer);
    function GetSorts(Index: Integer): TJvDockVIDTabSheet;
    function GetPanelHeight: Integer;
    function GetPanelWidth: Integer;
    procedure SetPanelHeight(const Value: Integer);
    function FindSheetWithPos(cX, cY, cTopOffset, cBottomOffset: Integer): Integer;
    function GetDockClientFromPageIndex(Index: Integer): TControl;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure SetShowTabImages(const Value: Boolean);
    procedure SetShowTabHints(const Value: Boolean);
    procedure SetTabHeight(const Value: Integer);
    function GetTabHeight: Integer;
  protected
    FSelectHotIndex: Integer;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function GetPageIndexFromMousePos(X, Y: Integer): Integer; virtual;
    procedure SetShowTabWidth;
    property TotalTabWidth: Integer read GetTotalTabWidth write SetTotalTabWidth;
    property MinTabWidth: TJvDockTabSheet read GetMinTabWidth;
    property MaxTabWidth: TJvDockTabSheet read GetMaxTabWidth;
    property TabLeftOffset: Integer read GetTabLeftOffset write SetTabLeftOffset default 5;
    property TabRightOffset: Integer read GetTabRightOffset write SetTabRightOffset default 5;
    property TabTopOffset: Integer read GetTabTopOffset write SetTabTopOffset default 2;
    property TabBottomOffset: Integer read GetTabBottomOffset write SetTabBottomOffset default 3;
    property TabSplitterWidth: Integer read GetTabSplitterWidth write SetTabSplitterWidth default 2;
    property CaptionTopOffset: Integer read GetCaptionTopOffset write SetCaptionTopOffset default 0;
    property CaptionLeftOffset: Integer read GetCaptionLeftOffset write SetCaptionLeftOffset default 5;
    property CaptionRightOffset: Integer read GetCaptionRightOffset write SetCaptionRightOffset default 5;
    property Sorts[Index: Integer]: TJvDockVIDTabSheet read GetSorts;
    property PanelHeight: Integer read GetPanelHeight write SetPanelHeight;
    property PanelWidth: Integer read GetPanelWidth;
    property TabHeight: Integer read GetTabHeight write SetTabHeight;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Resize; override;
    procedure DeleteSorts(Sheet: TJvDockVIDTabSheet);
    property Page: TJvDockVIDTabPageControl read FPage write SetPage;
    property SelectSheet: TJvDockVIDTabSheet read FSelectSheet write FSelectSheet;
    property ShowTabImages: Boolean read FShowTabImages write SetShowTabImages;
    property ShowTabHints: Boolean read FShowTabHints write SetShowTabHints;
    {NEW! If docked to a TJvDockPanel, this is it. if not (nil) then it is floating.}
    property DockPanel: TJvDockPanel read FDockPanel write FDockPanel;
  end;

  TJvDockTabPanelClass = class of TJvDockTabPanel;

  TJvDockVIDTabPageControl = class(TJvDockAdvTabPageControl)
  private
    FTabPanelClass: TJvDockTabPanelClass;
    FPanel: TJvDockTabPanel;
    FTempSheet: TJvDockVIDTabSheet;
    FTabImageList: TCustomImageList;
    procedure SetActiveSheetColor(const Value: TColor);
    procedure SetInactiveSheetColor(const Value: TColor);
    procedure SetTabBottomOffset(const Value: Integer);
    procedure SetTabLeftOffset(const Value: Integer);
    procedure SetTabRightOffset(const Value: Integer);
    procedure SetTabTopOffset(const Value: Integer);
    procedure SetActiveFont(Value: TFont);
    procedure SetInactiveFont(Value: TFont);
    procedure SetHotTrackColor(const Value: TColor);
    function GetTabBottomOffset: Integer;
    function GetTabLeftOffset: Integer;
    function GetTabRightOffset: Integer;
    function GetTabTopOffset: Integer;
    function GetInactiveSheetColor: TColor;
    function GetActiveSheetColor: TColor;
    function GetActiveFont: TFont;
    function GetInactiveFont: TFont;
    function GetVisibleSheetCount: Integer;
    function GetHotTrackColor: TColor;
    function GetShowTabImages: Boolean;
    procedure SetShowTabImages(const Value: Boolean);
    function GetShowTabHints: Boolean;
    procedure SetShowTabHints(const Value: Boolean);
    function GetPage(Index: Integer): TJvDockVIDTabSheet;
    function GetActiveVIDPage: TJvDockVIDTabSheet;
    procedure SetActiveVIDPage(const Value: TJvDockVIDTabSheet);
    procedure CMDockNotification(var Msg: TCMDockNotification); message CM_DOCKNOTIFICATION;
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure CreatePanel; virtual;
    procedure Change; override;
    procedure DoRemoveDockClient(Client: TControl); override;

    procedure CustomDockOver(Source: TJvDockDragDockObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure CustomGetSiteInfo(Source: TJvDockDragDockObject; Client: TControl; var InfluenceRect: TRect;
      MousePos: TPoint; var CanDock: Boolean); override;
    procedure CustomDockDrop(Source: TJvDockDragDockObject; X, Y: Integer); override;
    procedure CustomGetDockEdge(Source: TJvDockDragDockObject; MousePos: TPoint; var DropAlign: TAlign); override;
    function CustomUnDock(Source: TJvDockDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; override;
    procedure DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean); override;
    procedure CreateParams(var Params: TCreateParams); override;
    function GetDockClientFromMousePos(MousePos: TPoint): TControl; override;
    procedure Paint; override;
    procedure SetActivePage(Page: TJvDockTabSheet); override;
    procedure SetTabHeight(Value: Smallint); override;
    procedure SetTabPosition(Value: TTabPosition); override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure SetHotTrack(Value: Boolean); override;
    procedure SetImages(Value: TCustomImageList); override;
    procedure SyncWithStyle; override;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    property TabPanelClass: TJvDockTabPanelClass read FTabPanelClass write FTabPanelClass;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
    property ActiveVIDPage: TJvDockVIDTabSheet read GetActiveVIDPage write SetActiveVIDPage;
    destructor Destroy; override;
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;

    procedure UpdateCaption(Exclude: TControl); override;
    procedure Resize; override;
    property Pages[Index: Integer]: TJvDockVIDTabSheet read GetPage;
    property Panel: TJvDockTabPanel read FPanel;
    property TempSheet: TJvDockVIDTabSheet read FTempSheet write FTempSheet;
    property VisibleSheetCount: Integer read GetVisibleSheetCount;
  published
    property ActiveSheetColor: TColor read GetActiveSheetColor write SetActiveSheetColor;
    property InactiveSheetColor: TColor read GetInactiveSheetColor write SetInactiveSheetColor;
    property TabLeftOffset: Integer read GetTabLeftOffset write SetTabLeftOffset default 5;
    property TabRightOffset: Integer read GetTabRightOffset write SetTabRightOffset default 5;
    property TabTopOffset: Integer read GetTabTopOffset write SetTabTopOffset default 2;
    property TabBottomOffset: Integer read GetTabBottomOffset write SetTabBottomOffset default 3;
    property ActiveFont: TFont read GetActiveFont write SetActiveFont;
    property InactiveFont: TFont read GetInactiveFont write SetInactiveFont;
    property HotTrackColor: TColor read GetHotTrackColor write SetHotTrackColor;
    property ShowTabImages: Boolean read GetShowTabImages write SetShowTabImages;
    property ShowTabHints: Boolean read GetShowTabHints write SetShowTabHints;
    property ActivePage;
    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HotTrack;
    property Images;
    property MultiLine;
    property OwnerDraw;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RaggedRight;
    property ScrollOpposite;
    property ShowHint;
    property Style;
    property TabHeight;
    property TabIndex;
    property TabOrder;
    property TabPosition;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnChange;
    property OnChanging;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawTab;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  TJvDockVIDDragDockObject = class(TJvDockDragDockObject)
  private
    FOldDropAlign: TAlign;
    FCurrState: TDragState;
    FOldState: TDragState;
    FOldTarget: Pointer;
    FSourceDockClientList: TList;
    FDropTabControl: TJvDockVIDTabPageControl;
    FIsTabDockOver: Boolean;
    FErase: Boolean;
    function GetSourceDockClient(Index: Integer): TControl;
    function GetSourceDockClientCount: Integer;
    procedure SetOldState(const Value: TDragState);
    procedure SetCurrState(const Value: TDragState);
  protected
    procedure GetBrush_PenSize_DrawRect(var ABrush: TBrush; var PenSize: Integer;
      var DrawRect: TRect; Erase: Boolean); override;
    procedure MouseMsg(var Msg: TMessage); override;
    procedure DefaultDockImage(Erase: Boolean); override;
    function CanLeave(NewTarget: TWinControl): Boolean; override;
  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;
    function DragFindWindow(const Pos: TPoint): THandle; override;
    function GetDropCtl: TControl; override;
    property SourceDockClients[Index: Integer]: TControl read GetSourceDockClient;
    property SourceDockClientCount: Integer read GetSourceDockClientCount;
    property CurrState: TDragState read FCurrState write SetCurrState;
    property OldState: TDragState read FOldState write SetOldState;
  end;

procedure PaintGradientBackground(Canvas: TCanvas; ARect: TRect; StartColor, EndColor: TColor;
  Vertical: Boolean = False);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  Types,
  {$IFDEF JVCLThemesEnabled}
  JvThemes,
  {$ENDIF JVCLThemesEnabled}
  SysUtils, Math, Forms,
  JvDockSupportProc;

type
  TJvTempWinControl = class(TWinControl);

var
  gi_DockRect: TRect;

{ (rb) Compare to PaintGradientBackground in JvDockVIDVCStyle.pas }
procedure PaintGradientBackground(Canvas: TCanvas; ARect: TRect;
  StartColor, EndColor: TColor; Vertical: Boolean = False);
const
  D = 256;
var
  X, Y, C1, C2, R1, G1, B1, W, H: Integer;
  DR, DG, DB, DH, DW: Real;

  procedure InitRGBValues(C1, C2: Integer);
  begin
    R1 := GetRValue(C1);
    G1 := GetGValue(C1);
    B1 := GetBValue(C1);
    DR := (GetRValue(C2) - R1) / D;
    DG := (GetGValue(C2) - G1) / D;
    DB := (GetBValue(C2) - B1) / D;
  end;

begin
  with Canvas do
  begin
    Lock;
    try
      Brush.Style := bsSolid;

      { !! GetRValue etc. assume that the input param is a RGB value thus
           NO system color, such as clWindowText etc. }
      C1 := ColorToRGB(StartColor);
      C2 := ColorToRGB(EndColor);

      if C1 <> C2 then
      begin
        InitRGBValues(C1, C2);

        if not Vertical then
        begin
          DH := (ARect.Right - ARect.Left) / D;
          for X := 0 to 255 do
          begin
            Brush.Color := RGB(R1 + Round(DR * X), G1 + Round(DG * X),
              B1 + Round(DB * X));
            if ARect.Right <= ARect.Left + Round((X + 1) * DH) then
              W := ARect.Right
            else
              W := ARect.Left + Round((X + 1) * DH);
            FillRect(Rect(ARect.Left + Round(X * DH), ARect.Top, W, ARect.Bottom));
          end;
        end
        else
        begin
          DW := (ARect.Bottom - ARect.Top) / D;
          for Y := 0 to 255 do
          begin
            Brush.Color := RGB(R1 + Round(DR * Y), G1 + Round(DG * Y),
              B1 + Round(DB * Y));
            if ARect.Bottom <= ARect.Top + Round((Y + 1) * DW) then
              H := ARect.Bottom
            else
              H := ARect.Top + Round((Y + 1) * DW);
            FillRect(Rect(ARect.Left, ARect.Top + Round(Y * DW), ARect.Right, H));
          end;
        end;
      end
      else
      begin
        Brush.Color := StartColor;
        FillRect(ARect);
      end;
    finally
      Unlock;
    end;
  end;
end;

procedure AssignList(FromList, ToList: TList);
var
  I: Integer;
begin
  ToList.Clear;
  for I := 0 to FromList.Count - 1 do
    ToList.Add(FromList[I]);
end;

function ComputeVIDDockingRect(Target, Control: TControl; var DockRect: TRect; MousePos: TPoint): TAlign;
var
  DockTopRect: TRect;
  DockLeftRect: TRect;
  DockBottomRect: TRect;
  DockRightRect: TRect;
  DockCenterRect: TRect;
  DockTabRect: TRect;
begin
  Result := alNone;
  if Target = nil then
    Exit;

  with Target do
  begin
    DockLeftRect.TopLeft := Point(0, 0);
    DockLeftRect.BottomRight := Point(ClientWidth div 5, ClientHeight);

    DockTopRect.TopLeft := Point(ClientWidth div 5, 0);
    DockTopRect.BottomRight := Point(ClientWidth div 5 * 4, ClientHeight div 5);

    DockRightRect.TopLeft := Point(ClientWidth div 5 * 4, 0);
    DockRightRect.BottomRight := Point(ClientWidth, ClientHeight);

    if Target is TJvDockCustomTabControl then
    begin
      DockBottomRect.TopLeft := Point(ClientWidth div 5, ClientWidth div 5 * 4);
      DockBottomRect.BottomRight := Point(ClientWidth div 5 * 4, ClientHeight - JvDockGetSysCaptionHeight);
    end
    else
    begin
      DockBottomRect.TopLeft := Point(0, ClientHeight div 5 * 4);
      DockBottomRect.BottomRight := Point(ClientWidth, ClientHeight);
    end;

    DockCenterRect.TopLeft := Point(0, -JvDockGetSysCaptionHeight);
    DockCenterRect.BottomRight := Point(ClientWidth, 0);

    if Target is TJvDockCustomTabControl then
    begin
      DockTabRect.TopLeft := Point(0, ClientHeight - JvDockGetSysCaptionHeight);
      DockTabRect.BottomRight := Point(ClientWidth, ClientHeight);
    end
    else
      DockTabRect := Rect(0, 0, 0, 0);

    if PtInRect(DockCenterRect, MousePos) or
      PtInRect(DockTabRect, MousePos) then
    begin
      Result := alClient;
      DockRect := DockCenterRect;
      DockRect.BottomRight := Point(ClientWidth, ClientHeight);
    end
    else
    if PtInRect(DockLeftRect, MousePos) then
    begin
      Result := alLeft;
      DockRect := DockLeftRect;
      DockRect.Right := Min(ClientWidth div 2, Control.ClientWidth);
    end
    else
    if PtInRect(DockTopRect, MousePos) then
    begin
      Result := alTop;
      DockRect := DockTopRect;
      DockRect.Left := 0;
      DockRect.Right := ClientWidth;
      DockRect.Bottom := Min(ClientHeight div 2, Control.ClientHeight);
    end
    else
    if PtInRect(DockRightRect, MousePos) then
    begin
      Result := alRight;
      DockRect := DockRightRect;
      DockRect.Left := Max(ClientWidth div 2, ClientWidth - Control.ClientWidth);
    end
    else
    if PtInRect(DockBottomRect, MousePos) then
    begin
      Result := alBottom;
      DockRect := DockBottomRect;
      DockRect.Top := Max(ClientHeight div 2, ClientHeight - Control.ClientHeight);
    end;
    if Result = alNone then
      Exit;

    DockRect.TopLeft := ClientToScreen(DockRect.TopLeft);
    DockRect.BottomRight := ClientToScreen(DockRect.BottomRight);
  end;
end;

(*  (ahuser) not used - make Delphi 5 happy
procedure SetTabControlPreview(VIDSource: TJvDockVIDDragDockObject;
  TabControl: TJvDockVIDTabPageControl;
  State: TDragState; DropAlign: TAlign);

var
  I: Integer;
  Index: Integer;
begin
  if TabControl <> nil then
  begin
    if DropAlign = alClient then
    begin

      if TabControl.FTempSheet = nil then
      begin

        for I := VIDSource.SourceDockClientCount - 1 downto 0 do
        begin

          TabControl.FTempSheet := TJvDockVIDTabSheet.Create(TabControl);
          TabControl.FTempSheet.PageControl := TabControl;

          TabControl.FTempSheet.Caption := TJvTempWinControl(VIDSource.SourceDockClients[I]).Caption;
          Index := TabControl.FTabImageList.AddIcon(TForm(VIDSource.SourceDockClients[I]).Icon);
          if Index <> -1 then
            TabControl.FTempSheet.ImageIndex := Index;

          TabControl.FTempSheet.FIsSourceDockClient := True;
        end;

        TabControl.ActivePage := TabControl.FTempSheet;
        TabControl.Panel.SelectSheet := TabControl.FTempSheet;

        TabControl.Panel.FTempPages.Assign(TabControl.PageSheets);

        TabControl.ActivePage.Invalidate;

      end;
    end;

    if ((State = dsDragLeave) or (VIDSource.DropAlign <> alClient)) and (TabControl.FTempSheet <> nil) then
    begin

      for I := TabControl.PageCount - 1 downto 0 do
      begin
        if TJvDockVIDTabSheet(TabControl.Pages[I]).FIsSourceDockClient then
        begin

          Index := TabControl.Panel.FTempPages.IndexOf(TabControl.Pages[I]);

          if Index >= 0 then
          begin
            TabControl.Panel.FTempPages.Delete(Index);
            if TabControl.FTabImageList.Count > Index then
              TabControl.FTabImageList.Delete(Index);
          end;

          TabControl.Pages[I].Free;
        end;
      end;

      TabControl.FTempSheet := nil;

    end;

    TabControl.ParentForm.Caption := TabControl.ActivePage.Caption;

    if TabControl.ParentForm.HostDockSite is TJvDockCustomPanel then
      TabControl.ParentForm.HostDockSite.Invalidate;
  end;
end;
*)

//=== { TJvDockVIDStyle } ====================================================

constructor TJvDockVIDStyle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DockPanelClass := TJvDockVIDPanel;
  DockSplitterClass := TJvDockVIDSplitter;
  ConjoinPanelClass := TJvDockVIDConjoinPanel;
  TabDockClass := TJvDockVIDTabPageControl;
  DockPanelTreeClass := TJvDockVIDTree;
  DockPanelZoneClass := TJvDockVIDZone;
  ConjoinPanelTreeClass := TJvDockVIDTree;
  ConjoinPanelZoneClass := TJvDockVIDZone;
  ConjoinServerOptionClass := TJvDockVIDConjoinServerOption;
  TabServerOptionClass := TJvDockVIDTabServerOption;
end;

procedure TJvDockVIDStyle.FormDockOver(DockClient: TJvDockClient; Source: TJvDockDragDockObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  ARect: TRect;
begin
  with DockClient do
  begin
    Accept := EnableDock and EachOtherDock and
      IsDockable(ParentForm, Source.Control, Source.DropOnControl, Source.DropAlign);
    if State = dsDragMove then
    begin
      Source.DropAlign := ComputeVIDDockingRect(ParentForm, Source.Control, ARect, Point(X, Y));
      if Accept and (Source.DropAlign <> alNone) then
      begin
        if Source.DropAlign = alClient then
          Inc(ARect.Top, JvDockGetSysCaptionHeightAndBorderWidth + 1);
        Source.DockRect := ARect;
      end;
      gi_DockRect := ARect;
    end
    else
    if State = dsDragLeave then
      Source.DropAlign := alNone;
    if Source is TJvDockVIDDragDockObject then
    begin
      TJvDockVIDDragDockObject(Source).OldState := TJvDockVIDDragDockObject(Source).CurrState;
      TJvDockVIDDragDockObject(Source).CurrState := State;
    end;
  end;
end;

procedure TJvDockVIDStyle.FormGetSiteInfo(Source: TJvDockDragDockObject; DockClient: TJvDockClient;
  Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
  var CanDock: Boolean);
const
  DefExpandoRect = 20;
var
  CH_BW: Integer;
  ARect: TRect;
begin
  with DockClient do
  begin
    CanDock := IsDockable(ParentForm, Client, Source.DropOnControl, Source.DropAlign);
    if CanDock then
    begin
      GetWindowRect(ParentForm.Handle, InfluenceRect);
      if ParentForm.HostDockSite is TJvDockCustomPanel then
        Dec(InfluenceRect.Top, TJvDockCustomPanel(ParentForm.HostDockSite).JvDockManager.GrabberSize);
      if PtInRect(InfluenceRect, MousePos) then
      begin
        ARect := InfluenceRect;
        InflateRect(ARect, -DefExpandoRect, -DefExpandoRect);
        CH_BW := JvDockGetSysCaptionHeightAndBorderWidth;
        Inc(ARect.Top, CH_BW + 1);
        if PtInRect(ARect, MousePos) then
        begin
          InfluenceRect := Rect(0, 0, 0, 0);
          CanDock := False;
        end;
      end;
    end;
  end;
end;

procedure TJvDockVIDStyle.FormDockDrop(DockClient: TJvDockClient;
  Source: TJvDockDragDockObject; X, Y: Integer);
var
  ARect, DRect: TRect;
  DockType: TAlign;
  Host: TJvDockableForm;
  APanelDock: TWinControl;
  VIDSource: TJvDockVIDDragDockObject;
  I: Integer;
begin
  if Source is TJvDockVIDDragDockObject then
  begin
    TJvDockVIDDragDockObject(Source).CurrState := dsDragEnter;
    TJvDockVIDDragDockObject(Source).OldState := dsDragEnter;
  end;

  if IsDockable(DockClient.ParentForm, Source.Control, Source.DropOnControl, Source.DropAlign) then
  begin
    Host := nil;
    JvDockLockWindow(nil);
    try
      with DockClient do
      begin
        DockType := ComputeVIDDockingRect(DockClient.ParentForm, Source.Control, ARect, Point(X, Y));
        if ParentForm.HostDockSite is TJvDockPanel then
        begin
          if DockType = alClient then
          begin
            if Source.Control is TJvDockTabHostForm then
            begin
              APanelDock := ParentForm.HostDockSite;
              ARect := ParentForm.BoundsRect;
              ParentForm.ManualDock(TJvDockTabHostForm(Source.Control).PageControl, nil, alClient);
              TJvDockTabHostForm(Source.Control).PageControl.ActivePage.PageIndex := 0;
              Source.Control.BoundsRect := ARect;
              Source.Control.ManualDock(APanelDock, nil, alClient);
              if ParentForm.FormStyle = fsStayOnTop then
                TForm(Source.Control).FormStyle := fsStayOnTop;
            end
            else
            begin
              APanelDock := ParentForm.HostDockSite;
              DRect.TopLeft := ParentForm.HostDockSite.ClientToScreen(Point(0, 0));
              Host := CreateTabHostAndDockControl(ParentForm, Source.Control);
              SetDockSite(ParentForm, False);
              SetDockSite(TWinControl(Source.Control), False);
              Host.Top := DRect.Top;
              Host.Left := DRect.Left;
              Host.Visible := True;
              Host.ManualDock(APanelDock, nil, alClient);
            end;
          end
          else
          begin
            DRect := ParentForm.HostDockSite.BoundsRect;
            Source.Control.ManualDock(ParentForm.HostDockSite, nil, DockType);
            ParentForm.HostDockSite.BoundsRect := DRect;
            SetDockSite(TWinControl(Source.Control), False);
          end;
          Exit;
        end;

        if DockType = alClient then
        begin
          if Source.Control is TJvDockTabHostForm then
          begin
            APanelDock := ParentForm.HostDockSite;
            ARect := ParentForm.BoundsRect;
            ParentForm.ManualDock(TJvDockTabHostForm(Source.Control).PageControl, nil, alClient);
            TJvDockTabHostForm(Source.Control).PageControl.ActivePage.PageIndex := 0;
            Source.Control.BoundsRect := ARect;
            Source.Control.ManualDock(APanelDock, nil, alClient);
            if ParentForm.FormStyle = fsStayOnTop then
              TForm(Source.Control).FormStyle := fsStayOnTop;
            Exit;
          end
          else
          begin
            if Source is TJvDockVIDDragDockObject then
            begin
              VIDSource := TJvDockVIDDragDockObject(Source);
              DoFloatForm(Source.Control);
              FreeAllDockableForm;
              for I := 0 to VIDSource.SourceDockClientCount - 1 do
              begin
                VIDSource.Control := VIDSource.SourceDockClients[I];
                if Host = nil then
                  Host := DockClient.CreateTabHostAndDockControl(DockClient.ParentForm, Source.Control)
                else
                  Source.Control.ManualDock(TJvDockTabHostForm(Host).PageControl, nil, alClient);
              end;
              if not JvGlobalDockIsLoading and
                (TJvDockTabHostForm(Host).GetActiveDockForm <> nil) and
                GetParentForm(Host).Visible and
                TJvDockTabHostForm(Host).GetActiveDockForm.CanFocus then
                   TJvDockTabHostForm(Host).GetActiveDockForm.SetFocus;
              Host.Visible := True;
            end;
          end;
        end
        else
        if DockType <> alNone then
        begin
          Host := CreateConjoinHostAndDockControl(ParentForm, Source.Control, DockType);
          SetDockSite(ParentForm, False);
          SetDockSite(TWinControl(Source.Control), False);
          Host.Visible := True;
        end;

        if Host <> nil then
        begin
          Host.LRDockWidth := Source.Control.LRDockWidth;
          Host.TBDockHeight := Source.Control.TBDockHeight;
        end;
      end;
    finally
      JvDockUnLockWindow;
    end;
  end;
end;

procedure TJvDockVIDStyle.SetDockBaseControl(IsCreate: Boolean;
  DockBaseControl: TJvDockBaseControl);
var
  ADockClient: TJvDockClient;
begin
  if DockBaseControl is TJvDockClient then
  begin
    ADockClient := TJvDockClient(DockBaseControl);
    if IsCreate then
      ADockClient.DirectDrag := False;
  end;
end;

procedure TJvDockVIDStyle.FormStartDock(DockClient: TJvDockClient;
  var Source: TJvDockDragDockObject);
begin
  inherited FormStartDock(DockClient, Source);
  Source := TJvDockVIDDragDockObject.Create(DockClient.ParentForm);
{allows DockClient.OnCheckIsDockable event to fire once before docking, to block or allow drag/drop to this site. }
//  Source.DockClient := DockClient;
end;

procedure TJvDockVIDStyle.FormGetDockEdge(DockClient: TJvDockClient;
  Source: TJvDockDragDockObject; MousePos: TPoint; var DropAlign: TAlign);
var
  ARect: TRect;
begin
  DropAlign := ComputeVIDDockingRect(DockClient.ParentForm, Source.Control, ARect, MousePos);
end;

function TJvDockVIDStyle.DockClientWindowProc(DockClient: TJvDockClient;
  var Msg: TMessage): Boolean;
begin
  Result := inherited DockClientWindowProc(DockClient, Msg);
end;

procedure TJvDockVIDStyle.DoSystemInfoChange(Value: Boolean);
begin
  if Assigned(FSystemInfoChange) then
    FSystemInfoChange(Value);
end;

procedure TJvDockVIDStyle.SetAlwaysShowGrabber(const Value: Boolean);
begin
  if Value <> FAlwaysShowGrabber then
  begin
    FAlwaysShowGrabber := Value;
    Changed;
  end;
end;

//=== { TJvDockVIDPanel } ====================================================

procedure TJvDockVIDPanel.CustomDockDrop(Source: TJvDockDragDockObject; X, Y: Integer);
begin
  if Source.Control is TJvDockableForm then
    ShowDockPanel(True, Source.Control);
  if not ((Source.Control.HostDockSite <> nil) and
    (Source.DropOnControl = Source.Control.HostDockSite.Parent) and
    (Source.DropAlign = alClient)) then
  begin
    inherited CustomDockDrop(Source, X, Y);
    {$IFNDEF COMPILER9_UP}
    InvalidateDockHostSiteOfControl(Source.Control, False);
    {$ENDIF !COMPILER9_UP}
    if (Source.Control is TWinControl) and TWinControl(Source.Control).CanFocus then
      TWinControl(Source.Control).SetFocus;
    if (ControlCount > 0) and Assigned(Controls[0]) and (Controls[0] is TJvDockTabHostForm) then
    begin
      with TJvDockTabHostForm(Controls[0]) do
        if (GetActiveDockForm <> nil) and GetActiveDockForm.CanFocus then
          GetActiveDockForm.SetFocus;
    end;
  end;
end;

procedure TJvDockVIDPanel.CustomDockOver(Source: TJvDockDragDockObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  DropAlign: TAlign;
begin
  inherited CustomDockOver(Source, X, Y, State, Accept);
  if Accept and (Source is TJvDockVIDDragDockObject) then
    if State = dsDragMove then
    begin
      DropAlign := Source.DropAlign;
      JvDockManager.GetDockEdge(Source.DockRect, Source.DragPos, DropAlign, Source.Control);
    end;
end;

procedure TJvDockVIDPanel.CustomGetDockEdge(Source: TJvDockDragDockObject;
  MousePos: TPoint; var DropAlign: TAlign);
begin
end;

procedure TJvDockVIDPanel.CustomGetSiteInfo(Source: TJvDockDragDockObject;
  Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
  var CanDock: Boolean);
begin
  if VisibleDockClientCount = 0 then
    inherited CustomGetSiteInfo(Source, Client, InfluenceRect, MousePos, CanDock)
  else
  begin
    CanDock := IsDockable(Self, Client, Source.DropOnControl, Source.DropAlign);
    if CanDock then
      JvDockManager.GetSiteInfo(Client, InfluenceRect, MousePos, CanDock);
  end;
end;

procedure TJvDockVIDPanel.CustomStartDock(var Source: TJvDockDragDockObject);
begin
  Source := TJvDockVIDDragDockObject.Create(Self);
end;

procedure TJvDockVIDPanel.DockDrop(Source: TDragDockObject; X, Y: Integer);
begin
  inherited DockDrop(Source, X, Y);
end;

procedure TJvDockVIDPanel.UpdateCaption(Exclude: TControl);
begin
  inherited UpdateCaption(Exclude);
  Invalidate;
end;

//=== { TJvDockVIDTree } =====================================================

constructor TJvDockVIDTree.Create(DockSite: TWinControl;
  DockZoneClass: TJvDockZoneClass; ADockStyle: TJvDockObservableStyle);
begin
  inherited Create(DockSite, DockZoneClass, ADockStyle);
  FDropOnZone := nil;

  ButtonHeight := 11;
  ButtonWidth := 13;
  LeftOffset := 4;
  RightOffset := 4;
  TopOffset := 4;
  BottomOffset := 3;
  ButtonSplitter := 2;
  BorderWidth := 0;
  MinSize := 20;
  CaptionLeftOffset := 0;
  CaptionRightOffset := 0;
end;

function TJvDockVIDTree.GetDockGrabbersPosition: TJvDockGrabbersPosition;
begin
  Result := gpTop;
end;

function TJvDockVIDTree.GetTopGrabbersHTFlag(const MousePos: TPoint;
  out HTFlag: Integer; Zone: TJvDockZone): TJvDockZone;
begin
  if (MousePos.Y >= Zone.Top) and (MousePos.Y <= Zone.Top + GrabberSize) and
    (MousePos.X >= Zone.Left) and (MousePos.X <= Zone.Left + Zone.Width) then
  begin
    Result := Zone;
    with Zone.ChildControl do
    begin
      if PtInRect(Rect(
        Left + Width - ButtonWidth - RightOffset,
        Top - GrabberSize + TopOffset,
        Left + Width - RightOffset,
        Top - GrabberSize + TopOffset + ButtonHeight), MousePos) then
        HTFlag := HTCLOSE
      else
        HTFlag := HTCAPTION;
    end;
  end
  else
    Result := nil;
end;

procedure TJvDockVIDTree.InsertControl(Control: TControl; InsertAt: TAlign;
  DropCtl: TControl);
var
  I: Integer;
  Host: TJvDockTabHostForm;
  ChildCount: Integer;
  VIDSource: TJvDockVIDDragDockObject;
  TempControl: TControl;
  ARect: TRect;
  AZone: TJvDockZone;

  function CreateDockPageControl(Client: TControl): TJvDockTabHostForm;
  var
    Zone: TJvDockZone;
    TempCtl: TControl;
    TempPanel: TJvDockConjoinPanel;
    DockClient: TJvDockClient;
    APoint: TPoint;
  begin
    {$IFDEF JVDOCK_DEBUG}
    OutputDebugString('TJvDockVIDTree.InsertControl.CreateDockPageControl');
    {$ENDIF JVDOCK_DEBUG}
    Result := nil;
    Zone := FindControlZone(DropCtl);
    DockClient := FindDockClient(DropCtl);
    if (DockClient <> nil) and (Zone <> nil) then
    begin
      TempCtl := DropCtl;

      if Zone.ParentZone.Orientation = doHorizontal then
      begin
        if Zone.PrevSibling = nil then
        begin
          if Zone.NextSibling <> nil then
            DropCtl := Zone.NextSibling.ChildControl;
          InsertAt := alTop;
        end
        else
        begin
          DropCtl := Zone.PrevSibling.ChildControl;
          InsertAt := alBottom;
        end;
      end
      else
      if Zone.ParentZone.Orientation = doVertical then
      begin
        if Zone.PrevSibling = nil then
        begin
          if Zone.NextSibling <> nil then
            DropCtl := Zone.NextSibling.ChildControl;
          InsertAt := alLeft;
        end
        else
        begin
          DropCtl := Zone.PrevSibling.ChildControl;
          InsertAt := alRight;
        end;
      end;

      if TempCtl.HostDockSite is TJvDockConjoinPanel then
        TempPanel := TJvDockConjoinPanel(TempCtl.HostDockSite)
      else
        TempPanel := nil;

      Result := DockClient.CreateTabHostAndDockControl(TempCtl, Client);
      if TempPanel <> nil then

        TempPanel.ParentForm.UnDockControl := Result;

      SetDockSite(TWinControl(TempCtl), False);
      SetDockSite(TWinControl(Client), False);

      if DockSite.Align = alBottom then
        APoint := Point(0, -TempCtl.TBDockHeight)
      else
      if DockSite.Align = alRight then
        APoint := Point(-TempCtl.LRDockWidth, 0)
      else
        APoint := Point(0, 0);
      APoint := DockSite.ClientToScreen(APoint);
      Result.Left := APoint.X;
      Result.Top := APoint.Y;
      Result.UndockWidth := TempCtl.UndockWidth;
      Result.UndockHeight := TempCtl.UndockHeight;
      Result.LRDockWidth := TempCtl.LRDockWidth;
      Result.TBDockHeight := TempCtl.TBDockHeight + GrabberSize;

      Result.Visible := True;
    end;
  end;

begin
  {$IFDEF JVDOCK_DEBUG}
  OutputDebugString('TJvDockVIDTree.InsertControl');
  {$ENDIF JVDOCK_DEBUG}
  if not JvGlobalDockIsLoading then
    JvDockLockWindow(nil);
  try
    VIDSource := nil;
    if Control is TJvDockableForm then
    begin
      if InsertAt in [alClient] then
      begin
        if DropCtl is TJvDockTabHostForm then
        begin
          try
            VIDSource := TJvDockVIDDragDockObject.Create(Control);
            DoFloatForm(Control);
            FreeAllDockableForm;
            for I := VIDSource.SourceDockClientCount - 1 downto 0 do
            begin
              TempControl := VIDSource.SourceDockClients[I];
              TempControl.ManualDock(TJvDockTabHostForm(DropCtl).PageControl);
              if TempControl is TForm then
              begin
                TForm(TempControl).ActiveControl := nil;
                SetDockSite(TForm(TempControl), False);
              end;
            end;
          finally
            VIDSource.Free;
            JvGlobalDockManager.DragObject.Control := nil;
          end;
        end
        else
        begin
          if (DockSite is TJvDockCustomPanel) and (DockSite.VisibleDockClientCount > 1) and (DropCtl <> nil) then
          begin
            try
              VIDSource := TJvDockVIDDragDockObject.Create(Control);
              DoFloatForm(Control);
              FreeAllDockableForm;

              Host := CreateDockPageControl(VIDSource.SourceDockClients[0]);
              if Host <> nil then
              begin
                for I := VIDSource.SourceDockClientCount - 1 downto 1 do
                begin
                  TempControl := VIDSource.SourceDockClients[I];
                  TempControl.ManualDock(Host.PageControl);
                  if TempControl is TForm then
                  begin
                    TForm(TempControl).ActiveControl := nil;
                    SetDockSite(TForm(TempControl), False);
                  end;
                end;

                Host.ManualDock(DockSite, nil, InsertAt);
              end;
            finally
              VIDSource.Free;
              JvGlobalDockManager.DragObject.Control := nil;
            end;
          end
          else
            inherited InsertControl(Control, InsertAt, DropCtl);
        end;
      end
      else
      if Control is TJvDockConjoinHostForm then
      begin
        TJvTempWinControl(TJvDockableForm(Control).DockableControl).DockManager.ResetBounds(True);
        InsertControlFromConjoinHost(Control, InsertAt, DropCtl);
      end
      else
        inherited InsertControl(Control, InsertAt, DropCtl);
    end
    else
    begin
      if InsertAt in [alLeft, alTop] then
        DropDockSize := DropDockSize + SplitterWidth div 2;
      if InsertAt in [alClient] then
      begin
        if DropCtl is TJvDockTabHostForm then
          Control.ManualDock(TJvDockTabHostForm(DropCtl).PageControl, nil, alClient)
        else
        if TopZone.ChildZones <> nil then
        begin
          ChildCount := TopZone.ChildCount;
          if DropCtl <> nil then
          begin
            ARect := DropCtl.BoundsRect;
            AZone := FindControlZone(DropCtl);

            if DropCtl.DockOrientation = doHorizontal then
            begin
              if ((AZone <> nil) and (AZone.ZoneLimit <> DockSite.Height)) then
                ARect.Bottom := ARect.Bottom + SplitterWidth;
            end
            else
            begin
              if ((AZone <> nil) and (AZone.ZoneLimit <> DockSite.Width)) then
                ARect.Right := ARect.Right + SplitterWidth;
            end;
            DockRect := ARect;
          end
          else
            DockRect := Rect(0, 0, TopZone.Width, TopZone.Height);

          Host := CreateDockPageControl(Control);
          if Host <> nil then
            if (ChildCount >= 2) or (DockSite is TJvDockPanel) then
            begin
              if InsertAt in [alLeft, alRight] then
                DropDockSize := DockRect.Right - DockRect.Left
              else
                DropDockSize := DockRect.Bottom - DockRect.Top + GrabberSize;

              LockDropDockSize;
              Host.ManualDock(DockSite, DropCtl, InsertAt);

              UnlockDropDockSize;
            end
            else
              Host.BoundsRect := DockSite.Parent.BoundsRect;
        end
        else
          inherited InsertControl(Control, InsertAt, DropCtl);
      end
      else
        inherited InsertControl(Control, InsertAt, DropCtl);

      { (rb) no idea what gi_DockRect should be doing, but prevent it is used
        before it is set (by checking whether it is empty). Using it when the rect
        is empty causes align problems }
      if not IsRectEmpty(gi_DockRect) then
        DockRect := gi_DockRect;
    end;
    ForEachAt(nil, UpdateZone);
  finally
    if not JvGlobalDockIsLoading then
      JvDockUnLockWindow;
  end;
end;

procedure TJvDockVIDTree.InsertControlFromConjoinHost(Control: TControl;
  InsertAt: TAlign; DropCtl: TControl);
const
  OrientArray: array [TAlign] of TDockOrientation =
    (doNoOrient, doHorizontal, doHorizontal, doVertical, doVertical, doNoOrient, doNoOrient);
  MakeLast: array [TAlign] of Boolean =
    (False, False, True, False, True, False, False);
  ReverseAt: array [TAlign] of TAlign =
    (alClient, alBottom, alTop, alRight, alLeft, alNone, alCustom);
var
  Stream: TMemoryStream;
  TopOrientation: TDockOrientation;
  InsertOrientation: TDockOrientation;
  CurrentOrientation: TDockOrientation;
  ZoneLimit: Integer;
  Level, LastLevel, I: Integer;
  Zone, NextZone: TJvDockZone;
  DropCtlZone, LastZone: TJvDockZone;
  OffsetXYLimitArr: array [TDockOrientation] of Integer;
  ControlXYLimitArr: array [TDockOrientation] of Integer;

  procedure ReadZone(SetZone: Boolean);
  var
    I: Integer;
  begin
    with Stream do
    begin
      Read(Level, SizeOf(Level));
      if Level = TreeStreamEndFlag then
        Exit;
      Zone := DockZoneClass.Create(Self);
      CustomLoadZone(Stream, Zone);
      ZoneLimit := Zone.ZoneLimit;
    end;
    if SetZone then
    begin
      if Level = LastLevel then
      begin
        Zone.NextSibling := LastZone.NextSibling;
        if LastZone.NextSibling <> nil then
          LastZone.NextSibling.PrevSibling := Zone;
        LastZone.NextSibling := Zone;
        Zone.PrevSibling := LastZone;
        Zone.ParentZone := LastZone.ParentZone;
      end
      else
      if Level > LastLevel then
      begin
        LastZone.ChildZones := Zone;
        Zone.ParentZone := LastZone;
        InsertOrientation := LastZone.Orientation;
      end
      else
      if Level < LastLevel then
      begin
        NextZone := LastZone;
        for I := 1 to LastLevel - Level do
          NextZone := NextZone.ParentZone;
        Zone.NextSibling := NextZone.NextSibling;
        if NextZone.NextSibling <> nil then
          NextZone.NextSibling.PrevSibling := Zone;
        NextZone.NextSibling := Zone;
        Zone.PrevSibling := NextZone;
        Zone.ParentZone := NextZone.ParentZone;
        InsertOrientation := Zone.ParentZone.Orientation;
      end;
      Zone.ZoneLimit := OffsetXYLimitArr[InsertOrientation] + ZoneLimit;
    end;
    LastLevel := Level;
    LastZone := Zone;
  end;

begin
  ControlXYLimitArr[doNoOrient] := 0;
  ControlXYLimitArr[doHorizontal] := DockRect.Bottom - DockRect.Top;
  ControlXYLimitArr[doVertical] := DockRect.Right - DockRect.Left;

  Stream := TMemoryStream.Create;
  if Control is TJvDockConjoinHostForm then
    TJvDockConjoinHostForm(Control).Panel.JvDockManager.SaveToStream(Stream);
  Stream.Position := 0;

  BeginUpdate;
  try
    Stream.Read(I, SizeOf(I));
    Stream.Position := Stream.Position + 8;
    Stream.Read(TopOrientation, SizeOf(TopOrientation));
    Stream.Read(ZoneLimit, SizeOf(ZoneLimit));
    IgnoreZoneInfor(Stream);
    if (DropCtl = nil) and (TopZone.ChildCount = 1) then
      DropCtl := TopZone.ChildZones.ChildControl;
    DropCtlZone := FindControlZone(DropCtl);
    if InsertAt in [alClient, alNone] then
      InsertAt := alRight;
    InsertOrientation := OrientArray[InsertAt];
    if TopZone.ChildCount = 0 then
    begin
      TopZone.Orientation := TopOrientation;
      InsertOrientation := TopOrientation;
    end
    else
    if TopZone.ChildCount = 1 then
    begin
      TopZone.Orientation := InsertOrientation;
      case InsertOrientation of
        doHorizontal:
          begin
            TopZone.ZoneLimit := TopZone.ChildZones.Width;
            TopXYLimit := TopZone.ChildZones.Height;
          end;
        doVertical:
          begin
            TopZone.ZoneLimit := TopZone.ChildZones.Height;
            TopXYLimit := TopZone.ChildZones.Width;
          end;
      end;
    end;

    if DropCtlZone <> nil then
      CurrentOrientation := DropCtlZone.ParentZone.Orientation
    else
      CurrentOrientation := TopZone.Orientation;

    if InsertOrientation = doHorizontal then
      DropDockSize := DockRect.Bottom - DockRect.Top
    else
    if InsertOrientation = doVertical then
      DropDockSize := DockRect.Right - DockRect.Left
    else
      DropDockSize := 0;

    OffsetXYLimitArr[doNoOrient] := 0;
    if DropCtlZone <> nil then
    begin
      OffsetXYLimitArr[doHorizontal] := DropCtlZone.TopLeft[doHorizontal] +
        Integer(MakeLast[InsertAt]) * (DropCtlZone.HeightWidth[doHorizontal] - ControlXYLimitArr[doHorizontal]);
      if (FDropOnZone <> nil) and (InsertOrientation = doHorizontal) then
        OffsetXYLimitArr[doHorizontal] := FDropOnZone.ZoneLimit - Round((FDropOnZone.ZoneLimit -
          FDropOnZone.ParentZone.ChildZones.LimitBegin) * (DropDockSize + BorderWidth) /
            (FDropOnZone.ParentZone.Height));
      OffsetXYLimitArr[doVertical] := DropCtlZone.TopLeft[doVertical] +
        Integer(MakeLast[InsertAt]) * (DropCtlZone.HeightWidth[doVertical] - ControlXYLimitArr[doVertical]);
      if (FDropOnZone <> nil) and (InsertOrientation = doVertical) then
        OffsetXYLimitArr[doVertical] := FDropOnZone.ZoneLimit - Round((FDropOnZone.ZoneLimit -
          FDropOnZone.ParentZone.ChildZones.LimitBegin) * (DropDockSize + BorderWidth) /
            (FDropOnZone.ParentZone.Width));
    end
    else
    begin
      if TopZone.VisibleChildCount = 0 then
      begin
        OffsetXYLimitArr[doHorizontal] := 0;
        OffsetXYLimitArr[doVertical] := 0;
      end
      else
      begin
        OffsetXYLimitArr[doHorizontal] := Integer(MakeLast[InsertAt]) * ControlXYLimitArr[doHorizontal];
        OffsetXYLimitArr[doVertical] := Integer(MakeLast[InsertAt]) * ControlXYLimitArr[doVertical];
      end;
    end;

    if TopOrientation <> InsertOrientation then
    begin
      LastZone := DockZoneClass.Create(Self);
      if InsertOrientation <> CurrentOrientation then
        InsertNewParent(LastZone, DropCtlZone, InsertOrientation, MakeLast[InsertAt], True)
      else
        InsertSibling(LastZone, DropCtlZone, MakeLast[InsertAt], True);
      LastZone.Orientation := TopOrientation;
      LastLevel := 0;
    end
    else
    begin
      LastLevel := 1;
      if TopZone.ChildCount > 0 then
      begin
        ReadZone(False);
        if InsertOrientation <> CurrentOrientation then
          InsertNewParent(LastZone, DropCtlZone, InsertOrientation, MakeLast[InsertAt], True)
        else
          InsertSibling(LastZone, DropCtlZone, MakeLast[InsertAt], True);
        LastZone.ZoneLimit := ZoneLimit + OffsetXYLimitArr[InsertOrientation];
      end
      else
      begin
        LastLevel := 0;
        LastZone := TopZone;
      end;
    end;

    OffsetXYLimitArr[doHorizontal] := LastZone.TopLeft[doHorizontal];
    OffsetXYLimitArr[doVertical] := LastZone.TopLeft[doVertical];

    while True do
    begin
      ReadZone(True);
      if Level = TreeStreamEndFlag then
        Break;
    end;
  finally
    Stream.Free;
    EndUpdate;
  end;
  SetNewBounds(nil);
end;

procedure TJvDockVIDTree.DrawDockGrabber(Control: TWinControl; const ARect: TRect);
const
  TextAlignment: array [TAlignment] of UINT =
    (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  Option: TJvDockVIDConjoinServerOption;
  DrawRect: TRect;
  uFormat: UINT;
  IsActive: Boolean;
begin
  Assert(Assigned(Control));

  case GrabbersPosition of
    gpTop:
      if Assigned(DockStyle) and (DockStyle.ConjoinServerOption is TJvDockVIDConjoinServerOption) then
      begin
        Option := TJvDockVIDConjoinServerOption(DockStyle.ConjoinServerOption);

        IsActive := Assigned(Screen.ActiveControl) and Screen.ActiveControl.Focused and
          Control.ContainsControl(Screen.ActiveControl);
        DrawRect := ARect;

        Inc(DrawRect.Top, PPIScale(2));
        DrawRect.Bottom := DrawRect.Top + GrabberSize - PPIScale(3);
        if IsActive then
          PaintGradientBackground(Canvas, DrawRect, Option.ActiveTitleStartColor,
            Option.ActiveTitleEndColor, Option.ActiveTitleVerticalGradient)
        else
          PaintGradientBackground(Canvas, DrawRect, Option.InactiveTitleStartColor,
            Option.InactiveTitleEndColor, Option.InactiveTitleVerticalGradient);
        Canvas.Brush.Style := bsClear; // body already painted
        PaintDockGrabberRect(Canvas, Control, DrawRect, Option.ActiveDockGrabber);

        if IsActive then
          Canvas.Font.Assign(Option.ActiveFont)
        else
          Canvas.Font.Assign(Option.InactiveFont);
        // Scale font
        Canvas.Font.Height := MulDiv(Canvas.Font.Height, FCurrentPPI, Canvas.Font.PixelsPerInch);

        Canvas.Brush.Style := bsClear;
        GetCaptionRect(DrawRect);
        uFormat := DT_VCENTER or DT_SINGLELINE or
          (Cardinal(Ord(Option.TextEllipsis)) * DT_END_ELLIPSIS) or TextAlignment[Option.TextAlignment];
        { DIRTY cast }
        DrawText(Canvas.Handle, PChar(TForm(Control).Caption), -1, DrawRect, uFormat);
        if ShowCloseButtonOnGrabber or not (Control is TJvDockTabHostForm) then
          DrawCloseButton(Canvas, FindControlZone(Control), ARect.Right - RightOffset - ButtonWidth, ARect.Top + TopOffset);
      end;
    {$IFDEF JVDOCK_DEBUG}
    gpBottom:
      OutputDebugString('GrabbersPosition = gpBottom - Not supported');
    gpRight:
      OutputDebugString('GrabbersPosition = gpRight - Not supported');
    gpLeft:
      OutputDebugString('GrabbersPosition=gpLeft - Not supported');
    {$ENDIF JVDOCK_DEBUG}
  end;
end;

procedure TJvDockVIDTree.ResetBounds(Force: Boolean);
var
  R: TRect;
begin
  if not (csLoading in DockSite.ComponentState) and
    (TopZone <> nil) and (DockSite.DockClientCount > 0) then
  begin
    R := DockSite.ClientRect;
    if DockSite is TJvDockConjoinPanel then
    begin
      if R.Right = R.Left then
        Inc(R.Right, DockSite.Parent.UndockWidth);
      if R.Bottom = R.Top then
        Inc(R.Bottom, DockSite.Parent.UndockHeight);
    end;
    if Force or (not CompareMem(@R, @PreviousRect, SizeOf(TRect))) then
    begin
      case TopZone.Orientation of
        doHorizontal:
          begin
            if R.Right - R.Left > 0 then
              TopZone.ZoneLimit := R.Right - R.Left;
            if R.Bottom - R.Top > 0 then
              TopXYLimit := R.Bottom - R.Top;
          end;
        doVertical:
          begin
            if R.Bottom - R.Top > 0 then
              TopZone.ZoneLimit := R.Bottom - R.Top;
            if R.Right - R.Left > 0 then
              TopXYLimit := R.Right - R.Left;
          end;
      end;
      if DockSite.DockClientCount > 0 then
      begin
        if not JvGlobalDockIsLoading then
        begin
          if (R.Bottom - R.Top > 0) and (PreviousRect.Bottom - PreviousRect.Top > 0) then
            ScaleBy := (R.Bottom - R.Top) / (PreviousRect.Bottom - PreviousRect.Top)
          else
            ScaleBy := 1;

          ShiftScaleOrientation := doHorizontal;

          if (UpdateCount = 0) and (ScaleBy <> 1) then
            ForEachAt(nil, ScaleZone, tskForward);

          if (R.Right - R.Left > 0) and (PreviousRect.Right - PreviousRect.Left > 0) then
            ScaleBy := (R.Right - R.Left) / (PreviousRect.Right - PreviousRect.Left)
          else
            ScaleBy := 1;

          ShiftScaleOrientation := doVertical;

          if (UpdateCount = 0) and (ScaleBy <> 1) then
            ForEachAt(nil, ScaleZone, tskForward);
        end;

        SetNewBounds(nil);
        if UpdateCount = 0 then
          ForEachAt(nil, UpdateZone, tskForward);

        PreviousRect := R;
      end;
    end;
  end;
end;

procedure TJvDockVIDTree.DrawSplitterRect(const ARect: TRect);
begin
  inherited DrawSplitterRect(ARect);
end;

procedure TJvDockVIDTree.WindowProc(var Msg: TMessage);
var
  Align: TAlign;
begin
  if Msg.Msg = CM_DOCKNOTIFICATION then
  begin
    with TCMDockNotification(Msg) do
    begin
      if NotifyRec.ClientMsg = CM_INVALIDATEDOCKHOST then
        InvalidateDockSite(TControl(NotifyRec.MsgWParam))
      else
        inherited;
    end;
  end
  else
  if Msg.Msg = CM_DOCKCLIENT then
  begin
    { (rb) no idea what gi_DockRect should be doing, but prevent it is used
      before it is set (by checking whether it is empty). Using it when the rect
      is empty causes align problems }
    if not IsRectEmpty(gi_DockRect) then
    begin
      Align := TCMDockClient(Msg).DockSource.DropAlign;
      TCMDockClient(Msg).DockSource.DockRect := gi_DockRect;
      GetDockEdge(gi_DockRect, TCMDockClient(Msg).DockSource.DragPos, Align, TCMDockClient(Msg).DockSource.Control);
    end;
  end;
  inherited WindowProc(Msg);
end;

procedure TJvDockVIDTree.SplitterMouseUp;
var
  OldLimit: Integer;
  Zone: TJvDockZone;
begin
  Mouse.Capture := 0;
  DrawSizeSplitter;
  ReleaseDC(SizingWnd, SizingDC);

  OldLimit := SizingZone.ZoneLimit;

  ShiftScaleOrientation := SizingZone.ParentZone.Orientation;
  if SizingZone.ParentZone.Orientation = doHorizontal then
    SizingZone.ZoneLimit := SizePos.Y + (SplitterWidth div 2)
  else
    SizingZone.ZoneLimit := SizePos.X + (SplitterWidth div 2);

  ParentLimit := SizingZone.LimitBegin;
  if OldLimit - ParentLimit > 0 then
    ScaleBy := (SizingZone.ZoneLimit - ParentLimit) / (OldLimit - ParentLimit)
  else
    ScaleBy := 1;

  if SizingZone.ChildZones <> nil then
    ForEachAt(SizingZone.ChildZones, ScaleChildZone, tskForward);

  Zone := SizingZone;
  while (Zone.NextSibling <> nil) and (not Zone.NextSibling.Visibled) do
  begin
    Zone.NextSibling.ZoneLimit := SizingZone.ZoneLimit;
    Zone := Zone.NextSibling;
  end;

  if SizingZone.NextSibling <> nil then
  begin
    if SizingZone.NextSibling.ZoneLimit - OldLimit > 0 then
      ScaleBy := (SizingZone.NextSibling.ZoneLimit - SizingZone.ZoneLimit) / (SizingZone.NextSibling.ZoneLimit -
        OldLimit)
    else
      ScaleBy := 1;
    ParentLimit := SizingZone.NextSibling.ZoneLimit;

    if SizingZone.NextSibling.ChildZones <> nil then
      ForEachAt(SizingZone.NextSibling.ChildZones, ScaleSiblingZone, tskForward);
  end;

  SetNewBounds(SizingZone.ParentZone);
  ForEachAt(SizingZone.ParentZone, UpdateZone, tskForward);
  SizingZone := nil;
end;

procedure TJvDockVIDTree.DrawDockSiteRect;
begin
end;

procedure TJvDockVIDTree.InsertSibling(NewZone, SiblingZone: TJvDockZone;
  InsertLast, Update: Boolean);
begin
  if FDropOnZone <> nil then
    SiblingZone := FDropOnZone;
  inherited InsertSibling(NewZone, SiblingZone, InsertLast, Update);
end;

procedure TJvDockVIDTree.PositionDockRect(Client, DropCtl: TControl;
  DropAlign: TAlign; var DockRect: TRect);
label
  LBDropCtlExist;
var
  VisibleClients, NewX, NewY, NewWidth, NewHeight: Integer;
  Zone: TJvDockZone;
  HTFlag: Integer;
  MousePos: TPoint;
  Scale: Double;
  CtrlRect: TRect;

  procedure DockOverSplitter;
  begin
    NewX := Zone.ParentZone.Left;
    NewY := Zone.ParentZone.Top;
    NewWidth := Zone.ParentZone.Width;
    NewHeight := Zone.ParentZone.Height;
    case Zone.ParentZone.Orientation of
      doHorizontal:
        begin
          Scale := (Zone.ZoneLimit - Zone.ParentZone.ChildZones.LimitBegin) / NewHeight;
          NewHeight := Min(NewHeight div 2, Client.ClientHeight);
          NewY := Zone.ZoneLimit - Round(NewHeight * Scale);
        end;
      doVertical:
        begin
          Scale := (Zone.ZoneLimit - Zone.ParentZone.ChildZones.LimitBegin) / NewWidth;
          NewWidth := Min(NewWidth div 2, Client.ClientWidth);
          NewX := Zone.ZoneLimit - Round(NewWidth * Scale);
        end;
    end;
    DockRect := Bounds(NewX, NewY, NewWidth, NewHeight);
    if Zone.Visibled then
    begin
      if Zone.ParentZone.Orientation = doHorizontal then
        JvGlobalDockManager.DragObject.DropAlign := alBottom
      else
      if Zone.ParentZone.Orientation = doVertical then
        JvGlobalDockManager.DragObject.DropAlign := alRight;
      JvGlobalDockManager.DragObject.DropOnControl := Zone.ChildControl;
      FDropOnZone := Zone;
    end;
  end;

begin
  if DropAlign = alNone then
    DropAlign := alClient;
  VisibleClients := DockSite.VisibleDockClientCount;
  FDropOnZone := nil;

  if JvGlobalDockManager.DragObject <> nil then
    MousePos := JvGlobalDockManager.DragObject.DragPos
  else
    MousePos := Client.ScreenToClient(Mouse.CursorPos);

  MapWindowPoints(0, DockSite.Handle, MousePos, 2);
  Zone := InternalHitTest(MousePos, HTFlag);
  if Zone <> nil then
    if Zone.ChildControl <> nil then
      if (HTFlag = HTCAPTION) or (HTFlag = HTCLOSE) then
      begin
        DockRect := Zone.ChildControl.BoundsRect;
        JvGlobalDockManager.DragObject.DropAlign := alClient;
        if Zone.ChildControl is TJvDockTabHostForm then
        begin
          if JvGlobalDockManager.DragObject is TJvDockVIDDragDockObject then
            TJvDockVIDDragDockObject(JvGlobalDockManager.DragObject).FDropTabControl :=
              TJvDockVIDTabPageControl(TJvDockTabHostForm(Zone.ChildControl).PageControl);
        end
        else
        begin
          if JvGlobalDockManager.DragObject is TJvDockVIDDragDockObject then
            TJvDockVIDDragDockObject(JvGlobalDockManager.DragObject).FDropTabControl := nil;
        end;
      end;

  if DropCtl = nil then
  begin
    if Zone <> nil then
    begin
      if Zone.ChildControl <> nil then
      begin
        if (HTFlag = HTCAPTION) or (HTFlag = HTCLOSE) then
          JvGlobalDockManager.DragObject.DropOnControl := Zone.ChildControl
        else
        if HTFlag = HTCLIENT then
        begin
          DropCtl := Zone.ChildControl;
          goto LBDropCtlExist;
        end
        else
        if HTFlag = HTSPLITTER then
          DockOverSplitter;
      end
      else
      if HTFlag = HTSPLITTER then
      begin
        DockOverSplitter;
      end
      else
        Exit;
    end
    else
    begin
      DockRect := Rect(0, 0, DockSite.ClientWidth, DockSite.ClientHeight);

      if VisibleClients > 0 then
        case DropAlign of
          alLeft:
            DockRect.Right := DockRect.Right div 2;
          alRight:
            DockRect.Left := DockRect.Right div 2;
          alTop:
            DockRect.Bottom := DockRect.Bottom div 2;
          alBottom:
            DockRect.Top := DockRect.Bottom div 2;
        end;
    end;
  end
  else
  begin

  LBDropCtlExist:
    Zone := FindControlZone(DropCtl);
    CtrlRect := DockRect;
    MapWindowPoints(0, DockSite.Handle, CtrlRect, 2);
    if Zone <> nil then
    begin
      if Zone.ParentZone.Orientation = doVertical then
      begin
        if (DropAlign = alRight) and (Zone.NextSibling <> nil) then
        begin
          DockOverSplitter;
          MapWindowPoints(DockSite.Handle, 0, DockRect, 2);
          Exit;
        end
        else
        if (DropAlign = alLeft) and (Zone.PrevSibling <> nil) then
        begin
          Zone := Zone.PrevSibling;
          DockOverSplitter;
          MapWindowPoints(DockSite.Handle, 0, DockRect, 2);
          Exit;
        end
        else
        begin
          if DropAlign in [alLeft, alRight] then
            CtrlRect := Bounds(Zone.ParentZone.Left, Zone.ParentZone.Top, Zone.ParentZone.Width, Zone.ParentZone.Height)
          else
          if DropAlign in [alTop, alBottom, alClient] then
          begin
            CtrlRect := DropCtl.BoundsRect;
            Dec(CtrlRect.Top, GrabberSize);
          end;
          OffsetRect(CtrlRect, 0, GrabberSize);
        end;
      end
      else
      if Zone.ParentZone.Orientation = doHorizontal then
      begin
        if (DropAlign = alBottom) and (Zone.NextSibling <> nil) then
        begin
          DockOverSplitter;
          MapWindowPoints(DockSite.Handle, 0, DockRect, 2);
          Exit;
        end
        else
        if (DropAlign = alTop) and (Zone.PrevSibling <> nil) then
        begin
          Zone := Zone.PrevSibling;
          DockOverSplitter;
          MapWindowPoints(DockSite.Handle, 0, DockRect, 2);
          Exit;
        end
        else
        begin
          if DropAlign in [alTop, alBottom] then
            CtrlRect := Bounds(Zone.ParentZone.Left, Zone.ParentZone.Top, Zone.ParentZone.Width, Zone.ParentZone.Height)
          else
          if DropAlign in [alLeft, alRight, alClient] then
          begin
            CtrlRect := DropCtl.BoundsRect;
            Dec(CtrlRect.Top, GrabberSize);
          end;
          OffsetRect(CtrlRect, 0, GrabberSize);
        end;
      end
      else
      begin
        CtrlRect := DropCtl.BoundsRect;
        Dec(CtrlRect.Top, GrabberSize);
        OffsetRect(CtrlRect, 0, GrabberSize);
      end;

      NewX := CtrlRect.Left;
      NewY := CtrlRect.Top - GrabberSize;
      NewWidth := CtrlRect.Right - CtrlRect.Left;
      NewHeight := CtrlRect.Bottom - CtrlRect.Top;
      if DropAlign in [alLeft, alRight] then
        NewWidth := Min(Client.UndockWidth, NewWidth div 2)
      else
      if DropAlign in [alTop, alBottom] then
        NewHeight := Min(Client.UndockHeight, NewHeight div 2);
      case DropAlign of
        alRight:
          Inc(NewX, CtrlRect.Right - CtrlRect.Left - NewWidth);
        alBottom:
          Inc(NewY, CtrlRect.Bottom - CtrlRect.Top - NewHeight);
      end;
      DockRect := Bounds(NewX, NewY, NewWidth, NewHeight);
      if DropAlign = alClient then
        DockRect := Bounds(NewX, NewY, NewWidth, NewHeight);
      if DropAlign = alNone then
      begin
      end;
    end;
  end;
  MapWindowPoints(DockSite.Handle, 0, DockRect, 2);
end;

function TJvDockVIDTree.GetDockEdge(DockRect: TRect; MousePos: TPoint;
  var DropAlign: TAlign; Control: TControl): TControl;
begin
  Result := inherited GetDockEdge(DockRect, MousePos, DropAlign, Control);
  if FLockDropDockSizeCount = 0 then
  begin
    if DropAlign in [alLeft, alRight] then
      DropDockSize := DockRect.Right - DockRect.Left
    else
    if DropAlign in [alTop, alBottom] then
      DropDockSize := DockRect.Bottom - DockRect.Top
    else
      DropDockSize := 0;
    Self.DockRect := DockRect;
  end;
end;

procedure TJvDockVIDTree.InsertNewParent(NewZone, SiblingZone: TJvDockZone;
  ParentOrientation: TDockOrientation; InsertLast, Update: Boolean);
begin
  if FDropOnZone <> nil then
  begin
    SiblingZone := FDropOnZone;
    InsertSibling(NewZone, SiblingZone, InsertLast, Update);
  end
  else
    inherited InsertNewParent(NewZone, SiblingZone, ParentOrientation,
      InsertLast, Update);
end;

procedure TJvDockVIDTree.RemoveZone(Zone: TJvDockZone; Hide: Boolean);
begin
  if (FDropOnZone <> nil) and
    ((FDropOnZone.NextSibling = Zone) or (FDropOnZone = Zone)) then
    FDropOnZone := nil;
  inherited RemoveZone(Zone, Hide);
end;

procedure TJvDockVIDTree.GetSiteInfo(Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
var
  Zone: TJvDockZone;
  HTFlag: Integer;
  Pos: TPoint;
  Align: TAlign;
begin
  Pos := DockSite.ScreenToClient(MousePos);
  Zone := InternalHitTest(Pos, HTFlag);
  if Zone <> nil then
  begin
    if HTFlag = HTSPLITTER then
    begin
      InfluenceRect := GetSplitterRect(Zone);
      MapWindowPoints(DockSite.Handle, 0, InfluenceRect, 2);
    end
    else
    begin
      Pos := MousePos;
      if Zone.ChildControl <> nil then
        Pos := Zone.ChildControl.ScreenToClient(MousePos);
      Align := ComputeVIDDockingRect(Zone.ChildControl, Client, InfluenceRect, Pos);
      if (Align = alNone) or (Client = Zone.ChildControl) then
      begin
        InfluenceRect := Rect(0, 0, 0, 0);
        CanDock := False;
      end
      else
      begin
        if Zone.ParentZone.Orientation = doVertical then
        begin
          if (Align = alRight) and (Zone.NextSibling <> nil) and (Zone.NextSibling.Visibled) then
          begin
            InfluenceRect := GetSplitterRect(Zone);
            InflateRect(InfluenceRect, DefExpandoRect, 0);
          end
          else
          if (Align = alLeft) and (Zone.PrevSibling <> nil) and (Zone.PrevSibling.Visibled) then
          begin
            InfluenceRect := GetSplitterRect(Zone.PrevSibling);
            InflateRect(InfluenceRect, DefExpandoRect, 0);
          end
          else
            Exit;
        end
        else
        if Zone.ParentZone.Orientation = doHorizontal then
        begin
          if (Align = alBottom) and (Zone.NextSibling <> nil) and (Zone.NextSibling.Visibled) then
          begin
            InfluenceRect := GetSplitterRect(Zone);
            InflateRect(InfluenceRect, 0, DefExpandoRect);
          end
          else
          if (Align = alTop) and (Zone.PrevSibling <> nil) and (Zone.PrevSibling.Visibled) then
          begin
            InfluenceRect := GetSplitterRect(Zone.PrevSibling);
            InflateRect(InfluenceRect, 0, DefExpandoRect);
          end
          else
            Exit;
        end
        else
          Exit;
      end;
      MapWindowPoints(DockSite.Handle, 0, InfluenceRect, 2);
    end;
  end
  else
  begin
    InfluenceRect := Rect(0, 0, 0, 0);
    CanDock := False;
  end;
end;

procedure TJvDockVIDTree.LockDropDockSize;
begin
  Inc(FLockDropDockSizeCount);
end;

procedure TJvDockVIDTree.UnlockDropDockSize;
begin
  Dec(FLockDropDockSizeCount);
  if FLockDropDockSizeCount < 0 then
    FLockDropDockSizeCount := 0;
end;

procedure TJvDockVIDTree.PaintDockGrabberRect(Canvas: TCanvas;
  Control: TWinControl; const ARect: TRect; PaintAlways: Boolean = False);
begin
end;

procedure TJvDockVIDTree.SetCaptionLeftOffset(const Value: Integer);
begin
  FCaptionLeftOffset := Value;
end;

procedure TJvDockVIDTree.SetCaptionRightOffset(const Value: Integer);
begin
  FCaptionRightOffset := Value;
end;

procedure TJvDockVIDTree.DrawCloseButton(Canvas: TCanvas; Zone: TJvDockZone; Left, Top: Integer);
var
  AZone: TJvDockAdvZone;
  ADockClient: TJvDockClient;
  {$IFDEF JVCLThemesEnabled}
  Details: TThemedElementDetails;
  CurrentThemeType: TThemedWindow;
  {$ENDIF JVCLThemesEnabled}
begin
  AZone := TJvDockAdvZone(Zone);
  if AZone <> nil then
  begin
    ADockClient := FindDockClient(Zone.ChildControl);
    if (ADockClient <> nil) and not ADockClient.EnableCloseButton then
      Exit;
    {$IFDEF JVCLThemesEnabled}
    if StyleServices.Available and StyleServices.Enabled then
    begin
      if GrabberSize <= 18 then
      begin
        CurrentThemeType := twSmallCloseButtonNormal;
        if AZone.CloseBtnDown then
          CurrentThemeType := twSmallCloseButtonPushed;
      end
      else
      begin
        CurrentThemeType := twCloseButtonNormal;
        if AZone.CloseBtnDown then
          CurrentThemeType := twCloseButtonPushed;
      end;
      Details := StyleServices.GetElementDetails(CurrentThemeType);
      StyleServices.DrawElement(Canvas.Handle, Details, Rect(Left, Top, Left + ButtonWidth, Top + ButtonHeight));
    end
    else
      {$ENDIF JVCLThemesEnabled}
      DrawFrameControl(Canvas.Handle, Rect(Left, Top, Left + ButtonWidth,
        Top + ButtonHeight), DFC_CAPTION, DFCS_CAPTIONCLOSE or Integer(AZone.CloseBtnDown) * DFCS_PUSHED)
  end;
end;

function TJvDockVIDTree.GetCaptionLeftOffset: Integer;
begin
  Result := PPIScale(FCaptionLeftOffset);
end;

procedure TJvDockVIDTree.GetCaptionRect(var Rect: TRect);
begin
  Inc(Rect.Left, PPIScale(2) + CaptionLeftOffset);
  Inc(Rect.Top, PPIScale(1));
  Dec(Rect.Right, ButtonWidth + CaptionRightOffset - PPIScale(1));
  Dec(Rect.Bottom, PPIScale(2));
end;

function TJvDockVIDTree.GetCaptionRightOffset: Integer;
begin
  Result := PPIScale(FCaptionRightOffset);
end;

{ Adjust docking area rectangle to compensante for Grabber control }
procedure TJvDockVIDTree.AdjustDockRect(Control: TControl; var ARect: TRect);
begin
  if AlwaysShowGrabber or (DockSite.Align <> alClient) or (TopZone.VisibleChildTotal > 1) then
    inherited AdjustDockRect(Control, ARect);
end;

procedure TJvDockVIDTree.IgnoreZoneInfor(Stream: TMemoryStream);
var
  CompName: string;
begin
  Stream.Position := Stream.Position + 6;
  ReadControlName(Stream, CompName);
end;

procedure TJvDockVIDTree.SyncWithStyle;
begin
  inherited SyncWithStyle;

  if DockStyle is TJvDockVIDStyle then
    AlwaysShowGrabber := TJvDockVIDStyle(DockStyle).AlwaysShowGrabber;

  if DockStyle.TabServerOption is TJvDockVIDTabServerOption then
  begin
    ShowCloseButtonOnGrabber := TJvDockVIDTabServerOption(DockStyle.TabServerOption).ShowCloseButtonOnGrabber;
  end;

  { Not all properties are copied (See TJvDockVIDTree.DrawDockGrabber) so we
    must invalidate the DockSite so it gets repainted. }
  DockSite.Invalidate;
end;

procedure TJvDockVIDTree.SetShowCloseButtonOnGrabber(const Value: Boolean);
begin
  if Value <> FShowCloseButtonOnGrabber then
  begin
    FShowCloseButtonOnGrabber := Value;
    UpdateAll;
    DockSite.Invalidate;
  end;
end;

procedure TJvDockVIDTree.SetAlwaysShowGrabber(const Value: Boolean);
begin
  if Value <> FAlwaysShowGrabber then
  begin
    FAlwaysShowGrabber := Value;
    UpdateAll;
    DockSite.Invalidate;
  end;
end;

//=== { TJvDockVIDConjoinPanel } =============================================

procedure TJvDockVIDConjoinPanel.CustomDockDrop(Source: TJvDockDragDockObject;
  X, Y: Integer);
begin
  if not ((Source.Control.HostDockSite <> nil) and
    (Source.DropOnControl = Source.Control.HostDockSite.Parent) and
    (Source.DropAlign = alClient)) then
  begin
    inherited CustomDockDrop(Source, X, Y);
    ParentForm.Caption := '';
    {$IFNDEF COMPILER9_UP}
    InvalidateDockHostSiteOfControl(Source.Control, False);
    {$ENDIF !COMPILER9_UP}
    if (Source.Control is TWinControl) and Source.Control.Visible and
      TWinControl(Source.Control).CanFocus then
      TWinControl(Source.Control).SetFocus;
  end;
end;

procedure TJvDockVIDConjoinPanel.CustomDockOver(Source: TJvDockDragDockObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  DropAlign: TAlign;
begin
  inherited CustomDockOver(Source, X, Y, State, Accept);
  if Accept and (Source is TJvDockVIDDragDockObject) then
    if State = dsDragMove then
    begin
      DropAlign := Source.DropAlign;
      JvDockManager.GetDockEdge(Source.EraseDockRect, Source.DragPos, DropAlign, Source.Control);
    end;
end;

procedure TJvDockVIDConjoinPanel.CustomGetDockEdge(Source: TJvDockDragDockObject;
  MousePos: TPoint; var DropAlign: TAlign);
begin
end;

procedure TJvDockVIDConjoinPanel.CustomGetSiteInfo(Source: TJvDockDragDockObject; Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  JvDockManager.GetSiteInfo(Client, InfluenceRect, MousePos, CanDock);
  CanDock := IsDockable(Self, Client, Source.DropOnControl, Source.DropAlign);
end;

function TJvDockVIDConjoinPanel.CustomUnDock(Source: TJvDockDragDockObject; NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  Result := inherited CustomUnDock(Source, NewTarget, Client);
end;

procedure TJvDockVIDConjoinPanel.DockDrop(Source: TDragDockObject;
  X, Y: Integer);
begin
  inherited DockDrop(Source, X, Y);
end;

procedure TJvDockVIDConjoinPanel.UpdateCaption(Exclude: TControl);
begin
  if VisibleDockClientCount > 1 then
    ParentForm.Caption := ''
  else
    inherited UpdateCaption(Exclude);
  Invalidate;
end;

// TJvDockVIDTabPageControl ==================================================
function TJvDockVIDTabPageControl.DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean;
begin
  Result := inherited DoUnDock(NewTarget, Client);
  if Assigned(ParentForm) then
    ParentForm.Caption := ActivePage.Caption;
end;

constructor TJvDockVIDTabPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPanel := nil;
  TabWidth := 1;
  MultiLine := True;
  TabSheetClass := TJvDockVIDTabSheet;
  TabPanelClass := TJvDockTabPanel;
  FTempSheet := nil;
  TabPosition := tpTop; // Warren changed! was tpBottom;
  FTabImageList := nil;
  Images := nil;

  if AOwner is TJvDockTabHostForm then
  begin
    FTabImageList := TImageList.CreateSize(PPIScale(16), PPIScale(16));
    {$IFDEF RTL200_UP}
    FTabImageList.ColorDepth := cd32Bit;
    {$ENDIF RTL200_UP}
    Images := FTabImageList;
  end;
end;

destructor TJvDockVIDTabPageControl.Destroy;
begin
  if FTabImageList <> nil then
  begin
    FTabImageList.Free;
    FTabImageList := nil;
  end;
  if FPanel <> nil then
  begin
    FPanel.Free;
    FPanel := nil;
  end;
  inherited Destroy;
end;

procedure TJvDockVIDTabPageControl.AfterConstruction;
begin
  // we must create the panel before the inherited call, because
  // TJvDockTabPageControl.AfterConstruction calls SyncWithStyle which needs
  // a panel.
  CreatePanel;
  inherited AfterConstruction;
end;

procedure TJvDockVIDTabPageControl.Loaded;
begin
  inherited Loaded;
  CreatePanel;
end;

procedure TJvDockVIDTabPageControl.CreatePanel;
begin
  if FPanel = nil then
  begin
    FPanel := TabPanelClass.Create(Self);
    FPanel.Page := Self;
    FPanel.Parent := Self;
    FPanel.TabLeftOffset := 5;
    FPanel.TabRightOffset := 5;
    FPanel.TabTopOffset := 3;
    FPanel.TabBottomOffset := 3;
    ActiveSheetColor := clBtnFace;
    InactiveSheetColor := clBtnShadow;
  end;
  Resize;
end;

procedure TJvDockVIDTabPageControl.CreateWnd;
begin
  inherited CreateWnd;
end;

procedure TJvDockVIDTabPageControl.CMDockNotification(
  var Msg: TCMDockNotification);
begin
  if Msg.Msg = CM_DOCKNOTIFICATION then
  begin
    with TCMDockNotification(Msg) do
    begin
      if NotifyRec.ClientMsg = CM_INVALIDATEDOCKHOST then
        {$IFDEF COMPILER9_UP}
        Self.InvalidateDockHostSite(Boolean(NotifyRec.MsgLParam))
        {$ELSE}
        InvalidateDockHostSiteOfControl(Self, Boolean(NotifyRec.MsgLParam))
        {$ENDIF COMPILER9_UP}
      else
        inherited;
    end;
  end
  else
    inherited;
end;

procedure TJvDockVIDTabPageControl.CustomDockDrop(Source: TJvDockDragDockObject;
  X, Y: Integer);
var
  ARect: TRect;
  I: Integer;
  VIDSource: TJvDockVIDDragDockObject;
  DockClient: TJvDockClient;
  Host: TJvDockConjoinHostForm;
  Index: Integer;
begin
  if Source.DropAlign in [alClient, alNone] then
  begin
    if Source is TJvDockVIDDragDockObject then
    begin
      BeginDockLoading;
      try
        DoFloatForm(Source.Control);
        FreeAllDockableForm;
        VIDSource := TJvDockVIDDragDockObject(Source);

        for I := 0 to VIDSource.SourceDockClientCount - 1 do
        begin
          Source.Control := VIDSource.SourceDockClients[I];
          inherited CustomDockDrop(Source, X, Y);
          if Source.Control is TCustomForm then
            if FTabImageList <> nil then
            begin
              Index := FTabImageList.AddIcon(TForm(Source.Control).Icon);
              if Index <> -1 then
                ActivePage.ImageIndex := Index;
            end;
        end;
      finally
        EndDockLoading;
        JvGlobalDockManager.DragObject.Control := nil;
      end;
    end;
  end
  else
  begin
    DockClient := FindDockClient(ParentForm);
    if DockClient <> nil then
    begin
      ARect := ParentForm.BoundsRect;
      Host := DockClient.CreateConjoinHostAndDockControl(ParentForm, Source.Control, Source.DropAlign);
      Host.BoundsRect := ARect;
      SetDockSite(ParentForm, False);
      SetDockSite(TWinControl(Source.Control), False);
      Host.Visible := True;
    end;
  end;
  FPanel.SelectSheet := nil;
  with ActivePage do
    if not JvGlobalDockIsLoading and (ControlCount > 0) and Assigned(Controls[0]) then
    begin
      if Visible and (Controls[0] <> nil) and (Controls[0] as TWinControl).CanFocus then
          (Controls[0] as TWinControl).SetFocus;
    end;
  ParentForm.Caption := ActivePage.Caption;
end;

procedure TJvDockVIDTabPageControl.CustomDockOver(Source: TJvDockDragDockObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  ARect: TRect;
begin
  Accept := IsDockable(Self, Source.Control, Source.DropOnControl, Source.DropAlign);
  if Accept then
  begin
    if ParentForm.HostDockSite = nil then
    begin
      Source.DropAlign := ComputeVIDDockingRect(Self, Source.Control, ARect, Point(X, Y));
      if Source.DropAlign = alClient then
        ARect.Top := ARect.Top + JvDockGetSysCaptionHeight;

      if Accept and (Source.DropAlign <> alNone) then
      begin
        Source.DockRect := ARect;
        gi_DockRect := ARect;
      end;
    end
    else
    begin
      if ParentForm.HostDockSite is TJvDockCustomPanel then
      begin
        ARect := Source.DockRect;
        TJvDockCustomPanel(ParentForm.HostDockSite).JvDockManager.PositionDockRect(Source.Control, Source.DropOnControl,
          Source.DropAlign, ARect);
        Source.DockRect := ARect;
      end;
    end;
  end;
end;

procedure TJvDockVIDTabPageControl.CustomGetSiteInfo(Source: TJvDockDragDockObject; Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
const
  DefExpandoRect = 20;
var
  CH_BW: Integer;
  ARect: TRect;
begin
  CanDock := IsDockable(Self, Client, Source.DropOnControl, Source.DropAlign);
  if ParentForm.HostDockSite <> nil then
    CanDock := False;
  if CanDock then
  begin
    GetWindowRect(Parent.Handle, InfluenceRect);
    if PtInRect(InfluenceRect, MousePos) then
    begin
      ARect := InfluenceRect;
      InflateRect(ARect, -DefExpandoRect, -DefExpandoRect);

      CH_BW := JvDockGetSysCaptionHeightAndBorderWidth;
      Inc(ARect.Top, CH_BW + 1);
      Dec(ARect.Bottom, TabHeight);
      if PtInRect(ARect, MousePos) then
        InfluenceRect := Rect(0, 0, 0, 0);
    end;
  end;
end;

procedure TJvDockVIDTabPageControl.DoRemoveDockClient(Client: TControl);
begin
  inherited DoRemoveDockClient(Client);
  if Assigned(ParentForm) then
    ParentForm.Caption := ActivePage.Caption; {bugfix FEB 14, 2005 - WPostma.}
end;

procedure TJvDockVIDTabPageControl.Change;
begin
  Assert(Assigned(ParentForm));
  inherited Change;

  { During closing/undocking of a form,
    ActivePage is actually going to be wrong.
    See above in DoRemoveDockClient for where we fix
    this problem. }
  ParentForm.Caption := ActivePage.Caption;

  if ParentForm.HostDockSite is TJvDockCustomPanel then
  begin
    //    if ParentForm.Visible and ParentForm.CanFocus then
    //      ParentForm.SetFocus;
    ParentForm.HostDockSite.Invalidate;
  end;
  //  if (ActivePage <> nil) and (ActivePage.Visible) and (ActivePage.CanFocus) then
  //    if ParentForm.Visible and ParentForm.CanFocus then
  //      ActivePage.SetFocus;
end;

procedure TJvDockVIDTabPageControl.ChangeScale(M, D: Integer;
  isDpiChange: Boolean);
var
  Sheet: TJvDockVIDTabSheet;
begin
  inherited;
  if M = D then
    Exit;
  // Scale image list
  if Assigned(FTabImageList) then
    JvScaleImageList(FTabImageList, M, D);

  // Recalc tab width
  for var I := 0 to Count - 1 do
  begin
    Sheet := Pages[I] as TJvDockVIDTabSheet;
    Sheet.SetSheetSort(Sheet.Caption);
  end;
end;

procedure TJvDockVIDTabPageControl.AdjustClientRect(var Rect: TRect);
begin
  Rect := ClientRect;
  if (Parent is TJvDockTabHostForm) and (VisibleDockClientCount = 1) then
    Exit;
  case TabPosition of
    tpTop:
      Inc(Rect.Top, Panel.TabHeight - PPIScale(1));
    tpBottom:
      Dec(Rect.Bottom, Panel.TabHeight - PPIScale(1));
    tpLeft:
      Inc(Rect.Left, Panel.TabHeight - PPIScale(1));
    tpRight:
      Dec(Rect.Right, Panel.TabHeight - PPIScale(1));
  end;
end;

procedure TJvDockVIDTabPageControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

procedure TJvDockVIDTabPageControl.DrawTab(TabIndex: Integer;
  const Rect: TRect; Active: Boolean);
begin
  inherited DrawTab(TabIndex, Rect, Active);
end;

function TJvDockVIDTabPageControl.GetActiveFont: TFont;
begin
  Result := FPanel.FActiveFont;
end;

function TJvDockVIDTabPageControl.GetActiveSheetColor: TColor;
begin
  Result := FPanel.FActiveSheetColor;
end;

function TJvDockVIDTabPageControl.GetInactiveFont: TFont;
begin
  Result := FPanel.FInactiveFont;
end;

function TJvDockVIDTabPageControl.GetInactiveSheetColor: TColor;
begin
  Result := FPanel.Color;
end;

function TJvDockVIDTabPageControl.GetTabBottomOffset: Integer;
begin
  Result := FPanel.TabBottomOffset;
end;

function TJvDockVIDTabPageControl.GetTabLeftOffset: Integer;
begin
  Result := FPanel.TabLeftOffset;
end;

function TJvDockVIDTabPageControl.GetTabRightOffset: Integer;
begin
  Result := FPanel.TabRightOffset;
end;

function TJvDockVIDTabPageControl.GetTabTopOffset: Integer;
begin
  Result := FPanel.TabTopOffset;
end;

procedure TJvDockVIDTabPageControl.Paint;
begin
  inherited Paint;
end;

procedure TJvDockVIDTabPageControl.Resize;
begin
  inherited Resize;
  if FPanel = nil then
    Exit;
  case TabPosition of
    tpLeft: FPanel.SetBounds(0, 0, Panel.TabHeight, Height);
    tpRight: FPanel.SetBounds(Width - Panel.TabHeight, 0, Panel.TabHeight, Height);
    tpTop: FPanel.SetBounds(0, 0, Width, Panel.TabHeight);
    tpBottom: FPanel.SetBounds(0, Height - Panel.TabHeight, Width, Panel.TabHeight);
  end;
end;

procedure TJvDockVIDTabPageControl.SetActiveFont(Value: TFont);
begin
  FPanel.FActiveFont.Assign(Value);
  if ActivePage <> nil then
    TJvDockVIDTabSheet(ActivePage).SetSheetSort(ActivePage.Caption);
  FPanel.Invalidate;
end;

procedure TJvDockVIDTabPageControl.SetActiveSheetColor(const Value: TColor);
begin
  FPanel.FActiveSheetColor := Value;
  FPanel.Invalidate;
end;

procedure TJvDockVIDTabPageControl.SetInactiveFont(Value: TFont);
var
  I: Integer;
begin
  FPanel.FInactiveFont.Assign(Value);
  for I := 0 to Count - 1 do
    if Pages[I] <> ActivePage then
      TJvDockVIDTabSheet(Pages[I]).SetSheetSort(Pages[I].Caption);
  FPanel.Invalidate;
end;

procedure TJvDockVIDTabPageControl.SetInactiveSheetColor(const Value: TColor);
begin
  if FPanel.Color <> Value then
  begin
    FPanel.Color := Value;
    FPanel.Invalidate;
  end;
end;

procedure TJvDockVIDTabPageControl.SetTabBottomOffset(const Value: Integer);
begin
  if FPanel.TabBottomOffset <> Value then
  begin
    FPanel.TabBottomOffset := Value;
    FPanel.Invalidate;
  end;
end;

procedure TJvDockVIDTabPageControl.SetTabHeight(Value: Smallint);
begin
  inherited SetTabHeight(Value);
  if Panel.FTabHeight <> Value then
  begin
    Panel.FTabHeight := Value;
    FPanel.Invalidate;
  end;
end;

procedure TJvDockVIDTabPageControl.SetTabLeftOffset(const Value: Integer);
begin
  if FPanel.TabLeftOffset <> Value then
  begin
    FPanel.TabLeftOffset := Value;
    FPanel.Invalidate;
  end;
end;

procedure TJvDockVIDTabPageControl.SetTabPosition(Value: TTabPosition);
begin
  Assert(Value in [tpTop, tpBottom], RsEDockCannotSetTabPosition);
  inherited SetTabPosition(Value);
  Resize;
end;

procedure TJvDockVIDTabPageControl.SetTabRightOffset(const Value: Integer);
begin
  if FPanel.TabRightOffset <> Value then
  begin
    FPanel.TabRightOffset := Value;
    FPanel.Invalidate;
  end;
end;

procedure TJvDockVIDTabPageControl.SetTabTopOffset(const Value: Integer);
begin
  if FPanel.TabTopOffset <> Value then
  begin
    FPanel.TabTopOffset := Value;
    FPanel.Invalidate;
  end;
end;

procedure TJvDockVIDTabPageControl.SetActivePage(Page: TJvDockTabSheet);
begin
  inherited SetActivePage(Page);
  FPanel.Invalidate;
end;

procedure TJvDockVIDTabPageControl.DockDrop(Source: TDragDockObject;
  X, Y: Integer);
var
  Index: Integer;
  NewPage: TJvDockTabSheet;
begin
  inherited DockDrop(Source, X, Y);
  FPanel.SelectSheet := nil;
  if ActivePage <> nil then
    ParentForm.Caption := ActivePage.Caption;
  if Source.Control is TCustomForm then
  begin
    if Source.Control.Parent is TJvDockTabSheet then
      NewPage := TJvDockTabSheet(Source.Control.Parent)
    else
      NewPage := nil;
    if Source.Control.Visible and Assigned(NewPage) then
      ActivePage := NewPage;
    if FTabImageList <> nil then
    begin
      Index := FTabImageList.AddIcon(TForm(Source.Control).Icon);
      if (Index <> -1) and Assigned(NewPage) then
        NewPage.ImageIndex := Index;
    end;
  end;
end;

function TJvDockVIDTabPageControl.GetDockClientFromMousePos(MousePos: TPoint): TControl;
var
  PageIndex: Integer;
begin
  Result := nil;
  case TabPosition of
    tpTop:
      PageIndex := Panel.FindSheetWithPos(MousePos.X, MousePos.Y, 0, Panel.Height - TabBottomOffset);
    tpBottom:
      PageIndex := Panel.FindSheetWithPos(MousePos.X, MousePos.Y, TabBottomOffset, Panel.Height);
    tpLeft:
      PageIndex := Panel.FindSheetWithPos(MousePos.Y, MousePos.X, 0, Panel.Height - TabBottomOffset);
    tpRight:
      PageIndex := Panel.FindSheetWithPos(MousePos.Y, MousePos.X, TabBottomOffset, Panel.Height);
  else
    PageIndex := -1;
  end;
  if PageIndex >= 0 then
  begin
    Result := Pages[PageIndex].Controls[0];
    if Result.HostDockSite <> Self then
      Result := nil;
  end;
end;

procedure TJvDockVIDTabPageControl.CustomGetDockEdge(Source: TJvDockDragDockObject;
  MousePos: TPoint; var DropAlign: TAlign);
var
  ARect: TRect;
begin
  DropAlign := ComputeVIDDockingRect(Self, Source.Control, ARect, MousePos);
end;

function TJvDockVIDTabPageControl.GetVisibleSheetCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if Pages[I].TabVisible then
      Inc(Result);
end;

procedure TJvDockVIDTabPageControl.UpdateCaption(Exclude: TControl);
begin
  ParentForm.Caption := ActivePage.Caption;
  if Parent <> nil then
  begin
    Parent.Invalidate;
    if Parent.HostDockSite <> nil then
      Parent.HostDockSite.Invalidate;
  end;
end;

procedure TJvDockVIDTabPageControl.SetHotTrack(Value: Boolean);
begin
  inherited SetHotTrack(Value);
end;

procedure TJvDockVIDTabPageControl.SetImages(Value: TCustomImageList);
begin
  inherited SetImages(Value);
  if Panel <> nil then
  begin
    Panel.ShowTabImages := Value <> nil;
    Panel.Invalidate;
  end;
end;

function TJvDockVIDTabPageControl.GetHotTrackColor: TColor;
begin
  Result := Panel.FHotTrackColor;
end;

procedure TJvDockVIDTabPageControl.SetHotTrackColor(const Value: TColor);
begin
  if Panel.FHotTrackColor <> Value then
  begin
    Panel.FHotTrackColor := Value;
    Panel.Invalidate;
  end;
end;

function TJvDockVIDTabPageControl.GetShowTabImages: Boolean;
begin
  Result := FPanel.FShowTabImages;
end;

procedure TJvDockVIDTabPageControl.SetShowTabImages(const Value: Boolean);
begin
  FPanel.ShowTabImages := Value;
end;

function TJvDockVIDTabPageControl.GetShowTabHints: Boolean;
begin
  Result := FPanel.FShowTabHints;
end;

procedure TJvDockVIDTabPageControl.SetShowTabHints(const Value: Boolean);
begin
  FPanel.ShowTabHints := Value;
end;

function TJvDockVIDTabPageControl.CustomUnDock(Source: TJvDockDragDockObject;
  NewTarget: TWinControl; Client: TControl): Boolean;
var
  CurrPage: TJvDockTabSheet;
  I: Integer;
begin
  if not ((Source.Control.HostDockSite <> nil) and
    (Source.DropOnControl = Source.Control.HostDockSite.Parent) and
    (Source.DropAlign = alClient)) then
  begin
    CurrPage := GetPageFromDockClient(Client);
    if CurrPage <> nil then
    begin
      //if (FTabImageList <> nil) and ShowTabImages and
      //  (FTabImageList.Count > CurrPage.ImageIndex) then
      //prevent AV
      if Assigned(FTabImageList) then
        if ShowTabImages and
           (FTabImageList.Count > CurrPage.ImageIndex) and
           (CurrPage.ImageIndex >= 0) then
      begin
        FTabImageList.Delete(CurrPage.ImageIndex);
        for I := 0 to Count - 1 do
          if Pages[I].ImageIndex > CurrPage.ImageIndex then
            Pages[I].ImageIndex := Pages[I].ImageIndex - 1;
      end;
    end;
    Result := inherited CustomUnDock(Source, NewTarget, Client);
  end
  else
    Result := True;
end;

function TJvDockVIDTabPageControl.GetPage(Index: Integer): TJvDockVIDTabSheet;
begin
  Result := TJvDockVIDTabSheet(inherited Pages[Index]);
end;

function TJvDockVIDTabPageControl.GetActiveVIDPage: TJvDockVIDTabSheet;
begin
  Result := TJvDockVIDTabSheet(inherited ActivePage);
end;

procedure TJvDockVIDTabPageControl.SetActiveVIDPage(const Value: TJvDockVIDTabSheet);
begin
  ActivePage := Value;
end;

procedure TJvDockVIDTabPageControl.SyncWithStyle;
var
  VIDTabServerOption: TJvDockVIDTabServerOption;
begin
  inherited SyncWithStyle;
  // panel must be created
  if FPanel = nil then
    Exit;
  if DockStyle.TabServerOption is TJvDockVIDTabServerOption then
  begin
    VIDTabServerOption := TJvDockVIDTabServerOption(DockStyle.TabServerOption);

    ActiveFont := VIDTabServerOption.ActiveFont;
    ActiveSheetColor := VIDTabServerOption.ActiveSheetColor;
    HotTrackColor := VIDTabServerOption.HotTrackColor;
    InactiveFont := VIDTabServerOption.InactiveFont;
    InactiveSheetColor := VIDTabServerOption.InactiveSheetColor;
    ShowTabImages := VIDTabServerOption.ShowTabImages;
    ShowTabHints := VIDTabServerOption.ShowTabHints;
    TabPosition := VIDTabServerOption.TabPosition;
  end;
end;

//=== { TJvDockTabPanel } ====================================================

constructor TJvDockTabPanel.Create(AOwner: TComponent);
begin
  {$IFDEF JVDOCK_DEBUG}
  OutputDebugString('JvDockVIDStyle.pas: TJvDockTabPanel.Create');
  {$ENDIF JVDOCK_DEBUG}
  inherited Create(AOwner);
  Page := nil;
  FCaptionTopOffset := 0;
  FCaptionLeftOffset := 5;
  FCaptionRightOffset := 5;
  FTabBottomOffset := 3;
  FTabSplitterWidth := 3;
  FTabHeight := 22;
  FSortList := TList.Create;
  FActiveFont := TFont.Create;
  FActiveFont.Color := clBlack;
  FInactiveFont := TFont.Create;
  FInactiveFont.Color := clWhite;
  FHotTrackColor := clBlue;
  FTempPages := TList.Create;
  FSelectHotIndex := -1;
  FShowTabImages := False;
  FSelectSheet := nil;
end;

destructor TJvDockTabPanel.Destroy;
begin
  FActiveFont.Free;
  FInactiveFont.Free;
  FSortList.Free;
  FTempPages.Free;
  inherited Destroy;
end;

procedure TJvDockTabPanel.DeleteSorts(Sheet: TJvDockVIDTabSheet);
var
  SheetIndex: Integer;
begin
  SheetIndex := FSortList.IndexOf(Sheet);
  if SheetIndex >= 0 then
    FSortList.Delete(SheetIndex);
  if Sheet <> nil then
    Sheet.TabVisible := False;
  SetShowTabWidth;
  Page.Invalidate;
end;

function TJvDockTabPanel.FindSheetWithPos(cX, cY, cTopOffset, cBottomOffset: Integer): Integer;
var
  I: Integer;
  CompleteWidth, CurrTabWidth: Integer;
  Pages: TList;
begin
  Result := -1;
  if (cY > cBottomOffset) or (cY < cTopOffset) then
    Exit;
  CompleteWidth := 0;
  if FSelectSheet = nil then
    Pages := Page.PageSheets
  else
    Pages := FTempPages;
  for I := 0 to Pages.Count - 1 do
  begin
    if not TJvDockVIDTabSheet(Pages[I]).TabVisible then
      Continue;
    CurrTabWidth := TJvDockVIDTabSheet(Pages[I]).ShowTabWidth;
    if (cX >= FTabLeftOffset + CompleteWidth) and (cX <= FTabLeftOffset + CurrTabWidth + CompleteWidth +
      FTabSplitterWidth) then
    begin
      Result := I;
      Exit;
    end;
    Inc(CompleteWidth, CurrTabWidth + FTabSplitterWidth);
  end;
end;

function TJvDockTabPanel.GetPageIndexFromMousePos(X, Y: Integer): Integer;
begin
  Result := -1;
  case Page.TabPosition of
    tpTop:
      Result := FindSheetWithPos(X, Y, 0, Height - TabBottomOffset);
    tpBottom:
      Result := FindSheetWithPos(X, Y, TabBottomOffset, Height);
    tpLeft:
      Result := FindSheetWithPos(Y, X, 0, Height - TabBottomOffset);
    tpRight:
      Result := FindSheetWithPos(Y, X, TabBottomOffset, Height);
  end;
end;

function TJvDockTabPanel.GetMaxTabWidth: TJvDockTabSheet;
var
  I: Integer;
  MaxWidth, CurrWidth: Integer;
begin
  Result := nil;
  MaxWidth := 0;
  if Page = nil then
    Exit;
  for I := 0 to Page.Count - 1 do
  begin
    CurrWidth := Canvas.TextWidth(Page.Tabs[I]);
    if MaxWidth < CurrWidth then
    begin
      Result := Page.Pages[I];
      MaxWidth := CurrWidth;
    end;
  end;
end;

function TJvDockTabPanel.GetMinTabWidth: TJvDockTabSheet;
var
  I: Integer;
  MinWidth, CurrWidth: Integer;
begin
  Result := nil;
  MinWidth := 0;
  for I := 0 to Page.Count - 1 do
  begin
    CurrWidth := Canvas.TextWidth(Page.Tabs[I]);
    if MinWidth > CurrWidth then
    begin
      Result := Page.Pages[I];
      MinWidth := CurrWidth;
    end;
  end;
end;

function TJvDockTabPanel.GetPanelHeight: Integer;
begin
  case Page.TabPosition of
    tpLeft, tpRight:
      Result := Width;
    tpTop, tpBottom:
      Result := Height;
  else
    Result := 0;
  end;
end;

function TJvDockTabPanel.GetPanelWidth: Integer;
begin
  case Page.TabPosition of
    tpLeft, tpRight:
      Result := Height;
    tpTop, tpBottom:
      Result := Width;
  else
    Result := 0;
  end;
end;

function TJvDockTabPanel.GetSorts(Index: Integer): TJvDockVIDTabSheet;
begin
  Result := FSortList[Index];
end;

function TJvDockTabPanel.GetTabBottomOffset: Integer;
begin
  Result := FPage.PPIScale(FTabBottomOffset);
end;

function TJvDockTabPanel.GetTabHeight: Integer;
begin
  Result := FPage.PPIScale(FTabHeight);
end;

function TJvDockTabPanel.GetTabLeftOffset: Integer;
begin
  Result := FPage.PPIScale(FTabLeftOffset);
end;

function TJvDockTabPanel.GetTabRightOffset: Integer;
begin
  Result := FPage.PPIScale(FTabRightOffset);
end;

function TJvDockTabPanel.GetTabSplitterWidth: Integer;
begin
  Result := FPage.PPIScale(FTabSplitterWidth);
end;

function TJvDockTabPanel.GetTabTopOffset: Integer;
begin
  Result := FPage.PPIScale(FTabTopOffset);
end;

function TJvDockTabPanel.GetTotalTabWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  if FSortList = nil then
    Exit;
  for I := 0 to FSortList.Count - 1 do
    Inc(Result, Sorts[I].TabWidth + Integer(I <> FSortList.Count - 1) * TabSplitterWidth);
end;

procedure TJvDockTabPanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Ctrl: TControl;
  Index: Integer;
  Msg: TWMMouse;
  Sheet: TJvDockVIDTabSheet;
  AParentForm: TCustomForm;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Page = nil then
    Exit;

  Index := GetPageIndexFromMousePos(X, Y);
  if Index >= 0 then
  begin
    if Index <> Page.ActivePageIndex then
    begin
      Sheet := Page.ActiveVIDPage;
      Page.ActivePageIndex := Index;
      Sheet.SetSheetSort(Sheet.Caption);
      Page.ActiveVIDPage.SetSheetSort(Page.ActiveVIDPage.Caption);
      Page.Change;
      Invalidate;
    end;
    if Assigned(Page.ActivePage) and Page.ActivePage.CanFocus then
    begin
      AParentForm := GetParentForm(Page);
      if Assigned(AParentForm) then
      begin
        Page.SelectFirst;
        AParentForm.SetFocus;
      end;
    end;

    if Button = mbLeft then
    begin
      FSelectSheet := TJvDockVIDTabSheet(Page.ActivePage);
      FTempPages.Assign(Page.PageSheets);
    end;

    Ctrl := GetDockClientFromPageIndex(Index);
    if Ctrl <> nil then
    begin
      JvGlobalDockClient := FindDockClient(Ctrl);
      if JvGlobalDockClient <> nil then
      begin
        Msg.Msg := WM_NCLBUTTONDOWN + Integer(Button) * 3 + Integer(ssDouble in Shift) * 2;
        Msg.Pos.x := X;
        Msg.Pos.y := Y;
        if not (ssDouble in Shift) then
          JvGlobalDockClient.DoNCButtonDown(Page.DoMouseEvent(Msg, Page), Button, msTabPage)
        else
        begin
          JvGlobalDockClient.DoNCButtonDblClk(Page.DoMouseEvent(Msg, Page), Button, msTabPage);
          if (Button = mbLeft) and JvGlobalDockClient.CanFloat then
            JvGlobalDockClient.RestoreChild;
            //Ctrl.ManualDock(nil, nil, alNone);
        end;
      end;
    end;
  end;
end;

procedure TJvDockTabPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
  Ctrl: TControl;
  ARect: TRect;
  HintText: string;
begin
  inherited MouseMove(Shift, X, Y);
  Index := GetPageIndexFromMousePos(X, Y);

  if ShowTabHints and (Index > -1) then
  begin
    HintText := StringReplace(Page.Pages[Index].Caption, '&', '', [rfReplaceAll]);
    if HintText <> Hint then
    begin
      Hint := HintText;
      Application.ActivateHint(ClientToScreen(Point(X, Y)));
    end;
  end;

  if Page.HotTrack and (Index <> FSelectHotIndex) then
  begin
    FSelectHotIndex := Index;
    Invalidate;
  end;

  if Assigned(FSelectSheet) then
  begin
    Index := GetPageIndexFromMousePos(X, Y);
    if Index >= 0 then
    begin
      if (Index <> Page.ActivePageIndex) and (Page.Count > Index) then
      begin
        FSelectSheet.PageIndex := Index;
        Invalidate;
      end;
    end
    else
    begin
      case Page.TabPosition of
        tpTop:
          ARect := Rect(0, 0, Width, Height - TabBottomOffset);
        tpBottom:
          ARect := Rect(0, TabBottomOffset, Width, Height);
        tpLeft:
          ARect := Rect(0, 0, Width - TabBottomOffset, Height);
        tpRight:
          ARect := Rect(TabBottomOffset, 0, Width, Height);
      else
        ARect := Rect(0, 0, 0, 0);
      end;
      if PtInRect(ARect, Point(X, Y)) then
        Exit;
      if Page.FTempSheet = nil then
      begin
        Ctrl := GetDockClientFromPageIndex(FSelectSheet.PageIndex);
        if Ctrl <> nil then
          JvGlobalDockManager.BeginDrag(Ctrl, False, 1);
      end;
    end;
  end;
end;

procedure TJvDockTabPanel.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Ctrl: TControl;
  Index: Integer;
  Msg: TWMMouse;
begin
  {$IFDEF JVDOCK_DEBUG}
  OutputDebugString('JvDockVIDStyle.pas: TJvDockTabPanel.MouseUp');
  {$ENDIF JVDOCK_DEBUG}

  inherited MouseUp(Button, Shift, X, Y);
  FSelectSheet := nil;
  if Page = nil then
    Exit;

  Index := GetPageIndexFromMousePos(X, Y);
  Ctrl := GetDockClientFromPageIndex(Index);
  if Ctrl <> nil then
  begin
    JvGlobalDockClient := FindDockClient(Ctrl);
    if JvGlobalDockClient <> nil then
    begin
      Msg.Msg := WM_NCLBUTTONUP + Integer(Button) * 3 + Integer(ssDouble in Shift) * 2;
      Msg.Pos := PointToSmallPoint(Page.ScreenToClient(ClientToScreen(Point(X, Y))));
      if not (ssDouble in Shift) then
        JvGlobalDockClient.DoNCButtonUp(Page.DoMouseEvent(Msg, Page), Button, msTabPage);
    end;
  end;
end;

{ TJvDockTabPanel.Paint,etc.
  TODO-LIST-ITEM:
  ---------------
  VID style has a bit of a problem with what to do in
  case of a lot of tabs. It keeps making the text shorter via
  text drawn with ellipsis but doesn't EVER display the left/right
  buttons that allow you to scroll through a long list of tabs.
  To fix this is non-trivial. -WPostma.
  }
procedure TJvDockTabPanel.Paint;
var
  ARect: TRect;
  CurrTabWidth: Integer;
  I, CompleteWidth: Integer;
  ImageWidth: Integer;
  CaptionString: string;
begin
  inherited Paint;
  if Page = nil then
    Exit;

  if (Page.Images <> nil) and (Page.ShowTabImages) then
    ImageWidth := Page.Images.Width
  else
    ImageWidth := 0;

  Canvas.Brush.Color := Page.ActiveSheetColor;
  case Page.TabPosition of
    tpLeft:
      Canvas.FillRect(Rect(PanelHeight - TabBottomOffset, 0, PanelHeight, PanelWidth));
    tpRight:
      Canvas.FillRect(Rect(0, 0, TabBottomOffset, PanelWidth));
    tpTop:
      Canvas.FillRect(Rect(0, PanelHeight - TabBottomOffset, PanelWidth, PanelHeight));
    tpBottom:
      Canvas.FillRect(Rect(0, 0, PanelWidth, TabBottomOffset));
  end;

  case Page.TabPosition of
    tpTop, tpLeft:
      Canvas.Pen.Color := clWhite;
    tpBottom, tpRight:
      Canvas.Pen.Color := clBlack;
  end;

  case Page.TabPosition of
    tpLeft:
      begin
        Canvas.MoveTo(PanelHeight - TabBottomOffset, 0);
        Canvas.LineTo(PanelHeight - TabBottomOffset, PanelWidth);
      end;
    tpRight:
      begin
        Canvas.MoveTo(TabBottomOffset, 0);
        Canvas.LineTo(TabBottomOffset, PanelWidth);
      end;
    tpTop:
      begin
        Canvas.MoveTo(0, PanelHeight - TabBottomOffset);
        Canvas.LineTo(PanelWidth, PanelHeight - TabBottomOffset);
      end;
    tpBottom:
      begin
        Canvas.MoveTo(0, TabBottomOffset);
        Canvas.LineTo(PanelWidth, TabBottomOffset);
      end;
  end;

  CompleteWidth := 0;

  Canvas.Brush.Style := bsClear;

  for I := 0 to Page.Count - 1 do
  begin
    if not Page.Pages[I].TabVisible then
      Continue;

    CurrTabWidth := TJvDockVIDTabSheet(Page.Pages[I]).ShowTabWidth;

    if Page.ActivePageIndex = I then
    begin
      Canvas.Brush.Color := Page.ActiveSheetColor;
      case Page.TabPosition of
        tpLeft:
          Canvas.FillRect(Rect(TabTopOffset, CompleteWidth + TabLeftOffset,
            PanelHeight, CompleteWidth + TabLeftOffset + CurrTabWidth));
        tpRight:
          Canvas.FillRect(Rect(TabBottomOffset, CompleteWidth + TabLeftOffset,
            PanelHeight - TabTopOffset, CompleteWidth + TabLeftOffset + CurrTabWidth));
        tpTop:
          Canvas.FillRect(Rect(CompleteWidth + TabLeftOffset, TabTopOffset,
            CompleteWidth + TabLeftOffset + CurrTabWidth, PanelHeight));
        tpBottom:
          Canvas.FillRect(Rect(CompleteWidth + TabLeftOffset, TabBottomOffset,
            CompleteWidth + TabLeftOffset + CurrTabWidth, PanelHeight - TabTopOffset));
      end;

      Canvas.Pen.Color := clWhite;
      case Page.TabPosition of
        tpLeft:
          begin
            Canvas.MoveTo(PanelHeight - TabBottomOffset, CompleteWidth + TabLeftOffset);
            Canvas.LineTo(TabTopOffset, CompleteWidth + TabLeftOffset);
            Canvas.LineTo(TabTopOffset, CompleteWidth + TabLeftOffset + CurrTabWidth);
            Canvas.Pen.Color := clBlack;
            Canvas.LineTo(PanelHeight - TabBottomOffset, CompleteWidth + TabLeftOffset + CurrTabWidth);
          end;
        tpRight:
          begin
            Canvas.MoveTo(TabTopOffset, CompleteWidth + TabLeftOffset);
            Canvas.LineTo(PanelHeight - TabBottomOffset, CompleteWidth + TabLeftOffset);
            Canvas.Pen.Color := clBlack;
            Canvas.LineTo(PanelHeight - TabBottomOffset, CompleteWidth + TabLeftOffset + CurrTabWidth);
            Canvas.LineTo(TabTopOffset, CompleteWidth + TabLeftOffset + CurrTabWidth);
          end;
        tpTop:
          begin
            Canvas.MoveTo(CompleteWidth + TabLeftOffset, PanelHeight - TabBottomOffset);
            Canvas.LineTo(CompleteWidth + TabLeftOffset, TabTopOffset);
            Canvas.LineTo(CompleteWidth + TabLeftOffset + CurrTabWidth, TabTopOffset);
            Canvas.Pen.Color := clBlack;
            Canvas.LineTo(CompleteWidth + TabLeftOffset + CurrTabWidth, PanelHeight - TabTopOffset);
          end;
        tpBottom:
          begin
            Canvas.MoveTo(CompleteWidth + TabLeftOffset, TabBottomOffset);
            Canvas.LineTo(CompleteWidth + TabLeftOffset, PanelHeight - TabTopOffset);
            Canvas.Pen.Color := clBlack;
            Canvas.LineTo(CompleteWidth + TabLeftOffset + CurrTabWidth, PanelHeight - TabTopOffset);
            Canvas.LineTo(CompleteWidth + TabLeftOffset + CurrTabWidth, TabBottomOffset);
          end;
      end;

      Canvas.Font.Assign(FActiveFont);
    end
    else
    begin
      if (I < Page.ActivePageIndex - 1) or (I > Page.ActivePageIndex) then
      begin
        Canvas.Pen.Color := Page.InactiveFont.Color;
        case Page.TabPosition of
          tpLeft, tpRight:
            begin
              Canvas.MoveTo(PanelHeight - TabBottomOffset - FPage.PPIScale(3), CompleteWidth + TabLeftOffset + CurrTabWidth);
              Canvas.LineTo(TabTopOffset + FPage.PPIScale(2), CompleteWidth + TabLeftOffset + CurrTabWidth);
            end;
          tpTop, tpBottom:
            begin
              Canvas.MoveTo(CompleteWidth + TabLeftOffset + CurrTabWidth, PanelHeight - TabBottomOffset - FPage.PPIScale(3));
              Canvas.LineTo(CompleteWidth + TabLeftOffset + CurrTabWidth, TabTopOffset + FPage.PPIScale(2));
            end;
        end;
      end;
      Canvas.Brush.Color := Page.InactiveSheetColor;
      Canvas.Font.Assign(FInactiveFont);
    end;
    // Scale Font
    Canvas.Font.Height := MulDiv(Canvas.Font.Height, FCurrentPPI, Canvas.Font.PixelsPerInch);

    if FSelectHotIndex = I then
      Canvas.Font.Color := FHotTrackColor;

    case Page.TabPosition of
      tpLeft:
        ARect := Rect(TabTopOffset + CaptionTopOffset + FPage.PPIScale(1),
          CompleteWidth + TabLeftOffset + CaptionLeftOffset,
          PanelHeight,
          CompleteWidth + TabLeftOffset + CurrTabWidth - CaptionRightOffset);

      tpRight:
        ARect := Rect(TabBottomOffset + CaptionTopOffset + FPage.PPIScale(1),
          CompleteWidth + TabLeftOffset + CaptionLeftOffset,
          PanelHeight,
          CompleteWidth + TabLeftOffset + CurrTabWidth - CaptionRightOffset);

      tpTop:
        ARect := Rect(CompleteWidth + TabLeftOffset + CaptionLeftOffset +
          Integer(FShowTabImages) * (ImageWidth + CaptionLeftOffset),
          TabTopOffset + CaptionTopOffset + FPage.PPIScale(1),
          CompleteWidth + TabLeftOffset + CurrTabWidth - CaptionRightOffset,
          PanelHeight);

      tpBottom:
        ARect := Rect(CompleteWidth + TabLeftOffset + CaptionLeftOffset +
          Integer(FShowTabImages) * (ImageWidth + CaptionLeftOffset),
          TabBottomOffset + CaptionTopOffset + FPage.PPIScale(1),
          CompleteWidth + TabLeftOffset + CurrTabWidth - CaptionRightOffset,
          PanelHeight);
    end;

    CaptionString := Page.Pages[I].Caption;

//    DrawText(Canvas.Handle, PChar(CaptionString), Length(CaptionString),
//      ARect, DT_LEFT or DT_SINGLELINE or DT_END_ELLIPSIS);
    JvDrawTextWithMiddleEllipsis(Canvas, CaptionString, ARect, DT_LEFT or DT_SINGLELINE);

    if FShowTabImages and (Page.Images <> nil) and (CurrTabWidth > ImageWidth + 2 * CaptionLeftOffset) then
      Page.Images.Draw(Canvas, CompleteWidth + TabLeftOffset + CaptionLeftOffset,
        TabBottomOffset + CaptionTopOffset + FPage.PPIScale(1), Page.Pages[I].ImageIndex, True);

    Inc(CompleteWidth, CurrTabWidth + TabSplitterWidth);
  end;

  Canvas.Brush.Color := Page.ActiveSheetColor;
  ARect := ClientRect;
  Canvas.FrameRect(ARect);
end;

procedure TJvDockTabPanel.Resize;
begin
  inherited Resize;
  SetShowTabWidth;
end;

procedure TJvDockTabPanel.SetCaptionLeftOffset(const Value: Integer);
begin
  if FCaptionLeftOffset <> Value then
  begin
    FCaptionLeftOffset := Value;
    Invalidate;
  end;
end;

procedure TJvDockTabPanel.SetCaptionRightOffset(const Value: Integer);
begin
  if FCaptionRightOffset <> Value then
  begin
    FCaptionRightOffset := Value;
    Invalidate;
  end;
end;

procedure TJvDockTabPanel.SetCaptionTopOffset(const Value: Integer);
begin
  if FCaptionTopOffset <> Value then
  begin
    FCaptionTopOffset := Value;
    Invalidate;
  end;
end;

procedure TJvDockTabPanel.SetPage(const Value: TJvDockVIDTabPageControl);
begin
  FPage := Value;
end;

procedure TJvDockTabPanel.SetPanelHeight(const Value: Integer);
begin
  if PanelHeight <> Value then
  begin
    case Page.TabPosition of
      tpLeft, tpRight:
        Width := Value;
      tpTop, tpBottom:
        Height := Value;
    end;
    SetShowTabWidth;
  end;
end;

procedure TJvDockTabPanel.SetTabBottomOffset(const Value: Integer);
begin
  if FTabBottomOffset <> Value then
  begin
    FTabBottomOffset := Value;
    Invalidate;
  end;
end;

procedure TJvDockTabPanel.SetTabLeftOffset(const Value: Integer);
begin
  if FTabLeftOffset <> Value then
  begin
    FTabLeftOffset := Value;
    Invalidate;
  end;
end;

procedure TJvDockTabPanel.SetTabRightOffset(const Value: Integer);
begin
  if FTabRightOffset <> Value then
  begin
    FTabRightOffset := Value;
    Invalidate;
  end;
end;

procedure TJvDockTabPanel.SetTabSplitterWidth(const Value: Integer);
begin
  if FTabSplitterWidth <> Value then
  begin
    FTabSplitterWidth := Value;
    Invalidate;
  end;
end;

procedure TJvDockTabPanel.SetTabTopOffset(const Value: Integer);
begin
  if FTabTopOffset <> Value then
  begin
    FTabTopOffset := Value;
    Invalidate;
  end;
end;

procedure TJvDockTabPanel.SetTotalTabWidth(const Value: Integer);
begin
end;

function TJvDockTabPanel.GetCaptionLeftOffset: Integer;
begin
  Result := FPage.PPIScale(FCaptionLeftOffset);
end;

function TJvDockTabPanel.GetCaptionRightOffset: Integer;
begin
  Result := FPage.PPIScale(FCaptionRightOffset);
end;

function TJvDockTabPanel.GetCaptionTopOffset: Integer;
begin
  Result := FPage.PPIScale(FCaptionTopOffset);
end;

function TJvDockTabPanel.GetDockClientFromPageIndex(Index: Integer): TControl;
begin
  Result := nil;
  if Index >= 0 then
    if Page.Pages[Index].ControlCount = 1 then
    begin
      Result := Page.Pages[Index].Controls[0];
      if Result.HostDockSite <> Page then
        Result := nil;
    end;
end;

procedure TJvDockTabPanel.SetShowTabWidth;
var
  I, J, TempWidth: Integer;
  PanelWidth, VisibleCount: Integer;
  ImageWidth: Integer;
begin
  if Page = nil then
    Exit;
  if FSortList = nil then
    Exit;
  PanelWidth := 0;
  case Page.TabPosition of
    tpTop, tpBottom:
      PanelWidth := Width;
    tpLeft, tpRight:
      PanelWidth := Height;
  end;

  TempWidth := PanelWidth - CaptionLeftOffset - CaptionRightOffset;
  if Page.ShowTabImages then
    ImageWidth := Page.Images.Width + CaptionLeftOffset
  else
    ImageWidth := 0;
  VisibleCount := Page.VisibleSheetCount;
  J := 0;
  for I := 0 to FSortList.Count - 1 do
  begin
    if not Sorts[I].TabVisible then
      Continue;
    if (VisibleCount - J) * (Sorts[I].TabWidth + TabSplitterWidth + ImageWidth) > TempWidth then
      Sorts[I].FShowTabWidth := TempWidth div (VisibleCount - J) - TabSplitterWidth
    else
      Sorts[I].FShowTabWidth := Sorts[I].TabWidth + ImageWidth;
    Dec(TempWidth, Sorts[I].FShowTabWidth + TabSplitterWidth);
    Inc(J);
  end;
end;

procedure TJvDockTabPanel.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  if FSelectHotIndex <> -1 then
  begin
    FSelectHotIndex := -1;
    Invalidate;
  end;
end;

procedure TJvDockTabPanel.SetShowTabImages(const Value: Boolean);
begin
  if FShowTabImages <> Value then
  begin
    FShowTabImages := Value;
    SetShowTabWidth;
    Invalidate;
  end;
end;

procedure TJvDockTabPanel.SetShowTabHints(const Value: Boolean);
begin
  if FShowTabHints <> Value then
  begin
    FShowTabHints := Value;
    if not FShowTabHints then
      Hint := '';
    Invalidate;
  end;
end;

procedure TJvDockTabPanel.SetTabHeight(const Value: Integer);
begin
  FTabHeight := Value;
  //Height := TabHeight + TabTopOffset + TabBottomOffset;
end;

//=== { TJvDockVIDTabSheet } =================================================

constructor TJvDockVIDTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsSourceDockClient := False;
end;

destructor TJvDockVIDTabSheet.Destroy;
begin
  if (PageControl is TJvDockVIDTabPageControl) and (PageControl <> nil) then
    TJvDockVIDTabPageControl(PageControl).Panel.DeleteSorts(Self);
  inherited Destroy;
end;

procedure TJvDockVIDTabSheet.Loaded;
begin
  inherited Loaded;
  SetSheetSort(Caption);
end;

procedure TJvDockVIDTabSheet.SetPageControl(APageControl: TJvDockPageControl);
begin
  inherited SetPageControl(APageControl);
end;

procedure TJvDockVIDTabSheet.SetSheetSort(const CaptionStr: string);
var
  TabPanel: TJvDockTabPanel;
  TempWidth: Integer;

  procedure DoSetSheetSort;
  var
    I: Integer;
  begin
    TJvDockVIDTabPageControl(PageControl).Panel.FSortList.Remove(Self);
    for I := 0 to TJvDockVIDTabPageControl(PageControl).Panel.FSortList.Count - 1 do
      if TJvDockVIDTabPageControl(PageControl).Panel.Sorts[I].TabWidth > TempWidth then
      begin
        TJvDockVIDTabPageControl(PageControl).Panel.FSortList.Insert(I, Self);
        Exit;
      end;
    TJvDockVIDTabPageControl(PageControl).Panel.FSortList.Add(Self);
  end;

begin
  if (PageControl is TJvDockVIDTabPageControl) and (PageControl <> nil) then
  begin
    TabPanel := TJvDockVIDTabPageControl(PageControl).Panel;
    if PageControl.ActivePage = Self then
      TabPanel.Canvas.Font.Assign(TabPanel.Page.ActiveFont)
    else
      TabPanel.Canvas.Font.Assign(TabPanel.Page.InactiveFont);
    // Scale Font
    TabPanel.Canvas.Font.Height := MulDiv(TabPanel.Canvas.Font.Height,
      FCurrentPPI, TabPanel.Canvas.Font.PixelsPerInch);

    TempWidth := TabPanel.Canvas.TextWidth(
      CaptionStr) + TabPanel.CaptionLeftOffset + TabPanel.CaptionRightOffset;
    if TempWidth <> FTabWidth then
    begin
      DoSetSheetSort;
      FTabWidth := TempWidth;
      TabPanel.SetShowTabWidth;
      TabPanel.Invalidate;
    end;
  end;
end;

procedure TJvDockVIDTabSheet.SetTabWidth(const Value: Integer);
begin
  FTabWidth := Value;
end;

procedure TJvDockVIDTabSheet.UpdateTabShowing;
begin
  inherited UpdateTabShowing;
  TJvDockVIDTabPageControl(PageControl).Panel.SetShowTabWidth;
end;

procedure TJvDockVIDTabSheet.WMSetText(var Msg: TMessage);
begin
  inherited;
  SetSheetSort(PChar(Msg.LParam));
end;

//=== { TJvDockVIDDragDockObject } ===========================================

constructor TJvDockVIDDragDockObject.Create(AControl: TControl);

  procedure DoGetSourceDockClients(Control: TControl);
  var
    I: Integer;
    DockableControl: TWinControl;
  begin
    if Control is TJvDockableForm then
    begin
      DockableControl := TJvDockableForm(Control).DockableControl;
      for I := 0 to DockableControl.DockClientCount - 1 do
        DoGetSourceDockClients(DockableControl.DockClients[I]);
    end
    else
      FSourceDockClientList.Add(Control);
  end;

begin
  inherited Create(AControl);
  FSourceDockClientList := TList.Create;
  DoGetSourceDockClients(AControl);
  FDropTabControl := nil;
  FIsTabDockOver := False;
  CurrState := dsDragEnter;
  OldState := CurrState;
end;

destructor TJvDockVIDDragDockObject.Destroy;
begin
  FDropTabControl := nil;
  FSourceDockClientList.Free;
  inherited Destroy;
end;

procedure TJvDockVIDDragDockObject.GetBrush_PenSize_DrawRect(var ABrush: TBrush;
  var PenSize: Integer; var DrawRect: TRect; Erase: Boolean);
begin
  if DragTarget = nil then
    DropAlign := alNone;
  inherited GetBrush_PenSize_DrawRect(ABrush, PenSize, DrawRect, Erase);
  FIsTabDockOver := ((FOldDropAlign = alClient) and FErase) or
    ((DropAlign = alClient) and not FErase);
  FOldDropAlign := DropAlign;
  FOldTarget := DragTarget;
end;

// (rom) unused writeable const option removed

procedure TJvDockVIDDragDockObject.DefaultDockImage(Erase: Boolean);
var
  DrawRect: TRect;
  TabControlRect: TRect;
  TabRect: TRect;
  PenSize: Integer;
  ABrush: TBrush;
  ShowTab: Boolean;
  LeftOffset: Integer;
  BottomOffset: Integer;
  MaxTabWidth: Integer;
  PPI: Integer;
begin
  FErase := Erase;
  GetBrush_PenSize_DrawRect(ABrush, PenSize, DrawRect, Erase);
  if Erase then
    Exit;  // No need to erase

  ShowTab := False;
  if FIsTabDockOver and Assigned(FDropTabControl) then
  begin
    TabControlRect := FDropTabControl.BoundsRect;
    TabControlRect := Classes.Rect(FDropTabControl.ClientToScreen(TabControlRect.TopLeft),
                     FDropTabControl.ClientToScreen(TabControlRect.BottomRight));
    // This is to make sure the TabControlRect is included in the DrawRect
    if PtInRect(DrawRect, TabControlRect.TopLeft) and
       PtInRect(DrawRect, Point(TabControlRect.BottomRight.X -1,
                                TabControlRect.BottomRight.Y -1))
    then
      ShowTab := True;
  end;

  if not ShowTab then
  begin
    if (DropAlign = alNone) and (DragTarget = nil) then
    begin
      PPI := Screen.MonitorFromPoint(DragPos).PixelsPerInch;
      DockRect.Width := MulDiv(DockRect.Width, PPI, Control.CurrentPPI);
      DockRect.Height := MulDiv(DockRect.Height, PPI, Control.CurrentPPI);
      DrawRect := DockRect;
    end;
    AlphaBlendedForm.Visible := True;
    AlphaBlendedForm.BoundsRect := DrawRect;
    AlphaBlendedTab.Visible := False;
    AlphaBlendedTab.BoundsRect := Rect(0, 0, 0, 0);
  end
  else
  begin
    LeftOffset := FDropTabControl.TabLeftOffset;
    BottomOffset := FDropTabControl.Panel.TabHeight;
    if FDropTabControl.Panel.Page.Count > 0 then
      MaxTabWidth := FDropTabControl.Panel.Sorts[0].TabWidth
    else
      MaxTabWidth := 30;

    if TabControlRect.Right - TabControlRect.Left < LeftOffset +  2 * MaxTabWidth then
      MaxTabWidth := (TabControlRect.Right - TabControlRect.Left - LeftOffset) div 2;

    if TabControlRect.Bottom - TabControlRect.Top  < 2 * BottomOffset then
      BottomOffset := Max((TabControlRect.Bottom - TabControlRect.Top) div 2, 0);

    Assert(FDropTabControl.TabPosition in [tpBottom, tpTop],
      RsEDockCannotSetTabPosition);

    TabRect := TabControlRect;
    if FDropTabControl.TabPosition = tpBottom then
    begin
      Dec(TabControlRect.Bottom, BottomOffset);
      AlphaBlendedForm.Visible := True;
      AlphaBlendedForm.BoundsRect := TabControlRect;
      TabRect := Bounds(TabRect.Left + LeftOffset, TabRect.Bottom - BottomOffset,
                      MaxTabWidth, BottomOffset);
      AlphaBlendedTab.Visible := True;
      AlphaBlendedTab.BoundsRect := TabRect;
    end
    else
    begin
      Inc(TabControlRect.Top, BottomOffset);
      AlphaBlendedForm.Visible := True;
      AlphaBlendedForm.BoundsRect := TabControlRect;
      TabRect := Bounds(TabRect.Left + LeftOffset, TabRect.Top,
                      MaxTabWidth, BottomOffset);
      AlphaBlendedTab.Visible := True;
      AlphaBlendedTab.BoundsRect := TabRect;
    end;
  end;
end;

function TJvDockVIDDragDockObject.DragFindWindow(const Pos: TPoint): THandle;
begin
  Result := 0;
end;

function TJvDockVIDDragDockObject.GetDropCtl: TControl;
var
  ARect: TRect;
  I: Integer;
begin
  Result := inherited GetDropCtl;
  if (Result = nil) and (TargetControl is TJvDockCustomPanel) then
    for I := 0 to TargetControl.DockClientCount - 1 do
      if TargetControl.DockClients[I].Visible then
      begin
        ARect := TJvDockCustomPanel(DragTarget).JvDockManager.GetFrameRectEx(TargetControl.DockClients[I]);
        if PtInRect(ARect, DragPos) then
        begin
          Result := TargetControl.DockClients[I];
          Exit;
        end;
      end;
end;

function TJvDockVIDDragDockObject.GetSourceDockClient(Index: Integer): TControl;
begin
  Result := TControl(FSourceDockClientList[Index]);
end;

function TJvDockVIDDragDockObject.GetSourceDockClientCount: Integer;
begin
  Result := FSourceDockClientList.Count;
end;

procedure TJvDockVIDDragDockObject.MouseMsg(var Msg: TMessage);
var
  APos: TPoint;
  Page: TJvDockVIDTabPageControl;
begin
  inherited MouseMsg(Msg);

  // Warren added assertions:
  Assert(Assigned(JvGlobalDockClient));
  Assert(Assigned(JvGlobalDockManager));

  case Msg.Msg of
    WM_CAPTURECHANGED:
      begin
        // Warren added Assertions:
        Assert(Assigned(JvGlobalDockClient.ParentForm));
        // Assert(Assigned(JvGlobalDockClient.ParentForm.HostDockSite));

        if Assigned( JvGlobalDockClient.ParentForm.HostDockSite) and
          (JvGlobalDockClient.ParentForm.HostDockSite is TJvDockVIDTabPageControl) then
            TJvDockVIDTabPageControl(JvGlobalDockClient.ParentForm.HostDockSite).Panel.MouseUp(mbLeft, [], 0, 0)
        else
        if TWinControl(JvGlobalDockManager.DragObject.DragTarget) is TJvDockVIDTabPageControl then
          TJvDockVIDTabPageControl(JvGlobalDockManager.DragObject.TargetControl).Panel.MouseUp(mbLeft, [], 0, 0);

      end;
    WM_MOUSEMOVE:
      if JvGlobalDockManager.DragObject.TargetControl is TJvDockVIDTabPageControl then
      begin
        Page := TJvDockVIDTabPageControl(JvGlobalDockManager.DragObject.TargetControl);
        if Page.FTempSheet <> nil then
        begin
          APos := Point(TWMMouse(Msg).XPos, TWMMouse(Msg).YPos);
          APos := Page.Panel.ScreenToClient(APos);
          Page.Panel.MouseMove([], APos.X, APos.Y);
        end;
      end;
  end;
end;

procedure TJvDockVIDDragDockObject.SetOldState(const Value: TDragState);
begin
  FOldState := Value;
end;

procedure TJvDockVIDDragDockObject.SetCurrState(const Value: TDragState);
begin
  FCurrState := Value;
end;

function TJvDockVIDDragDockObject.CanLeave(NewTarget: TWinControl): Boolean;
begin
  Result := inherited CanLeave(NewTarget);
end;

//=== { TJvDockVIDZone } =====================================================

destructor TJvDockVIDZone.Destroy;
begin
  inherited Destroy;
end;

function TJvDockVIDZone.GetSplitterLimit(IsMin: Boolean): Integer;
begin
  if IsMin then
    Result := ZoneLimit
  else
    Result := LimitBegin;
end;

procedure TJvDockVIDZone.Insert(DockSize: Integer; Hide: Boolean);
var
  PrevShift: Integer;
  NextShift: Integer;
  TempSize: Integer;
  BorderSize: Integer;
  BeforeVisibleZone: TJvDockZone;
  AfterVisibleZone: TJvDockZone;
  BeginSize: Integer;
begin
  if (ParentZone <> nil) and (ParentZone.VisibleChildCount = 0) then
    ParentZone.Insert(ParentZone.VisibleSize, Hide);

  if (ParentZone = nil) or ((ParentZone = Tree.TopZone) and (ParentZone.ChildCount <= 1)) then
  begin
    Visibled := True;
    Exit;
  end;

  if (ParentZone <> nil) and (ParentZone.ChildZones <> nil) then
    BeginSize := ParentZone.ChildZones.LimitBegin
  else
    BeginSize := 0;

  BeforeVisibleZone := BeforeClosestVisibleZone;
  AfterVisibleZone := AfterClosestVisibleZone;

  BorderSize := TJvDockVIDTree(Tree).BorderWidth * Integer(AfterClosestVisibleZone <> nil) div 2;
  TempSize := ParentZone.HeightWidth[ParentZone.Orientation] + BorderSize;
  Visibled := False;

  if DockSize >= TempSize - (ParentZone.VisibleChildCount) * TJvDockVIDTree(Tree).MinSize then
    DockSize := (TempSize - (ParentZone.VisibleChildCount) * TJvDockVIDTree(Tree).MinSize) div 2;

  if DockSize < TJvDockVIDTree(Tree).MinSize then
    DockSize := TempSize div 2;

  if (BeforeVisibleZone = nil) and (AfterVisibleZone = nil) then
  begin
    PrevShift := 0;
    NextShift := 0;
    ZoneLimit := TempSize + BeginSize;
  end
  else
  if BeforeVisibleZone = nil then
  begin
    PrevShift := 0;
    NextShift := DockSize + BorderSize;
    ZoneLimit := DockSize + LimitBegin + BorderSize;
    if ParentZone.VisibleChildCount = 1 then
      AfterVisibleZone.ZoneLimit := TempSize + BeginSize;
  end
  else
  if AfterVisibleZone = nil then
  begin
    PrevShift := DockSize + BorderSize;
    NextShift := 0;
    if (ParentZone.VisibleChildCount = 1) and (ParentZone = Tree.TopZone) then
      BeforeVisibleZone.ZoneLimit := Tree.TopXYLimit - PrevShift
    else
      BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.ZoneLimit - PrevShift;
    ZoneLimit := TempSize + BeginSize;
  end
  else
  begin
    PrevShift := Round((BeforeVisibleZone.ZoneLimit - BeginSize) * (DockSize + BorderSize) / TempSize);
    NextShift := DockSize - PrevShift;
    if (ParentZone.VisibleChildCount = 1) and (ParentZone = Tree.TopZone) then
      BeforeVisibleZone.ZoneLimit := Tree.TopXYLimit - PrevShift
    else
      BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.ZoneLimit - PrevShift;
    ZoneLimit := BeforeVisibleZone.ZoneLimit + DockSize;
  end;

  if PrevShift <> 0 then
  begin
    with TJvDockVIDTree(Tree) do
    begin
      ReplacementZone := BeforeVisibleZone;
      try
        if (BeforeVisibleZone.ZoneLimit - BeginSize) * (BeforeVisibleZone.ZoneLimit - BeginSize + PrevShift) <> 0 then
          ScaleBy := (BeforeVisibleZone.ZoneLimit - BeginSize) / (BeforeVisibleZone.ZoneLimit - BeginSize + PrevShift)
        else
          ScaleBy := 1;
        ParentLimit := BeginSize;
        ShiftScaleOrientation := ParentZone.Orientation;
        if ScaleBy <> 1 then
          ForEachAt(ParentZone.ChildZones, ScaleChildZone, tskMiddle, tspChild);
      finally
        ReplacementZone := nil;
      end;
    end;

    if BeforeVisibleZone.LimitSize < TJvDockVIDTree(Tree).MinSize then
      BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.LimitBegin + TJvDockVIDTree(Tree).MinSize;
  end;

  if NextShift <> 0 then
    with TJvDockVIDTree(Tree) do
    begin
      if (TempSize + BeginSize - LimitBegin - NextShift) * (TempSize + BeginSize - LimitBegin) <> 0 then
        ScaleBy := (TempSize + BeginSize - LimitBegin - NextShift) / (TempSize + BeginSize - LimitBegin)
      else
        ScaleBy := 1;
      ParentLimit := TempSize + BeginSize;
      ShiftScaleOrientation := ParentZone.Orientation;
      if ScaleBy <> 1 then
        ForEachAt(AfterVisibleZone, ScaleSiblingZone, tskForward);
    end;
  Visibled := True;
end;

procedure TJvDockVIDZone.Remove(DockSize: Integer; Hide: Boolean);
var
  PrevShift: Integer;
  NextShift: Integer;
  TempSize: Integer;
  BorderSize: Integer;
  BeforeVisibleZone: TJvDockZone;
  AfterVisibleZone: TJvDockZone;
  BeginSize: Integer;
begin
  if (ParentZone <> nil) and (ParentZone.VisibleChildCount = 1) and (ParentZone <> Tree.TopZone) then
    ParentZone.Remove(ParentZone.LimitSize, Hide);

  if (ParentZone = nil) or ((ParentZone = Tree.TopZone) and (ParentZone.ChildCount <= 1)) then
  begin
    Visibled := False;
    Exit;
  end;

  if (ParentZone <> nil) and (ParentZone.ChildZones <> nil) then
    BeginSize := ParentZone.ChildZones.LimitBegin
  else
    BeginSize := 0;

  BeforeVisibleZone := BeforeClosestVisibleZone;
  AfterVisibleZone := AfterClosestVisibleZone;

  BorderSize := TJvDockVIDTree(Tree).BorderWidth * Integer(AfterClosestVisibleZone <> nil) div 2;
  TempSize := ParentZone.HeightWidth[ParentZone.Orientation] + BorderSize;

  if DockSize > TempSize - (ParentZone.VisibleChildCount - 1) * TJvDockVIDTree(Tree).MinSize then
    DockSize := TempSize - (ParentZone.VisibleChildCount - 1) * TJvDockVIDTree(Tree).MinSize;
  if DockSize = 0 then
    DockSize := TempSize div 2;

  Visibled := False;
  if (BeforeVisibleZone = nil) and (AfterVisibleZone = nil) then
    Exit;

  if BeforeVisibleZone = nil then
  begin
    PrevShift := 0;
    NextShift := -DockSize + BorderSize;
    ZoneLimit := -DockSize + BorderSize + BeginSize;
  end
  else
  if AfterVisibleZone = nil then
  begin
    PrevShift := -DockSize + BorderSize;
    NextShift := 0;
    BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.ZoneLimit - PrevShift;
    ZoneLimit := TempSize + BeginSize;
  end
  else
  begin
    PrevShift := -Round((BeforeVisibleZone.ZoneLimit - BeginSize) * (DockSize + BorderSize) / (TempSize - (DockSize +
      BorderSize)));
    NextShift := -DockSize - PrevShift;
    BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.ZoneLimit - PrevShift;
    ZoneLimit := BeforeVisibleZone.ZoneLimit;
  end;

  if PrevShift <> 0 then
  begin
    with TJvDockVIDTree(Tree) do
    begin
      ReplacementZone := BeforeVisibleZone;
      try
        if (BeforeVisibleZone.ZoneLimit - BeginSize) * (BeforeVisibleZone.ZoneLimit - BeginSize + PrevShift) <> 0 then
          ScaleBy := (BeforeVisibleZone.ZoneLimit - BeginSize) / (BeforeVisibleZone.ZoneLimit - BeginSize + PrevShift)
        else
          ScaleBy := 1;
        ParentLimit := BeginSize;
        ShiftScaleOrientation := ParentZone.Orientation;
        if ScaleBy <> 1 then
          ForEachAt(ParentZone.ChildZones, ScaleChildZone, tskMiddle, tspChild);
      finally
        ReplacementZone := nil;
      end;
    end;

    if BeforeVisibleZone.LimitSize < TJvDockVIDTree(Tree).MinSize then
      BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.LimitBegin + TJvDockVIDTree(Tree).MinSize;
  end;

  if NextShift <> 0 then
    with TJvDockVIDTree(Tree) do
    begin
      if (TempSize + BeginSize - LimitBegin) * (TempSize + BeginSize - LimitBegin + NextShift) <> 0 then
        ScaleBy := (TempSize + BeginSize - LimitBegin) / (TempSize + BeginSize - LimitBegin + NextShift)
      else
        ScaleBy := 1;
      ParentLimit := TempSize + BeginSize;
      ShiftScaleOrientation := ParentZone.Orientation;
      if ScaleBy <> 1 then
        ForEachAt(AfterVisibleZone, ScaleSiblingZone, tskForward);
    end;
end;

//=== { TJvDockVIDTabServerOption } ==========================================

constructor TJvDockVIDTabServerOption.Create(ADockStyle: TJvDockObservableStyle);
begin
  inherited Create(ADockStyle);
  TabPosition := tpBottom;
  FActiveFont := TFont.Create;
  FActiveFont.OnChange := FontChanged;
  FActiveSheetColor := clBtnFace;
  FHotTrackColor := clBlue;
  FInactiveFont := TFont.Create;
  FInactiveFont.Color := clWhite;
  FInactiveFont.OnChange := FontChanged;
  FInactiveSheetColor := clBtnShadow;
  FShowTabImages := False;
  FShowTabHints := False;
  FShowCloseButtonOnGrabber := True;
  FShowCloseButtonOnTabs := False;
end;

destructor TJvDockVIDTabServerOption.Destroy;
begin
  FActiveFont.Free;
  FInactiveFont.Free;
  inherited Destroy;
end;

procedure TJvDockVIDTabServerOption.Assign(Source: TPersistent);
var
  Src: TJvDockVIDTabServerOption;
begin
  if Source is TJvDockVIDTabServerOption then
  begin
    BeginUpdate;
    try
      Src := TJvDockVIDTabServerOption(Source);

      ActiveFont := Src.ActiveFont;
      ActiveSheetColor := Src.ActiveSheetColor;
      HotTrackColor := Src.HotTrackColor;
      InactiveFont := Src.InactiveFont;
      InactiveSheetColor := Src.InactiveSheetColor;
      ShowTabImages := Src.ShowTabImages;
      ShowTabHints := Src.ShowTabHints;

      inherited Assign(Source);
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TJvDockVIDTabServerOption.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TJvDockVIDTabServerOption.SetActiveFont(Value: TFont);
begin
  FActiveFont.Assign(Value);
end;

procedure TJvDockVIDTabServerOption.SetActiveSheetColor(const Value: TColor);
begin
  if FActiveSheetColor <> Value then
  begin
    FActiveSheetColor := Value;
    Changed;
  end;
end;

procedure TJvDockVIDTabServerOption.SetHotTrackColor(const Value: TColor);
begin
  if FHotTrackColor <> Value then
  begin
    FHotTrackColor := Value;
    Changed;
  end;
end;

procedure TJvDockVIDTabServerOption.SetInactiveFont(Value: TFont);
begin
  FInactiveFont.Assign(Value);
end;

procedure TJvDockVIDTabServerOption.SetInactiveSheetColor(const Value: TColor);
begin
  if FInactiveSheetColor <> Value then
  begin
    FInactiveSheetColor := Value;
    Changed;
  end;
end;

procedure TJvDockVIDTabServerOption.SetShowTabHints(const Value: Boolean);
begin
  if FShowTabHints <> Value then
  begin
    FShowTabHints := Value;
    Changed;
  end;
end;

procedure TJvDockVIDTabServerOption.SetShowTabImages(const Value: Boolean);
begin
  if FShowTabImages <> Value then
  begin
    FShowTabImages := Value;
    Changed;
  end;
end;

procedure TJvDockVIDTabServerOption.SetTabPosition(const Value: TTabPosition);
begin
//  if Value = tpBottom then
    inherited SetTabPosition(Value)
//  else // TabPosition property must be tpBottom.
//    raise Exception.CreateRes(@RsEDockTabPositionMustBetpBottom);
end;

procedure TJvDockVIDTabServerOption.SetShowCloseButtonOnGrabber(
  const Value: Boolean);
begin
  if FShowCloseButtonOnGrabber <> Value then
  begin
    FShowCloseButtonOnGrabber := Value;
    Changed;
  end;
end;

procedure TJvDockVIDTabServerOption.SetShowCloseButtonOnTabs(
  const Value: Boolean);
begin
  if FShowCloseButtonOnTabs <> Value then
  begin
    FShowCloseButtonOnTabs := Value;
    Changed;
  end;
end;

//=== { TJvDockVIDConjoinServerOption } ======================================

constructor TJvDockVIDConjoinServerOption.Create(ADockStyle: TJvDockObservableStyle);
begin
  inherited Create(ADockStyle);
  GrabbersSize := VIDDefaultDockGrabbersSize;
  SplitterWidth := VIDDefaultDockSplitterWidth;
  FActiveFont := TFont.Create;
  FActiveFont.OnChange := FontChanged;
  FInactiveFont := TFont.Create;
  FInactiveFont.OnChange := FontChanged;
  SystemInfo := True;
end;

destructor TJvDockVIDConjoinServerOption.Destroy;
begin
  { Make sure we unregister, can be called more than once }
  UnRegisterSettingChangeClient(Self);
  FActiveFont.Free;
  FInactiveFont.Free;
  inherited Destroy;
end;

procedure TJvDockVIDConjoinServerOption.Assign(Source: TPersistent);
var
  Src: TJvDockVIDConjoinServerOption;
begin
  if Source is TJvDockVIDConjoinServerOption then
  begin
    BeginUpdate;
    try
      Src := TJvDockVIDConjoinServerOption(Source);

      TextEllipsis := Src.TextEllipsis;
      TextAlignment := Src.TextAlignment;
      InactiveTitleEndColor := Src.InactiveTitleEndColor;
      InactiveTitleStartColor := Src.InactiveTitleStartColor;
      InactiveTitleVerticalGradient := Src.InactiveTitleVerticalGradient;
      ActiveTitleEndColor := Src.ActiveTitleEndColor;
      ActiveTitleStartColor := Src.ActiveTitleStartColor;
      ActiveTitleVerticalGradient := Src.ActiveTitleVerticalGradient;
      ActiveDockGrabber := Src.ActiveDockGrabber;
      ActiveFont := Src.ActiveFont;
      InactiveFont := Src.InactiveFont;
      SystemInfo := Src.SystemInfo;

      inherited Assign(Source);
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TJvDockVIDConjoinServerOption.SetActiveTitleEndColor(const Value: TColor);
begin
  if Value <> FActiveTitleEndColor then
  begin
    FActiveTitleEndColor := Value;
    // setting SystemInfo to False does not trigger a Changed call
    SystemInfo := False;
    Changed;
  end;
end;

procedure TJvDockVIDConjoinServerOption.SetActiveTitleStartColor(const Value: TColor);
begin
  if Value <> FActiveTitleStartColor then
  begin
    FActiveTitleStartColor := Value;
    SystemInfo := False;
    Changed;
  end;
end;

procedure TJvDockVIDConjoinServerOption.SetActiveTitleVerticalGradient(const Value: Boolean);
begin
  if Value <> FActiveTitleVerticalGradient then
  begin
    FActiveTitleVerticalGradient := Value;
    SystemInfo := False;
    Changed;
  end;
end;

procedure TJvDockVIDConjoinServerOption.SetActiveDockGrabber(
  const Value: Boolean);
begin
  if Value <> FActiveDockGrabber then
  begin
    FActiveDockGrabber := Value;
    SystemInfo := False;
    Changed;
  end;
end;

procedure TJvDockVIDConjoinServerOption.SetInactiveTitleEndColor(const Value: TColor);
begin
  if Value <> FInactiveTitleEndColor then
  begin
    FInactiveTitleEndColor := Value;
    SystemInfo := False;
    Changed;
  end;
end;

procedure TJvDockVIDConjoinServerOption.SetInactiveTitleStartColor(const Value: TColor);
begin
  if Value <> FInactiveTitleStartColor then
  begin
    FInactiveTitleStartColor := Value;
    SystemInfo := False;
    Changed;
  end;
end;

procedure TJvDockVIDConjoinServerOption.SetInactiveTitleVerticalGradient(const Value: Boolean);
begin
  if Value <> FInactiveTitleVerticalGradient then
  begin
    FInactiveTitleVerticalGradient := Value;
    SystemInfo := False;
    Changed;
  end;
end;

procedure TJvDockVIDConjoinServerOption.SetSystemInfo(const Value: Boolean);
begin
  if Value <> FSystemInfo then
  begin
    if FSystemInfo then
      UnRegisterSettingChangeClient(Self);
    FSystemInfo := Value;
    if FSystemInfo then
    begin
      RegisterSettingChangeClient(Self, SettingChange);
      SetDefaultSystemCaptionInfo;
      // If necessary Changed is called via SetDefaultSystemCaptionInfo
    end;
  end;
end;

procedure TJvDockVIDConjoinServerOption.SetTextAlignment(
  const Value: TAlignment);
begin
  if Value <> FTextAlignment then
  begin
    FTextAlignment := Value;
    SystemInfo := False;
    Changed;
  end;
end;

procedure TJvDockVIDConjoinServerOption.SetTextEllipsis(const Value: Boolean);
begin
  if Value <> FTextEllipsis then
  begin
    FTextEllipsis := Value;
    SystemInfo := False;
    Changed;
  end;
end;

procedure TJvDockVIDConjoinServerOption.SetDefaultSystemCaptionInfo;
var
  Saved: Boolean;
begin
  Saved := SystemInfo;
  BeginUpdate;
  FSystemInfo := False;
  try
    UpdateDefaultSystemCaptionInfo;
  finally
    FSystemInfo := Saved;
    EndUpdate;
  end;
end;

procedure TJvDockVIDConjoinServerOption.UpdateDefaultSystemCaptionInfo;
begin
  ActiveTitleStartColor := JvDockGetActiveTitleBeginColor;
  ActiveTitleEndColor := JvDockGetActiveTitleEndColor;
  ActiveTitleVerticalGradient := False;
  InactiveTitleStartColor := JvDockGetInactiveTitleBeginColor;
  InactiveTitleEndColor := JvDockGetInactiveTitleEndColor;
  InactiveTitleVerticalGradient := False;
  ActiveDockGrabber := False;
  TextAlignment := taLeftJustify;
  TextEllipsis := True;
  ActiveFont := JvDockGetTitleFont;
  ActiveFont.Style := FActiveFont.Style + [fsBold];
  InactiveFont := FActiveFont;
  ActiveFont.Color := JvDockGetActiveTitleFontColor;
  InactiveFont.Color := JvDockGetInactiveTitleFontColor;
  GrabbersSize := VIDDefaultDockGrabbersSize;
  SplitterWidth := VIDDefaultDockSplitterWidth;
end;

procedure TJvDockVIDConjoinServerOption.SetActiveFont(Value: TFont);
begin
  FActiveFont.Assign(Value);
end;

procedure TJvDockVIDConjoinServerOption.SetInactiveFont(Value: TFont);
begin
  FInactiveFont.Assign(Value);
end;

procedure TJvDockVIDConjoinServerOption.Changed;
begin
  inherited Changed;
//  SystemInfo := SystemInfo and (GrabbersSize = VIDDefaultDockGrabbersSize) and
//    (SplitterWidth = VIDDefaultDockSplitterWidth);
  TJvDockVIDStyle(DockStyle).DoSystemInfoChange(SystemInfo);
end;

function TJvDockVIDConjoinServerOption.IsNotSystemInfo: Boolean;
begin
  Result := not SystemInfo;
end;

procedure TJvDockVIDConjoinServerOption.FontChanged(Sender: TObject);
begin
  // setting SystemInfo to False does not trigger a Changed call
  SystemInfo := False;
  Changed;
end;

procedure TJvDockVIDConjoinServerOption.SettingChange(Sender: TObject);
begin
  { ?? }
  {DockStyle.ParentForm.Caption := '';}
  if SystemInfo then
    SetDefaultSystemCaptionInfo;
end;

{$IFNDEF COMPILER9_UP}
function GetRealParentForm(Control: TControl): TCustomForm;
begin
  while not (Control is TCustomForm) and (Control.Parent <> nil) do
    Control := Control.Parent;
  if Control is TCustomForm then
    Result := TCustomForm(Control)
  else
    Result := nil;
end;
{$ENDIF !COMPILER9_UP}

{$IFNDEF COMPILER9_UP}
type
  TWinControlAccessProtected = class(TWinControl);
{$ENDIF !COMPILER9_UP}

function GetDockManager(Control: TWinControl; out ADockManager: IDockManager): Boolean;
begin
  ADockManager := nil;
  {$IFDEF COMPILER9_UP}
  with Control do
    if UseDockManager then
      ADockManager := DockManager;
  {$ELSE}
  with TWinControlAccessProtected(Control) do
    if UseDockManager then
      ADockManager := DockManager;
  {$ENDIF COMPILER9_UP}
  Result := Assigned(ADockManager);
end;

procedure TJvDockVIDTree.InvalidateDockSite(const Client: TControl);
var
  ParentForm: TCustomForm;
  Rect: TRect;
  ADockManager: IDockManager;
begin
  {$IFDEF COMPILER9_UP}
  ParentForm := GetParentForm(Client, False);
  {$ELSE}
  ParentForm := GetRealParentForm(Client);
  {$ENDIF COMPILER9_UP}
  { Just invalidate the parent form's rect in the HostDockSite
    so that we can "follow focus" on docked items. }
  if (ParentForm <> nil) and (ParentForm.HostDockSite <> nil) then
  begin
    if GetDockManager(ParentForm.HostDockSite, ADockManager) then
    begin
      ADockManager.GetControlBounds(ParentForm, Rect);
      InvalidateRect(ParentForm.HostDockSite.Handle, @Rect, False);
    end;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
