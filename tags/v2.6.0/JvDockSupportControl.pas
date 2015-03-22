{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDockSupportControl.pas, released on 2003-12-31.

The Initial Developer of the Original Code is luxiaoban.
Portions created by luxiaoban are Copyright (C) 2002, 2003 luxiaoban.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDockSupportControl;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Messages, Windows, CommCtrl, Graphics, Controls, Forms, ImgList, Classes, ExtCtrls,
  ComCtrls,
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  JvComponent, JvAppStorage,
  JvDockTree;

type
  TJvAlphaBlendedForm = class(TForm)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

  TJvDockDragDockObject = class(TObject)
  private
    // FDockClient:TObject;{NEW: Opaque reference to TJvDockClient}
    FMouseDeltaX: Double;
    FMouseDeltaY: Double;
    FControl: TControl;
    FDragTarget: Pointer;
    FDragPos: TPoint;
    FDropOnControl: TControl;
    FDropAlign: TAlign;
    FDragHandle: THandle;
    FDragTargetPos: TPoint;
    FCancelling: Boolean;
    FFloating: Boolean;
    FFrameWidth: Integer;
    FBrush: TBrush;
    FCtrlDown: Boolean;
    FDockRect: TRect;
    FEraseDockRect: TRect;
    FAlphaBlendedForm: TJvAlphaBlendedForm;
    FAlphaBlendedTab: TJvAlphaBlendedForm;
    procedure SetBrush(const Value: TBrush);
    procedure SetDropAlign(const Value: TAlign);
    procedure SetDropOnControl(const Value: TControl);
    function GetTargetControl: TWinControl;
    procedure SetTargetControl(const Value: TWinControl);
    function GetAlphaBlendedTab: TJvAlphaBlendedForm;
  protected
    property AlphaBlendedForm: TJvAlphaBlendedForm read FAlphaBlendedForm;
    property AlphaBlendedTab: TJvAlphaBlendedForm read GetAlphaBlendedTab;
    procedure DefaultDockImage(Erase: Boolean); virtual;
    procedure DrawDragRect(DoErase: Boolean); virtual;
    procedure GetBrush_PenSize_DrawRect(var ABrush: TBrush; var PenSize: Integer;
      var DrawRect: TRect; Erase: Boolean); virtual;
    function GetFrameWidth: Integer; virtual;
    procedure SetFrameWidth(const Value: Integer); virtual;
    procedure MouseMsg(var Msg: TMessage); virtual;
    function CanLeave(NewTarget: TWinControl): Boolean; virtual;
  public
    constructor Create(AControl: TControl); virtual;
    destructor Destroy; override;

    procedure AdjustDockRect(const ARect: TRect); virtual;
    function Capture: THandle;
    function DragFindWindow(const Pos: TPoint): THandle; virtual;
    procedure ReleaseCapture(Handle: THandle);
    procedure EndDrag(Target: TObject; X, Y: Integer); virtual;
    procedure Finished(Target: TObject; X, Y: Integer; Accepted: Boolean); virtual;
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; virtual;
    function GetDragImages: TDragImageList; virtual;
    procedure DrawDragDockImage; virtual;
    procedure EraseDragDockImage; virtual;
    function GetDropCtl: TControl; virtual;

    property MouseDeltaX: Double read FMouseDeltaX write FMouseDeltaX;
    property MouseDeltaY: Double read FMouseDeltaY write FMouseDeltaY;
    property Control: TControl read FControl write FControl;
    property DockRect: TRect read FDockRect write FDockRect;
    property DragTarget: Pointer read FDragTarget write FDragTarget;
    property DragPos: TPoint read FDragPos write FDragPos;
    property DropOnControl: TControl read FDropOnControl write SetDropOnControl;
    property DropAlign: TAlign read FDropAlign write SetDropAlign;
    property DragHandle: THandle read FDragHandle write FDragHandle;
    property DragTargetPos: TPoint read FDragTargetPos write FDragTargetPos;
    property EraseDockRect: TRect read FEraseDockRect;
    property Cancelling: Boolean read FCancelling write FCancelling;
    property Floating: Boolean read FFloating write FFloating;
    property FrameWidth: Integer read GetFrameWidth write SetFrameWidth;
    property Brush: TBrush read FBrush write SetBrush;
    property CtrlDown: Boolean read FCtrlDown write FCtrlDown;
    property TargetControl: TWinControl read GetTargetControl write SetTargetControl;

    {DockClient: Opaque reference to TJvDockClient. Nil if none.}
    // property DockClient:TObject read FDockClient write FDockClient;
  end;

  TJvDockCustomControl = class(TJvCustomControl)
  private
    function GetJvDockManager: IJvDockManager;
  protected
    procedure WndProc(var Msg: TMessage); override;
    procedure CustomStartDock(var Source: TJvDockDragDockObject); virtual;
    procedure CustomGetSiteInfo(Source: TJvDockDragDockObject;
      Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
      var CanDock: Boolean); virtual;
    procedure CustomDockOver(Source: TJvDockDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); virtual;
    procedure CustomPositionDockRect(Source: TJvDockDragDockObject; X, Y: Integer); virtual;
    procedure CustomDockDrop(Source: TJvDockDragDockObject; X, Y: Integer); virtual;
    procedure CustomEndDock(Target: TObject; X, Y: Integer); virtual;
    function CustomUnDock(Source: TJvDockDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; virtual;
    procedure CustomGetDockEdge(Source: TJvDockDragDockObject; MousePos: TPoint; var DropAlign: TAlign); virtual;
  public
    procedure UpdateCaption(Exclude: TControl); virtual;
    property DockManager;
    property JvDockManager: IJvDockManager read GetJvDockManager{ write SetJvDockManager};
  end;

  TJvDockCustomPanel = class(TJvDockCustomControl)
  protected
    function CreateDockManager: IDockManager; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DockSite;
  end;

  TJvDockCustomTabControl = class;

  TJvDockTabStrings = class(TStrings)
  private
    FTabControl: TJvDockCustomTabControl;
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
  end;

  TJvDockDrawTabEvent = procedure(Control: TJvDockCustomTabControl; TabIndex: Integer;
    const Rect: TRect; Active: Boolean) of object;

  TJvDockPageControl = class;

  TJvDockCustomTabControl = class(TJvDockCustomControl)
  private
    FHotTrack: Boolean;
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FMultiLine: Boolean;
    FMultiSelect: Boolean;
    FOwnerDraw: Boolean;
    FRaggedRight: Boolean;
    FSaveTabIndex: Integer;
    FSaveTabs: TStringList;
    FScrollOpposite: Boolean;
    FStyle: TTabStyle;
    FTabPosition: TTabPosition;
    FTabs: TJvDockTabStrings;
    FTabSize: TSmallPoint;
    FUpdating: Boolean;
    FSavedAdjustRect: TRect;
    FOnChange: TNotifyEvent;
    FOnChanging: TTabChangingEvent;
    FOnDrawTab: TJvDockDrawTabEvent;
    FOnGetImageIndex: TTabGetImageEvent;
    function GetDisplayRect: TRect;
    function GetTabIndex: Integer;
    function GetTabs: TStrings;
    procedure ImageListChange(Sender: TObject);
    function InternalSetMultiLine(Value: Boolean): Boolean;
    procedure SetMultiLine(Value: Boolean);
    procedure SetMultiSelect(Value: Boolean);
    procedure SetOwnerDraw(Value: Boolean);
    procedure SetRaggedRight(Value: Boolean);
    procedure SetScrollOpposite(Value: Boolean);
    procedure SetStyle(Value: TTabStyle);
    procedure SetTabIndex(Value: Integer);
    procedure SetTabs(Value: TStrings);
    procedure SetTabWidth(Value: Smallint);
    procedure TabsChanged;
    procedure UpdateTabSize;
    procedure CMFontChanged(var Msg); message CM_FONTCHANGED;
    procedure CMSysColorChange(var Msg: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMTabStopChanged(var Msg: TMessage); message CM_TABSTOPCHANGED;
    procedure CNNotify(var Msg: TWMNotify); message CN_NOTIFY;
    procedure CMDialogChar(var Msg: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    procedure TCMAdjustRect(var Msg: TMessage); message TCM_ADJUSTRECT;
    procedure WMDestroy(var Msg: TWMDestroy); message WM_DESTROY;
    procedure WMNotifyFormat(var Msg: TMessage); message WM_NOTIFYFORMAT;
    procedure WMSize(var Msg: TMessage); message WM_SIZE;
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    function CanChange: Boolean; dynamic;
    function CanShowTab(TabIndex: Integer): Boolean; virtual;
    procedure Change; dynamic;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean); virtual;
    function GetImageIndex(TabIndex: Integer): Integer; virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PaintWindow(DC: HDC); override;
    procedure SetHotTrack(Value: Boolean); virtual;
    procedure SetImages(Value: TCustomImageList); virtual;
    procedure SetTabHeight(Value: Smallint); virtual;
    procedure SetTabPosition(Value: TTabPosition); virtual;
    procedure UpdateTabImages;
    property DisplayRect: TRect read GetDisplayRect;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property Images: TCustomImageList read FImages write SetImages;
    property MultiLine: Boolean read FMultiLine write SetMultiLine default False;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property OwnerDraw: Boolean read FOwnerDraw write SetOwnerDraw default False;
    property RaggedRight: Boolean read FRaggedRight write SetRaggedRight default False;
    property ScrollOpposite: Boolean read FScrollOpposite
      write SetScrollOpposite default False;
    property Style: TTabStyle read FStyle write SetStyle default tsTabs;
    property TabHeight: Smallint read FTabSize.Y write SetTabHeight default 0;
    property TabIndex: Integer read GetTabIndex write SetTabIndex default -1;
    property TabPosition: TTabPosition read FTabPosition write SetTabPosition default tpTop;
    property Tabs: TStrings read GetTabs write SetTabs;
    property TabWidth: Smallint read FTabSize.X write SetTabWidth default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TTabChangingEvent read FOnChanging write FOnChanging;
    property OnDrawTab: TJvDockDrawTabEvent read FOnDrawTab write FOnDrawTab;
    property OnGetImageIndex: TTabGetImageEvent read FOnGetImageIndex write FOnGetImageIndex;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IndexOfTabAt(X, Y: Integer): Integer;
    function GetHitTestInfoAt(X, Y: Integer): THitTests;
    function TabRect(Index: Integer): TRect;
    function RowCount: Integer;
    procedure ScrollTabs(Delta: Integer);
    property TabStop default True;
  end;

  TJvDockTabSheet = class(TJvWinControl)
  private
    FImageIndex: TImageIndex;
    FPageControl: TJvDockPageControl;
    FTabVisible: Boolean;
    FTabShowing: Boolean;
    FHighlighted: Boolean;
    FOnHide: TNotifyEvent;
    FOnShow: TNotifyEvent;
    function GetPageIndex: Integer;
    function GetTabIndex: Integer;
    procedure SetHighlighted(Value: Boolean);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetPageIndex(Value: Integer);
    procedure SetTabShowing(Value: Boolean);
    procedure SetTabVisible(Value: Boolean);
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    procedure CMShowingChanged(var Msg: TMessage); message CM_SHOWINGCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetPageControl(APageControl: TJvDockPageControl); virtual;
    procedure ReadState(Reader: TReader); override;
    procedure DoHide; dynamic;
    procedure DoShow; dynamic;
    procedure UpdateTabShowing; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property PageControl: TJvDockPageControl read FPageControl write SetPageControl;
    property TabIndex: Integer read GetTabIndex;
  published
    property Caption;
    property Height stored False;
    property Highlighted: Boolean read FHighlighted write SetHighlighted default False;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property Left stored False;
    property PageIndex: Integer read GetPageIndex write SetPageIndex stored False;
    property TabVisible: Boolean read FTabVisible write SetTabVisible default True;
    property Top stored False;
    property Visible stored False;
    property Width stored False;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
  end;

  TJvDockTabSheetClass = class of TJvDockTabSheet;

  TJvDockPageControl = class(TJvDockCustomTabControl)
  private
    FPages: TList;
    FActivePage: TJvDockTabSheet;
    FNewDockSheet: TJvDockTabSheet;
    FUndockingPage: TJvDockTabSheet;
    FTabSheetClass: TJvDockTabSheetClass;
    procedure ChangeActivePage(Page: TJvDockTabSheet);
    procedure DeleteTab(Page: TJvDockTabSheet; Index: Integer);
    function GetActivePageIndex: Integer;
    function GetPage(Index: Integer): TJvDockTabSheet;
    function GetCount: Integer;
    procedure InsertPage(Page: TJvDockTabSheet);
    procedure InsertTab(Page: TJvDockTabSheet);
    procedure MoveTab(CurIndex, NewIndex: Integer);
    procedure RemovePage(Page: TJvDockTabSheet);
    procedure SetActivePageIndex(const Value: Integer);
    procedure UpdateTab(Page: TJvDockTabSheet);
    procedure UpdateTabHighlights;
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMDialogKey(var Msg: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMDockClient(var Msg: TCMDockClient); message CM_DOCKCLIENT;
    procedure CMDockNotification(var Msg: TCMDockNotification); message CM_DOCKNOTIFICATION;
    procedure CMUnDockClient(var Msg: TCMUnDockClient); message CM_UNDOCKCLIENT;
    procedure WMLButtonDown(var Msg: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Msg: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMLButtonUp(var Msg: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMRButtonDown(var Msg: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMRButtonDblClk(var Msg: TWMRButtonDblClk); message WM_RBUTTONDBLCLK;
    procedure WMRButtonUp(var Msg: TWMRButtonUp); message WM_RBUTTONUP;
    procedure WMMButtonDown(var Msg: TWMMButtonDown); message WM_MBUTTONDOWN;
    procedure WMMButtonDblClk(var Msg: TWMMButtonDblClk); message WM_MBUTTONDBLCLK;
    procedure WMMButtonUp(var Msg: TWMMButtonUp); message WM_MBUTTONUP;
  protected
    function CanShowTab(TabIndex: Integer): Boolean; override;
    procedure Change; override;
    procedure DoAddDockClient(Client: TControl; const ARect: TRect); override;
    procedure DockOver(Source: TDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    function DoMouseEvent(var Msg: TWMMouse; Control: TControl): TWMNCHitMessage; virtual;
    procedure DoRemoveDockClient(Client: TControl); override;
    function GetDockClientFromMousePos(MousePos: TPoint): TControl; virtual;
    function GetImageIndex(TabIndex: Integer): Integer; override;
    function GetPageFromDockClient(Client: TControl): TJvDockTabSheet;
    procedure GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
      MousePos: TPoint; var CanDock: Boolean); override;
    procedure Loaded; override;
    procedure SetActivePage(Page: TJvDockTabSheet); virtual;
    procedure ShowControl(AControl: TControl); override;
    procedure UpdateActivePage; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;  // public in D2009
    function FindNextPage(CurPage: TJvDockTabSheet;
      GoForward, CheckTabVisible: Boolean): TJvDockTabSheet;
    procedure SelectNextPage(GoForward: Boolean; CheckTabVisible: Boolean = True);
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    property ActivePage: TJvDockTabSheet read FActivePage write SetActivePage;
    property ActivePageIndex: Integer read GetActivePageIndex
      write SetActivePageIndex;
    property Count: Integer read GetCount;
    property Pages[Index: Integer]: TJvDockTabSheet read GetPage;
    property PageSheets: TList read FPages;
    property TabSheetClass: TJvDockTabSheetClass read FTabSheetClass write FTabSheetClass;
  end;

  TJvDockDragOperation = (dopNone, dopDrag, dopDock);

  PSiteInfoRec = ^TSiteInfoRec;
  TSiteInfoRec = record
    Site: TWinControl;
    TopParent: THandle;
  end;

  TSiteList = class(TList)
  public
    procedure AddSite(ASite: TWinControl);
    procedure Clear; override;
    function Find(ParentWnd: THandle; var Index: Integer): Boolean;
    function GetTopSite: TWinControl;
  end;

  TJvDockManager = class(TObject)
  private
    FLoadCount: Integer;
    FSaveCount: Integer;
    FDragObject: TJvDockDragDockObject;
    FDragControl: TControl;
    FDragFreeObject: Boolean;
    FDragCapture: THandle;
    FDragStartPos: TPoint;
    FDragSaveCursor: HCURSOR;
    FDragThreshold: Integer;
    FActiveDrag: TJvDockDragOperation;
    FDragImageList: TDragImageList;
    FDockSiteList: TList;
    FQualifyingSites: TSiteList;
    procedure BeginLoad;
    procedure EndLoad;
    procedure BeginSave;
    procedure EndSave;
  public
    procedure CalcDockSizes(Control: TControl);
    constructor Create; virtual;
    destructor Destroy; override;
    function IsDockLoading: Boolean;
    function IsSaving: Boolean;
    procedure ShowDockForm(DockWindow: TWinControl);
    procedure HideDockForm(DockWindow: TWinControl);
    function GetFormVisible(DockWindow: TWinControl): Boolean;
    procedure SetTabDockHostBorderStyle(Value: TFormBorderStyle);
    procedure SetConjoinDockHostBorderStyle(Value: TFormBorderStyle);
    procedure SaveDockTreeToAppStorage(AppStorage: TJvCustomAppStorage; const AppStoragePath: string = '');
    procedure LoadDockTreeFromAppStorage(AppStorage: TJvCustomAppStorage; const AppStoragePath: string = '');
    procedure BeginDrag(Control: TControl; Immediate: Boolean; Threshold: Integer = -1); virtual;
    procedure DragInitControl(Control: TControl; Immediate: Boolean; Threshold: Integer); virtual;
    procedure DragInit(ADragObject: TJvDockDragDockObject; Immediate: Boolean; Threshold: Integer); virtual;
    procedure DragTo(const Pos: TPoint); virtual;
    procedure DragDone(Drop: Boolean); virtual;
    procedure CancelDrag; virtual;
    procedure ResetCursor; virtual;
    function DragFindTarget(const Pos: TPoint; var Handle: THandle;
      DragKind: TDragKind; Client: TControl): Pointer; virtual;
    procedure DoGetSiteInfo(Target, Client: TControl; var InfluenceRect: TRect;
      MousePos: TPoint; var CanDock: Boolean); virtual;
    function DoDockOver(DragState: TDragState): Boolean; virtual;
    procedure DoDockDrop(Source: TJvDockDragDockObject; Pos: TPoint); virtual;
    function DoUnDock(Source: TJvDockDragDockObject; Target: TWinControl; Client: TControl): Boolean; virtual;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); virtual;
    function DragFindWindow(const Pos: TPoint): THandle; virtual;
    function GetDockSiteAtPos(MousePos: TPoint; Client: TControl): TWinControl; virtual;
    procedure DoGetDockEdge(Target: TControl; MousePos: TPoint; var DropAlign: TAlign); virtual;
    procedure RegisterDockSite(Site: TWinControl; DoRegister: Boolean); virtual;
    property DragObject: TJvDockDragDockObject read FDragObject write FDragObject;
  end;

  TJvDockCustomPanelSplitter = class(TJvGraphicControl)
  private
    FActiveControl: TWinControl;
    FAutoSnap: Boolean;
    FBeveled: Boolean;
    FBrush: TBrush;
    FControl: TControl;
    FDownPos: TPoint;
    FLineDC: HDC;
    FLineVisible: Boolean;
    FMinSize: NaturalNumber;
    FMaxSize: Integer;
    FNewSize: Integer;
    FOldKeyDown: TKeyEvent;
    FOldSize: Integer;
    FPrevBrush: HBRUSH;
    FResizeStyle: TResizeStyle;
    FSplit: Integer;
    FOnCanResize: TCanResizeEvent;
    FOnMoved: TNotifyEvent;
    FOnPaint: TNotifyEvent;
    procedure AllocateLineDC;
    procedure CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
    procedure DrawLine;
    procedure FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ReleaseLineDC;
    procedure SetBeveled(Value: Boolean);
    procedure UpdateControlSize;
    procedure UpdateSize(X, Y: Integer);
  protected
    function CanResize(var NewSize: Integer): Boolean; reintroduce; virtual;
    function DoCanResize(var NewSize: Integer): Boolean; virtual;
    function FindControl: TControl; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure RequestAlign; override;
    procedure StopSizing; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
  published
    property Align default alLeft;
    property AutoSnap: Boolean read FAutoSnap write FAutoSnap default True;
    property Beveled: Boolean read FBeveled write SetBeveled default False;
    property Color;
    property Constraints;
    property MinSize: NaturalNumber read FMinSize write FMinSize default 30;
    property ParentColor;
    property ResizeStyle: TResizeStyle read FResizeStyle write FResizeStyle
      default rsPattern;
    property Visible;
    property OnCanResize: TCanResizeEvent read FOnCanResize write FOnCanResize;
    property OnMoved: TNotifyEvent read FOnMoved write FOnMoved;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

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
  Types, ComStrs, Consts, SysUtils,
  {$IFNDEF COMPILER12_UP}
  JvJCLUtils,
  {$ENDIF ~COMPILER12_UP}
  JvDockGlobals, JvDockControlForm, JvDockSupportProc, JvJVCLUtils;

type
  TlbNCButtonProc = procedure(Msg: TWMNCHitMessage; Button: TMouseButton;
    MouseStation: TJvDockMouseStation) of object;

  PCheckTargetInfo = ^TCheckTargetInfo;
  TCheckTargetInfo = record
    ClientWnd: THandle;
    TargetWnd: THandle;
    CurrentWnd: THandle;
    MousePos: TPoint;
    Found: Boolean;
  end;

  TControlAccessProtected = class(TControl);
  TWinControlAccessProtected = class(TWinControl);

//=== Local procedures =======================================================

function ButtonEvent(Page: TJvDockPageControl; Msg: TWMMouse;
  Button: TMouseButton; MouseStation: TJvDockMouseStation; Proc: TlbNCButtonProc): TControl;
begin
  Result := Page.GetDockClientFromMousePos(SmallPointToPoint(Msg.Pos));
  if (Result <> nil) and Assigned(Proc) then
  begin
    JvGlobalDockClient := FindDockClient(Result);
    Proc(Page.DoMouseEvent(Msg, Page), Button, MouseStation);
  end;
end;

procedure TabControlError(const S: string);
begin
  raise EListError.Create(S);
end;

procedure SetComCtlStyle(Ctl: TWinControl; Value: Integer; UseStyle: Boolean);
var
  Style: Integer;
begin
  if Ctl.HandleAllocated then
  begin
    Style := GetWindowLong(Ctl.Handle, GWL_STYLE);
    if not UseStyle then
      Style := Style and not Value
    else
      Style := Style or Value;
    SetWindowLong(Ctl.Handle, GWL_STYLE, Style);
  end;
end;

function IsBeforeTargetWindow(Window: HWND; Data: Longint): Bool; stdcall;
var
  R: TRect;
begin
  if Window = PCheckTargetInfo(Data)^.TargetWnd then
    Result := False
  else
  begin
    if PCheckTargetInfo(Data)^.CurrentWnd = 0 then
    begin
      GetWindowRect(Window, R);
      if PtInRect(R, PCheckTargetInfo(Data)^.MousePos) then
        PCheckTargetInfo(Data)^.CurrentWnd := Window;
    end;
    if Window = PCheckTargetInfo(Data)^.CurrentWnd then
    begin
      Result := False;
      PCheckTargetInfo(Data)^.Found := True;
    end
    else
    if Window = PCheckTargetInfo(Data)^.ClientWnd then
    begin
      Result := True;
      PCheckTargetInfo(Data)^.CurrentWnd := 0;
    end
    else
      Result := True;
  end;
end;

//=== { TJvDockCustomControl } ===============================================

procedure TJvDockCustomControl.CustomDockDrop(Source: TJvDockDragDockObject;
  X, Y: Integer);
var
  DestRect: TRect;
  Form: TCustomForm;
begin
  DestRect := Source.DockRect;
  MapWindowPoints(0, Handle, DestRect, 2);
  DisableAlign;
  try
    Source.Control.Dock(Self, DestRect);
    if UseDockManager and (DockManager <> nil) then
      DockManager.InsertControl(Source.Control,
        Source.DropAlign, Source.DropOnControl);
  finally
    EnableAlign;
  end;
  Form := GetParentForm(Self);
  if Form <> nil then
    Form.BringToFront;

  if Source.Control is TForm then
  begin
    TForm(Source.Control).ActiveControl := nil;
    SetDockSite(TForm(Source.Control), False);
  end;
end;

procedure TJvDockCustomControl.CustomDockOver(Source: TJvDockDragDockObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  CustomPositionDockRect(Source, X, Y);
end;

procedure TJvDockCustomControl.CustomEndDock(Target: TObject; X, Y: Integer);
begin
end;

procedure TJvDockCustomControl.CustomGetDockEdge(Source: TJvDockDragDockObject;
  MousePos: TPoint; var DropAlign: TAlign);
begin
  DropAlign := GetDockEdge(MousePos);
end;

procedure TJvDockCustomControl.CustomGetSiteInfo(Source: TJvDockDragDockObject;
  Client: TControl; var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  GetWindowRect(Handle, InfluenceRect);
  InflateRect(InfluenceRect, DefExpandoRect, DefExpandoRect);
end;

procedure TJvDockCustomControl.CustomPositionDockRect(Source: TJvDockDragDockObject;
  X, Y: Integer);
var
  NewWidth, NewHeight: Integer;
  TempX, TempY: Double;
  R: TRect;
begin
  with Source do
  begin
    if (DragTarget = nil) or (not TWinControlAccessProtected(DragTarget).UseDockManager) then
    begin
      NewWidth := Control.UndockWidth;
      NewHeight := Control.UndockHeight;
      TempX := DragPos.X - ((NewWidth) * MouseDeltaX);
      TempY := DragPos.Y - ((NewHeight) * MouseDeltaY);
      R := DockRect;
      R.Left := Round(TempX);
      R.Top := Round(TempY);
      R.Right := R.Left + NewWidth;
      R.Bottom := R.Top + NewHeight;
      DockRect := R;
      AdjustDockRect(DockRect);
    end
    else
    begin
      GetWindowRect(TargetControl.Handle, R);
      DockRect := R;
      if TWinControlAccessProtected(DragTarget).UseDockManager then
        if TargetControl is TJvDockCustomPanel then
          if TJvDockCustomPanel(DragTarget).JvDockManager <> nil then
          begin
            R := DockRect;
            TJvDockCustomPanel(DragTarget).JvDockManager.PositionDockRect(Control,
              DropOnControl, DropAlign, R);
            DockRect := R;
          end;
    end;
  end;
end;

procedure TJvDockCustomControl.CustomStartDock(var Source: TJvDockDragDockObject);
begin
end;

function TJvDockCustomControl.CustomUnDock(Source: TJvDockDragDockObject;
  NewTarget: TWinControl; Client: TControl): Boolean;
begin
  Result := (Perform(CM_UNDOCKCLIENT, WPARAM(NewTarget), LPARAM(Client)) = 0);
end;

function TJvDockCustomControl.GetJvDockManager: IJvDockManager;
begin
  Result := IJvDockManager(DockManager);
end;

procedure TJvDockCustomControl.UpdateCaption(Exclude: TControl);
var
  I: Integer;
  Host: TJvDockableForm;
begin
  if Parent is TJvDockableForm then
  begin
    Host := TJvDockableForm(Parent);
    Host.Caption := '';

    for I := 0 to Host.DockableControl.DockClientCount - 1 do
      if Host.DockableControl.DockClients[I].Visible and (Host.DockableControl.DockClients[I] <> Exclude) then
        Host.Caption := Host.Caption + TCustomForm(Host.DockableControl.DockClients[I]).Caption + RsDockStringSplitter;

    if Host.HostDockSite is TJvDockTabPageControl then
      with TJvDockTabPageControl(Host.HostDockSite) do
        if (ActivePage <> nil) and (ActivePage.Controls[0] = Self) then
          ActivePage.Caption := Host.Caption;
    if Host.HostDockSite is TJvDockCustomControl then
      TJvDockCustomControl(Host.HostDockSite).UpdateCaption(nil);
  end;
end;

procedure TJvDockCustomControl.WndProc(var Msg: TMessage);
var
  CMUnDockClient: TCMUnDockClient;
  DockableForm: TJvDockableForm;
begin
  if Msg.Msg = CM_UNDOCKCLIENT then
  begin
    CMUnDockClient := TCMUnDockClient(Msg);
    if CMUnDockClient.Client is TJvDockableForm then
    begin
      DockableForm := TJvDockableForm(CMUnDockClient.Client);
      if DockableForm.FloatingChild <> nil then
      begin
        if Self is TJvDockTabPageControl then
          DockableForm.FloatingChild.ManualDock(Self)
        else
        begin
          DisableAlign;
          try
            { using a null-rect as parameter for Dock causes align problems }
//            DockableForm.FloatingChild.Dock(Self, Rect(0, 0, 0, 0));
            DockableForm.FloatingChild.Dock(Self, Self.BoundsRect);
          finally
            EnableAlign;
          end;
        end;
        DockableForm.FloatingChild.Visible := True;
        if Self is TJvDockCustomPanel then
          JvDockManager.ReplaceZoneChild(DockableForm, DockableForm.FloatingChild);
      end;
    end;
  end;
  inherited WndProc(Msg);
end;

//=== { TJvDockCustomPanel } =================================================

constructor TJvDockCustomPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csOpaque, csDoubleClicks, csReplicatable];
  Color := clBtnFace;
  UseDockManager := True;
end;

destructor TJvDockCustomPanel.Destroy;
begin
  SetDockSite(Self, False);
  inherited Destroy;
end;

function TJvDockCustomPanel.CreateDockManager: IDockManager;
begin
  if (DockManager = nil) and DockSite and UseDockManager then
    Result := DefaultDockTreeClass.Create(Self, DefaultDockZoneClass, nil) as IJvDockManager
  else
    Result := DockManager;
  DoubleBuffered := DoubleBuffered or (Result <> nil);
end;

//=== { TJvDockCustomPanelSplitter } =========================================

constructor TJvDockCustomPanelSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoSnap := True;
  Align := alLeft;
  Width := 3;
  Cursor := crHSplit;
  FMinSize := 30;
  FResizeStyle := rsPattern;
  FOldSize := -1;
end;

destructor TJvDockCustomPanelSplitter.Destroy;
begin
  FBrush.Free;
  inherited Destroy;
end;

procedure TJvDockCustomPanelSplitter.AllocateLineDC;
begin
  FLineDC := GetDCEx(Parent.Handle, 0,
    DCX_CACHE or DCX_CLIPSIBLINGS or DCX_LOCKWINDOWUPDATE);
  if ResizeStyle = rsPattern then
  begin
    if FBrush = nil then
    begin
      FBrush := TBrush.Create;
      FBrush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
    end;
    FPrevBrush := SelectObject(FLineDC, FBrush.Handle);
  end;
end;

procedure TJvDockCustomPanelSplitter.CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
var
  S: Integer;
begin
  if Align in [alLeft, alRight] then
    Split := X - FDownPos.X
  else
    Split := Y - FDownPos.Y;
  S := 0;
  case Align of
    alLeft:
      S := FControl.Width + Split;
    alRight:
      S := FControl.Width - Split;
    alTop:
      S := FControl.Height + Split;
    alBottom:
      S := FControl.Height - Split;
  end;
  NewSize := S;
  if S < FMinSize then
    NewSize := FMinSize
  else
  if S > FMaxSize then
    NewSize := FMaxSize;
  if S <> NewSize then
  begin
    if Align in [alRight, alBottom] then
      S := S - NewSize
    else
      S := NewSize - S;
    Inc(Split, S);
  end;
end;

function TJvDockCustomPanelSplitter.CanResize(var NewSize: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnCanResize) then
    FOnCanResize(Self, NewSize, Result);
end;

function TJvDockCustomPanelSplitter.DoCanResize(var NewSize: Integer): Boolean;
begin
  Result := CanResize(NewSize);
  if Result and (NewSize <= MinSize) and FAutoSnap then
    NewSize := 0;
end;

procedure TJvDockCustomPanelSplitter.DrawLine;
var
  X, Y: Integer;
begin
  FLineVisible := not FLineVisible;
  X := Left;
  Y := Top;
  if Align in [alLeft, alRight] then
    X := Left + FSplit
  else
    Y := Top + FSplit;
  PatBlt(FLineDC, X, Y, Width, Height, PATINVERT);
end;

function TJvDockCustomPanelSplitter.FindControl: TControl;
var
  P: TPoint;
  I: Integer;
  R: TRect;
begin
  Result := nil;
  P := Point(Left, Top);
  case Align of
    alLeft:
      Dec(P.X);
    alRight:
      Inc(P.X, Width);
    alTop:
      Dec(P.Y);
    alBottom:
      Inc(P.Y, Height);
  else
    Exit;
  end;
  for I := 0 to Parent.ControlCount - 1 do
  begin
    Result := Parent.Controls[I];
    if Result.Visible and Result.Enabled then
    begin
      R := Result.BoundsRect;
      if (R.Right - R.Left) = 0 then
        if Align in [alTop, alLeft] then
          Dec(R.Left)
        else
          Inc(R.Right);
      if (R.Bottom - R.Top) = 0 then
        if Align in [alTop, alLeft] then
          Dec(R.Top)
        else
          Inc(R.Bottom);
      if PtInRect(R, P) then
        Exit;
    end;
  end;
  Result := nil;
end;

procedure TJvDockCustomPanelSplitter.FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    StopSizing
  else
  if Assigned(FOldKeyDown) then
    FOldKeyDown(Sender, Key, Shift);
end;

procedure TJvDockCustomPanelSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FControl := FindControl;
    FDownPos := Point(X, Y);
    if Assigned(FControl) then
    begin
      if Align in [alLeft, alRight] then
      begin
        FMaxSize := Parent.ClientWidth - FMinSize;
        for I := 0 to Parent.ControlCount - 1 do
          with Parent.Controls[I] do
            if Visible and (Align in [alLeft, alRight]) then
              Dec(FMaxSize, Width);
        Inc(FMaxSize, FControl.Width);
      end
      else
      begin
        FMaxSize := Parent.ClientHeight - FMinSize;
        for I := 0 to Parent.ControlCount - 1 do
          with Parent.Controls[I] do
            if Align in [alTop, alBottom] then
              Dec(FMaxSize, Height);
        Inc(FMaxSize, FControl.Height);
      end;
      UpdateSize(X, Y);
      AllocateLineDC;
      with ValidParentForm(Self) do
        if ActiveControl <> nil then
        begin
          FActiveControl := ActiveControl;
          FOldKeyDown := TWinControlAccessProtected(FActiveControl).OnKeyDown;
          TWinControlAccessProtected(FActiveControl).OnKeyDown := FocusKeyDown;
        end;
      if ResizeStyle in [rsLine, rsPattern] then
        DrawLine;
    end;
  end;
end;

procedure TJvDockCustomPanelSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewSize, Split: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  if (ssLeft in Shift) and Assigned(FControl) then
  begin
    CalcSplitSize(X, Y, NewSize, Split);
    if DoCanResize(NewSize) and (FNewSize <> NewSize) then
    begin
      if ResizeStyle in [rsLine, rsPattern] then
        DrawLine;
      FNewSize := NewSize;
      FSplit := Split;
      if ResizeStyle = rsUpdate then
        UpdateControlSize;
      if ResizeStyle in [rsLine, rsPattern] then
        DrawLine;
    end;
  end;
end;

procedure TJvDockCustomPanelSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Assigned(FControl) then
  begin
    if ResizeStyle in [rsLine, rsPattern] then
      DrawLine;
    UpdateControlSize;
    StopSizing;
  end;
end;

procedure TJvDockCustomPanelSplitter.Paint;
var
  FrameBrush: HBRUSH;
  R: TRect;
begin
  R := ClientRect;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
  if Beveled then
  begin
    if Align in [alLeft, alRight] then
      InflateRect(R, -1, 2)
    else
      InflateRect(R, 2, -1);
    OffsetRect(R, 1, 1);
    FrameBrush := CreateSolidBrush(ColorToRGB(clBtnHighlight));
    FrameRect(Canvas.Handle, R, FrameBrush);
    DeleteObject(FrameBrush);
    OffsetRect(R, -2, -2);
    FrameBrush := CreateSolidBrush(ColorToRGB(clBtnShadow));
    FrameRect(Canvas.Handle, R, FrameBrush);
    DeleteObject(FrameBrush);
  end;

  if csDesigning in ComponentState then
    with Canvas do
    begin
      Pen.Style := psDot;
      Pen.Mode := pmXor;
      Pen.Color := JvDockXorColor;
      Brush.Style := bsClear;
      Rectangle(0, 0, ClientWidth, ClientHeight);
    end;

  if Assigned(FOnPaint) then
    FOnPaint(Self);
end;

procedure TJvDockCustomPanelSplitter.ReleaseLineDC;
begin
  if FPrevBrush <> 0 then
    SelectObject(FLineDC, FPrevBrush);
  ReleaseDC(Parent.Handle, FLineDC);
  if FBrush <> nil then
  begin
    FBrush.Free;
    FBrush := nil;
  end;
end;

procedure TJvDockCustomPanelSplitter.RequestAlign;
begin
  inherited RequestAlign;
  if (Cursor <> crVSplit) and (Cursor <> crHSplit) then
    Exit;
  if Align in [alBottom, alTop] then
    Cursor := crVSplit
  else
    Cursor := crHSplit;
end;

procedure TJvDockCustomPanelSplitter.SetBeveled(Value: Boolean);
begin
  FBeveled := Value;
  Repaint;
end;

procedure TJvDockCustomPanelSplitter.StopSizing;
begin
  if Assigned(FControl) then
  begin
    if FLineVisible then
      DrawLine;
    FControl := nil;
    ReleaseLineDC;
    if Assigned(FActiveControl) then
    begin
      TWinControlAccessProtected(FActiveControl).OnKeyDown := FOldKeyDown;
      FActiveControl := nil;
    end;
  end;
  if Assigned(FOnMoved) then
    FOnMoved(Self);
end;

procedure TJvDockCustomPanelSplitter.UpdateControlSize;
begin
  if FNewSize <> FOldSize then
  begin
    case Align of
      alLeft:
        FControl.Width := FNewSize;
      alTop:
        FControl.Height := FNewSize;
      alRight:
        begin
          Parent.DisableAlign;
          try
            FControl.Left := FControl.Left + (FControl.Width - FNewSize);
            FControl.Width := FNewSize;
          finally
            Parent.EnableAlign;
          end;
        end;
      alBottom:
        begin
          Parent.DisableAlign;
          try
            FControl.Top := FControl.Top + (FControl.Height - FNewSize);
            FControl.Height := FNewSize;
          finally
            Parent.EnableAlign;
          end;
        end;
    end;
    TControlAccessProtected(FControl).Resize;
    Update;
    if Assigned(FOnMoved) then
      FOnMoved(Self);
    FOldSize := FNewSize;

  end;
end;

procedure TJvDockCustomPanelSplitter.UpdateSize(X, Y: Integer);
begin
  CalcSplitSize(X, Y, FNewSize, FSplit);
end;

//=== { TJvDockCustomTabControl } ============================================

constructor TJvDockCustomTabControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 289;
  Height := 193;
  TabStop := True;
  ControlStyle := [csAcceptsControls, csDoubleClicks];
  FTabs := TJvDockTabStrings.Create;
  FTabs.FTabControl := Self;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
end;

destructor TJvDockCustomTabControl.Destroy;
begin
  FreeAndNil(FTabs);
  FreeAndNil(FSaveTabs);
  FreeAndNil(FImageChangeLink);
  inherited Destroy;
end;

procedure TJvDockCustomTabControl.AdjustClientRect(var Rect: TRect);
begin
  Rect := DisplayRect;
  inherited AdjustClientRect(Rect);
end;

function TJvDockCustomTabControl.CanChange: Boolean;
begin
  Result := True;
  if Assigned(FOnChanging) then
    FOnChanging(Self, Result);
end;

function TJvDockCustomTabControl.CanShowTab(TabIndex: Integer): Boolean;
begin
  Result := True;
end;

procedure TJvDockCustomTabControl.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvDockCustomTabControl.CMDialogChar(var Msg: TCMDialogChar);
var
  I: Integer;
begin
  for I := 0 to FTabs.Count - 1 do
    if IsAccel(Msg.CharCode, FTabs[I]) and CanShowTab(I) and CanFocus then
    begin
      Msg.Result := 1;
      if CanChange then
      begin
        TabIndex := I;
        Change;
      end;
      Exit;
    end;
  inherited;
end;

procedure TJvDockCustomTabControl.CMFontChanged(var Msg);
begin
  inherited;
  if HandleAllocated then
    Perform(WM_SIZE, 0, 0);
end;

procedure TJvDockCustomTabControl.CMSysColorChange(var Msg: TMessage);
begin
  inherited;
  if not (csLoading in ComponentState) then
  begin
    Msg.Msg := WM_SYSCOLORCHANGE;
    DefaultHandler(Msg);
  end;
end;

procedure TJvDockCustomTabControl.CMTabStopChanged(var Msg: TMessage);
begin
  if not (csDesigning in ComponentState) then
    RecreateWnd;
end;

procedure TJvDockCustomTabControl.CNDrawItem(var Msg: TWMDrawItem);
var
  SaveIndex: Integer;
begin
  with Msg.DrawItemStruct^ do
  begin
    SaveIndex := SaveDC(hDC);
    Canvas.Lock;
    try
      Canvas.Handle := hDC;
      Canvas.Font := Font;
      Canvas.Brush := Brush;
      DrawTab(itemID, rcItem, itemState and ODS_SELECTED <> 0);
    finally
      Canvas.Handle := 0;
      Canvas.Unlock;
      RestoreDC(hDC, SaveIndex);
    end;
  end;
  Msg.Result := 1;
end;

procedure TJvDockCustomTabControl.CNNotify(var Msg: TWMNotify);
begin
  with Msg do
    case NMHdr^.code of
      TCN_SELCHANGE:
        Change;
      TCN_SELCHANGING:
        Result := Ord(not CanChange);
    end;
end;

procedure TJvDockCustomTabControl.CreateParams(var Params: TCreateParams);
const
  AlignStyles: array [Boolean, TTabPosition] of DWORD =
   ((0, TCS_BOTTOM, TCS_VERTICAL, TCS_VERTICAL or TCS_RIGHT),
    (0, TCS_BOTTOM, TCS_VERTICAL or TCS_RIGHT, TCS_VERTICAL));
  TabStyles: array [TTabStyle] of DWORD =
    (TCS_TABS, TCS_BUTTONS, TCS_BUTTONS or TCS_FLATBUTTONS);
  RRStyles: array [Boolean] of DWORD =
    (0, TCS_RAGGEDRIGHT);
begin
  InitCommonControl(ICC_TAB_CLASSES);
  inherited CreateParams(Params);
  CreateSubClass(Params, WC_TABCONTROL);
  with Params do
  begin
    Style := Style or WS_CLIPCHILDREN or
      AlignStyles[UseRightToLeftAlignment, FTabPosition] or
      TabStyles[FStyle] or RRStyles[FRaggedRight];
    if not TabStop then
      Style := Style or TCS_FOCUSNEVER;
    if FMultiLine then
      Style := Style or TCS_MULTILINE;
    if FMultiSelect then
      Style := Style or TCS_MULTISELECT;
    if FOwnerDraw then
      Style := Style or TCS_OWNERDRAWFIXED;
    if FTabSize.X <> 0 then
      Style := Style or TCS_FIXEDWIDTH;
    if FHotTrack and (not (csDesigning in ComponentState)) then
      Style := Style or TCS_HOTTRACK;
    if FScrollOpposite then
      Style := Style or TCS_SCROLLOPPOSITE;
    WindowClass.style := WindowClass.style and
      not (CS_HREDRAW or CS_VREDRAW) or CS_DBLCLKS;
  end;
end;

procedure TJvDockCustomTabControl.CreateWnd;
begin
  inherited CreateWnd;
  if (Images <> nil) and Images.HandleAllocated then
    Perform(TCM_SETIMAGELIST, 0, LPARAM(Images.Handle));
  if Integer(FTabSize) <> 0 then
    UpdateTabSize;
  if FSaveTabs <> nil then
  begin
    FTabs.Assign(FSaveTabs);
    SetTabIndex(FSaveTabIndex);
    FSaveTabs.Free;
    FSaveTabs := nil;
  end;
end;

procedure TJvDockCustomTabControl.DrawTab(TabIndex: Integer; const Rect: TRect;
  Active: Boolean);
begin
  if Assigned(FOnDrawTab) then
    FOnDrawTab(Self, TabIndex, Rect, Active)
  else
    Canvas.FillRect(Rect);
end;

function TJvDockCustomTabControl.GetDisplayRect: TRect;
begin
  Result := ClientRect;
  SendMessage(Handle, TCM_ADJUSTRECT, 0, LPARAM(@Result));
  if TabPosition = tpTop then
    Inc(Result.Top, 2);
end;

function TJvDockCustomTabControl.GetHitTestInfoAt(X, Y: Integer): THitTests;
var
  HitTest: TTCHitTestInfo;
begin
  Result := [];
  if PtInRect(ClientRect, Point(X, Y)) then
  begin
    HitTest.pt.X := X;
    HitTest.pt.Y := Y;
    if TabCtrl_HitTest(Handle, @HitTest) <> -1 then
    begin
      if (HitTest.flags and TCHT_NOWHERE) <> 0 then
        Include(Result, htNowhere);
      if (HitTest.flags and TCHT_ONITEM) = TCHT_ONITEM then
        Include(Result, htOnItem)
      else
      begin
        if (HitTest.flags and TCHT_ONITEM) <> 0 then
          Include(Result, htOnItem);
        if (HitTest.flags and TCHT_ONITEMICON) <> 0 then
          Include(Result, htOnIcon);
        if (HitTest.flags and TCHT_ONITEMLABEL) <> 0 then
          Include(Result, htOnLabel);
      end;
    end
    else
      Result := [htNowhere];
  end;
end;

function TJvDockCustomTabControl.GetImageIndex(TabIndex: Integer): Integer;
begin
  Result := TabIndex;
  if Assigned(FOnGetImageIndex) then
    FOnGetImageIndex(Self, TabIndex, Result);
end;

function TJvDockCustomTabControl.GetTabIndex: Integer;
begin
  Result := SendMessage(Handle, TCM_GETCURSEL, 0, 0);
end;

function TJvDockCustomTabControl.GetTabs: TStrings;
begin
  Result := FTabs;
end;

procedure TJvDockCustomTabControl.ImageListChange(Sender: TObject);
begin
  Perform(TCM_SETIMAGELIST, 0, LPARAM(TCustomImageList(Sender).Handle));
end;

function TJvDockCustomTabControl.IndexOfTabAt(X, Y: Integer): Integer;
var
  HitTest: TTCHitTestInfo;
begin
  Result := -1;
  if PtInRect(ClientRect, Point(X, Y)) then
  begin
    HitTest.pt.X := X;
    HitTest.pt.Y := Y;
    Result := TabCtrl_HitTest(Handle, @HitTest);
  end;
end;

function TJvDockCustomTabControl.InternalSetMultiLine(Value: Boolean): Boolean;
begin
  Result := FMultiLine <> Value;
  if Result then
  begin
    if not Value and ((TabPosition = tpLeft) or (TabPosition = tpRight)) then
      TabControlError(sTabMustBeMultiLine);
    FMultiLine := Value;
    if not Value then
      FScrollOpposite := False;
  end;
end;

procedure TJvDockCustomTabControl.Loaded;
begin
  inherited Loaded;
  if Images <> nil then
    UpdateTabImages;
end;

procedure TJvDockCustomTabControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;

procedure TJvDockCustomTabControl.PaintWindow(DC: HDC);
var
  Msg: TMessage;
begin
  if not OwnerDraw then
  begin
    Msg.Msg := WM_PAINT;
    Msg.WParam := DC;
    Msg.LParam := 0;
    Msg.Result := 0;
    DefaultHandler(Msg);
  end;
  inherited PaintWindow(DC);
end;

function TJvDockCustomTabControl.RowCount: Integer;
begin
  Result := TabCtrl_GetRowCount(Handle);
end;

procedure TJvDockCustomTabControl.ScrollTabs(Delta: Integer);
var
  Wnd: HWND;
  P: TPoint;
  Rect: TRect;
  I: Integer;
begin
  Wnd := FindWindowEx(Handle, 0, 'msctls_updown32', nil);
  if Wnd <> 0 then
  begin
    Windows.GetClientRect(Wnd, Rect);
    if Delta < 0 then
      P.X := Rect.Left + 2
    else
      P.X := Rect.Right - 2;
    P.Y := Rect.Top + 2;
    for I := 0 to Abs(Delta) - 1 do
    begin
      SendMessage(Wnd, WM_LBUTTONDOWN, 0, MakeLParam(P.X, P.Y));
      SendMessage(Wnd, WM_LBUTTONUP, 0, MakeLParam(P.X, P.Y));
    end;
  end;
end;

procedure TJvDockCustomTabControl.SetHotTrack(Value: Boolean);
begin
  if FHotTrack <> Value then
  begin
    FHotTrack := Value;
    RecreateWnd;
  end;
end;

procedure TJvDockCustomTabControl.SetImages(Value: TCustomImageList);
begin
  ReplaceImageListReference(Self, Value, FImages, FImageChangeLink);
  if Images <> nil then
    Perform(TCM_SETIMAGELIST, 0, LPARAM(Images.Handle))
  else
    Perform(TCM_SETIMAGELIST, 0, 0);
end;

procedure TJvDockCustomTabControl.SetMultiLine(Value: Boolean);
begin
  if InternalSetMultiLine(Value) then
    RecreateWnd;
end;

procedure TJvDockCustomTabControl.SetMultiSelect(Value: Boolean);
begin
  if FMultiSelect <> Value then
  begin
    FMultiSelect := Value;
    RecreateWnd;
  end;
end;

procedure TJvDockCustomTabControl.SetOwnerDraw(Value: Boolean);
begin
  if FOwnerDraw <> Value then
  begin
    FOwnerDraw := Value;
    RecreateWnd;
  end;
end;

procedure TJvDockCustomTabControl.SetRaggedRight(Value: Boolean);
begin
  if FRaggedRight <> Value then
  begin
    FRaggedRight := Value;
    SetComCtlStyle(Self, TCS_RAGGEDRIGHT, Value);
  end;
end;

procedure TJvDockCustomTabControl.SetScrollOpposite(Value: Boolean);
begin
  if FScrollOpposite <> Value then
  begin
    FScrollOpposite := Value;
    if Value then
      FMultiLine := Value;
    RecreateWnd;
  end;
end;

procedure TJvDockCustomTabControl.SetStyle(Value: TTabStyle);
begin
  if FStyle <> Value then
  begin
    if (Value <> tsTabs) and (TabPosition <> tpTop) then
      raise EInvalidOperation.CreateRes(@SInvalidTabStyle);
    FStyle := Value;
    RecreateWnd;
  end;
end;

procedure TJvDockCustomTabControl.SetTabHeight(Value: Smallint);
begin
  if FTabSize.Y <> Value then
  begin
    if Value < 0 then
      raise EInvalidOperation.CreateResFmt(@SPropertyOutOfRange, [Self.Classname]);
    FTabSize.Y := Value;
    UpdateTabSize;
  end;
end;

procedure TJvDockCustomTabControl.SetTabIndex(Value: Integer);
begin
  SendMessage(Handle, TCM_SETCURSEL, Value, 0);
end;

procedure TJvDockCustomTabControl.SetTabPosition(Value: TTabPosition);
begin
  if FTabPosition <> Value then
  begin
    if (Value <> tpTop) and (Style <> tsTabs) then
      raise EInvalidOperation.CreateRes(@SInvalidTabPosition);
    FTabPosition := Value;
    if not MultiLine and ((Value = tpLeft) or (Value = tpRight)) then
      InternalSetMultiLine(True);
    RecreateWnd;
  end;
end;

procedure TJvDockCustomTabControl.SetTabs(Value: TStrings);
begin
  FTabs.Assign(Value);
end;

procedure TJvDockCustomTabControl.SetTabWidth(Value: Smallint);
var
  OldValue: Smallint;
begin
  if FTabSize.X <> Value then
  begin
    if Value < 0 then
      raise EInvalidOperation.CreateResFmt(@SPropertyOutOfRange, [Self.ClassName]);
    OldValue := FTabSize.X;
    FTabSize.X := Value;
    if (OldValue = 0) or (Value = 0) then
      RecreateWnd
    else
      UpdateTabSize;
  end;
end;

function TJvDockCustomTabControl.TabRect(Index: Integer): TRect;
begin
  TabCtrl_GetItemRect(Handle, Index, Result);
end;

procedure TJvDockCustomTabControl.TabsChanged;
begin
  if not FUpdating then
  begin
    if HandleAllocated then
      SendMessage(Handle, WM_SIZE, SIZE_RESTORED, MakeLong(Word(Width), Word(Height)));
    Realign;
  end;
end;

procedure TJvDockCustomTabControl.TCMAdjustRect(var Msg: TMessage);
begin
  try
    inherited;
    if (TabPosition <> tpTop) and (Msg.WParam = 0) then
      FSavedAdjustRect := PRect(Msg.LParam)^;
  except
    PRect(Msg.LParam)^ := FSavedAdjustRect;
  end;
end;

procedure TJvDockCustomTabControl.UpdateTabImages;
var
  I: Integer;
  TCItem: TTCItem;
begin
  TCItem.mask := TCIF_IMAGE;
  for I := 0 to FTabs.Count - 1 do
  begin
    TCItem.iImage := GetImageIndex(I);
    if SendMessage(Handle, TCM_SETITEM, I, LPARAM(@TCItem)) = 0 then
      TabControlError(Format(sTabFailSet, [FTabs[I], I]));
  end;
  TabsChanged;
end;

procedure TJvDockCustomTabControl.UpdateTabSize;
begin
  SendMessage(Handle, TCM_SETITEMSIZE, 0, {$IFDEF RTL230_UP}PointToLParam{$ELSE}LPARAM{$ENDIF RTL230_UP}(FTabSize));
  TabsChanged;
end;

procedure TJvDockCustomTabControl.WMDestroy(var Msg: TWMDestroy);
var
  FocusHandle: HWND;
begin
  if (FTabs <> nil) and (FTabs.Count > 0) then
  begin
    FSaveTabs := TStringList.Create;
    FSaveTabs.Assign(FTabs);
    FSaveTabIndex := GetTabIndex;
  end;
  FocusHandle := GetFocus;
  if (FocusHandle <> 0) and ((FocusHandle = Handle) or
    IsChild(Handle, FocusHandle)) then
    Windows.SetFocus(0);
  inherited;
  WindowHandle := 0;
end;

procedure TJvDockCustomTabControl.WMNotifyFormat(var Msg: TMessage);
begin
  with Msg do
    Result := DefWindowProc(Handle, Msg, WParam, LParam);
end;

procedure TJvDockCustomTabControl.WMSize(var Msg: TMessage);
begin
  inherited;
  RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_ERASE);
end;

//=== { TJvAlphaBlendedForm } ==============================================

procedure TJvAlphaBlendedForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
end;

//=== { TJvDockDragDockObject } ==============================================

constructor TJvDockDragDockObject.Create(AControl: TControl);
begin
  inherited Create;
  FControl := AControl;
  FBrush := TBrush.Create;
  FBrush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
  FFrameWidth := 4;
  FCtrlDown := False;

  FAlphaBlendedForm := GetAlphaBlendedTab; { create the form ... }
  FAlphaBlendedTab := nil; { ... but use it for the form and not for the tab }
end;

destructor TJvDockDragDockObject.Destroy;
begin
  if FBrush <> nil then
  begin
    FBrush.Free;
    FBrush := nil;
  end;
  FAlphaBlendedForm.Free;
  FAlphaBlendedTab.Free;
  inherited Destroy;
end;

function TJvDockDragDockObject.GetAlphaBlendedTab: TJvAlphaBlendedForm;
begin
  if FAlphaBlendedTab = nil then
  begin
    FAlphaBlendedTab := TJvAlphaBlendedForm.CreateNew(nil);
    FAlphaBlendedTab.Visible := False;
    FAlphaBlendedTab.Color := clHighlight;
    FAlphaBlendedTab.AlphaBlend := True;
    FAlphaBlendedTab.AlphaBlendValue := 140;
    FAlphaBlendedTab.BorderIcons := [];
    FAlphaBlendedTab.BorderStyle := bsNone;
    FAlphaBlendedTab.FormStyle := fsStayOnTop;
    FAlphaBlendedTab.BoundsRect := Rect(0, 0, 0, 0);
  end;
  Result := FAlphaBlendedTab;
end;

procedure TJvDockDragDockObject.AdjustDockRect(const ARect: TRect);
var
  DeltaX, DeltaY: Integer;
  R: TRect;

  function AbsMin(Value1, Value2: Integer): Integer;
  begin
    if Abs(Value1) < Abs(Value2) then
      Result := Value1
    else
      Result := Value2;
  end;

begin
  if (ARect.Left > FDragPos.X) or (ARect.Right < FDragPos.X) then
    DeltaX := AbsMin(ARect.Left - FDragPos.X, ARect.Right - FDragPos.X)
  else
    DeltaX := 0;
  if (ARect.Top > FDragPos.Y) or (ARect.Bottom < FDragPos.Y) then
    DeltaY := AbsMin(ARect.Top - FDragPos.Y, ARect.Bottom - FDragPos.Y)
  else
    DeltaY := 0;
  if (DeltaX <> 0) or (DeltaY <> 0) then
  begin
    R := DockRect;
    OffsetRect(R, -DeltaX, -DeltaY);
    DockRect := R;
  end;
end;

function TJvDockDragDockObject.CanLeave(NewTarget: TWinControl): Boolean;
begin
  Result := (NewTarget <> TWinControl(FDragTarget));
end;

function TJvDockDragDockObject.Capture: THandle;
begin
  Result := AllocateHWnd(MouseMsg);
  SetCapture(Result);
end;

procedure TJvDockDragDockObject.DefaultDockImage(Erase: Boolean);
Var
  DrawRect: TRect;
  PenSize: Integer;
  ABrush: TBrush;
begin
  GetBrush_PenSize_DrawRect(ABrush, PenSize, DrawRect, Erase);
  AlphaBlendedForm.Visible := True;
  AlphaBlendedForm.BoundsRect := DrawRect;
end;

function TJvDockDragDockObject.DragFindWindow(const Pos: TPoint): THandle;
var
  WinControl: TWinControl;
begin
  WinControl := FindVCLWindow(Pos);
  if WinControl <> nil then
    Result := WinControl.Handle
  else
    Result := 0;
end;

procedure TJvDockDragDockObject.DrawDragDockImage;
begin
  DefaultDockImage(False);
end;

procedure TJvDockDragDockObject.DrawDragRect(DoErase: Boolean);
begin
  if not CompareMem(@DockRect, @EraseDockRect, SizeOf(TRect)) then
  begin
    if DoErase then
      EraseDragDockImage;
    DrawDragDockImage;
    FEraseDockRect := DockRect;
  end;
end;

procedure TJvDockDragDockObject.EndDrag(Target: TObject; X, Y: Integer);
begin
  JvGlobalDockManager.DoEndDrag(Target, X, Y);
end;

procedure TJvDockDragDockObject.EraseDragDockImage;
begin
  DefaultDockImage(True);
end;

procedure TJvDockDragDockObject.Finished(Target: TObject; X, Y: Integer;
  Accepted: Boolean);
begin
  if not Accepted then
    Target := nil;
  EndDrag(Target, X, Y);
end;

procedure TJvDockDragDockObject.GetBrush_PenSize_DrawRect(var ABrush: TBrush;
  var PenSize: Integer; var DrawRect: TRect; Erase: Boolean);
begin
  ABrush := Brush;
  PenSize := FrameWidth;
  if Erase then
    DrawRect := EraseDockRect
  else
    DrawRect := DockRect;
end;

function TJvDockDragDockObject.GetDragCursor(Accepted: Boolean;
  X, Y: Integer): TCursor;
begin
  Result := crDefault;
end;

function TJvDockDragDockObject.GetDragImages: TDragImageList;
begin
  Result := nil;
end;

function TJvDockDragDockObject.GetDropCtl: TControl;
var
  NextCtl: TControl;
  TargetCtl: TWinControl;
  CtlIdx: Integer;

  function GetDockClientsIndex: Integer;
  begin
    for Result := 0 to TWinControlAccessProtected(TargetCtl).DockClientCount - 1 do
      if TWinControlAccessProtected(TargetCtl).DockClients[Result] = NextCtl then
        Exit;
    Result := -1;
  end;

begin
  Result := nil;
  TargetCtl := DragTarget;
  if (TargetCtl = nil) or not TWinControlAccessProtected(TargetCtl).UseDockManager or
    (TargetCtl.DockClientCount = 0) or
    ((TargetCtl.DockClientCount = 1) and
    (TWinControlAccessProtected(TargetCtl).DockClients[0] = Control)) then
    Exit;
  NextCtl := FindDragTarget(DragPos, False);
  while (NextCtl <> nil) and (NextCtl <> TargetCtl) do
  begin
    CtlIdx := GetDockClientsIndex;
    if CtlIdx <> -1 then
    begin
      Result := TargetCtl.DockClients[CtlIdx];
      Exit;
    end
    else
      NextCtl := NextCtl.Parent;
  end;
end;

function TJvDockDragDockObject.GetFrameWidth: Integer;
begin
  Result := FFrameWidth;
end;

function TJvDockDragDockObject.GetTargetControl: TWinControl;
begin
  if FDragTarget <> nil then
    Result := TWinControl(FDragTarget)
  else
    Result := nil;
end;

procedure TJvDockDragDockObject.MouseMsg(var Msg: TMessage);
var
  P: TPoint;

  procedure DoDragDone(DropFlag: Boolean); {NEW! Warren added.}
  var
    DS: TJvDockServer;
    DC: TJvDockClient;
    DP: TJvDockPanel;
    DF: TForm;
  begin
    if not Assigned(JvGlobalDockManager) then
      Exit;

    if DropFlag and Assigned(FControl) then
    begin
      // only do this if DropFlag is true and there is a control (usually a form) we are dragging
      if not Assigned(TargetControl) then
      begin
        {$IFDEF JVDOCK_DEBUG}
        OutputDebugString('TJvDockDragDockObject.MouseMsg.DoDragDone: User drag finished, TargetControl=nil, user made form floating.');
        {$ENDIF JVDOCK_DEBUG}

        {In this case, we're dragging something off and making it floating. }
          {if Assigned(FControl) then
            DC := FindDockClient(FControl)
          else
             DC := nil;

          DP := nil;
          DS := nil;
          DF := nil;
          if Assigned(DC) then begin
            if Assigned(DC.OnCheckIsDockable) then begin
                DC.OnCheckIsDockable( DC, DF, DS, DP, DropFlag );
            end;
          end;}
      end
      else
      if TargetControl is TJvDockPanel then
      begin
        { In this case, we're about to dock to a TJvDockPanel }
          {DP := TargetControl as TJvDockPanel;
          DS := DP.DockServer;
          DC := FindDockClient(FControl);
          if FControl is TForm then
            DF := FControl as TForm
          else
            DF := nil;
          if Assigned(DC.OnCheckIsDockable) then begin
              DC.OnCheckIsDockable( DC, DF, DS, DP, DropFlag );
          end;}
      end
      else
      if TargetControl is TForm then
      begin
        { This appears to have something to do with conjoined and tabbed host forms }
        DC := FindDockClient(TargetControl);
        DP := nil;
        DS := nil;
        if FControl is TForm then
          DF := FControl as TForm
        else
          DF := nil;
        if Assigned(DC.OnCheckIsDockable) then
          DC.OnCheckIsDockable(DC, DF, DS, DP, DropFlag);
      end
      else
      begin
        {$IFDEF JVDOCK_DEBUG}
        // Debug message!
        OutputDebugString('TJvDockDragDockObject.MouseMsg.DoDragDone: TargetControl is not an expected type!');
        {$ENDIF JVDOCK_DEBUG}
      end;
    end;
    Assert(Assigned(JvGlobalDockManager));
    Assert(Assigned(JvGlobalDockClient));
    JvGlobalDockManager.DragDone(DropFlag);
    {$IFDEF JVDOCK_DEBUG}
    OutputDebugString('DoDragDone completed.');
    {$ENDIF JVDOCK_DEBUG}
  end;

begin
  try
    case Msg.Msg of
      WM_MOUSEMOVE:
        begin
          P := SmallPointToPoint(TWMMouse(Msg).Pos);
          ClientToScreen(JvGlobalDockManager.FDragCapture, P);
          JvGlobalDockManager.DragTo(P);
        end;
      WM_CAPTURECHANGED:
        DoDragDone(False); //JvGlobalDockManager.DragDone(False);
      WM_LBUTTONUP, WM_RBUTTONUP:
        if not JvGlobalDockClient.CanFloat then
        begin
          if (TargetControl = nil) and (JvGlobalDockClient.ParentForm.HostDockSite = nil) then
            DoDragDone(True) //JvGlobalDockManager.DragDone(True)
          else
            DoDragDone(TargetControl <> nil); //JvGlobalDockManager.DragDone(TargetControl <> nil);
        end
        else
          DoDragDone(True); //JvGlobalDockManager.DragDone(True);
      CN_KEYUP:
        if Msg.WParam = VK_CONTROL then
        begin
          FCtrlDown := False;
          JvGlobalDockManager.DragTo(JvGlobalDockManager.DragObject.DragPos);
        end;
      CN_KEYDOWN:
        case Msg.WParam of
          VK_CONTROL:
            begin
              FCtrlDown := True;
              JvGlobalDockManager.DragTo(JvGlobalDockManager.DragObject.DragPos);
            end;
          VK_ESCAPE:
            begin
              Msg.Result := 1;
              DoDragDone(False); //JvGlobalDockManager.DragDone(False);
            end;
        end;
    end;
  except
    if JvGlobalDockManager.FDragControl <> nil then
      DoDragDone(False); //JvGlobalDockManager.DragDone(False);
    raise;
  end;
end;

procedure TJvDockDragDockObject.ReleaseCapture(Handle: THandle);
begin
  Windows.ReleaseCapture;
  DeallocateHWnd(Handle);
end;

procedure TJvDockDragDockObject.SetBrush(const Value: TBrush);
begin
  FBrush.Assign(Value);
end;

procedure TJvDockDragDockObject.SetDropAlign(const Value: TAlign);
begin
  if FDropAlign <> Value then
    FDropAlign := Value;
end;

procedure TJvDockDragDockObject.SetDropOnControl(const Value: TControl);
begin
  FDropOnControl := Value;
end;

procedure TJvDockDragDockObject.SetFrameWidth(const Value: Integer);
begin
  FFrameWidth := Value;
end;

procedure TJvDockDragDockObject.SetTargetControl(const Value: TWinControl);
begin
  FDragTarget := Value;
end;

//=== { TJvDockManager } =====================================================

constructor TJvDockManager.Create;
begin
  inherited Create;
  FDockSiteList := TList.Create;
end;

destructor TJvDockManager.Destroy;
begin
  FDockSiteList.Free;
  inherited Destroy;
end;

procedure TJvDockManager.BeginDrag(Control: TControl; Immediate: Boolean; Threshold: Integer);
var
  P: TPoint;
begin
  if TControlAccessProtected(Control).DragKind <> dkDock then
    Exit;

  CalcDockSizes(Control);
  if (FDragControl = nil) or (FDragControl = Pointer($FFFFFFFF)) then
  begin
    FDragControl := nil;
    if csLButtonDown in Control.ControlState then
    begin
      GetCursorPos(P);
      P := Control.ScreenToClient(P);
      Control.Perform(WM_LBUTTONUP, 0, {$IFDEF RTL230_UP}PointToLParam{$ELSE}LPARAM{$ENDIF RTL230_UP}(PointToSmallPoint(P)));
    end;

    if Threshold < 0 then
      Threshold := Mouse.DragThreshold;

    if FDragControl <> Pointer($FFFFFFFF) then
      DragInitControl(Control, Immediate, Threshold);
  end;
end;

procedure TJvDockManager.BeginLoad;
begin
  Inc(FLoadCount);
  if FLoadCount = 1 then
  begin
  end;
end;

procedure TJvDockManager.BeginSave;
begin
  Inc(FSaveCount);
end;

procedure TJvDockManager.CalcDockSizes(Control: TControl);
var
  Rect: TRect;
begin
  with Control do
    if Floating then
    begin
      UndockHeight := Height;
      UndockWidth := Width;
    end
    else
    if HostDockSite is TJvDockCustomPanel then
    begin
      Rect := TJvDockCustomPanel(HostDockSite).JvDockManager.GetFrameRect(Control);
      if HostDockSite.Align in [alTop, alBottom] then
        TBDockHeight := Rect.Bottom - Rect.Top
      else
      if HostDockSite.Align in [alLeft, alRight] then
        LRDockWidth := Rect.Right - Rect.Left;
    end;
end;

procedure TJvDockManager.CancelDrag;
begin
  if DragObject <> nil then
    DragDone(False);
  FDragControl := nil;
end;

procedure TJvDockManager.DoDockDrop(Source: TJvDockDragDockObject; Pos: TPoint);
var
  Target: TWinControl;
  ADockClient: TJvDockClient;
  Pt: TPoint;
begin
  if Source.DragTarget <> nil then
  begin
    Target := Source.TargetControl;
    Pt := Target.ScreenToClient(Pos);
    if Target is TJvDockCustomControl then
      TJvDockCustomControl(Target).CustomDockDrop(Source, Pt.X, Pt.Y)
    else
    if Target is TForm then
    begin
      ADockClient := FindDockClient(Target);
      if ADockClient <> nil then
        ADockClient.FormDockDrop(Source, Pt.X, Pt.Y);
    end;
  end;
end;

function TJvDockManager.DoDockOver(DragState: TDragState): Boolean;
var
  Target: TControl;
  ADockClient: TJvDockClient;
  Pt: TPoint;
begin
  Result := True;
  if DragObject.DragTarget <> nil then
  begin
    Target := TControl(DragObject.DragTarget);
    Pt := Target.ScreenToClient(DragObject.DragPos);
    if Target is TJvDockCustomControl then
      TJvDockCustomControl(Target).CustomDockOver(DragObject, Pt.X, Pt.Y, DragState, Result)
    else
    if Target is TForm then
    begin
      ADockClient := FindDockClient(Target);
      if ADockClient <> nil then
        ADockClient.FormDockOver(DragObject, Pt.X, Pt.Y, DragState, Result);
    end;
  end;
end;

procedure TJvDockManager.DoEndDrag(Target: TObject; X, Y: Integer);
var
  ADockClient: TJvDockClient;
begin
  if Target is TJvDockCustomControl then
    TJvDockCustomControl(Target).CustomEndDock(Target, X, Y)
  else
  if Target is TForm then
  begin
    ADockClient := FindDockClient(TControl(Target));
    if ADockClient <> nil then
      ADockClient.FormEndDock(Target, X, Y);
  end;
end;

procedure TJvDockManager.DoGetDockEdge(Target: TControl; MousePos: TPoint; var DropAlign: TAlign);
var
  ADockClient: TJvDockClient;
begin
  if Target is TJvDockCustomControl then
    TJvDockCustomControl(Target).CustomGetDockEdge(DragObject, MousePos, DropAlign)
  else
  if Target is TForm then
  begin
    ADockClient := FindDockClient(Target);
    if ADockClient <> nil then
      ADockClient.FormGetDockEdge(DragObject, MousePos, DropAlign);
  end;
end;

procedure TJvDockManager.DoGetSiteInfo(Target, Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
var
  ADockClient: TJvDockClient;
begin
  if Target is TJvDockCustomControl then
    TJvDockCustomControl(Target).CustomGetSiteInfo(DragObject, Client, InfluenceRect, MousePos, CanDock)
  else
  if Target is TForm then
  begin
    ADockClient := FindDockClient(Target);
    if ADockClient <> nil then
      ADockClient.FormGetSiteInfo(DragObject, Client, InfluenceRect, MousePos, CanDock);
  end
  else
    CanDock := False;
end;

function TJvDockManager.DoUnDock(Source: TJvDockDragDockObject; Target: TWinControl; Client: TControl): Boolean;
var
  allow:Boolean;
begin
  if not (csDestroying in Client.ComponentState) then
  if Client is TForm then begin
      if Client is TForm then begin
        allow := true;
        if Assigned(TForm(Client).OnUnDock) then
          TForm(Client).OnUnDock(Self,Client,TWinControl(nil),allow);
         if not allow then begin
               result := false;
               exit;
         end;

      end;

  end;



  if Client.HostDockSite is TJvDockCustomControl then
    Result := TJvDockCustomControl(Client.HostDockSite).CustomUnDock(Source, Target, Client)
  else
    Result := False;
end;

procedure TJvDockManager.DragDone(Drop: Boolean);
var
  DragSave: TJvDockDragDockObject;
  DockObject: TJvDockDragDockObject;
  Accepted: Boolean;
  TargetPos: TPoint;
  ParentForm: TCustomForm;

  function CheckUndock: Boolean;
  begin
    Result := DragObject.DragTarget <> nil;
    with FDragControl do
      if Drop and (FActiveDrag = dopDock) then
        if Floating or (HostDockSite = nil) then
          Result := True
        else
          Result := DoUnDock(DragObject, DragObject.DragTarget, FDragControl);
  end;

  procedure DoFloatForm(Control: TControl);
  var
    WasVisible: Boolean;
  begin
    if Control.FloatingDockSiteClass = Control.ClassType then
    begin
      WasVisible := Control.Visible;
      try
        if Assigned(DragObject.AlphaBlendedForm) then
        DragObject.AlphaBlendedForm.Hide;
        Control.Dock(nil, DragObject.DockRect);
        if (Control.Left <> DragObject.DockRect.Left) or (Control.Top <> DragObject.DockRect.Top) then
        begin
          Control.Left := DragObject.DockRect.Left;
          Control.Top := DragObject.DockRect.Top;
        end;
      finally
        if WasVisible then
          Control.BringToFront;
      end;
    end;
  end;

begin
  DockObject := nil;
  DragSave := nil;
  Accepted := False;
  if (DragObject = nil) or DragObject.Cancelling then
    Exit;
  try
    DragSave := DragObject;
    try
      DragObject.Cancelling := True;
      DragObject.ReleaseCapture(FDragCapture);
      if FActiveDrag = dopDock then
      begin
        DockObject := DragObject;
        DockObject.EraseDragDockImage;
        DockObject.Floating := DockObject.DragTarget = nil;
      end;
      if (DragObject.DragTarget <> nil) and
        (TObject(DragObject.DragTarget) is TControl) then
        TargetPos := DragObject.DragTargetPos
      else
        TargetPos := DragObject.DragPos;

      {Check before we undock, then check if the drop is going to be accepted }

      Accepted := {local function:} CheckUndock and {DragDone parameter:} Drop;

      if FActiveDrag = dopDock then
      begin
        if Accepted and DockObject.Floating then
        begin
          ParentForm := GetParentForm(DockObject.Control);
          if (ParentForm <> nil) and
            (ParentForm.ActiveControl = DockObject.Control) then
            ParentForm.ActiveControl := nil;
          DoFloatForm(FDragControl);
        end;
      end
      else
      begin
        if FDragImageList <> nil then
          FDragImageList.EndDrag
        else
          Windows.SetCursor(FDragSaveCursor);
      end;
      FDragControl := nil;
      if DragSave.DragTarget <> nil then
      begin
        if not Accepted then
        begin
          DragSave.DragPos := Point(0, 0);
          TargetPos.X := 0;
          TargetPos.Y := 0;
        end
        else
          DoDockDrop(DragSave, DragSave.DragPos);
      end;
      DragObject := nil;
    finally
      FQualifyingSites.Free;
      FQualifyingSites := nil;
      DragSave.Cancelling := False;
      DragSave.Finished(DragSave.DragTarget, TargetPos.X, TargetPos.Y, Accepted);
      DragObject := nil;
    end;
  finally
    FDragControl := nil;
    DragSave.Free;
  end;
end;

function TJvDockManager.DragFindTarget(const Pos: TPoint; var Handle: THandle;
  DragKind: TDragKind; Client: TControl): Pointer;
begin
  Result := GetDockSiteAtPos(Pos, Client);
  if Result <> nil then
    Handle := TWinControl(Result).Handle;
end;

function TJvDockManager.DragFindWindow(const Pos: TPoint): THandle;
begin
  Result := DragObject.DragFindWindow(Pos);
end;

procedure TJvDockManager.DragInit(ADragObject: TJvDockDragDockObject;
  Immediate: Boolean; Threshold: Integer);
var
  R: TRect;
begin
  DragObject := ADragObject;
  DragObject.DragTarget := nil;
  GetCursorPos(FDragStartPos);
  DragObject.DragPos := FDragStartPos;
  FDragSaveCursor := Windows.GetCursor;
  FDragCapture := DragObject.Capture;
  FDragThreshold := Threshold;
  with ADragObject do
  begin
    R := DockRect;
    if R.Right - R.Left > 0 then
      MouseDeltaX := (DragPos.X - R.Left) / (R.Right - R.Left)
    else
      MouseDeltaX := 0;
    if R.Bottom - R.Top > 0 then
      MouseDeltaY := (DragPos.Y - R.Top) / (R.Bottom - R.Top)
    else
      MouseDeltaY := 0;
    if Immediate then
    begin
      FActiveDrag := dopDock;
      DrawDragDockImage;
    end
    else
      FActiveDrag := dopNone;
  end;
  FDragImageList := DragObject.GetDragImages;
  if FDragImageList <> nil then
    FDragImageList.BeginDrag(GetDesktopWindow, FDragStartPos.X, FDragStartPos.Y);
  FQualifyingSites := TSiteList.Create;
  if FActiveDrag <> dopNone then
    DragTo(FDragStartPos);
end;

procedure TJvDockManager.DragInitControl(Control: TControl;
  Immediate: Boolean; Threshold: Integer);
var
  ARect: TRect;
  DragObj: TJvDockDragDockObject;

  procedure DoStartDock;
  begin
    if Assigned(JvGlobalDockClient) then
    begin
      DragObj := DragObject;
      JvGlobalDockClient.FormStartDock(DragObj);
      DragObject := DragObj;
    end;
    if DragObject = nil then
    begin
      DragObject := TJvDockDragDockObject.Create(Control);
      FDragFreeObject := True;
    end;
  end;

begin
  FDragControl := Control;
  try
    DragObject := nil;
    FDragFreeObject := False;

    DoStartDock;
    if FDragControl = nil then
      Exit;
    with DragObject do
    begin
      if Control.HostDockSite is TJvDockCustomPanel then
        ARect := TJvDockCustomPanel(Control.HostDockSite).JvDockManager.GetFrameRectEx(Control)
      else
        GetWindowRect(TWinControl(Control).Handle, ARect);
      DockRect := ARect;
      FEraseDockRect := DockRect;
    end;
    DragInit(DragObject, Immediate, Threshold);
  except
    FDragControl := nil;
    raise;
  end;
end;

{ TJvDockManager.DragTo: WM_MOUSEMOVE Mouse Drag handler.

  The global Dock manager which is of this type, TJvDockManager, handles
  the WM_MOUSEMOVE messages sent to TJvDockDragDockObject.MouseMsg.

  In this function we decide what destination (drop object)
  we are in by calling DragFindTarget.

  There is a lot of boilerplate code here that isn't used, such
  as the ability to draw a drag image (not useful when dragging forms).

}
procedure TJvDockManager.DragTo(const Pos: TPoint);
var
  DragCursor: TCursor;
  Target: TControl;
  TargetHandle: THandle;
  DoErase: Boolean;
  TempAlign: TAlign;
begin
  if (Abs(FDragStartPos.X - Pos.X) >= FDragThreshold) or
    (Abs(FDragStartPos.Y - Pos.Y) >= FDragThreshold) then
  begin
    Target := DragFindTarget(Pos, TargetHandle, TControlAccessProtected(FDragControl).DragKind, FDragControl);
    if (FActiveDrag = dopNone) and (FDragImageList <> nil) then
      FDragImageList.BeginDrag(GetDesktopWindow, FDragStartPos.X, FDragStartPos.Y);
    DoErase := FActiveDrag <> dopNone;
    FActiveDrag := dopDock;

    if DragObject.CanLeave(TWinControl(Target)) then
    begin
      DoDockOver(dsDragLeave);
      if DragObject = nil then
        Exit;
      DragObject.DragTarget := Target;
      DragObject.DragHandle := TargetHandle;
      DragObject.DragPos := Pos;
      DoDockOver(dsDragEnter);
      if DragObject = nil then
        Exit;
    end;
    DragObject.DragPos := Pos;
    if DragObject.DragTarget <> nil then
      DragObject.DragTargetPos := TControl(DragObject.DragTarget).ScreenToClient(Pos);
    DragCursor := DragObject.GetDragCursor(DoDockOver(dsDragMove), Pos.X, Pos.Y);
    if FDragImageList <> nil then
    begin
      if (Target = nil) or (csDisplayDragImage in Target.ControlStyle) then
      begin
        FDragImageList.DragCursor := DragCursor;
        if not FDragImageList.Dragging then
          FDragImageList.BeginDrag(GetDesktopWindow, Pos.X, Pos.Y)
        else
          FDragImageList.DragMove(Pos.X, Pos.Y);
      end
      else
      begin
        FDragImageList.EndDrag;
        Windows.SetCursor(Screen.Cursors[DragCursor]);
      end;
    end;

    ResetCursor;
    if FActiveDrag = dopDock then
    begin
      with DragObject do
      begin
        if Target = nil then
        begin
          if Assigned(JvGlobalDockClient) then
            JvGlobalDockClient.FormPositionDockRect(DragObject);
        end
        else
        begin
          DropOnControl := GetDropCtl;
          TempAlign := DropAlign;
          if DropOnControl = nil then
            DoGetDockEdge(TargetControl, DragTargetPos, TempAlign)
          else
            DoGetDockEdge(DropOnControl, DropOnControl.ScreenToClient(Pos), TempAlign);
          DropAlign := TempAlign;
        end;
      end;
      if DragObject <> nil then
        DragObject.DrawDragRect(DoErase);
    end;
  end;
end;

procedure TJvDockManager.EndLoad;
begin
  Dec(FLoadCount);
  if FLoadCount <= 0 then
  begin
    FLoadCount := 0;
  end;
end;

procedure TJvDockManager.EndSave;
begin
  Dec(FSaveCount);
  if FSaveCount <= 0 then
    FSaveCount := 0;
end;

function TJvDockManager.GetDockSiteAtPos(MousePos: TPoint;
  Client: TControl): TWinControl;
var
  I: Integer;
  R: TRect;
  Site: TWinControl;
  CanDock, ControlKeyDown: Boolean;

  function ValidDockTarget(Target: TWinControl): Boolean;
  var
    Info: TCheckTargetInfo;
    Control: TWinControl;
    R1, R2: TRect;
  begin
    Result := True;

    Info.CurrentWnd := DragFindWindow(MousePos);
    if Info.CurrentWnd = 0 then
      Exit;
    if GetWindow(Info.CurrentWnd, GW_OWNER) <> Application.Handle then
    begin
      Control := FindControl(Info.CurrentWnd);
      if Control = nil then
        Exit;
      while Control.Parent <> nil do
        Control := Control.Parent;
      Info.CurrentWnd := Control.Handle;
    end;

    Control := Target;
    while Control.Parent <> nil do
      Control := Control.Parent;
    Info.TargetWnd := Control.Handle;
    if Info.CurrentWnd = Info.TargetWnd then
      Exit;

    if Client.Parent <> nil then
    begin
      Control := Client.Parent;
      while Control.Parent <> nil do
        Control := Control.Parent;
      Info.ClientWnd := Control.Handle;
    end
    else
    if Client is TWinControl then
      Info.ClientWnd := TWinControl(Client).Handle
    else
      Info.ClientWnd := 0;

    Info.Found := False;
    Info.MousePos := MousePos;
    EnumThreadWindows(GetCurrentThreadID, @IsBeforeTargetWindow, LPARAM(@Info));

    if Info.Found then
    begin
      GetWindowRect(Info.CurrentWnd, R1);
      DoGetSiteInfo(Target, Client, R2, MousePos, CanDock);

      if (DragObject.Control.HostDockSite <> nil) and
        (DragObject.Control.HostDockSite.Handle = Info.CurrentWnd) then
        Exit;
      if IntersectRect(R1, R1, R2) then
        Result := False;
    end;
  end;

  function IsSiteChildOfClient: Boolean;
  begin
    if Client is TWinControl then
      Result := IsChild(TWinControl(Client).Handle, Site.Handle)
    else
      Result := False;
  end;

begin
  Result := nil;
  ControlKeyDown := (GetKeyState(VK_CONTROL) and not $7FFF) <> 0;
  if (FDockSiteList = nil) or ControlKeyDown then
    Exit;
  FQualifyingSites.Clear;
  for I := 0 to FDockSiteList.Count - 1 do
  begin
    Site := TWinControl(FDockSiteList[I]);
    if (Site <> Client) and Site.Showing and Site.Enabled and
      IsWindowVisible(Site.Handle) and (not IsSiteChildOfClient) then
    begin
      CanDock := True;
      DoGetSiteInfo(Site, Client, R, MousePos, CanDock);
      if CanDock and PtInRect(R, MousePos) then
        FQualifyingSites.AddSite(Site);
    end;
  end;
  if FQualifyingSites.Count > 0 then
    Result := FQualifyingSites.GetTopSite;
  if (Result <> nil) and not ValidDockTarget(Result) then
    Result := nil;
end;

function TJvDockManager.GetFormVisible(DockWindow: TWinControl): Boolean;
begin
  Result := JvDockControlForm.GetFormVisible(DockWindow);
end;

procedure TJvDockManager.HideDockForm(DockWindow: TWinControl);
begin
  JvDockControlForm.HideDockForm(DockWindow);
end;

function TJvDockManager.IsDockLoading: Boolean;
begin
  Result := FLoadCount > 0;
end;

function TJvDockManager.IsSaving: Boolean;
begin
  Result := FSaveCount > 0;
end;

procedure TJvDockManager.LoadDockTreeFromAppStorage(AppStorage: TJvCustomAppStorage;
  const AppStoragePath: string = '');
begin
  BeginLoad;
  try
    JvDockControlForm.LoadDockTreeFromAppStorage(AppStorage, AppStoragePath);
  finally
    EndLoad;
  end;
end;

procedure TJvDockManager.RegisterDockSite(Site: TWinControl; DoRegister: Boolean);
var
  Index: Integer;
begin
  if Site <> nil then
  begin
    if FDockSiteList = nil then
      FDockSiteList := TList.Create;
    Index := FDockSiteList.IndexOf(Pointer(Site));
    if DoRegister then
    begin
      if Index = -1 then
        FDockSiteList.Add(Pointer(Site));
    end
    else
    begin
      if Index <> -1 then
        FDockSiteList.Delete(Index);
    end;
  end;
end;

procedure TJvDockManager.ResetCursor;
begin
  if (JvGlobalDockClient <> nil) and (JvGlobalDockClient.DockStyle <> nil) then
    JvGlobalDockClient.DockStyle.ResetCursor(DragObject);
end;

procedure TJvDockManager.SaveDockTreeToAppStorage(AppStorage: TJvCustomAppStorage;
  const AppStoragePath: string = '');
begin
  BeginSave;
  try
    JvDockControlForm.SaveDockTreeToAppStorage(AppStorage, AppStoragePath);
  finally
    EndSave;
  end;
end;

procedure TJvDockManager.SetConjoinDockHostBorderStyle(Value: TFormBorderStyle);
begin
  JvDockControlForm.SetConjoinDockHostBorderStyle(Value);
end;

procedure TJvDockManager.SetTabDockHostBorderStyle(Value: TFormBorderStyle);
begin
  JvDockControlForm.SetTabDockHostBorderStyle(Value);
end;

procedure TJvDockManager.ShowDockForm(DockWindow: TWinControl);
begin
  JvDockControlForm.ShowDockForm(DockWindow);
end;

//=== { TJvDockPageControl } =================================================

constructor TJvDockPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csDoubleClicks, csOpaque];
  FPages := TList.Create;
  FTabSheetClass := TJvDockTabSheet;
end;

destructor TJvDockPageControl.Destroy;
var
  I: Integer;
begin
  for I := 0 to FPages.Count - 1 do
    TJvDockTabSheet(FPages[I]).FPageControl := nil;
  FPages.Free;
  inherited Destroy;
end;

function TJvDockPageControl.CanShowTab(TabIndex: Integer): Boolean;
begin
  Result := TJvDockTabSheet(FPages[TabIndex]).Enabled;
end;

procedure TJvDockPageControl.Change;
var
  Form: TCustomForm;
begin
  if TabIndex >= 0 then
    UpdateActivePage;
  if csDesigning in ComponentState then
  begin
    Form := GetParentForm(Self);
    if (Form <> nil) and (Form.Designer <> nil) then
      Form.Designer.Modified;
  end;
  inherited Change;
end;

procedure TJvDockPageControl.ChangeActivePage(Page: TJvDockTabSheet);
var
  ParentForm: TCustomForm;
begin
  if FActivePage <> Page then
  begin
    ParentForm := GetParentForm(Self);
    if (ParentForm <> nil) and (FActivePage <> nil) and
      FActivePage.ContainsControl(ParentForm.ActiveControl) then
    begin
      ParentForm.ActiveControl := FActivePage;
      if ParentForm.ActiveControl <> FActivePage then
      begin
        TabIndex := FActivePage.TabIndex;
        Exit;
      end;
    end;
    if Page <> nil then
    begin
      Page.BringToFront;
      Page.Visible := True;
      if (ParentForm <> nil) and (FActivePage <> nil) and
        (ParentForm.ActiveControl = FActivePage) then
        if Page.CanFocus then
          ParentForm.ActiveControl := Page
        else
          ParentForm.ActiveControl := Self;
    end;
    if FActivePage <> nil then
      FActivePage.Visible := False;
    FActivePage := Page;
    if (ParentForm <> nil) and (FActivePage <> nil) and
      (ParentForm.ActiveControl = FActivePage) then
      FActivePage.SelectFirst;
  end;
end;

procedure TJvDockPageControl.CMDesignHitTest(var Msg: TCMDesignHitTest);
var
  HitIndex: Integer;
  HitTestInfo: TTCHitTestInfo;
begin
  HitTestInfo.pt := SmallPointToPoint(Msg.Pos);
  HitIndex := SendMessage(Handle, TCM_HITTEST, 0, LPARAM(@HitTestInfo));
  if (HitIndex >= 0) and (HitIndex <> TabIndex) then
    Msg.Result := 1;
end;

procedure TJvDockPageControl.CMDialogKey(var Msg: TCMDialogKey);
begin
  if (Focused or Windows.IsChild(Handle, Windows.GetFocus)) and
    (Msg.CharCode = VK_TAB) and (GetKeyState(VK_CONTROL) < 0) then
  begin
    SelectNextPage(GetKeyState(VK_SHIFT) >= 0);
    Msg.Result := 1;
  end
  else
    inherited;
end;

procedure TJvDockPageControl.CMDockClient(var Msg: TCMDockClient);
var
  IsVisible: Boolean;
  DockCtl: TControl;
begin
  Msg.Result := 0;
  if FTabSheetClass <> nil then
    FNewDockSheet := FTabSheetClass.Create(Self)
  else
    FNewDockSheet := TJvDockTabSheet.Create(Self);
  try
    try
      DockCtl := Msg.DockSource.Control;
      FNewDockSheet.PageControl := Self;
      if DockCtl is TCustomForm then
        FNewDockSheet.Caption := TCustomForm(DockCtl).Caption;
      DockCtl.Dock(Self, Msg.DockSource.DockRect);
    except
      FNewDockSheet.Free;
      raise;
    end;
    IsVisible := DockCtl.Visible;
    FNewDockSheet.TabVisible := IsVisible;
    if IsVisible then
      ActivePage := FNewDockSheet;
    DockCtl.Align := alClient;
  finally
    FNewDockSheet := nil;
  end;
end;

procedure TJvDockPageControl.CMDockNotification(var Msg: TCMDockNotification);
var
  I: Integer;
  S: string;
  Page: TJvDockTabSheet;
begin
  Page := GetPageFromDockClient(Msg.Client);
  if Page <> nil then
    case Msg.NotifyRec.ClientMsg of
      WM_SETTEXT:
        begin
          S := PChar(Msg.NotifyRec.MsgLParam);
          for I := 1 to Length(S) do
            if CharInSet(S[I], [#13, #10]) then
            begin
              SetLength(S, I - 1);
              Break;
            end;
          Page.Caption := S;
        end;
      CM_VISIBLECHANGED:
        Page.TabVisible := Boolean(Msg.NotifyRec.MsgWParam);
    end;
  inherited;
end;

procedure TJvDockPageControl.CMUnDockClient(var Msg: TCMUnDockClient);
var
  Page: TJvDockTabSheet;
begin
{$ifdef RTL210_UP}
  if (csDestroying in Self.ComponentState) then exit; // rather ugly workaround for Delphi 2010+ crash at shutdown.
{$endif}

  Msg.Result := 0;
  Page := GetPageFromDockClient(Msg.Client);
  if Page <> nil then
  begin
    FUndockingPage := Page;
    Msg.Client.Align := alNone;
  end;
  if (VisibleDockClientCount = 1) or (DockClientCount <= 2) then
    PostMessage(Parent.Handle, WM_CLOSE, 0, 0);
end;

procedure TJvDockPageControl.DeleteTab(Page: TJvDockTabSheet; Index: Integer);
var
  UpdateIndex: Boolean;
begin
  UpdateIndex := Page = ActivePage;
  Tabs.Delete(Index);
  if UpdateIndex then
  begin
    if Index >= Tabs.Count then
      Index := Tabs.Count - 1;
    TabIndex := Index;
  end;
  UpdateActivePage;
end;

procedure TJvDockPageControl.DoAddDockClient(Client: TControl; const ARect: TRect);
begin
  if FNewDockSheet <> nil then
    Client.Parent := FNewDockSheet;
end;

procedure TJvDockPageControl.DockOver(Source: TDragDockObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  R: TRect;
begin
  GetWindowRect(Handle, R);
  Source.DockRect := R;
  DoDockOver(Source, X, Y, State, Accept);
end;

function TJvDockPageControl.DoMouseEvent(var Msg: TWMMouse;
  Control: TControl): TWMNCHitMessage;
begin
  Result := JvDockCreateNCMessage(Control, Msg.Msg + WM_NCMOUSEFIRST - WM_MOUSEFIRST,
    HTCAPTION, SmallPointToPoint(Msg.Pos));
end;

procedure TJvDockPageControl.DoRemoveDockClient(Client: TControl);
begin
  if (FUndockingPage <> nil) and not (csDestroying in ComponentState) then
  begin
    SelectNextPage(True);
    FUndockingPage.Free;
    FUndockingPage := nil;
  end;
end;

function TJvDockPageControl.FindNextPage(CurPage: TJvDockTabSheet;
  GoForward, CheckTabVisible: Boolean): TJvDockTabSheet;
var
  I, StartIndex: Integer;
begin
  if FPages.Count <> 0 then
  begin
    StartIndex := FPages.IndexOf(CurPage);
    if StartIndex = -1 then
      if GoForward then
        StartIndex := FPages.Count - 1
      else
        StartIndex := 0;
    I := StartIndex;
    repeat
      if GoForward then
      begin
        Inc(I);
        if I = FPages.Count then
          I := 0;
      end
      else
      begin
        if I = 0 then
          I := FPages.Count;
        Dec(I);
      end;
      Result := FPages[I];
      if not CheckTabVisible or Result.TabVisible then
        Exit;
    until I = StartIndex;
  end;
  Result := nil;
end;

function TJvDockPageControl.GetActivePageIndex: Integer;
begin
  if ActivePage <> nil then
    Result := ActivePage.GetPageIndex
  else
    Result := -1;
end;

procedure TJvDockPageControl.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to FPages.Count - 1 do
    Proc(TComponent(FPages[I]));
end;

function TJvDockPageControl.GetCount: Integer;
begin
  Result := FPages.Count;
end;

function TJvDockPageControl.GetDockClientFromMousePos(MousePos: TPoint): TControl;
var
  I, HitIndex: Integer;
  HitTestInfo: TTCHitTestInfo;
  Page: TJvDockTabSheet;
begin
  Result := nil;
  if DockSite then
  begin
    HitTestInfo.pt := MousePos;
    HitIndex := SendMessage(Handle, TCM_HITTEST, 0, LPARAM(@HitTestInfo));
    if HitIndex >= 0 then
    begin
      Page := nil;
      for I := 0 to HitIndex do
        Page := FindNextPage(Page, True, True);
      if (Page <> nil) and (Page.ControlCount > 0) then
      begin
        Result := Page.Controls[0];
        if Result.HostDockSite <> Self then
          Result := nil;
      end;
    end;
  end;
end;

function TJvDockPageControl.GetImageIndex(TabIndex: Integer): Integer;
var
  I, Visible, NotVisible: Integer;
begin
  if Assigned(FOnGetImageIndex) then
    Result := inherited GetImageIndex(TabIndex)
  else
  begin
    Visible := 0;
    NotVisible := 0;
    for I := 0 to FPages.Count - 1 do
    begin
      if not GetPage(I).TabVisible then
        Inc(NotVisible)
      else
        Inc(Visible);
      if Visible = TabIndex + 1 then
        Break;
    end;
    Result := GetPage(TabIndex + NotVisible).ImageIndex;
  end;
end;

function TJvDockPageControl.GetPage(Index: Integer): TJvDockTabSheet;
begin
  Result := FPages[Index];
end;

function TJvDockPageControl.GetPageFromDockClient(Client: TControl): TJvDockTabSheet;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if (Client.Parent = Pages[I]) and (Client.HostDockSite = Self) then
    begin
      Result := Pages[I];
      Break;
    end;
end;

procedure TJvDockPageControl.GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
  MousePos: TPoint; var CanDock: Boolean);
begin
  CanDock := GetPageFromDockClient(Client) = nil;
  inherited GetSiteInfo(Client, InfluenceRect, MousePos, CanDock);
end;

procedure TJvDockPageControl.InsertPage(Page: TJvDockTabSheet);
begin
  FPages.Add(Page);
  Page.FPageControl := Self;
  Page.UpdateTabShowing;
end;

procedure TJvDockPageControl.InsertTab(Page: TJvDockTabSheet);
begin
  Tabs.InsertObject(Page.TabIndex, Page.Caption, Page);
  UpdateActivePage;
end;

procedure TJvDockPageControl.Loaded;
begin
  inherited Loaded;
  UpdateTabHighlights;
end;

procedure TJvDockPageControl.MoveTab(CurIndex, NewIndex: Integer);
begin
  Tabs.Move(CurIndex, NewIndex);
end;

procedure TJvDockPageControl.RemovePage(Page: TJvDockTabSheet);
var
  NextSheet: TJvDockTabSheet;
begin
  NextSheet := FindNextPage(Page, True, not (csDesigning in ComponentState));
  if NextSheet = Page then
    NextSheet := nil;
  Page.SetTabShowing(False);
  Page.FPageControl := nil;
  FPages.Remove(Page);
  SetActivePage(NextSheet);
end;

procedure TJvDockPageControl.SelectNextPage(GoForward: Boolean; CheckTabVisible: Boolean = True);
var
  Page: TJvDockTabSheet;
begin
  Page := FindNextPage(ActivePage, GoForward, CheckTabVisible);
  if (Page <> nil) and (Page <> ActivePage) and CanChange then
  begin
    SetActivePage(Page);
    Change;
  end;
end;

procedure TJvDockPageControl.SetActivePage(Page: TJvDockTabSheet);
begin
  if (Page <> nil) and (Page.PageControl <> Self) then
    Exit;
  ChangeActivePage(Page);
  if Page = nil then
    TabIndex := -1
  else
  if Page = FActivePage then
    TabIndex := Page.TabIndex;
end;

procedure TJvDockPageControl.SetActivePageIndex(const Value: Integer);
begin
  if (Value > -1) and (Value < Count) then
    ActivePage := Pages[Value]
  else
    ActivePage := nil;
end;

procedure TJvDockPageControl.SetChildOrder(Child: TComponent; Order: Integer);
begin
  TJvDockTabSheet(Child).PageIndex := Order;
end;

procedure TJvDockPageControl.ShowControl(AControl: TControl);
begin
  if (AControl is TJvDockTabSheet) and (TJvDockTabSheet(AControl).PageControl = Self) then
    SetActivePage(TJvDockTabSheet(AControl));
  inherited ShowControl(AControl);
end;

procedure TJvDockPageControl.UpdateActivePage;
begin
  if TabIndex >= 0 then
    SetActivePage(TJvDockTabSheet(Tabs.Objects[TabIndex]))
  else
    SetActivePage(nil);
end;

procedure TJvDockPageControl.UpdateTab(Page: TJvDockTabSheet);
begin
  Tabs[Page.TabIndex] := Page.Caption;
end;

procedure TJvDockPageControl.UpdateTabHighlights;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Pages[I].SetHighlighted(Pages[I].FHighlighted);
end;

procedure TJvDockPageControl.WMLButtonDblClk(var Msg: TWMLButtonDblClk);
var
  DockCtl: TControl;
begin
  inherited;
  if JvGlobalDockClient <> nil then
    DockCtl := ButtonEvent(Self, Msg, mbLeft, msTabPage, JvGlobalDockClient.DoNCButtonDblClk)
  else
    DockCtl := nil;
  if (DockCtl <> nil) and JvGlobalDockClient.CanFloat then
    DockCtl.ManualDock(nil, nil, alNone);
end;

procedure TJvDockPageControl.WMLButtonDown(var Msg: TWMLButtonDown);
var
  DockCtl: TControl;
begin
  inherited;
  if JvGlobalDockClient <> nil then
    DockCtl := ButtonEvent(Self, Msg, mbLeft, msTabPage, JvGlobalDockClient.DoNCButtonDown)
  else
    DockCtl := nil;
  if (DockCtl <> nil) and (Style = tsTabs) then
    JvGlobalDockManager.BeginDrag(DockCtl, False);
end;

procedure TJvDockPageControl.WMLButtonUp(var Msg: TWMLButtonUp);
begin
  inherited;
  if JvGlobalDockClient <> nil then
    ButtonEvent(Self, Msg, mbLeft, msTabPage, JvGlobalDockClient.DoNCButtonUp);
end;

procedure TJvDockPageControl.WMMButtonDblClk(var Msg: TWMMButtonDblClk);
begin
  inherited;
  if JvGlobalDockClient <> nil then
    ButtonEvent(Self, Msg, mbMiddle, msTabPage, JvGlobalDockClient.DoNCButtonDblClk);
end;

procedure TJvDockPageControl.WMMButtonDown(var Msg: TWMMButtonDown);
begin
  inherited;
  if JvGlobalDockClient <> nil then
    ButtonEvent(Self, Msg, mbMiddle, msTabPage, JvGlobalDockClient.DoNCButtonDown);
end;

procedure TJvDockPageControl.WMMButtonUp(var Msg: TWMMButtonUp);
begin
  inherited;
  if JvGlobalDockClient <> nil then
    ButtonEvent(Self, Msg, mbMiddle, msTabPage, JvGlobalDockClient.DoNCButtonUp);
end;

procedure TJvDockPageControl.WMRButtonDblClk(var Msg: TWMRButtonDblClk);
begin
  inherited;
  if JvGlobalDockClient <> nil then
    ButtonEvent(Self, Msg, mbRight, msTabPage, JvGlobalDockClient.DoNCButtonDblClk);
end;

procedure TJvDockPageControl.WMRButtonDown(var Msg: TWMRButtonDown);
begin
  Msg.Msg := WM_LBUTTONDOWN;
  inherited;
  if JvGlobalDockClient <> nil then
    ButtonEvent(Self, Msg, mbRight, msTabPage, JvGlobalDockClient.DoNCButtonDown);
end;

procedure TJvDockPageControl.WMRButtonUp(var Msg: TWMRButtonUp);
begin
  inherited;
  if JvGlobalDockClient <> nil then
    ButtonEvent(Self, Msg, mbRight, msTabPage, JvGlobalDockClient.DoNCButtonUp);
end;

//=== { TJvDockTabSheet } ====================================================

constructor TJvDockTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Align := alClient;
  ControlStyle := ControlStyle + [csAcceptsControls, csNoDesignVisible];
  Visible := False;
  FTabVisible := True;
  FHighlighted := False;
  FImageIndex := -1;
end;

destructor TJvDockTabSheet.Destroy;
begin
  if FPageControl <> nil then
  begin
    if FPageControl.FUndockingPage = Self then
      FPageControl.FUndockingPage := nil;
    FPageControl.RemovePage(Self);
  end;
  inherited Destroy;
end;

procedure TJvDockTabSheet.CMShowingChanged(var Msg: TMessage);
begin
  inherited;
  if Showing then
    try
      DoShow
    except
      Application.HandleException(Self);
    end
  else
  if not Showing then
    try
      DoHide;
    except
      Application.HandleException(Self);
    end;
end;

procedure TJvDockTabSheet.CMTextChanged(var Msg: TMessage);
begin
  if FTabShowing then
    FPageControl.UpdateTab(Self);
end;

procedure TJvDockTabSheet.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    style := style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TJvDockTabSheet.DoHide;
begin
  if Assigned(FOnHide) then
    FOnHide(Self);
end;

procedure TJvDockTabSheet.DoShow;
begin
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

function TJvDockTabSheet.GetPageIndex: Integer;
begin
  if (FPageControl <> nil) and (FPageControl.FPages <> nil) then
    Result := FPageControl.FPages.IndexOf(Self)
  else
    Result := -1;
end;

function TJvDockTabSheet.GetTabIndex: Integer;
var
  I: Integer;
begin
  Result := 0;
  if not FTabShowing then
    Dec(Result)
  else
    for I := 0 to PageIndex - 1 do
      if TJvDockTabSheet(FPageControl.FPages[I]).FTabShowing then
        Inc(Result);
end;

procedure TJvDockTabSheet.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TJvDockPageControl then
    PageControl := TJvDockPageControl(Reader.Parent);
end;

procedure TJvDockTabSheet.SetHighlighted(Value: Boolean);
begin
  if not (csReading in ComponentState) then
    SendMessage(PageControl.Handle, TCM_HIGHLIGHTITEM, TabIndex, MakeLong(Word(Value), 0));
  FHighlighted := Value;
end;

procedure TJvDockTabSheet.SetImageIndex(Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    if FTabShowing then
      FPageControl.UpdateTab(Self);
  end;
end;

procedure TJvDockTabSheet.SetPageControl(APageControl: TJvDockPageControl);
begin
  if FPageControl <> APageControl then
  begin
    if FPageControl <> nil then
      FPageControl.RemovePage(Self);
    Parent := APageControl;
    if APageControl <> nil then
      APageControl.InsertPage(Self);
  end;
end;

procedure TJvDockTabSheet.SetPageIndex(Value: Integer);
var
  I, MaxPageIndex: Integer;
begin
  if (FPageControl <> nil) and (FPageControl.FPages <> nil) then
  begin
    MaxPageIndex := FPageControl.FPages.Count - 1;
    if Value > MaxPageIndex then
      raise EListError.CreateResFmt(@SPageIndexError, [Value, MaxPageIndex]);
    I := TabIndex;
    FPageControl.FPages.Move(PageIndex, Value);
    if I >= 0 then
      FPageControl.MoveTab(I, TabIndex);
  end;
end;

procedure TJvDockTabSheet.SetTabShowing(Value: Boolean);
var
  Index: Integer;
begin
  if FTabShowing <> Value then
    if Value then
    begin
      FTabShowing := True;
      FPageControl.InsertTab(Self);
    end
    else
    begin
      Index := TabIndex;
      FTabShowing := False;
      FPageControl.DeleteTab(Self, Index);
    end;
end;

procedure TJvDockTabSheet.SetTabVisible(Value: Boolean);
begin
  if FTabVisible <> Value then
  begin
    FTabVisible := Value;
    UpdateTabShowing;
  end;
end;

procedure TJvDockTabSheet.UpdateTabShowing;
begin
  SetTabShowing((FPageControl <> nil) and FTabVisible);
end;

//=== { TJvDockTabStrings } ==================================================

procedure TJvDockTabStrings.Clear;
begin
  if SendMessage(FTabControl.Handle, TCM_DELETEALLITEMS, 0, 0) = 0 then
    TabControlError(sTabFailClear);
  FTabControl.TabsChanged;
end;

procedure TJvDockTabStrings.Delete(Index: Integer);
begin
  if SendMessage(FTabControl.Handle, TCM_DELETEITEM, Index, 0) = 0 then
    TabControlError(Format(sTabFailDelete, [Index]));
  FTabControl.TabsChanged;
end;

function TJvDockTabStrings.Get(Index: Integer): string;
const
  RTL: array [Boolean] of Longint = (0, TCIF_RTLREADING);
var
  TCItem: TTCItem;
  Buffer: array [0..4095] of Char;
begin
  TCItem.mask := TCIF_TEXT or RTL[FTabControl.UseRightToLeftReading];
  TCItem.pszText := Buffer;
  TCItem.cchTextMax := SizeOf(Buffer);
  if SendMessage(FTabControl.Handle, TCM_GETITEM, Index, LPARAM(@TCItem)) = 0 then
    TabControlError(Format(sTabFailRetrieve, [Index]));
  Result := Buffer;
end;

function TJvDockTabStrings.GetCount: Integer;
begin
  Result := SendMessage(FTabControl.Handle, TCM_GETITEMCOUNT, 0, 0);
end;

function TJvDockTabStrings.GetObject(Index: Integer): TObject;
var
  TCItem: TTCItem;
begin
  TCItem.mask := TCIF_PARAM;
  if SendMessage(FTabControl.Handle, TCM_GETITEM, Index, LPARAM(@TCItem)) = 0 then
    TabControlError(Format(sTabFailGetObject, [Index]));
  Result := TObject(TCItem.lParam);
end;

procedure TJvDockTabStrings.Insert(Index: Integer; const S: string);
const
  RTL: array [Boolean] of Longint = (0, TCIF_RTLREADING);
var
  TCItem: TTCItem;
begin
  TCItem.mask := TCIF_TEXT or RTL[FTabControl.UseRightToLeftReading] or
    TCIF_IMAGE;
  TCItem.pszText := PChar(S);
  TCItem.iImage := FTabControl.GetImageIndex(Index);
  if SendMessage(FTabControl.Handle, TCM_INSERTITEM, Index, LPARAM(@TCItem)) < 0 then
    TabControlError(Format(sTabFailSet, [S, Index]));
  FTabControl.TabsChanged;
end;

procedure TJvDockTabStrings.Put(Index: Integer; const S: string);
const
  RTL: array [Boolean] of Longint = (0, TCIF_RTLREADING);
var
  TCItem: TTCItem;
begin
  TCItem.mask := TCIF_TEXT or RTL[FTabControl.UseRightToLeftReading] or
    TCIF_IMAGE;
  TCItem.pszText := PChar(S);
  TCItem.iImage := FTabControl.GetImageIndex(Index);
  if SendMessage(FTabControl.Handle, TCM_SETITEM, Index, LPARAM(@TCItem)) = 0 then
    TabControlError(Format(sTabFailSet, [S, Index]));
  FTabControl.TabsChanged;
end;

procedure TJvDockTabStrings.PutObject(Index: Integer; AObject: TObject);
var
  TCItem: TTCItem;
begin
  TCItem.mask := TCIF_PARAM;
  TCItem.lParam := LPARAM(AObject);
  if SendMessage(FTabControl.Handle, TCM_SETITEM, WPARAM(Index), LPARAM(@TCItem)) = 0 then
    TabControlError(Format(sTabFailSetObject, [Index]));
end;

procedure TJvDockTabStrings.SetUpdateState(Updating: Boolean);
begin
  FTabControl.FUpdating := Updating;
  SendMessage(FTabControl.Handle, WM_SETREDRAW, Ord(not Updating), 0);
  if not Updating then
  begin
    FTabControl.Invalidate;
    FTabControl.TabsChanged;
  end;
end;

//=== { TSiteList } ==========================================================

procedure TSiteList.AddSite(ASite: TWinControl);
var
  SI: PSiteInfoRec;
  Index: Integer;

  function GetTopParent: HWND;
  var
    NextParent: HWND;
  begin
    NextParent := ASite.Handle;
    Result := NextParent;
    while NextParent <> 0 do
    begin
      Result := NextParent;
      NextParent := GetParent(NextParent);
    end;
  end;

begin
  New(SI);
  SI.Site := ASite;
  SI.TopParent := GetTopParent;
  if Find(SI.TopParent, Index) then
    Insert(Index, SI)
  else
    Add(SI);
end;

procedure TSiteList.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Dispose(PSiteInfoRec(Items[I]));
  inherited Clear;
end;

function TSiteList.Find(ParentWnd: THandle; var Index: Integer): Boolean;
begin
  Index := 0;
  Result := False;
  while Index < Count do
  begin
    Result := (PSiteInfoRec(Items[Index]).TopParent = ParentWnd);
    if Result then
      Exit;
    Inc(Index);
  end;
end;

function TSiteList.GetTopSite: TWinControl;
var
  Index: Integer;
  DesktopWnd, CurrentWnd: HWND;
begin
  Result := nil;
  if Count = 0 then
    Exit
  else
  if Count = 1 then
    Result := PSiteInfoRec(Items[0]).Site
  else
  begin
    DesktopWnd := GetDesktopWindow;
    CurrentWnd := GetTopWindow(DesktopWnd);
    while (Result = nil) and (CurrentWnd <> 0) do
      if Find(CurrentWnd, Index) then
        Result := PSiteInfoRec(List[Index])^.Site
      else
        CurrentWnd := GetNextWindow(CurrentWnd, GW_HWNDNEXT);
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
