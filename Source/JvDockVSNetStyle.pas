{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDockVSNetStyle.pas, released on 2003-12-31.

The Initial Developer of the Original Code is luxiaoban.
Portions created by luxiaoban are Copyright (C) 2002,2003 luxiaoban.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDockVSNetStyle;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Classes, Graphics, Controls, Forms, ExtCtrls,
  JvDockControlForm, JvDockSupportControl, JvDockTree, JvDockVIDStyle,
  JvDockGlobals, Contnrs;

type
  TJvDockVSNETConjoinServerOption = class(TJvDockVIDConjoinServerOption)
  protected
    procedure UpdateDefaultSystemCaptionInfo; override;
  public
    constructor Create(ADockStyle: TJvDockObservableStyle); override;
  end;

  TJvDockVSNETTabServerOption = class(TJvDockVIDTabServerOption)
  public
    constructor Create(ADockStyle: TJvDockObservableStyle); override;
  published
    property InactiveSheetColor default VSNETPageInactiveSheetColor;
    property ShowTabImages default True;
  end;

  TJvDockVSNETChannelOption = class(TJvDockBasicServerOption)
  private
    FActivePaneSize: Integer;
    FShowImage: Boolean;
    FMouseleaveHide: Boolean;
    FHideHoldTime: Integer;
    FTabColor: TColor;
    procedure SetActivePaneSize(Value: Integer);
    procedure SetShowImage(const Value: Boolean);
    procedure SetHideHoldTime(const Value: Integer);
    procedure SetMouseleaveHide(const Value: Boolean);
    procedure SetTabColor(const Value: TColor);
  public
    constructor Create(ADockStyle: TJvDockObservableStyle); override;
    procedure Assign(Source: TPersistent); override;
  published
    property ActivePaneSize: Integer read FActivePaneSize write SetActivePaneSize default 100;
    { ShowImage is not used }
    property ShowImage: Boolean read FShowImage write SetShowImage default True;
    property MouseleaveHide: Boolean read FMouseleaveHide write SetMouseleaveHide default True;
    property HideHoldTime: Integer read FHideHoldTime write SetHideHoldTime default 1000;
    property TabColor: TColor read FTabColor write SetTabColor default clBtnFace;
  end;

  TJvDockVSNETChannelOptionClass = class of TJvDockVSNETChannelOption;

  TJvDockVSBlock = class;
  TJvDockVSChannel = class;
  TJvDockVSNETPanel = class;
  TJvDockVSPopupPanel = class;
  TJvDockVSPopupPanelSplitter = class;

  TJvDockVSPane = class(TObject)
  private
    FBlock: TJvDockVSBlock;
    FDockForm: TCustomForm;
    FIndex: Integer;
    FWidth: Integer;
    FVisible: Boolean;
    function GetActive: Boolean;
  public
    constructor Create(ABlock: TJvDockVSBlock; AForm: TCustomForm; AWidth: Integer; AIndex: Integer); virtual;
    destructor Destroy; override;
    // KV added
    property PopUpPanelWidth: Integer read FWidth write FWidth;
    property Active: Boolean read GetActive;
    property Visible: Boolean read FVisible;
    property DockForm: TCustomForm read FDockForm;
  end;

  TJvDockBlockType = (btConjoinBlock, btTabBlock);

  TJvDockVSBlock = class(TObject)
  private
    FVSChannel: TJvDockVSChannel;
    FVSPanes: TObjectList;
    FActiveBlockWidth: Integer;
    FInactiveBlockWidth: Integer;
    FBlockType: TJvDockBlockType;
    FImageList: TImageList;
    FBlockStartPos: Integer;
    FActivePane: TJvDockVSPane;
    function GetVSPane(Index: Integer): TJvDockVSPane;
    function GetVSPaneCount: Integer;
    function GetActiveDockControl: TWinControl;
    procedure SetActivePane(APane: TJvDockVSPane);
    function GetActiveBlockWidth: Integer;
    function GetInactiveBlockWidth: Integer;
  protected
    procedure ResetActiveBlockWidth;
    function AddPane(AControl: TControl; const AWidth: Integer): TJvDockVSPane;
    procedure DeletePane(Index: Integer);
    procedure UpdateActivePane(StartIndex: Integer);
    function PPIScale(Value: Integer): Integer;
    { Following names should be ActivePaneWidth, InactivePaneWidth }
    { ActivePane has size ActiveBlockWidth.. }
    property ActiveBlockWidth: Integer read GetActiveBlockWidth write FActiveBlockWidth;
    { ..other panes have size InactiveBlockWidth }
    property InactiveBlockWidth: Integer read GetInactiveBlockWidth write FInactiveBlockWidth;
    { The popup dock form of ActivePane }
    property ActiveDockControl: TWinControl read GetActiveDockControl;
    { Pane that last displayed its popup dock form. A block always has an
      ActivePane. If no Pane has shown its popup dock form, then the last
      added pane is the ActivePane }
    property ActivePane: TJvDockVSPane read FActivePane write SetActivePane;
    property BlockType: TJvDockBlockType read FBlockType;
    { Owner }
    property VSChannel: TJvDockVSChannel read FVSChannel;
  public
    constructor Create(AOwner: TJvDockVSChannel); virtual;
    destructor Destroy; override;
    procedure AddDockControl(Control: TWinControl);
    procedure RemoveDockControl(Control: TWinControl);
    function FindDockControl(Control: TWinControl; var PaneIndex: Integer): Boolean;
    function GetTotalWidth: Integer;
    property VSPaneCount: Integer read GetVSPaneCount;
    property VSPane[Index: Integer]: TJvDockVSPane read GetVSPane;
    // KV properties added
    property ImageList: TImageList read FImageList;
  end;

  TJvDockChannelState = (csShow, csHide);

  {
     TJvDockServer
       |
       |----- TJvDockVSNETPanel (4x per server)
                |
                |----- TJvDockVSChannel
                             |
                             |----    TJvDockVSPopupPanel
                             |
                             |----    TJvDockVSPopupPanelSplitter

     -------- = maintains/creates

  }

  TJvDockVSChannel = class(TCustomControl)
  private
    FAnimationDelayTimer: TTimer;
    FPopupPane: TJvDockVSPane;
    FVSNETDockPanel: TJvDockVSNETPanel; { Owner }
    FCurrentPos: Integer;
    FBlocks: TObjectList;
    FChannelWidth: Integer;
    FBlockStartOffset: Integer;
    FBlockUpOffset: Integer;
    FBlockInterval: Integer;
    FVSPopupPanel: TJvDockVSPopupPanel;
    FVSPopupPanelSplitter: TJvDockVSPopupPanelSplitter;
    FActivePaneSize: Integer;
    FDelayPane: TJvDockVSPane;
    FStyleLink: TJvDockStyleLink;
    FTabColor: TColor;
    function GetBlockCount: Integer;
    function GetBlock(Index: Integer): TJvDockVSBlock;
    function PaneAtPos(MousePos: TPoint): TJvDockVSPane;
    procedure SetBlockStartOffset(const Value: Integer);
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure FreeBlockList;
    procedure SetActivePaneSize(const Value: Integer);
    procedure DoAnimationDelay(Sender: TObject);
    procedure DockStyleChanged(Sender: TObject);
    procedure SetTabColor(const Value: TColor);
    function GetDockServer: TJvDockServer;
    function GetDockStyle: TJvDockObservableStyle;
    function GetActiveDockForm: TCustomForm;
    function GetChannelWidth: Integer;
    function GetBlockStartOffset: Integer;
    function GetBlockUpOffset: Integer;
    function GetBlockInterval: Integer;
    function GetActivePaneSize: Integer;
  protected
    // KV move GetBlockRect to protected and added FBlockImageListSize, FInactiveBlockWidth
    FBlockImageListSize: Integer;
    FInactiveBlockWidth: Integer;
    procedure GetBlockRect(Block: TJvDockVSBlock; Index: Integer; var ARect: TRect);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure InternalInsertControl(AWinControl: TWinControl);
    procedure InternalRemoveControl(AWinControl: TWinControl);
    procedure SetPopupPane(APane: TJvDockVSPane);
    procedure PopupPaneChanged; virtual;

    procedure ResetFontAngle; virtual;
    procedure ResetBlock; virtual;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure SetVSPopupPanelSplitterPosition;
    procedure SyncWithStyle; virtual;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    property ChannelWidth: Integer read GetChannelWidth write FChannelWidth;
    property BlockStartOffset: Integer read GetBlockStartOffset write SetBlockStartOffset;
    property BlockUpOffset: Integer read GetBlockUpOffset;
    property BlockInterval: Integer read GetBlockInterval;
    property DockServer: TJvDockServer read GetDockServer;
    // KV property added
    property CurrentPos: Integer read FCurrentPos write FCurrentPos;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    { Same as FindPane? }
    function PPIScale(Value: Integer): Integer;
    function GetPaneWithControl(AControl: TControl): TJvDockVSPane;
    procedure CreateVSPopupPanel;
    procedure DestroyVSPopupPanel;
    procedure ResetPosition;
    procedure AddDockControl(Control: TWinControl);
    procedure RemoveDockControl(Control: TWinControl);
    function FindDockControl(Control: TWinControl; var BlockIndex: Integer;
      var PaneIndex: Integer): Boolean;
    function FindPane(Control: TWinControl): TJvDockVSPane;
    procedure AutoFocusActiveDockForm;
    { Slides the window into view }
    procedure PopupDockForm(Pane: TJvDockVSPane); overload;
    procedure PopupDockForm(Control: TWinControl); overload;
    { Disables auto-hide }
    procedure ShowPopupPanel(Pane: TJvDockVSPane); overload;
    procedure ShowPopupPanel(Control: TWinControl); overload;
    { Hides the window by sliding it to the edge of the form }
    procedure HidePopupPanel(Pane: TJvDockVSPane); overload;
    procedure HidePopupPanel(Control: TWinControl); overload;
    procedure HidePopupPanelWithAnimate;
    procedure ResetActivePaneWidth;
    procedure ResetPopupPanelHeight;
    procedure RemoveAllBlock;
    procedure DeleteBlock(Index: Integer);
    property BlockCount: Integer read GetBlockCount;
    property Block[Index: Integer]: TJvDockVSBlock read GetBlock;
    property VSPopupPanel: TJvDockVSPopupPanel read FVSPopupPanel;
    property VSPopupPanelSplitter: TJvDockVSPopupPanelSplitter read FVSPopupPanelSplitter;
    { Popup dock form that is visible; nil if no popup form is visible }
    property ActiveDockForm: TCustomForm read GetActiveDockForm;
    { Maximum size of a block's active pane }
    property ActivePaneSize: Integer read GetActivePaneSize write SetActivePaneSize;
    { Pane that has a visible popup dock form; nil if no popup dock form is visible }
    property PopupPane: TJvDockVSPane read FPopupPane;
    property TabColor: TColor read FTabColor write SetTabColor;
    property DockStyle: TJvDockObservableStyle read GetDockStyle;
  end;

  TJvDockVSChannelClass = class of TJvDockVSChannel;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvDockVSNetStyle = class(TJvDockVIDStyle)
  private
    FTimer: TTimer;
    FDockServers: TList;
    FCurrentTimer: Integer;
    FChannelOption: TJvDockVSNETChannelOption;
    FChannelOptionClass: TJvDockVSNETChannelOptionClass;
    procedure Timer(Sender: TObject);
    function GetChannelOption: TJvDockVSNETChannelOption;
    procedure SetChannelOption(const Value: TJvDockVSNETChannelOption);
  protected
    function DockServerWindowProc(DockServer: TJvDockServer; var Msg: TMessage): Boolean; override;
    function DockClientWindowProc(DockClient: TJvDockClient; var Msg: TMessage): Boolean; override;
    procedure AddDockBaseControl(ADockBaseControl: TJvDockBaseControl); override;
    procedure RemoveDockBaseControl(ADockBaseControl: TJvDockBaseControl); override;
    procedure CreateServerOption; override; { AfterConstruction }
    procedure FreeServerOption; override; { Destroy }

    procedure BeginPopup(AChannel: TJvDockVSChannel);
    procedure EndPopup(AChannel: TJvDockVSChannel);

    { construction/destruction of timer is a bit rigid }
    procedure CreateTimer;
    procedure DestroyTimer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoUnAutoHideDockForm(DockWindow: TWinControl); virtual;
    procedure DoShowDockForm(DockWindow: TWinControl); override;
    procedure DoHideDockForm(DockWindow: TWinControl); override;
    procedure SetDockFormVisible(ADockClient: TJvDockClient; AVisible: Boolean);
    procedure ShowDockForm(ADockClient: TJvDockClient); override;
    procedure HideDockForm(ADockClient: TJvDockClient); override;
    function GetDockFormVisible(ADockClient: TJvDockClient): Boolean; override;
    procedure RestoreClient(DockClient: TJvDockClient); override;
    class procedure SetAnimationInterval(const Value: Integer);
    class function GetAnimationInterval: Integer;
    class function GetAnimationStartInterval: Integer;
    class procedure SetAnimationMoveWidth(const Value: Integer);
    class function GetAnimationMoveWidth: Integer;
  published
    property ChannelOption: TJvDockVSNETChannelOption read GetChannelOption write SetChannelOption;
  end;

  TJvDockVSNETSplitter = class(TJvDockVIDSplitter);

  { A 'pure' TJvDockVSNETPanel maintains a TJvDockVSChannel (A TJvDockVSPopupPanel
    component that is a TJvDockVSNETPanel descendant does NOT, see
    TJvDockVSNETPanel.AddDockServer)
  }
  TJvDockVSNETPanel = class(TJvDockVIDPanel)
  private
    FVSChannelClass: TJvDockVSChannelClass;
    FVSChannel: TJvDockVSChannel;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure AddDockServer(ADockServer: TJvDockServer); override;
    procedure RemoveDockServer(ADockServer: TJvDockServer); override;
    procedure CustomDockDrop(Source: TJvDockDragDockObject; X, Y: Integer); override;
    procedure Resize; override;
    //KV
    property VSChannelClass: TJvDockVSChannelClass
      read FVSChannelClass write FVSChannelClass;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateVSChannel;
    procedure DestroyVSChannel;
    procedure DoAutoHideControl(Control: TWinControl);
    procedure DoHideControl(Control: TWinControl);
    procedure DoShowControl(Control: TWinControl);
    property VSChannel: TJvDockVSChannel read FVSChannel;
  end;

  TJvDockVSPopupPanel = class(TJvDockVSNETPanel)
  private
    FVSNETDockPanel: TJvDockVSNETPanel;
    {procedure SetVSNETDockPanel(const Value: TJvDockVSNETPanel);}
    function GetVSChannel: TJvDockVSChannel;
  protected
    function CreateDockManager: IDockManager; override;
    procedure SetParent(AParent: TWinControl); override;
  public
    // Can't put 'override' this one because signature is different!
    // But it MUST have DockStyle in the constructor now! -Wpostma!
    constructor Create(AOwner: TComponent; APanel: TJvDockVSNETPanel); reintroduce; virtual;

    procedure ShowDockPanel(MakeVisible: Boolean; Client: TControl;
      PanelSizeFrom: TJvDockSetDockPanelSizeFrom); override;
    { Dirty override; solve with virtual method? }
    property VSChannel: TJvDockVSChannel read GetVSChannel;
    { Owner }
    property VSNETDockPanel: TJvDockVSNETPanel read FVSNETDockPanel {write SetVSNETDockPanel};
  end;

  TJvDockVSNETConjoinPanel = class(TJvDockVIDConjoinPanel);

  TJvDockBtnState = (bsUp, bsNormal, bsDown);

  TJvDockVSNETZone = class(TJvDockVIDZone)
  private
    FAutoHideBtnDown: Boolean;
    FAutoHideBtnState: TJvDockBtnState;
    FCloseBtnState: TJvDockBtnState;
    FVSPaneVisible: Boolean;
    procedure SetAutoHideBtnState(const Value: TJvDockBtnState);
    procedure SetCloseBtnState(const Value: TJvDockBtnState);
    procedure SetAutoHideBtnDown(const Value: Boolean);
    procedure SetVSPaneVisible(const Value: Boolean);
  protected
    procedure DoCustomSetControlName; override;
    procedure SetChildControlVisible(Client: TControl; AVisible: Boolean); override;
    property AutoHideBtnDown: Boolean read FAutoHideBtnDown write SetAutoHideBtnDown;
    property AutoHideBtnState: TJvDockBtnState read FAutoHideBtnState write SetAutoHideBtnState;
    property CloseBtnState: TJvDockBtnState read FCloseBtnState write SetCloseBtnState;
    property VSPaneVisible: Boolean read FVSPaneVisible write SetVSPaneVisible;
  public
    constructor Create(Tree: TJvDockTree); override;
  end;

  TJvDockVSNETTree = class(TJvDockVIDTree)
  private
    FAutoHideZone: TJvDockVSNETZone;
  protected
    procedure IgnoreZoneInfor(Stream: TMemoryStream); override;
    procedure BeginDrag(Control: TControl;
      Immediate: Boolean; Threshold: Integer = -1); override;
    function DoLButtonDown(var Msg: TWMMouse;
      var Zone: TJvDockZone; out HTFlag: Integer): Boolean; override;
    procedure DoLButtonUp(var Msg: TWMMouse;
      var Zone: TJvDockZone; out HTFlag: Integer); override;
    procedure DoLButtonDbClk(var Msg: TWMMouse;
      var Zone: TJvDockZone; out HTFlag: Integer); override;
    procedure DoMouseMove(var Msg: TWMMouse;
      var AZone: TJvDockZone; out HTFlag: Integer); override;
    procedure DoHideZoneChild(AZone: TJvDockZone); override;
    function GetTopGrabbersHTFlag(const MousePos: TPoint;
      out HTFlag: Integer; Zone: TJvDockZone): TJvDockZone; override;
    procedure DrawDockGrabber(Control: TWinControl; const ARect: TRect); override;
    procedure PaintDockGrabberRect(Canvas: TCanvas; Control: TWinControl;
      const ARect: TRect; PaintAlways: Boolean = False); override;
    procedure DrawCloseButton(Canvas: TCanvas; Zone: TJvDockZone;
      Left, Top: Integer); override;
    procedure DrawAutoHideButton(Zone: TJvDockZone;
      Left, Top: Integer); virtual;
    procedure GetCaptionRect(var Rect: TRect); override;
    procedure DoOtherHint(Zone: TJvDockZone;
      HTFlag: Integer; var HintStr: string); override;
    procedure CustomSaveZone(Stream: TStream;
      Zone: TJvDockZone); override;
    procedure CustomLoadZone(Stream: TStream;
      var Zone: TJvDockZone); override;
    property AutoHideZone: TJvDockVSNETZone read FAutoHideZone
      write FAutoHideZone;
  public
    constructor Create(DockSite: TWinControl; DockZoneClass: TJvDockZoneClass;
      ADockStyle: TJvDockObservableStyle); override;
  end;

  TJvDockVSNETTabSheet = class(TJvDockVIDTabSheet)
  private
    FOldVisible: Boolean;
    procedure SetOldVisible(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    property OldVisible: Boolean read FOldVisible write SetOldVisible;
  end;

  TJvDockVSNETTabPanel = class(TJvDockTabPanel)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TJvDockVSNETTabPageControl = class(TJvDockVIDTabPageControl)
  protected
    procedure ShowControl(AControl: TControl); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TJvDockVSNETDragDockObject = class(TJvDockVIDDragDockObject);

  TJvDockVSPopupPanelSplitter = class(TCustomControl)
  private
    FVSPopupPanel: TJvDockVSPopupPanel;
    FSplitWidth: Integer;
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
    function FindControl: TControl;
    procedure FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ReleaseLineDC;
    procedure SetBeveled(Value: Boolean);
    procedure UpdateControlSize;
    procedure UpdateSize(X, Y: Integer);
    procedure SetVSPopupPanel(Value: TJvDockVSPopupPanel);
    function GetVSChannelAlign: TAlign;
    procedure SetSplitWidth(const Value: Integer);
    function GetMinSize: NaturalNumber;
    function GetSplitWidth: Integer;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    function CanResize(var NewSize: Integer): Boolean; reintroduce; virtual;
    function DoCanResize(var NewSize: Integer): Boolean; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure RequestAlign; override;
    procedure StopSizing; dynamic;
    function PPIScale(Value: Integer): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
    { Owner of the Owner }
    property VSPopupPanel: TJvDockVSPopupPanel read FVSPopupPanel write SetVSPopupPanel;
    property SplitWidth: Integer read GetSplitWidth write SetSplitWidth;
  published
    property Align default alLeft;
    property VSChannelAlign: TAlign read GetVSChannelAlign;
    property AutoSnap: Boolean read FAutoSnap write FAutoSnap default True;
    property Beveled: Boolean read FBeveled write SetBeveled default False;
    property Color;
    property Constraints;
    property MinSize: NaturalNumber read GetMinSize write FMinSize default 30;
    property ParentColor;
    property ResizeStyle: TResizeStyle read FResizeStyle write FResizeStyle default rsPattern;
    property Visible;
    property OnCanResize: TCanResizeEvent read FOnCanResize write FOnCanResize;
    property OnMoved: TNotifyEvent read FOnMoved write FOnMoved;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

procedure HideAllPopupPanel(ExcludeChannel: TJvDockVSChannel);
{ Disables auto-hide for ADockWindow. If ADockWindow is not auto-hidden then
  the procedures works the same as JvDockControlForm.ShowDockForm }
procedure UnAutoHideDockForm(ADockWindow: TWinControl);
function RetrieveChannel(HostDockSite: TWinControl): TJvDockVSChannel;

var
  DefaultVSChannelClass: TJvDockVSChannelClass = nil;

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
  Types, SysUtils, Math, ImgList, {AppEvnts,} JvJVCLUtils,
  JvDockSupportProc;

type
  TAnimateState = (asPopup, asHide);

  TCustomFormAccess = class(TCustomForm);
  TWinControlAccessProtected = class(TWinControl);
  TCustomControlAccessProtected = class(TCustomControl);

  { Enumerates the channels of a dock server; Ensure MoveNext returns true
    before reading Current }
  TChannelEnumerator = class
  private
    FIndex: Integer;
    FDockServer: TJvDockServer;
    function GetCurrent: TJvDockVSChannel;
  public
    constructor Create(ADockServer: TJvDockServer);
    function MoveNext: Boolean;
    property Current: TJvDockVSChannel read GetCurrent;
  end;

  TPopupPanelAnimate = class(TTimer)
  private
    FMaxWidth: Integer;
    FCurrentWidth: Integer;
    FActiveChannel: TJvDockVSChannel;
    FState: TAnimateState;
  protected
    procedure Timer; override;
    procedure OnCustomTimer(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    { Animates the popup of the form }
    procedure PopupForm(AChannel: TJvDockVSChannel; MaxWidth: Integer); virtual;
    { Animates the hiding of the form }
    procedure HideForm(AChannel: TJvDockVSChannel; MaxWidth: Integer); virtual;
  end;

var
  GlobalPopupPanelAnimate: TPopupPanelAnimate = nil;
  GlobalPopupPanelAnimateInterval: Integer = 20;
  GlobalPopupPanelAnimateMoveWidth: Integer = 20;
  GlobalPopupPanelStartAnimateInterval: Integer = 400;

//=== Local procedures =======================================================

function PopupPanelAnimate: TPopupPanelAnimate;
begin
  if GlobalPopupPanelAnimate = nil then
    GlobalPopupPanelAnimate := TPopupPanelAnimate.Create(nil);
  Result := GlobalPopupPanelAnimate;
end;

procedure ResetChannelBlockStartOffset(Channel: TJvDockVSChannel);
var
  LeftChannel: TJvDockVSChannel;
  OldOffset: Integer;
  LeftAlignArea: Integer;
begin
  LeftChannel := TJvDockVSNETPanel(Channel.DockServer.LeftDockPanel).VSChannel;
  if LeftChannel <> nil then
  begin
    LeftAlignArea := GetClientAlignControlArea(LeftChannel.Parent, alLeft);
    with TChannelEnumerator.Create(Channel.DockServer) do
    try
      while MoveNext do
        if Current.Align in [alTop, alBottom] then
        begin
          OldOffset := Current.BlockStartOffset;
          Current.BlockStartOffset := Channel.PPIScale(2) + LeftAlignArea;
          if OldOffset <> Current.BlockStartOffset then
            Current.Invalidate;
        end;
    finally
      Free;
    end;
  end;
end;

procedure SetControlBringToFront(Control: TWinControl; Align: TAlign);
var
  I: Integer;
begin
  for I := Control.ControlCount - 1 downto 0 do
    if Control.Controls[I].Visible and (Control.Controls[I].Align = Align) and
      not (Control.Controls[I] is TJvDockVSChannel) and
      not (Control.Controls[I] is TJvDockPanel) and
      not (Control.Controls[I] is TJvDockSplitter) then
      Control.Controls[I].BringToFront;
end;

function ControlIsOnPopup(AControl: TControl): Boolean;
begin
  Result := False;

  while Assigned(AControl) do
  begin
    if (AControl is TJvDockVSPopupPanel) or
      (AControl is TJvDockVSPopupPanelSplitter) or
      (AControl is TJvDockVSChannel) then
    begin
      Result := True;
      Exit;
    end;
    AControl := AControl.Parent;
  end;
end;

//=== Global procedures ======================================================

procedure HideAllPopupPanel(ExcludeChannel: TJvDockVSChannel);
var
  I: Integer;
  DockServer: TJvDockServer;
begin
  for I := 0 to JvGlobalDockManager.DockServerCount - 1 do
  begin
    DockServer := JvGlobalDockManager.DockServer[I];
    if Assigned(DockServer) then
      with TChannelEnumerator.Create(DockServer) do
      try
        while MoveNext do
          if Current <> ExcludeChannel then
            Current.HidePopupPanel(Current.PopupPane);
      finally
        Free;
      end;
  end;
end;

{ Returns the channel of a form that is docked onto a popup panel }

function RetrieveChannel(HostDockSite: TWinControl): TJvDockVSChannel;
begin
  Result := nil;
  if HostDockSite is TJvDockVSPopupPanel then
    // normal docked forms
    Result := TJvDockVSPopupPanel(HostDockSite).VSChannel
  else
  if Assigned(HostDockSite) and Assigned(HostDockSite.Parent) then
  begin
    HostDockSite := HostDockSite.Parent.HostDockSite;
    if HostDockSite is TJvDockVSPopupPanel then
      // tab docked forms
      Result := TJvDockVSPopupPanel(HostDockSite).VSChannel
  end;
end;

procedure UnAutoHideDockForm(ADockWindow: TWinControl);
var
  ADockClient: TJvDockClient;
begin
  // delegate to style
  ADockClient := FindDockClient(ADockWindow);
  if Assigned(ADockClient) and (ADockClient.DockStyle is TJvDockVSNetStyle) then
    TJvDockVSNetStyle(ADockClient.DockStyle).DoUnAutoHideDockForm(ADockWindow);
end;

//=== { TChannelEnumerator } =================================================

constructor TChannelEnumerator.Create(ADockServer: TJvDockServer);
begin
  inherited Create;
  FIndex := -1;
  FDockServer := ADockServer;
end;

function TChannelEnumerator.GetCurrent: TJvDockVSChannel;
begin
  Result := TJvDockVSNETPanel(FDockServer.DockPanelWithAlign[TAlign(FIndex)]).VSChannel;
end;

function TChannelEnumerator.MoveNext: Boolean;
var
  I: Integer;
  Panel: TJvDockPanel;
begin
  I := FIndex + 1;
  while I <= Ord(High(TAlign)) do
  begin
    Panel := FDockServer.DockPanelWithAlign[TAlign(I)];
    if (Panel is TJvDockVSNETPanel) and Assigned(TJvDockVSNETPanel(Panel).VSChannel) then
      Break;
    Inc(I);
  end;
  Result := I <= Ord(High(TAlign));
  if Result then
    FIndex := I;
end;

//=== { TJvDockVSBlock } =====================================================

constructor TJvDockVSBlock.Create(AOwner: TJvDockVSChannel);
var
  ImageListSize: Integer;
begin
  inherited Create;
  FVSChannel := AOwner;
  FVSPanes := TObjectList.Create;
  ImageListSize := PPIScale(FVSChannel.FBlockImageListSize);
  FImageList := TImageList.CreateSize(ImageListSize, ImageListSize);
  {$IFDEF RTL200_UP}
  FImageList.ColorDepth := cd32Bit;
  {$ENDIF RTL200_UP}
  FInactiveBlockWidth := FVSChannel.FInactiveBlockWidth;
  FActiveBlockWidth := 24;
end;

destructor TJvDockVSBlock.Destroy;
begin
  FImageList.Free;
  FVSPanes.Free;
  inherited Destroy;
end;

function TJvDockVSBlock.PPIScale(Value: Integer): Integer;
begin
  Result := VSChannel.PPIScale(Value);
end;

procedure TJvDockVSBlock.AddDockControl(Control: TWinControl);
var
  I, PaneWidth, FirstIndex: Integer;

  function GetPaneWidth: Integer;
  begin
    Result := PPIScale(100);
    if Control = nil then
      Exit;
    case VSChannel.Align of
      alLeft, alRight:
        Result := Control.Width;
      alTop, alBottom:
        Result := Control.Height;
    end;
  end;

var
  NewPane: TJvDockVSPane;
  Form: TCustomForm;
  APageControl: TJvDockTabPageControl;
begin
  PaneWidth := GetPaneWidth;
  if Control is TJvDockTabHostForm then
  begin
    FBlockType := btTabBlock;
    APageControl := TJvDockTabHostForm(Control).PageControl;
    FirstIndex := VSPaneCount;
    { Mantis 3989: (Kiriakos) PageControl.DockClients does NOT have to be in the
      same order as PageControl.Pages; for example, if we reorder the pages. }
    for I := 0 to APageControl.Count - 1 do
    begin
      Form := APageControl.DockForm[I];
      if Assigned(Form) then
      begin
        NewPane := AddPane(Form, PaneWidth);
        TJvDockVSNETTabSheet(APageControl.Pages[I]).OldVisible := Form.Visible;
        if APageControl.Pages[I] <> APageControl.ActivePage then
          Form.Visible := False
        else if Assigned(NewPane) then
          ActivePane := NewPane;
      end;
    end;
    if not Assigned(ActivePane) then
      UpdateActivePane(FirstIndex);
  end
  else
  begin
    FBlockType := btConjoinBlock;
    NewPane := AddPane(Control, PaneWidth);
    if Assigned(NewPane) then
      ActivePane := NewPane;
  end;
  ResetActiveBlockWidth;
end;

function TJvDockVSBlock.AddPane(AControl: TControl; const AWidth: Integer): TJvDockVSPane;
const
  ANDbits: array[0..2*16-1] of  Byte = ($FF,$FF,
                                        $FF,$FF,
                                        $FF,$FF,
                                        $FF,$FF,
                                        $FF,$FF,
                                        $FF,$FF,
                                        $FF,$FF,
                                        $FF,$FF,
                                        $FF,$FF,
                                        $FF,$FF,
                                        $FF,$FF,
                                        $FF,$FF,
                                        $FF,$FF,
                                        $FF,$FF,
                                        $FF,$FF,
                                        $FF,$FF);
  XORbits: array[0..2*16-1] of  Byte = ($00,$00,
                                        $00,$00,
                                        $00,$00,
                                        $00,$00,
                                        $00,$00,
                                        $00,$00,
                                        $00,$00,
                                        $00,$00,
                                        $00,$00,
                                        $00,$00,
                                        $00,$00,
                                        $00,$00,
                                        $00,$00,
                                        $00,$00,
                                        $00,$00,
                                        $00,$00);
var
  Icon: TIcon;
  ADockClient: TJvDockClient;
begin
  if not (AControl is TCustomForm) then
  begin
    Result := nil;
    Exit;
  end;

  Result := TJvDockVSPane.Create(Self, TCustomForm(AControl), AWidth, VSPaneCount);
  FVSPanes.Add(Result);
  if not JvGlobalDockIsLoading then
  begin
    ADockClient := FindDockClient(AControl);
    if ADockClient <> nil then
      ADockClient.VSPaneWidth := AWidth;
  end;
  { Add the form icon }

  if not Assigned(TCustomFormAccess(AControl).Icon) or not TCustomFormAccess(AControl).Icon.HandleAllocated then
  begin
    Icon := TIcon.Create;
    try
      Icon.Width := PPIScale(16);
      Icon.Height := PPIScale(16);
      //2. Adding an Icon without real bitmap does nothing,
      //so transparent icon needed
      Icon.Handle := CreateIcon(hInstance,16,16,1,1,@ANDbits,@XORbits);
      FImageList.AddIcon(Icon);
    finally
      Icon.Free;
    end;
  end
  else
    FImageList.AddIcon(TCustomFormAccess(AControl).Icon);
end;

procedure TJvDockVSBlock.DeletePane(Index: Integer);
var
  I: Integer;
  ActivePaneRemoved: Boolean;
begin
  for I := Index to VSPaneCount - 2 do
    VSPane[I + 1].FIndex := VSPane[I].FIndex;
  ActivePaneRemoved := VSPane[Index] = Self.ActivePane;
  FVSPanes.Delete(Index);
  { Remove the form icon }
  if Index < FImageList.Count then
    FImageList.Delete(Index);
  if ActivePaneRemoved then
    UpdateActivePane(Index);
end;

function TJvDockVSBlock.FindDockControl(Control: TWinControl;
  var PaneIndex: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  PaneIndex := -1;
  if Control = nil then
    Exit;
  for I := 0 to VSPaneCount - 1 do
    if VSPane[I].FDockForm = Control then
    begin
      PaneIndex := I;
      Result := True;
      Exit;
    end;
  if FBlockType = btTabBlock then
  begin
    if (VSPaneCount > 0) and (VSPane[0].FDockForm.HostDockSite.Parent = Control) then
    begin
      PaneIndex := 0;
      Result := True;
      Exit;
    end;
  end;
end;

function TJvDockVSBlock.GetActiveBlockWidth: Integer;
begin
  Result := PPIScale(FActiveBlockWidth);
end;

function TJvDockVSBlock.GetActiveDockControl: TWinControl;
begin
  if Assigned(ActivePane) then
    Result := ActivePane.DockForm
  else
    Result := nil;
end;

function TJvDockVSBlock.GetInactiveBlockWidth: Integer;
begin
  Result := PPIScale(FInactiveBlockWidth);
end;

function TJvDockVSBlock.GetTotalWidth: Integer;
begin
  // 1 pane is active, the rest is inactive
  Result := (VSPaneCount - 1) * InactiveBlockWidth + ActiveBlockWidth;
end;

function TJvDockVSBlock.GetVSPane(Index: Integer): TJvDockVSPane;
begin
  Result := TJvDockVSPane(FVSPanes[Index]);
end;

function TJvDockVSBlock.GetVSPaneCount: Integer;
begin
  Result := FVSPanes.Count;
end;

procedure TJvDockVSBlock.RemoveDockControl(Control: TWinControl);
begin
  ResetActiveBlockWidth;
end;

procedure TJvDockVSBlock.ResetActiveBlockWidth;
{
  FActiveBlockWidth stores the unscaled value
  Here it is calculating with the CurrentPPI settings and
  needs to be unscaled.
}
  function PPIUnScale(Value: Integer): Integer;
  begin
    Result := MulDiv(Value, FInactiveBlockWidth, InactiveBlockWidth);
  end;

var
  I: Integer;
  TextWidth: Integer;
  Canvas: TCanvas;
begin
  FActiveBlockWidth := 0;

  if VSPaneCount > 0 then
  begin
    if VSChannel.Parent is TCustomControl then
      Canvas := TCustomControlAccessProtected(VSChannel.Parent).Canvas
    else if VSChannel.Parent is TCustomForm then
      Canvas := TForm(VSChannel.Parent).Canvas
    else
      Canvas := nil;

    if Canvas <> nil then
    begin
      for I := 0 to VSPaneCount - 1 do
      begin
        TextWidth := Canvas.TextWidth(VSPane[I].FDockForm.Caption) +
          InactiveBlockWidth + PPIScale(18);
        if TextWidth >= VSChannel.ActivePaneSize then
        begin
          FActiveBlockWidth := VSChannel.FActivePaneSize; //unscaled!
          Exit;
        end;

        FActiveBlockWidth := Max(FActiveBlockWidth, TextWidth);
      end;
    end;
  end;

  if FActiveBlockWidth = 0 then
    FActiveBlockWidth := VSChannel.ActivePaneSize;
  FActiveBlockWidth := PPIUnScale(FActiveBlockWidth); //unscale
end;

procedure TJvDockVSBlock.SetActivePane(APane: TJvDockVSPane);
begin
  if FActivePane <> APane then
  begin
    FActivePane := APane;
    VSChannel.Invalidate;
  end;
end;

procedure TJvDockVSBlock.UpdateActivePane(StartIndex: Integer);
var
  I: Integer;
begin
  { Start looking at position StartIndex for a visible pane }
  for I := 0 to VSPaneCount - 1 do
    if VSPane[(I + StartIndex) mod VSPaneCount].FVisible then
    begin
      ActivePane := VSPane[(I + StartIndex) mod VSPaneCount];
      Break;
    end;
end;

//=== { TJvDockVSChannel } ===================================================

constructor TJvDockVSChannel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStyleLink := TJvDockStyleLink.Create;
  FBlocks := TObjectList.Create;
  FActivePaneSize := MaxActivePaneWidth;
  FTabColor := clBtnFace;
  FChannelWidth := 22;
  FBlockStartOffset := 2;
  FBlockUpOffset := 2;
  FBlockInterval := 13;
  FBlockImageListSize := 16;
  FInactiveBlockWidth := 24;
  if AOwner is TJvDockVSNETPanel then
  begin
    FVSNETDockPanel := TJvDockVSNETPanel(AOwner);
    { First set DockStyle then OnStyleChanged so no OnStyleChanged is fired;
      we do it ourself in AfterContruction }
    FStyleLink.DockStyle := DockServer.DockStyle;
  end;
  FStyleLink.OnStyleChanged := DockStyleChanged;
  Color := VSNETPageInactiveSheetColor;
  ParentFont := True;
end;

destructor TJvDockVSChannel.Destroy;
begin
  if Assigned(GlobalPopupPanelAnimate) and (GlobalPopupPanelAnimate.FActiveChannel = Self) then
  begin
    GlobalPopupPanelAnimate.Free;
    GlobalPopupPanelAnimate := nil;
  end;
  FreeBlockList;
  FAnimationDelayTimer.Free;
  FStyleLink.Free;
  inherited Destroy;
end;

procedure TJvDockVSChannel.AddDockControl(Control: TWinControl);
var
  ABlock: TJvDockVSBlock;
begin
  if Control is TJvDockTabHostForm then
  begin
    ABlock := TJvDockVSBlock.Create(Self);
    ABlock.AddDockControl(Control);
    FBlocks.Add(ABlock);
  end
  else
  begin
    if (BlockCount >= 1) and (Block[0].BlockType = btConjoinBlock) then
      Block[0].AddDockControl(Control)
    else
    begin
      ABlock := TJvDockVSBlock.Create(Self);
      ABlock.AddDockControl(Control);
      FBlocks.Insert(0, ABlock);
    end;
  end;
  HideAllPopupPanel(Self);
  ResetPosition;
  Invalidate;
end;

procedure TJvDockVSChannel.AfterConstruction;
begin
  inherited AfterConstruction;
  FStyleLink.StyleChanged;
end;

procedure TJvDockVSChannel.AutoFocusActiveDockForm;
begin
  if DockServer.AutoFocusDockedForm and Assigned(ActiveDockForm) and ActiveDockForm.CanFocus then
  begin
    ActiveDockForm.SetFocus;
    {$IFNDEF COMPILER9_UP}
    InvalidateDockHostSiteOfControl(ActiveDockForm, False);
    {$ENDIF !COMPILER9_UP}
  end;
end;

procedure TJvDockVSChannel.ChangeScale(M, D: Integer; isDpiChange: Boolean);
Var
  I, J : integer;
begin
  inherited;
  for I := 0 to FBlocks.Count - 1 do
  begin
    JvScaleImageList(Block[I].FImageList, M, D);
    for J := 0 to Block[I].VSPaneCount - 1 do
      Block[I].VSPane[J].PopUpPanelWidth := MulDiv(Block[I].VSPane[J].PopUpPanelWidth, M, D);
  end;
  ResetPosition;
  Invalidate;
end;

procedure TJvDockVSChannel.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
end;

procedure TJvDockVSChannel.CreateVSPopupPanel;
begin
  FVSPopupPanel := TJvDockVSPopupPanel.Create(Parent, FVSNETDockPanel);

  { Channel is maintainer/Creator }
  FVSPopupPanel.Name := FVSNETDockPanel.Name + '_PopupPanel';
  FVSPopupPanel.Visible := False;
  if Parent is TCustomForm then
  begin
    FVSPopupPanel.Parent := Parent;
    FVSPopupPanel.Align := alNone;
    FVSPopupPanel.BringToFront;
  end;
  FVSPopupPanel.FreeNotification(Self);
  FVSPopupPanelSplitter := TJvDockVSPopupPanelSplitter.Create(Parent);
  { Channel is maintainer/Creator }
  FVSPopupPanelSplitter.FreeNotification(Self);
  if Parent is TCustomForm then
  begin
    FVSPopupPanelSplitter.Parent := Parent;
    FVSPopupPanelSplitter.Align := alNone;
    FVSPopupPanelSplitter.VSPopupPanel := VSPopupPanel;
    FVSPopupPanelSplitter.Color := clBtnFace;
    FVSPopupPanelSplitter.Visible := False;
  end;
end;

procedure TJvDockVSChannel.DeleteBlock(Index: Integer);
begin
  FBlocks.Delete(Index);
end;

procedure TJvDockVSChannel.DestroyVSPopupPanel;
begin
  FreeAndNil(FVSPopupPanel);
  FreeAndNil(FVSPopupPanelSplitter);
end;

procedure TJvDockVSChannel.DoAnimationDelay(Sender: TObject);
var
  P: TPoint;
begin
  try
    // Show the form only if the cursor is still above the same pane
    GetCursorPos(P);
    if PaneAtPos(ScreenToClient(P)) = FDelayPane then
      PopupDockForm(FDelayPane);
  finally
    // dangerous to free in handler?
    FAnimationDelayTimer.Free;
    FAnimationDelayTimer := nil;
  end;
end;

procedure TJvDockVSChannel.DockStyleChanged(Sender: TObject);
begin
  SyncWithStyle;
end;

function TJvDockVSChannel.PPIScale(Value: Integer): Integer;
begin
  if FVSNETDockPanel <> nil then
    Result := FVSNETDockPanel.PPIScale(Value)
  else
    Result := MulDiv(Value, FCurrentPPI, 96);
end;

function TJvDockVSChannel.FindDockControl(Control: TWinControl;
  var BlockIndex: Integer; var PaneIndex: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  BlockIndex := -1;
  PaneIndex := -1;
  if Control = nil then
    Exit;

  for I := 0 to BlockCount - 1 do
    if Block[I].FindDockControl(Control, PaneIndex) then
    begin
      BlockIndex := I;
      Result := True;
      Exit;
    end;
end;

function TJvDockVSChannel.FindPane(Control: TWinControl): TJvDockVSPane;
var
  I, J: Integer;
begin
  Result := nil;
  if FindDockControl(Control, I, J) then
    Result := Block[I].VSPane[J];
end;

procedure TJvDockVSChannel.FreeBlockList;
begin
  FreeAndNil(FBlocks);
end;

function TJvDockVSChannel.GetActiveDockForm: TCustomForm;
begin
  if PopupPane <> nil then
    Result := PopupPane.DockForm
  else
    Result := nil;
end;

function TJvDockVSChannel.GetActivePaneSize: Integer;
begin
  Result := PPIScale(FActivePaneSize);
end;

function TJvDockVSChannel.GetBlock(Index: Integer): TJvDockVSBlock;
begin
  Result := TJvDockVSBlock(FBlocks[Index]);
end;

function TJvDockVSChannel.GetBlockCount: Integer;
begin
  Result := FBlocks.Count;
end;

function TJvDockVSChannel.GetBlockInterval: Integer;
begin
  Result := PPIScale(FBlockInterval);
end;

procedure TJvDockVSChannel.GetBlockRect(Block: TJvDockVSBlock; Index: Integer;
  var ARect: TRect);
var
  BlockWidth: Integer;
begin
  if Block.VSPane[Index] <> Block.ActivePane then
    BlockWidth := Block.InactiveBlockWidth
  else
    BlockWidth := Block.ActiveBlockWidth;

  case Align of
    alLeft:
      begin
        ARect.Left := 0 {-PPIScale(1)};
        ARect.Top := FCurrentPos;
        ARect.Right := Width - BlockUpOffset;
        ARect.Bottom := ARect.Top + BlockWidth;
      end;
    alRight:
      begin
        ARect.Left := BlockUpOffset;
        ARect.Top := FCurrentPos;
        ARect.Right := Width {+ PPIScale(1)};
        ARect.Bottom := ARect.Top + BlockWidth;
      end;
    alTop:
      begin
        ARect.Left := FCurrentPos;
        ARect.Top := 0 {-PPIScale(1)};
        ARect.Right := ARect.Left + BlockWidth;
        ARect.Bottom := Height - BlockUpOffset;
      end;
    alBottom:
      begin
        ARect.Left := FCurrentPos;
        ARect.Top := BlockUpOffset;
        ARect.Right := ARect.Left + BlockWidth;
        ARect.Bottom := Height{ + PPIScale(1)};
      end;
  end;

  Inc(FCurrentPos, BlockWidth {- PPIScale(1)});
end;

function TJvDockVSChannel.GetBlockStartOffset: Integer;
begin
  Result := PPIScale(FBlockStartOffset);
end;

function TJvDockVSChannel.GetBlockUpOffset: Integer;
begin
  Result := PPIScale(FBlockUpOffset)
end;

function TJvDockVSChannel.GetChannelWidth: Integer;
begin
  Result := PPIScale(FChannelWidth);
end;

function TJvDockVSChannel.GetDockServer: TJvDockServer;
begin
  if Assigned(FVSNETDockPanel) then
    Result := FVSNETDockPanel.DockServer
  else
    Result := nil;
end;

function TJvDockVSChannel.GetDockStyle: TJvDockObservableStyle;
begin
  Result := FStyleLink.DockStyle;
end;

function TJvDockVSChannel.GetPaneWithControl(AControl: TControl): TJvDockVSPane;
var
  I, J: Integer;
begin
  Result := nil;
  for I := 0 to BlockCount - 1 do
    for J := 0 to Block[I].VSPaneCount - 1 do
      if AControl = Block[I].VSPane[J].FDockForm then
      begin
        Result := Block[I].VSPane[J];
        Exit;
      end;
end;

procedure TJvDockVSChannel.HidePopupPanel(Pane: TJvDockVSPane);
begin
  if Pane <> nil then
  begin
    if Align in [alLeft, alRight] then
    begin
      VSPopupPanel.Width := 0;
      VSPopupPanelSplitter.Width := 0;
    end
    else
    if Align in [alTop, alBottom] then
    begin
      VSPopupPanel.Height := 0;
      VSPopupPanelSplitter.Height := 0;
    end;
    SetPopupPane(nil);
  end;
  VSPopupPanel.Visible := False;
  VSPopupPanelSplitter.Visible := False;
  SetPopupPane(nil);
end;

procedure TJvDockVSChannel.HidePopupPanel(Control: TWinControl);
var
  Pane: TJvDockVSPane;
begin
  Pane := FindPane(Control);
  if Assigned(Pane) then
    HidePopupPanel(Pane);
end;

procedure TJvDockVSChannel.HidePopupPanelWithAnimate;
begin
  if PopupPane <> nil then
    PopupPanelAnimate.HideForm(Self, PopupPane.FWidth);
end;

procedure TJvDockVSChannel.InternalInsertControl(AWinControl: TWinControl);
begin
  if Assigned(AWinControl) then
  begin
    if Assigned(VSPopupPanel) and VSPopupPanel.UseDockManager and (VSPopupPanel.JvDockManager <> nil) then
      VSPopupPanel.JvDockManager.InsertControl(AWinControl, alNone, nil);
    AWinControl.FreeNotification(Self);
  end;
end;

procedure TJvDockVSChannel.InternalRemoveControl(AWinControl: TWinControl);
begin
  if Assigned(AWinControl) then
  begin
    AWinControl.RemoveFreeNotification(Self);
    if Assigned(VSPopupPanel) and VSPopupPanel.UseDockManager and (VSPopupPanel.JvDockManager <> nil) then
      VSPopupPanel.JvDockManager.RemoveControl(AWinControl);
  end;
end;

procedure TJvDockVSChannel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Pane: TJvDockVSPane;
begin
  inherited MouseDown(Button, Shift, X, Y);
  Pane := PaneAtPos(Point(X, Y));
  if Assigned(Pane) then
  begin
    if PopupPane = Pane then
    begin
      if Pane.DockForm.CanFocus then
        Pane.DockForm.SetFocus;
    end
    else
      PopupDockForm(Pane);
  end;
end;

procedure TJvDockVSChannel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewDelayPane: TJvDockVSPane;
begin
  inherited MouseMove(Shift, X, Y);

  NewDelayPane := PaneAtPos(Point(X, Y));
  if Assigned(NewDelayPane) and (NewDelayPane <> PopupPane) and IsForegroundTask then
  begin
    // Create the timer object if not existing
    if FAnimationDelayTimer = nil then
    begin
      FAnimationDelayTimer := TTimer.Create(nil);
      FAnimationDelayTimer.OnTimer := DoAnimationDelay;
      FAnimationDelayTimer.Interval := TJvDockVSNetStyle.GetAnimationStartInterval;
      FAnimationDelayTimer.Enabled := True;
    end
    // Restart the timer only, if mouse is above another pane now
    else
    if NewDelayPane <> FDelayPane then
    begin
      FAnimationDelayTimer.Enabled := False;
      FAnimationDelayTimer.Enabled := True;
    end;
  end
  else
    FreeAndNil(FAnimationDelayTimer);

  FDelayPane := NewDelayPane;
end;

procedure TJvDockVSChannel.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvDockVSChannel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FVSPopupPanel then
    begin
      FVSPopupPanel := nil;
      DestroyVSPopupPanel;
    end
    else
    if AComponent = FVSPopupPanelSplitter then
    begin
      FVSPopupPanelSplitter := nil;
      DestroyVSPopupPanel;
    end
    else
    if AComponent is TWinControl then
      InternalRemoveControl(TWinControl(AComponent));
  end;
end;

procedure TJvDockVSChannel.Paint;
var
  I: Integer;

  procedure DrawSingleBlock(Block: TJvDockVSBlock);
  var
    DrawRect: TRect;
    I: Integer;
    OldGraphicsMode: Integer;
    VisiblePaneCount: Integer;

    procedure AdjustImagePos;
    begin
      if Align = alLeft then
      begin
        Inc(DrawRect.Left, PPIScale(3));
        Inc(DrawRect.Top, PPIScale(4));
      end
      else
      if Align = alTop then
      begin
        Inc(DrawRect.Left, PPIScale(4));
        Inc(DrawRect.Top, PPIScale(2));
      end
      else
      if Align = alRight then
      begin
        Inc(DrawRect.Left, PPIScale(4));
        Inc(DrawRect.Top, PPIScale(4));
      end
      else
      if Align = alBottom then
      begin
        Inc(DrawRect.Left, PPIScale(4));
        Inc(DrawRect.Top, PPIScale(3));
      end;
    end;

  begin
    VisiblePaneCount := 0;
    for I := 0 to Block.VSPaneCount - 1 do
    begin
      if not Block.VSPane[I].FVisible then
        Continue;

      GetBlockRect(Block, I, DrawRect);
      Canvas.Brush.Color := TabColor;
      Canvas.FillRect(DrawRect);
      Canvas.Brush.Color := clGray;
      Canvas.FrameRect(DrawRect);

      AdjustImagePos;
      Block.FImageList.Draw(Canvas, DrawRect.Left, DrawRect.Top, I, dsTransparent, itImage);

      if Block.ActivePane = Block.VSPane[I] then
      begin
        if Align in [alTop, alBottom] then
          Inc(DrawRect.Left, Block.InactiveBlockWidth)
        else
        if Align in [alLeft, alRight] then
        begin
          Inc(DrawRect.Top, Block.InactiveBlockWidth);
          if Align = alLeft then
            DrawRect.Left := PPIScale(15)
          else
            DrawRect.Left := PPIScale(20);
          DrawRect.Right := DrawRect.Left + (DrawRect.Bottom - DrawRect.Top);
        end;
        Canvas.Brush.Color := TabColor;
        Canvas.Pen.Color := clBlack;

        Dec(DrawRect.Right, PPIScale(3));

        OldGraphicsMode := SetGraphicsMode(Canvas.Handle, GM_ADVANCED);
        Canvas.Brush.Style := bsClear;

        // Scale Font
        Canvas.Font.Height := MulDiv(Canvas.Font.Height, FCurrentPPI, Canvas.Font.PixelsPerInch);
        DrawText(Canvas.Handle, PChar(Block.VSPane[I].FDockForm.Caption), -1, DrawRect, DT_END_ELLIPSIS or DT_NOCLIP);
        SetGraphicsMode(Canvas.Handle, OldGraphicsMode);
      end;
      Inc(VisiblePaneCount);
    end;
    if VisiblePaneCount > 0 then
      Inc(FCurrentPos, BlockInterval);
  end;

begin
  inherited Paint;

  FCurrentPos := BlockStartOffset;
  for I := 0 to BlockCount - 1 do
    DrawSingleBlock(Block[I]);
end;

function TJvDockVSChannel.PaneAtPos(MousePos: TPoint): TJvDockVSPane;
var
  I, J: Integer;
  ARect: TRect;
begin
  Result := nil;
  FCurrentPos := BlockStartOffset;
  for I := 0 to BlockCount - 1 do
  begin
    for J := 0 to Block[I].VSPaneCount - 1 do
    begin
      if not Block[I].VSPane[J].FVisible then
        Continue;
      GetBlockRect(Block[I], J, ARect);
      if PtInRect(ARect, MousePos) then
      begin
        Result := Block[I].VSPane[J];
        Exit;
      end;
    end;
    Inc(FCurrentPos, BlockInterval);
  end;
end;

procedure TJvDockVSChannel.PopupDockForm(Pane: TJvDockVSPane);

  procedure SetSingleDockFormVisible(HostDockSite: TWinControl; AForm: TCustomForm);
  var
    I: Integer;
  begin
    AForm.Visible := True;
    for I := 0 to HostDockSite.DockClientCount - 1 do
      if AForm <> HostDockSite.DockClients[I] then
        HostDockSite.DockClients[I].Visible := False;
  end;

begin
  if (Pane = nil) or (PopupPane = Pane) then
    Exit;

  HidePopupPanel(PopupPane);
  { !! Setting visible to true here is too early and causes Align problems.
       Visibility is anyway set by FVSPopupPanel.JvDockManager.ShowSingleControl
       call.
  }
  //    Pane.FDockForm.Visible := True;
  PopupPanelAnimate.PopupForm(Self, Pane.FWidth);
  if (Pane.FDockForm <> nil) and (Pane.FDockForm.HostDockSite.Parent is TJvDockTabHostForm) then
  begin
    // Popup is shown, but the dockform is on a pagecontrol with multiple
    // tabs. We hide the other tabs.
    SetSingleDockFormVisible(Pane.FDockForm.HostDockSite, Pane.FDockForm);
    TJvDockTabHostForm(Pane.FDockForm.HostDockSite.Parent).Caption := Pane.FDockForm.Caption;
    // Make the pagecontrol the only visible control.
    FVSPopupPanel.JvDockManager.ShowSingleControl(Pane.FDockForm.HostDockSite.Parent);
  end
  else
    FVSPopupPanel.JvDockManager.ShowSingleControl(Pane.FDockForm);
  SetPopupPane(Pane);
  FVSPopupPanel.JvDockManager.ResetBounds(True);
end;

procedure TJvDockVSChannel.PopupDockForm(Control: TWinControl);
var
  Pane: TJvDockVSPane;
begin
  Pane := FindPane(Control);
  if Assigned(Pane) then
    PopupDockForm(Pane);
end;

procedure TJvDockVSChannel.PopupPaneChanged;
begin
  { Notification }
end;

procedure TJvDockVSChannel.RemoveAllBlock;
var
  I: Integer;
begin
  for I := BlockCount - 1 downto 0 do
    DeleteBlock(I);
end;

procedure TJvDockVSChannel.RemoveDockControl(Control: TWinControl);
var
  BlockIndex, PaneIndex: Integer;
begin
  VSPopupPanel.Visible := False;
  if FindDockControl(Control, BlockIndex, PaneIndex) then
  begin
    Block[BlockIndex].DeletePane(PaneIndex);
    if (Block[BlockIndex].VSPaneCount <= 0) or (Block[BlockIndex].FBlockType = btTabBlock) then
      DeleteBlock(BlockIndex);
  end;
  ResetPosition;
  Invalidate;
end;

procedure TJvDockVSChannel.ResetActivePaneWidth;
var
  DockClient: TJvDockClient;
begin
  if PopupPane = nil then
    Exit;

  DockClient := FindDockClient(PopupPane.DockForm);
  if Align in [alLeft, alRight] then
    PopupPane.FWidth := VSPopupPanel.Width
  else
  if Align in [alTop, alBottom] then
    PopupPane.FWidth := VSPopupPanel.Height + VSPopupPanel.JvDockManager.GrabberSize;
  if DockClient <> nil then
    DockClient.VSPaneWidth := PopupPane.FWidth;
end;

procedure TJvDockVSChannel.ResetBlock;
var
  I: Integer;
begin
  if BlockCount > 0 then
  begin
    Block[0].FBlockStartPos := BlockStartOffset;
    for I := 1 to BlockCount - 1 do
      Block[I].FBlockStartPos := Block[I - 1].FBlockStartPos + Block[I - 1].GetTotalWidth + BlockInterval;
  end;
end;

procedure TJvDockVSChannel.ResetFontAngle;
var
  LogFont: TLogFont;
begin
  if Align in [alLeft, alRight] then
    if GetObject(Canvas.Font.Handle, SizeOf(LogFont), @LogFont) <> 0 then
    begin
      LogFont.lfEscapement := 2700;
      LogFont.lfOrientation := 2700;
      Canvas.Font.Handle := CreateFontIndirect(LogFont);
    end;
end;

procedure TJvDockVSChannel.ResetPopupPanelHeight;
begin
  if Align in [alLeft, alRight] then
  begin
    VSPopupPanel.Top := Top;
    VSPopupPanel.Height := Height;
    VSPopupPanelSplitter.Top := Top;
    VSPopupPanelSplitter.Height := Height;
  end;
end;

procedure TJvDockVSChannel.ResetPosition;
var
  I, J: Integer;
  PaneCount: Integer;
begin
  PaneCount := 0;
  for I := 0 to BlockCount - 1 do
    for J := 0 to Block[I].VSPaneCount - 1 do
      if Block[I].VSPane[J].FVisible then
        Inc(PaneCount);

  Visible := PaneCount > 0;
  case Align of
    alLeft:
      begin
        Width := ChannelWidth;
        Left := GetClientAlignControlArea(Parent, Align, Self);
      end;
    alRight:
      begin
        Width := ChannelWidth;
        Left := Parent.ClientWidth - GetClientAlignControlArea(Parent, Align, Self) - ChannelWidth + PPIScale(1);
      end;
    alTop:
      begin
        Height := ChannelWidth;
        Top := GetClientAlignControlArea(Parent, Align, Self);
      end;
    alBottom:
      begin
        Height := ChannelWidth;
        Top := Parent.ClientHeight - GetClientAlignControlArea(Parent, Align, Self) - ChannelWidth + PPIScale(1);
      end;
  end;
end;

procedure TJvDockVSChannel.SetActivePaneSize(const Value: Integer);
begin
  if FActivePaneSize <> Value then
  begin
    FActivePaneSize := Value;
    Invalidate;
  end;
end;

procedure TJvDockVSChannel.SetBlockStartOffset(const Value: Integer);
begin
  FBlockStartOffset := Value;
end;

procedure TJvDockVSChannel.SetPopupPane(APane: TJvDockVSPane);
begin
  if APane <> FPopupPane then
  begin
    FPopupPane := APane;
    { If a pane has a visible popup dock form, then it becomes the active pane of
      the block }
    if Assigned(FPopupPane) then
      FPopupPane.FBlock.ActivePane := FPopupPane;
    PopupPaneChanged;
  end;
end;

procedure TJvDockVSChannel.SetTabColor(const Value: TColor);
begin
  if FTabColor <> Value then
  begin
    FTabColor := Value;
    Invalidate;
  end;
end;

procedure TJvDockVSChannel.SetVSPopupPanelSplitterPosition;
begin
  case Align of
    alLeft:
      VSPopupPanelSplitter.SetBounds(VSPopupPanel.Left + VSPopupPanel.Width,
        VSPopupPanel.Top,
        VSPopupPanelSplitter.SplitWidth,
        VSPopupPanel.Height);
    alRight:
      VSPopupPanelSplitter.SetBounds(VSPopupPanel.Left - VSPopupPanelSplitter.SplitWidth,
        VSPopupPanel.Top,
        VSPopupPanelSplitter.SplitWidth,
        VSPopupPanel.Height);
    alTop:
      VSPopupPanelSplitter.SetBounds(VSPopupPanel.Left,
        VSPopupPanel.Top + VSPopupPanel.Height,
        VSPopupPanel.Width,
        VSPopupPanelSplitter.SplitWidth);
    alBottom:
      VSPopupPanelSplitter.SetBounds(VSPopupPanel.Left,
        VSPopupPanel.Top - VSPopupPanelSplitter.SplitWidth,
        VSPopupPanel.Width,
        VSPopupPanelSplitter.SplitWidth);
  end;
  VSPopupPanelSplitter.Visible := True;
  VSPopupPanelSplitter.BringToFront;
end;

procedure TJvDockVSChannel.ShowPopupPanel(Pane: TJvDockVSPane);

  procedure SetSingleDockFormVisible(HostDockSite: TWinControl; AForm: TCustomForm);
  var
    I: Integer;
  begin
    for I := 0 to HostDockSite.DockClientCount - 1 do
      HostDockSite.DockClients[I].Visible := AForm = HostDockSite.DockClients[I];
  end;
var
  LShowControl: TWinControl;
begin
  if Pane = nil then
    Exit;

  JvDockLockWindow(nil);
  Parent.DisableAlign;
  try
    { Auto-hide all popups of this pane }
    HidePopupPanel(PopupPane);
    Pane.FDockForm.Visible := True;
    if (Pane.FDockForm <> nil) and (Pane.FDockForm.HostDockSite.Parent is TJvDockTabHostForm) then
    begin
      FVSPopupPanel.JvDockManager.ShowSingleControl(Pane.FDockForm.HostDockSite.Parent);
      SetSingleDockFormVisible(Pane.FDockForm.HostDockSite, Pane.FDockForm);
      TJvDockTabHostForm(Pane.FDockForm.HostDockSite.Parent).Caption := Pane.FDockForm.Caption;
    end
    else
      FVSPopupPanel.JvDockManager.ShowSingleControl(Pane.FDockForm);

    SetPopupPane(Pane);
    FVSPopupPanel.JvDockManager.ResetBounds(True);

    VSPopupPanel.BringToFront;
    VSPopupPanelSplitter.BringToFront;
    SetControlBringToFront(Parent, Align);
    BringToFront;
    case Align of
      alLeft:
        begin
          VSPopupPanel.SetBounds(Left + Width,
            Top,
            Pane.FWidth,
            Height);
          VSPopupPanelSplitter.SetBounds(VSPopupPanel.Left + VSPopupPanel.Width,
            Top,
            VSPopupPanelSplitter.SplitWidth,
            Height);
        end;
      alRight:
        begin
          VSPopupPanel.SetBounds(Left - Pane.FWidth,
            Top,
            Pane.FWidth,
            Height);
          VSPopupPanelSplitter.SetBounds(VSPopupPanel.Left - VSPopupPanelSplitter.SplitWidth,
            Top,
            VSPopupPanelSplitter.SplitWidth,
            Height);
        end;
      alTop:
        begin
          VSPopupPanel.SetBounds(Left,
            Top + Height,
            Width,
            Pane.FWidth);
          VSPopupPanelSplitter.SetBounds(Left,
            VSPopupPanel.Top + VSPopupPanel.Height,
            Width,
            VSPopupPanelSplitter.SplitWidth);
        end;
      alBottom:
        begin
          VSPopupPanel.SetBounds(Left,
            Top - Pane.FWidth,
            Width,
            Pane.FWidth);
          VSPopupPanelSplitter.SetBounds(Left,
            VSPopupPanel.Top - VSPopupPanelSplitter.SplitWidth,
            Width,
            VSPopupPanelSplitter.SplitWidth);
        end;
    end;
    VSPopupPanel.Visible := True;
    VSPopupPanelSplitter.Visible := True;

    { If the form is on a tab, then show the parent of the pagecontrol
      (a TJvDockTabHostForm), otherwise show the form.
    }
    LShowControl := nil;
    case Pane.FBlock.BlockType of
      btTabBlock:
        if Pane.FDockForm.Parent is TJvDockTabSheet then
        begin
          LShowControl := TJvDockTabSheet(Pane.FDockForm.Parent).PageControl;
          if Assigned(LShowControl) then
            LShowControl := LShowControl.Parent;
        end;
      btConjoinBlock: LShowControl := Pane.FDockForm;
    end;
    if Assigned(LShowControl) then
      FVSPopupPanel.DoShowControl(LShowControl);

    AutoFocusActiveDockForm;
  finally
    Parent.EnableAlign;
    JvDockUnLockWindow;
  end;
end;

procedure TJvDockVSChannel.ShowPopupPanel(Control: TWinControl);
var
  Pane: TJvDockVSPane;
begin
  Pane := FindPane(Control);
  if Assigned(Pane) then
    ShowPopupPanel(Pane);
end;

procedure TJvDockVSChannel.SyncWithStyle;
begin
  if DockStyle is TJvDockVSNetStyle then
  begin
    ActivePaneSize := TJvDockVSNetStyle(DockStyle).ChannelOption.ActivePaneSize;
    TabColor := TJvDockVSNetStyle(DockStyle).ChannelOption.TabColor;
  end;
end;

//=== { TJvDockVSNETChannelOption } ==========================================

constructor TJvDockVSNETChannelOption.Create(ADockStyle: TJvDockObservableStyle);
begin
  inherited Create(ADockStyle);
  FActivePaneSize := 100;
  FShowImage := True;
  FMouseleaveHide := True;
  FHideHoldTime := 1000;
  FTabColor := clBtnFace;
end;

procedure TJvDockVSNETChannelOption.Assign(Source: TPersistent);
var
  Src: TJvDockVSNETChannelOption;
begin
  if Source is TJvDockVSNETChannelOption then
  begin
    BeginUpdate;
    try
      Src := TJvDockVSNETChannelOption(Source);

      ActivePaneSize := Src.ActivePaneSize;
      ShowImage := Src.ShowImage;
      MouseleaveHide := Src.MouseleaveHide;
      HideHoldTime := Src.HideHoldTime;
      TabColor := Src.TabColor;

      inherited Assign(Source);
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TJvDockVSNETChannelOption.SetActivePaneSize(Value: Integer);
begin
  if FActivePaneSize <> Value then
  begin
    FActivePaneSize := Value;
    Changed;
  end;
end;

procedure TJvDockVSNETChannelOption.SetHideHoldTime(const Value: Integer);
begin
  if FHideHoldTime <> Value then
    if Value < 100 then
    begin
      { (rom) disabled
      if csDesigning in DockStyle.ComponentState then
        ShowMessage('HideHoldTime cannot be less than 100');
      }
      FHideHoldTime := 100;
    end
    else
      FHideHoldTime := Value;
end;

procedure TJvDockVSNETChannelOption.SetMouseleaveHide(const Value: Boolean);
begin
  if FMouseleaveHide <> Value then
  begin
    FMouseleaveHide := Value;
    { Notify TJvDockVSNetStyle for enabling/disabling timer? }
  end;
end;

procedure TJvDockVSNETChannelOption.SetShowImage(const Value: Boolean);
begin
  FShowImage := Value;
end;

procedure TJvDockVSNETChannelOption.SetTabColor(const Value: TColor);
begin
  if FTabColor <> Value then
  begin
    FTabColor := Value;
    Changed;
  end;
end;

//=== { TJvDockVSNETConjoinServerOption } ====================================

constructor TJvDockVSNETConjoinServerOption.Create(ADockStyle: TJvDockObservableStyle);
begin
  inherited Create(ADockStyle);
  SystemInfo := True;
end;

procedure TJvDockVSNETConjoinServerOption.UpdateDefaultSystemCaptionInfo;
begin
  inherited UpdateDefaultSystemCaptionInfo;

  ActiveFont.Color := clWhite;
  ActiveFont.Style := [];

  InactiveFont.Color := clBlack;
  InactiveFont.Style := [];

  ActiveTitleEndColor := ActiveTitleStartColor;
  InactiveTitleStartColor := clBtnFace;
  InactiveTitleEndColor := clBtnFace;
end;

//=== { TJvDockVSNETPanel } ==================================================

constructor TJvDockVSNETPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVSChannelClass := TJvDockVSChannel;
end;

procedure TJvDockVSNETPanel.AddDockServer(ADockServer: TJvDockServer);
begin
  { Dirty; resolve with new class? }
  if not (Self is TJvDockVSPopupPanel) and Assigned(ADockServer) then
  begin
    CreateVSChannel;
  end;
end;

procedure TJvDockVSNETPanel.CreateVSChannel;
begin
  if (FVSChannelClass <> nil) and
    (FVSChannelClass <> TJvDockVSChannelClass(ClassType)) then
  begin
    FVSChannel := FVSChannelClass.Create(Self);
    FVSChannel.Parent := Parent;
    FVSChannel.Align := Align;
    FVSChannel.ResetFontAngle;
    FVSChannel.ResetPosition;
    FVSChannel.Visible := False;
    FVSChannel.Name := Name + '_VSChannel';
    FVSChannel.CreateVSPopupPanel;
    FVSChannel.FreeNotification(Self);
  end;
end;

procedure TJvDockVSNETPanel.CustomDockDrop(Source: TJvDockDragDockObject;
  X, Y: Integer);
begin
  inherited CustomDockDrop(Source, X, Y);
  VSChannel.ActiveDockForm.Perform(CM_EXIT, 0, 0);
end;

procedure TJvDockVSNETPanel.DestroyVSChannel;
begin
  FVSChannel.Free;
  FVSChannel := nil;
end;

procedure TJvDockVSNETPanel.DoAutoHideControl(Control: TWinControl);
begin
  if Align = alNone then
    DoShowControl(Control)
  else
    DoHideControl(Control);
end;

procedure TJvDockVSNETPanel.DoHideControl(Control: TWinControl);
begin
  JvDockLockWindow(nil);
  DisableAlign;
  try
    VSChannel.AddDockControl(Control);
    if LRDockWidth = 0 then
      LRDockWidth := Control.LRDockWidth;
    if TBDockHeight = 0 then
      TBDockHeight := Control.TBDockHeight;
    ShowDockPanel(VisibleDockClientCount > 1, Control, sdfDockPanel);
    { using a null-rect as parameter for Dock causes align problems }
    Control.Dock(VSChannel.VSPopupPanel, Control.BoundsRect);
//    Control.Dock(VSChannel.VSPopupPanel, Rect(0, 0, 0, 0));
    { (rb) For every call to InsertControl there must be a call to RemoveControl.
      That is not guaranteed now, so JvDockManager may be filled with dangling
      references }
//    VSChannel.VSPopupPanel.JvDockManager.InsertControl(Control, alNone, nil);
    VSChannel.InternalInsertControl(Control);
    VSChannel.VSPopupPanel.JvDockManager.ShowSingleControl(Control);
    JvDockManager.HideControl(Control);
    ResetChannelBlockStartOffset(VSChannel);
  finally
    EnableAlign;
    JvDockUnLockWindow;
  end;
end;

procedure TJvDockVSNETPanel.DoShowControl(Control: TWinControl);
var
  Panel: TJvDockVSNETPanel;

  procedure ResetDockFormVisible;
  var
    I: Integer;
  begin
    if Control is TJvDockTabHostForm then
      with TJvDockTabHostForm(Control) do
        for I := 0 to PageControl.Count - 1 do
        begin
          PageControl.Pages[I].Visible := TJvDockVSNETTabSheet(PageControl.Pages[I]).OldVisible;
          PageControl.Pages[I].Controls[0].Visible := PageControl.Pages[I].Visible;
          if PageControl.ActivePage = PageControl.Pages[I] then
            PageControl.Pages[I].BringToFront;
        end;
  end;

begin
  { Dirty; solve with virtual method? }
  if Self is TJvDockVSPopupPanel then
  begin
    Panel := TJvDockVSPopupPanel(Self).FVSNETDockPanel;

    JvDockLockWindow(nil);
    Panel.DisableAlign;
    try
      { using a null-rect as parameter for Dock causes align problems }
      Control.Dock(Panel, Control.BoundsRect);
//      Control.Dock(Panel, Rect(0, 0, 0, 0));
      Panel.JvDockManager.ShowControl(Control);
//      JvDockManager.RemoveControl(Control);
      Panel.VSChannel.InternalRemoveControl(Control);
      Panel.VSChannel.RemoveDockControl(Control);
      if Panel.LRDockWidth = 0 then
        Panel.LRDockWidth := Control.LRDockWidth;
      if Panel.TBDockHeight = 0 then
        Panel.TBDockHeight := Control.TBDockHeight;
      Panel.ShowDockPanel(Panel.VisibleDockClientCount > 0, Control, sdfDockPanel);
      Panel.VSChannel.AutoFocusActiveDockForm;
      Panel.VSChannel.HidePopupPanel(Panel.VSChannel.PopupPane);
      ResetDockFormVisible;
      ResetChannelBlockStartOffset(Panel.VSChannel);
    finally
      Panel.EnableAlign;
      JvDockUnLockWindow;
    end;
  end;
end;

procedure TJvDockVSNETPanel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = VSChannel) and (Operation = opRemove) then
    FVSChannel := nil;
end;

procedure TJvDockVSNETPanel.RemoveDockServer(ADockServer: TJvDockServer);
begin
  DestroyVSChannel;
end;

procedure TJvDockVSNETPanel.Resize;
begin
  inherited Resize;
  if (Align in [alTop, alBottom]) and Assigned(DockServer) then
  begin
    if Assigned(DockServer.DockPanelWithAlign[alLeft]) then
      TJvDockVSNETPanel(DockServer.DockPanelWithAlign[alLeft]).VSChannel.ResetPopupPanelHeight;
    if Assigned(DockServer.DockPanelWithAlign[alRight]) then
      TJvDockVSNETPanel(DockServer.DockPanelWithAlign[alRight]).VSChannel.ResetPopupPanelHeight;
  end;
end;

//=== { TJvDockVSNetStyle } ==================================================

constructor TJvDockVSNetStyle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DockPanelClass := TJvDockVSNETPanel;
  DockSplitterClass := TJvDockVSNETSplitter;
  ConjoinPanelClass := TJvDockVSNETConjoinPanel;
  TabDockClass := TJvDockVSNETTabPageControl;
  DockPanelTreeClass := TJvDockVSNETTree;
  DockPanelZoneClass := TJvDockVSNETZone;
  ConjoinPanelTreeClass := TJvDockVSNETTree;
  ConjoinPanelZoneClass := TJvDockVSNETZone;
  ConjoinServerOptionClass := TJvDockVSNETConjoinServerOption;
  TabServerOptionClass := TJvDockVSNETTabServerOption;
  FChannelOptionClass := TJvDockVSNETChannelOption;

  FDockServers := TList.Create;
end;

destructor TJvDockVSNetStyle.Destroy;
begin
  { Note that RemoveDockBaseControl can be called in the inherited Destroy call.
    So we set FTimer to nil, and destroy FDockServers after the inherited call.
  }
  DestroyTimer;
  inherited Destroy;
  FDockServers.Free;
end;

procedure TJvDockVSNetStyle.AddDockBaseControl(ADockBaseControl: TJvDockBaseControl);
begin
  inherited AddDockBaseControl(ADockBaseControl);

  if ADockBaseControl is TJvDockServer then
    FDockServers.Add(ADockBaseControl);
end;

procedure TJvDockVSNetStyle.BeginPopup(AChannel: TJvDockVSChannel);
begin
  CreateTimer;
end;

procedure TJvDockVSNetStyle.CreateServerOption;
begin
  inherited CreateServerOption;
  if (FChannelOption = nil) and (FChannelOptionClass <> nil) then
    FChannelOption := FChannelOptionClass.Create(Self);
end;

procedure TJvDockVSNetStyle.CreateTimer;
begin
  if not ChannelOption.MouseleaveHide then
    Exit;
  if csDesigning in ComponentState then
    Exit;

  if not Assigned(FTimer) then
  begin
    FTimer := TTimer.Create(Self);
    FTimer.Interval := 100; // !! high interval
    FTimer.OnTimer := Self.Timer;
    FTimer.Enabled := True;

    FCurrentTimer := ChannelOption.HideHoldTime;
  end;
end;

procedure TJvDockVSNetStyle.DestroyTimer;
begin
  FTimer.Free;
  FTimer := nil;
end;

function TJvDockVSNetStyle.DockClientWindowProc(DockClient: TJvDockClient;
  var Msg: TMessage): Boolean;
var
  Channel: TJvDockVSChannel;
begin
  Result := inherited DockClientWindowProc(DockClient, Msg);
  case Msg.Msg of
    CM_ENTER, CM_EXIT:
      begin
        Channel := RetrieveChannel(DockClient.ParentForm.HostDockSite);
        if Msg.Msg = CM_EXIT then
        begin
          if Assigned(Channel) and (Channel.ActiveDockForm = DockClient.ParentForm) then
            Channel.HidePopupPanelWithAnimate;
        end
        else
        if Msg.Msg = CM_ENTER then
          HideAllPopupPanel(Channel);
      end;
  end;
end;

function TJvDockVSNetStyle.DockServerWindowProc(DockServer: TJvDockServer;
  var Msg: TMessage): Boolean;
begin
  Result := inherited DockServerWindowProc(DockServer, Msg);
  if Msg.Msg = WM_SIZE then
    with TChannelEnumerator.Create(DockServer) do
    try
      while MoveNext do
        Current.HidePopupPanel(Current.PopupPane);
    finally
      Free;
    end;
end;

procedure TJvDockVSNetStyle.DoHideDockForm(DockWindow: TWinControl);
var
  TmpDockWindow: TWinControl;

  procedure HideDockChild(DockWindow: TWinControl);
  var
    I: Integer;
    DockClient: TJvDockClient;
  begin
    if DockWindow = nil then
      Exit;
    if (DockWindow is TJvDockableForm) and (DockWindow.Visible) then
      with TJvDockableForm(DockWindow).DockableControl do
        for I := 0 to DockClientCount - 1 do
          HideDockChild(TWinControl(DockClients[I]));
    DockClient := FindDockClient(DockWindow);
    if (DockWindow is TForm) and (TForm(DockWindow).FormStyle <> fsMDIChild) and
      (DockClient.DockStyle <> nil) then
      DockClient.DockStyle.HideDockForm(DockClient);
  end;

  procedure HideDockParent(DockWindow: TWinControl);
  var
    Host: TWinControl;
    DockClient: TJvDockClient;
  begin
    if (DockWindow <> nil) and (DockWindow.HostDockSite <> nil) then
    begin
      // work-around
      if Assigned(RetrieveChannel(DockWindow.HostDockSite)) then
        Exit;

      Host := DockWindow.HostDockSite;
      if Host.VisibleDockClientCount = 0 then
        if Host is TJvDockPanel then
          TJvDockPanel(Host).ShowDockPanel(False, nil)
        else
        begin
          if Host.Parent <> nil then
          begin
            DockClient := FindDockClient(Host.Parent);
            if (DockClient <> nil) and (DockClient.DockStyle <> nil) then
              DockClient.DockStyle.HideDockForm(DockClient);
            HideDockParent(Host.Parent);
          end;
        end;
    end;
  end;

  procedure HidePopupPanel(Client: TWinControl);
  var
    Channel: TJvDockVSChannel;
  begin
    Channel := RetrieveChannel(Client.HostDockSite);
    if Assigned(Channel) then
      Channel.HidePopupPanel(Client);
  end;

begin
  TmpDockWindow := DockWindow;
  HideDockChild(DockWindow);
  HideDockParent(DockWindow);
  if (DockWindow.HostDockSite is TJvDockCustomControl) then
    TJvDockCustomControl(DockWindow.HostDockSite).UpdateCaption(DockWindow);
  HidePopupPanel(TmpDockWindow);
end;

procedure TJvDockVSNetStyle.DoShowDockForm(DockWindow: TWinControl);

  procedure PopupAutoHiddenForm(Client: TWinControl);
  var
    Channel: TJvDockVSChannel;
  begin
    Channel := RetrieveChannel(Client.HostDockSite);
    if Assigned(Channel) then
      Channel.PopupDockForm(Client);
  end;

begin
  inherited DoShowDockForm(DockWindow);
  PopupAutoHiddenForm(DockWindow);
end;

procedure TJvDockVSNetStyle.DoUnAutoHideDockForm(DockWindow: TWinControl);

  procedure ShowAutoHiddenForm(Client: TWinControl);
  var
    Channel: TJvDockVSChannel;
  begin
    Channel := RetrieveChannel(Client.HostDockSite);
    if Assigned(Channel) then
      Channel.ShowPopupPanel(Client);
  end;

begin
  inherited DoShowDockForm(DockWindow);
  ShowAutoHiddenForm(DockWindow);
end;

procedure TJvDockVSNetStyle.EndPopup(AChannel: TJvDockVSChannel);
begin
  DestroyTimer;
end;

procedure TJvDockVSNetStyle.FreeServerOption;
begin
  inherited FreeServerOption;
  FChannelOption.Free;
  FChannelOption := nil;
end;

class function TJvDockVSNetStyle.GetAnimationInterval: Integer;
begin
  Result := GlobalPopupPanelAnimateInterval;
end;

class function TJvDockVSNetStyle.GetAnimationMoveWidth: Integer;
begin
  Result := GlobalPopupPanelAnimateMoveWidth;
end;

class function TJvDockVSNetStyle.GetAnimationStartInterval: Integer;
begin
  Result := GlobalPopupPanelStartAnimateInterval;
end;

function TJvDockVSNetStyle.GetChannelOption: TJvDockVSNETChannelOption;
begin
  Result := FChannelOption;
end;

function TJvDockVSNetStyle.GetDockFormVisible(ADockClient: TJvDockClient): Boolean;
var
  Channel: TJvDockVSChannel;
  Pane: TJvDockVSPane;
begin
  Result := True;
  if Assigned(ADockClient) then
  begin
    Channel := RetrieveChannel(ADockClient.ParentForm.HostDockSite);
    if Assigned(Channel) then
    begin
      Pane := Channel.FindPane(ADockClient.ParentForm);
      if Assigned(Pane) then
        Result := Pane.FVisible;
    end
    else
      Result := inherited GetDockFormVisible(ADockClient);
  end;
end;

procedure TJvDockVSNetStyle.HideDockForm(ADockClient: TJvDockClient);
begin
  inherited HideDockForm(ADockClient);
  SetDockFormVisible(ADockClient, False);
end;

procedure TJvDockVSNetStyle.RemoveDockBaseControl(
  ADockBaseControl: TJvDockBaseControl);
begin
  inherited RemoveDockBaseControl(ADockBaseControl);

  if ADockBaseControl is TJvDockServer then
  begin
    FDockServers.Remove(ADockBaseControl);
    if FDockServers.Count = 0 then
      DestroyTimer;
  end;
end;

procedure TJvDockVSNetStyle.RestoreClient(DockClient: TJvDockClient);
begin
  { Skip if the form is autohidden on a channel }
  if Assigned(RetrieveChannel(DockClient.ParentForm.HostDockSite)) then
    Exit;
  inherited RestoreClient(DockClient);
end;

class procedure TJvDockVSNetStyle.SetAnimationInterval(const Value: Integer);
begin
  if GlobalPopupPanelAnimateInterval <> Value then
  begin
    GlobalPopupPanelAnimateInterval := Value;
    FreeAndNil(GlobalPopupPanelAnimate);
  end;
end;

class procedure TJvDockVSNetStyle.SetAnimationMoveWidth(const Value: Integer);
begin
  if GlobalPopupPanelAnimateMoveWidth <> Value then
  begin
    GlobalPopupPanelAnimateMoveWidth := Value;
    FreeAndNil(GlobalPopupPanelAnimate);
  end;
end;

procedure TJvDockVSNetStyle.SetChannelOption(const Value: TJvDockVSNETChannelOption);
begin
  { !! May be nil }
  FChannelOption.Assign(Value);
end;

procedure TJvDockVSNetStyle.SetDockFormVisible(ADockClient: TJvDockClient;
  AVisible: Boolean);
var
  Channel: TJvDockVSChannel;
  Pane: TJvDockVSPane;

  procedure ResetActivePane;
  var
    I: Integer;
  begin
    if AVisible then
      Pane.FBlock.ActivePane := Pane
    else
    begin
      for I := Pane.FIndex downto 0 do
        if Pane.FBlock.VSPane[I].FVisible then
        begin
          Pane.FBlock.ActivePane := Pane.FBlock.VSPane[I];
          Exit;
        end;
      for I := Pane.FIndex + 1 to Pane.FBlock.VSPaneCount - 1 do
        if Pane.FBlock.VSPane[I].FVisible then
        begin
          Pane.FBlock.ActivePane := Pane.FBlock.VSPane[I];
          Exit;
        end;
    end;
  end;
begin
  if not Assigned(ADockClient) then
    Exit;

  Channel := RetrieveChannel(ADockClient.ParentForm.HostDockSite);
  if not Assigned(Channel) then
  begin
    // Mantis 3752
    if ADockClient.ParentForm.Parent is TJvDockVSNETTabSheet then
      (ADockClient.ParentForm.Parent as TJvDockVSNETTabSheet).OldVisible := AVisible;
    Exit;
  end;

  Pane := Channel.FindPane(ADockClient.ParentForm);
  if Assigned(Pane) and (Pane.FDockForm = ADockClient.ParentForm) then
  begin
    Pane.FVisible := AVisible;
    ResetActivePane;
    if ADockClient.ParentForm.Parent is TJvDockVSNETTabSheet then
      TJvDockVSNETTabSheet(ADockClient.ParentForm.Parent).OldVisible := AVisible;

    Channel.ResetPosition;
    Channel.Invalidate;
  end;
end;

procedure TJvDockVSNetStyle.ShowDockForm(ADockClient: TJvDockClient);
begin
  inherited ShowDockForm(ADockClient);
  SetDockFormVisible(ADockClient, True);
end;

procedure TJvDockVSNetStyle.Timer(Sender: TObject);

  function IsPopupWindow(Handle: HWND): Boolean;
  var
    OwningProcess: DWORD;
    LStyle: Cardinal;
  begin
    Result := False;
    if (Handle <> 0) and (GetWindowThreadProcessID(Handle, @OwningProcess) <> 0) and
       (OwningProcess = GetCurrentProcessId) then
    begin
      LStyle := GetWindowLong(Handle, GWL_STYLE);
      Result := WS_POPUP and LSTYLE <> 0;
    end;
  end;

  function PointIsOnPopup(P: TPoint; GlobalCheck: Boolean): Boolean;
  const
    GW_ENABLEDPOPUP = 6;
  var
    Control: TWinControl;
    Handle: HWND;
//    Rect: TRect;
    ActivePopupWindow: Boolean;
  begin
    Control := FindVCLWindow(P);
    Result := ControlIsOnPopup(Control);
    if not Result then
    begin
      // Check whether a popup window is currently displayed (hint, popup menu)
      Handle := WindowFromPoint(P);
      ActivePopupWindow := IsPopupWindow(Handle);
      if not ActivePopupWindow and GlobalCheck then
      begin
        Handle := GetWindow(Application.Handle, GW_ENABLEDPOPUP);
        ActivePopupWindow := IsPopupWindow(Handle);
        if not ActivePopupWindow then
        begin
          Handle := GetTopWindow(GetDesktopWindow);
          ActivePopupWindow := IsPopupWindow(Handle);
        end;
      end;
      Result := ActivePopupWindow;

//      if ActivePopupWindow then
//      begin
//        GetWindowRect(Handle, Rect);
//        // Search for a control one pixel to the left;
//        Dec(Rect.Left);
//        Result := PointIsOnPopup(Rect.TopLeft, False);
//        if not Result then
//        begin
//          // Search for a control one pixel to the Right;
//          Inc(Rect.Right);
//          Result := PointIsOnPopup(Point(Rect.Right, Rect.Top), False);
//        end;
//      end;
    end;
  end;

var
  P: TPoint;
  I: Integer;
  ADockServer: TJvDockServer;
begin
  if (csDesigning in ComponentState) or not ChannelOption.MouseleaveHide or
     ((GetAsyncKeyState(VK_LBUTTON) and $8000) <> 0) then
    Exit;

  GetCursorPos(P);
  if PointIsOnPopup(P, True) then
  begin
    { Reset timer }
    FCurrentTimer := ChannelOption.HideHoldTime;
    Exit;
  end;

  Dec(FCurrentTimer, 100);
  if FCurrentTimer > 0 then
    Exit;
  DestroyTimer;

  for I := 0 to FDockServers.Count - 1 do
  begin
    ADockServer := TJvDockServer(FDockServers[I]);
    with TChannelEnumerator.Create(ADockServer) do
    try
      while MoveNext do
        Current.HidePopupPanelWithAnimate;
    finally
      Free;
    end;
  end;
end;

//=== { TJvDockVSNETTabPageControl } =========================================

constructor TJvDockVSNETTabPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabSheetClass := TJvDockVSNETTabSheet;
  TabPanelClass := TJvDockVSNETTabPanel;
end;

procedure TJvDockVSNETTabPageControl.ShowControl(AControl: TControl);
begin
  inherited ShowControl(AControl);
end;

//=== { TJvDockVSNETTabPanel } ===============================================

constructor TJvDockVSNETTabPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabHeight := 25;
  CaptionTopOffset := 1;
end;

//=== { TJvDockVSNETTabServerOption } ========================================

constructor TJvDockVSNETTabServerOption.Create(ADockStyle: TJvDockObservableStyle);
begin
  inherited Create(ADockStyle);
  InactiveFont.Color := VSNETPageInactiveFontColor;
  InactiveSheetColor := VSNETPageInactiveSheetColor;
  ShowTabImages := True;
end;

//=== { TJvDockVSNETTabSheet } ===============================================

constructor TJvDockVSNETTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldVisible := True;
end;

procedure TJvDockVSNETTabSheet.SetOldVisible(const Value: Boolean);
begin
  FOldVisible := Value;
end;

//=== { TJvDockVSNETTree } ===================================================

constructor TJvDockVSNETTree.Create(DockSite: TWinControl;
  DockZoneClass: TJvDockZoneClass; ADockStyle: TJvDockObservableStyle);
begin
  inherited Create(DockSite, DockZoneClass, ADockStyle);

  ButtonHeight := 12;
  ButtonWidth := 16;
  LeftOffset := 2;
  RightOffset := 3;
  TopOffset := 4;
  BottomOffset := 3;
  ButtonSplitter := 2;
  CaptionLeftOffset := 5;
  CaptionRightOffset := 5;
end;

procedure TJvDockVSNETTree.BeginDrag(Control: TControl; Immediate: Boolean;
  Threshold: Integer);
begin
  if not (DockSite is TJvDockVSPopupPanel) then
    inherited BeginDrag(Control, Immediate, Threshold);
end;

procedure TJvDockVSNETTree.CustomLoadZone(Stream: TStream;
  var Zone: TJvDockZone);
var
  Pane: TJvDockVSPane;
  I: Integer;
  Sheet: TJvDockVSNETTabSheet;

  procedure SetPaneVisible(ChildControl: TControl; VSPaneVisible: Boolean);
  var
    ADockClient: TJvDockClient;
  begin
    if Pane <> nil then
    begin
      Pane.FVisible := VSPaneVisible;
      ADockClient := FindDockClient(Pane.FDockForm);
      if ADockClient <> nil then
        if Pane.FVisible then
        begin
          ADockClient.ParentVisible := False;
          ADockClient.ParentForm.Visible := True;
          ADockClient.MakeShowEvent;
        end
        else
          ADockClient.MakeHideEvent;
    end;
  end;

begin
  inherited CustomLoadZone(Stream, Zone);
  if Zone = nil then
    Exit;
  Stream.Read(TJvDockVSNETZone(Zone).FVSPaneVisible, SizeOf(TJvDockVSNETZone(Zone).VSPaneVisible));
  if DockSite is TJvDockVSPopupPanel then
  begin
    with TJvDockVSPopupPanel(DockSite).VSChannel, TJvDockVSNETZone(Zone) do
    begin
      if ChildControl is TJvDockTabHostForm then
      begin
        for I := 0 to TJvDockTabHostForm(ChildControl).PageControl.Count - 1 do
        begin
          Sheet := TJvDockVSNETTabSheet(TJvDockTabHostForm(ChildControl).PageControl.Pages[I]);
          Pane := FindPane(TWinControl(Sheet.Controls[0]));
          SetPaneVisible(ChildControl, Sheet.OldVisible);
        end;
      end
      else
      begin
        Pane := FindPane(ChildControl);
        SetPaneVisible(ChildControl, VSPaneVisible);
      end;
      ResetPosition;
    end;
  end;
end;

procedure TJvDockVSNETTree.CustomSaveZone(Stream: TStream;
  Zone: TJvDockZone);
var
  Pane: TJvDockVSPane;
begin
  inherited CustomSaveZone(Stream, Zone);
  if DockSite is TJvDockVSPopupPanel then
    with TJvDockVSPopupPanel(DockSite).VSChannel, TJvDockVSNETZone(Zone) do
    begin
      Pane := FindPane(ChildControl);
      if Pane <> nil then
        VSPaneVisible := Pane.FVisible;
    end;
  Stream.Write(TJvDockVSNETZone(Zone).VSPaneVisible, SizeOf(TJvDockVSNETZone(Zone).VSPaneVisible));
end;

procedure TJvDockVSNETTree.DoHideZoneChild(AZone: TJvDockZone);
var
  Form: TCustomForm;
  ADockClient: TJvDockClient;
begin
  if (AZone <> nil) and (AZone.ChildControl <> nil) then
  begin
    if AZone.ChildControl is TJvDockTabHostForm then
    begin
      Form := TJvDockTabHostForm(AZone.ChildControl).PageControl.ActiveDockForm;
      if Form <> nil then
      begin
        ADockClient := FindDockClient(Form);
        if (ADockClient <> nil) and not ADockClient.EnableCloseButton then
          Exit
        else
          Form.Close;
      end;
    end
    else
      inherited DoHideZoneChild(AZone);
  end;
end;

procedure TJvDockVSNETTree.DoLButtonDbClk(var Msg: TWMMouse;
  var Zone: TJvDockZone; out HTFlag: Integer);
begin
  if not (DockSite is TJvDockVSPopupPanel) then
    inherited DoLButtonDbClk(Msg, Zone, HTFlag);
end;

function TJvDockVSNETTree.DoLButtonDown(var Msg: TWMMouse;
  var Zone: TJvDockZone; out HTFlag: Integer): Boolean;
begin
  Result := inherited DoLButtonDown(Msg, Zone, HTFlag);
  if Zone <> nil then
  begin
    if HTFlag = HTCLOSE then
      TJvDockVSNETZone(Zone).CloseBtnState := bsDown
    else
    if HTFlag = HTAUTOHIDE then
    begin
      AutoHideZone := TJvDockVSNETZone(Zone);
      AutoHideZone.AutoHideBtnDown := True;
      AutoHideZone.AutoHideBtnState := bsDown;
    end;
  end;
end;

procedure TJvDockVSNETTree.DoLButtonUp(var Msg: TWMMouse;
  var Zone: TJvDockZone; out HTFlag: Integer);
begin
  if CloseButtonZone <> nil then
    TJvDockVSNETZone(CloseButtonZone).CloseBtnState := bsNormal;
  inherited DoLButtonUp(Msg, Zone, HTFlag);
  if AutoHideZone <> nil then
  begin
    AutoHideZone.AutoHideBtnDown := False;
    AutoHideZone.AutoHideBtnState := bsNormal;
    if HTFlag = HTAUTOHIDE then
      if DockSite is TJvDockVSNETPanel then
        TJvDockVSNETPanel(DockSite).DoAutoHideControl(AutoHideZone.ChildControl);
    AutoHideZone := nil;
  end;
end;

procedure TJvDockVSNETTree.DoMouseMove(var Msg: TWMMouse;
  var AZone: TJvDockZone; out HTFlag: Integer);
var
  Zone: TJvDockVSNETZone;
begin
  inherited DoMouseMove(Msg, AZone, HTFlag);
  if AZone <> nil then
  begin
    Zone := TJvDockVSNETZone(AZone);
    if Zone.AutoHideBtnDown then
    begin
      if HTFlag = HTAUTOHIDE then
        Zone.AutoHideBtnState := bsDown
      else
        Zone.AutoHideBtnState := bsUp;
    end
    else
    if (HTFlag = HTAUTOHIDE) and not Zone.CloseBtnDown then
      Zone.AutoHideBtnState := bsUp
    else
      Zone.AutoHideBtnState := bsNormal;

    if Zone.CloseBtnDown then
    begin
      if HTFlag = HTCLOSE then
        Zone.CloseBtnState := bsDown
      else
        Zone.CloseBtnState := bsUp;
    end
    else
    if (HTFlag = HTCLOSE) and not Zone.AutoHideBtnDown then
      Zone.CloseBtnState := bsUp
    else
      Zone.CloseBtnState := bsNormal;
  end;
end;

procedure TJvDockVSNETTree.DoOtherHint(Zone: TJvDockZone; HTFlag: Integer;
  var HintStr: string);
begin
  inherited DoOtherHint(Zone, HTFlag, HintStr);
  if HTFlag = HTAUTOHIDE then
    HintStr := RsDockVSNETDockTreeAutoHideBtnHint;
end;

procedure TJvDockVSNETTree.DrawAutoHideButton(Zone: TJvDockZone; Left, Top: Integer);
var
  AZone: TJvDockVSNETZone;
  ColorArr: array [1..2] of TColor;
  ADockClient: TJvDockClient;
  IsActive: Boolean;
begin
  if Zone <> nil then
  begin
    ADockClient := FindDockClient(Zone.ChildControl);
    if (ADockClient <> nil) and not ADockClient.EnableCloseButton then
      Left := Left + ButtonWidth; // move the auto hide button to the Close Button's location

    AZone := TJvDockVSNETZone(Zone);
    IsActive := Assigned(Screen.ActiveControl) and Screen.ActiveControl.Focused and
      AZone.ChildControl.ContainsControl(Screen.ActiveControl);
    if AZone.AutoHideBtnState <> bsNormal then
    begin
      if AZone.AutoHideBtnState = bsUp then
      begin
        ColorArr[1] := clBlack;
        if IsActive then
          ColorArr[2] := clBtnFace
        else
          ColorArr[2] := clWhite;
      end
      else
      if AZone.AutoHideBtnState = bsDown then
      begin
        ColorArr[1] := clBtnFace;
        ColorArr[2] := clBlack;
      end;
      Canvas.Pen.Color := ColorArr[1];
      Canvas.MoveTo(Left, Top + ButtonHeight);
      Canvas.LineTo(Left + ButtonWidth, Top + ButtonHeight);
      Canvas.LineTo(Left + ButtonWidth, Top);
      Canvas.Pen.Color := ColorArr[2];
      Canvas.LineTo(Left, Top);
      Canvas.LineTo(Left, Top + ButtonHeight);
    end;

    if AZone.AutoHideBtnState = bsDown then
    begin
      Inc(Left, PPIScale(1));
      Inc(Top, PPIScale(1));
    end;

    if IsActive then
      Canvas.Pen.Color := clWhite
    else
      Canvas.Pen.Color := clBlack;
    if DockSite.Align in [alLeft, alRight, alTop, alBottom] then
    begin
      Canvas.MoveTo(Left + PPIScale(9), Top + PPIScale(10));
      Canvas.LineTo(Left + PPIScale(9), Top + PPIScale(7));
      Canvas.MoveTo(Left + PPIScale(6), Top + PPIScale(7));
      Canvas.LineTo(Left + PPIScale(13), Top + PPIScale(7));
      Canvas.MoveTo(Left + PPIScale(7), Top + PPIScale(6));
      Canvas.LineTo(Left + PPIScale(7), Top + PPIScale(2));
      Canvas.LineTo(Left + PPIScale(10), Top + PPIScale(2));
      Canvas.LineTo(Left + PPIScale(10), Top + PPIScale(6));
      Canvas.LineTo(Left + PPIScale(11), Top + PPIScale(6));
      Canvas.LineTo(Left + PPIScale(11), Top + PPIScale(1));
    end
    else
    if DockSite.Align in [alNone] then
    begin
      Canvas.MoveTo(Left + PPIScale(5), Top + PPIScale(6));
      Canvas.LineTo(Left + PPIScale(8), Top + PPIScale(6));
      Canvas.MoveTo(Left + PPIScale(8), Top + PPIScale(3));
      Canvas.LineTo(Left + PPIScale(8), Top + PPIScale(10));
      Canvas.MoveTo(Left + PPIScale(9), Top + PPIScale(4));
      Canvas.LineTo(Left + PPIScale(12), Top + PPIScale(4));
      Canvas.LineTo(Left + PPIScale(12), Top + PPIScale(7));
      Canvas.LineTo(Left + PPIScale(9), Top + PPIScale(7));
      Canvas.LineTo(Left + PPIScale(9), Top + PPIScale(8));
      Canvas.LineTo(Left + PPIScale(13), Top + PPIScale(8));
    end;
  end;
end;

procedure TJvDockVSNETTree.DrawCloseButton(Canvas: TCanvas;
  Zone: TJvDockZone; Left, Top: Integer);
var
  DrawRect: TRect;
  AZone: TJvDockVSNETZone;
  ColorArr: array [1..2] of TColor;
  ADockClient: TJvDockClient;
  AForm: TCustomForm;
  IsActive: Boolean;
  OrgPenWidth: Integer;
begin
  if Zone <> nil then
  begin
    ADockClient := FindDockClient(Zone.ChildControl);
    if (ADockClient <> nil) and not ADockClient.EnableCloseButton then
      Exit;
    if Zone.ChildControl is TJvDockTabHostForm then
    begin
      AForm := TJvDockTabHostForm(Zone.ChildControl).PageControl.ActiveDockForm;
      if AForm <> nil then
      begin
        ADockClient := FindDockClient(AForm);
        if (ADockClient <> nil) and not ADockClient.EnableCloseButton then
          Exit;
      end;
    end;
    AZone := TJvDockVSNETZone(Zone);
    IsActive := Assigned(Screen.ActiveControl) and Screen.ActiveControl.Focused and
      AZone.ChildControl.ContainsControl(Screen.ActiveControl);

    DrawRect.Left := Left + PPIScale(6);
    DrawRect.Right := DrawRect.Left + PPIScale(7);
    DrawRect.Top := Top + PPIScale(3);
    DrawRect.Bottom := DrawRect.Top + PPIScale(7);

    if AZone.CloseBtnState <> bsNormal then
    begin
      if AZone.CloseBtnState = bsUp then
      begin
        ColorArr[1] := clBlack;
        if IsActive then
          ColorArr[2] := clBtnFace
        else
          ColorArr[2] := clWhite;
      end
      else
      if AZone.CloseBtnState = bsDown then
      begin
        ColorArr[1] := clBtnFace;
        ColorArr[2] := clBlack;
      end;
      Canvas.Pen.Color := ColorArr[1];
      Canvas.MoveTo(Left, Top + ButtonHeight);
      Canvas.LineTo(Left + ButtonWidth, Top + ButtonHeight);
      Canvas.LineTo(Left + ButtonWidth, Top);
      Canvas.Pen.Color := ColorArr[2];
      Canvas.LineTo(Left, Top);
      Canvas.LineTo(Left, Top + ButtonHeight);
    end;

    if AZone.CloseBtnState = bsDown then
      OffsetRect(DrawRect, PPIScale(1), PPIScale(1));

    if IsActive then
      Canvas.Pen.Color := clWhite
    else
      Canvas.Pen.Color := clBlack;
    OrgPenWidth := Canvas.Pen.Width;
    try
      Canvas.Pen.Width := PPIScale(2);
      Dec(DrawRect.Left);
      Dec(DrawRect.Right);
      Canvas.MoveTo(DrawRect.Left, DrawRect.Top);
      Canvas.LineTo(DrawRect.Right, DrawRect.Bottom);
      Canvas.MoveTo(DrawRect.Right, DrawRect.Top);
      Canvas.LineTo(DrawRect.Left, DrawRect.Bottom);
    finally
      Canvas.Pen.Width := OrgPenWidth;
    end;
  end;
end;

procedure TJvDockVSNETTree.DrawDockGrabber(Control: TWinControl; const ARect: TRect);
begin
  inherited DrawDockGrabber(Control, ARect);
  if DockSite.Align <> alClient then
  begin
    DrawAutoHideButton(FindControlZone(Control),
      ARect.Right - RightOffset - 2 * ButtonWidth - ButtonSplitter,
      ARect.Top + TopOffset)
  end;
end;

procedure TJvDockVSNETTree.GetCaptionRect(var Rect: TRect);
var
  ADockClient: TJvDockClient;
begin
  if DockSite.Align = alClient then
    inherited GetCaptionRect(Rect)
  else
  begin
    Inc(Rect.Left, PPIScale(2) + CaptionLeftOffset);
    ADockClient := FindDockClient(DockSite);
    Inc(Rect.Top, PPIScale(1));
    if (ADockClient = nil) or ADockClient.EnableCloseButton then
      Dec(Rect.Right, 2 * ButtonWidth + ButtonSplitter + CaptionRightOffset - PPIScale(1))
    else
      Dec(Rect.Right, 1 * ButtonWidth + ButtonSplitter + CaptionRightOffset - PPIScale(1));
    Dec(Rect.Bottom, PPIScale(2));
  end;
end;

function TJvDockVSNETTree.GetTopGrabbersHTFlag(const MousePos: TPoint;
  out HTFlag: Integer; Zone: TJvDockZone): TJvDockZone;
var
  ADockClient: TJvDockClient;
begin
  Result := inherited GetTopGrabbersHTFlag(MousePos, HTFlag, Zone);
  if Zone <> nil then
  begin
    ADockClient := FindDockClient(Zone.ChildControl);
    if (ADockClient <> nil) and not ADockClient.EnableCloseButton then
    begin
      if HTFlag = HTCLOSE then
        HTFLAG := HTAUTOHIDE;
      Exit;
    end;
  end;

  if (Zone <> nil) and (DockSite.Align <> alClient) and (HTFlag <> HTCLOSE) then
  begin
    with Zone.ChildControl do
      if PtInRect(Rect(
        Left + Width - 2 * ButtonWidth - RightOffset - ButtonSplitter,
        Top - GrabberSize + TopOffset,
        Left + Width - 1 * ButtonWidth - RightOffset - ButtonSplitter,
        Top - GrabberSize + TopOffset + ButtonHeight), MousePos) then
        HTFlag := HTAUTOHIDE;
  end;
end;

procedure TJvDockVSNETTree.IgnoreZoneInfor(Stream: TMemoryStream);
begin
  inherited IgnoreZoneInfor(Stream);
  Stream.Position := Stream.Position + 1;
end;

procedure TJvDockVSNETTree.PaintDockGrabberRect(Canvas: TCanvas;
  Control: TWinControl; const ARect: TRect; PaintAlways: Boolean = False);
var
  DrawRect: TRect;
  IsActive: Boolean;
begin
  inherited PaintDockGrabberRect(Canvas, Control, ARect);
  IsActive := Assigned(Screen.ActiveControl) and Screen.ActiveControl.Focused and
    Control.ContainsControl(Screen.ActiveControl);
  if not IsActive or PaintAlways then
  begin
    Canvas.Pen.Color := clGray;
    DrawRect := ARect;
    Inc(DrawRect.Left, PPIScale(1));
    Canvas.RoundRect(DrawRect.Left, DrawRect.Top, DrawRect.Right, DrawRect.Bottom, PPIScale(2), PPIScale(2));
  end;
end;

//=== { TJvDockVSNETZone } ===================================================

constructor TJvDockVSNETZone.Create(Tree: TJvDockTree);
begin
  inherited Create(Tree);
  FAutoHideBtnState := bsNormal;
  FCloseBtnState := bsNormal;
  FVSPaneVisible := True;
end;

procedure TJvDockVSNETZone.DoCustomSetControlName;
var
  I: Integer;
  Pane: TJvDockVSPane;
  ADockClient: TJvDockClient;
begin
  inherited DoCustomSetControlName;
  if Tree.DockSite is TJvDockVSPopupPanel then
  begin
    with TJvDockVSPopupPanel(Tree.DockSite).VSChannel do
    begin
      AddDockControl(ChildControl);
      if ChildControl is TJvDockTabHostForm then
      begin
        with TJvDockTabHostForm(ChildControl).PageControl do
          for I := 0 to DockClientCount - 1 do
          begin
            Pane := FindPane(TWinControl(DockClients[I]));
            ADockClient := FindDockClient(DockClients[I]);
            if (Pane <> nil) and (ADockClient <> nil) then
              Pane.FWidth := ADockClient.VSPaneWidth;
          end;
      end
      else
      begin
        Pane := FindPane(ChildControl);
        ADockClient := FindDockClient(ChildControl);
        if (Pane <> nil) and (ADockClient <> nil) then
          Pane.FWidth := ADockClient.VSPaneWidth;
      end;
    end;
  end;
end;

procedure TJvDockVSNETZone.SetAutoHideBtnDown(const Value: Boolean);
begin
  FAutoHideBtnDown := Value;
end;

procedure TJvDockVSNETZone.SetAutoHideBtnState(const Value: TJvDockBtnState);
begin
  if FAutoHideBtnState <> Value then
  begin
    FAutoHideBtnState := Value;
    Tree.DockSite.Invalidate;
  end;
end;

procedure TJvDockVSNETZone.SetChildControlVisible(Client: TControl;
  AVisible: Boolean);
begin
  inherited SetChildControlVisible(Client, AVisible);
end;

procedure TJvDockVSNETZone.SetCloseBtnState(const Value: TJvDockBtnState);
begin
  if FCloseBtnState <> Value then
  begin
    FCloseBtnState := Value;
    Tree.DockSite.Invalidate;
  end;
end;

procedure TJvDockVSNETZone.SetVSPaneVisible(const Value: Boolean);
begin
  FVSPaneVisible := Value;
end;

//=== { TJvDockVSPane } ======================================================

constructor TJvDockVSPane.Create(ABlock: TJvDockVSBlock; AForm: TCustomForm;
  AWidth: Integer; AIndex: Integer);
begin
  inherited Create;
  FBlock := ABlock;
  FDockForm := AForm;
  FWidth := AWidth;
  FIndex := AIndex;
  FVisible := AForm.Visible;
end;

destructor TJvDockVSPane.Destroy;
begin
  if FBlock.ActivePane = Self then
    FBlock.FActivePane := nil;
  if FBlock.VSChannel.PopupPane = Self then
    FBlock.VSChannel.SetPopupPane(nil);
  inherited Destroy;
end;

function TJvDockVSPane.GetActive: Boolean;
begin
  Result := FBlock.VSChannel.PopupPane = Self;
end;

//=== { TJvDockVSPopupPanel } ================================================

constructor TJvDockVSPopupPanel.Create(AOwner: TComponent; APanel: TJvDockVSNETPanel);
begin
  inherited Create(AOwner);

  FVSNETDockPanel := APanel;
  FVSChannel := APanel.VSChannel;
  DockServer := APanel.DockServer;

  DockSite := True; {calls CreateDockManager when you do this!}
  Anchors := [akLeft, akRight, akTop, akBottom];
  BoundsRect := Rect(0, 0, 0, 0);
end;

function TJvDockVSPopupPanel.CreateDockManager: IDockManager;
var
  ADockStyle: TJvDockBasicStyle;
  TreeClass: TJvDockTreeClass;
begin
  Result := nil;
  if (DockManager = nil) and DockSite and UseDockManager then
  begin
    if Assigned(DockServer) then
    begin
      ADockStyle := DockServer.DockStyle;
      if Assigned(ADockStyle) then
      begin
        TreeClass := ADockStyle.DockPanelTreeClass;
        if Assigned(TreeClass) and (TreeClass <> TJvDockTree) then
          Result := TreeClass.Create(Self, ADockStyle.DockPanelZoneClass, ADockStyle) as IJvDockManager;
      end;
    end;
  end;

  if Result = nil then
    Result := DockManager;
  { (rb) Why not? }
  //  DoubleBuffered := DoubleBuffered or (Result <> nil);
end;

function TJvDockVSPopupPanel.GetVSChannel: TJvDockVSChannel;
begin
  if FVSNETDockPanel <> nil then
    Result := FVSNETDockPanel.VSChannel
  else
    Result := nil;
end;

procedure TJvDockVSPopupPanel.SetParent(AParent: TWinControl);
begin
  // (rom) this is suspicious
  inherited SetParent(AParent);
  if AParent = nil then
    Exit;
end;

procedure TJvDockVSPopupPanel.ShowDockPanel(MakeVisible: Boolean;
  Client: TControl; PanelSizeFrom: TJvDockSetDockPanelSizeFrom);
begin
  { (rb) Meaning? }
  if Align <> alNone then
    inherited ShowDockPanel(MakeVisible, Client, PanelSizeFrom);
end;

//=== { TJvDockVSPopupPanelSplitter } ========================================

constructor TJvDockVSPopupPanelSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoSnap := False;
  Align := alNone;
  Height := 0;
  Width := 0;
  FMinSize := 30;
  FResizeStyle := rsPattern;
  FOldSize := -1;
  FSplitWidth := 4;
  Anchors := [akLeft, akRight, akTop, akBottom];
end;

destructor TJvDockVSPopupPanelSplitter.Destroy;
begin
  FBrush.Free;
  inherited Destroy;
end;

procedure TJvDockVSPopupPanelSplitter.AllocateLineDC;
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

procedure TJvDockVSPopupPanelSplitter.CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
var
  S: Integer;
begin
  if VSChannelAlign in [alLeft, alRight] then
    Split := X - FDownPos.X
  else
    Split := Y - FDownPos.Y;
  S := 0;
  case VSChannelAlign of
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
  if S < MinSize then
    NewSize := MinSize
  else
  if S > FMaxSize then
    NewSize := FMaxSize;
  if S <> NewSize then
  begin
    if VSChannelAlign in [alRight, alBottom] then
      S := S - NewSize
    else
      S := NewSize - S;
    Inc(Split, S);
  end;
end;

function TJvDockVSPopupPanelSplitter.CanResize(var NewSize: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnCanResize) then
    FOnCanResize(Self, NewSize, Result);
end;

function TJvDockVSPopupPanelSplitter.DoCanResize(var NewSize: Integer): Boolean;
begin
  Result := CanResize(NewSize);
  if Result and (NewSize <= MinSize) and FAutoSnap then
    NewSize := 0;
end;

procedure TJvDockVSPopupPanelSplitter.DrawLine;
var
  X, Y: Integer;
begin
  FLineVisible := not FLineVisible;
  X := Left;
  Y := Top;
  if VSChannelAlign in [alLeft, alRight] then
    X := Left + FSplit
  else
    Y := Top + FSplit;
  PatBlt(FLineDC, X, Y, Width, Height, PATINVERT);
end;

function TJvDockVSPopupPanelSplitter.FindControl: TControl;
begin
  Result := FVSPopupPanel;
end;

procedure TJvDockVSPopupPanelSplitter.FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    StopSizing
  else
  if Assigned(FOldKeyDown) then
    FOldKeyDown(Sender, Key, Shift);
end;

function TJvDockVSPopupPanelSplitter.GetMinSize: NaturalNumber;
begin
  Result := PPIScale(FMinSize);
end;

function TJvDockVSPopupPanelSplitter.GetSplitWidth: Integer;
begin
  Result := PPIScale(FSplitWidth);
end;

function TJvDockVSPopupPanelSplitter.GetVSChannelAlign: TAlign;
begin
  Result := alNone;
  if (VSPopupPanel <> nil) and (VSPopupPanel.FVSNETDockPanel <> nil) then
    Result := VSPopupPanel.FVSNETDockPanel.Align;
end;

procedure TJvDockVSPopupPanelSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState;
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
      if VSChannelAlign in [alLeft, alRight] then
      begin
        FMaxSize := Parent.ClientWidth - MinSize;
        for I := 0 to Parent.ControlCount - 1 do
          with Parent.Controls[I] do
            if Align in [alLeft, alRight] then
              Dec(FMaxSize, Width);
        Inc(FMaxSize, FControl.Width);
      end
      else
      begin
        FMaxSize := Parent.ClientHeight - MinSize;
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
          { !! Dirty }
          FOldKeyDown := TWinControlAccessProtected(FActiveControl).OnKeyDown;
          TWinControlAccessProtected(FActiveControl).OnKeyDown := FocusKeyDown;
        end;
      if ResizeStyle in [rsLine, rsPattern] then
        DrawLine;
    end;
  end;
end;

procedure TJvDockVSPopupPanelSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewSize, Split: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  if (ssLeft in Shift) and Assigned(FControl) then
  begin
    CalcSplitSize(X, Y, NewSize, Split);
    if DoCanResize(NewSize) then
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

procedure TJvDockVSPopupPanelSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState;
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

procedure TJvDockVSPopupPanelSplitter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = VSPopupPanel) then
    FVSPopupPanel := nil;
end;

procedure TJvDockVSPopupPanelSplitter.Paint;
var
  FrameBrush: HBRUSH;
  R: TRect;
begin
  R := ClientRect;
  Canvas.Brush.Color := Color;
  InflateRect(R, PPIScale(2), PPIScale(2));
  case VSChannelAlign of
    alLeft:
      Dec(R.Right, PPIScale(2));
    alRight:
      Inc(R.Left, PPIScale(3));
    alTop:
      Dec(R.Bottom, PPIScale(2));
    alBottom:
      Inc(R.Top, PPIScale(3));
  end;
  DrawFrameControl(Canvas.Handle, R, DFC_BUTTON, DFCS_BUTTONPUSH or DFCS_ADJUSTRECT);
  R := ClientRect;
  if Beveled then
  begin
    if VSChannelAlign in [alLeft, alRight] then
      InflateRect(R, -PPIScale(1), PPIScale(2))
    else
      InflateRect(R, PPIScale(2), -PPIScale(1));
    OffsetRect(R, 1, 1);
    FrameBrush := CreateSolidBrush(ColorToRGB(clBtnHighlight));
    FrameRect(Canvas.Handle, R, FrameBrush);
    DeleteObject(FrameBrush);
    OffsetRect(R, -PPIScale(2), -PPIScale(2));
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

function TJvDockVSPopupPanelSplitter.PPIScale(Value: Integer): Integer;
begin
  Result := MulDiv(Value, FCurrentPPI, 96);
end;

procedure TJvDockVSPopupPanelSplitter.ReleaseLineDC;
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

procedure TJvDockVSPopupPanelSplitter.RequestAlign;
begin
  inherited RequestAlign;
  if VSChannelAlign in [alBottom, alTop] then
    Cursor := crVSplit
  else
    Cursor := crHSplit;
end;

procedure TJvDockVSPopupPanelSplitter.SetBeveled(Value: Boolean);
begin
  FBeveled := Value;
  Repaint;
end;

procedure TJvDockVSPopupPanelSplitter.SetSplitWidth(const Value: Integer);
begin
  FSplitWidth := Value;
end;

procedure TJvDockVSPopupPanelSplitter.SetVSPopupPanel(Value: TJvDockVSPopupPanel);
begin
  { Dirty }
  Assert((Value <> nil) and (Value is TJvDockVSPopupPanel));
  FVSPopupPanel := Value;
end;

procedure TJvDockVSPopupPanelSplitter.StopSizing;
begin
  if Assigned(FControl) then
  begin
    if FLineVisible then
      DrawLine;
    FControl := nil;
    ReleaseLineDC;
    if Assigned(FActiveControl) then
    begin
      { !! Dirty }
      TWinControlAccessProtected(FActiveControl).OnKeyDown := FOldKeyDown;
      FActiveControl := nil;
    end;
  end;
  if Assigned(FOnMoved) then
    FOnMoved(Self);
end;

procedure TJvDockVSPopupPanelSplitter.UpdateControlSize;
begin
  if FNewSize <> FOldSize then
  begin
    case VSChannelAlign of
      alLeft:
        begin
          FControl.Width := FNewSize;
          Left := FControl.Left + FNewSize;
        end;
      alTop:
        begin
          FControl.Height := FNewSize;
          Top := FControl.Top + FNewSize;
        end;
      alRight:
        begin
          Parent.DisableAlign;
          try
            FControl.Left := FControl.Left + (FControl.Width - FNewSize);
            FControl.Width := FNewSize;
            Left := FControl.Left - Width;
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
            Top := FControl.Top - Height;
          finally
            Parent.EnableAlign;
          end;
        end;
    end;
    FVSPopupPanel.VSChannel.ResetActivePaneWidth;
    Update;
    if Assigned(FOnMoved) then
      FOnMoved(Self);
    FOldSize := FNewSize;
  end;
end;

procedure TJvDockVSPopupPanelSplitter.UpdateSize(X, Y: Integer);
begin
  CalcSplitSize(X, Y, FNewSize, FSplit);
end;

//=== { TPopupPanelAnimate } =================================================

constructor TPopupPanelAnimate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Interval := TJvDockVSNetStyle.GetAnimationInterval;
  Enabled := False;
  FMaxWidth := 0;
  FCurrentWidth := 0;
  OnTimer := OnCustomTimer;
  FState := asPopup;
end;

procedure TPopupPanelAnimate.HideForm(AChannel: TJvDockVSChannel; MaxWidth: Integer);
begin
  if FActiveChannel <> nil then
    Exit;
  FActiveChannel := AChannel;
  Enabled := (FActiveChannel <> nil) and (FActiveChannel.ActiveDockForm <> nil);
  if FActiveChannel <> nil then
  begin
    FMaxWidth := MaxWidth;
    FCurrentWidth := 0;
    FState := asHide;
  end;
end;

procedure TPopupPanelAnimate.OnCustomTimer(Sender: TObject);
begin
  // ??? no handler?
end;

procedure TPopupPanelAnimate.PopupForm(AChannel: TJvDockVSChannel; MaxWidth: Integer);
begin
  { Currently busy with animating? }
  if (FCurrentWidth > 0) and (FActiveChannel <> nil) then
    { Dangerous, not in try..finally }
    FActiveChannel.Parent.EnableAlign;
  FActiveChannel := AChannel;
  Enabled := FActiveChannel <> nil;
  if FActiveChannel <> nil then
  begin
    FMaxWidth := MaxWidth;
    FCurrentWidth := 0;
    FState := asPopup;
  end;
end;

procedure TPopupPanelAnimate.Timer;
var
  SuitableWidth: Integer;

  procedure SetControlBringToFront(Control: TWinControl; Align: TAlign);
  var
    I: Integer;
  begin
    for I := Control.ControlCount - 1 downto 0 do
      if Control.Controls[I].Visible and (Control.Controls[I].Align = Align) and
        not (Control.Controls[I] is TJvDockVSChannel) and
        not (Control.Controls[I] is TJvDockPanel) and
        not (Control.Controls[I] is TJvDockSplitter) then
        Control.Controls[I].BringToFront;
  end;

begin
  inherited Timer;
  if FActiveChannel = nil then
    Exit;

  SuitableWidth := Min(FCurrentWidth, FMaxWidth);
  with FActiveChannel do
  begin
    if FCurrentWidth = 0 then
    begin
      { Dangerous, not in try..finally }
      Parent.DisableAlign;
      VSPopupPanel.BringToFront;
      VSPopupPanelSplitter.BringToFront;
      SetControlBringToFront(Parent, Align);
      BringToFront;
    end;
    case Align of
      alLeft:
        begin
          if FState = asPopup then
          begin
            if FCurrentWidth = 0 then
            begin
              VSPopupPanel.Width := FMaxWidth;
              VSPopupPanel.Top := Top;
              VSPopupPanel.Height := Height;
              VSPopupPanelSplitter.Top := Top;
              VSPopupPanelSplitter.Height := Height;
              VSPopupPanelSplitter.Width := VSPopupPanelSplitter.SplitWidth;
            end;
            VSPopupPanel.Left := Left + Width + SuitableWidth - VSPopupPanel.Width;
          end
          else
          if FState = asHide then
            VSPopupPanel.Left := Left - FCurrentWidth;

          VSPopupPanelSplitter.Left := VSPopupPanel.Left + VSPopupPanel.Width;
        end;
      alRight:
        begin
          if FState = asPopup then
          begin
            if FCurrentWidth = 0 then
            begin
              VSPopupPanel.Width := FMaxWidth;
              VSPopupPanel.Top := Top;
              VSPopupPanel.Height := Height;
              VSPopupPanelSplitter.Top := Top;
              VSPopupPanelSplitter.Height := Height;
              VSPopupPanelSplitter.Width := VSPopupPanelSplitter.SplitWidth;
            end;
            VSPopupPanel.Left := Left - SuitableWidth;
          end
          else
          if FState = asHide then
            VSPopupPanel.Left := Left - VSPopupPanel.Width + FCurrentWidth;

          VSPopupPanelSplitter.Left := VSPopupPanel.Left - VSPopupPanelSplitter.SplitWidth;
        end;
      alTop:
        begin
          if FState = asPopup then
          begin
            if FCurrentWidth = 0 then
            begin
              VSPopupPanel.Left := Left;
              VSPopupPanel.Height := FMaxWidth;
              VSPopupPanel.Width := Width;
              VSPopupPanelSplitter.Left := Left;
              VSPopupPanelSplitter.Width := Width;
              VSPopupPanelSplitter.Height := VSPopupPanelSplitter.SplitWidth;
            end;
            VSPopupPanel.Top := Top + Height + SuitableWidth - VSPopupPanel.Height;
          end
          else
          if FState = asHide then
            VSPopupPanel.Top := Top - FCurrentWidth;

          VSPopupPanelSplitter.Top := VSPopupPanel.Top + VSPopupPanel.Height;
        end;
      alBottom:
        begin
          if FState = asPopup then
          begin
            if FCurrentWidth = 0 then
            begin
              VSPopupPanel.Left := Left;
              VSPopupPanel.Width := Width;
              VSPopupPanel.Height := FMaxWidth;
              VSPopupPanelSplitter.Left := Left;
              VSPopupPanelSplitter.Width := Width;
              VSPopupPanelSplitter.Height := VSPopupPanelSplitter.SplitWidth;
            end;
            VSPopupPanel.Top := Top - SuitableWidth;
          end
          else
          if FState = asHide then
            VSPopupPanel.Top := Top - VSPopupPanel.Height + FCurrentWidth;
          VSPopupPanelSplitter.Top := VSPopupPanel.Top - VSPopupPanelSplitter.SplitWidth;
        end;
    end;
    VSPopupPanel.Visible := True;
    VSPopupPanelSplitter.Visible := True;
  end;
  if FCurrentWidth >= FMaxWidth then
  begin
    { Dangerous, not in try..finally }
    FActiveChannel.Parent.EnableAlign;
    Self.Enabled := False;
    if FState = asHide then
    begin
      if FActiveChannel.DockServer.DockStyle is TJvDockVSNetStyle then
        TJvDockVSNetStyle(FActiveChannel.DockServer.DockStyle).EndPopup(FActiveChannel);
      FActiveChannel.HidePopupPanel(FActiveChannel.PopupPane);
    end
    else
    begin
      if FActiveChannel.DockServer.DockStyle is TJvDockVSNetStyle then
        TJvDockVSNetStyle(FActiveChannel.DockServer.DockStyle).BeginPopup(FActiveChannel);

      FActiveChannel.AutoFocusActiveDockForm;
      HideAllPopupPanel(FActiveChannel);
    end;
    FActiveChannel := nil;
    FCurrentWidth := 0;
    FMaxWidth := 0;
  end
  else
    Inc(FCurrentWidth, FActiveChannel.PPIScale(TJvDockVSNetStyle.GetAnimationMoveWidth));
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  GlobalPopupPanelAnimate.Free;
  GlobalPopupPanelAnimate := nil;
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
