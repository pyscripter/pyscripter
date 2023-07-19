{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDockControlForm.pas, released on 2003-12-31.

The Initial Developer of the Original Code is luxiaoban.
Portions created by luxiaoban are Copyright (C) 2002,2003 luxiaoban.
All Rights Reserved.

Contributor(s):
  2005-02-08 - Warren Postma - TJvDockServer.CustomPanel.

Last Modified: 2005-02-08

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{ Changes:

  2005-02-08 WP: Added TJvDockServer.CustomDock panel (new property),
  and new event TJvDockServer.OnCustomPanel to get
  a custom parent control (such as the form's center area) for the
  CustomPanel. Previously you could only dock at the edges. Created a
  splitter object (to avoid possible access violations) but the splitter
  control is hidden and never used for this custom panel.  Several
  changes were made  to the VID dock style also to make this custom
  panel more useful,allowing an "tabbed MDI" style application.
}

unit JvDockControlForm;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF RTL330_UP}
  System.Messaging,
  {$ENDIF RTL330_UP}
  Windows, Messages, Classes, Graphics, Controls, Forms, Menus,
  ExtCtrls, ComCtrls,
  JvComponentBase, JvComponent, JvAppStorage, JvConsts,
  JvDockTree, JvDockSupportClass, JvDockSupportControl, JvDockAdvTree;

const
  JvDockPositionCount = 5;

  JvDockState_Unknown = 0;
  JvDockState_Docking = 1;
  JvDockState_Floating = 2;

type
  TJvDockSplitterSize = 0..32767;

  TJvDockBaseControl = class;
  TJvDockServer = class;
  TJvDockClient = class;
  TJvDockConjoinPanel = class;
  TJvDockTabPageControl = class;
  TJvDockConjoinHostForm = class;
  TJvDockTabHostForm = class;

  TJvDockSplitter = class(TJvDockCustomPanelSplitter)
  private
    FDockServer: TJvDockServer;
    function GetSplitterIndex: Integer;
  protected
    function FindControl: TControl; override;
    property DockServer: TJvDockServer read FDockServer write FDockServer;
  public
    constructor Create(AOwner: TComponent); override;
    property SplitterIndex: Integer read GetSplitterIndex;
  end;

  TJvDockSetDockPanelSizeFrom = (sdfDockPanel, sdfClient);

  TJvDockPanel = class(TJvDockCustomPanel)
  private
    FDockServer: TJvDockServer;
    FCustomFlag: Boolean; // Set only if custom area! {NEW}
    function GetPanelIndex: Integer;
    procedure SetDockServer(ADockServer: TJvDockServer);
  protected
    function CreateDockManager: IDockManager; override;
    procedure AddDockServer(ADockServer: TJvDockServer); virtual;
    procedure RemoveDockServer(ADockServer: TJvDockServer); virtual;
    procedure ReloadDockedControl(const AControlName: string;
      var AControl: TControl); override;
    procedure CustomStartDock(var Source: TJvDockDragDockObject); override;
    procedure CustomGetSiteInfo(Source: TJvDockDragDockObject; Client: TControl; var InfluenceRect: TRect;
      MousePos: TPoint; var CanDock: Boolean); override;
    procedure CustomDockOver(Source: TJvDockDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure CustomPositionDockRect(Source: TJvDockDragDockObject; X, Y: Integer); override;
    procedure CustomDockDrop(Source: TJvDockDragDockObject; X, Y: Integer); override;
    procedure CustomEndDock(Target: TObject; X, Y: Integer); override;
    function CustomUnDock(Source: TJvDockDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; override;
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;

    procedure ShowDockPanel(MakeVisible: Boolean; Client: TControl;
      PanelSizeFrom: TJvDockSetDockPanelSizeFrom = sdfClient); virtual;
    procedure ResetPosition;
    // GetDockedControls:  NEW! -WPostma.
    // base class doesn't have this capability.
    // see TJvDockAdvPanel for override that implements this!
	  procedure GetDockedControls(WinControls: TList); virtual;  { not supported in base! }
	  function FindTabHostForm:TWinControl; virtual;

    property PanelIndex: Integer read GetPanelIndex;
    property DockServer: TJvDockServer read FDockServer write SetDockServer;
    property CustomFlag: Boolean read FCustomFlag write FCustomFlag; // Set only if custom area! {NEW}
  end;

  TJvDockAdvPanel = class(TJvDockPanel)
  private
    procedure CMUnDockClient(var Msg: TCMUnDockClient); message CM_UNDOCKCLIENT;
  protected
    procedure CustomDockDrop(Source: TJvDockDragDockObject; X, Y: Integer); override;
    function CustomUnDock(Source: TJvDockDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; override;
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;
  public
    procedure GetDockedControls(WinControls: TList); override;
	function FindTabHostForm:TWinControl; override;
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
  end;

  TJvDockPanelClass = class of TJvDockPanel;
  TJvDockSplitterClass = class of TJvDockSplitter;
  TJvDockConjoinPanelClass = class of TJvDockConjoinPanel;
  TJvDockTabClass = class of TJvDockTabPageControl;

  { Maintained by a TJvDockServer; TJvDockServer ensures that FDockServer is
    assigned. FSplitter may be nil }
  TJvDockSplitterStyle = class(TPersistent)
  private
    FSplitter: TJvDockSplitter;
    FDockServer: TJvDockServer;
    FColor: TColor;
    FCursor: TCursor;
    FParentColor: Boolean;
    FResizeStyle: TResizeStyle;
    FSize: TJvDockSplitterSize;
    FMinSize: TJvDockSplitterSize;
    procedure SetColor(const Value: TColor);
    procedure SetCursor(const Value: TCursor);
    procedure SetParentColor(const Value: Boolean);
    procedure SetResizeStyle(const Value: TResizeStyle);
    procedure SetSize(const Value: TJvDockSplitterSize);
    procedure SetMinSize(const Value: TJvDockSplitterSize);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure AssignToSplitter(Dest: TJvDockSplitter);
    procedure SetSplitterStyle;
    property Splitter: TJvDockSplitter read FSplitter write FSplitter;
  public
    constructor Create(ASplitter: TJvDockSplitter; ACursor: TCursor); virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clBtnFace;
    property Cursor: TCursor read FCursor write SetCursor;
    property ParentColor: Boolean read FParentColor write SetParentColor default True;
    property ResizeStyle: TResizeStyle read FResizeStyle write SetResizeStyle default rsPattern;
    property Size: TJvDockSplitterSize read FSize write SetSize default 3;
    property MinSize: TJvDockSplitterSize read FMinSize write SetMinSize default 30;
  end;

  TJvDockBasicStyle = class;

  TJvDockBasicStyle = class(TJvDockObservableStyle)
  private
    FDockPanelClass: TJvDockPanelClass;
    FDockSplitterClass: TJvDockSplitterClass;
    FConjoinPanelClass: TJvDockConjoinPanelClass;
    FTabDockClass: TJvDockTabClass;
    FDockPanelTreeClass: TJvDockTreeClass;
    FDockPanelZoneClass: TJvDockZoneClass;
    FConjoinPanelTreeClass: TJvDockTreeClass;
    FConjoinPanelZoneClass: TJvDockZoneClass;
    FDockBaseControls: TList;
    function GetDockBaseControlCount: Integer;
    function GetDockBaseControl(Index: Integer): TJvDockBaseControl;
  protected
    procedure FormStartDock(DockClient: TJvDockClient; var Source: TJvDockDragDockObject); virtual;
    procedure FormGetSiteInfo(Source: TJvDockDragDockObject; DockClient: TJvDockClient; Client: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean); virtual;
    procedure FormDockOver(DockClient: TJvDockClient; Source: TJvDockDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); virtual;
    procedure FormPositionDockRect(DockClient: TJvDockClient; Source: TJvDockDragDockObject); virtual;
    procedure FormDockDrop(DockClient: TJvDockClient; Source: TJvDockDragDockObject; X, Y: Integer); virtual;
    procedure FormEndDock(DockClient: TJvDockClient; Target: TObject; X, Y: Integer); virtual;
    function FormUnDock(DockClient: TJvDockClient; NewTarget: TWinControl; Client: TControl): Boolean; virtual;
    procedure FormGetDockEdge(DockClient: TJvDockClient; Source: TJvDockDragDockObject;
      MousePos: TPoint; var DropAlign: TAlign); virtual;

    procedure SetDockBaseControl(IsCreate: Boolean; DockBaseControl: TJvDockBaseControl); virtual;
    function DockServerWindowProc(DockServer: TJvDockServer; var Msg: TMessage): Boolean; virtual;
    function DockClientWindowProc(DockClient: TJvDockClient; var Msg: TMessage): Boolean; virtual;
    procedure AddDockBaseControl(ADockBaseControl: TJvDockBaseControl); virtual;
    procedure RemoveDockBaseControl(ADockBaseControl: TJvDockBaseControl); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CanSetEnableDocked(ADockBaseControl: TJvDockBaseControl): Boolean; virtual;
    function CanSetLeftDocked(ADockBaseControl: TJvDockBaseControl): Boolean; virtual;
    function CanSetRightDocked(ADockBaseControl: TJvDockBaseControl): Boolean; virtual;
    function CanSetTopDocked(ADockBaseControl: TJvDockBaseControl): Boolean; virtual;
    function CanSetBottomDocked(ADockBaseControl: TJvDockBaseControl): Boolean; virtual;
    function CanSetEachOtherDocked(ADockBaseControl: TJvDockBaseControl): Boolean; virtual;

    procedure ResetCursor(Source: TJvDockDragDockObject); virtual;

    function GetDockState(DockClient: TJvDockClient): Integer; virtual;

    procedure DoShowDockForm(DockWindow: TWinControl); virtual;
    procedure DoHideDockForm(DockWindow: TWinControl); virtual;

    procedure ShowDockForm(ADockClient: TJvDockClient); virtual;
    procedure HideDockForm(ADockClient: TJvDockClient); virtual;
    function GetDockFormVisible(ADockClient: TJvDockClient): Boolean; virtual;

    property DockBaseControlCount: Integer read GetDockBaseControlCount;
    property DockBaseControl[Index: Integer]: TJvDockBaseControl read GetDockBaseControl;

    procedure RestoreClient(DockClient: TJvDockClient); virtual;
    property DockPanelClass: TJvDockPanelClass read FDockPanelClass write FDockPanelClass;
    property DockSplitterClass: TJvDockSplitterClass read FDockSplitterClass write FDockSplitterClass;
    property ConjoinPanelClass: TJvDockConjoinPanelClass read FConjoinPanelClass write FConjoinPanelClass;
    property TabDockClass: TJvDockTabClass read FTabDockClass write FTabDockClass;
    property DockPanelTreeClass: TJvDockTreeClass read FDockPanelTreeClass write FDockPanelTreeClass;
    property DockPanelZoneClass: TJvDockZoneClass read FDockPanelZoneClass write FDockPanelZoneClass;
    property ConjoinPanelTreeClass: TJvDockTreeClass read FConjoinPanelTreeClass write FConjoinPanelTreeClass;
    property ConjoinPanelZoneClass: TJvDockZoneClass read FConjoinPanelZoneClass write FConjoinPanelZoneClass;
  end;

  TJvDockAdvStyle = class(TJvDockBasicStyle)
  protected
    function DockClientWindowProc(DockClient: TJvDockClient; var Msg: TMessage): Boolean; override;
  end;

  TJvDockBaseControl = class(TJvComponent)
  private
    FEnableDock: Boolean;
    FLeftDock: Boolean; { Can the parent form be docked into the Left dock? }
    FTopDock: Boolean; { Can the parent form be docked into the Top dock? }
    FRightDock: Boolean; { Can the parent form be docked into the Right dock? }
    FBottomDock: Boolean; { Can the parent form be docked into the Bottom dock? }
    FCustomDock: Boolean; {NEW!  Can the parent form be docked into the Custom (center window) dock area? }
    FEachOtherDock: Boolean;
    FDockStyle: TJvDockBasicStyle;
    FOldOnClose: TCloseEvent;
    FOldOnCreate: TNotifyEvent;
    FParentForm: TForm;
    FOldWindowProc: TWndMethod;
    procedure SetDockStyle(ADockStyle: TJvDockBasicStyle);
  protected
    procedure SetParentComponent(Value: TComponent); override;

    function CanSetEnableDocked: Boolean; virtual;
    function CanSetLeftDocked: Boolean; virtual;
    function CanSetRightDocked: Boolean; virtual;
    function CanSetTopDocked: Boolean; virtual;
    function CanSetBottomDocked: Boolean; virtual;
    function CanSetEachOtherDocked: Boolean; virtual;

    procedure DoFormOnClose(Sender: TObject; var Action: TCloseAction); virtual;
    procedure DoFormOnCreate(Sender: TObject); virtual;

    procedure SetBottomDock(const Value: Boolean); virtual;
    procedure SetEachOtherDock(const Value: Boolean); virtual;
    procedure SetEnableDock(const Value: Boolean); virtual;
    procedure SetLeftDock(const Value: Boolean); virtual;
    procedure SetRightDock(const Value: Boolean); virtual;
    procedure SetTopDock(const Value: Boolean); virtual;

    procedure AddDockStyle(ADockStyle: TJvDockBasicStyle); virtual;
    procedure RemoveDockStyle(ADockStyle: TJvDockBasicStyle); virtual;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WindowProc(var Msg: TMessage); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    { Owner of this component }
    property ParentForm: TForm read FParentForm;
    property EnableDock: Boolean read FEnableDock write SetEnableDock default True;
    property LeftDock: Boolean read FLeftDock write SetLeftDock default True;
    property TopDock: Boolean read FTopDock write SetTopDock default True;
    property RightDock: Boolean read FRightDock write SetRightDock default True;
    property BottomDock: Boolean read FBottomDock write SetBottomDock default True;
    property EachOtherDock: Boolean read FEachOtherDock write SetEachOtherDock default True;
    property CustomDock: Boolean read FCustomDock write FCustomDock default True; {NEW!}
    property DockStyle: TJvDockBasicStyle read FDockStyle write SetDockStyle;
  end;

  TJvDockCustomPanelEvent = procedure(Sender: TJvDockServer;
    var AParent: TWinControl; var Align: TAlign) of object; {NEW!}
  TJvDockCheckDockableEvent = procedure(DockClient: TJvDockClient; DockForm: TForm;
    DockServer: TJvDockServer; DockPanel: TJvDockPanel; var CanDock: Boolean) of object; {NEW!}
  TJvDockTabHostFormCreatedEvent = procedure(DockClient: TJvDockClient;
    TabHost: TJvDockTabHostForm) of object;
  TJvDockConjoinHostFormCreatedEvent = procedure(DockClient: TJvDockClient;
    TabHost: TJvDockConjoinHostForm) of object;

  TJvDockGetClientAlignSizeEvent = procedure(Align: TAlign; var Value: Integer) of object;
  TJvDockFinishSetDockPanelSizeEvent = procedure(DockPanel: TJvDockPanel) of object;

  {

    TJvDockServer is the Creator of 4 panels and 4 splitters that are placed on
    the form that contains the TJvDockServer. The type of the panels and splitters
    is determined by the DockStyle.

    o  TJvDockServer maintains the panels and splitters. If the dock server is
       destroyed then the panels and splitters are destroyed.
    o  If the DockStyle is changed then the panels+splitters are destroyed and
       recreated.
    o  If DockStyle is set to nil then the panels+splitters are nil.
  }

  TJvDockPosition = (dpLeft, dpRight, dpTop, dpBottom, dpCustom); {dpCustom NEW!}

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvDockServer = class(TJvDockBaseControl)
  private
    FDockPanelClass: TJvDockPanelClass;
    FDockPanels: array [TJvDockPosition] of TJvDockPanel;
    FDockSplitterClass: TJvDockSplitterClass;
    FSplitters: array [TJvDockPosition] of TJvDockSplitter;
    FSplitterStyles: array [TJvDockPosition] of TJvDockSplitterStyle;
    FOnCustomPanel: TJvDockCustomPanelEvent; // Self, AParent, Align
    FOnGetClientAlignSize: TJvDockGetClientAlignSizeEvent;
    FOnFinishSetDockPanelSize: TJvDockFinishSetDockPanelSizeEvent;
    FAutoFocusDockedForm: Boolean;
    procedure CreateDockPanelAndSplitter;
    procedure DestroyDockPanelAndSplitter;
    procedure CreateSplitterStyle;
    procedure DestroySplitterStyle;
    procedure SetSplitterStyles;
    procedure DoGetClientAlignControl(Align: TAlign; var Value: Integer);
    function GetDockPanel(DockPosition: TJvDockPosition): TJvDockPanel;
    function GetDockPanelIndex(const Index: Integer): TJvDockPanel;
    function GetDockPanelWithAlign(Index: TAlign): TJvDockPanel;
    function GetDockSplitterWithAlign(Index: TAlign): TJvDockSplitter;
    function GetSplitter(DockPosition: TJvDockPosition): TJvDockSplitter;
    function GetSplitterIndex(const Index: Integer): TJvDockSplitter;
    function GetSplitterStyle(DockPosition: TJvDockPosition): TJvDockSplitterStyle;
    function GetSplitterStyleIndex(const Index: Integer): TJvDockSplitterStyle;
    procedure SetSplitterStyle(DockPosition: TJvDockPosition; ASplitterStyle: TJvDockSplitterStyle);
    procedure SetSplitterStyleIndex(const Index: Integer; ASplitterStyle: TJvDockSplitterStyle);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoFinishSetDockPanelSize(DockPanel: TJvDockPanel);
    procedure DoFloatDockClients(DockPanel: TJvDockPanel);
    procedure SetBottomDock(const Value: Boolean); override;
    procedure SetEnableDock(const Value: Boolean); override;
    procedure SetLeftDock(const Value: Boolean); override;
    procedure SetRightDock(const Value: Boolean); override;
    procedure SetTopDock(const Value: Boolean); override;

    procedure AddDockStyle(ADockStyle: TJvDockBasicStyle); override;
    procedure RemoveDockStyle(ADockStyle: TJvDockBasicStyle); override;

    procedure WMActivate(var Msg: TWMActivate);
    procedure WindowProc(var Msg: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetClientAlignControl(Align: TAlign): Integer;

    property DockPanel[DockPosition: TJvDockPosition]: TJvDockPanel read GetDockPanel;
    property Splitter[DockPosition: TJvDockPosition]: TJvDockSplitter read GetSplitter;
    property SplitterStyle[DockPosition: TJvDockPosition]: TJvDockSplitterStyle read GetSplitterStyle write
      SetSplitterStyle;

    property LeftDockPanel: TJvDockPanel index 0 read GetDockPanelIndex;
    property RightDockPanel: TJvDockPanel index 1 read GetDockPanelIndex;
    property TopDockPanel: TJvDockPanel index 2 read GetDockPanelIndex;
    property BottomDockPanel: TJvDockPanel index 3 read GetDockPanelIndex;
    property CustomDockPanel: TJvDockPanel index 4 read GetDockPanelIndex;

    property LeftSplitter: TJvDockSplitter index 0 read GetSplitterIndex;
    property RightSplitter: TJvDockSplitter index 1 read GetSplitterIndex;
    property TopSplitter: TJvDockSplitter index 2 read GetSplitterIndex;
    property BottomSplitter: TJvDockSplitter index 3 read GetSplitterIndex;

    property DockPanelWithAlign[Index: TAlign]: TJvDockPanel read GetDockPanelWithAlign;
    property DockSplitterWithAlign[Index: TAlign]: TJvDockSplitter read GetDockSplitterWithAlign;
  published
    property LeftSplitterStyle: TJvDockSplitterStyle index 0 read GetSplitterStyleIndex write SetSplitterStyleIndex;
    property RightSplitterStyle: TJvDockSplitterStyle index 1 read GetSplitterStyleIndex write SetSplitterStyleIndex;
    property TopSplitterStyle: TJvDockSplitterStyle index 2 read GetSplitterStyleIndex write SetSplitterStyleIndex;
    property BottomSplitterStyle: TJvDockSplitterStyle index 3 read GetSplitterStyleIndex write SetSplitterStyleIndex;
    property AutoFocusDockedForm: Boolean read FAutoFocusDockedForm write FAutoFocusDockedForm default True;
    property EnableDock;
    property LeftDock;
    property TopDock;
    property RightDock;
    property BottomDock;
    property DockStyle;
    property CustomDock;
    property OnGetClientAlignSize: TJvDockGetClientAlignSizeEvent read FOnGetClientAlignSize
      write FOnGetClientAlignSize;
    property OnFinishSetDockPanelSize: TJvDockFinishSetDockPanelSizeEvent read FOnFinishSetDockPanelSize
      write FOnFinishSetDockPanelSize;
    property OnCustomPanel: TJvDockCustomPanelEvent read FOnCustomPanel write FOnCustomPanel; {NEW!}
  end;

  TJvDockMouseStation = (msFloat, msConjoin, msTabPage);

  TJvDockNCButtonEvent = procedure(DockClient: TJvDockClient; Button: TMouseButton;
    X, Y: Smallint; HitTest: Longint; MouseStation: TJvDockMouseStation) of object;
  TJvDockNCButtonDownEvent = TJvDockNCButtonEvent;
  TJvDockNCButtonUpEvent = TJvDockNCButtonEvent;
  TJvDockNCButtonDblClkEvent = TJvDockNCButtonEvent;
  TJvDockNCMouseMoveEvent = procedure(DockClient: TJvDockClient;
    X, Y: Smallint; HitTest: Longint; MouseStation: TJvDockMouseStation) of object;
  TJvDockPaintDockEvent = procedure(Canvas: TCanvas;
    Control: TControl; const ARect: TRect) of object;
  TJvDockPaintDockGrabberEvent = TJvDockPaintDockEvent;
  TJvDockPaintDockSplitterEvent = TJvDockPaintDockEvent;
  TJvDockFormHintEvent = procedure(HTFlag: Integer; var HintStr: string; var CanShow: Boolean) of object;


  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvDockClient = class(TJvDockBaseControl)
  private
    FConjoinPanelClass: TJvDockConjoinPanelClass;
    FTabDockClass: TJvDockTabClass;
    FParentVisible: Boolean;
    FNCPopupMenu: TPopupMenu;
    FDirectDrag: Boolean;
    FShowHint: Boolean;
    FCanFloat: Boolean;
    FDockLevel: Integer;
    FEnableCloseButton: Boolean;
    FOnNCButtonDown: TJvDockNCButtonDownEvent;
    FOnNCButtonUp: TJvDockNCButtonUpEvent;
    FOnNCMouseMove: TJvDockNCMouseMoveEvent;
    FOnNCButtonDblClk: TJvDockNCButtonDblClkEvent;
    FOnPaintDockGrabber: TJvDockPaintDockGrabberEvent;
    FOnPaintDockSplitter: TJvDockPaintDockSplitterEvent;
    FOnFormShowHint: TJvDockFormHintEvent;
    FOnFormShow: TNotifyEvent;
    FOnFormHide: TNotifyEvent;
    FOnCheckIsDockable: TJvDockCheckDockableEvent; {NEW!}
    FOnTabHostFormCreated: TJvDockTabHostFormCreatedEvent; {NEW!}
    FOnConjoinHostFormCreated: TJvDockConjoinHostFormCreatedEvent; {NEW!}
    FCurrentDockSite: TWinControl;
    FLastDockSite: TWinControl;
    FUnDockLeft: Integer;
    FUnDockTop: Integer;
    FVSPaneWidth: Integer;
    {$IFDEF RTL330_UP}
    FDPIChangedMessageID: Integer;
    FCurrentPPI: Integer;
    {$ENDIF RTL330_UP}
    procedure SetParentVisible(const Value: Boolean);
    function GetLRDockWidth: Integer;
    function GetTBDockHeight: Integer;
    procedure SetLRDockWidth(const Value: Integer);
    procedure SetTBDockHeight(const Value: Integer);
    procedure SetNCPopupMenu(Value: TPopupMenu);
    procedure WMNCLButtonDown(var Msg: TWMNCHitMessage);
    procedure WMNCLButtonUp(var Msg: TWMNCHitMessage);
    procedure WMNCLButtonDblClk(var Msg: TWMNCHitMessage);
    procedure WMNCMButtonDown(var Msg: TWMNCHitMessage);
    procedure WMNCMButtonUp(var Msg: TWMNCHitMessage);
    procedure WMNCMButtonDblClk(var Msg: TWMNCHitMessage);
    procedure WMNCRButtonDown(var Msg: TWMNCHitMessage);
    procedure WMNCRButtonUp(var Msg: TWMNCHitMessage);
    procedure WMNCRButtonDblClk(var Msg: TWMNCHitMessage);
    procedure WMNCMouseMove(var Msg: TWMNCHitMessage);
    procedure CMVisibleChanged(var Msg: TMessage);
    procedure SetCurrentDockSite(const Value: TWinControl);
    procedure SetLastDockSite(ALastDockSite: TWinControl);
    procedure SetVSPaneWidth(const Value: Integer);
    procedure SetUnDockLeft(const Value: Integer);
    procedure SetUnDockTop(const Value: Integer);
    function GetDockState: Integer;
    procedure SetCanFloat(const Value: Boolean);
    procedure SetDockLevel(const Value: Integer);
    procedure SetEnableCloseButton(const Value: Boolean);
    {$IFDEF RTL330_UP}
    procedure DPIChangedMessageHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
    {$ENDIF RTL330_UP}
  protected
    procedure DoMenuPopup(X, Y: Integer); virtual;
    procedure Deactivate; virtual;
    procedure Activate; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoFloatDockClients(PanelAlign: TAlign);
    procedure DoFloatDockEachOther;
    procedure SetBottomDock(const Value: Boolean); override;
    procedure SetEachOtherDock(const Value: Boolean); override;
    procedure SetEnableDock(const Value: Boolean); override;
    procedure SetLeftDock(const Value: Boolean); override;
    procedure SetRightDock(const Value: Boolean); override;
    procedure SetTopDock(const Value: Boolean); override;
    procedure DoFormOnClose(Sender: TObject; var Action: TCloseAction); override;
    procedure AddDockStyle(ADockStyle: TJvDockBasicStyle); override;
    procedure RemoveDockStyle(ADockStyle: TJvDockBasicStyle); override;
    procedure WMSize(var Msg: TWMSize);
    procedure WMActivate(var Msg: TWMActivate);
    procedure WindowProc(var Msg: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FormStartDock(var Source: TJvDockDragDockObject); virtual;
    procedure FormPositionDockRect(Source: TJvDockDragDockObject); virtual;
    procedure FormDockOver(Source: TJvDockDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); virtual;
    procedure FormDockDrop(Source: TJvDockDragDockObject; X, Y: Integer); virtual;
    procedure FormEndDock(Target: TObject; X, Y: Integer); virtual;
    function FormUnDock(NewTarget: TWinControl; Client: TControl): Boolean; virtual;
    procedure FormGetSiteInfo(Source: TJvDockDragDockObject;
      Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
      var CanDock: Boolean); virtual;
    procedure FormGetDockEdge(Source: TJvDockDragDockObject; MousePos: TPoint; var DropAlign: TAlign); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure MakeShowEvent;
    procedure MakeHideEvent;
    function CreateConjoinPanelClass(ConjoinHost: TForm): TJvDockConjoinPanel;
    function CreateTabDockClass(TabHost: TForm): TJvDockTabPageControl;
    function CreateConjoinHostAndDockControl(Control1, Control2: TControl;
      DockType: TAlign): TJvDockConjoinHostForm; virtual;
    function CreateTabHostAndDockControl(Control1, Control2: TControl): TJvDockTabHostForm; virtual;

    function FindTabHostForm: TForm;
    // return nil if not found, otherwise, get currently docked parent tabhost form if there is one.

    procedure DoNCButtonDown(Msg: TWMNCHitMessage; Button: TMouseButton;
      MouseStation: TJvDockMouseStation); virtual;
    procedure DoNCButtonUp(Msg: TWMNCHitMessage; Button: TMouseButton;
      MouseStation: TJvDockMouseStation); virtual;
    procedure DoNCMouseMove(Msg: TWMNCHitMessage;
      MouseStation: TJvDockMouseStation); virtual;
    procedure DoNCButtonDblClk(Msg: TWMNCHitMessage; Button: TMouseButton;
      MouseStation: TJvDockMouseStation); virtual;

    procedure DoPaintDockGrabber(Canvas: TCanvas;
      Control: TControl; const ARect: TRect);
    procedure DoPaintDockSplitter(Canvas: TCanvas;
      Control: TControl; const ARect: TRect);
    procedure DoFormShowHint(HTFlag: Integer; var HintStr: string; var CanShow: Boolean);
    procedure ShowParentForm;
    procedure HideParentForm;
    procedure RestoreChild;
    property VSPaneWidth: Integer read FVSPaneWidth write SetVSPaneWidth;
    property ParentVisible: Boolean read FParentVisible write SetParentVisible;
    { (rb) not used? }
    property CurrentDockSite: TWinControl read FCurrentDockSite write SetCurrentDockSite;
    property LastDockSite: TWinControl read FLastDockSite write SetLastDockSite;
    property UnDockLeft: Integer read FUnDockLeft write SetUnDockLeft;
    property UnDockTop: Integer read FUnDockTop write SetUnDockTop;
    property DockState: Integer read GetDockState;
  published
    property LRDockWidth: Integer read GetLRDockWidth write SetLRDockWidth default 100;
    property TBDockHeight: Integer read GetTBDockHeight write SetTBDockHeight default 100;
    property NCPopupMenu: TPopupMenu read FNCPopupMenu write SetNCPopupMenu;
    property DirectDrag: Boolean read FDirectDrag write FDirectDrag;
    property ShowHint: Boolean read FShowHint write FShowHint default True;
    property CanFloat: Boolean read FCanFloat write SetCanFloat default True;
    { Not implemented; intention: only forms with the same DockLevel could be docked together }
    property DockLevel: Integer read FDockLevel write SetDockLevel default 0;
    property EnableCloseButton: Boolean read FEnableCloseButton write SetEnableCloseButton default True;
    property EnableDock;
    property LeftDock;
    property TopDock;
    property RightDock;
    property BottomDock;
    property EachOtherDock;
    property DockStyle;
    property CustomDock;
    property OnFormShow: TNotifyEvent read FOnFormShow write FOnFormShow;
    property OnFormHide: TNotifyEvent read FOnFormHide write FOnFormHide;
    property OnCheckIsDockable: TJvDockCheckDockableEvent read FOnCheckIsDockable write FOnCheckIsDockable; {NEW!}
    property OnTabHostFormCreated: TJvDockTabHostFormCreatedEvent read FOnTabHostFormCreated
      write FOnTabHostFormCreated; {NEW!}
    property OnConjoinHostFormCreated: TJvDockConjoinHostFormCreatedEvent read FOnConjoinHostFormCreated
      write FOnConjoinHostFormCreated; {NEW!}
    property OnNCButtonDown: TJvDockNCButtonDownEvent read FOnNCButtonDown write FOnNCButtonDown;
    property OnNCButtonUp: TJvDockNCButtonUpEvent read FOnNCButtonUp write FOnNCButtonUp;
    property OnNCMouseMove: TJvDockNCMouseMoveEvent read FOnNCMouseMove write FOnNCMouseMove;
    property OnNCButtonDblClk: TJvDockNCButtonDblClkEvent read FOnNCButtonDblClk write FOnNCButtonDblClk;
    property OnPaintDockGrabber: TJvDockPaintDockGrabberEvent read FOnPaintDockGrabber write FOnPaintDockGrabber;
    property OnPaintDockSplitter: TJvDockPaintDockSplitterEvent read FOnPaintDockSplitter write FOnPaintDockSplitter;
    property OnFormShowHint: TJvDockFormHintEvent read FOnFormShowHint write FOnFormShowHint;
  end;

  { Maintained by a TJvDockConjoinHostForm; That is (always) the owner, ie.
    that is assumed on multiple places in the code) }
  TJvDockConjoinPanel = class(TJvDockCustomPanel)
  private
    function GetDockClient: TJvDockClient;
    function GetParentForm: TJvDockConjoinHostForm;
    procedure CMUnDockClient(var Msg: TCMUnDockClient); message CM_UNDOCKCLIENT;
  protected
    function CreateDockManager: IDockManager; override;
    procedure ReloadDockedControl(const AControlName: string; var AControl: TControl); override;
    procedure CustomStartDock(var Source: TJvDockDragDockObject); override;
    procedure CustomGetSiteInfo(Source: TJvDockDragDockObject;
      Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
      var CanDock: Boolean); override;
    procedure CustomDockOver(Source: TJvDockDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure CustomPositionDockRect(Source: TJvDockDragDockObject; X, Y: Integer); override;
    procedure CustomDockDrop(Source: TJvDockDragDockObject; X, Y: Integer); override;
    procedure CustomEndDock(Target: TObject; X, Y: Integer); override;
    function CustomUnDock(Source: TJvDockDragDockObject; NewTarget: TWinControl;
      Client: TControl): Boolean; override;
    procedure DoDockOver(Source: TDragDockObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
      MousePos: TPoint; var CanDock: Boolean); override;
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
    { DockClient of the Owner }
    property DockClient: TJvDockClient read GetDockClient;
    { ParentForm is the Owner }
    property ParentForm: TJvDockConjoinHostForm read GetParentForm;
  end;

  TJvDockAdvConjoinPanel = class(TJvDockConjoinPanel)
  private
    procedure CMUnDockClient(var Msg: TCMUnDockClient); message CM_UNDOCKCLIENT;
  protected
    procedure CustomDockDrop(Source: TJvDockDragDockObject; X, Y: Integer); override;
    function CustomUnDock(Source: TJvDockDragDockObject; NewTarget: TWinControl;
      Client: TControl): Boolean; override;
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;
  public
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
  end;

  { Maintained by a TJvDockTabHostForm; That is (always) the owner, ie.
    that is assumed on multiple places in the code) }
  TJvDockTabPageControl = class(TJvDockPageControl)
  private
    FVersion: Integer;
    FStyleLink: TJvDockStyleLink;
    function GetParentForm: TJvDockTabHostForm;
    procedure DockStyleChanged(Sender: TObject);
    function GetDockStyle: TJvDockObservableStyle;
    function GetActiveDockForm: TCustomForm;
    function GetDockForm(Index: Integer): TCustomForm;
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure ReloadDockedControl(const AControlName: string;
      var AControl: TControl); override;
    procedure CustomStartDock(var Source: TJvDockDragDockObject); override;
    procedure CustomGetSiteInfo(Source: TJvDockDragDockObject; Client: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean); override;
    procedure CustomDockOver(Source: TJvDockDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure CustomPositionDockRect(Source: TJvDockDragDockObject; X, Y: Integer); override;
    procedure CustomDockDrop(Source: TJvDockDragDockObject; X, Y: Integer); override;
    procedure CustomEndDock(Target: TObject; X, Y: Integer); override;
    function CustomUnDock(Source: TJvDockDragDockObject; NewTarget: TWinControl;
      Client: TControl): Boolean; override;
    procedure CustomGetDockEdge(Source: TJvDockDragDockObject; MousePos: TPoint;
      var DropAlign: TAlign); override;
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;

    procedure SyncWithStyle; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure ScaleForPPI(NewPPI: Integer); override;

    property ActiveDockForm: TCustomForm read GetActiveDockForm;
    property DockForm[Index: Integer]: TCustomForm read GetDockForm;
    { ParentForm is the Owner }
    property ParentForm: TJvDockTabHostForm read GetParentForm;
    property TabPosition;
    property DockStyle: TJvDockObservableStyle read GetDockStyle;
  end;

  TJvDockAdvTabPageControl = class(TJvDockTabPageControl)
  private
    procedure CMUnDockClient(var Msg: TCMUnDockClient); message CM_UNDOCKCLIENT;
  protected
    procedure CustomDockDrop(Source: TJvDockDragDockObject; X, Y: Integer); override;
    function CustomUnDock(Source: TJvDockDragDockObject; NewTarget: TWinControl;
      Client: TControl): Boolean; override;
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;
  public
    destructor Destroy; override;
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
  end;

  { A TJvDockableForm is a base class for TJvDockConjoinHostForm
     and TJvDockTabHostForm which are the base classes for the two kinds of
     docked-views possible for handling multiple controls docked to the same
     dock site.
     This form is not meant to be visible to the user nor to contain any
     visible components, it is just part of the docking framework.
     This is the reason why it does not inherit from TJvForm, and it were
     inheriting from it, all sorts of weird bugs would show up (Mantis 5023)
  }
  TJvDockableForm = class(TForm) { DO NOT MAKE THIS TJvForm! }
  private
    FDockClient: TJvDockClient;
    FDockableControl: TWinControl;
    FUnDockControl: TControl;
    FFloatingChild: TControl;
    function GetDockableControl: TWinControl;
    procedure SetDockableControl(const Value: TWinControl);
    procedure SetUnDockControl(const Value: TControl);
  protected
    procedure DoClose(var Action: TCloseAction); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Fixed DockClient owned by the component }
    property DockClient: TJvDockClient read FDockClient;
    { Either a TJvDockTabPageControl or a TJvDockConjoinPanel, assigned on
      construction of the dockable form }
    property DockableControl: TWinControl read GetDockableControl write SetDockableControl;
    { ?? Probably needs notification }
    property UnDockControl: TControl read FUnDockControl write SetUnDockControl;
    { ?? Probably needs notification }
    property FloatingChild: TControl read FFloatingChild;
  end;

  TJvDockConjoinHostForm = class(TJvDockableForm)
  private
    FPanel: TJvDockConjoinPanel;
  protected
    procedure DoClose(var Action: TCloseAction); override;
  public
    constructor Create(AOwner: TComponent); override;
    property DockClient;
    { Constructed in TJvDockClient.CreateConjoinPanelClass }
    property Panel: TJvDockConjoinPanel read FPanel write FPanel;
  end;

  TJvDockTabHostForm = class(TJvDockableForm)
  private
    FPageControl: TJvDockTabPageControl;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ShowDockedControl(AControl:TWinControl); virtual; // If aControl is docked in PageControl, change PageControl to that page. NEW! WPostma.
    procedure UpdateCaption(AControl:TWinControl); virtual; // update tab host's tabs and title bar when page caption changes.
    procedure DoDock(NewDockSite: TWinControl; var ARect: TRect); override;

    // backwards compatibility: Mantis 4100
    function GetActiveDockForm: TForm;
    property DockClient;
    { Constructed in TJvDockClient.CreateTabDockClass }
    property PageControl: TJvDockTabPageControl read FPageControl write FPageControl;
  end;

  TJvGlobalDockManager = class(TJvDockManager)
  private
    FDockServers: TList;
    FDockClients: TList;
    FDockableForms: TList;
    function GetDockClient(const Index: Integer): TJvDockClient;
    function GetDockClientCount: Integer;
    function GetDockServer(const Index: Integer): TJvDockServer;
    function GetDockServerCount: Integer;
    function GetDockableForm(const Index: Integer): TJvDockableForm;
    function GetDockableFormCount: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure RegisterDockServer(ADockServer: TJvDockServer);
    procedure RegisterDockClient(ADockClient: TJvDockClient);
    procedure RegisterDockableForm(ADockableForm: TJvDockableForm);
    procedure UnRegisterDockServer(ADockServer: TJvDockServer);
    procedure UnRegisterDockClient(ADockClient: TJvDockClient);
    procedure UnRegisterDockableForm(ADockableForm: TJvDockableForm);
    function FindDockServerForm(const AName: string): TControl;
    function FindDockClientForm(const AName: string): TControl;
    function FindDockControlForm(const AName: string): TControl;

    property DockServer[const Index: Integer]: TJvDockServer read GetDockServer;
    property DockClient[const Index: Integer]: TJvDockClient read GetDockClient;
    property DockableForm[const Index: Integer]: TJvDockableForm read GetDockableForm;
    property DockServerCount: Integer read GetDockServerCount;
    property DockClientCount: Integer read GetDockClientCount;
    property DockableFormCount: Integer read GetDockableFormCount;
  end;

var
  DefaultDockPanelClass: TJvDockPanelClass = TJvDockPanel;
  DefaultDockSplitterClass: TJvDockSplitterClass = TJvDockSplitter;
  DefaultConjoinPanelClass: TJvDockConjoinPanelClass = TJvDockConjoinPanel;
  DefaultTabDockClass: TJvDockTabClass = TJvDockTabPageControl;
  DefaultDockZoneClass: TJvDockZoneClass = TJvDockZone;
  DefaultDockTreeClass: TJvDockTreeClass = TJvDockTree;

procedure ShowDockForm(DockWindow: TWinControl);
procedure HideDockForm(DockWindow: TWinControl);
procedure MakeDockClientEvent(Host: TControl; Visible: Boolean);
function GetFormVisible(DockWindow: TWinControl): Boolean;
procedure SetDockPageControlPopupMenu(Value: TPopupMenu);
procedure SetDockPageControlHotTrack(Value: Boolean);
procedure SetTabDockHostBorderStyle(Value: TFormBorderStyle);
procedure SetConjoinDockHostBorderStyle(Value: TFormBorderStyle);

// Save Layout to JvAppStorage:
procedure SaveDockTreeToAppStorage(AppStorage: TJvCustomAppStorage; AppStoragePath: string = '');
// Load Layout from JvAppStorage:
procedure LoadDockTreeFromAppStorage(AppStorage: TJvCustomAppStorage; AppStoragePath: string = '');

procedure SaveDockTreeToFile(FileName: string);
procedure LoadDockTreeFromFile(FileName: string);
procedure SaveDockTreeToReg(ARootKey: DWORD; RegPatch: string);
procedure LoadDockTreeFromReg(ARootKey: DWORD; RegPatch: string);

function FindDockBaseControl(Client: TControl): TJvDockBaseControl;
function FindDockClient(Client: TControl): TJvDockClient;
function FindDockServer(Client: TControl): TJvDockServer;

function IsDockable(Sender: TWinControl; Client: TControl;
  DropCtl: TControl = nil; DockAlign: TAlign = alNone): Boolean;
function ComputeDockingRect(AControl: TControl;
  var DockRect: TRect; MousePos: TPoint): TAlign;

{ Undocks AControl from Sender; Sender is used to calculate the rectangle in
  which AControl is displayed when it starts floating. }
procedure DoFloat(Sender, AControl: TControl);
procedure SetDockSite(Control: TWinControl; SiteValue: Boolean);
procedure DoFloatForm(DockForm: TControl);
procedure FreeAllDockableForm;
procedure DoFloatAllForm;

function GetClientAlignControlArea(AControl: TWinControl; Align: TAlign; Exclude: TControl = nil): Integer;
procedure ResetDockClient(Control: TControl; NewTarget: TControl); overload;
procedure ResetDockClient(DockClient: TJvDockClient; NewTarget: TControl); overload;

{ Quick way to do tabbed docking programmatically - Added by Warren. New implementation Jan 2010 }
function ManualTabDock(DockSite: TWinControl; Form1, Form2: TForm): TJvDockTabHostForm;

function _ManualTabDock(DockSite: TWinControl; Form1, Form2: TForm;oldTechnique:Boolean=false): TJvDockTabHostForm; {experimental}


{ Must create the initial tab dock with two pages, using ManualTabDock,
  then you can add more pages with this:}
procedure ManualTabDockAddPage(TabHost: TJvDockTabHostForm; AForm: TForm);
function ManualConjoinDock(DockSite: TWinControl; Form1, Form2: TForm): TJvDockConjoinHostForm;

function DockStateStr(DockState: Integer): string; {return string for a dock state}

procedure BeginDockLoading;
procedure EndDockLoading;
function JvGlobalDockIsLoading: Boolean;

{$IFNDEF COMPILER9_UP}
procedure InvalidateDockHostSiteOfControl(Control: TControl; FocusLost: Boolean);
{$ENDIF !COMPILER9_UP}

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
  Types, SysUtils,
  JclSysInfo,
  JvAppRegistryStorage, JvAppIniStorage, JvTypes,
  JvDockSupportProc, JvDockGlobals, JvDockInfo, JvDockVSNetStyle, JvJVCLUtils;

{$R JvDockableForm.dfm}
{$R JvDockConjoinHost.dfm}
{$R JvDockTabHost.dfm}

type
  TControlAccessProtected = class(TControl);
  TWinControlAccessProtected = class(TWinControl);

var
  DockPageControlPopupMenu: TPopupMenu = nil;
  DockPageControlHotTrack: Boolean = False;
  TabDockHostBorderStyle: TFormBorderStyle = bsSizeToolWin;
  ConjoinDockHostBorderStyle: TFormBorderStyle = bsSizeToolWin;

  GDockLoadCount: Integer = 0;
  GShowingChanged: TList;

//=== Local procedures =======================================================

function IsWinXP_UP: Boolean;
begin
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and JclCheckWinVersion(5, 1);
end;

procedure ApplyShowingChanged;
var
  I: Integer;
begin
  if IsWinXP_UP and (GShowingChanged <> nil) then
    for I := 0 to Screen.FormCount - 1 do
      if GShowingChanged.IndexOf(Screen.Forms[I]) >= 0 then
        Screen.Forms[i].Perform(CM_SHOWINGCHANGED, 0, 0);
  FreeAndNil(GShowingChanged);
end;

procedure UpdateCaption(Source: TWinControl; Exclude: TControl);
var
  I: Integer;
  Host: TJvDockableForm;
begin
  if (Source <> nil) and (Source.Parent is TJvDockableForm) then
  begin
    Host := TJvDockableForm(Source.Parent);
    Host.Caption := '';

    for I := 0 to Source.DockClientCount - 1 do
      if Source.DockClients[I].Visible and (Source.DockClients[I] <> Exclude) then
        Host.Caption := Host.Caption + TCustomForm(Source.DockClients[I]).Caption + RsDockStringSplitter;

    if Host.HostDockSite is TJvDockTabPageControl then
      with TJvDockTabPageControl(Host.HostDockSite) do
        if (ActivePage <> nil) and (ActivePage.Controls[0] = Source) then
          ActivePage.Caption := Host.Caption;
    UpdateCaption(Host.HostDockSite, nil);
  end;
end;

//=== Global procedures ======================================================

procedure BeginDockLoading;
begin
  if GDockLoadCount = 0 then
  begin
    JvDockLockWindow(nil);
  end;
  Inc(GDockLoadCount);
end;

procedure EndDockLoading;
begin
  Dec(GDockLoadCount);
  if GDockLoadCount = 0 then
  begin
    ApplyShowingChanged;
    JvDockUnLockWindow;
  end;
end;

function JvGlobalDockIsLoading: Boolean;
begin
  Result := GDockLoadCount > 0;
end;

function ComputeDockingRect(AControl: TControl; var DockRect: TRect; MousePos: TPoint): TAlign;
var
  DockTopRect, DockLeftRect, DockBottomRect, DockRightRect, DockCenterRect: TRect;
begin
  Result := alNone;

  if AControl = nil then
    Exit;
  with AControl do
  begin
    DockLeftRect.TopLeft := Point(0, 0);
    DockLeftRect.BottomRight := Point(ClientWidth div 5, ClientHeight);

    DockTopRect.TopLeft := Point(ClientWidth div 5, 0);
    DockTopRect.BottomRight := Point(ClientWidth div 5 * 4, ClientHeight div 5);

    DockRightRect.TopLeft := Point(ClientWidth div 5 * 4, 0);
    DockRightRect.BottomRight := Point(ClientWidth, ClientHeight);

    DockBottomRect.TopLeft := Point(ClientWidth div 5, ClientHeight div 5 * 4);
    DockBottomRect.BottomRight := Point(ClientWidth div 5 * 4, ClientHeight);

    DockCenterRect.TopLeft := Point(ClientWidth div 5, ClientHeight div 5);
    DockCenterRect.BottomRight := Point(ClientWidth div 5 * 4, ClientHeight div 5 * 4);

    if PtInRect(DockLeftRect, MousePos) then
    begin
      Result := alLeft;
      DockRect := DockLeftRect;
      DockRect.Right := ClientWidth div 2;
    end
    else
    if PtInRect(DockTopRect, MousePos) then
    begin
      Result := alTop;
      DockRect := DockTopRect;
      DockRect.Left := 0;
      DockRect.Right := ClientWidth;
      DockRect.Bottom := ClientHeight div 2;
    end
    else
    if PtInRect(DockRightRect, MousePos) then
    begin
      Result := alRight;
      DockRect := DockRightRect;
      DockRect.Left := ClientWidth div 2;
    end
    else
    if PtInRect(DockBottomRect, MousePos) then
    begin
      Result := alBottom;
      DockRect := DockBottomRect;
      DockRect.Left := 0;
      DockRect.Right := ClientWidth;
      DockRect.Top := ClientHeight div 2;
    end
    else
    if PtInRect(DockCenterRect, MousePos) then
    begin
      Result := alClient;
      DockRect := DockCenterRect;
    end;
    if Result = alNone then
      Exit;

    DockRect.TopLeft := ClientToScreen(DockRect.TopLeft);
    DockRect.BottomRight := ClientToScreen(DockRect.BottomRight);
  end;
end;

function DockStateStr(DockState: Integer): string;
begin
  // (rom) XML strings do not localize
  case DockState of
    JvDockState_Unknown:
      Result := 'Unknown';
    JvDockState_Docking:
      Result := 'Docking';
    JvDockState_Floating:
      Result := 'Floating';
  else
    Result := IntToStr(DockState);
  end;
end;

procedure DoFloat(Sender, AControl: TControl);
var
  ARect: TRect;
  CH, BW: Integer;
begin
  BW := JvDockGetSysBorderWidth;
  CH := JvDockGetSysCaptionHeight;

  ARect.TopLeft := Sender.ClientToScreen(Point(-(BW + 3), -(CH + BW + 1)));
  ARect.BottomRight := Sender.ClientToScreen(
    Point(Sender.UndockWidth - (BW + 3), Sender.UndockHeight - (BW + CH + 1)));
  AControl.ManualFloat(ARect);
  if (AControl.Left <> ARect.Left) or (AControl.Top <> ARect.Top) then
  begin
    AControl.Left := ARect.Left;
    AControl.Top := ARect.Top;
  end;
end;

procedure DoFloatAllForm;
var
  I: Integer;
  TempList: TList;
begin
  TempList := TList.Create;
  try
    for I := 0 to Screen.CustomFormCount - 1 do
      if not (Screen.CustomForms[I] is TJvDockableForm) and
        (Assigned(FinddockClient(Screen.CustomForms[I])) or
         Assigned(FinddockServer(Screen.CustomForms[I]))) then
        TempList.Add(Screen.CustomForms[I]);

    for I := 0 to TempList.Count - 1 do
      DoFloatForm(TempList[I]);
  finally
    TempList.Free;
  end;
  FreeAllDockableForm;
end;

procedure DoFloatForm(DockForm: TControl);
var
  I: TAlign;
  J: Integer;
  ADockServer: TJvDockServer;
  //  ARect: TRect;
  Channel: TJvDockVSChannel;
  allow:Boolean;
  dockClient:TJvDockClient;
begin
  if not (csDestroying in DockForm.ComponentState) then
  if DockForm is TForm then begin
	allow := true;
	if Assigned(TForm(DockForm).OnUnDock) then
	  TForm(DockForm).OnUnDock(DockForm,DockForm,
		TWinControl(nil),allow);
	if allow then begin
	  dockClient := FindDockClient(DockForm);
	  if Assigned(dockclient) and (not dockClient.CanFloat) then begin
		exit;
	  end;
	end;
	if not allow then begin
	  exit;
	end;
  end;

  if DockForm is TJvDockableForm then
  begin
    with TJvDockableForm(DockForm).DockableControl do
    begin
      for J := DockClientCount - 1 downto 0 do
        DoFloatForm(DockClients[J]);

      DockForm.ManualDock(nil);
    end;
  end
  else
  begin
    ADockServer := FindDockServer(DockForm);
    if ADockServer <> nil then
    begin
      // (rom) better use a Count or introduce one
      // (p3) this is due to the fact that DockPanel returns a dockpanel based on the indices 0 to 3
      //  DockPanelWithAlign uses the TAlign enumeration, however
      for I := alTop to alRight do
        if Assigned(ADockServer.DockPanelWithAlign[I]) then
        begin
          for J := ADockServer.DockPanelWithAlign[I].DockClientCount - 1 downto 0 do
            DoFloatForm(ADockServer.DockPanelWithAlign[I].DockClients[J]);
          if ADockServer.DockPanelWithAlign[I] is TJvDockVSNETPanel then
            with TJvDockVSNETPanel(ADockServer.DockPanelWithAlign[I]).VSChannel do
            begin
              RemoveAllBlock;
              HidePopupPanel(ActiveDockForm);
            end;
        end;
    end
    else
    begin
      if DockForm.HostDockSite <> nil then
      begin
        if (DockForm.HostDockSite.Parent is TJvDockableForm) and
          (DockForm.HostDockSite.DockClientCount <= 2) then
          PostMessage(DockForm.HostDockSite.Parent.Handle, WM_CLOSE, 0, 0);
      end;
      //      else
      //        ARect := DockForm.BoundsRect;

      Channel := RetrieveChannel(DockForm.HostDockSite);
      if Assigned(Channel) then
      begin
        Channel.RemoveDockControl(TWinControl(DockForm));
        DockForm.Dock(nil, Bounds(DockForm.Left, DockForm.Top, DockForm.UndockWidth, DockForm.UndockHeight));
      end
      else
        DockForm.ManualDock(nil);
    end;
  end;
end;

function FindDockBaseControl(Client: TControl): TJvDockBaseControl;
var
  I: Integer;
begin
  Result := nil;
  if Client <> nil then
    for I := 0 to Client.ComponentCount - 1 do
      if Client.Components[I] is TJvDockBaseControl then
      begin
        Result := TJvDockBaseControl(Client.Components[I]);
        Break;
      end;
end;

function FindDockClient(Client: TControl): TJvDockClient;
var
  ADockControl: TJvDockBaseControl;
begin
  ADockControl := FindDockBaseControl(Client);
  if ADockControl is TJvDockClient then
    Result := TJvDockClient(ADockControl)
  else
    Result := nil;
end;

function FindDockServer(Client: TControl): TJvDockServer;
var
  ADockControl: TJvDockBaseControl;
begin
  ADockControl := FindDockBaseControl(Client);
  if ADockControl is TJvDockServer then
    Result := TJvDockServer(ADockControl)
  else
    Result := nil;
end;

procedure FreeAllDockableForm;
var
  I: Integer;
begin
  Assert(JvGlobalDockManager <> nil);
  for I := JvGlobalDockManager.DockableFormCount - 1 downto 0 do
    if JvGlobalDockManager.DockableForm[I].DockableControl.DockClientCount = 0 then
      JvGlobalDockManager.DockableForm[I].Free;
end;

function GetClientAlignControlArea(AControl: TWinControl; Align: TAlign; Exclude: TControl): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to AControl.ControlCount - 1 do
    if (AControl.Controls[I].Align = Align) and AControl.Controls[I].Visible and
      (AControl.Controls[I] <> Exclude) and
      not ((AControl.Controls[I] is TJvDockSplitter) or (AControl.Controls[I] is TJvDockPanel)) then
      if Align in [alLeft, alRight] then
        Inc(Result, AControl.Controls[I].Width)
      else
        Inc(Result, AControl.Controls[I].Height);
end;

function GetFormVisible(DockWindow: TWinControl): Boolean;
var
  ADockClient: TJvDockClient;
begin
  Result := True;
  ADockClient := FindDockClient(DockWindow);
  if ADockClient <> nil then
    Result := ADockClient.DockStyle.GetDockFormVisible(ADockClient);
end;

procedure HideDockForm(DockWindow: TWinControl);
var
  ADockClient: TJvDockClient;
begin
  // delegate to style
  ADockClient := FindDockClient(DockWindow);
  if Assigned(ADockClient) and Assigned(ADockClient.DockStyle) then
    ADockClient.DockStyle.DoHideDockForm(DockWindow);
end;

procedure TJvDockBasicStyle.DoHideDockForm(DockWindow: TWinControl);

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

begin
  HideDockChild(DockWindow);
  HideDockParent(DockWindow);
  if (DockWindow.HostDockSite is TJvDockCustomControl) then
    TJvDockCustomControl(DockWindow.HostDockSite).UpdateCaption(DockWindow);
end;

{$IFNDEF COMPILER9_UP}
procedure InvalidateDockHostSiteOfControl(Control: TControl; FocusLost: Boolean);
var
  ChildDockSite: TControl;
  ParentWalk: TWinControl;
begin
  { Invalidate the first dock site we come across; its ui may
    need updating to reflect which control has focus. }
  if Control = nil then
    Exit;
  ChildDockSite := Control;
  ParentWalk := Control.Parent;
  while (ChildDockSite.HostDockSite = nil) and (ParentWalk <> nil) do
  begin
    ChildDockSite := ParentWalk;
    ParentWalk := ParentWalk.Parent;
  end;
  if ChildDockSite <> nil then
    TWinControlAccessProtected(ChildDockSite).SendDockNotification(CM_INVALIDATEDOCKHOST,
      WPARAM(Control), LPARAM(FocusLost));
end;
{$ENDIF !COMPILER9_UP}

function IsDockable(Sender: TWinControl; Client: TControl; DropCtl: TControl = nil;
  DockAlign: TAlign = alNone): Boolean;
var
  I: Integer;
  ADockClient: TJvDockClient;
  SenderDockStyle, ClientDockStyle: TJvDockBasicStyle;
  SenderStyleName, ClientStyleName: string;
  SenderDocPanel: TJvDockPanel;
  //s:String;
  // (rom) disabled unused
  //label
  // JudgeRelation;
begin
  ADockClient := FindDockClient(Client);
  Result := False;
  if (ADockClient <> nil) and (ADockClient.EnableDock) then
  begin
    if Sender is TJvDockPanel then
    begin
      SenderDocPanel := TJvDockPanel(Sender);
      with SenderDocPanel do
      begin
        {$IFDEF JVDOCK_DEBUG}
        if CustomFlag and (not ADockClient.CustomDock) then
        begin
          OutputDebugString('IsDockable() - Debugging Dock-to-custom-panel problem.');
          ADockClient.CustomDock := True; // BUG WORKAROUND. WPostma.
        end;
        {$ENDIF JVDOCK_DEBUG}
        Result := DockServer.EnableDock and
          (((Align = alLeft) and DockServer.LeftDock and (ADockClient.LeftDock)) or
          ((Align = alTop) and DockServer.TopDock and (ADockClient.TopDock)) or
          ((Align = alRight) and DockServer.RightDock and (ADockClient.RightDock)) or
          ((Align = alBottom) and DockServer.BottomDock and (ADockClient.BottomDock)) or
          ((CustomFlag) and Assigned(DockServer.CustomDockPanel) and (ADockClient.CustomDock))
          );

          //{$IFDEF JVDOCK_DEBUG}
          // XXXXXX very noisy during drag operations:
          //if Result then begin
          //    if Assigned(DropCtl) then
          //        s := DropCtl.Name
          //   else
          //        s := 'nil';
          //    OutputDebugString(PChar( 'JvDockControlForm.pas IsDockable: '+Sender.Name+ ': Dockable. DropCtl='+s ));
          //end;
          //{$ENDIF JVDOCK_DEBUG}
        SenderDockStyle := DockServer.DockStyle;
      end;
    end
    else
    begin
      if (Sender <> nil) and (Sender.Parent is TJvDockableForm) then
        with TJvDockableForm(Sender.Parent).DockableControl do
          for I := 0 to DockClientCount - 1 do
            if DockClients[I] = Client then
              Exit;
      Result := ADockClient.EachOtherDock;
      if Sender <> nil then
        ADockClient := FindDockClient(Sender.Parent);
      if ADockClient <> nil then
        Result := Result and ADockClient.EachOtherDock;

      if ADockClient <> nil then
        SenderDockStyle := ADockClient.DockStyle
      else
        Exit;
    end;

    ADockClient := FindDockClient(Client);
    if ADockClient <> nil then
      ClientDockStyle := ADockClient.DockStyle
    else
      Exit;

    if SenderDockStyle = nil then
      SenderStyleName := ''
    else
      SenderStyleName := SenderDockStyle.ClassName;

    if ClientDockStyle = nil then
      ClientStyleName := ''
    else
      ClientStyleName := ClientDockStyle.ClassName;

    Result := Result and (SenderStyleName = ClientStyleName);

    //JudgeRelation:
  end;
end;

procedure LoadDockTreeFromAppStorage(AppStorage: TJvCustomAppStorage; AppStoragePath: string = '');
var
  JvDockInfoTree: TJvDockInfoTree;
begin
  AppStorage.BeginUpdate;
  try
    HideAllPopupPanel(nil); {This is in JvDockVSNetStyle.pas }

    JvDockInfoTree := TJvDockInfoTree.Create(TJvDockInfoZone);
    try
      BeginDockLoading;
      try
        JvDockInfoTree.AppStorage := AppStorage;
        JvDockInfoTree.AppStoragePath := AppStoragePath;
        JvDockInfoTree.ReadInfoFromAppStorage;
      finally
        EndDockLoading;
      end;
    finally
      JvDockInfoTree.Free;
    end;
  finally
    AppStorage.EndUpdate;
  end;
end;

procedure LoadDockTreeFromFile(FileName: string);
var
  JvAppStorage: TJvAppIniFileStorage;
begin
  JvAppStorage := TJvAppIniFileStorage.Create(nil);
  try
    JvAppStorage.Location := flCustom;
    JvAppStorage.FileName := FileName;
    JvAppStorage.Reload;
    LoadDockTreeFromAppStorage(JvAppStorage);
  finally
    JvAppStorage.Free;
  end;
end;

procedure LoadDockTreeFromReg(ARootKey: DWORD; RegPatch: string);
var
  JvAppStorage: TJvAppRegistryStorage;
begin
  JvAppStorage := TJvAppRegistryStorage.Create(nil);
  try
    // (p3) this seems dangerous but it's the same method as used by TJvAppRegistryStorage
    JvAppStorage.RegRoot := TJvRegKey(HKEY_CLASSES_ROOT + ARootKey);
    JvAppStorage.Path := RegPatch;
    LoadDockTreeFromAppStorage(JvAppStorage);
  finally
    JvAppStorage.Free;
  end;
end;

procedure MakeDockClientEvent(Host: TControl; Visible: Boolean);
var
  I: Integer;
  DC: TJvDockClient;
begin
  DC := FindDockClient(Host);
  if DC <> nil then
  begin
    if Visible then
      DC.MakeShowEvent
    else
      DC.MakeHideEvent;
    if (Host is TJvDockableForm) and Host.Visible then
      with TJvDockableForm(Host).DockableControl do
        for I := 0 to DockClientCount - 1 do
          MakeDockClientEvent(DockClients[I], Visible);
  end;
end;

{ Quick way to do conjoined docking programmatically - Added by Warren }

function ManualConjoinDock(DockSite: TWinControl; Form1, Form2: TForm): TJvDockConjoinHostForm;
var
  ConjoinHost: TJvDockConjoinHostForm;
  DockClient1, DockCLient2: TJvDockClient;
begin
  Form1.Show;
  Form2.Show;
  DockClient1 := FindDockClient(Form1);
  Assert(Assigned(DockClient1));
  DockClient2 := FindDockClient(Form2);
  Assert(Assigned(DockClient2));
  ConjoinHost := DockClient1.CreateConjoinHostAndDockControl(DockClient1.ParentForm, Form2, alTop);
  ShowDockForm(Form2);
  ConjoinHost.ManualDock(DockSite);
  Result := ConjoinHost;
end;

type
  TWinControlAccess = class(TWinControl);



{ Contributed by Kiriakos. Improved version 2011-12-27 }
function ManualTabDock(DockSite: TWinControl; Form1, Form2: TForm): TJvDockTabHostForm;
var
  TabHost: TJvDockTabHostForm;
  DockClient1, DockCLient2: TJvDockClient;
  ScreenPos: TRect;
begin
  DockClient1 := FindDockClient(Form1);
  Form1.Hide;

  Assert(Assigned(DockClient1));

  if DockClient1.DockState = JvDockState_Docking then
  begin
    ScreenPos := Application.MainForm.ClientRect; // Just making it float temporarily.
    Form1.ManualFloat(ScreenPos);
  end;

  DockClient2 := FindDockClient(Form2);
  Assert(Assigned(DockClient2));
  Form2.Hide;

  if DockClient2.DockState = JvDockState_Docking then
  begin
    ScreenPos := Application.MainForm.ClientRect; // Just making it float temporarily.
    Form2.ManualFloat(ScreenPos);
  end;

  TabHost := DockClient1.CreateTabHostAndDockControl(Form1, Form2);

  TabHost.ManualDock(DockSite,nil,alClient);

  ShowDockForm(Form1);
  ShowDockForm(Form2);
  Result := TabHost;
end;


{_ManualTabDock:experimental}
function _ManualTabDock(DockSite: TWinControl; Form1, Form2: TForm;oldTechnique:Boolean): TJvDockTabHostForm;
var
  DockClient: TJvDockClient;
  HostForm: TForm;
  dockPanel:TJvDockPanel;
begin
  Assert(DockSite <> nil);

  if not (DockSite is TJvDockPanel) then begin
    raise EInvalidOperation.Create('ManualTabDock:DockSite must be TJvDockPanel');
  end;
  dockPanel := TJvDockPanel(DockSite);


  { This is an initial sanity check, but the actual DockClient is required later,
    so we can find the tab host form that contains it. }
  DockClient := FindDockClient(Form1);
  if DockClient = nil then
    raise EInvalidOperation.Create('ManualTabDock:DockClient not found. Form you are trying to dock must have a dock style');

  { This should create the tab host form, if the docking style supports tabbed docking,
    as all 'advanced' docking styles provided in the JVCL do provide.

    This is the same call used when you drag something with your mouse, so it
    is much more reliable, and consistent, and updates the DOckManager state
	which prevents all manner of weird problems. }

	if oldTechnique then begin
	  HostForm :=  dockPanel.FindTabHostForm as TForm;
	  if Assigned(HostForm) then begin
		ManualTabDockAddPage( TJvDockTabHostForm(HostForm), Form1 );
		ManualTabDockAddPage( TJvDockTabHostForm(HostForm), Form2 );
		result := TJvDockTabHostForm(HostForm);
		exit;
	  end;
	  // This is the original way I had it in 2006: It had bugs.
	  HostForm := DockClient.CreateTabHostAndDockControl(FOrm1,Form2);
	  FOrm1.Show;
	  FOrm2.Show;

	  HostForm.ManualDock(DockSite,nil,alClient);
	  HostForm.Show;

  end else begin
	  // This was the fix in 2008, which broke somehow, later:
	  TWinControlAccess(DockSite).DockManager.InsertControl(Form2, alClient, Form1);
  end;

  {$IFDEF JVDOCK_DEBUG}
  if not Assigned(Form1.Parent) then begin
	  OutputDebugString('no parent on form 1');
  end;
  if not Assigned(Form2.Parent) then begin
      OutputDebugString('no parent on form 2');
  end;
  {$ENDIF JVDOCK_DEBUG}

  { Now find and return the the new tab host object created depp within the bowels
    of the Docking Style code. If anything fails, return EInvalidOperation because its
    likely that whoever called ManualTabDock sent us objects that can not be properly
    docked, or is using a docking style that does not support tab docking. }

  HostForm := DockClient.FindTabHostForm;
  if HostForm = nil then
    raise EInvalidOperation.Create('ManualTabDock:TabHost not created. Your Docking Style may not support tabbed docking.');

	Result := HostForm as TJvDockTabHostForm; {not nil, we checked, so this won't fail.}
end;


(*
 This old way was a kludge written by Warren that never properly worked anyways.
 It had the odd habit of rearranging controls, making previous docked forms (controls)
  disappear when docking a new one, and all manner of bad stuff like that.

function Old_ManualTabDock(DockSite: TWinControl; Form1, Form2: TForm): TJvDockTabHostForm;
var
  TabHost: TJvDockTabHostForm;
  DockClient1, DockCLient2: TJvDockClient;
  ScreenPos: TRect;
//  otherForm:TForm;
//  n:Integer;
begin
  DockClient1 := FindDockClient(Form1);
  Form1.Hide;


  Assert(Assigned(DockClient1));

  if DockClient1.DockState = JvDockState_Docking then
  begin
    ScreenPos := Application.MainForm.ClientRect; // Just making it float temporarily.
    Form1.ManualFloat(ScreenPos); // This screws up on Delphi 2010.
  end;
  DockClient2 := FindDockClient(Form2);

    Form2.Hide;

  Assert(Assigned(DockClient2));
  if DockClient2.DockState = JvDockState_Docking then
  begin
    ScreenPos := Application.MainForm.ClientRect; // Just making it float temporarily.
    Form2.ManualFloat(ScreenPos);
  end;

  TabHost := DockClient1.CreateTabHostAndDockControl(Form1, Form2);



  {Mantis # 5023 workaround}
  TabHost.Show; { I have NO Idea why we need to call show here sometimes, and hide works here other times.}



  TabHost.ManualDock(DockSite,nil,alClient);
  if not Form1.Visible then
    Form1.Show;
  if not Form2.Visible then
    Form2.Show;

//  TabHost.Show; { problems if done here!}

  ShowDockForm(Form2);
  Result := TabHost;
end;

*)

{ Must create the initial tab dock with two pages, using ManualTabDock,
  then you can add more pages with this:}

procedure ManualTabDockAddPage(TabHost: TJvDockTabHostForm; AForm: TForm);
begin
  Assert(Assigned(TabHost));
  Assert(Assigned(TabHost.PageControl));
  //AForm.Show;
  AForm.ManualDock(TabHost.PageControl);
  AForm.Show;
end;

procedure ResetDockClient(DockClient: TJvDockClient; NewTarget: TControl);
//var
//  Pt: TPoint;
begin
  if (DockClient <> nil) and not (csDestroying in DockClient.ParentForm.ComponentState) then
  begin
    if not ((DockClient.ParentForm.HostDockSite is TJvDockPanel) and (NewTarget is TJvDockPanel)) then
    begin
      if (DockClient.LastDockSite is TJvDockPanel) and (NewTarget is TJvDockPanel) and
        (DockClient.LastDockSite <> NewTarget) then
        with TJvDockPanel(DockClient.LastDockSite) do
          if UseDockManager and (JvDockManager <> nil) then
            JvDockManager.RemoveControl(DockClient.ParentForm);

      //KV
      DockClient.LastDockSite := DockClient.ParentForm.HostDockSite;
      //if DockClient.ParentForm.HostDockSite is TJvDockPanel then
      //  DockClient.LastDockSite := DockClient.ParentForm.HostDockSite
      //else
      //  DockClient.LastDockSite := nil;

      if DockClient.ParentForm.HostDockSite = nil then
      begin
        DockClient.UnDockLeft := DockClient.ParentForm.BoundsRect.TopLeft.X;
        DockClient.UnDockTop := DockClient.ParentForm.BoundsRect.TopLeft.Y;
        //  If the form was docked do not change UnDockLeft UnDockTop
//      end
//      else
//      begin
//        Pt := DockClient.ParentForm.BoundsRect.TopLeft;
//        Pt := DockClient.ParentForm.HostDockSite.ClientToScreen(Pt);
//        DockClient.UnDockLeft := Pt.X;
//        DockClient.UnDockTop := Pt.Y;
      end;
    end;
  end;
end;

procedure ResetDockClient(Control: TControl; NewTarget: TControl);
begin
  ResetDockClient(FindDockClient(Control), NewTarget);
end;

 // Save Layout to JvAppStorage:
 //
 // Uses Global VCL object Screens to go through all forms, and save layout for
 // all the forms. Use any JvCustomAppStorage descendant to store to, such
 // as registry, ini files, etc.

procedure SaveDockTreeToAppStorage(AppStorage: TJvCustomAppStorage; AppStoragePath: string = '');
var
  JvDockInfoTree: TJvDockInfoTree;
  I: Integer;
begin
  AppStorage.BeginUpdate;
  try
    HideAllPopupPanel(nil); {This is in JvDockVSNetStyle.pas }
    JvDockInfoTree := TJvDockInfoTree.Create(TJvDockInfoZone);
    try
      for I := 0 to Screen.CustomFormCount - 1 do
        if (Screen.CustomForms[I].Parent = nil) and
          ((FindDockClient(Screen.CustomForms[I]) <> nil) or
           (FindDockServer(Screen.CustomForms[I]) <> nil)) then
        begin
          {$IFDEF JVDOCK_DEBUG}
          OutputDebugString(PChar('SaveDockTreeToAppStorage : Form ' + Screen.CustomForms[I].Name));
          {$ENDIF JVDOCK_DEBUG}
          JvDockInfoTree.CreateZoneAndAddInfoFromApp(Screen.CustomForms[I]);
        end;

      JvDockInfoTree.AppStorage := AppStorage;
      JvDockInfoTree.AppStoragePath := AppStoragePath;
      JvDockInfoTree.WriteInfoToAppStorage;
    finally
      JvDockInfoTree.Free;
    end;
  finally
    AppStorage.EndUpdate;
  end;
end;

procedure SaveDockTreeToFile(FileName: string);
var
  JvAppStorage: TJvAppIniFileStorage;
begin
  JvAppStorage := TJvAppIniFileStorage.Create(nil);
  try
    JvAppStorage.Location := flCustom;
    JvAppStorage.FileName := FileName;
    JvAppStorage.Reload;
    SaveDockTreeToAppStorage(JvAppStorage);
  finally
    JvAppStorage.Flush;
    JvAppStorage.Free;
  end;
end;

procedure SaveDockTreeToReg(ARootKey: DWORD; RegPatch: string);
var
  JvAppStorage: TJvAppRegistryStorage;
begin
  JvAppStorage := TJvAppRegistryStorage.Create(nil);
  try
    // (p3) this seems dangerous but it's the same method as used by TJvAppRegistryStorage
    JvAppStorage.RegRoot := TJvRegKey(HKEY_CLASSES_ROOT + ARootKey);
    JvAppStorage.Path := RegPatch;
    SaveDockTreeToAppStorage(JvAppStorage);
  finally
    JvAppStorage.Free;
  end;
end;

procedure SetConjoinDockHostBorderStyle(Value: TFormBorderStyle);
var
  I: Integer;
begin
  ConjoinDockHostBorderStyle := Value;
  for I := 0 to Screen.FormCount - 1 do
    if (Screen.CustomForms[I] is TJvDockConjoinHostForm) and (Screen.CustomForms[I].HostDockSite = nil) then
      TJvDockConjoinHostForm(Screen.CustomForms[I]).BorderStyle := Value;
end;

procedure SetDockPageControlHotTrack(Value: Boolean);
var
  I: Integer;
begin
  DockPageControlHotTrack := Value;
  for I := 0 to Screen.FormCount - 1 do
    if Screen.CustomForms[I] is TJvDockTabHostForm then
      TJvDockTabHostForm(Screen.CustomForms[I]).PageControl.HotTrack := Value;
end;

procedure SetDockPageControlPopupMenu(Value: TPopupMenu);
var
  I: Integer;
begin
  DockPageControlPopupMenu := Value;
  for I := 0 to Screen.FormCount - 1 do
    if Screen.CustomForms[I] is TJvDockTabHostForm then
      TJvDockTabHostForm(Screen.CustomForms[I]).PageControl.PopupMenu := Value;
end;

procedure SetDockSite(Control: TWinControl; SiteValue: Boolean);
begin
  TWinControlAccessProtected(Control).DockSite := SiteValue;
  if (not (csDesigning in Control.ComponentState)) and (JvGlobalDockManager <> nil) then
    JvGlobalDockManager.RegisterDockSite(Control, SiteValue);
end;

procedure SetTabDockHostBorderStyle(Value: TFormBorderStyle);
var
  I: Integer;
begin
  TabDockHostBorderStyle := Value;
  for I := 0 to Screen.FormCount - 1 do
    if (Screen.CustomForms[I] is TJvDockTabHostForm) and (Screen.CustomForms[I].HostDockSite = nil) then
      TJvDockTabHostForm(Screen.CustomForms[I]).BorderStyle := Value;
end;

procedure ShowDockForm(DockWindow: TWinControl);
var
  ADockClient: TJvDockClient;
begin
  // delegate to style
  ADockClient := FindDockClient(DockWindow);
  if Assigned(ADockClient) and Assigned(ADockClient.DockStyle) then
    ADockClient.DockStyle.DoShowDockForm(DockWindow);
end;

procedure TJvDockBasicStyle.DoShowDockForm(DockWindow: TWinControl);

  procedure ShowClient(Client, DockParent: TWinControl);
  var
    ADockClient: TJvDockClient;
    ADockServer: TJvDockServer;
    I: Integer;
  begin
    if (DockParent is TJvDockableForm) and (Client <> nil) then
    begin
      with TJvDockableForm(DockParent).DockableControl do
        for I := 0 to DockClientCount - 1 do
          if DockClients[I] <> Client then
            MakeDockClientEvent(DockClients[I], True);
      if Client.HostDockSite is TJvDockCustomControl then
        TJvDockCustomControl(Client.HostDockSite).UpdateCaption(nil);
    end
    else
    begin
      ADockClient := FindDockClient(DockParent);
      if (ADockClient <> nil) and (ADockClient.DockStyle <> nil) then
      begin
        ADockClient.DockStyle.ShowDockForm(ADockClient);
        ADockServer := FindDockServer(GetParentForm(DockParent));
        if (not Assigned(ADockServer) or ADockServer.AutoFocusDockedForm) and
          DockParent.CanFocus then
          DockParent.SetFocus;
      end;
    end;
    if DockParent.Parent = nil then
      SetForegroundWindow(DockParent.Handle);
  end;

  function ShowDockPanel(Client: TWinControl): TWinControl;
  begin
    Result := Client;
    if Assigned(Client) and (Client.HostDockSite is TJvDockPanel) then
    begin
      ShowClient(nil, Client);
      TJvDockPanel(Client.HostDockSite).ShowDockPanel(True, Client, sdfDockPanel);
      Result := nil;
    end;
  end;

  function ShowTabDockHost(Client: TWinControl): TWinControl;
  var
    I: Integer;
  begin
    Result := Client;
    if Assigned(Client) and (Client.HostDockSite is TJvDockTabPageControl) then
    begin
      ShowClient(nil, Client);

      with TJvDockTabPageControl(Client.HostDockSite) do
        for I := 0 to Count - 1 do
          if Pages[I].Controls[0] = Client then
          begin
            Pages[I].Show;
            Break;
          end;
      if (Client.HostDockSite <> nil) and not (Client.HostDockSite is TJvDockPanel) then
      begin
        Result := Client.HostDockSite.Parent;
        ShowClient(Client, Result);
        if (Result <> nil) and (Result.HostDockSite is TJvDockTabPageControl) then
          { (rb) never called AFAICS }
          Result := ShowTabDockHost(Result)
        else
          ShowClient(nil, Result);
      end;
    end;
  end;

  function ShowConjoinDockHost(Client: TWinControl): TWinControl;
  begin
    Result := Client;
    if Assigned(Client) and Assigned(Client.HostDockSite) and not (Client.HostDockSite is TJvDockPanel) then
    begin
      ShowClient(nil, Client);
      if Client.HostDockSite.Parent <> nil then
      begin
        Result := Client.HostDockSite.Parent;
        ShowClient(Client, Result);
        if (Result <> nil) and (Result.HostDockSite is TJvDockConjoinPanel) then
          { (rb) never called AFAICS }
          Result := ShowConjoinDockHost(Result)
        else
          ShowClient(nil, Result);
      end;
    end;
  end;

begin
  repeat
    { Show single floating window }
    if Assigned(DockWindow) and (DockWindow.HostDockSite = nil) then
      ShowClient(nil, DockWindow);
    DockWindow := ShowTabDockHost(DockWindow);
    DockWindow := ShowConjoinDockHost(DockWindow);
    { Show docked window }
    DockWindow := ShowDockPanel(DockWindow);
  until (DockWindow = nil) or (DockWindow.Parent = nil);
end;

//=== { TJvDockableForm } ====================================================

constructor TJvDockableForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DragKind := dkDock;
  FDockClient := TJvDockClient.Create(Self);
  JvGlobalDockManager.RegisterDockableForm(Self);
  FFloatingChild := nil;
  TBDockHeight := FDockClient.TBDockHeight;
  LRDockWidth := FDockClient.LRDockWidth;
end;

destructor TJvDockableForm.Destroy;
begin
  if JvGlobalDockManager <> nil then
    JvGlobalDockManager.UnRegisterDockableForm(Self);
  { Now handled in destroy of DockClient via TJvDockClient.SetLastDockSite }
  //if DockClient.LastDockSite is TJvDockPanel then
  //  if Assigned(TJvDockPanel(DockClient.LastDockSite).JvDockManager) then
  //    TJvDockPanel(DockClient.LastDockSite).JvDockManager.RemoveControl(Self);
  inherited Destroy;
  // (rom) better comment this
  FFloatingChild := nil;
end;

procedure TJvDockableForm.DoClose(var Action: TCloseAction);
var
  I: Integer;
begin
  if not Assigned(DockableControl) then
    exit;

  if DockableControl.DockClientCount = 1 then
  begin
    FFloatingChild := DockableControl.DockClients[0];

    if HostDockSite <> nil then
      FFloatingChild.Visible := False;

    DoFloat(Self, DockableControl.DockClients[0]);
    Action := caFree;
  end
  else
  if DockableControl.DockClientCount = 0 then
    Action := caFree
  else
  begin
    Action := caHide;
    if (FUnDockControl <> nil) and (DockableControl.DockClientCount = 2) then
      for I := 0 to DockableControl.DockClientCount - 1 do
        if FUnDockControl = DockableControl.DockClients[I] then
        begin
          Action := caNone;
          Break;
        end;
  end;
  if (HostDockSite is TJvDockPanel) and (HostDockSite.VisibleDockClientCount = 1) and
    (FFloatingChild = nil) then
    TJvDockPanel(HostDockSite).ShowDockPanel(False, Self);

  inherited DoClose(Action);
  FUnDockControl := nil;
end;

function TJvDockableForm.GetDockableControl: TWinControl;
begin
  Result := FDockableControl;
end;

procedure TJvDockableForm.SetDockableControl(const Value: TWinControl);
begin
  FDockableControl := Value;
end;

procedure TJvDockableForm.SetUnDockControl(const Value: TControl);
begin
  FUnDockControl := Value;
end;

//=== { TJvDockAdvConjoinPanel } =============================================

procedure TJvDockAdvConjoinPanel.CMUnDockClient(var Msg: TCMUnDockClient);
begin
  inherited;
end;

procedure TJvDockAdvConjoinPanel.CustomDockDrop(Source: TJvDockDragDockObject;
  X, Y: Integer);
begin
  if not JvGlobalDockIsLoading then
    ResetDockClient(Source.Control, Source.TargetControl);
  inherited CustomDockDrop(Source, X, Y);
end;

function TJvDockAdvConjoinPanel.CustomUnDock(Source: TJvDockDragDockObject;
  NewTarget: TWinControl; Client: TControl): Boolean;
begin
  if not JvGlobalDockIsLoading then
    ResetDockClient(Source.Control, NewTarget);
  Result := inherited CustomUnDock(Source, NewTarget, Client);
end;

procedure TJvDockAdvConjoinPanel.DockDrop(Source: TDragDockObject;
  X, Y: Integer);
begin
  if not JvGlobalDockIsLoading then
    ResetDockClient(Source.Control, TControl(Source.DragTarget));
  inherited DockDrop(Source, X, Y);
end;

function TJvDockAdvConjoinPanel.DoUnDock(NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  if not JvGlobalDockIsLoading then
    ResetDockClient(Client, NewTarget);
  Result := inherited DoUnDock(NewTarget, Client);
end;

//=== { TJvDockAdvPanel } ====================================================

procedure TJvDockAdvPanel.CMUnDockClient(var Msg: TCMUnDockClient);
var
  DockClient: TJvDockClient;
begin
  if JvGlobalDockIsLoading then
    Exit;
  with Msg do
  begin
    Result := 0;
    if UseDockManager and (JvDockManager <> nil) then
    begin
      DockClient := FindDockClient(Client);
      if (NewTarget <> nil) or
        ((Client <> nil) and (csDestroying in Client.ComponentState)) then
      begin
        if DockClient <> nil then
          DockClient.LastDockSite := nil;
        JvDockManager.RemoveControl(Client);
      end
      else
      begin
        if DockClient <> nil then
          DockClient.LastDockSite := Self;
        JvDockManager.HideControl(Client);
      end;
    end;
  end;
end;

procedure TJvDockAdvPanel.CustomDockDrop(Source: TJvDockDragDockObject;
  X, Y: Integer);
begin
  if not JvGlobalDockIsLoading then
    ResetDockClient(Source.Control, Source.TargetControl);
  inherited CustomDockDrop(Source, X, Y);
end;

function TJvDockAdvPanel.CustomUnDock(Source: TJvDockDragDockObject;
  NewTarget: TWinControl; Client: TControl): Boolean;
begin
  if not JvGlobalDockIsLoading then
    ResetDockClient(Source.Control, NewTarget);
  Result := inherited CustomUnDock(Source, NewTarget, Client)
end;

procedure TJvDockAdvPanel.DockDrop(Source: TDragDockObject; X, Y: Integer);
begin
  if not JvGlobalDockIsLoading then
    ResetDockClient(Source.Control, TControl(Source.DragTarget));
  inherited DockDrop(Source, X, Y);
end;

function TJvDockAdvPanel.DoUnDock(NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  if not JvGlobalDockIsLoading then
    ResetDockClient(Client, NewTarget);
  Result := inherited DoUnDock(NewTarget, Client);
end;

// NEW! -WPostma.
// Calls FADVTree.QueryControls which finds forms containing JvDockClient
// objects that are directly or indirectly docked to this panel.

procedure TJvDockAdvPanel.GetDockedControls(WinControls: TList); //override;
begin
  { (rb) Same result could be get via iterating property TWinControl.DockClients }
  Assert(Assigned(WinControls));
  if Assigned(JvDockManager) then
  begin
    WinControls.Clear;
    {$IFDEF JVDOCK_QUERY}
    JvDockManager.ControlQuery({Docked to self}Self, WinControls);
    {$ENDIF JVDOCK_QUERY}
  end;
end;

function TJvDockAdvPanel.FindTabHostForm:TWinControl;
var
 n:Integer;
 wc:TControl;
begin
	for n := 0 to Self.DockClientCount-1 do begin
		wc := Self.DockClients[n];
		if wc is TJvDockTabHostForm then begin
		 result := wc as TWinControl;
		 exit;
		end;
	end;
	result := nil;

end;


//=== { TJvDockAdvStyle } ====================================================

function TJvDockAdvStyle.DockClientWindowProc(DockClient: TJvDockClient;
  var Msg: TMessage): Boolean;
var
  NewPPI: Integer;
begin
  if (DockClient <> nil) and (Msg.Msg = WM_NCLBUTTONDBLCLK) then
    //KV
    if DockClient.DockState = JvDockState_Floating then begin
      if Assigned(DockClient.LastDockSite) then begin
        NewPPI := DockClient.LastDockSite.CurrentPPI;
        if NewPPI <> TControlAccessProtected(DockClient.ParentForm).FCurrentPPI then
          DockClient.ParentForm.ScaleForPPI(NewPPI);
        DockClient.ParentForm.ManualDock(DockClient.LastDockSite);
      end;
    end else begin
      if DockClient.CanFloat then
        DockClient.RestoreChild;
    end;

  Result := inherited DockClientWindowProc(DockClient, Msg);
end;

//=== { TJvDockAdvTabPageControl } ===========================================

destructor TJvDockAdvTabPageControl.Destroy;
//var
//  DockClient: TJvDockClient;
begin
  { Now handled in TJvDockClient.SetLastDockSite }
  //  { Parent is always nil? Self is maintained by TJvDockTabHostForm (=Parent),
  //    Self is destroyed only if TJvDockTabHostForm is destroyed (thus is nil at
  //    this point }
  //  DockClient := FindDockClient(Parent);
  //  if (DockClient <> nil) and (DockClient.LastDockSite is TJvDockPanel) then
  //    with TJvDockPanel(DockClient.LastDockSite) do
  //      if UseDockManager and (JvDockManager <> nil) then
  //        JvDockManager.RemoveControl(Self.Parent);
  inherited Destroy;
end;

procedure TJvDockAdvTabPageControl.CMUnDockClient(var Msg: TCMUnDockClient);
begin
  inherited;
end;

procedure TJvDockAdvTabPageControl.CustomDockDrop(Source: TJvDockDragDockObject;
  X, Y: Integer);
begin
  if not JvGlobalDockIsLoading then
    ResetDockClient(Source.Control, Source.TargetControl);
  inherited CustomDockDrop(Source, X, Y);
end;

function TJvDockAdvTabPageControl.CustomUnDock(Source: TJvDockDragDockObject;
  NewTarget: TWinControl; Client: TControl): Boolean;
begin
  if not JvGlobalDockIsLoading then
    ResetDockClient(Source.Control, NewTarget);
  Result := inherited CustomUnDock(Source, NewTarget, Client)
end;

procedure TJvDockAdvTabPageControl.DockDrop(Source: TDragDockObject;
  X, Y: Integer);
begin
  if not JvGlobalDockIsLoading then
    ResetDockClient(Source.Control, TControl(Source.DragTarget));
  inherited DockDrop(Source, X, Y);
end;

function TJvDockAdvTabPageControl.DoUnDock(NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  if not JvGlobalDockIsLoading then
    ResetDockClient(Client, NewTarget);
  Result := inherited DoUnDock(NewTarget, Client);
end;

//=== { TJvDockBaseControl } =================================================

constructor TJvDockBaseControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { Dangerous/Dirty }
  FParentForm := TForm(AOwner);
  FEnableDock := True;
  FLeftDock := True;
  FTopDock := True;
  FCustomDock := True; {Allowed to dock in custom area!}
  FRightDock := True;
  FBottomDock := True;
  FEachOtherDock := True;
  FDockStyle := nil;
  if not (csDesigning in ComponentState) then
  begin
    FOldOnClose := FParentForm.OnClose;
    ParentForm.OnClose := DoFormOnClose;
    FOldOnCreate := FParentForm.OnCreate;
    ParentForm.OnCreate := DoFormOnCreate;
    FOldWindowProc := FParentForm.WindowProc;
    FParentForm.WindowProc := WindowProc;
  end;
end;

destructor TJvDockBaseControl.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    if Assigned(FOldWindowProc) then
      FParentForm.WindowProc := FOldWindowProc;
    FOldWindowProc := nil;
    { ?? FDockStyle is always a TJvDockBasicStyle }
    if Assigned(FDockStyle) {and not (FDockStyle is TJvDockBasicStyle)} then
      FDockStyle.SetDockBaseControl(False, Self);
  end;
  DockStyle := nil;
  inherited Destroy;
end;

procedure TJvDockBaseControl.AddDockStyle(ADockStyle: TJvDockBasicStyle);
begin
  { Notification }
end;

procedure TJvDockBaseControl.Assign(Source: TPersistent);
begin
  if Source is TJvDockBaseControl then
  begin
    EnableDock := TJvDockBaseControl(Source).EnableDock;
    LeftDock := TJvDockBaseControl(Source).LeftDock;
    TopDock := TJvDockBaseControl(Source).TopDock;
    RightDock := TJvDockBaseControl(Source).RightDock;
    BottomDock := TJvDockBaseControl(Source).BottomDock;
    CustomDock := TJvDockBaseControl(Source).CustomDock; {NEW!}
    EachOtherDock := TJvDockBaseControl(Source).EachOtherDock;
    DockStyle := TJvDockBaseControl(Source).DockStyle;
  end
  else
    inherited Assign(Source);
end;

function TJvDockBaseControl.CanSetBottomDocked: Boolean;
begin
  if DockStyle <> nil then
    Result := DockStyle.CanSetBottomDocked(Self)
  else
    Result := True;
end;

function TJvDockBaseControl.CanSetEachOtherDocked: Boolean;
begin
  if DockStyle <> nil then
    Result := DockStyle.CanSetEachOtherDocked(Self)
  else
    Result := True;
end;

function TJvDockBaseControl.CanSetEnableDocked: Boolean;
begin
  if DockStyle <> nil then
    Result := DockStyle.CanSetEnableDocked(Self)
  else
    Result := True;
end;

function TJvDockBaseControl.CanSetLeftDocked: Boolean;
begin
  if DockStyle <> nil then
    Result := DockStyle.CanSetLeftDocked(Self)
  else
    Result := True;
end;

function TJvDockBaseControl.CanSetRightDocked: Boolean;
begin
  if DockStyle <> nil then
    Result := DockStyle.CanSetRightDocked(Self)
  else
    Result := True;
end;

function TJvDockBaseControl.CanSetTopDocked: Boolean;
begin
  if DockStyle <> nil then
    Result := DockStyle.CanSetTopDocked(Self)
  else
    Result := True;
end;

procedure TJvDockBaseControl.DoFormOnClose(Sender: TObject;
  var Action: TCloseAction);
begin
  { Code is moved to AddDockStyle, RemoveDockStyle; if a user assigns OnClose,
    OnCreate handlers things would go wrong.
  }
  if Assigned(FOldOnClose) then
    FOldOnClose(Sender, Action);
end;

procedure TJvDockBaseControl.DoFormOnCreate(Sender: TObject);
begin
  { Code is moved to AddDockStyle, RemoveDockStyle; if a user assigns OnClose,
    OnCreate handlers things would go wrong.
  }
  if Assigned(FOldOnCreate) then
    FOldOnCreate(Sender);
end;

procedure TJvDockBaseControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FDockStyle then
      DockStyle := nil;
end;

procedure TJvDockBaseControl.RemoveDockStyle(ADockStyle: TJvDockBasicStyle);
begin
  { Notification }
end;

procedure TJvDockBaseControl.SetBottomDock(const Value: Boolean);
begin
  if CanSetBottomDocked then
    FBottomDock := Value;
end;

procedure TJvDockBaseControl.SetDockStyle(ADockStyle: TJvDockBasicStyle);
begin
  {$IFDEF JVDOCK_DEBUG}
  OutputDebugString('TJvDockBaseControl.SetDockStyle');
  {$ENDIF JVDOCK_DEBUG}
  if ADockStyle <> FDockStyle then
  begin
    ParentForm.DisableAlign;
    try
      if FDockStyle <> nil then
      begin
        { Remove Self from the internal list of the dock style component }
        FDockStyle.RemoveDockBaseControl(Self);

        { Give the ancestors a change to respond }
        RemoveDockStyle(FDockStyle);
      end;

      ReplaceComponentReference(Self, ADockStyle, TComponent(FDockStyle));

      if FDockStyle <> nil then
      begin
        { Let the style initialize the TJvDockClient/TJvDockServer }
        FDockStyle.SetDockBaseControl([csLoading, csDesigning] * ComponentState <> [], Self);

        { Add Self to the internal list of the dock style component }
        FDockStyle.AddDockBaseControl(Self);

        { Give the ancestors a change to respond }
        AddDockStyle(FDockStyle);

      end;
    finally
      ParentForm.EnableAlign;
    end;
  end;
end;

procedure TJvDockBaseControl.SetEachOtherDock(const Value: Boolean);
begin
  if CanSetEachOtherDocked then
    FEachOtherDock := Value;
end;

procedure TJvDockBaseControl.SetEnableDock(const Value: Boolean);
begin
  if CanSetEnableDocked then
    FEnableDock := Value;
end;

procedure TJvDockBaseControl.SetLeftDock(const Value: Boolean);
begin
  if CanSetLeftDocked then
    FLeftDock := Value;
end;

procedure TJvDockBaseControl.SetParentComponent(Value: TComponent);
var
  DockBaseControl: TJvDockBaseControl;
begin
  DockBaseControl := FindDockBaseControl(ParentForm);
  if Assigned(DockBaseControl) and (DockBaseControl <> Self) then
    raise EInvalidOperation.CreateResFmt(@RsEDockCannotLayAnother, [DockBaseControl.ClassName, ClassName]);
  inherited SetParentComponent(Value);
end;

procedure TJvDockBaseControl.SetRightDock(const Value: Boolean);
begin
  if CanSetRightDocked then
    FRightDock := Value;
end;

procedure TJvDockBaseControl.SetTopDock(const Value: Boolean);
begin
  if CanSetTopDocked then
    FTopDock := Value;
end;

procedure TJvDockBaseControl.WindowProc(var Msg: TMessage);
begin
  if not (csDesigning in ComponentState) then
    if Assigned(FOldWindowProc) then
      FOldWindowProc(Msg);
end;

//=== { TJvDockBasicStyle } ==================================================

constructor TJvDockBasicStyle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  DockPanelClass := DefaultDockPanelClass;
  DockSplitterClass := DefaultDockSplitterClass;
  ConjoinPanelClass := DefaultConjoinPanelClass;
  TabDockClass := DefaultTabDockClass;
  DockPanelTreeClass := DefaultDockTreeClass;
  DockPanelZoneClass := DefaultDockZoneClass;
  ConjoinPanelTreeClass := DefaultDockTreeClass;
  ConjoinPanelZoneClass := DefaultDockZoneClass;
  FDockBaseControls := TList.Create;
end;

destructor TJvDockBasicStyle.Destroy;
begin
  FDockBaseControls.Free;
  FreeServerOption;
  inherited Destroy;
end;

procedure TJvDockBasicStyle.AddDockBaseControl(ADockBaseControl: TJvDockBaseControl);
begin
  {$IFDEF JVDOCK_DEBUG}
  OutputDebugString('TJvDockBasicStyle.AddDockBaseControl');
  {$ENDIF JVDOCK_DEBUG}
  if ADockBaseControl = nil then
    Exit;
  FDockBaseControls.Add(ADockBaseControl);
end;

function TJvDockBasicStyle.CanSetBottomDocked(ADockBaseControl: TJvDockBaseControl): Boolean;
begin
  Result := True;
end;

function TJvDockBasicStyle.CanSetEachOtherDocked(ADockBaseControl: TJvDockBaseControl): Boolean;
begin
  Result := True;
end;

function TJvDockBasicStyle.CanSetEnableDocked(ADockBaseControl: TJvDockBaseControl): Boolean;
begin
  Result := True;
end;

function TJvDockBasicStyle.CanSetLeftDocked(ADockBaseControl: TJvDockBaseControl): Boolean;
begin
  Result := True;
end;

function TJvDockBasicStyle.CanSetRightDocked(ADockBaseControl: TJvDockBaseControl): Boolean;
begin
  Result := True;
end;

function TJvDockBasicStyle.CanSetTopDocked(ADockBaseControl: TJvDockBaseControl): Boolean;
begin
  Result := True;
end;

function TJvDockBasicStyle.DockClientWindowProc(DockClient: TJvDockClient; var Msg: TMessage): Boolean;
begin
  Result := False;
end;

function TJvDockBasicStyle.DockServerWindowProc(DockServer: TJvDockServer;
  var Msg: TMessage): Boolean;
begin
  Result := False;
end;

procedure TJvDockBasicStyle.FormDockDrop(DockClient: TJvDockClient;
  Source: TJvDockDragDockObject; X, Y: Integer);
var
  ARect, DRect: TRect;
  DockType: TAlign;
  Host: TCustomForm;
  APanelDock: TWinControl;
begin
  if IsDockable(DockClient.ParentForm, Source.Control, Source.DropOnControl, Source.DropAlign) then
  begin
    Host := nil;

    if not JvGlobalDockIsLoading then
      JvDockLockWindow(nil);
    try
      with DockClient do
      begin
        DockType := ComputeDockingRect(DockClient.ParentForm, ARect, Point(X, Y));

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
              Host.ManualDock(APanelDock, nil, alClient);
              Host.Visible := True;
            end;
          end
          else
          begin
            DRect := ParentForm.HostDockSite.BoundsRect;
            Source.Control.ManualDock(ParentForm.HostDockSite, nil, DockType);
            ParentForm.HostDockSite.BoundsRect := DRect;
          end;
          Exit;
        end;

        if DockType = alClient then
        begin
          Host := CreateTabHostAndDockControl(ParentForm, Source.Control);
          SetDockSite(ParentForm, False);
          SetDockSite(TWinControl(Source.Control), False);
          Host.Visible := True;
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
      if not JvGlobalDockIsLoading then
        JvDockUnLockWindow;
    end;
  end;
end;

procedure TJvDockBasicStyle.FormDockOver(DockClient: TJvDockClient;
  Source: TJvDockDragDockObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  ARect: TRect;
begin
  with DockClient do
  begin
    Accept := EnableDock and EachOtherDock and
      IsDockable(ParentForm.HostDockSite, Source.Control, Source.DropOnControl, Source.DropAlign);
    if Accept and (State = dsDragMove) and
      (ComputeDockingRect(ParentForm, ARect, Point(X, Y)) <> alNone) then
      Source.DockRect := ARect;
  end;
end;

procedure TJvDockBasicStyle.FormEndDock(DockClient: TJvDockClient;
  Target: TObject; X, Y: Integer);
begin
end;

procedure TJvDockBasicStyle.FormGetDockEdge(DockClient: TJvDockClient;
  Source: TJvDockDragDockObject; MousePos: TPoint; var DropAlign: TAlign);
begin
  DropAlign := TControlAccessProtected(DockClient.ParentForm).GetDockEdge(MousePos);
end;

procedure TJvDockBasicStyle.FormGetSiteInfo(Source: TJvDockDragDockObject;
  DockClient: TJvDockClient; Client: TControl; var InfluenceRect: TRect;
  MousePos: TPoint; var CanDock: Boolean);
begin
  with DockClient do
    CanDock := EnableDock and EachOtherDock and
      IsDockable(ParentForm, Client, Source.DropOnControl, Source.DropAlign);
end;

procedure TJvDockBasicStyle.FormPositionDockRect(DockClient: TJvDockClient;
  Source: TJvDockDragDockObject);
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
          if (TJvDockCustomPanel(DragTarget).JvDockManager <> nil) then
          begin
            R := DockRect;
            TJvDockCustomPanel(DragTarget).JvDockManager.PositionDockRect(Control,
              DropOnControl, DropAlign, R);
            DockRect := R;
          end;
    end;
  end;
end;

procedure TJvDockBasicStyle.FormStartDock(DockClient: TJvDockClient;
  var Source: TJvDockDragDockObject);
begin
end;

function TJvDockBasicStyle.FormUnDock(DockClient: TJvDockClient; NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  Result := True;
end;

function TJvDockBasicStyle.GetDockBaseControlCount: Integer;
begin
  Result := FDockBaseControls.Count;
end;

function TJvDockBasicStyle.GetDockBaseControl(Index: Integer): TJvDockBaseControl;
begin
  Result := TJvDockBaseControl(FDockBaseControls[Index]);
end;

function TJvDockBasicStyle.GetDockFormVisible(ADockClient: TJvDockClient): Boolean;
begin
  Result := True;
  if ADockClient <> nil then
  begin
    if ADockClient.ParentForm.Visible then
    begin
      if ADockClient.ParentForm.HostDockSite <> nil then
      begin
        if ADockClient.ParentForm.HostDockSite is TJvDockPanel then
          Result := ADockClient.ParentForm.HostDockSite.Width * ADockClient.ParentForm.HostDockSite.Height > 0
        else
          Result := GetFormVisible(ADockClient.ParentForm.HostDockSite.Parent);
      end;
    end
    else
      Result := False;
  end;
end;

function TJvDockBasicStyle.GetDockState(DockClient: TJvDockClient): Integer;
begin
  Result := JvDockState_Unknown;
  if (DockClient <> nil) and (DockClient.ParentForm <> nil) then
    if DockClient.ParentForm.Floating then
      Result := JvDockState_Floating
    else
      Result := JvDockState_Docking;
end;

procedure TJvDockBasicStyle.HideDockForm(ADockClient: TJvDockClient);
begin
  if ADockClient <> nil then
  begin
    ADockClient.ParentForm.Visible := False;
    ADockClient.MakeHideEvent;
  end;
end;

procedure TJvDockBasicStyle.RemoveDockBaseControl(ADockBaseControl: TJvDockBaseControl);
begin
  FDockBaseControls.Remove(ADockBaseControl);
end;

procedure TJvDockBasicStyle.ResetCursor(Source: TJvDockDragDockObject);
begin
  if (Source.TargetControl = nil) and (Source.Control <> nil) and (Source.Control.Floating) then
    Windows.SetCursor(Screen.Cursors[crDefault])
  else
  if (Source.TargetControl = nil) and (not JvGlobalDockClient.CanFloat) then
    Windows.SetCursor(Screen.Cursors[crNo])
  else
    Windows.SetCursor(Screen.Cursors[crDefault]);
end;

procedure TJvDockBasicStyle.RestoreClient(DockClient: TJvDockClient);
var
  TmpLastDockSite: TWinControl;
  TmpUnDockLeft, TmpUnDockTop: Integer;
  I: Integer;
  ADockClient: TJvDockClient;
  ADockServer: TJvDockServer;
  ARect: TRect;

  procedure DoFloatParentForm;
  var
    NewPPI: Integer;
  begin
    with DockClient do
      if (ParentForm.HostDockSite <> nil) then
      begin
        ARect := Bounds(TmpUnDockLeft, TmpUnDockTop, ParentForm.UndockWidth, ParentForm.UndockHeight);
        ParentForm.ManualFloat(ARect);
        NewPPI := Screen.MonitorFromPoint(Point(TmpUnDockLeft, TmpUnDockTop)).PixelsPerInch;
        if NewPPI <> TControlAccessProtected(ParentForm).FCurrentPPI then
          ParentForm.ScaleForPPI(NewPPI);

        if (ParentForm.Left <> ARect.Left) or (ParentForm.Top <> ARect.Top) then
        begin
          ParentForm.Left := ARect.Left;
          ParentForm.Top := ARect.Top;
        end;
      end;
  end;

begin
  if DockClient = nil then
    Exit;
  if not DockClient.CanFloat then
    Exit;
  with DockClient do
  begin
    if not EnableDock then
      Exit;
    if LastDockSite is TJvDockPanel then
    begin
      with TJvDockPanel(LastDockSite) do
      begin
        if ((not LeftDock) and (Align = alLeft)) or
          ((not RightDock) and (Align = alRight)) or
          ((not TopDock) and (Align = alTop)) or
          ((not BottomDock) and (Align = alBottom)) then
        begin
          DoFloatParentForm;
          Exit;
        end;

        ADockServer := DockServer;
        if ADockServer <> nil then
          if (not ADockServer.EnableDock) or
            ((not ADockServer.LeftDock) and (Align = alLeft)) or
            ((not ADockServer.RightDock) and (Align = alRight)) or
            ((not ADockServer.TopDock) and (Align = alTop)) or
            ((not ADockServer.BottomDock) and (Align = alBottom)) then
          begin
            DoFloatParentForm;
            Exit;
          end;
      end;
    end;

    if ParentForm is TJvDockConjoinHostForm then
    begin
      with TJvDockConjoinHostForm(ParentForm).Panel do
        for I := DockClientCount - 1 downto 0 do
        begin
          ADockClient := FindDockClient(DockClients[I]);
          if (ADockClient <> nil) and (ADockClient.LastDockSite is TJvDockPanel) then
            ADockClient.RestoreChild;
        end;
      Exit;
    end;

    TmpLastDockSite := LastDockSite;
    TmpUnDockLeft := UnDockLeft;
    TmpUnDockTop := UnDockTop;

    ResetDockClient(DockClient, nil);

    DoFloatParentForm;

    if TmpLastDockSite is TJvDockPanel then
    begin
      with TJvDockPanel(TmpLastDockSite) do
      begin
        if UseDockManager and (JvDockManager <> nil) then
        begin
          if not JvDockManager.HasZoneWithControl(ParentForm) then
            Exit;
          DisableAlign;
          try
            ParentForm.Dock(TmpLastDockSite, Rect(0, 0, 0, 0));

            JvDockManager.ShowControl(ParentForm);

            ParentForm.ActiveControl := nil;
            SetDockSite(ParentForm, False);

            if ParentForm.Visible and ParentForm.CanFocus then
              ParentForm.SetFocus;
            ShowDockPanel(True, ParentForm, sdfDockPanel);
          finally
            EnableAlign;
          end;
        end;
      end;
    end;
  end;
end;

procedure TJvDockBasicStyle.SetDockBaseControl(IsCreate: Boolean;
  DockBaseControl: TJvDockBaseControl);
begin
end;

procedure TJvDockBasicStyle.ShowDockForm(ADockClient: TJvDockClient);
begin
  if ADockClient <> nil then
  begin
    if ADockClient.FCurrentPPI <> ADockClient.ParentForm.CurrentPPI then
    begin
      // Scale but not size
      var BoundsRect := ADockClient.ParentForm.BoundsRect;
      ADockClient.ParentForm.ScaleForCurrentDPI;
      //if ADockClient.ParentForm.Parent = nil then
        ADockClient.ParentForm.BoundsRect := BoundsRect;
    end;
    ADockClient.ParentForm.Visible := True;
    ADockClient.MakeShowEvent;
  end;
end;

//=== { TJvDockClient } ======================================================

constructor TJvDockClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParentVisible := ParentForm.Visible;
  ParentForm.DragKind := dkDock;
  ParentForm.DragMode := dmAutomatic;
  ParentForm.UseDockManager := False;
  if not (ParentForm is TJvDockableForm) then
    SetDockSite(ParentForm, True);
  LRDockWidth := 100;
  TBDockHeight := 100;
  if JvGlobalDockClient = nil then
    JvGlobalDockClient := Self;
  FDirectDrag := False;
  FShowHint := True;
  FCanFloat := True;
  FDockLevel := 0;
  EnableCloseButton := True;
  {$IFDEF RTL330_UP}
  FCurrentPPI := 96;
  FDPIChangedMessageID := TMessageManager.DefaultManager.SubscribeToMessage(TChangeScaleMessage, DPIChangedMessageHandler);
  {$ENDIF RTL330_UP}
end;

destructor TJvDockClient.Destroy;
begin
  {$IFDEF RTL330_UP}
  TMessageManager.DefaultManager.Unsubscribe(TChangeScaleMessage, FDPIChangedMessageID);
  {$ENDIF RTL330_UP}
  if not (ParentForm is TJvDockableForm) then
    SetDockSite(ParentForm, False);
  ParentForm.DragKind := dkDrag;
  ParentForm.DragMode := dmManual;
  LastDockSite := nil;
  inherited Destroy;
end;

procedure TJvDockClient.Activate;
begin
  {$IFNDEF COMPILER9_UP}
  if ParentForm.HostDockSite is TJvDockCustomPanel then
    InvalidateDockHostSiteOfControl(ParentForm, False);
  {$ENDIF !COMPILER9_UP}
end;

procedure TJvDockClient.AddDockStyle(ADockStyle: TJvDockBasicStyle);
begin
  JvGlobalDockManager.RegisterDockClient(Self);

  if Assigned(ADockStyle) and Assigned(ADockStyle.ConjoinPanelClass) then
    FConjoinPanelClass := ADockStyle.ConjoinPanelClass
  else
    FConjoinPanelClass := DefaultConjoinPanelClass;

  if Assigned(ADockStyle) and Assigned(ADockStyle.TabDockClass) then
    FTabDockClass := ADockStyle.TabDockClass
  else
    FTabDockClass := DefaultTabDockClass;
end;

procedure TJvDockClient.Assign(Source: TPersistent);
begin
  if Source is TJvDockClient then
  begin
    FConjoinPanelClass := TJvDockClient(Source).FConjoinPanelClass;
    FTabDockClass := TJvDockClient(Source).FTabDockClass;
    ParentVisible := TJvDockClient(Source).ParentVisible;
    NCPopupMenu := TJvDockClient(Source).NCPopupMenu;
    DirectDrag := TJvDockClient(Source).DirectDrag;
    ShowHint := TJvDockClient(Source).ShowHint;
    CanFloat := TJvDockClient(Source).CanFloat;
    TBDockHeight := TJvDockClient(Source).TBDockHeight;
    LRDockWidth := TJvDockClient(Source).LRDockWidth;
    // (rom) either bug or needs comment
    FDockLevel := TJvDockClient(Source).DockLevel;
    CustomDock := TJvDockClient(Source).CustomDock; {NEW!}
    DockStyle := TJvDockClient(Source).DockStyle;
  end;
  inherited Assign(Source); // Also copy base class properties!
end;

procedure TJvDockClient.CMVisibleChanged(var Msg: TMessage);
begin
end;

function TJvDockClient.CreateConjoinHostAndDockControl(Control1, Control2: TControl;
  DockType: TAlign): TJvDockConjoinHostForm;
var
  APanel: TJvDockConjoinPanel;
  OldDockWidth, OldDockHeight: Integer;
begin
  Result := TJvDockConjoinHostForm.Create(Application);

  { CreateConjoinPanelClass implicitly sets Result.DockClient.DockStyle via the
    assign in that function }
  APanel := CreateConjoinPanelClass(Result);

  Result.SetBounds(Control1.Left, Control1.Top, Control1.UndockWidth, Control1.UndockHeight);
  Result.Position := poDesigned;
  Result.DefaultMonitor := dmDesktop;

  OldDockWidth := Control1.LRDockWidth;
  OldDockHeight := Control1.TBDockHeight;
  Control1.ManualDock(APanel, nil, alNone);
  Control1.LRDockWidth := OldDockWidth;
  Control1.TBDockHeight := OldDockHeight;

  OldDockWidth := Control2.LRDockWidth;
  OldDockHeight := Control2.TBDockHeight;
  Control2.ManualDock(APanel, nil, DockType);
  Control2.LRDockWidth := OldDockWidth;
  Control2.TBDockHeight := OldDockHeight;

  SetDockSite(Result, False);
  //TJvDockTabHostFormCreatedEvent:
  if Assigned(FOnConjoinHostFormCreated) then
    FOnConjoinHostFormCreated(Self, {ConjoinHost:TJvDockConjoinHostForm} Result);
end;

function TJvDockClient.CreateConjoinPanelClass(ConjoinHost: TForm): TJvDockConjoinPanel;
begin
  Result := nil;
  TJvDockConjoinHostForm(ConjoinHost).DockClient.Assign(Self);
  if (FConjoinPanelClass <> nil) and
    (FConjoinPanelClass <> TJvDockConjoinPanelClass(ClassType)) then
  begin
    Result := FConjoinPanelClass.Create(ConjoinHost);
    Result.Align := alClient;
    TJvDockConjoinHostForm(ConjoinHost).DockableControl := Result;
    TJvDockConjoinHostForm(ConjoinHost).Panel := Result;
    SetDockSite(Result, True);
  end;
end;

{ CreateTabDockClass creates a TJvDockTabPageControl subclass. The actual subclass
  used is stored in  FTabDockClass which is assigned by the Docking Style object,
  so this is truly polymorphic.

  We need a TJvDockTabPageControl to do tabbed-docking inside a container form.
  The typical container this is placed inside of is either a
  TJvDockConjoinHostForm or TJvDockTabHostForm.
}

function TJvDockClient.CreateTabDockClass(TabHost: TForm): TJvDockTabPageControl;
begin
  Result := nil;
  { copy properties of the JvTabDockPageControl's DockClient
    from the form it contains. }
  TJvDockTabHostForm(TabHost).DockClient.Assign(Self);

// Next line commented out, it seems it is useless. See Mantis 3900.
//  Assert(TJvDockTabHostForm(TabHost).DockClient.CustomDock, 'DEBUG HELPER: Not working!');

  if (FTabDockClass <> nil) and (FTabDockClass <> TJvDockTabClass(ClassType)) then
  begin
    Result := FTabDockClass.Create(TabHost);
    Result.Align := alClient;
    TJvDockTabHostForm(TabHost).DockableControl := Result;
    TJvDockTabHostForm(TabHost).PageControl := Result;
    SetDockSite(Result, True);
  end;
end;

{ This is the main function for creating a tabbed dock, given two non-tab-docked
 controls (usually these are actually Forms), we create a TJvDockTabHostForm,
 and then inside it we create a TabDock container.  A TJvDockTabHostForm is
 an empty TForm containing a DockClient object, and then it contains a set of
 tabbed pages inside the TJvDockTabPageControl object (Page). }

function TJvDockClient.CreateTabHostAndDockControl(Control1, Control2: TControl): TJvDockTabHostForm;
var
  Page: TJvDockTabPageControl;
  OldDockWidth, OldDockHeight: Integer;
begin
  Result := TJvDockTabHostForm.Create(Application);
  Result.Name := 'TJvDockTabHostForm_' + Control1.Name + '_' + Control2.Name + '_' +
    IntToHex(LPARAM(Result), 2 * SizeOf(LPARAM));

  { CreateTabDockClass implicitly sets Result.DockClient.DockStyle via the
    assign in that function }
  Page := CreateTabDockClass(Result);
  Page.Name := Page.ClassName + '_' + Control1.Name + '_' + Control2.Name; // debug!

  Result.SetBounds(Control1.Left, Control1.Top, Control1.UndockWidth, Control1.UndockHeight);
  Result.Position := poDesigned;
  Result.DefaultMonitor := dmDesktop;

  OldDockWidth := Control1.LRDockWidth;
  OldDockHeight := Control1.TBDockHeight;
  Control1.ManualDock(Page, nil, alClient);
  Control1.LRDockWidth := OldDockWidth;
  Control1.TBDockHeight := OldDockHeight;

  OldDockWidth := Control2.LRDockWidth;
  OldDockHeight := Control2.TBDockHeight;
  Control2.ManualDock(Page, nil, alClient);
  Control2.LRDockWidth := OldDockWidth;
  Control2.TBDockHeight := OldDockHeight;

  SetDockSite(Result, False);

  //TJvDockTabHostFormCreatedEvent:
  if Assigned(FOnTabHostFormCreated) then
    FOnTabHostFormCreated(Self, {TabHost:TJvDockTabHostForm} Result);
end;

procedure TJvDockClient.Deactivate;
begin
  {$IFNDEF COMPILER9_UP}
  if ParentForm.HostDockSite is TJvDockCustomPanel then
    InvalidateDockHostSiteOfControl(ParentForm, True);
  {$ENDIF !COMPILER9_UP}
end;

procedure TJvDockClient.DoFloatDockClients(PanelAlign: TAlign);
begin
  if not (csDestroying in ParentForm.ComponentState) and
    (ParentForm.HostDockSite is TJvDockPanel) and
    (PanelAlign = ParentForm.HostDockSite.Align) then
    RestoreChild;
end;

procedure TJvDockClient.DoFloatDockEachOther;
begin
  if not (csDestroying in ParentForm.ComponentState) and
    (ParentForm.HostDockSite <> nil) and
    (ParentForm.HostDockSite.Parent is TJvDockableForm) then
    RestoreChild;
end;

procedure TJvDockClient.DoFormOnClose(Sender: TObject; var Action: TCloseAction);
begin
  if Action = caHide then
  begin
    HideDockForm(ParentForm);
    FParentVisible := True;
  end;
  inherited DoFormOnClose(Sender, Action);
end;

procedure TJvDockClient.DoFormShowHint(HTFlag: Integer; var HintStr: string;
  var CanShow: Boolean);
begin
  if Assigned(FOnFormShowHint) then
    FOnFormShowHint(HTFlag, HintStr, CanShow);
end;

procedure TJvDockClient.DoMenuPopup(X, Y: Integer);
begin
  if FNCPopupMenu <> nil then
  begin
    FNCPopupMenu.PopupComponent := ParentForm;
    FNCPopupMenu.Popup(X, Y);
  end;
end;

procedure TJvDockClient.DoNCButtonDblClk(Msg: TWMNCHitMessage; Button: TMouseButton;
  MouseStation: TJvDockMouseStation);
begin
  if Assigned(FOnNCButtonDblClk) then
    FOnNCButtonDblClk(Self, Button, Msg.XCursor, Msg.YCursor,
      Msg.HitTest, MouseStation);
end;

procedure TJvDockClient.DoNCButtonDown(Msg: TWMNCHitMessage;
  Button: TMouseButton; MouseStation: TJvDockMouseStation);
begin
  if Assigned(FOnNCButtonDown) then
    FOnNCButtonDown(Self, Button, Msg.XCursor, Msg.YCursor,
      Msg.HitTest, MouseStation);
end;

procedure TJvDockClient.DoNCButtonUp(Msg: TWMNCHitMessage;
  Button: TMouseButton; MouseStation: TJvDockMouseStation);
begin
  if Assigned(FOnNCButtonUp) then
    FOnNCButtonUp(Self, Button, Msg.XCursor, Msg.YCursor,
      Msg.HitTest, MouseStation);
  if Button = mbRight then
    DoMenuPopup(Msg.XCursor, Msg.YCursor);
end;

procedure TJvDockClient.DoNCMouseMove(Msg: TWMNCHitMessage;
  MouseStation: TJvDockMouseStation);
begin
  if Assigned(FOnNCMouseMove) then
    FOnNCMouseMove(Self, Msg.XCursor, Msg.YCursor,
      Msg.HitTest, MouseStation);
end;

procedure TJvDockClient.DoPaintDockGrabber(Canvas: TCanvas;
  Control: TControl; const ARect: TRect);
begin
  if Assigned(FOnPaintDockGrabber) then
    FOnPaintDockGrabber(Canvas, Control, ARect);
end;

procedure TJvDockClient.DoPaintDockSplitter(Canvas: TCanvas;
  Control: TControl; const ARect: TRect);
begin
  if Assigned(FOnPaintDockSplitter) then
    FOnPaintDockSplitter(Canvas, Control, ARect);
end;

{$IFDEF RTL330_UP}
procedure TJvDockClient.DPIChangedMessageHandler(const Sender: TObject;
  const Msg: System.Messaging.TMessage);
begin
  if FindDockClient(TControl(Sender)) = Self then
  begin
    FCurrentPPI := MulDiv(FCurrentPPI, TChangeScaleMessage(Msg).M, TChangeScaleMessage(Msg).D);
    LRDockWidth := MulDiv(LRDockWidth, TChangeScaleMessage(Msg).M, TChangeScaleMessage(Msg).D);
    TBDockHeight := MulDiv(TBDockHeight, TChangeScaleMessage(Msg).M, TChangeScaleMessage(Msg).D);
  end;
end;
{$ENDIF RTL330_UP}

// return nil if not found, otherwise, get currently docked parent tabhost form if there is one.

function TJvDockClient.FindTabHostForm: TForm;
var
  OwnerWin: TWinControl;
  TabSheet: TJvDockTabSheet;
  CheckForm: TComponent;
begin
  Result := nil;
  if not Assigned(Owner) or not (Owner is TWinControl) then
    Exit;
  OwnerWin := TWinControl(Owner);
  if Self.DockState <> JvDockState_Docking then
    Exit;
  if not Assigned(OwnerWin.Parent) then
    Exit;
  if OwnerWin.Parent is TJvDockTabSheet then
  begin
    TabSheet := TJvDockTabSheet(OwnerWin.Parent);
    if TabSheet.Owner is TJvDockPageControl then
    begin
      CheckForm := TJvDockPageControl(TabSheet.Owner).Owner;
      if CheckForm is TJvDockTabHostForm then
        Result := TForm(CheckForm);
    end;
  end;
end;

procedure TJvDockClient.FormDockDrop(Source: TJvDockDragDockObject;
  X, Y: Integer);
begin
  if Assigned(DockStyle) then
    DockStyle.FormDockDrop(Self, Source, X, Y);
end;

procedure TJvDockClient.FormDockOver(Source: TJvDockDragDockObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  FormPositionDockRect(Source);
  if Assigned(DockStyle) then
    DockStyle.FormDockOver(Self, Source, X, Y, State, Accept);
end;

procedure TJvDockClient.FormEndDock(Target: TObject; X, Y: Integer);
begin
  if Assigned(DockStyle) then
    DockStyle.FormEndDock(Self, Target, X, Y);
end;

procedure TJvDockClient.FormGetDockEdge(Source: TJvDockDragDockObject;
  MousePos: TPoint; var DropAlign: TAlign);
begin
  if Assigned(DockStyle) then
    DockStyle.FormGetDockEdge(Self, Source, MousePos, DropAlign)
  else
    DropAlign := alNone;
end;

procedure TJvDockClient.FormGetSiteInfo(Source: TJvDockDragDockObject; Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  GetWindowRect(ParentForm.Handle, InfluenceRect);
  InflateRect(InfluenceRect, -4, -4);
  if Assigned(DockStyle) then
    DockStyle.FormGetSiteInfo(Source, Self, Client, InfluenceRect, MousePos, CanDock);
end;

procedure TJvDockClient.FormPositionDockRect(Source: TJvDockDragDockObject);
begin
  if Assigned(DockStyle) then
    DockStyle.FormPositionDockRect(Self, Source);
end;

procedure TJvDockClient.FormStartDock(var Source: TJvDockDragDockObject);
begin
  if Assigned(DockStyle) then
    DockStyle.FormStartDock(Self, Source);
end;

function TJvDockClient.FormUnDock(NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  if Assigned(DockStyle) then
    Result := DockStyle.FormUnDock(Self, NewTarget, Client)
  else
    Result := False;
end;

function TJvDockClient.GetDockState: Integer;
begin
  Result := JvDockState_Unknown;
  if DockStyle <> nil then
    Result := DockStyle.GetDockState(Self);
end;

function TJvDockClient.GetLRDockWidth: Integer;
begin
  Result := ParentForm.LRDockWidth;
end;

function TJvDockClient.GetTBDockHeight: Integer;
begin
  Result := ParentForm.TBDockHeight;
end;

procedure TJvDockClient.HideParentForm;
begin
  HideDockForm(ParentForm);
end;

procedure TJvDockClient.MakeHideEvent;
begin
  ParentVisible := False;
  if Assigned(FOnFormHide) then
    FOnFormHide(Self);
end;

procedure TJvDockClient.MakeShowEvent;
begin
  if ParentForm.Visible then
  begin
    if Assigned(FOnFormShow) then
      FOnFormShow(Self);
    ParentVisible := True;
  end;
end;

procedure TJvDockClient.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FLastDockSite then
      LastDockSite := nil
    else
    if AComponent = NCPopupMenu then
      NCPopupMenu := nil;
  end;
end;

procedure TJvDockClient.RemoveDockStyle(ADockStyle: TJvDockBasicStyle);
begin
  DoFloatDockClients(alTop);
  DoFloatDockClients(alBottom);
  DoFloatDockClients(alLeft);
  DoFloatDockClients(alRight);
  DoFloatDockEachOther;

  FConjoinPanelClass := nil;
  FTabDockClass := nil;

  if JvGlobalDockManager <> nil then
    JvGlobalDockManager.UnRegisterDockClient(Self);
end;

procedure TJvDockClient.RestoreChild;
begin
  DockStyle.RestoreClient(Self);
end;

procedure TJvDockClient.SetBottomDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(alBottom);
  inherited SetBottomDock(Value);
end;

procedure TJvDockClient.SetCanFloat(const Value: Boolean);
begin
  FCanFloat := Value;
end;

procedure TJvDockClient.SetCurrentDockSite(const Value: TWinControl);
begin
  FCurrentDockSite := Value;
end;

procedure TJvDockClient.SetDockLevel(const Value: Integer);
begin
  if not ParentForm.Floating then
    if FDockLevel <> Value then
      DoFloatForm(ParentForm);
  FDockLevel := Value;
end;

procedure TJvDockClient.SetEachOtherDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockEachOther;
  inherited SetEachOtherDock(Value);
end;

procedure TJvDockClient.SetEnableCloseButton(const Value: Boolean);
begin
  FEnableCloseButton := Value;
end;

procedure TJvDockClient.SetEnableDock(const Value: Boolean);
begin
  if not Value then
  begin
    DoFloatDockClients(alTop);
    DoFloatDockClients(alBottom);
    DoFloatDockClients(alLeft);
    DoFloatDockClients(alRight);
    DoFloatDockEachOther;
  end;
  if ParentForm <> nil then
    if Value then
      ParentForm.DragKind := dkDock
    else
      ParentForm.DragKind := dkDrag;
  inherited SetEnableDock(Value);
end;

procedure TJvDockClient.SetLastDockSite(ALastDockSite: TWinControl);
var
  JvDockManager: IJvDockManager;
begin
  if ALastDockSite <> FLastDockSite then
  begin
    if FLastDockSite <> nil then
    begin
      if TWinControlAccessProtected(FLastDockSite).UseDockManager and
        Supports(TWinControlAccessProtected(FLastDockSite).DockManager, IJvDockManager, JvDockManager) then
        JvDockManager.RemoveControl(Self.ParentForm);
    end;
    ReplaceComponentReference(Self, ALastDockSite, TComponent(FLastDockSite));
  end;
end;

procedure TJvDockClient.SetLeftDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(alLeft);
  inherited SetLeftDock(Value);
end;

procedure TJvDockClient.SetLRDockWidth(const Value: Integer);
begin
  if ParentForm.LRDockWidth <> Value then
    ParentForm.LRDockWidth := Value;
end;

procedure TJvDockClient.SetNCPopupMenu(Value: TPopupMenu);
begin
  ReplaceComponentReference(Self, Value, TComponent(FNCPopupMenu));
end;

procedure TJvDockClient.SetParentVisible(const Value: Boolean);
begin
  FParentVisible := Value;
end;

procedure TJvDockClient.SetRightDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(alRight);
  inherited SetRightDock(Value);
end;

procedure TJvDockClient.SetTBDockHeight(const Value: Integer);
begin
  if ParentForm.TBDockHeight <> Value then
    ParentForm.TBDockHeight := Value;
end;

procedure TJvDockClient.SetTopDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(alTop);
  inherited SetTopDock(Value);
end;

procedure TJvDockClient.SetUnDockLeft(const Value: Integer);
begin
  FUnDockLeft := Value;
end;

procedure TJvDockClient.SetUnDockTop(const Value: Integer);
begin
  FUnDockTop := Value;
end;

procedure TJvDockClient.SetVSPaneWidth(const Value: Integer);
begin
  FVSPaneWidth := Value;
end;

procedure TJvDockClient.ShowParentForm;
begin
  ShowDockForm(ParentForm);
end;

procedure TJvDockClient.WindowProc(var Msg: TMessage);
var
  OldOrient: TDockOrientation;
begin
  if Assigned(FDockStyle) then
    if FDockStyle.DockClientWindowProc(Self, Msg) then
      Exit;
  if not (csDesigning in ComponentState) then
  begin
    case Msg.Msg of
      CM_SHOWINGCHANGED:
        if IsWinXP_UP and JvGlobalDockIsLoading then
        begin
          if GShowingChanged = nil then
            GShowingChanged := TList.Create;
          GShowingChanged.Add(ParentForm);
          Exit;
        end;
      WM_NCLBUTTONDOWN:
        begin
          WMNCLButtonDown(TWMNCHitMessage(Msg));
          if Msg.Result = 1 then
            Exit;
        end;
      WM_NCLBUTTONUP:
        WMNCLButtonUp(TWMNCHitMessage(Msg));
      WM_NCLBUTTONDBLCLK:
        WMNCLButtonDblClk(TWMNCHitMessage(Msg));
      WM_NCMBUTTONDOWN:
        WMNCMButtonDown(TWMNCHitMessage(Msg));
      WM_NCMBUTTONUP:
        WMNCMButtonUp(TWMNCHitMessage(Msg));
      WM_NCMBUTTONDBLCLK:
        WMNCMButtonDblClk(TWMNCHitMessage(Msg));
      WM_NCRBUTTONDOWN:
        begin
          WMNCRButtonDown(TWMNCHitMessage(Msg));
          if FNCPopupMenu <> nil then
            Exit;
        end;
      WM_NCRBUTTONUP:
        WMNCRButtonUp(TWMNCHitMessage(Msg));
      WM_NCRBUTTONDBLCLK:
        WMNCRButtonDblClk(TWMNCHitMessage(Msg));
      WM_NCMOUSEMOVE:
        WMNCMouseMove(TWMNCHitMessage(Msg));
      WM_SIZE:
        WMSize(TWMSize(Msg));
      WM_ACTIVATE:
        WMActivate(TWMActivate(Msg));
      WM_WINDOWPOSCHANGED:
        begin
          ParentForm.ControlState := ParentForm.ControlState + [csDocking];
          OldOrient := ParentForm.DockOrientation;
          ParentForm.DockOrientation := doNoOrient;
          try
            inherited WindowProc(Msg);
          finally
            ParentForm.ControlState := ParentForm.ControlState - [csDocking];
            ParentForm.DockOrientation := OldOrient;
          end;
          Exit;
        end;
      CM_ENTER:
        Activate;
      CM_EXIT:
        Deactivate;
      CM_VISIBLECHANGED:
        CMVisibleChanged(Msg);
    end;
  end;

  inherited WindowProc(Msg);

  if Msg.Msg = WM_SETTEXT then
    if ParentForm.HostDockSite is TJvDockCustomControl then
      TJvDockCustomControl(ParentForm.HostDockSite).UpdateCaption(ParentForm);
end;

procedure TJvDockClient.WMActivate(var Msg: TWMActivate);
begin
  {$IFNDEF COMPILER9_UP}
  InvalidateDockHostSiteOfControl(ParentForm.ActiveControl, Msg.Active = WA_INACTIVE);
  {$ENDIF !COMPILER9_UP}
end;

procedure TJvDockClient.WMNCLButtonDblClk(var Msg: TWMNCHitMessage);
begin
  DoNCButtonDblClk(Msg, mbLeft, msFloat);
end;

procedure TJvDockClient.WMNCLButtonDown(var Msg: TWMNCHitMessage);
begin
  DoNCButtonDown(Msg, mbLeft, msFloat);

  JvGlobalDockClient := Self;

  if (Msg.HitTest = HTCAPTION) and (ParentForm.DragKind = dkDock) and not
    (csDesigning in ComponentState) and not IsIconic(ParentForm.Handle) then
  begin
    SetWindowPos(ParentForm.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or SWP_NOMOVE or SWP_NOSIZE);
    PostMessage(ParentForm.Handle, WM_NCLBUTTONUP, TMessage(Msg).WParam, TMessage(Msg).LParam);
    if ParentForm.Active then
      JvGlobalDockManager.BeginDrag(ParentForm, DirectDrag, Integer(DirectDrag) * 2 - 1);
    Msg.Result := 1;
  end
  else
    Msg.Result := 0;
end;

procedure TJvDockClient.WMNCLButtonUp(var Msg: TWMNCHitMessage);
begin
  DoNCButtonUp(Msg, mbLeft, msFloat);
end;

procedure TJvDockClient.WMNCMButtonDblClk(var Msg: TWMNCHitMessage);
begin
  DoNCButtonDblClk(Msg, mbMiddle, msFloat);
end;

procedure TJvDockClient.WMNCMButtonDown(var Msg: TWMNCHitMessage);
begin
  DoNCButtonDown(Msg, mbMiddle, msFloat);
end;

procedure TJvDockClient.WMNCMButtonUp(var Msg: TWMNCHitMessage);
begin
  DoNCButtonUp(Msg, mbMiddle, msFloat);
end;

procedure TJvDockClient.WMNCMouseMove(var Msg: TWMNCHitMessage);
begin
  DoNCMouseMove(Msg, msFloat);
end;

procedure TJvDockClient.WMNCRButtonDblClk(var Msg: TWMNCHitMessage);
begin
  DoNCButtonDblClk(Msg, mbRight, msFloat);
end;

procedure TJvDockClient.WMNCRButtonDown(var Msg: TWMNCHitMessage);
begin
  DoNCButtonDown(Msg, mbRight, msFloat);
end;

procedure TJvDockClient.WMNCRButtonUp(var Msg: TWMNCHitMessage);
begin
  DoNCButtonUp(Msg, mbRight, msFloat);
end;

procedure TJvDockClient.WMSize(var Msg: TWMSize);
begin
  inherited;
end;

//=== { TJvDockConjoinHostForm } =============================================

constructor TJvDockConjoinHostForm.Create(AOwner: TComponent);
begin
  {$IFDEF JVDOCK_DEBUG}
  OutputDebugString('TJvDockConjoinHostForm.Create');
  {$ENDIF JVDOCK_DEBUG}
  inherited Create(AOwner);
  BorderStyle := ConjoinDockHostBorderStyle;
end;

procedure TJvDockConjoinHostForm.DoClose(var Action: TCloseAction);
begin
  inherited DoClose(Action);
end;

//=== { TJvDockConjoinPanel } ================================================

constructor TJvDockConjoinPanel.Create(AOwner: TComponent);
begin
  {$IFDEF JVDOCK_DEBUG}
  OutputDebugString('TJvDockConjoinPanel.Create');
  {$ENDIF JVDOCK_DEBUG}
  inherited Create(AOwner);
  Parent := TWinControl(AOwner);
  Align := alClient;
  BevelOuter := bvNone;
  DoubleBuffered := True;
  ParentFont := False;
  Caption := '';
end;

procedure TJvDockConjoinPanel.CMUnDockClient(var Msg: TCMUnDockClient);
begin
  inherited;
  { Panel can be closed when
     * DockClientCount <= 1:
        One hidden form left that is freed. Call originates from TControl.Destroy
     * DockClientCount=2 and VisibleDockClientCount=1
        Two forms left, one is freed. Call originates from TControl.Destroy or
        TJvDockManager.DoUnDock etc.
  }
  if Assigned(ParentForm) and ParentForm.HandleAllocated and
    ((DockClientCount <= 1) or ((DockClientCount = 2) and (VisibleDockClientCount = 1)))
  then
    PostMessage(ParentForm.Handle, WM_CLOSE, 0, 0);
  if VisibleDockClientCount <= 2 then
    JvDockControlForm.UpdateCaption(Self, Msg.Client);
  if UseDockManager and (JvDockManager <> nil) then
    JvDockManager.ResetBounds(True);
end;

procedure TJvDockConjoinPanel.CustomDockDrop(Source: TJvDockDragDockObject;
  X, Y: Integer);
begin
  inherited CustomDockDrop(Source, X, Y);
  if Source.Control is TForm then
  begin
    ParentForm.ActiveControl := nil;
    if TForm(Source.Control).FormStyle = fsStayOnTop then
      TForm(Parent).FormStyle := fsStayOnTop;
  end;
  UpdateCaption(nil);
end;

procedure TJvDockConjoinPanel.CustomDockOver(Source: TJvDockDragDockObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  inherited CustomDockOver(Source, X, Y, State, Accept);
  Accept := IsDockable(Self, Source.Control, Source.DropOnControl, Source.DropAlign);
end;

procedure TJvDockConjoinPanel.CustomEndDock(Target: TObject; X, Y: Integer);
begin
  inherited CustomEndDock(Target, X, Y);
end;

procedure TJvDockConjoinPanel.CustomGetSiteInfo(Source: TJvDockDragDockObject;
  Client: TControl; var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  CanDock := IsDockable(Self, Client, Source.DropOnControl, Source.DropAlign);
  inherited CustomGetSiteInfo(Source, Client, InfluenceRect, MousePos, CanDock);
end;

procedure TJvDockConjoinPanel.CustomPositionDockRect(Source: TJvDockDragDockObject;
  X, Y: Integer);
begin
  inherited CustomPositionDockRect(Source, X, Y);
end;

procedure TJvDockConjoinPanel.CustomStartDock(var Source: TJvDockDragDockObject);
begin
  ParentForm.FUnDockControl := nil;
  inherited CustomStartDock(Source);
end;

function TJvDockConjoinPanel.CustomUnDock(Source: TJvDockDragDockObject;
  NewTarget: TWinControl; Client: TControl): Boolean;
begin
  ParentForm.FUnDockControl := Client;

  if not (Client is TJvDockableForm) then
    SetDockSite(TForm(Client), True);
  if ((VisibleDockClientCount = 1) or
    (DockClientCount <= 2)) and (NewTarget <> ParentForm.DockableControl) then
    PostMessage(Parent.Handle, WM_CLOSE, 0, 0);
  UpdateCaption(Client);
  Result := inherited CustomUnDock(Source, NewTarget, Client);
end;

procedure TJvDockConjoinPanel.DockDrop(Source: TDragDockObject; X, Y: Integer);
begin
  if Perform(CM_DOCKCLIENT, WPARAM(Source), {$IFDEF RTL230_UP}PointToLParam{$ELSE}LPARAM{$ENDIF RTL230_UP}(SmallPoint(X, Y))) >= 0 then
  begin
    if Source.Control is TForm then
    begin
      ParentForm.ActiveControl := nil;
      TForm(Source.Control).ActiveControl := nil;

      SetDockSite(TForm(Source.Control), False);
      if TForm(Source.Control).FormStyle = fsStayOnTop then
        TForm(Parent).FormStyle := fsStayOnTop;
    end;
    UpdateCaption(nil);
  end;
end;

procedure TJvDockConjoinPanel.DoDockOver(Source: TDragDockObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := IsDockable(Self, Source.Control, Source.DropOnControl, Source.DropAlign);
end;

function TJvDockConjoinPanel.DoUnDock(NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  ParentForm.FUnDockControl := Client;

  if not (Client is TJvDockableForm) then
    SetDockSite(TForm(Client), True);
  if (VisibleDockClientCount = 1) or
    (DockClientCount <= 2) then
    { It's possible that 2 WM_CLOSE are send to the parent form  }
    PostMessage(Parent.Handle, WM_CLOSE, 0, 0);
  UpdateCaption(Client);
  Result := Perform(CM_UNDOCKCLIENT, WPARAM(NewTarget), LPARAM(Client)) = 0;
end;

function TJvDockConjoinPanel.GetDockClient: TJvDockClient;
begin
  if Assigned(ParentForm) then
    Result := ParentForm.FDockClient
  else
    Result := nil;
end;

function TJvDockConjoinPanel.GetParentForm: TJvDockConjoinHostForm;
begin
  { Dirty }
  Result := TJvDockConjoinHostForm(Parent);
end;

procedure TJvDockConjoinPanel.GetSiteInfo(Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  CanDock := IsDockable(Self, Client);
  if CanDock then
    GetWindowRect(Handle, InfluenceRect);
end;

procedure TJvDockConjoinPanel.ReloadDockedControl(const AControlName: string;
  var AControl: TControl);
begin
  AControl := JvDockFindDockFormWithName(AControlName);
end;

function TJvDockConjoinPanel.CreateDockManager: IDockManager;
var
  TreeClass: TJvDockTreeClass;
  ADockStyle: TJvDockBasicStyle;
begin
  Result := nil;

  if (DockManager = nil) and DockSite and UseDockManager then
  begin
    if Assigned(DockClient) then
    begin
      ADockStyle := DockClient.DockStyle;
      if Assigned(ADockStyle) then
      begin
        TreeClass := ADockStyle.ConjoinPanelTreeClass;
        if Assigned(TreeClass) and (TreeClass <> TJvDockTree) then
          Result := TreeClass.Create(Self, ADockStyle.ConjoinPanelZoneClass, ADockStyle) as IJvDockManager;
      end;
    end;
  end;
  if Result = nil then
    Result := DockManager;
  DoubleBuffered := DoubleBuffered or (Result <> nil);
end;

//=== { TJvDockPanel } =======================================================

constructor TJvDockPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  BevelOuter := bvNone;
  Width := 10;
  Height := 10;
end;

procedure TJvDockPanel.AddDockServer(ADockServer: TJvDockServer);
begin
  { Notification }
end;

procedure TJvDockPanel.CustomDockDrop(Source: TJvDockDragDockObject;
  X, Y: Integer);
begin
  inherited CustomDockDrop(Source, X, Y);

  if Source.Control <> nil then
    ShowDockPanel(True, Source.Control);

  if Source.Control is TForm then begin
    if Assigned(TForm(Source.Control).OnEndDock) then
        TForm(Source.Control).OnEndDock( Self, Source.TargetControl,  X,Y);

  end;

end;

procedure TJvDockPanel.CustomDockOver(Source: TJvDockDragDockObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := IsDockable(Self, Source.Control, Source.DropOnControl, Source.DropAlign);
  if Accept then
    inherited CustomDockOver(Source, X, Y, State, Accept);
end;

procedure TJvDockPanel.CustomEndDock(Target: TObject; X, Y: Integer);
begin
  inherited CustomEndDock(Target, X, Y);
end;

procedure TJvDockPanel.CustomGetSiteInfo(Source: TJvDockDragDockObject; Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  inherited CustomGetSiteInfo(Source, Client, InfluenceRect, MousePos, CanDock);
  CanDock := IsDockable(Self, Client, Source.DropOnControl, Source.DropAlign);
end;

procedure TJvDockPanel.CustomPositionDockRect(Source: TJvDockDragDockObject; X, Y: Integer);

  function Scale(AValue: Integer): Integer;
  begin
    Result := MulDiv(AValue, FCurrentPPI, Source.Control.CurrentPPI);
  end;

var
  ARect: TRect;
begin
  inherited CustomPositionDockRect(Source, X, Y);
  if (VisibleDockClientCount = 0) and (JvGlobalDockClient <> nil) then
  begin
    case Align of
      alTop:
        begin
          ARect.TopLeft := ClientToScreen(Point(0, 0));
          ARect.BottomRight := ClientToScreen(Point(Width, Scale(Source.Control.TBDockHeight)));
        end;
      alBottom:
        begin
          ARect.TopLeft := ClientToScreen(Point(0, -Scale(Source.Control.TBDockHeight)));
          ARect.BottomRight := ClientToScreen(Point(Width, 0));
        end;
      alLeft:
        begin
          ARect.TopLeft := ClientToScreen(Point(0, 0));
          ARect.BottomRight := ClientToScreen(Point(Scale(Source.Control.LRDockWidth), Height));
        end;
      alRight:
        begin
          ARect.TopLeft := ClientToScreen(Point(-Scale(Source.Control.LRDockWidth), 0));
          ARect.BottomRight := ClientToScreen(Point(Width, Height));
        end;
      alClient:
        begin
          ARect.TopLeft := ClientToScreen(Point(0, 0));
          ARect.BottomRight := ClientToScreen(Point(Width, Height));
        end;
    end;
    Source.DockRect := ARect;
  end;
end;

procedure TJvDockPanel.CustomStartDock(var Source: TJvDockDragDockObject);
begin
  inherited CustomStartDock(Source);
end;

function TJvDockPanel.CustomUnDock(Source: TJvDockDragDockObject; NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  ShowDockPanel(False, nil);
  Result := inherited CustomUnDock(Source, NewTarget, Client);
  if not (Client is TJvDockableForm) and (Client is TWinControl) then
    SetDockSite(TWinControl(Client), True);
end;

procedure TJvDockPanel.DockDrop(Source: TDragDockObject; X, Y: Integer);
begin
  if Perform(CM_DOCKCLIENT, WPARAM(Source), {$IFDEF RTL230_UP}PointToLParam{$ELSE}LPARAM{$ENDIF RTL230_UP}(SmallPoint(X, Y))) >= 0 then
  begin
    if Source.Control is TForm then
    begin
      TForm(Source.Control).ActiveControl := nil;
      SetDockSite(TForm(Source.Control), False);
    end;
    UpdateCaption(nil);
  end;
  ShowDockPanel(TWinControl(Source.DragTarget).VisibleDockClientCount > 0, Source.Control);
end;

function TJvDockPanel.DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean;
begin
  Result := IsDockable(Self, Client);
  ShowDockPanel(False, nil);
  Result := Result and (Perform(CM_UNDOCKCLIENT, WPARAM(NewTarget), LPARAM(Client)) = 0);
  if Result then
    if not (Client is TJvDockableForm) then
      SetDockSite(TForm(Client), True);
end;

procedure TJvDockPanel.GetDockedControls(WinControls: TList);
begin
  // THE BASE CLASS DOESN'T SUPPORT THIS. JUST RETURN QUIETLY.
  // See TJvDockAdvPanel.GetDockedControls for the actual implementation.
end;

function TJvDockPanel.FindTabHostForm:TWinControl;
begin
	// base class does not support this. This version just returns nil.
	result := nil;
end;


function TJvDockPanel.GetPanelIndex: Integer;
begin
  case Align of
    alTop:
      Result := 0;
    alBottom:
      Result := 1;
    alLeft:
      Result := 2;
    alRight:
      Result := 3;
  else
    if FCustomFlag then
      Result := 4 {NEW!}
    else
      Result := -1; {unknown.}
  end;
end;

procedure TJvDockPanel.ReloadDockedControl(const AControlName: string;
  var AControl: TControl);
begin
  AControl := JvDockFindDockFormWithName(AControlName);
end;

procedure TJvDockPanel.RemoveDockServer(ADockServer: TJvDockServer);
begin
  { Notification }
end;

procedure TJvDockPanel.ResetPosition;
begin
  case Align of
    alLeft:
      Left := GetClientAlignControlArea(Parent, Align) + 1;
    alRight:
      Left := Parent.ClientWidth - GetClientAlignControlArea(Parent, Align) - Width - 1;
    alTop:
      Top := GetClientAlignControlArea(Parent, Align) + 1;
    alBottom:
      Top := Parent.ClientHeight - GetClientAlignControlArea(Parent, Align) - Height - 1;
  end;
end;

procedure TJvDockPanel.SetDockServer(ADockServer: TJvDockServer);
begin
  if ADockServer <> FDockServer then
  begin
    if FDockServer <> nil then
      RemoveDockServer(FDockServer);

    FDockServer := ADockServer;

    if FDockServer <> nil then
      AddDockServer(FDockServer);
  end;
end;

procedure TJvDockPanel.ShowDockPanel(MakeVisible: Boolean;
  Client: TControl; PanelSizeFrom: TJvDockSetDockPanelSizeFrom);
const
  DefaultDockSize = 100;
var
  DockHeight, DockWidth: Integer;
begin
  if (not MakeVisible and (VisibleDockClientCount > 1)) or
    (JvGlobalDockClient = nil) then
    Exit;

  if Assigned(DockServer.DockSplitterWithAlign[Align]) then
    DockServer.DockSplitterWithAlign[Align].Visible := MakeVisible;

  if MakeVisible and (Client <> nil) then
  begin
    if Width * Height = 0 then
    begin
      if (PanelSizeFrom = sdfDockPanel) or (Client = nil) then
      begin
        DockHeight := TBDockHeight;
        DockWidth := LRDockWidth;
      end
      else
      begin
        DockHeight := Client.TBDockHeight;
        DockWidth := Client.LRDockWidth;
      end;

      if DockHeight = 0 then
        DockHeight := DefaultDockSize;
      if DockWidth = 0 then
        DockWidth := DefaultDockSize;

      Parent.DisableAlign;
      try
        case Align of
          alTop:
            begin
              Top := DockServer.GetClientAlignControl(alTop);
              Height := DockHeight;
              DockServer.TopSplitter.Top := Top + Height;
            end;
          alBottom:
            begin
              Top := Parent.ClientHeight - DockServer.GetClientAlignControl(alBottom) - DockHeight + 1;
              Height := DockHeight;
              DockServer.BottomSplitter.Top := Top + DockServer.BottomSplitter.Height;
            end;
          alLeft:
            begin
              Left := DockServer.GetClientAlignControl(alLeft);
              Width := DockWidth;
              DockServer.LeftSplitter.Left := Left + Width;
            end;
          alRight:
            begin
              Width := DockWidth;
              Left := Parent.ClientWidth - DockServer.GetClientAlignControl(alRight) - DockWidth + 1;
              DockServer.RightSplitter.Left := Left - DockServer.RightSplitter.Width;
            end;
        end;
      finally
        Parent.EnableAlign;
        if UseDockManager and (JvDockManager <> nil) then
          JvDockManager.ResetBounds(True);
      end;
      DockServer.DoFinishSetDockPanelSize(Self);
    end;
  end
  else
  begin
    if (PanelSizeFrom = sdfDockPanel) or (Client = nil) then
    begin
      if Height > 0 then
        TBDockHeight := Height;
      if Width > 0 then
        LRDockWidth := Width;
    end
    else
    begin
      if Height > 0 then
        Client.TBDockHeight := Height;
      if Width > 0 then
        Client.LRDockWidth := Width;
    end;
    if Align in [alLeft, alRight] then
      Width := 0
    else
      Height := 0;

    ResetPosition;
  end;

  if MakeVisible and (Client <> nil) then
  begin
    if not Client.Visible then
      Client.Show;

    if DockServer.AutoFocusDockedForm and (Client is TWinControl) and
      not TWinControl(Client).Focused and TWinControl(Client).CanFocus then
      TWinControl(Client).SetFocus;
  end;
end;

function TJvDockPanel.CreateDockManager: IDockManager;
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
  DoubleBuffered := DoubleBuffered or (Result <> nil);
end;

//=== { TJvDockServer } ======================================================

constructor TJvDockServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoFocusDockedForm := True;
  CreateSplitterStyle;
end;

destructor TJvDockServer.Destroy;
begin
  DestroySplitterStyle;
  inherited Destroy;
end;

procedure TJvDockServer.AddDockStyle(ADockStyle: TJvDockBasicStyle);
begin
  JvGlobalDockManager.RegisterDockServer(Self);

  if Assigned(ADockStyle) and Assigned(ADockStyle.DockPanelClass) then
    FDockPanelClass := ADockStyle.DockPanelClass
  else
    FDockPanelClass := DefaultDockPanelClass;

  if Assigned(ADockStyle) and Assigned(ADockStyle.DockSplitterClass) then
    FDockSplitterClass := ADockStyle.DockSplitterClass
  else
    FDockSplitterClass := DefaultDockSplitterClass;

  if not (csDesigning in ComponentState) then
  begin
    CreateDockPanelAndSplitter;
    SetSplitterStyles;
  end;
end;

procedure TJvDockServer.CreateDockPanelAndSplitter;
//var
//  ControlList: TList;

  function CreatePanel(AParent: TWinControl; Align: TAlign; Name: string): TJvDockPanel;
  begin
    if (FDockPanelClass <> nil) and
      (FDockPanelClass <> TJvDockPanelClass(ClassType)) then
    begin
      Result := FDockPanelClass.Create({Owner} Self);
      // (ahuser) Delphi 5's TComponent.Notification() will fail if Owner=Self.Owner
      Result.Parent := AParent; {ParentForm;}
      Result.Name := Name;
      Result.Caption := '';
      Result.Align := Align;
      Result.DockServer := Self;
      Result.ResetPosition;
      if Align in [alTop, alBottom] then
        Result.Height := 0
      else
      if Align in [alLeft, alRight] then
        Result.Width := 0;
      SetDockSite(Result, True);

      Result.FreeNotification(Self);
    end
    else
      Result := nil;
  end;

  function CreateSplitter(AParent: TWinControl; Align: TAlign; Name: string): TJvDockSplitter;
  begin
    if (FDockSplitterClass <> nil) and
      (FDockSplitterClass <> TJvDockSplitterClass(ClassType)) then
    begin
      Result := FDockSplitterClass.Create({Owner}Self);
        // (ahuser) Delphi 5's TComponent.Notification() will fail if Owner=Self.Owner
      Result.Parent := AParent; {ParentForm;}
      Result.Name := Name;
      Result.Visible := False;
      Result.Align := Align;
      Result.DockServer := Self;
      Result.FreeNotification(Self);
    end
    else
      Result := nil;
  end;

  {NEW! Docking panel in center area, or other part of form than the built in left/top/right/bottom panels! }

  procedure CustomDockPanel;
  var
    AParent: TWinControl;
    LAlign: TAlign;
  begin
    AParent := PArentForm;
    LAlign := alClient;
    if Assigned(FOnCustomPanel) then
      //TJvDockCustomPanelEvent:
      FOnCustomPanel(Self, AParent, LAlign)
    else
    begin
      FDockPanels[dpCustom] := nil;
      FSplitters[dpCustom] := nil;
      Exit;
    end;
    if (LAlign <> alNone) and Assigned(AParent) then
    begin // Don't create if align is alNone or parent is NIL.
      FDockPanels[dpCustom] := CreatePanel(AParent, LAlign, {NOTRANSLATE} 'CustomDockPanel');
      FDockPanels[dpCustom].CustomFlag := True;
      if AParent is TPanel then
        FDockPanels[dpCustom].Color := TPanel(AParent).Color;
      FSplitters[dpCustom] := nil; //CreateSplitter(ParentForm, alTop, 'CustomSplitter' + cDefaultNameSuffix);
        //FSplitters[dpCustom].Visible := False;
    end
    else
      FDockPanels[dpCustom] := nil;
    FSplitters[dpCustom] := nil;
  end;

begin
  //  ControlList := TList.Create;
  //  try
  FDockPanels[dpLeft] := CreatePanel(ParentForm, alLeft, {NOTRANSLATE} 'LeftDockPanel');
  FSplitters[dpLeft] := CreateSplitter(ParentForm, alLeft, {NOTRANSLATE} 'LeftSplitter');
  FDockPanels[dpRight] := CreatePanel(ParentForm, alRight, {NOTRANSLATE} 'RightDockPanel');
  FSplitters[dpRight] := CreateSplitter(ParentForm, alRight, {NOTRANSLATE} 'RightSplitter');
  FDockPanels[dpTop] := CreatePanel(ParentForm, alTop, {NOTRANSLATE} 'TopDockPanel');
  FSplitters[dpTop] := CreateSplitter(ParentForm, alTop, {NOTRANSLATE} 'TopSplitter');
  FDockPanels[dpBottom] := CreatePanel(ParentForm, alBottom, {NOTRANSLATE} 'BottomDockPanel');
  FSplitters[dpBottom] := CreateSplitter(ParentForm, alBottom, {NOTRANSLATE} 'BottomSplitter');

  CustomDockPanel;

//  FSplitters[dpCustom] := nil; // NOT USED!
  //  finally
  //    ControlList.Free;
  //  end;
end;

procedure TJvDockServer.CreateSplitterStyle;
const
  cCursor: array [TJvDockPosition] of TCursor =
    (crHSplit, crHSplit, crVSplit, crVSplit, crNone);
var
  Position: TJvDockPosition;
begin
  for Position := Low(TJvDockPosition) to High(TJvDockPosition) do
  begin
    FSplitterStyles[Position] := TJvDockSplitterStyle.Create(Splitter[Position], cCursor[Position]);
    FSplitterStyles[Position].FDockServer := Self;
  end;
end;

procedure TJvDockServer.DestroyDockPanelAndSplitter;
var
  Position: TJvDockPosition;
begin
  for Position := Low(TJvDockPosition) to High(TJvDockPosition) do
  begin
    { SplitterStyles may already be destroyed }
    if Assigned(SplitterStyle[Position]) then
      SplitterStyle[Position].Splitter := nil;
    FreeAndNil(FDockPanels[Position]);
    FreeAndNil(FSplitters[Position]);
  end;
end;

procedure TJvDockServer.DestroySplitterStyle;
var
  Position: TJvDockPosition;
begin
  for Position := Low(TJvDockPosition) to High(TJvDockPosition) do
    FreeAndNil(FSplitterStyles[Position]);
end;

procedure TJvDockServer.DoFinishSetDockPanelSize(DockPanel: TJvDockPanel);
begin
  if Assigned(FOnFinishSetDockPanelSize) then
    FOnFinishSetDockPanelSize(DockPanel);
end;

procedure TJvDockServer.DoFloatDockClients(DockPanel: TJvDockPanel);
var
  I: Integer;
  DC: TJvDockClient;
begin
  if not (csDesigning in ComponentState) and (DockPanel <> nil) then
    for I := DockPanel.DockClientCount - 1 downto 0 do
    begin
      DC := FindDockClient(DockPanel.DockClients[I]);
      if DC <> nil then
        DC.RestoreChild;
    end;
end;

procedure TJvDockServer.DoGetClientAlignControl(Align: TAlign; var Value: Integer);
begin
  if Assigned(FOnGetClientAlignSize) then
    FOnGetClientAlignSize(Align, Value);
end;

function TJvDockServer.GetClientAlignControl(Align: TAlign): Integer;
begin
  Result := GetClientAlignControlArea(ParentForm, Align);
  DoGetClientAlignControl(Align, Result);
end;

function TJvDockServer.GetDockPanel(DockPosition: TJvDockPosition): TJvDockPanel;
begin
  Result := FDockPanels[DockPosition];
end;

function TJvDockServer.GetDockPanelIndex(const Index: Integer): TJvDockPanel;
begin
  Result := FDockPanels[TJvDockPosition(Index)];
end;

function TJvDockServer.GetDockPanelWithAlign(Index: TAlign): TJvDockPanel;
begin
  Result := nil;
  case Index of
    alLeft:
      Result := LeftDockPanel;
    alRight:
      Result := RightDockPanel;
    alTop:
      Result := TopDockPanel;
    alBottom:
      Result := BottomDockPanel;
  end;
end;

function TJvDockServer.GetDockSplitterWithAlign(Index: TAlign): TJvDockSplitter;
begin
  Result := nil;
  case Index of
    alLeft:
      Result := LeftSplitter;
    alRight:
      Result := RightSplitter;
    alTop:
      Result := TopSplitter;
    alBottom:
      Result := BottomSplitter;
  end;
end;

function TJvDockServer.GetSplitter(DockPosition: TJvDockPosition): TJvDockSplitter;
begin
  Result := FSplitters[DockPosition];
end;

function TJvDockServer.GetSplitterIndex(const Index: Integer): TJvDockSplitter;
begin
  Result := FSplitters[TJvDockPosition(Index)];
end;

function TJvDockServer.GetSplitterStyle(DockPosition: TJvDockPosition): TJvDockSplitterStyle;
begin
  Result := FSplitterStyles[DockPosition];
end;

function TJvDockServer.GetSplitterStyleIndex(const Index: Integer): TJvDockSplitterStyle;
begin
  Result := FSplitterStyles[TJvDockPosition(Index)];
end;

procedure TJvDockServer.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  Position: TJvDockPosition;
begin
  inherited Notification(AComponent, Operation);

  if Operation = opRemove then
  begin
    for Position := Low(TJvDockPosition) to High(TJvDockPosition) do
      if AComponent = DockPanel[Position] then
      begin
        FDockPanels[Position] := nil;
        DestroyDockPanelAndSplitter;
      end
      else
      if AComponent = Splitter[Position] then
      begin
        FSplitters[Position] := nil;
        if Assigned(SplitterStyle[Position]) then
          SplitterStyle[Position].Splitter := nil;
        DestroyDockPanelAndSplitter;
      end;
  end;
end;

procedure TJvDockServer.RemoveDockStyle(ADockStyle: TJvDockBasicStyle);
begin
  DoFloatDockClients(TopDockPanel);
  DoFloatDockClients(BottomDockPanel);
  DoFloatDockClients(LeftDockPanel);
  DoFloatDockClients(RightDockPanel);

  DestroyDockPanelAndSplitter;

  FDockPanelClass := nil;
  FDockSplitterClass := nil;

  { !! can be nil }
  if JvGlobalDockManager <> nil then
    JvGlobalDockManager.UnRegisterDockServer(Self);
end;

procedure TJvDockServer.SetBottomDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(BottomDockPanel);
  inherited SetBottomDock(Value);
end;

procedure TJvDockServer.SetEnableDock(const Value: Boolean);
begin
  if not Value then
  begin
    DoFloatDockClients(TopDockPanel);
    DoFloatDockClients(BottomDockPanel);
    DoFloatDockClients(LeftDockPanel);
    DoFloatDockClients(RightDockPanel);
  end;
  inherited SetEnableDock(Value);
end;

procedure TJvDockServer.SetLeftDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(LeftDockPanel);
  inherited SetLeftDock(Value);
end;

procedure TJvDockServer.SetRightDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(RightDockPanel);
  inherited SetRightDock(Value);
end;

procedure TJvDockServer.SetSplitterStyle(DockPosition: TJvDockPosition;
  ASplitterStyle: TJvDockSplitterStyle);
begin
  FSplitterStyles[DockPosition].Assign(ASplitterStyle);
end;

procedure TJvDockServer.SetSplitterStyleIndex(const Index: Integer;
  ASplitterStyle: TJvDockSplitterStyle);
begin
  FSplitterStyles[TJvDockPosition(Index)].Assign(ASplitterStyle);
end;

procedure TJvDockServer.SetSplitterStyles;
var
  Position: TJvDockPosition;
begin
  for Position := Low(TJvDockPosition) to High(TJvDockPosition) do
  begin
    SplitterStyle[Position].Splitter := Splitter[Position];
    SplitterStyle[Position].SetSplitterStyle;
  end;
end;

procedure TJvDockServer.SetTopDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(TopDockPanel);
  inherited SetTopDock(Value);
end;

procedure TJvDockServer.WindowProc(var Msg: TMessage);
begin
  if Assigned(FDockStyle) then
    if FDockStyle.DockServerWindowProc(Self, Msg) then
      Exit;
  if not (csDesigning in ComponentState) then
    if Msg.Msg = WM_ACTIVATE then
      WMActivate(TWMActivate(Msg));
  inherited WindowProc(Msg);
end;

procedure TJvDockServer.WMActivate(var Msg: TWMActivate);
{$IFNDEF COMPILER9_UP}
var
  Control: TWinControl;
{$ENDIF !COMPILER9_UP}
begin
  {$IFNDEF COMPILER9_UP}
  if Msg.Active = WA_INACTIVE then
  begin
    Control := ParentForm.ActiveControl;
    if Assigned(Control) then
      InvalidateDockHostSiteOfControl(Control, True);
  end
  else
  begin
    Control := ParentForm.ActiveControl;
    if Assigned(Control) then
    begin
      //      { ?? }
      //      if AutoFocusDockedForm and Control.CanFocus then
      //        Control.SetFocus;
      InvalidateDockHostSiteOfControl(Control, False);
    end;
  end;
  {$ENDIF !COMPILER9_UP}
end;

//=== { TJvDockSplitter } ====================================================

constructor TJvDockSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoSnap := False;
end;

function TJvDockSplitter.FindControl: TControl;
begin
  if DockServer <> nil then
    Result := DockServer.GetDockPanelWithAlign(Align)
  else
    Result := inherited FindControl;
end;

function TJvDockSplitter.GetSplitterIndex: Integer;
begin
  case Align of
    alTop:
      Result := 0;
    alBottom:
      Result := 1;
    alLeft:
      Result := 2;
    alRight:
      Result := 3;
  else
    Result := -1;
  end;
end;

//=== { TJvDockSplitterStyle } ===============================================

constructor TJvDockSplitterStyle.Create(ASplitter: TJvDockSplitter; ACursor: TCursor);
begin
  inherited Create;
  FSplitter := ASplitter;
  Color := clBtnFace;
  Cursor := ACursor;
  ParentColor := False;
  ResizeStyle := rsPattern;
  FSize := 3;
  FMinSize := 30;
end;

procedure TJvDockSplitterStyle.Assign(Source: TPersistent);
begin
  if Source is TJvDockSplitterStyle then
  begin
    Color := TJvDockSplitterStyle(Source).Color;
    Cursor := TJvDockSplitterStyle(Source).Cursor;
    ParentColor := TJvDockSplitterStyle(Source).ParentColor;
    ResizeStyle := TJvDockSplitterStyle(Source).ResizeStyle;
    Size := TJvDockSplitterStyle(Source).Size;
  end
  else
    inherited Assign(Source);
end;

procedure TJvDockSplitterStyle.AssignTo(Dest: TPersistent);
begin
  if Dest is TJvDockSplitterStyle then
    with TJvDockSplitterStyle(Dest) do
    begin
      Color := Self.Color;
      Cursor := Self.Cursor;
      ParentColor := Self.ParentColor;
      ResizeStyle := Self.ResizeStyle;
      Size := Self.Size;
    end
  else
    inherited AssignTo(Dest);
end;

procedure TJvDockSplitterStyle.AssignToSplitter(Dest: TJvDockSplitter);
begin
  Dest.Color := Color;
  Dest.Cursor := Cursor;
  Dest.MinSize := MinSize;
  Dest.ParentColor := ParentColor;
  Dest.ResizeStyle := ResizeStyle;
  if Dest.Align in [alTop, alBottom] then
    Dest.Height := MulDiv(Size, Dest.FCurrentPPI, 96)
  else
    Dest.Width := MulDiv(Size, Dest.FCurrentPPI, 96);
end;

procedure TJvDockSplitterStyle.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    ParentColor := False;
    if Assigned(FSplitter) then
      FSplitter.Color := Value;
  end;
end;

procedure TJvDockSplitterStyle.SetCursor(const Value: TCursor);
begin
  FCursor := Value;
  if Assigned(FSplitter) then
    FSplitter.Cursor := Value;
end;

procedure TJvDockSplitterStyle.SetMinSize(const Value: TJvDockSplitterSize);
begin
  FMinSize := Value;
  if Assigned(FSplitter) then
    FSplitter.MinSize := Value;
end;

procedure TJvDockSplitterStyle.SetParentColor(const Value: Boolean);
begin
  if FParentColor <> Value then
  begin
    FParentColor := Value;
    if Value then
      FColor := FDockServer.ParentForm.Color;
    if Assigned(FSplitter) then
      FSplitter.ParentColor := Value;
  end;
end;

procedure TJvDockSplitterStyle.SetResizeStyle(const Value: TResizeStyle);
begin
  FResizeStyle := Value;
  if Assigned(FSplitter) then
    FSplitter.ResizeStyle := Value;
end;

procedure TJvDockSplitterStyle.SetSize(const Value: TJvDockSplitterSize);
begin
  FSize := Value;
  if Assigned(FSplitter) then
  begin
    if FSplitter.Align in [alTop, alBottom] then
      FSplitter.Height := Value
    else
      FSplitter.Width := Value;
  end;
end;

procedure TJvDockSplitterStyle.SetSplitterStyle;
begin
  if Assigned(FSplitter) then
    AssignToSplitter(FSplitter);
end;

//=== { TJvDockTabHostForm } =================================================

constructor TJvDockTabHostForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := TabDockHostBorderStyle;
end;

procedure TJvDockTabHostForm.DoDock(NewDockSite: TWinControl; var ARect: TRect);
begin
  inherited;
  if not JvGlobalDockIsLoading and
    not Assigned(NewDockSite) and
    (GetActiveDockForm <> nil) and
    GetParentForm(Self).Visible and
    GetActiveDockForm.CanFocus then
       GetActiveDockForm.SetFocus;
end;

function TJvDockTabHostForm.GetActiveDockForm: TForm;
begin
  if PageControl.ActiveDockForm is TForm then
    Result := TForm(PageControl.ActiveDockForm)
  else
    Result := nil;
end;

//------------------------------------------------------------------------
// ShowDockedControl:
//
// If under program control you need a tabdock to switch pages to show
// a particular docked form, you call DockTabHostForm.ShowControl(aForm):
//
// If aControl is docked in DockTabhostForm.PageControl, this will
// change the PageControl.ActivePage to that page.
//------------------------------------------------------------------------

procedure TJvDockTabHostForm.ShowDockedControl(AControl: TWinControl); // NEW! WPostma.
var
  t: Integer;
  TabSheet: TJvDockTabSheet;
begin
  Assert(Assigned(AControl));
  if not Assigned(AControl.Parent) then
    exit;
  if not (AControl.Parent is TJvDockTabSheet) then
    exit;
  TabSheet := TJvDockTabSheet(AControl.Parent);
  // Now go through the pages and find it!
  for t := 0 to FPageControl.Count - 1 do
  begin
    if FPageControl.Pages[t] = TabSheet then
    begin
      FPageControl.ActivePage := TabSheet; { set page!}
      if AControl is TForm then
        TabSheet.Caption := TForm(AControl).Caption;
      Self.Caption := TabSheet.Caption;
      exit;
    end;
  end;
end;

// If while docked, you need a docked form's caption to update,
// and this update needs to be shown on the tabs on the screen,
// and possibly also the title bar of the docktabhost form needs
// updating also, then you need to call this function, like this:
//   DockTabHostForm.UpdateCaption(MyForm)

procedure TJvDockTabHostForm.UpdateCaption(AControl: TWinControl); //virtual;
var
  t: Integer;
  TabSheet: TJvDockTabSheet;
begin
  Assert(Assigned(AControl));
  if not Assigned(AControl.Parent) then
    exit;
  if not (AControl.Parent is TJvDockTabSheet) then
    exit;
  TabSheet := TJvDockTabSheet(AControl.Parent);
  // Now go through the pages and find the one page that needs
  // its caption updated.
  for t := 0 to FPageControl.Count - 1 do
  begin
    if FPageControl.Pages[t] = TabSheet then
    begin
      if AControl is TForm then
        TabSheet.Caption := TForm(AControl).Caption;
      if FPageControl.ActivePage = TabSheet then
        Self.Caption := TabSheet.Caption; // tabhost's form caption needs updating.
      exit;
    end;
  end;
end;

//=== { TJvDockTabPageControl } ==============================================

constructor TJvDockTabPageControl.Create(AOwner: TComponent);
var
  ADockClient: TJvDockClient;
begin
  inherited Create(AOwner);
  if AOwner is TWinControl then
    Parent := TWinControl(AOwner);
  FStyleLink := TJvDockStyleLink.Create;
  { First set DockStyle then OnStyleChanged so no OnStyleChanged is fired;
    we do it ourself in AfterContruction }
  ADockClient := FindDockClient(Parent);
  if Assigned(ADockClient) then
    FStyleLink.DockStyle := ADockClient.DockStyle;
  FStyleLink.OnStyleChanged := DockStyleChanged;
  SetDockSite(Self, True);
  PopupMenu := DockPageControlPopupMenu;
  HotTrack := DockPageControlHotTrack;
  DoubleBuffered := True;
  Caption := '';
  FVersion := $00040000;
end;

destructor TJvDockTabPageControl.Destroy;
begin
  FStyleLink.Free;
  SetDockSite(Self, False);
  inherited Destroy;
end;

procedure TJvDockTabPageControl.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  case TabPosition of
    tpLeft:
      Inc(Rect.Left, 2);
    tpRight:
      Dec(Rect.Right, 2);
    tpBottom:
      begin
        Dec(Rect.Top, 1);
        Dec(Rect.Bottom, 2);
      end;
  end;
end;

procedure TJvDockTabPageControl.AfterConstruction;
begin
  inherited AfterConstruction;
  FStyleLink.StyleChanged;
end;

procedure TJvDockTabPageControl.CustomDockDrop(Source: TJvDockDragDockObject;
  X, Y: Integer);
var
  DragDockObject: TDragDockObject;
begin
  if Source.DropAlign in [alClient, alNone] then
  begin
    DragDockObject := TDragDockObject.Create(Source.Control);
    try
      DragDockObject.DockRect := Source.DockRect;
      DragDockObject.Control := Source.Control;
      Perform(CM_DOCKCLIENT, WPARAM(DragDockObject), {$IFDEF RTL230_UP}PointToLParam{$ELSE}LPARAM{$ENDIF RTL230_UP}(SmallPoint(X, Y)));
      UpdateCaption(nil);
    finally
      DragDockObject.Free;
    end;
  end
  else
    inherited CustomDockDrop(Source, X, Y);
  if Source.Control is TForm then
  begin
    TForm(Source.Control).ActiveControl := nil;
    SetDockSite(TForm(Source.Control), False);
  end;
end;

procedure TJvDockTabPageControl.CustomDockOver(Source: TJvDockDragDockObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  Rect: TRect;
begin
  inherited CustomDockOver(Source, X, Y, State, Accept);

  Accept := IsDockable(Self, Source.Control, Source.DropOnControl, Source.DropAlign);

  if Accept then
  begin
    ComputeDockingRect(Self, Rect, Point(ClientWidth div 2, ClientHeight div 2));
    Source.DockRect := Rect;
  end;
end;

procedure TJvDockTabPageControl.CustomEndDock(Target: TObject; X, Y: Integer);
begin
  inherited CustomEndDock(Target, X, Y);
end;

procedure TJvDockTabPageControl.CustomGetDockEdge(Source: TJvDockDragDockObject;
  MousePos: TPoint; var DropAlign: TAlign);
var
  ARect: TRect;
begin
  ARect := Source.DockRect;
  DropAlign := ComputeDockingRect(Source.Control, ARect, MousePos);
end;

procedure TJvDockTabPageControl.CustomGetSiteInfo(Source: TJvDockDragDockObject;
  Client: TControl; var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  inherited CustomGetSiteInfo(Source, Client, InfluenceRect, MousePos, CanDock);
  CanDock := IsDockable(Self, Client, Source.DropOnControl, Source.DropAlign);
end;

procedure TJvDockTabPageControl.CustomPositionDockRect(Source: TJvDockDragDockObject;
  X, Y: Integer);
begin
  inherited CustomPositionDockRect(Source, X, Y);
end;

procedure TJvDockTabPageControl.CustomStartDock(var Source: TJvDockDragDockObject);
begin
  inherited CustomStartDock(Source);
end;

function TJvDockTabPageControl.CustomUnDock(Source: TJvDockDragDockObject;
  NewTarget: TWinControl; Client: TControl): Boolean;
begin
  if not (Client is TJvDockableForm) then
    SetDockSite(TForm(Client), True);
  if (VisibleDockClientCount = 1) or (DockClientCount <= 2) then
    PostMessage(Parent.Handle, WM_CLOSE, 0, 0);
  UpdateCaption(Client);
  Result := Perform(CM_UNDOCKCLIENT, WPARAM(NewTarget), LPARAM(Client)) = 0;
end;

procedure TJvDockTabPageControl.DockDrop(Source: TDragDockObject; X,
  Y: Integer);
begin
  if Perform(CM_DOCKCLIENT, WPARAM(Source), {$IFDEF RTL230_UP}PointToLParam{$ELSE}LPARAM{$ENDIF RTL230_UP}(SmallPoint(X, Y))) >= 0 then
  begin
    if Source.Control is TForm then
    begin
      TForm(Source.Control).ActiveControl := nil;
      SetDockSite(TWinControl(Source.Control), False);
      if TForm(Source.Control).FormStyle = fsStayOnTop then
        TForm(Parent).FormStyle := fsStayOnTop;
    end;
    UpdateCaption(nil);
  end;
end;

procedure TJvDockTabPageControl.DockStyleChanged(Sender: TObject);
begin
  SyncWithStyle;
end;

function TJvDockTabPageControl.DoUnDock(NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  if not (Client is TJvDockableForm) then
    SetDockSite(TForm(Client), True);
  if (VisibleDockClientCount = 1) or
    (DockClientCount <= 2) then
    PostMessage(Parent.Handle, WM_CLOSE, 0, 0);
  UpdateCaption(Client);
  Result := Perform(CM_UNDOCKCLIENT, WPARAM(NewTarget), LPARAM(Client)) = 0;
end;

function TJvDockTabPageControl.GetActiveDockForm: TCustomForm;
begin
  Result := DockForm[ActivePageIndex];
end;

function TJvDockTabPageControl.GetDockForm(Index: Integer): TCustomForm;
var
  Page: TJvDockTabSheet;
begin
  Result := nil;

  if (Index > -1) and (Index < Count) then
  begin
    Page := Pages[Index];
    if Assigned(Page) and (Page.ControlCount = 1) and (Page.Controls[0] is TCustomForm) then
    begin
      Result := TCustomForm(Page.Controls[0]);
    end
  end;
end;

function TJvDockTabPageControl.GetDockStyle: TJvDockObservableStyle;
begin
  Result := FStyleLink.DockStyle;
end;

function TJvDockTabPageControl.GetParentForm: TJvDockTabHostForm;
begin
  if Parent is TJvDockTabHostForm then
    Result := TJvDockTabHostForm(Parent)
  else
    Result := nil;
end;

procedure TJvDockTabPageControl.LoadFromStream(Stream: TStream);
var
  I, ACount, NameLen, SheetVisible, ActiveSheetIndex: Integer;
  ControlName: UTF8String;
  AControl: TControl;
  Index: Integer;
  TempW, TempH: Integer;
begin
  Stream.Read(I, SizeOf(I));

  Stream.Read(ACount, SizeOf(ACount));
  Index := 0;
  for I := 0 to ACount - 1 do
  begin
    ControlName := '';

    Stream.Read(NameLen, SizeOf(NameLen));
    if NameLen > 0 then
    begin
      SetLength(ControlName, NameLen);
      Stream.Read(ControlName[1], NameLen);
    end;

    Stream.Read(SheetVisible, SizeOf(SheetVisible));

    if ControlName <> '' then
    begin
      ReloadDockedControl({$IFDEF UNICODE}UTF8ToString{$ELSE}UTF8Decode{$ENDIF}(ControlName), AControl);
      if AControl <> nil then
      begin
        TempW := AControl.UndockWidth;
        TempH := AControl.UndockHeight;
        AControl.ManualDock(Self, nil, alClient);
        AControl.UndockWidth := TempW;
        AControl.UndockHeight := TempH;
        { DockClients[Index] is always AControl? }
        DockClients[Index].Visible := Boolean(SheetVisible);
        if (Self is TJvDockVSNETTabPageControl) and (Index = Count - 1) then
          TJvDockVSNETTabSheet(Pages[Index]).OldVisible := Boolean(SheetVisible);
        Inc(Index);
      end;
    end;
  end;

  Stream.Read(ActiveSheetIndex, SizeOf(ActiveSheetIndex));
  ActivePageIndex := ActiveSheetIndex;
  Change;
end;

procedure TJvDockTabPageControl.ReloadDockedControl(const AControlName: string;
  var AControl: TControl);
begin
  AControl := JvDockFindDockFormWithName(AControlName);
end;

procedure TJvDockTabPageControl.SaveToStream(Stream: TStream);
var
  I, ACount, NameLen, SheetVisible, ActiveSheetIndex: Integer;
  ControlName: UTF8String;
  CurrentControl: TControl;
  TabPageStreamEndFlag: Integer;
begin
  Stream.Write(FVersion, SizeOf(FVersion));
  ACount := Count;

  Stream.Write(ACount, SizeOf(ACount));
  for I := 0 to ACount - 1 do
  begin
    if Pages[I].ControlCount > 0 then
    begin
      CurrentControl := Pages[I].Controls[0];

      ControlName := UTF8Encode(CurrentControl.Name);
      NameLen := Length(ControlName);
      Stream.Write(NameLen, SizeOf(NameLen));
      if NameLen > 0 then
        Stream.Write(ControlName[1], NameLen);

      SheetVisible := 0;
      if (Self is TJvDockVSNETTabPageControl) and (ParentForm.HostDockSite is TJvDockPanel) then
        SheetVisible := Integer(TJvDockVSNETTabSheet(Pages[I]).OldVisible)
      else
        SheetVisible := SheetVisible + Integer(CurrentControl.Visible);

      Stream.Write(SheetVisible, SizeOf(SheetVisible));
    end;
  end;
  ActiveSheetIndex := ActivePageIndex;

  Stream.Write(ActiveSheetIndex, SizeOf(ActiveSheetIndex));

  TabPageStreamEndFlag := -10;
  Stream.Write(TabPageStreamEndFlag, SizeOf(TabPageStreamEndFlag));
end;

procedure TJvDockTabPageControl.ScaleForPPI(NewPPI: Integer);
// This is to work around the fact that TForm.ScaleForPPIRect
// does not scale parented forms with not allocated handle
Var
  I: integer;
  Form : TCustomForm;
begin
  for I := 0 to Count - 1 do
  begin
    Form := DockForm[I];
    if Assigned(Form) then
      Form.HandleNeeded;
  end;

  inherited;
end;

procedure TJvDockTabPageControl.SyncWithStyle;
begin
  HotTrack := DockStyle.TabServerOption.HotTrack;
  TabPosition := DockStyle.TabServerOption.TabPosition;
end;

//=== { TJvGlobalDockManager } ===============================================

constructor TJvGlobalDockManager.Create;
begin
  inherited Create;
  FDockServers := TList.Create;
  FDockClients := TList.Create;
  FDockableForms := TList.Create;
end;

destructor TJvGlobalDockManager.Destroy;
begin
  FDockableForms.Free;
  FDockServers.Free;
  FDockClients.Free;
  inherited Destroy;
end;

function TJvGlobalDockManager.FindDockClientForm(
  const AName: string): TControl;
var
  I: Integer;
begin
  for I := 0 to DockClientCount - 1 do
  begin
    Result := DockClient[I].ParentForm;
    if Result.Name = AName then
      Exit;
  end;
  Result := nil;
end;

function TJvGlobalDockManager.FindDockControlForm(
  const AName: string): TControl;
begin
  Result := FindDockServerForm(AName);
  if Result = nil then
    FindDockClientForm(AName);
end;

function TJvGlobalDockManager.FindDockServerForm(
  const AName: string): TControl;
var
  I: Integer;
begin
  for I := 0 to DockServerCount - 1 do
  begin
    Result := DockServer[I].ParentForm;
    if Result.Name = AName then
      Exit;
  end;
  Result := nil;
end;

function TJvGlobalDockManager.GetDockableForm(
  const Index: Integer): TJvDockableForm;
begin
  Result := TJvDockableForm(FDockableForms[Index]);
end;

function TJvGlobalDockManager.GetDockableFormCount: Integer;
begin
  Result := FDockableForms.Count;
end;

function TJvGlobalDockManager.GetDockClient(
  const Index: Integer): TJvDockClient;
begin
  Result := TJvDockClient(FDockClients[Index]);
end;

function TJvGlobalDockManager.GetDockClientCount: Integer;
begin
  Result := FDockClients.Count;
end;

function TJvGlobalDockManager.GetDockServer(
  const Index: Integer): TJvDockServer;
begin
  Result := TJvDockServer(FDockServers[Index]);
end;

function TJvGlobalDockManager.GetDockServerCount: Integer;
begin
  Result := FDockServers.Count;
end;

procedure TJvGlobalDockManager.RegisterDockableForm(
  ADockableForm: TJvDockableForm);
var
  Index: Integer;
begin
  Index := FDockableForms.IndexOf(ADockableForm);
  if Index < 0 then
    FDockableForms.Add(ADockableForm);
end;

procedure TJvGlobalDockManager.RegisterDockClient(
  ADockClient: TJvDockClient);
var
  Index: Integer;
begin
  Index := FDockClients.IndexOf(ADockClient);
  if Index < 0 then
    FDockClients.Add(ADockClient);
end;

procedure TJvGlobalDockManager.RegisterDockServer(
  ADockServer: TJvDockServer);
var
  Index: Integer;
begin
  Index := FDockServers.IndexOf(ADockServer);
  if Index < 0 then
    FDockServers.Add(ADockServer);
end;

procedure TJvGlobalDockManager.UnRegisterDockableForm(
  ADockableForm: TJvDockableForm);
begin
  FDockableForms.Remove(ADockableForm);
end;

procedure TJvGlobalDockManager.UnRegisterDockClient(
  ADockClient: TJvDockClient);
begin
  FDockClients.Remove(ADockClient);
end;

procedure TJvGlobalDockManager.UnRegisterDockServer(
  ADockServer: TJvDockServer);
begin
  FDockServers.Remove(ADockServer);
end;

procedure InitDockManager;
begin
  try
    JvGlobalDockManager.Free;
    JvGlobalDockManager := nil;
    JvGlobalDockManager := TJvGlobalDockManager.Create;
  except
  end;
end;

procedure DoneDockManager;
begin
  JvGlobalDockManager.Free;
  JvGlobalDockManager := nil;
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  InitDockManager;

finalization
  DoneDockManager;
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
