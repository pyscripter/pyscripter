unit TB2Toolbar;

{
  Toolbar2000
  Copyright (C) 1998-2008 by Jordan Russell
  All rights reserved.

  The contents of this file are subject to the "Toolbar2000 License"; you may
  not use or distribute this file except in compliance with the
  "Toolbar2000 License". A copy of the "Toolbar2000 License" may be found in
  TB2k-LICENSE.txt or at:
    http://www.jrsoftware.org/files/tb2k/TB2k-LICENSE.txt

  Alternatively, the contents of this file may be used under the terms of the
  GNU General Public License (the "GPL"), in which case the provisions of the
  GPL are applicable instead of those in the "Toolbar2000 License". A copy of
  the GPL may be found in GPL-LICENSE.txt or at:
    http://www.jrsoftware.org/files/tb2k/GPL-LICENSE.txt
  If you wish to allow use of your version of this file only under the terms of
  the GPL and not to allow others to use your version of this file under the
  "Toolbar2000 License", indicate your decision by deleting the provisions
  above and replace them with the notice and other provisions required by the
  GPL. If you do not delete the provisions above, a recipient may use your
  version of this file under either the "Toolbar2000 License" or the GPL.

  $jrsoftware: tb2k/Source/TB2Toolbar.pas,v 1.126 2008/06/23 18:05:47 jr Exp $
}

interface

{$I TB2Ver.inc}

uses
  {$IFDEF JR_D9} Types, {$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ImgList,
  Menus, ActnList,
  TB2Item, TB2Dock;

type
  TTBCustomToolbar = class;
  TTBChevronItem = class;
  TTBChevronItemClass = class of TTBChevronItem;

  TTBToolbarViewClass = class of TTBToolbarView;
  TTBToolbarView = class(TTBView)
  private
    FToolbar: TTBCustomToolbar;
  protected
    procedure AutoSize(AWidth, AHeight: Integer); override;
    procedure DoUpdatePositions(var ASize: TPoint); override;
    function GetChevronItem: TTBCustomItem; override;
    function GetMDIButtonsItem: TTBCustomItem; override;
    function GetMDISystemMenuItem: TTBCustomItem; override;
  public
    constructor Create(AOwner: TComponent; AParentView: TTBView;
      AParentItem: TTBCustomItem; AWindow: TWinControl;
      AIsToolbar, ACustomizing, AUsePriorityList: Boolean); override;
    function GetFont: TFont; override;
    procedure InvalidatePositions; override;
  end;

  TTBChevronPriorityForNewItems = (tbcpHighest, tbcpLowest);

  TTBCustomToolbar = class(TTBCustomDockableWindow, ITBItems)
  private
    FBaseSize: TPoint;
    FChevronItem: TTBChevronItem;
    FChevronMoveItems: Boolean;
    FChevronPriorityForNewItems: TTBChevronPriorityForNewItems;
    FDisableAlignArrange: Integer;
    FFloatingWidth: Integer;
    FIgnoreMouseLeave: Boolean;
    FItem: TTBRootItem;
    FLastWrappedLines: Integer;
    FMenuBar: Boolean;
    FOnShortCut: TShortCutEvent;
    FProcessShortCuts: Boolean;
    FMainWindowHookInstalled: Boolean;
    FShrinkMode: TTBShrinkMode;
    FSizeData: TObject;
    FSystemFont: Boolean;
    FUpdateActions: Boolean;

    procedure CancelHover;
    function CalcChevronOffset(const ADock: TTBDock;
      const AOrientation: TTBViewOrientation): Integer;
    function CalcWrapOffset(const ADock: TTBDock): Integer;
    function CreateWrapper(Index: Integer; Ctl: TControl): TTBControlItem;
    function FindWrapper(Ctl: TControl): TTBControlItem;
    function GetChevronHint: String;
    function GetImages: TCustomImageList;
    function GetItems: TTBCustomItem;
    function GetLinkSubitems: TTBCustomItem;
    function GetOptions: TTBItemOptions;
    procedure InstallMainWindowHook;
    function IsChevronHintStored: Boolean;
    class function MainWindowHook(var Message: TMessage): Boolean; {$IFDEF CLR} static; {$ENDIF}
    procedure SetChevronHint(const Value: String);
    procedure SetChevronMoveItems(Value: Boolean);
    procedure SetChevronPriorityForNewItems(Value: TTBChevronPriorityForNewItems);
    procedure SetFloatingWidth(Value: Integer);
    procedure SetImages(Value: TCustomImageList);
    procedure SetLinkSubitems(Value: TTBCustomItem);
    procedure SetMainWindowHook;
    procedure SetMenuBar(Value: Boolean);
    procedure SetOptions(Value: TTBItemOptions);
    procedure SetProcessShortCuts(Value: Boolean);
    procedure SetShrinkMode(Value: TTBShrinkMode);
    procedure SetSystemFont(Value: Boolean);
    procedure UninstallMainWindowHook;
    procedure UpdateViewProperties;

    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    {$IFNDEF CLR}
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
    procedure CMControlListChange(var Message: TCMControlListChange); message CM_CONTROLLISTCHANGE;
    {$ENDIF}
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMShowHintChanged(var Message: TMessage); message CM_SHOWHINTCHANGED;
    procedure CMWinIniChange(var Message: TWMWinIniChange); message CM_WININICHANGE;
    procedure WMCancelMode(var Message: TWMCancelMode); message WM_CANCELMODE;
    procedure WMGetObject(var Message: TMessage); message WM_GETOBJECT;
    procedure WMMouseLeave(var Message: TMessage); message WM_MOUSELEAVE;
    procedure WMNCMouseMove(var Message: TWMNCMouseMove); message WM_NCMOUSEMOVE;
    {$IFNDEF JR_D5}
    procedure WMRButtonUp(var Message: TWMRButtonUp); message WM_RBUTTONUP;
    {$ENDIF}
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;
  protected
    FMDIButtonsItem: TTBCustomItem;
    FMDISystemMenuItem: TTBCustomItem;
    FView: TTBToolbarView;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure BuildPotentialSizesList(SizesList: TList); dynamic;
    {$IFDEF CLR}
    procedure ControlChange(Inserting: Boolean; Child: TControl); override;
    procedure ControlListChange(Inserting: Boolean; Child: TControl); override;
    {$ENDIF}
    procedure ControlExistsAtPos(const P: TPoint; var ControlExists: Boolean);
      override;
    function DoArrange(CanMoveControls: Boolean; PreviousDockType: TTBDockType;
      NewFloating: Boolean; NewDock: TTBDock): TPoint; override;
    {$IFDEF JR_D5}
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    {$ENDIF}
    procedure GetBaseSize(var ASize: TPoint); override;
    procedure GetMinBarSize(var MinimumSize: TPoint);
    procedure GetMinShrinkSize(var AMinimumSize: Integer); override;
    function GetShrinkMode: TTBShrinkMode; override;
    function GetChevronItemClass: TTBChevronItemClass; dynamic;
    function GetItemClass: TTBRootItemClass; dynamic;
    function GetViewClass: TTBToolbarViewClass; dynamic;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure ResizeBegin(ASizeHandle: TTBSizeHandle); override;
    procedure ResizeTrack(var Rect: TRect; const OrigRect: TRect); override;
    procedure ResizeTrackAccept; override;
    procedure ResizeEnd; override;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;

    property SystemFont: Boolean read FSystemFont write SetSystemFont default True;
    property OnShortCut: TShortCutEvent read FOnShortCut write FOnShortCut;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure CreateWrappersForAllControls;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure GetTabOrderList(List: TList); override;
    procedure InitiateAction; override;
    function IsShortCut(var Message: TWMKey): Boolean;
    function KeyboardOpen(Key: Char; RequirePrimaryAccel: Boolean): Boolean;
    procedure ReadPositionData(const Data: TTBReadPositionData); override;
    procedure WritePositionData(const Data: TTBWritePositionData); override;

    property ChevronHint: String read GetChevronHint write SetChevronHint stored IsChevronHintStored;
    property ChevronMoveItems: Boolean read FChevronMoveItems write SetChevronMoveItems default True;
    property ChevronPriorityForNewItems: TTBChevronPriorityForNewItems read FChevronPriorityForNewItems
      write SetChevronPriorityForNewItems default tbcpHighest;
    property FloatingWidth: Integer read FFloatingWidth write SetFloatingWidth default 0;
    property Images: TCustomImageList read GetImages write SetImages;
    property Items: TTBRootItem read FItem;
    property LinkSubitems: TTBCustomItem read GetLinkSubitems write SetLinkSubitems;
    property Options: TTBItemOptions read GetOptions write SetOptions default [];
    property MenuBar: Boolean read FMenuBar write SetMenuBar default False;
    property ProcessShortCuts: Boolean read FProcessShortCuts write SetProcessShortCuts default False;
    property ShrinkMode: TTBShrinkMode read FShrinkMode write SetShrinkMode default tbsmChevron;
    property UpdateActions: Boolean read FUpdateActions write FUpdateActions default True;
    property View: TTBToolbarView read FView;
  published
    property Hint stored False;  { Hint is set dynamically; don't save it }
  end;

  TTBToolbar = class(TTBCustomToolbar)
  published
    property ActivateParent;
    property Align;
    property Anchors;
    property AutoResize;
    property BorderStyle;
    property Caption;
    property ChevronHint;
    property ChevronMoveItems;
    property ChevronPriorityForNewItems;
    property CloseButton;
    property CloseButtonWhenDocked;
    property Color;
    property CurrentDock;
    property DefaultDock;
    property DockableTo;
    property DockMode;
    property DockPos;
    property DockRow;
    property DragHandleStyle;
    property FloatingMode;
    property FloatingWidth;
    property Font;
    property FullSize;
    property HideWhenInactive;
    property Images;
    property Items;
    property LastDock;
    property LinkSubitems;
    property MenuBar;
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
    property UpdateActions;
    property UseLastDock;
    property Visible;

    property OnClose;
    property OnCloseQuery;
    {$IFDEF JR_D5}
    property OnContextPopup;
    {$ENDIF}
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
  end;

{ TTBChevronItem & TTBChevronItemViewer }

  TTBChevronItem = class(TTBCustomItem)
  private
    FToolbar: TTBCustomToolbar;
    function GetDefaultHint: String;
  protected
    function GetChevronParentView: TTBView; override;
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TTBChevronItemViewer = class(TTBItemViewer)
  protected
    procedure Paint(const Canvas: TCanvas; const ClientAreaRect: TRect;
      IsSelected, IsPushed, UseDisabledShadow: Boolean); override;
  end;

Var
  tbChevronSize: integer = 12;


implementation

uses
  {$IFDEF CLR} System.Runtime.InteropServices, System.Text, {$ENDIF}
  TB2Consts, TB2Common, TB2Hook;

{$IFDEF WIN64}
type
  TSmallPoint = TPoint;

function SmallPointToPoint(const P: TPoint): TPoint;
begin
  Result.X := P.X;
  Result.Y := P.Y;
end;

function PointToSmallPoint(const P: TPoint): TPoint;
begin
  Result.X := P.X;
  Result.Y := P.Y;
end;
{$ENDIF WIN64}


const
  { Constants for TTBCustomToolbar-specific registry values. Do not localize! }
  rvFloatRightX = 'FloatRightX';
  DockTypeToOrientation: array[TTBDockType] of TTBViewOrientation =
    (tbvoHorizontal, tbvoFloating, tbvoHorizontal, tbvoVertical);

type
  { Used internally by the TTBCustomToolbar.Resize* methods }
  TToolbarSizeData = class
  public
    SizeHandle: TTBSizeHandle;
    NewSizes: TList;  { List of valid new sizes. Items are casted into TSmallPoints }
    OrigWidth, OrigHeight, NCXDiff: Integer;
    CurRightX: Integer;
    DisableSensCheck, OpSide: Boolean;
    DistanceToSmallerSize, DistanceToLargerSize: Integer;
  end;


procedure HookProc(Code: THookProcCode; Wnd: HWND; WParam: WPARAM;
  LParam: LPARAM);
var
  Msg: {$IFNDEF CLR} PMsg {$ELSE} TMsg {$ENDIF};
  MainForm: TForm;
begin
  { Work around an annoying Windows or VCL bug. If you close all MDI child
    windows, the MDI client window gets the focus, and when it has the focus,
    pressing Alt+[char] doesn't send a WM_SYSCOMMAND message to the form for
    some reason. It seems silly to have to use a hook for this, but I don't
    see a better workaround.
    Also, route Alt+- to the main form so that when an MDI child form is
    maximized, Alt+- brings up the TB2k MDI system menu instead of the
    system's. }
  if Code = hpGetMessage then begin
    {$IFNDEF CLR}
    Msg := PMsg(LParam);
    {$ELSE}
    Msg := TMsg(Marshal.PtrToStructure(IntPtr(LParam), TypeOf(TMsg)));
    {$ENDIF}
    if (Msg.message = WM_SYSCHAR) and (Msg.hwnd <> 0) then begin
      { Note: On Windows NT/2000/XP, even though we install the hook using
        SetWindowsHookExW, Msg.wParam may either be an ANSI character or a
        Unicode character, due to an apparent bug on these platforms. It is
        an ANSI character when the message passes through a separate
        SetWindowsHookExA-installed WH_GETMESSAGE hook first, and that hook
        calls us via CallNextHookEx. Windows apparently "forgets" to convert
        the character from ANSI back to Unicode in this case.
        We can't convert the character code because there seems to be no way
        to detect whether it is ANSI or Unicode. So we can't really do much
        with Msg.wParam, apart from comparing it against character codes that
        are the same between ANSI and Unicode, such as '-'. }
      MainForm := Application.MainForm;
      if Assigned(MainForm) and MainForm.HandleAllocated and (GetCapture = 0) and
         ((Msg.hwnd = MainForm.ClientHandle) or
          ((Msg.wParam = Ord('-')) and (MainForm.ClientHandle <> 0) and
           IsChild(MainForm.ClientHandle, Msg.hwnd))) then begin
        { Redirect the message to the main form.
          Note: Unfortunately, due to a bug in Windows NT 4.0 (and not
          2000/XP/9x/Me), modifications to the message don't take effect if
          another WH_GETMESSAGE hook has been installed above this one.
          (The bug is that CallNextHookEx copies lParam^ to a local buffer, but
          does not propogate the changes made by the hook back to lParam^ when
          it returns.) I don't know of any clean workaround, other than to
          ensure other WH_GETMESSAGE hooks are installed *before*
          Toolbar2000's. }
        Msg.hwnd := MainForm.Handle;
        {$IFDEF CLR}
        Marshal.StructureToPtr(TObject(Msg), IntPtr(LParam), False);
        {$ENDIF}
      end;
    end;
  end;
end;

constructor TTBChevronItem.Create(AOwner: TComponent);
begin
  inherited;
  FToolbar := AOwner as TTBCustomToolbar;
  ItemStyle := ItemStyle + [tbisSubMenu, tbisNoAutoOpen];
  Hint := GetDefaultHint;
  Caption := EscapeAmpersands(GetShortHint(Hint));
end;

function TTBChevronItem.GetChevronParentView: TTBView;
begin
  Result := FToolbar.FView;
end;

function TTBChevronItem.GetDefaultHint: String;
begin
  Result := STBChevronItemMoreButtonsHint;
end;

function TTBChevronItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result := TTBChevronItemViewer;
end;

procedure TTBChevronItemViewer.Paint(const Canvas: TCanvas;
  const ClientAreaRect: TRect; IsSelected, IsPushed, UseDisabledShadow: Boolean);
const
  HorzPattern: array[0..15] of Byte =
    ($CC, 0, $66, 0, $33, 0, $66, 0, $CC, 0, 0, 0, 0, 0, 0, 0);
  VertPattern: array[0..15] of Byte =
    ($88, 0, $D8, 0, $70, 0, $20, 0, $88, 0, $D8, 0, $70, 0, $20, 0);
var
  DC: HDC;
  R2: TRect;
  TempBmp: TBitmap;

  procedure DrawPattern(const Color, Offset: Integer);
  begin
    SelectObject(DC, GetSysColorBrush(Color));
    BitBlt(DC, R2.Left, R2.Top + Offset, R2.Right - R2.Left,
      R2.Bottom - R2.Top, TempBmp.Canvas.Handle, 0, 0, $00E20746 {ROP_DSPDxax});
  end;

begin
  DC := Canvas.Handle;
  R2 := ClientAreaRect;
  if Item.Enabled then begin
    if IsPushed then
      DrawEdge(DC, R2, BDR_SUNKENOUTER, BF_RECT)
    else if IsSelected and not(csDesigning in Item.ComponentState) then
      DrawEdge(DC, R2, BDR_RAISEDINNER, BF_RECT);
  end;

  if View.Orientation <> tbvoVertical then begin
    R2.Top := 4;
    R2.Bottom := R2.Top + 5;
    Inc(R2.Left, 2);
    R2.Right := R2.Left + 8;
  end
  else begin
    R2.Left := R2.Right - 9;
    R2.Right := R2.Left + 5;
    Inc(R2.Top, 2);
    R2.Bottom := R2.Top + 8;
  end;
  if IsPushed then
    OffsetRect(R2, 1, 1);
  TempBmp := TBitmap.Create;
  try
    if View.Orientation = tbvoVertical then
      TempBmp.Handle := CreateMonoBitmap(8, 8, VertPattern)
    else
      TempBmp.Handle := CreateMonoBitmap(8, 8, HorzPattern);
    SetTextColor(DC, clBlack);
    SetBkColor(DC, clWhite);
    if Item.Enabled then
      DrawPattern(COLOR_BTNTEXT, 0)
    else begin
      DrawPattern(COLOR_BTNHIGHLIGHT, 1);
      DrawPattern(COLOR_BTNSHADOW, 0);
    end;
  finally
    TempBmp.Free;
  end;
end;


{ TTBToolbarView }

constructor TTBToolbarView.Create(AOwner: TComponent; AParentView: TTBView;
  AParentItem: TTBCustomItem; AWindow: TWinControl;
  AIsToolbar, ACustomizing, AUsePriorityList: Boolean);
begin
  FToolbar := AOwner as TTBCustomToolbar;
  inherited;
end;

procedure TTBToolbarView.AutoSize(AWidth, AHeight: Integer);
begin
  FToolbar.FBaseSize := BaseSize;
  if FToolbar.IsAutoResized then
    FToolbar.ChangeSize(AWidth, AHeight);
end;

procedure TTBToolbarView.DoUpdatePositions(var ASize: TPoint);
begin
  { Reset CurrentSize because it probably won't be valid after positions
    are recalculated [2001-06-24] }
  FToolbar.CurrentSize := 0;
  FToolbar.GetMinBarSize(ASize);
  { On FullSize toolbars, increase ASize.X/Y to WrapOffset so that
    right-aligned items always appear at the right edge even when the toolbar
    isn't wrapping }
  if FToolbar.FullSize then begin
    if (Orientation = tbvoHorizontal) and (ASize.X < WrapOffset) then
      ASize.X := WrapOffset
    else if (Orientation = tbvoVertical) and (ASize.Y < WrapOffset) then
      ASize.Y := WrapOffset;
  end;
  { Increment FDisableAlignArrange so that we don't recursively arrange when
    CalculatePositions moves controls }
  Inc(FToolbar.FDisableAlignArrange);
  try
    inherited;
  finally
    Dec(FToolbar.FDisableAlignArrange);
  end;
end;

procedure TTBToolbarView.InvalidatePositions;
begin
  { Reset CurrentSize because it probably won't be valid after positions
    are recalculated [2001-06-04] }
  FToolbar.CurrentSize := 0;
  inherited;
end;

function TTBToolbarView.GetFont: TFont;
begin
  if not FToolbar.SystemFont then
    Result := FToolbar.Font
  else
    Result := inherited GetFont;
end;

function TTBToolbarView.GetChevronItem: TTBCustomItem;
begin
  Result := FToolbar.FChevronItem;
end;

function TTBToolbarView.GetMDIButtonsItem: TTBCustomItem;
begin
  Result := FToolbar.FMDIButtonsItem;
end;

function TTBToolbarView.GetMDISystemMenuItem: TTBCustomItem;
begin
  Result := FToolbar.FMDISystemMenuItem;
end;


{ TTBCustomToolbar }

type
  {}TTBCustomItemAccess = class(TTBCustomItem);
  TTBItemViewerAccess = class(TTBItemViewer);

constructor TTBCustomToolbar.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls, csActionClient] -
    [csCaptureMouse];
  DockableWindowStyles := DockableWindowStyles - [tbdsResizeEightCorner,
    tbdsResizeClipCursor];
  FItem := GetItemClass.Create(Self);
  FItem.ParentComponent := Self;
  FChevronItem := GetChevronItemClass.Create(Self);
  FChevronItem.LinkSubitems := FItem;
  FChevronMoveItems := True;
  FView := GetViewClass.Create(Self, nil, FItem, Self, True, False,
    not(csDesigning in ComponentState));
  FView.BackgroundColor := clBtnFace;
  FUpdateActions := True;
  FShrinkMode := tbsmChevron;
  FSystemFont := True;
  Color := clBtnFace;
  SetBounds(Left, Top, 23, 22);{}
end;

destructor TTBCustomToolbar.Destroy;
begin
  { Call Destroying to ensure csDestroying is in ComponentState now. Only
    needed for Delphi 4 and earlier since Delphi 5 calls Destroying in
    TComponent.BeforeDestruction }
  Destroying;
  UninstallHookProc(Self, HookProc);
  UninstallMainWindowHook;
  FreeAndNil(FView);
  FreeAndNil(FChevronItem);
  FreeAndNil(FItem);
  inherited;
end;

function TTBCustomToolbar.GetItems: TTBCustomItem;
begin
  Result := FItem;
end;

function TTBCustomToolbar.GetItemClass: TTBRootItemClass;
begin
  Result := TTBRootItem;
end;

function TTBCustomToolbar.GetViewClass: TTBToolbarViewClass;
begin
  Result := TTBToolbarView;
end;

function TTBCustomToolbar.GetChevronItemClass: TTBChevronItemClass;
begin
  Result := TTBChevronItem;
end;

procedure TTBCustomToolbar.CreateWrappersForAllControls;
{ Create wrappers for any controls that don't already have them }
var
  L: TList;
  I, J, C: Integer;
begin
  if ControlCount = 0 then
    Exit;
  L := TList.Create;
  try
    L.Capacity := ControlCount;
    for I := 0 to ControlCount-1 do
      L.Add(Controls[I]);
    C := FItem.Count-1;
    for I := 0 to C do
      if FItem[I] is TTBControlItem then begin
        J := L.IndexOf(TTBControlItem(FItem[I]).Control);
        if J <> -1 then
          L[J] := nil;
      end;
    for I := 0 to L.Count-1 do
      if Assigned(L[I]) then
        CreateWrapper(FItem.Count, TControl(L[I]));
  finally
    L.Free;
  end;
end;

procedure TTBCustomToolbar.Loaded;
begin
  CreateWrappersForAllControls;
  inherited;
end;

procedure TTBCustomToolbar.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  TTBCustomItemAccess(TTBCustomItem(FItem)).GetChildren(Proc, Root);
  inherited;
end;

procedure TTBCustomToolbar.SetChildOrder(Child: TComponent; Order: Integer);
begin
  if Child is TTBCustomItem then
    TTBCustomItemAccess(TTBCustomItem(FItem)).SetChildOrder(Child, Order);
end;

procedure TTBCustomToolbar.AlignControls(AControl: TControl; var Rect: TRect);
{ VCL calls this whenever any child controls in the toolbar are moved, sized,
  inserted, etc., and also when the toolbar is resized. }
begin
  if FDisableAlignArrange = 0 then
    Arrange;
end;

procedure TTBCustomToolbar.InitiateAction;
begin
  inherited;
  {}{ also add this to popupmenu(?) }
  { Update visible top-level items }
  if FUpdateActions then
    FView.InitiateActions;
end;

procedure TTBCustomToolbar.CMColorChanged(var Message: TMessage);
begin
  { Synchronize FView.BackgroundColor with the new color }
  if Assigned(FView) then
    FView.BackgroundColor := Color;
  inherited;
end;

function TTBCustomToolbar.CreateWrapper(Index: Integer;
  Ctl: TControl): TTBControlItem;
var
  I: Integer;
  S: String;
begin
  Result := TTBControlItem.Create(Owner);
  Result.Control := Ctl;
  if (csDesigning in ComponentState) and Assigned(Owner) then begin
    { Needs a name for compatibility with form inheritance } 
    I := 1;
    while True do begin
      S := Format('TBControlItem%d', [I]);
      if Owner.FindComponent(S) = nil then
        Break;
      Inc(I);
    end;
    Result.Name := S;
  end;
  FItem.Insert(Index, Result);
end;

function TTBCustomToolbar.FindWrapper(Ctl: TControl): TTBControlItem;
var
  I: Integer;
  Item: TTBCustomItem;
begin
  Result := nil;
  for I := 0 to FItem.Count-1 do begin
    Item := FItem[I];
    if (Item is TTBControlItem) and
       (TTBControlItem(Item).Control = Ctl) then begin
      Result := TTBControlItem(Item);
      Break;
    end;
  end;
end;

{$IFNDEF CLR}
procedure TTBCustomToolbar.CMControlChange(var Message: TCMControlChange);
{$ELSE}
procedure TTBCustomToolbar.ControlChange(Inserting: Boolean; Child: TControl);
{$ENDIF}
{ A CM_CONTROLCHANGE handler must be used instead of a CM_CONTROLLISTCHANGE
  handler because when a CM_CONTROLLISTCHANGE message is sent it is relayed to
  *all* parents. CM_CONTROLCHANGE messages are only sent to the immediate
  parent. }

  procedure HandleControlChange(Inserting: Boolean; Child: TControl);
  begin
    { Don't automatically create TTBControlItem wrappers if the component
      is loading or being updated to reflect changes in an ancestor form,
      because wrappers will be streamed in }
    if Inserting and not(csLoading in ComponentState) and
       not(csUpdating in ComponentState) and
       (FindWrapper(Child) = nil) then
      CreateWrapper(FItem.Count, Child);
  end;

begin
  inherited;
  {$IFNDEF CLR}
  HandleControlChange(Message.Inserting, Message.Control);
  {$ELSE}
  HandleControlChange(Inserting, Child);
  {$ENDIF}
end;

{$IFNDEF CLR}
procedure TTBCustomToolbar.CMControlListChange(var Message: TCMControlListChange);
{$ELSE}
procedure TTBCustomToolbar.ControlListChange(Inserting: Boolean; Child: TControl);
{$ENDIF}
{ Don't handle deletions inside CM_CONTROLCHANGE because CM_CONTROLCHANGE is
  sent *before* a control begins removing itself from its parent. (It used
  to handle both insertions and deletions inside CM_CONTROLCHANGE but this
  caused AV's.) }

  procedure HandleControlListChange(Inserting: Boolean; Child: TControl);
  var
    Item: TTBControlItem;
  begin
    if not Inserting and Assigned(FItem) then begin
      while True do begin
        Item := FindWrapper(Child);
        if Item = nil then Break;
        { The control is being removed the control, not necessarily destroyed,
          so set DontFreeControl to True }
        Item.DontFreeControl := True;
        Item.Free;
      end;
    end;
  end;

begin
  inherited;
  {$IFNDEF CLR}
  HandleControlListChange(Message.Inserting, Message.Control);
  {$ELSE}
  HandleControlListChange(Inserting, Child);
  {$ENDIF}
end;

procedure TTBCustomToolbar.CMHintShow(var Message: TCMHintShow);
{ Since the items on a toolbar aren't "real" controls, it needs a CM_HINTSHOW
  handler for their hints to be displayed. }
begin
  FView.HandleHintShowMessage(Message);
end;

procedure TTBCustomToolbar.CMShowHintChanged(var Message: TMessage);
begin
  inherited;
  if ShowHint then
    FView.Style := FView.Style + [vsAlwaysShowHints]
  else
    FView.Style := FView.Style - [vsAlwaysShowHints];
end;

procedure TTBCustomToolbar.WMGetObject(var Message: TMessage);
begin
  if not FView.HandleWMGetObject(Message) then
    inherited;
end;

procedure TTBCustomToolbar.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
  Viewer: TTBItemViewer;
  Cursor: HCURSOR;
begin
  if not(csDesigning in ComponentState) and
     (Message.CursorWnd = WindowHandle) and
     (Smallint(Message.HitTest) = HTCLIENT) then begin
    { Note: This should not change the selection, because we can receive this
      message during a modal loop if a user sets "Screen.Cursor := crDefault"
      inside a submenu's OnClick handler (which really isn't recommended, as
      it won't necessarily restore the cursor we set originally). }
    GetCursorPos(P);
    P := ScreenToClient(P);
    Viewer := FView.ViewerFromPoint(P);
    if Assigned(Viewer) then begin
      Cursor := 0;
      Dec(P.X, Viewer.BoundsRect.Left);
      Dec(P.Y, Viewer.BoundsRect.Top);
      TTBItemViewerAccess(Viewer).GetCursor(P, Cursor);
      if Cursor <> 0 then begin
        SetCursor(Cursor);
        Message.Result := 1;
        Exit;
      end;
    end;
  end;
  inherited;
end;

procedure TTBCustomToolbar.WMSysCommand(var Message: TWMSysCommand);
var
  ConvertedKey: Char;
begin
  if FMenuBar and CanFocus then
    with Message do
      if (CmdType and $FFF0 = SC_KEYMENU) and (Key <> VK_SPACE) and
         (GetCapture = 0) then begin
        {$IFNDEF CLR}
        {$IFDEF JR_WIDESTR}
        { Under Unicode Win32 VCL, no conversion required }
        WideChar(ConvertedKey) := WideChar(Key);
        {$ELSE}
        if Win32Platform = VER_PLATFORM_WIN32_NT then begin
          { On Windows NT 4/2000/XP, Key is a wide character, so we have to
            convert it. Pressing Alt+N in a Russian input locale, for example,
            results in a Key value of $0442.
            This could perhaps be considered a bug in Windows NT since the
            character codes in other messages such as WM_SYSCHAR aren't left
            in Unicode form.
            The conversion isn't done with the system code page, but rather
            with the code page of the currently active input locale, like
            Windows does when sending WM_(SYS)CHAR messages. }
          if WideCharToMultiByte(GetInputLocaleCodePage, 0, @WideChar(Key), 1,
             @AnsiChar(ConvertedKey), 1, nil, nil) <> 1 then
            Exit;  { shouldn't fail, but if it does, we can't continue }
        end
        else begin
          { On Windows 95/98/Me, Key is not a wide character. }
          AnsiChar(ConvertedKey) := AnsiChar(Key);
        end;
        {$ENDIF}
        {$ELSE}
        if Marshal.SystemDefaultCharSize = 2 then begin
          { Strings are Unicode on .NET, so no need to downconvert to ANSI }
          ConvertedKey := WideChar(Key);
        end
        else begin
          { On Windows 98/Me, we have to convert ANSI->Unicode, using the
            code page of the currently active input locale }
          ConvertedKey := Encoding.GetEncoding(GetInputLocaleCodePage).
            GetChars([Byte(Key)])[0];
        end;
        {$ENDIF}
        if not KeyboardOpen(ConvertedKey, False) then begin
          if Key = Ord('-') then Exit;
          MessageBeep(0);
        end;
        Result := 1;
      end;
end;

procedure TTBCustomToolbar.Paint;
var
  R: TRect;
begin
  { Draw dotted border in design mode on undocked toolbars }
  if not Docked and (csDesigning in ComponentState) then
    with Canvas do begin
      R := ClientRect;
      Pen.Style := psDot;
      Pen.Color := clBtnShadow;
      Brush.Style := bsClear;
      Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      Pen.Style := psSolid;
    end;
  FView.DrawSubitems(Canvas);
end;

procedure TTBCustomToolbar.CMDialogKey(var Message: TCMDialogKey);
begin
  if not(csDesigning in ComponentState) and
     (Message.CharCode = VK_MENU) and FMenuBar and CanFocus then
    FView.SetAccelsVisibility(True);
  inherited;
end;

procedure TTBCustomToolbar.CMDialogChar(var Message: TCMDialogChar);
begin
  { On toolbars that aren't menu bars, handle CM_DIALOGCHAR instead of
    WM_SYSCOMMAND.
    Note: We have to check for csDesigning because on Delphi 2005/2006 we get
    CM_DIALOG* messages if Alt+[key] is pressed while a form with a toolbar is
    open in the embedded designer, and a tab other than Design is currently
    selected (e.g., Code). }
  if not(csDesigning in ComponentState) and
     not FMenuBar and CanFocus and (Message.CharCode <> 0) then
    if KeyboardOpen(Chr(Message.CharCode), True) then begin
      Message.Result := 1;
      Exit;
    end;
  inherited;
end;

procedure TTBCustomToolbar.CancelHover;
begin
  if not MouseCapture then
    FView.UpdateSelection(Point(Low(Integer), Low(Integer)), True);
end;

procedure TTBCustomToolbar.CMMouseLeave(var Message: TMessage);
begin
  CancelHover;
  inherited;
end;

{$IFDEF JR_D5}
procedure TTBCustomToolbar.DoContextPopup(MousePos: TPoint;
  var Handled: Boolean);
begin
  CancelHover;
  inherited;
end;
{$ENDIF}

{$IFNDEF JR_D5}
{ Delphi 4 and earlier don't have a DoContextPopup method; we instead have to
  trap WM_RBUTTONUP to determine if a popup menu (might) be displayed }
procedure TTBCustomToolbar.WMRButtonUp(var Message: TWMRButtonUp);
begin
  CancelHover;
  inherited;
end;
{$ENDIF}

procedure TTBCustomToolbar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  Item: TTBCustomItem;
begin
  if not(csDesigning in ComponentState) then begin
    P := ClientToScreen(Point(X, Y));
    FView.UpdateSelection(P, True);
    if Assigned(FView.Selected) then begin
      Item := FView.Selected.Item;
      if not(tboLongHintInMenuOnly in Item.EffectiveOptions) then
        Hint := Item.Hint
      else
        Hint := '';
    end
    else
      Hint := '';
  end;
  { Call TrackMouseEvent to be sure that we are notified when the mouse leaves
    the window. We won't get a CM_MOUSELEAVE message if the mouse moves
    directly from the toolbar to another application's window }
  CallTrackMouseEvent(Handle, TME_LEAVE);
  inherited;
end;

procedure TTBCustomToolbar.WMCancelMode(var Message: TWMCancelMode);
begin
  inherited;
  { We can receive a WM_CANCELMODE message during a modal loop if a dialog
    pops up. Respond by hiding menus to make it look like the modal loop
    has returned, even though it really hasn't yet.
    Note: Similar code in TTBModalHandler.WndProc. }
  if vsModal in FView.State then
    FView.CancelMode;
end;

procedure TTBCustomToolbar.WMMouseLeave(var Message: TMessage);
begin
  { A WM_MOUSELEAVE handler is necessary because the control won't get a
    CM_MOUSELEAVE message if the user presses Alt+Space. Also, CM_MOUSELEAVE
    messages are also not sent if the application is in a
    Application.ProcessMessages loop. }
  if not FIgnoreMouseLeave then
    CancelHover;
  inherited;
end;

procedure TTBCustomToolbar.WMNCMouseMove(var Message: TWMNCMouseMove);
begin
  Hint := '';
  CancelHover;
  inherited;
end;

function TTBCustomToolbar.KeyboardOpen(Key: Char;
  RequirePrimaryAccel: Boolean): Boolean;
var
  I: TTBItemViewer;
  IsOnlyItemWithAccel: Boolean;
begin
  Result := False;
  { Sanity check: Bail out early if re-entered }
  if vsModal in FView.State then
    Exit;
  I := nil;
  FView.SetAccelsVisibility(True);
  try
    if Key = #0 then begin
      I := FView.FirstSelectable;
      if I = nil then Exit;
      FView.Selected := I;
      FView.EnterToolbarLoop([]);
    end
    else begin
      I := FView.NextSelectableWithAccel(nil, Key, RequirePrimaryAccel,
        IsOnlyItemWithAccel);
      if (I = nil) or not I.Item.Enabled then
        Exit;
      if IsOnlyItemWithAccel then begin
        FView.Selected := I;
        FView.EnterToolbarLoop([tbetExecuteSelected]);
      end
      else if FMenuBar then begin
        FView.Selected := I;
        FView.EnterToolbarLoop([]);
      end
      else
        Exit;
    end;
    Result := True;
  finally
    if Assigned(I) then
      FView.SetAccelsVisibility(False);
  end;
end;

procedure TTBCustomToolbar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  OldParent: TWinControl;
  P: TPoint;
  Item: TTBCustomItem;
begin
  OldParent := Parent;
  inherited;
  if Parent <> OldParent then
    { if the inherited handler (TTBDockableWindow.MouseDown) changed the Parent
      (due to the toolbar moving between docks), nothing else should be done }
    Exit;
  if not(csDesigning in ComponentState) and (Button = mbLeft) then begin
    P := ClientToScreen(Point(X, Y));
    FView.UpdateSelection(P, True);
    if Assigned(FView.Selected) then begin
      Item := FView.Selected.Item;
      if not(tbisClicksTransparent in TTBCustomItemAccess(Item).ItemStyle) then begin
        FIgnoreMouseLeave := True;
        try
          FView.EnterToolbarLoop([tbetMouseDown]);
        finally
          FIgnoreMouseLeave := False;
        end;
      end;
    end;
  end;
end;

procedure TTBCustomToolbar.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if not FSystemFont then
    Arrange;
end;

function TTBCustomToolbar.GetChevronHint: String;
begin
  Result := FChevronItem.Hint;
end;

procedure TTBCustomToolbar.SetChevronHint(const Value: String);
begin
  FChevronItem.Hint := Value;
  FChevronItem.Caption := EscapeAmpersands(GetShortHint(Value));
end;

procedure TTBCustomToolbar.SetChevronMoveItems(Value: Boolean);
begin
  if FChevronMoveItems <> Value then begin
    FChevronMoveItems := Value;
    FView.UsePriorityList := Value and not(csDesigning in ComponentState);
  end;
end;

procedure TTBCustomToolbar.SetChevronPriorityForNewItems(Value: TTBChevronPriorityForNewItems);
begin
  FChevronPriorityForNewItems := Value;
  FView.NewViewersGetHighestPriority := (Value = tbcpHighest);
end;

function TTBCustomToolbar.IsChevronHintStored: Boolean;
begin
  Result := (FChevronItem.Hint <> FChevronItem.GetDefaultHint);
end;

function TTBCustomToolbar.GetImages: TCustomImageList;
begin
  Result := FItem.SubMenuImages;
end;

procedure TTBCustomToolbar.SetImages(Value: TCustomImageList);
begin
  FItem.SubMenuImages := Value;
end;

function TTBCustomToolbar.GetLinkSubitems: TTBCustomItem;
begin
  Result := FItem.LinkSubitems;
end;

procedure TTBCustomToolbar.SetLinkSubitems(Value: TTBCustomItem);
begin
  FItem.LinkSubitems := Value;
end;

procedure TTBCustomToolbar.SetMenuBar(Value: Boolean);
begin
  if FMenuBar <> Value then begin
    FMenuBar := Value;
    if Value then begin
      ControlStyle := ControlStyle + [csMenuEvents];
      FView.Style := FView.Style + [vsMenuBar, vsUseHiddenAccels];
    end
    else begin
      ControlStyle := ControlStyle - [csMenuEvents];
      FView.Style := FView.Style - [vsMenuBar, vsUseHiddenAccels];
    end;
    if not(csLoading in ComponentState) then begin
      FullSize := Value;
      if Value then
        ShrinkMode := tbsmWrap
      else
        ShrinkMode := tbsmChevron;
      CloseButton := not Value;
      ProcessShortCuts := Value;
    end;
    if Value and not(csDesigning in ComponentState) then
      InstallHookProc(Self, HookProc, [hpGetMessage])
    else
      UninstallHookProc(Self, HookProc);
    SetMainWindowHook;
  end;
end;

function TTBCustomToolbar.GetOptions: TTBItemOptions;
begin
  Result := FItem.Options;
end;

procedure TTBCustomToolbar.SetOptions(Value: TTBItemOptions);
begin
  FItem.Options := Value;
end;

procedure TTBCustomToolbar.SetProcessShortCuts(Value: Boolean);
begin
  if FProcessShortCuts <> Value then begin
    FProcessShortCuts := Value;
    SetMainWindowHook;
  end;
end;

procedure TTBCustomToolbar.SetSystemFont(Value: Boolean);
begin
  if FSystemFont <> Value then begin
    FSystemFont := Value;
    Arrange;
  end;
end;

procedure TTBCustomToolbar.SetShrinkMode(Value: TTBShrinkMode);
begin
  if FShrinkMode <> Value then begin
    FShrinkMode := Value;
    if Docked then
      CurrentDock.ArrangeToolbars
    else if not Floating then
      Arrange;
  end;
end;

procedure TTBCustomToolbar.SetFloatingWidth(Value: Integer);
begin
  if FFloatingWidth <> Value then begin
    FFloatingWidth := Value;
    if Floating then begin
      UpdateViewProperties;
      Arrange;
    end;
  end;
end;

function TTBCustomToolbar.CalcWrapOffset(const ADock: TTBDock): Integer;
begin
  if ADock = nil then
    Result := FFloatingWidth
  else begin
    if FShrinkMode = tbsmWrap then begin
      if not(ADock.Position in [dpLeft, dpRight]) then
        Result := ADock.Width - ADock.NonClientWidth - NonClientWidth
      else
        Result := ADock.Height - ADock.NonClientHeight - NonClientHeight;
    end
    else
      Result := 0;
  end;
end;

function TTBCustomToolbar.CalcChevronOffset(const ADock: TTBDock;
  const AOrientation: TTBViewOrientation): Integer;
begin
  if (FShrinkMode = tbsmChevron) and Docked and (ADock = CurrentDock) then begin
    Result := CurrentSize;
    { Subtract non-client size }
    if AOrientation <> tbvoVertical then
      Dec(Result, NonClientWidth)
    else
      Dec(Result, NonClientHeight);
    if Result < 0 then
      Result := 0;  { in case CurrentSize wasn't properly initialized yet }
  end
  else
    Result := 0;
end;

procedure TTBCustomToolbar.UpdateViewProperties;
var
  DT: TTBDockType;
begin
  DT := TBGetDockTypeOf(CurrentDock, Floating);
  FView.Orientation := DockTypeToOrientation[DT];
  FView.ChevronSize := tbChevronSize;
  if Assigned(CurrentDock) or Floating then begin
    FView.ChevronOffset := CalcChevronOffset(CurrentDock, FView.Orientation);
    FView.WrapOffset := CalcWrapOffset(CurrentDock);
  end
  else begin
    FView.ChevronOffset := 0;
    FView.WrapOffset := 0;
    { Only enable chevron/wrapping when the width of the toolbar is fixed }
    if not AutoResize or ((akLeft in Anchors) and (akRight in Anchors)) then begin
      if FShrinkMode = tbsmChevron then
        FView.ChevronOffset := Width - NonClientWidth
      else if FShrinkMode = tbsmWrap then
        FView.WrapOffset := Width - NonClientWidth;
    end;
  end;
end;

{}{DOCKING STUFF}

procedure TTBCustomToolbar.ReadPositionData(const Data: TTBReadPositionData);
begin
  inherited;
  with Data do
    FloatingWidth := ReadIntProc(Name, rvFloatRightX, 0, ExtraData);
end;

procedure TTBCustomToolbar.WritePositionData(const Data: TTBWritePositionData);
begin
  inherited;
  with Data do
    WriteIntProc(Name, rvFloatRightX, FFloatingWidth, ExtraData);
end;

procedure TTBCustomToolbar.GetMinBarSize(var MinimumSize: TPoint);
var
  WH: Integer;
begin
  MinimumSize.X := 0;
  MinimumSize.Y := 0;
  if Docked then begin
    WH := CurrentDock.GetMinRowSize(EffectiveDockRow, Self);
    if not(CurrentDock.Position in [dpLeft, dpRight]) then
      MinimumSize.Y := WH
    else
      MinimumSize.X := WH;
  end;
end;

procedure TTBCustomToolbar.GetBaseSize(var ASize: TPoint);
begin
  FView.ValidatePositions;
  ASize := FBaseSize;
end;

function TTBCustomToolbar.DoArrange(CanMoveControls: Boolean;
  PreviousDockType: TTBDockType; NewFloating: Boolean; NewDock: TTBDock): TPoint;
var
  DT: TTBDockType;
  O: TTBViewOrientation;
  TempBaseSize: TPoint;
begin
  //outputdebugstring (pchar(format('%s.DoArrange(%d)', [Name, Ord(CanMoveControls)])));
  if CanMoveControls then begin
    UpdateViewProperties;
    Result := FView.UpdatePositions;
  end
  else begin
    DT := TBGetDockTypeOf(NewDock, NewFloating);
    O := DockTypeToOrientation[DT];
    Result.X := 0;
    Result.Y := 0;
    FView.CalculatePositions(False, O, CalcWrapOffset(NewDock),
      CalcChevronOffset(NewDock, O), tbChevronSize, TempBaseSize, Result,
      FLastWrappedLines);
  end;
end;

procedure TTBCustomToolbar.ControlExistsAtPos(const P: TPoint;
  var ControlExists: Boolean);
var
  P2: TPoint;
begin
  inherited;
  if not ControlExists and not(csDesigning in ComponentState) then begin
    P2 := ClientToScreen(P);
    FView.UpdateSelection(P2, True);
    if Assigned(FView.Selected) and
       not(tbisClicksTransparent in TTBCustomItemAccess(FView.Selected.Item).ItemStyle) then
      ControlExists := True;
  end;
end;

procedure TTBCustomToolbar.BuildPotentialSizesList(SizesList: TList);
var
  Margins: TRect;
  MinX, SaveWrapX: Integer;
  X, PrevWrappedLines: Integer;
  S: TPoint;
  S2: TSmallPoint;
begin
  View.GetMargins(tbvoFloating, Margins);
  MinX := Margins.Left + Margins.Right;
  SaveWrapX := FFloatingWidth;
  try
    { Add the widest size to the list }
    FFloatingWidth := 0;
    S := DoArrange(False, dtNotDocked, True, nil);
    SizesList.Add(TListItemType(PointToSmallPoint(S)));
    { Calculate and add rest of sizes to the list }
    PrevWrappedLines := 1;
    X := S.X-1;
    while X >= MinX do begin
      FFloatingWidth := X;
      S := DoArrange(False, dtNotDocked, True, nil);
      if S.X > X then  { if it refuses to go any smaller }
        Break
      else
      if X = S.X then begin
        S2 := PointToSmallPoint(S);
        if FLastWrappedLines <> PrevWrappedLines then
          SizesList.Add(TListItemType(S2))
        else
          SizesList[SizesList.Count-1] := TListItemType(S2);
        PrevWrappedLines := FLastWrappedLines;
        Dec(X);
      end
      else
        X := S.X;
    end;
  finally
    FFloatingWidth := SaveWrapX;
  end;
end;

function CompareSizesX(Item1, Item2: TListItemType): Integer;
begin
  { Sorts in descending order }
  Result := TSmallPoint(Item2).X - TSmallPoint(Item1).X;
end;

function CompareSizesY(Item1, Item2: TListItemType): Integer;
begin
  { Sorts in descending order }
  Result := TSmallPoint(Item2).Y - TSmallPoint(Item1).Y;
end;

procedure TTBCustomToolbar.ResizeBegin(ASizeHandle: TTBSizeHandle);
const
  MaxDistance = 12;
var
  I, NewSize: Integer;
  S, N: TSmallPoint;
  P: TPoint;
begin
  inherited;

  FSizeData := TToolbarSizeData.Create;

  with TToolbarSizeData(FSizeData) do begin
    SizeHandle := ASizeHandle;
    OrigWidth := Parent.Width;
    OrigHeight := Parent.Height;
    NCXDiff := ClientToScreen(Point(0, 0)).X - Parent.Left;
    CurRightX := FFloatingWidth;
    DisableSensCheck := False;
    OpSide := False;

    NewSizes := TList.Create;
    BuildPotentialSizesList(NewSizes);
    for I := 0 to NewSizes.Count-1 do begin
      P := SmallPointToPoint(TSmallPoint(NewSizes[I]));
      AddFloatingNCAreaToSize(P);
      NewSizes[I] := TListItemType(PointToSmallPoint(P));
    end;
    if ASizeHandle in [twshTop, twshBottom] then
      NewSizes.Sort(CompareSizesY)
    else
      NewSizes.Sort(CompareSizesX);

    { Calculate distance in pixels to the nearest potential sizes smaller and
      larger than the current size, up to a maximum of MaxDistance pixels. }
    DistanceToSmallerSize := 0;
    DistanceToLargerSize := 0;
    for I := 0 to NewSizes.Count-1 do begin
      S := TSmallPoint(NewSizes[I]);
      if (S.X = OrigWidth) and (S.Y = OrigHeight) then begin
        if I > 0 then begin
          N := TSmallPoint(NewSizes[I-1]);
          if ASizeHandle in [twshLeft, twshRight] then
            NewSize := N.X - S.X
          else
            NewSize := N.Y - S.Y;
          if NewSize > MaxDistance then
            NewSize := MaxDistance;
          DistanceToLargerSize := NewSize;
        end;
        if I < NewSizes.Count-1 then begin
          N := TSmallPoint(NewSizes[I+1]);
          if ASizeHandle in [twshLeft, twshRight] then
            NewSize := S.X - N.X
          else
            NewSize := S.Y - N.Y;
          if NewSize > MaxDistance then
            NewSize := MaxDistance;
          DistanceToSmallerSize := NewSize;
        end;
        Break;
      end;
    end;
  end;
end;

procedure TTBCustomToolbar.ResizeTrack(var Rect: TRect; const OrigRect: TRect);
var
  Pos: TPoint;
  NewOpSide: Boolean;
  Reverse: Boolean;
  I: Integer;
  P: TSmallPoint;
begin
  inherited;

  with TToolbarSizeData(FSizeData) do begin
    Pos.X := Rect.Right - Rect.Left;
    Pos.Y := Rect.Bottom - Rect.Top;

    { Like Office, don't change from the original size until the mouse is moved
      a reasonable distance left/up or right/down. Without this, dragging the
      mouse just one pixel in either direction would cause the toolbar to
      change sizes. }
    if SizeHandle in [twshLeft, twshRight] then
      NewOpSide := Pos.X < OrigWidth
    else
      NewOpSide := Pos.Y < OrigHeight;
    if (not DisableSensCheck) or (OpSide <> NewOpSide) then begin
      DisableSensCheck := False;
      OpSide := NewOpSide;
      if SizeHandle in [twshLeft, twshRight] then begin
        if (Pos.X > OrigWidth-DistanceToSmallerSize) and (Pos.X < OrigWidth+DistanceToLargerSize) then
          Pos.X := OrigWidth;
      end
      else begin
        if (Pos.Y > OrigHeight-DistanceToSmallerSize) and (Pos.Y < OrigHeight+DistanceToLargerSize) then
          Pos.Y := OrigHeight;
      end;
    end;

    Rect := OrigRect;

    if SizeHandle in [twshLeft, twshRight] then
      Reverse := Pos.X > OrigWidth
    else
      Reverse := Pos.Y > OrigHeight;
    if not Reverse then
      I := 0
    else
      I := NewSizes.Count-1;
    while True do begin
      P := TSmallPoint(NewSizes[I]);
      if SizeHandle in [twshLeft, twshRight] then begin
        if (not Reverse and ((I = NewSizes.Count-1) or (Pos.X >= P.X))) or
           (Reverse and ((I = 0) or (Pos.X <= P.X))) then begin
          if I = 0 then
            CurRightX := 0
          else
            CurRightX := P.X - NCXDiff*2;
          if SizeHandle = twshRight then
            Rect.Right := Rect.Left + P.X
          else
            Rect.Left := Rect.Right - P.X;
          Rect.Bottom := Rect.Top + P.Y;
          DisableSensCheck := not EqualRect(Rect, OrigRect);
          Break;
        end;
      end
      else begin
        if (not Reverse and ((I = NewSizes.Count-1) or (Pos.Y >= P.Y))) or
           (Reverse and ((I = 0) or (Pos.Y <= P.Y))) then begin
          if I = NewSizes.Count-1 then
            CurRightX := 0
          else
            CurRightX := P.X - NCXDiff*2;
          if SizeHandle = twshBottom then
            Rect.Bottom := Rect.Top + P.Y
          else
            Rect.Top := Rect.Bottom - P.Y;
          Rect.Right := Rect.Left + P.X;
          DisableSensCheck := not EqualRect(Rect, OrigRect);
          Break;
        end;
      end;
      if not Reverse then
        Inc(I)
      else
        Dec(I);
    end;
  end;
end;

procedure TTBCustomToolbar.ResizeTrackAccept;
begin
  inherited;
  FloatingWidth := TToolbarSizeData(FSizeData).CurRightX;
end;

procedure TTBCustomToolbar.ResizeEnd;
begin
  inherited;
  if Assigned(FSizeData) then begin
    with TToolbarSizeData(FSizeData) do
      FreeAndNil(NewSizes);
    FreeAndNil(FSizeData);
  end;
end;

function TTBCustomToolbar.GetShrinkMode: TTBShrinkMode;
begin
  Result := FShrinkMode;
end;

procedure TTBCustomToolbar.GetMinShrinkSize(var AMinimumSize: Integer);
var
  I: TTBItemViewer;
begin
  I := FView.HighestPriorityViewer;
  if Assigned(I) then begin
    if not(CurrentDock.Position in [dpLeft, dpRight]) then
      AMinimumSize := I.BoundsRect.Right - I.BoundsRect.Left
    else
      AMinimumSize := I.BoundsRect.Bottom - I.BoundsRect.Top;
  end;
  if not(CurrentDock.Position in [dpLeft, dpRight]) then
    Inc(AMinimumSize, NonClientWidth)
  else
    Inc(AMinimumSize, NonClientHeight);
  Inc(AMinimumSize, tbChevronSize);
end;

procedure TTBCustomToolbar.BeginUpdate;
begin
  FView.BeginUpdate;
end;

procedure TTBCustomToolbar.EndUpdate;
begin
  FView.EndUpdate;
end;

procedure TTBCustomToolbar.GetTabOrderList(List: TList);
var
  CtlList: TList;
  I, J: Integer;
  CtlI, CtlJ: TWinControl;
begin
  inherited;
  { Remove off-edge items and their children from List }
  CtlList := TList.Create;
  try
    FView.GetOffEdgeControlList(CtlList);
    for I := 0 to CtlList.Count-1 do begin
      CtlI := TWinControl(CtlList[I]);
      J := 0;
      while J < List.Count do begin
        CtlJ := TWinControl(List[J]);
        if (CtlJ = CtlI) or CtlI.ContainsControl(CtlJ) then
          List.Delete(J)
        else
          Inc(J);
      end;
    end;
  finally
    CtlList.Free;
  end;
end;

procedure TTBCustomToolbar.CMWinIniChange(var Message: TWMWinIniChange);
begin
  inherited;
  if {$IFNDEF CLR}TMessage{$ENDIF}(Message).WParam = SPI_SETNONCLIENTMETRICS then begin
    TBInitToolbarSystemFont;
    Arrange;
  end;
end;

function TTBCustomToolbar.IsShortCut(var Message: TWMKey): Boolean;
begin
  Result := False;
  if Assigned(FOnShortCut) then
    FOnShortCut(Message, Result);
  Result := Result or FItem.IsShortCut(Message);
end;

var
  HookCount: Integer;
  HookList: TList;

class function TTBCustomToolbar.MainWindowHook(var Message: TMessage): Boolean;

  function GetActiveForm: TCustomForm;
  var
    Wnd: HWND;
    Ctl: TWinControl;
  begin
    { Note: We don't use Screen.ActiveCustomForm because when an EXE calls a
      DLL that shows a modal form, Screen.ActiveCustomForm doesn't change in
      the EXE; it remains set to the last form that was active in the EXE.
      Use FindControl(GetActiveWindow) instead to avoid this problem; it will
      return nil when a form in another module is active. }
    Result := nil;
    Wnd := GetActiveWindow;
    if Wnd <> 0 then begin
      Ctl := FindControl(Wnd);
      if Assigned(Ctl) and (Ctl is TCustomForm) then
        Result := TCustomForm(Ctl);
    end;
  end;

  function HandleShortCutOnForm(const Form: TCustomForm): Boolean;
  var
    I: Integer;
    Toolbar: TTBCustomToolbar;
    {$IFDEF CLR}
    KeyMsg: TWMKey;
    {$ENDIF}
  begin
    Result := False;
    if Form = nil then
      Exit;
    for I := 0 to HookList.Count-1 do begin
      Toolbar := TTBCustomToolbar(HookList[I]);
      if Toolbar.ProcessShortCuts and
         (TBGetToolWindowParentForm(Toolbar) = Form) and
         IsWindowEnabled(Form.Handle) then begin
        {$IFNDEF CLR}
        if Toolbar.IsShortCut(TWMKey(Message)) then begin
        {$ELSE}
        KeyMsg := TWMKey.Create(Message);
        if Toolbar.IsShortCut(KeyMsg) then begin
        {$ENDIF}
          Message.Result := 1;
          Result := True;
          Exit;
        end;
      end;
    end;
  end;

  function TraverseControls(Container: TWinControl): Boolean;
  var
    I: Integer;
    Control: TControl;
  begin
    Result := False;
    if Container.Showing then
      for I := 0 to Container.ControlCount - 1 do begin
        Control := Container.Controls[I];
        if Control.Visible and Control.Enabled then begin
          if (csMenuEvents in Control.ControlStyle) and
             ((Control is TTBDock) or (Control is TTBCustomToolbar)) and
             (Control.Perform(WM_SYSCOMMAND, TMessage(Message).WParam,
               TMessage(Message).LParam) <> 0) or (Control is TWinControl) and
             TraverseControls(TWinControl(Control)) then begin
            Result := True;
            Exit;
          end;
        end;
      end;
  end;

var
  ActiveForm: TCustomForm;
  ActiveMDIChild: TForm;
begin
  Result := False;
  if (Message.Msg = CM_APPKEYDOWN) and Assigned(HookList) then begin
    { Process shortcuts on toolbars. Search forms in this order:
      1. If the active form is an MDI parent form, the active MDI child form
         if it has the focus.
      2. The active form.
      3. The main form. }
    ActiveForm := GetActiveForm;
    if Assigned(ActiveForm) and (ActiveForm is TForm) and
       (TForm(ActiveForm).FormStyle = fsMDIForm) then begin
      ActiveMDIChild := TForm(ActiveForm).ActiveMDIChild;
      { Don't search the child form if a control on the MDI parent form is
        currently focused (i.e. Screen.ActiveCustomForm <> ActiveMDIChild) }
      if Assigned(ActiveMDIChild) and
         (Screen.ActiveCustomForm = ActiveMDIChild) and
         HandleShortCutOnForm(ActiveMDIChild) then begin
        Result := True;
        Exit;
      end;
    end;
    if HandleShortCutOnForm(ActiveForm) then
      Result := True
    else begin
      if (Application.MainForm <> ActiveForm) and
         HandleShortCutOnForm(Application.MainForm) then
        Result := True;
    end;
  end
  else if Message.Msg = CM_APPSYSCOMMAND then begin
    { Handle "Alt or Alt+[key] pressed on secondary form" case. If there's a
      menu bar on the active form we want the keypress to go to it instead of
      to the main form (the VCL's default handling). }
    ActiveForm := GetActiveForm;
    if Assigned(ActiveForm) and IsWindowEnabled(ActiveForm.Handle) and
       IsWindowVisible(ActiveForm.Handle) and TraverseControls(ActiveForm) then begin
      Message.Result := 1;
      Result := True;
    end;
  end;
end;

procedure TTBCustomToolbar.SetMainWindowHook;
begin
  if (ProcessShortCuts or MenuBar) and not(csDesigning in ComponentState) then
    InstallMainWindowHook
  else
    UninstallMainWindowHook;
end;

procedure TTBCustomToolbar.InstallMainWindowHook;
begin
  if FMainWindowHookInstalled then
    Exit;
  if HookCount = 0 then
    Application.HookMainWindow(MainWindowHook);
  Inc(HookCount);
  AddToList(HookList, Self);
  FMainWindowHookInstalled := True;
end;

procedure TTBCustomToolbar.UninstallMainWindowHook;
begin
  if not FMainWindowHookInstalled then
    Exit;
  FMainWindowHookInstalled := False;
  RemoveFromList(HookList, Self);
  Dec(HookCount);
  if HookCount = 0 then
    Application.UnhookMainWindow(MainWindowHook);
end;

initialization
  tbChevronSize := MulDiv(12, Screen.PixelsPerInch, 96);
end.
