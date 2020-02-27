{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDockAdvTree.pas, released on 2005-02-14.

The Initial Developer of the Original Code is luxiaoban.
Portions created by luxiaoban are Copyright (C) 2002,2003 luxiaoban.
All Rights Reserved.

Contributor(s):

Last Modified: 2005-02-08


You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org


Description:
  Code split out from JvDockTree.pas because of compiler issues - WPostma.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDockAdvTree;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Classes, Graphics, Controls, Forms,
  JvDockTree;

type
  TJvDockAdvTree = class(TJvDockTree)
  private
    FButtonHeight: Integer;
    FButtonWidth: Integer;
    FLeftOffset: Integer;
    FRightOffset: Integer;
    FTopOffset: Integer;
    FBottomOffset: Integer;
    FButtonSplitter: Integer;
    FCloseButtonZone: TJvDockAdvZone;
    FDropDockSize: Integer;
    FDockHeightWidth: array [TDockOrientation] of Integer;
    FDockRectangles: array [TDockOrientation, Boolean] of Integer;
    function GetBottomOffset: Integer;
    function GetButtonHeight: Integer;
    function GetButtonSplitter: Integer;
    function GetButtonWidth: Integer;
    function GetLeftOffset: Integer;
    function GetRightOffset: Integer;
    function GetTopOffset: Integer;
    procedure SetBottomOffset(const Value: Integer);
    procedure SetButtonHeight(const Value: Integer);
    procedure SetButtonSplitter(const Value: Integer);
    procedure SetButtonWidth(const Value: Integer);
    procedure SetLeftOffset(const Value: Integer);
    procedure SetRightOffset(const Value: Integer);
    procedure SetTopOffset(const Value: Integer);
    function GetDockHeightWidth(Orient: TDockOrientation): Integer;
    procedure SetDockHeightWidth(Orient: TDockOrientation; const Value: Integer);
    function GetDockRectangles(Orient: TDockOrientation; AtLast: Boolean): Integer;
    procedure SetDockRectangles(Orient: TDockOrientation; AtLast: Boolean; const Value: Integer);
    procedure SetDropDockSize(const Value: Integer);
  protected
    function DoLButtonDown(var Msg: TWMMouse;
      var Zone: TJvDockZone; out HTFlag: Integer): Boolean; override;
    procedure DoLButtonUp(var Msg: TWMMouse;
      var Zone: TJvDockZone; out HTFlag: Integer); override;
    procedure DoMouseMove(var Msg: TWMMouse;
      var Zone: TJvDockZone; out HTFlag: Integer); override;
    procedure InsertSibling(NewZone, SiblingZone: TJvDockZone;
      InsertLast, Update: Boolean); override;
    procedure InsertNewParent(NewZone, SiblingZone: TJvDockZone;
      ParentOrientation: TDockOrientation; InsertLast, Update: Boolean); override;
    procedure InitDockHeightWidth(NoOrValue, HorValue, VerValue: Integer);
    procedure InitDockRectangles(ARect: TRect);
    procedure ScaleZone(Zone: TJvDockZone); override;
    procedure ScaleChildZone(Zone: TJvDockZone); override;
    procedure ScaleSiblingZone(Zone: TJvDockZone); override;
    procedure ShiftZone(Zone: TJvDockZone); override;
    procedure RemoveZone(Zone: TJvDockZone; Hide: Boolean); override;
  public
    constructor Create(DockSite: TWinControl; ADockZoneClass: TJvDockZoneClass;
      ADockStyle: TJvDockObservableStyle); override;
    property BottomOffset: Integer read GetBottomOffset write SetBottomOffset;
    property ButtonHeight: Integer read GetButtonHeight write SetButtonHeight;
    property ButtonSplitter: Integer read GetButtonSplitter write SetButtonSplitter;
    property ButtonWidth: Integer read GetButtonWidth write SetButtonWidth;
    property LeftOffset: Integer read GetLeftOffset write SetLeftOffset;
    property RightOffset: Integer read GetRightOffset write SetRightOffset;
    property TopOffset: Integer read GetTopOffset write SetTopOffset;
    property CloseButtonZone: TJvDockAdvZone read FCloseButtonZone write FCloseButtonZone;
    property DockHeightWidth[Orient: TDockOrientation]: Integer read GetDockHeightWidth write SetDockHeightWidth;
    property DockRectangles[Orient: TDockOrientation; AtLast: Boolean]: Integer read GetDockRectangles write
      SetDockRectangles;
    property DropDockSize: Integer read FDropDockSize write SetDropDockSize;
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

//=== { TJvDockAdvTree } =====================================================

constructor TJvDockAdvTree.Create(DockSite: TWinControl;
  ADockZoneClass: TJvDockZoneClass; ADockStyle: TJvDockObservableStyle);
begin
  inherited Create(DockSite, ADockZoneClass, ADockStyle);
  FButtonHeight := 12;
  FButtonWidth := 12;
  FLeftOffset := 0;
  FRightOffset := 0;
  FTopOffset := 0;
  FBottomOffset := 0;
  FButtonSplitter := 2;
end;

function TJvDockAdvTree.DoLButtonDown(var Msg: TWMMouse;
  var Zone: TJvDockZone; out HTFlag: Integer): Boolean;
var
  TempZone: TJvDockAdvZone;
begin
  Result := inherited DoLButtonDown(Msg, Zone, HTFlag);
  if (Zone <> nil) and (HTFlag = HTCLOSE) then
  begin
    TempZone := TJvDockAdvZone(Zone);
    TempZone.CloseBtnDown := True;
    TempZone.MouseDown := True;
    FCloseButtonZone := TempZone;
    DockSite.Invalidate;
  end;
end;

procedure TJvDockAdvTree.DoLButtonUp(var Msg: TWMMouse;
  var Zone: TJvDockZone; out HTFlag: Integer);
begin
  inherited DoLButtonUp(Msg, Zone, HTFlag);
  if SizingZone = nil then
  begin
    FCloseButtonZone := nil;
    if (Zone <> nil) and (HTFlag = HTCLOSE) then
      TJvDockAdvZone(Zone).CloseBtnDown := False;
  end;
end;

procedure TJvDockAdvTree.DoMouseMove(var Msg: TWMMouse;
  var Zone: TJvDockZone; out HTFlag: Integer);
var
  TempZone: TJvDockAdvZone;
begin
  inherited DoMouseMove(Msg, Zone, HTFlag);
  if SizingZone = nil then
  begin
    TempZone := TJvDockAdvZone(Zone);
    if ((TempZone <> nil) and (TempZone.CloseBtnDown <> (HTFlag = HTCLOSE)) and
      ((FCloseButtonZone = TempZone) and FCloseButtonZone.MouseDown)) then
    begin
      TempZone.CloseBtnDown := (HTFlag = HTCLOSE) and FCloseButtonZone.MouseDown;
      DockSite.Invalidate;
    end;
  end;
end;

procedure TJvDockAdvTree.InsertSibling(NewZone, SiblingZone: TJvDockZone;
  InsertLast, Update: Boolean);
var
  TempUpdate: Boolean;
begin
  TempUpdate := Update;
  Update := False;
  try
    inherited InsertSibling(NewZone, SiblingZone, InsertLast, Update);
    if NewZone.ChildControl <> nil then
      InitDockHeightWidth(0, NewZone.ChildControl.TBDockHeight + BorderWidth,
        NewZone.ChildControl.LRDockWidth + BorderWidth)
    else
      InitDockHeightWidth(0, 0, 0);
  finally
    Update := TempUpdate;
  end;

  if Update then
  begin
    NewZone.Insert(FDropDockSize, False);
    SetNewBounds(NewZone.ParentZone);
    ForEachAt(NewZone.ParentZone, UpdateZone, tskForward);
  end;
end;

procedure TJvDockAdvTree.SetBottomOffset(const Value: Integer);
begin
  FBottomOffset := Value;
end;

procedure TJvDockAdvTree.SetButtonHeight(const Value: Integer);
begin
  FButtonHeight := Value;
end;

procedure TJvDockAdvTree.SetButtonSplitter(const Value: Integer);
begin
  FButtonSplitter := Value;
end;

procedure TJvDockAdvTree.SetButtonWidth(const Value: Integer);
begin
  FButtonWidth := Value;
end;

procedure TJvDockAdvTree.SetLeftOffset(const Value: Integer);
begin
  FLeftOffset := Value;
end;

procedure TJvDockAdvTree.SetRightOffset(const Value: Integer);
begin
  FRightOffset := Value;
end;

procedure TJvDockAdvTree.SetTopOffset(const Value: Integer);
begin
  FTopOffset := Value;
end;

function TJvDockAdvTree.GetBottomOffset: Integer;
begin
  Result := PPIScale(FBottomOffset);
end;

function TJvDockAdvTree.GetButtonHeight: Integer;
begin
  Result := PPIScale(FButtonHeight);
end;

function TJvDockAdvTree.GetButtonSplitter: Integer;
begin
  Result := PPIScale(FButtonSplitter);
end;

function TJvDockAdvTree.GetButtonWidth: Integer;
begin
  Result := PPIScale(FButtonWidth);
end;

function TJvDockAdvTree.GetDockHeightWidth(Orient: TDockOrientation): Integer;
begin
  Result := FDockHeightWidth[Orient];
end;

procedure TJvDockAdvTree.SetDockHeightWidth(Orient: TDockOrientation;
  const Value: Integer);
begin
  FDockHeightWidth[Orient] := Value;
end;

function TJvDockAdvTree.GetDockRectangles(Orient: TDockOrientation;
  AtLast: Boolean): Integer;
begin
  Result := FDockRectangles[Orient, AtLast];
end;

function TJvDockAdvTree.GetLeftOffset: Integer;
begin
   Result := PPIScale(FLeftOffset);
end;

function TJvDockAdvTree.GetRightOffset: Integer;
begin
  Result := PPIScale(FRightOffset);
end;

function TJvDockAdvTree.GetTopOffset: Integer;
begin
  Result := PPIScale(FTopOffset);
end;

procedure TJvDockAdvTree.SetDockRectangles(Orient: TDockOrientation;
  AtLast: Boolean; const Value: Integer);
begin
  FDockRectangles[Orient, AtLast] := Value;
end;

procedure TJvDockAdvTree.InitDockRectangles(ARect: TRect);
begin
  FDockRectangles[doNoOrient, False] := 0;
  FDockRectangles[doNoOrient, True] := 0;
  FDockRectangles[doHorizontal, False] := ARect.Top;
  FDockRectangles[doHorizontal, True] := ARect.Bottom;
  FDockRectangles[doVertical, False] := ARect.Left;
  FDockRectangles[doVertical, True] := ARect.Right;
end;

procedure TJvDockAdvTree.InitDockHeightWidth(NoOrValue, HorValue,
  VerValue: Integer);
begin
  FDockHeightWidth[doNoOrient] := NoOrValue;
  FDockHeightWidth[doHorizontal] := HorValue;
  FDockHeightWidth[doVertical] := VerValue;
end;

procedure TJvDockAdvTree.ScaleChildZone(Zone: TJvDockZone);
begin
  if Zone = ReplacementZone then
    ShiftScaleOrientation := doNoOrient;
  inherited ScaleChildZone(Zone);
end;

procedure TJvDockAdvTree.ScaleSiblingZone(Zone: TJvDockZone);
begin
  if Zone = ReplacementZone then
    ShiftScaleOrientation := doNoOrient;
  inherited ScaleSiblingZone(Zone);
end;

procedure TJvDockAdvTree.ScaleZone(Zone: TJvDockZone);
begin
  if Zone = ReplacementZone then
    ShiftScaleOrientation := doNoOrient;
  inherited ScaleZone(Zone);
end;

procedure TJvDockAdvTree.ShiftZone(Zone: TJvDockZone);
begin
  if Zone = ReplacementZone then
    ShiftScaleOrientation := doNoOrient;
  inherited ShiftZone(Zone);
end;

procedure TJvDockAdvTree.InsertNewParent(NewZone, SiblingZone: TJvDockZone;
  ParentOrientation: TDockOrientation; InsertLast, Update: Boolean);
var
  TempUpdate: Boolean;
begin
  TempUpdate := Update;
  Update := False;
  if NewZone.ChildControl <> nil then
    InitDockHeightWidth(0, NewZone.ChildControl.TBDockHeight + BorderWidth,
      NewZone.ChildControl.LRDockWidth + BorderWidth)
  else
    InitDockHeightWidth(0, 0, 0);

  if SiblingZone = nil then
    if InsertLast then
      ReplacementZone := TopZone
    else
      ReplacementZone := NewZone;

  try
    inherited InsertNewParent(NewZone, SiblingZone, ParentOrientation, InsertLast, Update);
  finally
    Update := TempUpdate;
    ReplacementZone := nil;
  end;

  if Update then
  begin
    NewZone.Insert(DropDockSize, False);
    ForEachAt(NewZone.ParentZone, UpdateZone, tskForward);
    SetNewBounds(NewZone.ParentZone);
  end;
end;

procedure TJvDockAdvTree.RemoveZone(Zone: TJvDockZone; Hide: Boolean);
begin
  inherited RemoveZone(Zone, Hide);
end;

procedure TJvDockAdvTree.SetDropDockSize(const Value: Integer);
begin
  FDropDockSize := Value;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
