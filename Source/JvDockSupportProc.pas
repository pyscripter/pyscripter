{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDockSupportProc.pas, released on 2003-12-31.

The Initial Developer of the Original Code is luxiaoban.
Portions created by luxiaoban are Copyright (C) 2002,2003 luxiaoban.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDockSupportProc;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Types, Windows, Messages, Classes, Graphics, Controls, Forms, ImgList;

type
  TJvDockListScanKind = (lskForward, lskBackward);

function JvDockStreamDataToString(Stream: TStream): string;
procedure JvDockStringToStreamData(Stream: TStream; const Data: string);

function JvDockFindDockFormWithName(const FormName: string;
  FromDockManager: Boolean = False;
  FromList: Boolean = True;
  ScanKind: TJvDockListScanKind = lskForward): TCustomForm;
function JvDockFindDockServerFormWithName(const FormName: string;
  FromDockManager: Boolean = False;
  FromList: Boolean = True;
  ScanKind: TJvDockListScanKind = lskForward): TCustomForm;
function JvDockFindDockClientFormWithName(const FormName: string;
  FromDockManager: Boolean = False;
  FromList: Boolean = True;
  ScanKind: TJvDockListScanKind = lskForward): TCustomForm;
function JvDockFindDockServerFromDockManager(const FormName: string;
  FromList: Boolean = True;
  ScanKind: TJvDockListScanKind = lskForward): TCustomForm;
function JvDockFindDockClientFromDockManager(const FormName: string;
  FromList: Boolean = True;
  ScanKind: TJvDockListScanKind = lskForward): TCustomForm;
function JvDockFindDockFormFromScreen(const FormName: string;
  ScanKind: TJvDockListScanKind = lskForward): TCustomForm;

function JvDockGetMinOffset(TBDockSize, ControlSize: Integer; Scale: Real): Integer;

function JvDockGetNoNClientMetrics: TNONCLIENTMETRICS;

function JvDockGetSysCaptionHeight: Integer;

function JvDockGetSysBorderWidth: Integer;
function JvDockGetSysCaptionHeightAndBorderWidth: Integer;
function JvDockGetActiveTitleBeginColor: TColor;
function JvDockGetActiveTitleEndColor: TColor;
function JvDockGetInactiveTitleBeginColor: TColor;
function JvDockGetInactiveTitleEndColor: TColor;
function JvDockGetTitleFontColor(Active: Boolean): TColor;
function JvDockGetActiveTitleFontColor: TColor;
function JvDockGetInactiveTitleFontColor: TColor;
function JvDockGetTitleFont: TFont;

procedure JvDockLockWindow(Control: TWinControl);
procedure JvDockUnLockWindow;

function JvDockCreateNCMessage(Control: TControl; Msg: Cardinal; HTFlag: Integer; Pos: TPoint): TWMNCHitMessage;
function JvDockExchangeOrient(Orient: TDockOrientation): TDockOrientation;
function JvDockGetControlOrient(AControl: TControl): TDockOrientation;
function JvDockGetControlSize(AControl: TControl): Integer;

procedure JvScaleImageList(const ImgList: TCustomImageList; M, D: Integer);
procedure JvDrawTextWithMiddleEllipsis(Canvas: TCanvas; Text: string; DrawRect:
  TRect; Flags: Integer);

procedure RegisterSettingChangeClient(Client: TObject; Event: TNotifyEvent);
procedure UnRegisterSettingChangeClient(Client: TObject);

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
  SysUtils, Math, CommCtrl,
  JvJVCLUtils, JvDockControlForm, JvDockGlobals;

type
  { The dock style components used to hook the form they were dropped on, so
    they could respond to Windows setting changes. The components can now use
    the TJvMsgWindow -via the RegisterSettingChangeClient procedure- that creates
    a window so it is able to receive WM_SETTINGCHANGE messages. Notification is
    done via the Observer pattern
  }
  TJvMsgWindow = class(TObject)
  private
    FHandle: HWND;
    FClients: TList;
    FNotifyEvents: TList;
    procedure WndProc(var Msg: TMessage);
    procedure NotifyClients;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure RegisterClient(Client: TObject; Event: TNotifyEvent); virtual;
    procedure UnRegisterClient(Client: TObject); virtual;
  end;

var
  GMsgHook: TJvMsgWindow;
  JvDockTitleFont: TFont = nil;

function JvDockStreamDataToString(Stream: TStream): string;
var
  Ch: AnsiChar;
begin
  Result := '';
  Stream.Position := 0;
  while Stream.Position < Stream.Size do
  begin
    Stream.Read(Ch, SizeOf(Ch));
    Result := Result + IntToHex(Ord(Ch), 2);
  end;
end;

procedure JvDockStringToStreamData(Stream: TStream; const Data: string);
var
  I: Integer;
  Ch: AnsiChar;
begin
  I := 1;
  while I < Length(Data) do
  begin
    Ch := AnsiChar(StrToInt('$' + Copy(Data, I, 2)));
    Stream.Write(Ch, SizeOf(Ch));
    Inc(I, 2);
  end;
end;

function JvDockFindDockFormWithName(const FormName: string; FromDockManager: Boolean;
  FromList: Boolean; ScanKind: TJvDockListScanKind): TCustomForm;
begin
  Result := JvDockFindDockClientFormWithName(FormName, FromDockManager, FromList, ScanKind);
  if Result = nil then
    Result := JvDockFindDockServerFormWithName(FormName, FromDockManager, FromList, ScanKind);
end;

function JvDockFindDockServerFormWithName(const FormName: string; FromDockManager: Boolean;
  FromList: Boolean; ScanKind: TJvDockListScanKind): TCustomForm;
begin
  if FromDockManager then
    Result := JvDockFindDockServerFromDockManager(FormName, FromList, ScanKind)
  else
    Result := JvDockFindDockFormFromScreen(FormName, ScanKind);
end;

function JvDockFindDockClientFormWithName(const FormName: string; FromDockManager: Boolean;
  FromList: Boolean; ScanKind: TJvDockListScanKind): TCustomForm;
begin
  if FromDockManager then
    Result := JvDockFindDockClientFromDockManager(FormName, FromList, ScanKind)
  else
    Result := JvDockFindDockFormFromScreen(FormName, ScanKind);
end;

function JvDockFindDockServerFromDockManager(const FormName: string; FromList: Boolean;
  ScanKind: TJvDockListScanKind): TCustomForm;
var
  I: Integer;
begin
  case ScanKind of
    lskForward:
      for I := 0 to JvGlobalDockManager.DockServerCount - 1 do
      begin
        Result := JvGlobalDockManager.DockServer[I].ParentForm;
        if Assigned(Result) and (FormName = Result.Name) then
          Exit;
      end;
    lskBackward:
      for I := JvGlobalDockManager.DockServerCount - 1 downto 0 do
      begin
        Result := JvGlobalDockManager.DockServer[I].ParentForm;
        if Assigned(Result) and (FormName = Result.Name) then
          Exit;
      end;
  end;
  Result := nil;
end;
function JvDockFindDockClientFromDockManager(const FormName: string; FromList: Boolean;
  ScanKind: TJvDockListScanKind): TCustomForm;
var
  I: Integer;
begin
  case ScanKind of
    lskForward:
      for I := 0 to JvGlobalDockManager.DockClientCount - 1 do
      begin
        Result := JvGlobalDockManager.DockClient[I].ParentForm;
        if Assigned(Result) and (FormName = Result.Name) then
          Exit;
      end;
    lskBackward:
      for I := JvGlobalDockManager.DockClientCount - 1 downto 0 do
      begin
        Result := JvGlobalDockManager.DockClient[I].ParentForm;
        if Assigned(Result) and (FormName = Result.Name) then
          Exit;
      end;
  end;
  Result := nil;
end;

function JvDockFindDockFormFromScreen(const FormName: string;
  ScanKind: TJvDockListScanKind): TCustomForm;
var
  I: Integer;
begin
  Result := nil;
  case ScanKind of
    lskForward:
      for I := 0 to Screen.CustomFormCount - 1 do
        if FormName = Screen.CustomForms[I].Name then
        begin
          Result := Screen.CustomForms[I];
          Break;
        end;
    lskBackward:
      for I := Screen.CustomFormCount - 1 downto 0 do
        if FormName = Screen.CustomForms[I].Name then
        begin
          Result := Screen.CustomForms[I];
          Break;
        end;
  end;
end;

function JvDockGetMinOffset(TBDockSize, ControlSize: Integer; Scale: Real): Integer;
begin
  if (Scale < 0) or (Scale > 1) then
    Scale := 1;
  Result := Min(TBDockSize, Round(ControlSize * Scale));
end;

function JvDockGetNoNClientMetrics: TNONCLIENTMETRICS;
begin
  {$IFDEF RTL210_UP}
  Result.cbSize := TNonClientMetrics.SizeOf;
  {$ELSE}
  Result.cbSize := SizeOf(Result);
  {$ENDIF RTL210_UP}
  SystemParametersInfo(SPI_GETNONCLIENTMETRICS, Result.cbSize, @Result, 0);
end;

function JvDockGetSysCaptionHeight: Integer;
begin
  Result := JvDockGetNoNClientMetrics.iCaptionHeight
end;

function JvDockGetSysBorderWidth: Integer;
begin
  Result := JvDockGetNoNClientMetrics.iBorderWidth;
end;

function JvDockGetSysCaptionHeightAndBorderWidth: Integer;
var
  NoNCM: TNONCLIENTMETRICS;
begin
  NoNCM := JvDockGetNoNClientMetrics;
  Result := NoNCM.iBorderWidth + NoNCM.iCaptionHeight;
end;

function JvDockGetActiveTitleBeginColor: TColor;
begin
  Result := GetSysColor(COLOR_ACTIVECAPTION);
end;

function JvDockGetActiveTitleEndColor: TColor;
begin
  Result := GetSysColor(COLOR_GRADIENTACTIVECAPTION);
end;

function JvDockGetInactiveTitleBeginColor: TColor;
begin
  Result := GetSysColor(COLOR_INACTIVECAPTION);
end;

function JvDockGetInactiveTitleEndColor: TColor;
begin
  Result := GetSysColor(COLOR_GRADIENTINACTIVECAPTION);
end;

function JvDockGetTitleFontColor(Active: Boolean): TColor;
begin
  if Active then
    Result := JvDockGetActiveTitleFontColor
  else
    Result := JvDockGetInactiveTitleFontColor;
end;

function JvDockGetActiveTitleFontColor: TColor;
begin
  Result := GetSysColor(COLOR_CAPTIONTEXT);
end;

function JvDockGetInactiveTitleFontColor: TColor;
begin
  Result := GetSysColor(COLOR_INACTIVECAPTIONTEXT);
end;

function JvDockGetTitleFont: TFont;
var
  NoNCM: TNONCLIENTMETRICS;
begin
  if JvDockTitleFont = nil then
    JvDockTitleFont := TFont.Create;
  Result := JvDockTitleFont;
  NoNCM := JvDockGetNoNClientMetrics;
  Result.Handle := CreateFontIndirect(NoNCM.lfCaptionFont);
end;

var
  GLockCount: Integer;
  GWindowLocked: Boolean;

procedure JvDockLockWindow(Control: TWinControl);
begin
  { Ignore Control parameter; otherwise nested JvDockLockWindow calls are not possible }
  if GLockCount = 0 then
    GWindowLocked := LockWindowUpdate(GetDesktopWindow);
  Inc(GLockCount);
end;

procedure JvDockUnLockWindow;
begin
  Dec(GLockCount);
  if GLockCount = 0 then
  begin
    if GWindowLocked then
      LockWindowUpdate(0);
    GWindowLocked := False;
  end;
end;

function JvDockCreateNCMessage(Control: TControl; Msg: Cardinal; HTFlag: Integer;
  Pos: TPoint): TWMNCHitMessage;
begin
  Result.Msg := Msg;
  Result.HitTest := HTFlag;
  Pos := Control.ClientToScreen(Pos);
  Result.XCursor := Pos.X;
  Result.YCursor := Pos.Y;
end;

function JvDockExchangeOrient(Orient: TDockOrientation): TDockOrientation;
begin
  case Orient of
    doHorizontal:
      Result := doVertical;
    doVertical:
      Result := doHorizontal;
  else
    Result := doNoOrient;
  end;
end;

function JvDockGetControlOrient(AControl: TControl): TDockOrientation;
begin
  Assert(AControl <> nil);
  Result := doNoOrient;
  case AControl.Align of
    alClient, alNone:
      Result := doNoOrient;
    alLeft, alRight:
      Result := doVertical;
    alTop, alBottom:
      Result := doHorizontal;
  end;
end;

function JvDockGetControlSize(AControl: TControl): Integer;
begin
  case JvDockGetControlOrient(AControl) of
    doVertical:
      Result := AControl.Width;
    doHorizontal:
      Result := AControl.Height;
  else
    raise Exception.CreateRes(@RsEDockCannotGetValueWithNoOrient);
  end;
end;

procedure RegisterSettingChangeClient(Client: TObject; Event: TNotifyEvent);
begin
  if GMsgHook = nil then
    GMsgHook := TJvMsgWindow.Create;
  GMsgHook.RegisterClient(Client, Event);
end;

procedure UnRegisterSettingChangeClient(Client: TObject);
begin
  if Assigned(GMsgHook) then
  begin
    GMsgHook.UnRegisterClient(Client);
    if GMsgHook.FClients.Count = 0 then
      FreeAndNil(GMsgHook);
  end;
end;

//=== { TJvMsgWindow } =======================================================

constructor TJvMsgWindow.Create;
begin
  inherited Create;
  FClients := TList.Create;
  FNotifyEvents := TList.Create;
  FHandle := AllocateHWndEx(WndProc);
end;

destructor TJvMsgWindow.Destroy;
begin
  if FHandle <> 0 then
    DeallocateHWndEx(FHandle);
  FClients.Free;
  FNotifyEvents.Free;
  inherited Destroy;
end;

procedure TJvMsgWindow.NotifyClients;
var
  I: Integer;
  NotifyEvent: TNotifyEvent;
begin
  for I := 0 to FClients.Count - 1 do
  begin
    TMethod(NotifyEvent).Code := FNotifyEvents[I];
    TMethod(NotifyEvent).Data := FClients[I];
    NotifyEvent(Self);
  end;
end;

procedure TJvMsgWindow.RegisterClient(Client: TObject; Event: TNotifyEvent);
begin
  FClients.Add(Client);
  FNotifyEvents.Add(TMethod(Event).Code);
end;

procedure TJvMsgWindow.UnRegisterClient(Client: TObject);
var
  Index: Integer;
begin
  Index := FClients.IndexOf(Client);
  if Index <> -1 then
  begin
    FClients.Delete(Index);
    FNotifyEvents.Delete(Index);
  end;
end;

procedure TJvMsgWindow.WndProc(var Msg: TMessage);
begin
  with Msg do
    if (Msg = WM_SETTINGCHANGE) or (Msg = WM_SYSCOLORCHANGE) then
    try
      NotifyClients;
    except
      if Assigned(ApplicationHandleException) then
        ApplicationHandleException(Self);
    end
    else
      { !! Call DefWindowProc, so messages like WM_QUERYENDSESSION are
           processed correctly, see Mantis #3527 }
      Result := DefWindowProc(FHandle, Msg, wParam, lParam);
end;

procedure JvScaleImageList(const ImgList: TCustomImageList; M, D: Integer);
var
  {ScaleFactor, }ii : integer;
  mb, ib, sib, smb : TBitmap;
  TmpImgList : TImageList;
begin
  if M = D then Exit;

//  ScaleFactor := M div D;
  //clear images
  TmpImgList := TImageList.Create(nil);
  try
    TmpImgList.Assign(ImgList);

    //set size to match DPI size (like 250% of 16px = 40px)
    ImgList.Clear;
    ImgList.SetSize(MulDiv(ImgList.Width, M, D), MulDiv(ImgList.Height, M, D));

    //add images back to original ImageList stretched (if DPI scaling > 150%) or centered (if DPI scaling <= 150%)
    for ii := 0 to -1 + TmpImgList.Count do
    begin
      ib := TBitmap.Create;
      mb := TBitmap.Create;
      try
        ib.SetSize(TmpImgList.Width, TmpImgList.Height);
        ib.Canvas.FillRect(ib.Canvas.ClipRect);

        mb.SetSize(TmpImgList.Width, TmpImgList.Height);
        mb.Canvas.FillRect(mb.Canvas.ClipRect);

        ImageList_DrawEx(TmpImgList.Handle, ii, ib.Canvas.Handle, 0, 0, ib.Width, ib.Height, CLR_NONE, CLR_NONE, ILD_NORMAL);
        ImageList_DrawEx(TmpImgList.Handle, ii, mb.Canvas.Handle, 0, 0, mb.Width, mb.Height, CLR_NONE, CLR_NONE, ILD_MASK);

        sib := TBitmap.Create; //stretched (or centered) image
        smb := TBitmap.Create; //stretched (or centered) mask
        try
          sib.SetSize(ImgList.Width, ImgList.Height);
          sib.Canvas.FillRect(sib.Canvas.ClipRect);
          smb.SetSize(ImgList.Width, ImgList.Height);
          smb.Canvas.FillRect(smb.Canvas.ClipRect);

          if M * 100 / D >= 150 then //stretch if >= 150%
          begin
            sib.Canvas.StretchDraw(Rect(0, 0, sib.Width, sib.Width), ib);
            smb.Canvas.StretchDraw(Rect(0, 0, smb.Width, smb.Width), mb);
          end
          else //center if < 150%
          begin
            sib.Canvas.Draw((sib.Width - ib.Width) DIV 2, (sib.Height - ib.Height) DIV 2, ib);
            smb.Canvas.Draw((smb.Width - mb.Width) DIV 2, (smb.Height - mb.Height) DIV 2, mb);
          end;
          ImgList.Add(sib, smb);
        finally
          sib.Free;
          smb.Free;
        end;
    finally
        ib.Free;
        mb.Free;
      end;
    end;
  finally
    TmpImgList.Free;
  end;
end;

procedure JvDrawTextWithMiddleEllipsis(Canvas: TCanvas; Text: string; DrawRect:
  TRect; Flags: Integer);
var
  S, LastS: string;
  R: TRect;
  Sz: TSize;
  RectWidth, I: Integer;
begin
  S := Text;
  R := DrawRect;
  GetTextExtentPoint32(Canvas.Handle, S, Length(S), Sz);
  RectWidth := DrawRect.Right - DrawRect.Left;
  if Sz.cx > RectWidth then
  begin
    // The string is too wide. Need to cut it down with ellipsis
    // Start with the smallest possible truncated-and-ellipsis-modified string,
    // and expand until we have the biggest one that can fit
    S := '...';
    for I := 1 to Length(Text) div 2 do
    begin
      LastS := S;
      // Get the first I chars, then the ellipsis, then the last I chars
      S := Copy(Text, 1, I) + '...' + Copy(Text, Length(Text) - I + 1, I);
      GetTextExtentPoint32(Canvas.Handle, S, Length(S), Sz);
      if Sz.cx > RectWidth then
      begin
        DrawText(Canvas.Handle, LastS, Length(LastS), DrawRect, Flags);
        Break;
      end;
    end;
  end
  else
    // The string will fit in the width of the given rect, don't mess with it
    DrawText(Canvas.Handle, S, Length(S), DrawRect, Flags);
end;


initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  FreeAndNil(JvDockTitleFont);
  FreeAndNil(GMsgHook);
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
