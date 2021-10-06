{-----------------------------------------------------------------------------
 Unit Name: cCodeHint
 Author:    Kiriakos Vlahos
 Date:      08-Dec-2005
 Purpose:   Classes to support code hints based on JvHint.pas of the JVCL
            library
 History:   Modified to get the hints asynchronously
-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHint.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

component   : TJvHint
description : Custom activated hint

Known Issues:
-----------------------------------------------------------------------------}
// $Id: JvHint.pas,v 1.22 2005/02/17 10:20:36 marquardt Exp $


unit cCodeHint;

interface

uses
  SysUtils, Classes, Windows, Controls, Forms, ExtCtrls, Dialogs,
  JvHTControls, JvTypes;

type
  TCodeHintWindow = class(THintWindow)
  public
    HtLabel: TJvHTLabel;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    function CalcHintRect(MaxWidth: Integer;
      const AHint: THintString; AData: Pointer): TRect; override;
  end;
  TCodeHintWindowClass = class of TCodeHintWindow;

  TCodeHintState = (tmBeginShow, tmShow, tmShowing, tmStopped, tmProcessing);
  TGetCodeHintEvent = procedure(Sender : TObject; AArea : TRect;
    var CodeHint : string) of object;


  TCodeHint = class(TComponent)
  private
    FAutoHide: Boolean;
    FOldCursorPos : TPoint;
    fOnGetCodeHint: TGetCodeHintEvent;
    FScreenPos : TPoint;
    procedure SetOnGetCodeHint(const Value: TGetCodeHintEvent);
    function GetHyperLinkClick: TJvHyperLinkClickEvent;
    procedure SetHyperLinkClick(const Value: TJvHyperLinkClickEvent);
  protected
    R: TRect;
    Area: TRect;
    State: TCodeHintState;
    HintWindow: TCodeHintWindow;
    TimerHint: TTimer;
    FDelay: Integer;
    FTaskId: Integer;
    FHintText: string;
    procedure TimerHintTimer(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ActivateHint(AArea: TRect);
    procedure ActivateHintAt(AArea: TRect; ScreenPos: TPoint);
    procedure CancelHint;
  published
    property AutoHide: Boolean read FAutoHide write FAutoHide default True;
    property OnGetCodeHint : TGetCodeHintEvent
      read fOnGetCodeHint write SetOnGetCodeHint;
    property OnHyperLinkClick : TJvHyperLinkClickEvent
      read GetHyperLinkClick write SetHyperLinkClick;
    property HintArea : TRect read Area;
  end;

var
  CodeHint : TCodeHint;

implementation

uses
  System.Threading,
  System.Math,
  Vcl.Graphics,
  Vcl.Themes,
  uCommonFunctions;


//=== { TCodeHint } ============================================================

constructor TCodeHint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TimerHint := TTimer.Create(Self);
  TimerHint.Enabled := False;
  TimerHint.Interval := 70;
  TimerHint.OnTimer := TimerHintTimer;
  HintWindow := TCodeHintWindowClass.Create(Self);
  FAutoHide := False;
end;

destructor TCodeHint.Destroy;
begin
  TimerHint.Free;
  HintWindow.Free;
  inherited Destroy;
end;

procedure TCodeHint.ActivateHint(AArea: TRect);
var
  P: TPoint;
begin
  GetCursorPos(P);
  Inc(P.Y, 20);
  ActivateHintAt(AArea, P);
end;

procedure TCodeHint.ActivateHintAt(AArea: TRect; ScreenPos: TPoint);
var
  P: TPoint;
begin
  GetCursorPos(P);
  fOldCursorPos := P;

  if (CompareMem(@Area, @AArea, SizeOf(TRect)) and
    (State in [tmBeginShow, tmProcessing, tmShow, tmShowing])) or
    ((State = tmShowing) and PtInRect(Area, P))
  then
    Exit;  //  already showing this hint

  if State = tmProcessing then
  begin
     // Do not run multiple tasks
     FTaskId := 0;
     Exit;
  end;

  Area := AArea;
  FScreenPos := ScreenPos;
  FTaskId := 0;
  State := tmBeginShow;
  TimerHint.Enabled := True;
end;

procedure TCodeHint.TimerHintTimer(Sender: TObject);
var
  P: TPoint;
  bPoint, bDelay: Boolean;
  Delay: Integer;
  HintPause: Integer;
begin
  Delay := FDelay * Integer(TimerHint.Interval);
  case State of
    tmProcessing : Exit;  // still working
    tmBeginShow:
      begin
        GetCursorPos(P);
        if (Abs(fOldCursorPos.X - P.X) > 2) or (Abs(fOldCursorPos.Y - P.Y) > 2) then begin
          //  mouse was on the move and not hovering
          State := tmStopped;
          Exit;
        end;
        bPoint := not PtInRect(Area, P) and (FindVCLWindow(P) <> HintWindow);
        if bPoint then
        begin
          State := tmStopped;
          Exit;
        end;
        if IsWindowVisible(HintWindow.Handle) then
          HintPause := Application.HintShortPause
        else
          HintPause := Application.HintPause;
        if Delay >= HintPause then
        begin
          // Get the text to display
          if Assigned(fOnGetCodeHint) then begin
            State := tmProcessing;
            var Task: ITask := TTask.Create(procedure
            begin
              var Txt := '';
              fOnGetCodeHint(Self, Area, Txt);
              if (Txt <> '') and (TTask.CurrentTask.Id = FTaskId) then
                TThread.Synchronize(nil, procedure
                begin
                  State := tmShow;
                  FHintText := Txt;
                end)
              else
                TThread.Synchronize(nil, procedure
                begin
                  State := tmStopped;
                end);
            end);
            FTaskId := Task.id;
            Task.Start;
          end;
        end
        else
          Inc(FDelay);
      end;
    tmShow:
      begin
        if FHintText = '' then begin
          State := tmStopped;
          Exit;
        end;
        var MaxWidth := Screen.MonitorFromPoint(FScreenPos).WorkareaRect.Width div 2;
        R := HintWindow.CalcHintRect(MaxWidth, fHintText, nil);
        R.Top := fScreenPos.Y;
        R.Left := fScreenPos.X;
        Inc(R.Bottom, R.Top);
        Inc(R.Right, R.Left);
        State := tmShowing;
        HintWindow.ActivateHint(R, '');
        FDelay := 0;
      end;
    tmShowing:
      begin
        GetCursorPos(P);
        bDelay := FAutoHide and (Delay > Application.HintHidePause);
        bPoint := not PtInRect(Area, P) and (FindVCLWindow(P) <> HintWindow);
        if bPoint or bDelay then
        begin
          if IsWindowVisible(HintWindow.Handle) then
            ShowWindow(HintWindow.Handle, SW_HIDE);
          FDelay := 0;
          if bPoint then
            HintWindow.HTLabel.Caption := '';
          State := tmStopped;
        end
        else
          Inc(FDelay);
      end;
    tmStopped:
      CancelHint;
  end;
end;

procedure TCodeHint.CancelHint;
begin
  FDelay := 0;
  FTaskId := 0;
  FHintText := '';
  if IsWindowVisible(HintWindow.Handle) then
    ShowWindow(HintWindow.Handle, SW_HIDE);
  State := tmStopped;
  TimerHint.Enabled := False;
  HintWindow.HtLabel.Caption := '';
end;

procedure TCodeHint.SetOnGetCodeHint(const Value: TGetCodeHintEvent);
begin
  CancelHint;
  fOnGetCodeHint := Value;
end;

function TCodeHint.GetHyperLinkClick: TJvHyperLinkClickEvent;
begin
  Result := HintWindow.HtLabel.OnHyperLinkClick;
end;

procedure TCodeHint.SetHyperLinkClick(const Value: TJvHyperLinkClickEvent);
begin
  HintWindow.HtLabel.OnHyperLinkClick := Value;
end;

//=== { TJvHTHintWindow } ====================================================

constructor TCodeHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HtLabel := TJvHTLabel.Create(Self);
  HtLabel.Parent := Self;
  HtLabel.SetBounds(2, 2, 0, 0);

  { TODO : Allow customization of these colors }
  HtLabel.Font.Name := DefaultCodeFontName;
  HtLabel.Font.Size := 10;
  HtLabel.Transparent := True;
end;

procedure TCodeHintWindow.Paint;
begin
  inherited;
end;

function TCodeHintWindow.CalcHintRect(MaxWidth: Integer;
  const AHint: THintString; AData: Pointer): TRect;
begin
  // HtLabel.Font.Color := StyleServices.GetSystemColor(Screen.HintFont.Color);   // #898
  HtLabel.Font.Color := StyleServices.GetSystemColor(clWindowText);
  HtLabel.Canvas.Pen.Color := HtLabel.Font.Color;
  HtLabel.Caption := AHint;
  Result := Bounds(0, 0, Min(HtLabel.Width, MaxWidth) + 6, HtLabel.Height + 2);
end;

initialization
  CodeHint := TCodeHint.Create(nil);
finalization
  FreeAndNil(CodeHint);
end.
