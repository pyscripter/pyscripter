{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvThreadDialog.PAS, released on 2004-12-06.

The Initial Developer of the Original Code is Jens Fudickar [jens dott fudickar att oratool dott de]
All Rights Reserved.

Contributor(s): Jens Fudickar [jens dott fudickar att oratool dott de].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id: JvThreadDialog.pas 11056 2006-11-29 10:47:33Z marquardt $

unit JvThreadDialog;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, Forms, Buttons, StdCtrls,
  {$IFDEF MSWINDOWS}
  Windows, Controls, ComCtrls, ExtCtrls,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  QWindows, QControls, QExtCtrls,
  {$ENDIF UNIX}
  JvTypes, JvComponentBase, JvThread, JvDynControlEngine;

type
  TJvThreadBaseDialogOptions = class(TJvCustomThreadDialogOptions)
  private
    FCancelButtonCaption: string;
    FCaption: string;
    FEnableCancelButton: Boolean;
    FInfoText: string;
    FInfoTextAlignment: TAlignment;
    FShowCancelButton: Boolean;
    FShowElapsedTime: Boolean;
  protected
    procedure SetCancelButtonCaption(Value: string);
    procedure SetCaption(Value: string);
    procedure SetEnableCancelButton(Value: Boolean);
    procedure SetInfoText(Value: string);
    procedure SetShowCancelButton(Value: Boolean);
    procedure SetShowElapsedTime(Value: Boolean);
  public
    constructor Create(AOwner: TJvCustomThreadDialog); override;
  published
    property CancelButtonCaption: string read FCancelButtonCaption
      write SetCancelButtonCaption;
    property Caption: string read FCaption write SetCaption;
    property EnableCancelButton: Boolean read FEnableCancelButton write SetEnableCancelButton default True;
    property InfoText: string read FInfoText write SetInfoText;
    property InfoTextAlignment: TAlignment read FInfoTextAlignment write FInfoTextAlignment default taLeftJustify;
    property ShowCancelButton: Boolean read FShowCancelButton write SetShowCancelButton default True;
    property ShowElapsedTime: Boolean read FShowElapsedTime write SetShowElapsedTime default True;
  end;

  TJvThreadAnimateDialogOptions = class(TJvThreadBaseDialogOptions)
  private
    FCommonAVI: TCommonAVI;
    FFileName: string;
    FResName: string;
  published
    property CommonAVI: TCommonAVI read FCommonAVI write FCommonAVI;
    property FileName: string read FFileName write FFileName;
    property ResName: string read FResName write FResName;
  end;

  TJvThreadAnimateDialog = class(TJvCustomThreadDialog)
  protected
    function CreateDialogOptions: TJvCustomThreadDialogOptions; override;
    function GetDialogOptions: TJvThreadAnimateDialogOptions;
    procedure SetDialogOptions(Value: TJvThreadAnimateDialogOptions);
  public
    function CreateThreadDialogForm(ConnectedThread: TJvThread): TJvCustomThreadDialogForm; override;
  published
    property DialogOptions: TJvThreadAnimateDialogOptions read GetDialogOptions write SetDialogOptions;
    property OnPressCancel;
  end;

  TJvThreadSimpleDialogOptions = class(TJvThreadBaseDialogOptions)
  private
    FShowProgressBar: Boolean;
    procedure SetShowProgressBar(const Value: Boolean);
  public
    constructor Create(AOwner: TJvCustomThreadDialog); override;
  published
    property ShowProgressBar: Boolean read FShowProgressBar write SetShowProgressBar default False;
  end;

  TJvThreadSimpleDialog = class(TJvCustomThreadDialog)
  private
    function GetDialogOptions: TJvThreadSimpleDialogOptions;
    procedure SetDialogOptions(Value: TJvThreadSimpleDialogOptions);
  protected
    function CreateDialogOptions: TJvCustomThreadDialogOptions; override;
  public
    function CreateThreadDialogForm(ConnectedThread: TJvThread): TJvCustomThreadDialogForm; override;
  published
    property DialogOptions: TJvThreadSimpleDialogOptions read GetDialogOptions write SetDialogOptions;
    property OnPressCancel;
  end;

  TJvDynControlEngineThreadDialogForm = class(TJvCustomThreadDialogForm)
  private
    function GetDynControlEngine: TJvDynControlEngine;
  protected
    property DynControlEngine: TJvDynControlEngine read GetDynControlEngine;
  end;

  TJvThreadSimpleDialogForm = class(TJvDynControlEngineThreadDialogForm)
  private
    FCancelBtn: TButton;
    FCancelButtonPanel: TWinControl;
    FCounter: Integer;
    FDefaultBorderWidth: Integer;
    FInfoText: TControl;
    FInfoTextPanel: TWinControl;
    FMainPanel: TWinControl;
    FProgressbar: TWinControl;
    FProgressbarPanel: TWinControl;
    FStartTime: TDateTime;
    FTimeText: TControl;
    FTimeTextPanel: TWinControl;
    function GetDialogOptions: TJvThreadSimpleDialogOptions;
    procedure SetDialogOptions(Value: TJvThreadSimpleDialogOptions);
  protected
    procedure CreateFormControls;
    procedure CreateTextPanel(AOwner: TComponent; AParent: TWinControl; var Panel: TWinControl;
      var Text: TControl; TextAlignment: TAlignment; const BaseName: string);
    procedure InitializeFormContents; override;
    procedure SetFormData;
    procedure SetFormHeightWidth;
    procedure UpdateFormContents; override;
  public
    property DialogOptions: TJvThreadSimpleDialogOptions read GetDialogOptions write SetDialogOptions;
  end;

  TJvThreadAnimateDialogForm = class(TJvDynControlEngineThreadDialogForm)
  private
    FAnimate: TAnimate;
    FAnimatePanel: TWinControl;
    FCancelBtn: TButton;
    FCancelButtonPanel: TWinControl;
    FDefaultBorderWidth: Integer;
    FInfoText: TControl;
    FInfoTextPanel: TWinControl;
    FMainPanel: TWinControl;
    FStartTime: TDateTime;
    FTimeText: TControl;
    FTimeTextPanel: TWinControl;
    function GetDialogOptions: TJvThreadAnimateDialogOptions;
    procedure SetDialogOptions(Value: TJvThreadAnimateDialogOptions);
  protected
    procedure CreateFormControls;
    procedure CreateTextPanel(AOwner: TComponent; AParent: TWinControl; var Panel: TWinControl;
      var Text: TControl; TextAlignment: TAlignment; const BaseName: string);
    procedure InitializeFormContents; override;
    procedure SetFormData;
    procedure SetFormHeightWidth;
    procedure UpdateFormContents; override;
  public
    property DialogOptions: TJvThreadAnimateDialogOptions read GetDialogOptions write SetDialogOptions;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jvcl.svn.sourceforge.net/svnroot/jvcl/branches/JVCL3_30_PREPARATION/run/JvThreadDialog.pas $';
    Revision: '$Revision: 11056 $';
    Date: '$Date: 2006-11-29 11:47:33 +0100 (mer., 29 nov. 2006) $';
    LogPath: 'JVCL\run'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  Dialogs, Graphics,
  JvResources, JvDynControlEngineIntf;

function Max(a, b: Integer): Integer;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

//=== { TJvThreadBaseDialogOptions } =========================================

constructor TJvThreadBaseDialogOptions.Create(AOwner: TJvCustomThreadDialog);
begin
  inherited Create(AOwner);
  FEnableCancelButton := True;
  FShowCancelButton := True;
  FShowElapsedTime := True;
  FCancelButtonCaption := RsButtonCancelCaption;
  FInfoTextAlignment := taLeftJustify;
end;

procedure TJvThreadBaseDialogOptions.SetCancelButtonCaption(Value: string);
begin
  FCancelButtonCaption := Value;
end;

procedure TJvThreadBaseDialogOptions.SetCaption(Value: string);
begin
  FCaption := Value;
end;

procedure TJvThreadBaseDialogOptions.SetEnableCancelButton(Value: Boolean);
begin
  FEnableCancelButton := Value;
end;

procedure TJvThreadBaseDialogOptions.SetInfoText(Value: string);
begin
  FInfoText := Value;
end;

procedure TJvThreadBaseDialogOptions.SetShowCancelButton(Value: Boolean);
begin
  FShowCancelButton := Value;
end;

procedure TJvThreadBaseDialogOptions.SetShowElapsedTime(Value: Boolean);
begin
  FShowElapsedTime := Value;
end;

//=== { TJvThreadSimpleDialog } ==============================================

function TJvThreadSimpleDialog.CreateDialogOptions: TJvCustomThreadDialogOptions;
begin
  Result := TJvThreadSimpleDialogOptions.Create(Self);
end;

function TJvThreadSimpleDialog.CreateThreadDialogForm(ConnectedThread: TJvThread): TJvCustomThreadDialogForm;
var
  ThreadDialogForm: TJvThreadSimpleDialogForm;
begin
  if DialogOptions.ShowDialog then
  begin
    ThreadDialogForm := TJvThreadSimpleDialogForm.CreateNewFormStyle(ConnectedThread,
      DialogOptions.FormStyle);
    ThreadDialogForm.DialogOptions := DialogOptions;
    ThreadDialogForm.OnPressCancel := OnPressCancel;
    ThreadDialogForm.CreateFormControls;
    Result := ThreadDialogForm;
  end
  else
    Result := nil;
end;

function TJvThreadSimpleDialog.GetDialogOptions: TJvThreadSimpleDialogOptions;
begin
  Result := TJvThreadSimpleDialogOptions(inherited DialogOptions);
end;

procedure TJvThreadSimpleDialog.SetDialogOptions(Value: TJvThreadSimpleDialogOptions);
begin
  inherited DialogOptions.Assign(Value);
end;

//=== { TJvThreadAnimateDialog } =============================================

function TJvThreadAnimateDialog.CreateDialogOptions: TJvCustomThreadDialogOptions;
begin
  Result := TJvThreadAnimateDialogOptions.Create(Self);
end;

function TJvThreadAnimateDialog.CreateThreadDialogForm(ConnectedThread: TJvThread):
TJvCustomThreadDialogForm;
var
  ThreadDialogForm: TJvThreadAnimateDialogForm;
begin
  if DialogOptions.ShowDialog then
  begin
    ThreadDialogForm := TJvThreadAnimateDialogForm.CreateNewFormStyle(ConnectedThread,
      DialogOptions.FormStyle);
    ThreadDialogForm.DialogOptions := DialogOptions;
    ThreadDialogForm.OnPressCancel := OnPressCancel;
    ThreadDialogForm.CreateFormControls;
    Result := ThreadDialogForm;
  end
  else
    Result := nil;
end;

function TJvThreadAnimateDialog.GetDialogOptions: TJvThreadAnimateDialogOptions;
begin
  Result := TJvThreadAnimateDialogOptions(inherited DialogOptions);
end;

procedure TJvThreadAnimateDialog.SetDialogOptions(Value: TJvThreadAnimateDialogOptions);
begin
  inherited DialogOptions.Assign(Value);
end;

//=== { TJvThreadSimpleDialogOptions } =======================================

constructor TJvThreadSimpleDialogOptions.Create(AOwner: TJvCustomThreadDialog);
begin
  inherited Create(AOwner);
  FShowProgressBar := False;
end;

procedure TJvThreadSimpleDialogOptions.SetShowProgressBar(const Value: Boolean);
begin
  FShowProgressBar := Value;
end;

procedure TJvThreadSimpleDialogForm.CreateTextPanel(AOwner: TComponent;
  AParent: TWinControl; var Panel: TWinControl; var Text: TControl;
  TextAlignment: TAlignment; const BaseName: string);
var
  ITmpPanel: IJvDynControlPanel;
  ITmpAlignment: IJvDynControlAlignment;
begin
  Panel := DynControlEngine.CreatePanelControl(AOwner, AParent,
    BaseName + 'Panel', '', alTop);
  if not Supports(Panel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  with ITmpPanel do
    ControlSetBorder(bvNone, bvNone, 0, bsNone, FDefaultBorderWidth);
  Text := DynControlEngine.CreateLabelControl(AOwner, Panel, BaseName + 'StaticText', '', nil);
  Text.Top := FDefaultBorderWidth;
  Text.Left := FDefaultBorderWidth;
  if Supports(Text, IJvDynControlAlignment, ITmpAlignment) then
    ITmpAlignment.ControlSetAlignment(TextAlignment);
end;

procedure TJvThreadSimpleDialogForm.CreateFormControls;
var
  ITmpPanel: IJvDynControlPanel;
  ITmpAlign: IJvDynControlAlign;
begin
  FDefaultBorderWidth := 3;
  FMainPanel := DynControlEngine.CreatePanelControl(Self, Self,
    'MainPanel', '', alClient);
  if not Supports(FMainPanel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  with ITmpPanel do
    ControlSetBorder(bvNone, bvNone, 0, bsNone, FDefaultBorderWidth);

  CreateTextPanel(Self, FMainPanel, FInfoTextPanel, FInfoText,
    DialogOptions.InfoTextAlignment, 'Info');
  CreateTextPanel(Self, FMainPanel, FTimeTextPanel, FTimeText, taCenter, 'Time');

  FProgressbarPanel := DynControlEngine.CreatePanelControl(Self,
    FMainPanel, 'ProgressbarPanel', '', alTop);
  if not Supports(FProgressbarPanel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  with ITmpPanel do
    ControlSetBorder(bvNone, bvNone, 0, bsNone, FDefaultBorderWidth);
  FProgressbar := DynControlEngine.CreateProgressbarControl(Self, FProgressbarPanel,
    'Progressbar');
  if Supports(FProgressbar, IJvDynControlAlign, ITmpAlign) then
    ITmpAlign.ControlSetAlign(alClient);
  FProgressbarPanel.Height := FProgressbar.Height + FDefaultBorderWidth*2;

  FCancelButtonPanel := DynControlEngine.CreatePanelControl(Self,
    FMainPanel, 'ButtonPanel', '', alTop);
  FCancelBtn := DynControlEngine.CreateButton(Self, FCancelButtonPanel,
    'CancelBtn', RsButtonCancelCaption, '', DefaultCancelBtnClick,
    True, True);
  with FCancelBtn do
  begin
    Anchors := [akTop];
    Top := FDefaultBorderWidth;
    FCancelButtonPanel.Height := FCancelBtn.Height + FDefaultBorderWidth*2;
  end;

  BorderIcons := [];
  BorderStyle := bsDialog;
  Caption := ' ';
  ClientHeight := 88;
  ClientWidth := 268;
  FormStyle := DialogOptions.FormStyle;
  {$IFDEF COMPILER7_UP}
  Position := poOwnerFormCenter;
  {$ELSE}
  Position := poScreenCenter;
  {$ENDIF COMPILER7_UP}

  SetFormData;
end;

procedure TJvThreadSimpleDialogForm.InitializeFormContents;
begin
  inherited InitializeFormContents;
  SetFormHeightWidth;
  FStartTime := Now;
  FCounter   := 0;
end;

procedure TJvThreadSimpleDialogForm.SetFormData;
var
  ITmpControl: IJvDynControl;
begin
  if Assigned(DialogOptions) then
  begin
    if Supports(FInfoText, IJvDynControl, ITmpControl) then
      ITmpControl.ControlSetCaption(DialogOptions.FInfoText);
    Caption := DialogOptions.Caption;
    FTimeTextPanel.Visible := DialogOptions.ShowElapsedTime;
    FCancelBtn.Enabled := DialogOptions.EnableCancelButton;
    FCancelButtonPanel.Visible := DialogOptions.ShowCancelButton;
    FProgressbarPanel.Visible := DialogOptions.ShowProgressBar;
  end;
end;

procedure TJvThreadSimpleDialogForm.SetFormHeightWidth;
var
  H, W: Integer;
  ITmpAutoSize: IJvDynControlAutoSize;
begin
  if Supports(FInfoText, IJvDynControlAutoSize, ITmpAutoSize) then
  begin
    ITmpAutoSize.ControlSetAutoSize(True);
    ITmpAutoSize.ControlSetAutoSize(False);
  end;
  if Supports(FTimeText, IJvDynControlAutoSize, ITmpAutoSize) then
  begin
    ITmpAutoSize.ControlSetAutoSize(True);
    ITmpAutoSize.ControlSetAutoSize(False);
  end;
  W := FInfoText.Width + 80;
  if W < 250 then
    W := 250;
  ClientWidth := W;
  FCancelBtn.Left := (FCancelButtonPanel.Width - FCancelBtn.Width) div 2;
  FInfoText.Width := FInfoTextPanel.Width-FDefaultBorderWidth*2;
  FInfoTextPanel.Height := FInfoText.Height+FDefaultBorderWidth*2;
  FTimeText.Width := FTimeTextPanel.Width-FDefaultBorderWidth*2;
  FTimeTextPanel.Height := FTimeText.Height+FDefaultBorderWidth*2;
  FProgressbar.Width := FProgressbarPanel.Width-FDefaultBorderWidth*2;
  H := FInfoTextPanel.Height;
  if FTimeTextPanel.Visible then
    H := H + FTimeTextPanel.Height;
  if FProgressbarPanel.Visible then
    H := H + FProgressbarPanel.Height;
  if FCancelButtonPanel.Visible then
    H := H + FCancelButtonPanel.Height;
  H := H + FDefaultBorderWidth*2;
  if ClientHeight <> H then
    ClientHeight := H;
end;

procedure TJvThreadSimpleDialogForm.UpdateFormContents;
var
  ITmpControl: IJvDynControl;
  ITmpProgressbar : IJvDynControlProgressbar;
begin
  inherited UpdateFormContents;
  FCounter := FCounter + 1;
  if Supports(FTimeText, IJvDynControl, ITmpControl) then
    ITmpControl.ControlSetCaption(FormatDateTime('hh:nn:ss', Now - FStartTime));
  if Supports(FProgressbar, IJvDynControlProgressbar, ITmpProgressbar) then
    ITmpProgressbar.ControlSetPosition((FCounter*10) mod 100);
  case FCounter mod 4 of
    0:
      Caption := DialogOptions.Caption + ' | ';
    1:
      Caption := DialogOptions.Caption + ' / ';
    2:
      Caption := DialogOptions.Caption + ' --';
    3:
      Caption := DialogOptions.Caption + ' \ ';
  end;
end;

function TJvThreadSimpleDialogForm.GetDialogOptions: TJvThreadSimpleDialogOptions;
begin
  Result := TJvThreadSimpleDialogOptions(inherited DialogOptions);
end;

procedure TJvThreadSimpleDialogForm.SetDialogOptions(Value:
  TJvThreadSimpleDialogOptions);
begin
  inherited DialogOptions := Value;
end;

procedure TJvThreadAnimateDialogForm.CreateFormControls;
var
  ITmpPanel: IJvDynControlPanel;
begin
  FDefaultBorderWidth:=3;
  FMainPanel := DynControlEngine.CreatePanelControl(Self, Self,
    'MainPanel', '', alClient);
  if not Supports(FMainPanel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  with ITmpPanel do
    ControlSetBorder(bvNone, bvNone, 0, bsNone, FDefaultBorderWidth);

  CreateTextPanel(Self, FMainPanel, FInfoTextPanel, FInfoText,
    DialogOptions.InfoTextAlignment,  'Info');

  FAnimatePanel := DynControlEngine.CreatePanelControl(Self, FMainPanel,
    'AnimatePanel', '', alTop);
  if not Supports(FAnimatePanel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  with ITmpPanel do
    ControlSetBorder(bvNone, bvNone, 0, bsNone, FDefaultBorderWidth);

  FAnimate := TAnimate.Create(Self);
  with FAnimate do
  begin
    Parent := FAnimatePanel;
    Top := FDefaultBorderWidth;
    Left := FDefaultBorderWidth;
    AutoSize := True;
    CommonAVI := TJvThreadAnimateDialogOptions(DialogOptions).CommonAVI;
    FileName := TJvThreadAnimateDialogOptions(DialogOptions).FileName;
    ResName := TJvThreadAnimateDialogOptions(DialogOptions).ResName;
    FAnimatePanel.Height := Height + FDefaultBorderWidth*2;
  end;

  CreateTextPanel(Self, FMainPanel, FTimeTextPanel, FTimeText, taCenter,  'Time');

  FCancelButtonPanel := DynControlEngine.CreatePanelControl(Self,
    FMainPanel, 'ButtonPanel', '', alTop);
  FCancelBtn := DynControlEngine.CreateButton(Self, FCancelButtonPanel,
    'CancelBtn', RsButtonCancelCaption, '', DefaultCancelBtnClick,
    True, True);
  with FCancelBtn do
  begin
    Anchors := [akTop];
    Top := FDefaultBorderWidth;
    FCancelButtonPanel.Height := FCancelBtn.Height + FDefaultBorderWidth*2;
  end;

  BorderIcons := [];
  BorderStyle := bsDialog;
  Caption := ' ';
  FormStyle := DialogOptions.FormStyle;
  {$IFDEF COMPILER7_UP}
  Position := poOwnerFormCenter;
  {$ELSE}
  Position := poScreenCenter;
  {$ENDIF COMPILER7_UP}
  SetFormData;
end;

procedure TJvThreadAnimateDialogForm.CreateTextPanel(AOwner: TComponent;
  AParent: TWinControl; var Panel: TWinControl; var Text: TControl;
  TextAlignment: TAlignment; const BaseName: string);
var
  ITmpPanel: IJvDynControlPanel;
  ITmpAlignment: IJvDynControlAlignment;
begin
  Panel := DynControlEngine.CreatePanelControl(AOwner, AParent,
    BaseName + 'Panel', '', alTop);
  if not Supports(Panel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  with ITmpPanel do
    ControlSetBorder(bvNone, bvNone, 0, bsNone, 3);
  Text := DynControlEngine.CreateLabelControl(AOwner,
    Panel, BaseName + 'StaticText', '', nil);
  Text.Top := FDefaultBorderWidth;
  Text.Left := FDefaultBorderWidth;
  if Supports(Text, IJvDynControlAlignment, ITmpAlignment) then
    ITmpAlignment.ControlSetAlignment(TextAlignment);
end;

procedure TJvThreadAnimateDialogForm.InitializeFormContents;
begin
  inherited InitializeFormContents;
  SetFormHeightWidth;
  FStartTime := Now;
  FAnimate.Active := True;
end;

procedure TJvThreadAnimateDialogForm.SetFormData;
var
  ITmpControl: IJvDynControl;
begin
  if Assigned(DialogOptions) then
  begin
    if Supports(FInfoText, IJvDynControl, ITmpControl) then
      ITmpControl.ControlSetCaption(DialogOptions.FInfoText);
    if Supports(FTimeText, IJvDynControl, ITmpControl) then
      ITmpControl.ControlSetCaption(FormatDateTime('hh:nn:ss', 0));
    Caption := DialogOptions.Caption;
    FInfoTextPanel.Visible := DialogOptions.InfoText <> '';
    FAnimatePanel.Visible := FileExists(FAnimate.FileName) or
      (FAnimate.CommonAVI <> aviNone) or (FAnimate.ResName <> '');
    FTimeTextPanel.Visible := DialogOptions.ShowElapsedTime;
    FCancelBtn.Enabled := DialogOptions.EnableCancelButton;
    FCancelButtonPanel.Visible := DialogOptions.ShowCancelButton;
  end;
end;

procedure TJvThreadAnimateDialogForm.SetFormHeightWidth;
var
  H, W: Integer;
  ITmpAutoSize:  IJvDynControlAutoSize;
begin
  H := 0;
  W := 200;
  if Supports(FInfoText, IJvDynControlAutoSize, ITmpAutoSize) then
  begin
    ITmpAutoSize.ControlSetAutoSize(True);
    ITmpAutoSize.ControlSetAutoSize(False);
  end;
  if Supports(FTimeText, IJvDynControlAutoSize, ITmpAutoSize) then
  begin
    ITmpAutoSize.ControlSetAutoSize(True);
    ITmpAutoSize.ControlSetAutoSize(False);
  end;

  if FInfoTextPanel.Visible then
    W := Max(FInfoText.Width + 80, W);
  if FAnimatePanel.Visible then
    W := Max(W, FAnimate.Width + 20);

  ClientWidth := W;

  FCancelBtn.Left := (FCancelButtonPanel.Width - FCancelBtn.Width) div 2;
  FAnimate.Left   := (FAnimatePanel.Width - FAnimate.Width) div 2;
  FInfoText.Width := FInfoTextPanel.Width-FDefaultBorderWidth*2;
  FInfoTextPanel.Height := FInfoText.Height+FDefaultBorderWidth*2;
  FTimeText.Width := FTimeTextPanel.Width-FDefaultBorderWidth*2;
  FTimeTextPanel.Height := FTimeText.Height+FDefaultBorderWidth*2;

  if FInfoTextPanel.Visible then
  begin
    FInfoTextPanel.Top := h;
    H := H + FInfoTextPanel.Height;
  end;
  if FAnimatePanel.Visible then
  begin
    FAnimatePanel.Top := h;
    H := H + FAnimatePanel.Height;
  end;
  if FTimeTextPanel.Visible then
  begin
    FTimeTextPanel.Top := h;
    H := H + FTimeTextPanel.Height;
  end;
  if FCancelButtonPanel.Visible then
  begin
    FCancelButtonPanel.Top := h;
    H := H + FCancelButtonPanel.Height;
  end;
  H := H + 6;
  ClientHeight := H;
end;

procedure TJvThreadAnimateDialogForm.UpdateFormContents;
var
  ITmpControl: IJvDynControl;
begin
  inherited UpdateFormContents;
  if Supports(FTimeText, IJvDynControl, ITmpControl) then
    ITmpControl.ControlSetCaption(FormatDateTime('hh:nn:ss', Now - FStartTime));
end;

function TJvThreadAnimateDialogForm.GetDialogOptions: TJvThreadAnimateDialogOptions;
begin
  Result := TJvThreadAnimateDialogOptions(inherited DialogOptions);
end;

procedure TJvThreadAnimateDialogForm.SetDialogOptions(Value:
  TJvThreadAnimateDialogOptions);
begin
  inherited DialogOptions := Value;
end;

function TJvDynControlEngineThreadDialogForm.GetDynControlEngine: TJvDynControlEngine;
begin
  Result := DefaultDynControlEngine;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

