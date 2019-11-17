{-----------------------------------------------------------------------------
 Unit Name: VCL.Styles.DPIAware
 Author:    PyScripter  (https://github.com/pyscripter)
 Date:      13-Nov-2017
 Purpose:   Use VCL Styles in DPI Aware applications by scaling styles
 History:
-----------------------------------------------------------------------------}
{
  To use the unit just add it to the implementation uses statement of the main form and add
  the following code to the FormCreate handler.

  procedure TFrmMain.FormCreate(Sender: TObject);
  Var
    StyleDPIAwareness : TStyleDPIAwareness;
  begin
    StyleDPIAwareness := TStyleDPIAwareness.Create(Self);
    StyleDPIAwareness.Parent := Self;

  By default, styles are scaled to whatever scaling factor results for Screen.PixelsPerInch.
  Most of the styles would work fine, but a few may show some visual defects.
  You can scale the styles at multiples of 100% by adding the line, before created the control:

  TStyleDPIAwareness.RoundScalingFactor := True;

  Limitations:
    Does not support perMonitor DPI Awareness.
    You need to set DPI Awareness to System.
}

unit VCL.Styles.DPIAware;

interface
uses
  Winapi.Windows, WinAPI.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Themes, Vcl.Styles;

Type
  TStyleDPIAwareness = class(TControl)
  private
    FScaledStyles : TStringList;
   protected
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    procedure RecreateForms;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ScaleStyle(Style : TCustomStyleServices);
    {Rounds the scaling factorto the nearest 100%}
    class var  RoundScalingFactor : Boolean;
    class var UseCustomScalingFactor : Boolean;
    class var CustomPPI : integer;
  end;

implementation

Uses
  System.Rtti,
  Vcl.SysStyles,
  {$IFDEF VER330} // RAD Studio 10.3
  DDetours,
  {$ENDIF VER330}
  uCommonFunctions;

{TBitmapHelper}

type TBitmapHelper = class helper for TBitmap
  procedure PublicUnPreMultiplyAlpha;
end;

procedure TBitmapHelper.PublicUnPreMultiplyAlpha;
begin
  with Self do UnPreMultiplyAlpha;
end;

{ TStyleDPIAwareness }

procedure TStyleDPIAwareness.CMStyleChanged(var Message: TMessage);
begin
  ScaleStyle(TStyleManager.ActiveStyle);
end;

constructor TStyleDPIAwareness.Create(AOwner: TComponent);
begin
  inherited;
  FScaledStyles := TStringList.Create;
  FScaledStyles.Sorted := False;

  ScaleStyle(TStyleManager.ActiveStyle);
end;

destructor TStyleDPIAwareness.Destroy;
begin
  FScaledStyles.Free;
  inherited;
end;

procedure TStyleDPIAwareness.RecreateForms;
Var
  i : Integer;
begin
  for i := 0 to Screen.FormCount - 1 do
    Screen.Forms[i].Perform(CM_RECREATEWND, 0, 0);
end;

procedure TStyleDPIAwareness.ScaleStyle(Style: TCustomStyleServices);
Var
  DPI : integer;
  SeStyle : TObject;
  SeStyleSource : TObject;
  BitmapList : TList;
  BitMap : TBitmap;
  StyleObjectList : Tlist;
  i : integer;
  StyleObject : TComponent;

  procedure ProcessBitmapLink(BL : TObject);
  Var
    BLType : TRTTIType;
  begin
    BLType := TRttiContext.Create.GetType(BL.ClassType);
    BLType.GetProperty('Bottom').SetValue(BL,  mulDiv(BLType.GetProperty('Bottom').GetValue(BL).AsInteger, DPI, 96));
    BLType.GetProperty('Right').SetValue(BL,  mulDiv(BLType.GetProperty('Right').GetValue(BL).AsInteger, DPI, 96));
    BLType.GetProperty('Left').SetValue(BL,  mulDiv(BLType.GetProperty('Left').GetValue(BL).AsInteger, DPI, 96));
    BLType.GetProperty('Top').SetValue(BL,  mulDiv(BLType.GetProperty('Top').GetValue(BL).AsInteger, DPI, 96));
  end;

  procedure ProcessSO(aSO : TComponent;  aSOType : TRTTIType);
  begin
    aSOType.GetProperty('Top').SetValue(aSO,  mulDiv(aSOType.GetProperty('Top').GetValue(aSO).AsInteger, DPI, 96));
    aSOType.GetProperty('Left').SetValue(aSO,  mulDiv(aSOType.GetProperty('Left').GetValue(aSO).AsInteger, DPI, 96));
    aSOType.GetProperty('Width').SetValue(aSO,  mulDiv(aSOType.GetProperty('Width').GetValue(aSO).AsInteger, DPI, 96));
    aSOType.GetProperty('Height').SetValue(aSO,  mulDiv(aSOType.GetProperty('Height').GetValue(aSO).AsInteger, DPI, 96));
    aSOType.GetProperty('MarginTop').SetValue(aSO,  mulDiv(aSOType.GetProperty('MarginTop').GetValue(aSO).AsInteger, DPI, 96));
    aSOType.GetProperty('MarginLeft').SetValue(aSO,  mulDiv(aSOType.GetProperty('MarginLeft').GetValue(aSO).AsInteger, DPI, 96));
    aSOType.GetProperty('MarginBottom').SetValue(aSO,  mulDiv(aSOType.GetProperty('MarginBottom').GetValue(aSO).AsInteger, DPI, 96));
    aSOType.GetProperty('MarginRight').SetValue(aSO,  mulDiv(aSOType.GetProperty('MarginRight').GetValue(aSO).AsInteger, DPI, 96));
    aSOType.GetProperty('TextMarginTop').SetValue(aSO,  mulDiv(aSOType.GetProperty('TextMarginTop').GetValue(aSO).AsInteger, DPI, 96));
    aSOType.GetProperty('TextMarginLeft').SetValue(aSO,  mulDiv(aSOType.GetProperty('TextMarginLeft').GetValue(aSO).AsInteger, DPI, 96));
    aSOType.GetProperty('TextMarginRight').SetValue(aSO,  mulDiv(aSOType.GetProperty('TextMarginRight').GetValue(aSO).AsInteger, DPI, 96));
  end;

  procedure ProcessStyleObject(SO : TComponent);
  var
    i : integer;
    ChildSo : TComponent;
    SOType : TRTTIType;
    BitmapLink : TObject;
  begin
      SOType := TRttiContext.Create.GetType(SO.ClassType);
      ProcessSO(SO, SOType);

    if So.ClassName = 'TSeBitmapObject' then begin
      BitmapLink := TRttiContext.Create.GetType(SO.ClassType).GetField('FBitmap').GetValue(SO).AsObject;;
      ProcessBitmapLink(BitmapLink);
    end;

    if So.ClassName = 'TSeActiveBitmap' then begin
      BitmapLink := TRttiContext.Create.GetType(SO.ClassType).GetField('FBitmap').GetValue(SO).AsObject;;
      ProcessBitmapLink(BitmapLink);
      BitmapLink := TRttiContext.Create.GetType(SO.ClassType).GetField('FActiveBitmap').GetValue(SO).AsObject;;
      ProcessBitmapLink(BitmapLink);
    end;

    if So.ClassName = 'TSeSystemButton' then begin
      // Shift the form title to the right
      if SO.Name = 'btnSysMenu' then
        SOType.GetProperty('Width').SetValue(SO,  MulDiv(28, DPI, 96));
      BitmapLink := TRttiContext.Create.GetType(SO.ClassType).GetField('FBitmap').GetValue(SO).AsObject;;
      ProcessBitmapLink(BitmapLink);
      BitmapLink := TRttiContext.Create.GetType(SO.ClassType).GetField('FActiveBitmap').GetValue(SO).AsObject;;
      ProcessBitmapLink(BitmapLink);
      BitmapLink := TRttiContext.Create.GetType(SO.ClassType).GetField('FBitmapPressed').GetValue(SO).AsObject;;
      ProcessBitmapLink(BitmapLink);
      BitmapLink := TRttiContext.Create.GetType(SO.ClassType).GetField('FBitmapHot').GetValue(SO).AsObject;;
      ProcessBitmapLink(BitmapLink);
    end;

   if So.ClassName = 'TSeButtonObject' then begin
      BitmapLink := TRttiContext.Create.GetType(SO.ClassType).GetField('FBitmap').GetValue(SO).AsObject;;
      ProcessBitmapLink(BitmapLink);
      BitmapLink := TRttiContext.Create.GetType(SO.ClassType).GetField('FBitmapFocused').GetValue(SO).AsObject;;
      ProcessBitmapLink(BitmapLink);
      BitmapLink := TRttiContext.Create.GetType(SO.ClassType).GetField('FBitmapHot').GetValue(SO).AsObject;;
      ProcessBitmapLink(BitmapLink);
      BitmapLink := TRttiContext.Create.GetType(SO.ClassType).GetField('FBitmapPressed').GetValue(SO).AsObject;;
      ProcessBitmapLink(BitmapLink);
      BitmapLink := TRttiContext.Create.GetType(SO.ClassType).GetField('FBitmapDisabled').GetValue(SO).AsObject;;
      ProcessBitmapLink(BitmapLink);
    end;

     for i := 0 to SO.ComponentCount - 1 do begin
       ChildSo := SO.Components[i];
       ProcessStyleObject(ChildSo);
     end;
  end;

begin
  // Check if Style already scaled then
  if FScaledStyles.IndexOf(Style.Name) >= 0 then
    Exit;

  if UseCustomScalingFactor then
    DPI := CustomPPI
  else begin
    DPI := Screen.PixelsPerInch;
    if RoundScalingFactor then
      DPI := Round(DPI / 96) * 96;
  end;

  if (Style = TStyleManager.SystemStyle) or  (DPI <= 120) then
    Exit;

  SeStyle := TRttiContext.Create.GetType(Style.ClassType).GetField('FSource').GetValue(Style).AsObject;
  SeStyleSource := TRttiContext.Create.GetType(SeStyle.ClassType).GetField('FCleanCopy').GetValue(SeStyle).AsObject;
  BitMapList := TRttiContext.Create.GetType(SeStyleSource.ClassType).GetField('FBitmaps').GetValue(SeStyleSource).AsObject as TList;

  if BitMapList.Count = 1 then begin
    Bitmap := TObject(BitmapList[0]) as TBitmap;

    if Bitmap.Width mod 2 = 1 then Bitmap.Width := Bitmap.Width - 1;
    // It appears that the bitmap is premultiplied but has AlphaFormat afIgnored!
    Bitmap.PublicUnPreMultiplyAlpha;
    ResizeBitmap(Bitmap, MulDiv(Bitmap.Width, DPI, 96), Muldiv(Bitmap.Height, DPI, 96));

    StyleObjectList := TRttiContext.Create.GetType(SeStyleSource.ClassType).GetField('FObjects').GetValue(SeStyleSource).AsObject as TList;
    for i := 0 to StyleObjectList.Count -1 do begin
      StyleObject := TObject(StyleObjectList[i]) as TComponent;
      ProcessStyleObject(StyleObject);
    end;
   TRttiContext.Create.GetType(SeStyle.ClassType).GetMethod('ResetStyle').Invoke(SeStyle, []);

  end;
  FScaledStyles.Add(Style.Name);
  if Style = TStyleManager.ActiveStyle then
    RecreateForms;
end;
{$IFDEF VER330} // RAD Studio 10.3
  type
   TGetBorderSize = function: TRect of object;

   TFormStyleHookFix = class helper for TFormStyleHook
     procedure SetStretchedCaptionInc(Value : Integer);
     function GetBorderSizeAddr: Pointer;
     function Detour_GetBorderSize: TRect;
   end;

var
  Trampoline_TFormStyleHook_GetBorderSize : TGetBorderSize;
  Detour_TFormStyleHook_GetBorderSize : TGetBorderSize;


{ TFormStyleHookFix }

function TFormStyleHookFix.GetBorderSizeAddr: Pointer;
var
  MethodPtr: TGetBorderSize;
begin
  with Self do MethodPtr := GetBorderSize;
  Result := TMethod(MethodPtr).Code;
end;

procedure TFormStyleHookFix.SetStretchedCaptionInc(Value: Integer);
begin
  with Self do FStretchedCaptionInc := Value;
end;

function TFormStyleHookFix.Detour_GetBorderSize: TRect;
var
  MethodPtr: TGetBorderSize;
begin
  TMethod(MethodPtr).Code := TMethod(Trampoline_TFormStyleHook_GetBorderSize).Code;
  TMethod(MethodPtr).Data := Pointer(Self);
  Result := MethodPtr;
  Self.SetStretchedCaptionInc(0);
  if (Screen.PixelsPerInch > 96) then
    Result.Top := MulDiv(Result.Top, 96, Screen.PixelsPerInch);
end;

type
   TSysDialogStyleHookFix = class helper for TSysDialogStyleHook
     procedure SetStretchedCaptionInc(Value : Integer);
     function GetBorderSizeAddr: Pointer;
     function Detour_GetBorderSize: TRect;
   end;

var
  Trampoline_TSysDialogStyleHook_GetBorderSize : TGetBorderSize;
  Detour_TSysDialogStyleHook_GetBorderSize : TGetBorderSize;


{ TSysDialogStyleHookFix }

function TSysDialogStyleHookFix.GetBorderSizeAddr: Pointer;
var
  VMT : NativeInt;
  MethodPtr: TGetBorderSize;
begin
  //  GetBorderSize is virtual
  //  Adjust Self to point to the VMT
  VMT := NativeInt(TSysDialogStyleHook);
  Self := TSysDialogStyleHook(@VMT);

  with Self do MethodPtr := GetBorderSize;
  Result := TMethod(MethodPtr).Code;
end;

procedure TSysDialogStyleHookFix.SetStretchedCaptionInc(Value: Integer);
begin
  with Self do FStretchedCaptionInc := Value;
end;

function TSysDialogStyleHookFix.Detour_GetBorderSize: TRect;
var
  MethodPtr: TGetBorderSize;
begin
  TMethod(MethodPtr).Code := TMethod(Trampoline_TSysDialogStyleHook_GetBorderSize).Code;
  TMethod(MethodPtr).Data := Pointer(Self);
  Result := MethodPtr;
  Self.SetStretchedCaptionInc(0);
  if (Screen.PixelsPerInch > 96) then
    Result.Top := MulDiv(Result.Top, 96, Screen.PixelsPerInch);
end;

initialization
 Detour_TFormStyleHook_GetBorderSize := TFormStyleHook(nil).Detour_GetBorderSize;
 TMethod(Trampoline_TFormStyleHook_GetBorderSize).Code :=
   InterceptCreate(TFormStyleHook(nil).GetBorderSizeAddr,
   TMethod(Detour_TFormStyleHook_GetBorderSize).Code);

 Detour_TSysDialogStyleHook_GetBorderSize := TSysDialogStyleHook(nil).Detour_GetBorderSize;
 TMethod(Trampoline_TSysDialogStyleHook_GetBorderSize).Code :=
   InterceptCreate(TSysDialogStyleHook(nil).GetBorderSizeAddr,
   TMethod(Detour_TSysDialogStyleHook_GetBorderSize).Code);
finalization
 InterceptRemove(TMethod(Trampoline_TFormStyleHook_GetBorderSize).Code);
 InterceptRemove(TMethod(Trampoline_TSysDialogStyleHook_GetBorderSize).Code);
{$ENDIF VER330}
end.
