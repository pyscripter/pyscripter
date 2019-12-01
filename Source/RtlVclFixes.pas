{-----------------------------------------------------------------------------
 Unit Name: RtlVclFixes
 Author:    Kiriakos Vlahos
 Date:      14-Nov-2019
 Purpose:   Various Fixes for Rtl-Vcl bugs
 History:
-----------------------------------------------------------------------------}

unit RtlVclFixes;

interface

implementation
Uses
  Winapi.Windows,
  Winapi.Wincodec,
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Consts,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  DDetours;

{$REGION 'Fix TWICImage - https://quality.embarcadero.com/browse/RSP-26621'}
type
  TCreateWICBitmap = procedure of object;

var
  Trampoline_TWICImage_CreateWICBitmap : TCreateWICBitmap;
  Detour_TWICImage_CreateWICBitmap : TCreateWICBitmap;


{ TWICImageHelper }
type
  TWICImageHelper = class helper for TWICImage
    function GetCreateWICBitmapAddr: Pointer;
    procedure Detour_CreateWICBitmap;
  end;

procedure TWICImageHelper.Detour_CreateWICBitmap;
var
  PixelFormat: TGUID;
  BitmapInfo: TBitmapInfo;
  Buffer: array of byte;
  hBMP: HBITMAP;
begin
  with Self do
  begin
    FData.Clear;
    FFormatChanged := True;

    if FBitmap.AlphaFormat = afDefined then
      PixelFormat := GUID_WICPixelFormat32bppPBGRA
    else
      PixelFormat := GUID_WICPixelFormat32bppBGR;

    FBitmap.PixelFormat := pf32bit;

    FWidth := FBitmap.Width;
    FHeight := FBitmap.Height;

    SetLength(Buffer, FWidth * 4 * FHeight);

    FillChar(BitmapInfo, sizeof(BitmapInfo), 0);
    BitmapInfo.bmiHeader.biSize := SizeOf(BitmapInfo);
    BitmapInfo.bmiHeader.biWidth := FWidth;
    BitmapInfo.bmiHeader.biHeight := -FHeight;
    BitmapInfo.bmiHeader.biPlanes := 1;
    BitmapInfo.bmiHeader.biBitCount := 32;
    // Forces evaluation of Bitmap.Handle before Bitmap.Canvas.Handle
    hBMP := FBitmap.Handle;
    GetDIBits(FBitmap.Canvas.Handle,  hBMP, 0, FHeight, @Buffer[0],
      BitmapInfo, DIB_RGB_COLORS);

    FImagingFactory.CreateBitmapFromMemory(FWidth, FHeight, PixelFormat,
      FWidth * 4, Length(Buffer), @Buffer[0], FWicBitmap);
  end;
end;

function TWICImageHelper.GetCreateWICBitmapAddr: Pointer;
var
  MethodPtr: TCreateWICBitmap;
begin
  with Self do MethodPtr := CreateWICBitmap;
  Result := TMethod(MethodPtr).Code;
end;
{$ENDREGION}

{$REGION 'Fix InputQuery - https://quality.embarcadero.com/browse/RSP-27077'}
type
  TInputQuery = function (const ACaption: string; const APrompts: array of string; var AValues: array of string; CloseQueryFunc: TInputCloseQueryFunc = nil): Boolean;

  TInputQueryForm = class(TForm)
  public
    FCloseQueryFunc: TFunc<Boolean>;
    function CloseQuery: Boolean; override;
  end;

var
  Original_InputQuery: TInputQuery;
  Trampoline_InputQuery: TInputQuery;
  Detour_InputQuery: TInputQuery;

function TInputQueryForm.CloseQuery: Boolean;
begin
  Result := (ModalResult = mrCancel) or (not Assigned(FCloseQueryFunc)) or FCloseQueryFunc();
end;

function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;

function GetTextBaseline(AControl: TControl; ACanvas: TCanvas): Integer;
var
  tm: TTextMetric;
  ClientRect: TRect;
  Ascent: Integer;
begin
  ClientRect := AControl.ClientRect;
  GetTextMetrics(ACanvas.Handle, tm);
  Ascent := tm.tmAscent + 1;
  Result := ClientRect.Top + Ascent;
  Result := AControl.Parent.ScreenToClient(AControl.ClientToScreen(TPoint.Create(0, Result))).Y - AControl.Top;
end;

function FixedInputQuery(const ACaption: string; const APrompts: array of string; var AValues: array of string; CloseQueryFunc: TInputCloseQueryFunc): Boolean;
var
  I, J: Integer;
  Form: TInputQueryForm;
  Prompt: TLabel;
  Edit: TEdit;
  DialogUnits: TPoint;
  PromptCount, CurPrompt: Integer;
  MaxPromptWidth: Integer;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;

  function GetPromptCaption(const ACaption: string): string;
  begin
    if (Length(ACaption) > 1) and (ACaption[1] < #32) then
      Result := Copy(ACaption, 2, MaxInt)
    else
      Result := ACaption;
  end;

  function GetMaxPromptWidth(Canvas: TCanvas): Integer;
  var
    I: Integer;
    LLabel: TLabel;
  begin
    Result := 0;
    // Use a TLabel rather than an API such as GetTextExtentPoint32 to
    // avoid differences in handling characters such as line breaks.
    LLabel := TLabel.Create(nil);
    try
      LLabel.Font := Canvas.Font;
      for I := 0 to PromptCount - 1 do
      begin
        LLabel.Caption := GetPromptCaption(APrompts[I]);
        Result := Max(Result, LLabel.Width + DialogUnits.X);
      end;
    finally
      LLabel.Free;
    end;
  end;

  function GetPasswordChar(const ACaption: string): Char;
  begin
    if (Length(ACaption) > 1) and (ACaption[1] < #32) then
      Result := '*'
    else
      Result := #0;
  end;

begin
  if Length(AValues) < Length(APrompts) then
    raise EInvalidOperation.CreateRes(@SPromptArrayTooShort);
  PromptCount := Length(APrompts);
  if PromptCount < 1 then
    raise EInvalidOperation.CreateRes(@SPromptArrayEmpty);
  Result := False;
  Form := TInputQueryForm.CreateNew(Application);
  with Form do
    try
      FCloseQueryFunc :=
        function: Boolean
        var
          I, J: Integer;
          LValues: array of string;
          Control: TControl;
        begin
          Result := True;
          if Assigned(CloseQueryFunc) then
          begin
            SetLength(LValues, PromptCount);
            J := 0;
            for I := 0 to Form.ControlCount - 1 do
            begin
              Control := Form.Controls[I];
              if Control is TEdit then
              begin
                LValues[J] := TEdit(Control).Text;
                Inc(J);
              end;
            end;
            Result := CloseQueryFunc(LValues);
          end;
        end;
      //KV
      var LPPI := Screen.PixelsPerInch;
      if Screen.ActiveForm <> nil then
        LPPI := Screen.ActiveForm.CurrentPPI
      else
        if Application.MainForm <> nil then
          LPPI := Application.MainForm.CurrentPPI;
      ScaleForPPI(LPPI);
      Font.Assign(Screen.MessageFont);
      Font.Height := Muldiv(Font.Height, CurrentPPI, Screen.PixelsPerInch);
      //KV
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      MaxPromptWidth := GetMaxPromptWidth(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      ClientWidth := MulDiv(180 + MaxPromptWidth, DialogUnits.X, 4);
      PopupMode := pmAuto;
      Position := poScreenCenter;
      CurPrompt := MulDiv(8, DialogUnits.Y, 8);
      Edit := nil;
      for I := 0 to PromptCount - 1 do
      begin
        Prompt := TLabel.Create(Form);
        with Prompt do
        begin
          Parent := Form;
          Caption := GetPromptCaption(APrompts[I]);
          Left := MulDiv(8, DialogUnits.X, 4);
          Top := CurPrompt;
          Constraints.MaxWidth := MaxPromptWidth;
          WordWrap := True;
        end;
        Edit := TEdit.Create(Form);
        with Edit do
        begin
          Parent := Form;
          PasswordChar := GetPasswordChar(APrompts[I]);
          Left := Prompt.Left + MaxPromptWidth;
          Top := Prompt.Top + Prompt.Height - DialogUnits.Y -
            (GetTextBaseline(Edit, Canvas) - GetTextBaseline(Prompt, Canvas));
          Width := Form.ClientWidth - Left - MulDiv(8, DialogUnits.X, 4);
          MaxLength := 255;
          Text := AValues[I];
          SelectAll;
          Prompt.FocusControl := Edit;
        end;
        CurPrompt := Edit.Top + Edit.Height + 5;
      end;
      ButtonTop := Edit.Top + Edit.Height + 15;
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgOK;
        ModalResult := mrOk;
        Default := True;
        SetBounds(Form.ClientWidth - (ButtonWidth + MulDiv(8, DialogUnits.X, 4)) * 2, ButtonTop, ButtonWidth, ButtonHeight);
      end;
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgCancel;
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(Form.ClientWidth - (ButtonWidth + MulDiv(8, DialogUnits.X, 4)), ButtonTop, ButtonWidth, ButtonHeight);
        Form.ClientHeight := Top + Height + MulDiv(13, LPPI, 96);
      end;
      if ShowModal = mrOk then
      begin
        J := 0;
        for I := 0 to ControlCount - 1 do
          if Controls[I] is TEdit then
          begin
            Edit := TEdit(Controls[I]);
            AValues[J] := Edit.Text;
            Inc(J);
          end;
        Result := True;
      end;
    finally
      Form.Free;
    end;
end;
{$ENDREGION}

initialization
 Detour_TWICImage_CreateWICBitmap := TWICImage(nil).Detour_CreateWICBitmap;
 TMethod(Trampoline_TWICImage_CreateWICBitmap).Code :=
   InterceptCreate(TWICImage(nil).GetCreateWICBitmapAddr,
   TMethod(Detour_TWICImage_CreateWICBitmap).Code);

  Original_InputQuery := Vcl.Dialogs.InputQuery;
  Detour_InputQuery := FixedInputQuery;
  Trampoline_InputQuery := TInputQuery(InterceptCreate(@Original_InputQuery, @Detour_InputQuery));

finalization
 InterceptRemove(TMethod(Trampoline_TWICImage_CreateWICBitmap).Code);
 InterceptRemove(@Trampoline_InputQuery);
end.
