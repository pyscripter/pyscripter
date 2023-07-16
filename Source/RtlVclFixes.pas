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
  Vcl.ImgList,
  Vcl.Themes,
  DDetours;

{$REGION 'Fix TWICImage - https://quality.embarcadero.com/browse/RSP-26621'}
{$IF CompilerVersion < 34}
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
{$ENDIF}
{$ENDREGION}

{$REGION 'Fix InputQuery - https://quality.embarcadero.com/browse/RSP-27077'}
{$IF CompilerVersion < 35}
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
  ButtonsRightOffset: Integer;

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
      Font.Height := Muldiv(Font.Height, LPPI, Screen.PixelsPerInch);
      //KV
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      MaxPromptWidth := GetMaxPromptWidth(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      ClientWidth := MulDiv(180 , DialogUnits.X, 4) + MaxPromptWidth;  //KV
      if TStyleManager.IsCustomStyleActive then
        ClientWidth := ClientWidth + 4;
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
          if TStyleManager.IsCustomStyleActive then
            Width := Width - 4;
          MaxLength := 255;
          Text := AValues[I];
          SelectAll;
          Prompt.FocusControl := Edit;
        end;
        CurPrompt := Edit.Top + Edit.Height + MulDiv(5, LPPI, 96); // KV
      end;
      ButtonTop := Edit.Top + Edit.Height + MulDiv(15, LPPI, 96); //KV
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := DialogUnits.Y + MulDiv(6, LPPI, 96);  //MulDiv(14, DialogUnits.Y, 8);
      if TStyleManager.IsCustomStyleActive then
        ButtonsRightOffset := 4
      else
        ButtonsRightOffset := 0;
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgOK;
        ModalResult := mrOk;
        Default := True;
        SetBounds(Form.ClientWidth - (ButtonWidth + MulDiv(8, DialogUnits.X, 4)) * 2 - ButtonsRightOffset, ButtonTop, ButtonWidth, ButtonHeight);
      end;
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgCancel;
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(Form.ClientWidth - (ButtonWidth + MulDiv(8, DialogUnits.X, 4)) - ButtonsRightOffset, ButtonTop, ButtonWidth, ButtonHeight);
        Form.ClientHeight := Top + Height + MulDiv(13 + 2 * ButtonsRightOffset, LPPI, 96);
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
{$ENDIF}
{$ENDREGION}

{$REGION 'Fix TMonitorSupport - https://quality.embarcadero.com/browse/RSP-28632}
{$IF CompilerVersion < 34}
{
  Fixes TMonitor event stack
  See https://en.delphipraxis.net/topic/2824-revisiting-tthreadedqueue-and-tmonitor/
  for discussion.
}
{====================== Patched TMonitorSupport below =========================}
{ This section provides the required support to the TMonitor record in System. }
type
  PEventItemHolder = ^TEventItemHolder;
  TEventItemHolder = record
    Next: PEventItemHolder;
    Event: Pointer;
  end align {$IFDEF CPUX64}16{$ELSE CPUX64}8{$ENDIF CPUX64};

  TEventStack = record
    Head: PEventItemHolder;
    Counter: NativeInt;
    procedure Push(EventItem: PEventItemHolder);
    function Pop: PEventItemHolder;
  end align {$IFDEF CPUX64}16{$ELSE CPUX64}8{$ENDIF CPUX64};

{$IFDEF Win64}
function InterlockedCompareExchange128(Destination: Pointer; ExchangeHigh, ExchangeLow: Int64; ComparandResult: Pointer): boolean;
// The parameters are in the RCX, RDX, R8 and R9 registers per the MS x64 calling convention:
//   RCX        Destination
//   RDX        ExchangeHigh
//   R8         ExchangeLow
//   R9         ComparandResult
//
// CMPXCHG16B requires the following register setup:
//   RDX:RAX    ComparandResult.High:ComparandResult.Low
//   RCX:RBX    ExchangeHigh:ExchangeLow
// See: https://www.felixcloutier.com/x86/cmpxchg8b:cmpxchg16b
asm
      .PUSHNV RBX
      MOV   R10,Destination             // RCX
      MOV   RBX,ExchangeLow             // R8
      MOV   RCX,ExchangeHigh            // RDX
      MOV   RDX,[ComparandResult+8]     // R9
      MOV   RAX,[ComparandResult]       // R9
 LOCK CMPXCHG16B [R10]
      MOV   [ComparandResult+8],RDX     // R9
      MOV   [ComparandResult],RAX       // R9
      SETZ  AL
 end;
{$ENDIF Win64}

function InterlockedCompareExchange(var Dest: TEventStack; const NewValue, CurrentValue: TEventStack): Boolean;
begin
  //Result := CAS(CurrentValue.Head, CurrentValue.Counter, NewValue.Head, NewValue.Counter, Dest);
  {$IFDEF CPUX64}
  Result := InterlockedCompareExchange128(@Dest, NewValue.Counter, Int64(NewValue.Head), @CurrentValue);
  {$ELSE CPUX64}
  Result := InterlockedCompareExchange64(Int64(Dest), Int64(NewValue), Int64(CurrentValue)) = Int64(CurrentValue);
  {$ENDIF CPUX64}
end;

procedure TEventStack.Push(EventItem: PEventItemHolder);
var
  Current, Next: TEventStack;
begin
  repeat
    Current := Self;
    EventItem.Next := Current.Head;
    Next.Head := EventItem;
    Next.Counter := Current.Counter + 1;
  until InterlockedCompareExchange(Self, Next, Current);
end;

function TEventStack.Pop: PEventItemHolder;
var
  Current, Next: TEventStack;
begin
  repeat
    Current := Self;
    if (Current.Head = nil) then
      Exit(nil);
    Next.Head := Current.Head.Next;
    Next.Counter := Current.Counter + 1;
  until InterlockedCompareExchange(Self, Next, Current);
  Result := Current.Head;
end;

var
  EventCache: TEventStack = (Head: nil; Counter: 0);
  EventItemHolders: TEventStack = (Head: nil; Counter: 0);
  MonitorSupportShutDown: Boolean = False;

function NewWaitObj: Pointer;
var
  EventItem: PEventItemHolder;
begin
  if MonitorSupportShutDown then
    EventItem := nil
  else
    EventItem := EventCache.Pop;

  if EventItem <> nil then
  begin
    Result := EventItem.Event;
    EventItem.Event := nil;
    EventItemHolders.Push(EventItem);
  end else
    Result := Pointer(CreateEvent(nil, False, False, nil));
  ResetEvent(THandle(Result));
end;

procedure FreeWaitObj(WaitObject: Pointer);
var
  EventItem: PEventItemHolder;
begin
  if MonitorSupportShutDown then begin
    CloseHandle(THandle(WaitObject));
    Exit;
  end;

  EventItem := EventItemHolders.Pop;
  if EventItem = nil then
    New(EventItem);
  EventItem.Event := WaitObject;
  EventCache.Push(EventItem);
end;

procedure CleanStack(Stack: PEventItemHolder);
var
  Walker: PEventItemHolder;
begin
  Walker := Stack;
  while Walker <> nil do
  begin
    Stack := Walker.Next;
    if Walker.Event <> nil then
      CloseHandle(THandle(Walker.Event));
    Dispose(Walker);
    Walker := Stack;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION 'Fix TCustomImageList.SetSize - https://quality.embarcadero.com/browse/RSP-30931}
// Fixed in D11.3
{$IF CompilerVersion < 35}
type
  TCustomImageList_SetSize = procedure(const Self: TCustomImageList; AWidth, AHeight: Integer);
var
  Trampoline_TCustomImageList_SetSize : TCustomImageList_SetSize = nil;

procedure Detour_TCustomImageList_SetSize(const Self: TCustomImageList; AWidth, AHeight: Integer);
begin
   Self.BeginUpdate;
   try
     if Assigned(Trampoline_TCustomImageList_SetSize) then
       Trampoline_TCustomImageList_SetSize(Self, AWidth, AHeight);
   finally
     Self.EndUpdate;
   end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION 'Fix TCustomForm.SetParent - https://quality.embarcadero.com/browse/RSP-41987}
{$IF CompilerVersion >= 35}
type
  TCustomForm_SetParent = procedure(const Self: TCustomForm; AParent: TWinControl);
  TCrackCustomForm = class(TCustomForm);

var
  Trampoline_TCustomForm_SetParent : TCustomForm_SetParent = nil;

procedure Detour_TCustomForm_SetParent(const Self: TCustomForm; AParent: TWinControl);
var
  OldFClientWidth: Integer;
  OldFClientHeight: Integer;
begin
  with TCrackCustomForm(Self) do
  begin
    OldFClientWidth := FClientWidth;
    OldFClientHeight := FClientHeight;
    FClientWidth := 0;
    FClientHeight := 0;
  end;
  if Assigned(Trampoline_TCustomForm_SetParent) then
    Trampoline_TCustomForm_SetParent(Self, AParent);
  with TCrackCustomForm(Self) do
  begin
    FClientWidth := OldFClientWidth;
    FClientHeight := OldFClientHeight;
  end;
end;
{$ENDIF}
{$ENDREGION}

initialization
  {$IF CompilerVersion < 34}
  Detour_TWICImage_CreateWICBitmap := TWICImage(nil).Detour_CreateWICBitmap;
  TMethod(Trampoline_TWICImage_CreateWICBitmap).Code :=
    InterceptCreate(TWICImage(nil).GetCreateWICBitmapAddr,
    TMethod(Detour_TWICImage_CreateWICBitmap).Code);
  {$ENDIF}

  {$IF CompilerVersion < 35}
  Original_InputQuery := Vcl.Dialogs.InputQuery;
  Detour_InputQuery := FixedInputQuery;
  Trampoline_InputQuery := TInputQuery(InterceptCreate(@Original_InputQuery, @Detour_InputQuery));
  {$ENDIF}

  {$IF CompilerVersion < 34}
  System.MonitorSupport.NewWaitObject := NewWaitObj;
  System.MonitorSupport.FreeWaitObject := FreeWaitObj;
  {$ENDIF}

  {$IF CompilerVersion < 35}
  Trampoline_TCustomImageList_SetSize :=
    InterceptCreate(@TCustomImageList.SetSize, @Detour_TCustomImageList_SetSize);
  {$ENDIF}

  {$IF CompilerVersion >= 35}
  Trampoline_TCustomForm_SetParent :=
    InterceptCreate(@TCrackCustomForm.SetParent, @Detour_TCustomForm_SetParent);
  {$ENDIF}
finalization
  {$IF CompilerVersion < 34}
  InterceptRemove(TMethod(Trampoline_TWICImage_CreateWICBitmap).Code);
  {$ENDIF}
  {$IF CompilerVersion < 35}
  InterceptRemove(@Trampoline_InputQuery);
  {$ENDIF}
  {$IF CompilerVersion < 34}
  MonitorSupportShutDown := True;
  CleanStack(AtomicExchange(Pointer(EventCache.Head), nil));
  CleanStack(AtomicExchange(Pointer(EventItemHolders.Head), nil));
  {$ENDIF}
  {$IF CompilerVersion < 35}
  InterceptRemove(@Trampoline_TCustomImageList_SetSize);
  {$ENDIF}
  {$IF CompilerVersion >= 35}
  InterceptRemove(@Trampoline_TCustomForm_SetParent);
  {$ENDIF}
end.
