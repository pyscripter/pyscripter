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
  Vcl.Graphics,
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

initialization
 Detour_TWICImage_CreateWICBitmap := TWICImage(nil).Detour_CreateWICBitmap;
 TMethod(Trampoline_TWICImage_CreateWICBitmap).Code :=
   InterceptCreate(TWICImage(nil).GetCreateWICBitmapAddr,
   TMethod(Detour_TWICImage_CreateWICBitmap).Code);

finalization
 InterceptRemove(TMethod(Trampoline_TWICImage_CreateWICBitmap).Code);
end.
