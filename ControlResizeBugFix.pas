{******************************************************************************}
{*                                                                            *}
{* Control resize bugfix for kernel stack overflow due to WH_CALLWNDPROC hook *)
{* Version 1.1 (2008-05-25)                                                   *}
{*                                                                            *}
{* (C) 2008 Andreas Hausladen (Andreas.Hausladen@gmx.de)                      *}
{*                                                                            *}
{******************************************************************************}
{$A+,B-,C+,D-,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}

{ Usage:
    Add the unit to the .dpr file's uses-list.

    Example:
      uses
        FastMM4, // optional memory manager
        ControlResizeBugFix,
        Forms,
        Unit1 in 'Unit1.pas';


    History:
      2008-05-25:
        - Added code to detect endless resizing controls.
        - Added experimental OPTIMIZED_RESIZE_REDRAW option for faster form resizing
      2008-05-24:
        - Initial release
}

unit ControlResizeBugFix;

{.$DEFINE OPTIMIZED_RESIZE_REDRAW}
{ The OPTIMIZED_RESIZE_REDRAW option is experimental. It speeds up the resizing of forms
  by not redrawing each control when it is realigned but by invalidating them all after
  one align round is done. }

{$UNDEF DEBUG} // undef any global DEBUG conditions
{.$DEFINE DEBUG}
{ The DEBUG option activates the source code debugging of this unit. }

interface

{$IFDEF DEBUG}
  {$D+}
{$ENDIF}

implementation

uses
  Windows, Messages, SysUtils, Classes, Controls;

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 18.0}
   {$DEFINE DELPHI2006_UP}
  {$IFEND}
  {$IF CompilerVersion >= 17.0}
   {$DEFINE DELPHI2005_UP}
  {$IFEND}
{$ENDIF}

type
  TFixWinControl = class(TWinControl)
  private
    procedure AlignControl(AControl: TControl);
    procedure HandleAlignControls(AControl: TControl; var R: TRect);
  protected
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

  TOpenWinControl = class(TWinControl);

{$IFNDEF DELPHI2005_UP}
  TD5WinControlPrivate = class(TControl)
  public
    FAlignLevel: Word;
  end;
{$ENDIF ~DELPHI2005_UP}

threadvar
  AlignControlList: TList;

procedure TFixWinControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  WindowPlacement: TWindowPlacement;
begin
  if (ALeft <> Left) or (ATop <> Top) or
    (AWidth <> Width) or (AHeight <> Height) then
  begin
    if HandleAllocated and not IsIconic(WindowHandle) then
    begin
      if AlignControlList <> nil then
        SetWindowPos(WindowHandle, 0, ALeft, ATop, AWidth, AHeight,
          SWP_NOZORDER or SWP_NOACTIVATE or SWP_DEFERERASE)
      else
        SetWindowPos(WindowHandle, 0, ALeft, ATop, AWidth, AHeight,
          SWP_NOZORDER or SWP_NOACTIVATE);
    end
    else
    begin
      PInteger(@Left)^ := ALeft;
      PInteger(@Top)^ := ATop;
      PInteger(@Width)^ := AWidth;
      PInteger(@Height)^ := AHeight;
      if HandleAllocated then
      begin
        WindowPlacement.Length := SizeOf(WindowPlacement);
        GetWindowPlacement(WindowHandle, @WindowPlacement);
        WindowPlacement.rcNormalPosition := BoundsRect;
        SetWindowPlacement(WindowHandle, @WindowPlacement);
      end;
    end;
    UpdateBoundsRect(Rect(Left, Top, Left + Width, Top + Height));
    RequestAlign;
  end;
end;
  
procedure TFixWinControl.HandleAlignControls(AControl: TControl; var R: TRect);

  function AlignWork: Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := ControlCount - 1 downto 0 do
      if (Controls[I].Align <> alNone) or
         (Controls[I].Anchors <> [akLeft, akTop]) then
        Exit;
    Result := False;
  end;

var
  OwnAlignControlList, TempAlignControlList: TList;
  ResizeList: TList;
  ResizeCounts: TList; // of Integer
  Ctrl: TWinControl;
  I, Index: Integer;
begin
  if AlignWork then
  begin
    OwnAlignControlList := nil;
    try
      if AlignControlList = nil then
      begin
        OwnAlignControlList := TList.Create;
        AlignControlList := OwnAlignControlList;
      end;

      AlignControls(AControl, R);

      if (OwnAlignControlList <> nil) and (OwnAlignControlList.Count > 0) then
      begin
        { Convert recursion into an iteration to prevent the kernel stack overflow }
        ResizeList := TList.Create;
        ResizeCounts := TList.Create;
        try
          { The controls in the OwnAlignControlList must be added to ResizeList in reverse order.
            Otherwise the OnResize events aren't fired in correct order. }
          AlignControlList := TList.Create;
          try
            repeat
              try
                for I := OwnAlignControlList.Count - 1 downto 0 do
                begin
                  Ctrl := TWinControl(OwnAlignControlList[I]);
                  Index := ResizeList.IndexOf(Ctrl);

                  { An endless resizing component was stopped by the kernel stack overflow bug.
                    So we must catch this condition to prevent an endless loop. }
                  if (Index = -1) or (Integer(ResizeCounts[Index]) < 30) then
                  begin
                    Ctrl.Realign;

                    if Index <> -1 then
                      ResizeCounts[Index] := Pointer(Integer(ResizeCounts[Index]) + 1);
                    ResizeCounts.Add(Pointer(0)); // keep index in sync
                    ResizeList.Add(Ctrl);
                  end
                  else if Index <> -1 then
                  begin
                    {$WARNINGS OFF}
                    if DebugHook <> 0 then
                    {$WARNINGS ON}
                      OutputDebugString(PChar(Format('The component "%s" of class %s has an endless resize loop', [Ctrl.Name, Ctrl.ClassName])));
                  end;
                end;
              finally
                OwnAlignControlList.Clear;

                { Switch lists }
                TempAlignControlList := AlignControlList;
                AlignControlList := OwnAlignControlList;
                OwnAlignControlList := TempAlignControlList;
              end;
            until (OwnAlignControlList.Count = 0) {or EndlessResizeDetection};
          finally
            { Let another AlignControlList handle any alignment that comes from the
              OnResize method. }
            FreeAndNil(AlignControlList);
          end;

          { Fire Resize events }
          for I := ResizeList.Count - 1 downto 0 do
          begin
            Ctrl := TWinControl(ResizeList[I]);
            if not (csLoading in Ctrl.ComponentState) then
              TOpenWinControl(Ctrl).Resize;
          end;
        finally
          ResizeCounts.Free;
          ResizeList.Free;
        end;
        {$IFDEF OPTIMIZED_RESIZE_REDRAW}
        Invalidate;
        {$ENDIF OPTIMIZED_RESIZE_REDRAW}
      end;
    finally
      if OwnAlignControlList <> nil then
      begin
        AlignControlList := nil;
        FreeAndNil(OwnAlignControlList);
      end;
    end;
  end
  else
    AlignControls(AControl, R);
end;

procedure TFixWinControl.WMSize(var Message: TWMSize);
begin
  {$IFDEF DELPHI2005_UP}
  UpdateBounds;
    {$IFDEF DELPHI2006_UP}
  UpdateExplicitBounds;
    {$ENDIF DELPHI2006_UP}
  {$ELSE}
  if HandleAllocated then
    Perform(WM_MOVE, 0, LPARAM(Left and $0000ffff) or (Top shl 16)); // calls the private UpdateBounds
  {$ENDIF DELPHI2005_UP}
  DefaultHandler(Message);
  if AlignControlList <> nil then
  begin
    if AlignControlList.IndexOf(Self) = -1 then
      AlignControlList.Add(Self)
  end
  else
  begin
    Realign;
    if not (csLoading in ComponentState) then
      Resize;
  end;
end;

procedure TFixWinControl.AlignControl(AControl: TControl);
var
  Rect: TRect;
begin
  if not HandleAllocated or (csDestroying in ComponentState) then
    Exit;
  {$IFDEF DELPHI2005_UP}
  if AlignDisabled then
  {$ELSE}
  if TD5WinControlPrivate(Self).FAlignLevel <> 0 then
  {$ENDIF DELPHI2005_UP}
    ControlState := ControlState + [csAlignmentNeeded]
  else
  begin
    DisableAlign;
    try
      Rect := GetClientRect;

      HandleAlignControls(AControl, Rect);
    finally
      ControlState := ControlState - [csAlignmentNeeded];
      EnableAlign;
    end;
  end;
end;

{---------------------------------------------------------------------------}

type
  TInjectRec = packed record
    Jump: Byte;
    Offset: Integer;
  end;

  PWin9xDebugThunk = ^TWin9xDebugThunk;
  TWin9xDebugThunk = packed record
    PUSH: Byte;
    Addr: Pointer;
    JMP: Byte;
    Offset: Integer;
  end;

  PAbsoluteIndirectJmp = ^TAbsoluteIndirectJmp;
  TAbsoluteIndirectJmp = packed record
    OpCode: Word;   //$FF25(Jmp, FF /4)
    Addr: ^Pointer;
  end;

function GetActualAddr(Proc: Pointer): Pointer;

  function IsWin9xDebugThunk(AAddr: Pointer): Boolean;
  begin
    Result := (AAddr <> nil) and
              (PWin9xDebugThunk(AAddr).PUSH = $68) and
              (PWin9xDebugThunk(AAddr).JMP = $E9);
  end;

begin
  if Proc <> nil then
  begin
    if (Win32Platform <> VER_PLATFORM_WIN32_NT) and IsWin9xDebugThunk(Proc) then
      Proc := PWin9xDebugThunk(Proc).Addr;
    if (PAbsoluteIndirectJmp(Proc).OpCode = $25FF) then
      Result := PAbsoluteIndirectJmp(Proc).Addr^
    else
      Result := Proc;
  end
  else
    Result := nil;
end;

function CodeRedirect(Proc: Pointer; NewProc: Pointer): TInjectRec;
var
  OldProtect: Cardinal;
begin
  if Proc = nil then
    Exit;
  Proc := GetActualAddr(Proc);
  if VirtualProtect(Proc, SizeOf(TInjectRec), PAGE_EXECUTE_READWRITE, OldProtect) then
  begin
    Result := TInjectRec(Proc^);
    TInjectRec(Proc^).Jump := $E9;
    TInjectRec(Proc^).Offset := Integer(NewProc) - (Integer(Proc) + SizeOf(TInjectRec));
    VirtualProtect(Proc, SizeOf(TInjectRec), OldProtect, @OldProtect);
    FlushInstructionCache(GetCurrentProcess, Proc, SizeOf(TInjectRec));
  end;
end;

{---------------------------------------------------------------------------}

function GetAlignControlProc: Pointer;
var
  P: PByteArray;
  Offset: Integer;
  MemInfo: TMemoryBasicInformation;
begin
  P := GetActualAddr(@TWinControl.Realign);
  if (P <> nil) and (VirtualQuery(P, MemInfo, SizeOf(MemInfo)) = SizeOf(MemInfo)) then
  begin
    if (MemInfo.AllocationProtect <> PAGE_NOACCESS) then
    begin
      Offset := 0;
      while Offset < $40 do
      begin
        if ((P[0] = $33) and (P[1] = $D2)) or   // xor edx,edx
           ((P[0] = $31) and (P[1] = $D2)) then // xor edx,edx
        begin
          if P[2] = $E8 then // call TWinControl.AlignControl
          begin
            Inc(PByte(P), 2);
            Result := PAnsiChar(P) + 5 + PInteger(PAnsiChar(P) + 1)^;
            Exit;
          end
          else if (P[2] = $8B) and (P[3] = $45) and (P[4] = $FC) and // mov eax,[ebp-$04]
                  (P[5] = $E8) then // call TWinControl.AlignControl
          begin
            Inc(PByte(P), 5);
            Result := PAnsiChar(P) + 5 + PInteger(PAnsiChar(P) + 1)^;
            Exit;
          end;
        end;
        Inc(PByte(P));
        Inc(Offset);
      end;
    end;
  end;
  Result := nil;
end;

function GetDynamicMethod(AClass: TClass; Index: Integer): Pointer;
asm
  call System.@FindDynaClass
end;

var
  WinControl_AlignControlProc, WinControl_WMSize, WinControl_SetBounds: Pointer;
  BackupAlignControl, BackupWMSize, BackupSetBounds: TInjectRec;

procedure Init;
begin
  WinControl_AlignControlProc := GetAlignControlProc;
  WinControl_WMSize := GetDynamicMethod(TWinControl, WM_SIZE);
  WinControl_SetBounds := @TOpenWinControl.SetBounds;
  if (WinControl_AlignControlProc <> nil) and (WinControl_WMSize <> nil) then
  begin
    { Redirect the original function to the bug fixed version }
    BackupAlignControl := CodeRedirect(WinControl_AlignControlProc, @TFixWinControl.AlignControl);
    BackupWMSize := CodeRedirect(WinControl_WMSize, @TFixWinControl.WMSize);
    {$IFDEF OPTIMIZED_RESIZE_REDRAW}
    BackupSetBounds := CodeRedirect(WinControl_SetBounds, @TFixWinControl.SetBounds);
    {$ENDIF OPTIMIZED_RESIZE_REDRAW}
  end;
end;

procedure Fini;
var
  n: DWORD;
begin
  { Restore the original function }
  if BackupAlignControl.Jump <> 0 then
    WriteProcessMemory(GetCurrentProcess, GetActualAddr(WinControl_AlignControlProc),
                       @BackupAlignControl, SizeOf(BackupAlignControl), n);
  if BackupWMSize.Jump <> 0 then
    WriteProcessMemory(GetCurrentProcess, GetActualAddr(WinControl_WMSize),
                       @BackupWMSize, SizeOf(BackupWMSize), n);
  if BackupSetBounds.Jump <> 0 then
    WriteProcessMemory(GetCurrentProcess, GetActualAddr(WinControl_SetBounds),
                       @BackupSetBounds, SizeOf(BackupSetBounds), n);
end;

initialization
  Init;

finalization
  Fini;

end.

