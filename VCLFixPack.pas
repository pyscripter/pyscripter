{**************************************************************************************************}
{                                                                                                  }
{ VCLFixPack unit - Unoffical bug fixes for Delphi/C++Builder                                      }
{ Version 1.1 (2009-01-25)                                                                         }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is VCLFixPack.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Andreas Hausladen (Andreas.Hausladen@gmx.de).      }
{ Portions created by Andreas Hausladen are Copyright (C) 2008 Andreas Hausladen.                  }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}

{$IFNDEF CONDITIONALEXPRESSIONS}
  Delphi5_is_not_supported
{$ENDIF}

{$A8,B-,C+,D-,E-,F-,G+,H+,I+,J-,K-,L+,M-,N-,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}

{ If you define VCLFIXPACK_DEBUG the patches are compiled with debug information. }
{$IFDEF VCLFIXPACK_DEBUG} {$D+} {$ENDIF}

{ If you use Delphi 6/7/2005 Personal you must disable the VCLFIXPACK_DB_SUPPORT define. }
{.$DEFINE VCLFIXPACK_DB_SUPPORT}

unit VCLFixPack;

{
 Usage
 =====
   Add the unit to the .dpr file's uses-list.
   C++Builder user can add the file to the project (Menu Project/Add to project)

 Example
 =======
   uses
     FastMM4, // optional memory manager
     VCLFixPack,
     Forms,
     Unit1 in 'Unit1.pas';


 Fixes the following bug
 =======================
   - [2006-2009]
     QC #68647: Infinite loop in Forms.GetNonToolWindowPopupParent
     (http://qc.codegear.com/wc/qcmain.aspx?d=68647)

   - [2007-2009]
     QC #68740: Lost focus after TOpenDialog when MainFormOnTaskBar is set
     (http://qc.codegear.com/wc/qcmain.aspx?d=68740)

   - [2007-2009]
     QC #59963: Closing non-modal forms after a task switch can deactivate the application
     (http://qc.codegear.com/wc/qcmain.aspx?d=59963)

   - [2009]
     QC #66892: Closing forms deactivates the application (missing "stdcall")
     (http://qc.codegear.com/wc/qcmain.aspx?d=66892)

   - [6-2007]
     Control resize bugfix for kernel stack overflow due to WH_CALLWNDPROC hook

   - [6-2007]
     QC #59654: TActionList access already released FActions field
     (http://qc.codegear.com/wc/qcmain.aspx?d=59654)

   - [6-2007]
     QC #54286 : Parent-PopupMenu overrides standard context menu (edit, memo, combobox, ...)
     (http://qc.codegear.com/wc/qcmain.aspx?d=54286)

   - [2006-2007]
     QC #50097: ObjAuto access violation on XEON (Data Execution Prevention bug)
     (http://qc.codegear.com/wc/qcmain.aspx?d=50097)

   - [6-2009]
     Classes.MakeObjectInstance memory leak fix
     (for usage in a DLL)

   - [2007]
     QC #58938: MainForm Minimize minimizes in the background
     (http://qc.codegear.com/wc/qcmain.aspx?d=58938)

   - [6-2009]
     QC #64484: SysUtils.Abort can raise an AccessViolation
     (http://qc.codegear.com/wc/qcmain.aspx?d=64484)

   - [2007]
     QC #58939: No taskbar button when starting from ShellLink with Show=Minimized
     (http://qc.codegear.com/wc/qcmain.aspx?d=58939)

   - [6-2009]
     QC #35001: MDIChild's active control focus is not set correctly
     (http://qc.codegear.com/wc/qcmain.aspx?d=35001)

   - [7-2009]
     QC #56252: TPageControl flickers a lot with active theming
     (http://qc.codegear.com/wc/qcmain.aspx?d=56252)
     QC #68730: TLabel is not painted on a themed, double-buffered TTabSheet in Vista
     (http://qc.codegear.com/wc/qcmain.aspx?d=68730)
     TLabels on TTabSheet are not painted (themes) if a TWinControl like TMemo is on the TTabSheet (TWinControl.PaintWindow bug)

   - [7-2009]
     Grid flickers with active theming (DBGrid, StringGrid and DrawGrid only, no derived classes)

   - [2009]
     QC #69112: TSpeedButton is painted as a black rectangle on a double buffered panel on a sheet of glass.

   - [2009]
     QC #69294: TProgressBar fails with PBS_MARQUEE and disabled Themes (Vista)
     http://qc.codegear.com/wc/qcmain.aspx?d=69294

   - [Windows Vista]
     Workaround for Windows Vista CompareString bug
     (Workaround is disabled by default, define "VistaCompareStringFix" to activate it)


 Changlog:
   2009-01-25:
     Fixed: DBGrid ScrollBar gab wasn't painted correctly in BiDiMode <> bdLeftToRight
     Fixed: TTabSheet could throw an access violation if no PageControl was assigned to it
     Changed: Rewritten TaskModalDialog bugfix
     Added: QC #69294: TProgressBar fails with PBS_MARQUEE and disabled Themes (Vista)

}

interface

{ ---------------------------------------------------------------------------- }

{.$DEFINE DBTextColorBugFix} // Delphi 6+

{$IF CompilerVersion >= 18.0} // Delphi 2006+
  {$DEFINE GetNonToolWindowPopupParentFix}
{$IFEND}

{$IF CompilerVersion >= 18.5} // Delphi 2007+
  {$DEFINE TaskModalDialogFix}
{$IFEND}

{$IF CompilerVersion >= 18.5} // Delphi 2007+
  {$DEFINE AppDeActivateZOrderFix}
{$IFEND}

{$IF CompilerVersion = 20.0} // Delphi 2009
  {$DEFINE HideStackTrashingFix}
{$IFEND}

{$IF CompilerVersion < 20.0} // Delphi 6-2007
  {$DEFINE ControlResizeFix}
  { The OPTIMIZED_RESIZE_REDRAW option is experimental. It speeds up the resizing of forms
    by not redrawing each control when it is realigned but by invalidating them all after
    one align round is done. }
  {.$DEFINE OPTIMIZED_RESIZE_REDRAW}
{$IFEND}

{$IF CompilerVersion < 20.0} // Delphi 6-2007
  {$DEFINE ActionListAVFix}
{$IFEND}

{$IF CompilerVersion < 20.0} // Delphi 6-2007
  {$DEFINE ContextMenuFix}
{$IFEND}

{$IF (CompilerVersion >= 18.0) and (CompilerVersion < 20.0)} // Delphi 2006-2007
  {$DEFINE ObjAutoDEPFix}
{$IFEND}

{$DEFINE MkObjInstLeakFix} // Delphi 6+

{$DEFINE SysUtilsAbortFix} // Delphi 6+

{$IF CompilerVersion = 18.5} // Delphi 2007
  {$DEFINE AppMinimizeFix}
{$IFEND}

{$IF CompilerVersion = 18.5} // Delphi 2007
  {$DEFINE CmdShowMinimizeFix}
{$IFEND}

{$DEFINE MDIChildFocusFix} // Delphi 6+

{$IF CompilerVersion >= 15} // Delphi 7+
  {.$DEFINE PageControlPaintingFix}
{$IFEND}

{$IF CompilerVersion >= 15} // Delphi 7+
  {.$DEFINE GridFlickerFix}
{$IFEND}

{$IF CompilerVersion = 20.0} // Delphi 2009
  {$DEFINE SpeedButtonGlassFix}
{$IFEND}

{$IF CompilerVersion = 20.0} // Delphi 2009
  {$DEFINE VistaProgressBarMarqueeFix}
{$IFEND}

{**************************************************************************************************}
{ Workaround for Windows Vista CompareString bug.                                                  }
{ The Ü/ü ($DC/$FC) and the UE/ue are treated equal in all locales, but they aren't equal. There   }
{ was a bugfix intended for Vista SP1 but it was removed before SP1 was released.                  }
{ Windows 2008 Server still includes this bugfix but Vista will never get this bugfix.             }
{ Microsoft: new versions are for correctness; service packs are for consistency and compatibility }
{**************************************************************************************************}
{ WARNING: This bugfix can slow down CompareString }
{.$DEFINE VistaCompareStringFix}

implementation

{$IF CompilerVersion >= 18.0}
 {$DEFINE DELPHI2006_UP}
{$IFEND}
{$IF CompilerVersion >= 17.0}
 {$DEFINE DELPHI2005_UP}
{$IFEND}

uses
  Windows, Messages, SysUtils, Classes, TypInfo, ActnList, SysConst,
  {$IFDEF ObjAutoDEPFix}
  ObjAuto,
  {$ENDIF ObjAutoDEPFix}
  {$IF CompilerVersion >= 15.0}
  Themes,
  {$IFEND}
  {$IFDEF VCLFIXPACK_DB_SUPPORT}
  DBGrids,
  {$ENDIF VCLFIXPACK_DB_SUPPORT}
  Graphics, Controls, Forms, Dialogs, StdCtrls, Grids, ComCtrls;

{ ---------------------------------------------------------------------------- }
{ Helper functions, shared }
type
  TOpenWinControl = class(TWinControl);
  TOpenCustomForm = class(TCustomForm);
  TOpenCommonDialog = class(TCommonDialog);
  TOpenCustomActionList = class(TCustomActionList);
  TOpenComponent = class(TComponent);
  TOpenCustomCombo = class(TCustomCombo);

  TJumpOfs = Integer;
  PPointer = ^Pointer;

type
  PXRedirCode = ^TXRedirCode;
  TXRedirCode = packed record
    Jump: Byte;
    Offset: TJumpOfs;
  end;

  PWin9xDebugThunk = ^TWin9xDebugThunk;
  TWin9xDebugThunk = packed record
    PUSH: Byte;
    Addr: Pointer;
    JMP: TXRedirCode;
  end;

  PAbsoluteIndirectJmp = ^TAbsoluteIndirectJmp;
  TAbsoluteIndirectJmp = packed record
    OpCode: Word;   //$FF25(Jmp, FF /4)
    Addr: PPointer;
  end;

var
  TaskActiveWindow: HWND;
  TaskFirstWindow: HWND;
  TaskFirstTopMost: HWND;

function DoFindWindow(Window: HWND; Param: LPARAM): Bool; stdcall;
begin
  if (Window <> TaskActiveWindow) and (Window <> Application.Handle) and
     IsWindowVisible(Window) and IsWindowEnabled(Window) then
  begin
    if GetWindowLong(Window, GWL_EXSTYLE) and WS_EX_TOPMOST = 0 then
    begin
      if TaskFirstWindow = 0 then
        TaskFirstWindow := Window;
    end else
    begin
      if TaskFirstTopMost = 0 then
        TaskFirstTopMost := Window;
    end;
  end;
  Result := True;
end;

function FindTopMostWindow(ActiveWindow: HWND): HWND;
begin
  TaskActiveWindow := ActiveWindow;
  TaskFirstWindow := 0;
  TaskFirstTopMost := 0;
  EnumThreadWindows(GetCurrentThreadID, @DoFindWindow, 0);
  if TaskFirstWindow <> 0 then
    Result := TaskFirstWindow
  else
    Result := TaskFirstTopMost;
end;

{ Hooking }

function GetActualAddr(Proc: Pointer): Pointer;

  function IsWin9xDebugThunk(AAddr: Pointer): Boolean;
  begin
    Result := (AAddr <> nil) and
              (PWin9xDebugThunk(AAddr).PUSH = $68) and
              (PWin9xDebugThunk(AAddr).JMP.Jump = $E9);
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

procedure HookProc(Proc, Dest: Pointer; var BackupCode: TXRedirCode);
var
  n: DWORD;
  Code: TXRedirCode;
begin
  Proc := GetActualAddr(Proc);
  Assert(Proc <> nil);
  if ReadProcessMemory(GetCurrentProcess, Proc, @BackupCode, SizeOf(BackupCode), n) then
  begin
    Code.Jump := $E9;
    Code.Offset := PAnsiChar(Dest) - PAnsiChar(Proc) - SizeOf(Code);
    WriteProcessMemory(GetCurrentProcess, Proc, @Code, SizeOf(Code), n);
  end;
end;

procedure UnhookProc(Proc: Pointer; var BackupCode: TXRedirCode);
var
  n: Cardinal;
begin
  if (BackupCode.Jump <> 0) and (Proc <> nil) then
  begin
    Proc := GetActualAddr(Proc);
    Assert(Proc <> nil);
    WriteProcessMemory(GetCurrentProcess, Proc, @BackupCode, SizeOf(BackupCode), n);
    BackupCode.Jump := 0;
  end;
end;

procedure ReplaceVmtField(AClass: TClass; OldProc, NewProc: Pointer);
type
  PVmt = ^TVmt;
  TVmt = array[0..MaxInt div SizeOf(Pointer) - 1] of Pointer;
var
  I: Integer;
  Vmt: PVmt;
  n: Cardinal;
  P: Pointer;
begin
  OldProc := GetActualAddr(OldProc);
  NewProc := GetActualAddr(NewProc);

  I := vmtSelfPtr div SizeOf(Pointer);
  Vmt := Pointer(AClass);
  while (I < 0) or (Vmt[I] <> nil) do
  begin
    P := Vmt[I];
    if (P <> OldProc) and (Integer(P) > $10000) and not IsBadReadPtr(P, 6) then
      P := GetActualAddr(P);
    if P = OldProc then
    begin
      WriteProcessMemory(GetCurrentProcess, @Vmt[I], @NewProc, SizeOf(NewProc), n);
      Exit;
    end;
    Inc(I);
  end;
end;

function GetDynamicMethod(AClass: TClass; Index: Integer): Pointer;
asm
  call System.@FindDynaClass
end;

procedure DebugLog(const S: string);
begin
  OutputDebugString(PChar('VCLFixPack patch installed: ' + S));
end;
{ ---------------------------------------------------------------------------- }



{ ---------------------------------------------------------------------------- }
{ QC #68647: Infinite loop in Forms.GetNonToolWindowPopupParent }
{$IFDEF GetNonToolWindowPopupParentFix}
{
Forms.pas.4712: Result := GetParent(WndParent);  <= must be "Result := GetParent(Result);"
56               push esi                        <= must be "push ebx"
E8181EFAFF       call GetParent
8BD8             mov ebx,eax
Forms.pas.4711: while (Result <> 0) and (GetWindowLong(Result, GWL_EXSTYLE) and WS_EX_TOOLWINDOW = WS_EX_TOOLWINDOW) do
85DB             test ebx,ebx
}
procedure FixGetNonToolWindowPopupParent;
var
  P: PAnsiChar;
  Len: Integer;
  Buf: Byte;
  n: Cardinal;
begin
  P := GetActualAddr(@TOpenCustomForm.CreateParams);
  Dec(P);
  Len := 0;
  while Len < 112 do
  begin
    if //(P[0] = #$56) and // push esi
       (P[1] = #$E8) and // call GetParent
       (P[6] = #$8B) and (P[7] = #$D8) and // mov ebx,eax
       (P[8] = #$85) and (P[9] = #$DB) then // test ebx,ebx
    begin
      if P[0] = #$56 then // push esi
      begin
        Buf := $53; // push esi (WndParent) => push ebx (Result)
        WriteProcessMemory(GetCurrentProcess, P, @Buf, 1, n);
        DebugLog('GetNonToolWindowPopupParentFix');
        Exit;
      end else
      if P[0] = #$53 then // push ebx => already fixed, abort search
        Exit;
    end;
    Dec(P);
    Inc(Len);
  end;
end;
{$ENDIF GetNonToolWindowPopupParentFix}
{ ---------------------------------------------------------------------------- }



{ ---------------------------------------------------------------------------- }
{ QC #68740: Lost focus after TOpenDialog when MainFormOnTaskBar is set }
{$IFDEF TaskModalDialogFix}
var
  DialogsTaskModalDialogHook: TXRedirCode;
  DialogsTaskModalDialogCritSect: TRTLCriticalSection;

function TCommonDialog_TaskModalDialog(Instance: TObject; DialogFunc: Pointer; var DialogData): Bool;
var
  FocusWindow: HWND;
  Func: function(Instance: TObject; DialogFunc: Pointer; var DialogData): Bool;
begin
  EnterCriticalSection(DialogsTaskModalDialogCritSect);
  try
    UnhookProc(@TOpenCommonDialog.TaskModalDialog, DialogsTaskModalDialogHook);
    try
      FocusWindow := GetFocus;
      try
        Func := @TOpenCommonDialog.TaskModalDialog;
        Result := Func(Instance, DialogFunc, DialogData);
      finally
        SetFocus(FocusWindow);
      end;
    finally
      HookProc(@TOpenCommonDialog.TaskModalDialog, @TCommonDialog_TaskModalDialog, DialogsTaskModalDialogHook);
    end;
  finally
    LeaveCriticalSection(DialogsTaskModalDialogCritSect);
  end;
end;

procedure InitTaskModalDialogFix;
begin
  InitializeCriticalSection(DialogsTaskModalDialogCritSect);
  HookProc(@TOpenCommonDialog.TaskModalDialog, @TCommonDialog_TaskModalDialog, DialogsTaskModalDialogHook);
  DebugLog('FixTaskModalDialog');
end;

procedure FiniTaskModalDialogFix;
begin
  UnhookProc(@TOpenCommonDialog.TaskModalDialog, DialogsTaskModalDialogHook);
  DeleteCriticalSection(DialogsTaskModalDialogCritSect);
end;
{$ENDIF TaskModalDialogFix}
{ ---------------------------------------------------------------------------- }



{ ---------------------------------------------------------------------------- }
{ QC #59963: Closing non-modal forms after a task switch can deactivate the application }
{$IFDEF AppDeActivateZOrderFix}
type
  TAppDeActivateZOrderFix = class(TObject)
    class function AppWndProc(var Message: TMessage): Boolean;
  end;

var
  FLastActivePopup: HWND;

class function TAppDeActivateZOrderFix.AppWndProc(var Message: TMessage): Boolean;
var
  TopWindow: HWND;
begin
  Result := False;
  case Message.Msg of
    CM_ACTIVATE:
      begin
        if Assigned(Application.OnActivate) then
          Application.OnActivate(Application);
        if Assigned(Application.MainForm) and Application.MainFormOnTaskBar and
           not IsWindowEnabled(Application.MainForm.Handle) and
           (FLastActivePopup <> Application.MainForm.Handle) then
          Windows.SetFocus(FLastActivePopup)
        else
        begin
          { If the user switches to a different process and then back, every DestroyWindow call
            will accidentally activate a window from another process. }
          TopWindow := GetActiveWindow;
          if not Assigned(Application.MainForm) or (TopWindow <> Application.MainFormHandle) then
          begin
            {if TopWindow = 0 then // this happens when another app is active and the user clicks on the Close-Button of our form
              Windows.SetFocus(FindTopMostWindow(0))
            else}
              Windows.SetFocus(Application.Handle); // tell Windows that our application is still the focus owner
            if TopWindow <> 0 then
              SetActiveWindow(TopWindow); // restore the active window
          end;
        end;
        FLastActivePopup := 0;

        Result := True;
      end;
    CM_DEACTIVATE:
      begin
        FLastActivePopup := GetLastActivePopup(Application.Handle);
        if Assigned(Application.OnDeactivate) then
          Application.OnDeactivate(Application);
        Result := True;
      end;
  end;
end;
{$ENDIF AppDeActivateZOrderFix}
{ ---------------------------------------------------------------------------- }



{ ---------------------------------------------------------------------------- }
{ QC #66892: Closing forms deactivates the application (missing "stdcall") }
{$IFDEF HideStackTrashingFix}
var
  FindTopMostWindowHook: TXRedirCode;
  FindTopMostWindowProc: Pointer;

{
Forms.pas.1880: begin
53               push ebx
Forms.pas.1881: TaskActiveWindow := ActiveWindow;
A3D8784700       mov [$004778d8],eax
Forms.pas.1882: TaskFirstWindow := 0;
33C0             xor eax,eax
A3DC784700       mov [$004778dc],eax
Forms.pas.1883: TaskFirstTopMost := 0;
33C0             xor eax,eax
A3E0784700       mov [$004778e0],eax
Forms.pas.1884: EnumProc := @DoFindWindow;
BBBCB64500       mov ebx,$0045b6bc
Forms.pas.1885: EnumThreadWindows(GetCurrentThreadID, EnumProc, 0);
6A00             push $00
53               push ebx
E8DBCBFAFF       call GetCurrentThreadId
50               push eax
E821D1FAFF       call EnumThreadWindows

}
function GetAddrOfFindTopMostWindow: Pointer;
var
  P: PByte;
  Len: Integer;
  NeedsFix: Boolean;
begin
  NeedsFix := False;
  P := GetActualAddr(@EnableTaskWindows);
  Len := 0;
  while Len < 2048 do
  begin
    {  DoFindWindow "begin" }
    if (P[0] = $53) and
       (P[1] = $8B) and (P[2] = $D8) then
    begin
      if (P[3] = $3B) and (P[4] = $1D) and
         (P[9] = $74) and ({Release}(P[10] = $4D) or {Debug}(P[10] = $4F)) then
      begin
        NeedsFix := True;
      end
    end
    else { FindTopMostWindow "begin", Release & Debug }
    if (P[0] = $53) and
       (P[1] = $A3) then
    begin
      if (P[6] = $33) and (P[7] = $C0) and
         (P[8] = $A3) and
         (P[13] = $33) and (P[14] = $C0) and
         (P[15] = $A3) and
         (P[20] = $BB) and
         (P[25] = $6A) and (P[26] = $00) and
         (P[27] = $53) then
      begin
        Result := nil;
        if NeedsFix then
          Result := P;
        Exit;
      end;
    end;
    Inc(P);
    Inc(Len);
  end;
  Result := nil;
end;
{$ENDIF HideStackTrashingFix}
{ ---------------------------------------------------------------------------- }



{ ---------------------------------------------------------------------------- }
{ Control resize bugfix for kernel stack overflow due to WH_CALLWNDPROC hook }
{$IFDEF ControlResizeFix}
{2008-05-25:
  - Added code to detect endless resizing controls.
  - Added experimental OPTIMIZED_RESIZE_REDRAW option for faster form resizing }
var
  WinControl_AlignControlProc, WinControl_WMSize, WinControl_SetBounds: Pointer;
  BackupAlignControl, BackupWMSize, BackupSetBounds: TXRedirCode;

type
  TControlResizeFixWinControl = class(TWinControl)
  private
    procedure AlignControl(AControl: TControl);
    procedure HandleAlignControls(AControl: TControl; var R: TRect);
  protected
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

  {$IFNDEF DELPHI2005_UP}
  TD5WinControlPrivate = class(TControl)
  public
    FAlignLevel: Word;
  end;
  {$ENDIF ~DELPHI2005_UP}

threadvar
  AlignControlList: TList;

procedure TControlResizeFixWinControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
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

procedure TControlResizeFixWinControl.HandleAlignControls(AControl: TControl; var R: TRect);

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

procedure TControlResizeFixWinControl.WMSize(var Message: TWMSize);
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

procedure TControlResizeFixWinControl.AlignControl(AControl: TControl);
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

procedure InitControlResizeFix;
begin
  WinControl_AlignControlProc := GetAlignControlProc;
  WinControl_WMSize := GetDynamicMethod(TWinControl, WM_SIZE);
  WinControl_SetBounds := @TOpenWinControl.SetBounds;
  if (WinControl_AlignControlProc <> nil) and (WinControl_WMSize <> nil) then
  begin
    DebugLog('ControlResizeFix');
    { Redirect the original function to the bug fixed version }
    HookProc(WinControl_AlignControlProc, @TControlResizeFixWinControl.AlignControl, BackupAlignControl);
    HookProc(WinControl_WMSize, @TControlResizeFixWinControl.WMSize, BackupWMSize);
    {$IFDEF OPTIMIZED_RESIZE_REDRAW}
    HookProc(WinControl_SetBounds, @TControlResizeFixWinControl.SetBounds, BackupSetBounds);
    {$ENDIF OPTIMIZED_RESIZE_REDRAW}
  end;
end;

procedure FiniControlResizeFix;
begin
  { Restore the original function }
  UnhookProc(WinControl_AlignControlProc, BackupAlignControl);
  UnhookProc(WinControl_WMSize, BackupWMSize);
  UnhookProc(WinControl_SetBounds, BackupSetBounds);
end;

{$ENDIF ControlResizeFix}
{ ---------------------------------------------------------------------------- }



{ ---------------------------------------------------------------------------- }
{ QC #59654: TActionList access already released FActions field }
{$IFDEF ActionListAVFix}
var
  HookTCustomActionList_Notification: TXRedirCode;

type
  TCustomActionListFix = class(TCustomActionList)
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  end;

procedure TCustomActionListFix.Notification(AComponent: TComponent; Operation: TOperation);
var
  P: procedure(Instance: TComponent; AComponent: TComponent; Operation: TOperation);
begin
  { inherited: }
  P := @TOpenComponent.Notification;
  P(Self, AComponent, Operation);

  if Operation = opRemove then
  begin
    if AComponent = Images then
      Images := nil
    else if {<*}not (csDestroying in ComponentState) and{*>} (AComponent is TContainedAction) then
      RemoveAction(TContainedAction(AComponent));
  end;
end;

procedure InitActionListAVFix;
begin
  DebugLog('ActionListAVFix');
  HookProc(@TOpenCustomActionList.Notification, @TCustomActionListFix.Notification, HookTCustomActionList_Notification);
end;

procedure FiniActionListAVFix;
begin
  UnhookProc(@TOpenCustomActionList.Notification, HookTCustomActionList_Notification);
end;
{$ENDIF ActionListAVFix}
{ ---------------------------------------------------------------------------- }



{ ---------------------------------------------------------------------------- }
{ QC #54286 : Parent-PopupMenu overrides standard context menu (edit, memo, combobox, ...) }
{$IFDEF ContextMenuFix}
type
  TContextMenuFixWinControl = class(TWinControl)
  public
    procedure DefaultHandler(var Message); override;
  end;

var
  RM_GetObjectInstance: DWORD;
  BackupDefaultHandler: TXRedirCode;

procedure TContextMenuFixWinControl.DefaultHandler(var Message);
type
  TDefHandler = procedure(Self: TControl; var Message);
begin
  if HandleAllocated then
  begin
    with TMessage(Message) do
    begin
      { Here was the WM_CONTEXTMENU Code that is not necessary because
        DefWndProc will send this message to the parent control. }

      { Keep the odd bahavior for grids because everybody seems to be used to it. }
      if (Msg = WM_CONTEXTMENU) and (Parent <> nil) and (Parent is TCustomGrid) then
      begin
        Result := Parent.Perform(Msg, WParam, LParam);
        if Result <> 0 then Exit;
      end;

      case Msg of
        WM_CTLCOLORMSGBOX..WM_CTLCOLORSTATIC:
          Result := SendMessage(LParam, CN_BASE + Msg, WParam, LParam);
        CN_CTLCOLORMSGBOX..CN_CTLCOLORSTATIC:
          begin
            SetTextColor(WParam, ColorToRGB(Font.Color));
            SetBkColor(WParam, ColorToRGB(Brush.Color));
            Result := Brush.Handle;
          end;
      else
        if Msg = RM_GetObjectInstance then
          Result := LRESULT(Self)
        else
          Result := CallWindowProc(DefWndProc, Handle, Msg, WParam, LParam);
      end;
      if Msg = WM_SETTEXT then
        SendDockNotification(Msg, WParam, LParam);
    end;
  end
  else
    //inherited DefaultHandler(Message);
    TDefHandler(@TControl.DefaultHandler)(Self, Message);
end;

procedure InitContextMenuFix;
var
  HInst: HMODULE;
begin
  HInst := FindHInstance(Pointer(TWinControl)); // get the HInstance of the module that contains Controls.pas
  RM_GetObjectInstance := RegisterWindowMessage(PChar(Format('ControlOfs%.8X%.8X', [HInst, GetCurrentThreadId])));

  DebugLog('ContextMenuFix');
  { Redirect the original function to the bug fixed version }
  HookProc(@TWinControl.DefaultHandler, @TContextMenuFixWinControl.DefaultHandler, BackupDefaultHandler);
end;

procedure FiniContextMenuFix;
begin
  UnhookProc(@TWinControl.DefaultHandler, BackupDefaultHandler);
end;
{$ENDIF ContextMenuFix}
{ ---------------------------------------------------------------------------- }



{ ---------------------------------------------------------------------------- }
{ QC #50097: ObjAuto access violation on XEON (Data Execution Prevention bug) }
{$IFDEF ObjAutoDEPFix}

type
  PParameterInfos = ^TParameterInfos;
  TParameterInfos = array[0..255] of ^PTypeInfo;

  TMethodHandlerInstance = class
  protected
    MethodHandler: IMethodHandler;
    TypeData: PTypeData;
    ParamInfos: PParameterInfos;
    Return: array[0..2] of Byte;
    ParamOffsets: array of Word;
  end;

{ procedure TMethodHandlerInstance.RegisterStub;
 50               push eax
 51               push ecx
 52               push edx
 89E2             mov edx,esp
 E866FDFFFF       call TMethodHandlerInstance.Handler
 89442404         mov [esp+$04],eax
 58               pop eax
 58               pop eax
 59               pop ecx
 8D4910           lea ecx,[ecx+$10]
 FFE1             jmp ecx
 C3               ret }

type
  PRegisterStubRec = ^TRegisterStubRec;
  TRegisterStubRec = packed record
    Push3Reg: array[0..2] of Byte;
    MovEdxEsp: Word;
    CallHandler: array[0..4] of Byte;
    SaveRetValue: LongWord;
    Pop3Reg: array[0..2] of Byte;
    case Integer of
      0: (LeaEcxEcx: Word;
          Off: Byte;
          JmpEcx: Word);
      1: (Jmp: Byte;
          Offset: TJumpOfs);
  end;

var
  RegisterStub: PRegisterStubRec;
  OrgCode: array[0..4] of Byte;

procedure RegisterStubRet;
asm
  lea ecx, [ecx].TMethodHandlerInstance.Return
  cmp byte ptr [ecx], $C2
  jne @@Leave
  movzx ecx, word ptr [ecx+$01]
  add ecx, 3
  and ecx, $FC
  add esp, ecx

  { restore return address }
  neg ecx
  mov ecx, [esp+ecx] // load return address
  push ecx // restore return address
@@Leave:
end;

procedure InitObjAutoDEPFix;
var
  Func: TTypeData;
  M: TMethod;
  OldProtect: Cardinal;
begin
  FillChar(Func, SizeOf(Func), 0);
  Func.MethodKind := mkProcedure;
  Func.ParamCount := 0;
  M := CreateMethodPointer(nil, @Func);
  RegisterStub := M.Code;
  ReleaseMethodPointer(M);

  if (RegisterStub.Push3Reg[0] = $50) and
     (RegisterStub.Push3Reg[1] = $51) and
     (RegisterStub.Push3Reg[2] = $52) and
     (RegisterStub.MovEdxEsp = $E289) and
     (RegisterStub.SaveRetValue = $04244489) and
     (RegisterStub.Pop3Reg[0] = $58) and
     (RegisterStub.Pop3Reg[1] = $58) and
     (RegisterStub.Pop3Reg[2] = $59) and
     (RegisterStub.LeaEcxEcx = $498D) and
     (RegisterStub.JmpEcx = $E1FF) then
  begin
    if VirtualProtect(@RegisterStub.Jmp, SizeOf(TXRedirCode), PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      DebugLog('ObjAutoDEPFix');
      Move(RegisterStub.Jmp, OrgCode[0], SizeOf(OrgCode));
      RegisterStub.Jmp := $E9;
      RegisterStub.Offset := TJumpOfs(@RegisterStubRet) - (TJumpOfs(@RegisterStub.Jmp) + SizeOf(TXRedirCode));
      VirtualProtect(@RegisterStub.Jmp, SizeOf(TXRedirCode), OldProtect, OldProtect);
      FlushInstructionCache(GetCurrentProcess, @RegisterStub.Jmp, SizeOf(TXRedirCode));
    end;
  end
  else
    RegisterStub := nil;
end;

procedure FiniObjAutoDEPFix;
var
  OldProtect: Cardinal;
begin
  if RegisterStub <> nil then
  begin
    if VirtualProtect(@RegisterStub.Jmp, SizeOf(TXRedirCode), PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      Move(OrgCode[0], RegisterStub.Jmp, SizeOf(OrgCode));
      VirtualProtect(@RegisterStub.Jmp, SizeOf(TXRedirCode), OldProtect, OldProtect);
      FlushInstructionCache(GetCurrentProcess, @RegisterStub.Jmp, SizeOf(TXRedirCode));
    end;
  end;
end;
{$ENDIF ObjAutoDEPFix}
{ ---------------------------------------------------------------------------- }



{ ---------------------------------------------------------------------------- }
{ Classes.MakeObjectInstance memory leak fix }
{$IFDEF MkObjInstLeakFix}
{ Limitation:
    The memory is only released if there is no dangling object instance in the
    memory block. }
var
  UnRegisterModuleClassesHook: TXRedirCode;
  MkObjInstLeakHooked: Boolean;

procedure HookedUnRegisterModuleClasses(Module: HMODULE);
  forward;


const
  InstanceCount = 313;
  PageSize = 4096;

{ Object instance management }

type
  PPObjectInstance = ^PObjectInstance;
  PObjectInstance = ^TObjectInstance;
  TObjectInstance = packed record
    Code: Byte;
    Offset: Integer;
    case Integer of
      0: (Next: PObjectInstance);
      1: (Method: TWndMethod);
  end;

  PPInstanceBlock = ^PInstanceBlock;
  PInstanceBlock = ^TInstanceBlock;
  TInstanceBlock = packed record
    Next: PInstanceBlock;
    Code: array[1..2] of Byte;
    WndProcPtr: Pointer;
    Instances: array[0..InstanceCount] of TObjectInstance;
  end;

(*
 procedure FreeObjectInstance(ObjectInstance: Pointer);
 begin
   if ObjectInstance <> nil then
   begin
    // 85C0             test eax,eax
    // 740E             jz $0041b2f6
     PObjectInstance(ObjectInstance)^.Next := InstFreeList;
    // 8B15E8594600     mov edx,[$004659e8]
    // 895005           mov [eax+$05],edx
     InstFreeList := ObjectInstance;
    // A3E8594600       mov [$004659e8],eax
   end;
 end;
    // C3               ret
*)

type
  TParamRec = packed record
    Op: Word;
    Off: Byte;
  end;

  PFreeObjInstRec = ^TFreeObjInstRec;
  TFreeObjInstRec = packed record
    TestEaxEax: Word;
    Jz1: Word;
    AssignToNext: packed record
                    MovEdx: Word;
                    Address: Cardinal;
                    MovEaxOffEdx: Word;
                    Off: Byte;
                  end;
    AssignToInstFreeList: packed record
                            MovMemToEax: Byte;
                            Address: Cardinal;
                          end;
    Ret: Byte;
  end;

  PFreeObjInstNoOptRec = ^TFreeObjInstNoOptRec;
  TFreeObjInstNoOptRec = packed record
    PushEbp: Byte;                 // $55
    MovEbpEsp: Word;               // $8B EC
    PushEcx: Byte;                 // $51
    MovOffReg: TParamRec;          // $89 45 FC
    Cmp: packed record
           Op: Word;               // $83 7D
           Off: Byte;              // $FC
           Value: Byte;            // $00
         end;
    Jz1: Word;                     // $74 14
    LoadParam1: TParamRec;         // $8B 45 FC
    AssignToNext: packed record
                    MovEdx: Word;  // $8B 15
                    Address: Cardinal;
                  end;
  end;

procedure GetObjectInstancePointers(out InstFreeListP: PPObjectInstance; out InstBlockListP: PPInstanceBlock);
var
  FreeObjInst: PFreeObjInstRec;
  FreeObjInstNoOpt: PFreeObjInstNoOptRec;
begin
  InstFreeListP := nil;
  InstBlockListP := nil;
  FreeObjInst := GetActualAddr(@Classes.FreeObjectInstance);
  FreeObjInstNoOpt := Pointer(FreeObjInst);

  if (FreeObjInst.TestEaxEax = $C085) and (FreeObjInst.Jz1 = $0E74) and
     (FreeObjInst.AssignToNext.MovEdx = $158B) and
     (FreeObjInst.AssignToNext.MovEaxOffEdx = $5089) and (FreeObjInst.AssignToNext.Off = $05) and
     (FreeObjInst.AssignToInstFreeList.MovMemToEax = $A3) and
     (FreeObjInst.Ret = $C3) then
  begin
    InstFreeListP := PPObjectInstance(FreeObjInst.AssignToNext.Address);
    InstBlockListP := PPInstanceBlock(FreeObjInst.AssignToNext.Address - SizeOf(Pointer));
  end
  else
  if (FreeObjInstNoOpt.PushEbp = $55) and (FreeObjInstNoOpt.MovEbpEsp = $EC8B) and
     (FreeObjInstNoOpt.PushEcx = $51) and
     (FreeObjInstNoOpt.MovOffReg.Op = $4589) and (FreeObjInstNoOpt.MovOffReg.Off = $FC) and
     (FreeObjInstNoOpt.Cmp.Op = $7D83) and (FreeObjInstNoOpt.Cmp.Off = $FC) and (FreeObjInstNoOpt.Cmp.Value = $00) and
     (FreeObjInstNoOpt.Jz1 = $1474) and
     (FreeObjInstNoOpt.LoadParam1.Op = $458B) and (FreeObjInstNoOpt.LoadParam1.Off = $FC) and
     (FreeObjInstNoOpt.AssignToNext.MovEdx = $158B) then
  begin
    InstFreeListP := PPObjectInstance(FreeObjInstNoOpt.AssignToNext.Address);
    InstBlockListP := PPInstanceBlock(FreeObjInstNoOpt.AssignToNext.Address - SizeOf(Pointer));
  end;
end;

procedure CleanupInstFreeList(var InstFreeList: PObjectInstance; BlockStart, BlockEnd: PAnsiChar);
var
  Prev, Next, Item: PObjectInstance;
begin
  Prev := nil;
  Item := InstFreeList;
  while Item <> nil do
  begin
    Next := Item.Next;
    if (PAnsiChar(Item) >= BlockStart) and (PAnsiChar(Item) <= BlockEnd) then
    begin
      Item := Prev;
      if Prev = nil then
        InstFreeList := Next
      else
        Prev.Next := Next;
    end;
    Prev := Item;
    Item := Next;
  end;
end;

function CalcFreeInstBlockItems(Item: PObjectInstance; Block: PInstanceBlock): Integer;
var
  I: Integer;
begin
  Result := 0;
  while Item <> nil do
  begin
    for I := High(Block.Instances) downto 0 do
    begin
      if @Block.Instances[I] = Item then
      begin
        Inc(Result);
        Break;
      end;
    end;
    Item := Item.Next;
  end;
end;

procedure ReleaseObjectInstanceBlocks;
var
  InstFreeListP: PPObjectInstance;
  InstBlockListP: PPInstanceBlock;
  NextBlock, Block, PrevBlock: PInstanceBlock;
  FreeCount: Integer;
begin
  GetObjectInstancePointers(InstFreeListP, InstBlockListP);
  if (InstFreeListP = nil) or (InstBlockListP = nil) then
  begin
    OutputDebugString('Cannot apply Classes.FreeObjectInstance memory leak-fix');
    Exit;
  end;

  Block := InstBlockListP^;
  PrevBlock := nil;
  while Block <> nil do
  begin
    NextBlock := Block.Next;

    { Obtain the number of free items in the InstanceBlock }
    FreeCount := CalcFreeInstBlockItems(InstFreeListP^, Block);

    { Release memory if the InstanceBlock contains only "free" items }
    if FreeCount = Length(Block.Instances) then
    begin
      { Remove all InstFreeList items that refer to the InstanceBlock }
      CleanupInstFreeList(InstFreeListP^, PAnsiChar(Block), PAnsiChar(Block) + SizeOf(TInstanceBlock) - 1);

      VirtualFree(Block, 0, MEM_RELEASE);

      Block := PrevBlock;
      if PrevBlock = nil then
        InstBlockListP^ := NextBlock
      else
        PrevBlock.Next := NextBlock;
    end;

    { Next InstanceBlock }
    PrevBlock := Block;
    Block := NextBlock;
  end;

  { Maybe the finalization was executed before the Application object was destroyed.
    Classes.Finalization calls UnRegisterModuleClasses(). We hook into that function
    to execute our finalization in the Classes.Finalization. }
  if (InstBlockListP^ <> nil) and not MkObjInstLeakHooked and
     (FindHInstance(GetActualAddr(@UnRegisterModuleClasses)) = HInstance) then
  begin
    MkObjInstLeakHooked := True;
    HookProc(@UnRegisterModuleClasses, @HookedUnRegisterModuleClasses, UnRegisterModuleClassesHook);
  end;
end;

procedure HookedUnRegisterModuleClasses(Module: HMODULE);
begin
  UnhookProc(@UnRegisterModuleClasses, UnRegisterModuleClassesHook);
  try
    UnRegisterModuleClasses(Module);
    if Module = HInstance then
      ReleaseObjectInstanceBlocks;
  finally
    HookProc(@UnRegisterModuleClasses, @HookedUnRegisterModuleClasses, UnRegisterModuleClassesHook);
  end;
end;
{$ENDIF MkObjInstLeakFix}
{ ---------------------------------------------------------------------------- }



{ ---------------------------------------------------------------------------- }
{ QC #58938: MainForm Minimize minimizes in the background }
{$IFDEF AppMinimizeFix}
var
  AppMinimizePatchAddress: ^TJumpOfs;
  OrgAppMinimizeSetActiveWindowOffset: TJumpOfs;

function ApplicationMinimizeSetActiveWindow(hWnd: HWND): HWND; stdcall;
begin
  if Application.MainFormOnTaskBar then
    Result := GetActiveWindow
  else
    Result := SetActiveWindow(hWnd);
end;

{
Forms.pas.7850: NormalizeTopMosts;
8BC3             mov eax,ebx
E84AF4FFFF       call TApplication.NormalizeTopMosts
Forms.pas.7851: SetActiveWindow(FHandle); // WM_ACTIVATEAPP can set AppIconic to False
8B4330           mov eax,[ebx+$30]
50               push eax
E87D1AFAFF       call SetActiveWindow
Forms.pas.7852: AppIconic := True;        // Set AppIconic here just to be safe
C605A8EC4B0001   mov byte ptr [$004beca8],$01
}

procedure InitAppMinimizeFix;
var
  P: PAnsiChar;
  Len: Integer;
  NewOffset: TJumpOfs;
  n: Cardinal;
begin
  P := GetActualAddr(@TApplication.Minimize);
  Len := 0;
  while Len < 64 do
  begin
    if (P[0] = #$8B) and (P[1] = #$C3) and // mov eax,ebx
       (P[2] = #$E8) and // call TApplication.NormalizeTopMosts
       (P[7] = #$8B) and (P[8] = #$43) and (P[9] = #$30) and // mov eax,[ebx+$30]
       (P[10] = #$50) and // push eax
       (P[11] = #$E8) and // call SetActiveWindow
       (P[16] = #$C6) and (P[17] = #$05) and (P[22] = #$01) then // mov byte ptr [$004beca8],$01
    begin
      DebugLog('AppMinimizeFix');
      AppMinimizePatchAddress := Pointer(P + 12);
      OrgAppMinimizeSetActiveWindowOffset := AppMinimizePatchAddress^;
      NewOffset := PAnsiChar(@ApplicationMinimizeSetActiveWindow) - PAnsiChar(AppMinimizePatchAddress) - SizeOf(TJumpOfs);
      WriteProcessMemory(GetCurrentProcess, AppMinimizePatchAddress, @NewOffset, SizeOf(NewOffset), n);
      Exit;
    end;
    Inc(P);
    Inc(Len);
  end;
end;

procedure FiniAppMinimizeFix;
var
  n: Cardinal;
begin
  if AppMinimizePatchAddress <> nil then
    WriteProcessMemory(GetCurrentProcess, AppMinimizePatchAddress, @OrgAppMinimizeSetActiveWindowOffset, SizeOf(TJumpOfs), n);
end;
{$ENDIF AppMinimizeFix}
{ ---------------------------------------------------------------------------- }



{ ---------------------------------------------------------------------------- }
{ QC #64484: SysUtils.Abort can raise an AccessViolation }
{$IFDEF SysUtilsAbortFix}
var
  SysUtilsAbortHook: TXRedirCode;

procedure SysUtilsAbort;
{ No dependency on EBP register }

  procedure ThrowException(ReturnAddr: Pointer);
  begin
    raise EAbort.CreateRes(@SOperationAborted) at ReturnAddr;
  end;

asm
  pop eax
  jmp ThrowException
end;

procedure InitSysUtilsAbortFix;
begin
  DebugLog('SysUtilsAbortFix');
  HookProc(@SysUtils.Abort, @SysUtilsAbort, SysUtilsAbortHook);
end;

procedure FiniSysUtilsAbortFix;
begin
  UnhookProc(@SysUtils.Abort, SysUtilsAbortHook);
end;
{$ENDIF SysUtilsAbortFix}
{ ---------------------------------------------------------------------------- }



{ ---------------------------------------------------------------------------- }
{ QC #58939: No taskbar button when starting from shelllink with Show=Minimized }
{$IFDEF CmdShowMinimizeFix}
var
  ApplicationRunHook: TXRedirCode;
  InitialMainFormState: ^TWindowState;

procedure ApplicationRun(App: TApplication);
begin
  UnhookProc(@TApplication.Run, ApplicationRunHook);
  ApplicationRunHook.Jump := 0;
  {$WARNINGS OFF}
  if (CmdShow = SW_SHOWMINNOACTIVE) and (InitialMainFormState <> nil) then
  {$WARNINGS ON}
    InitialMainFormState^ := wsMinimized;
  App.Run;
end;

{
Forms.pas.8214: if (FMainForm.FWindowState = wsMinimized) or (InitialMainFormState = wsMinimized) then
8B45FC           mov eax,[ebp-$04]
8B4044           mov eax,[eax+$44]
80B87302000001   cmp byte ptr [eax+$00000273],$01
7409             jz $004669a5
803DC8784B0001   cmp byte ptr [$004b78c8],$01
751E             jnz $004669c3
Forms.pas.8216: Minimize;
8B45FC           mov eax,[ebp-$04]
E8FFF5FFFF       call TApplication.Minimize
Forms.pas.8217: if (InitialMainFormState = wsMinimized) then
803DC8784B0001   cmp byte ptr [$004b78c8],$01
7514             jnz $004669ca
}

function FindInitialMainFormState: Pointer;
var
  P: PAnsiChar;
  Len: Integer;
begin
  P := GetActualAddr(@TApplication.Run);
  Len := 0;
  while Len < 128 do
  begin
    if (P[0] = #$74) and (P[1] = #$09) and // jz $004669a5
       (P[2] = #$80) and (P[3] = #$3D) and (P[8] = #$01) and // cmp byte ptr [$004b78c8],$01
       (P[9] = #$75) and (P[10] = #$1E) and // jnz $004669c3
       (P[11] = #$8B) and (P[12] = #$45) and (P[13] = #$FC) then // mov eax,[ebp-$04]
    begin
      DebugLog('CmdShowMinimizeFix');
      Result := PPointer(P + 4)^;
      Exit;
    end;
    Inc(P);
    Inc(Len);
  end;
  Result := nil;
end;

procedure InitCmdShowMinimizeFix;
begin
  InitialMainFormState := FindInitialMainFormState;
  if InitialMainFormState <> nil then
    HookProc(@TApplication.Run, @ApplicationRun, ApplicationRunHook);
end;

procedure FiniCmdShowMinimizeFix;
begin
  UnhookProc(@TApplication.Run, ApplicationRunHook);
end;

{$ENDIF CmdShowMinimizeFix}
{ ---------------------------------------------------------------------------- }



{ ---------------------------------------------------------------------------- }
{ QC #35001: MDIChild's active control focus is not set correctly }
{$IFDEF MDIChildFocusFix}
var
  CustomFormFocusControlHook: TXRedirCode;

procedure CustomFormFocusControl(Self: TOpenCustomForm; Control: TWinControl);
var
  WasActive: Boolean;
begin
  with Self do
  begin
    WasActive := Active;

    { Synchronize Windows's focus with VCL's focus }
    if WasActive and (FormStyle = fsMDIChild) and (Control <> nil) and (Control = ActiveControl) and
       Control.HandleAllocated and not Control.Focused then
    begin
      Windows.SetFocus(Control.Handle);
    end;

    ActiveControl := Control;
    if not WasActive then
      SetFocus;
  end;
end;

procedure InitMDIChildFocusFix;
begin
  DebugLog('MDIChildFocusFix');
  HookProc(@TOpenCustomForm.FocusControl, @CustomFormFocusControl, CustomFormFocusControlHook);
end;

procedure FiniMDIChildFocusFix;
begin
  UnhookProc(@TOpenCustomForm.FocusControl, CustomFormFocusControlHook);
end;
{$ENDIF MDIChildFocusFix}
{ ---------------------------------------------------------------------------- }



{ ---------------------------------------------------------------------------- }
{ QC #56252: TPageControl flickers a lot with active theming }
{ QC #68730: TLabel is not painted on a themed, double-buffered TTabSheet in Vista }
{ TLabels on TTabSheet are not painted (themes) if a TWinControl like TMemo is on the TTabSheet (TWinControl.PaintWindow bug) }
{$IFDEF PageControlPaintingFix}
type
  TFlickerlessPageControl = class(TPageControl)
    procedure NewWndProc(var Msg: TMessage);
  end;

  TFlickerlessTabSheet = class(TTabSheet)
  protected
    procedure NewCreateParams(var Params: TCreateParams);
    procedure WMPrintClient(var Message: TWMPrintClient); message WM_PRINTCLIENT;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;

    procedure NewWndProc(var Msg: TMessage);
  end;

{ TFlickerlessPageControl }

procedure TFlickerlessPageControl.NewWndProc(var Msg: TMessage);
begin
  case Msg.Msg of
    WM_SIZE:
      begin
        inherited WndProc(Msg);
        { Update the page control immediately to prevent flicker }
        if ThemeServices.ThemesEnabled then
          RedrawWindow(Handle, nil, 0, RDW_UPDATENOW or RDW_ERASE);
      end;
  else
    inherited WndProc(Msg); // "inherited" is required here, otherwise this would be an endless recursion
  end;
end;

{ TFlickerlessTabSheet }

procedure TFlickerlessTabSheet.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  R: TRect;
begin
  if (PageControl <> nil) and (PageControl.Style = tsTabs) and ThemeServices.ThemesEnabled then
  begin
    { Paint the TabSheet background without filling the gray color from the parent.
      And fill it in WM_ERASEBKGND where it belongs instead of WM_PRINTCLIENT. }
    GetWindowRect(Handle, R);
    OffsetRect(R, -R.Left, -R.Top);
    ThemeServices.DrawElement(Message.DC, ThemeServices.GetElementDetails(ttBody), R);
    Message.Result := 1;
  end
  else
    inherited;
end;

procedure TFlickerlessTabSheet.WMPrintClient(var Message: TWMPrintClient);
begin
  { Fixes "Labels are not painted if themes are enabled" bug }

  { Description see QC #3850 / RAID #159864, same as in TFrame.

    A better solution would be to change TWinControl.PaintWindow to
    use WM_PRINTCLIENT if it is called from a WM_PRINTCLIENT handler
    and to use WM_PAINT only if it is called from a WM_PAINT handler. }

  DefaultHandler(Message);
  PaintControls(Message.DC, nil);
end;

procedure TFlickerlessTabSheet.NewCreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if ThemeServices.ThemesEnabled then
    Params.WindowClass.style := Params.WindowClass.style and not (CS_VREDRAW or CS_HREDRAW);
end;

procedure TFlickerlessTabSheet.NewWndProc(var Msg: TMessage);
begin
  { Instead of hooking the DMT we simply call the replacement handlers directly }
  case Msg.Msg of
    WM_ERASEBKGND:
      WMEraseBkgnd(TWMEraseBkgnd(Msg));
    WM_PRINTCLIENT:
      WMPrintClient(TWMPrintClient(Msg));
    WM_SIZE:
      begin
        inherited WndProc(Msg);
        if ThemeServices.ThemesEnabled then
          RedrawWindow(Handle, nil, 0, RDW_UPDATENOW or RDW_INVALIDATE or RDW_ERASE or RDW_ALLCHILDREN);
      end;
  else
    inherited WndProc(Msg); // "inherited" is required here, otherwise this would be an endless recursion
  end;
end;

procedure InitPageControlPaintingFix;
begin
  DebugLog('PageControlPaintingFix');
  ReplaceVmtField(TTabSheet, @TFlickerlessTabSheet.WndProc, @TFlickerlessTabSheet.NewWndProc);
  ReplaceVmtField(TTabSheet, @TFlickerlessTabSheet.CreateParams, @TFlickerlessTabSheet.NewCreateParams);
  ReplaceVmtField(TPageControl, @TFlickerlessPageControl.WndProc, @TFlickerlessPageControl.NewWndProc);
end;

procedure FiniPageControlPaintingFix;
begin
  ReplaceVmtField(TTabSheet, @TFlickerlessTabSheet.NewWndProc, @TFlickerlessTabSheet.WndProc);
  ReplaceVmtField(TTabSheet, @TFlickerlessTabSheet.NewCreateParams, @TFlickerlessTabSheet.CreateParams);
  ReplaceVmtField(TPageControl, @TFlickerlessPageControl.NewWndProc, @TFlickerlessPageControl.WndProc);
end;
{$ENDIF PageControlPaintingFix}
{---------------------------------------------------------------------------}



{---------------------------------------------------------------------------}
{$IFDEF GridFlickerFix}
type
  {$IFDEF VCLFIXPACK_DB_SUPPORT}
  TFlickerlessDBGrid = class(TDBGrid)
  protected
    procedure NewWndProc(var Msg: TMessage);
  end;
  {$ENDIF VCLFIXPACK_DB_SUPPORT}

  TFlickerlessStringGrid = class(TStringGrid)
  protected
    procedure NewWndProc(var Msg: TMessage);
  end;

  TFlickerlessDrawGrid = class(TDrawGrid)
  protected
    procedure NewWndProc(var Msg: TMessage);
  end;

{ TFlickerlessDBGrid }

procedure GridWMEraseBkgnd(Grid: TCustomGrid; var Message: TWMEraseBkgnd);
var
  R: TRect;
  Size: TSize;
begin
  { Fill the area between the two scroll bars. }
  Size.cx := GetSystemMetrics(SM_CXVSCROLL);
  Size.cy := GetSystemMetrics(SM_CYHSCROLL);
  if {Grid.BiDiMode <> bdLeftToRight}Grid.UseRightToLeftAlignment then
    R := Bounds(0, Grid.Height - Size.cy, Size.cx, Size.cy)
  else
    R := Bounds(Grid.Width - Size.cx, Grid.Height - Size.cy, Size.cx, Size.cy);
  FillRect(Message.DC, R, Grid.Brush.Handle);
  Message.Result := 1;
end;

{$IFDEF VCLFIXPACK_DB_SUPPORT}
procedure TFlickerlessDBGrid.NewWndProc(var Msg: TMessage);
{var
  Rect: TRect;}
begin
  if Msg.Msg = WM_ERASEBKGND then
    GridWMEraseBkgnd(Self, TWMEraseBkgnd(Msg))
  else if Msg.Msg = WM_PAINT then
  begin
    {if UseRightToLeftAlignment then
    Begin
      Rect.TopLeft := ClientRect.TopLeft;
      Rect.BottomRight := ClientRect.BottomRight;
      InvalidateRect(Handle, @Rect, False);
    end;}
    inherited WndProc(Msg);
  end
  else
    inherited WndProc(Msg);
end;
{$ENDIF VCLFIXPACK_DB_SUPPORT}

procedure TFlickerlessStringGrid.NewWndProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_ERASEBKGND then
    GridWMEraseBkgnd(Self, TWMEraseBkgnd(Msg))
  else
    inherited WndProc(Msg);
end;

procedure TFlickerlessDrawGrid.NewWndProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_ERASEBKGND then
    GridWMEraseBkgnd(Self, TWMEraseBkgnd(Msg))
  else
    inherited WndProc(Msg);
end;

procedure InitGridFlickerFix;
begin
  DebugLog('GridFlickerFix');
  {$IFDEF VCLFIXPACK_DB_SUPPORT}
  ReplaceVmtField(TDBGrid, @TFlickerlessDBGrid.WndProc, @TFlickerlessDBGrid.NewWndProc);
  {$ENDIF VCLFIXPACK_DB_SUPPORT}
  ReplaceVmtField(TStringGrid, @TFlickerlessStringGrid.WndProc, @TFlickerlessStringGrid.NewWndProc);
  ReplaceVmtField(TDrawGrid, @TFlickerlessDrawGrid.WndProc, @TFlickerlessDrawGrid.NewWndProc);
end;

procedure FiniGridFlickerFix;
begin
  {$IFDEF VCLFIXPACK_DB_SUPPORT}
  ReplaceVmtField(TDBGrid, @TFlickerlessDBGrid.NewWndProc, @TFlickerlessDBGrid.WndProc);
  {$ENDIF VCLFIXPACK_DB_SUPPORT}
  ReplaceVmtField(TStringGrid, @TFlickerlessStringGrid.NewWndProc, @TFlickerlessStringGrid.WndProc);
  ReplaceVmtField(TDrawGrid, @TFlickerlessDrawGrid.NewWndProc, @TFlickerlessDrawGrid.WndProc);
end;
{$ENDIF GridFlickerFix}
{---------------------------------------------------------------------------}



{ ---------------------------------------------------------------------------- }
{ QC #69112: TSpeedButton is painted as a black rectangle on a double buffered panel on a sheet of glass. }
{$IFDEF SpeedButtonGlassFix}
type
  TGlassableSpeedButton = class(TSpeedButton)
    procedure NewWndProc(var Msg: TMessage);
  end;

function ControlInGlassPaint(AControl: TControl): Boolean;
var
  Parent: TWinControl;
begin
  Result := csGlassPaint in AControl.ControlState;
  if Result then
  begin
    { Control could be on a double buffered control. In that case the csGlassPaint flag
      shouldn't be set. }
    Parent := AControl.Parent;
    while (Parent <> nil) and not Parent.DoubleBuffered and not (Parent is TCustomForm) do
      Parent := Parent.Parent;
    Result := (Parent = nil) or not Parent.DoubleBuffered or (Parent is TCustomForm);
  end;
end;

{ TGlassableSpeedButton }

procedure TGlassableSpeedButton.NewWndProc(var Msg: TMessage);
begin
  if (Msg.Msg = WM_PAINT) and (csGlassPaint in ControlState) and
     not ControlInGlassPaint(Self) then
  begin
    ControlState := ControlState - [csGlassPaint];
    try
      inherited WndProc(Msg);
    finally
      ControlState := ControlState + [csGlassPaint];
    end;
  end
  else
    inherited WndProc(Msg);
end;

procedure InitSpeedButtonGlassFix;
begin
  DebugLog('SpeedButtonGlassFix');
  ReplaceVmtField(TSpeedButton, @TGlassableSpeedButton.WndProc, @TGlassableSpeedButton.NewWndProc);
end;

procedure FiniSpeedButtonGlassFix;
begin
  ReplaceVmtField(TSpeedButton, @TGlassableSpeedButton.NewWndProc, @TGlassableSpeedButton.WndProc);
end;
{$ENDIF SpeedButtonGlassFix}
{ ---------------------------------------------------------------------------- }



{ ---------------------------------------------------------------------------- }
{$IFDEF VistaProgressBarMarqueeFix}
{ QC #69294: TProgressBar fails with PBS_MARQUEE and disabled Themes }
type
  TVistaProgressBarMarqueeFix = class(TProgressBar)
  protected
    procedure SetMarqueeInterval(Value: Integer);
    procedure CreateParamsFix(var Params: TCreateParams);
    procedure CreateWndFix;
  end;

var
  SetMarqueeIntervalHook: TXRedirCode;
  SetMarqueeInterval: Pointer;

procedure TVistaProgressBarMarqueeFix.SetMarqueeInterval(Value: Integer);
var
  MarqueeEnabled: Boolean;
begin
  PInteger(@MarqueeInterval)^ := Value;
  if (Style = pbstMarquee) and HandleAllocated then
  begin
    MarqueeEnabled := Style = pbstMarquee;
    SendMessage(Handle, PBM_SETMARQUEE, WPARAM(MarqueeEnabled), LPARAM(MarqueeInterval));
  end;
end;

procedure TVistaProgressBarMarqueeFix.CreateParamsFix(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if Style = pbstMarquee then
    Params.Style := Params.Style or PBS_MARQUEE;
end;

procedure TVistaProgressBarMarqueeFix.CreateWndFix;
var
  MarqueeEnabled: Boolean;
begin
  inherited CreateWnd;
  MarqueeEnabled := Style = pbstMarquee;
  SendMessage(Handle, PBM_SETMARQUEE, WPARAM(THandle(MarqueeEnabled)), LPARAM(MarqueeInterval));
end;

procedure InitVistaProgressBarMarqueeFix;
var
  PropInfo: PPropInfo;
begin
  if CheckWin32Version(6, 0) then
  begin
    ReplaceVmtField(TProgressBar, @TVistaProgressBarMarqueeFix.CreateParams, @TVistaProgressBarMarqueeFix.CreateParamsFix);
    ReplaceVmtField(TProgressBar, @TVistaProgressBarMarqueeFix.CreateWnd, @TVistaProgressBarMarqueeFix.CreateWndFix);
    PropInfo := GetPropInfo(TProgressBar, 'MarqueeInterval');
    if (PropInfo <> nil) and (PropInfo.SetProc <> nil) and
       not (Byte(DWORD_PTR(PropInfo.SetProc) shr 24) in [$FF, $FE]) then
    begin
      SetMarqueeInterval := PropInfo.SetProc;
      HookProc(SetMarqueeInterval, @TVistaProgressBarMarqueeFix.SetMarqueeInterval, SetMarqueeIntervalHook);
    end;
    DebugLog('VistaProgressBarMarqueeFix');
  end;
end;

procedure FiniVistaProgressBarMarqueeFix;
begin
  if CheckWin32Version(6, 0) then
  begin
    ReplaceVmtField(TProgressBar, @TVistaProgressBarMarqueeFix.CreateParamsFix, @TVistaProgressBarMarqueeFix.CreateParams);
    ReplaceVmtField(TProgressBar, @TVistaProgressBarMarqueeFix.CreateWndFix, @TVistaProgressBarMarqueeFix.CreateWnd);
    UnhookProc(SetMarqueeInterval, SetMarqueeIntervalHook);
  end;
end;
{$ENDIF VistaProgressBarMarqueeFix}
{ ---------------------------------------------------------------------------- }



{ ---------------------------------------------------------------------------- }
{ Workaround for Windows Vista CompareString bug }
{$IFDEF VistaCompareStringFix}

{**************************************************************************************************}
{                                                                                                  }
{ CompareString Fix                                                                                }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is CompareStringFix.pas.                                                       }
{                                                                                                  }
{ The Initial Developer of the Original Code is Andreas Hausladen.                                 }
{ Portions created by Andreas Hausladen are Copyright (C) 2008 Andreas Hausladen.                  }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{ Apr 18, 2008 A. Garrels - When parameter Locale or user default LCID matches (German-Germany)    }
{ and sort order equals SORT_GERMAN_PHONE_BOOK the patch must not be applied.                      }
{                                                                                                  }
{**************************************************************************************************}
{ This unit contains a workaround for a Windows Vista bug.                                         }
{ The Ü/ü ($DC/$FC) and the UE/ue are treated equal in all locales, but they aren't equal. There   }
{ was a bugfix intended for Vista SP1 but it was removed before SP1 was released.                  }
{ Windows 2008 Server still includes this bugfix but Vista will never get this bugfix.             }
{ Microsoft: new versions are for correctness; service packs are for consistency and compatibility }
{**************************************************************************************************}

function _CompareStringA(Locale: LCID; dwCmpFlags: DWORD; lpString1: PAnsiChar;
  cchCount1: Integer; lpString2: PAnsiChar; cchCount2: Integer): Integer; stdcall;
  external kernel32 name 'CompareStringA';
function _CompareStringW(Locale: LCID; dwCmpFlags: DWORD; lpString1: PWideChar;
  cchCount1: Integer; lpString2: PWideChar; cchCount2: Integer): Integer; stdcall;
  external kernel32 name 'CompareStringW';

var
  CompareStringAProc: function(Locale: LCID; dwCmpFlags: DWORD; lpString1: PAnsiChar;
    cchCount1: Integer; lpString2: PAnsiChar; cchCount2: Integer): Integer; stdcall;
  CompareStringWProc: function(Locale: LCID; dwCmpFlags: DWORD; lpString1: PWideChar;
    cchCount1: Integer; lpString2: PWideChar; cchCount2: Integer): Integer; stdcall;
  CompareStringFixRequired: Boolean;

const
  MaxCompareStringFixBuffer = 2047;
var
  CachedUserDefaultLCID: LCID = 0;
  CachedSystemDefaultLCID: LCID = 0;

function IsGermanPhonebookSortOrder(ALcid: LCID): Boolean; {$IF CompilerVersion >= 18.0} inline; {$IFEND}
begin
  if ALcid <> 0 then
  begin
    if ALcid = LOCALE_USER_DEFAULT then
    begin
      if CachedUserDefaultLCID = 0 then
        CachedUserDefaultLCID := GetUserDefaultLCID();
      ALcid := CachedUserDefaultLCID;
    end
    else if ALcid = LOCALE_SYSTEM_DEFAULT then
    begin
      if CachedSystemDefaultLCID = 0 then
        CachedSystemDefaultLCID := GetSystemDefaultLCID();
      ALcid := CachedSystemDefaultLCID;
    end;

    Result := (Word(ALcid) = $0407) and (ALcid shr 16 and $0F = SORT_GERMAN_PHONE_BOOK);
  end
  else
    Result := False;
end;

function GetUmlautFixedString(P: PWideChar; var Count: Integer; Buf: PWideChar): PWideChar;
const
  CombiningDiaresis = $308;
var
  ValidCount, EndIndex, EndCount, I, CollationCount, Size, Cnt: Integer;
  Ch: WideChar;
  Source, Dest: PWideChar;
begin
  ValidCount := -1;
  CollationCount := 0;
  EndIndex := -1;
  Source := P;

  { Test for the affected code points }
  if Count = -1 then
  begin
    Ch := Source^;
    while Ch <> #0 do
    begin
      while Ch <> #0 do
      begin
        case Ch of
          #$00DC, #$00FC:
            Break;
        end;
        Inc(Source);
        Ch := Source^;
      end;

      if Ch <> #0 then
      begin
        I := Source - P;
        if ValidCount = -1 then
          ValidCount := I;
        EndIndex := I + 1;
        Inc(CollationCount);
        Inc(Source);
        Ch := Source^;
      end;
    end;

    Count := Source - P;
  end
  else
  begin
    Cnt := Count;
    while Cnt > 0 do
    begin
      while Cnt <> 0 do
      begin
        case Source^ of
          #$00DC, #$00FC:
            Break;
        end;
        Inc(Source);
        Dec(Cnt);
      end;

      if Cnt <> 0 then
      begin
        I := Source - P;
        if ValidCount = -1 then
          ValidCount := I;
        EndIndex := I + 1;
        Inc(CollationCount);
        Inc(Source);
        Dec(Cnt);
      end;
    end;
  end;

  if CollationCount > 0 then
  begin
    { Re-encode the string with combining diaresis }

    EndCount := Count - EndIndex;
    if EndCount < 4 then
    begin
      { Move() isn't faster if there are too less code points to copy }
      EndIndex := Count;
      EndCount := 0;
    end;

    { Allocate enough memory or use the stack based buffer if the string fits
      into it. }
    Inc(Count, CollationCount);
    Size := (Count + 1) * SizeOf(WideChar);
    if Count < MaxCompareStringFixBuffer then
      Result := Buf
    else
      GetMem(Result, Size);

    { Copy untouched code points }
    if ValidCount >= 4 then
      Move(P^, Result^, ValidCount * SizeOf(WideChar))
    else
      ValidCount := 0;

    { Copy code points and replace "Ü" ($DC) and "ü" ($FC) with "U"/"u" + combining diaresis }
    Source := P + ValidCount;
    Dest := Result + ValidCount;
    for I := ValidCount to EndIndex - 1 do
    begin
      Ch := Source^;
      case Ch of
        #$00DC:
          begin
            PLongint(Dest)^ := (CombiningDiaresis shl 16) or $55;
            Inc(Dest);
          end;
        #$00FC:
          begin
            PLongint(Dest)^ := (CombiningDiaresis shl 16) or $75;
            Inc(Dest);
          end;
      else
        Dest^ := Ch;
      end;
      Inc(Dest);
      Inc(Source);
    end;

    { Copy remaining untouched code points }
    if EndCount > 0 then
    begin
      Move(Source^, Dest^, EndCount * SizeOf(WideChar));
      Inc(Dest, EndCount);
    end;
    Dest^ := #0;
  end
  else
    Result := P;
end;

function CompareStringW(Locale: LCID; dwCmpFlags: DWORD; lpString1: PWideChar;
  cchCount1: Integer; lpString2: PWideChar; cchCount2: Integer): Integer; stdcall;
var
  String1, String2: PWideChar;
  // Stack allocation is much faster than heap allocation
  Buf1, Buf2: array[0..MaxCompareStringFixBuffer] of WideChar;
begin
  if (lpString1 <> nil) and (lpString2 <> nil) and (cchCount1 <> 0) and (cchCount2 <> 0) and
     (lpString1 <> lpString2) and
     not IsGermanPhonebookSortOrder(Locale) then
  begin
    String1 := GetUmlautFixedString(lpString1, cchCount1, Buf1);
    String2 := GetUmlautFixedString(lpString2, cchCount2, Buf2);

    Result := CompareStringWProc(Locale, dwCmpFlags, String1, cchCount1, String2, cchCount2);

    if (String1 <> lpString1) and (String1 <> @Buf1[0]) then
      FreeMem(String1);
    if (String2 <> lpString2) and (String2 <> @Buf2[0]) then
      FreeMem(String2);
  end
  else
    Result := CompareStringWProc(Locale, dwCmpFlags, lpString1, cchCount1, lpString2, cchCount2);
end;

function CompareStringA(Locale: LCID; dwCmpFlags: DWORD; lpString1: PAnsiChar;
  cchCount1: Integer; lpString2: PAnsiChar; cchCount2: Integer): Integer; stdcall;

  function ContainsProblematicUmlaut(P: PAnsiChar; Count: Integer): Boolean;
  begin
    Result := True;
    while Count > 0 do
    begin
      if P^ in [#$DC, #$FC] then
        Exit;
      Inc(P);
      Dec(Count);
    end;
    Result := False;
  end;

var
  String1, String2: WideString;
begin
  if (lpString1 <> nil) and (lpString2 <> nil) and (cchCount1 <> 0) and (cchCount2 <> 0) and
     (lpString1 <> lpString2) and
     not IsGermanPhonebookSortOrder(Locale) then
  begin
    case GetACP of
      {1250,} 1252{, 1254, 1257, 1258}:
        begin
          if cchCount1 = -1 then
            cchCount1 := StrLen(lpString1);
          if cchCount2 = -1 then
            cchCount2 := StrLen(lpString2);

          if ContainsProblematicUmlaut(lpString1, cchCount1) or
             ContainsProblematicUmlaut(lpString2, cchCount2) then
          begin
            SetString(String1, lpString1, cchCount1);
            SetString(String2, lpString2, cchCount2);
            Result := CompareStringW(Locale, dwCmpFlags, PWideChar(String1), Length(String1),
                                     PWideChar(String2), Length(String2));
            Exit;
          end;
        end;
    end;
  end;
  Result := CompareStringAProc(Locale, dwCmpFlags, lpString1, cchCount1, lpString2, cchCount2);
end;

type
  TJumpItem = packed record
    Code: TXRedirCode;
    Jump: Byte;
    Offset: Integer;
  end;
  PJumpTable = ^TJumpTable;
  TJumpTable = array[0..1] of TJumpItem;

var
  JumpTable: PJumpTable;

procedure PatchWinAPI(Proc, Dest: Pointer; var JumpItem: TJumpItem);
var
  n: DWORD;
  Code: TXRedirCode;
begin
  Proc := GetActualAddr(Proc);
  Assert(Proc <> nil);

  if ReadProcessMemory(GetCurrentProcess, Proc, @JumpItem.Code, SizeOf(JumpItem.Code), n) then
  begin
    JumpItem.Jump := $E9;
    JumpItem.Offset := Integer(Proc) - Integer(@JumpItem) - SizeOf(JumpItem.Code);

    Code.Jump := $E9;
    Code.Offset := Integer(Dest) - Integer(Proc) - SizeOf(Code);

    WriteProcessMemory(GetCurrentProcess, Proc, @Code, SizeOf(Code), n);
  end;
end;

procedure UnpatchWinAPI(Proc: Pointer; const JumpItem: TJumpItem);
var
  n: Cardinal;
begin
  if JumpItem.Code.Jump <> 0 then
  begin
    Proc := GetActualAddr(Proc);
    Assert(Proc <> nil);

    WriteProcessMemory(GetCurrentProcess, Proc, @JumpItem.Code, SizeOf(JumpItem.Code), n);
  end;
end;

procedure InitCompareStringFix;
const
  CSTR_EQUAL = 2;
begin
  { Only Vista is affected, Windows 2008 Server is not affected }
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and
     (Win32MajorVersion = 6) and (Win32MinorVersion = 0) then
  begin
    CompareStringFixRequired := _CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE, #$FC, 1, 'ue', 2) = CSTR_EQUAL;
    if CompareStringFixRequired then
    begin
      OutputDebugStringW('Installing CompareString workaround for Windows Vista');
      JumpTable := VirtualAlloc(nil, SizeOf(TJumpTable), MEM_COMMIT, PAGE_EXECUTE_READWRITE);

      PatchWinAPI(@_CompareStringA, @CompareStringA, JumpTable[0]);
      CompareStringAProc := @JumpTable[0];
      PatchWinAPI(@_CompareStringW, @CompareStringW, JumpTable[1]);
      CompareStringWProc := @JumpTable[1];
    end;
  end;
end;

procedure FiniCompareStringFix;
begin
  if CompareStringFixRequired then
  begin
    UnpatchWinAPI(@_CompareStringA, JumpTable[0]);
    UnpatchWinAPI(@_CompareStringW, JumpTable[1]);

    VirtualFree(JumpTable, 0, MEM_RELEASE);
    JumpTable := nil;
  end;
end;
{$ENDIF VistaCompareStringFix}
{ ---------------------------------------------------------------------------- }

initialization
  {$IFDEF GetNonToolWindowPopupParentFix}
  FixGetNonToolWindowPopupParent;
  {$ENDIF GetNonToolWindowPopupParentFix}

  {$IFDEF TaskModalDialogFix}
  InitTaskModalDialogFix;
  {$ENDIF TaskModalDialogFix}

  {$IFDEF AppDeActivateZOrderFix}
  DebugLog('AppDeActivateZOrderFix');
  Application.HookMainWindow(TAppDeActivateZOrderFix.AppWndProc);
  {$ENDIF AppDeActivateZOrderFix}

  {$IFDEF HideStackTrashingFix}
  FindTopMostWindowProc := GetAddrOfFindTopMostWindow;
  if FindTopMostWindowProc <> nil then
  begin
    DebugLog('HideStackTrashingFix');
    HookProc(FindTopMostWindowProc, @FindTopMostWindow, FindTopMostWindowHook);
  end;
  {$ENDIF HideStackTrashingFix}

  {$IFDEF ControlResizeFix}
  InitControlResizeFix;
  {$ENDIF ControlResizeFix}

  {$IFDEF ActionListAVFix}
  InitActionListAVFix;
  {$ENDIF ActionListAVFix}

  {$IFDEF ContextMenuFix}
  InitContextMenuFix;
  {$ENDIF ContextMenuFix}

  {$IFDEF ObjAutoDEPFix}
  InitObjAutoDEPFix;
  {$ENDIF ObjAutoDEPFix}

  {$IFDEF AppMinimizeFix}
  InitAppMinimizeFix;
  {$ENDIF AppMinimizeFix}

  {$IFDEF SysUtilsAbortFix}
  InitSysUtilsAbortFix;
  {$ENDIF SysUtilsAbortFix}

  {$IFDEF CmdShowMinimizeFix}
  InitCmdShowMinimizeFix;
  {$ENDIF CmdShowMinimizeFix}

  {$IFDEF MDIChildFocusFix}
  InitMDIChildFocusFix;
  {$ENDIF MDIChildFocusFix}

  {$IFDEF PageControlPaintingFix}
  InitPageControlPaintingFix;
  {$ENDIF PageControlPaintingFix}

  {$IFDEF GridFlickerFix}
  InitGridFlickerFix;
  {$ENDIF GridFlickerFix}

  {$IFDEF SpeedButtonGlassFix}
  InitSpeedButtonGlassFix;
  {$ENDIF SpeedButtonGlassFix}

  {$IFDEF VistaProgressBarMarqueeFix}
  InitVistaProgressBarMarqueeFix;
  {$ENDIF VistaProgressBarMarqueeFix}

  {$IFDEF VistaCompareStringFix}
  InitCompareStringFix;
  {$ENDIF VistaCompareStringFix}

finalization
  // In revers order

  {$IFDEF VistaCompareStringFix}
  FiniCompareStringFix;
  {$ENDIF VistaCompareStringFix}

  {$IFDEF VistaProgressBarMarqueeFix}
  FiniVistaProgressBarMarqueeFix;
  {$ENDIF VistaProgressBarMarqueeFix}

  {$IFDEF SpeedButtonGlassFix}
  FiniSpeedButtonGlassFix;
  {$ENDIF SpeedButtonGlassFix}

  {$IFDEF GridFlickerFix}
  FiniGridFlickerFix;
  {$ENDIF GridFlickerFix}

  {$IFDEF PageControlPaintingFix}
  FiniPageControlPaintingFix;
  {$ENDIF PageControlPaintingFix}

  {$IFDEF MDIChildFocusFix}
  FiniMDIChildFocusFix;
  {$ENDIF MDIChildFocusFix}

  {$IFDEF CmdShowMinimizeFix}
  FiniCmdShowMinimizeFix;
  {$ENDIF CmdShowMinimizeFix}

  {$IFDEF SysUtilsAbortFix}
  FiniSysUtilsAbortFix;
  {$ENDIF SysUtilsAbortFix}

  {$IFDEF AppMinimizeFix}
  FiniAppMinimizeFix;
  {$ENDIF AppMinimizeFix}

  {$IFDEF ObjAutoDEPFix}
  FiniObjAutoDEPFix;
  {$ENDIF ObjAutoDEPFix}

  {$IFDEF ContextMenuFix}
  FiniContextMenuFix;
  {$ENDIF ContextMenuFix}

  {$IFDEF ActionListAVFix}
  FiniActionListAVFix;
  {$ENDIF ActionListAVFix}

  {$IFDEF ControlResizeFix}
  FiniControlResizeFix;
  {$ENDIF ControlResizeFix}

  {$IFDEF HideStackTrashingFix}
  if FindTopMostWindowProc <> nil then
    UnhookProc(FindTopMostWindowProc, FindTopMostWindowHook);
  {$ENDIF HideStackTrashingFix}

  {$IFDEF TaskModalDialogFix}
  FiniTaskModalDialogFix;
  {$ENDIF TaskModalDialogFix}

  {$IFDEF AppDeActivateZOrderFix}
  if Application <> nil then
    Application.UnhookMainWindow(TAppDeActivateZOrderFix.AppWndProc);
  {$ENDIF AppDeActivateZOrderFix}

  {$IFDEF MkObjInstLeakFix}
  MkObjInstLeakHooked := False;
  ReleaseObjectInstanceBlocks;
  {$ENDIF MkObjInstLeakFix}

end.
