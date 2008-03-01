{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: RtlVclOptimize.pas, released on 2007-05-08

The Initial Developer of the Original Code is Andreas Hausladen
Portions created by Andreas Hausladen are Copyright (C) 2006, 2007 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

Version 2.7 (2007-10-01)

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D-,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W+,X+,Y+,Z1}

unit RtlVclOptimize;

{.$DEFINE WMPAINT_HOOK}
  { If WMPAINT_HOOK is defined the TWinControl.WMPaint message handler for
    Delphi 6, 7, 2005 and 2006 is overwritten by a version that takes less
    time and which is also used in the Delphi 2007 VCL by default. }

{.$DEFINE STREQUAL_INJECTION}
  { If STREQUAL_INJECTION is defined the _LStrCmp/_WStrCmp call is replaced
    by a call to _LStrEqual/_WStrEqual when the next opcode is a
    JZ/JNZ/SETZ/SETNZ. So every "if Str1 = Str2 then" and "if Str <> Str2 then"
    will call _LStrEqual/_WStrEqual directly on the next iteration. }

{.$DEFINE NOLEADBYTES_HOOK}
  { If NOLEADBYTES_HOOK is defined and SysLocal.LeadBytes is [], some ANSI
    functions are replaced by faster functions that ignore the LeadBytes. }

{.$DEFINE RTLDEBUG}
  { If RTLDEBUG is defined the unit is compiled with debug information. }

interface

{$IFDEF CONDITIONALEXPRESSIONS}
 {$DEFINE COMPILER6_UP}
 {$IF RtlVersion >= 15.0}
  {$DEFINE COMPILER7_UP}
 {$IFEND}
 {$IF RtlVersion >= 17.0}
  {$DEFINE COMPILER9_UP}
  {$IF RtlVersion = 17.0}
   {$DEFINE COMPILER9}
  {$IFEND}
 {$IFEND}
 {$IF RtlVersion >= 18.0}
  {$DEFINE COMPILER10_UP}
 {$IFEND}
 {$IF CompilerVersion >= 18.5}
  {$DEFINE DELPHI2007_UP}
 {$IFEND}
{$ELSE} // Delphi 5 or below
 {$IFDEF VER130}
  {$DEFINE COMPILER5}
  {$DEFINE COMPILER5_UP}
 {$ELSE}
  This_compiler_version_is_not_supported
 {$ENDIF}
{$ENDIF}

{$IFDEF COMPILER7_UP}
 {$WARN UNSAFE_TYPE OFF}
 {$WARN UNSAFE_CODE OFF}
 {$WARN UNSAFE_CAST OFF}
{$ENDIF COMPILER7_UP}

{$IFDEF RTLDEBUG}
 {$D+}
{$ENDIF RTLDEBUG}

implementation

uses
  Windows, Messages, SysUtils, Classes, Contnrs, TypInfo,
  {$IFDEF COMPILER6_UP} RtlConsts {$ELSE} Consts {$ENDIF},
  Controls;

{$IFDEF NOLEADBYTES_HOOK}
var
  NoLeadBytes: Boolean;
{$ENDIF NOLEADBYTES_HOOK}

const
  CurProcess = Cardinal(-1); // GetCurrentProcess returns EAX = DWORD(-1)

{------------------------------------------------------------------------------}
{ Memory manipulation functions                                                }
{------------------------------------------------------------------------------}
type
  TInjectRec = packed record
    Jump: Byte;
    Offset: Integer;
  end;

  PAbsoluteIndirectJmp = ^TAbsoluteIndirectJmp;
  TAbsoluteIndirectJmp = packed record
    OpCode: Word;   //$FF25
    Addr: ^Pointer;
  end;

  PWin9xDebugThunk = ^TWin9xDebugThunk;
  TWin9xDebugThunk = packed record
    PUSH: Byte;    // $68
    Addr: Pointer;
    JMP: Byte;     // $E9
    Offset: Integer;
  end;

  TLongCall = packed record
    Call: Byte;
    Offset: Integer;
    NextCmd: record end;
  end;

  PPointer = ^Pointer;

  PImportEntry = ^TImportEntry;
  TImportEntry = packed record
    Jmp: Word;
    Destination: PPointer;
    Magic: Word;
  end;

function IsWin9xDebugThunk(Addr: Pointer): Boolean;
begin
  Result := (Addr <> nil) and (PWin9xDebugThunk(Addr).PUSH = $68) and
                              (PWin9xDebugThunk(Addr).JMP = $E9);
end;

function GetActualAddr(Proc: Pointer): Pointer;
begin
  if Proc <> nil then
  begin
    if (SysUtils.Win32Platform <> VER_PLATFORM_WIN32_NT) and IsWin9xDebugThunk(Proc) then
      Proc := PWin9xDebugThunk(Proc).Addr;
    if (PAbsoluteIndirectJmp(Proc).OpCode = $25FF) then
      Result := PAbsoluteIndirectJmp(Proc).Addr^
    else
      Result := Proc;
  end
  else
    Result := nil;
end;

procedure CodeRedirect(Proc: Pointer; NewProc: Pointer);
var
  OldProtect: Cardinal;
begin
  if Proc = nil then
    Exit;
  Proc := GetActualAddr(Proc);
  if VirtualProtectEx(GetCurrentProcess, Proc, SizeOf(TInjectRec), PAGE_EXECUTE_READWRITE, OldProtect) then
  begin
    TInjectRec(Proc^).Jump := $E9;
    TInjectRec(Proc^).Offset := Integer(NewProc) - (Integer(Proc) + SizeOf(TInjectRec));
    VirtualProtectEx(GetCurrentProcess, Proc, SizeOf(TInjectRec), OldProtect, @OldProtect);
  end;
end;

function InjectCode(DestProc, SourceProc: Pointer; Size: Cardinal): Boolean;
var
  n: Cardinal;
begin
  DestProc := GetActualAddr(DestProc);
  Result := (DestProc <> nil) and (SourceProc <> nil) and
            WriteProcessMemory(GetCurrentProcess, DestProc, SourceProc, Size, n) and (n = Size);
end;

function GetDynamicMethod(AClass: TClass; Index: Integer): Pointer;
asm
  CALL System.@FindDynaClass
end;
{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
{ Long/Wide-String optimization                                                }
{------------------------------------------------------------------------------}
var
  OrgLStrCmp: Pointer;
  OrgWStrCmp: Pointer;

procedure _LStrCmp_LStrEqual{left: AnsiString; right: AnsiString}; forward;
procedure _WStrCmp_WStrEqual{left: WideString; right: WideString}; forward;

{------------------------------------------------------------------------------}
 (* ***** BEGIN LICENSE BLOCK *****
* Version: MPL 1.1
*
* The assembly implementation of function _LStrEqual is subject to the
* Mozilla Public License Version 1.1 (the "License"); you may
* not use this file except in compliance with the License.
* You may obtain a copy of the License at http://www.mozilla.org/MPL/
*
* Software distributed under the License is distributed on an "AS IS" basis,
* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
* for the specific language governing rights and limitations under the
* License.
*
* The Initial Developer of the Original Code is Pierre le Riche
*
* Portions created by the Initial Developer are Copyright (C) 2002-2007
* the Initial Developer. All Rights Reserved.
*
* Contributor(s): Pierre le Riche
*
* ***** END LICENSE BLOCK ***** *)
procedure _LStrEqual;
asm
  {On entry:
     eax = @S1[1]
     edx = @S2[1]
   On exit:
     Result in flags:
       ZF = 1 if S1 = S2, ZF = 0 otherwise
   Destroys:
     eax, edx, ecx
   Code size:
     68 bytes}
  {Do S1 and S2 point to the same string data?}
  cmp eax, edx
  je @CompareDoneNoPop
  {Is one of the two string pointers perhaps nil?}
  test eax, edx
  jz @PossibleNilString
@BothStringsNonNil:
  {Compare lengths}
  mov ecx, [eax - 4]
  cmp ecx, [edx - 4]
  jne @CompareDoneNoPop
  {Save ebx}
  push ebx
  {Get pointers to the 4th last bytes in the strings}
  lea edx, [edx + ecx - 4]
  lea ebx, [eax + ecx - 4]
  {Negate the loop counter}
  neg ecx
  {Compare the last four bytes. If the string length is less than four bytes
   then part of the length field is compared again - no harm done.}
  mov eax, [ebx]
  cmp eax, [edx]
  jne @CompareDonePop
@CompareLoop:
  {Next four bytes}
  add ecx, 4
  jns @Match
  {Compare four bytes per iteration}
  mov eax, [ebx + ecx]
  cmp eax, [edx + ecx]
  je @CompareLoop
@CompareDonePop:
  pop ebx
@CompareDoneNoPop:
  ret
@Match:
  {Strings match - set the zero flag}
  xor eax, eax
  pop ebx
  ret
@PossibleNilString:
  {There is a good probability that one of the strings are nil (but not both)}
  test eax, eax
  jz @FirstStringNil
  test edx, edx
  jnz @BothStringsNonNil
  {S2 is nil - compare lengths of the strings}
  cmp [eax - 4], edx
  ret
@FirstStringNil:
  {S1 is nil - compare lengths of the strings}
  cmp eax, [edx - 4]
end;

{------------------------------------------------------------------------------}

procedure ReplaceLStrCmpByLStrEqual(var ProcAddr: TLongCall);
var
  OldProtect: Cardinal;
  ImportEntry: PImportEntry;
begin
  ImportEntry := Pointer(Integer(@ProcAddr.NextCmd) + ProcAddr.Offset);
  if (ImportEntry = @_LStrCmp_LStrEqual) or (ImportEntry = OrgLStrCmp) then
  begin
    if VirtualProtectEx(CurProcess, @ProcAddr.Offset, SizeOf(Integer), PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      ProcAddr.Offset := Integer(@_LStrEqual) - (Integer(@ProcAddr.NextCmd));
      VirtualProtectEx(CurProcess, @ProcAddr.Offset, SizeOf(Integer), OldProtect, @OldProtect);
    end;
  end
  else
  begin
    try
      { protect against non-Delphi conform import tables }
      if //not IsBadReadPtr(ImportEntry, SizeOf(TImportEntry)) and
         (ImportEntry.Jmp = $25FF) and ((ImportEntry.Magic = $C08B) or (ImportEntry.Magic = $25FF)) then
      begin
        if VirtualProtectEx(CurProcess, @ProcAddr.Offset, SizeOf(Integer), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          ProcAddr.Offset := Integer(@_LStrEqual) - (Integer(@ProcAddr.NextCmd));
          VirtualProtectEx(CurProcess, @ProcAddr.Offset, SizeOf(Integer), OldProtect, @OldProtect);
        end;
      end;
    except
    end;
  end;
end;

{------------------------------------------------------------------------------}
(* ***** BEGIN LICENSE BLOCK *****
* Version: MPL 1.1
*
* The assembly implementation of function _LStrCmp is subject to the
* Mozilla Public License Version 1.1 (the "License"); you may
* not use this file except in compliance with the License.
* You may obtain a copy of the License at http://www.mozilla.org/MPL/
*
* Software distributed under the License is distributed on an "AS IS" basis,
* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
* for the specific language governing rights and limitations under the
* License.
*
* The Initial Developer of the Original Code is Pierre le Riche
*
* Portions created by the Initial Developer are Copyright (C) 2002-2007
* the Initial Developer. All Rights Reserved.
*
* Contributor(s): Pierre le Riche,
*                 Andreas Hausladen (inline function replacing)
*
* ***** END LICENSE BLOCK ***** *)
procedure _LStrCmp_LStrEqual{left: AnsiString; right: AnsiString};
asm
  {On entry:
     eax = @S1[1]
     edx = @S2[1]
   On exit:
     Result in flags:
       CF = 1 if S1 < S2, CF = 0 otherwise
       ZF = 1 if S1 = S2, ZF = 0 otherwise
   Destroys:
     eax, edx, ecx
   Code size:
     88+41 bytes}
  {Do S1 and S2 point to the same string data?}
  cmp eax, edx
  je @DoneNoPop
  {Is one of the two string pointers perhaps nil?}
  test eax, edx
  jz @PossibleNilString

@BothStringsNonNil:
  // can _LStrEqual be used
  mov ecx, [esp] // read return address
  mov cx, WORD PTR [ecx] // read first byte at the return address

{  cmp cl, $75 // JNZ near
  je @LStrEqual
  jg @StartCompare
  cmp cl, $74 // JZ near
  je @LStrEqual}
  add cl, $8c
  sub cl, $02
  jb @LStrEqual

  // prefix opcodes
{  cmp cl, $0f
  jne @StartCompare}
  add cl, $67 //=$02-$8c-$0f
  jnz @StartCompare

{  cmp ch, $95 // SETNZ al
  je @LStrEqual
  jg @StartCompare
  cmp ch, $84 // JZ far
  je @LStrEqual
  jl @StartCompare
  cmp ch, $94 // SETZ al
  je @LStrEqual
  cmp ch, $85 // JNZ far
  je @LStrEqual}
  add ch, $7c
  sub ch, $02
  jb @LStrEqual
  add ch, $f2
  sub ch, $02
  jb @LStrEqual

@StartCompare:
  {Compare the first character. (There has to be a trailing #0, so this
   comparison is safe). In "random" string compares this can save significant
   CPU time.}
  movzx ecx, byte ptr [eax]
  sub cl, [edx]
  jne @DoneNoPop
  {Save ebx}
  push ebx
  {Set ebx = length(S1)}
  mov ebx, [eax - 4]
  {Set ebx = length(S1) - length(S2)}
  sub ebx, [edx - 4]
  {Save the length difference on the stack}
  push ebx
  {Set ecx = 0 if length(S1) <= length(S2), $ffffffff otherwise}
  adc ecx, -1
  {Set ecx = - min(length(S1), length(S2))}
  and ecx, ebx
  sub ecx, [eax - 4]
  {Adjust the pointers to be negative offset based}
  sub eax, ecx
  sub edx, ecx
@CompareLoop:
  {Compare four bytes per cycle. (The start of string data is at least DWord
   aligned, so this is safe.)}
  mov ebx, [eax + ecx]
  xor ebx, [edx + ecx]
  jnz @Mismatch
  {Next four bytes}
  add ecx, 4
  js @CompareLoop
  {All characters match up to the compare length}
@MatchUpToLength:
  {Restore the string length difference to eax}
  pop eax
  {Set the flags according to the length difference}
  add eax, eax
  {Restore ebx and return}
  pop ebx
@DoneNoPop:
  ret
@Mismatch:
  {Find the byte index that mismatched}
  bsf ebx, ebx
  shr ebx, 3
  {Is the mismatch beyond the compare length?}
  add ecx, ebx
  jns @MatchUpToLength
  {Compare the mismatched byte, setting the flags}
  mov al, [eax + ecx]
  cmp al, [edx + ecx]
  {Pop the length difference, restore ebx and return}
  pop ebx
  pop ebx
  ret
@PossibleNilString:
  {There is a good probability that one of the strings are nil (but not both)}
  test eax, eax
  jz @FirstStringNil
  test edx, edx
  jnz @BothStringsNonNil
  {S2 is nil - compare lengths of the strings}
  cmp [eax - 4], edx
  ret
{ --- <LStrEqual> --- }
@LStrEqual:
{$IFDEF STREQUAL_INJECTION}
  push eax
  push edx
  mov eax, [esp+8]    // get caller...
  sub eax, 5          // ... and the calling opcode
  cmp BYTE PTR [eax], $E8
  jne @@CallFunction
  call ReplaceLStrCmpByLStrEqual
@@CallFunction:
  pop edx
  pop eax
{$ENDIF STREQUAL_INJECTION}
  {On entry:
     eax = @S1[1]
     edx = @S2[1]
   On exit:
     Result in flags:
       ZF = 1 if S1 = S2, ZF = 0 otherwise
   Destroys:
     eax, edx, ecx
   Code size:
     xx bytes}

  {Compare lengths}
  mov ecx, [eax - 4]
  cmp ecx, [edx - 4]
  jne @LStrEqual_CompareDoneNoPop
  {Save ebx}
  push ebx
  {Get pointers to the 4th last bytes in the strings}
  lea edx, [edx + ecx - 4]
  lea ebx, [eax + ecx - 4]
  {Negate the loop counter}
  neg ecx
  {Compare the last four bytes. If the string length is less than four bytes
   then part of the length field is compared again - no harm done.}
  mov eax, [ebx]
  cmp eax, [edx]
  jne @LStrEqual_CompareDonePop
@LStrEqual_CompareLoop:
  {Next four bytes}
  add ecx, 4
  jns @LStrEqual_Match
  {Compare four bytes per iteration}
  mov eax, [ebx + ecx]
  cmp eax, [edx + ecx]
  je @LStrEqual_CompareLoop
@LStrEqual_CompareDonePop:
  pop ebx
@LStrEqual_CompareDoneNoPop:
  ret
@LStrEqual_Match:
  {Strings match - set the zero flag}
  xor eax, eax
  pop ebx
  ret
{ --- </LStrEqual> --- }
@FirstStringNil:
  {S1 is nil - compare lengths of the strings}
  cmp eax, [edx - 4]
end;

{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
 (* ***** BEGIN LICENSE BLOCK *****
* Version: MPL 1.1
*
* The assembly implementation of function _LStrEqual is subject to the
* Mozilla Public License Version 1.1 (the "License"); you may
* not use this file except in compliance with the License.
* You may obtain a copy of the License at http://www.mozilla.org/MPL/
*
* Software distributed under the License is distributed on an "AS IS" basis,
* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
* for the specific language governing rights and limitations under the
* License.
*
* The Initial Developer of the Original Code is Pierre le Riche
*
* Portions created by the Initial Developer are Copyright (C) 2002-2007
* the Initial Developer. All Rights Reserved.
*
* Contributor(s): Pierre le Riche
*
* ***** END LICENSE BLOCK ***** *)
procedure _WStrEqual;
asm
  {On entry:
     eax = @S1[1]
     edx = @S2[1]
   On exit:
     Result in flags:
       ZF = 1 if S1 = S2, ZF = 0 otherwise
   Destroys:
     eax, edx, ecx
   Code size:
     68 bytes}
  {Do S1 and S2 point to the same string data?}
  cmp eax, edx
  je @CompareDoneNoPop
  {Is one of the two string pointers perhaps nil?}
  test eax, edx
  jz @PossibleNilString
@BothStringsNonNil:
  {Compare lengths}
  mov ecx, [eax - 4]
  cmp ecx, [edx - 4]
  jne @CompareDoneNoPop
  {Save ebx}
  push ebx
  {Get pointers to the 4th last bytes in the strings}
  lea edx, [edx + ecx - 4]
  lea ebx, [eax + ecx - 4]
  {Negate the loop counter}
  neg ecx
  {Compare the last four bytes. If the string length is less than four bytes
   then part of the length field is compared again - no harm done.}
  mov eax, [ebx]
  cmp eax, [edx]
  jne @CompareDonePop
@CompareLoop:
  {Next four bytes}
  add ecx, 4
  jns @Match
  {Compare four bytes per iteration}
  mov eax, [ebx + ecx]
  cmp eax, [edx + ecx]
  je @CompareLoop
@CompareDonePop:
  pop ebx
@CompareDoneNoPop:
  ret
@Match:
  {Strings match - set the zero flag}
  xor eax, eax
  pop ebx
  ret
@PossibleNilString:
  {There is a good probability that one of the strings are nil (but not both)}
  test eax, eax
  jz @FirstStringNil
  test edx, edx
  jnz @BothStringsNonNil
  {S2 is nil - compare lengths of the strings}
  cmp [eax - 4], edx
  ret
@FirstStringNil:
  {S1 is nil - compare lengths of the strings}
  cmp eax, [edx - 4]
end;

procedure ReplaceWStrCmpByWStrEqual(var ProcAddr: TLongCall);
type
  PInlineNilCmp = ^TInlineNilCmp;
  TInlineNilCmp = packed record
    TestEaxEax: Word;
    Nops: Word;
    Nop: Byte;
  end;

var
  OldProtect: Cardinal;
  ImportEntry: PImportEntry;
begin
  ImportEntry := Pointer(Integer(@ProcAddr.NextCmd) + ProcAddr.Offset);
  if (ImportEntry = @_WStrCmp_WStrEqual) or (ImportEntry = OrgWStrCmp)  then
  begin
    { Disabled because if there is .NET code in an IDE expert that calls an
      Win32 IDE function with a non-NULL zero length WideString will cause an
      access violation in the code that meight follow the inlined "if WS = '' then" }

    {if PWord(Cardinal(@ProcAddr) - 2)^ = $D233 then // xor edx,edx
    begin // => if WS = '' then ...
      if VirtualProtectEx(CurProcess, @ProcAddr, SizeOf(ProcAddr), PAGE_EXECUTE_READWRITE, OldProtect) then
      begin
        PInlineNilCmp(@ProcAddr).TestEaxEax := $C085; // test eax,eax
        PInlineNilCmp(@ProcAddr).Nops := $9090;
        PInlineNilCmp(@ProcAddr).Nop := $90;
        VirtualProtectEx(CurProcess, @ProcAddr, SizeOf(ProcAddr), OldProtect, @OldProtect);
      end;
    end
    else}
    if VirtualProtectEx(CurProcess, @ProcAddr.Offset, SizeOf(Integer), PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      ProcAddr.Offset := Integer(@_WStrEqual) - (Integer(@ProcAddr.NextCmd));
      VirtualProtectEx(CurProcess, @ProcAddr.Offset, SizeOf(Integer), OldProtect, @OldProtect);
    end;
  end
  else
  begin
    try
      { protect against non-Delphi conform import tables }
      if //not IsBadReadPtr(ImportEntry, SizeOf(TImportEntry)) and
         (ImportEntry.Jmp = $25FF) and ((ImportEntry.Magic = $C08B) or (ImportEntry.Magic = $25FF)) then
      begin
        { Disabled because if there is .NET code in an IDE expert that calls an
          Win32 IDE function with a non-NULL zero length WideString will cause an
          access violation in the code that meight follow the inlined "if WS = '' then" }

        {if PWord(Cardinal(@ProcAddr) - 2)^ = $D233 then // xor edx,edx
        begin // => if WS = '' then ...
          if VirtualProtectEx(CurProcess, @ProcAddr, SizeOf(ProcAddr), PAGE_EXECUTE_READWRITE, OldProtect) then
          begin
            PInlineNilCmp(@ProcAddr).TestEaxEax := $C085; // test eax,eax
            PInlineNilCmp(@ProcAddr).Nops := $9090;
            PInlineNilCmp(@ProcAddr).Nop := $90;
            VirtualProtectEx(CurProcess, @ProcAddr, SizeOf(ProcAddr), OldProtect, @OldProtect);
          end;
        end
        else}
        if VirtualProtectEx(CurProcess, @ProcAddr.Offset, SizeOf(Integer), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          ProcAddr.Offset := Integer(@_WStrEqual) - (Integer(@ProcAddr.NextCmd));
          VirtualProtectEx(CurProcess, @ProcAddr.Offset, SizeOf(Integer), OldProtect, @OldProtect);
        end;
      end;
    except
    end;
  end;
end;

{------------------------------------------------------------------------------}
(* ***** BEGIN LICENSE BLOCK *****
* Version: MPL 1.1
*
* The assembly implementation of function _LStrCmp is subject to the
* Mozilla Public License Version 1.1 (the "License"); you may
* not use this file except in compliance with the License.
* You may obtain a copy of the License at http://www.mozilla.org/MPL/
*
* Software distributed under the License is distributed on an "AS IS" basis,
* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
* for the specific language governing rights and limitations under the
* License.
*
* The Initial Developer of the Original Code is Pierre le Riche
*
* Portions created by the Initial Developer are Copyright (C) 2002-2007
* the Initial Developer. All Rights Reserved.
*
* Contributor(s): Pierre le Riche,
*                 Andreas Hausladen (inline function replacing)
*
* ***** END LICENSE BLOCK ***** *)
procedure _WStrCmp_WStrEqual{left: WideString; right: WideString};
asm
  // Check if this call is a candidate for _WStrEqual

  mov ecx, [esp]         { read return address }
  mov cx, WORD PTR [ecx] { read opcode at the return address }

  {cmp cl, $75 // JNZ near
  je @LStrEqual
  jg @StartCompare
  cmp cl, $74 // JZ near
  je @LStrEqual}
  add cl, $8c
  sub cl, $02
  jb @WStrEqual

  { prefixed opcodes }
  {cmp cl, $0f
  jne @StartCompare}
  add cl, $67 //=$02-$8c-$0f
  jnz @StartCompare

  {cmp ch, $95 // SETNZ al
  je @LStrEqual
  jg @StartCompare
  cmp ch, $84 // JZ far
  je @LStrEqual
  jl @StartCompare
  cmp ch, $94 // SETZ al
  je @LStrEqual
  cmp ch, $85 // JNZ far
  je @LStrEqual}
  add ch, $7c
  sub ch, $02
  jb @WStrEqual
  add ch, $f2
  sub ch, $02
  jb @WStrEqual

@StartCompare:
  {On entry:
     eax = @S1[1]
     edx = @S2[1]
   On exit:
     Result in flags:
       CF = 1 if S1 < S2, CF = 0 otherwise
       ZF = 1 if S1 = S2, ZF = 0 otherwise
   Destroys:
     eax, edx, ecx
   Code size:
     88+41 bytes}
  {Do S1 and S2 point to the same string data?}
  cmp eax, edx
  je @DoneNoPop
  {Is one of the two string pointers perhaps nil?}
  test eax, edx
  jz @PossibleNilString

@BothStringsNonNil:
  {Compare the first character. (There has to be a trailing #0, so this
   comparison is safe). In "random" string compares this can save significant
   CPU time.}
  movzx ecx, word ptr [eax]
  sub cx, [edx]
  jne @DoneNoPop
  {Save ebx}
  push ebx
  {Set ebx = length(S1)}
  mov ebx, [eax - 4]
  {Set ebx = length(S1) - length(S2)}
  sub ebx, [edx - 4]
  {Save the length difference on the stack}
  push ebx
  {Set ecx = 0 if length(S1) <= length(S2), $ffffffff otherwise}
  adc ecx, -1
  {Set ecx = - min(length(S1), length(S2))}
  and ecx, ebx
  sub ecx, [eax - 4]
  {Adjust the pointers to be negative offset based}
  sub eax, ecx
  sub edx, ecx
@CompareLoop:
  {Compare four bytes per cycle. (The start of string data is at least DWord
   aligned, so this is safe.)}
  mov ebx, [eax + ecx]
  xor ebx, [edx + ecx]
  jnz @Mismatch
  {Next four bytes}
  add ecx, 4
  js @CompareLoop
  {All characters match up to the compare length}
@MatchUpToLength:
  {Restore the string length difference to eax}
  pop eax
  {Set the flags according to the length difference}
  add eax, eax
  {Restore ebx and return}
  pop ebx
@DoneNoPop:
  ret
@Mismatch:
  {Find the byte index that mismatched}
  bsf ebx, ebx
  shr ebx, 3
  {Is the mismatch beyond the compare length?}
  add ecx, ebx
  jns @MatchUpToLength
  {Compare the mismatched byte, setting the flags}
  mov ax, [eax + ecx]
  cmp ax, [edx + ecx]
  {Pop the length difference, restore ebx and return}
  pop ebx
  pop ebx
  ret
@PossibleNilString:
  {There is a good probability that one of the strings are nil (but not both)}
  test eax, eax
  jz @FirstStringNil
  test edx, edx
  jnz @BothStringsNonNil
  {S2 is nil - compare lengths of the strings}
  cmp [eax - 4], edx
  ret

@WStrEqual:
{$IFDEF STREQUAL_INJECTION}
  push eax
  push edx
  mov eax, [esp+8]    // get caller...
  sub eax, 5          // ... and the calling opcode
  cmp BYTE PTR [eax], $E8
  jne @@CallFunction
  call ReplaceWStrCmpByWStrEqual
@@CallFunction:
  pop edx
  pop eax
{$ENDIF STREQUAL_INJECTION}
  jmp _WStrEqual

@FirstStringNil:
  {S1 is nil - compare lengths of the strings}
  cmp eax, [edx - 4]
end;

{------------------------------------------------------------------------------}

function FastStrScan(Str: PChar; Chr: Char): PChar;
begin
  Result := Str;
  if Result <> nil then
  begin
    while Result^ <> #0 do
    begin
      if Result^ = Chr then
        Exit;
      Inc(Result);
    end;
    if Result^ = Chr then
      Exit;
  end;
  Result := nil;
end;

{ Less CALLs by inlining the functions. }
function FastAnsiCompareText(const S1, S2: string): Integer;
type
  PStrRec = ^StrRec;
  StrRec = packed record
    refCnt: Longint;
    length: Longint;
  end;

  function InternCompare(const S1, S2: string): Integer;
  var
    Len1, Len2: Integer;
  begin
    Len1 := PStrRec(Integer(S1) - SizeOf(StrRec)).length;
    Len2 := PStrRec(Integer(S2) - SizeOf(StrRec)).length;
    Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
      Pointer(S1), Len1, Pointer(S2), Len2) - 2;
  end;

{begin
  if S1 = '' then
  begin
    if S2 = '' then
      Result := 0
    else
      Result := -1;
  end
  else
  if S2 = '' then
    Result := 1
  else
    Result := InternCompare(S1, S2);
end;}
asm
  test eax,eax
  jnz @@TestS2

  test edx,edx
  jnz @@More

@@Equal:
  xor eax, eax
  ret

@@More:
  or eax,-$01
  ret

@@TestS2:
  test edx, edx
  jnz InternCompare

@@Less:
  mov eax, 1
  ret
end;

{------------------------------------------------------------------------------}

function FastSameStr(const S1, S2: string): Boolean;
asm
  call _LStrEqual
  setz al
end;

function FastWideSameStr(const S1, S2: WideString): Boolean;
asm
  call _WStrEqual
  setz al
end;

{------------------------------------------------------------------------------}

{ WARNING: Never set a breakpoint in this function }
function FastAnsiByteType(const S: string; Index: Integer): TMbcsByteType;
begin
  Result := mbSingleByte;
end;

{ WARNING: Never set a breakpoint in this function }
function FastAnsiStrByteType(Str: PChar; Index: Cardinal): TMbcsByteType;
begin
  Result := mbSingleByte;
end;

{------------------------------------------------------------------------------}

function GetLStrCmp: Pointer; asm mov eax, OFFSET System.@LStrCmp; end;
function GetWStrCmp: Pointer; asm mov eax, OFFSET System.@WStrCmp; end;
{$IFNDEF COMPILER9}
function GetLStrPos: Pointer; asm mov eax, OFFSET System.@LStrPos; end;
{$ENDIF ~COMPILER9}

{------------------------------------------------------------------------------}
{ List optimization                                                            }
{------------------------------------------------------------------------------}

// FillZeroes32 (from FastObj)
//
procedure FillZeroes32(nbDWords: Integer; p: Pointer);
// fill nbDWords DWORDs with zeroes starting at p, nbDWORDS assumed > 0
asm
   push  edi
   mov   ecx, eax
   mov   edi, edx
   xor   eax, eax
   rep   stosd
   pop   edi
end;

type
  PMetaClass = ^TMetaClass;
  TMetaClass = record
    ClassType: TClass;
  end;

  TFastList = class(TObject)
  public
    FList: PPointerList;
    FCount: Integer;
    FCapacity: Integer;

    function Get(Index: Integer): Pointer;
    function IndexOf(Item: Pointer): Integer;
    procedure SetCount(NewCount: Integer);
    procedure Put(Index: Integer; Item: Pointer);
    procedure Insert(Index: Integer; Item: Pointer);
    procedure Delete(Index: Integer);
    function Last: Pointer;
    procedure Exchange(Index1, Index2: Integer);
    procedure Sort(Compare: TListSortCompare);
  end;

  {$IFDEF COMPILER7_UP}
  TMyStrings = class(TStrings)
  public
    {$IFDEF NOLEADBYTES_HOOK}
    procedure SetDelimitedText(const Value: string);
    {$ENDIF NOLEADBYTES_HOOK}
    function GetDelimitedText: string;
  end;
  {$ENDIF COMPILER7_UP}

  TFastStringList = class(TStrings)
  private
    FList: PStringItemList;
    FCount: Integer;
  public
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure QuickSort(L, R: Integer; SCompare: TStringListSortCompare);
    procedure CustomSort(Compare: TStringListSortCompare); virtual;
  end;

  TOpenList = class(TList);
  TOpenStringList = class(TStringList);
  TOpenObjectList = class(TObjectList);


{------------------------------------------------------------------------------}
function TFastList.Get(Index: Integer): Pointer;
{$IFNDEF COMPILER7}
begin
  if Cardinal(Index) < Cardinal(FCount) then
    Result := FList^[Index]
  else
  begin
    TList(Self).Error(@SListIndexError, Index);
    Result := nil;
  end;
end;
{$ELSE} // stupid Delphi 7 Update - Compiler optimization
asm
  cmp edx, [eax+$08]
  jnb @@Error
  mov eax, [eax+$04]
  mov eax, [eax+edx*4]
  ret
@@Error:
  push eax
  call GetListIndexErrorRs
  mov ecx, eax
  pop eax

  mov eax, [eax]
  xchg edx, ecx
  call TList.Error
  //xor eax, eax
  ret
end;
{$ENDIF}

{.$DEFINE PUREPASCAL}
{------------------------------------------------------------------------------}
function TFastList.IndexOf(Item: Pointer): Integer;
{$IFDEF COMPILER10_UP}
var
  LList: PPointerArray;
  LCount: Integer;
begin
  LCount := FCount;
  LList := PPointerArray(FList);
  for Result := 0 to LCount - 1 do // new optimizer doesn't use [esp] for Result
    if LList[Result] = Item then
      Exit;
  Result := -1;
end;
{$ELSE}
// inlined
asm
  // Result := InternIndexOf(PPointerArray(FList), Item, FCount);
  mov ecx, [eax+$08]
  mov eax, [eax+$04]

  push esi
  //for Result := 0 to Count - 1 do
  dec ecx
  test ecx, ecx
  jl @@LeaveNotFound
  inc ecx
  xor esi, esi
@@Next:
  //  if List[Result] = Item then
  cmp edx, [eax+esi*4]
  jz @@Leave
  //    Exit;
  inc esi
  //for Result := 0 to Count - 1 do
  dec ecx
  jnz @@Next
  // Result := -1;
@@LeaveNotFound:
  or esi, -$01
@@Leave:
  mov eax, esi
  pop esi
//  ret
end;
{$ENDIF COMPILER10_UP}

{------------------------------------------------------------------------------}

procedure TFastList.SetCount(NewCount: Integer); // based on FastObj code
var
  i: Integer;
  LCount: Integer;
begin
  LCount := FCount;
  if NewCount <> LCount then // VCL often calls List.Clear on empty lists in DoAlign
  begin
    if Cardinal(NewCount) <= Cardinal(MaxListSize) then
    begin
      if NewCount > FCapacity then
      begin
        TList(Self).Capacity := NewCount;
        FillZeroes32(NewCount - LCount, @FList^[LCount]);
      end
      else
      begin
        if NewCount > FCount then
          FillZeroes32(NewCount - LCount, @FList^[LCount])
        else
        if PMetaClass(Self).ClassType <> TList then
          for i := LCount - 1 downto NewCount do
            Delete(i);
      end;
      FCount := NewCount;
    end
    else
      TList(Self).Error(@SListCountError, newCount);
  end;
end;

{------------------------------------------------------------------------------}
procedure TFastList.Put(Index: Integer; Item: Pointer);
var
  Temp, LItem: Pointer;
  LList: PPointerList;
begin
  LItem := Item; // take Item(ecx) from [esp] into esi
  if Cardinal(Index) < Cardinal(FCount) then
  begin
    if PMetaClass(Self).ClassType = TList then
    begin
      LList := FList;
      if LItem <> LList^[Index] then
        LList^[Index] := LItem
    end
    else
    begin
      LList := FList;
      if LItem <> LList^[Index] then
      begin
        Temp := LList^[Index];
        LList^[Index] := LItem;
        if Temp <> nil then
          TOpenList(Self).Notify(Temp, lnDeleted);
        if Item <> nil then
          TOpenList(Self).Notify(LItem, lnAdded);
      end
    end;
  end
  else
    TList(Self).Error(@SListIndexError, Index);
end;

{------------------------------------------------------------------------------}
procedure TFastList.Insert(Index: Integer; Item: Pointer);
var
  Diff: Integer;
  LList: PPointerList;
begin
  if Cardinal(Index) <= Cardinal(FCount) then
  begin
    if FCount = FCapacity then
      TOpenList(Self).Grow;
    Diff := FCount - Index;
    if Diff > 0 then
    begin
      LList := FList;
      Move(LList^[Index], LList^[Index + 1], Diff * SizeOf(Pointer));
    end;
    FList^[Index] := Item;
    Inc(FCount);
    if (Item <> nil) and (PMetaClass(Self).ClassType <> TList) then
      TOpenList(Self).Notify(Item, lnAdded);
  end
  else
    TList(Self).Error(@SListIndexError, Index);
end;

{------------------------------------------------------------------------------}
procedure TFastList.Delete(Index: Integer);
var
  Temp: Pointer;
  Diff: Integer;
  LList: PPointerList;
  Item: Pointer;
begin
  if Cardinal(Index) < Cardinal(FCount) then
  begin
    LList := FList;
    Temp := LList^[Index];
    Dec(FCount);
    Diff := FCount - Index;
    if Diff > 0 then
      Move(LList^[Index + 1], LList^[Index], Diff * SizeOf(Pointer));
    Item := Temp; // why can't the compiler detect itself that some CPU registers are free 
    if (Item <> nil) and (PMetaClass(Self).ClassType <> TList) then
      TOpenList(Self).Notify(Item, lnDeleted);
  end
  else
    TList(Self).Error(@SListIndexError, Index);
end;

{------------------------------------------------------------------------------}
function TFastList.Last: Pointer;
{$IFNDEF COMPILER7}
var
  LCount: Integer;
begin
  LCount := FCount;
  if LCount > 0 then
    Result := FList^[LCount - 1]
  else
  begin
    TList(Self).Error(@SListIndexError, 0);
    Result := nil;
  end;
end;
{$ELSE} // stupid Delphi 7 Update - Compiler optimization
asm
  mov edx, [eax+$08]
  test edx, edx
  jle @@Error
  mov ecx, [eax+$04]
  mov eax, [ecx+edx*4-$04]
  ret

@@Error:
  push eax
  call GetListIndexErrorRs
  mov edx, eax
  pop eax

  mov eax, [eax]
  xor ecx, ecx
  call TList.Error
  //xor eax, eax
  ret
end;
{$ENDIF}

{------------------------------------------------------------------------------}
procedure TFastList.Exchange(Index1, Index2: Integer);
var
  Item: Pointer;
  LCount: Cardinal;
  LList: PPOinterList;
begin
  LCount := Cardinal(FCount);
  if Cardinal(Index1) < LCount then
  begin
    if Cardinal(Index2) < LCount then
    begin
      LList := FList;
      Item := LList^[Index1];
      LList^[Index1] := LList^[Index2];
      LList^[Index2] := Item;
    end
    else
      TList(Self).Error(@SListIndexError, Index2);
  end
  else
    TList(Self).Error(@SListIndexError, Index1);
end;

{------------------------------------------------------------------------------}
procedure QuickSort(SortList: PPointerList; L, R: Integer; SCompare: TListSortCompare);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while SCompare(SortList^[I], P) < 0 do
        Inc(I);
      while SCompare(SortList^[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          T := SortList^[I];
          SortList^[I] := SortList^[J];
          SortList^[J] := T;
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(SortList, L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TFastList.Sort(Compare: TListSortCompare);
var
  LCount: Integer;
begin
  LCount := FCount;
  if (FList <> nil) and (LCount > 1) then
    QuickSort(FList, 0, LCount - 1, Compare);
end;

{------------------------------------------------------------------------------}

{ WARNING: Never set a breakpoint in this function }
procedure TObjectList_Notify(List: TObjectList; Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then  // swap the two IFs for less memory accesses
    if List.OwnsObjects then
      if Ptr <> nil then
        TObject(Ptr).Destroy;
      //TObject(Ptr).Free;   can't be injected due to relative CALL, Destroy uses the VMT
  //inherited Notify(Ptr, Action); does nothing, so don't call it
end;

procedure TObjectList_NotifyEND;
begin
end;

{------------------------------------------------------------------------------}

procedure TFastStringList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Integer;
  Item1, Item2: PStringItem;
  List: PStringItemList;
begin
  List := FList; // remove one (object) memory access
  Item1 := @List^[Index1];
  Item2 := @List^[Index2];
  Temp := Integer(Item1^.FString);
  Integer(Item1^.FString) := Integer(Item2^.FString);
  Integer(Item2^.FString) := Temp;
  Temp := Integer(Item1^.FObject);
  Integer(Item1^.FObject) := Integer(Item2^.FObject);
  Integer(Item2^.FObject) := Temp;
end;

procedure TFastStringList.QuickSort(L, R: Integer; SCompare: TStringListSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(TStringList(Self), I, P) < 0 do
        Inc(I);
      while SCompare(TStringList(Self), J, P) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
          ExchangeItems(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TFastStringList.CustomSort(Compare: TStringListSortCompare);
begin
  with TOpenStringList(Self) do
  begin
    if not Sorted and (FCount > 1) then
    begin
      Changing;
      QuickSort(0, FCount - 1, Compare);
      Changed;
    end;
  end;
end;

{-------------------------------------------------------------------}

{$IFDEF COMPILER7_UP}

{$IFDEF NOLEADBYTES_HOOK}
{ Requires: LeadBytes = [] }
procedure TMyStrings.SetDelimitedText(const Value: string);
var
  P, P1: PChar;
  S: string;
  LStrictDelimiter: Boolean;
  LDelimiter: Char;
  QuoteCh: Char;
begin
  {$IFDEF COMPILER10_UP}
  LStrictDelimiter := StrictDelimiter;
  {$ELSE}
  LStrictDelimiter := False;
  {$ENDIF COMPILER10_UP}
  LDelimiter := Delimiter;
  QuoteCh := QuoteChar;

  BeginUpdate;
  try
    Clear;
    P := PChar(Value);
    if not LStrictDelimiter then
      while P^ in [#1..' '] do
        Inc(P);
    while P^ <> #0 do
    begin
      if P^ = QuoteCh then
        S := AnsiExtractQuotedStr(P, QuoteCh)
      else
      begin
        P1 := P;
        while ((not LStrictDelimiter and (P^ > ' ')) or
              (LStrictDelimiter and (P^ <> #0))) and (P^ <> LDelimiter) do
          Inc(P);
        SetString(S, P1, P - P1);
      end;
      Add(S);
      if not LStrictDelimiter then
        while P^ in [#1..' '] do
          Inc(P);

      if P^ = LDelimiter then
      begin
        P1 := P;
        if P1[1] = #0 then
          Add('');
        repeat
          Inc(P);
        until not (not LStrictDelimiter and (P^ in [#1..' ']));
      end;
    end;
  finally
    EndUpdate;
  end;
end;
{$ENDIF NOLEADBYTES_HOOK}

function TMyStrings.GetDelimitedText: string;
var
  S: string;
  P: PChar;
  I, Count: Integer;
  LDelimiters: TSysCharSet;
  QuoteCh: Char;
  Delim: string;
begin
  Count := GetCount;
  QuoteCh := QuoteChar;
  if (Count = 1) and (Get(0) = '') then
    Result := QuoteChar + QuoteChar
  else
  begin
    Result := '';
    if Count = 0 then
      Exit;
    Delim := Delimiter; // convert char to string here to remove this conversation from the loop
    LDelimiters := [#0, QuoteCh, Delim[1]];
    {$IFDEF COMPILER10_UP}
    if not StrictDelimiter then
    {$ENDIF COMPILER10_UP}
      LDelimiters := LDelimiters + [#1..' '];

    {$IFDEF NOLEADBYTES_HOOK}
    if not NoLeadBytes then
    begin
    {$ENDIF NOLEADBYTES_HOOK}
      for I := 0 to Count - 1 do
      begin
        S := Get(I);
        P := Pointer(S);
        if P <> nil then
        begin
          while not (P^ in LDelimiters) do
            P := CharNext(P);
          if P^ <> #0 then
            S := AnsiQuotedStr(S, QuoteCh);
        end;
        if I > 0 then
          Result := Result + Delim + S
        else
          Result := S;
      end;
    {$IFDEF NOLEADBYTES_HOOK}
    end
    else
    begin
      for I := 0 to Count - 1 do
      begin
        S := Get(I);
        P := Pointer(S);
        if P <> nil then
        begin
          while not (P^ in LDelimiters) do
            Inc(P);
          if P^ <> #0 then
            S := AnsiQuotedStr(S, QuoteCh);
        end;
        if I > 0 then
          Result := Result + Delim + S
        else
          Result := S;
      end;
    end;
    {$ENDIF NOLEADBYTES_HOOK}
  end;
end;

{ Helps to get the addresses of private methods }
type
  TPublishedStrings = class(TStrings)
  published
    property DelimitedText;
  end;

function GetGetDelimitedText: Pointer;
var
  Prop: PPropInfo;
begin
  Prop := GetPropInfo(TPublishedStrings, 'DelimitedText');
  if Prop <> nil then Result := Prop.GetProc else
    Result := nil;
end;

function GetSetDelimitedText: Pointer;
var
  Prop: PPropInfo;
begin
  Prop := GetPropInfo(TPublishedStrings, 'DelimitedText');
  if Prop <> nil then Result := Prop.SetProc else
    Result := nil;
end;
{$ENDIF COMPILER7_UP}

{------------------------------------------------------------------------------}
{ TComponent optimization                                                      }
{------------------------------------------------------------------------------}
{.$REGION 'class TNameHashList'}
type
  PNameCompItem = ^TNameCompItem;
  TNameCompItem = record
    Key: string;
    Value: TComponent;
    Next: PNameCompItem;
  end;

  TNameHashList = class(TList)
  private
    FNameCount: Integer;
    FItems: array[0..64 - 1] of PNameCompItem;

    function NameFind(const AItem: string; out Value: TComponent): Boolean; overload;
    function NameAdd(const AItem: string; AData: TComponent): TComponent; overload;
    function NameRemove(const AItem: string): TComponent;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    procedure Clear; override;
  end;

{ A very simple but fast string hash algorithm (no prime number) }
function HashUpString(const AItem: string): Integer;
asm
  or eax, eax
  jz @@Leave

  xchg eax, edx
  mov eax, [edx-$04] // Length(AItem)
  xor ecx, ecx

@@HashStringNextChar:
  mov cl, [edx]

  cmp     cl, 'a'
  jb      @@UpCaseEnd
  cmp     cl, 'z'
  ja      @@UpCaseEnd
  sub     cl, 'a' - 'A'
@@UpCaseEnd:

  ror cl, 4
  shl cx, 1
  add eax, ecx
  xor ch, ch
  inc edx
  or ecx, ecx
  jnz @@HashStringNextChar

  and eax, 64-1
@@Leave:
end;

{ TNameHashList }

procedure TNameHashList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  case Action of
    lnAdded:
      if TComponent(Ptr).Name <> '' then
        NameAdd(TComponent(Ptr).Name, TComponent(Ptr));
    lnExtracted, lnDeleted:
      if TComponent(Ptr).Name <> '' then
        NameRemove(TComponent(Ptr).Name);
  end;
end;

procedure TNameHashList.Clear;
var
  P, N: PNameCompItem;
  i: Integer;
begin
  if FNameCount > 0 then
  begin
    for i := 0 to High(FItems) do
    begin
      P := FItems[i];
      while P <> nil do
      begin
        N := P.Next;
        Dispose(P);
        P := N;
        Dec(FNameCount);
      end;
      FItems[i] := nil;
      if FNameCount = 0 then
        Break;
    end;
  end;
  FNameCount := 0;
  inherited Clear;
end;

function TNameHashList.NameAdd(const AItem: string; AData: TComponent): TComponent;
var
  N: PNameCompItem;
  AHash: Integer;
begin
  New(N);
  AHash := HashUpString(AItem);
  N.Next := FItems[AHash];
  FItems[AHash] := N;
  Inc(FNameCount);
  N.Key := AItem;
  N.Value := AData;
  Result := AData;
end;

function TNameHashList.NameRemove(const AItem: string): TComponent;
var
  Index: Integer;
  P, N: PNameCompItem;
begin
  if FNameCount > 0 then
  begin
    Index := HashUpString(AItem);
    N := FItems[Index];
    if N <> nil then
    begin
      if CompareText(N.Key, AItem) = 0 then
      begin
        Result := N.Value;
        P := N.Next;
        Dispose(N);
        FItems[Index] := P;
        Dec(FNameCount);
        Exit;
      end
      else
      begin
        P := N;
        N := N.Next;
        while N <> nil do
        begin
          if CompareText(N.Key, AItem) = 0 then
          begin
            Result := N.Value;
            P.Next := N.Next;
            Dispose(N);
            Dec(FNameCount);
            Exit;
          end;
          P := N;
          N := N.Next;
        end;
      end;
    end;
  end;
  Result := nil;
end;

function TNameHashList.NameFind(const AItem: string; out Value: TComponent): Boolean;
var
  N: PNameCompItem;
  AHash: Integer;
begin
  Value := nil;
  AHash := HashUpString(AItem);
  N := FItems[AHash];
  while N <> nil do
  begin
    if CompareText(N.Key, AItem) = 0 then
    begin
      Value := N.Value;
      Result := True;
      Exit;
    end;
    N := N.Next;
  end;
  Result := False;
end;
{.$ENDREGION}

type
  TFastComponent = class(TComponent)
  protected
    function ReplaceComponentList: TNameHashList;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ChangeName(const NewName: TComponentName);
  public
    procedure Destroying;
    function FindComponent(const AName: string): TComponent;
  end;

  TOpenComponent = class(TComponent);

  TPrivateComponent = class(TPersistent{, IInterface, IInterfaceComponentReference})
  public
    FOwner: TComponent;
    FName: TComponentName;
    FTag: Longint;
    FComponents: TNameHashList{TList};
    FFreeNotifies: TList;
    FDesignInfo: Longint;
    {$IFDEF COMPILER5}
    FVCLComObject: Pointer;
    {$ENDIF COMPILER5}
    FComponentState: TComponentState;
    {$IFDEF COMPILER6_UP}
    //FVCLComObject: Pointer;
    {$ENDIF COMPILER6_UP}
  end;

{ TFastComponent }

function TFastComponent.ReplaceComponentList: TNameHashList;
var
  List: TList;
  I: Integer;
begin
  Result := TNameHashList.Create;
  List := TPrivateComponent(Self).FComponents;
  Result.Capacity := List.Capacity;
  for I := 0 to List.Count - 1 do
    Result.Add(List.List[I]); // copy and hash
  TPrivateComponent(Self).FComponents := Result;
  List.Free;
end;

procedure TFastComponent.Destroying;

  procedure InternDestroying(Owner: TComponent);
  var
    I: Integer;
    Comps: TList;
    Comp: TPrivateComponent;
  begin
    Comps := TPrivateComponent(Owner).FComponents;
    for I := 0 to Comps.Count - 1 do
    begin
      Comp := TPrivateComponent(Comps.List[I]);
      if not (csDestroying in Comp.FComponentState) then
      begin
        Include(Comp.FComponentState, csDestroying);
        if Comp.FComponents <> nil then
          InternDestroying(TComponent(Comp));
      end;
    end;
  end;

var
  I: Integer;
  Comps: TList;
  Comp: TPrivateComponent;
begin
  if not (csDestroying in TPrivateComponent(Self).FComponentState) then
  begin
    Include(TPrivateComponent(Self).FComponentState, csDestroying);
    Comps := TPrivateComponent(Self).FComponents;
    if Comps <> nil then
      for I := 0 to Comps.Count - 1 do
      begin
        Comp := TPrivateComponent(Comps.List[I]);
        if not (csDestroying in Comp.FComponentState) then
        begin
          Include(Comp.FComponentState, csDestroying);
          if Comp.FComponents <> nil then
            InternDestroying(TComponent(Comp));
        end;
        //TFastComponent(Comps.List[I]).Destroying;
      end;
  end;
end;

function TFastComponent.FindComponent(const AName: string): TComponent;
var
  Comps: TNameHashList;
begin
  if AName <> '' then
  begin
    Comps := TPrivateComponent(Self).FComponents;
    if Comps <> nil then
    begin
      if PMetaClass(Comps).ClassType <> TNameHashList then
        Comps := ReplaceComponentList;

      if Comps.NameFind(AName, Result) then
        Exit;
    end;
  end;
  Result := nil;
end;

procedure TFastComponent.ChangeName(const NewName: TComponentName);
var
  Comps: TNameHashList;
begin
  if (Owner <> nil) then
  begin
    Comps := TPrivateComponent(Owner).FComponents;
    if (Comps <> nil) and (PMetaClass(Comps).ClassType = TNameHashList) then
    begin
      if Name <> '' then
        Comps.NameRemove(Name);
      if NewName <> '' then
        Comps.NameAdd(NewName, Self);
    end;
  end;
  TPrivateComponent(Self).FName := NewName;
end;

procedure TFastComponent.Notification(AComponent: TComponent; Operation: TOperation);
var
  I, CompCount: Integer;
  Comps: TList;
begin
  if (Operation = opRemove) and (AComponent <> nil) then
    RemoveFreeNotification(AComponent);

  Comps := TPrivateComponent(Self).FComponents;
  if Comps <> nil then
  begin
    I := Comps.Count - 1;
    while I >= 0 do
    begin
      TOpenComponent(Comps.List[I]).Notification(AComponent, Operation);
      Dec(I);
      CompCount := Comps.Count;
      if I >= CompCount then
        I := CompCount - 1;
    end;
  end;
end;

{------------------------------------------------------------------------------}
{ File optimization                                                            }
{------------------------------------------------------------------------------}
{$IFNDEF COMPILER10_UP}
{ GetFileAttributes() is a lot faster than the FindFirstFile call in the original
  FileExists function that calls FileAge. BDS 2006 fixes this. }
function FastFileExists(const Filename: string): Boolean;
begin
  Result := (Filename <> '') and (GetFileAttributes(Pointer(Filename)) and FILE_ATTRIBUTE_DIRECTORY = 0);
end;
{$ENDIF ~COMPILER10_UP}

{------------------------------------------------------------------------------}
{$IFDEF COMPILER5}
const
  PathSep = ';';
  DriveDelim = ':';
  PathDelim = '\';
{$ENDIF COMPILER5}

{$IFNDEF DELPHI2007_UP}
function FastFileSearch(const Name, DirList: string): string;
var
  I, P, L: Integer;
  C: Char;
begin
  Result := Name;
  if Result = '' then
    Exit;
  P := 0;
  L := Length(DirList) - 1;
  while True do
  begin
    if FileExists(Result) then
      Exit;
    while (P <= L) and (DirList[P + 1] = PathSep) do
      Inc(P);
    if P > L then
      Break;
    I := P;
    {$IFDEF NOLEADBYTES_HOOK}
    if not NoLeadBytes then
    begin
    {$ENDIF NOLEADBYTES_HOOK}
      while (P <= L) and (DirList[P + 1] <> PathSep) do
      begin
        if DirList[P + 1] in LeadBytes then
        {$IFNDEF COMPILER6_UP}
          Inc(P);
        {$ELSE}
          P := NextCharIndex(DirList, P)
        else
        {$ENDIF ~COMPILER6_UP}
          Inc(P);
      end;
      Result := Copy(DirList, I + 1, P - I);
      C := AnsiLastChar(Result)^;
    {$IFDEF NOLEADBYTES_HOOK}
    end
    else
    begin
      while (P <= L) and (DirList[P + 1] <> PathSep) do
        Inc(P);
      Result := Copy(DirList, I + 1, P - I);
      if Result <> '' then
        C := Result[Length(Result)]
      else
        C := #0;
    end;
    {$ENDIF NOLEADBYTES_HOOK}
    if (C <> DriveDelim) and (C <> PathDelim) then
      Result := Result + PathDelim;
    Result := Result + Name;
  end;
  Result := '';
end;

{------------------------------------------------------------------------------}
var
  GetFileAttributesExFunc: function(lpFileName: PChar; fInfoLevelId: TGetFileExInfoLevels;
    lpFileInformation: Pointer): BOOL; stdcall;
  {$WARNINGS OFF} // deprecated warning
  SysUtils_FileAge: function(const FileName: string): Integer = SysUtils.FileAge;
  {$IFDEF COMPILER10_UP}
  SysUtils_FileAgeNew: function(const FileName: string; out FileDateTime: TDateTime): Boolean = SysUtils.FileAge;
  {$ENDIF COMPILER10_UP}
  {$WARNINGS ON}

function FastFileAge(const FileName: string): Integer;
var
  FindData: TWin32FileAttributeData;
  LocalFileTime: TFileTime;
begin
  if GetFileAttributesExFunc(Pointer(Filename), GetFileExInfoStandard, @FindData) then
  begin
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
      if FileTimeToDosDateTime(LocalFileTime, LongRec(Result).Hi, LongRec(Result).Lo) then
        Exit;
    end;
  end;
  Result := -1;
end;

function FastFileAgeNew(const FileName: string; out FileDateTime: TDateTime): Boolean;
var
  FindData: TWin32FindData;
  LSystemTime: TSystemTime;
  LocalFileTime: TFileTime;
begin
  Result := False;
  if GetFileAttributesExFunc(Pointer(Filename), GetFileExInfoStandard, @FindData) then
  begin
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      Result := True;
      FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
      FileTimeToSystemTime(LocalFileTime, LSystemTime);
      with LSystemTime do
        FileDateTime := EncodeDate(wYear, wMonth, wDay) +
          EncodeTime(wHour, wMinute, wSecond, wMilliSeconds);
    end;
  end;
end;
{$ENDIF ~DELPHI2007}

{------------------------------------------------------------------------------}
{ System optimizations                                                         }
{------------------------------------------------------------------------------}

function GetCallDynaInstAddr: Pointer; asm mov eax, OFFSET System.@CallDynaInst; end;
function GetCallDynaClassAddr: Pointer; asm mov eax, OFFSET System.@CallDynaClass; end;
function GetFindDynaInstAddr: Pointer; asm mov eax, OFFSET System.@FindDynaInst; end;
function GetFindDynaClassAddr: Pointer; asm mov eax, OFFSET System.@FindDynaClass; end;

type
  IntPtr = Integer;
  {$IFDEF COMPILER5}
  PPointerArray = ^TPointerArray;
  TPointerArray = array[0..MaxInt div SizeOf(Pointer) - 1] of Pointer;
  {$ENDIF COMPILER5}

{$IFDEF COMPILER5}
const
  RaiseLastOSError: procedure = RaiseLastWin32Error;
{$ENDIF COMPILER5}

procedure SortDMT(IndexList: PWordArray; L, R: Integer; AddrList: PPointerArray);
var
  I, J: Integer;
  P, T: Word;
  TT: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := IndexList[(L + R) shr 1];
    repeat
      while IndexList[I] < P do
        Inc(I);
      while IndexList[J] > P do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          T := IndexList[I];
          IndexList[I] := IndexList[J];
          IndexList[J] := T;
          TT := AddrList[I];
          AddrList[I] := AddrList[J];
          AddrList[J] := TT;
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      SortDMT(IndexList, L, J, AddrList);
    L := I;
  until I >= R;
end;

type
  PDmt = ^TDmt;
  TDmt = packed record
    Count: Word;
    IndexList: array[0..0] of Word;
    {AddressList: array[0..0] of Pointer;}
  end;
  PPDmt = ^PDmt;

function InitDMTBinSearch(DmtP: PPDmt): PDmt;
{ called only once for each DMT }
var
  Size: Cardinal;
  Count: Integer;
  P: PWord;
  NewDmt: PDmt;
  NumWritten: Cardinal;
begin
  Count := DmtP^.Count;
  Size := 2 + Count * (SizeOf(Word) + SizeOf(Pointer));
  P := GlobalAllocPtr(GMEM_FIXED, 2 + Size + 4);
  if P <> nil then
  begin
    P^ := $FACE;
    NewDmt := Pointer(IntPtr(P) + SizeOf(Word));
    Move(DmtP^^, NewDmt^, Size);
    SortDMT(@NewDmt.IndexList, 0, Count - 1, Pointer(IntPtr(@NewDmt.IndexList) + Count * SizeOf(Word)));
    if not WriteProcessMemory(GetCurrentProcess, DmtP, @NewDmt, SizeOf(Pointer), NumWritten) then
      RaiseLastOSError;
  end
  else
  begin
    NewDmt := DmtP^;
    SortDMT(@NewDmt.IndexList, 0, Count - 1, Pointer(IntPtr(@NewDmt.IndexList) + Count * SizeOf(Word)));
  end;

  Result := NewDmt;
end;

function DMTBinSearch(Index: Word; IndexList: PWordArray; Count: Integer): Integer;
{$IFDEF PUREPASCAL}
var
  L, H: Integer;
begin
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    Result := (L + H) shr 1;
    if IndexList[Result] = Index then
      Exit
    else
    if IndexList[Result] < Index then
      L := Result + 1
    else
      H := Result - 1;
  end;
  Result := -1;
end;
{$ELSE}
asm
// begin
   push esi
   push edi
// L := 0;
   xor esi,esi
// H := Count - 1;
   mov edi,ecx
   dec edi
//  while L <= H do
   cmp edi,esi
   jl @@NotFound
@@Loop:
//  Result := (L + H) shr 1;
   lea ecx,[edi+esi]
   shr ecx,1
//  if IndexList[Result] = Index then
   cmp ax,[edx+ecx*2]
   jz @@Found
//  if IndexList[Result] < Index then
   cmp ax,[edx+ecx*2] // looks like this optimizes the CPU cache
   jbe @@Greater
//  L := Result + 1
   lea esi,[ecx+$01]
//  while L <= H do
   cmp edi,esi
   jnl @@Loop
   jmp @@NotFound
@@Greater:
//  H := Result - 1;
   mov edi,ecx
   dec edi
@@Next:
//  while L <= H do
   cmp edi,esi
   jnl @@Loop
@@NotFound:
//  Result := -1;
   or ecx,-$01
@@Found:
//  end;
   mov eax,ecx
   pop edi
   pop esi
end;
{$ENDIF PUREPASCAL}

function DMTSearch(Index: Integer; DmtP: PPDmt; Count: Integer): Pointer;
var
  Idx: Integer;
  Dmt: PDmt;
begin
  Dmt := DmtP^;
  if PWord(IntPtr(Dmt) - SizeOf(Word))^ <> $FACE then
    Dmt := InitDMTBinSearch(DmtP);
  Idx := DMTBinSearch(Index, @Dmt.IndexList, Count);

  if Idx <> -1 then
    Result := PPointerArray(IntPtr(@Dmt.IndexList) + (Count * SizeOf(Word)))^[Idx]
  else
    Result := nil;
end;

{$IFDEF COMPILER6_UP} { in SI, out ESI }
procedure GetDynaMethod;
{       function        GetDynaMethod(vmt: TClass; selector: Smallint) : Pointer;       }
asm
        { ->    EAX     vmt of class            }
        {       SI      dynamic method index    }
        { <-    ESI pointer to routine  }
        {       ZF = 0 if found         }
        {       trashes: EAX, ECX               }

        PUSH    EDI
        PUSH    EBX
        PUSH    EDX
        XCHG    EAX,ESI
        AND     EAX,$0000FFFF
        MOV     EBX,EAX
        JMP     @@haveVMT
@@outerLoop:
        MOV     ESI,[ESI]
@@haveVMT:
        LEA     EDX,[ESI].vmtDynamicTable
        MOV     EDI,[EDX]
        TEST    EDI,EDI
        JE      @@parent

        MOVZX   ECX,word ptr [EDI]
        MOV     EAX,EBX
        CMP     ECX,1
        JE      @@SingleFind
@@1:
        CALL    DMTSearch
        OR      EAX,EAX
        JNZ     @@found
@@parent:
        MOV     ESI,[ESI].vmtParent
        TEST    ESI,ESI
        JNE     @@outerLoop
        JMP     @@exit

@@SingleFind:
        MOVZX   EDX,word ptr [EDI+2]
        CMP     EAX,EDX
        JNZ     @@parent
        MOV     EAX,[EDI+4]
        TEST    EAX,EAX

@@found:
        MOV     ESI,EAX

@@exit:
        POP     EDX
        POP     EBX
        POP     EDI
end;

procedure oldOptimGetDynaMethod;
{       function        GetDynaMethod(vmt: TClass; selector: Smallint) : Pointer;       }
asm
        { ->    EAX     vmt of class            }
        {       SI      dynamic method index    }
        { <-    ESI pointer to routine  }
        {       ZF = 0 if found         }
        {       trashes: EAX, ECX               }

        PUSH    EDI
        PUSH    EBX
        PUSH    EDX
        XCHG    EAX,ESI
        AND     EAX,$0000FFFF
        JMP     @@haveVMT
@@outerLoop:
        MOV     ESI,[ESI]
@@haveVMT:
        MOV     EDI,[ESI].vmtDynamicTable
        TEST    EDI,EDI
        JE      @@parent
        MOVZX   ECX,word ptr [EDI]
        MOV     EBX,ECX
        ADD     EDI,2
@@Loop:
        MOVZX   EDX,WORD PTR [EDI]
        DEC     ECX
        ADD     EDI,2
        CMP     EAX,EDX
        JE      @@found
        OR      ECX,ECX
        JNZ     @@Loop

@@LeaveLoop:
        MOV     ECX,EBX
@@parent:
        MOV     ESI,[ESI].vmtParent
        TEST    ESI,ESI
        JNE     @@outerLoop
        JMP     @@exit

@@found:
        MOV     EAX,EBX
        ADD     EAX,EAX
        SUB     EAX,ECX         { this will always clear the Z-flag ! }
        MOV     ESI,[EDI+EAX*2-4]

@@exit:
        POP     EDX
        POP     EBX
        POP     EDI
end;
{$ELSE} // COMPILER5 { in BX, out EBX }
procedure GetDynaMethod;
{       function        GetDynaMethod(vmt: TClass; selector: Smallint) : Pointer;       }
asm
        { ->    EAX     vmt of class            }
        {       SI      dynamic method index    }
        { <-    ESI pointer to routine  }
        {       ZF = 0 if found         }
        {       trashes: EAX, ECX               }

        PUSH    EDI
        PUSH    ESI
        PUSH    EDX
        XCHG    EAX,EBX
        AND     EAX,$0000FFFF
        JMP     @@haveVMT
@@outerLoop:
        MOV     EBX,[EBX]
@@haveVMT:
        MOV     EDI,[EBX].vmtDynamicTable
        TEST    EDI,EDI
        JE      @@parent
        MOVZX   ECX,word ptr [EDI]
        MOV     ESI,ECX
        ADD     EDI,2
@@Loop:
        MOVZX   EDX,WORD PTR [EDI]
        DEC     ECX
        ADD     EDI,2
        CMP     EAX,EDX
        JE      @@found
        OR      ECX,ECX
        JNZ     @@Loop

@@LeaveLoop:
        MOV     ECX,ESI
@@parent:
        MOV     EBX,[EBX].vmtParent
        TEST    EBX,EBX
        JNE     @@outerLoop
        JMP     @@exit

@@found:
        MOV     EAX,ESI
        ADD     EAX,EAX
        SUB     EAX,ECX         { this will always clear the Z-flag ! }
        MOV     EBX,[EDI+EAX*2-4]

@@exit:
        POP     EDX
        POP     ESI
        POP     EDI
end;
{$ENDIF COMPILER6_UP}

function GetAddrGetDynaMethod: Pointer;
var
  P: PByteArray;
begin
  P := GetActualAddr(GetCallDynaInstAddr);
  while P[0] <> $E9 do
  begin
    if (P[0] = $E8) and (P[2] = $FF) and (P[3] = $FF) and (P[4] = $FF) then
    begin
      Result := Pointer(Integer(@P[5]) + PInteger(@P[1])^);
      Exit;
    end;
    Inc(PByte(P));
  end;
  Result := nil;
end;

procedure ReplaceAsmCall(Start: Pointer; OrgProc, NewProc: Pointer);
var
  P: PByteArray;
  OldProtect: Cardinal;
begin
  P := GetActualAddr(Start);
  OrgProc := GetActualAddr(OrgProc);
  NewProc := GetActualAddr(NewProc);
  while (P[0] <> $C2) and (P[0] <> $E9) do
  begin
    if (P[0] = $E8) then
    begin
      if Pointer(Integer(@P[5]) + PInteger(@P[1])^) = OrgProc then
      begin
        VirtualProtect(@P[1], 4, PAGE_EXECUTE_READWRITE, OldProtect);
        PInteger(@P[1])^ := Integer(NewProc) - Integer(@P[5]);
        VirtualProtect(@P[1], 4, OldProtect, OldProtect);
        FlushInstructionCache(GetCurrentProcess, @P[1], 4);
        Exit;
      end;
    end;
    Inc(PByte(P));
  end;
end;

function GetSetEqAddr: Pointer; asm mov eax, OFFSET System.@SetEq; end;

procedure _SetEq;
asm
{       FUNCTION _SetEq( CONST l, r: Set; size: Byte): ConditionCode;   }
{       EAX     =       left operand    }
{       EDX     =       right operand   }
{       CL      =       size of set     }

        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX
        MOV     EDI,EDX

        AND     ECX,0FFH
@@Loop:
        MOVZX   EAX,WORD PTR [ESI]
        MOVZX   EDX,WORD PTR [EDI]
        DEC     ECX
        JZ      @@ByteCheck
        ADD     EDI,2
        ADD     ESI,2
        CMP     EAX,EDX
        JNE     @@Leave
        DEC     ECX
        JNZ     @@Loop
@@Leave:

        POP     EDI
        POP     ESI
        RET

@@ByteCheck:
        CMP     AL,DL
        JNE     @@Leave
        OR      ECX,ECX

        POP     EDI
        POP     ESI
        RET
end;

var
  MainModuleFilename: array[0..MAX_PATH] of Char;

function LoadResourceModule(ModuleName: PChar; CheckOwner: Boolean): LongWord;
const
  LOCALE_SABBREVLANGNAME = $00000003;   { abbreviated language name }
  LOAD_LIBRARY_AS_DATAFILE = 2;
  HKEY_CURRENT_USER = $80000001;
  KEY_ALL_ACCESS = $000F003F;
  KEY_READ = $000F0019;

  OldLocaleOverrideKey = 'Software\Borland\Delphi\Locales'; // do not localize
  NewLocaleOverrideKey = 'Software\Borland\Locales'; // do not localize
var
  FileName: array[0..MAX_PATH] of Char;
  Key: HKey;
  LocaleName, LocaleOverride: array[0..4] of Char;
  Size: Integer;
  P: PChar;

  function FindBS(Current: PChar): PChar;
  begin
    Result := Current;
    while (Result^ <> #0) and (Result^ <> '\') do
      Result := CharNext(Result);
  end;

  function ToLongPath(AFileName: PChar; BufSize: Integer): PChar;
  var
    CurrBS, NextBS: PChar;
    Handle, L: Integer;
    FindData: TWin32FindData;
    Buffer: array[0..MAX_PATH] of Char;
    GetLongPathName: function (ShortPathName: PChar; LongPathName: PChar;
      cchBuffer: Integer): Integer stdcall;
  begin
    Result := AFileName;
    Handle := GetModuleHandle(kernel32);
    if Handle <> 0 then
    begin
      @GetLongPathName := GetProcAddress(Handle, 'GetLongPathNameA');
      if Assigned(GetLongPathName) and
        (GetLongPathName(AFileName, Buffer, SizeOf(Buffer)) <> 0) then
      begin
        lstrcpyn(AFileName, Buffer, BufSize);
        Exit;
      end;
    end;

    if AFileName[0] = '\' then
    begin
      if AFileName[1] <> '\' then
        Exit;
      CurrBS := FindBS(AFileName + 2);  // skip server name
      if CurrBS^ = #0 then Exit;
      CurrBS := FindBS(CurrBS + 1);     // skip share name
      if CurrBS^ = #0 then Exit;
    end else
      CurrBS := AFileName + 2;          // skip drive name

    L := CurrBS - AFileName;
    lstrcpyn(Buffer, AFileName, L + 1);
    while CurrBS^ <> #0 do
    begin
      NextBS := FindBS(CurrBS + 1);
      if L + (NextBS - CurrBS) + 1 > SizeOf(Buffer) then
        Exit;
      lstrcpyn(Buffer + L, CurrBS, (NextBS - CurrBS) + 1);

      Handle := FindFirstFile(Buffer, FindData);
      if (Handle = -1) then Exit;
      Windows.FindClose(Handle);

      if L + 1 + Integer(strlen(FindData.cFileName)) + 1 > SizeOf(Buffer) then
        Exit;
      Buffer[L] := '\';
      lstrcpyn(Buffer + L + 1, FindData.cFileName, Sizeof(Buffer) - L - 1);
      Inc(L, strlen(FindData.cFileName) + 1);
      CurrBS := NextBS;
    end;
    lstrcpyn(AFileName, Buffer, BufSize);
  end;
begin
  if MainModuleFileName[0] = #0 then
  begin
    GetModuleFileName(0, MainModuleFileName, SizeOf(MainModuleFileName)); // Get host application name
    ToLongPath(MainModuleFileName, sizeof(MainModuleFileName));
  end;
  LocaleOverride[0] := #0;
  if (RegOpenKeyEx(HKEY_CURRENT_USER, NewLocaleOverrideKey, 0, KEY_READ, Key) = 0) or
   (RegOpenKeyEx(HKEY_LOCAL_MACHINE, NewLocaleOverrideKey, 0, KEY_READ, Key) = 0) or
   (RegOpenKeyEx(HKEY_CURRENT_USER, OldLocaleOverrideKey, 0, KEY_READ, Key) = 0) then
  try
    Size := sizeof(LocaleOverride);
    if RegQueryValueEx(Key, MainModuleFileName, nil, nil, @LocaleOverride[0], @Size) <> 0 then
      if RegQueryValueEx(Key, '', nil, nil, @LocaleOverride[0], @Size) <> 0 then
        LocaleOverride[0] := #0;
    LocaleOverride[sizeof(LocaleOverride)-1] := #0;
  finally
    RegCloseKey(Key);
  end;
  lstrcpyn(FileName, ModuleName, sizeof(FileName));
  GetLocaleInfo(GetThreadLocale, LOCALE_SABBREVLANGNAME, LocaleName, sizeof(LocaleName));
  Result := 0;
  if (FileName[0] <> #0) and ((LocaleName[0] <> #0) or (LocaleOverride[0] <> #0)) then
  begin
    P := PChar(@FileName) + strlen(FileName);
    while (P^ <> '.') and (P <> @FileName) do Dec(P);
    if P <> @FileName then
    begin
      Inc(P);
      // First look for a locale registry override
      if LocaleOverride[0] <> #0 then
      begin
        lstrcpyn(P, LocaleOverride, sizeof(FileName) - (P - FileName));
        Result := LoadLibraryEx(FileName, 0, LOAD_LIBRARY_AS_DATAFILE);
      end;
      if (Result = 0) and (LocaleName[0] <> #0) then
      begin
        // Then look for a potential language/country translation
        lstrcpyn(P, LocaleName, sizeof(FileName) - (P - FileName));
        Result := LoadLibraryEx(FileName, 0, LOAD_LIBRARY_AS_DATAFILE);
        if Result = 0 then
        begin
          // Finally look for a language only translation
          LocaleName[2] := #0;
          lstrcpyn(P, LocaleName, sizeof(FileName) - (P - FileName));
          Result := LoadLibraryEx(FileName, 0, LOAD_LIBRARY_AS_DATAFILE);
        end;
      end;
    end;
  end;
end;


type
  TResStringRecCache = record
    Res: PResStringRec;
    S: string;
    RefCount: Longint;
  end;

var
  LastResStringRecs: array[0..64 - 1] of TResStringRecCache;
  LastResStringModule: LongWord;
  LastResModule: LongWord;

function LoadResString(ResStringRec: PResStringRec): string;
var
  Buffer: array [0..4095] of char;
  Inst: LongWord;
  MinValue: Longint;
  I, MinIndex: Integer;
  P: PResStringRec;
begin
  if ResStringRec = nil then
    Exit;
  if ResStringRec.Identifier < 64*1024 then
  begin
    { Find a cached item and obtain the index of the item that has the lowest reference count } 
    MinValue := 32;
    MinIndex := 0;
    for I := 0 to High(LastResStringRecs) do
    begin
      P := LastResStringRecs[I].Res;
      if ResStringRec = P then
      begin
        with P^ do
        begin
          if (ResStringRec^.Module = Module) and (ResStringRec^.Identifier = Identifier) then // is it still valid
          begin
            with LastResStringRecs[I] do
            begin
              Inc(RefCount);
              if RefCount >= 32 then // make place for new items
                RefCount := 0;
              Result := S;
            end;
            Exit;
          end
          else
          begin
            with LastResStringRecs[I] do
            begin
              Res := nil;
              RefCount := 0;
              S := '';
            end;
          end;
        end;
      end;

      { Find lowest reference count }
      with LastResStringRecs[I] do
        if RefCount < MinValue then
        begin
          MinValue := RefCount;
          MinIndex := I;
        end;
    end;

    with LastResStringRecs[MinIndex] do
    begin
      Res := ResStringRec;
      Inc(RefCount);
    end;

    { Single item cache for the resource module handle }
    if ResStringRec.Module^ = LastResStringModule then
      Inst := LastResModule
    else
    begin
      LastResStringModule := ResStringRec.Module^;
      Inst := FindResourceHInstance(LastResStringModule);
      LastResModule := Inst;
    end;

    SetString(Result, Buffer,
      LoadString(Inst, ResStringRec.Identifier, Buffer, SizeOf(Buffer)));
      
    LastResStringRecs[MinIndex].S := Result;
  end
  else
    Result := PChar(ResStringRec.Identifier);
end;

{------------------------------------------------------------------------------}
{ VCL optimization                                                             }
{------------------------------------------------------------------------------}
{$IFNDEF DELPHI2007_UP}
{$IFDEF WMPAINT_HOOK}
type
  TOpenWinControl = class(TWinControl);

{ Code by Pierre le Riche posted to Quality Central.
  Optimizes the double buffered drawing, only draws the clipping rectangle not
  the whole control what the original TWinControl.WMPaint implementation does. }
procedure WinControlWMPaint(Control: TWinControl; var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
begin
  if not Control.DoubleBuffered or (Message.DC <> 0) then
  begin
    if not (csCustomPaint in Control.ControlState) and (Control.ControlCount = 0) then
      Control.DefaultHandler(Message)
    else
      TOpenWinControl(Control).PaintHandler(Message);
  end
  else
  begin
    DC := BeginPaint(Control.Handle, PS);
    MemBitmap := CreateCompatibleBitmap(DC, PS.rcPaint.Right - PS.rcPaint.Left,
      PS.rcPaint.Bottom - PS.rcPaint.Top);
    MemDC := CreateCompatibleDC(DC);
    OldBitmap := SelectObject(MemDC, MemBitmap);
    SetWindowOrgEx(MemDC, PS.rcPaint.Left, PS.rcPaint.Top, nil);
    try
      Control.Perform(WM_ERASEBKGND, MemDC, MemDC);
      Message.DC := MemDC;
      WinControlWMPaint(Control, Message);
      Message.DC := 0;
      BitBlt(DC, PS.rcPaint.Left, PS.rcPaint.Top,
        PS.rcPaint.Right - PS.rcPaint.Left,
        PS.rcPaint.Bottom - PS.rcPaint.Top,
        MemDC,
        PS.rcPaint.Left, PS.rcPaint.Top,
        SRCCOPY);
    finally
      EndPaint(Control.Handle, PS);
      SelectObject(MemDC, OldBitmap);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
    end;
  end;
end;
{$ENDIF WMPAINT_HOOK}
{$ENDIF ~DELPHI2007_UP}

var
  GetDynaMethodAddr: Pointer;

initialization
  {$IFDEF NOLEADBYTES_HOOK}
  NoLeadBytes := LeadBytes = [];
  {$ENDIF NOLEADBYTES_HOOK}

{------------------------------------------------------------------------------}
{ Long/Wide-String optimization                                                }
{------------------------------------------------------------------------------}
  OrgLStrCmp := GetActualAddr(GetLStrCmp);
  OrgWStrCmp := GetActualAddr(GetWStrCmp);

  CodeRedirect(OrgLStrCmp, @_LStrCmp_LStrEqual);
  CodeRedirect(OrgWStrCmp, @_WStrCmp_WStrEqual);

  CodeRedirect(@SysUtils.AnsiCompareText, @FastAnsiCompareText);
  {$IFDEF COMPILER9_UP}
  CodeRedirect(@SameStr, @FastSameStr);
  {$ENDIF COMPILER9_UP}
  {$IFNDEF COMPILER5}
  CodeRedirect(@WideSameStr, @FastWideSameStr);
  {$ENDIF ~COMPILER5}

  {$IFDEF NOLEADBYTES_HOOK}
  if NoLeadBytes then
  begin
    {$IFNDEF COMPILER9}
    CodeRedirect(@SysUtils.AnsiPos, GetLStrPos);
    {$ENDIF ~COMPILER9}
    CodeRedirect(@SysUtils.AnsiStrScan, @FastStrScan);
    InjectCode(@SysUtils.ByteType, @FastAnsiByteType, 6);
    InjectCode(@SysUtils.StrByteType, @FastAnsiStrByteType, 6);
  end;
  {$ENDIF NOLEADBYTES_HOOK}

{------------------------------------------------------------------------------}
{ List optimization                                                            }
{------------------------------------------------------------------------------}
  CodeRedirect(@TOpenList.IndexOf, @TFastList.IndexOf);
  CodeRedirect(@TOpenList.SetCount, @TFastList.SetCount);
  CodeRedirect(@TOpenList.Put, @TFastList.Put);
  CodeRedirect(@TOpenList.Insert, @TFastList.Insert);
  CodeRedirect(@TOpenList.Last, @TFastList.Last);
  CodeRedirect(@TOpenList.Exchange, @TFastList.Exchange);
  CodeRedirect(@TOpenList.Sort, @TFastList.Sort);
  InjectCode(@TOpenObjectList.Notify, @TObjectList_Notify, Cardinal(@TObjectList_NotifyEND) - Cardinal(@TObjectList_Notify));
  CodeRedirect(@TStringList.CustomSort, @TFastStringList.CustomSort);

  {$IFDEF COMPILER7_UP}
  {$IFDEF NOLEADBYTES_HOOK}
  if NoLeadBytes then
    CodeRedirect(GetSetDelimitedText, @TMyStrings.SetDelimitedText);
  {$ENDIF NOLEADBYTES_HOOK}
  CodeRedirect(GetGetDelimitedText, @TMyStrings.GetDelimitedText);
  {$ENDIF COMPILER7_UP}

{------------------------------------------------------------------------------}
{ TComponent optimization                                                      }
{------------------------------------------------------------------------------}
  CodeRedirect(@TOpenComponent.Notification, @TFastComponent.Notification);
  CodeRedirect(@TOpenComponent.ChangeName, @TFastComponent.ChangeName);
  CodeRedirect(@TOpenComponent.FindComponent, @TFastComponent.FindComponent);
  CodeRedirect(@TOpenComponent.Destroying, @TFastComponent.Destroying);

{------------------------------------------------------------------------------}
{ File optimization                                                            }
{------------------------------------------------------------------------------}
  {$IFNDEF COMPILER10_UP}
  CodeRedirect(@SysUtils.FileExists, @FastFileExists);
  {$ENDIF ~COMPILER10_UP}

  {$IFNDEF DELPHI2007_UP}
  CodeRedirect(@SysUtils.FileSearch, @FastFileSearch);

  GetFileAttributesExFunc := GetProcAddress(GetModuleHandle(kernel32), 'GetFileAttributesExA');
  if Assigned(GetFileAttributesExFunc) then
  begin
    CodeRedirect(@SysUtils_FileAge, @FastFileAge);
    {$IFDEF COMPILER10}
    CodeRedirect(@SysUtils_FileAgeNew, @FastFileAge);
    {$ENDIF COMPILER10}
  end;
  {$ENDIF ~DELPHI2007_UP}

{------------------------------------------------------------------------------}
{ System optimizations                                                         }
{------------------------------------------------------------------------------}
  GetDynaMethodAddr := GetAddrGetDynaMethod;
  ReplaceAsmCall(GetCallDynaInstAddr, GetDynaMethodAddr, @GetDynaMethod);
  ReplaceAsmCall(GetCallDynaClassAddr, GetDynaMethodAddr, @GetDynaMethod);
  ReplaceAsmCall(GetFindDynaInstAddr, GetDynaMethodAddr, @GetDynaMethod);
  ReplaceAsmCall(GetFindDynaClassAddr, GetDynaMethodAddr, @GetDynaMethod);
  ReplaceAsmCall(@TObject.Dispatch, GetDynaMethodAddr, @GetDynaMethod);

  CodeRedirect(GetActualAddr(GetSetEqAddr), @_SetEq);
  CodeRedirect(GetActualAddr(@System.LoadResourceModule), @LoadResourceModule);
  CodeRedirect(GetActualAddr(@System.LoadResString), @LoadResString);

{------------------------------------------------------------------------------}
{ VCL optimization                                                             }
{------------------------------------------------------------------------------}
  {$IFDEF WMPAINT_HOOK}
  {$IFNDEF COMPILER10_UP}
  CodeRedirect(GetDynamicMethod(TWinControl, WM_PAINT), @WinControlWMPaint);
  {$ENDIF ~COMPILER10_UP}
  {$ENDIF WMPAINT_HOOK}

end.

