{-----------------------------------------------------------------------------
 Unit Name: uSysUtils
 Author:    PyScripter
 Date:      06-Jul-2023
 Purpose:   Enhanced Execute from Jcl and other system utilities
            Includes much of the relevant code from JclSysUtils
            Enhamcements
            - Modify environment
            - Provide stdin
            - Truly RawOutput with buffer callbacks
 History:
-----------------------------------------------------------------------------}

unit uSysUtils;
{$I jcl.inc}

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  JclSynch;

type
  TTextHandler = procedure(const Text: string) of object;
  TProcessBuffer = procedure(const Bytes: TBytes; BytesRead: Cardinal) of object;
  TJclProcessPriority = (ppIdle, ppNormal, ppHigh, ppRealTime, ppBelowNormal, ppAboveNormal);

const
  ABORT_EXIT_CODE = ERROR_CANCELLED;

(* Terminates a process and all child processes *)
function TerminateProcessTree(ProcessID: DWORD): Boolean;

(* Raises a keyword interrupt in another process *)
procedure RaiseKeyboardInterrupt(ProcessId: DWORD);

(* Execute a Command using CreateProcess and captures output.
   Execute overloads as in Jcl *)
function Execute(const CommandLine: string; OutputLineCallback: TTextHandler; RawOutput: Boolean = False;
  AbortPtr: PBoolean = nil; ProcessPriority: TJclProcessPriority = ppNormal;
  AutoConvertOem: Boolean = False): Cardinal; overload;
function Execute(const CommandLine: string; AbortEvent: TJclEvent;
  OutputLineCallback: TTextHandler; RawOutput: Boolean = False; ProcessPriority: TJclProcessPriority = ppNormal;
  AutoConvertOem: Boolean = False): Cardinal; overload;
function Execute(const CommandLine: string; var Output: string; RawOutput: Boolean = False;
  AbortPtr: PBoolean = nil; ProcessPriority: TJclProcessPriority = ppNormal;
  AutoConvertOem: Boolean = False): Cardinal; overload;
function Execute(const CommandLine: string; AbortEvent: TJclEvent;
  var Output: string; RawOutput: Boolean = False; ProcessPriority: TJclProcessPriority = ppNormal;
  AutoConvertOem: Boolean = False): Cardinal; overload;

function Execute(const CommandLine: string; OutputLineCallback, ErrorLineCallback: TTextHandler;
  RawOutput: Boolean = False; RawError: Boolean = False; AbortPtr: PBoolean = nil;
  ProcessPriority: TJclProcessPriority = ppNormal; AutoConvertOem: Boolean = False): Cardinal; overload;
function Execute(const CommandLine: string; AbortEvent: TJclEvent;
  OutputLineCallback, ErrorLineCallback: TTextHandler; RawOutput: Boolean = False; RawError: Boolean = False;
  ProcessPriority: TJclProcessPriority = ppNormal; AutoConvertOem: Boolean = False): Cardinal; overload;
function Execute(const CommandLine: string; var Output, Error: string;
  RawOutput: Boolean = False; RawError: Boolean = False; AbortPtr: PBoolean = nil;
  ProcessPriority: TJclProcessPriority = ppNormal; AutoConvertOem: Boolean = False): Cardinal; overload;
function Execute(const CommandLine: string; AbortEvent: TJclEvent;
  var Output, Error: string; RawOutput: Boolean = False; RawError: Boolean = False;
  ProcessPriority: TJclProcessPriority = ppNormal; AutoConvertOem: Boolean = False): Cardinal; overload;

(* Oveloads with different defaults than Jcl (mainly RawOuput is True) *)
function ExecuteCmd(Command : string; out CmdOutput: string): Cardinal; overload;
(* Ignores Error output *)
function ExecuteCmd(Command : string; out CmdOutput, CmdError: string): Cardinal; overload;

type
  TJclExecuteCmdProcessOptionBeforeResumeEvent =
    procedure(const ProcessInfo: TProcessInformation; InWritePipe: PHandle) of object;
  TStartupVisibility = (svHide, svShow, svNotSet);

  TJclExecuteCmdProcessOptions = class(TObject)
  private
    FBufferSize: Cardinal;
    FCommandLine: string;
    FAbortPtr: PBoolean;
    FAbortEvent: TJclEvent;

    FOutputLineCallback: TTextHandler;
    FOutputBufferCallback: TProcessBuffer;
    FRawOutput: Boolean;
    FMergeError: Boolean;
    FErrorLineCallback: TTextHandler;
    FErrorBufferCallback: TProcessBuffer;
    FRawError: Boolean;
    FProcessPriority: TJclProcessPriority;

    FAutoConvertOem: Boolean;
    FCurrentDir: string;
    FEnvironment: TStringList;
    FCreateProcessFlags: DWORD;
    FStartupVisibility: TStartupVisibility;
    FBeforeResume: TJclExecuteCmdProcessOptionBeforeResumeEvent;

    FExitCode: Cardinal;
    FOutput: string;
    FError: string;
    function GetEnvironment: TStrings;
    procedure SetEnvironment(const Value: TStrings);
  public
    // in:
    property CommandLine: string read FCommandLine write FCommandLine;
    property AbortPtr: PBoolean read FAbortPtr write FAbortPtr;
    property AbortEvent: TJclEvent read FAbortEvent write FAbortEvent;

    property BufferSize: Cardinal read FBufferSize write FBufferSize;
    property OutputLineCallback: TTextHandler read FOutputLineCallback write FOutputLineCallback;
    property OutputBufferCallback: TProcessBuffer read FOutputBufferCallback write FOutputBufferCallback;
    property RawOutput: Boolean read FRawOutput write FRawOutput default False;
    property MergeError: Boolean read FMergeError write FMergeError default False;
    property ErrorLineCallback: TTextHandler read FErrorLineCallback write FErrorLineCallback;
    property ErrorBufferCallback: TProcessBuffer read FErrorBufferCallback write FErrorBufferCallback;
    property RawError: Boolean read FRawError write FRawError default False;
    property ProcessPriority: TJclProcessPriority read FProcessPriority write FProcessPriority default ppNormal;

    // AutoConvertOem assumes the process outputs OEM encoded strings and converts them to the
    // default string encoding.
    property AutoConvertOem: Boolean read FAutoConvertOem write FAutoConvertOem default True;
    property CurrentDir: string read FCurrentDir write FCurrentDir;
    property Environment: TStrings read GetEnvironment write SetEnvironment;
    property CreateProcessFlags: DWORD read FCreateProcessFlags write FCreateProcessFlags;
    property StartupVisibility: TStartupVisibility read FStartupVisibility write FStartupVisibility;
    property BeforeResume: TJclExecuteCmdProcessOptionBeforeResumeEvent read FBeforeResume write FBeforeResume;

    // out:
    property ExitCode: Cardinal read FExitCode;
    property Output: string read FOutput;
    property Error: string read FError;
  public
    constructor Create(const ACommandLine: string);
    destructor Destroy; override;
  end;

function ExecuteCmdProcess(Options: TJclExecuteCmdProcessOptions): Boolean;

implementation

uses
  Winapi.TlHelp32,
  JclStrings;

type
 TProcessArray = array of DWORD;

function TerminateProcessTree(ProcessID: DWORD): Boolean;

  function GetChildrenProcesses(const Process: DWORD; const IncludeParent: Boolean): TProcessArray;
  var
    Snapshot: Cardinal;
    ProcessList: PROCESSENTRY32;
    Current: Integer;
  begin
    Current := 0;
    SetLength(Result, 1);
    Result[0] := Process;
    repeat
      ProcessList.dwSize := SizeOf(PROCESSENTRY32);
      Snapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
      if (Snapshot = INVALID_HANDLE_VALUE) or not Process32First(Snapshot, ProcessList) then
        Continue;
      repeat
        if ProcessList.th32ParentProcessID = Result[Current] then
        begin
          SetLength(Result, Length(Result) + 1);
          Result[Length(Result) - 1] := ProcessList.th32ProcessID;
        end;
      until Process32Next(Snapshot, ProcessList) = False;
      Inc(Current);
    until Current >= Length(Result);
    if not IncludeParent then
      Result := Copy(Result, 2, Length(Result));
  end;

var
  Handle: THandle;
  List: TProcessArray;
  I: Integer;
begin
  Result := True;
  List := GetChildrenProcesses(ProcessID, True);
  for I := Length(List) - 1 downto 0 do
    if Result then
    begin
      Handle := OpenProcess(PROCESS_TERMINATE, false, List[I]);
      Result := (Handle <> 0) and
        TerminateProcess(Handle, Cardinal(ABORT_EXIT_CODE)) and
        CloseHandle(Handle);
    end;
end;

function CtrlHandler(fdwCtrlType : DWORD): LongBool; stdcall;
begin
  Result := True;
end;

procedure RaiseKeyboardInterrupt(ProcessId: DWORD);
begin
  Win32Check(AttachConsole(ProcessId));
  Win32Check(SetConsoleCtrlHandler(@CtrlHandler, True));
  try
    Win32Check(GenerateConsoleCtrlEvent(CTRL_C_EVENT, 0));
    Sleep(100);
  finally
    Win32Check(SetConsoleCtrlHandler(@CtrlHandler, False));
    Win32Check(FreeConsole);
  end;
end;

procedure SafeCloseHandle(var Handle: THandle);
begin
  if Handle <> 0 then
  begin
    CloseHandle(Handle);
    Handle := 0;
  end;
end;

// memory initialization
procedure ResetMemory(out P; Size: Longint);
begin
  if Size > 0 then
  begin
    Byte(P) := 0;
    FillChar(P, Size, 0);
  end;
end;

function Iff(const Condition: Boolean; const TruePart, FalsePart: Pointer): Pointer;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

//=== Child processes ========================================================
type
  TPipeInfo = record
    PipeRead, PipeWrite: THandle;
    Buffer: TBytes;
    Line: string;
    TextHandler: TTextHandler;
    BufferHandler: TProcessBuffer;
    RawOutput: Boolean;
    AutoConvertOem: Boolean;
    Event: TJclEvent;
  end;
  PPipeInfo = ^TPipeInfo;

// MuteCRTerminatedLines was "outsourced" from Win32ExecAndRedirectOutput

function InternalExecuteMuteCRTerminatedLines(const RawOutput: string): string;
const
  Delta = 1024;
var
  BufPos, OutPos, LfPos, EndPos: Integer;
  C: Char;
begin
  SetLength(Result, Length(RawOutput));
  OutPos := 1;
  LfPos := OutPos;
  EndPos := OutPos;
  for BufPos := 1 to Length(RawOutput) do
  begin
    if OutPos >= Length(Result)-2 then
      SetLength(Result, Length(Result) + Delta);
    C := RawOutput[BufPos];
    case C of
      NativeCarriageReturn:
        OutPos := LfPos;
      NativeLineFeed:
        begin
          OutPos := EndPos;
          Result[OutPos] := NativeCarriageReturn;
          Inc(OutPos);
          Result[OutPos] := C;
          Inc(OutPos);
          EndPos := OutPos;
          LfPos := OutPos;
        end;
    else
      Result[OutPos] := C;
      Inc(OutPos);
      EndPos := OutPos;
    end;
  end;
  SetLength(Result, OutPos - 1);
end;

procedure InternalExecuteProcessLine(const PipeInfo: TPipeInfo; LineEnd: Integer);
begin
  if PipeInfo.RawOutput or (PipeInfo.Line[LineEnd] <> NativeCarriageReturn) then
  begin
    while (LineEnd > 0) and CharIsReturn(PipeInfo.Line[LineEnd]) do
      Dec(LineEnd);
    PipeInfo.TextHandler(Copy(PipeInfo.Line, 1, LineEnd));
  end;
end;

procedure InternalExecuteProcessBuffer(var PipeInfo: TPipeInfo; PipeBytesRead: Cardinal);
var
  CR, LF: Integer;
  LineLen, Len: Integer;
  S: AnsiString;
begin
  if Assigned(PipeInfo.BufferHandler) then
  begin
    PipeInfo.BufferHandler(PipeInfo.Buffer, PipeBytesRead);
    Exit;
  end;
  if PipeInfo.AutoConvertOem then
  begin
    Len := MultiByteToWideChar(CP_OEMCP, 0, PAnsiChar(PipeInfo.Buffer), PipeBytesRead, nil, 0);
    LineLen := Length(PipeInfo.Line);
    // Convert directly into the PipeInfo.Line string
    SetLength(PipeInfo.Line, LineLen + Len);
    MultiByteToWideChar(CP_OEMCP, 0, PAnsiChar(PipeInfo.Buffer), PipeBytesRead, PChar(PipeInfo.Line) + LineLen, Len);
  end
  else
  begin
    SetString(S, PAnsiChar(PipeInfo.Buffer), PipeBytesRead); // interpret as ANSI
    PipeInfo.Line := PipeInfo.Line + string(S); // ANSI => UNICODE
  end;
  if Assigned(PipeInfo.TextHandler) then
    repeat
      CR := Pos(NativeCarriageReturn, PipeInfo.Line);
      if CR = Length(PipeInfo.Line) then
        CR := 0;        // line feed at CR + 1 might be missing
      LF := Pos(NativeLineFeed, PipeInfo.Line);
      if (CR > 0) and ((LF > CR + 1) or (LF = 0)) then
        LF := CR;       // accept CR as line end
      if LF > 0 then
      begin
        InternalExecuteProcessLine(PipeInfo, LF);
        Delete(PipeInfo.Line, 1, LF);
      end;
    until LF = 0;
end;

procedure InternalExecuteReadPipe(var PipeInfo: TPipeInfo; var Overlapped: TOverlapped);
var
  NullDWORD: ^DWORD; // XE4 broke PDWORD
  Res: DWORD;
begin
  NullDWORD := nil;
  if not ReadFile(PipeInfo.PipeRead, PipeInfo.Buffer[0], Length(PipeInfo.Buffer), NullDWORD^, @Overlapped) then
  begin
    Res := GetLastError;
    case Res of
      ERROR_BROKEN_PIPE:
        begin
          CloseHandle(PipeInfo.PipeRead);
          PipeInfo.PipeRead := 0;
        end;
      ERROR_IO_PENDING:
        ;
    else
      {$IFDEF DELPHI11_UP}
      RaiseLastOSError(Res);
      {$ELSE}
      RaiseLastOSError;
      {$ENDIF DELPHI11_UP}
    end;
  end;
end;

procedure InternalExecuteHandlePipeEvent(var PipeInfo: TPipeInfo; var Overlapped: TOverlapped);
var
  PipeBytesRead: DWORD;
begin
  if GetOverlappedResult(PipeInfo.PipeRead, Overlapped, PipeBytesRead, False) then
  begin
    InternalExecuteProcessBuffer(PipeInfo, PipeBytesRead);
    // automatically launch the next read
    InternalExecuteReadPipe(PipeInfo, Overlapped);
  end
  else
  if GetLastError = ERROR_BROKEN_PIPE then
  begin
    CloseHandle(PipeInfo.PipeRead);
    PipeInfo.PipeRead := 0;
  end
  else
    RaiseLastOSError;
end;

procedure InternalExecuteFlushPipe(var PipeInfo: TPipeInfo; var Overlapped: TOverlapped);
var
  PipeBytesRead: Cardinal;
begin
  CancelIo(PipeInfo.PipeRead);
  GetOverlappedResult(PipeInfo.PipeRead, Overlapped, PipeBytesRead, True);
  if PipeBytesRead > 0 then
    InternalExecuteProcessBuffer(PipeInfo, PipeBytesRead);
  while PeekNamedPipe(PipeInfo.PipeRead, nil, 0, nil, @PipeBytesRead, nil) and (PipeBytesRead > 0) do
  begin
    if PipeBytesRead > Cardinal(Length(PipeInfo.Buffer)) then
      PipeBytesRead := Length(PipeInfo.Buffer);
    if not ReadFile(PipeInfo.PipeRead, PipeInfo.Buffer[0], PipeBytesRead, PipeBytesRead, nil) then
      RaiseLastOSError;
    InternalExecuteProcessBuffer(PipeInfo, PipeBytesRead);
  end;
end;

var
  AsyncPipeCounter: Integer;

// CreateAsyncPipe creates a pipe that uses overlapped reading.
function CreateAsyncPipe(var hReadPipe, hWritePipe: THandle;
  lpPipeAttributes: PSecurityAttributes; nSize: DWORD): BOOL;
var
  PipeName: string;
  Error: DWORD;
  PipeReadHandle, PipeWriteHandle: THandle;
begin
  Result := False;

  if (@hReadPipe = nil) or (@hWritePipe = nil) then
  begin
    SetLastError(ERROR_INVALID_PARAMETER);
    Exit;
  end;

  if nSize = 0 then
    nSize := 4096;

  InterlockedIncrement(AsyncPipeCounter);
  // In some (not so) rare instances there is a race condition
  // where the counter is the same for two threads at the same
  // time. This makes the CreateNamedPipe call below fail
  // because of the limit set to 1 in the call.
  // So, to be sure this call succeeds, we put both the process
  // and thread id in the name of the pipe.
  // This was found to happen while simply starting 7 instances
  // of the same exe file in parallel.
  PipeName := Format('\\.\Pipe\AsyncAnonPipe.%.8x.%.8x.%.8x', [GetCurrentProcessId, GetCurrentThreadId, AsyncPipeCounter]);

  PipeReadHandle := CreateNamedPipe(PChar(PipeName), PIPE_ACCESS_INBOUND or FILE_FLAG_OVERLAPPED,
      PIPE_TYPE_BYTE or PIPE_WAIT, 1, nSize, nSize, 120 * 1000, lpPipeAttributes);
  if PipeReadHandle = INVALID_HANDLE_VALUE then
    Exit;

  PipeWriteHandle := CreateFile(PChar(PipeName), GENERIC_WRITE, 0, lpPipeAttributes, OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL {or FILE_FLAG_OVERLAPPED}, 0);
  if PipeWriteHandle = INVALID_HANDLE_VALUE then
  begin
    Error := GetLastError;
    CloseHandle(PipeReadHandle);
    SetLastError(Error);
    Exit;
  end;

  hReadPipe := PipeReadHandle;
  hWritePipe := PipeWriteHandle;

  Result := True;
end;

const
  BELOW_NORMAL_PRIORITY_CLASS = $00004000;
  ABOVE_NORMAL_PRIORITY_CLASS = $00008000;

  ProcessPriorities: array [TJclProcessPriority] of DWORD =
    (IDLE_PRIORITY_CLASS, NORMAL_PRIORITY_CLASS, HIGH_PRIORITY_CLASS, REALTIME_PRIORITY_CLASS,
     BELOW_NORMAL_PRIORITY_CLASS, ABOVE_NORMAL_PRIORITY_CLASS);

function ExecuteCmdProcess(Options: TJclExecuteCmdProcessOptions): Boolean;
var
  OutPipeInfo, ErrorPipeInfo: TPipeInfo;
  Index: Cardinal;
const
  StartupVisibilityFlags: array[TStartupVisibility] of DWORD = (SW_HIDE, SW_SHOW, SW_SHOWDEFAULT);
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  SecurityAttr: TSecurityAttributes;
  OutOverlapped, ErrorOverlapped: TOverlapped;
  ProcessEvent: TJclDispatcherObject;
  WaitEvents: array of TJclDispatcherObject;
  InternalAbort: Boolean;
  LastError: DWORD;
  CommandLine: string;
  CurDir: PChar;
  AbortPtr: PBoolean;
  Flags: DWORD;
  InReadPipe, InWritePipe, InputWriteTmp: THandle;
  EnvironmentData: PChar;
begin
  Result := False;

  // hack to pass a null reference to the parameter lpNumberOfBytesRead of ReadFile
  Options.FExitCode := $FFFFFFFF;

  SecurityAttr.nLength := SizeOf(SecurityAttr);
  SecurityAttr.lpSecurityDescriptor := nil;
  SecurityAttr.bInheritHandle := True;

  if not CreatePipe(InReadPipe, InputWriteTmp, @SecurityAttr, 0) then
  begin
    Options.FExitCode := GetLastError;
    Exit;
  end;
  if not  DuplicateHandle(GetCurrentProcess, InputWritetmp, GetCurrentProcess,
    @InWritePipe, 0, False, DUPLICATE_SAME_ACCESS or DUPLICATE_CLOSE_SOURCE) then
  begin
    SafeCloseHandle(InReadPipe);
    SafeCloseHandle(InputWriteTmp);
    Options.FExitCode := GetLastError;
    Exit;
  end;
  ResetMemory(OutPipeInfo, SizeOf(OutPipeInfo));
  OutPipeInfo.TextHandler := Options.OutputLineCallback;
  OutPipeInfo.BufferHandler := Options.OutputBufferCallback;
  OutPipeInfo.RawOutput := Options.RawOutput;
  OutPipeInfo.AutoConvertOem := Options.AutoConvertOem;
  SetLength(OutPipeInfo.Buffer, Options.BufferSize);
  if not CreateAsyncPipe(OutPipeInfo.PipeRead, OutPipeInfo.PipeWrite, @SecurityAttr, Options.BufferSize) then
  begin
    Options.FExitCode := GetLastError;
    SafeCloseHandle(InReadPipe);
    SafeCloseHandle(InWritePipe);
    Exit;
  end;
  OutPipeInfo.Event := TJclEvent.Create(@SecurityAttr, False {automatic reset}, False {not flagged}, '' {anonymous});
  ResetMemory(ErrorPipeInfo, SizeOf(ErrorPipeInfo));
  if not Options.MergeError then
  begin
    ErrorPipeInfo.TextHandler := Options.ErrorLineCallback;
    ErrorPipeInfo.BufferHandler := Options.ErrorBufferCallback;
    ErrorPipeInfo.RawOutput := Options.RawError;
    ErrorPipeInfo.AutoConvertOem := Options.AutoConvertOem;
    SetLength(ErrorPipeInfo.Buffer, Options.BufferSize);
    if not CreateAsyncPipe(ErrorPipeInfo.PipeRead, ErrorPipeInfo.PipeWrite, @SecurityAttr, Options.BufferSize) then
    begin
      Options.FExitCode := GetLastError;
      CloseHandle(InReadPipe);
      CloseHandle(InWritePipe);
      CloseHandle(OutPipeInfo.PipeWrite);
      CloseHandle(OutPipeInfo.PipeRead);
      OutPipeInfo.Event.Free;
      Exit;
    end;
    ErrorPipeInfo.Event := TJclEvent.Create(@SecurityAttr, False {automatic reset}, False {not flagged}, '' {anonymous});
  end;

  ResetMemory(StartupInfo, SizeOf(TStartupInfo));
  StartupInfo.cb := SizeOf(TStartupInfo);
  StartupInfo.dwFlags := STARTF_USESTDHANDLES;
  if Options.StartupVisibility <> svNotSet then
  begin
    StartupInfo.dwFlags := StartupInfo.dwFlags or STARTF_USESHOWWINDOW;
    StartupInfo.wShowWindow := StartupVisibilityFlags[Options.StartupVisibility];
  end;
  StartupInfo.hStdInput :=  InReadPipe;
  StartupInfo.hStdOutput := OutPipeInfo.PipeWrite;
  if Options.MergeError then
    StartupInfo.hStdError := OutPipeInfo.PipeWrite
  else
    StartupInfo.hStdError := ErrorPipeInfo.PipeWrite;
  CommandLine := Options.CommandLine;
  CurDir := Iff(Options.FCurrentDir = '', nil, PWideChar(Options.CurrentDir));
  UniqueString(CommandLine); // CommandLine must be in a writable memory block
  ResetMemory(ProcessInfo, SizeOf(ProcessInfo));
  ProcessEvent := nil;

  try
    Flags := Options.CreateProcessFlags and not (NORMAL_PRIORITY_CLASS or IDLE_PRIORITY_CLASS or
                                                 HIGH_PRIORITY_CLASS or REALTIME_PRIORITY_CLASS);
    Flags := Flags or ProcessPriorities[Options.ProcessPriority];
    if Assigned(Options.BeforeResume) then
      Flags := Flags or CREATE_SUSPENDED;

    if Options.Environment.Count = 0 then
      EnvironmentData := nil
    else
    begin
      StringsToMultiSz(EnvironmentData, Options.Environment);
      Flags := Flags or CREATE_UNICODE_ENVIRONMENT;
    end;

    if CreateProcess(nil, PChar(CommandLine), nil, nil, True, Flags,
      EnvironmentData, CurDir, StartupInfo, ProcessInfo) then
    begin
      Result := True;
      try
        try
          if Assigned(Options.BeforeResume) then
            Options.BeforeResume(ProcessInfo, @InWritePipe);
        finally
          if Flags and CREATE_SUSPENDED <> 0 then // CREATE_SUSPENDED may also have come from CreateProcessFlags
            ResumeThread(ProcessInfo.hThread);
        end;

        // init out and error events
        SafeCloseHandle(InReadPipe);
        SafeCloseHandle(OutPipeInfo.PipeWrite);
        if not Options.MergeError then
          SafeCloseHandle(ErrorPipeInfo.PipeWrite);
        InternalAbort := False;
        AbortPtr := Options.AbortPtr;
        if AbortPtr <> nil then
          AbortPtr^ := {$IFDEF FPC}Byte({$ENDIF}False{$IFDEF FPC}){$ENDIF}
        else
          AbortPtr := @InternalAbort;
        // init the array of events to wait for
        ProcessEvent := TJclDispatcherObject.Attach(ProcessInfo.hProcess);
        SetLength(WaitEvents, 2);
        // add the process first
        WaitEvents[0] := ProcessEvent;
        // add the output event
        WaitEvents[1] := OutPipeInfo.Event;
        // add the error event
        if not Options.MergeError then
        begin
          SetLength(WaitEvents, 3);
          WaitEvents[2] := ErrorPipeInfo.Event;
        end;
        // add the abort event if any
        if Options.AbortEvent <> nil then
        begin
          Options.AbortEvent.ResetEvent;
          Index := Length(WaitEvents);
          SetLength(WaitEvents, Index + 1);
          WaitEvents[Index] := Options.AbortEvent;
        end;
        // init the asynchronous reads
        ResetMemory(OutOverlapped, SizeOf(OutOverlapped));
        OutOverlapped.hEvent := OutPipeInfo.Event.Handle;
        InternalExecuteReadPipe(OutPipeInfo, OutOverlapped);
        if not Options.MergeError then
        begin
          ResetMemory(ErrorOverlapped, SizeOf(ErrorOverlapped));
          ErrorOverlapped.hEvent := ErrorPipeInfo.Event.Handle;
          InternalExecuteReadPipe(ErrorPipeInfo, ErrorOverlapped);
        end;
        // event based loop
        while not {$IFDEF FPC}Boolean({$ENDIF}AbortPtr^{$IFDEF FPC}){$ENDIF} do
        begin
          Index := WaitAlertableForMultipleObjects(WaitEvents, False, INFINITE);
          if Index = WAIT_OBJECT_0 then
            // the subprocess has ended
            Break
          else
          if Index = (WAIT_OBJECT_0 + 1) then
          begin
            // event on output
            InternalExecuteHandlePipeEvent(OutPipeInfo, OutOverlapped);
          end
          else
          if (Index = (WAIT_OBJECT_0 + 2)) and not Options.MergeError then
          begin
            // event on error
            InternalExecuteHandlePipeEvent(ErrorPipeInfo, ErrorOverlapped);
          end
          else
          if ((Index = (WAIT_OBJECT_0 + 2)) and Options.MergeError) or
             ((Index = (WAIT_OBJECT_0 + 3)) and not Options.MergeError) then
            // event on abort
            AbortPtr^ := {$IFDEF FPC}Byte({$ENDIF}True{$IFDEF FPC}){$ENDIF}
          else
            {$IFDEF DELPHI11_UP}
            RaiseLastOSError(Index);
            {$ELSE}
            RaiseLastOSError;
            {$ENDIF DELPHI11_UP}
        end;
        if {$IFDEF FPC}Boolean({$ENDIF}AbortPtr^{$IFDEF FPC}){$ENDIF} then
        begin
          //TerminateProcess(ProcessEvent.Handle, Cardinal(ABORT_EXIT_CODE));
          // Close handles first
          SafeCloseHandle(InWritePipe);
          SafeCloseHandle(OutPipeInfo.PipeRead);
          if not Options.MergeError then
            SafeCloseHandle(ErrorPipeInfo.PipeRead);
          //Forcefully terminate the process tree
          TerminateProcessTree(ProcessInfo.dwProcessId);
        end;

        if (ProcessEvent.WaitForever = {$IFDEF RTL280_UP}TJclWaitResult.{$ENDIF RTL280_UP}wrSignaled) and not GetExitCodeProcess(ProcessEvent.Handle, Options.FExitCode) then
          Options.FExitCode := $FFFFFFFF;
        SafeCloseHandle(ProcessInfo.hThread);
        if OutPipeInfo.PipeRead <> 0 then
          // read data remaining in output pipe
          InternalExecuteFlushPipe(OutPipeinfo, OutOverlapped);
        if not Options.MergeError and (ErrorPipeInfo.PipeRead <> 0) then
          // read data remaining in error pipe
          InternalExecuteFlushPipe(ErrorPipeInfo, ErrorOverlapped);
      except
        // always terminate process in case of an exception.
        // This is especially useful when an exception occurred in one of
        // the texthandler but only do it if the process actually started,
        // this prevents eating up the last error value by calling those
        // three functions with an invalid handle
        // Note that we don't do it in the finally block because these
        // calls would also then eat up the last error value which we tried
        // to avoid in the first place
        if ProcessInfo.hProcess <> 0 then
        begin
          TerminateProcess(ProcessInfo.hProcess, Cardinal(ABORT_EXIT_CODE));
          WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
          GetExitCodeProcess(ProcessInfo.hProcess, Options.FExitCode);
        end;

        raise;
      end;
    end;
  finally
    LastError := GetLastError;
    try
      SafeCloseHandle(InReadPipe);
      SafeCloseHandle(InWritePipe);
      SafeCloseHandle(OutPipeInfo.PipeRead);
      SafeCloseHandle(OutPipeInfo.PipeWrite);
      SafeCloseHandle(ErrorPipeInfo.PipeRead);
      SafeCloseHandle(ErrorPipeInfo.PipeWrite);
      SafeCloseHandle(ProcessInfo.hThread);

      if Assigned(ProcessEvent) then
        ProcessEvent.Free // this calls CloseHandle(ProcessInfo.hProcess)
      else if ProcessInfo.hProcess <> 0 then
        SafeCloseHandle(ProcessInfo.hProcess);
      OutPipeInfo.Event.Free;
      ErrorPipeInfo.Event.Free;
    finally
      SetLastError(LastError);
    end;
  end;
  if OutPipeInfo.Line <> '' then
    if Assigned(OutPipeInfo.TextHandler) then
      // output wasn't terminated by a line feed...
      // (shouldn't happen, but you never know)
      InternalExecuteProcessLine(OutPipeInfo, Length(OutPipeInfo.Line))
    else
      if Options.RawOutput then
        Options.FOutput := OutPipeInfo.Line
      else
        Options.FOutput := InternalExecuteMuteCRTerminatedLines(OutPipeInfo.Line);
  if ErrorPipeInfo.Line <> '' then
    if Assigned(ErrorPipeInfo.TextHandler) then
      // error wasn't terminated by a line feed...
      // (shouldn't happen, but you never know)
      InternalExecuteProcessLine(ErrorPipeInfo, Length(ErrorPipeInfo.Line))
    else
      if Options.RawError then
        Options.FError := ErrorPipeInfo.Line
      else
        Options.FError := InternalExecuteMuteCRTerminatedLines(ErrorPipeInfo.Line);
end;

function InternalExecute(CommandLine: string; AbortPtr: PBoolean; AbortEvent: TJclEvent;
  var Output: string; OutputLineCallback: TTextHandler; RawOutput: Boolean;
  MergeError: Boolean; var Error: string; ErrorLineCallback: TTextHandler; RawError: Boolean;
  ProcessPriority: TJclProcessPriority; AutoConvertOem: Boolean): Cardinal;
var
  Options: TJclExecuteCmdProcessOptions;
begin
  Options := TJclExecuteCmdProcessOptions.Create(CommandLine);
  try
    Options.AutoConvertOem := AutoConvertOem;

    Options.AbortPtr := AbortPtr;
    Options.AbortEvent := AbortEvent;
    Options.OutputLineCallback := OutputLineCallback;
    Options.RawOutput := RawOutput;
    Options.MergeError := MergeError;
    Options.ErrorLineCallback := ErrorLineCallback;
    Options.RawError := RawError;
    Options.ProcessPriority := ProcessPriority;

    ExecuteCmdProcess(Options);

    Result := Options.ExitCode;

    // Append => backward compatiblity
    Output := Output + Options.Output;
    Error := Error + Options.Error;
  finally
    Options.Free;
  end;
end;

{ TODO -cHelp :
RawOutput: Do not process isolated carriage returns (#13).
That is, for RawOutput = False, lines not terminated by a line feed (#10) are deleted from Output. }

function Execute(const CommandLine: string; var Output: string; RawOutput: Boolean;
  AbortPtr: PBoolean; ProcessPriority: TJclProcessPriority; AutoConvertOem: Boolean): Cardinal;
var
  Error: string;
begin
  Error := '';
  Result := InternalExecute(CommandLine, AbortPtr, nil, Output, nil, RawOutput, True, Error,
    nil, False, ProcessPriority, AutoConvertOem);
end;

function Execute(const CommandLine: string; AbortEvent: TJclEvent; var Output: string; RawOutput: Boolean;
  ProcessPriority: TJclProcessPriority; AutoConvertOem: Boolean): Cardinal;
var
  Error: string;
begin
  Error := '';
  Result := InternalExecute(CommandLine, nil, AbortEvent, Output, nil, RawOutput, True, Error,
    nil, False, ProcessPriority, AutoConvertOem);
end;

function Execute(const CommandLine: string; OutputLineCallback: TTextHandler; RawOutput: Boolean;
  AbortPtr: PBoolean; ProcessPriority: TJclProcessPriority; AutoConvertOem: Boolean): Cardinal;
var
  Output, Error: string;
begin
  Output := '';
  Error := '';
  Result := InternalExecute(CommandLine, AbortPtr, nil, Output, OutputLineCallback, RawOutput, True, Error,
    nil, False, ProcessPriority, AutoConvertOem);
end;

function Execute(const CommandLine: string; AbortEvent: TJclEvent; OutputLineCallback: TTextHandler; RawOutput: Boolean;
  ProcessPriority: TJclProcessPriority; AutoConvertOem: Boolean): Cardinal;
var
  Output, Error: string;
begin
  Output := '';
  Error := '';
  Result := InternalExecute(CommandLine, nil, AbortEvent, Output, OutputLineCallback, RawOutput, True, Error,
    nil, False, ProcessPriority, AutoConvertOem);
end;

function Execute(const CommandLine: string; var Output, Error: string; RawOutput, RawError: Boolean;
  AbortPtr: PBoolean; ProcessPriority: TJclProcessPriority; AutoConvertOem: Boolean): Cardinal;
begin
  Result := InternalExecute(CommandLine, AbortPtr, nil, Output, nil, RawOutput, False, Error,
    nil, RawError, ProcessPriority, AutoConvertOem);
end;

function Execute(const CommandLine: string; AbortEvent: TJclEvent; var Output, Error: string;
  RawOutput, RawError: Boolean; ProcessPriority: TJclProcessPriority; AutoConvertOem: Boolean): Cardinal;
begin
  Result := InternalExecute(CommandLine, nil, AbortEvent, Output, nil, RawOutput, False, Error,
    nil, RawError, ProcessPriority, AutoConvertOem);
end;

function Execute(const CommandLine: string; OutputLineCallback, ErrorLineCallback: TTextHandler;
  RawOutput, RawError: Boolean; AbortPtr: PBoolean; ProcessPriority: TJclProcessPriority; AutoConvertOem: Boolean): Cardinal;
var
  Output, Error: string;
begin
  Output := '';
  Error := '';
  Result := InternalExecute(CommandLine, AbortPtr, nil, Output, OutputLineCallback, RawOutput, False, Error,
    ErrorLineCallback, RawError, ProcessPriority, AutoConvertOem);
end;

function Execute(const CommandLine: string; AbortEvent: TJclEvent; OutputLineCallback, ErrorLineCallback: TTextHandler;
  RawOutput, RawError: Boolean; ProcessPriority: TJclProcessPriority; AutoConvertOem: Boolean): Cardinal;
var
  Output, Error: string;
begin
  Output := '';
  Error := '';
  Result := InternalExecute(CommandLine, nil, AbortEvent, Output, OutputLineCallback, RawOutput, False, Error,
    ErrorLineCallback, RawError, ProcessPriority, AutoConvertOem);
end;


function ExecuteCmd(Command : string; out CmdOutput, CmdError: string): Cardinal; overload;
Var
  ProcessOptions : TJclExecuteCmdProcessOptions;
begin
  ProcessOptions := TJclExecuteCmdProcessOptions.Create(Command);
  try
    ProcessOptions.MergeError := False;
    ProcessOptions.RawOutput := True;
    ProcessOptions.RawError := True;
    ProcessOptions.AutoConvertOEM := False;
    ProcessOptions.CreateProcessFlags :=
      ProcessOptions.CreateProcessFlags or
       CREATE_UNICODE_ENVIRONMENT or CREATE_NEW_CONSOLE;
    ExecuteCmdProcess(ProcessOptions);
    Result := ProcessOptions.ExitCode;
    CmdOutput := ProcessOptions.Output;
    CmdError := ProcessOptions.Error;
  finally
    ProcessOptions.Free;
  end;
end;

function ExecuteCmd(Command : string; out CmdOutput: string): Cardinal; overload;
Var
  CmdError: string;
begin
  Result := ExecuteCmd(Command, CmdOutput, CmdError);
end;

{ TJclExecuteCmdProcessOptions }

constructor TJclExecuteCmdProcessOptions.Create(const ACommandLine: string);
begin
  inherited Create;
  FCommandLine := ACommandLine;
  FAutoConvertOem := True;
  FProcessPriority := ppNormal;
  FBufferSize := 4096;
  FEnvironment := TStringList.Create;
end;

destructor TJclExecuteCmdProcessOptions.Destroy;
begin
  FEnvironment.Free;
  inherited;
end;

function TJclExecuteCmdProcessOptions.GetEnvironment: TStrings;
begin
  Result := FEnvironment;
end;

procedure TJclExecuteCmdProcessOptions.SetEnvironment(const Value: TStrings);
begin
  FEnvironment.Assign(Value);
end;

end.
