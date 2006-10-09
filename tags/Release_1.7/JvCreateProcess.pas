{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSysComp.PAS, released Dec 26, 1999.

The Initial Developer of the Original Code is Petr Vones (petr dott v att mujmail dott cz)
Portions created by Petr Vones are Copyright (C) 1999 Petr Vones.
Portions created by Microsoft are Copyright (C) 1998, 1999 Microsoft Corp.
All Rights Reserved.

Contributor(s):
  Marcel van Brakel <brakelm att bart dott nl>.
  Remko Bonte <remkobonte att myrealbox dott com> (redirect console output)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvCreateProcess;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, SysUtils, Classes,
  {$IFDEF COMPILER5}
  Forms,
  {$ENDIF COMPILER5}
  ShellAPI, SyncObjs,
  JvComponentBase, JvTypes;

const
  CCPS_BufferSize = 1024;
  CCPS_MaxBufferSize = 65536;

type
  EJvProcessError = EJVCLException;

  TJvProcessPriority = (ppIdle, ppNormal, ppHigh, ppRealTime);

  TJvConsoleOption = (coOwnerData, coRedirect, coSeparateError);
  TJvConsoleOptions = set of TJvConsoleOption;

  TJvCPSRawReadEvent = procedure(Sender: TObject; const S: string) of object;
  TJvCPSReadEvent = procedure(Sender: TObject; const S: string; const StartsOnNewLine: Boolean) of object;
  TJvCPSTerminateEvent = procedure(Sender: TObject; ExitCode: DWORD) of object;

  TJvProcessEntry = class(TObject)
  private
    FFileName: TFileName;
    FProcessID: DWORD;
    FProcessName: string;
    function GetSystemIconIndex(IconType: Integer): Integer;
    function GetPriority: TJvProcessPriority;
    procedure SetPriority(const Value: TJvProcessPriority);
  public
    constructor Create(AProcessID: DWORD; const AFileName: TFileName; const AProcessName: string);
    function Close(UseQuit: Boolean = False): Boolean;
    class function PriorityText(Priority: TJvProcessPriority): string;
    function Terminate: Boolean;
    property FileName: TFileName read FFileName;
    property LargeIconIndex: Integer index SHGFI_LARGEICON read GetSystemIconIndex;
    property Priority: TJvProcessPriority read GetPriority write SetPriority;
    property ProcessID: DWORD read FProcessID;
    property ProcessName: string read FProcessName;
    property SmallIconIndex: Integer index SHGFI_SMALLICON read GetSystemIconIndex;
  end;

  TJvCPSBuffer = array [0..CCPS_BufferSize - 1] of Char;
  TJvCPSState = (psReady, psRunning, psWaiting);
  TJvCPSFlag = (cfDefaultErrorMode, cfNewConsole, cfNewProcGroup, cfSeparateWdm,
    cfSharedWdm, cfSuspended, cfUnicode, cfDetached);
  TJvCPSFlags = set of TJvCPSFlag;
  TJvCPSShowWindow = (swHide, swMinimize, swMaximize, swNormal);

  TJvCPSStartupInfo = class(TPersistent)
  private
    FDesktop: string;
    FTitle: string;
    FDefaultPosition: Boolean;
    FDefaultWindowState: Boolean;
    FDefaultSize: Boolean;
    FHeight: Integer;
    FLeft: Integer;
    FWidth: Integer;
    FShowWindow: TJvCPSShowWindow;
    FTop: Integer;
    FForceOnFeedback: Boolean;
    FForceOffFeedback: Boolean;
    function GetStartupInfo: TStartupInfo;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    property StartupInfo: TStartupInfo read GetStartupInfo;
  published
    property Desktop: string read FDesktop write FDesktop;
    property Title: string read FTitle write FTitle;
    property Left: Integer read FLeft write FLeft default 0;
    property Top: Integer read FTop write FTop default 0;
    property DefaultPosition: Boolean read FDefaultPosition write FDefaultPosition default True;
    property Width: Integer read FWidth write FWidth default 0;
    property Height: Integer read FHeight write FHeight default 0;
    property DefaultSize: Boolean read FDefaultSize write FDefaultSize default True;
    property ShowWindow: TJvCPSShowWindow read FShowWindow write FShowWindow default swNormal;
    property DefaultWindowState: Boolean read FDefaultWindowState write FDefaultWindowState default True;
    property ForceOnFeedback: Boolean read FForceOnFeedback write FForceOnFeedback default False;
    property ForceOffFeedback: Boolean read FForceOffFeedback write FForceOffFeedback default False;
  end;

  TJvCreateProcess = class;

  TJvBaseReader = class(TObject)
  private
    FCreateProcess: TJvCreateProcess;
    FConsoleOutput: TStringList;
    FOnRawRead: TJvCPSRawReadEvent;
    FOnRead: TJvCPSReadEvent;
    function GetConsoleOutput: TStrings;
  public
    constructor Create(ACreateProcess: TJvCreateProcess); virtual;
    destructor Destroy; override;

    property ConsoleOutput: TStrings read GetConsoleOutput;
  published
    property OnRead: TJvCPSReadEvent read FOnRead write FOnRead;
    property OnRawRead: TJvCPSRawReadEvent read FOnRawRead write FOnRawRead;
  end;

  TJvCreateProcess = class(TJvComponent)
  private
    FApplicationName: string;
    FCommandLine: string;
    FCreationFlags: TJvCPSFlags;
    FCurrentDirectory: string;
    FEnvironment: TStringList;
    FState: TJvCPSState;
    FStartupInfo: TJvCPSStartupInfo;
    FPriority: TJvProcessPriority;
    FProcessInfo: TProcessInformation;
    FWaitForTerminate: Boolean;
    FConsoleOptions: TJvConsoleOptions;
    FOnTerminate: TJvCPSTerminateEvent;
    FWaitThread: TThread;
    FInputReader: TJvBaseReader;
    FErrorReader: TJvBaseReader;
    FHandle: THandle;
    FExitCode: Cardinal;
    FRunningThreadCount: Integer;
    function GetConsoleOutput: TStrings;
    function GetEnvironment: TStrings;
    procedure SetWaitForTerminate(const Value: Boolean);
    procedure WaitThreadTerminated(Sender: TObject);
    procedure SetEnvironment(const Value: TStrings);
    function GetHandle: THandle;

    function GetOnErrorRawRead: TJvCPSRawReadEvent;
    function GetOnErrorRead: TJvCPSReadEvent;
    function GetOnRawRead: TJvCPSRawReadEvent;
    function GetOnRead: TJvCPSReadEvent;
    procedure SetOnErrorRawRead(const Value: TJvCPSRawReadEvent);
    procedure SetOnErrorRead(const Value: TJvCPSReadEvent);
    procedure SetOnRawRead(const Value: TJvCPSRawReadEvent);
    procedure SetOnRead(const Value: TJvCPSReadEvent);
    procedure SetStartupInfo(Value: TJvCPSStartupInfo);

    procedure GotoReadyState;
    procedure GotoWaitState(const AThreadCount: Integer);
    procedure GotoRunningState;
  protected
    procedure CheckReady;
    procedure CheckRunning;
    procedure CheckNotWaiting;
    procedure CloseProcessHandles;
    procedure TerminateWaitThread;
    procedure HandleReadEvent(Sender: TObject);
    procedure HandleThreadTerminated;
    procedure WndProc(var Msg: TMessage);
    property Handle: THandle read GetHandle;
    procedure CloseRead;
    procedure CloseWrite;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CloseApplication(SendQuit: Boolean = False): Boolean;
    procedure Run;
    procedure StopWaiting;
    procedure Terminate;
    function Write(const S: string): Boolean;
    function WriteLn(const S: string): Boolean;
    property ProcessInfo: TProcessInformation read FProcessInfo;
    property State: TJvCPSState read FState;
    property ConsoleOutput: TStrings read GetConsoleOutput;
    property InputReader: TJvBaseReader read FInputReader;
    property ErrorReader: TJvBaseReader read FErrorReader;
  published
    property ApplicationName: string read FApplicationName write FApplicationName;
    property CommandLine: string read FCommandLine write FCommandLine;
    property CreationFlags: TJvCPSFlags read FCreationFlags write FCreationFlags default [];
    property CurrentDirectory: string read FCurrentDirectory write FCurrentDirectory;
    property Environment: TStrings read GetEnvironment write SetEnvironment;
    property Priority: TJvProcessPriority read FPriority write FPriority default ppNormal;
    property StartupInfo: TJvCPSStartupInfo read FStartupInfo write SetStartupInfo;
    property WaitForTerminate: Boolean read FWaitForTerminate write SetWaitForTerminate default True;
    property ConsoleOptions: TJvConsoleOptions read FConsoleOptions write FConsoleOptions default [coOwnerData];
    property OnTerminate: TJvCPSTerminateEvent read FOnTerminate write FOnTerminate;
    property OnRead: TJvCPSReadEvent read GetOnRead write SetOnRead;
    property OnRawRead: TJvCPSRawReadEvent read GetOnRawRead write SetOnRawRead;
    property OnErrorRead: TJvCPSReadEvent read GetOnErrorRead write SetOnErrorRead;
    property OnErrorRawRead: TJvCPSRawReadEvent read GetOnErrorRawRead write SetOnErrorRawRead;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  Math,
  JclStrings,
  JvVCL5Utils, JvJCLUtils, JvJVCLUtils, JvConsts, JvResources;

const
  CM_READ = WM_USER + 1;
  CM_THREADTERMINATED = WM_USER + 2;

  //MaxProcessCount = 4096;
  ProcessPriorities: array [TJvProcessPriority] of DWORD =
    (IDLE_PRIORITY_CLASS, NORMAL_PRIORITY_CLASS, HIGH_PRIORITY_CLASS, REALTIME_PRIORITY_CLASS);

type
  { Threads which monitor the created process }

  TJvWaitForProcessThread = class(TThread)
  private
    FExitCode: DWORD;
    FCloseEvent: THandle;
    FProcessHandle: THandle;
  protected
    procedure Execute; override;
  public
    constructor Create(ProcessHandle: DWORD);
    destructor Destroy; override;
    procedure TerminateThread;
  end;

  TJvConsoleThread = class(TJvWaitForProcessThread)
  private
    // Write end of the pipe
    FWriteHandle: THandle;
    FWriteEvent: THandle;
    // Critical sections to synchronize access to the buffers
    FWriteLock: TCriticalSection;
    // Fixed size buffer; maybe change to sizeable
    FOutputBuffer: TJvCPSBuffer;
    FOutputBufferEnd: Cardinal;
  protected
    procedure Execute; override;
    function TryWrite: Boolean;
  public
    constructor Create(ProcessHandle: DWORD; AWriteHandle: THandle);
    destructor Destroy; override;
    function Write(const S: string): Boolean;
    procedure CloseWrite;
  end;

  TJvReadThread = class(TThread)
  private
    FOwner: TObject;
    // Read end of the pipe
    FReadHandle: THandle;
    // Critical sections to synchronize access to the buffers
    FReadLock: TCriticalSection;
    // Handle to the TJvCreateProcess
    FDestHandle: THandle;
    FPreBuffer: PChar;
    FInputBuffer: PChar;
    FInputBufferSize: Cardinal;
    FInputBufferEnd: Cardinal;
  protected
    procedure CopyToBuffer(Buffer: PChar; ASize: Cardinal);
    procedure Execute; override;
  public
    constructor Create(AOwner: TObject; AReadHandle, ADestHandle: THandle);
    destructor Destroy; override;
    procedure CloseRead;
    function ReadBuffer(var ABuffer: TJvCPSBuffer; out ABufferSize: Cardinal): Boolean;
    procedure TerminateThread;
  end;

  TJvReader = class(TJvBaseReader)
  private
    FThread: TJvReadThread;
    FCurrentLine: string; // Last output of the console with no #10 char.
    FCursorPosition: Integer; // Position of the cursor on FCurrentLine
    FStartsOnNewLine: Boolean;
    FParseBuffer: TJvCPSBuffer;
    procedure ThreadTerminated(Sender: TObject);
  protected
    procedure DoReadEvent(const EndsWithNewLine: Boolean);
    procedure DoRawReadEvent(Data: PChar; const ASize: Cardinal);
    procedure ParseConsoleOutput(Data: PChar; ASize: Cardinal);
    procedure HandleReadEvent;
  public
    procedure CreateThread(const AReadHandle: THandle);
    procedure CloseRead;
    procedure Terminate;
  end;

//=== Local procedures =======================================================

var
  GWinSrvHandle: HMODULE;
  GTriedLoadWinSrvDll: Boolean;

const
  WinSrvDllName = 'WINSRV.DLL';

function WinSrvHandle: HMODULE;
begin
  if not GTriedLoadWinSrvDll then
  begin
    GTriedLoadWinSrvDll := True;

    GWinSrvHandle := SafeLoadLibrary(WinSrvDllName);
    if GWinSrvHandle <> 0 then
      FreeLibrary(GWinSrvHandle);
  end;
  Result := GWinSrvHandle;
end;

function IsConsoleWindow(AHandle: THandle): Boolean;
begin
  Result := LongWord(GetWindowLong(AHandle, GWL_HINSTANCE)) = WinSrvHandle;
end;

function InternalCloseApp(ProcessID: DWORD; UseQuit: Boolean): Boolean;
type
  PEnumWinRec = ^TEnumWinRec;
  TEnumWinRec = record
    ProcessID: DWORD;
    PostQuit: Boolean;
    FoundWin: Boolean;
  end;
var
  EnumWinRec: TEnumWinRec;

  function EnumWinProc(Wnd: HWND; Param: PEnumWinRec): BOOL; stdcall;
  var
    PID, TID: DWORD;
  begin
    TID := GetWindowThreadProcessId(Wnd, @PID);
    if PID = Param.ProcessID then
    begin
      if Param.PostQuit then
        PostThreadMessage(TID, WM_QUIT, 0, 0)
      else
      if IsWindowVisible(Wnd) or IsConsoleWindow(Wnd) then
        PostMessage(Wnd, WM_CLOSE, 0, 0);
      Param.FoundWin := True;
    end;
    Result := True;
  end;

begin
  EnumWinRec.ProcessID := ProcessID;
  EnumWinRec.PostQuit := UseQuit;
  EnumWinRec.FoundWin := False;
  EnumWindows(@EnumWinProc, Integer(@EnumWinRec));
  Result := EnumWinRec.FoundWin;
end;

function InternalTerminateProcess(ProcessID: DWORD): Boolean;
var
  ProcessHandle: THandle;
begin
  ProcessHandle := OpenProcess(PROCESS_TERMINATE, False, ProcessID);
  OSCheck(ProcessHandle <> 0);
  Result := TerminateProcess(ProcessHandle, 0);
  CloseHandle(ProcessHandle);
end;

function SafeCloseHandle(var H: THandle): Boolean;
begin
  if H <> 0 then
  begin
    Result := CloseHandle(H);
    if Result then
      H := 0;
  end
  else
    Result := True;
end;

type
  { TJvRWEHandles: A simple class that maintains at most 3 handles. When the
    class is destroyed, the handles it maintains are closed. By calling Extract..
    you remove the handle from the class so you have to close it yourself.
    Assumed is that the 3 handles are not the same or 0.
  }

  TJvRWEHandles = class(TObject)
  private
    FHandle: array [0..2] of THandle;
    function GetHandle(const Index: Integer): THandle;
    procedure SetHandle(const Index: Integer; const Value: THandle);
    function ExtractHandle(const Index: Integer): THandle;
  public
    destructor Destroy; override;
    procedure Clear;

    property ExtractRead: THandle index 0 read ExtractHandle;
    property ExtractWrite: THandle index 1 read ExtractHandle;
    property ExtractError: THandle index 2 read ExtractHandle;

    property Read: THandle index 0 read GetHandle write SetHandle;
    property Write: THandle index 1 read GetHandle write SetHandle;
    property Error: THandle index 2 read GetHandle write SetHandle;
  end;

  TCreateDuplicateKind = (cdkInheritable_KeepSourceOpen, cdkNotInheritable_CloseSource);

function CreateDuplicate(AHandle: THandle; const Kind: TCreateDuplicateKind): THandle;
const
  cCloseAction: array [TCreateDuplicateKind] of DWORD = (0, DUPLICATE_CLOSE_SOURCE);
begin
  OSCheck(DuplicateHandle(GetCurrentProcess, AHandle,
    GetCurrentProcess,
    @Result, // Address of new handle.
    0, Kind = cdkInheritable_KeepSourceOpen,
    DUPLICATE_SAME_ACCESS or cCloseAction[Kind]));
end;

procedure ConstructPipe(LocalHandles, ConsoleHandles: TJvRWEHandles; const SeparateError: Boolean);
var
  Sa: TSecurityAttributes;
  Sd: TSecurityDescriptor;
  ReadHandle, WriteHandle: THandle;
begin
  { http://support.microsoft.com/default.aspx?scid=KB;EN-US;q190351& }
  { http://community.borland.com/article/0,1410,10387,00.html }

  // Set up the security attributes struct.
  FillChar(Sa, SizeOf(TSecurityAttributes), 0);
  Sa.nLength := SizeOf(TSecurityAttributes);

  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    // Initialize security descriptor (Windows NT)
    InitializeSecurityDescriptor(@Sd, SECURITY_DESCRIPTOR_REVISION);
    SetSecurityDescriptorDacl(@Sd, True, nil, False);
    Sa.lpSecurityDescriptor := @Sd;
  end
  else
    Sa.lpSecurityDescriptor := nil;
  Sa.bInheritHandle := True;

  if ConsoleHandles.Write = 0 then
  begin
    // Create the child output pipe.
    OSCheck(CreatePipe(ReadHandle, WriteHandle, @Sa, 0));

    // Create new output read handle. Set bInheritHandle to False. Otherwise,
    // the child inherits the properties and, as a result, non-closeable handles
    // to the pipes are created (cdkNOTINHERITABLE_CloseSource)

    // Close inheritable copies of the handles you do not want to be
    // inherited (cdkNotInheritable_CLOSESOURCE)

    // CreateDuplicate may raise an exception, so call it last otherwise WriteHandle
    // is not closed.
    ConsoleHandles.Write := WriteHandle;
    LocalHandles.Read := CreateDuplicate(ReadHandle, cdkNotInheritable_CloseSource);
  end;

  if ConsoleHandles.Error = 0 then
  begin
    if SeparateError then
    begin
      // Create the child input pipe.
      OSCheck(CreatePipe(ReadHandle, WriteHandle, @Sa, 0));

      ConsoleHandles.Error := WriteHandle;
      LocalHandles.Error := CreateDuplicate(ReadHandle, cdkNotInheritable_CloseSource);
    end
    else
    begin
      // Create a duplicate of the output write handle for the std error
      // write handle. This is necessary in case the child application
      // closes one of its std output handles.
      ConsoleHandles.Error := CreateDuplicate(ConsoleHandles.Write, cdkInheritable_KeepSourceOpen);
    end;
  end;

  if ConsoleHandles.Read = 0 then
  begin
    // Create the child input pipe.
    OSCheck(CreatePipe(ReadHandle, WriteHandle, @Sa, 0));

    ConsoleHandles.Read := ReadHandle;
    LocalHandles.Write := CreateDuplicate(WriteHandle, cdkNotInheritable_CloseSource);
  end;
end;

//=== { TJvBaseReader } ======================================================

constructor TJvBaseReader.Create(ACreateProcess: TJvCreateProcess);
begin
  inherited Create;
  FCreateProcess := ACreateProcess;
  FConsoleOutput := TStringList.Create;
end;

destructor TJvBaseReader.Destroy;
begin
  FConsoleOutput.Free;
  inherited Destroy;
end;

function TJvBaseReader.GetConsoleOutput: TStrings;
begin
  Result := FConsoleOutput;
end;

//=== { TJvProcessEntry } ====================================================

constructor TJvProcessEntry.Create(AProcessID: DWORD;
  const AFileName: TFileName; const AProcessName: string);
begin
  inherited Create;
  FFileName := AFileName;
  FProcessID := AProcessID;
  FProcessName := AProcessName;
end;

function TJvProcessEntry.Close(UseQuit: Boolean): Boolean;
begin
  Result := InternalCloseApp(ProcessID, UseQuit);
end;

function TJvProcessEntry.GetPriority: TJvProcessPriority;
var
  ProcessHandle: THandle;
  PriorityClass: DWORD;
begin
  if ProcessID = 0 then
    Result := ppNormal
  else
  begin
    ProcessHandle := OpenProcess(PROCESS_ALL_ACCESS, False, ProcessID);
    OSCheck(ProcessHandle <> 0);
    try
      PriorityClass := GetPriorityClass(ProcessHandle);
      OSCheck(PriorityClass <> 0);
      case PriorityClass of
        NORMAL_PRIORITY_CLASS:
          Result := ppNormal;
        IDLE_PRIORITY_CLASS:
          Result := ppIdle;
        HIGH_PRIORITY_CLASS:
          Result := ppHigh;
        REALTIME_PRIORITY_CLASS:
          Result := ppRealTime;
      else
        Result := ppNormal;
      end;
    finally
      CloseHandle(ProcessHandle);
    end;
  end;
end;

function TJvProcessEntry.GetSystemIconIndex(IconType: Integer): Integer;
var
  FileInfo: TSHFileInfo;
begin
  FillChar(FileInfo, SizeOf(FileInfo), #0);
  SHGetFileInfo(PChar(FileName), 0, FileInfo, SizeOf(FileInfo),
    SHGFI_SYSICONINDEX or IconType);
  Result := FileInfo.iIcon;
end;

class function TJvProcessEntry.PriorityText(Priority: TJvProcessPriority): string;
begin
  case Priority of
    ppIdle:
      Result := RsIdle;
    ppNormal:
      Result := RsNormal;
    ppHigh:
      Result := RsHigh;
    ppRealTime:
      Result := RsRealTime;
  end;
end;

procedure TJvProcessEntry.SetPriority(const Value: TJvProcessPriority);
var
  ProcessHandle: THandle;
begin
  ProcessHandle := OpenProcess(PROCESS_SET_INFORMATION, False, ProcessID);
  OSCheck(ProcessHandle <> 0);
  try
    OSCheck(SetPriorityClass(ProcessHandle, ProcessPriorities[Value]));
  finally
    CloseHandle(ProcessHandle);
  end;
end;

function TJvProcessEntry.Terminate: Boolean;
begin
  Result := InternalTerminateProcess(FProcessID);
end;

//=== { TJvCPSStartupInfo } ==================================================

constructor TJvCPSStartupInfo.Create;
begin
  inherited Create;
  FDefaultSize := True;
  FDefaultPosition := True;
  FDefaultWindowState := True;
  FShowWindow := swNormal;
end;

procedure TJvCPSStartupInfo.AssignTo(Dest: TPersistent);
begin
  if Dest is TJvCPSStartupInfo then
    with TJvCPSStartupInfo(Dest) do
    begin
      FDesktop := Self.FDesktop;
      FTitle := Self.FTitle;
      FLeft := Self.FLeft;
      FTop := Self.FTop;
      FDefaultPosition := Self.FDefaultPosition;
      FWidth := Self.FWidth;
      FHeight := Self.FHeight;
      FDefaultSize := Self.FDefaultSize;
      FShowWindow := Self.FShowWindow;
      FDefaultWindowState := Self.FDefaultWindowState;
      FForceOnFeedback := Self.FForceOnFeedback;
      FForceOffFeedback := Self.FForceOffFeedback;
    end
  else
    inherited AssignTo(Dest);
end;

function TJvCPSStartupInfo.GetStartupInfo: TStartupInfo;
const
  ShowWindowValues: array [TJvCPSShowWindow] of DWORD =
    (SW_HIDE, SW_SHOWMINIMIZED, SW_SHOWMAXIMIZED, SW_SHOWNORMAL);
begin
  FillChar(Result, SizeOf(TStartupInfo), #0);
  with Result do
  begin
    cb := SizeOf(TStartupInfo);
    if Length(FDesktop) > 0 then
      lpDesktop := PChar(FDesktop);
    if Length(FTitle) > 0 then
      lpTitle := PChar(Title);
    if not FDefaultPosition then
    begin
      dwX := FLeft;
      dwY := FTop;
      Inc(dwFlags, STARTF_USEPOSITION);
    end;
    if not FDefaultSize then
    begin
      dwXSize := FWidth;
      dwYSize := FHeight;
      Inc(dwFlags, STARTF_USESIZE);
    end;
    if not FDefaultWindowState then
    begin
      wShowWindow := ShowWindowValues[FShowWindow];
      Inc(dwFlags, STARTF_USESHOWWINDOW);
    end;
    if FForceOnFeedback then
      Inc(dwFlags, STARTF_FORCEONFEEDBACK);
    if FForceOffFeedback then
      Inc(dwFlags, STARTF_FORCEOFFFEEDBACK);
  end;
end;

//=== { TJvWaitForProcessThread } ============================================

constructor TJvWaitForProcessThread.Create(ProcessHandle: DWORD);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  Priority := tpLower;
  FCloseEvent := CreateEvent(nil, True, False, nil);
  FProcessHandle := ProcessHandle;
end;

destructor TJvWaitForProcessThread.Destroy;
begin
  SafeCloseHandle(FCloseEvent);
  inherited Destroy;
end;

procedure TJvWaitForProcessThread.Execute;
var
  WaitHandles: array [0..1] of THandle;
begin
  WaitHandles[0] := FCloseEvent;
  WaitHandles[1] := FProcessHandle;
  WaitForInputIdle(FProcessHandle, INFINITE);
  case WaitForMultipleObjects(2, PWOHandleArray(@WaitHandles[0]), False, INFINITE) of
    WAIT_OBJECT_0:
      FExitCode := MAXDWORD;
    WAIT_OBJECT_0 + 1:
      GetExitCodeProcess(FProcessHandle, FExitCode);
  else
    RaiseLastOSError;
  end;
end;

procedure TJvWaitForProcessThread.TerminateThread;
begin
  Terminate;
  SetEvent(FCloseEvent);
end;

//=== { TJvReadThread } ======================================================

constructor TJvReadThread.Create(AOwner: TObject; AReadHandle, ADestHandle: THandle);
begin
  inherited Create(True);

  FOwner := AOwner;
  FreeOnTerminate := True;
  Priority := tpLower;

  FReadLock := TCriticalSection.Create;

  // Note: TJvReadThread is responsible for closing the FReadHandle
  FReadHandle := AReadHandle;
  FDestHandle := ADestHandle;

  FInputBuffer := nil;
  FInputBufferSize := CCPS_BufferSize;
  FInputBufferEnd := 0;
  ReallocMem(FInputBuffer, FInputBufferSize);
  GetMem(FPreBuffer, CCPS_BufferSize);
end;

destructor TJvReadThread.Destroy;
begin
  SafeCloseHandle(FReadHandle);
  inherited Destroy;
  { It is (theoretically) possible that the inherited Destroy triggers an
    OnTerminate event and the following fields can be accessed in the handler,
    thus free them after the destroy.
  }
  ReallocMem(FInputBuffer, 0);
  FReadLock.Free;
  FreeMem(FPreBuffer);
end;

procedure TJvReadThread.CloseRead;
begin
  FReadLock.Acquire;
  try
    SafeCloseHandle(FReadHandle);
  finally
    FReadLock.Release;
  end;
end;

procedure TJvReadThread.CopyToBuffer(Buffer: PChar; ASize: Cardinal);
// Copy data in Buffer (with size ASize) to FInputBuffer.
begin
  FReadLock.Acquire;
  try
    if FInputBufferEnd + ASize > FInputBufferSize then
    begin
      // Safety check..
      if FInputBufferSize > CCPS_MaxBufferSize then
        // ..main thread seems to be blocked; flush the input buffer
        FInputBufferEnd := 0
      else
      begin
        // Need to upscale FInputBuffer
        FInputBufferSize := FInputBufferSize * 2;
        ReallocMem(FInputBuffer, FInputBufferSize);
      end;
    end;

    // Do the copy
    Move(Buffer[0], FInputBuffer[FInputBufferEnd], ASize);
    Inc(FInputBufferEnd, ASize);
  finally
    FReadLock.Release;
  end;

  // Notify TJvCreateProcess that data has been read from the pipe
  PostMessage(FDestHandle, CM_READ, Integer(FOwner), 0);
end;

procedure TJvReadThread.Execute;
// Read data from the pipe (FReadHandle) to FPreBuffer
var
  BytesRead: Cardinal;
begin
  while not Terminated do
  begin
    { ReadFile will block until *some* data is available on the pipe }
    if not ReadFile(FReadHandle, FPreBuffer[0], CCPS_BufferSize, BytesRead, nil) then
    begin
      // Only exit if last error is ERROR_BROKEN_PIPE, thus ignore other errors
      if GetLastError = ERROR_BROKEN_PIPE then
        // pipe done - normal exit path.
        Exit;
    end
    else
      CopyToBuffer(FPreBuffer, BytesRead);
  end;
end;

function TJvReadThread.ReadBuffer(var ABuffer: TJvCPSBuffer;
  out ABufferSize: Cardinal): Boolean;
// Copy FInputBuffer to ABuffer.
// This function is executed in the context of the main thread;
// FReadLock is for synchronization with the read thread.
begin
  FReadLock.Acquire;
  try
    Result := FInputBufferEnd > 0;
    if not Result then
      Exit;

    ABufferSize := Min(FInputBufferEnd, CCPS_BufferSize);

    // Copy the data from FInputBuffer to ABuffer.
    Move(FInputBuffer[0], ABuffer[0], ABufferSize);

    // If not all data in FInputBuffer is copied to ABuffer, then place
    // the data not copied at the begin of FInputBuffer.
    if FInputBufferEnd > ABufferSize then
      Move(FInputBuffer[ABufferSize], FInputBuffer[0],
        FInputBufferEnd - ABufferSize);

    Dec(FInputBufferEnd, ABufferSize);
  finally
    FReadLock.Release;
  end;
end;

procedure TJvReadThread.TerminateThread;
begin
  Terminate;
  CloseRead;
end;

//=== { TJvConsoleThread } ===================================================

constructor TJvConsoleThread.Create(ProcessHandle: DWORD;
  AWriteHandle: THandle);
begin
  inherited Create(ProcessHandle);

  FWriteLock := TCriticalSection.Create;

  // Note: TJvConsoleThread is responsible for closing the FWriteHandle
  FWriteHandle := AWriteHandle;

  FWriteEvent := CreateEvent(
    nil, // No security attributes
    True, // Manual reset
    False, // Initial state
    nil // No name
    );
end;

destructor TJvConsoleThread.Destroy;
begin
  SafeCloseHandle(FWriteHandle);
  SafeCloseHandle(FWriteEvent);
  inherited Destroy;
  { It is (theoretically) possible that the inherited Destroy triggers an
    OnTerminate event and the following fields can be accessed in the handler,
    thus free them after the destroy.
  }
  FWriteLock.Free;
end;

procedure TJvConsoleThread.CloseWrite;
begin
  FWriteLock.Acquire;
  try
    SafeCloseHandle(FWriteHandle);
  finally
    FWriteLock.Release;
  end;
end;

procedure TJvConsoleThread.Execute;
var
  WaitHandles: array [0..2] of THandle;
  HandleCount: Cardinal;
begin
  WaitHandles[0] := FCloseEvent;
  WaitHandles[1] := FProcessHandle;
  WaitHandles[2] := FWriteEvent;
  HandleCount := 3;

  WaitForInputIdle(FProcessHandle, INFINITE);

  while not Terminated do
    case WaitForMultipleObjects(HandleCount, PWOHandleArray(@WaitHandles[0]), False, INFINITE) of
      WAIT_OBJECT_0:
        begin
          // Close event fired; exit
          FExitCode := MAXDWORD;
          Exit;
        end;
      WAIT_OBJECT_0 + 1:
        begin
          // process ended; exit
          GetExitCodeProcess(FProcessHandle, FExitCode);
          Exit;
        end;
      WAIT_OBJECT_0 + 2:
        // Write event fired; try to write
        if not TryWrite then
          // No longer respond when write event fires
          HandleCount := 2;
    else
      Exit;
    end;
end;

function TJvConsoleThread.TryWrite: Boolean;
// Write data in FOutputBuffer to the pipe (FWriteHandle)
// Result = False; if console or user has closed the pipe.
var
  BytesWritten: Cardinal;
  BytesToWrite: Cardinal;
begin
  Result := True;

  FWriteLock.Acquire;
  try
    try
      { Check handle inside lock, because it can be closed by another thread, by
        calling CloseWrite }
      if FWriteHandle = 0 then
        Exit;

      if FOutputBufferEnd <= 0 then
        Exit;

      BytesToWrite := FOutputBufferEnd;

      if not WriteFile(FWriteHandle, FOutputBuffer, BytesToWrite, BytesWritten, nil) then
      begin
        { WriteFile documentation on MSDN states that WriteFile returns
          ERROR_BROKEN_PIPE if the console closes it's read handle, but that
          seems incorrect; check it anyway }
        if (GetLastError = ERROR_NO_DATA) or (GetLastError = ERROR_BROKEN_PIPE) then
          // Pipe was closed (normal exit path).
          SafeCloseHandle(FWriteHandle);
        Exit;
      end;

      if BytesWritten <= 0 then
        Exit;

      if BytesWritten < BytesToWrite then
        // Move unwritten tail to the begin of the buffer
        Move(FOutputBuffer[BytesWritten], FOutputBuffer[0], BytesToWrite - BytesWritten);

      Dec(FOutputBufferEnd, BytesWritten);
    finally
      Result := FWriteHandle <> 0;
      if FOutputBufferEnd = 0 then
        ResetEvent(FWriteEvent);
    end;
  finally
    FWriteLock.Release;
  end;
end;

function TJvConsoleThread.Write(const S: string): Boolean;
// Add S to FOutputBuffer; actual writing is done in TryWrite.
// This function is executed in the context of the main thread;
// FWriteLock is for synchronization with the write thread.
begin
  if Length(S) <= 0 then
  begin
    Result := True;
    Exit;
  end;

  FWriteLock.Acquire;
  try
    Result := FWriteHandle <> 0;
    if not Result then
      Exit;

    Result := Cardinal(Length(S)) + FOutputBufferEnd <= CCPS_BufferSize;
    if not Result then
      Exit;

    Move(PChar(S)^, FOutputBuffer[FOutputBufferEnd], Length(S));
    Inc(FOutputBufferEnd, Length(S));

    if FOutputBufferEnd > 0 then
      // Notify the TJvConsoleThread that there is some data to write
      SetEvent(FWriteEvent);
  finally
    FWriteLock.Release;
  end;
end;

//=== { TJvCreateProcess } ===================================================

constructor TJvCreateProcess.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCreationFlags := [];
  FEnvironment := TStringList.Create;
  FPriority := ppNormal;
  FState := psReady;
  FWaitForTerminate := True;
  FStartupInfo := TJvCPSStartupInfo.Create;
  FConsoleOptions := [coOwnerData];
  FErrorReader := TJvReader.Create(Self);
  FInputReader := TJvReader.Create(Self);
end;

destructor TJvCreateProcess.Destroy;
begin
  TerminateWaitThread;
  FErrorReader.Free;
  FInputReader.Free;
  FreeAndNil(FEnvironment);
  FreeAndNil(FStartupInfo);
  if FHandle <> 0 then
    DeallocateHWndEx(FHandle);
  CloseProcessHandles;
  inherited Destroy;
end;

procedure TJvCreateProcess.CheckNotWaiting;
begin
  if (FState = psWaiting) and (FRunningThreadCount > 0) then
    raise EJvProcessError.CreateRes(@RsEProcessIsRunning);
end;

procedure TJvCreateProcess.CheckReady;
begin
  if FState <> psReady then
    raise EJvProcessError.CreateRes(@RsEProcessIsRunning);
end;

procedure TJvCreateProcess.CheckRunning;
begin
  if FState = psReady then
    raise EJvProcessError.CreateRes(@RsEProcessNotRunning);
end;

function TJvCreateProcess.CloseApplication(SendQuit: Boolean): Boolean;
begin
  CheckRunning;
  Result := InternalCloseApp(ProcessInfo.dwProcessId, SendQuit);
end;

procedure TJvCreateProcess.CloseProcessHandles;
begin
  OSCheck(SafeCloseHandle(FProcessInfo.hProcess));
  OSCheck(SafeCloseHandle(FProcessInfo.hThread));
end;

procedure TJvCreateProcess.CloseRead;
begin
  TJvReader(FInputReader).CloseRead;
  TJvReader(FErrorReader).CloseRead;
end;

procedure TJvCreateProcess.CloseWrite;
begin
  if FWaitThread is TJvConsoleThread then
    TJvConsoleThread(FWaitThread).CloseWrite;
end;

procedure TJvCreateProcess.HandleThreadTerminated;
begin
  if FState = psWaiting then
  begin
    Dec(FRunningThreadCount);
    if FRunningThreadCount = 0 then
    begin
      GotoReadyState;
      if Assigned(FOnTerminate) then
        FOnTerminate(Self, FExitCode);
    end;
  end;
end;

function TJvCreateProcess.GetConsoleOutput: TStrings;
begin
  Result := FInputReader.ConsoleOutput;
end;

function TJvCreateProcess.GetEnvironment: TStrings;
begin
  Result := FEnvironment;
end;

function TJvCreateProcess.GetHandle: THandle;
begin
  if FHandle = 0 then
    FHandle := AllocateHWndEx(WndProc);
  Result := FHandle;
end;

function TJvCreateProcess.GetOnErrorRawRead: TJvCPSRawReadEvent;
begin
  Result := FErrorReader.OnRawRead;
end;

function TJvCreateProcess.GetOnErrorRead: TJvCPSReadEvent;
begin
  Result := FErrorReader.OnRead;
end;

function TJvCreateProcess.GetOnRawRead: TJvCPSRawReadEvent;
begin
  Result := FInputReader.OnRawRead;
end;

function TJvCreateProcess.GetOnRead: TJvCPSReadEvent;
begin
  Result := FInputReader.OnRead;
end;

procedure TJvCreateProcess.GotoReadyState;
begin
  CheckNotWaiting;
  FState := psReady;
  CloseProcessHandles;
  FRunningThreadCount := 0;
end;

procedure TJvCreateProcess.GotoRunningState;
begin
  CheckReady;
  FState := psRunning;
  CloseProcessHandles;
end;

procedure TJvCreateProcess.GotoWaitState(const AThreadCount: Integer);
begin
  CheckReady;
  FState := psWaiting;
  FRunningThreadCount := AThreadCount;
end;

procedure TJvCreateProcess.HandleReadEvent(Sender: TObject);
begin
  TJvReader(Sender).HandleReadEvent;
end;

procedure TJvCreateProcess.Run;
const
  CreationFlagsValues: array [TJvCPSFlag] of DWORD =
    (CREATE_DEFAULT_ERROR_MODE, CREATE_NEW_CONSOLE, CREATE_NEW_PROCESS_GROUP,
     CREATE_SEPARATE_WOW_VDM, CREATE_SHARED_WOW_VDM, CREATE_SUSPENDED,
     CREATE_UNICODE_ENVIRONMENT, DETACHED_PROCESS);
var
  ConsoleHandles: TJvRWEHandles; // Will be used by the console
  LocalHandles: TJvRWEHandles; // Will be used by TJvCreateProcess
  LStartupInfo: TStartupInfo;
  Flags: DWORD;
  F: TJvCPSFlag;
  AppName, CurrDir: PChar;
  EnvironmentData: PChar;
begin
  GotoReadyState;

  FillChar(FProcessInfo, SizeOf(FProcessInfo), #0);

  Flags := ProcessPriorities[FPriority];
  for F := Low(TJvCPSFlag) to High(TJvCPSFlag) do
    if F in FCreationFlags then
      Inc(Flags, CreationFlagsValues[F]);
  AppName := Pointer(Trim(FApplicationName));
  CurrDir := Pointer(Trim(FCurrentDirectory));
  if Environment.Count = 0 then
    EnvironmentData := nil
  else
    StringsToMultiSz(EnvironmentData, Environment);

  LocalHandles := TJvRWEHandles.Create;
  ConsoleHandles := TJvRWEHandles.Create;
  try
    LStartupInfo := FStartupInfo.GetStartupInfo;

    if coRedirect in ConsoleOptions then
    begin
      ConstructPipe(LocalHandles, ConsoleHandles, coSeparateError in ConsoleOptions);

      with LStartupInfo do
      begin
        dwFlags := dwFlags or STARTF_USESTDHANDLES;
        hStdOutput := ConsoleHandles.Write;
        hStdInput := ConsoleHandles.Read;
        hStdError := ConsoleHandles.Error;
      end;
    end;

    if not CreateProcess(AppName, PChar(FCommandLine), nil, nil, coRedirect in ConsoleOptions,
      Flags, EnvironmentData, CurrDir, LStartupInfo, FProcessInfo) then
    begin
      CloseProcessHandles;
      RaiseLastOSError;
    end;

    if coRedirect in ConsoleOptions then
    begin
      { We use a counter to determine whether all threads are done.
        This counter must be set before a thread is created, because some
        consoles are so short living that for example the wait thread (FWaitThread)
        is terminated before the read thread (FInputReader) is created.
        See Mantis #1393.
      }

      if coSeparateError in ConsoleOptions then
        GotoWaitState(3)
      else
        GotoWaitState(2);

      FWaitThread := TJvConsoleThread.Create(FProcessInfo.hProcess, LocalHandles.ExtractWrite);
      FWaitThread.OnTerminate := WaitThreadTerminated;
      FWaitThread.Resume;

      TJvReader(FInputReader).CreateThread(LocalHandles.ExtractRead);

      if coSeparateError in ConsoleOptions then
        TJvReader(FErrorReader).CreateThread(LocalHandles.ExtractError);
    end
    else
    if WaitForTerminate then
    begin
      GotoWaitState(1);

      FWaitThread := TJvWaitForProcessThread.Create(FProcessInfo.hProcess);
      FWaitThread.OnTerminate := WaitThreadTerminated;
      FWaitThread.Resume;
    end
    else
    begin
      { http://support.microsoft.com/default.aspx?scid=kb;en-us;124121 }
      WaitForInputIdle(FProcessInfo.hProcess, INFINITE);
      GotoRunningState;
    end;
  finally
    { Close pipe handles (do not continue to modify the parent).
      You need to make sure that no handles to the write end of the
      output pipe are maintained in this process or else the pipe will
      not close when the child process exits and the ReadFile will hang.
    }
    ConsoleHandles.Free;
    LocalHandles.Free;
    FreeMultiSz(EnvironmentData);
  end;
end;

procedure TJvCreateProcess.SetEnvironment(const Value: TStrings);
begin
  FEnvironment.Assign(Value);
end;

procedure TJvCreateProcess.SetOnErrorRawRead(
  const Value: TJvCPSRawReadEvent);
begin
  FErrorReader.OnRawRead := Value;
end;

procedure TJvCreateProcess.SetOnErrorRead(const Value: TJvCPSReadEvent);
begin
  FErrorReader.OnRead := Value;
end;

procedure TJvCreateProcess.SetOnRawRead(const Value: TJvCPSRawReadEvent);
begin
  FInputReader.OnRawRead := Value;
end;

procedure TJvCreateProcess.SetOnRead(const Value: TJvCPSReadEvent);
begin
  FInputReader.OnRead := Value;
end;

procedure TJvCreateProcess.SetStartupInfo(Value: TJvCPSStartupInfo);
begin
  FStartupInfo.Assign(Value);
end;

procedure TJvCreateProcess.SetWaitForTerminate(const Value: Boolean);
begin
  GotoReadyState;
  FWaitForTerminate := Value;
end;

procedure TJvCreateProcess.StopWaiting;
begin
  TerminateWaitThread;
end;

procedure TJvCreateProcess.Terminate;
begin
  CheckRunning;
  InternalTerminateProcess(FProcessInfo.dwProcessId);
end;

procedure TJvCreateProcess.TerminateWaitThread;
begin
  { This is a dangerous function; because the read thread uses a blocking
    function there's no way we can stop it (normally); just signal the
    thread that is has to end;

    Note that thus it's the user responsibility to ensure that the console
    will end. If the console ends, the read thread will end also.

    An console can (always?) be ended by calling 'TJvCreateProcess.Terminate'
  }
  if FState = psWaiting then
  begin
    if Assigned(FWaitThread) then
    begin
      FWaitThread.OnTerminate := nil;
      TJvWaitForProcessThread(FWaitThread).TerminateThread;
      FWaitThread := nil;
    end;
    TJvReader(FInputReader).Terminate;
    TJvReader(FErrorReader).Terminate;

    FRunningThreadCount := 0;
    GotoReadyState;
  end;
end;

procedure TJvCreateProcess.WaitThreadTerminated(Sender: TObject);
begin
  FExitCode := TJvWaitForProcessThread(Sender).FExitCode;
  FWaitThread := nil;

  // The user must be able to throw an exception in his OnTerminate handler.
  // But, if we call the OnTerminate handler now, and an exception is thrown
  // the application will halt:
  // Because DoThreadTerminated is called by a thread via Synchronize,
  // exceptions are handled by the thread, which will halt the execution of
  // the whole program. See mantis #3617

  // Another reason to use messages is that the threads can end almost
  // simultanious; without messages we should have used critical sections
  // to determine whether the last thread has ended.

  PostMessage(Handle, CM_THREADTERMINATED, 0, 0);
end;

procedure TJvCreateProcess.WndProc(var Msg: TMessage);
begin
  try
    with Msg do
      case Msg of
        CM_READ: HandleReadEvent(TObject(WParam));
        CM_THREADTERMINATED: HandleThreadTerminated;
      else
        Result := DefWindowProc(Handle, Msg, WParam, LParam);
      end;
  except
    {$IFDEF COMPILER6_UP}
    if Assigned(ApplicationHandleException) then
      ApplicationHandleException(Self);
    {$ELSE}
    Application.HandleException(Self);
    {$ENDIF COMPILER6_UP}
  end;
end;

function TJvCreateProcess.Write(const S: string): Boolean;
begin
  Result := (FWaitThread is TJvConsoleThread) and
    TJvConsoleThread(FWaitThread).Write(S);
end;

function TJvCreateProcess.WriteLn(const S: string): Boolean;
begin
  Result := Write(S + sLineBreak);
end;


//=== { TJvReader } ==========================================================

procedure TJvReader.CloseRead;
begin
  if Assigned(FThread) then
    FThread.CloseRead;
end;

procedure TJvReader.CreateThread(const AReadHandle: THandle);
begin
  FStartsOnNewLine := True;
  FCurrentLine := '';
  FCursorPosition := 0;

  FThread := TJvReadThread.Create(Self, AReadHandle, FCreateProcess.Handle);
  FThread.OnTerminate := ThreadTerminated;
  FThread.Resume;
end;

procedure TJvReader.DoRawReadEvent(Data: PChar; const ASize: Cardinal);
var
  S: string;
begin
  if Assigned(FOnRawRead) then
  begin
    // Do copy because of possible #0's etc.
    SetLength(S, ASize);
    Move(Data^, PChar(S)^, ASize);
    FOnRawRead(FCreateProcess, S);
  end;
end;

procedure TJvReader.DoReadEvent(const EndsWithNewLine: Boolean);
begin
  // Notify user and update current line & cursor
  if not (coOwnerData in FCreateProcess.ConsoleOptions) then
  begin
    if FStartsOnNewLine or (ConsoleOutput.Count = 0) then
      ConsoleOutput.Add(FCurrentLine)
    else
      ConsoleOutput[ConsoleOutput.Count - 1] := FCurrentLine;
  end;
  if Assigned(FOnRead) then
    FOnRead(FCreateProcess, FCurrentLine, FStartsOnNewLine);
  if EndsWithNewLine then
  begin
    FCurrentLine := '';
    FCursorPosition := 0;
  end;
  FStartsOnNewLine := EndsWithNewLine;
end;

procedure TJvReader.HandleReadEvent;
var
  ASize: Cardinal;
begin
  { Copy the data from the read thread to the this (main) thread and
    parse the console output }

  while Assigned(FThread) and FThread.ReadBuffer(FParseBuffer, ASize) do
    ParseConsoleOutput(FParseBuffer, ASize);
end;

procedure TJvReader.ParseConsoleOutput(Data: PChar; ASize: Cardinal);
var
  P, Q: PChar;

  procedure DoOutput;
    { Copy chunk [Q..P) to the current line & Update cursor position }
  var
    ChunkSize: Integer;
  begin
    ChunkSize := P - Q;
    if ChunkSize <= 0 then
      Exit;

    // Does the chunck fit on the current line..
    if Length(FCurrentLine) < FCursorPosition + ChunkSize then
      // .. if not resize current line
      SetLength(FCurrentLine, FCursorPosition + ChunkSize);

    // Move the chunk to the current line
    Move(Q^, (PChar(FCurrentLine) + FCursorPosition)^, ChunkSize);

    // Update the cursor
    Inc(FCursorPosition, ChunkSize);
  end;

  procedure DoTab;
  begin
    // Does the chunck (8 spaces) fit on the current line..
    if Length(FCurrentLine) < FCursorPosition + 8 then
      // .. if not resize current line }
      SetLength(FCurrentLine, FCursorPosition + 8);

    // Fill 8 spaces on the currentline at the cursor position
    FillChar((PChar(FCurrentLine) + FCursorPosition)^, 8, #32);

    // Update the cursor
    Inc(FCursorPosition, 8);
  end;

begin
  DoRawReadEvent(Data, ASize);

  P := Data;
  Q := Data;

  while Cardinal(P - Data) < ASize do
    case P^ of
      #0, #7: // NULL and BELL
        begin
          // Replace with space
          P^ := #32;
          Inc(P);
        end;
      Backspace:
        begin
          DoOutput;
          Dec(FCursorPosition);
          if FCursorPosition < 0 then
            FCursorPosition := 0;
          Inc(P);
          Q := P;
        end;
      Tab:
        begin
          // Replace with 8 spaces
          DoOutput;
          DoTab;
          Inc(P);
          Q := P;
        end;
      Lf:
        begin
          DoOutput;
          DoReadEvent(True);
          Inc(P);
          Q := P;
        end;
      Cr:
        begin
          DoOutput;
          FCursorPosition := 0;
          Inc(P);
          Q := P;
        end;
    else
      Inc(P);
    end;
  DoOutput;
  DoReadEvent(False);
end;

procedure TJvReader.Terminate;
begin
  if Assigned(FThread) then
  begin
    FThread.OnTerminate := nil;
    FThread.TerminateThread;
    FThread := nil;
  end;
end;

procedure TJvReader.ThreadTerminated(Sender: TObject);
begin
  // Read for the last time data from the read thread
  HandleReadEvent;
  if FCurrentLine <> '' then
    DoReadEvent(False);

  FThread := nil;

  PostMessage(FCreateProcess.Handle, CM_THREADTERMINATED, 0, 0);
end;

//=== { TJvRWEHandles } ======================================================

destructor TJvRWEHandles.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJvRWEHandles.Clear;
begin
  Read := 0;
  Write := 0;
  Error := 0;
end;

function TJvRWEHandles.ExtractHandle(const Index: Integer): THandle;
begin
  Result := FHandle[Index];
  FHandle[Index] := 0;
end;

function TJvRWEHandles.GetHandle(const Index: Integer): THandle;
begin
  Result := FHandle[Index];
end;

procedure TJvRWEHandles.SetHandle(const Index: Integer;
  const Value: THandle);
begin
  if Value <> FHandle[Index] then
  begin
    if FHandle[Index] <> 0 then
      CloseHandle(FHandle[Index]);
    FHandle[Index] := Value;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
