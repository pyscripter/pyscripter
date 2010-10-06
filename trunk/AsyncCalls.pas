{**************************************************************************************************}
{                                                                                                  }
{ Asynchronous function calls utilizing multiple threads.                                          }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is AsyncCalls.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Andreas Hausladen.                                 }
{ Portions created by Andreas Hausladen are Copyright (C) 2006-2010 Andreas Hausladen.             }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Version: 2.96 (2010-09-12)                                                                       }
{   Fixed: CoInitialize call was missing                                                           }
{                                                                                                  }
{ Version: 2.95 (2010-09-12)                                                                       }
{   Added: Support for RAD Studio XE                                                               }
{   Added: Support for UnicodeString                                                               }
{                                                                                                  }
{ Version: 2.92 (2009-08-30)                                                                       }
{   Added: Support for RAD Studio 2010                                                             }
{   Restored: Delphi 2009 Update 1 fixed the compiler bug. All generic methods are now available.  }
{                                                                                                  }
{ Version: 2.91 (2008-09-29)                                                                       }
{   Fixed: All generic methods are now disabled due to an internal compiler error in Delphi 2009   }
{                                                                                                  }
{ Version: 2.9 (2008-09-27)                                                                        }
{   Fixed: Window message handling                                                                 }
{   Added: Delphi 2009 support with generics and anonymous methods                                 }
{   Added: AsyncCall(Runnable: IAsyncRunnable)                                                     }
{                                                                                                  }
{ Version: 2.21 (2008-05-14)                                                                       }
{   Fixed: Fixed bug in AsyncMultiSync                                                             }
{                                                                                                  }
{ Version: 2.2 (2008-05-12)                                                                        }
{   Fixed: Bugs in main thread AsyncMultiSync implementation                                       }
{   Added: Delphi 5 support                                                                        }
{                                                                                                  }
{ Version: 2.1 (2008-05-06)                                                                        }
{   Added: Delphi 6 support                                                                        }
{   Added: Support for "Exit;" in the MainThread block                                             }
{   Fixed: Exception handling for Delphi 6, 7 and 2005                                             }
{   Fixed: EBX, ESI and EDI weren't copied into the synchronized block (e.g. used for Self-Pointer)}
{                                                                                                  }
{ Version: 2.0 (2008-05-04)                                                                        }
{   Added: EnterMainThread/LeaveMainThread                                                         }
{   Added: LocalVclCall, LocalAsyncVclCall, MsgAsyncMultiSync                                      }
{   Added: LocalAsyncExec, AsyncExec                                                               }
{   Added: IAsyncCall.ForceDifferentThread                                                         }
{   Fixed: Exception handling                                                                      }
{   Removed: Delphi 5 and 6 support                                                                }
{                                                                                                  }
{ Version: 1.2 (2008-02-10)                                                                        }
{   Added: CoInitialize                                                                            }
{   Added: LocalAsynCall() function                                                                }
{                                                                                                  }
{ Version: 1.1 (2007-08-14)                                                                        }
{   Fixed: Workaround for TThread.Resume bug                                                       }
{                                                                                                  }
{ Version: 1.0 (2006-12-23)                                                                        }
{   Initial release                                                                                }
{**************************************************************************************************}
{$A+,B-,C-,D-,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W+,X+,Y+,Z1}

unit AsyncCalls;

{$DEFINE DEBUG_ASYNCCALLS}

interface

{$IFNDEF CONDITIONALEXPRESSIONS}
  {$IFDEF VER130}
    {$DEFINE DELPHI5}
  {$ELSE}
    'Your compiler version is not supported'
  {$ENDIF}
{$ELSE}
  {$IFDEF VER140}
    {$DEFINE DELPHI6}
    {.$MESSAGE ERROR 'Your compiler version is not supported'}
  {$ELSE}
    {$DEFINE DELPHI7_UP}
  {$ENDIF}

  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$IF CompilerVersion >= 15.0}
    {$WARN UNSAFE_TYPE OFF}
    {$WARN UNSAFE_CODE OFF}
    {$WARN UNSAFE_CAST OFF}
  {$IFEND}

  {$IF CompilerVersion >= 18.0}
    {$DEFINE SUPPORTS_INLINE}
  {$IFEND}

  {$IF CompilerVersion >= 20.0}
    {$DEFINE DELPHI2009_UP}
  {$IFEND}

  {$IF CompilerVersion >= 21.0}
    {$DEFINE DELPHI2010_UP}
  {$IFEND}
{$ENDIF}

{$IFDEF DEBUG_ASYNCCALLS}
  {$D+,C+}
{$ENDIF DEBUG_ASYNCCALLS}

uses
  Windows, Messages, SysUtils, Classes, Contnrs, ActiveX, SyncObjs;

type
  {$IFNDEF CONDITIONALEXPRESSIONS}
  INT_PTR = Integer;
  IInterface = IUnknown;
  {$ELSE}
    {$IF not declared(INT_PTR)}
  INT_PTR = Integer;
    {$IFEND}
  {$ENDIF}

  TAsyncIdleMsgMethod = procedure of object;

  TCdeclFunc = Pointer; // function(Arg1: Type1; Arg2: Type2; ...); cdecl;
  TCdeclMethod = TMethod; // function(Arg1: Type1; Arg2: Type2; ...) of object; cdecl;
  TLocalAsyncProc = function: Integer;
  TLocalVclProc = function(Param: INT_PTR): INT_PTR;
  TLocalAsyncProcEx = function(Param: INT_PTR): INT_PTR;
  //TLocalAsyncForLoopProc = function(Index: Integer; SyncLock: TCriticalSection): Boolean;

  TAsyncCallArgObjectProc = function(Arg: TObject): Integer;
  TAsyncCallArgIntegerProc = function(Arg: Integer): Integer;
  TAsyncCallArgStringProc = function(const Arg: string): Integer;
  TAsyncCallArgWideStringProc = function(const Arg: WideString): Integer;
  TAsyncCallArgInterfaceProc = function(const Arg: IInterface): Integer;
  TAsyncCallArgExtendedProc = function(const Arg: Extended): Integer;
  TAsyncCallArgVariantProc = function(const Arg: Variant): Integer;

  TAsyncCallArgObjectMethod = function(Arg: TObject): Integer of object;
  TAsyncCallArgIntegerMethod = function(Arg: Integer): Integer of object;
  TAsyncCallArgStringMethod = function(const Arg: string): Integer of object;
  TAsyncCallArgWideStringMethod = function(const Arg: WideString): Integer of object;
  TAsyncCallArgInterfaceMethod = function(const Arg: IInterface): Integer of object;
  TAsyncCallArgExtendedMethod = function(const Arg: Extended): Integer of object;
  TAsyncCallArgVariantMethod = function(const Arg: Variant): Integer of object;

  TAsyncCallArgObjectEvent = procedure(Arg: TObject) of object;
  TAsyncCallArgIntegerEvent = procedure(Arg: Integer) of object;
  TAsyncCallArgStringEvent = procedure(const Arg: string) of object;
  TAsyncCallArgWideStringEvent = procedure(const Arg: WideString) of object;
  TAsyncCallArgInterfaceEvent = procedure(const Arg: IInterface) of object;
  TAsyncCallArgExtendedEvent = procedure(const Arg: Extended) of object;
  TAsyncCallArgVariantEvent = procedure(const Arg: Variant) of object;

  TAsyncCallArgRecordProc = function(var Arg{: TRecordType}): Integer;
  TAsyncCallArgRecordMethod = function(var Arg{: TRecordType}): Integer of object;
  TAsyncCallArgRecordEvent = procedure(var Arg{: TRecordType}) of object;

  EAsyncCallError = class(Exception);

  IAsyncCall = interface
    { Sync() waits until the asynchronous call has finished and returns the
      result value of the called function if that exists. }
    function Sync: Integer;

    { Finished() returns True if the asynchronous call has finished. }
    function Finished: Boolean;

    { ReturnValue() returns the result of the asynchronous call. It raises an
      exception if called before the function has finished. }
    function ReturnValue: Integer;

    { ForceDifferentThread() tells AsyncCalls that the assigned function must
      not be executed in the current thread. }
    procedure ForceDifferentThread;
  end;

  { *** Internal interface. Do not use it *** }
  IAsyncCallEx = interface
    ['{A31D8EE4-17B6-4FC7-AC94-77887201EE56}']
    function GetEvent: THandle;
    function SyncInThisThreadIfPossible: Boolean;
  end;

  IAsyncRunnable = interface
    ['{1A313BBD-0F89-43AD-8B57-BBA3205F4888}']
    procedure AsyncRun;
  end;


{ SetMaxAsyncCallThreads() controls how many threads can be used by the
  async call thread pool. The thread pool creates threads when they are needed.
  Allocated threads are not destroyed until the application has terminated, but
  they are suspended if not used. }
procedure SetMaxAsyncCallThreads(MaxThreads: Integer);
{ GetMaxAsyncCallThreads() returns the maximum number of threads that can
  exist in the thread pool. }
function GetMaxAsyncCallThreads: Integer;


{ AsyncCall() executes the given function/procedure in a separate thread. The
  result value of the asynchronous function is returned by IAsyncCall.Sync() and
  IAsyncCall.ReturnValue().
  The AsyncExec() function calls the IdleMsgMethod in a loop, while the async.
  method is executed.

Example:
  function FileAgeAsync(const Filename: string): Integer;
  begin
    Result := FileAge(Filename);
  end;

  var
    a: IAsyncCall;
  begin
    a := AsyncCall(FileAgeAsync, 'C:\Windows\notepad.exe');
    // do something
    Age := a.Sync;
  end;
}
function AsyncCall(Proc: TAsyncCallArgObjectProc; Arg: TObject): IAsyncCall; overload;
function AsyncCall(Proc: TAsyncCallArgIntegerProc; Arg: Integer): IAsyncCall; overload;
function AsyncCall(Proc: TAsyncCallArgStringProc; const Arg: string): IAsyncCall; overload;
function AsyncCall(Proc: TAsyncCallArgWideStringProc; const Arg: WideString): IAsyncCall; overload;
function AsyncCall(Proc: TAsyncCallArgInterfaceProc; const Arg: IInterface): IAsyncCall; overload;
function AsyncCall(Proc: TAsyncCallArgExtendedProc; const Arg: Extended): IAsyncCall; overload;
function AsyncCallVar(Proc: TAsyncCallArgVariantProc; const Arg: Variant): IAsyncCall; overload;

function AsyncCall(Method: TAsyncCallArgObjectMethod; Arg: TObject): IAsyncCall; overload;
function AsyncCall(Method: TAsyncCallArgIntegerMethod; Arg: Integer): IAsyncCall; overload;
function AsyncCall(Method: TAsyncCallArgStringMethod; const Arg: string): IAsyncCall; overload;
function AsyncCall(Method: TAsyncCallArgWideStringMethod; const Arg: WideString): IAsyncCall; overload;
function AsyncCall(Method: TAsyncCallArgInterfaceMethod; const Arg: IInterface): IAsyncCall; overload;
function AsyncCall(Method: TAsyncCallArgExtendedMethod; const Arg: Extended): IAsyncCall; overload;
function AsyncCallVar(Method: TAsyncCallArgVariantMethod; const Arg: Variant): IAsyncCall; overload;

function AsyncCall(Method: TAsyncCallArgObjectEvent; Arg: TObject): IAsyncCall; overload;
function AsyncCall(Method: TAsyncCallArgIntegerEvent; Arg: Integer): IAsyncCall; overload;
function AsyncCall(Method: TAsyncCallArgStringEvent; const Arg: string): IAsyncCall; overload;
function AsyncCall(Method: TAsyncCallArgWideStringEvent; const Arg: WideString): IAsyncCall; overload;
function AsyncCall(Method: TAsyncCallArgInterfaceEvent; const Arg: IInterface): IAsyncCall; overload;
function AsyncCall(Method: TAsyncCallArgExtendedEvent; const Arg: Extended): IAsyncCall; overload;
function AsyncCallVar(Method: TAsyncCallArgVariantEvent; const Arg: Variant): IAsyncCall; overload;

function AsyncCall(Runnable: IAsyncRunnable): IAsyncCall; overload;

procedure AsyncExec(Method: TNotifyEvent; Arg: TObject; IdleMsgMethod: TAsyncIdleMsgMethod);

{ LocalAsyncCall() executes the given local function/procedure in a separate thread.
  The result value of the asynchronous function is returned by IAsyncCall.Sync() and
  IAsyncCall.ReturnValue().
  The LocalAsyncExec() function calls the IdleMsgMethod while the local procedure is
  executed.

Example:
  procedure MainProc(const S: string);
  var
    Value: Integer;
    a: IAsyncCall;

    function DoSomething: Integer;
    begin
      if S = 'Abc' then
        Value := 1;
      Result := 0;
    end;

  begin
    a := LocalAsyncCall(@DoSomething);
    // do something
    a.Sync;

    LocalAsyncExec(@DoSomething, Application.ProcessMessages);
  end;
}
function LocalAsyncCall(LocalProc: TLocalAsyncProc): IAsyncCall;
function LocalAsyncCallEx(LocalProc: TLocalAsyncProcEx; Param: INT_PTR): IAsyncCall;
procedure LocalAsyncExec(Proc: TLocalAsyncProc; IdleMsgMethod: TAsyncIdleMsgMethod);



{ LocalVclCall() executes the given local function/procedure in the main thread. It
  uses the TThread.Synchronize function which blocks the current thread.
  LocalAsyncVclCall() execute the given local function/procedure in the main thread.
  It does not wait for the main thread to execute the function unless the current
  thread is the main thread. In that case it executes and waits for the specified
  function in the current thread like LocalVclCall().

  The result value of the asynchronous function is returned by IAsyncCall.Sync() and
  IAsyncCall.ReturnValue().

Example:
  procedure TForm1.MainProc;

    procedure DoSomething;

      procedure UpdateProgressBar(Percentage: Integer);
      begin
        ProgressBar.Position := Percentage;
        Sleep(20); // This delay does not affect the time for the 0..100 loop
                   // because UpdateProgressBar is non-blocking.
      end;

      procedure Finished;
      begin
        ShowMessage('Finished');
      end;

    var
      I: Integer;
    begin
      for I := 0 to 100 do
      begin
        // Do some time consuming stuff
        Sleep(30);
        LocalAsyncVclCall(@UpdateProgressBar, I); // non-blocking
      end;
      LocalVclCall(@Finished); // blocking
    end;

  var
    a: IAsyncCall;
  begin
    a := LocalAsyncCall(@DoSomething);
    a.ForceDifferentThread; // Do not execute in the main thread because this will
                            // change LocalAyncVclCall into a blocking LocalVclCall
    // do something
    //a.Sync; The Compiler will call this for us in the Interface._Release method
  end;
}

procedure LocalVclCall(LocalProc: TLocalVclProc; Param: INT_PTR = 0);
function LocalAsyncVclCall(LocalProc: TLocalVclProc; Param: INT_PTR = 0): IAsyncCall;



{ AsyncCallEx() executes the given function/procedure in a separate thread. The
  Arg parameter can be a record type. The fields of the record can be modified
  in the asynchon function.

Example:
  type
    TData = record
      Value: Integer;
    end;

  procedure TestRec(var Data: TData);
  begin
    Data.Value := 70;
  end;

  a := AsyncCallEx(@TestRec, MyData);
  a.Sync; // MyData.Value is now 70
}
function AsyncCallEx(Proc: TAsyncCallArgRecordProc; var Arg{: TRecordType}): IAsyncCall; overload;
function AsyncCallEx(Method: TAsyncCallArgRecordMethod; var Arg{: TRecordType}): IAsyncCall; overload;
function AsyncCallEx(Method: TAsyncCallArgRecordEvent; var Arg{: TRecordType}): IAsyncCall; overload;


{ The following AsyncCall() functions support variable parameters. All reference
  counted types are protected by an AddRef and later Release. The ShortString,
  Extended, Currency and Int64 types are internally copied to a temporary location.

Supported types:
  Integer      :  Arg: Integer
  Boolean      :  Arg: Boolean
  Char         :  Arg: AnsiChar
  WideChar     :  Arg: WideChar
  Int64        :  [const] Arg: Int64
  Extended     :  [const] Arg: Extended
  Currency     :  [const] Arg: Currency
  String       :  [const] Arg: ShortString
  Pointer      :  [const] Arg: Pointer
  PChar        :  [const] Arg: PChar
  Object       :  [const] Arg: TObject
  Class        :  [const] Arg: TClass
  AnsiString   :  [const] Arg: AnsiString
  UnicodeString:  [const] Arg: UnicodeString
  PWideChar    :  [const] Arg: PWideChar
  WideString   :  [const] Arg: WideString
  Interface    :  [const] Arg: IInterface
  Variant      :  const Arg: Variant

Example:
  procedure Test(const S: string; I: Integer; E: Extended; Obj: TObject); cdecl;
  begin
  end;

  AsyncCall(@Test, ['Hallo', 10, 3.5, MyObject]);
}
function AsyncCall(Proc: TCdeclFunc; const Args: array of const): IAsyncCall; overload;
function AsyncCall(Proc: TCdeclMethod; const Args: array of const): IAsyncCall; overload;



{ AsyncMultiSync() waits for the async calls and other handles to finish.
  MsgAsyncMultiSync() waits for the async calls, other handles and the message queue.

  Arguments:
    List            : An array of IAsyncCall interfaces for which the function
                      should wait.

    Handles         : An array of THandle for which the function should wait.

    WaitAll = True  : The function returns when all listed async calls have
                      finished. If Milliseconds is INFINITE the async calls
                      meight be executed in the current thread.
                      The return value is zero when all async calls have finished.
                      Otherwise it is -1.

    WaitAll = False : The function returns when at least one of the async calls
                      has finished. The return value is the list index of the
                      first finished async call. If there was a timeout, the
                      return value is -1.

    Milliseconds    : Specifies the number of milliseconds to wait until a
                      timeout happens. The value INFINITE lets the function wait
                      until all async calls have finished.

    dwWakeMask      : see Windows.MsgWaitForMultipleObjects()

  Limitations:
    Length(List)+Length(Handles) must not exceed MAXIMUM_ASYNC_WAIT_OBJECTS.

  Return value:
    WAIT_TIMEOUT
      The function timed out

    WAIT_OBJECT_0+index
      The first finished async call
    WAIT_OBJECT_0+Length(List)+index
      The first signaled handle
    WAIT_OBJECT_0+Length(List)+Length(Handles)
      A message was signaled

    WAIT_ABANDONED_0+index
      The abandoned async call
    WAIT_ABANDONED_0+Length(List)+index
      The abandoned handle

    WAIT_FAILED
      The function failed
}

const
  MAXIMUM_ASYNC_WAIT_OBJECTS = MAXIMUM_WAIT_OBJECTS - 3;

function AsyncMultiSync(const List: array of IAsyncCall; WaitAll: Boolean = True;
  Milliseconds: Cardinal = INFINITE): Cardinal;
function AsyncMultiSyncEx(const List: array of IAsyncCall; const Handles: array of THandle;
  WaitAll: Boolean = True; Milliseconds: Cardinal = INFINITE): Cardinal;
function MsgAsyncMultiSync(const List: array of IAsyncCall; WaitAll: Boolean;
  Milliseconds: Cardinal; dwWakeMask: DWORD): Cardinal;
function MsgAsyncMultiSyncEx(const List: array of IAsyncCall; const Handles: array of THandle;
  WaitAll: Boolean; Milliseconds: Cardinal; dwWakeMask: DWORD): Cardinal;

{
   EnterMainThread/LeaveMainThread can be used to temporary switch to the
   main thread. The code that should be synchonized (blocking) has to be put
   into a try/finally block and the LeaveMainThread() function must be called
   from the finally block. A missing try/finally will lead to an access violation.
   
   * All local variables can be used. (EBP points to the thread's stack while
     ESP points the the main thread's stack)
   * Unhandled exceptions are passed to the surrounding thread.
   * The integrated Debugger is not able to follow the execution flow. You have
     to use break points instead of "Step over/in".
   * Nested calls to EnterMainThread/LeaveMainThread are ignored. But they must
     strictly follow the try/finally structure.

   Example:

     procedure MyThreadProc;
     var
       S: string;
     begin
       Assert(GetCurrentThreadId <> MainThreadId);
       S := 'Hallo, I''m executed in the main thread';

       EnterMainThread;
       try
         Assert(GetCurrentThreadId = MainThreadId);
         ShowMessage(S);
       finally
         LeaveMainThread;
       end;

       Assert(GetCurrentThreadId <> MainThreadId);
     end;
}
procedure EnterMainThread;
procedure LeaveMainThread;


type
  { *** Internal class. Do not use it *** }
  { TAsyncCall is the base class for all parameter based async call types }
  TAsyncCall = class(TInterfacedObject, IAsyncCall, IAsyncCallEx)
  private
    FEvent: THandle;
    FReturnValue: Integer;
    FFinished: Boolean;
    FFatalException: Exception;
    FFatalErrorAddr: Pointer;
    FForceDifferentThread: Boolean;
    procedure InternExecuteAsyncCall;
    procedure InternExecuteSyncCall;
    procedure Quit(AReturnValue: Integer);
  protected
    { Decendants must implement this method. It is called  when the async call
      should be executed. }
    function ExecuteAsyncCall: Integer; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    function _Release: Integer; stdcall;
    function ExecuteAsync: TAsyncCall;
    function SyncInThisThreadIfPossible: Boolean;

    function GetEvent: Cardinal;

    function Sync: Integer;
    function Finished: Boolean;
    function ReturnValue: Integer;
    procedure ForceDifferentThread;
  end;

  { *** Internal class. Do not use it *** }
  { TSyncCall is a fake IAsyncCall implementor. The async call was already
    executed when the interface is returned. }
  TSyncCall = class(TInterfacedObject, IAsyncCall)
  private
    FReturnValue: Integer;
  public
    constructor Create(AReturnValue: Integer);
    function Sync: Integer;
    function Finished: Boolean;
    function ReturnValue: Integer;
    procedure ForceDifferentThread;
  end;

{$IFDEF DELPHI2009_UP}
type
  { *** Helpher class *** }
  TMultiArgProcCall<TProc, T1> = class(TAsyncCall)
  private
    FProc: TProc;
    FArg1: T1;
  public
    constructor Create(AProc: TProc; const AArg1: T1);
  end;

  TMultiArgProcCall<TProc, T1, T2> = class(TMultiArgProcCall<TProc, T1>)
  private
    FArg2: T2;
  public
    constructor Create(AProc: TProc; const AArg1: T1; const AArg2: T2);
  end;

  TMultiArgProcCall<TProc, T1, T2, T3> = class(TMultiArgProcCall<TProc, T1, T2>)
  private
    FArg3: T3;
  public
    constructor Create(AProc: TProc; const AArg1: T1; const AArg2: T2; const AArg3: T3);
  end;

  TMultiArgProcCall<TProc, T1, T2, T3, T4> = class(TMultiArgProcCall<TProc, T1, T2, T3>)
  private
    FArg4: T4;
  public
    constructor Create(AProc: TProc; const AArg1: T1; const AArg2: T2; const AArg3: T3; const AArg4: T4);
  end;

  TAsyncCalls = class(TObject)
  private
    type
      TAsyncCallArgGenericProc<T> = function(Arg: T): Integer;
      TAsyncCallArgGenericProc<T1, T2> = function(Arg1: T1; Arg2: T2): Integer;
      TAsyncCallArgGenericProc<T1, T2, T3> = function(Arg1: T1; Arg2: T2; Arg3: T3): Integer;
      TAsyncCallArgGenericProc<T1, T2, T3, T4> = function(Arg1: T1; Arg2: T2; Arg3: T3; Arg4: T4): Integer;
      TAsyncCallArgGenericMethod<T> = function(Arg: T): Integer of object;
      TAsyncCallArgGenericMethod<T1, T2> = function(Arg1: T1; Arg2: T2): Integer of object;
      TAsyncCallArgGenericMethod<T1, T2, T3> = function(Arg1: T1; Arg2: T2; Arg3: T3): Integer of object;
      TAsyncCallArgGenericMethod<T1, T2, T3, T4> = function(Arg1: T1; Arg2: T2; Arg3: T3; Arg4: T4): Integer of object;
      TIntFunc = reference to function: Integer;

      TAsyncCallArg<T> = class(TMultiArgProcCall<TAsyncCallArgGenericProc<T>, T>)
      protected
        function ExecuteAsyncCall: Integer; override;
      end;

      TAsyncCallArg<T1, T2> = class(TMultiArgProcCall<TAsyncCallArgGenericProc<T1, T2>, T1, T2>)
      protected
        function ExecuteAsyncCall: Integer; override;
      end;

      TAsyncCallArg<T1, T2, T3> = class(TMultiArgProcCall<TAsyncCallArgGenericProc<T1, T2, T3>, T1, T2, T3>)
      protected
        function ExecuteAsyncCall: Integer; override;
      end;

      TAsyncCallArg<T1, T2, T3, T4> = class(TMultiArgProcCall<TAsyncCallArgGenericProc<T1, T2, T3, T4>, T1, T2, T3, T4>)
      protected
        function ExecuteAsyncCall: Integer; override;
      end;

      TAsyncCallArgMethod<T> = class(TMultiArgProcCall<TAsyncCallArgGenericMethod<T>, T>)
      protected
        function ExecuteAsyncCall: Integer; override;
      end;

      TAsyncCallArgMethod<T1, T2> = class(TMultiArgProcCall<TAsyncCallArgGenericMethod<T1, T2>, T1, T2>)
      protected
        function ExecuteAsyncCall: Integer; override;
      end;

      TAsyncCallArgMethod<T1, T2, T3> = class(TMultiArgProcCall<TAsyncCallArgGenericMethod<T1, T2, T3>, T1, T2, T3>)
      protected
        function ExecuteAsyncCall: Integer; override;
      end;

      TAsyncCallArgMethod<T1, T2, T3, T4> = class(TMultiArgProcCall<TAsyncCallArgGenericMethod<T1, T2, T3, T4>, T1, T2, T3, T4>)
      protected
        function ExecuteAsyncCall: Integer; override;
      end;

      TAsyncCallAnonymProc = class(TAsyncCall)
      private
        FProc: TProc;
      protected
        function ExecuteAsyncCall: Integer; override;
      public
        constructor Create(AProc: TProc);
      end;

      TAsyncCallAnonymFunc = class(TAsyncCall)
      private
        FProc: TIntFunc;
      protected
        function ExecuteAsyncCall: Integer; override;
      public
        constructor Create(AProc: TIntFunc);
      end;

      TAsyncVclCallAnonymProc = class(TAsyncCall)
      private
        FProc: TProc;
      protected
        function ExecuteAsyncCall: Integer; override;
      public
        constructor Create(AProc: TProc);
      end;

  public
    { Invoke an asynchronous function call }
    class function Invoke<T>(Proc: TAsyncCallArgGenericProc<T>; const Arg: T): IAsyncCall; overload; static;
    class function Invoke<T>(Event: TAsyncCallArgGenericMethod<T>; const Arg: T): IAsyncCall; overload; static;
    class function Invoke<T1, T2>(Proc: TAsyncCallArgGenericProc<T1, T2>; const Arg1: T1; const Arg2: T2): IAsyncCall; overload; static;
    class function Invoke<T1, T2>(Event: TAsyncCallArgGenericMethod<T1, T2>; const Arg1: T1; const Arg2: T2): IAsyncCall; overload; static;
    class function Invoke<T1, T2, T3>(Proc: TAsyncCallArgGenericProc<T1, T2, T3>; const Arg1: T1; const Arg2: T2; const Arg3: T3): IAsyncCall; overload; static;
    class function Invoke<T1, T2, T3>(Event: TAsyncCallArgGenericMethod<T1, T2, T3>; const Arg1: T1; const Arg2: T2; const Arg3: T3): IAsyncCall; overload; static;
    class function Invoke<T1, T2, T3, T4>(Proc: TAsyncCallArgGenericProc<T1, T2, T3, T4>; const Arg1: T1; const Arg2: T2; const Arg3: T3; const Arg4: T4): IAsyncCall; overload; static;
    class function Invoke<T1, T2, T3, T4>(Event: TAsyncCallArgGenericMethod<T1, T2, T3, T4>; const Arg1: T1; const Arg2: T2; const Arg3: T3; const Arg4: T4): IAsyncCall; overload; static;

    { Invoke an asynchronous anonymous method call }
    class function Invoke(Func: TIntFunc): IAsyncCall; overload; static;
    class function Invoke(Proc: TProc): IAsyncCall; overload; static;

    { MsgExec waits for the @AsyncCall to finish. If there are any messages in
      the message queue and the function was called from the main thread, it will
      call @IdleMsgMethod. "Application.ProcessMessages" can be specified for
      @IdleMsgMethod. }
    class procedure MsgExec(AsyncCall: IAsyncCall; IdleMsgMethod: TAsyncIdleMsgMethod); static;

    { Synchronize with the VCL }

    { VCLSync returns when the anonymous method was called in the main thread }
    class procedure VCLSync(Proc: TProc); static;
    { VCLInvoke returns immediately. The anonymous method will be executed in
      the main thread. }
    class function VCLInvoke(Proc: TProc): IAsyncCall; static;
  end;
{$ENDIF DELPHI2009_UP}

implementation

{$IFDEF DELPHI5}
uses
  Forms; // AllocateHWnd
{$ENDIF DELPHI5}

resourcestring
  RsAsyncCallNotFinished = 'The asynchronous call is not finished yet';
  RsAsyncCallUnknownVarRecType = 'Unknown TVarRec type %d';
  RsLeaveMainThreadNestedError = 'Unpaired call to AsyncCalls.LeaveMainThread()';
  RsLeaveMainThreadThreadError = 'AsyncCalls.LeaveMainThread() was called outside of the main thread';

{$IFNDEF DELPHI7_UP}
var
  SyncEvent: THandle;

type
  TThread = class(Classes.TThread)
  {$IFDEF DELPHI6}
  private
    class procedure WakeMainThread(Sender: TObject);
  {$ENDIF DELPHI6}
  public
    class procedure StaticSynchronize(AThread: TThread; AMethod: TThreadMethod);
  end;

class procedure TThread.StaticSynchronize(AThread: TThread; AMethod: TThreadMethod);
var
  Obj: TThread;
begin
  if GetCurrentThreadId = MainThreadId then
    AMethod
  else if AThread <> nil then
    AThread.Synchronize(AMethod)
  else
  begin
    {$WARNINGS OFF} // suppress abstract class warning
    Obj := TThread.Create(True);
    {$WARNINGS ON}
    try
      Obj.Synchronize(AMethod);
    finally
      Obj.Free;
    end;
  end;
end;
{$ENDIF ~DELPHI7_UP}

procedure StaticSynchronize(AMethod: TThreadMethod);
begin
  {$IFDEF DELPHI2010_UP}
  TThread.Synchronize(nil, AMethod);
  {$ELSE}
  TThread.StaticSynchronize(nil, AMethod);
  {$ENDIF DELPHI2010_UP}
end;

{$IFDEF DELPHI5}
function CheckSynchronize(Timeout: Integer = 0): Boolean;
begin
  Result := False;
end;

function AcquireExceptionObject: Pointer;
type
  PRaiseFrame = ^TRaiseFrame;
  TRaiseFrame = record
    NextRaise: PRaiseFrame;
    ExceptAddr: Pointer;
    ExceptObject: TObject;
    ExceptionRecord: PExceptionRecord;
  end;
begin
  if RaiseList <> nil then
  begin
    Result := PRaiseFrame(RaiseList)^.ExceptObject;
    PRaiseFrame(RaiseList)^.ExceptObject := nil;
  end
  else
    Result := nil;
end;
{$ENDIF DELPHI5}

{$IFDEF DELPHI6}
var
  OrgWakeMainThread: TNotifyEvent;

class procedure TThread.WakeMainThread(Sender: TObject);
begin
  if Assigned(OrgWakeMainThread) then
    OrgWakeMainThread(Sender);
  SetEvent(SyncEvent);
end;

procedure HookWakeMainThread;
begin
  OrgWakeMainThread := Classes.WakeMainThread;
  Classes.WakeMainThread := TThread.WakeMainThread;
end;

procedure UnhookWakeMainThread;
begin
  Classes.WakeMainThread := OrgWakeMainThread;
end;
{$ENDIF DELPHI6}

type
  { TAsyncCallThread is a pooled thread. It looks itself for work. }
  TAsyncCallThread = class(TThread)
  protected
    FSuspended: Boolean;
    procedure Execute; override;
  public
    procedure ForceTerminate;
    procedure SuspendThread;
    procedure ResumeThread;

    property Suspended: Boolean read FSuspended;
  end;

  { TThreadPool contains a pool of threads that are either suspended or busy. }
  TThreadPool = class(TObject)
  private
    FMaxThreads: Integer;
    FThreads: TThreadList;
    FAsyncCalls: TThreadList;
    FNumberOfProcessors: Cardinal;

    FMainThreadSyncEvent: THandle;
    FMainThreadVclHandle: HWND;
    procedure MainThreadWndProc(var Msg: TMessage);
    procedure ProcessMainThreadSync;

    function AllocThread: TAsyncCallThread;
    function GetNextAsyncCall(Thread: TAsyncCallThread): TAsyncCall; // called from the threads
  public
    constructor Create;
    destructor Destroy; override;

    procedure SendVclSync(Call: TAsyncCall);

    procedure AddAsyncCall(Call: TAsyncCall);
    function RemoveAsyncCall(Call: TAsyncCall): Boolean;

    property MaxThreads: Integer read FMaxThreads;
    property NumberOfProcessors: Cardinal read FNumberOfProcessors;
    property MainThreadSyncEvent: THandle read FMainThreadSyncEvent;
  end;

{ ---------------------------------------------------------------------------- }

  TAsyncCallArgObject = class(TAsyncCall)
  private
    FProc: TAsyncCallArgObjectProc;
    FArg: TObject;
  protected
    function ExecuteAsyncCall: Integer; override;
  public
    constructor Create(AProc: TAsyncCallArgObjectProc; AArg: TObject);
  end;

  TAsyncCallArgString = class(TAsyncCall)
  private
    FProc: TAsyncCallArgStringProc;
    FArg: string;
  protected
    function ExecuteAsyncCall: Integer; override;
  public
    constructor Create(AProc: TAsyncCallArgStringProc; const AArg: string);
  end;

  TAsyncCallArgWideString = class(TAsyncCall)
  private
    FProc: TAsyncCallArgWideStringProc;
    FArg: WideString;
  protected
    function ExecuteAsyncCall: Integer; override;
  public
    constructor Create(AProc: TAsyncCallArgWideStringProc; const AArg: WideString);
  end;

  TAsyncCallArgInterface = class(TAsyncCall)
  private
    FProc: TAsyncCallArgInterfaceProc;
    FArg: IInterface;
  protected
    function ExecuteAsyncCall: Integer; override;
  public
    constructor Create(AProc: TAsyncCallArgInterfaceProc; const AArg: IInterface);
  end;

  TAsyncCallArgExtended = class(TAsyncCall)
  private
    FProc: TAsyncCallArgExtendedProc;
    FArg: Extended;
  protected
    function ExecuteAsyncCall: Integer; override;
  public
    constructor Create(AProc: TAsyncCallArgExtendedProc; const AArg: Extended);
  end;

  TAsyncCallArgVariant = class(TAsyncCall)
  private
    FProc: TAsyncCallArgVariantProc;
    FArg: Variant;
  protected
    function ExecuteAsyncCall: Integer; override;
  public
    constructor Create(AProc: TAsyncCallArgVariantProc; const AArg: Variant);
  end;

{ ---------------------------------------------------------------------------- }

  TAsyncCallLocalProc = class(TAsyncCall)
  private
    FProc: TLocalAsyncProc;
    FBasePointer: Pointer;
  protected
    function ExecuteAsyncCall: Integer; override;
  public
    constructor Create(AProc: TLocalAsyncProc; ABasePointer: Pointer);
  end;

  TAsyncCallLocalProcEx = class(TAsyncCall)
  private
    FProc: TLocalAsyncProc;
    FBasePointer: Pointer;
    FParam: INT_PTR;
  protected
    function ExecuteAsyncCall: Integer; override;
  public
    constructor Create(AProc: TLocalAsyncProc; AParam: INT_PTR; ABasePointer: Pointer);
  end;

  TAsyncVclCallLocalProc = class(TAsyncCall)
  private
    FProc: TLocalVclProc;
    FBasePointer: Pointer;
    FParam: INT_PTR;
  protected
    function ExecuteAsyncCall: Integer; override;
  public
    constructor Create(AProc: TLocalVclProc; AParam: INT_PTR; ABasePointer: Pointer);
  end;

{ ---------------------------------------------------------------------------- }

  TAsyncCallMethodArgObject = class(TAsyncCall)
  private
    FProc: TAsyncCallArgObjectMethod;
    FArg: TObject;
  protected
    function ExecuteAsyncCall: Integer; override;
  public
    constructor Create(AProc: TAsyncCallArgObjectMethod; AArg: TObject);
  end;

  TAsyncCallMethodArgString = class(TAsyncCall)
  private
    FProc: TAsyncCallArgStringMethod;
    FArg: string;
  protected
    function ExecuteAsyncCall: Integer; override;
  public
    constructor Create(AProc: TAsyncCallArgStringMethod; const AArg: string);
  end;

  TAsyncCallMethodArgWideString = class(TAsyncCall)
  private
    FProc: TAsyncCallArgWideStringMethod;
    FArg: WideString;
  protected
    function ExecuteAsyncCall: Integer; override;
  public
    constructor Create(AProc: TAsyncCallArgWideStringMethod; const AArg: WideString);
  end;

  TAsyncCallMethodArgInterface = class(TAsyncCall)
  private
    FProc: TAsyncCallArgInterfaceMethod;
    FArg: IInterface;
  protected
    function ExecuteAsyncCall: Integer; override;
  public
    constructor Create(AProc: TAsyncCallArgInterfaceMethod; const AArg: IInterface);
  end;

  TAsyncCallMethodArgExtended = class(TAsyncCall)
  private
    FProc: TAsyncCallArgExtendedMethod;
    FArg: Extended;
  protected
    function ExecuteAsyncCall: Integer; override;
  public
    constructor Create(AProc: TAsyncCallArgExtendedMethod; const AArg: Extended);
  end;

  TAsyncCallMethodArgVariant = class(TAsyncCall)
  private
    FProc: TAsyncCallArgVariantMethod;
    FArg: Variant;
  protected
    function ExecuteAsyncCall: Integer; override;
  public
    constructor Create(AProc: TAsyncCallArgVariantMethod; const AArg: Variant);
  end;

{ ---------------------------------------------------------------------------- }

  TAsyncCallArgRecord = class(TAsyncCall)
  private
    FProc: TAsyncCallArgRecordProc;
    FArg: Pointer;
  protected
    function ExecuteAsyncCall: Integer; override;
  public
    constructor Create(AProc: TAsyncCallArgRecordProc; AArg: Pointer);
  end;

  TAsyncCallMethodArgRecord = class(TAsyncCall)
  private
    FProc: TAsyncCallArgRecordMethod;
    FArg: Pointer;
  protected
    function ExecuteAsyncCall: Integer; override;
  public
    constructor Create(AProc: TAsyncCallArgRecordMethod; AArg: Pointer);
  end;

  TAsyncCallArrayOfConst = class(TAsyncCall)
  private
    FProc: function: Integer register;
    FArgs: array of TVarRec;
  protected
    function CopyVarRec(const Data: TVarRec): TVarRec;
    function ExecuteAsyncCall: Integer; override;
  public
    constructor Create(AProc: Pointer; const AArgs: array of const); overload;
    constructor Create(AProc: Pointer; MethodData: TObject; const AArgs: array of const); overload;
    destructor Destroy; override;
  end;

{ ---------------------------------------------------------------------------- }
var
  ThreadPool: TThreadPool;

procedure SetMaxAsyncCallThreads(MaxThreads: Integer);
begin
  if MaxThreads >= 0 then
    ThreadPool.FMaxThreads := MaxThreads;
end;

function GetMaxAsyncCallThreads: Integer;
begin
  Result := ThreadPool.FMaxThreads
end;

{ ---------------------------------------------------------------------------- }

function AsyncCall(Proc: TAsyncCallArgObjectProc; Arg: TObject): IAsyncCall;
begin
  { Execute the function synchron if no thread pool exists }
  if ThreadPool.MaxThreads = 0 then
    Result := TSyncCall.Create(Proc(Arg))
  else
    Result := TAsyncCallArgObject.Create(Proc, Arg).ExecuteAsync;
end;

function AsyncCall(Proc: TAsyncCallArgIntegerProc; Arg: Integer): IAsyncCall;
begin
  Result := AsyncCall(TAsyncCallArgObjectProc(Proc), TObject(Arg));
end;

function AsyncCall(Proc: TAsyncCallArgStringProc; const Arg: string): IAsyncCall;
begin
  { Execute the function synchron if no thread pool exists }
  if ThreadPool.MaxThreads = 0 then
    Result := TSyncCall.Create(Proc(Arg))
  else
    Result := TAsyncCallArgString.Create(Proc, Arg).ExecuteAsync;
end;

function AsyncCall(Proc: TAsyncCallArgWideStringProc; const Arg: WideString): IAsyncCall;
begin
  { Execute the function synchron if no thread pool exists }
  if ThreadPool.MaxThreads = 0 then
    Result := TSyncCall.Create(Proc(Arg))
  else
    Result := TAsyncCallArgWideString.Create(Proc, Arg).ExecuteAsync;
end;

function AsyncCall(Proc: TAsyncCallArgInterfaceProc; const Arg: IInterface): IAsyncCall;
begin
  { Execute the function synchron if no thread pool exists }
  if ThreadPool.MaxThreads = 0 then
    Result := TSyncCall.Create(Proc(Arg))
  else
    Result := TAsyncCallArgInterface.Create(Proc, Arg).ExecuteAsync;
end;

function AsyncCall(Proc: TAsyncCallArgExtendedProc; const Arg: Extended): IAsyncCall;
begin
  { Execute the function synchron if no thread pool exists }
  if ThreadPool.MaxThreads = 0 then
    Result := TSyncCall.Create(Proc(Arg))
  else
    Result := TAsyncCallArgExtended.Create(Proc, Arg).ExecuteAsync;
end;

function AsyncCallVar(Proc: TAsyncCallArgVariantProc; const Arg: Variant): IAsyncCall;
begin
  { Execute the function synchron if no thread pool exists }
  if ThreadPool.MaxThreads = 0 then
    Result := TSyncCall.Create(Proc(Arg))
  else
    Result := TAsyncCallArgVariant.Create(Proc, Arg).ExecuteAsync;
end;

{ ---------------------------------------------------------------------------- }

function AsyncCall(Method: TAsyncCallArgObjectMethod; Arg: TObject): IAsyncCall;
begin
  { Execute the function synchron if no thread pool exists }
  if ThreadPool.MaxThreads = 0 then
    Result := TSyncCall.Create(Method(Arg))
  else
    Result := TAsyncCallMethodArgObject.Create(Method, Arg).ExecuteAsync;
end;

function AsyncCall(Method: TAsyncCallArgIntegerMethod; Arg: Integer): IAsyncCall;
begin
  Result := AsyncCall(TAsyncCallArgObjectMethod(Method), TObject(Arg));
end;

function AsyncCall(Method: TAsyncCallArgStringMethod; const Arg: string): IAsyncCall;
begin
  { Execute the function synchron if no thread pool exists }
  if ThreadPool.MaxThreads = 0 then
    Result := TSyncCall.Create(Method(Arg))
  else
    Result := TAsyncCallMethodArgString.Create(Method, Arg).ExecuteAsync;
end;

function AsyncCall(Method: TAsyncCallArgWideStringMethod; const Arg: WideString): IAsyncCall;
begin
  { Execute the function synchron if no thread pool exists }
  if ThreadPool.MaxThreads = 0 then
    Result := TSyncCall.Create(Method(Arg))
  else
    Result := TAsyncCallMethodArgWideString.Create(Method, Arg).ExecuteAsync;
end;

function AsyncCall(Method: TAsyncCallArgInterfaceMethod; const Arg: IInterface): IAsyncCall;
begin
  { Execute the function synchron if no thread pool exists }
  if ThreadPool.MaxThreads = 0 then
    Result := TSyncCall.Create(Method(Arg))
  else
    Result := TAsyncCallMethodArgInterface.Create(Method, Arg).ExecuteAsync;
end;

function AsyncCall(Method: TAsyncCallArgExtendedMethod; const Arg: Extended): IAsyncCall;
begin
  { Execute the function synchron if no thread pool exists }
  if ThreadPool.MaxThreads = 0 then
    Result := TSyncCall.Create(Method(Arg))
  else
    Result := TAsyncCallMethodArgExtended.Create(Method, Arg).ExecuteAsync;
end;

function AsyncCallVar(Method: TAsyncCallArgVariantMethod; const Arg: Variant): IAsyncCall;
begin
  { Execute the function synchron if no thread pool exists }
  if ThreadPool.MaxThreads = 0 then
    Result := TSyncCall.Create(Method(Arg))
  else
    Result := TAsyncCallMethodArgVariant.Create(Method, Arg).ExecuteAsync;
end;

{ ---------------------------------------------------------------------------- }

function AsyncCall(Method: TAsyncCallArgObjectEvent; Arg: TObject): IAsyncCall;
begin
  Result := AsyncCall(TAsyncCallArgObjectMethod(Method), Arg);
end;

function AsyncCall(Method: TAsyncCallArgIntegerEvent; Arg: Integer): IAsyncCall;
begin
  Result := AsyncCall(TAsyncCallArgIntegerMethod(Method), Arg);
end;

function AsyncCall(Method: TAsyncCallArgStringEvent; const Arg: string): IAsyncCall;
begin
  Result := AsyncCall(TAsyncCallArgStringMethod(Method), Arg);
end;

function AsyncCall(Method: TAsyncCallArgWideStringEvent; const Arg: WideString): IAsyncCall;
begin
  Result := AsyncCall(TAsyncCallArgWideStringMethod(Method), Arg);
end;

function AsyncCall(Method: TAsyncCallArgInterfaceEvent; const Arg: IInterface): IAsyncCall;
begin
  Result := AsyncCall(TAsyncCallArgInterfaceMethod(Method), Arg);
end;

function AsyncCall(Method: TAsyncCallArgExtendedEvent; const Arg: Extended): IAsyncCall;
begin
  Result := AsyncCall(TAsyncCallArgExtendedMethod(Method), Arg);
end;

function AsyncCallVar(Method: TAsyncCallArgVariantEvent; const Arg: Variant): IAsyncCall;
begin
  Result := AsyncCallVar(TAsyncCallArgVariantMethod(Method), Arg);
end;

function AsyncCallRunnable(const Arg: IInterface): Integer;
begin
  IAsyncRunnable(Arg).AsyncRun;
  Result := 0;
end;

function AsyncCall(Runnable: IAsyncRunnable): IAsyncCall;
begin
  Result := AsyncCall(AsyncCallRunnable, IInterface(Runnable));
end;

{ ---------------------------------------------------------------------------- }

procedure AsyncExec(Method: TNotifyEvent; Arg: TObject; IdleMsgMethod: TAsyncIdleMsgMethod);
var
  Handle: IAsyncCall;
begin
  Handle := AsyncCall(Method, Arg);
  if Assigned(IdleMsgMethod) then
  begin
    Handle.ForceDifferentThread;
    IdleMsgMethod;
    while MsgAsyncMultiSync([Handle], False, INFINITE, QS_ALLINPUT or QS_ALLPOSTMESSAGE) = 1 do
      IdleMsgMethod;
  end;
end;

{ ---------------------------------------------------------------------------- }
function InternLocalAsyncCall(LocalProc: TLocalAsyncProc; BasePointer: Pointer): IAsyncCall;
begin
  Result := TAsyncCallLocalProc.Create(LocalProc, BasePointer).ExecuteAsync;
end;

function LocalAsyncCall(LocalProc: TLocalAsyncProc): IAsyncCall;
asm
  mov ecx, edx // interface return address
  mov edx, ebp
  jmp InternLocalAsyncCall
end;

function InternLocalAsyncCallEx(LocalProc: TLocalAsyncProc; Param: INT_PTR; BasePointer: Pointer): IAsyncCall;
begin
  Result := TAsyncCallLocalProcEx.Create(LocalProc, Param, BasePointer).ExecuteAsync;
end;

function LocalAsyncCallEx(LocalProc: TLocalAsyncProcEx; Param: INT_PTR): IAsyncCall;
asm
  push ecx // interface return address
  mov ecx, ebp
  call InternLocalAsyncCallEx
end;

procedure InternLocalAsyncExec(LocalProc: TLocalAsyncProc; IdleMsgMethod: TAsyncIdleMsgMethod; BasePointer: Pointer);
var
  Handle: IAsyncCall;
begin
  Handle := TAsyncCallLocalProc.Create(LocalProc, BasePointer).ExecuteAsync;
  if Assigned(IdleMsgMethod) then
  begin
    Handle.ForceDifferentThread;
    IdleMsgMethod;
    while MsgAsyncMultiSync([Handle], False, INFINITE, QS_ALLINPUT or QS_ALLPOSTMESSAGE) = 1 do
      IdleMsgMethod;
  end;
end;

{$STACKFRAMES ON}
procedure LocalAsyncExec(Proc: TLocalAsyncProc; IdleMsgMethod: TAsyncIdleMsgMethod);
asm // TMethod causes the compiler to generate a stackframe
  pop ebp // remove stackframe
  mov edx, ebp
  jmp InternLocalAsyncExec
end;
{$STACKFRAMES OFF}

{ ---------------------------------------------------------------------------- }

function AsyncCallEx(Proc: TAsyncCallArgRecordProc; var Arg{: TRecordType}): IAsyncCall;
begin
  { Execute the function synchron if no thread pool exists }
  if ThreadPool.MaxThreads = 0 then
    Result := TSyncCall.Create(Proc(Arg))
  else
    Result := TAsyncCallArgRecord.Create(Proc, @Arg).ExecuteAsync;
end;

function AsyncCallEx(Method: TAsyncCallArgRecordMethod; var Arg{: TRecordType}): IAsyncCall;
begin
  { Execute the function synchron if no thread pool exists }
  if ThreadPool.MaxThreads = 0 then
    Result := TSyncCall.Create(Method(Arg))
  else
    Result := TAsyncCallMethodArgRecord.Create(Method, @Arg).ExecuteAsync;
end;

function AsyncCallEx(Method: TAsyncCallArgRecordEvent; var Arg{: TRecordType}): IAsyncCall;
begin
  Result := AsyncCallEx(TAsyncCallArgRecordMethod(Method), Arg);
end;

{ ---------------------------------------------------------------------------- }

function AsyncCall(Proc: TCdeclFunc; const Args: array of const): IAsyncCall; overload;
var
  Call: TAsyncCall;
begin
  Call := TAsyncCallArrayOfConst.Create(Proc, Args);
  if ThreadPool.MaxThreads = 0 then
    Call.InternExecuteSyncCall
  else
    Call.ExecuteAsync;
  Result := Call;
end;

function AsyncCall(Proc: TCdeclMethod; const Args: array of const): IAsyncCall; overload;
var
  Call: TAsyncCall;
begin
  Call := TAsyncCallArrayOfConst.Create(Proc.Code, TObject(Proc.Data), Args);
  if ThreadPool.MaxThreads = 0 then
    Call.InternExecuteSyncCall
  else
    Call.ExecuteAsync;
  Result := Call;
end;

{ ---------------------------------------------------------------------------- }

function WaitForSingleObjectMainThread(AHandle: THandle; Timeout: Cardinal): Cardinal;
var
  Handles: array[0..2] of THandle;
begin
  Handles[0] := AHandle;
  Handles[1] := SyncEvent;
  Handles[2] := ThreadPool.MainThreadSyncEvent;
  {$IFDEF DELPHI6}
  HookWakeMainThread;
  try
  {$ENDIF DELPHI6}
  repeat
    Result := WaitForMultipleObjects(3, @Handles[0], False, Timeout);
    if Result = WAIT_OBJECT_0 + 1 then
      CheckSynchronize
    else if Result = WAIT_OBJECT_0 + 2 then
      ThreadPool.ProcessMainThreadSync;
  until (Result <> WAIT_OBJECT_0 + 1) and (Result <> WAIT_OBJECT_0 + 2);
  {$IFDEF DELPHI6}
  finally
    UnhookWakeMainThread;
  end;
  {$ENDIF DELPHI6}
end;

function WaitForMultipleObjectsMainThread(Count: Cardinal;
  const AHandles: array of THandle; WaitAll: Boolean; Timeout: Cardinal;
  MsgWait: Boolean; dwWakeMask: DWORD): Cardinal;
var
  Handles: array of THandle;
  Index: Cardinal;
  FirstFinished, OriginalCount: Cardinal;
begin
  { Wait for the specified events, for the VCL SyncEvent and for the MainThreadSync event }
  OriginalCount := Count;
  SetLength(Handles, Count + 2);
  Move(AHandles[0], Handles[0], Count * SizeOf(THandle));
  Handles[Count] := SyncEvent;
  Handles[Count + 1] := ThreadPool.MainThreadSyncEvent;
  {$IFDEF DELPHI6}
  HookWakeMainThread;
  try
  {$ENDIF DELPHI6}
  if not WaitAll then
  begin
    repeat
      if MsgWait then
      begin
        Result := MsgWaitForMultipleObjects(Count + 2, Handles[0], WaitAll, Timeout, dwWakeMask);
        if Result = WAIT_OBJECT_0 + Count + 2 then
        begin
          ThreadPool.ProcessMainThreadSync; // also uses the message queue
          Result := WAIT_OBJECT_0 + OriginalCount; // caller doesn't know about the 2 synchronization events
          Exit;
        end;
      end
      else
        Result := WaitForMultipleObjects(Count + 2, @Handles[0], WaitAll, Timeout);

      if Result = WAIT_OBJECT_0 + Count then
        CheckSynchronize
      else if Result = WAIT_OBJECT_0 + Count + 1 then
        ThreadPool.ProcessMainThreadSync;
    until (Result <> WAIT_OBJECT_0 + Count) and (Result <> WAIT_OBJECT_0 + Count + 1);
  end
  else
  begin
    FirstFinished := WAIT_TIMEOUT;
    repeat
      if MsgWait then
      begin
        Result := MsgWaitForMultipleObjects(Count + 2, Handles[0], False, Timeout, dwWakeMask);
        if Result = WAIT_OBJECT_0 + Count + 2 then
        begin
          ThreadPool.ProcessMainThreadSync; // also uses the message queue
          Result := WAIT_OBJECT_0 + OriginalCount; // caller doesn't know about the 2 synchronization events
          Exit;
        end;
      end
      else
        Result := WaitForMultipleObjects(Count + 2, @Handles[0], False, Timeout);

      if Result = WAIT_OBJECT_0 + Count then
        CheckSynchronize
      else if Result = WAIT_OBJECT_0 + Count + 1 then
        ThreadPool.ProcessMainThreadSync
      else
      if {(Result >= WAIT_OBJECT_0) and} (Result <= WAIT_OBJECT_0 + Count) then
      begin
        if FirstFinished = WAIT_TIMEOUT then
          FirstFinished := Result;
        Dec(Count);
        if Count > 0 then
        begin
          Index := Result - WAIT_OBJECT_0;
          Move(Handles[Index + 1], Handles[Index], ((Count + 2) - Index) * SizeOf(THandle));
        end;
      end
      else
        Break;
    until Count = 0;
    if Count = 0 then
      Result := FirstFinished;
  end;
  {$IFDEF DELPHI6}
  finally
    UnhookWakeMainThread;
  end;
  {$ENDIF DELPHI6}
end;

{ ---------------------------------------------------------------------------- }

function InternalAsyncMultiSync(const List: array of IAsyncCall; const Handles: array of THandle;
  WaitAll: Boolean; Milliseconds: Cardinal; MsgWait: Boolean; dwWakeMask: DWORD): Cardinal;

  function InternalWait(const List: array of IAsyncCall; const Handles: array of THandle;
    WaitAll: Boolean; Milliseconds: Cardinal; MsgWait: Boolean; dwWakeMask: DWORD): Cardinal;
  var
    WaitHandles: array of THandle;
    Mapping: array of Integer;
    I: Integer;
    Count: Cardinal;
    EventIntf: IAsyncCallEx;
    SignalState: Cardinal;
  begin
    SetLength(WaitHandles, Length(List) + Length(Handles));
    SetLength(Mapping, Length(WaitHandles));
    Count := 0;
    { Get the TAsyncCall events }
    for I := 0 to High(List) do
    begin
      if (List[I] <> nil) and Supports(List[I], IAsyncCallEx, EventIntf) then
      begin
        WaitHandles[Count] := EventIntf.GetEvent;
        if WaitHandles[Count] <> 0 then
        begin
          Mapping[Count] := I;
          Inc(Count);
        end;
      end
      else
      if not WaitAll then
      begin
        { There are synchron calls in List[] and the caller does not want to
          wait for all handles. }
        Result := I;
        Exit;
      end;
    end;

    { Append other handles }
    for I := 0 to High(Handles) do
    begin
      WaitHandles[Count] := Handles[I];
      Mapping[Count] := Length(List) + I;
      Inc(Count);
    end;

    { Wait for the async calls }
    if Count > 0 then
    begin
      if GetCurrentThreadId = MainThreadID then
      begin
        SignalState := WaitForMultipleObjectsMainThread(Count, WaitHandles, WaitAll, Milliseconds, MsgWait, dwWakeMask);
        if SignalState = Count then // "message" was signaled
        begin
          Result := SignalState;
          Exit;
        end;
      end
      else
      begin
        if MsgWait then
        begin
          SignalState := MsgWaitForMultipleObjects(Count, WaitHandles[0], WaitAll, Milliseconds, dwWakeMask);
          if SignalState = Count then // "message" was signaled
          begin
            Result := SignalState;
            Exit;
          end;
        end
        else
          SignalState := WaitForMultipleObjects(Count, @WaitHandles[0], WaitAll, Milliseconds);
      end;
      if {(SignalState >= WAIT_OBJECT_0) and} (SignalState < WAIT_OBJECT_0 + Count) then
        Result := WAIT_OBJECT_0 + Mapping[SignalState - WAIT_OBJECT_0]
      else if (SignalState >= WAIT_ABANDONED_0) and (SignalState < WAIT_ABANDONED_0 + Count) then
        Result := WAIT_ABANDONED_0 + Mapping[SignalState - WAIT_ABANDONED_0]
      else
        Result := SignalState;
    end
    else
      Result := WAIT_OBJECT_0; // all AsyncCalls are already synchronized
  end;

  function InternalWaitAllInfinite(const List: array of IAsyncCall; const Handles: array of THandle): Cardinal;
  var
    I: Integer;
  begin
    { Wait for the async calls that aren't finished yet. }
    for I := 0 to High(List) do
      if List[I] <> nil then
        List[I].Sync;

    if Length(Handles) > 0 then
    begin
      if GetCurrentThreadId = MainThreadID then
        WaitForMultipleObjectsMainThread(Length(Handles), Handles, True, INFINITE, False, 0)
      else
        WaitForMultipleObjects(Length(Handles), @Handles[0], True, INFINITE);
    end;
    Result := WAIT_OBJECT_0;
  end;

var
  Count: Integer;
begin
  Count := Length(List) + Length(Handles);
  if (Count > 0) and (Count <= MAXIMUM_ASYNC_WAIT_OBJECTS) then
  begin
    if WaitAll and (Milliseconds = INFINITE) and not MsgWait and (GetCurrentThreadId <> MainThreadId) then
      Result := InternalWaitAllInfinite(List, Handles)
    else
      Result := InternalWait(List, Handles, WaitAll, Milliseconds, MsgWait, dwWakeMask);
  end
  else
    Result := WAIT_FAILED;
end;

function AsyncMultiSync(const List: array of IAsyncCall; WaitAll: Boolean;
  Milliseconds: Cardinal): Cardinal;
begin
  Result := InternalAsyncMultiSync(List, [], WaitAll, Milliseconds, False, 0);
end;

function AsyncMultiSyncEx(const List: array of IAsyncCall; const Handles: array of THandle;
  WaitAll: Boolean = True; Milliseconds: Cardinal = INFINITE): Cardinal;
begin
  Result := InternalAsyncMultiSync(List, Handles, WaitAll, Milliseconds, False, 0);
end;

function MsgAsyncMultiSync(const List: array of IAsyncCall; WaitAll: Boolean;
  Milliseconds: Cardinal; dwWakeMask: DWORD): Cardinal;
begin
  Result := InternalAsyncMultiSync(List, [], WaitAll, Milliseconds, True, dwWakeMask);
end;

function MsgAsyncMultiSyncEx(const List: array of IAsyncCall; const Handles: array of THandle;
  WaitAll: Boolean; Milliseconds: Cardinal; dwWakeMask: DWORD): Cardinal;
begin
  Result := InternalAsyncMultiSync(List, Handles, WaitAll, Milliseconds, True, dwWakeMask);
end;

procedure NotFinishedError(const FunctionName: string);
begin
  {$IFDEF DEBUG_ASYNCCALLS}
  if FunctionName <> '' then
    OutputDebugString(PChar(FunctionName));
  {$ENDIF DEBUG_ASYNCCALLS}
  raise EAsyncCallError.Create(RsAsyncCallNotFinished);
end;

procedure UnknownVarRecType(VType: Byte);
begin
  raise EAsyncCallError.CreateFmt(RsAsyncCallUnknownVarRecType, [VType]);
end;

{ ---------------------------------------------------------------------------- }
{ TAsyncCallThread }

function GetMainWnd(wnd: THandle; var MainWnd: THandle): LongBool; stdcall;
begin
  Result := False;
  MainWnd := wnd;
end;

procedure TAsyncCallThread.Execute;
var
  FAsyncCall: TAsyncCall;
  CoInitialized: Boolean;
begin
  CoInitialized := CoInitialize(nil) = S_OK;
  try
    while not Terminated do
    begin
      FAsyncCall := ThreadPool.GetNextAsyncCall(Self); // calls Suspend if nothing has to be done.
      if FAsyncCall <> nil then
      begin
        try
          FAsyncCall.InternExecuteAsyncCall;
        except
          {$IFDEF DEBUG_ASYNCCALLS}
          on E: Exception do
            OutputDebugString(PChar('[' + E.ClassName + '] ' + E.Message));
          {$ENDIF DEBUG_ASYNCCALLS}
        end;
      end;
    end;
  finally
    if CoInitialized then
      CoUninitialize;
  end;
end;

procedure TAsyncCallThread.ForceTerminate;
begin
  if Suspended then
  begin
    Terminate;
    ResumeThread;
  end
  else
    Terminate;
end;

procedure TAsyncCallThread.ResumeThread;
begin
  FSuspended := False;
  Windows.ResumeThread(Handle);
end;

procedure TAsyncCallThread.SuspendThread;
begin
  FSuspended := True;
  Windows.SuspendThread(Handle);
end;

{ ---------------------------------------------------------------------------- }
{ TThreadPool }

constructor TThreadPool.Create;
var
  SysInfo: TSystemInfo;
begin
  inherited Create;
  FThreads := TThreadList.Create;
  FAsyncCalls := TThreadList.Create;
  FMainThreadVclHandle := AllocateHWnd(MainThreadWndProc);
  FMainThreadSyncEvent := CreateEvent(nil, False, False, nil);

  GetSystemInfo(SysInfo);
  FNumberOfProcessors := SysInfo.dwNumberOfProcessors;
  FMaxThreads := SysInfo.dwNumberOfProcessors * 4 - 2 {main thread};
end;

destructor TThreadPool.Destroy;
var
  I: Integer;
  List: TList;
begin
  List := FThreads.LockList;
  for I := List.Count - 1 downto 0 do
    TAsyncCallThread(List[I]).ForceTerminate;
  FThreads.UnlockList;
  FThreads.Free;

  List := FAsyncCalls.LockList;
  for I := List.Count - 1 downto 0 do
    SetEvent(TAsyncCall(List[I]).FEvent);
  FAsyncCalls.UnlockList;
  FAsyncCalls.Free;

  CloseHandle(FMainThreadSyncEvent);
  DeallocateHWnd(FMainThreadVclHandle);

  inherited Destroy;
end;

function TThreadPool.GetNextAsyncCall(Thread: TAsyncCallThread): TAsyncCall;
var
  List: TList;
begin
  List := FAsyncCalls.LockList;
  try
    if List.Count > 0 then
    begin
      { Get the "oldest" async call }
      Result := List[0];
      List.Delete(0);
    end
    else
      Result := nil;
  finally
    FAsyncCalls.UnlockList;
  end;
  { Nothing to do, go sleeping... }
  if Result = nil then
    Thread.SuspendThread;
end;

function TThreadPool.RemoveAsyncCall(Call: TAsyncCall): Boolean;
var
  List: TList;
  Index: Integer;
begin
  List := FAsyncCalls.LockList;
  try
    Index := List.IndexOf(Call);
    Result := Index >= 0;
    if Result then
      List.Delete(Index);
  finally
    FAsyncCalls.UnlockList;
  end;
end;

procedure TThreadPool.AddAsyncCall(Call: TAsyncCall);
var
  List: TList;
  FreeThreadFound: Boolean;
  I: Integer;
begin
  List := FAsyncCalls.LockList;
  List.Add(Call);
  FAsyncCalls.UnlockList;

  FreeThreadFound := False;
  List := FThreads.LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      if TAsyncCallThread(List[I]).Suspended then
      begin
        { Wake up the thread so it can execute the waiting async call. }
        TAsyncCallThread(List[I]).ResumeThread;
        FreeThreadFound := True;
        Break;
      end;
    end;
    { All threads are busy, we need to allocate another thread if possible }
    if not FreeThreadFound and (List.Count < MaxThreads) then
      AllocThread;
  finally
    FThreads.UnlockList;
  end;
end;

function TThreadPool.AllocThread: TAsyncCallThread;
begin
  Result := TAsyncCallThread.Create(True);
  Result.FreeOnTerminate := True;
  FThreads.Add(Result);
  {$IFDEF DELPHI2010_UP}
  Result.Start;
  {$ELSE}
  Result.Resume;
  {$ENDIF DELPHI2010_UP}
end;

const
  WM_VCLSYNC = WM_USER + 12;

procedure TThreadPool.SendVclSync(Call: TAsyncCall);
begin
  if not PostMessage(FMainThreadVclHandle, WM_VCLSYNC, 0, LPARAM(Call)) then
    Call.Quit(0)
  else
    SetEvent(FMainThreadSyncEvent);
end;

procedure TThreadPool.MainThreadWndProc(var Msg: TMessage);
begin
  case Msg.Msg of
    WM_VCLSYNC:
      TAsyncCall(Msg.LParam).InternExecuteSyncCall;
  else
    with Msg do
      Result := DefWindowProc(FMainThreadVclHandle, Msg, WParam, LParam);
  end;
end;

procedure TThreadPool.ProcessMainThreadSync;
var
  Msg: TMsg;
begin
  Assert( GetCurrentThreadId = MainThreadId ); 
  while PeekMessage(Msg, FMainThreadVclHandle, 0, 0, PM_REMOVE) do
  begin
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end;
end;

{ ---------------------------------------------------------------------------- }
{ TSyncCall }

constructor TSyncCall.Create(AReturnValue: Integer);
begin
  inherited Create;
  FReturnValue := AReturnValue;
end;

function TSyncCall.Finished: Boolean;
begin
  Result := True;
end;

procedure TSyncCall.ForceDifferentThread;
begin
end;

function TSyncCall.ReturnValue: Integer;
begin
  Result := FReturnValue;
end;

function TSyncCall.Sync: Integer;
begin
  Result := FReturnValue;
end;

{ ---------------------------------------------------------------------------- }
{ TAsyncCall }

constructor TAsyncCall.Create;
begin
  inherited Create;
  FEvent := CreateEvent(nil, True, False, nil);
end;

destructor TAsyncCall.Destroy; 
begin
  if FEvent <> 0 then
  begin
    try
      Sync;
    finally
      CloseHandle(FEvent);
      FEvent := 0;
    end;
  end;
  inherited Destroy;
end;

function TAsyncCall._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
  begin
    try
      if FEvent <> 0 then
        Sync;
    finally
      Destroy;
    end;
  end;
end;

function TAsyncCall.Finished: Boolean;
begin
  Result := (FEvent = 0) or FFinished or (WaitForSingleObject(FEvent, 0) = WAIT_OBJECT_0);
end;

procedure TAsyncCall.ForceDifferentThread;
begin
  FForceDifferentThread := True;
end;

function TAsyncCall.GetEvent: Cardinal;
begin
  Result := FEvent;
end;

procedure TAsyncCall.InternExecuteAsyncCall;
var
  Value: Integer;
begin
  Value := 0;
  try
    Value := ExecuteAsyncCall;
  except
    FFatalErrorAddr := ErrorAddr;
    FFatalException := AcquireExceptionObject;
  end;
  Quit(Value);
end;

procedure TAsyncCall.InternExecuteSyncCall;
begin
  Quit( ExecuteAsyncCall() );
end;

procedure TAsyncCall.Quit(AReturnValue: Integer);
begin
  FReturnValue := AReturnValue;
  FFinished := True;
  SetEvent(FEvent);
end;

function TAsyncCall.ReturnValue: Integer;
var
  E: Exception;
begin
  if not Finished then
    NotFinishedError('IAsyncCall.ReturnValue');
  Result := FReturnValue;

  if FFatalException <> nil then
  begin
    E := FFatalException;
    FFatalException := nil;
    raise E at FFatalErrorAddr;
  end;
end;

function TAsyncCall.Sync: Integer;
var
  E: Exception;
begin
  if not Finished then
  begin
    if not SyncInThisThreadIfPossible then
    begin
      if GetCurrentThreadId = MainThreadID then
      begin
        if WaitForSingleObjectMainThread(FEvent, INFINITE) <> WAIT_OBJECT_0 then
          NotFinishedError('IAsyncCall.Sync');
      end
      else
      if WaitForSingleObject(FEvent, INFINITE) <> WAIT_OBJECT_0 then
        NotFinishedError('IAsyncCall.Sync');
    end;
  end;
  Result := FReturnValue;

  if FFatalException <> nil then
  begin
    E := FFatalException;
    FFatalException := nil;
    raise E at FFatalErrorAddr;
  end;
end;

function TAsyncCall.SyncInThisThreadIfPossible: Boolean;
begin
  if not Finished then
  begin
    Result := False;
    if not FForceDifferentThread then
    begin
      { If no thread was assigned to this async call, remove it form the waiting
        queue and execute it in the current thread. }
      if ThreadPool.RemoveAsyncCall(Self) then
      begin
        InternExecuteSyncCall;
        Result := True;
      end;
    end;
  end
  else
    Result := True;
end;

function TAsyncCall.ExecuteAsync: TAsyncCall;
begin
  ThreadPool.AddAsyncCall(Self);
  Result := Self;
end;

{ ---------------------------------------------------------------------------- }
{ TAsyncCallArrayOfConst }

constructor TAsyncCallArrayOfConst.Create(AProc: Pointer; const AArgs: array of const);
var
  I: Integer;
begin
  inherited Create;
  FProc := AProc;
  SetLength(FArgs, Length(AArgs));
  for I := 0 to High(AArgs) do
    FArgs[I] := CopyVarRec(AArgs[I]);
end;

constructor TAsyncCallArrayOfConst.Create(AProc: Pointer; MethodData: TObject; const AArgs: array of const);
var
  I: Integer;
begin
  inherited Create;
  FProc := AProc;
  SetLength(FArgs, 1 + Length(AArgs));

  // insert "Self"
  FArgs[0].VType := vtObject;
  FArgs[0].VObject := MethodData;

  for I := 0 to High(AArgs) do
    FArgs[I + 1] := CopyVarRec(AArgs[I]);
end;

destructor TAsyncCallArrayOfConst.Destroy;
var
  I: Integer;
  V: PVarRec;
begin
  for I := 0 to High(FArgs) do
  begin
    V := @FArgs[I];
    case V.VType of
      vtAnsiString: AnsiString(V.VAnsiString) := '';
      vtWideString: WideString(V.VWideString) := '';
      {$IFDEF UNICODE}
      vtUnicodeString: UnicodeString(V.VUnicodeString) := '';
      {$ENDIF UNICODE}
      vtInterface : IInterface(V.VInterface) := nil;

      vtString    : Dispose(V.VString);
      vtExtended  : Dispose(V.VExtended);
      vtCurrency  : Dispose(V.VCurrency);
      vtInt64     : Dispose(V.VInt64);
      vtVariant   : Dispose(V.VVariant);
    end;
  end;
  inherited Destroy;
end;

function TAsyncCallArrayOfConst.CopyVarRec(const Data: TVarRec): TVarRec;
begin
  if (Data.VPointer <> nil) and
     (Data.VType in [vtString, vtAnsiString, vtWideString,
                     {$IFDEF UNICODE}vtUnicodeString,{$ENDIF} vtExtended,
                     vtCurrency, vtInt64, vtVariant, vtInterface]) then
  begin
    Result.VType := Data.VType;
    Result.VPointer := nil;
    { Copy and redirect TVarRec data to prevent conflicts with other threads,
      especially the calling thread. Otherwise reference counted types could
      be freed while this asynchron function is still executed. }
    case Result.VType of
      vtAnsiString: AnsiString(Result.VAnsiString) := AnsiString(Data.VAnsiString);
      vtWideString: WideString(Result.VWideString) := WideString(Data.VWideString);
      {$IFDEF UNICODE}
      vtUnicodeString: UnicodeString(Result.VUnicodeString) := UnicodeString(data.VUnicodeString);
      {$ENDIF UNICODE}
      vtInterface : IInterface(Result.VInterface) := IInterface(Data.VInterface);

      vtString    : begin New(Result.VString);   Result.VString^ := Data.VString^; end;
      vtExtended  : begin New(Result.VExtended); Result.VExtended^ := Data.VExtended^; end;
      vtCurrency  : begin New(Result.VCurrency); Result.VCurrency^ := Data.VCurrency^; end;
      vtInt64     : begin New(Result.VInt64);    Result.VInt64^ := Data.VInt64^; end;
      vtVariant   : begin New(Result.VVariant);  Result.VVariant^ := Data.VVariant^; end;
    end;
  end
  else
    Result := Data;
end;

function TAsyncCallArrayOfConst.ExecuteAsyncCall: Integer;
var
  I: Integer;
  V: ^TVarRec;
  ByteCount: Integer;
begin
  ByteCount := Length(FArgs) * SizeOf(Integer) + $40;
  { Create a zero filled buffer for functions that want more arguments than
    specified. }
  asm
    xor eax, eax
    mov ecx, $40 / 8
@@FillBuf:
    push eax
    push eax
    dec ecx
    jnz @@FillBuf
  end;

  for I := High(FArgs) downto 0 do // cdecl => right to left
  begin
    V := @FArgs[I];
    case V.VType of
      vtInteger:     // [const] Arg: Integer
        asm
          mov eax, V
          push [eax].TVarRec.VInteger
        end;

      vtBoolean,     // [const] Arg: Boolean
      vtChar:        // [const] Arg: AnsiChar
        asm
          mov eax, V
          xor edx, edx
          mov dl, [eax].TVarRec.VBoolean
          push edx
        end;

      vtWideChar:    // [const] Arg: WideChar
        asm
          mov eax, V
          xor edx, edx
          mov dx, [eax].TVarRec.VWideChar
          push edx
        end;

      vtExtended:    // [const] Arg: Extended
        asm
          add [ByteCount], 8 // two additional DWORDs
          mov eax, V
          mov edx, [eax].TVarRec.VExtended
          movzx eax, WORD PTR [edx + 8]
          push eax
          push DWORD PTR [edx + 4]
          push DWORD PTR [edx]
        end;

      vtCurrency,    // [const] Arg: Currency
      vtInt64:       // [const] Arg: Int64
        asm
          add [ByteCount], 4 // an additional DWORD
          mov eax, V
          mov edx, [eax].TVarRec.VCurrency
          push DWORD PTR [edx + 4]
          push DWORD PTR [edx]
        end;

      vtString,      // [const] Arg: ShortString
      vtPointer,     // [const] Arg: Pointer
      vtPChar,       // [const] Arg: PChar
      vtObject,      // [const] Arg: TObject
      vtClass,       // [const] Arg: TClass
      vtAnsiString,  // [const] Arg: AnsiString
      {$IFDEF UNICODE}
      vtUnicodeString, // [const] Arg: UnicodeString
      {$ENDIF UNICODE}
      vtPWideChar,   // [const] Arg: PWideChar
      vtVariant,     // const Arg: Variant
      vtInterface,   // [const]: IInterface
      vtWideString:  // [const] Arg: WideString
        asm
          mov eax, V
          push [eax].TVarRec.VPointer
        end;
    else
      UnknownVarRecType(V.VType);
    end;
  end;

  Result := FProc;

  asm // cdecl => we must clean up
    add esp, [ByteCount]
  end;
end;

{ ---------------------------------------------------------------------------- }
{ TAsyncCallArgRecord }

constructor TAsyncCallArgRecord.Create(AProc: TAsyncCallArgRecordProc; AArg: Pointer);
begin
  inherited Create;
  FProc := AProc;
  FArg := AArg;
end;

function TAsyncCallArgRecord.ExecuteAsyncCall: Integer;
begin
  Result := FProc(FArg^);
end;

{ ---------------------------------------------------------------------------- }
{ TAsyncCallMethodArgRecord }

constructor TAsyncCallMethodArgRecord.Create(AProc: TAsyncCallArgRecordMethod; AArg: Pointer);
begin
  inherited Create;
  FProc := AProc;
  FArg := AArg;
end;

function TAsyncCallMethodArgRecord.ExecuteAsyncCall: Integer;
begin
  Result := FProc(FArg^);
end;

{ ---------------------------------------------------------------------------- }
{ TAsyncCallArgObject }

constructor TAsyncCallArgObject.Create(AProc: TAsyncCallArgObjectProc; AArg: TObject);
begin
  inherited Create;
  FProc := AProc;
  FArg := AArg;
end;

function TAsyncCallArgObject.ExecuteAsyncCall: Integer;
begin
  Result := FProc(FArg);
end;

{ ---------------------------------------------------------------------------- }
{ TAsyncCallMethodArgObject }

constructor TAsyncCallMethodArgObject.Create(AProc: TAsyncCallArgObjectMethod; AArg: TObject);
begin
  inherited Create;
  FProc := AProc;
  FArg := AArg;
end;

function TAsyncCallMethodArgObject.ExecuteAsyncCall: Integer;
begin
  Result := FProc(FArg);
end;

{ ---------------------------------------------------------------------------- }
{ TAsyncCallArgString }

constructor TAsyncCallArgString.Create(AProc: TAsyncCallArgStringProc;
  const AArg: string);
begin
  inherited Create;
  FProc := AProc;
  FArg := AArg;
end;

function TAsyncCallArgString.ExecuteAsyncCall: Integer;
begin
  Result := FProc(FArg);
end;

{ ---------------------------------------------------------------------------- }
{ TAsyncCallMethodArgString }

constructor TAsyncCallMethodArgString.Create(AProc: TAsyncCallArgStringMethod; const AArg: string);
begin
  inherited Create;
  FProc := AProc;
  FArg := AArg;
end;

function TAsyncCallMethodArgString.ExecuteAsyncCall: Integer;
begin
  Result := FProc(FArg);
end;

{ ---------------------------------------------------------------------------- }
{ TAsyncCallArgWideString }

constructor TAsyncCallArgWideString.Create(AProc: TAsyncCallArgWideStringProc; const AArg: WideString);
begin
  inherited Create;
  FProc := AProc;
  FArg := AArg;
end;

function TAsyncCallArgWideString.ExecuteAsyncCall: Integer;
begin
  Result := FProc(FArg);
end;

{ ---------------------------------------------------------------------------- }
{ TAsyncCallMethodArgWideString }

constructor TAsyncCallMethodArgWideString.Create(AProc: TAsyncCallArgWideStringMethod; const AArg: WideString);
begin
  inherited Create;
  FProc := AProc;
  FArg := AArg;
end;

function TAsyncCallMethodArgWideString.ExecuteAsyncCall: Integer;
begin
  Result := FProc(FArg);
end;

{ ---------------------------------------------------------------------------- }
{ TAsyncCallArgInterface }

constructor TAsyncCallArgInterface.Create(AProc: TAsyncCallArgInterfaceProc; const AArg: IInterface);
begin
  inherited Create;
  FProc := AProc;
  FArg := AArg;
end;

function TAsyncCallArgInterface.ExecuteAsyncCall: Integer;
begin
  Result := FProc(FArg);
end;

{ ---------------------------------------------------------------------------- }
{ TAsyncCallMethodArgInterface }

constructor TAsyncCallMethodArgInterface.Create(AProc: TAsyncCallArgInterfaceMethod; const AArg: IInterface);
begin
  inherited Create;
  FProc := AProc;
  FArg := AArg;
end;

function TAsyncCallMethodArgInterface.ExecuteAsyncCall: Integer;
begin
  Result := FProc(FArg);
end;

{ ---------------------------------------------------------------------------- }
{ TAsyncCallArgExtended }

constructor TAsyncCallArgExtended.Create(AProc: TAsyncCallArgExtendedProc; const AArg: Extended);
begin
  inherited Create;
  FProc := AProc;
  FArg := AArg;
end;

function TAsyncCallArgExtended.ExecuteAsyncCall: Integer;
begin
  Result := FProc(FArg);
end;

{ ---------------------------------------------------------------------------- }
{ TAsyncCallMethodArgExtended }

constructor TAsyncCallMethodArgExtended.Create(AProc: TAsyncCallArgExtendedMethod; const AArg: Extended);
begin
  inherited Create;
  FProc := AProc;
  FArg := AArg;
end;

function TAsyncCallMethodArgExtended.ExecuteAsyncCall: Integer;
begin
  Result := FProc(FArg);
end;

{ ---------------------------------------------------------------------------- }
{ TAsyncCallArgVariant }

constructor TAsyncCallArgVariant.Create(AProc: TAsyncCallArgVariantProc; const AArg: Variant);
begin
  inherited Create;
  FProc := AProc;
  FArg := AArg;
end;

function TAsyncCallArgVariant.ExecuteAsyncCall: Integer;
begin
  Result := FProc(FArg);
end;

{ ---------------------------------------------------------------------------- }
{ TAsyncCallMethodArgVariant }

constructor TAsyncCallMethodArgVariant.Create(AProc: TAsyncCallArgVariantMethod; const AArg: Variant);
begin
  inherited Create;
  FProc := AProc;
  FArg := AArg;
end;

function TAsyncCallMethodArgVariant.ExecuteAsyncCall: Integer;
begin
  Result := FProc(FArg);
end;

{ ---------------------------------------------------------------------------- }

{ TAsyncCallLocalProc }

constructor TAsyncCallLocalProc.Create(AProc: TLocalAsyncProc; ABasePointer: Pointer);
begin
  inherited Create;
  @FProc := @AProc;
  FBasePointer := ABasePointer;
end;

function TAsyncCallLocalProc.ExecuteAsyncCall: Integer;
asm
  mov edx, [eax].TAsyncCallLocalProc.FBasePointer
  mov ecx, [eax].TAsyncCallLocalProc.FProc
  xor eax, eax // paramater
  push edx
  call ecx
  pop ecx
end;

{ TAsyncCallLocalProcEx }

constructor TAsyncCallLocalProcEx.Create(AProc: TLocalAsyncProc; AParam: INT_PTR; ABasePointer: Pointer);
begin
  inherited Create;
  @FProc := @AProc;
  FBasePointer := ABasePointer;
  FParam := AParam;
end;

function TAsyncCallLocalProcEx.ExecuteAsyncCall: Integer;
asm
  mov edx, [eax].TAsyncCallLocalProcEx.FBasePointer
  mov ecx, [eax].TAsyncCallLocalProcEx.FProc
  mov eax, [eax].TAsyncCallLocalProcEx.FParam
  push edx
  call ecx
  pop ecx
end;

{ ---------------------------------------------------------------------------- }

{ TAsyncVclCallLocalProc }

constructor TAsyncVclCallLocalProc.Create(AProc: TLocalVclProc; AParam: INT_PTR; ABasePointer: Pointer);
begin
  inherited Create;
  @FProc := @AProc;
  FBasePointer := ABasePointer;
  FParam := AParam;
end;

function TAsyncVclCallLocalProc.ExecuteAsyncCall: Integer;
asm
  mov edx, [eax].TAsyncCallLocalProcEx.FBasePointer
  mov ecx, [eax].TAsyncCallLocalProcEx.FProc
  mov eax, [eax].TAsyncCallLocalProcEx.FParam
  push edx
  call ecx
  pop ecx
end;

{ ---------------------------------------------------------------------------- }

type
  PLocalVclCallRec = ^TLocalVclCallRec;
  TLocalVclCallRec = record
    BasePointer: Pointer;
    Proc: TLocalVclProc;
    Param: INT_PTR;
  end;

function LocalVclCallProc(Data: PLocalVclCallRec): Integer;
asm
  mov edx, [eax].TLocalVclCallRec.BasePointer
  mov ecx, [eax].TLocalVclCallRec.Proc
  mov eax, [eax].TLocalVclCallRec.Param
  push edx
  call ecx
  pop ecx 
end;

procedure InternLocalVclCall(LocalProc: TLocalVclProc; Param: INT_PTR; BasePointer: Pointer);
var
  M: TMethod;
  Data: TLocalVclCallRec;
begin
  Data.BasePointer := BasePointer;
  Data.Proc := LocalProc;
  Data.Param := Param;
  if GetCurrentThreadId = MainThreadID then
    LocalVclCallProc(@Data)
  else
  begin
    M.Code := @LocalVclCallProc;
    M.Data := @Data;
    StaticSynchronize(TThreadMethod(M));
  end;
end;

procedure LocalVclCall(LocalProc: TLocalVclProc; Param: INT_PTR);
asm
  mov ecx, ebp
  jmp InternLocalVclCall
end;

function InternLocalAsyncVclCall(LocalProc: TLocalVclProc; Param: INT_PTR; BasePointer: Pointer): IAsyncCall;
var
  Data: TLocalVclCallRec;
  Call: TAsyncVclCallLocalProc;
begin
  if GetCurrentThreadId = MainThreadID then
  begin
    Data.BasePointer := BasePointer;
    Data.Proc := LocalProc;
    Data.Param := Param;
    Result := TSyncCall.Create( LocalVclCallProc(@Data) );
  end
  else
  begin
    Call := TAsyncVclCallLocalProc.Create(LocalProc, Param, BasePointer);
    ThreadPool.SendVclSync(Call);
    Result := Call;
  end;
end;

function LocalAsyncVclCall(LocalProc: TLocalVclProc; Param: INT_PTR = 0): IAsyncCall;
asm
  push ecx // interface return address
  mov ecx, ebp
  call InternLocalAsyncVclCall
end;

{----------------------------------------------------------------------------}

type
  TMainThreadContext = record
    MainThreadEntered: Longint;
    MainThreadOpenBlockCount: Longint;

    IntructionPointer: Pointer;
    BasePointer: Pointer;
    RetAddr: Pointer;

    MainBasePointer: Pointer;
    MainStackPointerStart: Pointer;
    ContextRetAddr: Pointer;

    MainRegEBX, MainRegEDI, MainRegESI: Pointer;
    ThreadRegEBX, ThreadRegEDI, ThreadRegESI: Pointer;

    StackBufferCount: Longint;
    StackBuffer: array of Pointer;
  end;

var
  MainThreadContext: TMainThreadContext;
  MainThreadContextCritSect: TRTLCriticalSection;

procedure ExecuteInMainThread(Data: TObject);
asm
  push ebp

  mov eax, OFFSET MainThreadContext

  { Backup main thread state }
  mov edx, OFFSET @@Leave
  mov [eax].TMainThreadContext.ContextRetAddr, edx
  mov [eax].TMainThreadContext.MainBasePointer, ebp
  mov [eax].TMainThreadContext.MainStackPointerStart, esp

  { Backup main thread registers }
  mov [eax].TMainThreadContext.MainRegEBX, ebx
  mov [eax].TMainThreadContext.MainRegEDI, edi
  mov [eax].TMainThreadContext.MainRegESI, esi

  { Set "nested call" control }
  mov ecx, [eax].TMainThreadContext.MainThreadOpenBlockCount
  mov [eax].TMainThreadContext.MainThreadEntered, ecx
  inc ecx
  mov [eax].TMainThreadContext.MainThreadOpenBlockCount, ecx

  { Switch to the thread state }
  mov ebp, [eax].TMainThreadContext.BasePointer
  mov edx, [eax].TMainThreadContext.IntructionPointer

  { Swicth to the thread registers }
  mov ebx, [eax].TMainThreadContext.ThreadRegEBX
  mov edi, [eax].TMainThreadContext.ThreadRegEDI
  mov esi, [eax].TMainThreadContext.ThreadRegESI

  { Jump to the user's synchronized code }
  jmp edx

  { LeaveMainThread() will jump to this address after it has restored the main
    thread state. }
@@Leave:
  pop ebp
end;

procedure LeaveMainThreadError(ErrorMode: Integer);
begin
  case ErrorMode of
    0: raise Exception.Create(RsLeaveMainThreadNestedError);
    1: raise Exception.Create(RsLeaveMainThreadThreadError);
  end;
end;

function InitStackBuffer(Count: Integer): Pointer;
begin
  MainThreadContext.StackBufferCount := Count;
  SetLength(MainThreadContext.StackBuffer, Count);
  if Count > 0 then
    Result := @MainThreadContext.StackBuffer[0]
  else
    Result := nil;
end;

function GetMainThreadId: LongWord;
begin
  Result := MainThreadId;
end;

procedure LeaveMainThread;
asm
  { Check if we are in the main thread }
  call GetCurrentThreadId
  mov ecx, eax
  call GetMainThreadId
  cmp eax, ecx
  jne @@ThreadError

  { "nested call" control }
  mov eax, OFFSET MainThreadContext
  mov ecx, [eax].TMainThreadContext.MainThreadOpenBlockCount
  dec ecx
  js @@NestedError
  mov [eax].TMainThreadContext.MainThreadOpenBlockCount, ecx
  cmp ecx, [eax].TMainThreadContext.MainThreadEntered
  jne @@Leave
  { Release "nested call" control }
  mov [eax].TMainThreadContext.MainThreadEntered, -1

  { Save the current registers for the return, the compiler might have
    generated code that changed the registers in the synchronized code. }
  mov [eax].TMainThreadContext.ThreadRegEBX, ebx
  mov [eax].TMainThreadContext.ThreadRegEDI, edi
  mov [eax].TMainThreadContext.ThreadRegESI, esi
  { Restore main thread registers }
  mov ebx, [eax].TMainThreadContext.MainRegEBX
  mov edi, [eax].TMainThreadContext.MainRegEDI
  mov esi, [eax].TMainThreadContext.MainRegESI

  { Detect if the finally block is called by System._HandleFinally.
    In that case an exception was raised in the MainThread-Block. The
    Classes.CheckSynchronize function will handle the exception and the
    thread switch for us. This will also restore the EBP regíster. }
  mov eax, [esp + $04] // finally return address
  mov edx, OFFSET System.@HandleFinally
  cmp eax, edx
  jl @@NoException
  mov edx, OFFSET System.@HandleAutoException
  cmp eax, edx
  jl @@InException
@@NoException:

  { Backup the return addresses }
  pop edx // procedure return address

  mov eax, OFFSET MainThreadContext
  mov [eax].TMainThreadContext.RetAddr, edx

  { Pop all items from the stack that are between ESP and MainStackPointerStart
    to an internal buffer that is pushed back on the stack in the
    "EnterMainThread" leave-code. }
  mov edx, [eax].TMainThreadContext.MainStackPointerStart
  mov eax, edx
  sub eax, esp
  shr eax, 2 // todo: adjust for 64Bit
  push edx // MainStackPointerStart => Stack
  push eax // Stack item count => Stack

  call InitStackBuffer // returns EAX=Pointer to first item

  pop ecx // Stack item count <= Stack
  pop edx // MainStackPointerStart <= Stack
  // copy stack
  or ecx, ecx
  jz @@IgnoreCopyStackLoop
  mov edx, eax
@@CopyStackLoop:
  pop eax
  mov [edx], eax
  add edx, 4
  dec ecx
  jnz @@CopyStackLoop
@@IgnoreCopyStackLoop:

  { Restore the main thread state }
  mov eax, OFFSET MainThreadContext
  mov ebp, [eax].TMainThreadContext.MainBasePointer
  mov edx, [eax].TMainThreadContext.ContextRetAddr
  //mov esp, [eax].TMainThreadContext.MainStackPointerStart // fixes stack pointer
  jmp edx

@@NestedError:
  xor eax, eax
  call LeaveMainThreadError
@@ThreadError:
  mov eax, 1
  call LeaveMainThreadError

@@InException:
@@Leave:
end;

procedure EnterMainThread;
asm
  { There is nothing to do if we are already in the main thread }
  call GetCurrentThreadId
  mov ecx, eax
  call GetMainThreadId
  cmp eax, ecx
  je @@InMainThread

  { Enter critical section => implicit waiting queue }
  mov eax, OFFSET MainThreadContextCritSect
  push eax
  call EnterCriticalSection

  { Take the return address from the stack to "clean" the stack }
  pop edx

  { Backup the current thread state }
  mov eax, OFFSET MainThreadContext
  mov [eax].TMainThreadContext.MainThreadEntered, ecx
  mov [eax].TMainThreadContext.IntructionPointer, edx
  mov [eax].TMainThreadContext.BasePointer, ebp
  { Backup the current thread registers }
  mov [eax].TMainThreadContext.ThreadRegEBX, ebx
  mov [eax].TMainThreadContext.ThreadRegEDI, edi
  mov [eax].TMainThreadContext.ThreadRegESI, esi

  { Begin try/finally }
@@Try:
  xor eax, eax
  push ebp
  push OFFSET @@HandleFinally
  push dword ptr fs:[eax]
  mov fs:[eax], esp

  { Call Synchronize(TMethod(ExecuteInMainThread)) }
  xor edx, edx
  push edx
  mov ecx, OFFSET ExecuteInMainThread
  push ecx
  call StaticSynchronize

  { Clean up try/finally }
  xor eax,eax
  pop edx
  pop ecx
  pop ecx
  mov fs:[eax], edx

  { Restore thread state }
  mov eax, OFFSET MainThreadContext
  mov ebp, [eax].TMainThreadContext.BasePointer

  { Push the backuped stack items back to the stack }
  mov ecx, [eax].TMainThreadContext.StackBufferCount
  dec ecx
  js @@IgnoreRestoreStack
  mov eax, [eax].TMainThreadContext.StackBuffer
  mov edx, ecx
  shl edx, 2 // todo: Adjust for 64 bit
  add eax, edx // move to buffer end
@@RestoreStack:
  mov edx, [eax]
  add eax, -4
  push edx
  dec ecx
  jns @@RestoreStack
@@IgnoreRestoreStack:

  { Put return address back to the stack }
  mov eax, OFFSET MainThreadContext
  mov edx, [eax].TMainThreadContext.RetAddr
  push edx

  { End try/finally }
@@Finally:
  { Restore thread registers } 
  mov eax, OFFSET MainThreadContext
  mov ebx, [eax].TMainThreadContext.ThreadRegEBX
  mov edi, [eax].TMainThreadContext.ThreadRegEDI
  mov esi, [eax].TMainThreadContext.ThreadRegESI

  { Leave critical section }
  mov eax, OFFSET MainThreadContextCritSect
  push eax
  call LeaveCriticalSection
  ret
@@HandleFinally:
  jmp System.@HandleFinally
  jmp @@Finally
@@LeaveFinally:
  ret

@@InMainThread:
  { Adjust "nested call" control.
    Threadsafe because we are in the main thread and only the main thread
    manipulates MainThreadOpenBlockCount }
  inc [MainThreadContext].TMainThreadContext.MainThreadOpenBlockCount
end;

{----------------------------------------------------------------------------}

{$IFDEF DELPHI2009_UP}
{ TMultiArgProcCall<TProc, T1> }
constructor TMultiArgProcCall<TProc, T1>.Create(AProc: TProc; const AArg1: T1);
begin
  inherited Create;
  FProc := AProc;
  FArg1 := AArg1;
end;

{ TMultiArgProcCall<TProc, T1, T2> }
constructor TMultiArgProcCall<TProc, T1, T2>.Create(AProc: TProc; const AArg1: T1; const AArg2: T2);
begin
  inherited Create(AProc, AArg1);
  FArg2 := AArg2;
end;

{ TMultiArgProcCall<TProc, T1, T2, T3> }
constructor TMultiArgProcCall<TProc, T1, T2, T3>.Create(AProc: TProc; const AArg1: T1; const AArg2: T2; const AArg3: T3);
begin
  inherited Create(AProc, AArg1, AArg2);
  FArg3 := AArg3;
end;

{ TMultiArgProcCall<TProc, T1, T2, T3, T4> }
constructor TMultiArgProcCall<TProc, T1, T2, T3, T4>.Create(AProc: TProc; const AArg1: T1; const AArg2: T2; const AArg3: T3; const AArg4: T4);
begin
  inherited Create(AProc, AArg1, AArg2, AArg3);
  FArg4 := AArg4;
end;

{ TAsyncVclCallAnonymProc }

constructor TAsyncCalls.TAsyncVclCallAnonymProc.Create(AProc: TProc);
begin
  inherited Create;
  FProc := AProc;
end;

function TAsyncCalls.TAsyncVclCallAnonymProc.ExecuteAsyncCall: Integer;
begin
  FProc();
  Result := 0;
end;

{ TAsyncCalls.TAsyncCallArg<T> }

function TAsyncCalls.TAsyncCallArg<T>.ExecuteAsyncCall: Integer;
begin
  Result := FProc(FArg1);
end;

{ TAsyncCalls.TAsyncCallArg<T1, T2> }

function TAsyncCalls.TAsyncCallArg<T1, T2>.ExecuteAsyncCall: Integer;
begin
  Result := FProc(FArg1, FArg2);
end;

{ TAsyncCalls.TAsyncCallArg<T1, T2, T3> }

function TAsyncCalls.TAsyncCallArg<T1, T2, T3>.ExecuteAsyncCall: Integer;
begin
  Result := FProc(FArg1, FArg2, FArg3);
end;

{ TAsyncCalls.TAsyncCallArg<T1, T2, T3, T4> }

function TAsyncCalls.TAsyncCallArg<T1, T2, T3, T4>.ExecuteAsyncCall: Integer;
begin
  Result := FProc(FArg1, FArg2, FArg3, FArg4);
end;

{ TAsyncCalls.TAsyncCallArgMethod<T> }

function TAsyncCalls.TAsyncCallArgMethod<T>.ExecuteAsyncCall: Integer;
begin
  Result := FProc(FArg1);
end;

{ TAsyncCalls.TAsyncCallArgMethod<T1, T2> }

function TAsyncCalls.TAsyncCallArgMethod<T1, T2>.ExecuteAsyncCall: Integer;
begin
  Result := FProc(FArg1, FArg2);
end;

{ TAsyncCalls.TAsyncCallArgMethod<T1, T2, T3> }

function TAsyncCalls.TAsyncCallArgMethod<T1, T2, T3>.ExecuteAsyncCall: Integer;
begin
  Result := FProc(FArg1, FArg2, FArg3);
end;

{ TAsyncCalls.TAsyncCallArgMethod<T1, T2, T3, T4> }

function TAsyncCalls.TAsyncCallArgMethod<T1, T2, T3, T4>.ExecuteAsyncCall: Integer;
begin
  Result := FProc(FArg1, FArg2, FArg3, FArg4);
end;

{ TAsyncCalls.TAsyncCallAnonymProc }

constructor TAsyncCalls.TAsyncCallAnonymProc.Create(AProc: TProc);
begin
  inherited Create;
  FProc := AProc;
end;

function TAsyncCalls.TAsyncCallAnonymProc.ExecuteAsyncCall: Integer;
begin
  FProc;
  Result := 0;
end;

{ TAsyncCalls.TAsyncCallAnonymFunc }

constructor TAsyncCalls.TAsyncCallAnonymFunc.Create(AProc: TIntFunc);
begin
  inherited Create;
  FProc := AProc;
end;

function TAsyncCalls.TAsyncCallAnonymFunc.ExecuteAsyncCall: Integer;
begin
  Result := FProc();
end;

{ TAsyncCalls<T> }

class function TAsyncCalls.Invoke<T>(Proc: TAsyncCallArgGenericProc<T>; const Arg: T): IAsyncCall;
begin
  { Execute the function synchron if no thread pool exists }
  if GetMaxAsyncCallThreads = 0 then
    Result := TSyncCall.Create(Proc(Arg))
  else
    Result := TAsyncCallArg<T>.Create(Proc, Arg).ExecuteAsync;
end;

class function TAsyncCalls.Invoke<T>(Event: TAsyncCallArgGenericMethod<T>; const Arg: T): IAsyncCall;
begin
  { Execute the function synchron if no thread pool exists }
  if GetMaxAsyncCallThreads = 0 then
    Result := TSyncCall.Create(Event(Arg))
  else
    Result := TAsyncCallArgMethod<T>.Create(Event, Arg).ExecuteAsync;
end;

class function TAsyncCalls.Invoke<T1, T2>(Proc: TAsyncCallArgGenericProc<T1, T2>; const Arg1: T1; const Arg2: T2): IAsyncCall;
begin
  { Execute the function synchron if no thread pool exists }
  if GetMaxAsyncCallThreads = 0 then
    Result := TSyncCall.Create(Proc(Arg1, Arg2))
  else
    Result := TAsyncCallArg<T1, T2>.Create(Proc, Arg1, Arg2).ExecuteAsync;
end;

class function TAsyncCalls.Invoke<T1, T2>(Event: TAsyncCallArgGenericMethod<T1, T2>; const Arg1: T1; const Arg2: T2): IAsyncCall;
begin
  { Execute the function synchron if no thread pool exists }
  if GetMaxAsyncCallThreads = 0 then
    Result := TSyncCall.Create(Event(Arg1, Arg2))
  else
    Result := TAsyncCallArgMethod<T1, T2>.Create(Event, Arg1, Arg2).ExecuteAsync;
end;

class function TAsyncCalls.Invoke<T1, T2, T3>(Proc: TAsyncCallArgGenericProc<T1, T2, T3>; const Arg1: T1; const Arg2: T2; const Arg3: T3): IAsyncCall;
begin
  { Execute the function synchron if no thread pool exists }
  if GetMaxAsyncCallThreads = 0 then
    Result := TSyncCall.Create(Proc(Arg1, Arg2, Arg3))
  else
    Result := TAsyncCallArg<T1, T2, T3>.Create(Proc, Arg1, Arg2, Arg3).ExecuteAsync;
end;

class function TAsyncCalls.Invoke<T1, T2, T3>(Event: TAsyncCallArgGenericMethod<T1, T2, T3>; const Arg1: T1; const Arg2: T2; const Arg3: T3): IAsyncCall;
begin
  { Execute the function synchron if no thread pool exists }
  if GetMaxAsyncCallThreads = 0 then
    Result := TSyncCall.Create(Event(Arg1, Arg2, Arg3))
  else
    Result := TAsyncCallArgMethod<T1, T2, T3>.Create(Event, Arg1, Arg2, Arg3).ExecuteAsync;
end;

class function TAsyncCalls.Invoke<T1, T2, T3, T4>(Proc: TAsyncCallArgGenericProc<T1, T2, T3, T4>; const Arg1: T1; const Arg2: T2; const Arg3: T3; const Arg4: T4): IAsyncCall;
begin
  { Execute the function synchron if no thread pool exists }
  if GetMaxAsyncCallThreads = 0 then
    Result := TSyncCall.Create(Proc(Arg1, Arg2, Arg3, Arg4))
  else
    Result := TAsyncCallArg<T1, T2, T3, T4>.Create(Proc, Arg1, Arg2, Arg3, Arg4).ExecuteAsync;
end;

class function TAsyncCalls.Invoke<T1, T2, T3, T4>(Event: TAsyncCallArgGenericMethod<T1, T2, T3, T4>; const Arg1: T1; const Arg2: T2; const Arg3: T3; const Arg4: T4): IAsyncCall;
begin
  { Execute the function synchron if no thread pool exists }
  if GetMaxAsyncCallThreads = 0 then
    Result := TSyncCall.Create(Event(Arg1, Arg2, Arg3, Arg4))
  else
    Result := TAsyncCallArgMethod<T1, T2, T3, T4>.Create(Event, Arg1, Arg2, Arg3, Arg4).ExecuteAsync;
end;

class function TAsyncCalls.Invoke(Func: TIntFunc): IAsyncCall;
begin
  { Execute the function synchron if no thread pool exists }
  if GetMaxAsyncCallThreads = 0 then
    Result := TSyncCall.Create(Func())
  else
    Result := TAsyncCallAnonymFunc.Create(Func).ExecuteAsync;
end;

class function TAsyncCalls.Invoke(Proc: TProc): IAsyncCall;
begin
  { Execute the function synchron if no thread pool exists }
  if GetMaxAsyncCallThreads = 0 then
  begin
    Proc();
    Result := TSyncCall.Create(0);
  end
  else
    Result := TAsyncCallAnonymProc.Create(Proc).ExecuteAsync;
end;

class procedure TAsyncCalls.VCLSync(Proc: TProc);

  procedure Exec(var P: TProc);
  begin
    P();
  end;

var
  M: TMethod;
begin
  if GetCurrentThreadId = MainThreadID then
    Proc()
  else
  begin
    M.Code := @Exec;
    M.Data := @Proc;
    StaticSynchronize(TThreadMethod(M));
  end;
end;

class function TAsyncCalls.VCLInvoke(Proc: TProc): IAsyncCall;
var
  Call: TAsyncVclCallAnonymProc;
begin
  if GetCurrentThreadId = MainThreadID then
  begin
    Proc();
    Result := TSyncCall.Create(0);
  end
  else
  begin
    Call := TAsyncVclCallAnonymProc.Create(Proc);
    ThreadPool.SendVclSync(Call);
    Result := Call;
  end;
end;

class procedure TAsyncCalls.MsgExec(AsyncCall: IAsyncCall; IdleMsgMethod: TAsyncIdleMsgMethod);
begin
  if GetCurrentThreadId = MainThreadID then
  begin
    if Assigned(IdleMsgMethod) then
    begin
      AsyncCall.ForceDifferentThread;
      IdleMsgMethod;
      while MsgAsyncMultiSync([AsyncCall], False, INFINITE, QS_ALLINPUT or QS_ALLPOSTMESSAGE) = 1 do
        IdleMsgMethod;
    end;
  end
  else
    AsyncCall.Sync;
end;

{$ENDIF DELPHI2009_UP}

{----------------------------------------------------------------------------}

initialization
  ThreadPool := TThreadPool.Create;
  MainThreadContext.MainThreadEntered := -1;
  {$IFNDEF DELPHi7_UP}
  SyncEvent := CreateEvent(nil, False, False, nil);
  {$ENDIF ~DELPHi7_UP}
  InitializeCriticalSection(MainThreadContextCritSect);

finalization
  ThreadPool.Free;
  ThreadPool := nil;
  DeleteCriticalSection(MainThreadContextCritSect);
  {$IFNDEF DELPHi7_UP}
  CloseHandle(SyncEvent);
  SyncEvent := 0;
  {$ENDIF ~DELPHi7_UP}

end.
