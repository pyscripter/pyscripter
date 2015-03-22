{

Fast Memory Manager: Options Include File

Set the default options for FastMM here.

}

{---------------------------Miscellaneous Options-----------------------------}

{Enable this define to align all blocks on 16 byte boundaries so aligned SSE
 instructions can be used safely. If this option is disabled then some of the
 smallest block sizes will be 8-byte aligned instead which may result in a
 reduction in memory usage. Medium and large blocks are always 16-byte aligned
 irrespective of this setting.}
{.$define Align16Bytes}

{Enable to use faster fixed-size move routines when upsizing small blocks.
 These routines are much faster than the Borland RTL move procedure since they
 are optimized to move a fixed number of bytes. This option may be used
 together with the FastMove library for even better performance.}
{$define UseCustomFixedSizeMoveRoutines}

{Enable this option to use an optimized procedure for moving a memory block of
 an arbitrary size. Disable this option when using the Fastcode move
 ("FastMove") library. Using the Fastcode move library allows your whole
 application to gain from faster move routines, not just the memory manager. It
 is thus recommended that you use the Fastcode move library in conjunction with
 this memory manager and disable this option.}
{$define UseCustomVariableSizeMoveRoutines}

{Enable to always assume that the application is multithreaded. Enabling this
 option will cause a significant performance hit with single threaded
 applications. Enable if you are using multi-threaded third party tools that do
 not properly set the IsMultiThread variable. Also set this option if you are
 going to share this memory manager between a single threaded application and a
 multi-threaded DLL.}
{.$define AssumeMultiThreaded}

{Enable this option to never put a thread to sleep if a thread contention
 occurs. This option will improve performance if the ratio of the number of
 active threads to the number of CPU cores is low (typically < 2). With this
 option set a thread will enter a "busy waiting" loop instead of relinquishing
 its timeslice when a thread contention occurs.}
{.$define NeverSleepOnThreadContention}

{Due to QC#14070 ("Delphi IDE attempts to free memory after the shutdown code
 of borlndmm.dll has been called"), FastMM cannot be uninstalled safely when
 used inside a replacement borlndmm.dll for the IDE. Setting this option will
 circumvent this problem by never uninstalling the memory manager.}
{.$define NeverUninstall}

{Set this option when you use runtime packages in this application or library.
 This will automatically set the "AssumeMultiThreaded" option. Note that you
 have to ensure that FastMM is finalized after all live pointers have been
 freed - failure to do so will result in a large leak report followed by a lot
 of A/Vs. (See the FAQ for more detail.) You may have to combine this option
 with the NeverUninstall option.}
{.$define UseRuntimePackages}

{-----------------------------Debugging Options-------------------------------}

{Enable this option to suppress the generation of debug info for the
 FastMM4.pas unit. This will prevent the integrated debugger from stepping into
 the memory manager code.}
{$define NoDebugInfo}

{Enable this option to suppress the display of all message dialogs. This is
 useful in service applications that should not be interrupted.}
{.$define NoMessageBoxes}

{Set this option to use the Windows API OutputDebugString procedure to output
 debug strings on startup/shutdown and when errors occur.}
{.$define UseOutputDebugString}

{Set this option to use the assembly language version which is faster than the
 pascal version. Disable only for debugging purposes. Setting the
 CheckHeapForCorruption option automatically disables this option.}
{$define ASMVersion}

{FastMM always catches attempts to free the same memory block twice, however it
 can also check for corruption of the memory heap (typically due to the user
 program overwriting the bounds of allocated memory). These checks are
 expensive, and this option should thus only be used for debugging purposes.
 If this option is set then the ASMVersion option is automatically disabled.}
{changed}
{$define CheckHeapForCorruption}

{Enable this option to catch attempts to perform MM operations after FastMM has
 been uninstalled. With this option set when FastMM is uninstalled it will not
 install the previous MM, but instead a dummy MM handler that throws an error
 if any MM operation is attempted. This will catch attempts to use the MM
 after FastMM has been uninstalled.}
{$define DetectMMOperationsAfterUninstall}

{Set the following option to do extensive checking of all memory blocks. All
 blocks are padded with both a header and trailer that are used to verify the
 integrity of the heap. Freed blocks are also cleared to to ensure that they
 cannot be reused after being freed. This option slows down memory operations
 dramatically and should only be used to debug an application that is
 overwriting memory or reusing freed pointers. Setting this option
 automatically enables CheckHeapForCorruption and disables ASMVersion.
 Very important: If you enable this option your application will require the
 FastMM_FullDebugMode.dll library. If this library is not available you will
 get an error on startup.}
{changed}
{$define FullDebugMode}

  {Set this option to perform "raw" stack traces, i.e. check all entries on the
   stack for valid return addresses. Note that this is significantly slower
   than using the stack frame tracing method, but is usually more complete. Has
   no effect unless FullDebugMode is enabled}
  {$define RawStackTraces}

  {Set this option to check for user code that uses an interface of a freed
   object. Note that this will disable the checking of blocks modified after
   being freed (the two are not compatible). This option has no effect if
   FullDebugMode is not also enabled.}
  {.$define CatchUseOfFreedInterfaces}

  {Set this option to log all errors to a text file in the same folder as the
   application. Memory errors (with the FullDebugMode option set) will be
   appended to the log file. Has no effect if "FullDebugMode" is not set.}
  {$define LogErrorsToFile}

  {Set this option to log all memory leaks to a text file in the same folder as
   the application. Memory leak reports (with the FullDebugMode option set)
   will be appended to the log file. Has no effect if "LogErrorsToFile" and
   "FullDebugMode" are not also set. Note that usually all leaks are always
   logged, even if they are "expected" leaks registered through
   AddExpectedMemoryLeaks. Expected leaks registered by pointer may be excluded
   through the HideExpectedLeaksRegisteredByPointer option.}
  {$define LogMemoryLeakDetailToFile}

  {Deletes the error log file on startup. No effect if LogErrorsToFile is not
   also set.}
  {changed}
 {$define ClearLogFileOnStartup}

  {Loads the FASTMM_FullDebugMode.dll dynamically. If the DLL cannot be found
   then stack traces will not be available. Note that this may cause problems
   due to a changed DLL unload order when sharing the memory manager. Use with
   care.}
  {changed}
  {$define LoadDebugDLLDynamically}

{---------------------------Memory Leak Reporting-----------------------------}

{Set this option to enable reporting of memory leaks. Combine it with the two
 options below for further fine-tuning.}
{$define EnableMemoryLeakReporting}

  {Set this option to suppress the display and logging of expected memory leaks
   that were registered by pointer. Leaks registered by size or class are often
   ambiguous, so these expected leaks are always logged to file (in
   FullDebugMode with the LogMemoryLeakDetailToFile option set) and are never
   hidden from the leak display if there are more leaks than are expected.}
  {$define HideExpectedLeaksRegisteredByPointer}

  {Set this option to require the presence of the Delphi IDE to report memory
   leaks. This option has no effect if the option "EnableMemoryLeakReporting"
   is not also set.}
  {.$define RequireIDEPresenceForLeakReporting}

  {Set this option to require the program to be run inside the IDE debugger to
   report memory leaks. This option has no effect if the option
   "EnableMemoryLeakReporting" is not also set.}
  {$define RequireDebuggerPresenceForLeakReporting}

  {Set this option to require the presence of debug info ($D+ option) in the
   compiled unit to perform memory leak checking. This option has no effect if
   the option "EnableMemoryLeakReporting" is not also set.}
  {.$define RequireDebugInfoForLeakReporting}

  {Set this option to enable manual control of the memory leak report. When
   this option is set the ReportMemoryLeaksOnShutdown variable (default = false)
   may be changed to select whether leak reporting should be done or not. When
   this option is selected then both the variable must be set to true and the
   other leak checking options must be applicable for the leak checking to be
   done.}
  {.$define ManualLeakReportingControl}

  {Set this option to disable the display of the hint below the memory leak
   message.}
  {.$define HideMemoryLeakHintMessage}

{--------------------------Instruction Set Options----------------------------}

{Set this option to enable the use of MMX instructions. Disabling this option
 will result in a slight performance hit, but will enable compatibility with
 AMD K5, Pentium I and earlier CPUs. MMX is currently only used in the variable
 size move routines, so if UseCustomVariableSizeMoveRoutines is not set then
 this option has no effect.}
{$define EnableMMX}

  {Set this option to force the use of MMX instructions without checking
   whether the CPU supports it. If this option is disabled then the CPU will be
   checked for compatibility first, and if MMX is not supported it will fall
   back to the FPU move code. Has no effect unless EnableMMX is also set.}
  {$define ForceMMX}

{-----------------------Memory Manager Sharing Options------------------------}

{Allow sharing of the memory manager between a main application and DLLs that
 were also compiled with FastMM. This allows you to pass dynamic arrays and
 long strings to DLL functions provided both are compiled to use FastMM.
 Sharing will only work if the library that is supposed to share the memory
 manager was compiled with the "AttemptToUseSharedMM" option set. Note that if
 the main application is single threaded and the DLL is multi-threaded that you
 have to set the IsMultiThread variable in the main application to true or it
 will crash when a thread contention occurs. Note that statically linked DLL
 files are initialized before the main application, so the main application may
 well end up sharing a statically loaded DLL's memory manager and not the other
 way around. }
{.$define ShareMM}

  {Allow sharing of the memory manager by a DLL with other DLLs (or the main
   application if this is a statically loaded DLL) that were also compiled with
   FastMM. Set this option with care in dynamically loaded DLLs, because if the
   DLL that is sharing its MM is unloaded and any other DLL is still sharing
   the MM then the application will crash. This setting is only relevant for
   DLL libraries and requires ShareMM to also be set to have any effect.
   Sharing will only work if the library that is supposed to share the memory
   manager was compiled with the "AttemptToUseSharedMM" option set. Note that
   if DLLs are statically linked then they will be initialized before the main
   application and then the DLL will in fact share its MM with the main
   application. This option has no effect unless ShareMM is also set.}
  {.$define ShareMMIfLibrary}

  {Define this option to allow sharing between the default memory manager and
   FastMM. This option only works together with the memory manager of BDS2006.
   With this option enabled FastMM can be shared with applications using the
   Delphi 2006 MM and vice versa. (You may have to add SimpleShareMem.pas to the
   project using the Delphi 2006 memory manager to enable sharing.)}
  {$define EnableSharingWithDefaultMM}

{Define this to attempt to share the MM of the main application or other loaded
 DLLs in the same process that were compiled with ShareMM set. When sharing a
 memory manager, memory leaks caused by the sharer will not be freed
 automatically. Take into account that statically linked DLLs are initialized
 before the main application, so set the sharing options accordingly.}
{.$define AttemptToUseSharedMM}

{--------------------------------Option Grouping------------------------------}

{Group the options you use for release and debug versions below}
{$ifdef Release}
  {Specify the options you use for release versions below}
  {.$undef FullDebugMode}
  {.$undef CheckHeapForCorruption}
  {.$define ASMVersion}
  {.$undef EnableMemoryLeakReporting}
  {.$undef UseOutputDebugString}
{$else}
  {Specify the options you use for debugging below}
  {.$define FullDebugMode}
  {.$define EnableMemoryLeakReporting}
  {.$define UseOutputDebugString}
{$endif}

{--------------------Compilation Options For borlndmm.dll---------------------}
{If you're compiling the replacement borlndmm.dll, set the defines below
 for the kind of dll you require.}

{Set this option when compiling the borlndmm.dll}
{.$define borlndmmdll}

{Set this option if the dll will be used by the Delphi IDE}
{.$define dllforide}

{Set this option if you're compiling a debug dll}
{.$define debugdll}

{Do not change anything below this line}
{$ifdef borlndmmdll}
  {$define AssumeMultiThreaded}
  {$undef HideExpectedLeaksRegisteredByPointer}
  {$undef RequireDebuggerPresenceForLeakReporting}
  {$undef RequireDebugInfoForLeakReporting}
  {$define DetectMMOperationsAfterUninstall}
  {$undef ManualLeakReportingControl}
  {$undef ShareMM}
  {$undef AttemptToUseSharedMM}
  {$ifdef dllforide}
    {$define NeverUninstall}
    {$define HideMemoryLeakHintMessage}
    {$undef RequireIDEPresenceForLeakReporting}
    {$ifndef debugdll}
      {$undef EnableMemoryLeakReporting}
    {$endif}
  {$else}
    {$define EnableMemoryLeakReporting}
    {$undef NeverUninstall}
    {$undef HideMemoryLeakHintMessage}
    {$define RequireIDEPresenceForLeakReporting}
  {$endif}
  {$ifdef debugdll}
    {$define FullDebugMode}
    {$define RawStackTraces}
    {$undef CatchUseOfFreedInterfaces}
    {$define LogErrorsToFile}
    {$define LogMemoryLeakDetailToFile}
    {$undef ClearLogFileOnStartup}
  {$else}
    {$undef FullDebugMode}
  {$endif}
{$endif}
