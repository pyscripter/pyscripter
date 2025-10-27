{-----------------------------------------------------------------------------
 Unit Name: uEditAppIntfs
 Author:    Kiriakos Vlahos
 Date:      09-Mar-2005
 Purpose:   Python related interfaces
            Interface-based access to Python services
-----------------------------------------------------------------------------}

unit uPythonItfs;

interface

uses
  PythonEngine,
  PythonVersions,
  SynEdit,
  cPySupportTypes,
  cPyBaseDebugger;

type

{$REGION 'Python IDE Interfaces'}

  /// <summary>
  ///  Provides interface-based access to python related services to the IDE,
  ///  thus encpsulating all the python internals
  /// </summary>
  IPyControl = interface
  ['{DE1C1145-DC0F-4829-B36B-74EC818E168E}']
    procedure Run(ARunConfig: TRunConfiguration = nil);
    procedure Debug(ARunConfig: TRunConfiguration = nil; InitStepIn: Boolean =
        False; RunToCursorLine: Integer = -1);
    procedure ExternalRun(ARunConfig: TRunConfiguration = nil);
    function PythonLoaded: Boolean;
    function Running: Boolean;
    function Inactive: Boolean;
    function GetCurrentPos: TEditorPos;
    function GetActiveDebugger: TPyBaseDebugger;
    function GetActiveInterpreter: TPyBaseInterpreter;
    function GetActiveSSHServerName: string;
    function GetDebuggerState: TDebuggerState;
    function GetErrorPos: TEditorPos;
    function GetInternalInterpreter: TPyBaseInterpreter;
    function GetLastRunFileId: string;
    function GetPythonHelpFile: string;
    function GetPythonVersion: TPythonVersion;
    function GetPythonEngineType: TPythonEngineType;
    procedure AppendProjectPaths;
    procedure SetActiveSSHServerName(const Value: string);
    procedure SetCurrentPos(const NewPos: TEditorPos);
    procedure SetDebuggerState(const NewState: TDebuggerState);
    procedure SetErrorPos(const NewPos: TEditorPos);
    procedure SetPythonEngineType(const Value: TPythonEngineType);
    procedure Pickle(AValue: Variant; FileName: string);
    property ActiveDebugger: TPyBaseDebugger read GetActiveDebugger;
    property ActiveInterpreter: TPyBaseInterpreter read GetActiveInterpreter;
    property ActiveSSHServerName: string read GetActiveSSHServerName
      write SetActiveSSHServerName;
    property CurrentPos: TEditorPos read GetCurrentPos write SetCurrentPos;
    property DebuggerState: TDebuggerState read GetDebuggerState
      write SetDebuggerState;
    property ErrorPos: TEditorPos read GetErrorPos write SetErrorPos;
    property InternalInterpreter: TPyBaseInterpreter
      read GetInternalInterpreter;
    property LastRunFileId: string read GetLastRunFileId;
    property PythonVersion: TPythonVersion read GetPythonVersion;
    property PythonEngineType: TPythonEngineType read GetPythonEngineType
      write SetPythonEngineType;
    property PythonHelpFile: string read GetPythonHelpFile;
  end;

  /// <summary>
  ///   Inteface that the interpreter UI needs to implement
  /// </summary>
  IPyInterpreter = interface
  ['{6BAAD187-B00E-4E2A-B01D-C47EED922E59}']
    procedure AppendPrompt;
    procedure RemovePrompt;
    procedure AppendText(const Str: string);
    procedure PrintEngineType;
    procedure PrintInterpreterBanner(AVersion: string = ''; APlatform: string = '');
    procedure WritePendingMessages;
    procedure ClearPendingMessages;
    procedure ClearDisplay;
    procedure ClearLastPrompt;
    function OutputSuppressor: IInterface;
    procedure StartOutputMirror(const AFileName: string; Append: Boolean);
    procedure StopFileMirror;
    procedure UpdatePythonKeywords;
    procedure SetPyInterpreterPrompt(Pip: TPyInterpreterPropmpt);
    function GetPythonIO: TPythonInputOutput;
    function GetEditor: TCustomSynEdit;
    function GetShowOutput: Boolean;
    procedure SetShowOutput(const Value: Boolean);
    procedure ShowWindow(Activate: Boolean = True);
    property Editor: TCustomSynEdit read GetEditor;
    property PythonIO: TPythonInputOutput read GetPythonIO;
    property ShowOutput: Boolean read GetShowOutput write SetShowOutput;
  end;

  /// <summary>
  ///   Inteface that provides access to breakpoint management
  /// </summary>
  IBreakpointManager = interface
  ['{2A3F48C2-06E0-455D-B2D1-73BEAD0CF8F7}']
    function GetBreakpointsChanged: Boolean;
    procedure SetBreakpointsChanged(Value: Boolean);
    procedure ToggleBreakpoint(const FileId: string; ALine: Integer;
      CtrlPressed: Boolean = False; UpdateUI: Boolean = True);
    procedure SetBreakpoint(const FileId: string; ALine: Integer;
      Disabled: Boolean; Condition: string = ''; IgnoreCount: Integer = 0;
      UpdateUI: Boolean = True);
    function AllBreakPoints: TArray<TBreakpointInfo>;
    function EditProperties(var Condition: string; var IgnoreCount: Integer): Boolean;
    procedure ClearAllBreakpoints;
    property BreakpointsChanged: Boolean read GetBreakpointsChanged
      write SetBreakpointsChanged;
  end;

  /// <summary>
  ///   Inteface that the Watches UI needs to implement
  /// </summary>
  IWatchManager = interface
  ['{98C8EE88-8C29-436F-9CDC-730E7C7F0CA8}']
    procedure AddWatch(Str: string);
    procedure UpdateWindow;
  end;

  /// <summary>
  ///   Inteface that the Variables UI needs to implement
  /// </summary>
  IVariablesWindow = interface
  ['{9BD1D8C0-A0A2-4A56-B30F-615DFC41846B}']
    procedure ClearAll;
    procedure UpdateWindow;
  end;

  /// <summary>
  ///   Inteface that the Call Stack UI needs to implement
  /// </summary>
  ICallStackWindow = interface
  ['{CE08088E-6AE5-4F14-9114-92AB67E241E7}']
    function GetSelectedStackFrame: TBaseFrameInfo;
    procedure ClearAll(IncludeThreads: Boolean = True);
    procedure UpdateWindow(NewState, OldState: TDebuggerState);
  end;

var
  // Global Interfaces
  GI_PyControl: IPyControl;
  GI_PyInterpreter: IPyInterpreter;
  GI_BreakpointManager: IBreakpointManager;
  GI_WatchManager: IWatchManager;
  GI_VariablesWindow: IVariablesWindow;
  GI_CallStackWindow: ICallStackWindow;

{$ENDREGION 'Python IDE Interfaces'}

implementation

end.
