{-----------------------------------------------------------------------------
 Unit Name: cPySupportTypes
 Author:    PyScripter
 Date:      09-Feb-2018
 Purpose:   Defines basic types used in interfacing with Python
 History:
-----------------------------------------------------------------------------}

unit cPySupportTypes;

interface

Uses
  System.SysUtils,
  System.Classes,
  System.RegularExpressions,
  PythonEngine,
  uEditAppIntfs,
  cTools;

type
   { The available types of PythonEngines }
   TPythonEngineType =
     (peInternal,   // always available - used to communicate with external ones
      peRemote,     // rpyc based external Python - default
      peRemoteTk,   // specialized engines used to run GUI scripts
      peRemoteWx,
      peSSH);       // SSH Python engine

Const
  // Defined DebugIDE events
  dbie_user_call            = 0;
  dbie_user_line            = 1;
  dbie_user_thread          = 2;
  dbie_user_exception       = 3;
  dbie_user_yield           = 4;

const
  EngineTypeName : array [TPythonEngineType] of string =
    ('Internal', 'Remote', 'Remote TK', 'Remote Wx', 'SSH');
  FilePosInfoFormat : string = '%s (%d:%d)';
  FilePosInfoRegExpr : string = '(.+) \((\d+):(\d+)\)$';


type
  TEditorPos = record
  public
    [weak] Editor : IEditor;
    Line : integer;
    Char : integer;
    IsSyntax : Boolean;
    ErrorMsg : string;
    procedure Clear;
    procedure NewPos(AEditor : IEditor; ALine : integer; AChar : integer = -1;
      IsSyntaxError : Boolean = False; AErrorMsg : string = '');
    class function EmptyPos: TEditorPos; static;
    class function NPos(AEditor : IEditor; ALine : integer; AChar : integer = -1;
      IsSyntaxError : Boolean = False; AErrorMsg : string = ''): TEditorPos; static;
  end;

  {
     Container of all info needed to run a given file
     Projects can contain multiple run configurations
  }
  TRunConfiguration = class(TPersistent)
  private
    fScriptName: string;
    fEngineType: TPythonEngineType;
    fWorkingDir: string;
    fParameters: string;
    fReinitializeBeforeRun: Boolean;
    fOutputFileName: string;
    fWriteOutputToFile: Boolean;
    fAppendToFile: Boolean;
    fExternalRun: TExternalRun;
    fDescription: string;
    procedure SetExternalRun(const Value: TExternalRun);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ScriptName : string read fScriptName write fScriptName;
    property Description : string read fDescription write fDescription;
    property EngineType : TPythonEngineType read fEngineType write fEngineType;
    property ReinitializeBeforeRun : Boolean read fReinitializeBeforeRun
      write fReinitializeBeforeRun;
    property Parameters : string read fParameters write fParameters;
    property WorkingDir : string read fWorkingDir write fWorkingDir;
    property WriteOutputToFile : Boolean read fWriteOutputToFile
      write fWriteOutputToFile;
    property OutputFileName : string read fOutputFileName write fOutputFileName;
    property AppendToFile : Boolean read fAppendToFile write fAppendToFile;
    property ExternalRun : TExternalRun read fExternalRun write SetExternalRun;
  end;

  { Python related regular expressions }
  TPyRegExpr = class
    class var BlockOpenerRE : TRegEx;
    class var BlockCloserRE : TRegEx;
    class var CodeCommentLineRE : TRegEx;
    class var NonExecutableLineRE : TRegEx;
    class var FunctionCallRE : TRegEx;
    class constructor Create;
    class function IsBlockOpener(S : string) : Boolean;
    class function IsBlockCloser(S : string) : Boolean;
    class function IsExecutableLine(Line : string) : Boolean;
  end;

Const
  IdentRE = '[_\p{L}]\w*';
  DottedIdentRE = '[_\p{L}][\w\.]*';

implementation

Uses
  Winapi.Windows,
  uCommonFunctions;

{ TRunConfiguration }

procedure TRunConfiguration.Assign(Source: TPersistent);
begin
  if Source is TRunConfiguration then with TRunConfiguration(Source) do begin
    Self.fScriptName := ScriptName;
    Self.fDescription := Description;
    Self.fEngineType := EngineType;
    Self.fWorkingDir := WorkingDir;
    Self.fParameters := fParameters;
    Self.fReinitializeBeforeRun := ReinitializeBeforeRun;
    Self.fWriteOutputToFile := WriteOutputToFile;
    Self.fOutputFileName := OutputFileName;
    Self.fAppendToFile := AppendToFile;
    Self.fExternalRun.Assign(fExternalRun);
  end else
    inherited;
end;

constructor TRunConfiguration.Create;
begin
  inherited;
  fEngineType := peRemote;
  fReinitializeBeforeRun := True;
  fOutputFileName := '$[ActiveScript-NoExt].log';
  fWorkingDir := '$[ActiveScript-Dir]';
  fExternalRun := TExternalRun.Create;
  with fExternalRun do begin
    Caption := 'External Run';
    Description := 'Run script using an external Python Interpreter';
    ApplicationName := '$[PythonExe-Short]';
    Parameters := '$[ActiveScript-Short]';
    WorkingDirectory := '$[ActiveScript-Dir]';
    SaveFiles := sfAll;
    Context := tcActiveFile;
    ParseTraceback := True;
    CaptureOutput := True;
    ConsoleHidden := True;
  end;
end;

destructor TRunConfiguration.Destroy;
begin
  fExternalRun.Free;
  inherited;
end;

procedure TRunConfiguration.SetExternalRun(const Value: TExternalRun);
begin
  fExternalRun.Assign(Value);
end;

{ TPyRegExpr }

class constructor TPyRegExpr.Create;
begin
  BlockOpenerRE.Create(':\s*(#.*)?$');
  BlockCloserRE.Create('\s*(return|break|continue|raise|pass)\b');
  CodeCommentLineRE.Create('^([ \t]*)##', [roNotEmpty, roMultiLine]);
  NonExecutableLineRE.Create('(^\s*(class|def)\b)|(^\s*#)|(^\s*$)');
  FunctionCallRE := CompiledRegEx(Format('^[ \t]*(%s)(\(?)', [DottedIdentRE]));
end;

class function TPyRegExpr.IsBlockCloser(S: string): Boolean;
begin
  Result := TPyRegExpr.BlockCloserRE.IsMatch(S);
end;

class function TPyRegExpr.IsBlockOpener(S: string): Boolean;
begin
  Result := TPyRegExpr.BlockOpenerRE.IsMatch(S);
end;

class function TPyRegExpr.IsExecutableLine(Line: string): Boolean;
begin
  Result := not ((Line = '') or TPyRegExpr.NonExecutableLineRE.IsMatch(Line));
end;

{ TEditorPos }

class function TEditorPos.EmptyPos: TEditorPos;
begin
  with Result do begin
    Editor := nil;
    Line := -1;
    Char := -1;
    IsSyntax := False;
    ErrorMsg := '';
  end;
end;

procedure TEditorPos.NewPos(AEditor : IEditor; ALine : integer; AChar : integer = -1;
                 IsSyntaxError : Boolean = False; AErrorMsg : string = '');
begin
  Editor := AEditor;
  Line := ALine;
  Char := AChar;
  IsSyntax := IsSyntaxError;
  ErrorMsg := AErrorMsg;
end;

class function TEditorPos.NPos(AEditor: IEditor; ALine, AChar: integer;
  IsSyntaxError: Boolean; AErrorMsg: string): TEditorPos;
begin
  with Result do begin
    Editor := AEditor;
    Line := ALine;
    Char := AChar;
    IsSyntax := IsSyntaxError;
    ErrorMsg := AErrorMsg;
  end;
end;

procedure TEditorPos.Clear;
begin
  Editor := nil;
  Line := -1;
  Char := -1;
  IsSyntax := False;
  ErrorMsg := '';
end;


end.
