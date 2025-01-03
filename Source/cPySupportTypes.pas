{-----------------------------------------------------------------------------
 Unit Name: cPySupportTypes
 Author:    PyScripter
 Date:      09-Feb-2018
 Purpose:   Defines basic types used in interfacing with Python
 History:
-----------------------------------------------------------------------------}

unit cPySupportTypes;

interface

uses
  System.Classes,
  System.RegularExpressions,
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

const
  // Defined DebugIDE events
  dbie_user_call            = 0;
  dbie_user_line            = 1;
  dbie_user_thread          = 2;
  dbie_user_exception       = 3;
  dbie_user_yield           = 4;

  EngineTypeName : array [TPythonEngineType] of string =
    ('Internal', 'Remote', 'Remote TK', 'Remote Wx', 'SSH');
  FilePosInfoFormat : string = '%s (%d:%d)';
  FilePosInfoRegExpr : string = '(.+) \((\d+):(\d+)\)$';


type
  TEditorPos = record
  public
    [Weak] Editor: IEditor;
    Line: Integer;
    Char: Integer;
    IsSyntax: Boolean;
    ErrorMsg: string;
    procedure Clear;
    procedure NewPos(AEditor: IEditor; ALine: Integer; AChar: Integer = -1;
      IsSyntaxError : Boolean = False; AErrorMsg : string = '');
    class function EmptyPos: TEditorPos; static;
    class function NPos(AEditor: IEditor; ALine: Integer; AChar: Integer = -1;
      IsSyntaxError: Boolean = False; AErrorMsg: string = ''): TEditorPos; static;
  end;

  {
     Container of all info needed to run a given file
     Projects can contain multiple run configurations
  }
  TRunConfiguration = class(TPersistent)
  private
    FScriptName: string;
    FEngineType: TPythonEngineType;
    FWorkingDir: string;
    FParameters: string;
    FReinitializeBeforeRun: Boolean;
    FOutputFileName: string;
    FWriteOutputToFile: Boolean;
    FAppendToFile: Boolean;
    FExternalRun: TExternalRun;
    FDescription: string;
    procedure SetExternalRun(const Value: TExternalRun);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ScriptName: string read FScriptName write FScriptName;
    property Description: string read FDescription write FDescription;
    property EngineType: TPythonEngineType read FEngineType write FEngineType;
    property ReinitializeBeforeRun: Boolean read FReinitializeBeforeRun
      write FReinitializeBeforeRun;
    property Parameters: string read FParameters write FParameters;
    property WorkingDir: string read FWorkingDir write FWorkingDir;
    property WriteOutputToFile: Boolean read FWriteOutputToFile
      write FWriteOutputToFile;
    property OutputFileName: string read FOutputFileName write FOutputFileName;
    property AppendToFile: Boolean read FAppendToFile write FAppendToFile;
    property ExternalRun: TExternalRun read FExternalRun write SetExternalRun;
  end;

  { Python related regular expressions }
  TPyRegExpr = class
    class var BlockOpenerRE : TRegEx;
    class var BlockCloserRE : TRegEx;
    class var CodeCommentLineRE : TRegEx;
    class var NonExecutableLineRE : TRegEx;
    class var FunctionCallRE : TRegEx;
    class constructor Create;
    class function IsBlockOpener(Line : string) : Boolean;
    class function IsBlockCloser(Line : string) : Boolean;
    class function IsExecutableLine(Line : string) : Boolean;
  end;

const
  IdentRE = '[_\p{L}]\w*';
  DottedIdentRE = '[_\p{L}][\w\.]*';

implementation

uses
  System.SysUtils,
  uCommonFunctions;

{ TRunConfiguration }

procedure TRunConfiguration.Assign(Source: TPersistent);
begin
  if Source is TRunConfiguration then
  begin
    var SourceRC := TRunConfiguration(Source);
    FScriptName := SourceRC.ScriptName;
    FDescription := SourceRC.Description;
    FEngineType := SourceRC.EngineType;
    FWorkingDir := SourceRC.WorkingDir;
    FParameters := SourceRC.Parameters;
    FReinitializeBeforeRun := SourceRC.ReinitializeBeforeRun;
    FWriteOutputToFile := SourceRC.WriteOutputToFile;
    FOutputFileName := SourceRC.OutputFileName;
    FAppendToFile := SourceRC.AppendToFile;
    FExternalRun.Assign(SourceRC.ExternalRun);
  end else
    inherited;
end;

constructor TRunConfiguration.Create;
begin
  inherited;
  FEngineType := peRemote;
  FReinitializeBeforeRun := True;
  FOutputFileName := '$[ActiveScript-NoExt].log';
  FWorkingDir := '$[ActiveScript-Dir]';
  FExternalRun := TExternalRun.Create;
  with FExternalRun do begin
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
  FExternalRun.Free;
  inherited;
end;

procedure TRunConfiguration.SetExternalRun(const Value: TExternalRun);
begin
  FExternalRun.Assign(Value);
end;

{ TPyRegExpr }

class constructor TPyRegExpr.Create;
begin
  BlockOpenerRE := CompiledRegEx(':\s*(#.*)?$');
  BlockCloserRE := CompiledRegEx('\s*(return|break|continue|raise|pass)\b');
  CodeCommentLineRE := CompiledRegEx('^([ \t]*)##', [roNotEmpty, roMultiLine]);
  NonExecutableLineRE := CompiledRegEx('(^\s*(class|def)\b)|(^\s*#)|(^\s*$)');
  FunctionCallRE := CompiledRegEx(Format('^[ \t]*(%s)(\(?)', [DottedIdentRE]));
end;

class function TPyRegExpr.IsBlockCloser(Line: string): Boolean;
begin
  Result := TPyRegExpr.BlockCloserRE.IsMatch(Line);
end;

class function TPyRegExpr.IsBlockOpener(Line: string): Boolean;
begin
  Result := TPyRegExpr.BlockOpenerRE.IsMatch(Line);
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

procedure TEditorPos.NewPos(AEditor: IEditor; ALine: Integer;
  AChar: Integer = -1; IsSyntaxError: Boolean = False; AErrorMsg: string = '');
begin
  Editor := AEditor;
  Line := ALine;
  Char := AChar;
  IsSyntax := IsSyntaxError;
  ErrorMsg := AErrorMsg;
end;

class function TEditorPos.NPos(AEditor: IEditor; ALine, AChar: Integer;
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
