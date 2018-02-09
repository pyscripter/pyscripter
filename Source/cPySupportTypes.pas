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
  System.Classes,
  cTools;

type
   { The available types of PythonEngines }
   TPythonEngineType =
     (peInternal,   // always available - used to communicate with external ones
      peRemote,     // rpyc based external python - default
      peRemoteTk,   // specialized engines used to run GUI scripts
      peRemoteWx);

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


implementation

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
    WaitForTerminate := True;
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


end.
