{-----------------------------------------------------------------------------
 Unit Name: cTools
 Author:    Kiriakos Vlahos
 Date:      02-Jun-2005
 Purpose:   Class definitions for Command Line Tools
 History:
-----------------------------------------------------------------------------}

unit cTools;

interface
uses
  System.SysUtils,
  System.Classes,
  Vcl.ActnList;

type
  TProcessStdInputOption = (piNone, piWordAtCursor, piCurrentLine, piSelection,
                                                    piActiveFile);
  TProcessStdOutputOption = (poNone, poWordAtCursor, poCurrentLine, poSelection,
                                                     poActiveFile, poNewFile);
  TToolContext = (tcAlwaysEnabled, tcActiveFile, tcActivePythonFile, tcSelectionAvailable);

  TSaveFiles = (sfNone, sfActive, sfAll);

  TExternalTool = class;

  TExternalToolExecute = procedure(Tool: TExternalTool) of object;

  {
     Describes the properties of a command line tool
  }
  TExternalTool = class(TPersistent)
  private
    FApplicationName: string;
    FParameters: string;
    FProcessInput: TProcessStdInputOption;
    FProcessOutput: TProcessStdOutputOption;
    FCaptureOutput: Boolean;
    FConsoleHidden: Boolean;
    FWorkingDirectory: string;
    FDescription: string;
    FCaption: string;
    FShortCut: TShortCut;
    FParseMessages : boolean;
    FParseTraceback : boolean;
    FMessagesFormat : string;
    FContext: TToolContext;
    FSaveFiles : TSaveFiles;
    FEnvironment : TStrings;
    FUseCustomEnvironment : Boolean;
    FUtf8IO: Boolean;
    procedure SetEnvironment(const Value: TStrings);
    function IsMessageFormatStore: Boolean;
  public
    class var ExternalToolExecute: TExternalToolExecute;
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
    procedure Assign(Source: TPersistent); override;
  published
    // The caption of the Menu Item corresponding to this tool
    property Caption : string read FCaption write FCaption;
    // The hint of the Menu Item corresponding to this tool
    property Description : string read FDescription write FDescription;
    property ApplicationName: string read FApplicationName write FApplicationName;
    property Parameters: string read FParameters write FParameters;
    property WorkingDirectory : string read FWorkingDirectory write FWorkingDirectory;
    property ShortCut : TShortCut read FShortCut write FShortCut default 0;
    // External Tool action context (tcAlwaysEnabled, tcActiveFile, tcSelectionAvailable)
    property Context : TToolContext read FContext write FContext
      default tcAlwaysEnabled;
    // Save file option (sfNone, sfActive, sfAll)
    property SaveFiles : TSaveFiles read FSaveFiles write FSaveFiles default sfNone;
    // Standard Input option
    //  piNone:  No standard input
    //  piWordAtCursor: Word at cursor
    //  piCurrentLine: Current line
    //  piSelection: Selection
    //  piActiveFile: active file
    property ProcessInput : TProcessStdInputOption read FProcessInput
      write FProcessInput default piNone;
    // Standard output option
    //  poNone:  Do nothing
    //  poWordAtCursor: Replace word at cursor
    //  poCurrentLine: Replace current line
    //  poSelection: Replace selection
    //  poActiveFile: Replace active file
    //  poNewFile: Place in new file
    property ProcessOutput : TProcessStdOutputOption read FProcessOutput
      write FProcessOutput default poNone;
    // Parse File Line LinePos info from output and put it in the Messages Window
    property ParseMessages : boolean read FParseMessages write FParseMessages
      default False;
    // Parse TraceBack and Syntax Errors from Python output and put it in the Messages Window
    property ParseTraceback : boolean read FParseTraceback write FParseTraceback
      default False;
    // Grep Expression for the parsing output lines for file/Line/Pos information
    // Can use the parameters $[Filename], $[LineNumber], $[Line], $[Column]
    property MessagesFormat : string read FMessagesFormat write FMessagesFormat
      stored IsMessageFormatStore;
    // Capture command line output and place it in the Output Window
    property CaptureOutput : Boolean read FCaptureOutput write FCaptureOutput
      default True;
    // Hide Console or External Tool window
    property ConsoleHidden : Boolean read FConsoleHidden write FConsoleHidden
      default True;
    property UseCustomEnvironment : boolean read FUseCustomEnvironment
      write FUseCustomEnvironment default False;
    // Custom Enviroment
    property Environment : TStrings read FEnvironment write SetEnvironment
      stored FUseCustomEnvironment;
    property Utf8IO: Boolean read FUtf8IO write FUtf8IO default False;
  end;

  // Differs only in persistence
  TExternalRun = class(TExternalTool)
  published
    property SaveFiles default sfAll;
    property Context default tcActiveFile;
    property ParseTraceback default True;
  end;

  TToolItem = class(TCollectionItem)
  private
    fExternalTool : TExternalTool;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ExternalTool : TExternalTool read fExternalTool
      write fExternalTool;
  end;

  TExternalToolAction = class(TAction)
  private
    fExternalTool : TExternalTool;
  protected
    procedure Change; override;
  public
    function Execute: Boolean; override;
    function Update: Boolean; override;
    constructor CreateExtToolAction(AOwner: TComponent; ExternalTool : TExternalTool);
  end;

{ Expands environment variables }
function ExpandEnv(const S: string): string;
{ Surrounds string with quotes when it contains spaces and is not quoted }
function AddQuotesUnless(const S: string): string;
{ Prepares an application name or the command line parameters for execution }
function PrepareCommandLine(S: string): string;

Const
  GrepFileNameParam = '$[FileName]';
  GrepLineNumberParam = '$[LineNumber]';
  GrepColumnNumberParam = '$[ColumnNumber]';

Var
  { A persisted collection of user-defined tools }
  ToolsCollection : TCollection;
  { External Python Tool }
  ExternalPython : TExternalTool;

implementation

uses
  Winapi.Windows,
  Vcl.Menus,
  JclStrings,
  JvGnuGetText,
  uEditAppIntfs,
  uCommonFunctions;


function ExpandEnv(const S: string): string;
var
  r: array[0..MAX_PATH] of WideChar;
begin
  ExpandEnvironmentStringsW(PWideChar(S), @r, MAX_PATH);
  Result := r;
end;

function AddQuotesUnless(const S: string): string;
begin
  Result := Trim(S);
  if (Pos(' ', Result) <> 0) and
     ((Result[1] <> '"') or (Result[Length(Result)] <> '"')) then
    Result := '"' + Result + '"';
end;

function PrepareCommandLine(S: string): string;
begin
  S := ExpandEnv(S);
  S:= GI_PyIDEServices.ReplaceParams(S);
  Result := Trim(S);
end;

{ TTool }

procedure TToolItem.Assign(Source: TPersistent);
begin
  if Source is TToolItem then with TToolItem(Source) do begin
    Self.fExternalTool.Assign(ExternalTool);
  end else
    inherited;
end;

constructor TToolItem.Create(Collection: TCollection);
begin
  inherited;
  fExternalTool := TExternalTool.Create;
end;

destructor TToolItem.Destroy;
begin
  fExternalTool.Free;
  inherited;
end;

function TToolItem.GetDisplayName: string;
begin
  Result := StripHotKey(_(fExternalTool.Caption));
end;

{ TExternalTool }

procedure TExternalTool.Assign(Source: TPersistent);
begin
  if Source is TExternalTool then with TExternalTool(Source) do begin
    Self.FCaption := Caption;
    Self.FDescription := Description;
    Self.FShortCut := ShortCut;
    Self.FContext := Context;
    Self.FSaveFiles := SaveFiles;
    Self.FApplicationName := ApplicationName;
    Self.FParameters := Parameters;
    Self.FWorkingDirectory := WorkingDirectory;
    Self.FProcessInput := ProcessInput;
    Self.FProcessOutput := ProcessOutput;
    Self.FParseMessages := ParseMessages;
    Self.FParseTraceback := ParseTraceback;
    Self.FMessagesFormat := MessagesFormat;
    Self.FCaptureOutput := CaptureOutput;
    Self.FConsoleHidden := ConsoleHidden;
    Self.FUtf8IO := Utf8IO;
    Self.FUseCustomEnvironment := UseCustomEnvironment;
    Self.FEnvironment.Assign(Environment);
  end else
    inherited;
end;

constructor TExternalTool.Create;
begin
  inherited;
  fProcessInput := piNone;
  fProcessOutput := poNone;
  fCaptureOutput := True;
  fConsoleHidden := True;
  fParseMessages := False;
  fContext := tcAlwaysEnabled;
  fSaveFiles := sfNone;
  fMessagesFormat := GrepFileNameParam + ' ' + GrepLineNumberParam;
  fEnvironment := TStringList.Create;
end;

destructor TExternalTool.Destroy;
begin
  fEnvironment.Free;
  inherited;
end;

procedure TExternalTool.Execute;
begin
  if Assigned(ExternalToolExecute) then
    ExternalToolExecute(Self);
end;

function TExternalTool.IsMessageFormatStore: Boolean;
begin
  Result :=FParseMessages and (FMessagesFormat <> (GrepFileNameParam + ' ' + GrepLineNumberParam));
end;

procedure TExternalTool.SetEnvironment(const Value: TStrings);
begin
  FEnvironment.Assign(Value);
end;

{ TExternalToolAction }

procedure TExternalToolAction.Change;
begin
  inherited;
  if Assigned(FExternalTool) then
    FExternalTool.ShortCut := ShortCut;
end;

constructor TExternalToolAction.CreateExtToolAction(AOwner: TComponent;
  ExternalTool: TExternalTool);
var
  S: string;
  AppFile: string;
begin
  inherited Create(AOwner);
  FExternalTool := ExternalTool;
  if Assigned(FExternalTool) then begin
    ShortCut := FExternalTool.ShortCut;
    Caption := _(FExternalTool.Caption);
    S := StrRemoveChars(Caption , [' ', '&', '.']);  // Fix error reported by David Funtowiez
    if IsValidIdent(S) then
      Name := 'actTools' + S;
    Category := 'External Tools';
    Hint := _(FExternalTool.Description);
    ImageIndex := -1;
    if (fExternalTool.ApplicationName <> '') then begin
      AppFile := PrepareCommandLine(fExternalTool.ApplicationName);
      if FileExists(AppFile) then
        ImageIndex := GetIconIndexFromFile(AppFile, True);
    end;
  end;
end;

function TExternalToolAction.Execute: Boolean;
begin
  if Assigned(FExternalTool) and Assigned(FExternalTool.ExternalToolExecute) then begin
    TThread.ForceQueue(nil, procedure
    begin
      FExternalTool.Execute;
    end);
  end;
  Result := True;
end;

function TExternalToolAction.Update: Boolean;
begin
  Result := True;
  if Assigned(FExternalTool) then begin
    case FExternalTool.Context of
      tcAlwaysEnabled : Enabled := True;
      tcActiveFile : Enabled := Assigned(GI_ActiveEditor);
      tcActivePythonFile :
        Enabled := Assigned(GI_ActiveEditor) and GI_ActiveEditor.HasPythonFile;
      tcSelectionAvailable : Enabled :=
        Assigned(GI_ActiveEditor) and GI_ActiveEditor.SynEdit.SelAvail;
    end;
  end else
    Enabled := False;
end;

initialization
  ToolsCollection := TCollection.Create(TToolItem);
  // Add a few standard tools to the collection
  with (ToolsCollection.Add as TToolItem).ExternalTool do begin
    Caption := _('Python &Interpreter');
    Description := _('External Python Interpreter');
    ApplicationName := '$[PythonExe-Short]';
    WorkingDirectory := '$[ActiveDoc-Dir]';
    SaveFiles := sfAll;
    ParseMessages := False;
    CaptureOutput := False;
    ConsoleHidden := False;
  end;

  with (ToolsCollection.Add as TToolItem).ExternalTool do begin
    Caption := _('Install Packages with pip');
    Description := _('Install Python packages');
    ApplicationName := '$[PythonDir-Short]Scripts\pip.exe';
    Parameters := 'install -U $[Package?Package Name]';
    ParseMessages := False;
    CaptureOutput := True;
    ConsoleHidden := True;
  end;

  with (ToolsCollection.Add as TToolItem).ExternalTool do begin
    Caption := _('Python&Win help');
    Description := _('Show PythonWin Help');
    ApplicationName := '$[PythonExe-Path-Short]Lib\site-packages\PyWin32.chm';
    ParseMessages := False;
    CaptureOutput := False;
    ConsoleHidden := False;
  end;

  with (ToolsCollection.Add as TToolItem).ExternalTool do begin
    Caption := _('Check &Indentation');
    Description := _('Check the indentation of the Python program');
    ApplicationName := '$[PythonExe-Short]';
    Parameters := '-m tabnanny "$[ActiveDoc]"';
    WorkingDirectory := '$[ActiveDoc-Dir]';
    ShortCut := Vcl.Menus.Shortcut(Ord('T'), [ssShift, ssCtrl]);
    Context := tcActivePythonFile;
    SaveFiles := sfActive;
    ParseTraceback := True;
    ParseMessages := True;
    CaptureOutput := True;
    ConsoleHidden := True;
    MessagesFormat := GrepFileNameParam + ' '+GrepLineNumberParam;
  end;

  with (ToolsCollection.Add as TToolItem).ExternalTool do begin
    Caption := _('Command Prompt');
    Description := _('Start a console at the directory of the active file');
    ApplicationName := '%COMSPEC%';
    WorkingDirectory := '$[ActiveDoc-Dir]';
    SaveFiles := sfAll;
    ParseMessages := False;
    CaptureOutput := False;
    ConsoleHidden := False;
  end;

  with (ToolsCollection.Add as TToolItem).ExternalTool do begin
    Caption := _('Profile');
    Description := _('Profile active file');
    ApplicationName := '$[PythonExe-Short]';
    Parameters := '-m profile "$[ActiveDoc]" $[CmdLineArgs]';
    WorkingDirectory := '$[ActiveDoc-Dir]';
    Context := tcActivePythonFile;
    SaveFiles := sfActive;
    ParseTraceback := True;
    CaptureOutput := True;
    ConsoleHidden := True;
  end;

  with (ToolsCollection.Add as TToolItem).ExternalTool do begin
    Caption := 'Py&lint';
    Description := _('PyLint tool (www.pylint.org)');
    ApplicationName :=  '$[PythonDir-Short]Scripts\pylint.exe';
    Parameters := '-f parseable "$[ActiveDoc]"';
    ShortCut := Vcl.Menus.Shortcut(Ord('L'), [ssShift, ssCtrl]);
    Context := tcActivePythonFile;
    SaveFiles := sfAll;
    ParseTraceback := True;
    ParseMessages := True;
    CaptureOutput := True;
    ConsoleHidden := True;
    MessagesFormat := Format('%s:%s: ', [GrepFileNameParam, GrepLineNumberParam]);
  end;

  with (ToolsCollection.Add as TToolItem).ExternalTool do begin
    Caption := _('Advanced Replace');
    Description := _('Advanced Search and replace');
    ApplicationName := '$[PythonExe-Short]';
    Parameters := '-c "import sys, re;l=sys.stdin.read();sys.stdout.write(re.sub(''$[st?Search Text:]'', ''$[rt?Replace Text:]'', l))"';
    Context := tcSelectionAvailable;
    SaveFiles := sfNone;
    ProcessInput := piSelection;
    ProcessOutput := poSelection;
    ParseMessages := False;
    CaptureOutput := False;
    ConsoleHidden := True;
    Utf8IO := True;
  end;

  with (ToolsCollection.Add as TToolItem).ExternalTool do begin
    Caption := _('Format Selection');
    Description := _('Format selected code using the "black" module');
    ApplicationName := '$[PythonExe-Short]';
    Parameters := '-m black -';
    ShortCut := Vcl.Menus.Shortcut(Ord('F'), [ssShift, ssAlt]);
    Context := tcSelectionAvailable;
    SaveFiles := sfNone;
    ProcessInput := piSelection;
    ProcessOutput := poSelection;
    ParseMessages := False;
    CaptureOutput := False;
    ConsoleHidden := True;
    Utf8IO := True;
  end;

  with (ToolsCollection.Add as TToolItem).ExternalTool do begin
    Caption := _('Sort Selection');
    Description := _('Sort the selected editor block ("one-liner" demo)');
    ApplicationName := '$[PythonExe-Short]';
    Parameters := '-c "import sys;l=sys.stdin.readlines();l.sort();sys.stdout.writelines(l)"';
    ShortCut := Vcl.Menus.Shortcut(Ord('S'), [ssShift, ssCtrl]);
    Context := tcSelectionAvailable;
    SaveFiles := sfNone;
    ProcessInput := piSelection;
    ProcessOutput := poSelection;
    ParseMessages := False;
    CaptureOutput := False;
    ConsoleHidden := True;
    Utf8IO := True;
  end;

  with (ToolsCollection.Add as TToolItem).ExternalTool do begin
    Caption := _('&Re-indent');
    Description := _('Re-indent the active file');
    ApplicationName := '$[PythonExe-Short]';
    Parameters := '$[PythonDir-Short]Tools\Scripts\reindent.py';
    Context := tcActivePythonFile;
    SaveFiles := sfNone;
    ProcessInput := piActiveFile;
    ProcessOutput := poActiveFile;
    ParseMessages := False;
    CaptureOutput := True;
    ConsoleHidden := True;
  end;

  // Create a Python External Run tool which is used in the run menu
  ExternalPython := TExternalRun.Create;
  with ExternalPython do begin
    Caption := _('Python Interpreter');
    Description := _('External Python Interpreter');
    ApplicationName := '$[PythonExe-Short]';
    Parameters := '$[ActiveDoc-Short] $[CmdLineArgs]';
    WorkingDirectory := '$[ActiveDoc-Dir]';
    SaveFiles := sfAll;
    Context := tcActiveFile;
    ParseTraceback := True;
    CaptureOutput := True;
    ConsoleHidden := True;
  end;

finalization
  ToolsCollection.Free;
  ExternalPython.Free;
end.
