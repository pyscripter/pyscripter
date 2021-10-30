{ -----------------------------------------------------------------------------
  Unit Name: JediLspClient
  Author:    pyscripter
  Date:      16-May-2021
  Purpose:   Jedi Lsp client
  History:
  ----------------------------------------------------------------------------- }

unit JediLspClient;

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  System.JSON,
  JclNotify,
  LspClient,
  SynEditTypes,
  SynEdit,
  LspUtils,
  uEditAppIntfs;

type
  TJedi = class
  public
    class var LspClient: TLspClient;
    class var SyncRequestTimeout: integer;
    class var OnInitialized: TJclNotifyEventBroadcast;
    class var OnShutDown: TJclNotifyEventBroadcast;
    class procedure PythonVersionChanged(Sender: TObject);
    class procedure OnLspClientInitialized(Sender: TObject);
    class procedure OnLspClientShutdown(Sender: TObject);
    class constructor Create;
    class destructor Destroy;
    class procedure CreateServer;
    class procedure Initialize;
    class function Ready: boolean;
    // Lsp functionality
    class procedure FindDefinitionByCoordinates(const Filename: string;
      const BC: TBufferCoord; out DefFileName: string; out DefBC: TBufferCoord);
    class function FindReferencesByCoordinates(Filename: string;
      const BC: TBufferCoord): TArray<TDocPosition>;
    class function HandleCodeCompletion(const Filename: string;
      const BC: TBufferCoord; out InsertText, DisplayText: string): boolean;
    class function ResolveCompletionItem(CCItem: string): string;
    class function HandleParamCompletion(const FileName: string;
      Editor: TSynEdit; out DisplayString, DocString: string; out StartX,
      ActiveParameter: integer): boolean;
    class function DocumentSymbols(const FileName: string): TJsonArray;
    class function SimpleHintAtCoordinates(const Filename: string;
      const BC: TBufferCoord): string;
    class function CodeHintAtCoordinates(const Filename: string;
      const BC: TBufferCoord; const Ident: string): string;
  end;

implementation

uses
  System.Character,
  System.SysUtils,
  System.IOUtils,
  System.Threading,
  System.RegularExpressions,
  System.Generics.Defaults,
  dmCommands,
  uCommonFunctions,
  SynEditLsp,
  cPyScripterSettings,
  StringResources,
  JvGnugettext;

{ TJedi }

class constructor TJedi.Create;
begin
  SyncRequestTimeout := 4000; // ms
  OnInitialized := TJclNotifyEventBroadcast.Create;
  OnShutDown := TJclNotifyEventBroadcast.Create;
  OnShutDown.AddHandler(TLspSynEditPlugin.OnLspShutDown);
  GI_PyControl.OnPythonVersionChange.AddHandler(PythonVersionChanged);
end;

class procedure TJedi.CreateServer;
// Creates or recreates the Server
var
  CmdLine: string;
  ServerPath: string;
const
  LspDebugFile = 'LspDebug.log';
begin
  if Assigned(LspClient) then
    LspClient.OnLspNotification := nil;
  FreeAndNil(LspClient);

  if not GI_PyControl.PythonLoaded then Exit;

  ServerPath :=
    TPath.Combine(TPyScripterSettings.LspServerPath,
    'jls\run-jedi-language-server.py');
  if not FileExists(ServerPath) then Exit;


  CmdLine :=    '"' + GI_PyControl.PythonVersion.PythonExecutable + '" -u ' +
    ServerPath;
  if PyIDEOptions.LspDebug then
  begin
    CmdLine := CmdLine + ' -v --log-file ' +
      TPath.Combine(TPyScripterSettings.UserDataPath, LspDebugFile);
  end;

  LspClient := TLspClient.Create(CmdLine);
  LspClient.OnInitialized := OnLspClientInitialized;
  LspClient.OnShutdown := OnLspClientShutdown;
  LspClient.OnLspNotification := TLspSynEditPlugin.HandleLspNotify;

  LspClient.StartServer;
  Initialize;
end;

class destructor TJedi.Destroy;
begin
  OnShutDown.RemoveHandler(TLspSynEditPlugin.OnLspShutDown);
  FreeAndNil(OnInitialized);
  FreeAndNil(OnShutDown);
  GI_PyControl.OnPythonVersionChange.RemoveHandler(PythonVersionChanged);
  LspClient.Free;
end;

class procedure TJedi.Initialize;
var
  ClientCapabilities: TJsonObject;     // Will be freed by Initialize
  InitializationOptions: TJsonObject;  // Will be freed by Initialize
Const
   ClientCapabilitiesJson =
    '{"textDocument":{"documentSymbol":{"hierarchicalDocumentSymbolSupport":true}}}';
//    '{"textDocument":{"documentSymbol":{"hierarchicalDocumentSymbolSupport":true},"completion": {"completionItem": {"documentationFormat": ["markdown", "plaintext"]}}}}';
   InitializationOptionsLsp =
    '{'#13#10 +
    '    "diagnostics": {'#13#10 +
    '      "enable": true,'#13#10 +
    '      "didOpen": true,'#13#10 +
    '      "didChange": %s,'#13#10 +
    '      "didSave": true'#13#10 +
    '    },'#13#10 +
    '   "completion": {'#13#10 +
    '       "disableSnippets": true,'#13#10 +
    '       "resolveEagerly": false'#13#10 +
    '   },'#13#10 +
//    '   "markupKindPreferred": "markdown",'#13#10 +
    '	  "jediSettings": {'#13#10 +
    '		"autoImportModules": [%s],'#13#10 +
    '		"caseInsensitiveCompletion": %s'#13#10 +
    '	  }'#13#10 +
    '}';

  function QuotePackages(Packages: string): string;
  begin
    var Arr := Packages.Split([',']);
    if Length(Arr) = 0 then Exit('');

    for var I := 0 to Length(Arr) - 1 do
      Arr[I] := '"' + Trim(Arr[I]) + '"';

    Result := String.Join(',', Arr);
  end;

begin
  if LspClient.Status <> lspStarted then Exit;

  ClientCapabilities := TJsonObject.Create;
  ClientCapabilities.Parse(TEncoding.UTF8.GetBytes(ClientCapabilitiesJson), 0);
  InitializationOptions := TJsonObject.Create;
  InitializationOptions.Parse(TEncoding.UTF8.GetBytes(
    Format(InitializationOptionsLsp,
    [BoolToStr(PyIDEOptions.CheckSyntaxAsYouType, True).ToLower,
     QuotePackages(PyIDEOptions.SpecialPackages),
     BoolToStr(not PyIDEOptions.CodeCompletionCaseSensitive, True).ToLower])), 0);

  LspClient.Initialize('PyScripter', ApplicationVersion, ClientCapabilities,
    InitializationOptions);
end;

class procedure TJedi.OnLspClientInitialized(Sender: TObject);
begin
  if Assigned(OnInitialized) and (OnInitialized.HandlerCount > 0) then
    TThread.ForceQueue(nil, procedure
    begin
      OnInitialized.Notify(LspClient);
    end);
end;

class procedure TJedi.OnLspClientShutdown(Sender: TObject);
begin
  if Assigned(OnShutDown) and (OnShutDown.HandlerCount > 0) then
    OnShutdown.Notify(LspClient);
end;

class procedure TJedi.PythonVersionChanged(Sender: TObject);
begin
  CreateServer;
end;

class function TJedi.Ready: boolean;
begin
  Result := Assigned(LspClient) and (LspClient.Status = lspInitialized);
end;


{$Region 'Lsp functionality'}

class procedure TJedi.FindDefinitionByCoordinates(const Filename: string;
  const BC: TBufferCoord; out DefFileName: string; out DefBC: TBufferCoord);
var
  Param: TJsonObject;
  AResult, Error: TJsonValue;
  Uri: string;
  Line, Char: integer;
begin
  DefFileName := '';
  if not Ready or (FileName = '') then Exit;

  Param := TSmartPtr.Make(LspDocPosition(FileName, BC))();
  LspClient.SyncRequest('textDocument/definition', Param.ToJSon, AResult,
    Error, SyncRequestTimeout);

  if Assigned(AResult) and AResult.TryGetValue<string>('[0].uri', Uri) and
    AResult.TryGetValue<integer>('[0].range.start.line', Line) and
    AResult.TryGetValue<integer>('[0].range.start.character', Char) then
  begin
    DefFileName := FileIdFromURI(Uri);
    DefBC := BufferCoord(Char + 1, Line + 1);
  end;

  FreeAndNil(AResult);
  FreeAndNil(Error);
end;

class function TJedi.FindReferencesByCoordinates(Filename: string;
  const BC: TBufferCoord): TArray<TDocPosition>;
var
  Param: TJsonObject;
  Context: TJsonObject;
  AResult, Error: TJsonValue;
begin
  SetLength(Result, 0);
  if not Ready or (FileName = '') then Exit;

  Param := TSmartPtr.Make(LspDocPosition(FileName, BC))();
  Context := TJsonObject.Create;
  Context.AddPair('includeDeclaration', TJSONBool.Create(True));
  Param.AddPair('context', Context);

  LspClient.SyncRequest('textDocument/references', Param.ToJSon, AResult, Error,
    4 * SyncRequestTimeout);

  if AResult is TJSONArray then
  begin
    SetLength(Result, TJsonArray(AResult).Count);

    for var I := 0 to TJsonArray(AResult).Count -1 do
    begin
      var Location := TJsonArray(AResult).Items[I];
      if not LspLocationToDocPosition(Location, Result[I]) then
      begin
        // Error happened
        SetLength(Result, 0);
        Break;
      end;
    end;
  end;

  FreeAndNil(AResult);
  FreeAndNil(Error);
end;

class function TJedi.HandleCodeCompletion(const Filename: string;
  const BC: TBufferCoord; out InsertText, DisplayText: string): boolean;

  function KindToImageIndex(Kind: TLspCompletionItemKind): integer;
  begin
    case Kind of
      TLspCompletionItemKind._Constructor,
      TLspCompletionItemKind.Method:     Result := Integer(TCodeImages.Method);
      TLspCompletionItemKind._Function:  Result := Integer(TCodeImages.Func);
      TLspCompletionItemKind.Variable:   Result := Integer(TCodeImages.Variable);
      TLspCompletionItemKind._Class:     Result := Integer(TCodeImages.Klass);
      TLspCompletionItemKind.Module:     Result := Integer(TCodeImages.Module);
      TLspCompletionItemKind.Field,
      TLspCompletionItemKind._Property:  Result := Integer(TCodeImages.Field);
      TLspCompletionItemKind.Keyword:    Result := Integer(TCodeImages.Keyword);
    else
      Result := -1;
    end;
  end;

var
  Param: TJsonObject;
  AResult, Error: TJsonValue;
  CompletionItems : TCompletionItems;
begin
  if not Ready or (FileName = '') then Exit(False);

  Param := TSmartPtr.Make(LspDocPosition(FileName, BC))();
  LspClient.SyncRequest('textDocument/completion', Param.ToJSon, AResult, Error,
    SyncRequestTimeout);
  CompletionItems := LspCompletionItems(AResult);

  if Length(CompletionItems) > 0 then
  begin
    // process completion items
    InsertText := '';
    DisplayText := '';
    for var Item in CompletionItems do
    begin
      InsertText := InsertText + Item._label + #10;
      var ImageIndex := KindToImageIndex(TLspCompletionItemKind(Item.kind));
      DisplayText := DisplayText + Format('\Image{%d}\hspace{8}%s', [ImageIndex, Item._label]) + #10;
    end;
    Result := True;
  end
  else
    Result := False;

  FreeAndNil(AResult);
  FreeAndNil(Error);
end;

class function TJedi.ResolveCompletionItem(CCItem: string): string;
var
  AResult, Error: TJsonValue;
begin
  if not Ready then Exit('');

  var Item := TSmartPtr.Make(TJsonObject.Create)();
  Item.AddPair('label', TJSONString.Create(CCItem));

  LspClient.SyncRequest('completionItem/resolve', Item.ToJson, AResult, Error,
    SyncRequestTimeout div 10);
  if Assigned(AResult) and AResult.TryGetValue<string>('documentation.value', Result) then
    Result := GetLineRange(Result, 1, 20)
  else
     Result := '';

  FreeAndNil(AResult);
  FreeAndNil(Error);
end;

class function TJedi.HandleParamCompletion(const FileName: string;
  Editor: TSynEdit; out DisplayString, DocString: string; out StartX,
  ActiveParameter: integer): boolean;
var
  TmpX: integer;
  Line: string;
  Param: TJsonObject;
  AResult, Error: TJsonValue;
  Signature : TJsonValue;
begin
  if not Ready or (FileName = '') then Exit(False);

  // Get Completion for the current word
  Line := Editor.LineText;

  TmpX := Editor.CaretX;
  if TmpX > Length(Line) then
    TmpX := Length(Line) + 1;
  while (TmpX > 1) and ((Line[TmpX-1] = '_') or Line[TmpX-1].IsLetterOrDigit) do
    Dec(TmpX);

  Param := TSmartPtr.Make(LspDocPosition(FileName, BufferCoord(TmpX, Editor.CaretY)))();
  LspClient.SyncRequest('textDocument/signatureHelp', Param.ToJSon, AResult,
    Error, SyncRequestTimeout);

  if Assigned(AResult) and AResult.TryGetValue('signatures[0]', Signature) then
  begin
    DisplayString := '';
    DocString := '';
    Signature.TryGetValue<string>('label', DisplayString);
    Signature.TryGetValue<string>('documentation.value', DocString);
    if not AResult.TryGetValue<integer>('activeParameter', ActiveParameter) then
      ActiveParameter := 0;
    StartX := 1;

    var RightPar := DisplayString.LastDelimiter(')');
    if RightPar >= 0 then
      Delete(DisplayString, RightPar + 1, 1);

    var LeftPar :=DisplayString.IndexOf('(');
    if LeftPar >= 0 then
    begin
      var FunctionName := Copy(DisplayString, 1, LeftPar).Trim;
      DisplayString := Copy(DisplayString, LeftPar + 2);
      var Match := TRegEx.Match(Line, FunctionName + '\s*(\()');
      if Match.Success then
        StartX := Match.Groups[1].Index;
    end;
    Result := True;
  end
  else
    Result := False;

  FreeAndNil(AResult);
  FreeAndNil(Error);
end;

class function TJedi.SimpleHintAtCoordinates(const Filename: string;
  const BC: TBufferCoord): string;
var
  Param: TJsonObject;
  AResult, Error: TJsonValue;
begin
  Result := '';
  if not Ready or (FileName = '') then Exit;

  Param := TSmartPtr.Make(LspDocPosition(FileName, BC))();
  TJedi.LspClient.SyncRequest('textDocument/hover', Param.ToJSon, AResult,
    Error, 1000);

  if Assigned(AResult) then
    AResult.TryGetValue<string>('contents.value', Result);

  FreeAndNil(AResult);
  FreeAndNil(Error);
end;

class function TJedi.CodeHintAtCoordinates(const Filename: string;
  const BC: TBufferCoord; const Ident: string): string;
var
  Param: TJsonObject;
  AResult, Error: TJsonValue;
  DefFileName: string;
  DefBC: TBufferCoord;
  ModuleName,
  DefinedIn: string;
begin
  Result := '';
  if not Ready or (FileName = '') then Exit;

  Param := TSmartPtr.Make(LspDocPosition(FileName, BC))();
  LspClient.SyncRequest('textDocument/hover', Param.ToJSon, AResult,
    Error, SyncRequestTimeout);

  if Assigned(AResult) then
  begin
    if not AResult.Null then
    begin
      AResult.TryGetValue<string>('contents.value', Result);
      Result := GetLineRange(Result, 1, 20, True);
    end;
    Result := StringReplace(Result, '<br>---<br>', '<hr >', []);

    FindDefinitionByCoordinates(FileName, BC, DefFileName, DefBC);
    if (DefFileName <> '') and FileExists(DefFileName) then begin
      if  FileIsPythonPackage(DefFileName)
      then
        ModuleName := FileNameToModuleName(DefFileName)
      else
        ModuleName := ExtractFileName(DefFileName);
      ModuleName := TPath.GetFileNameWithoutExtension(ModuleName);
      DefinedIn := Format(_(SFilePosInfoCodeHint),
        [DefFileName, DefBC.Line, DefBC.Char,
         ModuleName, DefBC.Line]);
      Result := Result + DefinedIn;
    end;
  end;

  FreeAndNil(AResult);
  FreeAndNil(Error);
end;

class function TJedi.DocumentSymbols(const FileName: string): TJsonArray;
var
  AResult, Error: TJsonValue;
begin
  if not Ready then Exit(nil);

  var Param := TSmartPtr.Make(TJsonObject.Create)();
  Param.AddPair('textDocument', LspTextDocumentIdentifier(FileName));

  LspClient.SyncRequest('textDocument/documentSymbol', Param.ToJson, AResult, Error,
    SyncRequestTimeout * 4);

  if AResult is TJsonArray then
     Result := TJsonArray(AResult)
  else
  begin
    FreeAndNil(AResult);
    Result := nil;
  end;

  FreeAndNil(Error);
end;

{$EndRegion 'Lsp functionality'}

end.

