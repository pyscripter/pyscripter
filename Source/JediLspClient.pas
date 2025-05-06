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
  System.SyncObjs,
  System.JSON,
  JclNotify,
  LspClient,
  SynEditTypes,
  SynEdit,
  LspUtils;

type

  TParamCompletionInfo = class
  private
    FRequestId: NativeUInt;
    FStartX: Integer;
    FActiveParameter: Integer;
    FHandled: Boolean;
    FSucceeded: Boolean;
    FDisplayString: string;
    FDocString: string;
    FFileId: string;
    FCurrentLine: string;
    FCaret: TBufferCoord;
    FCriticalSection: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock;
    procedure UnLock;
    property RequestId: NativeUInt read FRequestId write FRequestId;
    property StartX: Integer read FStartX;
    property ActiveParameter: Integer read FActiveParameter;
    property Handled: Boolean read FHandled write FHandled;
    property Succeeded: Boolean read FSucceeded;
    property CurrentLine: string read FCurrentLine;
    property DisplayString: string read FDisplayString;
    property DocString: string read FDocString;
    property FileId: string read FFileId;
    property Caret: TBufferCoord read FCaret;
  end;

  TJedi = class
  private
    class procedure ParamCompletionHandler(Id: NativeUInt; AResult, Error:
        TJSONValue);
  public
    class var LspClient: TLspClient;
    class var SyncRequestTimeout: Integer;
    class var ParamCompletionInfo: TParamCompletionInfo;
    class var OnInitialized: TJclNotifyEventBroadcast;
    class var OnShutDown: TJclNotifyEventBroadcast;
    class procedure PythonVersionChanged(Sender: TObject);
    class procedure OnLspClientInitialized(Sender: TObject);
    class procedure OnLspClientShutdown(Sender: TObject);
    class constructor Create;
    class destructor Destroy;
    class procedure CreateServer;
    class procedure Initialize;
    class function Ready: Boolean;
    // Lsp functionality
    class procedure FindDefinitionByCoordinates(const Filename: string;
      const BC: TBufferCoord; out DefFileName: string; out DefBC: TBufferCoord);
    class function FindReferencesByCoordinates(Filename: string;
      const BC: TBufferCoord): TArray<TDocPosition>;
    class function HandleCodeCompletion(const Filename: string;
      const BC: TBufferCoord; out InsertText, DisplayText: string): Boolean;
    class function ResolveCompletionItem(CCItem: string): string;
    class procedure RequestParamCompletion(const AFileId: string; Editor:
        TCustomSynEdit);
    class function DocumentSymbols(const FileName: string): TJSONArray;
    class function SimpleHintAtCoordinates(const Filename: string;
      const BC: TBufferCoord): string;
    class function CodeHintAtCoordinates(const Filename: string;
      const BC: TBufferCoord; const Ident: string): string;
  end;

implementation

uses
  System.Classes,
  System.Generics.Collections,
  System.Character,
  System.SysUtils,
  System.IOUtils,
  System.RegularExpressions,
  dmResources,
  dmCommands,
  uCommonFunctions,
  SynEditLsp,
  cPyScripterSettings,
  cPyControl,
  StringResources,
  JvGnugettext,
  cPySupportTypes,
  uEditAppIntfs,
  PythonEngine;

{ TJedi }

class constructor TJedi.Create;
begin
  ParamCompletionInfo := TParamCompletionInfo.Create;
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
  LspClient.Free;
  LspClient := nil;

  if not GI_PyControl.PythonLoaded then Exit;

  ServerPath :=
    TPath.Combine(TPyScripterSettings.LspServerPath,
    'jls\run-jedi-language-server.py');
  if not FileExists(ServerPath) then Exit;


  CmdLine := Format('"%s" -u "%s"',
    [GI_PyControl.PythonVersion.PythonExecutable, ServerPath]);
  if PyIDEOptions.LspDebug then
  begin
    CmdLine := Format('%s -v --log-file "%s"', [CmdLine,
      TPath.Combine(TPyScripterSettings.UserDataPath, LspDebugFile)]);
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
  GI_PyControl.OnPythonVersionChange.RemoveHandler(PythonVersionChanged);
  OnShutDown.RemoveHandler(TLspSynEditPlugin.OnLspShutDown);
  OnInitialized.Free;
  OnShutDown.Free;
  ParamCompletionInfo.Free;
  LspClient.Free;
end;

class procedure TJedi.Initialize;
var
  ClientCapabilities: TJSONObject;     // Will be freed by Initialize
  InitializationOptions: TJSONObject;  // Will be freed by Initialize
  InitString: string;
  ErrorCode: integer;
  Py: IPyEngineAndGIL;
const
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
    '	  },'#13#10 +
    '   "workspace": {'#13#10 +
    '       "extraPaths": [%s]'#13#10 +
    '	  }'#13#10 +
    '}';

  function QuotePackages(Packages: string): string;
  begin
    var Arr := Packages.Split([',']);
    if Length(Arr) = 0 then Exit('');

    for var I := 0 to Length(Arr) - 1 do
      Arr[I] := '"' + Trim(Arr[I]) + '"';

    Result := string.Join(',', Arr);
  end;

  function QuotePaths(Paths: TStringList): string;
  begin
    var Arr: TArray<String> := Paths.ToStringArray;
    if Length(Arr) = 0 then Exit('');

    for var I := 0 to Length(Arr) - 1 do
      Arr[I] := '"' + ExcludeTrailingPathDelimiter(Trim(Arr[I])).Replace('\','\\') + '"';

    Result := string.Join(',', Arr);
  end;

begin
  if LspClient.Status <> lspStarted then Exit;

  ClientCapabilities := TJSONObject.Create;
  ClientCapabilities.Parse(TEncoding.UTF8.GetBytes(ClientCapabilitiesJson), 0);
  InitializationOptions := TJSONObject.Create;

  Py := SafePyEngine;
  var Paths := TStringList.Create;
  Py.PythonEngine.GetPythonPathAsStrings(Paths, wppOnlyCustom);

  InitString := Format(InitializationOptionsLsp,
    [
      BoolToStr(PyIDEOptions.CheckSyntaxAsYouType, True).ToLower,
      QuotePackages(PyIDEOptions.SpecialPackages),
      BoolToStr(not PyIDEOptions.CodeCompletionCaseSensitive, True).ToLower,
      QuotePaths(Paths)
    ]
  );

  Paths.Free;
  ErrorCode := InitializationOptions.Parse(TEncoding.UTF8.GetBytes(InitString), 0);
  if ErrorCode < 0 then
    GI_PyIDEServices.WriteStatusMsg('Failed to create LSP InitializationOptions, Error code: ' + IntToStr(ErrorCode));

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
    OnShutDown.Notify(LspClient);
end;

class procedure TJedi.PythonVersionChanged(Sender: TObject);
begin
  CreateServer;
end;

class function TJedi.Ready: Boolean;
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
  Line, Char: Integer;
begin
  DefFileName := '';
  if not Ready or (FileName = '') then Exit;

  Param := TSmartPtr.Make(LspDocPosition(FileName, BC))();
  LspClient.SyncRequest('textDocument/definition', Param.ToJSon, AResult,
    Error, SyncRequestTimeout);

  if Assigned(AResult) and AResult.TryGetValue<string>('[0].uri', Uri) and
    AResult.TryGetValue<Integer>('[0].range.start.line', Line) and
    AResult.TryGetValue<Integer>('[0].range.start.character', Char) then
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
  const BC: TBufferCoord; out InsertText, DisplayText: string): Boolean;

  function KindToImageIndex(Kind: TLspCompletionItemKind): Integer;
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

  function ToLabel(Item: TCompletionItem): string;
  begin
    case Item.Kind of
      TLspCompletionItemKind._Constructor,
      TLspCompletionItemKind.Method,
      TLspCompletionItemKind._Function:
      Result := Item._Label + '()'
    else
      Result := Item._Label;
    end;
  end;

var
  Param: TJsonObject;
  AResult, Error: TJsonValue;
  CompletionItems: TCompletionItems;
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
      InsertText := InsertText + ToLabel(Item) + #10;
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

class procedure TJedi.ParamCompletionHandler(Id: NativeUInt; AResult,
  Error: TJsonValue);
var
  Signature: TJsonValue;
begin
  ParamCompletionInfo.Lock;
  try
    ParamCompletionInfo.FSucceeded := (Id = ParamCompletionInfo.RequestId) and
      Assigned(AResult) and AResult.TryGetValue('signatures[0]', Signature);

    if ParamCompletionInfo.FSucceeded then
    begin
      ParamCompletionInfo.FDisplayString := '';
      ParamCompletionInfo.FDocString := '';
      Signature.TryGetValue<string>('label', ParamCompletionInfo.FDisplayString);
      Signature.TryGetValue<string>('documentation.value', ParamCompletionInfo.FDocString);
      if not AResult.TryGetValue<Integer>('activeParameter', ParamCompletionInfo.FActiveParameter) then
        ParamCompletionInfo.FActiveParameter := 0;
      ParamCompletionInfo.FStartX := 1;

      if ParamCompletionInfo.FDisplayString.StartsWith('class') then
        Delete(ParamCompletionInfo.FDisplayString, 1, 5)
      else if ParamCompletionInfo.FDisplayString.StartsWith('def') then
        Delete(ParamCompletionInfo.FDisplayString, 1, 3);

      var RightPar := ParamCompletionInfo.FDisplayString.LastDelimiter(')');
      if RightPar >= 0 then
        Delete(ParamCompletionInfo.FDisplayString, RightPar + 1, 1);

      var LeftPar :=ParamCompletionInfo.FDisplayString.IndexOf('(');
      if LeftPar >= 0 then
      begin
        var FunctionName := Copy(ParamCompletionInfo.FDisplayString, 1, LeftPar).Trim;
        ParamCompletionInfo.FDisplayString := Copy(ParamCompletionInfo.FDisplayString, LeftPar + 2);
        var Match := TRegEx.Match(ParamCompletionInfo.FCurrentLine, FunctionName + '\s*(\()');
        if Match.Success then
          ParamCompletionInfo.FStartX := Match.Groups[1].Index + 1;
      end;
    end;

    // Do nothing if a newer request is expected
    if Id = ParamCompletionInfo.RequestId then begin
      // Mark the request info as handled (i.e. processed)
      ParamCompletionInfo.FHandled := True;

      // ActivateCompletion calls SynParamCompletionExecute in the main thread
      TThread.Queue(nil, procedure
        begin
          CommandsDataModule.SynParamCompletion.ActivateCompletion;
        end);
     end;
  finally
    ParamCompletionInfo.UnLock;
    AResult.Free;
    Error.Free;
  end;
end;

class procedure TJedi.RequestParamCompletion(const AFileId: string;
  Editor: TCustomSynEdit);
begin
  if not Ready or (AFileId = '') then Exit;

  // Get Completion for the current word
  var Line := Editor.LineText;

  var TmpX := Editor.CaretX;
  if TmpX > Length(Line) then
    TmpX := Length(Line) + 1;
  while (TmpX > 1) and ((Line[TmpX-1] = '_') or Line[TmpX-1].IsLetterOrDigit) do
    Dec(TmpX);

  var Param := TSmartPtr.Make(LspDocPosition(AFileId, BufferCoord(TmpX, Editor.CaretY)))();

  ParamCompletionInfo.Lock;
  try
    ParamCompletionInfo.FRequestId := LspClient.Request('textDocument/signatureHelp',
      Param.ToJSon, ParamCompletionHandler);
    ParamCompletionInfo.FFileId := AFileId;
    ParamCompletionInfo.FCaret := Editor.CaretXY;
    ParamCompletionInfo.FCurrentLine := Line;
  finally
    ParamCompletionInfo.UnLock;
  end;
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
    Result := StringReplace(Result, '<br>---<br>', '<hr>', []);

    FindDefinitionByCoordinates(FileName, BC, DefFileName, DefBC);
    if (DefFileName <> '') then begin
      if  FileIsPythonPackage(DefFileName)
      then
        ModuleName := FileNameToModuleName(DefFileName)
      else
        ModuleName := TPath.GetFileName(DefFileName);
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

{ TParamCompletionInfo }


{ TParamCompletionInfo }

constructor TParamCompletionInfo.Create;
begin
  inherited;
  FCriticalSection.Initialize;
end;

destructor TParamCompletionInfo.Destroy;
begin
  FCriticalSection.Free;
  inherited;
end;

procedure TParamCompletionInfo.Lock;
begin
  FCriticalSection.Enter;
end;

procedure TParamCompletionInfo.UnLock;
begin
  FCriticalSection.Leave;
end;

end.

