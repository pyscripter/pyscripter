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
  System.Messaging,
  LspClient,
  SynEditTypes,
  SynEdit,
  LspUtils,
  cPySupportTypes;

type

  TJedi = class
  private
    class procedure CodeCompletionHandler(Id: NativeUInt; AResult, Error:
        TJSONValue);
    class procedure ParamCompletionHandler(Id: NativeUInt; AResult, Error:
        TJSONValue);
  public
    class var LspClient: TLspClient;
    class var SyncRequestTimeout: Integer;
    class procedure PythonVersionChanged(const Sender: TObject;
      const Msg: System.Messaging.TMessage);
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
    class function HandleCodeCompletion(const FileId: string; const BC:
        TBufferCoord; out InsertText, DisplayText: string): Boolean;
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
  cCodeCompletion,
  StringResources,
  JvGnugettext;

{ TJedi }

class constructor TJedi.Create;
begin
  SyncRequestTimeout := 4000; // ms
  TMessageManager.DefaultManager.SubscribeToMessage(TPythonVersionChangeMessage,
    PythonVersionChanged);
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
  TMessageManager.DefaultManager.Unsubscribe(TPythonVersionChangeMessage, PythonVersionChanged);
  LspClient.Free;
end;

class procedure TJedi.Initialize;
var
  ClientCapabilities: TJSONObject;     // Will be freed by Initialize
  InitializationOptions: TJSONObject;  // Will be freed by Initialize
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

begin
  if LspClient.Status <> lspStarted then Exit;

  ClientCapabilities := TJSONObject.Create;
  ClientCapabilities.Parse(TEncoding.UTF8.GetBytes(ClientCapabilitiesJson), 0);
  InitializationOptions := TJSONObject.Create;
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
  TThread.ForceQueue(nil, procedure
  begin
    TMessageManager.DefaultManager.SendMessage(LspClient,
      TLspServerInitializedMessage.Create);
  end);
end;

class procedure TJedi.OnLspClientShutdown(Sender: TObject);
begin
  TThread.ForceQueue(nil, procedure
  begin
    TMessageManager.DefaultManager.SendMessage(LspClient,
      TLspServerShutDownMessage.Create);
  end);
end;

class procedure TJedi.PythonVersionChanged(const Sender: TObject; const Msg:
    System.Messaging.TMessage);
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

class function TJedi.HandleCodeCompletion(const FileId: string;
  const BC: TBufferCoord; out InsertText, DisplayText: string): Boolean;

var
  Param: TJsonObject;
begin
  if not Ready or (FileId = '') then Exit(False);

  Param := TSmartPtr.Make(LspDocPosition(FileId, BC))();
  TIDECompletion.EditorCodeCompletion.CompletionInfo.Id :=
     LspClient.Request('textDocument/completion', Param.ToJSon,
     CodeCompletionHandler);
  Result := False;
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
  var SignatureInfo := TIDECompletion.SignatureHelpInfo;

  SignatureInfo.Lock;
  try
    SignatureInfo.Succeeded := (Id = SignatureInfo.RequestId) and
      Assigned(AResult) and AResult.TryGetValue('signatures[0]', Signature);

    if SignatureInfo.Succeeded then
    begin
      SignatureInfo.DisplayString := '';
      SignatureInfo.DocString := '';
      Signature.TryGetValue<string>('label', SignatureInfo.DisplayString);
      Signature.TryGetValue<string>('documentation.value', SignatureInfo.DocString);
      if not AResult.TryGetValue<Integer>('activeParameter', SignatureInfo.ActiveParameter) then
        SignatureInfo.ActiveParameter := 0;
      SignatureInfo.StartX := 1;

      if SignatureInfo.DisplayString.StartsWith('class') then
        Delete(SignatureInfo.DisplayString, 1, 5)
      else if SignatureInfo.DisplayString.StartsWith('def') then
        Delete(SignatureInfo.DisplayString, 1, 3);

      var RightPar := SignatureInfo.DisplayString.LastDelimiter(')');
      if RightPar >= 0 then
        Delete(SignatureInfo.DisplayString, RightPar + 1, 1);

      var LeftPar :=SignatureInfo.DisplayString.IndexOf('(');
      if LeftPar >= 0 then
      begin
        var FunctionName := Copy(SignatureInfo.DisplayString, 1, LeftPar).Trim;
        SignatureInfo.DisplayString := Copy(SignatureInfo.DisplayString, LeftPar + 2);
        var Match := TRegEx.Match(SignatureInfo.CurrentLine, FunctionName + '\s*(\()');
        if Match.Success then
          SignatureInfo.StartX := Match.Groups[1].Index + 1;
      end;
    end;

    // Do nothing if a newer request is expected
    if Id = SignatureInfo.RequestId then begin
      // Mark the request info as handled (i.e. processed)
      SignatureInfo.Handled := True;

      // ActivateCompletion calls SynParamCompletionExecute in the main thread
      TThread.Queue(nil, procedure
        begin
          CommandsDataModule.SynParamCompletion.ActivateCompletion;
        end);
     end;
  finally
    SignatureInfo.UnLock;
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

  TIDECompletion.SignatureHelpInfo.Lock;
  try
    TIDECompletion.SignatureHelpInfo.RequestId :=
      LspClient.Request('textDocument/signatureHelp',
      Param.ToJSon, ParamCompletionHandler);
    TIDECompletion.SignatureHelpInfo.FileId := AFileId;
    TIDECompletion.SignatureHelpInfo.Caret := Editor.CaretXY;
    TIDECompletion.SignatureHelpInfo.CurrentLine := Line;
  finally
    TIDECompletion.SignatureHelpInfo.UnLock;
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

class procedure TJedi.CodeCompletionHandler(Id: NativeUInt; AResult,
  Error: TJSONValue);

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
  CompletionItems: TCompletionItems;
  InsertText, DisplayText: string;
begin
  if not Assigned(AResult) then Exit;

  try
    CompletionItems := LspCompletionItems(AResult);

    if Length(CompletionItems) > 0 then
    begin
      var CC := TIDECompletion.EditorCodeCompletion;
      CC.Lock.Enter;
      try
        // Check we deal with the most recent completion request
        if ID <> CC.CompletionInfo.Id  then Exit;

        // process completion items
        InsertText := '';
        DisplayText := '';
        for var Item in CompletionItems do
        begin
          InsertText := InsertText + ToLabel(Item) + #10;
          var ImageIndex := KindToImageIndex(TLspCompletionItemKind(Item.kind));
          DisplayText := DisplayText + Format('\Image{%d}\hspace{8}%s',
            [ImageIndex, Item._label]) + #10;
        end;
        CC.CompletionInfo.InsertText := InsertText;
        CC.CompletionInfo.DisplayText := DisplayText;

        TThread.ForceQueue(nil, procedure
        begin
          CommandsDataModule.SynCodeCompletion.ActivateCompletion;
        end)
      finally
        CC.Lock.Leave;
      end;
    end;
  finally
    FreeAndNil(AResult);
    FreeAndNil(Error);
  end;
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
    Result := StringReplace(Result, '<br>', SLineBreak, [rfReplaceAll]);
    Result := StringReplace(Result, SLineBreak + '---' + SLineBreak, '<hr>', [rfReplaceAll]);
    // In some corner cases the hint may contain html that may cause exceptions
    Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
    Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
    Result := StringReplace(Result, '&lt;hr&gt;', '<hr>', []);

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

end.

