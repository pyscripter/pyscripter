{
   Utility functions and definitions used by the LSP client
   (C) PyScripter 2021
}


unit LspUtils;

interface

uses
  System.SysUtils,
  System.JSON,
  System.Generics.Collections,
  WinApi.Windows;


type
{$SCOPEDENUMS ON}
   TLspCompletionItemKind = (
    Text = 1,
    Method = 2,
    _Function = 3,
    _Constructor = 4,
    Field = 5,
    Variable = 6,
    _Class = 7,
    _Interface = 8,
    Module = 9,
    _Property = 10,
    _Unit = 11,
    Value = 12,
    Enum = 13,
    Keyword = 14,
    Snippet = 15,
    Color = 16,
    _File = 17,
    Reference = 18,
    Folder = 19,
    EnumMember = 20,
    Constant = 21,
    Struct = 22,
    Event = 23,
    _Operator = 24,
    TypeParameter = 25);

  TSymbolKind = (
	  _File = 1,
	  Module = 2,
 	  Namespace = 3,
	  Package = 4,
	  _Class = 5,
 	  Method = 6,
  	_Property = 7,
	  Field = 8,
	  _Constructor = 9,
	  _Enum = 10,
	  _Interface = 11,
	  _Function = 12,
	  _Variable = 13,
	  _Constant = 14,
	  _String = 15,
    Number = 16,
	  _Boolean = 17,
	  _Array = 18,
	  _Object = 19,
	  _Key = 20,
	  _Null = 21,
	  _EnumMember = 22,
	  _Struct = 23,
	  _Event = 24,
	  _Operator = 25,
	  _TypeParameter = 26);

   TDiagnositicSeverity = (
     Error = 1,
     Warning = 2,
     Information = 3,
     Hint = 4);
{$SCOPEDENUMS OFF}

  TCompletionItem = record
    (*
     * The label of this completion item. By default
     * also the text that is inserted when selecting
     * this completion.
     *)
    _label: string;
    kind: TLspCompletionItemKind;
    documentation: string {| MarkupContent};
  end;

  TCompletionItems = TArray<TCompletionItem>;

  TDocPosition = record
    FileId: string;
    Line: integer;
    Char: integer;
    constructor Create(const AFileId: string; Line, Char: integer);
  end;

function FormatJSON(const json: string): string;
function FileIdToURI(const FilePath: string): string;
function FileIdFromURI(const URI: string): string;
function LSPInitializeParams(const ClientName, ClientVersion: string;
  ClientCapabilities: TJsonObject;
  InitializationOptions: TJsonObject = nil): TJsonObject;
function LspPosition(Line, Char: Integer): TJsonObject; overload;
function LspDocPosition(const FileName: string; Line, Char: integer): TJsonObject; overload;
function LspLocationToDocPosition(Location: TJsonValue; out DocPosition: TDocPosition): Boolean;
function LspTextDocumentItem(const FileName, LanguageId, Text: string; Version: integer): TJsonObject;
function LspTextDocumentIdentifier(const FileName: string): TJsonObject;
function LspVersionedTextDocumentIdentifier(const FileName: string; Version: NativeUInt): TJsonObject;
function LspCompletionItems(LspResult: TJsonValue): TCompletionItems;

type
  TJsonArrayHelper = class helper for TJsonArray
    procedure Clear;
  end;

implementation

uses
  System.Character,
  System.NetEncoding,
  cSSHSupport,
  uCommonFunctions;

{ TDocPosition }

constructor TDocPosition.Create(const AFileId: string; Line, Char: integer);
begin
  Self.FileId := AFileId;
  Self.Line := Line;
  Self.Char := Char;
end;

function FormatJSON(const json: string): string;
var
  tmpJson: TJsonValue;
begin
  tmpJson := TJSONObject.ParseJSONValue(json);
  Result := tmpJson.Format;
  FreeAndNil(tmpJson);
end;


function FileIdToURI(const FilePath: string): string;
var
  ServerName, FileName: string;
begin
  if FindDelimiter(':\/', FilePath) > 0 then
  begin
    if TSSHFileName.Parse(FilePath, ServerName, FileName) then
    begin
      if not FileName[1].IsInArray(['\', '/']) then
        FileName := '\' + FileName;
      Result := 'file://SSH/' + ServerName + FileName;
    end
    else
      Result := FilePathToURI(FilePath);
  end
  else
    // Not sure how to handle unsaved files
    // and used the following workaround
    Result := 'file://untitled/temp/'+ FilePath;
end;

function FileIdFromURI(const URI: string): string;
begin
  if URI.StartsWith('file://untitled/temp/') then
  begin
    Result := Copy(URI, 22);
  end
  else if URI.StartsWith('file://SSH/') then
  begin
    var FilePath := Copy(URI, 12);
    var Server := StrToken(FilePath, '/');
    FilePath := TNetEncoding.URL.Decode(FilePath, []);
    if FilePath[2] <> DriveDelim then
       FilePath := '/' + FilePath;
    Result := TSSHFileName.Format(Server, FilePath);
  end
  else
    Result := URIToFilePath(URI);
end;

function LSPInitializeParams(const ClientName, ClientVersion: string;
  ClientCapabilities: TJsonObject;
  InitializationOptions: TJsonObject = nil): TJsonObject;
var
  ClientInfo: TJsonObject;
begin
  ClientInfo := TJsonObject.Create;
  ClientInfo.AddPair('name', TJSONString.Create(ClientName));
  ClientInfo.AddPair('version', TJSONString.Create(ClientVersion));
  Result := TJsonObject.Create;
  Result.AddPair('clientInfo', ClientInfo);
  Result.AddPair('rootUri', TJSONNull.Create);
  Result.AddPair('capabilities', ClientCapabilities);
  if Assigned(InitializationOptions) then
    Result.AddPair('initializationOptions', InitializationOptions);
end;

function LspPosition(Line, Char: Integer): TJsonObject;
begin
  Result := TJsonObject.Create;
  Result.AddPair('line', TJSONNumber.Create(Line));
  Result.AddPair('character', TJSONNumber.Create(Char));
end;

function LspDocPosition(const FileName: string; Line, Char: integer): TJsonObject;
begin
  Result := TJsonObject.Create;
  Result.AddPair('textDocument', LspTextDocumentIdentifier(FileName));
  Result.AddPair('position', LSPPosition(Line, Char));
end;

function LspLocationToDocPosition(Location: TJsonValue; out DocPosition: TDocPosition): Boolean;
begin
  if not (Location is TJsonObject) then Exit(False);

  Result := Location.TryGetValue<string>('uri',  DocPosition.FileId) and
      Location.TryGetValue<integer>('range.start.line',  DocPosition.Line) and
      Location.TryGetValue<integer>('range.start.character', DocPosition.Char);

  if Result then
  begin
    DocPosition.FileId := FileIdFromURI(DocPosition.FileId);
    Inc(DocPosition.Line);
    Inc(DocPosition.Char);
  end;
end;

function LspTextDocumentItem(const FileName, LanguageId, Text: string; Version: integer): TJsonObject;
begin
  Result := TJsonObject.Create;
  Result.AddPair('uri', TJSONString.Create(FileIdToURI(FileName)));
  Result.AddPair('languageId', TJSONString.Create(LanguageId));
  Result.AddPair('version', TJSONNumber.Create(Version));
  Result.AddPair('text', TJSONString.Create(Text));
end;

function LspTextDocumentIdentifier(const FileName: string): TJsonObject;
begin
  Result := TJsonObject.Create;
  Result.AddPair('uri', TJSONString.Create(FileIdToURI(FileName)));
end;

function LspVersionedTextDocumentIdentifier(const FileName: string; Version: NativeUInt): TJsonObject;
begin
  Result := LspTextDocumentIdentifier(FileName);
  Result.AddPair('version', TJSONNumber.Create(Version));
end;


function LspCompletionItems(LspResult: TJsonValue): TCompletionItems;
begin
  if LspResult = nil then Exit;

  var Items := LspResult.FindValue('items');
  if Assigned(Items) and (Items is TJSONArray) then
  begin
    SetLength(Result, TJsonArray(Items).Count);
    for var I:= 0 to Length(Result) - 1  do
    begin
      var Item := TJsonArray(Items).Items[I];
      Item.TryGetValue<string>('label', Result[I]._label);
      var TempI: integer;
      if Item.TryGetValue<integer>('kind', TempI) then
        Result[I].kind := TLspCompletionItemKind(TempI);
    end;
  end;
end;

{ TJsonArrayHelper }

procedure TJsonArrayHelper.Clear;
begin
  while Self.Count > 0 do
    Self.Remove(Self.Count - 1).Free;
end;

end.
