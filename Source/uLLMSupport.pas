unit uLLMSupport;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.ImageList,
  System.Actions,
  System.Generics.Collections,
  System.JSON,
  System.JSON.Serializers,
  System.Net.HttpClient,
  System.Net.HttpClientComponent;

type

  TEndpointType = (
    etUnsupported,
    etOllamaGenerate,
    etOllamaChat,
    etCompletion,
    etChatCompletion);

  TLLMSettings = record
    EndPoint: string;
    ApiKey: string;
    Model: string;
    TimeOut: Integer;
    MaxTokens: Integer;
    SystemPrompt: string;
    function IsLocal: Boolean;
    function EndpointType: TEndpointType;
  end;

  TQAItem = record
    Question: string;
    Answer: string;
    constructor Create(const AQuestion, AnAnswer: string);
  end;

  TChatTopic = record
    Title: string;
    QAItems: TArray<TQAItem>;
  end;
  TChatTopics = TArray<TChatTopic>;

  TOnLLMResponseEvent = procedure(Sender: TObject; const Question, Answer: string) of object;
  TOnLLMErrorEvent = procedure(Sender: TObject; const Error: string) of object;

  TLLMChat = class
  private
    FHttpClient: TNetHTTPClient;
    FSourceStream: TStringStream;
    FOnLLMResponse: TOnLLMResponseEvent;
    FOnLLMError: TOnLLMErrorEvent;
    FLastPrompt: string;
    FContext: TJSONValue;
    FEndPointType: TEndpointType;
    FIsBusy: Boolean;
    FSerializer: TJsonSerializer;
    function CompletionParams(const Prompt: string): string;
    function ChatCompletionParams(const Prompt: string): string;
    procedure OnRequestError(const Sender: TObject; const AError: string);
    procedure OnRequestCompleted(const Sender: TObject; const AResponse: IHTTPResponse);
  public
    Settings: TLLMSettings;
    ActiveTopicIndex: Integer;
    ChatTopics: TArray<TChatTopic>;
    constructor Create(const LLMSettings: TLLMSettings);
    destructor Destroy; override;

    procedure ClearContext;
    function ActiveTopic: TChatTopic;
    procedure NextTopic;
    procedure PreviousTopic;
    procedure ClearTopic;
    procedure RemoveTopic;
    procedure NewTopic;

    procedure Ask(const Question: string);
    procedure SaveChat(const FName: string);
    procedure LoadChat(const FName: string);
    procedure SaveSettings(const FName: string);
    procedure LoadSettrings(const FName: string);

    property IsBusy: Boolean read FIsBusy;
    property OnLLMResponse: TOnLLMResponseEvent read FOnLLMResponse write FOnLLMResponse;
    property OnLLMError: TOnLLMErrorEvent read FOnLLMError write FOnLLMError;
  end;


implementation

uses
  System.Math,
  System.IOUtils;

resourcestring
  sLLMBusy = 'The LLM client is busy';
  sNoAPIKey = 'The API key is missing';
  sNoResponse = 'No response from the LLM Server';
  sUnsupportedEndpoint = 'The endpoint is not supported';
  sUnexpectedResponse = 'Unexpected response from the LLM Server';

function Obfuscate(const s: string): string;
// Reversible string obfuscation using the ROT13 algorithm
var
  I: integer;
begin
  result := s;
  for I := 1 to length(s) do
    case ord(s[I]) of
    ord('A')..ord('M'),ord('a')..ord('m'): result[I] := chr(ord(s[I])+13);
    ord('N')..ord('Z'),ord('n')..ord('z'): result[I] := chr(ord(s[I])-13);
    ord('0')..ord('4'): result[I] := chr(ord(s[I])+5);
    ord('5')..ord('9'): result[I] := chr(ord(s[I])-5);
    end;
end;

{ TLLMChat }

function TLLMChat.ActiveTopic: TChatTopic;
begin
  Result := ChatTopics[ActiveTopicIndex];
end;

procedure TLLMChat.Ask(const Question: string);
var
  ErrMsg: string;
  Params: string;
begin
  if Question = '' then Exit;

  if FIsBusy then ErrMsg := sLLMBusy;
  if not Settings.IsLocal and (Settings.ApiKey = '' ) then ErrMsg := sNoAPIKey;
  FEndPointType := Settings.EndpointType;
  if FEndPointType = etUnsupported then ErrMsg := sUnsupportedEndpoint;

  if ErrMsg <> '' then
  begin
    if Assigned(FOnLLMError) then
      FOnLLMError(Self, ErrMsg);
    Exit;
  end;

  FHttpClient.ConnectionTimeout := Settings.TimeOut;
  FHttpClient.ResponseTimeout := Settings.TimeOut * 2;

  FLastPrompt := Question;
  FIsBusy := True;

  case FEndPointType of
    etOllamaGenerate, etCompletion: Params := CompletionParams(Question);
    etOllamaChat, etChatCompletion: Params := ChatCompletionParams(Question);
  end;

  FSourceStream.Clear;
  FSourceStream.WriteString(Params);
  FSourceStream.Position := 0;

  if not (FEndPointType in [etOllamaGenerate, etOllamaChat]) then
    FHttpClient.CustomHeaders['Authorization'] := 'Bearer ' + Settings.ApiKey;
  FHttpClient.CustomHeaders['Content-Type'] := 'application/json';
  FHttpClient.CustomHeaders['AcceptEncoding'] := 'deflate, gzip;q=1.0, *;q=0.5';
  FHttpClient.Post(Settings.EndPoint , FSourceStream);
end;

function TLLMChat.ChatCompletionParams(const Prompt: string): string;
  function NewMessage(const Role, Content: string): TJsonObject;
  begin
    Result := TJsonObject.Create;
    Result.AddPair('role', Role);
    Result.AddPair('content', Content);
  end;

var
  JSON: TJsonObject;
  Messages: TJSONArray;
begin
  JSON := TJSONObject.Create;
  try
    JSON.AddPair('model', Settings.Model);
    JSON.AddPair('stream', False);

    case FEndPointType of
      etOllamaGenerate:
        begin
          var Options := TJSONObject.Create;
          Options.AddPair('num_predict', Settings.MaxTokens);
          JSON.AddPair('options', Options);
        end;
      etCompletion:
        JSON.AddPair('max_tokens', Settings.MaxTokens);
    end;

    Messages := TJSONArray.Create;
    // start with the system message
    Messages.Add(NewMessage('system', Settings.SystemPrompt));
    // add the history
    for var QAItem in ActiveTopic.QAItems do
    begin
      Messages.Add(NewMessage('user', QAItem.Question));
      Messages.Add(NewMessage('assistant', QAItem.Answer));
    end;
    // finally add the new prompt
    Messages.Add(NewMessage('user', Prompt));

    JSON.AddPair('messages', Messages);

    Result := JSON.ToJSON;
  finally
    JSON.Free;
  end;
end;

procedure TLLMChat.ClearContext;
begin
  FreeAndNil(FContext);
end;

procedure TLLMChat.ClearTopic;
begin
  ChatTopics[ActiveTopicIndex] := default(TChatTopic);
  ClearContext;
end;

function TLLMChat.CompletionParams(const Prompt: string): string;
var
  JSON: TJsonObject;
begin
  JSON := TJSONObject.Create;
  try
    JSON.AddPair('model', Settings.Model);
    JSON.AddPair('stream', False);
    JSON.AddPair('prompt', Prompt);
    case FEndPointType of
      etOllamaGenerate:
        begin
          JSON.AddPair('system', Settings.SystemPrompt);
          //JSON.AddPair('system', FSystemPrompt {+ ' Please surround code with triple ticks'});
          if Assigned(FContext) then
            JSON.AddPair('context', FContext);

          var Options := TJSONObject.Create;
          Options.AddPair('num_predict', Settings.MaxTokens);
          JSON.AddPair('options', Options);
        end;
      etCompletion:
        JSON.AddPair('max_tokens', Settings.MaxTokens);
    end;

    Result := JSON.ToJSON;
  finally
    JSON.Free;
  end;
end;

constructor TLLMChat.Create(const LLMSettings: TLLMSettings);
begin
  Settings := LLMSettings;

  FHttpClient := TNetHTTPClient.Create(nil);
  FHttpClient.OnRequestCompleted := OnRequestCompleted;
  FHttpClient.OnRequestError := OnRequestError;
  FHttpClient.Asynchronous := True;

  FSourceStream := TStringStream.Create('', TEncoding.UTF8);

  FSerializer := TJsonSerializer.Create;

  ChatTopics := [default(TChatTopic)];
  ActiveTopicIndex := 0;
end;

destructor TLLMChat.Destroy;
begin
  FSerializer.Free;
  FSourceStream.Free;
  FHttpClient.Free;
  inherited;
end;

procedure TLLMChat.LoadChat(const FName: string);
begin
  if FileExists(FName) then
  begin
    ChatTopics :=
      FSerializer.Deserialize<TArray<TChatTopic>>(
      TFile.ReadAllText(FName, TEncoding.UTF8));
    ActiveTopicIndex := High(ChatTopics);
  end;
end;

procedure TLLMChat.LoadSettrings(const FName: string);
begin
  if FileExists(FName) then
  begin
    Settings := FSerializer.Deserialize<TLLMSettings>(TFile.ReadAllText(FName));
    Settings.ApiKey := Obfuscate(Settings.ApiKey);
  end;
end;

procedure TLLMChat.NewTopic;
begin
  if Length(ActiveTopic.QAItems) = 0 then
    Exit;
  if Length(ChatTopics[High(ChatTopics)].QAItems) > 0 then
    ChatTopics := ChatTopics + [default(TChatTopic)];
  ActiveTopicIndex := High(ChatTopics);
end;

procedure TLLMChat.NextTopic;
begin
  if ActiveTopicIndex < Length(ChatTopics) - 1 then
  begin
    Inc(ActiveTopicIndex);
    ClearContext;
  end;
end;

procedure TLLMChat.OnRequestCompleted(const Sender: TObject;
  const AResponse: IHTTPResponse);
var
  ResponseData: TBytes;
  ResponseOK: Boolean;
  ErrMsg, Msg: string;
begin
  ResponseOK := False;
  if AResponse.ContentStream.Size > 0 then
  begin
    SetLength(ResponseData, AResponse.ContentStream.Size);
    AResponse.ContentStream.Read(ResponseData, AResponse.ContentStream.Size);
    var JsonResponse := TJsonValue.ParseJSONValue(ResponseData, 0);
    try
      if not (JsonResponse.TryGetValue('error.message', ErrMsg)
        or JsonResponse.TryGetValue('error', ErrMsg))
      then
        case FEndPointType of
          etChatCompletion:
            ResponseOK := JsonResponse.TryGetValue('choices[0].message.content', Msg);
          etCompletion:
            ResponseOK := JsonResponse.TryGetValue('choices[0].text', Msg);
          etOllamaGenerate:
            ResponseOK := JsonResponse.TryGetValue('response', Msg);
          etOllamaChat:
            ResponseOK := JsonResponse.TryGetValue('message.content', Msg);
        end;

      if FEndPointType = etOllamaGenerate then
      begin
        ClearContext;
        FContext := JsonResponse.FindValue('context');
        if Assigned(FContext) then
          FContext.Owned := False;
      end;
    finally
      JsonResponse.Free;
    end;
  end else
    ErrMsg := sNoResponse;

  if ResponseOK then
  begin
    ChatTopics[ActiveTopicIndex].QAItems := ActiveTopic.QAItems + [TQAItem.Create(FLastPrompt, Msg)];
    if Assigned(FOnLLMResponse)  then
      FOnLLMResponse(Self, FLastPrompt, Msg);
  end
  else
  begin
    if ErrMsg = '' then
      ErrMsg := sUnexpectedResponse;
    if Assigned(FOnLLMError) then
      FOnLLMError(Self, ErrMsg);
  end;
  FIsBusy := False;
end;

procedure TLLMChat.OnRequestError(const Sender: TObject;
  const AError: string);
begin
  if Assigned(FOnLLMError) then
    FOnLLMError(Self, AError);
  FIsBusy := False;
end;

procedure TLLMChat.PreviousTopic;
begin
  if ActiveTopicIndex > 0 then
  begin
    Dec(ActiveTopicIndex);
    ClearContext;
  end;
end;

procedure TLLMChat.RemoveTopic;
begin
  Delete(ChatTopics, ActiveTopicIndex, 1);

  if ActiveTopicIndex > High(ChatTopics) then
  begin
    if ActiveTopicIndex > 0 then
      Dec(ActiveTopicIndex)
    else
      ChatTopics := [default(TChatTopic)];
  end;
  ClearContext;
end;

procedure TLLMChat.SaveChat(const FName: string);
begin
  TFile.WriteAllText(FName, FSerializer.Serialize(ChatTopics));
end;

procedure TLLMChat.SaveSettings(const FName: string);
begin
  Settings.ApiKey := Obfuscate(Settings.ApiKey);
  try
    TFile.WriteAllText(FName, FSerializer.Serialize<TLLMSettings>(Settings));
  finally
    Settings.ApiKey := Obfuscate(Settings.ApiKey);
  end;
end;

{ TQAItem }

constructor TQAItem.Create(const AQuestion, AnAnswer: string);
begin
  Self.Question := AQuestion;
  Self.Answer := AnAnswer;
end;

{ TLLMSettings }

function TLLMSettings.EndpointType: TEndpointType;
begin
  Result := etUnsupported;
  if IsLocal then
  begin
    if Endpoint.EndsWith('api/generate') then
      Result := etOllamaGenerate
    else if Endpoint.EndsWith('api/chat') then
      Result := etOllamaChat;
  end
  else if EndPoint.EndsWith('chat/completions') then
    Result := etChatCompletion
  else if EndPoint.EndsWith('completions') then
    Result := etCompletion;
end;

function TLLMSettings.IsLocal: Boolean;
begin
  Result := EndPoint.Contains('localhost')  or EndPoint.Contains('127.0.0.1');
end;

end.
