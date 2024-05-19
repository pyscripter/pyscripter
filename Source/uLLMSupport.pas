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
    etOpenAICompletion,
    etOpenAIChatCompletion);

  TLLMSettingsValidation = (
    svValid,
    svModelEmpty,
    svInvalidEndpoint,
    svAPIKeyMissing);

  TLLMSettings = record
    EndPoint: string;
    ApiKey: string;
    Model: string;
    TimeOut: Integer;
    MaxTokens: Integer;
    SystemPrompt: string;
    function Validate: TLLMSettingsValidation;
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

  TLLMBase = class
  private
    FHttpClient: TNetHTTPClient;
    FSourceStream: TStringStream;
    FOnLLMResponse: TOnLLMResponseEvent;
    FOnLLMError: TOnLLMErrorEvent;
    FLastPrompt: string;
    FContext: TJSONValue;
    FEndPointType: TEndpointType;
    FIsBusy: Boolean;
    procedure OnRequestError(const Sender: TObject; const AError: string);
    procedure OnRequestCompleted(const Sender: TObject; const AResponse: IHTTPResponse);
  protected
    FSerializer: TJsonSerializer;
    procedure DoResponseOK(const Msg: string); virtual;
    function RequestParams(const Prompt: string): string; virtual; abstract;
  public
    Settings: TLLMSettings;
    ActiveTopicIndex: Integer;
    ChatTopics: TArray<TChatTopic>;
    procedure ClearContext;
    function ValidateSettings: TLLMSettingsValidation; virtual;
    function ValidationErrMsg(Validation: TLLMSettingsValidation): string;
    constructor Create;
    destructor Destroy; override;

    procedure Ask(const Question: string);
    procedure SaveSettings(const FName: string);
    procedure LoadSettrings(const FName: string);

    property IsBusy: Boolean read FIsBusy;
    property OnLLMResponse: TOnLLMResponseEvent read FOnLLMResponse write FOnLLMResponse;
    property OnLLMError: TOnLLMErrorEvent read FOnLLMError write FOnLLMError;
  end;

  TLLMChat = class(TLLMBase)
  protected
    procedure DoResponseOK(const Msg: string); override;
    function RequestParams(const Prompt: string): string; override;
  public
    ActiveTopicIndex: Integer;
    ChatTopics: TArray<TChatTopic>;
    function ValidateSettings: TLLMSettingsValidation; override;
    constructor Create;

    function ActiveTopic: TChatTopic;
    procedure NextTopic;
    procedure PreviousTopic;
    procedure ClearTopic;
    procedure RemoveTopic;
    procedure NewTopic;

    procedure SaveChat(const FName: string);
    procedure LoadChat(const FName: string);
  end;

  TLLMAssistant = class(TLLMBase)
  protected
    function RequestParams(const Prompt: string): string; override;
  public
    function ValidateSettings: TLLMSettingsValidation; override;
    constructor Create;
  end;

const
  GPT_4_Settings: TLLMSettings = (
    EndPoint: 'https://api.openai.com/v1/chat/completions';
    ApiKey: '';
    Model: 'gpt-4o';
    TimeOut: 20000;
    MaxTokens: 1000;
    SystemPrompt: 'You are my expert python coding assistant.');

  GPT_35_Settings: TLLMSettings = (
    EndPoint: 'https://api.openai.com/v1/chat/completions';
    ApiKey: '';
    Model: 'gpt-3.5-turbo';
    TimeOut: 20000;
    MaxTokens: 1000;
    SystemPrompt: 'You are my expert python coding assistant.');

  GPT_35_Instruct_Settings: TLLMSettings = (
    EndPoint: 'https://api.openai.com/v1/completions';
    ApiKey: '';
    Model: 'gpt-3.5-turbo-instruct';
    TimeOut: 20000;
    MaxTokens: 1000;
    SystemPrompt: '');

  OllamaSettings: TLLMSettings = (
    EndPoint: 'http://localhost:11434/api/chat';
    ApiKey: '';
    Model: 'codegema';
    //Model: 'starcoder2';
    //Model: 'codellama:';
    //Model: 'stable-code';
    TimeOut: 60000;
    MaxTokens: 1000;
    SystemPrompt: 'You are my expert python coding assistant.');

implementation

uses
  System.Math,
  System.IOUtils;

resourcestring
  sLLMBusy = 'The LLM client is busy';
  sNoResponse = 'No response from the LLM Server';
  sNoAPIKey = 'The LLM API key is missing';
  sNoModel = 'The LLM model has not been set';
  sUnsupportedEndpoint = 'The LLM endpoint is missing or not supported';
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

{ TLLMBase }

procedure TLLMBase.Ask(const Question: string);
var
  ErrMsg: string;
  Params: string;
begin
  if Question = '' then Exit;

  if FIsBusy then
    ErrMsg := sLLMBusy
  else
  begin
    var Validation := ValidateSettings;
    ErrMsg := ValidationErrMsg(Validation);
  end;

  if ErrMsg <> '' then
  begin
    if Assigned(FOnLLMError) then
      FOnLLMError(Self, ErrMsg);
    Exit;
  end;

  FEndPointType := Settings.EndpointType;
  FHttpClient.ConnectionTimeout := Settings.TimeOut;
  FHttpClient.ResponseTimeout := Settings.TimeOut * 2;

  FLastPrompt := Question;
  FIsBusy := True;

  Params := RequestParams(Question);

  FSourceStream.Clear;
  FSourceStream.WriteString(Params);
  FSourceStream.Position := 0;

  if FEndPointType in [etOpenAICompletion, etOpenAIChatCompletion] then
    FHttpClient.CustomHeaders['Authorization'] := 'Bearer ' + Settings.ApiKey;
  FHttpClient.CustomHeaders['Content-Type'] := 'application/json';
  FHttpClient.CustomHeaders['AcceptEncoding'] := 'deflate, gzip;q=1.0, *;q=0.5';
  FHttpClient.Post(Settings.EndPoint , FSourceStream);
end;

procedure TLLMBase.ClearContext;
begin
  FreeAndNil(FContext);
end;

constructor TLLMBase.Create;
begin
  inherited;
  Settings := GPT_4_Settings;
  FHttpClient := TNetHTTPClient.Create(nil);
  FHttpClient.OnRequestCompleted := OnRequestCompleted;
  FHttpClient.OnRequestError := OnRequestError;
  FHttpClient.Asynchronous := True;

  FSourceStream := TStringStream.Create('', TEncoding.UTF8);

  FSerializer := TJsonSerializer.Create;
end;

destructor TLLMBase.Destroy;
begin
  FSerializer.Free;
  FSourceStream.Free;
  FHttpClient.Free;
  FContext.Free;
  inherited;
end;

procedure TLLMBase.DoResponseOK(const Msg: string);
begin
  // Do nothing
end;

procedure TLLMBase.LoadSettrings(const FName: string);
begin
  if FileExists(FName) then
  begin
    Settings := FSerializer.Deserialize<TLLMSettings>(TFile.ReadAllText(FName));
    Settings.ApiKey := Obfuscate(Settings.ApiKey);
  end;
end;

procedure TLLMBase.OnRequestCompleted(const Sender: TObject;
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
          etOpenAIChatCompletion:
            ResponseOK := JsonResponse.TryGetValue('choices[0].message.content', Msg);
          etOpenAICompletion:
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
    DoResponseOK(Msg);
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

procedure TLLMBase.OnRequestError(const Sender: TObject; const AError: string);
begin
  if Assigned(FOnLLMError) then
    FOnLLMError(Self, AError);
  FIsBusy := False;
end;

procedure TLLMBase.SaveSettings(const FName: string);
begin
  Settings.ApiKey := Obfuscate(Settings.ApiKey);
  try
    TFile.WriteAllText(FName, FSerializer.Serialize<TLLMSettings>(Settings));
  finally
    Settings.ApiKey := Obfuscate(Settings.ApiKey);
  end;
end;

function TLLMBase.ValidateSettings: TLLMSettingsValidation;
begin
  Result := Settings.Validate;
end;

function TLLMBase.ValidationErrMsg(Validation: TLLMSettingsValidation): string;
begin
  case Validation of
    svValid: Result := '';
    svModelEmpty: Result := sNoModel;
    svInvalidEndpoint: Result := sUnsupportedEndpoint;
    svAPIKeyMissing: Result := sNoAPIKey;
  end;
end;

{ TLLMChat }

function TLLMChat.ActiveTopic: TChatTopic;
begin
  Result := ChatTopics[ActiveTopicIndex];
end;

procedure TLLMChat.ClearTopic;
begin
  ChatTopics[ActiveTopicIndex] := default(TChatTopic);
  ClearContext;
end;

constructor TLLMChat.Create;
begin
  inherited;
  Settings := GPT_4_Settings;

  ChatTopics := [default(TChatTopic)];
  ActiveTopicIndex := 0;
end;

procedure TLLMChat.DoResponseOK(const Msg: string);
begin
  ChatTopics[ActiveTopicIndex].QAItems := ActiveTopic.QAItems + [TQAItem.Create(FLastPrompt, Msg)];
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

procedure TLLMChat.PreviousTopic;
begin
  if ActiveTopicIndex > 0 then
  begin
    Dec(ActiveTopicIndex);
    ClearContext;
  end;
end;

function TLLMChat.RequestParams(const Prompt: string): string;

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
      etOllamaChat:
        begin
          var Options := TJSONObject.Create;
          Options.AddPair('num_predict', Settings.MaxTokens);
          JSON.AddPair('options', Options);
        end;
      etOpenAIChatCompletion:
        JSON.AddPair('max_tokens', Settings.MaxTokens);
    end;

    Messages := TJSONArray.Create;
    // start with the system message
    if Settings.SystemPrompt <> '' then
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

function TLLMChat.ValidateSettings: TLLMSettingsValidation;
begin
  Result := Settings.Validate;
  if (Result = svValid) and
    not (Settings.EndPointType in [etOllamaChat, etOpenAIChatCompletion])
  then
    Result := svInvalidEndpoint;
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
  if Endpoint.Contains('openai') then
  begin
    if EndPoint.EndsWith('chat/completions') then
      Result := etOpenAIChatCompletion
    else if EndPoint.EndsWith('completions') then
      Result := etOpenAICompletion;
  end
  else
  begin
    if Endpoint.EndsWith('api/generate') then
      Result := etOllamaGenerate
    else if Endpoint.EndsWith('api/chat') then
      Result := etOllamaChat;
  end
end;

function TLLMSettings.IsLocal: Boolean;
begin
  Result := EndPoint.Contains('localhost')  or EndPoint.Contains('127.0.0.1');
end;

function TLLMSettings.Validate: TLLMSettingsValidation;
begin
  if Model = '' then
    Exit(svModelEmpty);
  case EndpointType of
    etUnsupported: Exit(svInvalidEndpoint);
    etOpenAICompletion, etOpenAIChatCompletion:
      if ApiKey = '' then
        Exit(svAPIKeyMissing);
  end;
  Result := svValid;
end;


{ TLLMAssistant }

constructor TLLMAssistant.Create;
begin
  inherited;
  Settings := GPT_35_Instruct_Settings;
end;

function TLLMAssistant.RequestParams(const Prompt: string): string;
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
          if Assigned(FContext) then
            JSON.AddPair('context', FContext);

          var Options := TJSONObject.Create;
          Options.AddPair('num_predict', Settings.MaxTokens);
          JSON.AddPair('options', Options);
        end;
      etOpenAICompletion:
        JSON.AddPair('max_tokens', Settings.MaxTokens);
    end;

    Result := JSON.ToJSON;
  finally
    JSON.Free;
  end;
end;

function TLLMAssistant.ValidateSettings: TLLMSettingsValidation;
begin
  Result := Settings.Validate;
  if (Result = svValid) and
    not (Settings.EndPointType in [etOllamaGenerate, etOpenAICompletion])
  then
    Result := svInvalidEndpoint;
end;


end.
