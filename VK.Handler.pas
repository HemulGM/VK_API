unit VK.Handler;

interface

uses
  System.Classes, System.SysUtils, REST.Client, REST.Json, REST.Types, Json,
  VK.Types, VK.Entity.Common, REST.Authenticator.OAuth,
  System.Generics.Collections;

type
  TResponse = record
  private
    Response: string;
    function AppendItemsTag(Json: string): string; inline;
  public
    Success: Boolean;
    Json: string;

    Error: record
      Code: Integer;
      Text: string;
    end;

    function ResponseText: string;
    function ResponseAsItems: string;
    function ResponseIsTrue: Boolean;
    function ResponseIsFalse: Boolean;
    function ResponseAsBool(var Value: Boolean): Boolean;
    function ResponseAsInt(var Value: Integer): Boolean;
    function ResponseAsInt64(var Value: Int64): Boolean;
    function ResponseAsStr(var Value: string): Boolean;
    function GetJSONValue: TJSONValue;
    function GetJSONResponse: TJSONValue;
    function GetValue<T>(const Field: string; var Value: T): Boolean; overload;
    function GetValue<T: class>(var Value: T): Boolean; overload;
    function GetObject<T: TVkEntity, constructor>(var Value: T): Boolean;
    function GetObjects<T: TVkEntity, constructor>(var Value: T): Boolean;
    function IsError: Boolean;
  end;

  TCallMethodCallback = reference to procedure(Respone: TResponse);

  TVkHandler = class
  private
    FProxyServer: string;
    FProxyPassword: string;
    FProxyPort: Integer;
    FProxyUsername: string;
    FBaseURL: string;
    FStartRequest: Cardinal;
    FRequests: Integer;
    FOnConfirm: TOnConfirm;
    FOnLog: TOnLog;
    FUseServiceKeyOnly: Boolean;
    FOwner: TObject;
    FOnCaptcha: TOnCaptcha;
    FOnAuth: TOnAuthNeeded;
    FExecuting: Integer;
    FLogging: Boolean;
    FLogResponse: Boolean;
    FCaptchaWait: Boolean;
    FAuthWait: Boolean;
    FWaitCount: Integer;
    FRequestLimit: Integer;
    FQueueLock: TObject;
    FAuthenticator: TOAuth2Authenticator;
    FParams: TDictionary<string, string>;
    function DoConfirm(Answer: string): Boolean;
    procedure SetOnConfirm(const Value: TOnConfirm);
    procedure FLog(const Value: string);
    procedure SetOnLog(const Value: TOnLog);
    procedure SetUseServiceKeyOnly(const Value: Boolean);
    procedure SetOwner(const Value: TObject);
    procedure SetOnCaptcha(const Value: TOnCaptcha);
    function FExecute(Request: TRESTRequest; IsRepeat: Boolean): TResponse;
    function GetExecuting: Boolean;
    procedure WaitForQueue;
    function ProcessResponse(Request: TRESTRequest; IsRepeat: Boolean): TResponse;
    procedure SetLogging(const Value: Boolean);
    procedure SetLogResponse(const Value: Boolean);
    procedure WaitTime(MS: Int64);
    function GetWaiting: Boolean;
    property Waiting: Boolean read GetWaiting;
    function CreateRequest(Resource: string; Params: TParams; Method: TRESTRequestMethod = rmGET): TRESTRequest;
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    procedure Log(Sender: TObject; const Text: string);
    function AskCaptcha(Sender: TObject; const CaptchaImg: string; var Answer: string): Boolean;
    function DoLogin: Boolean;
    function ExecutePost(Request: string; Params: TParams): TResponse; overload;
    function Execute(Request: string; Params: TParams): TResponse; overload;
    function Execute(Request: string; Param: TParam): TResponse; overload;
    function Execute(Request: string): TResponse; overload;
    function Execute(Request: TRESTRequest; FreeRequset: Boolean = False; IsRepeat: Boolean = False): TResponse; overload;
    property OnConfirm: TOnConfirm read FOnConfirm write SetOnConfirm;
    property OnCaptcha: TOnCaptcha read FOnCaptcha write SetOnCaptcha;
    property OnAuth: TOnAuthNeeded read FOnAuth write FOnAuth;
    property OnLog: TOnLog read FOnLog write SetOnLog;
    property UseServiceKeyOnly: Boolean read FUseServiceKeyOnly write SetUseServiceKeyOnly;
    property Owner: TObject read FOwner write SetOwner;
    property Executing: Boolean read GetExecuting;
    property Logging: Boolean read FLogging write SetLogging;
    property LogResponse: Boolean read FLogResponse write SetLogResponse;
    property Authenticator: TOAuth2Authenticator read FAuthenticator write FAuthenticator;
    /// <summary>
    /// Лимит запросов в сек (по умолчанию 3)
    /// </summary>
    property RequestLimit: Integer read FRequestLimit write FRequestLimit;
    //
    procedure AddParameter(const Name, Value: string);
    procedure DeleteParameter(const Name: string);
    function Parameter(const Name: string): string;
    property ProxyServer: string read FProxyServer write FProxyServer;
    property ProxyPassword: string read FProxyPassword write FProxyPassword;
    property ProxyPort: Integer read FProxyPort write FProxyPort;
    property ProxyUsername: string read FProxyUsername write FProxyUsername;
    property BaseURL: string read FBaseURL write FBaseURL;
  end;

var
  TestCaptcha: Boolean = False;

implementation

uses
  VK.Errors, System.SyncObjs;

{ TVkHandler }

procedure TVkHandler.WaitTime(MS: Int64);
var
  TS: Cardinal;
begin
  if MS <= 0 then
    Exit;
  Inc(FWaitCount);
  while FCaptchaWait or FAuthWait do
    Sleep(100);
  TS := TThread.GetTickCount;
  while (TS + MS > TThread.GetTickCount) do
    Sleep(100);
  Dec(FWaitCount);
end;

procedure TVkHandler.AddParameter(const Name, Value: string);
begin
  TMonitor.Enter(FParams);
  try
    FParams.Add(Name, Value);
  finally
    TMonitor.Exit(FParams);
  end;
end;

procedure TVkHandler.DeleteParameter(const Name: string);
begin
  TMonitor.Enter(FParams);
  try
    if FParams.ContainsKey(Name) then
      FParams.Remove(Name);
  finally
    TMonitor.Exit(FParams);
  end;
end;

function TVkHandler.Parameter(const Name: string): string;
begin
  TMonitor.Enter(FParams);
  try
    if not FParams.TryGetValue(Name, Result) then
      Result := '';
  finally
    TMonitor.Exit(FParams);
  end;
end;

function TVkHandler.CreateRequest(Resource: string; Params: TParams; Method: TRESTRequestMethod): TRESTRequest;
var
  Param: TParam;
  Pair: TPair<string, string>;
begin
  Result := TRESTRequest.Create(nil);
  Result.SynchronizedEvents := False;
  Result.ReadTimeout := 5000;
  Result.ConnectTimeout := 5000;
  Result.Client := TRESTClient.Create(Result);
  Result.Client.SynchronizedEvents := False;
  Result.Client.ConnectTimeout := 5000;
  Result.Client.ReadTimeout := 5000;
  Result.Response := TRESTResponse.Create(Result);
  Result.Client.Authenticator := FAuthenticator;
  Result.Client.Accept := 'application/json, text/plain; q=0.9, text/html;q=0.8,';
  Result.Client.AcceptCharset := 'UTF-8, *;q=0.8';
  TMonitor.Enter(FParams);
  try
    Result.Client.BaseURL := FBaseURL;
    Result.Client.ProxyPort := FProxyPort;
    Result.Client.ProxyServer := FProxyServer;
    Result.Client.ProxyUsername := FProxyUsername;
    Result.Client.ProxyPassword := FProxyPassword;
    for Pair in FParams do
      Result.Client.AddParameter(Pair.Key, Pair.Value);
  finally
    TMonitor.Exit(FParams);
  end;

  Result.Resource := Resource;
  Result.Method := Method;
  for Param in Params do
  begin
    if not Param[0].IsEmpty then
      Result.Params.AddItem(Param[0], Param[1]);
  end;
end;

function TVkHandler.AskCaptcha(Sender: TObject; const CaptchaImg: string; var Answer: string): Boolean;
var
  FRes: string;
begin
  Result := False;
  if Assigned(FOnCaptcha) then
  begin
    FRes := '';
    Synchronize(
      procedure
      begin
        FOnCaptcha(Sender, CaptchaImg, FRes);
      end);
    Answer := FRes;
    Result := not Answer.IsEmpty;
  end;
end;

function TVkHandler.DoLogin: Boolean;
var
  FRes: Boolean;
begin
  Result := False;
  if Assigned(FOnAuth) then
  begin
    FRes := False;
    Synchronize(
      procedure
      begin
        FOnAuth(Self, FRes);
      end);
    Result := FRes;
  end;
end;

constructor TVkHandler.Create(AOwner: TObject);
begin
  inherited Create;
  FParams := TDictionary<string, string>.Create;
  FQueueLock := TObject.Create;
  FRequestLimit := 3;
  FOwner := AOwner;
  FCaptchaWait := False;
  FAuthWait := False;
  FExecuting := 0;
  FStartRequest := 0;
  FRequests := 0;
end;

destructor TVkHandler.Destroy;
begin
  FParams.Free;
  FQueueLock.Free;
  inherited;
end;

function TVkHandler.DoConfirm(Answer: string): Boolean;
var
  FRes: Boolean;
begin
  if not Assigned(FOnConfirm) then
  begin
    Exit(True);
  end
  else
  begin
    FRes := False;
    Synchronize(
      procedure
      begin
        FOnConfirm(Self, Answer, FRes);
      end);
    Result := FRes;
  end;
end;

function TVkHandler.Execute(Request: string; Param: TParam): TResponse;
begin
  Result := Execute(CreateRequest(Request, [Param]), True);
end;

function TVkHandler.Execute(Request: string; Params: TParams): TResponse;
begin
  Result := Execute(CreateRequest(Request, Params), True);
end;

function TVkHandler.ExecutePost(Request: string; Params: TParams): TResponse;
begin
  Result := Execute(CreateRequest(Request, Params, rmPOST), True);
end;

function TVkHandler.Execute(Request: string): TResponse;
begin
  Result := Execute(CreateRequest(Request, []), True);
end;

function TVkHandler.Execute(Request: TRESTRequest; FreeRequset: Boolean; IsRepeat: Boolean): TResponse;
begin
  try
    Inc(FExecuting);
    Result := FExecute(Request, IsRepeat);
  finally
    if not Waiting then
    begin
      FCaptchaWait := False;
      FAuthWait := False;
    end;
    if FreeRequset then
      Request.Free;
    Dec(FExecuting);
  end;
end;

procedure TVkHandler.WaitForQueue;
begin
  TMonitor.Enter(FQueueLock);
  try
    Inc(FRequests);
    // Если это первый запрос, то сохраняем метку
    if FRequests = 1 then
      FStartRequest := TThread.GetTickCount;
  finally
    TMonitor.Exit(FQueueLock);
  end;
  // Если уже 3 запроса было, то ждём до конца секунды FStartRequest
  if FRequests > RequestLimit then
  begin
    FRequests := 0;
    WaitTime(1300 - Int64(TThread.GetTickCount - FStartRequest));
  end;
end;

function TVkHandler.FExecute(Request: TRESTRequest; IsRepeat: Boolean): TResponse;
begin
  Result.Success := False;
  WaitForQueue;
  if FLogging then
    FLog(Request.GetFullRequestURL);
  Request.Execute;
  if FLogging and FLogResponse then
    FLog(Request.Response.JSONText);
  Result := ProcessResponse(Request, IsRepeat);
end;

function TVkHandler.ProcessResponse(Request: TRESTRequest; IsRepeat: Boolean): TResponse;
var
  JS: TJSONValue;
  CaptchaSID: string;
  CaptchaImg: string;
  Answer: string;
begin
  Result.Error.Code := -1;
  if TestCaptcha or Request.Response.JSONValue.TryGetValue<TJSONValue>('error', JS) then
  begin
    Result.Success := False;
    Result.Error.Code := JS.GetValue<Integer>('error_code', -1);
    Result.Error.Text := JS.GetValue<string>('error_msg', VKErrors.Get(Result.Error.Code));
    if TestCaptcha then
    begin
      Result.Error.Code := VK_ERROR_CAPTCHA;
      TestCaptcha := False;
    end;
    case Result.Error.Code of
      VK_ERROR_INVALID_TOKEN:
        begin
          if not DoLogin then
            raise TVkInvalidTokenException.Create(VKErrors.Get(Result.Error.Code), Result.Error.Code);
        end;
      VK_ERROR_TOO_MANY_SIMILAR_ACTIONS:
        raise TVkTooManySimilarActionException.Create(VKErrors.Get(Result.Error.Code), Result.Error.Code);
      VK_ERROR_CAPTCHA: // Капча
        begin
          if FCaptchaWait then
            Exit(Execute(Request));
          FCaptchaWait := True;
          CaptchaSID := JS.GetValue<string>('captcha_sid', '');
          CaptchaImg := JS.GetValue<string>('captcha_img', '');

          if AskCaptcha(Self, CaptchaImg, Answer) then
          begin
            Request.Params.AddItem('captcha_sid', CaptchaSID);
            Request.Params.AddItem('captcha_key', Answer);
            FCaptchaWait := False;
            Result := Execute(Request);
            Request.Params.Delete('captcha_sid');
            Request.Params.Delete('captcha_key');
          end
          else
          begin
            FCaptchaWait := False;
            raise TVkCaptchaException.Create(Result.Error.Text, Result.Error.Code);
          end;
        end;
      VK_ERROR_CONFIRM, VK_ERROR_TOKEN_CONFIRM, VK_ERROR_MORE_CONFIRM: // Подтверждение для ВК
        begin
          Answer := JS.GetValue<string>('confirmation_text', '');
          if DoConfirm(Answer) then
          begin
            Request.Params.AddItem('confirm', '1');
            Result := Execute(Request);
            Request.Params.Delete('confirm');
          end
          else
            raise TVkConfirmException.Create(Result.Error.Text, Result.Error.Code);
        end;
      VK_ERROR_REQUESTLIMIT: // Превышено кол-во запросов в сек
        begin
          FLog(Format('Превышено кол-во запросов в сек. (%d/%d, StartRequest %d)', [FRequests, RequestLimit, FStartRequest]));
          if not IsRepeat then
          begin
            WaitTime(2000);
            Result := Execute(Request, False, True);
          end
          else
            raise TVkExecuteErrorException.Create(Result.Error.Text, Result.Error.Code);
        end;
      VK_ERROR_INTERNAL_SERVER: // Internal Server Error
        begin
          if not IsRepeat then
          begin
            WaitTime(1000);
            Result := Execute(Request, False, True);
          end
          else
            raise TVkExecuteErrorException.Create(Result.Error.Text, Result.Error.Code);
        end;
      VK_ERROR_ACCESS_DENIED, VK_ERROR_ACCESS_DENIED_POST:
        raise TVkAccessDeniedException.Create(Result.Error.Text, Result.Error.Code);
    else
      raise TVkUnknownMethodException.Create(Result.Error.Text, Result.Error.Code);
    end;
  end
  else
  begin
    if Request.Response.StatusCode = 200 then
    begin
      if Request.Response.JSONValue.TryGetValue<TJSONValue>('response', JS) then
      begin
        Result.Response := JS.ToJSON;
        Result.Json := Request.Response.JSONText;
        Result.Success := True;
      end;
    end
    else
      raise TVkParserException.Create('Неизвестный ответ от сервера: ' + Request.Response.StatusCode.ToString);
  end;
end;

procedure TVkHandler.FLog(const Value: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, Value);
end;

function TVkHandler.GetExecuting: Boolean;
begin
  Result := FExecuting > 0;
end;

function TVkHandler.GetWaiting: Boolean;
begin
  Result := FWaitCount > 0;
end;

procedure TVkHandler.Log(Sender: TObject; const Text: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Sender, Text);
end;

procedure TVkHandler.SetOnLog(const Value: TOnLog);
begin
  FOnLog := Value;
end;

procedure TVkHandler.SetOwner(const Value: TObject);
begin
  FOwner := Value;
end;

procedure TVkHandler.SetUseServiceKeyOnly(const Value: Boolean);
begin
  FUseServiceKeyOnly := Value;
end;

procedure TVkHandler.SetLogging(const Value: Boolean);
begin
  FLogging := Value;
end;

procedure TVkHandler.SetLogResponse(const Value: Boolean);
begin
  FLogResponse := Value;
end;

procedure TVkHandler.SetOnCaptcha(const Value: TOnCaptcha);
begin
  FOnCaptcha := Value;
end;

procedure TVkHandler.SetOnConfirm(const Value: TOnConfirm);
begin
  FOnConfirm := Value;
end;

{ TResponse }

{$WARNINGS OFF}

function TResponse.GetJSONValue: TJSONValue;
begin
  if not Json.IsEmpty then
    Result := TJSONObject.ParseJSONValue(UTF8ToString(Json))
  else
    Result := nil;
end;

function TResponse.GetObject<T>(var Value: T): Boolean;
begin
  Result := Success;
  if Result then
  try
    Value := T.FromJsonString<T>(Response);
  except
    Result := False;
  end;
end;

function TResponse.GetObjects<T>(var Value: T): Boolean;
begin
  Result := Success;
  if Result then
  try
    Value := T.FromJsonString<T>(ResponseAsItems);
  except
    Result := False;
  end;
end;

function TResponse.GetValue<T>(var Value: T): Boolean;
var
  JSONItem: TJSONValue;
begin
  Result := Success;
  if Result then
  try
    JSONItem := TJSONObject.ParseJSONValue(Response);
    Value := T(JSONItem);
  except
    JSONItem.Free;
    Result := False;
  end;
end;

function TResponse.GetValue<T>(const Field: string; var Value: T): Boolean;
var
  JSONItem: TJSONValue;
begin
  Result := Success;
  if Result then
  begin
    try
      JSONItem := TJSONObject.ParseJSONValue(Response);
      try
        Result := JSONItem.TryGetValue<T>(Field, Value);
      finally
        JSONItem.Free;
      end;
    except
      Result := False;
    end;
  end;
end;

function TResponse.IsError: Boolean;
begin
  Result := (not Success) or (Error.Code <> -1);
end;

function TResponse.ResponseIsFalse: Boolean;
begin
  Result := Success and (Response = '0');
end;

function TResponse.ResponseAsInt(var Value: Integer): Boolean;
begin
  Result := Success and TryStrToInt(Response, Value);
end;

function TResponse.ResponseAsInt64(var Value: Int64): Boolean;
begin
  Result := Success and TryStrToInt64(Response, Value);
end;

function TResponse.ResponseAsStr(var Value: string): Boolean;
begin
  Result := Success;
  if Result then
    Value := Response;
end;

function TResponse.ResponseAsBool(var Value: Boolean): Boolean;
begin
  Result := Success;
  if Result then
    Value := ResponseIsTrue;
end;

function TResponse.ResponseIsTrue: Boolean;
begin
  Result := Success and (Response = '1');
end;

function TResponse.AppendItemsTag(Json: string): string;
begin
  Result := '{"Items": ' + Json + '}';
end;

function TResponse.ResponseAsItems: string;
begin
  Result := AppendItemsTag(Response);
end;

function TResponse.ResponseText: string;
begin
  Result := Response;
end;

function TResponse.GetJSONResponse: TJSONValue;
begin
  if not Response.IsEmpty then
    Result := TJSONObject.ParseJSONValue(UTF8ToString(Response))
  else
    Result := nil;
end;
{$WARNINGS ON}

end.

