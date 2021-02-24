unit VK.Handler;

interface

uses
  System.Classes, System.SysUtils,
  {$IFDEF NEEDFMX}
  FMX.Types, FMX.Forms,
  {$ELSE}
  Vcl.Forms,
  {$ENDIF}
  REST.Client, REST.Json, REST.Types, JSON, VK.Types, VK.Entity.Common;

type
  TResponse = record
  private
    function AppendItemsTag(JSON: string): string; inline;
  public
    Success: Boolean;
    Response: string;
    JSON: string;
    Error: record
      Code: Integer;
      Text: string;
    end;
    function ResponseAsItems: string;
    function ResponseIsTrue: Boolean;
    function ResponseIsFalse: Boolean;
    function ResponseAsBool(var Value: Boolean): Boolean;
    function ResponseAsInt(var Value: Integer): Boolean;
    function ResponseAsStr(var Value: string): Boolean;
    function GetJSONValue: TJSONValue;
    function GetJSONResponse: TJSONValue;
    function GetValue<T>(const Field: string; var Value: T): Boolean;
    function GetObject<T: TVkEntity, constructor>(var Value: T): Boolean;
    function GetObjects<T: TVkEntity, constructor>(var Value: T): Boolean;
    function IsError: Boolean;
  end;

  TCallMethodCallback = reference to procedure(Respone: TResponse);

  TRequestConstruct = class
    class var
      Client: TRESTClient;
  public
    class function Request(Resource: string; Params: TParams; Method: TRESTRequestMethod = rmGET): TRESTRequest;
  end;

  TVkHandler = class
    const
      RequestLimit = 3; //Round(1000 / 3) + 10; //задержка между запросами 3 запроса в секунду + 10 мс страховка
  private
    FStartRequest: Cardinal;
    FRequests: Integer;
    FRESTClient: TRESTClient;
    FOnConfirm: TOnConfirm;
    FOnError: TOnVKError;
    FOnLog: TOnLog;
    FUseServiceKeyOnly: Boolean;
    FOwner: TObject;
    FOnCaptcha: TOnCaptcha;
    FExecuting: Integer;
    FUsePseudoAsync: Boolean;
    FLogging: Boolean;
    FLogResponse: Boolean;
    FCaptchaWait: Boolean;
    FCancelAll: Boolean;
    FWaitCount: Integer;
    function DoConfirm(Answer: string): Boolean;
    function DoProcError(Sender: TObject; E: Exception; Code: Integer; Text: string): Boolean;
    procedure SetOnConfirm(const Value: TOnConfirm);
    procedure SetOnError(const Value: TOnVKError);
    procedure FLog(const Value: string);
    procedure SetOnLog(const Value: TOnLog);
    procedure SetUseServiceKeyOnly(const Value: Boolean);
    procedure SetOwner(const Value: TObject);
    procedure SetOnCaptcha(const Value: TOnCaptcha);
    function FExecute(Request: TRESTRequest): TResponse;
    function GetExecuting: Boolean;
    procedure SetUsePseudoAsync(const Value: Boolean);
    procedure WaitForQueue;
    function ProcessResponse(Request: TRESTRequest): TResponse;
    procedure SetLogging(const Value: Boolean);
    procedure SetLogResponse(const Value: Boolean);
    procedure WaitTime(MS: Int64);
    function GetWaiting: Boolean;
    property Waiting: Boolean read GetWaiting;
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    procedure Log(Sender: TObject; const Text: string);
    function AskCaptcha(Sender: TObject; const CaptchaImg: string; var Answer: string): Boolean;
    function ExecutePost(Request: string; Params: TParams): TResponse; overload;
    function Execute(Request: string; Params: TParams): TResponse; overload;
    function Execute(Request: string; Param: TParam): TResponse; overload;
    function Execute(Request: string): TResponse; overload;
    function Execute(Request: TRESTRequest; FreeRequset: Boolean = False): TResponse; overload;
    property Client: TRESTClient read FRESTClient;
    property OnConfirm: TOnConfirm read FOnConfirm write SetOnConfirm;
    property OnCaptcha: TOnCaptcha read FOnCaptcha write SetOnCaptcha;
    property OnError: TOnVKError read FOnError write SetOnError;
    property OnLog: TOnLog read FOnLog write SetOnLog;
    property UseServiceKeyOnly: Boolean read FUseServiceKeyOnly write SetUseServiceKeyOnly;
    property Owner: TObject read FOwner write SetOwner;
    property Executing: Boolean read GetExecuting;
    property UsePseudoAsync: Boolean read FUsePseudoAsync write SetUsePseudoAsync;
    property Logging: Boolean read FLogging write SetLogging;
    property LogResponse: Boolean read FLogResponse write SetLogResponse;
  end;

var
  TestCaptcha: Boolean = False;

implementation

uses
  VK.Errors;

procedure TVkHandler.WaitTime(MS: Int64);
var
  TS: Cardinal;
begin
  if MS <= 0 then
    Exit;
  Inc(FWaitCount);
  while FCaptchaWait do
    Sleep(100);
  TS := TThread.GetTickCount;
  while (TS + MS > TThread.GetTickCount) do
    Sleep(100);
  Dec(FWaitCount);
end;

{ TRequsetConstruct }

class function TRequestConstruct.Request(Resource: string; Params: TParams; Method: TRESTRequestMethod): TRESTRequest;
var
  Param: TParam;
begin
  Result := TRESTRequest.Create(nil);
  Result.Client := Client;
  Result.Resource := Resource;
  Result.Method := Method;
  for Param in Params do
  begin
    if not Param[0].IsEmpty then
      Result.Params.AddItem(Param[0], Param[1]);
  end;
end;

{ TVkHandler }

function TVkHandler.AskCaptcha(Sender: TObject; const CaptchaImg: string; var Answer: string): Boolean;
var
  FRes: string;
begin
  Result := False;
  if Assigned(FOnCaptcha) then
  begin
    if (TThread.Current.ThreadID = MainThreadID) then
    begin
      FOnCaptcha(Sender, CaptchaImg, Answer);
    end
    else
    begin
      FRes := '';
      TThread.Synchronize(nil,
        procedure
        begin
          FOnCaptcha(Sender, CaptchaImg, FRes);
        end);
      Answer := FRes;
    end;
    Result := not Answer.IsEmpty;
  end;
end;

constructor TVkHandler.Create(AOwner: TObject);
begin
  inherited Create;
  FOwner := AOwner;
  FCaptchaWait := False;
  FExecuting := 0;
  FStartRequest := 0;
  FRequests := 0;
  FUsePseudoAsync := False;
  FRESTClient := TRESTClient.Create(nil);
  FRESTClient.Accept := 'application/json, text/plain; q=0.9, text/html;q=0.8,';
  FRESTClient.AcceptCharset := 'UTF-8, *;q=0.8';

  TRequestConstruct.Client := FRESTClient;
end;

destructor TVkHandler.Destroy;
begin
  FRESTClient.Free;
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
    Result := False;
    if TThread.Current.ThreadID = MainThreadID then
      FOnConfirm(Self, Answer, Result)
    else
    begin
      FRes := False;
      TThread.Synchronize(nil,
        procedure
        begin
          FOnConfirm(Self, Answer, FRes);
        end);
      Result := FRes;
    end;
  end;
end;

function TVkHandler.DoProcError(Sender: TObject; E: Exception; Code: Integer; Text: string): Boolean;
begin
  Result := Assigned(FOnError);
  if Result then
  begin
    if TThread.Current.ThreadID = MainThreadID then
      FOnError(Sender, E, Code, Text)
    else
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          FOnError(Sender, E, Code, Text);
        end);
    end;
  end;
end;

function TVkHandler.Execute(Request: string; Param: TParam): TResponse;
begin
  Result := Execute(TRequestConstruct.Request(Request, [Param]), True);
end;

function TVkHandler.Execute(Request: string; Params: TParams): TResponse;
begin
  Result := Execute(TRequestConstruct.Request(Request, Params), True);
end;

function TVkHandler.ExecutePost(Request: string; Params: TParams): TResponse;
begin
  Result := Execute(TRequestConstruct.Request(Request, Params, rmPOST), True);
end;

function TVkHandler.Execute(Request: string): TResponse;
begin
  Result := Execute(TRequestConstruct.Request(Request, []), True);
end;

function TVkHandler.Execute(Request: TRESTRequest; FreeRequset: Boolean): TResponse;
begin
  try
    Inc(FExecuting);
    Result := FExecute(Request);
  finally
    if not Waiting then
    begin
      FCancelAll := False;
      FCaptchaWait := False;
    end;
    if FreeRequset then
      Request.Free;
    Dec(FExecuting);
  end;
end;

procedure TVkHandler.WaitForQueue;
begin
  FRequests := FRequests + 1;
  //Если это первый запрос, то сохраняем метку
  if FRequests = 1 then
    FStartRequest := TThread.GetTickCount;
  //Если уже 3 запроса было, то ждём до конца секунды FStartRequest
  if FRequests > RequestLimit then
  begin
    FRequests := 0;
    WaitTime(1300 - Int64(TThread.GetTickCount - FStartRequest));
  end;
end;

function TVkHandler.FExecute(Request: TRESTRequest): TResponse;
var
  IsDone, IsError: Boolean;
  Thr: TThread;
  ErrStr: string;
begin
  IsError := False;
  Result.Success := False;
  if FLogging then
    FLog(Request.GetFullRequestURL);
  try
    Request.Response := TRESTResponse.Create(Request);
    if (TThread.Current.ThreadID = MainThreadID) and FUsePseudoAsync then
    begin
      IsDone := False;
      Thr := TThread.CreateAnonymousThread(
        procedure
        begin
          WaitForQueue;
          if not FCancelAll then
          begin
            try
              Request.Execute;
            except
              on E: Exception do
              begin
                IsError := True;
                ErrStr := E.Message;
              end;
            end;
          end;
          IsDone := True;
        end);
      Thr.FreeOnTerminate := False;
      Thr.Start;
      while (not IsDone) and (not Thr.Finished) do
        Application.ProcessMessages;
      Thr.Free;
      if IsError then
        raise TVkHandlerException.Create(ErrStr);
    end
    else
    begin
      WaitForQueue;
      if not FCancelAll then
        Request.Execute;
    end;
  except
    on E: Exception do
    begin
      IsError := True;
      DoProcError(Self, E.Create(E.Message), ERROR_VK_NETWORK, E.Message);
    end;
  end;

  try
    if not IsError then
    begin
      if FCancelAll then
      begin
        if not Waiting then
          FCancelAll := False;
        if FLogging then
          FLog(Request.GetFullRequestURL + ' - canceled');
        Result.Success := False;
      end
      else
      begin
        if not Application.Terminated then
        begin
          if FLogResponse then
            FLog(Request.Response.JSONText);
          Result := ProcessResponse(Request);
        end;
      end;
    end;
  except
    on E: TVkMethodException do
      DoProcError(Self, TVkMethodException.Create(E.Message, E.Code), E.Code, E.Message);
  end;
end;

function TVkHandler.ProcessResponse(Request: TRESTRequest): TResponse;
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
      VK_ERROR_CAPTCHA: //Капча
        begin
          if FCaptchaWait then
          begin
            Exit(Execute(Request));
          end;
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
            FCancelAll := True;
            FCaptchaWait := False;
            raise TVkMethodException.Create(Result.Error.Text, Result.Error.Code);
          end;
        end;
      VK_ERROR_CONFIRM: //Подтверждение для ВК
        begin
          Answer := JS.GetValue<string>('confirmation_text', '');
          if DoConfirm(Answer) then
          begin
            Request.Params.AddItem('confirm', '1');
            Result := Execute(Request);
            Request.Params.Delete('confirm');
          end
          else
            raise TVkMethodException.Create(Result.Error.Text, Result.Error.Code);
        end;
      VK_ERROR_REQUESTLIMIT: //Превышено кол-во запросов в сек
        begin
          FLog(Format('Превышено кол-во запросов в сек. (%d/%d, StartRequest %d)', [FRequests, RequestLimit, FStartRequest]));
          WaitTime(1000);
          Result := Execute(Request);
        end;
      VK_ERROR_INTERNAL_SERVER: //Internal Server Error
        begin
          WaitTime(1000);
          Result := Execute(Request);
        end;
    else
      raise TVkMethodException.Create(Result.Error.Text, Result.Error.Code);
    end;
  end
  else
  begin
    if Request.Response.StatusCode = 200 then
    begin
      if Request.Response.JSONValue.TryGetValue<TJSONValue>('response', JS) then
      begin
        Result.Response := JS.ToJSON;
        Result.JSON := Request.Response.JSONText;
        Result.Success := True;
      end;
    end
    else
      raise TVkParserException.Create('Не известный ответ от сервера: ' + Request.Response.StatusCode.ToString);
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

procedure TVkHandler.SetOnError(const Value: TOnVKError);
begin
  FOnError := Value;
end;

procedure TVkHandler.SetOnLog(const Value: TOnLog);
begin
  FOnLog := Value;
end;

procedure TVkHandler.SetOwner(const Value: TObject);
begin
  FOwner := Value;
end;

procedure TVkHandler.SetUsePseudoAsync(const Value: Boolean);
begin
  FUsePseudoAsync := Value;
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
  if not JSON.IsEmpty then
    Result := TJSONObject.ParseJSONValue(UTF8ToString(JSON))
  else
    Result := nil;
end;

function TResponse.GetObject<T>(var Value: T): Boolean;
begin
  Result := Success;
  if Result then
  begin
    try
      Value := T.FromJsonString<T>(Response);
    except
      Result := False;
    end;
  end;
end;

function TResponse.GetObjects<T>(var Value: T): Boolean;
begin
  Result := Success;
  if Result then
  begin
    try
      Value := T.FromJsonString<T>(ResponseAsItems);
    except
      Result := False;
    end;
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

function TResponse.AppendItemsTag(JSON: string): string;
begin
  Result := '{"Items": ' + JSON + '}';
end;

function TResponse.ResponseAsItems: string;
begin
  Result := AppendItemsTag(Response);
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

