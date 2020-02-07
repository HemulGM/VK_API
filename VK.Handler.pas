unit VK.Handler;

interface

uses
  Winapi.Windows, System.Classes, System.SysUtils, Vcl.Forms, REST.Authenticator.OAuth, REST.Client,
  REST.Json, JSON, VK.Types;

type
  TRequestConstruct = class
    class var
      Client: TRESTClient;
  public
    class function Request(Resource: string; Params: TParams): TRESTRequest;
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
    function DoConfirm(Answer: string): Boolean;
    procedure ProcError(Code: Integer; Text: string = ''); overload;
    procedure ProcError(E: Exception); overload;
    procedure ProcError(Msg: string); overload;
    procedure SetOnConfirm(const Value: TOnConfirm);
    procedure SetOnError(const Value: TOnVKError);
    procedure FLog(const Value: string);
    procedure SetOnLog(const Value: TOnLog);
    procedure SetUseServiceKeyOnly(const Value: Boolean);
    procedure SetOwner(const Value: TObject);
    procedure SetOnCaptcha(const Value: TOnCaptcha);
    function FExecute(Request: TRESTRequest): TResponse;
    function GetExecuting: Boolean;
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    function AskCaptcha(Sender: TObject; const CaptchaImg: string; var Answer: string): Boolean;
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
  end;

implementation

procedure WaitTime(MS: Int64);
var
  TS: Cardinal;
begin
  if MS < 0 then
    Exit;
  if MS = 0 then
    Exit;
  TS := GetTickCount;
  while TS + MS > GetTickCount do
    Sleep(100);
end;

{ TRequsetConstruct }

class function TRequestConstruct.Request(Resource: string; Params: TParams): TRESTRequest;
var
  Param: TParam;
begin
  Result := TRESTRequest.Create(nil);
  Result.Client := Client;
  Result.Resource := Resource;
  for Param in Params do
  begin
    if not Param[0].IsEmpty then
      Result.Params.AddItem(Param[0], Param[1]);
  end;
end;

{ TVkHandler }

function TVkHandler.AskCaptcha(Sender: TObject; const CaptchaImg: string; var Answer: string): Boolean;
begin
  if Assigned(FOnCaptcha) then
  begin
    FOnCaptcha(Sender, CaptchaImg, Answer);
    Result := not Answer.IsEmpty;
  end
  else
    raise Exception.Create('Необходимо определить обработчика для запроса капчи');
end;

constructor TVkHandler.Create(AOwner: TObject);
begin
  inherited Create;
  FOwner := AOwner;
  FExecuting := 0;
  FStartRequest := 0;
  FRequests := 0;
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
begin
  if not Assigned(FOnConfirm) then
  begin
    Exit(True);
  end
  else
  begin
    Result := False;
    FOnConfirm(Self, Answer, Result);
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
    if FreeRequset then
      Request.Free;
    Dec(FExecuting);
  end;
end;

function TVkHandler.FExecute(Request: TRESTRequest): TResponse;
var
  JS: TJSONValue;
  CaptchaSID: string;
  CaptchaImg: string;
  CaptchaAns: string;
  IsDone: Boolean;
  TimeStamp: Cardinal;
  TimeStampLast: Cardinal;
  Thr: TThread;
begin
  Result.Success := False;
  FLog(Request.GetFullRequestURL);
  TimeStamp := GetTickCount;
  TimeStampLast := FStartRequest;
  try
    IsDone := False;
    Request.Response := TRESTResponse.Create(Request);
    Thr := TThread.CreateAnonymousThread(
      procedure
      begin
        try
          FRequests := FRequests + 1;
          //Если уже 3 запроса было, то ждём до конца секунды FStartRequest
          if FRequests > RequestLimit then
          begin
            FRequests := 0;
            WaitTime(1300 - Int64(GetTickCount - FStartRequest));
          end;
          Request.Execute;
        finally
        end;
        IsDone := True;
      end);
    Thr.FreeOnTerminate := False;
    Thr.Start;
    while (not IsDone) and (not Thr.Finished) {and (not Application.Terminated)} do
      Application.ProcessMessages;
    Thr.Free;

    if not Application.Terminated then
    begin
      FLog(Request.Response.JSONText);

        //Если это первый запрос, то сохраняем метку
      if FRequests = 1 then
        FStartRequest := GetTickCount;

      if Request.Response.JSONValue.TryGetValue<TJSONValue>('error', JS) then
      begin
        Result.Error.Code := JS.GetValue<Integer>('error_code', -1);
        Result.Error.Text := JS.GetValue<string>('error_msg', VKErrorString(Result.Error.Code));
        case Result.Error.Code of
          14: //Капча
            begin
              CaptchaSID := JS.GetValue<string>('captcha_sid', '');
              CaptchaImg := JS.GetValue<string>('captcha_img', '');
              if AskCaptcha(Self, CaptchaImg, CaptchaAns) then
              begin
                Request.Params.AddItem('captcha_sid', CaptchaSID);
                Request.Params.AddItem('captcha_key', CaptchaAns);
                Result := Execute(Request);
                Request.Params.Delete('captcha_sid');
                Request.Params.Delete('captcha_key');
                Exit;
              end
              else
                ProcError(Result.Error.Code, Result.Error.Text);
            end;
          24: //Подтверждение для ВК
            begin
              CaptchaAns := JS.GetValue<string>('confirmation_text', '');
              if DoConfirm(CaptchaAns) then
              begin
                Request.Params.AddItem('confirm', '1');
                Result := Execute(Request);
                Request.Params.Delete('confirm');
                Exit;
              end
              else
                ProcError(Result.Error.Code, Result.Error.Text);
            end;
          6: //Превышено кол-во запросов в сек.
            begin
              ProcError(Format('Превышено кол-во запросов в сек. (%d/%d, Enter %d, StartRequest %d, LastRequest %d)',
                [FRequests, RequestLimit, TimeStamp, FStartRequest, TimeStampLast]));
              WaitTime(1000);
              Result := Execute(Request);
              Exit;
            end;
        else
          ProcError(Result.Error.Code, Result.Error.Text);
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
        end;
      end;
    end;
  except
    on E: Exception do
      ProcError(E);
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

procedure TVkHandler.SetUseServiceKeyOnly(const Value: Boolean);
begin
  FUseServiceKeyOnly := Value;
end;

procedure TVkHandler.SetOnCaptcha(const Value: TOnCaptcha);
begin
  FOnCaptcha := Value;
end;

procedure TVkHandler.SetOnConfirm(const Value: TOnConfirm);
begin
  FOnConfirm := Value;
end;

procedure TVkHandler.ProcError(Msg: string);
begin
  if Assigned(FOnError) then
    FOnError(Self, TVkHandlerException.Create(Msg), ERROR_VK_UNKNOWN, Msg);
end;

procedure TVkHandler.ProcError(Code: Integer; Text: string);
begin
  if Assigned(FOnError) then
  begin
    if Text = '' then
      Text := VKErrorString(Code);
    FOnError(Self, TVkHandlerException.Create(Text), Code, Text);
  end;
end;

procedure TVkHandler.ProcError(E: Exception);
begin
  if Assigned(FOnError) then
    FOnError(Self, TVkHandlerException.Create(E.Message), ERROR_VK_UNKNOWN, E.Message);
end;

end.

