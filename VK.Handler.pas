unit VK.Handler;

interface

uses
  Winapi.Windows, System.SysUtils, Vcl.Forms, REST.Authenticator.OAuth,
  REST.Client, REST.Json, JSON, VK.Captcha, VK.Types;

type
  TRequestConstruct = class
    class var
      Client: TRESTClient;
  public
    class function Request(Resource: string; Params: TParams): TRESTRequest;
  end;

  TVKHandler = class
    const
      RequestLimit = 3; //Round(1000 / 3) + 10; //задержка между запросами 3 запроса в секунду + 10 мс страховка
  private
    FStartRequest: Cardinal;  //Время последнего запроса в ВК
    FRequests: Integer;
    FRESTClient: TRESTClient;
    FOnConfirm: TOnConfirm;
    FOnError: TOnVKError;
    procedure ProcError(Code: Integer; Text: string = ''); overload;
    procedure ProcError(E: Exception); overload;
    procedure ProcError(Msg: string); overload;
    function DoConfirm(Ans: string): Boolean;
    procedure SetOnConfirm(const Value: TOnConfirm);
    procedure SetOnError(const Value: TOnVKError);
  public
    constructor Create;
    destructor Destroy; override;
    function AskCapcha(const CapchaImg: string; var Answer: string): Boolean;
    function Execute(Request: string; Params: TParams): TResponse; overload;
    function Execute(Request: string; Param: TParam): TResponse; overload;
    function Execute(Request: string): TResponse; overload;
    function Execute(Request: TRESTRequest; FreeRequset: Boolean = False): TResponse; overload;
    property RESTClient: TRESTClient read FRESTClient;
    property OnConfirm: TOnConfirm read FOnConfirm write SetOnConfirm;
    property OnError: TOnVKError read FOnError write SetOnError;
  end;

implementation

uses
  HGM.Common.Utils;

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
    if (Param[0] <> '') and (Param[1] <> '') then
      Result.Params.AddItem(Param[0], Param[1]);
  end;
end;

{ TVKHandler }

function TVKHandler.AskCapcha(const CapchaImg: string; var Answer: string): Boolean;
begin
  Result := TFormCaptcha.Execute(CapchaImg, Answer);
end;

constructor TVKHandler.Create;
begin
  inherited;
  FStartRequest := 0;
  FRequests := 0;
  FRESTClient := TRESTClient.Create(nil);
  FRESTClient.Accept := 'application/json, text/plain; q=0.9, text/html;q=0.8,';
  FRESTClient.AcceptCharset := 'UTF-8, *;q=0.8';

  TRequestConstruct.Client := FRESTClient;
end;

destructor TVKHandler.Destroy;
begin
  FRESTClient.Free;
  inherited;
end;

function TVKHandler.DoConfirm(Ans: string): Boolean;
begin
  if not Assigned(FOnConfirm) then
  begin
    Exit(True);
  end
  else
  begin
    Result := False;
    FOnConfirm(Ans, Result);
  end;
end;

function TVKHandler.Execute(Request: string; Param: TParam): TResponse;
begin
  Result := Execute(TRequestConstruct.Request(Request, [Param]), True);
end;

function TVKHandler.Execute(Request: string; Params: TParams): TResponse;
begin
  Result := Execute(TRequestConstruct.Request(Request, Params), True);
end;

function TVKHandler.Execute(Request: string): TResponse;
begin
  Result := Execute(TRequestConstruct.Request(Request, []), True);
end;

function TVKHandler.Execute(Request: TRESTRequest; FreeRequset: Boolean): TResponse;
var
  JS: TJSONValue;
  CaptchaSID: string;
  CaptchaImg: string;
  CaptchaAns: string;
  IsDone: Boolean;
  ResponseCode: Integer;
  TimeStamp: Cardinal;
  TimeStampLast: Cardinal;
begin
  TimeStamp := GetTickCount;
  TimeStampLast := FStartRequest;
  Result.Success := False;
  try
    IsDone := False;
    //Log('Запрос: ' + Request.GetFullRequestURL);

    FRequests := FRequests + 1;
    //Если уже 3 запроса было, то ждём до конца секунды FStartRequest
    if FRequests > RequestLimit then
    begin
      FRequests := 0;
      WaitTime(1300 - Int64(GetTickCount - FStartRequest));
    end;
    Request.Response := TRESTResponse.Create(Request);
    Request.ExecuteAsync(
      procedure
      begin
        IsDone := True;
      end);
    while not IsDone do
      Application.ProcessMessages;

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
            if AskCapcha(CaptchaImg, CaptchaAns) then
            begin
              Request.Params.AddItem('captcha_sid', CaptchaSID);
              Request.Params.AddItem('captcha_key', CaptchaAns);
              Result := Execute(Request);
              Request.Params.Delete('captcha_sid');
              Request.Params.Delete('captcha_key');
              if FreeRequset then
                Request.Free;
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
              if FreeRequset then
                Request.Free;
              Exit;
            end
            else
              ProcError(Result.Error.Code, Result.Error.Text);
          end;
        6: //Превышено кол-во запросов в сек.
          begin
            ProcError(Format('Превышено кол-во запросов в сек. (%d/%d, Enter %d, StartRequest %d, LastRequest %d)', [FRequests, RequestLimit, TimeStamp, FStartRequest, TimeStampLast]));
            WaitTime(1000);
            Result := Execute(Request);
            if FreeRequset then
              Request.Free;
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
          Result.JSON := JS.ToJSON;
          Result.Success := True;
        end
        else if Request.Response.JSONValue.TryGetValue<Integer>('response', ResponseCode) then
        begin
          Result.Success := ResponseCode = 1;
        end;
      end;
    end;
  except
    on E: Exception do
      ProcError(E);
  end;
  if FreeRequset then
    Request.Free;
end;

procedure TVKHandler.SetOnError(const Value: TOnVKError);
begin
  FOnError := Value;
end;

procedure TVKHandler.SetOnConfirm(const Value: TOnConfirm);
begin
  FOnConfirm := Value;
end;

procedure TVKHandler.ProcError(Msg: string);
begin
  if Assigned(FOnError) then
    FOnError(-1, Msg);
end;

procedure TVKHandler.ProcError(Code: Integer; Text: string);
begin
  if Assigned(FOnError) then
  begin
    if Text = '' then
      Text := VKErrorString(Code);
    FOnError(Code, Text);
  end;
end;

procedure TVKHandler.ProcError(E: Exception);
begin
  if Assigned(FOnError) then
    FOnError(-2, E.Message);
end;

end.

