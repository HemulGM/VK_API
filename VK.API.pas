unit VK.API;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Dialogs,
  IPPeerClient, REST.Client, Vcl.Controls, REST.Authenticator.OAuth, VK.Types, VK.OAuth2, VK.Account,
  VK.Handler, VK.Auth, VK.Users;

type
  TCustomVK = class(TComponent)
  private
    FOAuth2Authenticator: TOAuth2Authenticator;
    FOnLogin: TOnLogin;
    FPermissionsList: TPermissions;
    FAuthForm: TFormOAuth2;
    FAppID: string;
    FAppKey: string;
    FEndPoint: string;
    FHandler: TVKHandler;
    FBaseURL: string;
    FAPIVersion: string;
    FAccount: TAccount;
    FAuth: TAuth;
    FServiceKey: string;
    FUseServiceKeyOnly: Boolean;
    FIsLogin: Boolean;
    FOnError: TOnVKError;
    FOnLog: TOnLog;
    FOnErrorLogin: TOnVKError;
    FChangePasswordHash: string;
    FUsers: TUsers;
    FOnCaptcha: TOnCaptcha;
    FOnConfirm: TOnConfirm;
    function GetPermissions: string;
    procedure FAskCaptcha(const CaptchaImg: string; var Answer: string);
    procedure FAfterRedirect(const AURL: string; var DoCloseWebView: boolean);
    procedure FAuthError(const AURL: string; AStatusCode: Integer; var Cancel: WordBool);
    procedure FLog(Sender: TObject; const Value: string);
    procedure FVKError(Sender: TObject; Code: Integer; Text: string);
    procedure SetOnLogin(const Value: TOnLogin);
    procedure SetPermissionsList(const Value: TPermissions);
    procedure DoLogin;
    procedure SetAppID(const Value: string);
    procedure SetAppKey(const Value: string);
    procedure SetEndPoint(const Value: string);
    procedure SetPermissions(const Value: string);
    procedure SetHandler(const Value: TVKHandler);
    procedure SetBaseURL(const Value: string);
    procedure SetAPIVersion(const Value: string);
    procedure SetServiceKey(const Value: string);
    procedure SetUseServiceKeyOnly(const Value: Boolean);
    procedure SetOnError(const Value: TOnVKError);
    procedure SetOnLog(const Value: TOnLog);
    procedure SetOnErrorLogin(const Value: TOnVKError);
    procedure SetOnCaptcha(const Value: TOnCaptcha);
    procedure SetOnConfirm(const Value: TOnConfirm);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Login(AParentWindow: TWinControl = nil);
    procedure CallMethod(MethodName: string; Params: TParams; Callback: TCallMethodCallback = nil);
    /// <summary>
    /// Универсальный метод, который позволяет запускать последовательность других методов, сохраняя и фильтруя промежуточные результаты.
    /// https://vk.com/dev/execute
    /// </summary>
    /// <param name="Code">код алгоритма в VKScript - формате, похожем на JavaSсript или ActionScript (предполагается совместимость с ECMAScript). Алгоритм должен завершаться командой return %выражение%. Операторы должны быть разделены точкой с запятой. </param>
    function Execute(Code: string): TResponse;
    property PermissionsList: TPermissions read FPermissionsList write SetPermissionsList;
    property Account: TAccount read FAccount;
    property Auth: TAuth read FAuth;
    property Users: TUsers read FUsers;
    property OnLogin: TOnLogin read FOnLogin write SetOnLogin;
    property OnError: TOnVKError read FOnError write SetOnError;
    property OnErrorLogin: TOnVKError read FOnErrorLogin write SetOnErrorLogin;
    property OnLog: TOnLog read FOnLog write SetOnLog;
    property OnCaptcha: TOnCaptcha read FOnCaptcha write SetOnCaptcha;
    property OnConfirm: TOnConfirm read FOnConfirm write SetOnConfirm;
    property AppID: string read FAppID write SetAppID;
    property AppKey: string read FAppKey write SetAppKey;
    property EndPoint: string read FEndPoint write SetEndPoint;
    property Permissions: string read GetPermissions write SetPermissions;
    property Handler: TVKHandler read FHandler write SetHandler;
    property APIVersion: string read FAPIVersion write SetAPIVersion;
    property BaseURL: string read FBaseURL write SetBaseURL;
    property ServiceKey: string read FServiceKey write SetServiceKey;
    property UseServiceKeyOnly: Boolean read FUseServiceKeyOnly write SetUseServiceKeyOnly;
    property IsLogin: Boolean read FIsLogin;
    property ChangePasswordHash: string read FChangePasswordHash;
  end;

implementation

uses
  System.DateUtils, VK.Captcha;

{ TVK }

procedure TCustomVK.CallMethod(MethodName: string; Params: TParams; Callback: TCallMethodCallback);
var
  Response: TResponse;
begin
  Response := Handler.Execute(MethodName, Params);
  if Assigned(Callback) then
  begin
    Callback(Response);
  end;
end;

constructor TCustomVK.Create(AOwner: TComponent);
begin
  inherited;
  FIsLogin := False;
  FUseServiceKeyOnly := False;
  FPermissionsList := TPermissions.Create;
  FOAuth2Authenticator := TOAuth2Authenticator.Create(Self);
  FHandler := TVKHandler.Create(Self);
  FHandler.OnError := FVKError;
  FHandler.OnLog := FLog;
  FHandler.RESTClient.Authenticator := FOAuth2Authenticator;
  FHandler.OnCaptcha := FAskCaptcha;
  FHandler.OnConfirm := FOnConfirm;

  EndPoint := 'https://oauth.vk.com/authorize';
  BaseURL := 'https://api.vk.com/method';
  APIVersion := '5.101';

  FAccount := TAccount.Create(FHandler);
  FAuth := TAuth.Create(FHandler);
  FUsers := TUsers.Create(FHandler);
end;

destructor TCustomVK.Destroy;
begin
  FUsers.Free;
  FAccount.Free;
  FAuth.Free;
  FPermissionsList.Free;
  FHandler.Free;
  inherited;
end;

procedure TCustomVK.DoLogin;
begin
  FIsLogin := True;
  if Assigned(FOnLogin) then
    FOnLogin(Self);
end;

procedure TCustomVK.FAskCaptcha(const CaptchaImg: string; var Answer: string);
begin
  if Assigned(FOnCaptcha) then
  begin
    FOnCaptcha(CaptchaImg, Answer);
  end
  else
    TFormCaptcha.Execute(CaptchaImg, Answer);
end;

function TCustomVK.Execute(Code: string): TResponse;
var
  Resp: TResponse;
begin
  CallMethod('execute', [['code', Code]],
    procedure(Response: TResponse)
    begin
      Resp := Response;
    end);
  Result := Resp;
end;

procedure TCustomVK.FAfterRedirect(const AURL: string; var DoCloseWebView: boolean);
var
  i: integer;
  Str: string;
  Params: TStringList;
begin
  i := Pos('#access_token=', AURL);
  if (i <> 0) and (FOAuth2Authenticator.AccessToken.IsEmpty) then
  begin
    Str := AURL;
    Delete(Str, 1, i);
    Params := TStringList.Create;
    try
      Params.Delimiter := '&';
      Params.DelimitedText := Str;
      FChangePasswordHash := Params.Values['change_password_hash'];
      FOAuth2Authenticator.AccessToken := Params.Values['access_token'];
      FOAuth2Authenticator.AccessTokenExpiry := IncSecond(Now, StrToInt(Params.Values['expires_in']));
      DoLogin;
    finally
      Params.Free;
    end;
    DoCloseWebView := True;
  end;
  FAuthForm.Free;
  FAuthForm := nil;
  if FOAuth2Authenticator.AccessToken.IsEmpty then
  begin
    FVKError(Self, ERROR_VK_NOTOKEN, 'Токен не был получен');
  end;
end;

procedure TCustomVK.FLog(Sender: TObject; const Value: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, Value);
end;

procedure TCustomVK.FAuthError(const AURL: string; AStatusCode: Integer; var Cancel: WordBool);
begin
  if Assigned(FOnErrorLogin) then
    FOnErrorLogin(Self, AStatusCode, AURL);
end;

procedure TCustomVK.FVKError(Sender: TObject; Code: Integer; Text: string);
begin
  if Assigned(FOnError) then
    FOnError(Self, Code, Text)
  else
    raise Exception.Create('Code: ' + Code.ToString + #13#10 + Text);
end;

procedure TCustomVK.Login(AParentWindow: TWinControl);
begin
  if not Assigned(FAuthForm) then
  begin
    FAuthForm := TFormOAuth2.Create(nil);
    FAuthForm.OnAfterRedirect := FAfterRedirect;
    FAuthForm.OnError := FAuthError;
  end;

  FOAuth2Authenticator.AccessToken := EmptyStr;
  FOAuth2Authenticator.ClientID := FAppID;
  FOAuth2Authenticator.ClientSecret := FAppKey;
  FOAuth2Authenticator.ResponseType := TOAuth2ResponseType.rtTOKEN;
  FOAuth2Authenticator.AuthorizationEndpoint := FEndPoint;

  FAuthForm.ShowWithURL(AParentWindow, FOAuth2Authenticator.AuthorizationRequestURI);
end;

procedure TCustomVK.SetAPIVersion(const Value: string);
begin
  FAPIVersion := Value;
  FHandler.RESTClient.AddParameter('v', FAPIVersion);
end;

procedure TCustomVK.SetServiceKey(const Value: string);
begin
  FServiceKey := Value;
  if csDesigning in ComponentState then
    Exit;

  if FUseServiceKeyOnly then
  begin
    FHandler.RESTClient.AddParameter('access_token', FServiceKey);
  end;
end;

procedure TCustomVK.SetAppID(const Value: string);
begin
  FAppID := Value;
end;

procedure TCustomVK.SetAppKey(const Value: string);
begin
  FAppKey := Value;
end;

procedure TCustomVK.SetEndPoint(const Value: string);
begin
  FEndPoint := Value;
end;

procedure TCustomVK.SetHandler(const Value: TVKHandler);
begin
  FHandler := Value;
end;

procedure TCustomVK.SetBaseURL(const Value: string);
begin
  FBaseURL := Value;
  FHandler.RESTClient.BaseURL := FBaseURL;
end;

procedure TCustomVK.SetOnCaptcha(const Value: TOnCaptcha);
begin
  FOnCaptcha := Value;
end;

procedure TCustomVK.SetOnConfirm(const Value: TOnConfirm);
begin
  FOnConfirm := Value;
end;

procedure TCustomVK.SetOnError(const Value: TOnVKError);
begin
  FOnError := Value;
end;

procedure TCustomVK.SetOnErrorLogin(const Value: TOnVKError);
begin
  FOnErrorLogin := Value;
end;

procedure TCustomVK.SetOnLog(const Value: TOnLog);
begin
  FOnLog := Value;
end;

procedure TCustomVK.SetOnLogin(const Value: TOnLogin);
begin
  FOnLogin := Value;
end;

procedure TCustomVK.SetPermissions(const Value: string);
var
  Params: TStringList;
begin
  Params := TStringList.Create;
  try
    Params.Delimiter := ',';
    Params.DelimitedText := Value;
    FPermissionsList.Assign(Params);
  finally
    Params.Free;
  end;
end;

function TCustomVK.GetPermissions: string;
begin
  Result := FPermissionsList.ToString;
end;

procedure TCustomVK.SetPermissionsList(const Value: TPermissions);
begin
  FPermissionsList := Value;
end;

procedure TCustomVK.SetUseServiceKeyOnly(const Value: Boolean);
begin
  FUseServiceKeyOnly := Value;
  if csDesigning in ComponentState then
    Exit;

  FHandler.UseServiceKeyOnly := Value;
  if FUseServiceKeyOnly then
  begin
    FHandler.RESTClient.Authenticator := nil;
    FHandler.RESTClient.AddParameter('access_token', FServiceKey);
  end
  else
  begin
    FHandler.RESTClient.Authenticator := FOAuth2Authenticator;
    FHandler.RESTClient.Params.Delete('access_token');
  end;
end;

end.

