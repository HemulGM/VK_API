unit VK.API;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Dialogs,
  IPPeerClient, REST.Client, Vcl.Controls, REST.Authenticator.OAuth, VK.Types, VK.OAuth2, VK.Account,
  VK.Handler;

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
    FServiceKey: string;
    FUseServiceKeyOnly: Boolean;
    FIsLogin: Boolean;
    FOnError: TOnVKError;
    function GetPermissions: string;
    procedure FAfterRedirect(const AURL: string; var DoCloseWebView: boolean);
    procedure FOnAuthError(const AURL: string; AStatusCode: Integer; var Cancel: WordBool);
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
    procedure FOnVKError(Code: Integer; Text: string);
    procedure SetServiceKey(const Value: string);
    procedure SetUseServiceKeyOnly(const Value: Boolean);
    procedure SetOnError(const Value: TOnVKError);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Login(AParentWindow: TWinControl);
    procedure CallMethod(MethodName: string; Params: TParams; Callback: TCallMethodCallback = nil);
    property PermissionsList: TPermissions read FPermissionsList write SetPermissionsList;
    property Account: TAccount read FAccount;
    property OnLogin: TOnLogin read FOnLogin write SetOnLogin;
    property OnError: TOnVKError read FOnError write SetOnError;
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
  end;

implementation

uses
  System.DateUtils;

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
  FHandler := TVKHandler.Create;
  FHandler.OnError := FOnVKError;
  FHandler.RESTClient.Authenticator := FOAuth2Authenticator;

  EndPoint := 'https://oauth.vk.com/authorize';
  BaseURL := 'https://api.vk.com/method';
  APIVersion := '5.101';

  FAccount := TAccount.Create(FHandler);
end;

destructor TCustomVK.Destroy;
begin
  FAccount.Free;
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
    FOnVKError(ERROR_VK_NOTOKEN, 'Токен не был получен');
  end;
end;

procedure TCustomVK.FOnAuthError(const AURL: string; AStatusCode: Integer; var Cancel: WordBool);
begin
  FOnVKError(AStatusCode, 'AuthError');
end;

procedure TCustomVK.FOnVKError(Code: Integer; Text: string);
begin
  if Assigned(FOnError) then
    FOnError(Code, Text)
  else
    raise Exception.Create('Code: ' + Code.ToString + #13#10 + Text);
end;

procedure TCustomVK.Login(AParentWindow: TWinControl);
begin
  if not Assigned(FAuthForm) then
  begin
    FAuthForm := TFormOAuth2.Create(nil);
    FAuthForm.OnAfterRedirect := FAfterRedirect;
    FAuthForm.OnError := FOnAuthError;
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

procedure TCustomVK.SetOnError(const Value: TOnVKError);
begin
  FOnError := Value;
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

procedure TCustomVK.SetServiceKey(const Value: string);
begin
  FServiceKey := Value;
end;

procedure TCustomVK.SetUseServiceKeyOnly(const Value: Boolean);
begin
  FUseServiceKeyOnly := Value;
end;

end.

