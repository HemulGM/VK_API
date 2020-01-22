unit VK.API;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, REST.Client,
  Vcl.Controls, REST.Authenticator.OAuth, IPPeerClient, VK.Types, VK.OAuth2, VK.Account, VK.Handler,
  VK.Auth, VK.Users, System.Net.HttpClient, VK.LongPollServer, System.JSON, VK.Messages,
  System.Generics.Collections, VK.Status, VK.Wall, VK.Uploader, VK.Docs, VK.Audio, VK.Likes,
  VK.Board;

type
  TCustomVK = class(TComponent)
  private
    FOAuth2Authenticator: TOAuth2Authenticator;
    FOnLogin: TOnLogin;
    FPermissionsList: TPermissions;
    FGroupLongPollServers: TGroupLongPollServers;
    FAuthForm: TFormOAuth2;
    FAppID: string;
    FAppKey: string;
    FEndPoint: string;
    FHandler: TVkHandler;
    FBaseURL: string;
    FAPIVersion: string;
    FAccount: TAccountController;
    FAuth: TAuthController;
    FServiceKey: string;
    FUseServiceKeyOnly: Boolean;
    FIsLogin: Boolean;
    FOnError: TOnVKError;
    FOnLog: TOnLog;
    FOnErrorLogin: TOnVKError;
    FChangePasswordHash: string;
    FUsers: TUsersController;
    FOnCaptcha: TOnCaptcha;
    FOnConfirm: TOnConfirm;
    FOnAuth: TOnAuth;
    FMessages: TMessagesController;
    FStatus: TStatusController;
    FWall: TWallController;
    FUploader: TUploader;
    FDoc: TDocController;
    FLikes: TLikesController;
    FAudio: TAudioController;
    FBoard: TBoardController;
    function GetPermissions: string;
    procedure FAskCaptcha(Sender: TObject; const CaptchaImg: string; var Answer: string);
    procedure FAfterRedirect(const AURL: string; var DoCloseWebView: boolean);
    procedure FAuthError(const AURL: string; AStatusCode: Integer; var Cancel: WordBool);
    procedure FLog(Sender: TObject; const Value: string);
    procedure FVKError(Sender: TObject; E: Exception; Code: Integer; Text: string);
    procedure SetOnLogin(const Value: TOnLogin);
    procedure SetPermissionsList(const Value: TPermissions);
    procedure DoLogin;
    procedure SetAppID(const Value: string);
    procedure SetAppKey(const Value: string);
    procedure SetEndPoint(const Value: string);
    procedure SetPermissions(const Value: string);
    procedure SetHandler(const Value: TVkHandler);
    procedure SetBaseURL(const Value: string);
    procedure SetAPIVersion(const Value: string);
    procedure SetServiceKey(const Value: string);
    procedure SetUseServiceKeyOnly(const Value: Boolean);
    procedure SetOnError(const Value: TOnVKError);
    procedure SetOnLog(const Value: TOnLog);
    procedure SetOnErrorLogin(const Value: TOnVKError);
    procedure SetOnCaptcha(const Value: TOnCaptcha);
    procedure SetOnConfirm(const Value: TOnConfirm);
    procedure SetOnAuth(const Value: TOnAuth);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoLog(Sender: TObject; Text: string);
    procedure DoError(Sender: TObject; E: Exception; Code: Integer; Text: string);
    procedure Login(AParentWindow: TWinControl = nil);
    procedure CallMethod(MethodName: string; Params: TParams; Callback: TCallMethodCallback = nil); overload;
    procedure CallMethod(MethodName: string; Param: string; Value: string; Callback:
      TCallMethodCallback = nil); overload;
    procedure CallMethod(MethodName: string; Callback: TCallMethodCallback = nil); overload;
    /// <summary>
    /// Универсальный метод, который позволяет запускать последовательность других методов, сохраняя и фильтруя промежуточные результаты.
    /// https://vk.com/dev/execute
    /// </summary>
    /// <param name="Code">код алгоритма в VKScript - формате, похожем на JavaSсript или ActionScript (предполагается совместимость с ECMAScript). Алгоритм должен завершаться командой return %выражение%. Операторы должны быть разделены точкой с запятой. </param>
    function Execute(Code: string): TResponse;
    property PermissionsList: TPermissions read FPermissionsList write SetPermissionsList;
    property Uploader: TUploader read FUploader;
    //Группы методов
    property Account: TAccountController read FAccount;
    property Auth: TAuthController read FAuth;
    property Users: TUsersController read FUsers;
    property Messages: TMessagesController read FMessages;
    property Status: TStatusController read FStatus;
    property Wall: TWallController read FWall;
    property Docs: TDocController read FDoc;
    property Likes: TLikesController read FLikes;
    property Audio: TAudioController read FAudio;
    property Board: TBoardController read FBoard;
    //
    property AppID: string read FAppID write SetAppID;
    property AppKey: string read FAppKey write SetAppKey;
    property EndPoint: string read FEndPoint write SetEndPoint;
    property Permissions: string read GetPermissions write SetPermissions;
    property Handler: TVkHandler read FHandler write SetHandler;
    property APIVersion: string read FAPIVersion write SetAPIVersion;
    property BaseURL: string read FBaseURL write SetBaseURL;
    property ServiceKey: string read FServiceKey write SetServiceKey;
    property UseServiceKeyOnly: Boolean read FUseServiceKeyOnly write SetUseServiceKeyOnly;
    property IsLogin: Boolean read FIsLogin;
    property ChangePasswordHash: string read FChangePasswordHash;
    //
    property OnLogin: TOnLogin read FOnLogin write SetOnLogin;
    property OnError: TOnVKError read FOnError write SetOnError;
    property OnErrorLogin: TOnVKError read FOnErrorLogin write SetOnErrorLogin;
    property OnLog: TOnLog read FOnLog write SetOnLog;
    property OnCaptcha: TOnCaptcha read FOnCaptcha write SetOnCaptcha;
    property OnConfirm: TOnConfirm read FOnConfirm write SetOnConfirm;
    property OnAuth: TOnAuth read FOnAuth write SetOnAuth;
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

procedure TCustomVK.CallMethod(MethodName: string; Param: string; Value: string; Callback: TCallMethodCallback);
var
  Response: TResponse;
begin
  Response := Handler.Execute(MethodName, [Param, Value]);
  if Assigned(Callback) then
  begin
    Callback(Response);
  end;
end;

procedure TCustomVK.CallMethod(MethodName: string; Callback: TCallMethodCallback);
var
  Response: TResponse;
begin
  Response := Handler.Execute(MethodName);
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
  FOAuth2Authenticator := TOAuth2Authenticator.Create(Self);
  FHandler := TVkHandler.Create(Self);
  FHandler.OnError := FVKError;
  FHandler.OnLog := FLog;
  FHandler.RESTClient.Authenticator := FOAuth2Authenticator;
  FHandler.OnCaptcha := FAskCaptcha;
  FHandler.OnConfirm := FOnConfirm;
  //Defaults
  EndPoint := 'https://oauth.vk.com/authorize';
  BaseURL := 'https://api.vk.com/method';
  APIVersion := '5.103';
  PermissionsList := ['groups', 'friends', 'wall', 'photos', 'video', 'docs', 'notes', 'market'];
  //
  FAccount := TAccountController.Create(FHandler);
  FAuth := TAuthController.Create(FHandler);
  FUsers := TUsersController.Create(FHandler);
  FMessages := TMessagesController.Create(FHandler);
  FStatus := TStatusController.Create(FHandler);
  FWall := TWallController.Create(FHandler);
  FDoc := TDocController.Create(FHandler);
  FLikes := TLikesController.Create(FHandler);
  FAudio := TAudioController.Create(FHandler);
  FBoard := TBoardController.Create(FHandler);
  //Groups LongPolls
  FUploader := TUploader.Create;
  FGroupLongPollServers := TGroupLongPollServers.Create;
end;

destructor TCustomVK.Destroy;
begin
  FGroupLongPollServers.Clear;
  FGroupLongPollServers.Free;

  FBoard.Free;
  FLikes.Free;
  FAudio.Free;
  FDoc.Free;
  FUploader.Free;
  FWall.Free;
  FStatus.Free;
  FUsers.Free;
  FAccount.Free;
  FAuth.Free;
  FMessages.Free;
  FHandler.Free;
  inherited;
end;

procedure TCustomVK.DoError(Sender: TObject; E: Exception; Code: Integer; Text: string);
begin
  FVKError(Sender, E, Code, Text);
end;

procedure TCustomVK.DoLog(Sender: TObject; Text: string);
begin
  FLog(Self, Text);
end;

procedure TCustomVK.DoLogin;
begin
  FIsLogin := True;
  if Assigned(FOnLogin) then
    FOnLogin(Self);
end;

procedure TCustomVK.FAskCaptcha(Sender: TObject; const CaptchaImg: string; var Answer: string);
begin
  if Assigned(FOnCaptcha) then
  begin
    FOnCaptcha(Self, CaptchaImg, Answer);
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
      FLog(Self, FOAuth2Authenticator.AccessToken);
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
    FVKError(Self, TVkException.Create('Токен не был получен'), ERROR_VK_NOTOKEN, 'Токен не был получен');
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
    FOnErrorLogin(Self, TVkException.Create('Ошибка авторизации, код : ' + AStatusCode.ToString), AStatusCode, AURL);
end;

procedure TCustomVK.FVKError(Sender: TObject; E: Exception; Code: Integer; Text: string);
begin
  if Assigned(FOnError) then
  begin
    FOnError(Self, E, Code, Text);
    if Assigned(E) then
      E.Free;
  end
  else
    raise E;
end;

procedure TCustomVK.Login(AParentWindow: TWinControl);
var
  Token, ChangePasswordHash: string;
  TokenExpiry: Int64;
begin
  FOAuth2Authenticator.AccessToken := EmptyStr;
  FOAuth2Authenticator.ClientID := FAppID;
  FOAuth2Authenticator.ClientSecret := FAppKey;
  FOAuth2Authenticator.ResponseType := TOAuth2ResponseType.rtTOKEN;
  FOAuth2Authenticator.AuthorizationEndpoint := FEndPoint;

  Token := '';
  TokenExpiry := 0;
  if Assigned(FOnAuth) then
    FOnAuth(Self, Token, TokenExpiry, ChangePasswordHash);

  if not Token.IsEmpty then
  begin
    FChangePasswordHash := ChangePasswordHash;
    FOAuth2Authenticator.AccessToken := Token;
    FOAuth2Authenticator.AccessTokenExpiry := IncSecond(Now, TokenExpiry);
    DoLogin;
  end
  else
  begin
    if not Assigned(FAuthForm) then
    begin
      FAuthForm := TFormOAuth2.Create(nil);
      FAuthForm.OnAfterRedirect := FAfterRedirect;
      FAuthForm.OnError := FAuthError;
    end;
    FAuthForm.ShowWithURL(AParentWindow, FOAuth2Authenticator.AuthorizationRequestURI);
  end;
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

procedure TCustomVK.SetHandler(const Value: TVkHandler);
begin
  FHandler := Value;
end;

procedure TCustomVK.SetBaseURL(const Value: string);
begin
  FBaseURL := Value;
  FHandler.RESTClient.BaseURL := FBaseURL;
end;

procedure TCustomVK.SetOnAuth(const Value: TOnAuth);
begin
  FOnAuth := Value;
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

