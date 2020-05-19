unit VK.API;

interface

uses
  System.SysUtils, System.Variants, System.Classes, REST.Client, REST.Authenticator.OAuth, VK.Types, VK.Account,
  VK.Handler, VK.Auth, VK.Users, VK.LongPollServer, System.JSON, VK.Messages, System.Generics.Collections, VK.Status,
  VK.Wall, VK.Uploader, VK.Docs, VK.Audio, VK.Likes, VK.Board, REST.Types, VK.Friends, VK.Groups, VK.Photos, VK.Catalog,
  VK.Utils, System.Types,
  {$IFDEF NEEDFMX}
  VK.FMX.Captcha,
  {$ELSE}
  VK.Vcl.Captcha,
  {$ENDIF}
  VK.Video;

type
  TCustomVK = class(TComponent)
  private
    FOAuth2Authenticator: TOAuth2Authenticator;
    FOnLogin: TOnLogin;
    FPermissionsList: TPermissions;
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
    FFriends: TFriendsController;
    FGroups: TGroupsController;
    FPhotos: TPhotosController;
    FCatalog: TCatalogController;
    FUtils: TUtilsController;
    FVideo: TVideoController;
    FLogging: Boolean;
    function GetPermissions: string;
    procedure FAskCaptcha(Sender: TObject; const CaptchaImg: string; var Answer: string);
    procedure FAuthError(const AText: string; AStatusCode: Integer);
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
    function GetToken: string;
    procedure FVKErrorLogin(Sender: TObject; E: Exception; Code: Integer; Text: string);
    procedure SetToken(const Value: string);
    function CheckAuth: Boolean;
    function GetTokenExpiry: Int64;
    procedure SetTokenExpiry(const Value: Int64);
    procedure SetLogging(const Value: Boolean);
    function GetIsWorking: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoLog(Sender: TObject; Text: string);
    procedure DoError(Sender: TObject; E: Exception; Code: Integer; Text: string);
    procedure DoErrorLogin(Sender: TObject; E: Exception; Code: Integer; Text: string);
    function Login: Boolean; overload;
    function Login(ALogin, APassword: string): Boolean; overload;
    procedure CallMethod(MethodName: string; Callback: TCallMethodCallback = nil); overload;
    procedure CallMethod(MethodName: string; Param, Value: string; Callback: TCallMethodCallback = nil); overload;
    procedure CallMethod(MethodName: string; Params: TParams; Callback: TCallMethodCallback = nil); overload;
    procedure CallMethodAsync(MethodName: string; Params: TParams; Callback: TCallMethodCallback = nil); overload;
    /// <summary>
    /// Универсальный метод, который позволяет запускать последовательность других методов, сохраняя и фильтруя промежуточные результаты.
    /// https://vk.com/dev/execute
    /// </summary>
    /// <param name="Code">код алгоритма в VKScript - формате, похожем на JavaSсript или ActionScript (предполагается совместимость с ECMAScript). Алгоритм должен завершаться командой return %выражение%. Операторы должны быть разделены точкой с запятой. </param>
    function Execute(Code: string): TResponse;
    procedure ExecuteAsync(Code: string; Callback: TCallMethodCallback = nil);
    procedure SetProxy(IP: string; Port: Integer; UserName: string = ''; Password: string = '');
    property PermissionsList: TPermissions read FPermissionsList write SetPermissionsList;
    //Tools
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
    property Friends: TFriendsController read FFriends;
    property Groups: TGroupsController read FGroups;
    property Photos: TPhotosController read FPhotos;
    property Catalog: TCatalogController read FCatalog;
    property Utils: TUtilsController read FUtils;
    property Video: TVideoController read FVideo;
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
    property Token: string read GetToken write SetToken;
    property TokenExpiry: Int64 read GetTokenExpiry write SetTokenExpiry;
    property Logging: Boolean read FLogging write SetLogging;
    property IsWorking: Boolean read GetIsWorking;
  end;

implementation

uses
  System.DateUtils, System.Net.HttpClient, VK.Entity.AccountInfo;

{ TCustomVK }

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

procedure TCustomVK.CallMethodAsync(MethodName: string; Params: TParams; Callback: TCallMethodCallback);
begin
  TThread.CreateAnonymousThread(
    procedure
    var
      Response: TResponse;
    begin
      Response := Handler.Execute(MethodName, Params);
      if Assigned(Callback) then
      begin
        TThread.Synchronize(nil,
          procedure
          begin
            Callback(Response);
          end);
      end;
    end).Start;
end;

procedure TCustomVK.CallMethod(MethodName: string; Param, Value: string; Callback: TCallMethodCallback);
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
  FLogging := False;
  FUseServiceKeyOnly := False;
  FOAuth2Authenticator := TOAuth2Authenticator.Create(Self);
  FHandler := TVkHandler.Create(Self);
  FHandler.OnError := FVKError;
  FHandler.OnLog := FLog;
  FHandler.Client.Authenticator := FOAuth2Authenticator;
  FHandler.OnCaptcha := FAskCaptcha;
  FHandler.OnConfirm := FOnConfirm;
  //Defaults
  EndPoint := 'https://oauth.vk.com/authorize';
  BaseURL := 'https://api.vk.com/method';
  APIVersion := '5.103';
  PermissionsList := ['groups', 'friends', 'wall', 'photos', 'video', 'docs', 'notes', 'market'];
  //Controllers
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
  FFriends := TFriendsController.Create(FHandler);
  FGroups := TGroupsController.Create(FHandler);
  FPhotos := TPhotosController.Create(FHandler);
  FCatalog := TCatalogController.Create(FHandler);
  FUtils := TUtilsController.Create(FHandler);
  FVideo := TVideoController.Create(FHandler);
  //
  FUploader := TUploader.Create;
end;

destructor TCustomVK.Destroy;
begin
  FBoard.Free;
  FFriends.Free;
  FGroups.Free;
  FPhotos.Free;
  FCatalog.Free;
  FUtils.Free;
  FVideo.Free;
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

procedure TCustomVK.DoErrorLogin(Sender: TObject; E: Exception; Code: Integer; Text: string);
begin
  FVKErrorLogin(Sender, E, Code, Text);
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
  begin
    {$IFDEF NEEDFMX}
    TFormFMXCaptcha.Execute(CaptchaImg, Answer);
    {$ELSE}
    TFormCaptcha.Execute(CaptchaImg, Answer);
    {$ENDIF}
  end;
end;

function TCustomVK.Execute(Code: string): TResponse;
var
  ExecuteResponse: TResponse;
begin
  CallMethod('execute', [['code', Code]],
    procedure(Response: TResponse)
    begin
      ExecuteResponse := Response;
    end);
  Result := ExecuteResponse;
end;

procedure TCustomVK.ExecuteAsync(Code: string; Callback: TCallMethodCallback);
begin
  CallMethodAsync('execute', [['code', Code]], Callback);
end;

procedure TCustomVK.FLog(Sender: TObject; const Value: string);
begin
  if FLogging then
    if Assigned(FOnLog) then
      FOnLog(Self, Value);
end;

procedure TCustomVK.FAuthError(const AText: string; AStatusCode: Integer);
var
  E: Exception;
begin
  E := TVkException.Create('Токен не был получен');
  if Assigned(FOnErrorLogin) then
  begin
    FOnErrorLogin(Self, E, AStatusCode, AText);
    if Assigned(E) then
      E.Free;
  end
  else
  begin
    DoLog(Self, E.Message);
    E.Free;
  end;
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

procedure TCustomVK.FVKErrorLogin(Sender: TObject; E: Exception; Code: Integer; Text: string);
begin
  if Assigned(FOnErrorLogin) then
  begin
    FOnErrorLogin(Self, E, Code, Text);
    if Assigned(E) then
      E.Free;
  end
  else
    raise E;
end;

function TCustomVK.CheckAuth: Boolean;
var
  TM: Int64;
begin
  Result := Utils.GetServerTime(TM);
end;

function TCustomVK.Login(ALogin, APassword: string): Boolean;
{var
  HTTP: THTTPClient;  }
begin
  raise TVkException.Create('Метод в разработке');
 { HTTP := THTTPClient.Create;
  try
    try
      //HTTP.
    finally
      HTTP.Free;
    end;
  except

  end;  }
end;

function TCustomVK.Login: Boolean;
var
  AToken, APasswordHash: string;
  ATokenExpiry: Int64;
begin
  Result := False;
  //FOAuth2Authenticator.AccessToken := EmptyStr;
  FOAuth2Authenticator.ClientID := FAppID;
  FOAuth2Authenticator.ClientSecret := FAppKey;
  FOAuth2Authenticator.ResponseType := TOAuth2ResponseType.rtTOKEN;
  FOAuth2Authenticator.AuthorizationEndpoint := FEndPoint;
  FOAuth2Authenticator.Scope := PermissionsList.ToString;
  APasswordHash := '';
  AToken := Token;
  ATokenExpiry := TokenExpiry;
  if Assigned(FOnAuth) then
    FOnAuth(Self, FOAuth2Authenticator.AuthorizationRequestURI, AToken, ATokenExpiry, APasswordHash);

  if not AToken.IsEmpty then
  begin
    FChangePasswordHash := APasswordHash;
    FOAuth2Authenticator.AccessToken := AToken;
    if ATokenExpiry > 0 then
      FOAuth2Authenticator.AccessTokenExpiry := IncSecond(Now, ATokenExpiry)
    else
      FOAuth2Authenticator.AccessTokenExpiry := 0;
    if CheckAuth then
    begin
      DoLogin;
      Exit(True);
    end
    else
      FAuthError('Ошибка авторизации', -1);
  end;
end;

procedure TCustomVK.SetAPIVersion(const Value: string);
begin
  FAPIVersion := Value;
  FHandler.Client.AddParameter('v', FAPIVersion);
end;

procedure TCustomVK.SetServiceKey(const Value: string);
begin
  FServiceKey := Value;
  if csDesigning in ComponentState then
    Exit;

  if FUseServiceKeyOnly then
  begin
    FHandler.Client.AddParameter('access_token', FServiceKey);
  end;
end;

procedure TCustomVK.SetToken(const Value: string);
begin
  FOAuth2Authenticator.AccessToken := Value;
end;

procedure TCustomVK.SetTokenExpiry(const Value: Int64);
begin
  FOAuth2Authenticator.AccessTokenExpiry := UnixToDateTime(Value, False);
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

procedure TCustomVK.SetLogging(const Value: Boolean);
begin
  FLogging := Value;
end;

procedure TCustomVK.SetBaseURL(const Value: string);
begin
  FBaseURL := Value;
  FHandler.Client.BaseURL := FBaseURL;
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

function TCustomVK.GetIsWorking: Boolean;
begin
  Result := FHandler.Executing;
end;

function TCustomVK.GetPermissions: string;
begin
  Result := FPermissionsList.ToString;
end;

function TCustomVK.GetToken: string;
begin
  Result := FOAuth2Authenticator.AccessToken;
end;

function TCustomVK.GetTokenExpiry: Int64;
begin
  Result := DateTimeToUnix(FOAuth2Authenticator.AccessTokenExpiry, False);
end;

procedure TCustomVK.SetPermissionsList(const Value: TPermissions);
begin
  FPermissionsList := Value;
end;

procedure TCustomVK.SetProxy(IP: string; Port: Integer; UserName, Password: string);
begin
  FHandler.Client.ProxyServer := IP;
  FHandler.Client.ProxyPort := Port;
  FHandler.Client.ProxyUsername := UserName;
  FHandler.Client.ProxyPassword := Password;
end;

procedure TCustomVK.SetUseServiceKeyOnly(const Value: Boolean);
begin
  FUseServiceKeyOnly := Value;
  if csDesigning in ComponentState then
    Exit;

  FHandler.UseServiceKeyOnly := Value;
  if FUseServiceKeyOnly then
  begin
    FHandler.Client.Authenticator := nil;
    FHandler.Client.AddParameter('access_token', FServiceKey);
  end
  else
  begin
    FHandler.Client.Authenticator := FOAuth2Authenticator;
    FHandler.Client.Params.Delete('access_token');
  end;
end;

end.

