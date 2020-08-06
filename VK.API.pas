unit VK.API;

interface

uses
  System.SysUtils, System.Variants, System.Classes, REST.Client, REST.Authenticator.OAuth, VK.Types, VK.Account,
  VK.Handler, VK.Auth, VK.Users, VK.LongPollServer, System.JSON, VK.Messages, System.Generics.Collections, VK.Status,
  VK.Wall, VK.Uploader, VK.Docs, VK.Audio, VK.Likes, VK.Board, REST.Types, VK.Friends, VK.Groups, VK.Photos, VK.Catalog,
  VK.Market, VK.Fave, VK.Notes, VK.Utils, VK.Video, VK.Gifts, VK.Newsfeed, VK.Notifications, VK.Orders, Vk.Pages,
  VK.Polls, VK.Podcasts, VK.Search, VK.Database, VK.Storage, VK.DownloadedGames, VK.Secure, VK.Stats, VK.Stories,
  {$IFDEF NEEDFMX}
  VK.FMX.Captcha,
  {$ELSE}
  VK.Vcl.Captcha,
  {$ENDIF}
  System.Types;

type
  TWalkMethod = reference to function(Offset: Integer; var Cancel: Boolean): Integer;

  TCustomVK = class(TComponent)
    type
      TVkProxy = class(TPersistent)
      private
        FOwner: TCustomVK;
        procedure SetIP(const Value: string);
        procedure SetPassword(const Value: string);
        procedure SetPort(const Value: Integer);
        procedure SetUserName(const Value: string);
        function GetIP: string;
        function GetPassword: string;
        function GetPort: Integer;
        function GetUserName: string;
      public
        constructor Create(AOwner: TCustomVK);
        procedure SetProxy(AIP: string; APort: Integer; AUserName: string = ''; APassword: string = '');
      published
        property IP: string read GetIP write SetIP;
        property Port: Integer read GetPort write SetPort;
        property UserName: string read GetUserName write SetUserName;
        property Password: string read GetPassword write SetPassword;
      end;
  private
    FAccount: TAccountController;
    FAPIVersion: string;
    FAppID: string;
    FAppKey: string;
    FAudio: TAudioController;
    FAuth: TAuthController;
    FBaseURL: string;
    FBoard: TBoardController;
    FCatalog: TCatalogController;
    FChangePasswordHash: string;
    FDoc: TDocController;
    FEndPoint: string;
    FFave: TFaveController;
    FFriends: TFriendsController;
    FGroups: TGroupsController;
    FHandler: TVkHandler;
    FIsLogin: Boolean;
    FLang: TVkLang;
    FLikes: TLikesController;
    FLogging: Boolean;
    FMarket: TMarketController;
    FMessages: TMessagesController;
    FNotes: TNotesController;
    FOAuth2Authenticator: TOAuth2Authenticator;
    FOnAuth: TOnAuth;
    FOnCaptcha: TOnCaptcha;
    FOnConfirm: TOnConfirm;
    FOnError: TOnVKError;
    FOnErrorLogin: TOnVKError;
    FOnLog: TOnLog;
    FOnLogin: TOnLogin;
    FPermissions: TVkPermissions;
    FPhotos: TPhotosController;
    FProxy: TVkProxy;
    FServiceKey: string;
    FStatus: TStatusController;
    FUploader: TUploader;
    FUserId: Integer;
    FUsers: TUsersController;
    FUseServiceKeyOnly: Boolean;
    FUtils: TUtilsController;
    FVideo: TVideoController;
    FWall: TWallController;
    FGifts: TGiftsController;
    FNewsfeed: TNewsfeedController;
    FLogResponse: Boolean;
    FNotifications: TNotificationsController;
    FOrders: TOrdersController;
    FPages: TPagesController;
    FPolls: TPollsController;
    FPodcasts: TPodcastsController;
    FSearch: TSearchController;
    FDatabase: TDatabaseController;
    FStorage: TStorageController;
    FDownloadedGames: TDownloadedGamesController;
    FSecure: TSecureController;
    FStats: TStatsController;
    FStories: TStoriesController;
    function CheckAuth: Boolean;
    function GetIsWorking: Boolean;
    function GetTestMode: Boolean;
    function GetToken: string;
    function GetTokenExpiry: Int64;
    procedure DoLogin;
    function DoOnError(Sender: TObject; E: Exception; Code: Integer; Text: string): Boolean;
    procedure FAskCaptcha(Sender: TObject; const CaptchaImg: string; var Answer: string);
    procedure FAuthError(const AText: string; AStatusCode: Integer);
    procedure FLog(Sender: TObject; const Value: string);
    procedure FVKError(Sender: TObject; E: Exception; Code: Integer; Text: string);
    procedure SetAPIVersion(const Value: string);
    procedure SetAppID(const Value: string);
    procedure SetAppKey(const Value: string);
    procedure SetBaseURL(const Value: string);
    procedure SetEndPoint(const Value: string);
    procedure SetHandler(const Value: TVkHandler);
    procedure SetLang(const Value: TVkLang);
    procedure SetLogging(const Value: Boolean);
    procedure SetOnAuth(const Value: TOnAuth);
    procedure SetOnCaptcha(const Value: TOnCaptcha);
    procedure SetOnConfirm(const Value: TOnConfirm);
    procedure SetOnError(const Value: TOnVKError);
    procedure SetOnErrorLogin(const Value: TOnVKError);
    procedure SetOnLog(const Value: TOnLog);
    procedure SetOnLogin(const Value: TOnLogin);
    procedure SetPermissions(const Value: TVkPermissions);
    procedure SetServiceKey(const Value: string);
    procedure SetTestMode(const Value: Boolean);
    procedure SetToken(const Value: string);
    procedure SetTokenExpiry(const Value: Int64);
    procedure SetUseServiceKeyOnly(const Value: Boolean);
    procedure SetUsePseudoAsync(const Value: Boolean);
    function GetUsePseudoAsync: Boolean;
    procedure SetLogResponse(const Value: Boolean);
    function GetUserId: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetOAuth2RequestURI: string;
    function LoadUserInfo: Boolean;
    function Login: Boolean; overload;
    function Login(ALogin, APassword: string): Boolean; overload;
    procedure DoLog(Sender: TObject; Text: string);
    procedure DoError(Sender: TObject; E: Exception; Code: Integer; Text: string = '');
    procedure CallMethod(MethodName: string; Callback: TCallMethodCallback = nil); overload;
    procedure CallMethod(MethodName: string; Param: TParam; Callback: TCallMethodCallback = nil); overload;
    procedure CallMethod(MethodName: string; Params: TParams; Callback: TCallMethodCallback = nil); overload;
    procedure CallMethodAsync(MethodName: string; Params: TParams; Callback: TCallMethodCallback = nil); overload;
    /// <summary>
    /// Универсальный метод, который позволяет запускать последовательность других методов, сохраняя и фильтруя промежуточные результаты.
    /// https://vk.com/dev/execute
    /// </summary>
    /// <param name="Code: string">Код алгоритма в VKScript - формате, похожем на JavaSсript или ActionScript (предполагается совместимость с ECMAScript). Алгоритм должен завершаться командой return %выражение%. Операторы должны быть разделены точкой с запятой. </param>
    function Execute(Code: string): TResponse;
    /// <summary>
    /// Универсальный метод, который позволяет запускать последовательность других методов, сохраняя и фильтруя промежуточные результаты.
    /// https://vk.com/dev/execute
    /// </summary>
    /// <param name="Code: string">Код алгоритма в VKScript - формате, похожем на JavaSсript или ActionScript (предполагается совместимость с ECMAScript). Алгоритм должен завершаться командой return %выражение%. Операторы должны быть разделены точкой с запятой. </param>
    /// <param name="Callback: TCallMethodCallback = nil"> Метод, который будет выполнен после выполнения Execute </param>
    procedure ExecuteAsync(Code: string; Callback: TCallMethodCallback = nil);
    /// <summary>
    /// Вспомогательный метод, для выполнения методов с Count и Offset
    /// </summary>
    procedure Walk(Method: TWalkMethod; Count: Integer);
    //Tools
    /// <summary>
    /// Методы для загрузки файлов на сервера ВК
    /// </summary>
    property Uploader: TUploader read FUploader;
    //Группы методов
    /// <summary>
    /// Методы для работы с аккаунтом.
    /// </summary>
    property Account: TAccountController read FAccount;
    /// <summary>
    /// Методы для работы с авторизацией.
    /// </summary>
    property Auth: TAuthController read FAuth;
    /// <summary>
    /// Методы для работы с аудиозаписями.
    /// </summary>
    property Audio: TAudioController read FAudio;
    /// <summary>
    /// Методы для работы с обсуждениями.
    /// </summary>
    property Board: TBoardController read FBoard;
    /// <summary>
    /// Методы для работы с каталогом рекомендация.
    /// </summary>
    property Catalog: TCatalogController read FCatalog;
    /// <summary>
    /// Методы этой секции предоставляют доступ к базе данных учебных заведений ВКонтакте.
    /// </summary>
    property Database: TDatabaseController read FDatabase;
    /// <summary>
    /// Методы для работы с документами.
    /// </summary>
    property Docs: TDocController read FDoc;
    /// <summary>
    /// Список методов секции downloadedGames.
    /// </summary>
    property DownloadedGames: TDownloadedGamesController read FDownloadedGames;
    /// <summary>
    /// Методы для работы с закладками.
    /// </summary>
    property Fave: TFaveController read FFave;
    /// <summary>
    /// Методы для работы с друзьями.
    /// </summary>
    property Friends: TFriendsController read FFriends;
    /// <summary>
    /// Методы для работы с подарками.
    /// </summary>
    property Gifts: TGiftsController read FGifts;
    /// <summary>
    /// Методы для работы с сообществами.
    /// </summary>
    property Groups: TGroupsController read FGroups;
    /// <summary>
    /// Методы для работы с отметками «Мне нравится».
    /// </summary>
    property Likes: TLikesController read FLikes;
    /// <summary>
    /// Методы market позволяют работать с товарами в сообществах.
    /// </summary>
    property Market: TMarketController read FMarket;
    /// <summary>
    /// Методы для работы с личными сообщениями.
    /// </summary>
    property Messages: TMessagesController read FMessages;
    /// <summary>
    /// Методы для работы с новостной лентой пользователя.
    /// </summary>
    property Newsfeed: TNewsfeedController read FNewsfeed;
    /// <summary>
    /// Методы для работы с заметками.
    /// </summary>
    property Notes: TNotesController read FNotes;
    /// <summary>
    /// Notifications
    /// </summary>
    property Notifications: TNotificationsController read FNotifications;
    /// <summary>
    /// Методы этой секции предоставляют дополнительную возможность управления состоянием заказов, которые были сделаны пользователями в приложениях.
    /// </summary>
    property Orders: TOrdersController read FOrders;
    /// <summary>
    /// Методы для работы с фотографиями.
    /// </summary>
    property Pages: TPagesController read FPages;
    /// <summary>
    /// Методы для работы с фотографиями.
    /// </summary>
    property Photos: TPhotosController read FPhotos;
    /// <summary>
    /// Методы для работы с подкастами.
    /// </summary>
    property Podcasts: TPodcastsController read FPodcasts;
    /// <summary>
    /// Методы для работы с опросами.
    /// </summary>
    property Polls: TPollsController read FPolls;
    /// <summary>
    /// Методы для работы с поиском.
    /// </summary>
    property Search: TSearchController read FSearch;
    /// <summary>
    /// Методы для работы с поиском.
    /// </summary>
    property Secure: TSecureController read FSecure;
    /// <summary>
    /// Методы для работы со статусом.
    /// </summary>
    property Status: TStatusController read FStatus;
    /// <summary>
    /// Методы для работы со статистикой.
    /// </summary>
    property Stats: TStatsController read FStats;
    /// <summary>
    /// Методы для работы с переменными в приложении.
    /// </summary>
    property Storage: TStorageController read FStorage;
    /// <summary>
    /// Методы для работы со историями.
    /// </summary>
    property Stories: TStoriesController read FStories;
    /// <summary>
    /// Методы для работы с данными пользователей.
    /// </summary>
    property Users: TUsersController read FUsers;
    /// <summary>
    /// Служебные методы.
    /// </summary>
    property Utils: TUtilsController read FUtils;
    /// <summary>
    /// Методы для работы с видеозаписями.
    /// </summary>
    property Video: TVideoController read FVideo;
    /// <summary>
    /// Методы для работы с записями на стене.
    /// </summary>
    property Wall: TWallController read FWall;
    //
    property AppID: string read FAppID write SetAppID;
    property AppKey: string read FAppKey write SetAppKey;
    property EndPoint: string read FEndPoint write SetEndPoint;
    property Handler: TVkHandler read FHandler write SetHandler;
    property APIVersion: string read FAPIVersion;
    property BaseURL: string read FBaseURL write SetBaseURL;
    property ServiceKey: string read FServiceKey write SetServiceKey;
    property UseServiceKeyOnly: Boolean read FUseServiceKeyOnly write SetUseServiceKeyOnly;
    property IsLogin: Boolean read FIsLogin;
    property ChangePasswordHash: string read FChangePasswordHash;
    property UserId: Integer read GetUserId;
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
    property LogResponse: Boolean read FLogResponse write SetLogResponse;
    property IsWorking: Boolean read GetIsWorking;
    property TestMode: Boolean read GetTestMode write SetTestMode;
    property Permissions: TVkPermissions read FPermissions write SetPermissions;
    property Lang: TVkLang read FLang write SetLang;
    property Proxy: TVkProxy read FProxy write FProxy;
    property UsePseudoAsync: Boolean read GetUsePseudoAsync write SetUsePseudoAsync;
  end;

const
  ERROR_INTERNAL = -1;

implementation

uses
  System.DateUtils, System.Net.HttpClient, VK.Entity.AccountInfo, VK.CommonUtils, VK.Entity.Profile;

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
        Callback(Response);
      end;
    end).Start;
end;

procedure TCustomVK.CallMethod(MethodName: string; Param: TParam; Callback: TCallMethodCallback);
var
  Response: TResponse;
begin
  Response := Handler.Execute(MethodName, [Param]);
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
  FUserId := -1;
  FLang := vlAuto;
  FUseServiceKeyOnly := False;
  FProxy := TVkProxy.Create(Self);
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
  SetAPIVersion('5.120');
  Permissions := [
    TVkPermission.Groups,
    TVkPermission.Friends,
    TVkPermission.Wall,
    TVkPermission.Photos,
    TVkPermission.Video,
    TVkPermission.Docs,
    TVkPermission.Notes,
    TVkPermission.Market];
  //Controllers
  FAccount := TAccountController.Create(FHandler);
  FAuth := TAuthController.Create(FHandler);
  FUsers := TUsersController.Create(FHandler);
  FMessages := TMessagesController.Create(FHandler);
  FNewsfeed := TNewsfeedController.Create(FHandler);
  FStatus := TStatusController.Create(FHandler);
  FStats := TStatsController.Create(FHandler);
  FStorage := TStorageController.Create(FHandler);
  FStories := TStoriesController.Create(FHandler);
  FSearch := TSearchController.Create(FHandler);
  FSecure := TSecureController.Create(FHandler);
  FWall := TWallController.Create(FHandler);
  FDoc := TDocController.Create(FHandler);
  FDownloadedGames := TDownloadedGamesController.Create(FHandler);
  FLikes := TLikesController.Create(FHandler);
  FAudio := TAudioController.Create(FHandler);
  FBoard := TBoardController.Create(FHandler);
  FFriends := TFriendsController.Create(FHandler);
  FGroups := TGroupsController.Create(FHandler);
  FGifts := TGiftsController.Create(FHandler);
  FPhotos := TPhotosController.Create(FHandler);
  FPodcasts := TPodcastsController.Create(FHandler);
  FPolls := TPollsController.Create(FHandler);
  FPages := TPagesController.Create(FHandler);
  FOrders := TOrdersController.Create(FHandler);
  FCatalog := TCatalogController.Create(FHandler);
  FDatabase := TDatabaseController.Create(FHandler);
  FUtils := TUtilsController.Create(FHandler);
  FVideo := TVideoController.Create(FHandler);
  FMarket := TMarketController.Create(FHandler);
  FFave := TFaveController.Create(FHandler);
  FNotes := TNotesController.Create(FHandler);
  FNotifications := TNotificationsController.Create(FHandler);
  //Tools
  FUploader := TUploader.Create;
end;

destructor TCustomVK.Destroy;
begin
  FBoard.Free;
  FFriends.Free;
  FNewsfeed.Free;
  FGroups.Free;
  FGifts.Free;
  FPhotos.Free;
  FPodcasts.Free;
  FPolls.Free;
  FOrders.Free;
  FPages.Free;
  FCatalog.Free;
  FDatabase.Free;
  FUtils.Free;
  FNotes.Free;
  FVideo.Free;
  FMarket.Free;
  FFave.Free;
  FLikes.Free;
  FAudio.Free;
  FDoc.Free;
  FDownloadedGames.Free;
  FUploader.Free;
  FWall.Free;
  FStatus.Free;
  FSearch.Free;
  FSecure.Free;
  FStorage.Free;
  FStats.Free;
  FStories.Free;
  FUsers.Free;
  FAccount.Free;
  FAuth.Free;
  FMessages.Free;
  FNotifications.Free;
  FProxy.Free;
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

function TCustomVK.DoOnError(Sender: TObject; E: Exception; Code: Integer; Text: string): Boolean;
begin
  Result := Assigned(FOnError);
  if Result then
  begin
    try
      FOnError(Sender, E, Code, Text);
    except
      //Ну зачем так?
    end;
  end;
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
  if DoOnError(Self, E, Code, Text) then
  begin
    if Assigned(E) then
      E.Free;
  end
  else
    raise E;
end;

function TCustomVK.LoadUserInfo: Boolean;
var
  User: TVkProfile;
begin
  Result := Users.Get(User);
  if Result then
  begin
    FUserId := User.Id;
    User.Free;
  end;
end;

function TCustomVK.CheckAuth: Boolean;
var
  MT: Int64;
begin
  Result := Utils.GetServerTime(MT);
end;

function TCustomVK.Login(ALogin, APassword: string): Boolean;
begin
  raise TVkException.Create('RU: Метод в разработке / EN: Method in development');
end;

function TCustomVK.GetOAuth2RequestURI: string;
begin
  FOAuth2Authenticator.ClientID := FAppID;
  FOAuth2Authenticator.ClientSecret := FAppKey;
  FOAuth2Authenticator.ResponseType := TOAuth2ResponseType.rtTOKEN;
  FOAuth2Authenticator.AuthorizationEndpoint := FEndPoint;
  FOAuth2Authenticator.Scope := FPermissions.ToString;
  Result := FOAuth2Authenticator.AuthorizationRequestURI;
end;

function TCustomVK.Login: Boolean;
var
  AToken, APasswordHash: string;
  ATokenExpiry: Int64;
  AuthUrl: string;
begin
  Result := False;
  AuthUrl := GetOAuth2RequestURI;
  APasswordHash := '';
  AToken := Token;
  ATokenExpiry := TokenExpiry;
  if Assigned(FOnAuth) then
    FOnAuth(Self, AuthUrl, AToken, ATokenExpiry, APasswordHash);

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

procedure TCustomVK.SetTestMode(const Value: Boolean);
begin
  FHandler.Client.AddParameter('test_mode', BoolToString(Value));
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

procedure TCustomVK.SetLang(const Value: TVkLang);
begin
  FLang := Value;
  if FLang <> vlAuto then
    FHandler.Client.AddParameter('lang', Ord(Value).ToString)
  else
    FHandler.Client.Params.Delete('lang');
end;

procedure TCustomVK.SetLogging(const Value: Boolean);
begin
  FLogging := Value;
  FHandler.Logging := Value;
end;

procedure TCustomVK.SetLogResponse(const Value: Boolean);
begin
  FLogResponse := Value;
  FHandler.LogResponse := Value;
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

function TCustomVK.GetIsWorking: Boolean;
begin
  Result := FHandler.Executing;
end;

function TCustomVK.GetTestMode: Boolean;
begin
  Result := False;
  if Assigned(FHandler.Client.Params.ParameterByName('test_mode')) then
    Result := FHandler.Client.Params.ParameterByName('test_mode').Value = '1';
end;

function TCustomVK.GetToken: string;
begin
  Result := FOAuth2Authenticator.AccessToken;
end;

function TCustomVK.GetTokenExpiry: Int64;
begin
  try
    if FOAuth2Authenticator.AccessTokenExpiry <> 0 then
      Result := DateTimeToUnix(FOAuth2Authenticator.AccessTokenExpiry, False)
    else
      Result := 0;
  except
    Result := 0;
  end;
end;

function TCustomVK.GetUsePseudoAsync: Boolean;
begin
  Result := FHandler.UsePseudoAsync;
end;

function TCustomVK.GetUserId: Integer;
begin
  if (FUserId > 0) or LoadUserInfo then
    Result := FUserId
  else
    Result := -1;
end;

procedure TCustomVK.SetPermissions(const Value: TVkPermissions);
begin
  FPermissions := Value;
end;

procedure TCustomVK.SetUsePseudoAsync(const Value: Boolean);
begin
  FHandler.UsePseudoAsync := Value;
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

procedure TCustomVK.Walk(Method: TWalkMethod; Count: Integer);
var
  Cnt, Offset: Integer;
  Cancel: Boolean;
begin
  try
    Offset := 0;
    Cancel := False;
    repeat
      Cnt := Method(Offset, Cancel);
      Inc(Offset, Count);
    until (Cnt < Count) or Cancel;
  except
    on E: Exception do
      DoError(Self, E, ERROR_INTERNAL);
  end;
end;

{ TCustomVK.TVkProxy }

constructor TCustomVK.TVkProxy.Create(AOwner: TCustomVK);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TCustomVK.TVkProxy.GetIP: string;
begin
  Result := FOwner.Handler.Client.ProxyServer;
end;

function TCustomVK.TVkProxy.GetPassword: string;
begin
  Result := FOwner.Handler.Client.ProxyPassword;
end;

function TCustomVK.TVkProxy.GetPort: Integer;
begin
  Result := FOwner.Handler.Client.ProxyPort;
end;

function TCustomVK.TVkProxy.GetUserName: string;
begin
  Result := FOwner.Handler.Client.ProxyUsername;
end;

procedure TCustomVK.TVkProxy.SetIP(const Value: string);
begin
  FOwner.Handler.Client.ProxyServer := Value;
end;

procedure TCustomVK.TVkProxy.SetPassword(const Value: string);
begin
  FOwner.Handler.Client.ProxyPassword := Value;
end;

procedure TCustomVK.TVkProxy.SetPort(const Value: Integer);
begin
  FOwner.Handler.Client.ProxyPort := Value;
end;

procedure TCustomVK.TVkProxy.SetUserName(const Value: string);
begin
  FOwner.Handler.Client.ProxyUsername := Value;
end;

procedure TCustomVK.TVkProxy.SetProxy(AIP: string; APort: Integer; AUserName, APassword: string);
begin
  IP := AIP;
  Port := APort;
  Username := AUserName;
  Password := APassword;
end;

end.

