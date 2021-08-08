unit VK.API;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.JSON,
  REST.Authenticator.OAuth, VK.Types, VK.Account, VK.Handler, VK.Auth, VK.Users,
  VK.Messages, VK.Status, VK.Wall, VK.Docs, VK.Audio, VK.Likes, VK.Board,
  VK.Friends, VK.Groups, VK.Photos, VK.Catalog, VK.Market, VK.Fave, VK.Notes,
  VK.Utils, VK.Video, VK.Gifts, VK.Newsfeed, VK.Notifications, VK.Orders,
  Vk.Pages, VK.Polls, VK.Podcasts, VK.Search, VK.Database, VK.Storage,
  VK.DownloadedGames, VK.Secure, VK.Stats, VK.Stories, VK.Apps, VK.Clients,
  VK.Donut, VK.Streaming;

type
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
    const
      Version = '5.144';
  private
    FAccount: TAccountController;
    FAPIVersion: string;
    FAppID: string;
    FAppKey: string;
    FApps: TAppsController;
    FAudio: TAudioController;
    FAuth: TAuthController;
    FBaseURL: string;
    FBoard: TBoardController;
    FCatalog: TCatalogController;
    FChangePasswordHash: string;
    FDatabase: TDatabaseController;
    FDoc: TDocController;
    FDonut: TDonutController;
    FDownloadedGames: TDownloadedGamesController;
    FEndPoint: string;
    FFave: TFaveController;
    FFriends: TFriendsController;
    FGifts: TGiftsController;
    FGroups: TGroupsController;
    FHandler: TVkHandler;
    FIsLogin: Boolean;
    FLang: TVkLang;
    FLikes: TLikesController;
    FLogging: Boolean;
    FLogResponse: Boolean;
    FMarket: TMarketController;
    FMessages: TMessagesController;
    FNewsfeed: TNewsfeedController;
    FNotes: TNotesController;
    FNotifications: TNotificationsController;
    FOAuth2Authenticator: TOAuth2Authenticator;
    FOnAuth: TOnAuth;
    FOnCaptcha: TOnCaptcha;
    FOnConfirm: TOnConfirm;
    FOnError: TOnVKError;
    FOnLog: TOnLog;
    FOnLogin: TOnLogin;
    FOrders: TOrdersController;
    FPages: TPagesController;
    FPermissions: TVkPermissions;
    FPhotos: TPhotosController;
    FPodcasts: TPodcastsController;
    FPolls: TPollsController;
    FProxy: TVkProxy;
    FSearch: TSearchController;
    FSecure: TSecureController;
    FServiceKey: string;
    FStats: TStatsController;
    FStatus: TStatusController;
    FStorage: TStorageController;
    FStories: TStoriesController;
    FUserId: Integer;
    FUsers: TUsersController;
    FUseServiceKeyOnly: Boolean;
    FUtils: TUtilsController;
    FVideo: TVideoController;
    FWall: TWallController;
    FStreaming: TStreamingController;
    function CheckAuth: Boolean;
    function DoOnError(Sender: TObject; E: Exception; Code: Integer; Text: string): Boolean;
    function GetIsWorking: Boolean;
    function GetTestMode: Boolean;
    function GetToken: string;
    function GetTokenExpiry: Int64;
    procedure DoLogin;
    procedure FAskCaptcha(Sender: TObject; const CaptchaImg: string; var Answer: string);
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
    procedure SetApplication(const Value: TVkApplicationData);
    function GetApplication: TVkApplicationData;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>
    /// Метод возвращает запрос для получения токена через OAuth2
    /// </summary>
    function GetOAuth2RequestURI: string;
    /// <summary>
    /// Загрузить информацию о пользователе
    /// </summary>
    function LoadUserInfo: Boolean;
    /// <summary>
    /// Метод выполняет проверку существования Token, если его нет, выполняет OnAuth, после чего, проверяет доступ к API.
    /// Авторизация рекомендуемым Вконтакте способом - OAuth2 или через имеющийся токен.
    /// <b>Не забывайте сохранять токен при закрытии приложения</b>
    /// </summary>
    function Login: Boolean; overload;
    /// <summary>
    /// Метод выполняет авторизацию, запрашивая токен у сервера, после чего, проверяет доступ к API.
    /// Авторизация "Client credentials flow" используя AppId и AppKey через логин и пароль.
    /// <b>Не забывайте сохранять токен при закрытии приложения</b>
    /// </summary>
    function Login(ALogin, APassword: string; On2FA: TOn2FA = nil): Boolean; overload;
    /// <summary>
    /// Генерировать событие лога
    /// </summary>
    procedure DoLog(Sender: TObject; Text: string);
    /// <summary>
    /// Генерировать событие ошибки
    /// </summary>
    procedure DoError(Sender: TObject; E: Exception; Code: Integer; Text: string = '');
    /// <summary>
    /// Выполнить метод
    /// </summary>
    procedure CallMethod(MethodName: string; Params: TParams = []; Callback: TCallMethodCallback = nil); overload;
    /// <summary>
    /// Выполнить метод асинхронно
    /// </summary>
    procedure CallMethodAsync(MethodName: string; Params: TParams = []; Callback: TCallMethodCallback = nil); overload;
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
    /// <summary>
    /// Метод для загрузки файлов на сервер. UploadUrl должен быть получен соответствющим типу файла образом.
    /// Например, для Фото в альбом - Photos.GetUploadServer.
    /// </summary>
    function Upload(const UploadUrl: string; FileNames: array of string; var Response: string): Boolean;
    //Группы методов
    /// <summary>
    /// Методы для работы с аккаунтом.
    /// </summary>
    property Account: TAccountController read FAccount;
    /// <summary>
    /// Методы для работы с приложениями.
    /// </summary>
    property Apps: TAppsController read FApps;
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
    /// Методы для работы с донатом.
    /// </summary>
    property Donut: TDonutController read FDonut;
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
    /// Методы для работы со стриамами.
    /// </summary>
    property Streaming: TStreamingController read FStreaming;
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
    ////////////////////////////////////////////////////////////////////////////
    /// <summary>
    /// ID приложения
    /// </summary>
    property AppID: string read FAppID write SetAppID;
    /// <summary>
    /// Защищённый ключ приложения
    /// </summary>
    property AppKey: string read FAppKey write SetAppKey;
    /// <summary>
    /// URL, который используется для авторизации через OAuth2
    /// </summary>
    property EndPoint: string read FEndPoint write SetEndPoint;
    /// <summary>
    /// Обработчик запросов. Реализует псевдо асинхронность и очередь выполнения
    /// </summary>
    property Handler: TVkHandler read FHandler write SetHandler;
    /// <summary>
    /// Версия API VK, которая поддеживается текущей оберткой
    /// </summary>
    property APIVersion: string read FAPIVersion;
    /// <summary>
    /// Базовый URL для доступа к метода VK API (https://api.vk.com/method)
    /// </summary>
    property BaseURL: string read FBaseURL write SetBaseURL;
    /// <summary>
    /// Сервисный ключ для доступа к API VK
    /// </summary>
    property ServiceKey: string read FServiceKey write SetServiceKey;
    /// <summary>
    /// Если установлен флаг, то будет использоваться сервисный ключ для доступа к методам ВК
    /// </summary>
    property UseServiceKeyOnly: Boolean read FUseServiceKeyOnly write SetUseServiceKeyOnly;
    /// <summary>
    /// True, если авторизация успешна
    /// </summary>
    property IsLogin: Boolean read FIsLogin;
    /// <summary>
    /// Хеш, получаемый для смены пароля ВК
    /// </summary>
    property ChangePasswordHash: string read FChangePasswordHash;
    /// <summary>
    /// Идентификатор пользователя (будет запрошен, если не сохранен)
    /// </summary>
    property UserId: Integer read GetUserId;
    /// <summary>
    /// Событие, которое происходит, если токен успешно получен и успешно пройдена проверка авторизации
    /// </summary>
    property OnLogin: TOnLogin read FOnLogin write SetOnLogin;
    /// <summary>
    /// Событие, которое происходит при возникновении ошибки
    /// </summary>
    property OnError: TOnVKError read FOnError write SetOnError;
    /// <summary>
    /// Событие, которое происходит при логировании
    /// </summary>
    property OnLog: TOnLog read FOnLog write SetOnLog;
    /// <summary>
    /// Событие, которое происходит когда ВК требует разгадать капчу (по умолчанию используется стандартный диалог)
    /// </summary>
    property OnCaptcha: TOnCaptcha read FOnCaptcha write SetOnCaptcha;
    /// <summary>
    /// Событие, которое происходит когда ВК требует подтверждения от пользователя (по умолчанию подтверждается)
    /// </summary>
    property OnConfirm: TOnConfirm read FOnConfirm write SetOnConfirm;
    /// <summary>
    /// Событие, которое происходит когда не указан токен и требуется его получить
    /// </summary>
    property OnAuth: TOnAuth read FOnAuth write SetOnAuth;
    /// <summary>
    /// Токен
    /// </summary>
    property Token: string read GetToken write SetToken;
    /// <summary>
    /// Срок действия токена UNIXTIME
    /// </summary>
    property TokenExpiry: Int64 read GetTokenExpiry write SetTokenExpiry;
    /// <summary>
    /// Логировать ли запросы
    /// </summary>
    property Logging: Boolean read FLogging write SetLogging;
    /// <summary>
    /// Логировать ответ запроса
    /// </summary>
    property LogResponse: Boolean read FLogResponse write SetLogResponse;
    /// <summary>
    /// Обработчик запросов выполняет какой-то запрос
    /// </summary>
    property IsWorking: Boolean read GetIsWorking;
    /// <summary>
    /// Передаваемый параметр "Тестовый режим"
    /// </summary>
    property TestMode: Boolean read GetTestMode write SetTestMode;
    /// <summary>
    /// Права приложения, которые будут запрошены при авторизации через OAuth2
    /// </summary>
    property Permissions: TVkPermissions read FPermissions write SetPermissions;
    /// <summary>
    /// Передаваемый параметр языка (ВК)
    /// </summary>
    property Lang: TVkLang read FLang write SetLang;
    /// <summary>
    /// Настройки прокси
    /// </summary>
    property Proxy: TVkProxy read FProxy write FProxy;
    /// <summary>
    /// Использовать "псевдо" асинхронность. Если метод будет вызван в основном потоке и будет включен этот флаг, то будет автоматически создан анонимный поток, завершение которого будет производится путём Application.ProccessMessages;
    /// </summary>
    property UsePseudoAsync: Boolean read GetUsePseudoAsync write SetUsePseudoAsync;
    /// <summary>
    /// Данные клиента AppId (client_id) + AppKey (client_secret)
    /// </summary>
    property Application: TVkApplicationData read GetApplication write SetApplication;
  end;

const
  ERROR_INTERNAL = -1;

implementation

uses
  System.DateUtils, System.Net.Mime, System.Net.HttpClient, VK.CommonUtils,
  VK.Entity.Profile, VK.Entity.Login;

{ TCustomVK }

procedure TCustomVK.CallMethod(MethodName: string; Params: TParams; Callback: TCallMethodCallback);
var
  Response: TResponse;
begin
  Response := Handler.Execute(MethodName, Params);
  if Assigned(Callback) then
    Callback(Response);
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
        Callback(Response);
    end).Start;
end;

function TCustomVK.Upload(const UploadUrl: string; FileNames: array of string; var Response: string): Boolean;
var
  HTTP: THTTPClient;
  Data: TMultipartFormData;
  ResStream: TStringStream;
  JSON: TJSONValue;
  FileName: string;
begin
  Result := False;
  Data := TMultipartFormData.Create;
  HTTP := THTTPClient.Create;
  ResStream := TStringStream.Create;
  try
    for FileName in FileNames do
    begin
      if not FileName.IsEmpty then
        Data.AddFile('file', FileName);
    end;
    if HTTP.Post(UploadUrl, Data, ResStream).StatusCode = 200 then
    begin
      try
        JSON := TJSONObject.ParseJSONValue(ResStream.DataString);
        try
          Response := JSON.GetValue<string>('file');
        finally
          JSON.Free;
        end;
      except
        Response := '';
      end;
      Result := not Response.IsEmpty;
      if not Result then
        Response := ResStream.DataString;
    end;
  finally
    ResStream.Free;
    Data.Free;
    HTTP.Free;
  end;
end;

constructor TCustomVK.Create(AOwner: TComponent);
begin
  inherited;
  FIsLogin := False;
  FLogging := False;
  FUserId := -1;
  FLang := TVkLang.Auto;
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
  SetAPIVersion(Version);
  Permissions := [TVkPermission.Groups, TVkPermission.Friends, TVkPermission.Wall, TVkPermission.Photos, TVkPermission.Video, TVkPermission.Docs, TVkPermission.Notes, TVkPermission.Market];
  //Controllers
  FAccount := TAccountController.Create(FHandler);
  FAuth := TAuthController.Create(FHandler);
  FApps := TAppsController.Create(FHandler);
  FDonut := TDonutController.Create(FHandler);
  FUsers := TUsersController.Create(FHandler);
  FMessages := TMessagesController.Create(FHandler);
  FNewsfeed := TNewsfeedController.Create(FHandler);
  FStatus := TStatusController.Create(FHandler);
  FStats := TStatsController.Create(FHandler);
  FStorage := TStorageController.Create(FHandler);
  FStories := TStoriesController.Create(FHandler);
  FStreaming := TStreamingController.Create(FHandler);
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
  FWall.Free;
  FStatus.Free;
  FSearch.Free;
  FSecure.Free;
  FStorage.Free;
  FStats.Free;
  FStories.Free;
  FStreaming.Free;
  FUsers.Free;
  FAccount.Free;
  FApps.Free;
  FDonut.Free;
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
      try
        FOnError(Sender, E, Code, Text);
      finally
        if Assigned(E) then
          E.Free;
      end;
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
    //raise TVkE.Create('Error Message');
{ TODO -oHemulGM -c : Captcha 30.07.2021 23:08:35 }

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

procedure TCustomVK.FVKError(Sender: TObject; E: Exception; Code: Integer; Text: string);
begin
  if not DoOnError(Self, E, Code, Text) then
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
  Result := Utils.GetServerTimeUnix(MT);
end;

function TCustomVK.Login(ALogin, APassword: string; On2FA: TOn2FA): Boolean;
var
  HTTP: THTTPClient;
  Response: TStringStream;
  EndResponse: IHTTPResponse;
  FormData: TStringList;
  Info: TVkLoginInfo;
  Remember: Boolean;
  Hash, Code, Url, CaptchaSid: string;
begin
  Token := '';
  HTTP := THTTPClient.Create;
  HTTP.HandleRedirects := True;
  Response := TStringStream.Create;
  try
    Url := Format('https://oauth.vk.com/token?grant_type=password&client_id=%s&client_secret=%s&username=%s&password=%s', [AppID, AppKey, ALogin, APassword]);
    case HTTP.Get(Url, Response).StatusCode of
      401:
        begin
          try
              {$WARNINGS OFF}
            Info := TVkLoginInfo.FromJsonString<TVkLoginInfo>(UTF8ToWideString(Response.DataString));
              {$WARNINGS ON}
            try
              if not Info.Error.IsEmpty then
              begin
                if Info.Error = 'need_validation' then
                begin
                  if TVkValidationType.FromString(Info.ValidationType) <> TVkValidationType.Unknown then
                  begin
                    if HTTP.Get(Info.RedirectUri, Response).StatusCode = 200 then
                    begin
                      if GetActionLinkHash(Response.DataString, Hash) then
                      begin
                        if On2FA(TVkValidationType.FromString(Info.ValidationType), Code, Remember) then
                        begin
                          FormData := TStringList.Create;
                          try
                            FormData.AddPair('code', Code);
                            FormData.AddPair('remember', BoolToString(Remember));

                            HTTP.HandleRedirects := False;
                            EndResponse := HTTP.Post('https://vk.com/login?act=authcheck_code&hash=' + Hash, FormData);
                            Response.SaveToFile('D:\temp.txt');
                            while EndResponse.StatusCode = 200 do
                            begin
                              Response.LoadFromStream(EndResponse.ContentStream);
                              if CheckForCaptcha(Response.DataString, CaptchaSid) then
                              begin
                                Code := '';
                                FAskCaptcha(Self, 'https://vk.com/captcha.php?sid=' + CaptchaSid, Code);
                                if not Code.IsEmpty then
                                begin
                                  EndResponse := HTTP.Post('https://vk.com/login?act=authcheck_code&hash=' + Hash + '&captcha_sid=' + CaptchaSid + '&captcha_key=' + Code, FormData);
                                end
                                else
                                  Break;
                              end
                              else
                                Break;
                            end;

                            if EndResponse.StatusCode = 302 then
                            begin
                              if GetTokenFromUrl(EndResponse.HeaderValue['Location'], Hash, Url, Code) then
                              begin
                                Token := Hash;
                              end;
                            end;
                          except
                            on E: Exception do
                              DoOnError(Self, E.Create(E.Message), ERROR_VK_PARSE, Response.DataString);
                          end;
                          FormData.Free;
                        end;
                      end;
                    end;
                  end;
                end;
                while Info.Error = 'need_captcha' do
                begin
                  Code := '';
                  FAskCaptcha(Self, Info.CaptchaImg, Code);
                  if not Code.IsEmpty then
                  begin
                    EndResponse := HTTP.Get(Url + '&captcha_sid=' + Info.CaptchaSid + '&captcha_key=' + Code, Response);
                    Info.Free;
                      {$WARNINGS OFF}
                    Info := TVkLoginInfo.FromJsonString<TVkLoginInfo>(UTF8ToWideString(Response.DataString));
                      {$WARNINGS ON}
                    Token := Info.AccessToken;
                  end
                  else
                    Break;
                end;
                if Info.Error = 'invalid_client' then
                begin
                  raise TVkParserException.Create(Info.ErrorDescription);
                end;
              end;
            finally
              Info.Free;
            end;
          except
            on E: Exception do
              DoOnError(Self, E.Create(E.Message), ERROR_VK_PARSE, Response.DataString);
          end;
        end;
      200:
        begin
          Info := TVkLoginInfo.FromJsonString<TVkLoginInfo>(Response.DataString);
          Token := Info.AccessToken;
          Info.Free;
        end;
    end;
  except
    on E: Exception do
      DoOnError(Self, E.Create(E.Message), ERROR_VK_PARSE, 'Login request error');
  end;
  Response.Free;
  HTTP.Free;
  Result := not Token.IsEmpty;
  try
    if Result and CheckAuth then
    begin
      DoLogin;
    end
    else
    begin
      Result := False;
      raise TVkAuthException.Create('Login request error');
    end;
  except
    on E: Exception do
      DoOnError(Self, E.Create(E.Message), ERROR_VK_AUTH, E.Message);
  end;
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
  AToken := Token;
  APasswordHash := ChangePasswordHash;
  ATokenExpiry := TokenExpiry;
  if AToken.IsEmpty then
  begin
    AuthUrl := GetOAuth2RequestURI;
    if Assigned(FOnAuth) then
      FOnAuth(Self, AuthUrl, AToken, ATokenExpiry, APasswordHash);
  end;

  if not AToken.IsEmpty then
  begin
    FChangePasswordHash := APasswordHash;
    Token := AToken;
    if ATokenExpiry > 0 then
      FOAuth2Authenticator.AccessTokenExpiry := IncSecond(Now, ATokenExpiry)
    else
      FOAuth2Authenticator.AccessTokenExpiry := 0;
    if CheckAuth then
    begin
      DoLogin;
      Result := True;
    end
    else
      Result := False;
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

procedure TCustomVK.SetApplication(const Value: TVkApplicationData);
begin
  AppID := Value.AppId;
  AppKey := Value.AppKey;
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
  if FLang <> TVkLang.Auto then
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

procedure TCustomVK.SetOnLog(const Value: TOnLog);
begin
  FOnLog := Value;
end;

procedure TCustomVK.SetOnLogin(const Value: TOnLogin);
begin
  FOnLogin := Value;
end;

function TCustomVK.GetApplication: TVkApplicationData;
begin
  Result.AppId := AppID;
  Result.AppKey := AppKey;
end;

function TCustomVK.GetIsWorking: Boolean;
begin
  Result := FHandler.Executing;
end;

function TCustomVK.GetTestMode: Boolean;
begin
  Result := Assigned(FHandler.Client.Params.ParameterByName('test_mode')) and (FHandler.Client.Params.ParameterByName('test_mode').Value = '1');
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
      DoError(Self, E.Create(E.Message), ERROR_INTERNAL);
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

