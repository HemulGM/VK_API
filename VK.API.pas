﻿unit VK.API;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.JSON,
  System.Generics.Collections, REST.Authenticator.OAuth, VK.Types, VK.Account,
  VK.Handler, VK.Auth, VK.Users, VK.Messages, VK.Status, VK.Wall, VK.Docs,
  VK.Audio, VK.Likes, VK.Board, VK.Friends, VK.Groups, VK.Photos, VK.Catalog,
  VK.Market, VK.Fave, VK.Notes, VK.Utils, VK.Video, VK.Gifts, VK.Newsfeed,
  VK.Notifications, VK.Orders, Vk.Pages, VK.Polls, VK.Podcasts, VK.Search,
  VK.Database, VK.Storage, VK.DownloadedGames, VK.Secure, VK.Stats, VK.Stories,
  VK.Apps, VK.Clients, VK.Donut, VK.Streaming, VK.Ads, VK.Asr, System.Sensors;

type
  TOnNeedGeoLocation = procedure(Sender: TObject; var Coord: TLocationCoord2D) of object;

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
    FUserName: string;
    FUserPhoto50: string;
    FUserSex: TVkSex;
    FUserPhoto100: string;
    FUsers: TUsersController;
    FUseServiceKeyOnly: Boolean;
    FUtils: TUtilsController;
    FVideo: TVideoController;
    FWall: TWallController;
    FStreaming: TStreamingController;
    FAds: TAds;
    FAsr: TAsr;
    FOnNeedGeoLocation: TOnNeedGeoLocation;
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
    procedure SetLogResponse(const Value: Boolean);
    function GetUserId: Integer;
    procedure SetApplication(const Value: TVkApplicationData);
    function GetApplication: TVkApplicationData;
    procedure SetRequestLimit(const Value: Integer);
    function GetRequestLimit: Integer;
    function GetUserName: string;
    function GetUserPhoto100: string;
    function GetUserPhoto50: string;
    function GetUserSex: TVkSex;
    function GetGeoLocation: TLocationCoord2D;
    procedure SetOnNeedGeoLocation(const Value: TOnNeedGeoLocation);
    procedure FOnAuthNeed(Sender: TObject; var AResult: Boolean);
  public
    constructor Create(const AToken: string); reintroduce; overload;
    constructor Create(AOwner: TComponent); overload; override;
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
    /// Очищает токен
    /// </summary>
    procedure Logout;
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
    function Upload(const UploadUrl: string; FileNames: TArray<string>; var Response: string): Boolean; overload;
    function Upload(const UploadUrl: string; const Files: TArray<TPair<string, TStream>>; var Response: string): Boolean; overload;
    //Группы методов
    /// <summary>
    /// Методы для работы с аккаунтом.
    /// </summary>
    property Account: TAccountController read FAccount;
    /// <summary>
    /// Методы для работы с аккаунтом.
    /// </summary>
    property Ads: TAds read FAds;
    /// <summary>
    /// Методы для работы с приложениями.
    /// </summary>
    property Apps: TAppsController read FApps;
    /// <summary>
    /// Метод выполняет распознавание речи из загруженного файла аудиозаписи.
    /// </summary>
    property Asr: TAsr read FAsr;
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
    /// Имя пользователя (будет запрошено, если не сохранено)
    /// </summary>
    property UserName: string read GetUserName;
    /// <summary>
    /// Фото пользователя (будет запрошено, если не сохранено)
    /// </summary>
    property UserPhoto50: string read GetUserPhoto50;
    /// <summary>
    /// Фото пользователя (будет запрошено, если не сохранено)
    /// </summary>
    property UserPhoto100: string read GetUserPhoto100;
    /// <summary>
    /// Пол пользователя (будет запрошено, если не сохранено)
    /// </summary>
    property UserSex: TVkSex read GetUserSex;
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
    /// Данные клиента AppId (client_id) + AppKey (client_secret)
    /// </summary>
    property Application: TVkApplicationData read GetApplication write SetApplication;
    /// <summary>
    /// Кол-во запросов в секунду
    /// </summary>
    property RequestLimit: Integer read GetRequestLimit write SetRequestLimit;
    /// <summary>
    /// Быстрый метод загрузки файла на диск
    /// </summary>
    function DownloadFile(const Url, FileName: string): Boolean;
    property GeoLocation: TLocationCoord2D read GetGeoLocation;
    property OnNeedGeoLocation: TOnNeedGeoLocation read FOnNeedGeoLocation write SetOnNeedGeoLocation;
  end;

  VKAPI = class(TCustomVK);

const
  ERROR_INTERNAL = -1;

implementation

uses
  System.DateUtils, System.Net.Mime, System.Net.HttpClient, VK.CommonUtils,
  VK.Entity.Profile, VK.Entity.Login, IdMultipartFormData;

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
    begin
      CallMethod(MethodName, Params, Callback);
    end).Start;
end;

function TCustomVK.Upload(const UploadUrl: string; FileNames: TArray<string>; var Response: string): Boolean;
var
  HTTP: THTTPClient;
  Data: TIdMultiPartFormDataStream;
  ResStream: TStringStream;
  JSON: TJSONValue;
  FileName: string;
begin
  Result := False;
  Response := '';
  Data := TIdMultiPartFormDataStream.Create;
  HTTP := THTTPClient.Create;
  ResStream := TStringStream.Create;
  try
    for FileName in FileNames do
      if not FileName.IsEmpty then
        Data.AddFile('file', FileName);
    if HTTP.Post(UploadUrl, Data, ResStream).StatusCode = 200 then
    begin
      try
        JSON := TJSONObject.ParseJSONValue(ResStream.DataString);
        if Assigned(JSON) then
        try
          Response := JSON.GetValue('file', '');
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

function TCustomVK.Upload(const UploadUrl: string; const Files: TArray<TPair<string, TStream>>; var Response: string): Boolean;
var
  HTTP: THTTPClient;
  Data: TIdMultiPartFormDataStream;
  ResStream: TStringStream;
  JSON: TJSONValue;
  FileItem: TPair<string, TStream>;
begin
  Result := False;
  Response := '';
  Data := TIdMultiPartFormDataStream.Create;
  HTTP := THTTPClient.Create;
  ResStream := TStringStream.Create;
  try
    for FileItem in Files do
      if FileItem.Value.Size > 0 then
        Data.AddFormField('file', '', '', FileItem.Value, FileItem.Key);
    if HTTP.Post(UploadUrl, Data, ResStream).StatusCode = 200 then
    begin
      try
        JSON := TJSONObject.ParseJSONValue(ResStream.DataString);
        if Assigned(JSON) then
        try
          Response := JSON.GetValue('file', '');
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
  FHandler.OnLog := FLog;
  FHandler.Authenticator := FOAuth2Authenticator;
  FHandler.OnCaptcha := FAskCaptcha;
  FHandler.OnConfirm := FOnConfirm;
  FHandler.OnAuth := FOnAuthNeed;
  //Defaults
  EndPoint := 'https://oauth.vk.com/authorize';
  BaseURL := 'https://api.vk.com/method';
  SetAPIVersion(Version);
  Permissions := [TVkPermission.Groups, TVkPermission.Friends, TVkPermission.Wall, TVkPermission.Photos, TVkPermission.Video, TVkPermission.Docs, TVkPermission.Notes, TVkPermission.Market];
  //Controllers
  FAccount := TAccountController.Create(FHandler);
  FAds := TAds.Create(FHandler);
  FAsr := TAsr.Create(FHandler);
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
  FAds.Free;
  FAsr.Free;
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
  try
    FOnError(Sender, E, Code, Text);
  finally
    if Assigned(E) then
      E.Free;
  end;
end;

function TCustomVK.DownloadFile(const Url, FileName: string): Boolean;
var
  HTTP: THTTPClient;
  Stream: TFileStream;
begin
  HTTP := THTTPClient.Create;
  try
    HTTP.HandleRedirects := True;
    Stream := TFileStream.Create(FileName, fmCreate);
    try
      Result := HTTP.Get(Url, Stream).StatusCode = 200;
    finally
      Stream.Free;
    end;
  finally
    HTTP.Free;
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

procedure TCustomVK.FOnAuthNeed(Sender: TObject; var AResult: Boolean);
begin
  Token := '';
  TokenExpiry := 0;
  AResult := Login;
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
  Result := Users.Get(User, [TVkExtendedField.Photo50, TVkExtendedField.Photo100, TVkExtendedField.Sex]);
  if Result then
  try
    FUserId := User.Id;
    FUserName := User.FullName;
    FUserPhoto50 := User.Photo50;
    FUserPhoto100 := User.Photo100;
    FUserSex := User.Sex;
  finally
    User.Free;
  end;
end;

function TCustomVK.CheckAuth: Boolean;
var
  MT: Int64;
begin
  Result := Utils.GetServerTimeUnix(MT);
end;

constructor TCustomVK.Create(const AToken: string);
begin
  with Create(nil) do
    Token := AToken;
end;

procedure TCustomVK.Logout;
begin
  Token := '';
  TokenExpiry := 0;
  FUserId := 0;
  FUserName := '';
  FUserSex := TVkSex.None;
  FUserPhoto50 := '';
  FUserPhoto100 := '';
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

function TCustomVK.GetRequestLimit: Integer;
begin
  Result := FHandler.RequestLimit;
end;

function TCustomVK.Login: Boolean;
var
  AToken, APasswordHash: string;
  ATokenExpiry: Int64;
begin
  Result := False;
  AToken := Token;
  ATokenExpiry := TokenExpiry;
  APasswordHash := ChangePasswordHash;
  if AToken.IsEmpty and Assigned(FOnAuth) then
  begin
    FOnAuth(Self, GetOAuth2RequestURI, AToken, ATokenExpiry, APasswordHash);
    Token := AToken;
    TokenExpiry := ATokenExpiry;
    FChangePasswordHash := APasswordHash;
  end;

  if not AToken.IsEmpty then
  begin
    if CheckAuth then
    begin
      DoLogin;
      Result := True;
    end
    else
    begin
      Token := '';
      TokenExpiry := 0;
      FChangePasswordHash := '';
      Result := False;
    end;
  end;
end;

procedure TCustomVK.SetAPIVersion(const Value: string);
begin
  FAPIVersion := Value;
  FHandler.AddParameter('v', FAPIVersion);
end;

procedure TCustomVK.SetServiceKey(const Value: string);
begin
  FServiceKey := Value;
  if csDesigning in ComponentState then
    Exit;

  if FUseServiceKeyOnly then
  begin
    FHandler.AddParameter('access_token', FServiceKey);
  end;
end;

procedure TCustomVK.SetTestMode(const Value: Boolean);
begin
  FHandler.AddParameter('test_mode', BoolToString(Value));
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
    FHandler.AddParameter('lang', Ord(Value).ToString)
  else
    FHandler.DeleteParameter('lang');
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
  FHandler.BaseURL := FBaseURL;
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

procedure TCustomVK.SetOnNeedGeoLocation(const Value: TOnNeedGeoLocation);
begin
  FOnNeedGeoLocation := Value;
end;

function TCustomVK.GetApplication: TVkApplicationData;
begin
  Result.AppId := AppID;
  Result.AppKey := AppKey;
end;

function TCustomVK.GetGeoLocation: TLocationCoord2D;
begin
  Result := TLocationCoord2D.Create(-1, -1);
  if Assigned(FOnNeedGeoLocation) then
    FOnNeedGeoLocation(Self, Result);
end;

function TCustomVK.GetIsWorking: Boolean;
begin
  Result := FHandler.Executing;
end;

function TCustomVK.GetTestMode: Boolean;
begin
  Result := FHandler.Parameter('test_mode') = '1';
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

function TCustomVK.GetUserId: Integer;
begin
  if (FUserId > 0) or LoadUserInfo then
    Result := FUserId
  else
    Result := -1;
end;

function TCustomVK.GetUserName: string;
begin
  if (FUserId > 0) or LoadUserInfo then
    Result := FUserName
  else
    Result := '';
end;

function TCustomVK.GetUserPhoto100: string;
begin
  if (FUserId > 0) or LoadUserInfo then
    Result := FUserPhoto100
  else
    Result := '';
end;

function TCustomVK.GetUserPhoto50: string;
begin
  if (FUserId > 0) or LoadUserInfo then
    Result := FUserPhoto50
  else
    Result := '';
end;

function TCustomVK.GetUserSex: TVkSex;
begin
  if (FUserId > 0) or LoadUserInfo then
    Result := FUserSex
  else
    Result := TVkSex.None;
end;

procedure TCustomVK.SetPermissions(const Value: TVkPermissions);
begin
  FPermissions := Value;
end;

procedure TCustomVK.SetRequestLimit(const Value: Integer);
begin
  FHandler.RequestLimit := Value;
end;

procedure TCustomVK.SetUseServiceKeyOnly(const Value: Boolean);
begin
  FUseServiceKeyOnly := Value;
  if csDesigning in ComponentState then
    Exit;

  FHandler.UseServiceKeyOnly := Value;
  if FUseServiceKeyOnly then
  begin
    FHandler.Authenticator := nil;
    FHandler.AddParameter('access_token', FServiceKey);
  end
  else
  begin
    FHandler.Authenticator := FOAuth2Authenticator;
    FHandler.DeleteParameter('access_token');
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
  Result := FOwner.Handler.ProxyServer;
end;

function TCustomVK.TVkProxy.GetPassword: string;
begin
  Result := FOwner.Handler.ProxyPassword;
end;

function TCustomVK.TVkProxy.GetPort: Integer;
begin
  Result := FOwner.Handler.ProxyPort;
end;

function TCustomVK.TVkProxy.GetUserName: string;
begin
  Result := FOwner.Handler.ProxyUsername;
end;

procedure TCustomVK.TVkProxy.SetIP(const Value: string);
begin
  FOwner.Handler.ProxyServer := Value;
end;

procedure TCustomVK.TVkProxy.SetPassword(const Value: string);
begin
  FOwner.Handler.ProxyPassword := Value;
end;

procedure TCustomVK.TVkProxy.SetPort(const Value: Integer);
begin
  FOwner.Handler.ProxyPort := Value;
end;

procedure TCustomVK.TVkProxy.SetUserName(const Value: string);
begin
  FOwner.Handler.ProxyUsername := Value;
end;

procedure TCustomVK.TVkProxy.SetProxy(AIP: string; APort: Integer; AUserName, APassword: string);
begin
  IP := AIP;
  Port := APort;
  Username := AUserName;
  Password := APassword;
end;

end.

