unit VK.Stories;

interface

uses
  System.SysUtils, System.Generics.Collections, VK.Controller, VK.Types,
  VK.Entity.Stories, VK.Entity.Stories.Sticker, VK.Entity.Stories.Stats,
  VK.Entity.Stories.Viewed;

type
  TVkParamsStoriesGet = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя, истории которого необходимо получить
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// True — возвращать в ответе дополнительную информацию о профилях пользователей
    /// </summary>
    function Extended(const Value: Boolean = False): Integer;
    /// <summary>
    /// Список дополнительных полей для объектов User и Group, которые необходимо вернуть
    /// </summary>
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
  end;

  TVkParamsStoriesGetUploadServer = record
    List: TParams;
    /// <summary>
    /// True — разместить историю в новостях
    /// </summary>
    function AddToNews(const Value: Boolean): Integer;
    /// <summary>
    /// Идентификаторы пользователей, которые будут видеть историю (для отправки в личном сообщении)
    /// </summary>
    function UserIds(const Value: TIdList): Integer;
    /// <summary>
    /// Идентификатор истории, в ответ на которую создается новая
    /// </summary>
    function ReplyToStory(const Value: string): Integer;
    /// <summary>
    /// Текст ссылки для перехода из истории (только для историй сообществ)
    /// </summary>
    function LinkText(const Value: TVkLinkText): Integer;
    /// <summary>
    /// Адрес ссылки для перехода из истории
    /// </summary>
    function LinkUrl(const Value: string): Integer;
    /// <summary>
    /// Идентификатор сообщества, в которое должна быть загружена история (при работе с ключом доступа пользователя)
    /// </summary>
    function GroupId(const Value: Integer): Integer;
    /// <summary>
    /// Объект кликабельного стикера (данные в формате JSON)
    /// </summary>
    function ClickableStickers(const Value: string): Integer; overload;
    /// <summary>
    /// Объект кликабельного стикера
    /// </summary>
    function ClickableStickers(Value: TVkStoriesStickersInfo): Integer; overload;
  end;

  TVkParamsStoriesGetReplies = record
    List: TParams;
    /// <summary>
    /// Идентификатор владельца истории
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// Идентификатор истории
    /// </summary>
    function StoryId(const Value: Integer): Integer;
    /// <summary>
    /// Ключ доступа для приватного объекта
    /// </summary>
    function AccessKey(const Value: string): Integer;
    /// <summary>
    /// True — возвращать дополнительную информацию о профилях и сообществах
    /// </summary>
    function Extended(const Value: Boolean = False): Integer;
    /// <summary>
    /// Дополнительные поля профилей и сообществ, которые необходимо вернуть в ответе
    /// </summary>
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
  end;

  TVkParamsStoriesGetViewers = record
    List: TParams;
    /// <summary>
    /// Идентификатор владельца истории
    /// </summary>
    function OwnerId(Value: Integer): Integer;
    /// <summary>
    /// Идентификатор истории
    /// </summary>
    function StoryId(Value: Integer): Integer;
    /// <summary>
    /// Максимальное число результатов в ответе
    /// </summary>
    function Count(Value: Integer = 100): Integer;
    /// <summary>
    /// Сдвиг для получения определённого подмножества результатов
    /// </summary>
    function Offset(Value: Integer = 0): Integer;
    /// <summary>
    /// True — возвращать в ответе расширенную информацию о пользователях
    /// </summary>
    function Extended(Value: Boolean = False): Integer;
  end;

  TVkParamsStoriesSearch = record
    List: TParams;
    /// <summary>
    /// Поисковый запрос
    /// </summary>
    function Query(Value: string): Integer;
    /// <summary>
    /// Идентификатор места
    /// </summary>
    function PlaceId(Value: Integer): Integer;
    /// <summary>
    /// Географическая широта точки, в радиусе которой необходимо производить поиск, заданная в градусах (от -90 до 90)
    /// </summary>
    function Latitude(Value: Extended): Integer;
    /// <summary>
    /// Географическая долгота точки, в радиусе которой необходимо производить поиск, заданная в градусах (от -180 до 180)
    /// </summary>
    function Longitude(Value: Extended): Integer;
    /// <summary>
    /// Радиус зоны поиска в метрах
    /// </summary>
    function Radius(Value: Integer): Integer;
    /// <summary>
    /// Идентификатор упомянутого в истории пользователя или сообщества
    /// </summary>
    function MentionedId(Value: Integer): Integer;
    /// <summary>
    /// Количество историй, информацию о которых необходимо вернуть
    /// </summary>
    function Count(Value: Integer = 20): Integer;
    /// <summary>
    /// Параметр, определяющий необходимость возвращать расширенную информацию о владельце истории
    /// False - возвращаются только идентификаторы
    /// True — будут дополнительно возвращены имя и фамили
    /// </summary>
    function Extended(Value: Boolean): Integer;
    /// <summary>
    /// Список дополнительных полей профилей, которые необходимо вернуть
    /// </summary>
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
  end;

  TVkParamsStoriesSendInteraction = record
    List: TParams;
    /// <summary>
    /// Ключ доступа пользователя, полученный при подписке. Возвращает событие VKWebAppSubscribeStoryApp
    /// </summary>
    function AccessKey(Value: string): Integer;
    /// <summary>
    /// Текст фидбека
    /// </summary>
    function Message(Value: string): Integer;
    /// <summary>
    /// False — фидбек виден только отправителю и автору истории;
    /// True — фидбек виден всем зрителям истории и автору
    /// </summary>
    function IsBroadcast(Value: Boolean = False): Integer;
    /// <summary>
    /// False — автор фидбека не анонимный;
    /// True — автор фидбека анонимный
    /// </summary>
    function IsAnonymous(Value: Boolean = False): Integer;
    function UnseenMarker(Value: Boolean): Integer;
  end;

  /// <summary>
  /// Stories
  /// </summary>
  TStoriesController = class(TVkController)
  public
    /// <summary>
    /// Позволяет скрыть из ленты новостей истории от выбранных источников.
    /// </summary>
    function BanOwner(const OwnersIds: TIdList): Boolean;
    /// <summary>
    /// Удаляет историю.
    /// </summary>
    function Delete(const OwnerId, StoryId: Integer): Boolean;
    /// <summary>
    /// Возвращает истории, доступные для текущего пользователя.
    /// </summary>
    function Get(var Items: TVkStoriesBlock; const Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает истории, доступные для текущего пользователя.
    /// </summary>
    function Get(var Items: TVkStoriesBlock; const Params: TVkParamsStoriesGet): Boolean; overload;
    /// <summary>
    /// Возвращает истории, доступные для текущего пользователя.
    /// </summary>
    function Get(var Items: TVkStoriesBlock; const OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Возвращает список источников историй, скрытых из ленты текущего пользователя.
    /// </summary>
    function GetBanned(var Items: TVkStoriesBanned; const Extended: Boolean = False; ProfileFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Boolean; overload;
    /// <summary>
    /// Возвращает истории, доступные для текущего пользователя.
    /// </summary>
    function GetById(var Items: TVkStoryItems; const Stories: TArrayOfString; Extended: Boolean = False; ProfileFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Boolean; overload;
    /// <summary>
    /// Позволяет получить адрес для загрузки истории с фотографией.
    /// </summary>
    function GetPhotoUploadServer(var UploadResult: string; const Params: TVkParamsStoriesGetUploadServer): Boolean;
    /// <summary>
    /// Позволяет получить ответы на историю.
    /// </summary>
    function GetReplies(var Items: TVkStoriesBlock; const Params: TVkParamsStoriesGetReplies): Boolean;
    /// <summary>
    /// Возвращает статистику истории.
    /// </summary>
    function GetStats(var Items: TVkStoryStat; const OwnerId, StoryId: Integer): Boolean;
    /// <summary>
    /// Позволяет получить адрес для загрузки видеозаписи в историю.
    /// </summary>
    function GetVideoUploadServer(var UploadResult: string; const Params: TVkParamsStoriesGetUploadServer): Boolean;
    /// <summary>
    /// Возвращает список пользователей, просмотревших историю.
    /// </summary>
    function GetViewers(var Items: TVkStoryViews; const Params: TVkParamsStoriesGetViewers): Boolean;
    /// <summary>
    /// Скрывает все ответы автора за последние сутки на истории текущего пользователя.
    /// </summary>
    function HideAllReplies(const OwnerId, GroupId: Integer): Boolean;
    /// <summary>
    /// Скрывает ответ на историю.
    /// </summary>
    function HideReply(const OwnerId, StoryId: Integer): Boolean;
    /// <summary>
    /// Сохраняет историю. В upload_results нужно передать строку, которую возвращает stories.getPhotoUploadServer или stories.getVideoUploadServer
    /// </summary>
    function Save(var Items: TVkStoryItems; const UploadResults: TArrayOfString): Boolean;
    /// <summary>
    /// Возвращает результаты поиска по историям.
    /// </summary>
    function Search(var Items: TVkStoriesBlock; const Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает результаты поиска по историям.
    /// </summary>
    function Search(var Items: TVkStoriesBlock; const Params: TVkParamsStoriesSearch): Boolean; overload;
    /// <summary>
    /// Отправляет фидбек на историю.
    /// Доступен приложениям с типом VK Mini Apps. Метод по умолчанию недоступен приложениям. Получить доступ к использованию метода можно подав заявку на размещение в каталоге приложений и подробно рассказав о механике, используемой в приложении.
    /// </summary>
    function SendInteraction(const Params: TVkParamsStoriesSendInteraction): Boolean;
    /// <summary>
    /// Позволяет вернуть пользователя или сообщество в список отображаемых историй в ленте.
    /// </summary>
    function UnbanOwner(const OwnersIds: TIdList): Boolean;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TStoriesController }

function TStoriesController.BanOwner(const OwnersIds: TIdList): Boolean;
begin
  Result := Handler.Execute('stories.banOwner', ['owners_ids', OwnersIds.ToString]).ResponseIsTrue;
end;

function TStoriesController.Delete(const OwnerId, StoryId: Integer): Boolean;
begin
  Result := Handler.Execute('stories.delete', [['owner_id', OwnerId.ToString], ['story_id', StoryId.ToString]]).ResponseIsTrue;
end;

function TStoriesController.Get(var Items: TVkStoriesBlock; const OwnerId: Integer): Boolean;
var
  Params: TVkParamsStoriesGet;
begin
  if OwnerId <> 0 then
    Params.OwnerId(OwnerId);
  Result := Get(Items, Params.List);
end;

function TStoriesController.GetBanned(var Items: TVkStoriesBanned; const Extended: Boolean; ProfileFields: TVkProfileFields; GroupFields: TVkGroupFields): Boolean;
var
  Params: TParams;
begin
  Params.Add('extended', Extended);
  if (ProfileFields <> []) or (GroupFields <> []) then
    Params.Add('fields', [ProfileFields.ToString, GroupFields.ToString]);
  Result := Handler.Execute('stories.getBanned', Params).GetObject<TVkStoriesBanned>(Items);
end;

function TStoriesController.GetById(var Items: TVkStoryItems; const Stories: TArrayOfString; Extended: Boolean; ProfileFields: TVkProfileFields; GroupFields: TVkGroupFields): Boolean;
var
  Params: TParams;
begin
  Params.Add('stories', Stories);
  Params.Add('extended', Extended);
  if (ProfileFields <> []) or (GroupFields <> []) then
    Params.Add('fields', [ProfileFields.ToString, GroupFields.ToString]);
  Result := Handler.Execute('stories.getById', Params).GetObject<TVkStoryItems>(Items);
end;

function TStoriesController.GetPhotoUploadServer(var UploadResult: string; const Params: TVkParamsStoriesGetUploadServer): Boolean;
begin
  Result := Handler.Execute('stories.getPhotoUploadServer', Params.List).GetValue('upload_result', UploadResult);
end;

function TStoriesController.GetReplies(var Items: TVkStoriesBlock; const Params: TVkParamsStoriesGetReplies): Boolean;
begin
  Result := Handler.Execute('stories.getReplies', Params.List).GetObject(Items);
end;

function TStoriesController.GetStats(var Items: TVkStoryStat; const OwnerId, StoryId: Integer): Boolean;
begin
  Result := Handler.Execute('stories.getStats', [['owner_id', OwnerId.ToString], ['story_id', StoryId.ToString]]).GetObject(Items);
end;

function TStoriesController.GetVideoUploadServer(var UploadResult: string; const Params: TVkParamsStoriesGetUploadServer): Boolean;
begin
  Result := Handler.Execute('stories.getVideoUploadServer', Params.List).GetValue('upload_result', UploadResult);
end;

function TStoriesController.GetViewers(var Items: TVkStoryViews; const Params: TVkParamsStoriesGetViewers): Boolean;
begin
  Result := Handler.Execute('stories.getViewers', Params.List).GetObject<TVkStoryViews>(Items);
end;

function TStoriesController.HideAllReplies(const OwnerId, GroupId: Integer): Boolean;
begin
  Result := Handler.Execute('stories.hideAllReplies', [['owner_id', OwnerId.ToString], ['group_id', GroupId.ToString]]).ResponseIsTrue;
end;

function TStoriesController.HideReply(const OwnerId, StoryId: Integer): Boolean;
begin
  Result := Handler.Execute('stories.hideReply', [['owner_id', OwnerId.ToString], ['story_id', StoryId.ToString]]).ResponseIsTrue;
end;

function TStoriesController.Search(var Items: TVkStoriesBlock; const Params: TVkParamsStoriesSearch): Boolean;
begin
  Result := Search(Items, Params.List);
end;

function TStoriesController.Search(var Items: TVkStoriesBlock; const Params: TParams): Boolean;
begin
  Result := Handler.Execute('stories.search', Params).GetObject<TVkStoriesBlock>(Items);
end;

function TStoriesController.Save(var Items: TVkStoryItems; const UploadResults: TArrayOfString): Boolean;
begin
  Result := Handler.Execute('stories.save', ['upload_results', UploadResults.ToString]).GetObject<TVkStoryItems>(Items);
end;

function TStoriesController.SendInteraction(const Params: TVkParamsStoriesSendInteraction): Boolean;
begin
  Result := Handler.Execute('stories.sendInteraction', Params.List).ResponseIsTrue;
end;

function TStoriesController.UnbanOwner(const OwnersIds: TIdList): Boolean;
begin
  Result := Handler.Execute('stories.unbanOwner', ['owners_ids', OwnersIds.ToString]).ResponseIsTrue;
end;

function TStoriesController.Get(var Items: TVkStoriesBlock; const Params: TVkParamsStoriesGet): Boolean;
begin
  Result := Get(Items, Params.List);
end;

function TStoriesController.Get(var Items: TVkStoriesBlock; const Params: TParams): Boolean;
begin
  Result := Handler.Execute('stories.get', Params).GetObject<TVkStoriesBlock>(Items);
end;

{ TVkParamsStoriesGet }

function TVkParamsStoriesGet.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsStoriesGet.Fields(UserFields: TVkProfileFields; GroupFields: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsStoriesGet.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

{ TVkParamsStoriesGetPhotoUploadServer }

function TVkParamsStoriesGetUploadServer.AddToNews(const Value: Boolean): Integer;
begin
  Result := List.Add('add_to_news', Value);
end;

function TVkParamsStoriesGetUploadServer.UserIds(const Value: TIdList): Integer;
begin
  Result := List.Add('user_ids', Value);
end;

function TVkParamsStoriesGetUploadServer.ReplyToStory(const Value: string): Integer;
begin
  Result := List.Add('reply_to_story', Value);
end;

function TVkParamsStoriesGetUploadServer.LinkText(const Value: TVkLinkText): Integer;
begin
  Result := List.Add('link_text', Value.ToString);
end;

function TVkParamsStoriesGetUploadServer.LinkUrl(const Value: string): Integer;
begin
  Result := List.Add('link_url', Value);
end;

function TVkParamsStoriesGetUploadServer.ClickableStickers(Value: TVkStoriesStickersInfo): Integer;
begin
  Result := List.Add('clickable_stickers', Value.ToJsonString);
end;

function TVkParamsStoriesGetUploadServer.GroupId(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsStoriesGetUploadServer.ClickableStickers(const Value: string): Integer;
begin
  Result := List.Add('clickable_stickers', Value);
end;

{ TVkParamsStoriesGetReplies }

function TVkParamsStoriesGetReplies.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsStoriesGetReplies.StoryId(const Value: Integer): Integer;
begin
  Result := List.Add('story_id', Value);
end;

function TVkParamsStoriesGetReplies.AccessKey(const Value: string): Integer;
begin
  Result := List.Add('access_key', Value);
end;

function TVkParamsStoriesGetReplies.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsStoriesGetReplies.Fields(UserFields: TVkProfileFields; GroupFields: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

{ TVkParamsStoriesGetViewers }

function TVkParamsStoriesGetViewers.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsStoriesGetViewers.StoryId(Value: Integer): Integer;
begin
  Result := List.Add('story_id', Value);
end;

function TVkParamsStoriesGetViewers.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsStoriesGetViewers.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsStoriesGetViewers.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

{ TVkParamsStoriesSearch }

function TVkParamsStoriesSearch.Query(Value: string): Integer;
begin
  Result := List.Add('q', Value);
end;

function TVkParamsStoriesSearch.PlaceId(Value: Integer): Integer;
begin
  Result := List.Add('place_id', Value);
end;

function TVkParamsStoriesSearch.Latitude(Value: Extended): Integer;
begin
  Result := List.Add('latitude', Value);
end;

function TVkParamsStoriesSearch.Longitude(Value: Extended): Integer;
begin
  Result := List.Add('longitude', Value);
end;

function TVkParamsStoriesSearch.Radius(Value: Integer): Integer;
begin
  Result := List.Add('radius', Value);
end;

function TVkParamsStoriesSearch.MentionedId(Value: Integer): Integer;
begin
  Result := List.Add('mentioned_id', Value);
end;

function TVkParamsStoriesSearch.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsStoriesSearch.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsStoriesSearch.Fields(UserFields: TVkProfileFields; GroupFields: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

{ TVkParamsStoriesSendInteraction }

function TVkParamsStoriesSendInteraction.AccessKey(Value: string): Integer;
begin
  Result := List.Add('access_key', Value);
end;

function TVkParamsStoriesSendInteraction.Message(Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsStoriesSendInteraction.IsBroadcast(Value: Boolean): Integer;
begin
  Result := List.Add('is_broadcast', Value);
end;

function TVkParamsStoriesSendInteraction.IsAnonymous(Value: Boolean): Integer;
begin
  Result := List.Add('is_anonymous', Value);
end;

function TVkParamsStoriesSendInteraction.UnseenMarker(Value: Boolean): Integer;
begin
  Result := List.Add('unseen_marker', Value);
end;

end.

