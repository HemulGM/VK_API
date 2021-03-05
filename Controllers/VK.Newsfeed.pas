unit VK.Newsfeed;

interface

uses
  System.SysUtils, VK.Controller, VK.Types, VK.Entity.Newsfeed, VK.Entity.Media;

type
  TVkParamsNewsfeedBanned = record
  private
    function Extended(const Value: Boolean): Integer;
  public
    List: TParams;
    /// <summary>
    /// список дополнительных полей профилей, которые необходимо вернуть
    /// </summary>
    function Fields(const GroupFields: TVkGroupFields = []; UserFields: TVkProfileFields = []): Integer;
    /// <summary>
    /// падеж для склонения имени и фамилии пользователя
    /// </summary>
    function NameCase(const Value: TVkNameCase): Integer;
  end;

  TVkParamsNewsfeedGet = record
    List: TParams;
    /// <summary>
    /// Перечисленные через запятую названия списков новостей, которые необходимо получить
    /// Если параметр не задан, то будут получены все возможные списки новостей
    /// </summary>
    function Filters(const Value: TVkNewsfeedTypes = []): Integer;
    /// <summary>
    /// True - включить в выдачу также скрытых из новостей пользователей. False - не возвращать скрытых пользователей
    /// </summary>
    function ReturnBanned(const Value: Boolean): Integer;
    /// <summary>
    /// Время, начиная с которого следует получить новости для текущего пользователя
    /// </summary>
    function StartTime(const Value: TDateTime): Integer;
    /// <summary>
    /// Время, до которого следует получить новости для текущего пользователя.
    /// Если параметр не задан, то он считается равным текущему времени
    /// </summary>
    function EndTime(const Value: TDateTime): Integer;
    /// <summary>
    /// Максимальное количество фотографий, информацию о которых необходимо вернуть (максимальное значение: 100)
    /// </summary>
    function MaxPhotos(const Value: Integer = 5): Integer;
    /// <summary>
    /// Перечисленные через запятую иcточники новостей, новости от которых необходимо получить
    /// [uid] или u[uid], -[gid] или g[gid]
    /// Помимо этого параметр может принимать строковые значения:
    /// friends - список друзей пользователя
    /// groups - список групп, на которые подписан текущий пользователь
    /// pages - список публичных страниц, на который подписан тeкущий пользователь
    /// following - список пользователей, на которых подписан текущий пользователь
    /// list[идентификатор списка новостей] - список новостей. Вы можете найти все списки новостей пользователя используя метод newsfeed.getLists
    /// </summary>
    function SourceIds(const Value: TArrayOfString): Integer;
    /// <summary>
    /// Идентификатор, необходимый для получения следующей страницы результатов. Значение, необходимое для передачи в этом параметре, возвращается в поле ответа next_from
    /// </summary>
    function StartFrom(const Value: string): Integer;
    /// <summary>
    /// Указывает, какое максимальное число новостей следует возвращать, но не более 100
    /// </summary>
    function Count(const Value: Integer = 50): Integer;
    /// <summary>
    /// Список дополнительных полей для профилей и групп, которые необходимо вернуть
    /// </summary>
    function Fields(const GroupFields: TVkGroupFields = []; UserFields: TVkProfileFields = []): Integer;
    /// <summary>
    /// [Нет описания]
    /// </summary>
    function Section(const Value: string): Integer;
  end;

  TVkParamsNewsfeedGetComments = record
    List: TParams;
    /// <summary>
    /// Перечисленные через запятую типы объектов, изменения комментариев к которым нужно вернуть
    /// </summary>
    function Filters(const Value: TVkNewsfeedCommentsTypes = []): Integer;
    /// <summary>
    /// Список дополнительных полей профилей, которые необходимо вернуть
    /// </summary>
    function Fields(const Value: TVkProfileFields = []): Integer;
    /// <summary>
    /// Указывает, какое максимальное число новостей следует возвращать, но не более 100.
    /// Для автоподгрузки Вы можете использовать возвращаемый данным методом параметр NewOffset.
    /// </summary>
    function Count(const Value: Integer = 30): Integer;
    /// <summary>
    /// Количество комментариев к записям, которые нужно получить.
    /// Положительное число, доступен начиная с версии 5.23, максимальное значение 10
    /// </summary>
    function LastCommentsCount(const Value: Integer = 0): Integer;
    /// <summary>
    /// Время, до которого следует получить новости для текущего пользователя.
    /// Если параметр не задан, то он считается равным текущему времени
    /// </summary>
    function StartTime(const Value: TDateTime): Integer;
    /// <summary>
    /// Время, начиная с которого следует получить новости для текущего пользователя.
    /// Если параметр не задан, то он считается равным значению времени, которое было сутки назад
    /// </summary>
    function EndTime(const Value: TDateTime): Integer;
    /// <summary>
    /// Идентификатор, необходимый для получения следующей страницы результатов.
    /// Значение, необходимое для передачи в этом параметре, возвращается в поле ответа NextFrom
    /// </summary>
    function StartFrom(const Value: string): Integer;
    /// <summary>
    /// Идентификатор объекта, комментарии к репостам которого необходимо вернуть,
    /// например wall1_45486. Если указан данный параметр, параметр Filters указывать необязательно
    /// </summary>
    function Reposts(const Value: string): Integer;
  end;

  TVkParamsNewsfeedGetMentions = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// Количество возвращаемых записей. Максимальное значение параметра 50
    /// </summary>
    function Count(const Value: Integer = 20): Integer;
    /// <summary>
    /// Смещение, необходимое для выборки определенного подмножества новостей
    /// </summary>
    function Offset(const Value: Integer = 0): Integer;
    /// <summary>
    /// Время, начиная с которого следует получать упоминания о пользователе.
    /// Если параметр не задан, то будут возвращены все упоминания о пользователе,
    /// если не задан параметр EndTime, в противном случае упоминания с учетом параметра EndTime
    /// </summary>
    function StartTime(const Value: TDateTime): Integer;
    /// <summary>
    /// Время, в формате unixtime, до которого следует получать упоминания о пользователе.
    /// Если параметр не задан, то будут возвращены все упоминания о пользователе,
    /// если не задан параметр StartTime, в противном случае упоминания с учетом параметра StartTime
    /// </summary>
    function EndTime(const Value: TDateTime): Integer;
  end;

  TVkParamsNewsfeedGetRecommended = record
    List: TParams;
    /// <summary>
    /// Список дополнительных полей профилей, которые необходимо вернуть
    /// </summary>
    function Fields(const Value: TVkProfileFields = []): Integer;
    /// <summary>
    /// Указывает, какое максимальное число новостей следует возвращать, но не более 100
    /// </summary>
    function Count(const Value: Integer = 50): Integer;
    /// <summary>
    /// Время, начиная с которого следует получить новости для текущего пользователя.
    /// Если параметр не задан, то он считается равным значению времени, которое было сутки назад
    /// </summary>
    function StartTime(const Value: TDateTime): Integer;
    /// <summary>
    /// Время, до которого следует получить новости для текущего пользователя.
    /// Если параметр не задан, то он считается равным текущему времени
    /// </summary>
    function EndTime(const Value: TDateTime): Integer;
    /// <summary>
    /// Идентификатор, необходимый для получения следующей страницы результатов.
    /// Значение, необходимое для передачи в этом параметре, возвращается в поле ответа NextFrom
    /// </summary>
    function StartFrom(const Value: string): Integer;
    /// <summary>
    /// Максимальное количество фотографий, информацию о которых необходимо вернуть
    /// </summary>
    function MaxPhotos(const Value: Integer = 5): Integer;
  end;

  TVkParamsNewsfeedSearch = record
    List: TParams;
    /// <summary>
    /// Поисковой запрос
    /// </summary>
    function Query(const Value: string): Integer;
    /// <summary>
    /// Список дополнительных полей для профилей и групп, которые необходимо вернуть
    /// </summary>
    function Fields(const GroupFields: TVkGroupFields = []; UserFields: TVkProfileFields = []): Integer;
    /// <summary>
    /// Указывает, какое максимальное число записей следует возвращать.
    /// Обратите внимание — даже при использовании параметра Offset для получения информации
    /// доступны только первые 1000 результатов (максимальное значение 200)
    /// </summary>
    function Count(const Value: Integer = 30): Integer;
    /// <summary>
    /// Время, начиная с которого следует получить новости для текущего пользователя.
    /// Если параметр не задан, то он считается равным значению времени, которое было сутки назад
    /// </summary>
    function StartTime(const Value: TDateTime): Integer;
    /// <summary>
    /// Время, до которого следует получить новости для текущего пользователя.
    /// Если параметр не задан, то он считается равным текущему времени
    /// </summary>
    function EndTime(const Value: TDateTime): Integer;
    /// <summary>
    /// Идентификатор, необходимый для получения следующей страницы результатов.
    /// Значение, необходимое для передачи в этом параметре, возвращается в поле ответа NextFrom
    /// </summary>
    function StartFrom(const Value: string): Integer;
    /// <summary>
    /// Географическая широта точки, в радиусе от которой необходимо производить поиск, заданная в градусах (от -90 до 90)
    /// </summary>
    function Latitude(const Value: Extended): Integer;
    /// <summary>
    /// Географическая долгота точки, в радиусе от которой необходимо производить поиск, заданная в градусах (от -180 до 180)
    /// </summary>
    function Longitude(const Value: Extended): Integer;
    /// <summary>
    /// True, если необходимо получить информацию о пользователе или сообществе, разместившем запись
    /// </summary>
    function Extended(const Value: Boolean = False): Integer;
  end;

  TNewsfeedController = class(TVkController)
  public
    /// <summary>
    /// Запрещает показывать новости от заданных пользователей и групп в ленте новостей текущего пользователя.
    /// </summary>
    function AddBan(UserIds: TIdList = []; GroupIds: TIdList = []): Boolean;
    /// <summary>
    /// Разрешает показывать новости от заданных пользователей и групп в ленте новостей текущего пользователя.
    /// </summary>
    function DeleteBan(UserIds: TIdList = []; GroupIds: TIdList = []): Boolean;
    /// <summary>
    /// Метод позволяет удалить пользовательский список новостей.
    /// </summary>
    function DeleteList(ListId: Integer): Boolean;
    /// <summary>
    /// Возвращает данные, необходимые для показа списка новостей для текущего пользователя.
    /// </summary>
    function Get(var Items: TVkNews; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает данные, необходимые для показа списка новостей для текущего пользователя.
    /// </summary>
    function Get(var Items: TVkNews; Params: TVkParamsNewsfeedGet): Boolean; overload;
    /// <summary>
    /// Возвращает список пользователей и групп, которые текущий пользователь скрыл из ленты новостей.
    /// </summary>
    function GetBanned(var Items: TVkNewsfeedBannedIds; Params: TVkParamsNewsfeedBanned): Boolean; overload;
    /// <summary>
    /// Возвращает список пользователей и групп, которые текущий пользователь скрыл из ленты новостей.
    /// </summary>
    function GetBanned(var Items: TVkNewsfeedBanned; Params: TVkParamsNewsfeedBanned): Boolean; overload;
    /// <summary>
    /// Возвращает данные, необходимые для показа раздела комментариев в новостях пользователя.
    /// </summary>
    function GetComments(var Items: TVkNews; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает данные, необходимые для показа раздела комментариев в новостях пользователя.
    /// </summary>
    function GetComments(var Items: TVkNews; Params: TVkParamsNewsfeedGetComments): Boolean; overload;
    /// <summary>
    /// Возвращает пользовательские списки новостей.
    /// </summary>
    function GetLists(var Items: TVkNewsfeedLists; ListIds: TIdList; Extended: Boolean = False): Boolean; overload;
    /// <summary>
    /// Возвращает список записей пользователей на своих стенах, в которых упоминается указанный пользователь.
    /// </summary>
    function GetMentions(var Items: TVkPosts; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список записей пользователей на своих стенах, в которых упоминается указанный пользователь.
    /// </summary>
    function GetMentions(var Items: TVkPosts; Params: TVkParamsNewsfeedGetMentions): Boolean; overload;
    /// <summary>
    /// Получает список новостей, рекомендованных пользователю.
    /// </summary>
    function GetRecommended(var Items: TVkNews; Params: TVkParamsNewsfeedGetRecommended): Boolean;
    /// <summary>
    /// Возвращает сообщества и пользователей, на которые текущему пользователю рекомендуется подписаться.
    /// </summary>
    function GetSuggestedSources(var Items: TVkSuggestedList; Shuffle: Boolean = False; Count: Integer = 20; Offset: Integer = 0): Boolean;
    /// <summary>
    /// Позволяет скрыть объект из ленты новостей.
    /// </summary>
    function IgnoreItem(ItemType: TVkNewsfeedIgnoreType; OwnerId, ItemId: Integer): Boolean;
    /// <summary>
    /// Метод позволяет создавать или редактировать пользовательские списки для просмотра новостей.
    /// </summary>
    function SaveList(var ListId: Integer; Title: string; SourceIds: TIdList; NoReposts: Boolean = False): Boolean; overload;
    /// <summary>
    /// Метод позволяет создавать или редактировать пользовательские списки для просмотра новостей.
    /// </summary>
    function SaveList(const ListId: Integer; SourceIds: TIdList; NoReposts: Boolean = False): Boolean; overload;
    /// <summary>
    /// Возвращает результаты поиска по статусам. Новости возвращаются в порядке от более новых к более старым.
    /// </summary>
    function Search(var Items: TVkNews; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает результаты поиска по статусам. Новости возвращаются в порядке от более новых к более старым.
    /// </summary>
    function Search(var Items: TVkNews; Params: TVkParamsNewsfeedSearch): Boolean; overload;
    /// <summary>
    /// Позволяет вернуть ранее скрытый объект в ленту новостей.
    /// </summary>
    function UnignoreItem(ItemType: TVkNewsfeedIgnoreType; OwnerId, ItemId: Integer; TrackCode: string = ''): Boolean;
    /// <summary>
    /// Отписывает текущего пользователя от комментариев к заданному объекту.
    /// </summary>
    function Unsubscribe(ItemType: TVkNewsfeedCommentsType; OwnerId, ItemId: Integer): Boolean;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TNewsfeedController }

function TNewsfeedController.AddBan(UserIds, GroupIds: TIdList): Boolean;
var
  Params: TParams;
begin
  if Length(UserIds) > 0 then
    Params.Add('user_ids', UserIds);
  if Length(GroupIds) > 0 then
    Params.Add('group_ids', GroupIds);
  Result := Handler.Execute('newsfeed.addBan', Params).ResponseIsTrue;
end;

function TNewsfeedController.DeleteBan(UserIds, GroupIds: TIdList): Boolean;
var
  Params: TParams;
begin
  if Length(UserIds) > 0 then
    Params.Add('user_ids', UserIds);
  if Length(GroupIds) > 0 then
    Params.Add('group_ids', GroupIds);
  Result := Handler.Execute('newsfeed.deleteBan', Params).ResponseIsTrue;
end;

function TNewsfeedController.DeleteList(ListId: Integer): Boolean;
begin
  Result := Handler.Execute('newsfeed.deleteList', ['list_id', ListId.ToString]).ResponseIsTrue;
end;

function TNewsfeedController.Get(var Items: TVkNews; Params: TVkParamsNewsfeedGet): Boolean;
begin
  Result := Get(Items, Params.List);
end;

function TNewsfeedController.GetBanned(var Items: TVkNewsfeedBanned; Params: TVkParamsNewsfeedBanned): Boolean;
begin
  Params.Extended(True);
  Result := Handler.Execute('newsfeed.getBanned', Params.List).GetObject(Items);
end;

function TNewsfeedController.GetComments(var Items: TVkNews; Params: TVkParamsNewsfeedGetComments): Boolean;
begin
  Result := GetComments(Items, Params.List);
end;

function TNewsfeedController.GetLists(var Items: TVkNewsfeedLists; ListIds: TIdList; Extended: Boolean): Boolean;
begin
  Result := Handler.Execute('newsfeed.getLists', [
    ['list_ids', ListIds.ToString],
    ['extended', BoolToString(Extended)]]).
    GetObject(Items);
end;

function TNewsfeedController.GetMentions(var Items: TVkPosts; Params: TVkParamsNewsfeedGetMentions): Boolean;
begin
  Result := GetMentions(Items, Params.List);
end;

function TNewsfeedController.GetRecommended(var Items: TVkNews; Params: TVkParamsNewsfeedGetRecommended): Boolean;
begin
  Result := Handler.Execute('newsfeed.getRecommended', Params.List).GetObject(Items);
end;

function TNewsfeedController.GetSuggestedSources(var Items: TVkSuggestedList; Shuffle: Boolean; Count, Offset: Integer): Boolean;
begin
  Result := Handler.Execute('newsfeed.getSuggestedSources', [
    ['shuffle', BoolToString(Shuffle)],
    ['count', Count.ToString],
    ['offset', Offset.ToString]]).
    GetObject(Items);
end;

function TNewsfeedController.IgnoreItem(ItemType: TVkNewsfeedIgnoreType; OwnerId, ItemId: Integer): Boolean;
begin
  Result := Handler.Execute('newsfeed.ignoreItem', [
    ['type', ItemType.ToString],
    ['owner_id', OwnerId.ToString],
    ['item_id', ItemId.ToString]]).
    ResponseIsTrue;
end;

function TNewsfeedController.SaveList(const ListId: Integer; SourceIds: TIdList; NoReposts: Boolean): Boolean;
var
  Items: TVkNewsfeedLists;
  Id: Integer;
begin
  {TODO -oHemulGM -cGeneral : Тут используется костыль, который запрашивает название листа, т.к. в ВК почему-то оно требуется даже для просто добавления новых источников}
  Result := GetLists(Items, [ListId], False);
  if Result then
  begin
    try
      try
        Result := (Length(Items.Items) > 0) and SaveList(Id, Items.Items[0].Title, SourceIds, NoReposts);
      finally
        Items.Free;
      end;
    except
      Result := False;
    end;
  end;
end;

function TNewsfeedController.Search(var Items: TVkNews; Params: TVkParamsNewsfeedSearch): Boolean;
begin
  Result := Search(Items, Params.List);
end;

function TNewsfeedController.UnignoreItem(ItemType: TVkNewsfeedIgnoreType; OwnerId, ItemId: Integer; TrackCode: string): Boolean;
var
  Params: TParams;
begin
  Params.Add('type', ItemType.ToString);
  Params.Add('owner_id', OwnerId);
  Params.Add('item_id', ItemId);
  if not TrackCode.IsEmpty then
    Params.Add('track_code', TrackCode);
  Result := Handler.Execute('newsfeed.unignoreItem', Params).ResponseIsTrue;
end;

function TNewsfeedController.Unsubscribe(ItemType: TVkNewsfeedCommentsType; OwnerId, ItemId: Integer): Boolean;
begin
  Result := Handler.Execute('newsfeed.unsubscribe', [
    ['type', ItemType.ToString],
    ['owner_id', OwnerId.ToString],
    ['item_id', ItemId.ToString]]).
    ResponseIsTrue;
end;

function TNewsfeedController.Search(var Items: TVkNews; Params: TParams): Boolean;
begin
  Result := Handler.Execute('newsfeed.search', Params).GetObject(Items);
end;

function TNewsfeedController.SaveList(var ListId: Integer; Title: string; SourceIds: TIdList; NoReposts: Boolean): Boolean;
var
  Params: TParams;
begin
  if ListId >= 0 then
    Params.Add('list_id', ListId);
  Params.Add('Title', Title);
  if Length(SourceIds) > 0 then
    Params.Add('source_ids', SourceIds);
  Params.Add('no_reposts', NoReposts);
  Result := Handler.Execute('newsfeed.saveList', Params).ResponseAsInt(ListId);
end;

function TNewsfeedController.GetMentions(var Items: TVkPosts; Params: TParams): Boolean;
begin
  Result := Handler.Execute('newsfeed.getMentions', Params).GetObject(Items);
end;

function TNewsfeedController.GetComments(var Items: TVkNews; Params: TParams): Boolean;
begin
  Result := Handler.Execute('newsfeed.getComments', Params).GetObject(Items);
end;

function TNewsfeedController.GetBanned(var Items: TVkNewsfeedBannedIds; Params: TVkParamsNewsfeedBanned): Boolean;
begin
  Params.Extended(False);
  Result := Handler.Execute('newsfeed.getBanned', Params.List).GetObject(Items);
end;

function TNewsfeedController.Get(var Items: TVkNews; Params: TParams): Boolean;
begin
  Result := Handler.Execute('newsfeed.get', Params).GetObject(Items);
end;

{ TVkParamsNewsfeedGet }

function TVkParamsNewsfeedGet.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsNewsfeedGet.EndTime(const Value: TDateTime): Integer;
begin
  Result := List.Add('end_time', Value);
end;

function TVkParamsNewsfeedGet.Fields(const GroupFields: TVkGroupFields; UserFields: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsNewsfeedGet.Filters(const Value: TVkNewsfeedTypes): Integer;
begin
  Result := List.Add('filters', Value.ToString);
end;

function TVkParamsNewsfeedGet.MaxPhotos(const Value: Integer): Integer;
begin
  Result := List.Add('max_photos', Value);
end;

function TVkParamsNewsfeedGet.ReturnBanned(const Value: Boolean): Integer;
begin
  Result := List.Add('return_banned', Value);
end;

function TVkParamsNewsfeedGet.Section(const Value: string): Integer;
begin
  Result := List.Add('section', Value);
end;

function TVkParamsNewsfeedGet.SourceIds(const Value: TArrayOfString): Integer;
begin
  Result := List.Add('source_ids', Value);
end;

function TVkParamsNewsfeedGet.StartFrom(const Value: string): Integer;
begin
  Result := List.Add('start_from', Value);
end;

function TVkParamsNewsfeedGet.StartTime(const Value: TDateTime): Integer;
begin
  Result := List.Add('start_time', Value);
end;

{ TVkParamsNewsfeedBanned }

function TVkParamsNewsfeedBanned.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsNewsfeedBanned.Fields(const GroupFields: TVkGroupFields; UserFields: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsNewsfeedBanned.NameCase(const Value: TVkNameCase): Integer;
begin
  Result := List.Add('name_case', Value.ToString);
end;

{ TVkParamsNewsfeedGetComments }

function TVkParamsNewsfeedGetComments.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsNewsfeedGetComments.EndTime(const Value: TDateTime): Integer;
begin
  Result := List.Add('end_time', Value);
end;

function TVkParamsNewsfeedGetComments.Fields(const Value: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsNewsfeedGetComments.Filters(const Value: TVkNewsfeedCommentsTypes): Integer;
begin
  Result := List.Add('filters', Value.ToString);
end;

function TVkParamsNewsfeedGetComments.LastCommentsCount(const Value: Integer): Integer;
begin
  Result := List.Add('last_comments_count', Value);
end;

function TVkParamsNewsfeedGetComments.Reposts(const Value: string): Integer;
begin
  Result := List.Add('reposts', Value);
end;

function TVkParamsNewsfeedGetComments.StartFrom(const Value: string): Integer;
begin
  Result := List.Add('start_from', Value);
end;

function TVkParamsNewsfeedGetComments.StartTime(const Value: TDateTime): Integer;
begin
  Result := List.Add('start_time', Value);
end;

{ TVkParamsNewsfeedGetMentions }

function TVkParamsNewsfeedGetMentions.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsNewsfeedGetMentions.EndTime(const Value: TDateTime): Integer;
begin
  Result := List.Add('end_time', Value);
end;

function TVkParamsNewsfeedGetMentions.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsNewsfeedGetMentions.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsNewsfeedGetMentions.StartTime(const Value: TDateTime): Integer;
begin
  Result := List.Add('start_time', Value);
end;

{ TVkParamsNewsfeedGetRecommended }

function TVkParamsNewsfeedGetRecommended.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsNewsfeedGetRecommended.EndTime(const Value: TDateTime): Integer;
begin
  Result := List.Add('end_time', Value);
end;

function TVkParamsNewsfeedGetRecommended.Fields(const Value: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsNewsfeedGetRecommended.MaxPhotos(const Value: Integer): Integer;
begin
  Result := List.Add('max_photos', Value);
end;

function TVkParamsNewsfeedGetRecommended.StartFrom(const Value: string): Integer;
begin
  Result := List.Add('start_from', Value);
end;

function TVkParamsNewsfeedGetRecommended.StartTime(const Value: TDateTime): Integer;
begin
  Result := List.Add('start_time', Value);
end;

{ TVkParamsNewsfeedSearch }

function TVkParamsNewsfeedSearch.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsNewsfeedSearch.EndTime(const Value: TDateTime): Integer;
begin
  Result := List.Add('end_time', Value);
end;

function TVkParamsNewsfeedSearch.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsNewsfeedSearch.Fields(const GroupFields: TVkGroupFields; UserFields: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsNewsfeedSearch.Latitude(const Value: Extended): Integer;
begin
  Result := List.Add('latitude', Value);
end;

function TVkParamsNewsfeedSearch.Longitude(const Value: Extended): Integer;
begin
  Result := List.Add('longitude', Value);
end;

function TVkParamsNewsfeedSearch.Query(const Value: string): Integer;
begin
  Result := List.Add('q', Value);
end;

function TVkParamsNewsfeedSearch.StartFrom(const Value: string): Integer;
begin
  Result := List.Add('start_from', Value);
end;

function TVkParamsNewsfeedSearch.StartTime(const Value: TDateTime): Integer;
begin
  Result := List.Add('start_time', Value);
end;

end.

