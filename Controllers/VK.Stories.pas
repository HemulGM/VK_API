unit VK.Stories;

interface

uses
  System.SysUtils, System.Generics.Collections, VK.Controller, VK.Types, VK.Entity.Stories, VK.Entity.Stories.Sticker,
  VK.Entity.Stories.Stats, VK.Entity.Stories.Viewed;

type
  TVkParamsStoriesGet = record
    List: TParams;
    function OwnerId(const Value: Integer): Integer;
    function Extended(const Value: Boolean): Integer;
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
  end;

  TVkParamsStoriesGetUploadServer = record
    List: TParams;
    function AddToNews(const Value: Boolean): Integer;
    function UserIds(const Value: TIds): Integer;
    function ReplyToStory(const Value: string): Integer;
    function LinkText(const Value: string): Integer;
    function LinkUrl(const Value: string): Integer;
    function GroupId(const Value: Integer): Integer;
    function ClickableStickers(const Value: string): Integer; overload;
    function ClickableStickers(Value: TVkStoriesStickersInfo): Integer; overload;
  end;

  TVkParamsStoriesGetReplies = record
    List: TParams;
    function OwnerId(const Value: Integer): Integer;
    function StoryId(const Value: Integer): Integer;
    function AccessKey(const Value: string): Integer;
    function Extended(const Value: Boolean): Integer;
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
  end;

  TVkParamsStoriesGetViewers = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function StoryId(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function Extended(Value: Boolean): Integer;
  end;

  TVkParamsStoriesSearch = record
    List: TParams;
    function Query(Value: string): Integer;
    function PlaceId(Value: Integer): Integer;
    function Latitude(Value: Extended): Integer;
    function Longitude(Value: Extended): Integer;
    function Radius(Value: Integer): Integer;
    function MentionedId(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
    function Extended(Value: Boolean): Integer;
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
  end;

  TVkParamsStoriesSendInteraction = record
    List: TParams;
    function AccessKey(Value: string): Integer;
    function Message(Value: string): Integer;
    function IsBroadcast(Value: Boolean): Integer;
    function IsAnonymous(Value: Boolean): Integer;
    function UnseenMarker(Value: Boolean): Integer;
  end;

  /// <summary>
  /// Stories
  /// </summary>
  TStoriesController = class(TVkController)
  public
    /// <summary>
    /// ѕозвол€ет скрыть из ленты новостей истории от выбранных источников.
    /// </summary>
    function BanOwner(const OwnersIds: TIds): Boolean;
    /// <summary>
    /// ”дал€ет историю.
    /// </summary>
    function Delete(const OwnerId, StoryId: Integer): Boolean;
    /// <summary>
    /// ¬озвращает истории, доступные дл€ текущего пользовател€.
    /// </summary>
    function Get(var Items: TVkStoriesBlock; const Params: TParams): Boolean; overload;
    /// <summary>
    /// ¬озвращает истории, доступные дл€ текущего пользовател€.
    /// </summary>
    function Get(var Items: TVkStoriesBlock; const Params: TVkParamsStoriesGet): Boolean; overload;
    /// <summary>
    /// ¬озвращает истории, доступные дл€ текущего пользовател€.
    /// </summary>
    function Get(var Items: TVkStoriesBlock; const OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// ¬озвращает список источников историй, скрытых из ленты текущего пользовател€.
    /// </summary>
    function GetBanned(var Items: TVkStoriesBanned; const Extended: Boolean = False; ProfileFields: TVkProfileFields = [];
      GroupFields: TVkGroupFields = []): Boolean; overload;
    /// <summary>
    /// ¬озвращает истории, доступные дл€ текущего пользовател€.
    /// </summary>
    function GetById(var Items: TVkStoryItems; const Stories: TArrayOfString; Extended: Boolean = False; ProfileFields:
      TVkProfileFields = []; GroupFields: TVkGroupFields = []): Boolean; overload;
    /// <summary>
    /// ѕозвол€ет получить адрес дл€ загрузки истории с фотографией.
    /// </summary>
    function GetPhotoUploadServer(var UploadResult: string; const Params: TVkParamsStoriesGetUploadServer): Boolean;
    /// <summary>
    /// ѕозвол€ет получить ответы на историю.
    /// </summary>
    function GetReplies(var Items: TVkStoriesBlock; const Params: TVkParamsStoriesGetReplies): Boolean;
    /// <summary>
    /// ¬озвращает статистику истории.
    /// </summary>
    function GetStats(var Items: TVkStoryStat; const OwnerId, StoryId: Integer): Boolean;
    /// <summary>
    /// ѕозвол€ет получить адрес дл€ загрузки видеозаписи в историю.
    /// </summary>
    function GetVideoUploadServer(var UploadResult: string; const Params: TVkParamsStoriesGetUploadServer): Boolean;
    /// <summary>
    /// ¬озвращает список пользователей, просмотревших историю.
    /// </summary>
    function GetViewers(var Items: TVkStoryViews; const Params: TVkParamsStoriesGetViewers): Boolean;
    /// <summary>
    /// —крывает все ответы автора за последние сутки на истории текущего пользовател€.
    /// </summary>
    function HideAllReplies(const OwnerId, GroupId: Integer): Boolean;
    /// <summary>
    /// —крывает ответ на историю.
    /// </summary>
    function HideReply(const OwnerId, StoryId: Integer): Boolean;
    /// <summary>
    /// —охран€ет историю. ¬ upload_results нужно передать строку, которую возвращает stories.getPhotoUploadServer или stories.getVideoUploadServer
    /// </summary>
    function Save(var Items: TVkStoryItems; const UploadResults: TArrayOfString): Boolean;
    /// <summary>
    /// ¬озвращает результаты поиска по истори€м.
    /// </summary>
    function Search(var Items: TVkStoriesBlock; const Params: TParams): Boolean; overload;
    /// <summary>
    /// ¬озвращает результаты поиска по истори€м.
    /// </summary>
    function Search(var Items: TVkStoriesBlock; const Params: TVkParamsStoriesSearch): Boolean; overload;
    /// <summary>
    /// ќтправл€ет фидбек на историю.
    /// ƒоступен приложени€м с типом VK Mini Apps. ћетод по умолчанию недоступен приложени€м. ѕолучить доступ к использованию метода можно подав за€вку на размещение в каталоге приложений и подробно рассказав о механике, используемой в приложении.
    /// </summary>
    function SendInteraction(const Params: TVkParamsStoriesSendInteraction): Boolean;
    /// <summary>
    /// ѕозвол€ет вернуть пользовател€ или сообщество в список отображаемых историй в ленте.
    /// </summary>
    function UnbanOwner(const OwnersIds: TIds): Boolean;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TStoriesController }

function TStoriesController.BanOwner(const OwnersIds: TIds): Boolean;
begin
  with Handler.Execute('stories.banOwner', ['owners_ids', OwnersIds.ToString]) do
  begin
    Result := Success and ResponseIsTrue;
  end;
end;

function TStoriesController.Delete(const OwnerId, StoryId: Integer): Boolean;
begin
  with Handler.Execute('stories.delete', [['owner_id', OwnerId.ToString], ['story_id', StoryId.ToString]]) do
  begin
    Result := Success and ResponseIsTrue;
  end;
end;

function TStoriesController.Get(var Items: TVkStoriesBlock; const OwnerId: Integer): Boolean;
var
  Params: TVkParamsStoriesGet;
begin
  if OwnerId <> 0 then
    Params.OwnerId(OwnerId);
  Result := Get(Items, Params.List);
end;

function TStoriesController.GetBanned(var Items: TVkStoriesBanned; const Extended: Boolean; ProfileFields:
  TVkProfileFields; GroupFields: TVkGroupFields): Boolean;
var
  Params: TParams;
begin
  Params.Add('extended', Extended);
  if (ProfileFields <> []) or (GroupFields <> []) then
    Params.Add('fields', [ProfileFields.ToString, GroupFields.ToString]);
  Result := Handler.Execute('stories.getBanned', Params).GetObject<TVkStoriesBanned>(Items);
end;

function TStoriesController.GetById(var Items: TVkStoryItems; const Stories: TArrayOfString; Extended: Boolean;
  ProfileFields: TVkProfileFields; GroupFields: TVkGroupFields): Boolean;
var
  Params: TParams;
begin
  Params.Add('stories', Stories);
  Params.Add('extended', Extended);
  if (ProfileFields <> []) or (GroupFields <> []) then
    Params.Add('fields', [ProfileFields.ToString, GroupFields.ToString]);
  Result := Handler.Execute('stories.getById', Params).GetObject<TVkStoryItems>(Items);
end;

function TStoriesController.GetPhotoUploadServer(var UploadResult: string; const Params: TVkParamsStoriesGetUploadServer):
  Boolean;
begin
  with Handler.Execute('stories.getPhotoUploadServer', Params.List) do
    Result := Success and GetValue('upload_result', UploadResult);
end;

function TStoriesController.GetReplies(var Items: TVkStoriesBlock; const Params: TVkParamsStoriesGetReplies): Boolean;
begin
  Result := Handler.Execute('stories.getReplies', Params.List).GetObject<TVkStoriesBlock>(Items);
end;

function TStoriesController.GetStats(var Items: TVkStoryStat; const OwnerId, StoryId: Integer): Boolean;
begin
  Result := Handler.Execute('stories.getStats', [['owner_id', OwnerId.ToString], ['story_id', StoryId.ToString]]).GetObject
    <TVkStoryStat>(Items);
end;

function TStoriesController.GetVideoUploadServer(var UploadResult: string; const Params: TVkParamsStoriesGetUploadServer):
  Boolean;
begin
  with Handler.Execute('stories.getVideoUploadServer', Params.List) do
    Result := Success and GetValue('upload_result', UploadResult);
end;

function TStoriesController.GetViewers(var Items: TVkStoryViews; const Params: TVkParamsStoriesGetViewers): Boolean;
begin
  Result := Handler.Execute('stories.getViewers', Params.List).GetObject<TVkStoryViews>(Items);
end;

function TStoriesController.HideAllReplies(const OwnerId, GroupId: Integer): Boolean;
begin
  with Handler.Execute('stories.hideAllReplies', [['owner_id', OwnerId.ToString], ['group_id', GroupId.ToString]]) do
  begin
    Result := Success and ResponseIsTrue;
  end;
end;

function TStoriesController.HideReply(const OwnerId, StoryId: Integer): Boolean;
begin
  with Handler.Execute('stories.hideReply', [['owner_id', OwnerId.ToString], ['story_id', StoryId.ToString]]) do
  begin
    Result := Success and ResponseIsTrue;
  end;
end;

function TStoriesController.Save(var Items: TVkStoryItems; const UploadResults: TArrayOfString): Boolean;
begin
  Result := Handler.Execute('stories.save', ['upload_results', UploadResults.ToString]).GetObject<TVkStoryItems>(Items);
end;

function TStoriesController.Search(var Items: TVkStoriesBlock; const Params: TVkParamsStoriesSearch): Boolean;
begin
  Result := Search(Items, Params.List);
end;

function TStoriesController.SendInteraction(const Params: TVkParamsStoriesSendInteraction): Boolean;
begin
  with Handler.Execute('stories.sendInteraction', Params.List) do
  begin
    Result := Success and ResponseIsTrue;
  end;
end;

function TStoriesController.UnbanOwner(const OwnersIds: TIds): Boolean;
begin
  with Handler.Execute('stories.unbanOwner', ['owners_ids', OwnersIds.ToString]) do
  begin
    Result := Success and ResponseIsTrue;
  end;
end;

function TStoriesController.Search(var Items: TVkStoriesBlock; const Params: TParams): Boolean;
begin
  Result := Handler.Execute('stories.search', Params).GetObject<TVkStoriesBlock>(Items);
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

function TVkParamsStoriesGetUploadServer.UserIds(const Value: TIds): Integer;
begin
  Result := List.Add('user_ids', Value);
end;

function TVkParamsStoriesGetUploadServer.ReplyToStory(const Value: string): Integer;
begin
  Result := List.Add('reply_to_story', Value);
end;

function TVkParamsStoriesGetUploadServer.LinkText(const Value: string): Integer;
begin
  Result := List.Add('link_text', Value);
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

