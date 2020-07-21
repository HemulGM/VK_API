unit VK.Newsfeed;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, System.JSON, VK.Entity.Newsfeed,
  VK.Entity.Media;

type
  /// <summary>
  /// post — новые записи со стен;
  /// photo — новые фотографии;
  /// photo_tag — новые отметки на фотографиях;
  /// wall_photo — новые фотографии на стенах;
  /// friend — новые друзья;
  /// note — новые заметки;
  /// audio — записи сообществ и друзей, содержащие аудиозаписи, а также новые аудиозаписи, добавленные ими;
  /// video — новые видеозаписи.
  /// </summary>
  TVkNewsfeedType = (ntPost, ntPhoto, ntPhotoTag, ntWallPhoto, ntFriend, ntNote, ntAudio, ntVideo);

  TVkNewsfeedTypeHelper = record Helper for TVkNewsfeedType
    function ToString: string; inline;
  end;

  TVkNewsfeedTypes = set of TVkNewsfeedType;

  TVkNewsfeedTypesHelper = record Helper for TVkNewsfeedTypes
    function ToString: string; inline;
  end;

  /// <summary>
  /// post — новые комментарии к записям со стен;
  /// photo — новые комментарии к фотографиям;
  /// video — новые комментарии к видеозаписям;
  /// topic — новые сообщения в обсуждениях;
  /// market — новые комментарии к товарам;
  /// note — новые комментарии к заметкам.
  /// </summary>
  TVkNewsfeedCommentsType = (nctPost, nctPhoto, nctVideo, nctTopic, nctMarket, nctNote);

  TVkNewsfeedCommentsTypeHelper = record Helper for TVkNewsfeedCommentsType
    function ToString: string; inline;
  end;

  TVkNewsfeedCommentsTypes = set of TVkNewsfeedCommentsType;

  TVkNewsfeedCommentsTypesHelper = record Helper for TVkNewsfeedCommentsTypes
    function ToString: string; inline;
  end;

  TVkParamsNewsfeedBanned = record
  private
    List: TParams;
    function Extended(Value: Boolean): Integer;
  public
    function Fields(const GroupFields: TVkGroupFields = []; UserFields: TVkUserFields = []): Integer;
    function NameCase(Value: TVkNameCase): Integer;
  end;

  TVkParamsNewsfeedGet = record
    List: TParams;
    function Filters(Value: TVkNewsfeedTypes = []): Integer;
    function ReturnBanned(Value: Boolean): Integer;
    function StartTime(Value: TDateTime): Integer;
    function EndTime(Value: TDateTime): Integer;
    function MaxPhotos(Value: Integer): Integer;
    function SourceIds(Value: TIds): Integer;
    function StartFrom(Value: string): Integer;
    function Count(Value: Integer): Integer;
    function Fields(const GroupFields: TVkGroupFields = []; UserFields: TVkUserFields = []): Integer;
    function Section(Value: string): Integer;
  end;

  TVkParamsNewsfeedGetComments = record
    List: TParams;
    function Filters(Value: TVkNewsfeedCommentsTypes = []): Integer;
    function Fields(const Value: TVkUserFields = []): Integer;
    function Count(Value: Integer): Integer;
    function LastCommentsCount(Value: Integer): Integer;
    function StartTime(Value: TDateTime): Integer;
    function EndTime(Value: TDateTime): Integer;
    function StartFrom(Value: string): Integer;
    function Reposts(Value: string): Integer;
  end;

  TVkParamsNewsfeedGetMentions = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function StartTime(Value: TDateTime): Integer;
    function EndTime(Value: TDateTime): Integer;
  end;

  TNewsfeedController = class(TVkController)
  public
    /// <summary>
    /// Запрещает показывать новости от заданных пользователей и групп в ленте новостей текущего пользователя.
    /// </summary>
    function AddBan(UserIds: TIds = []; GroupIds: TIds = []): Boolean;
    /// <summary>
    /// Разрешает показывать новости от заданных пользователей и групп в ленте новостей текущего пользователя.
    /// </summary>
    function DeleteBan(UserIds: TIds = []; GroupIds: TIds = []): Boolean;
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
    function GetLists(var Items: TVkNewsfeedLists; ListIds: TIds; Extended: Boolean = False): Boolean; overload;
    /// <summary>
    /// Возвращает список записей пользователей на своих стенах, в которых упоминается указанный пользователь.
    /// </summary>
    function GetMentions(var Items: TVkPosts; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список записей пользователей на своих стенах, в которых упоминается указанный пользователь.
    /// </summary>
    function GetMentions(var Items: TVkPosts; Params: TVkParamsNewsfeedGetMentions): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TNewsfeedController }

function TNewsfeedController.AddBan(UserIds, GroupIds: TIds): Boolean;
var
  Params: TParams;
begin
  if Length(UserIds) > 0 then
    Params.Add('user_ids', UserIds);
  if Length(GroupIds) > 0 then
    Params.Add('group_ids', GroupIds);
  with Handler.Execute('newsfeed.addBan', Params) do
    Result := Success and (Response = '1');
end;

function TNewsfeedController.DeleteBan(UserIds, GroupIds: TIds): Boolean;
var
  Params: TParams;
begin
  if Length(UserIds) > 0 then
    Params.Add('user_ids', UserIds);
  if Length(GroupIds) > 0 then
    Params.Add('group_ids', GroupIds);
  with Handler.Execute('newsfeed.deleteBan', Params) do
    Result := Success and (Response = '1');
end;

function TNewsfeedController.DeleteList(ListId: Integer): Boolean;
begin
  with Handler.Execute('newsfeed.deleteList', ['list_id', ListId.ToString]) do
    Result := Success and (Response = '1');
end;

function TNewsfeedController.Get(var Items: TVkNews; Params: TVkParamsNewsfeedGet): Boolean;
begin
  Result := Get(Items, Params.List);
end;

function TNewsfeedController.GetBanned(var Items: TVkNewsfeedBanned; Params: TVkParamsNewsfeedBanned): Boolean;
begin
  Params.Extended(True);
  with Handler.Execute('newsfeed.getBanned', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkNewsfeedBanned.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TNewsfeedController.GetComments(var Items: TVkNews; Params: TVkParamsNewsfeedGetComments): Boolean;
begin
  Result := GetComments(Items, Params.List);
end;

function TNewsfeedController.GetLists(var Items: TVkNewsfeedLists; ListIds: TIds; Extended: Boolean): Boolean;
begin
  with Handler.Execute('newsfeed.getLists', [['list_ids', ListIds.ToString], ['extended', BoolToString(Extended)]]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkNewsfeedLists.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TNewsfeedController.GetMentions(var Items: TVkPosts; Params: TVkParamsNewsfeedGetMentions): Boolean;
begin
  Result := GetMentions(Items, Params.List);
end;

function TNewsfeedController.GetMentions(var Items: TVkPosts; Params: TParams): Boolean;
begin
  with Handler.Execute('newsfeed.getMentions', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkPosts.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TNewsfeedController.GetComments(var Items: TVkNews; Params: TParams): Boolean;
begin
  with Handler.Execute('newsfeed.getComments', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkNews.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TNewsfeedController.GetBanned(var Items: TVkNewsfeedBannedIds; Params: TVkParamsNewsfeedBanned): Boolean;
begin
  Params.Extended(False);
  with Handler.Execute('newsfeed.getBanned', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkNewsfeedBannedIds.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TNewsfeedController.Get(var Items: TVkNews; Params: TParams): Boolean;
begin
  with Handler.Execute('newsfeed.get', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkNews.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

{ TVkNewsfeedTypeHelper }

function TVkNewsfeedTypeHelper.ToString: string;
begin
  case Self of
    ntPost:
      Result := 'post';
    ntPhoto:
      Result := 'photo';
    ntPhotoTag:
      Result := 'photo_tag';
    ntWallPhoto:
      Result := 'wall_photo';
    ntFriend:
      Result := 'friend';
    ntNote:
      Result := 'note';
    ntAudio:
      Result := 'audio';
    ntVideo:
      Result := 'video';
  else
    Result := ''
  end;
end;

{ TVkNewsfeedTypesHelper }

function TVkNewsfeedTypesHelper.ToString: string;
var
  Item: TVkNewsfeedType;
begin
  for Item in Self do
  begin
    Result := Result + Item.ToString + ',';
  end;
  Result.TrimRight([',']);
end;

{ TVkParamsNewsfeedGet }

function TVkParamsNewsfeedGet.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsNewsfeedGet.EndTime(Value: TDateTime): Integer;
begin
  Result := List.Add('end_time', Value);
end;

function TVkParamsNewsfeedGet.Fields(const GroupFields: TVkGroupFields; UserFields: TVkUserFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsNewsfeedGet.Filters(Value: TVkNewsfeedTypes): Integer;
begin
  Result := List.Add('filters', Value.ToString);
end;

function TVkParamsNewsfeedGet.MaxPhotos(Value: Integer): Integer;
begin
  Result := List.Add('max_photos', Value);
end;

function TVkParamsNewsfeedGet.ReturnBanned(Value: Boolean): Integer;
begin
  Result := List.Add('return_banned', Value);
end;

function TVkParamsNewsfeedGet.Section(Value: string): Integer;
begin
  Result := List.Add('section', Value);
end;

function TVkParamsNewsfeedGet.SourceIds(Value: TIds): Integer;
begin
  Result := List.Add('source_ids', Value);
end;

function TVkParamsNewsfeedGet.StartFrom(Value: string): Integer;
begin
  Result := List.Add('start_from', Value);
end;

function TVkParamsNewsfeedGet.StartTime(Value: TDateTime): Integer;
begin
  Result := List.Add('start_time', Value);
end;

{ TVkParamsNewsfeedBanned }

function TVkParamsNewsfeedBanned.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsNewsfeedBanned.Fields(const GroupFields: TVkGroupFields; UserFields: TVkUserFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsNewsfeedBanned.NameCase(Value: TVkNameCase): Integer;
begin
  Result := List.Add('name_case', Value.ToString);
end;

{ TVkNewsfeedCommentsTypeHelper }

function TVkNewsfeedCommentsTypeHelper.ToString: string;
begin
  case Self of
    nctPost:
      Result := 'post';
    nctPhoto:
      Result := 'photo';
    nctVideo:
      Result := 'video';
    nctTopic:
      Result := 'topic';
    nctMarket:
      Result := 'market';
    nctNote:
      Result := 'note';
  else
    Result := ''
  end;
end;

{ TVkNewsfeedCommentsTypesHelper }

function TVkNewsfeedCommentsTypesHelper.ToString: string;
var
  Item: TVkNewsfeedCommentsType;
begin
  for Item in Self do
  begin
    Result := Result + Item.ToString + ',';
  end;
  Result.TrimRight([',']);
end;

{ TVkParamsNewsfeedGetComments }

function TVkParamsNewsfeedGetComments.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsNewsfeedGetComments.EndTime(Value: TDateTime): Integer;
begin
  Result := List.Add('end_time', Value);
end;

function TVkParamsNewsfeedGetComments.Fields(const Value: TVkUserFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsNewsfeedGetComments.Filters(Value: TVkNewsfeedCommentsTypes): Integer;
begin
  Result := List.Add('filters', Value.ToString);
end;

function TVkParamsNewsfeedGetComments.LastCommentsCount(Value: Integer): Integer;
begin
  Result := List.Add('last_comments_count', Value);
end;

function TVkParamsNewsfeedGetComments.Reposts(Value: string): Integer;
begin
  Result := List.Add('reposts', Value);
end;

function TVkParamsNewsfeedGetComments.StartFrom(Value: string): Integer;
begin
  Result := List.Add('start_from', Value);
end;

function TVkParamsNewsfeedGetComments.StartTime(Value: TDateTime): Integer;
begin
  Result := List.Add('start_time', Value);
end;

{ TVkParamsNewsfeedGetMentions }

function TVkParamsNewsfeedGetMentions.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsNewsfeedGetMentions.EndTime(Value: TDateTime): Integer;
begin
  Result := List.Add('end_time', Value);
end;

function TVkParamsNewsfeedGetMentions.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsNewsfeedGetMentions.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsNewsfeedGetMentions.StartTime(Value: TDateTime): Integer;
begin
  Result := List.Add('start_time', Value);
end;

end.

