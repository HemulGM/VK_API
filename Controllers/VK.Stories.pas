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
    /// ������������� ������������, ������� �������� ���������� ��������
    /// </summary>
    function OwnerId(const Value: Integer): TVkParamsStoriesGet;
    /// <summary>
    /// True � ���������� � ������ �������������� ���������� � �������� �������������
    /// </summary>
    function Extended(const Value: Boolean = False): TVkParamsStoriesGet;
    /// <summary>
    /// ������ �������������� ����� ��� �������� User � Group, ������� ���������� �������
    /// </summary>
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): TVkParamsStoriesGet;
  end;

  TVkParamsStoriesGetUploadServer = record
    List: TParams;
    /// <summary>
    /// True � ���������� ������� � ��������
    /// </summary>
    function AddToNews(const Value: Boolean): TVkParamsStoriesGetUploadServer;
    /// <summary>
    /// �������������� �������������, ������� ����� ������ ������� (��� �������� � ������ ���������)
    /// </summary>
    function UserIds(const Value: TIdList): TVkParamsStoriesGetUploadServer;
    /// <summary>
    /// ������������� �������, � ����� �� ������� ��������� �����
    /// </summary>
    function ReplyToStory(const Value: string): TVkParamsStoriesGetUploadServer;
    /// <summary>
    /// ����� ������ ��� �������� �� ������� (������ ��� ������� ���������)
    /// </summary>
    function LinkText(const Value: TVkLinkText): TVkParamsStoriesGetUploadServer;
    /// <summary>
    /// ����� ������ ��� �������� �� �������
    /// </summary>
    function LinkUrl(const Value: string): TVkParamsStoriesGetUploadServer;
    /// <summary>
    /// ������������� ����������, � ������� ������ ���� ��������� ������� (��� ������ � ������ ������� ������������)
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsStoriesGetUploadServer;
    /// <summary>
    /// ������ ������������� ������� (������ � ������� JSON)
    /// </summary>
    function ClickableStickers(const Value: string): TVkParamsStoriesGetUploadServer; overload;
    /// <summary>
    /// ������ ������������� �������
    /// </summary>
    function ClickableStickers(Value: TVkStoriesStickersInfo): TVkParamsStoriesGetUploadServer; overload;
  end;

  TVkParamsStoriesGetReplies = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� �������
    /// </summary>
    function OwnerId(const Value: Integer): TVkParamsStoriesGetReplies;
    /// <summary>
    /// ������������� �������
    /// </summary>
    function StoryId(const Value: Integer): TVkParamsStoriesGetReplies;
    /// <summary>
    /// ���� ������� ��� ���������� �������
    /// </summary>
    function AccessKey(const Value: string): TVkParamsStoriesGetReplies;
    /// <summary>
    /// True � ���������� �������������� ���������� � �������� � �����������
    /// </summary>
    function Extended(const Value: Boolean = False): TVkParamsStoriesGetReplies;
    /// <summary>
    /// �������������� ���� �������� � ���������, ������� ���������� ������� � ������
    /// </summary>
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): TVkParamsStoriesGetReplies;
  end;

  TVkParamsStoriesGetViewers = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� �������
    /// </summary>
    function OwnerId(Value: Integer): TVkParamsStoriesGetViewers;
    /// <summary>
    /// ������������� �������
    /// </summary>
    function StoryId(Value: Integer): TVkParamsStoriesGetViewers;
    /// <summary>
    /// ������������ ����� ����������� � ������
    /// </summary>
    function Count(Value: Integer = 100): TVkParamsStoriesGetViewers;
    /// <summary>
    /// ����� ��� ��������� ������������ ������������ �����������
    /// </summary>
    function Offset(Value: Integer = 0): TVkParamsStoriesGetViewers;
    /// <summary>
    /// True � ���������� � ������ ����������� ���������� � �������������
    /// </summary>
    function Extended(Value: Boolean = False): TVkParamsStoriesGetViewers;
  end;

  TVkParamsStoriesSearch = record
    List: TParams;
    /// <summary>
    /// ��������� ������
    /// </summary>
    function Query(Value: string): TVkParamsStoriesSearch;
    /// <summary>
    /// ������������� �����
    /// </summary>
    function PlaceId(Value: Integer): TVkParamsStoriesSearch;
    /// <summary>
    /// �������������� ������ �����, � ������� ������� ���������� ����������� �����, �������� � �������� (�� -90 �� 90)
    /// </summary>
    function Latitude(Value: Extended): TVkParamsStoriesSearch;
    /// <summary>
    /// �������������� ������� �����, � ������� ������� ���������� ����������� �����, �������� � �������� (�� -180 �� 180)
    /// </summary>
    function Longitude(Value: Extended): TVkParamsStoriesSearch;
    /// <summary>
    /// ������ ���� ������ � ������
    /// </summary>
    function Radius(Value: Integer): TVkParamsStoriesSearch;
    /// <summary>
    /// ������������� ����������� � ������� ������������ ��� ����������
    /// </summary>
    function MentionedId(Value: Integer): TVkParamsStoriesSearch;
    /// <summary>
    /// ���������� �������, ���������� � ������� ���������� �������
    /// </summary>
    function Count(Value: Integer = 20): TVkParamsStoriesSearch;
    /// <summary>
    /// ��������, ������������ ������������� ���������� ����������� ���������� � ��������� �������
    /// False - ������������ ������ ��������������
    /// True � ����� ������������� ���������� ��� � ������
    /// </summary>
    function Extended(Value: Boolean): TVkParamsStoriesSearch;
    /// <summary>
    /// ������ �������������� ����� ��������, ������� ���������� �������
    /// </summary>
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): TVkParamsStoriesSearch;
  end;

  TVkParamsStoriesSendInteraction = record
    List: TParams;
    /// <summary>
    /// ���� ������� ������������, ���������� ��� ��������. ���������� ������� VKWebAppSubscribeStoryApp
    /// </summary>
    function AccessKey(Value: string): TVkParamsStoriesSendInteraction;
    /// <summary>
    /// ����� �������
    /// </summary>
    function Message(Value: string): TVkParamsStoriesSendInteraction;
    /// <summary>
    /// False � ������ ����� ������ ����������� � ������ �������;
    /// True � ������ ����� ���� �������� ������� � ������
    /// </summary>
    function IsBroadcast(Value: Boolean = False): TVkParamsStoriesSendInteraction;
    /// <summary>
    /// False � ����� ������� �� ���������;
    /// True � ����� ������� ���������
    /// </summary>
    function IsAnonymous(Value: Boolean = False): TVkParamsStoriesSendInteraction;
    function UnseenMarker(Value: Boolean): TVkParamsStoriesSendInteraction;
  end;

  /// <summary>
  /// Stories
  /// </summary>
  TStoriesController = class(TVkController)
  public
    /// <summary>
    /// ��������� ������ �� ����� �������� ������� �� ��������� ����������.
    /// </summary>
    function BanOwner(const OwnersIds: TIdList): Boolean;
    /// <summary>
    /// ������� �������.
    /// </summary>
    function Delete(const OwnerId, StoryId: Integer): Boolean;
    /// <summary>
    /// ���������� �������, ��������� ��� �������� ������������.
    /// </summary>
    function Get(var Items: TVkStoriesBlock; const Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� �������, ��������� ��� �������� ������������.
    /// </summary>
    function Get(var Items: TVkStoriesBlock; const Params: TVkParamsStoriesGet): Boolean; overload;
    /// <summary>
    /// ���������� �������, ��������� ��� �������� ������������.
    /// </summary>
    function Get(var Items: TVkStoriesBlock; const OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// ���������� ������ ���������� �������, ������� �� ����� �������� ������������.
    /// </summary>
    function GetBanned(var Items: TVkStoriesBanned; const Extended: Boolean = False; ProfileFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Boolean; overload;
    /// <summary>
    /// ���������� �������, ��������� ��� �������� ������������.
    /// </summary>
    function GetById(var Items: TVkStoryItems; const Stories: TArrayOfString; Extended: Boolean = False; ProfileFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Boolean; overload;
    /// <summary>
    /// ��������� �������� ����� ��� �������� ������� � �����������.
    /// </summary>
    function GetPhotoUploadServer(var UploadResult: string; const Params: TVkParamsStoriesGetUploadServer): Boolean;
    /// <summary>
    /// ��������� �������� ������ �� �������.
    /// </summary>
    function GetReplies(var Items: TVkStoriesBlock; const Params: TVkParamsStoriesGetReplies): Boolean;
    /// <summary>
    /// ���������� ���������� �������.
    /// </summary>
    function GetStats(var Items: TVkStoryStat; const OwnerId, StoryId: Integer): Boolean;
    /// <summary>
    /// ��������� �������� ����� ��� �������� ����������� � �������.
    /// </summary>
    function GetVideoUploadServer(var UploadResult: string; const Params: TVkParamsStoriesGetUploadServer): Boolean;
    /// <summary>
    /// ���������� ������ �������������, ������������� �������.
    /// </summary>
    function GetViewers(var Items: TVkStoryViews; const Params: TVkParamsStoriesGetViewers): Boolean;
    /// <summary>
    /// �������� ��� ������ ������ �� ��������� ����� �� ������� �������� ������������.
    /// </summary>
    function HideAllReplies(const OwnerId, GroupId: Integer): Boolean;
    /// <summary>
    /// �������� ����� �� �������.
    /// </summary>
    function HideReply(const OwnerId, StoryId: Integer): Boolean;
    /// <summary>
    /// ��������� �������. � upload_results ����� �������� ������, ������� ���������� stories.getPhotoUploadServer ��� stories.getVideoUploadServer
    /// </summary>
    function Save(var Items: TVkStoryItems; const UploadResults: TArrayOfString): Boolean;
    /// <summary>
    /// ���������� ���������� ������ �� ��������.
    /// </summary>
    function Search(var Items: TVkStoriesBlock; const Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ���������� ������ �� ��������.
    /// </summary>
    function Search(var Items: TVkStoriesBlock; const Params: TVkParamsStoriesSearch): Boolean; overload;
    /// <summary>
    /// ���������� ������ �� �������.
    /// �������� ����������� � ����� VK Mini Apps. ����� �� ��������� ���������� �����������. �������� ������ � ������������� ������ ����� ����� ������ �� ���������� � �������� ���������� � �������� ��������� � ��������, ������������ � ����������.
    /// </summary>
    function SendInteraction(const Params: TVkParamsStoriesSendInteraction): Boolean;
    /// <summary>
    /// ��������� ������� ������������ ��� ���������� � ������ ������������ ������� � �����.
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

function TVkParamsStoriesGet.Extended(const Value: Boolean): TVkParamsStoriesGet;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsStoriesGet.Fields(UserFields: TVkProfileFields; GroupFields: TVkGroupFields): TVkParamsStoriesGet;
begin
  List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
  Result := Self;
end;

function TVkParamsStoriesGet.OwnerId(const Value: Integer): TVkParamsStoriesGet;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

{ TVkParamsStoriesGetPhotoUploadServer }

function TVkParamsStoriesGetUploadServer.AddToNews(const Value: Boolean): TVkParamsStoriesGetUploadServer;
begin
  List.Add('add_to_news', Value);
  Result := Self;
end;

function TVkParamsStoriesGetUploadServer.UserIds(const Value: TIdList): TVkParamsStoriesGetUploadServer;
begin
  List.Add('user_ids', Value);
  Result := Self;
end;

function TVkParamsStoriesGetUploadServer.ReplyToStory(const Value: string): TVkParamsStoriesGetUploadServer;
begin
  List.Add('reply_to_story', Value);
  Result := Self;
end;

function TVkParamsStoriesGetUploadServer.LinkText(const Value: TVkLinkText): TVkParamsStoriesGetUploadServer;
begin
  List.Add('link_text', Value.ToString);
  Result := Self;
end;

function TVkParamsStoriesGetUploadServer.LinkUrl(const Value: string): TVkParamsStoriesGetUploadServer;
begin
  List.Add('link_url', Value);
  Result := Self;
end;

function TVkParamsStoriesGetUploadServer.ClickableStickers(Value: TVkStoriesStickersInfo): TVkParamsStoriesGetUploadServer;
begin
  List.Add('clickable_stickers', Value.ToJsonString);
  Result := Self;
end;

function TVkParamsStoriesGetUploadServer.GroupId(const Value: Integer): TVkParamsStoriesGetUploadServer;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsStoriesGetUploadServer.ClickableStickers(const Value: string): TVkParamsStoriesGetUploadServer;
begin
  List.Add('clickable_stickers', Value);
  Result := Self;
end;

{ TVkParamsStoriesGetReplies }

function TVkParamsStoriesGetReplies.OwnerId(const Value: Integer): TVkParamsStoriesGetReplies;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsStoriesGetReplies.StoryId(const Value: Integer): TVkParamsStoriesGetReplies;
begin
  List.Add('story_id', Value);
  Result := Self;
end;

function TVkParamsStoriesGetReplies.AccessKey(const Value: string): TVkParamsStoriesGetReplies;
begin
  List.Add('access_key', Value);
  Result := Self;
end;

function TVkParamsStoriesGetReplies.Extended(const Value: Boolean): TVkParamsStoriesGetReplies;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsStoriesGetReplies.Fields(UserFields: TVkProfileFields; GroupFields: TVkGroupFields): TVkParamsStoriesGetReplies;
begin
  List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
  Result := Self;
end;

{ TVkParamsStoriesGetViewers }

function TVkParamsStoriesGetViewers.OwnerId(Value: Integer): TVkParamsStoriesGetViewers;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsStoriesGetViewers.StoryId(Value: Integer): TVkParamsStoriesGetViewers;
begin
  List.Add('story_id', Value);
  Result := Self;
end;

function TVkParamsStoriesGetViewers.Count(Value: Integer): TVkParamsStoriesGetViewers;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsStoriesGetViewers.Offset(Value: Integer): TVkParamsStoriesGetViewers;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsStoriesGetViewers.Extended(Value: Boolean): TVkParamsStoriesGetViewers;
begin
  List.Add('extended', Value);
  Result := Self;
end;

{ TVkParamsStoriesSearch }

function TVkParamsStoriesSearch.Query(Value: string): TVkParamsStoriesSearch;
begin
  List.Add('q', Value);
  Result := Self;
end;

function TVkParamsStoriesSearch.PlaceId(Value: Integer): TVkParamsStoriesSearch;
begin
  List.Add('place_id', Value);
  Result := Self;
end;

function TVkParamsStoriesSearch.Latitude(Value: Extended): TVkParamsStoriesSearch;
begin
  List.Add('latitude', Value);
  Result := Self;
end;

function TVkParamsStoriesSearch.Longitude(Value: Extended): TVkParamsStoriesSearch;
begin
  List.Add('longitude', Value);
  Result := Self;
end;

function TVkParamsStoriesSearch.Radius(Value: Integer): TVkParamsStoriesSearch;
begin
  List.Add('radius', Value);
  Result := Self;
end;

function TVkParamsStoriesSearch.MentionedId(Value: Integer): TVkParamsStoriesSearch;
begin
  List.Add('mentioned_id', Value);
  Result := Self;
end;

function TVkParamsStoriesSearch.Count(Value: Integer): TVkParamsStoriesSearch;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsStoriesSearch.Extended(Value: Boolean): TVkParamsStoriesSearch;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsStoriesSearch.Fields(UserFields: TVkProfileFields; GroupFields: TVkGroupFields): TVkParamsStoriesSearch;
begin
  List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
  Result := Self;
end;

{ TVkParamsStoriesSendInteraction }

function TVkParamsStoriesSendInteraction.AccessKey(Value: string): TVkParamsStoriesSendInteraction;
begin
  List.Add('access_key', Value);
  Result := Self;
end;

function TVkParamsStoriesSendInteraction.Message(Value: string): TVkParamsStoriesSendInteraction;
begin
  List.Add('message', Value);
  Result := Self;
end;

function TVkParamsStoriesSendInteraction.IsBroadcast(Value: Boolean): TVkParamsStoriesSendInteraction;
begin
  List.Add('is_broadcast', Value);
  Result := Self;
end;

function TVkParamsStoriesSendInteraction.IsAnonymous(Value: Boolean): TVkParamsStoriesSendInteraction;
begin
  List.Add('is_anonymous', Value);
  Result := Self;
end;

function TVkParamsStoriesSendInteraction.UnseenMarker(Value: Boolean): TVkParamsStoriesSendInteraction;
begin
  List.Add('unseen_marker', Value);
  Result := Self;
end;

end.

