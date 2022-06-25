unit VK.Newsfeed;

interface

uses
  System.SysUtils, VK.Controller, VK.Types, VK.Entity.Newsfeed, VK.Entity.Media;

type
  TVkParamsNewsfeedBanned = record
  private
    function Extended(const Value: Boolean): TVkParamsNewsfeedBanned;
  public
    List: TParams;
    /// <summary>
    /// ������ �������������� ����� ��������, ������� ���������� �������
    /// </summary>
    function Fields(const GroupFields: TVkGroupFields = []; UserFields: TVkProfileFields = []): TVkParamsNewsfeedBanned;
    /// <summary>
    /// ����� ��� ��������� ����� � ������� ������������
    /// </summary>
    function NameCase(const Value: TVkNameCase): TVkParamsNewsfeedBanned;
  end;

  TVkParamsNewsfeedGet = record
    List: TParams;
    /// <summary>
    /// ������������� ����� ������� �������� ������� ��������, ������� ���������� ��������
    /// ���� �������� �� �����, �� ����� �������� ��� ��������� ������ ��������
    /// </summary>
    function Filters(const Value: TVkNewsfeedTypes = []): TVkParamsNewsfeedGet;
    /// <summary>
    /// True - �������� � ������ ����� ������� �� �������� �������������. False - �� ���������� ������� �������������
    /// </summary>
    function ReturnBanned(const Value: Boolean): TVkParamsNewsfeedGet;
    /// <summary>
    /// �����, ������� � �������� ������� �������� ������� ��� �������� ������������
    /// </summary>
    function StartTime(const Value: TDateTime): TVkParamsNewsfeedGet;
    /// <summary>
    /// �����, �� �������� ������� �������� ������� ��� �������� ������������.
    /// ���� �������� �� �����, �� �� ��������� ������ �������� �������
    /// </summary>
    function EndTime(const Value: TDateTime): TVkParamsNewsfeedGet;
    /// <summary>
    /// ������������ ���������� ����������, ���������� � ������� ���������� ������� (������������ ��������: 100)
    /// </summary>
    function MaxPhotos(const Value: Integer = 5): TVkParamsNewsfeedGet;
    /// <summary>
    /// ������������� ����� ������� �c������� ��������, ������� �� ������� ���������� ��������
    /// [uid] ��� u[uid], -[gid] ��� g[gid]
    /// ������ ����� �������� ����� ��������� ��������� ��������:
    /// friends - ������ ������ ������������
    /// groups - ������ �����, �� ������� �������� ������� ������������
    /// pages - ������ ��������� �������, �� ������� �������� �e����� ������������
    /// following - ������ �������������, �� ������� �������� ������� ������������
    /// list[������������� ������ ��������] - ������ ��������. �� ������ ����� ��� ������ �������� ������������ ��������� ����� newsfeed.getLists
    /// </summary>
    function SourceIds(const Value: TArrayOfString): TVkParamsNewsfeedGet;
    /// <summary>
    /// �������������, ����������� ��� ��������� ��������� �������� �����������. ��������, ����������� ��� �������� � ���� ���������, ������������ � ���� ������ next_from
    /// </summary>
    function StartFrom(const Value: string): TVkParamsNewsfeedGet;
    /// <summary>
    /// ���������, ����� ������������ ����� �������� ������� ����������, �� �� ����� 100
    /// </summary>
    function Count(const Value: Integer = 50): TVkParamsNewsfeedGet;
    /// <summary>
    /// ������ �������������� ����� ��� �������� � �����, ������� ���������� �������
    /// </summary>
    function Fields(const GroupFields: TVkGroupFields = []; UserFields: TVkProfileFields = []): TVkParamsNewsfeedGet;
    /// <summary>
    /// [��� ��������]
    /// </summary>
    function Section(const Value: string): TVkParamsNewsfeedGet;
  end;

  TVkParamsNewsfeedGetComments = record
    List: TParams;
    /// <summary>
    /// ������������� ����� ������� ���� ��������, ��������� ������������ � ������� ����� �������
    /// </summary>
    function Filters(const Value: TVkNewsfeedCommentsTypes = []): TVkParamsNewsfeedGetComments;
    /// <summary>
    /// ������ �������������� ����� ��������, ������� ���������� �������
    /// </summary>
    function Fields(const Value: TVkProfileFields = []): TVkParamsNewsfeedGetComments;
    /// <summary>
    /// ���������, ����� ������������ ����� �������� ������� ����������, �� �� ����� 100.
    /// ��� ������������� �� ������ ������������ ������������ ������ ������� �������� NewOffset.
    /// </summary>
    function Count(const Value: Integer = 30): TVkParamsNewsfeedGetComments;
    /// <summary>
    /// ���������� ������������ � �������, ������� ����� ��������.
    /// ������������� �����, �������� ������� � ������ 5.23, ������������ �������� 10
    /// </summary>
    function LastCommentsCount(const Value: Integer = 0): TVkParamsNewsfeedGetComments;
    /// <summary>
    /// �����, �� �������� ������� �������� ������� ��� �������� ������������.
    /// ���� �������� �� �����, �� �� ��������� ������ �������� �������
    /// </summary>
    function StartTime(const Value: TDateTime): TVkParamsNewsfeedGetComments;
    /// <summary>
    /// �����, ������� � �������� ������� �������� ������� ��� �������� ������������.
    /// ���� �������� �� �����, �� �� ��������� ������ �������� �������, ������� ���� ����� �����
    /// </summary>
    function EndTime(const Value: TDateTime): TVkParamsNewsfeedGetComments;
    /// <summary>
    /// �������������, ����������� ��� ��������� ��������� �������� �����������.
    /// ��������, ����������� ��� �������� � ���� ���������, ������������ � ���� ������ NextFrom
    /// </summary>
    function StartFrom(const Value: string): TVkParamsNewsfeedGetComments;
    /// <summary>
    /// ������������� �������, ����������� � �������� �������� ���������� �������,
    /// �������� wall1_45486. ���� ������ ������ ��������, �������� Filters ��������� �������������
    /// </summary>
    function Reposts(const Value: string): TVkParamsNewsfeedGetComments;
  end;

  TVkParamsNewsfeedGetMentions = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������
    /// </summary>
    function OwnerId(const Value: Integer): TVkParamsNewsfeedGetMentions;
    /// <summary>
    /// ���������� ������������ �������. ������������ �������� ��������� 50
    /// </summary>
    function Count(const Value: Integer = 20): TVkParamsNewsfeedGetMentions;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ��������
    /// </summary>
    function Offset(const Value: Integer = 0): TVkParamsNewsfeedGetMentions;
    /// <summary>
    /// �����, ������� � �������� ������� �������� ���������� � ������������.
    /// ���� �������� �� �����, �� ����� ���������� ��� ���������� � ������������,
    /// ���� �� ����� �������� EndTime, � ��������� ������ ���������� � ������ ��������� EndTime
    /// </summary>
    function StartTime(const Value: TDateTime): TVkParamsNewsfeedGetMentions;
    /// <summary>
    /// �����, � ������� unixtime, �� �������� ������� �������� ���������� � ������������.
    /// ���� �������� �� �����, �� ����� ���������� ��� ���������� � ������������,
    /// ���� �� ����� �������� StartTime, � ��������� ������ ���������� � ������ ��������� StartTime
    /// </summary>
    function EndTime(const Value: TDateTime): TVkParamsNewsfeedGetMentions;
  end;

  TVkParamsNewsfeedGetRecommended = record
    List: TParams;
    /// <summary>
    /// ������ �������������� ����� ��������, ������� ���������� �������
    /// </summary>
    function Fields(const Value: TVkProfileFields = []): TVkParamsNewsfeedGetRecommended;
    /// <summary>
    /// ���������, ����� ������������ ����� �������� ������� ����������, �� �� ����� 100
    /// </summary>
    function Count(const Value: Integer = 50): TVkParamsNewsfeedGetRecommended;
    /// <summary>
    /// �����, ������� � �������� ������� �������� ������� ��� �������� ������������.
    /// ���� �������� �� �����, �� �� ��������� ������ �������� �������, ������� ���� ����� �����
    /// </summary>
    function StartTime(const Value: TDateTime): TVkParamsNewsfeedGetRecommended;
    /// <summary>
    /// �����, �� �������� ������� �������� ������� ��� �������� ������������.
    /// ���� �������� �� �����, �� �� ��������� ������ �������� �������
    /// </summary>
    function EndTime(const Value: TDateTime): TVkParamsNewsfeedGetRecommended;
    /// <summary>
    /// �������������, ����������� ��� ��������� ��������� �������� �����������.
    /// ��������, ����������� ��� �������� � ���� ���������, ������������ � ���� ������ NextFrom
    /// </summary>
    function StartFrom(const Value: string): TVkParamsNewsfeedGetRecommended;
    /// <summary>
    /// ������������ ���������� ����������, ���������� � ������� ���������� �������
    /// </summary>
    function MaxPhotos(const Value: Integer = 5): TVkParamsNewsfeedGetRecommended;
  end;

  TVkParamsNewsfeedSearch = record
    List: TParams;
    /// <summary>
    /// ��������� ������
    /// </summary>
    function Query(const Value: string): TVkParamsNewsfeedSearch;
    /// <summary>
    /// ������ �������������� ����� ��� �������� � �����, ������� ���������� �������
    /// </summary>
    function Fields(const GroupFields: TVkGroupFields = []; UserFields: TVkProfileFields = []): TVkParamsNewsfeedSearch;
    /// <summary>
    /// ���������, ����� ������������ ����� ������� ������� ����������.
    /// �������� �������� � ���� ��� ������������� ��������� Offset ��� ��������� ����������
    /// �������� ������ ������ 1000 ����������� (������������ �������� 200)
    /// </summary>
    function Count(const Value: Integer = 30): TVkParamsNewsfeedSearch;
    /// <summary>
    /// �����, ������� � �������� ������� �������� ������� ��� �������� ������������.
    /// ���� �������� �� �����, �� �� ��������� ������ �������� �������, ������� ���� ����� �����
    /// </summary>
    function StartTime(const Value: TDateTime): TVkParamsNewsfeedSearch;
    /// <summary>
    /// �����, �� �������� ������� �������� ������� ��� �������� ������������.
    /// ���� �������� �� �����, �� �� ��������� ������ �������� �������
    /// </summary>
    function EndTime(const Value: TDateTime): TVkParamsNewsfeedSearch;
    /// <summary>
    /// �������������, ����������� ��� ��������� ��������� �������� �����������.
    /// ��������, ����������� ��� �������� � ���� ���������, ������������ � ���� ������ NextFrom
    /// </summary>
    function StartFrom(const Value: string): TVkParamsNewsfeedSearch;
    /// <summary>
    /// �������������� ������ �����, � ������� �� ������� ���������� ����������� �����, �������� � �������� (�� -90 �� 90)
    /// </summary>
    function Latitude(const Value: Extended): TVkParamsNewsfeedSearch;
    /// <summary>
    /// �������������� ������� �����, � ������� �� ������� ���������� ����������� �����, �������� � �������� (�� -180 �� 180)
    /// </summary>
    function Longitude(const Value: Extended): TVkParamsNewsfeedSearch;
    /// <summary>
    /// True, ���� ���������� �������� ���������� � ������������ ��� ����������, ������������ ������
    /// </summary>
    function Extended(const Value: Boolean = False): TVkParamsNewsfeedSearch;
  end;

  TNewsfeedController = class(TVkController)
  public
    /// <summary>
    /// ��������� ���������� ������� �� �������� ������������� � ����� � ����� �������� �������� ������������.
    /// </summary>
    function AddBan(UserIds: TIdList = []; GroupIds: TIdList = []): Boolean;
    /// <summary>
    /// ��������� ���������� ������� �� �������� ������������� � ����� � ����� �������� �������� ������������.
    /// </summary>
    function DeleteBan(UserIds: TIdList = []; GroupIds: TIdList = []): Boolean;
    /// <summary>
    /// ����� ��������� ������� ���������������� ������ ��������.
    /// </summary>
    function DeleteList(ListId: Integer): Boolean;
    /// <summary>
    /// ���������� ������, ����������� ��� ������ ������ �������� ��� �������� ������������.
    /// </summary>
    function Get(var Items: TVkNews; Params: TParams = []): Boolean; overload;
    /// <summary>
    /// ���������� ������, ����������� ��� ������ ������ �������� ��� �������� ������������.
    /// </summary>
    function Get(var Items: TVkNews; Params: TVkParamsNewsfeedGet): Boolean; overload;
    /// <summary>
    /// ���������� ������ ������������� � �����, ������� ������� ������������ ����� �� ����� ��������.
    /// </summary>
    function GetBanned(var Items: TVkNewsfeedBannedIds; Params: TVkParamsNewsfeedBanned): Boolean; overload;
    /// <summary>
    /// ���������� ������ ������������� � �����, ������� ������� ������������ ����� �� ����� ��������.
    /// </summary>
    function GetBanned(var Items: TVkNewsfeedBanned; Params: TVkParamsNewsfeedBanned): Boolean; overload;
    /// <summary>
    /// ���������� ������, ����������� ��� ������ ������� ������������ � �������� ������������.
    /// </summary>
    function GetComments(var Items: TVkNews; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ������, ����������� ��� ������ ������� ������������ � �������� ������������.
    /// </summary>
    function GetComments(var Items: TVkNews; Params: TVkParamsNewsfeedGetComments): Boolean; overload;
    /// <summary>
    /// ���������� ���������������� ������ ��������.
    /// </summary>
    function GetLists(var Items: TVkNewsfeedLists; ListIds: TIdList; Extended: Boolean = False): Boolean; overload;
    /// <summary>
    /// ���������� ������ ������� ������������� �� ����� ������, � ������� ����������� ��������� ������������.
    /// </summary>
    function GetMentions(var Items: TVkPosts; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ������ ������� ������������� �� ����� ������, � ������� ����������� ��������� ������������.
    /// </summary>
    function GetMentions(var Items: TVkPosts; Params: TVkParamsNewsfeedGetMentions): Boolean; overload;
    /// <summary>
    /// �������� ������ ��������, ��������������� ������������.
    /// </summary>
    function GetRecommended(var Items: TVkNews; Params: TVkParamsNewsfeedGetRecommended): Boolean;
    /// <summary>
    /// ���������� ���������� � �������������, �� ������� �������� ������������ ������������� �����������.
    /// </summary>
    function GetSuggestedSources(var Items: TVkSuggestedList; Shuffle: Boolean = False; Count: Integer = 20; Offset: Integer = 0): Boolean;
    /// <summary>
    /// ��������� ������ ������ �� ����� ��������.
    /// </summary>
    function IgnoreItem(ItemType: TVkNewsfeedIgnoreType; OwnerId, ItemId: Integer): Boolean;
    /// <summary>
    /// ����� ��������� ��������� ��� ������������� ���������������� ������ ��� ��������� ��������.
    /// </summary>
    function SaveList(var ListId: Integer; Title: string; SourceIds: TIdList; NoReposts: Boolean = False): Boolean; overload;
    /// <summary>
    /// ����� ��������� ��������� ��� ������������� ���������������� ������ ��� ��������� ��������.
    /// </summary>
    function SaveList(const ListId: Integer; SourceIds: TIdList; NoReposts: Boolean = False): Boolean; overload;
    /// <summary>
    /// ���������� ���������� ������ �� ��������. ������� ������������ � ������� �� ����� ����� � ����� ������.
    /// </summary>
    function Search(var Items: TVkNews; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ���������� ������ �� ��������. ������� ������������ � ������� �� ����� ����� � ����� ������.
    /// </summary>
    function Search(var Items: TVkNews; Params: TVkParamsNewsfeedSearch): Boolean; overload;
    /// <summary>
    /// ��������� ������� ����� ������� ������ � ����� ��������.
    /// </summary>
    function UnignoreItem(ItemType: TVkNewsfeedIgnoreType; OwnerId, ItemId: Integer; TrackCode: string = ''): Boolean;
    /// <summary>
    /// ���������� �������� ������������ �� ������������ � ��������� �������.
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
  {TODO -oHemulGM -cGeneral : ��� ������������ �������, ������� ����������� �������� �����, �.�. � �� ������-�� ��� ��������� ���� ��� ������ ���������� ����� ����������}
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

function TVkParamsNewsfeedGet.Count(const Value: Integer): TVkParamsNewsfeedGet;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsNewsfeedGet.EndTime(const Value: TDateTime): TVkParamsNewsfeedGet;
begin
  List.Add('end_time', Value);
  Result := Self;
end;

function TVkParamsNewsfeedGet.Fields(const GroupFields: TVkGroupFields; UserFields: TVkProfileFields): TVkParamsNewsfeedGet;
begin
  List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
  Result := Self;
end;

function TVkParamsNewsfeedGet.Filters(const Value: TVkNewsfeedTypes): TVkParamsNewsfeedGet;
begin
  List.Add('filters', Value.ToString);
  Result := Self;
end;

function TVkParamsNewsfeedGet.MaxPhotos(const Value: Integer): TVkParamsNewsfeedGet;
begin
  List.Add('max_photos', Value);
  Result := Self;
end;

function TVkParamsNewsfeedGet.ReturnBanned(const Value: Boolean): TVkParamsNewsfeedGet;
begin
  List.Add('return_banned', Value);
  Result := Self;
end;

function TVkParamsNewsfeedGet.Section(const Value: string): TVkParamsNewsfeedGet;
begin
  List.Add('section', Value);
  Result := Self;
end;

function TVkParamsNewsfeedGet.SourceIds(const Value: TArrayOfString): TVkParamsNewsfeedGet;
begin
  List.Add('source_ids', Value);
  Result := Self;
end;

function TVkParamsNewsfeedGet.StartFrom(const Value: string): TVkParamsNewsfeedGet;
begin
  List.Add('start_from', Value);
  Result := Self;
end;

function TVkParamsNewsfeedGet.StartTime(const Value: TDateTime): TVkParamsNewsfeedGet;
begin
  List.Add('start_time', Value);
  Result := Self;
end;

{ TVkParamsNewsfeedBanned }

function TVkParamsNewsfeedBanned.Extended(const Value: Boolean): TVkParamsNewsfeedBanned;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsNewsfeedBanned.Fields(const GroupFields: TVkGroupFields; UserFields: TVkProfileFields): TVkParamsNewsfeedBanned;
begin
  List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
  Result := Self;
end;

function TVkParamsNewsfeedBanned.NameCase(const Value: TVkNameCase): TVkParamsNewsfeedBanned;
begin
  List.Add('name_case', Value.ToString);
  Result := Self;
end;

{ TVkParamsNewsfeedGetComments }

function TVkParamsNewsfeedGetComments.Count(const Value: Integer): TVkParamsNewsfeedGetComments;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsNewsfeedGetComments.EndTime(const Value: TDateTime): TVkParamsNewsfeedGetComments;
begin
  List.Add('end_time', Value);
  Result := Self;
end;

function TVkParamsNewsfeedGetComments.Fields(const Value: TVkProfileFields): TVkParamsNewsfeedGetComments;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsNewsfeedGetComments.Filters(const Value: TVkNewsfeedCommentsTypes): TVkParamsNewsfeedGetComments;
begin
  List.Add('filters', Value.ToString);
  Result := Self;
end;

function TVkParamsNewsfeedGetComments.LastCommentsCount(const Value: Integer): TVkParamsNewsfeedGetComments;
begin
  List.Add('last_comments_count', Value);
  Result := Self;
end;

function TVkParamsNewsfeedGetComments.Reposts(const Value: string): TVkParamsNewsfeedGetComments;
begin
  List.Add('reposts', Value);
  Result := Self;
end;

function TVkParamsNewsfeedGetComments.StartFrom(const Value: string): TVkParamsNewsfeedGetComments;
begin
  List.Add('start_from', Value);
  Result := Self;
end;

function TVkParamsNewsfeedGetComments.StartTime(const Value: TDateTime): TVkParamsNewsfeedGetComments;
begin
  List.Add('start_time', Value);
  Result := Self;
end;

{ TVkParamsNewsfeedGetMentions }

function TVkParamsNewsfeedGetMentions.Count(const Value: Integer): TVkParamsNewsfeedGetMentions;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsNewsfeedGetMentions.EndTime(const Value: TDateTime): TVkParamsNewsfeedGetMentions;
begin
  List.Add('end_time', Value);
  Result := Self;
end;

function TVkParamsNewsfeedGetMentions.Offset(const Value: Integer): TVkParamsNewsfeedGetMentions;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsNewsfeedGetMentions.OwnerId(const Value: Integer): TVkParamsNewsfeedGetMentions;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsNewsfeedGetMentions.StartTime(const Value: TDateTime): TVkParamsNewsfeedGetMentions;
begin
  List.Add('start_time', Value);
  Result := Self;
end;

{ TVkParamsNewsfeedGetRecommended }

function TVkParamsNewsfeedGetRecommended.Count(const Value: Integer): TVkParamsNewsfeedGetRecommended;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsNewsfeedGetRecommended.EndTime(const Value: TDateTime): TVkParamsNewsfeedGetRecommended;
begin
  List.Add('end_time', Value);
  Result := Self;
end;

function TVkParamsNewsfeedGetRecommended.Fields(const Value: TVkProfileFields): TVkParamsNewsfeedGetRecommended;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsNewsfeedGetRecommended.MaxPhotos(const Value: Integer): TVkParamsNewsfeedGetRecommended;
begin
  List.Add('max_photos', Value);
  Result := Self;
end;

function TVkParamsNewsfeedGetRecommended.StartFrom(const Value: string): TVkParamsNewsfeedGetRecommended;
begin
  List.Add('start_from', Value);
  Result := Self;
end;

function TVkParamsNewsfeedGetRecommended.StartTime(const Value: TDateTime): TVkParamsNewsfeedGetRecommended;
begin
  List.Add('start_time', Value);
  Result := Self;
end;

{ TVkParamsNewsfeedSearch }

function TVkParamsNewsfeedSearch.Count(const Value: Integer): TVkParamsNewsfeedSearch;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsNewsfeedSearch.EndTime(const Value: TDateTime): TVkParamsNewsfeedSearch;
begin
  List.Add('end_time', Value);
  Result := Self;
end;

function TVkParamsNewsfeedSearch.Extended(const Value: Boolean): TVkParamsNewsfeedSearch;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsNewsfeedSearch.Fields(const GroupFields: TVkGroupFields; UserFields: TVkProfileFields): TVkParamsNewsfeedSearch;
begin
  List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
  Result := Self;
end;

function TVkParamsNewsfeedSearch.Latitude(const Value: Extended): TVkParamsNewsfeedSearch;
begin
  List.Add('latitude', Value);
  Result := Self;
end;

function TVkParamsNewsfeedSearch.Longitude(const Value: Extended): TVkParamsNewsfeedSearch;
begin
  List.Add('longitude', Value);
  Result := Self;
end;

function TVkParamsNewsfeedSearch.Query(const Value: string): TVkParamsNewsfeedSearch;
begin
  List.Add('q', Value);
  Result := Self;
end;

function TVkParamsNewsfeedSearch.StartFrom(const Value: string): TVkParamsNewsfeedSearch;
begin
  List.Add('start_from', Value);
  Result := Self;
end;

function TVkParamsNewsfeedSearch.StartTime(const Value: TDateTime): TVkParamsNewsfeedSearch;
begin
  List.Add('start_time', Value);
  Result := Self;
end;

end.

