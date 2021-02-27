unit VK.Friends;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.Profile,
  VK.Entity.Common, VK.Entity.Common.List;

type
  TVkParamsFriendsGet = record
    List: TParams;
    function UserId(const Value: Integer): Integer;
    function ListId(const Value: Integer): Integer;
    function Order(const Value: TVkFriendsOrder): Integer;
    function Count(const Value: Integer): Integer;
    function Offset(const Value: Integer): Integer;
    function Fields(const Value: TVkProfileFields): Integer;
    function NameCase(const Value: TVkNameCase): Integer;
    function Ref(const Value: string): Integer;
  end;

  TVkParamsFriendsListEdit = record
    List: TParams;
    function Name(const Value: string): Integer;
    function ListId(const Value: Integer): Integer;
    function UserIds(const Value: TIdList): Integer;
    function AddUserIds(const Value: TIdList): Integer;
    function DeleteUserIds(const Value: TIdList): Integer;
  end;

  TVkParamsFriendsGetMutual = record
    List: TParams;
    function SourceUid(const Value: Integer): Integer;
    function TargetUid(const Value: Integer): Integer;
    function TargetUids(const Value: TIdList): Integer;
    function OrderRandom(const Value: Boolean): Integer;
    function Count(const Value: Integer): Integer;
    function Offset(const Value: Integer): Integer;
  end;

  TVkParamsFriendsGetOnline = record
    List: TParams;
    function UserId(const Value: Integer): Integer;
    function ListId(const Value: Integer): Integer;
    function OnlineMobile(const Value: Boolean): Integer;
    function Order(const Value: TVkFriendsOrder): Integer;
    function Count(const Value: Integer): Integer;
    function Offset(const Value: Integer): Integer;
  end;

  TVkParamsFriendsGetRequests = record
    List: TParams;
    function Count(const Value: Integer): Integer;
    function Offset(const Value: Integer): Integer;
    function NeedMutual(const Value: Boolean): Integer;
    function &Out(const Value: Boolean = False): Integer;
    function Sort(const Value: Boolean): Integer;
    function NeedViewed(const Value: Boolean): Integer;
    function Suggested(const Value: Boolean): Integer;
    function Fields(const Value: TVkProfileFields): Integer;
  end;

  TVkParamsFriendsGetSuggestions = record
    List: TParams;
    function Count(const Value: Integer): Integer;
    function Offset(const Value: Integer): Integer;
    function Fields(const Value: TVkProfileFields): Integer;
    function FilterMutual(const Value: Boolean): Integer;
    function NameCase(const Value: TVkNameCase): Integer;
  end;

  TVkParamsFriendsSearch = record
    List: TParams;
    function UserId(const Value: Integer): Integer;
    function Query(const Value: string): Integer;
    function Count(const Value: Integer): Integer;
    function Offset(const Value: Integer): Integer;
    function Fields(const Value: TVkProfileFields): Integer;
    function NameCase(const Value: TVkNameCase): Integer;
  end;

  TFriendsController = class(TVkController)
  public
    /// <summary>
    /// Возвращает список идентификаторов друзей пользователя или расширенную информацию о друзьях пользователя (при использовании параметра fields).
    /// </summary>
    function Get(var Items: TVkProfiles; UserId: Integer; Fields: TVkProfileFields = []; Order: TVkFriendsOrder = TVkFriendsOrder.None): Boolean; overload;
    /// <summary>
    /// Возвращает список идентификаторов друзей пользователя или расширенную информацию о друзьях пользователя (при использовании параметра fields).
    /// </summary>
    function Get(var Items: TVkProfiles; Fields: TVkProfileFields = []; Order: TVkFriendsOrder = TVkFriendsOrder.None): Boolean; overload;
    /// <summary>
    /// Возвращает список идентификаторов друзей пользователя или расширенную информацию о друзьях пользователя (при использовании параметра fields).
    /// </summary>
    function Get(var Items: TVkProfiles; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список идентификаторов друзей пользователя или расширенную информацию о друзьях пользователя (при использовании параметра fields).
    /// </summary>
    function Get(var Items: TVkProfiles; Params: TVkParamsFriendsGet): Boolean; overload;
    /// <summary>
    /// Возвращает список идентификаторов друзей пользователя или расширенную информацию о друзьях пользователя (при использовании параметра fields).
    /// </summary>
    function GetIds(var Items: TVkIdList; Params: TVkParamsFriendsGet): Boolean; overload;
    /// <summary>
    /// Одобряет или создает заявку на добавление в друзья.
    /// </summary>
    function Add(var Info: TVkFriendAddInfo; UserId: Integer; Text: string; Follow: Boolean = False): Boolean;
    /// <summary>
    /// Создает новый список друзей у текущего пользователя.
    /// </summary>
    function AddList(var ListId: Integer; Name: string; UserIds: TIdList): Boolean;
    /// <summary>
    /// Возвращает информацию о том, добавлен ли текущий пользователь в друзья у указанных пользователей.
    /// </summary>
    function AreFriends(var Items: TVkFriendInfo; UserIds: TIdList; NeedSign: Boolean; Extended: Boolean): Boolean;
    /// <summary>
    /// Удаляет пользователя из списка друзей или отклоняет заявку в друзья.
    /// </summary>
    function Delete(var Info: TVkFriendDeleteInfo; UserId: Integer): Boolean; overload;
    /// <summary>
    /// Удаляет пользователя из списка друзей или отклоняет заявку в друзья.
    /// </summary>
    function Delete(UserId: Integer): Boolean; overload;
    /// <summary>
    /// Отмечает все входящие заявки на добавление в друзья как просмотренные.
    /// </summary>
    function DeleteAllRequests: Boolean;
    /// <summary>
    /// Удаляет существующий список друзей текущего пользователя.
    /// </summary>
    function DeleteList(ListId: Integer): Boolean;
    /// <summary>
    /// Редактирует списки друзей для выбранного друга.
    /// </summary>
    function Edit(UserId: Integer; ListIds: TIdList): Boolean;
    /// <summary>
    /// Редактирует списки друзей для выбранного друга.
    /// </summary>
    function EditList(Params: TParams): Boolean; overload;
    /// <summary>
    /// Редактирует списки друзей для выбранного друга.
    /// </summary>
    function EditList(Params: TVkParamsFriendsListEdit): Boolean; overload;
    /// <summary>
    /// Возвращает список идентификаторов друзей текущего пользователя, которые установили данное приложение.
    /// </summary>
    function GetAppUsers(var Items: TVkIdList): Boolean;
    /// <summary>
    /// Возвращает список друзей пользователя, у которых завалидированные или указанные в профиле телефонные номера входят в заданный список.
    /// </summary>
    function GetByPhones(var Items: TVkProfiles; Phones: TArrayOfString; Fields: TVkProfileFields): Boolean;
    /// <summary>
    /// Возвращает список меток друзей пользователя.
    /// </summary>
    function GetLists(var Items: TVkFriendsList; UserId: Integer; ReturnSystem: Boolean = False): Boolean; overload;
    /// <summary>
    /// Возвращает список идентификаторов общих друзей между парой пользователей.
    /// </summary>
    function GetMutual(var Items: TVkIdList; Params: TVkParamsFriendsGetMutual): Boolean;
    /// <summary>
    /// Возвращает список идентификаторов друзей пользователя, находящихся на сайте.
    /// </summary>
    function GetOnline(var Items: TVkFriendsOnline; Params: TVkParamsFriendsGetOnline): Boolean;
    /// <summary>
    /// Возвращает список идентификаторов недавно добавленных друзей текущего пользователя.
    /// </summary>
    function GetRecent(var Items: TVkIdList; Count: Integer = 100): Boolean;
    /// <summary>
    /// Возвращает информацию о полученных или отправленных заявках на добавление в друзья для текущего пользователя.
    /// </summary>
    function GetRequests(var Items: TVkProfiles; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает информацию о полученных или отправленных заявках на добавление в друзья для текущего пользователя.
    /// </summary>
    function GetRequests(var Items: TVkProfiles; Params: TVkParamsFriendsGetRequests): Boolean; overload;
    /// <summary>
    /// Возвращает информацию о полученных или отправленных заявках на добавление в друзья для текущего пользователя.
    /// </summary>
    function GetRequestsIds(var Items: TVkIdList; Params: TVkParamsFriendsGetRequests): Boolean; overload;
    /// <summary>
    /// Возвращает список профилей пользователей, которые могут быть друзьями текущего пользователя.
    /// </summary>
    function GetSuggestions(var Items: TVkProfiles; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список профилей пользователей, которые могут быть друзьями текущего пользователя.
    /// </summary>
    function GetSuggestions(var Items: TVkProfiles; Params: TVkParamsFriendsGetSuggestions): Boolean; overload;
    /// <summary>
    /// Позволяет искать по списку друзей пользователей.
    /// </summary>
    function Search(var Items: TVkProfiles; Params: TParams): Boolean; overload;
    /// <summary>
    /// Позволяет искать по списку друзей пользователей.
    /// </summary>
    function Search(var Items: TVkProfiles; Params: TVkParamsFriendsSearch): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TFriendsController }

function TFriendsController.Get(var Items: TVkProfiles; Fields: TVkProfileFields; Order: TVkFriendsOrder): Boolean;
var
  Params: TVkParamsFriendsGet;
begin
  if Fields <> [] then
    Params.Fields(Fields);
  if Order <> TVkFriendsOrder.None then
    Params.Order(Order);
  Result := Get(Items, Params);
end;

function TFriendsController.Get(var Items: TVkProfiles; UserId: Integer; Fields: TVkProfileFields; Order: TVkFriendsOrder): Boolean;
var
  Params: TVkParamsFriendsGet;
begin
  Params.UserId(UserId);
  if Fields <> [] then
    Params.Fields(Fields);
  if Order <> TVkFriendsOrder.None then
    Params.Order(Order);
  Result := Get(Items, Params);
end;

function TFriendsController.Get(var Items: TVkProfiles; Params: TVkParamsFriendsGet): Boolean;
begin
  Result := Get(Items, Params.List);
end;

function TFriendsController.Get(var Items: TVkProfiles; Params: TParams): Boolean;
begin
  if not Params.KeyExists('fields') then
    Params.Add('fields', 'domian');

  Result := Handler.Execute('friends.get', Params).GetObject(Items);
end;

function TFriendsController.Add(var Info: TVkFriendAddInfo; UserId: Integer; Text: string; Follow: Boolean): Boolean;
begin
  with Handler.Execute('friends.add', [['user_id', UserId.ToString], ['text', Text], ['follow', BoolToString(Follow)]]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Info := TVkFriendAddInfo(StrToInt(Response));
      except
        Result := False;
      end;
    end;
  end;
end;

function TFriendsController.AddList(var ListId: Integer; Name: string; UserIds: TIdList): Boolean;
begin
  Result := Handler.Execute('friends.add', [['name', Name], ['user_ids', UserIds.ToString]]).ResponseAsInt(ListId);
end;

function TFriendsController.AreFriends(var Items: TVkFriendInfo; UserIds: TIdList; NeedSign, Extended: Boolean): Boolean;
begin
  Result := Handler.Execute('friends.areFriends', [
    ['user_ids', UserIds.ToString],
    ['need_sign', BoolToString(NeedSign)],
    ['extended', BoolToString(Extended)]]).
    GetObjects(Items);
end;

function TFriendsController.Delete(UserId: Integer): Boolean;
var
  Info: TVkFriendDeleteInfo;
begin
  with Handler.Execute('friends.delete', ['user_id', UserId.ToString]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Info := TVkFriendDeleteInfo.FromJsonString<TVkFriendDeleteInfo>(Response);
        try
          Result := Info.Success;
        finally
          Info.Free;
        end;
      except
        Result := False;
      end;
    end;
  end;
end;

function TFriendsController.DeleteAllRequests: Boolean;
begin
  Result := Handler.Execute('friends.deleteAllRequests').ResponseIsTrue;
end;

function TFriendsController.DeleteList(ListId: Integer): Boolean;
begin
  Result := Handler.Execute('friends.deleteList', ['list_id', ListId.ToString]).ResponseIsTrue;
end;

function TFriendsController.Edit(UserId: Integer; ListIds: TIdList): Boolean;
begin
  Result := Handler.Execute('friends.edit', [
    ['user_id', UserId.ToString],
    ['list_ids', ListIds.ToString]]).
    ResponseIsTrue;
end;

function TFriendsController.EditList(Params: TVkParamsFriendsListEdit): Boolean;
begin
  Result := EditList(Params.List);
end;

function TFriendsController.EditList(Params: TParams): Boolean;
begin
  Result := Handler.Execute('friends.editList', Params).ResponseIsTrue;
end;

function TFriendsController.Delete(var Info: TVkFriendDeleteInfo; UserId: Integer): Boolean;
begin
  Result := Handler.Execute('friends.delete', ['user_id', UserId.ToString]).GetObject(Info);
end;

function TFriendsController.GetAppUsers(var Items: TVkIdList): Boolean;
begin
  Result := Handler.Execute('friends.getAppUsers').GetObject(Items);
end;

function TFriendsController.GetByPhones(var Items: TVkProfiles; Phones: TArrayOfString; Fields: TVkProfileFields): Boolean;
begin
  with Handler.Execute('friends.getByPhones', [['phones', Phones.ToString], ['fields', Fields.ToString]]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkProfiles.FromJsonString<TVkProfiles>(ResponseAsItems);
        Items.Count := Length(Items.Items);
      except
        Result := False;
      end;
    end;
  end;
end;

function TFriendsController.GetIds(var Items: TVkIdList; Params: TVkParamsFriendsGet): Boolean;
begin
  Result := Handler.Execute('friends.get', Params.List).GetObject(Items);
end;

function TFriendsController.GetLists(var Items: TVkFriendsList; UserId: Integer; ReturnSystem: Boolean): Boolean;
begin
  Result := Handler.Execute('friends.getLists', [
    ['user_id', UserId.ToString],
    ['return_system', BoolToString(ReturnSystem)]]).
    GetObject(Items);
end;

function TFriendsController.GetMutual(var Items: TVkIdList; Params: TVkParamsFriendsGetMutual): Boolean;
begin
  Result := Handler.Execute('friends.getMutual').GetObject(Items);
end;

function TFriendsController.GetOnline(var Items: TVkFriendsOnline; Params: TVkParamsFriendsGetOnline): Boolean;
begin
  Result := Handler.Execute('friends.getOnline', Params.List).GetObject(Items);
end;

function TFriendsController.GetRecent(var Items: TVkIdList; Count: Integer): Boolean;
begin
  Result := Handler.Execute('friends.getRecent', ['count', Count.ToString]).GetObject(Items);
end;

function TFriendsController.GetRequests(var Items: TVkProfiles; Params: TVkParamsFriendsGetRequests): Boolean;
begin
  Result := GetRequests(Items, Params.List);
end;

function TFriendsController.GetRequestsIds(var Items: TVkIdList; Params: TVkParamsFriendsGetRequests): Boolean;
begin
  Result := Handler.Execute('friends.getRequests', Params.List).GetObject(Items);
end;

function TFriendsController.GetSuggestions(var Items: TVkProfiles; Params: TVkParamsFriendsGetSuggestions): Boolean;
begin
  Result := GetSuggestions(Items, Params.List);
end;

function TFriendsController.Search(var Items: TVkProfiles; Params: TVkParamsFriendsSearch): Boolean;
begin
  Result := Search(Items, Params.List);
end;

function TFriendsController.Search(var Items: TVkProfiles; Params: TParams): Boolean;
begin
  Result := Handler.Execute('friends.search', Params).GetObject(Items);
end;

function TFriendsController.GetSuggestions(var Items: TVkProfiles; Params: TParams): Boolean;
begin
  Result := Handler.Execute('friends.getSuggestions', Params).GetObject(Items);
end;

function TFriendsController.GetRequests(var Items: TVkProfiles; Params: TParams): Boolean;
begin
  Params.Add('extended', True);
  Result := Handler.Execute('friends.getRequests', Params).GetObject(Items);
end;

{ TVkFriendsGetParams }

function TVkParamsFriendsGet.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsFriendsGet.Fields(const Value: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsFriendsGet.ListId(const Value: Integer): Integer;
begin
  Result := List.Add('list_id', Value);
end;

function TVkParamsFriendsGet.NameCase(const Value: TVkNameCase): Integer;
begin
  Result := List.Add('name_case', Value.ToString);
end;

function TVkParamsFriendsGet.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsFriendsGet.Order(const Value: TVkFriendsOrder): Integer;
begin
  Result := List.Add('order', Value.ToString);
end;

function TVkParamsFriendsGet.Ref(const Value: string): Integer;
begin
  Result := List.Add('ref', Value);
end;

function TVkParamsFriendsGet.UserId(const Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkParamsFriendsListEdit }

function TVkParamsFriendsListEdit.AddUserIds(const Value: TIdList): Integer;
begin
  Result := List.Add('add_user_ids', Value);
end;

function TVkParamsFriendsListEdit.DeleteUserIds(const Value: TIdList): Integer;
begin
  Result := List.Add('delete_user_ids', Value);
end;

function TVkParamsFriendsListEdit.ListId(const Value: Integer): Integer;
begin
  Result := List.Add('list_id', Value);
end;

function TVkParamsFriendsListEdit.Name(const Value: string): Integer;
begin
  Result := List.Add('name', Value);
end;

function TVkParamsFriendsListEdit.UserIds(const Value: TIdList): Integer;
begin
  Result := List.Add('user_ids', Value);
end;

{ TVkParamsFriendsGetMutual }

function TVkParamsFriendsGetMutual.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsFriendsGetMutual.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsFriendsGetMutual.OrderRandom(const Value: Boolean): Integer;
begin
  if Value then
    Result := List.Add('order', 'random')
  else
    Result := List.Add('order', '');
end;

function TVkParamsFriendsGetMutual.SourceUid(const Value: Integer): Integer;
begin
  Result := List.Add('source_uid', Value);
end;

function TVkParamsFriendsGetMutual.TargetUid(const Value: Integer): Integer;
begin
  Result := List.Add('target_uids', Value);
end;

function TVkParamsFriendsGetMutual.TargetUids(const Value: TIdList): Integer;
begin
  Result := List.Add('target_uids', Value);
end;

{ TVkParamsFriendsGetOnline }

function TVkParamsFriendsGetOnline.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsFriendsGetOnline.ListId(const Value: Integer): Integer;
begin
  Result := List.Add('list_id', Value);
end;

function TVkParamsFriendsGetOnline.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsFriendsGetOnline.OnlineMobile(const Value: Boolean): Integer;
begin
  Result := List.Add('online_mobile', Value);
end;

function TVkParamsFriendsGetOnline.Order(const Value: TVkFriendsOrder): Integer;
begin
  Result := List.Add('order', Value.ToString);
end;

function TVkParamsFriendsGetOnline.UserId(const Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkParamsFriendsGetRequests }

function TVkParamsFriendsGetRequests.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsFriendsGetRequests.Fields(const Value: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsFriendsGetRequests.NeedMutual(const Value: Boolean): Integer;
begin
  Result := List.Add('need_mutual', Value);
end;

function TVkParamsFriendsGetRequests.NeedViewed(const Value: Boolean): Integer;
begin
  Result := List.Add('need_viewed', Value);
end;

function TVkParamsFriendsGetRequests.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsFriendsGetRequests.out(const Value: Boolean): Integer;
begin
  Result := List.Add('out', Value);
end;

function TVkParamsFriendsGetRequests.Sort(const Value: Boolean): Integer;
begin
  Result := List.Add('sort', Value);
end;

function TVkParamsFriendsGetRequests.Suggested(const Value: Boolean): Integer;
begin
  Result := List.Add('suggested', Value);
end;

{ TVkParamsFriendsGetSuggestions }

function TVkParamsFriendsGetSuggestions.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsFriendsGetSuggestions.Fields(const Value: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsFriendsGetSuggestions.FilterMutual(const Value: Boolean): Integer;
begin
  if Value then
    Result := List.Add('filter', 'mutual')
  else
    Result := List.Add('filter', '');
end;

function TVkParamsFriendsGetSuggestions.NameCase(const Value: TVkNameCase): Integer;
begin
  Result := List.Add('name_case', Value.ToString);
end;

function TVkParamsFriendsGetSuggestions.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

{ TVkParamsFriendsSearch }

function TVkParamsFriendsSearch.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsFriendsSearch.Fields(const Value: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsFriendsSearch.NameCase(const Value: TVkNameCase): Integer;
begin
  Result := List.Add('name_case', Value.ToString);
end;

function TVkParamsFriendsSearch.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsFriendsSearch.Query(const Value: string): Integer;
begin
  Result := List.Add('q', Value);
end;

function TVkParamsFriendsSearch.UserId(const Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

end.

