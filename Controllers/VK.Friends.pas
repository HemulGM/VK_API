unit VK.Friends;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.User, VK.Entity.Common;

type
  /// <summary>
  /// <b>hints</b> — сортировать по рейтингу, аналогично тому, как друзья сортируются в разделе Мои друзья (Это значение доступно только для Standalone-приложений с ключом доступа, полученным по схеме Implicit Flow.).
  /// <b>random</b> — возвращает друзей в случайном порядке.
  /// <b>mobile</b> — возвращает выше тех друзей, у которых установлены мобильные приложения.
  /// <b>name</b> — сортировать по имени. Данный тип сортировки работает медленно, так как сервер будет получать всех друзей а не только указанное количество count. (работает только при переданном параметре fields).
  /// </summary>
  TVkFriendsSort = (fsNone, fsHints, fsRandom, fsMobile, fsName);

  TVkFriendsSortHelper = record helper for TVkFriendsSort
    function ToString: string; inline;
  end;

  TVkFriendAddInfo = (faiSuccess = 1, faiApproved = 2, faiResended = 4);

  /// <summary>
  /// <b>random</b> - возвращает друзей в случайном порядке, <b>hints</b> - сортировать по рейтингу, аналогично тому, как друзья сортируются в разделе Мои друзья (данный параметр доступен только для Desktop-приложений).
  /// </summary>
  TVkFriendsOnlineOrder = (fooRandom, fooHints);

  TVkFriendsOnlineOrderHelper = record helper for TVkFriendsOnlineOrder
    function ToString: string; inline;
  end;

  TVkParamsFriendsGet = record
    List: TParams;
    function UserId(Value: Integer): Integer;
    function ListId(Value: Integer): Integer;
    function Order(Value: TVkFriendsSort): Integer;
    function Count(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function Fields(Value: TVkUserFields): Integer;
    function NameCase(Value: TVkNameCase): Integer;
    function Ref(Value: string): Integer;
  end;

  TVkParamsFriendsListEdit = record
    List: TParams;
    function Name(Value: string): Integer;
    function ListId(Value: Integer): Integer;
    function UserIds(Value: TIds): Integer;
    function AddUserIds(Value: TIds): Integer;
    function DeleteUserIds(Value: TIds): Integer;
  end;

  TVkParamsFriendsGetMutual = record
    List: TParams;
    function SourceUid(Value: Integer): Integer;
    function TargetUid(Value: Integer): Integer;
    function TargetUids(Value: TIds): Integer;
    function OrderRandom(Value: Boolean): Integer;
    function Count(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
  end;

  TVkParamsFriendsGetOnline = record
    List: TParams;
    function UserId(Value: Integer): Integer;
    function ListId(Value: Integer): Integer;
    function OnlineMobile(Value: Boolean): Integer;
    function Order(Value: TVkFriendsOnlineOrder): Integer;
    function Count(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
  end;

  TVkParamsFriendsGetRequests = record
    List: TParams;
    function Count(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function NeedMutual(Value: Boolean): Integer;
    function &Out(Value: Boolean = False): Integer;
    function Sort(Value: Boolean): Integer;
    function NeedViewed(Value: Boolean): Integer;
    function Suggested(Value: Boolean): Integer;
    function Fields(Value: TVkUserFields): Integer;
  end;

  TVkParamsFriendsGetSuggestions = record
    List: TParams;
    function Count(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function Fields(Value: TVkUserFields): Integer;
    function FilterMutual(Value: Boolean): Integer;
    function NameCase(Value: TVkNameCase): Integer;
  end;

  TVkParamsFriendsSearch = record
    List: TParams;
    function UserId(Value: Integer): Integer;
    function Query(Value: string): Integer;
    function Count(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function Fields(Value: TVkUserFields): Integer;
    function NameCase(Value: TVkNameCase): Integer;
  end;

  TFriendsController = class(TVkController)
  public
    /// <summary>
    /// Возвращает список идентификаторов друзей пользователя или расширенную информацию о друзьях пользователя (при использовании параметра fields).
    /// </summary>
    function Get(var Items: TVkUsers; UserId: Integer; Fields: TVkUserFields = []; Order: TVkFriendsSort = fsNone):
      Boolean; overload;
    /// <summary>
    /// Возвращает список идентификаторов друзей пользователя или расширенную информацию о друзьях пользователя (при использовании параметра fields).
    /// </summary>
    function Get(var Items: TVkUsers; Fields: TVkUserFields = []; Order: TVkFriendsSort = fsNone): Boolean; overload;
    /// <summary>
    /// Возвращает список идентификаторов друзей пользователя или расширенную информацию о друзьях пользователя (при использовании параметра fields).
    /// </summary>
    function Get(var Items: TVkUsers; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список идентификаторов друзей пользователя или расширенную информацию о друзьях пользователя (при использовании параметра fields).
    /// </summary>
    function Get(var Items: TVkUsers; Params: TVkParamsFriendsGet): Boolean; overload;
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
    function AddList(var ListId: Integer; Name: string; UserIds: TIds): Boolean;
    /// <summary>
    /// Возвращает информацию о том, добавлен ли текущий пользователь в друзья у указанных пользователей.
    /// </summary>
    function AreFriends(var Items: TVkFriendInfo; UserIds: TIds; NeedSign: Boolean; Extended: Boolean): Boolean;
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
    function Edit(UserId: Integer; ListIds: TIds): Boolean;
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
    function GetByPhones(var Items: TVkUsers; Phones: TArrayOfString; Fields: TVkUserFields): Boolean;
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
    function GetRequests(var Items: TVkUsers; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает информацию о полученных или отправленных заявках на добавление в друзья для текущего пользователя.
    /// </summary>
    function GetRequests(var Items: TVkUsers; Params: TVkParamsFriendsGetRequests): Boolean; overload;
    /// <summary>
    /// Возвращает информацию о полученных или отправленных заявках на добавление в друзья для текущего пользователя.
    /// </summary>
    function GetRequestsIds(var Items: TVkIdList; Params: TVkParamsFriendsGetRequests): Boolean; overload;
    /// <summary>
    /// Возвращает список профилей пользователей, которые могут быть друзьями текущего пользователя.
    /// </summary>
    function GetSuggestions(var Items: TVkUsers; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список профилей пользователей, которые могут быть друзьями текущего пользователя.
    /// </summary>
    function GetSuggestions(var Items: TVkUsers; Params: TVkParamsFriendsGetSuggestions): Boolean; overload;
    /// <summary>
    /// Позволяет искать по списку друзей пользователей.
    /// </summary>
    function Search(var Items: TVkUsers; Params: TParams): Boolean; overload;
    /// <summary>
    /// Позволяет искать по списку друзей пользователей.
    /// </summary>
    function Search(var Items: TVkUsers; Params: TVkParamsFriendsSearch): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.Json;

{ TFriendsController }

function TFriendsController.Get(var Items: TVkUsers; Fields: TVkUserFields; Order: TVkFriendsSort): Boolean;
var
  Params: TVkParamsFriendsGet;
begin
  if Fields <> [] then
    Params.Fields(Fields);
  if Order <> fsNone then
    Params.Order(Order);
  Result := Get(Items, Params);
end;

function TFriendsController.Get(var Items: TVkUsers; UserId: Integer; Fields: TVkUserFields; Order: TVkFriendsSort): Boolean;
var
  Params: TVkParamsFriendsGet;
begin
  Params.UserId(UserId);
  if Fields <> [] then
    Params.Fields(Fields);
  if Order <> fsNone then
    Params.Order(Order);
  Result := Get(Items, Params);
end;

function TFriendsController.Get(var Items: TVkUsers; Params: TVkParamsFriendsGet): Boolean;
begin
  Result := Get(Items, Params.List);
end;

function TFriendsController.Get(var Items: TVkUsers; Params: TParams): Boolean;
begin
  if not Params.KeyExists('fields') then
    Params.Add('fields', 'domian');

  with Handler.Execute('friends.get', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkUsers.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
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

function TFriendsController.AddList(var ListId: Integer; Name: string; UserIds: TIds): Boolean;
begin
  with Handler.Execute('friends.add', [['name', Name], ['user_ids', UserIds.ToString]]) do
  begin
    Result := Success and TryStrToInt(Response, ListId);
  end;
end;

function TFriendsController.AreFriends(var Items: TVkFriendInfo; UserIds: TIds; NeedSign, Extended: Boolean): Boolean;
begin
  with Handler.Execute('friends.areFriends', [['user_ids', UserIds.ToString], ['need_sign', BoolToString(NeedSign)], ['extended',
    BoolToString(Extended)]]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkFriendInfo.FromJsonString(AppendItemsTag(Response));
      except
        Result := False;
      end;
    end;
  end;
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
        Info := TVkFriendDeleteInfo.FromJsonString(Response);
        Result := Info.Success;
        Info.Free;
      except
        Result := False;
      end;
    end;
  end;
end;

function TFriendsController.DeleteAllRequests: Boolean;
begin
  with Handler.Execute('friends.deleteAllRequests') do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFriendsController.DeleteList(ListId: Integer): Boolean;
begin
  with Handler.Execute('friends.deleteList', ['list_id', ListId.ToString]) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFriendsController.Edit(UserId: Integer; ListIds: TIds): Boolean;
begin
  with Handler.Execute('friends.edit', [['user_id', UserId.ToString], ['list_ids', ListIds.ToString]]) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFriendsController.EditList(Params: TVkParamsFriendsListEdit): Boolean;
begin
  Result := EditList(Params.List);
end;

function TFriendsController.EditList(Params: TParams): Boolean;
begin
  with Handler.Execute('friends.editList', Params) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFriendsController.Delete(var Info: TVkFriendDeleteInfo; UserId: Integer): Boolean;
begin
  with Handler.Execute('friends.delete', ['user_id', UserId.ToString]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Info := TVkFriendDeleteInfo.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TFriendsController.GetAppUsers(var Items: TVkIdList): Boolean;
begin
  with Handler.Execute('friends.getAppUsers') do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkIdList.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TFriendsController.GetByPhones(var Items: TVkUsers; Phones: TArrayOfString; Fields: TVkUserFields): Boolean;
begin
  with Handler.Execute('friends.getByPhones', [['phones', Phones.ToString], ['fields', Fields.ToString]]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkUsers.FromJsonString(AppendItemsTag(Response));
        Items.Count := Length(Items.Items);
      except
        Result := False;
      end;
    end;
  end;
end;

function TFriendsController.GetIds(var Items: TVkIdList; Params: TVkParamsFriendsGet): Boolean;
begin
  with Handler.Execute('friends.get', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkIdList.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TFriendsController.GetLists(var Items: TVkFriendsList; UserId: Integer; ReturnSystem: Boolean): Boolean;
begin
  with Handler.Execute('friends.getLists', [['user_id', UserId.ToString], ['return_system', BoolToString(ReturnSystem)]]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkFriendsList.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TFriendsController.GetMutual(var Items: TVkIdList; Params: TVkParamsFriendsGetMutual): Boolean;
begin
  with Handler.Execute('friends.getMutual') do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkIdList.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TFriendsController.GetOnline(var Items: TVkFriendsOnline; Params: TVkParamsFriendsGetOnline): Boolean;
begin
  with Handler.Execute('friends.getOnline', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkFriendsOnline.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TFriendsController.GetRecent(var Items: TVkIdList; Count: Integer): Boolean;
begin
  with Handler.Execute('friends.getRecent', ['count', Count.ToString]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkIdList.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TFriendsController.GetRequests(var Items: TVkUsers; Params: TVkParamsFriendsGetRequests): Boolean;
begin
  Result := GetRequests(Items, Params.List);
end;

function TFriendsController.GetRequestsIds(var Items: TVkIdList; Params: TVkParamsFriendsGetRequests): Boolean;
begin
  with Handler.Execute('friends.getRequests', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkIdList.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TFriendsController.GetSuggestions(var Items: TVkUsers; Params: TVkParamsFriendsGetSuggestions): Boolean;
begin
  Result := GetSuggestions(Items, Params.List);
end;

function TFriendsController.Search(var Items: TVkUsers; Params: TVkParamsFriendsSearch): Boolean;
begin
  Result := Search(Items, Params.List);
end;

function TFriendsController.Search(var Items: TVkUsers; Params: TParams): Boolean;
begin
  with Handler.Execute('friends.search', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkUsers.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TFriendsController.GetSuggestions(var Items: TVkUsers; Params: TParams): Boolean;
begin
  with Handler.Execute('friends.getSuggestions', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkUsers.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TFriendsController.GetRequests(var Items: TVkUsers; Params: TParams): Boolean;
begin
  Params.Add('extended', True);
  with Handler.Execute('friends.getRequests', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkUsers.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

{ TVkFriendsGetParams }

function TVkParamsFriendsGet.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsFriendsGet.Fields(Value: TVkUserFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsFriendsGet.ListId(Value: Integer): Integer;
begin
  Result := List.Add('list_id', Value);
end;

function TVkParamsFriendsGet.NameCase(Value: TVkNameCase): Integer;
begin
  Result := List.Add('name_case', Value.ToString);
end;

function TVkParamsFriendsGet.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsFriendsGet.Order(Value: TVkFriendsSort): Integer;
begin
  Result := List.Add('order', Value.ToString);
end;

function TVkParamsFriendsGet.Ref(Value: string): Integer;
begin
  Result := List.Add('ref', Value);
end;

function TVkParamsFriendsGet.UserId(Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkFriendsSortHelper }

function TVkFriendsSortHelper.ToString: string;
begin
  case Self of
    fsHints:
      Exit('hints');
    fsRandom:
      Exit('random');
    fsMobile:
      Exit('mobile');
    fsName:
      Exit('name');
  else
    Exit('');
  end;
end;

{ TVkParamsFriendsListEdit }

function TVkParamsFriendsListEdit.AddUserIds(Value: TIds): Integer;
begin
  Result := List.Add('add_user_ids', Value);
end;

function TVkParamsFriendsListEdit.DeleteUserIds(Value: TIds): Integer;
begin
  Result := List.Add('delete_user_ids', Value);
end;

function TVkParamsFriendsListEdit.ListId(Value: Integer): Integer;
begin
  Result := List.Add('list_id', Value);
end;

function TVkParamsFriendsListEdit.Name(Value: string): Integer;
begin
  Result := List.Add('name', Value);
end;

function TVkParamsFriendsListEdit.UserIds(Value: TIds): Integer;
begin
  Result := List.Add('user_ids', Value);
end;

{ TVkParamsFriendsGetMutual }

function TVkParamsFriendsGetMutual.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsFriendsGetMutual.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsFriendsGetMutual.OrderRandom(Value: Boolean): Integer;
begin
  if Value then
    Result := List.Add('order', 'random')
  else
    Result := List.Add('order', '');
end;

function TVkParamsFriendsGetMutual.SourceUid(Value: Integer): Integer;
begin
  Result := List.Add('source_uid', Value);
end;

function TVkParamsFriendsGetMutual.TargetUid(Value: Integer): Integer;
begin
  Result := List.Add('target_uids', Value);
end;

function TVkParamsFriendsGetMutual.TargetUids(Value: TIds): Integer;
begin
  Result := List.Add('target_uids', Value);
end;

{ TVkFriendsOnlineOrderHelper }

function TVkFriendsOnlineOrderHelper.ToString: string;
begin
  case Self of
    fooRandom:
      Result := 'random';
    fooHints:
      Result := 'hints';
  else
    Result := '';
  end;
end;

{ TVkParamsFriendsGetOnline }

function TVkParamsFriendsGetOnline.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsFriendsGetOnline.ListId(Value: Integer): Integer;
begin
  Result := List.Add('list_id', Value);
end;

function TVkParamsFriendsGetOnline.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsFriendsGetOnline.OnlineMobile(Value: Boolean): Integer;
begin
  Result := List.Add('online_mobile', Value);
end;

function TVkParamsFriendsGetOnline.Order(Value: TVkFriendsOnlineOrder): Integer;
begin
  Result := List.Add('order', Value.ToString);
end;

function TVkParamsFriendsGetOnline.UserId(Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkParamsFriendsGetRequests }

function TVkParamsFriendsGetRequests.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsFriendsGetRequests.Fields(Value: TVkUserFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsFriendsGetRequests.NeedMutual(Value: Boolean): Integer;
begin
  Result := List.Add('need_mutual', Value);
end;

function TVkParamsFriendsGetRequests.NeedViewed(Value: Boolean): Integer;
begin
  Result := List.Add('need_viewed', Value);
end;

function TVkParamsFriendsGetRequests.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsFriendsGetRequests.out(Value: Boolean): Integer;
begin
  Result := List.Add('out', Value);
end;

function TVkParamsFriendsGetRequests.Sort(Value: Boolean): Integer;
begin
  Result := List.Add('sort', Value);
end;

function TVkParamsFriendsGetRequests.Suggested(Value: Boolean): Integer;
begin
  Result := List.Add('suggested', Value);
end;

{ TVkParamsFriendsGetSuggestions }

function TVkParamsFriendsGetSuggestions.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsFriendsGetSuggestions.Fields(Value: TVkUserFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsFriendsGetSuggestions.FilterMutual(Value: Boolean): Integer;
begin
  if Value then
    Result := List.Add('filter', 'mutual')
  else
    Result := List.Add('filter', '');
end;

function TVkParamsFriendsGetSuggestions.NameCase(Value: TVkNameCase): Integer;
begin
  Result := List.Add('name_case', Value.ToString);
end;

function TVkParamsFriendsGetSuggestions.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

{ TVkParamsFriendsSearch }

function TVkParamsFriendsSearch.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsFriendsSearch.Fields(Value: TVkUserFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsFriendsSearch.NameCase(Value: TVkNameCase): Integer;
begin
  Result := List.Add('name_case', Value.ToString);
end;

function TVkParamsFriendsSearch.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsFriendsSearch.Query(Value: string): Integer;
begin
  Result := List.Add('q', Value);
end;

function TVkParamsFriendsSearch.UserId(Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

end.

