unit VK.Friends;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Profile, VK.Entity.Common, VK.Entity.Common.List;

type
  TVkParamsFriendsGet = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя, для которого необходимо получить список друзей.
    /// Если параметр не задан, то считается, что он равен идентификатору текущего
    /// пользователя (справедливо для вызова с передачей AccessToken)
    /// </summary>
    function UserId(const Value: TVkPeerId): TVkParamsFriendsGet;
    /// <summary>
    /// Идентификатор списка друзей, полученный методом GetLists,
    /// друзей из которого необходимо получить. Данный параметр учитывается,
    /// только когда параметр UserId равен идентификатору текущего пользователя.
    /// Этот параметр доступен только для Standalone-приложений с ключом доступа,
    /// полученным по схеме Implicit Flow
    /// </summary>
    function ListId(const Value: Integer): TVkParamsFriendsGet;
    /// <summary>
    /// Порядок, в котором нужно вернуть список друзей.
    /// По умолчанию список сортируется в порядке возрастания идентификаторов пользователей
    /// </summary>
    function Order(const Value: TVkFriendsOrder): TVkParamsFriendsGet;
    /// <summary>
    /// Количество друзей, которое нужно вернуть
    /// </summary>
    function Count(const Value: Integer = 5000): TVkParamsFriendsGet;
    /// <summary>
    /// Смещение, необходимое для выборки определенного подмножества друзей
    /// </summary>
    function Offset(const Value: Integer): TVkParamsFriendsGet;
    /// <summary>
    /// Список дополнительных полей, которые необходимо вернуть
    /// </summary>
    function Fields(const Value: TVkExtendedFields): TVkParamsFriendsGet;
    /// <summary>
    /// Падеж для склонения имени и фамилии пользователя
    /// </summary>
    function NameCase(const Value: TVkNameCase): TVkParamsFriendsGet;
    /// <summary>
    /// Ref
    /// </summary>
    function Ref(const Value: string): TVkParamsFriendsGet;
  end;

  TVkParamsFriendsListEdit = record
    List: TParams;
    /// <summary>
    /// Название списка друзей
    /// </summary>
    function Name(const Value: string): TVkParamsFriendsListEdit;
    /// <summary>
    /// Идентификатор списка друзей
    /// </summary>
    function ListId(const Value: Integer): TVkParamsFriendsListEdit;
    /// <summary>
    /// Идентификаторы пользователей, включенных в список
    /// </summary>
    function UserIds(const Value: TVkPeerIds): TVkParamsFriendsListEdit;
    /// <summary>
    /// Идентификаторы пользователей, которых необходимо добавить в список. (в случае если не передан UserIds)
    /// </summary>
    function AddUserIds(const Value: TVkPeerIds): TVkParamsFriendsListEdit;
    /// <summary>
    /// Идентификаторы пользователей, которых необходимо изъять из списка. (в случае если не передан UserIds)
    /// </summary>
    function DeleteUserIds(const Value: TVkPeerIds): TVkParamsFriendsListEdit;
  end;

  TVkParamsFriendsGetMutual = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя, чьи друзья пересекаются с друзьями пользователя с идентификатором TargetUid.
    /// Если параметр не задан, то считается, что SourceUid равен идентификатору текущего пользователя
    /// </summary>
    function SourceUid(const Value: TVkPeerId): TVkParamsFriendsGetMutual;
    /// <summary>
    /// Идентификатор пользователя, с которым необходимо искать общих друзей
    /// </summary>
    function TargetUid(const Value: TVkPeerId): TVkParamsFriendsGetMutual;
    /// <summary>
    /// Список идентификаторов пользователей, с которыми необходимо искать общих друзей (не более 100)
    /// </summary>
    function TargetUids(const Value: TVkPeerIds): TVkParamsFriendsGetMutual;
    /// <summary>
    /// Возвращает друзей в случайном порядке
    /// </summary>
    function OrderRandom(const Value: Boolean = False): TVkParamsFriendsGetMutual;
    /// <summary>
    /// Количество общих друзей, которое нужно вернуть. (по умолчанию – все общие друзья)
    /// </summary>
    function Count(const Value: Integer): TVkParamsFriendsGetMutual;
    /// <summary>
    /// Смещение, необходимое для выборки определенного подмножества общих друзей
    /// </summary>
    function Offset(const Value: Integer): TVkParamsFriendsGetMutual;
  end;

  TVkParamsFriendsGetOnline = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя, для которого необходимо получить список друзей онлайн.
    /// Если параметр не задан, то считается, что он равен идентификатору текущего пользователя
    /// </summary>
    function UserId(const Value: TVkPeerId): TVkParamsFriendsGetOnline;
    /// <summary>
    /// Идентификатор списка друзей. Если параметр не задан,
    /// возвращается информация обо всех друзьях, находящихся на сайте
    /// </summary>
    function ListId(const Value: Integer): TVkParamsFriendsGetOnline;
    /// <summary>
    /// True — будет возвращено дополнительное поле OnlineMobile
    /// </summary>
    function OnlineMobile(const Value: Boolean = False): TVkParamsFriendsGetOnline;
    /// <summary>
    /// Порядок, в котором нужно вернуть список друзей, находящихся на сайте.
    /// По умолчанию список сортируется в порядке возрастания идентификаторов пользователей.
    /// </summary>
    function Order(const Value: TVkFriendsOrder): TVkParamsFriendsGetOnline;
    /// <summary>
    /// Количество друзей онлайн, которое нужно вернуть. (по умолчанию – все друзья онлайн)
    /// </summary>
    function Count(const Value: Integer): TVkParamsFriendsGetOnline;
    /// <summary>
    /// Смещение, необходимое для выборки определенного подмножества друзей онлайн
    /// </summary>
    function Offset(const Value: Integer): TVkParamsFriendsGetOnline;
  end;

  TVkParamsFriendsGetRequests = record
    List: TParams;
    /// <summary>
    /// Максимальное количество заявок на добавление в друзья, которые необходимо получить (не более 1000)
    /// </summary>
    function Count(const Value: Integer = 100): TVkParamsFriendsGetRequests;
    /// <summary>
    /// Смещение, необходимое для выборки определенного подмножества заявок на добавление в друзья
    /// </summary>
    function Offset(const Value: Integer): TVkParamsFriendsGetRequests;
    /// <summary>
    /// Определяет, требуется ли возвращать в ответе список общих друзей, если они есть.
    /// Обратите внимание, что при использовании NeedMutual будет возвращено не более 2 заявок
    /// </summary>
    function NeedMutual(const Value: Boolean): TVkParamsFriendsGetRequests;
    /// <summary>
    /// False — возвращать полученные заявки в друзья, True — возвращать отправленные пользователем заявки
    /// </summary>
    function &Out(const Value: Boolean = False): TVkParamsFriendsGetRequests;
    /// <summary>
    /// False — сортировать по дате добавления, True — сортировать по количеству общих друзей.
    /// (Если Out = True, этот параметр не учитывается)
    /// </summary>
    function Sort(const Value: Boolean): TVkParamsFriendsGetRequests;
    /// <summary>
    /// False - не возвращать просмотренные заявки, True — возвращать просмотренные заявки.
    /// (Если Out = True, данный параметр не учитывается)
    /// </summary>
    function NeedViewed(const Value: Boolean = False): TVkParamsFriendsGetRequests;
    /// <summary>
    /// True — возвращать рекомендованных другими пользователями друзей, False — возвращать заявки в друзья
    /// </summary>
    function Suggested(const Value: Boolean = False): TVkParamsFriendsGetRequests;
    /// <summary>
    /// Список дополнительных полей, которые необходимо вернуть
    /// </summary>
    function Fields(const Value: TVkExtendedFields): TVkParamsFriendsGetRequests;
    /// <summary>
    /// Ref
    /// </summary>
    function Ref(const Value: string): TVkParamsFriendsGetRequests;
  end;

  TVkParamsFriendsGetSuggestions = record
    List: TParams;
    /// <summary>
    /// Количество рекомендаций, которое необходимо вернуть (максимальное значение 500)
    /// </summary>
    function Count(const Value: Integer = 500): TVkParamsFriendsGetSuggestions;
    /// <summary>
    /// Смещение, необходимое для выбора определённого подмножества списка
    /// </summary>
    function Offset(const Value: Integer): TVkParamsFriendsGetSuggestions;
    /// <summary>
    /// Список дополнительных полей, которые необходимо вернуть
    /// </summary>
    function Fields(const Value: TVkExtendedFields): TVkParamsFriendsGetSuggestions;
    /// <summary>
    /// Пользователи, с которыми много общих друзей
    /// </summary>
    function FilterMutual(const Value: Boolean): TVkParamsFriendsGetSuggestions;
    /// <summary>
    /// Падеж для склонения имени и фамилии пользователя
    /// </summary>
    function NameCase(const Value: TVkNameCase): TVkParamsFriendsGetSuggestions;
  end;

  TVkParamsFriendsSearch = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя, по списку друзей которого необходимо произвести поиск
    /// </summary>
    function UserId(const Value: TVkPeerId = 0): TVkParamsFriendsSearch;
    /// <summary>
    /// Строка запроса
    /// </summary>
    function Query(const Value: string): TVkParamsFriendsSearch;
    /// <summary>
    /// Количество друзей, которое нужно вернуть (максимальное значение 1000)
    /// </summary>
    function Count(const Value: Integer = 20): TVkParamsFriendsSearch;
    /// <summary>
    /// Смещение, необходимое для выборки определенного подмножества друзей
    /// </summary>
    function Offset(const Value: Integer): TVkParamsFriendsSearch;
    /// <summary>
    /// Список дополнительных полей, которые необходимо вернуть
    /// </summary>
    function Fields(const Value: TVkExtendedFields): TVkParamsFriendsSearch;
    /// <summary>
    /// Падеж для склонения имени и фамилии пользователя
    /// </summary>
    function NameCase(const Value: TVkNameCase): TVkParamsFriendsSearch;
  end;

  TFriendsController = class(TVkController)
  public
    /// <summary>
    /// Возвращает список идентификаторов друзей пользователя или расширенную информацию о друзьях пользователя (при использовании параметра fields)
    /// </summary>
    function Get(var Items: TVkProfiles; UserId: TVkPeerId; Fields: TVkExtendedFields = []; Order: TVkFriendsOrder = TVkFriendsOrder.None): Boolean; overload;
    /// <summary>
    /// Возвращает список идентификаторов друзей пользователя или расширенную информацию о друзьях пользователя (при использовании параметра fields)
    /// </summary>
    function Get(var Items: TVkProfiles; Fields: TVkExtendedFields = []; Order: TVkFriendsOrder = TVkFriendsOrder.None): Boolean; overload;
    /// <summary>
    /// Возвращает список идентификаторов друзей пользователя или расширенную информацию о друзьях пользователя (при использовании параметра fields)
    /// </summary>
    function Get(var Items: TVkProfiles; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список идентификаторов друзей пользователя или расширенную информацию о друзьях пользователя (при использовании параметра fields)
    /// </summary>
    function Get(var Items: TVkProfiles; Params: TVkParamsFriendsGet): Boolean; overload;
    /// <summary>
    /// Возвращает список идентификаторов друзей пользователя или расширенную информацию о друзьях пользователя (при использовании параметра fields)
    /// </summary>
    function GetIds(var Items: TVkIdList; Params: TVkParamsFriendsGet): Boolean; overload;
    /// <summary>
    /// Одобряет или создает заявку на добавление в друзья
    /// </summary>
    function Add(var Info: TVkFriendAddInfo; UserId: TVkPeerId; Text: string; Follow: Boolean = False): Boolean;
    /// <summary>
    /// Создает новый список друзей у текущего пользователя
    /// </summary>
    function AddList(var ListId: Integer; Name: string; UserIds: TVkPeerIds): Boolean;
    /// <summary>
    /// Возвращает информацию о том, добавлен ли текущий пользователь в друзья у указанных пользователей
    /// </summary>
    function AreFriends(var Items: TVkFriendInfo; UserIds: TVkPeerIds; NeedSign: Boolean; Extended: Boolean): Boolean;
    /// <summary>
    /// Удаляет пользователя из списка друзей или отклоняет заявку в друзья
    /// </summary>
    function Delete(var Info: TVkFriendDeleteInfo; UserId: TVkPeerId): Boolean; overload;
    /// <summary>
    /// Удаляет пользователя из списка друзей или отклоняет заявку в друзья
    /// </summary>
    function Delete(UserId: TVkPeerId): Boolean; overload;
    /// <summary>
    /// Отмечает все входящие заявки на добавление в друзья как просмотренные
    /// </summary>
    function DeleteAllRequests: Boolean;
    /// <summary>
    /// Удаляет существующий список друзей текущего пользователя
    /// </summary>
    function DeleteList(ListId: Integer): Boolean;
    /// <summary>
    /// Редактирует списки друзей для выбранного друга
    /// </summary>
    function Edit(UserId: TVkPeerId; ListIds: TIdList): Boolean;
    /// <summary>
    /// Редактирует списки друзей для выбранного друга
    /// </summary>
    function EditList(Params: TParams): Boolean; overload;
    /// <summary>
    /// Редактирует списки друзей для выбранного друга
    /// </summary>
    function EditList(Params: TVkParamsFriendsListEdit): Boolean; overload;
    /// <summary>
    /// Позволяет получить список идентификаторов пользователей, доступных для вызова в приложении, используя метод Client API callUser
    /// </summary>
    function GetAvailableForCall(var Items: TVkProfiles; Fields: TVkExtendedFields = []; NameCase: TVkNameCase = TVkNameCase.Nom): Boolean;
    /// <summary>
    /// Возвращает список идентификаторов друзей текущего пользователя, которые установили данное приложение
    /// </summary>
    function GetAppUsers(var Items: TVkIdList): Boolean;
    /// <summary>
    /// Возвращает список друзей пользователя, у которых завалидированные или указанные в профиле телефонные номера входят в заданный список
    /// </summary>
    function GetByPhones(var Items: TVkProfiles; Phones: TArrayOfString; Fields: TVkExtendedFields): Boolean;
    /// <summary>
    /// Возвращает список меток друзей пользователя
    /// </summary>
    function GetLists(var Items: TVkFriendsList; UserId: TVkPeerId; ReturnSystem: Boolean = False): Boolean; overload;
    /// <summary>
    /// Возвращает список идентификаторов общих друзей между парой пользователей
    /// </summary>
    function GetMutual(var Items: TVkIdList; Params: TVkParamsFriendsGetMutual): Boolean;
    /// <summary>
    /// Возвращает список идентификаторов друзей пользователя, находящихся на сайте
    /// </summary>
    function GetOnline(var Items: TVkFriendsOnline; Params: TVkParamsFriendsGetOnline): Boolean;
    /// <summary>
    /// Возвращает список идентификаторов недавно добавленных друзей текущего пользователя
    /// </summary>
    function GetRecent(var Items: TVkIdList; Count: Integer = 100): Boolean;
    /// <summary>
    /// Возвращает информацию о полученных или отправленных заявках на добавление в друзья для текущего пользователя
    /// </summary>
    function GetRequests(var Items: TVkProfiles; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает информацию о полученных или отправленных заявках на добавление в друзья для текущего пользователя
    /// </summary>
    function GetRequests(var Items: TVkProfiles; Params: TVkParamsFriendsGetRequests): Boolean; overload;
    /// <summary>
    /// Возвращает информацию о полученных или отправленных заявках на добавление в друзья для текущего пользователя
    /// </summary>
    function GetRequestsIds(var Items: TVkIdList; Params: TVkParamsFriendsGetRequests): Boolean; overload;
    /// <summary>
    /// Возвращает список профилей пользователей, которые могут быть друзьями текущего пользователя
    /// </summary>
    function GetSuggestions(var Items: TVkProfiles; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список профилей пользователей, которые могут быть друзьями текущего пользователя
    /// </summary>
    function GetSuggestions(var Items: TVkProfiles; Params: TVkParamsFriendsGetSuggestions): Boolean; overload;
    /// <summary>
    /// Позволяет искать по списку друзей пользователей
    /// </summary>
    function Search(var Items: TVkProfiles; Params: TParams): Boolean; overload;
    /// <summary>
    /// Позволяет искать по списку друзей пользователей
    /// </summary>
    function Search(var Items: TVkProfiles; Params: TVkParamsFriendsSearch): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TFriendsController }

function TFriendsController.Get(var Items: TVkProfiles; Fields: TVkExtendedFields; Order: TVkFriendsOrder): Boolean;
var
  Params: TVkParamsFriendsGet;
begin
  if Fields <> [] then
    Params.Fields(Fields);
  if Order <> TVkFriendsOrder.None then
    Params.Order(Order);
  Result := Get(Items, Params);
end;

function TFriendsController.Get(var Items: TVkProfiles; UserId: TVkPeerId; Fields: TVkExtendedFields; Order: TVkFriendsOrder): Boolean;
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

function TFriendsController.Add(var Info: TVkFriendAddInfo; UserId: TVkPeerId; Text: string; Follow: Boolean): Boolean;
var
  Value: Integer;
begin
  with Handler.Execute('friends.add', [['user_id', UserId.ToString], ['text', Text], ['follow', BoolToString(Follow)]]) do
  begin
    Result := ResponseAsInt(Value);
    if Result then
      Info := TVkFriendAddInfo(Value);
  end;
end;

function TFriendsController.AddList(var ListId: Integer; Name: string; UserIds: TVkPeerIds): Boolean;
begin
  Result := Handler.Execute('friends.add', [['name', Name], ['user_ids', UserIds.ToString]]).ResponseAsInt(ListId);
end;

function TFriendsController.AreFriends(var Items: TVkFriendInfo; UserIds: TVkPeerIds; NeedSign, Extended: Boolean): Boolean;
begin
  Result := Handler.Execute('friends.areFriends', [
    ['user_ids', UserIds.ToString],
    ['need_sign', BoolToString(NeedSign)],
    ['extended', BoolToString(Extended)]]).
    GetObjects(Items);
end;

function TFriendsController.Delete(UserId: TVkPeerId): Boolean;
var
  Info: TVkFriendDeleteInfo;
begin
  Result := False;
  with Handler.Execute('friends.delete', ['user_id', UserId.ToString]) do
  begin
    if GetObject(Info) then
    begin
      try
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

function TFriendsController.Edit(UserId: TVkPeerId; ListIds: TIdList): Boolean;
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

function TFriendsController.Delete(var Info: TVkFriendDeleteInfo; UserId: TVkPeerId): Boolean;
begin
  Result := Handler.Execute('friends.delete', ['user_id', UserId.ToString]).GetObject(Info);
end;

function TFriendsController.GetAppUsers(var Items: TVkIdList): Boolean;
begin
  Result := Handler.Execute('friends.getAppUsers').GetObject(Items);
end;

function TFriendsController.GetAvailableForCall(var Items: TVkProfiles; Fields: TVkExtendedFields; NameCase: TVkNameCase): Boolean;
begin
  if Fields = [] then
    Fields := [TVkExtendedField.Photo50];
  var Params: TParams;
  Params.Add('fields', Fields.ToString);
  Params.Add('name_case', NameCase.ToString);
  Result := Handler.Execute('friends.getAvailableForCall').GetObjects(Items);
end;

function TFriendsController.GetByPhones(var Items: TVkProfiles; Phones: TArrayOfString; Fields: TVkExtendedFields): Boolean;
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

function TFriendsController.GetLists(var Items: TVkFriendsList; UserId: TVkPeerId; ReturnSystem: Boolean): Boolean;
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
  Result := Handler.Execute('friends.getRequests', Params.Add('extended', True)).GetObject(Items);
end;

{ TVkFriendsGetParams }

function TVkParamsFriendsGet.Count(const Value: Integer): TVkParamsFriendsGet;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsFriendsGet.Fields(const Value: TVkExtendedFields): TVkParamsFriendsGet;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsFriendsGet.ListId(const Value: Integer): TVkParamsFriendsGet;
begin
  List.Add('list_id', Value);
  Result := Self;
end;

function TVkParamsFriendsGet.NameCase(const Value: TVkNameCase): TVkParamsFriendsGet;
begin
  List.Add('name_case', Value.ToString);
  Result := Self;
end;

function TVkParamsFriendsGet.Offset(const Value: Integer): TVkParamsFriendsGet;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsFriendsGet.Order(const Value: TVkFriendsOrder): TVkParamsFriendsGet;
begin
  List.Add('order', Value.ToString);
  Result := Self;
end;

function TVkParamsFriendsGet.Ref(const Value: string): TVkParamsFriendsGet;
begin
  List.Add('ref', Value);
  Result := Self;
end;

function TVkParamsFriendsGet.UserId(const Value: TVkPeerId): TVkParamsFriendsGet;
begin
  List.Add('user_id', Value);
  Result := Self;
end;

{ TVkParamsFriendsListEdit }

function TVkParamsFriendsListEdit.AddUserIds(const Value: TVkPeerIds): TVkParamsFriendsListEdit;
begin
  List.Add('add_user_ids', Value);
  Result := Self;
end;

function TVkParamsFriendsListEdit.DeleteUserIds(const Value: TVkPeerIds): TVkParamsFriendsListEdit;
begin
  List.Add('delete_user_ids', Value);
  Result := Self;
end;

function TVkParamsFriendsListEdit.ListId(const Value: Integer): TVkParamsFriendsListEdit;
begin
  List.Add('list_id', Value);
  Result := Self;
end;

function TVkParamsFriendsListEdit.Name(const Value: string): TVkParamsFriendsListEdit;
begin
  List.Add('name', Value);
  Result := Self;
end;

function TVkParamsFriendsListEdit.UserIds(const Value: TVkPeerIds): TVkParamsFriendsListEdit;
begin
  List.Add('user_ids', Value);
  Result := Self;
end;

{ TVkParamsFriendsGetMutual }

function TVkParamsFriendsGetMutual.Count(const Value: Integer): TVkParamsFriendsGetMutual;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsFriendsGetMutual.Offset(const Value: Integer): TVkParamsFriendsGetMutual;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsFriendsGetMutual.OrderRandom(const Value: Boolean): TVkParamsFriendsGetMutual;
begin
  if Value then
    List.Add('order', 'random')
  else
    List.Add('order', '');
  Result := Self;
end;

function TVkParamsFriendsGetMutual.SourceUid(const Value: TVkPeerId): TVkParamsFriendsGetMutual;
begin
  List.Add('source_uid', Value);
  Result := Self;
end;

function TVkParamsFriendsGetMutual.TargetUid(const Value: TVkPeerId): TVkParamsFriendsGetMutual;
begin
  List.Add('target_uids', Value);
  Result := Self;
end;

function TVkParamsFriendsGetMutual.TargetUids(const Value: TVkPeerIds): TVkParamsFriendsGetMutual;
begin
  List.Add('target_uids', Value);
  Result := Self;
end;

{ TVkParamsFriendsGetOnline }

function TVkParamsFriendsGetOnline.Count(const Value: Integer): TVkParamsFriendsGetOnline;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsFriendsGetOnline.ListId(const Value: Integer): TVkParamsFriendsGetOnline;
begin
  List.Add('list_id', Value);
  Result := Self;
end;

function TVkParamsFriendsGetOnline.Offset(const Value: Integer): TVkParamsFriendsGetOnline;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsFriendsGetOnline.OnlineMobile(const Value: Boolean): TVkParamsFriendsGetOnline;
begin
  List.Add('online_mobile', Value);
  Result := Self;
end;

function TVkParamsFriendsGetOnline.Order(const Value: TVkFriendsOrder): TVkParamsFriendsGetOnline;
begin
  List.Add('order', Value.ToString);
  Result := Self;
end;

function TVkParamsFriendsGetOnline.UserId(const Value: TVkPeerId): TVkParamsFriendsGetOnline;
begin
  List.Add('user_id', Value);
  Result := Self;
end;

{ TVkParamsFriendsGetRequests }

function TVkParamsFriendsGetRequests.Count(const Value: Integer): TVkParamsFriendsGetRequests;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsFriendsGetRequests.Fields(const Value: TVkExtendedFields): TVkParamsFriendsGetRequests;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsFriendsGetRequests.NeedMutual(const Value: Boolean): TVkParamsFriendsGetRequests;
begin
  List.Add('need_mutual', Value);
  Result := Self;
end;

function TVkParamsFriendsGetRequests.NeedViewed(const Value: Boolean): TVkParamsFriendsGetRequests;
begin
  List.Add('need_viewed', Value);
  Result := Self;
end;

function TVkParamsFriendsGetRequests.Offset(const Value: Integer): TVkParamsFriendsGetRequests;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsFriendsGetRequests.out(const Value: Boolean): TVkParamsFriendsGetRequests;
begin
  List.Add('out', Value);
  Result := Self;
end;

function TVkParamsFriendsGetRequests.Ref(const Value: string): TVkParamsFriendsGetRequests;
begin
  List.Add('ref', Value);
  Result := Self;
end;

function TVkParamsFriendsGetRequests.Sort(const Value: Boolean): TVkParamsFriendsGetRequests;
begin
  List.Add('sort', Value);
  Result := Self;
end;

function TVkParamsFriendsGetRequests.Suggested(const Value: Boolean): TVkParamsFriendsGetRequests;
begin
  List.Add('suggested', Value);
  Result := Self;
end;

{ TVkParamsFriendsGetSuggestions }

function TVkParamsFriendsGetSuggestions.Count(const Value: Integer): TVkParamsFriendsGetSuggestions;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsFriendsGetSuggestions.Fields(const Value: TVkExtendedFields): TVkParamsFriendsGetSuggestions;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsFriendsGetSuggestions.FilterMutual(const Value: Boolean): TVkParamsFriendsGetSuggestions;
begin
  if Value then
    List.Add('filter', 'mutual')
  else
    List.Add('filter', '');
  Result := Self;
end;

function TVkParamsFriendsGetSuggestions.NameCase(const Value: TVkNameCase): TVkParamsFriendsGetSuggestions;
begin
  List.Add('name_case', Value.ToString);
  Result := Self;
end;

function TVkParamsFriendsGetSuggestions.Offset(const Value: Integer): TVkParamsFriendsGetSuggestions;
begin
  List.Add('offset', Value);
  Result := Self;
end;

{ TVkParamsFriendsSearch }

function TVkParamsFriendsSearch.Count(const Value: Integer): TVkParamsFriendsSearch;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsFriendsSearch.Fields(const Value: TVkExtendedFields): TVkParamsFriendsSearch;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsFriendsSearch.NameCase(const Value: TVkNameCase): TVkParamsFriendsSearch;
begin
  List.Add('name_case', Value.ToString);
  Result := Self;
end;

function TVkParamsFriendsSearch.Offset(const Value: Integer): TVkParamsFriendsSearch;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsFriendsSearch.Query(const Value: string): TVkParamsFriendsSearch;
begin
  List.Add('q', Value);
  Result := Self;
end;

function TVkParamsFriendsSearch.UserId(const Value: TVkPeerId): TVkParamsFriendsSearch;
begin
  List.Add('user_id', Value);
  Result := Self;
end;

end.

