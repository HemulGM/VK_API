unit VK.Likes;

interface

uses
  System.SysUtils, VK.Controller, VK.Types, VK.Entity.Common, VK.Entity.Profile,
  System.JSON, VK.Entity.Common.List;

type
  TVkParamsLikesGetList = record
    List: TParams;
    /// <summary>
    /// Тип объекта
    /// </summary>
    function &Type(const Value: TVkItemType): TVkParamsLikesGetList;
    /// <summary>
    /// Идентификатор владельца Like-объекта: id пользователя, id сообщества (со знаком «минус») или id приложения.
    /// Если параметр type равен sitepage, то в качестве owner_id необходимо передавать id приложения.
    /// Если параметр не задан, то считается, что он равен либо идентификатору текущего пользователя,
    /// либо идентификатору текущего приложения (если Type равен Sitepage)
    /// </summary>
    function OwnerId(const Value: Integer): TVkParamsLikesGetList;
    /// <summary>
    /// Идентификатор Like-объекта. Если type равен Sitepage, то параметр ItemId может содержать
    /// значение параметра PageId, используемый при инициализации виджета «Мне нравится»
    /// </summary>
    function ItemId(const Value: Integer): TVkParamsLikesGetList;
    /// <summary>
    /// Url страницы, на которой установлен виджет «Мне нравится». Используется вместо параметра ItemId,
    /// если при размещении виджета не был указан PageId
    /// </summary>
    function PageUrl(const Value: string): TVkParamsLikesGetList;
    /// <summary>
    /// Указывает, следует ли вернуть всех пользователей, добавивших объект в список "Мне нравится" или только тех,
    /// которые рассказали о нем друзьям. False - вернуть всех пользователей
    /// </summary>
    function Filter(const Value: Boolean = False): TVkParamsLikesGetList;
    /// <summary>
    /// Указывает, необходимо ли возвращать только пользователей, которые являются друзьями текущего пользователя
    /// </summary>
    function FriendsOnly(const Value: Boolean): TVkParamsLikesGetList;
    /// <summary>
    /// Количество возвращаемых идентификаторов пользователей.
    /// Если параметр не задан, то считается, что он равен 100, если не задан параметр FriendsOnly, в противном случае 10.
    /// Максимальное значение параметра 1000, если не задан параметр FriendsOnly, в противном случае 100
    /// </summary>
    function Count(const Value: Integer): TVkParamsLikesGetList;
    /// <summary>
    /// Смещение, относительно начала списка, для выборки определенного подмножества
    /// </summary>
    function Offset(const Value: Integer = 0): TVkParamsLikesGetList;
    /// <summary>
    /// Не возвращать самого пользователя
    /// </summary>
    function SkipOwn(const Value: Boolean): TVkParamsLikesGetList;
  end;

  TLikesController = class(TVkController)
  public
    /// <summary>
    /// Получает список пользователей, которые добавили заданный объект в свой список Мне нравится
    /// </summary>
    function GetList(var Items: TVkProfiles; Params: TParams): Boolean; overload;
    /// <summary>
    /// Получает список пользователей, которые добавили заданный объект в свой список Мне нравится
    /// </summary>
    function GetList(var Items: TVkProfiles; Params: TVkParamsLikesGetList): Boolean; overload;
    /// <summary>
    /// Получает список идентификаторов пользователей, которые добавили заданный объект в свой список Мне нравится
    /// </summary>
    function GetListIds(var Items: TVkIdList; Params: TVkParamsLikesGetList): Boolean; overload;
    /// <summary>
    /// Получает список пользователей, которые добавили заданный объект в свой список Мне нравится
    /// </summary>
    function Add(var Items: Integer; &Type: TVkItemType; OwnerId, ItemId: Integer; AccessKey: string = ''): Boolean; overload;
    /// <summary>
    /// Удаляет указанный объект из списка Мне нравится текущего пользователя
    /// </summary>
    function Delete(var Items: Integer; &Type: TVkItemType; OwnerId, ItemId: Integer; AccessKey: string = ''): Boolean; overload;
    /// <summary>
    /// Проверяет, находится ли объект в списке Мне нравится заданного пользователя.
    /// </summary>
    function IsLiked(var Item: TVkLiked; UserId: Integer; &Type: TVkItemType; OwnerId, ItemId: Integer): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TLikesController }

function TLikesController.Add(var Items: Integer; &Type: TVkItemType; OwnerId, ItemId: Integer; AccessKey: string): Boolean;
begin
  Result := Handler.Execute('likes.add', [
    ['type', &Type.ToString],
    ['owner_id', OwnerId.ToString],
    ['item_id', ItemId.ToString],
    ['access_key', AccessKey]]).
    ResponseAsInt(Items);
end;

function TLikesController.Delete(var Items: Integer; &Type: TVkItemType; OwnerId, ItemId: Integer; AccessKey: string): Boolean;
begin
  Result := Handler.Execute('likes.delete', [
    ['type', &Type.ToString],
    ['owner_id', OwnerId.ToString],
    ['item_id', ItemId.ToString],
    ['access_key', AccessKey]]).
    ResponseAsInt(Items);
end;

function TLikesController.GetList(var Items: TVkProfiles; Params: TVkParamsLikesGetList): Boolean;
begin
  Result := GetList(Items, Params.List);
end;

function TLikesController.GetListIds(var Items: TVkIdList; Params: TVkParamsLikesGetList): Boolean;
begin
  Result := Handler.Execute('likes.getList', Params.List.Add('extended', False)).GetObject(Items);
end;

function TLikesController.IsLiked(var Item: TVkLiked; UserId: Integer; &Type: TVkItemType; OwnerId, ItemId: Integer): Boolean;
begin
  Result := Handler.Execute('likes.isLiked', [
    ['type', &Type.ToString],
    ['owner_id', OwnerId.ToString],
    ['item_id', ItemId.ToString],
    ['user_id', UserId.ToString]]).
    GetObject(Item);
end;

function TLikesController.GetList(var Items: TVkProfiles; Params: TParams): Boolean;
begin
  Result := Handler.Execute('likes.getList', Params.Add('extended', True)).GetObject(Items);
end;

{ TVkLikesParams }

function TVkParamsLikesGetList.Count(const Value: Integer): TVkParamsLikesGetList;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsLikesGetList.Filter(const Value: Boolean): TVkParamsLikesGetList;
begin
  if Value then
    List.Add('filter', 'copies')
  else
    List.Remove('filter');
  Result := Self;
end;

function TVkParamsLikesGetList.FriendsOnly(const Value: Boolean): TVkParamsLikesGetList;
begin
  List.Add('friends_only', Value);
  Result := Self;
end;

function TVkParamsLikesGetList.ItemId(const Value: Integer): TVkParamsLikesGetList;
begin
  List.Add('item_id', Value);
  Result := Self;
end;

function TVkParamsLikesGetList.&Type(const Value: TVkItemType): TVkParamsLikesGetList;
begin
  List.Add('type', Value.ToString);
  Result := Self;
end;

function TVkParamsLikesGetList.Offset(const Value: Integer): TVkParamsLikesGetList;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsLikesGetList.OwnerId(const Value: Integer): TVkParamsLikesGetList;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsLikesGetList.PageUrl(const Value: string): TVkParamsLikesGetList;
begin
  List.Add('page_url', Value);
  Result := Self;
end;

function TVkParamsLikesGetList.SkipOwn(const Value: Boolean): TVkParamsLikesGetList;
begin
  List.Add('skip_own', Value);
  Result := Self;
end;

end.

