unit VK.Likes;

interface

uses
  System.SysUtils, VK.Controller, VK.Types, VK.Entity.Common, VK.Entity.Profile, System.JSON, VK.Entity.Common.List;

type
  TVkLikesParams = record
    List: TParams;
    function &Type(const Value: TVkItemType): Integer;
    function OwnerId(const Value: Integer): Integer;
    function ItemId(const Value: Integer): Integer;
    function PageUrl(const Value: string): Integer;
    function Filter(Copies: Boolean): Integer;
    function FriendsOnly(const Value: Boolean): Integer;
    function Count(const Value: Integer): Integer;
    function Offset(const Value: Integer): Integer;
    function SkipOwn(const Value: Boolean): Integer;
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
    function GetList(var Items: TVkProfiles; Params: TVkLikesParams): Boolean; overload;
    /// <summary>
    /// Получает список идентификаторов пользователей, которые добавили заданный объект в свой список Мне нравится
    /// </summary>
    function GetListIds(var Items: TVkIdList; Params: TVkLikesParams): Boolean; overload;
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

function TLikesController.GetList(var Items: TVkProfiles; Params: TVkLikesParams): Boolean;
begin
  Result := GetList(Items, Params.List);
end;

function TLikesController.GetListIds(var Items: TVkIdList; Params: TVkLikesParams): Boolean;
begin
  Params.List.Add('extended', False);
  Result := Handler.Execute('likes.getList', Params.List).GetObject(Items);
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
  Params.Add('extended', True);
  Result := Handler.Execute('likes.getList', Params).GetObject(Items);
end;

{ TVkLikesParams }

function TVkLikesParams.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkLikesParams.Filter(Copies: Boolean): Integer;
begin
  if Copies then
    Result := List.Add('filter', 'copies')
  else
  begin
    List.Remove('filter');
    Result := -1;
  end;
end;

function TVkLikesParams.FriendsOnly(const Value: Boolean): Integer;
begin
  Result := List.Add('friends_only', Value);
end;

function TVkLikesParams.ItemId(const Value: Integer): Integer;
begin
  Result := List.Add('item_id', Value);
end;

function TVkLikesParams.&Type(const Value: TVkItemType): Integer;
begin
  Result := List.Add('type', Value.ToString);
end;

function TVkLikesParams.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkLikesParams.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkLikesParams.PageUrl(const Value: string): Integer;
begin
  Result := List.Add('page_url', Value);
end;

function TVkLikesParams.SkipOwn(const Value: Boolean): Integer;
begin
  Result := List.Add('skip_own', Value);
end;

end.

