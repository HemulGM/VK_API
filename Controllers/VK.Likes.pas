unit VK.Likes;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types,
  VK.Entity.Audio, System.JSON;

type
  TVkLikesCount = record
    Count: Integer;
    Users: TUserIds;
  end;

  TVkLikesParams = record
    List: TParams;
    function ItemType(Value: TVkItemType): Integer;
    function OwnerId(Value: Integer): Integer;
    function ItemId(Value: Integer): Integer;
    function PageUrl(Value: string): Integer;
    function Filter(Copies: Boolean): Integer;
    function FriendsOnly(Value: Boolean): Integer;
    function Count(Value: Integer): Integer;
    function SkipOwn(Value: Boolean): Integer;
  end;

  TLikesController = class(TVkController)
  public
    /// <summary>
    /// Получает список идентификаторов пользователей, которые добавили заданный объект в свой список Мне нравится.
    /// </summary>
    /// <param name="var Likes: TVkLikesCount">Возвращается кол-во лайков и список лайкнувших</param>
    /// <param name="ItemType: TVkItemType">Тип элемента</param>
    /// <param name="ItemId: Integer">ИД элемента</param>
    /// <param name="OwnerId: Integer">Владелец элемента</param>
    function GetList(var Likes: TVkLikesCount; ItemType: TVkItemType; ItemId: Integer; OwnerId:
      Integer = 0): Boolean; overload;
    /// <summary>
    /// Получает список идентификаторов пользователей, которые добавили заданный объект в свой список Мне нравится.
    /// </summary>
    /// <param name="var Likes: TVkLikesCount">Возвращается кол-во лайков и список лайкнувших</param>
    /// <param name="ItemType: TVkItemType">Тип элемента</param>
    /// <param name="ItemId: Integer">ИД элемента</param>
    /// <param name="Copies: Boolean">Только репосты</param>
    /// <param name="OwnerId: Integer">Владелец элемента</param>
    function GetList(var Likes: TVkLikesCount; ItemType: TVkItemType; ItemId: Integer; Copies:
      Boolean; OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Получает список идентификаторов пользователей, которые добавили заданный
    /// объект в свой список Мне нравится. Для виджетов
    /// </summary>
    /// <param name="var Likes: TVkLikesCount">Возвращается кол-во лайков и список лайкнувших</param>
    /// <param name="ItemId: Integer">ИД элемента</param>
    /// <param name="OwnerId: Integer">Владелец элемента</param>
    /// <param name="PageUrl: string">Адрес сраницы</param>
    function GetList(var Likes: TVkLikesCount; ItemId: Integer; OwnerId: Integer; PageUrl: string): Boolean; overload;
    /// <summary>
    /// Получает список идентификаторов пользователей, которые добавили заданный
    /// объект в свой список Мне нравится
    /// </summary>
    /// <param name="var Likes: TVkLikesCount">Возвращается кол-во лайков и список лайкнувших</param>
    /// <param name="Params: TVkLikesParams">Параметры</param>
    function GetList(var Likes: TVkLikesCount; Params: TVkLikesParams): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TLikesController }

function TLikesController.GetList(var Likes: TVkLikesCount; ItemType: TVkItemType; ItemId, OwnerId: Integer): Boolean;
var
  Params: TVkLikesParams;
begin
  if OwnerId <> 0 then
    Params.OwnerId(OwnerId);
  Params.ItemType(ItemType);
  Params.ItemId(ItemId);
  Result := GetList(Likes, Params);
end;

function TLikesController.GetList(var Likes: TVkLikesCount; ItemId, OwnerId: Integer; PageUrl: string): Boolean;
var
  Params: TVkLikesParams;
begin
  if OwnerId <> 0 then
    Params.OwnerId(OwnerId);
  Params.ItemType(TVkItemType.itSitepage);
  Params.ItemId(ItemId);
  Params.PageUrl(PageUrl);
  Result := GetList(Likes, Params);
end;

function TLikesController.GetList(var Likes: TVkLikesCount; ItemType: TVkItemType; ItemId: Integer;
  Copies: Boolean; OwnerId: Integer): Boolean;
var
  Params: TVkLikesParams;
begin
  if OwnerId <> 0 then
    Params.OwnerId(OwnerId);
  if Copies then
    Params.Filter(True);
  Params.ItemType(ItemType);
  Params.ItemId(ItemId);
  Result := GetList(Likes, Params);
end;

function TLikesController.GetList(var Likes: TVkLikesCount; Params: TVkLikesParams): Boolean;
var
  JSONItem: TJSONValue;
  JArray: TJSONArray;
  i: Integer;
begin
  with Handler.Execute('likes.getList', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      JSONItem := TJSONObject.ParseJSONValue(Response);
      Likes.Count := JSONItem.GetValue<Integer>('count', -1);
      JArray := JSONItem.GetValue<TJSONArray>('items', nil);
      if Assigned(JArray) then
      begin
        SetLength(Likes.Users, JArray.Count);
        for i := 0 to JArray.Count - 1 do
          Likes.Users[i] := JArray.Items[i].GetValue<Integer>();
        JArray.Free;
      end
      else
        Result := False;
      JSONItem.Free;
    end;
  end;
end;

{ TVkLikesParams }

function TVkLikesParams.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value.ToString);
end;

function TVkLikesParams.Filter(Copies: Boolean): Integer;
begin
  if Copies then
    Result := List.Add('filter', 'copies')
  else
    Result := -1;
end;

function TVkLikesParams.FriendsOnly(Value: Boolean): Integer;
begin
  Result := List.Add('friends_only', BoolToString(Value));
end;

function TVkLikesParams.ItemId(Value: Integer): Integer;
begin
  Result := List.Add('item_id', Value.ToString);
end;

function TVkLikesParams.ItemType(Value: TVkItemType): Integer;
begin
  Result := List.Add('type', Value.ToConst);
end;

function TVkLikesParams.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value.ToString);
end;

function TVkLikesParams.PageUrl(Value: string): Integer;
begin
  Result := List.Add('page_url', Value);
end;

function TVkLikesParams.SkipOwn(Value: Boolean): Integer;
begin
  Result := List.Add('skip_own', BoolToString(Value));
end;

end.

