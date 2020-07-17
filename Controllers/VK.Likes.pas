unit VK.Likes;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.Audio, System.JSON;

type
  TVkLikesCount = record
    Count: Integer;
    Users: TUserIds;
  end;

  TVkLikesParams = record
    List: TParams;
    function &Type(Value: TVkItemType): Integer;
    function OwnerId(Value: Integer): Integer;
    function ItemId(Value: Integer): Integer;
    function PageUrl(Value: string): Integer;
    function Filter(Copies: Boolean): Integer;
    function FriendsOnly(Value: Boolean): Integer;
    function Count(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function SkipOwn(Value: Boolean): Integer;
  end;

  TLikesController = class(TVkController)
  public
    /// <summary>
    /// Получает список идентификаторов пользователей, которые добавили заданный объект в свой список Мне нравится
    /// </summary>
    function GetList(var Items: TVkLikesCount; Params: TParams): Boolean; overload;
    /// <summary>
    /// Получает список идентификаторов пользователей, которые добавили заданный объект в свой список Мне нравится
    /// </summary>
    function GetList(var Items: TVkLikesCount; Params: TVkLikesParams): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TLikesController }

function TLikesController.GetList(var Items: TVkLikesCount; Params: TVkLikesParams): Boolean;
begin
  Result := GetList(Items, Params.List);
end;

function TLikesController.GetList(var Items: TVkLikesCount; Params: TParams): Boolean;
var
  JSONItem: TJSONValue;
  JArray: TJSONArray;
  i: Integer;
begin
  Params.Add('extended', True);
  with Handler.Execute('likes.getList', Params) do
  begin
    Result := Success;
    if Result then
    begin
      JSONItem := TJSONObject.ParseJSONValue(Response);
      Items.Count := JSONItem.GetValue<Integer>('count', -1);
      JArray := JSONItem.GetValue<TJSONArray>('items', nil);
      if Assigned(JArray) then
      begin
        SetLength(Items.Users, JArray.Count);
        for i := 0 to JArray.Count - 1 do
          Items.Users[i] := JArray.Items[i].GetValue<Integer>();
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

function TVkLikesParams.FriendsOnly(Value: Boolean): Integer;
begin
  Result := List.Add('friends_only', BoolToString(Value));
end;

function TVkLikesParams.ItemId(Value: Integer): Integer;
begin
  Result := List.Add('item_id', Value);
end;

function TVkLikesParams.&Type(Value: TVkItemType): Integer;
begin
  Result := List.Add('type', Value.ToString);
end;

function TVkLikesParams.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkLikesParams.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
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

