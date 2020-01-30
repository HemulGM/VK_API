unit VK.Friends;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.User;

type
  TFriendsController = class(TVkController)
  public
    /// <summary>
    /// Возвращает расширенную информацию о пользователях.
    /// </summary>
    function Get(var Users: TVkFriends; UserId: Integer; Fields: string): Boolean; overload;
    /// <summary>
    /// Возвращает расширенную информацию о пользователях.
    /// </summary>
    function Get(var Users: TVkFriends; Fields: string): Boolean; overload;
  end;

implementation

uses
  VK.API;

{ TFriendsController }

function TFriendsController.Get(var Users: TVkFriends; UserId: Integer; Fields: string): Boolean;
var
  Params: TParams;
begin
  if UserId <> 0 then
    Params.Add('user_id', UserId);
  if not Fields.IsEmpty then
    Params.Add('fields', Fields);
  with Handler.Execute('friends.get', Params) do
  begin
    Result := Success;
    if Result then
      Users := TVkFriends.FromJsonString(Response);
  end;
end;

function TFriendsController.Get(var Users: TVkFriends; Fields: string): Boolean;
begin
  Result := Get(Users, 0, Fields);
end;

end.

