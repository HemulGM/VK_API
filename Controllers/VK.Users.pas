unit VK.Users;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.User;

type
  TUsersController = class(TVkController)
  public
    /// <summary>
    /// Возвращает расширенную информацию о пользователях.
    /// </summary>
    function Get(var Users: TVkUsers; UserIds, Fields, NameCase: string): Boolean; overload;
    /// <summary>
    /// Возвращает расширенную информацию о пользователях.
    /// </summary>
    function Get(var Users: TVkUsers; UserIds: TIds; Fields, NameCase: string): Boolean; overload;
    /// <summary>
    /// Возвращает расширенную информацию о пользователях.
    /// </summary>
    function Get(var User: TVkUser; UserId: Integer = 0; Fields: string = ''; NameCase: string = ''): Boolean; overload;
  end;

{ Fields
photo_id, verified, sex, bdate, city, country, home_town, has_photo, photo_50,
photo_100, photo_200_orig, photo_200, photo_400_orig, photo_max, photo_max_orig,
online, domain, has_mobile, contacts, site, education, universities, schools,
status, last_seen, followers_count, common_count, occupation, nickname, relatives,
relation, personal, connections, exports, activities, interests, music, movies,
tv, books, games, about, quotes, can_post, can_see_all_posts, can_see_audio,
can_write_private_message, can_send_friend_request, is_favorite, is_hidden_from_feed,
timezone, screen_name, maiden_name, crop_photo, is_friend, friend_status, career,
military, blacklisted, blacklisted_by_me, can_be_invited_group
}


implementation

uses
  VK.API;

{ TUsers }

function TUsersController.Get(var Users: TVkUsers; UserIds, Fields, NameCase: string): Boolean;
var
  Params: TParams;
begin
  if not UserIds.IsEmpty then
    AddParam(Params, ['user_ids', UserIds]);
  if not Fields.IsEmpty then
    AddParam(Params, ['fields', Fields]);
  if not NameCase.IsEmpty then
    AddParam(Params, ['num', NameCase]);
  with Handler.Execute('users.get', Params) do
  begin
    Result := Success;
    if Result then
      Users := TVkUsers.FromJsonString(AppendItemsTag(Response));
  end;
end;

function TUsersController.Get(var User: TVkUser; UserId: Integer; Fields, NameCase: string): Boolean;
var
  Params: TParams;
  Users: TVkUsers;
begin
  if UserId < 0 then
    Exit(False);
  if UserId <> 0 then
    AddParam(Params, ['user_ids', UserId.ToString]);
  if not Fields.IsEmpty then
    AddParam(Params, ['fields', Fields]);
  if not NameCase.IsEmpty then
    AddParam(Params, ['num', NameCase]);
  with Handler.Execute('users.get', Params) do
  begin
    Result := Success;
    if Result then
    begin
      Users := TVkUsers.FromJsonString(AppendItemsTag(Response));
      if Length(Users.Items) > 0 then
      begin
        Users.SaveObjects := True;
        User := Users.Items[0];
      end
      else
        Result := False;
      Users.Free;
    end;
  end;
end;

function TUsersController.Get(var Users: TVkUsers; UserIds: TIds; Fields, NameCase: string): Boolean;
var
  Params: TParams;
begin
  if Length(UserIds) > 0 then
    AddParam(Params, ['user_ids', UserIds.ToString]);
  if not Fields.IsEmpty then
    AddParam(Params, ['fields', Fields]);
  if not NameCase.IsEmpty then
    AddParam(Params, ['num', NameCase]);
  with Handler.Execute('users.get', Params) do
  begin
    Result := Success;
    if Result then
      Users := TVkUsers.FromJsonString(AppendItemsTag(Response));
  end;
end;

end.

