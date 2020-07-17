unit VK.Users;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.User;

type
  TVkParamsUsersGet = record
    List: TParams;
    function UserIds(Value: TIds): Integer;
    function Fields(Value: string): Integer; overload;
    function Fields(Value: TVkUserFields): Integer; overload;
    function NameCase(Value: TVkNameCase): Integer; overload;
  end;

  TVkParamsUsersGetFollowers = record
    List: TParams;
    function UserId(Value: Integer): Integer;
    function Fields(Value: string): Integer; overload;
    function Count(Value: Integer): Integer; overload;
    function Offset(Value: Integer): Integer; overload;
    function Fields(Value: TVkUserFields): Integer; overload;
    function NameCase(Value: TVkNameCase): Integer; overload;
  end;

  TUsersController = class(TVkController)
  public
    /// <summary>
    /// Возвращает расширенную информацию о пользователях.
    /// </summary>
    function Get(var Users: TVkUsers; UserIds: TIds; Fields: TVkUserFields = []; NameCase: TVkNameCase = ncNom): Boolean;
      overload;
    /// <summary>
    /// Возвращает расширенную информацию о пользователях.
    /// </summary>
    function Get(var User: TVkUser; UserId: Integer; Fields: TVkUserFields = []; NameCase: TVkNameCase = ncNom): Boolean;
      overload;
    /// <summary>
    /// Возвращает расширенную информацию о пользователях.
    /// </summary>
    function Get(var User: TVkUser; Fields: TVkUserFields = []; NameCase: TVkNameCase = ncNom): Boolean; overload;
    /// <summary>
    /// Возвращает расширенную информацию о пользователях.
    /// </summary>
    function Get(var Users: TVkUsers; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает расширенную информацию о пользователях.
    /// </summary>
    function Get(var Users: TVkUsers; Params: TVkParamsUsersGet): Boolean; overload;
    /// <summary>
    /// Возвращает список идентификаторов пользователей, которые являются подписчиками пользователя.
    /// </summary>
    function GetFollowers(var Users: TVkUsers; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список идентификаторов пользователей, которые являются подписчиками пользователя.
    /// </summary>
    function GetFollowers(var Users: TVkUsers; Params: TVkParamsUsersGetFollowers): Boolean; overload;
  end;

implementation

uses
  VK.API;

{ TUsersController }

function TUsersController.Get(var User: TVkUser; UserId: Integer; Fields: TVkUserFields; NameCase: TVkNameCase): Boolean;
var
  Params: TVkParamsUsersGet;
  Users: TVkUsers;
begin
  if UserId <> 0 then
    Params.UserIds([UserId]);
  if Fields <> [] then
    Params.Fields(Fields);
  Params.NameCase(NameCase);
  with Handler.Execute('users.get', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Users := TVkUsers.FromJsonString(AppendItemsTag(Response));
        if Length(Users.Items) > 0 then
        begin
          Users.SaveObjects := True;
          User := Users.Items[0];
        end
        else
          Result := False;
        Users.Free;
      except
        Result := False;
      end;
    end;
  end;
end;

function TUsersController.Get(var Users: TVkUsers; UserIds: TIds; Fields: TVkUserFields; NameCase: TVkNameCase): Boolean;
var
  Params: TParams;
begin
  if Length(UserIds) > 0 then
    Params.Add('user_ids', UserIds);
  if Fields <> [] then
    Params.Add('fields', Fields.ToString);
  Params.Add('num', NameCase.ToString);
  Result := Get(Users, Params);
end;

function TUsersController.Get(var Users: TVkUsers; Params: TParams): Boolean;
begin
  with Handler.Execute('users.get', Params) do
  begin
    Result := Success;
    if Result then
    try
      Users := TVkUsers.FromJsonString(AppendItemsTag(Response));
    except
      Result := False;
    end;
  end;
end;

function TUsersController.Get(var Users: TVkUsers; Params: TVkParamsUsersGet): Boolean;
begin
  Result := Get(Users, Params.List);
end;

function TUsersController.Get(var User: TVkUser; Fields: TVkUserFields; NameCase: TVkNameCase): Boolean;
begin
  Result := Get(User, 0, Fields, NameCase);
end;

function TUsersController.GetFollowers(var Users: TVkUsers; Params: TVkParamsUsersGetFollowers): Boolean;
begin
  Result := GetFollowers(Users, Params.List);
end;

function TUsersController.GetFollowers(var Users: TVkUsers; Params: TParams): Boolean;
begin
  with Handler.Execute('users.getFollowers', Params) do
  begin
    Result := Success;
    if Result then
    try
      Users := TVkUsers.FromJsonString(Response);
    except
      Result := False;
    end;
  end;
end;

{ TVkParamsUsersGet }

function TVkParamsUsersGet.Fields(Value: string): Integer;
begin
  Result := List.Add('fields', Value);
end;

function TVkParamsUsersGet.Fields(Value: TVkUserFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsUsersGet.NameCase(Value: TVkNameCase): Integer;
begin
  Result := List.Add('name_case', Value.ToString);
end;

function TVkParamsUsersGet.UserIds(Value: TIds): Integer;
begin
  Result := List.Add('user_ids', Value);
end;

{ TVkParamsUsersGetFollowers }

function TVkParamsUsersGetFollowers.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsUsersGetFollowers.Fields(Value: string): Integer;
begin
  Result := List.Add('fields', Value);
end;

function TVkParamsUsersGetFollowers.Fields(Value: TVkUserFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsUsersGetFollowers.NameCase(Value: TVkNameCase): Integer;
begin
  Result := List.Add('name_case', Value.ToString);
end;

function TVkParamsUsersGetFollowers.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsUsersGetFollowers.UserId(Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

end.

