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
    function Get(var User: TVkUser; UserId: Integer = 0; Fields: string = ''; NameCase: string = ''): Boolean; overload;
  end;

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
      Users := TVkUsers.FromJsonString(JSON);
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

end.

