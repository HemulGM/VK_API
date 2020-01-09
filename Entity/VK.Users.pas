unit VK.Users;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Entity, VK.Types, VK.Structs,
  VK.Users.Types;

type
  TUsers = class(TVKEntity)
    /// <summary>
    /// Возвращает расширенную информацию о пользователях.
    /// </summary>
    function Get(var Users: TUsersClass; UserIds, Fields, NameCase: string): Boolean;
  end;

implementation

uses
  VK.API;

{ TUsers }

function TUsers.Get(var Users: TUsersClass; UserIds, Fields, NameCase: string): Boolean;
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
      Users := TUsersClass.FromJsonString(JSON);
  end;
end;

end.

