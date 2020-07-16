unit VK.Groups;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, REST.Json, System.Json, VK.Controller, VK.Types,
  VK.Entity.User, System.Classes, VK.Entity.Group, VK.CommonUtils, VK.Entity.Common;

type
  TVkParamsGroupsGetMembers = record
    List: TParams;
    /// <summary>
    /// Идентификатор сообщества.
    /// </summary>
    function GroupId(Value: Integer): Integer;
    function Filter(Value: TVkGroupMembersFilter): Integer;
    function Fields(Value: TVkGroupMemberFields): Integer;
    function Count(Value: Integer = 1000): Integer;
    function Offset(Value: Integer = 0): Integer;
    function Sort(Value: TVkSortIdTime): Integer;
  end;

  TVkParamsGroupsGet = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя.
    /// </summary>
    function UserId(Value: Integer): Integer;
    function Filter(Value: TVkGroupFilters): Integer; overload;
    function Fields(Value: TVkGroupFields): Integer; overload;
    function Count(Value: Integer = 1000): Integer;
    function Offset(Value: Integer = 0): Integer;
    function Extended(Value: Boolean = False): Integer;
  end;

  TVkParamsGroupsIsMember = record
    List: TParams;
    /// <summary>
    /// Идентификатор или короткое имя сообщества.
    /// </summary>
    function GroupId(Value: Integer): Integer; overload;
    /// <summary>
    /// Идентификатор или короткое имя сообщества.
    /// </summary>
    function GroupId(Value: string): Integer; overload;
    /// <summary>
    /// True — вернуть ответ в расширенной форме. По умолчанию — False.
    /// </summary>
    function Extended(Value: Boolean = False): Integer;
    /// <summary>
    /// Идентификатор пользователя.
    /// </summary>
    function UserId(Value: Integer): Integer;
    /// <summary>
    /// Идентификаторы пользователей, не более 500.
    /// </summary>
    function UserIds(Value: TIds): Integer;
  end;

  TGroupsController = class(TVkController)
  public
    /// <summary>
    /// Возвращает список участников сообщества.
    /// </summary>
    function GetMembers(var Items: TVkUsers; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список участников сообщества.
    /// </summary>
    function GetMembers(var Items: TVkUsers; Params: TVkParamsGroupsGetMembers): Boolean; overload;
    /// <summary>
    /// Возвращает список id участников сообщества.
    /// </summary>
    function GetMembersIds(var Items: TVkIdList; Params: TVkParamsGroupsGetMembers): Boolean; overload;
    /// <summary>
    /// Включает статус «онлайн» в сообществе.
    /// </summary>
    function EnableOnline(GroupId: Cardinal): Boolean;
    /// <summary>
    /// Выключает статус «онлайн» в сообществе.
    /// </summary>
    function DisableOnline(GroupId: Cardinal): Boolean;
    /// <summary>
    /// Получает информацию о статусе «онлайн» в сообществе.
    /// </summary>
    function GetOnlineStatus(var Value: TVkGroupStatus; GroupId: Cardinal): Boolean;
    /// <summary>
    /// Возвращает список сообществ указанного пользователя.
    /// </summary>
    function Get(var Items: TVkGroups; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список сообществ указанного пользователя.
    /// </summary>
    function Get(var Items: TVkGroups; Params: TVkParamsGroupsGet): Boolean; overload;
    /// <summary>
    /// Возвращает список id сообществ указанного пользователя.
    /// </summary>
    function GetIds(var Items: TVkIdList; Params: TVkParamsGroupsGet): Boolean;
    /// <summary>
    /// Возвращает информацию о том, является ли пользователь участником сообщества.
    /// </summary>
    function IsMember(var Items: TVkGroupMemberStates; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает информацию о том, является ли пользователь участником сообщества.
    /// </summary>
    function IsMember(var Items: TVkGroupMemberStates; Params: TVkParamsGroupsIsMember): Boolean; overload;
    /// <summary>
    ///  Позволяет покинуть сообщество или отклонить приглашение в сообщество.
    /// </summary>
    function Leave(GroupId: integer): Boolean;
    /// <summary>
    ///  Данный метод позволяет вступить в группу, публичную страницу, а также подтвердить участие во встрече.
    ///  NotSure - опциональный параметр, учитываемый, если GroupId принадлежит встрече.
    /// True — Возможно пойду. False — Точно пойду. По умолчанию False.
    /// </summary>
    function Join(GroupId: integer; NotSure: Boolean = False): Boolean;
    /// <summary>
    ///  Позволяет приглашать друзей в группу.
    /// </summary>
    function Invite(GroupId, UserId: integer): Boolean;
    /// <summary>
    ///  Позволяет исключить пользователя из группы или отклонить заявку на вступление.
    /// </summary>
    function RemoveUser(GroupId, UserId: integer): Boolean;
    /// <summary>
    ///  Позволяет одобрить заявку в группу от пользователя.
    /// </summary>
    function ApproveRequest(GroupId, UserId: integer): Boolean;
  end;

implementation

{ TGroupsController }

function TGroupsController.ApproveRequest(GroupId, UserId: integer): Boolean;
begin
  with Handler.Execute('groups.approveRequest', [['group_id', GroupId.ToString], ['user_id', UserId.ToString]]) do
    Result := Success and (Response = '1');
end;

function TGroupsController.DisableOnline(GroupId: Cardinal): Boolean;
begin
  with Handler.Execute('groups.disableOnline', ['group_id', GroupId.ToString]) do
    Result := Success and (Response = '1');
end;

function TGroupsController.EnableOnline(GroupId: Cardinal): Boolean;
begin
  with Handler.Execute('groups.enableOnline', ['group_id', GroupId.ToString]) do
    Result := Success and (Response = '1');
end;

function TGroupsController.Get(var Items: TVkGroups; Params: TVkParamsGroupsGet): Boolean;
begin
  Result := Get(Items, Params.List);
end;

function TGroupsController.Get(var Items: TVkGroups; Params: TParams): Boolean;
begin
  if not Params.KeyExists('extended') then
    Params.Add('extended', True);
  with Handler.Execute('groups.get', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkGroups.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TGroupsController.GetIds(var Items: TVkIdList; Params: TVkParamsGroupsGet): Boolean;
begin
  Params.Extended(False);
  with Handler.Execute('groups.get', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkIdList.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TGroupsController.GetMembers(var Items: TVkUsers; Params: TVkParamsGroupsGetMembers): Boolean;
begin
  if not Params.List.KeyExists('fields') then
    Params.Fields([mfDomain]);
  Result := GetMembers(Items, Params.List);
end;

function TGroupsController.GetMembers(var Items: TVkUsers; Params: TParams): Boolean;
begin
  with Handler.Execute('groups.getMembers', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkUsers.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TGroupsController.GetMembersIds(var Items: TVkIdList; Params: TVkParamsGroupsGetMembers): Boolean;
begin
  Params.Fields([]);
  with Handler.Execute('groups.getMembers', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkIdList.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TGroupsController.GetOnlineStatus(var Value: TVkGroupStatus; GroupId: Cardinal): Boolean;
begin
  with Handler.Execute('groups.getOnlineStatus', ['group_id', GroupId.ToString]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Value := TVkGroupStatus.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TGroupsController.Invite(GroupId, UserId: integer): Boolean;
begin
  with Handler.Execute('groups.invite', [['group_id', GroupId.ToString], ['user_id', UserId.ToString]]) do
    Result := Success and (Response = '1');
end;

function TGroupsController.IsMember(var Items: TVkGroupMemberStates; Params: TVkParamsGroupsIsMember): Boolean;
begin
  Result := IsMember(Items, Params.List);
end;

function TGroupsController.Join(GroupId: integer; NotSure: Boolean): Boolean;
var
  Params: TParams;
begin
  Params.Add('group_id', GroupId);
  if NotSure then
    Params.Add('not_sure', NotSure);
  with Handler.Execute('groups.join', Params) do
    Result := Success and (Response = '1');
end;

function TGroupsController.Leave(GroupId: integer): Boolean;
begin
  with Handler.Execute('groups.leave', ['group_id', GroupId.ToString]) do
    Result := Success and (Response = '1');
end;

function TGroupsController.RemoveUser(GroupId, UserId: integer): Boolean;
begin
  with Handler.Execute('groups.removeUser', [['group_id', GroupId.ToString], ['user_id', UserId.ToString]]) do
    Result := Success and (Response = '1');
end;

function TGroupsController.IsMember(var Items: TVkGroupMemberStates; Params: TParams): Boolean;
begin
  with Handler.Execute('groups.isMember', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkGroupMemberStates.FromJsonString(AppendItemsTag(Response));
      except
        Result := False;
      end;
    end;
  end;
end;

{ TVkGetMembersParams }

function TVkParamsGroupsGetMembers.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsGroupsGetMembers.Fields(Value: TVkGroupMemberFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsGroupsGetMembers.Filter(Value: TVkGroupMembersFilter): Integer;
begin
  Result := List.Add('filter', Value.ToString);
end;

function TVkParamsGroupsGetMembers.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsGetMembers.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsGroupsGetMembers.Sort(Value: TVkSortIdTime): Integer;
begin
  Result := List.Add('sort', Value.ToString);
end;

{ TVkGroupsGetParams }

function TVkParamsGroupsGet.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsGroupsGet.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsGroupsGet.Fields(Value: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsGroupsGet.Filter(Value: TVkGroupFilters): Integer;
begin
  Result := List.Add('filter', Value.ToString);
end;

function TVkParamsGroupsGet.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsGroupsGet.UserId(Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkParamsGroupsIsMember }

function TVkParamsGroupsIsMember.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsGroupsIsMember.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsIsMember.GroupId(Value: string): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsIsMember.UserId(Value: Integer): Integer;
begin
  Result := List.Add('user_ids', Value);
end;

function TVkParamsGroupsIsMember.UserIds(Value: TIds): Integer;
begin
  Result := List.Add('user_ids', Value);
end;

end.

