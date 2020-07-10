unit VK.Groups;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, REST.Json, System.Json, VK.Controller, VK.Types,
  VK.Entity.User, System.Classes, VK.Entity.Group, VK.CommonUtils, VK.Entity.Common;

type
  TVkParamsGroupsGetMembers = record
    List: TParams;
    function GroupId(Value: Integer): Integer;
    function Filter(Value: string): Integer;
    {friends Ч будут возвращены только друзь€ в этом сообществе.
     unsure Ч будут возвращены пользователи, которые выбрали Ђ¬озможно пойдуї (если сообщество относитс€ к меропри€ти€м).
     managers}
    function Fields(Value: string): Integer;
    function Count(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function Sort(Value: string): Integer;
     {id_asc Ч в пор€дке возрастани€ id;
      id_desc Ч в пор€дке убывани€ id;
      time_asc Ч в хронологическом пор€дке по вступлению в сообщество;
      time_desc }
  end;

  TVkParamsGroupsGetMembersIds = record
    List: TParams;
    function GroupId(Value: Integer): Integer;
    function Filter(Value: string): Integer;
    function Count(Value: Integer = 1000): Integer;
    function Offset(Value: Integer = 0): Integer;
    function Sort(Value: string): Integer;
  end;

  TVkParamsGroupsGet = record
    List: TParams;
    function UserId(Value: string): Integer;
    function Filter(Value: string): Integer; overload;
    function Filter(Value: TVkGroupFilters): Integer; overload;
    function Fields(Value: string): Integer; overload;
    function Fields(Value: TVkGroupFields): Integer; overload;
    function Count(Value: Integer): Integer;
    function Extended(Value: Boolean): Integer;
    function Offset(Value: Integer): Integer;
  end;

  TGroupsController = class(TVkController)
  public
    function GetMembers(var Users: TVkUsers; Params: TVkParamsGroupsGetMembers): Boolean;
    /// <summary>
    /// ¬озвращает расширенную информацию о пользовател€х.
    /// </summary>
    function GetMembersIds(var Items: TVkIdList; Params: TVkParamsGroupsGetMembersIds): Boolean; overload;
    function EnableOnline(GroupId: Integer): Boolean;
    function DisableOnline(GroupId: Integer): Boolean;
    function GetOnlineStatus(var Status: TVkGroupStatus; GroupId: Integer): Boolean;
    /// <summary>
    /// ¬озвращает список сообществ указанного пользовател€.
    /// </summary>
    function Get(var Groups: TVkGroups; Params: TParams): Boolean; overload;
    /// <summary>
    /// ¬озвращает список сообществ указанного пользовател€.
    /// </summary>
    function Get(var Groups: TVkGroups; Params: TVkParamsGroupsGet): Boolean; overload;
  end;

implementation

uses
  VK.API;

{ TGroupsController }

function TGroupsController.DisableOnline(GroupId: Integer): Boolean;
begin
  GroupId := Abs(GroupId);
  with Handler.Execute('groups.disableOnline', ['group_id', GroupId.ToString]) do
    Result := Success and (Response = '1');
end;

function TGroupsController.EnableOnline(GroupId: Integer): Boolean;
begin
  GroupId := Abs(GroupId);
  with Handler.Execute('groups.enableOnline', ['group_id', GroupId.ToString]) do
    Result := Success and (Response = '1');
end;

function TGroupsController.Get(var Groups: TVkGroups; Params: TVkParamsGroupsGet): Boolean;
begin
  Result := Get(Groups, Params.List);
end;

function TGroupsController.Get(var Groups: TVkGroups; Params: TParams): Boolean;
begin
  if not Params.KeyExists('extended') then
    Params.Add('extended', True);
  with Handler.Execute('groups.get', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Groups := TVkGroups.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TGroupsController.GetMembers(var Users: TVkUsers; Params: TVkParamsGroupsGetMembers): Boolean;
begin
  if not Params.List.KeyExists('fields') then
    Params.List.Add('fields', 'domian');
  with Handler.Execute('groups.getMembers', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Users := TVkUsers.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TGroupsController.GetMembersIds(var Items: TVkIdList; Params: TVkParamsGroupsGetMembersIds): Boolean;
begin
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

function TGroupsController.GetOnlineStatus(var Status: TVkGroupStatus; GroupId: Integer): Boolean;
begin
  GroupId := Abs(GroupId);
  with Handler.Execute('groups.getOnlineStatus', ['group_id', GroupId.ToString]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Status := TVkGroupStatus.FromJsonString(Response);
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

function TVkParamsGroupsGetMembers.Fields(Value: string): Integer;
begin
  Result := List.Add('fields', Value);
end;

function TVkParamsGroupsGetMembers.Filter(Value: string): Integer;
begin
  Result := List.Add('filter', Value);
end;

function TVkParamsGroupsGetMembers.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsGetMembers.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsGroupsGetMembers.Sort(Value: string): Integer;
begin
  Result := List.Add('sort', Value);
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

function TVkParamsGroupsGet.Fields(Value: string): Integer;
begin
  Result := List.Add('fields', Value);
end;

function TVkParamsGroupsGet.Fields(Value: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsGroupsGet.Filter(Value: TVkGroupFilters): Integer;
begin
  Result := List.Add('filter', Value.ToString);
end;

function TVkParamsGroupsGet.Filter(Value: string): Integer;
begin
  Result := List.Add('filter', Value);
end;

function TVkParamsGroupsGet.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsGroupsGet.UserId(Value: string): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkParamsGroupsGetMembersIds }

function TVkParamsGroupsGetMembersIds.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsGroupsGetMembersIds.Filter(Value: string): Integer;
begin
  Result := List.Add('filter', Value);
end;

function TVkParamsGroupsGetMembersIds.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsGetMembersIds.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsGroupsGetMembersIds.Sort(Value: string): Integer;
begin
  Result := List.Add('sort', Value);
end;

end.

