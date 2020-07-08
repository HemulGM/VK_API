unit VK.Groups;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, REST.Json, System.Json, VK.Controller, VK.Types,
  VK.Entity.User, System.Classes, VK.Entity.Group, VK.CommonUtils;

type
  TVkParamsGroupsGetMembers = record
    List: TParams;
    function GroupId(Value: Integer): Integer;
    function Filter(Value: string): Integer;
    {friends � ����� ���������� ������ ������ � ���� ����������.
     unsure � ����� ���������� ������������, ������� ������� ��������� ����� (���� ���������� ��������� � ������������).
     managers}
    function Fields(Value: string): Integer;
    function Count(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function Sort(Value: string): Integer;
     {id_asc � � ������� ����������� id;
      id_desc � � ������� �������� id;
      time_asc � � ��������������� ������� �� ���������� � ����������;
      time_desc }
  end;

  TVkParamsGroupsGet = record
    List: TParams;
    function UserId(Value: string): Integer;
    {������ �������� ���������, ������� ���������� �������, ������������� ����� �������.
    �������� �������� admin, editor, moder, advertiser, groups, publics, events, hasAddress.
    �� ��������� ������������ ��� ���������� ������������.
    hasAddress - �������� ����������, � ������� ������� ������ � ��������������� �����,
    admin - ����� ���������� ����������, � ������� ������������ �������� ���������������,
    editor � ��������������� ��� ����������,
    moder � ���������������, ���������� ��� �����������,
    advertiser � ��������������.
    ���� �������� ��������� ��������, �� �� ��������� ������������.}
    function Filter(Value: string): Integer;
    {city, country, place, description, wiki_page, members_count, counters,
    start_date, finish_date, can_post, can_see_all_posts, activity, status,
    contacts, links, fixed_post, verified, site, can_create_topic}
    function Fields(Value: string): Integer;
    function Count(Value: Integer): Integer;
    function Extended(Value: Boolean): Integer;
    function Offset(Value: Integer): Integer;
  end;

  TGroupsController = class(TVkController)
  public
    function GetMembers(var Users: TVkUsers; Params: TVkParamsGroupsGetMembers): Boolean;
    /// <summary>
    /// ���������� ����������� ���������� � �������������.
    /// </summary>
    function GetMembersFull(var Users: TVkUsers; GroupId: string): Boolean; overload;
    function GetMembersIds(var Users: TIds; GroupId: string): Boolean; overload;
    function EnableOnline(GroupId: Integer): Boolean;
    function DisableOnline(GroupId: Integer): Boolean;
    function GetOnlineStatus(var Status: TVkGroupStatus; GroupId: Integer): Boolean;
    /// <summary>
    /// ���������� ������ ��������� ���������� ������������.
    /// </summary>
    function Get(var Groups: TVkGroups; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ������ ��������� ���������� ������������.
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
  if not Params.KeyExists('fields') then
    Params.Add('fields', 'description');
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

function TGroupsController.GetMembersFull(var Users: TVkUsers; GroupId: string): Boolean;
var
  Params: TParams;
  FUsers: TVkUsers;
  FCount, FNeedCount: Integer;
  FOffset: Integer;
  Res: Boolean;
begin
  Params.Add('group_id', GroupId);
  Params.Add('fields', 'domain');
  Params.Add('count', 1000);
  Params.Add('offset', 0);
  FOffset := 0;
  Users := TVkUsers.Create;
  repeat
    with Handler.Execute('groups.getMembers', Params) do
    begin
      if Success then
      begin
        FUsers := TVkUsers.FromJsonString(Response);
        FNeedCount := FUsers.Count;
        Users.Append(FUsers);
        FUsers.SaveObjects := True;
        FUsers.Free;
        FCount := Length(Users.Items);
        Res := True;
      end
      else
      begin
        Res := False;
        Break;
      end;
    end;
    FOffset := FOffset + 1000;
    Params.Add('offset', FOffset);
  until FCount >= FNeedCount;
  Result := Res;
end;

function TGroupsController.GetMembersIds(var Users: TIds; GroupId: string): Boolean;
var
  Params: TParams;
  JArray: TJSONArray;
  JsonValue: TJSONValue;
  FCount, FNeedCount: Integer;
  FOffset, FCur: Integer;
  Res: Boolean;
  i: Integer;
begin
  Params.Add('group_id', GroupId);
  Params.Add('count', 1000);
  Params.Add('offset', 0);
  FCount := 0;
  FOffset := 0;
  FCur := 0;
  repeat
    with Handler.Execute('groups.getMembers', Params) do
    begin
      Res := Success;
      if Res then
      begin
        try
          JsonValue := TJSONObject.ParseJSONValue(Response);
          try
            FNeedCount := JsonValue.GetValue<Integer>('count', 0);
            if Length(Users) <> FNeedCount then
              SetLength(Users, FNeedCount);
            JArray := JsonValue.GetValue<TJSONArray>('items', nil);
            if Assigned(JArray) then
            begin
              FCount := FCount + JArray.Count;
              for i := 0 to JArray.Count - 1 do
              begin
                Users[FCur] := JArray.Items[i].GetValue<Integer>;
                Inc(FCur);
              end;
            end
            else
              Res := False;
          finally
            JsonValue.Free;
          end;
        except
          Res := False;
          Break;
        end;
      end
      else
        Break;
    end;
    FOffset := FOffset + 1000;
    Params.Add('offset', FOffset);
  until (FCount >= FNeedCount) or (not Res);
  Result := Res;
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

end.

