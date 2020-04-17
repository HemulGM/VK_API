unit VK.Groups;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, REST.Json, System.Json, VK.Controller, VK.Types,
  VK.Entity.User, System.Classes, VK.Entity.Group;

type
  TVkGetMembersParams = record
    List: TParams;
    function GroupId(Value: string): Integer;
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

  TGroupsController = class(TVkController)
  public
    function GetMembers(var Users: TVkUsers; GroupId: string; Params: TVkGetMembersParams): Boolean;
    /// <summary>
    /// ¬озвращает расширенную информацию о пользовател€х.
    /// </summary>
    function GetMembersFull(var Users: TVkUsers; GroupId: string): Boolean; overload;
    function GetMembersIds(var Users: TIds; GroupId: string): Boolean; overload;
    function EnableOnline(GroupId: Integer): Boolean;
    function DisableOnline(GroupId: Integer): Boolean;
    function GetOnlineStatus(var Status: TVkGroupStatus; GroupId: Integer): Boolean;
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

function TGroupsController.GetMembers(var Users: TVkUsers; GroupId: string; Params: TVkGetMembersParams): Boolean;
begin
  with Handler.Execute('groups.getMembers', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      Users := TVkUsers.FromJsonString(Response);
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
      Status := TVkGroupStatus.FromJsonString(Response);
    end;
  end;
end;

{ TVkGetMembersParams }

function TVkGetMembersParams.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkGetMembersParams.Fields(Value: string): Integer;
begin
  Result := List.Add('fields', Value);
end;

function TVkGetMembersParams.Filter(Value: string): Integer;
begin
  Result := List.Add('filter', Value);
end;

function TVkGetMembersParams.GroupId(Value: string): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkGetMembersParams.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkGetMembersParams.Sort(Value: string): Integer;
begin
  Result := List.Add('sort', Value);
end;

end.

