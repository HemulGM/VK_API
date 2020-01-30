unit VK.Groups;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, REST.Json, System.Json, VK.Controller,
  VK.Types, VK.Entity.User, System.Classes, VCl.Forms;

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
    function GetMembers(var Users: TVkUserList; GroupId: string; Params: TVkGetMembersParams): Boolean;
    /// <summary>
    /// ¬озвращает расширенную информацию о пользовател€х.
    /// </summary>
    function GetMembersFull(var Users: TVkUserList; GroupId: string): Boolean; overload;
  end;

const
  AllFields = 'sex, bdate, city, country, photo_50, photo_100, photo_200_orig, ' +
    'photo_200, photo_400_orig, photo_max, photo_max_orig, online, ' +
    'online_mobile, lists, domain, has_mobile, contacts, connections, ' +
    'site, education, universities, schools, can_post, can_see_all_posts, ' +
    'can_see_audio, can_write_private_message, status, last_seen, ' +
    'common_count, relation, relatives';

implementation

uses
  VK.API;

{ TGroupsController }

function TGroupsController.GetMembers(var Users: TVkUserList; GroupId: string; Params: TVkGetMembersParams): Boolean;
var
  JArray: TJSONArray;
  JsonValue: TJSONValue;
  Count, i: Integer;
begin
  with Handler.Execute('groups.getMembers', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      Result := False;
      JsonValue := TJSONObject.ParseJSONValue(Response);
      Count := JsonValue.GetValue<Integer>('count', -1);
      if Count >= 0 then
      begin
        Result := True;
        if Count > 0 then
        begin
          SetLength(Users, Count);
          JArray := JsonValue.GetValue<TJSONArray>('items', nil);
          if Assigned(JArray) then
          begin
            for i := 0 to JArray.Count - 1 do
              Users[i] := TVkUser.FromJsonString(JArray.Items[i].ToJSON);
          end
          else
            Result := False;
        end;
      end;
      JsonValue.Free;
    end;
  end;
end;

function TGroupsController.GetMembersFull(var Users: TVkUserList; GroupId: string): Boolean;
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
  Params.Add('fields', 'domain');
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
        FNeedCount := JsonValue.GetValue<Integer>('count', 0);
        if Length(Users) <> FNeedCount then
          SetLength(Users, FNeedCount);
        JArray := JsonValue.GetValue<TJSONArray>('items', nil);
        if Assigned(JArray) then
        begin
          FCount := FCount + JArray.Count;
          for i := 0 to JArray.Count - 1 do
          begin
            Users[FCur] := TVkUser.FromJsonString(JArray.Items[i].ToJSON);
            Inc(FCur);
          end;
        end
        else
          Res := False;
        JsonValue.Free;
      end
      else
        Break;
    end;
    FOffset := FOffset + 1000;
    Params.Add('offset', FOffset);
  until (FCount >= FNeedCount) or (not Res);
  Result := Res;
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

