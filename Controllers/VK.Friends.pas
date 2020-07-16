unit VK.Friends;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.User;

type
  /// <summary>
  /// <b>hints</b> � ����������� �� ��������, ���������� ����, ��� ������ ����������� � ������� ��� ������ (��� �������� �������� ������ ��� Standalone-���������� � ������ �������, ���������� �� ����� Implicit Flow.).
  /// <b>random</b> � ���������� ������ � ��������� �������.
  /// <b>mobile</b> � ���������� ���� ��� ������, � ������� ����������� ��������� ����������.
  /// <b>name</b> � ����������� �� �����. ������ ��� ���������� �������� ��������, ��� ��� ������ ����� �������� ���� ������ � �� ������ ��������� ���������� count. (�������� ������ ��� ���������� ��������� fields).
  /// </summary>
  TVkFriendsSort = (fsNone, fsHints, fsRandom, fsMobile, fsName);

  TVkFriendsSortHelper = record helper for TVkFriendsSort
    function ToConst: string; inline;
  end;

  TVkFriendAddInfo = (faiSuccess = 1, faiApproved = 2, faiResended = 4);

  /// <summary>
  /// <b>random</b> - ���������� ������ � ��������� �������, <b>hints</b> - ����������� �� ��������, ���������� ����, ��� ������ ����������� � ������� ��� ������ (������ �������� �������� ������ ��� Desktop-����������).
  /// </summary>
  TVkFriendsOnlineOrder = (fooRandom, fooHints);

  TVkFriendsOnlineOrderHelper = record helper for TVkFriendsOnlineOrder
    function ToString: string; inline;
  end;

  TVkParamsFriendsGet = record
    List: TParams;
    function UserId(Value: Integer): Integer;
    function ListId(Value: Integer): Integer;
    function Order(Value: TVkFriendsSort): Integer;
    function Count(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function Fields(Value: string): Integer; overload;
    function Fields(Value: TVkFriendFields): Integer; overload;
    function NameCase(Value: TVkNameCase): Integer;
    function Ref(Value: string): Integer;
  end;

  TVkParamsFriendsListEdit = record
    List: TParams;
    function Name(Value: string): Integer;
    function ListId(Value: Integer): Integer;
    function UserIds(Value: TIds): Integer;
    function AddUserIds(Value: TIds): Integer;
    function DeleteUserIds(Value: TIds): Integer;
  end;

  TVkParamsFriendsGetMutual = record
    List: TParams;
    function SourceUid(Value: Integer): Integer;
    function TargetUid(Value: Integer): Integer;
    function TargetUids(Value: TIds): Integer;
    function OrderRandom(Value: Boolean): Integer;
    function Count(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
  end;

  TVkParamsFriendsGetOnline = record
    List: TParams;
    function UserId(Value: Integer): Integer;
    function ListId(Value: Integer): Integer;
    function OnlineMobile(Value: Boolean): Integer;
    function Order(Value: TVkFriendsOnlineOrder): Integer;
    function Count(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
  end;

  TFriendsController = class(TVkController)
  public
    /// <summary>
    /// ���������� ������ ��������������� ������ ������������ ��� ����������� ���������� � ������� ������������ (��� ������������� ��������� fields).
    /// </summary>
    function Get(var Items: TVkUsers; UserId: Integer; Fields: TVkFriendFields = []; Order: TVkFriendsSort = fsNone):
      Boolean; overload;
    /// <summary>
    /// ���������� ������ ��������������� ������ ������������ ��� ����������� ���������� � ������� ������������ (��� ������������� ��������� fields).
    /// </summary>
    function Get(var Items: TVkUsers; Fields: TVkFriendFields = []; Order: TVkFriendsSort = fsNone): Boolean; overload;
    /// <summary>
    /// ���������� ������ ��������������� ������ ������������ ��� ����������� ���������� � ������� ������������ (��� ������������� ��������� fields).
    /// </summary>
    function Get(var Items: TVkUsers; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ������ ��������������� ������ ������������ ��� ����������� ���������� � ������� ������������ (��� ������������� ��������� fields).
    /// </summary>
    function Get(var Items: TVkUsers; Params: TVkParamsFriendsGet): Boolean; overload;
    /// <summary>
    /// �������� ��� ������� ������ �� ���������� � ������.
    /// </summary>
    function Add(var Info: TVkFriendAddInfo; UserId: Integer; Text: string; Follow: Boolean = False): Boolean;
    /// <summary>
    /// ������� ����� ������ ������ � �������� ������������.
    /// </summary>
    function AddList(var ListId: Integer; Name: string; UserIds: TIds): Boolean;
    /// <summary>
    /// ���������� ���������� � ���, �������� �� ������� ������������ � ������ � ��������� �������������.
    /// </summary>
    function AreFriends(var Items: TVkFriendInfo; UserIds: TIds; NeedSign: Boolean; Extended: Boolean): Boolean;
    /// <summary>
    /// ������� ������������ �� ������ ������ ��� ��������� ������ � ������.
    /// </summary>
    function Delete(var Info: TVkFriendDeleteInfo; UserId: Integer): Boolean; overload;
    /// <summary>
    /// ������� ������������ �� ������ ������ ��� ��������� ������ � ������.
    /// </summary>
    function Delete(UserId: Integer): Boolean; overload;
    /// <summary>
    /// �������� ��� �������� ������ �� ���������� � ������ ��� �������������.
    /// </summary>
    function DeleteAllRequests: Boolean;
    /// <summary>
    /// ������� ������������ ������ ������ �������� ������������.
    /// </summary>
    function DeleteList(ListId: Integer): Boolean;
    /// <summary>
    /// ����������� ������ ������ ��� ���������� �����.
    /// </summary>
    function Edit(UserId: Integer; ListIds: TIds): Boolean;
    /// <summary>
    /// ����������� ������ ������ ��� ���������� �����.
    /// </summary>
    function EditList(Params: TParams): Boolean; overload;
    /// <summary>
    /// ����������� ������ ������ ��� ���������� �����.
    /// </summary>
    function EditList(Params: TVkParamsFriendsListEdit): Boolean; overload;
    /// <summary>
    /// ���������� ������ ��������������� ������ �������� ������������, ������� ���������� ������ ����������.
    /// </summary>
    function GetAppUsers(var Items: TIds): Boolean;
    /// <summary>
    /// ���������� ������ ������ ������������, � ������� ���������������� ��� ��������� � ������� ���������� ������ ������ � �������� ������.
    /// </summary>
    function GetByPhones(var Items: TVkUsers; Phones: TArrayOfString; Fields: TVkFriendFields): Boolean;
    /// <summary>
    /// ���������� ������ ����� ������ ������������.
    /// </summary>
    function GetLists(var Items: TVkFriendsList; UserId: Integer; ReturnSystem: Boolean = False): Boolean; overload;
    /// <summary>
    /// ���������� ������ ��������������� ����� ������ ����� ����� �������������.
    /// </summary>
    function GetMutual(var Items: TIds; Params: TVkParamsFriendsGetMutual): Boolean;
    /// <summary>
    /// ���������� ������ ��������������� ������ ������������, ����������� �� �����.
    /// </summary>
    function GetOnline(var Items: TVkFriendsOnline; Params: TVkParamsFriendsGetOnline): Boolean;
    /// <summary>
    /// ���������� ������ ��������������� ������� ����������� ������ �������� ������������.
    /// </summary>
    function GetRecent(var Items: TIds; Count: Integer = 100): Boolean;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.Json;

{ TFriendsController }

function TFriendsController.Get(var Items: TVkUsers; Fields: TVkFriendFields; Order: TVkFriendsSort): Boolean;
var
  Params: TVkParamsFriendsGet;
begin
  if Fields <> [] then
    Params.Fields(Fields);
  if Order <> fsNone then
    Params.Order(Order);
  Result := Get(Items, Params);
end;

function TFriendsController.Get(var Items: TVkUsers; UserId: Integer; Fields: TVkFriendFields; Order: TVkFriendsSort): Boolean;
var
  Params: TVkParamsFriendsGet;
begin
  Params.UserId(UserId);
  if Fields <> [] then
    Params.Fields(Fields);
  if Order <> fsNone then
    Params.Order(Order);
  Result := Get(Items, Params);
end;

function TFriendsController.Get(var Items: TVkUsers; Params: TVkParamsFriendsGet): Boolean;
begin
  Result := Get(Items, Params.List);
end;

function TFriendsController.Get(var Items: TVkUsers; Params: TParams): Boolean;
begin
  if not Params.KeyExists('fields') then
    Params.Add('fields', 'domian');

  with Handler.Execute('friends.get', Params) do
  begin
    Result := Success;
    if Result then
    try
      Items := TVkUsers.FromJsonString(Response);
    except
      Result := False;
    end;
  end;
end;

function TFriendsController.Add(var Info: TVkFriendAddInfo; UserId: Integer; Text: string; Follow: Boolean): Boolean;
begin
  with Handler.Execute('friends.add', [['user_id', UserId.ToString], ['text', Text], ['follow', BoolToString(Follow)]]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Info := TVkFriendAddInfo(StrToInt(Response));
      except
        Result := False;
      end;
    end;
  end;
end;

function TFriendsController.AddList(var ListId: Integer; Name: string; UserIds: TIds): Boolean;
begin
  with Handler.Execute('friends.add', [['name', Name], ['user_ids', UserIds.ToString]]) do
  begin
    Result := Success and TryStrToInt(Response, ListId);
  end;
end;

function TFriendsController.AreFriends(var Items: TVkFriendInfo; UserIds: TIds; NeedSign, Extended: Boolean): Boolean;
begin
  with Handler.Execute('friends.areFriends', [['user_ids', UserIds.ToString], ['need_sign', BoolToString(NeedSign)], ['extended',
    BoolToString(Extended)]]) do
  begin
    Result := Success;
    if Result then
    try
      Items := TVkFriendInfo.FromJsonString(AppendItemsTag(Response));
    except
      Result := False;
    end;
  end;
end;

function TFriendsController.Delete(UserId: Integer): Boolean;
var
  Info: TVkFriendDeleteInfo;
begin
  with Handler.Execute('friends.delete', ['user_id', UserId.ToString]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Info := TVkFriendDeleteInfo.FromJsonString(Response);
        Result := Info.Success;
        Info.Free;
      except
        Result := False;
      end;
    end;
  end;
end;

function TFriendsController.DeleteAllRequests: Boolean;
begin
  with Handler.Execute('friends.deleteAllRequests') do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFriendsController.DeleteList(ListId: Integer): Boolean;
begin
  with Handler.Execute('friends.deleteList', ['list_id', ListId.ToString]) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFriendsController.Edit(UserId: Integer; ListIds: TIds): Boolean;
begin
  with Handler.Execute('friends.edit', [['user_id', UserId.ToString], ['list_ids', ListIds.ToString]]) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFriendsController.EditList(Params: TVkParamsFriendsListEdit): Boolean;
begin
  Result := EditList(Params.List);
end;

function TFriendsController.EditList(Params: TParams): Boolean;
begin
  with Handler.Execute('friends.editList', Params) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFriendsController.Delete(var Info: TVkFriendDeleteInfo; UserId: Integer): Boolean;
begin
  with Handler.Execute('friends.delete', ['user_id', UserId.ToString]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Info := TVkFriendDeleteInfo.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TFriendsController.GetAppUsers(var Items: TIds): Boolean;
var
  RespJson: TJSONValue;
begin
  with Handler.Execute('friends.getAppUsers') do
  begin
    Result := Success;
    if Result then
    begin
      try
        RespJson := TJSONObject.ParseJSONValue(JSON);
        Items := RespJson.GetValue<TIds>('response', []);
        RespJson.Free;
      except
        Result := False;
      end;
    end;
  end;
end;

function TFriendsController.GetByPhones(var Items: TVkUsers; Phones: TArrayOfString; Fields: TVkFriendFields): Boolean;
begin
  with Handler.Execute('friends.getByPhones', [['phones', Phones.ToString], ['fields', Fields.ToString]]) do
  begin
    Result := Success;
    if Result then
    try
      Items := TVkUsers.FromJsonString(AppendItemsTag(Response));
      Items.Count := Length(Items.Items);
    except
      Result := False;
    end;
  end;
end;

function TFriendsController.GetLists(var Items: TVkFriendsList; UserId: Integer; ReturnSystem: Boolean): Boolean;
begin
  with Handler.Execute('friends.getLists', [['user_id', UserId.ToString], ['return_system', BoolToString(ReturnSystem)]]) do
  begin
    Result := Success;
    if Result then
    try
      Items := TVkFriendsList.FromJsonString(Response);
    except
      Result := False;
    end;
  end;
end;

function TFriendsController.GetMutual(var Items: TIds; Params: TVkParamsFriendsGetMutual): Boolean;
var
  RespJson: TJSONValue;
begin
  with Handler.Execute('friends.getMutual') do
  begin
    Result := Success;
    if Result then
    begin
      try
        RespJson := TJSONObject.ParseJSONValue(JSON);
        Items := RespJson.GetValue<TIds>('response', []);
        RespJson.Free;
      except
        Result := False;
      end;
    end;
  end;
end;

function TFriendsController.GetOnline(var Items: TVkFriendsOnline; Params: TVkParamsFriendsGetOnline): Boolean;
begin
  with Handler.Execute('friends.getOnline', Params.List) do
  begin
    Result := Success;
    if Result then
    try
      Items := TVkFriendsOnline.FromJsonString(Response);
    except
      Result := False;
    end;
  end;
end;

function TFriendsController.GetRecent(var Items: TIds; Count: Integer): Boolean;
var
  RespJson: TJSONValue;
begin
  with Handler.Execute('friends.getRecent', ['count', Count.ToString]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        RespJson := TJSONObject.ParseJSONValue(JSON);
        Items := RespJson.GetValue<TIds>('response', []);
        RespJson.Free;
      except
        Result := False;
      end;
    end;
  end;
end;

{ TVkFriendsGetParams }

function TVkParamsFriendsGet.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsFriendsGet.Fields(Value: string): Integer;
begin
  Result := List.Add('fields', Value);
end;

function TVkParamsFriendsGet.Fields(Value: TVkFriendFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsFriendsGet.ListId(Value: Integer): Integer;
begin
  Result := List.Add('list_id', Value);
end;

function TVkParamsFriendsGet.NameCase(Value: TVkNameCase): Integer;
begin
  Result := List.Add('name_case', Value.ToConst);
end;

function TVkParamsFriendsGet.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsFriendsGet.Order(Value: TVkFriendsSort): Integer;
begin
  Result := List.Add('order', Value.ToConst);
end;

function TVkParamsFriendsGet.Ref(Value: string): Integer;
begin
  Result := List.Add('ref', Value);
end;

function TVkParamsFriendsGet.UserId(Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkFriendsSortHelper }

function TVkFriendsSortHelper.ToConst: string;
begin
  case Self of
    fsHints:
      Exit('hints');
    fsRandom:
      Exit('random');
    fsMobile:
      Exit('mobile');
    fsName:
      Exit('name');
  else
    Exit('');
  end;
end;

{ TVkParamsFriendsListEdit }

function TVkParamsFriendsListEdit.AddUserIds(Value: TIds): Integer;
begin
  Result := List.Add('add_user_ids', Value);
end;

function TVkParamsFriendsListEdit.DeleteUserIds(Value: TIds): Integer;
begin
  Result := List.Add('delete_user_ids', Value);
end;

function TVkParamsFriendsListEdit.ListId(Value: Integer): Integer;
begin
  Result := List.Add('list_id', Value);
end;

function TVkParamsFriendsListEdit.Name(Value: string): Integer;
begin
  Result := List.Add('name', Value);
end;

function TVkParamsFriendsListEdit.UserIds(Value: TIds): Integer;
begin
  Result := List.Add('user_ids', Value);
end;

{ TVkParamsFriendsGetMutual }

function TVkParamsFriendsGetMutual.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsFriendsGetMutual.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsFriendsGetMutual.OrderRandom(Value: Boolean): Integer;
begin
  if Value then
    Result := List.Add('order', 'random')
  else
    Result := List.Add('order', '');
end;

function TVkParamsFriendsGetMutual.SourceUid(Value: Integer): Integer;
begin
  Result := List.Add('source_uid', Value);
end;

function TVkParamsFriendsGetMutual.TargetUid(Value: Integer): Integer;
begin
  Result := List.Add('target_uids', Value);
end;

function TVkParamsFriendsGetMutual.TargetUids(Value: TIds): Integer;
begin
  Result := List.Add('target_uids', Value);
end;

{ TVkFriendsOnlineOrderHelper }

function TVkFriendsOnlineOrderHelper.ToString: string;
begin
  case Self of
    fooRandom:
      Result := 'random';
    fooHints:
      Result := 'hints';
  else
    Result := '';
  end;
end;

{ TVkParamsFriendsGetOnline }

function TVkParamsFriendsGetOnline.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsFriendsGetOnline.ListId(Value: Integer): Integer;
begin
  Result := List.Add('list_id', Value);
end;

function TVkParamsFriendsGetOnline.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsFriendsGetOnline.OnlineMobile(Value: Boolean): Integer;
begin
  Result := List.Add('online_mobile', Value);
end;

function TVkParamsFriendsGetOnline.Order(Value: TVkFriendsOnlineOrder): Integer;
begin
  Result := List.Add('order', Value.ToString);
end;

function TVkParamsFriendsGetOnline.UserId(Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

end.

