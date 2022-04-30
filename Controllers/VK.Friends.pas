unit VK.Friends;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.Profile,
  VK.Entity.Common, VK.Entity.Common.List;

type
  TVkParamsFriendsGet = record
    List: TParams;
    /// <summary>
    /// ������������� ������������, ��� �������� ���������� �������� ������ ������.
    /// ���� �������� �� �����, �� ���������, ��� �� ����� �������������� ��������
    /// ������������ (����������� ��� ������ � ��������� AccessToken)
    /// </summary>
    function UserId(const Value: Integer): TVkParamsFriendsGet;
    /// <summary>
    /// ������������� ������ ������, ���������� ������� GetLists,
    /// ������ �� �������� ���������� ��������. ������ �������� �����������,
    /// ������ ����� �������� UserId ����� �������������� �������� ������������.
    /// ���� �������� �������� ������ ��� Standalone-���������� � ������ �������,
    /// ���������� �� ����� Implicit Flow
    /// </summary>
    function ListId(const Value: Integer): TVkParamsFriendsGet;
    /// <summary>
    /// �������, � ������� ����� ������� ������ ������.
    /// �� ��������� ������ ����������� � ������� ����������� ��������������� �������������
    /// </summary>
    function Order(const Value: TVkFriendsOrder): TVkParamsFriendsGet;
    /// <summary>
    /// ���������� ������, ������� ����� �������
    /// </summary>
    function Count(const Value: Integer = 5000): TVkParamsFriendsGet;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ������
    /// </summary>
    function Offset(const Value: Integer): TVkParamsFriendsGet;
    /// <summary>
    /// ������ �������������� �����, ������� ���������� �������
    /// </summary>
    function Fields(const Value: TVkProfileFields): TVkParamsFriendsGet;
    /// <summary>
    /// ����� ��� ��������� ����� � ������� ������������
    /// </summary>
    function NameCase(const Value: TVkNameCase): TVkParamsFriendsGet;
    /// <summary>
    /// Ref
    /// </summary>
    function Ref(const Value: string): TVkParamsFriendsGet;
  end;

  TVkParamsFriendsListEdit = record
    List: TParams;
    /// <summary>
    /// �������� ������ ������
    /// </summary>
    function Name(const Value: string): TVkParamsFriendsListEdit;
    /// <summary>
    /// ������������� ������ ������
    /// </summary>
    function ListId(const Value: Integer): TVkParamsFriendsListEdit;
    /// <summary>
    /// �������������� �������������, ���������� � ������
    /// </summary>
    function UserIds(const Value: TIdList): TVkParamsFriendsListEdit;
    /// <summary>
    /// �������������� �������������, ������� ���������� �������� � ������. (� ������ ���� �� ������� UserIds)
    /// </summary>
    function AddUserIds(const Value: TIdList): TVkParamsFriendsListEdit;
    /// <summary>
    /// �������������� �������������, ������� ���������� ������ �� ������. (� ������ ���� �� ������� UserIds)
    /// </summary>
    function DeleteUserIds(const Value: TIdList): TVkParamsFriendsListEdit;
  end;

  TVkParamsFriendsGetMutual = record
    List: TParams;
    /// <summary>
    /// ������������� ������������, ��� ������ ������������ � �������� ������������ � ��������������� TargetUid.
    /// ���� �������� �� �����, �� ���������, ��� SourceUid ����� �������������� �������� ������������
    /// </summary>
    function SourceUid(const Value: Integer): TVkParamsFriendsGetMutual;
    /// <summary>
    /// ������������� ������������, � ������� ���������� ������ ����� ������
    /// </summary>
    function TargetUid(const Value: Integer): TVkParamsFriendsGetMutual;
    /// <summary>
    /// ������ ��������������� �������������, � �������� ���������� ������ ����� ������ (�� ����� 100)
    /// </summary>
    function TargetUids(const Value: TIdList): TVkParamsFriendsGetMutual;
    /// <summary>
    /// ���������� ������ � ��������� �������
    /// </summary>
    function OrderRandom(const Value: Boolean = False): TVkParamsFriendsGetMutual;
    /// <summary>
    /// ���������� ����� ������, ������� ����� �������. (�� ��������� � ��� ����� ������)
    /// </summary>
    function Count(const Value: Integer): TVkParamsFriendsGetMutual;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ����� ������
    /// </summary>
    function Offset(const Value: Integer): TVkParamsFriendsGetMutual;
  end;

  TVkParamsFriendsGetOnline = record
    List: TParams;
    /// <summary>
    /// ������������� ������������, ��� �������� ���������� �������� ������ ������ ������.
    /// ���� �������� �� �����, �� ���������, ��� �� ����� �������������� �������� ������������
    /// </summary>
    function UserId(const Value: Integer): TVkParamsFriendsGetOnline;
    /// <summary>
    /// ������������� ������ ������. ���� �������� �� �����,
    /// ������������ ���������� ��� ���� �������, ����������� �� �����
    /// </summary>
    function ListId(const Value: Integer): TVkParamsFriendsGetOnline;
    /// <summary>
    /// True � ����� ���������� �������������� ���� OnlineMobile
    /// </summary>
    function OnlineMobile(const Value: Boolean = False): TVkParamsFriendsGetOnline;
    /// <summary>
    /// �������, � ������� ����� ������� ������ ������, ����������� �� �����.
    /// �� ��������� ������ ����������� � ������� ����������� ��������������� �������������.
    /// </summary>
    function Order(const Value: TVkFriendsOrder): TVkParamsFriendsGetOnline;
    /// <summary>
    /// ���������� ������ ������, ������� ����� �������. (�� ��������� � ��� ������ ������)
    /// </summary>
    function Count(const Value: Integer): TVkParamsFriendsGetOnline;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ������ ������
    /// </summary>
    function Offset(const Value: Integer): TVkParamsFriendsGetOnline;
  end;

  TVkParamsFriendsGetRequests = record
    List: TParams;
    /// <summary>
    /// ������������ ���������� ������ �� ���������� � ������, ������� ���������� �������� (�� ����� 1000)
    /// </summary>
    function Count(const Value: Integer = 100): TVkParamsFriendsGetRequests;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ������ �� ���������� � ������
    /// </summary>
    function Offset(const Value: Integer): TVkParamsFriendsGetRequests;
    /// <summary>
    /// ����������, ��������� �� ���������� � ������ ������ ����� ������, ���� ��� ����.
    /// �������� ��������, ��� ��� ������������� NeedMutual ����� ���������� �� ����� 2 ������
    /// </summary>
    function NeedMutual(const Value: Boolean): TVkParamsFriendsGetRequests;
    /// <summary>
    /// False � ���������� ���������� ������ � ������, True � ���������� ������������ ������������� ������
    /// </summary>
    function &Out(const Value: Boolean = False): TVkParamsFriendsGetRequests;
    /// <summary>
    /// False � ����������� �� ���� ����������, True � ����������� �� ���������� ����� ������.
    /// (���� Out = True, ���� �������� �� �����������)
    /// </summary>
    function Sort(const Value: Boolean): TVkParamsFriendsGetRequests;
    /// <summary>
    /// False - �� ���������� ������������� ������, True � ���������� ������������� ������.
    /// (���� Out = True, ������ �������� �� �����������)
    /// </summary>
    function NeedViewed(const Value: Boolean = False): TVkParamsFriendsGetRequests;
    /// <summary>
    /// True � ���������� ��������������� ������� �������������� ������, False � ���������� ������ � ������
    /// </summary>
    function Suggested(const Value: Boolean = False): TVkParamsFriendsGetRequests;
    /// <summary>
    /// ������ �������������� �����, ������� ���������� �������
    /// </summary>
    function Fields(const Value: TVkProfileFields): TVkParamsFriendsGetRequests;
    /// <summary>
    /// Ref
    /// </summary>
    function Ref(const Value: string): TVkParamsFriendsGetRequests;
  end;

  TVkParamsFriendsGetSuggestions = record
    List: TParams;
    /// <summary>
    /// ���������� ������������, ������� ���������� ������� (������������ �������� 500)
    /// </summary>
    function Count(const Value: Integer = 500): TVkParamsFriendsGetSuggestions;
    /// <summary>
    /// ��������, ����������� ��� ������ ������������ ������������ ������
    /// </summary>
    function Offset(const Value: Integer): TVkParamsFriendsGetSuggestions;
    /// <summary>
    /// ������ �������������� �����, ������� ���������� �������
    /// </summary>
    function Fields(const Value: TVkProfileFields): TVkParamsFriendsGetSuggestions;
    /// <summary>
    /// ������������, � �������� ����� ����� ������
    /// </summary>
    function FilterMutual(const Value: Boolean): TVkParamsFriendsGetSuggestions;
    /// <summary>
    /// ����� ��� ��������� ����� � ������� ������������
    /// </summary>
    function NameCase(const Value: TVkNameCase): TVkParamsFriendsGetSuggestions;
  end;

  TVkParamsFriendsSearch = record
    List: TParams;
    /// <summary>
    /// ������������� ������������, �� ������ ������ �������� ���������� ���������� �����
    /// </summary>
    function UserId(const Value: Integer = 0): TVkParamsFriendsSearch;
    /// <summary>
    /// ������ �������
    /// </summary>
    function Query(const Value: string): TVkParamsFriendsSearch;
    /// <summary>
    /// ���������� ������, ������� ����� ������� (������������ �������� 1000)
    /// </summary>
    function Count(const Value: Integer = 20): TVkParamsFriendsSearch;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ������
    /// </summary>
    function Offset(const Value: Integer): TVkParamsFriendsSearch;
    /// <summary>
    /// ������ �������������� �����, ������� ���������� �������
    /// </summary>
    function Fields(const Value: TVkProfileFields): TVkParamsFriendsSearch;
    /// <summary>
    /// ����� ��� ��������� ����� � ������� ������������
    /// </summary>
    function NameCase(const Value: TVkNameCase): TVkParamsFriendsSearch;
  end;

  TFriendsController = class(TVkController)
  public
    /// <summary>
    /// ���������� ������ ��������������� ������ ������������ ��� ����������� ���������� � ������� ������������ (��� ������������� ��������� fields)
    /// </summary>
    function Get(var Items: TVkProfiles; UserId: Integer; Fields: TVkProfileFields = []; Order: TVkFriendsOrder = TVkFriendsOrder.None): Boolean; overload;
    /// <summary>
    /// ���������� ������ ��������������� ������ ������������ ��� ����������� ���������� � ������� ������������ (��� ������������� ��������� fields)
    /// </summary>
    function Get(var Items: TVkProfiles; Fields: TVkProfileFields = []; Order: TVkFriendsOrder = TVkFriendsOrder.None): Boolean; overload;
    /// <summary>
    /// ���������� ������ ��������������� ������ ������������ ��� ����������� ���������� � ������� ������������ (��� ������������� ��������� fields)
    /// </summary>
    function Get(var Items: TVkProfiles; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ������ ��������������� ������ ������������ ��� ����������� ���������� � ������� ������������ (��� ������������� ��������� fields)
    /// </summary>
    function Get(var Items: TVkProfiles; Params: TVkParamsFriendsGet): Boolean; overload;
    /// <summary>
    /// ���������� ������ ��������������� ������ ������������ ��� ����������� ���������� � ������� ������������ (��� ������������� ��������� fields)
    /// </summary>
    function GetIds(var Items: TVkIdList; Params: TVkParamsFriendsGet): Boolean; overload;
    /// <summary>
    /// �������� ��� ������� ������ �� ���������� � ������
    /// </summary>
    function Add(var Info: TVkFriendAddInfo; UserId: Integer; Text: string; Follow: Boolean = False): Boolean;
    /// <summary>
    /// ������� ����� ������ ������ � �������� ������������
    /// </summary>
    function AddList(var ListId: Integer; Name: string; UserIds: TIdList): Boolean;
    /// <summary>
    /// ���������� ���������� � ���, �������� �� ������� ������������ � ������ � ��������� �������������
    /// </summary>
    function AreFriends(var Items: TVkFriendInfo; UserIds: TIdList; NeedSign: Boolean; Extended: Boolean): Boolean;
    /// <summary>
    /// ������� ������������ �� ������ ������ ��� ��������� ������ � ������
    /// </summary>
    function Delete(var Info: TVkFriendDeleteInfo; UserId: Integer): Boolean; overload;
    /// <summary>
    /// ������� ������������ �� ������ ������ ��� ��������� ������ � ������
    /// </summary>
    function Delete(UserId: Integer): Boolean; overload;
    /// <summary>
    /// �������� ��� �������� ������ �� ���������� � ������ ��� �������������
    /// </summary>
    function DeleteAllRequests: Boolean;
    /// <summary>
    /// ������� ������������ ������ ������ �������� ������������
    /// </summary>
    function DeleteList(ListId: Integer): Boolean;
    /// <summary>
    /// ����������� ������ ������ ��� ���������� �����
    /// </summary>
    function Edit(UserId: Integer; ListIds: TIdList): Boolean;
    /// <summary>
    /// ����������� ������ ������ ��� ���������� �����
    /// </summary>
    function EditList(Params: TParams): Boolean; overload;
    /// <summary>
    /// ����������� ������ ������ ��� ���������� �����
    /// </summary>
    function EditList(Params: TVkParamsFriendsListEdit): Boolean; overload;
    /// <summary>
    /// ���������� ������ ��������������� ������ �������� ������������, ������� ���������� ������ ����������
    /// </summary>
    function GetAppUsers(var Items: TVkIdList): Boolean;
    /// <summary>
    /// ���������� ������ ������ ������������, � ������� ���������������� ��� ��������� � ������� ���������� ������ ������ � �������� ������
    /// </summary>
    function GetByPhones(var Items: TVkProfiles; Phones: TArrayOfString; Fields: TVkProfileFields): Boolean;
    /// <summary>
    /// ���������� ������ ����� ������ ������������
    /// </summary>
    function GetLists(var Items: TVkFriendsList; UserId: Integer; ReturnSystem: Boolean = False): Boolean; overload;
    /// <summary>
    /// ���������� ������ ��������������� ����� ������ ����� ����� �������������
    /// </summary>
    function GetMutual(var Items: TVkIdList; Params: TVkParamsFriendsGetMutual): Boolean;
    /// <summary>
    /// ���������� ������ ��������������� ������ ������������, ����������� �� �����
    /// </summary>
    function GetOnline(var Items: TVkFriendsOnline; Params: TVkParamsFriendsGetOnline): Boolean;
    /// <summary>
    /// ���������� ������ ��������������� ������� ����������� ������ �������� ������������
    /// </summary>
    function GetRecent(var Items: TVkIdList; Count: Integer = 100): Boolean;
    /// <summary>
    /// ���������� ���������� � ���������� ��� ������������ ������� �� ���������� � ������ ��� �������� ������������
    /// </summary>
    function GetRequests(var Items: TVkProfiles; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ���������� � ���������� ��� ������������ ������� �� ���������� � ������ ��� �������� ������������
    /// </summary>
    function GetRequests(var Items: TVkProfiles; Params: TVkParamsFriendsGetRequests): Boolean; overload;
    /// <summary>
    /// ���������� ���������� � ���������� ��� ������������ ������� �� ���������� � ������ ��� �������� ������������
    /// </summary>
    function GetRequestsIds(var Items: TVkIdList; Params: TVkParamsFriendsGetRequests): Boolean; overload;
    /// <summary>
    /// ���������� ������ �������� �������������, ������� ����� ���� �������� �������� ������������
    /// </summary>
    function GetSuggestions(var Items: TVkProfiles; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ������ �������� �������������, ������� ����� ���� �������� �������� ������������
    /// </summary>
    function GetSuggestions(var Items: TVkProfiles; Params: TVkParamsFriendsGetSuggestions): Boolean; overload;
    /// <summary>
    /// ��������� ������ �� ������ ������ �������������
    /// </summary>
    function Search(var Items: TVkProfiles; Params: TParams): Boolean; overload;
    /// <summary>
    /// ��������� ������ �� ������ ������ �������������
    /// </summary>
    function Search(var Items: TVkProfiles; Params: TVkParamsFriendsSearch): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TFriendsController }

function TFriendsController.Get(var Items: TVkProfiles; Fields: TVkProfileFields; Order: TVkFriendsOrder): Boolean;
var
  Params: TVkParamsFriendsGet;
begin
  if Fields <> [] then
    Params.Fields(Fields);
  if Order <> TVkFriendsOrder.None then
    Params.Order(Order);
  Result := Get(Items, Params);
end;

function TFriendsController.Get(var Items: TVkProfiles; UserId: Integer; Fields: TVkProfileFields; Order: TVkFriendsOrder): Boolean;
var
  Params: TVkParamsFriendsGet;
begin
  Params.UserId(UserId);
  if Fields <> [] then
    Params.Fields(Fields);
  if Order <> TVkFriendsOrder.None then
    Params.Order(Order);
  Result := Get(Items, Params);
end;

function TFriendsController.Get(var Items: TVkProfiles; Params: TVkParamsFriendsGet): Boolean;
begin
  Result := Get(Items, Params.List);
end;

function TFriendsController.Get(var Items: TVkProfiles; Params: TParams): Boolean;
begin
  if not Params.KeyExists('fields') then
    Params.Add('fields', 'domian');

  Result := Handler.Execute('friends.get', Params).GetObject(Items);
end;

function TFriendsController.Add(var Info: TVkFriendAddInfo; UserId: Integer; Text: string; Follow: Boolean): Boolean;
var
  Value: Integer;
begin
  with Handler.Execute('friends.add', [['user_id', UserId.ToString], ['text', Text], ['follow', BoolToString(Follow)]]) do
  begin
    Result := ResponseAsInt(Value);
    if Result then
      Info := TVkFriendAddInfo(Value);
  end;
end;

function TFriendsController.AddList(var ListId: Integer; Name: string; UserIds: TIdList): Boolean;
begin
  Result := Handler.Execute('friends.add', [['name', Name], ['user_ids', UserIds.ToString]]).ResponseAsInt(ListId);
end;

function TFriendsController.AreFriends(var Items: TVkFriendInfo; UserIds: TIdList; NeedSign, Extended: Boolean): Boolean;
begin
  Result := Handler.Execute('friends.areFriends', [
    ['user_ids', UserIds.ToString],
    ['need_sign', BoolToString(NeedSign)],
    ['extended', BoolToString(Extended)]]).
    GetObjects(Items);
end;

function TFriendsController.Delete(UserId: Integer): Boolean;
var
  Info: TVkFriendDeleteInfo;
begin
  Result := False;
  with Handler.Execute('friends.delete', ['user_id', UserId.ToString]) do
  begin
    if GetObject(Info) then
    begin
      try
        try
          Result := Info.Success;
        finally
          Info.Free;
        end;
      except
        Result := False;
      end;
    end;
  end;
end;

function TFriendsController.DeleteAllRequests: Boolean;
begin
  Result := Handler.Execute('friends.deleteAllRequests').ResponseIsTrue;
end;

function TFriendsController.DeleteList(ListId: Integer): Boolean;
begin
  Result := Handler.Execute('friends.deleteList', ['list_id', ListId.ToString]).ResponseIsTrue;
end;

function TFriendsController.Edit(UserId: Integer; ListIds: TIdList): Boolean;
begin
  Result := Handler.Execute('friends.edit', [
    ['user_id', UserId.ToString],
    ['list_ids', ListIds.ToString]]).
    ResponseIsTrue;
end;

function TFriendsController.EditList(Params: TVkParamsFriendsListEdit): Boolean;
begin
  Result := EditList(Params.List);
end;

function TFriendsController.EditList(Params: TParams): Boolean;
begin
  Result := Handler.Execute('friends.editList', Params).ResponseIsTrue;
end;

function TFriendsController.Delete(var Info: TVkFriendDeleteInfo; UserId: Integer): Boolean;
begin
  Result := Handler.Execute('friends.delete', ['user_id', UserId.ToString]).GetObject(Info);
end;

function TFriendsController.GetAppUsers(var Items: TVkIdList): Boolean;
begin
  Result := Handler.Execute('friends.getAppUsers').GetObject(Items);
end;

function TFriendsController.GetByPhones(var Items: TVkProfiles; Phones: TArrayOfString; Fields: TVkProfileFields): Boolean;
begin
  with Handler.Execute('friends.getByPhones', [['phones', Phones.ToString], ['fields', Fields.ToString]]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkProfiles.FromJsonString<TVkProfiles>(ResponseAsItems);
        Items.Count := Length(Items.Items);
      except
        Result := False;
      end;
    end;
  end;
end;

function TFriendsController.GetIds(var Items: TVkIdList; Params: TVkParamsFriendsGet): Boolean;
begin
  Result := Handler.Execute('friends.get', Params.List).GetObject(Items);
end;

function TFriendsController.GetLists(var Items: TVkFriendsList; UserId: Integer; ReturnSystem: Boolean): Boolean;
begin
  Result := Handler.Execute('friends.getLists', [
    ['user_id', UserId.ToString],
    ['return_system', BoolToString(ReturnSystem)]]).
    GetObject(Items);
end;

function TFriendsController.GetMutual(var Items: TVkIdList; Params: TVkParamsFriendsGetMutual): Boolean;
begin
  Result := Handler.Execute('friends.getMutual').GetObject(Items);
end;

function TFriendsController.GetOnline(var Items: TVkFriendsOnline; Params: TVkParamsFriendsGetOnline): Boolean;
begin
  Result := Handler.Execute('friends.getOnline', Params.List).GetObject(Items);
end;

function TFriendsController.GetRecent(var Items: TVkIdList; Count: Integer): Boolean;
begin
  Result := Handler.Execute('friends.getRecent', ['count', Count.ToString]).GetObject(Items);
end;

function TFriendsController.GetRequests(var Items: TVkProfiles; Params: TVkParamsFriendsGetRequests): Boolean;
begin
  Result := GetRequests(Items, Params.List);
end;

function TFriendsController.GetRequestsIds(var Items: TVkIdList; Params: TVkParamsFriendsGetRequests): Boolean;
begin
  Result := Handler.Execute('friends.getRequests', Params.List).GetObject(Items);
end;

function TFriendsController.GetSuggestions(var Items: TVkProfiles; Params: TVkParamsFriendsGetSuggestions): Boolean;
begin
  Result := GetSuggestions(Items, Params.List);
end;

function TFriendsController.Search(var Items: TVkProfiles; Params: TVkParamsFriendsSearch): Boolean;
begin
  Result := Search(Items, Params.List);
end;

function TFriendsController.Search(var Items: TVkProfiles; Params: TParams): Boolean;
begin
  Result := Handler.Execute('friends.search', Params).GetObject(Items);
end;

function TFriendsController.GetSuggestions(var Items: TVkProfiles; Params: TParams): Boolean;
begin
  Result := Handler.Execute('friends.getSuggestions', Params).GetObject(Items);
end;

function TFriendsController.GetRequests(var Items: TVkProfiles; Params: TParams): Boolean;
begin
  Result := Handler.Execute('friends.getRequests', Params.Add('extended', True)).GetObject(Items);
end;

{ TVkFriendsGetParams }

function TVkParamsFriendsGet.Count(const Value: Integer): TVkParamsFriendsGet;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsFriendsGet.Fields(const Value: TVkProfileFields): TVkParamsFriendsGet;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsFriendsGet.ListId(const Value: Integer): TVkParamsFriendsGet;
begin
  List.Add('list_id', Value);
  Result := Self;
end;

function TVkParamsFriendsGet.NameCase(const Value: TVkNameCase): TVkParamsFriendsGet;
begin
  List.Add('name_case', Value.ToString);
  Result := Self;
end;

function TVkParamsFriendsGet.Offset(const Value: Integer): TVkParamsFriendsGet;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsFriendsGet.Order(const Value: TVkFriendsOrder): TVkParamsFriendsGet;
begin
  List.Add('order', Value.ToString);
  Result := Self;
end;

function TVkParamsFriendsGet.Ref(const Value: string): TVkParamsFriendsGet;
begin
  List.Add('ref', Value);
  Result := Self;
end;

function TVkParamsFriendsGet.UserId(const Value: Integer): TVkParamsFriendsGet;
begin
  List.Add('user_id', Value);
  Result := Self;
end;

{ TVkParamsFriendsListEdit }

function TVkParamsFriendsListEdit.AddUserIds(const Value: TIdList): TVkParamsFriendsListEdit;
begin
  List.Add('add_user_ids', Value);
  Result := Self;
end;

function TVkParamsFriendsListEdit.DeleteUserIds(const Value: TIdList): TVkParamsFriendsListEdit;
begin
  List.Add('delete_user_ids', Value);
  Result := Self;
end;

function TVkParamsFriendsListEdit.ListId(const Value: Integer): TVkParamsFriendsListEdit;
begin
  List.Add('list_id', Value);
  Result := Self;
end;

function TVkParamsFriendsListEdit.Name(const Value: string): TVkParamsFriendsListEdit;
begin
  List.Add('name', Value);
  Result := Self;
end;

function TVkParamsFriendsListEdit.UserIds(const Value: TIdList): TVkParamsFriendsListEdit;
begin
  List.Add('user_ids', Value);
  Result := Self;
end;

{ TVkParamsFriendsGetMutual }

function TVkParamsFriendsGetMutual.Count(const Value: Integer): TVkParamsFriendsGetMutual;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsFriendsGetMutual.Offset(const Value: Integer): TVkParamsFriendsGetMutual;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsFriendsGetMutual.OrderRandom(const Value: Boolean): TVkParamsFriendsGetMutual;
begin
  if Value then
    List.Add('order', 'random')
  else
    List.Add('order', '');
  Result := Self;
end;

function TVkParamsFriendsGetMutual.SourceUid(const Value: Integer): TVkParamsFriendsGetMutual;
begin
  List.Add('source_uid', Value);
  Result := Self;
end;

function TVkParamsFriendsGetMutual.TargetUid(const Value: Integer): TVkParamsFriendsGetMutual;
begin
  List.Add('target_uids', Value);
  Result := Self;
end;

function TVkParamsFriendsGetMutual.TargetUids(const Value: TIdList): TVkParamsFriendsGetMutual;
begin
  List.Add('target_uids', Value);
  Result := Self;
end;

{ TVkParamsFriendsGetOnline }

function TVkParamsFriendsGetOnline.Count(const Value: Integer): TVkParamsFriendsGetOnline;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsFriendsGetOnline.ListId(const Value: Integer): TVkParamsFriendsGetOnline;
begin
  List.Add('list_id', Value);
  Result := Self;
end;

function TVkParamsFriendsGetOnline.Offset(const Value: Integer): TVkParamsFriendsGetOnline;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsFriendsGetOnline.OnlineMobile(const Value: Boolean): TVkParamsFriendsGetOnline;
begin
  List.Add('online_mobile', Value);
  Result := Self;
end;

function TVkParamsFriendsGetOnline.Order(const Value: TVkFriendsOrder): TVkParamsFriendsGetOnline;
begin
  List.Add('order', Value.ToString);
  Result := Self;
end;

function TVkParamsFriendsGetOnline.UserId(const Value: Integer): TVkParamsFriendsGetOnline;
begin
  List.Add('user_id', Value);
  Result := Self;
end;

{ TVkParamsFriendsGetRequests }

function TVkParamsFriendsGetRequests.Count(const Value: Integer): TVkParamsFriendsGetRequests;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsFriendsGetRequests.Fields(const Value: TVkProfileFields): TVkParamsFriendsGetRequests;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsFriendsGetRequests.NeedMutual(const Value: Boolean): TVkParamsFriendsGetRequests;
begin
  List.Add('need_mutual', Value);
  Result := Self;
end;

function TVkParamsFriendsGetRequests.NeedViewed(const Value: Boolean): TVkParamsFriendsGetRequests;
begin
  List.Add('need_viewed', Value);
  Result := Self;
end;

function TVkParamsFriendsGetRequests.Offset(const Value: Integer): TVkParamsFriendsGetRequests;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsFriendsGetRequests.out(const Value: Boolean): TVkParamsFriendsGetRequests;
begin
  List.Add('out', Value);
  Result := Self;
end;

function TVkParamsFriendsGetRequests.Ref(const Value: string): TVkParamsFriendsGetRequests;
begin
  List.Add('ref', Value);
  Result := Self;
end;

function TVkParamsFriendsGetRequests.Sort(const Value: Boolean): TVkParamsFriendsGetRequests;
begin
  List.Add('sort', Value);
  Result := Self;
end;

function TVkParamsFriendsGetRequests.Suggested(const Value: Boolean): TVkParamsFriendsGetRequests;
begin
  List.Add('suggested', Value);
  Result := Self;
end;

{ TVkParamsFriendsGetSuggestions }

function TVkParamsFriendsGetSuggestions.Count(const Value: Integer): TVkParamsFriendsGetSuggestions;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsFriendsGetSuggestions.Fields(const Value: TVkProfileFields): TVkParamsFriendsGetSuggestions;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsFriendsGetSuggestions.FilterMutual(const Value: Boolean): TVkParamsFriendsGetSuggestions;
begin
  if Value then
    List.Add('filter', 'mutual')
  else
    List.Add('filter', '');
  Result := Self;
end;

function TVkParamsFriendsGetSuggestions.NameCase(const Value: TVkNameCase): TVkParamsFriendsGetSuggestions;
begin
  List.Add('name_case', Value.ToString);
  Result := Self;
end;

function TVkParamsFriendsGetSuggestions.Offset(const Value: Integer): TVkParamsFriendsGetSuggestions;
begin
  List.Add('offset', Value);
  Result := Self;
end;

{ TVkParamsFriendsSearch }

function TVkParamsFriendsSearch.Count(const Value: Integer): TVkParamsFriendsSearch;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsFriendsSearch.Fields(const Value: TVkProfileFields): TVkParamsFriendsSearch;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsFriendsSearch.NameCase(const Value: TVkNameCase): TVkParamsFriendsSearch;
begin
  List.Add('name_case', Value.ToString);
  Result := Self;
end;

function TVkParamsFriendsSearch.Offset(const Value: Integer): TVkParamsFriendsSearch;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsFriendsSearch.Query(const Value: string): TVkParamsFriendsSearch;
begin
  List.Add('q', Value);
  Result := Self;
end;

function TVkParamsFriendsSearch.UserId(const Value: Integer): TVkParamsFriendsSearch;
begin
  List.Add('user_id', Value);
  Result := Self;
end;

end.

