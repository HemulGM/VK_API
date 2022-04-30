unit VK.Likes;

interface

uses
  System.SysUtils, VK.Controller, VK.Types, VK.Entity.Common, VK.Entity.Profile,
  System.JSON, VK.Entity.Common.List;

type
  TVkParamsLikesGetList = record
    List: TParams;
    /// <summary>
    /// ��� �������
    /// </summary>
    function &Type(const Value: TVkItemType): TVkParamsLikesGetList;
    /// <summary>
    /// ������������� ��������� Like-�������: id ������������, id ���������� (�� ������ ������) ��� id ����������.
    /// ���� �������� type ����� sitepage, �� � �������� owner_id ���������� ���������� id ����������.
    /// ���� �������� �� �����, �� ���������, ��� �� ����� ���� �������������� �������� ������������,
    /// ���� �������������� �������� ���������� (���� Type ����� Sitepage)
    /// </summary>
    function OwnerId(const Value: Integer): TVkParamsLikesGetList;
    /// <summary>
    /// ������������� Like-�������. ���� type ����� Sitepage, �� �������� ItemId ����� ���������
    /// �������� ��������� PageId, ������������ ��� ������������� ������� ���� ���������
    /// </summary>
    function ItemId(const Value: Integer): TVkParamsLikesGetList;
    /// <summary>
    /// Url ��������, �� ������� ���������� ������ ���� ���������. ������������ ������ ��������� ItemId,
    /// ���� ��� ���������� ������� �� ��� ������ PageId
    /// </summary>
    function PageUrl(const Value: string): TVkParamsLikesGetList;
    /// <summary>
    /// ���������, ������� �� ������� ���� �������������, ���������� ������ � ������ "��� ��������" ��� ������ ���,
    /// ������� ���������� � ��� �������. False - ������� ���� �������������
    /// </summary>
    function Filter(const Value: Boolean = False): TVkParamsLikesGetList;
    /// <summary>
    /// ���������, ���������� �� ���������� ������ �������������, ������� �������� �������� �������� ������������
    /// </summary>
    function FriendsOnly(const Value: Boolean): TVkParamsLikesGetList;
    /// <summary>
    /// ���������� ������������ ��������������� �������������.
    /// ���� �������� �� �����, �� ���������, ��� �� ����� 100, ���� �� ����� �������� FriendsOnly, � ��������� ������ 10.
    /// ������������ �������� ��������� 1000, ���� �� ����� �������� FriendsOnly, � ��������� ������ 100
    /// </summary>
    function Count(const Value: Integer): TVkParamsLikesGetList;
    /// <summary>
    /// ��������, ������������ ������ ������, ��� ������� ������������� ������������
    /// </summary>
    function Offset(const Value: Integer = 0): TVkParamsLikesGetList;
    /// <summary>
    /// �� ���������� ������ ������������
    /// </summary>
    function SkipOwn(const Value: Boolean): TVkParamsLikesGetList;
  end;

  TLikesController = class(TVkController)
  public
    /// <summary>
    /// �������� ������ �������������, ������� �������� �������� ������ � ���� ������ ��� ��������
    /// </summary>
    function GetList(var Items: TVkProfiles; Params: TParams): Boolean; overload;
    /// <summary>
    /// �������� ������ �������������, ������� �������� �������� ������ � ���� ������ ��� ��������
    /// </summary>
    function GetList(var Items: TVkProfiles; Params: TVkParamsLikesGetList): Boolean; overload;
    /// <summary>
    /// �������� ������ ��������������� �������������, ������� �������� �������� ������ � ���� ������ ��� ��������
    /// </summary>
    function GetListIds(var Items: TVkIdList; Params: TVkParamsLikesGetList): Boolean; overload;
    /// <summary>
    /// �������� ������ �������������, ������� �������� �������� ������ � ���� ������ ��� ��������
    /// </summary>
    function Add(var Items: Integer; &Type: TVkItemType; OwnerId, ItemId: Integer; AccessKey: string = ''): Boolean; overload;
    /// <summary>
    /// ������� ��������� ������ �� ������ ��� �������� �������� ������������
    /// </summary>
    function Delete(var Items: Integer; &Type: TVkItemType; OwnerId, ItemId: Integer; AccessKey: string = ''): Boolean; overload;
    /// <summary>
    /// ���������, ��������� �� ������ � ������ ��� �������� ��������� ������������.
    /// </summary>
    function IsLiked(var Item: TVkLiked; UserId: Integer; &Type: TVkItemType; OwnerId, ItemId: Integer): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TLikesController }

function TLikesController.Add(var Items: Integer; &Type: TVkItemType; OwnerId, ItemId: Integer; AccessKey: string): Boolean;
begin
  Result := Handler.Execute('likes.add', [
    ['type', &Type.ToString],
    ['owner_id', OwnerId.ToString],
    ['item_id', ItemId.ToString],
    ['access_key', AccessKey]]).
    ResponseAsInt(Items);
end;

function TLikesController.Delete(var Items: Integer; &Type: TVkItemType; OwnerId, ItemId: Integer; AccessKey: string): Boolean;
begin
  Result := Handler.Execute('likes.delete', [
    ['type', &Type.ToString],
    ['owner_id', OwnerId.ToString],
    ['item_id', ItemId.ToString],
    ['access_key', AccessKey]]).
    ResponseAsInt(Items);
end;

function TLikesController.GetList(var Items: TVkProfiles; Params: TVkParamsLikesGetList): Boolean;
begin
  Result := GetList(Items, Params.List);
end;

function TLikesController.GetListIds(var Items: TVkIdList; Params: TVkParamsLikesGetList): Boolean;
begin
  Result := Handler.Execute('likes.getList', Params.List.Add('extended', False)).GetObject(Items);
end;

function TLikesController.IsLiked(var Item: TVkLiked; UserId: Integer; &Type: TVkItemType; OwnerId, ItemId: Integer): Boolean;
begin
  Result := Handler.Execute('likes.isLiked', [
    ['type', &Type.ToString],
    ['owner_id', OwnerId.ToString],
    ['item_id', ItemId.ToString],
    ['user_id', UserId.ToString]]).
    GetObject(Item);
end;

function TLikesController.GetList(var Items: TVkProfiles; Params: TParams): Boolean;
begin
  Result := Handler.Execute('likes.getList', Params.Add('extended', True)).GetObject(Items);
end;

{ TVkLikesParams }

function TVkParamsLikesGetList.Count(const Value: Integer): TVkParamsLikesGetList;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsLikesGetList.Filter(const Value: Boolean): TVkParamsLikesGetList;
begin
  if Value then
    List.Add('filter', 'copies')
  else
    List.Remove('filter');
  Result := Self;
end;

function TVkParamsLikesGetList.FriendsOnly(const Value: Boolean): TVkParamsLikesGetList;
begin
  List.Add('friends_only', Value);
  Result := Self;
end;

function TVkParamsLikesGetList.ItemId(const Value: Integer): TVkParamsLikesGetList;
begin
  List.Add('item_id', Value);
  Result := Self;
end;

function TVkParamsLikesGetList.&Type(const Value: TVkItemType): TVkParamsLikesGetList;
begin
  List.Add('type', Value.ToString);
  Result := Self;
end;

function TVkParamsLikesGetList.Offset(const Value: Integer): TVkParamsLikesGetList;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsLikesGetList.OwnerId(const Value: Integer): TVkParamsLikesGetList;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsLikesGetList.PageUrl(const Value: string): TVkParamsLikesGetList;
begin
  List.Add('page_url', Value);
  Result := Self;
end;

function TVkParamsLikesGetList.SkipOwn(const Value: Boolean): TVkParamsLikesGetList;
begin
  List.Add('skip_own', Value);
  Result := Self;
end;

end.

