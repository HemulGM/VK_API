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
    function &Type(const Value: TVkItemType): Integer;
    /// <summary>
    /// ������������� ��������� Like-�������: id ������������, id ���������� (�� ������ ������) ��� id ����������.
    /// ���� �������� type ����� sitepage, �� � �������� owner_id ���������� ���������� id ����������.
    /// ���� �������� �� �����, �� ���������, ��� �� ����� ���� �������������� �������� ������������,
    /// ���� �������������� �������� ���������� (���� Type ����� Sitepage)
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� Like-�������. ���� type ����� Sitepage, �� �������� ItemId ����� ���������
    /// �������� ��������� PageId, ������������ ��� ������������� ������� ���� ���������
    /// </summary>
    function ItemId(const Value: Integer): Integer;
    /// <summary>
    /// Url ��������, �� ������� ���������� ������ ���� ���������. ������������ ������ ��������� ItemId,
    /// ���� ��� ���������� ������� �� ��� ������ PageId
    /// </summary>
    function PageUrl(const Value: string): Integer;
    /// <summary>
    /// ���������, ������� �� ������� ���� �������������, ���������� ������ � ������ "��� ��������" ��� ������ ���,
    /// ������� ���������� � ��� �������. False - ������� ���� �������������
    /// </summary>
    function Filter(const Value: Boolean = False): Integer;
    /// <summary>
    /// ���������, ���������� �� ���������� ������ �������������, ������� �������� �������� �������� ������������
    /// </summary>
    function FriendsOnly(const Value: Boolean): Integer;
    /// <summary>
    /// ���������� ������������ ��������������� �������������.
    /// ���� �������� �� �����, �� ���������, ��� �� ����� 100, ���� �� ����� �������� FriendsOnly, � ��������� ������ 10.
    /// ������������ �������� ��������� 1000, ���� �� ����� �������� FriendsOnly, � ��������� ������ 100
    /// </summary>
    function Count(const Value: Integer): Integer;
    /// <summary>
    /// ��������, ������������ ������ ������, ��� ������� ������������� ������������
    /// </summary>
    function Offset(const Value: Integer = 0): Integer;
    /// <summary>
    /// �� ���������� ������ ������������
    /// </summary>
    function SkipOwn(const Value: Boolean): Integer;
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
  Params.List.Add('extended', False);
  Result := Handler.Execute('likes.getList', Params.List).GetObject(Items);
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
  Params.Add('extended', True);
  Result := Handler.Execute('likes.getList', Params).GetObject(Items);
end;

{ TVkLikesParams }

function TVkParamsLikesGetList.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsLikesGetList.Filter(const Value: Boolean): Integer;
begin
  if Value then
    Result := List.Add('filter', 'copies')
  else
  begin
    List.Remove('filter');
    Result := -1;
  end;
end;

function TVkParamsLikesGetList.FriendsOnly(const Value: Boolean): Integer;
begin
  Result := List.Add('friends_only', Value);
end;

function TVkParamsLikesGetList.ItemId(const Value: Integer): Integer;
begin
  Result := List.Add('item_id', Value);
end;

function TVkParamsLikesGetList.&Type(const Value: TVkItemType): Integer;
begin
  Result := List.Add('type', Value.ToString);
end;

function TVkParamsLikesGetList.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsLikesGetList.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsLikesGetList.PageUrl(const Value: string): Integer;
begin
  Result := List.Add('page_url', Value);
end;

function TVkParamsLikesGetList.SkipOwn(const Value: Boolean): Integer;
begin
  Result := List.Add('skip_own', Value);
end;

end.

