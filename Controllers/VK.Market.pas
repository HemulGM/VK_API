unit VK.Market;

interface

uses
  System.SysUtils, System.Generics.Collections, VK.Controller, VK.Types,
  VK.Entity.Media, VK.Entity.Market, VK.Entity.Market.Album,
  VK.Entity.Market.Order;

type
  TVkParamsMarketAdd = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� ������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// �������� ������. ����������� �� ����� ��������� � ��������� cp1251 (����������� ����� 4, ������������ ����� 100)
    /// </summary>
    function Name(const Value: string): Integer;
    /// <summary>
    /// �������� ������ (����������� ����� 10)
    /// </summary>
    function Description(const Value: string): Integer;
    /// <summary>
    /// ������������� ��������� ������ (market.getCategories)
    /// </summary>
    function CategoryId(const Value: Integer): Integer;
    /// <summary>
    /// ���� ������
    /// </summary>
    function Price(const Value: Extended): Integer;
    /// <summary>
    /// ������ ���� ������
    /// </summary>
    function OldPrice(const Value: Extended): Integer;
    /// <summary>
    /// ������ ������ (True � ����� ������, False � ����� �� ������)
    /// </summary>
    function Deleted(const Value: Boolean): Integer;
    /// <summary>
    /// ������������� ���������� ������� ������.
    /// ���������� ������ ���� ��������� � ������� ������ photos.getMarketUploadServer, ������� �������� MainPhoto
    /// </summary>
    function MainPhotoId(const Value: Integer): Integer;
    /// <summary>
    /// �������������� �������������� ���������� ������.
    /// ���������� ������ ���� ��������� � ������� ������ photos.getMarketUploadServer
    /// </summary>
    function PhotoIds(const Value: TIdList): Integer;
    /// <summary>
    /// ������ �� ���� ������ (������������ ����� 320)
    /// </summary>
    function Url(const Value: string): Integer;
    /// <summary>
    /// ������ � ����������� (������������ �������� 100000)
    /// </summary>
    function DimensionWidth(const Value: Integer): Integer;
    /// <summary>
    /// ������ � ����������� (������������ �������� 100000)
    /// </summary>
    function DimensionHeight(const Value: Integer): Integer;
    /// <summary>
    /// ������� � ����������� (������������ �������� 100000)
    /// </summary>
    function DimensionLength(const Value: Integer): Integer;
    /// <summary>
    /// ��� � ������� (������������ �������� 100000000)
    /// </summary>
    function Weight(const Value: Integer): Integer;
    /// <summary>
    /// ������� ������, ������������ ������ (������������ ����� 50)
    /// </summary>
    function Sku(const Value: string): Integer;
  end;

  TVkParamsMarketCreateComment = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� ������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ������
    /// </summary>
    function ItemId(const Value: Integer): Integer;
    /// <summary>
    /// ����� ����������� (�������� ������������, ���� �� ����� �������� Attachments)
    /// ������������ ���������� ��������: 2048
    /// </summary>
    function Message(const Value: string): Integer;
    /// <summary>
    /// ������ ��������, ����������� � �����������
    /// </summary>
    function Attachments(const Value: TAttachmentArray): Integer;
    /// <summary>
    /// True � ����������� ����� ����������� �� ����� ������,
    /// False � ����������� ����� ����������� �� ����� ������������ (�� ���������)
    /// </summary>
    function FromGroup(const Value: Boolean = False): Integer;
    /// <summary>
    /// ������������� �����������, � ����� �� ������� ������ ���� �������� ����� �����������
    /// </summary>
    function ReplyToComment(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �������
    /// </summary>
    function StickerId(const Value: Integer): Integer;
    /// <summary>
    /// ���������� �������������, ��������������� ��� �������������� ��������� �������� ����������� �����������
    /// </summary>
    function Guid(const Value: string): Integer;
  end;

  TVkParamsMarketEdit = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� ������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ������
    /// </summary>
    function ItemId(const Value: Integer): Integer;
    /// <summary>
    /// �������� ������. ����������� �� ����� ��������� � ��������� cp1251 (����������� ����� 4, ������������ ����� 100)
    /// </summary>
    function Name(const Value: string): Integer;
    /// <summary>
    /// �������� ������ (����������� ����� 10)
    /// </summary>
    function Description(const Value: string): Integer;
    /// <summary>
    /// ������������� ��������� ������ (market.getCategories)
    /// </summary>
    function CategoryId(const Value: Integer): Integer;
    /// <summary>
    /// ���� ������
    /// </summary>
    function Price(const Value: Extended): Integer;
    /// <summary>
    /// ������ ���� ������
    /// </summary>
    function OldPrice(const Value: Extended): Integer;
    /// <summary>
    /// ������ ������ (True � ����� ����������, False � ����� ��������)
    /// </summary>
    function Deleted(const Value: Boolean): Integer;
    /// <summary>
    /// ������������� ���������� ������� ������.
    /// ���������� ������ ���� ��������� � ������� ������ photos.getMarketUploadServer, ������� �������� MainPhoto
    /// </summary>
    function MainPhotoId(const Value: Integer): Integer;
    /// <summary>
    /// �������������� �������������� ���������� ������.
    /// ���������� ������ ���� ��������� � ������� ������ photos.getMarketUploadServer
    /// </summary>
    function PhotoIds(const Value: TIdList): Integer;
    /// <summary>
    /// ������ �� ���� ������ (������������ ����� 320)
    /// </summary>
    function Url(const Value: string): Integer;
    /// <summary>
    /// ������ � ����������� (������������ �������� 100000)
    /// </summary>
    function DimensionWidth(const Value: Integer): Integer;
    /// <summary>
    /// ������ � ����������� (������������ �������� 100000)
    /// </summary>
    function DimensionHeight(const Value: Integer): Integer;
    /// <summary>
    /// ������� � ����������� (������������ �������� 100000)
    /// </summary>
    function DimensionLength(const Value: Integer): Integer;
    /// <summary>
    /// ��� � ������� (������������ �������� 100000000)
    /// </summary>
    function Weight(const Value: Integer): Integer;
    /// <summary>
    /// ������� ������, ������������ ������ (������������ ����� 50)
    /// </summary>
    function Sku(const Value: string): Integer;
  end;

  TVkParamsMarketEditAlbum = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� ��������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ��������
    /// </summary>
    function AlbumId(const Value: Integer): Integer;
    /// <summary>
    /// �������� �������� (������������ ����� 128)
    /// </summary>
    function Title(const Value: string): Integer;
    /// <summary>
    /// ������������� ����������-������� ��������.
    /// ���������� ������ ���� ��������� � ������� ������ photos.getMarkeAlbumUploadServer
    /// </summary>
    function PhotoId(const Value: Integer): Integer;
    /// <summary>
    /// ��������� �������� �������� (True � ���������, False � ���)
    /// </summary>
    function MainAlbum(const Value: Boolean): Integer;
  end;

  TVkParamsMarketEditComment = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� ������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �����������
    /// </summary>
    function CommentId(const Value: Integer): Integer;
    /// <summary>
    /// ����� ����������� (�������� ������������, ���� �� ����� �������� Attachments)
    /// ������������ ���������� ��������: 2048
    /// </summary>
    function Message(const Value: string): Integer;
    /// <summary>
    /// ������ ��������, ����������� � �����������
    /// </summary>
    function Attachments(const Value: TAttachmentArray): Integer;
  end;

  TVkParamsMarketEditOrder = record
    List: TParams;
    /// <summary>
    /// ������������� ������������
    /// </summary>
    function UserId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ������
    /// </summary>
    function OrderId(const Value: Integer): Integer;
    /// <summary>
    /// ����������� �������� (������������ ����� 400)
    /// </summary>
    function MerchantComment(const Value: string): Integer;
    /// <summary>
    /// ������ ������
    /// </summary>
    function Status(const Value: TVkOrderStatus): Integer;
    /// <summary>
    /// ����-�����
    /// </summary>
    function TrackNumber(const Value: string): Integer;
    /// <summary>
    /// ������ �������
    /// </summary>
    function PaymentStatus(const Value: TVkPaymentStatus): Integer;
    /// <summary>
    /// ��������� ��������
    /// </summary>
    function DeliveryPrice(const Value: Integer): Integer;
    /// <summary>
    /// ������
    /// </summary>
    function Width(const Value: Integer): Integer;
    /// <summary>
    /// �����
    /// </summary>
    function Length(const Value: Integer): Integer;
    /// <summary>
    /// ������
    /// </summary>
    function Height(const Value: Integer): Integer;
    /// <summary>
    /// ���
    /// </summary>
    function Weight(const Value: Integer): Integer;
    /// <summary>
    /// ����������� ��� ������������
    /// </summary>
    function CommentForUser(const Value: string): Integer;
  end;

  TVkParamsMarketGet = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� �������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ��������, ������ �� ������� ����� �������
    /// </summary>
    function AlbumId(const Value: Integer = 0): Integer;
    /// <summary>
    /// True � ����� ���������� �������������� ���� Likes, CanComment, CanRepost, Photos, ViewsCount
    /// </summary>
    function Extended(const Value: Boolean = False): Integer;
    /// <summary>
    /// �������� ������������ ������� ���������� ������ ��� ������� ������������� ������������
    /// </summary>
    function Offset(const Value: Integer): Integer;
    /// <summary>
    /// ���������� ������������ ������� (������������ �������� 200)
    /// </summary>
    function Count(const Value: Integer = 100): Integer;
  end;

  TVkParamsMarketGetComments = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� ������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ������
    /// </summary>
    function ItemId(const Value: Integer): Integer;
    /// <summary>
    /// True � ���������� ���������� � ������
    /// </summary>
    function NeedLikes(const Value: Boolean): Integer;
    /// <summary>
    /// ������������� �����������, ������� � �������� ����� ������� ������
    /// </summary>
    function StartCommentId(const Value: Integer): Integer;
    /// <summary>
    /// �����, ����������� ��� ��������� ���������� ������� �����������
    /// </summary>
    function Offset(const Value: Integer = 0): Integer;
    /// <summary>
    /// ����� ������������, ������� ���������� �������� (������������ �������� 100)
    /// </summary>
    function Count(const Value: Integer = 20): Integer;
    /// <summary>
    /// ������� ���������� ������������ (asc � �� ������ � �����, desc - �� ����� � ������)
    /// </summary>
    function Sort(const Value: TVkSort = TVkSort.Asc): Integer;
    /// <summary>
    /// True � ����������� � ������ ����� ���������� � ���� ��������������� ��������,
    /// ������������� ����� ���������� ������ �������� Profiles, Groups
    /// </summary>
    function Extended(const Value: Boolean): Integer;
    /// <summary>
    /// ������ �������������� ����� ��������, ������� ���������� �������
    /// </summary>
    function Fields(const UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
  end;

  TVkParamsMarketReorderAlbums = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� ��������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ��������
    /// </summary>
    function AlbumId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ��������, ����� ������� ������� ��������� �������
    /// </summary>
    function Before(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ��������, ����� ������� ������� ��������� �������
    /// </summary>
    function After(const Value: Integer): Integer;
  end;

  TVkParamsMarketReorderItems = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� ������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ��������, � ������� ��������� �����. 0 � ��� ���������� ������ ������ �������
    /// </summary>
    function AlbumId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ������
    /// </summary>
    function ItemId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ������, ����� ������� ������� ��������� �������
    /// </summary>
    function Before(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ������, ����� �������� ������� ��������� �������
    /// </summary>
    function After(const Value: Integer): Integer;
  end;

  TVkParamsMarketSearch = record
    List: TParams;
    /// <summary>
    /// ������������� ����������, �������� ����������� ������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ��������, ������ �� ������� ����� �������
    /// </summary>
    function AlbumId(const Value: Integer): Integer;
    /// <summary>
    /// ������ ���������� �������
    /// </summary>
    function Query(const Value: string): Integer;
    /// <summary>
    /// ����������� �������� ���� ������� � ����� ����� ������� ������. ��������, 100000
    /// </summary>
    function PriceFrom(const Value: Integer): Integer;
    /// <summary>
    /// ������������ �������� ���� ������� � ����� ����� ������� ������. ��������, 1410000
    /// </summary>
    function PriceTo(const Value: Integer): Integer;
    /// <summary>
    /// ��� ����������
    /// </summary>
    function Sort(const Value: TVkMarketSort = TVkMarketSort.User): Integer;
    /// <summary>
    /// False � �� ������������ �������� �������, True � ������������ �������� �������
    /// </summary>
    function Rev(const Value: Boolean = True): Integer;
    /// <summary>
    /// �������� ������������ ������� ���������� ������ ��� ������� ������������� ������������
    /// </summary>
    function Offset(const Value: Integer): Integer;
    /// <summary>
    /// ���������� ������������ ������� (������������ �������� 200)
    /// </summary>
    function Count(const Value: Integer = 20): Integer;
    /// <summary>
    /// True � ����� ���������� �������������� ���� Likes, CanComment, CanRepost, Photos, ViewsCount
    /// </summary>
    function Extended(const Value: Boolean = False): Integer;
    /// <summary>
    /// [�� ��������� ��������]
    /// </summary>
    function Status(const Value: Integer = 0): Integer;
    /// <summary>
    /// [�� ��������� ��������, �������� ������ �� ������������]
    /// </summary>
    function Tags(const Value: TIdList): Integer;
  end;

  TMarketController = class(TVkController)
  public
    /// <summary>
    /// ��������� ����� �����.
    /// </summary>
    function Add(var Id: Integer; Params: TParams): Boolean; overload;
    /// <summary>
    /// ��������� ����� �����.
    /// </summary>
    function Add(var Id: Integer; Params: TVkParamsMarketAdd): Boolean; overload;
    /// <summary>
    /// ��������� ����� �������� � ��������.
    /// </summary>
    function AddAlbum(var Id: Integer; OwnerId: Integer; Title: string; PhotoId: Integer = -1; MainAlbum: Boolean = False): Boolean;
    /// <summary>
    /// ��������� ����� � ���� ��� ��������� ��������� ��������.
    /// </summary>
    function AddToAlbum(const OwnerId, ItemId: Integer; AlbumIds: TIdList): Boolean;
    /// <summary>
    /// ������� ����� ����������� � ������.
    /// </summary>
    function CreateComment(var Id: Integer; Params: TParams): Boolean; overload;
    /// <summary>
    /// ������� ����� ����������� � ������.
    /// </summary>
    function CreateComment(var Id: Integer; Params: TVkParamsMarketCreateComment): Boolean; overload;
    /// <summary>
    /// ������� �����.
    /// </summary>
    function Delete(const OwnerId, ItemId: Integer): Boolean;
    /// <summary>
    /// ������� �������� � ��������.
    /// </summary>
    function DeleteAlbum(const OwnerId, AlbumId: Integer): Boolean;
    /// <summary>
    /// ������� ����������� � ������.
    /// </summary>
    function DeleteComment(const OwnerId, CommentId: Integer): Boolean;
    /// <summary>
    /// ����������� �����.
    /// </summary>
    function Edit(Params: TParams): Boolean; overload;
    /// <summary>
    /// ����������� �����.
    /// </summary>
    function Edit(Params: TVkParamsMarketEdit): Boolean; overload;
    /// <summary>
    /// ����������� �������� � ��������.
    /// </summary>
    function EditAlbum(Params: TParams): Boolean; overload;
    /// <summary>
    /// ����������� �������� � ��������.
    /// </summary>
    function EditAlbum(Params: TVkParamsMarketEditAlbum): Boolean; overload;
    /// <summary>
    /// �������� ����� ����������� � ������.
    /// </summary>
    function EditComment(Params: TParams): Boolean; overload;
    /// <summary>
    /// �������� ����� ����������� � ������.
    /// </summary>
    function EditComment(Params: TVkParamsMarketEditComment): Boolean; overload;
    /// <summary>
    /// ����������� �����.
    /// </summary>
    function EditOrder(Params: TParams): Boolean; overload;
    /// <summary>
    /// ����������� �����.
    /// </summary>
    function EditOrder(Params: TVkParamsMarketEditOrder): Boolean; overload;
    /// <summary>
    /// ���������� ������ ������� � ����������.
    /// </summary>
    function Get(var Items: TVkProducts; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ������ ������� � ����������.
    /// </summary>
    function Get(var Items: TVkProducts; Params: TVkParamsMarketGet): Boolean; overload;
    /// <summary>
    /// ���������� ������ �������� � ��������.
    /// </summary>
    function GetAlbumById(var Items: TVkMarketAlbums; const OwnerId: Integer; AlbumIds: TIdList): Boolean; overload;
    /// <summary>
    /// ���������� ������ �������� � ��������.
    /// </summary>
    function GetAlbums(var Items: TVkMarketAlbums; const OwnerId: Integer; Offset: Integer = 0; Count: Integer = 50): Boolean; overload;
    /// <summary>
    /// ���������� ���������� � ������� �� ���������������.
    /// </summary>
    function GetById(var Items: TVkProducts; const ItemIds: TIdList; Extended: Boolean = False): Boolean; overload;
    /// <summary>
    /// ���������� ������ ��������� ��� �������.
    /// </summary>
    function GetCategories(var Items: TVkProductCategories; const Offset: Integer = 0; Count: Integer = 10): Boolean; overload;
    /// <summary>
    /// ���������� ������ ������������ � ������.
    /// </summary>
    function GetComments(var Items: TVkComments; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ������ ������������ � ������.
    /// </summary>
    function GetComments(var Items: TVkComments; Params: TVkParamsMarketGetComments): Boolean; overload;
    /// <summary>
    /// ���������� ������ ����������.
    /// </summary>
    function GetGroupOrders(var Items: TVkOrders; GroupId: Integer; Offset: Integer = 0; Count: Integer = 10): Boolean; overload;
    /// <summary>
    /// ���������� ����� �� ��������������.
    /// </summary>
    function GetOrderById(var Item: TVkOrder; const OrderId: Integer; UserId: Integer = 0; Extended: Boolean = False): Boolean; overload;
    /// <summary>
    /// ���������� ������ � ������.
    /// </summary>
    function GetOrderItems(var Items: TVkProducts; const OrderId: Integer; UserId: Integer = 0; Offset: Integer = 0; Count: Integer = 50): Boolean; overload;
    /// <summary>
    /// ���������� ������.
    /// </summary>
    function GetOrders(var Items: TVkOrders; const Offset: Integer = 0; Count: Integer = 10; Extended: Boolean = False): Boolean; overload;
    /// <summary>
    /// ������� ����� �� ����� ��� ���������� ��������� ��������.
    /// </summary>
    function RemoveFromAlbum(const ItemId, OwnerId: Integer; AlbumIds: TIdList): Boolean; overload;
    /// <summary>
    /// �������� ��������� �������� � �������� � ������.
    /// </summary>
    function ReorderAlbums(const Params: TVkParamsMarketReorderAlbums): Boolean;
    /// <summary>
    /// �������� ��������� ������ � ��������.
    /// </summary>
    function ReorderItems(const Params: TVkParamsMarketReorderItems): Boolean;
    /// <summary>
    /// ��������� ��������� ������ �� �����.
    /// </summary>
    function Report(const OwnerId, ItemId: Integer; Reason: TVkMediaReportReason): Boolean;
    /// <summary>
    /// ��������� �������� ������ �� ����������� � ������.
    /// </summary>
    function ReportComment(const OwnerId, CommentId: Integer; Reason: TVkMediaReportReason): Boolean;
    /// <summary>
    /// ��������������� ��������� �����.
    /// </summary>
    function Restore(const OwnerId, ItemId: Integer): Boolean;
    /// <summary>
    /// ��������������� ��������� ����������� � ������.
    /// </summary>
    function RestoreComment(const OwnerId, CommentId: Integer): Boolean;
    /// <summary>
    /// ���� ������ � �������� ����������.
    /// </summary>
    function Search(var Items: TVkProducts; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���� ������ � �������� ����������.
    /// </summary>
    function Search(var Items: TVkProducts; Params: TVkParamsMarketSearch): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TMarketController }

function TMarketController.Add(var Id: Integer; Params: TVkParamsMarketAdd): Boolean;
begin
  Result := Add(Id, Params.List);
end;

function TMarketController.CreateComment(var Id: Integer; Params: TVkParamsMarketCreateComment): Boolean;
begin
  Result := CreateComment(Id, Params.List);
end;

function TMarketController.Edit(Params: TVkParamsMarketEdit): Boolean;
begin
  Result := Edit(Params.List);
end;

function TMarketController.EditAlbum(Params: TVkParamsMarketEditAlbum): Boolean;
begin
  Result := EditAlbum(Params.List);
end;

function TMarketController.EditComment(Params: TVkParamsMarketEditComment): Boolean;
begin
  Result := EditComment(Params.List);
end;

function TMarketController.EditOrder(Params: TVkParamsMarketEditOrder): Boolean;
begin
  Result := EditOrder(Params.List);
end;

function TMarketController.AddAlbum(var Id: Integer; OwnerId: Integer; Title: string; PhotoId: Integer; MainAlbum: Boolean): Boolean;
var
  Params: TParams;
begin
  Params.Add('owner_id', OwnerId);
  Params.Add('title', Title);
  if PhotoId <> -1 then
    Params.Add('photo_id', PhotoId);
  Params.Add('main_album', MainAlbum);
  Result := Handler.Execute('market.addAlbum', Params).ResponseAsInt(Id);
end;

function TMarketController.AddToAlbum(const OwnerId, ItemId: Integer; AlbumIds: TIdList): Boolean;
var
  Params: TParams;
begin
  Params.Add('owner_id', OwnerId);
  Params.Add('item_id', ItemId);
  Params.Add('album_ids', AlbumIds);
  Result := Handler.Execute('market.addToAlbum', Params).ResponseIsTrue;
end;

function TMarketController.Add(var Id: Integer; Params: TParams): Boolean;
begin
  Result := Handler.Execute('market.add', Params).ResponseAsInt(Id);
end;

function TMarketController.Delete(const OwnerId, ItemId: Integer): Boolean;
begin
  Result := Handler.Execute('market.delete', [
    ['owner_id', OwnerId.ToString],
    ['item_id', ItemId.ToString]]).
    ResponseIsTrue;
end;

function TMarketController.DeleteAlbum(const OwnerId, AlbumId: Integer): Boolean;
begin
  Result := Handler.Execute('market.deleteAlbum', [
    ['owner_id', OwnerId.ToString],
    ['album_id', AlbumId.ToString]]).
    ResponseIsTrue;
end;

function TMarketController.DeleteComment(const OwnerId, CommentId: Integer): Boolean;
begin
  Result := Handler.Execute('market.deleteComment', [
    ['owner_id', OwnerId.ToString],
    ['comment_id', CommentId.ToString]]).
    ResponseIsTrue;
end;

function TMarketController.EditOrder(Params: TParams): Boolean;
begin
  Result := Handler.Execute('market.editOrder', Params).ResponseIsTrue;
end;

function TMarketController.EditComment(Params: TParams): Boolean;
begin
  Result := Handler.Execute('market.editComment', Params).ResponseIsTrue;
end;

function TMarketController.EditAlbum(Params: TParams): Boolean;
begin
  Result := Handler.Execute('market.editAlbum', Params).ResponseIsTrue;
end;

function TMarketController.Edit(Params: TParams): Boolean;
begin
  Result := Handler.Execute('market.edit', Params).ResponseIsTrue;
end;

function TMarketController.CreateComment(var Id: Integer; Params: TParams): Boolean;
begin
  Result := Handler.Execute('market.createComment', Params).ResponseAsInt(Id);
end;

function TMarketController.Get(var Items: TVkProducts; Params: TVkParamsMarketGet): Boolean;
begin
  Result := Get(Items, Params.List);
end;

function TMarketController.Get(var Items: TVkProducts; Params: TParams): Boolean;
begin
  Result := Handler.Execute('market.get', Params).GetObject(Items);
end;

function TMarketController.GetAlbumById(var Items: TVkMarketAlbums; const OwnerId: Integer; AlbumIds: TIdList): Boolean;
begin
  Result := Handler.Execute('market.getAlbumById', [
    ['owner_id', OwnerId.ToString],
    ['album_ids', AlbumIds.ToString]]).
    GetObject(Items);
end;

function TMarketController.GetAlbums(var Items: TVkMarketAlbums; const OwnerId: Integer; Offset, Count: Integer): Boolean;
begin
  Result := Handler.Execute('market.getAlbums', [
    ['owner_id', OwnerId.ToString],
    ['count', Count.ToString],
    ['offset', Offset.ToString]]).
    GetObject(Items);
end;

function TMarketController.GetById(var Items: TVkProducts; const ItemIds: TIdList; Extended: Boolean): Boolean;
begin
  Result := Handler.Execute('market.getById', [
    ['item_ids', ItemIds.ToString],
    ['extended', BoolToString(Extended)]]).
    GetObject(Items);
end;

function TMarketController.GetCategories(var Items: TVkProductCategories; const Offset: Integer; Count: Integer): Boolean;
begin
  Result := Handler.Execute('market.getCategories', [
    ['count', Count.ToString],
    ['offset', Offset.ToString]]).
    GetObject(Items);
end;

function TMarketController.GetComments(var Items: TVkComments; Params: TVkParamsMarketGetComments): Boolean;
begin
  Result := GetComments(Items, Params.List);
end;

function TMarketController.GetGroupOrders(var Items: TVkOrders; GroupId, Offset, Count: Integer): Boolean;
begin
  Result := Handler.Execute('market.getGroupOrders', [
    ['group_id', GroupId.ToString],
    ['count', Count.ToString],
    ['offset', Offset.ToString]]).
    GetObject(Items);
end;

function TMarketController.GetOrderById(var Item: TVkOrder; const OrderId: Integer; UserId: Integer; Extended: Boolean): Boolean;
var
  Params: TParams;
begin
  Params.Add('order_id', OrderId);
  Params.Add('user_id', UserId);
  if Extended then
    Params.Add('extended', Extended);
  Result := Handler.Execute('market.getOrderById', Params).GetObject(Item);
end;

function TMarketController.GetOrderItems(var Items: TVkProducts; const OrderId: Integer; UserId, Offset, Count: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('order_id', OrderId);
  Params.Add('user_id', UserId);
  Params.Add('offset', Offset);
  Params.Add('count', Count);
  Result := Handler.Execute('market.getOrderItems', Params).GetObject(Items);
end;

function TMarketController.GetOrders(var Items: TVkOrders; const Offset: Integer; Count: Integer; Extended: Boolean): Boolean;
var
  Params: TParams;
begin
  Params.Add('offset', Offset);
  Params.Add('count', Count);
  if Extended then
    Params.Add('extended', Extended);
  Result := Handler.Execute('market.getOrders', Params).GetObject(Items);
end;

function TMarketController.RemoveFromAlbum(const ItemId, OwnerId: Integer; AlbumIds: TIdList): Boolean;
var
  Params: TParams;
begin
  Params.Add('owner_id', OwnerId);
  Params.Add('item_id', ItemId);
  Params.Add('album_ids', AlbumIds);
  Result := Handler.Execute('market.removeFromAlbum', Params).ResponseIsTrue;
end;

function TMarketController.ReorderAlbums(const Params: TVkParamsMarketReorderAlbums): Boolean;
begin
  Result := Handler.Execute('market.reorderAlbums', Params.List).ResponseIsTrue;
end;

function TMarketController.ReorderItems(const Params: TVkParamsMarketReorderItems): Boolean;
begin
  Result := Handler.Execute('market.reorderItems', Params.List).ResponseIsTrue;
end;

function TMarketController.Report(const OwnerId, ItemId: Integer; Reason: TVkMediaReportReason): Boolean;
var
  Params: TParams;
begin
  Params.Add('owner_id', OwnerId);
  Params.Add('item_id', ItemId);
  Params.Add('reason', Reason.ToString);
  Result := Handler.Execute('market.report', Params).ResponseIsTrue;
end;

function TMarketController.ReportComment(const OwnerId, CommentId: Integer; Reason: TVkMediaReportReason): Boolean;
var
  Params: TParams;
begin
  Params.Add('owner_id', OwnerId);
  Params.Add('comment_id', CommentId);
  Params.Add('reason', Reason.ToString);
  Result := Handler.Execute('market.reportComment', Params).ResponseIsTrue;
end;

function TMarketController.Restore(const OwnerId, ItemId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('owner_id', OwnerId);
  Params.Add('item_id', ItemId);
  Result := Handler.Execute('market.restore', Params).ResponseIsTrue;
end;

function TMarketController.RestoreComment(const OwnerId, CommentId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('owner_id', OwnerId);
  Params.Add('comment_id', CommentId);
  Result := Handler.Execute('market.restoreComment', Params).ResponseIsTrue;
end;

function TMarketController.Search(var Items: TVkProducts; Params: TVkParamsMarketSearch): Boolean;
begin
  Result := Search(Items, Params.List);
end;

function TMarketController.Search(var Items: TVkProducts; Params: TParams): Boolean;
begin
  Result := Handler.Execute('market.search', Params).GetObject(Items);
end;

function TMarketController.GetComments(var Items: TVkComments; Params: TParams): Boolean;
begin
  Result := Handler.Execute('market.getComments', Params).GetObject(Items);
end;

{ TVkParamsMarketGet }

function TVkParamsMarketGet.AlbumId(const Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsMarketGet.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsMarketGet.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsMarketGet.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsMarketGet.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

{ TVkParamsMarketAdd }

function TVkParamsMarketAdd.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsMarketAdd.Name(const Value: string): Integer;
begin
  Result := List.Add('name', Value);
end;

function TVkParamsMarketAdd.Description(const Value: string): Integer;
begin
  Result := List.Add('description', Value);
end;

function TVkParamsMarketAdd.CategoryId(const Value: Integer): Integer;
begin
  Result := List.Add('category_id', Value);
end;

function TVkParamsMarketAdd.Price(const Value: Extended): Integer;
begin
  Result := List.Add('price', Value);
end;

function TVkParamsMarketAdd.Sku(const Value: string): Integer;
begin
  Result := List.Add('sku', Value);
end;

function TVkParamsMarketAdd.OldPrice(const Value: Extended): Integer;
begin
  Result := List.Add('old_price', Value);
end;

function TVkParamsMarketAdd.Deleted(const Value: Boolean): Integer;
begin
  Result := List.Add('deleted', Value);
end;

function TVkParamsMarketAdd.MainPhotoId(const Value: Integer): Integer;
begin
  Result := List.Add('main_photo_id', Value);
end;

function TVkParamsMarketAdd.PhotoIds(const Value: TIdList): Integer;
begin
  Result := List.Add('photo_ids', Value);
end;

function TVkParamsMarketAdd.Url(const Value: string): Integer;
begin
  Result := List.Add('url', Value);
end;

function TVkParamsMarketAdd.DimensionWidth(const Value: Integer): Integer;
begin
  Result := List.Add('dimension_width', Value);
end;

function TVkParamsMarketAdd.DimensionHeight(const Value: Integer): Integer;
begin
  Result := List.Add('dimension_height', Value);
end;

function TVkParamsMarketAdd.DimensionLength(const Value: Integer): Integer;
begin
  Result := List.Add('dimension_length', Value);
end;

function TVkParamsMarketAdd.Weight(const Value: Integer): Integer;
begin
  Result := List.Add('weight', Value);
end;

{ TVkParamsMarketCreateComment }

function TVkParamsMarketCreateComment.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsMarketCreateComment.ItemId(const Value: Integer): Integer;
begin
  Result := List.Add('item_id', Value);
end;

function TVkParamsMarketCreateComment.Message(const Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsMarketCreateComment.Attachments(const Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsMarketCreateComment.FromGroup(const Value: Boolean): Integer;
begin
  Result := List.Add('from_group', Value);
end;

function TVkParamsMarketCreateComment.ReplyToComment(const Value: Integer): Integer;
begin
  Result := List.Add('reply_to_comment', Value);
end;

function TVkParamsMarketCreateComment.StickerId(const Value: Integer): Integer;
begin
  Result := List.Add('sticker_id', Value);
end;

function TVkParamsMarketCreateComment.Guid(const Value: string): Integer;
begin
  Result := List.Add('guid', Value);
end;

{ TVkParamsMarketEdit }

function TVkParamsMarketEdit.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsMarketEdit.ItemId(const Value: Integer): Integer;
begin
  Result := List.Add('item_id', Value);
end;

function TVkParamsMarketEdit.Name(const Value: string): Integer;
begin
  Result := List.Add('name', Value);
end;

function TVkParamsMarketEdit.Description(const Value: string): Integer;
begin
  Result := List.Add('description', Value);
end;

function TVkParamsMarketEdit.CategoryId(const Value: Integer): Integer;
begin
  Result := List.Add('category_id', Value);
end;

function TVkParamsMarketEdit.Price(const Value: Extended): Integer;
begin
  Result := List.Add('price', Value);
end;

function TVkParamsMarketEdit.Sku(const Value: string): Integer;
begin
  Result := List.Add('sku', Value);
end;

function TVkParamsMarketEdit.OldPrice(const Value: Extended): Integer;
begin
  Result := List.Add('old_price', Value);
end;

function TVkParamsMarketEdit.Deleted(const Value: Boolean): Integer;
begin
  Result := List.Add('deleted', Value);
end;

function TVkParamsMarketEdit.MainPhotoId(const Value: Integer): Integer;
begin
  Result := List.Add('main_photo_id', Value);
end;

function TVkParamsMarketEdit.PhotoIds(const Value: TIdList): Integer;
begin
  Result := List.Add('photo_ids', Value);
end;

function TVkParamsMarketEdit.Url(const Value: string): Integer;
begin
  Result := List.Add('url', Value);
end;

function TVkParamsMarketEdit.DimensionWidth(const Value: Integer): Integer;
begin
  Result := List.Add('dimension_width', Value);
end;

function TVkParamsMarketEdit.DimensionHeight(const Value: Integer): Integer;
begin
  Result := List.Add('dimension_height', Value);
end;

function TVkParamsMarketEdit.DimensionLength(const Value: Integer): Integer;
begin
  Result := List.Add('dimension_length', Value);
end;

function TVkParamsMarketEdit.Weight(const Value: Integer): Integer;
begin
  Result := List.Add('weight', Value);
end;

{ TVkParamsMarketEditAlbum }

function TVkParamsMarketEditAlbum.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsMarketEditAlbum.AlbumId(const Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsMarketEditAlbum.Title(const Value: string): Integer;
begin
  Result := List.Add('title', Value);
end;

function TVkParamsMarketEditAlbum.PhotoId(const Value: Integer): Integer;
begin
  Result := List.Add('photo_id', Value);
end;

function TVkParamsMarketEditAlbum.MainAlbum(const Value: Boolean): Integer;
begin
  Result := List.Add('main_album', Value);
end;

{ TVkParamsMarketEditComment }

function TVkParamsMarketEditComment.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsMarketEditComment.CommentId(const Value: Integer): Integer;
begin
  Result := List.Add('comment_id', Value);
end;

function TVkParamsMarketEditComment.Message(const Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsMarketEditComment.Attachments(const Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value);
end;

{ TVkParamsMarketEditOrder }

function TVkParamsMarketEditOrder.UserId(const Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

function TVkParamsMarketEditOrder.OrderId(const Value: Integer): Integer;
begin
  Result := List.Add('order_id', Value);
end;

function TVkParamsMarketEditOrder.MerchantComment(const Value: string): Integer;
begin
  Result := List.Add('merchant_comment', Value);
end;

function TVkParamsMarketEditOrder.Status(const Value: TVkOrderStatus): Integer;
begin
  Result := List.Add('status', Ord(Value));
end;

function TVkParamsMarketEditOrder.TrackNumber(const Value: string): Integer;
begin
  Result := List.Add('track_number', Value);
end;

function TVkParamsMarketEditOrder.PaymentStatus(const Value: TVkPaymentStatus): Integer;
begin
  Result := List.Add('payment_status', Value.ToString);
end;

function TVkParamsMarketEditOrder.CommentForUser(const Value: string): Integer;
begin
  Result := List.Add('comment_for_user', Value);
end;

function TVkParamsMarketEditOrder.DeliveryPrice(const Value: Integer): Integer;
begin
  Result := List.Add('delivery_price', Value);
end;

function TVkParamsMarketEditOrder.Width(const Value: Integer): Integer;
begin
  Result := List.Add('width', Value);
end;

function TVkParamsMarketEditOrder.Length(const Value: Integer): Integer;
begin
  Result := List.Add('length', Value);
end;

function TVkParamsMarketEditOrder.Height(const Value: Integer): Integer;
begin
  Result := List.Add('height', Value);
end;

function TVkParamsMarketEditOrder.Weight(const Value: Integer): Integer;
begin
  Result := List.Add('weight', Value);
end;

{ TVkParamsMarketGetComments }

function TVkParamsMarketGetComments.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsMarketGetComments.ItemId(const Value: Integer): Integer;
begin
  Result := List.Add('item_id', Value);
end;

function TVkParamsMarketGetComments.NeedLikes(const Value: Boolean): Integer;
begin
  Result := List.Add('need_likes', Value);
end;

function TVkParamsMarketGetComments.StartCommentId(const Value: Integer): Integer;
begin
  Result := List.Add('start_comment_id', Value);
end;

function TVkParamsMarketGetComments.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsMarketGetComments.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsMarketGetComments.Sort(const Value: TVkSort): Integer;
begin
  Result := List.Add('sort', Value.ToString);
end;

function TVkParamsMarketGetComments.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsMarketGetComments.Fields(const UserFields: TVkProfileFields; GroupFields: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', [UserFields.ToString, GroupFields.ToString]);
end;

{ TVkParamsMarketReorderAlbums }

function TVkParamsMarketReorderAlbums.After(const Value: Integer): Integer;
begin
  Result := List.Add('after', Value);
end;

function TVkParamsMarketReorderAlbums.AlbumId(const Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsMarketReorderAlbums.Before(const Value: Integer): Integer;
begin
  Result := List.Add('before', Value);
end;

function TVkParamsMarketReorderAlbums.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

{ TVkParamsMarketReorderItems }

function TVkParamsMarketReorderItems.After(const Value: Integer): Integer;
begin
  Result := List.Add('after', Value);
end;

function TVkParamsMarketReorderItems.AlbumId(const Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsMarketReorderItems.Before(const Value: Integer): Integer;
begin
  Result := List.Add('before', Value);
end;

function TVkParamsMarketReorderItems.ItemId(const Value: Integer): Integer;
begin
  Result := List.Add('item_id', Value);
end;

function TVkParamsMarketReorderItems.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

{ TVkParamsMarketSearch }

function TVkParamsMarketSearch.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsMarketSearch.AlbumId(const Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsMarketSearch.Query(const Value: string): Integer;
begin
  Result := List.Add('q', Value);
end;

function TVkParamsMarketSearch.PriceFrom(const Value: Integer): Integer;
begin
  Result := List.Add('price_from', Value);
end;

function TVkParamsMarketSearch.PriceTo(const Value: Integer): Integer;
begin
  Result := List.Add('price_to', Value);
end;

function TVkParamsMarketSearch.Tags(const Value: TIdList): Integer;
begin
  Result := List.Add('tags', Value);
end;

function TVkParamsMarketSearch.Sort(const Value: TVkMarketSort): Integer;
begin
  Result := List.Add('sort', Ord(Value));
end;

function TVkParamsMarketSearch.Rev(const Value: Boolean): Integer;
begin
  Result := List.Add('rev', Value);
end;

function TVkParamsMarketSearch.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsMarketSearch.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsMarketSearch.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsMarketSearch.Status(const Value: Integer): Integer;
begin
  Result := List.Add('status', Value);
end;

end.

