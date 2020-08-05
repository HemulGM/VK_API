unit VK.Market;

interface

uses
  System.SysUtils, System.Generics.Collections, VK.Controller, VK.Types, VK.Entity.Media, VK.Entity.Market,
  VK.Entity.Market.Album, VK.Entity.Market.Order;

type
  TVkParamsMarketAdd = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function Name(Value: string): Integer;
    function Description(Value: string): Integer;
    function CategoryId(Value: Integer): Integer;
    function Price(Value: Extended): Integer;
    function OldPrice(Value: Extended): Integer;
    function Deleted(Value: Boolean): Integer;
    function MainPhotoId(Value: Integer): Integer;
    function PhotoIds(Value: TIds): Integer;
    function Url(Value: string): Integer;
    function DimensionWidth(Value: Integer): Integer;
    function DimensionHeight(Value: Integer): Integer;
    function DimensionLength(Value: Integer): Integer;
    function Weight(Value: Integer): Integer;
  end;

  TVkParamsMarketCreateComment = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function ItemId(Value: Integer): Integer;
    function Message(Value: string): Integer;
    function Attachments(Value: TAttachmentArray): Integer;
    function FromGroup(Value: Boolean): Integer;
    function ReplyToComment(Value: Integer): Integer;
    function StickerId(Value: Integer): Integer;
    function Guid(Value: string): Integer;
  end;

  TVkParamsMarketEdit = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function ItemId(Value: Integer): Integer;
    function Name(Value: string): Integer;
    function Description(Value: string): Integer;
    function CategoryId(Value: Integer): Integer;
    function Price(Value: Extended): Integer;
    function OldPrice(Value: Extended): Integer;
    function Deleted(Value: Boolean): Integer;
    function MainPhotoId(Value: Integer): Integer;
    function PhotoIds(Value: TIds): Integer;
    function Url(Value: string): Integer;
    function DimensionWidth(Value: Integer): Integer;
    function DimensionHeight(Value: Integer): Integer;
    function DimensionLength(Value: Integer): Integer;
    function Weight(Value: Integer): Integer;
  end;

  TVkParamsMarketEditAlbum = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function AlbumId(Value: Integer): Integer;
    function Title(Value: string): Integer;
    function PhotoId(Value: Integer): Integer;
    function MainAlbum(Value: Boolean): Integer;
  end;

  TVkParamsMarketEditComment = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function CommentId(Value: Integer): Integer;
    function Message(Value: string): Integer;
    function Attachments(Value: TAttachmentArray): Integer;
  end;

  TVkParamsMarketEditOrder = record
    List: TParams;
    function UserId(Value: Integer): Integer;
    function OrderId(Value: Integer): Integer;
    function MerchantComment(Value: string): Integer;
    function Status(Value: Integer): Integer;
    function TrackNumber(Value: string): Integer;
    function PaymentStatus(Value: string): Integer;
    function DeliveryPrice(Value: Integer): Integer;
    function Width(Value: Integer): Integer;
    function Length(Value: Integer): Integer;
    function Height(Value: Integer): Integer;
    function Weight(Value: Integer): Integer;
  end;

  TVkParamsMarketGet = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function AlbumId(Value: Integer): Integer;
    function Extended(Value: Boolean): Integer;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
  end;

  TVkParamsMarketGetComments = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function ItemId(Value: Integer): Integer;
    function NeedLikes(Value: Boolean): Integer;
    function StartCommentId(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
    function Sort(Value: TVkSort): Integer;
    function Extended(Value: Boolean): Integer;
    function Fields(Value: TVkProfileFields): Integer;
  end;

  TVkParamsMarketReorderAlbums = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function AlbumId(Value: Integer): Integer;
    function Before(Value: Integer): Integer;
    function After(Value: Integer): Integer;
  end;

  TVkParamsMarketReorderItems = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function AlbumId(Value: Integer): Integer;
    function ItemId(Value: Integer): Integer;
    function Before(Value: Integer): Integer;
    function After(Value: Integer): Integer;
  end;

  TVkParamsMarketSearch = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function AlbumId(Value: Integer): Integer;
    function Query(Value: string): Integer;
    function PriceFrom(Value: Integer): Integer;
    function PriceTo(Value: Integer): Integer;
    function Tags(Value: TIds): Integer;
    function Sort(Value: Integer): Integer;
    function Rev(Value: Boolean): Integer;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
    function Extended(Value: Boolean): Integer;
    function Status(Value: Integer): Integer;
  end;

  TMarketController = class(TVkController)
  public
    /// <summary>
    /// Добавляет новый товар.
    /// </summary>
    function Add(var Id: Integer; Params: TParams): Boolean; overload;
    /// <summary>
    /// Добавляет новый товар.
    /// </summary>
    function Add(var Id: Integer; Params: TVkParamsMarketAdd): Boolean; overload;
    /// <summary>
    /// Добавляет новую подборку с товарами.
    /// </summary>
    function AddAlbum(var Id: Integer; OwnerId: Integer; Title: string; PhotoId: Integer = -1; MainAlbum: Boolean =
      False): Boolean;
    /// <summary>
    /// Добавляет товар в одну или несколько выбранных подборок.
    /// </summary>
    function AddToAlbum(const OwnerId, ItemId: Integer; AlbumIds: TIds): Boolean;
    /// <summary>
    /// Создает новый комментарий к товару.
    /// </summary>
    function CreateComment(var Id: Integer; Params: TParams): Boolean; overload;
    /// <summary>
    /// Создает новый комментарий к товару.
    /// </summary>
    function CreateComment(var Id: Integer; Params: TVkParamsMarketCreateComment): Boolean; overload;
    /// <summary>
    /// Удаляет товар.
    /// </summary>
    function Delete(const OwnerId, ItemId: Integer): Boolean;
    /// <summary>
    /// Удаляет подборку с товарами.
    /// </summary>
    function DeleteAlbum(const OwnerId, AlbumId: Integer): Boolean;
    /// <summary>
    /// Удаляет комментарий к товару.
    /// </summary>
    function DeleteComment(const OwnerId, CommentId: Integer): Boolean;
    /// <summary>
    /// Редактирует товар.
    /// </summary>
    function Edit(Params: TParams): Boolean; overload;
    /// <summary>
    /// Редактирует товар.
    /// </summary>
    function Edit(Params: TVkParamsMarketEdit): Boolean; overload;
    /// <summary>
    /// Редактирует подборку с товарами.
    /// </summary>
    function EditAlbum(Params: TParams): Boolean; overload;
    /// <summary>
    /// Редактирует подборку с товарами.
    /// </summary>
    function EditAlbum(Params: TVkParamsMarketEditAlbum): Boolean; overload;
    /// <summary>
    /// Изменяет текст комментария к товару.
    /// </summary>
    function EditComment(Params: TParams): Boolean; overload;
    /// <summary>
    /// Изменяет текст комментария к товару.
    /// </summary>
    function EditComment(Params: TVkParamsMarketEditComment): Boolean; overload;
    /// <summary>
    /// Редактирует заказ.
    /// </summary>
    function EditOrder(Params: TParams): Boolean; overload;
    /// <summary>
    /// Редактирует заказ.
    /// </summary>
    function EditOrder(Params: TVkParamsMarketEditOrder): Boolean; overload;
    /// <summary>
    /// Возвращает список товаров в сообществе.
    /// </summary>
    function Get(var Items: TVkProducts; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список товаров в сообществе.
    /// </summary>
    function Get(var Items: TVkProducts; Params: TVkParamsMarketGet): Boolean; overload;
    /// <summary>
    /// Возвращает данные подборки с товарами.
    /// </summary>
    function GetAlbumById(var Items: TVkMarketAlbums; const OwnerId: Integer; AlbumIds: TIds): Boolean; overload;
    /// <summary>
    /// Возвращает список подборок с товарами.
    /// </summary>
    function GetAlbums(var Items: TVkMarketAlbums; const OwnerId: Integer; Offset: Integer = 0; Count: Integer = 50):
      Boolean; overload;
    /// <summary>
    /// Возвращает информацию о товарах по идентификаторам.
    /// </summary>
    function GetById(var Items: TVkProducts; const ItemIds: TIds; Extended: Boolean = False): Boolean; overload;
    /// <summary>
    /// Возвращает список категорий для товаров.
    /// </summary>
    function GetCategories(var Items: TVkProductCategories; const Offset: Integer = 0; Count: Integer = 10): Boolean; overload;
    /// <summary>
    /// Возвращает список комментариев к товару.
    /// </summary>
    function GetComments(var Items: TVkComments; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список комментариев к товару.
    /// </summary>
    function GetComments(var Items: TVkComments; Params: TVkParamsMarketGetComments): Boolean; overload;
    /// <summary>
    /// Возвращает заказы сообщества.
    /// </summary>
    function GetGroupOrders(var Items: TVkOrders; GroupId: Integer; Offset: Integer = 0; Count: Integer = 10): Boolean; overload;
    /// <summary>
    /// Возвращает заказ по идентификатору.
    /// </summary>
    function GetOrderById(var Item: TVkOrder; const OrderId: Integer; UserId: Integer = 0; Extended: Boolean = False):
      Boolean; overload;
    /// <summary>
    /// Возвращает товары в заказе.
    /// </summary>
    function GetOrderItems(var Items: TVkProducts; const OrderId: Integer; UserId: Integer = 0; Offset: Integer = 0;
      Count: Integer = 50): Boolean; overload;
    /// <summary>
    /// Возвращает заказы.
    /// </summary>
    function GetOrders(var Items: TVkOrders; const Offset: Integer = 0; Count: Integer = 10; Extended: Boolean = False):
      Boolean; overload;
    /// <summary>
    /// Удаляет товар из одной или нескольких выбранных подборок.
    /// </summary>
    function RemoveFromAlbum(const ItemId, OwnerId: Integer; AlbumIds: TIds): Boolean; overload;
    /// <summary>
    /// Изменяет положение подборки с товарами в списке.
    /// </summary>
    function ReorderAlbums(const Params: TVkParamsMarketReorderAlbums): Boolean;
    /// <summary>
    /// Изменяет положение товара в подборке.
    /// </summary>
    function ReorderItems(const Params: TVkParamsMarketReorderItems): Boolean;
    /// <summary>
    /// Позволяет отправить жалобу на товар.
    /// </summary>
    function Report(const OwnerId, ItemId: Integer; Reason: TVkMediaReportReason): Boolean;
    /// <summary>
    /// Позволяет оставить жалобу на комментарий к товару.
    /// </summary>
    function ReportComment(const OwnerId, CommentId: Integer; Reason: TVkMediaReportReason): Boolean;
    /// <summary>
    /// Восстанавливает удаленный товар.
    /// </summary>
    function Restore(const OwnerId, ItemId: Integer): Boolean;
    /// <summary>
    /// Восстанавливает удаленный комментарий к товару.
    /// </summary>
    function RestoreComment(const OwnerId, CommentId: Integer): Boolean;
    /// <summary>
    /// Ищет товары в каталоге сообщества.
    /// </summary>
    function Search(var Items: TVkProducts; Params: TParams): Boolean; overload;
    /// <summary>
    /// Ищет товары в каталоге сообщества.
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

function TMarketController.AddAlbum(var Id: Integer; OwnerId: Integer; Title: string; PhotoId: Integer; MainAlbum:
  Boolean): Boolean;
var
  Params: TParams;
begin
  Params.Add('owner_id', OwnerId);
  Params.Add('title', Title);
  if PhotoId <> -1 then
    Params.Add('photo_id', PhotoId);
  Params.Add('main_album', MainAlbum);
  with Handler.Execute('market.addAlbum', Params) do
    Result := Success and ResponseAsInt(Id);
end;

function TMarketController.AddToAlbum(const OwnerId, ItemId: Integer; AlbumIds: TIds): Boolean;
var
  Params: TParams;
begin
  Params.Add('owner_id', OwnerId);
  Params.Add('item_id', ItemId);
  Params.Add('album_ids', AlbumIds);
  with Handler.Execute('market.addToAlbum', Params) do
    Result := Success and ResponseIsTrue;
end;

function TMarketController.Add(var Id: Integer; Params: TParams): Boolean;
begin
  with Handler.Execute('market.add', Params) do
    Result := Success and ResponseAsInt(Id);
end;

function TMarketController.Delete(const OwnerId, ItemId: Integer): Boolean;
begin
  with Handler.Execute('market.delete', [['owner_id', OwnerId.ToString], ['item_id', ItemId.ToString]]) do
    Result := Success and ResponseIsTrue;
end;

function TMarketController.DeleteAlbum(const OwnerId, AlbumId: Integer): Boolean;
begin
  with Handler.Execute('market.deleteAlbum', [['owner_id', OwnerId.ToString], ['album_id', AlbumId.ToString]]) do
    Result := Success and ResponseIsTrue;
end;

function TMarketController.DeleteComment(const OwnerId, CommentId: Integer): Boolean;
begin
  with Handler.Execute('market.deleteComment', [['owner_id', OwnerId.ToString], ['comment_id', CommentId.ToString]]) do
    Result := Success and ResponseIsTrue;
end;

function TMarketController.EditOrder(Params: TParams): Boolean;
begin
  with Handler.Execute('market.editOrder', Params) do
    Result := Success and ResponseIsTrue;
end;

function TMarketController.EditComment(Params: TParams): Boolean;
begin
  with Handler.Execute('market.editComment', Params) do
    Result := Success and ResponseIsTrue;
end;

function TMarketController.EditAlbum(Params: TParams): Boolean;
begin
  with Handler.Execute('market.editAlbum', Params) do
    Result := Success and ResponseIsTrue;
end;

function TMarketController.Edit(Params: TParams): Boolean;
begin
  with Handler.Execute('market.edit', Params) do
    Result := Success and ResponseIsTrue;
end;

function TMarketController.CreateComment(var Id: Integer; Params: TParams): Boolean;
begin
  with Handler.Execute('market.createComment', Params) do
    Result := Success and ResponseAsInt(Id);
end;

function TMarketController.Get(var Items: TVkProducts; Params: TVkParamsMarketGet): Boolean;
begin
  Result := Get(Items, Params.List);
end;

function TMarketController.Get(var Items: TVkProducts; Params: TParams): Boolean;
begin
  with Handler.Execute('market.get', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkProducts.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TMarketController.GetAlbumById(var Items: TVkMarketAlbums; const OwnerId: Integer; AlbumIds: TIds): Boolean;
begin
  with Handler.Execute('market.getAlbumById', [['owner_id', OwnerId.ToString], ['album_ids', AlbumIds.ToString]]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkMarketAlbums.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TMarketController.GetAlbums(var Items: TVkMarketAlbums; const OwnerId: Integer; Offset, Count: Integer): Boolean;
begin
  with Handler.Execute('market.getAlbums', [['owner_id', OwnerId.ToString], ['count', Count.ToString], ['offset', Offset.ToString]])
    do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkMarketAlbums.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TMarketController.GetById(var Items: TVkProducts; const ItemIds: TIds; Extended: Boolean): Boolean;
begin
  with Handler.Execute('market.getById', [['item_ids', ItemIds.ToString], ['extended', BoolToString(Extended)]]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkProducts.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TMarketController.GetCategories(var Items: TVkProductCategories; const Offset: Integer; Count: Integer): Boolean;
begin
  with Handler.Execute('market.getCategories', [['count', Count.ToString], ['offset', Offset.ToString]]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkProductCategories.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TMarketController.GetComments(var Items: TVkComments; Params: TVkParamsMarketGetComments): Boolean;
begin
  Result := GetComments(Items, Params.List);
end;

function TMarketController.GetGroupOrders(var Items: TVkOrders; GroupId, Offset, Count: Integer): Boolean;
begin
  with Handler.Execute('market.getGroupOrders', [['group_id', GroupId.ToString], ['count', Count.ToString], ['offset',
    Offset.ToString]]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkOrders.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TMarketController.GetOrderById(var Item: TVkOrder; const OrderId: Integer; UserId: Integer; Extended: Boolean): Boolean;
var
  Params: TParams;
begin
  Params.Add('order_id', OrderId);
  Params.Add('user_id', UserId);
  if Extended then
    Params.Add('extended', Extended);
  with Handler.Execute('market.getOrderById', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Item := TVkOrder.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TMarketController.GetOrderItems(var Items: TVkProducts; const OrderId: Integer; UserId, Offset, Count: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('order_id', OrderId);
  Params.Add('user_id', UserId);
  Params.Add('offset', Offset);
  Params.Add('count', Count);
  with Handler.Execute('market.getOrderItems', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkProducts.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TMarketController.GetOrders(var Items: TVkOrders; const Offset: Integer; Count: Integer; Extended: Boolean): Boolean;
var
  Params: TParams;
begin
  Params.Add('offset', Offset);
  Params.Add('count', Count);
  if Extended then
    Params.Add('extended', Extended);
  with Handler.Execute('market.getOrders', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkOrders.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TMarketController.RemoveFromAlbum(const ItemId, OwnerId: Integer; AlbumIds: TIds): Boolean;
var
  Params: TParams;
begin
  Params.Add('owner_id', OwnerId);
  Params.Add('item_id', ItemId);
  Params.Add('album_ids', AlbumIds);
  with Handler.Execute('market.removeFromAlbum', Params) do
    Result := Success and ResponseIsTrue;
end;

function TMarketController.ReorderAlbums(const Params: TVkParamsMarketReorderAlbums): Boolean;
begin
  with Handler.Execute('market.reorderAlbums', Params.List) do
    Result := Success and ResponseIsTrue;
end;

function TMarketController.ReorderItems(const Params: TVkParamsMarketReorderItems): Boolean;
begin
  with Handler.Execute('market.reorderItems', Params.List) do
    Result := Success and ResponseIsTrue;
end;

function TMarketController.Report(const OwnerId, ItemId: Integer; Reason: TVkMediaReportReason): Boolean;
var
  Params: TParams;
begin
  Params.Add('owner_id', OwnerId);
  Params.Add('item_id', ItemId);
  Params.Add('reason', Reason.ToString);
  with Handler.Execute('market.report', Params) do
    Result := Success and ResponseIsTrue;
end;

function TMarketController.ReportComment(const OwnerId, CommentId: Integer; Reason: TVkMediaReportReason): Boolean;
var
  Params: TParams;
begin
  Params.Add('owner_id', OwnerId);
  Params.Add('comment_id', CommentId);
  Params.Add('reason', Reason.ToString);
  with Handler.Execute('market.reportComment', Params) do
    Result := Success and ResponseIsTrue;
end;

function TMarketController.Restore(const OwnerId, ItemId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('owner_id', OwnerId);
  Params.Add('item_id', ItemId);
  with Handler.Execute('market.restore', Params) do
    Result := Success and ResponseIsTrue;
end;

function TMarketController.RestoreComment(const OwnerId, CommentId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('owner_id', OwnerId);
  Params.Add('comment_id', CommentId);
  with Handler.Execute('market.restoreComment', Params) do
    Result := Success and ResponseIsTrue;
end;

function TMarketController.Search(var Items: TVkProducts; Params: TVkParamsMarketSearch): Boolean;
begin
  Result := Search(Items, Params.List);
end;

function TMarketController.Search(var Items: TVkProducts; Params: TParams): Boolean;
begin
  with Handler.Execute('market.search', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkProducts.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TMarketController.GetComments(var Items: TVkComments; Params: TParams): Boolean;
begin
  with Handler.Execute('market.getComments', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkComments.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

{ TVkParamsMarketGet }

function TVkParamsMarketGet.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsMarketGet.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsMarketGet.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsMarketGet.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsMarketGet.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

{ TVkParamsMarketAdd }

function TVkParamsMarketAdd.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsMarketAdd.Name(Value: string): Integer;
begin
  Result := List.Add('name', Value);
end;

function TVkParamsMarketAdd.Description(Value: string): Integer;
begin
  Result := List.Add('description', Value);
end;

function TVkParamsMarketAdd.CategoryId(Value: Integer): Integer;
begin
  Result := List.Add('category_id', Value);
end;

function TVkParamsMarketAdd.Price(Value: Extended): Integer;
begin
  Result := List.Add('price', Value);
end;

function TVkParamsMarketAdd.OldPrice(Value: Extended): Integer;
begin
  Result := List.Add('old_price', Value);
end;

function TVkParamsMarketAdd.Deleted(Value: Boolean): Integer;
begin
  Result := List.Add('deleted', Value);
end;

function TVkParamsMarketAdd.MainPhotoId(Value: Integer): Integer;
begin
  Result := List.Add('main_photo_id', Value);
end;

function TVkParamsMarketAdd.PhotoIds(Value: TIds): Integer;
begin
  Result := List.Add('photo_ids', Value);
end;

function TVkParamsMarketAdd.Url(Value: string): Integer;
begin
  Result := List.Add('url', Value);
end;

function TVkParamsMarketAdd.DimensionWidth(Value: Integer): Integer;
begin
  Result := List.Add('dimension_width', Value);
end;

function TVkParamsMarketAdd.DimensionHeight(Value: Integer): Integer;
begin
  Result := List.Add('dimension_height', Value);
end;

function TVkParamsMarketAdd.DimensionLength(Value: Integer): Integer;
begin
  Result := List.Add('dimension_length', Value);
end;

function TVkParamsMarketAdd.Weight(Value: Integer): Integer;
begin
  Result := List.Add('weight', Value);
end;

{ TVkParamsMarketCreateComment }

function TVkParamsMarketCreateComment.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsMarketCreateComment.ItemId(Value: Integer): Integer;
begin
  Result := List.Add('item_id', Value);
end;

function TVkParamsMarketCreateComment.Message(Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsMarketCreateComment.Attachments(Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsMarketCreateComment.FromGroup(Value: Boolean): Integer;
begin
  Result := List.Add('from_group', Value);
end;

function TVkParamsMarketCreateComment.ReplyToComment(Value: Integer): Integer;
begin
  Result := List.Add('reply_to_comment', Value);
end;

function TVkParamsMarketCreateComment.StickerId(Value: Integer): Integer;
begin
  Result := List.Add('sticker_id', Value);
end;

function TVkParamsMarketCreateComment.Guid(Value: string): Integer;
begin
  Result := List.Add('guid', Value);
end;

{ TVkParamsMarketEdit }

function TVkParamsMarketEdit.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsMarketEdit.ItemId(Value: Integer): Integer;
begin
  Result := List.Add('item_id', Value);
end;

function TVkParamsMarketEdit.Name(Value: string): Integer;
begin
  Result := List.Add('name', Value);
end;

function TVkParamsMarketEdit.Description(Value: string): Integer;
begin
  Result := List.Add('description', Value);
end;

function TVkParamsMarketEdit.CategoryId(Value: Integer): Integer;
begin
  Result := List.Add('category_id', Value);
end;

function TVkParamsMarketEdit.Price(Value: Extended): Integer;
begin
  Result := List.Add('price', Value);
end;

function TVkParamsMarketEdit.OldPrice(Value: Extended): Integer;
begin
  Result := List.Add('old_price', Value);
end;

function TVkParamsMarketEdit.Deleted(Value: Boolean): Integer;
begin
  Result := List.Add('deleted', Value);
end;

function TVkParamsMarketEdit.MainPhotoId(Value: Integer): Integer;
begin
  Result := List.Add('main_photo_id', Value);
end;

function TVkParamsMarketEdit.PhotoIds(Value: TIds): Integer;
begin
  Result := List.Add('photo_ids', Value);
end;

function TVkParamsMarketEdit.Url(Value: string): Integer;
begin
  Result := List.Add('url', Value);
end;

function TVkParamsMarketEdit.DimensionWidth(Value: Integer): Integer;
begin
  Result := List.Add('dimension_width', Value);
end;

function TVkParamsMarketEdit.DimensionHeight(Value: Integer): Integer;
begin
  Result := List.Add('dimension_height', Value);
end;

function TVkParamsMarketEdit.DimensionLength(Value: Integer): Integer;
begin
  Result := List.Add('dimension_length', Value);
end;

function TVkParamsMarketEdit.Weight(Value: Integer): Integer;
begin
  Result := List.Add('weight', Value);
end;

{ TVkParamsMarketEditAlbum }

function TVkParamsMarketEditAlbum.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsMarketEditAlbum.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsMarketEditAlbum.Title(Value: string): Integer;
begin
  Result := List.Add('title', Value);
end;

function TVkParamsMarketEditAlbum.PhotoId(Value: Integer): Integer;
begin
  Result := List.Add('photo_id', Value);
end;

function TVkParamsMarketEditAlbum.MainAlbum(Value: Boolean): Integer;
begin
  Result := List.Add('main_album', Value);
end;

{ TVkParamsMarketEditComment }

function TVkParamsMarketEditComment.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsMarketEditComment.CommentId(Value: Integer): Integer;
begin
  Result := List.Add('comment_id', Value);
end;

function TVkParamsMarketEditComment.Message(Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsMarketEditComment.Attachments(Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value);
end;

{ TVkParamsMarketEditOrder }

function TVkParamsMarketEditOrder.UserId(Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

function TVkParamsMarketEditOrder.OrderId(Value: Integer): Integer;
begin
  Result := List.Add('order_id', Value);
end;

function TVkParamsMarketEditOrder.MerchantComment(Value: string): Integer;
begin
  Result := List.Add('merchant_comment', Value);
end;

function TVkParamsMarketEditOrder.Status(Value: Integer): Integer;
begin
  Result := List.Add('status', Value);
end;

function TVkParamsMarketEditOrder.TrackNumber(Value: string): Integer;
begin
  Result := List.Add('track_number', Value);
end;

function TVkParamsMarketEditOrder.PaymentStatus(Value: string): Integer;
begin
  Result := List.Add('payment_status', Value);
end;

function TVkParamsMarketEditOrder.DeliveryPrice(Value: Integer): Integer;
begin
  Result := List.Add('delivery_price', Value);
end;

function TVkParamsMarketEditOrder.Width(Value: Integer): Integer;
begin
  Result := List.Add('width', Value);
end;

function TVkParamsMarketEditOrder.Length(Value: Integer): Integer;
begin
  Result := List.Add('length', Value);
end;

function TVkParamsMarketEditOrder.Height(Value: Integer): Integer;
begin
  Result := List.Add('height', Value);
end;

function TVkParamsMarketEditOrder.Weight(Value: Integer): Integer;
begin
  Result := List.Add('weight', Value);
end;

{ TVkParamsMarketGetComments }

function TVkParamsMarketGetComments.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsMarketGetComments.ItemId(Value: Integer): Integer;
begin
  Result := List.Add('item_id', Value);
end;

function TVkParamsMarketGetComments.NeedLikes(Value: Boolean): Integer;
begin
  Result := List.Add('need_likes', Value);
end;

function TVkParamsMarketGetComments.StartCommentId(Value: Integer): Integer;
begin
  Result := List.Add('start_comment_id', Value);
end;

function TVkParamsMarketGetComments.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsMarketGetComments.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsMarketGetComments.Sort(Value: TVkSort): Integer;
begin
  Result := List.Add('sort', Value.ToString);
end;

function TVkParamsMarketGetComments.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsMarketGetComments.Fields(Value: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

{ TVkParamsMarketReorderAlbums }

function TVkParamsMarketReorderAlbums.After(Value: Integer): Integer;
begin
  Result := List.Add('after', Value);
end;

function TVkParamsMarketReorderAlbums.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsMarketReorderAlbums.Before(Value: Integer): Integer;
begin
  Result := List.Add('before', Value);
end;

function TVkParamsMarketReorderAlbums.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

{ TVkParamsMarketReorderItems }

function TVkParamsMarketReorderItems.After(Value: Integer): Integer;
begin
  Result := List.Add('after', Value);
end;

function TVkParamsMarketReorderItems.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsMarketReorderItems.Before(Value: Integer): Integer;
begin
  Result := List.Add('before', Value);
end;

function TVkParamsMarketReorderItems.ItemId(Value: Integer): Integer;
begin
  Result := List.Add('item_id', Value);
end;

function TVkParamsMarketReorderItems.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

{ TVkParamsMarketSearch }

function TVkParamsMarketSearch.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsMarketSearch.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsMarketSearch.Query(Value: string): Integer;
begin
  Result := List.Add('q', Value);
end;

function TVkParamsMarketSearch.PriceFrom(Value: Integer): Integer;
begin
  Result := List.Add('price_from', Value);
end;

function TVkParamsMarketSearch.PriceTo(Value: Integer): Integer;
begin
  Result := List.Add('price_to', Value);
end;

function TVkParamsMarketSearch.Tags(Value: TIds): Integer;
begin
  Result := List.Add('tags', Value);
end;

function TVkParamsMarketSearch.Sort(Value: Integer): Integer;
begin
  Result := List.Add('sort', Value);
end;

function TVkParamsMarketSearch.Rev(Value: Boolean): Integer;
begin
  Result := List.Add('rev', Value);
end;

function TVkParamsMarketSearch.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsMarketSearch.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsMarketSearch.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsMarketSearch.Status(Value: Integer): Integer;
begin
  Result := List.Add('status', Value);
end;

end.

