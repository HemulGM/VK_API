unit VK.Entity.Market.Order;

interface

uses
  Generics.Collections, REST.JsonReflect, VK.Entity.Common,
  VK.Entity.Common.List, VK.Entity.Market, VK.Entity.Photo, VK.Types,
  VK.Wrap.Interceptors;

type
  TVkOrderRecipient = class
  private
    FDisplay_text: string;
    FName: string;
    FPhone: string;
  public
    property DisplayText: string read FDisplay_text write FDisplay_text;
    property Name: string read FName write FName;
    property Phone: string read FPhone write FPhone;
  end;

  TVkOrderDelivery = class
  private
    FAddress: string;
    FType: string;
  public
    property Address: string read FAddress write FAddress;
    property &Type: string read FType write FType;
  end;

  TVkOrderPayment = class(TVkEntity)
  private
    FStatus: string;
    FPayment_status: string;
    FReceipt_link: string;
  public
    property Status: string read FStatus write FStatus;
    property ReceiptLink: string read FReceipt_link write FReceipt_link;
    property PaymentStatus: string read FPayment_status write FPayment_status;
  end;

  TVkOrderTag = class(TVkEntity)
  private
    FName: string;
    FIndex: Integer;
  public
    property Index: Integer read FIndex write FIndex;
    property Name: string read FName write FName;
  end;

  TVkOrderSeller = class(TVkEntity)
  private
    FContact_Id: TVkPeerId;
    FGroup_Id: TVkPeerId;
    FName: string;
    FTitle: string;
  public
    property ContactId: TVkPeerId read FContact_Id write FContact_Id;
    property GroupId: TVkPeerId read FGroup_Id write FGroup_Id;
    property Name: string read FName write FName;
    property Title: string read FTitle write FTitle;
  end;

  TVkOrder = class(TVkObject)
  private
    FComment: string;
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FDelivery: TVkOrderDelivery;
    FDisplay_order_id: string;
    FGroup_id: TVkPeerId;
    FItems_count: Integer;
    FPreview_order_items: TArray<TVkProduct>;
    FRecipient: TVkOrderRecipient;
    [JsonReflectAttribute(ctString, rtString, TOrderStatusInterceptor)]
    FStatus: TVkOrderStatus;
    FTotal_price: TVkProductPrice;
    FUser_id: TVkPeerId;
    FMerchant_comment: string;
    FPayment: TVkOrderPayment;
    FSeller: TVkOrderSeller;
    FTags: TArray<TVkOrderTag>;
  public
    property GroupId: TVkPeerId read FGroup_id write FGroup_id;
    property UserId: TVkPeerId read FUser_id write FUser_id;
    property Date: TDateTime read FDate write FDate;
    property Status: TVkOrderStatus read FStatus write FStatus;
    property ItemsCount: Integer read FItems_count write FItems_count;
    property TotalPrice: TVkProductPrice read FTotal_price write FTotal_price;
    property DisplayOrderId: string read FDisplay_order_id write FDisplay_order_id;
    property MerchantComment: string read FMerchant_comment write FMerchant_comment;
    property Tags: TArray<TVkOrderTag> read FTags write FTags;
    property Comment: string read FComment write FComment;
    property Delivery: TVkOrderDelivery read FDelivery write FDelivery;
    property PreviewOrderItems: TArray<TVkProduct> read FPreview_order_items write FPreview_order_items;
    property Recipient: TVkOrderRecipient read FRecipient write FRecipient;
    property Payment: TVkOrderPayment read FPayment write FPayment;
    property Seller: TVkOrderSeller read FSeller write FSeller;
    destructor Destroy; override;
  end;

  TVkOrders = TVkEntityList<TVkOrder>;

implementation

uses
  VK.CommonUtils;

{TVkOrder}

destructor TVkOrder.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkProduct>(FPreview_order_items);
  TArrayHelp.FreeArrayOfObject<TVkOrderTag>(FTags);
  if Assigned(FTotal_price) then
    FTotal_price.Free;
  if Assigned(FRecipient) then
    FRecipient.Free;
  if Assigned(FDelivery) then
    FDelivery.Free;
  if Assigned(FPayment) then
    FPayment.Free;
  if Assigned(FSeller) then
    FSeller.Free;
  inherited;
end;

end.

