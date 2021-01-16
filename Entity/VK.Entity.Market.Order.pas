unit VK.Entity.Market.Order;

interface

uses
  Generics.Collections, REST.Json.Interceptors, REST.JsonReflect, Rest.Json, VK.Entity.Common, VK.Entity.Common.List,
  VK.Entity.Market, VK.Entity.Photo;

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
    property&Type: string read FType write FType;
  end;

  TVkOrder = class(TVkObject)
  private
    FComment: string;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FDelivery: TVkOrderDelivery;
    FDisplay_order_id: string;
    FGroup_id: Integer;
    FItems_count: Integer;
    FPreview_order_items: TArray<TVkProduct>;
    FRecipient: TVkOrderRecipient;
    FStatus: Integer;
    FTotal_price: TVkProductPrice;
    FUser_id: Integer;
  public
    property Comment: string read FComment write FComment;
    property Date: TDateTime read FDate write FDate;
    property Delivery: TVkOrderDelivery read FDelivery write FDelivery;
    property DisplayOrderId: string read FDisplay_order_id write FDisplay_order_id;
    property GroupId: Integer read FGroup_id write FGroup_id;
    property ItemsCount: Integer read FItems_count write FItems_count;
    property PreviewOrderItems: TArray<TVkProduct> read FPreview_order_items write FPreview_order_items;
    property Recipient: TVkOrderRecipient read FRecipient write FRecipient;
    property Status: Integer read FStatus write FStatus;
    property TotalPrice: TVkProductPrice read FTotal_price write FTotal_price;
    property UserId: Integer read FUser_id write FUser_id;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkOrders = TVkEntityList<TVkOrder>;

implementation

uses
  VK.CommonUtils;

{TVkOrder}

constructor TVkOrder.Create;
begin
  inherited;
  FTotal_price := TVkProductPrice.Create();
  FDelivery := TVkOrderDelivery.Create();
  FRecipient := TVkOrderRecipient.Create();
end;

destructor TVkOrder.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkProduct>(FPreview_order_items);
  FTotal_price.Free;
  FDelivery.Free;
  FRecipient.Free;
  inherited;
end;

end.

