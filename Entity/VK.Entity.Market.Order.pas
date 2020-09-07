unit VK.Entity.Market.Order;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Market, VK.Entity.Photo;

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
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkOrderRecipient;
  end;

  TVkOrderDelivery = class
  private
    FAddress: string;
    FType: string;
  public
    property Address: string read FAddress write FAddress;
    property&Type: string read FType write FType;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkOrderDelivery;
  end;

  TVkOrder = class
  private
    FComment: string;
    FDate: Int64;
    FDelivery: TVkOrderDelivery;
    FDisplay_order_id: string;
    FGroup_id: Integer;
    FId: Integer;
    FItems_count: Integer;
    FPreview_order_items: TArray<TVkProduct>;
    FRecipient: TVkOrderRecipient;
    FStatus: Integer;
    FTotal_price: TVkProductPrice;
    FUser_id: Integer;
  public
    property Comment: string read FComment write FComment;
    property Date: Int64 read FDate write FDate;
    property Delivery: TVkOrderDelivery read FDelivery write FDelivery;
    property DisplayOrderId: string read FDisplay_order_id write FDisplay_order_id;
    property GroupId: Integer read FGroup_id write FGroup_id;
    property Id: Integer read FId write FId;
    property ItemsCount: Integer read FItems_count write FItems_count;
    property PreviewOrderItems: TArray<TVkProduct> read FPreview_order_items write FPreview_order_items;
    property Recipient: TVkOrderRecipient read FRecipient write FRecipient;
    property Status: Integer read FStatus write FStatus;
    property TotalPrice: TVkProductPrice read FTotal_price write FTotal_price;
    property UserId: Integer read FUser_id write FUser_id;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkOrder;
  end;

  TVkOrders = class
  private
    FCount: Integer;
    FItems: TArray<TVkOrder>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkOrder> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkOrders;
  end;

implementation

uses
  VK.CommonUtils;

{TVkOrderRecipient}

function TVkOrderRecipient.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkOrderRecipient.FromJsonString(AJsonString: string): TVkOrderRecipient;
begin
  result := TJson.JsonToObject<TVkOrderRecipient>(AJsonString)
end;

{TVkOrderDelivery}

function TVkOrderDelivery.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkOrderDelivery.FromJsonString(AJsonString: string): TVkOrderDelivery;
begin
  result := TJson.JsonToObject<TVkOrderDelivery>(AJsonString)
end;

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

function TVkOrder.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkOrder.FromJsonString(AJsonString: string): TVkOrder;
begin
  result := TJson.JsonToObject<TVkOrder>(AJsonString)
end;

{TVkOrders}

destructor TVkOrders.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkOrder>(FItems);
  inherited;
end;

function TVkOrders.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkOrders.FromJsonString(AJsonString: string): TVkOrders;
begin
  result := TJson.JsonToObject<TVkOrders>(AJsonString)
end;

end.

