unit VK.Entity.Gift;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkGift = class
  private
    FId: Integer;
    FThumb_256: string;
    FThumb_96: string;
    FThumb_48: string;
    FStickers_product_id: Integer;
  public
    property Id: Integer read FId write FId;
    property Thumb256: string read FThumb_256 write FThumb_256;
    property Thumb96: string read FThumb_96 write FThumb_96;
    property Thumb48: string read FThumb_48 write FThumb_48;
    property StickersProductId: Integer read FStickers_product_id write FStickers_product_id;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkGift;
  end;

  TVkGiftItem = class
  private
    FDate: Int64;
    FFrom_id: Integer;
    FGift: TVkGift;
    FGift_hash: string;
    FId: Integer;
    FMessage: string;
    FPrivacy: Integer;
  public
    property Date: Int64 read FDate write FDate;
    property FromId: Integer read FFrom_id write FFrom_id;
    property Gift: TVkGift read FGift write FGift;
    property GiftHash: string read FGift_hash write FGift_hash;
    property Id: Integer read FId write FId;
    property Message: string read FMessage write FMessage;
    property Privacy: Integer read FPrivacy write FPrivacy;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkGiftItem;
  end;

  TVkGiftItems = class
  private
    FCount: Integer;
    FItems: TArray<TVkGiftItem>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkGiftItem> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkGiftItems;
  end;

implementation

uses
  VK.CommonUtils;

{TVkGift}

function TVkGift.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkGift.FromJsonString(AJsonString: string): TVkGift;
begin
  result := TJson.JsonToObject<TVkGift>(AJsonString)
end;

{ TVkGiftItems }

destructor TVkGiftItems.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkGiftItem>(FItems);
  inherited;
end;

class function TVkGiftItems.FromJsonString(AJsonString: string): TVkGiftItems;
begin
  result := TJson.JsonToObject<TVkGiftItems>(AJsonString)
end;

function TVkGiftItems.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkGiftItem }

constructor TVkGiftItem.Create;
begin
  FGift := TVkGift.Create();
end;

destructor TVkGiftItem.Destroy;
begin
  FGift.Free;
  inherited;
end;

class function TVkGiftItem.FromJsonString(AJsonString: string): TVkGiftItem;
begin
  result := TJson.JsonToObject<TVkGiftItem>(AJsonString)
end;

function TVkGiftItem.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

end.

