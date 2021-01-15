unit VK.Entity.Gift;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkGift = class(TVkObject)
  private
    FThumb_256: string;
    FThumb_96: string;
    FThumb_48: string;
    FStickers_product_id: Integer;
  public
    property Thumb256: string read FThumb_256 write FThumb_256;
    property Thumb96: string read FThumb_96 write FThumb_96;
    property Thumb48: string read FThumb_48 write FThumb_48;
    property StickersProductId: Integer read FStickers_product_id write FStickers_product_id;
  end;

  TVkGiftItem = class(TVkObject)
  private
    FDate: Int64;
    FFrom_id: Integer;
    FGift: TVkGift;
    FGift_hash: string;
    FMessage: string;
    FPrivacy: Integer;
  public
    property Date: Int64 read FDate write FDate;
    property FromId: Integer read FFrom_id write FFrom_id;
    property Gift: TVkGift read FGift write FGift;
    property GiftHash: string read FGift_hash write FGift_hash;
    property Message: string read FMessage write FMessage;
    property Privacy: Integer read FPrivacy write FPrivacy;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkGiftItems = class(TVkEntity)
  private
    FCount: Integer;
    FItems: TArray<TVkGiftItem>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkGiftItem> read FItems write FItems;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{ TVkGiftItems }

destructor TVkGiftItems.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkGiftItem>(FItems);
  inherited;
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

end.

