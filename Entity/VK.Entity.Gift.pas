unit VK.Entity.Gift;

interface

uses
  Generics.Collections, REST.JsonReflect, REST.Json.Interceptors, Rest.Json, VK.Entity.Common, VK.Entity.Common.List;

type
  /// <summary>
  /// Объект, описывающий подарок
  /// </summary>
  TVkGift = class(TVkObject)
  private
    FThumb_256: string;
    FThumb_96: string;
    FThumb_48: string;
    FStickers_product_id: Integer;
  public
    /// <summary>
    /// Идентификатор подарка
    /// </summary>
    property Id;
    /// <summary>
    /// URL изображения 256x256px
    /// </summary>
    property Thumb256: string read FThumb_256 write FThumb_256;
    /// <summary>
    /// URL изображения 96x96px
    /// </summary>
    property Thumb96: string read FThumb_96 write FThumb_96;
    /// <summary>
    /// URL изображения 48x48px
    /// </summary>
    property Thumb48: string read FThumb_48 write FThumb_48;
    property StickersProductId: Integer read FStickers_product_id write FStickers_product_id;
  end;

  TVkGiftItem = class(TVkObject)
  private
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FFrom_id: Integer;
    FGift: TVkGift;
    FGift_hash: string;
    FMessage: string;
    FPrivacy: Integer;
    FAccess_key: string;
  public
    property AccessKey: string read FAccess_key write FAccess_key;
    property Date: TDateTime read FDate write FDate;
    property FromId: Integer read FFrom_id write FFrom_id;
    property Gift: TVkGift read FGift write FGift;
    property GiftHash: string read FGift_hash write FGift_hash;
    property Message: string read FMessage write FMessage;
    property Privacy: Integer read FPrivacy write FPrivacy;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkGiftItems = TVkEntityList<TVkGiftItem>;

implementation

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

