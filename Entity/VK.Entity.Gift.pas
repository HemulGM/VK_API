unit VK.Entity.Gift;

interface

uses
  REST.JsonReflect, Rest.Json, VK.Entity.Common, VK.Types, VK.Wrap.Interceptors,
  VK.Entity.Common.List;

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
    /// <summary>
    /// StickersProductId
    /// </summary>
    property StickersProductId: Integer read FStickers_product_id write FStickers_product_id;
  end;

  TVkGiftItem = class(TVkObject)
  private
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FFrom_id: TVkPeerId;
    FGift: TVkGift;
    FGift_hash: string;
    FMessage: string;
    [JsonReflectAttribute(ctString, rtString, TGiftPrivacyInterceptor)]
    FPrivacy: TVkGiftPrivacy;
    FAccess_key: string;
  public
    /// <summary>
    /// Идентификатор полученного подарка
    /// </summary>
    property Id;
    /// <summary>
    /// AccessKey
    /// </summary>
    property AccessKey: string read FAccess_key write FAccess_key;
    /// <summary>
    /// Время отправки подарка
    /// </summary>
    property Date: TDateTime read FDate write FDate;
    /// <summary>
    /// Идентификатор пользователя, который отправил подарок, или 0, если отправитель скрыт
    /// </summary>
    property FromId: TVkPeerId read FFrom_id write FFrom_id;
    /// <summary>
    /// Объект подарка
    /// </summary>
    property Gift: TVkGift read FGift write FGift;
    /// <summary>
    /// GiftHash
    /// </summary>
    property GiftHash: string read FGift_hash write FGift_hash;
    /// <summary>
    /// Текст сообщения, приложенного к подарку
    /// </summary>
    property Message: string read FMessage write FMessage;
    /// <summary>
    /// Значение приватности подарка (только для текущего пользователя)
    /// </summary>
    property Privacy: TVkGiftPrivacy read FPrivacy write FPrivacy;
    destructor Destroy; override;
  end;

  TVkGiftItems = TVkEntityList<TVkGiftItem>;

implementation

{ TVkGiftItem }

destructor TVkGiftItem.Destroy;
begin
  if Assigned(FGift) then
    FGift.Free;
  inherited;
end;

end.

