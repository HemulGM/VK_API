unit VK.Entity.PrettyCard;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Link;

type
  TVkCardImage = class
  private
    FHeight: Extended;
    FUrl: string;
    FWidth: Extended;
  public
    property height: Extended read FHeight write FHeight;
    property url: string read FUrl write FUrl;
    property width: Extended read FWidth write FWidth;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCardImage;
  end;

  TVkPrettyCard = class
  private
    FButton: TVkLinkButton;
    FCard_id: string;
    FImages: TArray<TVkCardImage>;
    FLink_url: string;
    FPrice: string;
    FPrice_old: string;
    FTitle: string;
  public
    property button: TVkLinkButton read FButton write FButton;
    property card_id: string read FCard_id write FCard_id;
    property images: TArray<TVkCardImage> read FImages write FImages;
    property link_url: string read FLink_url write FLink_url;
    property price: string read FPrice write FPrice;
    property price_old: string read FPrice_old write FPrice_old;
    property title: string read FTitle write FTitle;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPrettyCard;
  end;

  TVkPrettyCards = class
  private
    FItems: TArray<TVkPrettyCard>;
  public
    property Items: TArray<TVkPrettyCard> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPrettyCards;
  end;

implementation

{TVkCardImage}

function TVkCardImage.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCardImage.FromJsonString(AJsonString: string): TVkCardImage;
begin
  result := TJson.JsonToObject<TVkCardImage>(AJsonString)
end;

{TVkPrettyCard}

constructor TVkPrettyCard.Create;
begin
  inherited;
  FButton := TVkLinkButton.Create();
end;

destructor TVkPrettyCard.Destroy;
var
  LimagesItem: TVkCardImage;
begin

  for LimagesItem in FImages do
    LimagesItem.Free;

  FButton.Free;
  inherited;
end;

function TVkPrettyCard.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPrettyCard.FromJsonString(AJsonString: string): TVkPrettyCard;
begin
  result := TJson.JsonToObject<TVkPrettyCard>(AJsonString)
end;

{TVkPrettyCards}

destructor TVkPrettyCards.Destroy;
var
  LItemsItem: TVkPrettyCard;
begin

  for LItemsItem in FItems do
    LItemsItem.Free;

  inherited;
end;

function TVkPrettyCards.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPrettyCards.FromJsonString(AJsonString: string): TVkPrettyCards;
begin
  result := TJson.JsonToObject<TVkPrettyCards>(AJsonString)
end;

end.

