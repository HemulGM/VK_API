unit VK.Entity.PrettyCard;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Link, VK.Entity.Common;

type
  TVkCardImage = class
  private
    FHeight: Extended;
    FUrl: string;
    FWidth: Extended;
  public
    property Height: Extended read FHeight write FHeight;
    property Url: string read FUrl write FUrl;
    property Width: Extended read FWidth write FWidth;
  end;

  TVkPrettyCard = class(TVkEntity)
  private
    FButton: TVkLinkButton;
    FCard_id: string;
    FImages: TArray<TVkCardImage>;
    FLink_url: string;
    FPrice: string;
    FPrice_old: string;
    FTitle: string;
  public
    property Button: TVkLinkButton read FButton write FButton;
    property CardId: string read FCard_id write FCard_id;
    property Images: TArray<TVkCardImage> read FImages write FImages;
    property LinkUrl: string read FLink_url write FLink_url;
    property Price: string read FPrice write FPrice;
    property PriceOld: string read FPrice_old write FPrice_old;
    property Title: string read FTitle write FTitle;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkPrettyCards = class
  private
    FItems: TArray<TVkPrettyCard>;
  public
    property Items: TArray<TVkPrettyCard> read FItems write FItems;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TVkPrettyCard}

constructor TVkPrettyCard.Create;
begin
  inherited;
  FButton := TVkLinkButton.Create();
end;

destructor TVkPrettyCard.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkCardImage>(FImages);
  FButton.Free;
  inherited;
end;

{TVkPrettyCards}

destructor TVkPrettyCards.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkPrettyCard>(FItems);
  inherited;
end;

end.

