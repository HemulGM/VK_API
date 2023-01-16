unit VK.Entity.PrettyCard;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Link, VK.Entity.Common,
  VK.Entity.Common.List;

type
  TVkPrettyCard = class(TVkEntity)
  private
    FButton: TVkLinkButton;
    FCard_id: string;
    FImages: TArray<TVkSize>;
    FLink_url: string;
    FPrice: string;
    FPrice_old: string;
    FTitle: string;
  public
    property Button: TVkLinkButton read FButton write FButton;
    property CardId: string read FCard_id write FCard_id;
    property Images: TArray<TVkSize> read FImages write FImages;
    property LinkUrl: string read FLink_url write FLink_url;
    property Price: string read FPrice write FPrice;
    property PriceOld: string read FPrice_old write FPrice_old;
    property Title: string read FTitle write FTitle;
    destructor Destroy; override;
  end;

  TVkPrettyCards = TVkEntityList<TVkPrettyCard>;

implementation

uses
  VK.CommonUtils;

{TVkPrettyCard}

destructor TVkPrettyCard.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkSize>(FImages);
  if Assigned(FButton) then
    FButton.Free;
  inherited;
end;

end.

