unit VK.Account.ActiveOffers;

interface

uses
  Generics.Collections, Rest.Json;

type
  TActiveOfferClass = class
  private
    FDescription: string;
    FId: string;
    FImg: string;
    FInstruction: string;
    FInstruction_html: string;
    FPrice: Extended;
    FShort_description: string;
    FTag: string;
    FTitle: string;
  public
    property description: string read FDescription write FDescription;
    property id: string read FId write FId;
    property img: string read FImg write FImg;
    property instruction: string read FInstruction write FInstruction;
    property instruction_html: string read FInstruction_html write FInstruction_html;
    property price: Extended read FPrice write FPrice;
    property short_description: string read FShort_description write FShort_description;
    property tag: string read FTag write FTag;
    property title: string read FTitle write FTitle;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TActiveOfferClass;
  end;

  TActiveOffers = class
  private
    FCount: Extended;
    FItems: TArray<TActiveOfferClass>;
  public
    property count: Extended read FCount write FCount;
    property items: TArray<TActiveOfferClass> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TActiveOffers;
  end;

implementation

{TActiveOfferClass}

function TActiveOfferClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TActiveOfferClass.FromJsonString(AJsonString: string): TActiveOfferClass;
begin
  result := TJson.JsonToObject<TActiveOfferClass>(AJsonString)
end;

{TActiveOffers}

destructor TActiveOffers.Destroy;
var
  LitemsItem: TActiveOfferClass;
begin

  for LitemsItem in FItems do
    LitemsItem.Free;

  inherited;
end;

function TActiveOffers.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TActiveOffers.FromJsonString(AJsonString: string): TActiveOffers;
begin
  result := TJson.JsonToObject<TActiveOffers>(AJsonString)
end;

end.

