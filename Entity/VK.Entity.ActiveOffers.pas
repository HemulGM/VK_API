unit VK.Entity.ActiveOffers;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkActiveOffer = class
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
    property Description: string read FDescription write FDescription;
    property Id: string read FId write FId;
    property Img: string read FImg write FImg;
    property Instruction: string read FInstruction write FInstruction;
    property InstructionHtml: string read FInstruction_html write FInstruction_html;
    property Price: Extended read FPrice write FPrice;
    property ShortDescription: string read FShort_description write FShort_description;
    property Tag: string read FTag write FTag;
    property Title: string read FTitle write FTitle;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkActiveOffer;
  end;

  TVkActiveOffers = class
  private
    FCount: Extended;
    FItems: TArray<TVkActiveOffer>;
  public
    property count: Extended read FCount write FCount;
    property items: TArray<TVkActiveOffer> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkActiveOffers;
  end;

implementation

uses
  VK.CommonUtils;

{TVkActiveOffer}

function TVkActiveOffer.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkActiveOffer.FromJsonString(AJsonString: string): TVkActiveOffer;
begin
  result := TJson.JsonToObject<TVkActiveOffer>(AJsonString)
end;

{TVkActiveOffers}

destructor TVkActiveOffers.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkActiveOffer>(FItems);
  inherited;
end;

function TVkActiveOffers.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkActiveOffers.FromJsonString(AJsonString: string): TVkActiveOffers;
begin
  result := TJson.JsonToObject<TVkActiveOffers>(AJsonString)
end;

end.

