unit VK.Entity.ActiveOffers;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkActiveOffer = class(TVkObject)
  private
    FDescription: string;
    FImg: string;
    FInstruction: string;
    FInstruction_html: string;
    FPrice: Integer;
    FShort_description: string;
    FTag: string;
    FTitle: string;
  public
    property Description: string read FDescription write FDescription;
    property Img: string read FImg write FImg;
    property Instruction: string read FInstruction write FInstruction;
    property InstructionHtml: string read FInstruction_html write FInstruction_html;
    property Price: Integer read FPrice write FPrice;
    property ShortDescription: string read FShort_description write FShort_description;
    property Tag: string read FTag write FTag;
    property Title: string read FTitle write FTitle;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkActiveOffer;
  end;

  TVkActiveOffers = class
  private
    FCount: Integer;
    FItems: TArray<TVkActiveOffer>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkActiveOffer> read FItems write FItems;
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

