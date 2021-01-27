unit VK.Entity.ActiveOffers;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Common.List;

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
  end;

  TVkActiveOffers = TVkEntityList<TVkActiveOffer>;

implementation

uses
  VK.CommonUtils;

end.

