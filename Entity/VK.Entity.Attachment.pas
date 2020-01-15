unit VK.Entity.Attachment;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Photo, VK.Entity.Link;

type
  TVkAttachment = class
  private
    FLink: TVkLink;
    FPosted_photo: TVkPostedPhoto;
    FType: string;
  public
    property link: TVkLink read FLink write FLink;
    property posted_photo: TVkPostedPhoto read FPosted_photo write FPosted_photo;
    property&type: string read FType write FType;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkAttachment;
  end;

implementation

{TAttachmentsClass}

constructor TVkAttachment.Create;
begin
  inherited;
  FLink := TVkLink.Create();
end;

destructor TVkAttachment.Destroy;
begin
  FLink.Free;
  inherited;
end;

function TVkAttachment.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkAttachment.FromJsonString(AJsonString: string): TVkAttachment;
begin
  result := TJson.JsonToObject<TVkAttachment>(AJsonString)
end;

end.

