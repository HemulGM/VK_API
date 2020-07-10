unit VK.Entity.Fave;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Link, VK.Entity.Media, VK.Entity.Video, VK.Entity.Market, VK.Entity.Photo;

type
  TVkFaveTags = class
  private
    FId: Integer;
    FName: string;
  public
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkFaveTags;
  end;

  TVkFave = class
  private
    FAdded_date: Int64;
    FSeen: Boolean;
    FTags: TArray<TVkFaveTags>;
    FType: string;
    FLink: TVkLink;
    FPost: TVkPost;
    FVideo: TVkVideo;
    FProduct: TVkProduct;
    FPhoto: TVkPhoto;
  public
    property AddedDate: Int64 read FAdded_date write FAdded_date;
    property Link: TVkLink read FLink write FLink;
    property Post: TVkPost read FPost write FPost;
    property Video: TVkVideo read FVideo write FVideo;
    property Photo: TVkPhoto read FPhoto write FPhoto;
    property Product: TVkProduct read FProduct write FProduct;
    property Seen: Boolean read FSeen write FSeen;
    property Tags: TArray<TVkFaveTags> read FTags write FTags;
    property&Type: string read FType write FType;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkFave;
  end;

  TVkFaves = class
  private
    FCount: Integer;
    FItems: TArray<TVkFave>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkFave> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkFaves;
  end;

implementation

{TTagsClass}

function TVkFaveTags.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkFaveTags.FromJsonString(AJsonString: string): TVkFaveTags;
begin
  result := TJson.JsonToObject<TVkFaveTags>(AJsonString)
end;

{TItemsClass}

constructor TVkFave.Create;
begin
  inherited;
  //FLink := TLinkClass.Create();
end;

destructor TVkFave.Destroy;
var
  LtagsItem: TVkFaveTags;
begin

  for LtagsItem in FTags do
    LtagsItem.Free;

  if Assigned(FLink) then
    FLink.Free;
  if Assigned(FPost) then
    FPost.Free;
  if Assigned(FVideo) then
    FVideo.Free;
  if Assigned(FProduct) then
    FProduct.Free;
  if Assigned(FPhoto) then
    FPhoto.Free;
  inherited;
end;

function TVkFave.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkFave.FromJsonString(AJsonString: string): TVkFave;
begin
  result := TJson.JsonToObject<TVkFave>(AJsonString)
end;

{TVkFaves}

destructor TVkFaves.Destroy;
var
  LitemsItem: TVkFave;
begin

  for LitemsItem in FItems do
    LitemsItem.Free;

  inherited;
end;

function TVkFaves.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkFaves.FromJsonString(AJsonString: string): TVkFaves;
begin
  result := TJson.JsonToObject<TVkFaves>(AJsonString)
end;

end.

