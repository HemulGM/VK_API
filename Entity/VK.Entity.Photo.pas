unit VK.Entity.Photo;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkPhoto = class
  private
    FAlbum_id: Extended;
    FCan_comment: Extended;
    FCan_repost: Extended;
    FComments: TVkCommentsInfo;
    FDate: Extended;
    FId: Extended;
    FLikes: TVkLikesInfo;
    FOwner_id: Extended;
    FReposts: TVkRepostsInfo;
    FSizes: TArray<TVkSizes>;
    FTags: TVkTags;
    FText: string;
  public
    property album_id: Extended read FAlbum_id write FAlbum_id;
    property can_comment: Extended read FCan_comment write FCan_comment;
    property can_repost: Extended read FCan_repost write FCan_repost;
    property comments: TVkCommentsInfo read FComments write FComments;
    property date: Extended read FDate write FDate;
    property id: Extended read FId write FId;
    property likes: TVkLikesInfo read FLikes write FLikes;
    property owner_id: Extended read FOwner_id write FOwner_id;
    property reposts: TVkRepostsInfo read FReposts write FReposts;
    property sizes: TArray<TVkSizes> read FSizes write FSizes;
    property tags: TVkTags read FTags write FTags;
    property text: string read FText write FText;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPhoto;
  end;

  TVkPostedPhoto = class
  private
    FId: Extended;
    FOwner_id: Extended;
    FPhoto_130: string;
    FPhoto_604: string;
  public
    property id: Extended read FId write FId;
    property owner_id: Extended read FOwner_id write FOwner_id;
    property photo_130: string read FPhoto_130 write FPhoto_130;
    property photo_604: string read FPhoto_604 write FPhoto_604;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPostedPhoto;
  end;

implementation

{TRootClass}

constructor TVkPhoto.Create;
begin
  inherited;
  FLikes := TVkLikesInfo.Create();
  FReposts := TVkRepostsInfo.Create();
  FComments := TVkCommentsInfo.Create();
  FTags := TVkTags.Create();
end;

destructor TVkPhoto.Destroy;
var
  LsizesItem: TVkSizes;
begin

  for LsizesItem in FSizes do
    LsizesItem.Free;

  FLikes.Free;
  FReposts.Free;
  FComments.Free;
  FTags.Free;
  inherited;
end;

function TVkPhoto.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPhoto.FromJsonString(AJsonString: string): TVkPhoto;
begin
  result := TJson.JsonToObject<TVkPhoto>(AJsonString)
end;

{ TVkPostedPhoto }

class function TVkPostedPhoto.FromJsonString(AJsonString: string): TVkPostedPhoto;
begin
  result := TJson.JsonToObject<TVkPostedPhoto>(AJsonString)
end;

function TVkPostedPhoto.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

end.

