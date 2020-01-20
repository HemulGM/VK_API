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
    FUser_id: Extended;
    FWidth: Extended;
    FHeight: Extended;
    FAccess_key: string;
    FPhoto_604: string;
    FPhoto_75: string;
    FPhoto_1280: string;
    FPhoto_807: string;
    FPhoto_2560: string;
    FPhoto_130: string;
  public
    property id: Extended read FId write FId;
    property album_id: Extended read FAlbum_id write FAlbum_id;
    property owner_id: Extended read FOwner_id write FOwner_id;
    property user_id: Extended read FUser_id write FUser_id;
    property text: string read FText write FText;
    property date: Extended read FDate write FDate;
    property sizes: TArray<TVkSizes> read FSizes write FSizes;
    property width: Extended read FWidth write FWidth;
    property height: Extended read FHeight write FHeight;
    //
    property can_comment: Extended read FCan_comment write FCan_comment;
    property can_repost: Extended read FCan_repost write FCan_repost;
    property comments: TVkCommentsInfo read FComments write FComments;
    property likes: TVkLikesInfo read FLikes write FLikes;
    property reposts: TVkRepostsInfo read FReposts write FReposts;
    property tags: TVkTags read FTags write FTags;
    property access_key: string read FAccess_key write FAccess_key;
    //
    property photo_1280: string read FPhoto_1280 write FPhoto_1280;
    property photo_130: string read FPhoto_130 write FPhoto_130;
    property photo_2560: string read FPhoto_2560 write FPhoto_2560;
    property photo_604: string read FPhoto_604 write FPhoto_604;
    property photo_75: string read FPhoto_75 write FPhoto_75;
    property photo_807: string read FPhoto_807 write FPhoto_807;
    //
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

{TVkPhoto}

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

