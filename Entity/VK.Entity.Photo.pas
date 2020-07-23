unit VK.Entity.Photo;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkPhotoTag = class
  private
    FDate: Int64;
    FId: Integer;
    FPlacer_id: Integer;
    FTagged_name: string;
    FUser_id: Integer;
    FViewed: Integer;
    FX: Integer;
    FX2: Integer;
    FY: Integer;
    FY2: Integer;
  public
    property Date: Int64 read FDate write FDate;
    property Id: Integer read FId write FId;
    property PlacerId: Integer read FPlacer_id write FPlacer_id;
    property TaggedName: string read FTagged_name write FTagged_name;
    property UserId: Integer read FUser_id write FUser_id;
    property Viewed: Integer read FViewed write FViewed;
    property X: Integer read FX write FX;
    property X2: Integer read FX2 write FX2;
    property Y: Integer read FY write FY;
    property Y2: Integer read FY2 write FY2;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPhotoTag;
  end;

  TVkPhotoTags = class
  private
    FItems: TArray<TVkPhotoTag>;
    FCount: Integer;
  public
    property Items: TArray<TVkPhotoTag> read FItems write FItems;
    property Count: Integer read FCount write FCount;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPhotoTags;
  end;

  TVkPhoto = class
  private
    FAlbum_id: Integer;
    FCan_comment: Integer;
    FCan_repost: Integer;
    FComments: TVkCommentsInfo;
    FDate: Int64;
    FId: Integer;
    FLikes: TVkLikesInfo;
    FOwner_id: Integer;
    FReposts: TVkRepostsInfo;
    FSizes: TVkSizes;
    FTags: TVkTags;
    FText: string;
    FUser_id: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FAccess_key: string;
    FPhoto_604: string;
    FPhoto_75: string;
    FPhoto_1280: string;
    FPhoto_807: string;
    FPhoto_2560: string;
    FPhoto_130: string;
    FHas_tags: Boolean;
    FTag_created: Int64;
    FPlacer_id: Integer;
    FTag_id: Integer;
  public
    property Id: Integer read FId write FId;
    property AlbumId: Integer read FAlbum_id write FAlbum_id;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property UserId: Integer read FUser_id write FUser_id;
    property Text: string read FText write FText;
    property Date: Int64 read FDate write FDate;
    property Sizes: TVkSizes read FSizes write FSizes;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    //
    property CanComment: Integer read FCan_comment write FCan_comment;
    property CanRepost: Integer read FCan_repost write FCan_repost;
    property Comments: TVkCommentsInfo read FComments write FComments;
    property Likes: TVkLikesInfo read FLikes write FLikes;
    property Reposts: TVkRepostsInfo read FReposts write FReposts;
    property Tags: TVkTags read FTags write FTags;
    property AccessKey: string read FAccess_key write FAccess_key;
    property HasTags: Boolean read FHas_tags write FHas_tags;
    // old field api < 5.77
    property Photo1280: string read FPhoto_1280 write FPhoto_1280;
    property Photo130: string read FPhoto_130 write FPhoto_130;
    property Photo2560: string read FPhoto_2560 write FPhoto_2560;
    property Photo604: string read FPhoto_604 write FPhoto_604;
    property Photo75: string read FPhoto_75 write FPhoto_75;
    property Photo807: string read FPhoto_807 write FPhoto_807;
    //
    property PlacerId: Integer read FPlacer_id write FPlacer_id;
    property TagCreated: Int64 read FTag_created write FTag_created;
    property TagId: Integer read FTag_id write FTag_id;
    //
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPhoto;
  end;

  TVkCropPhoto = class
  private
    FCrop: TVkCrop;
    FPhoto: TVkPhoto;
    FRect: TVkRect;
  public
    property Photo: TVkPhoto read FPhoto write FPhoto;
    property Crop: TVkCrop read FCrop write FCrop;
    property Rect: TVkRect read FRect write FRect;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCropPhoto;
  end;

  TVkPostedPhoto = class
  private
    FId: Integer;
    FOwner_id: Integer;
    FPhoto_130: string;
    FPhoto_604: string;
  public
    property Id: Integer read FId write FId;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property Photo130: string read FPhoto_130 write FPhoto_130;
    property Photo604: string read FPhoto_604 write FPhoto_604;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPostedPhoto;
  end;

  TVkPhotos = class
  private
    FItems: TArray<TVkPhoto>;
    FCount: Integer;
    FSaveObjects: Boolean;
    procedure SetSaveObjects(const Value: Boolean);
  public
    property Items: TArray<TVkPhoto> read FItems write FItems;
    property Count: Integer read FCount write FCount;
    property SaveObjects: Boolean read FSaveObjects write SetSaveObjects;
    procedure Append(Users: TVkPhotos);
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPhotos;
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
  LsizesItem: TVkSize;
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

{TVkPhotos}

procedure TVkPhotos.Append(Users: TVkPhotos);
var
  OldLen: Integer;
begin
  OldLen := Length(Items);
  SetLength(FItems, OldLen + Length(Users.Items));
  Move(Users.Items[0], FItems[OldLen], Length(Users.Items) * SizeOf(TVkPhoto));
end;

constructor TVkPhotos.Create;
begin
  inherited;
  FSaveObjects := False;
end;

destructor TVkPhotos.Destroy;
var
  LItemsItem: TVkPhoto;
begin
  if not FSaveObjects then
  begin
    for LItemsItem in FItems do
      LItemsItem.Free;
  end;

  inherited;
end;

function TVkPhotos.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPhotos.FromJsonString(AJsonString: string): TVkPhotos;
begin
  result := TJson.JsonToObject<TVkPhotos>(AJsonString);
end;

procedure TVkPhotos.SetSaveObjects(const Value: Boolean);
begin
  FSaveObjects := Value;
end;

{ TVkCropPhoto }

class function TVkCropPhoto.FromJsonString(AJsonString: string): TVkCropPhoto;
begin
  result := TJson.JsonToObject<TVkCropPhoto>(AJsonString);
end;

function TVkCropPhoto.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkPhotoTag }

class function TVkPhotoTag.FromJsonString(AJsonString: string): TVkPhotoTag;
begin
  result := TJson.JsonToObject<TVkPhotoTag>(AJsonString);
end;

function TVkPhotoTag.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkPhotoTags }

destructor TVkPhotoTags.Destroy;
var
  LItemsItem: TVkPhotoTag;
begin
  for LItemsItem in FItems do
    LItemsItem.Free;
  inherited;
end;

class function TVkPhotoTags.FromJsonString(AJsonString: string): TVkPhotoTags;
begin
  result := TJson.JsonToObject<TVkPhotoTags>(AJsonString);
end;

function TVkPhotoTags.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

end.

