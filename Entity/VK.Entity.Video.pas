unit VK.Entity.Video;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkVideoFiles = class
  private
    FExternal: string;
  public
    property external: string read FExternal write FExternal;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkVideoFiles;
  end;

  TVkVideoImage = class
  private
    FHeight: Extended;
    FUrl: string;
    FWidth: Extended;
    FWith_padding: Extended;
  public
    property Height: Extended read FHeight write FHeight;
    property Url: string read FUrl write FUrl;
    property Width: Extended read FWidth write FWidth;
    property WithPadding: Extended read FWith_padding write FWith_padding;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkVideoImage;
  end;

  TVkVideo = class
  private
    FAccess_key: string;
    FAdded: Extended;
    FCan_add: Extended;
    FCan_add_to_faves: Extended;
    FCan_attach_link: Extended;
    FCan_comment: Extended;
    FCan_edit: Extended;
    FCan_like: Extended;
    FCan_repost: Extended;
    FCan_subscribe: Extended;
    FComments: Extended;
    FDate: Extended;
    FDescription: string;
    FDuration: Extended;
    FFiles: TVkVideoFiles;
    FId: Extended;
    FImage: TArray<TVkVideoImage>;
    FIs_favorite: Boolean;
    FLikes: TVkLikesInfo;
    FLocal_views: Extended;
    FOwner_id: Extended;
    FPlatform: string;
    FPlayer: string;
    FReposts: TVkRepostsInfo;
    FTitle: string;
    FType: string;
    FViews: Extended;
    Ffirst_frame_800: string;
    Fphoto_640: string;
    Ffirst_frame_320: string;
    Ffirst_frame_130: string;
    Fphoto_1280: string;
    Ffirst_frame_640: string;
    Fphoto_800: string;
    Fphoto_320: string;
    Ffirst_frame_1280: string;
    Fphoto_130: string;
    FAdding_date: Extended;
    FIs_private: Integer;
    FProcessing: Integer;
    FLive: Integer;
    FUpcoming: Integer;
  public
    property Id: Extended read FId write FId;
    property OwnerId: Extended read FOwner_id write FOwner_id;
    property Title: string read FTitle write FTitle;
    property Description: string read FDescription write FDescription;
    property Duration: Extended read FDuration write FDuration;
    property Photo130: string read Fphoto_130 write Fphoto_130;
    property Photo320: string read Fphoto_320 write Fphoto_320;
    property Photo640: string read Fphoto_640 write Fphoto_640;
    property Photo800: string read Fphoto_800 write Fphoto_800;
    property Photo1280: string read Fphoto_1280 write Fphoto_1280;
    property FirstFrame130: string read Ffirst_frame_130 write Ffirst_frame_130;
    property FirstFrame320: string read Ffirst_frame_320 write Ffirst_frame_320;
    property FirstFrame640: string read Ffirst_frame_640 write Ffirst_frame_640;
    property FirstFrame800: string read Ffirst_frame_800 write Ffirst_frame_800;
    property FirstFrame1280: string read Ffirst_frame_1280 write Ffirst_frame_1280;
    property Date: Extended read FDate write FDate;
    property AddingDate: Extended read FAdding_date write FAdding_date;
    property Views: Extended read FViews write FViews;
    property Comments: Extended read FComments write FComments;
    property Player: string read FPlayer write FPlayer;
    property&Platform: string read FPlatform write FPlatform;
    property CanEdit: Extended read FCan_edit write FCan_edit;
    property CanAdd: Extended read FCan_add write FCan_add;
    property IsPrivate: Integer read FIs_private write FIs_private;
    property AccessKey: string read FAccess_key write FAccess_key;
    property Processing: Integer read FProcessing write FProcessing;
    property Live: Integer read FLive write FLive;
    property UpComing: Integer read FUpcoming write FUpcoming;
    property IsFavorite: Boolean read FIs_favorite write FIs_favorite;
    //
    property Added: Extended read FAdded write FAdded;
    property CanAddToFaves: Extended read FCan_add_to_faves write FCan_add_to_faves;
    property CanAttachLink: Extended read FCan_attach_link write FCan_attach_link;
    property CanComment: Extended read FCan_comment write FCan_comment;
    property CanLike: Extended read FCan_like write FCan_like;
    property CanRepost: Extended read FCan_repost write FCan_repost;
    property CanSubscribe: Extended read FCan_subscribe write FCan_subscribe;
    property Files: TVkVideoFiles read FFiles write FFiles;
    property Image: TArray<TVkVideoImage> read FImage write FImage;
    property Likes: TVkLikesInfo read FLikes write FLikes;
    property LocalViews: Extended read FLocal_views write FLocal_views;
    property Reposts: TVkRepostsInfo read FReposts write FReposts;
    property&Type: string read FType write FType;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkVideo;
  end;

implementation

{TVkVideoFiles}

function TVkVideoFiles.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkVideoFiles.FromJsonString(AJsonString: string): TVkVideoFiles;
begin
  result := TJson.JsonToObject<TVkVideoFiles>(AJsonString)
end;

{TVkVideoImage}

function TVkVideoImage.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkVideoImage.FromJsonString(AJsonString: string): TVkVideoImage;
begin
  result := TJson.JsonToObject<TVkVideoImage>(AJsonString)
end;

{TVkVideo}

constructor TVkVideo.Create;
begin
  inherited;
  FFiles := TVkVideoFiles.Create();
  FLikes := TVkLikesInfo.Create();
  FReposts := TVkRepostsInfo.Create();
end;

destructor TVkVideo.Destroy;
var
  LimageItem: TVkVideoImage;
begin

  for LimageItem in FImage do
    LimageItem.Free;

  FFiles.Free;
  FLikes.Free;
  FReposts.Free;
  inherited;
end;

function TVkVideo.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkVideo.FromJsonString(AJsonString: string): TVkVideo;
begin
  result := TJson.JsonToObject<TVkVideo>(AJsonString)
end;

end.

