unit VK.Entity.Video;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Privacy,
  VK.Entity.Attachment;

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
    FHeight: Integer;
    FUrl: string;
    FWidth: Integer;
    FWith_padding: Integer;
  public
    property Height: Integer read FHeight write FHeight;
    property Url: string read FUrl write FUrl;
    property Width: Integer read FWidth write FWidth;
    property WithPadding: Integer read FWith_padding write FWith_padding;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkVideoImage;
  end;

  TVkVideo = class(TVkObject, IAttachment)
  private
    FAccess_key: string;
    FAdded: Int64;
    FCan_add: Boolean;
    FCan_add_to_faves: Boolean;
    FCan_attach_link: Boolean;
    FCan_comment: Boolean;
    FCan_edit: Boolean;
    FCan_like: Boolean;
    FCan_repost: Boolean;
    FCan_subscribe: Boolean;
    FComments: Integer;
    FDate: Int64;
    FDescription: string;
    FDuration: Integer;
    FFiles: TVkVideoFiles;
    FImage: TArray<TVkVideoImage>;
    FIs_favorite: Boolean;
    FLikes: TVkLikesInfo;
    FLocal_views: Integer;
    FOwner_id: Integer;
    FPlatform: string;
    FPlayer: string;
    FReposts: TVkRepostsInfo;
    FTitle: string;
    FType: string;
    FViews: Integer;
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
    FAdding_date: Int64;
    FIs_private: Integer;
    FProcessing: Integer;
    FLive: Integer;
    FUpcoming: Integer;
  public
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property Title: string read FTitle write FTitle;
    property Description: string read FDescription write FDescription;
    property Duration: Integer read FDuration write FDuration;
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
    property Date: Int64 read FDate write FDate;
    property AddingDate: Int64 read FAdding_date write FAdding_date;
    property Views: Integer read FViews write FViews;
    property Comments: Integer read FComments write FComments;
    property Player: string read FPlayer write FPlayer;
    property&Platform: string read FPlatform write FPlatform;
    property CanEdit: Boolean read FCan_edit write FCan_edit;
    property CanAdd: Boolean read FCan_add write FCan_add;
    property IsPrivate: Integer read FIs_private write FIs_private;
    property AccessKey: string read FAccess_key write FAccess_key;
    property Processing: Integer read FProcessing write FProcessing;
    property Live: Integer read FLive write FLive;
    property UpComing: Integer read FUpcoming write FUpcoming;
    property IsFavorite: Boolean read FIs_favorite write FIs_favorite;
    //
    property Added: Int64 read FAdded write FAdded;
    property CanAddToFaves: Boolean read FCan_add_to_faves write FCan_add_to_faves;
    property CanAttachLink: Boolean read FCan_attach_link write FCan_attach_link;
    property CanComment: Boolean read FCan_comment write FCan_comment;
    property CanLike: Boolean read FCan_like write FCan_like;
    property CanRepost: Boolean read FCan_repost write FCan_repost;
    property CanSubscribe: Boolean read FCan_subscribe write FCan_subscribe;
    property Files: TVkVideoFiles read FFiles write FFiles;
    property Image: TArray<TVkVideoImage> read FImage write FImage;
    property Likes: TVkLikesInfo read FLikes write FLikes;
    property LocalViews: Integer read FLocal_views write FLocal_views;
    property Reposts: TVkRepostsInfo read FReposts write FReposts;
    property&Type: string read FType write FType;
    function ToAttachment: string;
    constructor Create; override;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkVideo;
  end;

  TVkVideos = class
  private
    FItems: TArray<TVkVideo>;
    FCount: Integer;
    FSaveObjects: Boolean;
    procedure SetSaveObjects(const Value: Boolean);
  public
    property Items: TArray<TVkVideo> read FItems write FItems;
    property Count: Integer read FCount write FCount;
    property SaveObjects: Boolean read FSaveObjects write SetSaveObjects;
    procedure Append(Users: TVkVideos);
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkVideos;
  end;

  TVkVideoAlbum = class(TVkObject)
  private
    FCount: Integer;
    FImage: TArray<TVkVideoImage>;
    FOwner_id: Integer;
    FPrivacy: TVkPrivacy;
    FTitle: string;
    FUpdated_time: Int64;
  public
    property Count: Integer read FCount write FCount;
    property Image: TArray<TVkVideoImage> read FImage write FImage;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property Privacy: TVkPrivacy read FPrivacy write FPrivacy;
    property Title: string read FTitle write FTitle;
    property UpdatedTime: Int64 read FUpdated_time write FUpdated_time;
    constructor Create; override;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkVideoAlbum;
  end;

  TVkVideoAlbums = class
  private
    FCount: Integer;
    FItems: TArray<TVkVideoAlbum>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkVideoAlbum> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkVideoAlbums;
  end;

implementation

uses
  VK.Types, VK.CommonUtils;

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
begin
  TArrayHelp.FreeArrayOfObject<TVkVideoImage>(FImage);
  FFiles.Free;
  FLikes.Free;
  FReposts.Free;
  inherited;
end;

function TVkVideo.ToAttachment: string;
begin
  Result := Attachment.Video(Id, OwnerId, AccessKey);
end;

function TVkVideo.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkVideo.FromJsonString(AJsonString: string): TVkVideo;
begin
  result := TJson.JsonToObject<TVkVideo>(AJsonString)
end;

{TVkVideos}

procedure TVkVideos.Append(Users: TVkVideos);
var
  OldLen: Integer;
begin
  OldLen := Length(Items);
  SetLength(FItems, OldLen + Length(Users.Items));
  Move(Users.Items[0], FItems[OldLen], Length(Users.Items) * SizeOf(TVkVideo));
end;

constructor TVkVideos.Create;
begin
  inherited;
  FSaveObjects := False;
end;

destructor TVkVideos.Destroy;
begin
  {$IFNDEF AUTOREFCOUNT}
  if not FSaveObjects then
  begin
    TArrayHelp.FreeArrayOfObject<TVkVideo>(FItems);
  end;
  {$ENDIF}
  inherited;
end;

function TVkVideos.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkVideos.FromJsonString(AJsonString: string): TVkVideos;
begin
  result := TJson.JsonToObject<TVkVideos>(AJsonString);
end;

procedure TVkVideos.SetSaveObjects(const Value: Boolean);
begin
  FSaveObjects := Value;
end;

{TVkVideoAlbum}

constructor TVkVideoAlbum.Create;
begin
  inherited;
  FPrivacy := TVkPrivacy.Create();
end;

destructor TVkVideoAlbum.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkVideoImage>(FImage);
  FPrivacy.Free;
  inherited;
end;

function TVkVideoAlbum.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkVideoAlbum.FromJsonString(AJsonString: string): TVkVideoAlbum;
begin
  result := TJson.JsonToObject<TVkVideoAlbum>(AJsonString)
end;

{TVkVideoAlbums}

destructor TVkVideoAlbums.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkVideoAlbum>(FItems);
  inherited;
end;

function TVkVideoAlbums.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkVideoAlbums.FromJsonString(AJsonString: string): TVkVideoAlbums;
begin
  result := TJson.JsonToObject<TVkVideoAlbums>(AJsonString)
end;

end.

