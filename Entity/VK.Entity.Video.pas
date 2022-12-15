unit VK.Entity.Video;

interface

uses
  Generics.Collections, REST.Json.Interceptors, REST.JsonReflect, Rest.Json,
  VK.Entity.Common, VK.Entity.Privacy, VK.Entity.Common.List, VK.Entity.Info,
  VK.Types, VK.Wrap.Interceptors;

type
  TVkVideoFiles = class(TVkEntity)
  private
    FExternal: string;
    FMp4_720: string;
    FMp4_360: string;
    FMp4_480: string;
    FMp4_240: string;
    FHls_ondemand: string;
    FDash_ondemand: string;
    FFailover_host: string;
    FDash_uni: string;
    FDash_sep: string;
    FMp4_144: string;
    FMp4_1080: string;
  public
    property &External: string read FExternal write FExternal;
    property MP4_144: string read FMp4_144 write FMp4_144;
    property MP4_240: string read FMp4_240 write FMp4_240;
    property MP4_360: string read FMp4_360 write FMp4_360;
    property MP4_480: string read FMp4_480 write FMp4_480;
    property MP4_720: string read FMp4_720 write FMp4_720;
    property MP4_1080: string read FMp4_1080 write FMp4_1080;
    property HLSOndemand: string read FHls_ondemand write FHls_ondemand;
    property FashOndemand: string read FDash_ondemand write FDash_ondemand;
    property FailoverHost: string read FFailover_host write FFailover_host;
    property DashUni: string read FDash_uni write FDash_uni;
    property DashSep: string read FDash_sep write FDash_sep;
  end;

  TVkVideoImage = class(TVkSize)
  private
    FWith_padding: Integer;
  public
    property WithPadding: Integer read FWith_padding write FWith_padding;
  end;

  TVkTimelineThumbs = class(TVkEntity)
  private
    FCount_per_image: Integer;
    FCount_per_row: Integer;
    FCount_total: Integer;
    FFrame_height: Extended;
    FFrame_width: Extended;
    FFrequency: Extended;
    FIs_uv: Boolean;
    FLinks: TArray<string>;
  public
    property CountPerImage: Integer read FCount_per_image write FCount_per_image;
    property CountPerRow: Integer read FCount_per_row write FCount_per_row;
    property CountTotal: Integer read FCount_total write FCount_total;
    property FrameHeight: Extended read FFrame_height write FFrame_height;
    property FrameWidth: Extended read FFrame_width write FFrame_width;
    property Frequency: Extended read FFrequency write FFrequency;
    property IsUV: Boolean read FIs_uv write FIs_uv;
    property Links: TArray<string> read FLinks write FLinks;
  end;

  TVkVideo = class(TVkObject, IAttachment)
  private
    FAccess_key: string;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FAdded: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_add: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_add_to_faves: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_attach_link: Boolean;
    FCan_comment: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_edit: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_like: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_repost: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_subscribe: Boolean;
    FComments: Integer;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FDescription: string;
    FDuration: Int64;
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
    [JsonReflectAttribute(ctString, rtString, TVideoTypeInterceptor)]
    FType: TVkVideoType; //video
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
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FAdding_date: TDateTime;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FIs_private: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FProcessing: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FLive: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FUpcoming: Boolean;
    FFirstFrame: TArray<TVkSize>;
    FWidth: Integer;
    FHeight: Integer;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FRepeat: Boolean;
    FUser_id: Integer;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FConverting: Boolean;
    FIs_subscribed: Boolean;
    FBalance: Integer;
    [JsonReflectAttribute(ctString, rtString, TLiveStatusInterceptor)]
    FLive_status: TVkLiveStatus;
    FSpectators: Integer;
    FIs_author: Boolean;
    FTrack_code: string;
    FTimeline_thumbs: TVkTimelineThumbs;
    FCan_download: integer;
    FOv_id: string;
  public
    /// <summary>
    /// ���� ������� � �������
    /// </summary>
    property AccessKey: string read FAccess_key write FAccess_key;
    /// <summary>
    /// ��������� �� ����� � ������� ������������
    /// </summary>
    property Added: Boolean read FAdded write FAdded;
    /// <summary>
    /// ���� ���������� ����������� ������������� ��� �������
    /// </summary>
    property AddingDate: TDateTime read FAdding_date write FAdding_date;
    /// <summary>
    /// ������ ������� � ������ ����������
    /// </summary>
    property Balance: Integer read FBalance write FBalance;
    /// <summary>
    /// ����� �� ������������ �������� ����������� � ����
    /// </summary>
    property CanAdd: Boolean read FCan_add write FCan_add;
    /// <summary>
    /// ����� �� ������������ �������� ����� � ���������
    /// </summary>
    property CanAddToFaves: Boolean read FCan_add_to_faves write FCan_add_to_faves;
    /// <summary>
    /// ����� �� ������������ ���������� ������ �������� � �����
    /// </summary>
    property CanAttachLink: Boolean read FCan_attach_link write FCan_attach_link;
    /// <summary>
    /// ����� �� ������������ �������������� �����
    /// </summary>
    property CanComment: Boolean read FCan_comment write FCan_comment;
    /// <summary>
    /// ����� �� ������� (�� ��������)
    /// </summary>
    property CanDownload: integer read FCan_download write FCan_download;
    /// <summary>
    /// ����� �� ������������ ������������� �����
    /// </summary>
    property CanEdit: Boolean read FCan_edit write FCan_edit;
    /// <summary>
    /// ����� �� ������������ �������� ����� � ������ <<��� ��������>>
    /// </summary>
    property CanLike: Boolean read FCan_like write FCan_like;
    /// <summary>
    /// ����� �� ������������ ������� ������ �����
    /// </summary>
    property CanRepost: Boolean read FCan_repost write FCan_repost;
    /// <summary>
    /// ����� �� ������������ ����������� �� ������ �����
    /// </summary>
    property CanSubscribe: Boolean read FCan_subscribe write FCan_subscribe;
    /// <summary>
    /// ���������� ������������ � �����������
    /// </summary>
    property Comments: Integer read FComments write FComments;
    /// <summary>
    /// �������������� �� �����
    /// </summary>
    property Converting: Boolean read FConverting write FConverting;
    /// <summary>
    /// ���� �������� �����������
    /// </summary>
    property Date: TDateTime read FDate write FDate;
    /// <summary>
    /// ����� �������� �����������
    /// </summary>
    property Description: string read FDescription write FDescription;
    /// <summary>
    /// ������������ ������ � ��������
    /// </summary>
    property Duration: Int64 read FDuration write FDuration;
    property Files: TVkVideoFiles read FFiles write FFiles;
    property FirstFrame1280: string read Ffirst_frame_1280 write Ffirst_frame_1280;
    property FirstFrame130: string read Ffirst_frame_130 write Ffirst_frame_130;
    property FirstFrame320: string read Ffirst_frame_320 write Ffirst_frame_320;
    property FirstFrame640: string read Ffirst_frame_640 write Ffirst_frame_640;
    property FirstFrame800: string read Ffirst_frame_800 write Ffirst_frame_800;
    /// <summary>
    /// ����������� ������� �����
    /// </summary>
    property FirstFrame: TArray<TVkSize> read FFirstFrame write FFirstFrame;
    /// <summary>
    /// ������ �����
    /// </summary>
    property Height: Integer read FHeight write FHeight;
    /// <summary>
    /// ������������� �����������
    /// </summary>
    property Id;
    /// <summary>
    /// ����������� �������
    /// </summary>
    property Image: TArray<TVkVideoImage> read FImage write FImage;
    /// <summary>
    /// True, ���� ���� - ����� �������
    /// </summary>
    property IsAuthor: Boolean read FIs_author write FIs_author;
    /// <summary>
    /// True, ���� ������ �������� � �������� � �������� ������������
    /// </summary>
    property IsFavorite: Boolean read FIs_favorite write FIs_favorite;
    /// <summary>
    /// ���� ������������, ���� ����������� ��������� (��������, ���� ��������� � ������ ���������), ������ �������� True
    /// </summary>
    property IsPrivate: Boolean read FIs_private write FIs_private;
    /// <summary>
    /// �������� �� ������������ �� ������ �����
    /// </summary>
    property IsSubscribed: Boolean read FIs_subscribed write FIs_subscribed;
    /// <summary>
    /// �������� ������ ������� <<��� ��������>>
    /// </summary>
    property Likes: TVkLikesInfo read FLikes write FLikes;
    /// <summary>
    /// ���� ������������ � ��� ������, ���� ����������� �������� ������ �����������, ������ �������� True. �������� ��������, � ���� ������ � ���� duration ���������� �������� False
    /// </summary>
    property Live: Boolean read FLive write FLive;
    /// <summary>
    /// ������ ������ ����������
    /// </summary>
    property LiveStatus: TVkLiveStatus read FLive_status write FLive_status;
    /// <summary>
    /// ���� ����� �������, ���������� ���������� � ��
    /// </summary>
    property LocalViews: Integer read FLocal_views write FLocal_views;
    property OvId: string read FOv_id write FOv_id;
    /// <summary>
    /// ������������� ��������� �����������
    /// </summary>
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property Photo130: string read Fphoto_130 write Fphoto_130;
    property Photo320: string read Fphoto_320 write Fphoto_320;
    property Photo640: string read Fphoto_640 write Fphoto_640;
    property Photo800: string read Fphoto_800 write Fphoto_800;
    property Photo1280: string read Fphoto_1280 write Fphoto_1280;
    /// <summary>
    /// URL �������� � �������, ������� ����� ������������ ��� ��������������� ������ � ��������. �������������� flash � html5, ����� ������ �������������� �� ������� ����
    /// </summary>
    property Player: string read FPlayer write FPlayer;
    /// <summary>
    /// ���� ������������ � ��� ������, ���� ���������� ��������� � �������� ���������, ������ �������� 1
    /// </summary>
    property Processing: Boolean read FProcessing write FProcessing;
    /// <summary>
    /// ���� ������������ � ��� ������, ���� ����� ���������, ������ �������� 1
    /// </summary>
    property &Repeat: Boolean read FRepeat write FRepeat;
    /// <summary>
    /// �������� ������ �������
    /// </summary>
    property Reposts: TVkRepostsInfo read FReposts write FReposts;
    /// <summary>
    /// ���������� �������� ������ ����������
    /// </summary>
    property Spectators: Integer read FSpectators write FSpectators;
    property TimelineThumbs: TVkTimelineThumbs read FTimeline_thumbs write FTimeline_thumbs;
    /// <summary>
    /// �������� �����������
    /// </summary>
    property Title: string read FTitle write FTitle;
    /// <summary>
    /// TrackCode
    /// </summary>
    property TrackCode: string read FTrack_code write FTrack_code;
    /// <summary>
    /// ���� ��������������� � ���, ��� ���������� ����� ������� (��� live = 1)
    /// </summary>
    property UpComing: Boolean read FUpcoming write FUpcoming;
    /// <summary>
    /// ������������� ������������, ������������ �����, ���� ��� ���� ��������� � ������ ����� �� ����������
    /// </summary>
    property UserId: Integer read FUser_id write FUser_id;
    /// <summary>
    /// ���������� ���������� �����������
    /// </summary>
    property Views: Integer read FViews write FViews;
    /// <summary>
    /// ������ �����
    /// </summary>
    property Width: Integer read FWidth write FWidth;
    /// <summary>
    /// �������� ��������� (��� ������������, ����������� � ������� ������)
    /// </summary>
    property &Platform: string read FPlatform write FPlatform;
    /// <summary>
    /// ��� �����������
    /// </summary>
    property &Type: TVkVideoType read FType write FType;
    ///������
    function ToAttachment: TAttachment;
    function ToStringId: string;
    destructor Destroy; override;
  end;

  TVkVideos = TVkEntityList<TVkVideo>;

  TVkVideoAlbum = class(TVkObject)
  private
    FCount: Integer;
    FImage: TArray<TVkVideoImage>;
    FOwner_id: Integer;
    FPrivacy: TVkPrivacy;
    FTitle: string;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FUpdated_time: TDateTime;
  public
    property Count: Integer read FCount write FCount;
    property Image: TArray<TVkVideoImage> read FImage write FImage;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property Privacy: TVkPrivacy read FPrivacy write FPrivacy;
    property Title: string read FTitle write FTitle;
    property UpdatedTime: TDateTime read FUpdated_time write FUpdated_time;
    destructor Destroy; override;
  end;

  TVkVideoAlbums = TVkEntityList<TVkVideoAlbum>;

implementation

uses
  VK.CommonUtils, System.SysUtils;

{TVkVideo}

destructor TVkVideo.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkVideoImage>(FImage);
  TArrayHelp.FreeArrayOfObject<TVkSize>(FFirstFrame);
  if Assigned(FFiles) then
    FFiles.Free;
  if Assigned(FLikes) then
    FLikes.Free;
  if Assigned(FReposts) then
    FReposts.Free;
  if Assigned(FTimeline_thumbs) then
    FTimeline_thumbs.Free;
  inherited;
end;

function TVkVideo.ToAttachment: TAttachment;
begin
  Result := TAttachment.Video(OwnerId, Id, AccessKey);
end;

function TVkVideo.ToStringId: string;
begin
  Result := Format('%d_%d', [OwnerId, Id]);
  if not AccessKey.IsEmpty then
    Result := Result + '_' + AccessKey;
end;

{TVkVideoAlbum}

destructor TVkVideoAlbum.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkVideoImage>(FImage);
  if Assigned(FPrivacy) then
    FPrivacy.Free;
  inherited;
end;

end.

