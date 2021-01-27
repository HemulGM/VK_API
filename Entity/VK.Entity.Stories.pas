unit VK.Entity.Stories;

interface

uses
  Generics.Collections, REST.JsonReflect, REST.Json.Interceptors, Rest.Json,
  VK.Entity.Common, VK.Entity.Photo, VK.Entity.Profile, VK.Entity.Video,
  VK.Entity.Group, VK.Entity.Link, VK.Entity.App, VK.Entity.Common.List,
  VK.Entity.Common.ExtendedList;

type
  TVkStoryReplies = class(TVkCounterEntity)
  private
    FNew: Integer;
  public
    property New: Integer read FNew write FNew;
  end;

  TVkStory = class(TVkObject)
  private
    FAccess_key: string;
    FCan_ask: Boolean;
    FCan_ask_anonymous: Boolean;
    FCan_comment: Boolean;
    FCan_hide: Boolean;
    FCan_like: Boolean;
    FCan_reply: Boolean;
    FCan_see: Boolean;
    FCan_share: Boolean;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FDate: TDateTime;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FExpires_at: TDateTime;
    FOwner_id: Integer;
    FPhoto: TVkPhoto;
    FReplies: TVkStoryReplies;
    FTrack_code: string;
    FType: string;
    FIs_restricted: Boolean;
    FNo_sound: Boolean;
    FIs_owner_pinned: Boolean;
    FLink: TVkLink;
    FIs_one_time: Boolean;
    FNeed_mute: Boolean;
    FSeen: Integer;
    FMute_reply: Boolean;
    FIs_ads: Boolean;
    FPreloading_enabled: Boolean;
    FVideo: TVkVideo;
  public
    property Id;
    property AccessKey: string read FAccess_key write FAccess_key;
    property CanAsk: Boolean read FCan_ask write FCan_ask;
    property CanAskAnonymous: Boolean read FCan_ask_anonymous write FCan_ask_anonymous;
    property CanComment: Boolean read FCan_comment write FCan_comment;
    property CanHide: Boolean read FCan_hide write FCan_hide;
    property CanLike: Boolean read FCan_like write FCan_like;
    property CanReply: Boolean read FCan_reply write FCan_reply;
    property CanSee: Boolean read FCan_see write FCan_see;
    property CanShare: Boolean read FCan_share write FCan_share;
    property Date: TDateTime read FDate write FDate;
    property ExpiresAt: TDateTime read FExpires_at write FExpires_at;
    property IsAds: Boolean read FIs_ads write FIs_ads;
    property IsOneTime: Boolean read FIs_one_time write FIs_one_time;
    property IsOwnerPinned: Boolean read FIs_owner_pinned write FIs_owner_pinned;
    property IsRestricted: Boolean read FIs_restricted write FIs_restricted;
    property Link: TVkLink read FLink write FLink;
    property MuteReply: Boolean read FMute_reply write FMute_reply;
    property NeedMute: Boolean read FNeed_mute write FNeed_mute;
    property NoSound: Boolean read FNo_sound write FNo_sound;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property PreloadingEnabled: Boolean read FPreloading_enabled write FPreloading_enabled;
    property Replies: TVkStoryReplies read FReplies write FReplies;
    property Seen: Integer read FSeen write FSeen;
    property TrackCode: string read FTrack_code write FTrack_code;
    property&Type: string read FType write FType;
    property Photo: TVkPhoto read FPhoto write FPhoto;
    property Video: TVkVideo read FVideo write FVideo;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkStoryItems = TVkEntityExtendedList<TVkStory>;

  TVkStories = class(TVkEntity)
  private
    FStories: TArray<TVkStory>;
    FGrouped: TArray<TVkStories>;
    FType: string;
    FApp: TVkApp;
    function GetIsCommunityGroupedStories: Boolean;
    function GetIsStories: Boolean;
    function GetIsAppGroupedStories: Boolean;
  public
    property Stories: TArray<TVkStory> read FStories write FStories;
    property Grouped: TArray<TVkStories> read FGrouped write FGrouped;
    /// <summary>
    /// stories, community_grouped_stories, app_grouped_stories
    /// </summary>
    property&Type: string read FType write FType;
    property IsStories: Boolean read GetIsStories;
    property IsCommunityGroupedStories: Boolean read GetIsCommunityGroupedStories;
    property IsAppGroupedStories: Boolean read GetIsAppGroupedStories;
    property App: TVkApp read FApp write FApp;
    destructor Destroy; override;
  end;

  TVkStoriesBlock = class(TVkEntityExtendedList<TVkStories>)
  private
    FNeed_upload_screen: Boolean;
  public
    property NeedUploadScreen: Boolean read FNeed_upload_screen write FNeed_upload_screen;
  end;

  TVkStoriesBanned = TVkEntityExtendedSimpleList<Integer>;

implementation

uses
  VK.CommonUtils;

{TVkStory}

constructor TVkStory.Create;
begin
  inherited;
  FPhoto := TVkPhoto.Create();
  FLink := TVkLink.Create();
  FReplies := TVkStoryReplies.Create();
end;

destructor TVkStory.Destroy;
begin
  FPhoto.Free;
  FLink.Free;
  FReplies.Free;
  if Assigned(FVideo) then
    FVideo.Free;
  inherited;
end;

{TVkStories}

destructor TVkStories.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkStory>(FStories);
  TArrayHelp.FreeArrayOfObject<TVkStories>(FGrouped);
  if Assigned(FApp) then
    FApp.Free;
  inherited;
end;

function TVkStories.GetIsAppGroupedStories: Boolean;
begin
  Result := &Type = 'app_grouped_stories';
end;

function TVkStories.GetIsCommunityGroupedStories: Boolean;
begin
  Result := &Type = 'community_grouped_stories';
end;

function TVkStories.GetIsStories: Boolean;
begin
  Result := &Type = 'stories';
end;

end.

