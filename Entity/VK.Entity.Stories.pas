unit VK.Entity.Stories;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Photo, VK.Entity.Profile, VK.Entity.Group, VK.Entity.Link,
  VK.Entity.App;

type
  TVkStoryReplies = class
  private
    FCount: Integer;
    FNew: Integer;
  public
    property Count: Integer read FCount write FCount;
    property New: Integer read FNew write FNew;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkStoryReplies;
  end;

  TVkStory = class
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
    FDate: Int64;
    FExpires_at: Int64;
    FId: Integer;
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
  public
    property AccessKey: string read FAccess_key write FAccess_key;
    property CanAsk: Boolean read FCan_ask write FCan_ask;
    property CanAskAnonymous: Boolean read FCan_ask_anonymous write FCan_ask_anonymous;
    property CanComment: Boolean read FCan_comment write FCan_comment;
    property CanHide: Boolean read FCan_hide write FCan_hide;
    property CanLike: Boolean read FCan_like write FCan_like;
    property CanReply: Boolean read FCan_reply write FCan_reply;
    property CanSee: Boolean read FCan_see write FCan_see;
    property CanShare: Boolean read FCan_share write FCan_share;
    property Date: Int64 read FDate write FDate;
    property ExpiresAt: Int64 read FExpires_at write FExpires_at;
    property Id: Integer read FId write FId;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property Photo: TVkPhoto read FPhoto write FPhoto;
    property Replies: TVkStoryReplies read FReplies write FReplies;
    property TrackCode: string read FTrack_code write FTrack_code;
    property&Type: string read FType write FType;
    //
    property IsOneTime: Boolean read FIs_one_time write FIs_one_time;
    property IsOwnerPinned: Boolean read FIs_owner_pinned write FIs_owner_pinned;
    property IsRestricted: Boolean read FIs_restricted write FIs_restricted;
    property Link: TVkLink read FLink write FLink;
    property MuteReply: Boolean read FMute_reply write FMute_reply;
    property NeedMute: Boolean read FNeed_mute write FNeed_mute;
    property NoSound: Boolean read FNo_sound write FNo_sound;
    property Seen: Integer read FSeen write FSeen;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkStory;
  end;

  TVkStoryItems = class
  private
    FCount: Integer;
    FItems: TArray<TVkStory>;
    FProfiles: TArray<TVkProfile>;
    FGroups: TArray<TVkGroup>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkStory> read FItems write FItems;
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkStoryItems;
  end;

  TVkStories = class
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
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkStories;
  end;

  TVkStoriesBlock = class
  private
    FCount: Integer;
    FItems: TArray<TVkStories>;
    FNeed_upload_screen: Boolean;
    FProfiles: TArray<TVkProfile>;
    FGroups: TArray<TVkGroup>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkStories> read FItems write FItems;
    property NeedUploadScreen: Boolean read FNeed_upload_screen write FNeed_upload_screen;
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkStoriesBlock;
  end;

  TVkStoriesBanned = class
  private
    FCount: Integer;
    FItems: TArray<Integer>;
    FProfiles: TArray<TVkProfile>;
    FGroups: TArray<TVkGroup>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<Integer> read FItems write FItems;
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkStoriesBanned;
  end;

implementation

uses
  VK.CommonUtils;

{TVkStoryReplies}

function TVkStoryReplies.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkStoryReplies.FromJsonString(AJsonString: string): TVkStoryReplies;
begin
  result := TJson.JsonToObject<TVkStoryReplies>(AJsonString)
end;

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
  inherited;
end;

function TVkStory.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkStory.FromJsonString(AJsonString: string): TVkStory;
begin
  result := TJson.JsonToObject<TVkStory>(AJsonString)
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

function TVkStories.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkStories.FromJsonString(AJsonString: string): TVkStories;
begin
  result := TJson.JsonToObject<TVkStories>(AJsonString)
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

{TVkStoriesItems}

destructor TVkStoriesBlock.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkStories>(FItems);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  inherited;
end;

function TVkStoriesBlock.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkStoriesBlock.FromJsonString(AJsonString: string): TVkStoriesBlock;
begin
  result := TJson.JsonToObject<TVkStoriesBlock>(AJsonString)
end;

{ TVkStoriesBanned }

destructor TVkStoriesBanned.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  inherited;
end;

class function TVkStoriesBanned.FromJsonString(AJsonString: string): TVkStoriesBanned;
begin
  result := TJson.JsonToObject<TVkStoriesBanned>(AJsonString)
end;

function TVkStoriesBanned.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkStoryItems }

destructor TVkStoryItems.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  inherited;
end;

class function TVkStoryItems.FromJsonString(AJsonString: string): TVkStoryItems;
begin
  result := TJson.JsonToObject<TVkStoryItems>(AJsonString)
end;

function TVkStoryItems.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

end.

