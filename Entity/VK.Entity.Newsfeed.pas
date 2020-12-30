unit VK.Entity.Newsfeed;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Photo,
  VK.Entity.Link, VK.Entity.AudioMessage, VK.Entity.Sticker, VK.Entity.Gift,
  VK.Entity.Market, VK.Entity.Doc, VK.Entity.Audio, VK.Entity.Video,
  VK.Entity.Graffiti, VK.Entity.Note, VK.Entity.OldApp, VK.Entity.Poll,
  VK.Entity.Page, VK.Entity.Album, VK.Entity.PrettyCard, VK.Types,
  VK.Entity.Event, VK.Entity.Profile, VK.Entity.Group, VK.Entity.Call,
  VK.Entity.Media;

type
  TVkNewsCopyright = class
  private
    FLink: string;
    FName: string;
    FType: string;
  public
    property Link: string read FLink write FLink;
    property Name: string read FName write FName;
    property&Type: string read FType write FType;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkNewsCopyright;
  end;

  TVkNewsItem = class(TVkObject)
  private
    FOwner_id: Integer;
    FFrom_id: Integer;
    FCreated_by: Integer;
    FDate: Int64;
    FText: string;
    FReply_owner_id: Integer;
    FReply_post_id: Integer;
    FFriends_only: Integer;
    FComments: TVkCommentsInfo;
    FLikes: TVkLikesInfo;
    FReposts: TVkRepostsInfo;
    FViews: TVkViewsInfo;
    FPost_type: string;
    FPost_source: TVkPostSource;
    FAttachments: TArray<TVkAttachment>;
    FGeo: TVkGeo;
    FSigner_id: Integer;
    FCopy_history: TArray<TVkPost>;
    FCan_pin: Integer;
    FCan_delete: Integer;
    FCan_edit: Integer;
    FIs_pinned: Integer;
    FMarked_as_ads: Integer;
    FIs_favorite: Boolean;
    FPostponed_id: Integer;
    FTo_id: Integer;
    FPost_id: Integer;
    FCan_set_category: Boolean;
    FType: string;
    FSource_id: Integer;
    FCan_doubt_category: Boolean;
    FPhotos: TVkPhotos;
    FCopyright: TVkNewsCopyright;
    function GetDate: TDateTime;
    procedure SetDate(const Value: TDateTime);
  public
    property Attachments: TArray<TVkAttachment> read FAttachments write FAttachments;
    property CanDelete: Integer read FCan_delete write FCan_delete;
    property CanEdit: Integer read FCan_edit write FCan_edit;
    property CanPin: Integer read FCan_pin write FCan_pin;
    property Comments: TVkCommentsInfo read FComments write FComments;
    property CopyHistory: TArray<TVkPost> read FCopy_history write FCopy_history;
    property CreatedBy: Integer read FCreated_by write FCreated_by;
    property Date: TDateTime read GetDate write SetDate;
    property FriendsOnly: Integer read FFriends_only write FFriends_only;
    property FromId: Integer read FFrom_id write FFrom_id;
    property Geo: TVkGeo read FGeo write FGeo;
    property IsFavorite: Boolean read FIs_favorite write FIs_favorite;
    property IsPinned: Integer read FIs_pinned write FIs_pinned;
    property Likes: TVkLikesInfo read FLikes write FLikes;
    property MarkedAsAds: Integer read FMarked_as_ads write FMarked_as_ads;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property PostponedId: Integer read FPostponed_id write FPostponed_id;
    property PostSource: TVkPostSource read FPost_source write FPost_source;
    property PostType: string read FPost_type write FPost_type;
    property ReplyOwnerId: Integer read FReply_owner_id write FReply_owner_id;
    property ReplyPostId: Integer read FReply_post_id write FReply_post_id;
    property Reposts: TVkRepostsInfo read FReposts write FReposts;
    property SignerId: Integer read FSigner_id write FSigner_id;
    property Text: string read FText write FText;
    property ToId: Integer read FTo_id write FTo_id;
    property Views: TVkViewsInfo read FViews write FViews;
    property CanDoubtCategory: Boolean read FCan_doubt_category write FCan_doubt_category;
    property CanSetCategory: Boolean read FCan_set_category write FCan_set_category;
    property PostId: Integer read FPost_id write FPost_id;
    property SourceId: Integer read FSource_id write FSource_id;
    property&Type: string read FType write FType;
    property Photos: TVkPhotos read FPhotos write FPhotos;
    property Copyright: TVkNewsCopyright read FCopyright write FCopyright;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkNewsItem;
  end;

  TVkNews = class
  private
    FItems: TArray<TVkNewsItem>;
    FProfiles: TArray<TVkProfile>;
    FGroups: TArray<TVkGroup>;
    FNext_from: string;
    FCount: Integer;
    FTotal_count: Integer;
  public
    property Items: TArray<TVkNewsItem> read FItems write FItems;
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    property NextFrom: string read FNext_from write FNext_from;
    property Count: Integer read FCount write FCount;
    property TotalCount: Integer read FTotal_count write FTotal_count;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkNews;
  end;

  TVkNewsfeedBannedIds = class
  private
    FGroups: TArray<Integer>;
    FMembers: TArray<Integer>;
  public
    property Groups: TArray<Integer> read FGroups write FGroups;
    property Members: TArray<Integer> read FMembers write FMembers;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkNewsfeedBannedIds;
  end;

  TVkNewsfeedBanned = class
  private
    FGroups: TArray<TVkGroup>;
    FMembers: TArray<TVkProfile>;
  public
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    property Members: TArray<TVkProfile> read FMembers write FMembers;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkNewsfeedBanned;
  end;

  TVkNewsfeedList = class(TVkObject)
  private
    FNo_reposts: Integer;
    FSource_ids: TArray<Integer>;
    FTitle: string;
  public
    property NoReposts: Integer read FNo_reposts write FNo_reposts;
    property SourceIds: TArray<Integer> read FSource_ids write FSource_ids;
    property Title: string read FTitle write FTitle;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkNewsfeedList;
  end;

  TVkNewsfeedLists = class
  private
    FCount: Integer;
    FItems: TArray<TVkNewsfeedList>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkNewsfeedList> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkNewsfeedLists;
  end;

  TVkSuggestedItem = class(TVkObject)
  private
    FCan_access_closed: Boolean;
    FFirst_name: string;
    FIs_closed: Boolean;
    FLast_name: string;
    FPhoto_100: string;
    FPhoto_200: string;
    FPhoto_50: string;
    FScreen_name: string;
    FType: string;
    FIs_member: Integer;
    FName: string;
    FIs_advertiser: Integer;
    FIs_admin: Integer;
  public
    //common
    property&Type: string read FType write FType;
    property Photo50: string read FPhoto_50 write FPhoto_50;
    property IsClosed: Boolean read FIs_closed write FIs_closed;
    //profile
    property CanAccessClosed: Boolean read FCan_access_closed write FCan_access_closed;
    property FirstName: string read FFirst_name write FFirst_name;
    property LastName: string read FLast_name write FLast_name;
    property Photo100: string read FPhoto_100 write FPhoto_100;
    property Photo200: string read FPhoto_200 write FPhoto_200;
    property ScreenName: string read FScreen_name write FScreen_name;
    //page, group
    property IsAdmin: Integer read FIs_admin write FIs_admin;
    property IsAdvertiser: Integer read FIs_advertiser write FIs_advertiser;
    property IsMember: Integer read FIs_member write FIs_member;
    property Name: string read FName write FName;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkSuggestedItem;
  end;

  TVkSuggestedList = class
  private
    FCount: Integer;
    FItems: TArray<TVkSuggestedItem>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkSuggestedItem> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkSuggestedList;
  end;

implementation

uses
  System.DateUtils, VK.CommonUtils;

{ TVkNewsItem }

constructor TVkNewsItem.Create;
begin
  inherited;
  FPost_source := TVkPostSource.Create();
  FComments := TVkCommentsInfo.Create();
  FLikes := TVkLikesInfo.Create();
  FReposts := TVkRepostsInfo.Create();
  FGeo := TVkGeo.Create;
  FPhotos := TVkPhotos.Create;
end;

destructor TVkNewsItem.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkPost>(FCopy_history);
  TArrayHelp.FreeArrayOfObject<TVkAttachment>(FAttachments);
  FGeo.Free;
  FPhotos.Free;
  FPost_source.Free;
  FComments.Free;
  FLikes.Free;
  FReposts.Free;
  FViews.Free;
  inherited;
end;

function TVkNewsItem.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkNewsItem.FromJsonString(AJsonString: string): TVkNewsItem;
begin
  result := TJson.JsonToObject<TVkNewsItem>(AJsonString);
end;

function TVkNewsItem.GetDate: TDateTime;
begin
  Result := UnixToDateTime(FDate, False);
end;

procedure TVkNewsItem.SetDate(const Value: TDateTime);
begin
  FDate := DateTimeToUnix(Value, False);
end;

{ TVkNews }

destructor TVkNews.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkNewsItem>(FItems);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  inherited;
end;

class function TVkNews.FromJsonString(AJsonString: string): TVkNews;
begin
  result := TJson.JsonToObject<TVkNews>(AJsonString);
end;

function TVkNews.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkNewsCopyright }

class function TVkNewsCopyright.FromJsonString(AJsonString: string): TVkNewsCopyright;
begin
  result := TJson.JsonToObject<TVkNewsCopyright>(AJsonString);
end;

function TVkNewsCopyright.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkNewsfeedBannedIds }

class function TVkNewsfeedBannedIds.FromJsonString(AJsonString: string): TVkNewsfeedBannedIds;
begin
  result := TJson.JsonToObject<TVkNewsfeedBannedIds>(AJsonString);
end;

function TVkNewsfeedBannedIds.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkNewsfeedBanned }

destructor TVkNewsfeedBanned.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FMembers);
  inherited;
end;

class function TVkNewsfeedBanned.FromJsonString(AJsonString: string): TVkNewsfeedBanned;
begin
  result := TJson.JsonToObject<TVkNewsfeedBanned>(AJsonString);
end;

function TVkNewsfeedBanned.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkNewsfeedLists }

destructor TVkNewsfeedLists.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkNewsfeedList>(FItems);
  inherited;
end;

class function TVkNewsfeedLists.FromJsonString(AJsonString: string): TVkNewsfeedLists;
begin
  result := TJson.JsonToObject<TVkNewsfeedLists>(AJsonString);
end;

function TVkNewsfeedLists.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkNewsfeedList }

class function TVkNewsfeedList.FromJsonString(AJsonString: string): TVkNewsfeedList;
begin
  result := TJson.JsonToObject<TVkNewsfeedList>(AJsonString);
end;

function TVkNewsfeedList.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkSuggestedList }

destructor TVkSuggestedList.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkSuggestedItem>(FItems);
  inherited;
end;

class function TVkSuggestedList.FromJsonString(AJsonString: string): TVkSuggestedList;
begin
  result := TJson.JsonToObject<TVkSuggestedList>(AJsonString);
end;

function TVkSuggestedList.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkSuggestedItem }

class function TVkSuggestedItem.FromJsonString(AJsonString: string): TVkSuggestedItem;
begin
  result := TJson.JsonToObject<TVkSuggestedItem>(AJsonString);
end;

function TVkSuggestedItem.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

end.

