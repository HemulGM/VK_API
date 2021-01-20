unit VK.Entity.Newsfeed;

interface

uses
  Generics.Collections, REST.JsonReflect, REST.Json.Interceptors, Rest.Json, VK.Entity.Common, VK.Entity.Photo,
  VK.Entity.Link, VK.Entity.AudioMessage, VK.Entity.Sticker, VK.Entity.Gift, VK.Entity.Market, VK.Entity.Doc,
  VK.Entity.Audio, VK.Entity.Video, VK.Entity.Graffiti, VK.Entity.Note, VK.Entity.OldApp, VK.Entity.Poll, VK.Entity.Page,
  VK.Entity.Album, VK.Entity.PrettyCard, VK.Types, VK.Entity.Event, VK.Entity.Profile, VK.Entity.Group, VK.Entity.Call,
  VK.Entity.Media, VK.Entity.Common.List, VK.Entity.Common.ExtendedList;

type
  TVkNewsCopyright = class(TVkEntity)
  private
    FLink: string;
    FName: string;
    FType: string;
  public
    property Link: string read FLink write FLink;
    property Name: string read FName write FName;
    property&Type: string read FType write FType;
  end;

  TVkNewsItem = class(TVkObject)
  private
    FOwner_id: Integer;
    FFrom_id: Integer;
    FCreated_by: Integer;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FText: string;
    FReply_owner_id: Integer;
    FReply_post_id: Integer;
    FFriends_only: Boolean;
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
    FCan_pin: Boolean;
    FCan_delete: Boolean;
    FCan_edit: Boolean;
    FIs_pinned: Boolean;
    FMarked_as_ads: Boolean;
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
  public
    property Attachments: TArray<TVkAttachment> read FAttachments write FAttachments;
    property CanDelete: Boolean read FCan_delete write FCan_delete;
    property CanDoubtCategory: Boolean read FCan_doubt_category write FCan_doubt_category;
    property CanEdit: Boolean read FCan_edit write FCan_edit;
    property CanPin: Boolean read FCan_pin write FCan_pin;
    property CanSetCategory: Boolean read FCan_set_category write FCan_set_category;
    property Comments: TVkCommentsInfo read FComments write FComments;
    property CopyHistory: TArray<TVkPost> read FCopy_history write FCopy_history;
    property Copyright: TVkNewsCopyright read FCopyright write FCopyright;
    property CreatedBy: Integer read FCreated_by write FCreated_by;
    property Date: TDateTime read FDate write FDate;
    property FriendsOnly: Boolean read FFriends_only write FFriends_only;
    property FromId: Integer read FFrom_id write FFrom_id;
    property Geo: TVkGeo read FGeo write FGeo;
    property IsFavorite: Boolean read FIs_favorite write FIs_favorite;
    property IsPinned: Boolean read FIs_pinned write FIs_pinned;
    property Likes: TVkLikesInfo read FLikes write FLikes;
    property MarkedAsAds: Boolean read FMarked_as_ads write FMarked_as_ads;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property Photos: TVkPhotos read FPhotos write FPhotos;
    property PostId: Integer read FPost_id write FPost_id;
    property PostponedId: Integer read FPostponed_id write FPostponed_id;
    property PostSource: TVkPostSource read FPost_source write FPost_source;
    property PostType: string read FPost_type write FPost_type;
    property ReplyOwnerId: Integer read FReply_owner_id write FReply_owner_id;
    property ReplyPostId: Integer read FReply_post_id write FReply_post_id;
    property Reposts: TVkRepostsInfo read FReposts write FReposts;
    property SignerId: Integer read FSigner_id write FSigner_id;
    property SourceId: Integer read FSource_id write FSource_id;
    property Text: string read FText write FText;
    property ToId: Integer read FTo_id write FTo_id;
    property Views: TVkViewsInfo read FViews write FViews;
    property&Type: string read FType write FType;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkNews = class(TVkEntityExtendedList<TVkNewsItem>)
  private
    FNext_from: string;
    FTotal_count: Integer;
  public
    property NextFrom: string read FNext_from write FNext_from;
    property TotalCount: Integer read FTotal_count write FTotal_count;
  end;

  TVkNewsfeedBannedIds = class(TVkEntity)
  private
    FGroups: TArray<Integer>;
    FMembers: TArray<Integer>;
  public
    property Groups: TArray<Integer> read FGroups write FGroups;
    property Members: TArray<Integer> read FMembers write FMembers;
  end;

  TVkNewsfeedBanned = class(TVkEntity)
  private
    FGroups: TArray<TVkGroup>;
    FMembers: TArray<TVkProfile>;
  public
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    property Members: TArray<TVkProfile> read FMembers write FMembers;
    destructor Destroy; override;
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
  end;

  TVkNewsfeedLists = TVkEntityList<TVkNewsfeedList>;

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
    FIs_member: Boolean;
    FName: string;
    FIs_advertiser: Boolean;
    FIs_admin: Boolean;
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
    property IsAdmin: Boolean read FIs_admin write FIs_admin;
    property IsAdvertiser: Boolean read FIs_advertiser write FIs_advertiser;
    property IsMember: Boolean read FIs_member write FIs_member;
    property Name: string read FName write FName;
  end;

  TVkSuggestedList = TVkEntityList<TVkSuggestedItem>;

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

{ TVkNewsfeedBanned }

destructor TVkNewsfeedBanned.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FMembers);
  inherited;
end;

end.

