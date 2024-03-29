﻿unit VK.Entity.Newsfeed;

interface

uses
  Generics.Collections, REST.JsonReflect, Rest.Json, VK.Entity.Common,
  VK.Entity.Photo, VK.Entity.Link, VK.Entity.AudioMessage, VK.Entity.Sticker,
  VK.Entity.Gift, VK.Entity.Market, VK.Entity.Doc, VK.Entity.Audio,
  VK.Entity.Video, VK.Entity.Graffiti, VK.Entity.Note, VK.Entity.OldApp,
  VK.Entity.Poll, VK.Entity.Page, VK.Entity.Album, VK.Entity.PrettyCard,
  VK.Types, VK.Entity.Event, VK.Entity.Profile, VK.Entity.Group, VK.Entity.Call,
  VK.Entity.Media, VK.Entity.Info, VK.Entity.Common.List,
  VK.Entity.Common.ExtendedList, VK.Entity.Geo, VK.Wrap.Interceptors,
  VK.Entity.Donut;

type
  TVkNewsItem = class(TVkObject)
  private
    FOwner_id: TVkPeerId;
    FFrom_id: TVkPeerId;
    FCreated_by: TVkPeerId;
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FText: string;
    FReply_owner_id: TVkPeerId;
    FReply_post_id: Integer;
    FFriends_only: Boolean;
    FComments: TVkCommentsInfo;
    FLikes: TVkLikesInfo;
    FReposts: TVkRepostsInfo;
    FViews: TVkViewsInfo;
    FPost_type: string;
    FPost_source: TVkPostSource;
    FAttachments: TVkAttachmentArray;
    FGeo: TVkGeo;
    FSigner_id: TVkPeerId;
    FCopy_history: TArray<TVkPost>;
    FCan_pin: Boolean;
    FCan_delete: Boolean;
    FCan_edit: Boolean;
    FIs_pinned: Boolean;
    FMarked_as_ads: Boolean;
    FIs_favorite: Boolean;
    FPostponed_id: Integer;
    FTo_id: TVkPeerId;
    FPost_id: Integer;
    FCan_set_category: Boolean;
    [JsonReflectAttribute(ctString, rtString, TNewsfeedTypeInterceptor)]
    FType: TVkNewsfeedType;
    FSource_id: Integer;
    FCan_doubt_category: Boolean;
    FPhotos: TVkPhotos;
    FCopyright: TVkCopyright;
    FDonut: TVkDonut;
  public
    property Id;
    property Attachments: TVkAttachmentArray read FAttachments write FAttachments;
    property CanDelete: Boolean read FCan_delete write FCan_delete;
    property CanDoubtCategory: Boolean read FCan_doubt_category write FCan_doubt_category;
    property CanEdit: Boolean read FCan_edit write FCan_edit;
    property CanPin: Boolean read FCan_pin write FCan_pin;
    property CanSetCategory: Boolean read FCan_set_category write FCan_set_category;
    property Comments: TVkCommentsInfo read FComments write FComments;
    property CopyHistory: TArray<TVkPost> read FCopy_history write FCopy_history;
    property Copyright: TVkCopyright read FCopyright write FCopyright;
    property CreatedBy: TVkPeerId read FCreated_by write FCreated_by;
    property Date: TDateTime read FDate write FDate;
    property Donut: TVkDonut read FDonut write FDonut;
    property FriendsOnly: Boolean read FFriends_only write FFriends_only;
    property FromId: TVkPeerId read FFrom_id write FFrom_id;
    property Geo: TVkGeo read FGeo write FGeo;
    property IsFavorite: Boolean read FIs_favorite write FIs_favorite;
    property IsPinned: Boolean read FIs_pinned write FIs_pinned;
    property Likes: TVkLikesInfo read FLikes write FLikes;
    property MarkedAsAds: Boolean read FMarked_as_ads write FMarked_as_ads;
    property OwnerId: TVkPeerId read FOwner_id write FOwner_id;
    property Photos: TVkPhotos read FPhotos write FPhotos;
    property PostId: Integer read FPost_id write FPost_id;
    property PostponedId: Integer read FPostponed_id write FPostponed_id;
    property PostSource: TVkPostSource read FPost_source write FPost_source;
    property PostType: string read FPost_type write FPost_type;
    property ReplyOwnerId: TVkPeerId read FReply_owner_id write FReply_owner_id;
    property ReplyPostId: Integer read FReply_post_id write FReply_post_id;
    property Reposts: TVkRepostsInfo read FReposts write FReposts;
    property SignerId: TVkPeerId read FSigner_id write FSigner_id;
    property SourceId: Integer read FSource_id write FSource_id;
    property Text: string read FText write FText;
    property ToId: TVkPeerId read FTo_id write FTo_id;
    property Views: TVkViewsInfo read FViews write FViews;
    property &Type: TVkNewsfeedType read FType write FType;
    destructor Destroy; override;
  end;

  TVkNews = class(TVkEntityExtendedList<TVkNewsItem>)
  private
    FNext_from: string;
    FTotal_count: Integer;
    FNew_offset: Integer;
  public
    property NextFrom: string read FNext_from write FNext_from;
    property NewOffset: Integer read FNew_offset write FNew_offset;
    property TotalCount: Integer read FTotal_count write FTotal_count;
  end;

  TVkNewsfeedBannedIds = class(TVkEntity)
  private
    FGroups: TVkPeerIds;
    FMembers: TVkPeerIds;
  public
    property Groups: TVkPeerIds read FGroups write FGroups;
    property Members: TVkPeerIds read FMembers write FMembers;
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
    property &Type: string read FType write FType;
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
  VK.CommonUtils;

{ TVkNewsItem }

destructor TVkNewsItem.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkPost>(FCopy_history);
  TArrayHelp.FreeArrayOfObject<TVkAttachment>(FAttachments);
  if Assigned(FGeo) then
    FGeo.Free;
  if Assigned(FCopyright) then
    FCopyright.Free;
  if Assigned(FPhotos) then
    FPhotos.Free;
  if Assigned(FPost_source) then
    FPost_source.Free;
  if Assigned(FComments) then
    FComments.Free;
  if Assigned(FLikes) then
    FLikes.Free;
  if Assigned(FReposts) then
    FReposts.Free;
  if Assigned(FViews) then
    FViews.Free;
  if Assigned(FDonut) then
    FDonut.Free;
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

