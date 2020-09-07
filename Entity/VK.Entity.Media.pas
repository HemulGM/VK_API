unit VK.Entity.Media;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Photo, VK.Entity.Link, VK.Entity.AudioMessage,
  VK.Entity.Sticker, VK.Entity.Gift, VK.Entity.Market, VK.Entity.Doc, VK.Entity.Audio, VK.Entity.Video,
  VK.Entity.Graffiti, VK.Entity.Note, VK.Entity.OldApp, VK.Entity.Poll, VK.Entity.Page, VK.Entity.Album,
  VK.Entity.PrettyCard, VK.Types, VK.Entity.Event, VK.Entity.Profile, VK.Entity.Group, VK.Entity.Call,
  VK.Entity.Market.Album;

type
  TVkAttachment = class;

  TVkComment = class;

  TVkPost = class;

  TVkAttachment = class
  private
    FType: string;
    FLink: TVkLink;
    FPosted_photo: TVkPostedPhoto;
    FAudio_message: TVkAudioMessage;
    FWall_reply: TVkComment;
    FWall: TVkPost;
    FSticker: TVkSticker;
    FGift: TVkGift;
    FMarket_album: TVkMarketAlbum;
    FMarket: TVkProduct;
    FDoc: TVkDocument;
    FAudio: TVkAudio;
    FVideo: TVkVideo;
    FPhoto: TVkPhoto;
    FGraffiti: TVkGraffiti;
    FNote: TVkNote;
    FApp: TVkOldApp;
    FPoll: TVkPoll;
    FPage: TVkPage;
    FAlbum: TVkPhotoAlbum;
    FPretty_cards: TVkPrettyCards;
    FEvent: TVkEvent;
    FCall: TVkCall;
    function GetType: TVkAttachmentType;
    procedure SetType(const Value: TVkAttachmentType);
  public
    property&Type: TVkAttachmentType read GetType write SetType;
    property Link: TVkLink read FLink write FLink;
    property PostedPhoto: TVkPostedPhoto read FPosted_photo write FPosted_photo;
    property AudioMessage: TVkAudioMessage read FAudio_message write FAudio_message;
    property WallReply: TVkComment read FWall_reply write FWall_reply;
    property Wall: TVkPost read FWall write FWall;
    property Call: TVkCall read FCall write FCall;
    property Sticker: TVkSticker read FSticker write FSticker;
    property Gift: TVkGift read FGift write FGift;
    property MarketAlbum: TVkMarketAlbum read FMarket_album write FMarket_album;
    property Market: TVkProduct read FMarket write FMarket;
    property Doc: TVkDocument read FDoc write FDoc;
    property Audio: TVkAudio read FAudio write FAudio;
    property Video: TVkVideo read FVideo write FVideo;
    property Photo: TVkPhoto read FPhoto write FPhoto;
    property Graffiti: TVkGraffiti read FGraffiti write FGraffiti;
    property Note: TVkNote read FNote write FNote;
    property App: TVkOldApp read FApp write FApp;
    property Poll: TVkPoll read FPoll write FPoll;
    property Page: TVkPage read FPage write FPage;
    property Album: TVkPhotoAlbum read FAlbum write FAlbum;
    //property PhotosList: TVkPhotosList read FPhotosList write FPhotosList; -- я хз че это и где найти структуру)
    property PrettyCards: TVkPrettyCards read FPretty_cards write FPretty_cards;
    property Event: TVkEvent read FEvent write FEvent;
    constructor Create;
    destructor Destroy; override;
    function GetPreviewUrl: string;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkAttachment;
  end;

  TVkAttachmentHistoryItem = class
  private
    FAttachment: TVkAttachment;
    FMessage_id: Integer;
    FFrom_id: Integer;
  public
    property Attachment: TVkAttachment read FAttachment write FAttachment;
    property MessageId: Integer read FMessage_id write FMessage_id;
    property FromId: Integer read FFrom_id write FFrom_id;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkAttachmentHistoryItem;
  end;

  TVkAttachmentHistory = class
  private
    FItems: TArray<TVkAttachmentHistoryItem>;
    FNext_from: string;
    FProfiles: TArray<TVkProfile>;
    FGroups: TArray<TVkGroup>;
  public
    property Items: TArray<TVkAttachmentHistoryItem> read FItems write FItems;
    property NextFrom: string read FNext_from write FNext_from;
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkAttachmentHistory;
  end;

  TVkAttachments = class
  private
    FItems: TArray<TVkAttachment>;
    FCount: Integer;
  public
    property Items: TArray<TVkAttachment> read FItems write FItems;
    property Count: Integer read FCount write FCount;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkAttachments;
  end;

  TVkCommentThread = class
  private
    FItems: TArray<TVkComment>;
    FCount: Integer;
    FCan_post: Boolean;
    FShow_reply_button: Boolean;
    FGroups_can_post: Boolean;
  public
    property Items: TArray<TVkComment> read FItems write FItems;
    property Count: Integer read FCount write FCount;
    property CanPost: Boolean read FCan_post write FCan_post;
    property ShowReplyButton: Boolean read FShow_reply_button write FShow_reply_button;
    property GroupsCanPost: Boolean read FGroups_can_post write FGroups_can_post;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCommentThread;
  end;

  TVkComment = class
  private
    FDate: Int64;
    FFrom_id: Integer;
    FId: Integer;
    FPost_id: Integer;
    FPost_owner_id: Integer;
    FReply_to_comment: Integer;
    FReply_to_user: Integer;
    FText: string;
    FOwner_id: Integer;
    FAttachments: TVkAttachments;
    FDeleted: Boolean;
    FParents_stack: TArray<Integer>;
    FLikes: TVkLikesInfo;
    FThread: TVkCommentThread;
    FPid: Integer;
  public
    property Date: Int64 read FDate write FDate;
    property FromId: Integer read FFrom_id write FFrom_id;
    property Id: Integer read FId write FId;
    /// <summary>
    ///  Идентификатор фотографии, к которой был оставлен комментарий
    /// </summary>
    property Pid: Integer read FPid write FPid;
    property PostId: Integer read FPost_id write FPost_id;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property PostOwnerId: Integer read FPost_owner_id write FPost_owner_id;
    property ReplyToComment: Integer read FReply_to_comment write FReply_to_comment;
    property ReplyToUser: Integer read FReply_to_user write FReply_to_user;
    property Text: string read FText write FText;
    property Likes: TVkLikesInfo read FLikes write FLikes;
    property ParentsStack: TArray<Integer> read FParents_stack write FParents_stack;
    property Deleted: Boolean read FDeleted write FDeleted;
    property Attachments: TVkAttachments read FAttachments write FAttachments;
    property Thread: TVkCommentThread read FThread write FThread;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkComment;
  end;

  TVkComments = class
  private
    FCount: Integer;
    FItems: TArray<TVkComment>;
    FProfiles: TArray<TVkProfile>;
    FGroups: TArray<TVkGroup>;
    FCurrent_level_count: Integer;
    FCan_post: Boolean;
    FShow_reply_button: Boolean;
    FGroups_can_post: Boolean;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkComment> read FItems write FItems;
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    //
    property CurrentLevelCount: Integer read FCurrent_level_count write FCurrent_level_count;
    property CanPost: Boolean read FCan_post write FCan_post;
    property ShowReplyButton: Boolean read FShow_reply_button write FShow_reply_button;
    property GroupsCanPost: Boolean read FGroups_can_post write FGroups_can_post;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkComments;
  end;

  TVkPost = class
  private
    FId: Integer;
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
    property Id: Integer read FId write FId;
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
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPost;
  end;

  TVkPosts = class
  private
    FCount: Integer;
    FItems: TArray<TVkPost>;
    FProfiles: TArray<TVkProfile>;
    FGroups: TArray<TVkGroup>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkPost> read FItems write FItems;
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPosts;
  end;

  TVkRepostInfo = class
  private
    FLikes_count: Integer;
    FPost_id: Integer;
    FReposts_count: Integer;
    FSuccess: Boolean;
  public
    property LikesCount: Integer read FLikes_count write FLikes_count;
    property PostId: Integer read FPost_id write FPost_id;
    property RepostsCount: Integer read FReposts_count write FReposts_count;
    property Success: Boolean read FSuccess write FSuccess;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkRepostInfo;
  end;

implementation

uses
  System.DateUtils, VK.CommonUtils;

{TVkAttachment}

constructor TVkAttachment.Create;
begin
  inherited;
end;

destructor TVkAttachment.Destroy;
begin
  if Assigned(FLink) then
    FLink.Free;
  if Assigned(FPosted_photo) then
    FPosted_photo.Free;
  if Assigned(FAudio_message) then
    FAudio_message.Free;
  if Assigned(FAudio) then
    FAudio.Free;
  if Assigned(FWall_reply) then
    FWall_reply.Free;
  if Assigned(FWall) then
    FWall.Free;
  if Assigned(FCall) then
    FCall.Free;
  if Assigned(FSticker) then
    FSticker.Free;
  if Assigned(FGift) then
    FGift.Free;
  if Assigned(FMarket_album) then
    FMarket_album.Free;
  if Assigned(FMarket) then
    FMarket.Free;
  if Assigned(FDoc) then
    FDoc.Free;
  if Assigned(FVideo) then
    FVideo.Free;
  if Assigned(FPhoto) then
    FPhoto.Free;
  if Assigned(FGraffiti) then
    FGraffiti.Free;
  if Assigned(FNote) then
    FNote.Free;
  if Assigned(FApp) then
    FApp.Free;
  if Assigned(FPoll) then
    FPoll.Free;
  if Assigned(FPage) then
    FPage.Free;
  if Assigned(FAlbum) then
    FAlbum.Free;
  if Assigned(FPretty_cards) then
    FPretty_cards.Free;
  if Assigned(FEvent) then
    FEvent.Free;

  inherited;
end;

function TVkAttachment.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkAttachment.FromJsonString(AJsonString: string): TVkAttachment;
begin
  result := TJson.JsonToObject<TVkAttachment>(AJsonString);
end;

function TVkAttachment.GetType: TVkAttachmentType;
begin
  Result := TVkAttachmentType.Create(FType);
end;

function TVkAttachment.GetPreviewUrl: string;
begin
  case&Type of
    atPhoto:
      Exit(Self.FPhoto.Sizes[4].Url);
    atVideo:
      Exit(Self.FVideo.Image[4].Url);
    atAudio:
      Exit('');
    atDoc:
      Exit('');
    atLink:
      Exit('');
    atMarket:
      Exit('');
    atMarketAlbum:
      Exit('');
    atWall:
      Exit('');
    atWallReply:
      Exit('');
    atSticker:
      Exit(Self.FSticker.Images[1].Url);
    atGift:
      Exit('');
  else
    Result := '';
  end;
end;

procedure TVkAttachment.SetType(const Value: TVkAttachmentType);
begin
  FType := Value.ToString;
end;

{ TVkComment }

constructor TVkComment.Create;
begin
  FAttachments := TVkAttachments.Create;
  FLikes := TVkLikesInfo.Create;
  FThread := TVkCommentThread.Create;
end;

destructor TVkComment.Destroy;
begin
  FThread.Free;
  FAttachments.Free;
  FLikes.Free;
  inherited;
end;

class function TVkComment.FromJsonString(AJsonString: string): TVkComment;
begin
  result := TJson.JsonToObject<TVkComment>(AJsonString);
end;

function TVkComment.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{TVkPost}

constructor TVkPost.Create;
begin
  inherited;
  FPost_source := TVkPostSource.Create();
  FComments := TVkCommentsInfo.Create();
  FLikes := TVkLikesInfo.Create();
  FReposts := TVkRepostsInfo.Create();
  FGeo := TVkGeo.Create;
end;

destructor TVkPost.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkPost>(FCopy_history);
  TArrayHelp.FreeArrayOfObject<TVkAttachment>(FAttachments);
  FGeo.Free;
  FPost_source.Free;
  FComments.Free;
  FLikes.Free;
  FReposts.Free;
  FViews.Free;
  inherited;
end;

function TVkPost.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPost.FromJsonString(AJsonString: string): TVkPost;
begin
  result := TJson.JsonToObject<TVkPost>(AJsonString);
end;

function TVkPost.GetDate: TDateTime;
begin
  Result := UnixToDateTime(FDate, False);
end;

procedure TVkPost.SetDate(const Value: TDateTime);
begin
  FDate := DateTimeToUnix(Value, False);
end;

{ TVkPosts }

destructor TVkPosts.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkPost>(FItems);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  inherited;
end;

class function TVkPosts.FromJsonString(AJsonString: string): TVkPosts;
begin
  result := TJson.JsonToObject<TVkPosts>(AJsonString);
end;

function TVkPosts.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkAttachmentHistory }

destructor TVkAttachmentHistory.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkAttachmentHistoryItem>(FItems);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  inherited;
end;

class function TVkAttachmentHistory.FromJsonString(AJsonString: string): TVkAttachmentHistory;
begin
  result := TJson.JsonToObject<TVkAttachmentHistory>(AJsonString);
end;

function TVkAttachmentHistory.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkAttachmentHistoryItem }

constructor TVkAttachmentHistoryItem.Create;
begin
  FAttachment := TVkAttachment.Create;
end;

destructor TVkAttachmentHistoryItem.Destroy;
begin
  FAttachment.Free;
  inherited;
end;

class function TVkAttachmentHistoryItem.FromJsonString(AJsonString: string): TVkAttachmentHistoryItem;
begin
  result := TJson.JsonToObject<TVkAttachmentHistoryItem>(AJsonString);
end;

function TVkAttachmentHistoryItem.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkComments }

destructor TVkComments.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkComment>(FItems);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  inherited;
end;

class function TVkComments.FromJsonString(AJsonString: string): TVkComments;
begin
  result := TJson.JsonToObject<TVkComments>(AJsonString);
end;

function TVkComments.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkAttachments }

destructor TVkAttachments.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkAttachment>(FItems);
  inherited;
end;

class function TVkAttachments.FromJsonString(AJsonString: string): TVkAttachments;
begin
  result := TJson.JsonToObject<TVkAttachments>(AJsonString);
end;

function TVkAttachments.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkCommentThread }

destructor TVkCommentThread.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkComment>(FItems);
  inherited;
end;

class function TVkCommentThread.FromJsonString(AJsonString: string): TVkCommentThread;
begin
  result := TJson.JsonToObject<TVkCommentThread>(AJsonString);
end;

function TVkCommentThread.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkRepostInfo }

class function TVkRepostInfo.FromJsonString(AJsonString: string): TVkRepostInfo;
begin
  result := TJson.JsonToObject<TVkRepostInfo>(AJsonString);
end;

function TVkRepostInfo.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

end.

