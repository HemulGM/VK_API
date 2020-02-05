unit VK.Entity.Media;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Photo, VK.Entity.Link,
  VK.Entity.AudioMessage, VK.Entity.Sticker, VK.Entity.Gift, VK.Entity.Market, VK.Entity.Doc,
  VK.Entity.Audio, VK.Entity.Video, VK.Entity.Graffiti, VK.Entity.Note, VK.Entity.OldApp,
  VK.Entity.Poll, VK.Entity.Page, VK.Entity.Album, VK.Entity.PrettyCard, VK.Entity.Event;

type
  TVkAttachment = class;

  TVkComment = class;

  TVkPost = class;

  TVkPostCopyHistory = class;

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
    FMarket: TVkMarket;
    FDoc: TVkDocument;
    FAudio: TVkAudio;
    FVideo: TVkVideo;
    FPhoto: TVkPhoto;
    FGraffiti: TVkGraffiti;
    FNote: TVkNote;
    FApp: TVkOldApp;
    FPoll: TVkPoll;
    FPage: TVkPoll;
    FAlbum: TVkPhotoAlbum;
    FPretty_cards: TVkPrettyCards;
    FEvent: TVkEvent;
  public
    property&Type: string read FType write FType;
    property Link: TVkLink read FLink write FLink;
    property PostedPhoto: TVkPostedPhoto read FPosted_photo write FPosted_photo;
    property AudioMessage: TVkAudioMessage read FAudio_message write FAudio_message;
    property WallReply: TVkComment read FWall_reply write FWall_reply;
    property Wall: TvkPost read FWall write FWall;
    property Sticker: TVkSticker read FSticker write FSticker;
    property Gift: TVkGift read FGift write FGift;
    property MarketAlbum: TVkMarketAlbum read FMarket_album write FMarket_album;
    property Market: TVkMarket read FMarket write FMarket;
    property Doc: TVkDocument read FDoc write FDoc;
    property Audio: TVkAudio read FAudio write FAudio;
    property Video: TVkVideo read FVideo write FVideo;
    property Photo: TVkPhoto read FPhoto write FPhoto;
    property Graffiti: TVkGraffiti read FGraffiti write FGraffiti;
    property Note: TVkNote read FNote write FNote;
    property App: TVkOldApp read FApp write FApp;
    property Poll: TVkPoll read FPoll write FPoll;
    property Page: TVkPoll read FPage write FPage;
    property Album: TVkPhotoAlbum read FAlbum write FAlbum;
    property PrettyCards: TVkPrettyCards read FPretty_cards write FPretty_cards;
    property Event: TVkEvent read FEvent write FEvent;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkAttachment;
  end;

  TVkComment = class
  private
    FDate: Extended;
    FFrom_id: Extended;
    FId: Extended;
    FPost_id: Extended;
    FPost_owner_id: Extended;
    FReply_to_comment: Extended;
    FReply_to_user: Extended;
    FText: string;
    FOwner_id: Extended;
    FAttachments: TVkAttachment;
  public
    property Date: Extended read FDate write FDate;
    property FromId: Extended read FFrom_id write FFrom_id;
    property Id: Extended read FId write FId;
    property PostId: Extended read FPost_id write FPost_id;
    property OwnerId: Extended read FOwner_id write FOwner_id;
    property PostOwnerId: Extended read FPost_owner_id write FPost_owner_id;
    property ReplyToComment: Extended read FReply_to_comment write FReply_to_comment;
    property ReplyToUser: Extended read FReply_to_user write FReply_to_user;
    property Text: string read FText write FText;
    property Attachments: TVkAttachment read FAttachments write FAttachments;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkComment;
  end;

  TVkPostCopyHistory = class
  private
    FAttachments: TArray<TVkAttachment>;
    FDate: Extended;
    FFrom_id: Extended;
    FId: Extended;
    FOwner_id: Extended;
    FPost_source: TVkPostSource;
    FPost_type: string;
    FText: string;
  public
    property Attachments: TArray<TVkAttachment> read FAttachments write FAttachments;
    property Date: Extended read FDate write FDate;
    property FromId: Extended read FFrom_id write FFrom_id;
    property Id: Extended read FId write FId;
    property OwnerId: Extended read FOwner_id write FOwner_id;
    property PostSource: TVkPostSource read FPost_source write FPost_source;
    property PostType: string read FPost_type write FPost_type;
    property Text: string read FText write FText;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPostCopyHistory;
  end;

  TVkPost = class
  private
    FId: Extended;
    FOwner_id: Extended;
    FFrom_id: Extended;
    FCreated_by: Extended;
    FDate: Extended;
    FText: string;
    FReply_owner_id: Extended;
    FReply_post_id: Extended;
    FFriends_only: Extended;
    FComments: TVkCommentsInfo;
    FLikes: TVkLikesInfo;
    FReposts: TVkRepostsInfo;
    FViews: TVkViewsInfo;
    FPost_type: string;
    FPost_source: TVkPostSource;
    FAttachments: TArray<TVkAttachment>;
    FGeo: TVkGeo;
    FSigner_id: Extended;
    FCopy_history: TArray<TVkPostCopyHistory>;
    FCan_pin: Extended;
    FCan_delete: Extended;
    FCan_edit: Extended;
    FIs_pinned: Extended;
    FMarked_as_ads: Extended;
    FIs_favorite: Boolean;
    FPostponed_id: Extended;
    FTo_id: Extended;
  public
    property Id: Extended read FId write FId;
    property OwnerId: Extended read FOwner_id write FOwner_id;
    property FromId: Extended read FFrom_id write FFrom_id;
    property CreatedBy: Extended read FCreated_by write FCreated_by;
    property Date: Extended read FDate write FDate;
    property Text: string read FText write FText;
    property ReplyOwnerId: Extended read FReply_owner_id write FReply_owner_id;
    property ReplyPostId: Extended read FReply_post_id write FReply_post_id;
    property FriendsOnly: Extended read FFriends_only write FFriends_only;
    property Comments: TVkCommentsInfo read FComments write FComments;
    property Likes: TVkLikesInfo read FLikes write FLikes;
    property Reposts: TVkRepostsInfo read FReposts write FReposts;
    property Views: TVkViewsInfo read FViews write FViews;
    property PostType: string read FPost_type write FPost_type;
    property PostSource: TVkPostSource read FPost_source write FPost_source;
    property Attachments: TArray<TVkAttachment> read FAttachments write FAttachments;
    property Geo: TVkGeo read FGeo write FGeo;
    property SignerId: Extended read FSigner_id write FSigner_id;
    property CopyHistory: TArray<TVkPostCopyHistory> read FCopy_history write FCopy_history;
    property CanPin: Extended read FCan_pin write FCan_pin;
    property CanDelete: Extended read FCan_delete write FCan_delete;
    property CanEdit: Extended read FCan_edit write FCan_edit;
    property IsPinned: Extended read FIs_pinned write FIs_pinned;
    property MarkedAsAds: Extended read FMarked_as_ads write FMarked_as_ads;
    property IsFavorite: Boolean read FIs_favorite write FIs_favorite;
    property PostponedId: Extended read FPostponed_id write FPostponed_id;
    property ToId: Extended read FTo_id write FTo_id;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPost;
  end;

implementation

{TVkAttachment}

constructor TVkAttachment.Create;
begin
  //�� ������� ������� ��������, �� ������� JSON ������
  //������ ����� ������ ���� ������
  inherited;
end;

destructor TVkAttachment.Destroy;
begin
  //�� � ���, ���������, �� ��� ���� �������
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

{ TVkComment }

class function TVkComment.FromJsonString(AJsonString: string): TVkComment;
begin
  result := TJson.JsonToObject<TVkComment>(AJsonString);
end;

function TVkComment.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;


{TVkPostCopyHistory}

constructor TVkPostCopyHistory.Create;
begin
  inherited;
  FPost_source := TVkPostSource.Create();
end;

destructor TVkPostCopyHistory.Destroy;
var
  LattachmentsItem: TVkAttachment;
begin

  for LattachmentsItem in FAttachments do
    LattachmentsItem.Free;

  FPost_source.Free;
  inherited;
end;

function TVkPostCopyHistory.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPostCopyHistory.FromJsonString(AJsonString: string): TVkPostCopyHistory;
begin
  result := TJson.JsonToObject<TVkPostCopyHistory>(AJsonString)
end;

{TVkPost}

constructor TVkPost.Create;
begin
  inherited;
  FPost_source := TVkPostSource.Create();
  FComments := TVkCommentsInfo.Create();
  FLikes := TVkLikesInfo.Create();
  FReposts := TVkRepostsInfo.Create();
end;

destructor TVkPost.Destroy;
var
  Lcopy_historyItem: TVkPostCopyHistory;
  LAttachment: TVkAttachment;
begin

  for Lcopy_historyItem in FCopy_history do
    Lcopy_historyItem.Free;
  for LAttachment in FAttachments do
    LAttachment.Free;

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
  result := TJson.JsonToObject<TVkPost>(AJsonString)
end;

end.
