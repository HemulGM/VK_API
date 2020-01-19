unit VK.Entity.Media;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Photo, VK.Entity.Link,
  VK.Entity.AudioMessage, VK.Entity.Sticker, VK.Entity.Gift, VK.Entity.Market, VK.Entity.Doc,
  VK.Entity.Audio, VK.Entity.Video, VK.Entity.Graffiti;

type
  TVkAttachment = class;

  TVkComment = class;

  TVkPost = class;

  TVkPostCopyHistory = class;

  TVkCommentDeleted = class;

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
  public
    property&type: string read FType write FType;
    property link: TVkLink read FLink write FLink;
    property posted_photo: TVkPostedPhoto read FPosted_photo write FPosted_photo;
    property audio_message: TVkAudioMessage read FAudio_message write FAudio_message;
    property wall_reply: TVkComment read FWall_reply write FWall_reply;
    property wall: TvkPost read FWall write FWall;
    property sticker: TVkSticker read FSticker write FSticker;
    property gift: TVkGift read FGift write FGift;
    property market_album: TVkMarketAlbum read FMarket_album write FMarket_album;
    property market: TVkMarket read FMarket write FMarket;
    property doc: TVkDocument read FDoc write FDoc;
    property audio: TVkAudio read FAudio write FAudio;
    property video: TVkVideo read FVideo write FVideo;
    property photo: TVkPhoto read FPhoto write FPhoto;
    property graffiti: TVkGraffiti read FGraffiti write FGraffiti;
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
    property date: Extended read FDate write FDate;
    property from_id: Extended read FFrom_id write FFrom_id;
    property id: Extended read FId write FId;
    property post_id: Extended read FPost_id write FPost_id;
    property owner_id: Extended read FOwner_id write FOwner_id;
    property post_owner_id: Extended read FPost_owner_id write FPost_owner_id;
    property reply_to_comment: Extended read FReply_to_comment write FReply_to_comment;
    property reply_to_user: Extended read FReply_to_user write FReply_to_user;
    property text: string read FText write FText;
    property attachments: TVkAttachment read FAttachments write FAttachments;
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
    property attachments: TArray<TVkAttachment> read FAttachments write FAttachments;
    property date: Extended read FDate write FDate;
    property from_id: Extended read FFrom_id write FFrom_id;
    property id: Extended read FId write FId;
    property owner_id: Extended read FOwner_id write FOwner_id;
    property post_source: TVkPostSource read FPost_source write FPost_source;
    property post_type: string read FPost_type write FPost_type;
    property text: string read FText write FText;
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
    property id: Extended read FId write FId;
    property owner_id: Extended read FOwner_id write FOwner_id;
    property from_id: Extended read FFrom_id write FFrom_id;
    property created_by: Extended read FCreated_by write FCreated_by;
    property date: Extended read FDate write FDate;
    property text: string read FText write FText;
    property reply_owner_id: Extended read FReply_owner_id write FReply_owner_id;
    property reply_post_id: Extended read FReply_post_id write FReply_post_id;
    property friends_only: Extended read FFriends_only write FFriends_only;
    property comments: TVkCommentsInfo read FComments write FComments;
    property likes: TVkLikesInfo read FLikes write FLikes;
    property reposts: TVkRepostsInfo read FReposts write FReposts;
    property views: TVkViewsInfo read FViews write FViews;
    property post_type: string read FPost_type write FPost_type;
    property post_source: TVkPostSource read FPost_source write FPost_source;
    property attachments: TArray<TVkAttachment> read FAttachments write FAttachments;
    property geo: TVkGeo read FGeo write FGeo;
    property signer_id: Extended read FSigner_id write FSigner_id;
    property copy_history: TArray<TVkPostCopyHistory> read FCopy_history write FCopy_history;
    property can_pin: Extended read FCan_pin write FCan_pin;
    property can_delete: Extended read FCan_delete write FCan_delete;
    property can_edit: Extended read FCan_edit write FCan_edit;
    property is_pinned: Extended read FIs_pinned write FIs_pinned;
    property marked_as_ads: Extended read FMarked_as_ads write FMarked_as_ads;
    property is_favorite: Boolean read FIs_favorite write FIs_favorite;
    property postponed_id: Extended read FPostponed_id write FPostponed_id;
    property to_id: Extended read FTo_id write FTo_id;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPost;
  end;

  TVkCommentDeleted = class
  private
    FDeleter_id: Extended;
    FId: Extended;
    FOwner_id: Extended;
    FPost_id: Extended;
  public
    property deleter_id: Extended read FDeleter_id write FDeleter_id;
    property id: Extended read FId write FId;
    property owner_id: Extended read FOwner_id write FOwner_id;
    property post_id: Extended read FPost_id write FPost_id;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCommentDeleted;
  end;

  TOnWallReplyAction = procedure(Sender: TObject; GroupId: Integer; Comment: TVkComment; EventId: string) of object;

  TOnWallReplyDelete = procedure(Sender: TObject; GroupId: Integer; Comment: TVkCommentDeleted;
    EventId: string) of object;

implementation

{TVkAttachment}

constructor TVkAttachment.Create;
begin
  //Не создаем объекты вложений, их создаст JSON парсер
  //Создан будет только один объект
  inherited;
end;

destructor TVkAttachment.Destroy;
begin
  //Ну а тут, уничтожим, то что было создано
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
begin

  for Lcopy_historyItem in FCopy_history do
    Lcopy_historyItem.Free;

  FPost_source.Free;
  FComments.Free;
  FLikes.Free;
  FReposts.Free;
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

{ TWallCommentDeleted }

function TVkCommentDeleted.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCommentDeleted.FromJsonString(AJsonString: string): TVkCommentDeleted;
begin
  result := TJson.JsonToObject<TVkCommentDeleted>(AJsonString)
end;

end.

