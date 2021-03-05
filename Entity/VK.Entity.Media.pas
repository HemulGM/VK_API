unit VK.Entity.Media;

interface

uses
  Generics.Collections, REST.Json.Interceptors, REST.JsonReflect, Rest.Json, VK.Entity.Common, VK.Entity.Photo,
  VK.Entity.Link, VK.Entity.AudioMessage, VK.Entity.Sticker, VK.Entity.Gift, VK.Entity.Market, VK.Entity.Doc,
  VK.Entity.Audio, VK.Entity.Video, VK.Entity.Graffiti, VK.Entity.Note, VK.Entity.OldApp, VK.Entity.Poll, VK.Entity.Page,
  VK.Entity.Album, VK.Entity.PrettyCard, VK.Types, VK.Entity.Event, VK.Entity.Profile, VK.Entity.Group, VK.Entity.Call,
  VK.Entity.Market.Album, VK.Entity.Info, VK.Entity.Common.List, VK.Entity.Common.ExtendedList, VK.Entity.Donut,
  VK.Wrap.Interceptors, VK.Entity.MoneyTransfer, VK.Entity.Geo;

type
  TVkAttachment = class;

  TVkComment = class;

  TVkPost = class;

  TVkCommentThread = class;

  /// <summary>
  /// ������ PostSource, ����������� ������ ���������� ������ �� �����
  /// </summary>
  TVkPostSource = class(TVkEntity)
  private
    FData: string;
    FPlatform: string;
    [JsonReflectAttribute(ctString, rtString, TPostSourceTypeInterceptor)]
    FType: TVkPostSourceType;
    FUrl: string;
  public
    /// <summary>
    /// ��� �������� (������ ��� Type = VK ��� Widget)
    /// ��������� ��������:
    /// profile_activity � ��������� ������� ��� ������ ������������ (��� Type = VK);
    /// profile_photo � ��������� ���������� ���������� ������������ (��� Type = VK);
    /// comments � ������ ������������ (��� Type = Widget);
    /// like � ������ ���� ��������� (��� Type = Widget);
    /// poll � ������ ������� (��� Type = Widget);
    /// </summary>
    property Data: string read FData write FData;
    /// <summary>
    /// �������� ���������, ���� ��� �������� (android; iphone; wphone)
    /// </summary>
    property&Platform: string read FPlatform write FPlatform;
    /// <summary>
    /// ��� ���������
    /// </summary>
    property&Type: TVkPostSourceType read FType write FType;
    /// <summary>
    /// URL �������, � �������� ���� ������������ ������
    /// </summary>
    property Url: string read FUrl write FUrl;
  end;

  TVkAttachment = class(TVkEntity)
  private
    [JsonReflectAttribute(ctString, rtString, TAttachmentTypeInterceptor)]
    FType: TVkAttachmentType;
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
    FMoney_transfer: TVkMoneyTransfer;
  public
    property&Type: TVkAttachmentType read FType write FType;
    /// <summary>
    /// ������
    /// </summary>
    property Link: TVkLink read FLink write FLink;
    /// <summary>
    /// ����������, ����������� �������� (��� ���������� ��� ��������. �� ����� ���� ��������� ���� ��� �������, ��������� ������ 2013 ����)
    /// </summary>
    property PostedPhoto: TVkPostedPhoto read FPosted_photo write FPosted_photo;
    /// <summary>
    /// ��������������
    /// </summary>
    property AudioMessage: TVkAudioMessage read FAudio_message write FAudio_message;
    /// <summary>
    /// ����������� �� �����
    /// </summary>
    property WallReply: TVkComment read FWall_reply write FWall_reply;
    /// <summary>
    /// ������ �� �����
    /// </summary>
    property Wall: TVkPost read FWall write FWall;
    /// <summary>
    /// ������
    /// </summary>
    property Call: TVkCall read FCall write FCall;
    /// <summary>
    /// ������
    /// </summary>
    property Sticker: TVkSticker read FSticker write FSticker;
    /// <summary>
    /// �������
    /// </summary>
    property Gift: TVkGift read FGift write FGift;
    /// <summary>
    /// �������� �������
    /// </summary>
    property MarketAlbum: TVkMarketAlbum read FMarket_album write FMarket_album;
    /// <summary>
    /// �����
    /// </summary>
    property Market: TVkProduct read FMarket write FMarket;
    /// <summary>
    /// ��������
    /// </summary>
    property Doc: TVkDocument read FDoc write FDoc;
    /// <summary>
    /// �����������
    /// </summary>
    property Audio: TVkAudio read FAudio write FAudio;
    /// <summary>
    /// �����������
    /// </summary>
    property Video: TVkVideo read FVideo write FVideo;
    /// <summary>
    /// ����������
    /// </summary>
    property Photo: TVkPhoto read FPhoto write FPhoto;
    /// <summary>
    /// �������� (��� ���������� ��� ��������. �� ����� ���� ��������� ���� ��� �������, ��������� ������ 2013 ����. ��� ����� ����� ������� �������� ������������ � ���� �������� � ����� photo.)
    /// </summary>
    property Graffiti: TVkGraffiti read FGraffiti write FGraffiti;
    /// <summary>
    /// �������
    /// </summary>
    property Note: TVkNote read FNote write FNote;
    /// <summary>
    /// ������� ���������� (��� ���������� ��� ��������. �� ����� ���� ��������� ���� ��� �������, ��������� ������ 2013 ����.)
    /// </summary>
    property App: TVkOldApp read FApp write FApp;
    /// <summary>
    /// �����
    /// </summary>
    property Poll: TVkPoll read FPoll write FPoll;
    /// <summary>
    /// ����-��������
    /// </summary>
    property Page: TVkPage read FPage write FPage;
    /// <summary>
    /// �������� �������
    /// </summary>
    property MoneyTransfer: TVkMoneyTransfer read FMoney_transfer write FMoney_transfer;
    /// <summary>
    /// ������ � ������������
    /// </summary>
    property Album: TVkPhotoAlbum read FAlbum write FAlbum;
    // /// <summary>
    // /// ������ ����������
    // /// </summary>
    //property PhotosList: TVkPhotosList read FPhotosList write FPhotosList; -- � �� �� ��� � ��� ����� ���������)
    /// <summary>
    /// ��������
    /// </summary>
    property PrettyCards: TVkPrettyCards read FPretty_cards write FPretty_cards;
    /// <summary>
    /// �������
    /// </summary>
    property Event: TVkEvent read FEvent write FEvent;
    destructor Destroy; override;
    function GetPreviewUrl: string;
  end;

  TVkAttachmentHistoryItem = class(TVkEntity)
  private
    FAttachment: TVkAttachment;
    FMessage_id: Integer;
    FFrom_id: Integer;
  public
    property Attachment: TVkAttachment read FAttachment write FAttachment;
    property MessageId: Integer read FMessage_id write FMessage_id;
    property FromId: Integer read FFrom_id write FFrom_id;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkAttachmentHistory = class(TVkEntityExtendedList<TVkAttachmentHistoryItem>)
  private
    FNext_from: string;
  public
    property NextFrom: string read FNext_from write FNext_from;
  end;

  TVkAttachments = TVkEntityList<TVkAttachment>;

  /// <summary>
  /// ������, ����������� ����������� � ������
  /// </summary>
  TVkComment = class(TVkObject, IAttachment)
  private
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FFrom_id: Integer;
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
    FAccess_key: string;
    FDonut: TVkDonutInfo;
  public
    /// <summary>
    /// ������������� �����������
    /// </summary>
    property Id;
    /// <summary>
    /// ���� �������
    /// </summary>
    property AccessKey: string read FAccess_key write FAccess_key;
    /// <summary>
    /// ���� �������� �����������
    /// </summary>
    property Date: TDateTime read FDate write FDate;
    /// <summary>
    /// ���������� � VK Donut
    /// </summary>
    property Donut: TVkDonutInfo read FDonut write FDonut;
    /// <summary>
    /// ������������� ������ �����������
    /// </summary>
    property FromId: Integer read FFrom_id write FFrom_id;
    /// <summary>
    ///  ������������� ����������, � ������� ��� �������� �����������
    /// </summary>
    property PhotoId: Integer read FPid write FPid;
    /// <summary>
    /// ������������� ������, � ������� �������� ����������
    /// </summary>
    property PostId: Integer read FPost_id write FPost_id;
    /// <summary>
    /// ������������� ��������� �����, �� ������� �������� �����������
    /// </summary>
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property PostOwnerId: Integer read FPost_owner_id write FPost_owner_id;
    /// <summary>
    /// ������������� �����������, � ����� �� ������� �������� ������� (���� ���������)
    /// </summary>
    property ReplyToComment: Integer read FReply_to_comment write FReply_to_comment;
    /// <summary>
    /// ������������� ������������ ��� ����������, � ����� �������� �������� ������� ����������� (���� ���������)
    /// </summary>
    property ReplyToUser: Integer read FReply_to_user write FReply_to_user;
    /// <summary>
    /// ����� �����������
    /// </summary>
    property Text: string read FText write FText;
    property Likes: TVkLikesInfo read FLikes write FLikes;
    /// <summary>
    /// ������ ��������������� ������������ ������������
    /// </summary>
    property ParentsStack: TArray<Integer> read FParents_stack write FParents_stack;
    property Deleted: Boolean read FDeleted write FDeleted;
    /// <summary>
    /// ������������� ����������� (����������, ������ � �.�.)
    /// </summary>
    property Attachments: TVkAttachments read FAttachments write FAttachments;
    /// <summary>
    /// ���������� � ��������� ����� ������������
    /// </summary>
    property Thread: TVkCommentThread read FThread write FThread;
    constructor Create; override;
    destructor Destroy; override;
    function ToAttachment: TAttachment;
  end;

  TVkCommentThread = class(TVkEntityList<TVkComment>)
  private
    FCan_post: Boolean;
    FShow_reply_button: Boolean;
    FGroups_can_post: Boolean;
  public
    /// <summary>
    /// ������ �������� ������������ � ������ (������ ��� ������ wall.getComments)
    /// </summary>
    property Items;
    /// <summary>
    /// ���������� ������������ � �����
    /// </summary>
    property Count;
    /// <summary>
    /// ����� �� ������� ������������ ��������� ����������� � ���� �����
    /// </summary>
    property CanPost: Boolean read FCan_post write FCan_post;
    /// <summary>
    /// ����� �� ���������� ������ ���������� � �����
    /// </summary>
    property ShowReplyButton: Boolean read FShow_reply_button write FShow_reply_button;
    /// <summary>
    /// ����� �� ���������� ��������� ����������� � �����
    /// </summary>
    property GroupsCanPost: Boolean read FGroups_can_post write FGroups_can_post;
  end;

  TVkComments = class(TVkEntityExtendedList<TVkComment>)
  private
    FCurrent_level_count: Integer;
    FCan_post: Boolean;
    FShow_reply_button: Boolean;
    FGroups_can_post: Boolean;
  public
    property CurrentLevelCount: Integer read FCurrent_level_count write FCurrent_level_count;
    property CanPost: Boolean read FCan_post write FCan_post;
    property ShowReplyButton: Boolean read FShow_reply_button write FShow_reply_button;
    property GroupsCanPost: Boolean read FGroups_can_post write FGroups_can_post;
  end;

  /// <summary>
  /// ������, ����������� ������ �� ����� ������������ ��� ����������
  /// </summary>
  TVkPost = class(TVkObject, IAttachment)
  private
    FOwner_id: Integer;
    FFrom_id: Integer;
    FCreated_by: Integer;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FText: string;
    FReply_owner_id: Integer;
    FReply_post_id: Integer;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
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
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_pin: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_delete: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_edit: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FIs_pinned: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FMarked_as_ads: Boolean;
    FIs_favorite: Boolean;
    FPostponed_id: Integer;
    FTo_id: Integer;
    FAccess_key: string;
    FCopyright: TVkCopyright;
    FDonut: TVkDonut;
    [JsonReflectAttribute(ctString, rtString, TPostTypeInterceptor)]
    FType: TVkPostType;
    FCan_archive: Boolean;
    FIs_archived: Boolean;
    FShort_text_rate: Extended;
  public
    /// <summary>
    /// ������������� ������
    /// </summary>
    property Id;
    /// <summary>
    /// ���� �������
    /// </summary>
    property AccessKey: string read FAccess_key write FAccess_key;
    /// <summary>
    /// ������������� ������ (����������, ������ � �.�.)
    /// </summary>
    property Attachments: TArray<TVkAttachment> read FAttachments write FAttachments;
    /// <summary>
    /// ���������� � ���, ����� �� ������ �������� � �����
    /// </summary>
    property CanArchive: Boolean read FCan_archive write FCan_archive;
    /// <summary>
    /// ���������� � ���, ����� �� ������� ������������ ������� ������
    /// </summary>
    property CanDelete: Boolean read FCan_delete write FCan_delete;
    /// <summary>
    /// ���������� � ���, ����� �� ������� ������������ ������������� ������
    /// </summary>
    property CanEdit: Boolean read FCan_edit write FCan_edit;
    /// <summary>
    /// ���������� � ���, ����� �� ������� ������������ ��������� ������
    /// </summary>
    property CanPin: Boolean read FCan_pin write FCan_pin;
    /// <summary>
    /// ���������� � ������������ � ������
    /// </summary>
    property Comments: TVkCommentsInfo read FComments write FComments;
    /// <summary>
    /// ������, ���������� ������� �������� ��� ������. ������������ ������ � ��� ������, ���� ������ �������� ��������. ������ �� �������� �������, � ���� �������, �������� ��������-������� ������������ �������.
    /// </summary>
    property CopyHistory: TArray<TVkPost> read FCopy_history write FCopy_history;
    /// <summary>
    /// �������� ���������
    /// </summary>
    property Copyright: TVkCopyright read FCopyright write FCopyright;
    /// <summary>
    /// ������������� ��������������, ������� ����������� ������ (������������ ������ ��� ��������� ��� ������� � ������ ������� ��������������). ������������ � �������, �������������� ����� 24 ����� �����.
    /// </summary>
    property CreatedBy: Integer read FCreated_by write FCreated_by;
    /// <summary>
    /// ����� ���������� ������
    /// </summary>
    property Date: TDateTime read FDate write FDate;
    /// <summary>
    /// ���������� � ������ VK Donut
    /// </summary>
    property Donut: TVkDonut read FDonut write FDonut;
    /// <summary>
    /// True, ���� ������ ���� ������� � ������ ������� ��� ������.
    /// </summary>
    property FriendsOnly: Boolean read FFriends_only write FFriends_only;
    /// <summary>
    /// ������������� ������ ������ (�� ����� ����� ������������ ������)
    /// </summary>
    property FromId: Integer read FFrom_id write FFrom_id;
    /// <summary>
    /// ���������� � ��������������
    /// </summary>
    property Geo: TVkGeo read FGeo write FGeo;
    /// <summary>
    /// �������� ������
    /// </summary>
    property IsArchived: Boolean read FIs_archived write FIs_archived;
    /// <summary>
    /// True, ���� ������ �������� � �������� � �������� ������������.
    /// </summary>
    property IsFavorite: Boolean read FIs_favorite write FIs_favorite;
    /// <summary>
    /// ���������� � ���, ��� ������ ����������
    /// </summary>
    property IsPinned: Boolean read FIs_pinned write FIs_pinned;
    /// <summary>
    /// ���������� � ������ � ������
    /// </summary>
    property Likes: TVkLikesInfo read FLikes write FLikes;
    /// <summary>
    /// ���������� � ���, �������� �� ������ ������� "�������"
    /// </summary>
    property MarkedAsAds: Boolean read FMarked_as_ads write FMarked_as_ads;
    /// <summary>
    /// ������������� ��������� �����, �� ������� ��������� ������. � ������� API ���� 5.7 ��� ���� ���������� ToId
    /// </summary>
    property OwnerId: Integer read FOwner_id write FOwner_id;
    /// <summary>
    /// ������������� ���������� ������. ��� ���� ������������ �����, ����� ������ ������ �� �������
    /// </summary>
    property PostponedId: Integer read FPostponed_id write FPostponed_id;
    /// <summary>
    /// ���������� � ������� ���������� ������ (���� ������������ ������ ��� Standalone-���������� � ������ �������, ���������� � Implicit Flow.)
    /// </summary>
    property PostSource: TVkPostSource read FPost_source write FPost_source;
    /// <summary>
    /// ��� ������, ����� ��������� ��������� ��������: post, copy, reply, postpone, suggest.
    /// </summary>
    property PostType: string read FPost_type write FPost_type;
    /// <summary>
    /// ������������� ��������� ������, � ����� �� ������� ���� ��������� �������
    /// </summary>
    property ReplyOwnerId: Integer read FReply_owner_id write FReply_owner_id;
    /// <summary>
    /// ������������� ������, � ����� �� ������� ���� ��������� �������
    /// </summary>
    property ReplyPostId: Integer read FReply_post_id write FReply_post_id;
    /// <summary>
    /// ���������� � �������� ������ (����������� �������)
    /// </summary>
    property Reposts: TVkRepostsInfo read FReposts write FReposts;
    /// <summary>
    /// ������������� ������, ���� ������ ���� ������������ �� ����� ���������� � ��������� �������������
    /// </summary>
    property SignerId: Integer read FSigner_id write FSigner_id;
    /// <summary>
    /// [�������� ����������������� ����]
    /// </summary>
    property ShortTextRate: Extended read FShort_text_rate write FShort_text_rate;
    /// <summary>
    /// ����� ������
    /// </summary>
    property Text: string read FText write FText;
    /// <summary>
    /// ������������� ��������� �����, �� ������� ��������� ������ (API ���� 5.7)
    /// </summary>
    property ToId: Integer read FTo_id write FTo_id;
    /// <summary>
    /// ��� ������, ����� ��������� ��������� ��������: post, copy, reply, postpone, suggest.
    /// </summary>
    property&Type: TVkPostType read FType write FType;
    /// <summary>
    /// ���������� � ���������� ������
    /// </summary>
    property Views: TVkViewsInfo read FViews write FViews;
    constructor Create; override;
    destructor Destroy; override;
    function ToAttachment: TAttachment;
  end;

  TVkPosts = TVkEntityExtendedList<TVkPost>;

  TVkRepostInfo = class(TVkEntity)
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
  end;

implementation

uses
  System.DateUtils, VK.CommonUtils;

{TVkAttachment}

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
  if Assigned(FMoney_transfer) then
    FMoney_transfer.Free;
  if Assigned(FAlbum) then
    FAlbum.Free;
  if Assigned(FPretty_cards) then
    FPretty_cards.Free;
  if Assigned(FEvent) then
    FEvent.Free;

  inherited;
end;

function TVkAttachment.GetPreviewUrl: string;
begin
  case&Type of
    TVkAttachmentType.Photo:
      Exit(Self.FPhoto.Sizes[4].Url);
    TVkAttachmentType.Video:
      Exit(Self.FVideo.Image[4].Url);
    TVkAttachmentType.Audio:
      Exit('');
    TVkAttachmentType.Doc:
      Exit('');
    TVkAttachmentType.Link:
      Exit('');
    TVkAttachmentType.Market:
      Exit('');
    TVkAttachmentType.MarketAlbum:
      Exit('');
    TVkAttachmentType.Wall:
      Exit('');
    TVkAttachmentType.WallReply:
      Exit('');
    TVkAttachmentType.Sticker:
      Exit(Self.FSticker.Images[1].Url);
    TVkAttachmentType.Gift:
      Exit('');
  else
    Result := '';
  end;
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

function TVkComment.ToAttachment: TAttachment;
begin
  Result := TAttachment.WallReply(OwnerId, Id, AccessKey);
end;

{TVkPost}

constructor TVkPost.Create;
begin
  inherited;
  FComments := TVkCommentsInfo.Create();
  FLikes := TVkLikesInfo.Create();
  FReposts := TVkRepostsInfo.Create();
  FViews := TVkViewsInfo.Create;
end;

destructor TVkPost.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkPost>(FCopy_history);
  TArrayHelp.FreeArrayOfObject<TVkAttachment>(FAttachments);
  FComments.Free;
  FLikes.Free;
  FReposts.Free;
  FViews.Free;
  if Assigned(FGeo) then
    FGeo.Free;
  if Assigned(FPost_source) then
    FPost_source.Free;
  if Assigned(FCopyright) then
    FCopyright.Free;
  if Assigned(FDonut) then
    FDonut.Free;
  inherited;
end;

function TVkPost.ToAttachment: TAttachment;
begin
  Result := TAttachment.Wall(OwnerId, Id, AccessKey);
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

end.

