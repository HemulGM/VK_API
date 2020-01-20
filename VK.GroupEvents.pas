unit VK.GroupEvents;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, REST.Client,
  System.JSON, VK.Types, System.Generics.Collections, VK.LongPollServer, VK.API, VK.Entity.Media,
  VK.Entity.Audio, VK.Entity.Video, VK.Entity.Message, VK.Entity.ClientInfo, VK.Entity.Photo;

type
  TVkObjectInfo = record
    Id: Integer;
    OwnerId: Integer;
  end;

  TVkCommentInfo = record
    Id: Integer;
    OwnerId: Integer;
    DeleterId: Integer;
    ObjectId: Integer;
    UserId: Integer;
  end;

  TVkGroupUserBlock = record
    AdminId: integer;
    UserId: integer;
    UnblockDate: TDateTime;
    Reason: TUserBlockReason;
    Comment: string;
  end;

  TVkGroupUserUnBlock = record
    AdminId: integer;
    UserId: integer;
    ByEndDate: Boolean;
  end;

  TOnCommentAction = procedure(Sender: TObject; GroupId: Integer; Comment: TVkComment; Info:
    TVkObjectInfo; EventId: string) of object;

  TOnCommentDelete = procedure(Sender: TObject; GroupId: Integer; Info: TVkCommentInfo; EventId: string) of object;

  TOnGroupLeave = procedure(Sender: TObject; GroupId: Integer; UserId: Integer; IsSelf: Boolean;
    EventId: string) of object;

  TOnGroupJoin = procedure(Sender: TObject; GroupId: Integer; UserId: Integer; JoinType:
    TGroupJoinType; EventId: string) of object;

  TOnGroupUserBlock = procedure(Sender: TObject; GroupId: Integer; Info: TVkGroupUserBlock; EventId: string) of object;

  TOnGroupUserUnBlock = procedure(Sender: TObject; GroupId: Integer; Info: TVkGroupUserUnBlock;
    EventId: string) of object;

  TOnVideoNew = procedure(Sender: TObject; GroupId: Integer; Audio: TVkVideo; EventId: string) of object;

  TOnPhotoNew = procedure(Sender: TObject; GroupId: Integer; Video: TVkPhoto; EventId: string) of object;

  TOnWallPostAction = procedure(Sender: TObject; GroupId: Integer; Post: TVkPost; EventId: string) of object;

  TOnAudioNew = procedure(Sender: TObject; GroupId: Integer; Audio: TVkAudio; EventId: string) of object;

  TOnGroupMessageNew = procedure(Sender: TObject; GroupId: Integer; Message: TVkMessage; ClientInfo:
    TVkClientInfo; EventId: string) of object;

  TOnGroupMessageAction = procedure(Sender: TObject; GroupId: Integer; Message: TVkMessage; EventId: string) of object;

  TOnGroupMessageAccess = procedure(Sender: TObject; GroupId: Integer; UserId: Integer; Key: string;
    EventId: string) of object;

  TCustomGroupEvents = class(TComponent)
  private
    FLongPollServer: TLongPollServer;
    FVK: TCustomVK;
    FGroupID: Integer;
    FOnWallReplyNew: TOnCommentAction;
    FOnWallReplyEdit: TOnCommentAction;
    FOnWallReplyRestore: TOnCommentAction;
    FOnWallReplyDelete: TOnCommentDelete;
    FOnWallPostNew: TOnWallPostAction;
    FOnWallRepost: TOnWallPostAction;
    FOnAudioNew: TOnAudioNew;
    FOnVideoNew: TOnVideoNew;
    FOnMessageNew: TOnGroupMessageNew;
    FOnMessageReply: TOnGroupMessageAction;
    FOnMessageEdit: TOnGroupMessageAction;
    FOnMessageAllow: TOnGroupMessageAccess;
    FOnMessageDeny: TOnGroupMessageAccess;
    FOnPhotoNew: TOnPhotoNew;
    FOnVideoCommentDelete: TOnCommentDelete;
    FOnVideoCommentEdit: TOnCommentAction;
    FOnVideoCommentRestore: TOnCommentAction;
    FOnVideoCommentNew: TOnCommentAction;
    FOnPhotoCommentDelete: TOnCommentDelete;
    FOnPhotoCommentEdit: TOnCommentAction;
    FOnPhotoCommentRestore: TOnCommentAction;
    FOnPhotoCommentNew: TOnCommentAction;
    FOnBoardPostDelete: TOnCommentDelete;
    FOnBoardPostEdit: TOnCommentAction;
    FOnBoardPostRestore: TOnCommentAction;
    FOnBoardPostNew: TOnCommentAction;
    FOnMarketCommentRestore: TOnCommentAction;
    FOnMarketCommentNew: TOnCommentAction;
    FOnMarketCommentDelete: TOnCommentDelete;
    FOnMarketCommentEdit: TOnCommentAction;
    FOnGroupLeave: TOnGroupLeave;
    FOnGroupJoin: TOnGroupJoin;
    FOnUserBlock: TOnGroupUserBlock;
    FOnUserUnBlock: TOnGroupUserUnBlock;
    procedure FOnError(Sender: TObject; Code: Integer; Text: string);
    procedure FOnLongPollUpdate(Sender: TObject; GroupID: string; Update: TJSONValue);
    procedure DoEvent(Sender: TObject; Update: TJSONValue);
    procedure SetVK(const Value: TCustomVK);
    procedure SetGroupID(const Value: Integer);
    //
    procedure DoAudioNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoBoardPostDelete(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoBoardPostEdit(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoBoardPostNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoBoardPostRestore(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoGroupJoin(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoGroupLeave(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoGroupOfficersEdit(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoMarketCommentDelete(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoMarketCommentEdit(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoMarketCommentNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoMarketCommentRestore(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoMessageAllow(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoMessageDeny(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoMessageEdit(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoMessageNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoMessageReply(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoPhotoCommentDelete(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoPhotoCommentEdit(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoPhotoCommentNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoPhotoCommentRestore(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoPhotoNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoPollVoteNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoUserBlock(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoUserUnblock(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoVideoCommentEdit(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoVideoCommentNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoVideoCommentRestore(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoVideoCommentDelete(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoVideoNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoWallPostNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoWallReplyDelete(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoWallReplyEdit(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoWallReplyNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoWallReplyRestore(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoWallRepost(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoGroupChangeSettings(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoGroupChangePhoto(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoVkPayTransaction(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    procedure DoAppPayload(GroupId: Integer; EventObject: TJSONValue; EventId: string);
    //
    procedure SetOnWallReplyNew(const Value: TOnCommentAction);
    procedure SetOnWallReplyEdit(const Value: TOnCommentAction);
    procedure SetOnWallReplyRestore(const Value: TOnCommentAction);
    procedure SetOnWallReplyDelete(const Value: TOnCommentDelete);
    procedure SetOnWallPostNew(const Value: TOnWallPostAction);
    procedure SetOnWallRepost(const Value: TOnWallPostAction);
    procedure SetOnAudioNew(const Value: TOnAudioNew);
    procedure SetOnVideoNew(const Value: TOnVideoNew);
    procedure SetOnMessageNew(const Value: TOnGroupMessageNew);
    procedure SetOnMessageReply(const Value: TOnGroupMessageAction);
    procedure SetOnMessageEdit(const Value: TOnGroupMessageAction);
    procedure SetOnMessageAllow(const Value: TOnGroupMessageAccess);
    procedure SetOnMessageDeny(const Value: TOnGroupMessageAccess);
    procedure SetOnPhotoNew(const Value: TOnPhotoNew);
    procedure SetOnVideoCommentDelete(const Value: TOnCommentDelete);
    procedure SetOnVideoCommentEdit(const Value: TOnCommentAction);
    procedure SetOnVideoCommentNew(const Value: TOnCommentAction);
    procedure SetOnVideoCommentRestore(const Value: TOnCommentAction);
    procedure SetOnPhotoCommentDelete(const Value: TOnCommentDelete);
    procedure SetOnPhotoCommentEdit(const Value: TOnCommentAction);
    procedure SetOnPhotoCommentNew(const Value: TOnCommentAction);
    procedure SetOnPhotoCommentRestore(const Value: TOnCommentAction);
    procedure SetOnBoardPostDelete(const Value: TOnCommentDelete);
    procedure SetOnBoardPostEdit(const Value: TOnCommentAction);
    procedure SetOnBoardPostNew(const Value: TOnCommentAction);
    procedure SetOnBoardPostRestore(const Value: TOnCommentAction);
    procedure SetOnMarketCommentDelete(const Value: TOnCommentDelete);
    procedure SetOnMarketCommentEdit(const Value: TOnCommentAction);
    procedure SetOnMarketCommentNew(const Value: TOnCommentAction);
    procedure SetOnMarketCommentRestore(const Value: TOnCommentAction);
    procedure SetOnGroupLeave(const Value: TOnGroupLeave);
    procedure SetOnGroupJoin(const Value: TOnGroupJoin);
    procedure SetOnUserBlock(const Value: TOnGroupUserBlock);
    procedure SetOnUserUnBlock(const Value: TOnGroupUserUnBlock);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Stop;
    function Start: Boolean;
    property VK: TCustomVK read FVK write SetVK;
    property GroupID: Integer read FGroupID write SetGroupID;
    //
    property OnWallReplyNew: TOnCommentAction read FOnWallReplyNew write SetOnWallReplyNew;
    property OnWallReplyEdit: TOnCommentAction read FOnWallReplyEdit write SetOnWallReplyEdit;
    property OnWallReplyRestore: TOnCommentAction read FOnWallReplyRestore write SetOnWallReplyRestore;
    property OnWallReplyDelete: TOnCommentDelete read FOnWallReplyDelete write SetOnWallReplyDelete;
    property OnWallPostNew: TOnWallPostAction read FOnWallPostNew write SetOnWallPostNew;
    property OnWallRepost: TOnWallPostAction read FOnWallRepost write SetOnWallRepost;
    property OnAudioNew: TOnAudioNew read FOnAudioNew write SetOnAudioNew;
    property OnVideoNew: TOnVideoNew read FOnVideoNew write SetOnVideoNew;
    property OnPhotoNew: TOnPhotoNew read FOnPhotoNew write SetOnPhotoNew;
    property OnMessageNew: TOnGroupMessageNew read FOnMessageNew write SetOnMessageNew;
    property OnMessageReply: TOnGroupMessageAction read FOnMessageReply write SetOnMessageReply;
    property OnMessageEdit: TOnGroupMessageAction read FOnMessageEdit write SetOnMessageEdit;
    property OnMessageAllow: TOnGroupMessageAccess read FOnMessageAllow write SetOnMessageAllow;
    property OnMessageDeny: TOnGroupMessageAccess read FOnMessageDeny write SetOnMessageDeny;
    property OnVideoCommentNew: TOnCommentAction read FOnVideoCommentNew write SetOnVideoCommentNew;
    property OnVideoCommentEdit: TOnCommentAction read FOnVideoCommentEdit write SetOnVideoCommentEdit;
    property OnVideoCommentRestore: TOnCommentAction read FOnVideoCommentRestore write SetOnVideoCommentRestore;
    property OnVideoCommentDelete: TOnCommentDelete read FOnVideoCommentDelete write SetOnVideoCommentDelete;
    property OnPhotoCommentNew: TOnCommentAction read FOnPhotoCommentNew write SetOnPhotoCommentNew;
    property OnPhotoCommentEdit: TOnCommentAction read FOnPhotoCommentEdit write SetOnPhotoCommentEdit;
    property OnPhotoCommentRestore: TOnCommentAction read FOnPhotoCommentRestore write SetOnPhotoCommentRestore;
    property OnPhotoCommentDelete: TOnCommentDelete read FOnPhotoCommentDelete write SetOnPhotoCommentDelete;
    property OnBoardPostNew: TOnCommentAction read FOnBoardPostNew write SetOnBoardPostNew;
    property OnBoardPostEdit: TOnCommentAction read FOnBoardPostEdit write SetOnBoardPostEdit;
    property OnBoardPostRestore: TOnCommentAction read FOnBoardPostRestore write SetOnBoardPostRestore;
    property OnBoardPostDelete: TOnCommentDelete read FOnBoardPostDelete write SetOnBoardPostDelete;
    property OnMarketCommentNew: TOnCommentAction read FOnMarketCommentNew write SetOnMarketCommentNew;
    property OnMarketCommentEdit: TOnCommentAction read FOnMarketCommentEdit write SetOnMarketCommentEdit;
    property OnMarketCommentRestore: TOnCommentAction read FOnMarketCommentRestore write SetOnMarketCommentRestore;
    property OnMarketCommentDelete: TOnCommentDelete read FOnMarketCommentDelete write SetOnMarketCommentDelete;
    property OnGroupLeave: TOnGroupLeave read FOnGroupLeave write SetOnGroupLeave;
    property OnGroupJoin: TOnGroupJoin read FOnGroupJoin write SetOnGroupJoin;
    property OnUserBlock: TOnGroupUserBlock read FOnUserBlock write SetOnUserBlock;
    property OnUserUnBlock: TOnGroupUserUnBlock read FOnUserUnBlock write SetOnUserUnBlock;
  end;

  TGroupEventsItems = TList<TCustomGroupEvents>;

  TCustomGroupEventControl = class(TComponent)
  private
    FReaded: Boolean;
    FItems: TGroupEventsItems;
    FGroups: TStrings;
    FVK: TCustomVK;
    FOnWallReplyNew: TOnCommentAction;
    FOnWallReplyEdit: TOnCommentAction;
    FOnWallReplyRestore: TOnCommentAction;
    FOnWallReplyDelete: TOnCommentDelete;
    FOnWallPostNew: TOnWallPostAction;
    FOnWallRepost: TOnWallPostAction;
    FOnAudioNew: TOnAudioNew;
    FOnVideoNew: TOnVideoNew;
    FOnMessageNew: TOnGroupMessageNew;
    FOnMessageReply: TOnGroupMessageAction;
    FOnMessageEdit: TOnGroupMessageAction;
    FOnMessageAllow: TOnGroupMessageAccess;
    FOnMessageDeny: TOnGroupMessageAccess;
    FOnPhotoNew: TOnPhotoNew;
    FOnVideoCommentDelete: TOnCommentDelete;
    FOnVideoCommentEdit: TOnCommentAction;
    FOnVideoCommentRestore: TOnCommentAction;
    FOnVideoCommentNew: TOnCommentAction;
    FOnPhotoCommentDelete: TOnCommentDelete;
    FOnPhotoCommentEdit: TOnCommentAction;
    FOnPhotoCommentRestore: TOnCommentAction;
    FOnPhotoCommentNew: TOnCommentAction;
    FOnBoardPostDelete: TOnCommentDelete;
    FOnBoardPostEdit: TOnCommentAction;
    FOnBoardPostRestore: TOnCommentAction;
    FOnBoardPostNew: TOnCommentAction;
    FOnMarketCommentRestore: TOnCommentAction;
    FOnMarketCommentNew: TOnCommentAction;
    FOnMarketCommentDelete: TOnCommentDelete;
    FOnMarketCommentEdit: TOnCommentAction;
    FOnGroupLeave: TOnGroupLeave;
    FOnGroupJoin: TOnGroupJoin;
    FOnUserBlock: TOnGroupUserBlock;
    FOnUserUnBlock: TOnGroupUserUnBlock;
    procedure SetItems(const Value: TGroupEventsItems);
    procedure SetGroups(const Value: TStrings);
    procedure SetVK(const Value: TCustomVK);
    procedure SetOnWallReplyNew(const Value: TOnCommentAction);
    procedure SetOnWallReplyEdit(const Value: TOnCommentAction);
    procedure SetOnWallReplyRestore(const Value: TOnCommentAction);
    procedure SetOnWallReplyDelete(const Value: TOnCommentDelete);
    procedure SetOnWallPostNew(const Value: TOnWallPostAction);
    procedure SetOnWallRepost(const Value: TOnWallPostAction);
    procedure SetOnAudioNew(const Value: TOnAudioNew);
    procedure SetOnVideoNew(const Value: TOnVideoNew);
    procedure SetOnMessageNew(const Value: TOnGroupMessageNew);
    procedure SetOnMessageEdit(const Value: TOnGroupMessageAction);
    procedure SetOnMessageReply(const Value: TOnGroupMessageAction);
    procedure SetOnMessageAllow(const Value: TOnGroupMessageAccess);
    procedure SetOnMessageDeny(const Value: TOnGroupMessageAccess);
    procedure SetOnPhotoNew(const Value: TOnPhotoNew);
    procedure SetOnVideoCommentDelete(const Value: TOnCommentDelete);
    procedure SetOnVideoCommentEdit(const Value: TOnCommentAction);
    procedure SetOnVideoCommentNew(const Value: TOnCommentAction);
    procedure SetOnVideoCommentRestore(const Value: TOnCommentAction);
    procedure SetOnPhotoCommentDelete(const Value: TOnCommentDelete);
    procedure SetOnPhotoCommentEdit(const Value: TOnCommentAction);
    procedure SetOnPhotoCommentNew(const Value: TOnCommentAction);
    procedure SetOnPhotoCommentRestore(const Value: TOnCommentAction);
    procedure SetOnBoardPostDelete(const Value: TOnCommentDelete);
    procedure SetOnBoardPostEdit(const Value: TOnCommentAction);
    procedure SetOnBoardPostNew(const Value: TOnCommentAction);
    procedure SetOnBoardPostRestore(const Value: TOnCommentAction);
    procedure SetOnMarketCommentDelete(const Value: TOnCommentDelete);
    procedure SetOnMarketCommentEdit(const Value: TOnCommentAction);
    procedure SetOnMarketCommentNew(const Value: TOnCommentAction);
    procedure SetOnMarketCommentRestore(const Value: TOnCommentAction);
    procedure SetOnGroupLeave(const Value: TOnGroupLeave);
    procedure SetOnGroupJoin(const Value: TOnGroupJoin);
    procedure SetOnUserBlock(const Value: TOnGroupUserBlock);
    procedure SetOnUserUnBlock(const Value: TOnGroupUserUnBlock);
  public
    constructor Create(AOwner: TComponent); override;
    procedure DefineProperties(Filer: TFiler); override;
    destructor Destroy; override;
    procedure Stop;
    function Start: Boolean;
    procedure FillFromList;
    function Add(GroupId: Integer): TCustomGroupEvents;
    property Items: TGroupEventsItems read FItems write SetItems;
    property VK: TCustomVK read FVK write SetVK;
    property Groups: TStrings read FGroups write SetGroups;
    property OnWallReplyNew: TOnCommentAction read FOnWallReplyNew write SetOnWallReplyNew;
    property OnWallReplyEdit: TOnCommentAction read FOnWallReplyEdit write SetOnWallReplyEdit;
    property OnWallReplyRestore: TOnCommentAction read FOnWallReplyRestore write SetOnWallReplyRestore;
    property OnWallReplyDelete: TOnCommentDelete read FOnWallReplyDelete write SetOnWallReplyDelete;
    property OnWallPostNew: TOnWallPostAction read FOnWallPostNew write SetOnWallPostNew;
    property OnWallRepost: TOnWallPostAction read FOnWallRepost write SetOnWallRepost;
    property OnAudioNew: TOnAudioNew read FOnAudioNew write SetOnAudioNew;
    property OnVideoNew: TOnVideoNew read FOnVideoNew write SetOnVideoNew;
    property OnPhotoNew: TOnPhotoNew read FOnPhotoNew write SetOnPhotoNew;
    property OnMessageNew: TOnGroupMessageNew read FOnMessageNew write SetOnMessageNew;
    property OnMessageReply: TOnGroupMessageAction read FOnMessageReply write SetOnMessageReply;
    property OnMessageEdit: TOnGroupMessageAction read FOnMessageEdit write SetOnMessageEdit;
    property OnMessageAllow: TOnGroupMessageAccess read FOnMessageAllow write SetOnMessageAllow;
    property OnMessageDeny: TOnGroupMessageAccess read FOnMessageDeny write SetOnMessageDeny;
    property OnVideoCommentNew: TOnCommentAction read FOnVideoCommentNew write SetOnVideoCommentNew;
    property OnVideoCommentEdit: TOnCommentAction read FOnVideoCommentEdit write SetOnVideoCommentEdit;
    property OnVideoCommentRestore: TOnCommentAction read FOnVideoCommentRestore write SetOnVideoCommentRestore;
    property OnVideoCommentDelete: TOnCommentDelete read FOnVideoCommentDelete write SetOnVideoCommentDelete;
    property OnPhotoCommentNew: TOnCommentAction read FOnPhotoCommentNew write SetOnPhotoCommentNew;
    property OnPhotoCommentEdit: TOnCommentAction read FOnPhotoCommentEdit write SetOnPhotoCommentEdit;
    property OnPhotoCommentRestore: TOnCommentAction read FOnPhotoCommentRestore write SetOnPhotoCommentRestore;
    property OnPhotoCommentDelete: TOnCommentDelete read FOnPhotoCommentDelete write SetOnPhotoCommentDelete;
    property OnBoardPostNew: TOnCommentAction read FOnBoardPostNew write SetOnBoardPostNew;
    property OnBoardPostEdit: TOnCommentAction read FOnBoardPostEdit write SetOnBoardPostEdit;
    property OnBoardPostRestore: TOnCommentAction read FOnBoardPostRestore write SetOnBoardPostRestore;
    property OnBoardPostDelete: TOnCommentDelete read FOnBoardPostDelete write SetOnBoardPostDelete;
    property OnMarketCommentNew: TOnCommentAction read FOnMarketCommentNew write SetOnMarketCommentNew;
    property OnMarketCommentEdit: TOnCommentAction read FOnMarketCommentEdit write SetOnMarketCommentEdit;
    property OnMarketCommentRestore: TOnCommentAction read FOnMarketCommentRestore write SetOnMarketCommentRestore;
    property OnMarketCommentDelete: TOnCommentDelete read FOnMarketCommentDelete write SetOnMarketCommentDelete;
    property OnGroupLeave: TOnGroupLeave read FOnGroupLeave write SetOnGroupLeave;
    property OnGroupJoin: TOnGroupJoin read FOnGroupJoin write SetOnGroupJoin;
    property OnUserBlock: TOnGroupUserBlock read FOnUserBlock write SetOnUserBlock;
    property OnUserUnBlock: TOnGroupUserUnBlock read FOnUserUnBlock write SetOnUserUnBlock;
  end;

implementation

uses
  System.DateUtils;

{ TUserEvents }

constructor TCustomGroupEvents.Create(AOwner: TComponent);
begin
  inherited;
  FLongPollServer := TLongPollServer.Create;
  FLongPollServer.OnUpdate := FOnLongPollUpdate;
  FLongPollServer.OnError := FOnError;
end;

destructor TCustomGroupEvents.Destroy;
begin
  FLongPollServer.Free;
  inherited;
end;

procedure TCustomGroupEvents.DoEvent(Sender: TObject; Update: TJSONValue);
var
  GroupId: Integer;
  EventType: string;
  EventId: string;
  EventObject: TJSONValue;

  function NormalizePeerId(Value: Integer): Integer;
  begin
    if Value > 2000000000 then
      Exit(Value - 2000000000);
    if Value > 1000000000 then
      Exit(-(Value - 1000000000));
    Result := Value;
  end;

begin
  EventType := Update.GetValue<string>('type', '');
  EventObject := Update.GetValue<TJSONValue>('object', nil);
  GroupId := Update.GetValue<Integer>('group_id', 0);
  EventId := Update.GetValue<string>('event_id', '');

  if EventType = 'message_new' then
    DoMessageNew(GroupId, EventObject, EventId)
  else if (EventType = 'message_reply') then
    DoMessageReply(GroupId, EventObject, EventId)
  else if (EventType = 'message_edit') then
    DoMessageEdit(GroupId, EventObject, EventId)
  else if EventType = 'message_allow' then
    DoMessageAllow(GroupId, EventObject, EventId)
  else if EventType = 'message_deny' then
    DoMessageDeny(GroupId, EventObject, EventId)
  else if EventType = 'photo_new' then
    DoPhotoNew(GroupId, EventObject, EventId)
  else if EventType = 'photo_comment_new' then
    DoPhotoCommentNew(GroupId, EventObject, EventId)
  else if EventType = 'photo_comment_edit' then
    DoPhotoCommentEdit(GroupId, EventObject, EventId)
  else if EventType = 'photo_comment_restore' then
    DoPhotoCommentRestore(GroupId, EventObject, EventId)
  else if EventType = 'photo_comment_delete' then
    DoPhotoCommentDelete(GroupId, EventObject, EventId)
  else if EventType = 'audio_new' then
    DoAudioNew(GroupId, EventObject, EventId)
  else if EventType = 'video_new' then
    DoVideoNew(GroupId, EventObject, EventId)
  else if EventType = 'video_comment_new' then
    DoVideoCommentNew(GroupId, EventObject, EventId)
  else if EventType = 'video_comment_edit' then
    DoVideoCommentEdit(GroupId, EventObject, EventId)
  else if EventType = 'video_comment_restore' then
    DoVideoCommentRestore(GroupId, EventObject, EventId)
  else if EventType = 'video_comment_delete' then
    DoVideoCommentDelete(GroupId, EventObject, EventId)
  else if EventType = 'wall_post_new' then
    DoWallPostNew(GroupId, EventObject, EventId)
  else if EventType = 'wall_repost' then
    DoWallRepost(GroupId, EventObject, EventId)
  else if EventType = 'wall_reply_new' then
    DoWallReplyNew(GroupId, EventObject, EventId)
  else if EventType = 'wall_reply_edit' then
    DoWallReplyEdit(GroupId, EventObject, EventId)
  else if EventType = 'wall_reply_restore' then
    DoWallReplyRestore(GroupId, EventObject, EventId)
  else if EventType = 'wall_reply_delete' then
    DoWallReplyDelete(GroupId, EventObject, EventId)
  else if EventType = 'board_post_new' then
    DoBoardPostNew(GroupId, EventObject, EventId)
  else if EventType = 'board_post_edit' then
    DoBoardPostEdit(GroupId, EventObject, EventId)
  else if EventType = 'board_post_restore' then
    DoBoardPostRestore(GroupId, EventObject, EventId)
  else if EventType = 'board_post_delete' then
    DoBoardPostDelete(GroupId, EventObject, EventId)
  else if EventType = 'market_comment_new' then
    DoMarketCommentNew(GroupId, EventObject, EventId)
  else if EventType = 'market_comment_edit' then
    DoMarketCommentEdit(GroupId, EventObject, EventId)
  else if EventType = 'market_comment_restore' then
    DoMarketCommentRestore(GroupId, EventObject, EventId)
  else if EventType = 'market_comment_delete' then
    DoMarketCommentDelete(GroupId, EventObject, EventId)
  else if EventType = 'group_leave' then
    DoGroupLeave(GroupId, EventObject, EventId)
  else if EventType = 'group_join' then
    DoGroupJoin(GroupId, EventObject, EventId)
  else if EventType = 'group_officers_edit' then
    DoGroupOfficersEdit(GroupId, EventObject, EventId)
  else if EventType = 'user_block' then
    DoUserBlock(GroupId, EventObject, EventId)
  else if EventType = 'user_unblock' then
    DoUserUnblock(GroupId, EventObject, EventId)
  else if EventType = 'group_change_settings' then
    DoGroupChangeSettings(GroupId, EventObject, EventId)
  else if EventType = 'group_change_photo' then
    DoGroupChangePhoto(GroupId, EventObject, EventId)
  else if EventType = 'vkpay_transaction' then
    DoVkPayTransaction(GroupId, EventObject, EventId)
  else if EventType = 'app_payload' then
    DoAppPayload(GroupId, EventObject, EventId)
  else if EventType = 'poll_vote_new' then
    DoPollVoteNew(GroupId, EventObject, EventId);
end;

procedure TCustomGroupEvents.DoAudioNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Audio: TVkAudio;
begin
  if Assigned(FOnAudioNew) then
  begin
    Audio := TVkAudio.FromJsonString(EventObject.ToString);
    FOnAudioNew(Self, GroupId, Audio, EventId);
    Audio.Free;
  end;
end;

procedure TCustomGroupEvents.DoAppPayload(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoBoardPostDelete(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Info: TVkCommentInfo;
begin
  if Assigned(FOnBoardPostDelete) then
  begin
    Info.Id := EventObject.GetValue<Integer>('id', -1);
    Info.OwnerId := EventObject.GetValue<Integer>('owner_id', -1);
    Info.DeleterId := EventObject.GetValue<Integer>('deleter_id', -1);
    Info.ObjectId := -1;
    Info.UserId := -1;
    FOnBoardPostDelete(Self, GroupId, Info, EventId);
  end;
end;

procedure TCustomGroupEvents.DoBoardPostEdit(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Comment: TVkComment;
  Info: TVkObjectInfo;
begin
  if Assigned(FOnBoardPostEdit) then
  begin
    Comment := TVkComment.FromJsonString(EventObject.ToString);
    Info.Id := EventObject.GetValue<Integer>('topic_id', -1);
    Info.OwnerId := EventObject.GetValue<Integer>('topic_owner_id', -1);
    FOnBoardPostEdit(Self, GroupId, Comment, Info, EventId);
    Comment.Free;
  end;
end;

procedure TCustomGroupEvents.DoBoardPostNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Comment: TVkComment;
  Info: TVkObjectInfo;
begin
  if Assigned(FOnBoardPostNew) then
  begin
    Comment := TVkComment.FromJsonString(EventObject.ToString);
    Info.Id := EventObject.GetValue<Integer>('topic_id', -1);
    Info.OwnerId := EventObject.GetValue<Integer>('topic_owner_id', -1);
    FOnBoardPostNew(Self, GroupId, Comment, Info, EventId);
    Comment.Free;
  end;
end;

procedure TCustomGroupEvents.DoBoardPostRestore(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Comment: TVkComment;
  Info: TVkObjectInfo;
begin
  if Assigned(FOnBoardPostRestore) then
  begin
    Comment := TVkComment.FromJsonString(EventObject.ToString);
    Info.Id := EventObject.GetValue<Integer>('topic_id', -1);
    Info.OwnerId := EventObject.GetValue<Integer>('topic_owner_id', -1);
    FOnBoardPostRestore(Self, GroupId, Comment, Info, EventId);
    Comment.Free;
  end;
end;

procedure TCustomGroupEvents.DoGroupChangePhoto(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoGroupChangeSettings(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoGroupJoin(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  UserId: Integer;
  JoinType: TGroupJoinType;
begin
  if Assigned(FOnGroupJoin) then
  begin
    UserId := EventObject.GetValue<Integer>('user_id', -1);
    JoinType := GroupJoinType.Create(EventObject.GetValue<string>('join_type ', ''));
    FOnGroupJoin(Self, GroupId, UserId, JoinType, EventId);
  end;
end;

procedure TCustomGroupEvents.DoGroupLeave(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  UserId: Integer;
  IsSelf: Boolean;
begin
  if Assigned(FOnGroupLeave) then
  begin
    UserId := EventObject.GetValue<Integer>('user_id', -1);
    IsSelf := EventObject.GetValue<Integer>('self', -1) = 1;
    FOnGroupLeave(Self, GroupId, UserId, IsSelf, EventId);
  end;
end;

procedure TCustomGroupEvents.DoGroupOfficersEdit(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoMarketCommentDelete(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Info: TVkCommentInfo;
begin
  if Assigned(FOnMarketCommentDelete) then
  begin
    Info.Id := EventObject.GetValue<Integer>('id', -1);
    Info.OwnerId := EventObject.GetValue<Integer>('owner_id', -1);
    Info.UserId := EventObject.GetValue<Integer>('user_id', -1);
    Info.DeleterId := EventObject.GetValue<Integer>('deleter_id', -1);
    Info.ObjectId := EventObject.GetValue<Integer>('item_id', -1);
    FOnMarketCommentDelete(Self, GroupId, Info, EventId);
  end;
end;

procedure TCustomGroupEvents.DoMarketCommentEdit(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Comment: TVkComment;
  Info: TVkObjectInfo;
begin
  if Assigned(FOnMarketCommentEdit) then
  begin
    Comment := TVkComment.FromJsonString(EventObject.ToString);
    Info.Id := EventObject.GetValue<Integer>('item_id', -1);
    Info.OwnerId := EventObject.GetValue<Integer>('market_owner_id', -1);
    FOnMarketCommentEdit(Self, GroupId, Comment, Info, EventId);
    Comment.Free;
  end;
end;

procedure TCustomGroupEvents.DoMarketCommentNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Comment: TVkComment;
  Info: TVkObjectInfo;
begin
  if Assigned(FOnMarketCommentNew) then
  begin
    Comment := TVkComment.FromJsonString(EventObject.ToString);
    Info.Id := EventObject.GetValue<Integer>('item_id', -1);
    Info.OwnerId := EventObject.GetValue<Integer>('market_owner_id', -1);
    FOnMarketCommentNew(Self, GroupId, Comment, Info, EventId);
    Comment.Free;
  end;
end;

procedure TCustomGroupEvents.DoMarketCommentRestore(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Comment: TVkComment;
  Info: TVkObjectInfo;
begin
  if Assigned(FOnMarketCommentRestore) then
  begin
    Comment := TVkComment.FromJsonString(EventObject.ToString);
    Info.Id := EventObject.GetValue<Integer>('item_id', -1);
    Info.OwnerId := EventObject.GetValue<Integer>('market_owner_id', -1);
    FOnMarketCommentRestore(Self, GroupId, Comment, Info, EventId);
    Comment.Free;
  end;
end;

procedure TCustomGroupEvents.DoMessageAllow(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  UserId: Integer;
  Key: string;
begin
  if Assigned(FOnMessageAllow) then
  begin
    UserId := EventObject.GetValue<Integer>('user_id', -1);
    Key := EventObject.GetValue<string>('key', '');
    FOnMessageAllow(Self, GroupId, UserId, Key, EventId);
  end;
end;

procedure TCustomGroupEvents.DoMessageDeny(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  UserId: Integer;
begin
  if Assigned(FOnMessageDeny) then
  begin
    UserId := EventObject.GetValue<Integer>('user_id', -1);
    FOnMessageDeny(Self, GroupId, UserId, '', EventId);
  end;
end;

procedure TCustomGroupEvents.DoMessageEdit(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Message: TVkMessage;
begin
  if Assigned(FOnMessageEdit) then
  begin
    Message := TVkMessage.FromJsonString(EventObject.ToString);
    FOnMessageEdit(Self, GroupId, Message, EventId);
    Message.Free;
  end;
end;

procedure TCustomGroupEvents.DoMessageNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Message: TVkMessage;
  ClientInfo: TVkClientInfo;
begin
  if Assigned(FOnMessageNew) then
  begin
    Message := TVkMessage.FromJsonString(EventObject.GetValue<TJSONValue>('message').ToString);
    ClientInfo := TVkClientInfo.FromJsonString(EventObject.GetValue<TJSONValue>('client_info').ToString);
    FOnMessageNew(Self, GroupId, Message, ClientInfo, EventId);
    Message.Free;
    ClientInfo.Free;
  end;
end;

procedure TCustomGroupEvents.DoMessageReply(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Message: TVkMessage;
begin
  if Assigned(FOnMessageReply) then
  begin
    Message := TVkMessage.FromJsonString(EventObject.ToString);
    FOnMessageReply(Self, GroupId, Message, EventId);
    Message.Free;
  end;
end;

procedure TCustomGroupEvents.DoPhotoCommentDelete(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Info: TVkCommentInfo;
begin
  if Assigned(FOnPhotoCommentDelete) then
  begin
    Info.Id := EventObject.GetValue<Integer>('id', -1);
    Info.OwnerId := EventObject.GetValue<Integer>('owner_id', -1);
    Info.UserId := -1;
    Info.DeleterId := EventObject.GetValue<Integer>('deleter_id', -1);
    Info.ObjectId := EventObject.GetValue<Integer>('photo_id', -1);
    FOnPhotoCommentDelete(Self, GroupId, Info, EventId);
  end;
end;

procedure TCustomGroupEvents.DoPhotoCommentEdit(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Comment: TVkComment;
  Info: TVkObjectInfo;
begin
  if Assigned(FOnPhotoCommentEdit) then
  begin
    Comment := TVkComment.FromJsonString(EventObject.ToString);
    Info.Id := EventObject.GetValue<Integer>('photo_id', -1);
    Info.OwnerId := EventObject.GetValue<Integer>('photo_owner_id', -1);
    FOnPhotoCommentEdit(Self, GroupId, Comment, Info, EventId);
    Comment.Free;
  end;
end;

procedure TCustomGroupEvents.DoPhotoCommentNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Comment: TVkComment;
  Info: TVkObjectInfo;
begin
  if Assigned(FOnPhotoCommentNew) then
  begin
    Comment := TVkComment.FromJsonString(EventObject.ToString);
    Info.Id := EventObject.GetValue<Integer>('photo_id', -1);
    Info.OwnerId := EventObject.GetValue<Integer>('photo_owner_id', -1);
    FOnPhotoCommentNew(Self, GroupId, Comment, Info, EventId);
    Comment.Free;
  end;
end;

procedure TCustomGroupEvents.DoPhotoCommentRestore(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Comment: TVkComment;
  Info: TVkObjectInfo;
begin
  if Assigned(FOnPhotoCommentRestore) then
  begin
    Comment := TVkComment.FromJsonString(EventObject.ToString);
    Info.Id := EventObject.GetValue<Integer>('photo_id', -1);
    Info.OwnerId := EventObject.GetValue<Integer>('photo_owner_id', -1);
    FOnPhotoCommentRestore(Self, GroupId, Comment, Info, EventId);
    Comment.Free;
  end;
end;

procedure TCustomGroupEvents.DoPhotoNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Photo: TVkPhoto;
begin
  if Assigned(FOnPhotoNew) then
  begin
    Photo := TVkPhoto.FromJsonString(EventObject.ToString);
    FOnPhotoNew(Self, GroupId, Photo, EventId);
    Photo.Free;
  end;
end;

procedure TCustomGroupEvents.DoPollVoteNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoUserBlock(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Info: TVkGroupUserBlock;
begin
  if Assigned(FOnUserBlock) then
  begin
    Info.AdminId := EventObject.GetValue<Integer>('admin_id', -1);
    Info.UserId := EventObject.GetValue<Integer>('user_id', -1);
    Info.UnblockDate := UnixToDateTime(EventObject.GetValue<Integer>('unblock_date ', 0), False);
    Info.Reason := TUserBlockReason(EventObject.GetValue<Integer>('reason', 0));
    Info.Comment := EventObject.GetValue<string>('comment', '');
    FOnUserBlock(Self, GroupId, Info, EventId);
  end;
end;

procedure TCustomGroupEvents.DoUserUnblock(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Info: TVkGroupUserUnBlock;
begin
  if Assigned(FOnUserUnBlock) then
  begin
    Info.AdminId := EventObject.GetValue<Integer>('admin_id', -1);
    Info.UserId := EventObject.GetValue<Integer>('user_id', -1);
    Info.ByEndDate := EventObject.GetValue<Integer>('by_end_date ', -1) = 1;
    FOnUserUnBlock(Self, GroupId, Info, EventId);
  end;
end;

procedure TCustomGroupEvents.DoVideoCommentDelete(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Info: TVkCommentInfo;
begin
  if Assigned(FOnVideoCommentDelete) then
  begin
    Info.Id := EventObject.GetValue<Integer>('id', -1);
    Info.OwnerId := EventObject.GetValue<Integer>('owner_id', -1);
    Info.DeleterId := EventObject.GetValue<Integer>('deleter_id', -1);
    Info.UserId := EventObject.GetValue<Integer>('user_id', -1);
    Info.ObjectId := EventObject.GetValue<Integer>('video_id', -1);
    FOnVideoCommentDelete(Self, GroupId, Info, EventId);
  end;
end;

procedure TCustomGroupEvents.DoVideoCommentEdit(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Comment: TVkComment;
  Info: TVkObjectInfo;
begin
  if Assigned(FOnVideoCommentEdit) then
  begin
    Comment := TVkComment.FromJsonString(EventObject.ToString);
    Info.Id := EventObject.GetValue<Integer>('video_id', -1);
    Info.OwnerId := EventObject.GetValue<Integer>('video_owner_id', -1);
    FOnVideoCommentEdit(Self, GroupId, Comment, Info, EventId);
    Comment.Free;
  end;
end;

procedure TCustomGroupEvents.DoVideoCommentNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Comment: TVkComment;
  Info: TVkObjectInfo;
begin
  if Assigned(FOnVideoCommentNew) then
  begin
    Comment := TVkComment.FromJsonString(EventObject.ToString);
    Info.Id := EventObject.GetValue<Integer>('video_id', -1);
    Info.OwnerId := EventObject.GetValue<Integer>('video_owner_id', -1);
    FOnVideoCommentNew(Self, GroupId, Comment, Info, EventId);
    Comment.Free;
  end;
end;

procedure TCustomGroupEvents.DoVideoCommentRestore(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Comment: TVkComment;
  Info: TVkObjectInfo;
begin
  if Assigned(FOnVideoCommentRestore) then
  begin
    Comment := TVkComment.FromJsonString(EventObject.ToString);
    Info.Id := EventObject.GetValue<Integer>('video_id', -1);
    Info.OwnerId := EventObject.GetValue<Integer>('video_owner_id', -1);
    FOnVideoCommentRestore(Self, GroupId, Comment, Info, EventId);
    Comment.Free;
  end;
end;

procedure TCustomGroupEvents.DoVideoNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Video: TVkVideo;
begin
  if Assigned(FOnVideoNew) then
  begin
    Video := TVkVideo.FromJsonString(EventObject.ToString);
    FOnVideoNew(Self, GroupId, Video, EventId);
    Video.Free;
  end;
end;

procedure TCustomGroupEvents.DoVkPayTransaction(GroupId: Integer; EventObject: TJSONValue; EventId: string);
begin

end;

procedure TCustomGroupEvents.DoWallPostNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Post: TVkPost;
begin
  if Assigned(FOnWallPostNew) then
  begin
    Post := TVkPost.FromJsonString(EventObject.ToString);
    FOnWallPostNew(Self, GroupId, Post, EventId);
    Post.Free;
  end;
end;

procedure TCustomGroupEvents.DoWallReplyDelete(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Info: TVkCommentInfo;
begin
  if Assigned(FOnWallReplyDelete) then
  begin
    Info.Id := EventObject.GetValue<Integer>('id', -1);
    Info.OwnerId := EventObject.GetValue<Integer>('owner_id', -1);
    Info.DeleterId := EventObject.GetValue<Integer>('deleter_id', -1);
    Info.UserId := -1;
    Info.ObjectId := EventObject.GetValue<Integer>('post_id', -1);
    FOnWallReplyDelete(Self, GroupId, Info, EventId);
  end;
end;

procedure TCustomGroupEvents.DoWallReplyEdit(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Comment: TVkComment;
  Info: TVkObjectInfo;
begin
  if Assigned(FOnWallReplyEdit) then
  begin
    Comment := TVkComment.FromJsonString(EventObject.ToString);
    Info.Id := EventObject.GetValue<Integer>('post_id', -1);
    Info.OwnerId := EventObject.GetValue<Integer>('post_owner_id', -1);
    FOnWallReplyEdit(Self, GroupId, Comment, Info, EventId);
    Comment.Free;
  end;
end;

procedure TCustomGroupEvents.DoWallReplyNew(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Comment: TVkComment;
  Info: TVkObjectInfo;
begin
  if Assigned(FOnWallReplyNew) then
  begin
    Comment := TVkComment.FromJsonString(EventObject.ToString);
    Info.Id := EventObject.GetValue<Integer>('post_id', -1);
    Info.OwnerId := EventObject.GetValue<Integer>('post_owner_id', -1);
    FOnWallReplyNew(Self, GroupId, Comment, Info, EventId);
    Comment.Free;
  end;
end;

procedure TCustomGroupEvents.DoWallReplyRestore(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Comment: TVkComment;
  Info: TVkObjectInfo;
begin
  if Assigned(FOnWallReplyRestore) then
  begin
    Comment := TVkComment.FromJsonString(EventObject.ToString);
    Info.Id := EventObject.GetValue<Integer>('post_id', -1);
    Info.OwnerId := EventObject.GetValue<Integer>('post_owner_id', -1);
    FOnWallReplyRestore(Self, GroupId, Comment, Info, EventId);
    Comment.Free;
  end;
end;

procedure TCustomGroupEvents.DoWallRepost(GroupId: Integer; EventObject: TJSONValue; EventId: string);
var
  Post: TVkPost;
begin
  if Assigned(FOnWallRepost) then
  begin
    Post := TVkPost.FromJsonString(EventObject.ToString);
    FOnWallRepost(Self, GroupId, Post, EventId);
    Post.Free;
  end;
end;

procedure TCustomGroupEvents.FOnError(Sender: TObject; Code: Integer; Text: string);
begin
  FVK.DoLog(Self, Text);
end;

procedure TCustomGroupEvents.FOnLongPollUpdate(Sender: TObject; GroupID: string; Update: TJSONValue);
begin
  FVK.DoLog(Self, Update.ToString);
  DoEvent(Sender, Update);
end;

procedure TCustomGroupEvents.SetGroupID(const Value: Integer);
begin
  if Value > 0 then
    raise Exception.Create('Идентификатор группы может быть только отрицательным цислом');
  FGroupID := Value;
end;

procedure TCustomGroupEvents.SetOnAudioNew(const Value: TOnAudioNew);
begin
  FOnAudioNew := Value;
end;

procedure TCustomGroupEvents.SetOnBoardPostDelete(const Value: TOnCommentDelete);
begin
  FOnBoardPostDelete := Value;
end;

procedure TCustomGroupEvents.SetOnBoardPostEdit(const Value: TOnCommentAction);
begin
  FOnBoardPostEdit := Value;
end;

procedure TCustomGroupEvents.SetOnBoardPostNew(const Value: TOnCommentAction);
begin
  FOnBoardPostNew := Value;
end;

procedure TCustomGroupEvents.SetOnBoardPostRestore(const Value: TOnCommentAction);
begin
  FOnBoardPostRestore := Value;
end;

procedure TCustomGroupEvents.SetOnGroupJoin(const Value: TOnGroupJoin);
begin
  FOnGroupJoin := Value;
end;

procedure TCustomGroupEvents.SetOnGroupLeave(const Value: TOnGroupLeave);
begin
  FOnGroupLeave := Value;
end;

procedure TCustomGroupEvents.SetOnMarketCommentDelete(const Value: TOnCommentDelete);
begin
  FOnMarketCommentDelete := Value;
end;

procedure TCustomGroupEvents.SetOnMarketCommentEdit(const Value: TOnCommentAction);
begin
  FOnMarketCommentEdit := Value;
end;

procedure TCustomGroupEvents.SetOnMarketCommentNew(const Value: TOnCommentAction);
begin
  FOnMarketCommentNew := Value;
end;

procedure TCustomGroupEvents.SetOnMarketCommentRestore(const Value: TOnCommentAction);
begin
  FOnMarketCommentRestore := Value;
end;

procedure TCustomGroupEvents.SetOnMessageAllow(const Value: TOnGroupMessageAccess);
begin
  FOnMessageAllow := Value;
end;

procedure TCustomGroupEvents.SetOnMessageDeny(const Value: TOnGroupMessageAccess);
begin
  FOnMessageDeny := Value;
end;

procedure TCustomGroupEvents.SetOnMessageEdit(const Value: TOnGroupMessageAction);
begin
  FOnMessageEdit := Value;
end;

procedure TCustomGroupEvents.SetOnMessageNew(const Value: TOnGroupMessageNew);
begin
  FOnMessageNew := Value;
end;

procedure TCustomGroupEvents.SetOnMessageReply(const Value: TOnGroupMessageAction);
begin
  FOnMessageReply := Value;
end;

procedure TCustomGroupEvents.SetOnPhotoCommentDelete(const Value: TOnCommentDelete);
begin
  FOnPhotoCommentDelete := Value;
end;

procedure TCustomGroupEvents.SetOnPhotoCommentEdit(const Value: TOnCommentAction);
begin
  FOnPhotoCommentEdit := Value;
end;

procedure TCustomGroupEvents.SetOnPhotoCommentNew(const Value: TOnCommentAction);
begin
  FOnPhotoCommentNew := Value;
end;

procedure TCustomGroupEvents.SetOnPhotoCommentRestore(const Value: TOnCommentAction);
begin
  FOnPhotoCommentRestore := Value;
end;

procedure TCustomGroupEvents.SetOnPhotoNew(const Value: TOnPhotoNew);
begin
  FOnPhotoNew := Value;
end;

procedure TCustomGroupEvents.SetOnUserBlock(const Value: TOnGroupUserBlock);
begin
  FOnUserBlock := Value;
end;

procedure TCustomGroupEvents.SetOnUserUnBlock(const Value: TOnGroupUserUnBlock);
begin
  FOnUserUnBlock := Value;
end;

procedure TCustomGroupEvents.SetOnVideoCommentDelete(const Value: TOnCommentDelete);
begin
  FOnVideoCommentDelete := Value;
end;

procedure TCustomGroupEvents.SetOnVideoCommentEdit(const Value: TOnCommentAction);
begin
  FOnVideoCommentEdit := Value;
end;

procedure TCustomGroupEvents.SetOnVideoCommentNew(const Value: TOnCommentAction);
begin
  FOnVideoCommentNew := Value;
end;

procedure TCustomGroupEvents.SetOnVideoCommentRestore(const Value: TOnCommentAction);
begin
  FOnVideoCommentRestore := Value;
end;

procedure TCustomGroupEvents.SetOnVideoNew(const Value: TOnVideoNew);
begin
  FOnVideoNew := Value;
end;

procedure TCustomGroupEvents.SetOnWallPostNew(const Value: TOnWallPostAction);
begin
  FOnWallPostNew := Value;
end;

procedure TCustomGroupEvents.SetOnWallReplyDelete(const Value: TOnCommentDelete);
begin
  FOnWallReplyDelete := Value;
end;

procedure TCustomGroupEvents.SetOnWallReplyEdit(const Value: TOnCommentAction);
begin
  FOnWallReplyEdit := Value;
end;

procedure TCustomGroupEvents.SetOnWallReplyNew(const Value: TOnCommentAction);
begin
  FOnWallReplyNew := Value;
end;

procedure TCustomGroupEvents.SetOnWallReplyRestore(const Value: TOnCommentAction);
begin
  FOnWallReplyRestore := Value;
end;

procedure TCustomGroupEvents.SetOnWallRepost(const Value: TOnWallPostAction);
begin
  FOnWallRepost := Value;
end;

procedure TCustomGroupEvents.SetVK(const Value: TCustomVK);
begin
  FVK := Value;
end;

function TCustomGroupEvents.Start: Boolean;
begin
  if not Assigned(FVK) then
    raise Exception.Create('Для работы необходим VK контроллер (Свойство VK)');
  if FGroupID >= 0 then
    raise Exception.Create('Идентификатор группы не указан или указан не верно');
  FLongPollServer.Client := FVK.Handler.RESTClient;
  FLongPollServer.Method := 'groups.getLongPollServer';
  FLongPollServer.Params := [['lp_version', '3'], ['group_id', (FGroupID *  - 1).ToString]];
  Result := FLongPollServer.Start;
  if Result then
    FVK.DoLog(FLongPollServer, FGroupID.ToString + ' started')
  else
    FVK.DoLog(FLongPollServer, FGroupID.ToString + ' not start');
end;

procedure TCustomGroupEvents.Stop;
begin
  FLongPollServer.Stop;
end;

{ TCustomGroupEventControl }

function TCustomGroupEventControl.Add(GroupId: Integer): TCustomGroupEvents;
begin
  Result := TCustomGroupEvents.Create(Self);
  FItems.Add(Result);
  Result.GroupID := GroupId;
  Result.VK := FVK;
  Result.OnWallReplyNew := FOnWallReplyNew;
  Result.OnWallReplyEdit := FOnWallReplyEdit;
  Result.OnWallReplyRestore := FOnWallReplyRestore;
  Result.OnWallReplyDelete := FOnWallReplyDelete;
  Result.OnWallPostNew := FOnWallPostNew;
  Result.OnWallRepost := FOnWallRepost;
  Result.OnAudioNew := FOnAudioNew;
  Result.OnVideoNew := FOnVideoNew;
  Result.OnVideoCommentNew := FOnVideoCommentNew;
  Result.OnVideoCommentEdit := FOnVideoCommentEdit;
  Result.OnVideoCommentRestore := FOnVideoCommentRestore;
  Result.OnVideoCommentDelete := FOnVideoCommentDelete;
  Result.OnPhotoNew := FOnPhotoNew;
  Result.OnPhotoCommentNew := FOnPhotoCommentNew;
  Result.OnPhotoCommentEdit := FOnPhotoCommentEdit;
  Result.OnPhotoCommentRestore := FOnPhotoCommentRestore;
  Result.OnPhotoCommentDelete := FOnPhotoCommentDelete;
  Result.OnMessageNew := FOnMessageNew;
  Result.OnMessageReply := FOnMessageReply;
  Result.OnMessageEdit := FOnMessageEdit;
  Result.OnMessageAllow := FOnMessageAllow;
  Result.OnMessageDeny := FOnMessageDeny;
  Result.OnBoardPostNew := FOnBoardPostNew;
  Result.OnBoardPostEdit := FOnBoardPostEdit;
  Result.OnBoardPostRestore := FOnBoardPostRestore;
  Result.OnBoardPostDelete := FOnBoardPostDelete;
  Result.OnMarketCommentNew := FOnMarketCommentNew;
  Result.OnMarketCommentEdit := FOnMarketCommentEdit;
  Result.OnMarketCommentRestore := FOnMarketCommentRestore;
  Result.OnMarketCommentDelete := FOnMarketCommentDelete;
  Result.OnGroupLeave := FOnGroupLeave;
  Result.OnGroupJoin := FOnGroupJoin;
  Result.OnUserBlock := FOnUserBlock;
  Result.OnUserUnBlock := FOnUserUnBlock;
end;

constructor TCustomGroupEventControl.Create(AOwner: TComponent);
begin
  inherited;
  FReaded := False;
  FItems := TGroupEventsItems.Create;
  FGroups := TStringList.Create;
end;

procedure TCustomGroupEventControl.DefineProperties(Filer: TFiler);
begin
  inherited;
  if (FGroups.Count > 0) and (not FReaded) then
  begin
    FillFromList;
    FReaded := True;
  end;
end;

destructor TCustomGroupEventControl.Destroy;
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
  begin
    FItems[i].Free;
  end;
  FItems.Free;
  FGroups.Free;
  inherited;
end;

procedure TCustomGroupEventControl.FillFromList;
var
  i, Id: Integer;
begin
  for i := 0 to FGroups.Count - 1 do
    if TryStrToInt(FGroups[i], Id) then
      if Id < 0 then
        Add(Id);
end;

procedure TCustomGroupEventControl.SetGroups(const Value: TStrings);
begin
  FGroups.Assign(Value);
end;

procedure TCustomGroupEventControl.SetItems(const Value: TGroupEventsItems);
begin
  FItems := Value;
end;

procedure TCustomGroupEventControl.SetOnAudioNew(const Value: TOnAudioNew);
begin
  FOnAudioNew := Value;
end;

procedure TCustomGroupEventControl.SetOnBoardPostDelete(const Value: TOnCommentDelete);
begin
  FOnBoardPostDelete := Value;
end;

procedure TCustomGroupEventControl.SetOnBoardPostEdit(const Value: TOnCommentAction);
begin
  FOnBoardPostEdit := Value;
end;

procedure TCustomGroupEventControl.SetOnBoardPostNew(const Value: TOnCommentAction);
begin
  FOnBoardPostNew := Value;
end;

procedure TCustomGroupEventControl.SetOnBoardPostRestore(const Value: TOnCommentAction);
begin
  FOnBoardPostRestore := Value;
end;

procedure TCustomGroupEventControl.SetOnGroupJoin(const Value: TOnGroupJoin);
begin
  FOnGroupJoin := Value;
end;

procedure TCustomGroupEventControl.SetOnGroupLeave(const Value: TOnGroupLeave);
begin
  FOnGroupLeave := Value;
end;

procedure TCustomGroupEventControl.SetOnMarketCommentDelete(const Value: TOnCommentDelete);
begin
  FOnMarketCommentDelete := Value;
end;

procedure TCustomGroupEventControl.SetOnMarketCommentEdit(const Value: TOnCommentAction);
begin
  FOnMarketCommentEdit := Value;
end;

procedure TCustomGroupEventControl.SetOnMarketCommentNew(const Value: TOnCommentAction);
begin
  FOnMarketCommentNew := Value;
end;

procedure TCustomGroupEventControl.SetOnMarketCommentRestore(const Value: TOnCommentAction);
begin
  FOnMarketCommentRestore := Value;
end;

procedure TCustomGroupEventControl.SetOnMessageAllow(const Value: TOnGroupMessageAccess);
begin
  FOnMessageAllow := Value;
end;

procedure TCustomGroupEventControl.SetOnMessageDeny(const Value: TOnGroupMessageAccess);
begin
  FOnMessageDeny := Value;
end;

procedure TCustomGroupEventControl.SetOnMessageEdit(const Value: TOnGroupMessageAction);
begin
  FOnMessageEdit := Value;
end;

procedure TCustomGroupEventControl.SetOnMessageNew(const Value: TOnGroupMessageNew);
begin
  FOnMessageNew := Value;
end;

procedure TCustomGroupEventControl.SetOnMessageReply(const Value: TOnGroupMessageAction);
begin
  FOnMessageReply := Value;
end;

procedure TCustomGroupEventControl.SetOnPhotoCommentDelete(const Value: TOnCommentDelete);
begin
  FOnPhotoCommentDelete := Value;
end;

procedure TCustomGroupEventControl.SetOnPhotoCommentEdit(const Value: TOnCommentAction);
begin
  FOnPhotoCommentEdit := Value;
end;

procedure TCustomGroupEventControl.SetOnPhotoCommentNew(const Value: TOnCommentAction);
begin
  FOnPhotoCommentNew := Value;
end;

procedure TCustomGroupEventControl.SetOnPhotoCommentRestore(const Value: TOnCommentAction);
begin
  FOnPhotoCommentRestore := Value;
end;

procedure TCustomGroupEventControl.SetOnPhotoNew(const Value: TOnPhotoNew);
begin
  FOnPhotoNew := Value;
end;

procedure TCustomGroupEventControl.SetOnUserBlock(const Value: TOnGroupUserBlock);
begin
  FOnUserBlock := Value;
end;

procedure TCustomGroupEventControl.SetOnUserUnBlock(const Value: TOnGroupUserUnBlock);
begin
  FOnUserUnBlock := Value;
end;

procedure TCustomGroupEventControl.SetOnVideoCommentDelete(const Value: TOnCommentDelete);
begin
  FOnVideoCommentDelete := Value;
end;

procedure TCustomGroupEventControl.SetOnVideoCommentEdit(const Value: TOnCommentAction);
begin
  FOnVideoCommentEdit := Value;
end;

procedure TCustomGroupEventControl.SetOnVideoCommentNew(const Value: TOnCommentAction);
begin
  FOnVideoCommentNew := Value;
end;

procedure TCustomGroupEventControl.SetOnVideoCommentRestore(const Value: TOnCommentAction);
begin
  FOnVideoCommentRestore := Value;
end;

procedure TCustomGroupEventControl.SetOnVideoNew(const Value: TOnVideoNew);
begin
  FOnVideoNew := Value;
end;

procedure TCustomGroupEventControl.SetOnWallPostNew(const Value: TOnWallPostAction);
begin
  FOnWallPostNew := Value;
end;

procedure TCustomGroupEventControl.SetOnWallReplyDelete(const Value: TOnCommentDelete);
begin
  FOnWallReplyDelete := Value;
end;

procedure TCustomGroupEventControl.SetOnWallReplyEdit(const Value: TOnCommentAction);
begin
  FOnWallReplyEdit := Value;
end;

procedure TCustomGroupEventControl.SetOnWallReplyNew(const Value: TOnCommentAction);
begin
  FOnWallReplyNew := Value;
end;

procedure TCustomGroupEventControl.SetOnWallReplyRestore(const Value: TOnCommentAction);
begin
  FOnWallReplyRestore := Value;
end;

procedure TCustomGroupEventControl.SetOnWallRepost(const Value: TOnWallPostAction);
begin
  FOnWallRepost := Value;
end;

procedure TCustomGroupEventControl.SetVK(const Value: TCustomVK);
var
  i: Integer;
begin
  FVK := Value;
  for i := 0 to FItems.Count - 1 do
    FItems[i].VK := FVK;
end;

function TCustomGroupEventControl.Start: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FItems.Count - 1 do
    Result := FItems[i].Start or Result;
end;

procedure TCustomGroupEventControl.Stop;
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    FItems[i].Stop;
end;

end.

