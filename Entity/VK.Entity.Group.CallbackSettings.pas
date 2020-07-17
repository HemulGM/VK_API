unit VK.Entity.Group.CallbackSettings;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkCallbackSetting = class
  private
    FAudio_new: Boolean;
    FBoard_post_delete: Boolean;
    FBoard_post_edit: Boolean;
    FBoard_post_new: Boolean;
    FBoard_post_restore: Boolean;
    FGroup_change_photo: Boolean;
    FGroup_change_settings: Boolean;
    FGroup_join: Boolean;
    FGroup_leave: Boolean;
    FGroup_officers_edit: Boolean;
    FMarket_comment_delete: Boolean;
    FMarket_comment_edit: Boolean;
    FMarket_comment_new: Boolean;
    FMarket_comment_restore: Boolean;
    FMessage_allow: Boolean;
    FMessage_deny: Boolean;
    FMessage_new: Boolean;
    FMessage_reply: Boolean;
    FMessage_typing_state: Boolean;
    FMessages_edit: Boolean;
    FPhoto_comment_delete: Boolean;
    FPhoto_comment_edit: Boolean;
    FPhoto_comment_new: Boolean;
    FPhoto_comment_restore: Boolean;
    FPhoto_new: Boolean;
    FPoll_vote_new: Boolean;
    FUser_block: Boolean;
    FUser_unblock: Boolean;
    FVideo_comment_delete: Boolean;
    FVideo_comment_edit: Boolean;
    FVideo_comment_new: Boolean;
    FVideo_comment_restore: Boolean;
    FVideo_new: Boolean;
    FWall_post_new: Boolean;
    FWall_reply_delete: Boolean;
    FWall_reply_edit: Boolean;
    FWall_reply_new: Boolean;
    FWall_reply_restore: Boolean;
    FWall_repost: Boolean;
  public
    property AudioNew: Boolean read FAudio_new write FAudio_new;
    property BoardPostDelete: Boolean read FBoard_post_delete write FBoard_post_delete;
    property BoardPostEdit: Boolean read FBoard_post_edit write FBoard_post_edit;
    property BoardPostNew: Boolean read FBoard_post_new write FBoard_post_new;
    property BoardPostRestore: Boolean read FBoard_post_restore write FBoard_post_restore;
    property GroupChangePhoto: Boolean read FGroup_change_photo write FGroup_change_photo;
    property GroupChangeSettings: Boolean read FGroup_change_settings write FGroup_change_settings;
    property GroupJoin: Boolean read FGroup_join write FGroup_join;
    property GroupLeave: Boolean read FGroup_leave write FGroup_leave;
    property GroupOfficersEdit: Boolean read FGroup_officers_edit write FGroup_officers_edit;
    property MarketCommentDelete: Boolean read FMarket_comment_delete write FMarket_comment_delete;
    property MarketCommentEdit: Boolean read FMarket_comment_edit write FMarket_comment_edit;
    property MarketCommentNew: Boolean read FMarket_comment_new write FMarket_comment_new;
    property MarketCommentRestore: Boolean read FMarket_comment_restore write FMarket_comment_restore;
    property MessageAllow: Boolean read FMessage_allow write FMessage_allow;
    property MessageDeny: Boolean read FMessage_deny write FMessage_deny;
    property MessageNew: Boolean read FMessage_new write FMessage_new;
    property MessageReply: Boolean read FMessage_reply write FMessage_reply;
    property MessageTypingState: Boolean read FMessage_typing_state write FMessage_typing_state;
    property MessagesEdit: Boolean read FMessages_edit write FMessages_edit;
    property photoCommentDelete: Boolean read FPhoto_comment_delete write FPhoto_comment_delete;
    property photoCommentEdit: Boolean read FPhoto_comment_edit write FPhoto_comment_edit;
    property PhotoCommentNew: Boolean read FPhoto_comment_new write FPhoto_comment_new;
    property PhotoCommentRestore: Boolean read FPhoto_comment_restore write FPhoto_comment_restore;
    property PhotoNew: Boolean read FPhoto_new write FPhoto_new;
    property PollVoteNew: Boolean read FPoll_vote_new write FPoll_vote_new;
    property UserBlock: Boolean read FUser_block write FUser_block;
    property UserUnblock: Boolean read FUser_unblock write FUser_unblock;
    property VideoCommentDelete: Boolean read FVideo_comment_delete write FVideo_comment_delete;
    property VideoCommentEdit: Boolean read FVideo_comment_edit write FVideo_comment_edit;
    property VideoCommentNew: Boolean read FVideo_comment_new write FVideo_comment_new;
    property VideoCommentRestore: Boolean read FVideo_comment_restore write FVideo_comment_restore;
    property VideoNew: Boolean read FVideo_new write FVideo_new;
    property WallPostNew: Boolean read FWall_post_new write FWall_post_new;
    property WallReplyDelete: Boolean read FWall_reply_delete write FWall_reply_delete;
    property WallReplyEdit: Boolean read FWall_reply_edit write FWall_reply_edit;
    property WallReplyNew: Boolean read FWall_reply_new write FWall_reply_new;
    property WallReplyRestore: Boolean read FWall_reply_restore write FWall_reply_restore;
    property WallRepost: Boolean read FWall_repost write FWall_repost;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCallbackSetting;
  end;

  TVkCallbackSettings = class
  private
    FApi_version: string;
    FEvents: TVkCallbackSetting;
  public
    property ApiVersion: string read FApi_version write FApi_version;
    property Events: TVkCallbackSetting read FEvents write FEvents;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCallbackSettings;
  end;

implementation

{TVkCallbackSetting}

function TVkCallbackSetting.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCallbackSetting.FromJsonString(AJsonString: string): TVkCallbackSetting;
begin
  result := TJson.JsonToObject<TVkCallbackSetting>(AJsonString)
end;

{TVkCallbackSettings}

constructor TVkCallbackSettings.Create;
begin
  inherited;
  FEvents := TVkCallbackSetting.Create();
end;

destructor TVkCallbackSettings.Destroy;
begin
  FEvents.Free;
  inherited;
end;

function TVkCallbackSettings.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCallbackSettings.FromJsonString(AJsonString: string): TVkCallbackSettings;
begin
  result := TJson.JsonToObject<TVkCallbackSettings>(AJsonString)
end;

end.

