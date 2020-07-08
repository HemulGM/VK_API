unit VK.Components;

interface

uses
  System.SysUtils, System.Classes, VK.GroupEvents,
  {$IFDEF NEEDFMX}
  FMX.Types,
  {$ELSE}
  System.Types,
  {$ENDIF}  VK.API, VK.UserEvents;

type
  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TVK = class(TCustomVK)
  published
    property AppID;
    property AppKey;
    property EndPoint;    // default 'https://oauth.vk.com/authorize';
    property Permissions; // default 'groups,friends,wall,photos,video,docs,notes,market';
    property APIVersion;  // default '5.103';
    property BaseURL;     // default 'https://api.vk.com/method';
    property ServiceKey;
    property UseServiceKeyOnly default False;
    property OnAuth;
    property OnLogin;
    property OnLog;
    property OnError;
    property OnErrorLogin;
    property OnCaptcha;
    property OnConfirm;
    property Logging;
    property TestMode default False;
  end;

  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TVkUserEvents = class(TCustomUserEvents)
  published
    property VK;
    property OnNewMessage;
    property OnEditMessage;
    property OnUserOnline;
    property OnUserOffline;
    property OnChangeMessageFlags;
    property OnChangeDialogFlags;
    property OnReadMessages;
    property OnRecoverMessages;
    property OnDeleteMessages;
    property OnChatChanged;
    property OnChatChangeInfo;
    property OnUserTyping;
    property OnUsersTyping;
    property OnUsersRecording;
    property OnUserCall;
    property OnCountChange;
    property OnNotifyChange;
  end;

  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TVkGroupEvents = class(TCustomGroupEvents)
  private
    FActualVersion: string;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ActualVersion: string read FActualVersion;
    property VK;
    property GroupID default 0;
    property OnWallReplyNew;
    property OnWallReplyEdit;
    property OnWallReplyRestore;
    property OnWallReplyDelete;
    property OnWallPostNew;
    property OnWallRepost;
    property OnAudioNew;
    property OnVideoNew;
    property OnVideoCommentNew;
    property OnVideoCommentEdit;
    property OnVideoCommentRestore;
    property OnVideoCommentDelete;
    property OnPhotoNew;
    property OnPhotoCommentNew;
    property OnPhotoCommentEdit;
    property OnPhotoCommentRestore;
    property OnPhotoCommentDelete;
    property OnMessageNew;
    property OnMessageReply;
    property OnMessageEdit;
    property OnMessageAllow;
    property OnMessageDeny;
    property OnMessageTypingState;
    property OnBoardPostNew;
    property OnBoardPostEdit;
    property OnBoardPostRestore;
    property OnBoardPostDelete;
    property OnMarketCommentNew;
    property OnMarketCommentEdit;
    property OnMarketCommentRestore;
    property OnMarketCommentDelete;
    property OnGroupLeave;
    property OnGroupJoin;
    property OnUserBlock;
    property OnUserUnBlock;
    property OnGroupPollVoteNew;
    property OnGroupOfficersEdit;
    property OnGroupChangeSettings;
    property OnGroupChangePhoto;
    property OnGroupPayTransaction;
    property OnGroupAppPayload;
  end;

  [ComponentPlatformsAttribute(pidAllPlatforms)]
  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TVkGroupEventsController = class(TCustomGroupEventControl)
  private
    FActualVersion: string;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ActualVersion: string read FActualVersion;
    property VK;
    property Groups;
    property OnWallReplyNew;
    property OnWallReplyEdit;
    property OnWallReplyRestore;
    property OnWallReplyDelete;
    property OnWallPostNew;
    property OnWallRepost;
    property OnAudioNew;
    property OnVideoNew;
    property OnVideoCommentNew;
    property OnVideoCommentEdit;
    property OnVideoCommentRestore;
    property OnVideoCommentDelete;
    property OnPhotoNew;
    property OnPhotoCommentNew;
    property OnPhotoCommentEdit;
    property OnPhotoCommentRestore;
    property OnPhotoCommentDelete;
    property OnMessageNew;
    property OnMessageReply;
    property OnMessageEdit;
    property OnMessageAllow;
    property OnMessageDeny;
    property OnMessageTypingState;
    property OnBoardPostNew;
    property OnBoardPostEdit;
    property OnBoardPostRestore;
    property OnBoardPostDelete;
    property OnMarketCommentNew;
    property OnMarketCommentEdit;
    property OnMarketCommentRestore;
    property OnMarketCommentDelete;
    property OnGroupLeave;
    property OnGroupJoin;
    property OnUserBlock;
    property OnUserUnBlock;
    property OnGroupPollVoteNew;
    property OnGroupOfficersEdit;
    property OnGroupChangeSettings;
    property OnGroupChangePhoto;
    property OnGroupPayTransaction;
    property OnGroupAppPayload;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('VK API HGM', [TVK]);
  RegisterComponents('VK API HGM', [TVkUserEvents]);
  RegisterComponents('VK API HGM', [TVkGroupEvents]);
  RegisterComponents('VK API HGM', [TVkGroupEventsController]);
end;

{ TVkGroupEventsController }

constructor TVkGroupEventsController.Create(AOwner: TComponent);
begin
  inherited;
  FActualVersion := '1.503';
end;

{ TVkGroupEvents }

constructor TVkGroupEvents.Create(AOwner: TComponent);
begin
  inherited;
  FActualVersion := '1.503';
end;

end.

