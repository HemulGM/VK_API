﻿unit VK.Components;

interface

uses
  System.SysUtils, System.Classes, VK.API, VK.Types, VK.GroupEvents,
  VK.UserEvents;

type
  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TVK = class(TCustomVK)
  published
    property APIVersion;  // readonly
    property AppID;       // default empty
    property AppKey;      // default empty
    property BaseURL;     // default 'https://api.vk.com/method';
    property EndPoint;    // default 'https://oauth.vk.com/authorize';
    property Lang default TVkLang.Auto;
    property Logging default False;
    property LogResponse default False;
    property OnAuth;
    property OnCaptcha;
    property OnConfirm;
    property OnError;
    property OnLog;
    property OnLogin;
    property OnNeedGeoLocation;
    property Permissions nodefault; // default 'groups,friends,wall,photos,video,docs,notes,market';
    property Proxy;
    property ServiceKey;  // default empty
    property TestMode default False;
    property Token;
    property UseServiceKeyOnly default False;
    property RequestLimit default 3;
  end;

  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TVkUserEvents = class(TCustomUserEvents)
  private
    function GetActualVersion: string;
  published
    property ActualVersion: string read GetActualVersion;
    /// <description>
    /// События вызываются асинхронно (без синхронизации)
    /// </description>
    property Async default False;
    property Logging default False;
    property OnChangeConversationMajorId;
    property OnChangeConversationMinorId;
    property OnChangeDialogFlags;
    property OnChangeMessageFlags;
    property OnChatChanged;
    property OnChatChangeInfo;
    property OnCountChange;
    property OnDeleteMessages;
    property OnEditMessage;
    property OnNewMessage;
    property OnNotifyChange;
    property OnReadMessages;
    property OnRecoverMessages;
    property OnUnhandledEvents;
    property OnUserCall;
    property OnUserOffline;
    property OnUserOnline;
    property OnUsersRecording;
    property OnUsersTyping;
    property OnUserTyping;
    property Version;
    property VK;
  end;

  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TVkGroupEvents = class(TCustomGroupEvents)
  private
    function GetActualVersion: string;
  published
    property ActualVersion: string read GetActualVersion;
    property Async default False;
    property GroupID default 0;
    property Logging default False;
    property OnAudioNew;
    property OnBoardPostDelete;
    property OnBoardPostEdit;
    property OnBoardPostNew;
    property OnBoardPostRestore;
    property OnGroupAppPayload;
    property OnGroupChangePhoto;
    property OnGroupChangeSettings;
    property OnGroupJoin;
    property OnGroupLeave;
    property OnGroupOfficersEdit;
    property OnGroupPayTransaction;
    property OnGroupPollVoteNew;
    property OnGroupUnhandledEvents;
    property OnMarketCommentDelete;
    property OnMarketCommentEdit;
    property OnMarketCommentNew;
    property OnMarketCommentRestore;
    property OnMessageAllow;
    property OnMessageDeny;
    property OnMessageEdit;
    property OnMessageNew;
    property OnMessageReply;
    property OnMessageTypingState;
    property OnPhotoCommentDelete;
    property OnPhotoCommentEdit;
    property OnPhotoCommentNew;
    property OnPhotoCommentRestore;
    property OnPhotoNew;
    property OnUserBlock;
    property OnUserUnBlock;
    property OnVideoCommentDelete;
    property OnVideoCommentEdit;
    property OnVideoCommentNew;
    property OnVideoCommentRestore;
    property OnVideoNew;
    property OnWallPostNew;
    property OnWallReplyDelete;
    property OnWallReplyEdit;
    property OnWallReplyNew;
    property OnWallReplyRestore;
    property OnWallRepost;
    property Version;
    property VK;
  end;

  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TVkGroupEventsController = class(TCustomGroupEventControl)
  private
    function GetActualVersion: string;
  published
    property ActualVersion: string read GetActualVersion;
    property Groups;
    property Logging default False;
    property OnAudioNew;
    property OnBoardPostDelete;
    property OnBoardPostEdit;
    property OnBoardPostNew;
    property OnBoardPostRestore;
    property OnGroupAppPayload;
    property OnGroupChangePhoto;
    property OnGroupChangeSettings;
    property OnGroupJoin;
    property OnGroupLeave;
    property OnGroupOfficersEdit;
    property OnGroupPayTransaction;
    property OnGroupPollVoteNew;
    property OnGroupUnhandledEvents;
    property OnMarketCommentDelete;
    property OnMarketCommentEdit;
    property OnMarketCommentNew;
    property OnMarketCommentRestore;
    property OnMessageAllow;
    property OnMessageDeny;
    property OnMessageEdit;
    property OnMessageNew;
    property OnMessageReply;
    property OnMessageTypingState;
    property OnPhotoCommentDelete;
    property OnPhotoCommentEdit;
    property OnPhotoCommentNew;
    property OnPhotoCommentRestore;
    property OnPhotoNew;
    property OnUserBlock;
    property OnUserUnBlock;
    property OnVideoCommentDelete;
    property OnVideoCommentEdit;
    property OnVideoCommentNew;
    property OnVideoCommentRestore;
    property OnVideoNew;
    property OnWallPostNew;
    property OnWallReplyDelete;
    property OnWallReplyEdit;
    property OnWallReplyNew;
    property OnWallReplyRestore;
    property OnWallRepost;
    property Version;
    property VK;
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

{ TVkUserEvents }

function TVkUserEvents.GetActualVersion: string;
begin
  Result := TVK.Version;
end;

{ TVkGroupEvents }

function TVkGroupEvents.GetActualVersion: string;
begin
  Result := TVK.Version;
end;

{ TVkGroupEventsController }

function TVkGroupEventsController.GetActualVersion: string;
begin
  Result := TVK.Version;
end;

end.

