unit VK.Components;

interface

uses
  System.SysUtils, System.Classes, VK.API, VK.UserEvents, VK.GroupEvents;

type
  TVK = class(TCustomVK)
  published
    property AppID;
    property AppKey;
    property EndPoint;    // default 'https://oauth.vk.com/authorize';
    property Permissions; // default 'groups,friends,wall,photos,video,docs,notes,market,messages';
    property APIVersion;  // default '5.101';
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
  end;

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

  TVkGroupEvents = class(TCustomGroupEvents)
  published
    property VK;
    property GroupID default 0;
    property OnWallReplyNew;
    property OnWallReplyEdit;
    property OnWallReplyRestore;
    property OnWallReplyDelete;
  end;

  TVkGroupEventsController = class(TCustomGroupEventControl)
  published
    property VK;
    property Groups;
    property OnWallReplyNew;
    property OnWallReplyEdit;
    property OnWallReplyRestore;
    property OnWallReplyDelete;
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

end.

