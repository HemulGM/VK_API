object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 559
  ClientWidth = 1092
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 304
    Top = 0
    Width = 788
    Height = 559
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object LabelLogin: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 782
      Height = 13
      Align = alTop
      Caption = 'Logining...'
      ExplicitWidth = 51
    end
    object PageControl1: TPageControl
      AlignWithMargins = True
      Left = 3
      Top = 39
      Width = 782
      Height = 517
      Margins.Top = 20
      ActivePage = TabSheet10
      Align = alClient
      MultiLine = True
      TabOrder = 0
      object TabSheet9: TTabSheet
        Caption = 'General'
        ImageIndex = 8
        object Button20: TButton
          Left = 3
          Top = 3
          Width = 137
          Height = 25
          Caption = 'UploadAudioMessage'
          TabOrder = 0
          OnClick = Button20Click
        end
        object Button26: TButton
          Left = 2
          Top = 34
          Width = 137
          Height = 25
          Caption = 'UploadAudio'
          TabOrder = 1
          OnClick = Button26Click
        end
        object Button13: TButton
          Left = 146
          Top = 3
          Width = 137
          Height = 25
          Caption = 'LongPollStart'
          TabOrder = 2
          OnClick = Button13Click
        end
        object Button14: TButton
          Left = 145
          Top = 34
          Width = 137
          Height = 25
          Caption = 'LongPollStop'
          TabOrder = 3
          OnClick = Button14Click
        end
        object Button15: TButton
          Left = 289
          Top = 3
          Width = 137
          Height = 25
          Caption = 'GroupLongPollStart'
          TabOrder = 4
          OnClick = Button15Click
        end
        object Button16: TButton
          Left = 289
          Top = 34
          Width = 137
          Height = 25
          Caption = 'GroupLongPollStop'
          TabOrder = 5
          OnClick = Button16Click
        end
        object Button8: TButton
          Left = 145
          Top = 83
          Width = 137
          Height = 25
          Caption = 'CallMethod'
          TabOrder = 6
          OnClick = Button8Click
        end
        object Button34: TButton
          Left = 2
          Top = 83
          Width = 137
          Height = 25
          Caption = 'Search.GetHints'
          TabOrder = 7
          OnClick = Button34Click
        end
        object Button35: TButton
          Left = 1
          Top = 114
          Width = 137
          Height = 25
          Caption = 'Database.GetRegions'
          TabOrder = 8
          OnClick = Button35Click
        end
        object Button36: TButton
          Left = 2
          Top = 145
          Width = 137
          Height = 25
          Caption = 'Database.GetSchoolClasses'
          TabOrder = 9
          OnClick = Button36Click
        end
        object Button37: TButton
          Left = 1
          Top = 176
          Width = 137
          Height = 25
          Caption = 'Storage.Get'
          TabOrder = 10
          OnClick = Button37Click
        end
        object Button38: TButton
          Left = 1
          Top = 207
          Width = 137
          Height = 25
          Caption = 'Secure.GetBalance'
          TabOrder = 11
          OnClick = Button38Click
        end
        object Button39: TButton
          Left = 1
          Top = 238
          Width = 137
          Height = 25
          Caption = 'Stories.Get'
          TabOrder = 12
          OnClick = Button39Click
        end
      end
      object TabSheet1: TTabSheet
        Caption = 'Account'
        object Button1: TButton
          Left = 3
          Top = 3
          Width = 137
          Height = 25
          Caption = 'Ban -1'
          TabOrder = 0
          OnClick = Button1Click
        end
        object Button2: TButton
          Left = 3
          Top = 34
          Width = 137
          Height = 25
          Caption = 'Unban -1'
          TabOrder = 1
          OnClick = Button2Click
        end
        object Button3: TButton
          Left = 3
          Top = 65
          Width = 137
          Height = 25
          Caption = 'ActiveOffers'
          TabOrder = 2
          OnClick = Button3Click
        end
        object Button4: TButton
          Left = 4
          Top = 96
          Width = 137
          Height = 25
          Caption = 'AppPermissions'
          TabOrder = 3
          OnClick = Button4Click
        end
        object Button5: TButton
          Left = 4
          Top = 127
          Width = 137
          Height = 25
          Caption = 'Counters'
          TabOrder = 4
          OnClick = Button5Click
        end
        object Button6: TButton
          Left = 4
          Top = 158
          Width = 137
          Height = 25
          Caption = 'PushSettings'
          TabOrder = 5
          OnClick = Button6Click
        end
        object Button7: TButton
          Left = 4
          Top = 189
          Width = 137
          Height = 25
          Caption = 'SaveProfileInfo'
          TabOrder = 6
          OnClick = Button7Click
        end
        object Button9: TButton
          Left = 3
          Top = 220
          Width = 137
          Height = 25
          Caption = 'Online'
          TabOrder = 7
          OnClick = Button9Click
        end
        object Button10: TButton
          Left = 3
          Top = 251
          Width = 137
          Height = 25
          Caption = 'Offline'
          TabOrder = 8
          OnClick = Button10Click
        end
        object Button40: TButton
          Left = 146
          Top = 3
          Width = 137
          Height = 25
          Caption = 'GetProfileInfo'
          TabOrder = 9
          OnClick = Button40Click
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Auth'
        ImageIndex = 1
        object Button11: TButton
          Left = 3
          Top = 3
          Width = 137
          Height = 25
          Caption = 'CheckPhone'
          TabOrder = 0
          OnClick = Button11Click
        end
        object Button41: TButton
          Left = 3
          Top = 34
          Width = 137
          Height = 25
          Caption = 'Signup'
          TabOrder = 1
          OnClick = Button41Click
        end
      end
      object TabSheet3: TTabSheet
        Caption = 'Audio'
        ImageIndex = 2
        object Button21: TButton
          Left = 3
          Top = 3
          Width = 137
          Height = 25
          Caption = 'Get'
          TabOrder = 0
          OnClick = Button21Click
        end
        object Button23: TButton
          Left = 4
          Top = 34
          Width = 137
          Height = 25
          Caption = 'GetAlbums'
          TabOrder = 1
          OnClick = Button23Click
        end
        object Button24: TButton
          Left = 3
          Top = 65
          Width = 137
          Height = 25
          Caption = 'GetRecoms'
          TabOrder = 2
          OnClick = Button24Click
        end
        object Button29: TButton
          Left = 3
          Top = 96
          Width = 137
          Height = 25
          Caption = 'GetPop'
          TabOrder = 3
          OnClick = Button29Click
        end
        object Button30: TButton
          Left = 3
          Top = 127
          Width = 137
          Height = 25
          Caption = 'GetChart'
          TabOrder = 4
          OnClick = Button30Click
        end
        object ButtonGetCatalog: TButton
          Left = 146
          Top = 3
          Width = 119
          Height = 25
          Caption = 'GetCatalog'
          TabOrder = 5
          OnClick = ButtonGetCatalogClick
        end
        object ButtonCreatePlaylist: TButton
          Left = 147
          Top = 34
          Width = 119
          Height = 25
          Caption = 'CreatePlaylist'
          TabOrder = 6
          OnClick = ButtonCreatePlaylistClick
        end
        object ButtonEditPlaylist: TButton
          Left = 146
          Top = 65
          Width = 119
          Height = 25
          Caption = 'EditPlaylist'
          TabOrder = 7
          OnClick = ButtonEditPlaylistClick
        end
        object Button31: TButton
          Left = 146
          Top = 96
          Width = 119
          Height = 25
          Caption = 'AddToPlaylist'
          TabOrder = 8
          OnClick = Button31Click
        end
        object Button44: TButton
          Left = 3
          Top = 158
          Width = 137
          Height = 25
          Caption = 'GetCount'
          TabOrder = 9
          OnClick = Button44Click
        end
      end
      object TabSheet4: TTabSheet
        Caption = 'Board'
        ImageIndex = 3
        object Button22: TButton
          Left = 3
          Top = 3
          Width = 137
          Height = 25
          Caption = 'CreateComment'
          TabOrder = 0
          OnClick = Button22Click
        end
      end
      object TabSheet5: TTabSheet
        Caption = 'Status'
        ImageIndex = 4
        object Button17: TButton
          Left = 3
          Top = 3
          Width = 137
          Height = 25
          Caption = 'Get'
          TabOrder = 0
          OnClick = Button17Click
        end
        object Button18: TButton
          Left = 3
          Top = 34
          Width = 137
          Height = 25
          Caption = 'Set'
          TabOrder = 1
          OnClick = Button18Click
        end
      end
      object TabSheet6: TTabSheet
        Caption = 'Wall'
        ImageIndex = 5
        object Button19: TButton
          Left = 3
          Top = 3
          Width = 137
          Height = 25
          Caption = 'Post'
          TabOrder = 0
          OnClick = Button19Click
        end
        object ButtonWallGet: TButton
          Left = 3
          Top = 34
          Width = 137
          Height = 25
          Caption = 'Get'
          TabOrder = 1
          OnClick = ButtonWallGetClick
        end
      end
      object TabSheet7: TTabSheet
        Caption = 'Users'
        ImageIndex = 6
        object Button12: TButton
          Left = 3
          Top = 3
          Width = 137
          Height = 25
          Caption = 'Get'
          TabOrder = 0
          OnClick = Button12Click
        end
      end
      object TabSheet8: TTabSheet
        Caption = 'Groups'
        ImageIndex = 7
        object Button25: TButton
          Left = 3
          Top = 3
          Width = 137
          Height = 25
          Caption = 'GetMembers'
          TabOrder = 0
          OnClick = Button25Click
        end
        object Button42: TButton
          Left = 3
          Top = 34
          Width = 137
          Height = 25
          Caption = 'GetById'
          TabOrder = 1
          OnClick = Button42Click
        end
        object Button43: TButton
          Left = 3
          Top = 65
          Width = 137
          Height = 25
          Caption = 'Get'
          TabOrder = 2
          OnClick = Button43Click
        end
      end
      object TabSheet10: TTabSheet
        Caption = 'Messages'
        ImageIndex = 9
        object Button27: TButton
          Left = 3
          Top = 3
          Width = 166
          Height = 25
          Caption = 'GetConversations'
          TabOrder = 0
          OnClick = Button27Click
        end
        object Button46: TButton
          Left = 3
          Top = 34
          Width = 166
          Height = 25
          Caption = 'Get'
          TabOrder = 1
          OnClick = Button46Click
        end
        object ButtonSend: TButton
          Left = 175
          Top = 3
          Width = 75
          Height = 25
          Caption = 'ButtonSend'
          TabOrder = 2
          OnClick = ButtonSendClick
        end
      end
      object TabSheet11: TTabSheet
        Caption = 'Friends'
        ImageIndex = 10
        object Button28: TButton
          Left = 3
          Top = 3
          Width = 137
          Height = 25
          Caption = 'Get'
          TabOrder = 0
          OnClick = Button28Click
        end
        object Button45: TButton
          Left = 3
          Top = 34
          Width = 137
          Height = 25
          Caption = 'GetFriendsWithAudio'
          TabOrder = 1
          OnClick = Button45Click
        end
      end
      object TabSheetPolls: TTabSheet
        Caption = 'Polls'
        ImageIndex = 11
        object Button32: TButton
          Left = 3
          Top = 3
          Width = 137
          Height = 25
          Caption = 'GetBackgrounds'
          TabOrder = 0
          OnClick = Button32Click
        end
      end
      object TabSheetPodcasts: TTabSheet
        Caption = 'Podcasts'
        ImageIndex = 12
        object Button33: TButton
          Left = 3
          Top = 3
          Width = 137
          Height = 25
          Caption = 'Search'
          TabOrder = 0
          OnClick = Button33Click
        end
      end
      object TabSheetNewsfeed: TTabSheet
        Caption = 'Newsfeed'
        ImageIndex = 13
        object Button48: TButton
          Left = 3
          Top = 3
          Width = 75
          Height = 25
          Caption = 'Button48'
          TabOrder = 0
          OnClick = Button48Click
        end
      end
    end
    object ButtonLogin: TButton
      Left = 635
      Top = 9
      Width = 78
      Height = 25
      Caption = 'Login'
      TabOrder = 1
      OnClick = ButtonLoginClick
    end
    object Button47: TButton
      Left = 719
      Top = 9
      Width = 62
      Height = 25
      Caption = 'Logout'
      TabOrder = 2
      OnClick = Button47Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 304
    Height = 559
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Memo1: TMemo
      Left = 0
      Top = 0
      Width = 304
      Height = 360
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object MemoLog: TMemo
      Left = 0
      Top = 360
      Width = 304
      Height = 199
      Align = alBottom
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 1
    end
  end
  object VK1: TVK
    AppID = '6121396'
    BaseURL = 'https://api.vk.com/method'
    EndPoint = 'https://oauth.vk.com/authorize'
    Logging = True
    LogResponse = True
    OnAuth = VK1Auth
    OnError = VK1Error
    OnLog = VK1Log
    OnLogin = VK1Login
    Permissions = [Notify, Friends, Photos, Audio, Video, Stories, Pages, Status, Notes, Wall, Ads, Offline, Docs, Groups, Notifications, Stats, Email, Market, AppWidget, Manage]
    Proxy.Port = 0
    Token = 
      '550f3c12ef5d4c8b1cbc4a90a868e370d641cb24fd1621172582b6dff0da0e33' +
      '63895528b6d9376895a99'
    UsePseudoAsync = True
    Left = 96
    Top = 56
  end
  object VkUserEvents1: TVkUserEvents
    Logging = True
    OnChangeDialogFlags = VkUserEvents1ChangeDialogFlags
    OnChangeMessageFlags = VkUserEvents1ChangeMessageFlags
    OnChatChanged = VkUserEvents1ChatChanged
    OnChatChangeInfo = VkUserEvents1ChatChangeInfo
    OnCountChange = VkUserEvents1CountChange
    OnDeleteMessages = VkUserEvents1DeleteMessages
    OnEditMessage = VkUserEvents1EditMessage
    OnNewMessage = VkUserEvents1NewMessage
    OnNotifyChange = VkUserEvents1NotifyChange
    OnReadMessages = VkUserEvents1ReadMessages
    OnRecoverMessages = VkUserEvents1RecoverMessages
    OnUnhandledEvents = VkUserEvents1UnhandledEvents
    OnUserCall = VkUserEvents1UserCall
    OnUserOffline = VkUserEvents1UserOffline
    OnUserOnline = VkUserEvents1UserOnline
    OnUsersRecording = VkUserEvents1UsersRecording
    OnUsersTyping = VkUserEvents1UsersTyping
    OnUserTyping = VkUserEvents1UserTyping
    Version = '3'
    VK = VK1
    Left = 208
    Top = 56
  end
  object VkGroupEventsController1: TVkGroupEventsController
    Logging = True
    OnAudioNew = VkGroupEventsController1AudioNew
    OnBoardPostDelete = VkGroupEventsController1BoardPostDelete
    OnBoardPostEdit = VkGroupEventsController1BoardPostEdit
    OnBoardPostNew = VkGroupEventsController1BoardPostNew
    OnBoardPostRestore = VkGroupEventsController1BoardPostRestore
    OnGroupAppPayload = VkGroupEventsController1GroupAppPayload
    OnGroupChangePhoto = VkGroupEventsController1GroupChangePhoto
    OnGroupChangeSettings = VkGroupEventsController1GroupChangeSettings
    OnGroupJoin = VkGroupEventsController1GroupJoin
    OnGroupLeave = VkGroupEventsController1GroupLeave
    OnGroupOfficersEdit = VkGroupEventsController1GroupOfficersEdit
    OnGroupPayTransaction = VkGroupEventsController1GroupPayTransaction
    OnGroupPollVoteNew = VkGroupEventsController1GroupPollVoteNew
    OnGroupUnhandledEvents = VkGroupEventsController1GroupUnhandledEvents
    OnMarketCommentDelete = VkGroupEventsController1MarketCommentDelete
    OnMarketCommentEdit = VkGroupEventsController1MarketCommentEdit
    OnMarketCommentNew = VkGroupEventsController1MarketCommentNew
    OnMarketCommentRestore = VkGroupEventsController1MarketCommentRestore
    OnMessageAllow = VkGroupEventsController1MessageAllow
    OnMessageDeny = VkGroupEventsController1MessageDeny
    OnMessageEdit = VkGroupEventsController1MessageEdit
    OnMessageNew = VkGroupEventsController1MessageNew
    OnMessageReply = VkGroupEventsController1MessageReply
    OnMessageTypingState = VkGroupEventsController1MessageTypingState
    OnPhotoCommentDelete = VkGroupEventsController1PhotoCommentDelete
    OnPhotoCommentEdit = VkGroupEventsController1PhotoCommentEdit
    OnPhotoCommentNew = VkGroupEventsController1PhotoCommentNew
    OnPhotoCommentRestore = VkGroupEventsController1PhotoCommentRestore
    OnPhotoNew = VkGroupEventsController1PhotoNew
    OnUserBlock = VkGroupEventsController1UserBlock
    OnUserUnBlock = VkGroupEventsController1UserUnBlock
    OnVideoCommentDelete = VkGroupEventsController1VideoCommentDelete
    OnVideoCommentEdit = VkGroupEventsController1VideoCommentEdit
    OnVideoCommentNew = VkGroupEventsController1VideoCommentNew
    OnVideoCommentRestore = VkGroupEventsController1VideoCommentRestore
    OnVideoNew = VkGroupEventsController1VideoNew
    OnWallPostNew = VkGroupEventsController1WallPostNew
    OnWallReplyDelete = VkGroupEventsController1WallReplyDelete
    OnWallReplyEdit = VkGroupEventsController1WallReplyEdit
    OnWallReplyNew = VkGroupEventsController1WallReplyNew
    OnWallReplyRestore = VkGroupEventsController1WallReplyRestore
    OnWallRepost = VkGroupEventsController1WallRepost
    Version = '3'
    VK = VK1
    Left = 96
    Top = 120
  end
  object VkGroupEvents1: TVkGroupEvents
    Logging = True
    Version = '3'
    VK = VK1
    Left = 208
    Top = 120
  end
end
