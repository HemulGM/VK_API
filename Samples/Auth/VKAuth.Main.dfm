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
      Left = 0
      Top = 19
      Width = 788
      Height = 540
      ActivePage = TabSheet9
      Align = alClient
      TabOrder = 0
      object TabSheet9: TTabSheet
        Caption = 'General'
        ImageIndex = 8
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
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
          Top = 96
          Width = 137
          Height = 25
          Caption = 'CallMethod'
          TabOrder = 6
          OnClick = Button8Click
        end
        object Button34: TButton
          Left = 3
          Top = 147
          Width = 137
          Height = 25
          Caption = 'Search.GetHints'
          TabOrder = 7
          OnClick = Button34Click
        end
        object Button35: TButton
          Left = 2
          Top = 178
          Width = 137
          Height = 25
          Caption = 'Database.GetRegions'
          TabOrder = 8
          OnClick = Button35Click
        end
        object Button36: TButton
          Left = 3
          Top = 209
          Width = 137
          Height = 25
          Caption = 'Database.GetSchoolClasses'
          TabOrder = 9
          OnClick = Button36Click
        end
        object Button37: TButton
          Left = 2
          Top = 240
          Width = 137
          Height = 25
          Caption = 'Storage.Get'
          TabOrder = 10
          OnClick = Button37Click
        end
        object Button38: TButton
          Left = 2
          Top = 271
          Width = 137
          Height = 25
          Caption = 'Secure.GetBalance'
          TabOrder = 11
          OnClick = Button38Click
        end
        object Button39: TButton
          Left = 2
          Top = 302
          Width = 137
          Height = 25
          Caption = 'Stories.Get'
          TabOrder = 12
          OnClick = Button39Click
        end
      end
      object TabSheet1: TTabSheet
        Caption = 'Account'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Button1: TButton
          Left = 3
          Top = 3
          Width = 137
          Height = 25
          Caption = 'Account.Ban -1'
          TabOrder = 0
          OnClick = Button1Click
        end
        object Button2: TButton
          Left = 3
          Top = 34
          Width = 137
          Height = 25
          Caption = 'Account.Unban -1'
          TabOrder = 1
          OnClick = Button2Click
        end
        object Button3: TButton
          Left = 3
          Top = 65
          Width = 137
          Height = 25
          Caption = 'Account.ActiveOffers'
          TabOrder = 2
          OnClick = Button3Click
        end
        object Button4: TButton
          Left = 4
          Top = 96
          Width = 137
          Height = 25
          Caption = 'Account.AppPermissions'
          TabOrder = 3
          OnClick = Button4Click
        end
        object Button5: TButton
          Left = 4
          Top = 127
          Width = 137
          Height = 25
          Caption = 'Account.Counters'
          TabOrder = 4
          OnClick = Button5Click
        end
        object Button6: TButton
          Left = 4
          Top = 158
          Width = 137
          Height = 25
          Caption = 'Account.PushSettings'
          TabOrder = 5
          OnClick = Button6Click
        end
        object Button7: TButton
          Left = 4
          Top = 189
          Width = 137
          Height = 25
          Caption = 'Account.SaveProfileInfo'
          TabOrder = 6
          OnClick = Button7Click
        end
        object Button9: TButton
          Left = 3
          Top = 220
          Width = 137
          Height = 25
          Caption = 'Account.Online'
          TabOrder = 7
          OnClick = Button9Click
        end
        object Button10: TButton
          Left = 3
          Top = 251
          Width = 137
          Height = 25
          Caption = 'Account.Offline'
          TabOrder = 8
          OnClick = Button10Click
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Auth'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Button11: TButton
          Left = 3
          Top = 3
          Width = 137
          Height = 25
          Caption = 'Auth.CheckPhone'
          TabOrder = 0
          OnClick = Button11Click
        end
      end
      object TabSheet3: TTabSheet
        Caption = 'Audio'
        ImageIndex = 2
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Button21: TButton
          Left = 3
          Top = 3
          Width = 137
          Height = 25
          Caption = 'Audio.Get'
          TabOrder = 0
          OnClick = Button21Click
        end
        object Button23: TButton
          Left = 4
          Top = 34
          Width = 137
          Height = 25
          Caption = 'Audio.GetAlbums'
          TabOrder = 1
          OnClick = Button23Click
        end
        object Button24: TButton
          Left = 3
          Top = 65
          Width = 137
          Height = 25
          Caption = 'Audio.GetRecoms'
          TabOrder = 2
          OnClick = Button24Click
        end
        object Button29: TButton
          Left = 3
          Top = 96
          Width = 137
          Height = 25
          Caption = 'Audio.GetPop'
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
          Caption = 'Button31'
          TabOrder = 8
          OnClick = Button31Click
        end
      end
      object TabSheet4: TTabSheet
        Caption = 'Board'
        ImageIndex = 3
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Button22: TButton
          Left = 3
          Top = 3
          Width = 137
          Height = 25
          Caption = 'Board.CreateComment'
          TabOrder = 0
          OnClick = Button22Click
        end
      end
      object TabSheet5: TTabSheet
        Caption = 'Status'
        ImageIndex = 4
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Button17: TButton
          Left = 3
          Top = 3
          Width = 137
          Height = 25
          Caption = 'Status.Get'
          TabOrder = 0
          OnClick = Button17Click
        end
        object Button18: TButton
          Left = 3
          Top = 34
          Width = 137
          Height = 25
          Caption = 'Status.Set'
          TabOrder = 1
          OnClick = Button18Click
        end
      end
      object TabSheet6: TTabSheet
        Caption = 'Wall'
        ImageIndex = 5
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Button19: TButton
          Left = 3
          Top = 3
          Width = 137
          Height = 25
          Caption = 'Wall.Post'
          TabOrder = 0
          OnClick = Button19Click
        end
      end
      object TabSheet7: TTabSheet
        Caption = 'Users'
        ImageIndex = 6
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Button12: TButton
          Left = 3
          Top = 3
          Width = 137
          Height = 25
          Caption = 'Users.get'
          TabOrder = 0
          OnClick = Button12Click
        end
      end
      object TabSheet8: TTabSheet
        Caption = 'Groups'
        ImageIndex = 7
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Button25: TButton
          Left = 3
          Top = 3
          Width = 137
          Height = 25
          Caption = 'Groups.getMembers'
          TabOrder = 0
          OnClick = Button25Click
        end
      end
      object TabSheet10: TTabSheet
        Caption = 'Messages'
        ImageIndex = 9
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Button27: TButton
          Left = 3
          Top = 3
          Width = 166
          Height = 25
          Caption = 'Messages.GetConversations'
          TabOrder = 0
          OnClick = Button27Click
        end
      end
      object TabSheet11: TTabSheet
        Caption = 'Friends'
        ImageIndex = 10
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Button28: TButton
          Left = 3
          Top = 3
          Width = 137
          Height = 25
          Caption = 'Friends.get'
          TabOrder = 0
          OnClick = Button28Click
        end
      end
      object TabSheetPolls: TTabSheet
        Caption = 'Polls'
        ImageIndex = 11
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Button32: TButton
          Left = 3
          Top = 3
          Width = 137
          Height = 25
          Caption = 'Polls.GetBackgrounds'
          TabOrder = 0
          OnClick = Button32Click
        end
      end
      object TabSheetPodcasts: TTabSheet
        Caption = 'Podcasts'
        ImageIndex = 12
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Button33: TButton
          Left = 3
          Top = 3
          Width = 137
          Height = 25
          Caption = 'Podcasts.Search'
          TabOrder = 0
          OnClick = Button33Click
        end
      end
    end
    object ButtonLogin: TButton
      Left = 635
      Top = 9
      Width = 137
      Height = 25
      Caption = 'Login'
      TabOrder = 1
      OnClick = ButtonLoginClick
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
    EndPoint = 'https://oauth.vk.com/authorize'
    Permissions = [Notify, Friends, Photos, Audio, Video, Stories, Pages, Status, Notes, Wall, Ads, Offline, Docs, Groups, Notifications, Stats, Email, Market, AppWidget, Manage]
    BaseURL = 'https://api.vk.com/method'
    UsePseudoAsync = True
    Proxy.Port = 0
    OnAuth = VK1Auth
    OnLogin = VK1Login
    OnLog = VK1Log
    OnError = VK1Error
    OnErrorLogin = VK1ErrorLogin
    Left = 96
    Top = 56
  end
  object VkUserEvents1: TVkUserEvents
    VK = VK1
    OnNewMessage = VkUserEvents1NewMessage
    OnEditMessage = VkUserEvents1EditMessage
    OnUserOnline = VkUserEvents1UserOnline
    OnUserOffline = VkUserEvents1UserOffline
    OnChangeMessageFlags = VkUserEvents1ChangeMessageFlags
    OnChangeDialogFlags = VkUserEvents1ChangeDialogFlags
    OnReadMessages = VkUserEvents1ReadMessages
    OnRecoverMessages = VkUserEvents1RecoverMessages
    OnDeleteMessages = VkUserEvents1DeleteMessages
    OnChatChanged = VkUserEvents1ChatChanged
    OnChatChangeInfo = VkUserEvents1ChatChangeInfo
    OnUserTyping = VkUserEvents1UserTyping
    OnUsersTyping = VkUserEvents1UsersTyping
    OnUsersRecording = VkUserEvents1UsersRecording
    OnUserCall = VkUserEvents1UserCall
    OnCountChange = VkUserEvents1CountChange
    OnNotifyChange = VkUserEvents1NotifyChange
    Version = '3'
    Left = 208
    Top = 56
  end
  object VkGroupEventsController1: TVkGroupEventsController
    VK = VK1
    Groups.Strings = (
      '-145962568'
      '-184755622')
    OnWallReplyNew = VkGroupEventsController1WallReplyNew
    OnWallReplyEdit = VkGroupEventsController1WallReplyEdit
    OnWallReplyRestore = VkGroupEventsController1WallReplyRestore
    OnWallReplyDelete = VkGroupEventsController1WallReplyDelete
    OnWallPostNew = VkGroupEventsController1WallPostNew
    OnWallRepost = VkGroupEventsController1WallRepost
    OnAudioNew = VkGroupEventsController1AudioNew
    OnVideoNew = VkGroupEventsController1VideoNew
    OnVideoCommentNew = VkGroupEventsController1VideoCommentNew
    OnVideoCommentEdit = VkGroupEventsController1VideoCommentEdit
    OnVideoCommentRestore = VkGroupEventsController1VideoCommentRestore
    OnVideoCommentDelete = VkGroupEventsController1VideoCommentDelete
    OnPhotoNew = VkGroupEventsController1PhotoNew
    OnPhotoCommentNew = VkGroupEventsController1PhotoCommentNew
    OnPhotoCommentEdit = VkGroupEventsController1PhotoCommentEdit
    OnPhotoCommentRestore = VkGroupEventsController1PhotoCommentRestore
    OnPhotoCommentDelete = VkGroupEventsController1PhotoCommentDelete
    OnMessageNew = VkGroupEventsController1MessageNew
    OnMessageReply = VkGroupEventsController1MessageReply
    OnMessageEdit = VkGroupEventsController1MessageEdit
    OnMessageAllow = VkGroupEventsController1MessageAllow
    OnMessageDeny = VkGroupEventsController1MessageDeny
    OnMessageTypingState = VkGroupEventsController1MessageTypingState
    OnBoardPostNew = VkGroupEventsController1BoardPostNew
    OnBoardPostEdit = VkGroupEventsController1BoardPostEdit
    OnBoardPostRestore = VkGroupEventsController1BoardPostRestore
    OnBoardPostDelete = VkGroupEventsController1BoardPostDelete
    OnMarketCommentNew = VkGroupEventsController1MarketCommentNew
    OnMarketCommentEdit = VkGroupEventsController1MarketCommentEdit
    OnMarketCommentRestore = VkGroupEventsController1MarketCommentRestore
    OnMarketCommentDelete = VkGroupEventsController1MarketCommentDelete
    OnGroupLeave = VkGroupEventsController1GroupLeave
    OnGroupJoin = VkGroupEventsController1GroupJoin
    OnUserBlock = VkGroupEventsController1UserBlock
    OnUserUnBlock = VkGroupEventsController1UserUnBlock
    OnGroupPollVoteNew = VkGroupEventsController1GroupPollVoteNew
    OnGroupOfficersEdit = VkGroupEventsController1GroupOfficersEdit
    OnGroupChangeSettings = VkGroupEventsController1GroupChangeSettings
    OnGroupChangePhoto = VkGroupEventsController1GroupChangePhoto
    OnGroupPayTransaction = VkGroupEventsController1GroupPayTransaction
    OnGroupAppPayload = VkGroupEventsController1GroupAppPayload
    Version = '3'
    Left = 96
    Top = 120
  end
  object VkGroupEvents1: TVkGroupEvents
    VK = VK1
    Version = '3'
    Left = 208
    Top = 120
  end
end
