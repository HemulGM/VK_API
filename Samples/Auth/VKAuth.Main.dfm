object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 559
  ClientWidth = 921
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
    Left = 624
    Top = 0
    Width = 297
    Height = 559
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object LabelLogin: TLabel
      Left = 8
      Top = 8
      Width = 51
      Height = 13
      Caption = 'Logining...'
    end
    object Button1: TButton
      Left = 8
      Top = 35
      Width = 137
      Height = 25
      Caption = 'Account.Ban -1'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 8
      Top = 66
      Width = 137
      Height = 25
      Caption = 'Account.Unban -1'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 8
      Top = 97
      Width = 137
      Height = 25
      Caption = 'Account.ActiveOffers'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 8
      Top = 128
      Width = 137
      Height = 25
      Caption = 'Account.AppPermissions'
      TabOrder = 3
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 8
      Top = 159
      Width = 137
      Height = 25
      Caption = 'Account.Counters'
      TabOrder = 4
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 8
      Top = 190
      Width = 137
      Height = 25
      Caption = 'Account.PushSettings'
      TabOrder = 5
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 8
      Top = 221
      Width = 137
      Height = 25
      Caption = 'Account.SaveProfileInfo'
      TabOrder = 6
      OnClick = Button7Click
    end
    object Button8: TButton
      Left = 149
      Top = 453
      Width = 137
      Height = 25
      Caption = 'CallMethod'
      TabOrder = 7
      OnClick = Button8Click
    end
    object Button9: TButton
      Left = 8
      Top = 252
      Width = 137
      Height = 25
      Caption = 'Account.Online'
      TabOrder = 8
      OnClick = Button9Click
    end
    object Button10: TButton
      Left = 8
      Top = 283
      Width = 137
      Height = 25
      Caption = 'Account.Offline'
      TabOrder = 9
      OnClick = Button10Click
    end
    object Button11: TButton
      Left = 6
      Top = 329
      Width = 137
      Height = 25
      Caption = 'Auth.CheckPhone'
      TabOrder = 10
      OnClick = Button11Click
    end
    object Button12: TButton
      Left = 151
      Top = 35
      Width = 137
      Height = 25
      Caption = 'Users.get'
      TabOrder = 11
      OnClick = Button12Click
    end
    object Button13: TButton
      Left = 149
      Top = 329
      Width = 137
      Height = 25
      Caption = 'LongPollStart'
      TabOrder = 12
      OnClick = Button13Click
    end
    object Button14: TButton
      Left = 149
      Top = 360
      Width = 137
      Height = 25
      Caption = 'LongPollStop'
      TabOrder = 13
      OnClick = Button14Click
    end
    object Button15: TButton
      Left = 149
      Top = 391
      Width = 137
      Height = 25
      Caption = 'GroupLongPollStart'
      TabOrder = 14
      OnClick = Button15Click
    end
    object Button16: TButton
      Left = 149
      Top = 422
      Width = 137
      Height = 25
      Caption = 'GroupLongPollStop'
      TabOrder = 15
      OnClick = Button16Click
    end
    object Button17: TButton
      Left = 151
      Top = 97
      Width = 137
      Height = 25
      Caption = 'Status.Get'
      TabOrder = 16
      OnClick = Button17Click
    end
    object Button18: TButton
      Left = 151
      Top = 128
      Width = 137
      Height = 25
      Caption = 'Status.Set'
      TabOrder = 17
      OnClick = Button18Click
    end
    object Button19: TButton
      Left = 151
      Top = 190
      Width = 137
      Height = 25
      Caption = 'Wall.Post'
      TabOrder = 18
      OnClick = Button19Click
    end
    object Button20: TButton
      Left = 151
      Top = 252
      Width = 137
      Height = 25
      Caption = 'UploadAudioMessage'
      TabOrder = 19
      OnClick = Button20Click
    end
    object Button21: TButton
      Left = 6
      Top = 391
      Width = 137
      Height = 25
      Caption = 'Audio.Get'
      TabOrder = 20
      OnClick = Button21Click
    end
    object Button22: TButton
      Left = 6
      Top = 453
      Width = 137
      Height = 25
      Caption = 'Board.CreateComment'
      TabOrder = 21
      OnClick = Button22Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 624
    Height = 559
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Memo1: TMemo
      Left = 0
      Top = 0
      Width = 624
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
      Width = 624
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
    AppID = '9837245'
    AppKey = 'XBMNBmndgfhdnmB34sxcZ'
    EndPoint = 'https://oauth.vk.com/authorize'
    Permissions = 'groups,friends,wall,photos,video,docs,notes,market,messages'
    APIVersion = '5.103'
    BaseURL = 'https://api.vk.com/method'
    ServiceKey = 'fcsdfbvdsbvfcsbdvfcasdgfhgasdfjkshadgfbdsmfgbmdsfngbmdfsgb'
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
    Left = 96
    Top = 176
  end
  object VkGroupEvents1: TVkGroupEvents
    VK = VK1
    Left = 208
    Top = 120
  end
end
