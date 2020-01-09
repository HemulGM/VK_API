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
    Left = 688
    Top = 0
    Width = 233
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
      Width = 106
      Height = 25
      Caption = 'Ban -1'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 8
      Top = 66
      Width = 106
      Height = 25
      Caption = 'Unban -1'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 8
      Top = 97
      Width = 106
      Height = 25
      Caption = 'ActiveOffers'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 8
      Top = 128
      Width = 106
      Height = 25
      Caption = 'AppPermissions'
      TabOrder = 3
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 8
      Top = 159
      Width = 106
      Height = 25
      Caption = 'Counters'
      TabOrder = 4
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 8
      Top = 190
      Width = 106
      Height = 25
      Caption = 'PushSettings'
      TabOrder = 5
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 8
      Top = 221
      Width = 106
      Height = 25
      Caption = 'SaveProfileInfo'
      TabOrder = 6
      OnClick = Button7Click
    end
    object Button8: TButton
      Left = 8
      Top = 252
      Width = 106
      Height = 25
      Caption = 'CallMethod'
      TabOrder = 7
      OnClick = Button8Click
    end
    object Button9: TButton
      Left = 8
      Top = 283
      Width = 106
      Height = 25
      Caption = 'Online'
      TabOrder = 8
      OnClick = Button9Click
    end
    object Button10: TButton
      Left = 8
      Top = 314
      Width = 106
      Height = 25
      Caption = 'Offline'
      TabOrder = 9
      OnClick = Button10Click
    end
    object Button11: TButton
      Left = 8
      Top = 345
      Width = 106
      Height = 25
      Caption = 'CheckPhone'
      TabOrder = 10
      OnClick = Button11Click
    end
    object Button12: TButton
      Left = 120
      Top = 35
      Width = 106
      Height = 25
      Caption = 'users.get'
      TabOrder = 11
      OnClick = Button12Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 688
    Height = 559
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Memo1: TMemo
      Left = 0
      Top = 0
      Width = 688
      Height = 448
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
      Top = 448
      Width = 688
      Height = 111
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
    OnLogin = VK1Login
    OnLog = VK1Log
    OnError = VK1Error
    AppID = '7245537'
    AppKey = 'XVJ5V9F65v5eo1bhu3qY'
    EndPoint = 'https://oauth.vk.com/authorize'
    Permissions = 'groups,friends,wall,photos,video,docs,notes,market,messages'
    APIVersion = '5.103'
    BaseURL = 'https://api.vk.com/method'
    ServiceKey = 
      'cbbadfcecbbadfcecbbadfce3ccbd4512fccbbacbbadfce9649f09303f1cb32b' +
      '4717e70'
    Left = 368
    Top = 128
  end
end
