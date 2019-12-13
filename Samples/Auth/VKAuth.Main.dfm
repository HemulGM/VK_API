object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 385
  ClientWidth = 617
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LabelLogin: TLabel
    Left = 503
    Top = 11
    Width = 51
    Height = 13
    Caption = 'Logining...'
  end
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 489
    Height = 370
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Button1: TButton
    Left = 503
    Top = 38
    Width = 106
    Height = 25
    Caption = 'Ban -1'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 503
    Top = 69
    Width = 106
    Height = 25
    Caption = 'Unban -1'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 503
    Top = 100
    Width = 106
    Height = 25
    Caption = 'ActiveOffers'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 503
    Top = 131
    Width = 106
    Height = 25
    Caption = 'AppPermissions'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 503
    Top = 162
    Width = 106
    Height = 25
    Caption = 'Counters'
    TabOrder = 5
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 503
    Top = 193
    Width = 106
    Height = 25
    Caption = 'PushSettings'
    TabOrder = 6
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 503
    Top = 224
    Width = 106
    Height = 25
    Caption = 'SaveProfileInfo'
    TabOrder = 7
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 503
    Top = 255
    Width = 106
    Height = 25
    Caption = 'CallMethod'
    TabOrder = 8
    OnClick = Button8Click
  end
  object Button9: TButton
    Left = 503
    Top = 286
    Width = 106
    Height = 25
    Caption = 'Online'
    TabOrder = 9
    OnClick = Button9Click
  end
  object Button10: TButton
    Left = 503
    Top = 317
    Width = 106
    Height = 25
    Caption = 'Offline'
    TabOrder = 10
    OnClick = Button10Click
  end
  object VK1: TVK
    OnLogin = VK1Login
    OnError = VK1Error
    AppID = '7245537'
    AppKey = 'XVJ5V9F65v5eo1bhu3qY'
    EndPoint = 'https://oauth.vk.com/authorize'
    Permissions = 'groups,friends,wall,photos,video,docs,notes,market,messages'
    APIVersion = '5.101'
    BaseURL = 'https://api.vk.com/method'
    ServiceKey = 
      'cbbadfcecbbadfcecbbadfce3ccbd4512fccbbacbbadfce9649f09303f1cb32b' +
      '4717e70'
    Left = 112
    Top = 256
  end
end
