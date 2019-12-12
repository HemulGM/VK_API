object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 386
  ClientWidth = 612
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
    Left = 8
    Top = 8
    Width = 51
    Height = 13
    Caption = 'Logining...'
  end
  object Memo1: TMemo
    Left = 8
    Top = 27
    Width = 393
    Height = 198
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Button1: TButton
    Left = 407
    Top = 25
    Width = 75
    Height = 25
    Caption = 'Ban -1'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 407
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Unban -1'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 407
    Top = 87
    Width = 75
    Height = 25
    Caption = 'ActiveOffers'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 407
    Top = 118
    Width = 106
    Height = 25
    Caption = 'AppPermissions'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 407
    Top = 149
    Width = 75
    Height = 25
    Caption = 'Counters'
    TabOrder = 5
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 407
    Top = 180
    Width = 75
    Height = 25
    Caption = 'PushSettings'
    TabOrder = 6
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 407
    Top = 211
    Width = 106
    Height = 25
    Caption = 'SaveProfileInfo'
    TabOrder = 7
    OnClick = Button7Click
  end
  object VK1: TVK
    OnLogin = VK1Login
    AppID = '7066268'
    AppKey = 'abihU3LO2wy7MHNnfZoy'
    EndPoint = 'https://oauth.vk.com/authorize'
    Permissions = 'groups,friends,wall,photos,video,docs,notes,market,messages'
    APIVersion = '5.101'
    BaseURL = 'https://api.vk.com/method'
    Left = 112
    Top = 256
  end
end
