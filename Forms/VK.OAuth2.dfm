object FormOAuth2: TFormOAuth2
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = #1040#1074#1090#1086#1088#1080#1079#1072#1094#1080#1103' OAuth2'
  ClientHeight = 503
  ClientWidth = 742
  Color = clWhite
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object Shape1: TShape
    Left = 0
    Top = 24
    Width = 742
    Height = 1
    Align = alTop
    Brush.Color = 15132390
    Pen.Color = 15132390
    ExplicitWidth = 635
  end
  object Browser: TWebBrowser
    Left = 0
    Top = 25
    Width = 742
    Height = 478
    Align = alClient
    TabOrder = 0
    OnTitleChange = BrowserTitleChange
    OnBeforeNavigate2 = BrowserBeforeNavigate2
    OnNavigateComplete2 = BrowserNavigateComplete2
    OnFileDownload = BrowserFileDownload
    OnNavigateError = BrowserNavigateError
    ExplicitTop = 30
    ControlData = {
      4C000000B04C0000673100000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126202000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object EditAddr: TEdit
    Left = 0
    Top = 0
    Width = 742
    Height = 24
    Align = alTop
    BevelEdges = [beTop, beBottom]
    BevelInner = bvSpace
    BevelKind = bkFlat
    BevelOuter = bvSpace
    BevelWidth = 2
    BorderStyle = bsNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    Text = ' '#1047#1072#1075#1088#1091#1079#1082#1072'...'
    OnChange = EditAddrChange
  end
end
