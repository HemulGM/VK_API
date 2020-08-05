object FormOAuth2: TFormOAuth2
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = #1040#1074#1090#1086#1088#1080#1079#1072#1094#1080#1103' OAuth2'
  ClientHeight = 600
  ClientWidth = 550
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
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object Browser: TWebBrowser
    Left = 0
    Top = 0
    Width = 550
    Height = 600
    Align = alClient
    TabOrder = 0
    OnNavigateComplete2 = BrowserNavigateComplete2
    OnFileDownload = BrowserFileDownload
    OnNavigateError = BrowserNavigateError
    ExplicitTop = 30
    ExplicitWidth = 742
    ExplicitHeight = 478
    ControlData = {
      4C000000D8380000033E00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126202000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
end
