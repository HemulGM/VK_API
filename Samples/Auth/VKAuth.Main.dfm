object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'VK API Tester'
  ClientHeight = 715
  ClientWidth = 1069
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 488
    Width = 1069
    Height = 227
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 487
    ExplicitWidth = 1065
    object Memo1: TMemo
      Left = 0
      Top = 17
      Width = 400
      Height = 210
      Align = alLeft
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
      Left = 400
      Top = 17
      Width = 669
      Height = 210
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 1
      ExplicitWidth = 665
    end
    object HeaderControl1: THeaderControl
      Left = 0
      Top = 0
      Width = 1069
      Height = 17
      FullDrag = False
      Sections = <
        item
          FixedWidth = True
          ImageIndex = -1
          MaxWidth = 400
          MinWidth = 400
          Text = #1054#1082#1085#1086' '#1074#1099#1074#1086#1076#1072
          Width = 400
        end
        item
          ImageIndex = -1
          Text = #1051#1086#1075' '#1079#1072#1087#1088#1086#1089#1086#1074
          Width = 100
        end>
      Style = hsFlat
      NoSizing = True
      ExplicitWidth = 1065
    end
  end
  object PageControl2: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 1063
    Height = 482
    ActivePage = TabSheetTests
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabHeight = 30
    TabOrder = 1
    ExplicitWidth = 1059
    ExplicitHeight = 481
    object TabSheetWelcome: TTabSheet
      Caption = #1044#1086#1073#1088#1086' '#1087#1086#1078#1072#1083#1086#1074#1072#1090#1100
      object Label1: TLabel
        AlignWithMargins = True
        Left = 30
        Top = 30
        Width = 995
        Height = 29
        Margins.Left = 30
        Margins.Top = 30
        Margins.Right = 30
        Margins.Bottom = 0
        Align = alTop
        Caption = #1055#1088#1080#1074#1077#1090#1089#1090#1074#1091#1102
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -24
        Font.Name = 'Roboto'
        Font.Style = []
        ParentFont = False
        ExplicitWidth = 155
      end
      object Memo2: TMemo
        AlignWithMargins = True
        Left = 30
        Top = 69
        Width = 995
        Height = 343
        Margins.Left = 30
        Margins.Top = 10
        Margins.Right = 30
        Margins.Bottom = 30
        Align = alClient
        BorderStyle = bsNone
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Roboto'
        Font.Style = []
        Lines.Strings = (
          
            #1047#1076#1077#1089#1100' '#1103' '#1090#1077#1089#1090#1080#1088#1091#1102' VK API. '#1058#1091#1090' '#1084#1086#1078#1085#1086' '#1085#1072#1081#1090#1080' '#1089#1087#1086#1089#1086#1073#1099' '#1072#1074#1090#1086#1088#1080#1079#1072#1094#1080#1080', '#1087#1088 +
            #1080#1084#1077#1088#1099' '#1080#1089#1087#1086#1083#1100#1079#1086#1074#1072#1085#1080#1103' API '#1080' '#1086#1073#1088#1072#1073#1086#1090#1082#1091' '#1086#1096#1080#1073#1086#1082
          ''
          
            #1044#1072#1085#1085#1072#1103' '#1086#1073#1077#1088#1090#1082#1072' '#1087#1088#1077#1076#1089#1090#1072#1074#1083#1077#1085#1072' "'#1082#1072#1082' '#1077#1089#1090#1100'". '#1071' '#1087#1086#1089#1090#1072#1088#1072#1083#1089#1103' '#1088#1077#1072#1083#1080#1079#1086#1074#1072#1090#1100 +
            ' '#1073#1086#1083#1100#1096#1091#1102' '#1095#1072#1089#1090#1100' '#1084#1077#1090#1086#1076#1086#1074', '#1087#1088#1086#1090#1077#1089#1090#1080#1088#1086#1074#1072#1090#1100' '#1080' '#1086#1090#1083#1072#1076#1080#1090#1100' '#1080#1093'.'
          #1058#1072#1082#1078#1077', '#1088#1077#1072#1083#1080#1079#1086#1074#1072#1085#1086' '#1085#1077#1089#1082#1086#1083#1100#1082#1086' '#1076#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1093' '#1074#1077#1097#1077#1081':'
          
            '1. '#1054#1095#1077#1088#1077#1076#1100' '#1074#1099#1079#1086#1074#1086#1074' '#1079#1072#1087#1088#1086#1089#1086#1074' ('#1076#1083#1103' '#1089#1086#1073#1083#1102#1076#1077#1085#1080#1103' '#1086#1075#1088#1072#1085#1080#1095#1077#1085#1080#1103' '#1082#1086#1083'-'#1074#1072' '#1079 +
            #1072#1087#1088#1086#1089#1086#1074' '#1074' '#1089#1077#1082'). '#1058'.'#1077'. '#1074#1072#1084' '#1085#1077' '#1085#1091#1078#1085#1086' '#1079#1072#1073#1086#1090#1080#1090#1100#1089#1103' '#1086' '#1090#1086#1084', '#1095#1090#1086' '
          #1074#1099' '#1089#1083#1080#1096#1082#1086#1084' '#1073#1099#1089#1090#1088#1086' '#1080' '#1095#1072#1089#1090#1086' '#1074#1099#1079#1099#1074#1072#1077#1090#1077' '#1084#1077#1090#1086#1076#1099
          
            '2. '#1056#1077#1072#1083#1080#1079#1086#1074#1072#1085#1099' '#1087#1077#1088#1077#1093#1074#1072#1090#1099' '#1086#1096#1080#1073#1086#1082', '#1082#1086#1090#1086#1088#1099#1077' '#1090#1088#1077#1073#1091#1102#1090' '#1076#1077#1081#1089#1090#1074#1080#1081'. '#1053#1072#1087#1088#1080 +
            #1084#1077#1088', '#1082#1072#1087#1095#1072' '#1080#1083#1080' "'#1087#1086#1076#1090#1074#1077#1088#1078#1076#1077#1085#1080#1077'"'
          ''
          
            #1057' '#1085#1077#1076#1072#1074#1085#1080#1093' '#1087#1086#1088', '#1103' '#1091#1073#1088#1072#1083' '#1090#1072#1082#1091#1102' '#1074#1077#1097#1100' '#1082#1072#1082' "'#1087#1089#1077#1074#1076#1086' '#1072#1089#1080#1085#1093#1088#1086#1085#1085#1086#1089#1090#1100'". '#1045 +
            #1089#1083#1080' '#1074#1099' '#1077#1105' '#1085#1077' '#1079#1072#1089#1090#1072#1083#1080' - '#1093#1086#1088#1086#1096#1086', '#1077#1089#1083#1080' '#1079#1072#1089#1090#1072#1083#1080', '#1090#1086' '#1074#1072#1084' '
          
            #1087#1088#1080#1076#1077#1090#1089#1103' '#1087#1077#1088#1077#1076#1077#1083#1072#1090#1100' '#1089#1074#1086#1080' '#1087#1088#1086#1077#1082#1090#1099' '#1080' '#1074#1079#1099#1074#1072#1090#1100' '#1079#1072#1087#1088#1086#1089#1099' '#1074' '#1086#1090#1076#1077#1083#1100#1085#1099#1093' '#1087 +
            #1086#1090#1086#1082#1072#1093' ('#1080#1083#1080' '#1090#1072#1089#1082#1072#1093') '#1089#1072#1084#1086#1089#1090#1086#1103#1090#1077#1083#1100#1085#1086'.'
          ''
          #1056#1072#1079#1088#1072#1073#1086#1090#1095#1080#1082' '#1043#1077#1085#1085#1072#1076#1080#1081' aka HemulGM '#1052#1072#1083#1080#1083#1085#1080#1085)
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object TabSheetAuth: TTabSheet
      Caption = #1040#1074#1090#1086#1088#1080#1079#1072#1094#1080#1103
      ImageIndex = 1
      OnResize = TabSheetAuthResize
      object ScrollBox1: TScrollBox
        Left = 0
        Top = 0
        Width = 1055
        Height = 442
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Color = clWindow
        ParentColor = False
        TabOrder = 0
        object Label2: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 26
          Width = 1049
          Height = 85
          Align = alTop
          Caption = 
            #1052#1077#1090#1086#1076' VK.Login '#1087#1088#1086#1074#1077#1088#1103#1077#1090', '#1091#1089#1090#1072#1085#1086#1074#1083#1077#1085' '#1083#1080' '#1058#1086#1082#1077#1085', '#1077#1089#1083#1080' '#1085#1077#1090', '#1090#1086' '#1074#1099#1079#1099 +
            #1074#1072#1077#1090' '#1089#1086#1073#1099#1090#1080#1077' OnAuth, '#1074' '#1082#1086#1090#1086#1088#1086#1084' '#1090#1088#1077#1073#1091#1077#1090' '#1074#1077#1088#1085#1091#1090#1100' '#1090#1086#1082#1077#1085' '#1074' '#1072#1088#1075#1091#1084#1077#1085#1090#1072 +
            #1093' '#1076#1083#1103' '#1077#1075#1086' '#1091#1089#1090#1072#1085#1086#1074#1082#1080' '#1080' '#1080#1089#1087#1086#1083#1100#1079#1086#1074#1072#1085#1080#1103'.'#13#10#1042' '#1076#1072#1085#1085#1086#1084' '#1087#1088#1080#1084#1077#1088#1077' '#1088#1077#1072#1083#1080#1079#1086#1074#1072 +
            #1085#1086' '#1086#1082#1085#1086' '#1089' '#1072#1074#1090#1086#1088#1080#1079#1072#1094#1080#1077#1081' ('#1082#1086#1090#1086#1088#1086#1077' '#1080#1084#1077#1077#1090#1089#1103' '#1082#1072#1082' '#1076#1083#1103' VCL '#1090#1072#1082' '#1080' '#1076#1083#1103' FM' +
            'X '#1080#1079' '#1082#1086#1088#1086#1073#1082#1080'). '#1042#1099#1079#1099#1074#1072#1077#1090#1089#1103' '#1086#1082#1085#1086' '#1089' '#1072#1074#1090#1086#1088#1080#1079#1072#1094#1080#1077#1081' OAuth2 '#1080' '#1086#1078#1080#1076#1072#1077#1090#1089#1103 +
            ' '#1090#1086#1082#1077#1085' '#1074' '#1072#1076#1088#1077#1089#1077' '#1079#1072#1087#1088#1086#1089#1072' ('#1087#1086#1089#1083#1077' '#1091#1089#1087#1077#1096#1085#1086#1081' '#1072#1074#1090#1086#1088#1080#1079#1072#1094#1080#1080' '#1085#1072' '#1089#1072#1081#1090#1077')'#13#10#13 +
            #10#1055#1086#1089#1083#1077' '#1101#1090#1086#1075#1086', '#1080#1083#1080' '#1077#1089#1083#1080' '#1090#1086#1082#1077#1085' '#1091#1078#1077' '#1091#1089#1090#1072#1085#1086#1074#1083#1077#1085', '#1074#1099#1087#1086#1083#1085#1103#1077#1090#1089#1103' '#1087#1088#1086#1074#1077#1088#1086 +
            #1095#1085#1099#1081' '#1079#1072#1087#1088#1086#1089'. '#1055#1086#1089#1083#1077' '#1082#1086#1090#1086#1088#1086#1075#1086' '#1073#1091#1076#1077#1090' '#1103#1089#1085#1086' '#1074#1072#1083#1080#1076#1085#1099#1081' '#1090#1086#1082#1077#1085' '#1080#1083#1080' '#1085#1077#1090
          WordWrap = True
          ExplicitWidth = 1047
        end
        object LabelLogin: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 1049
          Height = 17
          Align = alTop
          Caption = #1042#1093#1086#1076
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Segoe UI Semibold'
          Font.Style = [fsBold]
          ParentFont = False
          ExplicitWidth = 31
        end
        object Label3: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 158
          Width = 1049
          Height = 17
          Align = alTop
          Caption = 
            #1050#1085#1086#1087#1082#1072' Logout '#1087#1088#1086#1089#1090#1086' '#1086#1095#1080#1097#1072#1077#1090' '#1091#1089#1090#1072#1085#1086#1074#1083#1077#1085#1085#1099#1081' '#1090#1086#1082#1077#1085', '#1072' '#1090#1072#1082#1078#1077' '#1082#1077#1096' '#1089#1090 +
            #1072#1085#1076#1072#1088#1090#1085#1086#1075#1086' '#1073#1088#1072#1091#1079#1077#1088#1072
          WordWrap = True
          ExplicitWidth = 546
        end
        object Panel3: TPanel
          Left = 0
          Top = 114
          Width = 1055
          Height = 41
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object ButtonLogin: TButton
            Left = 3
            Top = 6
            Width = 84
            Height = 31
            Caption = 'Login'
            TabOrder = 0
            OnClick = ButtonLoginClick
          end
        end
        object Panel4: TPanel
          Left = 0
          Top = 178
          Width = 1055
          Height = 41
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          object Button47: TButton
            Left = 3
            Top = 6
            Width = 84
            Height = 31
            Caption = 'Logout'
            TabOrder = 0
            OnClick = Button47Click
          end
        end
      end
    end
    object TabSheetMethods: TTabSheet
      Caption = #1052#1077#1090#1086#1076#1099
      ImageIndex = 2
      object CategoryButtons1: TCategoryButtons
        Left = 0
        Top = 0
        Width = 305
        Height = 442
        Align = alLeft
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        ButtonFlow = cbfVertical
        ButtonHeight = 30
        ButtonOptions = [boFullSize, boShowCaptions, boBoldCaptions]
        Categories = <
          item
            Caption = 'Account'
            Color = 15400959
            Collapsed = False
            Items = <
              item
                Caption = 'ban'
              end
              item
                Caption = 'changePassword'
              end>
          end
          item
            Caption = 'Ads'
            Color = 16777194
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'AppWidgets'
            Color = 15395839
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Apps'
            Color = 15466474
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Asr'
            Color = 16771818
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Auth'
            Color = 16771839
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Board'
            Color = 16053492
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Calls'
            Color = 15395839
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Database'
            Color = 16771839
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Docs'
            Color = 16771818
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Donut'
            Color = 15400959
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'DownloadedGames'
            Color = 16777194
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Fave'
            Color = 15395839
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Friends'
            Color = 15466474
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Gifts'
            Color = 16771818
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Groups'
            Color = 16771839
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'LeadForms'
            Color = 16053492
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Likes'
            Color = 15395839
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Market'
            Color = 16771839
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Messages'
            Color = 16771818
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Newsfeed'
            Color = 15400959
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Notes'
            Color = 16777194
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Notifications'
            Color = 15395839
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Orders'
            Color = 15466474
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Pages'
            Color = 16771818
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Photos'
            Color = 16771839
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Places'
            Color = 16053492
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Podcasts'
            Color = 15395839
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Polls'
            Color = 16771839
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'PrettyCards'
            Color = 16771818
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Search'
            Color = 15400959
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Secure'
            Color = 16777194
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Stats'
            Color = 15395839
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Status'
            Color = 15466474
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Storage'
            Color = 16771818
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Store'
            Color = 16771839
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Stories'
            Color = 16053492
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Streaming'
            Color = 15395839
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Users'
            Color = 16771839
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Utils'
            Color = 16771818
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Video'
            Color = 15400959
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Wall'
            Color = 16777194
            Collapsed = False
            Items = <>
          end
          item
            Caption = 'Widgets'
            Color = 15395839
            Collapsed = False
            Items = <>
          end>
        RegularButtonColor = clWhite
        SelectedButtonColor = 15132390
        TabOrder = 0
        OnButtonClicked = CategoryButtons1ButtonClicked
      end
      object MemoCode: TMemo
        Left = 305
        Top = 0
        Width = 750
        Height = 442
        Align = alClient
        BorderStyle = bsNone
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 1
      end
    end
    object TabSheetTests: TTabSheet
      Caption = #1058#1077#1089#1090#1099
      ImageIndex = 3
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 1055
        Height = 442
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitWidth = 1051
        ExplicitHeight = 441
        object PageControl1: TPageControl
          AlignWithMargins = True
          Left = 30
          Top = 30
          Width = 995
          Height = 382
          Margins.Left = 30
          Margins.Top = 30
          Margins.Right = 30
          Margins.Bottom = 30
          ActivePage = TabSheetPages
          Align = alClient
          TabOrder = 0
          object TabSheet9: TTabSheet
            Caption = 'General'
            ImageIndex = 8
            object ButtonUploadAudioMessage: TButton
              Left = 3
              Top = 3
              Width = 137
              Height = 25
              Caption = 'UploadAudioMessage'
              TabOrder = 0
              OnClick = ButtonUploadAudioMessageClick
            end
            object ButtonLongPollStart: TButton
              Left = 146
              Top = 3
              Width = 137
              Height = 25
              Caption = 'LongPollStart'
              TabOrder = 1
              OnClick = ButtonLongPollStartClick
            end
            object ButtonLongPollStop: TButton
              Left = 145
              Top = 34
              Width = 137
              Height = 25
              Caption = 'LongPollStop'
              TabOrder = 2
              OnClick = ButtonLongPollStopClick
            end
            object ButtonGLongPollStart: TButton
              Left = 289
              Top = 3
              Width = 137
              Height = 25
              Caption = 'GroupLongPollStart'
              TabOrder = 3
              OnClick = ButtonGLongPollStartClick
            end
            object ButtonGLongPollStop: TButton
              Left = 289
              Top = 34
              Width = 137
              Height = 25
              Caption = 'GroupLongPollStop'
              TabOrder = 4
              OnClick = ButtonGLongPollStopClick
            end
            object ButtonCallMethod: TButton
              Left = 145
              Top = 189
              Width = 137
              Height = 25
              Caption = 'CallMethod'
              TabOrder = 5
              OnClick = ButtonCallMethodClick
            end
            object ButtonSearchGetHints: TButton
              Left = 3
              Top = 34
              Width = 137
              Height = 25
              Caption = 'Search.GetHints'
              TabOrder = 6
              OnClick = ButtonSearchGetHintsClick
            end
            object ButtonDBGetRegions: TButton
              Left = 2
              Top = 65
              Width = 137
              Height = 25
              Caption = 'DB.GetRegions'
              TabOrder = 7
              OnClick = ButtonDBGetRegionsClick
            end
            object ButtonDBGetSchoolClasses: TButton
              Left = 3
              Top = 96
              Width = 137
              Height = 25
              Caption = 'DB.GetSchoolClasses'
              TabOrder = 8
              OnClick = ButtonDBGetSchoolClassesClick
            end
            object ButtonStorageGet: TButton
              Left = 2
              Top = 127
              Width = 137
              Height = 25
              Caption = 'Storage.Get'
              TabOrder = 9
              OnClick = ButtonStorageGetClick
            end
            object ButtonSecureGetBalance: TButton
              Left = 2
              Top = 158
              Width = 137
              Height = 25
              Caption = 'Secure.GetBalance'
              TabOrder = 10
              OnClick = ButtonSecureGetBalanceClick
            end
            object ButtonStoriesGet: TButton
              Left = 2
              Top = 189
              Width = 137
              Height = 25
              Caption = 'Stories.Get'
              TabOrder = 11
              OnClick = ButtonStoriesGetClick
            end
          end
          object TabSheet1: TTabSheet
            Caption = 'Account'
            object ButtonAccountBan: TButton
              Left = 3
              Top = 3
              Width = 137
              Height = 25
              Caption = 'Ban -1'
              TabOrder = 0
              OnClick = ButtonAccountBanClick
            end
            object ButtonAccountUnban: TButton
              Left = 3
              Top = 34
              Width = 137
              Height = 25
              Caption = 'Unban -1'
              TabOrder = 1
              OnClick = ButtonAccountUnbanClick
            end
            object ButtonAccountActiveOffers: TButton
              Left = 3
              Top = 65
              Width = 137
              Height = 25
              Caption = 'ActiveOffers'
              TabOrder = 2
              OnClick = ButtonAccountActiveOffersClick
            end
            object ButtonAccountAppPrem: TButton
              Left = 4
              Top = 96
              Width = 137
              Height = 25
              Caption = 'AppPermissions'
              TabOrder = 3
              OnClick = ButtonAccountAppPremClick
            end
            object ButtonAccountCounters: TButton
              Left = 4
              Top = 127
              Width = 137
              Height = 25
              Caption = 'Counters'
              TabOrder = 4
              OnClick = ButtonAccountCountersClick
            end
            object ButtonAccountPushSettings: TButton
              Left = 4
              Top = 158
              Width = 137
              Height = 25
              Caption = 'PushSettings'
              TabOrder = 5
              OnClick = ButtonAccountPushSettingsClick
            end
            object ButtonAccountSaveProfileInfo: TButton
              Left = 4
              Top = 189
              Width = 137
              Height = 25
              Caption = 'SaveProfileInfo'
              TabOrder = 6
              OnClick = ButtonAccountSaveProfileInfoClick
            end
            object ButtonAccountOnline: TButton
              Left = 3
              Top = 220
              Width = 137
              Height = 25
              Caption = 'Online'
              TabOrder = 7
              OnClick = ButtonAccountOnlineClick
            end
            object ButtonAccountOffline: TButton
              Left = 3
              Top = 251
              Width = 137
              Height = 25
              Caption = 'Offline'
              TabOrder = 8
              OnClick = ButtonAccountOfflineClick
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
            object ButtonAuthCheckPhone: TButton
              Left = 3
              Top = 3
              Width = 174
              Height = 25
              Caption = 'CheckPhone (depricated)'
              TabOrder = 0
              OnClick = ButtonAuthCheckPhoneClick
            end
            object ButtonAuthSingup: TButton
              Left = 3
              Top = 34
              Width = 174
              Height = 25
              Caption = 'Signup'
              TabOrder = 1
              OnClick = ButtonAuthSingupClick
            end
          end
          object TabSheet3: TTabSheet
            Caption = 'Audio'
            ImageIndex = 2
            object ButtonAudioGet: TButton
              Left = 3
              Top = 3
              Width = 137
              Height = 25
              Caption = 'Get'
              TabOrder = 0
              OnClick = ButtonAudioGetClick
            end
            object ButtonAudioGetAlbums: TButton
              Left = 4
              Top = 34
              Width = 137
              Height = 25
              Caption = 'GetAlbums'
              TabOrder = 1
              OnClick = ButtonAudioGetAlbumsClick
            end
            object ButtonAudioGetRecoms: TButton
              Left = 3
              Top = 65
              Width = 137
              Height = 25
              Caption = 'GetRecoms'
              TabOrder = 2
              OnClick = ButtonAudioGetRecomsClick
            end
            object ButtonAudioGetPop: TButton
              Left = 3
              Top = 96
              Width = 137
              Height = 25
              Caption = 'GetPop'
              TabOrder = 3
              OnClick = ButtonAudioGetPopClick
            end
            object ButtonAudioGetChart: TButton
              Left = 3
              Top = 127
              Width = 137
              Height = 25
              Caption = 'GetChart'
              TabOrder = 4
              OnClick = ButtonAudioGetChartClick
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
            object ButtonAudioAddToPlaylist: TButton
              Left = 146
              Top = 96
              Width = 119
              Height = 25
              Caption = 'AddToPlaylist'
              TabOrder = 8
              OnClick = ButtonAudioAddToPlaylistClick
            end
            object ButtonAudioGetCount: TButton
              Left = 3
              Top = 158
              Width = 137
              Height = 25
              Caption = 'GetCount'
              TabOrder = 9
              OnClick = ButtonAudioGetCountClick
            end
            object ButtonUploadAudio: TButton
              Left = 146
              Top = 127
              Width = 120
              Height = 25
              Caption = 'UploadAudio'
              TabOrder = 10
              OnClick = ButtonUploadAudioClick
            end
          end
          object TabSheet4: TTabSheet
            Caption = 'Board'
            ImageIndex = 3
            object ButtonBoardCreateComment: TButton
              Left = 3
              Top = 3
              Width = 137
              Height = 25
              Caption = 'CreateComment'
              TabOrder = 0
              OnClick = ButtonBoardCreateCommentClick
            end
            object ButtonBoardGetBoard: TButton
              Left = 3
              Top = 34
              Width = 137
              Height = 25
              Caption = 'GetBoard'
              TabOrder = 1
              OnClick = ButtonBoardGetBoardClick
            end
          end
          object TabSheet5: TTabSheet
            Caption = 'Status'
            ImageIndex = 4
            object ButtonStatusGet: TButton
              Left = 3
              Top = 3
              Width = 137
              Height = 25
              Caption = 'Get'
              TabOrder = 0
              OnClick = ButtonStatusGetClick
            end
            object ButtonStatusSet: TButton
              Left = 3
              Top = 34
              Width = 137
              Height = 25
              Caption = 'Set'
              TabOrder = 1
              OnClick = ButtonStatusSetClick
            end
          end
          object TabSheet6: TTabSheet
            Caption = 'Wall'
            ImageIndex = 5
            object ButtonWallPost: TButton
              Left = 3
              Top = 3
              Width = 137
              Height = 25
              Caption = 'Post'
              TabOrder = 0
              OnClick = ButtonWallPostClick
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
            object ButtonGroupsGetMembers: TButton
              Left = 3
              Top = 3
              Width = 137
              Height = 25
              Caption = 'GetMembers'
              TabOrder = 0
              OnClick = ButtonGroupsGetMembersClick
            end
            object ButtonGroupsGetById: TButton
              Left = 3
              Top = 34
              Width = 137
              Height = 25
              Caption = 'GetById'
              TabOrder = 1
              OnClick = ButtonGroupsGetByIdClick
            end
            object ButtonGroupsGet: TButton
              Left = 3
              Top = 65
              Width = 137
              Height = 25
              Caption = 'Get'
              TabOrder = 2
              OnClick = ButtonGroupsGetClick
            end
            object ButtonVideoDelete: TButton
              Left = 146
              Top = 3
              Width = 111
              Height = 25
              Caption = 'VideoDelete'
              TabOrder = 3
              OnClick = ButtonVideoDeleteClick
            end
          end
          object TabSheet10: TTabSheet
            Caption = 'Messages'
            ImageIndex = 9
            object ButtonMesGetConv: TButton
              Left = 3
              Top = 3
              Width = 166
              Height = 25
              Caption = 'GetConversations'
              TabOrder = 0
              OnClick = ButtonMesGetConvClick
            end
            object ButtonMesGetHistory: TButton
              Left = 3
              Top = 34
              Width = 166
              Height = 25
              Caption = 'GetHistory'
              TabOrder = 1
              OnClick = ButtonMesGetHistoryClick
            end
            object ButtonMesSendToPeer: TButton
              Left = 175
              Top = 3
              Width = 138
              Height = 25
              Caption = 'SendToPeer'
              TabOrder = 2
              OnClick = ButtonMesSendToPeerClick
            end
            object Button49: TButton
              Left = 3
              Top = 65
              Width = 166
              Height = 25
              Caption = 'SendToGroupChat'
              TabOrder = 3
              OnClick = Button49Click
            end
            object ButtonSendPhoto: TButton
              Left = 175
              Top = 34
              Width = 138
              Height = 25
              Caption = 'SendPhoto'
              TabOrder = 4
              OnClick = ButtonSendPhotoClick
            end
            object ButtonMessageGetChat: TButton
              Left = 3
              Top = 96
              Width = 166
              Height = 25
              Caption = 'GetChat'
              TabOrder = 7
              OnClick = ButtonMessageGetChatClick
            end
            object ButtonMessageGetConverstion: TButton
              Left = 3
              Top = 127
              Width = 166
              Height = 25
              Caption = 'GetConversation'
              TabOrder = 6
              OnClick = ButtonMessageGetConverstionClick
            end
            object ButtonSendAudioMessage: TButton
              Left = 175
              Top = 65
              Width = 138
              Height = 25
              Caption = 'SendAudioMessage'
              TabOrder = 5
              OnClick = ButtonSendAudioMessageClick
            end
            object ButtonGetMessageById: TButton
              Left = 3
              Top = 158
              Width = 166
              Height = 25
              Caption = 'GetMessageById'
              TabOrder = 8
              OnClick = ButtonGetMessageByIdClick
            end
          end
          object TabSheet11: TTabSheet
            Caption = 'Friends'
            ImageIndex = 10
            object ButtonFriendsGet: TButton
              Left = 3
              Top = 3
              Width = 137
              Height = 25
              Caption = 'Get'
              TabOrder = 0
              OnClick = ButtonFriendsGetClick
            end
            object ButtonGetFriendWithAudio: TButton
              Left = 3
              Top = 34
              Width = 137
              Height = 25
              Caption = 'GetFriendsWithAudio'
              TabOrder = 1
              OnClick = ButtonGetFriendWithAudioClick
            end
          end
          object TabSheetPolls: TTabSheet
            Caption = 'Polls'
            ImageIndex = 11
            object ButtonPollsGetBG: TButton
              Left = 3
              Top = 3
              Width = 137
              Height = 25
              Caption = 'GetBackgrounds'
              TabOrder = 0
              OnClick = ButtonPollsGetBGClick
            end
          end
          object TabSheetPodcasts: TTabSheet
            Caption = 'Podcasts'
            ImageIndex = 12
            object ButtonPodcastsSearch: TButton
              Left = 3
              Top = 3
              Width = 137
              Height = 25
              Caption = 'Search'
              TabOrder = 0
              OnClick = ButtonPodcastsSearchClick
            end
          end
          object TabSheetNewsfeed: TTabSheet
            Caption = 'Newsfeed'
            ImageIndex = 13
            object ButtonNewsfeedGet: TButton
              Left = 3
              Top = 3
              Width = 102
              Height = 25
              Caption = 'Get'
              TabOrder = 0
              OnClick = ButtonNewsfeedGetClick
            end
          end
          object TabSheetAds: TTabSheet
            Caption = 'Ads'
            ImageIndex = 14
            object ButtonAdsGetAccounts: TButton
              Left = 3
              Top = 3
              Width = 142
              Height = 25
              Caption = 'GetAccounts'
              TabOrder = 0
              OnClick = ButtonAdsGetAccountsClick
            end
          end
          object TabSheetPhotos: TTabSheet
            Caption = 'Photos'
            ImageIndex = 15
            object ButtonPhotosGetAlbum: TButton
              Left = 3
              Top = 3
              Width = 110
              Height = 25
              Caption = 'GetAlbum'
              TabOrder = 0
              OnClick = ButtonPhotosGetAlbumClick
            end
          end
          object TabSheetMarket: TTabSheet
            Caption = 'Market'
            ImageIndex = 16
            object ButtonMarketFilterCategories: TButton
              Left = 3
              Top = 3
              Width = 126
              Height = 25
              Caption = 'FilterCategories'
              TabOrder = 0
              OnClick = ButtonMarketFilterCategoriesClick
            end
          end
          object TabSheetPages: TTabSheet
            Caption = 'Pages'
            ImageIndex = 17
            object ButtonPagesSave: TButton
              Left = 3
              Top = 3
              Width = 75
              Height = 25
              Caption = 'PagesSave'
              TabOrder = 0
              OnClick = ButtonPagesSaveClick
            end
            object ButtonPagesGet: TButton
              Left = 3
              Top = 34
              Width = 75
              Height = 25
              Caption = 'PagesGet'
              TabOrder = 1
              OnClick = ButtonPagesGetClick
            end
            object EditPageiD: TEdit
              Left = 84
              Top = 34
              Width = 121
              Height = 25
              TabOrder = 2
              Text = 'EditPageiD'
            end
          end
        end
      end
    end
  end
  object VK1: TVK
    AppID = '7245537'
    BaseURL = 'https://api.vk.com/method'
    EndPoint = 'https://oauth.vk.com/authorize'
    Logging = True
    LogResponse = True
    OnAuth = VK1Auth
    OnError = VK1Error
    OnLog = VK1Log
    OnLogin = VK1Login
    Permissions = [Notify, Friends, Photos, Audio, Video, Stories, Pages, Status, Notes, Messages, Wall, Ads, Offline, Docs, Groups, Notifications, Stats, Email, Market, AppWidget, Manage]
    Proxy.Port = 0
    Left = 480
    Top = 512
  end
  object VkUserEvents1: TVkUserEvents
    Logging = True
    OnChangeDialogFlags = VkUserEvents1ChangeDialogFlags
    OnChangeMessageFlags = VkUserEvents1ChangeMessageFlags
    OnCountChange = VkUserEvents1CountChange
    OnEditMessage = VkUserEvents1EditMessage
    OnNewMessage = VkUserEvents1NewMessage
    OnUnhandledEvents = VkUserEvents1UnhandledEvents
    OnUsersRecording = VkUserEvents1UsersRecording
    OnUsersTyping = VkUserEvents1UsersTyping
    Version = '3'
    VK = VK1
    Left = 600
    Top = 512
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
    Left = 480
    Top = 568
  end
  object VkGroupEvents1: TVkGroupEvents
    Logging = True
    Version = '3'
    VK = VK1
    Left = 480
    Top = 624
  end
end
