object Form14: TForm14
  Left = 0
  Top = 0
  Caption = 'VK API Helper'
  ClientHeight = 600
  ClientWidth = 1036
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object PageControl1: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 1030
    Height = 594
    ActivePage = TabSheet2
    Align = alClient
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabHeight = 30
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = #1043#1077#1085#1077#1088#1072#1094#1080#1103' '#1089#1087#1080#1089#1082#1072' '#1087#1072#1088#1072#1084#1077#1090#1088#1086#1074
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      object MemoIn: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 34
        Width = 310
        Height = 479
        Align = alLeft
        Lines.Strings = (
          'owner_id'
          ''
          ''
          ''
          'video_id'
          ''
          'message'
          ''
          'attachments'
          ''
          ''
          ''
          ''
          ''
          ''
          ''
          ''
          ''
          ''
          ''
          ''
          ''
          ''
          ''
          ''
          'from_group'
          ''
          'reply_to_comment'
          ''
          'sticker_id'
          ''
          'guid')
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object MemoOut: TMemo
        AlignWithMargins = True
        Left = 319
        Top = 34
        Width = 700
        Height = 479
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'TVkParamsVideosAddToAlbum = record'
          '  List: TParams;'
          '  function OwnerId(Value: Int64): TVkParamsVideosAddToAlbum;'
          '  function VideoId(Value: Int64): TVkParamsVideosAddToAlbum;'
          '  function Message(Value: string): TVkParamsVideosAddToAlbum;'
          '  function Attachments(Value: TIds): TVkParamsVideosAddToAlbum;'
          '  function FromGroup(Value: Int64): TVkParamsVideosAddToAlbum;'
          
            '  function ReplyToComment(Value: string): TVkParamsVideosAddToAl' +
            'bum;'
          '  function StickerId(Value: Int64): TVkParamsVideosAddToAlbum;'
          '  function Guid(Value: Int64): TVkParamsVideosAddToAlbum;'
          'end;'
          ''
          ''
          '{ TVkParamsVideosAddToAlbum }'
          ''
          
            'function TVkParamsVideosAddToAlbum.OwnerId(Value: Int64): TVkPar' +
            'amsVideosAddToAlbum;'
          'begin'
          '  List.Add('#39'owner_id'#39', Value);'
          '  Result := Self;'
          'end;'
          ''
          
            'function TVkParamsVideosAddToAlbum.VideoId(Value: Int64): TVkPar' +
            'amsVideosAddToAlbum;'
          'begin'
          '  List.Add('#39'video_id'#39', Value);'
          '  Result := Self;'
          'end;'
          ''
          
            'function TVkParamsVideosAddToAlbum.Message(Value: string): TVkPa' +
            'ramsVideosAddToAlbum;'
          'begin'
          '  List.Add('#39'message'#39', Value);'
          '  Result := Self;'
          'end;'
          ''
          
            'function TVkParamsVideosAddToAlbum.Attachments(Value: TIds): TVk' +
            'ParamsVideosAddToAlbum;'
          'begin'
          '  List.Add('#39'attachments'#39', Value);'
          '  Result := Self;'
          'end;'
          ''
          
            'function TVkParamsVideosAddToAlbum.FromGroup(Value: Int64): TVkP' +
            'aramsVideosAddToAlbum;'
          'begin'
          '  List.Add('#39'from_group'#39', Value);'
          '  Result := Self;'
          'end;'
          ''
          
            'function TVkParamsVideosAddToAlbum.ReplyToComment(Value: string)' +
            ': TVkParamsVideosAddToAlbum;'
          'begin'
          '  List.Add('#39'reply_to_comment'#39', Value);'
          '  Result := Self;'
          'end;'
          ''
          
            'function TVkParamsVideosAddToAlbum.StickerId(Value: Int64): TVkP' +
            'aramsVideosAddToAlbum;'
          'begin'
          '  List.Add('#39'sticker_id'#39', Value);'
          '  Result := Self;'
          'end;'
          ''
          
            'function TVkParamsVideosAddToAlbum.Guid(Value: Int64): TVkParams' +
            'VideosAddToAlbum;'
          'begin'
          '  List.Add('#39'guid'#39', Value);'
          '  Result := Self;'
          'end;')
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 1
      end
      object EditName: TEdit
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 1016
        Height = 25
        Align = alTop
        TabOrder = 2
        Text = 'TVkParamsVideosAddToAlbum'
        TextHint = 'TypeName'
      end
      object Panel1: TPanel
        Left = 0
        Top = 516
        Width = 1022
        Height = 38
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 3
        object ButtonParse: TButton
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 100
          Height = 32
          Align = alLeft
          Caption = 'Parse'
          TabOrder = 0
          OnClick = ButtonParseClick
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = #1043#1077#1085#1077#1088#1072#1094#1080#1103' Enum'
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ImageIndex = 1
      ParentFont = False
      object MemoTypes: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 34
        Width = 1016
        Height = 65
        Align = alTop
        Lines.Strings = (
          '"waiting", "started", "finished", "failed", "upcoming"')
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object MemoTypesOut: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 146
        Width = 1016
        Height = 405
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'TVkLiveStatus = (Waiting, Started, Finished, Failed, Upcoming);'
          ''
          'TVkLiveStatusHelper = record helper for TVkLiveStatus'
          '  function ToString: string; inline;'
          
            '  class function Create(const Value: string): TVkLiveStatus; sta' +
            'tic;'
          'end;'
          ''
          
            'VkLiveStatus: array[TVkLiveStatus] of string = ('#39'waiting'#39', '#39'star' +
            'ted'#39', '#39'finished'#39', '#39'failed'#39', '#39'upcoming'#39');'
          ''
          '{ TVkLiveStatusHelper }'
          ''
          
            'class function TVkLiveStatusHelper.Create(const Value: string): ' +
            'TVkLiveStatus;'
          'begin'
          '  Result := TVkLiveStatus(IndexStr(Value, VkLiveStatus));'
          'end;'
          ''
          'function TVkLiveStatusHelper.ToString: string;'
          'begin'
          '  Result := VkLiveStatus[Self];'
          'end;'
          ''
          '---------------------------------------------------'
          'TLiveStatusInterceptor = class(TEnumInterceptor<TVkLiveStatus>)'
          'public'
          
            '  function StringConverter(Data: TObject; Field: string): string' +
            '; override;'
          
            '  procedure StringReverter(Data: TObject; Field: string; Arg: st' +
            'ring); override;'
          'end;'
          ''
          '{ TLiveStatusInterceptor }'
          ''
          
            'function TLiveStatusInterceptor.StringConverter(Data: TObject; F' +
            'ield: string): string;'
          'begin'
          
            '  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValu' +
            'e(Data).AsType<TVkLiveStatus>.ToString;'
          'end;'
          ''
          
            'procedure TLiveStatusInterceptor.StringReverter(Data: TObject; F' +
            'ield, Arg: string);'
          'begin'
          
            '  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TV' +
            'alue.From(TVkLiveStatus.Create(Arg)));'
          'end;')
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 1
      end
      object EditType: TEdit
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 1016
        Height = 25
        Align = alTop
        TabOrder = 2
        Text = 'TVkLiveStatus'
        TextHint = 'TVkLivestatus'
      end
      object Panel2: TPanel
        Left = 0
        Top = 102
        Width = 1022
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 3
        object Button1: TButton
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 100
          Height = 35
          Align = alLeft
          Caption = 'Generate'
          TabOrder = 0
          OnClick = Button1Click
        end
      end
    end
  end
end
