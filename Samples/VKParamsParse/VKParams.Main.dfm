object Form14: TForm14
  Left = 0
  Top = 0
  Caption = 'Form14'
  ClientHeight = 600
  ClientWidth = 889
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 889
    Height = 600
    ActivePage = TabSheet2
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      DesignSize = (
        881
        569)
      object MemoIn: TMemo
        Left = 8
        Top = 38
        Width = 433
        Height = 491
        Anchors = [akLeft, akTop, akBottom]
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
      object ButtonParse: TButton
        Left = 8
        Top = 535
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Parse'
        TabOrder = 1
        OnClick = ButtonParseClick
      end
      object MemoOut: TMemo
        Left = 447
        Top = 38
        Width = 433
        Height = 491
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
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
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 2
      end
      object EditName: TEdit
        Left = 8
        Top = 8
        Width = 433
        Height = 24
        TabOrder = 3
        Text = 'TVkParamsVideosAddToAlbum'
        TextHint = 'TypeName'
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
      DesignSize = (
        881
        569)
      object MemoTypes: TMemo
        Left = 11
        Top = 40
        Width = 854
        Height = 65
        Anchors = [akLeft, akTop, akRight]
        Lines.Strings = (
          '"waiting", "started", "finished", "failed", "upcoming"')
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object Button1: TButton
        Left = 11
        Top = 111
        Width = 75
        Height = 25
        Caption = 'Generate'
        TabOrder = 1
        OnClick = Button1Click
      end
      object MemoTypesOut: TMemo
        Left = 11
        Top = 142
        Width = 854
        Height = 411
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 2
      end
      object EditType: TEdit
        Left = 11
        Top = 10
        Width = 230
        Height = 24
        TabOrder = 3
        Text = 'TVkLiveStatus'
        TextHint = 'TVkLivestatus'
      end
    end
  end
end
