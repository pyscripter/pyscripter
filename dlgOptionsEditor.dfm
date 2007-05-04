object OptionsInspector: TOptionsInspector
  Left = 437
  Top = 134
  Caption = 'Options Inspector'
  ClientHeight = 349
  ClientWidth = 321
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 321
    Height = 312
    Align = alClient
    BevelInner = bvLowered
    TabOrder = 0
    object Inspector: TJvInspector
      Left = 2
      Top = 2
      Width = 317
      Height = 308
      Style = isItemPainter
      Align = alClient
      BevelKind = bkFlat
      BevelInner = bvRaised
      BevelOuter = bvRaised
      RelativeDivider = True
      Divider = 61
      ItemHeight = 16
      Painter = JvInspectorDotNETPainter1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 312
    Width = 321
    Height = 37
    Align = alBottom
    Anchors = [akLeft, akBottom]
    TabOrder = 1
    DesignSize = (
      321
      37)
    object OKButton: TBitBtn
      Left = 30
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      TabOrder = 0
      OnClick = OKButtonClick
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 126
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      TabOrder = 1
      Kind = bkCancel
    end
    object HelpButton: TBitBtn
      Left = 222
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      TabOrder = 2
      OnClick = HelpButtonClick
      Kind = bkHelp
    end
  end
  object JvInspectorDotNETPainter1: TJvInspectorDotNETPainter
    CategoryFont.Charset = DEFAULT_CHARSET
    CategoryFont.Color = clBtnText
    CategoryFont.Height = -11
    CategoryFont.Name = 'Tahoma'
    CategoryFont.Style = []
    NameFont.Charset = DEFAULT_CHARSET
    NameFont.Color = clWindowText
    NameFont.Height = -11
    NameFont.Name = 'Tahoma'
    NameFont.Style = []
    ValueFont.Charset = DEFAULT_CHARSET
    ValueFont.Color = clWindowText
    ValueFont.Height = -11
    ValueFont.Name = 'Tahoma'
    ValueFont.Style = []
    DrawNameEndEllipsis = False
    HideSelectFont.Charset = DEFAULT_CHARSET
    HideSelectFont.Color = clHighlightText
    HideSelectFont.Height = -11
    HideSelectFont.Name = 'Tahoma'
    HideSelectFont.Style = []
    SelectedFont.Charset = DEFAULT_CHARSET
    SelectedFont.Color = clHighlightText
    SelectedFont.Height = -11
    SelectedFont.Name = 'Tahoma'
    SelectedFont.Style = []
    Left = 369
    Top = 14
  end
end
