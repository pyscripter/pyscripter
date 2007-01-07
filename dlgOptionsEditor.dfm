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
      Align = alClient
      BevelKind = bkFlat
      BevelInner = bvRaised
      BevelOuter = bvRaised
      RelativeDivider = True
      Divider = 62
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
      Caption = '&OK'
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
      Caption = '&Cancel'
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
    DrawNameEndEllipsis = False
    Left = 369
    Top = 14
  end
end
