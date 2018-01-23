object DisForm: TDisForm
  Left = 281
  Top = 152
  HelpContext = 860
  Caption = 'Disassembly View'
  ClientHeight = 382
  ClientWidth = 599
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object DisSynEdit: TSynEdit
    Left = 0
    Top = 0
    Width = 599
    Height = 382
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 0
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.Visible = False
    Gutter.Width = 0
    Highlighter = SynPythonSyn
    FontSmoothing = fsmNone
  end
  object SynPythonSyn: TSynPythonSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    CommentAttri.Foreground = clMaroon
    CommentAttri.Style = []
    KeyAttri.Foreground = clHotLight
    NumberAttri.Foreground = clTeal
    SpaceAttri.Background = clWhite
    StringAttri.Foreground = clPurple
    DocStringAttri.Foreground = clMaroon
    Left = 537
    Top = 11
  end
end
