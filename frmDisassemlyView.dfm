object DisForm: TDisForm
  Left = 281
  Top = 152
  HelpContext = 860
  Caption = 'Dissassemby View'
  ClientHeight = 382
  ClientWidth = 599
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
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
    Font.Name = 'Lucida Console'
    Font.Style = []
    TabOrder = 0
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.Visible = False
    Highlighter = SynPythonSyn
  end
  object SynPythonSyn: TSynPythonSyn
    CommentAttri.Foreground = clMaroon
    CommentAttri.Style = []
    KeyAttri.Foreground = clBlue
    NumberAttri.Foreground = clTeal
    StringAttri.Foreground = clPurple
    DocStringAttri.Foreground = clMaroon
    Left = 537
    Top = 11
  end
end
