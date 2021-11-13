object DisForm: TDisForm
  Left = 281
  Top = 152
  HelpContext = 860
  Caption = 'Disassembly View'
  ClientHeight = 382
  ClientWidth = 599
  Color = clBtnFace
  ParentFont = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
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
    Font.Quality = fqClearTypeNatural
    TabOrder = 0
    UseCodeFolding = False
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.ShowLineNumbers = True
    Gutter.Bands = <
      item
        Kind = gbkMarks
        Visible = True
        Width = 13
      end
      item
        Kind = gbkLineNumbers
      end
      item
        Kind = gbkFold
      end
      item
        Kind = gbkMargin
        Visible = True
        Width = 3
      end>
  end
end
