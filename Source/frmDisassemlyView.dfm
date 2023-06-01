object DisForm: TDisForm
  Left = 281
  Top = 152
  HelpContext = 860
  Caption = 'Disassembly'
  ClientHeight = 382
  ClientWidth = 599
  Color = clBtnFace
  ParentFont = True
  OnCreate = FormCreate
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
    Gutter.Font.Quality = fqClearTypeNatural
    Gutter.ShowLineNumbers = True
    Gutter.Bands = <
      item
        Kind = gbkMarks
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
        Width = 3
      end>
    SelectedColor.Alpha = 0.400000005960464500
  end
end
