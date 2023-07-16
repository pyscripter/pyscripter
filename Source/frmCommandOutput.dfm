inherited OutputWindow: TOutputWindow
  Left = 319
  Top = 173
  HelpContext = 450
  Caption = 'Output'
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    0000010020000000000040040000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000B6A1
    98FF8F7B6AFF8F7B6AFF8F7B6AFF8F7B6AFF8F7B6AFF8F7B6AFF8F7B6AFF8F7B
    6AFF8F7B6AFF8F7B6AFF8F7B6AFF8F7B6AFF8F7B6AFF8F7B6AFFB6ABA1FF7340
    2AFF402311FF402311FF402311FF402311FF402311FF402311FF402311FF4023
    11FF402311FF402311FF402311FF402311FF402311FF582311FF6A5040FF5040
    31FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
    00FF000000FF000000FF000000FF000000FF000000FF381606FF735848FF5040
    31FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
    00FF000000FF000000FF000000FF000000FF000000FF381606FF735848FF5040
    31FF000000FF313131FFFFFFFFFFFFFFFFFF313131FF000000FF000000FF0000
    00FF000000FF313131FFFFFFFFFF000000FF000000FF381606FF735848FF5038
    31FF000000FFFFFFFFFF313131FF313131FFFFFFFFFF000000FFFFFFFFFF0000
    00FF000000FFFFFFFFFF313131FF000000FF000000FF381606FF6A5040FF4838
    31FF000000FFFFFFFFFF000000FF000000FF000000FF000000FF000000FF0000
    00FF313131FFFFFFFFFF000000FF000000FF000000FF381606FF735848FF4838
    31FF000000FFFFFFFFFF313131FF313131FFFFFFFFFF000000FFFFFFFFFF0000
    00FFFFFFFFFF313131FF000000FF000000FF000000FF381606FF735848FF5040
    31FF000000FF313131FFFFFFFFFFFFFFFFFF313131FF000000FF000000FF0000
    00FFFFFFFFFF000000FF000000FF000000FF000000FF381606FF735848FF5038
    31FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
    00FF000000FF000000FF000000FF000000FF000000FF381606FF6A5040FF8F60
    38FF58401CFF58401CFF58401CFF58401CFF58401CFF58401CFF58401CFF5840
    1CFF58401CFF584823FF584823FF584823FF50402AFF6A3816FF6A5040FFD46A
    11FFEA730BFFEA730BFFEA730BFFEA730BFFEA730BFFEA730BFFEA730BFFEA73
    0BFFEA730BFFFFA140FFFF9838FFFFA148FF8F737BFF8F4023FF846A58FFA16A
    40FFAB5823FFB6582AFFB6582AFFB6582AFFB6582AFFB6582AFFB6582AFFB658
    2AFFB6582AFFB6602AFFB6602AFFB6602AFFA15831FF986040FFB6ABA1FF0000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    0000FFFF00000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000FFFF0000}
  TextHeight = 15
  inherited BGPanel: TPanel
    inherited FGPanel: TPanel
      object lsbConsole: TListBox
        Left = 0
        Top = 0
        Width = 223
        Height = 394
        TabStop = False
        Align = alClient
        BevelInner = bvNone
        BorderStyle = bsNone
        Ctl3D = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        ItemHeight = 15
        ParentCtl3D = False
        ParentFont = False
        PopupMenu = OutputPopup
        TabOrder = 0
      end
    end
  end
  object OutputPopup: TSpTBXPopupMenu
    Images = vilImages
    Left = 28
    Top = 76
    object RunningProcess: TSpTBXSubmenuItem
      Caption = 'Running Process'
      object mnTerminate: TSpTBXItem
        Action = actToolTerminate
      end
    end
    object N1: TSpTBXSeparatorItem
    end
    object mnCopy: TSpTBXItem
      Action = actCopy
    end
    object mnClear: TSpTBXItem
      Action = actClearOutput
    end
    object N2: TSpTBXSeparatorItem
    end
    object mnFont: TSpTBXItem
      Action = actOutputFont
    end
  end
  object OutputActions: TActionList
    Images = vilImages
    OnUpdate = OutputActionsUpdate
    Left = 27
    Top = 130
    object actCopy: TAction
      Caption = 'Co&py'
      Hint = 'Copy contents to Clipboard'
      ImageIndex = 0
      ImageName = 'Copy'
      OnExecute = actCopyExecute
    end
    object actOutputFont: TAction
      Caption = 'Font...'
      Hint = 'Select font'
      ImageIndex = 5
      ImageName = 'Font'
      OnExecute = actOutputFontExecute
    end
    object actClearOutput: TAction
      Caption = 'Clear'
      Hint = 'Clear output'
      ImageIndex = 1
      ImageName = 'Delete'
      OnExecute = actClearOutputExecute
    end
    object actToolTerminate: TAction
      Caption = '&Terminate'
      Hint = 'Terminate running tool (unsafe)'
      ImageIndex = 2
      ImageName = 'Stop'
      OnExecute = actToolTerminateExecute
    end
  end
  object vilImages: TVirtualImageList
    Images = <
      item
        CollectionIndex = 16
        CollectionName = 'Copy'
        Name = 'Copy'
      end
      item
        CollectionIndex = 21
        CollectionName = 'Delete'
        Name = 'Delete'
      end
      item
        CollectionIndex = 111
        CollectionName = 'Stop'
        Name = 'Stop'
      end
      item
        CollectionIndex = 30
        CollectionName = 'Exit'
        Name = 'Exit'
      end
      item
        CollectionIndex = 86
        CollectionName = 'Quit'
        Name = 'Quit'
      end
      item
        CollectionIndex = 47
        CollectionName = 'Font'
        Name = 'Font'
      end>
    ImageCollection = ResourcesDataModule.icSVGImages
    PreserveItems = True
    Width = 20
    Height = 20
    Left = 24
    Top = 240
  end
end
