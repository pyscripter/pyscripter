inherited FindInFilesDialog: TFindInFilesDialog
  Left = 412
  Top = 114
  HelpContext = 810
  Caption = 'Find in Files Search'
  ClientHeight = 301
  ClientWidth = 374
  Font.Name = 'MS Shell Dlg 2'
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  ExplicitWidth = 380
  ExplicitHeight = 327
  DesignSize = (
    374
    301)
  PixelsPerInch = 96
  TextHeight = 13
  object gbxOptions: TSpTBXGroupBox
    Left = 8
    Top = 53
    Width = 167
    Height = 106
    Caption = 'Options'
    SkinType = sknSkin
    Anchors = [akLeft, akBottom]
    TabOrder = 2
    object cbNoCase: TSpTBXCheckBox
      Left = 8
      Top = 16
      Width = 87
      Height = 15
      Caption = '&Case sensitive'
      ParentColor = True
      TabOrder = 0
      SkinType = sknSkin
    end
    object cbNoComments: TSpTBXCheckBox
      Left = 8
      Top = 79
      Width = 101
      Height = 15
      Caption = '&Ignore comments'
      ParentColor = True
      TabOrder = 3
      SkinType = sknSkin
    end
    object cbWholeWord: TSpTBXCheckBox
      Left = 8
      Top = 36
      Width = 75
      Height = 15
      Caption = '&Whole word'
      ParentColor = True
      TabOrder = 1
      SkinType = sknSkin
    end
    object cbRegEx: TSpTBXCheckBox
      Left = 8
      Top = 58
      Width = 110
      Height = 15
      Caption = 'Regular e&xpression'
      ParentColor = True
      TabOrder = 2
      SkinType = sknSkin
    end
  end
  object gbxWhere: TSpTBXGroupBox
    Left = 184
    Top = 53
    Width = 183
    Height = 106
    Caption = 'Where'
    SkinType = sknSkin
    Anchors = [akRight, akBottom]
    TabOrder = 3
    object rbOpenFiles: TSpTBXRadioButton
      Left = 8
      Top = 36
      Width = 66
      Height = 15
      Caption = '&Open files'
      ParentColor = True
      TabOrder = 1
      OnClick = rbProjectClick
      SkinType = sknSkin
    end
    object rbProject: TSpTBXRadioButton
      Left = 8
      Top = 58
      Width = 74
      Height = 15
      Caption = '&Project files'
      ParentColor = True
      TabOrder = 2
      OnClick = rbProjectClick
      SkinType = sknSkin
    end
    object rbCurrentOnly: TSpTBXRadioButton
      Left = 8
      Top = 16
      Width = 95
      Height = 15
      Caption = 'Current &file only'
      ParentColor = True
      TabOrder = 0
      OnClick = rbProjectClick
      SkinType = sknSkin
    end
    object rbDirectories: TSpTBXRadioButton
      Left = 8
      Top = 79
      Width = 115
      Height = 15
      Caption = 'Search in &directories'
      ParentColor = True
      TabOrder = 3
      OnClick = rbProjectClick
      SkinType = sknSkin
    end
  end
  object gbxDirectories: TSpTBXGroupBox
    Left = 8
    Top = 166
    Width = 360
    Height = 97
    Caption = 'Directory Search'
    SkinType = sknSkin
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 1
    DesignSize = (
      360
      97)
    object btnBrowse: TSpTBXButton
      Left = 335
      Top = 22
      Width = 20
      Height = 20
      Hint = 'Select Directory'
      Caption = '...'
      Anchors = [akTop, akRight]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      TabStop = False
      OnClick = btnBrowseClick
    end
    object cbInclude: TSpTBXCheckBox
      Left = 78
      Top = 75
      Width = 123
      Height = 15
      Caption = 'Include su&bdirectories'
      Anchors = [akLeft, akTop, akRight]
      ParentColor = True
      TabOrder = 3
      SkinType = sknSkin
    end
    object cbMasks: TSpTBXComboBox
      Left = 78
      Top = 48
      Width = 277
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 15
      ItemHeight = 13
      TabOrder = 2
      SkinType = sknSkin
    end
    object cbDirectory: TSpTBXComboBox
      Left = 78
      Top = 21
      Width = 255
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 15
      ItemHeight = 13
      TabOrder = 0
      OnDropDown = cbDirectoryDropDown
      SkinType = sknSkin
    end
    object lblMasks: TSpTBXLabel
      Left = 15
      Top = 52
      Width = 52
      Height = 13
      Caption = 'File mas&ks:'
      ParentColor = True
      Alignment = taRightJustify
      FocusControl = cbMasks
    end
    object lblDirectory: TSpTBXLabel
      Left = 14
      Top = 26
      Width = 55
      Height = 13
      Caption = 'Di&rectories:'
      ParentColor = True
      Alignment = taRightJustify
      FocusControl = cbDirectory
    end
  end
  object btnOK: TSpTBXButton
    Left = 132
    Top = 269
    Width = 75
    Height = 25
    Caption = '&OK'
    Anchors = [akRight, akBottom]
    TabOrder = 4
    OnClick = btnOKClick
    Default = True
    ModalResult = 1
  end
  object btnCancel: TSpTBXButton
    Left = 213
    Top = 269
    Width = 75
    Height = 25
    Caption = '&Cancel'
    Anchors = [akRight, akBottom]
    TabOrder = 5
    Cancel = True
    ModalResult = 2
  end
  object btnHelp: TSpTBXButton
    Left = 293
    Top = 269
    Width = 75
    Height = 25
    Caption = '&Help'
    Anchors = [akRight, akBottom]
    TabOrder = 6
    OnClick = btnHelpClick
  end
  object cbText: TSpTBXComboBox
    Left = 8
    Top = 28
    Width = 358
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 15
    ItemHeight = 13
    TabOrder = 0
    SkinType = sknSkin
  end
  object lblFind: TSpTBXLabel
    Left = 8
    Top = 8
    Width = 60
    Height = 13
    Caption = '&Text to find:'
    ParentColor = True
    Alignment = taRightJustify
    FocusControl = cbText
  end
end
