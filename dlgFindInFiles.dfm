inherited FindInFilesDialog: TFindInFilesDialog
  Left = 412
  Top = 114
  HelpContext = 810
  Caption = 'Find in Files Search'
  ClientHeight = 301
  ClientWidth = 374
  Position = poScreenCenter
  ExplicitWidth = 380
  ExplicitHeight = 329
  PixelsPerInch = 96
  TextHeight = 13
  object SpTBXPanel1: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 374
    Height = 301
    Caption = 'SpTBXPanel1'
    Align = alClient
    TabOrder = 0
    TBXStyleBackground = True
    DesignSize = (
      374
      301)
    object gbxOptions: TSpTBXGroupBox
      Left = 8
      Top = 53
      Width = 167
      Height = 106
      Caption = 'Options'
      Anchors = [akLeft, akBottom]
      TabOrder = 2
      TBXStyleBackground = True
      object cbCaseSensitive: TSpTBXCheckBox
        Left = 8
        Top = 16
        Width = 93
        Height = 21
        Caption = '&Case sensitive'
        ParentColor = True
        TabOrder = 0
      end
      object cbNoComments: TSpTBXCheckBox
        Left = 8
        Top = 79
        Width = 107
        Height = 21
        Caption = '&Ignore comments'
        ParentColor = True
        TabOrder = 3
      end
      object cbWholeWord: TSpTBXCheckBox
        Left = 8
        Top = 36
        Width = 81
        Height = 21
        Caption = '&Whole word'
        ParentColor = True
        TabOrder = 1
      end
      object cbRegEx: TSpTBXCheckBox
        Left = 8
        Top = 58
        Width = 116
        Height = 21
        Caption = 'Regular e&xpression'
        ParentColor = True
        TabOrder = 2
      end
    end
    object gbxWhere: TSpTBXGroupBox
      Left = 184
      Top = 53
      Width = 183
      Height = 106
      Caption = 'Where'
      Anchors = [akRight, akBottom]
      TabOrder = 3
      TBXStyleBackground = True
      object rbOpenFiles: TSpTBXRadioButton
        Left = 8
        Top = 36
        Width = 72
        Height = 21
        Caption = '&Open files'
        ParentColor = True
        TabOrder = 1
        OnClick = rbProjectClick
      end
      object rbProject: TSpTBXRadioButton
        Left = 8
        Top = 58
        Width = 80
        Height = 21
        Caption = '&Project files'
        ParentColor = True
        TabOrder = 2
        OnClick = rbProjectClick
      end
      object rbCurrentOnly: TSpTBXRadioButton
        Left = 8
        Top = 16
        Width = 101
        Height = 21
        Caption = 'Current &file only'
        ParentColor = True
        TabOrder = 0
        OnClick = rbProjectClick
      end
      object rbDirectories: TSpTBXRadioButton
        Left = 8
        Top = 79
        Width = 121
        Height = 21
        Caption = 'Search in &directories'
        ParentColor = True
        TabOrder = 3
        OnClick = rbProjectClick
      end
    end
    object gbxDirectories: TSpTBXGroupBox
      Left = 8
      Top = 166
      Width = 360
      Height = 97
      Caption = 'Directory Search'
      Anchors = [akLeft, akRight, akBottom]
      TabOrder = 1
      TBXStyleBackground = True
      DesignSize = (
        360
        97)
      object btnBrowse: TSpTBXButton
        Left = 335
        Top = 22
        Width = 20
        Height = 20
        Hint = 'Select Directory'
        Anchors = [akTop, akRight]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        TabStop = False
        OnClick = btnBrowseClick
        Images = CommandsDataModule.Images
        ImageIndex = 45
      end
      object cbInclude: TSpTBXCheckBox
        Left = 78
        Top = 75
        Width = 129
        Height = 21
        Caption = 'Include su&bdirectories'
        Anchors = [akLeft, akTop, akRight]
        ParentColor = True
        TabOrder = 3
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
      end
      object lblMasks: TSpTBXLabel
        Left = 3
        Top = 52
        Width = 58
        Height = 19
        Caption = 'File mas&ks:'
        Alignment = taRightJustify
        FocusControl = cbMasks
      end
      object lblDirectory: TSpTBXLabel
        Left = 2
        Top = 26
        Width = 61
        Height = 19
        Caption = 'Di&rectories:'
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
    end
    object lblFind: TSpTBXLabel
      Left = 8
      Top = 3
      Width = 66
      Height = 19
      Caption = '&Text to find:'
      Alignment = taRightJustify
      FocusControl = cbText
    end
  end
end
