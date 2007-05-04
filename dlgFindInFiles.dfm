object FindInFilesDialog: TFindInFilesDialog
  Left = 412
  Top = 114
  HelpContext = 810
  BorderStyle = bsDialog
  Caption = 'Find in Files Search'
  ClientHeight = 288
  ClientWidth = 361
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  DesignSize = (
    361
    288)
  PixelsPerInch = 96
  TextHeight = 13
  object lblFind: TLabel
    Left = 21
    Top = 12
    Width = 53
    Height = 13
    Alignment = taRightJustify
    Caption = '&Text to find'
    FocusControl = cbText
  end
  object cbText: TComboBox
    Left = 80
    Top = 8
    Width = 275
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 15
    ItemHeight = 13
    TabOrder = 0
  end
  object gbxOptions: TGroupBox
    Left = 8
    Top = 40
    Width = 161
    Height = 106
    Caption = 'Options'
    TabOrder = 1
    object cbNoCase: TSpTBXCheckBox
      Left = 8
      Top = 16
      Width = 86
      Height = 15
      Caption = '&Case sensitive'
      ParentColor = True
      TabOrder = 0
      ThemeType = thtWindows
    end
    object cbNoComments: TSpTBXCheckBox
      Left = 8
      Top = 79
      Width = 99
      Height = 15
      Caption = '&Ignore comments'
      ParentColor = True
      TabOrder = 3
      ThemeType = thtWindows
    end
    object cbWholeWord: TSpTBXCheckBox
      Left = 8
      Top = 36
      Width = 75
      Height = 15
      Caption = '&Whole word'
      ParentColor = True
      TabOrder = 1
      ThemeType = thtWindows
    end
    object cbRegEx: TSpTBXCheckBox
      Left = 8
      Top = 58
      Width = 108
      Height = 15
      Caption = 'Regular e&xpression'
      ParentColor = True
      TabOrder = 2
      ThemeType = thtWindows
    end
  end
  object gbxWhere: TGroupBox
    Left = 176
    Top = 40
    Width = 178
    Height = 106
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Where'
    TabOrder = 2
    object rbOpenFiles: TSpTBXRadioButton
      Left = 8
      Top = 36
      Width = 65
      Height = 15
      Caption = '&Open files'
      ParentColor = True
      TabOrder = 1
      OnClick = rbDirectoriesClick
      ThemeType = thtWindows
    end
    object rbDirectories: TSpTBXRadioButton
      Left = 8
      Top = 57
      Width = 114
      Height = 15
      Caption = 'Search in &directories'
      ParentColor = True
      TabOrder = 2
      OnClick = rbDirectoriesClick
      ThemeType = thtWindows
    end
    object rbCurrentOnly: TSpTBXRadioButton
      Left = 8
      Top = 16
      Width = 90
      Height = 15
      Caption = 'Current &file only'
      ParentColor = True
      TabOrder = 0
      OnClick = rbDirectoriesClick
      ThemeType = thtWindows
    end
  end
  object gbxDirectories: TGroupBox
    Left = 8
    Top = 152
    Width = 347
    Height = 97
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Directory Search'
    TabOrder = 3
    DesignSize = (
      347
      97)
    object lblMasks: TLabel
      Left = 15
      Top = 52
      Width = 49
      Height = 13
      Alignment = taRightJustify
      Caption = 'File mas&ks'
      FocusControl = cbMasks
    end
    object lblDirectory: TLabel
      Left = 14
      Top = 26
      Width = 50
      Height = 13
      Alignment = taRightJustify
      Caption = 'Di&rectories'
      FocusControl = cbDirectory
    end
    object cbMasks: TComboBox
      Left = 70
      Top = 48
      Width = 264
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 15
      ItemHeight = 13
      TabOrder = 2
    end
    object cbDirectory: TComboBox
      Left = 70
      Top = 22
      Width = 242
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 15
      ItemHeight = 13
      TabOrder = 0
      OnDropDown = cbDirectoryDropDown
    end
    object btnBrowse: TSpTBXButton
      Left = 313
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
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'Tahoma'
      LinkFont.Style = [fsUnderline]
    end
    object cbInclude: TSpTBXCheckBox
      Left = 70
      Top = 72
      Width = 121
      Height = 15
      Caption = 'Include su&bdirectories'
      Anchors = [akLeft, akTop, akRight]
      ParentColor = True
      TabOrder = 3
      ThemeType = thtWindows
    end
  end
  object btnOK: TSpTBXButton
    Left = 120
    Top = 256
    Width = 75
    Height = 25
    Caption = 'OK'
    Anchors = [akRight, akBottom]
    TabOrder = 4
    OnClick = btnOKClick
    Default = True
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'Tahoma'
    LinkFont.Style = [fsUnderline]
  end
  object btnCancel: TSpTBXButton
    Left = 200
    Top = 256
    Width = 75
    Height = 25
    Caption = 'Cancel'
    Anchors = [akRight, akBottom]
    TabOrder = 5
    Cancel = True
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'Tahoma'
    LinkFont.Style = [fsUnderline]
    ModalResult = 2
  end
  object btnHelp: TSpTBXButton
    Left = 280
    Top = 256
    Width = 75
    Height = 25
    Caption = '&Help'
    Anchors = [akRight, akBottom]
    TabOrder = 6
    OnClick = btnHelpClick
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'Tahoma'
    LinkFont.Style = [fsUnderline]
  end
end
