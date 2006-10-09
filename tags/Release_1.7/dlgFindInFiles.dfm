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
    object cbNoCase: TCheckBox
      Left = 8
      Top = 16
      Width = 148
      Height = 17
      Caption = '&Case sensitive'
      TabOrder = 0
    end
    object cbNoComments: TCheckBox
      Left = 8
      Top = 79
      Width = 148
      Height = 17
      Caption = '&Ignore comments'
      TabOrder = 3
    end
    object cbWholeWord: TCheckBox
      Left = 8
      Top = 36
      Width = 148
      Height = 17
      Caption = '&Whole word'
      TabOrder = 1
    end
    object cbRegEx: TCheckBox
      Left = 8
      Top = 58
      Width = 148
      Height = 17
      Caption = 'Regular e&xpression'
      TabOrder = 2
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
    object rbOpenFiles: TRadioButton
      Left = 8
      Top = 36
      Width = 168
      Height = 17
      Caption = '&Open files'
      TabOrder = 1
      OnClick = rbDirectoriesClick
    end
    object rbDirectories: TRadioButton
      Left = 8
      Top = 57
      Width = 168
      Height = 17
      Caption = 'Search in &directories'
      TabOrder = 2
      OnClick = rbDirectoriesClick
    end
    object rbCurrentOnly: TRadioButton
      Left = 8
      Top = 16
      Width = 168
      Height = 17
      Caption = 'Current &file only'
      TabOrder = 0
      OnClick = rbDirectoriesClick
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
    object cbInclude: TCheckBox
      Left = 70
      Top = 72
      Width = 266
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Include su&bdirectories'
      TabOrder = 3
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
    object btnBrowse: TButton
      Left = 313
      Top = 22
      Width = 20
      Height = 20
      Hint = 'Select Directory'
      Anchors = [akTop, akRight]
      Caption = '...'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      TabStop = False
      OnClick = btnBrowseClick
    end
  end
  object btnOK: TButton
    Left = 120
    Top = 256
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 4
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 200
    Top = 256
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object btnHelp: TButton
    Left = 280
    Top = 256
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 6
    OnClick = btnHelpClick
  end
end
