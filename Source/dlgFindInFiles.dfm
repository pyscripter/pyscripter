inherited FindInFilesDialog: TFindInFilesDialog
  Left = 412
  Top = 114
  HelpContext = 810
  Caption = 'Find in Files Search'
  ClientHeight = 301
  ClientWidth = 388
  Position = poScreenCenter
  OnShow = FormShow
  ExplicitWidth = 394
  ExplicitHeight = 330
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 388
    Height = 301
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 374
    DesignSize = (
      388
      301)
    object lblFind: TLabel
      Left = 8
      Top = 9
      Width = 60
      Height = 13
      Alignment = taRightJustify
      Caption = '&Text to find:'
      FocusControl = cbText
    end
    object gbxOptions: TGroupBox
      Left = 10
      Top = 54
      Width = 169
      Height = 106
      Anchors = [akLeft]
      Caption = 'Options'
      TabOrder = 2
      object cbCaseSensitive: TCheckBox
        Left = 10
        Top = 16
        Width = 93
        Height = 21
        Caption = '&Case sensitive'
        TabOrder = 0
      end
      object cbNoComments: TCheckBox
        Left = 10
        Top = 79
        Width = 107
        Height = 21
        Caption = '&Ignore comments'
        TabOrder = 3
      end
      object cbWholeWord: TCheckBox
        Left = 10
        Top = 36
        Width = 81
        Height = 21
        Caption = '&Whole word'
        TabOrder = 1
      end
      object cbRegEx: TCheckBox
        Left = 10
        Top = 58
        Width = 116
        Height = 21
        Caption = 'Regular e&xpression'
        TabOrder = 2
      end
    end
    object gbxWhere: TGroupBox
      Left = 193
      Top = 53
      Width = 188
      Height = 106
      Anchors = [akRight]
      Caption = 'Where'
      TabOrder = 3
      object rbOpenFiles: TRadioButton
        Left = 11
        Top = 36
        Width = 72
        Height = 21
        Caption = '&Open files'
        TabOrder = 1
        OnClick = rbProjectClick
      end
      object rbProject: TRadioButton
        Left = 11
        Top = 58
        Width = 80
        Height = 21
        Caption = '&Project files'
        TabOrder = 2
        OnClick = rbProjectClick
      end
      object rbCurrentOnly: TRadioButton
        Left = 11
        Top = 16
        Width = 101
        Height = 21
        Caption = 'Current &file only'
        TabOrder = 0
        OnClick = rbProjectClick
      end
      object rbDirectories: TRadioButton
        Left = 11
        Top = 79
        Width = 121
        Height = 21
        Caption = 'Search in &directories'
        TabOrder = 3
        OnClick = rbProjectClick
      end
    end
    object gbxDirectories: TGroupBox
      Left = 8
      Top = 166
      Width = 374
      Height = 97
      Anchors = [akLeft, akRight, akBottom]
      Caption = 'Directory Search'
      TabOrder = 1
      ExplicitWidth = 360
      DesignSize = (
        374
        97)
      object lblMasks: TLabel
        Left = 9
        Top = 52
        Width = 52
        Height = 13
        Alignment = taRightJustify
        Caption = 'File mas&ks:'
        FocusControl = cbMasks
      end
      object lblDirectory: TLabel
        Left = 8
        Top = 26
        Width = 55
        Height = 13
        Alignment = taRightJustify
        Caption = 'Di&rectories:'
        FocusControl = cbDirectory
      end
      object btnBrowse: TButton
        Left = 349
        Top = 22
        Width = 20
        Height = 20
        Hint = 'Select Directory'
        Anchors = [akTop, akRight]
        ImageIndex = 45
        Images = CommandsDataModule.Images
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        TabStop = False
        OnClick = btnBrowseClick
        ExplicitLeft = 335
      end
      object cbInclude: TCheckBox
        Left = 78
        Top = 75
        Width = 200
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Include su&bdirectories'
        TabOrder = 3
        ExplicitWidth = 186
      end
      object cbMasks: TComboBox
        Left = 78
        Top = 48
        Width = 291
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 15
        TabOrder = 2
      end
      object cbDirectory: TComboBox
        Left = 78
        Top = 21
        Width = 269
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 15
        TabOrder = 0
        OnDropDown = cbDirectoryDropDown
      end
    end
    object btnOK: TButton
      Left = 146
      Top = 269
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 4
      OnClick = btnOKClick
      ExplicitLeft = 132
    end
    object btnCancel: TButton
      Left = 227
      Top = 269
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 5
      ExplicitLeft = 213
    end
    object btnHelp: TButton
      Left = 307
      Top = 269
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Help'
      TabOrder = 6
      OnClick = btnHelpClick
      ExplicitLeft = 293
    end
    object cbText: TComboBox
      Left = 8
      Top = 28
      Width = 372
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 15
      TabOrder = 0
    end
  end
end
