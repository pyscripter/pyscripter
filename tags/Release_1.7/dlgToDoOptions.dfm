object fmToDoOptions: TfmToDoOptions
  Left = 347
  Top = 241
  HelpContext = 465
  ActiveControl = lstTokens
  BorderStyle = bsDialog
  Caption = 'To Do Options'
  ClientHeight = 270
  ClientWidth = 512
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    512
    270)
  PixelsPerInch = 96
  TextHeight = 13
  object gbxTokens: TGroupBox
    Left = 8
    Top = 6
    Width = 233
    Height = 256
    Caption = 'To Do Tokens'
    TabOrder = 0
    object lblPriority: TLabel
      Left = 131
      Top = 185
      Width = 31
      Height = 13
      Caption = '&Priority'
      FocusControl = cboPriority
    end
    object lblToken: TLabel
      Left = 131
      Top = 139
      Width = 31
      Height = 13
      Caption = 'To&ken'
      FocusControl = edToken
    end
    object lstTokens: TListBox
      Left = 8
      Top = 19
      Width = 118
      Height = 228
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
      OnClick = lstTokensClick
    end
    object btnInsert: TButton
      Left = 140
      Top = 30
      Width = 77
      Height = 26
      Caption = '&Insert'
      TabOrder = 1
      OnClick = btnInsertClick
    end
    object btnApply: TButton
      Left = 140
      Top = 62
      Width = 77
      Height = 26
      Caption = '&Apply'
      TabOrder = 2
      OnClick = btnApplyClick
    end
    object btnRemove: TButton
      Left = 140
      Top = 94
      Width = 77
      Height = 26
      Caption = '&Remove'
      TabOrder = 3
      OnClick = btnRemoveClick
    end
    object edToken: TEdit
      Left = 131
      Top = 155
      Width = 96
      Height = 21
      TabOrder = 4
      OnChange = edTokenChange
    end
    object cboPriority: TComboBox
      Left = 131
      Top = 203
      Width = 96
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 5
      OnChange = cboPriorityChange
      Items.Strings = (
        'High'
        'Normal'
        'Low')
    end
  end
  object btnOK: TButton
    Left = 254
    Top = 238
    Width = 80
    Height = 26
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 342
    Top = 238
    Width = 80
    Height = 26
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object gbxOptions: TGroupBox
    Left = 251
    Top = 7
    Width = 253
    Height = 48
    Caption = 'Options'
    TabOrder = 1
    object cbShowTokens: TCheckBox
      Left = 8
      Top = 19
      Width = 243
      Height = 17
      Caption = '&Show tokens in description'
      TabOrder = 0
    end
  end
  object gbxSearchFiles: TGroupBox
    Left = 251
    Top = 62
    Width = 253
    Height = 168
    Caption = 'Search for To Do Tokens'
    TabOrder = 2
    object btnBrowse: TButton
      Left = 222
      Top = 53
      Width = 20
      Height = 20
      Hint = 'Select Directory'
      Caption = '...'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = btnBrowseClick
    end
    object chkInclude: TCheckBox
      Left = 8
      Top = 142
      Width = 235
      Height = 17
      Caption = 'Include su&bdirectories'
      TabOrder = 3
    end
    object radScanOpen: TRadioButton
      Left = 6
      Top = 17
      Width = 242
      Height = 17
      Caption = 'Scan &open project files'
      TabOrder = 0
      TabStop = True
      OnClick = radScanDirClick
    end
    object radScanDir: TRadioButton
      Left = 6
      Top = 35
      Width = 242
      Height = 17
      Caption = 'Scan &directories'
      TabOrder = 1
      TabStop = True
      OnClick = radScanDirClick
    end
    object meDirectories: TMemo
      Left = 9
      Top = 53
      Width = 208
      Height = 83
      ScrollBars = ssBoth
      TabOrder = 4
      WordWrap = False
    end
  end
  object btnHelp: TButton
    Left = 430
    Top = 239
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 5
    OnClick = btnHelpClick
  end
end
