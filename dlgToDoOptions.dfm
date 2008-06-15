inherited fmToDoOptions: TfmToDoOptions
  Left = 347
  Top = 241
  HelpContext = 465
  Caption = 'To Do Options'
  ClientHeight = 270
  ClientWidth = 512
  Font.Name = 'MS Shell Dlg 2'
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnShow = FormShow
  DesignSize = (
    512
    270)
  PixelsPerInch = 96
  TextHeight = 13
  object gbxTokens: TSpTBXGroupBox
    Left = 8
    Top = 6
    Width = 233
    Height = 256
    Caption = 'To Do Tokens'
    ThemeType = thtWindows
    TabOrder = 0
    object btnInsert: TSpTBXButton
      Left = 140
      Top = 30
      Width = 77
      Height = 26
      Caption = '&Insert'
      TabOrder = 1
      OnClick = btnInsertClick
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'MS Shell Dlg 2'
      LinkFont.Style = [fsUnderline]
    end
    object btnApply: TSpTBXButton
      Left = 140
      Top = 62
      Width = 77
      Height = 26
      Caption = '&Apply'
      TabOrder = 2
      OnClick = btnApplyClick
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'MS Shell Dlg 2'
      LinkFont.Style = [fsUnderline]
    end
    object btnRemove: TSpTBXButton
      Left = 140
      Top = 94
      Width = 77
      Height = 26
      Caption = '&Remove'
      TabOrder = 3
      OnClick = btnRemoveClick
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'MS Shell Dlg 2'
      LinkFont.Style = [fsUnderline]
    end
    object lstTokens: TSpTBXListBox
      Left = 7
      Top = 20
      Width = 118
      Height = 228
      Style = lbStandard
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
      OnClick = lstTokensClick
      ThemeType = thtWindows
    end
    object edToken: TSpTBXEdit
      Left = 131
      Top = 155
      Width = 96
      Height = 21
      TabOrder = 4
      OnChange = edTokenChange
      ThemeType = thtWindows
    end
    object cboPriority: TSpTBXComboBox
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
      ThemeType = thtWindows
    end
    object lblPriority: TSpTBXLabel
      Left = 131
      Top = 185
      Width = 34
      Height = 13
      Caption = '&Priority'
      ParentColor = True
      FocusControl = cboPriority
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'MS Shell Dlg 2'
      LinkFont.Style = [fsUnderline]
    end
    object lblToken: TSpTBXLabel
      Left = 131
      Top = 139
      Width = 29
      Height = 13
      Caption = 'To&ken'
      ParentColor = True
      FocusControl = edToken
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'MS Shell Dlg 2'
      LinkFont.Style = [fsUnderline]
    end
  end
  object gbxOptions: TSpTBXGroupBox
    Left = 251
    Top = 7
    Width = 253
    Height = 48
    Caption = 'Options'
    ThemeType = thtWindows
    TabOrder = 1
    object cbShowTokens: TSpTBXCheckBox
      Left = 8
      Top = 19
      Width = 145
      Height = 15
      Caption = '&Show tokens in description'
      ParentColor = True
      TabOrder = 0
      ThemeType = thtWindows
    end
  end
  object gbxSearchFiles: TSpTBXGroupBox
    Left = 251
    Top = 62
    Width = 253
    Height = 171
    Caption = 'Search for To Do Tokens'
    ThemeType = thtWindows
    TabOrder = 2
    object btnBrowse: TSpTBXButton
      Left = 223
      Top = 72
      Width = 20
      Height = 20
      Hint = 'Select Directory'
      Caption = '...'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = btnBrowseClick
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'MS Shell Dlg 2'
      LinkFont.Style = [fsUnderline]
    end
    object chkInclude: TSpTBXCheckBox
      Left = 8
      Top = 150
      Width = 123
      Height = 15
      Caption = 'Include su&bdirectories'
      ParentColor = True
      TabOrder = 3
      ThemeType = thtWindows
    end
    object radScanOpen: TSpTBXRadioButton
      Left = 6
      Top = 17
      Width = 90
      Height = 15
      Caption = 'Scan &open files'
      ParentColor = True
      TabOrder = 0
      TabStop = True
      OnClick = radScanDirClick
      ThemeType = thtWindows
    end
    object radScanDir: TSpTBXRadioButton
      Left = 6
      Top = 51
      Width = 94
      Height = 15
      Caption = 'Scan &directories'
      ParentColor = True
      TabOrder = 1
      TabStop = True
      OnClick = radScanDirClick
      ThemeType = thtWindows
    end
    object radScanProject: TSpTBXRadioButton
      Left = 6
      Top = 34
      Width = 100
      Height = 15
      Caption = 'Scan &project files'
      ParentColor = True
      TabOrder = 5
      TabStop = True
      OnClick = radScanDirClick
      ThemeType = thtWindows
    end
    object meDirectories: TTntRichEdit
      Left = 9
      Top = 72
      Width = 208
      Height = 72
      PlainText = True
      ScrollBars = ssBoth
      TabOrder = 4
      WordWrap = False
    end
  end
  object btnOK: TSpTBXButton
    Left = 254
    Top = 238
    Width = 80
    Height = 26
    Caption = '&OK'
    TabOrder = 3
    Default = True
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'MS Shell Dlg 2'
    LinkFont.Style = [fsUnderline]
    ModalResult = 1
  end
  object btnCancel: TSpTBXButton
    Left = 342
    Top = 238
    Width = 80
    Height = 26
    Caption = '&Cancel'
    TabOrder = 4
    Cancel = True
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'MS Shell Dlg 2'
    LinkFont.Style = [fsUnderline]
    ModalResult = 2
  end
  object btnHelp: TSpTBXButton
    Left = 430
    Top = 239
    Width = 75
    Height = 25
    Caption = '&Help'
    Anchors = [akRight, akBottom]
    TabOrder = 5
    OnClick = btnHelpClick
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'MS Shell Dlg 2'
    LinkFont.Style = [fsUnderline]
  end
end
