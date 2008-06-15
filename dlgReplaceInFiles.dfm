inherited ReplaceInFilesDialog: TReplaceInFilesDialog
  Left = 378
  Top = 227
  HelpContext = 460
  Caption = 'Replace Matches'
  ClientHeight = 158
  ClientWidth = 401
  Font.Name = 'MS Shell Dlg 2'
  Position = poScreenCenter
  Scaled = False
  ExplicitWidth = 407
  ExplicitHeight = 184
  DesignSize = (
    401
    158)
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TSpTBXButton
    Left = 160
    Top = 125
    Width = 75
    Height = 25
    Caption = '&OK'
    Anchors = [akRight, akBottom]
    TabOrder = 1
    Default = True
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'MS Shell Dlg 2'
    LinkFont.Style = [fsUnderline]
    ModalResult = 1
    ExplicitLeft = 127
  end
  object btnCancel: TSpTBXButton
    Left = 239
    Top = 125
    Width = 75
    Height = 25
    Caption = '&Cancel'
    Anchors = [akRight, akBottom]
    TabOrder = 2
    Cancel = True
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'MS Shell Dlg 2'
    LinkFont.Style = [fsUnderline]
    ModalResult = 2
    ExplicitLeft = 206
  end
  object btnHelp: TSpTBXButton
    Left = 318
    Top = 125
    Width = 75
    Height = 25
    Caption = '&Help'
    Anchors = [akRight, akBottom]
    TabOrder = 3
    OnClick = btnHelpClick
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'MS Shell Dlg 2'
    LinkFont.Style = [fsUnderline]
    ExplicitLeft = 285
  end
  object cbBackup: TSpTBXCheckBox
    Left = 10
    Top = 105
    Width = 119
    Height = 15
    Caption = 'Backup Modified Files'
    ParentColor = True
    TabOrder = 4
    ThemeType = thtWindows
  end
  object cbReplace: TSpTBXComboBox
    Left = 92
    Top = 30
    Width = 302
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
    ThemeType = thtWindows
  end
  object lblInString: TSpTBXLabel
    Left = 92
    Top = 57
    Width = 332
    Height = 13
    Caption = 'C:\test\dir\TestFile.pas'
    Anchors = [akLeft, akTop, akRight]
    ParentColor = True
    ShowAccelChar = False
    Wrapping = twWrap
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'MS Shell Dlg 2'
    LinkFont.Style = [fsUnderline]
  end
  object lblReplaceString: TSpTBXLabel
    Left = 92
    Top = 8
    Width = 49
    Height = 13
    Caption = 'TestString'
    ParentColor = True
    ShowAccelChar = False
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'MS Shell Dlg 2'
    LinkFont.Style = [fsUnderline]
  end
  object lblWith: TSpTBXLabel
    Left = 10
    Top = 33
    Width = 22
    Height = 13
    Caption = '&With'
    ParentColor = True
    FocusControl = cbReplace
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'MS Shell Dlg 2'
    LinkFont.Style = [fsUnderline]
  end
  object lblIn: TSpTBXLabel
    Left = 10
    Top = 57
    Width = 10
    Height = 13
    Caption = 'In'
    ParentColor = True
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'MS Shell Dlg 2'
    LinkFont.Style = [fsUnderline]
  end
  object lblReplace: TSpTBXLabel
    Left = 10
    Top = 8
    Width = 38
    Height = 13
    Caption = 'Replace'
    ParentColor = True
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'MS Shell Dlg 2'
    LinkFont.Style = [fsUnderline]
  end
end
