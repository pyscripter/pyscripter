object ReplaceInFilesDialog: TReplaceInFilesDialog
  Left = 378
  Top = 227
  HelpContext = 460
  BorderStyle = bsDialog
  Caption = 'Replace Matches'
  ClientHeight = 158
  ClientWidth = 368
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  DesignSize = (
    368
    158)
  PixelsPerInch = 96
  TextHeight = 13
  object lblWith: TLabel
    Left = 10
    Top = 33
    Width = 22
    Height = 13
    Caption = '&With'
    FocusControl = cbReplace
  end
  object lblIn: TLabel
    Left = 10
    Top = 57
    Width = 9
    Height = 13
    Caption = 'In'
  end
  object lblInString: TLabel
    Left = 61
    Top = 57
    Width = 300
    Height = 40
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'C:\test\dir\TestFile.pas'
    ShowAccelChar = False
    WordWrap = True
  end
  object lblReplace: TLabel
    Left = 10
    Top = 8
    Width = 40
    Height = 13
    Caption = 'Replace'
  end
  object lblReplaceString: TLabel
    Left = 61
    Top = 8
    Width = 48
    Height = 13
    Caption = 'TestString'
    ShowAccelChar = False
  end
  object cbReplace: TComboBox
    Left = 61
    Top = 30
    Width = 300
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 127
    Top = 125
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 206
    Top = 125
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnHelp: TButton
    Left = 285
    Top = 125
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 3
    OnClick = btnHelpClick
  end
  object cbBackup: TCheckBox
    Left = 10
    Top = 105
    Width = 191
    Height = 17
    Caption = 'Backup Modified Files'
    TabOrder = 4
  end
end
