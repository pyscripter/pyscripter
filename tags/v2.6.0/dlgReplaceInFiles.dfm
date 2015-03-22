inherited ReplaceInFilesDialog: TReplaceInFilesDialog
  Left = 378
  Top = 227
  HelpContext = 460
  Caption = 'Replace Matches'
  ClientHeight = 158
  ClientWidth = 401
  Position = poScreenCenter
  ExplicitWidth = 407
  ExplicitHeight = 186
  PixelsPerInch = 96
  TextHeight = 13
  object SpTBXPanel1: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 401
    Height = 158
    Caption = 'SpTBXPanel1'
    Align = alClient
    TabOrder = 0
    TBXStyleBackground = True
    DesignSize = (
      401
      158)
    object btnOK: TSpTBXButton
      Left = 160
      Top = 125
      Width = 75
      Height = 25
      Caption = '&OK'
      Anchors = [akRight, akBottom]
      TabOrder = 1
      Default = True
      ModalResult = 1
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
      ModalResult = 2
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
    end
    object cbBackup: TSpTBXCheckBox
      Left = 10
      Top = 105
      Width = 125
      Height = 21
      Caption = 'Backup Modified Files'
      ParentColor = True
      TabOrder = 4
    end
    object cbReplace: TSpTBXComboBox
      Left = 92
      Top = 30
      Width = 302
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 0
    end
    object lblInString: TSpTBXLabel
      Left = 92
      Top = 57
      Width = 332
      Height = 19
      Caption = 'C:\test\dir\TestFile.pas'
      Anchors = [akLeft, akTop, akRight]
      ShowAccelChar = False
      Wrapping = twWrap
    end
    object lblReplaceString: TSpTBXLabel
      Left = 92
      Top = 8
      Width = 55
      Height = 19
      Caption = 'TestString'
      ShowAccelChar = False
    end
    object lblWith: TSpTBXLabel
      Left = 10
      Top = 33
      Width = 28
      Height = 19
      Caption = '&With'
      FocusControl = cbReplace
    end
    object lblIn: TSpTBXLabel
      Left = 10
      Top = 57
      Width = 16
      Height = 19
      Caption = 'In'
    end
    object lblReplace: TSpTBXLabel
      Left = 10
      Top = 8
      Width = 44
      Height = 19
      Caption = 'Replace'
    end
  end
end
