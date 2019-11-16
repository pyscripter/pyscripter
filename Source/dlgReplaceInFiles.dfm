inherited ReplaceInFilesDialog: TReplaceInFilesDialog
  Left = 378
  Top = 227
  HelpContext = 460
  Caption = 'Replace Matches'
  ClientHeight = 158
  ClientWidth = 401
  Position = poScreenCenter
  ExplicitWidth = 407
  ExplicitHeight = 187
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 401
    Height = 158
    Align = alClient
    TabOrder = 0
    DesignSize = (
      401
      158)
    object lblReplaceString: TLabel
      Left = 92
      Top = 8
      Width = 49
      Height = 13
      Caption = 'TestString'
      Color = clNone
      ParentColor = False
      ShowAccelChar = False
    end
    object lblWith: TLabel
      Left = 10
      Top = 33
      Width = 22
      Height = 13
      Caption = '&With'
      Color = clNone
      FocusControl = cbReplace
      ParentColor = False
    end
    object lblIn: TLabel
      Left = 10
      Top = 57
      Width = 10
      Height = 13
      Caption = 'In'
      Color = clNone
      ParentColor = False
    end
    object lblReplace: TLabel
      Left = 10
      Top = 8
      Width = 38
      Height = 13
      Caption = 'Replace'
      Color = clNone
      ParentColor = False
    end
    object lblInString: TLabel
      Left = 92
      Top = 57
      Width = 3
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      Color = clNone
      ParentColor = False
      ShowAccelChar = False
    end
    object cbReplace: TComboBox
      Left = 92
      Top = 30
      Width = 302
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object btnOK: TButton
      Left = 160
      Top = 125
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
    object btnCancel: TButton
      Left = 239
      Top = 125
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object btnHelp: TButton
      Left = 318
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
      Width = 300
      Height = 21
      Caption = 'Backup Modified Files'
      TabOrder = 4
    end
  end
end
