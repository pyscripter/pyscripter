inherited FindResultsOptionsDialog: TFindResultsOptionsDialog
  Left = 322
  Top = 238
  HelpContext = 460
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Find in Files Options'
  ClientHeight = 307
  ClientWidth = 307
  Font.Name = 'MS Shell Dlg 2'
  Position = poScreenCenter
  Scaled = False
  ExplicitWidth = 313
  ExplicitHeight = 333
  DesignSize = (
    307
    307)
  PixelsPerInch = 96
  TextHeight = 13
  object gbxMatchList: TSpTBXGroupBox
    Left = 2
    Top = 8
    Width = 301
    Height = 103
    Caption = 'Match Results List'
    SkinType = sknSkin
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    ExplicitWidth = 305
    object chkGrepMiddle: TSpTBXCheckBox
      Left = 12
      Top = 36
      Width = 225
      Height = 15
      Caption = 'Jump to matches in the &middle of the editor'
      ParentColor = True
      TabOrder = 2
      SkinType = sknSkin
    end
    object chkGrepExpandAll: TSpTBXCheckBox
      Left = 12
      Top = 16
      Width = 186
      Height = 15
      Caption = '&Expand all matches after searching'
      ParentColor = True
      TabOrder = 1
      SkinType = sknSkin
    end
    object pnlListFont: TPanel
      Left = 28
      Top = 57
      Width = 243
      Height = 33
      Caption = 'Match List Font...'
      Color = clWindow
      UseDockManager = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = pnlListFontClick
    end
  end
  object gbxMatchContext: TSpTBXGroupBox
    Left = 2
    Top = 117
    Width = 301
    Height = 153
    Caption = 'Match Context Display'
    SkinType = sknSkin
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    ExplicitWidth = 305
    object lblContextLines: TSpTBXLabel
      Left = 28
      Top = 101
      Width = 114
      Height = 13
      Caption = 'Number of context lines'
      ParentColor = True
    end
    object spnContextLines: TSpTBXSpinEdit
      Left = 28
      Top = 118
      Width = 54
      Height = 21
      TabOrder = 2
      Alignment = taLeftJustify
      SkinType = sknSkin
      SpinButton.Left = 35
      SpinButton.Top = 0
      SpinButton.Width = 15
      SpinButton.Height = 17
      SpinButton.Align = alRight
      SpinButton.SkinType = sknSkin
    end
    object pnlContextFont: TPanel
      Left = 28
      Top = 19
      Width = 243
      Height = 33
      Caption = 'Context Font...'
      Color = clWindow
      UseDockManager = False
      TabOrder = 0
      OnClick = pnlContextFontClick
    end
    object pnlMatchLineColor: TPanel
      Left = 28
      Top = 58
      Width = 243
      Height = 33
      Caption = 'Match Font Color...'
      Color = clWindow
      UseDockManager = False
      TabOrder = 1
      OnClick = pnlMatchLineColorClick
    end
  end
  object btnOK: TSpTBXButton
    Left = 30
    Top = 274
    Width = 75
    Height = 25
    Caption = '&OK'
    Anchors = [akRight, akBottom]
    TabOrder = 2
    Default = True
    ModalResult = 1
  end
  object btnCancel: TSpTBXButton
    Left = 114
    Top = 274
    Width = 75
    Height = 25
    Caption = '&Cancel'
    Anchors = [akRight, akBottom]
    TabOrder = 3
    Cancel = True
    ModalResult = 2
  end
  object btnHelp: TSpTBXButton
    Left = 198
    Top = 274
    Width = 75
    Height = 25
    Caption = '&Help'
    Anchors = [akRight, akBottom]
    TabOrder = 4
    OnClick = btnHelpClick
  end
  object dlgGrepListFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = []
    Left = 184
    Top = 66
  end
  object dlgGrepContextFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = []
    Left = 184
    Top = 132
  end
  object dlgContextFontColor: TColorDialog
    Options = [cdSolidColor]
    Left = 183
    Top = 172
  end
end
