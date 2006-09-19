object FindResultsOptionsDialog: TFindResultsOptionsDialog
  Left = 322
  Top = 238
  HelpContext = 460
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Find in Files Options'
  ClientHeight = 279
  ClientWidth = 267
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
    267
    279)
  PixelsPerInch = 96
  TextHeight = 13
  object gbxMatchList: TGroupBox
    Left = 4
    Top = 4
    Width = 257
    Height = 103
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Match Results List'
    TabOrder = 0
    object pnlListFont: TPanel
      Left = 28
      Top = 60
      Width = 197
      Height = 33
      BevelWidth = 2
      Caption = 'Match List Font...'
      Color = clWindow
      TabOrder = 0
      OnClick = pnlListFontClick
    end
    object chkGrepMiddle: TCheckBox
      Left = 12
      Top = 36
      Width = 241
      Height = 17
      Caption = 'Jump to matches in the &middle of the editor'
      TabOrder = 2
    end
    object chkGrepExpandAll: TCheckBox
      Left = 12
      Top = 16
      Width = 241
      Height = 17
      Caption = '&Expand all matches after searching'
      TabOrder = 1
    end
  end
  object gbxMatchContext: TGroupBox
    Left = 4
    Top = 112
    Width = 257
    Height = 129
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Match Context Display'
    TabOrder = 1
    object lblContextLines: TLabel
      Left = 28
      Top = 101
      Width = 111
      Height = 13
      Caption = 'Number of context lines'
    end
    object pnlContextFont: TPanel
      Left = 28
      Top = 18
      Width = 197
      Height = 33
      BevelWidth = 2
      Caption = 'Context Font...'
      Color = clWindow
      TabOrder = 0
      OnClick = pnlContextFontClick
    end
    object pnlMatchLineColor: TPanel
      Left = 28
      Top = 58
      Width = 197
      Height = 33
      BevelWidth = 2
      Caption = 'Match Font Color...'
      Color = clWindow
      TabOrder = 1
      OnClick = pnlMatchLineColorClick
    end
    object spnContextLines: TJvSpinEdit
      Left = 149
      Top = 99
      Width = 54
      Height = 21
      TabOrder = 2
    end
  end
  object btnOK: TButton
    Left = 16
    Top = 248
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 100
    Top = 248
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object btnHelp: TButton
    Left = 184
    Top = 248
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 4
    OnClick = btnHelpClick
  end
  object dlgGrepListFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 184
    Top = 66
  end
  object dlgGrepContextFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
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
