inherited FindResultsOptionsDialog: TFindResultsOptionsDialog
  Left = 322
  Top = 238
  HelpContext = 460
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Find in Files Options'
  ClientHeight = 307
  ClientWidth = 307
  Position = poScreenCenter
  OnShow = FormShow
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 307
    Height = 307
    Align = alClient
    TabOrder = 0
    DesignSize = (
      307
      307)
    object gbxMatchList: TGroupBox
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 299
      Height = 103
      Align = alTop
      Caption = 'Match Results List'
      TabOrder = 0
      object pnlListFont: TPanel
        Left = 28
        Top = 61
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
      object chkGrepMiddle: TCheckBox
        Left = 12
        Top = 36
        Width = 280
        Height = 21
        Caption = 'Jump to matches in the &middle of the editor'
        TabOrder = 2
      end
      object chkGrepExpandAll: TCheckBox
        Left = 12
        Top = 16
        Width = 280
        Height = 21
        Caption = '&Expand all matches after searching'
        TabOrder = 1
      end
    end
    object gbxMatchContext: TGroupBox
      Left = 5
      Top = 114
      Width = 297
      Height = 153
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Match Context Display'
      TabOrder = 1
      object lblContextLines: TLabel
        Left = 28
        Top = 101
        Width = 128
        Height = 15
        Caption = 'Number of context lines'
        Color = clNone
        ParentColor = False
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
        StyleElements = [seClient, seBorder]
        OnClick = pnlMatchLineColorClick
      end
      object spnContextLines: TSpinEdit
        Left = 28
        Top = 123
        Width = 54
        Height = 24
        MaxValue = 0
        MinValue = 0
        TabOrder = 2
        Value = 0
      end
    end
    object btnOK: TButton
      Left = 30
      Top = 274
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 2
    end
    object btnCancel: TButton
      Left = 114
      Top = 274
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 3
    end
    object btnHelp: TButton
      Left = 198
      Top = 274
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Help'
      TabOrder = 4
      OnClick = btnHelpClick
    end
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
