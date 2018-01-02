inherited FindResultsOptionsDialog: TFindResultsOptionsDialog
  Left = 322
  Top = 238
  HelpContext = 460
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Find in Files Options'
  ClientHeight = 307
  ClientWidth = 307
  Position = poScreenCenter
  ExplicitWidth = 313
  ExplicitHeight = 335
  PixelsPerInch = 96
  TextHeight = 13
  object SpTBXPanel1: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 307
    Height = 307
    Caption = 'SpTBXPanel1'
    Align = alClient
    TabOrder = 0
    TBXStyleBackground = True
    DesignSize = (
      307
      307)
    object gbxMatchList: TSpTBXGroupBox
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 297
      Height = 103
      Caption = 'Match Results List'
      Align = alTop
      TabOrder = 0
      TBXStyleBackground = True
      object chkGrepMiddle: TSpTBXCheckBox
        Left = 12
        Top = 36
        Width = 231
        Height = 21
        Caption = 'Jump to matches in the &middle of the editor'
        ParentColor = True
        TabOrder = 2
      end
      object chkGrepExpandAll: TSpTBXCheckBox
        Left = 12
        Top = 16
        Width = 192
        Height = 21
        Caption = '&Expand all matches after searching'
        ParentColor = True
        TabOrder = 1
      end
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
    end
    object gbxMatchContext: TSpTBXGroupBox
      Left = 5
      Top = 114
      Width = 297
      Height = 153
      Caption = 'Match Context Display'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      TBXStyleBackground = True
      object lblContextLines: TSpTBXLabel
        Left = 28
        Top = 101
        Width = 120
        Height = 19
        Caption = 'Number of context lines'
      end
      object spnContextLines: TSpTBXSpinEdit
        Left = 28
        Top = 118
        Width = 54
        Height = 21
        Alignment = taLeftJustify
        TabOrder = 2
        SpinButton.Left = 35
        SpinButton.Top = 0
        SpinButton.Width = 15
        SpinButton.Height = 17
        SpinButton.Align = alRight
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
