inherited RunConfigurationForm: TRunConfigurationForm
  HelpContext = 940
  Caption = 'Run Configuration'
  ClientHeight = 436
  ClientWidth = 399
  OnDestroy = FormDestroy
  ExplicitWidth = 405
  ExplicitHeight = 465
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 399
    Height = 436
    Align = alClient
    TabOrder = 0
    TBXStyleBackground = True
    DesignSize = (
      399
      436)
    object Bevel1: TBevel
      Left = 3
      Top = 392
      Width = 396
      Height = 4
      Anchors = [akLeft, akRight, akBottom]
      Shape = bsBottomLine
      ExplicitTop = 458
      ExplicitWidth = 418
    end
    object GroupBox1: TSpTBXGroupBox
      Left = 7
      Top = 5
      Width = 385
      Height = 48
      Caption = 'General'
      TabOrder = 0
      TBXStyleBackground = True
      DesignSize = (
        385
        48)
      object Label5: TSpTBXLabel
        Left = 8
        Top = 19
        Width = 63
        Height = 19
        Caption = 'Desc&ription:'
        Anchors = [akLeft, akBottom]
        FocusControl = edDescription
      end
      object edDescription: TSpTBXEdit
        Left = 92
        Top = 19
        Width = 281
        Height = 21
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 0
      end
    end
    object btnOK: TSpTBXButton
      Left = 139
      Top = 402
      Width = 75
      Height = 25
      Caption = '&OK'
      Anchors = [akRight, akBottom]
      TabOrder = 5
      Default = True
      ModalResult = 1
    end
    object btnCancel: TSpTBXButton
      Left = 223
      Top = 402
      Width = 75
      Height = 25
      Caption = '&Cancel'
      Anchors = [akRight, akBottom]
      TabOrder = 6
      Cancel = True
      ModalResult = 2
    end
    object btnHelp: TSpTBXButton
      Left = 307
      Top = 402
      Width = 75
      Height = 25
      Caption = '&Help'
      Anchors = [akRight, akBottom]
      TabOrder = 7
      OnClick = btnHelpClick
    end
    object GroupBox2: TSpTBXGroupBox
      Left = 7
      Top = 59
      Width = 385
      Height = 106
      Caption = 'Python Script'
      TabOrder = 1
      TBXStyleBackground = True
      object SynFileName: TSynEdit
        Left = 92
        Top = 16
        Width = 259
        Height = 18
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 0
        OnEnter = SynEditEnter
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.Width = 0
        HideSelection = True
        Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
        ScrollBars = ssNone
        WantReturns = False
        FontSmoothing = fsmNone
      end
      object SynParameters: TSynEdit
        Left = 92
        Top = 40
        Width = 281
        Height = 18
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 2
        OnEnter = SynEditEnter
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.Width = 0
        HideSelection = True
        Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
        ScrollBars = ssNone
        WantReturns = False
        FontSmoothing = fsmNone
      end
      object SynWorkDir: TSynEdit
        Left = 120
        Top = 64
        Width = 231
        Height = 18
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 3
        OnEnter = SynEditEnter
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.Width = 0
        HideSelection = True
        Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
        ScrollBars = ssNone
        WantReturns = False
        FontSmoothing = fsmNone
      end
      object btnFileName: TSpTBXButton
        Left = 357
        Top = 14
        Width = 19
        Height = 20
        Caption = '...'
        TabOrder = 1
        OnClick = btnFileNameClick
      end
      object btnWorkDir: TSpTBXButton
        Left = 357
        Top = 62
        Width = 19
        Height = 20
        Caption = '...'
        TabOrder = 4
        OnClick = btnWorkDirClick
      end
      object Label2: TSpTBXLabel
        Left = 8
        Top = 18
        Width = 56
        Height = 19
        Caption = '&File Name:'
        FocusControl = SynFileName
      end
      object Label6: TSpTBXLabel
        Left = 8
        Top = 42
        Width = 65
        Height = 19
        Caption = '&Parameters:'
        FocusControl = SynParameters
      end
      object Label7: TSpTBXLabel
        Left = 8
        Top = 66
        Width = 95
        Height = 19
        Caption = 'Working &directory:'
        FocusControl = SynWorkDir
      end
      object Label3: TSpTBXLabel
        Left = 8
        Top = 85
        Width = 258
        Height = 19
        Caption = 'Parameters : Shift+Ctrl+P, Modifiers : Shift+Ctrl+M '
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGrayText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        ParentFont = False
      end
    end
    object gbRemoteEngine: TSpTBXGroupBox
      Left = 7
      Top = 171
      Width = 385
      Height = 68
      Caption = 'Python Engine'
      TabOrder = 2
      TBXStyleBackground = True
      DesignSize = (
        385
        68)
      object cbReinitializeBeforeRun: TSpTBXCheckBox
        Left = 8
        Top = 44
        Width = 131
        Height = 21
        Caption = 'Reinitiali&ze Before Run'
        Anchors = [akTop, akRight]
        TabOrder = 0
      end
      object Label1: TSpTBXLabel
        Left = 8
        Top = 21
        Width = 69
        Height = 19
        Caption = 'Engine &Type:'
        FocusControl = cbEngineType
      end
      object cbEngineType: TSpTBXComboBox
        Left = 103
        Top = 17
        Width = 103
        Height = 21
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        ItemHeight = 13
        ItemIndex = 1
        ParentFont = False
        TabOrder = 1
        Text = 'Remote'
        OnChange = cbEngineTypeChange
        Items.Strings = (
          'Internal'
          'Remote'
          'Remote (Tk)'
          'Remote (Wx)')
      end
    end
    object GroupBox3: TSpTBXGroupBox
      Left = 7
      Top = 245
      Width = 385
      Height = 47
      Caption = 'External Run'
      TabOrder = 3
      TBXStyleBackground = True
      object btnExternalRun: TSpTBXButton
        Left = 52
        Top = 16
        Width = 277
        Height = 25
        Caption = '&Set External Run properties'
        TabOrder = 0
        OnClick = btnExternalRunClick
      end
    end
    object gbSaveOutput: TSpTBXGroupBox
      Left = 7
      Top = 298
      Width = 385
      Height = 88
      Caption = 'Output'
      TabOrder = 4
      TBXStyleBackground = True
      object cbAppendToFile: TSpTBXCheckBox
        Left = 8
        Top = 64
        Width = 91
        Height = 21
        Caption = '&Append to file'
        TabOrder = 0
      end
      object SynOutputFileName: TSynEdit
        Left = 92
        Top = 43
        Width = 259
        Height = 18
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 1
        OnEnter = SynEditEnter
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.Width = 0
        HideSelection = True
        Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
        ScrollBars = ssNone
        WantReturns = False
        FontSmoothing = fsmNone
      end
      object btnOutputFileName: TSpTBXButton
        Left = 357
        Top = 41
        Width = 19
        Height = 20
        Caption = '...'
        TabOrder = 2
        OnClick = btnOutputFileNameClick
      end
      object cbSaveOutput: TSpTBXCheckBox
        Left = 8
        Top = 20
        Width = 85
        Height = 21
        Caption = 'Save Output'
        TabOrder = 3
        OnClick = cbSaveOutputClick
      end
      object Label4: TSpTBXLabel
        Left = 8
        Top = 45
        Width = 56
        Height = 19
        Caption = 'File &Name:'
        FocusControl = SynOutputFileName
      end
    end
  end
end
