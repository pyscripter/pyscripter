object RunConfigurationForm: TRunConfigurationForm
  Left = 0
  Top = 0
  HelpContext = 940
  ActiveControl = cbEngineType
  BorderStyle = bsDialog
  Caption = 'Run Configuration'
  ClientHeight = 403
  ClientWidth = 395
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 395
    Height = 403
    Align = alClient
    TabOrder = 0
    DesignSize = (
      395
      403)
    object Bevel1: TBevel
      Left = 3
      Top = 359
      Width = 392
      Height = 4
      Anchors = [akLeft, akRight, akBottom]
      Shape = bsBottomLine
      ExplicitTop = 458
      ExplicitWidth = 418
    end
    object GroupBox1: TGroupBox
      Left = 4
      Top = 5
      Width = 385
      Height = 48
      Caption = 'General'
      TabOrder = 0
      DesignSize = (
        385
        48)
      object Label5: TLabel
        Left = 8
        Top = 19
        Width = 57
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Desc&ription:'
        FocusControl = edDescription
        ExplicitTop = 39
      end
      object edDescription: TEdit
        Left = 71
        Top = 16
        Width = 307
        Height = 21
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 0
      end
    end
    object btnOK: TSpTBXButton
      Left = 135
      Top = 369
      Width = 75
      Height = 25
      Caption = '&OK'
      Anchors = [akRight, akBottom]
      TabOrder = 5
      Default = True
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'Tahoma'
      LinkFont.Style = [fsUnderline]
      ModalResult = 1
    end
    object btnCancel: TSpTBXButton
      Left = 219
      Top = 369
      Width = 75
      Height = 25
      Caption = '&Cancel'
      Anchors = [akRight, akBottom]
      TabOrder = 6
      Cancel = True
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'Tahoma'
      LinkFont.Style = [fsUnderline]
      ModalResult = 2
    end
    object btnHelp: TSpTBXButton
      Left = 303
      Top = 369
      Width = 75
      Height = 25
      Caption = '&Help'
      Anchors = [akRight, akBottom]
      TabOrder = 7
      OnClick = btnHelpClick
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'Tahoma'
      LinkFont.Style = [fsUnderline]
    end
    object GroupBox2: TGroupBox
      Left = 4
      Top = 59
      Width = 385
      Height = 106
      Caption = 'Python Script'
      TabOrder = 1
      object Label2: TLabel
        Left = 8
        Top = 18
        Width = 50
        Height = 13
        Caption = '&File Name:'
        FocusControl = SynFileName
      end
      object Label6: TLabel
        Left = 8
        Top = 42
        Width = 59
        Height = 13
        Caption = '&Parameters:'
        FocusControl = SynParameters
      end
      object Label7: TLabel
        Left = 8
        Top = 66
        Width = 89
        Height = 13
        Caption = 'Working &directory:'
        FocusControl = SynWorkDir
      end
      object Label3: TLabel
        Left = 8
        Top = 85
        Width = 252
        Height = 13
        Caption = 'Parameters : Shift+Ctrl+P, Modifiers : Shift+Ctrl+M '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGrayText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        ParentFont = False
      end
      object SynFileName: TSynEdit
        Left = 73
        Top = 16
        Width = 278
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
      end
      object SynParameters: TSynEdit
        Left = 73
        Top = 40
        Width = 300
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
      end
      object SynWorkDir: TSynEdit
        Left = 103
        Top = 64
        Width = 248
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
      end
      object btnFileName: TSpTBXButton
        Left = 357
        Top = 14
        Width = 19
        Height = 20
        Caption = '...'
        TabOrder = 1
        OnClick = btnFileNameClick
        LinkFont.Charset = DEFAULT_CHARSET
        LinkFont.Color = clBlue
        LinkFont.Height = -11
        LinkFont.Name = 'Tahoma'
        LinkFont.Style = [fsUnderline]
      end
      object btnWorkDir: TSpTBXButton
        Left = 357
        Top = 62
        Width = 19
        Height = 20
        Caption = '...'
        TabOrder = 4
        OnClick = btnWorkDirClick
        LinkFont.Charset = DEFAULT_CHARSET
        LinkFont.Color = clBlue
        LinkFont.Height = -11
        LinkFont.Name = 'Tahoma'
        LinkFont.Style = [fsUnderline]
      end
    end
    object gbRemoteEngine: TGroupBox
      Left = 4
      Top = 171
      Width = 385
      Height = 50
      Caption = 'Python Engine'
      TabOrder = 2
      DesignSize = (
        385
        50)
      object Label1: TLabel
        Left = 8
        Top = 21
        Width = 63
        Height = 13
        Caption = 'Engine &Type:'
        FocusControl = cbEngineType
      end
      object cbEngineType: TComboBox
        Left = 103
        Top = 17
        Width = 103
        Height = 21
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
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
      object cbReinitializeBeforeRun: TSpTBXCheckBox
        Left = 226
        Top = 20
        Width = 125
        Height = 15
        Caption = 'Reinitiali&ze Before Run'
        Anchors = [akTop, akRight]
        TabOrder = 0
        ThemeType = thtWindows
      end
    end
    object GroupBox3: TGroupBox
      Left = 4
      Top = 227
      Width = 385
      Height = 41
      Caption = 'External Run'
      TabOrder = 3
      object btnExternalRun: TSpTBXButton
        Left = 103
        Top = 10
        Width = 194
        Height = 25
        Caption = '&Set Enternal Run properties'
        TabOrder = 0
        OnClick = btnExternalRunClick
        LinkFont.Charset = DEFAULT_CHARSET
        LinkFont.Color = clBlue
        LinkFont.Height = -11
        LinkFont.Name = 'Tahoma'
        LinkFont.Style = [fsUnderline]
      end
    end
    object gbSaveOutput: TGroupBox
      Left = 4
      Top = 274
      Width = 385
      Height = 88
      Caption = 'Output'
      TabOrder = 4
      object Label4: TLabel
        Left = 8
        Top = 45
        Width = 50
        Height = 13
        Caption = 'File &Name:'
        FocusControl = SynOutputFileName
      end
      object cbAppendToFile: TSpTBXCheckBox
        Left = 8
        Top = 64
        Width = 85
        Height = 15
        Caption = '&Append to file'
        TabOrder = 0
        ThemeType = thtWindows
      end
      object SynOutputFileName: TSynEdit
        Left = 73
        Top = 43
        Width = 278
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
      end
      object btnOutputFileName: TSpTBXButton
        Left = 357
        Top = 41
        Width = 19
        Height = 20
        Caption = '...'
        TabOrder = 2
        OnClick = btnOutputFileNameClick
        LinkFont.Charset = DEFAULT_CHARSET
        LinkFont.Color = clBlue
        LinkFont.Height = -11
        LinkFont.Name = 'Tahoma'
        LinkFont.Style = [fsUnderline]
      end
      object cbSaveOutput: TSpTBXCheckBox
        Left = 8
        Top = 20
        Width = 79
        Height = 15
        Caption = 'Save Output'
        TabOrder = 3
        OnClick = cbSaveOutputClick
      end
    end
  end
end
