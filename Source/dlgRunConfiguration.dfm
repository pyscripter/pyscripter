inherited RunConfigurationForm: TRunConfigurationForm
  HelpContext = 940
  Caption = 'Run Configuration'
  ClientHeight = 436
  ClientWidth = 399
  OnDestroy = FormDestroy
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 399
    Height = 436
    Align = alClient
    TabOrder = 0
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
    end
    object GroupBox1: TGroupBox
      Left = 8
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
        Top = 22
        Width = 63
        Height = 15
        Anchors = [akLeft, akBottom]
        Caption = 'Desc&ription:'
        Color = clNone
        FocusControl = edDescription
        ParentColor = False
      end
      object edDescription: TEdit
        Left = 105
        Top = 19
        Width = 268
        Height = 23
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 0
      end
    end
    object GroupBox2: TGroupBox
      Left = 8
      Top = 59
      Width = 385
      Height = 106
      Caption = 'Python Script'
      TabOrder = 1
      object Label2: TLabel
        Left = 8
        Top = 21
        Width = 56
        Height = 15
        Caption = '&File Name:'
        Color = clNone
        FocusControl = SynFileName
        ParentColor = False
      end
      object Label6: TLabel
        Left = 8
        Top = 45
        Width = 62
        Height = 15
        Caption = '&Parameters:'
        Color = clNone
        FocusControl = SynParameters
        ParentColor = False
      end
      object Label7: TLabel
        Left = 8
        Top = 69
        Width = 99
        Height = 15
        Caption = 'Working &Directory:'
        Color = clNone
        FocusControl = SynWorkDir
        ParentColor = False
      end
      object Label3: TLabel
        Left = 8
        Top = 85
        Width = 252
        Height = 13
        Caption = 'Parameters : Shift+Ctrl+P, Modifiers : Shift+Ctrl+M '
        Color = clNone
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGrayText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object SynFileName: TSynEdit
        Left = 105
        Top = 16
        Width = 224
        Height = 18
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Font.Quality = fqClearTypeNatural
        TabOrder = 0
        OnEnter = SynEditEnter
        UseCodeFolding = False
        ExtraLineSpacing = 0
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.Font.Quality = fqClearTypeNatural
        Gutter.Visible = False
        Gutter.Bands = <
          item
            Kind = gbkMarks
            Width = 15
          end
          item
            Kind = gbkLineNumbers
          end
          item
            Kind = gbkFold
          end
          item
            Kind = gbkMargin
            Width = 2
          end>
        HideSelection = True
        Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
        ScrollBars = ssNone
        SelectedColor.Alpha = 0.400000005960464500
        WantReturns = False
      end
      object SynParameters: TSynEdit
        Left = 105
        Top = 40
        Width = 268
        Height = 18
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Font.Quality = fqClearTypeNatural
        TabOrder = 2
        OnEnter = SynEditEnter
        UseCodeFolding = False
        ExtraLineSpacing = 0
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.Font.Quality = fqClearTypeNatural
        Gutter.Visible = False
        Gutter.Bands = <
          item
            Kind = gbkMarks
            Width = 15
          end
          item
            Kind = gbkLineNumbers
          end
          item
            Kind = gbkFold
          end
          item
            Kind = gbkMargin
            Width = 2
          end>
        HideSelection = True
        Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
        ScrollBars = ssNone
        SelectedColor.Alpha = 0.400000005960464500
        WantReturns = False
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
        Font.Quality = fqClearTypeNatural
        TabOrder = 3
        OnEnter = SynEditEnter
        UseCodeFolding = False
        ExtraLineSpacing = 0
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.Font.Quality = fqClearTypeNatural
        Gutter.Visible = False
        Gutter.Bands = <
          item
            Kind = gbkMarks
            Width = 15
          end
          item
            Kind = gbkLineNumbers
          end
          item
            Kind = gbkFold
          end
          item
            Kind = gbkMargin
            Width = 2
          end>
        HideSelection = True
        Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
        ScrollBars = ssNone
        SelectedColor.Alpha = 0.400000005960464500
        WantReturns = False
      end
      object btnFileName: TButton
        Left = 335
        Top = 14
        Width = 19
        Height = 20
        ImageIndex = 0
        ImageName = 'FileOpen'
        Images = vilImages
        TabOrder = 1
        OnClick = btnFileNameClick
      end
      object btnWorkDir: TButton
        Left = 357
        Top = 62
        Width = 19
        Height = 20
        ImageIndex = 1
        ImageName = 'OpenFolder'
        Images = vilImages
        TabOrder = 4
        OnClick = btnWorkDirClick
      end
      object btnRemoteFileName: TButton
        Left = 357
        Top = 14
        Width = 19
        Height = 20
        ImageIndex = 2
        ImageName = 'Download'
        Images = vilImages
        TabOrder = 5
        OnClick = btnRemoteFileNameClick
      end
    end
    object gbRemoteEngine: TGroupBox
      Left = 8
      Top = 171
      Width = 385
      Height = 68
      Caption = 'Python Engine'
      TabOrder = 2
      DesignSize = (
        385
        68)
      object Label1: TLabel
        Left = 8
        Top = 18
        Width = 66
        Height = 15
        Caption = 'Engine &Type:'
        Color = clNone
        FocusControl = cbEngineType
        ParentColor = False
      end
      object cbReinitializeBeforeRun: TCheckBox
        Left = 8
        Top = 44
        Width = 350
        Height = 21
        Anchors = [akTop, akRight]
        Caption = 'Reinitiali&ze Before Run'
        Color = clNone
        ParentColor = False
        TabOrder = 0
      end
      object cbEngineType: TComboBox
        Left = 120
        Top = 17
        Width = 103
        Height = 21
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        ItemIndex = 1
        ParentFont = False
        TabOrder = 1
        Text = 'Remote'
        OnChange = cbEngineTypeChange
        Items.Strings = (
          'Internal'
          'Remote'
          'Remote (Tk)'
          'Remote (Wx)'
          'SSH')
      end
    end
    object GroupBox3: TGroupBox
      Left = 7
      Top = 245
      Width = 385
      Height = 47
      Caption = 'External Run'
      TabOrder = 3
      object btnExternalRun: TButton
        Left = 52
        Top = 16
        Width = 277
        Height = 25
        Caption = '&Set External Run properties'
        TabOrder = 0
        OnClick = btnExternalRunClick
      end
    end
    object gbSaveOutput: TGroupBox
      Left = 8
      Top = 298
      Width = 385
      Height = 88
      Caption = 'Output'
      TabOrder = 4
      object Label4: TLabel
        Left = 8
        Top = 44
        Width = 56
        Height = 15
        Caption = 'File &Name:'
        Color = clNone
        FocusControl = SynOutputFileName
        ParentColor = False
      end
      object SynOutputFileName: TSynEdit
        Left = 105
        Top = 45
        Width = 246
        Height = 18
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Font.Quality = fqClearTypeNatural
        TabOrder = 1
        OnEnter = SynEditEnter
        UseCodeFolding = False
        ExtraLineSpacing = 0
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.Font.Quality = fqClearTypeNatural
        Gutter.Visible = False
        Gutter.Bands = <
          item
            Kind = gbkMarks
            Width = 15
          end
          item
            Kind = gbkLineNumbers
          end
          item
            Kind = gbkFold
          end
          item
            Kind = gbkMargin
            Width = 2
          end>
        HideSelection = True
        Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
        ScrollBars = ssNone
        SelectedColor.Alpha = 0.400000005960464500
        WantReturns = False
      end
      object btnOutputFileName: TButton
        Left = 357
        Top = 43
        Width = 19
        Height = 20
        ImageIndex = 0
        ImageName = 'FileOpen'
        Images = vilImages
        TabOrder = 2
        OnClick = btnOutputFileNameClick
      end
      object cbAppendToFile: TCheckBox
        Left = 8
        Top = 64
        Width = 350
        Height = 21
        Caption = '&Append to file'
        Color = clNone
        ParentColor = False
        TabOrder = 0
      end
      object cbSaveOutput: TCheckBox
        Left = 8
        Top = 20
        Width = 350
        Height = 21
        Caption = 'Save Output'
        Color = clNone
        ParentColor = False
        TabOrder = 3
        OnClick = cbSaveOutputClick
      end
    end
    object btnOK: TButton
      Left = 139
      Top = 402
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 5
    end
    object btnCancel: TButton
      Left = 223
      Top = 402
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 6
    end
    object btnHelp: TButton
      Left = 307
      Top = 402
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Help'
      TabOrder = 7
      OnClick = btnHelpClick
    end
  end
  object vilImages: TVirtualImageList
    Images = <
      item
        CollectionIndex = 38
        CollectionName = 'FileOpen'
        Name = 'FileOpen'
      end
      item
        CollectionIndex = 63
        CollectionName = 'OpenFolder'
        Name = 'OpenFolder'
      end
      item
        CollectionIndex = 23
        CollectionName = 'Download'
        Name = 'Download'
      end>
    ImageCollection = ResourcesDataModule.icSVGImages
    PreserveItems = True
    Left = 24
    Top = 384
  end
end
