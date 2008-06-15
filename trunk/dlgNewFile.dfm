inherited NewFileDialog: TNewFileDialog
  HelpContext = 640
  Caption = 'New File'
  ClientHeight = 297
  ClientWidth = 466
  Font.Name = 'MS Shell Dlg 2'
  Position = poMainFormCenter
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 466
    Height = 297
    ThemeType = thtWindows
    Align = alClient
    Color = clBtnFace
    UseDockManager = True
    ParentColor = False
    TabOrder = 0
    object Panel2: TSpTBXPanel
      Left = 2
      Top = 2
      Width = 462
      Height = 255
      ThemeType = thtWindows
      Align = alTop
      Color = clBtnFace
      UseDockManager = True
      ParentColor = False
      TabOrder = 0
      object Panel3: TSpTBXPanel
        Left = 2
        Top = 2
        Width = 185
        Height = 251
        ThemeType = thtWindows
        Align = alLeft
        Color = clBtnFace
        UseDockManager = True
        ParentColor = False
        TabOrder = 0
        object tvCategories: TVirtualStringTree
          Left = 2
          Top = 26
          Width = 181
          Height = 223
          Align = alBottom
          Header.AutoSizeIndex = 0
          Header.Font.Charset = DEFAULT_CHARSET
          Header.Font.Color = clWindowText
          Header.Font.Height = -11
          Header.Font.Name = 'MS Shell Dlg 2'
          Header.Font.Style = []
          Header.MainColumn = -1
          Header.Options = [hoColumnResize, hoDrag]
          NodeDataSize = 0
          TabOrder = 0
          OnChange = tvCategoriesChange
          OnGetText = tvCategoriesGetText
          Columns = <>
        end
        object Label1: TSpTBXLabel
          Left = 9
          Top = 7
          Width = 56
          Height = 13
          Caption = 'Categories:'
          ParentColor = True
          LinkFont.Charset = DEFAULT_CHARSET
          LinkFont.Color = clBlue
          LinkFont.Height = -11
          LinkFont.Name = 'MS Shell Dlg 2'
          LinkFont.Style = [fsUnderline]
        end
      end
      object Panel4: TSpTBXPanel
        Left = 190
        Top = 2
        Width = 270
        Height = 251
        ThemeType = thtWindows
        Align = alClient
        Color = clBtnFace
        UseDockManager = True
        ParentColor = False
        TabOrder = 1
        object Label2: TSpTBXLabel
          Left = 6
          Top = 7
          Width = 53
          Height = 13
          Caption = 'Templates:'
          ParentColor = True
          LinkFont.Charset = DEFAULT_CHARSET
          LinkFont.Color = clBlue
          LinkFont.Height = -11
          LinkFont.Name = 'MS Shell Dlg 2'
          LinkFont.Style = [fsUnderline]
        end
        object lvTemplates: TTntListView
          Left = 2
          Top = 26
          Width = 266
          Height = 223
          Align = alBottom
          Columns = <>
          FlatScrollBars = True
          ReadOnly = True
          TabOrder = 0
          OnDblClick = lvTemplatesDblClick
          OnSelectItem = lvTemplatesSelectItem
        end
      end
      object Splitter1: TSpTBXSplitter
        Left = 187
        Top = 2
        Width = 3
        Height = 251
        Cursor = crSizeWE
        ThemeType = thtWindows
        MinSize = 30
      end
    end
    object btnCancel: TSpTBXButton
      Left = 378
      Top = 264
      Width = 75
      Height = 25
      Caption = '&Cancel'
      TabOrder = 1
      Cancel = True
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'MS Shell Dlg 2'
      LinkFont.Style = [fsUnderline]
      ModalResult = 2
    end
    object btnCreate: TSpTBXButton
      Left = 293
      Top = 265
      Width = 75
      Height = 25
      Caption = 'C&reate'
      Enabled = False
      TabOrder = 2
      OnClick = btnCreateClick
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'MS Shell Dlg 2'
      LinkFont.Style = [fsUnderline]
    end
    object btnManageTemplates: TSpTBXButton
      Left = 13
      Top = 263
      Width = 166
      Height = 25
      Caption = 'Manage File Templates...'
      TabOrder = 3
      OnClick = btnManageTemplatesClick
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'MS Shell Dlg 2'
      LinkFont.Style = [fsUnderline]
    end
  end
end
