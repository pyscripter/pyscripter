inherited NewFileDialog: TNewFileDialog
  HelpContext = 640
  Caption = 'New File'
  ClientHeight = 297
  ClientWidth = 466
  OnDestroy = FormDestroy
  OnShow = FormShow
  ExplicitWidth = 472
  ExplicitHeight = 326
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 466
    Height = 297
    Align = alClient
    UseDockManager = True
    TabOrder = 0
    TBXStyleBackground = True
    object Panel2: TSpTBXPanel
      Left = 2
      Top = 2
      Width = 462
      Height = 255
      Align = alTop
      UseDockManager = True
      TabOrder = 0
      TBXStyleBackground = True
      object Panel3: TSpTBXPanel
        Left = 2
        Top = 2
        Width = 185
        Height = 251
        Align = alLeft
        UseDockManager = True
        TabOrder = 0
        TBXStyleBackground = True
        object tvCategories: TVirtualStringTree
          Left = 2
          Top = 26
          Width = 181
          Height = 223
          Align = alBottom
          BevelInner = bvNone
          BevelOuter = bvNone
          BevelKind = bkFlat
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
          TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection]
          OnChange = tvCategoriesChange
          OnGetText = tvCategoriesGetText
          Columns = <>
        end
        object Label1: TSpTBXLabel
          Left = 9
          Top = 7
          Width = 62
          Height = 19
          Caption = 'Categories:'
        end
      end
      object Panel4: TSpTBXPanel
        Left = 190
        Top = 2
        Width = 270
        Height = 251
        Align = alClient
        UseDockManager = True
        TabOrder = 1
        TBXStyleBackground = True
        object Label2: TSpTBXLabel
          Left = 6
          Top = 7
          Width = 59
          Height = 19
          Caption = 'Templates:'
        end
        object lvTemplates: TEasyListview
          Left = 2
          Top = 26
          Width = 266
          Height = 223
          Align = alBottom
          EditManager.Font.Charset = DEFAULT_CHARSET
          EditManager.Font.Color = clWindowText
          EditManager.Font.Height = -11
          EditManager.Font.Name = 'Tahoma'
          EditManager.Font.Style = []
          Header.Columns.Items = {
            0600000001000000110000005445617379436F6C756D6E53746F726564FFFECE
            0006000000800800010100010000000000000161000000FFFFFF1F0001000000
            00000000000000000000000000000000}
          HotTrack.Enabled = True
          PaintInfoGroup.MarginBottom.CaptionIndent = 4
          TabOrder = 1
          OnItemDblClick = lvTemplatesItemDblClick
          OnItemSelectionsChanged = lvTemplatesItemSelectionsChanged
        end
      end
      object Splitter1: TSpTBXSplitter
        Left = 187
        Top = 2
        Width = 3
        Height = 251
        Cursor = crSizeWE
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
    end
    object btnManageTemplates: TSpTBXButton
      Left = 13
      Top = 263
      Width = 166
      Height = 25
      Caption = 'Manage File Templates...'
      TabOrder = 3
      OnClick = btnManageTemplatesClick
    end
  end
end
