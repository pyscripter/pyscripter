inherited FileTemplatesDialog: TFileTemplatesDialog
  Left = 340
  Top = 192
  HelpContext = 640
  Caption = 'File Templates'
  ClientHeight = 451
  ClientWidth = 554
  ShowHint = True
  OnClose = FormClose
  OnDestroy = FormDestroy
  TextHeight = 15
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 554
    Height = 451
    Align = alClient
    TabOrder = 0
    DesignSize = (
      554
      451)
    object GroupBox: TGroupBox
      Left = 5
      Top = 148
      Width = 544
      Height = 264
      Anchors = [akLeft, akTop, akRight]
      Caption = 'File Template:'
      TabOrder = 2
      DesignSize = (
        544
        264)
      object Label1: TLabel
        Left = 10
        Top = 24
        Width = 35
        Height = 15
        Caption = '&Name:'
        FocusControl = edName
      end
      object Label2: TLabel
        Left = 10
        Top = 70
        Width = 51
        Height = 15
        Caption = '&Template:'
        FocusControl = SynTemplate
      end
      object Label5: TLabel
        Left = 290
        Top = 20
        Width = 51
        Height = 15
        Caption = '&Category:'
        FocusControl = edCategory
      end
      object Label4: TLabel
        Left = 10
        Top = 242
        Width = 230
        Height = 15
        Caption = 'Press Shift+Ctrl+M for Modifier completion'
        Enabled = False
      end
      object Label3: TLabel
        Left = 10
        Top = 226
        Width = 235
        Height = 15
        Caption = 'Press Shift+Ctrl+P for Parameter completion'
        Enabled = False
      end
      object Label6: TLabel
        Left = 10
        Top = 44
        Width = 95
        Height = 15
        Caption = '&Default Extension:'
        FocusControl = edExtension
      end
      object Label7: TLabel
        Left = 288
        Top = 44
        Width = 63
        Height = 15
        Caption = '&Highlighter:'
        FocusControl = edCategory
      end
      object SynTemplate: TSynEdit
        Left = 10
        Top = 90
        Width = 524
        Height = 131
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        Font.Quality = fqClearTypeNatural
        TabOrder = 1
        UseCodeFolding = False
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
        Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
        SelectedColor.Alpha = 0.400000005960464500
        TabWidth = 4
      end
      object CBHighlighters: TComboBox
        Left = 400
        Top = 44
        Width = 134
        Height = 23
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
        OnChange = CBHighlightersChange
      end
      object edName: TEdit
        Left = 127
        Top = 17
        Width = 127
        Height = 23
        TabOrder = 0
      end
      object edCategory: TEdit
        Left = 400
        Top = 20
        Width = 134
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
      end
      object edExtension: TEdit
        Left = 216
        Top = 44
        Width = 38
        Height = 23
        TabOrder = 3
      end
    end
    object TBXButton1: TButton
      Left = 15
      Top = 118
      Width = 101
      Height = 24
      Action = actAddItem
      Images = vilImages
      TabOrder = 3
    end
    object TBXButton3: TButton
      Left = 120
      Top = 118
      Width = 101
      Height = 24
      Action = actDeleteItem
      Images = vilImages
      TabOrder = 4
    end
    object TBXButton4: TButton
      Left = 226
      Top = 118
      Width = 101
      Height = 24
      Action = actMoveUp
      Images = vilImages
      TabOrder = 5
    end
    object TBXButton5: TButton
      Left = 332
      Top = 118
      Width = 101
      Height = 24
      Action = actMoveDown
      Images = vilImages
      TabOrder = 6
    end
    object TBXButton2: TButton
      Left = 438
      Top = 118
      Width = 101
      Height = 24
      Action = actUpdateItem
      Images = vilImages
      TabOrder = 7
    end
    object btnCancel: TButton
      Left = 382
      Top = 418
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 9
    end
    object btnOK: TButton
      Left = 294
      Top = 418
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
    object btnHelp: TButton
      Left = 470
      Top = 418
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Help'
      TabOrder = 8
      OnClick = btnHelpClick
    end
    object lvItems: TListView
      Left = 1
      Top = 1
      Width = 552
      Height = 109
      Align = alTop
      Columns = <
        item
          Caption = 'Name'
          Width = 200
        end
        item
          Caption = 'Category'
          Width = 280
        end>
      ColumnClick = False
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnSelectItem = lvItemsSelectItem
    end
  end
  object ActionList: TActionList
    Images = vilImages
    OnUpdate = ActionListUpdate
    Left = 379
    Top = 27
    object actAddItem: TAction
      Caption = '&Add'
      Hint = 'Add item'
      ImageIndex = 4
      ImageName = 'Plus'
      OnExecute = actAddItemExecute
    end
    object actDeleteItem: TAction
      Caption = '&Delete'
      Hint = 'Delete item'
      ImageIndex = 0
      ImageName = 'Delete'
      OnExecute = actDeleteItemExecute
    end
    object actMoveUp: TAction
      Caption = '&Up'
      Hint = 'Move item up'
      ImageIndex = 2
      ImageName = 'Up'
      OnExecute = actMoveUpExecute
    end
    object actMoveDown: TAction
      Caption = '&Down'
      Hint = 'Move item down'
      ImageIndex = 3
      ImageName = 'Down'
      OnExecute = actMoveDownExecute
    end
    object actUpdateItem: TAction
      Caption = '&Update'
      Hint = 'Update item'
      ImageIndex = 1
      ImageName = 'Refresh'
      OnExecute = actUpdateItemExecute
    end
  end
  object vilImages: TVirtualImageList
    Images = <
      item
        CollectionIndex = 21
        CollectionName = 'Delete'
        Name = 'Delete'
      end
      item
        CollectionIndex = 88
        CollectionName = 'Refresh'
        Name = 'Refresh'
      end
      item
        CollectionIndex = 130
        CollectionName = 'Up'
        Name = 'Up'
      end
      item
        CollectionIndex = 22
        CollectionName = 'Down'
        Name = 'Down'
      end
      item
        CollectionIndex = 68
        CollectionName = 'Plus'
        Name = 'Plus'
      end>
    ImageCollection = ResourcesDataModule.icSVGImages
    PreserveItems = True
    Left = 320
    Top = 27
  end
end
