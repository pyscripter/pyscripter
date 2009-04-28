inherited FileTemplatesDialog: TFileTemplatesDialog
  Left = 340
  Top = 192
  HelpContext = 640
  ActiveControl = edExtension
  BorderIcons = []
  Caption = 'File Templates'
  ClientHeight = 451
  ClientWidth = 528
  Font.Name = 'MS Shell Dlg 2'
  Position = poMainFormCenter
  ShowHint = True
  OnDestroy = FormDestroy
  ExplicitWidth = 534
  ExplicitHeight = 477
  PixelsPerInch = 96
  TextHeight = 13
  object Panel: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 528
    Height = 451
    SkinType = sknSkin
    Align = alClient
    TabOrder = 0
    DesignSize = (
      528
      451)
    object GroupBox: TSpTBXGroupBox
      Left = 14
      Top = 148
      Width = 511
      Height = 261
      Caption = 'File Template:'
      SkinType = sknSkin
      TabOrder = 3
      object SynTemplate: TSynEdit
        Left = 7
        Top = 87
        Width = 498
        Height = 134
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 1
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.Width = 0
        Options = [eoAutoIndent, eoAutoSizeMaxScrollWidth, eoDragDropEditing, eoEnhanceHomeKey, eoGroupUndo, eoHideShowScrollbars, eoKeepCaretX, eoShowScrollHint, eoTrimTrailingSpaces]
        TabWidth = 4
      end
      object edName: TSpTBXEdit
        Left = 106
        Top = 20
        Width = 127
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnKeyPress = edNameKeyPress
      end
      object edCategory: TSpTBXEdit
        Left = 366
        Top = 17
        Width = 131
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
      end
      object edExtension: TSpTBXEdit
        Left = 195
        Top = 44
        Width = 38
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        OnKeyPress = edNameKeyPress
      end
      object CBHighlighters: TSpTBXComboBox
        Left = 366
        Top = 44
        Width = 131
        Height = 21
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        ItemHeight = 13
        ParentFont = False
        TabOrder = 4
        OnChange = CBHighlightersChange
      end
      object Label1: TSpTBXLabel
        Left = 8
        Top = 24
        Width = 31
        Height = 13
        Caption = '&Name:'
        ParentColor = True
        FocusControl = edName
      end
      object Label2: TSpTBXLabel
        Left = 8
        Top = 70
        Width = 48
        Height = 13
        Caption = '&Template:'
        ParentColor = True
        FocusControl = SynTemplate
      end
      object Label5: TSpTBXLabel
        Left = 271
        Top = 21
        Width = 49
        Height = 13
        Caption = '&Category:'
        ParentColor = True
        FocusControl = edCategory
      end
      object Label4: TSpTBXLabel
        Left = 7
        Top = 242
        Width = 204
        Height = 13
        Caption = 'Press Shift+Ctrl+M for Modifier completion'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGrayText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        ParentColor = True
        ParentFont = False
      end
      object Label3: TSpTBXLabel
        Left = 7
        Top = 228
        Width = 214
        Height = 13
        Caption = 'Press Shift+Ctrl+P for Parameter completion'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGrayText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        ParentColor = True
        ParentFont = False
      end
      object Label6: TSpTBXLabel
        Left = 8
        Top = 47
        Width = 89
        Height = 13
        Caption = '&Default Extension:'
        ParentColor = True
        FocusControl = edExtension
      end
      object Label7: TSpTBXLabel
        Left = 270
        Top = 47
        Width = 55
        Height = 13
        Caption = '&Highlighter:'
        ParentColor = True
        FocusControl = edCategory
      end
    end
    object TBXButton1: TSpTBXButton
      Left = 24
      Top = 118
      Width = 84
      Height = 24
      Action = actAddItem
      TabOrder = 4
      Images = CommandsDataModule.Images
      ImageIndex = 49
    end
    object TBXButton3: TSpTBXButton
      Left = 122
      Top = 118
      Width = 84
      Height = 24
      Action = actDeleteItem
      TabOrder = 5
      Images = CommandsDataModule.Images
      ImageIndex = 14
    end
    object TBXButton4: TSpTBXButton
      Left = 220
      Top = 118
      Width = 84
      Height = 24
      Action = actMoveUp
      TabOrder = 6
      Images = CommandsDataModule.Images
      ImageIndex = 47
    end
    object TBXButton5: TSpTBXButton
      Left = 318
      Top = 118
      Width = 84
      Height = 24
      Action = actMoveDown
      TabOrder = 7
      Images = CommandsDataModule.Images
      ImageIndex = 48
    end
    object TBXButton2: TSpTBXButton
      Left = 417
      Top = 118
      Width = 84
      Height = 24
      Action = actUpdateItem
      TabOrder = 8
      Images = CommandsDataModule.Images
      ImageIndex = 39
    end
    object btnCancel: TSpTBXButton
      Left = 356
      Top = 418
      Width = 75
      Height = 25
      Caption = '&Cancel'
      Anchors = [akRight, akBottom]
      TabOrder = 1
      Cancel = True
      ModalResult = 2
    end
    object btnOK: TSpTBXButton
      Left = 268
      Top = 418
      Width = 75
      Height = 25
      Caption = '&OK'
      Anchors = [akRight, akBottom]
      TabOrder = 2
      Default = True
      ModalResult = 1
    end
    object btnHelp: TSpTBXButton
      Left = 444
      Top = 418
      Width = 75
      Height = 25
      Caption = '&Help'
      Anchors = [akRight, akBottom]
      TabOrder = 9
      OnClick = btnHelpClick
    end
    object lvItems: TTntListView
      Left = 2
      Top = 2
      Width = 524
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
      GridLines = True
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = lvItemsChange
    end
  end
  object ActionList: TTntActionList
    Images = CommandsDataModule.Images
    OnUpdate = ActionListUpdate
    Left = 379
    Top = 27
    object actAddItem: TTntAction
      Caption = '&Add'
      Hint = 'Add item'
      ImageIndex = 49
      OnExecute = actAddItemExecute
    end
    object actDeleteItem: TTntAction
      Caption = '&Delete'
      Hint = 'Delete item'
      ImageIndex = 14
      OnExecute = actDeleteItemExecute
    end
    object actMoveUp: TTntAction
      Caption = '&Up'
      Hint = 'Move item up'
      ImageIndex = 47
      OnExecute = actMoveUpExecute
    end
    object actMoveDown: TTntAction
      Caption = '&Down'
      Hint = 'Move item down'
      ImageIndex = 48
      OnExecute = actMoveDownExecute
    end
    object actUpdateItem: TTntAction
      Caption = '&Update'
      Hint = 'Update item'
      ImageIndex = 39
      OnExecute = actUpdateItemExecute
    end
  end
end
