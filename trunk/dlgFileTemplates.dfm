object FileTemplatesDialog: TFileTemplatesDialog
  Left = 340
  Top = 192
  HelpContext = 640
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'File Templates'
  ClientHeight = 451
  ClientWidth = 528
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 528
    Height = 451
    Align = alClient
    TabOrder = 0
    DesignSize = (
      528
      451)
    object lvItems: TListView
      Left = 1
      Top = 1
      Width = 526
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
    object btnCancel: TButton
      Left = 356
      Top = 418
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnOK: TButton
      Left = 268
      Top = 418
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 2
    end
    object JvGroupBox: TJvGroupBox
      Left = 10
      Top = 150
      Width = 511
      Height = 261
      Caption = 'File Template:'
      TabOrder = 3
      object Label1: TLabel
        Left = 8
        Top = 24
        Width = 31
        Height = 13
        Caption = '&Name:'
        FocusControl = edName
      end
      object Label2: TLabel
        Left = 8
        Top = 70
        Width = 48
        Height = 13
        Caption = '&Template:'
        FocusControl = SynTemplate
      end
      object Label5: TLabel
        Left = 281
        Top = 21
        Width = 49
        Height = 13
        Caption = '&Category:'
        FocusControl = edCategory
      end
      object Label4: TLabel
        Left = 8
        Top = 242
        Width = 204
        Height = 13
        Caption = 'Press Shift+Ctrl+M for Modifier completion'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGradientActiveCaption
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        ParentFont = False
      end
      object Label3: TLabel
        Left = 7
        Top = 228
        Width = 214
        Height = 13
        Caption = 'Press Shift+Ctrl+P for Parameter completion'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGradientActiveCaption
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        ParentFont = False
      end
      object Label6: TLabel
        Left = 8
        Top = 47
        Width = 89
        Height = 13
        Caption = '&Default Extension:'
        FocusControl = edExtension
      end
      object Label7: TLabel
        Left = 280
        Top = 47
        Width = 55
        Height = 13
        Caption = '&Highlighter:'
        FocusControl = edCategory
      end
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
      object edName: TEdit
        Left = 46
        Top = 20
        Width = 185
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
      object edCategory: TEdit
        Left = 340
        Top = 17
        Width = 157
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
      end
      object edExtension: TEdit
        Left = 102
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
      object CBHighlighters: TComboBox
        Left = 340
        Top = 43
        Width = 145
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
    end
    object TBXButton1: TTBXButton
      Left = 9
      Top = 118
      Width = 64
      Height = 24
      Action = actAddItem
      ButtonStyle = bsFlat
      ImageIndex = 49
      Images = CommandsDataModule.Images
      TabOrder = 4
    end
    object TBXButton3: TTBXButton
      Left = 115
      Top = 118
      Width = 64
      Height = 24
      Action = actDeleteItem
      ButtonStyle = bsFlat
      ImageIndex = 14
      Images = CommandsDataModule.Images
      TabOrder = 5
    end
    object TBXButton4: TTBXButton
      Left = 222
      Top = 118
      Width = 80
      Height = 24
      Action = actMoveUp
      ButtonStyle = bsFlat
      ImageIndex = 47
      Images = CommandsDataModule.Images
      TabOrder = 6
    end
    object TBXButton5: TTBXButton
      Left = 329
      Top = 118
      Width = 84
      Height = 24
      Action = actMoveDown
      ButtonStyle = bsFlat
      ImageIndex = 48
      Images = CommandsDataModule.Images
      TabOrder = 7
    end
    object TBXButton2: TTBXButton
      Left = 436
      Top = 118
      Width = 66
      Height = 24
      Action = actUpdateItem
      ButtonStyle = bsFlat
      ImageIndex = 39
      Images = CommandsDataModule.Images
      TabOrder = 8
    end
    object btnHelp: TButton
      Left = 444
      Top = 418
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Help'
      TabOrder = 9
      OnClick = btnHelpClick
    end
  end
  object ActionList: TActionList
    Images = CommandsDataModule.Images
    OnUpdate = ActionListUpdate
    Left = 379
    Top = 27
    object actAddItem: TAction
      Caption = '&Add'
      Hint = 'Add item'
      ImageIndex = 49
      OnExecute = actAddItemExecute
    end
    object actDeleteItem: TAction
      Caption = '&Delete'
      Hint = 'Delete item'
      ImageIndex = 14
      OnExecute = actDeleteItemExecute
    end
    object actMoveUp: TAction
      Caption = 'Move &Up'
      Hint = 'Move item up'
      ImageIndex = 47
      OnExecute = actMoveUpExecute
    end
    object actMoveDown: TAction
      Caption = 'Move &Down'
      Hint = 'Move item down'
      ImageIndex = 48
      OnExecute = actMoveDownExecute
    end
    object actUpdateItem: TAction
      Caption = '&Update'
      Hint = 'Update item'
      ImageIndex = 39
      OnExecute = actUpdateItemExecute
    end
  end
end
