inherited FileTemplatesDialog: TFileTemplatesDialog
  Left = 340
  Top = 192
  HelpContext = 640
  Caption = 'File Templates'
  ClientHeight = 451
  ClientWidth = 528
  ShowHint = True
  OnDestroy = FormDestroy
  ExplicitWidth = 534
  ExplicitHeight = 480
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
    object GroupBox: TGroupBox
      Left = 5
      Top = 148
      Width = 518
      Height = 264
      Anchors = [akLeft, akTop, akRight]
      Caption = 'File Template:'
      TabOrder = 2
      object Label1: TLabel
        Left = 10
        Top = 24
        Width = 31
        Height = 13
        Caption = '&Name:'
        FocusControl = edName
      end
      object Label2: TLabel
        Left = 10
        Top = 70
        Width = 48
        Height = 13
        Caption = '&Template:'
        FocusControl = SynTemplate
      end
      object Label5: TLabel
        Left = 290
        Top = 20
        Width = 49
        Height = 13
        Caption = '&Category:'
        FocusControl = edCategory
      end
      object Label4: TLabel
        Left = 10
        Top = 242
        Width = 204
        Height = 13
        Caption = 'Press Shift+Ctrl+M for Modifier completion'
        Enabled = False
      end
      object Label3: TLabel
        Left = 10
        Top = 226
        Width = 214
        Height = 13
        Caption = 'Press Shift+Ctrl+P for Parameter completion'
        Enabled = False
      end
      object Label6: TLabel
        Left = 10
        Top = 44
        Width = 89
        Height = 13
        Caption = '&Default Extension:'
        FocusControl = edExtension
      end
      object Label7: TLabel
        Left = 288
        Top = 44
        Width = 55
        Height = 13
        Caption = '&Highlighter:'
        FocusControl = edCategory
      end
      object SynTemplate: TSynEdit
        Left = 10
        Top = 90
        Width = 498
        Height = 131
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 1
        CodeFolding.GutterShapeSize = 11
        CodeFolding.CollapsedLineColor = clGrayText
        CodeFolding.FolderBarLinesColor = clGrayText
        CodeFolding.IndentGuidesColor = clGray
        CodeFolding.IndentGuides = True
        CodeFolding.ShowCollapsedLine = False
        CodeFolding.ShowHintMark = True
        UseCodeFolding = False
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.Width = 0
        Options = [eoAutoIndent, eoAutoSizeMaxScrollWidth, eoDragDropEditing, eoEnhanceHomeKey, eoGroupUndo, eoHideShowScrollbars, eoKeepCaretX, eoShowScrollHint, eoTrimTrailingSpaces]
        TabWidth = 4
        FontSmoothing = fsmNone
      end
      object CBHighlighters: TComboBox
        Left = 377
        Top = 44
        Width = 131
        Height = 21
        Style = csDropDownList
        TabOrder = 4
        OnChange = CBHighlightersChange
      end
      object edName: TEdit
        Left = 111
        Top = 17
        Width = 127
        Height = 21
        TabOrder = 0
        OnKeyPress = edNameKeyPress
      end
      object edCategory: TEdit
        Left = 377
        Top = 20
        Width = 131
        Height = 21
        TabOrder = 2
      end
      object edExtension: TEdit
        Left = 195
        Top = 44
        Width = 38
        Height = 21
        TabOrder = 3
        OnKeyPress = edNameKeyPress
      end
    end
    object TBXButton1: TButton
      Left = 24
      Top = 118
      Width = 84
      Height = 24
      Action = actAddItem
      Images = CommandsDataModule.Images
      TabOrder = 3
    end
    object TBXButton3: TButton
      Left = 122
      Top = 118
      Width = 84
      Height = 24
      Action = actDeleteItem
      Images = CommandsDataModule.Images
      TabOrder = 4
    end
    object TBXButton4: TButton
      Left = 220
      Top = 118
      Width = 84
      Height = 24
      Action = actMoveUp
      Images = CommandsDataModule.Images
      TabOrder = 5
    end
    object TBXButton5: TButton
      Left = 318
      Top = 118
      Width = 84
      Height = 24
      Action = actMoveDown
      Images = CommandsDataModule.Images
      TabOrder = 6
    end
    object TBXButton2: TButton
      Left = 417
      Top = 118
      Width = 84
      Height = 24
      Action = actUpdateItem
      Images = CommandsDataModule.Images
      TabOrder = 7
    end
    object btnCancel: TButton
      Left = 356
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
      Left = 268
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
      Left = 444
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
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = lvItemsChange
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
      Caption = '&Up'
      Hint = 'Move item up'
      ImageIndex = 47
      OnExecute = actMoveUpExecute
    end
    object actMoveDown: TAction
      Caption = '&Down'
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
