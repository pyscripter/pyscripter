inherited CustomizeParams: TCustomizeParams
  Left = 340
  Top = 192
  HelpContext = 720
  Caption = 'Custom Parameters'
  ClientHeight = 343
  ClientWidth = 557
  ShowHint = True
  OnDestroy = FormDestroy
  OnShow = FormShow
  ExplicitWidth = 563
  ExplicitHeight = 372
  PixelsPerInch = 96
  TextHeight = 13
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 557
    Height = 343
    Align = alClient
    TabOrder = 0
    DesignSize = (
      557
      343)
    object Label3: TLabel
      Left = 12
      Top = 304
      Width = 214
      Height = 13
      Caption = 'Press Shift+Ctrl+P for Parameter completion'
      Enabled = False
    end
    object Label4: TLabel
      Left = 12
      Top = 319
      Width = 204
      Height = 13
      Caption = 'Press Shift+Ctrl+M for Modifier completion'
      Enabled = False
    end
    object TBXButton1: TButton
      Left = 8
      Top = 183
      Width = 100
      Height = 24
      Action = actAddItem
      Images = CommandsDataModule.Images
      TabOrder = 3
    end
    object TBXButton3: TButton
      Left = 117
      Top = 183
      Width = 100
      Height = 24
      Action = actDeleteItem
      Images = CommandsDataModule.Images
      TabOrder = 4
    end
    object TBXButton4: TButton
      Left = 227
      Top = 183
      Width = 100
      Height = 24
      Action = actMoveUp
      Images = CommandsDataModule.Images
      TabOrder = 5
    end
    object TBXButton5: TButton
      Left = 336
      Top = 183
      Width = 100
      Height = 24
      Action = actMoveDown
      Images = CommandsDataModule.Images
      TabOrder = 6
    end
    object TBXButton2: TButton
      Left = 446
      Top = 183
      Width = 100
      Height = 24
      Action = actUpdateItem
      Images = CommandsDataModule.Images
      TabOrder = 7
    end
    object btnOK: TButton
      Left = 382
      Top = 308
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
    object btnCancel: TButton
      Left = 462
      Top = 308
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 8
    end
    object GroupBox1: TGroupBox
      Left = 10
      Top = 215
      Width = 536
      Height = 83
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'Name-Value Pair'
      TabOrder = 2
      object Label1: TLabel
        Left = 14
        Top = 28
        Width = 31
        Height = 13
        Caption = '&Name:'
      end
      object Label2: TLabel
        Left = 14
        Top = 50
        Width = 30
        Height = 13
        Caption = '&Value:'
      end
      object SynValue: TSynEdit
        Left = 133
        Top = 48
        Width = 380
        Height = 20
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Pitch = fpFixed
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
        HideSelection = True
        Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
        ScrollBars = ssNone
        WantReturns = False
        FontSmoothing = fsmNone
      end
      object edName: TEdit
        Left = 133
        Top = 23
        Width = 155
        Height = 21
        TabOrder = 0
        OnKeyPress = edNameKeyPress
      end
    end
    object lvItems: TListView
      Left = 1
      Top = 1
      Width = 555
      Height = 171
      Align = alTop
      Columns = <
        item
          Caption = 'Name'
          Width = 140
        end
        item
          Caption = 'Value'
          Width = 250
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
    Images = CommandsDataModule.Images
    OnUpdate = ActionListUpdate
    Left = 378
    Top = 138
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
