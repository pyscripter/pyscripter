object CustomizeParams: TCustomizeParams
  Left = 340
  Top = 192
  HelpContext = 720
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Custom Parameters'
  ClientHeight = 343
  ClientWidth = 415
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 415
    Height = 343
    Align = alClient
    TabOrder = 0
    DesignSize = (
      415
      343)
    object Label3: TLabel
      Left = 12
      Top = 304
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
    object Label4: TLabel
      Left = 13
      Top = 319
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
    object lvItems: TListView
      Left = 1
      Top = 1
      Width = 413
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
      GridLines = True
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = lvItemsChange
    end
    object btnCancel: TButton
      Left = 320
      Top = 308
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnOK: TButton
      Left = 240
      Top = 308
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 2
    end
    object JvGroupBox1: TJvGroupBox
      Left = 10
      Top = 215
      Width = 394
      Height = 83
      Caption = 'Name-Value Pair'
      TabOrder = 3
      object Label1: TLabel
        Left = 14
        Top = 28
        Width = 31
        Height = 13
        Caption = '&Name:'
        FocusControl = edName
      end
      object Label2: TLabel
        Left = 14
        Top = 50
        Width = 30
        Height = 13
        Caption = '&Value:'
        FocusControl = SynValue
      end
      object SynValue: TSynEdit
        Left = 61
        Top = 48
        Width = 320
        Height = 20
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Lucida Console'
        Font.Style = []
        TabOrder = 1
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.Width = 0
        Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
        ScrollBars = ssNone
        WantReturns = False
      end
      object edName: TEdit
        Left = 61
        Top = 23
        Width = 121
        Height = 20
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Lucida Console'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnKeyPress = edNameKeyPress
      end
    end
    object TBXButton1: TTBXButton
      Left = 10
      Top = 183
      Width = 64
      Height = 24
      Action = actAddItem
      ButtonStyle = bsFlat
      ImageIndex = 49
      Images = CommandsDataModule.Images
      TabOrder = 4
    end
    object TBXButton3: TTBXButton
      Left = 81
      Top = 183
      Width = 64
      Height = 24
      Action = actDeleteItem
      ButtonStyle = bsFlat
      ImageIndex = 14
      Images = CommandsDataModule.Images
      TabOrder = 5
    end
    object TBXButton4: TTBXButton
      Left = 152
      Top = 183
      Width = 80
      Height = 24
      Action = actMoveUp
      ButtonStyle = bsFlat
      ImageIndex = 47
      Images = CommandsDataModule.Images
      TabOrder = 6
    end
    object TBXButton5: TTBXButton
      Left = 239
      Top = 183
      Width = 84
      Height = 24
      Action = actMoveDown
      ButtonStyle = bsFlat
      ImageIndex = 48
      Images = CommandsDataModule.Images
      TabOrder = 7
    end
    object TBXButton2: TTBXButton
      Left = 330
      Top = 183
      Width = 66
      Height = 24
      Action = actUpdateItem
      ButtonStyle = bsFlat
      ImageIndex = 39
      Images = CommandsDataModule.Images
      TabOrder = 8
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
