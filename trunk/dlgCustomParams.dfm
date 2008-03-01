object CustomizeParams: TCustomizeParams
  Left = 340
  Top = 192
  HelpContext = 720
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Custom Parameters'
  ClientHeight = 343
  ClientWidth = 435
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
    Width = 435
    Height = 343
    Align = alClient
    TabOrder = 0
    DesignSize = (
      435
      343)
    object Label3: TLabel
      Left = 12
      Top = 304
      Width = 214
      Height = 13
      Caption = 'Press Shift+Ctrl+P for Parameter completion'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 12
      Top = 319
      Width = 204
      Height = 13
      Caption = 'Press Shift+Ctrl+M for Modifier completion'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = []
      ParentFont = False
    end
    object lvItems: TListView
      Left = 1
      Top = 1
      Width = 433
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
    object GroupBox1: TGroupBox
      Left = 10
      Top = 215
      Width = 414
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
        Width = 330
        Height = 20
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 1
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
      object edName: TEdit
        Left = 61
        Top = 23
        Width = 121
        Height = 22
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnKeyPress = edNameKeyPress
      end
    end
    object TBXButton1: TSpTBXButton
      Left = 5
      Top = 183
      Width = 84
      Height = 24
      Action = actAddItem
      TabOrder = 4
      Images = CommandsDataModule.Images
      ImageIndex = 49
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'Tahoma'
      LinkFont.Style = [fsUnderline]
    end
    object TBXButton3: TSpTBXButton
      Left = 89
      Top = 183
      Width = 84
      Height = 24
      Action = actDeleteItem
      TabOrder = 5
      Images = CommandsDataModule.Images
      ImageIndex = 14
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'Tahoma'
      LinkFont.Style = [fsUnderline]
    end
    object TBXButton4: TSpTBXButton
      Left = 174
      Top = 183
      Width = 84
      Height = 24
      Action = actMoveUp
      TabOrder = 6
      Images = CommandsDataModule.Images
      ImageIndex = 47
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'Tahoma'
      LinkFont.Style = [fsUnderline]
    end
    object TBXButton5: TSpTBXButton
      Left = 258
      Top = 183
      Width = 84
      Height = 24
      Action = actMoveDown
      TabOrder = 7
      Images = CommandsDataModule.Images
      ImageIndex = 48
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'Tahoma'
      LinkFont.Style = [fsUnderline]
    end
    object TBXButton2: TSpTBXButton
      Left = 343
      Top = 183
      Width = 84
      Height = 24
      Action = actUpdateItem
      TabOrder = 8
      Images = CommandsDataModule.Images
      ImageIndex = 39
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'Tahoma'
      LinkFont.Style = [fsUnderline]
    end
    object btnOK: TSpTBXButton
      Left = 260
      Top = 308
      Width = 75
      Height = 25
      Caption = 'OK'
      Anchors = [akRight, akBottom]
      TabOrder = 2
      Default = True
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'Tahoma'
      LinkFont.Style = [fsUnderline]
      ModalResult = 1
    end
    object btnCancel: TSpTBXButton
      Left = 340
      Top = 308
      Width = 75
      Height = 25
      Caption = 'Cancel'
      Anchors = [akRight, akBottom]
      TabOrder = 1
      Cancel = True
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'Tahoma'
      LinkFont.Style = [fsUnderline]
      ModalResult = 2
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
