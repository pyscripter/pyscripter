inherited CustomizeParams: TCustomizeParams
  Left = 340
  Top = 192
  HelpContext = 720
  Caption = 'Custom Parameters'
  ClientHeight = 343
  ClientWidth = 435
  ShowHint = True
  OnDestroy = FormDestroy
  OnShow = FormShow
  ExplicitWidth = 441
  ExplicitHeight = 371
  PixelsPerInch = 96
  TextHeight = 13
  object Panel: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 435
    Height = 343
    Align = alClient
    TabOrder = 0
    TBXStyleBackground = True
    DesignSize = (
      435
      343)
    object TBXButton1: TSpTBXButton
      Left = 5
      Top = 183
      Width = 84
      Height = 24
      Action = actAddItem
      TabOrder = 3
      Images = CommandsDataModule.Images
      ImageIndex = 49
    end
    object TBXButton3: TSpTBXButton
      Left = 89
      Top = 183
      Width = 84
      Height = 24
      Action = actDeleteItem
      TabOrder = 4
      Images = CommandsDataModule.Images
      ImageIndex = 14
    end
    object TBXButton4: TSpTBXButton
      Left = 174
      Top = 183
      Width = 84
      Height = 24
      Action = actMoveUp
      TabOrder = 5
      Images = CommandsDataModule.Images
      ImageIndex = 47
    end
    object TBXButton5: TSpTBXButton
      Left = 258
      Top = 183
      Width = 84
      Height = 24
      Action = actMoveDown
      TabOrder = 6
      Images = CommandsDataModule.Images
      ImageIndex = 48
    end
    object TBXButton2: TSpTBXButton
      Left = 343
      Top = 183
      Width = 84
      Height = 24
      Action = actUpdateItem
      TabOrder = 7
      Images = CommandsDataModule.Images
      ImageIndex = 39
    end
    object btnOK: TSpTBXButton
      Left = 260
      Top = 308
      Width = 75
      Height = 25
      Caption = '&OK'
      Anchors = [akRight, akBottom]
      TabOrder = 1
      Default = True
      ModalResult = 1
    end
    object btnCancel: TSpTBXButton
      Left = 340
      Top = 308
      Width = 75
      Height = 25
      Caption = '&Cancel'
      Anchors = [akRight, akBottom]
      TabOrder = 0
      Cancel = True
      ModalResult = 2
    end
    object Label3: TSpTBXLabel
      Left = 12
      Top = 304
      Width = 220
      Height = 19
      Caption = 'Press Shift+Ctrl+P for Parameter completion'
      Enabled = False
    end
    object Label4: TSpTBXLabel
      Left = 12
      Top = 319
      Width = 210
      Height = 19
      Caption = 'Press Shift+Ctrl+M for Modifier completion'
      Enabled = False
    end
    object GroupBox1: TSpTBXGroupBox
      Left = 10
      Top = 215
      Width = 414
      Height = 83
      Caption = 'Name-Value Pair'
      TabOrder = 2
      TBXStyleBackground = True
      object SynValue: TSynEdit
        Left = 61
        Top = 48
        Width = 330
        Height = 20
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Pitch = fpFixed
        Font.Style = []
        TabOrder = 1
        BorderStyle = bsNone
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
      object Label1: TSpTBXLabel
        Left = 14
        Top = 28
        Width = 37
        Height = 19
        Caption = '&Name:'
      end
      object Label2: TSpTBXLabel
        Left = 14
        Top = 50
        Width = 36
        Height = 19
        Caption = '&Value:'
      end
      object edName: TSpTBXEdit
        Left = 61
        Top = 23
        Width = 121
        Height = 21
        TabOrder = 0
        OnKeyPress = edNameKeyPress
      end
    end
    object lvItems: TEasyListview
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 425
      Height = 174
      Align = alTop
      EditManager.Font.Charset = DEFAULT_CHARSET
      EditManager.Font.Color = clWindowText
      EditManager.Font.Height = -11
      EditManager.Font.Name = 'Tahoma'
      EditManager.Font.Style = []
      Header.Columns.Items = {
        0600000002000000110000005445617379436F6C756D6E53746F726564FFFECE
        000600000080080001010001000000000000018C000000FFFFFF1F0001000000
        01000000040000004E0061006D00650000000000000000000000000011000000
        5445617379436F6C756D6E53746F726564FFFECE000600000080080001010001
        01000000000000FA000000FFFFFF1F0001000000010000000500000056006100
        6C0075006500000000000000000000000000}
      Header.Draggable = False
      Header.Visible = True
      HotTrack.Enabled = True
      PaintInfoGroup.MarginBottom.CaptionIndent = 4
      Selection.FullRowSelect = True
      TabOrder = 10
      View = elsReport
      OnColumnClick = lvItemsColumnClick
      OnItemSelectionsChanged = lvItemsItemSelectionsChanged
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
