inherited CodeTemplates: TCodeTemplates
  Left = 340
  Top = 192
  HelpContext = 540
  Caption = 'Code Templates'
  ClientHeight = 426
  ClientWidth = 430
  ShowHint = True
  OnClose = FormClose
  OnDestroy = FormDestroy
  OnShow = FormShow
  ExplicitWidth = 436
  ExplicitHeight = 454
  PixelsPerInch = 96
  TextHeight = 13
  object Panel: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 430
    Height = 426
    Align = alClient
    TabOrder = 0
    TBXStyleBackground = True
    DesignSize = (
      430
      426)
    object btnAdd: TSpTBXButton
      Left = 6
      Top = 118
      Width = 84
      Height = 24
      Action = actAddItem
      TabOrder = 3
      Images = CommandsDataModule.Images
      ImageIndex = 49
    end
    object btnDelete: TSpTBXButton
      Left = 89
      Top = 118
      Width = 84
      Height = 24
      Action = actDeleteItem
      TabOrder = 4
      Images = CommandsDataModule.Images
      ImageIndex = 14
    end
    object btnMoveup: TSpTBXButton
      Left = 173
      Top = 118
      Width = 84
      Height = 24
      Action = actMoveUp
      TabOrder = 5
      Images = CommandsDataModule.Images
      ImageIndex = 47
    end
    object btnMoveDown: TSpTBXButton
      Left = 257
      Top = 118
      Width = 84
      Height = 24
      Action = actMoveDown
      TabOrder = 6
      Images = CommandsDataModule.Images
      ImageIndex = 48
    end
    object btnUpdate: TSpTBXButton
      Left = 341
      Top = 118
      Width = 84
      Height = 24
      Action = actUpdateItem
      TabOrder = 7
      Images = CommandsDataModule.Images
      ImageIndex = 39
    end
    object btnCancel: TSpTBXButton
      Left = 263
      Top = 394
      Width = 75
      Height = 25
      Caption = '&Cancel'
      Anchors = [akRight, akBottom]
      TabOrder = 0
      Cancel = True
      ModalResult = 2
    end
    object btnOK: TSpTBXButton
      Left = 179
      Top = 394
      Width = 75
      Height = 25
      Caption = '&OK'
      Anchors = [akRight, akBottom]
      TabOrder = 1
      Default = True
      ModalResult = 1
    end
    object btnHelp: TSpTBXButton
      Left = 347
      Top = 394
      Width = 75
      Height = 25
      Caption = '&Help'
      Anchors = [akRight, akBottom]
      TabOrder = 8
      OnClick = btnHelpClick
    end
    object GroupBox: TSpTBXGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 148
      Width = 424
      Height = 239
      Caption = 'Code Template:'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      TBXStyleBackground = True
      DesignSize = (
        424
        239)
      object SynTemplate: TSynEdit
        Left = 9
        Top = 90
        Width = 408
        Height = 110
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Pitch = fpFixed
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
        FontSmoothing = fsmNone
      end
      object Label1: TSpTBXLabel
        Left = 8
        Top = 21
        Width = 37
        Height = 19
        Caption = '&Name:'
      end
      object Label2: TSpTBXLabel
        Left = 8
        Top = 65
        Width = 54
        Height = 19
        Caption = '&Template:'
      end
      object Label5: TSpTBXLabel
        Left = 8
        Top = 44
        Width = 63
        Height = 19
        Caption = '&Description:'
      end
      object Label4: TSpTBXLabel
        Left = 8
        Top = 200
        Width = 210
        Height = 19
        Caption = 'Press Shift+Ctrl+M for Modifier completion'
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        ParentFont = False
      end
      object Label3: TSpTBXLabel
        Left = 8
        Top = 215
        Width = 220
        Height = 19
        Caption = 'Press Shift+Ctrl+P for Parameter completion'
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        ParentFont = False
      end
      object edDescription: TSpTBXEdit
        Left = 83
        Top = 43
        Width = 334
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
      end
      object edShortcut: TSpTBXEdit
        Left = 83
        Top = 19
        Width = 121
        Height = 21
        TabOrder = 0
        OnKeyPress = edShortcutKeyPress
      end
    end
    object lvItems: TEasyListview
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 420
      Height = 112
      Align = alTop
      EditManager.Font.Charset = DEFAULT_CHARSET
      EditManager.Font.Color = clWindowText
      EditManager.Font.Height = -11
      EditManager.Font.Name = 'Tahoma'
      EditManager.Font.Style = []
      Header.Columns.Items = {
        0600000002000000110000005445617379436F6C756D6E53746F726564FFFECE
        0006000000800800010100010000000000000078000000FFFFFF1F0001000000
        01000000040000004E0061006D00650000000000000000000000000011000000
        5445617379436F6C756D6E53746F726564FFFECE000600000080080001010001
        0100000000000018010000FFFFFF1F0001000000010000000B00000044006500
        73006300720069007000740069006F006E00000000000000000000000000}
      Header.Draggable = False
      Header.Visible = True
      HotTrack.Enabled = True
      PaintInfoGroup.MarginBottom.CaptionIndent = 4
      Selection.FullRowSelect = True
      TabOrder = 9
      View = elsReport
      OnItemFreeing = lvItemsItemFreeing
      OnItemSelectionsChanged = lvItemsItemSelectionsChanged
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
