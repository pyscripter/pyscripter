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
  ExplicitHeight = 479
  PixelsPerInch = 96
  TextHeight = 13
  object Panel: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 528
    Height = 451
    Align = alClient
    TabOrder = 0
    TBXStyleBackground = True
    DesignSize = (
      528
      451)
    object GroupBox: TSpTBXGroupBox
      Left = 5
      Top = 148
      Width = 518
      Height = 264
      Caption = 'File Template:'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      TBXStyleBackground = True
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
      object edName: TSpTBXEdit
        Left = 106
        Top = 20
        Width = 127
        Height = 21
        TabOrder = 0
        OnKeyPress = edNameKeyPress
      end
      object edCategory: TSpTBXEdit
        Left = 377
        Top = 20
        Width = 131
        Height = 21
        TabOrder = 2
      end
      object edExtension: TSpTBXEdit
        Left = 195
        Top = 44
        Width = 38
        Height = 21
        TabOrder = 3
        OnKeyPress = edNameKeyPress
      end
      object CBHighlighters: TSpTBXComboBox
        Left = 377
        Top = 44
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 4
        OnChange = CBHighlightersChange
      end
      object Label1: TSpTBXLabel
        Left = 10
        Top = 24
        Width = 37
        Height = 19
        Caption = '&Name:'
        FocusControl = edName
      end
      object Label2: TSpTBXLabel
        Left = 10
        Top = 70
        Width = 54
        Height = 19
        Caption = '&Template:'
        FocusControl = SynTemplate
      end
      object Label5: TSpTBXLabel
        Left = 290
        Top = 20
        Width = 55
        Height = 19
        Caption = '&Category:'
        FocusControl = edCategory
      end
      object Label4: TSpTBXLabel
        Left = 10
        Top = 242
        Width = 210
        Height = 19
        Caption = 'Press Shift+Ctrl+M for Modifier completion'
        Enabled = False
      end
      object Label3: TSpTBXLabel
        Left = 10
        Top = 226
        Width = 220
        Height = 19
        Caption = 'Press Shift+Ctrl+P for Parameter completion'
        Enabled = False
      end
      object Label6: TSpTBXLabel
        Left = 10
        Top = 44
        Width = 95
        Height = 19
        Caption = '&Default Extension:'
        FocusControl = edExtension
      end
      object Label7: TSpTBXLabel
        Left = 288
        Top = 44
        Width = 61
        Height = 19
        Caption = '&Highlighter:'
        FocusControl = edCategory
      end
    end
    object TBXButton1: TSpTBXButton
      Left = 24
      Top = 118
      Width = 84
      Height = 24
      Action = actAddItem
      TabOrder = 3
      Images = CommandsDataModule.Images
      ImageIndex = 49
    end
    object TBXButton3: TSpTBXButton
      Left = 122
      Top = 118
      Width = 84
      Height = 24
      Action = actDeleteItem
      TabOrder = 4
      Images = CommandsDataModule.Images
      ImageIndex = 14
    end
    object TBXButton4: TSpTBXButton
      Left = 220
      Top = 118
      Width = 84
      Height = 24
      Action = actMoveUp
      TabOrder = 5
      Images = CommandsDataModule.Images
      ImageIndex = 47
    end
    object TBXButton5: TSpTBXButton
      Left = 318
      Top = 118
      Width = 84
      Height = 24
      Action = actMoveDown
      TabOrder = 6
      Images = CommandsDataModule.Images
      ImageIndex = 48
    end
    object TBXButton2: TSpTBXButton
      Left = 417
      Top = 118
      Width = 84
      Height = 24
      Action = actUpdateItem
      TabOrder = 7
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
      TabOrder = 0
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
      TabOrder = 1
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
      TabOrder = 8
      OnClick = btnHelpClick
    end
    object lvItems: TEasyListview
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 518
      Height = 110
      Align = alTop
      EditManager.Font.Charset = DEFAULT_CHARSET
      EditManager.Font.Color = clWindowText
      EditManager.Font.Height = -11
      EditManager.Font.Name = 'Tahoma'
      EditManager.Font.Style = []
      Header.Columns.Items = {
        0600000002000000110000005445617379436F6C756D6E53746F726564FFFECE
        00060000008008000101000100000000000001C8000000FFFFFF1F0001000000
        01000000040000004E0061006D00650000000000000000000000000011000000
        5445617379436F6C756D6E53746F726564FFFECE000600000080080001010001
        0100000000000118010000FFFFFF1F0001000000010000000800000043006100
        7400650067006F0072007900000000000000000000000000}
      Header.Draggable = False
      Header.Visible = True
      HotTrack.Enabled = True
      PaintInfoGroup.MarginBottom.CaptionIndent = 4
      Selection.FullRowSelect = True
      TabOrder = 9
      View = elsReport
      OnColumnClick = lvItemsColumnClick
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
