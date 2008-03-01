object CodeTemplates: TCodeTemplates
  Left = 340
  Top = 192
  HelpContext = 540
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Code Templates'
  ClientHeight = 423
  ClientWidth = 430
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnClose = FormClose
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 430
    Height = 423
    Align = alClient
    TabOrder = 0
    DesignSize = (
      430
      423)
    object lvItems: TListView
      Left = 1
      Top = 1
      Width = 428
      Height = 109
      Align = alTop
      Columns = <
        item
          Caption = 'Name'
          Width = 120
        end
        item
          Caption = 'Description'
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
      OnDeletion = lvItemsDeletion
    end
    object GroupBox: TGroupBox
      Left = 10
      Top = 148
      Width = 415
      Height = 235
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Code Template:'
      TabOrder = 3
      DesignSize = (
        415
        235)
      object Label1: TLabel
        Left = 7
        Top = 24
        Width = 31
        Height = 13
        Caption = '&Name:'
        FocusControl = edShortcut
      end
      object Label2: TLabel
        Left = 7
        Top = 65
        Width = 48
        Height = 13
        Caption = '&Template:'
        FocusControl = SynTemplate
      end
      object Label5: TLabel
        Left = 7
        Top = 46
        Width = 57
        Height = 13
        Caption = '&Description:'
        FocusControl = edDescription
      end
      object Label4: TLabel
        Left = 8
        Top = 203
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
      object Label3: TLabel
        Left = 8
        Top = 215
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
      object SynTemplate: TSynEdit
        Left = 4
        Top = 80
        Width = 400
        Height = 117
        Anchors = [akLeft, akTop, akRight]
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
      object edShortcut: TEdit
        Left = 69
        Top = 19
        Width = 119
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnKeyPress = edShortcutKeyPress
      end
      object edDescription: TEdit
        Left = 70
        Top = 43
        Width = 338
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
      end
    end
    object TBXButton1: TSpTBXButton
      Left = 6
      Top = 118
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
      Top = 118
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
      Left = 173
      Top = 118
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
      Left = 257
      Top = 118
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
      Left = 341
      Top = 118
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
    object btnCancel: TSpTBXButton
      Left = 263
      Top = 391
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
    object btnOK: TSpTBXButton
      Left = 179
      Top = 391
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
    object btnHelp: TSpTBXButton
      Left = 347
      Top = 391
      Width = 75
      Height = 25
      Caption = '&Help'
      Anchors = [akRight, akBottom]
      TabOrder = 9
      OnClick = btnHelpClick
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'Tahoma'
      LinkFont.Style = [fsUnderline]
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
