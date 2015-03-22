inherited frmCustomKeyboard: TfrmCustomKeyboard
  Left = 306
  Top = 259
  HelpContext = 615
  Caption = 'Customize IDE Shortcuts'
  ClientHeight = 376
  ClientWidth = 346
  KeyPreview = True
  Position = poOwnerFormCenter
  OnDestroy = FormDestroy
  OnShow = FormShow
  ExplicitWidth = 352
  ExplicitHeight = 404
  PixelsPerInch = 96
  TextHeight = 13
  object SpTBXPanel1: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 346
    Height = 376
    Caption = 'SpTBXPanel1'
    Align = alClient
    TabOrder = 0
    TBXStyleBackground = True
    DesignSize = (
      346
      376)
    object Bevel1: TBevel
      Left = 4
      Top = 330
      Width = 348
      Height = 2
      Anchors = [akLeft, akRight, akBottom]
      Shape = bsTopLine
      ExplicitTop = 338
      ExplicitWidth = 337
    end
    object gbDescription: TSpTBXGroupBox
      Left = 8
      Top = 131
      Width = 328
      Height = 61
      Caption = ' Description '
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      TBXStyleBackground = True
      DesignSize = (
        328
        61)
      object lblDescription: TSpTBXLabel
        Left = 8
        Top = 17
        Width = 312
        Height = 35
        Anchors = [akLeft, akTop, akRight, akBottom]
        AutoSize = False
        Wrapping = twWrap
      end
    end
    object edNewShortcut: THotKey
      Left = 8
      Top = 224
      Width = 169
      Height = 21
      HotKey = 32833
      TabOrder = 0
      OnChange = edNewShortcutChange
    end
    object btnOK: TSpTBXButton
      Left = 95
      Top = 343
      Width = 75
      Height = 25
      Caption = '&OK'
      Anchors = [akRight, akBottom]
      TabOrder = 7
      Default = True
      ModalResult = 1
    end
    object btnCancel: TSpTBXButton
      Left = 179
      Top = 343
      Width = 75
      Height = 25
      Caption = '&Cancel'
      Anchors = [akRight, akBottom]
      TabOrder = 8
      Cancel = True
      ModalResult = 2
    end
    object btnHelp: TSpTBXButton
      Left = 263
      Top = 343
      Width = 75
      Height = 25
      Caption = '&Help'
      Anchors = [akRight, akBottom]
      TabOrder = 9
      OnClick = HelpButtonClick
    end
    object btnAssign: TSpTBXButton
      Left = 8
      Top = 293
      Width = 75
      Height = 25
      Caption = '&Assign'
      TabOrder = 4
      OnClick = btnAssignClick
      ImageIndex = 0
    end
    object btnRemove: TSpTBXButton
      Left = 192
      Top = 293
      Width = 75
      Height = 25
      Caption = '&Remove'
      TabOrder = 5
      OnClick = btnRemoveClick
    end
    object lblNewShortcutKey: TSpTBXLabel
      Left = 8
      Top = 205
      Width = 122
      Height = 19
      Caption = 'Press &new shortcut key:'
    end
    object lblCategories: TSpTBXLabel
      Left = 16
      Top = 8
      Width = 62
      Height = 19
      Caption = '&Categories:'
      FocusControl = lbCategories
    end
    object lblCommands: TSpTBXLabel
      Left = 148
      Top = 8
      Width = 62
      Height = 19
      Caption = 'C&ommands:'
      FocusControl = lbCommands
    end
    object lblCurrent: TSpTBXLabel
      Left = 8
      Top = 251
      Width = 113
      Height = 19
      Caption = 'Currently assigned to:'
      Visible = False
    end
    object lblAssignedTo: TSpTBXLabel
      Left = 8
      Top = 270
      Width = 71
      Height = 19
      Caption = 'lblAssignedTo'
      Visible = False
    end
    object lblCurrentKeys: TSpTBXLabel
      Left = 192
      Top = 205
      Width = 73
      Height = 19
      Caption = 'C&urrent Keys:'
      FocusControl = lbCurrentKeys
    end
    object lbCategories: TSpTBXListBox
      Left = 8
      Top = 27
      Width = 125
      Height = 97
      Style = lbStandard
      ItemHeight = 13
      TabOrder = 2
      OnClick = lbCategoriesClick
    end
    object lbCommands: TSpTBXListBox
      Left = 148
      Top = 28
      Width = 188
      Height = 97
      Style = lbStandard
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      Sorted = True
      TabOrder = 3
      OnClick = lbCommandsClick
    end
    object lbCurrentKeys: TSpTBXListBox
      Left = 192
      Top = 224
      Width = 144
      Height = 57
      Style = lbStandard
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 6
      OnClick = lbCurrentKeysClick
    end
  end
end
