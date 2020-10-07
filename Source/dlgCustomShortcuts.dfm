inherited frmCustomKeyboard: TfrmCustomKeyboard
  Left = 306
  Top = 259
  HelpContext = 615
  Caption = 'Customize IDE Shortcuts'
  ClientHeight = 376
  ClientWidth = 459
  KeyPreview = True
  Position = poOwnerFormCenter
  OnDestroy = FormDestroy
  OnShow = FormShow
  ExplicitWidth = 465
  ExplicitHeight = 405
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 459
    Height = 376
    Align = alClient
    UseDockManager = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 0
    DesignSize = (
      459
      376)
    object Bevel1: TBevel
      Left = 4
      Top = 330
      Width = 461
      Height = 2
      Anchors = [akLeft, akRight, akBottom]
      Shape = bsTopLine
      ExplicitWidth = 348
    end
    object lblNewShortcutKey: TLabel
      Left = 8
      Top = 205
      Width = 116
      Height = 13
      Caption = 'Press &new shortcut key:'
      Color = clNone
      ParentColor = False
    end
    object lblCategories: TLabel
      Left = 10
      Top = 9
      Width = 56
      Height = 13
      Caption = '&Categories:'
      Color = clNone
      ParentColor = False
    end
    object lblCommands: TLabel
      Left = 200
      Top = 9
      Width = 56
      Height = 13
      Caption = 'C&ommands:'
      Color = clNone
      ParentColor = False
    end
    object lblCurrent: TLabel
      Left = 8
      Top = 251
      Width = 107
      Height = 13
      Caption = 'Currently assigned to:'
      Color = clNone
      ParentColor = False
      Visible = False
    end
    object lblAssignedTo: TLabel
      Left = 8
      Top = 270
      Width = 65
      Height = 13
      Caption = 'lblAssignedTo'
      Color = clNone
      ParentColor = False
      Visible = False
    end
    object lblCurrentKeys: TLabel
      Left = 200
      Top = 205
      Width = 67
      Height = 13
      Caption = 'C&urrent Keys:'
      Color = clNone
      ParentColor = False
    end
    object btnOK: TButton
      Left = 208
      Top = 343
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 3
    end
    object btnCancel: TButton
      Left = 292
      Top = 343
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 4
    end
    object btnHelp: TButton
      Left = 376
      Top = 343
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Help'
      TabOrder = 8
      OnClick = HelpButtonClick
    end
    object btnAssign: TButton
      Left = 8
      Top = 287
      Width = 107
      Height = 25
      Action = actAssignShortcut
      TabOrder = 6
    end
    object btnRemove: TButton
      Left = 192
      Top = 287
      Width = 97
      Height = 25
      Action = actRemoveShortcut
      TabOrder = 7
    end
    object lbCategories: TListBox
      Left = 10
      Top = 28
      Width = 184
      Height = 97
      ItemHeight = 13
      TabOrder = 1
      OnClick = lbCategoriesClick
    end
    object lbCommands: TListBox
      Left = 200
      Top = 28
      Width = 249
      Height = 97
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      Sorted = True
      TabOrder = 2
      OnClick = lbCommandsClick
    end
    object lbCurrentKeys: TListBox
      Left = 200
      Top = 224
      Width = 249
      Height = 57
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 5
    end
    object gbDescription: TGroupBox
      Left = 10
      Top = 131
      Width = 441
      Height = 61
      Anchors = [akLeft, akTop, akRight]
      Caption = ' Description '
      TabOrder = 0
      object lblDescription: TLabel
        Left = 2
        Top = 15
        Width = 437
        Height = 44
        Align = alClient
        AutoSize = False
        Color = clNone
        ParentColor = False
        Transparent = True
        ExplicitWidth = 324
      end
    end
  end
  object ActionList1: TActionList
    Left = 304
    object actAssignShortcut: TAction
      Caption = '&Assign'
      Hint = 'Assign shortcut to command'
      OnExecute = btnAssignClick
      OnUpdate = actAssignShortcutUpdate
    end
    object actRemoveShortcut: TAction
      Caption = '&Remove'
      Hint = 'Remove shortcut'
      OnExecute = btnRemoveClick
      OnUpdate = actRemoveShortcutUpdate
    end
  end
end
