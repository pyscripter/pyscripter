object frmCustomKeyboard: TfrmCustomKeyboard
  Left = 306
  Top = 259
  HelpContext = 615
  BorderStyle = bsDialog
  Caption = 'Customize IDE Shortcuts'
  ClientHeight = 295
  ClientWidth = 423
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    423
    295)
  PixelsPerInch = 96
  TextHeight = 13
  object lblNewShortcutKey: TLabel
    Left = 12
    Top = 136
    Width = 113
    Height = 13
    Caption = 'Press &new shortcut key:'
  end
  object lblCategories: TLabel
    Left = 16
    Top = 8
    Width = 53
    Height = 13
    Caption = '&Categories:'
    FocusControl = lbCategories
  end
  object lblCommands: TLabel
    Left = 148
    Top = 8
    Width = 55
    Height = 13
    Caption = 'C&ommands:'
    FocusControl = lbCommands
  end
  object lblCurrent: TLabel
    Left = 12
    Top = 184
    Width = 101
    Height = 13
    Caption = 'Currently assigned to:'
    Visible = False
  end
  object lblAssignedTo: TLabel
    Left = 20
    Top = 204
    Width = 66
    Height = 13
    Caption = 'lblAssignedTo'
    Visible = False
  end
  object lblCurrentKeys: TLabel
    Left = 192
    Top = 136
    Width = 63
    Height = 13
    Caption = 'C&urrent Keys:'
    FocusControl = lbCurrentKeys
  end
  object gbDescription: TGroupBox
    Left = 8
    Top = 224
    Width = 317
    Height = 61
    Caption = ' Description '
    TabOrder = 1
    object lblDescription: TLabel
      Left = 16
      Top = 18
      Width = 289
      Height = 35
      AutoSize = False
      WordWrap = True
    end
  end
  object lbCategories: TListBox
    Left = 12
    Top = 28
    Width = 121
    Height = 97
    ItemHeight = 13
    TabOrder = 2
    OnClick = lbCategoriesClick
  end
  object lbCommands: TListBox
    Left = 148
    Top = 28
    Width = 177
    Height = 97
    ItemHeight = 13
    Sorted = True
    TabOrder = 3
    OnClick = lbCommandsClick
  end
  object btnAssign: TButton
    Left = 340
    Top = 28
    Width = 75
    Height = 25
    Caption = '&Assign'
    TabOrder = 4
    OnClick = btnAssignClick
  end
  object btnRemove: TButton
    Left = 340
    Top = 64
    Width = 75
    Height = 25
    Caption = '&Remove'
    TabOrder = 5
    OnClick = btnRemoveClick
  end
  object lbCurrentKeys: TListBox
    Left = 192
    Top = 156
    Width = 133
    Height = 57
    ItemHeight = 13
    TabOrder = 6
    OnClick = lbCurrentKeysClick
  end
  object edNewShortcut: THotKey
    Left = 10
    Top = 156
    Width = 167
    Height = 21
    HotKey = 32833
    TabOrder = 0
    OnChange = edNewShortcutChange
  end
  object OKButton: TBitBtn
    Left = 340
    Top = 175
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    TabOrder = 7
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 340
    Top = 215
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Cancel'
    TabOrder = 8
    Kind = bkCancel
  end
  object HelpButton: TBitBtn
    Left = 340
    Top = 255
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 9
    OnClick = HelpButtonClick
    Kind = bkHelp
  end
end
