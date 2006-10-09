object PickListDialog: TPickListDialog
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'PickListDialog'
  ClientHeight = 278
  ClientWidth = 207
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lbMessage: TLabel
    Left = 8
    Top = 8
    Width = 50
    Height = 13
    Caption = 'lbMessage'
  end
  object CheckListBox: TCheckListBox
    Left = 0
    Top = 42
    Width = 207
    Height = 199
    Align = alBottom
    BevelKind = bkSoft
    ItemHeight = 13
    Items.Strings = (
      'A'
      'B'
      'C')
    PopupMenu = PickListPopUp
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 0
    Top = 241
    Width = 207
    Height = 37
    Align = alBottom
    Anchors = [akLeft, akBottom]
    TabOrder = 1
    object OKButton: TBitBtn
      Left = 15
      Top = 6
      Width = 75
      Height = 25
      Caption = '&OK'
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 111
      Top = 6
      Width = 75
      Height = 25
      Caption = '&Cancel'
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object PickListPopUp: TPopupMenu
    Left = 160
    Top = 80
    object mnSelectAll: TMenuItem
      Caption = '&Select All'
      Hint = 'Select all items'
      OnClick = mnSelectAllClick
    end
    object mnDeselectAll: TMenuItem
      Caption = '&Deselect All'
      Hint = 'Deselect all items'
      OnClick = mnDeselectAllClick
    end
  end
end
