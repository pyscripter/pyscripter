object PickListDialog: TPickListDialog
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'PickListDialog'
  ClientHeight = 333
  ClientWidth = 369
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    369
    333)
  PixelsPerInch = 96
  TextHeight = 13
  object imgIcon: TImage
    Left = 8
    Top = 12
    Width = 32
    Height = 32
    Transparent = True
  end
  object lbMessage: TLabel
    Left = 56
    Top = 8
    Width = 305
    Height = 55
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    WordWrap = True
    ExplicitWidth = 304
  end
  object CheckListBox: TCheckListBox
    Left = 0
    Top = 69
    Width = 369
    Height = 180
    Align = alBottom
    BevelKind = bkSoft
    ItemHeight = 13
    PopupMenu = PickListPopUp
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 0
    Top = 249
    Width = 369
    Height = 84
    Align = alBottom
    Anchors = [akLeft, akBottom]
    TabOrder = 1
    object Bevel1: TBevel
      Left = 0
      Top = 36
      Width = 404
      Height = 2
      Shape = bsTopLine
    end
    object OKButton: TBitBtn
      Left = 92
      Top = 52
      Width = 75
      Height = 25
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 199
      Top = 52
      Width = 75
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
    object TBXButton1: TTBXButton
      Left = 92
      Top = 6
      Width = 88
      Height = 24
      ButtonStyle = bsFlat
      Caption = '&Select All'
      ImageIndex = 104
      Images = CommandsDataModule.Images
      TabOrder = 2
      OnClick = mnSelectAllClick
    end
    object TBXButton2: TTBXButton
      Left = 186
      Top = 6
      Width = 88
      Height = 24
      ButtonStyle = bsFlat
      Caption = '&Deselect All'
      ImageIndex = 105
      Images = CommandsDataModule.Images
      TabOrder = 3
      OnClick = mnDeselectAllClick
    end
  end
  object PickListPopUp: TPopupMenu
    Images = CommandsDataModule.Images
    Left = 368
    Top = 104
    object mnSelectAll: TMenuItem
      Caption = '&Select All'
      Hint = 'Select all items'
      ImageIndex = 104
      OnClick = mnSelectAllClick
    end
    object mnDeselectAll: TMenuItem
      Caption = '&Deselect All'
      Hint = 'Deselect all items'
      ImageIndex = 105
      OnClick = mnDeselectAllClick
    end
  end
end
