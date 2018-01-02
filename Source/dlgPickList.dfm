inherited PickListDialog: TPickListDialog
  Caption = 'PickListDialog'
  ClientHeight = 333
  ClientWidth = 369
  ExplicitWidth = 375
  ExplicitHeight = 362
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TSpTBXPanel
    Left = 0
    Top = 249
    Width = 369
    Height = 84
    Align = alBottom
    Anchors = [akLeft, akBottom]
    UseDockManager = True
    TabOrder = 1
    TBXStyleBackground = True
    object Bevel1: TBevel
      Left = 0
      Top = 36
      Width = 404
      Height = 2
      Shape = bsTopLine
    end
    object btnSelectAll: TSpTBXButton
      Left = 66
      Top = 6
      Width = 114
      Height = 24
      Caption = '&Select All'
      TabOrder = 0
      OnClick = mnSelectAllClick
      Images = CommandsDataModule.Images
      ImageIndex = 104
    end
    object btnDeselectAll: TSpTBXButton
      Left = 186
      Top = 6
      Width = 114
      Height = 24
      Caption = '&Deselect All'
      TabOrder = 1
      OnClick = mnDeselectAllClick
      Images = CommandsDataModule.Images
      ImageIndex = 105
    end
    object btnOk: TSpTBXButton
      Left = 92
      Top = 52
      Width = 75
      Height = 25
      Caption = '&OK'
      TabOrder = 2
      Default = True
      ModalResult = 1
    end
    object btnCancel: TSpTBXButton
      Left = 199
      Top = 52
      Width = 75
      Height = 25
      Caption = '&Cancel'
      TabOrder = 3
      Cancel = True
      ModalResult = 2
    end
  end
  object SpTBXPanel1: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 369
    Height = 249
    Caption = 'SpTBXPanel1'
    Align = alClient
    TabOrder = 0
    TBXStyleBackground = True
    DesignSize = (
      369
      249)
    object imgIcon: TImage
      Left = 8
      Top = 12
      Width = 32
      Height = 32
      Transparent = True
    end
    object lbMessage: TSpTBXLabel
      Left = 56
      Top = 8
      Width = 305
      Height = 6
      Anchors = [akLeft, akTop, akRight]
      Wrapping = twWrap
    end
    object CheckListBox: TSpTBXCheckListBox
      AlignWithMargins = True
      Left = 5
      Top = 64
      Width = 359
      Height = 180
      Align = alBottom
      BevelInner = bvNone
      BevelOuter = bvNone
      BevelKind = bkSoft
      ItemHeight = 13
      PopupMenu = PickListPopUp
      Style = lbStandard
      TabOrder = 0
      ExplicitLeft = 7
      ExplicitTop = 66
    end
  end
  object PickListPopUp: TPopupMenu
    Images = CommandsDataModule.Images
    Left = 296
    Top = 112
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
