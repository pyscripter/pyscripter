inherited PickListDialog: TPickListDialog
  Caption = 'PickListDialog'
  ClientHeight = 333
  ClientWidth = 369
  Font.Name = 'MS Shell Dlg 2'
  Position = poMainFormCenter
  ExplicitWidth = 375
  ExplicitHeight = 359
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
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'MS Shell Dlg 2'
      LinkFont.Style = [fsUnderline]
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
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'MS Shell Dlg 2'
      LinkFont.Style = [fsUnderline]
    end
    object btnOk: TSpTBXButton
      Left = 92
      Top = 52
      Width = 75
      Height = 25
      Caption = '&OK'
      TabOrder = 2
      Default = True
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'Tahoma'
      LinkFont.Style = [fsUnderline]
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
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'Tahoma'
      LinkFont.Style = [fsUnderline]
      ModalResult = 2
    end
  end
  object lbMessage: TSpTBXLabel
    Left = 56
    Top = 8
    Width = 305
    Height = 0
    Anchors = [akLeft, akTop, akRight]
    ParentColor = True
    Wrapping = twWrap
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'MS Shell Dlg 2'
    LinkFont.Style = [fsUnderline]
  end
  object CheckListBox: TSpTBXCheckListBox
    Left = 0
    Top = 69
    Width = 369
    Height = 180
    Align = alBottom
    BevelKind = bkSoft
    ItemHeight = 13
    PopupMenu = PickListPopUp
    Style = lbStandard
    TabOrder = 0
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
