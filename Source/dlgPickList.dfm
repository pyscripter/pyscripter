inherited PickListDialog: TPickListDialog
  Caption = 'PickListDialog'
  ClientHeight = 333
  ClientWidth = 484
  ExplicitWidth = 490
  ExplicitHeight = 362
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 249
    Width = 484
    Height = 84
    Align = alBottom
    Anchors = [akLeft, akBottom]
    TabOrder = 1
    DesignSize = (
      484
      84)
    object Bevel1: TBevel
      Left = 0
      Top = 36
      Width = 519
      Height = 2
      Anchors = [akLeft, akTop, akRight]
      Shape = bsTopLine
      ExplicitWidth = 404
    end
    object btnSelectAll: TButton
      Left = 109
      Top = 6
      Width = 130
      Height = 24
      Caption = '&Select All'
      ImageIndex = 0
      Images = vilImages
      TabOrder = 0
      OnClick = mnSelectAllClick
    end
    object btnDeselectAll: TButton
      Left = 245
      Top = 6
      Width = 130
      Height = 24
      Caption = '&Deselect All'
      ImageIndex = 1
      Images = vilImages
      TabOrder = 1
      OnClick = mnDeselectAllClick
    end
    object btnOk: TButton
      Left = 151
      Top = 47
      Width = 75
      Height = 25
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 2
    end
    object btnCancel: TButton
      Left = 258
      Top = 47
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 3
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 484
    Height = 249
    Align = alClient
    TabOrder = 0
    DesignSize = (
      484
      249)
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
      Width = 3
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      Color = clNone
      ParentColor = False
    end
    object CheckListBox: TCheckListBox
      AlignWithMargins = True
      Left = 4
      Top = 68
      Width = 476
      Height = 177
      Align = alBottom
      BevelInner = bvNone
      BevelOuter = bvNone
      BevelKind = bkSoft
      ItemHeight = 13
      PopupMenu = PickListPopUp
      TabOrder = 0
    end
  end
  object PickListPopUp: TPopupMenu
    Images = vilImages
    Left = 296
    Top = 112
    object mnSelectAll: TMenuItem
      Caption = '&Select All'
      Hint = 'Select all items'
      ImageIndex = 0
      OnClick = mnSelectAllClick
    end
    object mnDeselectAll: TMenuItem
      Caption = '&Deselect All'
      Hint = 'Deselect all items'
      ImageIndex = 1
      OnClick = mnDeselectAllClick
    end
  end
  object vilImages: TVirtualImageList
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 104
        CollectionName = 'Item105'
        Disabled = False
        Name = 'Item105'
      end
      item
        CollectionIndex = 105
        CollectionName = 'Item106'
        Disabled = False
        Name = 'Item106'
      end>
    ImageCollection = CommandsDataModule.icImages
    Left = 224
    Top = 112
  end
end
