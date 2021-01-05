inherited PickListDialog: TPickListDialog
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeable
  Caption = 'PickListDialog'
  ClientHeight = 323
  ClientWidth = 461
  ExplicitWidth = 477
  ExplicitHeight = 362
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 239
    Width = 461
    Height = 84
    Align = alBottom
    Anchors = [akLeft, akBottom]
    TabOrder = 1
    ExplicitTop = 241
    ExplicitWidth = 474
    DesignSize = (
      461
      84)
    object Bevel1: TBevel
      Left = 0
      Top = 36
      Width = 496
      Height = 2
      Anchors = [akLeft, akTop, akRight, akBottom]
      Shape = bsTopLine
      ExplicitWidth = 404
    end
    object btnSelectAll: TButton
      Left = 85
      Top = 6
      Width = 130
      Height = 24
      Caption = '&Select All'
      ImageIndex = 0
      ImageName = 'Item105'
      Images = vilImages
      TabOrder = 0
      OnClick = mnSelectAllClick
    end
    object btnDeselectAll: TButton
      Left = 240
      Top = 6
      Width = 130
      Height = 24
      Anchors = [akRight, akBottom]
      Caption = '&Deselect All'
      ImageIndex = 1
      ImageName = 'Item106'
      Images = vilImages
      TabOrder = 1
      OnClick = mnDeselectAllClick
    end
    object btnOk: TButton
      Left = 275
      Top = 48
      Width = 80
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 2
      ExplicitLeft = 288
    end
    object btnCancel: TButton
      Left = 372
      Top = 48
      Width = 80
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 3
      ExplicitLeft = 385
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 461
    Height = 239
    Align = alClient
    TabOrder = 0
    ExplicitLeft = -1
    ExplicitWidth = 474
    DesignSize = (
      461
      239)
    object imgIcon: TImage
      Left = 8
      Top = 12
      Width = 32
      Height = 32
      Transparent = True
    end
    object lbMessage: TLabel
      Left = 58
      Top = 14
      Width = 394
      Height = 52
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Color = clNone
      ParentColor = False
      WordWrap = True
    end
    object CheckListBox: TCheckListBox
      AlignWithMargins = True
      Left = 4
      Top = 72
      Width = 453
      Height = 163
      Align = alBottom
      Anchors = [akLeft, akTop, akRight, akBottom]
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
